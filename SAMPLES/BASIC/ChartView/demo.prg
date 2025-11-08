/*
 * MINIGUI - Harbour Win32 GUI library Demo
 */

#include "hmg.ch"
#include "GraphPlus.ch"
#include "hbclass.ch"

STATIC aTypes[ 4 ]

/*
 * Description: Initializes the main window for a chart demo application.
 * Parameters: None
 * Return: NIL - No value returned.
 * Purpose: Sets up a GUI with a grid for data input, chart type selection, and size controls to demonstrate GraphPlus charting; e.g., allows users to visualize data in various chart formats.
 * Notes: Uses sample data for demonstration; window is fixed-size and centered.
 */
FUNCTION Main()
   LOCAL aRows[ 6 ], aCombo := {}

   aRows[ 1 ] := { 'Simpson', 5 }
   aRows[ 2 ] := { 'Mulder', 30 }
   aRows[ 3 ] := { 'Smart', 50 }
   aRows[ 4 ] := { 'Grillo', 120 }
   aRows[ 5 ] := { 'Kirk', 90 }
   aRows[ 6 ] := { 'Barriga', 200 }

   aTypes[ 1 ] := { "Pie", "PieChart" }
   aTypes[ 2 ] := { "Line", "LineChart" }
   aTypes[ 3 ] := { "Bar", "BarChart" }
   aTypes[ 4 ] := { "Columns", "ColumnChart" }

   AEval( aTypes, {| x | AAdd( aCombo, x[ 1 ] ) } )

   DEFINE WINDOW Win_1 WIDTH 600 HEIGHT 500 TITLE "GraphPlus Chart Sample" MAIN
      @ 20, 20 GRID Grid_1 ;
         WIDTH 300 HEIGHT 110 ;
         HEADERS { 'Label', 'Data' } ;
         WIDTHS { 100, 100 } ;
         ITEMS aRows ;
         VALUE { 1, 1 } ;
         COLUMNCONTROLS { { 'TEXTBOX', 'CHARACTER' }, { 'TEXTBOX', 'NUMERIC', '9999' } } ;
         EDIT ;
         CELLNAVIGATION

      @ 20, 390 COMBOBOX Combo_1 ;
         WIDTH 135 HEIGHT 194 ;
         ITEMS aCombo ;
         VALUE 1 ;
         ON LISTCLOSE This.Button_1.SetFocus()

      @ 60, 390 TEXTBOX t_Width ;
         WIDTH 50 ;
         VALUE 480 ;
         NUMERIC INPUTMASK "999"

      @ 100, 390 TEXTBOX t_Height ;
         WIDTH 50 ;
         VALUE 250 ;
         NUMERIC INPUTMASK "999"

      @ 20, 340 LABEL Label_1 WIDTH 50 HEIGHT 20 VALUE 'Type'
      @ 60, 340 LABEL Label_2 WIDTH 50 HEIGHT 20 VALUE 'Width'
      @ 100, 340 LABEL Label_3 WIDTH 50 HEIGHT 20 VALUE 'Height'

      @ 100, 460 BUTTON Button_1 ;
         CAPTION "Generate" ;
         ACTION GenerateChart() ;
         WIDTH 70 HEIGHT 28 DEFAULT
   END WINDOW

   CENTER WINDOW Win_1
   ACTIVATE WINDOW Win_1
RETURN NIL

/*
 * Description: Generates a chart based on user input.
 * Parameters: None
 * Return: NIL - No value returned.
 * Purpose: Creates and displays a chart using data from the grid, with type and size set by user controls; e.g., visualizes user-entered data in selected chart format.
 * Notes: Adjusts chart properties based on type; pie charts show legends, others hide them for clarity.
 */
FUNCTION GenerateChart()
   LOCAL aRows := Win_1.Grid_1.GetArray()
   LOCAL data1 := {}
   LOCAL data2 := {}
   LOCAL oChartView

   AEval( aRows, {| x | AAdd( data1, x[ 1 ] ) } )
   AEval( aRows, {| x | AAdd( data2, x[ 2 ] ) } )

   oChartView := ChartView():New( "Win_1", 150, 20, Win_1.t_Width.VALUE +70, Win_1.t_Height.VALUE +50 )
   WITH OBJECT oChartView
      :oGraph:LegendFont := array FONT 'Arial' SIZE 10
      :SetData( data2, data1, data1 )
      SWITCH Win_1.Combo_1.VALUE
      CASE 1
         :SetGraphType( GT_PIE )
         :oGraph:LegendPos := LEGEND_ON_RIGHT
         EXIT
      CASE 2
         :SetGraphType( GT_LINE )
         :oGraph:lShowLegends := .F.
         EXIT
      CASE 3
         :oGraph:BarGapRatio := 0
         :oGraph:BarGapWidthRatio := 0.1
         :SetGraphType( GT_BAR )
         :oGraph:lShowLegends := .F.
         EXIT
      CASE 4
         :SetGraphType( GT_COLUMNS )
         :oGraph:lShowLegends := .F.
         EXIT
      END
      :SetTitles( aTypes[ Win_1.Combo_1.Value ][ 1 ] + " Chart Sample" )
      :Draw()
   END WITH
RETURN NIL

/*
 * Description: Defines a class for rendering charts in a MiniGUI window.
 * Parameters: None
 * Return: None - Class definition.
 * Purpose: Encapsulates chart rendering functionality for reusable chart display; e.g., provides a flexible way to create various chart types in GUI applications.
 * Notes: Depends on GraphPlus.ch for chart types and rendering.
 */
CLASS ChartView
   DATA oGraph
   DATA oParent
   DATA nWidth
   DATA nHeight
   DATA nRow
   DATA nCol

   METHOD New( oParent, nRow, nCol, nWidth, nHeight )
   METHOD SetData( aData, aCategories, aLegends )
   METHOD SetTitles( cTitle, cXTitle, cYTitle )
   METHOD SetGraphType( nGraphType )
   METHOD Draw()
   METHOD Save( cFileName )
ENDCLASS

/*
 * Description: Initializes a ChartView object for graph display.
 * Parameters: oParent - Character, required, parent window name; nRow - Numeric, required, row position; nCol - Numeric, required, column position; nWidth - Numeric, required, graph width; nHeight - Numeric, required, graph height.
 * Return: Object - Self, the initialized ChartView instance.
 * Purpose: Sets up a chart area in the specified window; e.g., creates a space for rendering charts in a GUI.
 * Notes: Releases existing graph control to avoid conflicts; initializes GraphPlus object.
 */
METHOD New( oParent, nRow, nCol, nWidth, nHeight ) CLASS ChartView
   IF _isControlDefined( 'graph', oParent )
      _ReleaseControl( 'graph', oParent )
   ENDIF
   DEFINE IMAGE graph
      PARENT &oParent
      ROW nRow
      COL nCol
      WIDTH nWidth
      HEIGHT nHeight
   END IMAGE

   ::oParent := oParent
   ::nRow := nRow
   ::nCol := nCol
   ::nWidth := nWidth
   ::nHeight := nHeight
   ::oGraph := GraphPlus():New()
RETURN Self

/*
 * Description: Sets data for the chart.
 * Parameters: aData - Array, required, numeric data values; aCategories - Array, required, x-axis labels; aLegends - Array, required, series labels.
 * Return: NIL - No value returned.
 * Purpose: Configures chart data for rendering; e.g., maps user input to chart axes for visualization.
 * Notes: Assumes arrays are properly formatted for GraphPlus.
 */
METHOD SetData( aData, aCategories, aLegends ) CLASS ChartView
   ::oGraph:GraphData := aData
   ::oGraph:Categories := aCategories
   ::oGraph:Legends := aLegends
RETURN NIL

/*
 * Description: Sets chart titles.
 * Parameters: cTitle - Character, required, main chart title; cXTitle - Character, optional, x-axis title; cYTitle - Character, optional, y-axis title.
 * Return: NIL - No value returned.
 * Purpose: Defines chart labels for clarity; e.g., adds descriptive titles to enhance chart readability.
 * Notes: Optional parameters default to empty if not provided.
 */
METHOD SetTitles( cTitle, cXTitle, cYTitle ) CLASS ChartView
   ::oGraph:cTitle := cTitle
   ::oGraph:cXTitle := cXTitle
   ::oGraph:cYTitle := cYTitle
RETURN NIL

/*
 * Description: Sets the chart type.
 * Parameters: nGraphType - Numeric, required, GraphPlus chart type constant (e.g., GT_PIE).
 * Return: NIL - No value returned.
 * Purpose: Specifies chart style for rendering; e.g., allows switching between pie, line, or bar charts.
 * Notes: Enables legends by default for flexibility.
 */
METHOD SetGraphType( nGraphType ) CLASS ChartView
   ::oGraph:GraphType := nGraphType
   ::oGraph:lShowLegends := .T.
RETURN NIL

/*
 * Description: Renders the chart in the window.
 * Parameters: None
 * Return: NIL - No value returned.
 * Purpose: Draws the configured chart in the GUI; e.g., displays user-selected chart type with data.
 * Notes: Sets image dimensions before drawing; uses GraphPlus bitmap for rendering.
 */
METHOD Draw() CLASS ChartView
   ::oGraph:nImageHeight := ::nHeight
   ::oGraph:nImageWidth := ::nWidth
   ::oGraph:Draw()
   SetProperty( ::oParent, "graph", "HBITMAP", ::oGraph:Bitmap )
RETURN NIL

/*
 * Description: Saves the chart as a PNG file.
 * Parameters: cFileName - Character, required, output file name.
 * Return: NIL - No value returned.
 * Purpose: Exports chart for external use; e.g., allows users to save visualizations for reports.
 * Notes: Appends .png if extension missing; shows confirmation message.
 */
METHOD Save( cFileName ) CLASS ChartView
   IF Empty( cFileName )
      RETURN NIL
   ENDIF
   IF ! ".png" $ Lower( cFileName )
      cFileName += ".png"
   ENDIF
   ::oGraph:Save( cFileName )
   MsgInfo( "Graph saved as: " + cFileName )
RETURN NIL
