#include "minigui.ch"
#include "GraphPlus.ch"
#include "hbclass.ch"

/*
 * Class: ChartView
 * This class represents a graphical chart display using GraphPlus in a MiniGUI window.
 * It provides methods to set data, titles, graph type, draw the graph, and save it as an image.
 */
CLASS ChartView
   DATA oGraph       /* Object to handle graph rendering */
   DATA oParent      /* Parent window/container where the graph is displayed */
   DATA nWidth       /* Width of the graph area */
   DATA nHeight      /* Height of the graph area */
   DATA nRow         /* Row position of the graph in the window */
   DATA nCol         /* Column position of the graph in the window */

   METHOD New( oParent, nRow, nCol, nWidth, nHeight ) /* Constructor */
   METHOD SetData( aData, aCategories, aLegends )     /* Set graph data */
   METHOD SetTitles( cTitle, cXTitle, cYTitle )       /* Set graph titles */
   METHOD SetGraphType( nGraphType )                  /* Set graph type */
   METHOD Draw()                                      /* Draw the graph */
   METHOD Save( cFileName )                           /* Save the graph as an image file */
ENDCLASS

/*
 * Method: New
 * Initializes a ChartView object and creates a graphical area in the MiniGUI window.
 */
METHOD New( oParent, nRow, nCol, nWidth, nHeight ) CLASS ChartView
   IF _isControlDefined( 'graph', oParent )
      _ReleaseControl( 'graph', oParent )
   ENDIF
   /* Create an area to display the graph */
   DEFINE IMAGE graph
      PARENT &oParent
      ROW nRow
      COL nCol
      WIDTH nWidth
      HEIGHT nHeight
   END IMAGE
   
   /* Store properties */
   ::oParent := oParent
   ::nRow := nRow
   ::nCol := nCol
   ::nWidth := nWidth
   ::nHeight := nHeight
   
   /* Initialize GraphPlus object */
   ::oGraph := GraphPlus():New()
   RETURN Self

/*
 * Method: SetData
 * Sets the data for the graph along with categories (x-axis) and legends (series labels).
 */
METHOD SetData( aData, aCategories, aLegends ) CLASS ChartView
   ::oGraph:aData := aData
   ::oGraph:aCategories := aCategories
   ::oGraph:aLegends := aLegends
   RETURN NIL

/*
 * Method: SetTitles
 * Sets the main title, x-axis title, and y-axis title of the graph.
 */
METHOD SetTitles( cTitle, cXTitle, cYTitle ) CLASS ChartView
   ::oGraph:cTitle := cTitle
   ::oGraph:cXTitle := cXTitle
   ::oGraph:cYTitle := cYTitle
   RETURN NIL

/*
 * Method: SetGraphType
 * Sets the type of graph to be displayed (e.g., bar, line, pie chart).
 */
METHOD SetGraphType( nGraphType ) CLASS ChartView
   ::oGraph:nGraphType := nGraphType
   ::oGraph:lShowLegends := .T.
   RETURN NIL

/*
 * Method: Draw
 * Renders the graph and displays it in the MiniGUI window.
 */
METHOD Draw() CLASS ChartView
   ::oGraph:nImageWidth := ::nWidth
   ::oGraph:nImageHeight := ::nHeight
   ::oGraph:Draw()
   SetProperty( ::oParent, "graph", "HBITMAP", ::oGraph:Bitmap )
   RETURN NIL

/*
 * Method: Save
 * Saves the rendered graph as a PNG file. If the file extension is missing, it appends .png.
 */
METHOD Save( cFileName ) CLASS ChartView
   IF Empty( cFileName )
      RETURN NIL
   ENDIF

   /* Ensure the file has a .png extension */
   IF ! ".png" $ Lower(cFileName)
      cFileName += ".png"
   ENDIF

   ::oGraph:Save( cFileName )
   MsgInfo( "Graph saved as: " + cFileName )
   RETURN NIL

/*
 * Function: Main
 * Entry point of the application. Creates a window and an instance of ChartView.
 */
FUNCTION Main()
   LOCAL oChartView

   /* Define and create the main window */
   DEFINE WINDOW Win_1 AT 0, 0 WIDTH 700 HEIGHT 700 TITLE "ChartView Example" MAIN

   /* Create a ChartView object within the window */
   oChartView := ChartView():New( "Win_1", 10, 30, 625, 600 )
   oChartView:SetData( { { 15, 30, 45 }, { 20, 35, 50 } }, { "Q1", "Q2", "Q3" }, { "Product A", "Product B" } )
   oChartView:SetTitles( "Quarterly Sales Report", "Quarters", "Sales (in Thousands)" )
   oChartView:SetGraphType( GT_COLUMNS )

   /* Add buttons for drawing and saving the graph */
   @ 620, 240 BUTTON BtnDraw CAPTION "Draw Graph" ACTION oChartView:Draw()
   @ 620, 350 BUTTON BtnSave CAPTION "Save Graph" ACTION oChartView:Save( PutFile( {{ "PNG Files (*.png)", "*.png" }}, "Save Graph As" ) )

   END WINDOW

   CENTER WINDOW Win_1
   /* Activate and display the window */
   ACTIVATE WINDOW Win_1
RETURN NIL
