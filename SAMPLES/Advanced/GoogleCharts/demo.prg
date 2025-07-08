/*
 * Google Chart Integration with Harbour MiniGUI
 *
 * This program demonstrates how to use Google Charts API to create dynamic charts 
 * within a Harbour MiniGUI application. It allows users to input data through a grid,
 * select a chart type, and specify dimensions for the chart display.
 *
 * Dependencies:
 * - Harbour MiniGUI
 * - Google Charts API
 *
 * Author: Edward
 * Date: November 2018
 */

#include "hmg.ch"

MEMVAR aTypes

// Main Function - Initializes the GUI and sets up controls
FUNCTION Main()
    LOCAL aRows[ 6 ], aCombo := {}
    
    // Sample data representing names and numeric values
    aRows[ 1 ] := { 'Simpson', 5 }
    aRows[ 2 ] := { 'Mulder', 30 }
    aRows[ 3 ] := { 'Smart', 50 }
    aRows[ 4 ] := { 'Grillo', 120 }
    aRows[ 5 ] := { 'Kirk', 90 }
    aRows[ 6 ] := { 'Barriga', 200 }

    // Chart types available for selection
    PRIVATE aTypes[ 5 ]
    aTypes[ 1 ] := { "Pie", "PieChart" }
    aTypes[ 2 ] := { "Pie 3D", "PieChart3D" }
    aTypes[ 3 ] := { "Line", "LineChart" }
    aTypes[ 4 ] := { "Bar", "BarChart" }
    aTypes[ 5 ] := { "Column", "ColumnChart" }

    AEval( aTypes, {| x | AAdd( aCombo, x[ 1 ] ) } )

    // Define Main Window
    DEFINE WINDOW Win_1 WIDTH 600 HEIGHT 500 TITLE "Google Chart HTML Sample" MAIN

        // Data Grid for user input
        @ 20, 20 GRID Grid_1 ;
            WIDTH 300 HEIGHT 110 ;
            HEADERS { 'Label', 'Data' } ;
            WIDTHS { 100, 100 } ;
            ITEMS aRows ;
            VALUE { 1, 1 } ;
            COLUMNCONTROLS { { 'TEXTBOX', 'CHARACTER' }, { 'TEXTBOX', 'NUMERIC', '9999' } } ;
            EDIT ;
            CELLNAVIGATION

        // Chart type selection dropdown
        @ 20, 390 COMBOBOX Combo_1 ;
            WIDTH 135 HEIGHT 194 ;
            ITEMS aCombo ;
            VALUE 1

        // Width input field for the chart
        @ 60, 390 TEXTBOX t_Width ;
            WIDTH 50 ;
            VALUE 480 ;
            NUMERIC INPUTMASK "999"

        // Height input field for the chart
        @ 100, 390 TEXTBOX t_Height ;
            WIDTH 50 ;
            VALUE 250 ;
            NUMERIC INPUTMASK "999"

        // Labels for input fields
        @ 20, 340 LABEL Label_1 WIDTH 50 HEIGHT 20 VALUE 'Type'
        @ 60, 340 LABEL Label_2 WIDTH 50 HEIGHT 20 VALUE 'Width'
        @ 100, 340 LABEL Label_3 WIDTH 50 HEIGHT 20 VALUE 'Height'

        // Generate button to create the chart
        @ 100, 460 BUTTON Button_1 ;
            CAPTION "Generate" ;
            ACTION GoogleChartApi_() ;
            WIDTH 70 HEIGHT 28

        // ActiveX control to display the chart in a browser window
        DEFINE ACTIVEX Activex_1
            ROW 150
            COL 20
            WIDTH Win_1.t_Width.Value + 70
            HEIGHT Win_1.t_Height.Value + 50
            PROGID "shell.explorer.2"
        END ACTIVEX

    END WINDOW

    // Set window styles and display
    HMG_ChangeWindowStyle( GetControlHandle( "Activex_1", "Win_1" ), WS_EX_CLIENTEDGE, NIL, .T. )
    CENTER WINDOW Win_1
    ACTIVATE WINDOW Win_1
RETURN NIL

// ***************************************************************************************
// Procedure to generate and display the Google Chart
PROCEDURE GoogleChartApi_()

   LOCAL cChartType := aTypes[ Win_1.Combo_1.Value ][ 2 ]
   LOCAL cChartWidth := hb_ntos( Win_1.t_Width.Value )
   LOCAL cChartHeight := hb_ntos( Win_1.t_Height.Value )
   LOCAL i, nItemCnt
   LOCAL cHeaders := "User-Agent: Mozilla/5.0 (compatible; MSIE 9.0; Windows Phone OS 7.5; Trident/5.0; IEMobile/9.0)"
   LOCAL cHtmlFile := "GoogleChart.html"
   LOCAL cLocalUrl := "file:///" + GetCurrentFolder() + "/" + cHtmlFile
   LOCAL cHtmlString := "" // "<!-- saved from url=(0017)" + "http://localhost/" /* cLocalUrl */ + " -->" + CRLF
   LOCAL oActiveX

   // Construct HTML content with embedded JavaScript for Google Charts API
   cHtmlString += '<html>  <head> <meta http-equiv="X-UA-Compatible" content="IE=edge" />  <meta charset="utf-8">' + CRLF + '<!--Load the AJAX API-->' + CRLF
   cHtmlString += '<script type="text/javascript" src="https://www.gstatic.com/charts/loader.js"></script>' + CRLF
   cHtmlString += '<script type="text/javascript">' + CRLF
   cHtmlString += '// Load the Visualization API and the corechart package.' + CRLF
   cHtmlString += "google.charts.load('current', {'packages':['corechart']});" + CRLF
   cHtmlString += '// Set a callback to run when the Google Visualization API is loaded.' + CRLF
   cHtmlString += 'google.charts.setOnLoadCallback(drawChart);' + CRLF
   cHtmlString += '// Callback that creates and populates a data table, instantiates the pie chart, passes in the data and draws it.' + CRLF
   cHtmlString += 'function drawChart() {' + CRLF
   cHtmlString += '// Create the data table.' + CRLF
   cHtmlString += 'var data = new google.visualization.DataTable();' + CRLF
   cHtmlString += "data.addColumn('string', 'Topping');" + CRLF
   cHtmlString += "data.addColumn('number', 'Slices');" + CRLF
   cHtmlString += "data.addRows([" + CRLF

   nItemCnt := Win_1.Grid_1.ItemCount
   FOR i = 1 TO nItemCnt

      cHtmlString += "['" + hb_StrToUTF8( AllTrim( hb_ValToStr( Win_1.Grid_1.Cell( i, 1 ) ) ) ) + "', " + AllTrim( hb_ValToStr( Win_1.Grid_1.Cell( i, 2 ) ) ) + "]"
      IF i < nItemCnt
         cHtmlString += ","
      ENDIF

      cHtmlString += CRLF
   NEXT i

   cHtmlString += "]);" + CRLF
   cHtmlString += '// Set chart options' + CRLF
   cHtmlString += "var options = {'title':'Sample Google Chart'," + CRLF
   IF cChartType == 'PieChart3D'
      cHtmlString += "    'is3D': true,"
      cChartType := 'PieChart'
   ENDIF

   cHtmlString += "               'width':" + cChartWidth + "," + CRLF
   cHtmlString += "               'height':" + cChartHeight + "};" + CRLF
   cHtmlString += '// Instantiate and draw our chart, passing in some options.' + CRLF
   cHtmlString += "var chart = new google.visualization." + cChartType + "(document.getElementById('chart_div'));" + CRLF
   cHtmlString += "chart.draw(data, options);" + CRLF
   cHtmlString += "}" + CRLF
   cHtmlString += '</script>  </head>' + CRLF
   cHtmlString += '<body>' + CRLF
   cHtmlString += '<!--Div that will hold the pie chart-->' + CRLF
   cHtmlString += '<div id="chart_div" style="width: ' + cChartWidth + 'px; height: ' + cChartHeight + 'px"></div>' + CRLF
   cHtmlString += '</body> </html>'

   STRFILE( cHtmlString, cHtmlFile )

   Win_1.Activex_1.WIDTH := Win_1.t_Width.VALUE + 70
   Win_1.Activex_1.HEIGHT := Win_1.t_Height.VALUE + 50

   oActiveX := GetProperty( 'Win_1', 'Activex_1', 'XObject' )
   oActiveX:Silent := 1
   oActiveX:Navigate( cLocalUrl, , , , cHeaders )

RETURN
