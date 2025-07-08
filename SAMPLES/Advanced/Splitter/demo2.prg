/*
 * Harbour MiniGUI Splitter Demo
 */
#include "minigui.ch"

PROCEDURE Main()

    SET AUTOADJUST ON  // Automatically adjust controls when the window is resized

    // Open the DBF file for the BROWSE control
    OpenBrowseDB()

    // Define the main window
    DEFINE WINDOW MainWin ;
        AT 0, 0 ;
        WIDTH 930 ;
        HEIGHT 650 ;
        TITLE "Complex SPLITTER Example with BROWSE Data" ;
        MAIN

        // Define controls in the top-left area
        @ 10, 10 EDITBOX EditBoxTopLeft ;
            WIDTH 250 ;
            HEIGHT 100 ;
            VALUE "Top Left Pane" ;
            NOVSCROLL NOHSCROLL

        @ 120, 10 EDITBOX EditBoxBottomLeft ;
            WIDTH 250 ;
            HEIGHT 100 ;
            VALUE "Bottom Left Pane" ;
            NOVSCROLL NOHSCROLL

        // Horizontal splitter between EditBoxTopLeft and EditBoxBottomLeft
        DEFINE SPLITTER HSplitterLeft ;
            AT 110, 10 ;
            WIDTH 250 ;
            HEIGHT 10 ;
            HORIZONTAL ;
            SPLIT {"EditBoxTopLeft"} FROM {"EditBoxBottomLeft"} ;
            USEGRADIENT ;
            GRADIENT {180, 180, 180}, {100, 100, 100} ;
            GRADIENTHOVER {255, 200, 200}, {200, 100, 100}

        // Define controls in the top-right area (a Grid)
        @ 10, 300 GRID GridTopRight ;
            WIDTH 600 ;
            HEIGHT 240 ;
            HEADERS {'Name', 'Age', 'Gender', 'Occupation'} ;
            ITEMS GenerateGridData() ;
            WIDTHS {150, 100, 100, 200}

        // Define controls in the bottom-right area (a Browse control)
        @ 260, 300 BROWSE BrowseBottomRight ;
            WIDTH 600 ;
            HEIGHT 240 ;
            HEADERS {'ID', 'First Name', 'Last Name'} ;
            WIDTHS {50, 240, 240} ;
            FIELDS {'TestData->id', 'TestData->first', 'TestData->last'} ;
            WORKAREA 'TestData'

        // Horizontal splitter between GridTopRight and BrowseBottomRight
        DEFINE SPLITTER HSplitterRight ;
            AT 250, 300 ;
            WIDTH 600 ;
            HEIGHT 10 ;
            HORIZONTAL ;
            SPLIT {"GridTopRight"} FROM {"BrowseBottomRight"} ;
            USEGRADIENT ;
            GRADIENT {150, 150, 255}, {100, 100, 255} ;
            GRADIENTHOVER {255, 200, 255}, {200, 100, 255}

        // Vertical splitter to divide the left and right sides of the window
        DEFINE SPLITTER VSplitterMain ;
            AT 10, 275 ;
            WIDTH 10 ;
            HEIGHT 530 ;
            VERTICAL ;
            SPLIT {"EditBoxTopLeft", "EditBoxBottomLeft", "HSplitterLeft"} FROM {"GridTopRight", "BrowseBottomRight", "HSplitterRight"} ;
            USEGRADIENT ;
            GRADIENT {255, 255, 0}, {255, 128, 0} ;
            GRADIENTHOVER {255, 200, 0}, {255, 80, 0} ;
            LIMITS {5, 5}  // Set the minimum size limits for each pane

        // Define a Button section below the main split areas
        @ 550, 100 BUTTON Btn1 ;
            CAPTION "Submit" ;
            WIDTH 200 ;
            HEIGHT 40 ;
            ACTION NIL

        @ 550, 350 BUTTON Btn2 ;
            CAPTION "Cancel" ;
            WIDTH 200 ;
            HEIGHT 40 ;
            ACTION ThisWindow.RELEASE

        // Horizontal splitter to separate buttons from other controls
        DEFINE SPLITTER HSplitterButtons ;
            AT 540, 10 ;
            WIDTH 900 ;
            HEIGHT 10 ;
            HORIZONTAL ;
            SPLIT {"GridTopRight", "BrowseBottomRight", "VSplitterMain"} FROM {"Btn1", "Btn2"} ;
            USEGRADIENT ;
            GRADIENT {64, 128, 128}, {32, 64, 64} ;
            GRADIENTHOVER {128, 255, 255}, {64, 128, 128}

    END WINDOW

    CENTER WINDOW MainWin

    ACTIVATE WINDOW MainWin

RETURN

// Function to generate data for the grid control
FUNCTION GenerateGridData()
    LOCAL aData[10][4], i

    FOR i := 1 TO 10
        aData[i][1] := "Name " + HB_NTOS(i)
        aData[i][2] := 20 + i
        aData[i][3] := IF(i % 2 == 0, "Male", "Female")
        aData[i][4] := "Occupation " + HB_NTOS(i)
    NEXT

RETURN aData

// Procedure to open the DBF for the BROWSE control
PROCEDURE OpenBrowseDB()
    IF !FILE("TestData.dbf")
        GenerateTestData()
    ENDIF
    USE TestData
RETURN

// Procedure to create a DBF file for the BROWSE control if it doesn't exist
PROCEDURE GenerateTestData()
    LOCAL aStruct := { ;
        {"ID", "N", 5, 0}, ;
        {"First", "C", 20, 0}, ;
        {"Last", "C", 20, 0} }

    DBCREATE("TestData", aStruct)
    USE TestData
    FOR i := 1 TO 20
        APPEND BLANK
        REPLACE TestData->ID WITH i
        REPLACE TestData->First WITH "First Name " + HB_NTOS(i)
        REPLACE TestData->Last WITH "Last Name " + HB_NTOS(i)
    NEXT
    USE
RETURN
