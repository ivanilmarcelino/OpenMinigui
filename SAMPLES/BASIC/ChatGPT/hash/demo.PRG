#include "minigui.ch"
#include "TSBrowse.ch"

PROCEDURE Main()
    LOCAL oBrw
    LOCAL hData := { => }  // Hash table to store data
    LOCAL aGridData := {}  // Array to hold data for the grid

    // Initialize hash table with sample data
    hData["101"] := { "John Doe", "New York", 1200.50 }
    hData["102"] := { "Jane Smith", "Los Angeles", 950.00 }
    hData["103"] := { "Michael Brown", "Chicago", 1100.75 }
    hData["104"] := { "Emily Davis", "Houston", 870.30 }

    // Convert hash data into an array format for the grid
    aGridData := ConvertHashToArray(hData)

    // Create the main window
    DEFINE WINDOW oWnd TITLE "Harbour MiniGUI - Hash Data in TBROWSE" WIDTH 440 HEIGHT 300 ;
        MAIN NOMAXIMIZE

    @ 10, 10 TBROWSE oBrw WIDTH 400 HEIGHT 200 ;
        HEADERS "ID", "Name", "City", "Balance" ;
        WIDTHS 50, 150, 120, 80 ;
        ARRAY aGridData

    oBrw:nClrLine := COLOR_GRID
    oBrw:nHeightCell += 4
    oBrw:nHeightHead := oBrw:nHeightCell + 2
    oBrw:SetColor( { CLR_FOCUSF, CLR_FOCUSB }, { RGB(255, 255, 255), RGB(0, 120, 215) } )

    oBrw:lNoHScroll := .T.

    END WINDOW

    CENTER WINDOW oWnd
    ACTIVATE WINDOW oWnd

RETURN

// Function to convert hash data into an array format for the grid
FUNCTION ConvertHashToArray(hData)
    LOCAL aData := {}, cKey

    // Iterate through hash keys and build an array
    FOR EACH cKey IN hb_HKeys(hData)
        AADD(aData, { cKey, hData[cKey][1], hData[cKey][2], hData[cKey][3] })
    NEXT

RETURN aData
