#include "minigui.ch"

PROCEDURE Main()
    LOCAL hData := { => }  // Hash table to store data
    LOCAL aGridData := {}  // Array to store converted hash data
    LOCAL nTotal, nIndex, aPair

    // Initialize hash table with sample data
    hData["101"] := { "John Doe", "New York", 1200.50 }
    hData["102"] := { "Jane Smith", "Los Angeles", 950.00 }
    hData["103"] := { "Michael Brown", "Chicago", 1100.75 }
    hData["104"] := { "Emily Davis", "Houston", 870.30 }

    // Convert hash table to array using hb_HPairAt()
    nTotal := Len(hData)  // Get total number of items in the hash
    FOR nIndex := 1 TO nTotal
        aPair := hb_HPairAt(hData, nIndex)  // Retrieve key-value pair at index
        IF !Empty(aPair)
            AADD(aGridData, { aPair[1], aPair[2][1], aPair[2][2], aPair[2][3] })
        ENDIF
    NEXT

    // Create the main window
    DEFINE WINDOW oWnd TITLE "Harbour MiniGUI - Hash Data in GRID" ;
        WIDTH 440 HEIGHT 300 MAIN NOMAXIMIZE

    @ 10, 10 GRID oGrid AUTOSIZEWIDTH 460 HEIGHT 200 OF oWnd ;
        HEADERS { "ID", "Name", "City", "Balance" } ;
        WIDTHS {50, 150, 120, 80} ;
        JUSTIFY {0, 0, 0, 1} ;
        ITEMS aGridData VALUE 1

    END WINDOW

    CENTER WINDOW oWnd
    ACTIVATE WINDOW oWnd
RETURN
