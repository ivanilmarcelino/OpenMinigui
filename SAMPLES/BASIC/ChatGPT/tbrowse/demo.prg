#include "minigui.ch"
#include "tsbrowse.ch"

function Main()
    LOCAL oBrw, hFont, nI
    LOCAL aData := { ;
        { "John Doe", 28, "Engineer", "New York" }, ;
        { "Jane Smith", 34, "Manager", "Los Angeles" }, ;
        { "Sam Johnson", 25, "Intern", "Chicago" }, ;
        { "Alice Brown", 42, "Director", "Miami" }, ;
        { "Bob White", 30, "Developer", "Seattle" } }

    // Define the main window.
    DEFINE WINDOW oWnd ;
        AT 0, 0 ;
        WIDTH 700 ;
        HEIGHT 400 ;
        TITLE "TSBrowse Event Handling and Color Customization" ;
        MAIN

    END WINDOW

    // Create the TSBrowse control (data grid).
    @ 20, 20 TBROWSE oBrw ;
        WIDTH 650 ;
        HEIGHT 300 ;
        OF oWnd ;
        ON DBLCLICK BrowseDblClick( oBrw ) ;  // Event: Double-click handler
        ON CHANGE BrowseSelChange( oBrw )     // Event: Row selection change handler

    // Event handler for right-click on the browse control
    oBrw:bRClicked := {|x, y, nRow, oBrw| x := y := nil, BrowseRightClick( oBrw, nRow )}

    // Define and apply a custom font to the browse control (optional)
    DEFINE FONT Normal FONTNAME "Arial" SIZE 10
    hFont := GetFontHandle( "Normal" )
    oBrw:SetFont( hFont )

    // Customize row and column colors dynamically:
    // 1. Set foreground color based on the row/column using the GetForeColor function
    // 2. Set alternating background colors (gray and white) for even/odd rows
    oBrw:SetColor( { 1, 2 }, { {|nRow, nCol, oBrw| GetForeColor( oBrw, nRow, nCol )}, ;
                               {|nRow, nCol, oBrw| iif( nRow % 2 == 0, RGB( 240, 240, 240 ), CLR_WHITE )} } )

    // Set specific color for the header text color
    oBrw:SetColor( { 4 }, { CLR_WHITE } )

    // Customize additional colors
    oBrw:SetColor( { 5, 6 }, { {|nRow, nCol, oBrw| GetForeColor( oBrw, nRow, nCol )}, {CLR_WHITE, CLR_GRAY} } )

    // Set the array data as the source for the browse control
    oBrw:SetArray( aData )

    // Define columns for the TSBrowse control:
    // The first parameter is the data element (array column) followed by the title and width
    ADD COLUMN TO oBrw DATA ARRAY ELEMENT 1 TITLE "Name" WIDTH 200
    ADD COLUMN TO oBrw DATA ARRAY ELEMENT 2 TITLE "Age" WIDTH 50 ALIGN DT_RIGHT
    ADD COLUMN TO oBrw DATA ARRAY ELEMENT 3 TITLE "Occupation" WIDTH 150
    ADD COLUMN TO oBrw DATA ARRAY ELEMENT 4 TITLE "Location" WIDTH 150

    // Iterate over columns and set the header foreground color based on the column index
    FOR nI := 1 TO Len( oBrw:aColumns )
        oBrw:aColumns[ nI ]:nClrHeadFore := GetForeColor( oBrw, , nI )
    NEXT

    // Adjust columns' sizes automatically to fit within the browse control
    oBrw:AdjColumns()

    // Center the window on the screen
    CENTER WINDOW oWnd

    // Activate and display the window
    ACTIVATE WINDOW oWnd

return nil

// Function to determine the text color for the browse control
function GetForeColor( oBrw, nRow, nCol )
    LOCAL hFGColor

    // Set different colors for each column:
    // Name (column 1) - Blue, Age (column 2) - Red, Occupation (column 3) - Green, Location (column 4) - Brown
    DO CASE
        CASE nCol == 1
            hFGColor := CLR_BLUE  // Blue for Name column
        CASE nCol == 2
            hFGColor := CLR_RED   // Red for Age column
        CASE nCol == 3
            hFGColor := CLR_GREEN // Green for Occupation column
        CASE nCol == 4
            hFGColor := CLR_BROWN // Brown for Location column
        OTHERWISE
            hFGColor := CLR_BLACK // Default black color
    ENDCASE

return hFGColor

// Procedure called when the user selects a different row in the TSBrowse control
procedure BrowseSelChange( oBrw )
    LOCAL nRow := oBrw:nAt()  // Get the selected row number
    IF !oBrw:lInitGotop  // Avoid showing the message on the initial load of the browse
        MsgInfo( "Selected Row: " + STR( nRow, 1 ) )
    ELSE
        oBrw:lInitGotop := .F.
    ENDIF
return

// Procedure called when the user double-clicks on a row in the TSBrowse control
procedure BrowseDblClick( oBrw )
    LOCAL nRow := oBrw:nAt()  // Get the selected row number
    LOCAL aRow := oBrw:aArray[ nRow ]  // Access the selected row data from the array
    // Display the selected row data in a message box
    MsgInfo( "Double Clicked Row: " + STR( nRow, 1 ) + CRLF + ;
             "Name: " + aRow[ 1 ] + CRLF + ;
             "Age: " + STR( aRow[ 2 ], 2 ) + CRLF + ;
             "Occupation: " + aRow[ 3 ] + CRLF + ;
             "Location: " + aRow[ 4 ] )
return

// Procedure called when the user right-clicks on a row in the TSBrowse control
procedure BrowseRightClick( oBrw, nRow )
    LOCAL aRow := oBrw:aArray[ nRow ]  // Access the selected row data from the array
    // Display the right-clicked row data in a message box
    MsgInfo( "Right Clicked Row: " + STR( nRow, 1 ) + CRLF + ;
             "Name: " + aRow[ 1 ] + CRLF + ;
             "Age: " + STR( aRow[ 2 ], 2 ) + CRLF + ;
             "Occupation: " + aRow[ 3 ] + CRLF + ;
             "Location: " + aRow[ 4 ] )
return
