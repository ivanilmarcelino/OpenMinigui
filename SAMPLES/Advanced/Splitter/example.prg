/*
 * Harbour MiniGUI Splitter Demo
 * Copyright 2024 Kamil Kalus <kamilkalus0[at]gmail.com>
 */
#include "minigui.ch"
#include "text.ch"

Procedure Main()

    LOCAL splitter1, splitter2, splitter3, splitter4, splitter5
    LOCAL splitter6, splitter7, splitter8, splitter9

    // Enable auto-adjusting of control size/position when the window is resized
    SET AUTOADJUST ON

    // Define the main window
    DEFINE WINDOW Win_1;
        AT 0,0;                            // Position of the window
        WIDTH 1395;                        // Window width
        HEIGHT 965;                        // Window height
        TITLE 'SPLITTER Showcase';         // Window title
        ON INIT OpenDBF();                 // Open database when window initializes
        ON RELEASE CloseDBF();             // Close database when window is closed
        MAIN                               // Define as the main application window

        // Define the first editbox with sample text
        @ 0, 0 EDITBOX editbox;
          WIDTH 300 HEIGHT 190;
          value    LOREM;                  // Set value to a sample text constant
          NOVSCROLL NOHSCROLL              // No scrollbars

        // Define a second editbox below the first one
        @ 200, 0 EDITBOX tbx2;
          WIDTH 300 HEIGHT 200;
          value    LOREM;
          NOVSCROLL NOHSCROLL

        // Define a horizontal splitter between editbox and tbx2
        DEFINE SPLITTER splitter6;
            HORIZONTAL;
            SPLIT {'editbox'} FROM {'tbx2'}

        // Define a browse control to display data from a DBF table
        @ 50,315 BROWSE brow;
            WIDTH 750; 
            HEIGHT 250; 	
            HEADERS {'LP', 'First Name', 'Last Name', 'Gender', 'Married', 'Birth Date', 'Biography'};
            WIDTHS {50 , 140 , 140 , 60 , 60 , 150 , 150};
            WORKAREA 'Test';               // Link browse to 'Test' DBF table
            FIELDS {'Test->lp', 'Test->fname', 'Test->lname', 'Test->gender', 'Test->married', 'Test->birth', 'Test->bio'};
            JUSTIFY {BROWSE_JTFY_CENTER, BROWSE_JTFY_LEFT, BROWSE_JTFY_LEFT, BROWSE_JTFY_CENTER, BROWSE_JTFY_CENTER, BROWSE_JTFY_CENTER, BROWSE_JTFY_CENTER};
            EDIT                            // Make the browse editable

        // Define a vertical splitter between the first two editboxes and the browse control
        DEFINE SPLITTER splitter1;
          VERTICAL NOHOVER;
          COLOR {179, 70, 62};             // Splitter color
          SPLIT {"editbox", "tbx2", "splitter6"} FROM {"brow"}; // Components to be separated by the splitter
          LIMITS {5, 15}                   // Minimum and maximum sizes for each section

        // Define additional editboxes for further layout customization
        @ 0, 1080 EDITBOX tbx3;
            WIDTH 300 HEIGHT 200;
            value    LOREM;
            NOVSCROLL NOHSCROLL

        @ 210, 1080 EDITBOX tbx4;
            WIDTH 300 HEIGHT 190;
            value    LOREM;
            NOVSCROLL NOHSCROLL

        // Horizontal splitter between tbx3 and tbx4
        DEFINE SPLITTER splitter7;
            HORIZONTAL;
            USEGRADIENT;                   // Use gradient color effect
            SPLIT {"tbx3"} FROM {"tbx4"}

        // Another vertical splitter between browse and additional editboxes
        DEFINE SPLITTER splitter2;
            VERTICAL;
            NOARROW;                       // Disable arrows
            COLOR {23, 69, 110};           // Color of the splitter
            BACKCOLOR {23, 133, 230};      // Background color of the splitter
            COLORHOVER {87, 23, 66};       // Color when hovering over the splitter
            BACKCOLORHOVER {173, 29, 126}; // Background color on hover
            SPLIT {"brow"} FROM {"tbx3", "tbx4", "splitter7"};
            LIMITS {5, 5}

        // Define a tree control with hierarchical nodes and tree items
        DEFINE TREE tree;
            AT 425, 0;
            WIDTH 390 HEIGHT 500;
            VALUE 15;                      // Initial selected item
            INDENT 20;
            ITEMHEIGHT 20                  // Height of each tree item

            // Define tree nodes and items
            NODE 'Node A'
                TREEITEM 'Item A.1'
                TREEITEM 'Item A.2' 
                TREEITEM 'Item A.3'
                TREEITEM 'Item A.4'
                TREEITEM 'Item A.5'
            END NODE

            NODE 'Node B'
                TREEITEM 'Item B.1'
                TREEITEM 'Item B.2'

                NODE 'Node B2'
                    TREEITEM 'Item B2.1'
                    TREEITEM 'Item B2.2'
                    TREEITEM 'Item B2.3'
                    TREEITEM 'Item B2.4'
                    TREEITEM 'Item B2.5'
                    TREEITEM 'Item B2.6'
                END NODE

                TREEITEM 'Item B.3'
                TREEITEM 'Item B.4'
            END NODE

            NODE 'Node C'
                TREEITEM 'Item C.1'
                TREEITEM 'Item C.2' 
                TREEITEM 'Item C.3'
                TREEITEM 'Item C.4'
                TREEITEM 'Item C.5' 
            END NODE    

            NODE 'Node D'
                TREEITEM 'Item D.1'

                NODE 'D2'
                    TREEITEM 'Item D2.1'

                    NODE 'D3'
                        TREEITEM 'Item D3.1'
                        TREEITEM 'Item D3.2'
                    END NODE

                    TREEITEM 'Item D2.2'
                END NODE

                TREEITEM 'Item D.2'
            END  NODE  
        END TREE

        // Define buttons below the tree control
        @ 475, 400 BUTTON but1;
            CAPTION "Button 1";
            WIDTH 200 HEIGHT 125;

        @ 625, 425 BUTTON but2;
            CAPTION "Button 2";
            WIDTH 150 HEIGHT 100;

        @ 750, 400 BUTTON but3;
            CAPTION "Button 3";
            WIDTH 200 HEIGHT 125;

        // Vertical splitter between the tree and buttons
        DEFINE SPLITTER splitter8;
            VERTICAL USEGRADIENT;
            SPLIT {"tree"} FROM {"but1", "but2", "but3"}

        // Define a grid control to display generated data
        @ 425, 615 GRID grid1;
            WIDTH 500 HEIGHT 500;
            HEADERS {'First Name', 'Last Name', 'Age', 'Gender'};
            ITEMS GenerateGridData();       // Function to generate grid data
            WIDTHS {150, 150, 100, 100};
            EDIT;
            JUSTIFY {BROWSE_JTFY_LEFT, BROWSE_JTFY_LEFT, BROWSE_JTFY_CENTER, BROWSE_JTFY_LEFT}

        // Vertical splitter between the grid and other controls
        DEFINE SPLITTER splitter3;
            VERTICAL;
            NOARROW;
            USEGRADIENT NOHOVER;
            GRADIENT {222, 168, 20}, {209, 209, 36};   // Gradient colors
            SPLIT {"tree", "but1", "but2", "but3", "splitter8"} FROM {"grid1"};
            LIMITS {40, 15}

        // Define listboxes for additional data display
        @ 425, 1130 LISTBOX list1;
            WIDTH 250 HEIGHT 240;
            ITEMS {'Item 1', 'Item 2', 'Item 3', 'Item 4', 'Item 5'}

        @ 675, 1130 LISTBOX list2;
            WIDTH 250 HEIGHT 250;
            ITEMS {'Item 6', 'Item 7', 'Item 8', 'Item 9', 'Item 10'}

        // Horizontal splitter between two listboxes
        DEFINE SPLITTER splitter9;
            HORIZONTAL;
            SPLIT {"list1"} FROM {"list2"}

        // Define a vertical splitter between the grid and the two listboxes
        DEFINE SPLITTER splitter4;
            VERTICAL;
            USEGRADIENT;
            GRADIENT {144, 209, 182}, {92, 184, 145};   // Gradient color settings
            GRADIENTHOVER {92, 184, 145}, {42, 140, 99}; // Gradient hover settings
            SPLIT {"grid1"} FROM {"list1", "list2", "splitter9"};
            LIMITS {10, 10}                   // Define the minimum and maximum size limits

        // Final horizontal splitter combining various sections of the layout
        DEFINE SPLITTER splitter5;
            HORIZONTAL;
            USEGRADIENT;
            GRADIENT {191, 191, 64}, {89, 166, 89};
            GRADIENTHOVER {176, 245, 142}, {39, 64, 26}, {208, 230, 14};
            SPLIT {"editbox", "tbx2", "tbx3", "tbx4", "brow", "splitter1", "splitter2", "splitter6", "splitter7"}; 
                 FROM {"tree", "but1", "but2", "but3", "grid1", "splitter3", "list1", "list2", "splitter4", "splitter8", "splitter9"};
            LIMITS {5, 5}

    END WINDOW

    // Activate the main window
    ACTIVATE WINDOW Win_1

Return

// Open the DBF file for the browse control
Procedure OpenDBF
    If !File("Test.dbf")
        GenerateTable()                    // If DBF doesn't exist, generate a new one
    EndIf

    Use Test
    Go Top
    Win_1.brow.Value := RecNo()            // Set the current record in the browse
Return

// Close the DBF file when the window is closed
Procedure CloseDBF
    Use
Return

// Generate data for the grid control
Function GenerateGridData
    Local aRows[50][4]                     // Define a 50-row array with 4 columns
    Local i

    For i := 1 To 50
        aRows[i][1] := 'First Name ' + hb_ntos(i)
        aRows[i][2] := 'Second Name ' + hb_ntos(i)
        aRows[i][3] := 'Age - ' + hb_ntos(i)
        aRows[i][4] := If(i % 2 = 0, 'Male', 'Female') // Alternate between Male and Female
    Next i 

Return aRows

// Generate the DBF structure and fill it with sample data
Procedure GenerateTable
    Local dbfStruct := {;
        {'lp', 'N', 10, 0},;
        {'fname', 'C', 30, 0},;
        {'lname', 'C', 30, 0},;
        {'gender', 'C', 1, 0},;
        {'married', 'L', 1, 0},;
        {'birth', 'D', 8, 0},;
        {'bio', 'M', 50, 0};
    }
    Local i
    
    DBCreate("Test", dbfStruct)             // Create the 'Test' DBF file
    Use Test

    // Populate the DBF with 100 records
    For i:= 1 To 100
        Append blank
        Replace &(dbfStruct[1][1]) With i 
        Replace &(dbfStruct[2][1]) With 'First Name ' + hb_ntos(i)
        Replace &(dbfStruct[3][1]) With 'Last Name ' + hb_ntos(i)
        Replace &(dbfStruct[4][1]) With If(i % 2 = 0, 'M', 'F')
        Replace &(dbfStruct[5][1]) With If(i % 2 = 0 .OR. i % 3 = 0, .T., .F.)
        Replace &(dbfStruct[6][1]) With date() - i
    Next i
    
    Use
    
Return
