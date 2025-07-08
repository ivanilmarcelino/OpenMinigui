/*
 * HMG Drag and Drop demo
 *
 * This program demonstrates drag-and-drop functionality in an HMG-based GUI application.
 * Users can drag files from the file explorer and drop them into various controls within the window.
*/

#include "hmg.ch"  // Include HMG library for GUI components

FUNCTION Main()

   // Define the main window
   DEFINE WINDOW oWindow1;
      ROW    10;                    // Window's top position
      COL    10;                    // Window's left position
      WIDTH  500;                   // Window's width
      HEIGHT 400;                   // Window's height
      TITLE  'Drag and Drop demo';  // Window title
      WINDOWTYPE MAIN;              // Define window type as MAIN
      ONINIT oWindow1.Center()      // Center the window when initialized

      // Set event to handle drag-and-drop files into the window
      This.OnDropFiles := {| aFiles | ResolveDrop( "oWindow1", HMG_GetFormControls( "oWindow1" ), aFiles ) }

      // Define a label
      DEFINE LABEL NUL 
         ROW 10
         COL 10
         WIDTH 300
         HEIGHT 40
         ALIGNMENT CENTER            // Align the text to center
         ALIGNMENT VCENTER           // Vertically center the text
         BORDER .T.                  // Show border around the label
         VALUE "Drag one or more files from explorer into the controls."
      END LABEL

      // Define a checkbox
      DEFINE CHECKBOX NUL
         ROW      10
         COL      350
         VALUE    .F.                // Default value is unchecked (false)
         CAPTION  'CheckBox!!!'      // Checkbox label
      END CHECKBOX

      // Define a combobox (dropdown list)
      DEFINE COMBOBOX NUL
         ROW      40
         COL      350
         WIDTH    100                // Width of the combobox
         ITEMS    { "E - Item 1", "D - Item 2", "C - Item 3", "B - Item 4", "A - Item 5" }  // Items in the combobox
         VALUE    3                  // Default selected item (3rd item)
      END COMBOBOX

      // Define a date picker
      DEFINE DATEPICKER NUL
         ROW    80
         COL    350
         VALUE  DATE()               // Default value is the current date
         UPDOWN .T.                  // Show up/down controls for the date picker
      END DATEPICKER

      // Define an editbox (multi-line text input)
      DEFINE EDITBOX NUL
         ROW        60
         COL        10
         WIDTH      300
         HEIGHT     100
         VALUE      "Checkbox, Combobox, Datepicker, Label, Textbox, " + ;
                    "Editbox, Listbox and Tree work well and accept OnDropFiles event."  // Default text in the editbox
         NOHSCROLLBAR .T.            // Disable horizontal scroll bar
      END EDITBOX

      // Define a listbox (single selection list)
      DEFINE LISTBOX NUL
         ROW        170
         COL        10
         WIDTH      150
         HEIGHT      80
         ITEMS      {"Item 1","Item 2","Item 3","Item 4","Item 5"}  // Items in the listbox
         VALUE      3                // Default selected item (3rd item)
      END LISTBOX

      // Define a textbox (single-line text input)
      DEFINE TEXTBOX NUL
         ROW     260
         COL     10
         WIDTH   300
         VALUE   'TextBox'           // Default value in the textbox
      END TEXTBOX

      // Define a tree control (hierarchical structure)
      DEFINE TREE NUL ;
         ROW     120  ;
         COL     350  ;
         WIDTH   120  ;
         HEIGHT  200

         // Define nodes and tree items
         NODE "Item1"
            TREEITEM "Item1.1"
            TREEITEM "Item1.2"
         END NODE

         NODE "Item2"
            TREEITEM "Item2.1"
            NODE "Item2.2"
               TREEITEM "Item2.2.1"
            END NODE
            TREEITEM "Item2.3"
         END NODE

      END TREE

   END WINDOW

   // Activate the window (make it visible and interactive)
   ACTIVATE WINDOW oWindow1

RETURN NIL

/*..............................................................................
   Drop Event Processing
   This function processes files dropped into the controls of the window.
   It determines where the files were dropped and displays an alert with details.
..............................................................................*/
FUNCTION ResolveDrop( cForm, aCtrl, aFiles )

   LOCAL mx, my, ni, tx, ty, bx, by, ct
   LOCAL aRect := { 0, 0, 0, 0 }, txt := ""
   LOCAL aCtlPos := {}
   LOCAL cTarget := ""

   // Get the cursor position on the screen when the files are dropped
   my := GetCursorRow()  // Mouse Y position on the desktop
   mx := GetCursorCol()  // Mouse X position on the desktop

   // Get the position and dimensions of each control in the form
   FOR ni = 1 TO Len( aCtrl )
      GetWindowRect( GetControlHandle( aCtrl[ ni ], cForm ), aRect )
      AAdd( aCtlPos, { aCtrl[ ni ], aRect[ 1 ], aRect[ 2 ], aRect[ 3 ], aRect[ 4 ] } )
   NEXT ni

   // Check which control the mouse cursor was over when the files were dropped
   ni := 0
   DO WHILE ni ++ < Len( aCtlPos )
      tx := aCtlPos[ ni, 2 ]  // Top-left corner X
      ty := aCtlPos[ ni, 3 ]  // Top-left corner Y
      bx := aCtlPos[ ni, 4 ]  // Bottom-right corner X
      by := aCtlPos[ ni, 5 ]  // Bottom-right corner Y
      IF mx >= tx .AND. mx <= bx .AND. my >= ty .AND. my <= by
         cTarget := aCtlPos[ ni, 1 ]  // Set target control based on mouse position
         EXIT
      ENDIF
   ENDDO

   // If a target control is identified, process the files dropped into it
   IF Len( cTarget ) > 0
      ct := GetControlType( cTarget, cForm )  // Get the control type (e.g., textbox, listbox)
      FOR ni = 1 TO Len(aFiles)
         txt += aFiles[ni] + " received into " + cTarget + " (" + ct + ")" + CRLF  // Prepare message
      NEXT ni
      AlertInfo( txt )  // Display an alert with the list of files received and the target control
   ENDIF

RETURN NIL
