/*
 * HMG Tab demo
 * (c) 2010 Roberto Lopez <mail.box.hmg@gmail.com>
 * This example demonstrates the usage of tabs and basic controls in an HMG window.
 */

#include "hmg.ch"  // Include the HMG library for GUI components

FUNCTION Main()

   LOCAL cTabCtl  // Declare a local variable to store the tab control reference

   // Define the main window
   DEFINE WINDOW oWindow1 ;
      Row     10 ;              // Window's row position
      Col     10 ;              // Window's column position
      Width  400 ;              // Window's width
      Height 400 ;              // Window's height
      Title  'Tab Pages Demo' ; // Window's title
      WindowType Main ;         // This is the main window
      OnInit oWindow1.Center()  // Center the window upon initialization

      // Define the main menu
      DEFINE MAIN MENU

         // Menu section for "Page Tests"
         POPUP 'Page Tests'
            // Change the caption of Page 1
            ITEM 'Set Page1 Caption'   ACTION oWindow1.(cTabCtl).Caption( 1 ) := 'New'
            // Change the image of Page 1
            ITEM 'Set Page1 Image'     ACTION oWindow1.(cTabCtl).Image( 1 )   := 'save.png'
            // Set the tooltip for Page 1
            ITEM 'Set Page1 ToolTip'   ACTION oWindow1.(cTabCtl).ToolTip( 1 ) := 'Page ToolTip'
            SEPARATOR  // Add a separator between menu items
            // Disable Page 1
            ITEM 'Set Page1 Disable'   ACTION oWindow1.(cTabCtl).Enabled( 1 ) := .F.
            // Enable Page 1
            ITEM 'Set Page1 Enable'    ACTION oWindow1.(cTabCtl).Enabled( 1 ) := .T.
            SEPARATOR
            // Get the caption of Button1 on Page 1
            MENUITEM 'Get Button1 Caption' ACTION MsgInfo ( oWindow1.&cTabCtl.( 1 ).Button1.Caption )
            // Set a new caption for Button1 on Page 1
            MENUITEM 'Set Button1 Caption' ACTION oWindow1.&cTabCtl.( 1 ).Button1.Caption := 'New Caption'
            SEPARATOR
            // Show Button1 on Page 1
            MENUITEM 'Show Button1' ACTION oWindow1.&cTabCtl.( 1 ).Button1.Show()
            // Hide Button1 on Page 1
            MENUITEM 'Hide Button1' ACTION oWindow1.&cTabCtl.( 1 ).Button1.Hide()
         END POPUP

         // Menu section for "Tab Tests"
         POPUP 'Tab Tests'
            // Set the active tab to Page 1
            ITEM 'Set Active Page To 1'        ACTION oWindow1.(cTabCtl).Value := 1
            // Set the active tab to Page 2
            ITEM 'Set Active Page To 2'        ACTION oWindow1.(cTabCtl).Value := 2
            SEPARATOR
            // Set an event that triggers when the tab changes
            ITEM 'Set OnChange Event'          ACTION oWindow1.(cTabCtl).OnChange := { || MsgInfo( 'Changed' ) }
            // Clear the OnChange event
            ITEM 'Clear OnChange Event'        ACTION oWindow1.(cTabCtl).OnChange := { || NIL }
            SEPARATOR
            // Delete Page 2 from the tab
            ITEM 'Delete Page 2'               ACTION oWindow1.(cTabCtl).DeletePage( 2 )
            // Add a new page at position 2 with an image
            ITEM 'Add Page At Position 2'      ACTION ( oWindow1.(cTabCtl).Image( 1 )   := 'save.png', ;
                                                      oWindow1.(cTabCtl).AddPage( 2, 'New Page', 'open.png' ) )
         END POPUP

      END MENU

      // Define a tab control with two pages
      DEFINE TAB NUL AT 20, 10 Width 350 Height 300

         // First tab page named "One"
         DEFINE PAGE 'One'

            // Define a button on Page 1
            DEFINE BUTTON Button1
               Row     60           // Button position (row)
               Col     20           // Button position (column)
               Caption 'Button Caption'  // Initial button caption
               OnClick MsgInfo( 'Click!' )  // Display message on button click
            END BUTTON

         END PAGE

         // Second tab page named "Two"
         DEFINE PAGE 'Two'

            // Define an editbox on Page 2
            DEFINE EDITBOX Edit1
               Row        60              // Editbox position (row)
               Col        20              // Editbox position (column)
               Width      200             // Editbox width
               Height     100             // Editbox height
               Value      'EditBox Text'  // Initial value of the editbox
               HScrollBar .F.             // Disable horizontal scrollbar
            END EDITBOX

         END PAGE

      END TAB

      // Store the reference to the first tab control in the variable `cTabCtl`
      cTabCtl := HMG_GetFormControls( This.Name, "TAB" )[ 1 ]

   END WINDOW

   ACTIVATE WINDOW oWindow1  // Activate and display the main window

RETURN NIL  // Return NIL to end the function
