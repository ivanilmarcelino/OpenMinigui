/*
 * Harbour MiniGUI Splitter Demo (Updated)
 */
ANNOUNCE RDDSYS

#include "minigui.ch"

PROCEDURE Main()

    LOCAL VerticalSplitter, HorizontalSplitter, VerticalSplitterPage1, HorizontalSplitterPage1

    // Enable automatic resizing of controls when the window is resized
    SET AUTOADJUST ON

    // Define the main window
    DEFINE WINDOW MainWindow ;
        AT 0, 0 ;
        WIDTH 800 ;
        HEIGHT 650 ;
        TITLE "Vertical and Horizontal SPLITTER in Tab" ;
        MAIN

    // Define the main menu
    DEFINE MAIN MENU
        DEFINE MENU POPUP "File" NAME oNMenu_MAIN_Main1
            MENUITEM "Hide" ACTION MainWindow.HorizontalSplitter2.Hide
            MENUITEM "Show" ACTION MainWindow.HorizontalSplitter2.Show
        END POPUP
    END MENU

    // Define a tab control in the main window
    @ 20, 20 TAB TabMain ;
      WIDTH 750 ;
      HEIGHT 560

      // Tab page 1
      PAGE "Page 1"
        // Define two EditBoxes for the vertical splitter in Page 1
        @ 50, 50 EDITBOX LeftEditBoxPage1 ;
            WIDTH 320 ;
            HEIGHT 250 ;
            VALUE "Left Pane - Page 1" ;
            NOVSCROLL NOHSCROLL

        @ 50, 380 EDITBOX RightEditBoxPage1 ;
            WIDTH 320 ;
            HEIGHT 250 ;
            VALUE "Right Pane - Page 1" ;
            NOVSCROLL NOHSCROLL

        // Define a VERTICAL SPLITTER between the two EditBoxes in Page 1
        DEFINE SPLITTER VerticalSplitterPage1 ;
            AT 50, 370 ;                  // Position the splitter between controls
            WIDTH 10 ;                    // Width of the splitter
            HEIGHT 250 ;                  // Height of the splitter
            VERTICAL ;                    // Vertical splitter
            SPLIT {"LeftEditBoxPage1"} FROM {"RightEditBoxPage1"} ;  // Split the two controls
            COLOR {0, 128, 255} ;         // Set the color of the splitter
            BACKCOLOR {230, 230, 230} ;   // Background color
            USEGRADIENT ;                 // Enable gradient effect
            GRADIENT {255, 255, 255}, {0, 128, 255}, {0, 0, 128} ;  // Gradient colors
            GRADIENTHOVER {255, 200, 200}, {200, 50, 50}, {100, 0, 0} ;  // Hover gradient colors
            HIDEARROW ;                   // Hide the arrow icon
            LIMITS {5, 10} ;              // Define the minimum and maximum limits for each section
            ROUNDED

        // Define two more EditBoxes for the horizontal splitter in Page 1
        @ 320, 50 EDITBOX TopEditBoxPage1 ;
            WIDTH 650 ;
            HEIGHT 100 ;
            VALUE "Top Pane - Page 1" ;
            NOVSCROLL NOHSCROLL

        @ 430, 50 EDITBOX BottomEditBoxPage1 ;
            WIDTH 650 ;
            HEIGHT 100 ;
            VALUE "Bottom Pane - Page 1" ;
            NOVSCROLL NOHSCROLL

        // Define a HORIZONTAL SPLITTER between the two EditBoxes in Page 1
        DEFINE SPLITTER HorizontalSplitterPage1 ;
            AT 420, 50 ;                  // Position the splitter between controls
            WIDTH 650 ;                   // Width of the splitter
            HEIGHT 10 ;                   // Height of the splitter
            HORIZONTAL ;                  // Horizontal splitter
            SPLIT {"TopEditBoxPage1"} FROM {"BottomEditBoxPage1"} ;  // Split the two controls
            COLOR {255, 128, 0} ;         // Set the color of the splitter
            BACKCOLOR {230, 230, 230} ;   // Background color
            USEGRADIENT ;                 // Enable gradient effect
            GRADIENT {255, 255, 255}, {255, 128, 0}, {255, 64, 0} ;  // Gradient colors
            GRADIENTHOVER {255, 200, 200}, {255, 100, 100}, {200, 0, 0} ;  // Hover gradient colors
            LIMITS {5, 15} ;              // Define the minimum and maximum limits for each section
            ROUNDED

      END PAGE

      // Tab page 2
      PAGE "Page 2"

        // Define two EditBoxes for the vertical splitter in Page 2
        @ 50, 50 EDITBOX LeftEditBox2 ;
            WIDTH 320 ;
            HEIGHT 250 ;
            VALUE "Left Pane - Page 2" ;
            NOVSCROLL NOHSCROLL

        @ 50, 380 EDITBOX RightEditBox2 ;
            WIDTH 320 ;
            HEIGHT 250 ;
            VALUE "Right Pane - Page 2" ;
            NOVSCROLL NOHSCROLL

        // Define a VERTICAL SPLITTER between the two EditBoxes in Page 2
        DEFINE SPLITTER VerticalSplitter2 ;
            AT 50, 370 ;                  // Position the splitter between controls
            WIDTH 10 ;                    // Width of the splitter
            HEIGHT 250 ;                  // Height of the splitter
            VERTICAL ;                    // Vertical splitter
            SPLIT {"LeftEditBox2"} FROM {"RightEditBox2"} ;  // Split the two controls
            COLOR {0, 128, 255} ;         // Set the color of the splitter
            BACKCOLOR {230, 230, 230} ;   // Background color
            USEGRADIENT ;                 // Enable gradient effect
            GRADIENT {255, 255, 255}, {0, 128, 255}, {0, 0, 128} ;  // Gradient colors
            GRADIENTHOVER {255, 200, 200}, {200, 50, 50}, {100, 0, 0} ;  // Hover gradient colors
            HIDEARROW ;                   // Hide the arrow icon
            LIMITS {5, 10} ;              // Define the minimum and maximum limits for each section
            ROUNDED

        // Define two more EditBoxes for the horizontal splitter in Page 2
        @ 320, 50 EDITBOX TopEditBox2 ;
            WIDTH 650 ;
            HEIGHT 100 ;
            VALUE "Top Pane - Page 2" ;
            NOVSCROLL NOHSCROLL

        @ 430, 50 EDITBOX BottomEditBox2 ;
            WIDTH 650 ;
            HEIGHT 100 ;
            VALUE "Bottom Pane - Page 2" ;
            NOVSCROLL NOHSCROLL

        // Define a HORIZONTAL SPLITTER between the two EditBoxes in Page 2
        DEFINE SPLITTER HorizontalSplitter2 ;
            AT 420, 50 ;                  // Position the splitter between controls
            WIDTH 650 ;                   // Width of the splitter
            HEIGHT 10 ;                   // Height of the splitter
            HORIZONTAL ;                  // Horizontal splitter
            SPLIT {"TopEditBox2"} FROM {"BottomEditBox2"} ;  // Split the two controls
            COLOR {255, 128, 0} ;         // Set the color of the splitter
            BACKCOLOR {230, 230, 230} ;   // Background color
            USEGRADIENT ;                 // Enable gradient effect
            GRADIENT {255, 255, 255}, {255, 128, 0}, {255, 64, 0} ;  // Gradient colors
            GRADIENTHOVER {255, 200, 200}, {255, 100, 100}, {200, 0, 0} ;  // Hover gradient colors
            LIMITS {5, 15} ;              // Define the minimum and maximum limits for each section
            ROUNDED

      END PAGE
    END TAB

    END WINDOW

    // Center the main window
    CENTER WINDOW MainWindow

    // Activate the main window
    ACTIVATE WINDOW MainWindow

RETURN
