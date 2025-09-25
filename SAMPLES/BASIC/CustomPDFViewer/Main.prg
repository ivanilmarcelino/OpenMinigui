#include "minigui.ch"

// Request the definition of a custom PDF viewer control
REQUEST DefinePDFViewer

// Command definition for creating a PDF viewer control in the window
#command @ <row>,<col> PDFVIEWER <name> ;
		[ <dummy1: OF, PARENT> <parent> ] ;
		[ WIDTH <w> ] ;
		[ HEIGHT <h> ] ;
		FILE <file> ;
		[ <invisible : INVISIBLE> ] ;
	=>;
      DefinePDFViewer( <(name)>, <(parent)>, <col>, <row>, <w>, <h>, <file>, <.invisible.> )

/*
 * FUNCTION Main()
 *
 * Initializes the main application window and sets up the UI elements for a PDF viewer.
 *
 * Purpose:
 *   This function serves as the entry point for the application. It:
 *     1. Defines the main window with specific dimensions and title.
 *     2. Sets up event handlers for window resizing and maximizing to adjust the PDF viewer size accordingly.
 *     3. Defines a button to open a PDF file.
 *     4. Activates the main window to display it.
 *   Example: Called automatically when the application starts to set up the UI.
 *
 * Notes:
 *   The window contains a button that, when clicked, allows the user to select and view a PDF file.
 *   Ensure that the PDF viewer control is properly defined and accessible.
 */
FUNCTION Main()
   LOCAL cControlName := "pdf1" // Name of the PDF viewer control

   // Define the main window of the application
   DEFINE WINDOW Form_1 ;
      AT 0,0 ;
      WIDTH 800 ;
      HEIGHT 620 ;
      TITLE "Embedded PDF Viewer" ;
      MAIN ;
      ON MAXIMIZE iif( _IsControlDefined( cControlName, This.Name ), ( Form_1.(cControlName).Width := (Form_1.Width) - 40, Form_1.(cControlName).Height := (Form_1.Height) - 100 ), ) ;
      ON SIZE iif( _IsControlDefined( cControlName, This.Name ), ( Form_1.(cControlName).Width := (Form_1.Width) - 40, Form_1.(cControlName).Height := (Form_1.Height) - 100 ), )

      // Define a button to open a PDF file
      DEFINE BUTTON Button_1
         ROW 10
         COL 10
         WIDTH 120
         CAPTION "Open PDF"
         ACTION ViewPDF( cControlName, ThisWindow.Name )
      END BUTTON
   END WINDOW

   // Activate the main window to display it
   ACTIVATE WINDOW Form_1

RETURN NIL

/*
 * FUNCTION ViewPDF( cpdf1, cForm )
 *
 * Opens a PDF file in the PDF viewer control.
 *
 * Parameters:
 *   cpdf1 (Character): The name of the PDF viewer control.
 *   cForm (Character): The name of the form containing the PDF viewer control.
 *
 * Returns:
 *   Nil
 *
 * Purpose:
 *   This function allows the user to select a PDF file and display it in the PDF viewer control. It:
 *     1. Opens a file dialog to select a PDF file.
 *     2. Releases the existing PDF viewer control if it exists.
 *     3. Creates a new PDF viewer control and loads the selected PDF file.
 *
 * Notes:
 *   Ensure that the PDF viewer control is properly released before creating a new one to avoid memory leaks.
 *   The function uses a file dialog to allow the user to select a PDF file.
 */
FUNCTION ViewPDF( cpdf1, cForm )
   LOCAL cPDF := GetFile( { {"PDF files", "*.pdf"} }, "Select PDF File" ) // Open file dialog to select a PDF file

   // Check if a PDF file was selected
   IF !Empty( cPDF )
      // Check if the PDF viewer control already exists
      IF _IsControlDefined( cpdf1, cForm )
         ReleasePDFViewer( cForm, cpdf1 ) // Release the existing PDF viewer control
      ENDIF

      // Create a new PDF viewer control and load the selected PDF file
      @ 10, 50 PDFViewer (cpdf1) ;
         Parent (cForm) ;
         Width Form_1.Width - 40 ;
         Height Form_1.Height - 100 ;
         File cPDF
   ENDIF

RETURN NIL
