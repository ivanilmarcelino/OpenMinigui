/*
 * HMG Miscellaneous Functions Demo
 *
 * Demonstrates how to use HMG functions to interact with system folders, the clipboard,
 * desktop size, and more. This program provides a window-based interface to access
 * these functions using a menu system.
*/

#include "hmg.ch"  // Include HMG library, which provides access to HMG-specific functions and objects

FUNCTION Main()

   // Define the main window of the application
   DEFINE WINDOW oWindow1 ;
      ROW 10 ;                          // Set the top position of the window (row)
      COL 10 ;                          // Set the left position of the window (column)
      WIDTH 400 ;                       // Set the width of the window
      HEIGHT 400 ;                      // Set the height of the window
      TITLE 'HMG misc funcs/objects' ;  // Title of the window
      WindowType MAIN ;                 // Window type is "Main"
      OnInit oWindow1.Center()          // On window initialization, center it on the screen

      // Define the main menu for the application
      DEFINE MAIN MENU

         // Define a popup menu for miscellaneous functions
         DEFINE POPUP 'Misc functions'

            // Define a submenu for folder-related functions
            DEFINE POPUP 'Folders'
               MENUITEM 'Get Desktop Folder' ONCLICK MsgInfo( GetDesktopFolder() ) // Display the desktop folder path
               MENUITEM 'Get MyDocuments Folder' ONCLICK MsgInfo( GetMyDocumentsFolder() ) // Display the "My Documents" folder path
               MENUITEM 'Get Program Files Folder' ONCLICK MsgInfo( GetProgramFilesFolder() ) // Display the "Program Files" folder path
               MENUITEM 'Get Temp Folder' ONCLICK MsgInfo( GetTempFolder() ) // Display the temporary files folder path
               MENUITEM 'Get Windows Temp Folder' ONCLICK MsgInfo( GetWindowsTempFolder() ) // Display the Windows temporary files folder path
               MENUITEM 'Get Clipboard' ONCLICK MsgInfo( RetrieveTextFromClipboard() ) // Display the current content of the clipboard
               MENUITEM 'Set Clipboard' ONCLICK CopyToClipboard( 'New Clipboard Value' ) // Set a new value in the clipboard
               MENUITEM 'Clean Temp Folder' ONCLICK MsgInfo( DirRemoveAllExceptParent( GetTempFolder() ) ) // Clean the temp folder except for the parent folder
               MENUITEM 'Clean User Temp Folder' ONCLICK MsgInfo( DirRemoveAllExceptParent( GetUserTempFolder() ) ) // Clean the user's temp folder
               MENUITEM 'Clean AppData CrashDumps Folder' ONCLICK MsgInfo( DirRemoveAllExceptParent( GetAppLocalDataFolder() + "\CrashDumps" ) )

               // If the temp folder and Windows temp folder are not the same, add an option to clean the Windows temp folder
               IF !( GetTempFolder() == GetWindowsTempFolder() )
                  MENUITEM 'Clean Windows Temp Folder' ONCLICK MsgInfo( DirRemoveAllExceptParent( GetWindowsTempFolder() ) ) // Clean the Windows temp folder
               ENDIF
            END POPUP

            // Define a submenu for system objects functions
            DEFINE POPUP 'System.Objects'
               MENUITEM 'System.DesktopFolder' ONCLICK MsgInfo( System.DesktopFolder ) // Display the system's desktop folder path
               MENUITEM 'System.MyDocumentsFolder' ONCLICK MsgInfo( System.MyDocumentsFolder ) // Display the system's My Documents folder path
               MENUITEM 'System.ProgramFilesFolder' ONCLICK MsgInfo( System.ProgramFilesFolder ) // Display the system's Program Files folder path
               MENUITEM 'System.TempFolder' ONCLICK MsgInfo( System.TempFolder ) // Display the system's temporary files folder path
               MENUITEM 'System.Clipboard' ONCLICK MsgInfo( System.Clipboard ) // Display the content of the system clipboard
               MENUITEM 'System.Clipboard := "New Value"' ONCLICK System.Clipboard := "New Value" // Set a new value in the system clipboard
               MENUITEM 'System.DefaultPrinter' ONCLICK MsgInfo( System.DefaultPrinter ) // Display the system's default printer
            END POPUP

            // Define a submenu for desktop size functions
            DEFINE POPUP 'Desktop Size'
               MENUITEM 'Width' ONCLICK MsgInfo( GetDesktopWidth() ) // Display the desktop width
               MENUITEM 'Client Width' ONCLICK MsgInfo( System.ClientWidth ) // Display the window client area width
               MENUITEM 'Height' ONCLICK MsgInfo( GetDesktopHeight() ) // Display the desktop height
               MENUITEM 'Client Height' ONCLICK MsgInfo( System.ClientHeight ) // Display the window client area height
            END POPUP

         END POPUP

      END MENU

   END WINDOW

   // Activate and display the main window
   ACTIVATE WINDOW oWindow1

RETURN NIL

// -----------------------------------------------------------------------------
// Helper function to clean all files and folders in the directory except for the parent folder
// It removes both files and subdirectories within the given directory
// -----------------------------------------------------------------------------
#include "directry.ch"  // Include directory management functions
#include "fileio.ch"    // Include file input/output functions

FUNCTION DirRemoveAllExceptParent( cDir )

   LOCAL aFile, cPath, cFile, nAttr, lSuccess := .T. // Local variables

   // Check if the directory exists and is not empty
   IF ! Empty( cDir ) .AND. hb_vfDirExists( cDir )
      cPath := hb_DirSepAdd( cDir ) // Ensure the directory path has the appropriate separator

      // Iterate over all files and directories in the specified path
      FOR EACH aFile IN hb_vfDirectory( cPath + hb_osFileMask(), "HSD" )

         // If the entry is a directory (i.e., attribute contains "D")
         IF "D" $ aFile[ F_ATTR ]

            // Skip special directories like "." and ".."
            IF !( aFile[ F_NAME ] == "." .OR. aFile[ F_NAME ] == ".." .OR. aFile[ F_NAME ] == "" )
               // Recursively remove the directory and its contents
               IF ! hb_DirRemoveAll( cPath + aFile[ F_NAME ] )
                  lSuccess := .F. // Set success flag to false if removal fails
               ENDIF
            ENDIF

         ELSE

            // Handle file removal

            // Check if the file is read-only and change its attributes if necessary
            cFile := cPath + aFile[ F_NAME ]
            IF "R" $ aFile[ F_ATTR ] .AND. hb_vfAttrGet( cFile, @nAttr )
               hb_vfAttrSet( cFile, hb_bitAnd( nAttr, hb_bitNot( HB_FA_READONLY ) ) )
            ENDIF

            // Erase the file, and set success flag to false if deletion fails
            IF ! hb_vfErase( cFile ) == 0
               lSuccess := .F.
            ENDIF

         ENDIF

      NEXT

      RETURN lSuccess // Return success or failure of the cleanup
   ENDIF

RETURN lSuccess // Default return in case the directory doesn't exist or is empty
