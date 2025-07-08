/*
   MINIGUI - Harbour Win32 GUI library Demo

   Purpose is to test the hb_DirScan() function which
   allows you to scan an entire directory with subdirectories
   and some other interesting hb_* functions to split filename from directory name.
*/

#include "minigui.ch"
#include "directry.ch"

FUNCTION Main()
/*
   Purpose:  This is the main function of the application. It defines the main window,
             its controls (grid, button, menu, statusbar), and sets up the initial
             state of the application. It also handles window resizing events.
*/
   LOCAL cSMsg := ""
   LOCAL aHeaders := { 'Select', 'Path', 'Name', 'Bytes', 'Date', 'Time', 'Attr' }
   LOCAL aLijst := {}

   SET DATE FORMAT TO "dd.mm.yy"

   DEFINE WINDOW MainForm ;
      AT 90, 90 ;
      WIDTH 920 ;
      HEIGHT 550 ;
      TITLE 'Directory Scan for *.prg files' ;
      MAIN ;
      ON INIT Resize() ;
      ON MAXIMIZE Resize() ;
      ON SIZE Resize()

   DEFINE MAIN MENU of MainForm
      DEFINE POPUP "File"
         MENUITEM "Directory" ACTION DoFolder( "" )
         SEPARATOR
         MENUITEM "Exit" ACTION ThisWindow.Release
      END POPUP
      DEFINE POPUP "Help"
         MENUITEM "About" ACTION About()
      END POPUP
   END MENU

   DEFINE STATUSBAR FONT "Verdana" SIZE 10
      STATUSITEM cSMsg
      CLOCK WIDTH 90
      DATE WIDTH 90
   END STATUSBAR

   @ 005, 005 GRID Grid_1 ;
      WIDTH 1000 HEIGHT 425 ;
      HEADERS aHeaders ;
      WIDTHS { 20, 300, 244, 100, 80, 80, 50 } ;
      ITEMS aLijst ;
      FONT "Courier New" SIZE 10 ;
      FONTCOLOR BLUE ;
      ON DBLCLICK  SelectMe() ;
      LOCKCOLUMNS 1

      @ 440, 50 BUTTON Button_1 CAPTION "Select All" WIDTH 120 HEIGHT 28 ACTION SelectAll()

   END WINDOW

   MainForm.Center
   MainForm.Activate

RETURN NIL

PROCEDURE About()
/*
   Purpose:  Displays an "About" dialog box with information about the application
             and the MiniGUI version.
*/
   DEFINE FONT DlgFont FONTNAME "Verdana" SIZE 10

   AlertInfo( "hb_DirScan() Function Example;;" + MiniguiVersion() )

   RELEASE FONT DlgFont

RETURN

PROCEDURE Resize()
/*
   Purpose:  Handles the resizing of the main window and adjusts the size and position
             of the grid and button controls accordingly. This ensures that the layout
             remains consistent when the window is resized.
*/
   MainForm.Grid_1.Width := MainForm.Width - 25
   MainForm.Grid_1.Height := MainForm.Height - 125

   MainForm.Button_1.Row := MainForm.Height - 110 - GetBorderHeight() / 2

RETURN

PROCEDURE SelectMe()
/*
   Purpose:  Handles the double-click event on a grid row. It toggles the "Select"
             column (the first column) of the clicked row between "x" (selected) and
             " " (not selected).

   Parameters: None (implicitly receives the This object representing the grid)

   Return Value: None
*/
   LOCAL aLine := MainForm.Grid_1.Item( This.CellRowIndex )

   MainForm.Grid_1.Item( This.CellRowIndex ) := iif( Empty( aLine[1] ), { "x" }, { " " } )

   MainForm.Grid_1.Refresh()

RETURN

PROCEDURE SelectAll()
/*
   Purpose:  Selects all rows in the grid by setting the "Select" column (the first
             column) of each row to "x".
*/
   LOCAL i
   LOCAL nCnt := MainForm.Grid_1.ItemCount

   FOR i:=1 TO nCnt
      MainForm.Grid_1.Item( i ) := { "x" }
   NEXT

   MainForm.Grid_1.SetFocus()
   MainForm.Grid_1.Refresh()

RETURN

PROCEDURE DoFolder( cNewDir )
/*
   Purpose:  Allows the user to select a directory and then scans it for *.prg files
             using the hb_DirScan() function. The results are displayed in the grid.
             It also saves the last selected directory to an INI file for persistence.

   Parameters:
      - cNewDir:  An optional string containing a directory path. If empty, the user
                  is prompted to select a directory. If not empty, the specified
                  directory is used directly.

   Return Value: None
*/
   STATIC cFolder := "", cNewFolder := "", cMsg := ""
   LOCAL cExePath := Left( ExeName(), RAt( "\", ExeName() ) )
   LOCAL aDir
   LOCAL k
   LOCAL aItem

   MainForm.Statusbar.Item( 1 ) := " "

   IF Empty( cNewDir )

      BEGIN INI File ( cExePath + "dirselect.ini" )
         GET cFolder SECTION "General" ENTRY "LastPath"
      END INI

      cNewFolder := GetFolder( "Select a Folder", iif( Empty( cFolder ), cExePath, cFolder ) )
   
   ELSE

      cNewFolder := cNewDir

   ENDIF

   IF !Empty( cNewFolder )

      cFolder := cNewFolder
      BEGIN INI File ( cExePath + "dirselect.ini" )
         SET SECTION "General" ENTRY "LastPath" TO cFolder
      END INI

   ENDIF

   IF !Empty( cFolder )

      IF !( DirChange( cFolder ) == 0 )  // changing to the directory in question to read it
         MsgStop( "Error Changing to " + cFolder )
         RETURN
      ENDIF

      WAIT WINDOW "Scanning Directories" NOWAIT

      aDir := hb_DirScan( cFolder, "*.prg" )

      MainForm.StatusBar.Item( 1 ) := "Folder " + cFolder + " contains " + hb_ntos( k := Len( aDir ) ) + " files."

      InkeyGUI( 100 )

      MainForm.Grid_1.DeleteAllItems()

      FOR EACH aItem IN aDir

         MainForm.Grid_1.Additem( { " ", ;
            hb_DirSepAdd( cFolder ) + hb_FNameDir( aItem[ F_NAME ] ), ;
            hb_FNameNameExt( aItem[ F_NAME ] ), ;
            Transform( aItem[ F_SIZE ], "999,999,999" ), ;
            DToC( aItem[ F_DATE ] ), aItem [ F_TIME ], aItem[ F_ATTR ] } )

         cMsg := "Reading " + hb_ntos( Int( hb_enumIndex( aItem ) / k * 100 ) ) + "%"

         WAIT WINDOW cMsg NOWAIT

      NEXT

      InkeyGUI( 100 )

      WAIT CLEAR

      MainForm.Grid_1.Value := 1

   ELSE

      MsgStop( "No Folder Selected" )

   ENDIF

RETURN
