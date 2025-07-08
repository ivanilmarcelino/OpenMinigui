/*
 HMG DEMO
 (c) 2010 Roberto Lopez
*/

#include "minigui.ch"

STATIC aCustColors

FUNCTION Main

   LOCAL x
   LOCAL n := 256

   aCustColors := { { 255, 255, 255 } }

   FOR x := 1 TO 15
      n -= 16
      AAdd( aCustColors, { n, n, n } )
   NEXT

   DEFINE WINDOW Win1 ;
         ROW 10 ;
         COL 10 ;
         WIDTH 400 ;
         HEIGHT 400 ;
         TITLE 'HMG common dialogs' ;
         WINDOWTYPE MAIN

      DEFINE MAIN MENU

         DEFINE POPUP 'Dialogs'

            POPUP 'GetFile'
               MENUITEM 'Test' ONCLICK GetFile_Test()
               MENUITEM 'Extensions' ONCLICK GetFile_Extensions()
               MENUITEM 'Multiselect' ONCLICK GetFile_Multiselect()
            END POPUP

            POPUP 'GetFolder'
               MENUITEM 'No Params' ONCLICK GetFolder_NoParams()
               MENUITEM 'Title and Path' ONCLICK GetFolder_Params()
               MENUITEM 'Show Files' ONCLICK GetFolder_Files()
            END POPUP

            POPUP 'GetColor'
               MENUITEM 'Test' ONCLICK GetColor_Test()
               MENUITEM 'Cust Colors' ONCLICK GetColor_CustColors()
            END POPUP

            POPUP 'GetFont'
               MENUITEM 'Test' ONCLICK GetFont_Test()
            END POPUP

            POPUP 'PutFile'
               MENUITEM 'Test' ONCLICK PutFile_Test()
            END POPUP

            POPUP 'InputBox'
               MENUITEM 'Test' ONCLICK InputBox_Test()
            END POPUP

            POPUP 'Create/Remove Folder'
               MENUITEM 'CreateFolder' ONCLICK CreateFolder_Test()
               MENUITEM 'RemoveFolder' ONCLICK RemoveFolder_Test()
            END POPUP

            POPUP 'Get/Set CurrentFolder'
               MENUITEM 'GetCurrentFolder' ONCLICK MsgInfo( GetCurrentFolder() )
               MENUITEM 'SetCurrentFolder' ONCLICK MsgInfo( IF( SetCurrentFolder( '\minigui' ), '\minigui is the current folder', 'Folder not found!' ) )
            END POPUP

         END POPUP

      END MENU

      @ 10, 10 EDITBOX Edit1 ;
         WIDTH 365 ;
         HEIGHT 320 ;
         VALUE '' ;
         NOHSCROLL

   END WINDOW

   CENTER WINDOW Win1
   ACTIVATE WINDOW Win1

RETURN NIL

*--------------------------------------------------------------*
FUNCTION CreateFolder_Test()
*--------------------------------------------------------------*

   IF ! CreateFolder( 'NewFolder' )
      IF GetLastError() == ERROR_FILE_NOT_FOUND .OR. GetLastError() == ERROR_PATH_NOT_FOUND
         MsgInfo( 'New Folder Creation Failed' )
      ELSEIF GetLastError() == ERROR_ALREADY_EXISTS
         MsgInfo( 'This Folder Already Exists' )
      ENDIF
   ELSEIF GetLastError() == NO_ERROR
      MsgInfo( 'New Folder Creation is Successful' )
   ENDIF

RETURN NIL

*--------------------------------------------------------------*
FUNCTION RemoveFolder_Test()
*--------------------------------------------------------------*

   IF ! RemoveFolder( 'NewFolder' )
      IF GetLastError() == ERROR_FILE_NOT_FOUND .OR. GetLastError() == ERROR_PATH_NOT_FOUND
         MsgInfo( 'Folder Removal Failed' )
      ENDIF
   ELSEIF GetLastError() == NO_ERROR
      MsgInfo( 'Folder Removal is Successful' )
   ENDIF

RETURN NIL

*--------------------------------------------------------------*
FUNCTION GetFile_Test()
*--------------------------------------------------------------*
   LOCAL x

   Win1.Edit1.VALUE := "GetFile( aFilters, cTitle, cInitFolder )" + CRLF + ;
      "Return STRING, path + filename or an empty STRING" + CRLF

   x := GetFile()
   Win1.Edit1.VALUE := Win1.Edit1.VALUE + CRLF + "Return " + x

RETURN NIL

*--------------------------------------------------------------*
FUNCTION GetFile_Extensions()
*--------------------------------------------------------------*
   LOCAL x

   Win1.Edit1.VALUE := "GetFile( aFilters, cTitle, cInitFolder, lMultiselect, lNoChangeDirectory )" + CRLF + ;
      "Return STRING, path + filename or an empty STRING" + CRLF

   x := GetFile( { { 'Src', '*.prg' } }, 'Select file', 'c:\minigui\source', .F., .T. )
   Win1.Edit1.VALUE := Win1.Edit1.VALUE + CRLF + "Return " + x

RETURN NIL


*--------------------------------------------------------------*
FUNCTION GetFile_Multiselect()
*--------------------------------------------------------------*
   LOCAL x, s := "", n

   Win1.Edit1.VALUE := "GetFile( aFilter, cTitle, cInitFolder, lMultiSelect, lNoChangeDirectory )" + CRLF + ;
      "IF MultiSelect = .T. when return ARRAY" + CRLF + ;
      " Accept return ARRAY, Cancel an empty ARRAY" + CRLF + ;
      " Each item = path + filename" + CRLF

   x := GetFile( { { 'Src', '*.prg;*.c' }, { 'text', '*.txt;*.doc' } }, 'Select file', 'c:\minigui\source', .T., .T. )

   s := "Return array len = " + hb_ValToStr( Len( x ) ) + CRLF
   FOR n := 1 TO Len( x )
      s += x[ n ] + CRLF
   NEXT

   Win1.Edit1.VALUE := Win1.Edit1.VALUE + s

RETURN NIL


*--------------------------------------------------------------*
FUNCTION GetFolder_NoParams()
*--------------------------------------------------------------*
   LOCAL x

   Win1.Edit1.VALUE := "GetFolder() No parameters." + CRLF + ;
      " Accept return STRING with path name, Cancel an empty STRING" + CRLF

   x := GetFolder()
   Win1.Edit1.VALUE := Win1.Edit1.VALUE + CRLF + "Return " + x

RETURN NIL


*--------------------------------------------------------------*
FUNCTION GetFolder_Params()
*--------------------------------------------------------------*
   LOCAL x

   Win1.Edit1.VALUE := "GetFolder( cTitle, cInitFolder )." + CRLF + ;
      " Accept return string with path name, Cancel an empty string" + CRLF + ;
      " Extension. cInitFolder. Dialog box open the Init folder" + CRLF

   x := GetFolder( "Select Directory", "c:\minigui" )
   Win1.Edit1.VALUE := Win1.Edit1.VALUE + CRLF + "Return " + x

RETURN NIL


*--------------------------------------------------------------*
FUNCTION GetFolder_Files()
*--------------------------------------------------------------*
   LOCAL x

   Win1.Edit1.VALUE := "GetFolder( cTitle, cInitFolder, nFlags )." + CRLF + ;
      " Accept return string with path name or file name, Cancel an empty string" + CRLF + ;
      " Extension. BIF flags. Dialog box will display files as well as folders" + CRLF

   x := GetFolder( "Select Directory Or File", "c:\minigui", ;
      BIF_BROWSEINCLUDEFILES + BIF_EDITBOX + BIF_VALIDATE + BIF_NEWDIALOGSTYLE )
   Win1.Edit1.VALUE := Win1.Edit1.VALUE + CRLF + "Return " + x

RETURN NIL


*--------------------------------------------------------------*
FUNCTION GetColor_Test()
*--------------------------------------------------------------*
   LOCAL x

   Win1.Edit1.VALUE := "GetColor( xColor )." + CRLF + ;
      " Accept return ARRAY R G B, Cancel an empty ARRAY" + CRLF

   x := GetColor( { 255, 0, 0 } )

   Win1.Edit1.VALUE := Win1.Edit1.VALUE +CRLF + "Return Array " + CRLF + ;
      "Item 1= " + hb_ValToStr( x[ 1 ] ) + CRLF + ;
      "Item 2= " + hb_ValToStr( x[ 2 ] ) + CRLF + ;
      "Item 3= " + hb_ValToStr( x[ 3 ] ) + CRLF

RETURN NIL


*--------------------------------------------------------------*
FUNCTION GetColor_CustColors()
*--------------------------------------------------------------*
   LOCAL x

   Win1.Edit1.VALUE := "GetColor( xColor, @aCustColors )." + CRLF + ;
      " Accept return ARRAY R G B, Cancel an empty ARRAY" + CRLF

   x := GetColor( aCustColors[ 1 ], @aCustColors )

   Win1.Edit1.VALUE := Win1.Edit1.VALUE +CRLF + "Return Array " + CRLF + ;
      "Item 1= " + hb_ValToStr( x[ 1 ] ) + CRLF + ;
      "Item 2= " + hb_ValToStr( x[ 2 ] ) + CRLF + ;
      "Item 3= " + hb_ValToStr( x[ 3 ] ) + CRLF + ;
      "Custom Colors Element 1 Is " + CRLF + ;
      "Item 1= " + hb_ValToStr( aCustColors[ 1 ][ 1 ] ) + CRLF + ;
      "Item 2= " + hb_ValToStr( aCustColors[ 1 ][ 2 ] ) + CRLF + ;
      "Item 3= " + hb_ValToStr( aCustColors[ 1 ][ 3 ] )

RETURN NIL


*--------------------------------------------------------------*
FUNCTION GetFont_Test()
*--------------------------------------------------------------*
   LOCAL x

   Win1.Edit1.VALUE := "GetFont( cInitFontName, nInitFontSize, lBold, lItalic, anInitColor, lUnderLine, lStrikeOut, nCharset )" + CRLF + ;
      " Accept return ARRAY, Cancel an empty ARRAY" + CRLF

   x := GetFont( "Arial", 12, .T., .T., NIL, .T., .T., NIL )

   Win1.Edit1.VALUE := Win1.Edit1.VALUE + CRLF + "Return Array :" + CRLF + ;
      "Item 1= " + hb_ValToStr( x[ 1 ] ) + CRLF + ;
      "Item 2= " + hb_ValToStr( x[ 2 ] ) + CRLF + ;
      "Item 3= " + hb_ValToStr( x[ 3 ] ) + CRLF + ;
      "Item 4= " + hb_ValToStr( x[ 4 ] ) + CRLF + ;
      "Item 5= " + "{" + hb_ValToStr( x[ 5 ][ 1 ] ) + "," + hb_ValToStr( x[ 5 ][ 2 ] ) + "," + hb_ValToStr( x[ 5 ][ 3 ] ) + "}" + CRLF + ;
      "Item 6= " + hb_ValToStr( x[ 6 ] ) + CRLF + ;
      "Item 7= " + hb_ValToStr( x[ 7 ] ) + CRLF + ;
      "Item 8= " + hb_ValToStr( x[ 8 ] ) + CRLF

RETURN NIL


*--------------------------------------------------------------*
FUNCTION PutFile_Test()
*--------------------------------------------------------------*
   LOCAL x

   Win1.Edit1.VALUE := "PutFile( aFilters, cTitle, cInitFolder, lNoChangeDirectory, cFileName)" + CRLF + ;
      "Return STRING, path + filename or an empty STRING" + CRLF + ;
      " Extension. Predefined cFileName" + CRLF

   x := PutFile( { { 'Src', '*.prg' } }, 'Select file', 'c:\minigui\source', .F., "test" )

   Win1.Edit1.VALUE := Win1.Edit1.VALUE + CRLF + "Return " + x

RETURN NIL


*--------------------------------------------------------------*
FUNCTION InputBox_Test()
*--------------------------------------------------------------*
   LOCAL x

   Win1.Edit1.VALUE := "InputBox()" + CRLF

   x := InputBox( 'Enter text', 'InputBox Demo', 'Default Value' )

   Win1.Edit1.VALUE := Win1.Edit1.VALUE + CRLF + "Return " + hb_ValToStr( x )

RETURN NIL
