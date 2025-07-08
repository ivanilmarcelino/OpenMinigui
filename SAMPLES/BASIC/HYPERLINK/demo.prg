/*
 * Harbour MiniGui Hyperlink Demo
 *
 * Copyright 2003 Jacek Kubica <kubica@wssk.wroc.pl>
 *
 * Copyright 2008-2018 Grigory Filatov <gfilatov@inbox.ru>
*/

#include "minigui.ch"

#ifndef __XHARBOUR__
#define __CALLDLL__
#endif

#define DARKBLUE {0, 0, 128}
#define TITLE_1  "Harbour MiniGUI - Internet Explorer"
#define TITLE_2  "Create message"
#define TITLE_3  "Calculator"
#define TITLE_4  "HYPERLINK"
#define TITLE_5  "MY PROCEDURE"

PROCEDURE Main

   SET FONT TO "Arial", 9

   DEFINE WINDOW Main_form ;
      AT 0, 0 ;
      WIDTH 300 ;
      HEIGHT 170 ;
      TITLE 'HyperLink Demo' ;
      MAIN ;
      NOMAXIMIZE NOSIZE

   DEFINE MAIN MENU

   POPUP 'Get/Set Tests'
      ITEM 'Set new URL address' ACTION {|| Main_form.hl1.Value := "http://hmgextended.com/download", Main_form.hl1.Address := "http://hmgextended.com/download.html" }
      ITEM 'Set new e-mail address' ACTION {|| Main_form.hl2.Value := "kubica@wssk.wroc.pl", Main_form.hl2.Address := "kubica@wssk.wroc.pl" }

      ITEM 'Get URL from label' ACTION MsgInfo ( "URL is: " + Main_form.hl1.Address, "Url Label Address Value" )
      ITEM 'Get e-mail addres from label' ACTION MsgInfo ( "Mail is: " + Main_form.hl2.Address, "E-Mail Label Address Value" )
   END POPUP

   END MENU

   DEFINE HYPERLINK hl1
      ROW        10
      COL        10
      VALUE      'http://hmgextended.com'
      AUTOSIZE   .T.
      ADDRESS    'http://hmgextended.com'
      HANDCURSOR .T.
   END HYPERLINK

   @ 30, 10 HYPERLINK hl2 ;
      VALUE 'gfilatov@inbox.ru' ;
      AUTOSIZE ;
      ADDRESS 'gfilatov@inbox.ru' ;
      HANDCURSOR

   @ 50, 10 HYPERLINK hl3 ;
      VALUE 'Run Calculator' ;
      AUTOSIZE ;
      ACTION { || _Execute( 0, , GetFileInPath( 'Calc.exe' ), , , 5 ) } 

   @ 70, 10 HYPERLINK hl4 ;
      VALUE 'Open a current folder' ;
      AUTOSIZE ;
      ADDRESS "file:\\" + GetCurrentFolder() ;
      HANDCURSOR

   @ 90, 10 HYPERLINK hl5 ;
      VALUE 'Execute My Procedure' ;
      AUTOSIZE ;
      ADDRESS "proc:\\MyProc('This is a message from My Procedure !')" ;
      HANDCURSOR

   ON KEY ESCAPE ACTION ThisWindow.Release

   DEFINE TIMER Timer_1 INTERVAL 200 ACTION SetLinkBackColor()

   END WINDOW

   CENTER WINDOW Main_form

   ACTIVATE WINDOW Main_form

RETURN

PROCEDURE MyProc( cText )

   MsgInfo( cText, TITLE_5 )

RETURN


FUNCTION SetLinkBackColor()

   DO CASE

   CASE FindWindow( 0, TITLE_1 ) > 0
      Main_form.hl1.FontColor := DARKBLUE

   CASE FindWindow( 0, TITLE_2 ) > 0
      Main_form.hl2.FontColor := DARKBLUE

   CASE FindWindow( 0, TITLE_3 ) > 0
      Main_form.hl3.FontColor := DARKBLUE

   CASE FindWindow( 0, TITLE_4 ) > 0
      Main_form.hl4.FontColor := DARKBLUE

   CASE FindWindow( 0, TITLE_5 ) > 0
      Main_form.hl5.FontColor := DARKBLUE

   END CASE

RETURN NIL


FUNCTION GetFileInPath( cFile )

   LOCAL cPath  := GetEnv( 'PATH' ) + ';'
   LOCAL lFound := .N.
   LOCAL nLPos  := 0
   LOCAL nRPos  := 0
   LOCAL cSearch

   WHILE nRPos < Len( cPath ) .AND. !lFound
      nRPos   := At( ';', cPath, nLPos + 1 )
      cSearch := AddSlash( SubStr( cPath, nLPos + 1, nRPos - nLPos -1 ) )
      lFound  := File( cSearch + cFile )
      nLPos   := nRPos
   END WHILE

RETURN ( cSearch + cFile )


FUNCTION AddSlash( cInFolder )

   LOCAL cOutFolder := AllTrim( cInFolder )

   IF !Empty( cOutFolder ) .AND. Right( cOutfolder, 1 ) != '\'
      cOutFolder += '\'
   ENDIF

RETURN cOutFolder

#ifdef __CALLDLL__
#define HB_DYN_CTYPE_LONG                   0x0000004

FUNCTION FindWindow( ClassName, cTitleName )

RETURN ( HMG_CallDLL( "USER32.DLL", HB_DYN_CTYPE_LONG, "FindWindow", ClassName, cTitleName ) )

#else

DECLARE DLL_TYPE_LONG ;
      FindWindow ( DLL_TYPE_LPSTR lpClassName, DLL_TYPE_LPSTR lpWindowName ) ;
      IN USER32.DLL

#endif
