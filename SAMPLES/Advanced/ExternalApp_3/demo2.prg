/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Read a text from another program / Прочитать текст из другой программы
 *
 * Copyright 2015 Verchenko Andrey <verchenkoag@gmail.com>
 * Copyright 2015 Sidorov Aleksandr <aksidorov@mail.ru>
 *
 * Revised by Grigory Filatov <gfilatov@inbox.ru> and Petr Chornyj <myorg63@mail.ru>
*/

#include "minigui.ch"

STATIC cAppTitle := "InputMask Demo"

FUNCTION Main()

   DEFINE WINDOW Form_Main ;
      AT 0, 0 ;
      WIDTH 600 HEIGHT 400 ;
      TITLE "Read a text of the program: " + cAppTitle ;
      MAIN TOPMOST ;
      BACKCOLOR { 231, 178, 30 } ;
      ON RELEASE CloseIt()

   DEFINE BUTTONEX Button_1
      ROW 20
      COL 20
      WIDTH 250
      CAPTION 'Read the textbox "Edit"'
      BACKCOLOR LGREEN
      FONTCOLOR WHITE
      NOXPSTYLE .T.
      HANDCURSOR .T.
      ACTION ReadGetIt()
   END BUTTONEX

   DEFINE BUTTONEX Button_3
      ROW 20
      COL 310
      WIDTH 250
      CAPTION 'Cancel'
      BACKCOLOR MAROON
      FONTCOLOR WHITE
      NOXPSTYLE .T.
      HANDCURSOR .T.
      ACTION ThisWindow.Release
   END BUTTONEX

   @ 60, 20 EDITBOX Edit_Result ;
      WIDTH 540 HEIGHT 290      ;
      VALUE ''                  ;
      NOHSCROLL

   END WINDOW

   CENTER WINDOW   Form_Main
   ACTIVATE WINDOW Form_Main

RETURN NIL

///////////////////////////////////////////////////////////////
#define WM_CLOSE     0x0010

FUNCTION CloseIt()

   LOCAL hWnd := FindWindowEx( ,,, cAppTitle )

   IF IsWindowHandle( hWnd )
      PostMessage( hWnd, WM_CLOSE, 0, 0 )  // close programm
   ENDIF

RETURN NIL

///////////////////////////////////////////////////////////////
#ifdef __XHARBOUR__
#define ENUMINDEX hb_EnumIndex()
#else
#define ENUMINDEX c:__EnumIndex
#endif

FUNCTION ReadGetIt()

   LOCAL aChilds, c, cText
   LOCAL hWnd := FindWindowEx( ,,, cAppTitle )

   IF ! IsWindowHandle( hWnd )

      MsgStop( "Can not found the window: " + cAppTitle )

   ELSE

      aChilds := EnumChildWindows( hWnd, .T. )

      cText := ""
      FOR EACH c IN aChilds
         IF c[ 2 ] == "Edit"
            cText += hb_ntos( ENUMINDEX ) + ": " + c[ 3 ] + CRLF + CRLF
         ENDIF
      NEXT

      Form_Main.Edit_Result.Value := cText

   ENDIF

RETURN NIL

///////////////////////////////////////////////////////////////
STATIC FUNCTION EnumChildWindows( hWnd, bExt )
   LOCAL aChilds := {}, bAction

   IF hb_defaultValue( bExt, .F. )
      bAction := {|hChild| AAdd( aChilds, { hChild, GetClassName( hChild ), GetEditText( hChild ) } ), .T. }
   ELSE
      bAction := {|hChild| AAdd( aChilds, hChild ), .T. }
   ENDIF

   C_EnumChildWindows( hWnd, bAction )

RETURN aChilds

///////////////////////////////////////////////////////////////
STATIC FUNCTION GetEditText( hChild )

RETURN If( GetClassName( hChild ) == "Edit", MyGetEditText( hChild ), "" )

///////////////////////////////////////////////////////////////
#pragma BEGINDUMP

#include <mgdefs.h>

HB_FUNC( MYGETEDITTEXT )
{
   HWND hWnd = ( HWND ) HB_PARNL( 1 );

   hb_retc_null();

   if( IsWindow( hWnd ) )
   {
      USHORT nLen  = ( USHORT ) SendMessage( hWnd, WM_GETTEXTLENGTH, 0, 0 );

      if ( ++nLen > 1 )
      {
         char * cText = ( char * ) hb_xgrab( nLen );

         SendMessageA( hWnd, WM_GETTEXT, nLen, ( LPARAM ) cText /*address of buffer for text*/);

         hb_retc( ( const char * ) cText );

         hb_xfree( cText );
      }
   }
}

#pragma ENDDUMP
