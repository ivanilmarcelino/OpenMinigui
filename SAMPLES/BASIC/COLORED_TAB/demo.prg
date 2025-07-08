/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2016-2025 Grigory Filatov <gfilatov@inbox.ru>
 */

#include "minigui.ch"

#define COLOR_BTNFACE 15

FUNCTION Main()

   SET FONT TO 'Arial', 10

   DEFINE WINDOW Form_1 ;
      MAIN ;
      CLIENTAREA 640, 480 ;
      TITLE 'Colored Tab Control Demo' ;
      ; // Init a colored label of the first Tab page
      ON INIT SizePageBack( 1, .F. ) ;
      ON SIZE SizeTest()

      DEFINE MAIN MENU

         DEFINE POPUP 'Style'
            MENUITEM 'Top pages' ACTION ( SetTab_1(), SizePageBack( 1, .F. ) )
            MENUITEM 'Bottom pages' ACTION ( SetTab_1( .T. ), SizePageBack( 1, .T. ) )
            SEPARATOR
            MENUITEM 'Exit' ACTION ThisWindow.RELEASE
         END POPUP

         DEFINE POPUP 'Tests'
            MENUITEM 'Change Page' ACTION Form_1.Tab_1.VALUE := 2
            MENUITEM 'Get Page Count' ACTION MsgInfo( Form_1.Tab_1.ItemCount )
            SEPARATOR
            // Optional Syntax (Refer button as Tab child )
            MENUITEM 'Get Button Caption' ACTION MsgInfo ( Form_1.Tab_1( 1 ).Button_1.Caption )
            MENUITEM 'Set Button Caption' ACTION Form_1.Tab_1( 1 ).Button_1.CAPTION := 'New'
         END POPUP

      END MENU

   END WINDOW

   SetTab_1()

   Form_1.Center()

   Form_1.Activate()

RETURN NIL


STATIC PROCEDURE SizeTest()

   Form_1.Tab_1.WIDTH := Form_1.ClientWidth - 24
   Form_1.Tab_1.HEIGHT := Form_1.ClientHeight - 24

   SizePageBack( Form_1.Tab_1.VALUE, _HMG_ActiveTabBottom )

RETURN


PROCEDURE SetTab_1( lBottomStyle )

   LOCAL nColor := GetSysColor( COLOR_BTNFACE )
   LOCAL aColor := HMG_n2RGB( nColor )
   LOCAL aTabColors := {}

   AAdd( aTabColors, { 159, 191, 236 } ) // Office_2003 Blue
   AAdd( aTabColors, { 251, 230, 148 } ) // Office_2003 Orange
   AAdd( aTabColors, { 255, 178, 178 } ) // LightRed
   AAdd( aTabColors, { 195, 224, 133 } ) // Purple
   AAdd( aTabColors, { 178, 135, 214 } ) // DarkBlue

DEFAULT lBottomStyle := .F.

   IF IsControlDefined( Tab_1, Form_1 )
      Form_1.Tab_1.RELEASE
   ENDIF

   DEFINE TAB Tab_1 ;
         OF Form_1 ;
         AT 10, 10 ;
         WIDTH Form_1.ClientWidth - 24 ;
         HEIGHT Form_1.ClientHeight - 24 ;
         VALUE 1 ;
         BACKCOLOR aColor ;
         HOTTRACK ;
         HTFORECOLOR BLACK ;
         HTINACTIVECOLOR WHITE ;
         ON CHANGE SizePageBack( This.VALUE, lBottomStyle )

      _HMG_ActiveTabBottom := lBottomStyle

      PAGE 'Page &1' IMAGE 'Exit' TOOLTIP 'TabPage 1'

         @ 0, 2 LABEL Page_1 VALUE "" WIDTH 0 HEIGHT 0 BACKCOLOR aTabColors[ 1 ]

         @ 100, 100 BUTTON Button_1 CAPTION "Test" WIDTH 50 HEIGHT 50 ACTION MsgInfo( 'Test!' )

      END PAGE

      PAGE 'Page &2' IMAGE 'Info' TOOLTIP 'TabPage 2'

         @ 0, 2 LABEL Page_2 VALUE "" WIDTH 0 HEIGHT 0 BACKCOLOR aTabColors[ 2 ]

      END PAGE

      PAGE 'Page &3' IMAGE 'Check' TOOLTIP 'TabPage 3'

         @ 0, 2 LABEL Page_3 VALUE "" WIDTH 0 HEIGHT 0 BACKCOLOR aTabColors[ 3 ]

      END PAGE

      PAGE 'Page &4' IMAGE 'Check' TOOLTIP 'TabPage 4'

         @ 0, 2 LABEL Page_4 VALUE "" WIDTH 0 HEIGHT 0 BACKCOLOR aTabColors[ 4 ]

      END PAGE

      PAGE 'Page &5' IMAGE 'Check' TOOLTIP 'TabPage 5'

         @ 0, 2 LABEL Page_5 VALUE "" WIDTH 0 HEIGHT 0 BACKCOLOR aTabColors[ 5 ]

      END PAGE

   END TAB

   // Assign the colors to the Tab bookmarks
   Form_1.Tab_1.Cargo := aTabColors

RETURN


STATIC PROCEDURE SizePageBack( nValue, lBottomStyle )

   LOCAL cName := 'Page_' + hb_ntos( nValue )
   LOCAL nH := GetBookmarkHeight()

   Form_1.( cName ).ROW := iif( lBottomStyle, 2, nH - 1 )
   Form_1.( cName ).WIDTH := Form_1.Tab_1.WIDTH - iif( IsThemed(), 5, GetBorderWidth() )
   Form_1.( cName ).HEIGHT := Form_1.Tab_1.HEIGHT - nH - 2

RETURN


STATIC FUNCTION GetBookmarkHeight()

   LOCAL hWnd := Form_1.Tab_1.HANDLE

RETURN GetTabHeaderHeight( hWnd )


#pragma BEGINDUMP

#include <mgdefs.h>
#include <commctrl.h>

HB_FUNC( GETTABHEADERHEIGHT )
{
   HWND     hwndTab = hmg_par_raw_HWND( 1 );
   RECT     rect;
   TCITEM   tci;
   tci.mask = TCIF_TEXT;   // We only need the text for this example
   tci.pszText = NULL;     // Not needed for GetItemRect
   tci.cchTextMax = 0;

   // Get the rectangle of the first tab (index 0)
   if( TabCtrl_GetItemRect( hwndTab, 0, &rect ) )
   {
      hmg_ret_NINT( rect.bottom - rect.top + 4 );  // Height of the tab
   }
   else
   {
      // Handle error (e.g., no tabs exist)
      hb_retni( 0 );
   }
}

#pragma ENDDUMP
