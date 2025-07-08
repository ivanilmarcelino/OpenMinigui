/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
*/

#include "minigui.ch"
#include "i_winuser.ch"
#include "resource.h"

MEMVAR hMenu

Procedure Main

   SET EVENTS FUNCTION TO App_OnEvents

   DEFINE WINDOW Win1				;
      CLIENTAREA 320, 200			;
      MINWIDTH 220 MINHEIGHT 110		;
      TITLE "Test application"			;
      ICON IDI_APPICON				;
      MAIN 					;
      ON INIT Win1_OnInit( ThisWindow.Handle )	;
      ON RELEASE DestroyMenu( hMenu )

   END WINDOW

   CENTER WINDOW Win1

   ACTIVATE WINDOW Win1

Return


static procedure Win1_OnInit( hWnd )

   local hAccel
   local hSysMenu

   public hMenu

   // Load menu from resources.
   hMenu  := LoadMenu( Nil, IDR_MAINMENU )
   // Load accelerators.
   hAccel := LoadAccelerators( Nil, IDR_ACCELERATOR )

   // Add main menu.
   if ! Empty( hMenu )
      SetMenu( hWnd, hMenu )
   endif

   // Add accelerators.
   if ! Empty( hAccel )
      SetAcceleratorTable( hWnd, hAccel )
   endif

   // Add "about" to the system menu.
   hSysMenu := GetSystemMenu( ThisWindow.Handle )
   InsertMenuSeparator( hSysMenu, 5 )
   InsertMenu( hSysMenu, 6, ID_HELP_ABOUT, "About" )

return


static procedure AboutDialog()

   static lDlg := .F.

   if lDlg
      return
   endif

   DEFINE FONT Font_1 FONTNAME "MS Shell Dlg" SIZE 8

   DEFINE DIALOG Dlg_1 OF Win1 RESOURCE IDD_ABOUTDIALOG ;
      CAPTION "Test application" ;
      ON INIT { |hDlg| lDlg := .T., SetForegroundWindow(hDlg) } ;
      ON RELEASE lDlg := .F.

   REDEFINE BUTTON Btn_1 ID IDOK FONT "Font_1" ;
      ACTION _ReleaseWindow ( 'Dlg_1' )

   END DIALOG

   RELEASE FONT Font_1

return


function App_OnEvents( hWnd, nMsg, wParam, lParam )

   local nResult

   switch nMsg

   case WM_COMMAND

      switch LoWord( wParam )

      case ID_HELP_ABOUT
         AboutDialog()
         nResult := 0
         exit   

      case ID_FILE_EXIT
         QUIT
         nResult := 0
         exit   

      otherwise
         nResult := Events( hWnd, nMsg, wParam, lParam )

      end
      exit

   case WM_SYSCOMMAND

      switch LoWord( wParam )

      case ID_HELP_ABOUT
         AboutDialog()
         nResult := 0
         exit   

      end
      exit

   otherwise
      nResult := Events( hWnd, nMsg, wParam, lParam )

   end switch

return nResult


#pragma BEGINDUMP

#include <mgdefs.h>

#ifdef UNICODE
   LPWSTR AnsiToWide( LPCSTR );
#endif

HB_FUNC( INSERTMENU )
{
#ifndef UNICODE
   LPCSTR lpNewItem = hb_parc( 4 );
#else
   LPWSTR lpNewItem = AnsiToWide( ( char * ) hb_parc( 4 ) );
#endif
   hb_retl( InsertMenu( ( HMENU ) HB_PARNL( 1 ), hb_parni( 2 ), MF_BYPOSITION, hb_parni( 3 ), lpNewItem ) );

#ifdef UNICODE
   hb_xfree( lpNewItem );
#endif
}

HB_FUNC( INSERTMENUSEPARATOR )
{
   hb_retl( InsertMenu( ( HMENU ) HB_PARNL( 1 ), hb_parni( 2 ), MF_BYPOSITION | MF_SEPARATOR, 0, NULL ) );
}

#pragma ENDDUMP
