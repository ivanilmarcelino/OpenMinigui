/*-------------------------------------------------------------------------
   MINIGUI - Harbour Win32 GUI library source code

   Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
   http://harbourminigui.googlepages.com/

   This  program is free software; you can redistribute it and/or modify it
   under  the  terms  of the GNU General Public License as published by the
   Free  Software  Foundation; either version 2 of the License, or (at your
   option) any later version.

   This  program  is  distributed  in  the hope that it will be useful, but
   WITHOUT   ANY   WARRANTY;   without   even   the   implied  warranty  of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
   Public License for more details.

   You  should have received a copy of the GNU General Public License along
   with  this  software;  see  the  file COPYING. If not, write to the Free
   Software  Foundation,  Inc.,  59  Temple  Place,  Suite  330, Boston, MA
   02111-1307 USA (or visit the web site http://www.gnu.org/).

   As  a  special exception, you have permission for additional uses of the
   text contained in this release of Harbour Minigui.

   The  exception  is  that,  if  you link the Harbour Minigui library with
   other  files to produce an executable, this does not by itself cause the
   resulting  executable  to  be covered by the GNU General Public License.
   Your  use  of  that  executable  is  in  no way restricted on account of
   linking the Harbour-Minigui library code into it.

   Parts of this project are based upon:

    "Harbour GUI framework for Win32"
    Copyright 2001 Alexander S.Kresin <alex@kresin.ru>
    Copyright 2001 Antonio Linares <alinares@fivetech.com>
    www - https://harbour.github.io/

    "Harbour Project"
    Copyright 1999-2025, https://harbour.github.io/

    "WHAT32"
    Copyright 2002 AJ Wos <andrwos@aust1.net>

    "HWGUI"
    Copyright 2001-2021 Alexander S.Kresin <alex@kresin.ru>

   Parts of this code are contributed and used here under permission of the
   author:
   Copyright 2007-2017 (C) P.Chornyj <myorg63@mail.ru>
  ----------------------------------------------------------------------*/
#if !defined( __WINNT__ )
#define __WINNT__
#endif
#include <mgdefs.h>
#include "hbapierr.h"
#include "hbapiitm.h"

#if !defined( __XHARBOUR__ )
#include "hbwinuni.h"
#endif
#if defined( __XHARBOUR__ ) || ( __HARBOUR__ - 0 < 0x030200 )
#define HB_STRNLEN   hb_strnlen
#define HB_STRNDUP   hb_strndup
#endif
#define MAX_ITEM_TEXT   256

#include "c_menu.h"

// extern functions
#ifdef UNICODE
LPWSTR         AnsiToWide( LPCSTR );
LPSTR          WideToAnsi( LPWSTR );
#endif
HINSTANCE      GetResources( void );
extern HBITMAP Icon2Bmp( HICON hIcon );
extern BOOL    SetAcceleratorTable( HWND, HACCEL );

HBITMAP        HMG_LoadPicture
               (
                  const char  *FileName,
                  int         New_Width,
                  int         New_Height,
                  HWND        hWnd,
                  int         ScaleStretch,
                  int         Transparent,
                  long        BackgroundColor,
                  int         AdjustImage,
                  HB_BOOL     bAlphaFormat,
                  int         iAlpfaConstant
               );

/*
 * FUNCTION SETACCELERATORTABLE( hWndMain, hAccel )
 *
 * Sets the accelerator table for a given window.
 *
 * Parameters:
 *   hWndMain : HWND - Handle to the main window.
 *   hAccel   : HACCEL - Handle to the accelerator table.
 *
 * Returns:
 *   None. This function does not return a value.
 *
 * Purpose:
 *   This function associates an accelerator table with a window, allowing
 *   keyboard shortcuts defined in the table to be processed by the window.
 *   This is useful for providing keyboard access to menu items and other
 *   window functions.
 *
 * Notes:
 *   The function checks if both hWndMain and hAccel are valid handles
 *   before attempting to set the accelerator table.
 */
HB_FUNC( SETACCELERATORTABLE )
{
   HWND     hWndMain = hmg_par_raw_HWND( 1 );
   HACCEL   hAccel = hmg_par_raw_HACCEL( 2 );

   if( hWndMain && hAccel )
   {
      SetAcceleratorTable( hWndMain, hAccel );
   }
}

/*
 * FUNCTION ACCELERATORTABLE2ARRAY( hAccel )
 *
 * Converts an accelerator table to a Harbour array.
 *
 * Parameters:
 *   hAccel : HACCEL - Handle to the accelerator table.
 *
 * Returns:
 *   PHB_ITEM - A Harbour array where each element represents an accelerator entry.
 *              Each accelerator entry is itself an array containing three elements:
 *              1. fVirt (Numeric): Flags indicating virtual key modifiers (e.g., Ctrl, Shift, Alt).
 *              2. key (Numeric): The virtual key code.
 *              3. cmd (Numeric): The command ID associated with the accelerator.
 *              Returns an empty array if hAccel is NULL or if an error occurs.
 *
 * Purpose:
 *   This function allows you to inspect the contents of an accelerator table
 *   from within Harbour code. This is useful for dynamically displaying
 *   keyboard shortcuts in the user interface or for programmatically
 *   modifying accelerator tables.
 *
 * Notes:
 *   The function allocates memory for the accelerator entries. It is the
 *   caller's responsibility to release the returned array using hb_itemRelease().
 */
HB_FUNC( ACCELERATORTABLE2ARRAY )
{
   HACCEL   hAccel = hmg_par_raw_HACCEL( 1 );
   PHB_ITEM aAccels = hb_itemArrayNew( 0 );

   if( hAccel )
   {
      int   cAccelEntries = CopyAcceleratorTable( hAccel, NULL, 0 );

      if( cAccelEntries > 0 )
      {
         LPACCEL  lpAccel = ( LPACCEL ) hb_xalloc( cAccelEntries * sizeof( ACCEL ) );

         if( NULL != lpAccel )
         {
            if( CopyAcceleratorTable( hAccel, lpAccel, cAccelEntries ) )
            {
               int   i;

               for( i = 0; i < cAccelEntries; i++ )
               {
                  PHB_ITEM aAccel = hb_itemArrayNew( 3 );

                  hb_arraySetNI( aAccel, 1, lpAccel[i].fVirt );
                  hb_arraySetNL( aAccel, 2, lpAccel[i].key );
                  hb_arraySetNL( aAccel, 3, lpAccel[i].cmd );

                  hb_arrayAddForward( aAccels, aAccel );

                  hb_itemRelease( aAccel );
               }

               hb_xfree( lpAccel );
            }
         }
      }
   }

   hb_itemReturnRelease( aAccels );
}

/*
 * FUNCTION ARRAY2ACCELERATORTABLE( pArray )
 *
 * Creates an accelerator table from a Harbour array.
 *
 * Parameters:
 *   pArray : PHB_ITEM - A Harbour array where each element represents an accelerator entry.
 *              Each accelerator entry should be an array containing three elements:
 *              1. fVirt (Numeric): Flags indicating virtual key modifiers (e.g., Ctrl, Shift, Alt).
 *              2. key (Numeric): The virtual key code.
 *              3. cmd (Numeric): The command ID associated with the accelerator.
 *
 * Returns:
 *   HACCEL - Handle to the created accelerator table. Returns NULL if pArray is NULL,
 *            empty, or if an error occurs during creation.
 *
 * Purpose:
 *   This function allows you to create an accelerator table dynamically
 *   from within Harbour code. This is useful for loading keyboard shortcuts
 *   from a configuration file or for allowing the user to customize
 *   keyboard shortcuts.
 *
 * Notes:
 *   The function allocates memory for the accelerator entries. It is the
 *   caller's responsibility to destroy the returned accelerator table using
 *   DestroyAcceleratorTable() when it is no longer needed.
 */
HB_FUNC( ARRAY2ACCELERATORTABLE )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );
   int      nLen;
   HACCEL   hAccel = NULL;

   if( pArray && ( ( nLen = ( int ) hb_arrayLen( pArray ) ) > 0 ) )
   {
      LPACCEL  lpAccel = ( LPACCEL ) hb_xalloc( nLen * sizeof( ACCEL ) );

      if( NULL != lpAccel )
      {
         int   i;

         for( i = 0; i < nLen; i++ )
         {
            if( hb_arrayGetType( pArray, i + 1 ) & HB_IT_ARRAY )
            {
               PHB_ITEM pAccel = hb_arrayGetItemPtr( pArray, i + 1 );

               if( hb_arrayLen( pAccel ) == 3 )
               {
                  lpAccel[i].fVirt = ( BYTE ) hb_arrayGetNI( pAccel, 1 );
                  lpAccel[i].key = ( WORD ) hb_arrayGetNL( pAccel, 2 );
                  lpAccel[i].cmd = ( WORD ) hb_arrayGetNL( pAccel, 3 );
               }
            }
         }

         hAccel = CreateAcceleratorTable( lpAccel, nLen );
         hb_xfree( lpAccel );
      }
   }

   hmg_ret_raw_HACCEL( hAccel );
}

/*
 * FUNCTION COPYACCELERATORTABLE( hAccelSrc, lpAccelDst )
 *
 * Copies an accelerator table.
 *
 * Parameters:
 *   hAccelSrc : HACCEL - Handle to the source accelerator table.
 *   lpAccelDst: Pointer - Pointer to memory where the copied accelerator table will be stored. If NULL, the function returns the number of entries in the table.
 *
 * Returns:
 *   INT - The number of accelerator entries copied. Returns 0 if hAccelSrc is NULL.
 *
 * Purpose:
 *   This function allows you to create a copy of an existing accelerator table.
 *   This is useful for modifying an accelerator table without affecting the original.
 *
 * Notes:
 *   If lpAccelDst is NULL, the function returns the number of entries in the
 *   accelerator table. You can then allocate memory for the destination buffer
 *   and call the function again with the allocated buffer.
 *   The caller is responsible for allocating and freeing the memory pointed to by lpAccelDst.
 */
HB_FUNC( COPYACCELERATORTABLE )
{
   HACCEL   hAccelSrc = hmg_par_raw_HACCEL( 1 );

   hb_retni( 0 );

   if( NULL != hAccelSrc )
   {
      int   cAccelEntries = CopyAcceleratorTable( hAccelSrc, NULL, 0 );

      if( cAccelEntries > 0 )
      {
         LPACCEL  lpAccelDst = ( LPACCEL ) hb_xalloc( cAccelEntries * sizeof( ACCEL ) );

         if( NULL != lpAccelDst )
         {
            hb_retni( CopyAcceleratorTable( hAccelSrc, lpAccelDst, cAccelEntries ) );

            hb_storptr( lpAccelDst, 2 );
         }
      }
   }
}

/*
 * FUNCTION CREATEACCELERATORTABLE( lpAccel, cAccelEntries )
 *
 * Creates an accelerator table.
 *
 * Parameters:
 *   lpAccel       : Pointer - Pointer to an array of ACCEL structures that define the accelerator table entries.
 *   cAccelEntries : INT - The number of entries in the lpAccel array.
 *
 * Returns:
 *   HACCEL - Handle to the created accelerator table. Returns NULL if lpAccel is NULL,
 *            cAccelEntries is less than or equal to 0, or if an error occurs during creation.
 *
 * Purpose:
 *   This function creates an accelerator table from an array of ACCEL structures.
 *   This is the underlying function used by ARRAY2ACCELERATORTABLE().
 *
 * Notes:
 *   The function takes ownership of the memory pointed to by lpAccel and frees it.
 *   The caller is responsible for ensuring that lpAccel points to valid memory
 *   containing an array of ACCEL structures.
 */
HB_FUNC( CREATEACCELERATORTABLE )
{
   LPACCEL  lpAccels = ( LPACCEL ) hb_parptr( 1 );
   HACCEL   hAccel = NULL;
   int      cAccelEntries = hb_parni( 2 );

   if( lpAccels && ( cAccelEntries > 0 ) )
   {
      hAccel = CreateAcceleratorTable( lpAccels, cAccelEntries );

      hb_xfree( lpAccels );
   }

   hmg_ret_raw_HACCEL( hAccel );
}

/*
 * FUNCTION DESTROYACCELERATORTABLE( hAccel )
 *
 * Destroys an accelerator table.
 *
 * Parameters:
 *   hAccel : HACCEL - Handle to the accelerator table to destroy.
 *
 * Returns:
 *   BOOL - TRUE if the accelerator table was successfully destroyed, FALSE otherwise.
 *
 * Purpose:
 *   This function destroys an accelerator table, freeing the resources
 *   associated with it. It is important to destroy accelerator tables when
 *   they are no longer needed to prevent memory leaks.
 *
 * Notes:
 *   The function returns FALSE if hAccel is an invalid handle.
 */
HB_FUNC( DESTROYACCELERATORTABLE )
{
   HACCEL   hAccel = hmg_par_raw_HACCEL( 1 );

   hmg_ret_L( DestroyAcceleratorTable( hAccel ) );
}

/*
 * FUNCTION LOADACCELERATORS( hInstance, lpTableName )
 *
 * Loads an accelerator table from a resource file.
 *
 * Parameters:
 *   hInstance : HINSTANCE - Handle to the instance of the module whose executable file contains the accelerator table.
 *                           If 0, the function loads from the current executable.
 *   lpTableName : LPCTSTR - The name of the accelerator table resource. This can be either a string or an integer resource ID.
 *
 * Returns:
 *   HACCEL - Handle to the loaded accelerator table. Returns NULL if the accelerator table could not be loaded.
 *
 * Purpose:
 *   This function loads an accelerator table from a resource file. This is the
 *   most common way to create accelerator tables in Windows applications.
 *
 * Notes:
 *   If hInstance is 0, the function loads the accelerator table from the
 *   executable file of the current process.
 *   The lpTableName parameter can be either a string or an integer resource ID.
 *   If it is an integer, it must be converted to a string using the MAKEINTRESOURCE() macro.
 */
HB_FUNC( LOADACCELERATORS )
{
   HACCEL      hAccel = ( HACCEL ) NULL;
   HINSTANCE   hInstance = HB_ISNUM( 1 ) ? hmg_par_raw_HINSTANCE( 1 ) : GetResources();
   LPCTSTR     lpTableName;

   if( HB_ISNUM( 2 ) )
   {
      lpTableName = MAKEINTRESOURCE( hmg_par_WORD( 2 ) );

      hAccel = LoadAccelerators( hInstance, lpTableName );
   }
   else if( hb_parclen( 2 ) > 0 )
   {
#ifndef __XHARBOUR__
      void  *hTableName;
      lpTableName = HB_PARSTR( 2, &hTableName, NULL );
#else
      lpTableName = ( LPCTSTR ) hb_parc( 2 );
#endif
      hAccel = LoadAccelerators( hInstance, lpTableName );
#ifndef __XHARBOUR__
      hb_strfree( hTableName );
#endif
   }

   hmg_ret_raw_HACCEL( hAccel );
}

/*
 * FUNCTION LOADMENU( hInstance, lpMenuName )
 *
 * Loads a menu resource from the specified module.
 *
 * Parameters:
 *   hInstance : HINSTANCE - Handle to the instance of the module whose executable file contains the menu resource.
 *                           If 0, the function loads from the current executable.
 *   lpMenuName : LPCTSTR - The name of the menu resource. This can be either a string or an integer resource ID.
 *
 * Returns:
 *   HMENU - Handle to the loaded menu. Returns NULL if the menu could not be loaded.
 *
 * Purpose:
 *   This function loads a menu resource from a resource file. This is the
 *   most common way to create menus in Windows applications.
 *
 * Notes:
 *   If hInstance is 0, the function loads the menu from the executable file
 *   of the current process.
 *   The lpMenuName parameter can be either a string or an integer resource ID.
 *   If it is an integer, it must be converted to a string using the MAKEINTRESOURCE() macro.
 */
HB_FUNC( LOADMENU )
{
   HMENU       hMenu = ( HMENU ) NULL;
   HINSTANCE   hInstance = HB_ISNUM( 1 ) ? hmg_par_raw_HINSTANCE( 1 ) : GetResources();
   LPCTSTR     lpMenuName;

   if( HB_ISNUM( 2 ) )
   {
      lpMenuName = MAKEINTRESOURCE( hmg_par_WORD( 2 ) );

      hMenu = LoadMenu( hInstance, lpMenuName );
   }
   else if( HB_ISCHAR( 2 ) )
   {
#ifndef __XHARBOUR__
      void  *hMenuName;
      lpMenuName = HB_PARSTR( 2, &hMenuName, NULL );
#else
      lpMenuName = ( LPCTSTR ) hb_parc( 2 );
#endif
      hMenu = LoadMenu( hInstance, lpMenuName );
#ifndef __XHARBOUR__
      hb_strfree( hMenuName );
#endif
   }

   hmg_ret_raw_HMENU( hMenu );
}

/*
 * FUNCTION _NEWMENUSTYLE( bCustomDraw )
 *
 * Sets or retrieves the flag indicating whether custom menu drawing is enabled.
 *
 * Parameters:
 *   bCustomDraw : HB_BOOL (Optional) - A logical value indicating whether to enable custom menu drawing.
 *                 If .T., custom menu drawing is enabled. If .F., it is disabled.
 *                 If omitted, the function only retrieves the current setting.
 *
 * Returns:
 *   HB_BOOL - A logical value indicating the current state of custom menu drawing.
 *             Returns .T. if custom menu drawing is enabled, .F. otherwise.
 *
 * Purpose:
 *   This function allows you to enable or disable custom menu drawing.
 *   Custom menu drawing allows you to customize the appearance of menus
 *   beyond the standard Windows menu styles.
 *
 * Notes:
 *   This function affects the global variable s_bCustomDraw.
 */
HB_FUNC( _NEWMENUSTYLE )
{
   if( HB_ISLOG( 1 ) )
   {
      s_bCustomDraw = hb_parl( 1 );
   }

   hb_retl( s_bCustomDraw );
}

/*
 * FUNCTION _CLOSEMENU()
 *
 * Ends the current menu.
 *
 * Parameters:
 *   None.
 *
 * Returns:
 *   HB_BOOL - A logical value indicating whether the menu was successfully ended.
 *             Returns the result of the EndMenu() function.
 *
 * Purpose:
 *   This function is used to programmatically end a menu.
 *
 * Notes:
 *   This function calls the Windows API function EndMenu().
 */
HB_FUNC( _CLOSEMENU )
{
   hb_retl( EndMenu() );
}

/*
 * FUNCTION TRACKPOPUPMENU( hMenu, x, y, hwnd, bPostNull )
 *
 * Displays a popup menu.
 *
 * Parameters:
 *   hMenu     : HMENU - Handle to the popup menu to display.
 *   x         : INT - The horizontal screen coordinate at which to display the menu.
 *   y         : INT - The vertical screen coordinate at which to display the menu.
 *   hwnd      : HWND - Handle to the window that owns the menu. This window receives all messages from the menu.
 *   bPostNull : HB_BOOL (Optional) - A logical value indicating whether to post a WM_NULL message to the window after the menu is closed.
 *               This is a workaround for a Microsoft "feature" that can prevent tray menus from closing properly.
 *
 * Returns:
 *   None. This function does not return a value.
 *
 * Purpose:
 *   This function displays a popup menu at the specified coordinates.
 *   It is commonly used to display context menus when the user right-clicks
 *   on a window or control.
 *
 * Notes:
 *   The function uses SetForegroundWindow() to ensure that the menu is displayed
 *   correctly.
 *   The bPostNull parameter is a workaround for a Microsoft "feature" that can
 *   prevent tray menus from closing properly.
 */
HB_FUNC( TRACKPOPUPMENU )
{
   HWND  hwnd = hmg_par_raw_HWND( 4 );

   SetForegroundWindow( hwnd );           /* hack for Microsoft "feature" */

   TrackPopupMenu( hmg_par_raw_HMENU( 1 ), 0, hb_parni( 2 ), hb_parni( 3 ), 0, hwnd, NULL );

   if( hb_pcount() > 4 && HB_ISLOG( 5 ) && hb_parl( 5 ) )
   {
      PostMessage( hwnd, WM_NULL, 0, 0 ); /* hack for tray menu closing */
   }
}

/*
 * FUNCTION SETMENU( hWnd, hMenu )
 *
 * Sets the menu for a window.
 *
 * Parameters:
 *   hWnd  : HWND - Handle to the window.
 *   hMenu : HMENU - Handle to the menu to set.
 *
 * Returns:
 *   None. This function does not return a value.
 *
 * Purpose:
 *   This function sets the menu for a window. The menu will be displayed
 *   at the top of the window.
 *
 * Notes:
 *   This function calls the Windows API function SetMenu().
 */
HB_FUNC( SETMENU )
{
   SetMenu( hmg_par_raw_HWND( 1 ), hmg_par_raw_HMENU( 2 ) );
}

/*
 * FUNCTION SETMENUDEFAULTITEM( hMenu, uItem, fByPos )
 *
 * Sets the default menu item for a menu.
 *
 * Parameters:
 *   hMenu  : HMENU - Handle to the menu.
 *   uItem : INT - The menu item to set as the default.
 *   fByPos : BOOL - Specifies whether the uItem parameter is the position of the menu item or its command ID.  Always FALSE in this implementation.
 *
 * Returns:
 *   None. This function does not return a value.
 *
 * Purpose:
 *   This function sets the default menu item for a menu. The default menu item
 *   is the item that is selected when the user presses the Enter key.
 *
 * Notes:
 *   This function calls the Windows API function SetMenuDefaultItem().
 *   The fByPos parameter is always FALSE in this implementation, meaning that
 *   the uItem parameter is always interpreted as a command ID.
 */
HB_FUNC( SETMENUDEFAULTITEM )
{
   SetMenuDefaultItem( hmg_par_raw_HMENU( 1 ), hb_parni( 2 ), FALSE );
}

/*
 * FUNCTION XCHECKMENUITEM( hMenu, uIDCheckItem )
 *
 * Checks a menu item.
 *
 * Parameters:
 *   hMenu       : HMENU - Handle to the menu.
 *   uIDCheckItem : INT - The command ID of the menu item to check.
 *
 * Returns:
 *   None. This function does not return a value.
 *
 * Purpose:
 *   This function checks a menu item, adding a checkmark next to it.
 *
 * Notes:
 *   This function calls the Windows API function CheckMenuItem().
 */
HB_FUNC( XCHECKMENUITEM )
{
   CheckMenuItem( hmg_par_raw_HMENU( 1 ), hb_parni( 2 ), MF_CHECKED );
}

/*
 * FUNCTION XUNCHECKMENUITEM( hMenu, uIDCheckItem )
 *
 * Unchecks a menu item.
 *
 * Parameters:
 *   hMenu       : HMENU - Handle to the menu.
 *   uIDCheckItem : INT - The command ID of the menu item to uncheck.
 *
 * Returns:
 *   None. This function does not return a value.
 *
 * Purpose:
 *   This function unchecks a menu item, removing the checkmark next to it.
 *
 * Notes:
 *   This function calls the Windows API function CheckMenuItem().
 */
HB_FUNC( XUNCHECKMENUITEM )
{
   CheckMenuItem( hmg_par_raw_HMENU( 1 ), hb_parni( 2 ), MF_UNCHECKED );
}

/*
 * FUNCTION XENABLEMENUITEM( hMenu, uIDEnableItem )
 *
 * Enables a menu item.
 *
 * Parameters:
 *   hMenu       : HMENU - Handle to the menu.
 *   uIDEnableItem : INT - The command ID of the menu item to enable.
 *
 * Returns:
 *   None. This function does not return a value.
 *
 * Purpose:
 *   This function enables a menu item, allowing the user to select it.
 *
 * Notes:
 *   This function calls the Windows API function EnableMenuItem().
 */
HB_FUNC( XENABLEMENUITEM )
{
   EnableMenuItem( hmg_par_raw_HMENU( 1 ), hb_parni( 2 ), MF_ENABLED );
}

/*
 * FUNCTION XDISABLEMENUITEM( hMenu, uIDEnableItem )
 *
 * Disables a menu item.
 *
 * Parameters:
 *   hMenu       : HMENU - Handle to the menu.
 *   uIDEnableItem : INT - The command ID of the menu item to disable.
 *
 * Returns:
 *   None. This function does not return a value.
 *
 * Purpose:
 *   This function disables a menu item, preventing the user from selecting it.
 *   The menu item is typically grayed out to indicate that it is disabled.
 *
 * Notes:
 *   This function calls the Windows API function EnableMenuItem().
 */
HB_FUNC( XDISABLEMENUITEM )
{
   EnableMenuItem( hmg_par_raw_HMENU( 1 ), hb_parni( 2 ), MF_GRAYED );
}

/*
 * FUNCTION XDISABLECLOSEBUTTON( hWnd, bEnable )
 *
 * Enables or disables the Close button on a window's system menu.
 *
 * Parameters:
 *   hWnd    : HWND - Handle to the window.
 *   bEnable : HB_BOOL - A logical value indicating whether to enable or disable the Close button.
 *             If .T., the Close button is enabled. If .F., it is disabled.
 *
 * Returns:
 *   None. This function does not return a value.
 *
 * Purpose:
 *   This function allows you to control whether the user can close a window
 *   using the Close button in the system menu.
 *
 * Notes:
 *   This function calls the Windows API function EnableMenuItem() to enable or
 *   disable the SC_CLOSE command in the system menu.
 */
HB_FUNC( XDISABLECLOSEBUTTON )
{
   EnableMenuItem( GetSystemMenu( hmg_par_raw_HWND( 1 ), FALSE ), SC_CLOSE, MF_BYCOMMAND | ( hb_parl( 2 ) ? MF_ENABLED : MF_GRAYED ) );
}

/*
 * FUNCTION CREATEMENU()
 *
 * Creates a new menu.
 *
 * Parameters:
 *   None.
 *
 * Returns:
 *   HMENU - Handle to the created menu.
 *
 * Purpose:
 *   This function creates a new, empty menu. You can then add menu items
 *   to the menu using functions such as AppendMenu() and InsertMenu().
 *
 * Notes:
 *   This function calls the Windows API function CreateMenu().
 */
HB_FUNC( CREATEMENU )
{
   HMENU hMenu = CreateMenu();

#ifndef __WINNT__
   if( s_bCustomDraw )
   {
      SetMenuBarColor( hMenu, clrMenuBar1, TRUE );
   }
#endif
   hmg_ret_raw_HMENU( hMenu );
}

/*
 * FUNCTION CREATEPOPUPMENU()
 *
 * Creates a new popup menu.
 *
 * Parameters:
 *   None.
 *
 * Returns:
 *   HMENU - Handle to the created popup menu.
 *
 * Purpose:
 *   This function creates a new, empty popup menu. Popup menus are typically
 *   displayed when the user right-clicks on a window or control.
 *
 * Notes:
 *   This function calls the Windows API function CreatePopupMenu().
 */
HB_FUNC( CREATEPOPUPMENU )
{
   hmg_ret_raw_HMENU( CreatePopupMenu() );
}

/*
 * FUNCTION: APPENDMENUSTRING
 *
 * Appends a new menu item to the end of a menu.
 *
 * Parameters:
 *   hMenu: HMENU - Handle to the menu to which the item will be appended.
 *   nIDNewItem: INT - The command ID of the new menu item.
 *   lpNewItem: STRING - The text of the new menu item.
 *   nStyle: INT - The style of the new menu item. Can be 0 (MF_STRING), 1 (MF_STRING | MF_MENUBREAK), or 2 (MF_STRING | MF_MENUBARBREAK).
 *
 * Returns:
 *   HB_BOOL - A logical value indicating whether the menu item was successfully appended.
 *             Returns .T. if the menu item was appended successfully, .F. otherwise.
 *
 * Purpose:
 *   This function appends a new menu item to the end of a menu.
 *   The menu item can be a string, a separator, or a submenu.
 *
 * Notes:
 *   This function calls the Windows API function AppendMenu().
 *   The nStyle parameter determines the appearance and behavior of the menu item.
 *   If custom menu drawing is enabled (s_bCustomDraw is .T.), the function
 *   creates a MENUITEM structure and passes it to AppendMenu(). Otherwise, it
 *   passes the string directly to AppendMenu().
 */
HB_FUNC( APPENDMENUSTRING )
{
#ifndef UNICODE
   LPCSTR   lpNewItem = hb_parc( 3 );     // Get the text of the new menu item as an ANSI string
#else
   LPWSTR   lpNewItem = AnsiToWide( ( char * ) hb_parc( 3 ) ); // Convert the text of the new menu item to a wide string for Unicode
#endif
   UINT     Style;            // Variable to hold the style of the new menu item

   // Check if custom menu drawing is enabled
   if( s_bCustomDraw )
   {
      LPMENUITEM  lpMenuItem; // Pointer to a MENUITEM structure
      UINT        cch = ( UINT ) HB_STRNLEN( lpNewItem, MAX_ITEM_TEXT * sizeof( TCHAR ) );   // Calculate the length of the menu item text
      lpMenuItem = ( LPMENUITEM ) hb_xgrab( ( sizeof( MENUITEM ) ) );   // Allocate memory for the MENUITEM structure
      ZeroMemory( lpMenuItem, sizeof( MENUITEM ) );                     // Initialize the MENUITEM structure to zero

      // Set the properties of the MENUITEM structure
      lpMenuItem->cbSize = hb_parni( 2 );                   // Set the size of the structure
      lpMenuItem->uiID = hb_parni( 2 );                     // Set the command ID of the menu item
      lpMenuItem->caption = HB_STRNDUP( lpNewItem, cch );   // Duplicate the menu item text
      lpMenuItem->cch = cch;                    // Set the length of the menu item text
      lpMenuItem->hBitmap = ( HBITMAP ) NULL;   // Set the bitmap handle to NULL
      lpMenuItem->hFont = ( HFONT ) NULL;       // Set the font handle to NULL
      lpMenuItem->uiItemType = hb_parni( 4 );   // Set the item type
      lpMenuItem->hwnd = ( HWND ) NULL;         // Set the window handle to NULL

      // Determine the style of the menu item based on the nStyle parameter
      switch( hb_parni( 4 ) )
      {
         case 1:
            Style = MF_OWNERDRAW | MF_MENUBREAK;      // Set style for owner-drawn menu item with a menu break
            break;

         case 2:
            Style = MF_OWNERDRAW | MF_MENUBARBREAK;   // Set style for owner-drawn menu item with a menu bar break
            break;

         default:
            Style = MF_OWNERDRAW;                     // Set style for owner-drawn menu item
      }

      // Append the menu item to the menu and return the result
      hb_retl( AppendMenu( hmg_par_raw_HMENU( 1 ), Style, hb_parni( 2 ), ( LPTSTR ) lpMenuItem ) );
   }
   else
   {
      // Determine the style of the menu item based on the nStyle parameter
      switch( hb_parni( 4 ) )
      {
         case 1:
            Style = MF_STRING | MF_MENUBREAK;         // Set style for string menu item with a menu break
            break;

         case 2:
            Style = MF_STRING | MF_MENUBARBREAK;      // Set style for string menu item with a menu bar break
            break;

         default:
            Style = MF_STRING;         // Set style for string menu item
      }

      // Append the menu item to the menu and return the result
      hb_retl( AppendMenu( hmg_par_raw_HMENU( 1 ), Style, hb_parni( 2 ), lpNewItem ) );
   }

#ifdef UNICODE
   hb_xfree( lpNewItem );              // Free the wide string memory if Unicode is defined
#endif
}

/*
 * FUNCTION: APPENDMENUPOPUP
 *
 * Appends a new popup menu item to the end of a menu.
 *
 * Parameters:
 *   hMenu: HMENU - Handle to the menu to which the item will be appended.
 *   nIDNewItem: INT - The command ID of the new menu item.
 *   lpNewItem: STRING - The text of the new menu item.
 *   nStyle: INT - The style of the new menu item.
 *   hFont: HFONT - The font to use for the new menu item.
 *
 * Returns:
 *   HB_BOOL - A logical value indicating whether the menu item was successfully appended.
 *             Returns .T. if the menu item was appended successfully, .F. otherwise.
 *
 * Purpose:
 *   This function appends a new popup menu item to the end of a menu.
 *   The menu item can be a string, a separator, or a submenu.
 *
 * Notes:
 *   This function calls the Windows API function AppendMenu().
 *   If custom menu drawing is enabled (s_bCustomDraw is .T.), the function
 *   creates a MENUITEM structure and passes it to AppendMenu(). Otherwise, it
 *   passes the string directly to AppendMenu().
 */
HB_FUNC( APPENDMENUPOPUP )
{
#ifndef UNICODE
   LPCSTR   lpNewItem = hb_parc( 3 );  // Get the text of the new menu item as an ANSI string
#else
   LPWSTR   lpNewItem = AnsiToWide( ( char * ) hb_parc( 3 ) ); // Convert the text of the new menu item to a wide string for Unicode
#endif

   // Check if custom menu drawing is enabled
   if( s_bCustomDraw )
   {
      LPMENUITEM  lpMenuItem; // Pointer to a MENUITEM structure
      UINT        cch = ( UINT ) HB_STRNLEN( lpNewItem, MAX_ITEM_TEXT * sizeof( TCHAR ) );   // Calculate the length of the menu item text
      lpMenuItem = ( LPMENUITEM ) hb_xgrabz( ( sizeof( MENUITEM ) ) );  // Allocate and zero memory for the MENUITEM structure

      // Set the properties of the MENUITEM structure
      lpMenuItem->cbSize = hb_parni( 2 );                   // Set the size of the structure
      lpMenuItem->uiID = hb_parni( 2 );                     // Set the command ID of the menu item
      lpMenuItem->caption = HB_STRNDUP( lpNewItem, cch );   // Duplicate the menu item text
      lpMenuItem->cch = cch;                    // Set the length of the menu item text
      lpMenuItem->hBitmap = ( HBITMAP ) NULL;   // Set the bitmap handle to NULL
      lpMenuItem->hFont = hmg_par_raw_HFONT( 5 );  // Set the font handle
      lpMenuItem->uiItemType = hb_parni( 4 );      // Set the item type

      // Append the popup menu item to the menu and return the result
      hb_retl( AppendMenu( hmg_par_raw_HMENU( 1 ), MF_POPUP | MF_OWNERDRAW, hb_parni( 2 ), ( LPTSTR ) lpMenuItem ) );
   }
   else
   {
      // Append the popup menu item to the menu and return the result
      hb_retl( AppendMenu( hmg_par_raw_HMENU( 1 ), MF_POPUP | MF_STRING, hb_parni( 2 ), lpNewItem ) );
   }

#ifdef UNICODE
   hb_xfree( lpNewItem );  // Free the wide string memory if Unicode is defined
#endif
}

/*
 * FUNCTION: APPENDMENUSEPARATOR
 *
 * Appends a new separator item to the end of a menu.
 *
 * Parameters:
 *   hMenu: HMENU - Handle to the menu to which the separator will be appended.
 *
 * Returns:
 *   HB_BOOL - A logical value indicating whether the separator was successfully appended.
 *             Returns .T. if the separator was appended successfully, .F. otherwise.
 *
 * Purpose:
 *   This function appends a new separator item to the end of a menu.
 *   The separator is used to visually separate groups of menu items.
 *
 * Notes:
 *   This function calls the Windows API function AppendMenu().
 *   If custom menu drawing is enabled (s_bCustomDraw is .T.), the function
 *   creates a MENUITEM structure and passes it to AppendMenu(). Otherwise, it
 *   passes NULL directly to AppendMenu().
 */
HB_FUNC( APPENDMENUSEPARATOR )
{
   // Check if custom menu drawing is enabled
   if( s_bCustomDraw )
   {
      LPMENUITEM  lpMenuItem = ( LPMENUITEM ) hb_xgrabz( ( sizeof( MENUITEM ) ) );  // Allocate and zero memory for the MENUITEM structure
      lpMenuItem->uiItemType = 1000;   // Set the item type to separator

      // Append the separator to the menu and return the result
      hb_retl( AppendMenu( hmg_par_raw_HMENU( 1 ), MF_SEPARATOR | MF_OWNERDRAW, 0, ( LPTSTR ) lpMenuItem ) );
   }
   else
   {
      // Append the separator to the menu and return the result
      hb_retl( AppendMenu( hmg_par_raw_HMENU( 1 ), MF_SEPARATOR, 0, NULL ) );
   }
}

/*
 * FUNCTION: MODIFYMENUITEM
 *
 * Modifies an existing menu item.
 *
 * Parameters:
 *   hMenu: HMENU - Handle to the menu containing the item to modify.
 *   nPosition: INT - The position of the menu item to modify.
 *   nIDNewItem: INT - The new command ID of the menu item.
 *   lpNewItem: STRING - The new text of the menu item.
 *
 * Returns:
 *   HB_BOOL - A logical value indicating whether the menu item was successfully modified.
 *             Returns .T. if the menu item was modified successfully, .F. otherwise.
 *
 * Purpose:
 *   This function modifies an existing menu item in a menu.
 *   The menu item can be a string, a separator, or a submenu.
 *
 * Notes:
 *   This function calls the Windows API function ModifyMenu().
 */
HB_FUNC( MODIFYMENUITEM )
{
#ifndef UNICODE
   LPCSTR   lpNewItem = hb_parc( 4 );  // Get the new text of the menu item as an ANSI string
#else
   LPWSTR   lpNewItem = AnsiToWide( ( char * ) hb_parc( 4 ) ); // Convert the new text of the menu item to a wide string for Unicode
#endif

   // Modify the menu item and return the result
   hb_retl( ModifyMenu( hmg_par_raw_HMENU( 1 ), hb_parni( 2 ), MF_BYCOMMAND | MF_STRING, hb_parni( 3 ), lpNewItem ) );

#ifdef UNICODE
   hb_xfree( lpNewItem );              // Free the wide string memory if Unicode is defined
#endif
}

/*
 * FUNCTION: INSERTMENUITEM
 *
 * Inserts a new menu item at a specified position in a menu.
 *
 * Parameters:
 *   hMenu: HMENU - Handle to the menu where the item will be inserted.
 *   nPosition: INT - The position at which the new menu item will be inserted.
 *   nIDNewItem: INT - The command ID of the new menu item.
 *   lpNewItem: STRING - The text of the new menu item.
 *
 * Returns:
 *   HB_BOOL - A logical value indicating whether the menu item was successfully inserted.
 *             Returns .T. if the menu item was inserted successfully, .F. otherwise.
 *
 * Purpose:
 *   This function inserts a new menu item at a specified position in a menu.
 *   The menu item can be a string, a separator, or a submenu.
 *
 * Notes:
 *   This function calls the Windows API function InsertMenu().
 */
HB_FUNC( INSERTMENUITEM )
{
#ifndef UNICODE
   LPCSTR   lpNewItem = hb_parc( 4 );  // Get the text of the new menu item as an ANSI string
#else
   LPWSTR   lpNewItem = AnsiToWide( ( char * ) hb_parc( 4 ) ); // Convert the text of the new menu item to a wide string for Unicode
#endif

   // Insert the menu item at the specified position and return the result
   hb_retl( InsertMenu( hmg_par_raw_HMENU( 1 ), hb_parni( 2 ), MF_BYCOMMAND | MF_STRING, hb_parni( 3 ), lpNewItem ) );

#ifdef UNICODE
   hb_xfree( lpNewItem );  // Free the wide string memory if Unicode is defined
#endif
}

/*
 * FUNCTION: REMOVEMENUITEM
 *
 * Removes an existing menu item from a menu.
 *
 * Parameters:
 *   hMenu: HMENU - Handle to the menu containing the item to remove.
 *   nPosition: INT - The position of the menu item to remove.
 *
 * Returns:
 *   HB_BOOL - A logical value indicating whether the menu item was successfully removed.
 *             Returns .T. if the menu item was removed successfully, .F. otherwise.
 *
 * Purpose:
 *   This function removes an existing menu item from a menu.
 *
 * Notes:
 *   This function calls the Windows API function RemoveMenu().
 */
HB_FUNC( REMOVEMENUITEM )
{
   // Remove the menu item at the specified position and return the result
   hb_retl( RemoveMenu( hmg_par_raw_HMENU( 1 ), hb_parni( 2 ), MF_BYCOMMAND ) );
}

HB_FUNC( MENUITEM_SETBITMAPS )
{
   HBITMAP  himage1;
   int      Transparent = s_bCustomDraw ? 0 : 1;

   himage1 = HMG_LoadPicture( hb_parc( 3 ), -1, -1, NULL, 0, Transparent, -1, 0, HB_FALSE, 255 );

   if( s_bCustomDraw )
   {
      MENUITEMINFO   MenuItemInfo;
      MENUITEM       *pMENUITEM;

      MenuItemInfo.cbSize = sizeof( MENUITEMINFO );
      MenuItemInfo.fMask = MIIM_DATA;

      if( GetMenuItemInfo( hmg_par_raw_HMENU( 1 ), hb_parni( 2 ), FALSE, &MenuItemInfo ) )
      {
         pMENUITEM = ( MENUITEM * ) MenuItemInfo.dwItemData;
         if( pMENUITEM->hBitmap != NULL )
         {
            DeleteObject( pMENUITEM->hBitmap );
         }

         pMENUITEM->hBitmap = himage1;
      }
   }
   else
   {
      HBITMAP  himage2;

      himage2 = HMG_LoadPicture( hb_parc( 4 ), -1, -1, NULL, 0, Transparent, -1, 0, HB_FALSE, 255 );

      SetMenuItemBitmaps( hmg_par_raw_HMENU( 1 ), hb_parni( 2 ), MF_BYCOMMAND, himage1, himage2 );
   }

   hmg_ret_raw_HANDLE( himage1 );
}

HB_FUNC( MENUITEM_SETCHECKMARKS )
{
   if( s_bCustomDraw )
   {
      MENUITEMINFO   MenuItemInfo;
      HBITMAP        himage1;
      HBITMAP        himage2;

      himage1 = HMG_LoadPicture( hb_parc( 3 ), -1, -1, NULL, 0, 0, -1, 0, HB_FALSE, 255 );
      himage2 = HMG_LoadPicture( hb_parc( 4 ), -1, -1, NULL, 0, 0, -1, 0, HB_FALSE, 255 );

      MenuItemInfo.cbSize = sizeof( MENUITEMINFO );
      MenuItemInfo.fMask = MIIM_CHECKMARKS;

      if( GetMenuItemInfo( hmg_par_raw_HMENU( 1 ), hb_parni( 2 ), FALSE, &MenuItemInfo ) )
      {
         if( MenuItemInfo.hbmpChecked != NULL )
         {
            DeleteObject( MenuItemInfo.hbmpChecked );
         }

         MenuItemInfo.hbmpChecked = himage1;

         if( MenuItemInfo.hbmpUnchecked != NULL )
         {
            DeleteObject( MenuItemInfo.hbmpUnchecked );
         }

         MenuItemInfo.hbmpUnchecked = himage2;

         SetMenuItemInfo( hmg_par_raw_HMENU( 1 ), hb_parni( 2 ), FALSE, &MenuItemInfo );
      }
   }
}

HB_FUNC( MENUITEM_SETICON )
{
   HBITMAP  himage1;
   HICON    hIcon;

#ifndef UNICODE
   LPCSTR   lpIconName = hb_parc( 3 );
#else
   LPWSTR   lpIconName = AnsiToWide( ( char * ) hb_parc( 3 ) );
#endif
   hIcon = ( HICON ) LoadImage( GetResources(), lpIconName, IMAGE_ICON, 0, 0, LR_DEFAULTSIZE | LR_DEFAULTCOLOR );
   if( hIcon == NULL )
   {
      hIcon = ( HICON ) LoadImage( NULL, lpIconName, IMAGE_ICON, 0, 0, LR_LOADFROMFILE | LR_DEFAULTCOLOR );
   }

   // convert icon to bitmap
   himage1 = Icon2Bmp( hIcon );

   if( s_bCustomDraw )
   {
      MENUITEMINFO   MenuItemInfo;
      LPMENUITEM     lpMenuItem;

      MenuItemInfo.cbSize = sizeof( MENUITEMINFO );
      MenuItemInfo.fMask = MIIM_DATA;

      if( GetMenuItemInfo( hmg_par_raw_HMENU( 1 ), hb_parni( 2 ), FALSE, &MenuItemInfo ) )
      {
         lpMenuItem = ( LPMENUITEM ) MenuItemInfo.dwItemData;
         if( lpMenuItem->hBitmap != NULL )
         {
            DeleteObject( lpMenuItem->hBitmap );
         }

         lpMenuItem->hBitmap = himage1;
      }
   }
   else
   {
      SetMenuItemBitmaps( hmg_par_raw_HMENU( 1 ), hb_parni( 2 ), MF_BYCOMMAND, himage1, himage1 );
   }

   DestroyIcon( hIcon );

   hmg_ret_raw_HANDLE( himage1 );

#ifdef UNICODE
   hb_xfree( lpIconName );
#endif
}

HB_FUNC( MENUITEM_SETFONT )
{
   if( s_bCustomDraw )
   {
      MENUITEMINFO   MenuItemInfo;
      LPMENUITEM     lpMenuItem;

      MenuItemInfo.cbSize = sizeof( MENUITEMINFO );
      MenuItemInfo.fMask = MIIM_DATA;

      if( GetMenuItemInfo( hmg_par_raw_HMENU( 1 ), hb_parni( 2 ), FALSE, &MenuItemInfo ) )
      {
         lpMenuItem = ( LPMENUITEM ) MenuItemInfo.dwItemData;

         if( GetObjectType( hmg_par_raw_HGDIOBJ( 3 ) ) == OBJ_FONT )
         {
            if( lpMenuItem->hFont != NULL )
            {
               DeleteObject( lpMenuItem->hFont );
            }

            lpMenuItem->hFont = hmg_par_raw_HFONT( 3 );
         }
      }
   }
}

HB_FUNC( XGETMENUCAPTION )
{
   if( s_bCustomDraw )
   {
#ifdef UNICODE
      LPSTR          pStr;
#endif
      MENUITEMINFO   MenuItemInfo;
      MENUITEM       *lpMenuItem;

      MenuItemInfo.cbSize = sizeof( MENUITEMINFO );
      MenuItemInfo.fMask = MIIM_DATA;
      GetMenuItemInfo( hmg_par_raw_HMENU( 1 ), hb_parni( 2 ), FALSE, &MenuItemInfo );

      lpMenuItem = ( MENUITEM * ) MenuItemInfo.dwItemData;

      if( lpMenuItem->caption != NULL )
      {
#ifndef UNICODE
         hb_retclen( lpMenuItem->caption, lpMenuItem->cch );
#else
         pStr = WideToAnsi( lpMenuItem->caption );
         hb_retclen( pStr, lpMenuItem->cch );
         hb_xfree( pStr );
#endif
      }
      else
      {
         hb_retc( "" );
      }
   }
}

HB_FUNC( XSETMENUCAPTION )
{
   if( s_bCustomDraw )
   {
#ifndef UNICODE
      LPCSTR         lpNewItem = hb_parc( 3 );
#else
      LPWSTR         lpNewItem = AnsiToWide( ( char * ) hb_parc( 3 ) );
      LPSTR          pStr;
#endif
      MENUITEMINFO   MenuItemInfo;
      MENUITEM       *lpMenuItem;

      MenuItemInfo.cbSize = sizeof( MENUITEMINFO );
      MenuItemInfo.fMask = MIIM_DATA;
      GetMenuItemInfo( hmg_par_raw_HMENU( 1 ), hb_parni( 2 ), FALSE, &MenuItemInfo );

      lpMenuItem = ( MENUITEM * ) MenuItemInfo.dwItemData;

      if( lpMenuItem->caption != NULL )
      {
         UINT  cch = ( UINT ) HB_STRNLEN( lpNewItem, MAX_ITEM_TEXT * sizeof( TCHAR ) );

#ifndef UNICODE
         hb_retclen( lpMenuItem->caption, lpMenuItem->cch );
#else
         pStr = WideToAnsi( lpMenuItem->caption );
         hb_retclen( pStr, lpMenuItem->cch );
         hb_xfree( pStr );
#endif
         hb_xfree( lpMenuItem->caption );

         lpMenuItem->cch = cch;
         lpMenuItem->caption = HB_STRNDUP( lpNewItem, cch );
      }
      else
      {
         hb_retc( "" );
      }

#ifdef UNICODE
      hb_xfree( lpNewItem );
#endif
   }
}

HB_FUNC( XGETMENUCHECKSTATE )
{
   UINT  state = GetMenuState( hmg_par_raw_HMENU( 1 ), hb_parni( 2 ), MF_BYCOMMAND );

   if( state != 0xFFFFFFFF )
   {
      hmg_ret_L( ( state & MF_CHECKED ) );
   }
   else
   {
      hb_retl( HB_FALSE );
   }
}

HB_FUNC( XGETMENUENABLEDSTATE )
{
   UINT  state = GetMenuState( hmg_par_raw_HMENU( 1 ), hb_parni( 2 ), MF_BYCOMMAND );

   if( state != 0xFFFFFFFF )
   {
      hmg_ret_L( !( ( state & MF_GRAYED ) || ( state & MF_DISABLED ) ) );
   }
   else
   {
      hb_retl( HB_FALSE );
   }
}

HB_FUNC( ISMENU )
{
   hb_retl( IsMenu( hmg_par_raw_HMENU( 1 ) ) );
}

HB_FUNC( GETMENU )
{
   hmg_ret_raw_HMENU( GetMenu( hmg_par_raw_HWND( 1 ) ) );
}

HB_FUNC( GETSYSTEMMENU )
{
   hmg_ret_raw_HMENU( GetSystemMenu( hmg_par_raw_HWND( 1 ), FALSE ) );
}

HB_FUNC( GETMENUITEMCOUNT )
{
   hmg_ret_NINT( GetMenuItemCount( hmg_par_raw_HMENU( 1 ) ) );
}

/*
 * Owner draw function
 */

/*
 * ODA_DRAWENTIRE - This bit is set when the entire control needs to be drawn.
 * ODA_FOCUS - This bit is set when the control gains or loses input focus. The itemState member should be checked to determine whether the control has focus.
 * ODA_SELECT - This bit is set when only the selection status has changed. The itemState member should be checked to determine the new selection state.
 *
 * Owner draw state
 *
 * ODS_CHECKED - This bit is set if the menu item is to be checked. This bit is used only in a menu.
 * ODS_DISABLED - This bit is set if the item is to be drawn as disabled.
 * ODS_FOCUS - This bit is set if the item has input focus.
 * ODS_GRAYED - This bit is set if the item is to be dimmed. This bit is used only in a menu.
 * ODS_SELECTED - This bit is set if the item's status is selected.
 * ODS_COMBOBOXEDIT - The drawing takes place in the selection field (edit control) of an ownerdrawn combo box.
 * ODS_DEFAULT - The item is the default item.
 */
HB_FUNC( _ONDRAWMENUITEM )
{
   LPDRAWITEMSTRUCT  lpdis;
   MENUITEM          *lpMenuItem;
   int               iLen;
   COLORREF          clrText, clrBackground;
   UINT              bkMode;
   BOOL              fSelected = FALSE;
   BOOL              fGrayed = FALSE;
   BOOL              fChecked = FALSE;

   HFONT             oldfont;

   lpdis = ( LPDRAWITEMSTRUCT ) HB_PARNL( 1 );
   lpMenuItem = ( MENUITEM * ) lpdis->itemData;

   if( lpdis->CtlType != ODT_MENU )
   {
      return;
   }

   // draw SEPARATOR
   if( lpdis->itemID == 0 )
   {
      DrawSeparator( lpdis->hDC, lpdis->rcItem );
      return;
   }

   if( lpMenuItem->hFont != NULL )
   {
      oldfont = ( HFONT ) SelectObject( lpdis->hDC, lpMenuItem->hFont );
   }
   else
   {
      oldfont = ( HFONT ) SelectObject( lpdis->hDC, GetStockObject( DEFAULT_GUI_FONT ) );
   }

   // save prev. colours state
   clrText = SetTextColor( lpdis->hDC, clrText1 );
   clrBackground = SetBkColor( lpdis->hDC, clrBk1 );
   bkMode = SetBkMode( lpdis->hDC, TRANSPARENT );

   // set colours and flags ( fSelected etc. )
   if( ( ( lpdis->itemAction & ODA_SELECT ) || ( lpdis->itemAction & ODA_DRAWENTIRE ) ) && ( !( lpdis->itemState & ODS_GRAYED ) ) )
   {
      if( lpdis->itemState & ODS_SELECTED )
      {
         clrText = SetTextColor( lpdis->hDC, ( lpMenuItem->uiItemType != 1 ) ? clrSelectedText1 : clrMenuBarSelectedText );
         clrBackground = SetBkColor( lpdis->hDC, ( lpMenuItem->uiItemType != 1 ) ? clrSelectedBk1 : clrMenuBar1 );
         fSelected = TRUE;
      }
      else
      {
         clrText = SetTextColor( lpdis->hDC, ( lpMenuItem->uiItemType != 1 ) ? clrText1 : clrMenuBarText );
         clrBackground = SetBkColor( lpdis->hDC, ( lpMenuItem->uiItemType != 1 ) ? clrBk2 : clrMenuBar2 );
         fSelected = FALSE;
      }
   }

   if( lpdis->itemState & ODS_GRAYED )
   {
      clrText = SetTextColor( lpdis->hDC, ( lpMenuItem->uiItemType != 1 ) ? clrGrayedText1 : clrMenuBarGrayedText );
      fGrayed = TRUE;
   }

   if( lpdis->itemState & ODS_CHECKED )
   {
      fChecked = TRUE;
   }

   // draw menu item bitmap background
   if( lpMenuItem->uiItemType != 1 )
   {
      DrawBitmapBK( lpdis->hDC, lpdis->rcItem );
   }

   //draw menu item background
   DrawItemBk( lpdis->hDC, lpdis->rcItem, fSelected, fGrayed, lpMenuItem->uiItemType, ( ( lpMenuItem->hBitmap == NULL ) && ( !fChecked ) ) );

   // draw menu item border
   if( fSelected && ( !fGrayed ) )
   {
      DrawSelectedItemBorder( lpdis->hDC, lpdis->rcItem, lpMenuItem->uiItemType, ( ( lpMenuItem->hBitmap == NULL ) && ( !fChecked ) ) );
   }

   // draw bitmap
   if( ( lpMenuItem->hBitmap != NULL ) && ( !fChecked ) )
   {
      DrawGlyph
      (
         lpdis->hDC,
         lpdis->rcItem.left + cx_delta - 2,
         lpdis->rcItem.top + cy_delta,
         bm_size,
         bm_size,
         lpMenuItem->hBitmap,
         RGB( 125, 125, 125 ),
         ( ( fGrayed ) ? TRUE : FALSE ),
         TRUE
      );

      if( fSelected && ( lpMenuItem->uiItemType != 1 ) && ( eMenuCursorType == Short ) && bSelectedItemBorder3d )
      {
         HPEN  pen, pen1, oldPen;
         RECT  rect;

         pen = CreatePen( PS_SOLID, 1, clrSelectedItemBorder2 );
         pen1 = CreatePen( PS_SOLID, 1, clrSelectedItemBorder4 );

         oldPen = ( HPEN ) SelectObject( lpdis->hDC, pen1 );

         CopyRect( &rect, &lpdis->rcItem );
         rect.left += ( cx_delta / 2 - 2 );
         rect.top += ( cy_delta / 2 );
         rect.right = rect.left + bm_size + cx_delta;
         rect.bottom = rect.top + bm_size + cy_delta;

         MoveToEx( lpdis->hDC, rect.left, rect.top, NULL );

         LineTo( lpdis->hDC, rect.right, rect.top );
         SelectObject( lpdis->hDC, pen );
         LineTo( lpdis->hDC, rect.right, rect.bottom );
         LineTo( lpdis->hDC, rect.left, rect.bottom );
         SelectObject( lpdis->hDC, pen1 );
         LineTo( lpdis->hDC, rect.left, rect.top );

         SelectObject( lpdis->hDC, oldPen );

         DeleteObject( pen );
         DeleteObject( pen1 );
      }
   }

   // draw menu item text
   iLen = ( int ) HB_STRNLEN( lpMenuItem->caption, MAX_ITEM_TEXT * sizeof( TCHAR ) );

   if( lpMenuItem->uiItemType == 1 )
   {
      DrawText( lpdis->hDC, lpMenuItem->caption, iLen, &lpdis->rcItem, DT_CENTER | DT_VCENTER | DT_SINGLELINE | DT_END_ELLIPSIS | DT_EXPANDTABS );
   }
   else
   {
      lpdis->rcItem.left += ( min_width + cx_delta + 2 );
      DrawText( lpdis->hDC, lpMenuItem->caption, iLen, &lpdis->rcItem, DT_LEFT | DT_VCENTER | DT_SINGLELINE | DT_END_ELLIPSIS | DT_EXPANDTABS );
      lpdis->rcItem.left -= ( min_width + cx_delta + 2 );
   }

   // draw menu item checked mark
   if( fChecked )
   {
      MENUITEMINFO   MenuItemInfo;
      SIZE           size;

      MenuItemInfo.cbSize = sizeof( MENUITEMINFO );
      MenuItemInfo.fMask = MIIM_CHECKMARKS;
      GetMenuItemInfo( ( HMENU ) lpdis->hwndItem, lpdis->itemID, FALSE, &MenuItemInfo );

      size.cx = bm_size;   //GetSystemMetrics(SM_CXMENUCHECK);
      size.cy = bm_size;   //GetSystemMetrics(SM_CYMENUCHECK);
      DrawCheck( lpdis->hDC, size, lpdis->rcItem, fGrayed, fSelected, MenuItemInfo.hbmpChecked );
   }

   SelectObject( lpdis->hDC, oldfont );

   // restore prev. colours state
   SetTextColor( lpdis->hDC, clrText );
   SetBkColor( lpdis->hDC, clrBackground );
   SetBkMode( lpdis->hDC, bkMode );
}

VOID DrawSeparator( HDC hDC, RECT r )
{
   HPEN  pen, oldPen;
   RECT  rect;

   CopyRect( &rect, &r );
   rect.right = rect.left + min_width + cx_delta / 2;

   if( ( EnabledGradient() ) && ( !IsColorEqual( clrImageBk1, clrImageBk2 ) ) )
   {
      FillGradient( hDC, &rect, FALSE, clrImageBk1, clrImageBk2 );
   }
   else
   {
      HBRUSH   brush = CreateSolidBrush( clrImageBk1 );
      FillRect( hDC, &rect, brush );
      DeleteObject( brush );
   }

   CopyRect( &rect, &r );
   rect.left += ( min_width + cx_delta / 2 );

   if( ( EnabledGradient() ) && ( !IsColorEqual( clrBk1, clrBk2 ) ) )
   {
      FillGradient( hDC, &rect, FALSE, clrBk1, clrBk2 );
   }
   else
   {
      HBRUSH   brush = CreateSolidBrush( clrBk1 );
      FillRect( hDC, &rect, brush );
      DeleteObject( brush );
   }

   CopyRect( &rect, &r );

   pen = CreatePen( PS_SOLID, 1, clrSeparator1 );
   oldPen = ( HPEN ) SelectObject( hDC, pen );

   if( eSeparatorPosition == Right )
   {
      rect.left += ( min_width + cx_delta + 2 );
   }
   else if( eSeparatorPosition == Middle )
   {
      rect.left += ( min_width - cx_delta );
      rect.right -= ( min_width - cx_delta );
   }

   rect.top += ( rect.bottom - rect.top ) / 2;
   MoveToEx( hDC, rect.left, rect.top, NULL );
   LineTo( hDC, rect.right, rect.top );

   if( eSeparatorType == Double )
   {
      HPEN  pen1, oldPen1;
      pen1 = CreatePen( PS_SOLID, 1, clrSeparator2 );
      oldPen1 = ( HPEN ) SelectObject( hDC, pen1 );

      rect.top += 1;
      MoveToEx( hDC, rect.left, rect.top, NULL );
      LineTo( hDC, rect.right, rect.top );

      SelectObject( hDC, oldPen1 );
      DeleteObject( pen1 );
   }

   SelectObject( hDC, oldPen );
   DeleteObject( pen );
}

VOID DrawBitmapBK( HDC hDC, RECT r )
{
   RECT  rect;

   CopyRect( &rect, &r );
   rect.right = rect.left + min_width + cx_delta / 2;

   if( ( EnabledGradient() ) && ( !IsColorEqual( clrImageBk1, clrImageBk2 ) ) )
   {
      FillGradient( hDC, &rect, FALSE, clrImageBk1, clrImageBk2 );
   }
   else
   {
      HBRUSH   brush = CreateSolidBrush( clrImageBk1 );
      FillRect( hDC, &rect, brush );
      DeleteObject( brush );
   }
}

VOID DrawItemBk( HDC hDC, RECT r, BOOL Selected, BOOL Grayed, UINT itemType, BOOL clear )
{
   RECT  rect;

   CopyRect( &rect, &r );
   if( ( !Selected ) && ( itemType != 1 ) )
   {
      rect.left += ( min_width + cx_delta / 2 );
   }

   if( ( Selected ) && ( itemType != 1 ) && ( eMenuCursorType == Short ) && ( !clear ) )
   {
      rect.left += ( min_width + cx_delta / 2 );
   }

   if( !Grayed )
   {
      if( Selected )
      {
         if( EnabledGradient() )
         {
            FillGradient
            (
               hDC,
               &rect,
               ( itemType == 1 ) ? TRUE : gradientVertical,
               ( itemType == 1 ) ? clrSelectedMenuBarItem1 : clrSelectedBk1,
               ( itemType == 1 ) ? clrSelectedMenuBarItem2 : clrSelectedBk2
            );
         }
         else
         {
            HBRUSH   brush = CreateSolidBrush( ( itemType == 1 ) ? clrSelectedMenuBarItem1 : clrSelectedBk1 );
            FillRect( hDC, &rect, brush );
            DeleteObject( brush );
         }
      }
      else
      {
         if( EnabledGradient() && ( !IsColorEqual( clrMenuBar1, clrMenuBar2 ) || ( !IsColorEqual( clrBk1, clrBk2 ) && ( itemType != 1 ) ) ) )
         {
            FillGradient
            (
               hDC,
               &rect,
               ( ( itemType == 1 ) ? TRUE : FALSE ),
               ( ( itemType == 1 ) ? clrMenuBar1 : clrBk1 ),
               ( ( itemType == 1 ) ? clrMenuBar2 : clrBk2 )
            );
         }
         else
         {
            HBRUSH   brush = CreateSolidBrush( ( itemType == 1 ) ? clrMenuBar1 : clrBk1 );
            FillRect( hDC, &rect, brush );
            DeleteObject( brush );
         }
      }
   }
   else
   {
      if( EnabledGradient() )
      {
         FillGradient( hDC, &rect, FALSE, clrGrayedBk1, clrGrayedBk2 );
      }
      else
      {
         HBRUSH   brush = CreateSolidBrush( clrGrayedBk1 );
         FillRect( hDC, &rect, brush );
         DeleteObject( brush );
      }
   }
}

VOID DrawSelectedItemBorder( HDC hDC, RECT r, UINT itemType, BOOL clear )
{
   HPEN  pen, pen1, oldPen;
   RECT  rect;

   if( itemType != 1 )
   {
      pen = CreatePen( PS_SOLID, 1, clrSelectedItemBorder1 );
      pen1 = CreatePen( PS_SOLID, 1, clrSelectedItemBorder3 );
   }
   else
   {
      pen = CreatePen( PS_SOLID, 1, clrSelectedItemBorder2 );
      pen1 = CreatePen( PS_SOLID, 1, clrSelectedItemBorder4 );
   }

   oldPen = ( HPEN ) SelectObject( hDC, pen );

   CopyRect( &rect, &r );
   if( ( eMenuCursorType == Short ) && ( itemType != 1 ) && ( !clear ) )
   {
      rect.left += ( min_width + cx_delta / 2 );
   }

   InflateRect( &rect, -1, -1 );

   MoveToEx( hDC, rect.left, rect.top, NULL );

   if( ( itemType == 1 ) && bSelectedItemBorder3d )
   {
      LineTo( hDC, rect.right, rect.top );
      SelectObject( hDC, pen1 );
      LineTo( hDC, rect.right, rect.bottom );
      LineTo( hDC, rect.left, rect.bottom );
      SelectObject( hDC, pen );
      LineTo( hDC, rect.left, rect.top );
   }
   else
   {
      LineTo( hDC, rect.right, rect.top );
      LineTo( hDC, rect.right, rect.bottom );
      LineTo( hDC, rect.left, rect.bottom );
      LineTo( hDC, rect.left, rect.top );
   }

   SelectObject( hDC, oldPen );
   DeleteObject( pen );
   DeleteObject( pen1 );
}

VOID DrawCheck( HDC hdc, SIZE size, RECT rect, BOOL disabled, BOOL selected, HBITMAP hbitmap )
{
   if( hbitmap != 0 )
   {
      DrawGlyph
      (
         hdc,
         rect.left + cx_delta / 2,
         rect.top + cy_delta / 2 + 2,
         size.cx,
         size.cy,
         hbitmap,
         RGB( 125, 125, 125 ),
         ( ( disabled ) ? TRUE : FALSE ),
         TRUE
      );
   }
   else
   {
      HPEN     pen, oldPen;
      HBRUSH   brush, oldBrush;
      UINT     x, y, w, h;

      if( ( selected ) && ( eMenuCursorType != Short ) )
      {
         brush = CreateSolidBrush( clrSelectedBk1 );
      }
      else
      {
         brush = CreateSolidBrush( clrCheckMarkBk );
      }

      oldBrush = ( HBRUSH ) SelectObject( hdc, brush );

      pen = CreatePen( PS_SOLID, 1, clrCheckMarkSq );
      oldPen = ( HPEN ) SelectObject( hdc, pen );

      w = ( size.cx > min_width ? min_width : size.cx );
      h = w;
      x = rect.left + ( min_width - w ) / 2;
      y = rect.top + ( min_height + cy_delta - h ) / 2;

      Rectangle( hdc, x, y, x + w, y + h );

      DeleteObject( pen );

      if( disabled )
      {
         pen = CreatePen( PS_SOLID, 1, clrCheckMarkGr );
      }
      else
      {
         pen = CreatePen( PS_SOLID, 1, clrCheckMark );
      }

      SelectObject( hdc, pen );

      MoveToEx( hdc, x + 1, y + 5, NULL );
      LineTo( hdc, x + 4, y + h - 2 );
      MoveToEx( hdc, x + 2, y + 5, NULL );
      LineTo( hdc, x + 4, y + h - 3 );
      MoveToEx( hdc, x + 2, y + 4, NULL );
      LineTo( hdc, x + 5, y + h - 3 );
      MoveToEx( hdc, x + 4, y + h - 3, NULL );
      LineTo( hdc, x + w + 2, y - 1 );
      MoveToEx( hdc, x + 4, y + h - 2, NULL );
      LineTo( hdc, x + w - 2, y + 3 );

      SelectObject( hdc, oldPen );
      SelectObject( hdc, oldBrush );

      DeleteObject( pen );
      DeleteObject( brush );
   }
}

/*
 * Misc
 */
HB_FUNC( SETMENUBITMAPHEIGHT )
{
   bm_size = hb_parni( 1 );
   min_height = min_width = bm_size + 4;
   hb_retni( bm_size );
}

HB_FUNC( GETMENUBITMAPHEIGHT )
{
   hb_retni( bm_size );
}

HB_FUNC( SETMENUSEPARATORTYPE )
{
   eSeparatorType = ( SEPARATORTYPE ) hb_parni( 1 );
   eSeparatorPosition = ( SEPARATORPOSITION ) hb_parni( 2 );
}

HB_FUNC( SETMENUSELECTEDITEM3D )
{
   bSelectedItemBorder3d = ( BOOL ) hb_parl( 1 );
}

HB_FUNC( SETMENUCURSORTYPE )
{
   eMenuCursorType = ( MENUCURSORTYPE ) hb_parni( 1 );
}

/*
 * Color Management of HMG menus
 */
#ifndef __WINNT__
VOID SetMenuBarColor( HMENU hMenu, COLORREF clrBk, BOOL fSubMenu )
{
   MENUINFO MenuInfo;

   MenuInfo.cbSize = sizeof( MENUINFO );
   GetMenuInfo( hMenu, &MenuInfo );

   MenuInfo.fMask = MIM_BACKGROUND;
   if( fSubMenu )
   {
      MenuInfo.fMask |= MIM_APPLYTOSUBMENUS;
   }

   MenuInfo.hbrBack = CreateSolidBrush( clrBk );
   SetMenuInfo( hMenu, &MenuInfo );
}
#endif
static BOOL IsColorEqual( COLORREF clr1, COLORREF clr2 )
{
   return
      (
         ( GetRValue( clr1 ) == GetRValue( clr2 ) )
      && ( GetGValue( clr1 ) == GetGValue( clr2 ) )
      && ( GetBValue( clr1 ) == GetBValue( clr2 ) )
      ) ? TRUE : FALSE;
}

HB_FUNC( GETMENUCOLORS )
{
   PHB_ITEM aResult = hb_itemArrayNew( 28 );

   HB_arraySetNL( aResult, 1, clrMenuBar1 );
   HB_arraySetNL( aResult, 2, clrMenuBar2 );
   HB_arraySetNL( aResult, 3, clrMenuBarText );
   HB_arraySetNL( aResult, 4, clrMenuBarSelectedText );
   HB_arraySetNL( aResult, 5, clrMenuBarGrayedText );
   HB_arraySetNL( aResult, 6, clrSelectedMenuBarItem1 );
   HB_arraySetNL( aResult, 7, clrSelectedMenuBarItem2 );
   HB_arraySetNL( aResult, 8, clrText1 );
   HB_arraySetNL( aResult, 9, clrSelectedText1 );
   HB_arraySetNL( aResult, 10, clrGrayedText1 );
   HB_arraySetNL( aResult, 11, clrBk1 );
   HB_arraySetNL( aResult, 12, clrBk2 );
   HB_arraySetNL( aResult, 13, clrSelectedBk1 );
   HB_arraySetNL( aResult, 14, clrSelectedBk2 );
   HB_arraySetNL( aResult, 15, clrGrayedBk1 );
   HB_arraySetNL( aResult, 16, clrGrayedBk2 );
   HB_arraySetNL( aResult, 17, clrImageBk1 );
   HB_arraySetNL( aResult, 18, clrImageBk2 );
   HB_arraySetNL( aResult, 19, clrSeparator1 );
   HB_arraySetNL( aResult, 20, clrSeparator2 );
   HB_arraySetNL( aResult, 21, clrSelectedItemBorder1 );
   HB_arraySetNL( aResult, 22, clrSelectedItemBorder2 );
   HB_arraySetNL( aResult, 23, clrSelectedItemBorder3 );
   HB_arraySetNL( aResult, 24, clrSelectedItemBorder4 );
   HB_arraySetNL( aResult, 25, clrCheckMark );
   HB_arraySetNL( aResult, 26, clrCheckMarkBk );
   HB_arraySetNL( aResult, 27, clrCheckMarkSq );
   HB_arraySetNL( aResult, 28, clrCheckMarkGr );

   hb_itemReturnRelease( aResult );
}

HB_FUNC( SETMENUCOLORS )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( ( pArray != NULL ) && ( hb_arrayLen( pArray ) >= 28 ) )
   {
      clrMenuBar1 = hmg_parv_COLORREF( 1, 1 );
      clrMenuBar2 = hmg_parv_COLORREF( 1, 2 );
      clrMenuBarText = hmg_parv_COLORREF( 1, 3 );
      clrMenuBarSelectedText = hmg_parv_COLORREF( 1, 4 );
      clrMenuBarGrayedText = hmg_parv_COLORREF( 1, 5 );
      clrSelectedMenuBarItem1 = hmg_parv_COLORREF( 1, 6 );
      clrSelectedMenuBarItem2 = hmg_parv_COLORREF( 1, 7 );
      clrText1 = hmg_parv_COLORREF( 1, 8 );
      clrSelectedText1 = hmg_parv_COLORREF( 1, 9 );
      clrGrayedText1 = hmg_parv_COLORREF( 1, 10 );
      clrBk1 = hmg_parv_COLORREF( 1, 11 );
      clrBk2 = hmg_parv_COLORREF( 1, 12 );
      clrSelectedBk1 = hmg_parv_COLORREF( 1, 13 );
      clrSelectedBk2 = hmg_parv_COLORREF( 1, 14 );
      clrGrayedBk1 = hmg_parv_COLORREF( 1, 15 );
      clrGrayedBk2 = hmg_parv_COLORREF( 1, 16 );
      clrImageBk1 = hmg_parv_COLORREF( 1, 17 );
      clrImageBk2 = hmg_parv_COLORREF( 1, 18 );
      clrSeparator1 = hmg_parv_COLORREF( 1, 19 );
      clrSeparator2 = hmg_parv_COLORREF( 1, 20 );
      clrSelectedItemBorder1 = hmg_parv_COLORREF( 1, 21 );
      clrSelectedItemBorder2 = hmg_parv_COLORREF( 1, 22 );
      clrSelectedItemBorder3 = hmg_parv_COLORREF( 1, 23 );
      clrSelectedItemBorder4 = hmg_parv_COLORREF( 1, 24 );
      clrCheckMark = hmg_parv_COLORREF( 1, 25 );
      clrCheckMarkBk = hmg_parv_COLORREF( 1, 26 );
      clrCheckMarkSq = hmg_parv_COLORREF( 1, 27 );
      clrCheckMarkGr = hmg_parv_COLORREF( 1, 28 );
   }
}

/*
 * Call this funtions on WM_DESTROY, WM_MEASUREITEM of menu owner window
 */
HB_FUNC( _ONDESTROYMENU )
{
   HMENU menu = hmg_par_raw_HMENU( 1 );

   if( IsMenu( menu ) )
   {
      HB_BOOL  bResult = _DestroyMenu( menu );

#ifdef _ERRORMSG_
      if( !bResult )
      {
         MessageBox( NULL, "Menu is not destroyed successfully", "Warning", MB_OK | MB_ICONWARNING );
      }
#endif
      if( hb_pcount() > 1 && hb_parl( 2 ) )
      {
         bResult = bResult && DestroyMenu( menu );
      }

      hb_retl( bResult );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
   }
}

static BOOL _DestroyMenu( HMENU menu )
{
   int   i;
   BOOL  bResult = TRUE;

   for( i = 0; i < GetMenuItemCount( menu ); i++ )
   {
      MENUITEMINFO   MenuItemInfo;
      HMENU          pSubMenu;

      MenuItemInfo.cbSize = sizeof( MENUITEMINFO );
      MenuItemInfo.fMask = MIIM_CHECKMARKS | MIIM_DATA;

      GetMenuItemInfo( menu, i, TRUE, &MenuItemInfo );

      if( MenuItemInfo.hbmpChecked != NULL )
      {
         bResult = DeleteObject( MenuItemInfo.hbmpChecked );
         MenuItemInfo.hbmpChecked = NULL;
      }

      if( MenuItemInfo.hbmpUnchecked != NULL )
      {
         bResult = bResult && DeleteObject( MenuItemInfo.hbmpUnchecked );
         MenuItemInfo.hbmpUnchecked = NULL;
      }

      if( s_bCustomDraw )
      {
         LPMENUITEM  lpMenuItem;
         lpMenuItem = ( LPMENUITEM ) MenuItemInfo.dwItemData;

         if( lpMenuItem->caption != NULL )
         {
            hb_xfree( lpMenuItem->caption );
            lpMenuItem->caption = NULL;
         }

         if( lpMenuItem->hBitmap != NULL )
         {
            bResult = bResult && DeleteObject( lpMenuItem->hBitmap );
            lpMenuItem->hBitmap = NULL;
         }

         if( GetObjectType( ( HGDIOBJ ) lpMenuItem->hFont ) == OBJ_FONT )
         {
            bResult = bResult && DeleteObject( lpMenuItem->hFont );
            lpMenuItem->hFont = NULL;
         }

         hb_xfree( lpMenuItem );
      }

      pSubMenu = GetSubMenu( menu, i );

      if( pSubMenu != NULL )
      {
         bResult = bResult && _DestroyMenu( pSubMenu );
      }
   }

   return bResult;
}

HB_FUNC( _ONMEASUREMENUITEM )
{
   HWND  hwnd = hmg_par_raw_HWND( 1 );

   if( IsWindow( hwnd ) )
   {
      HDC                  hdc = GetDC( hwnd );
      LPMEASUREITEMSTRUCT  lpmis = hmg_par_raw_MITEMSTRUCT( 4 );
      MENUITEM             *lpMenuItem = ( MENUITEM * ) lpmis->itemData;
      SIZE                 size = { 0, 0 };
      HFONT                oldfont;

      if( GetObjectType( ( HGDIOBJ ) lpMenuItem->hFont ) == OBJ_FONT )
      {
         oldfont = ( HFONT ) SelectObject( hdc, lpMenuItem->hFont );
      }
      else
      {
         oldfont = ( HFONT ) SelectObject( hdc, GetStockObject( DEFAULT_GUI_FONT ) );
      }

      if( lpMenuItem->uiItemType == 1000 )
      {
         lpmis->itemHeight = 2 * cy_delta;
         lpmis->itemWidth = 0;
      }
      else
      {
         GetTextExtentPoint32( hdc, lpMenuItem->caption, lpMenuItem->cch, &size );
      }

      if( lpMenuItem->uiItemType == 1 )
      {
         lpmis->itemWidth = size.cx;
      }
      else if( lpmis->itemID > 0 )
      {
         lpmis->itemWidth = min_width + cx_delta + size.cx + 8;
      }

      if( lpMenuItem->uiItemType != 1000 )
      {
         lpmis->itemHeight = ( size.cy > min_height ? size.cy : min_height );
         lpmis->itemHeight += cy_delta;
      }

      SelectObject( hdc, oldfont );

      ReleaseDC( hwnd, hdc );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
   }
}

HB_FUNC( _COLORMENU )
{
   HMENU    iMenu;
   MENUINFO iMenuInfo;
   HWND     hWnd = hmg_par_raw_HWND( 1 );

   INT      nRed = HB_PARNI( 2, 1 );
   INT      nGreen = HB_PARNI( 2, 2 );
   INT      nBlue = HB_PARNI( 2, 3 );
   BOOL     lSubMenu = HB_ISLOG( 3 ) ? hb_parl( 3 ) : FALSE;

   iMenu = GetMenu( hWnd );
   GetMenuInfo( iMenu, &iMenuInfo );
   iMenuInfo.cbSize = sizeof( MENUINFO );
   if( lSubMenu )
   {
      iMenuInfo.fMask = MIM_BACKGROUND | MIM_APPLYTOSUBMENUS;
   }
   else
   {
      iMenuInfo.fMask = MIM_BACKGROUND;
   }

   iMenuInfo.hbrBack = CreateSolidBrush( RGB( nRed, nGreen, nBlue ) );

   SetMenuInfo( iMenu, &iMenuInfo );

   DrawMenuBar( ( HWND ) hWnd );
}
