/*----------------------------------------------------------------------------
   MINIGUI - Harbour Win32 GUI library source code

   Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
   http://harbourminigui.googlepages.com/

   This program is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free Software
   Foundation; either version 2 of the License, or (at your option) any later
   version.

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along with
   this software; see the file COPYING. If not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA (or
   visit the web site http://www.gnu.org/).

   As a special exception, you have permission for additional uses of the text
   contained in this release of Harbour Minigui.

   The exception is that, if you link the Harbour Minigui library with other
   files to produce an executable, this does not by itself cause the resulting
   executable to be covered by the GNU General Public License.
   Your use of that executable is in no way restricted on account of linking the
   Harbour-Minigui library code into it.

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

 ---------------------------------------------------------------------------*/
#define _WIN32_IE 0x0501

#include <mgdefs.h>
#include <commctrl.h>

#include "hbapiitm.h"
#include "hbapierr.h"

#ifdef __XHARBOUR__
typedef wchar_t   HB_WCHAR;
#else
#include "hbapicdp.h"
#endif
extern BOOL       _isValidCtrlClass( HWND, LPCTSTR );

HIMAGELIST        HMG_ImageListLoadFirst( const char *FileName, int cGrow, int Transparent, int *nWidth, int *nHeight );
void              HMG_ImageListAdd( HIMAGELIST himl, char *FileName, int Transparent );

#ifdef UNICODE
LPWSTR            AnsiToWide( LPCSTR );
LPSTR             WideToAnsi( LPWSTR );
#endif
HINSTANCE         GetInstance( void );
HINSTANCE         GetResources( void );

// Minigui Resources control system
void              RegisterResource( HANDLE hResource, LPCSTR szType );

#if ( ( defined( __BORLANDC__ ) && __BORLANDC__ < 1410 ) )
typedef struct tagLVITEMA2
{
   UINT     mask;
   int      iItem;
   int      iSubItem;
   UINT     state;
   UINT     stateMask;
   LPSTR    pszText;
   int      cchTextMax;
   int      iImage;
   LPARAM   lParam;
   int      iIndent;
#if ( NTDDI_VERSION >= NTDDI_WINXP )
   int      iGroupId;
   UINT     cColumns;                        // tile view columns
   PUINT    puColumns;
#endif
#if ( NTDDI_VERSION >= NTDDI_VISTA )         // Will be unused downlevel, but sizeof(LVITEMA) must be equal to sizeof(LVITEMW)
   int      *piColFmt;
   int      iGroup;                          // readonly. only valid for owner data.
#endif
} LVITEMA2, *LPLVITEMA2;

typedef struct tagLVITEMW2
{
   UINT     mask;
   int      iItem;
   int      iSubItem;
   UINT     state;
   UINT     stateMask;
   LPWSTR   pszText;
   int      cchTextMax;
   int      iImage;
   LPARAM   lParam;
   int      iIndent;
#if ( NTDDI_VERSION >= NTDDI_WINXP )
   int      iGroupId;
   UINT     cColumns;                        // tile view columns
   PUINT    puColumns;
#endif
#if ( NTDDI_VERSION >= NTDDI_VISTA )
   int      *piColFmt;
   int      iGroup;                          // readonly. only valid for owner data.
#endif
} LVITEMW2, *LPLVITEMW2;

#ifdef UNICODE
#define _LVITEM   LVITEMW2
#define _LPLVITEM LPLVITEMW2
#else
#define _LVITEM   LVITEMA2
#define _LPLVITEM LPLVITEMA2
#endif
#define LVGF_NONE                0x00000000
#define LVGF_HEADER              0x00000001
#define LVGF_FOOTER              0x00000002
#define LVGF_STATE               0x00000004
#define LVGF_ALIGN               0x00000008
#define LVGF_GROUPID             0x00000010

#define LVGS_NORMAL              0x00000000
#define LVGS_COLLAPSED           0x00000001
#define LVGS_HIDDEN              0x00000002
#define LVGS_NOHEADER            0x00000004
#define LVGS_COLLAPSIBLE         0x00000008
#define LVGS_FOCUSED             0x00000010
#define LVGS_SELECTED            0x00000020
#define LVGS_SUBSETED            0x00000040
#define LVGS_SUBSETLINKFOCUSED   0x00000080

#define LVGA_HEADER_LEFT         0x00000001
#define LVGA_HEADER_CENTER       0x00000002
#define LVGA_HEADER_RIGHT        0x00000004  // Don't forget to validate exclusivity
#define LVGA_FOOTER_LEFT         0x00000008
#define LVGA_FOOTER_CENTER       0x00000010
#define LVGA_FOOTER_RIGHT        0x00000020  // Don't forget to validate exclusivity
#define LVIF_GROUPID             0x100
#define LVIF_COLUMNS             0x200

typedef struct tagLVGROUP
{
   UINT     cbSize;
   UINT     mask;
   LPWSTR   pszHeader;
   int      cchHeader;
   LPWSTR   pszFooter;
   int      cchFooter;
   int      iGroupId;
   UINT     stateMask;
   UINT     state;
   UINT     uAlign;
} LVGROUP, *PLVGROUP;

#define LVM_ENABLEGROUPVIEW                           ( LVM_FIRST + 157 )
#define ListView_EnableGroupView( hwnd, fEnable )     SNDMSG( ( hwnd ), LVM_ENABLEGROUPVIEW, ( WPARAM ) ( fEnable ), 0 )
#define LVM_REMOVEALLGROUPS                           ( LVM_FIRST + 160 )
#define ListView_RemoveAllGroups( hwnd )              SNDMSG( ( hwnd ), LVM_REMOVEALLGROUPS, 0, 0 )
#define LVM_HASGROUP                                  ( LVM_FIRST + 161 )
#define ListView_HasGroup( hwnd, dwGroupId )          SNDMSG( ( hwnd ), LVM_HASGROUP, dwGroupId, 0 )
#define LVM_ISGROUPVIEWENABLED                        ( LVM_FIRST + 175 )
#define ListView_IsGroupViewEnabled( hwnd )           ( BOOL ) SNDMSG( ( hwnd ), LVM_ISGROUPVIEWENABLED, 0, 0 )
#define LVM_INSERTGROUP                               ( LVM_FIRST + 145 )
#define ListView_InsertGroup( hwnd, index, pgrp )     SNDMSG( ( hwnd ), LVM_INSERTGROUP, ( WPARAM ) index, ( LPARAM ) pgrp )
#define LVM_SETGROUPINFO                              ( LVM_FIRST + 147 )
#define ListView_SetGroupInfo( hwnd, iGroupId, pgrp ) SNDMSG( ( hwnd ), LVM_SETGROUPINFO, ( WPARAM ) iGroupId, ( LPARAM ) pgrp )
#define LVM_GETGROUPINFO                              ( LVM_FIRST + 149 )
#define ListView_GetGroupInfo( hwnd, iGroupId, pgrp ) SNDMSG( ( hwnd ), LVM_GETGROUPINFO, ( WPARAM ) iGroupId, ( LPARAM ) pgrp )
#define LVM_REMOVEGROUP                               ( LVM_FIRST + 150 )
#define ListView_RemoveGroup( hwnd, iGroupId )        SNDMSG( ( hwnd ), LVM_REMOVEGROUP, ( WPARAM ) iGroupId, 0 )
#define LVM_MOVEGROUP                                 ( LVM_FIRST + 151 )
#define ListView_MoveGroup( hwnd, iGroupId, toIndex ) SNDMSG( ( hwnd ), LVM_MOVEGROUP, ( WPARAM ) iGroupId, ( LPARAM ) toIndex )
#define LVM_GETGROUPCOUNT                             ( LVM_FIRST + 152 )
#define ListView_GetGroupCount( hwnd )                SNDMSG( ( hwnd ), LVM_GETGROUPCOUNT, ( WPARAM ) 0, ( LPARAM ) 0 )
#endif

/*
 * FUNCTION: INITLISTVIEW
 *
 * Initializes a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the parent window.
 *   hMenu: Handle to the menu.
 *   x: The x-coordinate of the upper-left corner of the list view control.
 *   y: The y-coordinate of the upper-left corner of the list view control.
 *   w: The width of the list view control.
 *   h: The height of the list view control.
 *   MultiSelect: If .T., the list view control allows multiple selections.
 *   TabStop: If .T., the list view control is included in the tab order.
 *   NoHeader: If .T., the list view control does not have a header.
 *   NoSortHeader: If .T., the list view control does not sort when a header is clicked.
 *   OwnerData: If .T., the list view control uses owner data.
 *   ItemCount: The number of items in the list view control.
 *
 * Returns:
 *   Handle to the created list view control.
 *
 * Purpose:
 *   This function initializes a list view control with the specified properties.
 *   It is used to create a list view control within a window.
 *
 * Notes:
 *   The function checks for various conditions and ensures that the list view control is properly initialized.
 *   It also handles the registration of the list view control class and the creation of the list view control handle.
 */
HB_FUNC( INITLISTVIEW )
{
   HWND                 hbutton;
   DWORD                style;

   INITCOMMONCONTROLSEX i;

   i.dwSize = sizeof( INITCOMMONCONTROLSEX );
   i.dwICC = ICC_LISTVIEW_CLASSES;
   InitCommonControlsEx( &i );

   style = LVS_SHOWSELALWAYS | WS_CHILD | WS_VISIBLE | LVS_REPORT;

   if( !hb_parl( 9 ) )
   {
      style |= LVS_SINGLESEL;
   }

   if( !hb_parl( 12 ) )
   {
      style |= WS_TABSTOP;
   }

   if( !hb_parl( 10 ) )
   {
      style |= LVS_NOCOLUMNHEADER;
   }
   else if( hb_parl( 11 ) )
   {
      style |= LVS_NOSORTHEADER;
   }

   if( hb_parl( 7 ) )
   {
      style |= LVS_OWNERDATA;
   }

   hbutton = CreateWindowEx
      (
         WS_EX_CLIENTEDGE,
         WC_LISTVIEW,
         TEXT( "" ),
         style,
         hb_parni( 3 ),
         hb_parni( 4 ),
         hb_parni( 5 ),
         hb_parni( 6 ),
         hmg_par_raw_HWND( 1 ),
         hmg_par_raw_HMENU( 2 ),
         GetInstance(),
         NULL
      );

   if( hb_parl( 7 ) )
   {
      ListView_SetItemCount( hbutton, hb_parni( 8 ) );
   }

   hmg_ret_raw_HWND( hbutton );
}

/*
 * FUNCTION: LISTVIEW_SETITEMCOUNT
 *
 * Sets the number of items in a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   ItemCount: The number of items in the list view control.
 *
 * Returns:
 *   NIL
 *
 * Purpose:
 *   This function sets the number of items in a list view control.
 *   It is used to update the item count in a list view control.
 */
HB_FUNC( LISTVIEW_SETITEMCOUNT )
{
   ListView_SetItemCount( hmg_par_raw_HWND( 1 ), hb_parni( 2 ) );
}

/*
 * FUNCTION: ADDLISTVIEWBITMAP
 *
 * Adds a bitmap to a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   aBitmaps: An array of bitmap file names.
 *
 * Returns:
 *   The width of the bitmap.
 *
 * Purpose:
 *   This function adds a bitmap to a list view control, which is used to display icons in the list view.
 *   It is used to enhance the visual appearance of the list view control.
 *
 * Notes:
 *   The function checks for various conditions and ensures that the bitmap is properly added to the list view control.
 */
HB_FUNC( ADDLISTVIEWBITMAP )                 // Grid+
{
   HIMAGELIST  himl = ( HIMAGELIST ) NULL;
   PHB_ITEM    hArray;
   char        *FileName;
   int         nCount;
   int         s;
   int         cx = -1;
   int         cy = -1;

   nCount = ( int ) hb_parinfa( 2, 0 );

   if( nCount > 0 )
   {
      hArray = hb_param( 2, HB_IT_ARRAY );

      for( s = 1; s <= nCount; s++ )
      {
         FileName = ( char * ) hb_arrayGetCPtr( hArray, s );

         if( himl == NULL )
         {
            himl = HMG_ImageListLoadFirst( FileName, nCount, 1, &cx, &cy );
         }
         else
         {
            HMG_ImageListAdd( himl, FileName, 1 );
         }
      }

      if( himl != NULL )
      {
         SendMessage( hmg_par_raw_HWND( 1 ), LVM_SETIMAGELIST, ( WPARAM ) LVSIL_SMALL, ( LPARAM ) himl );
      }
   }

   hb_retni( cx );
}

/*
 * FUNCTION: ADDLISTVIEWBITMAPHEADER
 *
 * Adds a bitmap to the header of a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   aBitmaps: An array of bitmap file names.
 *
 * Returns:
 *   Handle to the image list.
 *
 * Purpose:
 *   This function adds a bitmap to the header of a list view control, which is used to display icons in the header.
 *   It is used to enhance the visual appearance of the list view control header.
 *
 * Notes:
 *   The function checks for various conditions and ensures that the bitmap is properly added to the list view control header.
 */
HB_FUNC( ADDLISTVIEWBITMAPHEADER )           // Grid+
{
   HWND        hheader;
   HIMAGELIST  himl = ( HIMAGELIST ) NULL;
   PHB_ITEM    hArray;
   char        *FileName;
   int         nCount;
   int         s;
   int         cx = -1;
   int         cy = -1;

   hheader = ListView_GetHeader( hmg_par_raw_HWND( 1 ) );

   if( hheader )
   {
      nCount = ( int ) hb_parinfa( 2, 0 );

      if( nCount > 0 )
      {
         hArray = hb_param( 2, HB_IT_ARRAY );

         for( s = 1; s <= nCount; s++ )
         {
            FileName = ( char * ) hb_arrayGetCPtr( hArray, s );

            if( himl == NULL )
            {
               himl = HMG_ImageListLoadFirst( FileName, nCount, 1, &cx, &cy );
            }
            else
            {
               HMG_ImageListAdd( himl, FileName, 1 );
            }
         }

         if( himl != NULL )
         {
            SendMessage( hheader, HDM_SETIMAGELIST, 0, ( LPARAM ) himl );
            RegisterResource( himl, "IMAGELIST" );
         }
      }
   }

   hmg_ret_raw_HANDLE( himl );
}

/*
 * FUNCTION: LISTVIEW_GETFOCUSEDITEM
 *
 * Retrieves the index of the focused item in a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *
 * Returns:
 *   The index of the focused item.
 *
 * Purpose:
 *   This function retrieves the index of the focused item in a list view control.
 *   It is used to determine which item currently has focus.
 */
HB_FUNC( LISTVIEW_GETFOCUSEDITEM )
{
   hb_retni( ListView_GetNextItem( hmg_par_raw_HWND( 1 ), -1, LVNI_ALL | LVNI_FOCUSED ) + 1 );
}

/*
 * FUNCTION: LISTVIEW_GETFIRSTITEM
 *
 * Retrieves the index of the first selected item in a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *
 * Returns:
 *   The index of the first selected item.
 *
 * Purpose:
 *   This function retrieves the index of the first selected item in a list view control.
 *   It is used to determine which item is currently selected.
 */
HB_FUNC( LISTVIEW_GETFIRSTITEM )
{
   hb_retni( ListView_GetNextItem( hmg_par_raw_HWND( 1 ), -1, LVNI_ALL | LVNI_SELECTED ) + 1 );
}

/*
 * FUNCTION: INITLISTVIEWCOLUMNS
 *
 * Initializes the columns of a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   aColumns: An array of column captions.
 *   aWidths: An array of column widths.
 *   aJustify: An array of column justification values.
 *
 * Returns:
 *   NIL
 *
 * Purpose:
 *   This function initializes the columns of a list view control with the specified captions, widths, and justification values.
 *   It is used to set up the columns in a list view control.
 *
 * Notes:
 *   The function checks for various conditions and ensures that the columns are properly initialized.
 */
HB_FUNC( INITLISTVIEWCOLUMNS )               // code INITLISTVIEWCOLUMNS function was borrowed from ooHG
{
   PHB_ITEM    wArray;
   PHB_ITEM    hArray;
   PHB_ITEM    jArray;

   HWND        hc;

#ifndef UNICODE
   LPSTR       lpText;
#else
   LPWSTR      lpText;
#endif
   LV_COLUMN   COL;
   int         iLen;
   int         s;
   int         iColumn = 0;

   hc = hmg_par_raw_HWND( 1 );

   iLen = ( int ) hb_parinfa( 2, 0 ) - 1;
   hArray = hb_param( 2, HB_IT_ARRAY );
   wArray = hb_param( 3, HB_IT_ARRAY );
   jArray = hb_param( 4, HB_IT_ARRAY );

   COL.mask = LVCF_FMT | LVCF_WIDTH | LVCF_TEXT | LVCF_SUBITEM;

   for( s = 0; s <= iLen; s++ )
   {
      COL.fmt = hb_arrayGetNI( jArray, s + 1 );
      COL.cx = hb_arrayGetNI( wArray, s + 1 );
#ifndef UNICODE
      lpText = ( char * ) hb_arrayGetCPtr( hArray, s + 1 );
#else
      lpText = AnsiToWide( ( char * ) hb_arrayGetCPtr( hArray, s + 1 ) );
#endif
      COL.pszText = lpText;
      COL.iSubItem = iColumn;
      ListView_InsertColumn( hc, iColumn, &COL );
      if( iColumn == 0 && COL.fmt != LVCFMT_LEFT )
      {
         iColumn++;
         COL.iSubItem = iColumn;
         ListView_InsertColumn( hc, iColumn, &COL );
      }

      iColumn++;

#ifdef UNICODE
      hb_xfree( lpText );
#endif
   }

   if( iColumn != s )
   {
      ListView_DeleteColumn( hc, 0 );
   }
}

/*
 * FUNCTION: ADDLISTVIEWITEMS
 *
 * Adds items to a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   aItems: An array of item captions.
 *   iImage: The index of the image to associate with the items.
 *   nRow: The row index to insert the items.
 *
 * Returns:
 *   NIL
 *
 * Purpose:
 *   This function adds items to a list view control with the specified captions and image index.
 *   It is used to populate the list view control with data.
 *
 * Notes:
 *   The function checks for various conditions and ensures that the items are properly added to the list view control.
 */
HB_FUNC( ADDLISTVIEWITEMS )                  // AddListViewItems( hWnd, aItem, iImage, [nRow] )
{
   PHB_ITEM hArray;
   char     *caption;

#ifndef UNICODE
   LPSTR    lpText;
#else
   LPWSTR   lpText;
#endif
   LV_ITEM  LI;
   HWND     h;
   int      l;          // ColumnCount
   int      s;          // Col
   int      c;          // Row
   h = hmg_par_raw_HWND( 1 );
   l = ( int ) hb_parinfa( 2, 0 ) - 1;
   hArray = hb_param( 2, HB_IT_ARRAY );
   c = HB_ISNIL( 4 ) ? ListView_GetItemCount( h ) : hb_parni( 4 );

   caption = ( char * ) hb_arrayGetCPtr( hArray, 1 );
#ifndef UNICODE
   lpText = caption;
#else
   lpText = AnsiToWide( ( char * ) caption );
#endif
   LI.mask = LVIF_TEXT | LVIF_IMAGE;
   LI.state = 0;
   LI.stateMask = 0;
   LI.iImage = hb_parni( 3 );
   LI.iSubItem = 0;
   LI.iItem = c;
   LI.pszText = lpText;

   ListView_InsertItem( h, &LI );

   for( s = 1; s <= l; s = s + 1 )
   {
      caption = ( char * ) hb_arrayGetCPtr( hArray, s + 1 );
#ifndef UNICODE
      lpText = caption;
#else
      lpText = AnsiToWide( ( char * ) caption );
#endif
      ListView_SetItemText( h, c, s, lpText );
#ifdef UNICODE
      hb_xfree( lpText );
#endif
   }
}

/*
 * FUNCTION: LISTVIEW_SETCURSEL
 *
 * Sets the current selection in a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   Item: The index of the item to select.
 *
 * Returns:
 *   NIL
 *
 * Purpose:
 *   This function sets the current selection in a list view control to the specified item.
 *   It is used to change the selected item in the list view control.
 */
HB_FUNC( LISTVIEW_SETCURSEL )
{
   ListView_SetItemState( hmg_par_raw_HWND( 1 ), ( WPARAM ) hb_parni( 2 ) - 1, LVIS_FOCUSED | LVIS_SELECTED, LVIS_FOCUSED | LVIS_SELECTED );
}

/*
 * FUNCTION: LISTVIEWGETMULTISEL
 *
 * Retrieves the indices of the selected items in a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *
 * Returns:
 *   An array of the indices of the selected items.
 *
 * Purpose:
 *   This function retrieves the indices of the selected items in a list view control.
 *   It is used to determine which items are currently selected.
 */
HB_FUNC( LISTVIEWGETMULTISEL )
{
   HWND  hwnd = hmg_par_raw_HWND( 1 );
   int   i = -1;
   int   n;
   int   j = 0;

   n = ( int ) SendMessage( hwnd, LVM_GETSELECTEDCOUNT, 0, 0 );

   hb_reta( n );

   while( 1 )
   {
      i = ListView_GetNextItem( hwnd, i, LVNI_ALL | LVNI_SELECTED );

      if( i == -1 )
      {
         break;
      }
      else
      {
         j++;
      }

      HB_STORNI( i + 1, -1, j );
   }
}

/*
 * FUNCTION: LISTVIEWSETMULTISEL
 *
 * Sets the selected items in a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   aItems: An array of item indices to select.
 *
 * Returns:
 *   NIL
 *
 * Purpose:
 *   This function sets the selected items in a list view control to the specified item indices.
 *   It is used to change the selected items in the list view control.
 */
HB_FUNC( LISTVIEWSETMULTISEL )
{
   PHB_ITEM wArray;
   HWND     hwnd = hmg_par_raw_HWND( 1 );
   int      i = -1;
   int      l;

   wArray = hb_param( 2, HB_IT_ARRAY );

   l = ( int ) hb_parinfa( 2, 0 ) - 1;

   // Clear current selections
   while( 1 )
   {
      i = ListView_GetNextItem( hwnd, i, LVNI_ALL | LVNI_SELECTED );

      if( i == -1 )
      {
         break;
      }
      else
      {
         ListView_SetItemState( hwnd, ( WPARAM ) i, 0, LVIS_FOCUSED | LVIS_SELECTED );
      }
   }

   // Set new selections
   for( i = 0; i <= l; i++ )
   {
      ListView_SetItemState( hwnd, hb_arrayGetNI( wArray, i + 1 ) - 1, LVIS_FOCUSED | LVIS_SELECTED, LVIS_FOCUSED | LVIS_SELECTED );
   }
}

/*
 * FUNCTION: LISTVIEWSETITEM
 *
 * Sets the text of an item in a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   aTexts: An array of text strings for the item.
 *   Row: The row index of the item.
 *
 * Returns:
 *   NIL
 *
 * Purpose:
 *   This function sets the text of an item in a list view control to the specified text strings.
 *   It is used to update the text of an item in the list view control.
 */
HB_FUNC( LISTVIEWSETITEM )
{
   PHB_ITEM hArray;
   char     *caption;

#ifndef UNICODE
   LPSTR    lpText;
#else
   LPWSTR   lpText;
#endif
   HWND     h = hmg_par_raw_HWND( 1 );
   int      l = ( int ) hb_parinfa( 2, 0 ) - 1;
   int      c = hb_parni( 3 ) - 1;
   int      s;

   hArray = hb_param( 2, HB_IT_ARRAY );

   for( s = 0; s <= l; s = s + 1 )
   {
      caption = ( char * ) hb_arrayGetCPtr( hArray, s + 1 );
#ifndef UNICODE
      lpText = caption;
#else
      lpText = AnsiToWide( caption );
#endif
      ListView_SetItemText( h, c, s, lpText );

#ifdef UNICODE
      hb_xfree( lpText );
#endif
   }
}

/*
 * FUNCTION: GetLVItemText
 *
 * Retrieves the text of an item in a list view control.
 *
 * Parameters:
 *   hListView: Handle to the list view control.
 *   i: The index of the item.
 *   iSubItem_: The index of the subitem.
 *
 * Returns:
 *   The text of the item.
 *
 * Purpose:
 *   This function retrieves the text of an item in a list view control.
 *   It is used to get the text of an item for display or processing.
 */
static TCHAR *GetLVItemText( HWND hListView, int i, int iSubItem_ )
{
   TCHAR    *lpText = NULL;
   int      nLen = 64;
   int      nRes;
   LV_ITEM  lvi;

   lvi.iSubItem = iSubItem_;

   do
   {
      nLen *= 2;
      lpText = ( TCHAR * ) hb_xrealloc( lpText, sizeof( TCHAR ) * nLen );
      lvi.cchTextMax = nLen;
      lvi.pszText = lpText;
      nRes = ( int ) SendMessage( hListView, LVM_GETITEMTEXT, ( WPARAM ) i, ( LPARAM ) ( LV_ITEM FAR * ) &lvi );
   }
   while( nRes >= nLen - 1 );

   return ( TCHAR * ) lpText;
}

/*
 * FUNCTION: LISTVIEWGETITEM
 *
 * Retrieves the text of an item in a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   Row: The row index of the item.
 *   ColCount: The number of columns in the item.
 *
 * Returns:
 *   An array of the text strings of the item.
 *
 * Purpose:
 *   This function retrieves the text of an item in a list view control.
 *   It is used to get the text of an item for display or processing.
 */
HB_FUNC( LISTVIEWGETITEM )
{
#ifdef UNICODE
   LPSTR pStr;
#endif
   HWND  h = hmg_par_raw_HWND( 1 );
   int   c = hb_parni( 2 ) - 1;
   int   l = hb_parni( 3 );
   int   s;
   TCHAR *pszRet;

   hb_reta( l );

   for( s = 0; s <= l - 1; s++ )
   {
      pszRet = GetLVItemText( h, c, s );
#ifndef UNICODE
      HB_STORC( pszRet, -1, s + 1 );
#else
      pStr = WideToAnsi( pszRet );
      HB_STORC( pStr, -1, s + 1 );
      hb_xfree( pStr );
#endif
      hb_xfree( pszRet );
   }
}

/*
 * FUNCTION: LISTVIEWGETITEMROW
 *
 * Retrieves the row index of an item in a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   Item: The index of the item.
 *
 * Returns:
 *   The row index of the item.
 *
 * Purpose:
 *   This function retrieves the row index of an item in a list view control.
 *   It is used to determine the row index of an item.
 */
HB_FUNC( LISTVIEWGETITEMROW )
{
   POINT point;

   ListView_GetItemPosition( hmg_par_raw_HWND( 1 ), hb_parni( 2 ), &point );

   hmg_ret_LONG( point.y );
}

/*
 * FUNCTION: LISTVIEWGETITEMCOUNT
 *
 * Retrieves the number of items in a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *
 * Returns:
 *   The number of items in the list view control.
 *
 * Purpose:
 *   This function retrieves the number of items in a list view control.
 *   It is used to determine the total number of items in the list view control.
 */
HB_FUNC( LISTVIEWGETITEMCOUNT )
{
   hmg_ret_NINT( ListView_GetItemCount( hmg_par_raw_HWND( 1 ) ) );
}

/*
 * FUNCTION: SETGRIDCOLUMNJUSTIFY
 *
 * Sets the justification of a column in a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   Column: The index of the column.
 *   Justify: The justification value for the column.
 *
 * Returns:
 *   NIL
 *
 * Purpose:
 *   This function sets the justification of a column in a list view control.
 *   It is used to align the text in the column.
 */
HB_FUNC( SETGRIDCOLUMNJUSTIFY )
{
   LV_COLUMN   COL;

   COL.mask = LVCF_FMT;
   COL.fmt = hb_parni( 3 );

   ListView_SetColumn( hmg_par_raw_HWND( 1 ), hb_parni( 2 ) - 1, &COL );
}

/*
 * FUNCTION: SETGRIDCOLUMNHEADER
 *
 * Sets the header text and justification of a column in a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   Column: The index of the column.
 *   Text: The text for the column header.
 *   Justify: The justification value for the column header.
 *
 * Returns:
 *   NIL
 *
 * Purpose:
 *   This function sets the header text and justification of a column in a list view control.
 *   It is used to customize the appearance of the column header.
 */
HB_FUNC( SETGRIDCOLUMNHEADER )
{
#ifndef UNICODE
   LPSTR       lpText = ( char * ) hb_parc( 3 );
#else
   LPWSTR      lpText = AnsiToWide( ( char * ) hb_parc( 3 ) );
#endif
   LV_COLUMN   COL;

   COL.mask = LVCF_FMT | LVCF_TEXT;
   COL.pszText = lpText;
   COL.fmt = hb_parni( 4 );

   ListView_SetColumn( hmg_par_raw_HWND( 1 ), hb_parni( 2 ) - 1, &COL );

#ifdef UNICODE
   hb_xfree( lpText );
#endif
}

/*
 * FUNCTION: SETGRIDCOLUMNHEADERIMAGE
 *
 * Sets the header image and justification of a column in a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   Column: The index of the column.
 *   Image: The index of the image for the column header.
 *   Right: If .T., the image is displayed on the right side of the header.
 *
 * Returns:
 *   NIL
 *
 * Purpose:
 *   This function sets the header image and justification of a column in a list view control.
 *   It is used to customize the appearance of the column header with an image.
 */
HB_FUNC( SETGRIDCOLUMNHEADERIMAGE )
{
   LV_COLUMN   COL;
   int         fmt = LVCFMT_IMAGE | LVCFMT_COL_HAS_IMAGES;

   COL.mask = LVCF_FMT | LVCF_IMAGE;

   if( hb_parl( 4 ) )
   {
      fmt = fmt | LVCFMT_BITMAP_ON_RIGHT | LVCFMT_RIGHT;
   }
   else
   {
      fmt = fmt | LVCFMT_LEFT;
   }

   COL.fmt = fmt;
   COL.iImage = hb_parni( 3 ) - 1;

   ListView_SetColumn( hmg_par_raw_HWND( 1 ), hb_parni( 2 ) - 1, &COL );
}

/*
 * FUNCTION: LISTVIEWGETCOUNTPERPAGE
 *
 * Retrieves the number of items that can fit vertically in the visible area of a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *
 * Returns:
 *   The number of items that can fit vertically in the visible area.
 *
 * Purpose:
 *   This function retrieves the number of items that can fit vertically in the visible area of a list view control.
 *   It is used to determine the visible capacity of the list view control.
 */
HB_FUNC( LISTVIEWGETCOUNTPERPAGE )
{
   hmg_ret_NINT( ListView_GetCountPerPage( hmg_par_raw_HWND( 1 ) ) );
}

/*
 * FUNCTION: LISTVIEW_ENSUREVISIBLE
 *
 * Ensures that an item in a list view control is visible.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   Item: The index of the item to ensure is visible.
 *
 * Returns:
 *   NIL
 *
 * Purpose:
 *   This function ensures that an item in a list view control is visible, scrolling the list view if necessary.
 *   It is used to make sure an item is visible to the user.
 */
HB_FUNC( LISTVIEW_ENSUREVISIBLE )
{
   ListView_EnsureVisible( hmg_par_raw_HWND( 1 ), hb_parni( 2 ) - 1, 1 );
}

/*
 * FUNCTION: SETIMAGELISTVIEWITEMS
 *
 * Sets the image for an item in a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   Item: The index of the item.
 *   Image: The index of the image to set.
 *
 * Returns:
 *   NIL
 *
 * Purpose:
 *   This function sets the image for an item in a list view control.
 *   It is used to customize the appearance of the item with an image.
 */
HB_FUNC( SETIMAGELISTVIEWITEMS )
{
   HWND     h = hmg_par_raw_HWND( 1 );
   LV_ITEM  LI;

   LI.mask = LVIF_IMAGE;
   LI.state = 0;
   LI.stateMask = 0;
   LI.iImage = hb_parni( 3 );
   LI.iSubItem = 0;
   LI.iItem = hb_parni( 2 ) - 1;

   ListView_SetItem( h, &LI );
}

/*
 * FUNCTION: GETIMAGELISTVIEWITEMS
 *
 * Retrieves the image index for an item in a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   Item: The index of the item.
 *
 * Returns:
 *   The index of the image for the item.
 *
 * Purpose:
 *   This function retrieves the image index for an item in a list view control.
 *   It is used to get the image index of an item.
 */
HB_FUNC( GETIMAGELISTVIEWITEMS )
{
   HWND     h = hmg_par_raw_HWND( 1 );
   LV_ITEM  LI;
   int      i;

   LI.mask = LVIF_IMAGE;
   LI.state = 0;
   LI.stateMask = 0;
   LI.iSubItem = 0;
   LI.iItem = hb_parni( 2 ) - 1;

   ListView_GetItem( h, &LI );
   i = LI.iImage;

   hmg_ret_NINT( i );
}

/*
 * FUNCTION: LISTVIEW_GETTOPINDEX
 *
 * Retrieves the index of the topmost visible item in a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *
 * Returns:
 *   The index of the topmost visible item.
 *
 * Purpose:
 *   This function retrieves the index of the topmost visible item in a list view control.
 *   It is used to determine the position of the visible area in the list view control.
 */
HB_FUNC( LISTVIEW_GETTOPINDEX )
{
   hmg_ret_NINT( ListView_GetTopIndex( hmg_par_raw_HWND( 1 ) ) );
}

/*
 * FUNCTION: LISTVIEW_REDRAWITEMS
 *
 * Redraws the items in a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   First: The index of the first item to redraw.
 *   Last: The index of the last item to redraw.
 *
 * Returns:
 *   A logical value indicating whether the redraw was successful.
 *
 * Purpose:
 *   This function redraws the items in a list view control.
 *   It is used to update the display of the items in the list view control.
 */
HB_FUNC( LISTVIEW_REDRAWITEMS )
{
   hb_retl( ListView_RedrawItems( hmg_par_raw_HWND( 1 ), hb_parni( 2 ), hb_parni( 3 ) ) );
}

/*
 * FUNCTION: LISTVIEW_HITTEST
 *
 * Performs a hit test on a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   Row: The row index to test.
 *   Col: The column index to test.
 *   Checkbox: If .T., the hit test is performed on the checkbox area.
 *
 * Returns:
 *   An array containing the item and subitem indices if the hit test is successful.
 *
 * Purpose:
 *   This function performs a hit test on a list view control to determine if a specific point is within an item or subitem.
 *   It is used to handle user interactions with the list view control.
 */
HB_FUNC( LISTVIEW_HITTEST )
{
   POINT          point;
   LVHITTESTINFO  lvhti;

   point.y = hb_parni( 2 );
   point.x = hb_parni( 3 );
   lvhti.pt = point;

   if( hb_parni( 4 ) )  // checkbox area.
   {
      ListView_HitTest( hmg_par_raw_HWND( 1 ), &lvhti );

      hmg_ret_L( lvhti.flags & LVHT_ONITEMSTATEICON );
   }
   else                 // item area.
   {
      ListView_SubItemHitTest( hmg_par_raw_HWND( 1 ), &lvhti );

      if( lvhti.flags & LVHT_ONITEM )
      {
         hb_reta( 2 );
         HB_STORNI( lvhti.iItem + 1, -1, 1 );
         HB_STORNI( lvhti.iSubItem + 1, -1, 2 );
      }
      else
      {
         hb_reta( 2 );
         HB_STORNI( 0, -1, 1 );
         HB_STORNI( 0, -1, 2 );
      }
   }
}

/*
 * FUNCTION: LISTVIEW_GETSUBITEMRECT
 *
 * Retrieves the bounding rectangle of a subitem in a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   Item: The index of the item.
 *   SubItem: The index of the subitem.
 *
 * Returns:
 *   An array containing the coordinates of the bounding rectangle.
 *
 * Purpose:
 *   This function retrieves the bounding rectangle of a subitem in a list view control.
 *   It is used to determine the position and size of a subitem.
 */
HB_FUNC( LISTVIEW_GETSUBITEMRECT )
{
   RECT  *pRect;

   pRect = ( RECT * ) hb_xgrab( sizeof( RECT ) );

   ListView_GetSubItemRect( hmg_par_raw_HWND( 1 ), hb_parni( 2 ), hb_parni( 3 ), LVIR_BOUNDS, pRect );

   hb_reta( 4 );
   HB_STORNI( pRect->top, -1, 1 );
   HB_STORNI( pRect->left, -1, 2 );
   HB_STORNI( pRect->right - pRect->left, -1, 3 );
   HB_STORNI( pRect->bottom - pRect->top, -1, 4 );

   hb_xfree( ( void * ) pRect );
}

/*
 * FUNCTION: LISTVIEW_GETITEMRECT
 *
 * Retrieves the bounding rectangle of an item in a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   Item: The index of the item.
 *
 * Returns:
 *   An array containing the coordinates of the bounding rectangle.
 *
 * Purpose:
 *   This function retrieves the bounding rectangle of an item in a list view control.
 *   It is used to determine the position and size of an item.
 */
HB_FUNC( LISTVIEW_GETITEMRECT )
{
   RECT  *pRect;

   pRect = ( RECT * ) hb_xgrab( sizeof( RECT ) );

   ListView_GetItemRect( hmg_par_raw_HWND( 1 ), hb_parni( 2 ), pRect, LVIR_LABEL );

   hb_reta( 4 );
   HB_STORNI( pRect->top, -1, 1 );
   HB_STORNI( pRect->left, -1, 2 );
   HB_STORNI( pRect->right - pRect->left, -1, 3 );
   HB_STORNI( pRect->bottom - pRect->top, -1, 4 );

   hb_xfree( ( void * ) pRect );
}

/*
 * FUNCTION: LISTVIEW_UPDATE
 *
 * Updates an item in a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   Item: The index of the item to update.
 *
 * Returns:
 *   NIL
 *
 * Purpose:
 *   This function updates an item in a list view control.
 *   It is used to refresh the display of an item in the list view control.
 */
HB_FUNC( LISTVIEW_UPDATE )
{
   ListView_Update( hmg_par_raw_HWND( 1 ), hb_parni( 2 ) - 1 );
}

/*
 * FUNCTION: LISTVIEW_SCROLL
 *
 * Scrolls the contents of a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   dx: The number of pixels to scroll horizontally.
 *   dy: The number of pixels to scroll vertically.
 *
 * Returns:
 *   NIL
 *
 * Purpose:
 *   This function scrolls the contents of a list view control by the specified number of pixels.
 *   It is used to change the visible area of the list view control.
 */
HB_FUNC( LISTVIEW_SCROLL )
{
   ListView_Scroll( hmg_par_raw_HWND( 1 ), hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * FUNCTION: LISTVIEW_SETBKCOLOR
 *
 * Sets the background color of a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   Red: The red component of the background color.
 *   Green: The green component of the background color.
 *   Blue: The blue component of the background color.
 *
 * Returns:
 *   NIL
 *
 * Purpose:
 *   This function sets the background color of a list view control.
 *   It is used to customize the appearance of the list view control.
 */
HB_FUNC( LISTVIEW_SETBKCOLOR )
{
   ListView_SetBkColor( hmg_par_raw_HWND( 1 ), RGB( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ) ) );
}

/*
 * FUNCTION: LISTVIEW_SETTEXTBKCOLOR
 *
 * Sets the background color of the text in a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   Red: The red component of the background color.
 *   Green: The green component of the background color.
 *   Blue: The blue component of the background color.
 *
 * Returns:
 *   NIL
 *
 * Purpose:
 *   This function sets the background color of the text in a list view control.
 *   It is used to customize the appearance of the text in the list view control.
 */
HB_FUNC( LISTVIEW_SETTEXTBKCOLOR )
{
   ListView_SetTextBkColor( hmg_par_raw_HWND( 1 ), RGB( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ) ) );
}

/*
 * FUNCTION: LISTVIEW_SETTEXTCOLOR
 *
 * Sets the text color of a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   Red: The red component of the text color.
 *   Green: The green component of the text color.
 *   Blue: The blue component of the text color.
 *
 * Returns:
 *   NIL
 *
 * Purpose:
 *   This function sets the text color of a list view control.
 *   It is used to customize the appearance of the text in the list view control.
 */
HB_FUNC( LISTVIEW_SETTEXTCOLOR )
{
   ListView_SetTextColor( hmg_par_raw_HWND( 1 ), RGB( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ) ) );
}

/*
 * FUNCTION: LISTVIEW_GETTEXTCOLOR
 *
 * Retrieves the text color of a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *
 * Returns:
 *   The text color of the list view control.
 *
 * Purpose:
 *   This function retrieves the text color of a list view control.
 *   It is used to get the current text color of the list view control.
 */
HB_FUNC( LISTVIEW_GETTEXTCOLOR )
{
   hmg_ret_NINT( ListView_GetTextColor( hmg_par_raw_HWND( 1 ) ) );
}

/*
 * FUNCTION: LISTVIEW_GETBKCOLOR
 *
 * Retrieves the background color of a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *
 * Returns:
 *   The background color of the list view control.
 *
 * Purpose:
 *   This function retrieves the background color of a list view control.
 *   It is used to get the current background color of the list view control.
 */
HB_FUNC( LISTVIEW_GETBKCOLOR )
{
   hmg_ret_NINT( ListView_GetBkColor( hmg_par_raw_HWND( 1 ) ) );
}

/*
 * FUNCTION: LISTVIEW_GETHEADER
 *
 * Retrieves the handle to the header control of a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *
 * Returns:
 *   Handle to the header control.
 *
 * Purpose:
 *   This function retrieves the handle to the header control of a list view control.
 *   It is used to access the header control for further operations.
 */
HB_FUNC( LISTVIEW_GETHEADER )
{
   hmg_ret_raw_HWND( ListView_GetHeader( hmg_par_raw_HWND( 1 ) ) );
}

/*
 * FUNCTION: GETHEADERLISTVIEWITEM
 *
 * Retrieves the index of the header item in a list view control.
 *
 * Parameters:
 *   lParam: Pointer to the header notification data.
 *
 * Returns:
 *   The index of the header item.
 *
 * Purpose:
 *   This function retrieves the index of the header item in a list view control.
 *   It is used to determine which header item is being interacted with.
 */
HB_FUNC( GETHEADERLISTVIEWITEM )
{
   LPNMHEADER  lpnmheader = ( LPNMHEADER ) HB_PARNL( 1 );

   hmg_ret_NINT( lpnmheader->iItem );
}

/*
 * FUNCTION: GETHEADERLISTVIEWITEMCX
 *
 * Retrieves the width of a header item in a list view control.
 *
 * Parameters:
 *   lParam: Pointer to the header notification data.
 *
 * Returns:
 *   The width of the header item.
 *
 * Purpose:
 *   This function retrieves the width of a header item in a list view control.
 *   It is used to determine the width of a header item.
 */
HB_FUNC( GETHEADERLISTVIEWITEMCX )
{
   LPNMHEADER  lpnmheader = ( LPNMHEADER ) HB_PARNL( 1 );

   if( lpnmheader->pitem->mask == HDI_WIDTH )
   {
      hmg_ret_NINT( lpnmheader->pitem->cxy );
   }
   else
   {
      hb_retni( -1 );
   }
}

/*
 * FUNCTION: LISTVIEW_ADDCOLUMN
 *
 * Adds a column to a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   Column: The index of the column to add.
 *   Width: The width of the column.
 *   Text: The text of the column header.
 *   Justify: The justification of the column header.
 *
 * Returns:
 *   NIL
 *
 * Purpose:
 *   This function adds a column to a list view control with the specified width, text, and justification.
 *   It is used to customize the appearance of the list view control.
 */
HB_FUNC( LISTVIEW_ADDCOLUMN )
{
#ifndef UNICODE
   LPSTR       lpText;
#else
   LPWSTR      lpText;
#endif
   LV_COLUMN   COL;
   HWND        hwnd = hmg_par_raw_HWND( 1 );
   int         iColumn = hb_parni( 2 ) - 1;
   PHB_ITEM    pValue = hb_itemNew( NULL );

   hb_itemCopy( pValue, hb_param( 4, HB_IT_STRING ) );

   COL.mask = LVCF_WIDTH | LVCF_TEXT | LVCF_FMT | LVCF_SUBITEM;
   COL.cx = hb_parni( 3 );
#ifndef UNICODE
   lpText = ( char * ) hb_itemGetCPtr( pValue );
#else
   lpText = AnsiToWide( ( char * ) hb_itemGetCPtr( pValue ) );
#endif
   COL.pszText = lpText;
   COL.iSubItem = iColumn;
   COL.fmt = hb_parni( 5 );

   ListView_InsertColumn( hwnd, iColumn, &COL );

   if( iColumn == 0 && COL.fmt != LVCFMT_LEFT )
   {
      COL.iSubItem = 1;
      ListView_InsertColumn( hwnd, 1, &COL );
      ListView_DeleteColumn( hwnd, 0 );
   }

#ifdef UNICODE
   hb_xfree( lpText );
#endif
   SendMessage( hwnd, LVM_DELETEALLITEMS, 0, 0 );

   RedrawWindow( hwnd, NULL, NULL, RDW_ERASE | RDW_INVALIDATE | RDW_ALLCHILDREN | RDW_ERASENOW | RDW_UPDATENOW );
}

/*
 * FUNCTION: LISTVIEW_DELETECOLUMN
 *
 * Deletes a column from a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   Column: The index of the column to delete.
 *
 * Returns:
 *   NIL
 *
 * Purpose:
 *   This function deletes a column from a list view control.
 *   It is used to remove a column from the list view control.
 */
HB_FUNC( LISTVIEW_DELETECOLUMN )
{
   HWND  hwnd = hmg_par_raw_HWND( 1 );

   ListView_DeleteColumn( hwnd, hb_parni( 2 ) - 1 );

   SendMessage( hwnd, LVM_DELETEALLITEMS, 0, 0 );

   RedrawWindow( hwnd, NULL, NULL, RDW_ERASE | RDW_INVALIDATE | RDW_ALLCHILDREN | RDW_ERASENOW | RDW_UPDATENOW );
}

/*
 * FUNCTION: LISTVIEW_GETCOLUMNWIDTH
 *
 * Retrieves the width of a column in a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   Column: The index of the column.
 *
 * Returns:
 *   The width of the column.
 *
 * Purpose:
 *   This function retrieves the width of a column in a list view control.
 *   It is used to determine the width of a column.
 */
HB_FUNC( LISTVIEW_GETCOLUMNWIDTH )
{
   hmg_ret_NINT( ListView_GetColumnWidth( hmg_par_raw_HWND( 1 ), hb_parni( 2 ) ) );
}

/*
 * FUNCTION: LISTVIEW_SETCOLUMNWIDTH
 *
 * Sets the width of a column in a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   Column: The index of the column.
 *   Width: The width to set for the column.
 *
 * Returns:
 *   A logical value indicating whether the width was successfully set.
 *
 * Purpose:
 *   This function sets the width of a column in a list view control.
 *   It is used to customize the appearance of the list view control.
 */
HB_FUNC( LISTVIEW_SETCOLUMNWIDTH )        // (JK) HMG Experimental Build 6
{
   hb_retl( ListView_SetColumnWidth( hmg_par_raw_HWND( 1 ), hb_parni( 2 ), hb_parni( 3 ) ) );
}

/*
 * FUNCTION: LISTVIEW_GETCHECKSTATE
 *
 * Retrieves the check state of an item in a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   Item: The index of the item.
 *
 * Returns:
 *   A logical value indicating the check state of the item.
 *
 * Purpose:
 *   This function retrieves the check state of an item in a list view control.
 *   It is used to determine the check state of an item.
 */
HB_FUNC( LISTVIEW_GETCHECKSTATE )
{
   HWND  hwndLV = hmg_par_raw_HWND( 1 );

   if( _isValidCtrlClass( hwndLV, WC_LISTVIEW ) )
   {
      hb_retl( ListView_GetCheckState( hwndLV, hb_parni( 2 ) - 1 ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
   }
}

/*
 * FUNCTION: LISTVIEW_SETCHECKSTATE
 *
 * Sets the check state of an item in a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   Item: The index of the item.
 *   Check: The check state to set for the item.
 *
 * Returns:
 *   A logical value indicating whether the check state was successfully set.
 *
 * Purpose:
 *   This function sets the check state of an item in a list view control.
 *   It is used to change the check state of an item.
 */
HB_FUNC( LISTVIEW_SETCHECKSTATE )
{
   HWND  hwndLV = hmg_par_raw_HWND( 1 );

   if( _isValidCtrlClass( hwndLV, WC_LISTVIEW ) )
   {
      ListView_SetCheckState( hwndLV, hb_parni( 2 ) - 1, hb_parl( 3 ) );

      hb_retl( HB_TRUE );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
   }
}

/*
 * FUNCTION: LISTVIEW_GETCOLUMNCOUNT
 *
 * Retrieves the number of columns in a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *
 * Returns:
 *   The number of columns in the list view control.
 *
 * Purpose:
 *   This function retrieves the number of columns in a list view control.
 *   It is used to determine the total number of columns in the list view control.
 */
HB_FUNC( LISTVIEW_GETCOLUMNCOUNT )        // Dr. Claudio Soto 2016/APR/07
{
   HWND  hwndLV = hmg_par_raw_HWND( 1 );

   if( _isValidCtrlClass( hwndLV, WC_LISTVIEW ) )
   {
      hmg_ret_NINT( Header_GetItemCount( ListView_GetHeader( hwndLV ) ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
   }
}

/*
 * FUNCTION: LISTVIEW_GETCOLUMNORDERARRAY
 *
 * Retrieves the order of columns in a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   ColumnCount: The number of columns.
 *
 * Returns:
 *   An array of column indices in the current order.
 *
 * Purpose:
 *   This function retrieves the order of columns in a list view control.
 *   It is used to determine the current order of columns.
 */
HB_FUNC( LISTVIEW_GETCOLUMNORDERARRAY )
{
   int   iCols = hb_parni( 2 );

   if( iCols )
   {
      int      i;
      int      *iArray = ( int * ) hb_xgrab( iCols * sizeof( int ) );
      PHB_ITEM pArray = hb_itemArrayNew( ( HB_SIZE ) iCols );

      ListView_GetColumnOrderArray( hmg_par_raw_HWND( 1 ), iCols, ( int * ) iArray );

      for( i = 0; i < iCols; i++ )
      {
         hb_arraySetNI( pArray, ( HB_SIZE ) i + 1, iArray[i] + 1 );
      }

      hb_xfree( iArray );

      hb_itemReturnRelease( pArray );
   }
   else
   {
      hb_reta( 0 );
   }
}

/*
 * FUNCTION: LISTVIEW_SETCOLUMNORDERARRAY
 *
 * Sets the order of columns in a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   ColumnCount: The number of columns.
 *   OrderArray: An array of column indices in the new order.
 *
 * Returns:
 *   NIL
 *
 * Purpose:
 *   This function sets the order of columns in a list view control.
 *   It is used to customize the appearance of the list view control.
 */
HB_FUNC( LISTVIEW_SETCOLUMNORDERARRAY )
{
   PHB_ITEM pOrder = hb_param( 3, HB_IT_ARRAY );

   if( NULL != pOrder )
   {
      int   iColumn = hb_parni( 2 );

      if( iColumn )
      {
         int   i;
         int   *iArray = ( int * ) hb_xgrab( iColumn * sizeof( int ) );

         for( i = 0; i < iColumn; i++ )
         {
            iArray[i] = HB_PARNI( 3, i + 1 ) - 1;
         }

         ListView_SetColumnOrderArray( hmg_par_raw_HWND( 1 ), iColumn, ( int * ) iArray );

         hb_xfree( iArray );
      }
   }
}

/*
 * FUNCTION: LISTVIEW_CHANGEEXTENDEDSTYLE
 *
 * Changes the extended style of a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   AddStyle: The style to add.
 *   RemoveStyle: The style to remove.
 *
 * Returns:
 *   The new extended style of the list view control.
 *
 * Purpose:
 *   This function changes the extended style of a list view control by adding or removing styles.
 *   It is used to customize the appearance and behavior of the list view control.
 */
HB_FUNC( LISTVIEW_CHANGEEXTENDEDSTYLE )   // Dr. Claudio Soto
{
   HWND  hWnd = hmg_par_raw_HWND( 1 );
   DWORD Add = hmg_par_DWORD( 2 );
   DWORD Remove = hmg_par_DWORD( 3 );
   DWORD OldStyle, NewStyle;

   OldStyle = ListView_GetExtendedListViewStyle( hWnd );
   NewStyle = ( OldStyle | Add ) & ( ~Remove );

   hmg_ret_DWORD( ListView_SetExtendedListViewStyle( hWnd, NewStyle ) );
}

/*
 * FUNCTION: LISTVIEW_GETEXTENDEDSTYLE
 *
 * Retrieves the extended style of a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   ExStyle: The extended style to check.
 *
 * Returns:
 *   A logical value indicating whether the extended style is set.
 *
 * Purpose:
 *   This function retrieves the extended style of a list view control.
 *   It is used to determine the current extended style of the list view control.
 */
HB_FUNC( LISTVIEW_GETEXTENDEDSTYLE )      // Dr. Claudio Soto
{
   HWND  hWnd = hmg_par_raw_HWND( 1 );
   DWORD ExStyle = hmg_par_DWORD( 2 );
   DWORD OldStyle = ListView_GetExtendedListViewStyle( hWnd );

   if( HB_ISNUM( 2 ) )
   {
      hmg_ret_L( ( OldStyle & ExStyle ) == ExStyle );
   }
   else
   {
      hmg_ret_DWORD( OldStyle );
   }
}

#if ( ( defined( __BORLANDC__ ) && __BORLANDC__ < 1410 ) )
#define HDF_SORTDOWN 0x0200
#define HDF_SORTUP   0x0400
#endif

/*
 * FUNCTION: LISTVIEW_SETSORTHEADER
 *
 * Sets the sort indicator for a header in a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   Column: The index of the column.
 *   Type: The type of sort indicator to set.
 *   Image: If .T., use an image for the sort indicator.
 *
 * Returns:
 *   The previous sort indicator type.
 *
 * Purpose:
 *   This function sets the sort indicator for a header in a list view control.
 *   It is used to indicate the sort direction of a column.
 */
HB_FUNC( LISTVIEW_SETSORTHEADER )
{
   HWND     hWndHD = ( HWND ) SendMessage( hmg_par_raw_HWND( 1 ), LVM_GETHEADER, 0, 0 );
   INT      nItem = hb_parni( 2 ) - 1;
   INT      nType;
   HDITEM   hdItem;

   if( hb_parl( 4 ) )
   {
      hdItem.mask = HDI_FORMAT;

      SendMessage( hWndHD, HDM_GETITEM, nItem, ( LPARAM ) &hdItem );

      if( hdItem.fmt & HDF_SORTUP )
      {
         hb_retni( 1 );
      }
      else if( hdItem.fmt & HDF_SORTDOWN )
      {
         hb_retni( -1 );
      }
      else
      {
         hb_retni( 0 );
      }

      if( ( hb_pcount() > 2 ) && HB_ISNUM( 3 ) )
      {
         nType = hb_parni( 3 );

         if( nType == 0 )
         {
            hdItem.fmt &= ~( HDF_SORTDOWN | HDF_SORTUP );
         }
         else if( nType > 0 )
         {
            hdItem.fmt = ( hdItem.fmt &~HDF_SORTDOWN ) | HDF_SORTUP;
         }
         else
         {
            hdItem.fmt = ( hdItem.fmt &~HDF_SORTUP ) | HDF_SORTDOWN;
         }

         SendMessage( hWndHD, HDM_SETITEM, nItem, ( LPARAM ) &hdItem );
      }
   }
   else
   {
      hdItem.mask = HDI_BITMAP | HDI_FORMAT;

      SendMessage( hWndHD, HDM_GETITEM, nItem, ( LPARAM ) &hdItem );

      nType = hb_parni( 3 );

      if( nType == 0 )
      {
         hdItem.mask = HDI_FORMAT;
         hdItem.fmt &= ~( HDF_BITMAP | HDF_BITMAP_ON_RIGHT );
      }
      else
      {
         if( nType > 0 )
         {
            hdItem.hbm = ( HBITMAP ) LoadImage
               (
                  GetInstance(),
                  TEXT( "MINIGUI_GRID_ASC" ),
                  IMAGE_BITMAP,
                  0,
                  0,
                  LR_LOADTRANSPARENT | LR_DEFAULTCOLOR | LR_LOADMAP3DCOLORS
               );
         }
         else
         {
            hdItem.hbm = ( HBITMAP ) LoadImage
               (
                  GetInstance(),
                  TEXT( "MINIGUI_GRID_DSC" ),
                  IMAGE_BITMAP,
                  0,
                  0,
                  LR_LOADTRANSPARENT | LR_DEFAULTCOLOR | LR_LOADMAP3DCOLORS
               );
         }

         hdItem.fmt |= HDF_BITMAP;
         if( hdItem.fmt & HDF_RIGHT )
         {
            hdItem.fmt &= ~HDF_BITMAP_ON_RIGHT;
         }
         else
         {
            hdItem.fmt |= HDF_BITMAP_ON_RIGHT;
         }
      }

      SendMessage( hWndHD, HDM_SETITEM, nItem, ( LPARAM ) &hdItem );
   }
}

#define MAX_GROUP_BUFFER   2048

/*
 * FUNCTION: LISTVIEW_GROUPITEMSETID
 *
 * Sets the group ID for an item in a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   Row: The index of the item.
 *   GroupID: The group ID to set.
 *
 * Returns:
 *   A logical value indicating whether the group ID was successfully set.
 *
 * Purpose:
 *   This function sets the group ID for an item in a list view control.
 *   It is used to group items in the list view control.
 */
HB_FUNC( LISTVIEW_GROUPITEMSETID )
{
   HWND     hWnd = hmg_par_raw_HWND( 1 );
   INT      nRow = hmg_par_INT( 2 );
   INT      GroupID = hmg_par_INT( 3 );

#if ( ( defined( __BORLANDC__ ) && __BORLANDC__ < 1410 ) )
   _LVITEM  LVI;
#else
   LVITEM   LVI;
#endif
   LVI.mask = LVIF_GROUPID;
   LVI.iItem = nRow;
   LVI.iSubItem = 0;
   LVI.iGroupId = GroupID;

   hb_retl( ListView_SetItem( hWnd, &LVI ) );
}

/*
 * FUNCTION: LISTVIEW_GROUPITEMGETID
 *
 * Retrieves the group ID for an item in a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   Row: The index of the item.
 *
 * Returns:
 *   The group ID of the item.
 *
 * Purpose:
 *   This function retrieves the group ID for an item in a list view control.
 *   It is used to determine the group ID of an item.
 */
HB_FUNC( LISTVIEW_GROUPITEMGETID )
{
   HWND     hWnd = hmg_par_raw_HWND( 1 );
   INT      nRow = hmg_par_INT( 2 );

#if ( ( defined( __BORLANDC__ ) && __BORLANDC__ < 1410 ) )
   _LVITEM  LVI;
#else
   LVITEM   LVI;
#endif
   LVI.mask = LVIF_GROUPID;
   LVI.iItem = nRow;
   LVI.iSubItem = 0;
   ListView_GetItem( hWnd, &LVI );

   hmg_ret_NINT( LVI.iGroupId );
}

/*
 * FUNCTION: LISTVIEW_ISGROUPVIEWENABLED
 *
 * Determines whether group view is enabled in a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *
 * Returns:
 *   A logical value indicating whether group view is enabled.
 *
 * Purpose:
 *   This function determines whether group view is enabled in a list view control.
 *   It is used to check the current state of the list view control.
 */
HB_FUNC( LISTVIEW_ISGROUPVIEWENABLED )
{
   hb_retl( ListView_IsGroupViewEnabled( hmg_par_raw_HWND( 1 ) ) );
}

/*
 * FUNCTION: LISTVIEW_ENABLEGROUPVIEW
 *
 * Enables or disables group view in a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   Enable: If .T., enables group view; otherwise, disables group view.
 *
 * Returns:
 *   NIL
 *
 * Purpose:
 *   This function enables or disables group view in a list view control.
 *   It is used to customize the appearance and behavior of the list view control.
 */
HB_FUNC( LISTVIEW_ENABLEGROUPVIEW )
{
   ListView_EnableGroupView( hmg_par_raw_HWND( 1 ), hb_parl( 2 ) );
}

/*
 * FUNCTION: LISTVIEW_GROUPDELETEALL
 *
 * Deletes all groups in a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *
 * Returns:
 *   NIL
 *
 * Purpose:
 *   This function deletes all groups in a list view control.
 *   It is used to remove all groups from the list view control.
 */
HB_FUNC( LISTVIEW_GROUPDELETEALL )
{
   ListView_RemoveAllGroups( hmg_par_raw_HWND( 1 ) );
}

/*
 * FUNCTION: LISTVIEW_GROUPDELETE
 *
 * Deletes a group in a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   GroupID: The ID of the group to delete.
 *
 * Returns:
 *   A logical value indicating whether the group was successfully deleted.
 *
 * Purpose:
 *   This function deletes a group in a list view control.
 *   It is used to remove a specific group from the list view control.
 */
HB_FUNC( LISTVIEW_GROUPDELETE )
{
   hb_retni( ( int ) ListView_RemoveGroup( hmg_par_raw_HWND( 1 ), hmg_par_INT( 2 ) ) );
}

/*
 * FUNCTION: LISTVIEW_GROUPADD
 *
 * Adds a group to a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   GroupID: The ID of the group to add.
 *   Index: The index at which to insert the group.
 *
 * Returns:
 *   The index of the newly added group.
 *
 * Purpose:
 *   This function adds a group to a list view control.
 *   It is used to create a new group in the list view control.
 */
HB_FUNC( LISTVIEW_GROUPADD )
{
   HWND     hWnd = hmg_par_raw_HWND( 1 );
   INT      GroupID = hmg_par_INT( 2 );
   INT      nIndex = HB_ISNUM( 3 ) ? hmg_par_INT( 3 ) : ( INT ) - 1;

   LVGROUP  LVG;

   LVG.cbSize = sizeof( LVGROUP );
   LVG.stateMask = LVM_SETGROUPINFO;
   LVG.mask = LVGF_GROUPID | LVGF_HEADER | LVGF_FOOTER | LVGF_ALIGN | LVGF_STATE;
   LVG.iGroupId = GroupID;
   LVG.pszHeader = L"";
   LVG.pszFooter = L"";
   LVG.uAlign = LVGA_HEADER_LEFT | LVGA_FOOTER_LEFT;
   LVG.state = LVGS_NORMAL;

   hb_retni( ( int ) ListView_InsertGroup( hWnd, nIndex, &LVG ) );
}

/*
 * FUNCTION: LISTVIEW_GROUPSETINFO
 *
 * Sets the information for a group in a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   GroupID: The ID of the group.
 *   Header: The header text for the group.
 *   AlignHeader: The alignment of the header text.
 *   Footer: The footer text for the group.
 *   AlignFooter: The alignment of the footer text.
 *   State: The state of the group.
 *
 * Returns:
 *   The index of the group if successful, otherwise -1.
 *
 * Purpose:
 *   This function sets the information for a group in a list view control.
 *   It is used to customize the appearance and behavior of the group.
 */
HB_FUNC( LISTVIEW_GROUPSETINFO )
{
   HWND     hWnd = hmg_par_raw_HWND( 1 );
   INT      GroupID = hmg_par_INT( 2 );
#ifdef UNICODE
   HB_WCHAR *cHeader = ( HB_WCHAR * ) ( hb_parclen( 3 ) == 0 ? NULL : AnsiToWide( hb_parc( 3 ) ) );
   HB_WCHAR *cFooter = ( HB_WCHAR * ) ( hb_parclen( 5 ) == 0 ? NULL : AnsiToWide( hb_parc( 5 ) ) );
#else
   HB_WCHAR *cHeader = ( HB_WCHAR * ) ( hb_parclen( 3 ) == 0 ? NULL : hb_mbtowc( hb_parc( 3 ) ) );
   HB_WCHAR *cFooter = ( HB_WCHAR * ) ( hb_parclen( 5 ) == 0 ? NULL : hb_mbtowc( hb_parc( 5 ) ) );
#endif
   UINT     nAlignHeader = hmg_par_UINT( 4 );
   UINT     nAlignFooter = hmg_par_UINT( 6 );
   UINT     nState = hmg_par_UINT( 7 );

   HB_WCHAR cHeaderBuffer[MAX_GROUP_BUFFER];
   HB_WCHAR cFooterBuffer[MAX_GROUP_BUFFER];

   LVGROUP  LVG;

   LVG.cbSize = sizeof( LVGROUP );
   LVG.stateMask = LVM_GETGROUPINFO;
   LVG.mask = LVGF_HEADER | LVGF_FOOTER | LVGF_ALIGN | LVGF_STATE;
   LVG.pszHeader = cHeaderBuffer;
   LVG.cchHeader = sizeof( cHeaderBuffer ) / sizeof( WCHAR );
   LVG.pszFooter = cFooterBuffer;
   LVG.cchFooter = sizeof( cFooterBuffer ) / sizeof( WCHAR );

   if( ListView_GetGroupInfo( hWnd, GroupID, &LVG ) != -1 )
   {
      UINT  nAlign = 0;
      LVG.stateMask = LVM_SETGROUPINFO;
      LVG.pszHeader = ( cHeader != NULL ) ? cHeader : cHeaderBuffer;
      LVG.pszFooter = ( cFooter != NULL ) ? cFooter : cFooterBuffer;
      nAlign = nAlign | ( ( nAlignHeader != 0 ) ? nAlignHeader : ( LVG.uAlign & 0x07 ) );
      nAlign = nAlign | ( ( nAlignFooter != 0 ) ? ( nAlignFooter << 3 ) : ( LVG.uAlign & 0x38 ) );
      LVG.uAlign = nAlign;
      LVG.state = nState != 0 ? ( nState >> 1 ) : LVG.state;

      hb_retni( ( int ) ListView_SetGroupInfo( hWnd, GroupID, &LVG ) );
   }
   else
   {
      hb_retni( -1 );
   }
}

/*
 * FUNCTION: LISTVIEW_GROUPGETINFO
 *
 * Retrieves the information for a group in a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   GroupID: The ID of the group.
 *   Header: Reference to store the header text.
 *   AlignHeader: Reference to store the alignment of the header text.
 *   Footer: Reference to store the footer text.
 *   AlignFooter: Reference to store the alignment of the footer text.
 *   State: Reference to store the state of the group.
 *
 * Returns:
 *   The result of the operation.
 *
 * Purpose:
 *   This function retrieves the information for a group in a list view control.
 *   It is used to get the current settings of the group.
 */
HB_FUNC( LISTVIEW_GROUPGETINFO )
{
   HWND     hWnd = hmg_par_raw_HWND( 1 );
   INT      GroupID = hmg_par_INT( 2 );

   int      nRet;
   HB_WCHAR cHeaderBuffer[MAX_GROUP_BUFFER];
   HB_WCHAR cFooterBuffer[MAX_GROUP_BUFFER];

   LVGROUP  LVG;

   LVG.cbSize = sizeof( LVGROUP );
   LVG.stateMask = LVM_GETGROUPINFO;
   LVG.mask = LVGF_HEADER | LVGF_FOOTER | LVGF_ALIGN | LVGF_STATE;
   LVG.pszHeader = cHeaderBuffer;
   LVG.cchHeader = sizeof( cHeaderBuffer ) / sizeof( WCHAR );
   LVG.pszFooter = cFooterBuffer;
   LVG.cchFooter = sizeof( cFooterBuffer ) / sizeof( WCHAR );

   if( ( nRet = ( int ) ListView_GetGroupInfo( hWnd, GroupID, &LVG ) ) != -1 )
   {
      hb_storc( hb_wctomb( cHeaderBuffer ), 3 );
      hb_storni( ( LVG.uAlign & 0x07 ), 4 );
      hb_storc( hb_wctomb( cFooterBuffer ), 5 );
      hb_storni( ( ( LVG.uAlign & 0x38 ) >> 3 ), 6 );
      hb_storni( LVG.state != 0 ? ( LVG.state << 1 ) : 1, 7 );
   }

   hb_retni( nRet );
}

/*
 * FUNCTION: LISTVIEW_HASGROUP
 *
 * Determines whether a group exists in a list view control.
 *
 * Parameters:
 *   hWnd: Handle to the list view control.
 *   GroupID: The ID of the group.
 *
 * Returns:
 *   A logical value indicating whether the group exists.
 *
 * Purpose:
 *   This function determines whether a group exists in a list view control.
 *   It is used to check the existence of a group in the list view control.
 */
HB_FUNC( LISTVIEW_HASGROUP )
{
   HWND  hWnd = hmg_par_raw_HWND( 1 );
   INT   GroupID = hmg_par_INT( 2 );

   hb_retl( ( BOOL ) ListView_HasGroup( hWnd, GroupID ) );
}

/*
 * FUNCTION: HEADER_CUSTOMDRAW_GETITEM
 *
 * Retrieves the item index from a header custom draw notification.
 *
 * Parameters:
 *   lParam: Pointer to the custom draw notification data.
 *
 * Returns:
 *   The item index.
 *
 * Purpose:
 *   This function retrieves the item index from a header custom draw notification.
 *   It is used to determine which item is being customized.
 */
HB_FUNC( HEADER_CUSTOMDRAW_GETITEM )
{
   LPARAM         lParam = hmg_par_raw_LPARAM( 1 );
   LPNMCUSTOMDRAW lpNMCustomDraw = ( LPNMCUSTOMDRAW ) lParam;

   hmg_ret_DWORD( lpNMCustomDraw->dwItemSpec );
}

/*
 * FUNCTION: HEADER_CUSTOMDRAW_GETACTION
 *
 * Determines the action to take for a header custom draw notification.
 *
 * Parameters:
 *   lParam: Pointer to the custom draw notification data.
 *
 * Returns:
 *   The action to take.
 *
 * Purpose:
 *   This function determines the action to take for a header custom draw notification.
 *   It is used to customize the appearance of the header control.
 */
HB_FUNC( HEADER_CUSTOMDRAW_GETACTION )
{
   LPARAM         lParam = hmg_par_raw_LPARAM( 1 );
   LPNMCUSTOMDRAW lpNMCustomDraw = ( LPNMCUSTOMDRAW ) lParam;

   if( lpNMCustomDraw->dwDrawStage == CDDS_PREPAINT )
   {
      hb_retni( CDRF_NOTIFYITEMDRAW );
   }
   else if( lpNMCustomDraw->dwDrawStage == CDDS_ITEMPREPAINT )
   {
      hb_retni( -1 );
   }
   else
   {
      hb_retni( CDRF_DODEFAULT );
   }
}

/*
 * FUNCTION: HEADER_SETFONT
 *
 * Sets the font for a header custom draw notification.
 *
 * Parameters:
 *   lParam: Pointer to the custom draw notification data.
 *   BackColor: The background color.
 *   TextColor: The text color.
 *   hFont: Handle to the font.
 *
 * Returns:
 *   The action to take.
 *
 * Purpose:
 *   This function sets the font for a header custom draw notification.
 *   It is used to customize the appearance of the header control.
 */
HB_FUNC( HEADER_SETFONT )
{
   LPARAM         lParam = hmg_par_raw_LPARAM( 1 );
   LPNMCUSTOMDRAW lpNMCustomDraw = ( LPNMCUSTOMDRAW ) lParam;
   COLORREF       BackColor = hmg_par_COLORREF( 2 );
   COLORREF       TextColor = hmg_par_COLORREF( 3 );
   HFONT          hFont = hmg_par_raw_HFONT( 4 );
   RECT           rect = lpNMCustomDraw->rc;
   HBRUSH         hBrush = CreateSolidBrush( BackColor );

   SetBkColor( lpNMCustomDraw->hdc, BackColor );
   SetTextColor( lpNMCustomDraw->hdc, TextColor );
   SetBkMode( lpNMCustomDraw->hdc, TRANSPARENT );
   FillRect( lpNMCustomDraw->hdc, &rect, hBrush );
   DeleteObject( hBrush );

   if( hFont )
   {
      SelectObject( lpNMCustomDraw->hdc, hFont );
   }

   hb_retni( CDRF_NEWFONT );
}
