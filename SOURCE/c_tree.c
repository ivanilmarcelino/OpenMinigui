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

#define _WIN32_IE 0x0501      // Define the minimum required Internet Explorer version for the application
#include <mgdefs.h>           // Include application-specific definitions
#include <commctrl.h>         // Include common control definitions and structures for UI elements

// Functions to load and add images to an ImageList for use with the TreeView control
HIMAGELIST  HMG_ImageListLoadFirst( const char *FileName, int cGrow, int Transparent, int *nWidth, int *nHeight );
void        HMG_ImageListAdd( HIMAGELIST himl, char *FileName, int Transparent );

// Functions to get the instance and resources handles of the application
HINSTANCE   GetInstance( void );
HINSTANCE   GetResources( void );

// Initialize a TreeView control with specific settings
HB_FUNC( INITTREE )
{
   INITCOMMONCONTROLSEX icex; // Structure for common control initialization
   UINT                 mask; // Variable to store TreeView style flags
   if( hb_parni( 9 ) != 0 )
   {
      mask = 0x0000;          // No additional style if 9th parameter is non-zero
   }
   else
   {
      mask = TVS_LINESATROOT; // Add root line if 9th parameter is zero
   }

   icex.dwSize = sizeof( INITCOMMONCONTROLSEX );   // Set the structure size
   icex.dwICC = ICC_TREEVIEW_CLASSES;              // Specify TreeView control class
   InitCommonControlsEx( &icex );                  // Initialize the TreeView control

   // Create and return a TreeView control window
   hmg_ret_raw_HWND
   (
      CreateWindowEx
         (
            WS_EX_CLIENTEDGE,                // Extended window style
            WC_TREEVIEW,                     // Class name for TreeView control
            TEXT( "" ),                      // Window name (empty for this case)
            WS_VISIBLE | WS_TABSTOP | WS_CHILD | TVS_HASLINES | TVS_HASBUTTONS | mask | TVS_SHOWSELALWAYS,
            hb_parni( 2 ),                   // X position
            hb_parni( 3 ),                   // Y position
            hb_parni( 4 ),                   // Width
            hb_parni( 5 ),                   // Height
            hmg_par_raw_HWND( 1 ),           // Parent window handle
            hmg_par_raw_HMENU( 6 ),          // Menu handle
            GetInstance(),                   // Application instance handle
            NULL                             // Additional parameters
         )
   );
}

// Initialize and load a TreeView control's bitmap image list
HB_FUNC( INITTREEVIEWBITMAP )
{
   HIMAGELIST  himl = ( HIMAGELIST ) NULL;   // Image list handle for TreeView bitmaps
   PHB_ITEM    hArray;
   char        *FileName;
   int         ic = 0;                    // Image count
   int         nCount;
   int         s;
   int         cx = -1;                   // Image width
   int         cy = -1;                   // Image height
   nCount = ( int ) hb_parinfa( 2, 0 );   // Get the number of images in the array
   if( nCount > 0 )
   {
      int   Transparent = hb_parl( 3 ) ? 0 : 1; // Check if images should be transparent
      hArray = hb_param( 2, HB_IT_ARRAY );      // Get array parameter

      // Loop through array to load each image file into the image list
      for( s = 1; s <= nCount; s++ )
      {
         FileName = ( char * ) hb_arrayGetCPtr( hArray, s );

         if( himl == NULL )
         {
            // Load the first image and initialize the image list
            himl = HMG_ImageListLoadFirst( FileName, nCount, Transparent, &cx, &cy );
         }
         else
         {
            // Add subsequent images to the existing image list
            HMG_ImageListAdd( himl, FileName, Transparent );
         }
      }

      if( himl != NULL )
      {
         // Set the TreeView's normal image list
         SendMessage( hmg_par_raw_HWND( 1 ), TVM_SETIMAGELIST, ( WPARAM ) TVSIL_NORMAL, ( LPARAM ) himl );
      }

      ic = ImageList_GetImageCount( himl );     // Get the count of images in the image list
   }

   hb_retni( ic );   // Return the number of images loaded
}

// Add a bitmap image to an existing TreeView control's image list
HB_FUNC( ADDTREEVIEWBITMAP )
{
   HWND        hbutton = hmg_par_raw_HWND( 1 );             // Handle to the TreeView control
   HIMAGELIST  himl;
   int         Transparent = hb_parl( 3 ) ? 0 : 1;          // Set transparency option
   int         ic = 0;

   himl = TreeView_GetImageList( hbutton, TVSIL_NORMAL );   // Retrieve the existing image list
   if( himl != NULL )
   {
      // Add the image to the existing image list
      HMG_ImageListAdd( himl, ( char * ) hb_parc( 2 ), Transparent );

      // Update the TreeView control's image list
      SendMessage( hbutton, TVM_SETIMAGELIST, ( WPARAM ) TVSIL_NORMAL, ( LPARAM ) himl );

      ic = ImageList_GetImageCount( himl );                 // Get the updated image count
   }

   hb_retni( ic );            // Return the image count after addition
}

#define MAX_ITEM_TEXT   256   // Define maximum text length for TreeView items

// Structure to store information about a TreeView item
typedef struct
{
   HTREEITEM   ItemHandle;    // Handle of the TreeView item
   LONG        nID;           // Item ID
   BOOL        IsNodeFlag;    // Flag indicating if the item is a node
} HMG_StructTreeItemLPARAM;

// Function to associate custom data with a TreeView item
void AddTreeItemLPARAM( HWND hWndTV, HTREEITEM ItemHandle, LONG nID, BOOL IsNodeFlag )
{
   TV_ITEM  TV_Item;

   if( ( hWndTV != NULL ) && ( ItemHandle != NULL ) )
   {
      // Allocate memory for the custom data structure
      HMG_StructTreeItemLPARAM   *TreeItemLPARAM = ( HMG_StructTreeItemLPARAM * ) hb_xgrab( sizeof( HMG_StructTreeItemLPARAM ) );
      TreeItemLPARAM->ItemHandle = ItemHandle;           // Set the item handle
      TreeItemLPARAM->nID = nID;                         // Set the item ID
      TreeItemLPARAM->IsNodeFlag = IsNodeFlag;           // Set the node flag
      TV_Item.mask = TVIF_PARAM;                         // Specify we are setting the lParam field
      TV_Item.hItem = ( HTREEITEM ) ItemHandle;          // Set the item handle
      TV_Item.lParam = ( LPARAM ) TreeItemLPARAM;        // Set the custom data
      TreeView_SetItem( hWndTV, &TV_Item );              // Assign the custom data to the TreeView item
   }
}

// Function to add an item to the TreeView
HB_FUNC( ADDTREEITEM )
{
   HWND              hWndTV = hmg_par_raw_HWND( 1 );     // Handle of the TreeView control
   HTREEITEM         hPrev = hmg_par_raw_TREEITEM( 2 );  // Handle to the parent item
   HTREEITEM         hRet;

#ifndef UNICODE
   LPSTR             lpText = ( LPSTR ) hb_parc( 3 );    // ANSI text
#else
   LPWSTR            lpText = hb_osStrU16Encode( ( char * ) hb_parc( 3 ) );   // Unicode text
#endif
   TV_ITEM           tvi;
   TV_INSERTSTRUCT   is;

   LONG              nID = hmg_par_LONG( 6 );            // Unique ID for the item
   BOOL              IsNodeFlag = ( BOOL ) hb_parl( 7 ); // Flag indicating if the item is a node
   tvi.mask = TVIF_TEXT | TVIF_IMAGE | TVIF_SELECTEDIMAGE | TVIF_PARAM; // Specify fields for the item
   tvi.pszText = lpText;                                 // Set the item text
   tvi.cchTextMax = 1024;                                // Maximum text length
   tvi.iImage = hb_parni( 4 );                           // Set the image index for the item
   tvi.iSelectedImage = hb_parni( 5 );                   // Set the selected image index
   tvi.lParam = nID;                                     // Set item ID as custom data

   // Insert the item into the TreeView
#if ( defined( __BORLANDC__ ) && __BORLANDC__ <= 1410 )
   is.DUMMYUNIONNAME.item = tvi;
#else
   is.item = tvi;
#endif
   if( hPrev == 0 )
   {
      is.hInsertAfter = hPrev;                           // Insert at the root if no parent item specified
      is.hParent = NULL;
   }
   else
   {
      is.hInsertAfter = TVI_LAST;                        // Insert as last child of specified parent
      is.hParent = hPrev;
   }

   hRet = TreeView_InsertItem( hWndTV, &is );            // Insert the item and get its handle
   AddTreeItemLPARAM( hWndTV, hRet, nID, IsNodeFlag );   // Associate custom data with the item
   hmg_ret_raw_HANDLE( hRet );               // Return the handle of the added item
#ifdef UNICODE
   hb_xfree( lpText );                       // Free memory if Unicode text was used
#endif
}

// Function to get the currently selected item in a TreeView control
HB_FUNC( TREEVIEW_GETSELECTION )
{
   HTREEITEM   ItemHandle;

   // Retrieve the handle of the currently selected TreeView item
   ItemHandle = TreeView_GetSelection( hmg_par_raw_HWND( 1 ) );

   if( ItemHandle == NULL )
   {
      return;                                // If no item is selected, exit the function
   }

   hmg_ret_raw_HANDLE( ItemHandle );         // Return the handle of the selected item
}

// Function to select a specific item in a TreeView control
HB_FUNC( TREEVIEW_SELECTITEM )
{
   TreeView_SelectItem( hmg_par_raw_HWND( 1 ), hmg_par_raw_TREEITEM( 2 ) );
}

// Recursive function to free memory associated with TreeView items
void TreeView_FreeMemoryLPARAMRecursive( HWND hWndTV, HTREEITEM ItemHandle )
{
   HMG_StructTreeItemLPARAM   *TreeItemLPARAM;
   TV_ITEM                    TreeItem;
   HTREEITEM                  ChildItem;
   HTREEITEM                  NextItem;

   // Retrieve the lParam data from the specified item
   TreeItem.mask = TVIF_PARAM;
   TreeItem.hItem = ItemHandle;
   TreeItem.lParam = ( LPARAM ) NULL;
   TreeView_GetItem( hWndTV, &TreeItem );

   // Free memory if custom data is attached to the item
   TreeItemLPARAM = ( HMG_StructTreeItemLPARAM * ) TreeItem.lParam;
   if( TreeItemLPARAM != NULL )
   {
      hb_xfree( TreeItemLPARAM );            // Free allocated memory
      TreeItem.lParam = ( LPARAM ) NULL;     // Set lParam to NULL for security
      TreeView_SetItem( hWndTV, &TreeItem ); // Update item to remove custom data
   }

   // Recursively free memory for all child items
   ChildItem = TreeView_GetChild( hWndTV, ItemHandle );
   while( ChildItem != NULL )
   {
      TreeView_FreeMemoryLPARAMRecursive( hWndTV, ChildItem );
      NextItem = TreeView_GetNextSibling( hWndTV, ChildItem );
      ChildItem = NextItem;
   }
}

// Function to delete a specific item from a TreeView control
HB_FUNC( TREEVIEW_DELETEITEM )
{
   HWND        TreeHandle = hmg_par_raw_HWND( 1 );
   HTREEITEM   ItemHandle = hmg_par_raw_TREEITEM( 2 );

   // Free any allocated memory associated with the item
   TreeView_FreeMemoryLPARAMRecursive( TreeHandle, ItemHandle );

   // Delete the specified item from the TreeView control
   TreeView_DeleteItem( TreeHandle, ItemHandle );
}

// Function to delete all items from a TreeView control
HB_FUNC( TREEVIEW_DELETEALLITEMS )
{
   HWND                       TreeHandle = hmg_par_raw_HWND( 1 );
   int                        nCount = ( int ) hb_parinfa( 2, 0 );
   int                        i;
   TV_ITEM                    TreeItem;
   HMG_StructTreeItemLPARAM   *TreeItemLPARAM;

   // Iterate through each item to free any allocated memory
   for( i = 1; i <= nCount; i++ )
   {
      TreeItem.mask = TVIF_PARAM;
      TreeItem.hItem = hmg_parv_raw_TREEITEM( 2, i );
      TreeItem.lParam = ( LPARAM ) 0;

      TreeView_GetItem( TreeHandle, &TreeItem );

      TreeItemLPARAM = ( HMG_StructTreeItemLPARAM * ) TreeItem.lParam;
      if( TreeItemLPARAM != NULL )
      {
         hb_xfree( TreeItemLPARAM );         // Free allocated memory for each item
      }
   }

   // Delete all items from the TreeView control
   TreeView_DeleteAllItems( TreeHandle );
}

// Function to get the count of items in a TreeView control
HB_FUNC( TREEVIEW_GETCOUNT )
{
   hmg_ret_UINT( TreeView_GetCount( hmg_par_raw_HWND( 1 ) ) ); // Return the item count
}

// Function to get the previous sibling of a specified TreeView item
HB_FUNC( TREEVIEW_GETPREVSIBLING )
{
   HWND        TreeHandle = hmg_par_raw_HWND( 1 );
   HTREEITEM   ItemHandle = hmg_par_raw_TREEITEM( 2 );
   HTREEITEM   PrevItemHandle;

   // Retrieve the handle of the previous sibling item
   PrevItemHandle = TreeView_GetPrevSibling( TreeHandle, ItemHandle );

   hmg_ret_raw_HANDLE( PrevItemHandle );  // Return the handle of the previous sibling
}

// Function to get information about a specified TreeView item
HB_FUNC( TREEVIEW_GETITEM )
{
   HWND        TreeHandle;
   HTREEITEM   TreeItemHandle;
   TV_ITEM     TreeItem;
   TCHAR       ItemText[MAX_ITEM_TEXT];

#ifdef UNICODE
   LPSTR       pStr;
#endif
   TreeHandle = hmg_par_raw_HWND( 1 );
   TreeItemHandle = hmg_par_raw_TREEITEM( 2 );

   memset( &TreeItem, 0, sizeof( TV_ITEM ) );

   TreeItem.mask = TVIF_TEXT;
   TreeItem.hItem = TreeItemHandle;

   TreeItem.pszText = ItemText;
   TreeItem.cchTextMax = sizeof( ItemText ) / sizeof( TCHAR );

   // Get the item's text from the TreeView control
   TreeView_GetItem( TreeHandle, &TreeItem );

#ifndef UNICODE
   hb_retc( ItemText );                   // Return the item text if non-UNICODE
#else
   pStr = hb_osStrU16Decode( ItemText );  // Decode UNICODE text to ANSI
   hb_retc( pStr );
   hb_xfree( pStr ); // Free allocated memory for ANSI text
#endif
}

// Function to set the text of a specified TreeView item
HB_FUNC( TREEVIEW_SETITEM )
{
   HWND        TreeHandle;
   HTREEITEM   TreeItemHandle;
   TV_ITEM     TreeItem;
   TCHAR       ItemText[MAX_ITEM_TEXT];

#ifdef UNICODE
   LPWSTR      lpText;
#endif
   TreeHandle = hmg_par_raw_HWND( 1 );
   TreeItemHandle = hmg_par_raw_TREEITEM( 2 );

   memset( &TreeItem, 0, sizeof( TV_ITEM ) );
#ifdef UNICODE
   lpText = hb_osStrU16Encode( hb_parc( 3 ) );  // Encode ANSI text to UNICODE
   lstrcpy( ItemText, lpText );
#else
   lstrcpy( ItemText, hb_parc( 3 ) );           // Copy the text parameter to ItemText
#endif
   TreeItem.mask = TVIF_TEXT;                   // Specify text setting
   TreeItem.hItem = TreeItemHandle;             // Set the item handle
   TreeItem.pszText = ItemText;                 // Set the new item text
   TreeItem.cchTextMax = sizeof( ItemText ) / sizeof( TCHAR );

   TreeView_SetItem( TreeHandle, &TreeItem );   // Update the TreeView item
#ifdef UNICODE
   hb_xfree( lpText );  // Free allocated memory if UNICODE was used
#endif
}

// TreeItem_GetImageIndex ( hWndTV, ItemHandle , @iUnSel , @iSelectedImage ) --> Return { iUnSel , iSelectedImage }
HB_FUNC( TREEITEM_GETIMAGEINDEX )
{
   HWND        hWndTV = hmg_par_raw_HWND( 1 );
   HTREEITEM   ItemHandle = hmg_par_raw_TREEITEM( 2 );

   INT         iUnSel;
   INT         iSelectedImage;
   TV_ITEM     TreeItem;
   TreeItem.mask = TVIF_IMAGE | TVIF_SELECTEDIMAGE;
   TreeItem.hItem = ItemHandle;
   TreeItem.iImage = 0;
   TreeItem.iSelectedImage = 0;
   TreeView_GetItem( hWndTV, &TreeItem );

   iUnSel = TreeItem.iImage;
   iSelectedImage = TreeItem.iSelectedImage;

   if( HB_ISBYREF( 3 ) )
   {
      hb_storni( iUnSel, 3 );
   }

   if( HB_ISBYREF( 4 ) )
   {
      hb_storni( iSelectedImage, 4 );
   }

   hb_reta( 2 );
   HB_STORNI( iUnSel, -1, 1 );
   HB_STORNI( iSelectedImage, -1, 2 );
}

// Function to set the image index of a TreeView item
HB_FUNC( TREEITEM_SETIMAGEINDEX )
{
   HWND        TreeHandle = hmg_par_raw_HWND( 1 );       // Get the TreeView handle
   HTREEITEM   ItemHandle = hmg_par_raw_TREEITEM( 2 );   // Get the item handle
   TV_ITEM     TreeItem;

   TreeItem.mask = TVIF_IMAGE | TVIF_SELECTEDIMAGE;      // Specify the item images
   TreeItem.hItem = ItemHandle;
   TreeItem.iImage = hb_parni( 3 );                   // Set the normal image index
   TreeItem.iSelectedImage = hb_parni( 4 );           // Set the selected image index
   TreeView_SetItem( TreeHandle, &TreeItem );         // Update the item with new images
}

// Function to get the ID of the currently selected item in a TreeView
HB_FUNC( TREEVIEW_GETSELECTIONID )
{
   HWND                       TreeHandle;
   HTREEITEM                  ItemHandle;
   TV_ITEM                    TreeItem;
   HMG_StructTreeItemLPARAM   *TreeItemLPARAM;

   TreeHandle = hmg_par_raw_HWND( 1 );                // Get the TreeView handle
   ItemHandle = TreeView_GetSelection( TreeHandle );  // Get the selected item
   if( ItemHandle != NULL )
   {
      TreeItem.mask = TVIF_PARAM;                     // Retrieve the custom parameter
      TreeItem.hItem = ItemHandle;
      TreeItem.lParam = ( LPARAM ) 0;

      TreeView_GetItem( TreeHandle, &TreeItem );      // Get item data
      TreeItemLPARAM = ( HMG_StructTreeItemLPARAM * ) TreeItem.lParam;
      hmg_ret_LONG( TreeItemLPARAM->nID );            // Return the item's ID
   }
}

// Function to get the next sibling of a specified TreeView item
HB_FUNC( TREEVIEW_GETNEXTSIBLING )
{
   HWND        TreeHandle = hmg_par_raw_HWND( 1 );    // Get the TreeView handle
   HTREEITEM   ItemHandle = hmg_par_raw_TREEITEM( 2 );                  // Get the current item handle
   HTREEITEM   NextItemHandle;

   NextItemHandle = TreeView_GetNextSibling( TreeHandle, ItemHandle );  // Retrieve next sibling
   hmg_ret_raw_HANDLE( NextItemHandle );                 // Return the next sibling's handle
}

// Function to get the first child of a specified TreeView item
HB_FUNC( TREEVIEW_GETCHILD )
{
   HWND        TreeHandle = hmg_par_raw_HWND( 1 );       // Get the TreeView handle
   HTREEITEM   ItemHandle = hmg_par_raw_TREEITEM( 2 );   // Get the parent item handle
   HTREEITEM   ChildItemHandle;

   ChildItemHandle = TreeView_GetChild( TreeHandle, ItemHandle ); // Retrieve the first child
   hmg_ret_raw_HANDLE( ChildItemHandle );                // Return the child's handle
}

// Function to get the parent item of a specified TreeView item
HB_FUNC( TREEVIEW_GETPARENT )
{
   HWND        TreeHandle = hmg_par_raw_HWND( 1 );       // Get the TreeView handle
   HTREEITEM   ItemHandle = hmg_par_raw_TREEITEM( 2 );   // Get the child item handle
   HTREEITEM   ParentItemHandle;

   ParentItemHandle = TreeView_GetParent( TreeHandle, ItemHandle );  // Retrieve the parent item
   hmg_ret_raw_HANDLE( ParentItemHandle );               // Return the parent's handle
}

//**************************************************
//    by  Dr. Claudio Soto  (November 2013)
//**************************************************
// Function to retrieve the state of a TreeView item
HB_FUNC( TREEVIEW_GETITEMSTATE )
{
   HWND        hWndTV = hmg_par_raw_HWND( 1 );           // Get the TreeView handle
   HTREEITEM   ItemHandle = hmg_par_raw_TREEITEM( 2 );   // Get the item handle
   UINT        StateMask = hmg_par_UINT( 3 );            // Get the state mask to check
   hmg_ret_UINT( TreeView_GetItemState( hWndTV, ItemHandle, StateMask ) ); // Return item state
}

// Helper function to check if a TreeView item has child nodes
BOOL TreeView_IsNode( HWND hWndTV, HTREEITEM ItemHandle )
{
   if( TreeView_GetChild( hWndTV, ItemHandle ) != NULL )
   {
      return TRUE;   // Returns TRUE if the item has at least one child
   }
   else
   {
      return FALSE;  // Returns FALSE if the item has no children
   }
}

//--------------------------------------------------------------------------------------------------------
//   TreeView_ExpandChildrenRecursive ( hWndTV, ItemHandle, nExpand, fRecurse )
//--------------------------------------------------------------------------------------------------------
void TreeView_ExpandChildrenRecursive( HWND hWndTV, HTREEITEM ItemHandle, UINT nExpand )
{
   HTREEITEM   ChildItem;
   HTREEITEM   NextItem;

   // Check if the given item is a valid TreeView node
   if( TreeView_IsNode( hWndTV, ItemHandle ) )
   {
      // Expand or collapse the current item
      TreeView_Expand( hWndTV, ItemHandle, nExpand );
      // Get the first child item of the current node
      ChildItem = TreeView_GetChild( hWndTV, ItemHandle );

      // Iterate over all sibling child items
      while( ChildItem != NULL )
      {
         // Recursively expand or collapse the child items
         TreeView_ExpandChildrenRecursive( hWndTV, ChildItem, nExpand );

         // Get the next sibling of the current child item
         NextItem = TreeView_GetNextSibling( hWndTV, ChildItem );
         ChildItem = NextItem;
      }
   }
}

// Function: TREEVIEW_EXPANDCHILDRENRECURSIVE
// HMG function to expand/collapse TreeView items with optional recursion
HB_FUNC( TREEVIEW_EXPANDCHILDRENRECURSIVE )
{
   HWND        hWndTV = hmg_par_raw_HWND( 1 );
   HTREEITEM   ItemHandle = hmg_par_raw_TREEITEM( 2 );
   UINT        nExpand = hmg_par_UINT( 3 );
   BOOL        fRecurse = ( BOOL ) hb_parl( 4 );
   HWND        hWndParent = GetParent( hWndTV );
   BOOL        lEnabled = IsWindowEnabled( hWndParent );

   if( fRecurse == FALSE )
   {
      // Expand or collapse only the specified item
      TreeView_Expand( hWndTV, ItemHandle, nExpand );
   }
   else
   {
      // Temporarily disable the parent window to avoid flickering or user interaction
      EnableWindow( hWndParent, FALSE );

      // Recursively expand/collapse all children of the specified item
      TreeView_ExpandChildrenRecursive( hWndTV, ItemHandle, nExpand );

      // Restore the enabled state of the parent window
      if( lEnabled == TRUE )
      {
         EnableWindow( hWndParent, TRUE );
      }
   }
}

//---------------------------------------------------------------------------------------------------------------------
#define SORTTREENODE_FIRST 0
#define SORTTREENODE_LAST  1
#define SORTTREENODE_MIX   2

typedef struct
{
   HWND  hWndTV;
   BOOL  CaseSensitive;
   BOOL  AscendingOrder;
   int   NodePosition;
} HMG_StructTreeViewCompareInfo;

// Function: TreeViewCompareFunc
// Custom comparison function for sorting TreeView nodes
// Parameters:
//   LPARAM lParam1 - Item data for the first node
//   LPARAM lParam2 - Item data for the second node
//   LPARAM lParamSort - Additional sorting information
int CALLBACK TreeViewCompareFunc( LPARAM lParam1, LPARAM lParam2, LPARAM lParamSort )
{
   // Extract TreeView item data
   HMG_StructTreeItemLPARAM      *TreeItemLPARAM1 = ( HMG_StructTreeItemLPARAM * ) lParam1;
   HMG_StructTreeItemLPARAM      *TreeItemLPARAM2 = ( HMG_StructTreeItemLPARAM * ) lParam2;

   HMG_StructTreeViewCompareInfo *TreeViewCompareInfo = ( HMG_StructTreeViewCompareInfo * ) lParamSort;

   HWND                          hWndTV = TreeViewCompareInfo->hWndTV;

   HTREEITEM                     ItemHandle1 = ( HTREEITEM ) TreeItemLPARAM1->ItemHandle;
   HTREEITEM                     ItemHandle2 = ( HTREEITEM ) TreeItemLPARAM2->ItemHandle;

   BOOL                          IsTreeNode1;
   BOOL                          IsTreeNode2;
   int                           CmpValue;

   TCHAR                         ItemText1[MAX_ITEM_TEXT];
   TV_ITEM                       TV_Item1;
   TCHAR                         ItemText2[MAX_ITEM_TEXT];
   TV_ITEM                       TV_Item2;

   TV_Item1.mask = TVIF_TEXT;
   TV_Item1.pszText = ItemText1;
   TV_Item1.cchTextMax = sizeof( ItemText1 ) / sizeof( TCHAR );
   TV_Item1.hItem = ( HTREEITEM ) ItemHandle1;
   TreeView_GetItem( hWndTV, &TV_Item1 );

   TV_Item2.mask = TVIF_TEXT;
   TV_Item2.pszText = ItemText2;
   TV_Item2.cchTextMax = sizeof( ItemText2 ) / sizeof( TCHAR );
   TV_Item2.hItem = ( HTREEITEM ) ItemHandle2;
   TreeView_GetItem( hWndTV, &TV_Item2 );

   IsTreeNode1 = ( TreeItemLPARAM1->IsNodeFlag == TRUE || TreeView_GetChild( hWndTV, ItemHandle1 ) != NULL ) ? TRUE : FALSE;
   IsTreeNode2 = ( TreeItemLPARAM2->IsNodeFlag == TRUE || TreeView_GetChild( hWndTV, ItemHandle2 ) != NULL ) ? TRUE : FALSE;

   if( TreeViewCompareInfo->CaseSensitive == FALSE )
   {
      CmpValue = lstrcmpi( ItemText1, ItemText2 );
   }
   else
   {
      CmpValue = lstrcmp( ItemText1, ItemText2 );
   }

   if( TreeViewCompareInfo->AscendingOrder == FALSE )
   {
      CmpValue = CmpValue * ( -1 );
   }

   // Handle special node position sorting
   if( TreeViewCompareInfo->NodePosition == SORTTREENODE_FIRST )
   {
      if( IsTreeNode1 && !IsTreeNode2 ) return -1; // Nodes first
      if( !IsTreeNode1 && IsTreeNode2 ) return +1; // Non-nodes after

   }

   if( TreeViewCompareInfo->NodePosition == SORTTREENODE_LAST )
   {
      if( IsTreeNode1 && !IsTreeNode2 ) return +1; // Nodes last
      if( !IsTreeNode1 && IsTreeNode2 ) return -1; // Non-nodes first

   }

   return CmpValue;
}

void TreeView_SortChildrenRecursiveCB( HWND hWndTV, TVSORTCB TVSortCB )
{
   HTREEITEM   ChildItem;
   HTREEITEM   NextItem;

   if( TreeView_IsNode( hWndTV, TVSortCB.hParent ) )
   {
      TreeView_SortChildrenCB( hWndTV, &TVSortCB, 0 );
      ChildItem = TreeView_GetChild( hWndTV, TVSortCB.hParent );

      while( ChildItem != NULL )
      {
         TVSortCB.hParent = ( HTREEITEM ) ChildItem;
         TreeView_SortChildrenRecursiveCB( hWndTV, TVSortCB );

         NextItem = TreeView_GetNextSibling( hWndTV, ChildItem );
         ChildItem = NextItem;
      }
   }
}

//---------------------------------------------------------------------------------------------------------------------
// TreeView_SortChildrenRecursiveCB ( hWndTV, ItemHandle, fRecurse, lCaseSensitive, lAscendingOrder, nNodePosition )
//---------------------------------------------------------------------------------------------------------------------
HB_FUNC( TREEVIEW_SORTCHILDRENRECURSIVECB )
{
   HWND                          hWndTV = hmg_par_raw_HWND( 1 );
   HTREEITEM                     ItemHandle = hmg_par_raw_TREEITEM( 2 );
   BOOL                          fRecurse = ( BOOL ) hb_parl( 3 );
   BOOL                          lCaseSensitive = ( BOOL ) hb_parl( 4 );
   BOOL                          lAscendingOrder = ( BOOL ) hb_parl( 5 );
   INT                           nNodePosition = hmg_par_INT( 6 );
   HWND                          hWndParent = GetParent( hWndTV );
   BOOL                          lEnabled = IsWindowEnabled( hWndParent );

   TVSORTCB                      TVSortCB;
   HMG_StructTreeViewCompareInfo TreeViewCompareInfo;

   TreeViewCompareInfo.hWndTV = hWndTV;
   TreeViewCompareInfo.CaseSensitive = lCaseSensitive;
   TreeViewCompareInfo.AscendingOrder = lAscendingOrder;
   TreeViewCompareInfo.NodePosition = nNodePosition;

   TVSortCB.hParent = ( HTREEITEM ) ItemHandle;
   TVSortCB.lpfnCompare = ( PFNTVCOMPARE ) TreeViewCompareFunc;
   TVSortCB.lParam = ( LPARAM ) & TreeViewCompareInfo;

   if( fRecurse == FALSE )
   {
      TreeView_SortChildrenCB( hWndTV, &TVSortCB, 0 );
   }
   else
   {
      EnableWindow( hWndParent, FALSE );

      TreeView_SortChildrenRecursiveCB( hWndTV, TVSortCB );

      if( lEnabled == TRUE )
      {
         EnableWindow( hWndParent, TRUE );
      }
   }
}

HB_FUNC( TREEVIEW_GETROOT )
{
   HTREEITEM   RootItemHandle = TreeView_GetRoot( hmg_par_raw_HWND( 1 ) );

   hmg_ret_raw_HANDLE( RootItemHandle );
}

// Function: TREEITEM_GETID
// Retrieves the ID associated with a TreeView item
HB_FUNC( TREEITEM_GETID )
{
   HWND        hWndTV = hmg_par_raw_HWND( 1 );
   HTREEITEM   ItemHandle = hmg_par_raw_TREEITEM( 2 );

   TV_ITEM     TreeItem;

   TreeItem.mask = TVIF_PARAM;
   TreeItem.hItem = ItemHandle;
   TreeItem.lParam = ( LPARAM ) 0;

   if( TreeView_GetItem( hWndTV, &TreeItem ) == TRUE )
   {
      HMG_StructTreeItemLPARAM   *TreeItemLPARAM = ( HMG_StructTreeItemLPARAM * ) TreeItem.lParam;
      hmg_ret_LONG( TreeItemLPARAM->nID );
   }
}

// Function: TREEITEM_SETNODEFLAG
// Sets the IsNodeFlag for a TreeView item
HB_FUNC( TREEITEM_SETNODEFLAG )
{
   HWND                       hWndTV = hmg_par_raw_HWND( 1 );
   HTREEITEM                  ItemHandle = hmg_par_raw_TREEITEM( 2 );
   BOOL                       IsNodeFlag = ( BOOL ) hb_parl( 3 );

   HMG_StructTreeItemLPARAM   *TreeItemLPARAM;
   TV_ITEM                    TreeItem;

   TreeItem.mask = TVIF_PARAM;
   TreeItem.hItem = ItemHandle;
   TreeItem.lParam = ( LPARAM ) 0;
   TreeView_GetItem( hWndTV, &TreeItem );

   // Update the IsNodeFlag in the lParam structure
   TreeItemLPARAM = ( HMG_StructTreeItemLPARAM * ) TreeItem.lParam;
   TreeItemLPARAM->IsNodeFlag = IsNodeFlag;
   TreeItem.lParam = ( LPARAM ) TreeItemLPARAM;
   TreeView_SetItem( hWndTV, &TreeItem );
}

// Function: TREEITEM_GETNODEFLAG
// Retrieves the IsNodeFlag for a TreeView item
HB_FUNC( TREEITEM_GETNODEFLAG )
{
   HWND                       hWndTV = hmg_par_raw_HWND( 1 );
   HTREEITEM                  ItemHandle = hmg_par_raw_TREEITEM( 2 );

   HMG_StructTreeItemLPARAM   *TreeItemLPARAM;
   TV_ITEM                    TreeItem;

   TreeItem.mask = TVIF_PARAM;
   TreeItem.hItem = ItemHandle;
   TreeItem.lParam = ( LPARAM ) 0;

   TreeView_GetItem( hWndTV, &TreeItem );

   TreeItemLPARAM = ( HMG_StructTreeItemLPARAM * ) TreeItem.lParam;
   hb_retl( TreeItemLPARAM->IsNodeFlag );
}

//        TreeView_SetImageList ( hWnd , hImageList , [iImageList] )
HB_FUNC( TREEVIEW_SETIMAGELIST )
{
   HWND        hWnd = hmg_par_raw_HWND( 1 );
   HIMAGELIST  hImageList = hmg_par_raw_HIMAGELIST( 2 );
   int         iImageList = HB_ISNIL( 3 ) ? TVSIL_NORMAL : hb_parni( 3 );
   HIMAGELIST  hImageListPrevious = TreeView_SetImageList( hWnd, hImageList, iImageList );
   hmg_ret_raw_HANDLE( hImageListPrevious );
}

//        TreeView_GetImageList ( hWnd , [iImageList] ) --> hImageList
HB_FUNC( TREEVIEW_GETIMAGELIST )
{
   HWND        hWnd = hmg_par_raw_HWND( 1 );
   int         iImageList = HB_ISNIL( 2 ) ? TVSIL_NORMAL : hb_parni( 2 );
   HIMAGELIST  hImageList = TreeView_GetImageList( hWnd, iImageList );
   hmg_ret_raw_HANDLE( hImageList );
}

HB_FUNC( TREEVIEW_SETHASBUTTON )
{
   HWND        hWndTV = hmg_par_raw_HWND( 1 );
   HTREEITEM   ItemHandle = hmg_par_raw_TREEITEM( 2 );
   BOOL        lHasButton = ( BOOL ) hb_parl( 3 );

   TV_ITEM     TreeItem;
   TreeItem.mask = TVIF_CHILDREN;
   TreeItem.hItem = ItemHandle;
   TreeItem.cChildren = lHasButton ? 1 : 0;
   TreeView_SetItem( hWndTV, &TreeItem );
}

HB_FUNC( TREEVIEW_GETHASBUTTON )
{
   HWND        hWndTV = hmg_par_raw_HWND( 1 );
   HTREEITEM   ItemHandle = hmg_par_raw_TREEITEM( 2 );

   TV_ITEM     TreeItem;
   TreeItem.mask = TVIF_CHILDREN;
   TreeItem.hItem = ItemHandle;
   TreeView_GetItem( hWndTV, &TreeItem );
   hmg_ret_L( TreeItem.cChildren == 0 );
}
