/*----------------------------------------------------------------------------
   MINIGUI - Harbour Win32 GUI library source code

   Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
   http://harbourminigui.googlepages.com/

   IMAGELIST control source code
   (C)2005 Janusz Pora <januszpora@onet.eu>

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
#include <mgdefs.h>

#include <commctrl.h>

#if ( defined( __BORLANDC__ ) && __BORLANDC__ <= 1410 )
WINCOMMCTRLAPI void WINAPI ImageList_EndDrag( void );
#endif
extern HBITMAP             HMG_LoadImage( const char *FileName );
extern HBITMAP             HMG_LoadPicture
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

#ifdef UNICODE
LPWSTR                     AnsiToWide( LPCSTR );
#endif
HINSTANCE                  GetInstance( void );
HINSTANCE                  GetResources( void );

// Minigui Resources control system
void                       RegisterResource( HANDLE hResource, LPCSTR szType );

/*
 * FUNCTION: INITIMAGELIST( cx, cy, mask, nCount )
 *
 * Creates and initializes a new image list with specified dimensions and characteristics.
 *
 * Parameters:
 *   cx      : (Integer) Width of each image in the list, in pixels.
 *   cy      : (Integer) Height of each image in the list, in pixels.
 *   mask    : (Logical) If .T., enables mask support (transparency).  If .F., no mask is used.
 *   nCount  : (Integer) Initial number of images the list can store.  This is a hint to the system for memory allocation.
 *
 * Returns:
 *   A handle to the newly created HIMAGELIST object, returned via hmg_ret_raw_HANDLE().  Returns NULL on failure.
 *
 * Purpose:
 *   Image lists are required for associating collections of images with controls such as
 *   ListView, TreeView, or custom GUI components. This function prepares such a list with
 *   desired characteristics (like transparency and initial capacity).
 *   Example Usage:
 *     hImageList := InitImageList( 16, 16, .T., 10 )  // Creates an image list for 16x16 icons with transparency, initially holding 10 images.
 *
 * Notes:
 *   - The function registers the created HIMAGELIST with the internal HMG resource system, allowing it to be managed by HMG.
 *   - Uses ILC_COLOR32 by default for 32-bit color support, providing better color fidelity.
 *   - The HIMAGELIST handle must be released using DestroyImageList when it is no longer needed to prevent memory leaks.
 */
HB_FUNC( INITIMAGELIST )
{
   HIMAGELIST  himlIcons;
   UINT        nFlags = ILC_COLOR32;

   if( hb_parl( 3 ) )
   {
      nFlags |= ILC_MASK;
   }

   InitCommonControls();

   himlIcons = ImageList_Create( hb_parni( 1 ), hb_parni( 2 ), nFlags, hb_parni( 4 ), 0 );

   RegisterResource( himlIcons, "IMAGELIST" );
   hmg_ret_raw_HANDLE( himlIcons );
}

/*
 * FUNCTION: IL_ADD( himl, image, maskimage, ix, iy, imagecount )
 *
 * Adds an image or a strip of images to an image list. Optionally includes a mask image for transparency.
 *
 * Parameters:
 *   himl       : (HIMAGELIST) Handle to the target image list.
 *   image      : (String) Path or resource name of the image to load.  Can be a file path or a resource identifier.
 *   maskimage  : (String) Path or resource name of the mask image (optional).  If provided, this image is used as the transparency mask.
 *   ix         : (Integer) Width of a single image within the image or image strip, in pixels.
 *   iy         : (Integer) Height of a single image within the image or image strip, in pixels.
 *   imagecount : (Integer) Number of images in the image strip (optional, default = 1).  Used when adding a strip of images from a single bitmap.
 *
 * Returns:
 *   Index of the image added in the list (Integer), or -1 on failure.  The index is zero-based.
 *
 * Purpose:
 *   Allows developers to add images (and their corresponding transparency masks) to an
 *   image list. Supports image strips to efficiently add multiple images at once, which is useful for animations or icon sets.
 *   Example Usage:
 *     IL_ADD( hImageList, "icon1.bmp", "icon1_mask.bmp", 16, 16 )  // Adds a 16x16 icon with a separate mask.
 *     IL_ADD( hImageList, "icons.bmp", "", 32, 32, 4 )  // Adds a strip of 4 32x32 icons from a single bitmap.
 *
 * Notes:
 *   - Supports loading from application resources or external files.  It first attempts to load from resources, then from the file system.
 *   - Ensures image dimensions match expectations before adding to avoid inconsistencies.  The width of the image must be a multiple of ix if imagecount > 1.
 *   - The function automatically deletes the loaded HBITMAP objects to prevent memory leaks.
 */
HB_FUNC( IL_ADD )
{
#ifndef UNICODE
   LPCSTR   lpImageName = hb_parc( 2 );
   LPCSTR   lpImageName2 = hb_parc( 3 );
#else
   LPWSTR   lpImageName = AnsiToWide( ( char * ) hb_parc( 2 ) );
   LPWSTR   lpImageName2 = AnsiToWide( ( char * ) hb_parc( 3 ) );
#endif
   BITMAP   bm;
   HBITMAP  himage1;
   HBITMAP  himage2;
   int      lResult = -1;
   int      ic = 1;

   if( hb_parni( 6 ) )
   {
      ic = hb_parni( 6 );
   }

   // Attempt to load primary image from resources or file system
   himage1 = ( HBITMAP ) LoadImage( GetResources(), lpImageName, IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT );
   if( himage1 == NULL )
   {
      himage1 = ( HBITMAP ) LoadImage( NULL, lpImageName, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT );
   }

   if( himage1 == NULL )
   {
      himage1 = ( HBITMAP ) HMG_LoadImage( hb_parc( 2 ) );
   }

   // Load mask image if provided
   himage2 = 0;
   if( hb_parclen( 3 ) )
   {
      himage2 = ( HBITMAP ) LoadImage( GetResources(), lpImageName2, IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT );
      if( himage2 == NULL )
      {
         himage2 = ( HBITMAP ) LoadImage( NULL, lpImageName2, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT );
      }

      if( himage2 == NULL )
      {
         himage2 = ( HBITMAP ) HMG_LoadImage( hb_parc( 3 ) );
      }
   }

#ifdef UNICODE
   hb_xfree( lpImageName );
   hb_xfree( lpImageName2 );
#endif
   if( GetObject( himage1, sizeof( BITMAP ), &bm ) != 0 )
   {
      if( ( hb_parni( 4 ) * ic == bm.bmWidth ) & ( hb_parni( 5 ) == bm.bmHeight ) )
      {
         lResult = ImageList_Add( hmg_par_raw_HIMAGELIST( 1 ), himage1, himage2 );
      }

      DeleteObject( himage1 );
      if( himage2 )
      {
         DeleteObject( himage2 );
      }
   }

   hmg_ret_NINT( lResult );
}

/*
 * FUNCTION: IL_ADDMASKED( himl, image, color, ix, iy, imagecount )
 *
 * Adds a masked image (with specified transparent color) to an image list.
 *
 * Parameters:
 *   himl       : (HIMAGELIST) Handle to the target image list.
 *   image      : (String) Path or resource name of the image. Can be a file path or a resource identifier.
 *   color      : (Integer) RGB color value used as transparency mask.  Pixels of this color in the image will be treated as transparent.
 *   ix         : (Integer) Width of each image, in pixels.
 *   iy         : (Integer) Height of each image, in pixels.
 *   imagecount : (Integer) Number of images in the image strip (optional, default = 1). Used when adding a strip of images from a single bitmap.
 *
 * Returns:
 *   Index of the image added in the list (Integer), or -1 on failure. The index is zero-based.
 *
 * Purpose:
 *   Provides a simpler interface for adding images with transparency based on a
 *   specific background color. Suitable for non-alpha transparency (e.g., legacy icons).
 *   Example Usage:
 *     IL_ADDMASKED( hImageList, "icon.bmp", RGB(255, 0, 255), 16, 16 )  // Adds a 16x16 icon, treating magenta (RGB(255,0,255)) as transparent.
 *
 * Notes:
 *   - Supports loading from application resources or external files. It first attempts to load from resources, then from the file system.
 *   - Ensures image dimensions match expectations before adding to avoid inconsistencies. The width of the image must be a multiple of ix if imagecount > 1.
 *   - The function automatically deletes the loaded HBITMAP object to prevent memory leaks.
 */
HB_FUNC( IL_ADDMASKED )
{
#ifndef UNICODE
   LPCSTR   lpImageName = hb_parc( 2 );
#else
   LPWSTR   lpImageName = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif
   BITMAP   bm;
   HBITMAP  himage1;
   COLORREF clrBk = CLR_NONE;
   int      lResult = -1;
   int      ic = 1;

   if( hb_parnl( 3 ) )
   {
      clrBk = hmg_par_COLORREF( 3 );
   }

   if( hb_parni( 6 ) )
   {
      ic = hb_parni( 6 );
   }

   himage1 = ( HBITMAP ) LoadImage( GetResources(), lpImageName, IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT );
   if( himage1 == NULL )
   {
      himage1 = ( HBITMAP ) LoadImage( NULL, lpImageName, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT );
   }

   if( himage1 == NULL )
   {
      himage1 = ( HBITMAP ) HMG_LoadPicture( hb_parc( 2 ), -1, -1, NULL, 0, 1, -1, 0, HB_FALSE, 255 );
   }

#ifdef UNICODE
   hb_xfree( lpImageName );
#endif
   if( GetObject( himage1, sizeof( BITMAP ), &bm ) != 0 )
   {
      if( ( hb_parni( 4 ) * ic == bm.bmWidth ) & ( hb_parni( 5 ) == bm.bmHeight ) )
      {
         lResult = ImageList_AddMasked( hmg_par_raw_HIMAGELIST( 1 ), himage1, clrBk );
      }

      DeleteObject( himage1 );
   }

   hmg_ret_NINT( lResult );
}

/*
 * FUNCTION: IL_DRAW( hwnd, himl, imageindex, cx, cy )
 *
 * Draws an image from an image list at specified coordinates on a window.
 *
 * Parameters:
 *   hwnd        : (HWND) Handle to the window where image should be drawn.
 *   himl        : (HIMAGELIST) Handle to the image list.
 *   imageindex  : (Integer) Index of the image within the list (zero-based).
 *   cx          : (Integer) X coordinate (horizontal position) to draw the image, relative to the window's client area.
 *   cy          : (Integer) Y coordinate (vertical position) to draw the image, relative to the window's client area.
 *
 * Returns:
 *   .T. if the image was successfully drawn, .F. otherwise.
 *
 * Purpose:
 *   Provides a mechanism to render an image from an image list directly onto a
 *   window's device context. Useful for custom painting scenarios where you need to
 *   draw images at specific locations within a window.
 *   Example Usage:
 *     IL_DRAW( hWnd, hImageList, 0, 10, 10 )  // Draws the first image in the list at coordinates (10, 10) on the window.
 *
 * Notes:
 *   - The function uses ILD_TRANSPARENT drawing style, which respects the transparency mask of the image.
 *   - It obtains the device context (HDC) of the window, draws the image, and then releases the device context.
 *   - Ensure that the image index is within the valid range of the image list.
 */
HB_FUNC( IL_DRAW )
{
   HDC   hdc;
   HWND  hwnd = hmg_par_raw_HWND( 1 );

   if( ( hdc = GetDC( hwnd ) ) == NULL )
   {
      hb_retl( HB_FALSE );
   }

   hmg_ret_L( ImageList_Draw( hmg_par_raw_HIMAGELIST( 2 ), hmg_par_INT( 3 ), hdc, hb_parni( 4 ), hb_parni( 5 ), ILD_TRANSPARENT ) );

   ReleaseDC( hwnd, hdc );
}

/*
 * FUNCTION: IL_REMOVE( himl, imageindex )
 *
 * Removes an image from the image list by index.
 *
 * Parameters:
 *   himl        : (HIMAGELIST) Handle to the image list.
 *   imageindex  : (Integer) Index of the image to remove (zero-based).
 *
 * Returns:
 *   .T. on success, .F. on failure.
 *
 * Purpose:
 *   Removes an image from the image list when it is no longer needed, freeing memory and resources.
 *   This is important for managing memory usage, especially when dealing with large image lists or dynamic image updates.
 *   Example Usage:
 *     IL_REMOVE( hImageList, 2 )  // Removes the third image from the list.
 *
 * Notes:
 *   - Removing an image shifts the indices of subsequent images in the list.
 *   - It's crucial to update any references to image indices after removing an image to avoid drawing the wrong image or causing errors.
 */
HB_FUNC( IL_REMOVE )
{
   hb_retl( ImageList_Remove( hmg_par_raw_HIMAGELIST( 1 ), hmg_par_INT( 2 ) ) );
}

/*
 * FUNCTION: IL_SETBKCOLOR( himl, color )
 *
 * Sets the background color for the image list.
 *
 * Parameters:
 *   himl  : (HIMAGELIST) Handle to the image list.
 *   color : (Integer) RGB color to set as background. If not passed, CLR_NONE is used, which means no background color.
 *           Use RGB() macro to create the color value.
 *
 * Returns:
 *   The previous background color as COLORREF.
 *
 * Purpose:
 *   Configures the background color used when drawing transparent or masked images.
 *   This allows you to control how the transparent areas of the images are rendered.
 *   Example Usage:
 *     IL_SETBKCOLOR( hImageList, RGB(255, 255, 255) )  // Sets the background color to white.
 *
 * Notes:
 *   - If no color is specified, the background color is set to CLR_NONE, which means the images will be drawn without a background.
 *   - The background color is only visible in the transparent areas of the images.
 */
HB_FUNC( IL_SETBKCOLOR )
{
   hmg_ret_COLORREF( ImageList_SetBkColor( hmg_par_raw_HIMAGELIST( 1 ), HB_ISNUM( 2 ) ? hmg_par_COLORREF( 2 ) : CLR_NONE ) );
}

/*
 * FUNCTION: IL_ERASEIMAGE( hwnd, ix, iy, dx, dy )
 *
 * Invalidates a region of the window, effectively erasing the image in that area.
 *
 * Parameters:
 *   hwnd : (HWND) Handle to the target window.
 *   ix   : (Integer) X coordinate of the region to erase, relative to the window's client area.
 *   iy   : (Integer) Y coordinate of the region to erase, relative to the window's client area.
 *   dx   : (Integer) Width of the region to erase.
 *   dy   : (Integer) Height of the region to erase.
 *
 * Returns:
 *   None.
 *
 * Purpose:
 *   Used to erase or clear an image from the screen by invalidating its area and forcing
 *   a redraw. This is useful when you need to remove an image that was previously drawn
 *   using IL_DRAW or a similar function.
 *   Example Usage:
 *     IL_ERASEIMAGE( hWnd, 10, 10, 16, 16 )  // Erases a 16x16 region at coordinates (10, 10) on the window.
 *
 * Notes:
 *   - The function invalidates the specified rectangle, which causes the window to be redrawn.
 *   - The HB_TRUE parameter in InvalidateRect indicates that the background should be erased when the window is redrawn.
 *   - UpdateWindow forces an immediate redraw of the invalidated region.
 */
HB_FUNC( IL_ERASEIMAGE )
{
   HWND  hwnd = hmg_par_raw_HWND( 1 );
   RECT  rcImage;

   SetRect( &rcImage, hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ) + hb_parni( 2 ), hb_parni( 5 ) + hb_parni( 3 ) );

   InvalidateRect( hwnd, &rcImage, HB_TRUE );
   UpdateWindow( hwnd );
}

/*
 * FUNCTION: IL_BEGINDRAG( hwnd, himl, imageIndex, ix, iy )
 *
 * Begins a drag-and-drop operation for an image in the image list.
 *
 * Parameters:
 *   hwnd       : (HWND) Handle to the window where the drag operation will occur.
 *   himl       : (HIMAGELIST) Handle to the image list.
 *   imageIndex : (Integer) Index of the image to drag (zero-based).
 *   ix         : (Integer) X offset of the cursor from the upper-left corner of the image during the drag operation.
 *   iy         : (Integer) Y offset of the cursor from the upper-left corner of the image during the drag operation.
 *
 * Returns:
 *   A logical value indicating success or failure of the drag initialization operation. Returns .T. on success, .F. on failure.
 *
 * Purpose:
 *   Used in custom drag-and-drop implementations using MiniGUI. This function initializes the drag operation,
 *   preparing the image to be moved around the screen.
 *   Example Usage:
 *     IL_BEGINDRAG( hWnd, hImageList, 0, 5, 5 )  // Starts dragging the first image in the list, with the cursor offset by (5, 5) pixels.
 *
 * Notes:
 *   - The function invalidates the area around the image to ensure that the old image is erased before the drag operation begins.
 *   - The ImageList_BeginDrag function creates a temporary image that follows the cursor during the drag operation.
 *   - You must call IL_ENDDRAG to clean up resources and end the drag operation.
 */
HB_FUNC( IL_BEGINDRAG )
{
   int   cx, cy;
   HWND  hwnd = hmg_par_raw_HWND( 1 );
   RECT  rcImage;

   if( ImageList_GetIconSize( hmg_par_raw_HIMAGELIST( 2 ), &cx, &cy ) )
   {
      SetRect( &rcImage, hb_parni( 4 ) - 2, hb_parni( 5 ) - 2, hb_parni( 4 ) + cx + 2, hb_parni( 5 ) + cy + 2 );
      InvalidateRect( hwnd, &rcImage, HB_TRUE );
      UpdateWindow( hwnd );
   }

   hb_retl( ImageList_BeginDrag( hmg_par_raw_HIMAGELIST( 2 ), hb_parni( 3 ), 0, 0 ) );
}

/*
 * FUNCTION: IL_DRAGMOVE( ix, iy )
 *
 * Moves the image during a drag-and-drop operation.
 *
 * Parameters:
 *   ix : (Integer) New X position of the image, relative to the screen.
 *   iy : (Integer) New Y position of the image, relative to the screen.
 *
 * Returns:
 *   A logical value indicating if the move was successful. Returns .T. on success, .F. on failure.
 *
 * Purpose:
 *   This function is called repeatedly during a drag-and-drop operation to update the position of the dragged image
 *   as the user moves the mouse.
 *   Example Usage:
 *     IL_DRAGMOVE( MouseX(), MouseY() )  // Moves the dragged image to the current mouse coordinates.
 *
 * Notes:
 *   - This function should be called within a mouse move event handler during a drag operation.
 *   - The coordinates are screen coordinates, not client coordinates.
 */
HB_FUNC( IL_DRAGMOVE )
{
   hb_retl( ImageList_DragMove( hb_parni( 1 ), hb_parni( 2 ) ) );
}

/*
 * FUNCTION: IL_DRAGENTER( hwnd, ix, iy )
 *
 * Begins a drag operation within a window.
 *
 * Parameters:
 *   hwnd : (HWND) Handle to the target window.
 *   ix   : (Integer) X coordinate of the cursor, relative to the screen.
 *   iy   : (Integer) Y coordinate of the cursor, relative to the screen.
 *
 * Returns:
 *   A logical value indicating success. Returns .T. on success, .F. on failure.
 *
 * Purpose:
 *   This function is called when the mouse enters a window during a drag-and-drop operation.
 *   It prepares the window to receive the dragged image.
 *   Example Usage:
 *     IL_DRAGENTER( hWnd, MouseX(), MouseY() )  // Indicates that the dragged image has entered the window.
 *
 * Notes:
 *   - This function should be called within a WM_MOUSEENTER or similar event handler.
 *   - The coordinates are screen coordinates, not client coordinates.
 */
HB_FUNC( IL_DRAGENTER )
{
   hb_retl( ImageList_DragEnter( hmg_par_raw_HWND( 1 ), hb_parni( 2 ), hb_parni( 3 ) ) );
}

/*
 * FUNCTION: IL_ENDDRAG( hwnd )
 *
 * Ends a drag-and-drop operation.
 *
 * Parameters:
 *   hwnd : (HWND) Handle to the target window.
 *
 * Returns:
 *   None.
 *
 * Purpose:
 *   Cleans up resources and removes the drag image from the screen, ending the drag-and-drop operation.
 *   This function should always be called after IL_BEGINDRAG to release resources and prevent memory leaks.
 *   Example Usage:
 *     IL_ENDDRAG( hWnd )  // Ends the drag operation.
 *
 * Notes:
 *   - This function calls ImageList_EndDrag to release the resources associated with the drag image.
 *   - It also calls ImageList_DragLeave to remove the drag image from the screen.
 */
HB_FUNC( IL_ENDDRAG )
{
   ImageList_EndDrag();
   ImageList_DragLeave( hmg_par_raw_HWND( 1 ) );
}

/*
 * FUNCTION: IL_GETIMAGECOUNT( himl )
 *
 * Retrieves the number of images currently in the image list.
 *
 * Parameters:
 *   himl : (HIMAGELIST) Handle to the image list.
 *
 * Returns:
 *   A numeric value indicating the number of images in the image list.
 *
 * Purpose:
 *   This function allows you to determine the number of images in an image list, which is useful for iterating
 *   through the list or validating image indices.
 *   Example Usage:
 *     nImageCount := IL_GETIMAGECOUNT( hImageList )  // Gets the number of images in the list.
 *
 * Notes:
 *   - The returned value is the number of images currently stored in the image list.
 */
HB_FUNC( IL_GETIMAGECOUNT )
{
   hmg_ret_NINT( ImageList_GetImageCount( hmg_par_raw_HIMAGELIST( 1 ) ) );
}
