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

   Parts of this code are contributed and used here under permission of the author:
       Copyright 2005 (C) Jacek Kubica <kubica@wssk.wroc.pl>
 ---------------------------------------------------------------------------*/
#include <mgdefs.h>     // Minigui definitions
#include <shellapi.h>   // Shell API for shell functions, e.g., icon extraction
#include <commctrl.h>   // Common control functions

// Define constants for compatibility with older Borland compilers
#if ( defined( __BORLANDC__ ) && __BORLANDC__ < 1410 )
#define WC_BUTTON                      "Button" // Button class name
#define BUTTON_IMAGELIST_ALIGN_CENTER  4        // Image alignment center
#endif
#include <math.h>       // Math functions
#include "hbapiitm.h"   // Harbour API for item management
#include "hbvm.h"       // Harbour VM interaction
#ifndef BCM_FIRST       // Define BCM_FIRST for button message compatibility
#define BCM_FIRST          0x1600
#define BCM_SETIMAGELIST   ( BCM_FIRST + 0x0002 )                 // Set image list for button
#endif

// Function declarations (some for image manipulation and button customization)
static HBRUSH     CreateGradientBrush( HDC hDC, INT nWidth, INT nHeight, COLORREF Color1, COLORREF Color2 );

HBITMAP           HMG_LoadPicture
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

HIMAGELIST        HMG_SetButtonImageList( HWND hButton, const char *FileName, int Transparent, UINT uAlign );
BOOL              bmp_SaveFile( HBITMAP hBitmap, TCHAR *FileName );

LRESULT CALLBACK  OwnButtonProc( HWND hbutton, UINT msg, WPARAM wParam, LPARAM lParam );

#ifdef UNICODE
LPWSTR            AnsiToWide( LPCSTR );                           // Convert ANSI string to wide string
#endif
HINSTANCE         GetInstance( void );                            // Get instance handle
HINSTANCE         GetResources( void );                           // Get resource handle

// Minigui resource management function
void              RegisterResource( HANDLE hResource, LPCSTR szType );

// Define structure for button image list for compatibility with Borland and MinGW compilers
#if ( defined( __BORLANDC__ ) && __BORLANDC__ < 1410 ) || ( defined( __MINGW32__ ) && defined( __MINGW32_VERSION ) )
typedef struct
{
   HIMAGELIST  himl;
   RECT        margin;
   UINT        uAlign;
} BUTTON_IMAGELIST, *PBUTTON_IMAGELIST;
#endif

/*
 * FUNCTION: INITBUTTON
 *
 * Creates and initializes a basic button with configurable styles.
 *
 * Parameters:
 *   1: HWND - Handle of the parent window.
 *   2: LPCSTR/LPCWSTR - Button text.
 *   3: HMENU - Menu handle (used as control ID).
 *   4: INT - X coordinate of the button.
 *   5: INT - Y coordinate of the button.
 *   6: INT - Width of the button.
 *   7: INT - Height of the button.
 *   10: LOGICAL - Flag to set flat style.
 *   11: LOGICAL - Flag to enable tab stop.
 *   12: LOGICAL - Flag to make button visible.
 *   13: LOGICAL - Flag to enable multiline text.
 *   14: LOGICAL - Flag to set default push button style.
 *
 * Returns:
 *   HWND - Handle of the created button.
 *
 * Purpose:
 *   Creates a button with specified styles and dimensions.
 */
HB_FUNC( INITBUTTON )
{
#ifndef UNICODE
   LPCSTR   lpWindowName = hb_parc( 2 );                          // Button text (ANSI)
#else
   LPCWSTR  lpWindowName = AnsiToWide( ( char * ) hb_parc( 2 ) ); // Button text (Unicode)
#endif
   DWORD    Style = BS_NOTIFY | WS_CHILD | ( hb_parl( 14 ) ? BS_DEFPUSHBUTTON : BS_PUSHBUTTON );

   // Add optional styles based on parameters
   if( hb_parl( 10 ) )
   {
      Style |= BS_FLAT;                   // Flat style
   }

   if( !hb_parl( 11 ) )
   {
      Style |= WS_TABSTOP;                // Enable tab stop
   }

   if( !hb_parl( 12 ) )
   {
      Style |= WS_VISIBLE;                // Make button visible
   }

   if( hb_parl( 13 ) )
   {
      Style |= BS_MULTILINE;              // Multiline button text
   }

   // Create button window
   hmg_ret_raw_HWND
   (
      CreateWindowEx
         (
            0,
            WC_BUTTON,
            lpWindowName,
            Style,
            hb_parni( 4 ),                // Button position (x-coordinate)
            hb_parni( 5 ),                // Button position (y-coordinate)
            hb_parni( 6 ),                // Button width
            hb_parni( 7 ),                // Button height
            hmg_par_raw_HWND( 1 ),        // Parent window handle
            hmg_par_raw_HMENU( 3 ),       // Menu handle (used as control ID)
            GetInstance(),                // Instance handle
            NULL
         )
   );

#ifdef UNICODE
   hb_xfree( ( TCHAR * ) lpWindowName );  // Free allocated memory for Unicode text
#endif
}

/*
 * FUNCTION: INITIMAGEBUTTON
 *
 * Creates and initializes a button with an image (icon or bitmap).
 * Supports both inline and image list-based image display.
 *
 * Parameters:
 *   1: HWND - Handle of the parent window.
 *   2: LPCSTR/LPCWSTR - Button text.
 *   3: HMENU - Menu handle (used as control ID).
 *   4: INT - X coordinate of the button.
 *   5: INT - Y coordinate of the button.
 *   6: INT - Width of the button.
 *   7: INT - Height of the button.
 *   8: LPCSTR/LPCWSTR - Image file name.
 *   9: LOGICAL - Flag to set flat style.
 *   10: LOGICAL - Flag for transparency.
 *   11: LOGICAL - Flag to make button visible.
 *   12: LOGICAL - Flag to enable tab stop.
 *   13: LOGICAL - Flag to set default push button style.
 *   14: LPCSTR/LPCWSTR - Icon file name.
 *   15: LOGICAL - Flag to extract icon.
 *   16: INT - Icon index.
 *   17: LOGICAL - Flag to use image list.
 *
 * Returns:
 *   Array - Array containing handles to the button and image/icon.
 *
 * Purpose:
 *   Creates a button with an image or icon, with various customization options.
 */
HB_FUNC( INITIMAGEBUTTON )
{
   HWND        hbutton;
   HWND        himage;
   HICON       hIcon;

#ifndef UNICODE
   LPCSTR      lpWindowName = hb_parc( 2 );  // Button text (ANSI)
   LPCSTR      lpIconName = hb_parc( 14 );   // Icon file name (ANSI)
#else
   LPWSTR      lpWindowName = AnsiToWide( ( char * ) hb_parc( 2 ) ); // Button text (Unicode)
   LPWSTR      lpIconName = AnsiToWide( ( char * ) hb_parc( 14 ) );  // Icon file name (Unicode)
#endif
   HIMAGELIST  himl;

   HWND        hwnd = hmg_par_raw_HWND( 1 ); // Parent window handle
   DWORD       Style = BS_NOTIFY | WS_CHILD | ( hb_parl( 13 ) ? BS_DEFPUSHBUTTON : BS_PUSHBUTTON ) | ( ( hb_parc( 14 ) == NULL ) ? BS_BITMAP : BS_ICON );

   if( hb_parl( 9 ) )
   {
      Style |= BS_FLAT;          // Flat button style
   }

   if( !hb_parl( 11 ) )
   {
      Style |= WS_VISIBLE;       // Make button visible
   }

   if( !hb_parl( 12 ) )
   {
      Style |= WS_TABSTOP;       // Enable tab stop
   }

   // Create button window with specified style and parameters
   hbutton = CreateWindowEx
      (
         0,
         WC_BUTTON,
         lpWindowName,
         Style,
         hb_parni( 4 ),          // Button position (x-coordinate)
         hb_parni( 5 ),          // Button position (y-coordinate)
         hb_parni( 6 ),          // Button width
         hb_parni( 7 ),          // Button height
         hwnd,
         hmg_par_raw_HMENU( 3 ), // Menu handle (used as control ID)
         GetInstance(),          // Instance handle
         NULL
      );

#ifdef UNICODE
   hb_xfree( lpWindowName );     // Free allocated memory for Unicode text
#endif

   // If no icon provided, load a bitmap as button image
   if( HB_ISNIL( 14 ) )
   {
      if( !hb_parl( 17 ) )
      {
         // Load bitmap image
         himage = ( HWND ) HMG_LoadPicture( hb_parc( 8 ), -1, -1, hwnd, 0, hb_parl( 10 ) ? 0 : 1, -1, 0, HB_FALSE, 255 );

         // Set the loaded bitmap as button image
         SendMessage( hbutton, BM_SETIMAGE, ( WPARAM ) IMAGE_BITMAP, ( LPARAM ) himage );

         hb_reta( 2 );
         hmg_storvnl_HANDLE( hbutton, -1, 1 );
         hmg_storvnl_HANDLE( himage, -1, 2 );
      }
      else
      {
         // Create an image list and set it to button
         himl = HMG_SetButtonImageList( hbutton, hb_parc( 8 ), hb_parl( 10 ) ? 0 : 1, BUTTON_IMAGELIST_ALIGN_CENTER );

         hb_reta( 2 );
         hmg_storvnl_HANDLE( hbutton, -1, 1 );
         hmg_storvnl_HANDLE( himl, -1, 2 );
      }
   }
   else  // If icon is provided
   {
      if( !hb_parl( 15 ) )
      {
         // Load icon from resources or file
         hIcon = ( HICON ) LoadImage( GetResources(), lpIconName, IMAGE_ICON, 0, 0, LR_DEFAULTCOLOR );

         if( hIcon == NULL )
         {
            hIcon = ( HICON ) LoadImage( 0, lpIconName, IMAGE_ICON, 0, 0, LR_LOADFROMFILE | LR_DEFAULTCOLOR );
         }
      }
      else
      {
         // Extract icon from specified file
         hIcon = ( HICON ) ExtractIcon( GetInstance(), lpIconName, hb_parni( 16 ) );

         if( hIcon == NULL )
         {
            hIcon = ( HICON ) ExtractIcon( GetInstance(), TEXT( "user.exe" ), 0 );
         }
      }

#ifdef UNICODE
      hb_xfree( lpIconName );           // Free allocated memory for Unicode text
#endif

      // Handle image list or single icon setting
      if( hb_parl( 17 ) )
      {
         BITMAP            bm;
         ICONINFO          sIconInfo;
         BUTTON_IMAGELIST  bi;

         // Extract icon info to create an image list for the button
         if( GetIconInfo( hIcon, &sIconInfo ) )
         {
            GetObject( sIconInfo.hbmColor, sizeof( BITMAP ), ( LPVOID ) & bm );

            if( sIconInfo.hbmMask )
            {
               DeleteObject( sIconInfo.hbmMask );
            }

            if( sIconInfo.hbmColor )
            {
               DeleteObject( sIconInfo.hbmColor );
            }

            // Create image list and add icon
            himl = ImageList_Create( bm.bmWidth, bm.bmHeight, ILC_COLOR32 | ILC_MASK, 1, 0 );
            ImageList_AddIcon( himl, hIcon );

            DestroyIcon( hIcon );

            // Set image list properties
            bi.himl = himl;
            bi.margin.left = 10;
            bi.margin.top = 10;
            bi.margin.bottom = 10;
            bi.margin.right = 10;
            bi.uAlign = BUTTON_IMAGELIST_ALIGN_CENTER;

            SendMessage( ( HWND ) hbutton, BCM_SETIMAGELIST, ( WPARAM ) 0, ( LPARAM ) & bi );

            hb_reta( 2 );
            hmg_storvnl_HANDLE( hbutton, -1, 1 );
            hmg_storvnl_HANDLE( himl, -1, 2 );
         }
      }
      else
      {
         // Set icon as button image
         SendMessage( hbutton, BM_SETIMAGE, ( WPARAM ) IMAGE_ICON, ( LPARAM ) hIcon );

         hb_reta( 2 );
         hmg_storvnl_HANDLE( hbutton, -1, 1 );
         hmg_storvnl_HANDLE( hIcon, -1, 2 );
      }
   }
}

/*
 * FUNCTION: INITOWNERBUTTON
 *
 * Initializes a custom owner-drawn button with images or icons.
 *
 * Parameters:
 *   1: HWND - Handle of the parent window.
 *   2: LPCSTR/LPCWSTR - Button text.
 *   3: HMENU - Menu handle (used as control ID).
 *   4: INT - X coordinate of the button.
 *   5: INT - Y coordinate of the button.
 *   6: INT - Width of the button.
 *   7: INT - Height of the button.
 *   8: LPCSTR/LPCWSTR - Image file name.
 *   9: LOGICAL - Flag to set flat style.
 *   10: LOGICAL - Flag for transparency.
 *   11: LOGICAL - Flag to make button visible.
 *   12: LOGICAL - Flag to enable tab stop.
 *   13: LOGICAL - Flag to set default push button style.
 *   14: LPCSTR/LPCWSTR - Icon file name.
 *   15: INT - Width of the image/icon.
 *   16: INT - Height of the image/icon.
 *
 * Returns:
 *   Array - Array containing handles to the button and image/icon.
 *
 * Purpose:
 *   Creates a custom owner-drawn button with images or icons.
 */
HB_FUNC( INITOWNERBUTTON )
{
   HWND     hbutton;                      // Handle to the button control
   HWND     himage;                       // Handle to the image associated with the button (bitmap)
   HICON    hIcon;                        // Handle to the icon associated with the button (icon)
#ifndef UNICODE
   LPCSTR   lpWindowName = hb_parc( 2 );  // Button text in non-UNICODE mode
   LPCSTR   lpImageName = hb_parc( 8 );   // Image file name (bitmap)
   LPCSTR   lpIconName = hb_parc( 14 );   // Icon file name
#else
   LPCWSTR  lpWindowName = AnsiToWide( ( char * ) hb_parc( 2 ) ); // Button text in UNICODE mode
   LPCWSTR  lpImageName = AnsiToWide( ( char * ) hb_parc( 8 ) );  // Image file name (bitmap)
   LPCWSTR  lpIconName = AnsiToWide( ( char * ) hb_parc( 14 ) );  // Icon file name
#endif

   // Button style configuration based on parameters
   DWORD    Style = BS_NOTIFY | WS_CHILD | BS_OWNERDRAW | ( hb_parl( 13 ) ? BS_DEFPUSHBUTTON : BS_PUSHBUTTON ) | ( HB_ISNIL( 14 ) ? BS_BITMAP : BS_ICON );
   UINT     ImgStyle = hb_parl( 10 ) ? 0 : LR_LOADTRANSPARENT;    // Image transparency setting

   // Apply flat button style if parameter 9 is true
   if( hb_parl( 9 ) )
   {
      Style |= BS_FLAT;
   }

   // Set visibility and tabstop styles based on parameters
   if( !hb_parl( 11 ) )
   {
      Style |= WS_VISIBLE;
   }

   if( !hb_parl( 12 ) )
   {
      Style |= WS_TABSTOP;
   }

   // Create the button window with specified styles and dimensions
   hbutton = CreateWindowEx
      (
         0,             // Extended window style
         WC_BUTTON,     // Window class (button)
         lpWindowName,  // Button text
         Style,         // Button style
         hb_parni( 4 ), // X position
         hb_parni( 5 ), // Y position
         hb_parni( 6 ), // Width
         hb_parni( 7 ), // Height
         hmg_par_raw_HWND( 1 ),           // Parent window handle
         hmg_par_raw_HMENU( 3 ),          // Button identifier
         GetInstance(),                   // Application instance
         NULL                             // Additional data
      );

   // Subclass the button to use a custom window procedure
   SetProp( ( HWND ) hbutton, TEXT( "oldbtnproc" ), ( HANDLE ) GetWindowLongPtr( ( HWND ) hbutton, GWLP_WNDPROC ) );
   SubclassWindow2( hbutton, OwnButtonProc );

   // Check if using a bitmap or icon and load accordingly
   if( HB_ISNIL( 14 ) )
   {
      // Load bitmap image for the button
      himage = ( HWND ) LoadImage
         (
            GetResources(),
            lpImageName,
            IMAGE_BITMAP,
            HB_MAX( hb_parnidef( 15, 0 ), 0 ),
            HB_MAX( hb_parnidef( 16, 0 ), 0 ),
            LR_LOADMAP3DCOLORS | ImgStyle
         );

      // Fallback to loading from file if not found in resources
      if( himage == NULL )
      {
         himage = ( HWND ) LoadImage
            (
               NULL,
               lpImageName,
               IMAGE_BITMAP,
               HB_MAX( hb_parnidef( 15, 0 ), 0 ),
               HB_MAX( hb_parnidef( 16, 0 ), 0 ),
               LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | ImgStyle
            );
      }

      // Return handles for the button and image
      hb_reta( 2 );
      hmg_storvnl_HANDLE( hbutton, -1, 1 );
      hmg_storvnl_HANDLE( himage, -1, 2 );
   }
   else
   {
      // Load icon for the button
      hIcon = ( HICON ) LoadImage
         (
            GetResources(),
            lpIconName,
            IMAGE_ICON,
            HB_MAX( hb_parnidef( 15, 0 ), 0 ),
            HB_MAX( hb_parnidef( 16, 0 ), 0 ),
            LR_DEFAULTCOLOR
         );

      // Fallback to loading icon from file if not found in resources
      if( hIcon == NULL )
      {
         hIcon = ( HICON ) LoadImage
            (
               NULL,
               lpIconName,
               IMAGE_ICON,
               HB_MAX( hb_parnidef( 15, 0 ), 0 ),
               HB_MAX( hb_parnidef( 16, 0 ), 0 ),
               LR_LOADFROMFILE | LR_DEFAULTCOLOR
            );
      }

      // Extract icon from file if LoadImage fails
      if( hIcon == NULL )
      {
         hIcon = ( HICON ) ExtractIcon( GetInstance(), lpIconName, 0 );
      }

      // Return handles for the button and icon
      hb_reta( 2 );
      hmg_storvnl_HANDLE( hbutton, -1, 1 );
      hmg_storvnl_HANDLE( hIcon, -1, 2 );
   }

#ifdef UNICODE
   // Free allocated memory for UNICODE strings
   hb_xfree( ( TCHAR * ) lpWindowName );
   hb_xfree( ( TCHAR * ) lpImageName );
   hb_xfree( ( TCHAR * ) lpIconName );
#endif
}

/*
 * FUNCTION: _SETBTNPICTURE
 *
 * Sets a bitmap picture on an existing button.
 *
 * Parameters:
 *   1: HWND - Handle to the button.
 *   2: LPCSTR/LPCWSTR - Image file name.
 *   3: INT - Width of the image.
 *   4: INT - Height of the image.
 *
 * Returns:
 *   HWND - Handle to the loaded image.
 *
 * Purpose:
 *   Sets a bitmap picture on an existing button.
 */
HB_FUNC( _SETBTNPICTURE )
{
   HWND     hwnd = hmg_par_raw_HWND( 1 ); // Handle to the button
   HWND     himage;  // Handle to the loaded bitmap image
#ifndef UNICODE
   LPCSTR   lpImageName = hb_parc( 2 );   // Image file name in non-UNICODE
#else
   LPWSTR   lpImageName = AnsiToWide( ( char * ) hb_parc( 2 ) );  // Image file name in UNICODE
#endif

   // Load the bitmap image from resources or file
   himage = ( HWND ) LoadImage
      (
         GetResources(),
         lpImageName,
         IMAGE_BITMAP,
         HB_MAX( hb_parnidef( 3, 0 ), 0 ),
         HB_MAX( hb_parnidef( 4, 0 ), 0 ),
         LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT
      );

   if( himage == NULL )
   {
      himage = ( HWND ) LoadImage
         (
            NULL,
            lpImageName,
            IMAGE_BITMAP,
            HB_MAX( hb_parnidef( 3, 0 ), 0 ),
            HB_MAX( hb_parnidef( 4, 0 ), 0 ),
            LR_LOADFROMFILE | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT
         );
   }

   // Attempt to load as picture if LoadImage fails
   if( himage == NULL )
   {
      himage = ( HWND ) HMG_LoadPicture( hb_parc( 2 ), hb_parni( 3 ), hb_parni( 4 ), hwnd, 0, 1, -1, 0, HB_FALSE, 255 );
   }

   // Set the loaded bitmap as the button's image
   SendMessage( hwnd, BM_SETIMAGE, ( WPARAM ) IMAGE_BITMAP, ( LPARAM ) himage );

   RegisterResource( himage, "BMP" );  // Register the bitmap resource
   hmg_ret_raw_HANDLE( himage );       // Return handle to the loaded image
#ifdef UNICODE
   hb_xfree( lpImageName );            // Free UNICODE image name memory
#endif
}

/*
 * FUNCTION: _GETBTNPICTUREHANDLE
 *
 * Retrieves a handle to the current button picture.
 *
 * Parameters:
 *   1: HWND - Handle to the button.
 *
 * Returns:
 *   HWND - Handle to the current button picture.
 *
 * Purpose:
 *   Retrieves a handle to the current button picture.
 */
HB_FUNC( _GETBTNPICTUREHANDLE )
{
   hmg_ret_raw_HWND( SendMessage( hmg_par_raw_HWND( 1 ), BM_GETIMAGE, ( WPARAM ) IMAGE_BITMAP, ( LPARAM ) 0 ) );
}

/*
 * FUNCTION: _SETMIXEDBTNPICTURE
 *
 * Sets a button image list with multiple images.
 *
 * Parameters:
 *   1: HWND - Handle to the button.
 *   2: LPCSTR - Image file name.
 *   3: LOGICAL - Flag for transparency.
 *   4: UINT - Alignment.
 *
 * Returns:
 *   HIMAGELIST - Handle to the image list.
 *
 * Purpose:
 *   Sets a button image list with multiple images.
 */
HB_FUNC( _SETMIXEDBTNPICTURE )
{
   HIMAGELIST  himl;

   himl = HMG_SetButtonImageList( hmg_par_raw_HWND( 1 ), hb_parc( 2 ), hb_parl( 3 ) ? 0 : 1, hb_parnidef( 4, BUTTON_IMAGELIST_ALIGN_CENTER ) );

   RegisterResource( himl, "IMAGELIST" );
   hmg_ret_raw_HANDLE( himl );
}

/*
 * FUNCTION: _SETBTNICON
 *
 * Sets an icon for the button.
 *
 * Parameters:
 *   1: HWND - Handle to the button.
 *   2: LPCSTR/LPCWSTR - Icon file name.
 *   3: INT - Width of the icon.
 *   4: INT - Height of the icon.
 *
 * Returns:
 *   HICON - Handle to the loaded icon.
 *
 * Purpose:
 *   Sets an icon for the button.
 */
HB_FUNC( _SETBTNICON )
{
   HICON    hIcon;

#ifndef UNICODE
   LPCSTR   lpIconName = hb_parc( 2 ); // Icon file name in non-UNICODE
#else
   LPWSTR   lpIconName = AnsiToWide( ( char * ) hb_parc( 2 ) );   // Icon file name in UNICODE
#endif

   // Load the icon from resources or file
   hIcon = ( HICON ) LoadImage( GetResources(), lpIconName, IMAGE_ICON, HB_MAX( hb_parnidef( 3, 0 ), 0 ), HB_MAX( hb_parnidef( 4, 0 ), 0 ), LR_DEFAULTCOLOR );

   if( hIcon == NULL )
   {
      hIcon = ( HICON ) LoadImage
         (
            NULL,
            lpIconName,
            IMAGE_ICON,
            HB_MAX( hb_parnidef( 3, 0 ), 0 ),
            HB_MAX( hb_parnidef( 4, 0 ), 0 ),
            LR_LOADFROMFILE | LR_DEFAULTCOLOR
         );
   }

   // Extract icon if loading failed
   if( hIcon == NULL )
   {
      hIcon = ( HICON ) ExtractIcon( GetInstance(), lpIconName, 0 );
   }

   // Set the icon as the button's image
   SendMessage( hmg_par_raw_HWND( 1 ), BM_SETIMAGE, ( WPARAM ) IMAGE_ICON, ( LPARAM ) hIcon );

   RegisterResource( hIcon, "ICON" );     // Register the icon resource
   hmg_ret_raw_HANDLE( hIcon );           // Return handle to the loaded icon
#ifdef UNICODE
   hb_xfree( lpIconName );                // Free UNICODE icon name memory
#endif
}

/*
 * FUNCTION: _SETMIXEDBTNICON
 *
 * Sets a button icon list with multiple icons.
 *
 * Parameters:
 *   1: HWND - Handle to the button.
 *   2: LPCSTR/LPCWSTR - Icon file name.
 *
 * Returns:
 *   HIMAGELIST - Handle to the image list.
 *
 * Purpose:
 *   Sets a button icon list with multiple icons.
 */
HB_FUNC( _SETMIXEDBTNICON )
{
   BITMAP            bm;
   ICONINFO          sIconInfo;

#ifndef UNICODE
   LPCSTR            lpIconName = hb_parc( 2 );
#else
   LPWSTR            lpIconName = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif
   HIMAGELIST        himl;
   BUTTON_IMAGELIST  bi;

   HICON             hIcon = ( HICON ) LoadImage( GetResources(), lpIconName, IMAGE_ICON, 0, 0, LR_DEFAULTCOLOR );

   if( hIcon == NULL )
   {
      hIcon = ( HICON ) LoadImage( NULL, lpIconName, IMAGE_ICON, 0, 0, LR_LOADFROMFILE | LR_DEFAULTCOLOR );
   }

   GetIconInfo( hIcon, &sIconInfo );

   GetObject( sIconInfo.hbmColor, sizeof( BITMAP ), ( LPVOID ) & bm );

   if( sIconInfo.hbmMask )
   {
      DeleteObject( sIconInfo.hbmMask );
   }

   if( sIconInfo.hbmColor )
   {
      DeleteObject( sIconInfo.hbmColor );
   }

   himl = ImageList_Create( bm.bmWidth, bm.bmHeight, ILC_COLOR32 | ILC_MASK, 1, 0 );

   ImageList_AddIcon( himl, hIcon );

   DestroyIcon( hIcon );

   bi.himl = himl;
   bi.margin.left = 10;
   bi.margin.top = 10;
   bi.margin.bottom = 10;
   bi.margin.right = 10;
   bi.uAlign = 4;

   SendMessage( hmg_par_raw_HWND( 1 ), BCM_SETIMAGELIST, ( WPARAM ) 0, ( LPARAM ) & bi );

   RegisterResource( himl, "IMAGELIST" );
   hmg_ret_raw_HANDLE( himl );

#ifdef UNICODE
   hb_xfree( lpIconName );
#endif
}

/*
 * FUNCTION: DRAWBUTTON
 *
 * Handles custom drawing of the button, including different states (focused, clicked, etc.).
 *
 * Parameters:
 *   1: INT - Focus state.
 *   2: INT - Button state.
 *   3: INT - Mouse over state.
 *   4: INT - Flat style state.
 *   5: DRAWITEMSTRUCT* - Pointer to the DRAWITEMSTRUCT structure.
 *
 * Returns:
 *   None.
 *
 * Purpose:
 *   Handles custom drawing of the button, including different states (focused, clicked, etc.).
 */
HB_FUNC( DRAWBUTTON )
{
   DRAWITEMSTRUCT *pps = hmg_par_raw_DITEMSTRUCT( 4 );

   UINT           iFocus = hb_parni( 2 );
   UINT           iState = hb_parni( 3 );
   UINT           iMouseOver = hb_parni( 5 );
   UINT           iFlat = hb_parni( 6 );

   if( iFocus == 1 || iMouseOver == 1 )
   {
      InflateRect( &pps->rcItem, -1, -1 );
   }

   DrawFrameControl( pps->hDC, &pps->rcItem, DFC_BUTTON, iFlat ? ( iState | DFCS_FLAT ) : iState );

   if( iFocus == 1 )
   {
      HPEN     OldPen = ( HPEN ) SelectObject( pps->hDC, GetStockObject( BLACK_PEN ) );
      HBRUSH   OldBrush = ( HBRUSH ) SelectObject( pps->hDC, GetStockObject( NULL_BRUSH ) );

      InflateRect( &pps->rcItem, 1, 1 );
      Rectangle( pps->hDC, pps->rcItem.left, pps->rcItem.top, pps->rcItem.right, pps->rcItem.bottom );

      SelectObject( pps->hDC, OldBrush );
      SelectObject( pps->hDC, OldPen );
   }
}

/*
 * FUNCTION: GETOWNBTNHANDLE
 *
 * Returns the handle of the button from the DRAWITEMSTRUCT structure.
 *
 * Parameters:
 *   1: DRAWITEMSTRUCT* - Pointer to the DRAWITEMSTRUCT structure.
 *
 * Returns:
 *   HWND - Handle to the button.
 *
 * Purpose:
 *   Returns the handle of the button from the DRAWITEMSTRUCT structure.
 */
HB_FUNC( GETOWNBTNHANDLE )
{
   DRAWITEMSTRUCT *pps = hmg_par_raw_DITEMSTRUCT( 1 );

   if( pps )
   {
      hmg_ret_raw_HWND( pps->hwndItem );
   }
}

/*
 * FUNCTION: GETOWNBTNSTATE
 *
 * Returns the state of the button from the DRAWITEMSTRUCT structure.
 *
 * Parameters:
 *   1: DRAWITEMSTRUCT* - Pointer to the DRAWITEMSTRUCT structure.
 *
 * Returns:
 *   UINT - State of the button.
 *
 * Purpose:
 *   Returns the state of the button from the DRAWITEMSTRUCT structure.
 */
HB_FUNC( GETOWNBTNSTATE )
{
   DRAWITEMSTRUCT *pps = hmg_par_raw_DITEMSTRUCT( 1 );

   if( pps )
   {
      hmg_ret_UINT( pps->itemState );
   }
}

/*
 * FUNCTION: GETOWNBTNDC
 *
 * Returns the device context handle from the DRAWITEMSTRUCT structure.
 *
 * Parameters:
 *   1: DRAWITEMSTRUCT* - Pointer to the DRAWITEMSTRUCT structure.
 *
 * Returns:
 *   HDC - Device context handle.
 *
 * Purpose:
 *   Returns the device context handle from the DRAWITEMSTRUCT structure.
 */
HB_FUNC( GETOWNBTNDC )
{
   DRAWITEMSTRUCT *pps = hmg_par_raw_DITEMSTRUCT( 1 );

   if( pps )
   {
      hmg_ret_raw_HDC( pps->hDC );
   }
}

/*
 * FUNCTION: GETOWNBTNITEMID
 *
 * Returns the item ID from the DRAWITEMSTRUCT structure.
 *
 * Parameters:
 *   1: DRAWITEMSTRUCT* - Pointer to the DRAWITEMSTRUCT structure.
 *
 * Returns:
 *   UINT - Item ID.
 *
 * Purpose:
 *   Returns the item ID from the DRAWITEMSTRUCT structure.
 */
HB_FUNC( GETOWNBTNITEMID )
{
   DRAWITEMSTRUCT *pps = hmg_par_raw_DITEMSTRUCT( 1 );

   if( pps )
   {
      hmg_ret_UINT( pps->itemID );
   }
}

/*
 * FUNCTION: GETOWNBTNITEMACTION
 *
 * Returns the item action from the DRAWITEMSTRUCT structure.
 *
 * Parameters:
 *   1: DRAWITEMSTRUCT* - Pointer to the DRAWITEMSTRUCT structure.
 *
 * Returns:
 *   UINT - Item action.
 *
 * Purpose:
 *   Returns the item action from the DRAWITEMSTRUCT structure.
 */
HB_FUNC( GETOWNBTNITEMACTION )
{
   DRAWITEMSTRUCT *pps = hmg_par_raw_DITEMSTRUCT( 1 );

   if( pps )
   {
      hmg_ret_UINT( pps->itemAction );
   }
}

/*
 * FUNCTION: GETOWNBTNCTLTYPE
 *
 * Returns the control type from the DRAWITEMSTRUCT structure.
 *
 * Parameters:
 *   1: DRAWITEMSTRUCT* - Pointer to the DRAWITEMSTRUCT structure.
 *
 * Returns:
 *   UINT - Control type.
 *
 * Purpose:
 *   Returns the control type from the DRAWITEMSTRUCT structure.
 */
HB_FUNC( GETOWNBTNCTLTYPE )
{
   DRAWITEMSTRUCT *pps = hmg_par_raw_DITEMSTRUCT( 1 );

   if( pps )
   {
      hmg_ret_UINT( pps->CtlType );
   }
}

/*
 * FUNCTION: GETOWNBTNRECT
 *
 * Returns an array with the button rectangle coordinates from the DRAWITEMSTRUCT structure.
 *
 * Parameters:
 *   1: DRAWITEMSTRUCT* - Pointer to the DRAWITEMSTRUCT structure.
 *
 * Returns:
 *   Array - Array with the button rectangle coordinates.
 *
 * Purpose:
 *   Returns an array with the button rectangle coordinates from the DRAWITEMSTRUCT structure.
 */
HB_FUNC( GETOWNBTNRECT )
{
   PHB_ITEM       aMetr = hb_itemArrayNew( 4 );
   DRAWITEMSTRUCT *pps = hmg_par_raw_DITEMSTRUCT( 1 );

   RECT           rc = pps->rcItem;

   HB_arraySetNL( aMetr, 1, rc.left );
   HB_arraySetNL( aMetr, 2, rc.top );
   HB_arraySetNL( aMetr, 3, rc.right );
   HB_arraySetNL( aMetr, 4, rc.bottom );

   hb_itemReturnRelease( aMetr );
}

/*
 * FUNCTION: OwnButtonProc
 *
 * Custom button procedure handling button events (e.g., mouse events).
 *
 * Parameters:
 *   hButton: HWND - Handle to the button.
 *   Msg: UINT - Message identifier.
 *   wParam: WPARAM - Additional message-specific information.
 *   lParam: LPARAM - Additional message-specific information.
 *
 * Returns:
 *   LRESULT - Result of the message processing.
 *
 * Purpose:
 *   Custom button procedure handling button events (e.g., mouse events).
 */
LRESULT CALLBACK OwnButtonProc( HWND hButton, UINT Msg, WPARAM wParam, LPARAM lParam )
{
   static PHB_SYMB   pSymbol = NULL;
   LRESULT           r;
   TRACKMOUSEEVENT   tme;
   WNDPROC           OldWndProc;

   OldWndProc = ( WNDPROC ) ( HB_PTRUINT ) GetProp( hButton, TEXT( "oldbtnproc" ) );

   switch( Msg )
   {
      case WM_DESTROY:
         SubclassWindow2( hButton, OldWndProc );
         RemoveProp( hButton, TEXT( "oldbtnproc" ) );
         break;

      case WM_LBUTTONDBLCLK:
         SendMessage( hButton, WM_LBUTTONDOWN, wParam, lParam );
         break;

      case WM_MOUSEMOVE:
         tme.cbSize = sizeof( TRACKMOUSEEVENT );
         tme.dwFlags = TME_LEAVE;
         tme.hwndTrack = hButton;
         tme.dwHoverTime = 0;
         _TrackMouseEvent( &tme );

         if( !pSymbol )
         {
            pSymbol = hb_dynsymSymbol( hb_dynsymGet( "OBTNEVENTS" ) );
         }

         if( pSymbol )
         {
            hb_vmPushSymbol( pSymbol );
            hb_vmPushNil();
            hb_vmPushNumInt( ( HB_PTRUINT ) hButton );
            hb_vmPushLong( Msg );
            hb_vmPushNumInt( wParam );
            hb_vmPushNumInt( lParam );
            hb_vmDo( 4 );
         }

         r = hmg_par_LRESULT( -1 );

         return( r != 0 ) ? r : DefWindowProc( hButton, Msg, wParam, lParam );

      case WM_MOUSELEAVE:
         if( !pSymbol )
         {
            pSymbol = hb_dynsymSymbol( hb_dynsymGet( "OBTNEVENTS" ) );
         }

         if( pSymbol )
         {
            hb_vmPushSymbol( pSymbol );
            hb_vmPushNil();
            hb_vmPushNumInt( ( HB_PTRUINT ) hButton );
            hb_vmPushLong( Msg );
            hb_vmPushNumInt( wParam );
            hb_vmPushNumInt( lParam );
            hb_vmDo( 4 );
         }

         r = hmg_par_LRESULT( -1 );

         return( r != 0 ) ? r : DefWindowProc( hButton, Msg, wParam, lParam );
   }

   return CallWindowProc( OldWndProc, hButton, Msg, wParam, lParam );
}

/*
 * FUNCTION: CREATEBUTTONBRUSH
 *
 * Creates a gradient brush for button backgrounds.
 *
 * Parameters:
 *   1: HDC - Device context handle.
 *   2: INT - Width of the brush.
 *   3: INT - Height of the brush.
 *   4: COLORREF - First color of the gradient.
 *   5: COLORREF - Second color of the gradient.
 *
 * Returns:
 *   HBRUSH - Handle to the created gradient brush.
 *
 * Purpose:
 *   Creates a gradient brush for button backgrounds.
 */
HB_FUNC( CREATEBUTTONBRUSH )
{
   // Create a gradient brush by calling the helper function CreateGradientBrush
   // using the device context handle (HDC) and color values provided as arguments
   HBRUSH   hBrush = CreateGradientBrush( hmg_par_raw_HDC( 1 ), hb_parni( 2 ), hb_parni( 3 ), hmg_par_COLORREF( 4 ), hmg_par_COLORREF( 5 ) );

   // Register the created brush as a resource so it can be managed and cleaned up
   RegisterResource( hBrush, "BRUSH" );

   // Return the brush handle as the function's result
   hmg_ret_raw_HBRUSH( hBrush );
}

/*
 * FUNCTION: CreateGradientBrush
 *
 * Helper function that generates a gradient brush.
 *
 * Parameters:
 *   hDC: HDC - Device context handle.
 *   nWidth: INT - Width of the brush.
 *   nHeight: INT - Height of the brush.
 *   Color1: COLORREF - First color of the gradient.
 *   Color2: COLORREF - Second color of the gradient.
 *
 * Returns:
 *   HBRUSH - Handle to the created gradient brush.
 *
 * Purpose:
 *   Generates a gradient brush for button backgrounds.
 */
static HBRUSH CreateGradientBrush( HDC hDC, INT nWidth, INT nHeight, COLORREF Color1, COLORREF Color2 )
{
   // Variables to hold handles and color calculations
   HDC      hDCComp;                      // Compatible device context for creating the gradient
   HBITMAP  hBitmap;                      // Bitmap to hold the gradient pattern
   HBRUSH   hBrush, hBrushOld, hBrushPat; // Brushes for gradient drawing and final pattern
   RECT     rcF;                    // Rectangle for drawing gradient
   int      r1, g1, b1, r2, g2, b2; // RGB values for start and end colors
   int      nCount;                 // Number of gradient steps based on size
   int      i;                // Loop counter for gradient steps

   // Extract RGB components of the two colors for gradient calculation
   r1 = GetRValue( Color1 );
   g1 = GetGValue( Color1 );
   b1 = GetBValue( Color1 );
   r2 = GetRValue( Color2 );
   g2 = GetGValue( Color2 );
   b2 = GetBValue( Color2 );

   // Create a compatible DC and bitmap for drawing the gradient pattern
   hDCComp = CreateCompatibleDC( hDC );
   hBitmap = CreateCompatibleBitmap( hDC, nWidth, nHeight );
   SelectObject( hDCComp, hBitmap );

   // Initialize the rectangle to cover the entire button area
   rcF.left = 0;
   rcF.top = 0;
   rcF.right = nWidth;
   rcF.bottom = nHeight;

   // Determine the number of gradient steps based on button size
   nCount = ( int ) ceil( ( double ) ( ( nWidth > nHeight ) ? nHeight : nWidth ) / 2 );

   // Draw the gradient by gradually changing the color in each step
   for( i = 0; i < nCount; i++ )
   {
      // Calculate the color at the current gradient step
      hBrush = CreateSolidBrush( RGB( r1 + ( i * ( r2 - r1 ) / nCount ), g1 + ( i * ( g2 - g1 ) / nCount ), b1 + ( i * ( b2 - b1 ) / nCount ) ) );

      // Select the brush into the DC and fill the rectangle with it
      hBrushOld = SelectObject( hDCComp, hBrush );
      FillRect( hDCComp, &rcF, hBrush );

      // Restore the previous brush and delete the created brush for the current step
      SelectObject( hDCComp, hBrushOld );
      DeleteObject( hBrush );

      // Shrink the rectangle for the next gradient step
      InflateRect( &rcF, -1, -1 );
   }

   // Create a pattern brush from the gradient bitmap to use as a button background
   hBrushPat = CreatePatternBrush( hBitmap );

   // Clean up the compatible DC and bitmap after creating the brush
   DeleteDC( hDCComp );
   DeleteObject( hBitmap );

   // Return the gradient pattern brush
   return hBrushPat;
}

/*
 * FUNCTION: HMG_SetButtonImageList
 *
 * Loads an image into an image list for buttons.
 *
 * Parameters:
 *   hButton: HWND - Handle to the button.
 *   FileName: LPCSTR - Image file name.
 *   Transparent: INT - Flag for transparency.
 *   uAlign: UINT - Alignment.
 *
 * Returns:
 *   HIMAGELIST - Handle to the image list.
 *
 * Purpose:
 *   Loads an image into an image list for buttons.
 */
HIMAGELIST HMG_SetButtonImageList( HWND hButton, const char *FileName, int Transparent, UINT uAlign )
{
   HBITMAP           hBitmap; // Bitmap to hold the loaded image
   HIMAGELIST        himl;    // Image list to hold the button images
   BITMAP            Bmp;     // Bitmap structure to get image details
   BUTTON_IMAGELIST  bi;      // Structure to set image list for a button
   TCHAR             TempPath[MAX_PATH];  // Temporary file path for loading the image
   TCHAR             TempPathFileName[MAX_PATH];

   // Load the image file into a bitmap object with transparency if needed
   hBitmap = HMG_LoadPicture( FileName, -1, -1, NULL, 0, 0, -1, 0, HB_TRUE, 255 );
   if( hBitmap == NULL )
   {
      // If loading failed, return NULL
      return NULL;
   }

   // Retrieve bitmap information (dimensions, color format, etc.)
   GetObject( hBitmap, sizeof( BITMAP ), &Bmp );

   // Get a temporary path and file name for saving the loaded image
   GetTempPath( MAX_PATH, TempPath );
   GetTempFileName( TempPath, TEXT( "HMG" ), 0, TempPathFileName );
   bmp_SaveFile( hBitmap, TempPathFileName );
   DeleteObject( hBitmap );

   // Load the bitmap into an image list, using transparency if specified
   if( Transparent == 1 )
   {
      himl = ImageList_LoadImage
         (
            GetResources(),
            TempPathFileName,
            Bmp.bmWidth,
            6,
            CLR_DEFAULT,
            IMAGE_BITMAP,
            LR_LOADFROMFILE | LR_CREATEDIBSECTION | LR_LOADMAP3DCOLORS | LR_LOADTRANSPARENT
         );
   }
   else
   {
      himl = ImageList_LoadImage
         (
            GetResources(),
            TempPathFileName,
            Bmp.bmWidth,
            6,
            CLR_NONE,
            IMAGE_BITMAP,
            LR_LOADFROMFILE | LR_CREATEDIBSECTION | LR_LOADMAP3DCOLORS
         );
   }

   // Delete the temporary file after loading the image
   DeleteFile( TempPathFileName );

   // Set up the BUTTON_IMAGELIST structure with margin and alignment details
   bi.himl = himl;
   bi.margin.left = 10;
   bi.margin.top = 10;
   bi.margin.bottom = 10;
   bi.margin.right = 10;
   bi.uAlign = uAlign;

   // Assign the image list to the button control
   SendMessage( hButton, BCM_SETIMAGELIST, ( WPARAM ) 0, ( LPARAM ) & bi );

   // Return the handle to the created image list
   return himl;
}
