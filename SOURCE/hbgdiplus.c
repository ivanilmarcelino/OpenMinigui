/*
   Parts of this code are contributed and used here under permission of the
   author: Copyright 2007-2017 (C) P.Chornyj <myorg63@mail.ru>
 */
#include <mgdefs.h>

#define _HMG_STUB_
#include "hbgdiplus.h"
#undef _HMG_STUB_
DECLARE_FUNCPTR( GdiplusStartup );
DECLARE_FUNCPTR( GdiplusShutdown );

DECLARE_FUNCPTR( GdipCreateBitmapFromFile );
DECLARE_FUNCPTR( GdipCreateBitmapFromResource );
DECLARE_FUNCPTR( GdipCreateBitmapFromStream );
DECLARE_FUNCPTR( GdipCreateHBITMAPFromBitmap );
DECLARE_FUNCPTR( GdipDisposeImage );
DECLARE_FUNCPTR( GdipGetImageEncodersSize );
DECLARE_FUNCPTR( GdipGetImageEncoders );
DECLARE_FUNCPTR( GdipGetImageThumbnail );
DECLARE_FUNCPTR( GdipCreateBitmapFromHBITMAP );
DECLARE_FUNCPTR( GdipSaveImageToFile );

HMODULE           g_GpModule = NULL;
static ULONG_PTR  g_GpToken = 0;

/*
 * FUNCTION GdiplusInit()
 *
 * Initializes the GDI+ library. Loads the Gdiplus.dll, retrieves function pointers,
 * and starts the GDI+ subsystem.
 *
 * Parameters:
 *   None
 *
 * Returns:
 *   GpStatus: A status code indicating the result of the initialization.
 *     - Ok: The GDI+ library was successfully initialized.
 *     - GdiplusNotInitialized: The Gdiplus.dll could not be loaded.
 *     - NotImplemented: One or more required GDI+ functions could not be found in the DLL.
 *     - Other GpStatus values may indicate specific initialization errors. See GDI+ documentation for details.
 *
 * Purpose:
 *   This function enables GDI+ functionality within the application by loading the necessary DLL and retrieving
 *   pointers to the GDI+ functions that will be used. It must be called before any GDI+ operations.
 *
 * Notes:
 *   - The function attempts to load "Gdiplus.dll". Ensure this DLL is present in the application's directory
 *     or in a location specified in the system's PATH environment variable.
 *   - The function checks for the existence of several key GDI+ functions. If any of these functions are missing,
 *     the function returns NotImplemented, indicating that the GDI+ library is incomplete or incompatible.
 *   - The global variables g_GpModule and g_GpToken store the handle to the loaded Gdiplus.dll and the GDI+ token, respectively.
 *     These are used by other GDI+ functions.
 */
GpStatus GdiplusInit( void )
{
   LPCTSTR                 lpFileName = TEXT( "Gdiplus.dll" );
   GDIPLUS_STARTUP_INPUT   GdiplusStartupInput = { 1, NULL, FALSE, FALSE };

   if( NULL == g_GpModule )
   {
      g_GpModule = LoadLibrary( lpFileName );
   }

   if( NULL == g_GpModule )
   {
      return GdiplusNotInitialized;
   }

   if( _EMPTY_PTR( g_GpModule, GdiplusStartup ) )
   {
      return NotImplemented;
   }

   if( _EMPTY_PTR( g_GpModule, GdiplusShutdown ) )
   {
      return NotImplemented;
   }

   if( _EMPTY_PTR( g_GpModule, GdipCreateBitmapFromFile ) )
   {
      return NotImplemented;
   }

   if( _EMPTY_PTR( g_GpModule, GdipCreateBitmapFromResource ) )
   {
      return NotImplemented;
   }

   if( _EMPTY_PTR( g_GpModule, GdipCreateBitmapFromStream ) )
   {
      return NotImplemented;
   }

   if( _EMPTY_PTR( g_GpModule, GdipCreateHBITMAPFromBitmap ) )
   {
      return NotImplemented;
   }

   if( _EMPTY_PTR( g_GpModule, GdipDisposeImage ) )
   {
      return NotImplemented;
   }

   if( _EMPTY_PTR( g_GpModule, GdipGetImageEncodersSize ) )
   {
      return NotImplemented;
   }

   if( _EMPTY_PTR( g_GpModule, GdipGetImageEncoders ) )
   {
      return NotImplemented;
   }

   if( _EMPTY_PTR( g_GpModule, GdipCreateBitmapFromHBITMAP ) )
   {
      return NotImplemented;
   }

   if( _EMPTY_PTR( g_GpModule, GdipSaveImageToFile ) )
   {
      return NotImplemented;
   }

   if( _EMPTY_PTR( g_GpModule, GdipGetImageThumbnail ) )
   {
      return NotImplemented;
   }

   return fn_GdiplusStartup( &g_GpToken, &GdiplusStartupInput, NULL );
}

/*
 * FUNCTION GDIPLUSSHUTDOWN( [lFreeLibrary] )
 *
 * Shuts down the GDI+ library and optionally unloads the Gdiplus.dll.
 *
 * Parameters:
 *   lFreeLibrary : Optional. A logical value indicating whether to unload the Gdiplus.dll.
 *                  If .T. or omitted, the DLL is unloaded. If .F., the DLL remains loaded.
 *
 * Returns:
 *   None
 *
 * Purpose:
 *   This function is the counterpart to GdiplusInit(). It should be called when the application
 *   no longer needs GDI+ functionality. It shuts down the GDI+ subsystem and releases resources.
 *   Optionally, it can also unload the Gdiplus.dll from memory.
 *
 * Notes:
 *   - Call this function to properly release GDI+ resources and prevent memory leaks.
 *   - If lFreeLibrary is set to .T., the GDI+ module (g_GpModule) will be unloaded from memory.
 *     This should only be done if the application is finished using GDI+ and no other parts of the
 *     application depend on it.
 */
HB_FUNC( GDIPLUSSHUTDOWN )
{
   if( NULL != fn_GdiplusShutdown )
   {
      fn_GdiplusShutdown( g_GpToken );
   }

   if( HB_TRUE == hb_parldef( 1, HB_TRUE ) && ( NULL != g_GpModule ) )
   {
      FreeLibrary( g_GpModule );
   }
}

/*
 * FUNCTION GDIPCREATEBITMAPFROMFILE( cFileName, @pBitmap )
 *
 * Creates a GDI+ Bitmap object from an image file.
 *
 * Parameters:
 *   cFileName : The name of the image file to load. Must be a valid path to an image file
 *               supported by GDI+ (e.g., BMP, JPG, PNG, GIF, TIFF).
 *   pBitmap   : A pass-by-reference parameter. A pointer to a GpBitmap variable that will
 *               receive the address of the newly created GDI+ Bitmap object.
 *
 * Returns:
 *   Numeric: A GpStatus code indicating the result of the operation.
 *     - Ok: The bitmap was successfully created.
 *     - InvalidParameter: cFileName is NULL or empty.
 *     - NotImplemented: The GdipCreateBitmapFromFile function is not available (GDI+ not properly initialized).
 *     - Other GpStatus values may indicate file access errors, invalid image format, or other GDI+ errors.
 *       See GDI+ documentation for details.
 *
 * Purpose:
 *   This function loads an image from a file into a GDI+ Bitmap object. The Bitmap object
 *   can then be used for various image processing and drawing operations.
 *
 * Notes:
 *   - The caller is responsible for disposing of the GDI+ Bitmap object using GdipDisposeImage() when it is no longer needed.
 *   - The function converts the Harbour string cFileName to a wide character string (HB_WCHAR) for use with the GDI+ API.
 *   - If the file name is empty, the function returns InvalidParameter.
 */
HB_FUNC( GDIPCREATEBITMAPFROMFILE )
{
   GpBitmap *bitmap = ( GpBitmap * ) NULL;

   if( NULL != fn_GdipCreateBitmapFromFile )
   {
      HB_WCHAR *lpFName = ( HB_WCHAR * ) ( ( hb_parclen( 1 ) == 0 ) ? NULL : hb_mbtowc( hb_parc( 1 ) ) );

      if( NULL != lpFName )
      {
         hb_retni( fn_GdipCreateBitmapFromFile( lpFName, &bitmap ) );

         hb_xfree( lpFName );
      }
      else
      {
         hb_retni( InvalidParameter );
      }
   }
   else
   {
      hb_retni( NotImplemented );
   }

   hb_storptr( bitmap, 2 );
}

/*
 * FUNCTION GDIPCREATEHBITMAPFROMBITMAP( pBitmap, @hBitmap, nARGB )
 *
 * Creates a Windows HBITMAP (handle to a device-dependent bitmap) from a GDI+ Bitmap object.
 *
 * Parameters:
 *   pBitmap : A pointer to the GDI+ Bitmap object to convert.
 *   hBitmap : A pass-by-reference parameter. A pointer to an HBITMAP variable that will
 *             receive the handle to the newly created Windows HBITMAP.
 *   nARGB   : An ARGB (Alpha, Red, Green, Blue) color value to use for transparent pixels.
 *             This value is used when converting the GDI+ Bitmap to an HBITMAP.
 *
 * Returns:
 *   Numeric: A GpStatus code indicating the result of the operation.
 *     - Ok: The HBITMAP was successfully created.
 *     - InvalidParameter: pBitmap is NULL.
 *     - NotImplemented: The GdipCreateHBITMAPFromBitmap function is not available (GDI+ not properly initialized).
 *     - Other GpStatus values may indicate memory allocation errors or other GDI+ errors.
 *       See GDI+ documentation for details.
 *
 * Purpose:
 *   Converts a GDI+ Bitmap object into a standard Windows HBITMAP, useful for integrating GDI+ images
 *   with other parts of the application that use the traditional Windows GDI API.
 *
 * Notes:
 *   - The caller is responsible for deleting the HBITMAP using DeleteObject() when it is no longer needed.
 *   - The nARGB parameter allows specifying a color to be treated as transparent in the HBITMAP.
 */
HB_FUNC( GDIPCREATEHBITMAPFROMBITMAP )
{
   HBITMAP  hbitmap = ( HBITMAP ) NULL;

   if( NULL != fn_GdipCreateHBITMAPFromBitmap )
   {
      GpBitmap *bitmap = ( GpBitmap * ) hb_parptr( 1 );

      if( NULL != bitmap )
      {
         ARGB  argb = ( ARGB ) hb_parnl( 3 );

         hb_retni( fn_GdipCreateHBITMAPFromBitmap( bitmap, &hbitmap, argb ) );
      }
      else
      {
         hb_retni( InvalidParameter );
      }
   }
   else
   {
      hb_retni( NotImplemented );
   }

   hb_storptr( hbitmap, 2 );
}

/*
 * FUNCTION GDIPDISPOSEIMAGE( pImage )
 *
 * Disposes of a GDI+ Image object (Bitmap, Metafile, etc.), releasing its resources.
 *
 * Parameters:
 *   pImage : A pointer to the GDI+ Image object to dispose of.
 *
 * Returns:
 *   Numeric: A GpStatus code indicating the result of the operation.
 *     - Ok: The image was successfully disposed of.
 *     - NotImplemented: The GdipDisposeImage function is not available (GDI+ not properly initialized).
 *     - Other GpStatus values may indicate invalid image pointer or other GDI+ errors.
 *       See GDI+ documentation for details.
 *
 * Purpose:
 *   Releases the resources allocated by the GDI+ Image object to prevent memory leaks.
 *
 * Notes:
 *   - Call this function for every GDI+ Image object that is created.
 *   - Failure to dispose of GDI+ Image objects can lead to memory leaks and application instability.
 */
HB_FUNC( GDIPDISPOSEIMAGE )
{
   if( NULL != fn_GdipDisposeImage )
   {
      hb_retni( fn_GdipDisposeImage( hb_parptr( 1 ) ) );
   }
   else
   {
      hb_retni( NotImplemented );
   }
}
