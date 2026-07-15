This is a Harbour wrapper for the FreeImage DLL, enabling Harbour programs to use FreeImage's image processing capabilities. Below is a breakdown of its key components:

Purpose of the Wrapper
The wrapper allows Harbour programs to interact with the FreeImage library, which supports various image formats (JPG, PNG, BMP, etc.), image transformations, and metadata handling. By exposing FreeImage functions via Harbour’s HB_FUNC macros, Harbour applications can call these functions directly.

---

Key Features of the Wrapper
1. Memory Management & Garbage Collection
   - Structures like HB_FIBITMAP manage image objects, ensuring proper deallocation using FreeImage_Unload().
   - HB_GARBAGE_FUNC defines garbage collection routines to prevent memory leaks.

2. Error Handling
   - A custom error handler (FreeImageErrorHandler()) captures FreeImage errors and reports them in Harbour.
   - FI_SETOUTPUTMESSAGE() sets an error callback to process messages from FreeImage.

3. Basic Image Operations
   - Loading an Image

     HB_FUNC( FI_LOAD )
     {
        if( HB_ISNUM( 1 ) && HB_ISCHAR( 2 ) && HB_ISNUM( 3 ) )
        {
           FREE_IMAGE_FORMAT fif = ( FREE_IMAGE_FORMAT ) hb_parni( 1 );
           const char        *filename = hb_parc( 2 );
           int               flags = hb_parni( 3 );

           FIBITMAP          *dib = FreeImage_Load( fif, filename, flags );

           if( dib )
           {
              hb_FIBITMAP_ret( dib, HB_TRUE );
           }
        }
     }
     
     - Loads an image using FreeImage_Load(), which requires format, filename, and flags.
     - The image is wrapped in a Harbour HB_FIBITMAP structure for safe handling.

   - Saving an Image

     HB_FUNC( FI_SAVE )
     {
        if( HB_ISNUM( 1 ) && hb_FIBITMAP_is( 2 ) && HB_ISCHAR( 3 ) && HB_ISNUM( 4 ) )
        {
           FREE_IMAGE_FORMAT fif = ( FREE_IMAGE_FORMAT ) hb_parni( 1 );
           FIBITMAP          *dib = hb_FIBITMAP_par( 2 );
           const char        *filename = hb_parc( 3 );
           int               flags = hb_parni( 4 );

           hb_fi_retl( FreeImage_Save( fif, dib, filename, flags ) );
        }
     }
     
     - Saves an image with FreeImage_Save(), specifying the format, image, filename, and flags.

4. Image Information & Metadata
   - Get image properties (width, height, color depth):

     HB_FUNC( FI_GETWIDTH )
     {
        if( hb_FIBITMAP_is( 1 ) )
        {
           hb_retnl( FreeImage_GetWidth( hb_FIBITMAP_par( 1 ) ) );
        }
     }
     
   - Retrieves metadata, such as comments or EXIF data.

5. Image Manipulation
   - Convert between color depths:

     HB_FUNC( FI_CONVERTTO24BITS )
     {
        if( hb_FIBITMAP_is( 1 ) )
        {
           hb_FIBITMAP_ret( FreeImage_ConvertTo24Bits( hb_FIBITMAP_par( 1 ) ), HB_TRUE );
        }
     }
     
   - Perform image transformations such as scaling, rotation, and flipping:

     HB_FUNC( FI_ROTATE )
     {
        if( hb_FIBITMAP_is( 1 ) && HB_ISNUM( 2 ) )
        {
           FIBITMAP *dib = hb_FIBITMAP_par( 1 );
           double   angle = hb_parnd( 2 );

           hb_FIBITMAP_ret( FreeImage_Rotate( dib, angle, NULL ), HB_TRUE );
        }
     }

---

How Harbour Calls These Functions
Harbour programs can load and manipulate images using these wrapped functions:

hbFImage := FI_LOAD( FIF_JPEG, "image.jpg", 0 )
hbFImage := FI_RESCALE( hbFImage, 300, 200, FILTER_BILINEAR )
FI_SAVE( FIF_PNG, hbFImage, "output.png", 0 )
FI_UNLOAD( hbFImage )

This workflow:
1. Loads a JPEG image.
2. Resizes it to 300x200 pixels.
3. Saves it as a PNG.
4. Unloads the image to free memory.

---

Conclusion
This Harbour wrapper for FreeImage provides powerful image processing capabilities, handling memory efficiently while exposing essential FreeImage functions to Harbour applications.