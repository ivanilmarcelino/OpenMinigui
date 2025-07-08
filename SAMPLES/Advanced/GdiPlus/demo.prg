/*
 * GDI+ demo
 *
 * Author: P.Chornyj <myorg63@mail.ru>
 *
*/

#include "minigui.ch"
#include "hbgdip.ch"

#define c1Tab CHR(9)
#define NTrim( n ) LTRIM( STR( n, IF( n == INT( n ), 0, 2 ) ) )

#xtranslate gSave => HMG_SaveImage

memvar cPicture

//////////////////////////////////////////////////////////////////////////////
procedure Main()

   if StatusOk != GdiplusInitExt( _GDI_GRAPHICS )
      quit
   endif

   public cPicture := 'demo'

   define window Form_Main ;
      at 0,0 ;
      width 320 height 240 ;
      title 'GDI+: Save Bitmap To File Demo' ;
      main ;
      nomaximize nosize

      s_CreateMenu()

      @ 20,20 image Image_1 picture cPicture
   end window

   on key Escape of Form_Main action ThisWindow.Release

   center   window Form_Main
   activate window Form_Main

return

//////////////////////////////////////////////////////////////////////////////
static procedure s_CreateMenu()

   local i
   local aMimeType := Array( GPlusGetEncodersNum() )

   for i := 1 to Len( aMimeType )
      aMimeType[i] := GPlusGetEncodersMimeType()[i]
   next

   define main menu
      define popup "&File" 
         for i := 1 TO Len( aMimeType )
            if "bmp" $ aMimeType[i]
               loop
            else

               if "jpeg" $ aMimeType[i]
                  menuitem '&Save as '+ aMimeType[i] action;
                     MsgInfo( iif( gSave( cPicture, cPicture+".jpeg", "jpeg", 90 ), "Saved", "Failure" ), "Result" )
               endif

               if "gif" $ aMimeType[i]
                  menuitem '&Save as '+ aMimeType[i] action ;
                     MsgInfo( iif( gSave( cPicture, cPicture+".gif", "gif" ), "Saved", "Failure" ), "Result" )
               endif

               if "tif" $ aMimeType[i]
                  menuitem '&Save as '+ aMimeType[i] action ;
                     MsgInfo( iif( gSave( cPicture, cPicture+".tif", "tiff" ), "Saved", "Failure" ), "Result" )
               endif

               if "png" $ aMimeType[i]
                  menuitem '&Save as '+ aMimeType[i] action ;
                     MsgInfo( iif( gSave( cPicture, cPicture+".png", "png" ), "Saved", "Failure" ), "Result" )
               endif
            endif
         next 

         separator

         menuitem "E&xit" action ThisWindow.Release
      end popup

      define popup "&?" 
         menuitem '&Get number of image coders' action ;
            MsgInfo( "Number of image coders"  + c1Tab + ": " + NTrim( gPlusGetEncodersNum() ), "Info" )

         menuitem '&Get size of image coders array in bytes' action ;
            MsgInfo( "Size of image coders array (in bytes)"  + c1Tab + ": " + NTrim( gPlusGetEncodersSize() ), "Info" )

         MENUITEM "&BMP Info"  ACTION s_GetImageInfo( GetStartupFolder() + "\demo.bmp" )
         MENUITEM "&JPEG Info" ACTION s_GetImageInfo( GetStartupFolder() + "\rainbow.jpg" )
         MENUITEM "&PNG Info"  ACTION s_GetImageInfo( GetStartupFolder() + "\demo.png" )
      end popup
   end menu

return

//////////////////////////////////////////////////////////////////////////////
static procedure s_GetImageInfo( cFile )

   local image 
   local width := 0, height := 0
   local cMsg

   if StatusOk == GdipLoadImageFromFile( cFile, @image )
      GdipGetImageDimension( image, @width, @height )

      cMsg := "Picture name" + c1Tab + ": " + cFileNoPath( cFile ) + CRLF
      cMsg += "Image Width"  + c1Tab + ": " + NTrim( width ) + CRLF
      cMsg += "Image Height" + c1Tab + ": " + NTrim( height )

      MsgInfo( cMsg, "Image Info" )

      GdipDisposeImage( image )
   endif

return
