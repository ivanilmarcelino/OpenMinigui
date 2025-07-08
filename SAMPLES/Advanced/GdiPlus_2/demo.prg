/*
 * GDIPlus Sample
 * Author: Fernando Yurisich <fyurisich@oohg.org>
 *
 * Licensed under The Code Project Open License (CPOL) 1.02
 * See <http://www.codeproject.com/info/cpol10.aspx>
 *
 * This sample shows how to use GDI+ library to load
 * and save bmp, jpeg, gif, tiff and png images.
 *
 */

#include "hmg.ch"
#include "hbgdip.ch"

#define HBITMAP_WIDTH     1
#define HBITMAP_HEIGHT    2
#define HBITMAP_BITSPIXEL 3

MEMVAR cPicture, cType, aSize

PROCEDURE Main()
   LOCAL i, aMimeType
   PUBLIC cPicture, cType, aSize

   if StatusOk != GdiplusInitExt( _GDI_GRAPHICS )
      quit
   endif

   aMimeType := GPlusGetEncodersMimeType()
   /*
    * Default types:
    * image/bmp
    * image/jpeg
    * image/gif
    * image/tiff
    * image/png
    */

   DEFINE WINDOW Form_Main ;
      AT 0,0 ;
      WIDTH 640 ;
      HEIGHT 480 ;
      TITLE 'GDI+: Save Bitmap To File' ;
      ICON 'demo.ico' ;
      MAIN ;
      NOMAXIMIZE ;
      NOSIZE

      DEFINE MAIN MENU
         DEFINE POPUP "&File"
            FOR i := 1 TO Len( aMimeType )
               IF "jpeg" $ aMimeType[i]
                  MENUITEM 'Save as JPEG' NAME mnu_JPEG DISABLED ;
                     ACTION SaveToFile( Form_Main.Image_1.HBitMap, ;
                                        "new.jpeg", ;
                                        aSize[HBITMAP_WIDTH], ;
                                        aSize[HBITMAP_HEIGHT], ;
                                        "jpeg", ;
                                        100 )
               ENDIF
               IF "gif" $ aMimeType[i]
                  MENUITEM 'Save as GIF' NAME mnu_GIF DISABLED ;
                     ACTION SaveToFile( Form_Main.Image_1.HBitMap, ;
                                        "new.gif", ;
                                        aSize[HBITMAP_WIDTH], ;
                                        aSize[HBITMAP_HEIGHT], ;
                                        "gif", ;
                                        100 )
               ENDIF
               IF "tiff" $ aMimeType[i]
                  MENUITEM 'Save as TIFF' NAME mnu_TIFF DISABLED ;
                     ACTION SaveToFile( Form_Main.Image_1.HBitMap, ;
                                        "new.tiff", ;
                                        aSize[HBITMAP_WIDTH], ;
                                        aSize[HBITMAP_HEIGHT], ;
                                        "tiff", ;
                                        100 )
               ENDIF
               IF "png" $ aMimeType[i]
                  MENUITEM 'Save as PNG' NAME mnu_PNG DISABLED ;
                     ACTION SaveToFile( Form_Main.Image_1.HBitMap, ;
                                        "new.png", ;
                                        aSize[HBITMAP_WIDTH], ;
                                        aSize[HBITMAP_HEIGHT], ;
                                        "png", ;
                                        100 )
               ENDIF
               IF "bmp" $ aMimeType[i]
                  MENUITEM 'Save as BMP' NAME mnu_BMP DISABLED ;
                     ACTION SaveToFile( Form_Main.Image_1.HBitMap, ;
                                        "new.bmp", ;
                                        aSize[HBITMAP_WIDTH], ;
                                        aSize[HBITMAP_HEIGHT], ;
                                        "bmp", ;
                                        100 )
               ENDIF
            NEXT
            SEPARATOR
            MENUITEM "E&xit" ACTION ThisWindow.Release
         END POPUP
         DEFINE POPUP "&?"
            MENUITEM 'Get number of image encoders' ;
               ACTION MsgInfo( "Number of image encoders: " + ;
                                  hb_ntos( gPlusGetEncodersNum() ), ;
                               "Info" )
            SEPARATOR
            MENUITEM "Image Info" NAME mnu_INFO DISABLED ;
               ACTION MsgInfo( "Name: " + CRLF + ;
                                        cPicture + CRLF + ;
                                     "Width: " + CRLF + ;
                                        hb_ntos( aSize[HBITMAP_WIDTH] ) + CRLF + ;
                                     "Height: " + CRLF + ;
                                        hb_ntos( aSize[HBITMAP_HEIGHT] ) + CRLF + ;
                                     "Bits per Pixel: " + CRLF + ;
                                        hb_ntos( aSize[HBITMAP_BITSPIXEL] ), ;
                                   "Image Info" )
         END POPUP
      END MENU

      @ 05, 20 LABEL lbl_Type ;
         VALUE "Type:" ;
         WIDTH 50 ;
         HEIGHT 24

      @ 05, 70 COMBOBOX cmb_Type ;
         WIDTH 150 ;
         ITEMS {'bmp','jpeg','gif','tiff','png','emf'} ;
         VALUE 0 ;
         ON CHANGE LoadImage( Form_Main.cmb_Type.Value )

      @ 05, 240 LABEL lbl_Image ;
         VALUE "Image loaded: <none>" ;
         WIDTH 350 ;
         HEIGHT 24

      @ 40, 20 IMAGE Image_1 PICTURE "" ;
         ADJUSTIMAGE

      ON KEY ESCAPE ACTION ThisWindow.Release
   END WINDOW

   CENTER WINDOW Form_Main
   ACTIVATE WINDOW Form_Main
RETURN

FUNCTION LoadImage( i )
   cType          := {'bmp','jpeg','gif','tiff','png','emf'} [i]
   cPicture       := "demo." + cType
   aSize          := BmpSize( cPicture )
   Form_Main.Image_1.Picture := cPicture

   Form_Main.lbl_Image.Value  := "Image loaded: " + cPicture

   Form_Main.mnu_BMP.Enabled  := ( i # 1 )
   Form_Main.mnu_JPEG.Enabled := ( i # 2 )
   Form_Main.mnu_GIF.Enabled  := ( i # 3 )
   Form_Main.mnu_TIFF.Enabled := ( i # 4 )
   Form_Main.mnu_PNG.Enabled  := ( i # 5 )
   Form_Main.mnu_INFO.Enabled := .T.
RETURN NIL

FUNCTION GetImageInfo( cFile )
   LOCAL nImage, nWidth, nHeight

   GdipLoadImageFromFile( cFile, @nImage )
   GdipGetImageDimension( nImage, @nWidth, @nHeight )

   MsgInfo( "Name: " + CRLF + cFile + CRLF + ;
                  "Width: "  + CRLF + hb_ntos( Int(nWidth) ) + CRLF + ;
                  "Height: " + CRLF + hb_ntos( Int(nHeight) ), ;
                "Image Info" )
RETURN NIL

FUNCTION SaveToFile( hBitMap, cFile, nWidth, nHeight, cMimeType, nQuality )
   LOCAL lRet := HMG_SaveImage( hBitMap, ;
                                         cFile, ;
                                         cMimeType, ;
                                         nQuality, ;
                                         {nWidth, ;
                                         nHeight} ;
                                          )
RETURN MsgInfo( iif( lRet, "Saved to " + cFile, "Failure" ), "Result" )
