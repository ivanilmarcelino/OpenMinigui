/**********************************************************
 * Harbour-MiniGUI source code
 * Project      : Simple PDF Creator
 * Created      : 09/15/2016, 17:36
 * Author (c)   : Pete D.
 * Last Update  : 10:20 рм 15/2/2022   by :(Pete D.):
 **********************************************************
*/

#ifdef __XHARBOUR__
#define __MINIPRINT__
#define _RPTGEN_
#endif

#include "hmg.ch"
#include "harupdf.ch"

#ifdef __XHARBOUR__
#xtranslate hb_eol() => hb_OsNewLine()
#xtranslate WAPI_SHELLEXECUTE( [<x,...>] ) => SHELLEXECUTE( <x> )
#else
REQUEST HB_CODEPAGE_UTF8
#endif

#define MAX_IMAGE 20

********************************************************************************
FUNCTION _CreatePdf( aPages, cPdfFile, lOpen, cTitle )
********************************************************************************
   LOCAL hDoc, hPage, nPage
   LOCAL cPage, cImageFile
   LOCAL cCodePage, cOldCodePage
   LOCAL nPageHeight, nPageWidth
   LOCAL nPages, hBitmap

   LOCAL cAuthor
   LOCAL cCreator := "Simple PDF Creator"
   LOCAL page_size := HPDF_PAGE_SIZE_A4
   LOCAL page_orient := HPDF_PAGE_PORTRAIT

   LOCAL aSizes
   LOCAL lRet := .T.
   LOCAL cErrMess := ""

   IF Empty( aPages )
      cErrMess := "- Source folder" + hb_eol()
   ENDIF
   IF Empty( nPages := Len( aPages ) )
      cErrMess += "- Number of total pages" + hb_eol()
   ENDIF
   IF Empty( cPdfFile )
      cErrMess += "- Output file" + hb_eol()
   ENDIF
   IF ! Empty( cErrMess )
      MsgExclamation( cErrMess + "cannot be empty!", "Warning" )
      RETURN .F.
   ENDIF

   hb_default( @cTitle, "Untitled" )

   cPdfFile := hb_FNameExtSet( cPdfFile, "pdf" )

   IF hb_FileExists( cPdfFile )
      IF ! MsgYesNo( "File " + cPdfFile + " already exists!" + hb_eol() + "Overwrite?", "Warning!" )
         RETURN .F.
      ENDIF
   ENDIF

   CLEAN MEMORY

   // create pdf doc
   IF ( hDoc := HPDF_New() ) == NIL
      RETURN UPDF_Error( "CREATE", hDoc )
   ENDIF

   cOldCodePage := hb_cdpSelect( "UTF8" )
   cCodePage := "UTF-8"
   cAuthor := GetUserName()

   WaitWindow( "Creating PDF file", .T. )

   BEGIN SEQUENCE

      IF HPDF_SetCompressionMode( hDoc, HPDF_COMP_ALL ) <> HPDF_OK
         lRet := UPDF_Error( "COMPRESS", hDoc )
         BREAK
      ENDIF

      HPDF_UseUTFEncodings( hDoc )

      HPDF_SetCurrentEncoder( hDoc, cCodePage )

      PdfSetInfo( hDoc, cTitle, cAuthor, cCreator )

      nPage := 0
      // start main loop
      WHILE ++nPage <= Min( nPages, MAX_IMAGE )
         cPage := aPages[ nPage ]

         hBitmap := BT_BitmapLoadEMF( cPage, WHITE )
         cImageFile := hb_FNameExtSet( cPage, "png" )
         BT_BitmapSaveFile( hBitmap, cImageFile, BT_FILEFORMAT_PNG )

         aSizes := BmpSize( hBitmap )
         IF aSizes[ 1 ] - 850 > aSizes[ 2 ]
            page_orient := HPDF_PAGE_LANDSCAPE
         ENDIF
         BT_BitmapRelease( hBitmap )

         // create new page
         hPage := HPDF_AddPage( hDoc )
         HPDF_Page_SetSize( hPage, page_size, page_orient )
         nPageHeight := HPDF_Page_GetHeight( hPage )
         nPageWidth := HPDF_Page_GetWidth( hPage )
         // put page picture
         lRet := PutPageImage( hDoc, hPage, cImageFile, nPageHeight, nPageWidth )
#ifndef __DEBUG__
         FErase( cImageFile )
#endif
         IF ! lRet
            MsgExclamation( "There was an error with image file:" + hb_eol() + cPage + " !", "Warning" )
            lRet := .F.
            EXIT
         ENDIF
      END

      IF lRet
         IF !( HPDF_SaveToFile( hDoc, cPdfFile ) == 0 )
            lRet := UPDF_Error( "SAVE", hDoc )
            BREAK
         ENDIF
      ENDIF

   END SEQUENCE

   HPDF_ResetError( hDoc )
   HPDF_Free( hDoc )

   hb_cdpSelect( cOldCodePage )

   WaitWindow()

   CLEAN MEMORY

   DEFAULT lOpen := MsgYesNo( "View " + cPdfFile + " (Y/N) ?", "Please select" )

   IF lRet .AND. lOpen
      wapi_shellExecute( NIL, "open", '"' + cPdfFile + '"' )
   ENDIF

RETURN lRet

********************************************************************************
STATIC FUNCTION PutPageImage( hDoc, hPage, cLogoFile, nPageHeight, nPageWidth )
********************************************************************************
   LOCAL hImage, nResult

   IF Upper( hb_FNameExt( cLogoFile ) ) == ".PNG"
      hImage := HPDF_LoadPngImageFromFile( hDoc, cLogoFile )
   ELSE
      hImage := HPDF_LoadJpegImageFromFile( hDoc, cLogoFile )
   ENDIF
   nResult := HPDF_Page_DrawImage( hPage, hImage, 20, 10, nPageWidth - 30, nPageHeight - 20 )

RETURN ( nResult == HPDF_OK )

********************************************************************************
STATIC FUNCTION PdfSetInfo( hDoc, cTitle, cAuthor, cCreator )
********************************************************************************
   LOCAL dDate := Date()
   LOCAL cTime := Time()

   HPDF_SetInfoAttr( hDoc, HPDF_INFO_AUTHOR, cAuthor )
   HPDF_SetInfoAttr( hDoc, HPDF_INFO_CREATOR, cCreator )
   HPDF_SetInfoAttr( hDoc, HPDF_INFO_TITLE, cTitle )

   HPDF_SetInfoDateAttr( hDoc, HPDF_INFO_CREATION_DATE, ;
      { Year( dDate ), Month( dDate ), Day( dDate ), ;
      Val( SubStr( cTime, 1, 2 ) ), Val( SubStr( cTime, 4, 2 ) ), ;
      Val( SubStr( cTime, 7 ) ) ;
      } )

RETURN NIL

********************************************************************************
STATIC FUNCTION UPDF_Error( cType, hDoc ) // allways return .F.
********************************************************************************
   LOCAL nError := HPDF_GetError( hDoc )
   LOCAL cMessage

   hb_default( @cType, "CREATE" )
   DO CASE
   CASE cType == "CREATE"
      cMessage := "PDF file creation operation failed!"
   CASE cType == "COMPRESS"
      cMessage := "PDF file compress operation failed!"
   CASE cType == "SAVE"
      cMessage := "PDF file save operation failed!"
   OTHERWISE
      cMessage := "Error(s) occured!"
   ENDCASE
   cMessage += hb_eol() + "Error Code: " + hb_ntos( nError ) + " (HPDF)"
   MsgExclamation( cMessage )

RETURN .F.
