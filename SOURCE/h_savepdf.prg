/**********************************************************
 * Harbour-MiniGUI source code
 * Project      : Simple PDF Creator
 * Created      : 09/15/2016, 17:36
 * Author (c)   : Pete D.
 * Last Update  : 10:20 pm 15/2/2022 by: (Pete D.)
 **********************************************************/

#ifdef __XHARBOUR__
#define __MINIPRINT__
#define _RPTGEN_
#endif

#include "hmg.ch"
#include "harupdf.ch"

#ifndef __XHARBOUR__
REQUEST HB_CODEPAGE_UTF8
#else
#xtranslate hb_eol() => hb_OsNewLine()
#xtranslate WAPI_SHELLEXECUTE([<x,...>]) => SHELLEXECUTE(<x>)
#endif

#define MAX_IMAGE 20

/*
 * FUNCTION _CreatePdf(aPages, cPdfFile, lOpen, cTitle)
 *
 * Creates a PDF file from a series of image files.
 *
 * Parameters:
 *   aPages (ARRAY): An array of image file paths to be included in the PDF.
 *   cPdfFile (CHARACTER): The path of the output PDF file.
 *   lOpen (LOGICAL, optional): Whether to open the PDF file after creation. Defaults to .T. (true).
 *   cTitle (CHARACTER, optional): The title of the PDF document. Defaults to "Untitled".
 *
 * Return Value:
 *   LOGICAL: Returns .T. if the PDF was created successfully, otherwise .F.
 *
 * Purpose:
 *   This function generates a PDF file from a series of image files. It checks for necessary inputs, initializes the PDF document,
 *   adds each image as a page in the PDF, and saves the PDF file. Optionally, it can open the PDF file after creation.
 *
 * Notes:
 *   The function uses the Harbour PDF library (harupdf.ch) to generate the PDF.
 *   It handles errors and ensures the output file does not already exist unless confirmed by the user.
 */
FUNCTION _CreatePdf( aPages, cPdfFile, lOpen, cTitle )

   LOCAL hPdfDocument, hPage
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
   LOCAL nPage

   // Check for empty parameters
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

   // Check if the output file already exists
   IF hb_FileExists( cPdfFile )
      IF ! MsgYesNo( "File " + cPdfFile + " already exists!" + hb_eol() + "Overwrite?", "Warning!" )
         RETURN .F.
      ENDIF
   ENDIF

   CLEAN MEMORY

   // Create PDF document
   TRY
      IF ( hPdfDocument := HPDF_New() ) == NIL
         lRet := UPDF_Error( "CREATE", hPdfDocument )
         BREAK
      ENDIF

      cOldCodePage := hb_cdpSelect( "UTF8" )
      cCodePage := "UTF-8"
      cAuthor := GetUserName()

      WaitWindow( "Creating PDF file", .T. )

      // Set compression mode
      IF HPDF_SetCompressionMode( hPdfDocument, HPDF_COMP_ALL ) <> HPDF_OK
         lRet := UPDF_Error( "COMPRESS", hPdfDocument )
         BREAK
      ENDIF

      HPDF_UseUTFEncodings( hPdfDocument )
      HPDF_SetCurrentEncoder( hPdfDocument, cCodePage )
      PdfSetInfo( hPdfDocument, cTitle, cAuthor, cCreator )

      nPage := 0

      // Start main loop to add each image as a page in the PDF
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

         // Create new page
         hPage := HPDF_AddPage( hPdfDocument )
         HPDF_Page_SetSize( hPage, page_size, page_orient )
         nPageHeight := HPDF_Page_GetHeight( hPage )
         nPageWidth := HPDF_Page_GetWidth( hPage )

         // Put page picture
         lRet := PutPageImage( hPdfDocument, hPage, cImageFile, nPageHeight, nPageWidth )

#ifndef __DEBUG__
         FErase( cImageFile )
#endif
         IF ! lRet
            MsgExclamation( "There was an error with image file:" + hb_eol() + cPage + "!", "Warning" )
            lRet := .F.
            EXIT
         ENDIF
      END

      IF lRet
         IF !( HPDF_SaveToFile( hPdfDocument, cPdfFile ) == 0 )
            lRet := UPDF_Error( "SAVE", hPdfDocument )
            BREAK
         ENDIF
      ENDIF

   CATCH
      // Handle any errors that occurred during PDF creation
      lRet := .F.
   FINALLY
      HPDF_ResetError( hPdfDocument )
      HPDF_Free( hPdfDocument )
      hb_cdpSelect( cOldCodePage )
      WaitWindow()
      CLEAN MEMORY
   END TRY

   DEFAULT lOpen := MsgYesNo( "View " + cPdfFile + " (Y/N) ?", "Please select" )

   IF lRet .AND. lOpen
      wapi_shellExecute( NIL, "open", '"' + cPdfFile + '"' )
   ENDIF

RETURN lRet

/*
 * STATIC FUNCTION PutPageImage(hPdfDocument, hPage, cLogoFile, nPageHeight, nPageWidth)
 *
 * Places an image on a PDF page.
 *
 * Parameters:
 *   hPdfDocument (HANDLE): Handle to the PDF document.
 *   hPage (HANDLE): Handle to the PDF page.
 *   cLogoFile (CHARACTER): Path to the image file.
 *   nPageHeight (NUMERIC): Height of the PDF page.
 *   nPageWidth (NUMERIC): Width of the PDF page.
 *
 * Return Value:
 *   LOGICAL: Returns .T. if the image was placed successfully, otherwise .F.
 *
 * Purpose:
 *   This function loads an image file and places it on a specified PDF page. It supports PNG and JPEG image formats.
 *
 * Notes:
 *   The function uses the Harbour PDF library (harupdf.ch) to place the image on the page.
 */
STATIC FUNCTION PutPageImage( hPdfDocument, hPage, cLogoFile, nPageHeight, nPageWidth )

   LOCAL hImage, nResult

   IF Upper( hb_FNameExt( cLogoFile ) ) == ".PNG"
      hImage := HPDF_LoadPngImageFromFile( hPdfDocument, cLogoFile )
   ELSE
      hImage := HPDF_LoadJpegImageFromFile( hPdfDocument, cLogoFile )
   ENDIF

   nResult := HPDF_Page_DrawImage( hPage, hImage, 20, 10, nPageWidth - 30, nPageHeight - 20 )

RETURN ( nResult == HPDF_OK )

/*
 * STATIC FUNCTION PdfSetInfo(hPdfDocument, cTitle, cAuthor, cCreator)
 *
 * Sets the information attributes of a PDF document.
 *
 * Parameters:
 *   hPdfDocument (HANDLE): Handle to the PDF document.
 *   cTitle (CHARACTER): Title of the PDF document.
 *   cAuthor (CHARACTER): Author of the PDF document.
 *   cCreator (CHARACTER): Creator of the PDF document.
 *
 * Return Value:
 *   NIL
 *
 * Purpose:
 *   This function sets the information attributes of a PDF document, including the title, author, creator, and creation date.
 *
 * Notes:
 *   The function uses the Harbour PDF library (harupdf.ch) to set the information attributes.
 */
STATIC FUNCTION PdfSetInfo( hPdfDocument, cTitle, cAuthor, cCreator )

   LOCAL dDate := Date()
   LOCAL cTime := Time()

   HPDF_SetInfoAttr( hPdfDocument, HPDF_INFO_AUTHOR, cAuthor )
   HPDF_SetInfoAttr( hPdfDocument, HPDF_INFO_CREATOR, cCreator )
   HPDF_SetInfoAttr( hPdfDocument, HPDF_INFO_TITLE, cTitle )
   HPDF_SetInfoDateAttr( hPdfDocument, HPDF_INFO_CREATION_DATE, ;
      { Year( dDate ), Month( dDate ), Day( dDate ), ;
      Val( SubStr( cTime, 1, 2 ) ), Val( SubStr( cTime, 4, 2 ) ), ;
      Val( SubStr( cTime, 7 ) ) ;
      } )

RETURN NIL

/*
 * STATIC FUNCTION UPDF_Error(cType, hPdfDocument)
 *
 * Handles errors that occur during PDF creation.
 *
 * Parameters:
 *   cType (CHARACTER): Type of operation that failed (e.g., "CREATE", "COMPRESS", "SAVE").
 *   hPdfDocument (HANDLE): Handle to the PDF document.
 *
 * Return Value:
 *   LOGICAL: Always returns .F.
 *
 * Purpose:
 *   This function handles errors that occur during PDF creation, displaying an appropriate error message based on the type of operation that failed.
 *
 * Notes:
 *   The function uses the Harbour PDF library (harupdf.ch) to get the error code and display an error message.
 */
STATIC FUNCTION UPDF_Error( cType, hPdfDocument ) // always return .F.

   LOCAL nError := HPDF_GetError( hPdfDocument )
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
      cMessage := "Error(s) occurred!"
   ENDCASE

   cMessage += hb_eol() + "Error Code: " + hb_ntos( nError ) + " (HPDF)"
   MsgExclamation( cMessage )

RETURN .F.
