/*
 * Harbour MiniGUI Clipboard Test
 * (c) 2002-2009 Roberto Lopez <harbourminigui@gmail.com>
 *
 * Revised by Vladimir Chumachenko <ChVolodymyr@yandex.ru>
 */

#include "minigui.ch"


#translate MSGINFO_( <cMessage>, <cTitle> ) ;
      = > ;
      MsgInfo( < cMessage >, < cTitle >, , .F., .F. )


/*****
*
*   Set tests for:
*   - clipboard (get/store text, clear)
*   - Desktop size (width, height)
*   - location of system folders (Desktop, My documents, Program Files,
*     Windows, System32, Tmp)
*   - the name of the default printer
*
*/

PROCEDURE MAIN

   LOCAL cFile := "test.txt"

   IF ! File( cFile )
      Create_txt( cFile )
   ENDIF

   DEFINE WINDOW wDemo ;
         AT 0, 0 ;
         WIDTH 400 ;
         HEIGHT 400 ;
         TITLE 'Clipboard & Others Tests' ;
         MAIN ;
         NOMAXIMIZE ;
         NOSIZE

      DEFINE TAB tbTest ;
            AT 5, 5 ;
            WIDTH 380 ;
            HEIGHT 360 ;
            HotTrack

         PAGE 'Clipboard' // Operations with Clipboard

            @ 35, 20 BUTTONEX btnGetClip ;
               CAPTION 'Get' ;
               WIDTH 50 ;
               FONTCOLOR BROWN ;
               Action ( hb_MemoWrit( "test_copy.txt", System.Clipboard ), MSGINFO_( System.Clipboard, 'Text in clipboard' ) ) ;
               BACKCOLOR WHITE

            @ 35, 80 BUTTONEX btnSetClip ;
               CAPTION 'Set' ;
               WIDTH 50 ;
               FONTCOLOR BROWN ;
               ACTION System.Clipboard := hb_MemoRead( cFile ) ;
               BACKCOLOR WHITE

            @ 35, 180 BUTTONEX btnClearClip ;
               CAPTION 'Clear' ;
               WIDTH 50 ;
               FONTCOLOR RED ;
               Action {|| ClearClipboard(), MSGINFO_( 'Clipboard is cleaned!', 'Warning' ) } ;
               BACKCOLOR WHITE

            @ 35, 280 BUTTONEX btnTag ;
               CAPTION '{...}' ;
               WIDTH 50 ;
               FONTCOLOR BLUE ;
               BOLD ;
               ACTION Bracketed() ;
               BACKCOLOR WHITE

            @ 75, 20 EDITBOX edtText ;
               WIDTH 340 ;
               HEIGHT 260 ;
               VALUE 'Highlight the text in a word or more, and then click button "{...}"' ;
               NoHScroll
         END PAGE

         PAGE 'Desktop' // Desktop sizes

            @ 60, 110 BUTTONEX btnWidth ;
               CAPTION 'Get Desktop Width' ;
               WIDTH 140 ;
               ACTION MSGINFO_( System.DesktopWidth, 'Desktop width' ) ;
               FONTCOLOR BROWN ;
               BACKCOLOR WHITE

            @ 130, 110 BUTTONEX btnHeight ;
               CAPTION 'Get Desktop Height' ;
               WIDTH 140 ;
               ACTION MSGINFO_( System.DesktopHeight, 'Desktop height' ) ;
               FONTCOLOR BROWN ;
               BACKCOLOR WHITE

         END PAGE

         PAGE 'System Folders' // System Folders location

            @ 60, 95 BUTTONEX btnDesktopPath ;
               CAPTION 'Get Desktop Folder' ;
               WIDTH 170 ;
               ACTION MSGINFO_( System.DesktopFolder, 'Path to Desktop' ) ;
               FONTCOLOR BROWN ;
               BACKCOLOR WHITE

            @ 105, 95 BUTTONEX btnMyDocPath ;
               CAPTION 'Get MyDocuments Folder' ;
               WIDTH 170 ;
               ACTION MSGINFO_( System.MyDocumentsFolder, 'Path to My Documents' ) ;
               FONTCOLOR BROWN ;
               BACKCOLOR WHITE

            @ 150, 95 BUTTONEX btnProgPath ;
               CAPTION 'Get Program Files Folder' ;
               WIDTH 170 ;
               ACTION MSGINFO_( System.ProgramFilesFolder, 'Path to Program Files' ) ;
               FONTCOLOR BROWN ;
               BACKCOLOR WHITE

            @ 195, 95 BUTTONEX btnWinPath ;
               CAPTION 'Get Windows Folder' ;
               WIDTH 170 ;
               ACTION MSGINFO_( System.WindowsFolder, 'Path to Windows folder' ) ;
               FONTCOLOR BROWN ;
               BACKCOLOR WHITE

            @ 240, 95 BUTTONEX btnSysPath ;
               CAPTION 'Get System Folder' ;
               WIDTH 170 ;
               ACTION MSGINFO_( System.SystemFolder, 'Path to System32 folder' ) ;
               FONTCOLOR BROWN ;
               BACKCOLOR WHITE

            @ 285, 95 BUTTONEX btnTempPath ;
               CAPTION 'Get Temp Folder' ;
               WIDTH 170 ;
               ACTION MSGINFO_( System.TempFolder, 'Path to Temp folder' ) ;
               FONTCOLOR BROWN ;
               BACKCOLOR WHITE

         END PAGE

         PAGE 'Printer' // Show Default Printer

            @ 60, 110 BUTTONEX btnDefPrinter ;
               CAPTION 'Get Default Printer' ;
               WIDTH 140 ;
               ACTION MSGINFO_( System.DefaultPrinter, 'Printer by default' ) ;
               FONTCOLOR BROWN ;
               BACKCOLOR WHITE

         END PAGE

         PAGE 'Add Bitmap' // Put/Get Bitmap image to/from Clipboard

            @ 60, 110 BUTTONEX btnSetGetBitmap ;
               CAPTION 'Get Bitmap from Clipboard' ;
               WIDTH 160 ;
               Action ( SetClpbrdImage( 'harbour.bmp' ), GetClpbrdImage() ) ;
               FONTCOLOR BROWN ;
               BACKCOLOR WHITE

            @ 120, 40 LABEL lblText VALUE "" AUTOSIZE

            @ 150, 30 IMAGE imgBitmap PICTURE "" TRANSPARENT

         END PAGE

      END TAB

      ON KEY ESCAPE ACTION ThisWindow.Release()

   END WINDOW

   CENTER WINDOW wDemo
   ACTIVATE WINDOW wDemo

RETURN


/******
*
*       Bracketed()
*
*       Cut text and paste it in the brackets
*
*/

STATIC PROCEDURE Bracketed

   LOCAL nHandle := GetControlHandle( 'edtText', 'wDemo' ), ;
      cNewText, ;
      cText

   ClearClipboard( Application.Handle )
   RichEditBox_SelCut( nHandle )

   cText := AllTrim( System.Clipboard )
   // OR
   // cText := AllTrim( RetrieveTextFromClipboard() )
   cNewText := ( '{' + cText + '}' )
   If ! Empty( cText )
      cNewText += ' '
   ENDIF

   CopyToClipboard( cNewText )
   RichEditBox_SelPaste( nHandle )

   wDemo.edtText.SetFocus

RETURN


STATIC FUNCTION Create_txt( cFile )

   LOCAL t

   t := 'This exception applies only to the code released by the Harbour' + CRLF
   t += 'Project under the name Harbour.  If you copy code from other' + CRLF
   t += 'Harbour Project or Free Software Foundation releases into a copy of' + CRLF
   t += 'Harbour, as the General Public License permits, the exception does' + CRLF
   t += 'not apply to the code that you add in this way.  To avoid misleading' + CRLF
   t += 'anyone as to the status of such modified files, you must delete' + CRLF
   t += 'this exception notice from them.' + CRLF

   hb_MemoWrit( cFile, t )

RETURN NIL


PROCEDURE SetClpbrdImage ( cImage )

   LOCAL hBitmap

   hBitmap := LoadBitmap( cImage )

   If ! Empty( hBitmap )
      CopyBmpToClipboard( hBitmap )
      DeleteObject( hBitmap )
   ENDIF

RETURN


PROCEDURE GetClpbrdImage ()

   LOCAL hBitmap

   hBitmap := RetrieveBmpFromClipboard()

   If ! Empty( hBitmap )
      wDemo.imgBitmap.hBitmap := hBitmap
      ImgSize()
   ENDIF

RETURN


PROCEDURE ImgSize

   LOCAL n

   wDemo.lblText.VALUE := "Width: " + hb_ntos( wDemo.imgBitmap.WIDTH ) + "    Height: " + hb_ntos( wDemo.imgBitmap.HEIGHT )

   IF IsControlDefined ( frmImage, wDemo ) == .T.
      wDemo.frmImage.RELEASE
   ENDIF

   n := 2
   @ ( wDemo.imgBitmap.ROW - n ), ( wDemo.imgBitmap.COL - n ) FRAME frmImage OF wDemo ;
      WIDTH ( wDemo.imgBitmap.WIDTH + 2 * n ) ;
      HEIGHT ( wDemo.imgBitmap.HEIGHT + 2 * n ) ;
      TRANSPARENT

   wDemo.tbTest.AddControl( 'frmImage', 5, wDemo.frmImage.ROW, wDemo.frmImage.COL )

RETURN

/*
 * C-level
 */

#pragma BEGINDUMP

#include <mgdefs.h>

// Minigui Resources control system
void RegisterResource( HANDLE hResource, LPCSTR szType );

HBITMAP DuplicateBitmap( HBITMAP hbmpSrc )
{
   HBITMAP hbmpOldSrc, hbmpOldDest, hbmpNew;
   HDC     hdcSrc, hdcDest;
   BITMAP  bmp;

   hdcSrc = CreateCompatibleDC( NULL );
   hdcDest = CreateCompatibleDC( hdcSrc );

   GetObject( hbmpSrc, sizeof( BITMAP ), &bmp );

   hbmpOldSrc = (HBITMAP) SelectObject( hdcSrc, hbmpSrc );

   hbmpNew = CreateCompatibleBitmap( hdcSrc, bmp.bmWidth, bmp.bmHeight );

   hbmpOldDest = (HBITMAP) SelectObject( hdcDest, hbmpNew );

   BitBlt( hdcDest, 0, 0, bmp.bmWidth, bmp.bmHeight, hdcSrc, 0, 0, SRCCOPY);

   SelectObject( hdcDest, hbmpOldDest );
   SelectObject( hdcSrc, hbmpOldSrc );

   DeleteDC( hdcDest );
   DeleteDC( hdcSrc );

   return hbmpNew;
}

HB_FUNC( COPYBMPTOCLIPBOARD ) // CopyBmpToClipboard(hBitmap) store hBitmap in Windows clipboard
{
   if( ! OpenClipboard( GetActiveWindow() ) )
   {
      hb_retl( HB_FALSE );
      return;
   }

   hb_retl( SetClipboardData( CF_BITMAP, DuplicateBitmap( hmg_par_raw_HBITMAP( 1 ) ) ) != NULL );

   CloseClipboard();
}

HB_FUNC( RETRIEVEBMPFROMCLIPBOARD )
{
   if( ! OpenClipboard( GetActiveWindow() ) )
   {
      hb_retl( HB_FALSE );
      return;
   }

   if( IsClipboardFormatAvailable( CF_BITMAP ) )
   {
      HBITMAP hBitmap;

      hBitmap = DuplicateBitmap( ( HBITMAP ) GetClipboardData( CF_BITMAP ) );

      RegisterResource( hBitmap, "BMP" );
      hmg_ret_raw_HANDLE( hBitmap );
   }
   else
      hb_retnl( 0 );

   CloseClipboard();
}

#pragma ENDDUMP
