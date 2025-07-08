#include <hmg.ch>

PROCEDURE Main()

   DEFINE WINDOW MainWindow ;
         AT 0, 0 ;
         WIDTH 400 HEIGHT 220 ;
         MAIN ;
         TITLE "Get Text Width Test" ;
         NOMINIMIZE NOMAXIMIZE ;
         ON INIT MainWin_Onload()

      @ 20, 20 COMBOBOX cboFontName WIDTH 150 HEIGHT 100 ON CHANGE cboFontName_Change()
      @ 60, 20 CHECKBOX chkCheckBold CAPTION "Bold" WIDTH 60 ON CHANGE chkCheckBold_Change()
      @ 60, 90 CHECKBOX chkCheckItalic CAPTION "Italic" WIDTH 60 ON CHANGE chkCheckItalic_Change()

      @ 20, 200 BUTTON cmdGetWidth CAPTION "Show Text Width" WIDTH 120 HEIGHT 28 ON Click cmdGetWidth_Click()
      @ 60, 200 BUTTON cmdDrawRuler CAPTION "Draw Ruler" WIDTH 120 HEIGHT 28 ON Click cmdDrawRuler_Click()
      @ 105, 200 LABEL lblWidth WIDTH 200 HEIGHT 20 VALUE "" FONT "Arial" SIZE 12

      @ 105, 20 LABEL lblSample WIDTH 150 HEIGHT 20 VALUE "A Sample Text" FONT "Arial" SIZE 12
   END WINDOW

   MainWindow.cboFontName.AddItem( "Arial" )
   MainWindow.cboFontName.AddItem( "Courier New" )
   MainWindow.cboFontName.AddItem( "Tahoma" )
   MainWindow.cboFontName.AddItem( "Times New Roman" )
   MainWindow.cboFontName.VALUE := 1

   MainWindow.CENTER
   MainWindow.ACTIVATE

RETURN


PROCEDURE MainWin_Onload

   cmdDrawRuler_Click()
   cmdGetWidth_Click()

RETURN


PROCEDURE chkCheckBold_Change

   MainWindow.lblSample.FONTBOLD := MainWindow.chkCheckBold.VALUE
   cmdGetWidth_Click()

RETURN


PROCEDURE chkCheckItalic_Change

   MainWindow.lblSample.FontItalic := MainWindow.chkCheckItalic.VALUE
   cmdGetWidth_Click()

RETURN


PROCEDURE cboFontName_Change

   LOCAL cFontName, nItemNo

   nItemNo := MainWindow.cboFontName.VALUE
   cFontName := MainWindow.cboFontName.Item( nItemNo )
   MainWindow.lblSample.FONTNAME := cFontName
   MainWin_Onload()

RETURN


PROCEDURE cmdGetWidth_Click

   LOCAL nTextWidth
   LOCAL hFont

   hFont := GetWindowFont( GetControlHandle( "lblSample", "MainWindow" ) )
   nTextWidth := GetTextWidth( 0, MainWindow.lblSample.VALUE, hFont )

   cmdDrawRuler_Click()
   DrawLine( "MainWindow", 140, 20, 140, 20 + nTextWidth, RED )

   MainWindow.lblWidth.VALUE := "Text Width: " + hb_ntos( nTextWidth )
   MainWindow.lblWidth.FONTBOLD := MainWindow.chkCheckBold.VALUE
   MainWindow.lblWidth.FontItalic := MainWindow.chkCheckItalic.VALUE

RETURN

/* Draw ruler (150 pixels width) */
PROCEDURE cmdDrawRuler_Click

   LOCAL nI, nCol, cLabel

   EraseWindow( "MainWindow" )
   DrawLine( "MainWindow", 140, 20, 140, 170 )
   FOR nI := 0 TO 15
      nCol := nI * 10 + 20
      DrawLine( "MainWindow", 135, nCol, 145, nCol )
   NEXT
   FOR nI := 0 TO 3
      nCol := nI * 50 + 20
      DrawLine( "MainWindow", 130, nCol, 150, nCol )
      cLabel := hb_ntos( nI * 50 )
      nCol -= iif( Len( cLabel ) < 2, 2, iif( Len( cLabel ) < 3, 4, iif( Len( cLabel ) < 4, 8, 0 ) ) )
      DrawTextOut( "MainWindow", 150, nCol, cLabel, , , "Arial", 8, .F., .F., .F., .F., .T. )
   NEXT

RETURN
