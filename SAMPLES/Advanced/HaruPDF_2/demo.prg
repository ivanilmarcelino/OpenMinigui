/*
 * MiniGUI HaruPDF Class Demo
 *
 * (c) 2016-2021 Grigory Filatov <gfilatov@inbox.ru>
*/

#include "minigui.ch"

#include "pdfclass.prg"

PROCEDURE Main
  
	DEFINE WINDOW Form_1 ;
		AT 0,0 ;
		WIDTH 400 ;
		HEIGHT 200 ;
		TITLE 'HaruPDF Class Demo' ;
		MAIN ;
		ON INTERACTIVECLOSE MsgYesNo ( 'Are You Sure ?', 'Exit' )

		ON KEY ESCAPE ACTION ThisWindow.Release

		DEFINE BUTTON Button_1
			ROW	10
			COL	10
			WIDTH	180
			CAPTION 'Generate PDF Portrait'
			ACTION Test( PDFCLASS_PORTRAIT )
		END BUTTON

		DEFINE BUTTON Button_2
			ROW	50
			COL	10
			WIDTH	180
			CAPTION 'Generate PDF Landscape'
			ACTION Test( PDFCLASS_LANDSCAPE )
		END BUTTON

		DEFINE BUTTON Button_3
			ROW	90
			COL	10
			WIDTH	180
			CAPTION 'Generate TEXT List'
			ACTION Test( PDFCLASS_TXT )
		END BUTTON

	END WINDOW

	CENTER WINDOW Form_1
	ACTIVATE WINDOW Form_1

RETURN


PROCEDURE Test( nType )

   LOCAL nCont

   WITH OBJECT PDFClass():New()

      :SetType( nType )
      :acHeader := { "TEST REPORT" + Str( nType, 2 ) + "   " + TxtSaida()[nType] }
      :cFileName := "test" + Str( nType, 1 ) + "." + iif( nType < 3, "pdf", "txt" )

      :Begin()
      //            cAuthor,        cCreator,       cTitle,        cSubject
      :SetInfo( 'Jose Quintas', 'MiniGUI Demo', 'TEST REPORT', TxtSaida()[nType] )

      FOR nCont := 1 TO 1000

         :MaxRowTest()
         :DrawText( :nRow++, 0, nCont )

      NEXT

      :End()

   END WITH

RETURN


FUNCTION TxtSaida()

RETURN { "PDF Portrait", "PDF Landscape", "Matrix" }
