/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2025 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * _TBrowse() Разное
*/
#define  _HMG_OUTLOG
#include "hmg.ch"
#include "tsbrowse.ch"
////////////////////////////////////////////////////////////////////////////////
FUNCTION myBigSizeLabel(cForm,cObj)
   LOCAL cText, cFont, lBold, nFSize, nWObj, nHobj

   cText   := GetProperty( cForm , cObj, "Value"        )
   cFont   := GetProperty( cForm , cObj, "FontName"     )
   lBold   := GetProperty( cForm , cObj, "FontBold"     )
   nWobj   := GetProperty( cForm , cObj, "ClientWidth"  ) - 2
   nHobj   := GetProperty( cForm , cObj, "ClientHeight" ) - 2
   IF LEN(cText) > 0
      nFSize  := GetFontSize4Text( cText, cFont, , lBold, nWObj, nHobj )
      SetProperty( cForm , cObj, "Fontsize", nFSize ) // change font size
   ENDIF

Return Nil

//////////////////////////////////////////////////////////////////
// Функция вернет максимальный размер фонта
// для заданной строке по ширине и высоте для НЕСКОЛЬКО строк
FUNCTION GetFontSize4Text( cText, cFontName, nFontSize, lBold, nWmax, nHmax )
   LOCAL hFont, nK := 1, cT := "", nHeig, nWidt
   LOCAL nSize := 6 // App.FontSize

   IF CRLF $ cText
      AEval(hb_ATokens(cText, CRLF), {|t,n| nK := Max( nK, n ), cT := iif( Len( t ) > Len( cT ), t, cT ) })
      cText := cT
   ENDIF

//   nSize := nFontSize
   lBold := !Empty(lBold)
   hFont := InitFont( cFontName, nSize, lBold )
   nHeig := GetTextHeight( 0, cText, hFont ) * nK
   nWidt := GetTextWidth ( 0, cText, hFont )
   DeleteObject( hFont )

   IF     nHeig > nHmax .and. nWidt > nWmax
      DO WHILE .T.
         hFont := InitFont( cFontName, nSize, lBold )
         nHeig := GetTextHeight( 0, cText, hFont ) * nK
         nWidt := GetTextWidth ( 0, cText, hFont )
         DeleteObject( hFont )
         IF nHeig <= nHmax .or. nWidt <= nWmax ; nSize ++ ; EXIT
         ENDIF
         nSize --
      ENDDO
   ELSEIF nHeig < nHmax .and. nWidt < nWmax
      DO WHILE .T.
         hFont := InitFont( cFontName, nSize, lBold )
         nHeig := GetTextHeight( 0, cText, hFont ) * nK
         nWidt := GetTextWidth ( 0, cText, hFont )
         DeleteObject( hFont )
         IF nHeig >= nHmax .or. nWidt >= nWmax ; nSize -- ; EXIT
         ENDIF
         nSize ++
      ENDDO
   ENDIF

RETURN iif( Empty(nFontSize), nSize, Min( nFontSize, nSize ) )

