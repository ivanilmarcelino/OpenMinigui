/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2019-2023 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2015-2023 Verchenko Andrey <verchenkoag@gmail.com>
 * Many thanks for your help - forum http://clipper.borda.ru
 *
*/

#include "hmg.ch"
#include "hbdyn.ch"

//////////////////////////////////////////////////////////////////////////////
FUNCTION GetTxtWidth( cText, nFontSize, cFontName, lBold )  // �������� Width ������
   LOCAL hFont, nWidth
   DEFAULT cText     := REPL('A', 2)        ,  ;
           cFontName := _HMG_DefaultFontName,  ;   // �� MiniGUI.Init()
           nFontSize := _HMG_DefaultFontSize,  ;   // �� MiniGUI.Init()
           lBold     := .F.

   IF Valtype(cText) == 'N'
      cText := repl('A', cText)
   ENDIF

   hFont  := InitFont(cFontName, nFontSize, lBold)
   nWidth := GetTextWidth(0, cText, hFont)         // ������ ������
   DeleteObject (hFont)

   RETURN nWidth

///////////////////////////////////////////////////////////////////////////////
FUNCTION GetTxtHeight( cText, nFontSize, cFontName, lBold )  // �������� Height ������
   LOCAL hFont, nHeight
   DEFAULT cText     := "B"                 ,  ;
           cFontName := _HMG_DefaultFontName,  ;   // �� MiniGUI.Init()
           nFontSize := _HMG_DefaultFontSize,  ;   // �� MiniGUI.Init()
           lBold     := .F.

   hFont := InitFont( cFontName, nFontSize, lBold )
   nHeight := GetTextHeight( 0, cText , hFont )    // ������ ������
   DeleteObject( hFont )

   RETURN nHeight

///////////////////////////////////////////////////////////////////
// ������� ������� �� ������������ ������ �����
FUNCTION SetFontSizeText(cForm, cObj)
LOCAL cFText, cFName, lFBold, nWidth, nHeight, nFSize, cFType

   cFType := GetProperty( cForm, cObj, "Type" )
   IF cFType == "LABEL" .OR. cFType == "GETBOX" .OR. cFType == "TEXTBOX"
      cFText := GetProperty( cForm, cObj, "Value"   )
   ELSE
      cFText := GetProperty( cForm, cObj, "Caption" )
   ENDIF
   cFName  := GetProperty( cForm, cObj, "FontName"     )
   lFBold  := GetProperty( cForm, cObj, "FontBold"     )
   nWidth  := GetProperty( cForm, cObj, "ClientWidth"  ) - 5
   nHeight := GetProperty( cForm, cObj, "ClientHeight" ) - 5


   IF LEN(cFText) > 4
      nFSize  := GetFontSize4Text( cFText, cFName, , lFBold, nWidth, nHeight )
      // �������� ������ �����
      SetProperty(cForm, cObj, "Fontsize", nFSize)
   ELSE
      MsgDebug("Error!",LEN(cFText),cFText,ProcNL(1),ProcNL(2))
   ENDIF

RETURN NIL

//////////////////////////////////////////////////////////////////
// ������� ������ ������������ ������ �����
// ��� �������� ������ �� ������ � ������ ��� ��������� �����
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

//////////////////////////////////////////////////////////////////
// ������� ������ ������������ ������ �����
// ��� �������� ������ �� ������ � ������ ��� ����� ������
FUNCTION FontSizeMaxAutoFit( cText, cFontName, lBold, nWidth, nHeight )
   LOCAL nTxtWidth, nFSize, lExit := .T.
   DEFAULT lBold := .F. , nHeight := 12

   cText := cText + "AA" // ��� �������� ����� � ������
   nFSize := 6
   DO WHILE lExit
      nTxtWidth := GetTxtWidth( cText, nFSize, cFontName, lBold )
      IF nTxtWidth >= nWidth
         lExit := .F.
         nFSize--
      ELSE
         nFSize++
      ENDIF
      IF nFSize >= nHeight
         lExit := .F.
         nFSize--
      ENDIF
   ENDDO

RETURN nFSize

//////////////////////////////////////////////////////////////////
// ������� ������ ������������ ������ �����
// ��� ���������� ����� �� ������ ������
FUNCTION FontSizeMaxHeight( nLine, cFontName, lBold, nHeight )
   LOCAL nTxtHeight, nFSize, nHSize, lExit := .T.
   DEFAULT lBold := .F. , nHeight := 22

   nFSize := 6
   DO WHILE lExit
      nHSize := GetTxtHeight( "����", nFSize, cFontName, lBold )  // �������� Height ������
      IF nLine == 2
         nTxtHeight := nLine * nHSize + nHSize/2             // 2 ������
      ELSE
         nTxtHeight := nLine * nHSize + nHSize/2*(nLine-1)   // 3,4,... ������
      ENDIF
      IF nTxtHeight >= nHeight
         lExit := .F.
         nFSize--
      ELSE
         nFSize++
      ENDIF
   ENDDO

RETURN nFSize

/*
 * MINIGUI - Harbour Win32 GUI library
 *
 * Copyright 2015-2018 Verchenko Andrey <verchenkoag@gmail.com>
 * Many thanks for your help - forum http://clipper.borda.ru
*/
*----------------------------------------------------------------------------*
Static Function GetFonts( nCharset )
   LOCAL aFontList := {}
   GetFontList( , , nCharset, , , , @aFontList )
   Return ( aFontList )

*----------------------------------------------------------------------------*
FUNCTION IsFontExist( cFontname, nCharset )
   DEFAULT nCharset := ANSI_CHARSET
   RETURN ( ascan(GetFonts( nCharset ), {|x| x==cFontname}) > 0 )
