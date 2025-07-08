/*
 * MINIGUI - Harbour Win32 GUI library
 *
 * Copyright 2015 Verchenko Andrey <verchenkoag@gmail.com>
*/
#define _HMG_OUTLOG

#include "minigui.ch"
//////////////////////////////////////////////////////////////////////
Function ModeSizeFont()
LOCAL nSize := 10, nH := GetDesktopHeight()

   IF nH == 600
      nSize := IIF(LargeFonts(),10,12)
   ELSEIF nH == 768
      nSize := IIF(LargeFonts(),12,14)
   ELSEIF nH >= 800 .AND. nH <= 900
      nSize := IIF(LargeFonts(),12,16)
   ELSEIF (nH >= 1050 .AND. nH <= 1080)
      nSize := IIF(LargeFonts(),14,18)
   ELSEIF nH == 1152
      nSize := IIF(LargeFonts(),16,20)
   ELSEIF nH >= 1200
      nSize := IIF(LargeFonts(),18,20)
   ELSE
      nSize := 12
   ENDIF

RETURN nSize

//////////////////////////////////////////////////////////////////////
Function ModeSizeIco()
LOCAL nSize := 24, nH := GetDesktopHeight()

   IF ( nH >= 600 .AND. nH < 768 )
      nSize := 32
   ELSEIF nH == 768
      nSize := 32
   ELSEIF ( nH > 768  .AND. nH <= 800 )
      nSize := 48
   ELSEIF ( nH > 800  .AND. nH < 1050 )
      nSize := 54
   ELSEIF ( nH >= 1050 .AND. nH <= 1080 )
      nSize := 64
   ELSEIF ( nH > 1080 .AND. nH <= 1152 )
      nSize := 72
   ELSEIF nH >= 1200
      nSize := 128
   ENDIF

RETURN nSize

//////////////////////////////////////////////////////////////////////
// высота отступа в зависимости от разрешения экрана
Function ModeSizeHeight()
LOCAL nW1 := 2, nH := GetDesktopHeight()

   IF ( nH >= 600 .AND. nH < 768 )
      nW1 := 6 - IIF( Large2Fonts(), 2, 0 )
   ELSEIF nH == 768
      nW1 := 7 - IIF( Large2Fonts(), 3, 0 )
   ELSEIF ( nH > 768  .AND. nH <= 800 )
      nW1 := 8 - IIF( Large2Fonts(), 4, 0 )
   ELSEIF ( nH > 800  .AND. nH < 1050 )
      nW1 := 10 - IIF( Large2Fonts(), 5, 0 )
   ELSEIF ( nH >= 1050 .AND. nH <= 1080 )
      nW1 := 14 - IIF( Large2Fonts(), 6, 0 )
   ELSEIF ( nH > 1080 .AND. nH <= 1152 )
      nW1 := 16 - IIF( Large2Fonts(), 10, 0 )
   ELSEIF nH >= 1200
      nW1 := 20 - IIF( Large2Fonts(), 10, 0 )
   ENDIF

RETURN nW1

////////////////////////////////////////////////////////////////////////////////
FUNCTION mySizeTBDir(cForm,cObj)
   LOCAL cText, cFont, lBold, nFSize, nWObj, nHobj

   cText   := GetProperty( cForm , cObj, "Value"        )
   cFont   := GetProperty( cForm , cObj, "FontName"     )
   lBold   := GetProperty( cForm , cObj, "FontBold"     )
   nWobj   := GetProperty( cForm , cObj, "ClientWidth"  ) - 1
   nHobj   := GetProperty( cForm , cObj, "ClientHeight" ) - 1
   cText   := ALLTRIM(cText)
   IF LEN(cText) > 0
      nFSize  := GetFontSize4Text( cText, cFont, , lBold, nWObj, nHobj )
      //nFSize  := FontSizeMaxAutoFit( cText, cFont, lBold, nWObj, nHobj )
      SetProperty( cForm , cObj, "Fontsize", nFSize ) // change font size
   ENDIF

Return Nil

*----------------------------------------------------------------------------*
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

*-----------------------------------------------------------------------------*
#define FR_PRIVATE   0x10
#define FR_NOT_ENUM  0x20

Function AddFont(cFontFile)
   Return AddFontResourceEx( cFontFile, FR_PRIVATE+FR_NOT_ENUM, 0 )

*-----------------------------------------------------------------------------*
Function RemoveFont(cFontFile)
   Return RemoveFontResourceEx( cFontFile, FR_PRIVATE+FR_NOT_ENUM, 0 )

*-----------------------------------------------------------------------------*
DECLARE DLL_TYPE_INT AddFontResourceEx ( DLL_TYPE_LPCTSTR lpszFilename, DLL_TYPE_DWORD flag, DLL_TYPE_LPVOID pdv ) IN GDI32.DLL
DECLARE DLL_TYPE_BOOL RemoveFontResourceEx ( DLL_TYPE_LPCTSTR lpFileName, DLL_TYPE_DWORD flag, DLL_TYPE_LPVOID pdv ) IN GDI32.DLL
*-----------------------------------------------------------------------------*

////////////////////////////////////////////////////////////////////////////
FUNCTION ToFontSelectResources( cFontFile, cFontPath )
   LOCAL cMsg, lRet := .F., cDiskTo := cFontPath
   LOCAL nResult, cResName := CharRepl(".", cFontFile, "_")

   IF hb_FileExists( cDiskTo + cFontFile )
      // Файл уже есть на диске
      lRet := .T.
      RETURN lRet
   ENDIF

   // Unloading function of resource
   nResult := RCDataToFile( cResName, cDiskTo + cFontFile, "CUSTOM" )
   IF nResult > 0

      IF hb_FileExists( cDiskTo + cFontFile )
         // Файл успешно записан на диск
         lRet := .T.
      ENDIF

   ELSE
      cMsg := "Ошибка извлечения файла !;" + cDiskTo + cFontFile + ";"
      cMsg += "RCDataToFile() - Code: " + hb_NtoS( nResult ) + ";"
      cMsg += PROCNL(0) + ";"
      cMsg += PROCNL(1) + ";"
      AlertStop( cMsg, "Ошибка" )
   ENDIF

RETURN lRet

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

//////////////////////////////////////////////////////////////////
// Функция вернет максимальный размер фонта
// для заданной строке по ширине и высоте для ОДНОЙ строки
FUNCTION FontSizeMaxAutoFit( cText, cFontName, lBold, nWidth, nHeight )
   LOCAL nTxtWidth, nFSize, lExit := .T.
   DEFAULT lBold := .F. , nHeight := 12

   cText := cText + "AA" // для отступов слева и справа
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

///////////////////////////////////////////////////////////////////////////////
FUNCTION GetTxtWidth( cText, nFontSize, cFontName, lBold )  // получить Width текста
   LOCAL hFont, nWidth
   DEFAULT cText     := REPL('A', 2)        ,  ;
           cFontName := _HMG_DefaultFontName,  ;   // из MiniGUI.Init()
           nFontSize := _HMG_DefaultFontSize,  ;   // из MiniGUI.Init()
           lBold     := .F.

   IF Valtype(cText) == 'N'
      cText := repl('A', cText)
   ENDIF

   hFont  := InitFont(cFontName, nFontSize, lBold)
   nWidth := GetTextWidth(0, cText, hFont)         // ширина текста
   DeleteObject (hFont)

   RETURN nWidth

///////////////////////////////////////////////////////////////////////////////
FUNCTION GetTxtHeight( cText, nFontSize, cFontName, lBold )  // получить Height текста
   LOCAL hFont, nHeight
   DEFAULT cText     := "B"                 ,  ;
           cFontName := _HMG_DefaultFontName,  ;   // из MiniGUI.Init()
           nFontSize := _HMG_DefaultFontSize,  ;   // из MiniGUI.Init()
           lBold     := .F.

   hFont := InitFont( cFontName, nFontSize, lBold )
   nHeight := GetTextHeight( 0, cText , hFont )    // высота шрифта
   DeleteObject( hFont )

   RETURN nHeight

///////////////////////////////////////////////////////////////////////////////////////////
// Функция проверки установлен ли БОЛЬШОЙ фонт в настройках системы
// Пример вызова:        nSizeFont := IIF(Large2Fonts(),9,11)
FUNCTION Large2Fonts()
LOCAL hDC, nPixelX, lRet := .F.
hDC := CreateDC( "DISPLAY", "", "" )
nPixelX := GetDevCaps( hDC )
DeleteDC( hDc )
IF nPixelX > 100
   lRet := .T.
ENDIF
RETURN (lRet)

///////////////////////////////////////////////////////////////////////////////////////////
// Функция проверки установлен ли БОЛЬШОЙ фонт в настройках системы
// Пример вызова:        nModeLF := LargeFontsMode()
FUNCTION LargeFontsMode()
LOCAL hDC, nPixelX, lRet := .F.
hDC := CreateDC( "DISPLAY", "", "" )
nPixelX := GetDevCaps( hDC )
DeleteDC( hDc )
RETURN nPixelX

///////////////////////////////////////////////////////////////////////////////////////////
// Функция проверки установлен ли БОЛЬШОЙ фонт в настройках системы
// Пример вызова:        nSizeFont := IIF(LargeFonts(),9,11)
FUNCTION LargeFonts()
LOCAL hDC, nPixelX
hDC := CreateDC( "DISPLAY", "", "" )
nPixelX := GetDevCaps( hDC )
DeleteDC( hDc )
RETURN (nPixelX == 120)

#pragma BEGINDUMP
#include <windows.h>
#include "hbapi.h"
HB_FUNC( CREATEDC )
{
   hb_retnl( ( LONG ) CreateDC( hb_parc( 1 ), hb_parc( 2 ), hb_parc( 3 ), 0 ) );
}
HB_FUNC( DELETEDC )
{
   hb_retl( DeleteDC( ( HDC ) hb_parnl( 1 ) ) );
}
HB_FUNC ( GETDEVCAPS )
{
 INT      ix;
 HDC      hdc;
 hdc = ( HDC ) hb_parnl( 1 );

 ix  = GetDeviceCaps( hdc, LOGPIXELSX );

 hb_retni( (UINT) ix );
}
#pragma ENDDUMP

