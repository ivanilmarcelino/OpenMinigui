/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2019-2023 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2015-2023 Verchenko Andrey <verchenkoag@gmail.com>
 * Many thanks for your help - forum http://clipper.borda.ru
 *
*/

#include "hmg.ch"

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
FUNCTION LargeFontPixel()
LOCAL hDC, nPixelX, lRet := .F.
hDC := CreateDC( "DISPLAY", "", "" )
nPixelX := GetDevCaps( hDC )
DeleteDC( hDc )
RETURN nPixelX

///////////////////////////////////////////////////////////////////////////////////////////
// Функция проверки установлен ли БОЛЬШОЙ фонт в настройках системы
// для ХР - 120%, для Win7 - 125%
FUNCTION LargeFontMode()
LOCAL hDC, nPixelX, lRet := .F.
hDC := CreateDC( "DISPLAY", "", "" )
nPixelX := GetDevCaps( hDC )
DeleteDC( hDc )
IF nPixelX > 100
   lRet := .T.
ENDIF
RETURN (lRet)


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


