/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2018 Sergej Kiselev <bilance@bilance.lv>
 *
*/

#include "hmg.ch"

*----------------------------------------------------------------------------*
FUNC SetsEnv()
*----------------------------------------------------------------------------*

   SET CENTURY      ON
   SET DATE         GERMAN
   SET DELETED      ON
   SET EXCLUSIVE    ON
   SET EPOCH TO     2000
   SET EXACT        ON
   SET SOFTSEEK     ON

   SET NAVIGATION   EXTENDED
   SET FONT         TO "Arial", 14
   SET DEFAULT ICON TO "1MAIN_ICO"

   DEFINE FONT DlgFont  FONTNAME "Verdana" SIZE 16  // for HMG_Alert()

   // --------------------------------
   SET OOP ON
   // --------------------------------

RETURN NIL


/*
 * MINIGUI - Harbour Win32 GUI library
 *
 * Copyright 2015-2018 Verchenko Andrey <verchenkoag@gmail.com>
 * Many thanks for your help - forum http://clipper.borda.ru
*/

*----------------------------------------------------------------------------*
FUNCTION GetTxtWidth( cText, nFontSize, cFontName )  // получить Width текста
*----------------------------------------------------------------------------*
   LOCAL hFont, nWidth
   DEFAULT cText     := REPL('A', 2)        ,  ;
           cFontName := _HMG_DefaultFontName,  ;   // из MiniGUI.Init()
           nFontSize := _HMG_DefaultFontSize       // из MiniGUI.Init()

   IF Valtype(cText) == 'N'
      cText := repl('A', cText)
   ENDIF

   hFont  := InitFont(cFontName, nFontSize)
   nWidth := GetTextWidth(0, cText, hFont)        // ширина текста 
   DeleteObject (hFont)                    

   RETURN nWidth
