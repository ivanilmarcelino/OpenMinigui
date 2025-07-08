/*
 * MINIGUI - Harbour Win32 GUI library
 *
 * Copyright 2015-2018 Verchenko Andrey <verchenkoag@gmail.com>
 * Many thanks for your help - forum http://clipper.borda.ru
*/

#include "minigui.ch"

//////////////////////////////////////////////////////////////////////
Function ModeSizeFont()
LOCAL nSize

   IF GetDesktopHeight() >= 600 .AND. GetDesktopHeight() < 720
      nSize := 10
   ELSEIF GetDesktopHeight() >= 720 .AND. GetDesktopHeight() < 768
      nSize := 12
   ELSEIF GetDesktopHeight() >= 768 .AND. GetDesktopHeight() < 800
      nSize := 14
   ELSEIF GetDesktopHeight() >= 800 .AND. GetDesktopHeight() < 1050
      nSize := 14
   ELSEIF GetDesktopHeight() >= 1050 .AND. GetDesktopHeight() <= 1080
      nSize := 16
   ELSEIF GetDesktopHeight() >= 1152 .AND. GetDesktopHeight() < 1200
      nSize := 18
   ELSEIF GetDesktopHeight() >= 1200
      nSize := 20
   ELSE
      nSize := 15
   ENDIF

   // Checking the installation of a BIG font in the system settings
   nSize := nSize - IIF(Large2Fonts(),2,0)

RETURN nSize 

//////////////////////////////////////////////////////////////////
// The function will return the maximum font size for a given string at a given width
FUNCTION FontSizeMaxAutoFit( cText, cFName, nWinWidth )
   LOCAL nTxtWidth, nFSize, lExit := .T.

   nFSize := 6    
   DO WHILE lExit
      nTxtWidth := GetTxtWidth( cText, nFSize, cFName )
      IF nTxtWidth >= nWinWidth 
         lExit := .F.
      ELSE
         nFSize++    
      ENDIF
   ENDDO

   RETURN nFSize

///////////////////////////////////////////////////////////////////////////////
FUNCTION GetTxtWidth( cText, nFontSize, cFontName )  // get Width of the text
   LOCAL hFont, nWidth
   DEFAULT cText     := REPL('A', 2)        ,  ;
           cFontName := _HMG_DefaultFontName,  ;   // from MiniGUI.Init()
           nFontSize := _HMG_DefaultFontSize       // from MiniGUI.Init()

   IF Valtype(cText) == 'N'
      cText := repl('A', cText)
   ENDIF

   hFont  := InitFont(cFontName, nFontSize)
   nWidth := GetTextWidth(0, cText, hFont)        // text width
   DeleteObject (hFont)                    

   RETURN nWidth

///////////////////////////////////////////////////////////////////////////////
FUNCTION GetTxtHeight( cText, nFontSize, cFontName )  // get Height of the text
   LOCAL hFont, nHeight
   DEFAULT cText     := "B"                 ,  ;
           cFontName := _HMG_DefaultFontName,  ;   // from MiniGUI.Init()
           nFontSize := _HMG_DefaultFontSize       // from MiniGUI.Init()

   hFont := InitFont( cFontName, nFontSize )    
   nHeight := GetTextHeight( 0, cText , hFont )        // font height
   DeleteObject( hFont )
   
   RETURN nHeight

///////////////////////////////////////////////////////////////////////////////////////////
// Function to check if a BIG font is installed in the system settings
// Call example:        nSizeFont := IIF(Large2Fonts(),9,11)
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
// Function to check if a BIG font is installed in the system settings
// Call example:        nSizeFont := IIF(LargeFonts(),9,11)
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

