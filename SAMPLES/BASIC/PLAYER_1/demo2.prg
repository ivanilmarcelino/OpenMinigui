/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 */

#include "hmg.ch"

FUNCTION Main()

   LOCAL nWIDTH, nHEIGHT, aSize
   LOCAL oActivex
   LOCAL cFile := GetStartupFolder() + "\sample.avi"

   aSize := GetAviFileSize( cFile )

   IF aSize[1] > 0
	nWIDTH  := Max( 300, aSize[1] )
	nHEIGHT := Max( 250, aSize[2] + 64 )
   ENDIF

   DEFINE WINDOW Win_1 ;
      CLIENTAREA nWIDTH, nHEIGHT ;
      TITLE 'Media Player Test' ;
      MAIN ;
      NOMAXIMIZE NOSIZE

      oActiveX := TActiveX():New( "Win_1", "WMPlayer.OCX.7", 0, 0, nWIDTH, nHEIGHT ):Load()

      oActiveX:url := cFile
      oActiveX:Settings:Volume := 100
      oactiveX:StretchToFit := .F.

   END WINDOW

   CENTER WINDOW Win_1

   ACTIVATE WINDOW Win_1

RETURN NIL
