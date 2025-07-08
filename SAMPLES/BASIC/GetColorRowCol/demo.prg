/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2019, Verchenko Andrey <verchenkoag@gmail.com>
 *
 * Чтение цвета фона по координатам / Read background color by coordinates
*/

ANNOUNCE RDDSYS

#include "minigui.ch"

FUNCTION Main()

   LOCAL nFWidth, nFHeight
   LOCAL nR, nC, nW

   DEFINE WINDOW Form_1 AT 0, 0 ;
         WIDTH 350 HEIGHT 290 ;
         WINDOWTYPE MAIN ;
         TITLE 'Read color by coordinates - Demo' ;
         ON INIT OnInitForm1()

      nFWidth := This.ClientWidth
      nFHeight := This.ClientHeight

      @ 20, 40 IMAGE Image_1 PICTURE 'Passw64.png' WIDTH 64 HEIGHT 64

      nW := nFWidth - 40 * 2 - 64
      nC := 64 + 40
      @ 20, nC LABEL Label_1 WIDTH nW HEIGHT 64 VALUE "User test" ;
         SIZE 16 FONTCOLOR WHITE VCENTERALIGN CENTERALIGN

      @ 94, 40 IMAGE Image_2 OF Form_1 PICTURE 'Teleph48.png' ;
         WIDTH 64 HEIGHT 64 STRETCH TRANSPARENT

      @ 94, nC LABEL Label_2 WIDTH nW HEIGHT 64 VALUE "Telephone test" ;
         SIZE 16 FONTCOLOR WHITE BACKCOLOR LGREEN VCENTERALIGN CENTERALIGN

      nR := nFHeight - 48 - 10
      nW := nFWidth - 20 * 2
      @ nR, 24 BUTTON Butt_Exit CAPTION "Exit" WIDTH nW HEIGHT 48 ;
         ACTION ThisWindow.RELEASE

      This.MaxButton := .F.
      This.Sizable := .F.
      This.SysMenu := .F.

      ON KEY ESCAPE ACTION ThisWindow.RELEASE

   END WINDOW

   CENTER WINDOW Form_1

   ACTIVATE WINDOW Form_1

RETURN NIL

//////////////////////////////////////////////////////////////
FUNCTION OnInitForm1()

   Form_1.Label_1.BACKCOLOR := RowColColorRGB( 21, 41 )
   Form_1.Image_2.BACKGROUNDCOLOR := RowColColorRGB( 95, 110 )

RETURN NIL

//////////////////////////////////////////////////////////////
FUNCTION RowColColorRGB( y, x )

   LOCAL hdc, aColor := { 0, 0, 0 }
   LOCAL hWin := ThisWindow.Handle

   hdc := GetDC( hWin )
   IF ! GetPixelColor( hdc, x, y, @aColor )
      MsgAlert( "Error at getting of a pixel color.", "Warning" )
   ENDIF
   ReleaseDC( hWin, hdc )

RETURN aColor
