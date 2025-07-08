/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2020 Grigory Filatov <gfilatov@inbox.ru>
 */

#include "hmg.ch"
#include "i_winuser.ch"

FUNCTION Main

   LOCAL Clr_BtnFace := nRGB2Arr( GetSysColor( COLOR_BTNFACE ) )

   DEFINE WINDOW  main ;
	CLIENTAREA 810, 390 ;
	TITLE 'Progress Wheel Control Demo' ;
	ICON 'MAINICON' ;
	MAIN ;
	FONT 'MS Sans Serif' SIZE 9

     DEFINE FRAME Frame_1
            ROW	8
            COL	16
            WIDTH 380
            HEIGHT 175
            CAPTION 'Inner size'
     END FRAME

     DEFINE LABEL Label_1
            ROW    155
            COL    75
            WIDTH  10
            HEIGHT 15
            VALUE  '0'
            VCENTERALIGN .T.
     END LABEL  

     DEFINE LABEL Label_2
            ROW    155
            COL    195
            WIDTH  15
            HEIGHT 15
            VALUE  '50'
            VCENTERALIGN .T.
     END LABEL

     DEFINE LABEL Label_3
            ROW    155
            COL    320
            WIDTH  15
            HEIGHT 15
            VALUE  '90'
            VCENTERALIGN .T.
     END LABEL

     DEFINE PROGRESSWHEEL PW_1
            ROW    30
            COL    22
            WIDTH  114
            HEIGHT 114
            VALUE  25
            COLORINNER Clr_BtnFace
            INNERSIZE 0
            SHOWTEXT .F.
     END PROGRESSWHEEL

     DEFINE PROGRESSWHEEL PW_2
            ROW    30
            COL    145
            WIDTH  114
            HEIGHT 114
            VALUE  25
            COLORINNER Clr_BtnFace
            INNERSIZE 50
            SHOWTEXT .F.
     END PROGRESSWHEEL

     DEFINE PROGRESSWHEEL PW_3
            ROW    30
            COL    268
            WIDTH  114
            HEIGHT 114
            VALUE  25
            COLORINNER Clr_BtnFace
            INNERSIZE 90
            SHOWTEXT .F.
     END PROGRESSWHEEL

     DEFINE FRAME Frame_2
            ROW	8
            COL	416
            WIDTH 380
            HEIGHT 175
            CAPTION 'Inner color'
     END FRAME

     DEFINE LABEL Label_4
            ROW    155
            COL    422
            WIDTH  114
            HEIGHT 15
            VALUE  'White'
            VCENTERALIGN .T.
            CENTERALIGN .T.
     END LABEL  

     DEFINE LABEL Label_5
            ROW    155
            COL    545
            WIDTH  114
            HEIGHT 15
            VALUE  'Gray'
            VCENTERALIGN .T.
            CENTERALIGN .T.
     END LABEL

     DEFINE LABEL Label_6
            ROW    155
            COL    668
            WIDTH  114
            HEIGHT 15
            VALUE  'Black'
            VCENTERALIGN .T.
            CENTERALIGN .T.
     END LABEL

     DEFINE PROGRESSWHEEL PW_4
            ROW    30
            COL    422
            WIDTH  114
            HEIGHT 114
            VALUE  25
            INNERSIZE 50
            SHOWTEXT .F.
     END PROGRESSWHEEL

     DEFINE PROGRESSWHEEL PW_5
            ROW    30
            COL    545
            WIDTH  114
            HEIGHT 114
            VALUE  25
            COLORINNER GRAY
            INNERSIZE 50
            SHOWTEXT .F.
     END PROGRESSWHEEL

     DEFINE PROGRESSWHEEL PW_6
            ROW    30
            COL    668
            WIDTH  114
            HEIGHT 114
            VALUE  25
            COLORINNER BLACK
            INNERSIZE 50
            SHOWTEXT .F.
     END PROGRESSWHEEL

     DEFINE FRAME Frame_3
            ROW	203
            COL	16
            WIDTH 380
            HEIGHT 175
            CAPTION 'Text'
     END FRAME

     DEFINE LABEL Label_7
            ROW    350
            COL    22
            WIDTH  114
            HEIGHT 15
            VALUE  'Standard'
            VCENTERALIGN .T.
            CENTERALIGN .T.
     END LABEL  

     DEFINE LABEL Label_8
            ROW    350
            COL    145
            WIDTH  114
            HEIGHT 15
            VALUE  'Custom'
            VCENTERALIGN .T.
            CENTERALIGN .T.
     END LABEL

     DEFINE LABEL Label_9
            ROW    350
            COL    268
            WIDTH  114
            HEIGHT 15
            VALUE  'Custom 2'
            VCENTERALIGN .T.
            CENTERALIGN .T.
     END LABEL

     DEFINE PROGRESSWHEEL PW_7
            ROW    226
            COL    22
            WIDTH  114
            HEIGHT 114
            VALUE  25
            COLORINNER Clr_BtnFace
            INNERSIZE 50
     END PROGRESSWHEEL

     DEFINE PROGRESSWHEEL PW_8
            ROW    226
            COL    145
            WIDTH  114
            HEIGHT 114
            VALUE  25
            COLORINNER Clr_BtnFace
            INNERSIZE 50
     END PROGRESSWHEEL

     Main.PW_8.SetShowText( {| Position, Max | hb_ntos( Position ) + '/' + hb_ntos( Max ) } )

     DEFINE PROGRESSWHEEL PW_9
            ROW    226
            COL    268
            WIDTH  114
            HEIGHT 114
            VALUE  25
            COLORINNER Clr_BtnFace
            INNERSIZE 60
     END PROGRESSWHEEL

      Main.PW_9.SetShowText( {| Position, Max | hb_ntos( Position ) + ' from ' + hb_ntos( Max ) } )

     DEFINE FRAME Frame_4
            ROW	203
            COL	416
            WIDTH 380
            HEIGHT 175
            CAPTION 'Colors (ColorRemain/ColorDoneMax)'
     END FRAME

     DEFINE LABEL Label_10
            ROW    350
            COL    422
            WIDTH  114
            HEIGHT 15
            VALUE  'BtnFace/Green'
            VCENTERALIGN .T.
            CENTERALIGN .T.
     END LABEL  

     DEFINE LABEL Label_11
            ROW    350
            COL    545
            WIDTH  114
            HEIGHT 15
            VALUE  'Black/Yellow'
            VCENTERALIGN .T.
            CENTERALIGN .T.
     END LABEL

     DEFINE LABEL Label_12
            ROW    350
            COL    668
            WIDTH  114
            HEIGHT 15
            VALUE  'White/SkyBlue'
            VCENTERALIGN .T.
            CENTERALIGN .T.
     END LABEL

     DEFINE PROGRESSWHEEL PW_10
            ROW    226
            COL    422
            WIDTH  114
            HEIGHT 114
            VALUE  25
            COLORDONEMAX LGREEN
            COLORREMAIN Clr_BtnFace
            COLORINNER Clr_BtnFace
            SHOWTEXT .F.
     END PROGRESSWHEEL

     DEFINE PROGRESSWHEEL PW_11
            ROW    226
            COL    545
            WIDTH  114
            HEIGHT 114
            VALUE  25
            COLORDONEMAX YELLOW
            COLORREMAIN BLACK
            COLORINNER Clr_BtnFace
            SHOWTEXT .F.
     END PROGRESSWHEEL

     DEFINE PROGRESSWHEEL PW_12
            ROW    226
            COL    668
            WIDTH  114
            HEIGHT 114
            VALUE  25
            COLORDONEMAX { 166, 202, 240 }
            COLORREMAIN WHITE
            COLORINNER Clr_BtnFace
            SHOWTEXT .F.
     END PROGRESSWHEEL

      DEFINE TIMER Timer_1 INTERVAL 40 ACTION OnTimer()

   END WINDOW	

   Main.Center
   Main.Activate	
	
RETURN NIL



PROCEDURE OnTimer

   LOCAL i, Max := 100, ColorRemain, ColorDoneMax
   LOCAL aColors := { RED, { 0, 160, 0 }, { 0, 128, 255 }, { 255, 128, 64 } }
   STATIC n := 1, Position := 20

   Position += 5
   IF Position <= Max
      FOR i := 1 TO 12
         PW_SetPosition( "PW_" + hb_ntos( i ), 'Main', Position )
      NEXT
   ELSE
      n++
      IF n > Len( aColors )
         n := 1
      ENDIF
      FOR i := 1 TO 12
         IF i < 10
            ColorDoneMax := Main.&("PW_" + hb_ntos( i )).ColorDoneMax
            PW_SetColorRemain( "PW_" + hb_ntos( i ), 'Main', ColorDoneMax, .F. )
            PW_SetColorDoneMax( "PW_" + hb_ntos( i ), 'Main', aColors[ n ], .F. )
         ELSE
            ColorRemain := Main.&("PW_" + hb_ntos( i )).ColorRemain
            ColorDoneMax := Main.&("PW_" + hb_ntos( i )).ColorDoneMax
            PW_SetColorRemain( "PW_" + hb_ntos( i ), 'Main', ColorDoneMax, .F. )
            PW_SetColorDoneMax( "PW_" + hb_ntos( i ), 'Main', ColorRemain, .F. )
         ENDIF
      NEXT
      Position := 0
   ENDIF

RETURN
