/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2020 Grigory Filatov <gfilatov@inbox.ru>
 */

#include "hmg.ch"

FUNCTION MAIN

   DEFINE WINDOW MAIN ;
         WIDTH 640 ;
         HEIGHT 480 ;
         TITLE 'Progress Wheel Control Test' ;
         ICON 'MAINICON' ;
         MAIN ;
         FONT 'MS Sans Serif' SIZE 9

      DEFINE PROGRESSWHEEL PW_1
         ROW 30
         COL 30
         WIDTH 114
         HEIGHT 114
         VALUE 25
         SHOWTEXT .F.
      END PROGRESSWHEEL

      DEFINE LABEL Label_0
         ROW 154
         COL 30
         WIDTH 114
         HEIGHT 25
         VALUE "Preview"
         CENTERALIGN .T.
      END LABEL

      DEFINE SLIDER Slider_1
         ROW 20
         COL 200
         WIDTH 200
         HEIGHT 25
         RANGEMIN 0
         RANGEMAX 100
         VALUE 25
         NOTICKS .T.
         BOTH .T.
         ON CHANGE PositionChange( this.Value )
         ON SCROLL PositionChange( this.Value )
      END SLIDER

      DEFINE LABEL Label_1
         ROW 20
         COL 420
         WIDTH 100
         HEIGHT 25
         VALUE "Position"
         VCENTERALIGN .T.
      END LABEL

      DEFINE SLIDER Slider_2
         ROW 60
         COL 200
         WIDTH 200
         HEIGHT 25
         RANGEMIN 0
         RANGEMAX 350
         VALUE 0
         NOTICKS .T.
         BOTH .T.
         ON CHANGE StartAngleChange( this.Value )
         ON SCROLL StartAngleChange( this.Value )
      END SLIDER

      DEFINE LABEL Label_2
         ROW 60
         COL 420
         WIDTH 100
         HEIGHT 25
         VALUE "Start Angle"
         VCENTERALIGN .T.
      END LABEL

      DEFINE SLIDER Slider_3
         ROW 100
         COL 200
         WIDTH 200
         HEIGHT 25
         RANGEMIN 0
         RANGEMAX 99
         VALUE 75
         NOTICKS .T.
         BOTH .T.
         ON CHANGE InnerSizeChange( this.Value )
         ON SCROLL InnerSizeChange( this.Value )
      END SLIDER

      DEFINE LABEL Label_3
         ROW 100
         COL 420
         WIDTH 100
         HEIGHT 25
         VALUE "Inner Size"
         VCENTERALIGN .T.
      END LABEL

      DEFINE LABEL Label_4
         ROW 140
         COL 200
         WIDTH 100
         HEIGHT 25
         VALUE "Color Inner"
         VCENTERALIGN .T.
      END LABEL

      DEFINE LABEL Label_4_1
         ROW 140
         COL 300
         WIDTH 100
         HEIGHT 25
         VALUE ""
         BACKCOLOR Main.PW_1.ColorInner
         BORDER .T.
      END LABEL

      DEFINE BUTTON Button_1
         ROW 140
         COL 405
         WIDTH 25
         HEIGHT 25
         CAPTION '...'
         ACTION ButtonColorInnerClick()
      END BUTTON

      DEFINE LABEL Label_5
         ROW 175
         COL 200
         WIDTH 100
         HEIGHT 25
         VALUE "Color Remain"
         VCENTERALIGN .T.
      END LABEL

      DEFINE LABEL Label_5_1
         ROW 175
         COL 300
         WIDTH 100
         HEIGHT 25
         VALUE ""
         BACKCOLOR Main.PW_1.ColorRemain
         BORDER .T.
      END LABEL

      DEFINE BUTTON Button_2
         ROW 175
         COL 405
         WIDTH 25
         HEIGHT 25
         CAPTION '...'
         ACTION ButtonColorRemainClick()
      END BUTTON

      DEFINE LABEL Label_6
         ROW 210
         COL 200
         WIDTH 100
         HEIGHT 25
         VALUE "Color DoneMin"
         VCENTERALIGN .T.
      END LABEL

      DEFINE LABEL Label_6_1
         ROW 210
         COL 300
         WIDTH 100
         HEIGHT 25
         VALUE ""
         BACKCOLOR Main.PW_1.ColorDoneMin
         BORDER .T.
      END LABEL

      DEFINE BUTTON Button_3
         ROW 210
         COL 405
         WIDTH 25
         HEIGHT 25
         CAPTION '...'
         ACTION ButtonColorDoneMinClick()
      END BUTTON

      DEFINE LABEL Label_7
         ROW 245
         COL 200
         WIDTH 100
         HEIGHT 25
         VALUE "Color DoneMax"
         VCENTERALIGN .T.
      END LABEL

      DEFINE LABEL Label_7_1
         ROW 245
         COL 300
         WIDTH 100
         HEIGHT 25
         VALUE ""
         BACKCOLOR Main.PW_1.ColorDoneMax
         BORDER .T.
      END LABEL

      DEFINE BUTTON Button_4
         ROW 245
         COL 405
         WIDTH 25
         HEIGHT 25
         CAPTION '...'
         ACTION ButtonColorDoneMaxClick()
      END BUTTON

      DEFINE LABEL Label_8
         ROW 280
         COL 200
         WIDTH 100
         HEIGHT 25
         VALUE "Gradient Mode"
         VCENTERALIGN .T.
      END LABEL

      DEFINE COMBOBOX Combo_1
         ROW 280
         COL 300
         WIDTH 130
         HEIGHT 100
         ITEMS { "None", "Position", "Angle" }
         VALUE 1
         ONCHANGE GradientModeChange( this.Value )
      END COMBOBOX

   END WINDOW

   MAIN.CENTER
   MAIN.ACTIVATE

RETURN NIL


PROCEDURE ButtonColorInnerClick()

   LOCAL Color

   Color := GetProperty ( thiswindow.Name, 'Label_4_1', 'backcolor' )
   Color := GetColor( Color )

   IF ValType( Color[ 1 ] ) # 'N'
      RETURN
   ENDIF

   SetProperty ( thiswindow.Name, 'Label_4_1', 'backcolor', Color )
   Main.PW_1.ColorInner := Color

RETURN


PROCEDURE ButtonColorRemainClick()

   LOCAL Color

   Color := GetProperty ( thiswindow.Name, 'Label_5_1', 'backcolor' )
   Color := GetColor( Color )

   IF ValType( Color[ 1 ] ) # 'N'
      RETURN
   ENDIF

   SetProperty ( thiswindow.Name, 'Label_5_1', 'backcolor', Color )
   Main.PW_1.ColorRemain := Color

RETURN


PROCEDURE ButtonColorDoneMinClick()

   LOCAL Color

   Color := GetProperty ( thiswindow.Name, 'Label_6_1', 'backcolor' )
   Color := GetColor( Color )

   IF ValType( Color[ 1 ] ) # 'N'
      RETURN
   ENDIF

   SetProperty ( thiswindow.Name, 'Label_6_1', 'backcolor', Color )
   Main.PW_1.ColorDoneMin := Color

RETURN


PROCEDURE ButtonColorDoneMaxClick()

   LOCAL Color

   Color := GetProperty ( thiswindow.Name, 'Label_7_1', 'backcolor' )
   Color := GetColor( Color )

   IF ValType( Color[ 1 ] ) # 'N'
      RETURN
   ENDIF

   SetProperty ( thiswindow.Name, 'Label_7_1', 'backcolor', Color )
   Main.PW_1.ColorDoneMax := Color

RETURN


PROCEDURE PositionChange( n )

   Main.PW_1.Position := n

RETURN


PROCEDURE StartAngleChange( n )

   Main.PW_1.StartAngle := n

RETURN


PROCEDURE InnerSizeChange( n )

   Main.PW_1.InnerSize := n

RETURN


PROCEDURE GradientModeChange( n )

   Main.PW_1.GradientMode := n

RETURN
