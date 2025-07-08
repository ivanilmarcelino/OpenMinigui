/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2007-2018 Grigory Filatov <gfilatov@inbox.ru>
 *
 * Revised edition 2019 by Pierpaolo Martinello <assistenza[at]pcmodula.it>
 * Based upon a contribution of Petr Chornyj <myorg63/at/gmail.com>.
*/

#include "minigui.ch"

*-----------------------------------------------------------------------------*
Procedure Main
*-----------------------------------------------------------------------------*
   Local nQ := 75, cMsg := "Based on Gdiplus library by Petr Chornyj"+CRLF+CRLF
   Local aFileDim := BmpSize( GetStartupFolder() + '\demo.png' )

   cMsg += "revised by Grigory Filatov"+CRLF+CRLF+"and extended by Pierpaolo Martinello."

   SET NAVIGATION EXTENDED

   DEFINE WINDOW Form_1 ;
          AT 0,0 ;
          WIDTH  400 ;
          HEIGHT 400 ;
          TITLE 'PNG To JPG sample by Grigory Filatov' ;
          MAIN ;
          NOMAXIMIZE NOSIZE ;
          ON RELEASE FErase( "demo.jpg" )

          DEFINE BUTTON Button_1
                 ROW      26
                 COL      295
                 CAPTION 'Press Me'
                 ACTION  SaveToJPG( nQ, GetStartupFolder() + '\demo.png', aFileDim )
                 WIDTH   90
                 HEIGHT  26
                 DEFAULT .T.
                 TOOLTIP 'Save to JPG'
          END BUTTON

          DEFINE BUTTON Button_2
                 ROW     56
                 COL     295
                 CAPTION 'Cancel'
                 ACTION  ThisWindow.Release
                 WIDTH   90
                 HEIGHT  26
                 TOOLTIP 'Exit'
          END BUTTON

          DEFINE BUTTON Button_3
                 ROW     129
                 COL     295
                 CAPTION 'Credits'
                 ACTION  MsgInfo(cMsg," Authors: ")
                 WIDTH   90
                 HEIGHT  26
                 NOTABSTOP .T.
          END BUTTON

          DEFINE LABEL Label_1
                 ROW   5
                 COL   10
                 WIDTH 130
                 VALUE 'Source:'
                 CENTERALIGN .T.
          END LABEL

          @ 25,5 FRAME Frame_1 WIDTH 130 HEIGHT 130

          DEFINE IMAGE Image_1
                 ROW     30
                 COL     10
                 HEIGHT  120
                 WIDTH   120
                 PICTURE 'demo.png'
                 STRETCH .T.
          END IMAGE

          DEFINE LABEL Label_2
                 ROW    5
                 COL    150
                 WIDTH  130
                 VALUE 'Destination:'
                 CENTERALIGN .T.
          END LABEL

          @ 25,150 FRAME Frame_2 WIDTH 130 HEIGHT 130

          DEFINE IMAGE Image_2
                 ROW     30
                 COL     155
                 HEIGHT  120
                 WIDTH   120
                 PICTURE NIL
                 STRETCH .F.
          END IMAGE

          @ 170,5 FRAME Frame_3 CAPTION "JPEG:" WIDTH 380 HEIGHT 95

          DEFINE LABEL Label_3
                 ROW   225
                 COL   20
                 WIDTH 100
                 VALUE 'Save quality:'
          END LABEL

          DEFINE LABEL Label_4
                 ROW   190
                 COL   120
                 WIDTH 50
                 VALUE 'lowest'
          END LABEL

          DEFINE LABEL Label_5
                 ROW   190
                 COL   320
                 WIDTH 40
                 VALUE 'best'
                 RIGHTALIGN .T.
          END LABEL

          DEFINE LABEL Label_6
                 ROW   190
                 COL   240
                 WIDTH 40
                 VALUE Ltrim(Str(nQ))
          END LABEL

          DEFINE SLIDER Slider_1
                 ROW    220
                 COL    110
                 VALUE  nQ
                 WIDTH  260
                 HEIGHT 30
                 RANGEMIN 0
                 RANGEMAX 100
                 NOTICKS .T.
                 BOTH .T.
                 ON SCROLL ( nQ := Form_1.Slider_1.Value, Form_1.Label_6.Value := hb_ntos(nQ) )
                 ON CHANGE ( nQ := Form_1.Slider_1.Value, Form_1.Label_6.Value := hb_ntos(nQ), Form_1.Button_1.OnClick )
          END SLIDER

          @ 270,5 FRAME Frame_4 CAPTION "Dimension:" WIDTH 380 HEIGHT 95

           DEFINE LABEL Label_Size
                  ROW   295
                  COL   60
                  WIDTH 250
                  VALUE 'Current input size: '+ hb_ntos(aFileDim[1])+" x "+hb_ntos(aFileDim[2])+" Pixel"
                  CENTERALIGN  .T.
           END LABEL

           DEFINE LABEL Label_NewSize
                  ROW   325
                  COL   10
                  WIDTH 350
                  VALUE 'Set the output dimension :        Width :                       Height :'
                  VCENTERALIGN .T.
           END LABEL

           DEFINE TEXTBOX Text_V
                   ROW  325
                   COL  225
                   WIDTH 40
                   NUMERIC .T.
                   VALUE aFileDim[1]
                   MAXLENGTH 4
                   ON CHANGE aFiledim[1] := this.value
           END TEXTBOX

           DEFINE TEXTBOX Text_H
                   ROW  325
                   COL  335
                   WIDTH 40
                   NUMERIC .T.
                   VALUE aFileDim[2]
                   MAXLENGTH 4
                   ON CHANGE aFiledim[2] := this.value
           END TEXTBOX

   END WINDOW

   Form_1.Height := 365 + GetTitleHeight() + 2 * GetBorderHeight()

   CENTER WINDOW Form_1

   ACTIVATE WINDOW Form_1

Return
/*
*/
*-----------------------------------------------------------------------------*
Function SaveToJPG( nQ, cFile, aFileDim )
*-----------------------------------------------------------------------------*
    Local lResult
    Local cJpg := GetStartupFolder() + "\demo.jpg"

    lResult := HMG_SaveImage( cFile, cJpg, "JPEG", nQ, aFileDim )	// Save to JPEG

    If ! lResult							// An error occured
        MsgStop( "Conversion did not succeed!", "Error" )
    EndIf

    SetProperty( "Form_1", "Image_2", "Picture", cJpg )

Return lResult
