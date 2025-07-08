/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2023 Verchenko Andrey <verchenkoag@gmail.com>
*/

#include "minigui.ch"
#include "metrocolor.ch"

STATIC nCursRow, nCursCol
/////////////////////////////////////////////////////////////////////////
// Ввод пароля - меню для входа в программу
FUNCTION myGetPassword( cTitle, cUserName, cPassword, nType )
   LOCAL cIco, cFont, nFontSize, aBackColor, aColor
   LOCAL nW, nH, nRet, lPassword, cPswd, cLogin
   LOCAL aGradFillEx, aGradOverEx, aGradFillOk, aGradOverOk
   LOCAL cResName, aYX, nPicWidth, nPicHeight, aPicBackColor
   LOCAL nR1, nC1, aColor1, nR2, nC2, aColor2, nR3, nC3, aColor3
   LOCAL nR4, nC4, nR5, nC5, aColor5, nR6, nC6, aColor6, nR7, nC7
   LOCAL nR8, nC8, aBColor8, aColor8, nRB1, nCB1, nRB2, nCB2
   LOCAL nRS1, nCS1, cSoglash, aBFColor, nL, cSog
   LOCAL cLblInfo, cLblLog, cLblPass, cCapt1, cCapt2
   DEFAULT cTitle := "Ввод пароля", cUserName := "", cPassword := "", nType := 0

   cIco       := "iKey64"
   cFont      := 'Tahoma'
   nFontSize  := 12
   aBackColor := COLOR_OF2003_SILVER  // Цвет фона всей формы
   lPassword  := .F.
   cPswd      := cLogin := ""
   nRet       := 0

   IF M->nProgLang == 2  // 1-русский, 2-english, 3-украинский язык
      cLblInfo := "Enter CODE and PASSWORD into program"
      cLblLog  := "Operator:"
      cLblPass := "Password:"
      cSog     := "By using this program, the user agrees to the license agreement"
      cSoglash := 'The license agreement is located ;in the menu "License: XX days left",;'
      cSoglash += ' next button "Read license agreement" !'
      cCapt1   := "Input"
      cCapt2   := "Cancel"
   ELSE
      cLblInfo := "Введите КОД и ПАРОЛЬ в программу"
      cLblLog  := "Оператор-КОД:"
      cLblPass := "Пароль:"
      cSog     := "Используя эту программу, пользователь согласен с лицензионным"+CRLF+"соглашением"
      cSoglash := 'Лицензионное соглашение располагается ;в меню "Лицензия: осталось ХХ дней",;'
      cSoglash += ' далее кнопка "Прочесть лицензионное соглашение" !'
      cCapt1   := "Ввод"
      cCapt2   := "Отмена"
   ENDIF

   IF nType == 0
      aPicBackColor := FUCHSIA
      cResName := "DemoPass"
      aYX := GetImageSizeFromRes(cResName)
      nPicWidth := aYX[1]  ; nPicHeight := aYX[2]
      IF nPicWidth == 0 .OR. nPicHeight == 0
         MsgStop( cResName + " image file open error !", "ERROR !" )
      ENDIF
      nW   := nPicWidth
      nH   := nPicHeight
      nL   := 10
      nR1  := 143 + nL ; nC1 := 70   ; aColor1 := YELLOW
      nR2  := 175 + nL ; nC2 := 70   ; aColor2 := BLUE
      nR3  := 170 + nL ; nC3 := 400  ; aColor3 := RED
      nR4  := 172 + nL ; nC4 := 208
      nR5  := 207 + nL ; nC5 := 70   ; aColor5 := BLUE
      nR6  := 204 + nL ; nC6 := 400  ; aColor6 := RED
      nR7  := 205 + nL ; nC7 := 208
      nR8  := 205 + nL ; nC8 := 378  ; aBColor8 := {135,190,220} ; aColor8 := BLUE
      nRB1 := 245 + nL ; nCB1 := 160
      nRB2 := 245 + nL ; nCB2 := 270 ; aBFColor := BLUE
      nRS1 := 120 ; nCS1 := 60
      aBackColor := aPicBackColor
   ELSE
      cResName := 'GetPass'
      nW   := 370
      nH   := 345
      nL   := 0
      nR1  := 90  + nL ; nC1 := 10   ; aColor1 := BLACK
      nR2  := 130 + nL ; nC2 := 20   ; aColor2 := BLACK
      nR3  := 160 + nL ; nC3 := 420  ; aColor3 := RED
      nR4  := 127 + nL ; nC4 := 158
      nR5  := 167 + nL ; nC5 := 20   ; aColor5 := BLACK
      nR6  := 194 + nL ; nC6 := 420  ; aColor6 := RED
      nR7  := 165 + nL ; nC7 := 158
      nR8  := 165 + nL ; nC8 := 330  ; aBColor8 := aBackColor    ; aColor8 := BLACK
      nRB1 := 235 + nL ; nCB1 := 140
      nRB2 := 235 + nL ; nCB2 := 250 ; aBFColor := BLACK
      nRS1 := 280 + nL ; nCS1 := 10
   ENDIF

   DEFINE WINDOW Form_Login             ;
       AT 0,0 WIDTH nW HEIGHT nH        ;
       TITLE cTitle ICON cIco           ;
       BACKCOLOR aBackColor             ;
       MODAL NOSIZE NOSYSMENU /*NOCAPTION*/ ;
       FONT cFont SIZE nFontSize        ;
       ON INIT OnInitLoginPass(nType)   ;
       ON MOUSEDRAG MoveForm()          ;
       ON MOUSEMOVE SetCoords()

       nW := This.ClientWidth
       nH := This.ClientHeight

      IF nType == 0
         @ 0, 0 IMAGE Image_1 PICTURE cResName
      ELSE
         @ 0, 0 IMAGE Img_1 PICTURE cResName WIDTH nW HEIGHT 70 STRETCH
      ENDIF

     @ nR1, nC1 LABEL Label_Info VALUE cLblInfo ;
        FONTCOLOR aColor1 BOLD AUTOSIZE TRANSPARENT

     @ nR2, nC2 LABEL Label_Login VALUE cLblLog WIDTH 130 HEIGHT 20 ;
        FONTCOLOR aColor2 TRANSPARENT RIGHTALIGN

     @ nR3, nC3 LABEL Label_LoginInf VALUE "Admin" WIDTH 75 HEIGHT 30 ;
        FONTCOLOR aColor3 BOLD SIZE 18 TRANSPARENT INVISIBLE

     @ nR4, nC4 GETBOX Text_Login HEIGHT 26 WIDTH 160 VALUE cLogin   ;
        PICTURE '9999' ;
        BACKCOLOR {{255,255,255},{255,255,200},{200,255,255}} ;
        FONTCOLOR {{0,0,0},{255,255,200},{0,0,255}} ;
        ON LOSTFOCUS { || cLogin := This.Text_Login.Value }

     @ nR5, nC5 LABEL Label_Pass VALUE cLblPass WIDTH 130 HEIGHT 20 ;
       FONTCOLOR aColor5 TRANSPARENT RIGHTALIGN

     @ nR6, nC6 LABEL Label_PassInf VALUE "admin" WIDTH 75 HEIGHT 30 ;
        FONTCOLOR aColor6 BOLD SIZE 18 TRANSPARENT INVISIBLE

     @ nR7, nC7 GETBOX Text_Pass HEIGHT 26 WIDTH 160 VALUE cPswd PASSWORD ;
        PICTURE 'xxxxxxxxxxxx' ;
        BACKCOLOR {{255,255,255},{255,255,200},{200,255,255}} ;
        FONTCOLOR {{0,0,0},{255,255,200},{0,0,255}} ;
        ON LOSTFOCUS { || cPswd := This.Text_Pass.Value }

     @ nR7, nC7 GETBOX Text_Pass2 HEIGHT 26 WIDTH 160 VALUE cPswd ;
        PICTURE 'xxxxxxxxxxxx' ;
        BACKCOLOR {{255,255,255},{255,255,200},{200,255,255}} ;
        FONTCOLOR {{0,0,0},{255,255,200},{0,0,255}} ;
        ON LOSTFOCUS { || cPswd := This.Text_Pass2.Value } ;
        INVISIBLE

     @ nR8, nC8 CHECKBOX Check_Pass CAPTION '' VALUE lPassword NOTABSTOP ;
        WIDTH 22 HEIGHT 23 SIZE 14 FONTCOLOR aColor8 BACKCOLOR aBColor8  ;
        ON CHANGE {|| lPassword := This.Check_Pass.Value, ;
                                   iif( lPassword, ( DoMethod("Form_Login","Text_Pass","Hide"),;
                                   This.Text_Pass2.Value := cPswd,;
                                   DoMethod("Form_Login","Text_Pass2","Show") ),;
                                   ( DoMethod("Form_Login","Text_Pass2","Hide"),;
                                   This.Text_Pass.Value := GetProperty("Form_Login","Text_Pass2","Value"),;
                                   DoMethod("Form_Login","Text_Pass","Show") ) ) ,;
                                   iif( lPassword, This.Text_Pass.Setfocus, This.Text_Pass2.Setfocus ) }

     aColor      := CLR_GRAY  // CLR_GREEN
     aGradOverOk := { { 0.5, CLR_BLACK, aColor    }, { 0.5, aColor   , CLR_BLACK } }
     aGradFillOk := { { 0.5, aColor   , CLR_WHITE }, { 0.5, CLR_WHITE, aColor    } }
     @ nRB1, nCB1 BUTTONEX BUTTON_Ok WIDTH 100 HEIGHT 35        ;
        CAPTION cCapt1 FONTCOLOR aBFColor                       ;
        NOXPSTYLE HANDCURSOR                                    ;
        BACKCOLOR aGradOverOk GRADIENTFILL aGradFillOk          ;
        ON MOUSEHOVER ( This.Fontcolor := WHITE   , This.GradientFill := aGradFillOk ) ;
        ON MOUSELEAVE ( This.Fontcolor := aBFColor, This.GradientOver := aGradOverOk ) ;
        ACTION {|| nRet := 0, cUserName := ALLTRIM(cLogin),;
                   cPassword := ALLTRIM(cPswd), Form_Login.Release }

     aColor      := CLR_GRAY  // CLR_RED
     aGradOverEx := { { 0.5, CLR_BLACK, aColor    }, { 0.5, aColor   , CLR_BLACK } }
     aGradFillEx := { { 0.5, aColor   , CLR_WHITE }, { 0.5, CLR_WHITE, aColor    } }
     @ nRB2, nCB2 BUTTONEX BUTTON_Exit WIDTH 100 HEIGHT 35      ;
        CAPTION cCapt2 FONTCOLOR aBFColor                       ;
        NOXPSTYLE HANDCURSOR                                    ;
        BACKCOLOR aGradOverEx GRADIENTFILL aGradFillEx          ;
        ON MOUSEHOVER ( This.Fontcolor := WHITE   , This.GradientFill := aGradFillEx ) ;
        ON MOUSELEAVE ( This.Fontcolor := aBFColor, This.GradientOver := aGradOverEx ) ;
        ACTION {|| nRet := -1, Form_Login.Release }

     IF nType == 0
       @ nRS1, nCS1 LABEL Label_Sogl VALUE cSog AUTOSIZE BOLD SIZE 7 ;
         TRANSPARENT OnMouseHover RC_CURSOR( "MINIGUI_FINGER" ) ACTION {|| AlertInfo(cSoglash,'Инфо') }
     ELSE
       @ nRS1, nCS1 LABEL Label_Sogl VALUE cSog WIDTH nW-10*2 HEIGHT 30 SIZE 8  ;
         TRANSPARENT OnMouseHover RC_CURSOR( "MINIGUI_FINGER" ) ACTION {|| AlertInfo(cSoglash,'Инфо')  }
     ENDIF

   END WINDOW

   IF nType == 0
      SetLayeredWindowAttributes( GetFormHandle( "Form_Login" ), ( aPicBackColor[1] + ( aPicBackColor[2] * 256 ) + ( aPicBackColor[3] * 65536 ) ), 0, 0x01 )
      //SET WINDOW Form_Login TRANSPARENT TO COLOR aPicBackColor
   ENDIF

   Form_Login.Cursor := "DragCurs"
   Form_Login.Center()
   //Form_Login.Activate()
   ACTIVATE WINDOW Form_Login ON INIT {||  This.Minimize, This.Restore, This.Topmost := .T. }

RETURN nRet

/////////////////////////////////////////////////////////////////////////
FUNCTION OnInitLoginPass(nType)

   SetCoords()
   IF nType == 0
      //Form_Login.Sizable   := .F.  // NOSIZE
      //Form_Login.MaxButton := .F.  // NOMAXIMIZE
      //Form_Login.MinButton := .F.  // NOMINIMIZE
      Form_Login.TitleBar := .F.     // NOCAPTION

      Form_Login.Label_LoginInf.Show
      Form_Login.Label_PassInf.Show
   ENDIF
   Form_Login.Text_Login.Setfocus

RETURN NIL
*-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.

PROCEDURE MoveForm()
   LOCAL nCurWRow  := Form_Login.Row, nCurWCol  := Form_Login.Col
   LOCAL nCursRowC := GetCursorRow(), nCursColC := GetCursorCol()
   LOCAL nDiffVert := nCursRow - nCurWRow, nDiffHorz := nCursCol - nCurWCol

   nCurWRow := MAX( nCursRowC - nDiffVert, 0 )

   Form_Login.Row := nCurWRow

   nCurWCol := MAX( nCursColC - nDiffHorz, 0 )

   Form_Login.Col := nCurWCol

   SetCoords()

RETURN // MoveForm()
*-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.

PROCEDURE SetCoords()

   nCursRow := GetCursorRow()
   nCursCol := GetCursorCol()

RETURN // SetCoords()
*-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.

FUNCTION GetImageSizeFromRes( cResName )
   LOCAL cMsg, hBitmap,  aSize := {0,0}

   hBitmap := C_GetResPicture( cResName )

   aSize := GetBitmapSize( hBitmap )

   DeleteObject( hBitmap )

   If aSize[1] == 0 .OR. aSize[2] == 0
      cMsg := "Calling from: " + ProcName(0) + "(" + hb_ntos(ProcLine(0)) + ") -> " + ProcFile(0) + CRLF + CRLF
      cMsg += "There is no such resource in the exe file!" + CRLF + CRLF
      cMsg += "Invalid name: " + cResName + CRLF + CRLF
      MsgStop( cMsg , "Error" )
   endif

RETURN aSize // GetImageSizeFromRes()

