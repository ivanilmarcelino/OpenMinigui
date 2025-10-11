/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2024-2025 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Просмотр массивов/dbf в окне через _TBrowse()
 * Viewing arrays/dbf in a window using _TBrowse()
*/
#define  _HMG_OUTLOG
#include "minigui.ch"
#include "tsbrowse.ch"
#define PROGVER   "Version 0.7 (15.09.2025)"
#define TSB_W1COL 4      // number of characters in the 1st column
/////////////////////////////////////////////////////////////////////////////////////////
FUNCTION AlertTsb(cType,nIcoSize,aXDim,cTitle,aButton,oWin,o1Brw,bInitForm)
   LOCAL nFontSize, nY, nX, nW, nH, nG, cForm, cIcon, nWTsb, nHTsb, aRet, cMsg
   LOCAL nBtnH, nBtnW, cN, aFColor, aBColor, aBtnBClr2, aBtnFClr2, cTmp, aPost
   LOCAL nWDim, nHDim, hWndIsx, cFrmIsx, cFont, aFont, lBold, nW2, nH2, cVal, nI
   LOCAL aBtnTxt, aBtnBClr, aBtnFClr, aBtnFont, aWHDim, oTsb, oBrw, nWTxt, nPost
   LOCAL cHelp, aHelpBClr, aHelpFClr, nLine, nHhelp, cFontHlp, nFSizeHlp, lBoldHlp
   LOCAL lInitForm, cInitForm, nHDsk, nWFlg, aVal, nCol, cAls, lIsDbf, aFntHelp
   LOCAL nHlpLine, aBClrWin, aFontTsb
   DEFAULT cType := "", nIcoSize := 128, cTitle := ""
   DEFAULT aXDim := { {"Error !!!","aXDim = {} empty array  !!!", ProcNL() } }
   DEFAULT aButton := {}, oWin := oHmgData(), o1Brw := oHmgData()
   DEFAULT bInitForm := {|| Nil }

   aRet      := {}   // вернуть массив
   cForm     := "MG_Form_Tsb_" + HB_NtoS( _GetId() )
   aFont     := GetFontParam(GetFontHandle("Bold"))   ; Default aFont := {}
   cFontHlp  := IIF( LEN(aFont) > 0, aFont[1], cFont     )
   nFSizeHlp := IIF( LEN(aFont) > 0, aFont[2], nFontSize ) + 2
   lBoldHlp  := IIF( LEN(aFont) > 0, aFont[3], .F.       )
   aFont     := GetFontParam(GetFontHandle("Normal")) ; Default aFont := {}
   cFont     := "DejaVu Sans Mono"
   nFontSize := 14
   cFont     := IIF( LEN(aFont) > 0, aFont[1], cFont     )
   nFontSize := IIF( LEN(aFont) > 0, aFont[2], nFontSize )
   lBold     := IIF( LEN(aFont) > 0, aFont[3], .F.       )
   aBtnFont  := {"Comic Sans MS",nFontSize + 2,.T.}
   aFontTsb  := { "Normal", "Bold", "Italic", "SpecHdr" , "SuperHd", "TsbEdit" }

   // запомнить предыдущее окно, если оно есть
   hWndIsx := 0
   cFrmIsx := _HMG_ThisFormName
   IF !Empty(cFrmIsx) .and. _IsWindowDefined(cFrmIsx)
      hWndIsx := GetFormHandle( cFrmIsx )
   ENDIF

   IF Empty( _HMG_MainHandle )  // если нет MAIN окна
      MsgDebug("ERROR ! NO MAIN window !"+ProcNL()+";"+ProcNL(1))
      SET WINDOW MAIN OFF
   ENDIF

   IF IsArray(aButton) .AND. LEN(aButton) > 6
      cMsg := "ERROR ! aButton array greater than 6 is not allowed !;;"
      cMsg += ProcNL()+";"+ProcNL(1)
      AlertStop(cMsg, , , 64)
      RETURN NIL
   ENDIF

   lIsDbf := .F.
   IF IsString(aXDim)  // это dbf / this is dbf
      lIsDbf := .T.
      cAls := aXDim
      IF SELECT(cAls) == 0
         cMsg := "ERROR ! There is no such database alias: "+cAls+" !;;"
         cMsg += ProcNL()+";"+ProcNL(1)
         AlertStop(cMsg, , , 64)
         RETURN NIL
      ENDIF
   ELSE
      IF !IsArray(aXDim)  
         cTmp  := aXDim
         aXDim := { {"ERROR !!!", "aXDim not an array !", cTmp, ProcNL(), ProcNL(1) } }
      ELSE
         // one-dimensional array
         IF Len( aXDim ) > 0 .and. !IsArray( aXDim[1] )
            aXDim := { {"ERROR !!!", "aXDim one-dimensional array !!!", ProcNL(), ProcNL(1) } }
         ELSEIF Len( aXDim ) == 0
            aXDim := { { "Error !!!" , "aXDim = {} empty array  !!!", ProcNL(), ProcNL(1) } }
         ENDIF
      ENDIF
      nCol  := LEN(aXDim[1])
      nLine := 0
      FOR nI := 1 TO LEN(aXDim)
         aVal  := aXDim[nI]
         nLine := MAX(nCol,LEN(aVal))
      NEXT
      IF nCol # nLine
         aXDim := { { "Error !!!" , "aXDim[1] # aXDim[nI] Difference in columns !!!", ProcNL(), ProcNL(1) } }
      ENDIF
   ENDIF

   aBColor  := App.Cargo:aDlgBColor ;  Default aBColor  := SILVER
   aFColor  := App.Cargo:aDlgFColor ;  Default aFColor  := BLACK
   aBClrWin := oWin:aBColor         ;  Default aBClrWin := {}        // цвет всей формы из oWin:aBColor
   IF LEN(aBClrWin) > 0
      aBColor := aBClrWin
   ENDIF

   IF Hb_LangSelect() == "ru.RU1251"  ; aBtnTxt  := { '&Да' }
   ELSE                               ; aBtnTxt  := { '&Ok' }
   ENDIF
   IF IsString(App.Cargo:cLang) .AND.  App.Cargo:cLang == "RU"
      aBtnTxt  := { '&Да' }
   ELSE
      aBtnTxt  := { '&Ok' }
   ENDIF

   aPost    := { 98 }               // событие - выход / event - output
   DEFAULT aBtnBClr := { BLUE   }
   DEFAULT aBtnFClr := { YELLOW }
   aBtnFClr2 := oWin:aBtnFClr2  ; DEFAULT aBtnFClr2 := WHITE    // инвертный цвет фонта кнопки (фокус на кнопке)
   aBtnBClr2 := oWin:aBtnBClr2  ; DEFAULT aBtnBClr2 := BLACK    // инвертный цвет фона кнопки  (фокус на кнопке)

   cIcon := "iMgNone128"
   cMsg  := "ERROR ! Invalid parameter cType"
   cMsg  += "; cType = " + cValToChar(cType)
   cMsg  += ";;Correct values:"
   cMsg  += "; cType = INFO/STOP/EXCLAM"
   cMsg  += "; YESNO/YN/RETRYCANCEL/RC/YESNOCANCEL/YNC" 
   cMsg  += "; cType = xxxx.ico - show the given icon"
   cMsg  += ";;" + ProcNL()+";"+ProcNL(1)
   IF !IsString(cType) 
      AlertStop(cMsg, , , 64, {RED})
      cType := cValToChar(cType)
   ELSE
      IF LEN(cType) == 0
         AlertStop(cMsg, , , 64, {RED})
      ENDIF
   ENDIF
   cType       := UPPER(ALLTRIM(cType))
   IF cType == "INFO"
      cIcon    := "iMgInfo128"
      aBColor  := { 133, 205, 242 }      // фон окна светло-синий
      aFColor  := BLUE
      aBtnBClr := { {42,174,239} }
      aBtnFClr := { YELLOW }             // цвет фонта кнопки
   ELSEIF cType == "STOP"
      cIcon    := "iMgStop128"
      aBColor  := {248,209,211}          // фон окна светло-красный
      aFColor  := RED
      aBtnBClr := { {222, 14, 32} }
      aBtnFClr := { YELLOW }             // цвет фонта кнопки
   ELSEIF cType == "EXCLAM"
      cIcon    := "iMgExclam128"
      aBColor  := { 238, 249, 142 }      // фон окна светло-жёлтый
      aFColor  := { 174, 134,  33 }
      aBtnBClr := { {254,184,1}   }      // цвет фона кнопки
      aBtnFClr := { BLACK }              // цвет фонта кнопки
   ELSEIF cType == "YESNO" .OR. cType == "YN" .OR. ;
          cType == "RC"  .OR. cType == "RETRYCANCEL"
      cIcon    := "iMgQuest128"
      aBColor  := { 251, 225, 170 }                 // фон окна светло-оранжевый
      aBtnBClr := { LGREEN , {222, 14, 32} }        // цвет фона кнопки
      aBtnFClr := { YELLOW , YELLOW        }        // цвет фонта кнопки
      // порядок следования кнопок: 1-Yes, 2-No
      aBtnTxt  := { '&' + _HMG_aABMLangLabel [20], '&' + _HMG_aABMLangLabel [21] }
      IF cType == "RC"  .OR. cType == "RETRYCANCEL"
         aBtnTxt := { _HMG_aLangButton[ 13 ], _HMG_aLangButton[ 7 ] }
      ENDIF
      IF IsArray(aButton) .AND. LEN(aButton) > 0
         aBtnTxt := { "Button-1" , "Button-2" }
         FOR nI := 1 TO LEN(aButton)
            cVal := aButton[nI]
            IF IsString(cVal) ; aBtnTxt[nI] := cVal
            ELSE              ; aBtnTxt[nI] := cValToChar(cVal)
            ENDIF
         NEXT
      ENDIF
      aPost    := { 80, 98 }
   ELSEIF cType == "YESNOCANCEL" .OR. cType == "YNCANCEL" .OR. cType == "YNC"
      cIcon    := "iMgQuest128"
      aBtnBClr := { LGREEN , {66, 92,251}, {222, 14, 32} }    // цвет фона кнопки
      aBtnFClr := { YELLOW , YELLOW      , YELLOW }           // цвет фонта кнопки
      aBtnTxt  := { '&' + _HMG_aABMLangLabel[20], '&' + _HMG_aABMLangLabel[21], '&' + _HMG_aABMLangButton[13] }
      IF IsArray(aButton) .AND. LEN(aButton) > 0
         aBtnTxt := { "Button-1" , "Button-2", "Button-3" }
         FOR nI := 1 TO LEN(aButton)
            cVal := aButton[nI]
            IF IsString(cVal) ; aBtnTxt[nI] := cVal
            ELSE              ; aBtnTxt[nI] := cValToChar(cVal)
            ENDIF
         NEXT
      ENDIF
      aPost := { 80, 80, 98 }
   ELSE
      cIcon := cType          // иконка = cType
      DEFAULT cIcon := "iMgNone128"
      IF IsArray(aButton) .AND. LEN(aButton) > 0
         aPost    := {}
         aBtnFClr := {}
         aBtnBClr := {}
         aBtnTxt  := ARRAY(LEN(aButton))
         FOR nI := 1 TO LEN(aButton)
            cVal := aButton[nI]
            IF IsString(cVal) ; aBtnTxt[nI] := cVal
            ELSE              ; aBtnTxt[nI] := cValToChar(cVal)
            ENDIF
            nPost := IIF(nI == LEN(aButton),98,80)
            AADD(aPost,nPost)
            AADD(aBtnFClr, YELLOW  )
            AADD(aBtnBClr, LGREEN  )
         NEXT
         aBtnBClr[LEN(aButton)] := {222, 14, 32}  // цвет фона кнопки
      ENDIF
      // цвет кнопок из oWin
      IF IsArray(oWin:aBtnFClr)
         IF LEN(oWin:aBtnFClr) # LEN(aBtnTxt)
            cMsg := "ERROR ! LEN(oWin:aBtnFClr) # LEN(aBtnTxt) !;;"
            cMsg += ProcNL()+";"+ProcNL(1)
            AlertStop(cMsg, , , 64)
         ENDIF
         aBtnFClr := oWin:aBtnFClr
      ENDIF
      IF IsArray(oWin:aBtnBClr)
         IF LEN(oWin:aBtnBClr) # LEN(aBtnTxt)
            cMsg := "ERROR ! LEN(oWin:aBtnBClr) # LEN(aBtnTxt) !;;"
            cMsg += ProcNL()+";"+ProcNL(1)
            AlertStop(cMsg, , , 64)
         ENDIF
         aBtnBClr := oWin:aBtnBClr
      ENDIF
   ENDIF
   //
   IF Hb_LangSelect() == "ru.RU1251"
      cTitle := IIF( LEN(cTitle) == 0, "ВНИМАНИЕ !", cTitle )
      cTitle += SPACE(5) + ProcName(1) + "(..)"
   ELSE
      cTitle := IIF( LEN(cTitle) == 0, "ATTENTION!", cTitle )
      cTitle += SPACE(5) + ProcName(1) + "(..)"
   ENDIF
   // подсказка над кнопками / hint above buttons
   nHhelp    := 0
   cHelp     := oWin:cHelp      ; Default cHelp     := ""
   aHelpBClr := oWin:aHelpBClr  ; Default aHelpBClr := aBColor
   aHelpFClr := oWin:aHelpFClr  ; Default aHelpFClr := aFColor
   aFntHelp  := oWin:aFntHelp   ; Default aFntHelp  := {}
   nHlpLine  := 0
   IF LEN(cHelp) > 0
      nHlpLine := NumAt( CRLF , cHelp ) + 1
      IF LEN(aFntHelp) > 0
         nHhelp := nHlpLine * aFntHelp[2] * 1.6
      ELSE
         nHhelp := nHlpLine * nFSizeHlp * 2
      ENDIF
   ENDIF

   // ширина кнопок / button width
   nW2 := 0
   FOR nI := 1 TO LEN(aBtnTxt)
      cVal  := aBtnTxt[nI]
      IF LEN(cVal) == 3
         cVal += "HHHHHHHHH"
      ELSE
         cVal += "HHHHHH"
      ENDIF
      nWTxt := GetTxtWidth( cVal, nFontSize, cFont, lBold ) 
      nW2   := MAX(nWTxt,nW2)
   NEXT

   nBtnH := 55      // высота кнопки / button height
   nBtnW := nW2     // ширина кнопки / button width
   nG    := 20      // отступы / indents
   // расчёт ширины на самую широкую строку в таблице
   IF lIsDbf
      aWHDim := MaxSize_TsbDbf(cAls, o1Brw, aFontTsb )
   ELSE
      aWHDim := MaxSize_Tsb(aXDim, o1Brw, aFontTsb )
   ENDIF
   nWDim  := aWHDim[1]                    // ширина таблицы / table width
   nHDim  := aWHDim[2]                    // высота таблицы - примерная
   nW2    := nG*3 + nIcoSize + nWDim + GetBorderWidth()  // ширина окна
   nW     := IIF( nW2 > System.DesktopWidth*0.96, System.DesktopWidth*0.96, nW2 )
   nWFlg  := IIF( nW2 > System.DesktopWidth*0.96, 1, 0 )
   nH2    := nHDim + nBtnH + nG*3
   nH2    += nHhelp //+ IIF(nHhelp==0,0,4)  // высота подсказки над кнопками
   nH2    := IIF( nH2 < nIcoSize + nBtnH + nG*3, nIcoSize + nBtnH + nG*3, nH2 )
   nH2    += GetTitleHeight() + GetBorderHeight()
   nHDsk  := App.Cargo:aDisplayMode[2]  //{ Sys.ClientWidth , Sys.ClientHeight }
   nH     := IIF( nH2 > nHDsk*0.95, nHDsk*0.95, nH2 )

   DEFINE WINDOW &cForm AT nY, nX WIDTH nW HEIGHT nH   ;
      TITLE cTitle                                     ;
      MODAL NOSIZE                                     ;
      FONT cFont SIZE nFontSize                        ;
      BACKCOLOR aBColor                                ;
      ON INIT     {|| _wPost( 0) }                     ;
      ON RELEASE  {|| _wSend(90) }

      This.Cargo := oHmgData()
      This.Cargo:hWndIsx := hWndIsx
      This.Cargo:aBtnTxt := aBtnTxt

      nW := This.ClientWidth
      nH := This.ClientHeight
      nY := nG
      nX := nG

      @ 0, 0 LABEL Buff PARENT &cForm WIDTH 10 HEIGHT 10 VALUE '' TRANSPARENT

      DRAW ICON IN WINDOW &cForm AT nY, nX PICTURE cIcon WIDTH nIcoSize HEIGHT nIcoSize COLOR aBColor

      nX    += nIcoSize + nG
      IF nWFlg == 1   // таблица за пределами экрана / table off screen
         nWTsb := nW - nIcoSize - nG*3
      ELSE
         nWTsb := nWDim
      ENDIF
      nHTsb := nH - nG*2 - nBtnH - nHhelp  // высота подсказки над кнопками
                                           // height of tooltip above buttons
      //@ nY, nX LABEL Label_Tsb PARENT &cForm WIDTH nWTsb HEIGHT nHTsb ;
      //  VALUE 'Table' BACKCOLOR aBColor FONTCOLOR aFColor BORDER
      ////////////// определение таблицы / table definition ////////////////////////////////
      oTsb := TsbPatam(cForm,aXDim,"oBrw",aBColor,o1Brw,lIsDbf,aFontTsb)
      // функция в библиотеке \MiniGUI\SOURCE\TsBrowse\h_controlmisc2.prg
      oBrw := _TBrowse( oTsb, aXDim, "oBrw", nY, nX, nWTsb, nHTsb )
      //
      oBrw:Cargo:nModify := 0     // счётчик изменений в ТСБ / counter of changes in TSB
      This.Cargo:oBrw    := oBrw

      /////////////////// подсказка над кнопками / hint above buttons //////////////////////
      IF LEN(cHelp) > 0
         nI := nY + nHTsb + 2
         @ nI, nX LABEL Help PARENT &cForm WIDTH nWTsb HEIGHT nHhelp VALUE cHelp ;
           FONTCOLOR aHelpFClr BACKCOLOR aHelpBClr FONT cFontHlp SIZE nFSizeHlp BOLD
         nHTsb += nHhelp + 2*2
         IF LEN(aFntHelp) > 0
            This.Help.Fontname   := aFntHelp[1]
            This.Help.Fontsize   := aFntHelp[2]
            This.Help.Fontbold   := aFntHelp[3]
            This.Help.FontItalic := aFntHelp[4]
         ENDIF
      ENDIF

      //////////////// кнопки внизу формы / buttons at the bottom of the form ///////////////
      nY := nH - nBtnH - nG + 5
      nX := nW - nBtnW - nG

      FOR nI := LEN(aBtnTxt) TO 1 STEP - 1
         cN := "Btn_" + StrZero(nI,2)
         cN := IIF( nI == LEN(aBtnTxt) , "Btn_Exit", cN )
         @ nY, nX BUTTONEX &cN PARENT &cForm                     ;
           WIDTH nBtnW HEIGHT nBtnH CAPTION aBtnTxt[nI] ICON Nil ;
           NOHOTLIGHT NOXPSTYLE HANDCURSOR                       ;
           FONT aBtnFont[1] SIZE aBtnFont[2] BOLD                ;
           FONTCOLOR aBtnFClr[1] BACKCOLOR aBtnBClr[nI]          ;
           ON MOUSEHOVER ( This.Backcolor := aBtnBClr2  , This.Fontcolor := aBtnFClr2   ) ; // инвертный цвет фона и фонта кнопки
           ON MOUSELEAVE ( This.Backcolor := This.Cargo:aBClr, This.Fontcolor := This.Cargo:aFClr ) ;
           ACTION {|| This.Enabled := .F., _wPost(This.Cargo:nPost, ThisWindow.Name, This.Name) }

           This.&(cN).Cargo := oHmgData()
           WITH OBJECT This.&(cN).Cargo
              :nBtn     := nI           // номер нажатой кнопки / number of the button pressed
              :nPost    := aPost[nI]
              :cCapt    := aBtnTxt[nI]
              :aBClr    := aBtnBClr[nI]
              :aFClr    := aBtnFClr[nI]
              :cObj     := cN
           END WITH
         nX -= nBtnW + nG
      NEXT

      // доп.обработка для окна, oBrw, oTsb / additional processing for window, oBrw, oTsb //
      cInitForm := Valtype(bInitForm)
      IF HB_ISCHAR( bInitForm )
         IF ! ( "|...|" $ bInitForm .and. "(...)" $ bInitForm )
            bInitForm := "{|...| "+bInitForm+"(...)"
         ENDIF
         cInitForm := bInitForm
         bInitForm := &( bInitForm )
      ENDIF

      lInitForm := .F.
      IF HB_ISBLOCK( bInitForm )
         BEGIN SEQUENCE WITH { |e|break(e) }
            EVal( bInitForm, This.Object, This.Cargo:oBrw, oTsb )
            lInitForm := .T.
         END SEQUENCE
         IF ! lInitForm
            cMsg := "ERROR ! "
            cMsg += "Must be a block of code !"
            cMsg += " cInitForm=" + cInitForm
            MsgDebug(cMsg, bInitForm)
         ENDIF
      ENDIF

      (This.Object):Event( 0, {|ow| // ON INIT
                                    Local ob := ow:Cargo:oBrw
                                    This.Topmost := .T.
                                    ob:Setfocus()
                                    DO EVENTS
                                    Return Nil
                                    })

      (This.Object):Event(80, {|ow,ky,cn,ob,nAt| ob  := ow:Cargo:oBrw ,;
                                nAt := ow:Cargo:oBrw:nAt ,;  // номер строки в ТСБ / line number in TSB
                                aRet := {This.&(cn).Cargo:nBtn, This.&(cn).Cargo:cCapt, ob:aArray, nAt, ky} ,;
                                ow:Release() } )

      (This.Object):Event(90, {|ow| // ON RELEASE - return to previous window
                                    ? "---- ON RELEASE ----", ow:Name, ProcNL()
                                    ? "---- aRet=",aRet ; ?v aRet
                                    If ow:Cargo:hWndIsx > 0
                                       SwitchToThisWindow(hWndIsx)
                                    Endif
                                    DO EVENTS
                                    Return Nil
                                    })

      (This.Object):Event(98, {|ow,ky,cn,ob| ob := ow:Cargo:oBrw,;  // Exit
                                aRet := {} /*{This.&(cn).Cargo:nBtn, cn, ob:aArray, ky}*/ ,;
                                ky := cn := ob ,;
                                ow:Release() } )

      ON KEY F1     OF &cForm ACTION NIL
      ON KEY ESCAPE OF &cForm ACTION {|| _wPost(98,cForm) }

   END WINDOW

     CENTER WINDOW &cForm
   ACTIVATE WINDOW &cForm

RETURN aRet

/////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION TsbPatam(cForm,aXDim,cBrw,aBColor,o1Brw,lIsDbf,aFontTsb)
   LOCAL oTsb, nClr1, nClr2, a, aHead, nI, cMsg, cAls, nHCell, aFoot, nChar

   oTsb := o1Brw       ;   Default oTsb := oHmgData()
   oTsb:cBrw           := cBrw
   oTsb:cForm          := cForm      
   oTsb:cFormName      := cForm      
   oTsb:lNoPicture     := .T.
   //                         cell     Head    foot     SpecHider  SuperHider   Edit
   //oTsb:aFont        := { "Normal", "Bold", "Italic", "SpecHdr" , "SuperHd", "TsbEdit" }
   oTsb:aFont          := aFontTsb
   //
   IF IsArray(oTsb:aNumber) .AND. LEN(oTsb:aNumber) == 0
      oTsb:aNumber     := NIL
   ELSE
      nChar            := TSB_W1COL                                    // кол-во знаков первой колонки
      oTsb:aNumber     := { 1, GetFontWidth(oTsb:aFont[4], nChar)  }   // колонка нумерации и её ширина
   ENDIF
   // высота ячеек
   IF IsNumeric(o1Brw:nHeightCell)
      oTsb:nHeightCell    := o1Brw:nHeightCell              
   ELSE
      nHCell              := INT( GetFontHeight(oTsb:aFont[1])*1.35 )
      // высота ячеек = высоте картинки чекбокса
      nHCell              := IIF( nHCell < 32, 32, nHCell )   
      oTsb:nHeightCell    := nHCell                     
   ENDIF
   nHCell := oTsb:nHeightCell
   // высота шапки
   IF IsNumeric(o1Brw:nHeightHead)
      oTsb:nHeightHead := o1Brw:nHeightHead              
   ELSE
      oTsb:nHeightHead := nHCell                         
   ENDIF
   //
   IF IsLogic(oTsb:lFooting) .AND. !oTsb:lFooting
      oTsb:lFooting    := .F.
      oTsb:aFoot       := .F.                           
   ELSE
      oTsb:lFooting    := .T.                            // поставить в таблице подвал
   ENDIF
   // высота подвала
   IF IsNumeric(o1Brw:nHeightFoot)
      oTsb:nHeightFoot := o1Brw:nHeightFoot              
   ELSE
      oTsb:nHeightFoot := nHCell                
   ENDIF

   // высота нумератора
   IF !IsLogic(o1Brw:lSpecHd)
      oTsb:lSpecHd     := .F.       // НЕ ставить в таблице нумератор
   ENDIF
   IF oTsb:lSpecHd
      oTsb:nHeightSpecHd := INT( GetFontHeight(oTsb:aFont[4])*1.35 )      
   ENDIF
   // высота суперхидера
   IF IsLogic(oTsb:lSuperHd) .AND. oTsb:lSuperHd
      IF IsNumeric(o1Brw:nHeightSuperHd)
         oTsb:nHeightSuperHd := o1Brw:nHeightSuperHd                     
      ELSE
         oTsb:nHeightSuperHd := 30                     
      ENDIF
   ENDIF
   //
   nClr1 := HMG_RGB2n(aBColor)     // цвет фона шапка+подвал
   nClr2 := RGB( 48, 29,26)        // серо-черный фон
   oTsb:aSuperHdColor  := {CLR_YELLOW, { nClr1, nClr2 } }   // цвет: текст и фон суперхидера
   oTsb:aBrush         := {240,240,240}                     // цвет фона под таблицей

   // цвета в таблицу
   a := {}
   // 1 , текста ячеек
   AAdd(a, { CLR_TEXT, CLR_BLACK } )                // 1 , текста ячеек
   // 2 , фона в ячейках таблицы
   //AAdd(a, { CLR_PANE, {|nr,nc,ob,nd| nr := CLR_BLACK, nc := CLR_WHITE, nd := CLR_HGRAY, ;
   //                      iif( (ob:cAlias)->(DELETED()), nr, iif( ob:nAt % 2 == 0, nc, nd ) ) } } )
   AAdd(a, { CLR_HEADF, CLR_WHITE                })  // 3 , текста шапки таблицы
   AAdd(a, { CLR_HEADB, { nClr2, nClr1 }         })  // 4 , фона шапки таблицы
   AAdd(a, { CLR_FOCUSB, {|a,b,c| a := b, If( c:nCell == b, -CLR_HRED, -CLR_BLUE ) } } ) // 6 , фона курсора
   AAdd(a, { CLR_EDITF, CLR_YELLOW               })  // 7 , текста редактируемого поля
   AAdd(a, { CLR_EDITB, CLR_HRED                 })  // 8 , фона редактируемого поля
   AAdd(a, { CLR_FOOTF, CLR_WHITE                })  // 9 , текста подвала таблицы
   AAdd(a, { CLR_FOOTB, { nClr1, nClr2 }         })  // 10, фона подвала таблицы
   AAdd(a, { CLR_SPCF , CLR_YELLOW               })  // 18, specheader text - нумератор
   AAdd(a, { CLR_SPCB , { nClr1, nClr2 }         })  // 19, specheader back - нумератор
   oTsb:aColorAdd := a
   oTsb:lZebra    := .T.
   oTsb:aZebra    := { {230,230,230}, SILVER }

   IF lIsDbf
      aHead := {}
      aFoot := {}
      cAls  := aXDim
      IF !IsArray(o1Brw:aHead)
         oTsb:aHead := aHead
      ENDIF
      IF !IsArray(o1Brw:aFoot)
         oTsb:aFoot := aFoot
      ENDIF
   ELSE
      a := aXDim[1]    // this is for an array
      IF IsArray(oTsb:aHead)
         // no need to check, the number of header columns can be less
         // than the number of array columns
         IF ! ( LEN(oTsb:aHead) == LEN(a) )
            cMsg := "ERROR! Arrays are not equal !;"
            cMsg += "oTsb:aHead # aXDim[1];;"
            cMsg += ProcNL() + ";" + ProcNL(1)
            //AlertStop(cMsg, , , 64)
         ENDIF
      ELSE
         aHead := {}
         FOR nI := 1 TO LEN(a)
            AADD( aHead, HB_ValToExp(nI) )
         NEXT
         oTsb:aHead := aHead
      ENDIF
   ENDIF
   //oTsb:aHideCol := {}        // hide columns - reserve

   // такой порядок работы блоков кода
   oTsb:bInit := {|ob,op| // настройки тсб
                   If IsArray(op:aHideCol)
                      ob:HideColumns( op:aHideCol ,.t.)           // скрыть колонки
                   Endif
                   ob:nFreeze     := ob:nColumn("ORDKEYNO")       // Заморозить столбцы
                   ob:lLockFreeze := .T.                          // Избегать прорисовки курсора на замороженных столбцах
                   ob:lNoKeyChar  := .F.                          // ввод в ячейки от букв, цифр
                   ob:nMemoHV     :=  1                           // показ одной строки мемо-поля
                   ob:nCellMarginLR := 1     // отступ от линии ячейки при прижатии влево, вправо на кол-во пробелов
                   IF IsBlock(op:bInit_2)
                      ? "### Additional settings connected :bInit_2",ProcNL()
                      EVal(op:bInit_2, ob, op)
                   ENDIF
                   Return Nil
                   }

   oTsb:bBody := {|ob,op| // другие настройки тсб
                     Local nBClr := GetSysColor( COLOR_BTNFACE )   // цвет системный
                     Local hFont := GetFontParam(op:aFont[4])      // SpecHider
                     Local lFoot, cv, oc, i := 0
                     //
                     hFont := ob:aColumns[1]:hFontSpcHd            // 4-special header font - или так
                     ob:lNoHScroll  := .T.   // нет показа горизонтального скролинга
                     ob:oHScroll    := NIL
                     // замена первой колонки и спецхидера
                     FOR EACH oc IN ob:aColumns
                        oc:cSpcHeading := NIL
                        IF oc:cName == "ARRAYNO" .OR. oc:cName == "ORDKEYNO"
                           oc:cSpcHeading := "#"
                           oc:nClrBack    := nBClr            // изменение цвета фона виртуальной колонки
                           oc:nClrFore    := CLR_RED          // изменение цвета текста виртуальной колонки
                           oc:hFont       := hFont            // изменение фонта виртуальной колонки
                           oc:bDecode     := {|cv| Alltrim(cv) }
                           oc:nAlign      := DT_CENTER
                           oc:nFAlign     := DT_CENTER
                        ELSE
                           oc:cSpcHeading := hb_ntos( ++i )
                           oc:nFAlign     := DT_CENTER
                        ENDIF
                     NEXT
                     lFoot := .F.
                     // проверка подвала / basement check
                     FOR EACH oc IN ob:aColumns
                        IF oc:cName == "ARRAYNO" .OR. oc:cName == "ORDKEYNO"
                           LOOP
                        ELSE
                           IF ob:lIsDbf     // ТОЛЬКО для Dbf
                              cv := oc:cFooting
                              If IsString(cv) .AND. Len(cv) > 0
                                 lFoot := .T.  // подвал есть - заполнен
                              Endif
                           ENDIF
                        ENDIF
                     NEXT
                     //
                     FOR EACH oc IN ob:aColumns
                        IF oc:cName == "ARRAYNO" .OR. oc:cName == "ORDKEYNO"
                           LOOP
                        ELSE
                           IF oc:lCheckBox
                              oc:lEdit    := .T. 
                              oc:cPicture := Nil 
                              oc:nAlign := DT_CENTER 
                              oc:nEditMove := 0 
                              oc:aCheck := { LoadImage("bMgCheckT24"), LoadImage("bMgCheckF24") } 
                           ENDIF
                           IF ob:lIsDbf .AND. !lFoot         // ТОЛЬКО для Dbf и нет подвала
                              cv := oc:cFieldTyp + '('       // ONLY for Dbf and no footer
                              cv += HB_NtoS(oc:nFieldLen) + ','
                              cv += HB_NtoS(oc:nFieldDec) + ')'
                              oc:cFooting := cv
                              oc:nFAlign  := DT_CENTER
                           ENDIF
                        ENDIF
                     NEXT
                     //
                     IF IsBlock(op:bBody_2)
                        ? "### Additional settings connected :bBody_2",ProcNL()
                        EVal(op:bBody_2, ob, op)
                     ENDIF
                     DO EVENTS
                     Return Nil
                     }

   oTsb:bAfter := {|ob,op| // блок кода после END TBROWSE / block of code after END TBROWSE
                   //... тут настройки основные для работы / here are the main settings for work
                   IF IsBlock(op:bAfter_2)
                      ? "### Additional settings connected :bAfter_2",ProcNL()
                      EVal(op:bAfter_2, ob, op)
                   ENDIF
                   //... тут завершение / here is the end
                   Return Nil
                   }

   // назначим клавиши в таблице
   oTsb:aUserKeys := { ;
        {VK_F2    , {|ob| _wPost(32, ob:cParentWnd, ob) } }, ;
        {VK_F3    , {|ob| _wPost(33, ob:cParentWnd, ob) } }  ;
                     }
   // назначить события на окно
   oTsb:aEvents   := { ;
        {32, {|ow,ky,ob| myTsbListColumn( ob ), ob:Setfocus(), ky:=ow:Name } }, ;   // инфо по списку колонок
        {33, {|ow,ky,ob| myTsbListFont( ob )  , ob:Setfocus(), ky:=ow:Name } }, ;   // инфо по фонтам таблицы
        {50, {|ow,ky,ob| _wPost("_TsbRClick",ow) , ky:=ow:=ob              } }  ;   // правый клик мышки
                     }

RETURN oTsb

/////////////////////////////////////////////////////////////////////////////////////
FUNCTION myTsbListColumn( oBrw )
   LOCAL oCol, nCol, cCol, cSize, cFld, cMsg, cTitle

#ifdef KEY_ENG // for this project demo1-en.hbp
   cTitle := 'Info on the list of columns'
#else
   cTitle := 'Инфо по списку колонок'
#endif

   cMsg   := "Table alias: " + oBrw:cAlias + ";;"
   FOR nCol := 1 TO Len(oBrw:aColumns)
      oCol  := oBrw:aColumns[ nCol ]
      cCol  := oCol:cName
      cFld  := oCol:cField
      cSize := HB_NtoS( INT(oBrw:GetColSizes()[nCol]) )
      cMsg  += HB_NtoS(nCol) + ") " + cCol + " = " + cSize
      cMsg  += ' ( "'+ cFld + '", "'  + oCol:cFieldTyp + '" '
      cMsg  += HB_NtoS(oCol:nFieldLen)
      cMsg  += ',' + HB_NtoS(oCol:nFieldDec) + ' ) ;'
   NEXT
   cMsg += ";"
   FOR nCol := 1 TO Len(oBrw:aColumns)
      oCol  := oBrw:aColumns[ nCol ]
      cCol  := oCol:cName
      cFld  := cValToChar( oCol:lEdit )    // oCol:cPicture
      cMsg  += HB_NtoS(nCol) + ") " + PADR(cCol,12) + " - "
      cMsg  += ' oCol:lEdit = .'+ cFld + '.  ;'
   NEXT
   cMsg += REPL("; ",20)

   AlertInfo(cMsg , cTitle, , 64, {RED})

RETURN Nil

///////////////////////////////////////////////////////////////////////////////
FUNCTION myTsbListFont( oBrw )
   LOCAL cMsg, cTitle, aFont, nI, aFPar, hFont, cFont

#ifdef KEY_ENG // for this project demo1-en.hbp
   cTitle := 'Info on table fonts'
#else
   cTitle := 'Инфо по фонтам таблицы'
#endif

   cMsg   := "Table alias: " + oBrw:cAlias + ";;"
   cMsg   += "     1-Cell: "+hb_valtoexp(GetFontParam(oBrw:hFont)) + ";"
   cMsg   += "     2-Head: "+hb_valtoexp(GetFontParam(oBrw:hFontHead )) + ";"
   cMsg   += "     3-Foot: "+hb_valtoexp(GetFontParam(oBrw:hFontFoot )) + ";"
   cMsg   += "    4-SpcHd: "+hb_valtoexp(GetFontParam(oBrw:hFontSpcHd)) + ";"
   cMsg   += "     5-Edit: "+hb_valtoexp(GetFontParam(oBrw:hFontEdit )) + ";"
   cMsg   += "  6-SuperHd: "+hb_valtoexp(GetFontParam(oBrw:hFontSupHdGet(1))) + ";;"

   cMsg += Replicate( "-._.", 22 ) + ";;"
   cMsg += "1) Height = " + HB_NtoS(oBrw:nHeight) + ";"
   cMsg += "2) HeightHead = " + HB_NtoS(oBrw:nHeightHead) + ";"
   cMsg += "3) HeightSuper = " + HB_NtoS(oBrw:nHeightSuper) + ";"
   cMsg += "4) HeightFoot = " + HB_NtoS(oBrw:nHeightFoot) + ";"
   cMsg += "5) HeightSpecHd = " + HB_NtoS(oBrw:nHeightSpecHd) + ";"
   cMsg += "6) HeightCell = " + HB_NtoS(oBrw:nHeightCell) + ";;"
   cMsg += "Number of rows in the table = " + HB_NtoS(oBrw:nRowCount()) + ";;"
   cMsg += "GetHScrollBarHeight() = " + HB_NtoS(GetHScrollBarHeight()) + ";"
   cMsg += "GetVScrollBarWidth() = " + HB_NtoS(GetVScrollBarWidth()) + ";"
   cMsg += Replicate( "-._.", 22 ) + ";"

   nI := cFont := hFont := aFPar
   aFont := oBrw:Cargo:aFont
   /*FOR nI := 1 TO Len(aFont)
      cFont := aFont[nI]
      hFont := GetFontHandle(cFont)
      aFPar := GetFontParam( hFont )
      cMsg  += "  " + HB_NtoS(nI) + ": "
      cMsg  += cFont + " - ["
      cMsg  += hb_ntos(hFont) + "] - "
      cMsg  += hb_valtoexp(aFPar) + ";"
   NEXT */
   cMsg   += REPL("; ",20)

   AlertInfo(cMsg , cTitle, ,64, {RED})

RETURN Nil

/////////////////////////////////////////////////////////////////////////////////////
// расчёт ширины на самую широкую строку Array
STATIC FUNCTION MaxSize_Tsb(aXDim, o1Brw, aFontTsb )
   LOCAL aDim, aLen, nWTxt, nHTxt, nI, nJ, cVal, lSize, aHide, nS, cMsk, aWH
   LOCAL aFont, nFSize, cFont, lBold
   //                          1        2       3          4           5          6
   //                         cell     Head    foot     SpecHider   SuperHider   Edit
   //oTsb:aFont        := { "Normal", "Bold", "Italic", "SpecHdr" , "SuperHd", "TsbEdit" }
   aFont := GetFontParam(aFontTsb[1])     
   cFont := aFont[1] ; nFSize := aFont[2] ; lBold := aFont[3]

   aHide := o1Brw:aHideCol               // для скрытых колонок
   IF UPPER("DejaVu") $ UPPER(cFont)
      cMsk := "HHHHH"  // для DejaVu Sans Mono
   ELSE
      cMsk := "HHH"    // для Arial и др.фонты
   ENDIF

   nWTxt := 0
   aLen  := ARRAY(LEN(aXDim[1]))
   AFILL(aLen, 0)
   FOR nI := 1 TO LEN(aXDim)
      aDim  := aXDim[nI]
      FOR nJ := 1 TO LEN(aDim)
         cVal  := aDim[nJ]
         lSize := .T.
         IF IsArray(aHide) .AND. LEN(aHide) > 0
            FOR nS := 1 TO LEN(aHide)
               IF nJ == aHide[nS]     // пропуск подсчёта
                  lSize := .F.
                  EXIT
               ENDIF
            NEXT
         ENDIF
         IF lSize
            IF VALTYPE(cVal) # "C"
               cVal := cValToChar(cVal)
            ENDIF
            cVal += cMsk
            nWTxt    := GetTxtWidth( cVal, nFSize, cFont, lBold )  
            aLen[nJ] := MAX(nWTxt,aLen[nJ])
         ENDIF
      NEXT
   NEXT

   nWTxt := 0
   FOR nI := 1 TO LEN(aLen)
      nWTxt += aLen[nI]
   NEXT

   aWH := myGetTsb_WH(o1Brw,aFontTsb)   // размеры ТСБ / TSB dimensions
   // aWH := { nWSlctr, nW1Col, nHCell, nHHead, nHFoot, nHSpecHd, nHSuperHd }
   //            1       2       3        4       5          6       7

   // ширина таблицы / table width
   nWTxt += aWH[1] + aWH[2] + GetHScrollBarHeight() + 5

   // высота таблицы / table height 
   nHTxt := LEN(aXDim) * aWH[3] + aWH[4] + aWH[5] + aWH[6] + aWH[7]

Return { nWTxt, nHTxt }

////////////////////////////////////////////////////////////////////////////////////////
// расчёт ширины на самую широкую строку Dbf
STATIC FUNCTION MaxSize_TsbDbf(cAls, o1Brw, aFontTsb )
   LOCAL cType, nWTxt, nI, cVal, cMsk, nWTsb, nLen, nHTsb, aStru, nStru
   LOCAL nJ, lYes, aFld, cName, aFont, nFSize, cFont, lBold, aWH
   //                          1        2       3          4           5          6
   //                         cell     Head    foot     SpecHider   SuperHider   Edit
   //oTsb:aFont        := { "Normal", "Bold", "Italic", "SpecHdr" , "SuperHd", "TsbEdit" }
   aFont := GetFontParam(GetFontHandle(aFontTsb[1]))     
   cFont := aFont[1] ; nFSize := aFont[2] ; lBold := aFont[3]

   aFld  := o1Brw:aField
   IF !IsArray(aFld) ; aFld := {}
   ENDIF

   IF UPPER("DejaVu") $ UPPER(cFont)
      cMsk := "HHHHH"  // для DejaVu Sans Mono
   ELSE
      cMsk := "HHH"    // для Arial и др.фонты
   ENDIF
   // ширина таблицы  
   nWTsb := 0
   DbSelectArea(cAls)
   aStru := dbStruct()
   nStru := LEN(aStru)

   IF LEN(aFld) == 0           // нет списка полей базы
      FOR nI := 1 TO nStru
         cType := FieldType(nI)
         nLen  := FieldLen(nI)
         IF cType $ "T@="
            nLen := 24
         ENDIF
         cVal  := REPL("a", nLen) + cMsk
         nWTxt := GetTxtWidth( cVal, nFSize, cFont, lBold )  
         nWTsb += nWTxt
      NEXT
   ELSE 
      FOR nI := 1 TO nStru
         cName := FieldName(nI)
         lYes  := .F.
         FOR nJ := 1 TO LEN(aFld)
            IF UPPER(cName) == UPPER(aFld[nJ])
               lYes := .T. 
               EXIT
            ENDIF
         NEXT
         IF lYes
            cType := FieldType(nI)
            nLen  := FieldLen(nI)
            IF cType $ "T@="
               nLen := 24
            ENDIF
            cVal  := REPL("a", nLen) + cMsk
            nWTxt := GetTxtWidth( cVal, nFSize, cFont, lBold )  
            nWTsb += nWTxt
         ENDIF
      NEXT
   ENDIF

   aWH := myGetTsb_WH(o1Brw,aFontTsb)   // размеры ТСБ / TSB dimensions
   // aWH := { nWSlctr, nW1Col, nHCell, nHHead, nHFoot, nHSpecHd, nHSuperHd }
   //            1       2       3        4       5          6       7

   nWTsb += aWH[1] + aWH[2] + GetHScrollBarHeight() + 5

   // высота таблицы  
   nHTsb := LastRec() * aWH[3] + aWH[4] + aWH[5] + aWH[6] + aWH[7]

Return { nWTsb, nHTsb }

////////////////////////////////////////////////////////////////////////////
// настройки редактирования, редактирование колонок
FUNCTION myAlertTsbEdit( oBrw )
   LOCAL oCol, cCol, nI

   FOR EACH oCol IN oBrw:aColumns
      nI   := hb_EnumIndex(oCol)
      cCol := oCol:cName
      IF cCol == "SELECTOR" .OR. cCol == "ORDKEYNO"  ; LOOP
      ENDIF
      IF oBrw:lIsDbf                // это для Dbf
         IF oCol:cFieldTyp $ "+^="  // эти поля не редактируются
            oCol:lEdit := .F.
            oCol:nClrFootFore := CLR_WHITE
            oCol:nClrFootBack := CLR_RED
         ELSE
            // будет использоватся функции по умолчанию
            //oCol:bPrevEdit := {|val, brw| myTsbEditPrevDbf( val, brw ) }  // -> самостоятельно
            //oCol:bPostEdit := {|val, brw| myTsbEditPostDbf( val, brw ) }  // -> самостоятельно
         ENDIF
      ELSE
         oCol:bPrevEdit := {|val, brw| myTsbEditPrev( val, brw ) }  // -> см.ниже
         oCol:bPostEdit := {|val, brw| myTsbEditPost( val, brw ) }  // -> см.ниже
         //oCol:lEdit := .T.
      ENDIF
      //? nI, oCol:cName, oCol:lEdit, oCol:cFieldTyp, oCol:nFieldLen, oCol:nFieldDec, oCol:cHeading
   NEXT

RETURN NIL

////////////////////////////////////////////////////////////////////////////
// ob:aArray[ob:nAt][4], ob:aArray[ob:nAt][5], ob:aArray[ob:nAt][6]
//  1                     2                               3              4         5      6
//{ "" , "Disable/enable display of cancelled applica:", .F.         , "lZa1"    , "L"  , ""   } )
//{ "" , "Example of displaying a menu selection with:", REPL("-?",3), "aDogA"   , "ARR", HB_ValToExp(aDog) } )
//{ "" , "Date and time of contract control:"          , cDT         , "cDTDg"   , "DT" , ""   } )
//{ "" , "Selection window - button font:"             , REPL("?",5) , "aFont"   , "FNT", ""     } )
//{ "" , "Selection window - button font color:"       , SPACE(5)    , "aBntFClr", "CLR", HB_ValToExp(RED)    } )
//{ "" , "Selection window - button background color:" , SPACE(5)    , "aBntBClr", "CLR", HB_ValToExp(YELLOW) } )
////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbEditPrev( uVal, ob )
   LOCAL nCol, oCol, cNam, cAls, uOld, lRet, cJTyp, aFnt, aClr
   LOCAL cTyp, cMsg, lWrt, cStr, xVal, aDim, cRet, aRet

   WITH OBJECT ob
      nCol  := :nCell
      oCol  := :aColumns[ nCol ]
      cAls  := :cAlias
      cTyp  := oCol:cFieldTyp        // тип обработки колонки
      cNam  := oCol:cName
      cJTyp := ob:aArray[ob:nAt][5]  // тип обработки строки
      Default cJTyp := "+"
   END WITH

   uOld := uVal
   //? ProcNL(), nCol, cTyp
   cStr := 'oCol:bPrevEdit !;'
   cStr += 'oCol:cName = (' + cNam + ");"
   cStr += 'Column array cJTyp: "' + cJTyp + '" ;'
   cStr += 'NO processing for this field!;'
   lWrt := .T.        // записывать поле

   IF LEN(cJTyp) == 0             // нет обработки
      lRet := .F.                 // не давать редактировать поле в :get
   ELSEIF cJTyp $ "NLCD"
      lRet := .T.                 // редактировать поле в :get
   ELSEIF cJTyp $ "DMN"
      cTyp := "D"
      aRet := CellEdit_DT(ob, cTyp, uVal)
      IF LEN(aRet) > 0
         uVal := aRet[1]
      ENDIF
      lRet := .F.                 // не давать редактировать поле в :get
   ELSEIF cJTyp $ "DT"
      cTyp := "T"
      aRet := CellEdit_DT(ob, cTyp, uVal)
      IF LEN(aRet) > 0
         uVal := aRet[1]
      ENDIF
      lRet := .F.                 // не давать редактировать поле в :get
   ELSEIF cJTyp $ "CLR"
      xVal := ob:aArray[ob:nAt][6]
      aClr := myStrToArray( xVal )
      aRet := Tsb_ChangeColor(ob,aFnt) // см.ниже
      IF IsArray(aRet)
         IF LEN(aRet) > 0
            ob:aArray[ob:nAt][6] := HB_ValToExp(aRet)
            uVal := SPACE(5)
         ENDIF
      ENDIF
      lRet := .F.                 // не давать редактировать поле в :get
   ELSEIF cJTyp $ "FNT"
      aFnt := myStrToArray( "{" + uVal + "}" )
      aRet := Tsb_ChangeFont(ob,aFnt) // см.ниже
      IF LEN(aRet) > 0
         cRet := HB_ValToExp(aRet)
         uVal := CHARREM( '{}', cRet )
      ENDIF
      lRet := .F.                  // не давать редактировать поле в :get
   ELSEIF cJTyp $ "ARR"
      xVal := ob:aArray[ob:nAt][6]
      xVal := ALLTRIM(xVal)
      aDim := myStrToArray( xVal )
      cRet := Tsb_ContexMenu(ob,aDim)  // см.ниже
      IF LEN(cRet) > 0
         uVal := cRet
      ENDIF
      lRet := .F.                      // не давать редактировать поле в :get
   ELSE
      //? ProcNL(), "uVal=", uVal, HB_ValToExp(uVal)
      cMsg := ProcNL(0) + ";" + ProcNL(1) + ";;"
      AlertStop(cMsg + cStr,,,64,{RED})
      lWrt := .F.             // не записывать в ячейку
      lRet := .F.             // не давать редактировать поле в :get
   ENDIF

   IF lWrt                         // записывать ячейку
      ob:Cargo:nModify ++          // счётчик-изменения в таблице
      ob:SetValue(nCol,uVal)
   ENDIF
   ob:DrawSelect()    // перерисовать текущую ячейку таблицы
   ob:SetFocus()

   DO EVENTS

RETURN lRet

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbEditPost( uVal, ob )
   LOCAL nCol, oCol, cNam, uOld, cAls, lMod, cJTyp
   LOCAL oWnd  := _WindowObj(ob:cParentWnd)
   LOCAL cTyp, cMsg, cStr

   WITH OBJECT ob
      nCol := :nCell
      oCol := :aColumns[ nCol ]
      cNam := oCol:cName
      cTyp := oCol:cFieldTyp        // тип обработки колонки
      uOld := oCol:xOldEditValue    // old value
      lMod := ! uVal == uOld        // .T. - modify value
      cAls := :cAlias
      cJTyp := ob:aArray[ob:nAt][5]  // тип обработки строки
      Default cJTyp := "+"
   END WITH

   cStr := 'oCol:bPrevEdit !;'
   cStr += 'oCol:cName = ' + cNam
   cStr += ';Column array cJTyp: "' + cJTyp + '" ;'

   IF cJTyp $ "CNDL"
      // стандартная обработка
   ELSE
      cMsg := ProcNL(0) + ";" + ProcNL(1) + ";;"
      AlertInfo(cMsg + cStr,,,64,{RED})
      RETURN .F.
   ENDIF
   ob:DrawSelect()    // перерисовать текущую ячейку таблицы
   ob:SetFocus()

   DO EVENTS

RETURN .T.

////////////////////////////////////////////////////////////////////////////
// CLR_PANE  , {|nr,nc,ob| // 2 , фона в ячейках таблицы
FUNCTION myAlertTsbColorBack(nr,nc,ob)
   Local nClr, aZebra, cType, nClr6, aColor, xVal, nBack
   // ob:aArray[ob:nAt][4], ob:aArray[ob:nAt][5], ob:aArray[ob:nAt][6]
   //  1                     2                                 3           4          5       6
   //{ "" , "Selection window - button font:"             , REPL("?",5), "aFont"   , "FNT", ""                  } )
   //{ "" , "Selection window - button font color:"       , SPACE(5)   , "aBntFClr", "CLR", HB_ValToExp(RED)    } )
   //{ "" , "Selection window - button background color:" , SPACE(5)   , "aBntBClr", "CLR", HB_ValToExp(YELLOW) } )
   //? ProcNL(), "nAt=", nr, "nCol=", nc

   nBack := GetSysColor( COLOR_BTNFACE )   // цвет фона системы
   // убрать засветку на правой виртуальной колонке
   // remove highlight on the right virtual column
   IF IsObject( ob:oPhant ) ;  ob:oPhant:nClrBack := nBack
   ENDIF

   nr := nc  // nr - здесь не использую - это ob:nAt
   aZebra := ob:Cargo:aZebra
   //? ProcNL(), "aZebra=",aZebra
   IF !IsArray(aZebra)
      aZebra := { HMG_RGB2n({230,230,230}), HMG_RGB2n(SILVER) }
   ENDIF

   IF LEN(ob:aArray[ob:nAt]) == 6
      cType := ob:aArray[ob:nAt][5]
      IF !IsString(cType) ; cType := "NOT5"
      ENDIF
      xVal := ob:aArray[ob:nAt][6]
      If cType == "CLR"
         xVal   := ALLTRIM(xVal)
         aColor := myStrToArray( xVal )
         IF !IsArray(aColor) ; aColor := BLACK
         ENDIF
         nClr6 := HMG_RGB2n(aColor)
      Endif
   ELSE
      cType := "NOT5"
      nClr6 := HMG_RGB2n(MAROON)
   ENDIF

   nClr := CLR_HRED
   If ob:nAt % 2 == 0
      nClr := aZebra[2]  // строка % 2
   Else
      nClr := aZebra[1]  // цвет фона таблицы
   Endif

   If cType == "CLR"
      nClr := nClr6
   Endif

RETURN nClr

///////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Tsb_ContexMenu(oBrw, aDim)
   LOCAL oWnd, cForm, hFont1, hFont2, nY, nX, cRet, nI, cMenu, bAction
   LOCAL lChk, lDis, hFont, lMenuStyle, nMenuBitmap, cImg, lIcon, nMenu
   LOCAL aFont, nFSize, cName, nWCell, nHCell, oCell

   cForm  := oBrw:cParentWnd
   oWnd   := _WindowObj(oBrw:cParentWnd)
   // координаты ячейки в которой Edit
   oCell  := oBrw:GetCellInfo(oBrw:nRowPos)
   nY     := oWnd:Row + oCell:nRow - 2 //+ GetTitleHeight() /*+ GetMenuBarHeight()*/
   //nY   += oBrw:nTop + IIF( oBrw:lDrawSuperHd, oBrw:nHeightSuper , 0 )
   //nY   += oBrw:nHeightHead
   nX     := oWnd:Col + oCell:nCol + 2
   nWCell := oCell:nWidth - 2
   nHCell := oCell:nHeight - 2

   hFont1 := GetFontHandle("ComSanMS")
   hFont2 := GetFontHandle("DlgFont")
   aFont  := GetFontParam("DlgFont")
   nFSize := aFont[2]
   nMenu  := 0
   lIcon  := .T.   // иконки в меню - резерв

   lMenuStyle  := IsExtendedMenuStyleActive()     // menu style EXTENDED/STANDARD
   nMenuBitmap := GetMenuBitmapHeight()           // bmp height in context menu
   SET MENUSTYLE EXTENDED                         // switch menu style to advanced
   SetMenuBitmapHeight( nFSize*2 )                // set image size

   DEFINE CONTEXT MENU OF &cForm
      FOR nI := 1 TO LEN(aDim)
         cName   := StrZero(nI, 10)
         cImg    := ""
         cMenu   := aDim[nI]
         bAction := {|| nMenu := Val( This.Name ) }
         lChk    := .F.
         lDis    := .F.
         hFont   := IIF( lDis, hFont2, hFont1 )

         IF lIcon
            _DefineMenuItem( cMenu, bAction, cName,     , lChk, lDis, , hFont , , .F., .F. , cImg, .F. )
         ELSE
            _DefineMenuItem( cMenu, bAction, cName, cImg, lChk, lDis, , hFont , , .F., .F. )
         ENDIF
      NEXT
      SEPARATOR
      MENUITEM  "Delete value" ACTION  {|| nMenu := -1 } FONT hFont2
      SEPARATOR
      MENUITEM  "Exit"  ACTION  {|| nMenu := -99 } FONT hFont2 ICON "iExit32"
   END MENU

   _ShowContextMenu(cForm, nY, nX, .f. ) // SHOWING DROP OUT MENU
   InkeyGui(100)

   DEFINE CONTEXT MENU OF &cForm         // delete menu after exiting
   END MENU

   SetMenuBitmapHeight(nMenuBitmap) // bmp height in context menu   - return as it was
   _NewMenuStyle( lMenuStyle )      // menu style EXTENDED/STANDARD - return as it was

   DO EVENTS

   IF nMenu > 0
      cRet := "(" + HB_NtoS(nMenu)+")-("
      cRet += aDim[nMenu] + ")"
   ELSEIF nMenu == -1
      cRet := "-"
   ELSE
      cRet := ""
   ENDIF

   DO EVENTS

RETURN cRet

///////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Tsb_ChangeFont(oBrw,aFnt)
   LOCAL aFont, hFont, aF, cForm, aRet := {}

   cForm := oBrw:cParentWnd
   hFont := GetFontHandle( "TsbEdit" )
   aF    := GetFontParam( hFont )

   IF LEN(aFnt) == 0
      aFnt := aF
   ENDIF

   // вызвать стандартное меню шрифт для Windows
   aFont := GetFont( aFnt[1], aFnt[2], aFnt[3], aFnt[4], {0,0,0} , .f. , .f. , 0 )
   if ! empty ( aFont[1] )
      aRet := { aFont[1], aFont[2], aFont[3], aFnt[4] }
   EndIf

RETURN aRet

///////////////////////////////////////////////////////////////////
STATIC FUNCTION Tsb_ChangeColor(oBrw, aClr)
   LOCAL aColor := {}, cForm

   cForm := oBrw:cParentWnd
   // вызвать стандартное WINDOWS меню цвета
   aColor := GetColor(aClr)
   IF aColor[1] # NIL
      aClr  := aColor
   ENDIF

RETURN aClr

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION CellEdit_DT(oBrw,cType,xGet)
   LOCAL oCell  := oBrw:GetCellInfo(oBrw:nRowPos)
   LOCAL nY     := oCell:nRow + oBrw:nHeightHead
   LOCAL nX     := oCell:nCol
   LOCAL nWCell := oCell:nWidth - 2
   LOCAL nHCell := oCell:nHeight //- 2
   LOCAL oWnd, hWnd, oJWnd, aRet, cForm, nWBtn, nHObj, nHIco, aTime, cVal
   LOCAL cFont, nFSize, aFont, cText, nWDate, dDate1, tDTime, nW, nH

   //? ProcNL(), "cType=", cType, "xGet=", xGet, "VALTYPE=", VALTYPE(xGet)
   cForm := oBrw:cParentWnd
   oJWnd := _WindowObj(oBrw:cParentWnd)

   nY    += oJWnd:Row - 5
   nX    += oJWnd:Col + 7
   IF oBrw:lDrawSpecHd
      nY -= oBrw:nHeightSpecHd    // высота спецхидера ENUMERATOR
   ENDIF

   nY     += IIF( App.Cargo:aDisplayMode[2] <= 720, 8, 4 )
   nHCell += IIF( App.Cargo:aDisplayMode[2] <= 720, 3, 0 )

   aFont  := GetFontParam(oBrw:hFont)
   cFont  := aFont[1]
   nFSize := aFont[2]

   nHObj  := nHCell - 7 //nFSize * 2
   nHIco  := nHObj - 2
   cText  := "120DECEMBER020240"
   nWDate := GetTxtWidth( cText, nFSize, cFont, .T. ) + 65
   IF cType $ "@T"
      cText  := REPL("0",24) + '0|0'
      nWDate := GetTxtWidth( cText, nFSize, cFont, .T. ) + 55
   ENDIF
   nWBtn  := nHCell + nHCell + 4       // две кнопки
   nW     := nWDate + nWBtn
   aRet   := {}   // всегда массив - пусто, значит отказ от ввода

   // выход за границы экрана/прижимаем к правому концу ячейки
   IF nX + nW > App.Cargo:aDisplayMode[2] //Sys.ClientWidth
      nX := (nWCell + nX) - nW
   ENDIF
   nH := nHCell

   // новое окно в ячейку таблицы
   DEFINE WINDOW Cell AT nY,nX WIDTH nW HEIGHT nH  ;
      MODAL NOCAPTION                              ;
      FONT cFont SIZE nFSize                       ;
      ON LOSTFOCUS {|| oWnd:Release() }            ;
      ON INIT      {|| DoEvents() }

      oWnd := ThisWindow.Object
      hWnd := oWnd:Handle

      IF cType == "D"

         IF VALTYPE(xGet) == "C"
            xGet := CTOD(xGet)
         ELSEIF VALTYPE(xGet) == "D"
         ELSE
            xGet := CTOD('')
         ENDIF
         dDate1 := xGet
         IF dDate1 == CTOD('')
            dDate1 := DATE()
         ENDIF

         @ 3, 3 DATEPICKER Date_1 VALUE dDate1 WIDTH nWDate HEIGHT nHObj ;
            DATEFORMAT "dd'.'MMMM' 'yyyy" SHOWNONE
         nX := This.Date_1.Width + 5

         @ 3, nX BUTTONEX Btn_Ok WIDTH nHObj HEIGHT nHObj CAPTION '' ;
           ICON "iOk32" NOHOTLIGHT NOXPSTYLE HANDCURSOR ;
           ACTION {|| aRet := { This.Date_1.Value } , oWnd:Release() }

         This.Btn_Ok.ImageWidth  := nHIco
         This.Btn_Ok.ImageHeight := nHIco
         This.Btn_Ok.Icon        := LoadIconByName( "iMg_Ok32", nHIco, nHIco )

         nX += This.Btn_Ok.Width + 5

         @ 3, nX BUTTONEX Btn_Esc WIDTH nHObj HEIGHT nHObj CAPTION '' ;
           ICON "iCancel32" NOHOTLIGHT NOXPSTYLE HANDCURSOR ;
           ACTION {||  aRet := {} , oWnd:Release() }

         This.Btn_Esc.ImageWidth  := nHIco
         This.Btn_Esc.ImageHeight := nHIco
         This.Btn_Esc.Icon        := LoadIconByName( "iMg_Cancel32", nHIco, nHIco )

      ELSEIF cType $ "@T"

         IF IsString(xGet)
            tDTime := hb_CToT(xGet)
         ELSE
            tDTime := xGet
         ENDIF
         IF tDTime == hb_CToT("")
            tDTime := hb_DateTime()
         ENDIF
         dDate1   := hb_TToD(tDTime)
         aTime    := {0,0,0}
         cVal     := hb_TtoS(tDTime)   // 2003 12 20 191944859
         aTime[1] := VAL(SUBSTR(cVal,9,2))
         aTime[2] := VAL(SUBSTR(cVal,11,2))
         aTime[3] := VAL(SUBSTR(cVal,13,2))

         @ 3, 3 DATEPICKER Date_2 VALUE dDate1 WIDTH nWDate-3 HEIGHT nHObj ;
           SHOWNONE UPDOWN DATEFORMAT "dd MMMM yyyy' | 'HH:mm:ss"

         This.Date_2.VALUE := { Year( dDate1 ), Month( dDate1 ), Day( dDate1 ), aTime[1], aTime[2], aTime[3] }
         nX := This.Date_2.Width + 5

         @ 3, nX BUTTONEX Btn_Ok WIDTH nHObj HEIGHT nHObj CAPTION '' ;
           ICON "iOk32" NOHOTLIGHT NOXPSTYLE HANDCURSOR ;
           ACTION {|| tDTime := This.Date_2.Value  ,;
                      aRet   := { tDTime } , oWnd:Release() }

         This.Btn_Ok.ImageWidth  := nHIco
         This.Btn_Ok.ImageHeight := nHIco
         This.Btn_Ok.Icon        := LoadIconByName( "iMg_Ok32", nHIco, nHIco )

         nX += This.Btn_Ok.Width + 5

         @ 3, nX BUTTONEX Btn_Esc WIDTH nHObj HEIGHT nHObj CAPTION '' ;
           ICON "iCancel32" NOHOTLIGHT NOXPSTYLE HANDCURSOR ;
           ACTION {|| aRet := {} , oWnd:Release() }

         This.Btn_Esc.ImageWidth  := nHIco
         This.Btn_Esc.ImageHeight := nHIco
         This.Btn_Esc.Icon        := LoadIconByName( "iMg_Cancel32", nHIco, nHIco )

      ENDIF

       DRAW LINE IN WINDOW Cell AT 2, 2 TO 2, nW PENCOLOR RED PENWIDTH 4
       DRAW LINE IN WINDOW Cell AT nH-2, 2 TO nH-2, nW PENCOLOR RED PENWIDTH 4
       DRAW LINE IN WINDOW Cell AT 2, 2 TO nH, 2 PENCOLOR RED PENWIDTH 4
       DRAW LINE IN WINDOW Cell AT 2, nW-2 TO nH, nW-2 PENCOLOR RED PENWIDTH 4

   END WINDOW

   SetWindowLong(hWnd, GWL_STYLE, WS_BORDER)

   _DefineHotKey ( "CELL" , 0 , VK_ESCAPE , {|| oWnd:Release() } )
   _DefineHotKey ( "CELL" , 0 , VK_RETURN , {|| oWnd:Release() } )
   Cell.Activate

RETURN aRet // всегда массив, если пусто - значит отказ от ввода

///////////////////////////////////////////////////////////////////////////////
FUNCTION myStrToArray( cBuf )
   LOCAL aBuf, lBuf

   IF left(cBuf, 3) == ["{"]
      cBuf := subs(cBuf, 2)
   ENDIF
   IF right(cBuf, 3) == ["}"]
      cBuf := left(cBuf, Len(cBuf)-1)
   ENDIF
   lBuf := .F.
   IF left(cBuf, 1) == "{" .and. right(cBuf, 1) == "}"
      BEGIN SEQUENCE WITH {|e| break( e ) }
         aBuf := &(cBuf)
         lBuf := .T.
      END SEQUENCE
   ELSE
      aBuf := {}
   ENDIF
   IF !lBuf
      aBuf := {}
   ENDIF

RETURN aBuf

/////////////////////////////////////////////////////////////////////////////////////
// выборка из массива по какомо то условию - резерв
FUNCTION ThisGetTsbIf(oBrw)
   LOCAL nI, aVal, aDim

   aDim := oBrw:aArray // весь массив

   FOR nI := 1 TO Len(oBrw:aArray)
      aVal := oBrw:aArray[nI]
   NEXT

RETURN aDim

///////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myGetTsb_WH(o1Brw,aFontTsb)   // размеры ТСБ / TSB dimensions
   LOCAL aRet, nW1Col, nHCell, nHHead, nHFoot, nHSpecHd, nHSuperHd, aHead
   LOCAL nLine, nK, nI, nWSlctr

   //oTsb:uSelector := 20
   IF IsNumeric(o1Brw:uSelector) .AND. LEN(o1Brw:uSelector) > 0
      nWSlctr := o1Brw:uSelector
   ELSE
      nWSlctr := 0
   ENDIF

   // колонка нумерации - ставим всегда / numbering column - always put
   nW1Col := 0
   //IF IsArray(o1Brw:aNumber) .AND. LEN(o1Brw:aNumber) > 0
   nW1Col := GetFontWidth(aFontTsb[4], TSB_W1COL)  
   //ENDIF

   // высота ячеек
   nHCell := 0
   IF IsNumeric(o1Brw:nHeightCell)
      nHCell := o1Brw:nHeightCell              
   ELSE
      nHCell := INT( GetFontHeight(aFontTsb[1])*1.35 )
      // высота ячеек = высоте картинки чекбокса
      nHCell := IIF( nHCell < 32, 32, nHCell )   
   ENDIF

   // высота шапки
   nHHead := 0
   IF IsNumeric(o1Brw:nHeightHead)
      nHHead := o1Brw:nHeightHead              
   ELSE
      nHHead := nHCell                         
   ENDIF
   nLine := 1
   // oTsb:aHead := { "User;Code" , "User" , "Group" , "Label", "Edited;Date;Time", "Print"  }
   IF IsArray(o1Brw:aHead)
      aHead := o1Brw:aHead
      FOR nI := 1 TO LEN(aHead)
         nK    := NUMAT( ";", aHead[nI] ) + 1
         nLine := MAX(nLine, nK)
      NEXT
   ENDIF
   nHHead := INT( nHHead * nLine * 0.7 )

   // высота подвала
   nHFoot := 0
   IF IsNumeric(o1Brw:nHeightFoot)
      nHFoot := o1Brw:nHeightFoot              
   ELSE
      nHFoot := nHCell                
   ENDIF

   // высота нумератора
   nHSpecHd := 0
   IF IsLogic(o1Brw:lSpecHd) .AND. o1Brw:lSpecHd
      nHSpecHd := INT( GetFontHeight(aFontTsb[4])*1.35 )      
   ENDIF

   // высота суперхидера
   nHSuperHd := 0                     
   IF IsLogic(o1Brw:lSuperHd) .AND. o1Brw:lSuperHd
      IF IsNumeric(o1Brw:nHeightSuperHd)
         nHSuperHd := o1Brw:nHeightSuperHd                     
      ELSE
         nHSuperHd := 30                     
      ENDIF
   ENDIF

   aRet := { nWSlctr, nW1Col, nHCell, nHHead, nHFoot, nHSpecHd, nHSuperHd }
   ? ProcNL(), "aWZ=", HB_ValToExp(aRet)

RETURN aRet
///////////////////////////////////////////////////////////////////////////////
// получить Width текста
STATIC FUNCTION GetTxtWidth( cText, nFontSize, cFontName, lBold )
   LOCAL hFont, nWidth
   DEFAULT cText     := REPL('A', 2)        ,  ;
           cFontName := _HMG_DefaultFontName,  ;
           nFontSize := _HMG_DefaultFontSize,  ;
           lBold     := .F.

   IF Valtype(cText) == 'N'
      cText := repl('A', cText)
   ENDIF

   hFont  := InitFont(cFontName, nFontSize, lBold)
   nWidth := GetTextWidth(0, cText, hFont)         // ширина текста
   DeleteObject (hFont)

RETURN nWidth
