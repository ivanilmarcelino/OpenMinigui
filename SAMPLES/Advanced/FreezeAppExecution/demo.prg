/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2023 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 * Copyright 2023 Sergej Kiselev <bilance@bilance.lv>
 *
 * Заморозить/заблокировать выполнение программы
 * Freeze/block program execution
*/
//#define _HMG_OUTLOG
#define PRG_NAME      "Freeze/block program execution" 
#define PRG_NOTEPAD   GetStartUpFolder() + "\demo.prg"  
#define EXE_NOTEPAD   "notepad.exe"  

#include "minigui.ch"
#include "TSBrowse.ch"

STATIC hProcessOpenHandle := 0
////////////////////////////////////////////////////////////////////////////////
PROCEDURE Main
   LOCAL cTitle := "MiniGui demo. Freeze/block program execution"
   LOCAL nW, nH, aBackClrTitle, aBackcolor := {159,191,236}
   LOCAL nWBtn, nHBtn, nG, nRow, nCol, nHLbl, cMsg, lQ
   LOCAL nHIco, cResImg := "DEMO", aImgSize := BmpSize(cResImg)
   LOCAL bAct, aBtn, nWImg := aImgSize[1], nHImg := aImgSize[2]

   nG    := nRow := nCol := 20
   nWBtn := 240
   nH    := nHImg + nG * 2 + GetTitleHeight() * 2 + GetBorderHeight()
   nW    := nCol + nWImg + nG + nWBtn + nG + GetBorderWidth() * 2
   aBackClrTitle := {12,66,126}

   DEFINE WINDOW wMain             ;
      AT 150,nW WIDTH nW HEIGHT nH ;
      TITLE PRG_NAME ICON "1_MAIN" ;
      MAIN NOMAXIMIZE NOSIZE       ;
      BACKCOLOR  aBackcolor        ;
      FONT "Tahoma" SIZE 14        ;
      ;//ON RELEASE {|| _LogFile(.T., REPL("=",20) + " End", ;
      ;//               HB_DATETIME() ), DoEvents()      }   ;  // executed before destroying the window
      ON INTERACTIVECLOSE {|lRet| lRet := myQuit() }         // NO exit while there is a window ShellExecuteEx()

      nW    := This.ClientWidth
      nH    := This.ClientHeight
      nHLbl := GetTitleHeight() 
      nHBtn := ( nH - nG * 6 - nHLbl ) / 5 
      nHIco := nHBtn - 4*2                     // высота иконки

      @ nRow, nCol IMAGE Image_1 PICTURE cResImg WIDTH nWImg HEIGHT nHImg

      @ nRow + 113, nCol + nG LABEL Label_2 VALUE "Free open source GUI: " + MiniGUIVersion() ;
        FONT "Arial" SIZE 12 BOLD FONTCOLOR YELLOW AUTOSIZE TRANSPARENT                     

      @ nRow + 245, nCol + nG LABEL Label_3 VALUE "Free open source: " + Version() ;
        FONT "Arial" SIZE 12 BOLD FONTCOLOR BLACK AUTOSIZE TRANSPARENT          

      @ nRow + 275, nCol + nG + 200 LABEL Label_4 VALUE hb_Ccompiler()  ;
        FONT "Arial" SIZE 12 BOLD FONTCOLOR YELLOW AUTOSIZE TRANSPARENT

      cMsg := " Examples: Freeze/block program execution"
      cMsg += " for all opened windows"
      @ nH - nHLbl, 0 LABEL Buff WIDTH nW HEIGHT nHLbl + 2 VALUE cMsg ;
        SIZE 10 FONTCOLOR WHITE BACKCOLOR aBackClrTitle VCENTERALIGN

      // ------ buttons on the right 
      nCol += This.Image_1.Width + nG
 
      aBtn := { "Button_1", "Variant (1)", "iPiople64x1", "iPiople64x2", nHIco, YELLOW }
      bAct := {|ath| ath := _ThisInfo() , This.Button_1.Enabled := .F. ,; 
                     myWindow(1,This.Cargo) , _ThisInfo(ath)           ,; 
                     This.Button_1.Enabled := .T. , To_Focus("wMain", "Buff") } 
      myDrawButton(nRow, nCol, nWBtn, nHBtn, aBtn, bAct)
      nRow += nHBtn + nG

      aBtn := { "Button_2", "Variant (2)", "iMess64x1", "iMess64x2", nHIco, PURPLE }
      bAct := {|ath| ath := _ThisInfo() , This.Button_2.Enabled := .F. ,;
                     myWindow(2,This.Cargo) , _ThisInfo(ath)           ,; 
                     This.Button_2.Enabled := .T. , To_Focus("wMain", "Buff") } 
      myDrawButton(nRow, nCol, nWBtn, nHBtn, aBtn, bAct)
      nRow += nHBtn + nG

      aBtn := { "Button_3", "Variant (3)", "iMusic64x1", "iMusic64x2", nHIco, GREEN }
      bAct := {|ath| ath := _ThisInfo() , This.Button_3.Enabled := .F. ,;
                     myWindow(3,This.Cargo) , _ThisInfo(ath)           ,;
                     This.Button_3.Enabled := .T. , To_Focus("wMain", "Buff") } 
      myDrawButton(nRow, nCol, nWBtn, nHBtn, aBtn, bAct)
      nRow += nHBtn + nG

      aBtn := { "Button_4", "Variant (4)", "iAbout64x1", "iAbout64x2", nHIco, ORANGE }
      bAct := {|ath| ath := _ThisInfo() , This.Button_4.Enabled := .F. ,;
                     myWindow(4,This.Cargo) , _ThisInfo(ath)           ,; 
                     This.Button_4.Enabled := .T. , To_Focus("wMain", "Buff") } 
      myDrawButton(nRow, nCol, nWBtn, nHBtn, aBtn, bAct)
      nRow += nHBtn + nG

      aBtn := { "Button_5", "Exit", "iExit64x1", "iExit64x2", nHIco, MAROON }
      bAct := {|| This.Button_5.Enabled := .F.     ,;
                  IIF( lQ := myQuit() , ThisWindow.Release , wMain.Buff.Setfocus ) ,;
                  This.Button_5.Enabled := .T.  }   
      myDrawButton(nRow, nCol, nWBtn, nHBtn, aBtn, bAct)

      ON KEY F1     ACTION NIL
 
   END WINDOW

   CENTER WINDOW wMain
   ACTIVATE WINDOW wMain

RETURN

/////////////////////////////////////////////////////////////////////
// Initialize variables for my program (example)
INIT PROCEDURE MyInitWin()
   LOCAL tTime := HB_DATETIME()
   LOCAL cFont := "DejaVu Sans Mono", nSize := 12
   LOCAL cLog  := "_msg.log" 
   LOCAL aBClrDlg := { 238, 249, 142 }

   SET LANGUAGE TO RUSSIAN
   SET CODEPAGE TO RUSSIAN

   SET DATE    TO GERMAN  
   SET EPOCH   TO 2000
   SET CENTURY ON
   SET OOP ON      // OOP we use

   // new log filename for debug output
   _SetGetLogFile( cLog ) ; DELETEFILE(cLog)

   // the main fund of the program
   SET FONT TO cFont, nSize

   // font for functions HMG_Alert() и Alert...() 
   DEFINE FONT DlgFont FONTNAME cFont SIZE nSize + 2

   // font for TBROWSE 
   DEFINE FONT TsbNorm   FONTNAME "DejaVu Sans Mono"   SIZE nSize
   DEFINE FONT TsbBold   FONTNAME "Comic Sans MS"      SIZE nSize BOLD
   DEFINE FONT TsbSpecH  FONTNAME _HMG_DefaultFontName SIZE _HMG_DefaultFontSize BOLD
   DEFINE FONT TsbSuperH FONTNAME "Comic Sans MS"      SIZE nSize + 2 BOLD
   DEFINE FONT TsbEdit   FONTNAME _HMG_DefaultFontName SIZE _HMG_DefaultFontSize BOLD

   // other setting
   SET DEFAULT ICON       TO "1_MAIN"
   SET MSGALERT BACKCOLOR TO aBClrDlg
   SET NAVIGATION EXTENDED
   SET TOOLTIP BALLOON ON
   //SET WINDOW MODAL PARENT HANDLE ON  // Modal windows get parent -> active window

   ? REPL("=",20) + " Program start - " + HB_TTOC( tTime ) + " " + REPL("=",20)
   ? "Screen resolution: " + HB_NtoS(GetDesktopWidth())+" x "+HB_NtoS(GetDesktopHeight())
   ? MiniGuiVersion()  ; ? Version() ; ? hb_Ccompiler() ; ? ProcNL() ; ?

   PUBLIC tPubStart, aPubThisDir, aPubTsbFont, cPubWarn  // here you can declare other necessary variables
   M->tPubStart   := tTime
   M->aPubThisDir := DIRECTORY( GetStartUpFolder() + "\*.*" )
   ASORT(M->aPubThisDir,,, { |x, y| x[1] < y[1] })

   M->aPubTsbFont := { "TsbNorm", "TsbBold", "TsbBold", "TsbSpecH", "TsbSuperH", "TsbEdit" }

   //M->cPubWarn := "ВНИМАНИЕ ! Запущена внешняя программа !" + CRLF
   //M->cPubWarn += "Окно будет доступно ТОЛЬКО после её закрытия !" + CRLF
   M->cPubWarn := "WARNING ! An external program is running!" + CRLF
   M->cPubWarn += "Window will be available ONLY after closing it !" + CRLF

   //Иконки - это не контролы и просто рисуются и забываются. Хранится только
   //последний handle, который удаляется, освобождая память, при след. рисовании иконки 
   PUBLIC aPubDrawIcon
   M->aPubDrawIcon := {}    // здесь будут храниться все открытые иконки

RETURN

///////////////////////////////////////////////////////////////////////////////
FUNCTION myQuit()        // NO exit while there is a window ShellExecuteEx()
   LOCAL cMsg, lRet, cFrm, cWin := "", nCnt := 0, lQuit := .T. 

   FOR EACH cFrm IN HMG_GetForms()
       IF GetWindowType(cFrm) == "S" ; nCnt++
          cWin += cFrm + ";"
       ENDIF
   NEXT

   IF nCnt > 0
      cMsg := "You can't exit the program !;"
      cMsg += "You need to close the program windows:;" 
      cMsg += cWin
      AlertStop(cMsg, "Attention")
      RETURN .F.
   ENDIF

   IF lQuit
      IF hProcessOpenHandle > 0
         cMsg := M->cPubWarn + CRLF
         cMsg += "You need to close the program window !" + CRLF
         cMsg += EXE_NOTEPAD + " " + PRG_NOTEPAD + CRLF + CRLF
         cMsg += "hProcessOpenHandle > 0"
         AlertStop(cMsg, "Attention")
         lRet := .F.
      ELSE
         lRet := .T.
      ENDIF
   ELSE
      lRet := .T.  // always exit is for debugging
   ENDIF

RETURN lRet


///////////////////////////////////////////////////////////////////////////////
FUNCTION myWindow(nJ,aCargo)
   LOCAL nY, nX, nW, nH, nG, nWBtn, nHBtn, cTitle, cForm, bAction, cWndMain
   LOCAL nBtn, cBtn, aBtn, cCapt, cIco1, cIco2, aBColor, nI, nY2, nWObj
   LOCAL oBrw, aArray, aHead, aGroup, hIcon1, hIcon2

   nBtn     := nJ
   aBtn     := aCargo[1]
   cWndMain := aCargo[2]
   cBtn     := aCargo[3]
   cCapt    := aBtn[2]
   cIco1    := aBtn[3]
   cIco2    := aBtn[4]
   aBColor  := aBtn[6]
   cForm    := "STANDARD_" + HB_NtoS(nBtn)
   cTitle   := "Test windows: " + cForm + "  Button: " + cCapt
   aGroup   := { "RadioGroup-1", "RadioGroup-2" }

   ? ProcNL(), aCargo, aBColor, cWndMain
   nY := nX := 50 * nBtn
   nW := nH := 600
   nG := 20     

   IF nBtn == 1     
      bAction := {||// Перебор окон и объектов / Iterating over windows and objects 
                    Local cw := cForm                    // имя будущей формы
                    Local cn := "Button_1"               // кнопка определяется ниже
                    SetProperty(cw,cn, "Enabled", .F.)   // блокировка кнопки
                    //This.&(cn).Enabled := .F.          
                    myIterating(ThisWindow.Name) 
                    // проверка обязательна, т.к. возможно закрытие формы
                    If _IsWindowDefined(cw) 
                       If _IsControlDefined(cn, cw)
                          SetProperty(cw,cn, "Enabled", .T.)  // разблокировка кнопки
                       Endif
                       To_Focus( cw, "Buff" )        // Buff определяется ниже
                    Endif
                    Return Nil
                   }

   ELSEIF nBtn == 2 
      bAction := {|| // Изменение прозрачности окон / Changing window transparency 
                    Local cw := cForm                    // имя будущей формы
                    Local cn := "Button_1"               // кнопка определяется ниже
                    SetProperty(cw,cn, "Enabled", .F.)   // блокировка кнопки
                    myTransparence(ThisWindow.Name) 
                    // проверка обязательна, т.к. возможно закрытие формы
                    If _IsWindowDefined(cw) 
                       If _IsControlDefined(cn, cw)
                          SetProperty(cw,cn, "Enabled", .T.)  // разблокировка кнопки
                       Endif
                       To_Focus( cw, "Buff" )        // Buff определяется ниже
                    Endif
                    Return Nil
                   }

   ELSEIF nBtn == 3 
      bAction := {|| // AlphaBlend прозрачности окон / AlphaBlend window Transparency
                    Local cw := cForm                    // имя будущей формы
                    Local cn := "Button_1"               // кнопка определяется ниже
                    SetProperty(cw,cn, "Enabled", .F.)   // блокировка кнопки
                    myAlphaBlendTransp(ThisWindow.Name)
                    // проверка обязательна, т.к. возможно закрытие формы
                    If _IsWindowDefined(cw) 
                       If _IsControlDefined(cn, cw)
                          SetProperty(cw,cn, "Enabled", .T.)  // разблокировка кнопки
                       Endif
                       To_Focus( cw, "Buff" )        // Buff определяется ниже
                    Endif
                    Return Nil
                   }

   ELSEIF nBtn == 4 
      bAction := {||// мой вариант (наложение прозрачного окна) / my version (transparent window overlay)
                    Local cw := cForm                    // имя будущей формы
                    Local cn := "Button_1"               // кнопка определяется ниже
                    SetProperty(cw,cn, "Enabled", .F.)   // блокировка кнопки
                    myFormsDarken(ThisWindow.Name) 
                    // проверка обязательна, т.к. возможно закрытие формы
                    If _IsWindowDefined(cw) 
                       If _IsControlDefined(cn, cw)
                          SetProperty(cw,cn, "Enabled", .T.)  // разблокировка кнопки
                       Endif
                       To_Focus( cw, "Buff" )        // Buff определяется ниже
                    Endif
                    Return Nil
                   }
   ENDIF

   DEFINE WINDOW &cForm AT nY,nX WIDTH nW HEIGHT nH TITLE cForm            ;
      WINDOWTYPE STANDARD TOPMOST NOMAXIMIZE NOSIZE BACKCOLOR aBColor      ;
      ON INIT    {|| This.Topmost := .F., DoEvents(), This.Buff.Setfocus } ; 
      ON RELEASE {|| myFormDestroyIcon(cForm) }

      nW    := This.ClientWidth
      nH    := This.ClientHeight
      nHBtn := 80 
      nWBtn := ( nW - nG * 3 ) / 2
      nY    := nX := nG

      @ 5, 0 LABEL Buff WIDTH nW HEIGHT 20 VALUE cTitle TRANSPARENT CENTERALIGN
      nY := This.Buff.Height + nG/2

      /////////////////////// Building Browse  ////////////////////////////
      aArray  := M->aPubThisDir

      DEFINE TBROWSE Tsb_1 OBJ oBrw AT nY, nX WIDTH nW - nX*2 HEIGHT 200 CELL ;
         ALIAS  aArray                 ;      // массив - это ТСБ по массиву 
         FONT   M->aPubTsbFont         ;
         BACKCOLOR aBColor             ;
         BRUSH     aBColor             ;
         COLNUMBER { 1, 30 }  // колонка NN

      END TBROWSE ON END {|ob| ob:SetNoHoles(), ob:SetFocus() }

      nY += This.Tsb_1.Height + nG/2
      aHead := ARRAY(LEN(aArray))
      FOR nI := 1 TO LEN(aArray)
         aHead[nI] := aArray[nI,1]
      NEXT
      /////////////////////// Building Browse  ////////////////////////////

      nWObj := ( nW - nG*2 - nG*2 ) / 3

      // вывод иконок на форму / display icons on the form
      hIcon1 := LoadIconByName( cIco1, 85, 85 )
      hIcon2 := LoadIconByName( cIco2, 85, 85 )
      DRAW ICON IN WINDOW &cForm AT nY, nX HICON hIcon1 WIDTH 85 HEIGHT 85 COLOR aBColor
      DRAW ICON IN WINDOW &cForm AT nY, nX + 85 + 10 HICON hIcon2 WIDTH 85 HEIGHT 85 COLOR aBColor
      // запомнить в хранилище открытых иконок / remember in open icon storage
      AADD( M->aPubDrawIcon, { cForm, cIco1, hIcon1, nY, nX, 85, 85, aBColor           } )   
      AADD( M->aPubDrawIcon, { cForm, cIco2, hIcon2, nY, nX + 85 + 10, 85, 85, aBColor } )

      nY  += 85 + nG/2
      nY2 := nY

      @ nY, nX CHECKBOX CheckBox_1 CAPTION "CheckBox 1" VALUE .T.  ;
        WIDTH nWObj HEIGHT 25 TRANSPARENT          
      nY += This.CheckBox_1.Height + 5 

      @ nY, nX CHECKBOX CheckBox_2 CAPTION "CheckBox 2" VALUE .F.  ;
        WIDTH nWObj HEIGHT 25 FONTCOLOR RED TRANSPARENT 
      nY += This.CheckBox_2.Height + nG

      @ nY, nX COMBOBOXEX ComboEx_1  WIDTH nWObj HEIGHT 400  ;
        ITEMS aHead VALUE 1  BACKCOLOR aBColor  TOOLTIP "Sample ComboXex"  
      nX += nWObj + nG 

      nY := nY2
      @ nY, nX TEXTBOX TextBox_1  WIDTH nWObj HEIGHT 25  ;
        VALUE "TextBox 1" FONTCOLOR BLUE BACKCOLOR WHITE  
      nY += This.TextBox_1.Height + nG

      @ nY, nX GETBOX GetBox_1  HEIGHT 30 WIDTH nWObj ;
        VALUE DATE() PICTURE '@D'                     ;
        FONTCOLOR RED BACKCOLOR WHITE  TOOLTIP "Sample GetBox_1"
      nX += nWObj + nG 

      nY := nY2
      @ nY, nX FRAME Frame_1 CAPTION " Frame " WIDTH nWObj HEIGHT 90 TRANSPARENT

      @ nY + nG, nX + nG/2 RadioGroup RG_1  ; 
        Options aGroup VALUE 1   ;
        Width nWObj - nG Spacing 30   ;
        FONTCOLOR RED TRANSPARENT       

      /////////////////////// Building Button  ////////////////////////////
      nY := nH - nG - nHBtn
      nX := nG
      @ nY, nX BUTTONEX Button_1 CAPTION "Launching an"+CRLF+"external program" ICON "iEdit64x1" ;
        WIDTH nWBtn HEIGHT nHBtn BACKCOLOR SILVER BOLD ;
        NOXPSTYLE HANDCURSOR NOTABSTOP /*VERTICAL*/    ;
        ACTION Eval(bAction) 
      nX += nWBtn + nG

      @ nY, nX BUTTONEX Button_Ex CAPTION "Exit" ICON "iExit64x1" ;
        WIDTH nWBtn HEIGHT nHBtn BACKCOLOR RED BOLD  ;
        NOXPSTYLE HANDCURSOR NOTABSTOP /*VERTICAL*/  ;
        ACTION  {|| This.Button_Ex.Enabled := .F., ThisWindow.Release } 

      ON KEY F1     ACTION NIL

   END WINDOW

   //CENTER WINDOW &cForm
   ACTIVATE WINDOW &cForm ON INIT {|| This.Minimize, wApi_Sleep(50), ;
                                        This.Restore , DoEvents() }
RETURN NIL

// удаляем из хранилища открытых иконок / remove from storage of open icons
FUNCTION myFormDestroyIcon(cForm) 
   LOCAL aDim, hIcon, aNew := {}

   IF hb_IsArray(M->aPubDrawIcon) .and. Len(M->aPubDrawIcon) > 0
      ? ProcNL(), "M->aPubDrawIcon =", M->aPubDrawIcon
      FOR EACH aDim IN M->aPubDrawIcon
         ? hb_enumIndex(aDim), hb_valtoexp(aDim)
         hIcon := aDim[3]
         IF aDim[1] == cForm
            DestroyIcon(hIcon)  // удаляем хендл иконки
         ELSE
            AADD( aNew, aDim ) 
         ENDIF
      NEXT
      M->aPubDrawIcon := ACLONE(aNew)
      ? "aNew=", aNew
   ENDIF

RETURN NIL

////////////////////////////////////////////////////////////////////
// Перебор окон и объектов / Iterating over windows and objects
FUNCTION myIterating(cForm0)
   LOCAL aFreeze, nI, aFrm, aBCls, cType, cCtrl, cForm, cLabel
   LOCAL nW, nH, aObDel, aBack, cMsg, hIcon, aDim

   ? ProcNL(), "cForm0=", cForm0

   cMsg   := M->cPubWarn  // сообщение на все формы
   aObDel := {}
   aFrm   := HMG_GetForms()
   aBCls  := ARRAY(LEN(aFrm))
   aBack  := GRAY                // меняем цвет формы
   FOR nI := 1 TO LEN(aFrm)
      cForm     := aFrm[nI]
      aBCls[nI] := GetProperty( aFrm[nI], "BACKCOLOR" )
      cLabel := cForm + '_Label_Delete' 
      AADD( aObDel, { cForm, cLabel } )
   NEXT
   // удаляем из хранилища открытых иконок / remove from storage of open icons
   IF hb_IsArray(M->aPubDrawIcon) .and. Len(M->aPubDrawIcon) > 0
      ? "=== " + ProcNL(), "M->aPubDrawIcon =", M->aPubDrawIcon
      FOR EACH aDim IN M->aPubDrawIcon
         ? hb_enumIndex(aDim), hb_valtoexp(aDim)
         hIcon := aDim[3]
         DestroyIcon(hIcon)  // удаляем хендл иконки
      NEXT
   ENDIF

   DO EVENTS

   aFreeze := myListObjectsForms()
   FOR nI := 1 TO LEN(aFreeze)
      // блокируем все объекты на формах
      cForm := aFreeze[nI,1]
      cCtrl := aFreeze[nI,2]
      cType := aFreeze[nI,3]
      IF cType == "TBROWSE"
         // блокировка таблицы с закраской
         GetProperty( cForm, cCtrl, "Object" ):Hide()  // скрыть
      ELSE
         SetProperty(cForm, cCtrl, "Visible", .F.)
      ENDIF
   NEXT

   // создадим объекты на окнах
   FOR nI := 1 TO LEN(aObDel)
      cForm := aObDel[nI,1]
      cCtrl := aObDel[nI,2]
      nW := GetProperty( cForm, "Width"  ) - 10*2 - GetBorderWidth()*2 + IIF( IsXPThemeActive(), 8, 0 )
      nH := GetProperty( cForm, "Height" ) - 10*2 - GetBorderHeight() - GetTitleHeight() 
      @ 10, 10 LABEL &cCtrl PARENT &cForm WIDTH nW HEIGHT nH VALUE M->cPubWarn ;
        FONT "Arial Black" SIZE 24 FONTCOLOR SILVER BACKCOLOR aBack
      DoMethod(cForm, cCtrl, 'ReDraw')
   NEXT
   DO EVENTS

   hProcessOpenHandle := 555
   // запуск программы с ожиданием / start program with waiting
   wApi_ShellExecute_Wait(, 'open', EXE_NOTEPAD, PRG_NOTEPAD,, SW_SHOWNORMAL)
   hProcessOpenHandle := 0

   // удалим созданные объекты
   FOR nI := 1 TO LEN(aObDel)
      cForm := aObDel[nI,1]
      cCtrl := aObDel[nI,2]
      IF _IsWindowDefined( cForm ) 
        IF GetControlIndex(cCtrl, cForm ) > 0
           DoMethod(cForm, cCtrl, 'Release')
        ENDIF
      ENDIF
   NEXT

   FOR nI := 1 TO LEN(aFrm)
      cForm := aFrm[nI]
      IF _IsWindowDefined( cForm ) 
         SetProperty( cForm, "BACKCOLOR", aBCls[nI] )  // восстанавливаем цвет формы
      ENDIF
   NEXT

   FOR nI := 1 TO LEN(aFreeze)
      // разблокируем все объекты на формах
      cForm := aFreeze[nI,1]
      cCtrl := aFreeze[nI,2]
      cType := aFreeze[nI,3]
      IF _IsWindowDefined( cForm ) 
         IF cType == "TBROWSE"
            // разблокировка таблицы с закраской
            GetProperty( cForm, cCtrl, "Object" ):Show()
         ELSE
            SetProperty(cForm, cCtrl, "Enabled", .T.)
            SetProperty(cForm, cCtrl, "Visible", .T.)
         ENDIF
      ENDIF
   NEXT

   // заново открываем иконки на формах / reopen icons on forms
   IF hb_IsArray(M->aPubDrawIcon) .and. Len(M->aPubDrawIcon) > 0
      ? "=== " + ProcNL(), "M->aPubDrawIcon =", M->aPubDrawIcon
      FOR EACH aDim IN M->aPubDrawIcon
         cForm := aDim[1]
         hIcon := LoadIconByName( aDim[2], aDim[6], aDim[7] )
         DRAW ICON IN WINDOW &cForm AT aDim[4], aDim[5] HICON hIcon WIDTH aDim[6] HEIGHT aDim[7] COLOR aDim[8]
         aDim[3] := hIcon   // записываем новый хендл иконки
         ? hb_enumIndex(aDim), hb_valtoexp(aDim)
      NEXT
   ENDIF

   DO EVENTS
   ? ProcNL(), "End function"

RETURN NIL

///////////////////////////////////////////////////////////////////////
// Изменение прозрачности окон / Changing window transparency
// Создать своё окно поверх существующего / Create your own window on top of an existing one
FUNCTION myTransparence(cForm0)
   LOCAL nI, aForm, aBClr, cForm, cForm2, aRele, nY, nX, nW, nH, cMsg, nTran, hWnd

   ? ProcNL(), "cForm0=", cForm0

   cMsg  := M->cPubWarn  // сообщение на все формы
   aRele := {}
   aForm := HMG_GetForms()
   aBClr := PURPLE
   nTran := 45  // %

   // Set XXXX Color Transparency ON
   FOR nI := 1 TO LEN(aForm)
      cForm  := aForm[nI]
      cForm2 := cForm + "Shadow"
      IF GetProperty( cForm, "Visible" )  // только НЕ скрытые формы
         nY := GetProperty(cForm, "Row"   )
         nX := GetProperty(cForm, "Col"   )
         nW := GetProperty(cForm, "Width" )
         nH := GetProperty(cForm, "Height")

         DEFINE WINDOW &cForm2 AT nY, nX WIDTH nW HEIGHT nH TITLE " " ;
            MODAL NOCAPTION NOSIZE BACKCOLOR aBClr 

            nW   := This.ClientWidth  - 10*2 
            nH   := This.ClientHeight - 10*2 
            hWnd := GetFormHandle(cForm2)

            @ 10, 10 LABEL Label_Del PARENT &cForm2 WIDTH nW HEIGHT nH VALUE cMsg ;
              FONT "Comic Sans MS" SIZE 28 BOLD FONTCOLOR YELLOW BACKCOLOR aBClr

         END WINDOW
         SetLayeredWindowAttributes( hWnd, 0, (255 * nTran) / 100, 0x00000002 )
         // для этой команды необходим #include "hmg.ch"
         // SET WINDOW &cForm2 TRANSPARENT TO (255 * nTran) / 100
         ACTIVATE WINDOW &cForm2 NOWAIT

         AADD( aRele, cForm2 )   
         // отключаем управление окном
         SetProperty( cForm, "SysMenu"  , .F. )

      ENDIF
      ? nI, cForm, cForm2, HB_ValToExp(aBClr), "BackColorTransparent=", (255 * nTran) / 100
   NEXT

   hProcessOpenHandle := 444
   // запуск программы с ожиданием / start program with wait
   wApi_ShellExecute_Wait(, 'open', EXE_NOTEPAD, PRG_NOTEPAD,, SW_SHOWNORMAL)
   hProcessOpenHandle := 0

   // Set XXXX Color Transparency ON - не делаем, а просто удаляем окно
   FOR nI := 1 TO LEN(aRele)
      cForm := aRele[nI]
      IF _IsWindowDefined( cForm ) 
         DoMethod(cForm, 'Release')
      ENDIF
   NEXT

   FOR nI := 1 TO LEN(aForm)
      cForm  := aForm[nI]
      IF _IsWindowDefined( cForm ) 
         // подключаем управление окном
         SetProperty( cForm, "SysMenu"  , .T. )
      ENDIF
   NEXT

   DO EVENTS
   ? ProcNL(), "End function"

RETURN NIL

////////////////////////////////////////////////////////////////////////////////
// AlphaBlend прозрачности окон / AlphaBlend window Transparency
FUNCTION myAlphaBlendTransp(cForm0)
   LOCAL nI, aFrm, cForm, nTra

   ? ProcNL(), "cForm0=", cForm0

   nTra := 180
   aFrm := HMG_GetForms()

   FOR nI := 1 TO LEN(aFrm)
      cForm := aFrm[nI]
      SetProperty( cForm, "AlphaBlendTransparent", nTra )
   NEXT

   hProcessOpenHandle := 333
   // запуск программы с ожиданием / start program with wait
   wApi_ShellExecute_Wait(, 'open', EXE_NOTEPAD, PRG_NOTEPAD,, SW_SHOWNORMAL)
   hProcessOpenHandle := 0

   nTra := 255 

   FOR nI := 1 TO LEN(aFrm)
      cForm := aFrm[nI]
      IF _IsWindowDefined( cForm ) 
         SetProperty( cForm, "AlphaBlendTransparent", nTra )
      ENDIF
   NEXT

   DO EVENTS
   ? ProcNL(), "End function"

RETURN NIL

////////////////////////////////////////////////////////////////////////////////
// мой вариант (наложение прозрачного окна) / my version (transparent window overlay)
FUNCTION myFormsDarken(cForm0)
   LOCAL nI, aForm, cForm, hWnd, cMsg, aColor, aFontClr, cFont, nFSize, nTransparencyLevel

   ? ProcNL(), "cForm0=", cForm0

   aColor   := YELLOW            // Цвет фона 
   nTransparencyLevel := 128     // Уровень прозрачности
   cMsg     := M->cPubWarn       // сообщение на все формы
   aForm    := HMG_GetForms()
   cFont    := "Comic Sans MS" 
   nFSize   := 28
   aFontClr := BLACK
   ? "Color Back =" , aColor, " Transparency level=", nTransparencyLevel

   FOR nI := 1 TO LEN(aForm)
      cForm := aForm[nI]
      hWnd  := GetFormHandle(cForm)
      //  -> demo_misc.prg  
      // старый синтаксис вызова
      //OverlayCreate( hWnd, aColor[1], aColor[2], aColor[3], nTransparencyLevel )

      // новый синтаксис вызова
      OverlayCreate( hWnd, aColor[1], aColor[2], aColor[3], nTransparencyLevel,;
                           cMsg, cFont, nFSize, aFontClr[1], aFontClr[2], aFontClr[3] )

      // отключаем управление окном
      SetProperty( cForm, "SysMenu", .F. )
   NEXT

   hProcessOpenHandle := 111
   // запуск программы с ожиданием / start program with wait
   wApi_ShellExecute_Wait(, 'open', EXE_NOTEPAD, PRG_NOTEPAD,, SW_SHOWNORMAL)
   hProcessOpenHandle := 0

   FOR nI := 1 TO LEN(aForm)
      cForm := aForm[nI]
      IF _IsWindowDefined(cForm) 
         hWnd := GetFormHandle(cForm)
         OverlayClose( hWnd )    //  -> demo_misc.prg
         // включаем управление окном
         SetProperty( cForm, "SysMenu", .T. )
      ENDIF
   NEXT

   DO EVENTS
   ? ProcNL(), "End function"

RETURN NIL
