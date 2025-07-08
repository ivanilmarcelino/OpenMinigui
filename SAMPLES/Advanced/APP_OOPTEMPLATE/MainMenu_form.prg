/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * (c) 2013-2023 Verchenko Andrey <verchenkoag@gmail.com>
 * (c) 2023 Sergej Kiselev <bilance@bilance.lv>
*/

#define _HMG_OUTLOG

#include "minigui.ch"
#include "metrocolor.ch"
#include "i_winuser.ch"

/////////////////////////////////////////////////////////////////////////
// Функция главной формы-меню задачи
// Function of the main task menu form
FUNCTION Form_Menu_Main(oMainWnd)
   LOCAL cTitle, nWForm, nHForm, nY, nX, nG, nWBtn, nHBtn, cCaption
   LOCAL aBtnIco1, aBtnIco2, aBtnIco3, aBtnIco4, aBtnIco5, aBtnFClr1
   LOCAL aBtnClr1, aBtnClr2, aBtnClr3, aBtnClr4, aBtnClr5, cDisplay
   LOCAL cFont, nFSize, aLblBClr1, aLblBClr2, aBackColor, aBtnFntClr
   LOCAL nH, nW, hIcon, cForm, cInfo, nBtnBase, nWLst, nXLst, nHBtn2
   LOCAL aBtnIco6, aBtnClr6, aBtnIco7, aBtnClr7, nY2
                                                
   ? ProcNL(), oMainWnd:Name, "Функция главной формы-меню задачи/Function of the main task menu form"

   cForm      := "Forma_Main"
   cTitle     := "STANDARD: " + cForm
   cInfo      := "Пример собранный из нескольких модулей: логотипа программы, проверка и т.д." + CRLF
   cInfo      += "An example assembled from several modules: program logo, verification, etc."
   nFSize     := App.Cargo:nDefFontSize
   cFont      := App.Cargo:cDefFontName
   aBackColor := HMG_n2RGB( GetSysColor(COLOR_BACKGROUND) )     // color of the Desktop
   aLblBClr1  := HMG_n2RGB( GetSysColor(COLOR_MENU) )           // color of the Menu background
   aLblBClr2  := HMG_ColorWinActiveCaption()                    // color of the Active window caption
   aBtnFntClr := WHITE
   nBtnBase   := 10  // смещение для кнопки и события этого окна, т.е. +10
   // высота и размер формы считаем ТОЛЬКО от настроек главного окна
   nWForm     := App.Cargo:aDisplayMode[1]
   nHForm     := App.Cargo:aDisplayMode[2]
   cDisplay   := "  (" + App.Cargo:cDisplayMode + ")"
   //nWForm := IIF( nWForm > 1920, 1920, nWForm )
   //nHForm := nHForm - GetTaskBarHeight()    // высота Панели задач Desktop
   App.Cargo:cMainMenuProg := cForm           // имя окна главного меню программы

   SET MENUSTYLE EXTENDED              // switch menu style to advanced
   SetMenuBitmapHeight( 32 )           // set menu icons size to 32x32

   // для данного окна делаем так
   nWForm := 850   
   nHForm := 540

   DEFINE WINDOW &cForm AT nY,nX WIDTH nWForm HEIGHT nHForm            ;
      TITLE cTitle + cDisplay                                          ;
      WINDOWTYPE STANDARD TOPMOST                                      ;
      NOMAXIMIZE NOSIZE                                                ;
      BACKCOLOR aBackColor                                             ;
      ON GOTFOCUS {|| App.Cargo:cFormGotFocus := This.Name }           ;
      ON INIT     {|| This.Topmost := .F., _wPost(0) }                 ;
      ON INTERACTIVECLOSE {|| _wSend(99,App.Cargo:oWinMain), .F. }     ; // close window by [x] ! делать обязательно возврат логический
      ON RELEASE  {|| AEval({90,91,92}, {|n| _wSend(n), DoEvents()}) }   // executed before destroying the window
      //                     ^^^^^^^^ этот код сработает один раз при завершении работы этого окна,
      //                                т.е. когда следующуе событие вернет .T. - окно можно закрыть
      //                                это будет пред-пред последним кодом выполнения
      // Внимание ! _wSend(99,App.Cargo:oWinMain) - посылка сообщения для окна MAIN - wMainInit

      nW := This.ClientWidth       // width inside window
      nH := This.ClientHeight      // height inside the window

      This.Cargo := oHmgData()
      This.Cargo:aBClr    := aBackColor       // в контейнер запоминаем основной процесс окна
      This.Cargo:nBtnBase := nBtnBase         // смещение для кнопки и события этого окна
      This.Cargo:oWinMenu := This.Object      // объект окна это как в событии {|ow|...}
      This.Cargo:cWinMenu := This.Name        // имя окна, просто для примера
      This.Cargo:hWinMenu := This.Handle      // handle окна, просто для примера
      This.Cargo:cForm    := cForm            
      App.Cargo:oMainMenuProg := This.Object  // объект окна главного меню программы

      myTopMainMenu(This.Cargo,oMainWnd)     // верхнее меню      

      @ 5,0 LABEL Label_1 OF &cForm VALUE cInfo WIDTH nWForm HEIGHT 60 ;
        SIZE 12 FONTCOLOR WHITE TRANSPARENT CENTERALIGN

      nWLst := 220
      nXLst := ( nW / 2 - nWLst ) / 2
      @ 80, nXLst LISTBOX List_1 OF &cForm WIDTH nWLst HEIGHT 230 ;
        ITEMS M->aPubListFrom VALUE 1 NOTABSTOP                   ;
        FONT "Tahona" SIZE 11 BACKCOLOR {82,242,210}              ;
        ON CHANGE NIL ON DBLCLICK NIL

      nXLst := nW / 2 + nWLst / 2
      @ 80, nXLst LISTBOX List_2 OF &cForm WIDTH nWLst HEIGHT 230 ;
        ITEMS M->aPubListTo VALUE 1     NOTABSTOP                 ;
        FONT "Tahona" SIZE 11 BACKCOLOR {82,242,210}              ;
        ON CHANGE NIL ON DBLCLICK NIL

      nWBtn := (nW - 64) / 2
      hIcon := LoadIconByName( "1MG", 64, 64 )
      DRAW ICON IN WINDOW &cForm AT 60, nWBtn HICON hIcon WIDTH 64 HEIGHT 64 COLOR aBackColor

      nWBtn     := 150
      nG        := ( nW - nWBtn * 5 ) / 6  // между кнопками
      nHBtn     := 72 * 2 + 5
      nY        := 140
      nX        := nW / 2 - nWBtn / 2
      aBtnIco1  := { "iPiople64x1", "iPiople64x2" }
      aBtnClr1  := { YELLOW, BLACK }
      aBtnFClr1 := { RED   , WHITE }
      cCaption  := "Sample-1"
      @ nY, nX BUTTONEX Button_1 OF &cForm WIDTH nWBtn HEIGHT nHBtn ICON aBtnIco1[1]  ;
        CAPTION cCaption FONTCOLOR aBtnFClr1[1] BACKCOLOR aBtnClr1[1]                 ;
        SIZE 16 BOLD NOXPSTYLE HANDCURSOR NOTABSTOP VERTICAL                          ;
        ON MOUSEHOVER ( This.Icon := aBtnIco1[2], This.Backcolor := aBtnClr1[2], This.Fontcolor := aBtnFClr1[2] ) ;
        ON MOUSELEAVE ( This.Icon := aBtnIco1[1], This.Backcolor := aBtnClr1[1], This.Fontcolor := aBtnFClr1[1] ) ;
        ACTION _wPost(This.Cargo:nBtn+This.Cargo:nBtnBase, ThisWindow.Name, This.Name) ;
        ON INIT {|o| This.Cargo := oHmgData(), o := This.Cargo ,;
                     o:cBtn  := This.Name      ,;
                     o:cCap  := This.Caption   ,;
                     o:aBClr := This.BackColor ,;
                     o:nY    := This.Row       ,;
                     o:nX    := This.Col       ,;
                     o:nBtn  := 1 }
        This.Button_1.Cargo:nBtnBase := nBtnBase

      nY := 330
      nX := nG
      aBtnIco2 := { "iMess64x1", "iMess64x2"  }
      aBtnClr2 := { COLOR_ORANGE_METRO, BLACK }
      cCaption := "Sample-2"
      @ nY, nX BUTTONEX Button_2 OF &cForm WIDTH nWBtn HEIGHT nHBtn ICON aBtnIco2[1]   ;
        CAPTION cCaption FONTCOLOR aBtnFntClr BACKCOLOR aBtnClr2[1]                    ;
        SIZE 16 BOLD NOXPSTYLE HANDCURSOR NOTABSTOP VERTICAL                           ;
        ON MOUSEHOVER ( This.Icon := aBtnIco2[2], This.Backcolor := aBtnClr2[2] )      ;
        ON MOUSELEAVE ( This.Icon := aBtnIco2[1], This.Backcolor := aBtnClr2[1] )      ;
        ACTION _wPost(This.Cargo:nBtn+This.Cargo:nBtnBase, ThisWindow.Name, This.Name) ;
        ON INIT {|o| This.Cargo := oHmgData(), o := This.Cargo ,;
                     o:cBtn  := This.Name      ,;
                     o:cCap  := This.Caption   ,;
                     o:aBClr := This.BackColor ,;
                     o:nY    := This.Row       ,;
                     o:nX    := This.Col       ,;
                     o:nBtn  := 2 }
        This.Button_2.Cargo:nBtnBase := nBtnBase

      nX += This.Button_2.Width + nG

      aBtnIco3 := { "iMusic64x1", "iMusic64x2" }
      aBtnClr3 := { {30,213,56}, BLACK  }
      cCaption := "Sample-3"
      @ nY, nX BUTTONEX Button_3 OF &cForm WIDTH nWBtn HEIGHT nHBtn ICON aBtnIco3[1]   ;
        CAPTION cCaption FONTCOLOR aBtnFntClr BACKCOLOR aBtnClr3[1]                    ;
        SIZE 16 BOLD NOXPSTYLE HANDCURSOR NOTABSTOP VERTICAL                           ;
        ON MOUSEHOVER ( This.Icon := aBtnIco3[2], This.Backcolor := aBtnClr3[2] )      ;
        ON MOUSELEAVE ( This.Icon := aBtnIco3[1], This.Backcolor := aBtnClr3[1] )      ;
        ACTION _wPost(This.Cargo:nBtn+This.Cargo:nBtnBase, ThisWindow.Name, This.Name) ;
        ON INIT {|o| This.Cargo := oHmgData(), o := This.Cargo ,;
                     o:cBtn  := This.Name      ,;
                     o:cCap  := This.Caption   ,;
                     o:aBClr := This.BackColor ,;
                     o:nY    := This.Row       ,;
                     o:nX    := This.Col       ,;
                     o:nBtn  := 3 }
        This.Button_3.Cargo:nBtnBase := nBtnBase

      nX += This.Button_3.Width + nG

      aBtnIco4 := { "iAbout64x1", "iAbout64x2" }
      aBtnClr4 := { COLOR_BLUE_SKYPE, BLACK  }
      cCaption := "Sample-4"
      @ nY, nX BUTTONEX Button_4 OF &cForm WIDTH nWBtn HEIGHT nHBtn ICON aBtnIco4[1]   ;
        CAPTION cCaption FONTCOLOR aBtnFntClr BACKCOLOR aBtnClr4[1]                    ;
        SIZE 16 BOLD NOXPSTYLE HANDCURSOR NOTABSTOP VERTICAL                           ;
        ON MOUSEHOVER ( This.Icon := aBtnIco4[2], This.Backcolor := aBtnClr4[2] )      ;
        ON MOUSELEAVE ( This.Icon := aBtnIco4[1], This.Backcolor := aBtnClr4[1] )      ;
        ACTION _wPost(This.Cargo:nBtn+This.Cargo:nBtnBase, ThisWindow.Name, This.Name) ;
        ON INIT {|o| This.Cargo := oHmgData(), o := This.Cargo ,;
                     o:cBtn  := This.Name      ,;
                     o:cCap  := This.Caption   ,;
                     o:aBClr := This.BackColor ,;
                     o:nY    := This.Row       ,;
                     o:nX    := This.Col       ,;
                     o:nBtn  := 4 }
        This.Button_4.Cargo:nBtnBase := nBtnBase

      nX += This.Button_4.Width + nG

      //nHBtn  := 72 * 2 + nG
      nHBtn2   := 72 
      aBtnIco5 := { "iSanta64x1", "iSanta64x2" }
      aBtnClr5 := { COLOR_PURPLE_METRO, BLACK  }
      cCaption := "S-5"
      @ nY, nX BUTTONEX Button_5 OF &cForm WIDTH nWBtn HEIGHT nHBtn2 ICON aBtnIco5[1]  ;
        CAPTION cCaption FONTCOLOR aBtnFntClr BACKCOLOR aBtnClr5[1]                    ;
        SIZE 16 BOLD NOXPSTYLE HANDCURSOR NOTABSTOP                                    ;
        ON MOUSEHOVER ( This.Icon := aBtnIco5[2], This.Backcolor := aBtnClr5[2] )      ;
        ON MOUSELEAVE ( This.Icon := aBtnIco5[1], This.Backcolor := aBtnClr5[1] )      ;
        ACTION _wPost(This.Cargo:nBtn+This.Cargo:nBtnBase, ThisWindow.Name, This.Name) ;
        ON INIT {|o| This.Cargo := oHmgData(), o := This.Cargo ,;
                     o:cBtn  := This.Name      ,;
                     o:cCap  := This.Caption   ,;
                     o:aBClr := This.BackColor ,;
                     o:nY    := This.Row       ,;
                     o:nX    := This.Col       ,;
                     o:nBtn  := 5 }
        This.Button_5.Cargo:nBtnBase := nBtnBase

      aBtnIco6 := { "iHP64x1", "iHP64x2" }
      aBtnClr6 := { ORANGE   , BLACK     }
      cCaption := "S-6"
      nY2      := nY + This.Button_5.Height + 5
      @ nY2, nX BUTTONEX Button_6 OF &cForm WIDTH nWBtn HEIGHT nHBtn2 ICON aBtnIco6[1] ;
        CAPTION cCaption FONTCOLOR aBtnFntClr BACKCOLOR aBtnClr6[1]                    ;
        SIZE 16 BOLD NOXPSTYLE HANDCURSOR NOTABSTOP                                    ;
        ON MOUSEHOVER ( This.Icon := aBtnIco6[2], This.Backcolor := aBtnClr6[2] )      ;
        ON MOUSELEAVE ( This.Icon := aBtnIco6[1], This.Backcolor := aBtnClr6[1] )      ;
        ACTION _wPost(This.Cargo:nBtn+This.Cargo:nBtnBase, ThisWindow.Name, This.Name) ;
        ON INIT {|o| This.Cargo := oHmgData(), o := This.Cargo ,;
                     o:cBtn  := This.Name      ,;
                     o:cCap  := This.Caption   ,;
                     o:aBClr := This.BackColor ,;
                     o:nY    := This.Row       ,;
                     o:nX    := This.Col       ,;
                     o:nBtn  := 6 }
        This.Button_6.Cargo:nBtnBase := nBtnBase

      nX += This.Button_5.Width + nG

      aBtnIco7 := { "iExitDr64x1","iExitDr64x2" }
      aBtnClr7 := { COLOR_RED_METRO, BLACK      }
      cCaption := "Exit"
      @ nY, nX BUTTONEX Button_7 OF &cForm WIDTH nWBtn HEIGHT nHBtn ICON aBtnIco7[1]   ;
        CAPTION cCaption FONTCOLOR aBtnFntClr BACKCOLOR aBtnClr7[1]                    ;
        SIZE 16 BOLD NOXPSTYLE HANDCURSOR NOTABSTOP VERTICAL                           ;
        ON MOUSEHOVER ( This.Icon := aBtnIco7[2], This.Backcolor := aBtnClr7[2] )      ;
        ON MOUSELEAVE ( This.Icon := aBtnIco7[1], This.Backcolor := aBtnClr7[1] )      ;
        ACTION _wPost(This.Cargo:nBtn+This.Cargo:nBtnBase, ThisWindow.Name, This.Name) ;
        ON INIT {|o| This.Cargo := oHmgData(), o := This.Cargo ,;
                    o:cBtn  := This.Name      ,;
                    o:cCap  := This.Caption   ,;
                    o:aBClr := This.BackColor ,;
                    o:nY    := This.Row       ,;
                    o:nX    := This.Col       ,;
                    o:nBtn  := 7 }
        This.Button_7.Cargo:nBtnBase := nBtnBase

      nY += This.Button_4.Height + nG

      nG  := GetMenuBarHeight()
      @ nY, 0 LABEL Label_Bottom3 OF &cForm VALUE "..." WIDTH nWForm HEIGHT nG ;
        SIZE nFSize - 3 FONTCOLOR WHITE TRANSPARENT VCENTERALIGN
      nY += nG

      @ nY, 0 LABEL Label_Bottom2 OF &cForm VALUE ".." WIDTH nWForm HEIGHT nG ;
        SIZE nFSize - 3 FONTCOLOR RED BACKCOLOR aLblBClr1 VCENTERALIGN
      nY += nG

      nG := GetTitleHeight()
      @ nY, 0 LABEL Label_Bottom1 OF &cForm VALUE "." WIDTH nWForm HEIGHT nG ;
        SIZE nFSize - 2 FONTCOLOR YELLOW BACKCOLOR aLblBClr2 VCENTERALIGN
      nY += IIF( IsMSVC(), nG / 2 - 4, 0 )  // IsMSVC() -> util_misc.prg

      App.Cargo:aTimerLabel := { "Label_Bottom1" , "Label_Bottom2", "Label_Bottom3" }  // запомним

      // установить внешнюю высоту окна, уменьшить её, для красоты
      // set the outer height of the window, reduce it, for beauty
      This.Height := nY + GetBorderHeight() + GetMenuBarHeight()*2 + GetTitleHeight() - 2

      (This.Object):Event(  0, {|ow| // старт ON INIT
                                     Local nY := nX := 0
                                     // считать координаты окна из ини-файла
                                     IniReadPosWinThis(ow, @nY, @nX)
                                     IF nY == 0 .AND. nX == 0
                                        Domethod(ow:Name, "Center")
                                     ELSE
                                       ow:Row := nY
                                       ow:Col := nX
                                     ENDIF
                                     ow:Setfocus('Label_1')
                                     Return Nil
                                 } )

      (This.Object):Event(  1, {|ow| SendMessage( ow:Handle, WM_PAINT, 0, 0 ) ,;    // Show form ICO 
                                     ow:Setfocus('Label_1')                } )  
                                                                          
      // назначаем обработку событий по кнопкам / assign event handling to buttons
      (This.Object):Event( 11, {|ow,ky,cn| my_Btn(ky,cn,ow), _wPost(1,ow)  } )      // запуск события 1
      (This.Object):Event( 12, {|ow,ky,cn| my_Btn(ky,cn,ow), _wPost(1,ow)  } )
      (This.Object):Event( 13, {|ow,ky,cn| my_Btn(ky,cn,ow), _wPost(1,ow)  } )
      (This.Object):Event( 14, {|ow,ky,cn| my_Btn(ky,cn,ow), _wPost(1,ow)  } )
      (This.Object):Event( 15, {|ow,ky,cn| my_Btn(ky,cn,ow), _wPost(1,ow)  } )
      (This.Object):Event( 16, {|ow,ky,cn| my_Btn(ky,cn,ow), _wPost(1,ow)  } )
      (This.Object):Event( 17, {|ow,ky,cn| _wSend(99,App.Cargo:oWinMain), ow:Setfocus('Label_1'), ky:=cn /* Exit */ } )
      // назначаем обработку событий для Sets_aMenuItems(lCrt)
      (This.Object):Event( 20, {|ow,ky,ap|
                               Local cn := ap[1], aRet
                               Local ct := ap[2], cCap
                               Local nn := Val(cn), oCar
                               cCap := This.&(cn).Caption
                               oCar := This.&(cn).Cargo
                               ? "ow:Event = 2: Item name =", cn, "Caption =", cCap
                               _o2log(oCar,,"==> This."+cn+".Cargo", .T.)
                               ? "-", oCar:nPosM,"aItem =", hb_valtoexp(ow:Cargo:aMenuItems[ oCar:nPosM ])
                               aRet := TestAchoice(nn,ct)
                               // можно обрабатывать aRet в др. событиях
                               ow:Cargo:aRezult := aRet // для других событий окна
                               IF( ky := Empty(aRet) ) ; _wPost(23,,ct)
                               ELSE                    ; _wPost(24,,ct)
                               ENDIF
                               Return Nil
                               })
      (This.Object):Event( 23, {|ow,ky,ct|
                               Local aRet := ow:Cargo:aRezult
                               Local cMsg := "aRet= "
                               cMsg += ct + CRLF + HB_ValToExp(aRet) + ";;"
                               cMsg += "NO - SELECTED MODE !; ky= " 
                               cMsg += HB_NtoS(ky) + ";;" + ProcNL()
                               AlertInfo( cMsg, "Return result" )
                               _wPost(30)
                               ? "============ _wPost(23)", ProcNL(), ow:Name,ky,ct
                               Return Nil
                               })
      (This.Object):Event( 24, {|ow,ky,ct|
                               Local aRet := ow:Cargo:aRezult
                               Local cMsg := "aRet= "
                               cMsg += ct + CRLF + HB_ValToExp(aRet) + ";;"
                               cMsg += "YES - SELECTED MODE !;" 
                               cMsg += HB_NtoS(ky) + ";;" + ProcNL()
                               AlertInfo( cMsg, "Return result" )
                               _wPost(30)
                               ? "============ _wPost(24)", ProcNL(), ow:Name,ky,ct
                               Return Nil
                               })
      (This.Object):Event( 30, {|ow|
                               ow:SetFocus("Label_1")
                               DoEvents()
                               wApi_Sleep(150)
                               ? "============ _wPost(30) VK_MENU", ProcNL(), ow:Name
                               _PushKey( VK_MENU )
                               //_PushKey( VK_RETURN )
                               _PushKey( VK_M    )  // '&M - enu examples'
                               Return Nil
                               })
      // ON RELEASE  90, 91, 92
      (This.Object):Event( 90, {|  | DestroyIcon(hIcon)                                          } )
      (This.Object):Event( 91, {|ow| IniSavePosWinThis(ow, "сохранить координаты окна в ини")    } )
      (This.Object):Event( 92, {|ow| _LogFile(.T., ">>> ON RELEASE WINDOW: "+ow:Name, ProcNL() ) } )
      // закрыть окно
      (This.Object):Event( 99, {|ow| ow:Release() } )

      (This.Object):Event(201, {|ow,ky,cn| // window WM_COPYDATA - enable
                                           Local lVal := .T.
                                           ? ProcNL(), ow:Name, ky, cn
                                           App.Cargo:WM_CD_lShow := lVal
                                           DoMethod(App.Cargo:cCopyData_Wnd, "Show")
                                           DoMethod(App.Cargo:cCopyData_Wnd, "MiniMize")
                                           DoMethod(App.Cargo:cCopyData_Wnd, "Restore")
                                           ShowImageThis("смена картинки в меню")
                                           IniSetWrite("COM","lShow_COPYDATA",lVal)  // Запись в ини
                                           Return Nil
                                 } )

      (This.Object):Event(202, {|ow,ky,cn| // window WM_COPYDATA - disable
                                           Local lVal := .F.
                                           ? ProcNL(), ow:Name, ky, cn
                                           App.Cargo:WM_CD_lShow := lVal
                                           DoMethod(App.Cargo:cCopyData_Wnd, "Hide")
                                           ShowImageThis("смена картинки в меню")
                                           IniSetWrite("COM","lShow_COPYDATA",lVal)  // Запись в ини
                                           Return Nil
                                 } )

      (This.Object):Event(203, {|ow,ky,cn| // File mode cFileLog - enable
                                           Local lVal := .T.
                                           ? ProcNL(), ow:Name, ky, cn
                                           App.Cargo:lFileLog := lVal
                                           ShowImageThis("смена картинки в меню")
                                           IniSetWrite("COM","lFileLog",lVal)  // Запись в ини
                                           Return Nil
                                 } )
      (This.Object):Event(204, {|ow,ky,cn| // File mode cFileLog - disable
                                           Local lVal := .F.
                                           ? ProcNL(), ow:Name, ky, cn
                                           App.Cargo:lFileLog := lVal
                                           ShowImageThis("смена картинки в меню")
                                           IniSetWrite("COM","lFileLog",lVal)  // Запись в ини
                                           Return Nil
                                 } )

      ? SPACE(5) + "     This.Name:", This.Name
      ? SPACE(5) + "   This.Handle:", This.Handle
      ?

   END WINDOW

   // CENTER WINDOW &cForm
   ACTIVATE WINDOW &cForm

RETURN NIL

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTopMainMenu(oCargo, oMainWnd)
   LOCAL cForm, hFont0, hFont1, hFont2, hFont3, hFont4, hFont5
   LOCAL cFileLog, cCDLog, cTest_Demo_Errorc // для создания ошибки

   cForm      := oCargo:cForm         // имя окна главного меню программы
                                      // можно и так  App.Cargo:cMainMenuProg
   cFileLog   := cFileNoPath(App.Cargo:cFileLog)
   cCDLog     := cFileNoPath(App.Cargo:cCopyDataLog)
   // счтитаем для меню фонты
   hFont0     :=  App.Cargo:hFontMnMain
   hFont1     :=  App.Cargo:hFontNormal
   hFont2     :=  App.Cargo:hFontBold
   hFont3     :=  App.Cargo:hFontCSM
   hFont4     :=  App.Cargo:hFontItlc
   hFont5     :=  App.Cargo:hFontFSmart

      // добавить объект на форму / add an object to a form
      DEFINE MAIN MENU OF &cForm
         Popup '&File' FONT hFont0
            Item "List of all program forms"       ACTION {|| PromptGetForms()                } ICON "i_About32" FONT hFont1
            Item "Get Forms All"                   ACTION {|| AlertInfo( oGetForms(.T.), "" ) } ICON "i_About32" FONT hFont1
            Item "Create an error in the program"  ACTION {|| cTest_Demo_Errorc++ }             ICON "i_Stop32"  FONT hFont2
            Separator
            Item 'Exit'  ACTION {|| _wSend(99,App.Cargo:oWinMain) }   ICON "i_Exit32"  FONT hFont5
            // Внимание ! _wSend(99,App.Cargo:oWinMain) - посылка сообщения для окна MAIN - wMainInit
         End Popup
         Popup '&Setting'  FONT hFont0                          // прорисовка иконки сразу на форме
            Item 'Show MAIN window' ACTION {|| oMainWnd:Show(), SendMessage( oMainWnd:Handle, WM_PAINT, 0, 0 ) }  ICON "i_Menu32x1"  FONT hFont1
            Item 'Hide MAIN window' ACTION {|| oMainWnd:Hide() }  ICON "i_Menu32x2"  FONT hFont1
            SEPARATOR
            Item 'Show log WM_COPYDATA' ACTION {|| DoMethod( App.Cargo:cCopyData_Wnd, "Show" ) } ICON "i_Menu32x1"   FONT hFont1
            Item 'Hide log WM_COPYDATA' ACTION {|| DoMethod( App.Cargo:cCopyData_Wnd, "Hide" ) } ICON "i_Menu32x2"   FONT hFont1
            Popup 'Show window WM_COPYDATA' NAME SetImage1 IMAGE IIF(App.Cargo:WM_CD_lShow, "bRecOn32", "bRecOff32") FONT hFont3 // Level 2
               Item 'window WM_COPYDATA - enable ' IMAGE "bRecOn32"  ACTION _wPost(201) FONT hFont1
               Item 'window WM_COPYDATA - disable' IMAGE "bRecOff32" ACTION _wPost(202) FONT hFont1
            End Popup
            SEPARATOR
            ITEM 'delete file: ' + cFileLog + ' - ON/OFF' ;
                 ACTION {|| App.Cargo:lFileLog := ! App.Cargo:lFileLog, ShowImageThis(), IniSetWrite("COM","lFileLog",App.Cargo:lFileLog) } ;
                 NAME SetImage2 FONT hFont1 ICON IIF(App.Cargo:lFileLog, "i_FDel32", "i_FDelNo32" )
            SEPARATOR
            Popup 'File mode ' + cFileLog NAME SetImage4 IMAGE IIF(App.Cargo:lFileLog, "bFDel32", "bFDelNo32") FONT hFont3 // Level 2
               Item 'Delete file: ' + cFileLog + ' - ON'  ICON "i_FDel32"   ACTION _wPost(203) FONT hFont1
               Item 'Delete file: ' + cFileLog + ' - OFF' ICON "i_FDelNo32" ACTION _wPost(204) FONT hFont1
            End Popup
            SEPARATOR
            ITEM 'delete file: ' + cCDLog + ' - ON/OFF' ;
                 ACTION {|| App.Cargo:lCopyDataLog := ! App.Cargo:lCopyDataLog, ShowImageThis(), IniSetWrite("COM","lCopyDataLog",App.Cargo:lCopyDataLog) } ;
                 NAME SetImage3 FONT hFont1 ICON IIF(App.Cargo:lCopyDataLog, "i_FDel32", "i_FDelNo32" )
            SEPARATOR
            Item 'Info:' ACTION {|| MsgDebug("App.Cargo:lFileLog=",App.Cargo:lFileLog,"App.Cargo:lCopyDataLog=",App.Cargo:lCopyDataLog) }  FONT hFont1
         End Popup
         Popup '&About'  FONT hFont0
            Item 'About the program' Action MsgAbout()           ICON "i_Smile32"
            SEPARATOR
            Item 'About TsbViewer()' Action MsgAbout_TsbViewer() ICON "i_Smile32"
         End Popup
         Popup '&Menu examples'  FONT hFont0
            Sets_aMenuItems(.T.)            
         End Popup

      END MENU

RETURN NIL

///////////////////////////////////////////////////////////////////
FUNCTION ShowImageThis()   // смена картинки в меню
   LOCAL cForm, lShow, lCDLg, lFile, oWnd := ThisWindow.Object

   cForm := oWnd:Name
   lShow := App.Cargo:WM_CD_lShow
   lFile := App.Cargo:lFileLog
   lCDLg := App.Cargo:lCopyDataLog
   // для иконок
   _SetMenuItemIcon  ( "SetImage2" , cForm, if( lFile, "i_FDel32", "i_FDelNo32" ) )
   _SetMenuItemIcon  ( "SetImage3" , cForm, if( lCDLg, "i_FDel32", "i_FDelNo32" ) )
   // для .bmp
   _SetMenuItemBitmap( "SetImage1", cForm, if( lShow, "bRecOn32" , "bRecOff32"  ) )
   _SetMenuItemBitmap( "SetImage4", cForm, if( lFile, "bFDel32"  , "bFDelNo32"  ) )

RETURN NIL

////////////////////////////////////////////////////////////
// считать координаты окна из ини-файла
/// read window coordinates from the ini file
STATIC FUNCTION IniReadPosWinThis(oWnd,nY,nX)
   LOCAL cSection, oIni, oSec, cForm
   DEFAULT nY := nX := 0

   oIni     := App.Cargo:oIni   // считаем ini-файл из глобальной App.Cargo
   cForm    := oWnd:Name
   cSection := cForm + "/Настройки"
   IF Empty( oSec := oIni:Get(cSection) )
      nY := nX := 0
      // Запись переменной в ини файл
      IniSetWrite(cSection,"Pos_Y" , nY)
      IniSetWrite(cSection,"Pos_X" , nX)
      IniSetWrite(cSection,"PosRem", "Координаты окна")
      IniWriteParam()   // Запись всего ини файла
   ENDIF

   nY := GetIniData( oIni, cSection, "Pos_Y", 0 )
   nX := GetIniData( oIni, cSection, "Pos_X", 0 )
   nY := IIF( nY < 0, 0, nY )
   nX := IIF( nX < 0, 0, nX )

RETURN NIL

////////////////////////////////////////////////////////////
// сохранить координаты окна в ини-файле
// save window coordinates in an ini file
STATIC FUNCTION IniSavePosWinThis(oWnd)
   LOCAL cSection, cForm, nY, nX
   LOCAL tTime := HB_DATETIME()

   cForm    := oWnd:Name
   cSection := cForm + "/Настройки"

   nY := GetProperty(cForm, "Row" )
   nX := GetProperty(cForm, "Col" )
   nY := IIF( nY < 0, 0, nY )
   nX := IIF( nX < 0, 0, nX )

   IniSetWrite(cSection,"Pos_Y", nY)
   IniSetWrite(cSection,"Pos_X", nX)
   IniSetWrite(cSection,"PosRem", "Координаты окна/правка " + HB_TTOC( tTime ))
   IniWriteParam()   // Запись всего ини файла

RETURN NIL

/////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION my_Btn( nEvnt, cButt, oWnd )
   LOCAL cBtn, cWnd, o, cForm, y, x, cAls, cTitle, nBtn, cRun, cParam, cMsg
                                                         
   cWnd  := oWnd:Name
   nBtn  := nEvnt - oWnd:Cargo:nBtnBase  // 10 смещение для кнопки и события этого окна
   cBtn  := "Button_" + hb_ntos( nBtn )  // Button_1, Button_2, ...
   cForm := "w"+cBtn+"_Standart"
   ? ProcNL(), nEvnt, nBtn, cButt, cWnd, cBtn != cButt
   SetProperty(cWnd, cButt, "Enabled", .F. )  // блокировать кнопку

   IF cBtn != cButt
      ? ProcNL(), "ERROR", cBtn, "!=", cButt
      RETURN .F.
   ENDIF

   SET WINDOW THIS TO cWnd

   o      := This.(cBtn).Cargo
   cAls   := "CUST_"+hb_ntos(o:nBtn)
   cTitle := cBtn+": "+hb_ntos(o:nBtn)+" "+o:cCap
   y      := o:nY - 100
   x      := o:nX +  50

   ? ProcNL(), nEvnt, cBtn, o:cBtn, o:nBtn, o:cCap, cForm, cTitle

   IF nBtn == 4 //  => nEvnt == 114
      cTitle := "WINDOWTYPE STANDARD: " + cTitle
      my_Standard4( cForm, o:nBtn, cTitle, o:aBClr, y, x, o:nW, o:nH, cAls, cWnd) // -> table4.prg
   ELSEIF nBtn == 3 //  => nEvnt == 113
      my_Standard3( cForm, o:nBtn, cAls, cWnd)                                    // -> table3.prg
   ELSEIF nBtn == 5 .OR. nBtn == 6 //  => nEvnt == 115/116
      cRun   := M->cPubMainFolder + "Run_sample4.bat"
      cParam := ''
      IF !FILE(cRun)
         cMsg := "No file to run !;;" + cRun 
         cMsg += ";;Program: " + App.Cargo:cAppTitle + ";STOPPED"
         AlertStop( cMsg, "Error", "ZZZ_B_STOP64", 64 )
      ELSE
         ShellExecute( , 'open', cRun, cParam, , SW_SHOWNORMAL)
      ENDIF
      wApi_Sleep(100)
   ELSE
      my_Standard( cForm, o:nBtn, cTitle, o:aBClr, y, x, o:nW, o:nH, cAls )       // -> table12.prg
   ENDIF

   SET WINDOW THIS TO

   SetProperty(cWnd, cButt, "Enabled", .T. )  // разблокировать кнопку

RETURN .T.

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Sets_aMenuItems(lCrt)
   LOCAL a, o, aa, cn
   DEFAULT lCrt := .F.

     // Name       Item                                   Icon        Font
   a := {   ;
         { 1, '&1. Menu without icon (default)'         , "iSanta1" , "Bold"    }, ;
         { 2, '&2. Menu without icon (with parameters)' , "iSanta1" , "Normal"  }, ;
         { 3, '&3. Menu with icons (icon on the left)'  , "iSanta1" , "Bold"    }, ;
         { 4, '&4. Menu with icons (icon on the right)' , "iSanta1" , "Normal"  }, ;
         { 5, '&5. Menu with icons (icon on the top)'   , "iSanta1" , "Bold"    }, ;
         { 6, '&6. Menu with icons (icon on the bottom)', "iSanta1" , "Normal"  }, ;
         { 7, '&7. Menu with icon + gradient '          , "iFolder2", "DlgFont" }, ;
         { 8, '&8. Menu with icon + gradient HORIZONTAL', "iFolder2", "DlgFont" }, ;
         {  ,                                           ,           ,           }, ;
         { 9, '&9. Large menu without icons'            , "iSanta2" , "Italic"  }, ;
         {10, '&'+'A. Large menu with icons + gradient' , "iSanta2" , "Italic"  }, ;
         {  ,                                           ,           ,           }, ;
         {11, '&'+'B. Menu by array (default)'          , "iHp1"    , "DlgFont" }, ;
         {12, '&'+'C. Array menu with icon on the left of the form'                 , "iHp1", "ComSnMs" }, ;
         {13, '&'+'D. Array menu with icon on the right side of the form + gradient', "iHp1", "ComSnMs" }, ;
         {  ,                                           ,           ,           }, ;
         {14, '&'+'E. Days of the week menu (align text left) '  , "iHp1"    , "DlgFont" }, ;
         {15, '&'+'E. Days of the week menu (align text right)'  , "iHp1"    , "DlgFont" }  ;
        }

   IF lCrt
      This.Object:Cargo:aMenuItems := a     // в Cargo окна запомнили / in Cargo windows remembered
      FOR EACH aa IN a
          IF Empty(aa) .or. Empty(aa[1])
             SEPARATOR
          ELSEIF aa[1] == 99
             ITEM aa[2] ACTION _wPost(Val(This.Name),,This.Caption) ;
                          NAME &( StrZero(aa[1], 4) ) ICON aa[3] FONT aa[4]
          ELSE
             cn := StrZero(aa[1], 4)
             ITEM aa[2] ACTION {|| _wPost(20,,{This.Name, This.Caption}) } ;
                          NAME &(cn) ICON aa[3] FONT aa[4]
             This.&(cn).Cargo := oHmgData()
             o := This.&(cn).Cargo
             o:nPosM := hb_enumindex(aa)
             o:nItem := aa[1]
             o:cText := aa[2]
             o:cIcon := aa[3]
             o:cFont := aa[4]
             o:oWnd  := This.Object
          ENDIF
      NEXT
   ENDIF

RETURN a

