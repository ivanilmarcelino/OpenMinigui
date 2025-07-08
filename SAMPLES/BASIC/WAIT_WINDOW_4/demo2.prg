/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2023-24 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2023-24 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Создание правильного многооконного интерфейса для МиниГуи
 * Creating the correct multi-window interface for MiniGui
 * Работа с окнами, 2 варианта работы: блокировать/ не блокировать кнопки на форме
 * Working with windows, 2 options: block/not block buttons on the form
 * Работа с AVI-объектом без потока / Working with an AVI object no stream
 * Анимация на кнопке, кнопка через LABEL / Animation on a button, button via LABEL
*/
ANNOUNCE RDDSYS
#define _HMG_OUTLOG
#include "hmg.ch"
#include "i_winuser.ch"

#define PROGRAM   "Working with a AVI object no Thread"
#define PROGVER   'Version 0.2 (18.01.2024)'
/////////////////////////////////////////////////////////////////
FUNCTION Main(...)
   LOCAL i, j, k, y, x, w, h, p, g, aFClrChk, nFntChk
   LOCAL aFntClr, aBtnBClr, nHBtn, aBtn, cSay

   y := x := 20 ; nHBtn := 40 ; g := 20

   aFClrChk := BLUE //WHITE
   nFntChk  := App.Cargo:nFDlg - 3
   aFntClr  := { WHITE , BLACK  }
   aBtnBClr := { GRAY  , YELLOW }
   aBtn     := wMainBtn_Init()

   h := Len(aBtn) * ( nHBtn + g ) + g * 3
   h += ( App.Cargo:nFDlg * 2 + g ) * 2

   DEFINE WINDOW wMain AT y,x WIDTH 500 HEIGHT h ;
      TITLE App.Cargo:cTitle MAIN NOSIZE TOPMOST ;
      BACKCOLOR App.Cargo:aBColor                ;
      FONT App.Cargo:cFDlg SIZE App.Cargo:nFDlg  ;
      ON INIT    {|| _wPost( 0) }                ;
      ON RELEASE {|| _wSend(90) }

      This.Cargo := oHmgData()
      This.Cargo:aBtnAll := aBtn
      This.Cargo:lChk_1  := App.Cargo:lMainHide
      This.Cargo:lChk_2  := App.Cargo:lBtnBlock
      This.Cargo:aCheck  := {"lChk_1", "lChk_2"}

      w := This.ClientWidth
      h := nHBtn

      @ 0,0 Label Buff Value "" AUTOSIZE

      //@ 5,5 BUTTON Btn_A CAPTION "About" WIDTH 120 HEIGHT h ACTION {|| MsgAbout() } - проверка

      FOR i := 1 TO ( k := Len(aBtn) )
          j := "Btn_" + StrZero(i, 2)
          p := aBtn[ i ][1]

         @ y, x BUTTONEX &j WIDTH w - g*2 HEIGHT h CAPTION p     ;
           ICON NIL FLAT NOXPSTYLE HANDCURSOR NOTABSTOP          ;
           BOLD FONTCOLOR aFntClr[1] BACKCOLOR aBtnBClr[1]       ;
           ON MOUSEHOVER ( This.Fontcolor := aFntClr[2], This.Backcolor := aBtnBClr[2] ) ;
           ON MOUSELEAVE ( This.Fontcolor := aFntClr[1], This.Backcolor := aBtnBClr[1] ) ;
           ACTION  {|| This.Enabled := .F., _wPost(This.Cargo:nPost, This.Index) }  ;
           ON INIT {||
                    This.Cargo := oHmgData()
                    This.Cargo:cObj := This.Name
                    This.Cargo:lAction := .F.
                    Return Nil
                    }

          This.&(j).Cargo:nBtn  := i
          This.&(j).Cargo:aBtn  := aBtn[ i ]
          This.&(j).Cargo:nPost := 1
          This.&(j).Cargo:aBClr := aBtnBClr
          //This.&(j).Cargo:oWnd  := This.Object  не надо - лишнее
          // всегда можно получить obtn := This.&(j).Object

          IF i == k
             This.&(j).Caption := "Exit"
             This.&(j).Action  := {|| _wPost(99) }
          ENDIF
          y += h + g
      NEXT

      cSay := "enable hiding this window after button click" + CRLF
      cSay += "включить скрытие этого окна после нажатия кнопки"
      @ y, x CHECKBOX Chk_1 CAPTION cSay VALUE This.Cargo:lChk_1 ;
        WIDTH w-x*2 HEIGHT App.Cargo:nFDlg*2                     ;
        SIZE nFntChk FONTCOLOR aFClrChk TRANSPARENT              ;
        ON CHANGE _wPost("@Check", This.Index)                   ;
        ON INIT {||
                 This.Cargo := oHmgData()
                 This.Cargo:cObj := This.Name
                 This.Cargo:nObj := 1
                 SetStyleML( This.Handle )   // стиль показа объекта MULTILINE
                 Return Nil
                 }
      y += This.Chk_1.Height + g
      // изменить стиль показа объекта на MULTILINE
      //SetStyleML( GetControlHandle('Chk_1','wMain') )  // можно и так

      cSay := "enable button lock" + CRLF
      cSay += "включить блокировку кнопки"
      @ y, x CHECKBOX Chk_2 CAPTION cSay VALUE This.Cargo:lChk_2 ;
        WIDTH w-x*2 HEIGHT App.Cargo:nFDlg*2                     ;
        SIZE nFntChk FONTCOLOR aFClrChk TRANSPARENT              ;
        ON CHANGE _wPost("@Check", This.Index) ;
        ON INIT {||
                 This.Cargo := oHmgData()
                 This.Cargo:cObj := This.Name
                 This.Cargo:nObj := 2
                 SetStyleML( This.Handle )   // стиль показа объекта MULTILINE
                 Return Nil
                 }
      // изменить стиль показа объекта на MULTILINE
      //SetStyleML( GetControlHandle('Chk_2','wMain') ) // можно и так

      WITH OBJECT This.Object
      :Event( 0, {|ow  |
                  This.Topmost := .F.
                  ? ProcNL(), "----- :Event(0)"
                  ?? ow:Name
                  ow:Setfocus("Buff")
                  Return Nil
                  })

      :Event( 1, {|obtn|
                  Local ow := obtn:Window
                  Local o  := obtn:Cargo
                  obtn:Enable()                    // разблокировали button
                  IF o:lAction                     // уже работает ACTION
                     SET WINDOW THIS TO ow:Name
                     myWinTable( obtn:Cargo , ow:Name )  // имя "wMain"
                     SET WINDOW THIS TO
                     RETURN Nil
                  ENDIF
                  o:lAction := .T.        // блокируем 2-ой вход в ACTION
                  ? ProcNL(), "----- :Event(1)"
                  ?? ow:Name, obtn:Name //, obtn:Enabled()

                  IF ow:Cargo:lChk_2 // App.Cargo:lBtnBlock
                     obtn:Disable()
                     ?? "Блокирую кнопку/Block the button"
                  ENDIF

                  _wSend("@Show\Hide", ow, 0)  // hide window

                  ?? ":lBtnBlock=", ow:Cargo:lChk_2, ":lMainHide=", ow:Cargo:lChk_1

                  SET WINDOW THIS TO ow:Name
                  myWinTable( obtn:Cargo , ow:Name )
                  SET WINDOW THIS TO

                  ? ProcNL(), "----- :Event(1)"
                  ?? ow:Name, obtn:Name //, obtn:Enabled()

                  IF ow:Cargo:lChk_2 // App.Cargo:lBtnBlock
                     obtn:Enable()                // разблокировали
                     ?? "Разблокирую кнопку/Unlock the button"
                  ENDIF

                  _wSend("@Restore"  , ow)     // restore window

                  _wSend("@Show\Hide", ow, 1)  // show window

                  o:lAction := .F.             // завершил работу в ACTION
                  ow:Setfocus("Buff")
                  Return Nil
                  })

      :Event({10, "@Show\Hide"}, {|ow,ky,vxod|
                  IF ow:Cargo:lChk_1 // App.Cargo:lMainHide
                     IF Empty(vxod)
                        ow:Hide()
                        ky := "Скрываю окно/Hiding the window"
                     ELSE
                        ow:Show()
                        ky := "Показываю окно/Showing the window"
                     ENDIF
                     ? ow:Name, vxod, ky
                  ENDIF
                  Return Nil
                  })
      :Event({11, "@Restore"}, {|ow,ky,cfocu|
                  Domethod(ow:Name, "Minimize")
                  DO EVENTS
                  Domethod(ow:Name, "Restore" )
                  IF !Empty(cfocu) ; ow:SetFocus(cFocu)
                  ENDIF
                  DO EVENTS
                  ky := cfocu
                  Return Nil
                  })

      :Event({3, "@Check"}, {|ochk,ky|
                  Local ow := ochk:Window
                  Local nchk := ochk:Cargo:nObj
                  ky := ow:Cargo:aCheck[ nchk ]
                  ow:Cargo:&(ky) := ochk:Value
                  //или
                  //ow:Cargo:Set("l"+ochk:Name, ochk:Value)
                  //или
                  //IF upper(ochk:Name) == "CHK_1"
                  //   ow:Cargo:lChk_1 := ochk:Value
                  //ELSE
                  //   ow:Cargo:lChk_2 := ochk:Value
                  //ENDIF
                  ow:Setfocus("Buff")
                  DO EVENTS
                  Return Nil
                  })

      :Event(90, {|ow  | // закрытие окна
                   _wSend(91)
                   ? ProcNL(), "----- :Event(90)"
                   ?? ">>> RELEASE: " + ow:Name
                   DO EVENTS
                   Return Nil
                   })
      :Event(91, {|ow  | // save variable
                   App.Cargo:lMainHide := ow:Cargo:lChk_1
                   App.Cargo:lBtnBlock := ow:Cargo:lChk_2
                   // можно сохранить в ini или файле настройки
                   Return Nil
                   })
      :Event(99, {|ow  | ow:Release })
      END WITH

   END WINDOW

   ACTIVATE WINDOW wMain

RETURN Nil

/////////////////////////////////////////////////////////////////
STATIC FUNCTION wMainBtn_Init()
   Local aBtn

   aBtn := {{"Test-1  (Avi3dMan128.avi)" , "Avi3dMan128",       ,       , , }, ;
            {"Test-2  (ZipAvi.avi)"      , "ZipAvi"     , SILVER, RED   , , }, ;
            {"Test-3  (FindFolder.avi)"  , "FindFolder" , TEAL  , WHITE , , }, ;
            {"Exit"                      ,              ,       ,       , , }  ;
           }

RETURN aBtn

/////////////////////////////////////////////////////////////////
// эта процедура ВСЕГДА стартует ПЕРВОЙ !
// this procedure ALWAYS starts FIRST!
INIT PROCEDURE Sets_ENV()
   LOCAL aBClrDlg := { 141, 179, 226 }
   LOCAL aBColor  := {  86, 152, 251 }
   LOCAL o

   SET LANGUAGE TO RUSSIAN // язык
   SET CODEPAGE TO RUSSIAN // кодовая страница

   SET MULTIPLE QUIT

   //RddSetDefault("DBFCDX")    // резерв

   o := SetsEnv()               // -> demo_util.prg

   fErase( o:cLog )

   ? REPL("=",20) + " Start" , HB_DATETIME() , REPL("=",20)
   ? MG_Version(), MG_Version(.T.), MG_Version(.F.)

   o:cTitle        := PROGRAM
   o:cVersion      := PROGVER
   o:cTitleRu      := "Работа с AVI-объектом без потока"
   o:cAvtor        := "Copyright 2023-24 Verchenko Andrey"
   o:cEmail        := "<verchenkoag@gmail.com> Dmitrov, Moscow region"
   o:cPrgInfo1     := "Many thanks for your help: Grigory Filatov <gfilatov@inbox.ru>"
   o:cPrgInfo2     := "Tips and tricks programmers from our forum http://clipper.borda.ru"
   o:aBColor       := aBColor
   o:lDebug        := .T.    // включить отладку в лог-файл
   o:lMainHide     := .T.    // включить скрытие окна после нажатия
   o:lBtnBlock     := .T.    // включить блокировку кнопки

RETURN

//////////////////////////////////////////////////////////////////////////////////
FUNCTION myWinTable(oBtnCargo,cMainForm)
   LOCAL nW, nH, nG, nX, nRow, nCol, cFormName, cFormTitle, aBackColor, cNWin
   LOCAL cVal, nI, nBtn, cCapt, cResAvi, aFontClr, aBackClr, lDebug, lFcs, cN
   LOCAL cResAvi2, aResWH, aBtnFClr, aBtnBClr, nFSize, tTimeStart, nWBtn, nHBtn
   Local oo

   lDebug     := App.Cargo:lDebug  ; Default lDebug := .F.
   aBackColor := App.Cargo:aBColor
   nW         := System.DesktopWidth
   nH         := System.DesktopHeight - GetTaskBarHeight()   // высота Панели задач Desktop
   nG         := 20
   tTimeStart := hb_DateTime()                               // время начала процесса
   aBtnFClr   := { WHITE , YELLOW, WHITE  }                  // цвет фонта кнопки {MOUSEHOVER,MOUSELEAVE,3-блокировка}
   aBtnBClr   := { LGREEN, BLACK , SILVER }                  // цвет фона кнопки  {MOUSEHOVER,MOUSELEAVE,3-блокировка}
   nWBtn      := 300                                         // ширина кнопки
   nHBtn      := 60                                          // высота кнопки
   nFSize     := 22
   IF lDebug
      ? ProcNL(), "oBtnCargo=", oBtnCargo
      _o2Log(oBtnCargo, 15, "==> .T. oBtnCargo: ", .T.)      // вывод в лог-файл
   ENDIF
   nBtn       :=  oBtnCargo:nBtn                             // номер кнопки
   cCapt      :=  oBtnCargo:aBtn[1]                          // название кнопки
   cResAvi    :=  oBtnCargo:aBtn[2]                          // имя ресурса
   aBackClr   :=  oBtnCargo:aBtn[3]                          // цвет фона формы
   aFontClr   :=  oBtnCargo:aBtn[4]                          // цвет фонта на форме
   cFormName  := "Form_" + HB_NtoS(nBtn)
   cFormTitle := "STANDART: " + cFormName + " - " + cCapt
   cResAvi2   := "FindFolder"

   IF _IsWindowActive( cFormName )
      IF lDebug ;  ? "===> " + ProcNL(), "поднятие ранее созданного окна, SwitchToWin(" + cFormName + ")"
         ?? "Для формы:", cMainForm, "делаем Minimize"
      ENDIF
      //Domethod(cMainForm, "Minimize") // можно так
      SwitchToWin( cFormName )
      SendMessage( GetFormHandle(cFormName) , WM_PAINT, 0, 0 )     // Show form ICO
      DO EVENTS
      RETURN NIL
   ENDIF

   oBtnCargo:cFormName := cFormName

   IF lDebug ; ? ProcNL(), cFormName, "создание окна"
   ENDIF

   // чистка памяти, уборка мусора / memory cleaning, garbage collection
   // это для больших программ / this is for large programs
   DO EVENTS ; hb_gcAll() ; DO EVENTS

   SET FONT TO "Tahoma", nFSize

   /////////////// --------------------- открытие баз, считывание из ини-файла
   lFcs  := .F.   // вернуть фокус на пред.окно
   cNWin := nI := cVal
   cNWin := WaitWinAvi( 'Creating a table, step 1',tTimeStart,lFcs,cResAvi, aBackClr, aFontClr )  // создаём окно ожидания с потоком
   IF Empty(cNWin)
      IF lDebug ;  ? "===[] " + ProcNL(), "Error creating window !"
      ENDIF
      RETURN NIL
   ENDIF
   // эмуляция - цикл вычислений/расчётов/созданий баз и т.д.
   FOR nI := 1 TO 50
      wApi_Sleep( 100 )
      DO EVENTS
      cVal := hb_ntos( nI ) + "/100"
      // _wSend("S_a_y", cNWin, cVal)
      // _wSend(3, cNWin, cVal)   // или так
      // WaitWinAviSay( cVal )    // или так
   NEXT
   WaitWinAvi()   // закрыть окно "ожидания"
   /////////////// --------------------- открытие баз, считывание из ини-файла

   DEFINE WINDOW &cFormName AT 0, 0 WIDTH nW HEIGHT nH TITLE cFormTitle ;
          WINDOWTYPE STANDARD TOPMOST NOMAXIMIZE NOSIZE                 ;
          BACKCOLOR aBackColor          ;
          ON INIT    {|| _wPost( 0) }   ;  // выполняется после инициализации окна
          ON RELEASE {|| _wSend(90) }      // выполняется перед разрушением окна !!! только _wSend()
          This.Cargo := oHmgData()

      nW := This.ClientWidth
      nH := This.ClientHeight

      This.Cargo:hIcon := LoadIconByName( App.Cargo:cIconDef, 64, 64 )
      ? ProcNL(), "hIcon=", This.Cargo:hIcon, App.Cargo:cIconDef
      DRAW ICON IN WINDOW &cFormName AT 10, 10 HICON This.Cargo:hIcon  WIDTH 64 HEIGHT 64 COLOR This.BackColor

      // установим контейнер на окно
      This.Cargo:lClose    := .T.
      This.Cargo:tStart    := tTimeStart       // время начала процесса
      This.Cargo:cBtnTitle := cCapt
      This.Cargo:cResAvi2  := cResAvi2
      This.Cargo:cAviName  := "Avi_1"

      nX := nW - nG - nWBtn
      cN := "Label_Exit"
      @ nG, nX LABEL &cN VALUE "Exit" WIDTH nWBtn HEIGHT nHBtn    ;
            FONTCOLOR aBtnFClr[1] BACKCOLOR aBtnBClr[1]           ;
            CENTERALIGN VCENTERALIGN BORDER CLIENTEDGE            ;
            ON MOUSEHOVER {|| MouseFocus_Label(.T., This.Cargo) } ;
            ON MOUSELEAVE {|| MouseFocus_Label(.F., This.Cargo) } ;
            ON CLICK {| | _wPost( This.Cargo:nPost ) } ;
            ON INIT  {|o|
                       This.Cargo := oHmgData()
                       o := This.Cargo
                       o:oObj    := This.Object
                       o:cObj    := o:oObj:Name
                       o:nObj    := 99
                       o:lAction := .F.
                       o:lHover  := .F.
                       o:lBorder  := .T.
                       o:lClientEdge := .T.
                       Return Nil
                       }
      oo := This.&(cN).Cargo
      oo:nPost      := 99
      oo:nFontHover := nFSize + 4
      oo:nFontLeave := nFSize
      oo:aFontFClr  := aBtnFClr
      oo:aFontBClr  := aBtnBClr
      IF Empty( oo:lBorder )
         ChangeStyle ( oo:oObj:Handle, , WS_BORDER, .F. )
      ENDIF
      IF Empty( oo:lClientEdge )
         ChangeStyle ( oo:oObj:Handle, , WS_EX_CLIENTEDGE, .T. )
      ENDIF

      nX := This.Label_Exit.Col - nG - This.Label_Exit.Width
      cN := "Label_Info"
      @ nG, nX LABEL &cN VALUE "Info win" WIDTH nWBtn HEIGHT nHBtn ;
            FONTCOLOR aBtnFClr[1] BACKCOLOR aBtnBClr[1]            ;
            CENTERALIGN VCENTERALIGN BORDER CLIENTEDGE             ;
            ON MOUSEHOVER {|| MouseFocus_Label(.T., This.Cargo) }  ;
            ON MOUSELEAVE {|| MouseFocus_Label(.F., This.Cargo) }  ;
            ON CLICK {| | _wPost(This.Cargo:nPost, This.Index ) }  ;
            ON INIT  {|o|
                       This.Cargo := oHmgData()
                       o := This.Cargo
                       o:oObj    := This.Object
                       o:cObj    := o:oObj:Name
                       o:nObj    := 1
                       o:cAvi    := ThisWindow.Cargo:cAviName
                       o:lAction := .F.
                       o:lHover  := .F.
                       o:lBorder  := .T.
                       o:lClientEdge := .T.
                       Return Nil
                       }
      oo := This.&(cN).Cargo
      oo:nPost      := "Info" //3  - вызов события строка/число
      oo:nFontHover := nFSize + 4
      oo:nFontLeave := nFSize
      oo:aFontFClr  := aBtnFClr
      oo:aFontBClr  := aBtnBClr
      IF Empty( oo:lBorder )
         ChangeStyle ( oo:oObj:Handle, , WS_BORDER, .F. )
      ENDIF
      IF Empty( oo:lClientEdge )
         ChangeStyle ( oo:oObj:Handle, , WS_EX_CLIENTEDGE, .T. )
      ENDIF

      aResWH := GetAviResSize(cResAvi2)
      nRow   := ( This.Label_Info.Height - aResWH[2] ) / 2 + nG
      nCol   := nX + 5
      // в МиниГуи показ AVI поддерживается кодеком Microsoft RLE Video, современные кодеки не поддерживаются
      // in MiniGui, AVI display is supported by the Microsoft RLE Video codec, modern codecs are not supported
      @ nRow, nCol ANIMATEBOX Avi_1 WIDTH aResWH[1] HEIGHT aResWH[2] File cResAvi2 AUTOPLAY ;
        TRANSPARENT BACKCOLOR aBtnBClr[1] NOBORDER

      nX := This.Label_Info.Col - nG - This.Label_Info.Width/2
      cN := "Label_About"
      @ nG, nX LABEL &cN VALUE "About" WIDTH nWBtn/2 HEIGHT nHBtn ;
            FONTCOLOR aBtnFClr[1] BACKCOLOR aBtnBClr[1]           ;
            CENTERALIGN VCENTERALIGN BORDER CLIENTEDGE            ;
            ON MOUSEHOVER {|| MouseFocus_Label(.T., This.Cargo) } ;
            ON MOUSELEAVE {|| MouseFocus_Label(.F., This.Cargo) } ;
            ON CLICK {| | _wPost(This.Cargo:nPost, This.Index ) } ;
            ON INIT  {|o|
                       This.Cargo := oHmgData()
                       o := This.Cargo
                       o:oObj     := This.Object
                       o:cObj     := o:oObj:Name // This.Name
                       o:nObj     := 1
                       o:lAction  := .F.
                       o:lHover   := .F.
                       o:lBorder  := .F.
                       o:lClientEdge := .F.
                       Return Nil
                       }
      oo := This.&(cN).Cargo
      oo:nPost      := "About" //4  - вызов события строка/число
      oo:nFontHover := nFSize + 4
      oo:nFontLeave := nFSize
      oo:aFontFClr  := aBtnFClr
      oo:aFontBClr  := aBtnBClr
      IF Empty( oo:lBorder )
         ChangeStyle ( oo:oObj:Handle, , WS_BORDER, .F. )
      ENDIF
      IF Empty( oo:lClientEdge )
         ChangeStyle ( oo:oObj:Handle, , WS_EX_CLIENTEDGE, .T. )
      ENDIF
/*
   CHANGESTYLE (hWnd,dwAdd,dwRemove,lExStyle)
   Action: Modifies the basic styles of a window
   Parameters: hWnd - handle to window
               dwAdd - window styles to add
               dwRemove - window styles to remove
               lExStyle - TRUE for Extended style otherwise FALSE
   HMG 1.1 Expermental Build 12a
   (C)Jacek Kubica <kubica@wssk.wroc.pl>
 */

      @ 220, 0 LABEL Label_1 VALUE "Calculation in progress - " + cCapt WIDTH nW HEIGHT 60 ;
         SIZE 26 FONTCOLOR BLACK BOLD TRANSPARENT CENTERALIGN

      // Установка событий на это окно
      WITH OBJECT This.OBJECT
         :Event( 0, {|ow|
                      This.Topmost := .F.
                      _wPost(1, ow)
                      Return Nil
                      })

         :Event( 1, {|ow| // ON INIT windows + close the "calculation" window
                      LOCAL cMsg, cNWin, cVal, nI, t2, aBC
                      t2   := ow:Cargo:tStart               // время продолжения процесса
                      cMsg := ow:Cargo:cBtnTitle + " - "

                      //_StopAnimateBox( "Avi_1" , ow:Name )
                      aBC := GetProperty( ow:Name, "Backcolor"  )
                      SetProperty( ow:Name, "Backcolor", GRAY )
                      DoMethod(ow:Name, "DisableUpdate")  // блокировать всю форму
                                                          // block the whole form
                      ? ProcNL(), "----- :Event(1)", ow:Name
                      /////////////// ---- второй цикл доп. расчётов
                      cNWin := WaitWinAvi( 'Creating a table, step 2',t2,, cResAvi, aBackClr, aFontClr )  // создаём окно ожидания с потоком
                      IF !Empty(cNWin)
                         // эмуляция - цикл вычислений/расчётов/созданий баз и т.д.
                         FOR nI := 50 TO 100
                            wApi_Sleep( 100 )
                            DO EVENTS
                            cVal := hb_ntos( nI ) + "/100"
                            _wSend("S_a_y", cNWin, cVal)
                            //_wSend(3, cNWin, cVal)   // или так
                            // WaitWinAviSay( cVal )   // или так
                         NEXT
                         WaitWinAvi()   // закрыть окно "ожидания"
                      ENDIF
                      /////////////// ---- второй цикл доп. расчётов

                      DO EVENTS
                      DoMethod(ow:Name, "EnableUpdate")    // разблокировка всей формы
                                                           // unlock the whole form
                      SetProperty( ow:Name, "Backcolor", aBC )
                      //_PlayAnimateBox( "Avi_1" , ow:Name )
                      //_OpenAnimateBox( "Avi_1" , ow:Name, ow:Cargo:cResAvi2 )
                      _wSend("AviOpen", ow)
                      cMsg += "Elapsed processing time - "
                      cMsg += HMG_TimeMS( ow:Cargo:tStart )
                      This.Label_1.VALUE := cMsg
                      SendMessage( ow:Handle, WM_PAINT, 0, 0 )     // Show form ICO
                      DoMethod( ow:Name, "Label_1", "SetFocus" )
                      // ow:Setfocus("Label_1")   // или так
                      ? ProcNL(), cMsg
                      DO EVENTS

                      Return Nil
                      })

         :Event({3, "Info"}, {|olbl| // INFO button-label "Label_Info"
                      Local ow := olbl:Window, n := 1
                      Local o  := olbl:Cargo
                      Local o1 := ow:GetObj("Label_1")
                      IF o:lAction ; Return Nil
                      ENDIF
                      o:lAction := .T.
                      _wSend("AviStop", ow, olbl)
                      o1:Setfocus()
                      DO EVENTS
                      SET WINDOW THIS TO ow:Name
                      PromptGetForms()   // -> demo_util.prg
                      SET WINDOW THIS TO
                      _wSend("AviOpen", ow, olbl)
                      olbl:BackColor := o:aFontBClr[n]
                      o1:Setfocus()
                      SendMessage( ow:Handle, WM_PAINT, 0, 0 )     // Show form ICO
                      o:lAction := .F.
                      Return Nil
                      })

         :Event({4, "About" }, {|olbl| // About button-label "Label_About"
                      Local ow := olbl:Window, n := 1
                      Local o  := olbl:Cargo
                      Local o1 := ow:GetObj("Label_1")
                      IF o:lAction ; Return Nil
                      ENDIF
                      o:lAction := .T.
                      _wSend("AviStop", ow, olbl)
                      o1:Setfocus()
                      DO EVENTS
                      SET WINDOW THIS TO ow:Name
                      MsgAbout( ow:Cargo:hIcon, 64 )             // -> demo_util.prg
                      SET WINDOW THIS TO
                      _wSend("AviOpen", ow, olbl)
                      olbl:BackColor := o:aFontBClr[n]
                      o1:Setfocus()
                      o:lAction := .F.
                      Return Nil
                      })

         :Event({5, "AviOpen" }, {|ow,ky,olbl|
                      Local cAvi := ow:Cargo:cAviName
                      _OpenAnimateBox( cAvi, ow:Name, ow:Cargo:cResAvi2 )
                      IF !Empty(olbl)
                         ky := olbl:Cargo
                         This.&(cAvi).BackColor := ky:aFontBClr[1]
                      ENDIF
                      Return Nil
                      })
         :Event({6, "AviStop" }, {|ow,ky,olbl|
                      Local cAvi := ow:Cargo:cAviName
                      _StopAnimateBox( cAvi, ow:Name )
                      IF !Empty(olbl)
                         ky := olbl:Cargo
                      ENDIF
                      Return Nil
                      })

         :Event(90, {|ow|
                      _logfile(.t.,"   ---[ :Event(90) ]---" + ProcNL(), ">>> RELEASE: " + ow:Name )
                      Return Nil
                      })
         :Event(99, {|ow| ow:Release() } )
      END WITH

   END WINDOW

   DoMethod( cFormName, "Center" )
   ACTIVATE WINDOW &cFormName ON INIT {|| This.Minimize, DoEvents(), This.Restore, DoEvents() }

   // чистка памяти, уборка мусора / memory cleaning, garbage collection
   // это для больших программ / this is for large programs
   DO EVENTS ; hb_gcAll() ; DO EVENTS

RETURN NIL

/////////////////////////////////////////////////////////////////
FUNCTION MouseFocus_Label(lFocus, o)
   LOCAL n, oo, ns := 4
   DEFAULT o := This.Cargo

   oo := o:oObj

   IF !Empty(lFocus)                         // got focus

      IF !o:lHover
         n := iif( o:lAction, 3, 2 )
         This.FontSize  := o:nFontHover
         //This.FontColor := o:aFontFClr[2]
         //This.BackColor := o:aFontBClr[n]
         Set_FBColor( oo:Index, o:aFontFClr[2], o:aFontBClr[n] )
         IF !Empty(o:cAvi)
            This.&(o:cAvi).BackColor := o:aFontBClr[n]
         ENDIF
         o:lHover := .T.
         oo:SetSize(oo:Row - ns, oo:Col - ns, oo:Width + ns*2, oo:Height + ns*2)
      ENDIF

   ELSE                                      // lost focus

      n := iif( o:lAction, 3, 1 )
      This.FontSize  := o:nFontLeave
      //This.FontColor := o:aFontFClr[1]
      //This.BackColor := o:aFontBClr[n]
      Set_FBColor( oo:Index, o:aFontFClr[1], o:aFontBClr[n] )
      IF !Empty(o:cAvi)
         This.&(o:cAvi).BackColor := o:aFontBClr[n]
      ENDIF
      o:lHover := .F.
      oo:SetSize(oo:Row + ns, oo:Col + ns, oo:Width - ns*2, oo:Height - ns*2)

   ENDIF

RETURN Nil

/////////////////////////////////////////////////////////////////
FUNCTION MouseFocus_Button(lFocus, oo)
   LOCAL ns := 5, o
   DEFAULT oo := This.Object

   o := oo:Cargo

   IF !Empty(lFocus)                            // got focus
      Set_FBColor( oo, o:aFClr[2], o:aBClr[2] )
      IF !o:lHover
         o:lHover := .T.
         oo:SetSize(o:nRow - ns, o:nCol - ns, o:nWidth + ns*2, o:nHeight + ns*2)
      ENDIF
   ELSE                                         // lost focus
      Set_FBColor( oo, o:aFClr[1], o:aBClr[1] )
      IF o:lHover
         o:lHover := .F.
         oo:SetSize(o:nRow, o:nCol, o:nWidth, o:nHeight)
      ENDIF
   ENDIF

   DO EVENTS

RETURN Nil

/////////////////////////////////////////////////////////////////
FUNCTION Set_FBColor( x, aFontClr, aBackClr )  // Control: LABEL, OBUTTON
   LOCAL i, t, h
   DEFAULT x := This.Index

   IF pCount() > 0
      IF HB_ISOBJECT(x) ; i := x:Index
      ELSE              ; i := x
      ENDIF
      IF i > 0
         t := _HMG_aControlType[ i ]               // type
         h := _HMG_aControlHandles[ i ]            // handle
         IF HB_ISARRAY(aFontClr)
            _HMG_aControlFontColor[ i ] := aFontClr
         ENDIF
         IF HB_ISARRAY(aBackClr)
            _HMG_aControlBkColor[ i ] := aBackClr
         ENDIF
         IF HB_ISARRAY( h )
            AEval ( h, {|x| RedrawWindow ( x , .T. ) } )
         ELSEIF t == "OBUTTON"
            InvalidateRect( h, 0 )
         ELSE
            RedrawWindow ( h )
         ENDIF
      ENDIF
   ENDIF

RETURN Nil

/////////////////////////////////////////////////////////////////
FUNCTION Control_Redraw( i )
   LOCAL h

   IF i > 0

      h := _HMG_aControlHandles[ i ]

      IF ValType ( h ) == "A"
         AEval ( h, {|x| RedrawWindow ( x , .T. ) } )
      ELSEIF _HMG_aControlType[ i ] == "OBUTTON"
         InvalidateRect( h, 0 )
      ELSE
         RedrawWindow ( h )
      ENDIF

   ENDIF

RETURN Nil
