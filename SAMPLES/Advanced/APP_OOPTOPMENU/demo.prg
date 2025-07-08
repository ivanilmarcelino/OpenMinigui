/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2024 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Меню на кнопках / Menu on buttons
 * Две кнопки на одной, два состояния кнопки / Two buttons on one, two button states
*/
ANNOUNCE RDDSYS
#define _HMG_OUTLOG
#include "MiniGUI.ch"
#include "i_winuser.ch"

#define PROGRAM_EN  "Menu on buttons - two button states"
#define PROGRAM_RU  "Меню на кнопках - два состояния кнопки"
#define PROGVER  "Version 0.4 (07.11.2024)"
#define LANG_PRG "EN" // English interface-lang
//////////////////////////////////////////////////////////////////////////////////////////
FUNCTION Main()
   LOCAL cForm, aBColor, aFClr, cLogo, hLogo, aLogo, oMenu
   LOCAL nY, nX, nH, nW, nG, owc, o, cFName, nFSize

   cForm := "wMain"
   cLogo := "LogoDemo"               // 646x110
   hLogo := LoadImage(cLogo)
   aLogo := GetBitmapSize( hLogo )   // Size
   DeleteObject( hLogo )
   IF App.Cargo:aDisplayMode[2] <= 720
      aLogo := { aLogo[1] * 0.7 , aLogo[2] * 0.8 }
   ENDIF
   nY      := nX := 0  ; nG := 20
   nW      := App.Cargo:aDisplayMode[1]  //Sys.ClientWidth
   nH      := aLogo[2] + GetTitleHeight() + GetBorderHeight()
   aBColor := {109,1,1}
   aFClr   := YELLOW
   cFName  := App.Cargo:cFontName
   nFSize  := App.Cargo:nFontSize

   DEFINE WINDOW &cForm AT nY, nX WIDTH nW HEIGHT nH ;
      TITLE App.Cargo:cTitle                         ;
      MAIN TOPMOST                                   ;
      NOMAXIMIZE NOSIZE                              ;
      BACKCOLOR aBColor                              ;
      FONT cFName SIZE nFSize                        ;
      ON INIT    _wPost( 0)                          ;
      ON RELEASE _wSend(90)

      // координаты внутри окна
      nW  := This.ClientWidth
      nH  := This.ClientHeight

      This.Cargo    := oHmgData() ; owc := This.Cargo
      owc:aBClr     := This.BACKCOLOR            // запомним
      owc:nX        := aLogo[1] + nG
      owc:nG        := nG
      owc:Lbl_1     := MiniGuiVersion(1)
      owc:Lbl_2     := App.Cargo:cTitle
      owc:Lbl_3     := PROGVER
      owc:ahIcoDel  := {}                        // для удаления хендлов иконок с формы
      owc:nBtnSeach := 1                         // флаг для кнопки
      owc:nBtnPrint := 1                         // флаг для кнопки

      @ 0, 0 IMAGE Img_1 OF &cForm PICTURE cLogo ;
        WIDTH aLogo[1] HEIGHT aLogo[2] STRETCH TRANSPARENT

      // top menu of the window with buttons
      oMenu := MenuThisForm(owc:aBClr)            // ставить здесь ! используем - owc:aBColor
      oMenu:nX    := owc:nX                       // координаты начала кнопок
      oMenu:nY    := INT(nG/4)
      oMenu:nHBtn := nH - INT(nG/2)               // высота кнопки
      MenuTopIconButtons( owc, oMenu )            // -> menu_topButton.prg (owc:nG)
      nX      := owc:nWEndTB
      owc:nX  := nX + nG/2
      owc:nH3 := nH/3
      @ 0        , owc:nX LABEL Lbl_1 VALUE owc:Lbl_1 WIDTH nW - owc:nX HEIGHT owc:nH3 FONTCOLOR aFClr TRANSPARENT VCENTERALIGN
      @ owc:nH3  , owc:nX LABEL Lbl_2 VALUE owc:Lbl_2 WIDTH nW - owc:nX HEIGHT owc:nH3 FONTCOLOR aFClr TRANSPARENT VCENTERALIGN
      @ owc:nH3*2, owc:nX LABEL Lbl_3 VALUE owc:Lbl_3 WIDTH nW - owc:nX HEIGHT owc:nH3 FONTCOLOR aFClr TRANSPARENT VCENTERALIGN

      //_o2log(owc, 27, ProcNL() + "  owc => This.Cargo: " + cForm, .T. ) ; ?

      o := This.Object
      o:Event( 0, {|     | This.Topmost := .F. , _wPost(1) })
      o:Event( 1, {|ow,ky| // ON INIT
                           ? "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                           ?  Repl(".", 10), "=> ONINIT WINDOW <=", ow:Name
                           DO EVENTS
                           This.&("_Exit").Enabled := .F. // block button
                           _wPost({10,"_Help"},ow:Name)
                           Return Nil
                           })

      // object name + event name - { "_Help" , "_Search", "_Print", "_Exit"  }
      o:Event({10,"_Help"}, {|ow,ky,cn| This.&(cn).Enabled := .F. , _SetThisFormInfo(ow) ,;
                                        MsgDebug(ow:Name,ky,cn)                          ,;
                                        _SetThisFormInfo()                               ,;
                                        This.&(cn).Enabled := .T.      ,;  // unlock button
                                        This.&("_Exit").Enabled := .T. ,;  // unlock button
                                        ow:Setfocus('Img_1')  } )

      o:Event({20,"_Search"}, {|ow,ky,cn| //  Две кнопки на одной
                                          Local owc := ow:Cargo
                                          This.&(cn).Enabled := .F.

                                          SET WINDOW THIS TO ow:Name
                                          IF owc:nBtnSeach == 1
                                             MsgDebug(ow:Name,ky,cn,"Change button - owc:nBtnSeach=",owc:nBtnSeach)
                                          ELSE
                                             AlertInfo("Change button - owc:nBtnSeach=" + HB_NtoS(owc:nBtnSeach) )
                                          ENDIF
                                          SET WINDOW THIS TO

                                          owc:nBtnSeach ++
                                          owc:nBtnSeach := IIF(owc:nBtnSeach>2,1,owc:nBtnSeach)
                                          ? ProcNL(), "Change button - owc:nBtnSeach=",owc:nBtnSeach
                                          This.&(cn).Cargo:nI :=  owc:nBtnSeach
                                          RefreshButton(cn)       // перепоказать кнопку
                                          This.&(cn).Enabled      := .T.
                                          This.&("_Exit").Enabled := .T.     // unlock button
                                          ow:Setfocus('Img_1')
                                          DO EVENTS
                                          Return Nil
                                          } )

      o:Event({30,"_Print"}, {|ow,ky,cn| // Две кнопки на одной
                                          Local owc  := ow:Cargo
                                          This.&(cn).Enabled := .F.

                                          SET WINDOW THIS TO ow:Name
                                          IF owc:nBtnPrint == 1
                                             MsgDebug(ow:Name,ky,cn, "Change button - owc:nBtnPrint=",owc:nBtnPrint)
                                          ELSE
                                             AlertInfo("Change button - owc:nBtnPrint=" + HB_NtoS(owc:nBtnPrint) )
                                          ENDIF
                                          SET WINDOW THIS TO

                                          owc:nBtnPrint ++
                                          owc:nBtnPrint := IIF(owc:nBtnPrint>2,1,owc:nBtnPrint)
                                          ? ProcNL(), "Change button - owc:nBtnPrint=",owc:nBtnPrint
                                          // включить переключатель внутри кнопки
                                          This.&(cn).Cargo:nI     := owc:nBtnPrint
                                          RefreshButton(cn)       // перепоказать кнопку
                                          This.&(cn).Enabled      := .T.
                                          This.&("_Exit").Enabled := .T.   // unlock button
                                          ow:Setfocus('Img_1')
                                          DO EVENTS
                                          Return Nil
                                          } )

      o:Event({89,"_Exit"  }, {|ow| _LogFile(.T., ProcNL(),">>> Exit button pressed! Window: "+ow:Name), _wSend(99) } )

      o:Event(90, {|ow,ky| // ON RELEASE
                           Local ah := ow:Cargo:ahIcoDel
                           ? "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                           ? Repl(".", 10), "=> RELEASE WINDOW <=", ow:Name
                           ? Repl(".", 10),"Delete handle icon - ow:Cargo:ahIcoDel="
                           ?? ah, HB_ValToExp(ah)
                           IF IsArray(ah)
                              AEval(ah, {|h| DestroyIcon(h) })  // удалить хендлы иконок
                           ENDIF
                           DO EVENTS
                           Return Nil
                           })

      o:Event(99, {|ow| ow:Release()   })

      ON KEY F1     ACTION NIL
      ON KEY ESCAPE ACTION _wPost(99)

   END WINDOW

   //CENTER WINDOW &cForm
   ACTIVATE WINDOW &cForm

RETURN NIL

///////////////////////////////////////////////////////////////////
STATIC FUNCTION MenuThisForm(aBColor)
   LOCAL hFont, aFont, oMenu := oHmgData()
   DEFAULT aBColor := GRAY

   hFont := GetFontHandle('ItalBold')
   aFont := GetFontParam(hFont)
   // имя объекта + имя события
   oMenu:aObj  := { "_Help" , "_Search", "_Print", "_Exit"  }
   oMenu:aImg  := { {"i1Help48x1" ,"i1Help48x2"}                                ,;
                    {"i1Search48x1","i1Search48x2","i1Clear48x1","i1Clear48x2"} ,;
                    {"i1Print48x1","i1Print48x2","i1Text48x1","i1Text48x2"}     ,;
                    {"i1Exit48x1","i1Exit48x2"}         }
   oMenu:aMnRu := { "Помощь", {"Поиск;оператора","Очистить;поиск"}, {"Печать","Блокнот" }, "Выход" }
   oMenu:aMnEn := { "Help"  , {"Search;user"    ,"Clear;search"  }, {"Print" ,"Notepad" }, "Exit" }
   oMenu:aCapt := IIF( App.Cargo:cLang == "RU", oMenu:aMnRu, oMenu:aMnEn )
   oMenu:nHIco := IIF( App.Cargo:aDisplayMode[2] <= 720, 32, 48 )  // высота-ширина иконки на кнопке
   oMenu:nHG2  := 5           // добавочная высота к тексту кнопки
   oMenu:aBtnFClr  := { WHITE   , BLACK, RED, BLACK  }            // цвет фонта кнопки + 2/3/4-цвет инвертный
   //oMenu:aBtnBClr  := { {66,92,251} , WHITE, YELLOW, GRAY }     // цвет фона кнопки + 2/3/4-цвет инвертный
   oMenu:aBtnBClr:= { aBColor , WHITE, YELLOW, GRAY  }            // цвет фона кнопки + цвет инвертный
   oMenu:aBtnFont:= { aFont[1], aFont[2], aFont[3] }              // фонт на кнопках
   //oMenu:aBtnFont  := { "Tahoma", aFont[2]+2, .T. }  // фонт на кнопках - можно и так задать
   oMenu:nX        := 0
   oMenu:nY        := 0
   oMenu:lAutoSize := .T.        // T - автоматический расчёт высоты и ширины кнопки от высоты иконки
   oMenu:nWBtn     := 0          // ручное задание ширины кнопки
   oMenu:nHBtn     := 0          // ручное задание высоты кнопки

   //oMenu:lAutoSize := .F.      // F - ручное задание
   //oMenu:nWBtn   := 120        // ручное задание ширины кнопки
   //oMenu:nHBtn   := 100        // ручное задание высоты кнопки

RETURN oMenu

*----------------------------------------------------------------------------*
INIT PROCEDURE Sets_ENV()
*----------------------------------------------------------------------------*
   LOCAL o

   SET DECIMALS  TO 4
   SET EPOCH     TO 2000
   SET DATE      TO GERMAN
   SET CENTURY   ON
   SET DELETED   OFF
   SET AUTOPEN   OFF
   SET EXACT     ON
   SET EXCLUSIVE ON
   SET SOFTSEEK  ON
   SET OOP ON
   SET DATE FORMAT TO "DD.MM.YY"
   SET TOOLTIPSTYLE BALLOON

   SET CODEPAGE TO RUSSIAN
   SET LANGUAGE TO RUSSIAN

   //rddSetDefault("DBFNTX")

   SET WINDOW MAIN OFF

   IF !HB_ISOBJECT( App.Cargo ) ; App.Cargo := oHmgData()
   ENDIF

   o := App.Cargo
   o:cLang          := LANG_PRG           // язык программы
   o:tStart         := hb_DateTime()      // start time
   o:cTitle         := iif( o:cLang == "EN", PROGRAM_EN, PROGRAM_RU )
   o:cVersion       := PROGVER
   o:cFontName      := "DejaVu Sans Mono" //"Arial"
   o:nFontSize      := 12
   o:cLogFile       := "_msg.log"
   o:lLogDel        := .T.
   o:cDlgFont       := "DejaVu Sans Mono"
   o:nDlgSize       := o:nFontSize + 2
   o:aDlgBColor     := { 197, 130, 159 }     // Alert* BackColor
   o:aDlgFColor     := {  0 ,  0 ,  0  }     // Alert* FontColor
   o:aBClrMain      := {  5 , 191, 255 }
   o:cDefAppIcon    := "1MG"
   o:lDebug         := .T.
   o:nMenuBmpHeight := 32                    // высота иконок контекстного меню
   o:cTitleWin      := o:cTitle
   o:cVersion       := PROGVER
   o:cProga         := o:cTitleWin + SPACE(3) + PROGVER
   o:cAvtor         := "Copyright 2024 Verchenko Andrey + Sergej Kiselev"
   o:cEmail         := "<verchenkoag@gmail.com> Dmitrov, Moscow region / <bilance@bilance.lv>"
   o:cPathTemp      := GetUserTempFolder() + "\TEMP\"
   o:cPathStart     := GetStartUpFolder() + "\"
   o:aDisplayMode   := { Sys.ClientWidth , Sys.ClientHeight }
   // задание своих параметров, позволяет протестировать на другие разрешения экрана
   // setting your parameters, allows you to test for other screen resolutions
   //o:aDisplayMode := { 1280 , 1280 }   // задать свои координаты
   //o:aDisplayMode := { 1280 , 680 }  // дисплей Сергея
   o:cDisplayMode   := HB_NtoS(o:aDisplayMode[1]) + "x" + HB_NtoS(o:aDisplayMode[2])
   o:nIcoMainConn   := 24
   o:nMenuBmpHeight := 32
   o:nTsbHCell      := 48           // высота иконок / ячейки таблицы = высоте иконкм

   IF o:aDisplayMode[2] <= 720
      o:nTsbHCell      := 32
      o:nFontSize      -= 3
      o:nDlgSize       -= 2
      o:nMenuBmpHeight := 24
      o:nIcoMainConn   := 16
   ENDIF

   _SetGetLogFile( o:cLogFile )

   IF o:lLogDel ; hb_FileDelete( o:cLogFile )
   ENDIF

   IF o:lDebug ; SET LOGERROR ON
   ELSE        ; SET LOGERROR OFF
   ENDIF

   // Default font
   SET FONT TO o:cFontName , o:nFontSize
   // TsBrowse                                       bold italic
   _DefineFont("Normal"  , o:cFontName    , o:nFontSize  , .F., .F. )
   _DefineFont("Bold"    , o:cFontName    , o:nFontSize  , .T., .F. )
   _DefineFont("SpecHdr" , o:cFontName    , o:nFontSize-3, .F., .T. )
   _DefineFont("SuperHd" , "Comic Sans MS", o:nFontSize+2, .T., .T. )
   _DefineFont("TsbEdit" , "Arial"        , o:nFontSize  , .F., .T. )
   // Menu* font
   _DefineFont("ComSanMS" , "Comic Sans MS", o:nFontSize+2 , .F., .F. )
   // Alert* font
   _DefineFont("DlgFont" , o:cDlgFont , o:nDlgSize   , .F., .F. )
   // Alert* colors
   SET MSGALERT BACKCOLOR  TO o:aDlgBColor
   SET MSGALERT FONTCOLOR  TO o:aDlgFColor
   //
   // Проверка на запуск второй копии программы
   _HMG_MESSAGE[4] := "Attempting to run a second copy of the program:" + CRLF + ;
                      App.ExeName + CRLF + ;
                      "Refused to start !" + CRLF + _HMG_MESSAGE[4]
   SET DEFAULT ICON TO o:cDefAppIcon
   SET MULTIPLE QUIT WARNING  // окно маленькое
   SET WINDOW MODAL PARENT HANDLE ON
   SET TOOLTIPSTYLE BALLOON
   SET NAVIGATION EXTENDED
   SET MENUSTYLE  EXTENDED
   Set ShowRedAlert On        // увеличить фонт для окна "Program Error"

   SetMenuBitmapHeight( o:nMenuBmpHeight )

   // Добавочные данные

RETURN

///////////////////////////////////////////////////////////////////////////////
FUNCTION GetTxtWidth( cText, nFontSize, cFontName, lBold )  // получить Width текста
   LOCAL hFont, nWidth
   DEFAULT cText     := REPL('A', 2)        ,  ;
           cFontName := _HMG_DefaultFontName,  ;   // из MiniGUI.Init()
           nFontSize := _HMG_DefaultFontSize,  ;   // из MiniGUI.Init()
           lBold     := .F.

   IF Valtype(cText) == 'N'
      cText := repl('A', cText)
   ENDIF

   hFont  := InitFont(cFontName, nFontSize, lBold)
   nWidth := GetTextWidth(0, cText, hFont)         // ширина текста
   DeleteObject (hFont)

   RETURN nWidth

////////////////////////////////////////////////////////////////
FUNCTION ProcNL(nVal, cMsg)
   Default cMsg := ">>> "
   nVal := iif( Empty(nVal), 0, nVal ) + 1
   cMsg += ProcName(nVal) + "(" + hb_ntos( ProcLine(nVal) ) + ")"
   cMsg += " => " + ProcFile(nVal)
RETURN cMsg

