/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2025 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * _TBrowse() Журнал событий программы в DBf-файле
 * _TBrowse() Program event log in DBf file
*/
#define  _HMG_OUTLOG
#include "hmg.ch"

//REQUEST HB_CODEPAGE_RU1251, HB_CODEPAGE_RU866    // Enter the required code page here
REQUEST DBFNTX, DBFCDX, DBFFPT
#define PROGRAM  "MiniGui: _TBrowse(). Program event log in DBf file"
#define PROGVER  "   Version 0.82 (02.10.2025)"

FUNCTION Main()
   LOCAL nY, nX, nG := 10, owc, oMenu, aBClr := {23,116,232}
   LOCAL cVal := MiniGuiVersion() + CRLF + Version() + CRLF + hb_Ccompiler()
   LOCAL nH := 96 + nG                  //Sys.ClientHeight
   LOCAL nW := Sys.ClientWidth

   WaitWindow( {"... Wait for the preparation to complete ...", App.ExeName }, .T., 600, 16, NIL, WHITE, aBClr )
   Set_Start_Init()   // -> demo_start.prg
   WaitWindow()

   SET FONT TO _GetSysFont(), App.Cargo:nFontSize

   DEFINE WINDOW wMain CLIENTAREA nW, nH TITLE PROGRAM + PROGVER ;
          MAIN NOMAXIMIZE NOSIZE TOPMOST BACKCOLOR aBClr         ;
          ON INIT    ( This.Topmost := .F., _wPost( 0) )         ;
          ON RELEASE ( This.Hide, _wSend(90) )
          This.Cargo := oHmgData() ; owc := This.Cargo

      owc:cForm    := This.Name
      owc:aBClr    := This.Backcolor
      owc:cMsg     := "An entry has been made in the Program Event Log!"
      owc:nG       := nG            // отступы
      owc:ahIcoDel := {}            // для удаления хендлов иконок с формы
      nW           := This.ClientWidth
      nH           := This.ClientHeight

      nY := nX := nG
      DRAW ICON IN WINDOW wMain AT nY, nW-96-nG PICTURE "1MG" WIDTH 96 HEIGHT 96 COLOR aBClr

      @ nY, nX LABEL Buff VALUE cVal WIDTH nW-96-nY*2 HEIGHT nH - nY*2 ;
        FONTCOLOR WHITE BOLD TRANSPARENT RIGHTALIGN

      // меню окна с кнопками / window menu with buttons
      oMenu       := MenuThisForm(owc:aBClr,owc)   // put here! we use - owc:aBClr
      oMenu:nX    := nX                            // coordinates of the start of the buttons
      oMenu:nY    := nY
      oMenu:nHBtn := nH - nG*2                     // button height
      oMenu:nWBtn := oMenu:nHBtn                   // button width
      MenuTopIconButtons(owc, oMenu, nG)           // -> menu_topButton.prg

      ON KEY F1     ACTION NIL
      ON KEY ESCAPE ACTION _wPost(99)

      WITH OBJECT This.Object
        :Event( 0, {|ow| ow:Setfocus("Buff"), DoEvents() })

        :Event({ 1,"Btn_1"}, {|ow,ky,cn,ne,ce| ne := 100, ce := FindEventsDim(ne) ,;      // -> user2log.prg
                                     User2LogWrite(ne, ce, cn)             ,;   // запись в журнал событий
                                     _SetThisFormInfo(ow) , User2Tsb(ky,cn) ,;  // -> user2tsb.prg
                                     _SetThisFormInfo() , This.&(cn).Enabled := .T. ,;
                                     ow:Setfocus("Buff"), DoEvents() })

        :Event({ 2,"Btn_2"}, {|ow,ky,cn| // button - Test 1
                               Local nEvtn, cEvtn, cRem
                               nEvtn := 101
                               cEvtn := FindEventsDim(nEvtn)
                               cRem  := cn
                               User2LogWrite(nEvtn, cEvtn, cRem)    // write to event log
                               _SetThisFormInfo(ow)
                               MsgDebug(ow:Name,ky,cn,ow:Cargo:cMsg)
                               _SetThisFormInfo()
                               This.&(cn).Enabled := .T.
                               ow:Setfocus("Buff")
                               DoEvents()
                               Return Nil
                               } )

        :Event({ 3,"Btn_3"}, {|ow,ky,cn| // button - Test 2
                               Local nEvtn, cEvtn, cRem
                               nEvtn := 102
                               cEvtn := FindEventsDim(nEvtn)
                               cRem  := cn
                               _SetThisFormInfo(ow)
                               User2LogWrite(nEvtn, cEvtn, cRem)
                               MsgDebug(ow:Name,ky,cn,ow:Cargo:cMsg)
                               _SetThisFormInfo()
                               This.&(cn).Enabled := .T.
                               ow:Setfocus("Buff")
                               DoEvents()
                               Return Nil
                               } )

        :Event({ 4,"Btn_4"}, {|ow,ky,cn| // button - Test 3
                               Local nEvtn, cEvtn, cRem
                               nEvtn := 103
                               cEvtn := FindEventsDim(nEvtn)
                               cRem  := cn
                               User2LogWrite(nEvtn, cEvtn, cRem)
                               _SetThisFormInfo(ow)
                               MsgDebug(ow:Name,ky,cn,ow:Cargo:cMsg)
                               _SetThisFormInfo()
                               This.&(cn).Enabled := .T.
                               ow:Setfocus("Buff")
                               DoEvents()
                               Return Nil
                               } )

        :Event({ 5,"Btn_5"}, {|ow,ky,cn|  // button - Error
                               Local nEvtn, cEvtn, cRem
                               nEvtn := 991
                               cEvtn := FindEventsDim(nEvtn)
                               cRem  := "Non-existent object for error!"
                               User2LogWrite(nEvtn, cEvtn, cRem)
                               _SetThisFormInfo(ow)
                               //
                               _SetThisFormInfo()
                               This.&(cn).Enabled := .T.
                               ow:Setfocus("Buff")
                               SetProperty( ow:Name, "Label_1", "Value", cRem )
                               DoEvents()
                               ky := cn
                               Return Nil
                               } )

        :Event({ 6,"Btn_6"}, {|ow,ky,cn|  // button - Index
                               Local nEvtn, cEvtn, cRem
                               nEvtn := 105
                               cEvtn := FindEventsDim(nEvtn)
                               cRem  := cn
                               User2LogWrite(nEvtn, cEvtn, cRem)
                               _SetThisFormInfo(ow)
                               MenuIndex(ow,ky,cn)        // User2Index.prg
                               _SetThisFormInfo()
                               This.&(cn).Enabled := .T.
                               ow:Setfocus("Buff")
                               DoEvents()
                               Return Nil
                               } )

        :Event({ 7,"Btn_Exit"}, {|ow,ky,cn| _wSend(99,ow) , ky := cn })

        :Event(10, {|ow,ky,cn| _SetThisFormInfo(ow) , MsgDebug(ky,cn) ,;   // as a reserve
                               _SetThisFormInfo() , ow:Setfocus("Buff"), DoEvents() })
        :Event(90, {|ow,ky| // ON Release windows
                            Local cm, ct, ah
                            cm := ProcNL()
                            ct := HMG_TimeMS( App.Cargo:tStart )
                            ?  cm, "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                            ?  Repl(".", Len(cm)), "=> RELEASE WINDOW <=", ow:Name
                            ?? "... Program running time -", ct
                            // entry in the program Event Log
                            User2Close(ct)  // -> user2log.prg
                            ? Repl(".", 10),"Delete handle icon - ow:Cargo:ahIcoDel="
                            ?? ah, HB_ValToExp(ah)
                            IF IsArray(ah)
                               AEval(ah, {|h| DestroyIcon(h) })  // удалить хендлы иконок
                            Endif
                            DO EVENTS
                            Return Nil
                            })

        :Event(99, {|ow| ow:Release() })
      END WITH

   END WINDOW

   ACTIVATE WINDOW wMain

RETURN NIL

///////////////////////////////////////////////////////////////////
STATIC FUNCTION MenuThisForm(aBColor, owc, cPref, l99)
   LOCAL hFont, aFont, oMenu := oHmgData()
   DEFAULT aBColor := GRAY
   DEFAULT cPref := "Btn_", l99 := .T.

   hFont := GetFontHandle('ItalBold')
   aFont := GetFontParam(hFont)
   // имя объекта + имя события
   oMenu:aObj  := { "Btn_1" , "Btn_2", "Btn_3", "Btn_4", "Btn_5" , "Btn_6" , "Btn_Exit"  }
   oMenu:aImg  := { {"iHelp48x1","iHelp48x2"} , {"iHelp48x1","iHelp48x2"} , {"iHelp48x1","iHelp48x2"} ,;
                    {"iHelp48x1","iHelp48x2"} , {"iHelp48x1","iHelp48x2"} , {"iHelp48x1","iHelp48x2"} ,;
                    {"iReturn48x1","iReturn48x2"}  }
   oMenu:aMnRu := { "ТСБ-лог", "Тест 1" , "Тест 2" , "Тест 3" , "Ошибка" , "Индекс" , "Выход" }
   oMenu:aMnEn := { "Log-Tsb", "Test 1" , "Test 2" , "Test 3" , "Error"  , "Index"  , "Exit"  }
   oMenu:aTpRu := { "Журнал событий в программе", "Тестовое окно 1", "Тестовое окно 2", "Тестовое окно 3", "Ошибка - аварийное закрытие программы", "Индексация базы данных", "Выход из программы" }
   oMenu:aTpEn := { "Event log in the program", "Test window 1", "Test window 2", "Test window 3", "Error - abnormal program closure", "Database indexing", "Exit the program" }
   // запомним на окне языки / Let's remember the languages on the window
   oMenu:aCapt := IIF( App.Cargo:cLang == "RU", oMenu:aMnRu, oMenu:aMnEn )
   owc:aTopMenuRu    := oMenu:aMnRu
   owc:aTopMenuEn    := oMenu:aMnEn
   owc:aTopMenuRuTip := oMenu:aTpRu
   owc:aTopMenuEnTip := oMenu:aTpEn
   //
   oMenu:nHIco       := IIF( App.Cargo:aDisplayMode[2] <= 720, 28, 48 )  // высота-ширина иконки на кнопке
   oMenu:nHG2        := 5                                                // добавочная высота к тексту кнопки
   oMenu:aBtnFClr    := { BLACK, YELLOW     }                            // цвет фонта кнопки + 2-цвет инвертный
   //oMenu:aBtnBClr  := { {66,92,251} , WHITE, YELLOW, GRAY }            // цвет фона кнопки + 2/3/4-цвет инвертный
   oMenu:aBtnBClr    := { aBColor , BLACK   }                            // цвет фона кнопки + цвет инвертный
   oMenu:aBtnFont    := { "Tahoma", 12, .T. }                            // фонт на кнопках 
   oMenu:nX          := 0
   oMenu:nY          := 0
   oMenu:lAutoSize   := .F.        // T - автоматический расчёт высоты и ширины кнопки от высоты иконки
   //oMenu:nWBtn     := 120        // ручное задание ширины кнопки
   //oMenu:nHBtn     := 100        // ручное задание высоты кнопки

RETURN oMenu

*----------------------------------------------------------------------------*
INIT PROCEDURE Sets_ENV()
*----------------------------------------------------------------------------*
   LOCAL o, cLog, cIni  := hb_FNameExtSet( App.ExeName, ".ini" )

   //SET CODEPAGE TO ENGLISH       // analogue HB_CDPSELECT( "EN" )
   //SET LANGUAGE TO ENGLISH       // analogue HB_LANGSELECT( "EN" )
   //SET CODEPAGE TO RUSSIAN
   //SET LANGUAGE TO RUSSIAN

   rddSetDefault( "DBFCDX" )

   SET DECIMALS  TO 4
   SET EPOCH     TO 2000
   SET DATE      TO GERMAN
   SET CENTURY   ON
   SET DELETED   OFF
   SET AUTOPEN   ON  // !!! всегда так / always like this
   SET EXACT     ON
   SET EXCLUSIVE ON
   SET SOFTSEEK  ON
   SET OOP ON
   SET DATE FORMAT TO "DD.MM.YY"
   SET TOOLTIPSTYLE BALLOON
   //!!! такой порядок
   IF !HB_ISOBJECT( App.Cargo ) ; App.Cargo := oHmgData()
   ENDIF
   o := App.Cargo

   Set ShowRedAlert On        // увеличить фонт для окна "Program Error"

   // Проверка на запуск второй копии программы
   _HMG_MESSAGE[4] := "Attempting to run a second copy of the program:" + CRLF + ;
                      App.ExeName + CRLF + ;
                      "Refused to start." + CRLF + _HMG_MESSAGE[4]
   SET MULTIPLE QUIT WARNING  // окно маленькое
   SET WINDOW MAIN OFF

   o:tStart         := hb_DateTime()        // start time
   o:cLogFile       := ChangeFileExt( App.ExeName, '.log' )
   // для отладки - потом убрать
   cLog             := o:cLogFile
   //o:cLogFile       := cFilePath( cLog ) + "\"
   //o:cLogFile       += "_" + cFileNoPath( cLog )
   //
   o:tStart         := hb_DateTime()       // start time
   o:cIniFile       := cIni
   o:lLogDel        := .T.
   o:aDlgBColor     := {  5 , 191, 255 }     // Alert* BackColor
   o:aDlgFColor     := {  0 ,  0 ,  0  }     // Alert* FontColor
   o:aBClrMain      := {127,189,228}
   o:cDefAppIcon    := "1MG"
   o:lDebug         := .T.
   o:nMenuBmpHeight := 32
   o:aWinOpen       := {}
   o:cTitle         := PROGRAM
   o:cVersion       := PROGVER
   o:cLang          := "EN"
   o:cAvtor         := "Copyright 2025 Verchenko Andrey + Sergej Kiselev"
   o:cEmail         := "<verchenkoag@gmail.com> Dmitrov, Moscow region / <bilance@bilance.lv>"
   o:cPrgInfo1      := "Many thanks for your help: Grigory Filatov <gfilatov@inbox.ru>"
   o:cPrgInfo2      := "Tips and tricks programmers from our forum http://clipper.borda.ru"
   o:cSiteDownload  := "Home page for download - http://www.hmgextended.com/"
   o:cPathTemp      := GetUserTempFolder() + "\"
   o:cPathDbf       := GetStartUpFolder() + "\DBF\"
   o:cPathStart     := GetStartUpFolder() + "\"
   o:cPathErrLog    := GetStartUpFolder() + "\ErrorsLog_My\"
   //o:aDisplayMode := { System.DesktopWidth , System.DesktopHeight - GetTaskBarHeight() }
   o:aDisplayMode   := { Sys.ClientWidth , Sys.ClientHeight }
   // задание своих параметров, позволяет протестировать на другие разрешения экрана
   // setting your parameters, allows you to test for other screen resolutions
   //o:aDisplayMode   := { 1280 , 900 }
   o:cDisplayMode   := HB_NtoS(o:aDisplayMode[1]) + "x" + HB_NtoS(o:aDisplayMode[2])
   o:cFontName      := "DejaVu Sans Mono"   // "Arial"
   o:cFontName2     := "Comic Sans MS"
   o:nFontSize      := 14
   o:cDlgFont       := "DejaVu Sans Mono"
   o:nDlgSize       := o:nFontSize + 2
   o:nDefFontSize   := o:nDlgSize
   o:cUser          := "This_user_program"
   o:nUser          := -1

   PUBLIC SETTEMP
   M->SETTEMP       := o:cPathTemp   // совместимость со старыми версиями
                                     // compatibility with older versions
   _SetGetLogFile( o:cLogFile )

   IF o:lLogDel ; hb_FileDelete( o:cLogFile )
   ENDIF

   IF o:lDebug ; SET LOGERROR ON
   ELSE        ; SET LOGERROR OFF
   ENDIF

   // Default font
   SET FONT TO o:cFontName , o:nFontSize
   // TsBrowse                                       bold italic
   _DefineFont("Normal"  , o:cFontName , o:nFontSize  , .F., .F. )
   _DefineFont("Bold"    , o:cFontName , o:nFontSize  , .T., .F. )
   _DefineFont("Italic"  , o:cFontName , o:nFontSize-2, .F., .T. )
   _DefineFont("ItalBold", o:cFontName , o:nFontSize-2, .T., .T. )
   _DefineFont("SpecHdr" , o:cFontName , o:nFontSize-4, .T., .T. )
   _DefineFont("SuperHd" , o:cFontName2, o:nFontSize+6, .F., .F. )
   _DefineFont("TsbEdit" , "Arial"     , o:nFontSize  , .F., .T. )
   _DefineFont("Bold3"   , o:cFontName , o:nFontSize-2, .T., .F. )
   // Menu* font
   _DefineFont("ComSanMS" , o:cFontName2 , o:nFontSize+2 , .F., .F. )         // фонт главного верхнего меню
   _DefineFont("MnNormal" , o:cFontName  , o:nFontSize+2 , .F., .F. )         // фонт главного верхнего меню
   _DefineFont("MenuBtn"  , o:cFontName  , o:nFontSize   , .T., .F. )         // фонт кнопок верхнего меню
   _DefineFont("WinBtn"   , o:cFontName  , o:nFontSize+2 , .F., .F. )         // фонт кнопок окон
   _DefineFont("FntCnMn1" , o:cFontName  , o:nFontSize+2 , .F., .F. )         // фонт-1 в контекстном меню
   _DefineFont("FntCnMn2" , "Arial"      , o:nFontSize+2 , .F., .T. )         // фонт-2 в контекстном меню
   // Alert* font
   _DefineFont("DlgFont" , o:cDlgFont , o:nDlgSize   , .F., .F. )             // фонт окна Alert*
   // Alert* colors
   SET MSGALERT BACKCOLOR  TO o:aDlgBColor
   SET MSGALERT FONTCOLOR  TO o:aDlgFColor
   //
   SET DEFAULT ICON TO o:cDefAppIcon
   SET WINDOW MODAL PARENT HANDLE ON
   SET TOOLTIPSTYLE BALLOON
   SET NAVIGATION EXTENDED
   SET MENUSTYLE  EXTENDED
   SetMenuBitmapHeight( 32 )           // set menu icons size to 32x32

   // Аварийная ошибка в программе / Program crash
   //_HMG_bOnErrorInit := {|cMsg| myExitError(cMsg) }
   //_HMG_bOnErrorExit := {|| myExitError() }
   _HMG_bOnErrorInit := {|cMsg,oErr,cTxt,cErr| my_ErrorExit(cMsg,oErr,cTxt,cErr) }
   _HMG_bOnErrorExit := {|    | my_ErrorExit()     }

   ? REPL("=",20) + " Program start - " + HB_TTOC( o:tStart ) + " " + REPL("=",20)
   ? MiniGuiVersion()  ; ? Version() ; ? hb_Ccompiler() ; ? ProcNL()

RETURN

