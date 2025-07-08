/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2024 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Отладка программы. Запуск окна отладки.
*/
#define _HMG_OUTLOG
#include "hmg.ch"
#include "i_winuser.ch"

REQUEST HB_CODEPAGE_UTF8, HB_CODEPAGE_RU866, HB_CODEPAGE_RU1251
REQUEST DBFNTX, DBFCDX, DBFFPT

#define PROGRAM  "Debugging the program. Launching an external editor"
#define PROGVER  "Version 0.1 (20.05.2024)"
#define LANG_PRG "EN" // English interface-lang

*----------------------------------------------------------------------------*
FUNCTION Main()
   LOCAL nH, nW, aBClr, owc

   nH    := 64 + 5 + 5
   aBClr := ORANGE

   DEFINE WINDOW wMain CLIENTAREA Sys.ClientWidth, nH   ;
      TITLE PROGRAM + SPACE(10) + PROGVER               ;
      MAIN NOMAXIMIZE NOSIZE BACKCOLOR aBClr            ;
      ON INIT    _wPost(0)                              ;
      ON RELEASE _wSend(90)

      nW := This.ClientWidth                        // width inside window
      nH := This.ClientHeight                       // height inside the window

      This.Cargo := oHmgData() ; owc := This.Cargo  // для окна создаем объект без переменных (условно пустой)
      owc:oWinMain := This.Object                   // объект окна, просто для примера
      owc:cForm    := This.Name                     // имя окна, просто для примера
      owc:aBColor  := This.BackColor                // цвет окна
      owc:cVal     := MiniGuiVersion() + CRLF + Version() + CRLF + hb_Ccompiler()
      owc:nHIco    := 64                            // размер иконки
      owc:cRes     := App.Cargo:cDefAppIcon         // имя ресурса иконки
      owc:nXGIco   := 10                            // отступ для иконок от начала формы по X
      owc:nYGaps   := 2
      Draw_Icon(owc)                                // вывод иконок на форму
      owc:nW3Ico   := owc:nWIcoEnd + owc:nXGaps     // последняя иконка по X
      //? ProcNL(), "### Draw_Icon()  nHIcon=", owc:nHIco, HB_ValToExp(owc:aIcoLogoYX)

      DRAW ICON IN WINDOW wMain AT 5, nW-owc:nHIco PICTURE owc:cRes WIDTH owc:nHIco HEIGHT owc:nHIco COLOR owc:aBColor

      @ 5, owc:nW3Ico LABEL Buff VALUE owc:cVal WIDTH nW-owc:nHIco-5-owc:nW3Ico HEIGHT nH-5*2 ;
        FONTCOLOR WHITE TRANSPARENT RIGHTALIGN

      (This.Object):Event( 0, {|ow| // ON Init windows
                                    _wSend(1)           // выполнить и ждать пока не выполниться
                                    ow:SetFocus("Buff")
                                    DoEvents()
                                    wApi_Sleep(50)
                                    Test(owc:oWinMain) // проверка записи переменных
                                    wMain.Release()    // закрыть программу
                                    Return Nil
                                    })
      (This.Object):Event( 1, {|ow| // _wSend(1)
                                    DoEvents()
                                    // прорисовка иконки сразу на форме
                                    SendMessage( ow:Handle, WM_PAINT, 0, 0 )
                                    DoEvents()
                                    // запуск отдельного окна отладки
                                    WinDebugLog(App.Cargo:cLogFile)  // -->> form_debug_log.prg
                                    WinDebugLog(App.Cargo:cIniFile)  // -->> form_debug_log.prg
                                    DoEvents()
                                    Return Nil
                                    })

      (This.Object):Event(90, {|ow,ky| // ON Release windows
                                       Local cm, ah := ow:Cargo:ahIcoLogo
                                       ow:Hide()
                                       DO EVENTS
                                       IF IsArray(ah)
                                          AEval(ah, {|h| DestroyIcon(h) })  // удалить хендлы иконок
                                       ENDIF
                                       cm := ProcNL()
                                       ?  cm, "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                                       ?  Repl(".", Len(cm)), "=> RELEASE WINDOW <=", ow:Name, ah
                                       ?? "... Program running time -", HMG_TimeMS( App.Cargo:tStart )
                                       DO EVENTS
                                       Return Nil
                                       })

      ON KEY F1     ACTION NIL
      ON KEY ESCAPE ACTION NIL

   END WINDOW

   ACTIVATE WINDOW wMain

RETURN NIL

*----------------------------------------------------------------------------*
INIT PROCEDURE Sets_ENV()
*----------------------------------------------------------------------------*
   LOCAL o, cIni  := hb_FNameExtSet( App.ExeName, ".ini" )
/*
   SET CODEPAGE TO RUSSIAN
   SET LANGUAGE TO RUSSIAN
*/
   rddSetDefault( "DBFCDX" )

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

   SET WINDOW MAIN OFF

   IF !HB_ISOBJECT( App.Cargo ) ; App.Cargo := oHmgData()
   ENDIF
   o := App.Cargo

   o:tStart         := hb_DateTime()       // start time
   o:cFontName      := "DejaVu Sans Mono"
   o:cFontName2     := "Comic Sans MS"
   o:nFontSize      := 14
   o:cLogFile       := "_msg.log"
   o:cIniFile       := cIni
   o:lLogDel        := .T.
   o:cDlgFont       := "DejaVu Sans Mono"
   o:nDlgSize       := o:nFontSize + 2
   o:aDlgBColor     := { 141, 179, 226 }     // Alert* BackColor
   o:aDlgFColor     := {  0 ,  0 ,  0  }     // Alert* FontColor
   o:aBClrMain      := {127,189,228}
   o:cDefAppIcon    := "1MG"
   o:lDebug         := .T.
   o:nMenuBmpHeight := 32
   o:cTitle         := PROGRAM
   o:cVersion       := PROGVER
   o:cLang          := LANG_PRG
   o:cAvtor         := "Copyright 2024 Verchenko Andrey + Sergej Kiselev"
   o:cEmail         := "<verchenkoag@gmail.com> Dmitrov, Moscow region / <bilance@bilance.lv>"
   o:cPathTemp      := GetUserTempFolder() + "\"
   o:cPathStart     := GetStartUpFolder()  + "\"
   o:cPathDbf       := GetStartUpFolder()  + "\DBF\"
   o:aDisplayMode   := { Sys.ClientWidth , Sys.ClientHeight }
   // задание своих параметров, позволяет протестировать на другие разрешения экрана
   // setting your parameters, allows you to test for other screen resolutions
   //o:aDisplayMode := { 1080 , 1080 }   // можно так задать
   o:cDisplayMode   := HB_NtoS(o:aDisplayMode[1]) + "x" + HB_NtoS(o:aDisplayMode[2])
   //o:cDebugLogRun := GetStartUpFolder() + "\BIN\notepad2.exe"  // можно так
   o:cDebugLogRun   := "notepad.exe"

   _SetGetLogFile( o:cLogFile )

   IF o:lLogDel ; hb_FileDelete( o:cLogFile )
   ENDIF

   IF o:lDebug ; SET LOGERROR ON
   ELSE        ; SET LOGERROR OFF
   ENDIF

   // Default font
   SET FONT TO o:cFontName , o:nFontSize
   // TsBrowse                                       bold italic
   _DefineFont("Normal"  , o:cFontName, o:nFontSize  , .F., .F. )
   _DefineFont("Bold"    , o:cFontName, o:nFontSize  , .T., .F. )
   _DefineFont("Italic"  , o:cFontName, o:nFontSize-2, .F., .T. )
   _DefineFont("ItalBold", o:cFontName, o:nFontSize-2, .T., .T. )
   _DefineFont("SpecHdr" , o:cFontName, o:nFontSize-2, .T., .T. )
   _DefineFont("TsbEdit" , "Arial"    , o:nFontSize  , .F., .T. )
   // Menu* font
   _DefineFont("ComSanMS" , o:cFontName2 , o:nFontSize+2 , .F., .F. )
   // Alert* font
   _DefineFont("DlgFont" , o:cDlgFont , o:nDlgSize   , .F., .F. )
   // Alert* colors
   SET MSGALERT BACKCOLOR  TO o:aDlgBColor
   SET MSGALERT FONTCOLOR  TO o:aDlgFColor
   //
   SET DEFAULT ICON TO o:cDefAppIcon
   SET WINDOW MODAL PARENT HANDLE ON
   SET TOOLTIPSTYLE BALLOON
   SET NAVIGATION EXTENDED
   SET MENUSTYLE  EXTENDED
   Set ShowRedAlert On        // увеличить фонт для окна "Program Error"

   // Проверка на запуск второй копии программы
   IF App.Cargo:cLang == "RU"
      o:aMsg := {"Попытка запуска второй копии программы:","Отказано в запуске."}
   ELSE
      o:aMsg := {"Attempting to launch a second copy of the program:","Start refused."}
   ENDIF
   _HMG_MESSAGE[4] := o:aMsg[1] + CRLF + App.ExeName + CRLF + o:aMsg[2] + CRLF + _HMG_MESSAGE[4]

   SET MULTIPLE QUIT WARNING  // окно большое

   SetMenuBitmapHeight( o:nMenuBmpHeight )

   ? PadC( " Program start - " + HB_TTOC( hb_DateTime() ) + " ", 80, "-" )
   ? " Screen resolution:", HB_NtoS(GetDesktopWidth())+" x "+HB_NtoS(GetDesktopHeight())
   ? "Free Open Software:", Version()
   ? "     Free Compiler:", hb_Ccompiler()
   ? "  Free Gui library:", MiniGuiVersion()

   o:cIniFile := cIni
   o:lIni     := hb_FileExists(cIni)
   // доступ к ини-файлу везде в программе - App.Cargo:oIni
   o:oIni := TIniData():New(cIni, .T.):Read()

   Default o:oIni:INFO := oHmgData()
   Default o:oIni:INFO:Developed_in   := MiniGUIVersion()
   Default o:oIni:INFO:xBase_compiler := Version()
   Default o:oIni:INFO:C_compiler     := Hb_Compiler()
   Default o:oIni:INFO:Programm       := o:cTitle
   Default o:oIni:INFO:ProgVers       := o:cVersion
   Default o:oIni:INFO:Avtor          := o:cAvtor
   Default o:oIni:INFO:Email          := o:cEmail

   Default o:oIni:MAIN := oHmgData()
   Default o:oIni:MAIN:ExeFile      := App.Exename
   Default o:oIni:MAIN:aBClrMain    := {215, 166, 0}
   Default o:oIni:MAIN:ComSanMS     := { o:cFontName2 , o:nFontSize+2 , .F., .F. }   // фонт главного верхнего меню
   Default o:oIni:MAIN:aWindow      := {0, 0, 0, 0}
   // TsBrowse
   Default o:oIni:TsBrowse := oHmgData()
   Default o:oIni:TsBrowse:Normal   := { o:cFontName, o:nFontSize  , .F., .F. }
   Default o:oIni:TsBrowse:Bold     := { o:cFontName, o:nFontSize  , .T., .F. }
   Default o:oIni:TsBrowse:Italic   := { o:cFontName, o:nFontSize-2, .F., .T. }
   Default o:oIni:TsBrowse:ItalBold := { o:cFontName, o:nFontSize-2, .T., .T. }
   Default o:oIni:TsBrowse:SpecHdr  := { o:cFontName, o:nFontSize-2, .T., .T. }
   Default o:oIni:TsBrowse:SuperHdr := { o:cFontName, o:nFontSize-2, .T., .F. }
   Default o:oIni:TsBrowse:Edit     := { o:cFontName, o:nFontSize+2, .F., .F. }
   //_o2log(o:oIni, 27, ProcNL() + "  o:oIni => ", .T. ) ; ?

   Default o:oIni:WinDebug := oHmgData()
   Default o:oIni:WinDebug:Rem := "Координаты окна отладки"
   Default o:oIni:WinDebug:nY  := 0
   Default o:oIni:WinDebug:nX  := 0

   IF !o:lIni
      // если нет файла, то создадим его
      o:oIni:cCommentBegin  := " Modify: " + hb_TtoC( hb_DateTime() )
      o:oIni:Write()  // НЕ UTF8, т.е. нет BOM на выходе
   ENDIF

   App.Cargo:WinDebug_nY := o:oIni:WinDebug:nY
   App.Cargo:WinDebug_nX := o:oIni:WinDebug:nX

   DO EVENTS

RETURN

//////////////////////////////////////////////////////////////////////////////////
FUNCTION Draw_Icon(owc)  // иконки на форме
   LOCAL nI, nX, nY, hIcon, cForm, nHIco, cMsg := ""
   LOCAL aIcon := { "iMg1x64", "iMg2x64", "iMg3x64" }

   owc:aIcoLogo    := aIcon  // сохраним для дальнейше работы
   owc:ahIcoLogo   := { 0, 0, 0 }
   owc:aIcoLogoYX  := { 0, 0, 0 }
   //owc:nYGaps    := 0           // отступ от начала формы по Y
   owc:nXGaps      := owc:nXGIco  // отступ от начала формы по X -> owc:nXGIco = см. главную форму
   cForm           := owc:cForm
   nHIco           := owc:nHIco
   nX              := owc:nXGaps
   nY              := owc:nYGaps

   ? ProcNL(), cForm
   IF cForm == NIL
      cMsg += "Error ! No variable - owc:cForm;;  "
      cMsg += ProcNL()
      ? cMsg
      AlertStop( cMsg )
      RETURN NIL
   ENDIF

   FOR nI := 1 TO LEN(aIcon)

      hIcon := LoadIconByName( aIcon[nI], nHIco, nHIco )
      ? "   .", nI, aIcon[nI], nHIco, nHIco, hIcon

      IF hIcon == 0
         cMsg += "Error ! No icon " + aIcon[nI]
         cMsg += " in program resources !;"
      ELSE
         DRAW ICON IN WINDOW &cForm AT nY, nX HICON hIcon ;
               WIDTH nHIco HEIGHT nHIco COLOR This.BackColor
      ENDIF
      ?? "nY=", nY, "nX=", nX
      owc:ahIcoLogo[nI]  := hIcon
      owc:aIcoLogoYX[nI] := { owc:nYGaps, nX }

      nX += nHIco

   NEXT

   owc:nWIcoEnd := owc:nXGaps + nHIco*3  // последняя координата кнопки

   IF LEN(cMsg) > 0
      AlertStop(cMsg)
      ? cMsg
   ENDIF

RETURN NIL

///////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Test(oWnd)
   LOCAL cFD := App.Cargo:cLogFile
   LOCAL a, hWnd, oIni, oSec, cSect, owc := oWnd:Cargo

   a := {   ;
         { 1, '&1. Menu   ', "iSanta1" , "Bold"    }, ;
         { 2, '&2. Menu   ', "iSanta1" , "Normal"  }, ;
         { 3, '&3. Menu   ', "iSanta1" , "Bold"    }, ;
         { 4, '&4. Menu   ', "iSanta1" , "Normal"  }, ;
         { 5, '&5. Menu   ', "iSanta1" , "Bold"    }, ;
         {  ,              ,           , {1,2,3}   }, ;
         {50, '&'+'Z. Menu', ""        , "Bold"    }  ;
        }

   ?v a 
   ? "  a= " + HMG_Arr2Txt(a)
   AlertDebug(a, "a= " + HMG_Arr2Txt(a))

   UPDATELOG(1)   // -> form_debug_log.prg
   hWnd := GetFormHandle( App.Cargo:aWinLog[1,1] )
   SwitchToThisWindow(hWnd)     // функция МиниГуи

   AlertDebug("Switch to the window with the file = " + cFD, hWnd, oWnd, App.Cargo)

   ? ProcNL(), "Output language in the program = ", App.Cargo:cLang

   ? ProcNL(), oWnd:Name, "owc=", owc

   // список переменных в oWnd:Cargo
   _o2log(owc, 17, ProcNL() + "  oWnd:Cargo => ", .T. )

   // обновить в окне файл _msg.log
   UPDATELOG(1)   // -> form_debug_log.prg

   AlertDebug( "Output to the debug window ! File = " + cFD, oWnd )

   //--------------------------------------------------
   hWnd := GetFormHandle( App.Cargo:aWinLog[2,1] )
   SwitchToThisWindow(hWnd)   // функция МиниГуи

   AlertDebug( "Switch to the window with the file demo2.ini", hWnd)

   oIni  := App.Cargo:oIni      // считаем ини-файла
   cSect := ProcName()          // новая секция
   oIni:Set(cSect, oHmgData())  // секцию добавили

   // записать иконки - пример
   oSec := oIni:Get(cSect)
   oSec:Set("aH1", owc:aIcoLogo   )
   oSec:Set("aH2", owc:ahIcoLogo  )
   oSec:Set("aH3", owc:aIcoLogoYX )

   // записать координаты окна WinDebug
   oIni:WinDebug:nY := App.Cargo:WinDebug_nY
   oIni:WinDebug:nX := App.Cargo:WinDebug_nX
   // записать изменения в файл
   oIni:cCommentBegin := " Modify: " + hb_TtoC( hb_DateTime() )
   oIni:Write()

   AlertDebug( "Contents demo2.ini=", oIni)

   // обновить в окне demo2.ini
   UPDATELOG(2)   // -> form_debug_log.prg

   AlertDebug( "Output to the debug window ! File= demo2.ini" )

   AlertDebug( "Close the program !" )

RETURN NIL
