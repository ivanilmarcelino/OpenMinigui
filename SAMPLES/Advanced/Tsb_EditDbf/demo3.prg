/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2025 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * _TBrowse() –абота с Dbf, таблица и карточка на “—Ѕ
 * _TBrowse()
 */
#define  _HMG_OUTLOG

#include "hmg.ch"
#include "tsbrowse.ch"

#define PROGRAM  "Demo TBrowse. Dbf"
#define PROGVER  "Version 1.01 (21.05.2025)"
#define PROGINF  "Test"

REQUEST DBFCDX

Function Main()
   LOCAL cForm := "wMain", aBClr := App.Cargo:aBClrMain
   LOCAL owc, nY, nX, nH, nW, nG := 20
   LOCAL cVal := MiniGuiVersion() + CRLF + Version() + CRLF + hb_Ccompiler()

   WaitWindow( {"... Wait for the preparation to complete ...", App.ExeName }, .T., 600, 16, NIL, WHITE, aBClr )
   App.Cargo:nMemoChar := CalcMemoLine()   // for function MEMOLINE(xxxx,App.Cargo:nMemoChar,1)
                                           // the sizes change after reboot
   IF !UseDbf()                            // открыть базы и считать справочник в массив
       QUIT                                // open databases and read the directory into an array
   ENDIF
   WaitWindow()

   nY := nX := nG

   // фонты дл€ программы задаютс€ в Sets_ENV() / fonts for the program are set in Sets_ENV()
   SET FONT TO _GetSysFont(), App.Cargo:nFontSize  // ->    o:nFontSize := ??

   // размеры экрана программы задаютс€ в Sets_ENV() / The program screen sizes are set in Sets_ENV()
   // можно протестировать любое разрешение экрана / you can test any screen resolution
   // o:aDisplayMode   := { 1440, 800 }
   nW := App.Cargo:aDisplayMode[1]
   nH := 64 + 5 * 2 + GetTitleHeight() + GetBorderHeight()

   DEFINE WINDOW &cForm AT 0,0 WIDTH nW HEIGHT nH TITLE App.Cargo:cTitle ;
          MAIN NOSIZE  TOPMOST BACKCOLOR aBClr          ;
          ON INIT    ( This.Topmost := .F., _wPost(0) ) ;
          ON RELEASE ( _wSend(90) )

      This.Cargo := oHmgData() ; owc := This.Cargo
      owc:cForm  := This.Name
      owc:aBClr  := This.Backcolor

      nW         := This.ClientWidth
      nH         := This.ClientHeight
      owc:nHMain := This.Height

      nY := 5
      DRAW ICON IN WINDOW &cForm AT nY, nW-60-10 PICTURE "1MG" WIDTH 64 HEIGHT 64 COLOR owc:aBClr

      @ nY, 5 LABEL Buff VALUE cVal WIDTH nW-60-nY*2 HEIGHT nH - nY*2 ;
        SIZE 12 FONTCOLOR YELLOW TRANSPARENT RIGHTALIGN

      @ nY, nX BUTTONEX Btn_Test WIDTH nH-5*2 HEIGHT nH-5*2 CAPTION "Test1" ;
        NOHOTLIGHT NOXPSTYLE HANDCURSOR ACTION {|| _wPost(1, , This.Name) }
      nX += This.Btn_Test.Width + 5

      @ nY, nX BUTTONEX Btn_Test2 WIDTH nH-5*2 HEIGHT nH-5*2 CAPTION "Test2" ;
        NOHOTLIGHT NOXPSTYLE HANDCURSOR ACTION {|| _wPost(2, , This.Name) }
      nX += This.Btn_Test2.Width + 5

      @ nY, nX BUTTONEX Btn_Exit WIDTH nH-5*2 HEIGHT nH-5*2 CAPTION "Exit" ;
        NOHOTLIGHT NOXPSTYLE HANDCURSOR ACTION {|| _wPost(99, , This.Name) }

      ON KEY F1     ACTION NIL
      ON KEY ESCAPE ACTION _wPost(99)

      WITH OBJECT This.Object
        :Event( 0, {|ow| DoEvents(), _wSend(1,ow,"Btn_Test"), ow:Setfocus("Buff") })

        :Event( 1, {|ow,ky,cn| _SetThisFormInfo(ow) , This.&(cn).Enabled := .F. ,;
                               This.Btn_Exit.Enabled := .F. ,;
                               Table_One(ow,ky,cn) ,;  // -> demo3_1Base.prg
                               _SetThisFormInfo() , This.&(cn).Enabled := .T.,;
                               This.Btn_Exit.Enabled := .T. , DoEvents() })

        :Event( 2, {|ow,ky,cn| _SetThisFormInfo(ow) , This.&(cn).Enabled := .F. ,;
                               This.Btn_Exit.Enabled := .F. ,;
                               Table_Two(ow,ky,cn) ,;  // -> demo3_2Base.prg
                               _SetThisFormInfo() , This.&(cn).Enabled := .T.,;
                               This.Btn_Exit.Enabled := .T. , DoEvents() })

        :Event(90, {|ow,ky| // ON Release windows
                            Local cm
                            ow:Hide()
                            DO EVENTS
                            cm := ProcNL()
                            ?  cm, "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                            ?  Repl(".", Len(cm)), "=> RELEASE WINDOW <=", ow:Name
                            ?? "... Program running time -", HMG_TimeMS( App.Cargo:tStart )
                            DO EVENTS
                            Return Nil
                            })

        :Event(99, {|ow| ow:Release() })
      END WITH

   END WINDOW

   //CENTER WINDOW &cForm
   ACTIVATE WINDOW &cForm

RETURN NIL

*----------------------------------------------------------------------------*
INIT PROCEDURE Sets_ENV()
*----------------------------------------------------------------------------*
   LOCAL o, cLog, cIni  := hb_FNameExtSet( App.ExeName, ".ini" )

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

   IF !HB_ISOBJECT( App.Cargo ) ; App.Cargo := oHmgData()
   ENDIF
   o := App.Cargo

   Set ShowRedAlert On        // увеличить фонт дл€ окна "Program Error"

   // ѕроверка на запуск второй копии программы
   _HMG_MESSAGE[4] := "Attempting to run a second copy of the program:" + CRLF + ;
                      App.ExeName + CRLF + ;
                      "Refused to start." + CRLF + _HMG_MESSAGE[4]
   SET MULTIPLE QUIT WARNING  // окно маленькое
   SET WINDOW MAIN OFF

   SET CODEPAGE TO RUSSIAN
   SET LANGUAGE TO RUSSIAN

   o:tStart         := hb_DateTime()        // start time
   o:cLogFile       := ChangeFileExt( App.ExeName, '.log' )
   // дл€ отладки - потом убрать
   cLog             := o:cLogFile
   //o:cLogFile       := cFilePath( cLog ) + "\"
   //o:cLogFile       += "_" + cFileNoPath( cLog )
   //
   o:tStart         := hb_DateTime()       // start time
   o:cIniFile       := cIni
   o:lLogDel        := .T.
   o:aDlgBColor     := {  5 , 191, 255 }     // Alert* BackColor
   o:aDlgFColor     := {  0 ,  0 ,  0  }     // Alert* FontColor
   o:aBClrMain      := {4,135,109}
   o:cDefAppIcon    := "1MG"
   o:lDebug         := .T.
   o:nMenuBmpHeight := 32
   o:aWinOpen       := {}
   o:cTitle         := PROGRAM + " ! " + PROGVER
   o:cVersion       := PROGVER
   o:cLang          := "EN"
   o:cAvtor         := "Copyright 2024 Verchenko Andrey + Sergej Kiselev"
   o:cEmail         := "<verchenkoag@gmail.com> Dmitrov, Moscow region / <bilance@bilance.lv>"
   o:cPrgInfo1      := "Many thanks for your help: Grigory Filatov <gfilatov@inbox.ru>"
   o:cPrgInfo2      := "Tips and tricks programmers from our forum http://clipper.borda.ru"
   o:cSiteDownload  := "Home page for download - http://www.hmgextended.com/"
   o:cAbout         := o:cTitle + ";;" + o:cAvtor + ";" + o:cEmail + ";;" + o:cPrgInfo1
   o:cAbout         += ";" + o:cPrgInfo2 + ";;" + o:cSiteDownload
   o:cPathTemp      := GetUserTempFolder() + "\"
   o:cPathDbf       := GetStartUpFolder() + "\DBF\"
   o:cPathStart     := GetStartUpFolder() + "\"
   //o:aDisplayMode := { System.DesktopWidth , System.DesktopHeight - GetTaskBarHeight() }
   o:aDisplayMode   := { Sys.ClientWidth , Sys.ClientHeight }
   // задание своих параметров, позвол€ет протестировать на другие разрешени€ экрана
   // setting your parameters, allows you to test for other screen resolutions
   //o:aDisplayMode   := { 1280 , 1280 }
   o:cDisplayMode   := HB_NtoS(o:aDisplayMode[1]) + "x" + HB_NtoS(o:aDisplayMode[2])
   o:cFontName      := "DejaVu Sans Mono"   // "Arial"
   o:cFontName2     := "Comic Sans MS"
   o:nFontSize      := 15
   o:cDlgFont       := "DejaVu Sans Mono"
   o:nDlgSize       := o:nFontSize + 2
   o:cTitle         += " " + o:cDisplayMode + "  FontSize: " + HB_NtoS(o:nFontSize)
   o:lPosWinOrTsb   := .T.   // позици€ окна по родит.окну / window position by parent window
   //o:lPosWinOrTsb := .F.   // позици€ окна по “—Ѕ / window position according to TSB

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
   _DefineFont("SuperHd" , o:cFontName2, o:nFontSize+2, .F., .F. )
   _DefineFont("TsbEdit" , "Arial"     , o:nFontSize  , .F., .T. )
   // Menu* font
   _DefineFont("ComSanMS"  , o:cFontName2 , o:nFontSize+2 , .F., .F. )         // фонт главного верхнего меню
   _DefineFont("MnNormal"  , o:cFontName  , o:nFontSize+2 , .F., .F. )         // фонт главного верхнего меню
   _DefineFont("MenuBtn"   , o:cFontName  , o:nFontSize   , .T., .F. )         // фонт кнопок верхнего меню
   _DefineFont("WinBtn"    , o:cFontName  , o:nFontSize+2 , .F., .F. )         // фонт кнопок окон
   _DefineFont("FntBtnMain", "Tahoma"     , 12            , .T., .F. )         // фонт кнопок главной формы
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

   ? PadC( " Program start - " + HB_TTOC( hb_DateTime() ) + " ", 80, "-" )
   ? " Screen resolution:", HB_NtoS(GetDesktopWidth())+" x "+HB_NtoS(GetDesktopHeight())
   ? "Free Open Software:", Version()
   ? "     Free Compiler:", hb_Ccompiler()
   ? "  Free Gui library:", MiniGuiVersion()
   ? ATREPL( "!", App.Cargo:cTitle, CRLF ) ; ?

RETURN
