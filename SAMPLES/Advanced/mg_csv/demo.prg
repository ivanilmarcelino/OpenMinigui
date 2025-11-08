/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2025 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * _TBrowse() Показ файлов .txt .csv .arr
 * Конвертация файлов в другие кодировки, включая UTF8
 * Экспорт файлов в .xls, .dbf, .arr
 * _TBrowse() Displaying .txt and .csv and .arr files
 * Convert files to other encodings, including UTF8
 * Export files to .xls, .dbf, and .arr
*/
#define  _HMG_OUTLOG
#include "hmg.ch"
#include "tsbrowse.ch"
#include "dbinfo.ch"

REQUEST HB_CODEPAGE_RU866, HB_CODEPAGE_RU1251, HB_CODEPAGE_RUKOI8, HB_CODEPAGE_RUISO
REQUEST HB_CODEPAGE_UA866, HB_CODEPAGE_UA1251, HB_CODEPAGE_UAKOI8, HB_CODEPAGE_UA1125
REQUEST HB_CODEPAGE_UTF8, HB_CODEPAGE_UTF8EX, HB_CODEPAGE_UTF16LE
REQUEST DBFCDX

#define PROGRAM  "MG CSV Viewer"
#define PROGVER  "Version 0.3 (09.10.2025)"
#define PROGINF  "files: *.txt *.csv *.arr"
///////////////////////////////////////////////////////////////////
FUNCTION MenuMainBtn()
   LOCAL oMenu := oHmgData()
   oMenu:aObj   := { "_Help" , "_Files"   , "_Table" , "_Config"  , "_Exit"   }
   oMenu:aIco   := { {"iCsv48x1","iCsv48x2"} , {"iFind48x1","iFind48x2"} ,;
                     {"iWin48x1","iWin48x2"} , {"iGear48x1","iGear48x2"} , {"iExit48x1","iExit48x2"} }
   oMenu:aMnRu  := { "Помощь", "Файлы"    , "Таблицы", "Настройки", "Выход"   }
   oMenu:aMnEn  := { "Help"  , "Files"    , "Tables" , "Settings" , "Exit"    }
   oMenu:aTipRu := { "Помощь", "Выбор файла"   , "Список таблиц" , "Настройки программы", "Выход из программы" }
   oMenu:aTipEn := { "Help"  , "File selection", "List of tables", "Program settings"   , "Exit program" }
   oMenu:aCap   := IIF( App.Cargo:lRu, oMenu:aMnRu , oMenu:aMnEn )
   oMenu:aTtip  := IIF( App.Cargo:lRu, oMenu:aTipRu, oMenu:aTipEn )
   oMenu:aFont  := { "Comic Sans MS", 12, .T., .F. , 14, "Increase button font size - reserve" }
   oMenu:aFClr  := { BLACK  , YELLOW }
   oMenu:aBClr  := { LGREEN , BLACK  }
   oMenu:nHIco  := 48          // 32,55  - задаём размер картинки на кнопке
   //oMenu:nHIco:= IIF( App.Cargo:aDisplayMode[2] <= 720, 28, oMenu:nHIco )
RETURN oMenu

///////////////////////////////////////////////////////////////////
Function Main()
   LOCAL cForm := "wMain" , aBClr := {168,251,181}
   LOCAL cVal := MiniGuiVersion() + CRLF + Version() + CRLF + hb_Ccompiler()
   LOCAL o, owc, nY, nX, nW, nH, nG, oMenu, nPost, nHIco, aFile

   aFile := CommandLine( hb_aParams() )
   nG    := 10
   nHIco := 96
   nH    := nHIco + nG
   nW    := App.Cargo:aDisplayMode[1]
   nPost := IIF( LEN(aFile) == 0, 0, 1 )

   DEFINE WINDOW &cForm CLIENTAREA nW, nH TITLE App.Cargo:cProga  ;
          MAIN NOMAXIMIZE NOSIZE TOPMOST BACKCOLOR aBClr          ;
          ON INIT    ( This.Topmost := .F., _wPost(nPost) )       ;
          ON RELEASE ( This.Hide, _wSend(90) )
          This.Cargo := oHmgData() ; o := This.Object ; owc := o:Cargo

      owc:aFile    := aFile
      owc:aBClr    := This.Backcolor
      owc:nIdWin   := LEN(owc:aFile)
      owc:ahIcoDel := {}                  // to remove icon handles from a form
      owc:nHMain   := nH + GetTitleHeight() + GetBorderHeight()
      App.Cargo:aBClr := This.Backcolor

      nY := nX := 5
      DRAW ICON IN WINDOW wMain AT nY, nW-nHIco-10 PICTURE "1MG" WIDTH nHIco HEIGHT nHIco COLOR aBClr

      @ nY+5, nX LABEL Buff VALUE cVal WIDTH nW-nHIco-nY*2 HEIGHT nH - nY*2 ;
        FONTCOLOR LGREEN TRANSPARENT RIGHTALIGN

      oMenu := MenuMainBtn()
      oMenu:aBClr := { owc:aBClr, LGREEN  }                  // change the background color of buttons
      TopMenuButtons(owc,oMenu,nG,nG,nHIco-nG,nHIco-nG,nG)   // menu_topButtons.prg

      ON KEY F1     ACTION NIL
      ON KEY ESCAPE ACTION ThisWindow.Release

      o:Event( 0, {|ow| ow:Setfocus("Buff"), DoEvents() })

      o:Event( 1, {|ow,ky| // processing files from the command line
                    Local i, j, cForm, aForm := {}
                    Local aFile := ow:Cargo:aFile
                    Local cFile, aDim, aLbl, cLbl, cTxt, cww
                    aDim := { cFileNoPath(App.Exename), "Loading file:", "..." }
                    cww  := WaitWindow( aDim, .T., 600, 16, NIL, WHITE, LGREEN )
                    aLbl := HMG_GetFormControls(cww, "LABEL")
                    FOR i := 1 TO Len(aLbl)
                        cTxt := GetProperty(cww, aLbl[ i ], "Value")
                        IF "..." $ cTxt
                           cLbl := aLbl[ i ]
                           EXIT
                        ENDIF
                    NEXT
                    j := 1
                    For i := 1 To Len(aFile)
                       cFile := aFile[ i ]
                       cTxt  := cFileNoPath(cFile)
                       IF !Empty(cLbl) ; SetProperty(cww, cLbl, "Value", cTxt)
                       ENDIF
                       DO EVENTS
                       cForm := Table_Csv(ow, j, cFile, ky)      // demo_win_tsb.prg
                       IF LEN(cForm) > 0 ; AADD(aForm, cForm) ; j++
                       ENDIF
                       DO EVENTS
                    Next
                    WaitWindow()
                    wApi_Sleep(100)
                    IF LEN(aForm) > 0
                       // activate all windows
                       _ActivateWindow( aForm, .F., , )
                    ENDIF
                    Return Nil
                    })

       o:Event({11,"_Help"}, {|ow,ky,cn| //
                                         SET WINDOW THIS TO ow
                                         ky := HelpThis()      // demo_add.prg
                                         AlertInfo(ky,"About",App.Cargo:cDefAppIcon,64,{RED})
                                         SET WINDOW THIS TO
                                         ow:Enabler(cn, .T.)
                                         ow:Setfocus('Buff')
                                         Return Nil
                                         } )

       o:Event({12,"_Files"}, {|ow,ky,cn| //
                                          Local i, cFrm, aForm := {}
                                          Local cTtl  := "Select CSV file"
                                          Local cPath := App.Cargo:cPathCsv
                                          Local owc   := ow:Cargo
                                          Local aDim, cFile, aF, aTxt := {}
                                          SET WINDOW THIS TO ow
                                          aF := GetFile( { {"CSV files", "*.txt;*.csv;*.arr;*.y*"}, {"All files", "*.*"} }, cTtl, cPath, .T. )
                                          SET WINDOW THIS TO
                                          IF LEN(aF) > 0
                                             aTxt := ARRAY(Len(aF))
                                             For i := 1 To Len(aF)
                                                aTxt[i] := cFileNoPath(aF[i])
                                             Next
                                             aDim := { cFileNoPath(App.Exename), "Loading file:" }
                                             aMerge( aDim, aTxt )
                                             WaitWindow( aDim, .T., 600, 16, NIL, WHITE, LGREEN )
                                             For i := 1 To Len(aF)
                                                cFile := aF[i]
                                                owc:nIdWin ++ // ID нового окна / new window
                                                cFrm := Table_Csv(ow, owc:nIdWin, cFile, ky)  // demo_win_tsb.prg
                                                IF LEN(cFrm) > 0 ; AADD(aForm, cFrm)
                                                ENDIF
                                                DO EVENTS
                                             Next
                                             WaitWindow()
                                             wApi_Sleep(100)
                                          ENDIF
                                          ow:Enabler(cn, .T.)
                                          ow:Setfocus('Buff')
                                          IF LEN(aForm) > 0
                                             // activate all windows
                                             _ActivateWindow( aForm, .F., , )
                                          ENDIF
                                          Return Nil
                                          } )

       o:Event({13,"_Table"}, {|ow,ky,cn| //
                                         SET WINDOW THIS TO ow
                                         ky := Table_List(ow, ky, cn) // demo_add.prg
                                         SET WINDOW THIS TO
                                         ow:Enabler(cn, .T.)
                                         If ky > 0
                                            ow:Setfocus('Buff')
                                         Endif
                                         Return Nil
                                         } )

       o:Event({14,"_Config"}, {|ow,ky,cn| //
                                         SET WINDOW THIS TO ow
                                         Table_Config(ow, ky, cn, This.&(cn).Caption)  // demo_add.prg
                                         SET WINDOW THIS TO
                                         ow:Enabler(cn, .T.)
                                         ow:Setfocus('Buff')
                                         Return Nil
                                         } )

       o:Event({15,"_Exit"}, {|ow,ky,cn| _LogFile(.T., "  -->> Button:",cn, ow:Name, ky ) ,;
                                         _wSend(99,ow:Name) } )

       o:Event(90, {|ow,ky| // ON Release windows
                            Local cm, ct, ah
                            cm := ProcNL()
                            ct := HMG_TimeMS( App.Cargo:tStart )
                            ?  cm, "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                            ?  Repl(".", Len(cm)), "=> RELEASE WINDOW <=", ow:Name
                            ?? "... Program running time -", ct
                            ? Repl(".", 10),"Delete handle icon - ow:Cargo:ahIcoDel="
                            ?? ah, HB_ValToExp(ah)
                            IF IsArray(ah)
                               AEval(ah, {|h| DestroyIcon(h) })  // удалить хендлы иконок
                            Endif
                            DO EVENTS
                            Return Nil
                            })

      o:Event({97, "ReStart"}, {|ow,ky,xp|              // App.ExeName restart
                                          Local cc
                                          cc := "_ReStart_" + hb_TtoS(hb_DateTime())
                                          cc += " " + iif( xp == NIL, "0", hb_valtoexp(xp) )
                                          ? ; ? ProcNL(), "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                                          ShellExecute( , 'open', App.ExeName, , , SW_SHOWNORMAL)
                                          DO EVENTS ; wApi_Sleep(150)
                                          _wPost(99)
                                          Return Nil
                                          })

      o:Event(99, {|ow| ow:Release() })

   END WINDOW

   // CENTER WINDOW &cForm
   ACTIVATE WINDOW &cForm

RETURN NIL

*----------------------------------------------------------------------------*
INIT PROCEDURE Sets_ENV()
*----------------------------------------------------------------------------*
   LOCAL o, aFont, cFont := "DejaVu Sans Mono", nSize := 12, lDebug := .F.
   LOCAL cLog := hb_FNameExtSet( App.ExeName, '.log' )
   LOCAL cIni := hb_FNameExtSet( App.ExeName, ".ini" )

   //SET CODEPAGE TO ENGLISH
   //SET LANGUAGE TO ENGLISH
   SET CODEPAGE TO RUSSIAN
   SET LANGUAGE TO RUSSIAN

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
   SET TOOLTIPSTYLE BALLOON
   //
   IF !HB_ISOBJECT( App.Cargo ) ; App.Cargo := oHmgData()
   ENDIF
   o := App.Cargo

   Set ShowRedAlert On

   //SET MULTIPLE QUIT WARNING - не надо
   SET WINDOW MAIN OFF

   SET DEFAULT ICON TO "iCsv64"
   SET WINDOW MODAL PARENT HANDLE ON
   SET TOOLTIPSTYLE BALLOON
   SET NAVIGATION EXTENDED
   SET MENUSTYLE  EXTENDED
   // Alert* colors
   SET MSGALERT BACKCOLOR  TO  {168,251,181}
   SET MSGALERT FONTCOLOR  TO  { 0 ,  0, 0 }
   //
   _SetGetLogFile( cLog ) ; hb_FileDelete( cLog ) ; SET LOGERROR ON
   //
   o:aBCAlert       := {255,114,255}
   o:tStart         := HB_DATETIME()
   o:cDefAppIcon    := "iCsv64"
   o:cLang          := "EN"
   o:lRu            := IIF( o:cLang == "RU", .T., .F.)
   o:lDebug         := lDebug
   o:cTitle         := PROGRAM + " " + PROGINF
   o:cVersion       := PROGVER
   o:cProga         := PROGRAM + " " + PROGINF + "  " + PROGVER
   o:cAvtor1        := "Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com> "
   o:cAvtor2        := "Copyright 2025 Sergej Kiselev   <bilance@bilance.lv> "
   o:cEmail         := "<verchenkoag@gmail.com> Dmitrov, Moscow region / <bilance@bilance.lv>"
   o:cPrgInfo1      := "Many thanks for your help: Grigory Filatov <gfilatov@inbox.ru>"
   o:cPrgInfo2      := "Tips and tricks programmers from our forum http://clipper.borda.ru"
   o:cSiteDownload  := "Home page for download - http://www.hmgextended.com/"
   o:cInfo1Ru       := "_TBrowse() Показ файлов .txt .csv .arr"
   o:cInfo2Ru       := "Конвертация файлов в другие кодировки, включая UTF8"
   o:cInfo3Ru       := "Экспорт файлов в .xls, .dbf, .arr"
   o:cInfo1En       := "_TBrowse() Displaying .txt and .csv and .arr files"
   o:cInfo2En       := "Convert files to other encodings, including UTF8"
   o:cInfo3En       := "Export files to .xls, .dbf, and .arr"
   o:cPathStart     := GetUserTempFolder() + "\"
   o:cPathCsv       := GetStartUpFolder() + "\CSV\"
   //o:aDisplayMode := { System.DesktopWidth , System.DesktopHeight - GetTaskBarHeight() }
   o:aDisplayMode   := { Sys.ClientWidth , Sys.ClientHeight }
   // задание своих параметров, позволяет протестировать на другие разрешения экрана
   // setting your parameters, allows you to test for other screen resolutions
   //o:aDisplayMode := { 1280 , 1280 }
   //o:aDisplayMode := { 1280 , 680 }  // дисплей Сергея
   o:cDisplayMode   := HB_NtoS(o:aDisplayMode[1]) + "x" + HB_NtoS(o:aDisplayMode[2])
   o:lColumnWidthFull := .F.          // ширина колонок полная по тексту - отключить
   //
   o:cIniFile := cIni
   o:lIni     := hb_FileExists(cIni)
   // доступ к ини-файлу везде в программе - App.Cargo:oIni
   o:oIni := TIniData():New(cIni, .T.):Read()
   //
   IF     Sys.DesktopWidth >= 1920 ; nSize += 4
   ELSEIF Sys.DesktopWidth >  1280 ; nSize += 2
   ENDIF
   //
   SET FONT TO cFont, nSize
   IF ! o:lIni
      _DefineFont("Normal"  , cFont            , nSize  , .F., .F. )
      _DefineFont("Bold"    , "Times New Roman", nSize-2, .T., .F. )
      _DefineFont("Italic"  , "Tahoma"         , nSize-4, .F., .T. )
      _DefineFont("SpecHdr" , cFont            , nSize-4, .T., .T. )
      _DefineFont("SuperHdr", cFont            , nSize  , .T., .F. )
      _DefineFont("TsbEdit" , "Arial"          , nSize  , .F., .T. )
      // Alert* font
      _DefineFont("DlgFont" , cFont, nSize+2, .F., .F. )
      // Menu* font
      _DefineFont("ComSanMS", "Comic Sans MS", nSize+2 , .F., .F. )
   ELSE
      aFont := o:oIni:TsBrowse:Normal
      _DefineFont("Normal"  , aFont[1], aFont[2] , aFont[3], aFont[4] )
      aFont := o:oIni:TsBrowse:Bold
      _DefineFont("Bold"    , aFont[1], aFont[2] , aFont[3], aFont[4] )
      aFont := o:oIni:TsBrowse:Italic
      _DefineFont("Italic"  , aFont[1], aFont[2] , aFont[3], aFont[4] )
      aFont := o:oIni:TsBrowse:SpecHdr
      _DefineFont("SpecHdr"  , aFont[1], aFont[2] , aFont[3], aFont[4] )
      aFont := o:oIni:TsBrowse:SuperHdr
      _DefineFont("SuperHdr" , aFont[1], aFont[2] , aFont[3], aFont[4] )
      aFont := o:oIni:TsBrowse:Edit
      _DefineFont("TsbEdit"  , aFont[1], aFont[2] , aFont[3], aFont[4] )
      // Alert* font
      aFont := o:oIni:MAIN:DlgFont
      _DefineFont("DlgFont" , aFont[1], aFont[2] , aFont[3], aFont[4] )
      // Menu* font
      aFont := o:oIni:MAIN:ComSanMS
      _DefineFont("ComSanMS", "Comic Sans MS", nSize+2 , .F., .F. )
      // зададим язык из ини-файла
      App.Cargo:cLang := o:oIni:MAIN:cLang         // язык интерфейса программы
      o:lRu           := IIF( o:cLang == "RU", .T., .F.)
      App.Cargo:lColumnWidthFull := o:oIni:MAIN:lColumnWidthFull
      Default App.Cargo:lColumnWidthFull := .F.
   ENDIF
   //
   //_HMG_bOnErrorInit := {|cMsg,oErr,cTxt,cErr| my_ErrorExit(cMsg,oErr,cTxt,cErr) }
   //_HMG_bOnErrorExit := {|| my_ErrorExit() }
   //
   ALTD(iif( lDebug, 1, 0 ))       // 1 - debug mode, 0 - no debug mode
   //
   ? REPL("=",20) + " Program start - " + HB_TTOC( HB_DATETIME() ) + " " + REPL("=",20)
   ? MiniGuiVersion() , MiniGuiVersionNumba()
   ? Version(), hb_Ccompiler() ; ? "lDebug=", lDebug
   ? "Computer/User: " + NetName() + "/" + hb_UserName()
   //
   Default o:oIni:INFO := oHmgData()
   Default o:oIni:INFO:Developed_in   := MiniGUIVersion()
   Default o:oIni:INFO:xBase_compiler := Version()
   Default o:oIni:INFO:C_compiler     := Hb_Compiler()
   Default o:oIni:INFO:Programm       := o:cTitle
   Default o:oIni:INFO:ProgVers       := o:cVersion
   Default o:oIni:INFO:Avtor1         := o:cAvtor1
   Default o:oIni:INFO:Avtor2         := o:cAvtor2
   //
   Default o:oIni:MAIN := oHmgData()
   Default o:oIni:MAIN:aBClrMain      := {215, 166, 0}
   Default o:oIni:MAIN:ComSanMS       := GetFontParam(GetFontHandle("ComSanMS" ))
   Default o:oIni:MAIN:DlgFont        := GetFontParam(GetFontHandle("DlgFont" ))
   Default o:oIni:MAIN:Window         := {0, 0, 0, 0}            // координаты окна
   Default o:oIni:MAIN:cLang          := App.Cargo:cLang         // язык интерфейса программы
   Default o:oIni:MAIN:lColumnWidthFull := .F.                   // ширина колонок полная по тексту - отключить
   Default App.Cargo:lColumnWidthFull   := .F.

   // TsBrowse
   Default o:oIni:TsBrowse := oHmgData()
   Default o:oIni:TsBrowse:Normal   := GetFontParam(GetFontHandle("Normal"  ))
   Default o:oIni:TsBrowse:Bold     := GetFontParam(GetFontHandle("Bold"    ))
   Default o:oIni:TsBrowse:Italic   := GetFontParam(GetFontHandle("Italic"  ))
   Default o:oIni:TsBrowse:SpecHdr  := GetFontParam(GetFontHandle("SpecHdr" ))
   Default o:oIni:TsBrowse:SuperHdr := GetFontParam(GetFontHandle("SuperHdr"))
   Default o:oIni:TsBrowse:Edit     := GetFontParam(GetFontHandle("TsbEdit" ))
   //                    cell     Head   foot      SpecHider   SuperHider   Edit
   //oTsb:aFont   := { "Normal", "Bold", "Italic", "SpecHdr" , "SuperHdr", "TsbEdit" }

   IF ! o:lIni
       // запись в ини-файл
       o:oIni:cCommentBegin := " Modify: " + hb_TtoC( hb_DateTime() )
       o:oIni:Write()  // NOT UTF8, i.e. no BOM in the output
   ENDIF

   //New colors and icons in the Alert*() function
   oAlert() // demo_add.prg 

RETURN


