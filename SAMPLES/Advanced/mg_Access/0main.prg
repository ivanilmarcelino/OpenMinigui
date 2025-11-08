/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2024 Grigory Filatov <gfilatov@inbox.ru>
 * Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 * Copyright 2024 Sergej Kiselev <bilance@bilance.lv>
 *
 * Просмотр Access файлов / Viewing Access files
*/
#define _HMG_OUTLOG

#include "hmg.ch"

REQUEST HB_CODEPAGE_RU866, HB_CODEPAGE_RU1251, HB_CODEPAGE_RUKOI8, HB_CODEPAGE_RUISO
REQUEST HB_CODEPAGE_UA866, HB_CODEPAGE_UA1251, HB_CODEPAGE_UAKOI8, HB_CODEPAGE_UA1125
REQUEST HB_CODEPAGE_UTF8, HB_CODEPAGE_UTF8EX, HB_CODEPAGE_UTF16LE
REQUEST DBFNTX, DBFDBT, DBFCDX, DBFFPT, SIXCDX, DBFNSX

#define PROGRAM  "MG Access files (*.mdb *.accdb) ADO"
#define PROGVER  "Version 0.72 (22.10.2024)"
#define LANG_PRG "EN" // English interface-lang
//////////////////////////////////////////////////////////////////////////////
FUNCTION Main()
   LOCAL cForm := "wMain"
   LOCAL nY, nX, nH, nW, nG, a4Clr, aBClr, aFClr, o, owc

   nY    := nX := 0  ; nG := 20
   nW    := Sys.ClientWidth
   nH    := App.Cargo:nHMain
   aBClr := {255, 151, 197}     // светло-красный
   aFClr := MAROON
   a4Clr := { {197,17,98}, {227,92,28}, {244,244,244} , {240,128,128} }
   a4Clr := { {197,17,98}, {133,6,63} , {244,244,244} , {223,225,230} }
   a4Clr := { {197,17,98}, {133,6,63} , {244,244,244} , {247,196,196} }
   App.Cargo:a4Clr      := a4Clr                // записать цвета для окон таблицы
   App.Cargo:aBCAlert   := aBClr                // записать цвета для Alert*() и другие окна
   App.Cargo:aDlgBColor := aBClr                // Alert* BackColor
   App.Cargo:aWinOpen   := {}                   // здесь будут записываться открытые окна таблицы
   App.Cargo:aRSet      := {}                   // здесь будут записываться открытые таблицы Access

   SET MSGALERT BACKCOLOR TO App.Cargo:aBCAlert

   DEFINE WINDOW &cForm AT nY, nX WIDTH nW HEIGHT nH TITLE App.Cargo:cProga ;
      MAIN NOSIZE TOPMOST                                                   ;
      BACKCOLOR aBClr                                                       ;
      ON INIT    _wPost( 0)                                                 ;
      ON RELEASE _wSend(90)

      App.Cargo:nHMain := This.Height + GetBorderHeight() + 3 // высота всего окна
      This.Cargo := oHmgData() ; owc := This.Cargo  // для окна создаем объект без переменных (условно пустой)
      owc:aBColor   := This.BackColor   // цвет окна
      owc:aBClrPsw  := {247,196,196}    // цвет окна ввода пароля
      owc:cForm     := cForm
      owc:nG        := nG
      owc:cPath     := MiniGuiVersion()
      owc:cVers     := App.Cargo:cPathDbf
      owc:cInfo     := ""
      owc:aLabel    := { "Lbl_1", "Lbl_2", "Lbl_3" }
      owc:cFile     := ""                            // путь файла к файлу access
      owc:cLine3    := ""                            // 3 строка

      // верхнее меню окна
      myToolBar(owc)
      nY  := 0
      nX  := nG
      This.Height := owc:nHEndTB + GetBorderHeight()*2 //+ GetTitleHeight()

      nW  := This.ClientWidth
      nH  := This.ClientHeight 
      owc:nWLbl   := owc:nWEndTB      // последняя координата кнопок
      owc:nH3Line := INT( nH / 3 ) 
      owc:nFSize  := INT( owc:nH3Line / 1.7 )

      @ nY, owc:nWLbl  LABEL Lbl_1 VALUE owc:cPath WIDTH nW-owc:nWLbl HEIGHT owc:nH3Line SIZE owc:nFSize FONTCOLOR aFClr VCENTERALIGN TRANSPARENT
      nY += This.Lbl_1.Height 
      @ nY, owc:nWLbl LABEL Lbl_2 VALUE owc:cVers  WIDTH nW-owc:nWLbl HEIGHT owc:nH3Line SIZE owc:nFSize FONTCOLOR aFClr VCENTERALIGN TRANSPARENT
      nY += This.Lbl_2.Height 
      @ nY, owc:nWLbl LABEL Lbl_3 VALUE owc:cInfo  WIDTH nW-owc:nWLbl HEIGHT owc:nH3Line SIZE owc:nFSize FONTCOLOR aFClr VCENTERALIGN  TRANSPARENT

      owc:nHMain  := This.Height

      ON KEY F1     ACTION _wPost(9)
      ON KEY ESCAPE ACTION _wPost(99)

      o := This.Object
      o:Event( 0, {|ow| This.Topmost := .F., _LogFile(.T., ProcNL(),">>> Start programm ! Window: "+ow:Name) ,;
                        This.&("_Table").Enabled  := .F.  })

      o:Event({ 9,"_Help"  }, {|ow,ky,cn| This.&(cn).Enabled := .F. , _SetThisFormInfo(ow) ,;
                                          MsgAbout(,,,ky,cn), _SetThisFormInfo(),;
                                          This.&(cn).Enabled := .T. , ow:SetFocus('Lbl_1')   } )

      o:Event({10,"_Files" }, {|ow,ky,cn| // кнопка - путь к Access-файлу
                                          Local cTtl := "Select Access file"
                                          Local cPath := App.Cargo:cPathDbf
                                          Local owc   := ow:Cargo
                                          Local aObj  := owc:aLabel
                                          Local aF, lOpen, cForm := ow:Name
                                          This.&(cn).Enabled := .T.
                                          SET WINDOW THIS TO ow:Name    // ОБЯЗАТЕЛЬНО !!!
                                          aF := GetFile( { {"Access Files", "*.mdb;*.accdb"} }, cTtl, cPath, .T., .T. )
                                          SET WINDOW THIS TO
                                          IF LEN(aF) > 0
                                             ? "++++++++++", ProcNL(), LEN(App.Cargo:aWinOpen), HB_ValToExp(App.Cargo:aWinOpen)
                                             IF LEN(App.Cargo:aWinOpen) > 0
                                                //  закрыть открытые окна с таблицами
                                                AEval(App.Cargo:aWinOpen, {|aw| Domethod(aw,"Release") })
                                                App.Cargo:aWinOpen := {}   // здесь будут записываться открытые окна таблицы
                                             ENDIF
                                             ? "++++++++++", ProcNL(), LEN(App.Cargo:aRSet)
                                             IF LEN(App.Cargo:aRSet) > 0
                                                // закрыть рабочие области Access
                                                AEval(App.Cargo:aRSet, {|rs| rs:Close() })
                                                App.Cargo:oConx:Close()
                                                App.Cargo:oTbl:Close()
                                             ENDIF
                                             //
                                             ow:Cargo:cFile := aF[1]       // исправили путь файла
                                             SET WINDOW THIS TO ow:Name    // ОБЯЗАТЕЛЬНО !!!
                                                 // меню пароля и открытия файла
                                                 lOpen := Menu2OpenFile(ow)         // -> 2openAccess.prg
                                             SET WINDOW THIS TO
                                             ? ProcNL()+"########", "["+owc:cFile+"]", ky, "lOpen=", lOpen
                                             IF lOpen
                                                This.&("_Table").Enabled  := .T.  // разблокировать кнопку
                                                // перепоказать на форме для TOOLBAREX
                                                SetProperty(cForm,aObj[1],"Visible", .T.)
                                                SetProperty(cForm,aObj[2],"Value", owc:cFile)
                                                SetProperty(cForm,aObj[3],"Value", owc:cLine3)
                                                SetProperty(cForm,aObj[3],"Visible", .T.)
                                             ENDIF
                                          ENDIF
                                          This.&(cn).Enabled := .T.
                                          ow:Setfocus('Lbl_1')
                                          DO EVENTS
                                          IF LEN(aF) > 0 .AND. lOpen // файл открылсф
                                             _wSend("_Table",ow)     // вызов кнопки Таблицы
                                          ENDIF
                                          Return Nil
                                          } )

      o:Event({20,"_Table" }, {|ow,ky,cn| /*This.&(cn).Enabled := .F. ,*/ _SetThisFormInfo(ow) ,;
                                         Menu3Table(ow,ky,cn) ,;
                                         _SetThisFormInfo()/*, This.&(cn).Enabled := .T.*/ , ow:SetFocus('Lbl_1') } )

      o:Event({30,"_Config"}, {|ow,ky,cn| This.&(cn).Enabled := .F. , _SetThisFormInfo(ow)      ,;
                                          Menu4Config(ow,ky,cn) /*MsgDebug(ow:Name,ky,cn)*/     ,;
                                          _SetThisFormInfo()        , This.&(cn).Enabled := .T. ,;
                                          ow:Setfocus('Lbl_1')  } )

      o:Event({89,"_Exit"  }, {|ow| _LogFile(.T., ProcNL(),">>> Exit button pressed! Window: "+ow:Name), _wSend(99) } )

      o:Event(90, {|ow,ky| // ON Release
                           Local i, aFile, aDim, cPath := cFilePath( App.Exename) + "\"
                           ? "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                           ?  Repl(".", 10), "=> RELEASE WINDOW <=", ow:Name
                           _wSend(94, ow)   // Закрыть все таблицы
                           // После работы надо закрывать рекордсет и соединение методом Close()
                           ?? "aRSet:Close()"
                           AEval(App.Cargo:aRSet, {|rs| rs:Close() })  // <<-- закрыть TOleAuto():new()
                           ? "App.Cargo:aRSet=",App.Cargo:aRSet
                           ? "App.Cargo:oConx=",App.Cargo:oConx
                           If IsObject(App.Cargo:oConx) 
                               App.Cargo:oConx:Close()                   
                           Endif
                           ?? "... Program running time -", HMG_TimeMS( App.Cargo:tStart )
                           aFile := Directory( cPath + "*.ldb"     )  // считываю текущую папку
                           aDim  := Directory( cPath + "*.laccdb"  )  // считываю текущую папку
                           aMerge( aFile, aDim )
                           For i := 1 To Len(aFile)
                               ? i, "Delete file - ", cPath + aFile[i,1]
                               hb_FileDelete( cPath + aFile[i,1] )
                               DO EVENTS
                               ?? IIF( FILE(cPath + aFile[i,1]), "не удалил!", "удалил")
                           Next
                           Return Nil
                           })

      o:Event(91, {|  | ReleaseAllWindows () })
      o:Event(92, {|ow| _wSend(99, ow)       })
      o:Event(93, {|ow| // Закрыть все таблицы / Close all tables
                        Local cFrm
                        FOR EACH cFrm IN HMG_GetForms()
                            IF cFrm == ow:Name ; LOOP
                            ENDIF
                            _wSend(99, cFrm)
                            DO EVENTS ; wApi_Sleep(100)
                        NEXT
                        Return Nil
                        })
      o:Event(94, {|ow| // Закрыть все таблицы / Close all tables
                        Local i, cFrm, aFrm := HMG_GetForms()
                        FOR i := Len(aFrm) TO 1 STEP -1
                            cFrm := aFrm[ i ]
                            IF cFrm == ow:Name ; LOOP
                            ENDIF
                            _wSend(99, cFrm)
                            DO EVENTS ; wApi_Sleep(100)
                        NEXT
                        Return Nil
                        })
      o:Event(99, {|ow| ow:Release()         })

   END WINDOW

   //CENTER WINDOW &cForm
   ACTIVATE WINDOW &cForm

RETURN NIL

*-----------------------------------------------------------------------------------------*
INIT PROCEDURE Sets_ENV()   // стартует всегда перед MAIN() / always starts before MAIN()
*-----------------------------------------------------------------------------------------*
   LOCAL o, cIni  := hb_FNameExtSet( App.ExeName, ".ini" )

   SET CODEPAGE TO RUSSIAN
   SET LANGUAGE TO RUSSIAN

   //SET CODEPAGE TO ENGLISH       // аналог HB_CDPSELECT( "EN" )
   //SET LANGUAGE TO ENGLISH       // аналог HB_LANGSELECT( "EN" )

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
   SET TOOLTIPSTYLE BALLOON

   SET WINDOW MAIN OFF

   IF !HB_ISOBJECT( App.Cargo ) ; App.Cargo := oHmgData()
   ENDIF
   o := App.Cargo

   o:tStart         := hb_DateTime()   // start time
   o:cLang          := LANG_PRG
   o:cFontName      := "DejaVu Sans Mono" //"Arial"
   o:cFontName2     := "Comic Sans MS"
   o:nFontSize      := 13
   o:cLogFile       := "_msg.log"
   o:cIniFile       := cIni
   o:lLogDel        := .T.
   o:lDebug         := .T.
   o:cDlgFont       := "DejaVu Sans Mono"
   o:nDlgSize       := o:nFontSize + 2
   o:aDlgBColor     := { 233, 204, 249 }     // Alert* BackColor
   o:aDlgFColor     := {  0 ,  0 ,  0  }     // Alert* FontColor
   o:aBClrMain      := { 184, 107, 228 }
   o:cDefAppIcon    := "1MG"
   o:nMenuBmpHeight := 32
   o:cTitle         := PROGRAM
   o:cTitleRu       := SUBSTR( PROGRAM, 1, AT("/",PROGRAM ) - 1 )
   o:cTitleEn       := SUBSTR( PROGRAM, AT("/",PROGRAM ) + 2 )
   o:cTitleWin      := PROGRAM //IIF(o:cLang == "RU", o:cTitleRu, o:cTitleEn )
   o:cVersion       := PROGVER
   o:cProga         := o:cTitleWin + SPACE(3) + PROGVER
   o:cAvtor         := "Copyright 2024 Verchenko Andrey + Grigory Filatov + Sergej Kiselev"
   o:cEmail         := "<verchenkoag@gmail.com> / <gfilatov@inbox.ru> / <bilance@bilance.lv>"
   o:cPrgInfo1      := "Many thanks for your help: Grigory Filatov <gfilatov@inbox.ru>"
   o:cPrgInfo2      := "Tips and tricks programmers from our forum http://clipper.borda.ru"
   o:cSiteDownload  := "Home page for download - http://www.hmgextended.com/"
   o:cPathTemp      := GetUserTempFolder() + "\"
   o:cPathDbf       := GetStartUpFolder() + "\Access\"
   o:aDisplayMode   := { Sys.ClientWidth , Sys.ClientHeight }
   // задание своих параметров, позволяет протестировать на другие разрешения экрана
   // setting your parameters, allows you to test for other screen resolutions
   //o:aDisplayMode   := { 1280 , 1280 }
   o:cDisplayMode   := HB_NtoS(o:aDisplayMode[1]) + "x" + HB_NtoS(o:aDisplayMode[2])
   o:nHMain         := 100   // высота окна главной формы
   o:aWinOpen       := {}    // общий-список-окон-этой-программы

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
   _DefineFont("SpecHdr" , o:cFontName, o:nFontSize-3, .T., .T. )
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
   _HMG_MESSAGE[4] := "Attempting to run a second copy of the program:" + CRLF + ;
                      App.ExeName + CRLF + ;
                      "Refused to start !" + CRLF + _HMG_MESSAGE[4]
   SET MULTIPLE QUIT WARNING  // окно маленькое

   SetMenuBitmapHeight( o:nMenuBmpHeight )

   PUBLIC nOperat, cOperator, nPubYear
   M->nOperat     := 20
   M->cOperator   := "User-Test"
   M->nPubYear    := YEAR(DATE())

   ? PadC( " Program start - " + HB_TTOC( hb_DateTime() ) + " ", 80, "-" )
   ? " Screen resolution:", HB_NtoS(GetDesktopWidth())+" x "+HB_NtoS(GetDesktopHeight())
   //?? "LargeFontsMode()=", HB_NtoS( LargeFontsMode() )
   ? "Free Open Software:", MiniGuiVersion()
   ? "     Free Compiler:", hb_Ccompiler()
   ? "  Free Gui library:", Version()

   o:cIniFile := cIni
   o:lIni     := hb_FileExists(cIni)
   // доступ к ини-файлу везде в программе - App.Cargo:oIni
   //o:oIni := TIniData():New(cIni, .T.):Read()

   //_o2log(o:oIni, 27, ProcNL() + "  o:oIni => ", .T. ) ; ?

RETURN

///////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myToolBar(oWC)
   LOCAL nW, nH, nX, hFont, aFont, cFont, nFSize, lBold, nHImg, aImg
   LOCAL aImg1, aObj, aCap, hIco, hBmp, aFrmt, cFile, cPath, aBtnObj
   LOCAL nWBtn, nHBtn, cCap, aTip, nWtxt, nWCap, cObj, cForm, i, o

   ? ProcNL(), "oWC=", oWC

   cForm  := oWC:cForm                   // имя окна
   hFont  := GetFontHandle('ItalBold')
   aFont  := GetFontParam(hFont)
   cFont  := aFont[1]
   nFSize := aFont[2]
   lBold  := aFont[3]
   nHImg  := 48          // 32,55  - задаём размер картинки на кнопке
   aBtnObj:= {}

   IF App.Cargo:aDisplayMode[2] <= 720
      nHImg  := 32
   ENDIF

   // преобразуем ICO -> XXX так как нет в TOOLBAR поддержи .ico
   cPath  := App.Cargo:cPathTemp              // путь, куда выгружаются картинки
   aFrmt  := { "BMP", "PNG", "GIF" }
   aImg   := { "iMG48", "iAccess48", "iBase48", "iConfig48", "iExit48" }
   aImg1  := ARRAY(LEN(aImg))

   FOR i := 1 TO LEN(aImg)
      hIco  := LoadIconByName( aImg[i], nHImg, nHImg )
      hBmp  := BmpFromIcon( hIco )          // вернет хендл bmp
      cFile := cPath + aImg[i] + ".png"
      HMG_SaveImage( hBmp, cFile, "png" )
      aImg1[i] := cFile
      DestroyIcon(hIco)
      DeleteObject( hBmp )
      DO EVENTS
   NEXT

   aObj := { "_Help" , "_Files", "_Table", "_Config", "_Exit"  }
   owc:aTopMenuRu    := { "Помощь"  , "Файлы" , "Таблицы" , "Настройки",  "Выход"  }
   owc:aTopMenuRuTip := { "Помощь"  , "Выбор файла" , "Список таблиц в файле", "Настройки программы", "Выход из программы" }
   owc:aTopMenuEn    := { "Help", "Files", "Tables", "Settings", "Exit" }
   owc:aTopMenuEnTip := { "Help", "File selection", "List of tables in file", "Program settings", "Exit program" }

   aCap := IIF( App.Cargo:cLang == "RU", owc:aTopMenuRu, owc:aTopMenuEn       )
   aTip := IIF( App.Cargo:cLang == "RU", owc:aTopMenuRuTip, owc:aTopMenuEnTip )

   // расчёт по тексту
   nWtxt  := nW := nH := 0
   FOR i := 1 TO LEN(aCap)
      cCap := aCap[ i ]
      //nWCap := GetTxtWidth(cMenu, nFSize, cFont, lBold )
      nWCap := GetTextWidth( NIL, cCap, hFont )
      nWTxt := MAX(nWTxt,nWCap)
   NEXT
   nWTxt := IIF(nWTxt < nHImg, nHImg, nWTxt )   // nHImg-высота bmp
   nWBtn := nWTxt + 5                           // ширина кнопки
   nHBtn := nHImg + 5 + nFSize + 5              // высота кнопки

   IF lBold
      DEFINE TOOLBAREX ToolBar_1 CAPTION "Menu: - not displayed" BUTTONSIZE nWBtn, nHBtn FLAT BORDER ;
         FONT cFont SIZE nFSize BOLD /*TOOLTIP "Double Clik for customizing"*/ CUSTOMIZE
   ELSE
      DEFINE TOOLBAREX ToolBar_1 CAPTION "Menu: - not displayed" BUTTONSIZE nWBtn, nHBtn FLAT BORDER ;
         FONT cFont SIZE nFSize  /*TOOLTIP "Double Clik for customizing"*/ CUSTOMIZE
   ENDIF

      nW := nX := 0
      FOR i := 1 TO LEN(aCap)

         cObj := aObj[i]    // контрол на окне

         BUTTON &cObj CAPTION aCap[i] PICTURE aImg1[i] TOOLTIP aTip[i]   ;
            ACTION _wPost(This.Name, ,This.Name) SEPARATOR  //AUTOSIZE

         This.&(cObj).FONTCOLOR := WHITE       // <<<<----------------- НЕ РАБОТАЕТ
         This.&(cObj).Cargo := oHmgData() ; o := This.&(cObj).Cargo
         o:nBtn := i   ; o:cImage := aImg[i]   // пример

         //IF i % 5 == 0 .AND. i # LEN(aImg)
         //  cObj += "_Dop"
         //  BUTTON &cObj CAPTION " " PICTURE "TB_empty32" ACTION NIL SEPARATOR
         //ENDIF

         AADD( aBtnObj, { i, cObj, "-имя объекта", aCap[i], 0, nW, This.&(cObj).Width, nHBtn, cObj, "-событие" } )

         nW += This.&(cObj).Width + 10

      NEXT

   END TOOLBAR

   nH := This.ToolBar_1.Height + 5 + owc:nG

   owc:nWEndTB := nW       // конец кнопок
   owc:nHEndTB := nH       // высота ToolBar
   owc:aBtnObj := aBtnObj  // массив кнопок на форме
   ?v aBtnObj

RETURN NIL
