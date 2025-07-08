/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 * Copyright 2024 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2024 Grigory Filatov <gfilatov@inbox.ru>
 *
 * �������� SQLite ������ / Viewing SQLite files
*/
#define _HMG_OUTLOG

#include "hmg.ch"

REQUEST HB_CODEPAGE_RU866, HB_CODEPAGE_RU1251, HB_CODEPAGE_RUKOI8, HB_CODEPAGE_RUISO
REQUEST HB_CODEPAGE_UA866, HB_CODEPAGE_UA1251, HB_CODEPAGE_UAKOI8, HB_CODEPAGE_UA1125
REQUEST HB_CODEPAGE_UTF8, HB_CODEPAGE_UTF8EX, HB_CODEPAGE_UTF16LE
REQUEST DBFNTX, DBFDBT, DBFCDX, DBFFPT, SIXCDX, DBFNSX
REQUEST SDDSQLITE3, SQLMIX

#define PROGRAM  "MG SQLite Viewer"
#define PROGVER  "Version 0.34 (17.10.2024)"
#define PROGINF  "files: *.db *.db3 *.sq3 *.s3db *.sqlite"
///////////////////////////////////////////////////////////////////
FUNCTION DimMenuMain()
   LOCAL oMenu := oHmgData()
   // ����  - ��� ������� + ��� �������
   oMenu:aObj   := { "_Help" , "_Files"   , "_Table" , "_Config"  , "_Exit"   }
   oMenu:aImg   := { "iMG48" , "iSQLite48", "iBase48", "iConfig48", "iExit48" }
   oMenu:aMnRu  := { "������", "�����"    , "�������", "���������", "�����"   }
   oMenu:aMnEn  := { "Help"  , "Files"    , "Tables" , "Settings" , "Exit"    }
   oMenu:aTipRu := { "������", "����� �����"   , "������ ������ � �����" , "��������� ���������", "����� �� ���������" }
   oMenu:aTipEn := { "Help"  , "File selection", "List of tables in file", "Program settings"   , "Exit program" }
   oMenu:aCap   := IIF( App.Cargo:cLang == "RU", oMenu:aMnRu , oMenu:aMnEn )
   oMenu:aTtip  := IIF( App.Cargo:cLang == "RU", oMenu:aTipRu, oMenu:aTipEn )
RETURN oMenu

//////////////////////////////////////////////////////////////////////////////
FUNCTION Main(...)
   LOCAL nY, nX, nH, nW, nG, a2Clr, a4Clr, aBClr, aFClr, o, owc
   LOCAL aParam := hb_aParams() , cFile := "", cForm := "wMain"

   IF LEN(aParam) > 0    // ��������� � ��������� ������
      IF AT(":\",aParam[1]) == 0
         cFile := GetCurrentFolder() + "\" + aParam[1]
      ELSE
         cFile := aParam[1]
      ENDIF
      ? ProcNL(), HB_ValToExp(aParam), cFile
   ENDIF

   nY    := nX := 0  
   nG    := IIF( App.Cargo:aDisplayMode[2] <= 720, 10, 20 )
   nW    := Sys.ClientWidth
   nH    := 100    // ������ ���� ������� �����
   //a4Clr:= { {127,189,228}, { 48,153,219}, { 49,177,255} , {240,240,240} }  // ������� 1
   a2Clr := { {116,134,242}, {  0,206,209}, {165,177,251} , {194,201,247} }  // ������� 2
   a4Clr := { {100,149,237}, { 30,144,255}, {173,216,230} , {  5,191,255} }  // ������� 3
   aBClr := a4Clr[1]
   aFClr := MAROON
   App.Cargo:a4Clr       := a4Clr                // �������� ����� ��� ���� ������� ��������
   App.Cargo:a4ClrFilter := a2Clr                // �������� ����� ��� ���� ������� �������
   App.Cargo:aBCAlert    := aBClr                // �������� ����� ��� Alert*() � ������ ����
   App.Cargo:aDlgBColor  := aBClr                // Alert* BackColor

   SET MSGALERT BACKCOLOR TO App.Cargo:aBCAlert

   DEFINE WINDOW &cForm AT nY, nX WIDTH nW HEIGHT nH TITLE App.Cargo:cProga ;
      MAIN NOSIZE TOPMOST                                                   ;
      BACKCOLOR aBClr                                                       ;
      ON INIT    _wPost( 0)                                                 ;
      ON RELEASE _wSend(90)

      App.Cargo:nHMain := This.Height + GetBorderHeight() + 3   // ������� - ������ ����� ����
      This.Cargo := oHmgData() ; owc := This.Cargo  // ��� ���� ������� ������ ��� ���������� (������� ������)
      owc:aBColor   := This.BackColor   // ���� ����
      owc:aBClrPsw  := {247,196,196}    // ���� ���� ����� ������
      owc:cForm     := cForm
      owc:nG        := nG
      owc:cPath     := MiniGuiVersion()
      owc:cVers     := App.Cargo:cPathDbf
      owc:cInfo     := ""
      owc:aLabel    := { "Lbl_1", "Lbl_2", "Lbl_3" }
      owc:cFile     := cFile                         // ���� � ����� 
      owc:cCdPg     := "UTF8"                        // ������� �� ����� CodePage ����� 
      owc:cLine3    := ""                            // 3 ������
      owc:oMenu     := DimMenuMain()                 // ������� ������� ����
      owc:nHMain    := This.Height

      // ������� ���� ���� TOOLBAREX -> ��.����
      myToolBarMenuMain(owc)
      nY  := 0
      nX  := nG
      This.Height := owc:nHEndTB + GetBorderHeight()*2 //+ GetTitleHeight()

      nW  := This.ClientWidth
      nH  := This.ClientHeight
      owc:nWLbl   := owc:nWEndTB      // ��������� ���������� ������
      owc:nH3Line := INT( nH / 3 )
      owc:nFSize  := INT( owc:nH3Line / 1.7 )

      @ nY, owc:nWLbl  LABEL Lbl_1 VALUE owc:cPath WIDTH nW-owc:nWLbl HEIGHT owc:nH3Line SIZE owc:nFSize FONTCOLOR aFClr VCENTERALIGN TRANSPARENT
      nY += This.Lbl_1.Height
      @ nY, owc:nWLbl LABEL Lbl_2 VALUE owc:cVers  WIDTH nW-owc:nWLbl HEIGHT owc:nH3Line SIZE owc:nFSize FONTCOLOR aFClr VCENTERALIGN TRANSPARENT
      nY += This.Lbl_2.Height
      @ nY, owc:nWLbl LABEL Lbl_3 VALUE owc:cInfo  WIDTH nW-owc:nWLbl HEIGHT owc:nH3Line SIZE owc:nFSize FONTCOLOR aFClr VCENTERALIGN  TRANSPARENT

      ON KEY F1     ACTION _wPost(9)
      ON KEY ESCAPE ACTION _wPost(99)

      o := This.Object
      o:Event( 0, {|ow| // On Init
                        Local aObj := ow:Cargo:aLabel
                        Local lOpen, cForm := ow:Name
                        This.Topmost := .F.
                        _LogFile(.T., ProcNL(),">>> Start programm ! Window: "+ow:Name)
                        This.&("_Table").Enabled := .F.  
                        IF LEN(ow:Cargo:cFile) > 0 // ���� � ����� 
                           SET WINDOW THIS TO ow:Name    // ����������� !!!
                              // ���� ������ � �������� �����
                              lOpen := Menu2OpenFile(ow)         // -> 2openFiles.prg
                           SET WINDOW THIS TO
                           IF lOpen // ���� ��������
                              This.&("_Table").Enabled  := .T.  // �������������� ������
                              // ������������ �� ����� ��� TOOLBAREX
                              SetProperty(cForm,aObj[1],"Visible", .T.)
                              SetProperty(cForm,aObj[2],"Value", owc:cFile)
                              SetProperty(cForm,aObj[3],"Value", owc:cLine3)
                              SetProperty(cForm,aObj[3],"Visible", .T.)
                              _wSend("_Table",ow)     // ����� ������ �������
                           ENDIF
                        ENDIF
                        Return Nil
                        } )

      o:Event({ 9,"_Help"  }, {|ow,ky,cn| This.&(cn).Enabled := .F. , _SetThisFormInfo(ow) ,;
                                          MsgAbout(,,,ky,cn), _SetThisFormInfo(),;
                                          This.&(cn).Enabled := .T. , ow:SetFocus('Lbl_1')   } )

      o:Event({10,"_Files" }, {|ow,ky,cn| // ������ - ���� � �����
                                          Local cTtl := "Select SQLite file"
                                          Local cPath := App.Cargo:cPathDbf
                                          Local owc   := ow:Cargo
                                          Local aObj  := owc:aLabel
                                          Local aF, lOpen, cForm := ow:Name
                                          This.&(cn).Enabled := .T.
                                          SET WINDOW THIS TO ow:Name    // ����������� !!!
                                          aF := GetFile( { {"SQLite files", "*.db;*.db3;*.sq3;*.s3db;*.sqlite"}, {"All files", "*.*"} }, cTtl, cPath, .T. )
                                          SET WINDOW THIS TO
                                          IF LEN(aF) > 0
                                             ? "++++++++++", ProcNL(), LEN(App.Cargo:aWinOpen), HB_ValToExp(App.Cargo:aWinOpen)
                                             IF LEN(App.Cargo:aWinOpen) > 0
                                                //  ������� �������� ���� � ���������
                                                AEval(App.Cargo:aWinOpen, {|aw| Domethod(aw,"Release") })
                                                App.Cargo:aWinOpen := {}   // ����� ����� ������������ �������� ���� �������
                                                owc:cFile := aF            // ���� � ����� 
                                             ENDIF
                                             //
                                             ow:Cargo:cFile := aF[1]       // ��������� ���� �����
                                             SET WINDOW THIS TO ow:Name    // ����������� !!!
                                                 // ���� ������ � �������� �����
                                                 lOpen := Menu2OpenFile(ow)         // -> 2openFiles.prg
                                             SET WINDOW THIS TO
                                             ? ProcNL()+"########", "["+owc:cFile+"]", ky, "lOpen=", lOpen
                                             IF lOpen
                                                This.&("_Table").Enabled  := .T.  // �������������� ������
                                                // ������������ �� ����� ��� TOOLBAREX
                                                SetProperty(cForm,aObj[1],"Visible", .T.)
                                                SetProperty(cForm,aObj[2],"Value", owc:cFile)
                                                SetProperty(cForm,aObj[3],"Value", owc:cLine3)
                                                SetProperty(cForm,aObj[3],"Visible", .T.)
                                             ENDIF
                                          ENDIF
                                          This.&(cn).Enabled := .T.
                                          ow:Setfocus('Lbl_1')
                                          DO EVENTS
                                          IF LEN(aF) > 0 .AND. lOpen // ���� ��������
                                             _wSend("_Table",ow)     // ����� ������ �������
                                          ENDIF
                                          Return Nil
                                          } )

      o:Event({20,"_Table" }, {|ow,ky,cn| /*This.&(cn).Enabled := .F. ,*/ _SetThisFormInfo(ow) ,;
                                         Menu3Table(ow,ky,cn) ,;
                                         _SetThisFormInfo()/*, This.&(cn).Enabled := .T.*/ , ow:SetFocus('Lbl_1') } )

      o:Event({40,"_Config"}, {|ow,ky,cn| This.&(cn).Enabled := .F. , _SetThisFormInfo(ow)      ,;
                                          Menu4Config(ow,ky,cn) /*MsgDebug(ow:Name,ky,cn)*/     ,;
                                          _SetThisFormInfo()        , This.&(cn).Enabled := .T. ,;
                                          ow:Setfocus('Lbl_1')  } )

      o:Event({89,"_Exit"  }, {|ow| _LogFile(.T., ProcNL(),">>> Exit button pressed! Window: "+ow:Name), _wSend(99) } )

      o:Event(90, {|ow,ky| // ON Release
                           ? "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                           ?  Repl(".", 10), "=> RELEASE WINDOW <=", ow:Name
                           _wSend(94, ow)   // ������� ��� �������
                           ?? "... Program running time -", HMG_TimeMS( App.Cargo:tStart )
                           Return Nil
                           })

      o:Event(91, {|  | ReleaseAllWindows () })
      o:Event(92, {|ow| _wSend(99, ow)       })
      o:Event(93, {|ow| // ������� ��� ������� / Close all tables
                        Local cFrm
                        FOR EACH cFrm IN HMG_GetForms()
                            IF cFrm == ow:Name ; LOOP
                            ENDIF
                            _wSend(99, cFrm)
                            DO EVENTS ; wApi_Sleep(100)
                        NEXT
                        Return Nil
                        })
      o:Event(94, {|ow| // ������� ��� ������� / Close all tables
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

///////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myToolBarMenuMain(oWC)
   LOCAL nW, nH, nX, hFont, aFont, cFont, nFSize, lBold, nHImg, aImg
   LOCAL aImg1, aObj, aCap, hIco, hBmp, aFrmt, cFile, cPath, aBtnObj
   LOCAL nWBtn, nHBtn, cCap, aTip, nWtxt, nWCap, cObj, cForm, i, o

   ? ProcNL(), "oWC=", oWC

   cForm  := oWC:cForm                   // ��� ����
   hFont  := GetFontHandle('ItalBold')
   aFont  := GetFontParam(hFont)
   cFont  := aFont[1]
   nFSize := aFont[2]
   lBold  := aFont[3]
   nHImg  := 48          // 32,55  - ����� ������ �������� �� ������
   aBtnObj:= {}

   IF App.Cargo:aDisplayMode[2] <= 720
      nHImg  := 32
   ENDIF

   // ����������� ICO -> XXX ��� ��� ��� � TOOLBAR �������� .ico
   cPath  := App.Cargo:cPathTemp              // ����, ���� ����������� ��������
   aFrmt  := { "BMP", "PNG", "GIF" }
   //aImg := { "iMG48", "iSQLite48", "iBase48", "iExample48", "iConfig48", "iExit48" }
   aImg   := owc:oMenu:aImg
   aImg1  := ARRAY(LEN(aImg))

   FOR i := 1 TO LEN(aImg)
      hIco  := LoadIconByName( aImg[i], nHImg, nHImg )
      hBmp  := BmpFromIcon( hIco )          // ������ ����� bmp
      cFile := cPath + aImg[i] + ".png"
      HMG_SaveImage( hBmp, cFile, "png" )
      aImg1[i] := cFile
      DestroyIcon(hIco)
      DeleteObject( hBmp )
      DO EVENTS
   NEXT

   // �������� ������� �� ���� ��� ������������ ����� � ������ ����
   owc:aTopMenuRu    := owc:oMenu:aMnRu
   owc:aTopMenuEn    := owc:oMenu:aMnEn
   owc:aTopMenuEnTip := owc:oMenu:aTipRu
   owc:aTopMenuRuTip := owc:oMenu:aTipEn

   // aObj := { "_Help" , "_Files", "_Table", "_Examples", "_Config", "_Exit"  }
   // aCap := IIF( App.Cargo:cLang == "RU", owc:aTopMenuRu, owc:aTopMenuEn       )
   // aTip := IIF( App.Cargo:cLang == "RU", owc:aTopMenuRuTip, owc:aTopMenuEnTip )
   aObj := owc:oMenu:aObj
   aCap := owc:oMenu:aCap
   aTip := owc:oMenu:aTtip

   // ������ �� ������
   nWtxt  := nW := nH := 0
   FOR i := 1 TO LEN(aCap)
      cCap := aCap[ i ]
      //nWCap := GetTxtWidth(cMenu, nFSize, cFont, lBold )
      nWCap := GetTextWidth( NIL, cCap, hFont )
      nWTxt := MAX(nWTxt,nWCap)
   NEXT
   nWTxt := IIF(nWTxt < nHImg, nHImg, nWTxt )   // nHImg-������ bmp
   nWBtn := nWTxt + 5                           // ������ ������
   nHBtn := nHImg + 5 + nFSize + 5              // ������ ������

   IF lBold
      DEFINE TOOLBAREX ToolBar_1 CAPTION "Menu: - not displayed" BUTTONSIZE nWBtn, nHBtn FLAT BORDER ;
         FONT cFont SIZE nFSize BOLD /*TOOLTIP "Double Clik for customizing"*/ CUSTOMIZE
   ELSE
      DEFINE TOOLBAREX ToolBar_1 CAPTION "Menu: - not displayed" BUTTONSIZE nWBtn, nHBtn FLAT BORDER ;
         FONT cFont SIZE nFSize  /*TOOLTIP "Double Clik for customizing"*/ CUSTOMIZE
   ENDIF

      nW := nX := 0
      FOR i := 1 TO LEN(aCap)

         cObj := aObj[i]    // ������� �� ����

         BUTTON &cObj CAPTION aCap[i] PICTURE aImg1[i] TOOLTIP aTip[i]   ;
            ACTION _wPost(This.Name, ,This.Name) SEPARATOR  //AUTOSIZE

         This.&(cObj).FONTCOLOR := WHITE       // <<<<----------------- �� ��������
         This.&(cObj).Cargo := oHmgData() ; o := This.&(cObj).Cargo
         o:nBtn := i   ; o:cImage := aImg[i]   // ������

         //IF i % 5 == 0 .AND. i # LEN(aImg)
         //  cObj += "_Dop"
         //  BUTTON &cObj CAPTION " " PICTURE "TB_empty32" ACTION NIL SEPARATOR
         //ENDIF

         AADD( aBtnObj, { i, cObj, "-��� �������", aCap[i], 0, nW, This.&(cObj).Width, nHBtn, cObj, "-�������" } )

         nW += This.&(cObj).Width + 10

      NEXT

   END TOOLBAR

   nH := This.ToolBar_1.Height + 5 + owc:nG

   owc:nWEndTB := nW       // ����� ������
   owc:nHEndTB := nH       // ������ ToolBar
   owc:aBtnObj := aBtnObj  // ������ ������ �� �����
   ?v aBtnObj

RETURN NIL

*----------------------------------------------------------------------------*
INIT PROCEDURE Sets_ENV()
*----------------------------------------------------------------------------*
   LOCAL o, cLog, aFont, cIni := hb_FNameExtSet( App.ExeName, ".ini" )

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
   SET DATE FORMAT TO "DD.MM.YY"
   SET TOOLTIPSTYLE BALLOON
   //!!! ����� �������
   IF !HB_ISOBJECT( App.Cargo ) ; App.Cargo := oHmgData()
   ENDIF
   o := App.Cargo

   Set ShowRedAlert On        // ��������� ���� ��� ���� "Program Error"

   // �������� �� ������ ������ ����� ���������
   _HMG_MESSAGE[4] := "Attempting to run a second copy of the program:" + CRLF + ;
                      App.ExeName + CRLF + ;
                      "Refused to start !" + CRLF + _HMG_MESSAGE[4]
   SET MULTIPLE QUIT WARNING  // ���� ���������
   SET WINDOW MAIN OFF

   o:tStart         := hb_DateTime()        // start time
   o:cLogFile       := ChangeFileExt( App.ExeName, '.log' )
   // ��� ������� - ����� ������
   cLog             := o:cLogFile
   //o:cLogFile       := cFilePath( cLog ) + "\"
   //o:cLogFile       += "_" + cFileNoPath( cLog )
   //
   o:cIniFile       := cIni
   o:lLogDel        := .T.
   o:aDlgBColor     := { 141, 179, 226 }     // Alert* BackColor
   o:aDlgFColor     := {  0 ,  0 ,  0  }     // Alert* FontColor
   o:aBClrMain      := {127,189,228}
   o:cDefAppIcon    := "iSQLite64"
   o:lDebug         := .T.
   o:aWinOpen       := {}
   o:cTitle         := PROGRAM + " " + PROGINF
   o:cVersion       := PROGVER
   o:cProga         := PROGRAM + " " + PROGINF + "  " + PROGVER
   o:cLang          := "EN"
   o:cAvtor         := "Copyright 2024 Verchenko Andrey + Sergej Kiselev + Grigory Filatov"
   o:cEmail         := "<verchenkoag@gmail.com> Dmitrov, Moscow region / <bilance@bilance.lv>"
   o:cEmail         += " / <gfilatov@inbox.ru>"
   o:cExport1       := "Export to Open Office"
   o:cExport2       := "(c) 2018 Pavel Tsarenko <tpe2@mail.ru>"
   o:cExport3       := "(c) 2020 Sidorov Aleksandr <aksidorov@mail.ru>"
   o:cPrgInfo1      := "Many thanks for your help: Grigory Filatov <gfilatov@inbox.ru>"
   o:cPrgInfo2      := "Tips and tricks programmers from our forum http://clipper.borda.ru"
   o:cSiteDownload  := "Home page for download - http://www.hmgextended.com/"
   o:cPathTemp      := GetUserTempFolder() + "\"
   o:cPathDbf       := GetStartUpFolder() + "\SQLite\"
   //o:aDisplayMode := { System.DesktopWidth , System.DesktopHeight - GetTaskBarHeight() }
   o:aDisplayMode   := { Sys.ClientWidth , Sys.ClientHeight }
   // ������� ����� ����������, ��������� �������������� �� ������ ���������� ������
   // setting your parameters, allows you to test for other screen resolutions
   //o:aDisplayMode := { 1280 , 1280 }
   //o:aDisplayMode := { 1280 , 680 }  // ������� ������
   o:cDisplayMode   := HB_NtoS(o:aDisplayMode[1]) + "x" + HB_NtoS(o:aDisplayMode[2])
   o:cFontName      := "DejaVu Sans Mono"   // "Arial"
   o:cFontName2     := "Comic Sans MS"
   o:nFontSize      := 14
   o:cDlgFont       := "DejaVu Sans Mono"
   o:nDlgSize       := o:nFontSize + 2
   o:nMenuBmpHeight := 32
   o:nTsbHCell      := 32    // ������ ������ ������� = ������ ������
   o:aWinOpen       := {}    // �����-������-����-����-���������

   IF o:aDisplayMode[2] <= 720
      o:nFontSize -= 3
      o:nDlgSize  -= 2
      o:nMenuBmpHeight := 24
      o:nTsbHCell      := 24    // ������ ������ ������� = ������ ������
   ENDIF

   // Default font
   SET FONT TO o:cFontName , o:nFontSize

   _SetGetLogFile( o:cLogFile )

   IF o:lLogDel ; hb_FileDelete( o:cLogFile )
   ENDIF

   IF o:lDebug ; SET LOGERROR ON
   ELSE        ; SET LOGERROR OFF
   ENDIF

   o:cIniFile := cIni
   o:lIni     := hb_FileExists(cIni)
   // ������ � ���-����� ����� � ��������� - App.Cargo:oIni
   o:oIni := TIniData():New(cIni, .T.):Read()

   _DefineFont("ItalBold", o:cFontName, o:nFontSize-2, .T., .T. )
   // Menu* font
   _DefineFont("ComSanMS" , o:cFontName2 , o:nFontSize+2 , .F., .F. )         // ���� �������� �������� ����
   _DefineFont("MnNormal" , o:cFontName  , o:nFontSize+2 , .F., .F. )         // ���� �������� �������� ����
   _DefineFont("MenuBtn"  , o:cFontName  , o:nFontSize   , .T., .F. )         // ���� ������ �������� ����
   _DefineFont("WinBtn"   , o:cFontName  , o:nFontSize+2 , .F., .F. )         // ���� ������ ����
   // Alert* font
   _DefineFont("DlgFont"  , o:cDlgFont   , o:nDlgSize    , .F., .F. )         // ���� ���� Alert*
   //
   IF ! o:lIni
      // TsBrowse                                       bold italic
      _DefineFont("Normal"  , "Tahoma"         , o:nFontSize  , .F., .F. )
      _DefineFont("Bold"    , "Times New Roman", o:nFontSize  , .T., .F. )
      _DefineFont("Italic"  , "Tahoma"         , o:nFontSize-2, .T., .F. )
      _DefineFont("SpecHdr" , o:cFontName      , o:nFontSize-4, .T., .T. )
      _DefineFont("SuperHdr", o:cFontName      , o:nFontSize  , .T., .F. )
      _DefineFont("TsbEdit" , "Arial"          , o:nFontSize  , .F., .T. )
   ELSE
      aFont := o:oIni:TsBrowse:Normal
      _DefineFont("Normal"  , aFont[1], aFont[2] , aFont[3], aFont[3] )
      aFont := o:oIni:TsBrowse:Bold
      _DefineFont("Bold"    , aFont[1], aFont[2] , aFont[3], aFont[3] )
      aFont := o:oIni:TsBrowse:Italic
      _DefineFont("Italic"  , aFont[1], aFont[2] , aFont[3], aFont[3] )
      aFont := o:oIni:TsBrowse:SpecHdr
      _DefineFont("SpecHdr"  , aFont[1], aFont[2] , aFont[3], aFont[3] )
      aFont := o:oIni:TsBrowse:SuperHdr
      _DefineFont("SuperHdr" , aFont[1], aFont[2] , aFont[3], aFont[3] )
      aFont := o:oIni:TsBrowse:Edit
      _DefineFont("TsbEdit"  , aFont[1], aFont[2] , aFont[3], aFont[3] )
   ENDIF
   // Alert* colors
   SET MSGALERT BACKCOLOR  TO o:aDlgBColor
   SET MSGALERT FONTCOLOR  TO o:aDlgFColor
   //
   SET DEFAULT ICON TO o:cDefAppIcon
   SET WINDOW MODAL PARENT HANDLE ON
   SET TOOLTIPSTYLE BALLOON
   SET NAVIGATION EXTENDED
   SET MENUSTYLE  EXTENDED

   SetMenuBitmapHeight( o:nMenuBmpHeight )

   ? PadC( " Program start - " + HB_TTOC( hb_DateTime() ) + " ", 80, "-" )
   ? " Screen resolution:", HB_NtoS(GetDesktopWidth())+" x "+HB_NtoS(GetDesktopHeight())
   ?? "LargeFontsMode()=", HB_NtoS( LargeFontsMode() )
   ? "Free Open Software:", MiniGuiVersion()
   ? "     Free Compiler:", hb_Ccompiler()
   ? "  Free Gui library:", Version()

   Default o:oIni:INFO := oHmgData()
   Default o:oIni:INFO:Developed_in   := MiniGUIVersion()
   Default o:oIni:INFO:xBase_compiler := Version()
   Default o:oIni:INFO:C_compiler     := Hb_Compiler()
   Default o:oIni:INFO:Programm       := o:cTitle
   Default o:oIni:INFO:ProgVers       := o:cVersion
   Default o:oIni:INFO:Avtor          := o:cAvtor
   Default o:oIni:INFO:Email          := o:cEmail

   Default o:oIni:MAIN := oHmgData()
   Default o:oIni:MAIN:aBClrMain      := {215, 166, 0}
   Default o:oIni:MAIN:ComSanMS       := { o:cFontName2 , o:nFontSize+2 , .F., .F. }   // ���� �������� �������� ����
   Default o:oIni:MAIN:MnNormal       := { o:cFontName  , o:nFontSize+2 , .F., .F. }   // ���� �������� �������� ����
   Default o:oIni:MAIN:MenuBtn        := { o:cFontName  , o:nFontSize   , .T., .F. }   // ���� ������ �������� ����
   Default o:oIni:MAIN:WinBtn         := { o:cFontName2 , o:nFontSize+2 , .F., .F. }   // ���� ������ ����
   Default o:oIni:MAIN:Window         := {0, 0, 0, 0}            // ���������� ����
   Default o:oIni:MAIN:cLang          := App.Cargo:cLang         // ���� ���������� ���������

   // ������� ���� �� ���-�����
   App.Cargo:cLang := o:oIni:MAIN:cLang                        // ���� ���������� ���������

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

   // Filter
   Default o:oIni:Filter := oHmgData()
   Default o:oIni:Filter:List_01 := 'Deleted()'   // � �������� �������

   IF ! o:lIni
       // ������ � ���-����
       o:oIni:cCommentBegin := " Modify: " + hb_TtoC( hb_DateTime() )
       o:oIni:Write()  // �� UTF8, �.�. ��� BOM �� ������
   ENDIF

RETURN

