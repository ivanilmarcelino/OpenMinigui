/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2024 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * �������� DBF ������ / Viewing DBF files
*/
#define  _HMG_OUTLOG
#include "hmg.ch"
#include "tsbrowse.ch"
#include "hbextcdp.ch"
#include "hb_CodePage.ch"
#include "i_winnls.ch"

REQUEST DBFNTX, DBFDBT, DBFCDX, DBFFPT, SIXCDX, DBFNSX, HB_MEMIO
REQUEST BMDBFNTX, BMDBFCDX, BMDBFNSX, BM_DBSEEKWILD

#define PROGRAM  "MG DBF-view"
#define PROGVER  "Version 0.87 (19.11.2024)"
#define PROGINF  "Viewing dbf files for Harbour/Clipper/DbaseIV/Foxpro/Six"
///////////////////////////////////////////////////////////////////
FUNCTION DimMenuMain()
   LOCAL oMenu := oHmgData()
   // ����  - ��� ������� + ��� �������
   oMenu:aObj   := { "_Help" , "_File"   , "_DBase"    , "_Filter"   , "_Export"  , "_Config"    , "_Exit"    } // VV-��� ������
   oMenu:aImg   := { "iMG48" , "i1File48", "iDbfInfo48", "iFilter48" , "iDbfExp48", "i1Config48" , "i1Exit48" , "iFilterDel48" }
   oMenu:aMnRu  := { "������", "�����"   , "����"      , "������"    , "�������"  , "���������"  , "�����"    }
   oMenu:aMnEn  := { "Help"  , "Files"   , "Base"      , "Filter"    , "Export"   , "Settings"   , "Exit"     }
   oMenu:aTipRu := { "������", "����� �����"   , "��� ����"     , "������ �� ����"    , "������� ����"   , "��������� ����� ����"   , "����� �� ���������" }
   oMenu:aTipEn := { "Help"  , "File selection", "This database", "Filter on the base", "Export database", "Settings of this window", "Exit the program" }
   oMenu:aCap   := IIF( App.Cargo:cLang == "RU", oMenu:aMnRu , oMenu:aMnEn )
   oMenu:aTtip  := IIF( App.Cargo:cLang == "RU", oMenu:aTipRu, oMenu:aTipEn )
   oMenu:cMenu4 := IIF( App.Cargo:cLang == "RU", "��������", "Clear" )
RETURN oMenu

///////////////////////////////////////////////////////////////////
FUNCTION Main(...)
   LOCAL nH, aBClr, cWIco, aUse := {}
   LOCAL aParam := hb_aParams()

   //aParam := {"Total.dbf"}
   IF LEN(aParam) > 0    // ��������� � ��������� ������
      ? ProcNL(), HB_ValToExp(aParam)
      aUse := SaveFileParam(aParam)
   ENDIF

   nH     := 100         // ������ ���� ������� �����
   aBClr  := ORANGE
   cWIco  := "iDbfFile32"

   DEFINE WINDOW wMain CLIENTAREA Sys.ClientWidth, nH   ;
      TITLE PROGRAM ICON cWIco                          ;
      MAIN NOMAXIMIZE NOSIZE /*NOSHOW*/ BACKCOLOR aBClr ;
      ON INIT _wPost(0)  // ������ ��� ��������� !!!

      This.Cargo := oHmgData()

      WITH OBJECT This.Object
        :Event( 0, {|ow| ow:Hide(), _SetThisFormInfo(ow),;
                         Test_Dbf(), Form_TsbView(aUse) ,;
                         _SetThisFormInfo(), _wPost(99, ow) })
        :Event(99, {|ow| ow:Release() })
      END WITH

   END WINDOW

   ACTIVATE WINDOW wMain

RETURN NIL

//////////////////////////////////////////////////////////////////////////
FUNCTION Form_TsbView(aUse)
   LOCAL a4Clr, a2Clr, cForm, aBColor, cTitle, cWIco, oWin, oTsb, oBrw
   LOCAL nY, nX, nH, nW, nG, owc, nW1, nH1, cFont, nFSize, nW2

   // �����:      ���� ����   �����+������    ������ %1       ������ %2 � ��� ��������
   a2Clr   := { {184,107,228}, {142, 25,142}, {238,130,238} , {232,212,244} }  // ���������� 1
   a4Clr   := { {147,112,219}, {115, 25,110}, {238,230,250} , {216,191,216} }  // ���������� 2
   a4Clr   := { {147,112,219}, {115, 25,110}, {194,154,194} , {208,196,232} }  // ���������� 3
   nY      := nX := 0  ; nG := IIF( App.Cargo:aDisplayMode[2] <= 720, 10, 20 )
   nW      := Sys.ClientWidth
   nH      := Sys.ClientHeight
   cTitle  := PROGRAM + SPACE(3) + PROGVER
   cTitle  += SPACE(10) + MiniGUIVersion()
   cWIco   := "iDbfFile32"
   cForm   := "Form_Tsb1"
   aBColor := a4Clr[1]
   cFont   := App.Cargo:cFontName
   nFSize  := App.Cargo:nFontSize
   oWin    := oHmgData()              // ��������� ���� - ������� ������ ��� ����������
   Load_IniFileThis(oWin,nW,nH)       // ������� ��������� �� ���-����� �� ���������� ����
   App.Cargo:a4ClrFilter := a2Clr     // �������� ����� ��� ���� ������� �������

   SET MSGALERT BACKCOLOR TO aBColor
   App.Cargo:aDlgBColor  := aBColor   // Alert* BackColor

   DEFINE WINDOW &cForm AT oWin:nY, oWin:nX WIDTH oWin:nW HEIGHT oWin:nH ;
      TITLE cTitle ICON cWIco                                     ;
      MINWIDTH 500 MINHEIGHT 500                                  ; // ���������� ���������� �������� ����
      WINDOWTYPE STANDARD TOPMOST                                 ;
      ON MAXIMIZE ( ResizeForm( This.Object ) )                   ;
      ON SIZE     ( ResizeForm( This.Object ) )                   ;
      FONT cFont SIZE nFSize                                      ;
      BACKCOLOR aBColor                                           ;
      ON INIT    _wPost( 0)                                       ;
      ON RELEASE _wSend(90)

      This.Cargo   := oHmgData() ; owc := This.Cargo  // ��� ���� ������� ������ ��� ���������� (������� ������)
      owc:aBColor  := This.BackColor                  // ���� ����
      owc:nG       := nG                              // ������ �� ���� ����
      owc:cAls     := ""                              // ����� ������� ����
      owc:ahIcoDel := {}                              // ��� �������� ������� ������ � �����
      owc:aUse     := aUse                            // { cFile, lShared, cDrvDbf, cCdPg, cPsw, cSetDel }
      owc:oMenu    := DimMenuMain()                   // ������� ������� ����
      owc:cMenu4   := owc:oMenu:cMenu4                // �������: ��������/Clear

      // ������� ���� ���� TOOLBAREX -> ��.����
      myToolBarMenuMain(owc)
      owc:aBmpFltr := { owc:aBmp[4], owc:aBmp[LEN(owc:aBmp)] }
      owc:aMn4Fltr := { owc:aBtnObj[4,4], owc:cMenu4 }
      owc:aMn4FClr := { BLACK, YELLOW }
      owc:nFilter  := 1
      //
      nY  := owc:nHEndTB     // ������ ������ ������� ����
      nW1 := owc:nWEndTB     // ����� ������ ������� ����
      nX  := nG
      nW  := This.ClientWidth
      nH  := This.ClientHeight
      nW2 := nW - nW1 - nG * 2
      nH1 := 0
      owc:nH3Line := INT( owc:nHEndTB / 3 )
      owc:nFSize  := INT( owc:nH3Line / 2.1 )

      owc:cMsg1 := PROGINF
      @ nH1, nW1 + nG  LABEL Lbl_1 VALUE owc:cMsg1 WIDTH nW2 HEIGHT owc:nH3Line SIZE owc:nFSize ;
        FONTCOLOR YELLOW TRANSPARENT VCENTERALIGN
      nH1 += This.Lbl_1.Height

      owc:cMsg2 := App.Cargo:cPathDbf
      @ nH1, nW1 + nG LABEL Lbl_2 VALUE owc:cMsg2 WIDTH nW2 HEIGHT owc:nH3Line SIZE owc:nFSize ;
        FONTCOLOR WHITE VCENTERALIGN TRANSPARENT
      nH1 += This.Lbl_2.Height

      owc:cRus  := "F2/F3-���������,   Ins-����� ������, Del-�������/������������ ������"
      owc:cEng  := "F2/F3-encoding,   Ins-new recno, Del-delete/restore recno"
      owc:cMsg3 := IIF( App.Cargo:cLang == "RU", owc:cRus, owc:cEng)
      owc:cLang := "HB_LANGSELECT()=" + HB_LANGSELECT()
      @ nH1, nW1 + nG LABEL Lbl_3 VALUE owc:cLang WIDTH nW2 HEIGHT owc:nH3Line SIZE owc:nFSize ;
        FONTCOLOR WHITE TRANSPARENT VCENTERALIGN //INVISIBLE
      owc:aLblUp      := { "Lbl_1", "Lbl_2" , "Lbl_3"}        // ������ ���������
      owc:aLbSay      := { owc:cMsg1, owc:cMsg2, owc:cMsg3 }
      // ���������� ������ ����
      App.Cargo:aLbl3 := { GetProperty(cForm, "Row") + GetTitleHeight() + nH1,;
                            GetProperty(cForm, "Col") + GetBorderWidth() + nW1 + nG }

      This.MinWidth  := nW1 + nG + GetBorderWidth()*2    // ���������� ���������� �������� ����
      //This.MinHeight := 400 + GetBorderHeight()*2      // ���������� ���������� �������� ����

      /////////////////////// ������� ///////////////////////////////////////////////////
      oTsb := oHmgData()
      oTsb:nY    := nY
      oTsb:nX    := nG
      oTsb:nW    := nW - oTsb:nX * 2
      oTsb:nH    := nH - oTsb:nY - nG
      oTsb:a4Clr := a4Clr     // ��� ����� �������

      // ����� ��� ������� - ����� ��� ������� ������-�������
      oTsb:aTemp := a4Clr[4]
      @ oTsb:nY, oTsb:nX LABEL Label_Table WIDTH oTsb:nW HEIGHT oTsb:nH ;
        VALUE '' VCENTERALIGN CENTERALIGN BACKCOLOR oTsb:aTemp BORDER  //INVISIBLE
      //_o2log(oTsb, 15, ProcNL()+" ��������� ������� : => oTsb", .T.)
      owc:cObjTsb := "Label_Table"   // ��� ResizeForm()

      oBrw     := "������ ������� ������� / Future table object"
      owc:oBrw := oBrw               // �������� �� ���� ������ ���
      owc:cBrw := "Tsb_1Dbf"         // �������� �� ����
      owc:oTsb := oTsb               // ���������� � ����� �������

      _o2log(owc, 15, ProcNL()+" -------------- ��������� ������� : => owc-Cargo", .T.)

      ON KEY F1     ACTION _wPost("_Help", cForm,"_Help")
      ON KEY ESCAPE ACTION _wPost(99)

      // ��������� ������� �� ��� ���� ���������
      Sets_Event2WindowStandart()

      ? ProcNL()
      ? SPACE(5) + "WINDOWTYPE STANDARD:", App.FormName, App.Handle
      ?? "This.Handle:", This.Handle
      ? SPACE(5) + " _HMG_MainHandle:", _HMG_MainHandle

   END WINDOW

   //CENTER WINDOW &cForm
   ACTIVATE WINDOW &cForm

RETURN NIL

////////////////////////////////////////////////////////////
// ��������� ������� �� WINDOWTYPE STANDARD ���� ���������
STATIC FUNCTION Sets_Event2WindowStandart()

      WITH OBJECT This.Object
      // events begin
       :Event( 0, {|ow| // ������ ����� ���������� ����
                        Local aUse := ow:Cargo:aUse
                        This.Topmost := .F.
                        ? ProcNL(),">>> Start window: "+ow:Name
                        IF LEN(aUse) > 0
                           This.Lbl_2.Value := aUse[1]
                           OpenFileView(aUse,ow)           // -->> 2file.prg
                        ELSE
                           // ��������� ������ ��� ������
                           This.&("_DBase").Enabled  := .F.
                           This.&("_Filter").Enabled := .F.
                           This.&("_Export").Enabled := .F.
                        ENDIF
                        DO EVENTS
                        Return Nil
                        })
       // ��� ������� + ��� ������� //   ������ �������� ����
       //            VVVV           //   { "_Help", "_File", "_DBase" , "_Export" , "_Config" , "_Exit"  }
       :Event({10,"_Help"  }, {|ow,ky,cn,ob| This.&(cn).Enabled := .F. ,;
                                             _SetThisFormInfo(ow)      ,;
                                             MsgAbout(,,,ky,cn)        ,;
                                             _SetThisFormInfo()        ,;
                                             This.&(cn).Enabled := .T. ,;
                                             ob := ow:Cargo:oBrw       ,;
                                             IIF( ISOBJECT(ob), ob:SetFocus(), ow:Setfocus('Lbl_1') ) } )

       :Event({11,"_File" }, {|ow,ky,cn,ob| This.&(cn).Enabled := .F. , ob := ow:Cargo:oBrw ,;
                                    _SetThisFormInfo(ow)          ,;
                                    ob := Menu2OpenFile(ow,ky,cn) ,; ////// �������� ���� + ������������� ������ /////
                                    _SetThisFormInfo()            ,;
                                    This.&(cn).Enabled := .T.     ,;
                                    IIF( ISOBJECT(ob), ob:SetFocus(), ow:Setfocus('Lbl_1') ) } )

       :Event({12,"_DBase" }, {|ow,ky,cn,ob| This.&(cn).Enabled := .F. , ob := ow:Cargo:oBrw ,;
                                             _SetThisFormInfo(ow)        ,;
                                             Menu3InfoDBase(ow,ky,cn,ob) ,;
                                             _SetThisFormInfo()          ,;
                                             This.&(cn).Enabled := .T.   ,;
                                             IIF( ISOBJECT(ob), ob:SetFocus(), ow:Setfocus('Lbl_1') ) } )

       :Event({13,"_Filter"}, {|ow,ky,cn,ob| // ������ �� ����
                                             Local cFltr, owc := ow:Cargo
                                             Local cCapt, cBmp, aFClr
                                             This.&(cn).Enabled := .F.
                                             ob := ow:Cargo:oBrw
                                             cBmp  := owc:aBmpFltr[1]
                                             cCapt := owc:aMn4Fltr[1]
                                             aFClr := owc:aMn4FClr[1]
                                             IF owc:nFilter == 1               // ������� ������
                                                _SetThisFormInfo(ow)
                                                Form4Filter(ow,ky,cn,ob)       // -> 4Filter.prg - ����� ����
                                                _SetThisFormInfo()
                                                DbSelectArea(ob:cAlias)        // �� ������ ������
                                                cFltr := App.Cargo:cRetFilter  // ������� ������ �������
                                                If LEN(cFltr) > 0
                                                   ob:FilterData( cFltr )       // ��������� ������� �� ����
                                                   mySuperHdFilter(ob, cFltr)   // ����� ������� � �����������
                                                   owc:nFilter := 2
                                                   cBmp  := owc:aBmpFltr[2]
                                                   cCapt := owc:aMn4Fltr[2]
                                                   aFClr := owc:aMn4FClr[2]
                                                Endif
                                             Else                               // ������� ��������
                                                ob:FilterData()                 // ������� ������� �� ����
                                                mySuperHdFilter(ob, "")         // ����� ������� � �����������
                                                owc:nFilter := 1
                                             Endif
                                             This.&("_Filter").Picture   := cBmp
                                             This.&("_Filter").Caption   := cCapt
                                             This.&("_Filter").Fontcolor := aFClr
                                             This.&(cn).Enabled := .T.
                                             // ������ ��������� - ����� ������, ��-�� ���� ��� ������ TOOLBAREX
                                             This.&("Lbl_1").Value := owc:aLbSay[1]
                                             This.&("Lbl_2").Value := owc:aLbSay[2]
                                             This.&("Lbl_3").Value := owc:aLbSay[3]
                                             ob:SetFocus()
                                             DO EVENTS
                                             Return Nil
                                             } )

       :Event({14,"_Export"}, {|ow,ky,cn,ob| This.&(cn).Enabled := .F. , ob := ow:Cargo:oBrw  ,;
                                             _SetThisFormInfo(ow)         ,;
                                             Menu5Export(ow,ky,cn,ob)     ,;  // -> 5export.prg
                                             _SetThisFormInfo()           ,;
                                             This.&(cn).Enabled := .T.    ,;
                                             IIF( ISOBJECT(ob), ob:SetFocus(), ow:Setfocus('Lbl_1') ) } )

       :Event({15,"_Config"}, {|ow,ky,cn,ob| This.&(cn).Enabled := .F. , ob := ow:Cargo:oBrw ,;
                                             _SetThisFormInfo(ow)        ,;
                                             Menu6Config(ow,ky,cn,ob)   ,;
                                             _SetThisFormInfo()          ,;
                                             This.&(cn).Enabled := .T.   ,;
                                             IIF( ISOBJECT(ob), ob:SetFocus(), ow:Setfocus('Lbl_1') ) } )

       :Event({89,"_Exit"  }, {|ow| _LogFile(.T., ProcNL(),">>> Exit button pressed! Window: "+ow:Name), _wSend(99) } )

       :Event(90, {|ow,ky| // Release
                           Local aWin, oIni, oSec, cSec, ah, cAls := ow:Cargo:cAls
                           ? "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                           ?  Repl(".", 10), "=> RELEASE WINDOW <=", ow:Name
                           ?? "... Program running time -", HMG_TimeMS( App.Cargo:tStart )
                           ah := ow:Cargo:ahIcoDel
                           ? Repl(".", 10),"Delete handle icon - ow:Cargo:ahIcoDel="
                           ?? ah, HB_ValToExp(ah)
                           IF IsArray(ah)
                              AEval(ah, {|h| DestroyIcon(h) })  // ������� ������ ������
                           ENDIF
                           // ��������� ������� ����
                           oIni := App.Cargo:oIni
                           aWin := { ow:Row, ow:Col, ow:Width, ow:Height }
                           cSec := "MAIN"
                           oSec := oIni:Get(cSec)
                           oSec:Set("Window", aWin )
                           // ������ � ���-����
                           oIni:cCommentBegin := " Modify: " + hb_TtoC( hb_DateTime() )
                           oIni:Write()  // �� UTF8, �.�. ��� BOM �� ������
                           IF LEN(cAls) > 0
                              (cAls)->( dbCloseArea() )
                           ENDIF
                           Return Nil
                           })

       :Event(99, {|ow| ow:Release()        })
      // events end
      END WITH

RETURN NIL

///////////////////////////////////////////////////////////////////////////////
// ������� ��������� �� ���-����� �� ���������� ����
STATIC FUNCTION Load_IniFileThis(oWin,nW,nH)
   LOCAL cKey, aWin, oSec, cSect, cMsg

   cSect := "MAIN"                     // ������
   oSec  := App.Cargo:oIni:Get(cSect)
   cKey  := "Window"                   // ��������
   aWin  := oSec:Get(cKey)

   IF IsArray(aWin)
      IF aWin[1] < 0 .OR. aWin[2] < 0
         // ��� ������� ����
      ELSEIF aWin[3] <= 0 .OR. aWin[4] <= 0
         // ��� ���� ��������� ����
      ELSE
         oWin:nY := aWin[1]
         oWin:nX := aWin[2]
         oWin:nW := aWin[3]
         oWin:nH := aWin[4]
      ENDIF
      // �������� �� ������ ���.������
      IF aWin[3] > App.Cargo:aDisplayMode[1]
         oWin:nW  := aWin[3] := App.Cargo:aDisplayMode[1]
      ENDIF
      IF aWin[4] > App.Cargo:aDisplayMode[2]
         oWin:nH := aWin[4] := App.Cargo:aDisplayMode[2]
      ENDIF
   ELSE
     oWin:nY := 0
     oWin:nX := 0
     oWin:nW := nW
     oWin:nH := nH
   ENDIF

   IF App.Cargo:cLang == "RU"
      cMsg := "�������� !; �������� ������������ ���������� �����;"
      cMsg += "������ � ��������� Dbf-�����"
   ELSE
      cMsg := "Attention!; Auto-opening of the index file is enabled�"
      cMsg += "along with opening the Dbf file"
   ENDIF

   // ������������ ���������� �����
   IF App.Cargo:oIni:MAIN:lAutoIndexOpen
      cMsg += ";; SET AUTOPEN ON"
      //AlertInfo( cMsg, , "1MG", 64, {RED} )
      SET AUTOPEN ON
   ELSE
      SET AUTOPEN OFF
   ENDIF

RETURN NIL

*----------------------------------------------------------------------------*
INIT PROCEDURE Sets_ENV()
*----------------------------------------------------------------------------*
   LOCAL o, cLog, aFont, cIni := hb_FNameExtSet( App.ExeName, ".ini" )

   SET CODEPAGE TO ENGLISH       // HB_CDPSELECT( "EN" )
   SET LANGUAGE TO ENGLISH       // HB_LANGSELECT( "EN" )

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
   o:cIniFile       := cIni
   o:lLogDel        := .T.
   o:aDlgBColor     := { 141, 179, 226 }     // Alert* BackColor
   o:aDlgFColor     := {  0 ,  0 ,  0  }     // Alert* FontColor
   o:aBClrMain      := {127,189,228}
   o:cDefAppIcon    := "1MG"
   o:lDebug         := .T.
   o:aWinOpen       := {}
   o:cTitle         := PROGRAM + " ! " + PROGINF
   o:cVersion       := PROGVER
   o:cLang          := "EN"
   o:cAvtor         := "Copyright 2024 Verchenko Andrey + Sergej Kiselev"
   o:cEmail         := "<verchenkoag@gmail.com> Dmitrov, Moscow region / <bilance@bilance.lv>"
   o:cExport1       := "Export to Open Office"
   o:cExport2       := "(c) 2018 Pavel Tsarenko <tpe2@mail.ru>"
   o:cExport3       := "(c) 2020 Sidorov Aleksandr <aksidorov@mail.ru>"
   o:cExport4       := "Export to SQLite"
   o:cExport5       := "(c) 2024 Grigory Filatov <gfilatov@inbox.ru>"
   o:cPrgInfo1      := "Many thanks for your help: Grigory Filatov <gfilatov@inbox.ru>"
   o:cPrgInfo2      := "Tips and tricks programmers from our forum http://clipper.borda.ru"
   o:cSiteDownload  := "Home page for download - http://www.hmgextended.com/"
   o:cPathTemp      := GetUserTempFolder() + "\"
   o:cPathDbf       := GetStartUpFolder() + "\"
   //o:aDisplayMode := { System.DesktopWidth , System.DesktopHeight - GetTaskBarHeight() }
   o:aDisplayMode   := { Sys.ClientWidth , Sys.ClientHeight }
   // ������� ����� ����������, ��������� �������������� �� ������ ���������� ������
   // setting your parameters, allows you to test for other screen resolutions
   //o:aDisplayMode := { 1280 , 1280 }
   //o:aDisplayMode   := { 1280 , 680 }  // ������� ������
   o:cDisplayMode   := HB_NtoS(o:aDisplayMode[1]) + "x" + HB_NtoS(o:aDisplayMode[2])
   o:cFontName      := "DejaVu Sans Mono"   // "Arial"
   o:cFontName2     := "Comic Sans MS"
   o:nFontSize      := 14
   o:cDlgFont       := "DejaVu Sans Mono"
   o:nDlgSize       := o:nFontSize + 2
   o:nMenuBmpHeight := 32
   o:nTsbHCell      := 32       // ������ ������ ������� = ������ ������

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

   ThisAboutProg()  // ����� � ��� ���� � ���������
   ? o:cLogFile ; ?

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
   _DefineFont("DlgFont" , o:cDlgFont , o:nDlgSize   , .F., .F. )             // ���� ���� Alert*
   //
   IF ! o:lIni
      // TsBrowse                                       bold italic
      _DefineFont("Normal"  , o:cFontName, o:nFontSize  , .F., .F. )
      _DefineFont("Bold"    , o:cFontName, o:nFontSize  , .T., .F. )
      _DefineFont("Italic"  , "Tahoma"   , o:nFontSize-2, .F., .T. )
      _DefineFont("SpecHdr" , o:cFontName, o:nFontSize-4, .T., .T. )
      _DefineFont("SuperHdr", o:cFontName, o:nFontSize-4, .T., .T. )
      _DefineFont("TsbEdit" , "Arial"    , o:nFontSize  , .F., .T. )
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
      _DefineFont("SuperHdr"  , aFont[1], aFont[2] , aFont[3], aFont[3] )
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
   Default o:oIni:MAIN:lAutoIndexOpen := .F.                     // SET AUTOPEN   OFF
   Default o:oIni:MAIN:Shared         := "SHARED"
   Default o:oIni:MAIN:CodePade       := "RU1251"
   Default o:oIni:MAIN:SetDeleted     := "OFF"
   Default o:oIni:MAIN:DrvDbf         := "DBFCDX"
   Default o:oIni:MAIN:Rem            := "������� �������� ���������/program code page"
   Default o:oIni:MAIN:SET_CODEPAGE   := "" //"RU1251"
   Default o:oIni:MAIN:SET_LANGUAGE   := "" //"RU1251"

   // ������� ���� �� ���-�����
   App.Cargo:cLang    := o:oIni:MAIN:cLang                        // ���� ���������� ���������
   App.Cargo:cSetCdpg := o:oIni:MAIN:SET_CODEPAGE
   App.Cargo:cSetLang := o:oIni:MAIN:SET_LANGUAGE

   IF LEN(App.Cargo:cSetCdpg) == 0
      // ��� ����� �� ������������ � ���������, ��� �������� ��� �� ���������
      // this is not used anywhere in the program, there is no default database opening
      App.Cargo:cSetCdpg := "RU1251"
      App.Cargo:oIni:MAIN:SET_CODEPAGE := "RU1251"
   ENDIF

   ? ProcNL(), HMG_GetLocaleInfo( LOCALE_SENGLANGUAGE ), App.Cargo:cSetLang
   IF !IsString(App.Cargo:cSetLang) .OR. !IsString(App.Cargo:cSetLang) .OR. ;
      LEN(App.Cargo:cSetLang) == 0
      // �������� ����� � ��������� / Checking the language in the program
      // ���� �� ���������� / language on computer
      IF UPPER(HMG_GetLocaleInfo( LOCALE_SENGLANGUAGE )) == "RUSSIAN"
         SET CODEPAGE TO RUSSIAN
         SET LANGUAGE TO RUSSIAN
      ELSE
         // ����� ����� ��������� ��� ����, �������� hb_CodePage.prg
         // here you can set your language, see hb_CodePage.prg
         // HB_CDPSELECT( "EN" )
         // HB_LANGSELECT( "EN" )
      ENDIF
   ELSE
      IF App.Cargo:cSetLang == "RU1251" .OR. App.Cargo:cSetLang == "RU866"
         SET LANGUAGE TO RUSSIAN
         SET CODEPAGE TO RUSSIAN
      ELSE
         HB_CDPSELECT ( App.Cargo:cSetCdpg )
         HB_LANGSELECT( App.Cargo:cSetLang )
      ENDIF
   ENDIF
   ? ProcNL(), "HB_LANGSELECT()=", HB_LANGSELECT(), "HB_CDPSELECT()=", HB_CDPSELECT()

   // TsBrowse
   Default o:oIni:TsBrowse := oHmgData()
   Default o:oIni:TsBrowse:Normal   := GetFontParam(GetFontHandle("Normal"  ))
   Default o:oIni:TsBrowse:Bold     := GetFontParam(GetFontHandle("Bold"    ))
   Default o:oIni:TsBrowse:Italic   := GetFontParam(GetFontHandle("Italic"  ))
   Default o:oIni:TsBrowse:SpecHdr  := GetFontParam(GetFontHandle("SpecHdr" ))
   Default o:oIni:TsBrowse:SuperHdr := GetFontParam(GetFontHandle("SuperHdr"))
   Default o:oIni:TsBrowse:Edit     := GetFontParam(GetFontHandle("TsbEdit" ))
   //                    cell     Head   foot    SpecHider   SuperHider   Edit
   //oTsb:aFont   := { "Normal", "Bold", "Bold", "SpecHdr" , "SuperHdr", "TsbEdit" }


   // Filter
   Default o:oIni:Filter := oHmgData()
   Default o:oIni:Filter:List_01 := 'Deleted()'   // � �������� �������

   IF ! o:lIni
       // ������ � ���-����
       o:oIni:cCommentBegin := " Modify: " + hb_TtoC( hb_DateTime() )
       o:oIni:Write()  // �� UTF8, �.�. ��� BOM �� ������
   ENDIF

RETURN

///////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION ResizeForm( oWnd )
   Local nG, owc, nTsbY, nTsbX, cBrw, cObj, nH, nW, nHTBar, oTsb, oBrw, cForm
   DEFAULT oWnd := _WindowObj( GetActiveWindow() )

   ? ProcNL(), oWnd, oWnd:Name
   cForm  := oWnd:Name
   nW     := This.ClientWidth
   nH     := This.ClientHeight
   owc    := oWnd:Cargo
   nTsbY  := owc:nTsbY
   nTsbX  := owc:nTsbX
   nG     := owc:nG
   nHTBar := owc:nHEndTB    // ����� ������ �� ������ - ������ ToolBar

   // ������ Label ��������
   cObj  := owc:cObjTsb             // ��� ������� ������-�������
   IF GetControlIndex(cObj, cForm ) > 0
      This.&(cObj).Width  := nW - nG*2
      This.&(cObj).Height := nH - nG - nHTBar
   ENDIF

   oTsb    := owc:oTsb          // ��� ����� ������� ������-�������
   oTsb:nW := nW - oTsb:nX * 2
   oTsb:nH := nH - oTsb:nY - nG

   oBrw    := oWnd:Cargo:oBrw    // �������

   IF !ISOBJECT(oBrw)
      //AlertStop("Not an oBrw object !;" + ProcNL())
      //RETURN NIL
   ELSE
      cBrw   := owc:cBrw
      ? ProcNL(), cBrw, oBrw:cControlName, oBrw:ClassName
      //cBrw := oBrw:cControlName
      This.&(cBrw).Enabled := .F. // ����������� ������� ������� (������ �� ������������)

      // �� ������ Move() ����������� ReSize() - �������� ���������� ��. TControl.prg
      oBrw:Move( oBrw:nLeft, oBrw:nTop, nW - oBrw:nLeft - nG, nH - oBrw:nTop - nG, .T. )

      This.&(cBrw).Enabled := .T. // �������������� ������� ������� (������ ������������)

      oBrw:Paint()
      oBrw:Refresh(.T.)
      oBrw:SetNoHoles()
      oBrw:SetFocus()
   ENDIF

   DO EVENTS

RETURN NIL

///////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION ThisAboutProg()

   ? PadC( " Program start - " + HB_TTOC( hb_DateTime() ) + " ", 80, "-" )
   ? App.Cargo:cTitle ; ? App.Cargo:cVersion
   ? " Screen resolution:", HB_NtoS(GetDesktopWidth())+" x "+HB_NtoS(GetDesktopHeight())
   ?? "LargeFontsMode()=", HB_NtoS( LargeFontsMode() )
   ? "Free Open Software:", MiniGuiVersion()
   ? "     Free Compiler:", hb_Ccompiler()
   ? "  Free Gui library:", Version()
   ? "   System language:", HMG_GetLocaleInfo( LOCALE_ILANGUAGE ), HMG_GetLocaleInfo( LOCALE_SLANGUAGE )
   ?? HMG_GetLocaleInfo( LOCALE_SNATIVELANGNAME ), HMG_GetLocaleInfo( LOCALE_SENGLANGUAGE )
   ? " Programm language:", hb_cdpSelect(), hb_langSelect() ; ?

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
   // �������� ������� bmp ����
   owc:aBmp := aImg1

   // �������� ������� �� ���� ��� ������������ ����� � ������ ����
   owc:aTopMenuRu    := owc:oMenu:aMnRu
   owc:aTopMenuEn    := owc:oMenu:aMnEn
   owc:aTopMenuEnTip := owc:oMenu:aTipRu
   owc:aTopMenuRuTip := owc:oMenu:aTipEn
   // aObj := { "_Help" , "_Files", "_Table", "_Examples", "_Config", "_Exit"  }
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
      DEFINE TOOLBAREX ToolBar_1 CAPTION "Menu: - not displayed" BUTTONSIZE nWBtn, nHBtn FLAT ;
         FONT cFont SIZE nFSize BOLD /*TOOLTIP "Double Clik for customizing"*/ CUSTOMIZE
   ELSE
      DEFINE TOOLBAREX ToolBar_1 CAPTION "Menu: - not displayed" BUTTONSIZE nWBtn, nHBtn FLAT ;
         FONT cFont SIZE nFSize  /*TOOLTIP "Double Clik for customizing"*/ CUSTOMIZE
   ENDIF

      nW := nX := 0
      FOR i := 1 TO LEN(aCap)

         cObj := aObj[i]    // ������� �� ����

         BUTTON &cObj CAPTION aCap[i] PICTURE aImg1[i] TOOLTIP aTip[i]   ;
           ACTION _wPost(This.Name, ,This.Name) SEPARATOR  //AUTOSIZE

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

   nH := This.ToolBar_1.Height + owc:nG //+ 5

   owc:nWEndTB := nW       // ����� ������
   owc:nHEndTB := nH       // ������ ToolBar
   owc:aBtnObj := aBtnObj  // ������ ������ �� �����
   ?v aBtnObj


RETURN NIL

///////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Test_Dbf()
/*
FIELD CENA_ALL, OB4ORUD
   LOCAL cFile, cPath := App.Cargo:cPathDbf

   cFile := cPath + "oborud.ru866.test.dbf"
   IF FILE(cFile)
      DbUseArea(.T., "DBFNTX", cFile, "cAls", .F., .F., "RU866")
      If OrdCount() < 1
         INDEX ON RECNO()      TAG ALL                      // DbSetOrder(1) ����� ���� �������
         INDEX ON RECNO()      TAG NN   FOR !Deleted()      // DbSetOrder(2) ����� �� �������� �������
         INDEX ON RECNO()      TAG DEL  FOR  Deleted()      // DbSetOrder(3) ����� �������� �������
         INDEX ON UPPER(OB4ORUD) TAG OB4ORUD                // DbSetOrder(4)
         INDEX ON CENA_ALL     TAG CENA FOR CENA_ALL > 0    // DbSetOrder(5)
      ENDIF
      ("cAls")->( dbCloseArea() )
   ENDIF

   cFile := cPath + "oborud(ru866).dbf"
   IF FILE(cFile)
      DbUseArea(.T., "DBFCDX", cFile, "cAls", .F., .F., "RU866")
      If OrdCount() < 1
         INDEX ON RECNO()      TAG ALL                      // DbSetOrder(1) ����� ���� �������
         INDEX ON RECNO()      TAG NN   FOR !Deleted()      // DbSetOrder(2) ����� �� �������� �������
         INDEX ON RECNO()      TAG DEL  FOR  Deleted()      // DbSetOrder(3) ����� �������� �������
         INDEX ON UPPER(OB4ORUD) TAG OB4ORUD                // DbSetOrder(4)
         INDEX ON CENA_ALL     TAG CENA FOR CENA_ALL > 0    // DbSetOrder(5)
      ENDIF
      ("cAls")->( dbCloseArea() )
   ENDIF
*/
RETURN NIL

///////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION SaveFileParam(aPrm)
   LOCAL nI, aUse, lShared, cShared, cDrvDbf, cCdPg, cSetDel, cFile, ao

   // ������� ��������� �� ���-�����
   ao := (App.Cargo)
   cShared := ao:oIni:MAIN:Shared       ; Default cShared := "SHARED"
   cCdPg   := ao:oIni:MAIN:CodePade     ; Default cCdPg   := "RU1251"
   cSetDel := ao:oIni:MAIN:SetDeleted   ; Default cSetDel := "OFF"
   cDrvDbf := ao:oIni:MAIN:DrvDbf       ; Default cDrvDbf := "DBFCDX"
   lShared := IIF( cShared == "EXCLUSIVE", .F.  , .T.  )   // EXCLUSIVE/SHARED
   cSetDel := IIF( cSetDel == "OFF"      , "OFF", "ON" )   // SET DELETED OFF/SET DELETED ON
   cFile   := aPrm[1]
   IF AT(":\", cFile ) == 0
      //cFile := GetStartUpFolder() + "\" + cFile
      cFile   := GetCurrentFolder() + "\" + cFile
      aPrm[1] := cFile
   ENDIF

   //   := { cFile, lShared, cDrvDbf , cCdPg  , cPsw, cSetDel }
   //aUse := {""    , .F.    , "DBFCDX", "RU866", ""  , "OFF"}
   aUse := {cFile   , lShared  , cDrvDbf , cCdPg  , ""  , cSetDel}

   FOR nI := 1 TO LEN(aPrm)
      IF nI == 2
         aUse[nI] := IIF(UPPER(aPrm[nI])==".T.", .T., .F.)
      ELSE
         aUse[nI] := aPrm[nI]
      ENDIF
   NEXT

RETURN aUse
