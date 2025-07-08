/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2023 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2023 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 * SAMPLES\Advanced\Tsb_Viewer
 *
 * ��������/������ Dbf �����. �����/�������� �� ����
 * View/edit Dbf file. Options/properties by base
*/

#define _HMG_OUTLOG

#include "minigui.ch"
#include "tsbrowse.ch"
#include "Dbinfo.ch"
////////////////////////////////////////////////////////////////////////////////////
// ����� ���� � �������� / Show table window
FUNCTION my_Standard3( cForm, nBtn, cAls, cWndMain )
   LOCAL cVal, nI, hW, bInitForm, oCnf, cSection, aPost, aThrd
   LOCAL cPathDbf := App.Cargo:cPathDbf  //GetStartUpFolder()
   LOCAL oWin, oUse, oMenu, oTsb, aEvent, oIndx, cMsg, aCurrLang
   DEFAULT nBtn     := 3
   DEFAULT cForm    := "w"+hb_ntos(nBtn)+"_Standart"
   DEFAULT cAls     := ""
   DEFAULT cWndMain := App.Cargo:cMainMenuProg    // ��� ���� �������� ���� ��������� Forma_Main

   aCurrLang := { hb_SetCodepage(), hb_CdpSelect(), Hb_LangSelect() }  // ������� ���� ���������

   SET CODEPAGE TO UKRAINIAN     // ������ hb_SetCodepage( "UA1251" )
   SET LANGUAGE TO UKRAINIAN     // ������ hb_CdpSelect( "UA1251" )
   // ���������� ������� _LogFile() ��� ����� ����� -> util_misc.prg

   cVal := nI := hW
   // ��� � ��� �� ��������, �.�. ���� ��������, ���� �������� ������� _LogFile() ��� ���������
   ? ProcNL(), cForm, nBtn, cAls, cWndMain, "|" ,_IsWindowDefined(cForm), hb_CdpSelect()

   //cAls += nBtn  // �������� �������� ������ ��� ������ �����

   aThrd := WaitThreadAvi( 'Create a table ...',,,2 )  // ������ ���� �������� � �������
   // �������� ���� ����������/��������/�������� ��� � �.�. - � �������� �������
   FOR nI := 1 TO 20
      wApi_Sleep( 70 )
      DO EVENTS
      cVal := hb_ntos( nI ) + "/" + "30"
      WaitThreadAviSay( aThrd, cVal )
   NEXT

   IF _IsWindowDefined( cWndMain )
      Domethod(cWndMain, "Minimize")
      DO EVENTS
   ENDIF
   SET FONT TO "DejaVu Sans Mono", 13   // ����� ���� ��� �������

   oCnf     := App.Cargo:oCnf           // cnfg-���� �� ���������� App.Cargo
   cSection := "�������_��_��������"    // ������
   ? ProcNL(), "===== [" + cSection + "] ====="
   //_o2Log(oCnf, 20, "==> .T. cnfg: ", .T.)
   // �������� ������
   IF !CnfgSection(oCnf, cSection)
      RETURN .F.
   ENDIF

   // ������� ��������� �� cnfg-�����
   oWin  := CnfgDataWin(oCnf, cSection)              // ��������� ����
   oUse  := CnfgDataDbf(oCnf,cSection,cPathDbf,cAls) // ���� ��� ��������� ���
   oIndx := Tsb3DataIndex(oUse, cPathDbf)            // ������ ������� - ����� ���� ��������� � cfg-����
   oMenu := CnfgDataMenu(oCnf, cSection)             // ����-������ ���� � ������� �� �������
   oTsb  := CnfgDataTsb(oCnf, cSection, oUse, oWin)  // ��������� ���
   oWin:aCurrLang := aCurrLang                       // ��������� ������� ���� ���������, ��� ����������
                                                     // ��������� � �������������� � TsbViewer.prg

   //_o2Log(oWin,  20, "==> .T. oWin: ", .T.)        // �������� � ���-����
   //_o2Log(oUse,  20, "==> .T. oUse: ", .T.)        // �������� � ���-����
   //_o2Log(oMenu, 20, "==> .T. oMenu: ", .T.)       // �������� � ���-����
   //_o2Log(oTsb,  20, "==> .T. oTsb: ", .T.)        // �������� � ���-����

   // ��� ���� ����� ���� ��������� � cfg-����, ��� ����������� ��������� ���
   aPost  := oMenu:aPost //{1,2,3,4,99}                                  // ��� ������� �� ������ ����
   aEvent := {}                                                          // ������� �� ����, ����� �������
   AAdd( aEvent, { aPost[1], {|ow,ky,cn,ob| ob := This.oBrw.Object, my3Btn1(ow,ky,cn,ob)  } } )  // ������ 1
   AAdd( aEvent, { aPost[2], {|ow,ky,cn,ob| ob := This.oBrw.Object, my3Btn1(ow,ky,cn,ob)  } } )  // ������ 2
   AAdd( aEvent, { aPost[3], {|ow,ky,cn,ob| ob := This.oBrw.Object, my3Btn1(ow,ky,cn,ob)  } } )  // ������ 3
   AAdd( aEvent, { aPost[4], {|ow,ky,cn,ob| ob := This.oBrw.Object, my3Btn1(ow,ky,cn,ob)  } } )  // ������ 4
   AAdd( aEvent, { aPost[5], {|ow,ky,cn| SetProperty(ow:Name, cn, "Enabled", .T. ), ow:Release(), ky:=cn } } ) // �����
   //_o2Log(aEvent, 20, "==> .T. cnfg: ", .T.) // �������� � ���-����

   ? ProcNL(), ALIAS(), oUse:cAlias, Used()
   IF ! oUse:lOpen
      RETURN NIL
   ENDIF
   ?? DBINFO( DBI_FULLPATH )

   // ���� ��� ����������� ��� � ���� - TsbViewer.prg
   // this code is already executed in the window - TsbViewer.prg
   bInitForm  := {|ow,ob|
                  Local oc, cw, i, cv, ns := 0
                  cw := ow:Name
                  ? ProcNL()
                  ?? "===>>> bInitForm:", cw, ob:cControlName
                  oc := ob:GetColumn("ORDKEYNO")
                  oc:nAlign  := DT_RIGHT
                  oc:nFAlign := oc:nAlign
                  oc:nSAlign := oc:nAlign
                  oc:cSpcHeading += Space( ob:nCellMarginLR )
                  FOR EACH oc IN ob:aColumns
                     //? "      ...",hb_enumIndex(oc), oc:cName, oc:lEdit, oc:lCheckBox
                  NEXT
                  //
                  FOR i := 1 TO ob:nLen
                     ob:GotoRec(i)
                     ns += ob:GetValue("TAXRATE")
                  NEXT
                  ob:GoTop()
                  cv := "Amount by field [TAXRATE] = " + HB_NtoS(ns)
                  // "LblDown" - ����� �� oWin:aDown
                  ob:GotoRec(20)
                  cv += SPACE(10) + "Jump to record 20  /  oBrw:GotoRec(20) "
                  IF HB_IsArray(oWin:aDown)
                     IF LEN(oWin:aDown) >=6
                        cv += oWin:aDown[6]
                     ENDIF
                  ENDIF
                  IF HB_IsArray(ow:Cargo:aCurrLang)
                     cv += " [����������� ���� "
                     cv += ow:Cargo:aCurrLang[3]+"]"
                  ENDIF
                  IF GetControlIndex("LblDown", cw ) > 0
                     SetProperty(cw, "LblDown", "Value", cv)
                  ENDIF
                  Return Nil
                 }

   cMsg := "hb_SetCodepage()= " + hb_SetCodepage() + ";"
   cMsg += "hb_CdpSelect()  = " + hb_CdpSelect() + ";"
   cMsg += "hb_LangSelect() = " + hb_LangSelect() + ";"
   //cMsg += ";" + ProcNL()
   AlertInfo(cMsg)
   ? ProcNL(), cMsg

   // ������� ���� �������� � �������
   WaitThreadAviClose( aThrd )
   // ���� � �������� - ����� ���� ������� �� �������� � oWin
   TsbObjViewer(oWin, oUse, oIndx, oMenu, oTsb, aEvent, bInitForm)
   // ��������� �������� � \MiniGUI\SAMPLES\Advanced\Tsb_Viewer

   IF SELECT(oUse:cAlias) > 0
      (oUse:cAlias)->(dbCloseArea())  // ������� ����
   ENDIF
   ? ProcNL(), "--- END ------ Alias:", ALIAS()

   IF _IsWindowDefined( cWndMain )
      // ������� ������� ����
      Domethod(cWndMain, "Restore")
   ENDIF
   DO EVENTS

   SET LANGUAGE TO RUSSIAN
   SET CODEPAGE TO RUSSIAN

RETURN Nil

///////////////////////////////////////////////////////////////////////////////
FUNCTION my3Btn1(ow,ky,cn,ob)

   MsgDebug(ow:Name,ky,cn,This.&(cn).caption)

   IF ky == 1         // ������ 1
      myLoadTableCnf(ow)
   ELSEIF ky == 2     // ������ 2
      myTable4menu(ow,ky,cn,ob)
   ELSEIF ky == 3     // ������ 3
      myGetLang("����� ���� � ���� ?","SAY")  // TsbViewMenu.prg
   ENDIF

   ow:Enabler(cn, .T.)
   ob:Setfocus()
   DO EVENTS

RETURN Nil

///////////////////////////////////////////////////////////////////////////////
FUNCTION CnfgDataWin(oIni, cSection)
   LOCAL oWin, aColor, aVal, nI

   oWin := oHmgData()
   oWin:cIcon      := GetIniData( oIni, cSection, "Win_cIcon"  , ""  )       ; ? "   Win_cIcon", oWin:cIcon
   oWin:lWait      := GetIniData( oIni, cSection, "Win_lWait"  , .T. ) // .T.-"WAIT", .F.="NOWAIT"
   oWin:lCenter    := GetIniData( oIni, cSection, "Win_lCenter", .F. )
   oWin:nPosY      := GetIniData( oIni, cSection, "Win_nPosY"  , 20, .t. )   ; ? "   Win_nPosY", oWin:nPosY
   oWin:nPosX      := GetIniData( oIni, cSection, "Win_nPosX"  , 20, .t. )   ; ? "   Win_nPosX", oWin:nPosX
   oWin:nPosW      := GetIniData( oIni, cSection, "Win_nPosW"  , -1  )
   oWin:nPosH      := GetIniData( oIni, cSection, "Win_nPosH"  , -1  )
   IF oWin:nPosW < 0
      oWin:nPosW   := App.Cargo:aDisplayMode[1]  //System.ClientWidth
   ENDIF
   IF oWin:nPosH < 0
      oWin:nPosH   := App.Cargo:aDisplayMode[2]  //System.ClientHeight
      oWin:nPosH   -= GetTaskBarHeight()         // ������ ������ ����� Desktop
   ENDIF

   oWin:aBackcolor := GetIniData( oIni, cSection, "Win_aBackcolor", RED       , .T. )
   oWin:cTitle     := GetIniData( oIni, cSection, "Win_cTitle"    , "no-Title", .T. )
   oWin:lTopmost   := GetIniData( oIni, cSection, "Win_lTopmost"  , .F.             ) // This.Topmost := lTopmost, ���� .T. �� ������������� �� ������ ���� ����� ������
   oWin:bOnInit    := Nil      // �� ��������� ������� � TsbViewer.prg
   oWin:bOnRelease := {||Nil}  // �� ��������� ������� � TsbViewer.prg
   oWin:bIAClose   := {||Nil}  // �� ��������� ������� � TsbViewer.prg
   oWin:aDown      := {}       // ��� label ����� ����
   // ���� label ����� ���� {���-label, ������, ���� ����, ���� ������, ���������, ��� �����}
   aVal            := { "LblDown", GetTitleHeight()+1, WHITE, {42,97,181}, .T., "! ����� ����� ���-�� �������� ...." }
   oWin:aDown      := GetIniData( oIni, cSection, "Win_aDown", aVal )
   // ������ ������� � ������� oWin:aDown
   IF HB_IsArray(oWin:aDown)
      aVal := oWin:aDown
      FOR nI := 1 TO LEN(aVal)
         aVal[nI] := myStrFuncValue(aVal[nI])
      NEXT
      oWin:aDown := aVal
   ENDIF

   // ����� ����� ���� ������ ������ [4] ��� Win10
   oWin:lClrDown4  := GetIniData( oIni, cSection, "Win_lClrDown4", .F. )
   IF oWin:lClrDown4
      aVal       := oWin:aDown
      aColor     := aVal[4]
      aVal[4]    := IIF( IsWin10OrLater(), Color10_ActiveCaption(), aColor )
      oWin:aDown := aVal
   ENDIF

RETURN oWin

///////////////////////////////////////////////////////////////////////////////
FUNCTION CnfgDataDbf(oIni, cSection, cPath, cAlsNew)
   LOCAL oUse, cMsg, oError, cOld, cAls, cCodePage, lShared, cUse, oSec
   DEFAULT cAlsNew := ""

   oUse := oHmgData()
   IF Empty( oSec := oIni:Get(cSection) )    // ��� ������
      cMsg := "��� ������ [" + cSection + "] � Demo_timer.cnfg !"
      cMsg += ";;" + ProcNL() + ";" + ProcNL(1)
      AlertStop(cMsg)
      oUse:cFile  := "none-section.dbf"
      oUse:cError := AtRepl( ";", cMsg, CRLF )
      oUse:cPath  := cPath
      oUse:cAlias := ""
      RETURN oUse
   ENDIF

   oSec := oIni:Get(cSection)
   //_o2Log(oSec,  20, "==> .T. =========["+cSection+"]======== oSec: ", .T.)
   ? ProcNL(), "=========[" + cSection + "]========"
   oUse:cFile     := oSec:Get("Dbf_File"    , "none.dbf" )  ;  ? "   Dbf_File     =", oUse:cFile
   oUse:cAlias    := oSec:Get("Dbf_Alias"   , "TEMP"     )  ;  ? "   Dbf_Alias    =", oUse:cAlias
   oUse:cCodePage := oSec:Get("Dbf_CodePage", "RU866"    )  ;  ? "   Dbf_CodePage =", oUse:cCodePage
   oUse:lShared   := oSec:Get("Dbf_Shared"  , .F.        )  ;  ? "   Dbf_Shared   =", oUse:lShared
   IF LEN(cAlsNew) > 0
      oUse:cAlias := cAlsNew  // �������������� �����
   ELSE
      // ����� ����� �� Demo_timer.cfg
   ENDIF
   // ��� ����� ���
   //oUse:cFile     := GetIniData( oIni, cSection, "Dbf_File"    , "none.dbf" ,.T.)
   //oUse:cAlias    := GetIniData( oIni, cSection, "Dbf_Alias"   , "TEMP"     ,.T.)
   //oUse:cCodePage := GetIniData( oIni, cSection, "Dbf_CodePage", "RU866"    ,.T.)
   //oUse:lShared   := GetIniData( oIni, cSection, "Dbf_Shared"  , .T.        ,.T.)

   oUse:cError    := ""
   oUse:cPath     := cPath
   oUse:cFullPath := cPath + oUse:cFile
   cUse           := cPath + oUse:cFile
   cAls           := oUse:cAlias
   cCodePage      := oUse:cCodePage
   lShared        := oUse:lShared

   BEGIN SEQUENCE  WITH { |e|break( e ) }
      cOld := hb_cdpSelect(cCodePage)
      IF hb_cdpSelect() == cCodePage
         // ���� ����� ������� ��������
         // there is such a code page
      ENDIF
      hb_cdpSelect(cOld)
   RECOVER USING oError
      cMsg := "Code page error!;"
      cMsg += "No driver for CodePage: "
      cMsg += cCodePage + ";" + ProcNL()
      AlertStop( cMsg, "ERROR")
      oUse:cError := cMsg
      oUse:cAlias := ""
      oUse:lOpen  := .F.
      RETURN oUse
   END SEQUENCE

   IF hb_FileExists( cUse )

      BEGIN SEQUENCE  WITH { |e|break( e ) }

         IF lShared
            USE (cUse) ALIAS ( cAls ) CODEPAGE cCodePage SHARED NEW
         ELSE
            USE (cUse) ALIAS ( cAls ) CODEPAGE cCodePage EXCLUSIVE NEW
         ENDIF
         oUse:lOpen  := .T.

      RECOVER USING oError

         cMsg := "Error opening Database!;"
         cMsg += "The Database is occupied by another process;"
         cMsg += cUse + ";" + ProcNL()
         AlertStop( cMsg, "ERROR")
         oUse:cError := cMsg
         oUse:cAlias := ""
         oUse:lOpen  := .F.

      END SEQUENCE

   ELSE

      cMsg := 'File not found !;' + cUse + ";" + ProcNL()
      //AlertStop(cMsg, "ERROR")
      oUse:cAlias := ""
      oUse:lOpen  := .F.
      oUse:cError := cMsg

   ENDIF

RETURN oUse

///////////////////////////////////////////////////////////////////////////////
FUNCTION Tsb3DataIndex(oUse,cPathTemp)                    // ������� �������
   LOCAL oIndx, cIndex, cAls, cMsg, nI, cOn, cFor, cTag

   oIndx  := oHmgData()
   cIndex := ChangeFileExt( oUse:cFullPath, '.cdx' )
   cIndex := cPathTemp + cFileNoPath( cIndex )
   DeleteFile(cIndex)   // ����������� �������
   cAls   := oUse:cAlias

   oIndx:cIndex    := cIndex
   oIndx:aFor      := { ""         , "!Deleted()"   , "Deleted()"    , "CUSTNO>0"    }
   oIndx:aTag      := { "PRINT"    , "NO_DEL"       , "DEL"          , "Except_zero" }
   oIndx:aIndxOn   := { "CUSTNO"   , "CUSTNO"       , "CUSTNO"       , "CUSTNO"      }
   oIndx:cError    := ""
   oIndx:nSetOrder := 0

   IF LEN(cAls) > 0  // ���� ���� �������

      IF !hb_DirExists( cPathTemp )
         cMsg := "Couldn't create indexes !; There is no such path for files - "
         cMsg += cPathTemp + ";" + ProcNL()
         //AlertStop(cMsg, "ERROR")
         oIndx:cError  := cMsg
      ELSE
         dbSelectArea( cAls )
         FOR nI := 1 TO LEN(oIndx:aFor)
            cOn  := oIndx:aIndxOn[nI]
            cTag := oIndx:aTag[nI]
            cFor := oIndx:aFor[nI]
            IF LEN(cFor) == 0
               INDEX ON &cOn TAG (cTag) TO (cIndex) DESCENDING
            ELSE
               INDEX ON &cOn TAG (cTag) TO (cIndex) FOR &cFor DESCENDING
            ENDIF
         NEXT
         oIndx:nSetOrder := 1  // ��� ��� DbSetOrder(1)
      ENDIF

   ELSE
      cMsg := "Couldn't create indexes !; Database is not open!;"  + ProcNL()
      //AlertStop(cMsg, "ERROR")
      oIndx:cError  := cMsg
   ENDIF

RETURN oIndx

///////////////////////////////////////////////////////////////////////////////
FUNCTION CnfgDataMenu(oIni, cSection)             // ����-������ ����
   LOCAL oMenu, nWMenu, nKolvo, a5Fnt, cMsg, nI, aVal, cVal, xVal

   oMenu := oHmgData()
   oMenu:lDebug    := GetIniData( oIni, cSection, "Menu_lDebug"   , .T. )       // �������, ����� ������
   oMenu:nPosWin   := GetIniData( oIni, cSection, "Menu_nPosWin"  , 1   )       // 1-TopWindow, 2-BottomWindow, 3-LeftWindow, 4-RightWindow
   oMenu:nHAlign   := GetIniData( oIni, cSection, "Menu_nHAlign"  , 0   )       // �������������� ������: 0-LEFT, 1-CENTER, 2-RIGHT
   oMenu:nVAlign   := GetIniData( oIni, cSection, "Menu_nVAlign"  , 0   )       // ������������ ������: 0-TOP , 1-CENTER, 2-BOTTOM
   oMenu:aCaption  := GetIniData( oIni, cSection, "Menu_aCaption" , {}  )       // ������ ����
   oMenu:aBtnPost  := GetIniData( oIni, cSection, "Menu_aBtnPost" , {}  )       // _wPost(�) - ����� ������� �� ������
   oMenu:aBColor   := GetIniData( oIni, cSection, "Menu_aBColor"  , {}  )       // ������ ���� ����
   oMenu:lBtnIco   := GetIniData( oIni, cSection, "Menu_lBtnIco"  , .T. )       // F-������ ��� ������
   oMenu:aIcon     := GetIniFor ( oIni, cSection, "Menu_Icon_"    , {}  , .F.)  // ������ ������
   oMenu:nIcoSize  := GetIniData( oIni, cSection, "Menu_nIcoSize" , 48  )       // ��������� ������ �� 48�48
   oMenu:lTextVert := GetIniData( oIni, cSection, "Menu_lTextVert", .F. )       //.T.-������������ ����� ��� ������
   oMenu:lTextLeft := GetIniData( oIni, cSection, "Menu_lTextLeft", .T. )       //.F.-����� ����� ��� ������
   a5Fnt           := { "Comic Sans MS", 15, .T., .F. , 17, "���������� ����� ������" }
   oMenu:aFont     := GetIniData( oIni, cSection, "Menu_aFont"   , a5Fnt          )   // ���� �� �������
   oMenu:aFClr     := GetIniData( oIni, cSection, "Menu_aFClr"   , {BLACK,YELLOW} )   // ���� ����� �� �������
   oMenu:aHelp     := GetIniData( oIni, cSection, "Menu_aHelp"   , {}             )   // Tooltip ������
   oMenu:nIndent   := GetIniData( oIni, cSection, "Menu_nIndent" , 0              )   // ������ ������ ������  - ������
   oMenu:nHBtn     := GetIniData( oIni, cSection, "Menu_nHBtn"   , 54             )   // ������ ������
   oMenu:nWBtn     := GetIniData( oIni, cSection, "Menu_nWBtn"   , 220            )   // ������ ������
   oMenu:nGaps     := GetIniData( oIni, cSection, "Menu_nGaps"   , 5              )   // ������ ������ �� ���� ����
   oMenu:nGapsBtn  := GetIniData( oIni, cSection, "Menu_nGapsBtn", 15             )   // ����� �������� �� ������/������
   oMenu:aPost     := oMenu:aBtnPost // ��� ������� �� ������� �������� ���� AAdd( aEvent, { aPost[1]....

   // ������ ����� �� �������
   aVal := oMenu:aCaption
   FOR nI := 1 TO LEN(aVal)
       aVal[nI] := AtRepl( "|", aVal[nI], ";" )
   NEXT

   // ������ ����� �� �������
   cVal := ""
   aVal := oMenu:aBColor
   FOR nI := 1 TO LEN(aVal)
      xVal := aVal[nI]
      IF HB_IsArray(xVal)
      ELSEIF HB_IsChar(xVal)
         IF AT("()", xVal ) > 0
            aVal[nI] := myStrFuncValue(aVal[nI])
         ENDIF
      ELSEIF HB_IsNumeric(xVal)
      ENDIF
      IF !HB_IsArray(aVal[nI])
         cVal += "oMenu:aBColor[" + HB_NtoS(nI) + "] = "
         cVal += cValToChar(xVal) +  ";"
      ENDIF
   NEXT
   oMenu:aBColor := aVal
   IF LEN(cVal) > 0
      cMsg := "ERROR !; ������ ������ ���� ����� {XXX,XXX,XXX} !;"
      cMsg += cVal + ";;" + ProcNL() + ";" + ProcNL(1)
      AlertStop(cMsg)
   ENDIF

   // �������� ������� ��� ������
   IF LEN(oMenu:aCaption) # LEN(oMenu:aBtnPost) .AND. LEN(oMenu:aCaption) # LEN(oMenu:aBColor) .AND. ;
      LEN(oMenu:aCaption) # LEN(oMenu:aIcon)
      cMsg := "ERROR !; ������� ������ �� ����� !"
      cMsg += '(' + HB_NtoS(LEN(oMenu:aCaption)) + ') oMenu:aCaption - "Menu_aCaption" - ������ ������ ����;'
      cMsg += '(' + HB_NtoS(LEN(oMenu:aBtnPost)) + ') oMenu:aBtnPost - "Menu_aBtnPost" - ����� ������� �� ������;'
      cMsg += '(' + HB_NtoS(LEN(oMenu:aBColor )) + ') oMenu:aBColor  - "Menu_aBColor"  - ������ ����� ����;'
      cMsg += '(' + HB_NtoS(LEN(oMenu:aIcon   )) + ') oMenu:aIcon    - "Menu_Icon_"    - ������ ������;'
      cMsg +=  ";" + ProcNL() + ";" + ProcNL(1)
      AlertStop(cMsg)
   ENDIF

   // �������� ������ ���� ������
   nKolvo := LEN(oMenu:aCaption)
   nWMenu := oMenu:nGaps * 2 + oMenu:nWBtn * nKolvo + oMenu:nGapsBtn * nKolvo
   IF nWMenu > App.Cargo:aDisplayMode[1]  //System.ClientWidth
      oMenu:nWBtn := ( App.Cargo:aDisplayMode[1] - oMenu:nGaps*2 - oMenu:nGapsBtn * nKolvo ) / nKolvo
   ENDIF

   IF oMenu:nPosWin == 1 .OR. oMenu:nPosWin == 2
      // ��� 1-TopWindow, 2-BottomWindow
      oMenu:nHMenu   := oMenu:nHBtn + oMenu:nGaps * 2      // ������ ����� ����
   ELSE
      // ���  3-LeftWindow, 4-RightWindow
      oMenu:nHMenu   := oMenu:nWBtn + oMenu:nGaps * 2      // ������ ����� ����
   ENDIF

RETURN oMenu

///////////////////////////////////////////////////////////////////////////////
FUNCTION CnfgDataTsb(oIni, cSection, oUse, oWin)      // ��������� ���
   LOCAL aHead, aFSize, aFoot, aPict, aAlign, aName, aField, aFAlign, nAlgn
   LOCAL aDbf, nJ, nK, aEdit, cErr, cFld, cVal, cAls, cMsg, oTsb, cTmp, cTyp
   LOCAL aBColor, nBColor, nClr, nGrad, cRun, bClrDef, aClrDef

   oTsb   := CreateDateTsb(oUse,oUse:cCodePage,"Checkpoint (1) !",oWin)  // ��������� ���

   aBColor := oWin:aBackcolor
   nBColor := RGB( aBColor[1], aBColor[2], aBColor[3] )

   oTsb := oHmgData()
   oTsb:nWGaps       := GetBorderWidth()        // ������ �� ������ �����
   oTsb:nHGaps       := GetBorderHeight()       // ������ �� ������ �����
   oTsb:cError       := ""
   // ��������� �������
   oTsb:lSelector    := GetIniData( oIni, cSection, "Tsb_lSelector"   , .T.        )  // F-������ � ������� ����.������� SELECTOR
   oTsb:lColNumber   := GetIniData( oIni, cSection, "Tsb_lColNumber"  , .T.        )  // F-������ � ������� ����.������� ORDKEYNO
   oTsb:aColNumber   := GetIniData( oIni, cSection, "Tsb_aColNumber"  , { 1, 60 }  )  // ����.������� � �������� - ����� ��������� ������������� ������ �������
   oTsb:lSuperHead   := GetIniData( oIni, cSection, "Tsb_lSuperHead"  , .T.        )  // F-������ � ������� ����������
   oTsb:lSpecHd      := GetIniData( oIni, cSection, "Tsb_lSpecHd"     , .T.        )  // F-������ � ������� ���������
   oTsb:cSupHd1Title := GetIniData( oIni, cSection, "Tsb_cSuperHead1" , oUse:cCodePage     )
   oTsb:cSupHd2Title := GetIniData( oIni, cSection, "Tsb_cSuperHead2" , "Checkpoint (1) !" )

   ? "//////////// ����� ������� /////", ProcNL()
   oTsb:lShowZebra   := GetIniData( oIni, cSection, "Tsb_lShowZebra", .F. )  // ����� ������\�������� ������
   ? SPACE(3)+"oTsb:lShowZebra =", oTsb:lShowZebra, "����� ������\�������� ������"

   bClrDef        := {|| myRGB('CLR_WHITE') }
   cRun           := GetIniData( oIni, cSection, "Tsb_nClr22Bck", bClrDef )  // ���� ���� ������\�������� ������ ��� Tsb_lShowZebra = .T.
   oTsb:nClr22Bck := EVal(cRun)
   ? SPACE(3)+"oTsb:nClr22Bck =", cRun, oTsb:nClr22Bck, HB_ValToExp(n2RGB(oTsb:nClr22Bck)), "���� ���� ������\�������� ������ ��� Tsb_lShowZebra = .T."

   oTsb:aBrush    := GetIniData( oIni, cSection, "Tsb_aBrush"   , {60,60,60} ) // ��� �������� - ���� ���� ����
   ? SPACE(3)+"oTsb:aBrush =", HB_ValToExp( oTsb:aBrush )

   bClrDef        := {|| myRGB({128,128,128},.T.) }
   cRun           := GetIniData( oIni, cSection, "Tsb_nClrNoDbf", bClrDef ) // ���������/����������/����.�������
   oTsb:nClrNoDbf := EVal(cRun) //&cRun
   ? SPACE(3)+"oTsb:nClrNoDbf =", cRun, oTsb:nClrNoDbf, HB_ValToExp(n2RGB(oTsb:nClrNoDbf))

   bClrDef         := {|| myRGB({255,0,0},.T.) }
   cRun            := GetIniData( oIni, cSection, "Tsb_nClrNoEdit", bClrDef ) // �����/������ ������� ���� "+=^"
   oTsb:nClrNoEdit := EVal(cRun) //&cRun
   ? SPACE(3)+"oTsb:nClrNoEdit =", cRun, oTsb:nClrNoEdit, HB_ValToExp(n2RGB(oTsb:nClrNoEdit))

   bClrDef          := {|| myRGB({50,50,50},.T.) }
   cRun             := GetIniData( oIni, cSection, "Tsb_nClrBackDel", bClrDef ) // ���� �������� �������
   oTsb:nClrBackDel := EVal(cRun) //&cRun
   ? SPACE(3)+"oTsb:nClrBackDel =", cRun, oTsb:nClrBackDel, HB_ValToExp(n2RGB(oTsb:nClrBackDel))

   bClrDef          := {|| myRGB('CLR_GRAY') }
   cRun             := GetIniData( oIni, cSection, "Tsb_nClrForeDel", bClrDef ) // ����� �������� �������
   oTsb:nClrForeDel := EVal(cRun) //&cRun
   ? SPACE(3)+"oTsb:nClrForeDel =", cRun, oTsb:nClrForeDel, HB_ValToExp(n2RGB(oTsb:nClrForeDel))

   bClrDef         := {|| myRGB('CLR_BLUE') }
   cRun            := GetIniData( oIni, cSection, "Tsb_nClr1Fore", bClrDef )  // 1 , ����� � ������� �������
   oTsb:nClr1Fore  := EVal(cRun)
   ? SPACE(3)+"oTsb:nClr1Fore =", cRun, oTsb:nClr1Fore, HB_ValToExp(n2RGB(oTsb:nClr1Fore))

   bClrDef         := {|| myRGB('CLR_WHITE') }
   cRun            := GetIniData( oIni, cSection, "Tsb_nClr2Back", bClrDef )  // 2 , ���   � ������� �������
   oTsb:nClr2Back  := EVal(cRun)
   ? SPACE(3)+"oTsb:nClr2Back =", cRun, oTsb:nClr2Back, HB_ValToExp(n2RGB(oTsb:nClr2Back))

   bClrDef         := {|| myRGB('CLR_YELLOW') }
   cRun            := GetIniData( oIni, cSection, "Tsb_nClr3Fore", bClrDef )  // 3 , ������ ����� �������
   oTsb:nClr3Fore  := EVal(cRun)
   ? SPACE(3)+"oTsb:nClr3Fore =", cRun, oTsb:nClr3Fore, HB_ValToExp(n2RGB(oTsb:nClr3Fore))

   ? SPACE(3)+"oTsb:nClr4Back = { 1N, 2N } "
   nClr            := RGB(40,122,237)
   nGrad           := RGB(48,29,26)
   ?? "{",nClr,",",nGrad, "}"
   aClrDef         := { nClr , nGrad  }
   oTsb:nClr4Back  := GetIniData( oIni, cSection, "Tsb_nClr4Back", aClrDef )  // 4 , ���� ����� �������
   ?? HB_ValToExp(oTsb:nClr4Back)

   bClrDef         := {|| myRGB('CLR_YELLOW') }
   cRun            := GetIniData( oIni, cSection, "Tsb_nClr9Fore", bClrDef )  // 9 , ������ ������� �������
   oTsb:nClr9Fore  := EVal(cRun)
   ? SPACE(3)+"oTsb:nClr9Fore =", cRun, oTsb:nClr9Fore, HB_ValToExp(n2RGB(oTsb:nClr9Fore))

   ? SPACE(3)+"oTsb:nClr10Back = { 1N, 2N } "
   nClr            := RGB(40,122,237)
   nGrad           := RGB(48,29,26)
   ?? "{",nClr,",",nGrad, "}"
   aClrDef         := { nClr , nGrad  }
   oTsb:nClr10Back  := GetIniData( oIni, cSection, "Tsb_nClr10Back", aClrDef )  // 10, ���� ������� �������
   ?? HB_ValToExp(oTsb:nClr10Back)

   bClrDef         := {|| myRGB('CLR_WHITE') }
   cRun            := GetIniData( oIni, cSection, "Tsb_nClr17Fore", bClrDef )   // 17, ������ �����������
   oTsb:nClr17Fore := EVal(cRun)
   ? SPACE(3)+"oTsb:nClr17Fore =", cRun, oTsb:nClr17Fore, HB_ValToExp(n2RGB(oTsb:nClr17Fore))

   ? SPACE(3)+"oTsb:nClr16Back = { 1N, 2N } "
   nClr            := RGB(40,122,237)
   nGrad           := RGB(48,29,26)
   ?? "{",nClr,",",nGrad, "}"
   aClrDef         := { nClr , nGrad  }
   oTsb:nClr16Back  := GetIniData( oIni, cSection, "Tsb_nClr16Back", aClrDef )  // 16, ���� �����������
   ?? HB_ValToExp(oTsb:nClr16Back)

   bClrDef          := {|| myRGB('CLR_RED') }
   cRun             := GetIniData( oIni, cSection, "Tsb_n1Clr17Fore", bClrDef )   // 1.17, ������ �����������  ������� 1
   oTsb:n1Clr17Fore := EVal(cRun)
   ? SPACE(3)+"oTsb:n1Clr17Fore =", cRun, oTsb:n1Clr17Fore, HB_ValToExp(n2RGB(oTsb:n1Clr17Fore))

   ? SPACE(3)+"oTsb:n1Clr16Back = { 1N, 2N } "
   nClr             := RGB(247,172,8)
   nGrad            := RGB(86,211,83)
   ?? "{",nClr,",",nGrad, "}"
   aClrDef          := { nClr , nGrad  }
   oTsb:n1Clr16Back := GetIniData( oIni, cSection, "Tsb_n1Clr16Back", aClrDef )  // 1.16, ���� ����������� ������� 1
   ?? HB_ValToExp(oTsb:n1Clr16Back)

   ? "// ----- ������� 1-2 -------"
   ? SPACE(3)+"oTsb:n12Clr4Back = { 1N, 2N } TEST={" , CLR_GREEN, CLR_YELLOW, "} ,"
   nClr             := GetSysColor( COLOR_BTNFACE )     // ���������/����������/����.�������
   nGrad            := GetSysColor( COLOR_BTNFACE )     // ���������/����������/����.�������
   aClrDef          := { nClr , nGrad  }
   ?? HB_ValToExp(aClrDef)
   oTsb:n12Clr4Back := GetIniData( oIni, cSection, "Tsb_n12Clr4Back", aClrDef )  //  4 , ���� ����� ������� ������� 1-2
   ?? HB_ValToExp(oTsb:n12Clr4Back)

   ? SPACE(3)+"oTsb:n12Clr10Back = { 1N, 2N } "
   oTsb:n12Clr10Back := GetIniData( oIni, cSection, "Tsb_n12Clr10Back", aClrDef )  //  10, ���� ������� ������� ������� 1-2
   ?? HB_ValToExp(oTsb:n12Clr10Back)

   bClrDef          := {|| myRGB('CLR_RED') }
   cRun             := GetIniData( oIni, cSection, "Tsb_n12Clr3Fore", bClrDef )   // 3 , ������ ����� ������� ������� 1-2
   oTsb:n12Clr3Fore := EVal(cRun)
   ? SPACE(3)+"oTsb:n12Clr3Fore =", cRun, oTsb:n12Clr3Fore, HB_ValToExp(n2RGB(oTsb:n12Clr3Fore))

   cRun             := GetIniData( oIni, cSection, "Tsb_n12Clr9Fore", bClrDef )   // 9 , ������ ������� ������� ������� 1-2
   oTsb:n12Clr9Fore := EVal(cRun)
   ? SPACE(3)+"oTsb:n12Clr9Fore =", cRun, oTsb:n12Clr9Fore, HB_ValToExp(n2RGB(oTsb:n12Clr9Fore))

   // ����� �������
   bClrDef          := {|| myRGB({5,5,5},.T.) * -1 }          // ������ ���������
   cRun             := GetIniData( oIni, cSection, "Tsb_nClrFocus1", bClrDef )
   oTsb:nClrFocus1  := EVal(cRun)
   ? SPACE(3)+"oTsb:nClrFocus1 =", cRun, oTsb:nClrFocus1, HB_ValToExp(n2RGB(oTsb:nClrFocus1))

   bClrDef          := {|| myRGB("CLR_HRED") * -1 }          // ������� ���������
   cRun             := GetIniData( oIni, cSection, "Tsb_nClrFocus2", bClrDef )
   oTsb:nClrFocus2  := EVal(cRun)
   ? SPACE(3)+"oTsb:nClrFocus2 =", cRun, oTsb:nClrFocus2, HB_ValToExp(n2RGB(oTsb:nClrFocus2))

   bClrDef          := {|| myRGB("CLR_SKYPE") * -1 }          // ��������� ��� ������
   cRun             := GetIniData( oIni, cSection, "Tsb_nClrNoFocus1", bClrDef )
   oTsb:nClrNoFocus1:= EVal(cRun)
   ? SPACE(3)+"oTsb:nClrNoFocus1 =", cRun, oTsb:nClrNoFocus1, HB_ValToExp(n2RGB(oTsb:nClrNoFocus1))

   bClrDef          := {|| myRGB({128, 225, 225},.T.) * -1 }   // ��������� ��� ������
   cRun             := GetIniData( oIni, cSection, "Tsb_nClrNoFocus2", bClrDef )
   oTsb:nClrNoFocus2:= EVal(cRun)
   ? SPACE(3)+"oTsb:nClrNoFocus2 =", cRun, oTsb:nClrNoFocus2, HB_ValToExp(n2RGB(oTsb:nClrNoFocus2))

   //oTsb:nClrSeleF   := GetSysColor( COLOR_WINDOWTEXT )
   bClrDef          := {|| myRGB("COLOR_WINDOWTEXT",.T.) }
   cRun             := GetIniData( oIni, cSection, "Tsb_nClrSeleF", bClrDef )   //  ���� ������ ������ ������� ��� ������
   oTsb:nClrSeleF   := EVal(cRun)
   ? SPACE(3)+"oTsb:nClrSeleF =", cRun, oTsb:nClrSeleF, HB_ValToExp(n2RGB(oTsb:nClrSeleF)), myRGB("COLOR_WINDOWTEXT",.T.)

   ? SPACE(3)+"oTsb:nClrSelectorHdBack = { 1N, 2N } TEST={" , CLR_GREEN, ",", CLR_GREEN, "} ,"
   nClr             := GetSysColor( COLOR_BTNFACE )     // ���������/����������/����.�������
   nGrad            := GetSysColor( COLOR_BTNFACE )     // ���������/����������/����.�������
   aClrDef          := { nClr , nGrad  }
   ?? HB_ValToExp(aClrDef)
   // ���� ���� �����/������� ������� ������� 1 - Selector
   oTsb:nClrSelectorHdBack := GetIniData( oIni, cSection, "Tsb_nClrSelectorHdBack", aClrDef )
   ?? HB_ValToExp(oTsb:nClrSelectorHdBack)
   //
   // ������ � ���������  - ����� ��� ���-�� ������, � �� ��������
   //oTsb:aWidthCol  := { {"LOGPRN", -3}, {"CUSTNO", -5}, {"FAX", +2}, {"TAXRATE", -4} }
   oTsb:aWidthCol    := GetIniData( oIni, cSection, "Tsb_aWidthCol"  , {}  )  // �������� ���������  - ����� ��� ���-�� ������, � �� ��������

   cAls := oUse:cAlias
   cErr := ""
   /*AADD( aDbf, { "LOGPRN"     , "L",  1, 0, "Print;recno"         , .T. } )
   AADD( aDbf, { "CUSTNO"     , "N", 15, 0, "Company;number"      , .T. } )
   AADD( aDbf, { "COMPANY"    , "C", 30, 0, "Company"             , .T. } )
   AADD( aDbf, { "ADDR1"      , "C", 30, 0, "Adres-1"             , .T. } )
   AADD( aDbf, { "ADDR2"      , "C", 30, 0, "not-show"            , .T. } )
   AADD( aDbf, { "CITY"       , "C", 15, 0, "City"                , .T. } )
   AADD( aDbf, { "STATE"      , "C", 20, 0, "State"               , .T. } )
   AADD( aDbf, { "ZIP"        , "C", 10, 0, "Zip"                 , .T. } )
   AADD( aDbf, { "COUNTRY"    , "C", 20, 0, "Country"             , .T. } )
   AADD( aDbf, { "PHONE"      , "C", 15, 0, "Phone;company"       , .T. } )
   AADD( aDbf, { "FAX"        , "C", 15, 0, "Fax;company"         , .T. } )
   AADD( aDbf, { "TAXRATE"    , "N", 19, 4, "Taxrate"             , .T. } )
   AADD( aDbf, { "CONTACT"    , "C", 20, 0, "Contact"             , .T. } )
   AADD( aDbf, { "LASTINVOIC" , "C", 30, 0, "LAST INVOIC"         , .F. } )
   AADD( aDbf, { "LASTINVOIC" , "C", 30, 0, "not-show"            , .T. } ) */
   aDbf := GetIniFor( oIni, cSection, "Tsb_Column_" , {} )  // ������ ����� �������

   nK      := LEN(aDbf)
   aHead   := {}  // ������ ����� ������� �������
   aFoot   := {}  // ������ ������� �������
   aPict   := {}  // ������ ������� ������� �������
   aName   := {}  // ������ PICTURE ������� �������
   aAlign  := {}  // ������ ������� ������� �������
   aField  := {}  // ������ ����� ���� ������� �������
   aFSize  := {}  // ������ ����� ���� ������� �������
   aFAlign := {}  // ������ ������� ������� ������� �������
   aEdit   := {}  // �������������� ����

   IF LEN(cAls) > 0  // ���� ���� �������

      dbSelectArea( cAls )
      FOR nJ := 1 TO nK
         cFld := aDbf[nJ,1]
         cTyp := aDbf[nJ,2]
         cVal := aDbf[nJ,5]
         IF LOWER( cVal ) == "not-show"
            // �������
         ELSE
            IF FIELDNUM(cFld) == 0
               cVal := HB_ValToExp(aDbf[nJ])
               cVal := AtRepl( ";", cVal, "|" )
               cErr += HB_ValToExp(cVal) + ";"
            ELSE
               IF LEN(cVal) == 0
                  cTmp := cFld
               ELSE
                  cTmp := cVal
               ENDIF
               cTmp := AtRepl( "|", cTmp, ";" )
               AADD( aHead  , cTmp )
               AADD( aFoot  , "[ " + cFld + " ]" )
               AADD( aName  , cFld      )
               AADD( aField , cFld      )
               AADD( aFAlign, DT_CENTER )
               IF cTyp == 'C' .OR. cTyp == 'M'
                  nAlgn := DT_LEFT
               ELSEIF cTyp == 'N'
                  nAlgn := DT_RIGHT
               ELSE
                  nAlgn := DT_CENTER
               ENDIF
               AADD( aAlign , nAlgn )
               AADD( aEdit  , aDbf[nJ,6] )
            ENDIF
         ENDIF
      NEXT

      IF LEN(cErr) > 0
         cMsg := "No field in the database " + Alias() + " !;"
         cErr += ProcNL()
         //AlertStop( cMsg + cErr, "ERROR")
         oTsb:cError := cMsg + cErr
      ENDIF

      oTsb:aHead   := aHead
      oTsb:aFoot   := aFoot
      oTsb:aPict   := aPict
      oTsb:aName   := aName
      oTsb:aAlign  := aAlign
      oTsb:aField  := aField
      oTsb:aFSize  := aFSize
      oTsb:aFAlign := aFAlign
      oTsb:aEdit   := aEdit
      //oTsb:aEdit := .F.     // ������ ������ ���� �����, ���������� aEdit

   ENDIF

RETURN oTsb


///////////////////////////////////////////////////////////////////////////////
FUNCTION myLoadTableCnf(oWnd)
   LOCAL cFile, cTxt, cFind, cSect

   ? ProcNL(), oWnd:Name

   cFile := ChangeFileExt( Application.ExeName, ".cfg" )
   cTxt  := HB_MemoRead(cFile)
   cFind := "[�������_��_��������]"
   cSect := SUBSTR(cTxt, AT(cFind,UPPER(cTxt))-1 )
   cFind := ";////////////////"
   cSect := SUBSTR(cSect, 1, AT(cFind,cSect)-1 )
   cTxt  := AtRepl( ";", cSect, "|" )
   cFile := cFileNoPath(cFile)
   AlertInfo(cTxt, "�������� �����ֲ �� ����� " + cFile)
   ? cTxt
   ? "-----------------"

RETURN NIL
