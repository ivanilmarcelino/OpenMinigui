/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2021-23 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2021-23 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * ��������/������ Dbf �����. �����/�������� �� ����. ������� �� ���� �������
 * View/edit Dbf file. Options/properties by base. Objects on the table window
*/

#define  _HMG_OUTLOG           // ����� ������� � ����
#include "hmg.ch"
#include "tsbrowse.ch"
#include "Dbinfo.ch"

REQUEST HB_CODEPAGE_UA1251, HB_CODEPAGE_UA866    // ���������� ����
REQUEST HB_CODEPAGE_RU1251, HB_CODEPAGE_RU866    // ������� ����

#define  SHOW_TITLE  "TsbViewer(c)"
#define  SHOW_VERS   SPACE(5) + "Ver 1.3 - 02.02.25"
////////////////////////////////////////////////////////////////////////////////////
FUNCTION TsbObjViewer(oWin, oUse, oIndx, oMenu, oTsb, aEvent, bInitForm)
   LOCAL cWait, cAlias, nY, nX, nW, nH, lCenter, aBClr, lTopmost
   LOCAL bOnInit, bOnRele, bIClose, aWndDown, cIcoWin, aCurrLang
   LOCAL c1Title, c2Title, aTsbPar, aWinPar, cTtlWin

   nY        := oWin:nPosY
   nX        := oWin:nPosX
   nW        := oWin:nPosW
   nH        := oWin:nPosH
   cTtlWin   := oWin:cTitle
   cIcoWin   := IIF( oWin:cIcon == NIL, "", oWin:cIcon )
   lCenter   := oWin:lCenter
   aBClr     := oWin:aBackcolor
   cWait     := IIF( oWin:lWait, "WAIT", "NOWAIT" )
   lTopmost  := oWin:lTopmost
   bOnInit   := oWin:bOnInit
   bOnRele   := oWin:bOnRelease
   bIClose   := oWin:bIAClose
   aWndDown  := oWin:aDown       // label ����� ���� { ���, ������, ���� ����, ���� ������, ���������, ��� �����}
   aCurrLang := oWin:aCurrLang   // ������� ���� ���������, ���� �����
   IF !HB_IsArray(aCurrLang)
      aCurrLang := {}
   ENDIF

   cAlias   := oUse:cAlias
   c1Title  := oTsb:cSupHd1Title
   c2Title  := oTsb:cSupHd2Title

   aTsbPar  := { cAlias, c1Title, c2Title }
   aWinPar  := { cWait,cTtlWin,nY,nX,nW,nH,lCenter,aBClr,aWndDown,lTopmost,bOnInit,bOnRele,bIClose,cIcoWin }

   TsbViewer( aTsbPar, aWinPar, oUse, oIndx, oMenu, oTsb, aEvent, bInitForm, aCurrLang)

RETURN NIL

///////////////////////////////////////////////////////////////////////////////////////////////
FUNCTION TsbViewer( aTsbPar, aWinPar, oUse, oIndx, oMenu, oTsb, aEvent, bInitForm, aCurrLang)
   LOCAL nWBrw, nHBrw, nYBrw, nXBrw, nWGaps, nHGaps, cForm, oThis, lTsb2
   LOCAL cTitle, cIcon, lModal, cSetCP, cSelCdp, cLngSel, lTsb, aBClr
   LOCAL cWait, cAlias, cDbfCP, c2Title, nY, nX, nW, nH, lCenter, cPath
   LOCAL nSel, cErr, aPosiW, aEve, aEditFunc, cPrev := ALIAS()
   LOCAL lTopmst, bOnInit, bOnRele, bIClose, nHDown, aWndDown
   LOCAL cObj, cInitForm, lInitForm, lMainWnd, cMsg, cIcoWin
   LOCAL oBrw   // ��������� ������� ��� �������
   DEFAULT aTsbPar   := {}, aWinPar := {}
   DEFAULT oUse      := oHmgData(), oIndx := oHmgData()
   DEFAULT oMenu     := oHmgData(), oTsb  := oHmgData()
   DEFAULT aEvent    := {}
   DEFAULT aCurrLang := {}  // ��� ����� �������� ����� ���������

   DEFINE FONT DlgFont FONTNAME "DejaVu Sans Mono" SIZE 14   // for HMG_Alert()
   SET OOP ON    // �� ������ ������

   // ��������� ��� �������� - ������ public ����������
   IF !hb_IsObject(App.Cargo)
      App.Cargo := oHmgData()
   ENDIF
   App.Cargo:cMsgAbout := SHOW_TITLE
   App.Cargo:cVersion  := SHOW_VERS
   App.Cargo:aClrInfo  := { 183, 221, 232 }
   App.Cargo:cTempPath := myGetPathTemp()         // ��� ��������� ��������� �����

   // ��������� ����, ��.����
   myWinParam(aWinPar,@cWait,@cTitle,@nY,@nX,@nW,@nH,@lCenter,@aBClr,@aWndDown,@lTopmst,@bOnInit,@bOnRele,@bIClose,@cIcoWin)

   IF LEN(aTsbPar) == 0
      cAlias  := ALIAS()
      cDbfCP  := dbInfo( DBI_CODEPAGE )   // ������ ������� �������� ����
      c2Title := SHOW_TITLE
   ELSE
      cAlias  := aTsbPar[1]
      cDbfCP  := aTsbPar[2]
      c2Title := aTsbPar[3]
   ENDIF

   cForm   := "HMG_" + cAlias + "_" + HB_NtoS( _GetId() )
   IF LEN(cIcoWin) > 0
      cIcon := cIcoWin                 // ������� ������
   ELSE
      cIcon := Icon32TempCreate()      // -> TsbViewMisc.prg
      //cIcon := Icon64TempCreate()    // -> TsbViewMisc.prg
      IF !FILE(cIcon)
         cMsg := "Error ! Could not create icon resources for the table !;"
         cMsg += cIcon + ";;" + ProcNL() + ";" + ProcNL(1)
         AlertStop( cMsg, "Result", "ZZZ_B_STOP64", 64 )
      ENDIF
   ENDIF

   cSetCP  := hb_SetCodepage()
   cSelCdp := hb_CdpSelect()
   cLngSel := Hb_LangSelect()
   lTsb    := lTsb2 := .T.
   aPosiW  := { 0, 0, 0, 0 }

   IF LEN(cTitle) == 0 .AND. LEN(cAlias) > 0
      cPath  := DBINFO( DBI_FULLPATH )
      cPath  := IIF( !IsString(cPath) , "" , cPath )
      cTitle := cAlias + " " +  + " " + RddName()
      cTitle += " [" + cSetCP + "/" + cSelCdp + "/" + cLngSel + "]"
   ENDIF

   // ���������� ����������� ����
   IF bOnInit == Nil ; bOnInit := {|| This.Topmost := lTopmst, _wPost(0), iif(oBrw==Nil, Nil, oBrw:Setfocus()) }
   ENDIF
   IF bOnRele == Nil ; bOnRele := {|| iif( !Empty(cPrev), dbSelectArea(cPrev), Nil ) }
   ENDIF
   IF bIClose == Nil ; bIClose := {|| MG_YesNoQuit() }
   ENDIF

   // ���� ���� ������� ��������� ���� MODAL, �� ����� .T.
   lModal   := _HMG_IsModalActive
   lMainWnd := .F.

   IF Empty( _HMG_MainHandle )                // --- MAIN window ---
      DEFINE WINDOW &cForm AT nY,nX WIDTH nW HEIGHT nH TITLE cTitle   ;
         ICON cIcon MAIN TOPMOST NOMAXIMIZE NOSIZE BACKCOLOR aBClr    ;
         ON GOTFOCUS {|| App.Cargo:cFormGotFocus := This.Name,        ;
            myLangRecover(cAlias,cSetCP,cSelCdp,cLngSel) }            ; // ������� ������ �� �����
         ON LOSTFOCUS {|| myLangRecoverLost(This.Cargo:aCurrLang) }     // ������ ������ � �����
      This.OnInterActiveClose := bIClose
      lMainWnd := .T.
   ELSEIF lModal                              // --- MODAL window ---
      DEFINE WINDOW &cForm AT nY,nX WIDTH nW HEIGHT nH TITLE cTitle   ;
         ICON cIcon MODAL NOSIZE BACKCOLOR aBClr                      ;
         ON GOTFOCUS {|| App.Cargo:cFormGotFocus := This.Name,        ;
            myLangRecover(cAlias,cSetCP,cSelCdp,cLngSel) }            ; // ������� ������ �� �����
         ON LOSTFOCUS {|| myLangRecoverLost(This.Cargo:aCurrLang) }     // ������ ������ � �����
   ELSE                                    // --- STANDARD window ---
      DEFINE WINDOW &cForm AT nY,nX WIDTH nW HEIGHT nH TITLE cTitle   ;
         ICON cIcon WINDOWTYPE STANDARD TOPMOST NOMAXIMIZE NOSIZE     ;
         BACKCOLOR aBClr                                              ;
         ON GOTFOCUS  {|| App.Cargo:cFormGotFocus := This.Name,       ;
            myLangRecover(cAlias,cSetCP,cSelCdp,cLngSel) }            ; // ������� ������ �� �����
         ON LOSTFOCUS {|| myLangRecoverLost(This.Cargo:aCurrLang) }     // ������ ������ � �����
      This.OnInterActiveClose := bIClose
   ENDIF

      //bOnGotFocus := myLangRecover(cAlias,cSetCP,cSelCdp,cLngSel) // ������� ������ �� �����
      // _SetFormAction( This.Name , "OnGotFocus" , bOnGotFocus )
      // _SetFormAction( This.Name , "OnLostFocus" , bOnLostFocus )

      // ��������� ����������� ����
      This.OnInit    := bOnInit
      This.OnRelease := bOnRele

      oThis                := This.Object
      cForm                := This.Name
      This.Cargo           := oHmgData()           // ��������� ��� ����� ����
      This.Cargo:cMenu     := "Table"
      This.Cargo:aObjBtn   := {}
      This.Cargo:cForm     := cForm
      This.Cargo:oUse      := oUse
      This.Cargo:oIndx     := oIndx
      This.Cargo:oMenu     := oMenu
      This.Cargo:oTsb      := oTsb
      nW                   := This.ClientWidth
      nH                   := This.ClientHeight
      This.Cargo:aCurrLang := aCurrLang           // ������� ���� ���������, ���� �� ����, ����� {}

      // ������ �� ������ �����
      IF hb_IsNumeric(oTsb:nWGaps)
         nWGaps := oTsb:nWGaps
      ELSE
         nWGaps := GetBorderWidth()
      ENDIF
      // ������ �� ������ �����
      IF hb_IsNumeric(oTsb:nHGaps)
         nHGaps := oTsb:nHGaps
      ELSE
         nHGaps := GetBorderHeight()
      ENDIF
      // �������� �� ������
      IF hb_IsString(oTsb:cError) .AND. LEN(oTsb:cError) > 0
         lTsb := .F.  // ������
      ENDIF
      IF hb_IsString(oIndx:cError) .AND. LEN(oIndx:cError) > 0
         lTsb := .F.  // ������
      ENDIF

      IF lTsb  // ���� ��� ������
         aPosiW := TsbButtonMenu(oMenu,nWGaps,nHGaps,oThis)   // ���� ������ ��� �������
      ENDIF

      nYBrw := nHGaps + aPosiW[1]
      nXBrw := nWGaps + aPosiW[2]
      nWBrw := nW - nWGaps*2 + aPosiW[3]
      nHBrw := nH - nHGaps*2 + aPosiW[4]

      IF hb_IsArray(aWndDown)
         IF LEN(aWndDown) == 0    // ��� label ����� ����
         ELSE
            IF LEN(aWndDown) < 6
               // ���� label ����� ���� {������, ������, ���� ����, ���� ������, ���������, ��� �����}
               cErr := "ERROR ! "
               cErr += "oWin:aDown[6] - not true !"
               cErr += " aWndDown="
               MsgDebug(cErr, aWndDown)
            ELSE
               cObj   := aWndDown[1]
               nHDown := aWndDown[2]
               nHBrw  -= nHDown
               @ nHBrw + nHGaps*2 + aPosiW[1], 0 LABEL &cObj OF &cForm VALUE aWndDown[6] ;
                 WIDTH nW HEIGHT nHDown FONTCOLOR aWndDown[3] BACKCOLOR aWndDown[4] VCENTERALIGN
               IF aWndDown[5]
                  SetProperty(cForm, cObj, "Alignment", "CENTER")
               ENDIF
            ENDIF
         ENDIF
      ENDIF

      lTsb2 := .F.
      IF HB_ISCHAR( cAlias )
         IF LEN(cAlias) > 0
            nSel := Select(cAlias)
            IF nSel > 0
               dbSelectArea( cAlias )
               lTsb2 := .T.
            ENDIF
         ENDIF
      ENDIF

      IF lTsb .AND. lTsb2 // ���� ��� ������
         IF oIndx:nSetOrder # NIL
            DbSetOrder(oIndx:nSetOrder)
         ENDIF
         DbGotop()
         // ��������� ������ ��� �������
         oTsb := Tbrowse_OnInit(cAlias, cDbfCP, c2Title, oTsb)

         // ����� ������� ������� �� h_controlmisc2.prg
         oBrw := _TBrowse( oTsb, cAlias, , nYBrw, nXBrw, nWBrw, nHBrw )
         Tbrowse_Customization(oBrw, oTsb)           // ����������� �������
         This.Cargo:oBrw  := oBrw                    // ��������� ������ �������
         oTsb:aColFilter  :=  {}                     // ������� ������� � ��������
         oTsb:aColNumFltr :=  {}                     // ������ ������� ������� � ��������

         DO EVENTS
         oBrw:SetFocus()
         DO EVENTS
      ELSE
        cErr := "ERROR ! " + ProcNL() + ";;"
        cErr += oUse:cError  + ";;"
        cErr += oIndx:cError + ";;"
        cErr += oTsb:cError
        cErr := ATREPL( ";", cErr , CRLF )

        @ nHGaps, nWGaps LABEL Label_Err WIDTH nW-nWGaps*2 HEIGHT nH-nHGaps*2 ;
          VALUE cErr FONTCOLOR MAROON BOLD TRANSPARENT //CENTERALIGN

      ENDIF

      IF LEN(aEvent) > 0           // ������� �� ���� - ������ �����
         WITH OBJECT This.Object
            FOR EACH aEve IN aEvent
               :Event( aEve[1], aEve[2] )
            NEXT
         END WITH
      ENDIF

      // ��������� ������� ������� - ������� ������� - � �������� ��������� 29.06.23
      aEditFunc := oTsb:aEditFunc
      /*IF hb_IsArray(oTsb:aEditFunc)
         aEditFunc := oTsb:aEditFunc
         IF LEN(aEditFunc) > 0
            WITH OBJECT This.Object
                  :Event(100, {|ow,ky,ob| myFunc0(ow,ky,ob) })  // �������� demo3.prg
                  FOR nI := 1 TO LEN(aEditFunc)
                     :Event( aEditFunc[ nI ][2], aEditFunc[ nI ][3] ) // ������� �� ����
                  NEXT
            END WITH
         ENDIF
      ENDIF */

      ON KEY F1 ACTION {|| MsgAbout_TsbViewer() }
      //ON KEY ESCAPE ACTION {|| iif(oBrw==Nil, ThisWindow.Release,;
      //                              iif( oBrw:IsEdit, oBrw:SetFocus(), ThisWindow.Release ) ) }

      // ���.��������� ��� ����, oBrw, oTsb
      cInitForm := Valtype(bInitForm)
      IF HB_ISCHAR( bInitForm )
         IF ! ( "|...|" $ bInitForm .and. "(...)" $ bInitForm )
            bInitForm := "{|...| "+bInitForm+"(...)"
         ENDIF
         cInitForm := bInitForm
         bInitForm := &( bInitForm )
      ENDIF

      lInitForm := .F.
      IF HB_ISBLOCK( bInitForm )
         BEGIN SEQUENCE WITH { |e|break(e) }
            EVal( bInitForm, This.Object, oBrw, oTsb )
            lInitForm := .T.
         END SEQUENCE
         IF ! lInitForm
            cErr := "ERROR ! "
            cErr += "Must be a block of code !"
            cErr += " cInitForm="+cInitForm
            MsgDebug(cErr, bInitForm)
         ENDIF
      ENDIF

      WITH OBJECT This.Object
         :Event(  0, {|ow|// start ON INIT
                          Local p1, p2, cMsg, cTitle := This.Title
                          Local aLang := ow:Cargo:aCurrLang
                          // aCurrLang := { hb_SetCodepage(), hb_CdpSelect(), Hb_LangSelect() }
                          // ����������� ���� ���� ���� ���������� oWin:aCurrLang := aCurrLang
                          IF HB_IsArray(aLang)
                             IF LEN(aLang) >= 3
                                p1 := SUBSTR(aLang[3],1,AT(".",aLang[3])-1)
                                p2 := SUBSTR(aLang[3],AT(".",aLang[3])+1)
                                hb_LangSelect(p1,p2)
                                hb_SetCodepage(aLang[1])
                                hb_CdpSelect(aLang[2])
                                DO EVENTS
                                cTitle += SPACE(3)+ "{������������: "
                                cTitle += hb_LangSelect() + "}"
                                This.Title := cTitle
                             ENDIF
                          ENDIF
                          cMsg := "hb_SetCodepage()= " + hb_SetCodepage() + ";"
                          cMsg += "hb_CdpSelect()  = " + hb_CdpSelect() + ";"
                          cMsg += "hb_LangSelect() = " + hb_LangSelect() + ";"
                          cMsg += ";" + ProcNL()
                          //AlertInfo(cMsg)
                          DO EVENTS
                          Return Nil
                     } )
         // ���� ��� ������ � ������� Windows
         // menu for working with the Windows buffer
         :Event(10, {|ow,ky,ob| myRCellClick(ow,ky,ob) } )
      END WITH

   END WINDOW

   IF lMainWnd
      cWait := "WAIT"
   ENDIF

   IF lCenter
      CENTER WINDOW &cForm
   ENDIF

   IF UPPER(cWait) == "NOWAIT"
      ACTIVATE WINDOW &cForm NOWAIT ON INIT {|| This.Minimize, wApi_Sleep(50), ;
                                                This.Restore , DoEvents() }
   ELSE
      ACTIVATE WINDOW &cForm ON INIT {|| This.Minimize, wApi_Sleep(50), ;
                                         This.Restore , DoEvents()        }
   ENDIF

RETURN oThis

///////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myWinParam(aWinPar,cWait,cTitle,nY,nX,nW,nH,lCenter,aBClr,;
                                 aDown,lTopmst,bOnInit,bOnRele,bIClose,cIcoWin)
   LOCAL cErr

   IF LEN(aWinPar) == 0
      cWait   := "NOWAIT"
      cTitle  := ""
      nY      := nX := 0
      nW      := System.ClientWidth
      nH      := System.ClientHeight
      lCenter := .T.
      aBClr   := Nil
      lTopmst := .F.  // .T. - ����� ������������� ��� ��������� ����
      bOnInit := Nil
      bOnRele := Nil
      bIClose := Nil
      cIcoWin := ""
   ELSE
      IF LEN(aWinPar) < 6
         cErr := "Error ! Wrong number of parameters !;"
         cErr += "LEN(aWinPar) == " + HB_NtoS(LEN(aWinPar))
         cErr += " # aWinPar[8];"
         cErr += "aWinPar := { 'NOWAIT', '', nY, nX, nW, nH, .T./.F., COLOR };;"
         AlertStop(cErr + ProcNL(),"ERROR")
      ENDIF
      cWait   := aWinPar[1]
      cTitle  := aWinPar[2]
      nY      := aWinPar[3]
      nX      := aWinPar[4]
      nW      := aWinPar[5]
      nH      := aWinPar[6]
      IF nW == 0  ;  nW := System.ClientWidth
      ENDIF
      IF nH == 0  ;  nH := System.ClientHeight
      ENDIF

      IF LEN(aWinPar) < 7  ; lCenter := .T.
      ELSE                 ; lCenter := aWinPar[7]
      ENDIF
      IF LEN(aWinPar) < 8  ; aBClr   := Nil
      ELSE                 ; aBClr   := aWinPar[8]
      ENDIF
      IF LEN(aWinPar) < 9  ; aDown   := {}
      ELSE                 ; aDown   := aWinPar[9]
      ENDIF
      IF LEN(aWinPar) < 10 ; lTopmst := .F.
      ELSE                 ; lTopmst := aWinPar[10]
      ENDIF
      IF LEN(aWinPar) < 11 ; bOnInit := Nil
      ELSE                 ; bOnInit := aWinPar[11]
      ENDIF
      IF bOnInit # Nil
         IF !HB_ISBLOCK( bOnInit )
            cErr := "ERROR ! "
            cErr += "Must be a block of code !"
            cErr += " aWinPar[11]="
            MsgDebug(cErr, bOnInit)
         ENDIF
      ENDIF

      IF LEN(aWinPar) < 12 ; bOnRele := Nil
      ELSE                 ; bOnRele := aWinPar[12]
      ENDIF
      IF bOnRele # Nil
         IF !HB_ISBLOCK( bOnRele )
            cErr := "ERROR ! "
            cErr += "Must be a block of code !"
            cErr += " aWinPar[12]="
            MsgDebug(cErr, bOnRele)
         ENDIF
      ENDIF

      IF LEN(aWinPar) < 13 ; bIClose := Nil
      ELSE                 ; bIClose := aWinPar[13]
      ENDIF
      IF bIClose # Nil
         IF !HB_ISBLOCK( bIClose )
            cErr := "ERROR ! "
            cErr += "Must be a block of code !"
            cErr += " aWinPar[13]="
            MsgDebug(cErr, bIClose)
         ENDIF
      ENDIF

      IF LEN(aWinPar) < 14 ; cIcoWin := ""
      ELSE                 ; cIcoWin := aWinPar[14]
      ENDIF

   ENDIF
   lCenter := IIF( lCenter == NIL, .F. , lCenter )

RETURN NIL

///////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myLangRecover(cAlias,cSetCP,cSelCdp,cLngSel)
   LOCAL p1,p2

   p1 := SUBSTR(cLngSel,1,AT(".",cLngSel)-1)
   p2 := SUBSTR(cLngSel,AT(".",cLngSel)+1)

   hb_LangSelect(p1,p2)
   hb_SetCodepage(cSetCP)
   hb_CdpSelect(cSelCdp)
   IF LEN(cAlias) > 0
      IF ( Select( cAlias ) > 0 )
         dbSelectArea( cAlias )
      ENDIF
   ENDIF
   DO EVENTS

RETURN NIL

//////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myLangRecoverLost(aLang)   // ������ ������ � �����
   LOCAL p1,p2
   // aCurrLang := { hb_SetCodepage(), hb_CdpSelect(), Hb_LangSelect() }
   // ���������� ���� ���� ���� ���������� oWin:aCurrLang := aCurrLang
   IF HB_IsArray(aLang)
      IF LEN(aLang) >= 3
         p1 := SUBSTR(aLang[3],1,AT(".",aLang[3])-1)
         p2 := SUBSTR(aLang[3],AT(".",aLang[3])+1)
         hb_LangSelect(p1,p2)
         hb_SetCodepage(aLang[1])
         hb_CdpSelect(aLang[2])
         DO EVENTS
      ENDIF
   ENDIF

RETURN NIL

//////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION MG_YesNoQuit()
   LOCAL aColors, aOptions, nRet, lRet, nType, xIcon, nSize, bInit, lClosable
   LOCAL cTitle, aLang, cMsg

   _HMG_ModalDialogReturn := 1
   aLang     := myLangeRes(3)
   cMsg      := aLang[1]
   cTitle    := aLang[2]
   aColors   := { LGREEN, RED   , YELLOW }
   aOptions  := { '&' + aLang[3], '&' + aLang[4] , '&QuitExe' }
   nType     := NIL
   xIcon     := NIL
   nSize     := NIL
   bInit     := {|| This.TopMost := .T. }
   lClosable := .F.

   nRet := HMG_Alert( cMsg, aOptions, cTitle, nType, xIcon, nSize, aColors, bInit, lClosable )
   IF     nRet == 1
      lRet := .T.
   ELSEIF nRet == 2
      lRet := .F.
   ELSEIF nRet == 3
      //DbCloseAll()
      ReleaseAllWindows()
   ENDIF

RETURN lRet

////////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Tbrowse_OnInit(cAls, c1Title, c2Title, oTsb)  // ��������� ������ ��� �������
   LOCAL cTitle, cFont1, cFont2, cFont3, cFont5, cFont6, nFS5, nClrFace, nGrad
   LOCAL nClrFocus1, nClrFocus2, nClrNoFocus1, nClrNoFocus2, nClrSeleF, nClrNoEdit
   LOCAL aResCheck, aResBitMaps, cFile, aFile, cMsg, cErr, nI, aClrDef
   DEFAULT c1Title := hb_SetCodepage()

   cMsg        := ""
   aFile       := {}
   aResCheck   := { BmpTemp_CheckT24(), BmpTemp_CheckF24() }
   aResBitMaps := { PngTemp_Arrow_Down24(), PngTemp_Arrow_Down24(), BmpTemp_RecDel() }
   aMerge( aFile, aResCheck   )
   aMerge( aFile, aResBitMaps )

   FOR nI := 1 TO LEN(aFile)
      cFile := aFile[nI]
      IF !FILE(cFile)
         cMsg += cFile + ";"
      ENDIF
   NEXT

   IF LEN(cMsg) > 0
      cErr := "Error ! Could not create icon resources for the table !;;"
      cErr += cMsg + ";;" + ProcNL() + ";" + ProcNL(1)
      AlertStop( cErr, "Result", "ZZZ_B_STOP64", 64 )
   ENDIF

   cFont1 := "Tsb_Normal_" + cAls
   IF Empty( GetFontHandle( cFont1 ) )
      DEFINE FONT &cFont1 FONTNAME _HMG_DefaultFontName SIZE _HMG_DefaultFontSize  //CHARSET 0
   ENDIF
   cFont2 := "Tsb_Bold_" + cAls
   IF Empty( GetFontHandle( cFont2 ) )
      DEFINE FONT &cFont2 FONTNAME _HMG_DefaultFontName SIZE _HMG_DefaultFontSize BOLD  //CHARSET 0
   ENDIF
   cFont3 := "Tsb_Italic_" + cAls
   IF Empty( GetFontHandle( cFont3 ) )
      DEFINE FONT &cFont3 FONTNAME _HMG_DefaultFontName SIZE _HMG_DefaultFontSize-4 BOLD ITALIC  //CHARSET 0
   ENDIF
   cFont5 := "Tsb_SuperHd_" + cAls
   IF Empty( GetFontHandle( cFont5 ) )
      nFS5 := INT( _HMG_DefaultFontSize * 1.2 )
      DEFINE FONT &cFont5 FONTNAME _HMG_DefaultFontName SIZE nFS5 BOLD   //CHARSET 0
   ENDIF
   cFont6 := "Tsb_Edit_" + cAls
   IF Empty( GetFontHandle( cFont6 ) )
      DEFINE FONT &cFont6 FONTNAME "Tahoma" SIZE _HMG_DefaultFontSize BOLD   //CHARSET 0
   ENDIF

   IF hb_IsObject(oTsb)
      IF oTsb:cSupHd1Title # NIL
         c1Title := oTsb:cSupHd1Title
      ENDIF
      IF oTsb:cSupHd2Title # NIL
         c2Title := oTsb:cSupHd2Title
      ENDIF
   ELSE
      oTsb := oHmgData()  // �������� ���������� �������� �-� _TBrowse()
   ENDIF
   // ����� �������    1-Cells, 2-Headers, 3-Footers, 4-SpecHeader, 5-SuperHeader, 6-Edit
   oTsb:aFont     := { cFont1 , cFont2   , cFont3   , cFont3      , cFont5       , cFont6  }

   // ����� �������
   IF oTsb:nClr2Back == NIL
      oTsb:nClr2Back := RGB(202,233,244)
      oTsb:nClr22Bck := RGB(186,216,232)
   ENDIF
   nGrad := RGB(48,29,26)
   IF oTsb:nClr16Back == NIL
      oTsb:nClr16Back := { RGB(96,255,255), nGrad }         // 16, ���� �����������
   ENDIF
   IF oTsb:nClr4Back == NIL
      oTsb:nClr4Back  := { RGB(40,122,237), nGrad }         // 4 , ���� ����� �������
      oTsb:nClr10Back := { RGB(40,122,237), nGrad }         // 10, ���� ������� �������
   ENDIF
   IF oTsb:nClr3Fore == NIL
      oTsb:nClr3Fore  := CLR_YELLOW                         // 3 , ������ ����� �������
   ENDIF
   IF oTsb:nClr9Fore == NIL
      oTsb:nClr9Fore  := CLR_YELLOW                         // 9 , ������ ������� �������
   ENDIF
   IF oTsb:nClr17Fore == NIL
      oTsb:nClr17Fore := CLR_WHITE                          // 17, ������ �����������
   ENDIF

   nClrFace         := GetSysColor( COLOR_BTNFACE )
   nClrNoEdit       := RGB(242,163,167)                                                            // �����/������ ������� ���� "+=^"
   oTsb:aBrush      := IIF( hb_IsArray(oTsb:aBrush)      , oTsb:aBrush      , RGB(240,240,240) )   // ��� ��������
   oTsb:nClrNoDbf   := IIF( hb_IsNumeric(oTsb:nClrNoDbf ) , oTsb:nClrNoDbf  , nClrFace         )   // ��������/���������
   oTsb:nForeNoDbf  := IIF( hb_IsNumeric(oTsb:nForeNoDbf) , oTsb:nForeNoDbf , CLR_RED          )   // ��������/���������
   oTsb:nClrNoEdit  := IIF( hb_IsNumeric(oTsb:nClrNoEdit) , oTsb:nClrNoEdit , nClrNoEdit       )
   oTsb:nClr22Bck   := IIF( hb_IsNumeric(oTsb:nClr22Bck ) , oTsb:nClr22Bck  , CLR_GRAY         )   // �������� ������
   oTsb:nClrBackDel := IIF( hb_IsNumeric(oTsb:nClrBackDel), oTsb:nClrBackDel, RGB(50,50,50)    )   // ���� �������� �������
   oTsb:nClrForeDel := IIF( hb_IsNumeric(oTsb:nClrForeDel), oTsb:nClrForeDel, CLR_WHITE        )   // ����� �������� �������
   oTsb:nClr1Fore   := IIF( hb_IsNumeric(oTsb:nClr1Fore  ), oTsb:nClr1Fore  , CLR_BLACK        )   // 1 , ����� � ������� �������
   oTsb:nClr2Back   := IIF( hb_IsNumeric(oTsb:nClr2Back  ), oTsb:nClr2Back  , CLR_WHITE        )   // 2 , ���   � ������� �������
   oTsb:nClr3Fore   := IIF( hb_IsNumeric(oTsb:nClr3Fore  ), oTsb:nClr3Fore  , CLR_BLACK        )   // 3 , ������ ����� �������
   IF !hb_IsArray(oTsb:nClr4Back)
   oTsb:nClr4Back   := IIF( hb_IsNumeric(oTsb:nClr4Back)  , oTsb:nClr4Back  , CLR_BLACK        )   // 4 , ���� ����� �������
   ENDIF
   oTsb:nClr9Fore   := IIF( hb_IsNumeric(oTsb:nClr9Fore)  , oTsb:nClr9Fore  , CLR_RED          )   // 9 , ������ ������� �������
   IF !hb_IsArray(oTsb:nClr10Back)
   oTsb:nClr10Back  := IIF( hb_IsNumeric(oTsb:nClr10Back) , oTsb:nClr10Back , CLR_BLACK        )   // 10, ���� ������� �������
   ENDIF
   IF !hb_IsArray(oTsb:n1Clr16Back)
   oTsb:n1Clr16Back := IIF( hb_IsNumeric(oTsb:n1Clr16Back), oTsb:n1Clr16Back, RGB(84,141,212)  )   // 16, ���� ����������� ������� 1
   ENDIF
   oTsb:n1Clr17Fore := IIF( hb_IsNumeric(oTsb:n1Clr17Fore), oTsb:n1Clr17Fore, CLR_YELLOW       )   // 17, ������ ����������� ������� 1
   // ����� �������
   nClrFocus1   := IIF( hb_IsNumeric(oTsb:nClrFocus1  ), oTsb:nClrFocus1  , -CLR_HRED                       ) // ������ ��������� ������ � ������
   nClrFocus2   := IIF( hb_IsNumeric(oTsb:nClrFocus2  ), oTsb:nClrFocus2  , -RGB(1,1,1)                     ) // ������� ��������� ������ ������� � ������
   nClrSeleF    := IIF( hb_IsNumeric(oTsb:nClrSeleF   ), oTsb:nClrSeleF   , GetSysColor( COLOR_WINDOWTEXT ) ) // ���� ������ ������ ������� ��� ������
   nClrNoFocus1 := IIF( hb_IsNumeric(oTsb:nClrNoFocus1), oTsb:nClrNoFocus1, -CLR_BLUE                       ) // ��������� ��� ������
   nClrNoFocus2 := IIF( hb_IsNumeric(oTsb:nClrNoFocus2), oTsb:nClrNoFocus2, -RGB( 128, 225, 225 )           ) // ��������� ��� ������
   //----- 07.11.23
   aClrDef      := {nClrFace,nClrFace}
   IF !hb_IsArray(oTsb:n12Clr10Back)
   oTsb:n12Clr10Back := IIF( hb_IsNumeric(oTsb:n12Clr10Back), oTsb:n12Clr10Back, aClrDef  )     // 10, ���� ������� ������� ������� 1-2
   ENDIF
   IF !hb_IsArray(oTsb:n12Clr4Back)
   oTsb:n12Clr4Back := IIF( hb_IsNumeric(oTsb:n12Clr4Back), oTsb:n12Clr4Back, aClrDef  )        // 4 , ���� ����� ������� ������� 1-2
   ENDIF
   oTsb:n12Clr9Fore  := IIF( hb_IsNumeric(oTsb:n12Clr9Fore) , oTsb:n12Clr9Fore , CLR_HMAGENTA )   // 9 , ������ ������� ������� ������� 1-2
   oTsb:n12Clr3Fore  := IIF( hb_IsNumeric(oTsb:n12Clr9Fore) , oTsb:n12Clr3Fore , CLR_HMAGENTA )   // 3 , ������ ����� ������� ������� 1-2
   IF !hb_IsArray(oTsb:nClrSelectorHdBack)
      oTsb:nClrSelectorHdBack := aClrDef       // ���� ���� �����/������� ������� ������� 1 Selector
   ENDIF

   oTsb:aColor := { ;
          { CLR_FOCUSB, {|c,n,b| c := n, iif( b:nCell == n, nClrFocus1, nClrFocus2 ) } }, ;
          { CLR_SELEF , nClrSeleF }, ;
          { CLR_SELEB , {|c,n,b| c := n, iif( b:nCell == n, nClrNoFocus1, nClrNoFocus2 ) } }  ;
         }

   IF !hb_IsLogical(oTsb:lShowZebra)        // ����� ������\�������� ������
      oTsb:lShowZebra := .F.
   ENDIF

   // ��������� �������
   IF hb_IsLogical(oTsb:lSelector)
      oTsb:uSelector := oTsb:lSelector
      IF !oTsb:lSelector
         oTsb:uSelector := Nil
      ENDIF
   ELSE
      oTsb:uSelector := .T.   // ������ ��������� � ������� ����.������� SELECTOR
   ENDIF

   IF hb_IsLogical(oTsb:lColNumber)
      IF oTsb:lColNumber
         oTsb:aNumber := IIF( hb_IsArray(oTsb:aColNumber), oTsb:aColNumber, { 1, 40 } )
      ELSE
         oTsb:aNumber := Nil
      ENDIF
   ELSE
      oTsb:aNumber := { 1, 40 }  // ������ ��������� � ������� ����.������� ORDKEYNO
   ENDIF

   IF hb_IsLogical(oTsb:lSuperHead)
      oTsb:lSuperHead := oTsb:lSuperHead
   ELSE
      oTsb:lSuperHead := .T.   // ������ ��������� � ������� ���������� - ��� ����
   ENDIF

   IF hb_IsLogical(oTsb:lSpecHd)
      oTsb:lSpecHd := oTsb:lSpecHd
   ELSE
      oTsb:lSpecHd := .T.   // ������ ��������� � ������� ���������
   ENDIF

   IF hb_IsLogical(oTsb:lFooting)
      oTsb:lFooting := oTsb:lFooting
   ELSE
      oTsb:lFooting := .T.   // ������ ��������� � ������� ������
   ENDIF

   IF hb_IsLogical(oTsb:lDrawDelAll)
      oTsb:lDrawDelAll := oTsb:lDrawDelAll    // ������ ��: �����/������/����������/��������� - ��� ����
   ENDIF

   IF hb_IsNumeric(oTsb:nHeightHead)
      oTsb:nHeightHead := oTsb:nHeightHead    // ������ �����
   ENDIF

   IF hb_IsLogical(oTsb:lAdjust)
      oTsb:lAdjust := oTsb:lAdjust       // ������ ���������� �������� � ������� ��� �������� ������ � �����
   ENDIF

   // oTsb:aEdit - ������ ������ ����� �� ��������, �������� ������� _TBrowse()
   // ��������: Nil �  .F.-������ ���������, .T.-������ ���������
   // ���� oTsb:aEdit := oTsb:aEdit �� �������� ���������� ������
   oTsb:aEdit := IIF( oTsb:aEdit==Nil, .T., oTsb:aEdit )
   oTsb:aFoot := IIF( oTsb:aFoot==Nil, .T., oTsb:aFoot )

   // -------------------- �������� ------------------
   // � ������� ����������/�����/���������/������ ������� ������, ����� ������������� ����������
   //oTsb:lDrawDelAll := .T.   // ������ ��: �����/������/����������/��������� - ��� ����
   //oTsb:uSelector   := NIL   // ������ � ������� ����.������� SELECTOR
   //oTsb:aNumber     := Nil   // ������ � ������� ����.������� ORDKEYNO
   //oTsb:lSuperHead  := .F.   // ������ � ������� ���������� - ��� ����
   //oTsb:lHeading    := .F.   // ������ � ������� ����� - ��� ���� - �� ��������
   //oTsb:lSpecHd     := .F.   // ������ ��������� � �������
   //oTsb:lFooting    := .T.   // ������ � ������� ������
   //oTsb:lAdjust     := .F.   // ������ ���������� �������� � ������� ��� �������� ������ � �����
   //oTsb:aEdit       := .F.   // ������ ������ ���� �����

   IF hb_IsArray(oTsb:aWidthCol)
      oTsb:aWidthCol := oTsb:aWidthCol   // ������ � ���������
   ENDIF

   IF hb_IsArray(oTsb:aRelat)
      oTsb:aRelat  := oTsb:aRelat  // ���� ����������� �� Set Relation
   ENDIF

   IF hb_IsArray(oTsb:aEditFunc)
      oTsb:aEditFunc := oTsb:aEditFunc  // ��������� ������� ������� - ������� �������
   ENDIF

   // ���� �������������
   oTsb:bInit     := {|ob,op|
         Local oc
         ob:GetColumn("ORDKEYNO"):hFont := GetFontHandle(op:aFont[4])    // "Italic"
         // ��� ��� ������ ��������� �������
         //AEval(ob:aColumns, {|oc| oc:nAlign  := iif( oc:cFieldTyp $ "DLT^=@", DT_CENTER, oc:nAlign ) }) // ��� ��� ������
         //AEval(ob:aColumns, {|oc| oc:nFAlign := oc:nAlign }) // ��� ��� ������, ��������� Align �� Footer
         //AEval(ob:aColumns, {|oc| oc:nWidth  += iif( oc:cFieldTyp $ "T=@", 15, 0 ) }) // ��������� width � TimeStamp
         //AEval(ob:aColumns, {|oc| iif( oc:cFieldTyp $ "T=@", oc:cPicture := "@R 9999-99-99 99:99:99", ) })
         FOR EACH oc IN ob:aColumns
             IF oc:cFieldTyp $ "T=@"
                oc:cPicture := "@R 9999-99-99 99:99:99"
                oc:bDecode  := {|tval| hb_TtoS(tval) }
                oc:nAlign   := DT_CENTER
                oc:nWidth   += 10            // �������� 10 ��������
             ELSEIF oc:cFieldTyp $ "L^"
                oc:nAlign   := DT_CENTER
             ELSEIF oc:cFieldTyp $ "D"
                oc:cPicture := NIL
                oc:nAlign   := DT_CENTER
             ENDIF
             //oc:nFAlign := oc:nAlign  // �� �������
         NEXT

         op:lSpecHd := IIF( op:lSpecHd == Nil, .T., op:lSpecHd ) // ���� ������ ����� ���
         IF op:lSpecHd
            ob:nHeightSpecHd := 16                           // ������ ���������� ENUMERATOR
            ob:lDrawSpecHd   := .T.
         ELSE
            ob:nHeightSpecHd := 0                            // ������ ���������� ENUMERATOR
            ob:lDrawSpecHd   := .F.
         ENDIF
         ob:nHeightCell   += 4                               // ������� �������� � ������ �����
         ob:nHeightHead   := GetFontHeight(op:aFont[2])      // ������ �����
         ob:nHeightFoot   := GetFontHeight(op:aFont[4]) + 6  // ������ �������

         IF hb_IsNumeric(op:nHeightHead)        // �� ����� ��������, ��� ��������� oTsb:bEnd
            ob:nHeightHead := op:nHeightHead    // ������ ����� �� ����������
         ENDIF

         op:lDrawDelAll := IIF( op:lDrawDelAll == Nil, .F., op:lDrawDelAll ) // ���� ������ ����� ���
         IF op:lDrawDelAll          // ��� ����
            ob:lDrawHeaders  := .F.
            ob:lDrawSpecHd   := .F.
            ob:lFooting      := .F.
            op:lSuperHead    := .F. // ��� ����
            ob:nHeightSuper  := 0   // ������ �����������
            ob:nHeightSpecHd := 0   // ������ ����������
            ob:nHeightHead   := 0   // ������ �����
            ob:nHeightFoot   := 0   // ������ �������
         ENDIF

         op:lFooting := IIF( op:lFooting == Nil, .T., op:lFooting ) // ���� ������ ����� ���
         IF !op:lFooting
            ob:lFooting := .F.
         ENDIF

         op:lSuperHead := IIF( op:lSuperHead == Nil, .T., op:lSuperHead ) // ���� ������ ����� ���
         IF !op:lSuperHead
            ob:nHeightSuper  := 0   // ������ �����������
         ELSE
            ob:nHeightSuper  := 10  // ������ �����������
         ENDIF

         op:lHeading := IIF( op:lHeading == Nil, .T., op:lHeading ) // ���� ������ ����� ���
         // - �� �������� !!!
         //IF !op:lHeading
         //   ob:nHeightHead  := 0   // ������ �����
         //   ob:lDrawHeaders := .F.
         //ENDIF

         Return Nil
         }

   // ������ ���������� � �������
   oTsb:cSupHd1    := c1Title             // ��������� 1 �����������
   cTitle          := c2Title + "  Alias: " + ALIAS() + " , "
   cTitle          += cFileNoPath(DBINFO(DBI_FULLPATH)) + " , " + RddName()
   oTsb:cSupHd2    := cTitle             // ��������� 2 �����������
   //oTsb:cSupHdImg1 := "rezerv"           // �������� ��� �����������
   //oTsb:cSupHdImg2 := 'MG_TSB_DW'        // �������� ��� ����������� "ArrDown24"
   oTsb:cSupHdImg1 := aResBitMaps[1]       // �������� ��� �����������
   oTsb:cSupHdImg2 := aResBitMaps[2]       // �������� ��� ����������� "ArrDown24"
   oTsb:nMaskBmp   := 0x008800C6
   // �������� ����
   oTsb:bBody := {|ob,op|
         Local nFrom, nTo, nI, hBmp1, hBmp2, nOrdKeyNo, oCol

         // --------- �������� ������� CHECKBOX �� ���� �������� ---------
         //ob:aCheck   := { LoadImage("MG_TSB_ChkT24"), LoadImage("MG_TSB_ChkF24") }
         ob:aCheck   := { LoadImage(aResCheck[1]), LoadImage(aResCheck[2]) }
         // --------- ��������� ��������, ��������� ����� �������� ������� ��������� ------
         ob:aBitMaps := { LoadImage(op:cSupHdImg1),  LoadImage(op:cSupHdImg2) ,;
                          LoadImage(aResBitMaps[3])                        }
         //ob:aBitMaps := { LoadImage(op:cSupHdImg1),  LoadImage(op:cSupHdImg2) ,;
         //                 LoadImage("MG_TSB_RECDEL")                    }
         /* ������        LoadImage("Empty16" ), LoadImage("No16") ,;
                          LoadImage("Arrow_down")    ,; // �������� �������_����  30x30
                          LoadImage("Arrow_up")      ,; // �������� �������_����� 30x30
                          LoadImage("ArrowDown20")   ,; // �������� �������_����  20x20
                          LoadImage("ArrowUp20")     ,; // �������� �������_����� 20x20
                          LoadImage("bFltrAdd20")    ,; // �������� ������ 20x20
                          LoadImage("bSupHd40")      ,; // �������� 40x140
                          LoadImage("ArrDown40Blue") ,; // �������� �������_���� 40x40 - PNG
                        } */
                        // �������� PNG � ������������� �� ���� ������ ���
                        // :nBmpMaskXXXX := 0x00CC0020    // SRCCOPY
         nOrdKeyNo   := ob:nColumn( "ORDKEYNO", .T. )
         hBmp1       := ob:aBitMaps[1]
         hBmp2       := ob:aBitMaps[2]
         ob:Cargo:hArrDown := ob:aBitMaps[2]

         oCol := ob:GetColumn("ORDKEYNO")
         oCol:uBmpCell := {|nc,ob| nc := (ob:cAlias)->(Deleted()) ,;
                                   iif( nc, ob:aBitMaps[3], Nil )    }

         IF ob:lDrawHeaders .AND. ob:nHeightSuper > 0

            nFrom := nTo := nI := 1

            IF nOrdKeyNo > 0
               nTo := nOrdKeyNo
            ENDIF

            IF nOrdKeyNo > 0
               ADD SUPER HEADER TO ob FROM nFrom TO nTo Color op:n1Clr17Fore, op:n1Clr16Back ;
                   TITLE " " + op:cSupHd1 /*BITMAP hBmp1*/ HORZ DT_CENTER
               ob:aSuperHead[ nI ][15] := op:nMaskBmp
               ob:aSuperHead[ 1, 7 ]   := oTsb:aFont[3]    // ������� ���� ������� 1
               //ob:aSuperHead[ 1, 4 ]   := CLR_YELLOW       // ������� ���� ������ ������� 1
               //ob:aSuperHead[ 1, 5 ]   := RGB(84,141,212)  // ������� ���� ���� ������� 1
               nFrom := nTo + 1
               nI    += 1
            ENDIF

            nTo := ob:nColCount()
            ADD SUPER HEADER TO ob FROM nFrom TO nTo Color op:nClr17Fore, op:nClr16Back ;
                   TITLE " " + op:cSupHd2 BITMAP hBmp2 HORZ DT_CENTER

            ob:aSuperHead[ nI ][15] := op:nMaskBmp

            ob:nHeightSuper := 28   // ������ �����������

         ENDIF  // ob:lDrawHeaders

         Return Nil
         }

   // �������� ����������� ���� _TBrowse(...) �� ����
   // oTsb:bEnd := {|| NIL }

   // ����� �� _TBrowse() � ���������� �� ����
   oTsb:bEnd := {|ob,op|
                      // ��� ��������������� HScroll � ���� SELECTOR
                      IF op:uSelector != NIL .and. op:lAdjust == NIL .and. ob:lNoHScroll
                         IF HB_ISBLOCK( op:bAdjColumns )
                            EVal( op:bAdjColumns, ob, op )  // :AdjColumns(...)
                         ENDIF
                      ENDIF
                      IF ob:nLen > ob:nRowCount()           // ����� VScroll
                         ob:ResetVScroll( .T. )
                      ENDIF

                      IF hb_IsNumeric(op:nHeightHead)    // ���� ������ ������ ����� �������
                         // �� ������ �������� �� ����� ����� �������
                      ELSE
                         ob:SetNoHoles()
                      ENDIF
                      //----- 07.11.23
                      // ����������� ����� � ����� ��� ����������� �������
                      //? ProcNL(), "==========", hb_IsArray(op:nClr4Back)
                      IF hb_IsArray(op:nClr4Back)
                         ob:oPhant:nClrHeadBack := op:nClr4Back
                         //?? HB_ValToExp(op:nClr4Back)
                      ENDIF
                      // ����������� ����� � ������� ��� ����������� �������
                      //?? hb_IsArray(op:nClr10Back)
                      IF hb_IsArray(op:nClr10Back)
                         ob:oPhant:nClrFootBack := op:nClr10Back
                      ENDIF
                      ob:Refresh()
                      //------- 07.11.23
                      ob:SetFocus()
                      Return Nil
                    }

RETURN oTsb

//////////////////////////////////////////////////////////////////////
STATIC FUNCTION Tbrowse_Customization( oBrw, oTsb )   // ����������� �������
   LOCAL oCol, nI, cCol, cTyp, nClrNoDbf, nClrNoEdit, nForeNoDbf
   LOCAL cErr, hFont, cMsg, nJ, aRelat, aVal, aWidthCol := {}
   LOCAL cFld, cName, cAls, nCol, cHead, nSize, nAlgn, cFunc
   LOCAL nCharSize, nWidth, nZnak, nWCol, lEdit, aEditFunc
   LOCAL cVal, nVal, aFont

   // ���� ����������� �� Set Relation
   hFont := oBrw:hFont  // 1-cells font
   IF hb_IsArray(oTsb:aRelat)
      aRelat := oTsb:aRelat
      FOR nI := 1 TO LEN(aRelat)
         aVal  := aRelat[nI]
         cFld  := aVal[1]        // ��������� �������/����
         cName := UPPER(aVal[2]) // ����� ��� �������/����
         cAls  := aVal[3]        // ����� �������. �����������
         nSize := aVal[4]        // ���-�� �������� ��� ������
         nAlgn := aVal[5]        // ���������
         cFunc := aVal[6]        // ������ - ��� ������� ��� ��������� �������
         nCol  := myGetNumbaColumn(oBrw,cFld)
         cHead := oBrw:GetColumn(cFld):cHeading
         IF nCol == 0
            cMsg := 'ERROR on Set Relation !;'
            cMsg += 'There is no such field "'+cFld+'" in the database !;'
            cMsg += 'Array search: ' + HB_ValToExp(aVal) + ';;' + ProcNL()
            AlertStop(cMsg)
         ELSE
            // ������ � �������������� �������
            IF oTsb:aEdit == Nil            ; lEdit := .F.
            ELSEIF hb_IsArray(oTsb:aEdit)   ; lEdit := oTsb:aEdit[nCol]
            ELSEIF hb_IsLogical(oTsb:aEdit) ; lEdit := oTsb:aEdit
            ELSE                            ; lEdit := .F.
            ENDIF

            ADD COLUMN TO oBrw DATA FieldWBlock(cName, select(cAls))  ;
               HEADER cHead /*FOOTER "+"*/ FIXED NAME &(cName)

            oBrw:DelColumn(cFld)         // ������� �������
            oBrw:MoveColumn( oBrw:nColumn(cName), nCol )
            aFont          := oTsb:aFont // �������� �����
            cErr           := 'Array search: ' + HB_ValToExp(aVal) + ';;' + ProcNL()
            oCol           := oBrw:GetColumn(cName)
            oCol:cHeading  := '"'+cHead+'"'
            oCol:nWidth    := GetTextWidth( Nil, REPL("a", nSize), hFont )
            oCol:nAlign    := nAlgn
            oCol:Cargo     := { cFunc, cFld, cErr, aFont, cAls } // ��� �������� � �������
            oCol:lEdit     := lEdit
            /* ������ ��� ����� ������� ����� �������
            oCol:bPrevEdit := {|uv,ob|
                 uv := (ob:cAlias)->( RecNo() )
                 myTsbExtern(ob)                   // oCol:Cargo - ������� ������ �������
                 (ob:cAlias)->( dbGoto(uv) )
                 ob:Setfocus()
                 ob:DrawSelect()
                 DoEvents()
                 Return .F.
                 } */
         ENDIF
      NEXT
   ENDIF

   nClrNoEdit := oTsb:nClrNoEdit
   nClrNoDbf  := oTsb:nClrNoDbf       // ��� ��������/���������/����.�������
   nForeNoDbf := oTsb:nForeNoDbf      // ����� ��������/���������/����.�������

   // ��������� ����� ����������� �������
   oCol          := oBrw:GetColumn("ORDKEYNO")
   oCol:nClrBack := nClrNoDbf
   // ��������� ������ ����������� ������� - ������� 1
   oBrw:GoBottom()
   IF oTsb:lSelector == NIL
      nCol := 1
   ELSE
      nCol := IIF(oTsb:lSelector,2,1)
   ENDIF
   nVal := oBrw:GetValue(nCol)
   cVal := nVal + "00"
   oBrw:GoTop()
   // ��������� ������ ����������� ������� - ������� 2
   cVal := iif( hb_IsBlock( oCol:cFooting ), Eval( oCol:cFooting, nCol, oBrw ), oCol:cFooting )
   cVal += "00"
   //hFont := oBrw:hFont         // 1-cells font
   hFont := oBrw:hFontFoot       // 3-footer font
   nWCol := GetTextWidth( Nil, REPL("0",LEN(cVal)), hFont )  // ���-�� ������ + 2 �����
   oCol:nWidth := nWCol          // ����� ������

   IF hb_IsArray(oTsb:aWidthCol)      // ������ � ���������
      aWidthCol := oTsb:aWidthCol
   ENDIF

   // ��������� ����� �����/�������/����������/���������� ������� � ������ ��������
   FOR EACH oCol IN oBrw:aColumns
      IF oBrw:lDrawSpecHd
         oCol:nClrSpcHdBack := nClrNoDbf          // ::aColorsBack[ 18 ]
         oCol:nClrSpcHdFore := nForeNoDbf         // ::aColorsBack[ 19 ]
      ENDIF
      cCol := oCol:cName
      // ������ ������� - ��������
      IF cCol == "SELECTOR" .OR. cCol == "ORDKEYNO"
         //IF oCol:cName == "ORDKEYNO"
         //   oCol:cSpcHeading := "#"        // ��. ����
         //ENDIF
      ELSE
         oCol:nClrHeadBack := oTsb:nClr4Back     // 4 , ���� ����� �������
         oCol:nClrHeadFore := oTsb:nClr3Fore     // 3 , ������ ����� �������
         oCol:nClrFootBack := oTsb:nClr10Back    // 10, ���� ������� �������
         oCol:nClrFootFore := oTsb:nClr9Fore     // 9 , ������ ������� �������
         // ������ ����+���������
         oCol:hFontFoot    := GetFontHandle(oTsb:aFont[4])
         oCol:nFAlign      := DT_CENTER
         //oCol:cFooting   := cCol
      ENDIF
      cTyp := oCol:cFieldTyp
      IF cTyp $ "+=^"   // Type: [+] [=] [^]
         oCol:nClrHeadBack := nClrNoEdit
         oCol:nClrFootBack := nClrNoEdit
      ENDIF
      // ������ � ���������
      nWCol := oCol:nWidth
      IF LEN(aWidthCol) > 0
         FOR nJ := 1 TO LEN(aWidthCol)
            IF cCol == aWidthCol[nJ,1]
               nCharSize := aWidthCol[nJ,2]
               nZnak := 1
               IF nCharSize < 0
                  nCharSize := nCharSize * -1
                  nZnak := -1
               ENDIF
               nWidth := GetTextWidth( Nil, REPL("a", nCharSize), hFont )
               IF nZnak < 0
                  oCol:nWidth := nWCol - nWidth
               ELSE
                  oCol:nWidth := nWCol + nWidth
               ENDIF
               EXIT
            ENDIF
         NEXT
      ENDIF
   NEXT

   // ���������� ����� ������������� SpecHd �� ������ ����������� ������� ORDKEYNO
   EVal( oTsb:bSpecHdEnum, oBrw, oTsb, "#" )

   // ��� �������� �������  � �.�
   oBrw:SetColor( {1}, { { |nr,nc,ob| myCellColorFore(nr,nc,ob) } } ) // 1 , ������ � ������� �������
   oBrw:SetColor( {2}, { { |nr,nc,ob| myCellColorBack(nr,nc,ob) } } ) // 2 , ���� � ������� �������

   // ���� ����� ��������
   oBrw:nClrLine := CLR_BLACK //COLOR_GRID

   // ������ ������ ������� ���� "+=^"
   AEval(oBrw:aColumns, {|oc| oc:lEdit := iif( oc:cFieldTyp $ "+=^", .F., oc:lEdit )})

   // �� ������ ������� ����� ���� "^"
   FOR EACH oCol IN oBrw:aColumns
      IF oCol:cFieldTyp == '^' .OR. oCol:cFieldTyp == '+'
         oCol:bDecode := {|nVal| hb_ntos( nVal ) }
      ENDIF
   NEXT

   // ����� ������� + ������ ���� - special�selector header background color
   oBrw:nClrSelectorHdBack := nClrNoDbf

   //  ��������� �� ���������� ��������� ������� ��� ������ ����� END TBROWSE
   FOR EACH oCol IN oBrw:aColumns
      // ����� /*� ������*/ ������ ����� ��� ����� �������
      oCol:bHLClicked := {|Ypix,Xpix,nAt,ob| iif( Ypix > ob:nHeightSuper, ;
                           Tbrowse_Header("Header:",Ypix,Xpix,nAt,ob) ,;
                           Tbrowse_SuperHd("Super:",Ypix,Xpix,nAt,ob) ) }
      cCol := oCol:cName
      IF cCol == "ORDKEYNO" .OR. cCol == "SELECTOR"
      ELSE
         // �������� � ����� ������� ������� - �������_����  20x20
         oCol:uBmpHead := {|nc,ob| nc := ob:Cargo, nc:hArrDown }
         oCol:nHAlign  := nMakeLong( DT_CENTER, DT_RIGHT  )
         // �������� � ������� ������� ������� - �������_����� 20x20
         //oCol:uBmpFoot  := {|nc,ob| nc := ob:Cargo, nc:hArrUp20  }
         //oCol:nFAlign   := nMakeLong( DT_CENTER, DT_RIGHT  )
         // �������� � ���������� ������� ������� - �������_����  20x20
         //oCol:uBmpSpcHd := {|nc,ob| nc := ob:Cargo, nc:hArrDown20   }
         //oCol:nSAlign   := nMakeLong( DT_CENTER, DT_RIGHT  )
      ENDIF
      // ����� ������ ��������
         //oCol:nBmpMaskHead := 0x00CC0020    // SRCCOPY - ������
         //oCol:nBmpMaskFoot := 0x00CC0020    // SRCCOPY - ������
         oCol:nBmpMaskHead   := 0x00BB0226    // MERGEPAINT
         oCol:nBmpMaskFoot   := 0x00BB0226    // MERGEPAINT
         oCol:nBmpMaskSpcHd  := 0x00CC0020    // SRCCOPY
         //oCol:nBmpMaskCell := 0x00CC0020    // SRCCOPY - ������ ������� ����������
         //oCol:nBmpMaskCell := 0x00BB0226    // MERGEPAINT - ������ �������
   NEXT

   // ��������� ������� ������� - ������� �������
   IF hb_IsArray(oTsb:aEditFunc)
      ? ProcNL(), "��������� ������� ������� - ������� �������="
      oBrw:Cargo:oEditFunc := oHmgData()
      aEditFunc := oTsb:aEditFunc
      FOR nI := 1 TO LEN(aEditFunc)
         oBrw:Cargo:oEditFunc:Set( aEditFunc[ nI ][1], aEditFunc[ nI ][2] )
         ? STR(nI,6), aEditFunc[ nI ][1], aEditFunc[ nI ][2], aEditFunc[ nI ][3]
      NEXT

      oBrw:bLDblClick := {|p1,p2,nf,ob| p1:=p2:=nf, ob:PostMsg(WM_KEYDOWN, VK_RETURN, 0) }
      oBrw:UserKeys( VK_RETURN, {|ob|
                     Local nMsg
                     Local oFun := ob:Cargo:oEditFunc
                     Local nCol := ob:nCell
                     Local oCol := ob:aColumns[ nCol ]
                     Local cCol := oCol:cName
                     nMsg := oFun:Get(cCol, 100) // ��� ��. field 100
                     MsgDebug(nMsg,oFun)
                     _wPost( nMsg, ob:cParentWnd, ob)
                     Return Nil
                } )
   ENDIF

   // ��������� ������ � ���������
   oBrw:bLDblClick := {|p1,p2,nf,ob| p1:=p2:=nf, ob:PostMsg(WM_KEYDOWN, VK_RETURN, 0) }
   oBrw:bRClicked  := {|p1,p2,p3,ob| p1:=p2:=p3, _wPost(10, ob:cParentWnd, ob)        }
   oBrw:UserKeys(VK_F2, {|ob| TsbTracing(ob)   })  // ���� �� ������ �������

   // ������ �������, ���� ��� ������� �� ������� - ������������� ����� �������
   oBrw:bEvents := {|ob,nmsg|
                     Local nI, oTsb, aFltr, aNFltr, nDel, cFltr
                     If nmsg == WM_LBUTTONUP .and. ob:nLen == 0
                        oTsb := ob:Cargo:oParam
                        nI  := myFilterClear(ob)
                        IF nI == 0
                        ELSEIF nI == 1
                           aFltr  := oTsb:aColFilter
                           aNFltr := oTsb:aColNumFltr
                           nDel   := LEN(aFltr)
                           // ������� ������� ������� ��� �������
                           ADel( aFltr , nDel, .T. )
                           ADel( aNFltr, nDel, .T. )
                           cFltr := ""
                           For nI := 1 TO LEN(aFltr)
                              cFltr += aFltr[nI] + IIF(nI==LEN(aFltr),""," .AND. ")
                           Next
                           ob:FilterData(cFltr)
                           oTsb:aColFilter  := aFltr     // ������� ������� � ��������
                           oTsb:aColNumFltr := aNFltr    // ������ ������� ������� � ��������
                        ELSEIF nI == 2
                           oTsb:aColFilter  := {}    // ������� ������� � ��������
                           oTsb:aColNumFltr := {}    // ������ ������� ������� � ��������
                           ob:FilterData()
                        ENDIF
                        ob:SetFocus()
                     EndIf
                     Return Nil
                    }

   IF !hb_IsArray(oTsb:aFoot)
      // � ������ ���������� ����� �����
      FOR EACH oCol IN oBrw:aColumns
         cCol := oCol:cName
         IF cCol == "ORDKEYNO" .OR. cCol == "SELECTOR"
         ELSE
            cCol := oCol:cName
            oCol:cFooting := "[" + cCol + "]"
         ENDIF
      NEXT
   ENDIF

   //----- 07.11.23 ---- ������ �������
   FOR EACH oCol IN oBrw:aColumns
      cCol := oCol:cName
      IF cCol == "ORDKEYNO"
         oCol:nClrHeadFore := oTsb:n12Clr3Fore    // 3 , ������ ����� �������   ������� 2
         oCol:nClrHeadBack := oTsb:n12Clr4Back    // 4 , ���� ����� �������     ������� 2
         oCol:nClrFootFore := oTsb:n12Clr9Fore    // 9 , ������ ������� ������� ������� 2
         oCol:nClrFootBack := oTsb:n12Clr10Back   // 10, ���� ������� �������   ������� 2
      ELSEIF cCol == "SELECTOR"
      ELSE
         EXIT
      ENDIF
   NEXT

   IF oBrw:lSelector
      oBrw:nClrSelectorHdBack := oTsb:nClrSelectorHdBack // ���� ����� � ������� ��������� - ������� 1
   ENDIF

RETURN NIL

///////////////////////////////////////////////////////////////////
// ����������� ����� ��� ��������� ����������
STATIC FUNCTION TsbTracing(oBrw)
   LOCAL oCol, cMsg

   cMsg := "----------" + ProcNL(1) + ";"
   FOR EACH oCol IN oBrw:aColumns
       cMsg += HB_NtoS(hb_enumindex(oCol)) + "  " + oCol:cName
       cMsg += "  " + oCol:cFieldTyp  + "  " + oCol:cPicture
       cMsg += "  nWidth=" + HB_NtoS(oCol:nWidth)
       cMsg += "  nAlign=" + HB_NtoS(oCol:nAlign) + ";"
   NEXT
   AlertInfo(cMsg + REPL(";",30))

RETURN NIL

///////////////////////////////////////////////////////////////////
STATIC FUNCTION myGetNumbaColumn(oBrw,cFld)
   LOCAL nI, oCol, cCol, nCol := 0

   FOR nI := 1 TO Len(oBrw:aColumns)
      oCol := oBrw:aColumns[ nI ]
      cCol := oCol:cName
      IF UPPER(cCol) == UPPER(cFld)
         nCol := nI
         EXIT
      ENDIF
   NEXT

RETURN nCol

///////////////////////////////////////////////////////////////////
FUNCTION myTsbExtern(oBrw)
   LOCAL cMsg, cBlock, nRet, cRun, cFld, cErr, nCell, aFont
   LOCAL oCol, oCell, nY, nX, nW, nH, aVal, cForm, cSprAls, aArr

   oCol    := oBrw:aColumns[ oBrw:nCell ]
   aArr    := oCol:Cargo
   cRun    := aArr[1]
   cFld    := aArr[2]
   cErr    := aArr[3]
   aFont   := aArr[4]
   cSprAls := aArr[5]
   nCell   := oBrw:nCell  // Column �
   cForm   := oBrw:cParentWnd
   oCell   := oBrw:GetCellInfo(oBrw:nRowPos)
   nY      := GetProperty(cForm, "Row") + GetTitleHeight()
   nY      += oCell:nRow + oBrw:nHeightHead + 4
   nX      := GetProperty(cForm, "Col") + GetBorderWidth()
   nX      += oCell:nCol
   nW      := oCell:nWidth
   nH      := oCell:nHeight
   aVal    := { nY, nX, aFont, cSprAls, cForm }
   cBlock  := cRun + "(" + HB_ValToExp(aVal) + ")"

   IF !hb_IsFunction( cRun )
      cMsg := "Functions: " + cRun + "() not in the EXE file!;"
      cMsg += "call - " + cErr + ";" + ProcNL()
      HMG_Alert( cMsg, {"&Continue"}, "Error!", ICON_STOP )
      nRet := 0
   ELSE
      nRet := Eval( hb_macroBlock( cBlock ) )
      IF nRet # 0
         IF (oBrw:cAlias)->( RLock() )
            (oBrw:cAlias)->( FIELDPUT( FIELDNUM(cFld), nRet ) )
            (oBrw:cAlias)->( DbUnlock() )
            (oBrw:cAlias)->( DbCommit() )
            DO EVENTS
            oBrw:GoRight()     // ����������� ������ ������
         ELSE
            AlertStop("Recording is locked !;" + ProcNL())
         ENDIF
      ELSE
         DO EVENTS
         oBrw:Refresh( .F. )
      ENDIF
   ENDIF

RETURN NIL

///////////////////////////////////////////////////////////////////
// 1 , ����� � ������� �������
STATIC FUNCTION myCellColorFore( nAt, nCol, oBrw )
   LOCAL nColor, oTsb, nText, lDel, cCol, nTextNoDb, nClrDel
   Default nAt := 0 , nCol := 0

   oTsb      := oBrw:Cargo:oParam               // �������� ������ �� �������
   nText     := oTsb:nClr1Fore                  // ���� ����� �������
   nTextNoDb := oTsb:nForeNoDbf                 // ���� ���� ������� 1-2/��������/����������
   nClrDel   := oTsb:nClrForeDel                // ����� �������� �������
   lDel      := (oBrw:cAlias)->( DELETED() )    // ������� �� ������ ?

   cCol := oBrw:aColumns[ nCol ]:cName
   IF cCol == "SELECTOR" .OR. cCol == "ORDKEYNO"
      nColor := nTextNoDb
   ELSE

      nColor := nText
      // ��� ������� ��������� ������
      IF lDel // ������� �� ������ ?
         nColor := nClrDel
      ENDIF

   ENDIF

RETURN nColor

////////////////////////////////////////////////////////////////////////////////////
// 2 , ���� � ������� �������
STATIC FUNCTION myCellColorBack( nAt, nCol, oBrw )
   LOCAL nColor, lDel, oTsb, nTBC, nTBC2, nClrDel, nBC1Col, cCol
   LOCAL nBCFltr, aColFltr, nJ, lZebra
   Default nAt := 0, nCol := 0

   oTsb     := oBrw:Cargo:oParam               // �������� ������ �� �������
   nTBC     := oTsb:nClr2Back                  // ���� ���� �������
   nTBC2    := oTsb:nClr22Bck                  // ���� ������\�������� row
   nClrDel  := oTsb:nClrBackDel                // ���� �������� �������
   nBC1Col  := oTsb:nClrNoDbf                  // ���� ���� ������� 1-2/��������/���������
   nBCFltr  := CLR_YELLOW                      // ���� ���� ������� ������� � ��������
   aColFltr := oTsb:aColNumFltr                // ������ ������� ������� � ��������
   lDel     := (oBrw:cAlias)->( DELETED() )    // ������� �� ������ ?
   lZebra   := oTsb:lShowZebra                 // ����� ������\�������� ������
   IF lZebra == NIL
      lZebra := .F.
   ENDIF

   cCol := oBrw:aColumns[ nCol ]:cName
   IF cCol == "SELECTOR" .OR. cCol == "ORDKEYNO"
      nColor := nBC1Col
   ELSE

      IF lZebra                // ������\�������� row
         IF oBrw:nAt % 2 == 0
            nColor := nTBC2
         ELSE
            nColor := nTBC
         ENDIF
      ELSE
         nColor := nTBC
      ENDIF

      // ���� ���� ������ �� �������
      IF LEN(aColFltr) > 0
         FOR nJ := 1 TO LEN(aColFltr)
            IF aColFltr[nJ] == nCol
               nColor := nBCFltr
            ENDIF
         NEXT
      ENDIF

      // ��� ������� ��������� ������
      IF lDel                 // ������� �� ������ ?
         nColor := nClrDel
      ENDIF

   ENDIF

RETURN nColor

/////////////////////////////////////////////////////////////////////////////////
FUNCTION Tbrowse_Header(cMenu,nRowPix,nColPix,nAt,oBrw)
   LOCAL cForm, nCol, oCol, nY, nX, hFont1, hFont2, hFont3, nLine, cNCol, cCol
   LOCAL cMsg, cName, cVirt, cCnr, nCnr, nRzrv, cExit, lVirt, aMenu, nMenu
   LOCAL nJ, lFilter, lChange, cFilter, nDel, cMenu2, nRowPos, nCell, cSpcHd
   LOCAL aFltr, aColFltr, a2Fltr, c1Fltr, c2Fltr, oTsb

   oTsb     := oBrw:Cargo:oParam
   aFltr    := oTsb:aColFilter                        // ������� ������� � ��������
   aColFltr := oTsb:aColNumFltr                       // ������ ������� ������� � ��������
   nRzrv    := nRowPix                                // �� ���������
   cForm    := oBrw:cParentWnd
   nLine    := nAt                                    // ����� ������ ������� � �������
   nCol     := Max(oBrw:nAtColActual( nColPix ), 1 )  // ����� �������� ������� ������� � �������
   oCol     := oBrw:aColumns[ nCol ]
   cName    := oCol:cName
   cSpcHd   := oCol:cSpcHeading
   cVirt    := ",ORDKEYNO,SELECTOR,"
   cCnr     := oBrw:aColumns[ nCol ]:cSpcHeading
   nCnr     := Val( cCnr )
   cNCol    := ' "' + hb_ntos(nCnr) + '"'
   nY       := GetProperty(cForm, "Row") + GetTitleHeight()
   nY       += GetMenuBarHeight() + oBrw:nTop
   nX       := GetProperty(cForm, "Col") + GetBorderWidth()
   // ����� ���������� �� ����� �������
   nY       += IIF( oBrw:lDrawSuperHd, oBrw:nHeightSuper , 0 )
   nX       += oBrw:nLeft + oCol:oCell:nCol - 4
   hFont1   := GetFontHandle(oTsb:aFont[1])
   hFont2   := GetFontHandle(oTsb:aFont[2])
   hFont3   := GetFontHandle(oTsb:aFont[4])
   cExit    := myLangeRes(1)  // �����
   aMenu    := myLangeRes(4)  // ������ ���� - �������
   nMenu    := 0
   lFilter  := .F.
   lChange  := .F.
   cFilter  := ""

   IF oBrw:lDrawSpecHd  // ���� ���������
      cNCol := ' "' + hb_ntos(nCnr) + '"'
   ELSE
      cNCol := ' "' + hb_ntos(nCol) + '"'
   ENDIF

   cVirt    := ",ORDKEYNO,SELECTOR,"
   IF cName $ cVirt
      // ������� ��������� ���������
      cMsg  := cMenu + "Virtual column: " + hb_ntos(nCol) + " [" + cName + "]"
      lVirt := .T.
   ELSE
      // ���� ����� ������� �������
      cMsg := cMenu + "Column: " + cNCol + " [" + cName + "]"
      lVirt := .F.
   ENDIF

   IF LEN(aColFltr) > 0
      FOR nJ := 1 TO LEN(aColFltr)
         IF aColFltr[nJ] == nCol  // ����� �������� �������
            lFilter  := .T.
            EXIT
         ENDIF
      NEXT
   ENDIF

   SET MENUSTYLE EXTENDED                       // switch menu style to advanced
   SetMenuBitmapHeight( 26 )                    // set image size

   DEFINE CONTEXT MENU OF &cForm
      IF lVirt
         MENUITEM aMenu[3] ACTION {|| nMenu := 3 } FONT hFont2  // ������� ��� ������� �� ��������
         MENUITEM aMenu[4] ACTION {|| Tbrowse_Zero(oBrw)  } FONT hFont2  // ������� ��� ������� �� ��������
      ELSE
         cMenu2 := aMenu[1] + cNCol
         MENUITEM cMenu2 ACTION {|| nMenu := 1, a2Fltr := Tbrowse_MenuFltr(oBrw,cMenu2,cName) } FONT hFont2
         IF lFilter
            MENUITEM aMenu[2] + cNCol ACTION {|| nMenu := 2 } FONT hFont2
         ELSE
            MENUITEM aMenu[2] + cNCol DISABLED FONT hFont2
         ENDIF
         SEPARATOR
            MENUITEM aMenu[3] ACTION {|| nMenu := 3 } FONT hFont2
      ENDIF
      //SEPARATOR
      //MENUITEM cExit ACTION {|| nMenu := 0 } FONT hFont3 NAME 0079
   END MENU

   _ShowContextMenu(cForm, nY, nX, .f. ) // SHOWING DROP OUT MENU
   DO EVENTS

   DEFINE CONTEXT MENU OF &cForm         // delete menu after exiting
   END MENU

   DO EVENTS

   IF nMenu == 0                  // ������ �� ������
      lChange := .F.
   ELSEIF nMenu == 1              // ������ ������ �� �������
      IF LEN(a2Fltr) > 0  // ��������� ������ �� �������
         lChange := .T.
         nDel    := 0
         FOR nJ := 1 TO LEN(aColFltr)
            IF aColFltr[nJ] == nCol  // ����� �������� �������
               nDel := nJ
               EXIT
            ENDIF
         NEXT
         c1Fltr := a2Fltr[1]  // ������ �������
         c2Fltr := a2Fltr[2]  // ������ �������, ������
         IF nDel == 0
            // ����� ������� ������� ��� �������
            AADD( aFltr   , c1Fltr )
            AADD( aColFltr, nCol )   // ����� �������� �������
            cSpcHd += "  [" + hb_ntos(LEN(aFltr)) + "]"
         ELSE
            //  ������ ��� ����
            aFltr[nDel] := c1Fltr
         ENDIF
         oCol:cSpcHeading := cSpcHd
         // �������� � ���������� ������� ������� - ������  20x20
         //oCol:uBmpSpcHd := {|nc,ob| nc := ob:Cargo, nc:hFltrAdd20   }
         // �������� ������ �� ������ ��������, ���� ����
         cFilter := ""
         FOR nJ := 1 TO LEN(aFltr)
            cFilter += aFltr[nJ] + IIF(nJ==LEN(aFltr),""," .AND. ")
         NEXT
      ENDIF

   ELSEIF nMenu == 2              // ������� ������ �� �������
      lChange := .T.
      nDel    := 0
      FOR nJ := 1 TO LEN(aColFltr)
         IF aColFltr[nJ] == nCol    // ����� �������� �������
            nDel := nJ
            EXIT
         ENDIF
      NEXT
      IF nDel > 0
         // ������� ������� ������� ��� �������
         ADel( aFltr   , nDel, .T. )
         ADel( aColFltr, nDel, .T. )
      ENDIF
      IF AT( "[", cSpcHd ) > 0
         cSpcHd := SUBSTR( cSpcHd, 1, AT("[",cSpcHd) - 2 )
      ENDIF
      oCol:cSpcHeading := cSpcHd
      // �������� � ���������� ������� ������� - �������_���� 20x20
      //oCol:uBmpSpcHd := {|nc,ob| nc := ob:Cargo, nc:hArrDown20  }
      // ������� ������� ������� �� �������
      IF LEN(aFltr) == 0
         cFilter := ""
      ELSE
         cFilter := ""
         FOR nJ := 1 TO LEN(aFltr)
            cFilter += aFltr[nJ] + IIF(nJ==LEN(aFltr),""," .AND. ")
         NEXT
      ENDIF

   ELSEIF nMenu == 3              // ������� ��� ������� �� ��������
      lChange  := .T.
      aFltr    := {}
      aColFltr := {}
      nCnr     := 0
      FOR EACH oCol IN oBrw:aColumns
         cCol := oCol:cName
         IF cCol == "SELECTOR" .OR. cCol == "ORDKEYNO"
            // ������� ���������
         ELSE
            IF oCol:lVisible
               oCol:cSpcHeading := hb_ntos( ++nCnr )
               // �������� � ���������� ������� ������� - �������_���� 20x20
               //oCol:uBmpSpcHd   := {|nc,ob| nc := ob:Cargo, nc:hArrDown20  }
            ENDIF
         ENDIF
      NEXT
      // ������� ������� ������� �� �������
      cFilter := ""

   ENDIF

   nRowPos := oBrw:nRowPos
   nCell   := oBrw:nCell
   // ���������� ����� � ���������
   //oBrw:DrawHeaders()
   IF lChange
      // ������������ �������� � ���������-������
      oTsb:aColFilter  := aFltr       // ������� ������� � ��������
      oTsb:aColNumFltr := aColFltr    // ������ ������� ������� � ��������

      // oBrw:Reset() - ��� �� ����, ��� ���� � oBrw:FilterData()
      IF LEN(cFilter) == 0
         oBrw:FilterData()
      ELSE
         oBrw:FilterData( cFilter )         // ��������� ������� �� ����
      ENDIF
      //mySuperHeaderChange( oBrw, cFilter )  // �������� ���������� �������

      // ��� ���������� ������������ ������� (�� ��������� ���� ���)
      DO EVENTS
      nCell := nCell - 1
      oBrw:GoPos( nRowPos, nCell )          // ������������ ������ � ������� �� ������/�������
      oBrw:GoRight()
   ENDIF

   IF oBrw:nLen == 0
      Tbrowse_Zero(oBrw)
   ENDIF

   DO EVENTS
   oBrw:SetFocus()

RETURN NIL

////////////////////////////////////////////////////////////////////////////
FUNCTION Tbrowse_SuperHd(cMenu,nRowPix,nColPix,nAt,oBrw)
   LOCAL cForm, nCell, nCol, oCol, nY, nX, hFont1, hFont2, hFont3, lReset
   LOCAL cTitle, nRow, nLine, oTsb, aItm, nItm, cItm, cExit, cName
   LOCAL aMenu, cVirt, lVirt, nMenu

   oTsb   := oBrw:Cargo:oParam
   aItm   := oTsb:aMenuTest
   cExit  := myLangeRes(1)  // �����
   aItm   := myLangeRes(2)  // ������ ���� -> TsbViewLang.prg
   cForm  := oBrw:cParentWnd
   nRow   := oBrw:GetTxtRow(nRowPix)                 // �� �� ! ����� ������ ������� � �������
   nCol   := Max(oBrw:nAtColActual( nColPix ), 1 )   // ����� �������� ������� ������� � �������
   nCell  := oBrw:nCell                              // ����� ������ � �������
   oCol   := oBrw:aColumns[ nCol ]
   cName  := oCol:cName
   cVirt  := ",ORDKEYNO,SELECTOR,"
   nLine  := nAt                                     // ����� ������ ������� � �������
   cTitle := cMenu
   nY     := GetProperty(cForm, "Row") + GetTitleHeight()
   nY     += GetMenuBarHeight() + oBrw:nTop
   //nY     += IIF( oBrw:lDrawSuperHd, oBrw:nHeightSuper , 0 )
   nX     := GetProperty(cForm, "Col") + GetBorderWidth()
   nX     += oBrw:nLeft + oCol:oCell:nCol - 2
   hFont1 := GetFontHandle(oTsb:aFont[2])
   hFont2 := GetFontHandle(oTsb:aFont[1])
   hFont3 := GetFontHandle(oTsb:aFont[4])
   lReset := .F.    // ��������� ����
   aMenu  := { "RU1251", "RU866", "UA1251", "UA866", "EN" }
   nMenu  := 0

   IF cName $ cVirt // ������� ��������� ����
      lVirt := .T.
   ELSE
      lVirt := .F.
   ENDIF

   SET MENUSTYLE EXTENDED                       // switch menu style to advanced
   SetMenuBitmapHeight( 26 )                    // set image size

   DEFINE CONTEXT MENU OF &cForm
      IF lVirt
         MENUITEM "codepage RU1251" ACTION {|| lReset := .T., ReopenDbase(oBrw,"RU1251"), nMenu := 1 } FONT hFont2
         MENUITEM "codepage RU866"  ACTION {|| lReset := .T., ReopenDbase(oBrw,"RU866") , nMenu := 2 } FONT hFont2
         MENUITEM "codepage UA1251" ACTION {|| lReset := .T., ReopenDbase(oBrw,"UA1251"), nMenu := 3 } FONT hFont2
         MENUITEM "codepage UA866"  ACTION {|| lReset := .T., ReopenDbase(oBrw,"UA866") , nMenu := 4 } FONT hFont2
         MENUITEM "codepage EN"     ACTION {|| lReset := .T., ReopenDbase(oBrw,"EN")    , nMenu := 5 } FONT hFont2
         //MENUITEM "codepage DEWIN"  ACTION {|| lReset := .T., ReopenDbase(oBrw,"DEWIN")  } FONT hFont2
      ELSE
         FOR EACH cItm IN aItm
            nItm := hb_enumindex(cItm)
            IF cItm == "SEPARATOR"
               SEPARATOR
            ELSE
               MENUITEM cItm ACTION  {|| Tbrowse_MenuSupHd( VAL(This.Name),This.Name,cItm,oBrw) } FONT hFont2 NAME &(StrZero(nItm,4))
            ENDIF
         NEXT
      ENDIF
      //SEPARATOR
      //MENUITEM cExit ACTION {|| Nil } FONT hFont3 NAME 0089
   END MENU

   _ShowContextMenu(cForm, nY, nX, .f. ) // SHOWING DROP OUT MENU
   DO EVENTS

   DEFINE CONTEXT MENU OF &cForm         // delete menu after exiting
   END MENU

   DO EVENTS

   IF nMenu > 0
      oBrw:aSuperHead[1,3] := aMenu[nMenu]  // �������� ����������
   ENDIF

   IF lReset  // ��������� ����
      oBrw:Reset()
      oBrw:GoTop()
   ENDIF

   oBrw:SetFocus()

RETURN NIL


///////////////////////////////////////////////////////////////////////////////////
FUNCTION BtnTsbMenu( nMenu, cName )   // ������� �����
   LOCAL cForm, nY, nX, hFont1, hFont2, hFont3, lReset
   LOCAL oTsb, aItm, nItm, cItm, cExit, oWnd, oBrw, cObj

   oWnd   := ThisWindow.Object  // ������ ����
   cForm  := oWnd:Name
   oBrw   := oWnd:Cargo:oBrw
   oTsb   := oBrw:Cargo:oParam
   cObj   := cName //ow:Name
   cExit  := myLangeRes(1)  // �����
   aItm   := myLangeRes(2)  // ������ ����

   nY     := GetProperty(cForm, "Row") + GetTitleHeight()
   nY     += GetProperty(cForm, cObj, "Row") + GetProperty(cForm, cObj, "Height")
   nX     := GetProperty(cForm, "Col") + GetBorderWidth() * 2
   nX     := GetProperty(cForm, cObj, "Col")
   hFont1 := GetFontHandle(oTsb:aFont[2])
   hFont2 := GetFontHandle(oTsb:aFont[1])
   hFont3 := GetFontHandle(oTsb:aFont[4])
   lReset := .F.    // ��������� ����

   SET MENUSTYLE EXTENDED                       // switch menu style to advanced
   SetMenuBitmapHeight( 26 )                    // set image size

   DEFINE CONTEXT MENU OF &cForm
      IF nMenu == 1
         MENUITEM "codepage RU1251" ACTION {|| lReset := .T., ReopenDbase(oBrw,"RU1251") } FONT hFont2
         MENUITEM "codepage RU866"  ACTION {|| lReset := .T., ReopenDbase(oBrw,"RU866")  } FONT hFont2
         MENUITEM "codepage UA1251" ACTION {|| lReset := .T., ReopenDbase(oBrw,"UA1251") } FONT hFont2
         MENUITEM "codepage UA866"  ACTION {|| lReset := .T., ReopenDbase(oBrw,"UA866")  } FONT hFont2
         MENUITEM "codepage EN"     ACTION {|| lReset := .T., ReopenDbase(oBrw,"EN")     } FONT hFont2
         //MENUITEM "codepage DEWIN"  ACTION {|| lReset := .T., ReopenDbase(oBrw,"DEWIN")  } FONT hFont2
      ELSE
         FOR EACH cItm IN aItm
            nItm := hb_enumindex(cItm)
            IF cItm == "SEPARATOR"
               SEPARATOR
            ELSE
               MENUITEM cItm ACTION  {|| Tbrowse_MenuSupHd( VAL(This.Name),This.Name,cItm,oBrw) } FONT hFont2 NAME &(StrZero(nItm,4))
            ENDIF
         NEXT
      ENDIF
      SEPARATOR
      MENUITEM cExit ACTION {|| Nil } FONT hFont3 NAME 0089
   END MENU

   _ShowContextMenu(cForm, nY, nX, .f. ) // SHOWING DROP OUT MENU
   DO EVENTS

   DEFINE CONTEXT MENU OF &cForm         // delete menu after exiting
   END MENU

   DO EVENTS

   IF lReset  // ��������� ����
      oBrw:Reset()
      oBrw:GoTop()
   ENDIF

   oBrw:SetFocus()

RETURN NIL

///////////////////////////////////////////////////////////////////////////////////
FUNCTION myFilterClear(oBrw)
   LOCAL cForm, hFont1, hFont2, hFont3, oTsb, cExit, aMenu, nMenu

   oTsb   := oBrw:Cargo:oParam
   cExit  := myLangeRes(1)   // �����
   aMenu  := myLangeRes(11)  // ������ ����
   cForm  := oBrw:cParentWnd
   nMenu  := 0
   hFont1 := GetFontHandle(oTsb:aFont[2])
   hFont2 := GetFontHandle(oTsb:aFont[1])
   hFont3 := GetFontHandle(oTsb:aFont[4])

   SET MENUSTYLE EXTENDED                       // switch menu style to advanced
   SetMenuBitmapHeight( 26 )                    // set image size

   DEFINE CONTEXT MENU OF &cForm
      MENUITEM aMenu[1] ACTION  {|| nMenu := 1 } FONT hFont1
      SEPARATOR
      MENUITEM aMenu[2] ACTION  {|| nMenu := 2 } FONT hFont2
      //SEPARATOR
      //MENUITEM cExit    ACTION  {|| nMenu := 0 } FONT hFont3
   END MENU

   _ShowContextMenu(cForm, , , .f. ) // SHOWING DROP OUT MENU
   DO EVENTS

   DEFINE CONTEXT MENU OF &cForm         // delete menu after exiting
   END MENU

   DO EVENTS

RETURN nMenu

///////////////////////////////////////////////////////////////////////////////////
FUNCTION Tbrowse_MenuSupHd( nMenu, cMetka, cMenu, oBrw)
   LOCAL hWnd, cForm, c2 := cMetka

   IF ISOBJECT(oBrw)
      cForm := oBrw:cParentWnd
      hWnd  := GetFormHandle(cForm)
      DBSELECTAREA(oBrw:cAlias)
      IF nMenu == 1
         myDbGetAllUse(cMenu)            // ������ �������� ��
      ELSEIF nMenu == 2
         myDbIndexesThis(cMenu)          // ������� ���� ����
      ELSEIF nMenu == 3
         myDbIndexChange(cMenu, oBrw)    // ����������� ������ ����
      ELSEIF nMenu == 4
         myDbRelation(cMenu)             // Set relation ���� ����
      ELSEIF nMenu == 5
         myDbFilter(cMenu)               // DbFilter ���� ����
      ELSEIF nMenu == 6
         myDbStructure(cMenu)            // ��������� ���� ����
      ELSEIF nMenu == 8
         myDbWriteCsv(cMenu)             // ��������� CSV
      ELSEIF nMenu == 9
         myGetLang(cMenu)                // ����� ���� � ���� ?
      ELSEIF nMenu == 10
         FontsListAll(cMenu)             // ����� � ���������
      ELSEIF nMenu == 11
         FontsTbrowse(cMenu, oBrw)       // ����� � �������
      ELSEIF nMenu == 12
         MsgListAllWinTsbView(cMenu)     // ������ ���� � ���������
      ELSEIF nMenu == 14
         MsgAbout_TsbViewer(cMenu)       // � ���������
      ENDIF
   ENDIF

RETURN NIL

///////////////////////////////////////////////////////////////////////////
FUNCTION myFunc0(oWnd, nMsg, oBrw)
   MsgDebug(oWnd:Name,nMsg, oBrw:cAlias)
RETURN NIL

//////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myRCellClick(oWnd,ky,oBrw)
   LOCAL oCol, nCol, cType, xVal, cMsg, nY, nX, cForm, nPost
   LOCAL nI, aLang, nLen, oPos, hFont1, hFont2, nHFS
   LOCAL lMenuStyle, nMenuBitmap

   cForm := oWnd:Name
   cForm := oBrw:cParentWnd                // ��� ���
   nCol  := oBrw:nCell
   oCol  := oBrw:aColumns[nCol]
   cType := oCol:cFieldTyp                 // valtype field
   xVal  := oBrw:GetValue(nCol)            // real value
   nPost := ky                             // ����� ������� - ������
   oPos  := oBrw_Cell(oBrw, nCol)          // ���������� ��������� ������

   //? ProcNL(), "||", cForm, "ky=",ky, "nCol=",nCol
   //_o2Log(oPos, 20, "==> .T. oPos: ", .T.)

   DrawRR(YELLOW, 3, oPos:Y, oPos:X, oPos:H, oPos:W)
   wApi_Sleep(500)
   DrawRR(GREEN, 3, oPos:Y, oPos:X, oPos:H, oPos:W)
   wApi_Sleep(500)

   // ����� ���������� �� �������������� �����
   //nX := _HMG_MouseCol
   //nY := _HMG_MouseRow
   // ����� ���������� �� ����� ������� + ���������� ����
   nY := INT( oPos:Y + oPos:H + GetTitleHeight() + oWnd:Row )
   nX := INT( oPos:X + oWnd:Col )

   hFont1 := oBrw:hFontHead
   hFont2 := oBrw:hFont
   nHFS   := GetTextHeight( 0, "H", hFont1 )   // ������ �����
   nHFS   += nHFS/2                            // ������ ������ ������������ ����

   nLen  := 0
   aLang := myLangeRes(13)  // -> TsbViewLang.prg
   FOR nI := 1 TO LEN(aLang) - 1
       nLen := MAX(nLen, LEN(aLang[nI]))
   NEXT
   cMsg := REPL("A",nLen)

   lMenuStyle  := IsExtendedMenuStyleActive()     // menu style EXTENDED/STANDARD
   nMenuBitmap := GetMenuBitmapHeight()           // bmp height in context menu

   SET MENUSTYLE EXTENDED        // switch menu style to advanced
   SetMenuBitmapHeight( nHFS )   // set image size

   //SetThemes(2)  // ���� "Office 2000 theme" � ContextMenu
   //SetThemes(3)  // ���� "Dark theme" � ContextMenu

   DEFINE CONTEXT MENU OF &cForm
       // ���������� � �����
       MENUITEM  aLang[1]  ACTION  {|| System.Clipboard := cValToChar(oBrw:GetValue(nCol)) , oBrw:SetFocus() } FONT hFont1
       SEPARATOR
       // �������� �� ������
       MENUITEM  aLang[2]  ACTION  {|| Copy_Cell_Clipboard(1,oBrw,cType,nCol) }  FONT hFont1
       SEPARATOR
       // �������
       MENUITEM  aLang[3]  ACTION  {|| Copy_Cell_Clipboard(0,oBrw,cType,nCol) }  FONT hFont2
   END MENU

   _ShowContextMenu(cForm, nY, nX, .f. ) // SHOWING DROP OUT MENU
   InkeyGui(100)

   IF _IsWindowDefined( cForm )
      DEFINE CONTEXT MENU OF &cForm    // deleting menu after exiting
      END MENU
   ENDIF

   //SetThemes(1)                   // restoring the themes - ContextMenu
   SetMenuBitmapHeight(nMenuBitmap) // bmp height in context menu   - return as it was
   _NewMenuStyle( lMenuStyle )      // menu style EXTENDED/STANDARD - return as it was

   DO EVENTS

RETURN Nil

//////////////////////////////////////////////////////////////////
STATIC FUNCTION Copy_Cell_Clipboard(nMenu,oBrw,cType,nCol)
   LOCAL xWrt

   IF nMenu == 1
      xWrt := System.Clipboard
   ELSE
      xWrt := ""
   ENDIF

   IF cType == VALTYPE(xWrt)
   ELSEIF cType == "C" .AND. VALTYPE(xWrt) == "N"
      xWrt := HB_NtoS(xWrt)
   ELSEIF cType == "N" .AND. VALTYPE(xWrt) == "C"
      xWrt := VAL(xWrt)
   ELSEIF cType == "D"
      xWrt := CTOD(xWrt)
   ELSEIF cType == "T" .OR. cType == "@"
      xWrt := hb_CToT(xWrt)
   ELSEIF cType == "L"
      xWrt := IIF( nMenu == 1, .T., .F. )
   ENDIF

   IF cType $ "+=^"   // Type field: [+] [=] [^]
      AlertStop('Fields of type: "+" "=" "^" cannot be edited in the database!')
   ELSE
      IF (oBrw:cAlias)->( RLock() )
          oBrw:SetValue(nCol, xWrt)
         (oBrw:cAlias)->( DbUnlock() )
         (oBrw:cAlias)->( DbCommit() )
      ELSE
         AlertStop("Recording is locked!")
      ENDIF
   ENDIF

   oBrw:DrawSelect()
   oBrw:Refresh()
   DO EVENTS
   oBrw:SetFocus()

RETURN Nil

//////////////////////////////////////////////////////////////////
STATIC FUNCTION oBrw_Cell( oBrw, nCol, nRow )
   LOCAL o := oHmgData(), oCol
   Default nCol := oBrw:nCell, nRow := oBrw:nRowPos

   IF HB_ISCHAR(nCol) ; nCol := oBrw:nColumn(nCol)
   ENDIF

   oCol := oBrw:aColumns[ nCol ]

   o:N := oCol:cName
   o:Y := oBrw:nTop
   o:X := GetBorderWidth()
   IF oBrw:lDrawSuperHd ; o:Y += oBrw:nHeightSuper
   ENDIF
   IF oBrw:lDrawHeaders ; o:Y += oBrw:nHeightHead
   ENDIF
   IF oBrw:lDrawSpecHd  ; o:Y += oBrw:nHeightSpecHd
   ENDIF
   IF nRow > 0 .and. nRow <= oBrw:nRowCount()
      o:Y += ( nRow - 1 ) * oCol:oCell:nHeight
   ENDIF
   o:X += oCol:oCell:nCol
   o:H := oCol:oCell:nHeight
   o:W := oCol:oCell:nWidth

RETURN o

///////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION DrawRR( focus, nPen, y, x, h, w, cWindowName, nCurve )
   LOCAL aColor

   DEFAULT y := This.Row, x := This.Col, h := This.Height, w := This.Width
   DEFAULT focus := .F., cWindowName := ThisWindow.Name, nCurve := 5
   DEFAULT nPen := 3, focus := .F.

   IF ISARRAY( focus ) ; aColor := focus
   ELSE ; aColor := iif( focus, { 0, 120, 215 }, { 100, 100, 100 } )
   ENDIF

   DRAW ROUNDRECTANGLE IN WINDOW ( cWindowName ) ;
      AT y + 2, x + 2 TO y + h - 2, x + w - 2 ;
      ROUNDWIDTH nCurve ROUNDHEIGHT nCurve ;
      PENCOLOR aColor PENWIDTH nPen

RETURN NIL

///////////////////////////////////////////////////////////////////////////////
FUNCTION MsgAboutThis()
RETURN SHOW_TITLE + SHOW_VERS
