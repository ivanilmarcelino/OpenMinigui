/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2020-2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * ������ � ����
*/

#define _HMG_OUTLOG
#include "minigui.ch"
#include "dbinfo.ch"
/////////////////////////////////////////////////////////////////////////////////////////////////
FUNCTION Menu3InfoDbase(ow,ky,cn,ob)
   LOCAL aMenu, aLang, nBmpSize, nFSize, nChoice, nPos, lExit, cRun
   LOCAL xRet, cForm, aFntExt, cFunc, cMsg, nOrder, lMn5

   ? ProcNL(), ow:Name,ky,cn,ob:cAlias,This.&(cn).Caption
   cForm := ow:Name
   DBSELECTAREA(ob:cAlias)
   nOrder := INDEXORD()
   lMn5   := IIF( OrdCount() == 0, .T., .F. )

   IF App.Cargo:cLang == "RU"
      aLang := { "������ �������� ��" , "��������� ���� ����" , "������� �� �������", "������� �� ������� (���)" ,;
                 "������� ���� ����", "������� ������", "����������� ������ ����", "�������� ������" }
   ELSE
      aLang := { "List of open DBs" , "Structure of this database" , "Goto column", "Goto column (name)" ,;
                 "Indexes of this database", "Open index", "Switch database index", "Add index" }
   ENDIF

   aMenu := {}
   AADD( aMenu, { "iDbf48", aLang[1] , .F.  , "myDbGetAllUse"  , "Str1" , 1, ow, ob } )     // -> util_dbf.prg
   AADD( aMenu, {                                                                   } )
   AADD( aMenu, { "iDbf48", aLang[2] , .F.  , "myDbStructure"  , "Str2" , 2, ow, ob } )     // -> util_dbf.prg
   AADD( aMenu, {                                                                   } )
   AADD( aMenu, { "iDbf48", aLang[3] , .F.  , ""               , "Str3" , 3, ow, ob } )     // -> util_dbf.prg
   AADD( aMenu, { "iDbf48", aLang[4] , .F.  , ""               , "Str4" , 4, ow, ob } )     // -> util_dbf.prg
   AADD( aMenu, {                                                                   } )
   AADD( aMenu, { "iDbf48", aLang[5] , .F.  , ""               , "Str5" , 5, ow, ob } )     // -> util_dbf.prg
   AADD( aMenu, { "iDbf48", aLang[6] , .F.  , ""               , "Str6" , 6, ow, ob } )
   AADD( aMenu, { "iDbf48", aLang[7] , lMn5 , ""               , "Str7" , 7, ow, ob } )
   //AADD( aMenu, { "iDbf48", aLang[7] , lMn5 , "myDbIndexNew" , "Str6" , 6, ow, ob } )

   nPos     := 3
   nBmpSize := 32
   nFSize   := 16
   lExit    := .F.
   aFntExt  := { "DejaVu Sans Mono", "Comic Sans MS" }
   nChoice  := DynamicContextMenuExtend( cForm, aMenu, nPos, nBmpSize, nFSize, lExit, aFntExt, "Icon" )
   IF nChoice > 0
      nPos  := aMenu[nChoice,6]
      cMsg  := aLang[nPos]
      cFunc := aMenu[nChoice,4]
      cRun  := cFunc + '("' + cMsg + '")' //+ HB_ValToExp(aMenu[nChoice]) + ')'
      IF nPos == 3 .OR. nPos == 4
         xRet := myDbColumn( cMsg, ow, ob, nPos )
      ELSEIF nPos == 5
         xRet := myDbIndexesThis( cMsg, ow, ob )
      ELSEIF nPos == 6
         xRet := myOpenIndex( cMsg, ow, ob )
      ELSEIF nPos == 7
         xRet := myDbIndexChange( cMsg, ow, ob )
      ELSEIF nPos == 8
         //xRet := myDbIndexNew( cMsg, ow, ob )
      ELSE
         IF MyIsFunNoRun(cFunc)
            xRet := EVal( hb_MacroBlock( cRun ) , cMsg )
         ELSE
            xRet := NIL
         ENDIF
      ENDIF
   ENDIF
   DO EVENTS

RETURN xRet

////////////////////////////////////////////////////////////////////////////
FUNCTION myOpenIndex(cTitle, oWnd, oBrw )
   LOCAL xRet := .F., aDrv, nI, owc, cFile, cPath, aIGet, cMsg, cMsg2
   LOCAL lOpen, cMsk, cVia, aFile, aUse, k, i, a

   ? ProcNL(), cTitle, oWnd, oWnd:ClassName, oBrw, oBrw:ClassName
   aDrv := {}
   AADD( aDrv, { "DBFCDX"       , "*.cdx" } )
   AADD( aDrv, { "DBFNTX"       , "*.ntx" } )
   AADD( aDrv, { "DBFNSX"       , "*.nsx" } )
   AADD( aDrv, { "SIXCDX"       , "*.cdx" } )
   AADD( aDrv, { "BMDBFCDX"     , "*.cdx" } )
   AADD( aDrv, { "BMDBFNTX"     , "*.ntx" } )
   AADD( aDrv, { "BMDBFNSX"     , "*.nsx" } )
   AADD( aDrv, { "BM_DBSEEKWILD", "*.???" } )

   k := Len( aDrv )
   FOR EACH a IN aDrv
       i := hb_enumindex()
       /*IF i < k
          RddSetDefault(a[1])
          ? i, a[1], a[2], right(a[1], 3)
          ?? dbinfo(RDDI_TABLEEXT )
          ?? dbinfo(RDDI_MEMOEXT  )
          ?? dbinfo(RDDI_ORDBAGEXT)
          ?? dbinfo(RDDI_ENCRYPTION)
          ?? dbinfo(RDDI_TRIGGER   )
          ?? dbinfo(RDDI_MEMOBLOCKSIZE)
       ENDIF*/
   NEXT

   owc   := oWnd:Cargo
   aUse  := owc:aUse      // �������� ���� - { cFile, lShared, cDrvDbf, cCdPg, cPsw, cSetDel }
   cFile := aUse[1]
   cPath := cFilePath(cFile)
   cVia  := aUse[3]
   cMsk  := "*.*"
   FOR nI := 1 TO LEN(aDrv)
      IF aDrv[nI,1] == cVia
         cMsk := aDrv[nI,2]
         EXIT
      ENDIF
   NEXT
   cMsg  := IIF( App.Cargo:cLang == "RU", "����� " + cVia, cVia + " files" )
   cMsg2 := IIF( App.Cargo:cLang == "RU", "��� �����", "All Files" )
   aIGet := { {cMsg, cMsk}, {cMsg2, "*.*"} }
   // ���� windows - ����� �����
   aFile := GetFile( aIGet, cTitle, cPath , .T. )
   IF LEN(aFile) > 0

      cFile := aFile[1]
      cPath := hb_FNameDir( aFile[1] )
      lOpen := .F.
      BEGIN SEQUENCE WITH { |e|break(e) }          // .F. - lReadonly
         ORDLISTADD( cFile )
         DBSetOrder(1)
         lOpen := .T.
      END SEQUENCE

      IF !lOpen
         cMsg := IIF( App.Cargo:cLang == "RU", "������ �������� ���������� ����� !",;
                       "ERROR opening index file !" )
         cMsg += ";" + cFile + ";;" + ProcNL()
         AlertStop( cMsg, , "ZZZ_B_STOP64", 64 )
       ENDIF

   ENDIF

RETURN xRet

//////////////////////////////////////////////////////////////////////////////
FUNCTION myDbIndexChange(cTitle, oWnd, oBrw)
   LOCAL aRet, nIndx, cOrd, cVal, cTtl, cMsg

   ? ProcNL(), cTitle, oWnd, oWnd:ClassName, oBrw, oBrw:ClassName

   aRet := Form_IndexChoice(cTitle, oWnd, oBrw)
   IF LEN(aRet) > 0
      nIndx := aRet[1]
      cOrd  := aRet[2]
      DbSelectArea(oBrw:cAlias)
      DbSetOrder(nIndx)
      oBrw:uLastTag := (oBrw:cAlias)->( OrdName(nIndx) )  // ��� ����� ������ �������
      // ������ � ������� �����������
      cVal := oBrw:aSuperhead[ 1, 3 ]            // cSuperHd  += ",  ORDER: [" + cOrder + "]"
      cMsg := cVal
      IF AT("[", cVal) > 0
         cMsg := SUBSTR( cVal, 1, AT("[", cVal ) - 1 )
      ENDIF
      cTtl := "[" + HB_NtoS(nIndx) + "/" + cOrd + "]"
      oBrw:aSuperhead[ 1, 3 ] := cMsg + cTtl
      oBrw:DrawHeaders()                         // ���������� ����������/�����/���������
      oBrw:Reset()
      oBrw:Refresh(.T.)
      oBrw:GoTop()
      DO EVENTS
   ENDIF
   oBrw:Setfocus()

RETURN NIL

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Form_IndexChoice(cTitle, oWnd, oBrw)
   LOCAL cIcon, cFont, nFontSize, aBackColor, aBColorTxt, cMsg
   LOCAL nY, nX, nW, nH, nG, nWTxt, nHTxt, nHBtn, nWBtn, nYBtn, nXBtn
   LOCAL nGBtn, nYCmb, nXCmb, nWCmb, ao, owc, aBtnFont, aBtnFClr
   LOCAL nHIco, cTxt, aBtn, bAct, a2Index, aIndex, aOrdName, nLR, nI
   LOCAL aFontIni, cVal, ahIco, aRet, cFontBtn, aNumIndex, nValIndx

   ao         := (App.Cargo)
   aFontIni   := ao:oIni:MAIN:COMSANMS   ; Default aFontIni := { "Tahona" , 14 , .F., .F. }
   cFontBtn   := aFontIni[1]
   nFontSize  := aFontIni[2]
   aBackColor := oWnd:Cargo:aBColor           // ���� ���� �����
   aBColorTxt := {255,255,240}
   cIcon      := "i1FindFile48"
   cFont      := ao:cFontName
   aBtnFClr   := { BLACK, YELLOW }
   aBtnFont   := { cFontBtn, nFontSize, .T. }
   aRet       := {}                           // ������� {  nIndex, cOrder }
   a2Index    := myDbIndexesThis("","ADIM")   // ������ ��������
   cMsg       := IIF( ao:cLang == "RU", " --- ��� ������� --- ", " --- without index --- " )
   aIndex     := { "(0), " + cMsg }
   cMsg       := IIF( ao:cLang == "RU", " -���- ", " -no- " )
   aOrdName   := { cMsg }
   aNumIndex  := { 0 }
   aMerge( aIndex  , a2Index[1] )
   aMerge( aOrdName, a2Index[2] )

   DbSelectArea(oBrw:cAlias)
   nValIndx   := INDEXORD() + 1
   FOR nI := 1 TO LEN(aIndex) - 1
      AADD(aNumIndex, nI )
   NEXT

   // ������ ������ Desktop
   nH   := GetDesktopHeight()
   IF nH <= 600                       ;  nG := 10
   ELSEIF nH >= 768 .AND. nH <= 864   ;  nG := 15
   ELSEIF nH > 864  .AND. nH <= 1080  ;  nG := 20
   ELSE                               ;  nG := 25
   ENDIF

   nX     := nLR := nG                      // ������ ����� � �����
   nY     := nG                             // ������ ������ � �����
   nW     := ao:aDisplayMode[1] * IIF( ao:aDisplayMode[2] <= 720, 0.9, 0.7) // ������� ����
   nH     := 560                            // ������� ����
   nHTxt  := nFontSize*2 + nFontSize/2      // ������ GET'��

   DEFINE WINDOW Form_Index AT nY, nX WIDTH nW HEIGHT nH ;
      ICON cIcon TITLE cTitle BACKCOLOR aBackColor      ;
      MODAL NOSIZE                                      ;
      FONT cFont SIZE nFontSize                         ;
      ON INIT    _wPost( 0)                             ;
      ON RELEASE _wSend(98)

      This.Cargo := oHmgData() ; owc := This.Cargo  // ��� ���� ������� ������ ��� ���������� (������� ������)
      owc:aBColor  := This.BackColor   // ���� ����
      owc:ahIcoDel := {}               // ��� �������� ������� ������ � �����
      owc:nIndex   := 0
      owc:cOrder   := ""

      nW       := This.ClientWidth
      nH       := This.ClientHeight
      nWTxt    := nW - nLR * 2

      @ 0, 0 LABEL Label_0 WIDTH nG HEIGHT nG VALUE '' INVISIBLE

      cVal := IIF( ao:cLang == "RU", " ������ �������� �� �� ", " List of indexes by database " )
      @ nY, nX FRAME Frame_1 CAPTION cVal ;
        WIDTH nWTxt HEIGHT nG*4 BACKCOLOR aBackColor //OPAQUE

      nYCmb   := nY + nG + nG/2
      nXCmb   := nX + nLR
      nWCmb   := This.Frame_1.Width - nG*2

      @ nYCmb, nXCmb COMBOBOXEX Combo_Index WIDTH nWCmb HEIGHT 220  ;
        ITEMS aIndex VALUE nValIndx IMAGE {} BACKCOLOR SILVER       ;
        ON CHANGE { |nI,ow| nI := This.Combo_Index.Value        ,;
                               ow := ThisWindow.Object          ,;
                               ow:Cargo:nIndex := aNumIndex[nI] ,; // ���������
                               ow:Cargo:cOrder := aOrdName[nI]  ,; // ���������
                               This.Label_0.Setfocus }

      nY += This.Frame_1.Height + nG * 2

      /////////////////////// Button ////////////////////////////
      nWBtn    := 350
      nHBtn    := 64 + nG/2
      nGBtn    := (nW - nWBtn*2) / 3
      nYBtn    := nY
      nXBtn    := nGBtn
      nHIco    := 64

      cTxt  := IIF( ao:cLang == "RU", "����������", "Install" )
      nXBtn := nGBtn
      aBtn  := { "Button_Open", cTxt, "iDbC64x1", "iDbC64x2", nHIco, aBtnFClr, aBtnFont, cVal }
      bAct  := {|| /*MsgDebug(This.Cargo),*/ _wPost(90) }   // ������� �� �����
      ahIco := my2DrawButtonGrad(nYBtn, nXBtn, nWBtn, nHBtn, aBtn, bAct, {40,221,65})
      IF LEN(ahIco) > 0 // ��� �������� ������� ������ � �����
         AADD( owc:ahIcoDel , ahIco[1] )
         AADD( owc:ahIcoDel , ahIco[2] )
      ENDIF

      cTxt  := IIF( ao:cLang == "RU", "������", "Cancel" )
      nXBtn := nGBtn * 2 + nWBtn
      aBtn  := { "Button_Exit", cTxt, "iReturn64x1", "iReturn64x2", nHIco, aBtnFClr, aBtnFont, cVal }
      bAct  := {|| /*MsgDebug(This.Cargo),*/ _wPost(99) }   // ������� �� �����
      ahIco := my2DrawButtonGrad(nYBtn, nXBtn, nWBtn, nHBtn, aBtn, bAct, CLR_HRED )
      IF LEN(ahIco) > 0 // ��� �������� ������� ������ � �����
         AADD( owc:ahIcoDel , ahIco[1] )
         AADD( owc:ahIcoDel , ahIco[2] )
      ENDIF

      nY += nHBtn + nG

      // �������� ������� ������ ����
      ThisWindow.Height := nY + GetTitleHeight() + GetBorderHeight()

      _o2log(owc, 15, ProcNL()+" -------------- ��������� ������� : => owc", .T.)

      WITH OBJECT This.Object
        :Event( 0, {|ow| // ������ ����� ���������� ����
                          This.Topmost := .F.
                          ? ProcNL(),">>> Start window: "+ow:Name
                          ow:Setfocus('Label_0')
                          DO EVENTS
                          Return Nil
                          })

        :Event(90, {|ow| // ���������� / Install
                         Local owc := ow:Cargo
                         aRet := { owc:nIndex , owc:cOrder }   // �������
                         _wPost(99,ow:Name)
                         Return Nil
                         } )

        :Event(98, {|ow| // ON Release
                         Local ah := ow:Cargo:ahIcoDel
                         ?  ProcNL()
                         ?? ">>> Exit button pressed! Window: "+ow:Name
                         ? Repl(".", 10),"Delete handle icon - ow:Cargo:ahIcoDel="
                         ?? ah, HB_ValToExp(ah)
                         IF IsArray(ah)
                            AEval(ah, {|h| DestroyIcon(h) })  // ������� ������ ������
                         ENDIF
                         DO EVENTS
                         Return Nil
                         } )

        :Event(99, {|ow| ow:Release() } )
      END WITH

      ON KEY F1     OF Form_Index ACTION NIL
      ON KEY ESCAPE OF Form_Index ACTION _wPost(99)

   END WINDOW

   CENTER   WINDOW Form_Index
   ACTIVATE WINDOW Form_Index

RETURN aRet          // �������


