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
FUNCTION my_Standard4( cForm, nBtn, cTitle, aBClr, nY, nX, nW, nH, cAls, cWndMain )
   LOCAL cVal, nI, hW, bInitForm, aThrd
   LOCAL cPathDbf := App.Cargo:cPathDbf  //GetStartUpFolder()
   LOCAL oWin, oUse, oMenu, oTsb, aEvent, oIndx
   DEFAULT cAls := "CUST_"+hb_ntos(nBtn)
   DEFAULT nY := 0, nX := 0, nW := 500, nH := 400
   DEFAULT aBClr  := { 93,114,148}
   DEFAULT cTitle := cForm + ". WINDOW STANDARD"

   SET LANGUAGE TO RUSSIAN
   SET CODEPAGE TO RUSSIAN

   cVal := nI := hW
   ? ProcNL(), cForm, _IsWindowDefined(cForm), hb_CdpSelect()
   ? cForm, nBtn, cTitle, aBClr, nY, nX, nW, nH, cAls, cWndMain

   aThrd := WaitThreadAvi( 'Create a table ...' )  // ������ ���� �������� � �������
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
   SET FONT TO "DejaVu Sans Mono", 13  // ����� ���� ��� �������

   oWin   := CreateDataWin(cTitle, aBClr, nY, nX, nW, nH)                // ��������� ����
   oUse   := CreateDateDbf(cPathDbf,'Customer2.dbf',cAls,"RU866",.T.)    // ���� ��� ��������� ���
   oIndx  := CreateDateIndex(oUse,cPathDbf)                              // ������ �������
   oMenu  := CreateDateMenu( {1,2,3,4,99} )                              // ����-������ ���� � ������� �� �������
   oTsb   := CreateDateTsb(oUse,oUse:cCodePage,"Checkpoint (1) !",oWin)  // ��������� ���
   aEvent := {}                                                          // ������� �� ����, ����� �������
   AAdd( aEvent, { 1, {|ow,ky,cn,ob| ob := This.oBrw.Object, my4Btn1(ow,ky,cn,ob)  } } )    // ������ 1
   AAdd( aEvent, { 2, {|ow,ky,cn,ob| ob := This.oBrw.Object, my4Btn1(ow,ky,cn,ob)  } } )    // ������ 2
   AAdd( aEvent, { 3, {|ow,ky,cn,ob| ob := This.oBrw.Object, my4Btn1(ow,ky,cn,ob)  } } )    // ������ 3
   AAdd( aEvent, { 4, {|ow,ky,cn,ob| ob := This.oBrw.Object, my4Btn1(ow,ky,cn,ob)  } } )    // ������ 4
   AAdd( aEvent, {99, {|ow,ky,cn| SetProperty(ow:Name, cn, "Enabled", .T. ), ow:Release(), ky:=cn } } ) // �����

   ? ProcNL(), ALIAS(), Used() ; ?? DBINFO( DBI_FULLPATH ), dbInfo( DBI_CODEPAGE )

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
                      //? "   ...", hb_enumIndex(oc), oc:cName, oc:lEdit, oc:lCheckBox
                  NEXT
                  //?
                  FOR i := 1 TO ob:nLen
                     ob:GotoRec(i)
                     ns += ob:GetValue("TAXRATE")
                  NEXT
                  ob:GoTop()
                  cv := "Amount by field [TAXRATE] = " + HB_NtoS(ns)
                  // "LblDown" - ����� �� oWin:aDown
                  ob:GotoRec(20)
                  cv +=  SPACE(10) + "Jump to record 20  /  oBrw:GotoRec(20) "
                  IF GetControlIndex("LblDown", cw ) > 0
                     SetProperty(cw, "LblDown", "Value", cv)
                  ENDIF
                  Return Nil
                 }

   // ������� ���� �������� � �������
   WaitThreadAviClose( aThrd )

//? "*** TsbObjViewer() *** Start" ; To2Log() ; ?
   // ���� � �������� - ����� ���� ������� �� �������� � oWin
   TsbObjViewer(oWin, oUse, oIndx, oMenu, oTsb, aEvent, bInitForm)
   // ��������� �������� � \MiniGUI\SAMPLES\Advanced\Tsb_Viewer
//? "*** TsbObjViewer() *** Stop" ; To2Log() ; ?

   IF SELECT(cAls) > 0
      (cAls)->(dbCloseArea())  // ������� ����
   ENDIF
   ? ProcNL(), "--- END ------ Alias:", ALIAS()

   IF _IsWindowDefined( cWndMain )
      // ������� ������� ����
      Domethod(cWndMain, "Restore")
   ENDIF
   DO EVENTS

RETURN Nil

///////////////////////////////////////////////////////////////////////////////
FUNCTION my4Btn1(ow,ky,cn,ob)
   LOCAL aRet

   IF ky == 1  // ������ 1
      aRet := myTable4menu(ow,ky,cn)
      IF LEN(aRet) > 0
         MsgDebug(aRet)
      ENDIF
   ELSEIF ky == 2  // ������ 2
       myTable4Card(ow,ky,cn,ob)
   ELSEIF ky == 3  // ������ 3
       myGetLang("����� ���� � ���� ?","SAY")  // TsbViewMenu.prg
   ELSE
      MsgDebug(ow:Name,ky,cn,This.&(cn).caption,ob:cAlias)
   ENDIF

   ow:Enabler(cn, .T.)
   ob:Setfocus()
   DO EVENTS

RETURN Nil

///////////////////////////////////////////////////////////////////////////
/*FUNCTION myFunc0(oWnd, nMsg, oBrw)
   MsgDebug(oWnd:Name,nMsg, oBrw:cAlias)
RETURN NIL*/

///////////////////////////////////////////////////////////////////////////////
FUNCTION CreateDataWin(cTitle, aBClr, nY, nX, nW, nH)
   LOCAL oWin, aColor

   nY := nX := nW := nH := 0  // ����� ���������
   oWin := oHmgData()
   oWin:lWait      := .T. // .T.-"WAIT", .F.="NOWAIT"
   oWin:lCenter    := .F.
   oWin:nPosY      := 0
   oWin:nPosX      := 0
   oWin:nPosW      := App.Cargo:aDisplayMode[1]  //System.ClientWidth
   oWin:nPosH      := App.Cargo:aDisplayMode[2]  //System.ClientHeight
   oWin:nPosH      -= GetTaskBarHeight()         // ������ ������ ����� Desktop
   oWin:aBackcolor := aBClr
   oWin:cTitle     := cTitle
   oWin:lTopmost   := .F.      // This.Topmost := lTopmost, ���� .T. �� ������������� �� ������ ���� ����� ������
   oWin:bOnInit    := Nil      // �� ��������� ������� � TsbViewer.prg
   oWin:bOnRelease := {||Nil}  // �� ��������� ������� � TsbViewer.prg
   oWin:bIAClose   := {||Nil}  // �� ��������� ������� � TsbViewer.prg
   oWin:aDown      := {}       // ��� label ����� ����
   // ���� label ����� ���� {������, ���� ����, ���� ������, ���������, ��� �����}
   oWin:aDown      := { "LblDown", GetTitleHeight(), WHITE, {42,97,181}, .T., "! you can write something here ...." }
   aColor          := oWin:aDown[4]
   oWin:aDown[4]   := IIF( IsWin10OrLater(), Color10_ActiveCaption(), aColor )

RETURN oWin

///////////////////////////////////////////////////////////////////////////////
FUNCTION CreateDateDbf(cPath, cFile, cAls, cCodePage, lShared)
   LOCAL oUse, cMsg, oError, cOld, cUse := cPath + cFile

   oUse := oHmgData()
   oUse:cFullPath := cUse
   oUse:cPath     := cPath
   oUse:cFile     := cFile
   oUse:cCodePage := cCodePage
   oUse:lShared   := lShared
   oUse:cError    := ""

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
         oUse:cAlias := ALIAS()
         oUse:lOpen  := .T.

      RECOVER USING oError

         cMsg := "Error opening Database!;"
         cMsg += "The Database is occupied by another process;"
         cMsg += cUse + ";" + ProcNL()
         //AlertStop( cMsg, "ERROR")
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
FUNCTION CreateDateIndex(oUse,cPathTemp)                    // ������� �������
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
FUNCTION CreateDateMenu(aPost)             // ����-������ ����
   LOCAL oMenu, nWMenu, nKolvo

   oMenu := oHmgData()
   oMenu:lDebug    := .T.       // �������, ����� ������
   oMenu:nPosWin   := 1         // 1-TopWindow, 2-BottomWindow, 3-LeftWindow, 4-RightWindow
   oMenu:nHAlign   := DT_LEFT   // �������������� ������: 0-LEFT, 1-CENTER, 2-RIGHT
   oMenu:nVAlign   := DT_TOP    // ������������ ������: 0-TOP , 1-CENTER, 2-BOTTOM
   oMenu:aCaption  := { "New menu"  , "Card recno" , "����� ����;� ���� ?"  , "Test-4" , "Exit"      }
   oMenu:aBtnPost  := aPost     // _wPost(�) - ����� ������� �� ������
   oMenu:aBColor   := { BLUE        , ORANGE       , GRAY     , GRAY     , {189,30,73} }
   oMenu:lBtnIco   := .T.       // F-������ ��� ������
   oMenu:aIcon     := { {"iDbInfo64x1","iDbInfo64x2"} , {"iDbInfo64x1","iDbInfo64x2"} ,;
                        {"iDbInfo64x1","iDbInfo64x2"} , {"iDbInfo64x1","iDbInfo64x2"} , { "iExit64x1", "iExit64x2" } }
   oMenu:nIcoSize  := 48
   //oMenu:lTextVert := .T. // ������������ ����� ��� ������
   //oMenu:lTextLeft := .F. // ����� ����� ��� ������
   oMenu:aFont     := { "Comic Sans MS", 14, .T., .F. , 16, "���������� ����� ������" }
   oMenu:aFClr     := { BLACK , YELLOW }
   oMenu:aHelp     := {}
   oMenu:nIndent   := 0                  // ������ ������ ������  - ������
   oMenu:nHBtn     := 56                 // ������ ������
   oMenu:nWBtn     := 220                // ������ ������
   oMenu:nGaps     := 5                  // ������ ������ �� ���� ����
   oMenu:nGapsBtn  := 10                 // ����� �������� �� ������/������
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
FUNCTION CreateDateTsb(oUse, c1Title, c2Title, oWin)      // ��������� ���
   LOCAL aHead, aFSize, aFoot, aPict, aAlign, aName, aField, aFAlign, nAlgn
   LOCAL aDbf, nJ, nK, aEdit, cErr, cFld, cVal, cAls, cMsg, oTsb, cTmp, cTyp
   LOCAL aBColor, nBColor, aGradient, nGrad, nClr

   aBColor := oWin:aBackcolor
   nBColor := RGB( aBColor[1], aBColor[2], aBColor[3] )

   oTsb := oHmgData()
   oTsb:nWGaps       := GetBorderWidth()        // ������ �� ������ �����
   oTsb:nHGaps       := GetBorderHeight()       // ������ �� ������ �����
   oTsb:cSupHd1Title := c1Title
   oTsb:cSupHd2Title := c2Title
   oTsb:cError       := ""
   // ��������� �������
   oTsb:lSelector    := .T.         // F-������ � ������� ����.������� SELECTOR
   oTsb:lColNumber   := .T.         // F-������ � ������� ����.������� ORDKEYNO
   oTsb:aColNumber   := { 1, 60 }   // ����.������� � �������� - ����� ��������� ������������� ������ �������
   oTsb:lSuperHead   := .T.         // F-������ � ������� ����������
   oTsb:lSpecHd      := .T.         // F-������ � ������� ���������

   // ����� �������
   nGrad             := RGB(48,29,26)
   oTsb:aBrush       := aBColor                         // ��� ��������
   oTsb:nClrNoDbf    := GetSysColor( COLOR_BTNFACE )    // ���������/����������/����.�������
   aGradient         := { RGB(242,163,167), nGrad }
   oTsb:nClrNoEdit   := aGradient                       // �����/������ ������� ���� "+=^"
   oTsb:nClrBackDel  := RGB(50, 50, 50)                 // ���� �������� �������
   oTsb:nClrForeDel  := CLR_GRAY                        // ����� �������� �������
   oTsb:nClr1Fore    := CLR_BLUE                        // 1 , ����� � ������� �������
   oTsb:nClr2Back    := nBColor       //CLR_WHITE       // 2 , ���   � ������� �������
   oTsb:nClr3Fore    := CLR_YELLOW                      // 3 , ������ ����� �������
   aGradient         := { RGB(40,122,237), nGrad }
   oTsb:nClr4Back    := aGradient                       // 4 , ���� ����� �������
   oTsb:nClr9Fore    := CLR_YELLOW                      // 9 , ������ ������� �������
   oTsb:nClr10Back   := aGradient                       // 10, ���� ������� �������
   aGradient         := { RGB(96,255,255), nGrad }
   oTsb:nClr16Back   := aGradient                       // 16, ���� �����������
   oTsb:nClr17Fore   := CLR_WHITE                       // 17, ������ �����������
   oTsb:n1Clr16Back  := aGradient                       // 16, ���� ����������� ������� 1
   oTsb:n1Clr17Fore  := CLR_RED                         // 17, ������ ����������� ������� 1
   // ----- 07.11.23
   oTsb:n12Clr9Fore  := CLR_YELLOW                      // 9 , ������ ������� ������� ������� 2
   oTsb:n12Clr10Back := { CLR_BLUE, RGB(96,255,255) }   // 10, ���� ������� ������� ������� 2
   oTsb:n12Clr3Fore  := CLR_YELLOW                      // 3 , ������ ����� ������� ������� 2
   oTsb:n12Clr4Back  := { CLR_BLUE, RGB(96,255,255) }   // 4 , ���� ����� ������� ������� 2
   oTsb:nClrSelectorHdBack := oTsb:n12Clr4Back          // ���� ���� �����/������� ������� ������� 1 - Selector
   // ������� 2 �������/������� 1-2
   nClr  := GetSysColor( COLOR_BTNFACE )                // ���������/����������/����.�������
   //oTsb:n12Clr10Back := { nClr, nClr }                // 10, ���� ������� ������� ������� 2
   //oTsb:n12Clr4Back  := { nClr, nClr }                // 4 , ���� ����� ������� ������� 2
   //oTsb:n12Clr9Fore  := CLR_GREEN                     // 9 , ������ ������� ������� ������� 2
   //oTsb:n12Clr3Fore  := CLR_GREEN                     // 3 , ������ ����� ������� ������� 2
   //oTsb:nClrSelectorHdBack := { nClr, nClr }          // ���� ���� �����/������� ������� ������� 1 - Selector

   // ����� �������
   oTsb:nClrFocus1   := -RGB(1,1,1)       // ������ ���������
   oTsb:nClrFocus2   := -CLR_HRED         // ������� ���������
   //oTsb:nClrSeleF  := GetSysColor( COLOR_WINDOWTEXT )   // ���� ������ ������ ������� ��� ������
   oTsb:nClrSeleF    := CLR_YELLOW                        // ���� ������ ������ ������� ��� ������
   oTsb:nClrNoFocus1 := -CLR_GREEN                        // ��������� ��� ������
   oTsb:nClrNoFocus2 := -RGB( 128, 225, 225 )             // ��������� ��� ������
   // ������� � ������
   oTsb:lShowZebra   := .T.               // ����� ������\�������� ������
   oTsb:nClr22Bck    := CLR_WHITE         // ���� ������\�������� row
   // ������ � ��������� ������ - ����� ��� ���-�� ������, � �� ��������
   oTsb:aWidthCol    := { {"ID", -4}, {"LOGPRN", -3}, {"CUSTNO", -5}, {"FAX", +2}, {"TAXRATE", -4} }

   cAls := oUse:cAlias
   cErr := ""
   aDbf := {}                                                 // edit cell
   AADD( aDbf, { "ID"         , "+",  4, 0, "Recno;increment"     , .F. } )
   AADD( aDbf, { "LOGPRN"     , "L",  1, 0, "Print;recno"         , .T. } )
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
   AADD( aDbf, { "LASTINVOIC" , "C", 30, 0, "not-show"            , .T. } )

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
      //oTsb:aEdit  := .F.     // ������ ������ ���� �����, ���������� aEdit

   ENDIF

RETURN oTsb

