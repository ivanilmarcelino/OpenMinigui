/*                                                         
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2024 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * ����� � ������� � ������ �������
 * ������������� ����� ����� � ���� '^+=@T' � ������
 * ���� ���� ��� ��������� ����� ����� � ���� "DTM"
 * Table fonts and column widths
 * Using field types in the database '^+=@T' and others
 * Own windows for processing field types in the "DTM" database
*/
                                     
REQUEST DBFCDX

#define _HMG_OUTLOG
#include "minigui.ch"
#include "TSBrowse.ch"

//////////////////////////////////////////////////////////////////////
PROCEDURE MAIN
   LOCAL oBrw, nY, nX, nW, nH, nG, cAls, aBClr, aBrush, nFSz := 14
   LOCAL aTsbFont, owc

   rddSetDefault( "DBFCDX" )

   SET DELETED   ON
   SET EXCLUSIVE ON
   SET AUTOPEN   ON
   SET DATE      TO GERMAN
   SET EPOCH     TO 2000
   SET CENTURY   ON
   SET EXACT     ON
   SET DECIMALS  TO 4

   SET OOP       ON

   SET NAVIGATION EXTENDED
   SET DEFAULT ICON TO "1MAIN_ICO"
   SET SHOWREDALERT ON        // ��������� ���� ��� ���� "Program Error"

   SET FONT TO 'Arial', nFSz

   //_DefineFont("TsbNorm" , "DejaVu Sans Mono" , nFSz  , .F., .F. )
   _DefineFont("TsbNorm" , "Tahoma"           , nFSz  , .F., .F. )
   _DefineFont("TsbBold" , "Times New Roman"  , nFSz  , .T., .F. )
   _DefineFont("Italic"  , "Tahoma"           , nFSz-2, .F., .T. )
   _DefineFont("ItalBold", "Arial Black"      , nFSz  , .T., .F. )
   _DefineFont("SpecHdr" , "Tahoma"           , nFSz-4, .T., .T. )
   _DefineFont("TsbEdit" , "Arial"            , nFSz  , .F., .T. )
   _DefineFont("DlgFont" , "DejaVu Sans Mono" , nFSz  , .F., .F. )    // ���� ���� Alert*

   nY     := nX := nG := 20
   aBClr  := AQUA
   aBrush := {179,230,251}
   cAls   := myOpenDbf("test.dbf")
   //            cell         head       foot     SpecHider   SuperHider   Edit
   aTsbFont := { "TsbNorm", "TsbBold", "TsbBold", "SpecHdr" , "ItalBold", "TsbEdit" } 

   DEFINE WINDOW wMain                 ;
      TITLE "TBROWSE Edit Fields Demo" ;
      BACKCOLOR aBClr                  ;
      MAIN                             ;
      NOMAXIMIZE NOSIZE                ;
      ON INIT    {|| _LogFile(.T., ProcNL(),">>> Start!") , oBrw:Setfocus() } ;
      ON RELEASE {|| _LogFile(.T., ProcNL(),">>> Exit ! Number of changes: ", oBrw:Cargo:nModify) }

      This.Cargo := oHmgData() ; owc := This.Cargo  // ��� ���� ������� ������ ��� ���������� (������� ������)
      owc:aBColor    := This.BackColor   // ���� ����
      owc:oWnd       := This.Object      // ��������� ���� ������ ����� ����

      nW := This.ClientWidth
      nH := This.ClientHeight

      DEFINE TBROWSE oBrw                          ;
             AT nY, nX ALIAS cAls                  ;
             WIDTH nW-nG*2 HEIGHT nH-nG*2 CELL     ;
             FONT       aTsbFont                   ;
             BRUSH      aBrush                     ;
             COLNUMBER  { 1, 40 }                  ; // ����� ������� ����������� ������� � ����������
             FIXED ADJUST COLEMPTY                 ;
             LOADFIELDS                            ; // �������������� �������� �������� �� ����� �������� ���� ������
             ENUMERATOR                            ; // ��������� �������
             SELECTOR .T.                          ; // ������ ������� - �������� �������
             EDIT GOTFOCUSSELECT                   ;
             ON INIT  {|ob| ob:Cargo := oHmgData() } // ������� ������ ��� ���������� (������� ������)

             myTsbSet( oBrw , aTsbFont )
             myTsbColor( oBrw )
             myTsbSuperHd( oBrw )   
             myTsbFont( oBrw )
             myTsbKeyFX( oBrw )
             myTsbEdit( oBrw )
    
      END TBROWSE ON END {|ob| ob:SetNoHoles(), ob:SetFocus() }
      // ��������� �������� � ���
      myTsbEnd(oBrw)

      oBrw:Cargo:nModify := 0                    // ��������� � �������
      oBrw:Cargo:aFont   := aTsbFont             // �������� �����
      //oBrw:Cargo:ObjWnd:= owc:oWnd             // �� ���� �� ������ Cargo ���    
      oBrw:Cargo:ObjWnd  := This.Object          // ��� ���

      ON KEY F1     ACTION NIL
      ON KEY ESCAPE ACTION iif( oBrw:IsEdit, oBrw:SetFocus(), wMain.Release() )

   END WINDOW

   wMain.Activate

RETURN

///////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbSet( oBrw , aTsbFont)
   LOCAL nHCell := oBrw:nHeightCell + 6  // ������ �����
   LOCAL oCol, cCol, oDlu, nDlu, n
   LOCAL hFont, aFont, cFont, nFSize, cHead, nWCol, cFrmt

   hFont  := GetFontHandle( aTsbFont[1] )
   aFont  := GetFontParam(hFont)
   cFont  := aFont[1]
   nFSize := aFont[2]
   // �������� ������� ������� �� �����
   oDlu := _Font2oDlu( aTsbFont[1] )
   nDlu := oDlu:nSize
   // --- �������� ������� ������� ---
   //?  _HMG_DefaultFontName, _HMG_DefaultFontSize, "nDlu=", nDlu, oTsb:aTsbFont[1]
   //?  "oDlu:H1=",oDlu:H1, oDlu:H1 + 6, oDlu:H(1.25), oDlu:H1 + oDlu:H(0.25)
   IF cFont == "DejaVu Sans Mono"
      // !!! ��� ����� MONO - DejaVu Sans Mono ������ �������
      n := iif( nFSize <= 15, 20, iif( nFSize < 20, 40, 50 ) )
      oDlu:nPixWidth    += n
      oDlu:nPixWidthDT  += n
      oDlu:nPixWidthDT1 += n
      oDlu:nPixWidthDT2 += n
   ENDIF
   //
   nHCell := oDlu:H1 + 6              // ������ ����� � ���
   //                ^^^ - ���������
   nHCell := oDlu:H(1.25)             // ��� ����������, �� ������� ����� ������
   //              ^^^^  - ��������� �� ������� �����

   WITH OBJECT oBrw
      :nColOrder     := 0
      :lNoChangeOrd  := .F.
      :nWheelLines   := 1
      :lNoGrayBar    := .F.
      :lNoLiteBar    := .F.
      :lNoResetPos   := .F.
      :lNoHScroll    := .T.
      :lNoPopUp      := .T.
      :lNoKeyChar    := .F.          // ��� ����� � ������ �� ����, ����
      :nHeightCell   := nHCell       // ������ ����� = ������ ��������
      :nHeightHead   := nHCell * 1.2 // ������ �����
      :nHeightFoot   := nHCell + 4   // ������ �������
      :nHeightSpecHd := 12           // ������ ���������� ENUMERATOR
      :lFooting      := .T.          // ������������ ������
      :lDrawFooters  := .T.          // ��������  �������
      :nFreeze       := 1            // ���������� �������
      :lLockFreeze   := .T.          // �������� ���������� ������� �� ������������ ��������
      :nCellMarginLR := 1            // ������ �� ����� ������ ��� �������� �����, ������ �� ���-�� ��������
      :nMemoHV       := 1            // ����� 1 ������ ����-����
      :GetColumn("ORDKEYNO"):nWidth := nHCell + 10   // ������ ������� 1
   END WITH

   FOR EACH oCol IN oBrw:aColumns
      cCol := oCol:cName
      IF cCol == "SELECTOR" .OR. cCol == "ORDKEYNO"
      ELSE
         oCol:cFooting := '"' + oCol:cFieldTyp + '"'
         oCol:nFAlign  := DT_CENTER
      ENDIF
      // !!! ��� ����� - DejaVu Sans Mono ������ ������� 
      IF oCol:cFieldTyp == "C"
         //oCol:cPicture := Nil
         oCol:nWidth := oCol:ToWidth( iif( oCol:nFieldLen > 50, 50, oCol:nFieldLen ) )
      ELSEIF oCol:cFieldTyp $ "D"
         oCol:nWidth   := oCol:ToWidth(10+2)   // 01.01.2024
         oCol:nAlign   := DT_CENTER
      ELSEIF oCol:cFieldTyp $ "N"
         hFont := oCol:hFont                  // ����� ���� � ������
         cFrmt := REPL("0",oCol:nFieldLen) + "9" + REPL("0",oCol:nFieldDec)
         cFrmt += "99"
         nWCol := GetTextWidth( Nil, cFrmt, hFont )
         oCol:nWidth := nWCol
      ELSEIF oCol:cFieldTyp $ "T=@"
         //oCol:cPicture := "@R 9999-99-99 99:99:99" // 23 �������
         //oCol:bDecode  := {|tval| iif( tval == hb_CToT(""), "", hb_TtoS(tval) ) }   
         //oCol:bDecode  := {|tval| hb_TtoS(tval) }
         //oCol:nAlign   := DT_LEFT
         // ����� ���
         oCol:cPicture := NIL
         oCol:nAlign   := DT_CENTER
         oCol:nWidth   := oCol:ToWidth(26) 
         //^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ - ��� �� ��������, ���� ����� oCol:cPicture, �� � ����������
      ELSEIF oCol:cFieldTyp $ "^"
         oCol:bDecode  := {|tval| hb_NtoS(tval) }
         oCol:nAlign   := DT_CENTER
      ELSEIF oCol:cFieldTyp $ "L"
         oCol:nAlign   := DT_CENTER
      ELSEIF oCol:cFieldTyp $ "D"
         oCol:cPicture := Nil
         oCol:nAlign   := DT_CENTER
         oCol:nWidth   := oCol:ToWidth(10)
      ELSEIF oCol:cFieldTyp $ "MV"
         oCol:cPicture := Nil
         oCol:nWidth   := oCol:ToWidth(40)
      ENDIF 
      // ����������� ������ �������
      hFont := oCol:hFontHead                  // ����� ���� � ������� �����
      cHead := oCol:cHeading + "H"
      nWCol := GetTextWidth( Nil, cHead, hFont )
      IF oCol:nWidth < nWCol
         oCol:nWidth := nWCol
      ENDIF
   NEXT

RETURN Nil

///////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbColor( oBrw )

   WITH OBJECT oBrw
      :nClrLine              := RGB(180,180,180) // COLOR_GRID
      :SetColor( { 11 }, { { || RGB(0,0,0) } } )
      :SetColor( {  2 }, { { || RGB(255,255,240) } } )
      :SetColor( {  5 }, { { || RGB(0,0,0) } } )
      :SetColor( {  6 }, { { |a,b,c| a:=b, iif( c:nCell == b,  -CLR_HRED        , -RGB(128,225,225) ) } } )
      :SetColor( { 12 }, { { |a,b,c| a:=b, iif( c:nCell == b,  -RGB(128,225,225), -RGB(128,225,225) ) } } )
   END WITH

RETURN Nil

//////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbSuperHd( oBrw )
   LOCAL hFont, nHFont, aFont, cTitle

   aFont  := GetFontParam(oBrw:hFont) 
   cTitle := "Cell font: " + aFont[1] + " " + HB_NtoS(aFont[2])
   cTitle += SPACE(10)
   cTitle += "Editing different types of fields in TBROWSE  (F2-test)"
   hFont  := oBrw:hFontSupHdGet(1)
   nHFont := GetTextHeight( 0, "B", hFont )

   WITH OBJECT oBrw
      // ������ ���������� � ������� �������� 0
      :AddSuperHead( 1, :nColCount(), "Super_Header_Table" ) 
      :aSuperhead[ 1, 3 ] := cTitle
      :nHeightSuper := nHFont * 2      // 2 ������
      // ������ ����� �����������
      :SetColor( {16}, { { RGB(40,110,212),RGB(0,176,240) }  } ) // 16, ���� 
      :SetColor( {17}, { CLR_WHITE                           } ) // 17, ������ 
   END WIDTH

RETURN NIL

///////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbFont( oBrw )
   LOCAL hFont, oCol

   hFont := oBrw:aColumns[1]:hFontSpcHd    // 4-special header font
   // ���������� ���� ��� 1 ������� �������
   oCol := oBrw:GetColumn("ORDKEYNO")
   oCol:nAlign    := DT_CENTER
   oCol:hFont     := hFont     // 1-cells font
   oCol:hFontFoot := hFont     // 3-footer font

RETURN Nil

//////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbKeyFX( oBrw )    // ��������� ������

   WITH OBJECT oBrw

      // ��������� �����
      :bLDblClick := {|p1,p2,p3,ob| p1:=p2:=p3, ob:PostMsg( WM_KEYDOWN, VK_RETURN, 0 ) }
      :SetAppendMode( .F. )            // ��������� ������� ������ � ����� ���� �������� ����
      :SetDeleteMode( .F. )

      // ��������� ������� ESC � ������
      //:UserKeys(VK_ESCAPE, {|ob| _wSend(99, ob:cParentWnd), .F. })
      //:UserKeys(VK_INSERT, {|ob| RecnoInsert(ob), .F. })
      //:UserKeys(VK_DELETE, {|ob| RecnoDelete(ob), .F. })

      // ������� FXX
      :UserKeys(VK_F2    , {|ob| myTsb_Test( ob ), ob:Setfocus() }) 

   END WITH

RETURN Nil

////////////////////////////////////////////////////////////////////////////
// ��������� ��������������, �������������� �������
STATIC FUNCTION myTsbEdit( oBrw )
   LOCAL oCol, cCol, nI

   ? ProcNL()
   FOR EACH oCol IN oBrw:aColumns
      nI   := hb_EnumIndex(oCol)
      cCol := oCol:cName
      ? "    .",nI, cCol
      IF cCol == "SELECTOR" .OR. cCol == "ORDKEYNO"  ; LOOP
      ENDIF
      oCol:bPrevEdit := {|val, brw| myTsbEditPrev( val, brw ) }  // -> ��.����
      oCol:bPostEdit := {|val, brw| myTsbEditPost( val, brw ) }  // -> ��.����
      oCol:lEdit := .T.
      IF oCol:cFieldTyp $ "+^="  // ��� ���� �� �������������
         oCol:lEdit := .F.
         oCol:nClrFootFore := CLR_WHITE
         oCol:nClrFootBack := CLR_RED
      ENDIF
      ?? oCol:lEdit, oCol:cFieldTyp, oCol:nFieldLen, oCol:nFieldDec, oCol:cHeading
   NEXT

RETURN NIL

////////////////////////////////////////////////////////////////////////////
// ����-���� ������� ������ ����� END TBROWSE
STATIC FUNCTION myTsbEnd( oBrw )
   LOCAL nBCSpH, oCol, nCol, nLen, hFont, nWCol
                                   
   nBCSpH := GetSysColor( COLOR_BTNFACE )   // ���� ���� ���������� �������

   // ������ ������ ���������� ������� �� ���� ����, ����� SELECTOR
   oBrw:lClrSelectorHdBack := .T. // background OFF
   FOR EACH oCol IN oBrw:aColumns
      oCol:nClrSpcHdFore := CLR_RED
      oCol:nClrSpcHdBack := nBCSpH
   NEXT

   // ��������� ����������� �������
   nLen := LEN(HB_NtoS(oBrw:nLen))
   nCol := oBrw:nColumn("ORDKEYNO", .T.)
   IF nCol > 0
      oCol := oBrw:GetColumn("ORDKEYNO")
      oCol:nClrBack      := nBCSpH
      oCol:nClrFore      := CLR_RED
      //oCol:nClrFootBack  := nBCSpH
      oCol:nClrFootFore  := CLR_RED
      oCol:SaveColor()                                           // ��������� ����� �������
      hFont := oBrw:aColumns[nCol]:hFont                         // ����� ���� � �������
      nWCol := GetTextWidth( Nil, REPL("0", nLen + 2), hFont )   // ���-�� ������ + 2 �����
      oCol:nWidth := nWCol                                       // ����� ������
   ENDIF

   // ��������� ������ ���� ���� "+"
   nLen := LEN(HB_NtoS(oBrw:nLen))
   FOR EACH oCol IN oBrw:aColumns
      nCol := hb_EnumIndex(oCol)
      IF oCol:cFieldTyp == "+"
         hFont := oBrw:aColumns[nCol]:hFont                         // ����� ���� � �������
         nWCol := GetTextWidth( Nil, REPL("0", nLen + 4), hFont )   // ���-�� ������ + 2 ����� + 2 ���� ������
         oCol:nWidth := nWCol                                       // ����� ������
      ENDIF
   NEXT

   IF oBrw:lDrawSuperHd  // ���-2
      // ��������� ���������� �� ����� �������
      ATail(oBrw:aSuperHead)[2] := oBrw:nColCount()
   ENDIF

   oBrw:DrawHeaders()   // ���������� ����������/�����/���������
   oBrw:Refresh()
   oBrw:SetFocus()
   DO EVENTS

RETURN NIL

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbEditPrev( uVal, oBrw )
   LOCAL nCol, oCol, cNam, cAls, uOld, lRet
   LOCAL cTyp, cMsg, xRet, lWrt, cStr, aRet

   WITH OBJECT oBrw
      nCol  := :nCell
      oCol  := :aColumns[ nCol ]
      cAls  := :cAlias
      cTyp  := oCol:cFieldTyp        // ��� ��������� �������
      cNam  := oCol:cName
   END WITH

   uOld := uVal
   ? ProcNL(), nCol, cTyp 
   cStr := 'oCol:bPrevEdit !;'
   cStr += 'oCol:cName = ' + cNam + ";"
   cStr += 'Column field type: "' + cTyp + '" ;'
   cStr += 'NO processing for this field!;'
   lWrt := .T.     // ���������� ����
   aRet := {uVal}       // �������� � ������ - �� ��� ������

   IF cTyp $ "NLI^"
      oCol:nClrEditFore := CLR_YELLOW
      oCol:nClrEditBack := CLR_BLACK
   ELSEIF cTyp $ "CMV"
      oCol:nClrEditFore := CLR_BLUE
      oCol:nClrEditBack := CLR_HGRAY
      // ������ ��� ����� �������
      IF AT(CRLF,uVal) > 0           // ���� � ���� "C" ���� CRLF
         aRet := CellEditMemo(uVal, oBrw)
         lRet := .F.                 // �� ������ ������������� ���� � :get
      ELSEIF cTyp == "M" .OR. cTyp == "V"
         aRet := CellEditMemo(uVal, oBrw) 
         lRet := .F.                 // �� ������ ������������� ���� � :get
      ENDIF
   ELSEIF cTyp $ "=@T" .OR. cTyp $ "D"   
      aRet := CellEdit_DT(oBrw, cTyp, uVal) 
      lRet := .F.             // �� ������ ������������� ���� � :get
   ELSE
      oCol:nClrEditFore := CLR_YELLOW
      oCol:nClrEditBack := CLR_RED
      ? ProcNL(), "uVal=", uVal, HB_ValToExp(uVal)
      //cMsg := ProcNL(0) + ";" + ProcNL(1) + ";;"
      //AlertInfo(cMsg + cStr)
      //lWrt := .F.             // �� ���������� � ������
      //lRet := .F.             // �� ������ ������������� ���� � :get
   ENDIF

   //? ProcNL(), "#######-0", "lWrt=", lWrt, aRet, HB_ValToExp(aRet)
   IF lWrt                                         // ���������� ������
      IF (oBrw:cAlias)->(RLock())                  // ������ ������
         // !!! ������ ������ - ���� ������, �� ��� ����� �� �����
         IF LEN(aRet) > 0                          
            ? ProcNL(), "#######-?", aRet, HB_ValToExp(aRet)
            oBrw:Cargo:nModify ++                  // �������-��������� � �������
            xRet := aRet[1]                              
            oBrw:SetValue(nCol,xRet)
            //(oBrw:cAlias)->KOPERAT  := 555       // ��� ������ ������
            //(oBrw:cAlias)->DATEVVOD := DATE()    // ���� ������
            //(oBrw:cAlias)->TIMEVVOD := 9999      // ����� ������
            (oBrw:cAlias)->( DbUnlock() )
            (oBrw:cAlias)->( DbCommit() )
         ENDIF
      ELSE
         cMsg := "Recording is locked !; Recno="
         cMsg += HB_NtoS(RECNO()) + ";;" + ProcNL()
         AlertStop( cMsg )
      ENDIF
   ENDIF
   oBrw:DrawSelect()    // ������������ ������� ������ �������
   oBrw:SetFocus()

   DO EVENTS

RETURN lRet

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbEditPost( uVal, oBrw )
   LOCAL nCol, oCol, cNam, uOld, cAls, lMod
   LOCAL oWnd := _WindowObj(oBrw:cParentWnd)
   LOCAL cTyp, cMsg, cStr

   WITH OBJECT oBrw
      nCol := :nCell
      oCol := :aColumns[ nCol ]
      cNam := oCol:cName
      cTyp := oCol:cFieldTyp        // ��� ��������� �������
      uOld := oCol:xOldEditValue    // old value
      lMod := ! uVal == uOld        // .T. - modify value
      cAls := :cAlias
   END WITH

   ? ProcNL(), nCol, cTyp 
   cStr := 'oCol:bPrevEdit !;'
   cStr += 'oCol:cName = ' + cNam
   cStr += ';Column processing type: "' + cTyp + '" ;'

   IF cTyp $ "CNDL"
      // ����������� ���������
   ELSE
      cMsg := ProcNL(0) + ";" + ProcNL(1) + ";;"
      AlertInfo(cMsg + cStr)
      RETURN .F.
   ENDIF
   oBrw:DrawSelect()    // ������������ ������� ������ �������
   oBrw:SetFocus()

   DO EVENTS

RETURN .T.

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION CellEdit_DT(oBrw,cType,xGet)
   LOCAL oCell  := oBrw:GetCellInfo(oBrw:nRowPos) 
   LOCAL nY     := oCell:nRow + oBrw:nHeightHead
   LOCAL nX     := oCell:nCol 
   LOCAL nWCell := oCell:nWidth - 2
   LOCAL nHCell := oCell:nHeight - 2
   LOCAL oWnd, hWnd, oJWnd, aRet, cForm, nWBtn, nHObj, nHIco, aTime, cVal
   LOCAL cFont, nFSize, aFont, cText, nWDate, dDate1, tDTime, nW, nH
                        
   ? ProcNL(), "cType=", cType, "xGet=", xGet, "VALTYPE=", VALTYPE(xGet) 

   oJWnd := oBrw:Cargo:ObjWnd        // ������� ����    
   cForm := oJWnd:Name 
   nY    += oJWnd:Row 
   nX    += oJWnd:Col + 3
   IF oBrw:lDrawSpecHd  
      nY -= oBrw:nHeightSpecHd    // ������ ���������� ENUMERATOR
   ENDIF

   nY     += IIF( Sys.ClientWidth  <= 720, 4, -3)
   nHCell += IIF( Sys.ClientHeight <= 720, 3, 0 )
   
   aFont  := GetFontParam(oBrw:hFont) 
   cFont  := aFont[1]
   nFSize := aFont[2]

   nHObj  := nHCell - 7 //nFSize * 2
   nHIco  := nHObj - 2
   cText  := "120DECEMBER020240"
   nWDate := GetTxtWidth( cText, nFSize, cFont, .T. ) + 65
   IF cType $ "@T"
      cText  := REPL("0",24) + '0|0'
      nWDate := GetTxtWidth( cText, nFSize, cFont, .T. ) + 55
   ENDIF
   nWBtn  := nHCell + nHCell + 4       // ��� ������
   nW     := nWDate + nWBtn
   aRet   := {}   // ������ ������ - �����, ������ ����� �� �����

   // ����� �� ������� ������/��������� � ������� ����� ������
   IF nX + nW > Sys.ClientWidth
      nX := (nWCell + nX) - nW
   ENDIF
   nH := nHCell

   // ����� ���� � ������ �������
   DEFINE WINDOW Cell AT nY,nX WIDTH nW HEIGHT nH  ;
      MODAL NOCAPTION                              ; 
      FONT cFont SIZE nFSize                       ;
      ON LOSTFOCUS {|| oWnd:Release() }            ;
      ON INIT      {|| DoEvents() }

      oWnd := ThisWindow.Object 
      hWnd := oWnd:Handle 

      IF cType == "D"

         dDate1 := xGet
         IF dDate1 == CTOD('')
            dDate1 := DATE()
         ENDIF

         @ 3, 3 DATEPICKER Date_1 VALUE dDate1 WIDTH nWDate HEIGHT nHObj ;
            DATEFORMAT "dd'.'MMMM' 'yyyy" SHOWNONE 
         nX := This.Date_1.Width + 5

         @ 3, nX BUTTONEX Btn_Ok WIDTH nHObj HEIGHT nHObj CAPTION '' ;
           ICON "iOk32" NOHOTLIGHT NOXPSTYLE HANDCURSOR ;
           ACTION {|| aRet := { This.Date_1.Value } , oWnd:Release() }

         This.Btn_Ok.ImageWidth  := nHIco
         This.Btn_Ok.ImageHeight := nHIco
         This.Btn_Ok.Icon        := LoadIconByName( "iOk32", nHIco, nHIco )

         nX += This.Btn_Ok.Width + 5

         @ 3, nX BUTTONEX Btn_Esc WIDTH nHObj HEIGHT nHObj CAPTION '' ;
           ICON "iCancel32" NOHOTLIGHT NOXPSTYLE HANDCURSOR ;
           ACTION {||  aRet := {} , oWnd:Release() }

         This.Btn_Esc.ImageWidth  := nHIco
         This.Btn_Esc.ImageHeight := nHIco
         This.Btn_Esc.Icon        := LoadIconByName( "iCancel32", nHIco, nHIco )

      ELSEIF cType $ "@T"

         tDTime := xGet
         IF tDTime == hb_CToT("")
            tDTime := hb_DateTime() 
         ENDIF
         dDate1   := hb_TToD(tDTime)
         aTime    := {0,0,0}
         cVal     := hb_TtoS(tDTime)   // 2003 12 20 191944859 
         aTime[1] := VAL(SUBSTR(cVal,9,2))
         aTime[2] := VAL(SUBSTR(cVal,11,2))
         aTime[3] := VAL(SUBSTR(cVal,13,2))

         @ 3, 3 DATEPICKER Date_2 VALUE dDate1 WIDTH nWDate-3 HEIGHT nHObj ;  
           SHOWNONE UPDOWN DATEFORMAT "dd MMMM yyyy' | 'HH:mm:ss" 

         This.Date_2.VALUE := { Year( dDate1 ), Month( dDate1 ), Day( dDate1 ), aTime[1], aTime[2], aTime[3] }
         nX := This.Date_2.Width + 5

         @ 3, nX BUTTONEX Btn_Ok WIDTH nHObj HEIGHT nHObj CAPTION '' ;
           ICON "iOk32" NOHOTLIGHT NOXPSTYLE HANDCURSOR ;
           ACTION {|| tDTime := This.Date_2.Value  ,;
                      aRet   := { tDTime } , oWnd:Release() }

         This.Btn_Ok.ImageWidth  := nHIco
         This.Btn_Ok.ImageHeight := nHIco
         This.Btn_Ok.Icon        := LoadIconByName( "iOk32", nHIco, nHIco )

         nX += This.Btn_Ok.Width + 5

         @ 3, nX BUTTONEX Btn_Esc WIDTH nHObj HEIGHT nHObj CAPTION '' ;
           ICON "iCancel32" NOHOTLIGHT NOXPSTYLE HANDCURSOR ;
           ACTION {|| aRet := {} , oWnd:Release() }

         This.Btn_Esc.ImageWidth  := nHIco
         This.Btn_Esc.ImageHeight := nHIco
         This.Btn_Esc.Icon        := LoadIconByName( "iCancel32", nHIco, nHIco )

      ENDIF

       DRAW LINE IN WINDOW Cell AT 2, 2 TO 2, nW PENCOLOR RED PENWIDTH 4
       DRAW LINE IN WINDOW Cell AT nH-2, 2 TO nH-2, nW PENCOLOR RED PENWIDTH 4
       DRAW LINE IN WINDOW Cell AT 2, 2 TO nH, 2 PENCOLOR RED PENWIDTH 4
       DRAW LINE IN WINDOW Cell AT 2, nW-2 TO nH, nW-2 PENCOLOR RED PENWIDTH 4
 
   END WINDOW 
 
   SetWindowLong(hWnd, GWL_STYLE, WS_BORDER) 
 
   _DefineHotKey ( "CELL" , 0 , VK_ESCAPE , {|| oWnd:Release() } ) 
   _DefineHotKey ( "CELL" , 0 , VK_RETURN , {|| oWnd:Release() } ) 
   Cell.Activate 

RETURN aRet // ������ ������, ���� ����� - ������ ����� �� �����

////////////////////////////////////////////////////////////////////////////
FUNCTION myTsb_Test(oBrw)
   MsgDebug( oBrw:cAlias, "Test something!")
RETURN NIL
