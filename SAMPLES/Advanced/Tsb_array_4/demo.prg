/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2023 Verchenko Andrey <verchenkoag@gmail.com>
 * Copyright 2023 Sergej Kiselev <bilance@bilance.lv>
 *
 * ������� �� �������. ������ ������ � ��������: ��������/�������/�����
 * ���� ����������� ������� ����� - ���������
 * ������ � ������ � ���� JSON
 * Table by array. Recording methods: deletion/insertion/duplicate
 * Own virtual column on the left - numerator
 * Writing and reading to a JSON file
*/
#define _HMG_OUTLOG

REQUEST HB_CODEPAGE_RU1251, HB_CODEPAGE_RU866
REQUEST DBFCDX, DBFFPT

#include "minigui.ch"
#include "TSBrowse.ch"

STATIC lStatAccess := .T.
///////////////////////////////////////////////////////////////////////////
PROCEDURE MAIN ()
   LOCAL aDatos, aArray, aHead, aSize, aFoot, aPict, aAlign, aName, aField
   LOCAL nG, nY, nX, nW, nH, nX2, tTime, cLog  := "_msg.log"
   LOCAL cFont := "Arial", nSize := 14
   LOCAL oBrw, nCol, aVal, aYX

   SET LANGUAGE TO RUSSIAN
   SET CODEPAGE TO RUSSIAN

   RddSetDefault('DBFCDX')

   SET DECIMALS TO 4
   SET DATE TO GERMAN
   SET EPOCH    TO 2000
   SET CENTURY  ON
   SET EXACT    ON
   SET OOP ON

   SET FONT TO cFont, nSize
   SET DEFAULT ICON TO "1MAIN_ICO"

   DEFINE FONT Norm FONTNAME cFont SIZE nSize - 1
   DEFINE FONT Bold FONTNAME cFont SIZE nSize - 1 BOLD
   DEFINE FONT Ital FONTNAME cFont SIZE nSize - 3 ITALIC
   DEFINE FONT DlgFont FONTNAME "DejaVu Sans Mono" SIZE 14  // for HMG_Alert()

   _SetGetLogFile( cLog ) ; DeleteFile( cLog )

   tTime := HB_DATETIME()
   ? REPL("=",20) + " Program start - " + HB_TTOC( HB_DATETIME() ) + " " + REPL("=",20)
   ? MiniGuiVersion()  ;  ?

   WITH OBJECT ( App.Cargo := oHmgData() )
      :cPRG_NAME := "SetArray - delete/insert/duplicate records !"
      :cFileJson := ChangeFileExt( App.ExeName, ".json" )
      :cPathTemp := GetUserTempFolder() + "\"
      :nVersion  := 1.0                        // ������ �������
      :cVerDate  := HB_TTOC( HB_DATETIME() )   // ���� ������ �������
      :cTsbName  := "Table version: "
   END WITH

   DEFINE WINDOW test                                      ;
      TITLE (App.Cargo):cPRG_NAME                          ;
      MAIN NOMAXIMIZE NOSIZE                               ;
      BACKCOLOR { 195, 224, 133 }                          ;
      ON INIT    {|| DoEvents() , _wPost(0)             }  ;   // ����������� ����� ������������� ����
      ON RELEASE {|| _wSend(92), DoEvents(), _wSend(91) }  ;   // ����������� ����� ����������� ����
      ON INTERACTIVECLOSE {|| NIL }                            // �������� ���� �� [x]

      This.Cargo := oHmgData()          // ��� ���� ������� ������ ��� ���������� (������� ������)
      This.Cargo:aWinBtn := Nil         // ������ ����

      DEFINE STATUSBAR
         STATUSITEM "+"                 FONTCOLOR RED
         STATUSITEM "Item 1" WIDTH 230  FONTCOLOR BLUE
         STATUSITEM "Item 2" WIDTH 230  FONTCOLOR BLUE
         STATUSITEM "Item 3" WIDTH 230  FONTCOLOR BLUE
      END STATUSBAR

      nG  := 5
      nY  := 1 + iif( IsVistaOrLater(), GetBorderWidth ()/2, 0 )
      nX  := 1 + iif( IsVistaOrLater(), GetBorderHeight()/2, 0 )
      aYX := myTopMenu( nY, nX, nG )
      nY  := aYX[1]
      nX2 := aYX[2] + 20
      nW  := This.WIDTH  - 2 * GetBorderWidth()
      nH  := This.HEIGHT - 2 * GetBorderHeight() -  ;
             GetTitleHeight() - This.StatusBar.Height - nY
      // ���� � ������ �������
      @    0, nX2 LABEL Label_0 WIDTH nW-nX2 HEIGHT nY/2 VALUE "" TRANSPARENT VCENTERALIGN
      @ nY/2, nX2 LABEL Label_1 WIDTH nW-nX2 HEIGHT nY/2 VALUE "" TRANSPARENT VCENTERALIGN
      // �������� ����� �������� ���� �������� �� json - ������ � �������� �������
      This.Cargo:aLabel := {"Label_0", "Label_1"}

      aDatos  := CreateDatos()
      aArray  := aDatos[ 1 ]
      aHead   := aDatos[ 2 ]
      aSize   := aDatos[ 3 ]
      aFoot   := aDatos[ 4 ]
      aPict   := aDatos[ 5 ]
      aAlign  := aDatos[ 6 ]
      aName   := aDatos[ 7 ]
      aField  := aDatos[ 8 ]
      // aFoot := .F.

      This.Cargo:aNewRec := aDatos[ 9]

      // ���� ��� ������������ ����� (������ �������)
      aVal := aArray[1]
      This.Cargo:aItog := array(Len(aVal))
      FOR nCol := 1 TO Len(aVal)
         IF hb_IsNumeric(aVal[nCol])
            This.Cargo:aItog[nCol] := 0
         ENDIF
      NEXT

      DEFINE TBROWSE oBrw ;
             AT nY, nX ALIAS aArray WIDTH nW HEIGHT nH CELL ;
             FONT { "Norm", "Bold", "Bold", "Ital" }        ;
             BRUSH    This.Backcolor                        ;
             HEADERS  aHead                                 ;
             COLSIZES aSize                                 ;
             PICTURE  aPict                                 ;
             JUSTIFY  aAlign                                ;
             COLUMNS  aField                                ;
             COLNAMES aName                                 ;
             FOOTERS  aFoot                                 ;
             FIXED    COLSEMPTY                             ;
             LOADFIELDS                                     ;
             ENUMERATOR EDIT GOTFOCUSSELECT
             //COLNUMBER  { 1, 20 } - ������ ������������ � ������� � �����������, �.�. �� ��������

             myTsbSet  ( oBrw )
             myTsbColor( oBrw )
             myTsbEdit ( oBrw )    // ��������� ��������������
             // ��������� ������� ������� ����� END TBROWSE
             myTsb_Before( oBrw )

      END TBROWSE ON END {|ob| ob:SetNoHoles(), ob:SetFocus() }

      WITH OBJECT This.Object
         //:Event(  0, {|  | DoEvents() , MG_Info("������� 0", "��������", YELLOW) } )
         :Event(  0, {|ow,ky,ob   | ob := This.oBrw.Object, ArrayJsonLoad(ow,ky,ob)              } )
         :Event( 10, {|ow,ky,cn,ob| ob := This.oBrw.Object, myRunBtn(ow,ky,cn,ob), ob:Setfocus() } )
         :Event( 11, {|ow,ky,cn,ob| ob := This.oBrw.Object, myRunBtn(ow,ky,cn,ob), ob:Setfocus() } )
         :Event( 12, {|ow,ky,cn,ob| ob := This.oBrw.Object, myRunBtn(ow,ky,cn,ob), ob:Setfocus() } )
         :Event( 13, {|ow,ky,cn,ob| ob := This.oBrw.Object, myRunBtn(ow,ky,cn,ob), ob:Setfocus() } )
         :Event( 14, {|ow,ky,cn,ob| ob := This.oBrw.Object, myRunBtn(ow,ky,cn,ob), ob:Setfocus() } )
         // ��� SortSum_Refresh(...)
         :Event( 67, {|ow,ob,nc| Sort_Table(ow,ob,nc) } )
         :Event( 68, {|ow      | Itog_Table(ow)       } )
         :Event( 69, {|ow,ob   | Draw_Table(ow,ob)    } )
         :Event( 70, {|ow,ky,ob|
                       Local oCol, lEdit
                       lEdit := ob:aArray[1][1] > 0
                       FOR EACH oCol IN ob:aColumns
                           IF hb_EnumIndex(oCol) > 0
                              oCol:lEdit := lEdit
                           ENDIF
                       NEXT
                       ky := ow
                       Return Nil
                     } )
         // ������ �� ������� - ��������������
         :Event( 80, {|ow,ky,cn,ob| ob := This.oBrw.Object, Access_Table(ow,ky,ob), ob:Setfocus(), ;
                                    SetProperty(ow:Name, cn, "Enabled", .T.) } )
         :Event( 90, {|ow,ky,cn,ob| ob := This.oBrw.Object, MsgAbout(ow, ky, ob), ob:Setfocus(), ;
                                    SetProperty(ow:Name, cn, "Enabled", .T.) } )
         :Event( 91, {|        | _LogFile(.T., ">>> STOP <<<  "+HMG_TimeMS(tTime)), _LogFile() } )
         :Event( 92, {|ow,ky,ob| ob := This.oBrw.Object, ArrayJsonSave(ow,ky,ob) } )
         :Event( 99, {|ow| ow:Release() } )
      END WITH

      // ������� ����� END TBROWSE � ��������� �������
      myTsb_After(oBrw)

   END WINDOW

   DoMethod( "test", "Activate" )

RETURN

/////////////////////////////////////////////////////////////////////////////////////
// ��������� ��������������
STATIC FUNCTION myTsbEdit( oBrw )
   LOCAL i, oCol, cMsg

   oBrw:SetAppendMode( .F. )     // ��������� ������� ������ � ����� ���� �������� ����
   oBrw:SetDeleteMode( .T., .F. )
   //oBrw:SetDeleteMode( .T., .T. ) - ������ �� ��������

   FOR EACH oCol IN oBrw:aColumns
       i := hb_EnumIndex(oCol)
       IF i == 1
          oCol:lEdit := .F.
       ELSE
          oCol:Cargo := oHmgData()
          oCol:lEdit := .T.
          //oCol:nEditMove := 0  // ����. ����������� ������� ����� :Edit()
          IF oCol:cName == "NAME_3"
             oCol:bValid := {|xv,ob,lErr| lErr := Msg_Name_3_Year( xv, ob:Cargo:nCurrentYear), !lErr  }
          ENDIF
          // �� ������ ������� ����� lStatAccess ��� ���������� ������� �������
          oCol:Cargo:lStatAccess := .T.
          oCol:bPrevEdit := {|xv,ob|
                              Local nc, oc, dVal
                              IF ob:aArray[1][1] == 0
                                 cMsg := '������� ������, ��� ������� ! �������� ����� ������ !;'
                                 cMsg += 'The table is empty, there are no records! Insert a new entry !;'
                                 SET WINDOW THIS TO ob
                                 MG_Stop(cMsg)
                                 SET WINDOW THIS TO
                                 Return .F.   // ��� �������, ��� ������
                              ENDIF
                              nc := ob:nCell
                              oc := ob:GetColumn( nc )
                              xv := oc:Cargo
                              IF ! xv:lStatAccess  // �������� �������
                                 cMsg := '������ ������� ��� ������� ! ���������� � ������ !;'
                                 cMsg += 'Forbid editing this column! Contact admin !;'
                                 SET WINDOW THIS TO ob
                                 MG_Stop(cMsg)
                                 SET WINDOW THIS TO
                              ELSE
                                 // �������������� ��������� ������� ���� ����
                                 IF oc:cName == "NAME_3"
                                    ? ProcNL(), oc:cName, nc
                                    dVal := oBrw:GetValue(nc)
                                    ?? "������ :bPrevEdit", dVal
                                 ENDIF
                              ENDIF
                              Return xv:lStatAccess
                             }
          oCol:bPostEdit := {|xv,ob|
                              Local nc, oc, lModify
                              Local nCurYear := ob:Cargo:nCurrentYear
                              Local nName_3  := ob:Cargo:nName_3
                              nc := ob:nCell
                              oc := ob:GetColumn(nc)
                              IF ( lModify := oc:xOldEditValue != xv )   // modify value
                                 ? ProcNL(), oc:cName, nc
                                 //������ � ������-��������-�������������-���������
                                 //write to the program-user-actions-log
                              ELSE
                                 ? ProcNL(), oc:cName, nc
                                 ?? "������ :bPostEdit - ��� ��������������"
                                 Msg_Name_3_Year( ob:GetValue(nName_3), nCurYear )
                              ENDIF
                              IF lModify             // modify value (sort, sum, refresh)
                                 SortSum_Refresh(ob, Valtype(xv) == "N")
                              ENDIF
                              Return Nil
                             }
       ENDIF
   NEXT

RETURN NIL

///////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Msg_Name_3_Year( dDate, nYear )
   Local cMsg, lMsg := .F.

   IF hb_IsDate(dDate) .and. ( lMsg := Year(dDate) # nYear )
      cMsg := PADC("� � � � � � � �  /  ATTENTION !",50) + ";;"
      cMsg += '"���� �������" ������ ���� ����� "���� �����" !;'
      cMsg += '"Incoming Date" must be equal to "Input Year" !;'
      MG_Stop( cMsg, "�������������� ���� / Date editing" )
   ENDIF

RETURN lMsg          // .T. - error

///////////////////////////////////////////////////////////////////////////
FUNCTION myTsbSet( oBrw )

   WITH OBJECT oBrw
    :Cargo := oHmgData()
    :Cargo:aAddRec := aClone( (ThisWindow.Cargo):aNewRec )  // ��� ����� �������
    :Cargo:nCurrentYear := Year(Date())                     // ������� ���� ����
    :Cargo:nModify := 0

    :lNoChangeOrd  := .T.      // ��������� ����������
    :nColOrder     := 0        // ������ ������ ���������� �� �������
    :nWheelLines   := 1
    :lNoGrayBar    := .F.
    :lNoLiteBar    := .F.
    :lNoResetPos   := .F.
    :lNoHScroll    := .T.
    :lNoPopUp      := .T.
    :nCellMarginLR := 1
    :lMoveCols     := .F.
    :lNoMoveCols   := .T.     // .T. - ������ ������ �������� ������ ��� ���������� �������
    :lNoKeyChar    := .F.     // .T. - ����. ����� KeyChar(...) - ���� �� ����, ����

    :nHeightCell   += 9       // ������� �������� � ������ �����
    :nHeightHead   += 18      // ������ �����
    :nHeightFoot   += 6       // ������ �������
    :nHeightSpecHd := 24      // ������ ���������� ENUMERATOR

   END WITH

RETURN Nil

///////////////////////////////////////////////////////////////////////////
FUNCTION myTsb_Before( oBrw )
   LOCAL nLen, cBrw, nTsb

   WITH OBJECT oBrw
      :Cargo:nName_3 := :nColumn("NAME_3")    // ��������� ����� �������
      // ��������� ������� ESC � ������
      :UserKeys(VK_ESCAPE, {|ob| _wSend(99, ob:cParentWnd), .F.                        })
      :UserKeys(VK_INSERT, {|ob| DoEvents(), _wPost(10, ob:cParentWnd, "BTN_INS"), .F. })
      :UserKeys(VK_DELETE, {|ob| DoEvents(), _wPost(13, ob:cParentWnd, "Btn_03" ), .F. })

      :nFreeze := :nColumn("NN")  // ���������� ������ 1 ������� - ��� ����������� �������
      :lLockFreeze := .T.         // �������� ���������� ������� �� ������������ ��������
      :nCell := :nFreeze + 1

      cBrw := :cControlName
      nTsb := This.&(cBrw).ClientWidth
      nLen := :GetAllColsWidth() - 1
      IF nLen > nTsb
         :lAdjColumn  := .T.
         :lNoHScroll  := .F.
         :lMoreFields := ( :nColCount() > 45 )
      ELSE
         :AdjColumns()
      ENDIF
   END WITH

RETURN Nil

///////////////////////////////////////////////////////////////////////////
FUNCTION myTsb_After( oBrw )
   LOCAL oCol, nCol

   WITH OBJECT oBrw
      oCol := oBrw:GetColumn("NN")
      oCol:bDecode := {|xv,ob| iif( Empty(xv), "", hb_ntos(ob:nAtPos) ) }
      oCol:nAlign  := DT_CENTER
      oCol:lIndexCol := .F.

      // ����� ��� ���������� ������� ���������� !!!
      nCol := :nColumn("NAME_3")
      :lNoChangeOrd  := .F.      // �������� ����������
      :nColOrder := nCol         // ��������� ������ ���������� �� �������
      :SetOrder(nCol)
      :GoPos(1,nCol)             // ����������� ������ �� X ������ � X �������
   END WITH

   SortSum_Refresh(oBrw)

RETURN Nil

///////////////////////////////////////////////////////////////////////////
FUNCTION myTsbColor( oBrw, oWnd )
   LOCAL nCol, oCol, xVal, aItg, O
   DEFAULT oWnd := ThisWindow.Object

   WITH OBJECT oBrw:Cargo
      // ������ �������� ����������
      :nBtnText   :=  GetSysColor( COLOR_BTNTEXT ) // nClrSpecHeadFore
      :nBtnFace   :=  GetSysColor( COLOR_BTNFACE ) // nClrSpecHeadBack
      :nBClrSpH   :=  GetSysColor( COLOR_BTNFACE ) // nClrSpecHeadBack
      // ��� ����� � �������
      :nClrBC   := RGB(195,224,133)           // ���� ���� �������
      :nClrPrc  := RGB(187,240,197)           // ������ % 2
      :nClrErr  := RGB(192,0,255)             // ���������� - ���� ������
      :nHead1   := ToRGB(LGREEN)              // ������ � ����� �������
      :nHead2   := RGB(18,236,48)             // ������ � ����� �������
   END WITH

   WITH OBJECT oBrw
      O := :Cargo
      :nClrLine              := RGB(180,180,180)                  // COLOR_GRID
      :SetColor( {  1 }, { { || CLR_BLACK               } } )     // 1 , ������ � ������� �������
      :SetColor( {  2 }, { { || O:nClrBC                } } )     // 2 , ���� � ������� �������
      :SetColor( {  3 }, { { || CLR_YELLOW              } } )     // 3 , ������ ����� �������
      :SetColor( {  4 }, { { || { O:nHead2, O:nHead1 }  } } )     // 4 , ���� ����� �������
      :SetColor( {  5 }, { { || RGB(0,0,0)              } } )     // 5 , ������ �������, ����� � ������� � �������
      :SetColor( {  6 }, { { |a,b,c| a:=nil, iif( c:nCell == b, -CLR_HRED, -RGB(7,118,23) ) } } )  // 6 , ���� �������
      :SetColor( {  9 }, { { || CLR_YELLOW              } } )     // 9 , ������ ������� �������
      :SetColor( { 10 }, { { || { O:nHead1, O:nHead2 }  } } )     // 10, ���� ������� �������
      :SetColor( { 11 }, { { || RGB(0,0,0)              } } )     // 11, ������ ����������� ������� (selected cell no focused)
      :SetColor( { 12 }, { { |a,b,c| a:=nil, iif( c:nCell == b, -CLR_HRED, -RGB(9,57,16) ) } } ) // 12, ���� ����������� ������� (selected cell no focused)
      :hBrush  := CreateSolidBrush(187,240,197)                   // ���� ���� ��� ��������
   END WITH

   // ������� ���� ������� - ���� ����������� ������� / own virtual column
   oBrw:GetColumn("NN"):nClrBack := oBrw:Cargo:nBtnFace  //GetSysColor( COLOR_BTNFACE )

   aItg := oWnd:Cargo:aItog      // ������� ����� �� ����

   FOR EACH xVal IN aItg
       IF ( nCol := hb_EnumIndex(xVal) ) == 1 ; LOOP
       ENDIF
       oCol := oBrw:GetColumn(nCol)
       ? ProcNL() , nCol, oCol:cName, xVal
       // �������� ���� ����� � �������
       oCol:nClrBack := {|nv,nc,ob| // ��� ����� ������ � "NAME_3" � �� ������ ��������
                           Local oc
                           Local o := oBrw:Cargo            // ������������� ���������
                           Local nClrPrc := o:nClrPrc       // ������ % 2
                           Local nClrBC  := o:nClrBC        // ���� ���� �������
                           Local nCol    := o:nNAME_3       // ����� ������� 3
                           Local nClr    := nClrBC
                              // �������� ������ � �������� - :nAt ��� ��� ����
                              ob:nAt := Min(ob:nAt, ob:nLen)
                              oc := ob:GetColumn(nc)
                              nv := ob:GetValue(nCol)
                              nClr := iif( Month(nv) % 2 == 0, nClrPrc, nClrBC )
                           Return nClr
                           }

       IF hb_IsNumeric(aItg[nCol])
          oCol:nClrFore := {|nv,nc,ob|
                             Local oc, nClr := CLR_BLACK
                                // �������� ������ � �������� - :nAt ��� ��� ����
                                ob:nAt := Min(ob:nAt, ob:nLen)
                                oc := ob:GetColumn(nc)
                                nv := ob:GetValue(nc)
                                IF nv < 0 ; nClr := CLR_HRED
                                ENDIF
                             Return nClr
                            }
          oCol:nClrFocuFore := {|nv,nc,ob|
                                 Local oc, nClr := CLR_BLACK
                                    // �������� ������ � �������� - :nAt ��� ��� ����
                                    ob:nAt := Min(ob:nAt, ob:nLen)
                                    oc := ob:GetColumn(nc)
                                    nv := ob:GetValue(nc)
                                    IF nv < 0 ; nClr := CLR_HRED
                                    ENDIF
                                 Return nClr
                                }
       ELSE
          IF oCol:cName == "NAME_3"
             // ������ ���� �� ������� ��������� �������� ���� - ������� 2
             oCol:nClrFore := {|nv,nc,ob|
                                Local o := oBrw:Cargo         // ������������� ���������
                                Local nCurYear := o:nCurrentYear
                                Local oc, nClr := CLR_BLACK
                                   // �������� ������ � �������� - :nAt ��� ��� ����
                                   ob:nAt := Min(ob:nAt, ob:nLen)
                                   oc := ob:GetColumn(nc)
                                   nv := ob:GetValue(nc)
                                   IF Year(nv) # nCurYear ; nClr := CLR_YELLOW
                                   ENDIF
                                Return nClr
                              }
             oCol:nClrBack := {|nv,nc,ob|
                                Local o := oBrw:Cargo           // ������������� ���������
                                Local nCurYear := o:nCurrentYear
                                Local nClrPrc  := o:nClrPrc     // ������ % 2
                                Local nClrBC   := o:nClrBC      // ���� ���� �������
                                Local nClrErr  := o:nClrErr     // ���� ���� ������
                                Local oc, nClr := nClrBC
                                   // �������� ������ � �������� - :nAt ��� ��� ����
                                   ob:nAt := Min(ob:nAt, ob:nLen)
                                   oc := ob:GetColumn(nc)
                                   nv := ob:GetValue(nc)
                                   IF hb_IsDate(nv)         // ��� ����� ��������
                                      IF Year(nv) == nCurYear
                                         nClr := iif( Month(nv) % 2 == 0, nClrPrc, nClrBC )
                                      ELSE
                                         nClr := nClrErr
                                      ENDIF
                                   ENDIF
                                Return nClr
                              }
          ENDIF
       ENDIF
   NEXT

RETURN Nil

///////////////////////////////////////////////////////////////////////////
STATIC FUNCTION SortSum_Refresh( oBrw, lSum )
   DEFAULT lSum := .T.

   IF Len( oBrw:aArray) > 1
      _wSend(67, oBrw:cParentWnd, oBrw:nCell)   // sort
   ENDIF
   IF !Empty(lSum)
      _wSend(68, oBrw:cParentWnd)               // summa
   ENDIF
   IF Len( oBrw:aArray) < 2
      oBrw:nLen := Len(oBrw:aArray)
      oBrw:nAt  := Min(oBrw:nAt, oBrw:nLen)
      oBrw:GoPos(1, oBrw:nCell)
   ENDIF
   // :lNoKeyChar  := .F/T.   // .T. - ����. ����� KeyChar(...) - ���� �� ����, ����
   oBrw:lNoKeyChar := oBrw:aArray[1][1] == 0    // edit / not edit ������ KeyChar()
   DO EVENTS
   _wPost(69, oBrw:cParentWnd)                  // refresh

RETURN Nil

///////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Sort_Table( ow, ob, nc )
   Local cWnd, oc, cc := "NAME_3", ne

   cWnd := ow:Name
   ob := This.oBrw.Object
   IF !hb_IsNumeric(nc)
      nc := iif( hb_IsChar(nc), nc, cc )
      nc := ob:nColumn(nc)
   ENDIF
   ne := nc                    // ��������� ��� ����
   nc := ob:nColOrder          // �� ��� �� ������� sort
   oc := ob:GetColumn(nc)
   ob:SetOrder(nc, , oc:lDescend)
   ob:nCell := ne              // ������� ��� ���� �������
   DO EVENTS

RETURN Nil

///////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Itog_Table( ow )
   Local aItg := ow:Cargo:aItog
   Local oBrw := This.oBrw.Object
   Local nPos := oBrw:nColumn("NN")
   Local aLine, nCol, oCol

   FOR nCol := 1 TO Len(aItg)
       IF hb_IsNumeric(aItg[nCol]) ; aItg[nCol] := 0      // ����� � 0
       ENDIF
   NEXT
   FOR EACH aLine IN oBrw:aArray
       FOR nCol := 1 TO Len(aLine)
           IF nCol == nPos
              aItg[nCol] += iif( aLine[nCol] > 0, 1, 0 )  // ����� �� ���-��
           ELSEIF hb_IsNumeric(aItg[nCol]) .and. hb_IsNumeric(aLine[nCol])
              aItg[nCol] += aLine[nCol]                   // ����� ���������
           ENDIF
       NEXT
   NEXT
   FOR EACH oCol IN oBrw:aColumns
       nCol := hb_EnumIndex(oCol)
       IF hb_IsNumeric(aItg[nCol])
          oCol:cFooting := cValToChar(aItg[nCol])         // ����� � ������ �������
       ENDIF
   NEXT
   DO EVENTS

RETURN Nil

///////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Draw_Table( ow, ob )

   ob := This.oBrw.Object
   ob:DrawFooters()
   ob:Refresh()
   DO EVENTS
   ob := ow

RETURN Nil

///////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Butt_Table( oWnd, oBrw, lNoRows, lEnable )
   LOCAL nK, aBtn, o
   DEFAULT lNoRows := Len(oBrw:aArray) == 1 .and. oBrw:aArray[1][1] == 0 // .T. - ��� ������ �������
   DEFAULT lEnable := .F.

   IF lNoRows
      aBtn := oWnd:GetObj4Type( "BUTT", .F. )      // "BUTT" $ _HMG_ControlType[ i ]
      FOR EACH o IN aBtn                           // object button
          nK := Val(right( o:Name, 2 ))
          IF nK > 1 .and. nK < 5             // Btn_02,Btn_03,Btn_04 => disable\enable
             SetProperty(oWnd:Name, o:Name, "Enabled", lEnable)
          ENDIF
      NEXT
   ENDIF
   DO EVENTS

RETURN Nil

///////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Cols_Blank( oBrw, lAddRec, nAtPos )
   LOCAL nCol, xVal
   DEFAULT lAddRec := .F.
   DEFAULT nAtPos  := 1

   IF lAddRec ; oBrw:aArray := { aClone(oBrw:Cargo:aAddRec) }
   ENDIF

   FOR nCol := 1 TO Len( oBrw:aArray[nAtPos] )
       xVal := oBrw:aArray[nAtPos][ nCol ]
       oBrw:aArray[nAtPos][ nCol ] := Blank( xVal, Valtype(xVal))
   NEXT

RETURN nAtPos

/////////////////////////////////////////////////////////////////////
STATIC FUNCTION myRunBtn(oWnd, xPar, cNBtn, oBrw)
   LOCAL cWnd, nKy, aParams := hb_aParams(), lNoRows, cMsg
   LOCAL nKyBase := 10, nI, nK, nCol, xVal, aRec, nDel
   DEFAULT oWnd := ThisWindow.Object

   cWnd := oWnd:Name

   ? ProcNL(), cWnd
   ? "aParams=", aParams, oWnd:ClassName, oBrw:ClassName ; ?v aParams

   IF hb_IsChar(cNBtn)
      IF _IsControlDefined(cNBtn,cWnd)
        // ������ ��� ������������� ������ ��� �������
        // This.&(cNBtn).Enabled := .F.  // ����������� ������ - � �������� �������
      ENDIF
   ELSE
      cMsg  := "Parameter not defined !;"
      cMsg  += "cNBtn=" + cValToChar(cNBtn)
      AlertStop(cMsg)
      cNBtn := "keyNone"
   ENDIF

   lNoRows := Len(oBrw:aArray) == 1 .and. oBrw:aArray[1][1] == 0 // .T. - ��� ������ �������

   IF hb_IsArray(xPar)
      ? "xPar=", hb_valtoexp(xPar)
      FOR EACH nKy IN xPar // nKy := 1, 2, 3
          ? hb_enumindex(nKy), nKy
      NEXT
      ?
   ELSE
      nKy := xPar - nKyBase   // nKy := 0, 1, 2, 3
      ? nKy
      IF lNoRows .and. nKy > 1                           // ��� ������ �������
      ELSEIF nKy == 0    // �������� 1 ������
         IF cNBtn == "BTN_INS" // ������ INSERT
            cMsg := "�� ������ �������� ����� ������ � ������� ?;"
            cMsg += "You want to insert a NEW record into the table ?;"
         ELSE
            cMsg := "�� ������ �������� ����� ������ � ������� ?;"
            cMsg += "Do you want to insert a DOUBLE record into the table ?;"
         ENDIF
         IF MG_YesNo(cMsg, "������� ������ / Insert an entry")
            oBrw:Enabled(.F.)

            IF lNoRows                                  // ��� ������ �������
               oBrw:aArray := {} ; aRec := aClone(oBrw:Cargo:aAddRec)
            ELSE                 ; aRec := aClone(oBrw:aArray[ oBrw:nAtPos ])
            ENDIF
            IF cNBtn == "BTN_INS" ; aRec := aClone(oBrw:Cargo:aAddRec)
            ENDIF

            AAdd(oBrw:aArray, aRec)
            nK := Len(oBrw:aArray)

            oBrw:Enabled(.T.)
            oBrw:Reset()
            oBrw:GotoRec(nK)
            oBrw:Cargo:nModify += 1

            SortSum_Refresh( oBrw )

            Butt_Table( oWnd, oBrw, .T., .T. )

         ENDIF
      ELSEIF nKy == 1    // �������� 3 ������
         cMsg := "�� ������ �������� 3 ������ � ������� ?;"
         cMsg += "Do you want to insert 3 records into the table?;"
         IF MG_YesNo(cMsg, "������� ������ / Insert an entry")
            oBrw:Enabled(.F.)

            IF lNoRows                                  // ��� ������ �������
               oBrw:aArray := {}
               aRec := aClone(oBrw:Cargo:aAddRec)
            ELSE
               aRec := aClone(oBrw:aArray[1])
            ENDIF
            nCol := oBrw:nColumn("NAME_3")
            aRec[ nCol ] := StoD(hb_ntos(year(Date()))+"0101") // CtoD( "01.01." + HB_NtoS(Year(Date())) )
                                                               // �������  ^  ^ ������� �� SET DATE TO ������
            FOR nI := 1 TO 3 ; AAdd(oBrw:aArray, aClone(aRec) )
            NEXT

            oBrw:Enabled(.T.)
            oBrw:Reset()
            oBrw:Cargo:nModify += 1

            SortSum_Refresh( oBrw )

            Butt_Table( oWnd, oBrw, .T., .T. )

         ENDIF

      ELSEIF nKy == 2    // ������� (01.01)
         cMsg := "������� ������ 01.01.yyyy �� ������� (3) ?;"
         cMsg += "Delete entries 01.01.yyyy from column (3) ?;"
         IF MG_YesNo(cMsg, "�������� ������ / Deleting an entry")
            oBrw:Enabled(.F.)
            nCol := oBrw:nColumn("NAME_3")
            ? 'oBrw:nColumn("NAME_3")=', nCol
            ? "aArray, aColumns =", oBrw:aArray, oBrw:aColumns, "NAME_3 ="
            ? 'oBrw:nColumn("NAME_3")=', nCol
            aRec := {}
            nDel := 1
            nK := 0
            WHILE nDel > 0
               nDel := 0
               FOR nI := 1 TO Len(oBrw:aArray)
                   xVal := oBrw:aArray[nI][ nCol ]
                   ? nI, xVal
                   IF !Empty(xVal) .and. hb_IsDate(xVal) .and. Right(Dtos(xVal), 4) == "0101"
                      IF Len(oBrw:aArray) > 1 ; hb_ADel(oBrw:aArray, nI, .T.)
                      ELSE                    ; Cols_Blank( oBrw )
                      ENDIF
                      nDel := nI
                      nK++
                      ?? "Delete !"
                      EXIT
                   ENDIF
               NEXT
            END
            IF nK > 0 ; oBrw:Reset()
            ENDIF
            oBrw:Enabled(.T.)
            oBrw:Cargo:nModify += 1

            SortSum_Refresh( oBrw )

            Butt_Table( oWnd, oBrw,  )

         ENDIF

      ELSEIF nKy == 3    // ������� ���� ������
         cMsg := "�� ������ ������� ��� ������ ?;"
         cMsg += "Do you want to delete this entry?;"
         IF MG_YesNo(cMsg, "�������� ������ / Deleting an entry")
            oBrw:DeleteRow( ) // Array method - Delete selected row
            oBrw:Cargo:nModify += 1

            SortSum_Refresh( oBrw )

            Butt_Table( oWnd, oBrw,  )

         ENDIF

      ELSEIF nKy == 4    // ������� ���� ������
         cMsg := "�� ������ ������� ��� ������� ?;"
         cMsg += "Do you want to drop the ENTIRE table?;"
         IF MG_YesNo(cMsg, "������� ������� / Clearing the table" )
            Cols_Blank( oBrw, .T. )
            oBrw:Reset()
            oBrw:Cargo:nModify += 1

            SortSum_Refresh( oBrw )

            Butt_Table( oWnd, oBrw, .T. )

         ENDIF

      ELSEIF ! lNoRows
         MG_Debug("��� ��������� ������� / No event handling =", nKy, xPar, nKyBase)

      ENDIF

   ENDIF

   IF _IsControlDefined(cNBtn,cWnd)
      // �������������� ������,  ���� ������� �� ������
      This.&(cNBtn).Enabled := oBrw:aArray[1][1] > 0
   ENDIF

RETURN NIL

/////////////////////////////////////////////////////////////////////
FUNCTION ArrayJsonLoad(oWnd, ky, oBrw)
   LOCAL cFileJson := (App.Cargo):cFileJson
   LOCAL cPathTemp := (App.Cargo):cPathTemp
   LOCAL cMsg, aDim, cStr, i, j, a, h, aNew, nVers
   LOCAL aVal, xVal, aTsbOne, aType, cVers, cDate
   LOCAL lRet := .F.

   ? ProcNL(), oWnd:Name, oWnd:ClassName, ky, oBrw, oBrw:cAlias
   ? cFileJson, cPathTemp
   cMsg := "�������� / Attention !;"
   cMsg += "�� ����� ���� ���� � ���������� ��������;"
   cMsg += "There is a file on the disk with the previous table;"
   cMsg += cFileJson + ";;"
   cMsg += "�� ������ ��������� ��� ������� ?"
   cMsg += "Do you want to download this spreadsheet ?;"

   nVers := (App.Cargo):nVersion                         // ������ �������
   cDate := (App.Cargo):cVerDate                         // ���� ������ �������
   cVers := (App.Cargo):cTsbName + HB_NtoS(nVers)
   FOR EACH i, j IN oWnd:Cargo:aLabel, {cVers, cDate} ; SetProperty(oWnd:Name, i, "Value", j)
   NEXT
   oWnd:StatusBar:Say(cVers, 2)
   oWnd:StatusBar:Say(cDate, 3)

   IF !FILE(cFileJson) ; RETURN lRet
   ENDIF

   IF ( lRet := MG_YesNo( cMsg, "����� / Choice", , , , , {"&���������/Download", "&������/Cancel"} ) )

      cStr := HB_MemoRead(cFileJson)
      cStr := SubS( cStr, At ("{", cStr) )
      cStr := Left( cStr, RAt("}", cStr) )

      hb_jsonDecode(cStr,@h)
      IF !HHasKey( h, "ArrayTsbrowse" )
         lRet := .F.
         cStr := "������������ JSON ! ��� ������: ArrayTsbrowse !;"
         cStr += "Invalid JSON ! No section: ArrayTsbrowse !;"
         MG_Stop(cStr)
         RETURN lRet
      ENDIF
      IF HHasKey( h, "VERSION" )   ;  nVers := h["VERSION"]
      ELSE                         ;  nVers := -1
      ENDIF
      IF HHasKey( h, "VERDATE" )   ;  cDate := h["VERDATE"]
      ELSE                         ;  cDate := "��� � JSON"
      ENDIF

      aTsbOne := oBrw:Cargo:aAddRec // �������� ������ ����� ����� ������
      aType := Array(Len(aTsbOne))
      ? "�������� ���� ������ oBrw:Cargo:aAddRec=", aTsbOne, HB_ValToExp(aTsbOne)
      For i := 1 to Len(aTsbOne)
         ? i, ValType(aTsbOne[i]), aTsbOne[i]
         aType[i] := ValType(aTsbOne[i])
      Next
      ? "�������� ������ ����� ����� ������ aType=", aType

      ? "������ ����� ������ aDim="
      aDim := {}
      a   := h["ArrayTsbrowse"]
      For i := 1 to Len(a)
         aVal := a[i]
         ? "----- i=", i, aVal, HB_ValToExp(aVal)
         aNew := Array(Len(aVal))
         For j := 1 To Len(aVal)
            xVal := aVal[j]
            IF hb_IsChar(xVal)
               IF Left(xVal, 2) == "0d" .and. Len(xVal) == 10
                  xVal := StoD(Substr(xVal, 3))
               ELSEIF left(xVal, 2) == 't"' .and. ":" $ xVal
                  xVal := &(xVal)
               ENDIF
            ENDIF
            aNew[j] := xVal
         Next
         AADD( aDim, aNew )
      Next
      ? "aDim=",aDim ; ?v aDim

      oBrw:Enabled(.F.)
      oBrw:aArray := aDim
      oBrw:Enabled(.T.)
      oBrw:Reset()
      oBrw:Cargo:nModify := 0

      SortSum_Refresh( oBrw )

      cVers := (App.Cargo):cTsbName + HB_NtoS(nVers)
      (App.Cargo):nVersion := nVers                       // ������� ����� ������ �������
      FOR EACH i, j IN oWnd:Cargo:aLabel, {cVers, cDate}
          SetProperty(oWnd:Name, i, "Value", j)           // ������, ���� ������ �������
      NEXT
      oWnd:StatusBar:Say(cVers, 2)
      oWnd:StatusBar:Say(cDate, 3)

   ENDIF

RETURN lRet

/////////////////////////////////////////////////////////////////////
FUNCTION ArrayJsonSave(oWnd, ky, oBrw)
   LOCAL oApp      := App.Cargo
   LOCAL cFileJson := oApp:cFileJson    // ��� App.Cargo:cFileJson
   LOCAL cPathTemp := oApp:cPathTemp
   LOCAL nVers     := oApp:nVersion     // ������ �������
   LOCAL cMsg, h, i, j, u, aTmp
   LOCAL lRet := .F.

   ? ProcNL(), oWnd:Name, oWnd:ClassName, ky, oBrw, oBrw:cAlias, oBrw:Cargo:nModify
   ? cFileJson, cPathTemp
   // ���� ������ �� ��������� ��� ��������� ������ ��� - �� ��� �����
   //IF LEN(oBrw:aArray) == 1
   //   RETURN NIL
   //ENDIF
   //IF Empty(oBrw:Cargo:nModify)  // ����������� � ������� �� ����, No Save
   //   RETURN lRet
   //ENDIF

   cMsg := "�������� / Attention !;"
   h    := Hash()
   h["COPYRIGHT"] := "Andrey Verchenko & Sergej Kiselev"
   h["DATE"]      := "25.02.2023"
   h["VERSION"]   := nVers + 1
   h["VERDATE"]   := HB_TTOC( HB_DATETIME() )
   // ���������� json
   aTmp := aClone(oBrw:aArray)
   FOR i := 1 TO Len(aTmp)
       FOR j := 1 TO Len(aTmp[ i ])
          u := aTmp[ i ][j]
          IF hb_IsDateTime(u) ; u := hb_valtoexp(u)
          ELSEIF hb_IsDate(u) ; u := hb_valtoexp(u)
          ELSEIF hb_IsChar(u) ; u := alltrim(u)
          ENDIF
          aTmp[ i ][j] := u
       NEXT
   NEXT
   h["ArrayTsbrowse"] := aTmp

   IF FILE(cFileJson)
      cMsg += "�� ����� ��� ���� ���� � ���������� ��������;"
      cMsg += "There is already a file with the previous table on the disk;"
      cMsg += cFileJson + ";;"
      cMsg += "�� ������ ������������ ��� ������� ?;"
      cMsg += "Do you want to overwrite this table?"
   ELSE
      cMsg += ";�� ������ �������� ��� ������� � ���� ?"
      cMsg += ";Do you want to write this table to a file ?"
   ENDIF
   IF ( lRet := MG_YesNo( cMsg, "����� / Choice", , , , , {"&���������/Save", "&Cancel"} ) )
      j := hb_jsonEncode(h,.T.)  //.F.
      HB_MemoWrit( cFileJson, j)
   ENDIF

RETURN lRet

//////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Access_Table(oWnd,ky,oBrw)
   LOCAL cAcs, cMsg, cEng, oCol, i, aWinBtn, aGrClr, aBtn, lFind, aBtnGrd
   LOCAL aGrFill, aBtnGrad, cBtnObj, cCapt, lNoKeyChar

   ? ProcNL(), oWnd:Name, oWnd:ClassName, oBrw:cAlias, oBrw:ClassName, "ky=",ky
   aWinBtn := oWnd:Cargo:aWinBtn
   ? "      oWnd:Cargo:aWinBtn=",aWinBtn ; ?v aWinBtn ; ?

   IF lStatAccess ; lStatAccess := .F.  ; lNoKeyChar := .T.
   ELSE           ; lStatAccess := .T.  ; lNoKeyChar := .F.
   ENDIF
   IF lStatAccess ; cAcs := "�����ب�" ; cEng := "ENABLED"  ; aGrClr := CLR_BLUE
   ELSE           ; cAcs := "�����٨�" ; cEng := "DISABLED" ; aGrClr := CLR_RED
   ENDIF

   FOR EACH oCol IN oBrw:aColumns
      i := hb_EnumIndex(oCol)
      IF i # 1
         // �� ������ ������� ����� lStatAccess ��� ����������/������� ������� �������
         oCol:Cargo:lStatAccess := lStatAccess
      ENDIF
   NEXT

   // :lNoKeyChar  := .F/T.   // .T. - ����. ����� KeyChar(...) - ���� �� ����, ����
   oBrw:lNoKeyChar := lNoKeyChar    // edit / not edit ������ KeyChar()
   cMsg := "�������� ! ������ � �������������� �������: " + cAcs + ";;"
   cMsg += "ATTENTION ! Access to edit columns: " + cEng

   MG_Info(cMsg)

   lFind := .F.
   FOR i := 1 TO LEN(aWinBtn)
      aBtn := aWinBtn[i]
      IF aBtn[7] == ky
         // {6, "Btn_05", e"������ ��\r\n�������=.T.", 8388608, {255, 255, 255}, NIL, 80}
         lFind   := .T.
         cBtnObj := aBtn[2]
         cCapt   := aBtn[3]
         aBtnGrd := aBtn[5]
         EXIT
      ENDIF
   NEXT

   IF lFind
      IF "\" $ cCapt
         cCapt := StrTran(cCapt, "\", CRLF )
      ENDIF
      cCapt := SUBSTR(cCapt,1, AT("=",cCapt) ) + iif( lStatAccess, "Yes", "No" )
      //cCapt  := "������ ��" + CRLF + "�������=" + iif( lStatAccess, "Yes", "No" )
      aBtnGrad := { aGrClr, aBtnGrd }
      aGrFill  := { { 0.5, aBtnGrad[1], aBtnGrad[2] }, { 0.5, aBtnGrad[2], aBtnGrad[1] } }
      SetProperty(oWnd:Name, cBtnObj, "GRADIENTFILL", aGrFill)
      SetProperty(oWnd:Name, cBtnObj, "CAPTION" , cCapt  )
   ENDIF

RETURN NIL

///////////////////////////////////////////////////////////////////////////
STATIC FUNCTION CreateDatos()
   LOCAL i, k := 17
   LOCAL aDatos, aHead, aSize, aFoot, aPict, aAlign, aName, aField, aNewRec

      aDatos := Array( k )
      FOR i := 1 TO k
         aDatos[ i ] := {   ;                              //
            1, ;                                           // 1 - ���� ����������� ������� / own virtual column
            "Str" + ntoc( i ) + "_123" + Repl("x",10) ,;   // 2
            Date() + i*20, ;                               // 3
            PadR( "Test line - " + ntoc( i ), 20, "a" ),;  // 4
            Round( ( 10000 -i ) * i / 3, 2 ), ;            // 5
            100.00 * i, ;                                  // 6
            0.12 * i/100, ;                                // 7
            Round( 100.00 * i * 0.12, 2 ), ;               // 8
            Round( 1234567.00 / i, 3 ), ;                  // 9
            PadR( "Line " + StrZero( i, 5 ), 20 ), ;       // 10
            Date() - i, ;                                  // 11
            Time(), ;                                      // 12
            i % 2 == 0 ,;                                  // 13
            Hb_DateTime() - i*100 ,;                       // 14
            Hb_DateTime() + i*50 ,;                        // 15
            Date() + i  }                                  // 16
            IF i > 12
               aDatos[ i ][2] := "-"
               aDatos[ i ][3] := StoD(hb_ntos(year(Date()))+"0101")
               //aDatos[ i ][3] := CtoD( "01.01." + HB_NtoS(Year(Date())) ) - ������� ������� �� SET DATE TO ������
               aDatos[ i ][4] := "-"
               aDatos[ i ][5] := aDatos[ i ][5] * -1
               aDatos[ i ][6] := aDatos[ i ][6] * -1
               aDatos[ i ][7] := aDatos[ i ][7] * -1
               aDatos[ i ][8] := aDatos[ i ][8] * -1
               aDatos[ i ][9] := aDatos[ i ][9] * -1
            ENDIF
         IF i == 12
            aDatos[ i ][3] := StoD(hb_ntos(year(Date()))+"0201")
         ENDIF
         IF i > 15
            aDatos[ i ][3] := StoD("20190101")
            aDatos[ i ][4] := "-Error-"
            aDatos[ i ][5] := aDatos[ i ][6] := aDatos[ i ][7] := 0
            aDatos[ i ][8] := aDatos[ i ][9] := 0
         ENDIF
         IF i > 16
            aDatos[ i ][3] := StoD("20190201")
         ENDIF
      NEXT

      aHead  := AClone( aDatos[ 1 ] )
      AEval(aHead, {|x,n| x:=nil, aHead[ n ] := "Col_" + hb_ntos(n) })

      aFoot  := Array( Len( aDatos[ 1 ] ) )
      AEval( aFoot, {| x, n| x:=nil, aFoot[ n ] := n } )
      // aFoot  := .T.                           // �������� ���� � ������� ����������

      aPict := Array( Len( aDatos[ 1 ] ) )       // ����� �� ��������, �����������
      aPict[ 10 ] := "99999999999.999"           // ��������� ��� C,N �� ��� ��������

      aSize := Array( Len( aDatos[ 1 ] ) )       // ����� �� ��������, �����������
      aSize[ 10 ] := aPict[ 10 ]                 // ��������� �� ��� �������� � �������

      aAlign    := Array( Len( aDatos[ 1 ] ) )   // ��� ���� C  - DT_LEFT
      aAlign[ 2 ] := DT_CENTER                   // D,L - DT_CENTER
                                                 // N - DT_RIGHT

      aName := Array( Len( aDatos[ 1 ] ) )
      AEval( aName, {| x, n| x:=nil, aName[ n ] := UPPER( "Name_" + hb_ntos( n ) ) } )  // ����������� UPPER()

      // ��� ������������ ��� �������
      FOR i := 1 TO LEN(aName)
        aFoot[ i ] := aName [ i ]
      NEXT

      // ���� ����������� ������� / own virtual column
      aPict [ 1 ] := "999 999"
      aAlign[ 1 ] := DT_CENTER
      aFoot [ 1 ] := aName[ 1 ] := aHead[ 1 ] := "NN"

      aNewRec := AClone( aDatos[ 1 ] )
      // ������� ������ ����������� ������ ��� ����� ����� �������
      aNewRec[ 1] := 1                                      // 1 - "�����������" ������� / "virtual" column
      aNewRec[ 2] := "Str" + "_???_" + Repl("?",9)          // 2
      //aNewRec[ 3] := Eom(aNewRec[ 3])                     // 3
      aNewRec[ 3] := DATE()                                 // 3
      aNewRec[ 4] := "Test line - ???"                      // 4
      aNewRec[ 5] := 0                                      // 5
      aNewRec[ 6] := 0                                      // 6
      aNewRec[ 7] := 0.00                                   // 7
      aNewRec[ 8] := 0.00                                   // 8
      aNewRec[ 9] := 0.000                                  // 9
      aNewRec[10] := PadR( "Line " + '?????' )              // 10
      aNewRec[11] := Bom(aNewRec[11])                       // 11
      aNewRec[12] := '23:59:59'                             // 12
      aNewRec[13] := .F.                                    // 13
      aNewRec[14] := Hb_DateTime()                          // 14
      aNewRec[15] := Hb_DateTime()                          // 15
      aNewRec[16] := Date()                                 // 16

RETURN { aDatos, aHead, aSize, aFoot, aPict, aAlign, aName, aField, aNewRec }

///////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTopMenu( nY, nX, nG )
   LOCAL nHeight, nWidth, i, y, x, w, h, nGaps, cObj, nPst, aObj, lBlk, aBlk
   LOCAL aIco, aClr, aPst, aCap, cCap, cIco, aBtnObj := {}

   nHeight := 0
   nY :=  nG ; nX := nG ; nGaps := int(nG * 1.7)
   y  := nY
   x  := nX
   w  := 135
   h  := 60

         //       1                  2                           3                     4                5                   6                      7        8
// aCap  := { "�����;������"     , "��������;3 ������", "������� (01.01);� ������� 3", "�������;������� ������", "�������;���� ������", "������ ��;�������=Yes", "(i) ������" , "�����"  }
   aCap  := { "Duplicate;records", "Insert;3 records" , "Delete (01.01);in column 3" , "Delete;current record" , "Delete;whole list"  , "Access;columns=Yes"   , "(i) Help"   , "Exit"   }
   aClr  := { {177,192,16}       , CLR_ORANGE         , CLR_GRAY                     , CLR_GRAY                , CLR_GRAY             , CLR_BLUE               , {141,179,226}, CLR_HRED }
   aPst  := { 10, 11, 12, 13, 14, 80, 90, 99 }  // _wPost(�) - ����� ������� �� ������
   aIco  := array(Len(aCap))
   aObj  := array(Len(aCap))
   aBlk  := array(Len(aCap)) ; aFill(aBlk, .T.)  // ����������� ������ ��� �������

   FOR EACH cObj, cCap, cIco, nPst, lBlk IN aObj, aCap, aIco, aPst, aBlk
      i := hb_EnumIndex(cObj)
      DEFAULT cObj := "Btn_"+StrZero(i - 1, 2)
      cCap := StrTran( aCap[ i ], ";" , CRLF )
      my2BUTTON(y, x, w, h, cObj, cCap, {aClr[ i ], WHITE}, , cIco, , , nPst, lBlk )  // ����� - �������
      x += This.&(cObj).Width + nGaps
      AADD( aBtnObj, { i, cObj, cCap, aClr[ i ], WHITE, cIco, nPst, lBlk } )
      nWidth := x
   NEXT

   (This.Cargo):aWinBtn := aBtnObj  // �������� ������ �������� ������ � cargo ����

   nHeight := h + nY * 2

RETURN { nHeight, nWidth }

///////////////////////////////////////////////////////////////////////////
#define COPYRIGHT  "Author: Andrey Verchenko. Dmitrov, 2023."
#define COPYRIGHT2 "Author: Sergej Kiselev. Latvia, 2023."
#define PRG_VERS  "Version 1.0"
#define PRG_RU01  "������� �� �������. ������ ������ � ��������: ��������/�������/�����"
#define PRG_RU02  "���� ����������� ������� ����� - ���������"
#define PRG_EN01  "Table by array. Recording methods: deletion/insertion/duplicate"
#define PRG_EN02  "Own virtual column on the left - numerator"
#define PRG_INFO1  "Tips and tricks programmers from our forum http://clipper.borda.ru"
#define PRG_INFO2  "Thanks a lot to all"
/////////////////////////////////////////////////////////////////////
FUNCTION MsgAbout()
   RETURN MG_Info( PadC( (App.Cargo):cPRG_NAME , 70 ) + CRLF + ;
                   PadC( PRG_VERS , 70 ) + CRLF + ;
                   PadC( COPYRIGHT, 70 ) + CRLF + ;
                   PadC( COPYRIGHT2, 70 ) + CRLF + CRLF + ;
                   PadC( PRG_RU01 , 70 ) + CRLF + ;
                   PadC( PRG_RU02 , 70 ) + CRLF + ;
                   PadC( PRG_EN01 , 70 ) + CRLF + ;
                   PadC( PRG_EN02 , 70 ) + CRLF + CRLF + ;
                   PadC( PRG_INFO1, 70 ) + CRLF + ;
                   PadC( PRG_INFO2, 70 ) + CRLF + CRLF + ;
                   hb_compiler() + CRLF + ;
                   Version() + CRLF + ;
                   MiniGuiVersion() + CRLF + CRLF + ;
                   PadC( "This program is Freeware!", 70 ) + CRLF + ;
                   PadC( "Copying is allowed!", 70 ), "About", "ZZZ_B_ALERT", .F. )
