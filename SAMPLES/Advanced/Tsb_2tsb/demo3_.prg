/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
*/
#define  _HMG_OUTLOG

#include "hmg.ch"
#include "tsbrowse.ch"
#include "dbinfo.ch"

FUNCTION My_File(oWnd, cName, cTypW, nY, nX, nFile, nDubl, aBClr)
   LOCAL cForm, cFocus := "Buff", cCap := "Array TBrowse. DEMO3. "
   LOCAL o, y, x, w, h, owc, oTsb, nW, nH, nK
   LOCAL nDop, nMin := 5, nMax := 21   // ~кол-во строк в тсб
   LOCAL oac := App.Cargo
   Default oWnd  := ThisWindow.Object
   Default cName := ProcName(), cTypW := "STANDARD"
   Default nY := 0, nX := 0, nFile := 2, nDubl := 0

   cForm := "w"+cName+"_"+hb_ntos(nFile)+"_"+hb_ntos(nDubl)
   cTypW := iif( oWnd:Type == "M", "MODAL", cTypW ) // под modal только modal
   cTypW := upper(left(cTypW, 1))
   //
   oWnd:Cargo:cLastBtn := cName
   //
   IF _IsWindowDefined( cForm )
      //owc := GetProperty(cForm, "Cargo")
      IF IsIconic( nH := GetFormHandle(cForm) ) ; _Restore( nH )
      ENDIF
      _wPost(23, cForm)
      RETURN cForm
   ENDIF
   //
   oTsb := oTsb_Def(nFile, nDubl):Clone()
   //
   nK := Len(oTsb:uAlias)
   nK := iif( nK < nMin, nMin, iif( nK > nMax, nMax, nK ) )

   IF nFile == 1 ; nDop := 5 + 1      // доп. строки к сетке от высоты фонта
   ELSE          ; nDop := 6 + 2 + 1  // доп. строки к сетке от высоты фонта
   ENDIF

   y := App.Object:nMargHeight        // y тсб
   x := App.Object:nMargWidth         // x тсб

   nW := Sys.ClientWidth * 0.8
   nW += x * 2
   IF ( nX + nW ) > Sys.ClientWidth
      nW := Sys.ClientWidth - nX
   ENDIF
   nH := oTsb:nHeightCell * ( nK + nDop ) // сетка от высоты фонта
   nH += y * 2                            
   IF ( nY + nH ) > Sys.ClientHeight
      nH := Sys.ClientHeight - nY 
   ENDIF
   //
   oWnd:Cargo:lRezult := .F.              // .T. - возврат результата 
   oWnd:Cargo:aRezult := {}               // Len(...) > 0
   //
      IF cTypW == "S"
         oWnd:Cargo:nLastWnd += 1      // счетчик уникальный окон standard
   DEFINE WINDOW &cForm TITLE cCap + "STANDARD "         ;
          AT nY,nX WIDTH nW HEIGHT nH BACKCOLOR aBClr    ;
          WINDOWTYPE STANDARD TOPMOST NOMAXIMIZE NOSIZE  ;
          ON INIT    ( This.Topmost := .F., _wPost(0) )  ;
          ON RELEASE ( This.Hide, _wSend(90) )
          This.Cargo := oHmgData() ; o := This.Object ; owc := o:Cargo
          owc:nLastWnd := oWnd:Cargo:nLastWnd      // ID окона standard
      ELSE
          _HMG_InplaceParentHandle := oWnd:Handle
   DEFINE WINDOW &cForm TITLE cCap + "MODAL "            ;
          AT nY,nX WIDTH nW HEIGHT nH BACKCOLOR aBClr    ;
          MODAL NOSIZE                                   ;
          ON INIT    ( _wPost( 0) )                      ;
          ON RELEASE ( _wSend(90) )
          This.Cargo := oHmgData() ; o := This.Object ; owc := o:Cargo
      ENDIF

      //
      owc:oParent  := oWnd
      owc:cFocus   := cFocus
      owc:cLastBtn := cName
      //
      @ 0,0 LABEL &cFocus VALUE "" WIDTH 2 HEIGHT 2 TRANSPARENT
      //
      oTsb_my(oTsb, nFile)                    // доп.настройка тсб
      //
      w := This.ClientWidth  - x * 2          // отступы окна по x
      h := This.ClientHeight - y * 2          // отступы окна по y
      //
      owc:oBrw := _TBrowse(oTsb, , , y, x, w, h) 
      //
      ON KEY F1     ACTION NIL
      ON KEY ESCAPE ACTION {|ob| ob := ThisWindow.Cargo:oBrw, ;
                                 iif( ob:IsEdit, ob:SetFocus(), _wSend(99) ) }
      o:Event( 0, {|ow| 
                    Local owc := ow:Cargo
                    ow:Title += space(3)+"FormName: "+ow:Name
                    IF ow:Type == "S"                          // standard
                       ow:Title += space(3)+"ID - "+hb_ntos(owc:nLastWnd)
                    ENDIF
                    Return Nil
                    })
      o:Event(23, {|ow| 
                    Local owc := ow:Cargo
                    ow:SetFocus()        // или на тсб фокус
                    //owc:oBrw:SetFocus()
                    DO EVENTS
                    Return Nil
                    })
      o:Event(99, {|ow| ow:Release() })
      
   END WINDOW

   IF nY == 0 .and. nX == 0
      CENTER WINDOW &cForm
   ENDIF
   ACTIVATE  WINDOW &cForm

   _HMG_InplaceParentHandle := 0

RETURN ""

FUNCTION oTsb_my(oTsb, nFile)
   Default nFile := 1

   IF nFile == 1
   ELSE
   ENDIF

RETURN oTsb

FUNCTION oTsb_Def(aDatos, nDubl)
   LOCAL a, i, j, k, m, o, t, oTsb

   IF IsNumeric(aDatos) ; aDatos := Get_arr(aDatos, nDubl)
   ENDIF

   oTsb := oHmgData() ; o := oTsb

   o:uAlias := aDatos[1]
   o:aHead  := aDatos[2]
   o:aName  := aDatos[3]
   //
   IF Empty(o:aHead)
      a := o:uAlias[1]
      k := Len(a)
      o:aHead := array(k)
      FOR i := 1 TO k ; o:aHead[ i ] := "_"+hb_ntos(i)+"_"
      NEXT
   ENDIF
   //
   IF Empty(o:aName)
      a := o:uAlias[1]
      k := Len(a)
      o:aName := array(k)
      FOR i := 1 TO k ; o:aName[ i ] := "_"+hb_ntos(i)+"_"
      NEXT
   ENDIF
   //
   o:lZebra      := .T.
   o:aFoot       := .T.
   o:uSelector   := 20
   o:aNumber     := { 1, App.Object:W(0.5) }
   o:aFoot       := .T.
   o:lSuperHd    := .T.
   o:cSuperHd    := " "
   o:lSpecHd     := .T.
   o:cSpecHdChar := "#"
   o:aSizeLen    := {}

   o:nHeightHead  := App.Object:H(1.1)
   o:nHeightCell  := App.Object:H(1.1)
   o:nHeightFoot  := App.Object:H(1.1)
   o:nHeightSuper := App.Object:H(1.2)

   m := AClone(o:aHead)
   FOR i := 1 TO Len(m)
       IF ";" $ o:aHead[ i ]
          o:aHead[ i ] := StrTran(o:aHead[ i ], ";", CRLF)
          t := ""            // выделим max строку массива
          FOR EACH k IN hb_ATokens(m[ i ], ";")
              IF len(k) > len(t) ; t := k
              ENDIF
          NEXT
          m[ i ] := t      // строки max длины для header расчета ширины
       ENDIF
   NEXT

   FOR EACH j, t IN o:uAlias[1], m
       k := Len(hb_valtoexp(j))   // xVal в aDim[1]
       k := Max(k, len(t)) + 2
       IF k > 10 ; k := int( k * 0.8 )
       ENDIF
       AAdd(o:aSizeLen, k )       // Len колонки
   NEXT

   o:bAfter := {|ob,op|
                 IF IsBlock(op:b_Itog_Arr)  // в др. тсб можно так делать
                    EVal(op:b_Itog_Arr, ob)
                 ENDIF
                 Return Nil
                 }

   o:b_Itog_Arr := {|ob| // ф-я расчета итогов по массиву и отображение их
                 Local aSum, aNum, nCol, oCol, cCol, nRow
                 Local nPos, aNam := {}, xVal, aLine
                 Local k := Len(ob:aArray[1])
                 aSum := array(k) ; AFill(aSum, 0)
                 aNum := array(k) ; AFill(aNum, 0)
                 nPos := 0          // надо учитывать доп. колонки
                 IF ob:nColumn("SELECTOR", .T.) > 0 ; nPos += 1
                 ENDIF
                 IF ob:nColumn("ARRAYNO" , .T.) > 0 ; nPos += 1
                 ENDIF
                 FOR EACH aLine IN ob:aArray
                     FOR EACH xVal IN aLine
                         nCol := hb_enumindex(xVal)  // номер элемента массива
                         IF !IsNumeric( xVal ) ; LOOP
                         ENDIF
                         aSum[ nCol ] += xVal       // итог
                         aNum[ nCol ] += 1          // счетчик
                     NEXT
                 NEXT
                 FOR EACH nCol, xVal IN aNum, aSum
                     IF nCol > 0        // поле числовое и есть сумма
                        nCol := hb_enumindex(nCol) + nPos // реал. колонка
                        oCol := ob:aColumns[nCol]
                        IF Empty(xVal) ; oCol:cFooting := ""
                        ELSE           ; oCol:cFooting := hb_ntos(xVal)
                        ENDIF
                     ENDIF
                 NEXT
                 ob:DrawFooters()
                 DO EVENTS
                 Return Nil
                 }

RETURN oTsb

*----------------------------------------------------------------------------*
FUNCTION Sets_ENV_my()
*----------------------------------------------------------------------------*
   LOCAL cFont := "Arial", nSize := 12, lDebug := .F., o
   LOCAL cLog  := hb_FNameDir (App.ExeName) + "_" + ;
                  hb_FNameName(App.ExeName) + ".log"
   LOCAL cNam  := hb_FNameDir (App.ExeName) + ;
                  hb_FNameName(App.ExeName) 
   //
   _SetGetLogFile( cLog ) ; hb_FileDelete( cLog ) ; SET LOGERROR ON
   //
   IF     Sys.DesktopWidth >= 1920 ; nSize += 4
   ELSEIF Sys.DesktopWidth >  1280 ; nSize += 2
   ENDIF
   //
   SET FONT TO cFont, nSize
   //
   _DefineFont("Normal"  , cFont, nSize  , .F., .F. )
   _DefineFont("Bold"    , cFont, nSize  , .T., .F. )
   _DefineFont("Italic"  , cFont, nSize-4, .F., .T. )
   // Alert* font
   _DefineFont("DlgFont" , cFont, nSize+2, .F., .F. )
   //
   ALTD(iif( lDebug, 1, 0 ))       // 1 - debug mode, 0 - no debug mode
   //
   App.Cargo:aFile := { cNam + "_1.txt", cNam + "_2.txt" }

RETURN Nil

FUNCTION Get_arr(nFile, nDubl)
   LOCAL cFil := App.Cargo:aFile[nFile]
   LOCAL cBuf := hb_memoread(cFil)
   LOCAL aBuf := {}, aHead, aName, a, i

   IF nFile == 1
      FOR EACH cBuf IN hb_ATokens(cBuf, CRLF)
          IF Empty(cBuf) ; LOOP
          ENDIF
          AAdd(aBuf, &(alltrim(cBuf)))
      NEXT
   ELSE
      aBuf  := &(cBuf)
      aHead := {"Operators", "Program;working;time", "Operator;working;time", "Button:;Log-Tsb", "Button:;Test;1", "Button:;Test;2", "Button:;Test;3", "Button:;Error", "Button:;Index", "Button:;search;by;phone", "Button:;search;by;personal;account", "Button:;search;by;address", "New;application", "Print;receipt;A35", "Print;debt;of;subscriber;A32", "Print;debt;of;subscriber;A33", "###"} 
      aName := {"COL_1", "COL_2", "COL_3", "COL_4", "COL_5", "COL_6", "COL_7", "COL_8", "COL_9", "COL_10", "COL_11", "COL_12", "COL_13", "COL_14", "COL_15", "COL_16", "COL_17"} 
   ENDIF

   IF !Empty(nDubl)  // увеличим кол-во строк для теста высоты окна, тсб
      a := AClone(aBuf)
      FOR i := 1 TO nDubl ; AEval(a, {|m| AAdd(aBuf, AClone(m)) })
      NEXT
   ENDIF

RETURN { aBuf, aHead, aName }

*----------------------------------------------------------------------------*
FUNCTION ProcNL(nVal, cMsg)
*----------------------------------------------------------------------------*
   Default cMsg := ">>> "
   nVal := iif( Empty(nVal), 0, nVal ) + 1
   cMsg += ProcName(nVal) + "(" + hb_ntos( ProcLine(nVal) ) + ")"
   cMsg += " => " + ProcFile(nVal)
RETURN cMsg

*----------------------------------------------------------------------------*
FUNCTION DrawRR( focus, nPen, t, l, b, r, cWindowName, nCurve )
*----------------------------------------------------------------------------*
   LOCAL aColor

   DEFAULT t := This.Row, l := This.Col, b := This.Height, r := This.Width
   DEFAULT focus := .F., cWindowName := ThisWindow.Name, nCurve := 5
   DEFAULT nPen  := 3

   IF ISARRAY( focus ) ; aColor := focus
   ELSE                ; aColor := iif( focus, { 0, 120, 215 }, { 100, 100, 100 } )
   ENDIF

   DRAW ROUNDRECTANGLE IN WINDOW (cWindowName)  ;
        AT t - 2, l - 2 TO t + b + 2, l + r + 2 ;
        ROUNDWIDTH  nCurve ROUNDHEIGHT nCurve   ;
        PENCOLOR  aColor PENWIDTH   nPen

RETURN Nil

*----------------------------------------------------------------------------*
FUNCTION ButtonBar(aBtn, cPref, nY, nX, nW, nH, nG, l99)
*----------------------------------------------------------------------------*
   LOCAL cTxt, nLen, nBtn, cBtn, nMsg, aNam := {}, cTool, cCapt, nButt := 0

   IF IsNumeric(cPref)      // начало, база для номерации button
      nButt := cPref
      cPref := NIL
   ENDIF

   Default cPref := "Btn_", l99 := .T.

   Default nG := App.Object:nMargWidth, aBtn := {}
   Default nY := nG, nX := nG , ;
           nW := App.Object:W1, ;
           nH := App.Object:H2

   nBtn := 0
   FOR EACH cTxt IN aBtn
       nLen := hb_enumindex(cTxt)
       IF Empty(cTxt) ; nX += nW + nG ; LOOP
       ENDIF
       nBtn := ++nBtn + nButt
       cBtn := cPref  + hb_ntos( nBtn )
       nMsg := nBtn
       IF IsArray(cTxt) ; cTool := cTxt[2] ; cCapt := cTxt[1]
       ELSE             ; cTool := NIL     ; cCapt := cTxt
       ENDIF
       @ nY, nX BUTTONEX &cBtn WIDTH nW HEIGHT nH CAPTION cCapt ;
                TOOLTIP  cTool                                  ;
                NOHOTLIGHT NOXPSTYLE HANDCURSOR NOTABSTOP       ;
                ACTION ( This.Enabled := .F., _wPost(This.Cargo,, This.Name) )
       AAdd(aNam, cBtn)
       nX += This.&(cBtn).Width + nG
       This.&(cBtn).Cargo   := iif( l99 .and. nLen == Len(aBtn), 99, nMsg )
       This.Cargo:cBtn_Exit := cBtn
   NEXT

RETURN aNam
