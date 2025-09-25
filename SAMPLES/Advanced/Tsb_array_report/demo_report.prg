/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2025 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Показ массива в _TBrowse() / Show array in _TBrowse()
 * Расчёт ширины колонок по массиву / Calculating column widths from an array 
*/
#define _HMG_OUTLOG

#include "hmg.ch"
#include "tsbrowse.ch"
/////////////////////////////////////////////////////////////////////////
FUNCTION Table_Rprt(oWnd, a4Dim ) 
   LOCAL oTsb, owc, cNam, nY, nX, nH, nW, nG, nTbl, cTtl, aBClr, cFocus
   LOCAL oBrw, aTbClr, cSpHd, a3Clr, cForm
   LOCAL aXDim, aTbl, cTime

   ? ProcNL(), oWnd, a4Dim
   aXDim := a4Dim[1]
   aTbl  := a4Dim[2]
   cTime := a4Dim[3]
   cForm := "w" + a4Dim[4]

   IF _IsWindowDefined( cForm )
      oWnd:Cargo:cFocWnd := cForm
      IF IsIconic( nH := GetFormHandle(cForm) ) ; _Restore( nH )
      ENDIF
      DoMethod(cForm, "SetFocus")
      RETURN NIL
   ENDIF

   a3Clr  := { { {184,107,228}, {244,202,242}, {238,130,238} } ,;       
               { { 90,217,217}, {192,217,217}, {146,244,244} } ,;       
               { {197,17 ,98} , {207,86,141} , {244,244,244} } ,;        
               { {181,172,98} , {230,222,152}, {209,199,133} }   }        

   nTbl   := aTbl[1]
   cTtl   := aTbl[2]
   cNam   := "Run_S_"
   cFocus := "Buff"
   nY     := (nTbl-1) * 50
   nX     := (nTbl-1) * 50
   nW     := Sys.ClientWidth  - nX
   nH     := Sys.ClientHeight - nY
   nG     := 20 
   aBClr  := a3Clr[nTbl,1] 
   //         window     line-1         line-2       aBrush
   aTbClr := { aBClr , a3Clr[nTbl,2], a3Clr[nTbl,3], aBClr }
   cSpHd  := "Calculation results of the function: " + a4Dim[4] + "(..)"
   cSpHd  += SPACE(10) + cTime

   // вернуть в главную форму / return to main form
   oWnd:Cargo:cRetForm := cForm

   DEFINE WINDOW &cForm AT nY, nX WIDTH nW HEIGHT nH          ;
          TITLE cTtl + "  Report TBrowse. STANDARD " + cForm  ;
          WINDOWTYPE STANDARD TOPMOST NOMAXIMIZE NOSIZE       ;
          BACKCOLOR aBClr                                     ;
          ON INIT    ( This.Topmost := .F., _wPost( 0) )      ;
          ON RELEASE ( _wSend(90) )
          This.Cargo := oHmgData() ; owc := This.Cargo

      owc:oParent := oWnd
      owc:cFocus  := cFocus

      @ 0,0 LABEL &cFocus VALUE "" WIDTH 2 HEIGHT 2 TRANSPARENT

      ButtonBar({"Print", "Excel", "Exit"}, cNam, , , , nG*2, nG)

      nY := nG + nG * 3
      nX := nG
      nW := This.ClientWidth  - nG*2
      nH := This.ClientHeight - nY - nG

      ////////////////////////////////////////////////////////////////////////
      oTsb := TablePatam( cForm, aXDim, "cTable", aTbClr, nW, cSpHd)
      //? _o2log(oTsb, 27, ProcNL() + "  oTsb => ", .T. ) // check in log
      // function in library \MiniGUI\SOURCE\TsBrowse\h_controlmisc2.prg
      oBrw := _TBrowse( oTsb, aXDim, "cTable", nY, nX, nW, nH )
      This.Cargo:oBrw := oBrw

      ON KEY F1     ACTION NIL
      ON KEY ESCAPE ACTION {|ob| ob := ThisWindow.Cargo:oBrw, ;
                            iif( ob:IsEdit, ob:SetFocus(), _wSend(99) ) }

      WITH OBJECT This.Object
         :Event( 0, {|ow      | /*ow:SetFocus(ow:Cargo:cFocus)*/ ow:Cargo:oBrw:SetFocus() })
         :Event( 1, {|ow,ky,cn|
                      SET WINDOW THIS TO ow:Name
                      MsgDebug(ow:Name,ky,cn,This.&(cn).Caption)
                      SET WINDOW THIS TO
                      This.&(cn).Enabled := .T.
                      ow:SetFocus(ky := ow:Cargo:cFocus)
                      DO EVENTS
                      Return Nil
                      })
         :Event( 2, {|ow,ky,cn|
                      SET WINDOW THIS TO ow:Name
                      MsgDebug(ow:Name,ky,cn,This.&(cn).Caption)
                      SET WINDOW THIS TO
                      This.&(cn).Enabled := .T.
                      ow:SetFocus(ky := ow:Cargo:cFocus)
                      DO EVENTS
                      Return Nil
                      })
         :Event(90, {|ow,ky|
                      ? ProcNL(), ">>> ON RELEASE: "+ow:Name+":Event("+hb_ntos(ky)+")"
                      Return Nil
                      })
         :Event(99, {|ow| ow:Release() })
      END WITH

   END WINDOW

   //ACTIVATE WINDOW &cForm NOWAIT
   //ACTIVATE WINDOW &cForm 

RETURN cForm

////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION TablePatam(cForm,aXDim,cBrw,aIniClr,nWTsb,cSpHTtl)
   LOCAL oTsb, nClr1, nClr2, a, nHFnt, nI, aHead, aName, nHCell, aWSize, nWNum

   oTsb := oHmgData()
   oTsb:cBrw           := cBrw
   oTsb:cForm          := cForm      // <--- обязательно так !!!
   oTsb:cFormName      := cForm      // или так
   oTsb:lNoPicture     := .T.
   //                         cell      Head        Foot     SpecHider  SuperHider   Edit
   oTsb:aFont          := { "DlgFont", "DlgFont", "DlgFont", "DlgFont" , "DlgFont", "DlgFont" }
   //oTsb:aNumber      := NIL                                    // не ставить колонку нумерации
   nWNum               := GetFontWidth(oTsb:aFont[4], 4)
   oTsb:aNumber        := { 1, nWNum }                         // колонка нумерации и её ширина
   oTsb:lFooting       := .T.                                  // поставить в таблице подвал
   oTsb:lSuperHd       := .T.                                  // поставить в таблице суперхидер
   oTsb:cSuperHd       := cSpHTtl                            
   //oTsb:uSelector    := 20                                 
   oTsb:lFooting       := .T.                                  // поставить в таблице подвал
   oTsb:aFoot          := .T.                                  // заполнить подвал
   nHFnt               := INT( GetFontHeight(oTsb:aFont[1]) )  // только целые числа
   nHCell              := INT( nHFnt * 1.4 )                   // только целые числа
   oTsb:nHeightCell    := nHCell                               // высота ячеек
   oTsb:nHeightHead    := 1 //nHFnt                            // высота шапки
   oTsb:nHeightFoot    := nHFnt                                // высота подвала
   oTsb:nHeightSpecHd  := nHFnt                                // высота нумератора
   oTsb:nHeightSuperHd := nHFnt * 2                            // высота суперхидера

   nClr1 := HMG_RGB2n(aIniClr[1])                              // цвет фона шапка+подвал
   nClr2 := RGB( 48, 29,26)                                    // серо-черный фон
   oTsb:aSuperHdColor  := {CLR_YELLOW, { nClr1, nClr2 } }      // цвет: текст и фон суперхидера
   oTsb:aBrush         := aIniClr[4]                           // цвет фона под таблицей

   // цвета в таблицу
   a := {}
   // 1 , текста ячеек
   AAdd(a, { CLR_TEXT, CLR_BLACK } )                // 1 , текста ячеек
   // 2 , фона в ячейках таблицы
   //AAdd(a, { CLR_PANE, {|nr,nc,ob,nd| nr := CLR_BLACK, nc := CLR_WHITE, nd := CLR_HGRAY, ;
   //                      iif( (ob:cAlias)->(DELETED()), nr, iif( ob:nAt % 2 == 0, nc, nd ) ) } } )
   AAdd(a, { CLR_HEADF, CLR_WHITE                })  // 3 , текста шапки таблицы
   AAdd(a, { CLR_HEADB, { nClr2, nClr1 }         })  // 4 , фона шапки таблицы
   AAdd(a, { CLR_FOCUSB, {|a,b,c| a := b, If( c:nCell == b, -CLR_HRED, -CLR_BLUE ) } } ) // 6 , фона курсора
   AAdd(a, { CLR_EDITF, CLR_YELLOW               })  // 7 , текста редактируемого поля
   AAdd(a, { CLR_EDITB, CLR_HRED                 })  // 8 , фона редактируемого поля
   AAdd(a, { CLR_FOOTF, CLR_WHITE                })  // 9 , текста подвала таблицы
   AAdd(a, { CLR_FOOTB, { nClr1, nClr2 }         })  // 10, фона подвала таблицы
   AAdd(a, { CLR_SPCF , CLR_YELLOW               })  // 18, specheader text - нумератор
   AAdd(a, { CLR_SPCB , { nClr1, nClr2 }         })  // 19, specheader back - нумератор
   oTsb:aColorAdd := a
   oTsb:lZebra    := .T.
   //oTsb:aZebra  := { {230,230,230}, SILVER }    // серый
   oTsb:aZebra    := { HMG_RGB2n(aIniClr[2]), HMG_RGB2n(aIniClr[3]) }

   // table header and column titles
   IF ! IsArray(oTsb:aHead)
      a := aXDim[1]
      aHead  := {}
      aName  := {}
      FOR nI := 1 TO LEN(a)
         AADD( aHead, "" ) 
         AADD( aName, "COL_" + HB_ValToExp(nI) )
      NEXT
      oTsb:aHead := aHead
   ENDIF
   oTsb:aName    := aName                                   
   oTsb:aHideCol := {}                                           // скрыть колонки, учитываем SELECTOR и ARRAYNO
   aWSize        := CalculatColumnWidths(aXDim,0,nWTsb,nWNum)    // подсчёт ширины колонок - показ всех колонок
   oTsb:aSize    := aWSize                                       // назначим ширину колонок для ТСБ

   // Проверка колонок таблицы / Checking table columns
   //  ? "oTsb:aField=", oTsb:aField ; ?v oTsb:aField
   //  ? "oTsb:aName= ", oTsb:aName  ; ?v oTsb:aName
   //  ? "oTsb:aPict=" , oTsb:aPict  ; ?v oTsb:aPict
   //  ? "oTsb:aHead= ", oTsb:aHead  ; ?v oTsb:aHead
   //  ? "oTsb:aFoot= ", oTsb:aFoot  ; ? IIF( IsArray(oTsb:aFoot), HB_ValToExp(oTsb:aFoot), oTsb:aFoot )
   //  ? "oTsb:aSize= ", oTsb:aSize  ; ?v oTsb:aSize
   //  ? "oTsb:aAlign=", oTsb:aAlign //; ?v oTsb:aAlign

   // такой порядок работы блоков кода / this is the order of the code blocks
   oTsb:bInit := {|ob,op| // TSB initialization
                   Local ox := op // oTsb
                   //ob:Hide()                                    // скрыть таблицу для дальнейшей прорисовки
                   //ob:HideColumns( op:aHideCol ,.t.)            // скрыть колонки
                   //? "### oTsb:bInit", ProcNL(), HB_ValToExp(op:aHideCol)
                   ob:nFreeze     := ob:nColumn("ARRAYNO") + 1    // Заморозить столбцы
                   ob:lLockFreeze := .T.                          // Избегать прорисовки курсора на замороженных столбцах
                   ob:nCell       := ob:nFreeze + 1               // передвинуть курсор
                   ob:lNoKeyChar  := .F.                          // ввод в ячейки от букв, цифр
                   //myTsbEdit(ob,op)                             // Editing table cells
                   ox:Rezerv := "Rezerv"
                   Return Nil
                   }

   oTsb:bBody := {|ob,op| // other TSB settings
                   Local oc, i := 0
                   Local nBClr := GetSysColor( COLOR_BTNFACE )   // цвет системный
                   Local hFont := GetFontParam(op:aFont[4])      // SpecHider
                   hFont := ob:aColumns[1]:hFontSpcHd            // 4-special header font - или так
                   ob:lNoHScroll  := .T.   // нет показа горизонтального скролинга
                   ob:oHScroll    := NIL
                   FOR EACH oc IN ob:aColumns
                      oc:cSpcHeading := NIL
                      IF oc:cName == "ARRAYNO"
                         oc:cSpcHeading := "#"
                         oc:nClrBack    := nBClr            // RGB(240,240,240) // изменение цвета фона виртуальной колонки
                         oc:nClrFore    := CLR_RED          // изменение цвета текста виртуальной колонки
                         oc:hFont       := hFont            // изменение фонта виртуальной колонки
                         oc:bDecode     := {|cv| Alltrim(cv) }
                         oc:nAlign      := DT_CENTER
                      ELSE
                         oc:cSpcHeading := hb_ntos( ++i )
                      ENDIF
                   NEXT
                   //? "### oTsb:bBody", ProcNL(), ob:nHeightSuper, ob:lDrawSuperHd, ;
                   //  "Head=",ob:nHeightHead, "Foot",ob:nHeightFoot, op:cSuperHd
                   Return Nil
                   }

RETURN oTsb

///////////////////////////////////////////////////////////////////
// расчёт ширины колонок / calculating column widths
STATIC FUNCTION CalculatColumnWidths(aXDim,nCol,nWTsb,nWNum)
   LOCAL aDim, v, a, i, hFont, nW, aWSize

   aDim   := ACLONE(aXDim)
   hFont  := GetFontHandle("DlgFont")
   aWSize := Array(Len(aDim[1]))
   aFill(aWSize, 0)

   FOR EACH a IN aDim
      FOR EACH v IN a
         i := hb_enumindex(v)
         // показ только 2 колонок
         //IF i > nCol ; LOOP
         //ENDIF
         IF !IsChar(v) ; v := cValToChar(v)
         ENDIF
         v  += "HH"  // добавка
         nW := GetTextWidth( Nil, v, hFont )
         aWSize[ i ] := MAX(nW,aWSize[ i ])
         //IF i > nCol 
         //  aWSize[ i ] := 0
         //ENDIF
      NEXT
   NEXT

   nW := 0
   FOR i := 1 TO nCol - 1
      nW += aWSize[ i ]
   NEXT

   //oTsb:aNumber := { 1, 30 }     nWNum        // колонка нумерации и её ширина
   nCol := LEN(aWSize)
   // для колонки nCol делаем всю ширину экрана показа, за минусом колонки 1
   aWSize[nCol] := nWTsb - nW - GetHScrollBarHeight() - nWNum - 1

RETURN aWSize

/////////////////////////////////////////////////////////////////////////
FUNCTION ButtonBar(aBtn, cPref, nY, nX, nW, nH, nG, l99)
   LOCAL cTxt, nBtn, cBtn, aNam := {}, cTool, cCapt
   Default cPref := "Btn_", l99 := .T.

   Default nG := 15, aBtn := {}, nBtn := 0
   Default nY := nG, nX := nG , ;
           nW := App.Object:W1, ;
           nH := App.Object:H2

   FOR EACH cTxt IN aBtn
       nBtn  := hb_enumindex(cTxt)
       IF IsArray(cTxt) ; cTool := cTxt[2] ; cCapt := cTxt[1]
       ELSE             ; cTool := NIL     ; cCapt := cTxt
       ENDIF
       cBtn := cPref + hb_ntos( nBtn )
       @ nY, nX BUTTONEX &cBtn WIDTH nW HEIGHT nH CAPTION cCapt ;
                TOOLTIP  cTool                                  ;
                NOHOTLIGHT NOXPSTYLE HANDCURSOR NOTABSTOP       ;
                ACTION ( This.Enabled := .F., _wPost(This.Cargo,, This.Name) )
       This.&(cBtn).Cargo := iif( l99 .and. nBtn == Len(aBtn), 99, nBtn )
       nX += This.&(cBtn).Width + nG
       This.Cargo:cBtn_Exit := cBtn
       AAdd(aNam, cBtn)
   NEXT

RETURN aNam
