/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2024 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * ќкно дл€ мемо-edit / Window for memo-edit
 */
#define _HMG_OUTLOG

#include "hmg.ch"

////////////////////////////////////////////////////////////////////////////
FUNCTION CellEditMemo(uVal, oBrw)   // создать окно редактировани€ memo
   LOCAL cFont, nFSize, hFont, aFont, cForm, o
   LOCAL cHelp, nW, nH, nCol, oCol, aBrw, oCell, nWCell, nHCell
   LOCAL nWTsb, nY, nX, nG, cTyp, cRetVal
   LOCAL oWnd := _WindowObj(oBrw:cParentWnd)
   LOCAL lMod := _HMG_IsModalActive
   LOCAL hMod := _HMG_ActiveModalHandle

   cRetVal := ""
   cForm   := "Form_Memo_Cell"
   hFont   := GetFontHandle( "TsbEdit" )
   aFont   := GetFontParam(hFont)
   cFont   := aFont[1]
   nFSize  := aFont[2]
   nCol    := oBrw:nCell
   oCol    := oBrw:aColumns[ nCol ]
   cHelp   := "F2-write,  Esc-exit"
   cTyp    := VALTYPE(uVal)
   IF cTyp # "C"  // такое бывает
      uVal := cValToChar(uVal)
   ENDIF
   // координаты €чейки
   aBrw   := {0,0,0,0}
   GetWindowRect( oBrw:hWnd, aBrw )
   oCell  := oBrw:GetCellinfo( oBrw:nRowPos, oBrw:nCell, .F. )
   nX     := oCell:nCol + aBrw[ 1 ] - oBrw:nLeft
   nY     := oCell:nRow + aBrw[ 2 ] - oBrw:nTop - oBrw:nHeightCell
   nWCell := oCell:nWidth
   nHCell := oCell:nHeight
   nW     := oCell:nWidth
   nH     := 250            // произвольна€ высота, потом уменьшим
   nG     := 5
   nWTsb  := oBrw:nWidth - oBrw:nLeft

   _HMG_IsModalActive     := .F.
   _HMG_ActiveModalHandle := 0

   nW += 150  // добавим

   IF oCell:nCol + nW > nWTsb
      nX := nWTsb - nW + 5
   ENDIF

   // если ставим NOSIZE, то - 5 не надо
   DEFINE WINDOW &cForm AT nY + nHCell, nX-5 CLIENTAREA nW-5, nH ;
      MINWIDTH 150 MINHEIGHT 150                                 ;
      CHILD NOSYSMENU NOCAPTION BACKCOLOR RED                    ;
      FONT cFont SIZE nFSize                                     ;
      ON SIZE {|| ResizeMemoForm( This.Cargo ) }                 ;
      ON INIT {|| _wPost(0) }                                    ;
      ON RELEASE _wSend(90)
      //ON LOSTFOCUS _wSend(92)

      This.Cargo := oHmgData()
      This.Cargo:oWnd := oWnd            // окно родитель со своим Cargo
      This.Cargo:oBrw := oBrw            // тсб со своим Cargo
      This.Cargo:uVal := uVal            // значение €чейки на входе
      This.Cargo:cTyp := cTyp            // тип значени€ €чейки на входе
      This.Cargo:nG   := nG              // отступ на окне

      nW := This.ClientWidth
      nH := This.ClientHeight
      nY := nG

      @ nY, nG EDITBOX Edit_Memo WIDTH nW-nG*2 HEIGHT nFSize*12 ;
        VALUE uVal                                              ;
        BACKCOLOR {240, 240, 240} FONTCOLOR BLUE NOTABSTOP      ;
        ON LOSTFOCUS _wSend(92)

      nY += This.Edit_Memo.Height
      This.Cargo:cObj := "Edit_Memo"

      @ nY, 0 LABEL Label_1 WIDTH nW HEIGHT nFSize*2 VALUE cHelp ;
        FONTCOLOR YELLOW BOLD VCENTERALIGN CENTERALIGN TRANSPARENT
      nY += This.Label_1.Height
      This.Cargo:cObj2  := "Label_1"
      This.Cargo:nHObj2 := This.Label_1.Height

      This.Height    := nY + nG
      This.MinWidth  := nW + GetBorderWidth()*2      // блокировка уменьшени€ размеров окна !!!
      This.MinHeight := nY + nG                      // блокировка уменьшени€ размеров окна !!!

      ON KEY F1     ACTION NIL
      ON KEY F2     ACTION _wPost(2)
      ON KEY ESCAPE ACTION _wPost(99)

      o := This.Object
      o:Event( 0, {|ow| // ON INIT
                        Local nWDsk := Sys.ClientHeight
                        Local nHWin := ow:Row + ow:Height
                        IF nHWin > nWDsk
                           ow:Row := nWDsk - ow:Height
                        ENDIF
                        This.Topmost := .F.
                        ow:SetFocus('Edit_Memo')
                        Return Nil
                        } )
      o:Event( 2, {|ow| // SAVE + EXIT
                        Local cFTp, cVal := This.Edit_Memo.Value
                        Local nFLn, nLen, oCol, cFld, oBrw := ow:Cargo:oBrw
                        oCol := oBrw:aColumns[ oBrw:nCell ]
                        cFld := oCol:cField
                        nFLn := oCol:nFieldLen
                        cFTp := oCol:cFieldTyp    // доп.проверка на тип пол€
                        IF cVal # ow:Cargo:uVal
                           IF cFTp == "C"
                              nLen := LEN( ALLTRIM(cVal) )
                              IF nLen > nFLn
                                 AlertStop("ƒлина строки ЅќЋ№Ў≈ длины пол€ !;"+;
                                           "—троку приводим к длине пол€ !;;" + ProcNL())
                              ENDIF
                           ENDIF
                           cRetVal := cVal       // вернуть правку значение €чейки
                        ELSE
                          cRetVal := ""
                        ENDIF
                        _wPost(99, ow:Name)
                        Return Nil
                  } )
      o:Event(90, {|ow,ky| // ON Release
                           ? "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                           ?  Repl(".", 10), "=> RELEASE WINDOW <=", ow:Name
                           Return Nil
                           })

      o:Event(92, {|ow| // ON LOSTFOCUS
                        Local cVal := This.Edit_Memo.Value
                        Local cOld := ow:Cargo:uVal
                        ? "«апрос на запись / Write request", cVal, cOld
                        _wPost(99, ow:Name)
                        Return Nil
                        } )
      o:Event(99, {|ow| ow:Release()        })

   END WINDOW

   ACTIVATE WINDOW &cForm

   _HMG_IsModalActive     := lMod
   _HMG_ActiveModalHandle := hMod

RETURN cRetVal

///////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION ResizeMemoForm( owc )
   LOCAL nW, nH, nG, nH2

   nW  := This.ClientWidth
   nH  := This.ClientHeight
   nG  := owc:nG
   nH2 := owc:nHObj2

   IF LEN(owc:cObj) > 0
      This.&(owc:cObj).Height := nH - nG*2 - nH2
      This.&(owc:cObj).Width  := nW - nG*2
   ENDIF

   IF LEN(owc:cObj2) > 0
      This.&(owc:cObj2).Row    := nH - nH2
      This.&(owc:cObj2).Width  := nW
   ENDIF

   DO EVENTS

RETURN NIL

