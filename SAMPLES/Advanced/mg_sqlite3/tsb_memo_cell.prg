/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2024 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * ќкно дл€ мемо-edit / Window for memo-edit  // 10.10.24
 */
#define _HMG_OUTLOG

#include "hmg.ch"

////////////////////////////////////////////////////////////////////////////
FUNCTION CellEditMemo(uVal, oBrw)   // создать окно редактировани€ memo
   LOCAL cFont, nFSize, hFont, aFont, cForm, cFType, nFLn, o
   LOCAL cHelp, nW, nH, nCol, oCol, aBrw, oCell, nWCell, nHCell
   LOCAL nWTsb, nY, nX, nG, cTyp, aRetVal, nWBtn, nHBtn, nHIco
   LOCAL oWnd := _WindowObj(oBrw:cParentWnd)
   LOCAL lMod := _HMG_IsModalActive
   LOCAL hMod := _HMG_ActiveModalHandle

   aRetVal := {}
   cForm   := "Form_Memo_Cell"
   hFont   := GetFontHandle( "TsbEdit" )
   aFont   := GetFontParam(hFont)
   cFont   := aFont[1]
   nFSize  := aFont[2]
   nCol    := oBrw:nCell
   oCol    := oBrw:aColumns[ nCol ]
   cFType  := oCol:cFieldTyp           // тип пол€
   nFLn    := oCol:nFieldLen           // длина пол€
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
      //ON LOSTFOCUS _wSend(92) - не использую

      This.Cargo := oHmgData()
      This.Cargo:oWnd   := oWnd            // окно родитель со своим Cargo
      This.Cargo:cForm  := cForm           // это окно
      This.Cargo:oBrw   := oBrw            // тсб со своим Cargo
      This.Cargo:uVal   := uVal            // значение €чейки на входе
      This.Cargo:cTyp   := cTyp            // тип значени€ €чейки на входе
      This.Cargo:cFType := cFType          // тип правки пол€ базы данных
      This.Cargo:nFLn   := nFLn            // длина пол€ правки пол€ базы данных
      This.Cargo:nG     := nG              // отступ на окне


      nW := This.ClientWidth
      nH := This.ClientHeight
      nY := nG

      @ nY, nG EDITBOX Edit_Memo WIDTH nW-nG*2 HEIGHT nFSize*12 ;
        VALUE uVal                                              ;
        BACKCOLOR {240, 240, 240} FONTCOLOR BLUE NOTABSTOP      ;
        ON LOSTFOCUS _wSend(92)

      This.Cargo:cObj := "Edit_Memo"  // запомним на окне

      nY += This.Edit_Memo.Height + nG

      nWBtn := 200
      IF nWBtn*2 + nG*3 > nW-5
         nWBtn := ( nW - 5 ) / 3
      ENDIF
      nHBtn := 32 + nG
      nHIco := 32
      nX    := (nW - nWBtn*2 - nG*3) /2
      @ nY, nX BUTTONEX Btn_Ok WIDTH nWBtn HEIGHT nHBtn CAPTION "F2-write" ;
        ICON "iOk32" NOHOTLIGHT NOXPSTYLE HANDCURSOR ACTION {|| _wPost(2) }

      This.Btn_Ok.ImageWidth  := nHIco
      This.Btn_Ok.ImageHeight := nHIco
      This.Btn_Ok.Icon        := LoadIconByName( "iOk32", nHIco, nHIco )
      This.Cargo:cObj1        := "Btn_Ok"  // запомним на окне
      This.Cargo:nWBtn        := nWBtn     // запомним на окне
      This.Cargo:nHBtn        := nHBtn     // запомним на окне

      nX += This.Btn_Ok.Width + nG*2

      @ nY, nX BUTTONEX Btn_Esc WIDTH nWBtn HEIGHT nHBtn CAPTION "Esc-exit" ;
        ICON "iCancel32" NOHOTLIGHT NOXPSTYLE HANDCURSOR ACTION {|| _wPost(99) }

      This.Btn_Esc.ImageWidth  := nHIco
      This.Btn_Esc.ImageHeight := nHIco
      This.Btn_Esc.Icon        := LoadIconByName( "iCancel32", nHIco, nHIco )
      This.Cargo:cObj2         := "Btn_Esc"  // запомним на окне

      nY += This.Btn_Ok.Height + nG*3

      This.Height    := nY + nG
      This.MinWidth  := nW + GetBorderWidth()*2   // блокировка уменьшени€ размеров окна !!!
      This.MinHeight := nY + nG                   // блокировка уменьшени€ размеров окна !!!

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
                        Local nLen, cVal := This.Edit_Memo.Value
                        Local nFLn := ow:Cargo:nFLn    // длина пол€ правки пол€
                        ? ProcNL(), "["+cVal+"]", "["+ow:Cargo:uVal+"]"
                        IF cVal # ow:Cargo:uVal
                           // тип правки пол€ базы данных - это не дл€ пол€ "M"
                           // в поле "C" могут быть знаки CRLF
                           IF ow:Cargo:cFType == "C"
                              nLen := LEN( ALLTRIM(cVal) )
                              IF nLen > nFLn
                                 //AlertStop("ƒлина строки ЅќЋ№Ў≈ длины пол€ !;"+;
                                 //          "—троку приводим к длине пол€ !;;" + ProcNL(),,,64,{RED})
                                 AlertStop("String length is GREATER than field length!;"+;
                                           "We convert the string to the field length!;;" + ProcNL(),,,64,{RED})
                              ENDIF
                           ENDIF
                           aRetVal := {cVal}      // вернуть правку значение €чейки
                        ELSE
                          aRetVal := {}
                        ENDIF
                        _wPost(99, ow:Name)
                        Return Nil
                  } )
      o:Event(90, {|ow,ky| // ON Release
                           ? "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                           ?  Repl(".", 10), "=> RELEASE WINDOW <=", ow:Name
                           Return Nil
                           })
      /*
      o:Event(92, {|ow| // ON LOSTFOCUS
                        Local cVal := This.Edit_Memo.Value
                        Local cOld := ow:Cargo:uVal
                        ? "«апрос на запись / Write request", cVal, cOld
                        _wPost(99, ow:Name)
                        Return Nil
                        } )
      */
      o:Event(99, {|ow| ow:Release()        })

   END WINDOW

   ACTIVATE WINDOW &cForm

   _HMG_IsModalActive     := lMod
   _HMG_ActiveModalHandle := hMod

RETURN aRetVal   // всегда массив, если пусто - значит отказ от ввода

///////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION ResizeMemoForm( owc )
   LOCAL nW, nH, nG, nH2, nX, nWBtn, cObj, cObj1, cObj2, cForm

   nW    := This.ClientWidth
   nH    := This.ClientHeight
   cForm := owc:cForm
   nG    := owc:nG
   nH2   := owc:nHBtn
   nWBtn := owc:nWBtn

   cObj  := owc:cObj   // объект EditBox изменить
   IF _IsControlDefined(cObj, cForm)
      This.&(cObj).Height := nH - nG*3 - nH2
      This.&(cObj).Width  := nW - nG*2
   ENDIF

   cObj1 := owc:cObj1  // объект Btn1 изменить
   IF _IsControlDefined(cObj1, cForm)
      nX := (nW - nWBtn*2 - nG*3) /2
      This.&(cObj1).Row  := nH - nH2 - nG
      This.&(cObj1).Col  := nX
   ENDIF

   cObj2 := owc:cObj2  // объект Btn2 изменить
   IF _IsControlDefined(cObj2, cForm)
      nX := (nW - nWBtn*2 - nG*3) / 2 + nWBtn + nG*2
      This.&(cObj2).Row  := nH - nH2 - nG
      This.&(cObj2).Col  := nX
   ENDIF

   DO EVENTS

RETURN NIL

