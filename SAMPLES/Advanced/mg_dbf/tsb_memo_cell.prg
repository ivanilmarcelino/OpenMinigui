/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2024 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * ���� ��� ����-edit / Window for memo-edit  // 10.10.24
 */
#define _HMG_OUTLOG

#include "hmg.ch"

////////////////////////////////////////////////////////////////////////////
FUNCTION CellEditMemo(uVal, oBrw)   // ������� ���� �������������� memo
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
   cFType  := oCol:cFieldTyp           // ��� ����
   nFLn    := oCol:nFieldLen           // ����� ����
   cHelp   := "F2-write,  Esc-exit"
   cTyp    := VALTYPE(uVal)
   IF cTyp # "C"  // ����� ������
      uVal := cValToChar(uVal)
   ENDIF
   // ���������� ������
   aBrw   := {0,0,0,0}
   GetWindowRect( oBrw:hWnd, aBrw )
   oCell  := oBrw:GetCellinfo( oBrw:nRowPos, oBrw:nCell, .F. )
   nX     := oCell:nCol + aBrw[ 1 ] - oBrw:nLeft
   nY     := oCell:nRow + aBrw[ 2 ] - oBrw:nTop - oBrw:nHeightCell
   nWCell := oCell:nWidth
   nHCell := oCell:nHeight
   nW     := oCell:nWidth
   nH     := 250            // ������������ ������, ����� ��������
   nG     := 5
   nWTsb  := oBrw:nWidth - oBrw:nLeft

   _HMG_IsModalActive     := .F.
   _HMG_ActiveModalHandle := 0

   nW += 150  // �������

   IF oCell:nCol + nW > nWTsb
      nX := nWTsb - nW + 5
   ENDIF

   // ���� ������ NOSIZE, �� - 5 �� ����
   DEFINE WINDOW &cForm AT nY + nHCell, nX-5 CLIENTAREA nW-5, nH ;
      MINWIDTH 150 MINHEIGHT 150                                 ;
      CHILD NOSYSMENU NOCAPTION BACKCOLOR RED                    ;
      FONT cFont SIZE nFSize                                     ;
      ON SIZE {|| ResizeMemoForm( This.Cargo ) }                 ;
      ON INIT {|| _wPost(0) }                                    ;
      ON RELEASE _wSend(90)
      //ON LOSTFOCUS _wSend(92) - �� ���������

      This.Cargo := oHmgData()
      This.Cargo:oWnd   := oWnd            // ���� �������� �� ����� Cargo
      This.Cargo:cForm  := cForm           // ��� ����
      This.Cargo:oBrw   := oBrw            // ��� �� ����� Cargo
      This.Cargo:uVal   := uVal            // �������� ������ �� �����
      This.Cargo:cTyp   := cTyp            // ��� �������� ������ �� �����
      This.Cargo:cFType := cFType          // ��� ������ ���� ���� ������
      This.Cargo:nFLn   := nFLn            // ����� ���� ������ ���� ���� ������
      This.Cargo:nG     := nG              // ������ �� ����


      nW := This.ClientWidth
      nH := This.ClientHeight
      nY := nG

      @ nY, nG EDITBOX Edit_Memo WIDTH nW-nG*2 HEIGHT nFSize*12 ;
        VALUE uVal                                              ;
        BACKCOLOR {240, 240, 240} FONTCOLOR BLUE NOTABSTOP      ;
        ON LOSTFOCUS _wSend(92)

      This.Cargo:cObj := "Edit_Memo"  // �������� �� ����

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
      This.Cargo:cObj1        := "Btn_Ok"  // �������� �� ����
      This.Cargo:nWBtn        := nWBtn     // �������� �� ����
      This.Cargo:nHBtn        := nHBtn     // �������� �� ����

      nX += This.Btn_Ok.Width + nG*2

      @ nY, nX BUTTONEX Btn_Esc WIDTH nWBtn HEIGHT nHBtn CAPTION "Esc-exit" ;
        ICON "iCancel32" NOHOTLIGHT NOXPSTYLE HANDCURSOR ACTION {|| _wPost(99) }

      This.Btn_Esc.ImageWidth  := nHIco
      This.Btn_Esc.ImageHeight := nHIco
      This.Btn_Esc.Icon        := LoadIconByName( "iCancel32", nHIco, nHIco )
      This.Cargo:cObj2         := "Btn_Esc"  // �������� �� ����

      nY += This.Btn_Ok.Height + nG*3

      This.Height    := nY + nG
      This.MinWidth  := nW + GetBorderWidth()*2   // ���������� ���������� �������� ���� !!!
      This.MinHeight := nY + nG                   // ���������� ���������� �������� ���� !!!

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
                        Local nFLn := ow:Cargo:nFLn    // ����� ���� ������ ����
                        ? ProcNL(), "["+cVal+"]", "["+ow:Cargo:uVal+"]"
                        IF cVal # ow:Cargo:uVal
                           // ��� ������ ���� ���� ������ - ��� �� ��� ���� "M"
                           // � ���� "C" ����� ���� ����� CRLF
                           IF ow:Cargo:cFType == "C"
                              nLen := LEN( ALLTRIM(cVal) )
                              IF nLen > nFLn
                                 //AlertStop("����� ������ ������ ����� ���� !;"+;
                                 //          "������ �������� � ����� ���� !;;" + ProcNL(),,,64,{RED})
                                 AlertStop("String length is GREATER than field length!;"+;
                                           "We convert the string to the field length!;;" + ProcNL(),,,64,{RED})
                              ENDIF
                           ENDIF
                           aRetVal := {cVal}      // ������� ������ �������� ������
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
                        ? "������ �� ������ / Write request", cVal, cOld
                        _wPost(99, ow:Name)
                        Return Nil
                        } )
      */
      o:Event(99, {|ow| ow:Release()        })

   END WINDOW

   ACTIVATE WINDOW &cForm

   _HMG_IsModalActive     := lMod
   _HMG_ActiveModalHandle := hMod

RETURN aRetVal   // ������ ������, ���� ����� - ������ ����� �� �����

///////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION ResizeMemoForm( owc )
   LOCAL nW, nH, nG, nH2, nX, nWBtn, cObj, cObj1, cObj2, cForm

   nW    := This.ClientWidth
   nH    := This.ClientHeight
   cForm := owc:cForm
   nG    := owc:nG
   nH2   := owc:nHBtn
   nWBtn := owc:nWBtn

   cObj  := owc:cObj   // ������ EditBox ��������
   IF _IsControlDefined(cObj, cForm)
      This.&(cObj).Height := nH - nG*3 - nH2
      This.&(cObj).Width  := nW - nG*2
   ENDIF

   cObj1 := owc:cObj1  // ������ Btn1 ��������
   IF _IsControlDefined(cObj1, cForm)
      nX := (nW - nWBtn*2 - nG*3) /2
      This.&(cObj1).Row  := nH - nH2 - nG
      This.&(cObj1).Col  := nX
   ENDIF

   cObj2 := owc:cObj2  // ������ Btn2 ��������
   IF _IsControlDefined(cObj2, cForm)
      nX := (nW - nWBtn*2 - nG*3) / 2 + nWBtn + nG*2
      This.&(cObj2).Row  := nH - nH2 - nG
      This.&(cObj2).Col  := nX
   ENDIF

   DO EVENTS

RETURN NIL

