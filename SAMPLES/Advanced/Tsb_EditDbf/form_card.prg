/*
 * MINIGUI - Harbour Win32 GUI library
 *
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com>
 *
 * Карточка по таблице / Card by table
*/
#define  _HMG_OUTLOG
#include "hmg.ch"
#include "tsbrowse.ch"
#include "tsb_mydef.ch"
//////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Draw_BtnEx_Card( nY, nX, nGBtn, nW )
   LOCAL nHIco, lIco, nWBtn, nHBtn, aBtnCap, nBtnLen, aFont2, oBtn
   LOCAL nHBtn2, lRow := .T.  // кнопки по горизонтали

   oBtn       := oHmgData()
   aBtnCap    := {"Button;(1)","Button;(2)", "Button;(3)", "Button;(4)", "Exit" }
   nHIco      := myScreenIconSize(App.Cargo:aDisplayMode[2])  // высота иконки от экрана
   nHIco      += 5
   lIco       := .T.  // растягивать размер иконки
   nHBtn      := nHIco + 15
   oBtn:aCap  := aBtnCap
   oBtn:aObj  := { "_Card1" ,"_Card2" ,"_Card3"  , "_Card4", "_CardExit"  } // метка события
   nBtnLen    := LEN(aBtnCap)
   nWBtn      := ( nW - nGBtn * (nBtnLen+1) ) / nBtnLen       // ширина кнопки
   oBtn:aWBtn := { nWBtn, nWBtn, nWBtn, nWBtn, nWBtn }
   oBtn:aClr  := { {26,84,91}, {115,96,241}, {0,215,87}, {38,158,218} , {254, 73, 83} }
   //oBtn:aPst := { 10, 11, 12, 13, 99 }  // _wPost(Х) - номер события на кнопке - не использую
   aFont2     := GetFontParam(GetFontHandle("FntBtn_1"))
   oBtn:aFnt  := { aFont2[1], aFont2[2], aFont2[3] }       // фонты для всех кнопок
   nHBtn2     := aFont2[2] * 4                             // 2 строки текста на кнопках
   nHBtn      := MAX(nHBtn,nHBtn2)                         // скорректируем высоту кнопки
   nHIco      := nHBtn - 10                                // высота-ширина иконки на кнопке
   oBtn:nHBtn := nHBtn                                     // ручное задание высоты кнопки
   oBtn:aIco  := { {"iMg96x1"       ,"iMg96x2"      , lIco, nHIco } ,;
                   {"iMg96x1"       ,"iMg96x2"      , lIco, nHIco } ,;
                   {"iMg96x1"       ,"iMg96x2"      , lIco, nHIco } ,;
                   {"iMg96x1"       ,"iMg96x2"      , lIco, nHIco } ,;
                   {"iReturn48x1"   , "iReturn48x2" , lIco, nHIco }  }
   oBtn:aFntClr  := { BLACK, OLIVE }
   ? ProcNL(), "^^^^^^^^^^^ 2 rows of buttons=", nHBtn, nHBtn2

   Draw_BtnEx( nY, nX, oBtn, nWBtn, nHBtn, nGBtn, lRow )  // -> util_button.prg

RETURN nY + nHBtn + nGBtn

//////////////////////////////////////////////////////////////////////////////////////
Function Card_for_table(oWnd,ky,oBrw1,cIcon,cTitle,aBClr)
   LOCAL aRet, owc, cForm, nTekRec, cAdres, aFont2, cAls, nWTxt
   LOCAL nY, nX, nW, nH, nG, nHUp, nY2, nX2, nW2, nH2, nWTsb, nHTsb
   LOCAL cFont, nFSize, lCaEdit, cEdit, aEdBClr

   ? ProcNL(),oWnd,ky,oBrw1
   // позиция окна ТСБ
   //oWnd  := _WindowObj( oBrw1:cParentWnd )   // родительское окно
   nY      := GetWindowRow(oBrw1:hWnd)
   nX      := GetWindowCol(oBrw1:hWnd)
   nW      := GetWindowWidth(oBrw1:hWnd)
   nH      := GetWindowHeight(oBrw1:hWnd)
   // Координаты окна можно так получить
   nY      := oWnd:Row   ; nX := oWnd:Col
   nW      := oWnd:Width ; nH := oWnd:Height
   nG      := 15                                // между объектами на форме
   aRet    := {}                                // всегда массив - пусто, значит отказ от ввода
   cForm   := oWnd:Name + "_Card_Modal"
   aFont2  := GetFontParam(GetFontHandle("FntBtn_1"))
   cFont   := aFont2[1]
   nFSize  := aFont2[2]
   cAls    := oBrw1:cAlias
   nTekRec := RECNO()         // номер записи запомним
   cAdres  := ALLTRIM((cAls)->ADRESPRN)
   lCaEdit := .F.
   IF (cAls)->Dateza > DATE() - 30
      lCaEdit := .T.
   ENDIF
   cEdit   := IIF( lCaEdit, "[editing allowed]", "[editing prohibited]" )
   aEdBClr := IIF( lCaEdit, BLUE, RED )

   // ОБЯЗАТЕЛЬНО для окна MODAL / REQUIRED for MODAL window
   IF !Empty( _HMG_MainHandle ) ; _HMG_InplaceParentHandle := GetActiveWindow()
   ENDIF

   // новое окно в координаты таблицы
   nW2 := nW * 0.94
   nH2 := nH * 0.98
   nX2 := ( nW - nW2 ) / 2 + nX
   nY2 := ( nH - nH2 ) / 2 + nY
   cTitle  += SPACE(5) + ProcFile()

   DEFINE WINDOW &cForm AT nY2,nX2 WIDTH nW2 HEIGHT nH2 TITLE cTitle ;
      MODAL ICON cIcon BACKCOLOR aBClr                               ;
      ON INIT    {|| _wSend(0)                    }                  ;
      ON RELEASE {|| /*This.Hide,*/ _wSend(90)    }   // модальное окно нельзя делать Hide
      This.Cargo := oHmgData() ; owc := This.Cargo

      owc:aBClr    := This.Backcolor
      owc:ObjWin   := This.Object
      owc:hWin     := This.Handle                     // хендл этого окна
      owc:Name     := This.Name
      owc:oWin     := This.Object
      owc:ahIcoDel := {}                              // для удаления хендлов иконок с формы
      owc:nG       := nG                              // отступ от края окна
      owc:aObjHide := {}                              // для сокрытия кнопок
      owc:aTsbDel  := {}                              // для удаления 5-таблиц
      owc:cAls     := ALIAS()
      owc:nRecno   := RECNO()                         // номер записи заявки
      owc:nTekRec  := nTekRec                         // вернуть указатель записи где был
      owc:lCaEdit  := lCaEdit

      nY := nX := nG
      nW := This.ClientWidth
      nH := This.ClientHeight

      @ 0, 0 LABEL Buff VALUE "" WIDTH 5 HEIGHT 5 TRANSPARENT

      // задать и вывести кнопки над таблицей / set and display buttons above the table
      nHUp  := Draw_BtnEx_Card( nY, nX, nG, nW )
      nY    := nHUp

      nWTxt := GetTxtWidth( cEdit, nFSize, cFont, .T. ) + 10
      @ nY, nW - nG - nWTxt LABEL Lbl_Edit VALUE cEdit WIDTH nWTxt HEIGHT nFSize*2 ;
        FONT cFont SIZE nFSize FONTCOLOR WHITE BACKCOLOR aEdBClr CENTERALIGN

      @ nY, nX LABEL Lbl_Adr VALUE cAdres WIDTH nW-nG*2-nWTXt HEIGHT nFSize*4 ;
        FONT cFont SIZE nFSize FONTCOLOR WHITE TRANSPARENT
      myBigSizeLabel(ThisWindow.Name,"Lbl_Adr")

      nY    += This.Lbl_Adr.Height + nG/2
      nWTsb := nW - nG * 2  ; nHTsb := nH - nY - nG
      //@ nY, nX LABEL Label_Tsb VALUE "Table" WIDTH nWTsb HEIGHT nHTsb FONTCOLOR WHITE BACKCOLOR GRAY
      owc:aTbl := { nY, nX, nWTsb, nHTsb, "Место под таблицу / Space for table" }

      @ nY, nX LABEL Lbl_Wait VALUE "TABLES ARE BEING CONSTRUCTED" WIDTH nWTsb HEIGHT nHTsb ;
        SIZE 42 FONTCOLOR WHITE TRANSPARENT CENTERALIGN VCENTERALIGN BORDER 
      //myBigSizeLabel(ThisWindow.Name,"Lbl_Wait")

      // НЕ использую oMenu:aObj := { "_3Tab1", "_3Tab2", "_3Tab3", "_3Tab4", "_3Tab5", "_3Tab6" }
      // из form_card_6tsb.prg - только одно событие на всех кнопках "_3Tab0"
      //
      WITH OBJECT This.Object
         :Event(  0, {|ow| // ON INIT 
                            DoEvents()
                            /////// таблицы 6 штук ////////////
                            DBSELECTAREA(ow:Cargo:cAls)
                            Card_6Tsb(ow:Cargo:oWin,ow:Cargo:aBClr,{},"",ow:Cargo:lCaEdit)
                            This.Lbl_Wait.Hide
                            //_wSend("_3Tab0",ow,"_3Zaivka")
                            DoEvents()
                            Return Nil
                            } )

         // имя объекта + имя события //   кнопки верхнего меню
         //            VVVV           //   oBtn:aObj := { "_Card1" ,"_Card2" ,"_Card3"  , "_Card4", "_CardExit"  }
         :Event({10,"_Card1"}, {|ow,ky,cn| //
                                           _SetThisFormInfo(ow)
                                           MsgDebug(ow:Name, ky, cn)
                                           _SetThisFormInfo()
                                           This.&(cn).Enabled := .T.     // unlock button
                                           ow:Setfocus('Buff')
                                           Return Nil
                                           } )

         :Event({11,"_Card2"}, {|ow,ky,cn| //
                                           _SetThisFormInfo(ow)
                                           MsgDebug(ow:Name, ky, cn)
                                           _SetThisFormInfo()
                                           This.&(cn).Enabled := .T.     // unlock button
                                           ow:Setfocus('Buff')
                                           Return Nil
                                           } )

         :Event({12,"_Card3"}, {|ow,ky,cn| //
                                           _SetThisFormInfo(ow)
                                           MsgDebug(ow:Name, ky, cn)
                                           _SetThisFormInfo()
                                           This.&(cn).Enabled := .T.     // unlock button
                                           ow:Setfocus('Buff')
                                           Return Nil
                                           } )

         :Event({13,"_Card4"}, {|ow,ky,cn| //
                                           _SetThisFormInfo(ow)
                                           MsgDebug(ow:Name, ky, cn)
                                           _SetThisFormInfo()
                                           This.&(cn).Enabled := .T.     // unlock button
                                           ow:Setfocus('Buff')
                                           Return Nil
                                           } )

         // :Event( 32, 33, 34 заняты - VK_F2 VK_F3 VK_F4

         :Event({40,"_3Tab0"}, {|ow,ky,cn| // 6 таблиц на окне
                                              Local i, obr, a6Tbl
                                              Local aTsb  := ow:Cargo:aTsbDel
                                              Local aBrw  := ow:Cargo:aBrw
                                              Local nBrw  := ow:Cargo:nBrw      // тек. тсб в фокусе
                                              Local obc   := This.&(cn).Cargo
                                              Local nBtn  := obc:nBtn           // номер кнопки
                                              Local cForm := ow:Name
                                              Local owc   := ow:Cargo

                                              _LogFile(.T., "  -->> Event: ",ky, cn, ow:Name, "--- 6 таблиц ---")
                                              ? "  -->> " + ProcNL(), "nBrw=",nBrw, owc:nBrw, "aBrw=", aBrw
                                              ?? "  Номер кнопки/таблицы",nBtn

                                              a6Tbl := HMG_GetFormControls(cForm, "TBROWSE")
                                              //?v a6Tbl

                                              For i := 1 TO LEN(aTsb)
                                                 DoMethod(cForm, aTsb[i], "Hide")
                                                 //? "  -->> ", i, aTsb[i], "Hide"
                                              Next

                                              DoMethod(cForm, aTsb[nBtn], "Show")
                                              This.&(aTsb[nBtn]).Setfocus
                                              This.&(aTsb[nBtn]).Show

                                              owc:nBrw := This.&(cn).Cargo:nBtn  // тек. тсб в фокусе

                                              FOR EACH obr IN ow:Cargo:aBrw
                                                 i := hb_EnumIndex(obr)
                                                 //? "   >>", i, obr:cControlName
                                                 //obr:Hide()
                                              NEXT
                                              //IF owc:nBrw > 0 ; AEval(owc:aBrw, {|ob| ob:Hide() })
                                              //ENDIF
                                              //obr := owc:aBrw[ owc:nBrw ]
                                              //obr := ow:Cargo:aBrw[n]
                                              //obr:SetFocus()
                                              //obr:Show()
                                              This.&(cn).Enabled := .T.
                                              DO EVENTS
                                              Return Nil
                                              } )

         :Event(42, {|ow,ky| // переключать видимость столбцов из подвала таблицы, колонка (2)  ob:aColumns[2]:bFLClicked
                      Local aIsx, obr, cName, oCol, nCol, nI, lChk, aVal, aDim, i, aUser, nBtn, nAlg, owc
                      Local aHide := {"F_NN","F_PROCES", "F_BASE", "F_READ", "F_WRITE", "F_WINDOWS", "F_ACCESS","V_CALC", "V_SPR","V_AADD"}
                      //                 3         4         5        6           7             8        9         10        11      12
                      owc  := ow:Cargo
                      nBtn := owc:nBrw               // текущий тсб в фокусе
                      obr  := owc:aBrw[nBtn]         // тек.объект из массива объектов таблицы
                      aIsx := obr:Cargo:aIsxTbl      // ИСХОДНЫЙ массив в массиве объектов таблицы
                      lChk := obr:Cargo:lChkTsb      // правка настроек строк таблицы
                      //
                      obr:Hide()                     // скрыть таблицу для дальнейшей прорисовки
                      obr:aArray := aIsx             // заместить массив ТСБ на ИСХОДНЫЙ массив
                      obr:nLen   := LEN(aIsx)
                      obr:Reset()
                      //
                      IF lChk
                         // "---- показ всех колонок ----"
                         FOR EACH oCol IN obr:aColumns
                            nCol := hb_EnumIndex(oCol)
                            oCol:lVisible := lChk
                         NEXT
                         //obr:lDrawSpecHd   := .T.  - ломает показ ТСБ
                      ELSE
                         //obr:lDrawSpecHd   := .F.  - ломает показ ТСБ
                         // "---- скрыть показ колонок ----"
                         FOR EACH oCol IN obr:aColumns
                            nCol  := hb_EnumIndex(oCol)
                            cName := oCol:cName
                            IF "_" $ cName             // вариант-2
                               //oCol:lVisible := .F.
                            ENDIF
                            FOR nI := 1 TO LEN(aHide)
                               IF cName == aHide[nI]
                                  oCol:lVisible := .F.
                               ENDIF
                            NEXT
                         NEXT
                         // сортировка юзера
                         obr:Hide()
                         aUser := {}
                         aDim  := obr:aArray
                         aDim  := ASORT( aDim,,, { |x, y| x[ADIM_SORT] < y[ADIM_SORT] } )
                         For i := 1 TO Len(aDim)
                            aVal := aDim[i]
                            If aVal[ADIM_SORT] # 0
                               AADD( aUser, aVal )
                            Endif
                         Next
                         If Len(aUser) == 0
                            aVal    := ACLONE(aDim[1])
                            aVal[1] := "Нет строк таблицы для показа !"
                            aVal[2] := "Исправьте сортировку показа через (3) столбец !"
                            AADD( aUser, aVal )
                            AADD( aUser, aVal )
                         Endif
                         // далее заместить массив ТСБ на новый !!!
                         obr:aArray := aUser
                         obr:nLen   := LEN(aUser)
                         obr:Reset()
                         obr:Refresh()
                         obr:Show()
                         DO EVENTS
                      ENDIF
                      // перерисовать таблицу
                      nAlg := iif( lChk, DT_RIGHT, DT_CENTER )
                      obr:nCell  := obr:Cargo:nCellFoc
                      obr:oHScroll := NIL
                      obr:Refresh()
                      obr:DrawSelect()
                      obr:SetFocus()
                      obr:Show()
                      DO EVENTS
                      obr:nAlignSupHdSet(1,, nAlg)
                      obr:SendMsg(WM_KEYDOWN, VK_RIGHT, 0) // _PushKey(VK_RIGHT)
                      DO EVENTS
                      obr:Cargo:lChkTsb := ! lChk  // смена состояния
                      ky := nBtn
                      Return Nil
                      } )

         //  ob:aColumns[3]:bFLClicked - меню сортировки пользователя
         :Event(43, {|ow,ky,cN| _SetThisFormInfo(ow) , myContexMenuSort(ow,ky,cN) ,;
                                _SetThisFormInfo() , ow:Setfocus("Buff"), DoEvents() })

         :Event(90, {|ow,ky| // ON Release windows
                             Local cm
                             cm := ProcNL()
                             ?  cm, "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                             ?  Repl(".", Len(cm)), "=> RELEASE WINDOW <=", ow:Name
                             DO EVENTS
                             Return Nil
                             })

         :Event({98,"_CardExit"}, {|ow,ky,cn| // выход
                                             _LogFile(.T., "  -->> Button: ",cn, ow:Name, ky)
                                             _SetThisFormInfo(ow)
                                             IF  lCaEdit
                                                // запись всей записи базы
                                                ThisWriteRecno(owc)
                                             ENDIF
                                             _SetThisFormInfo()
                                             aRet := {}
                                             _wSend(99,ow:Name)
                                             Return Nil
                                             } )

         :Event({99,"_CardRls"}, {|ow| ow:Release() } )
      END WITH

      ON KEY F1     ACTION NIL
      ON KEY ESCAPE ACTION _wPost(98)

   END WINDOW

   ACTIVATE WINDOW &cForm

   //IF _IsWindowDefined(cPrnt)               // можно так / you can do it like this
   //   DoMethod(cPrnt, "SetFocus")
   //ENDIF

   IF _IsWindowDefined(oWnd:Name)             // ОБЯЗАТЕЛЬНО / REQUIRED
      oWnd:SetFocus()
   ENDIF

   _HMG_InplaceParentHandle := 0   // ОБЯЗАТЕЛЬНО для окна MODAL / REQUIRED for MODAL window

   DO EVENTS

RETURN aRet // всегда массив, если пусто - значит отказ от ввода

////////////////////////////////////////////////////////////////////////////////////////
FUNCTION ThisWriteRecno(owc) // запись всей заявки
   LOCAL cMsg, nTime, cStr, cAls, i, j, aTsb, aTsbIni, iWrt, aBrw, oBrw, cForm, cBrw

   ? ProcNL(), "----- запись карточки в базу", owc:cAls
   owc:cTxtZaivk := "Здесь текст по заявке!"  // вернуть
   cStr    := ""
   cAls    := owc:cAls
   nTime   := VAL( SUBSTR(TIME(), 1, 2)+SUBSTR(TIME(), 4, 2) )
   aTsb    := owc:aTsbDel         // таблицы для удаления
   aTsbIni := owc:aTsbIni         // настройки сортировки строк в таблице
   aBrw    := owc:aBrw            // массив объектов ТСБ
   cForm   := owc:Name
   iWrt    := 0

   WaitWindow( {'Запись данных в БД-' + owc:cAls, App.ExeName }, .T., 400, 16, NIL, WHITE, {192,0,255} )
   DbSelectArea(cAls)
   DbGoto(owc:nRecno)            // номер записи ЭТОЙ заявки

   If Len(aTsb) > 0 // таблицы для удаления
      DbGoto(owc:nRecno)
      For i := 1 TO Len(aTsb)   // { "cTable_1", "cTable_2", ...
         cBrw := aTsb[i]
         ? "   >>> RECNO()=",RECNO(), i, cForm, cBrw
         If _IsControlDefined( cBrw, cForm )
            oBrw := aBrw[i]
            If Len(owc:aWrtIni) > 0
               For j := 1 To Len(owc:aWrtIni)
                  If i == owc:aWrtIni[j,2]
                     ?? "[Запись "+ owc:aWrtIni[j,1] + "]"
                     oBrw:Cargo:nModify++
                     iWrt++
                  Endif
               Next
            Endif
            ?? "oBrw=", oBrw, cFileNoPath(aTsbIni[i]), oBrw:Cargo:nModify
            IF oBrw:Cargo:nModify >  0           // счётчик изменений
               iWrt++
               ArrayDbfSave(oBrw:aArray)         // запись из массива в базу
                      // V--- ИСХОДНЫЙ массив ВАЖНО !!!
               IniSave(oBrw:Cargo:aIsxTbl,aTsbIni[i])  // запись сортировки показа
               ?? "[write dbf]"
            ENDIF
         Endif
      Next
   Endif

   If (cAls)->(RLock())
      IF iWrt > 0
         //(cAls)->KOPERAT  := M->nOperat  // кто правил запись ?
         (cAls)->DATEVVOD := DATE()
         (cAls)->TIMEVVOD := nTime
         (cAls)->( DBCommit() )
         (cAls)->( DBUnlock() )
      ENDIF
   ELSE
      cMsg := "БД " + cAls + " ! "
      cMsg += "Запись " +HB_NtoS(owc:nRecno)+ " заблокирована !;"
      cMsg += "Попробуйте ещё раз чуть позже"
      AlertStop( cMsg, "Ошибка!", "iMgStop128", 72, {RED} )
      ? ProcNL(), AtRepl( ";", cMsg, CRLF )
   ENDIF

   DbGoto(owc:nTekRec)    // вернуть указатель записи где был
   WaitWindow()

RETURN NIL
