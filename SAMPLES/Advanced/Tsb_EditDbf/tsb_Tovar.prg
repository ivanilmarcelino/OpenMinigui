/*
 * MINIGUI - Harbour Win32 GUI library
 *
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com>
 * Edit 15.04.25
 *
 * _TBrowse() Разные функции для редактирований ячеек таблицы
 * _TBrowse() Various functions for editing table cells
*/
#define  _HMG_OUTLOG
#include "hmg.ch"
#include "tsbrowse.ch"
#include "tsb_mydef.ch"

//////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Draw_BtnEx_Tovar( nY, nX, nGBtn, nW )
   LOCAL nHIco, lIco, nWBtn, nHBtn, aBtnCap, nBtnLen, aFont2, nHBtn2, oBtn
   LOCAL lRow := .T.  // кнопки по горизонтали

#ifdef KEY_ENG // for this project demo1-en.hbp
   aBtnCap := {"Ins-insert;recno","Del-delete;recno", "Delete;all recno", "Save", "Cancel" }
#else
   aBtnCap := {"Ins-вставка;записи","Del-удалить;запись", "Удалить;всё записи", "Сохранить", "Отменить" }
#endif

   oBtn      := oHmgData()
   nHIco     := myScreenIconSize(App.Cargo:aDisplayMode[2])  // высота иконки от экрана
   //nHIco   := 48
   lIco      := .T.  // растягивать размер иконки
   nHBtn     := nHIco + 15
   oBtn:aCap := aBtnCap
   nBtnLen   := LEN(aBtnCap)
   nWBtn     := ( nW - nGBtn * (nBtnLen+1) ) / nBtnLen       // ширина кнопки
   oBtn:aWBtn:= { nWBtn, nWBtn, nWBtn, nWBtn, nWBtn }
   oBtn:aClr := { GRAY, GRAY, GRAY, { 35,179, 15} , {254, 73, 83} }
   oBtn:aPst := { 10, 11, 12, 13, 99 }  // _wPost(Х) - номер события на кнопке
   oBtn:aObj := { "_2InsRec" ,"_2DelRec" ,"_2DelAll"  , "_2Save", "_2Cancel"  }
   oBtn:aIco := { {"iRecIns48x1"   , "iRecIns48x2"  , lIco, nHIco} ,;
                  {"iRecDel48x1"   , "iRecDel48x2"  , lIco, nHIco} ,;
                  {"iRecAll48x1"   , "iRecAll48x1"  , lIco, nHIco} ,;
                  {"iFloppy48x1"   , "iFloppy48x2"  , lIco, nHIco} ,;
                  {"iReturn48x1"   , "iReturn48x2"  , lIco, nHIco}  }
   aFont2    := GetFontParam(GetFontHandle("ComSanMS"))
   oBtn:aFnt := { aFont2[1], aFont2[2], aFont2[3] }  // фонты для всех кнопок
   nHBtn2    := aFont2[2] * 4                        // 2 строки текста на кнопках
   ? ProcNL(), nHIco, "$$$$$$$ 2 rows of buttons=", nHBtn, nHBtn2, HB_ValToExp(App.Cargo:aDisplayMode)

   IF App.Cargo:aDisplayMode[2] > 799
      nHBtn := MAX(nHBtn,nHBtn2)                    // скорректируем высоту кнопки
   ENDIF

   Draw_BtnEx( nY, nX, oBtn, nWBtn, nHBtn, nGBtn, lRow )  // -> util_button.prg

RETURN nY + nHBtn + nGBtn

//////////////////////////////////////////////////////////////////////////////////////
Function Tsb_Tovar_HMG(oBrw1,cVal13,aLine,cForm)   // брать за основу myWinCalc()
   LOCAL oWnd, aRet, cTitle, cIcon, aXTovar, owc, cMsg
   LOCAL nY, nX, nW, nH, nG, aBClr, nHUp, a1Dim, aLang
   LOCAL nY2, nX2, nW2, nH2, nWTsb, nHTsb, oBrw, oTsb
   DEFAULT cForm := "Tsb_Win_Tovar"

   //MsgDebug(aDim,"oDop=",oDop,"aLine=",aLine,cForm)
   ? ProcNL(),"cVal13=",cVal13,"aLine=",aLine,cForm

   IF "DEMO3.EXE" $ UPPER(cFileNoPath(App.ExeName))
      // позиция окна по окну / window position by window
      oWnd   := _WindowObj( oBrw1:cParentWnd )    // parent window
      nY     := oWnd:Row
      nX     := oWnd:Col
      nW     := oWnd:Width
      nH     := oWnd:Height
   ELSE
      // позиция окна по ТСБ / window position according to TSB
      oWnd   := _WindowObj( oBrw1:cParentWnd )   // родительское окно
      nY     := GetWindowRow(oBrw1:hWnd)
      nX     := GetWindowCol(oBrw1:hWnd)
      nW     := GetWindowWidth(oBrw1:hWnd)
      nH     := GetWindowHeight(oBrw1:hWnd)
   ENDIF

   nG     := 15   // между объектами на форме
   aRet   := {}   // всегда массив - пусто, значит отказ от ввода
   aBClr  := {143,153,219}
   cIcon  := "gear48x1"
   cTitle := aLine[ACOL_1] + SPACE(5) + ProcFile()

#ifdef KEY_ENG // for this project demo1-en.hbp
   cMsg  := "selection from directory"
   aLang := { "Insert an entry"   , "You want to insert a NEW record into the table ?;" ,;
              "Insert an entry"   , "Do you want to delete this recno ?;"    ,;
              "Clearing the table", "Do you want to drop the ENTIRE table?;" ,;
              "The table is empty, there are no records!;" }
#else
   cMsg  := "выбор из справочника"
   aLang := { "Вставка записи" , "Вы хотите вставить НОВУЮ запись в таблицу ?;" ,;
              "Удаление записи", "Вы хотите удалить эту запись ?;"  ,;
              "Очистка таблицы", "Вы хотите удалить ВСЮ таблицу ?;" ,;
              "Таблица пустая, нет записей !;" }
#endif

   // таблица из 1-записи
   //          1   2    3   4   5    6    7   8  9  10
   a1Dim := { "", "", cMsg, 0, 0.0, 0.0, 0.0, 0, 0, 0  }
   IF LEN(cVal13) == 0
      aXTovar := { a1Dim }
   ELSE
      aXTovar := GetLineDim(cVal13)
   ENDIF

   // ОБЯЗАТЕЛЬНО для окна MODAL / REQUIRED for MODAL window
   IF !Empty( _HMG_MainHandle ) ; _HMG_InplaceParentHandle := GetActiveWindow()
   ENDIF

   // новое окно в координаты таблицы
   nW2 := nW //* 0.9
   nH2 := nH //* 0.9
   nX2 := ( nW - nW2 ) / 2 + nX
   nY2 := ( nH - nH2 ) / 2 + nY

   DEFINE WINDOW &cForm AT nY2,nX2 WIDTH nW2 HEIGHT nH2 TITLE cTitle ;
      MODAL NOSIZE ICON cIcon BACKCOLOR aBClr                        ;
      ON INIT    {|| _wSend(0)                    }                  ;
      ON RELEASE {|| /*This.Hide,*/ _wSend(90)    }   // модальное окно нельзя делать Hide
      This.Cargo := oHmgData() ; owc := This.Cargo

      owc:aBClr := This.Backcolor
      owc:aLang := aLang
      nY        := nX := nG
      nW        := This.ClientWidth
      nH        := This.ClientHeight

      @ 0, 0 LABEL Lbl_0 VALUE "" WIDTH 5 HEIGHT 5 TRANSPARENT

      // задать и вывести кнопки над таблицей / set and display buttons above the table
      nHUp  := Draw_BtnEx_Tovar( nY, nX, nG, nW )

      nY    := nHUp         ; nX := nG
      nWTsb := nW - nG * 2  ; nHTsb := nH - nY - nG
      //@ nY, nX LABEL Label_Tsb VALUE "Table" WIDTH nWTsb HEIGHT nHTsb FONTCOLOR WHITE BACKCOLOR GRAY
      /////////////////////// таблица /////////////////////////////////////////////////
      oTsb := TablePatamTvr( cForm, aXTovar, "cTableTvr", nWTsb, cTitle)
      // функция в библиотеке \MiniGUI\SOURCE\TsBrowse\h_controlmisc2.prg
      oBrw := _TBrowse( oTsb, aXTovar, "cTableTvr", nY, nX, nWTsb, nHTsb )
      // здесь делаем донастройку таблицы
      oBrw:Cargo:nCellFoc := 2                            // фокусная ячейка
      oBrw:Cargo:nModify  := 0                            // счётчик изменений
      oBrw:Cargo:a1Dim    := a1Dim                        // пустая строка массива
      oBrw:Cargo:aLang    := aLang
      // объект положим на окно
      This.Cargo:oBrw     := oBrw                         // oWnd:Cargo:oBrw
      This.Cargo:cBrw     := oBrw:cControlName            //"cTableTvr"
      This.Cargo:aIsx     := oBrw:aArray                  // ИСХОДНЫЙ массив ВАЖНО !!!
      This.Cargo:aItog    := oTsb:aItogo                  // поля для суммирования внизу (подвал таблицы)
      //
      WITH OBJECT This.Object
         :Event(  0, {|ow| ow:Cargo:oBrw:SetFocus(), DoEvents(), _wSend("_2Itogo",ow) } )
         // имя объекта + имя события //   кнопки верхнего меню
         //            VVVV           //   oBtn:aObj := { "_2InsRec","_2DelRec","_2DelAll","_2Save","_2Cancel"}
         :Event({10,"_2InsRec"}, {|ow,ky,cn| // Вставка записи
                                             Local aLng := ow:Cargo:aLang
                                             Local ob   := ow:Cargo:oBrw
                                             Local aRec := ACLONE(ob:Cargo:a1Dim)
                                             Local nK, lYes := .F.
                                             If !IsString(cn)  // это когда срабатывает по клавише INS
                                                cn := "_2InsRec"
                                             Endif
                                             _SetThisFormInfo(ow)
                                             IF AlertYesNo(aLng[2]+";", aLng[1],,,64,{LGREEN,RED})
                                                ob:Enabled(.F.)
                                                AAdd(ob:aArray, aRec)
                                                nK := Len(oBrw:aArray)
                                                ob:Enabled(.T.)
                                                ob:Reset()
                                                ob:GotoRec(nK)
                                                ob:Cargo:nModify += 1
                                                lYes := .T.
                                             ENDIF
                                             _SetThisFormInfo()

                                             This.&(cn).Enabled := .T.
                                             If lYes
                                                _wSend("_2Itogo",ow)
                                             Endif
                                             ob:SetFocus()
                                             DO EVENTS
                                             ky := cn
                                             Return Nil
                                             })

         :Event({13,"_2DelRec"}, {|ow,ky,cn| // Удаление записи
                                             Local aLng := ow:Cargo:aLang
                                             Local ob   := ow:Cargo:oBrw
                                             Local cVal := ALLTRIM(ob:aArray[1][1])
                                             Local i, k, oc, xVal, lNoRows, lYes := .F.
                                             If !IsString(cn)  // это когда срабатывает по клавише DEL
                                                cn := "_2DelRec"
                                             Endif
                                             lNoRows := Len(ob:aArray) == 1 .and. LEN(cVal) == 0 // .T. - все записи удалены
                                             _SetThisFormInfo(ow)
                                             If AlertYesNo(aLng[4]+";", aLng[3],,,64,{LGREEN,RED})
                                                ob:Enabled(.F.)
                                                If Len(ob:aArray) == 1
                                                   // чистим колонки таблицы
                                                   k := 0
                                                   IF ob:nColumn("SELECTOR", .T.) > 0 ; k++
                                                   ENDIF
                                                   IF ob:nColumn("ARRAYNO", .T.) > 0  ; k++
                                                   ENDIF
                                                   For i := 1 TO Len(ob:aColumns)
                                                      oc := ob:aColumns[ i ]
                                                      If oc:cName $ "SELECTOR,ARRAYNO" ; LOOP
                                                      Endif
                                                      xVal := ob:aArray[1][i - k]
                                                      ob:aArray[1][i - k] := NIL
                                                   Next
                                                   ob:DrawSelect()   // ob:Refresh()
                                                   ? "$$$$$$$$$$$$$ ===> ob:aArray[1]", ob:aArray[1]
                                                   ?v ob:aArray[1]  ; ?
                                                Else
                                                   ob:DeleteRow( ) // Array method - Delete selected row
                                                   ob:Reset()
                                                Endif
                                                ob:Enabled(.T.)
                                                ob:Cargo:nModify += 1
                                                lYes := .T.
                                             Endif
                                             _SetThisFormInfo()
                                             This.&(cn).Enabled := .T.
                                             If lYes
                                                _wSend("_2Itogo",ow)
                                             Endif
                                             ob:SetFocus()
                                             DO EVENTS
                                             ky := cn
                                             Return Nil
                                             })

         :Event({15,"_2DelAll"}, {|ow,ky,cn| // очистка всей таблицы
                                             Local aLng := ow:Cargo:aLang
                                             Local ob   := ow:Cargo:oBrw
                                             Local aRec := ACLONE(ob:Cargo:a1Dim)
                                             Local lYes := .F.
                                             _SetThisFormInfo(ow)
                                             If AlertYesNo(aLng[6]+";", aLng[5],,,64,{LGREEN,RED})
                                                ob:Enabled(.F.)
                                                oBrw:aArray := { aRec }
                                                ob:Reset()
                                                ob:nCell := 4
                                                ob:Enabled(.T.)
                                                ob:Cargo:nModify += 1
                                                lYes := .T.
                                             Endif
                                             _SetThisFormInfo()
                                             This.&(cn).Enabled := .T.
                                             If lYes
                                                _wSend("_2Itogo",ow)
                                             Endif
                                             ob:SetFocus()
                                             DO EVENTS
                                             ky := cn
                                             Return Nil
                                             })

         // назначим клавиши в таблице, см. ниже
         // oTsb:aUserKeys := { VK_F2, VK_F3, VK_F4, VK_INSERT   , VK_DELETE
         //             _wPost(  32  ,  33  , 34   ,  10         ,  13

         :Event({60,"_2Itogo"}, {|ow| Itog_Table(ow), ow:Cargo:oBrw:SetFocus(), DoEvents() } )  // итоги в подвале таблицы

         :Event({80,"_2Save"}, {|ow,ky,cn| // Сохранить таблицу
                                           Local ob := ow:Cargo:oBrw
                                           _SetThisFormInfo(ow)
                                           aRet := myTsbCloseTvr(ob)
                                           _SetThisFormInfo()
                                           This.&(cn).Enabled := .T.
                                           ob:SetFocus()
                                           DO EVENTS
                                           ky := cn
                                           _wSend("_2Releas",ow:Name)
                                           Return Nil
                                           })

         :Event(90, {|ow,ky| // ON Release windows
                             Local cm
                             cm := ProcNL()
                             ?  cm, "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                             ?  Repl(".", Len(cm)), "=> RELEASE WINDOW <=", ow:Name
                             DO EVENTS
                             Return Nil
                             })

         :Event({98,"_2Cancel"}, {|ow| aRet := {}, ow:Release() } )
         :Event({99,"_2Releas"}, {|ow| ow:Release() } )
      END WITH

      ON KEY F1     ACTION NIL
      ON KEY ESCAPE ACTION _wPost(98)
      ON KEY F7     ACTION {|ob|// просмотр массива
                                ob := ThisWindow.Cargo:oBrw
                                ? ":aArray", ob:aArray, "[1]", ob:aArray[1]
                                ?v ob:aArray[1]
                                ?
                                ?v ob:aDefValue
                                MsgDebug(":aArray=",ob:aArray,"[1]=",;
                                          ob:aArray[1],":aDefValue=", ob:aDefValue)
                                Return Nil
                                }
   END WINDOW

   ACTIVATE WINDOW &cForm

   IF _IsWindowDefined(oWnd:Name)             // ОБЯЗАТЕЛЬНО / REQUIRED
      oWnd:SetFocus()
   ENDIF

   _HMG_InplaceParentHandle := 0   // ОБЯЗАТЕЛЬНО для окна MODAL / REQUIRED for MODAL window

   DO EVENTS

RETURN aRet // всегда массив, если пусто - значит отказ от ввода

////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION GetLineDim(cVal)
   LOCAL aTmp, a4Tmp, aDim, aVal, nSum, nCode, cName, nI, nCod1, nCod2

   aDim := {}
   // cVal = "2,35,277,2.00;2,31,244,1.00;1,2,141,1.00;"
   aTmp := HB_ATokens( cVal, ";", .T., .T. )
   FOR nI := 1 TO LEN(aTmp)
      IF LEN(aTmp) > 1
         a4Tmp := HB_ATokens( aTmp[nI], ",", .T., .T. )
         IF LEN(a4Tmp) == 4
            aVal := {}
            nCod1 := VAL(a4Tmp[1])
            cName := Alltrim( SAY_SEL_DIM( nCod1,'OB1ZAIV','OB1ZAIV'  ) )
            AADD( aVal, cName )
            nCod2 := VAL(a4Tmp[2])
            cName := Alltrim( SAY_SEL_DIM( nCod2,'OB2WORK','OB2WORK'  ) )
            AADD( aVal, cName )
            nCode := VAL(a4Tmp[3])
            cName := Alltrim( SAY_SEL_DIM( nCode,'Ob4orud','Ob4orud'  ) )
            AADD( aVal, cName )
            AADD( aVal, VAL(a4Tmp[4]) ) // кол-во
            nSum := SAY_SEL_DIM( nCode,'Ob4orud','Cena_All'  )
            AADD( aVal, nSum )       // "Cena_All" , "N",10, 2, "Цена Общая ($/руб.)
            nSum := SAY_SEL_DIM( nCode,'Ob4orud','CenaObor'  )
            AADD( aVal, nSum )       // "CenaObor" , "N",10, 2, "Цена оборуд.,неиспр. ($/руб.)
            nSum := SAY_SEL_DIM( nCode,'Ob4orud','CenaMast'  )
            AADD( aVal, nSum )       // "CenaMast" , "N",10, 2, "Цена мастеру ($/руб.)
            AADD( aVal, nCode )      // 8  - код Ob4orud
            AADD( aVal, nCod1 )      // 9  - код KOB1ZAIV
            AADD( aVal, nCod2 )      // 10 - код KOB2WORK
            // итого
            AADD( aDim,  aVal )
         ENDIF
      ENDIF
   NEXT

RETURN aDim

////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbCloseTvr( oBrw )
   LOCAL aRet, cAls, nCol, cNam, xVal, nI, aVal, aDim, cTemp, cCode, aLng
   LOCAL nSum_All, nSumObor, nSumMast

   aDim  := {}
   cAls  := oBrw:cAlias
   oBrw:GoTop()

   FOR nI := 1 TO oBrw:nLen
       aVal   := {}
       oBrw:GoPos(nI,)     // передвинуть МАРКЕР на nI-строку, nCol-колонку
       FOR nCol := 1 TO oBrw:nColCount()
           cNam := oBrw:aColumns[ nCol ]:cName
           IF     cNam == "ORDKEYNO"  ; LOOP
           ELSEIF cNam == "SELECTOR"  ; LOOP
           ELSE
              xVal := oBrw:GetValue(cNam)
              IF IsString(xVal)
                 xVal := ALLTRIM(xVal)
              ENDIF
              AADD( aVal, xVal )
           ENDIF
       NEXT
       AADD( aDim, aVal )
   NEXT
   // ---------- можно и так -----------
   // nAtPos := nI
   // aVal   := {}
   // FOR nCol := 1 TO oBrw:nColCount()
   //    xVal := oBrw:aArray[nAtPos][ nCol ]
   //    AADD( aVal , xVal )
   // NEXT
   // AADD( aRecno , aVal )

#ifdef KEY_ENG // for this project demo1-en.hbp
   aLng := { " pcs.", ", price ", "cost " }
#else
   aLng := { " шт.", ", цена ", "стоимостью " }
#endif

   aRet  := {}
   cTemp := cCode := ""
   nSum_All := nSumObor := nSumMast := 0
   //      1    2       3             4         5       6      7      8      9  10  11
   // 1  {"1", "М", "проч.обор.", "K-DOM 750", 2.00, 1650.00, 0.00, 230.00, 277, 2, 35}
   // 2  {"2", "М", "аудио", "А/У DP-20H", 1.00, 700.00, 0.00, 70.00, 244, 2, 31}
   // 3  {"3", "Т/О", "замена/уст", "БВД Элтис DP 400-TD", 1.00, 0.00, 0.00, 0.00, 141, 1, 2}
   FOR nI := 1 TO LEN(aDim)
      aVal := aDim[nI]
      ? nI, HB_ValToExp(aVal)
      IF aVal[5] > 0 // Kolvo
         cCode += HB_NtoS(aVal[10]) + "," + HB_NtoS(aVal[11])+","
         cCode += HB_NtoS(aVal[9]) + "," + HB_NtoS(aVal[5]) + ";"
         //
         cTemp += aVal[2] + "," + aVal[3] + "," + aVal[4] + ","
         cTemp += HB_NtoS(INT(aVal[5])) + aLng[1]
         IF aVal[6] > 0  // Cena_All
            cTemp += aLng[2] + ALLTRIM(TRANSFORM(aVal[6],"999 999.99" )) + " "
            IF aVal[5] > 1
               cTemp += aLng[3] + ALLTRIM(TRANSFORM(aVal[6]*aVal[5],"999 999.99" )) + " "
            ENDIF
            cTemp += "; "
         ELSE
            cTemp += "; "
         ENDIF
         nSum_All += aVal[5] * aVal[6]
         nSumObor += aVal[5] * aVal[7]
         nSumMast += aVal[5] * aVal[8]
      ENDIF
   NEXT
   //      мемо-поле "MKob4or"     список полей и значений для записи
   aRet := { cTemp  , cCode    , { {"SumVsego","SumWObor","SumMaster"} , {nSum_All,nSumObor,nSumMast} } }

RETURN aRet

////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION TablePatamTvr(cForm,aXDim,cBrw,nWTsb,cTitle)
   LOCAL oTsb, nClr1, nClr2, a, nHFnt, aWSize, aBClr, nHCell
   //
   oTsb := oHmgData()
   oTsb:cBrw           := cBrw
   oTsb:cForm          := cForm      // <--- обязательно так !!!
   oTsb:cFormName      := cForm      // или так
   //                         cell     Head    Foot     SpecHider  SuperHider   Edit
   oTsb:aFont          := { "Normal", "Bold", "Italic", "SpecHdr" , "SuperHd", "TsbEdit" }
   nHCell              := GetFontHeight(oTsb:aFont[1])*1.35       // высота ячеек  / cell height
   oTsb:nHeightCell    := nHCell                                  // the supplement depends on the screen size
   oTsb:aNumber        := { 1, GetFontWidth(oTsb:aFont[4], 3) }   // колонка нумерации и её ширина / numbering column and its width
   oTsb:nHeightHead    := nHCell * 1.6                            // высота шапки - убрать шапку таблицы
   oTsb:nHeightFoot    := nHCell * 1.6                            // высота подвала
   oTsb:lFooting       := .T.                                     // ставить в таблице подвал
   oTsb:lSpecHd        := .T.                                     // поставить в таблице нумератор колонок
   oTsb:lSuperHd       := .T.                                     // поставить в таблице суперхидер
   oTsb:cSuperHd       := cTitle                                  // текст суперхидера
   oTsb:nHeightSuperHd := nHCell * 1.6                            // высота суперхидера
   oTsb:nCellMarginLR  := 0                                       // отступ от линии ячейки при прижатии влево, вправо на кол-во пробелов
   //oTsb:uSelector    := 20                                      // селестор слева таблицы
   oTsb:lNoPicture     := .T.
   oTsb:aName          := { "F_GRP", "F_WORK", "F_NAME", "F_KOLVO", "F_TOTAL", "F_DEVICE" , "F_MASTER", "F_CODE","F_KGRP", "F_KWORK" }
   oTsb:aItogo         := { "F_KOLVO", "F_TOTAL", "F_DEVICE" , "F_MASTER" }  // поля для суммирования внизу (подвал таблицы)
   ? ProcNL(), "######## nHCell=", nHCell

#ifdef KEY_ENG // for this project demo1-en.hbp
   //                    1                  2                       3                       4             5                 6                7                8        9  10
   oTsb:aHead := { "Group;requests" , "Group;works", "Name of equipment/works"        , "Quantity", "TOTAL;price", "EQUIPMENT;price" , "MASTER;price" , "Codes;equip.","","" }
#else
   oTsb:aHead := { "Группа;заявки" , "Группа;работ", "Наименование оборудования/работ", "Количество", "ОБЩАЯ;цена", "ОБОРУДОВ.;цена" , "МАСТЕРУ;цена" , "Коды;обор."  ,"","" }
#endif

   oTsb:aHideCol := {10,11}   // скрыть колонки, учитываем SELECTOR и ARRAYNO
   aWSize        := CalculatColumnWidthsTvr(aXDim,3,nWTsb)   // подсчёт ширины колонок - добавка во 2 колонку
   oTsb:aSize    := aWSize                                   // назначим ширину колонок для ТСБ

   // высоту можно задать и так
   nHFnt              := App.Cargo:nFontSize * 1.8
   //oTsb:nHeightHead := nHFnt                          // высота шапки
   //oTsb:nHeightFoot := nHFnt                          // высота подвала

   IF IsLogic(oTsb:lFooting) .AND. !oTsb:lFooting
      oTsb:lFooting    := .F.
      oTsb:aFoot       := .F.
   ELSE
      oTsb:lFooting    := .T.                            // поставить в таблице подвал
      oTsb:aFoot       := .T.                            // заполнить подвал
   ENDIF

   IF !IsLogic(oTsb:lSpecHd)
      oTsb:lSpecHd     := .F.                            // НЕ поставить в таблице нумератор
   ENDIF
   IF oTsb:lSpecHd
      oTsb:nHeightSpecHd := App.Cargo:nFontSize          // высота нумератора
   ENDIF

   aBClr               := This.Backcolor
   nClr1               := HMG_RGB2n(aBClr)                  // цвет фона шапка+подвал
   nClr2               := RGB( 48, 29,26)                   // серо-черный фон
   oTsb:aSuperHdColor  := {CLR_YELLOW, { nClr1, nClr2 } }   // цвет: текст и фон суперхидера
   oTsb:aBrush         := aBClr                             // цвет фона под таблицей
   // цвета в таблицу
   oTsb:lZebra    := .T.                                    // это вкл.\откл. механизм zebra
   //oTsb:aZebra  := { {230,230,230}, SILVER }              // серый цвет
   oTsb:aZebra    := { aBClr, {206,211,242} }
   a := {}
   AAdd(a, { CLR_HEADF, CLR_WHITE                })  // 3 , текста шапки таблицы
   AAdd(a, { CLR_HEADB, { nClr2, nClr1 }         })  // 4 , фона шапки таблицы
   AAdd(a, { CLR_FOCUSB, {|a,b,c| a := b, If( c:nCell == b, -CLR_HRED, -CLR_BLUE ) } } ) // 6 , фона курсора
   AAdd(a, { CLR_EDITF, CLR_YELLOW               })  // 7 , текста редактируемого поля
   AAdd(a, { CLR_EDITB, CLR_HRED                 })  // 8 , фона редактируемого поля
   AAdd(a, { CLR_FOOTF, CLR_WHITE                })  // 9 , текста подвала таблицы
   AAdd(a, { CLR_FOOTB, { nClr1, nClr2 }         })  // 10, фона подвала таблицы
   AAdd(a, { CLR_SPCF , CLR_RED                  })  // 18, specheader text - нумератор
   AAdd(a, { CLR_SPCB , RGB(240,240,240)         })  // 19, specheader back - нумератор
   oTsb:aColorAdd := a

   // блоки кода для _TBrowse(...) - название переменных bInit,bBody,bEnd,bAfter менять нельзя
   // ob == oBrw, op == oTsb, ob:Cargo:oParam == oTsb == op
   //oTsb:bInit  := {|ob,op| myTsbInit(ob,op)                   }  // настройки тсб
   //oTsb:bBody  := {|ob,op| myTsbKeyFX(ob,op), myTsbEdit(ob,op)}  // другие настройки тсб
   //oTsb:bAfter := {|ob,op| myTsbAfter(ob,op)                  }  // блок кода после END TBROWSE, чтобы не изменять oTsb:bEnd
   //oTsb:bEnd   := {|ob,op| myTsbEnd(ob,op) } // блок кода после END TBROWSE НЕ использовать
                                               // без необходимости, работает DEFAULT значение
                                               // !!! все делать в oTsb:bAfter !!!
   // Проверка
   //  ?v aXDim
   //  ?v oTsb:aName
   //  ?v oTsb:aHead
   //  ?v oTsb:aSize
   //
   // такой порядок работы блоков кода
   oTsb:bInit := {|ob,op| // настройки тсб
                   //ob:Hide()                                    // скрыть таблицу для дальнейшей прорисовки
                   ob:HideColumns( op:aHideCol ,.t.)              // скрыть колонки
                   ? "### oTsb:bInit", ProcNL(), HB_ValToExp(op:aHideCol)
                   ob:nFreeze     := ob:nColumn("F_WORK")         // Заморозить столбцы
                   ob:lLockFreeze := .T.                          // Избегать прорисовки курсора на замороженных столбцах
                   ob:nCell       := ob:nFreeze + 1               // передвинуть курсор
                   ob:lNoKeyChar  := .F.                          // ввод в ячейки от букв, цифр
                   myTsbEditTvr(ob,op)                            // редактирование ячеек таблицы
                   Return Nil
                   }

   oTsb:bBody := {|ob,op| // другие настройки тсб
                   Local oc, i := 0
                   Local nBClr := GetSysColor( COLOR_BTNFACE )   // цвет системный
                   Local hFont := GetFontParam(op:aFont[4])      // SpecHider
                   hFont := ob:aColumns[1]:hFontSpcHd            // 4-special header font - или так
                   ob:lPickerMode := .F.
                   //oc := ob:aColumns[2]
                   //oc:lEdit     := .T.
                   //oc:cPicture  := Nil
                   //oc:lCheckBox := .T.
                   //oc:nAlign    := DT_LEFT
                   //oc:nEditMove := 0    // перечитать ячейку
                   //IF ob:nHeightCell > 40
                   //   oc:aCheck := { LoadImage("bMgCheckT38"), LoadImage("bMgCheckF38") }
                   //ELSE
                   //   oc:aCheck := { LoadImage("bMgCheckT24"), LoadImage("bMgCheckF24") }
                   //ENDIF
                   FOR EACH oc IN ob:aColumns
                      oc:cSpcHeading := NIL
                      IF oc:cName == "ARRAYNO"
                         oc:cSpcHeading := "#"
                         oc:nClrBack    := nBClr            // RGB(240,240,240) // изменение цвета фона виртуальной колонки
                         oc:nClrFore    := CLR_RED          // изменение цвета текста виртуальной колонки
                         oc:hFont       := hFont            // изменение фонта виртуальной колонки
                         oc:bDecode     := {|cv| Alltrim(cv) }
                         oc:nAlign      := DT_CENTER
                         // здесь не работает уменьшение ширины колонки
                         //oc:nWidth := GetTextWidth( Nil, "0000", hFont )   // кол-во знаков
                      ELSE
                         oc:cSpcHeading := hb_ntos( ++i )
                      ENDIF
                   NEXT
                   ob:lNoHScroll := .T.   // нет показа горизонтального скролинга
                   ob:oHScroll   := NIL
                   ? "### oTsb:bBody", ProcNL(), ob:nHeightSuper, ob:lDrawSuperHd, ;
                     "Head=",ob:nHeightHead, "Foot",ob:nHeightFoot, op:cSuperHd
                   Return Nil
                   }

   oTsb:bAfter := {|ob|// после END TBROWSE
                    Local oc, nw := 0, nn
                    ? ProcNL(), "@@@ TSB @@@ width =", ob:GetAllColsWidth()
                    FOR EACH oc IN ob:aColumns
                       IF oc:cName == "ARRAYNO"
                          nn := oc:nWidth
                          // вот здесь делаем уменьшение ширины колонки
                          oc:nWidth := GetTextWidth( Nil, "00000", oc:hFont )
                          nn -= oc:nWidth
                       ENDIF
                       IF oc:lVisible ; nw += oc:nWidth
                       ENDIF
                    NEXT
                    IF !Empty(nn)
                       //oc := ATail(ob:aColumns)
                       oc := ob:GetColumn("F_NAME")
                       oc:nWidth += nn
                    ENDIF
                    ? repl("-", Len(ProcNL())), "=== TSB === nWidth =", nw ; ?
                    DO EVENTS
                    Return Nil
                    }

   // назначим клавиши в таблице --> tsb_util.prg
   oTsb:aUserKeys := { ;
        {VK_F2    , {|ob| _wPost(32, ob:cParentWnd, ob) } }, ;
        {VK_F3    , {|ob| _wPost(33, ob:cParentWnd, ob) } }, ;
        {VK_F4    , {|ob| _wPost(34, ob:cParentWnd, ob) } }, ;
        {VK_INSERT, {|ob| _wPost(10, ob:cParentWnd, ob) } }, ;
        {VK_DELETE, {|ob| _wPost(13, ob:cParentWnd, ob) } }  ;
                     }

   // назначить события на окно
   oTsb:aEvents   := { ;
        {32, {|ow,ky,ob| myTsbListColumn( ob ), ob:Setfocus(), ky:=ow:Name } }, ;
        {33, {|ow,ky,ob| myTsbListFont( ob )  , ob:Setfocus(), ky:=ow:Name } }, ;
        {34, {|ow,ky,ob| myTsbArrayLine( ob ) , ob:Setfocus(), ky:=ow:Name } }  ;
                     }
RETURN oTsb

///////////////////////////////////////////////////////////////////
// расчёт ширины колонок
STATIC FUNCTION CalculatColumnWidthsTvr(aXDim,nCol,nWTsb)
   LOCAL aDim, v, a, i, hFont, nW, aWSize

   aDim   := ACLONE(aXDim)
   hFont  := GetFontHandle("Normal")
   aWSize := Array(Len(aDim[1]))
   aFill(aWSize, 0)

   FOR EACH a IN aDim
       FOR EACH v IN a
           i := hb_enumindex(v)
           // показ только 2 колонок
           //IF i > nCol ; LOOP
           //ENDIF
           // делаем расчёт на все колонки, т.к. будет показ других колонок
           IF !IsChar(v) ; v := cValToChar(v)
           ENDIF
           v  += "HH"  // добавка
           nW := GetTextWidth( Nil, v, hFont )
           nW := IIF( nW > 400, 400, nW )
           aWSize[ i ] := MAX(nW,aWSize[ i ])
       NEXT
   NEXT

   //oTsb:aNumber := { 1, 30 }                // колонка нумерации и её ширина
   // для колонки 2 делаем всю ширину экрана показа, кроме колонки 1
   //aWSize[2] := nWTsb - aWSize[1] - GetHScrollBarHeight() - 30 - 1
   // ширина показа других колонок в зависимости от ширины текста
   ? ProcNL(), "aWSize=",aWSize ; ? HB_ValToExp(aWSize) , "nWTsb=",nWTsb,"nCol=",nCol
   //?? "Колонка:"+HB_NtoS(nCol)+"=",aWSize[2]

RETURN aWSize

////////////////////////////////////////////////////////////////////////////
// настройки редактирования, редактирование колонок
STATIC FUNCTION myTsbEditTvr( oBrw )
   LOCAL oCol, cCol, nI

   oBrw:SetAppendMode( .F. )     // запрещена вставка записи в конце базы стрелкой вниз
   oBrw:SetDeleteMode( .T., .F. )
   //oBrw:SetDeleteMode( .T., .T. ) - запрос на удаление

   FOR EACH oCol IN oBrw:aColumns
      nI   := hb_EnumIndex(oCol)
      cCol := oCol:cName
      //? "    .",nI, cCol
      IF cCol == "SELECTOR" .OR. cCol == "ORDKEYNO" .OR. cCol == "ARRAYNO" ; LOOP
      ENDIF
      // oCol:lEdit := .T. // задано выше oc:lEdit  := .T.
      // oCol:bPrevEdit := {|val, brw| myTsbEditPrev( val, brw ) }  // -> здесь не нужно
      // oCol:bPostEdit := {|val, brw| myTsbEditPost( val, brw ) }  // -> здесь не нужно
      IF "F_NAME" $ cCol .OR. "F_KOLVO" $ cCol
         oCol:lEdit     := .T.   // сделано выше
         //oCol:nEditMove := 0     // откл. перемещение курсора после :Edit()
         oCol:bPrevEdit := {|val, brw| myTsbEditPrev( val, brw ) }
         oCol:bPostEdit := {|val, brw| myTsbEditPost( val, brw ) }
      ENDIF
      IF oCol:cFieldTyp $ "+^="  // эти поля не редактируются - для массива не работает
         oCol:lEdit := .F.
         oCol:nClrFootFore := CLR_WHITE
         oCol:nClrFootBack := CLR_RED
      ENDIF
      //?? oCol:lEdit, oCol:cFieldTyp, oCol:nFieldLen, oCol:nFieldDec, oCol:cHeading
   NEXT

RETURN NIL

////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbEditPrev( uVal, ob )
   LOCAL nCol, oCol, cNam, cAls, uOld, lRet, cJTyp, nCode
   LOCAL cTyp, cMsg, lWrt, cStr, aVal, aRet, nAt, cName
   LOCAL aLng := ob:Cargo:aLang

   WITH OBJECT ob
      aVal    := :aArray[:nAt]         // вся строка массива
      nCol    := :nCell
      oCol    := :aColumns[ nCol ]
      nAt     := nAt                   // номер в массиве
      oCol:Cargo := oHmgData()         // создадим контейнер на колонку
      cAls    := :cAlias
      cTyp    := oCol:cFieldTyp        // тип обработки колонки
      cNam    := oCol:cName
      Default cJTyp := "+"
      IF cNam == "F_NAME"              // только это поле
         cJTyp := "CALC"
      ELSEIF cNam == "F_KOLVO"
         cJTyp := "N"
      ENDIF
   END WITH

   uOld := uVal
   ? ProcNL(), nCol, cNam, cTyp, Valtype(uVal)
   cStr := 'oCol:bPrevEdit !;'
   cStr += 'oCol:cName = ' + cNam + ";"
   //cStr += 'Column array type: "' + cTyp + '" ;'
   cStr += 'Column array cJTyp: "' + cJTyp + '" ;'
   cStr += 'NO processing for this field!;'
   lWrt := .F.                    // не записывать поле
   lRet := .F.                    // не давать редактировать поле в :get

   IF LEN(cJTyp) == 0             // нет обработки
   ELSEIF cJTyp == "0"            // нет обработки
   ELSEIF cJTyp $ "NLCD"
      lRet := .T.                 // редактировать поле в :get
      lWrt := .T.                 // записывать в ячейку
   ELSEIF cJTyp == "M"
      aRet := CellEditMemo(uVal, ob)   // -->> tsb_memo_cell.prg
      IF LEN(aRet) > 0
         uVal := aRet[1]
         lWrt := .T.                // записывать в ячейку
      ENDIF

   ELSEIF cJTyp $ "CALC"

      SET WINDOW THIS TO ob:cParentWnd
      aRet := Tsb_Tovar_Outfit(ob)        // -> tsb_tovar_outfit.prg
      IF LEN(aRet) > 0
         nCode := aRet[6]
         cName := Alltrim( SAY_SEL_DIM( nCode,'OB1ZAIV','OB1ZAIV'  ) )
         ob:SetValue("F_GRP"   , cName )
         nCode := aRet[7]
         cName := Alltrim( SAY_SEL_DIM( nCode,'OB2WORK','OB2WORK'  ) )
         ob:SetValue("F_WORK"  , cName   )
         ob:SetValue("F_NAME"  , aRet[1] )   // процессор СД-3099
         ob:SetValue("F_CODE"  , aRet[5] )   // код: процессор СД-3099
         ob:SetValue("F_KOLVO" , 0       )
         ob:SetValue("F_TOTAL" , aRet[2] )
         ob:SetValue("F_MASTER", aRet[3] )
         ob:SetValue("F_DEVICE", aRet[4] )
         ob:SetValue("F_KGRP"  , aRet[6] )
         ob:SetValue("F_KWORK" , aRet[7] )
         //ob:Reset() - не надо
         ob:DrawLine()                // перерисовать текущую строку таблицы
         lWrt := .T.                  // записывать в ячейку
      ENDIF

      SET WINDOW THIS TO

      ? "###### " + ProcNL(), cJTyp, "aRet=", aRet, hb_valtoexp(aRet)

   ELSE
      ? ProcNL(), "uVal=", uVal, HB_ValToExp(uVal)
      cMsg := ProcNL(0) + ";" + ProcNL(1) + ";;"
      AlertStop(cMsg + cStr,,,64,{RED})
   ENDIF

   //? ProcNL(), "#######-0", "lWrt=", lWrt, aRet, HB_ValToExp(aRet)
   IF lWrt                           // записывать ячейку
      //ob:Cargo:nModify ++          // счётчик-изменения в таблице
      //ob:SetValue(nCol,uVal)       // <== только при lRet == .F., в :bPostEdit ТСБ пишет сам, без твоего участия
   ENDIF
   //!!! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   //    ob:Cargo:nModify ++          // счётчик-изменения в таблице
   //    это делается в :bPostEdit при lRet == .T., используя значение в колонке
   //    IF oCol:xOldEditValue != uVal  <== :bPostEdit !!!
   //       ob:Cargo:nModify ++
   //    ENDIF
   //!!! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

   // Вот этот случай нужно делать !!! Иначе нет сохранения в базу для типа (3): SPR_A,CALC,SPR_J,SPR_S,CALC
   IF lWrt .AND. !lRet
      ob:Cargo:nModify ++          // счётчик-изменения в таблице
      //ob:SetValue(nCol,uVal)     // <== только при lRet == .F., в :bPostEdit ТСБ пишет сам, без твоего участия
      ob:DrawSelect()              // перерисовать текущую ячейку таблицы
      ob:SetFocus()
   ENDIF

   DO EVENTS

RETURN lRet

////////////////////////////////////////////////////////////////////////////
// Срабатывает после ввода в get
STATIC FUNCTION myTsbEditPost( uVal, ob )
   LOCAL nCol, oCol, cNam, uOld, cAls, lMod, cJTyp
   LOCAL oWnd  := _WindowObj(ob:cParentWnd)
   LOCAL cTyp, cMsg, cStr

   WITH OBJECT ob
      nCol := :nCell
      oCol := :aColumns[ nCol ]
      cNam := oCol:cName
      cTyp := oCol:cFieldTyp               // тип обработки колонки
      uOld := oCol:xOldEditValue           // old value
      lMod := ! uVal == uOld               // .T. - modify value
      cAls := :cAlias
      cJTyp := VALTYPE(uVal)              // тип обработки строки
   END WITH

   ? ProcNL(), nCol, cNam, cTyp, Valtype(uVal)
   cStr := 'oCol:bPrevEdit !;'
   cStr += 'oCol:cName = ' + cNam
   //cStr += ';Column processing type: "' + cTyp + '" ;'
   cStr += ';Column array cJTyp: "' + cJTyp + '" ;'

   IF cJTyp $ "CNDLM"
      // стандартная обработка
   ELSE
      cMsg := ProcNL(0) + ";" + ProcNL(1) + ";;"
      AlertInfo(cMsg + cStr,,,64,{RED})
      RETURN .F.
   ENDIF
   //    это делается в :bPostEdit при lRet == .T., используя значение в колонке
   IF oCol:xOldEditValue != uVal  // <== :bPostEdit !!!
      ob:Cargo:nModify ++         // счётчик-изменения в таблице
   ENDIF

   ? "###### " + ProcNL(), cJTyp, "[2]=", HB_ValToExp( ob:aArray[ob:nAt][ACOL_2] )
   ob:DrawSelect()    // перерисовать текущую ячейку таблицы
   ob:SetFocus()

   IF "F_KOLVO" $ cNam
      _wSend("_2Itogo",ob:cParentWnd)  // итого в подвале таблицы
   ENDIF

   DO EVENTS

RETURN .T.

///////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Itog_Table( ow )
   //oTsb:aItogo := { "F_KOLVO", "F_TOTAL", "F_DEVICE" , "F_MASTER" }
   Local aCol := ow:Cargo:aItog
   Local oBrw := ow:Cargo:oBrw
   Local aLine, nCol, oCol, aItg, cPic, cNam
   //     1       2               3       4      5       6      7      8
   //1  {"М", "проч.обор.", "K-DOM 750", 2.00, 1650.00, 0.00, 230.00, 277}
   //2  {"М", "аудио", "А/У DP-20H", 1.00, 700.00, 0.00, 70.00, 244}
   //3  {"Т/О", "замена/уст", "БВД Элтис DP 400-TD", 1.00, 0.00, 0.00, 0.00, 141}
   aItg := ARRAY(Len(aCol))
   AFILL(aItg,0)
   FOR EACH aLine IN oBrw:aArray
      nCol := hb_EnumIndex(aLine)
      IF IsNumeric(aLine[4]) .AND. IsNumeric(aLine[5]) .AND. ;
         IsNumeric(aLine[6]) .AND. IsNumeric(aLine[7])
         aItg[1] += aLine[4]
         aItg[2] += aLine[4] * aLine[5]
         aItg[3] += aLine[4] * aLine[6]
         aItg[4] += aLine[4] * aLine[7]
      ENDIF
   NEXT
   FOR EACH cNam IN aCol
      nCol := hb_EnumIndex(cNam)
      oCol := oBrw:GetColumn(aCol[nCol])
      cPic := IIF( nCol == 1, "@Z 999 999", "@Z 999 999.99" )
      oCol:cFooting := ALLTRIM(TRANSFORM(aItg[nCol],cPic))  // итоги в подвал таблицы
   NEXT
   oBrw:DrawFooters()
   DO EVENTS

RETURN Nil

