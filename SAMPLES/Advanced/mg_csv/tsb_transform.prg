/*
 * MINIGUI - Harbour Win32 GUI library
 *
 * Copyright 2025 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com>
 * Copyright 2025 Grigory Filatov <gfilatov@inbox.ru>
 *
 * TBrowse() вставка/удаление/преобразование колонок таблицы
 * TBrowse() insert/delete/transform table columns
*/
#define  _HMG_OUTLOG
#include "hmg.ch"
#include "tsbrowse.ch"
//////////////////////////////////////////////////////////////////////////
FUNCTION Table_Transf(oWnd, ky, cBtn)
   LOCAL nY, nX, cForm, hFont1, hFont2, cLang1

   ? ProcNL(), oWnd:ClassName, ky, cBtn

   cForm  := oWnd:Name
   hFont1 := GetFontHandle( "ComSanMS" )
   hFont2 := GetFontHandle( "DlgFont"  )
   // координаты вывода окна / window output coordinates
   nY     := GetProperty(cForm, "Row") + GetTitleHeight()
   nY     += GetProperty(cForm, cBtn, "Row") + GetProperty(cForm, cBtn, "Height")
   nX     := GetProperty(cForm, "Col") + GetBorderWidth()
   nX     += GetProperty(cForm, cBtn, "Col") - 4
   cLang1 := IIF( App.Cargo:lRu, "Преобразовать колонку" , "Transform column" )

   SET MENUSTYLE EXTENDED
   SetMenuBitmapHeight( 32 )

   DEFINE CONTEXT MENU OF &cForm
      MENUITEM cLang1 NAME SetWin1 ACTION {|| Tsb_1Transf(oWnd,cLang1) } ICON "iArrowC48x1" FONT hFont2
      SEPARATOR
   END MENU

   _ShowContextMenu(cForm, nY, nX, .f. )

   DO EVENTS

   DEFINE CONTEXT MENU OF &cForm
   END MENU

   DO EVENTS

RETURN NIL

//////////////////////////////////////////////////////////////////////////////////////
Function Tsb_1Transf(oWnd, cTtl)
   LOCAL cForm, cTitle, cIcon, owc, o, cFont, nFSize, cLng1, cLng2, nFSz1, aColmn
   LOCAL nY, nX, nW, nH, nG, aBClr, nY2, nX2, nW2, nH2, nWTtl, nHLbl, oBrw, aName
   LOCAL nWClmn, nWTmp, cText, nCombo, nJ, oCol, cCol, uData, nWBtn, nHBtn, oMenu

   ? ProcNL(), oWnd:Name
   // позиция окна по родительскому окну / window position relative to parent window
   cForm  := oWnd:Name + "_Tsb_1Transf"
   nY     := oWnd:Row
   nX     := oWnd:Col
   nW     := oWnd:Width
   nH     := oWnd:Height
   oBrw   := oWnd:Cargo:oBrw
   aBClr  := App.Cargo:aBClr
   cIcon  := "iArrowC48x1"
   cTitle := cTtl + SPACE(5) + ProcFile()
   // фонты на окне
   cFont  := _HMG_ActiveFontName
   nFSize := _HMG_ActiveFontSize
   nHLbl  := nFSize * 2
   cLng1  := IIF( App.Cargo:lRu, 'Преобразовать колонку типа "C" в "N"' , 'Convert column type "C" to "N"' )
   cLng2  := IIF( App.Cargo:lRu, 'Выберите колонку:' , 'Select a column:' )
   nFSz1  := 32
   nG     := 20   // между объектами на форме
   nWTtl  := GetTxtWidth( cLng1, nFSz1, cFont, .T. ) + nG * 2

   aColmn := {}
   aName  := {}
   nWClmn := nCombo := 0

   FOR EACH oCol IN oBrw:aColumns
       nJ   := hb_EnumIndex(oCol)
       cCol := oCol:cName
       AADD( aName, cCol )
       IF cCol == "SELECTOR" .OR. cCol == "ARRAYNO"
          cText := STR(nJ,2) + " - " + cCol
          AADD( aColmn, cText )
       ELSE
          uData := If( ValType( oCol:cHeading ) == "B", Eval( oCol:cHeading ), ;
                                oCol:cHeading )
          If ValType( uData ) != "C"
             uData := ""
          EndIf
          uData := StrTran( uData, CRLF, ";" )
          cText := STR(nJ,2) + " - " + uData
          AADD( aColmn, cText )
       ENDIF
       nWTmp  := GetTxtWidth( cText, nFSize, cFont, .T. )
       nWClmn := MAX(nWClmn , nWTmp)
   NEXT
   nWClmn += 55 // sign width addition [v]

   oMenu := BottomWindowMenu()  // кнопки нижнего меню / bottom menu buttons
   // высота нового окна
   nH2   := nFSz1*1.5 + nG + nHLbl + nG * 2 + oMenu:nHBtn + nG * 2
   nH2   += GetTitleHeight() + GetBorderHeight()

   // новое окно в координаты родительского окна
   nW2 := nWTtl // nW * 0.6
   //nH2 := 300   // nH * 0.4
   nX2 := ( nW - nW2 ) / 2 + nX
   nY2 := ( nH - nH2 ) / 2 + nY

   // ОБЯЗАТЕЛЬНО для окна MODAL / REQUIRED for MODAL window
   IF !Empty( _HMG_MainHandle ) ; _HMG_InplaceParentHandle := GetActiveWindow()
   ENDIF

   DEFINE WINDOW &cForm AT nY2,nX2 WIDTH nW2 HEIGHT nH2 TITLE cTitle ;
      MODAL NOSIZE ICON cIcon BACKCOLOR aBClr                        ;
      ON INIT    {|| _wSend(0)                    }                  ;
      ON RELEASE {|| /*This.Hide,*/ _wSend(90)    }   // модальное окно нельзя делать Hide
      This.Cargo := oHmgData() ; o := This.Object ; owc := o:Cargo

      owc:aBClr := This.Backcolor
      owc:ahIcoDel := {}       // для удаления хендлов иконок с формы
      owc:cTtl     := cLng1
      nY := nX := nG
      nW := This.ClientWidth
      nH := This.ClientHeight

      @ 0, 0 LABEL Buff VALUE "" WIDTH nW HEIGHT 5 TRANSPARENT

      @ nY, 0 LABEL Label_0 VALUE cLng1 WIDTH nW HEIGHT nFSz1*1.5 FONTCOLOR MAROON SIZE nFSz1 ;
        TRANSPARENT CENTERALIGN VCENTERALIGN
      nY += This.Label_0.Height + nG

      nWTmp := GetTxtWidth( cLng2, nFSize, cFont, .T. ) + 20
      nX    := ( nW - ( nWTmp + nWClmn + nG*3) ) / 2

      @ nY, nX LABEL Label_1 VALUE cLng2 WIDTH nWTmp HEIGHT nHLbl FONTCOLOR MAROON ;
        TRANSPARENT VCENTERALIGN
      nX += This.Label_1.Width + nG

      nCombo := 3
      @ nY, nX COMBOBOXEX CmBox_1 BOLD WIDTH nWClmn HEIGHT 300  ;
        ITEMS aColmn VALUE nCombo                               ;
        ON CHANGE {|| nCombo := This.Value, This.Buff.Setfocus }

      nY += This.Label_1.Height + nG*2

      nHBtn := oMenu:nHBtn
      nWBtn := oMenu:nWBtn
      nX    := (nW - nWBtn*2 - nG*3 ) / 2
      TopMenuButtons(owc,oMenu,nY,nX,nWBtn,nHBtn,nG)   // menu_topButtons.prg
      //nY  += owc:nHTBar - nG       // высота ToolBar
      //nY  -= GetBorderHeight()

      //@ nY, 0 LABEL Buff2 VALUE "" WIDTH nW HEIGHT 20 BACKCOLOR RED

      //ThisWindow.ClientHeight := nY  // обрежем окно, установим внешнюю ширину окна

      ON KEY F1     ACTION NIL
      ON KEY ESCAPE ACTION _wPost(98)

      // события на форме / events on the form
      o:Event( 0, {|ow| // ON INIT
                        ow:Setfocus("Buff")
                        DoEvents()
                        Return Nil
                        })

      o:Event({11,"_BtnOk"}, {|ow,ky,cn| //
                                           SET WINDOW THIS TO ow
                                           Tsb_1ColTransf(ow,ky,cn,oBrw,nCombo,aName,aColmn)
                                           SET WINDOW THIS TO
                                           ow:Enabler(cn, .T.)
                                           ow:Setfocus('Buff')
                                           _wSend(99,ow:Name)
                                           Return Nil
                                           } )

       o:Event({15,"_BtnExit"}, {|ow,ky,cn| _LogFile(.T., "  -->> Button:",cn, ow:Name, ky ) ,;
                                           _wSend(99,ow:Name) } )

       o:Event(90, {|ow,ky| // ON Release windows
                            Local ah
                            ?  ProcNL(), "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                            ? Repl(".", 10),"Delete handle icon - ow:Cargo:ahIcoDel="
                            ?? ah, HB_ValToExp(ah)
                            IF IsArray(ah)
                               AEval(ah, {|h| DestroyIcon(h) })  // удалить хендлы иконок
                            Endif
                            DO EVENTS
                            Return Nil
                            })

      o:Event(99, {|ow| ow:Release() })

   END WINDOW

   ACTIVATE WINDOW &cForm

   IF _IsWindowDefined(oWnd:Name)             // ОБЯЗАТЕЛЬНО / REQUIRED
      oWnd:SetFocus()
   ENDIF

   _HMG_InplaceParentHandle := 0   // ОБЯЗАТЕЛЬНО для окна MODAL / REQUIRED for MODAL window

   DO EVENTS

RETURN NIL

///////////////////////////////////////////////////////////////////
STATIC FUNCTION BottomWindowMenu()
   LOCAL oMenu := oHmgData()
   oMenu:aObj   := { "_BtnOk"  , "_BtnExit"   }
   oMenu:aIco   := { {"iMg_Ok48x1","iMg_Ok48x2"} , {"iMg_Cancel48x1","iMg_Cancel48x2"} }
   oMenu:aMnRu  := { "Преобразовать", "Отменить"   }
   oMenu:aMnEn  := { "Convert"      , "Undo"       }
   oMenu:aTipRu := { "Преобразовать колонку в таблице"   , "Выход из меню" }
   oMenu:aTipEn := { "Convert Column in Table"           , "Exit Menu"     }
   oMenu:aCap   := IIF( App.Cargo:lRu, oMenu:aMnRu , oMenu:aMnEn )
   oMenu:aTtip  := IIF( App.Cargo:lRu, oMenu:aTipRu, oMenu:aTipEn )
   oMenu:aFont  := { "Comic Sans MS", 18, .T., .F. , 14, "Increase button font size - reserve" }
   oMenu:aFClr  := { BLACK  , YELLOW }
   oMenu:aBClr  := { ORANGE , BLACK  }
   oMenu:nHIco  := 64         // 32,55  - задаём размер картинки на кнопке
   oMenu:nHIco  := IIF( App.Cargo:aDisplayMode[2] <= 720, 28, oMenu:nHIco )
   oMenu:nG     := IIF( App.Cargo:aDisplayMode[2] <= 720, 5, 10 )
   oMenu:nY     := oMenu:nG
   oMenu:nX     := oMenu:nG
   oMenu:nWBtn  := GetTxtWidth( oMenu:aCap[1], oMenu:aFont[2], oMenu:aFont[1], oMenu:aFont[3] )
   oMenu:nWBtn  += oMenu:nG*2 + oMenu:nHIco + oMenu:nG * 3
   oMenu:nHBtn  := oMenu:nHIco + oMenu:nG
   oMenu:lCaptu := .T.                                 // кнопка с надписями / button with inscriptions
   oMenu:nHMenu := oMenu:nY + oMenu:nHBtn + oMenu:nG   // высота вернего меню кнопок
   oMenu:lVertText := .F.                              // НЕ вертикальный текст
RETURN oMenu

/////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Tsb_1ColTransf(oWnd,ky,cn,oBrw,nI,cName,aColmn)
   LOCAL aLine, cCol, cStr, xVal, nJ, nSum, nVal, cMsg, cForm, cTitl, aSay
   LOCAL nPos, oCol, oCNew, aSum, oMsg, cLng1, cLng2, cLng3, cLng4, cLng0
   LOCAL c, cSim, o

   IF nI < 3 ; RETURN NIL
   ENDIF

   o     := App.Cargo
   cForm := oWnd:Name
   cTitl := oWnd:Cargo:cTtl
   nSum  := ky := cn
   cSim  := ",-./"
   cCol  := cName[nI]
   cStr  := aColmn[nI]
   xVal  := oBrw:GetValue(cCol)
   cLng0 := IIF( o:lRu, "Ошибка ! Колонка: ["+cStr+"];" , "Error ! Column: ["+cStr+"];")
   cLng1 := IIF( o:lRu, 'Количество символов' , 'Number of characters' )
   cLng2 := IIF( o:lRu, 'больше' , 'is greater than' )
   cLng3 := IIF( o:lRu, 'Это строка символов !;' , 'This is a string of characters!;' )
   cLng4 := IIF( o:lRu, "Отменить конвертацию;" , "Cancel conversion;" )

   IF VALTYPE(xVal) # "C"
      cMsg := cLng0
      cMsg += cLng4
      AlertExclamation(cMsg,App.Cargo:cTitle,,64,{ORANGE})
      RETURN NIL
   ENDIF

   oMsg := oHmgData()
   oMsg:Set(",", cLng1 + " <,> " + cLng2 + " 1;")
   oMsg:Set("-", cLng1 + " <-> " + cLng2 + " 1;")
   oMsg:Set(".", cLng1 + " <.> " + cLng2 + " 1;")
   oMsg:Set("/", cLng1 + " </> " + cLng2 + " 0;")

   cMsg := ""
   FOR EACH aLine IN oBrw:aArray
      nJ   := hb_EnumIndex(aLine)
      xVal := oBrw:aArray[nJ][nI-2]  // SELECTOR + ARRAYNO
      xVal := StrTran(xVal, " ", "")
      cMsg := ""
      FOR EACH cStr IN xVal  // строка как массив по одному символу
          IF !cStr $ "0123456789"
             IF cStr $ cSim ;  cMsg += oMsg:Get(cStr, "")
             ENDIF
             cMsg := cLng3 + cMsg
             EXIT
          ENDIF
      NEXT
      IF !Empty(cMsg) ; EXIT
      ENDIF
   NEXT

   IF LEN(cMsg) > 0
      cStr := cLng0
      cMsg += cLng4
      AlertExclamation(cStr + cMsg,App.Cargo:cTitle,,64,{ORANGE})
      RETURN NIL
   ELSE
      FOR EACH c IN cSim
          xVal := StrTran(xVal, c, "")
      NEXT
   ENDIF

   aSay := { cTitl, cStr, App.Exename }
   WaitWindow( aSay, .T., 800, 13, NIL, BLACK, App.Cargo:aBCAlert, 14, BLUE, 4 ) // open the wait window

   // добавим новую колонку в таблицу / let's add a new column to the table
   //oCol := oBrw:GetColumn(2)     // колонка ARRAYNO
   oCol  := oBrw:GetColumn(cCol)   // колонка ARRAYNO
   oCNew := oCol:Clone()
   nPos  := nI + 1
   oBrw:InsColumn(nPos, oCNew)
   oBrw:aColumns[nPos]:cPicture  := NIL
   oBrw:aColumns[nPos]:cDataType := "N"
   //oBrw:InsColumn( nPos, ColClone( oBrw:aColumns[ nI ] ) ) // можно и так
   nSum := 0
   aSum := {}
   FOR EACH aLine IN oBrw:aArray
      nJ   := hb_EnumIndex(aLine)
      xVal := oBrw:aArray[nJ][nI-2]  // SELECTOR + ARRAYNO
      //? "column check nI-2", nJ, xVal
      // убрать внутренние пробелы / remove internal spaces
      xVal := StrTran(xVal, " ", "")
      IF "," $ xVal
         xVal := StrTran(xVal, ",", ".")
      ENDIF
      nVal := VAL(xVal)
      nSum += nVal
      AADD( aSum, nVal )
      //?? "nVal=",nVal
   NEXT

   // удалить колонку в таблицу / delete a column in a table
   oBrw:DelColumn( nI )
   oBrw:Reset()
   oBrw:GoTop()
   // здесь уже другая колонка
   oCol := oBrw:aColumns[nI]
   oCol:cPicture  := NIL
   oCol:cDataType := "N"
   FOR nJ := 1 TO oBrw:nLen
      xVal := oBrw:GetValue(nI)
      //? "column check nI=", nI, cCol
      //?? nJ, "xVal=", xVal, aSum[nJ]
      oBrw:SetValue(nI,aSum[nJ])
      oBrw:Skip(1)
   NEXT

   WaitWindow()

   cStr := ALLTRIM(TRANSFORM(nSum,"9 999 999.99"))
   oCol := oBrw:aColumns[nI]
   oCol:cFooting := cStr
   oCol:nAlign   := DT_RIGHT
   oCol:nFAlign  := DT_RIGHT
   oBrw:DrawFooters()
   oBrw:Reset()
   oBrw:Refresh(.T.)       // перечитывает данные в таблице
   oBrw:GoTop()
   oBrw:SetFocus()
   DO EVENTS

RETURN NIL

