/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2025 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * _TBrowse() Показ файлов .txt .csv .arr
 *            Преобразование колонок, поиск по массиву
 * _TBrowse() Displaying .txt .csv .arr
 *            Transforming columns, searching by array
*/
#define  _HMG_OUTLOG
#include "hmg.ch"
#include "tsbrowse.ch"
///////////////////////////////////////////////////////////////////
FUNCTION TopMenuTsb()
   LOCAL oMenu := oHmgData()
   oMenu:aObj   := { "_WTransf"  , "_WExport"  , "_WExit"   }
   oMenu:aIco   := { {"iArrowC48x1","iArrowC48x2"} , {"iExport48x1","iExport48x2"} ,;
                     {"iExit48x1","iExit48x2"} }
   oMenu:aMnRu  := { "Действия", "Экспорт" , "Выход"  }
   oMenu:aMnEn  := { "Actions", "Export"  , "Exit"    }
   oMenu:aTipRu := { "Действия в таблице"   , "Экспорт таблицы", "Выход из таблицы" }
   oMenu:aTipEn := { "Actions in the table" , "Export table"   , "Exit from table"  }
   oMenu:aCap   := IIF( App.Cargo:lRu, oMenu:aMnRu , oMenu:aMnEn )
   oMenu:aTtip  := IIF( App.Cargo:lRu, oMenu:aTipRu, oMenu:aTipEn )
   oMenu:aFont  := { "Comic Sans MS", 12, .T., .F. , 14, "Increase button font size - reserve" }
   oMenu:aFClr  := { BLACK  , YELLOW }
   oMenu:aBClr  := { LGREEN , BLACK  }
   oMenu:nHIco  := 40          // 32,55  - размер картинки на кнопке / image size on the button
   oMenu:nHIco  := IIF( App.Cargo:aDisplayMode[2] <= 720, 28, oMenu:nHIco )
   oMenu:nG     := IIF( App.Cargo:aDisplayMode[2] <= 720, 5, 10 )
   oMenu:nY     := oMenu:nG
   oMenu:nX     := oMenu:nG
   oMenu:nWBtn  := oMenu:nHIco + oMenu:nG
   oMenu:nHBtn  := oMenu:nHIco + oMenu:nG
   oMenu:lCaptu := .F.                                 // кнопки без надписей / buttons without labels
   oMenu:nHMenu := oMenu:nY + oMenu:nHBtn + oMenu:nG   // высота вернего меню кнопок
RETURN oMenu

///////////////////////////////////////////////////////////////////
Function Table_Csv(oWnd, nJ, cFile)
   LOCAL cForm, nH1, nH3, nHMenu, nY, nX, nHTbl, nWTbl, nI, cFont2
   LOCAL o, owc, oTsb, oBrw, nW, nH, nK, cMsg, aBClr, aTxt, nFSiz2
   LOCAL oMenu, lIgn, aBClr1, aBClr3, aFClr1, aFClr3, lOem, lUtf
   LOCAL nHMain, l124, cFont := "Lucida Console", nFSize := 13
   LOCAL nWGb, nXGb, lCntr, lErr, cTitle := App.Cargo:cTitle
   LOCAL lStop, oGet, aFont
   LOCAL aIgnor := { ".exe", ".prg", ".htm", ".ini", ".log", ".7z" }

   ? ProcNL(), oWnd, nJ, cFile
   nHMain := oWnd:Cargo:nHMain
   IF !FILE(cFile)
      cMsg := IIF( App.Cargo:lRu, "Ошибка ! Нет файла !;" ,;
                   "Error ! No such file !;")
      cMsg += cFile
      AlertExclamation(cMsg,cTitle,,64,{ORANGE})
      RETURN ""
   ENDIF
   lIgn := .F.
   FOR nI := 1 TO LEN(aIgnor)
      IF aIgnor[nI] $ LOWER(cFile)
         lIgn := .T.
      ENDIF
   NEXT
   IF lIgn
      cMsg := IIF( App.Cargo:lRu, "Ошибка ! Файл этого типа игнорируется !;" ,;
                   "Error! File of this type is ignored !;")
      cMsg += HB_ValToExp(aIgnor) + ";"
      cMsg += cFile
      AlertExclamation(cMsg,cTitle,,64,{ORANGE})
      RETURN ""
   ENDIF
   //
   oMenu  := TopMenuTsb()            // параметры верхнего меню
   nHMenu := oMenu:nHMenu            // высота вернего меню кнопок
   cForm  := "Form_" + HB_NtoS(nJ)
   aBClr  := oWnd:Cargo:aBClr
   aBClr1 := LGREEN
   aBClr3 := LGREEN
   aFClr1 := WHITE
   aFClr3 := YELLOW
   cTitle := "CSV(" + HB_NtoS(nJ) + ") " + cFileNoPath(cFile)

   aTxt   := Csv_Reestr(cFile,@lOem,@lUtf,@l124,@lErr)   // разборка файла на части
   // aTxt: [1]-заголовок реестра, [2]-сама таблица, [3]-подвал реестра, [4]-шапка, [5]-lStop
   lStop  := aTxt[5]
   IF lStop  ;  RETURN ""
   ENDIF

   oTsb   := oTsb_Def(,aBClr,aTxt[2],aTxt[4],cFile)  // построение таблицы: [2]-таблица, [4]-шапка
   oTsb:cSuperHd := cFile
   aFont  := GetFontParam( GetFontHandle( oTsb:aFont[1] ) )
   cFont2 := aFont[1]
   nFSiz2 := aFont[2]

   nH1 := nH3 := 0
   IF LEN(aTxt[1]) > 0
      nH1 := App.Object:H(0.9)*5  //!!! 5 строк
      //nH1 := INT( GetFontHeight(oTsb:aFont[1])*5 )   // 5 строки !!! только целые числа
   ENDIF
   IF LEN(aTxt[3]) > 0
      nH3 := App.Object:H(0.9)*2  //!!! 2 строки
      //nH3 := INT( GetFontHeight(oTsb:aFont[1])*2 )   // 2 строки !!! только целые числа
   ENDIF

   nK    := Len(oTsb:uAlias)                                 // длина массива
   nK    := iif( nK < 5, 5, nK )                             // 5 строк таблицы, если мало строк
   //
   nHTbl := oTsb:nHeightCell * nK                            // сетка от высоты фонта
   nHTbl += oTsb:nHeightFoot + oTsb:nHeightHead + oTsb:nHeightSuper + oTsb:nHeightSpecHd*2
   nHTbl += GetHScrollBarHeight()
   nHTbl += App.Object:H(0.5)                     // добавка для дырки и :SetNoHoles()
   //
   nH    := nHTbl                                 // высота таблицы
   nH    += nH1 + nH3                             // заголовок и подвал реестра
   nH    += nHMenu                                // высота вернего меню

   nW    := Sys.ClientWidth
   IF nH > Sys.ClientHeight
      nH    := Sys.ClientHeight - GetTaskBarHeight()  // высота Панели задач Desktop
      nHTbl := nH - nHMenu - nH1 - nH3
      nHTbl -= oTsb:nHeightFoot  - oTsb:nHeightHead
      nHTbl -= oTsb:nHeightSuper - oTsb:nHeightSpecHd
      nHTbl += GetHScrollBarHeight() + App.Object:H(0.3)  // добавка для дырки и :SetNoHoles()
      nY    := 0
      lCntr := .F.
   ELSE
      nY    := nHMain + (nJ-1) * 60
      lCntr := .T.
   ENDIF

   nX     := 0

   DEFINE WINDOW &cForm AT nY, nX CLIENTAREA nW, nH   ;
      TITLE cTitle                                    ;
      WINDOWTYPE STANDARD TOPMOST NOMAXIMIZE NOSIZE   ;
      BACKCOLOR aBClr  FONT cFont SIZE nFSize         ;
      ON INIT    ( This.Topmost := .F., _wPost(0) )   ;
      ON RELEASE ( This.Hide, _wSend(90) )
      This.Cargo := oHmgData() ; o := This.Object ; owc := o:Cargo

      owc:ahIcoDel := {}       // для удаления хендлов иконок с формы
      owc:cFile    := cFile
      nY := nX := 0
      nW := This.ClientWidth
      nH := This.ClientHeight

      @ nY, nX LABEL Buff VALUE "" WIDTH nW HEIGHT 10 ;
        FONTCOLOR LGREEN TRANSPARENT RIGHTALIGN

      oMenu:aBClr := { aBClr, LGREEN  }              // change the background color of buttons
      TopMenuButtons(owc,oMenu)                      // menu_topButtons.prg
      owc:nX := owc:nWBtnEnd                         // конец кнопок

      IF !lErr
         owc:cStr1 := IIF( App.Cargo:lRu, "   Кол-во строк: " , "  Number of lines: " ) + HB_NtoS(nK)
         owc:cStr2 := IIF( App.Cargo:lRu, "Кол-во столбцов: " , "Number of columns: " )
         owc:cStr3 := HB_NtoS( LEN(oTsb:aHead) )
         owc:nWStr := GetTxtWidth(owc:cStr1, nFSize, cFont, .T. ) + 25

         @  5, owc:nX LABEL Lbl_1 VALUE owc:cStr1 WIDTH owc:nWStr HEIGHT 20 BOLD ;
           FONTCOLOR LGREEN TRANSPARENT VCENTERALIGN

         @ 25, owc:nX LABEL Lbl_2 VALUE owc:cStr2 + owc:cStr3 WIDTH owc:nWStr HEIGHT 20 BOLD ;
           FONTCOLOR LGREEN TRANSPARENT VCENTERALIGN

         owc:cStr4 := IIF( App.Cargo:lRu, "Кодировка файла: " , "File encoding: " )
         owc:cStr5 := IIF( lOem, "RU866", "RU1251"  )
         owc:cStr5 := IIF( lUtf, "UTF8" , owc:cStr5 )
         owc:nX    += owc:nWStr
         owc:nWCod := GetTxtWidth(owc:cStr4 + owc:cStr5, nFSize, cFont, .T. ) + 5

         @ 5, owc:nX LABEL Lbl_3 VALUE owc:cStr4 + owc:cStr5 WIDTH owc:nWCod HEIGHT 20 BOLD ;
           FONTCOLOR LGREEN TRANSPARENT VCENTERALIGN

         owc:cStr6 := IIF( App.Cargo:lRu, "Разделитель: " , "Separator: " )
         owc:cStr7 := IIF( l124, "<|>", "<;>"  )
         owc:nWSep := GetTxtWidth(owc:cStr6 + owc:cStr7, nFSize, cFont, .T. ) + 5

         @ 25, owc:nX LABEL Lbl_4 VALUE owc:cStr6 + owc:cStr7 WIDTH owc:nWSep HEIGHT 20 BOLD ;
           FONTCOLOR LGREEN TRANSPARENT VCENTERALIGN
         owc:nX    += owc:nWCod + 20

         owc:cSrch  := IIF( App.Cargo:lRu, "Поиск:" , "Search:" )
         owc:nWSrch := GetTxtWidth(owc:cSrch, nFSize, cFont, .T. ) + 5

         @ 8, owc:nX LABEL Lbl_5 VALUE owc:cSrch WIDTH owc:nWSrch HEIGHT 25 BOLD ;
           FONTCOLOR BLACK TRANSPARENT VCENTERALIGN

         // GetBox для поиска в таблицы
         nWGb := 150
         nXGb := owc:nX + owc:nWSrch
         //@ 8, nXGb LABEL Lbl_6 VALUE "" WIDTH 120 HEIGHT 25 BACKCOLOR WHITE BORDER

         @ 8, nXGb GETBOX GB_Find OBJ oGet WIDTH nWGb HEIGHT 25  VALUE " "  ;
           PICTURE "@K "+Repl("X", 30) NOTABSTOP ;
           ACTION       {|| This.Value := "" }   ;
           IMAGE        { "bDelRed24","" }       ; // tobegin, tofix, collect
           BUTTONWIDTH  25                       ;
           ON GOTFOCUS  {|ob| ob := ThisWindow.Cargo:oBrw, ob:nCell := 5, ob:DrawSelect() } ;
           ON CHANGE    {|| Search_ArrTSB( ThisWindow.Object ) } ;
           ON INIT      {|| This.Cargo := .T. }

         This.Cargo:oGet := oGet
         This.Cargo:cGet := "GB_Find"    // запомнить для дальнейшего использования
         This.Cargo:nGB_Find := 0

         owc:nX   := nXGb + This.GB_Find.Width + 25
         owc:cF1  := IIF( App.Cargo:lRu, "F1: Преобразовать текущую колонку" ,;
                                         "F1: Transform current column" )
         owc:nWF1 := GetTxtWidth(owc:cF1, nFSize, cFont, .T. ) + 5

         @ 5, owc:nX LABEL Lbl_6 VALUE owc:cF1 WIDTH owc:nWF1 HEIGHT 20 BOLD ;
           FONTCOLOR LGREEN TRANSPARENT VCENTERALIGN

         owc:cColW  := IIF( App.Cargo:lRu, "Расчёт ширины колонок по строкам:" ,;
                                           "Calculation of column widths by rows:" )
         owc:cColS  := IIF(App.Cargo:lColumnWidthFull, " ON", " OFF")
         owc:nWCol := GetTxtWidth(owc:cColW + owc:cColS, nFSize, cFont, .T. ) + 5
         @ 25, owc:nX LABEL Lbl_7 VALUE owc:cColW + owc:cColS WIDTH owc:nWCol HEIGHT 20 BOLD ;
           FONTCOLOR LGREEN TRANSPARENT VCENTERALIGN

      ENDIF
      nY += oMenu:nHMenu

      @ nY, nX EDITBOX Edit_Memo1 WIDTH nW HEIGHT nH1 VALUE aTxt[1] FONT cFont2 SIZE nFSiz2 ;
        BACKCOLOR aBClr3 FONTCOLOR aFClr3 MAXLENGTH 1200 NOHSCROLL READONLY
      nY += This.Edit_Memo1.Height

      nWTbl    := nW
      oBrw     := _TBrowse( oTsb, aTxt[2], "cBrw", nY, nX, nWTbl, nHTbl )
      oBrw:Cargo:aIsx := oBrw:aArray   // !!! запомнили на окне, иначе не будет поиска Search_Arr1TSB()
                                       // !!! Remember this on the window, otherwise there will be no search Search_Arr1TSB()
      owc:oBrw := oBrw                 // запомнили на окне

      //nY += nHTbl
      nY := nH - nH3

      @ nY, nX EDITBOX Edit_Memo3 WIDTH nW HEIGHT nH3 VALUE aTxt[3] FONT cFont2 SIZE nFSiz2 ;
        BACKCOLOR aBClr3 FONTCOLOR aFClr3 MAXLENGTH 200 NOHSCROLL READONLY

      //ON KEY F1     ACTION NIL
      ON KEY F1 ACTION {|| ONKEYF1() }
      ON KEY ESCAPE ACTION ThisWindow.Release

      o:Event( 0, {|ow| // ON INIT
                       ow:Setfocus('Buff')
                       ow:Cargo:oBrw:Setfocus()
                       Return Nil
                       })

      o:Event({11,"_WTransf"}, {|ow,ky,cn| //
                                           SET WINDOW THIS TO ow
                                           Table_Transf(ow,ky,cn)  // tsb_transform.prg
                                           SET WINDOW THIS TO
                                           ow:Enabler(cn, .T.)
                                           ow:Setfocus('Buff')
                                           Return Nil
                                           } )

      o:Event({12,"_WExport"}, {|ow,ky,cn| //
                                           Local ob := ow:Cargo:oBrw
                                           SET WINDOW THIS TO ow
                                           Menu_Export(ow,ky,cn,ob) // tsb_export.prg
                                           SET WINDOW THIS TO
                                           ow:Enabler(cn, .T.)
                                           ow:Setfocus('Buff')
                                           Return Nil
                                           } )

       o:Event({15,"_WExit"}, {|ow,ky,cn| _LogFile(.T., "  -->> Button:",cn, ow:Name, ky ) ,;
                                          _wSend(99,ow:Name) } )

       o:Event(90, {|ow,ky| // ON Release windows
                            Local cm, ct, ah
                            cm := ProcNL()
                            ct := HMG_TimeMS( App.Cargo:tStart )
                            ?  cm, "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                            ?  Repl(".", Len(cm)), "=> RELEASE WINDOW <=", ow:Name
                            ?? "... Program running time -", ct
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

   IF lCntr
     // CENTER WINDOW &cForm
   ENDIF
   //ACTIVATE WINDOW &cForm

RETURN cForm

//////////////////////////////////////////////////////////////////////
FUNCTION Search_ArrTSB(oWnd)
   LOCAL nVer := MiniGuiVersionNumba()
   DEFAULT oWnd := ThisWindow.Object

   IF nVer == 231290 .OR. nVer > 250900
      Search_Arr2TSB( oWnd )    // Option 2
   ELSE
      Search_Arr1TSB( oWnd )    // Option 1
   ENDIF

RETURN NIL

//////////////////////////////////////////////////////////////////////
FUNCTION Search_Arr1TSB(oWnd)
   LOCAL oBrw  := oWnd:Cargo:oBrw
   LOCAL cGet  := oWnd:Cargo:cGet             // это "GB_Find"
   LOCAL cVal  := trim( This.&(cGet).Value )
   LOCAL nBtn  := oWnd:Cargo:Get("n"+cGet, 0)
   LOCAL lSwap := .F.
   LOCAL nLen  := Len( cVal )
   LOCAL aRow, xVal, aNew := {}, a

   //? "~~~~~~~>>>", cGet, nBtn, cVal
   IF     nLen == 0
      oBrw:aArray := oBrw:Cargo:aIsx
      lSwap := .T.
   ELSEIF nLen > 2    // от 3-х символов поиск
     //oBrw:FilterFTS( cVal, .T. )         // нельзя использовать для ТСБ-массива
     //
     cVal := upper(cVal)
     aNew := {}
     FOR EACH aRow IN oBrw:Cargo:aIsx
        FOR EACH xVal IN aRow
            IF cVal $ UPPER(xVal)
               AAdd(aNew, AClone(aRow))
               EXIT
            ENDIF
        NEXT
     NEXT
     IF Len(aNew) = 0
        a := array( Len(oBrw:aArray[1]) )
        a[1] := 'No line"' + cVal + '" ....'
        a[2] := 'Нет строки: "' + cVal + '" ....'
        a[3] := 'No line: "' + cVal + '" ....'
        a[4] := 'Нет строки: "' + cVal + '" ....'
        a[5] := 'No line: "' + cVal + '" ....'
        a[6] := 'Нет: "' + cVal + '" ....'
        AAdd( aNew, a )
     ENDIF
     oBrw:aArray := aNew
     lSwap := .T.

   ENDIF

   IF lSwap
      oBrw:nCell := 3
      oBrw:Reset()
      EVal(oBrw:Cargo:oParam:b_Total_Sum, oBrw)   // sum total
      oBrw:Refresh()
      DO EVENTS
   ENDIF

RETURN NIL

//////////////////////////////////////////////////////////////////////
STATIC FUNCTION Search_Arr2TSB(oWnd)
   LOCAL oBrw  := oWnd:Cargo:oBrw
   LOCAL cGet  := oWnd:Cargo:cGet             // это "GB_Find"
   LOCAL cVal  := trim( This.&(cGet).Value )
   LOCAL nBtn  := oWnd:Cargo:Get("n"+cGet, 0)
   LOCAL lSwap := .F.
   LOCAL nLen, xRet

   nLen := Len( cVal )
   //? "~~~~~~~>>>", cGet, nBtn, cVal, nLen
   IF     nLen == 0
      xRet := oBrw:FilterFTS()
      lSwap := .T.
   ELSEIF nLen > 2    // от 3-х символов поиск
     xRet := oBrw:FilterFTS( cVal, .T. )
     //?? "xRet=",xRet
     IF Empty(xRet)  //xRet == 0
        oBrw:SetValue(oBrw:nColumn("ARRAYNO")+1, "*** not found !")
        oBrw:SetValue(oBrw:nColumn("ARRAYNO")+2, "*** not found !")
        oBrw:SetValue(oBrw:nColumn("ARRAYNO")+3, "*** not found !")
     ENDIF
     lSwap := .T.
   ENDIF

   IF lSwap
      oBrw:nCell := 3
      // oBrw:Reset()
      // DO EVENTS
      EVal(oBrw:Cargo:oParam:b_Total_Sum, oBrw)   // sum total
      oBrw:Refresh()
      DO EVENTS
   ENDIF

RETURN NIL

/////////////////////////////////////////////////////////////////////////
FUNCTION oTsb_Def(oTsb,aBClr,aDim,aHead,cFile)
   LOCAL a, i, j, k, m, t, aName, cMsg, nLine, aCol
   DEFAULT aHead := {}

   Default oTsb := oHmgData()
   oTsb:aEdit       := .T.
   oTsb:aFoot       := .T.
   oTsb:uSelector   := 20
   oTsb:aNumber     := { 1, App.Object:W(0.5) }
   oTsb:aFoot       := .T.
   oTsb:lSuperHd    := .T.
   oTsb:cSuperHd    := "TEST cSuperHd"
   oTsb:lSpecHd     := .F.
   oTsb:cSpecHdChar := "#"
   oTsb:uAlias      := aDim
   //                      cell     Head    Foot   SpecHider  SuperHider  Edit
   //oTsb:aFont     := { "Normal", "Bold", "Bold", "Italic" , "Bold"    , "Normal" }
   oTsb:aFont       := { "Normal", "Bold", "Italic", "SpecHdr" , "SuperHdr", "TsbEdit" }

   a := aDim[1]
   m := aHead
   IF !IsArray(m)
      cMsg := "Error! aHead - not an array !;"
      IF !IsString(m)
         m := cValToChar(m)
      ENDIF
      cMsg += "aHead = " + m
      cMsg += ";;" + ProcNL() + ";" + ProcNL(1)
      AlertStop(cMsg,,,64,{RED})
   ENDIF

   IF LEN(aHead) == 0
      aHead := {}
      FOR i := 1 TO LEN(a)
         AADD( aHead, "(" + HB_ValToExp(i) + ")" )
      NEXT
   ELSE
      oTsb:lSpecHd     := .T.    // включить нумерацию колонок
      IF LEN(a) # LEN(aHead)
         cMsg := cFile + ";;"
         cMsg += "Error! The arrays do not match !;"
         cMsg += "aDim[1]=" + HB_NtoS(LEN(a)) + " # "
         cMsg += "aHead=" + HB_NtoS(LEN(aHead))
         cMsg += ";;" + ProcNL() + ";" + ProcNL(1)
         AlertStop(cMsg,,,64,{RED})
         ? cMsg ; ? HB_ValToExp(aHead)
         IF LEN(a) < LEN(aHead)
            aName := {}
            FOR i := 1 TO LEN(a)
               AADD( aName, aHead[i] )
            NEXT
            aHead := aName
         ELSE
            FOR i := LEN(aHead) TO LEN(a)
               AADD( aHead, "+++" + HB_NtoS(i) )
            NEXT
         ENDIF
      ENDIF
   ENDIF

   aName := {}
   FOR i := 1 TO LEN(a)
       AADD( aName, "COL_" + HB_ValToExp(i) )
   NEXT

   oTsb:aName := aName
   oTsb:aHead := aHead

   m := AClone(oTsb:aHead)
   FOR i := 1 TO Len(m)
       IF ";" $ oTsb:aHead[ i ]
          oTsb:aHead[ i ] := StrTran(oTsb:aHead[ i ], ";", CRLF)
          t := ""            // выделим max строку массива
          FOR EACH k IN hb_ATokens(m[ i ], ";")
              IF len(k) > len(t) ; t := k
              ENDIF
          NEXT
          m[ i ] := t      // строки max длины для header расчета ширины
       ENDIF
   NEXT

   // расчёт 1-го элемента массива на кол-во знаков в колонках
   // calculation of the 1st element of the array for the number of characters in columns
   oTsb:aSizeLen := {}
   FOR EACH j, t IN aDim[1], m
       k := Len(hb_valtoexp(j))   // xVal в aDim[1]
       k := Max(k, len(t)) + 2
       IF k > 10 ; k := int( k * 0.9 )
       ENDIF
       AAdd(oTsb:aSizeLen, k ) // Len колонки
   NEXT
   //
   Default App.Cargo:lColumnWidthFull := .F.
   IF App.Cargo:lColumnWidthFull
      // полный перебор всего массива на кол-во знаков в колонках !!! убрать на большие массивы - в настройке
      // full iteration of the entire array for the number of characters in the columns !!! remove for large arrays
      aCol := ACLONE(aDim)
      oTsb:aSizeLen := Array(Len(aCol[1]))
      aFill(oTsb:aSizeLen, 0)

      FOR EACH a IN aCol
          FOR EACH m IN a
             j := hb_enumindex(m)
             IF !IsChar(m) ; m := cValToChar(m)
             ENDIF
             m += "HH"  // additive for parameter [oBrw:nCellMarginLR]
             oTsb:aSizeLen[ j ] := MAX( LEN(m), oTsb:aSizeLen[ j ])
          NEXT
      NEXT
   ENDIF

   nLine := 0
   m     := AClone(oTsb:aHead)
   FOR i := 1 TO Len(m)
      nLine := MAX( nLine, NumAt( CRLF , m[i] ) )
   NEXT
   nLine := IIF( nLine==0, 1, nLine-1)

   oTsb:nHeightHead   := App.Object:H(1.1) * nLine
   oTsb:nHeightCell   := App.Object:H(1.1)

   IF oTsb:aFoot
      oTsb:nHeightFoot   := App.Object:H(1.0)           // высота подвала
   ELSE
      oTsb:nHeightFoot   := 0
   ENDIF

   IF oTsb:lSuperHd
      oTsb:nHeightSuper  := App.Object:H(1.2)           // высота суперхидера
   ELSE
      oTsb:nHeightSuper  := 0
   ENDIF

   IF oTsb:lSpecHd
      oTsb:nHeightSpecHd := App.Object:H(0.8)            // высота спецхидера ENUMERATOR
   ELSE
      oTsb:nHeightSpecHd := 0
   ENDIF
   // ======================
   oTsb:l_Total_Sum := .T.
   // ======================
   // цвета в таблицу
   oTsb:lZebra        := .T.
   oTsb:aZebra        := { aBClr , {95,207,113} }
   oTsb:aBrush        := aBClr                              // цвет фона под таблицей

   oTsb:nClr1         := HMG_RGB2n(LGREEN)                  // цвет фона шапка+подвал
   oTsb:nClr2         := RGB( 48, 29,26)                    // серо-черный фон
   oTsb:aClr1         := { oTsb:nClr1, oTsb:nClr2 }         // цвет фона шапка
   oTsb:aClr2         := { oTsb:nClr2, oTsb:nClr1 }         // цвет фона подвал
   oTsb:aSuperHdColor := { CLR_YELLOW, oTsb:aClr1 }         // цвет: текст и фон суперхидера
   oTsb:aNumber_nBClr := GetSysColor( COLOR_BTNFACE )       // system color
   oTsb:aNumber_nFClr := CLR_RED

   a := oTsb:aColorAdd ; Default a := {}
   AAdd(a, { CLR_TEXT , CLR_BLACK  })  // 1 , цвет текста ячеек
   // 2 , фона в ячейках таблицы
   //AAdd(a, { CLR_PANE, {|nr,nc,ob,nd| nr := CLR_BLACK, nc := CLR_WHITE, nd := CLR_HGRAY, ;
   //                      iif( (ob:cAlias)->(DELETED()), nr, iif( ob:nAt % 2 == 0, nc, nd ) ) } } )
   AAdd(a, { CLR_HEADF, CLR_WHITE  })  // 3 , текста шапки таблицы
   AAdd(a, { CLR_HEADB, oTsb:aClr2 })  // 4 , фона шапки таблицы
                                       // 6 , фона курсора
   AAdd(a, { CLR_FOCUSB, {|a,b,c| a := b, iif( c:nCell == b, -CLR_HRED, -CLR_BLUE ) } } ) // 6
   AAdd(a, { CLR_EDITF, CLR_YELLOW })  // 7 , текста редактируемого поля
   AAdd(a, { CLR_EDITB, CLR_HRED   })  // 8 , фона редактируемого поля
   AAdd(a, { CLR_FOOTF, CLR_WHITE  })  // 9 , текста подвала таблицы
   AAdd(a, { CLR_FOOTB, oTsb:aClr1 })  // 10, фона подвала таблицы
   AAdd(a, { CLR_SPCF , CLR_YELLOW })  // 18, specheader text - нумератор
   AAdd(a, { CLR_SPCB , oTsb:aClr1 })  // 19, specheader back - нумератор
   //
   oTsb:aColorAdd     := a

   // блоки кода в таблицу
   // separate block for :aNumber
   oTsb:b_aNumber := {|ob,op,nCol,cCol|
          Local oc := ob:aColumns[ nCol ], hFont
          Local nBClr := ob:Cargo:oParam:aNumber_nBClr
          Local nFClr := ob:Cargo:oParam:aNumber_nFClr
          Default nBClr := GetSysColor( COLOR_BTNFACE ) // system color
          Default nFClr := CLR_RED
          ? "### Additional settings connected oTsb:b_aNumber"
          If IsArray(op:aFont)
             hFont := GetFontHandle(op:aFont[4])        // SpecHider
          Else
             hFont := ob:aColumns[1]:hFontSpcHd         // 4-special header font
          Endif
          oc:nClrBack := nBClr
          oc:nClrFore := nFClr
          oc:hFont    := hFont
          //oc:bDecode  := {|cv| iif( IsChar(cv), Alltrim(cv), cv ) }
          // !!! можно так для красоты, без дальнейших преобразований колонок
          // !!! You can do this for beauty, without further transformations of the columns
          cCol := op    // column name "ARRAYNO" or "ORDKEYNO"
          Return Nil
          }

   oTsb:b_Init_Def := {|ob,op| // TSB settings
          Local cID := op:cFreeze, nMemoHV := op:nMemoHV   // carried out
          Local nMarginLR   := op:nCellMarginLR            // parameters
          Local lNoKeyChar  := op:lNoKeyChar               // to oTsb
          Default nMarginLR := 1, lNoKeyChar := .F., nMemoHV := 1
          ? "### Additional settings connected oTsb:b_Init_Def"
          IF IsNumeric(op:nBrw) ; ob:Cargo:nBrw := op:nBrw // number tsb
          ENDIF
          IF !Empty(cID)
             ob:nFreeze     := ob:nColumn(cID) // Freeze columns
             ob:lLockFreeze := .T.             // Avoid cursor rendering on frozen columns
          ENDIF
          ob:lNoKeyChar     := lNoKeyChar // input of letters and numbers into cells
          ob:nMemoHV        := nMemoHV    // displaying one line of the database memo field ONLY for Dbf
          ob:nCellMarginLR  := nMarginLR  // indent from the cell line when pressing left or right by the number of spaces
          IF IsBlock(op:bInit_2)
             ? "### Additional settings connected :bInit_2"
             EVal(op:bInit_2, ob, op)
          ENDIF
          Return Nil
          }

   oTsb:b_Body_Def := {|ob,op| // other TSB settings
          ? "### Additional settings connected oTsb:b_Init_Def",ob,op
          Return Nil
          }

   oTsb:bAfter := {|ob,op|
                    Local aa, oc, xv, cv, cn
                    IF !Empty(op:l_Total_Sum) // задаем oTsb:l_Total_Sum := .T.
                       op:b_Total_Sum := {|obr| _TBrowse_Total_Sum(obr) }
                       EVal(op:b_Total_Sum, ob, .F.)   // sum total init
                       aa := ob:aArray[1]
                       FOR EACH oc IN ob:aColumns
                           cn := ","+upper(oc:cName)+","
                           IF cn $ ",SELECTOR,ORDKEYNO,ARRAYNO," ; LOOP
                           ENDIF
                           FOR EACH xv IN aa
                               IF IsBlock(oc:bDecode) ; LOOP
                               ELSEIF !IsChar(xv)     ; LOOP
                               ENDIF
                               cv := alltrim(xv)
                               IF cv == hb_ntos(Val(cv))  // "N"
                                  oc:Cargo:lTotal := .T.
                                  oc:cPicture := NIL
                                  oc:bDecode  := {|cv,ct,nv|
                                      IF IsChar(cv)
                                         ct := alltrim(cv)
                                         nv := Val(cv)
                                         cv := iif( hb_ntos(nv) == ct, nv, cv )
                                      ENDIF
                                      Return cv
                                      }
                               ENDIF
                           NEXT
                       NEXT
                    ENDIF
                    IF IsBlock(op:b_Total_Sum) .and. !Empty(op:l_Total_Sum)
                       EVal(op:b_Total_Sum, ob)   // sum total
                    ELSEIF IsBlock(op:b_Itog_Arr) // в др. тсб можно так делать
                       EVal(op:b_Itog_Arr, ob)
                    ENDIF
                    IF IsBlock(op:b_After_Def)
                       EVal(op:b_After_Def, ob)
                    ENDIF
                    Return Nil
                    }

   oTsb:b_Itog_Arr := {|ob| // ф-я расчета отогов по массиву и отображение их
                    Local aSum, aNum, nCol, oCol
                    Local nPos, xVal, aLine
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

   oTsb:b_After_Def := {|ob| //,op|
          Local oc
          IF ob:lSelector
             ob:lClrSelectorHdBack := .F.
             // If you remove the line ob:lClrSelectorHdBack, then the line below
             //ob:nClrSelectorHdBack := GetSysColor( COLOR_BTNFACE ) // system color
             oc := ob:aColumns[1]
             oc:nClrBack := {|clr,del,obr|
                              clr := obr:Cargo:oParam:aNumber_nBClr
                              If obr:lIsDbf    // ONLY for Dbf
                                 If ( del := (obr:cAlias)->( Deleted() ) )
                                    clr := CLR_GRAY
                                 Endif
                              Endif
                              Return clr
                              }
          ENDIF
          Return Nil
          }

RETURN oTsb

/////////////////////////////////////////////////////////////////////////
STATIC FUNCTION ONKEYF1()
   Local ow  := ThisWindow.Object
   Local owc := ow:Cargo, cs1 := " "
   Local ob  := owc:oBrw, cs2 := ","
   Local nc  := ob:nCell, oc := ob:aColumns[nc]
   Local np1 := iif( ob:nColumn("SELECTOR", .T.) > 0, 1, 0 )
   Local np2 := iif( ob:nColumn("ARRAYNO" , .T.) > 0, 1, 0 )
   Local nk  := nc - ( np1 + np2 ), xv, nv, le, c, i, k
   IF !AlertYesNo('Transform column '+hb_ntos(nk)+': "C" => "N" ?')
      Return Nil
   ENDIF
   oc:cPicture := NIL
   oc:bDecode  := NIL
   ? "@Transform column "+hb_ntos(nk), nk, nc, np1, np2
   k := 0
   FOR i := 1 TO Len(ob:aArray)
       xv := ob:aArray[ i ][nk]
       IF valtype(xv) != "C" ; LOOP       // не символы
       ENDIF
       xv := alltrim(xv)
       IF Empty(xv)
          nv := Val("0")
       ELSE
          IF cs1 $ xv ; xv := StrTran(xv, cs1,  "")
          ENDIF
          IF cs2 $ xv ; xv := StrTran(xv, cs2, ".")
          ENDIF
          le := .F.
          FOR EACH c IN xv
              le := c $ "0123456789."     // цифры ?
              IF !le ; EXIT               // нет
              ENDIF
          NEXT
          IF !le ; LOOP
          ENDIF
          nv := Val(xv)
       ENDIF
       ob:aArray[ i ][nk] := nv
       k++
   NEXT
   IF k > 0
      oc:nAlign  := DT_RIGHT
      oc:nFAlign := DT_RIGHT
      EVal(ob:Cargo:oParam:b_Total_Sum, ob)   // sum total
      ob:Refresh()
   ENDIF
Return Nil

/////////////////////////////////////////////////////////////////////////
FUNCTION Csv_Reestr(cFile,l866,lUtf,l124,lErr)
   LOCAL cTxt, aTxt1, aTxt2, aTxt3, cStr, aSprt, cSprt, cBuf, nCnt
   LOCAL aVal, aDim2, nI, nJ, c1, c3, aHead, lArr, aDim, cBOM, lStop

   cBuf := HB_MemoRead(cFile)
   cBOM := hb_utf8Chr( 0xFEFF )  // проверка 3х байт
   //IF ( lUtf8 := left(cBuf, Len(cBOM)) == cBOM )
   //   cBuf := subs(cBuf, Len(cBOM)+1)
   //ENDIF
   l866 := l124 := lErr := .F.
   lUtf := hb_StrIsUtf8(cBuf)

   IF ( lUtf := hb_StrIsUtf8(cBuf) ) ; cTxt := hb_Utf8ToStr(cBuf)
   ELSE                              ; cTxt := cBuf
   ENDIF

   IF ( l124 := "|" $ cTxt )
      nCnt := hb_TokenCount( cTxt, "|", .T., .T. )
      cTxt := CharRepl( "|", cTxt, ";" )
   ENDIF

   l866 := IsOemText(cTxt)    // вернуть наверх параметр
   IF l866
      cTxt := HB_OEMTOANSI(cTxt)
   ENDIF

   aSprt := { ";", "|" }
   cSprt := aSprt[1]
   aTxt1 := {}
   aTxt3 := {}
   aTxt2 := {}
   aHead := {}
   lArr  := .F.
   aDim  := HB_ATokens(cTxt,CRLF,.F.,.F.)

   IF AT("{",cTxt) > 0
      // это массив - строки {....}
      lArr := .T.
      FOR nI := 1 TO LEN(aDim)
          cStr := ALLTRIM(aDim[nI])
          IF LEN(cStr) == 0               // пропуск строки
             LOOP
          ELSEIF "HEAD" $ cStr            // шапка таблицы
             cStr := ALLTRIM( CharRem( "HEAD", cStr ) )
             IF AT( "{", cStr ) > 0 .AND. AT( "}", cStr ) > 0
                cStr  := SUBSTR(cStr, AT("{",cStr) )
                aVal  := &cStr
                FOR nJ := 1 TO LEN(aVal)
                  AADD( aHead, aVal[nJ] )
                NEXT
             ENDIF
          ELSE
             // сама таблица
             IF AT( "{", cStr ) > 0 .AND. AT( "}", cStr ) > 0
                cStr := SUBSTR(cStr,AT("{",cStr) )
                cStr := SUBSTR(cStr,1,RAT("}",cStr)+1)
                //aVal := &cStr
                aVal := myMacro(cStr)
                IF IsArray(aVal)
                   AADD( aTxt2 , aVal )
                ELSE
                   ? ProcNL(), aVal
                ENDIF
             ENDIF
          ENDIF
          DO EVENTS
      NEXT
      //? ProcNL(), "Table: aTxt2=",aTxt2 ; ?v aTxt2
   ELSE
      FOR nI := 1 TO LEN(aDim)
          cStr := aDim[nI]
          // Доп.обработка массива
          IF SUBSTR(cStr,1,1) == "#"      // заголовок реестра
             AADD(aTxt1,cStr)
          ELSEIF SUBSTR(cStr,1,1) == "="  // подвал реестра
             AADD(aTxt3,cStr)
          ELSEIF LEN(ALLTRIM(cStr)) == 0
             // пропуск строки
          ELSE
             // сама таблица
             aDim2 := HB_ATokens(cStr,cSprt,.F.,.F.)
             aVal  := {}
             FOR nJ := 1 TO LEN(aDim2)
                AADD( aVal, aDim2[nJ] )
             NEXT
             AADD( aTxt2 , aVal )
          ENDIF
          DO EVENTS
      NEXT
   ENDIF

   IF Len(aTxt2) == 0
      IF lArr  ; cStr := "{ ... , ... }"
      ELSE     ; cStr := HB_ValToExp(aSprt)
      ENDIF
      aTxt2 := { {"No data for table ! No repeating separator characters found: " + cStr } }
      lErr := .T.
   ENDIF
   c1 := ""
   IF Len(aTxt1) > 0
      FOR nI := 1 TO LEN(aTxt1)
         c1 += aTxt1[nI] + CRLF
      NEXT
   ENDIF
   c3 := ""
   IF Len(aTxt3) > 0
      FOR nI := 1 TO LEN(aTxt3)
         c3 += aTxt3[nI] + CRLF
      NEXT
   ENDIF

   lStop := CheckingArrayStrings(aTxt2,cFile)

RETURN {c1,aTxt2,c3,aHead,lStop}

///////////////////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION CheckingArrayStrings(aDim, cFile)
   LOCAL nI, nCol, aErr, cMsg, lStop, a

   lStop := .F.
   nCol  := LEN(aDim[1])
   aErr  := {}
   FOR nI := 1 TO LEN(aDim)
      a := aDim[nI]
      IF LEN(a) # nCol
         cMsg := IIF( App.Cargo:lRu, "Строка: " , "Line: " )
         cMsg += HB_NtoS(nI)
         cMsg += " ! LEN()=" + HB_NtoS(LEN(a))
         AADD( aErr, cMsg )
      ENDIF
      DO EVENTS
   NEXT
   IF LEN(aErr) > 0
      cMsg := IIF( App.Cargo:lRu, "Ошибка ! Кол-во столбцов разные в массиве !;" ,;
                   "Error! The number of columns in the array is different!;" )
      cMsg += IIF( App.Cargo:lRu, "Исправить файл ! Отказ показа !;" ,;
                   "Fix file ! Display refused !;" )
      cMsg += cFile + ";;"

      cMsg += IIF( App.Cargo:lRu, "Кол-во столбцов в первой строке = " ,;
                   "Number of columns in the first row = " )
      cMsg += HB_NtoS(nCol) + ";"
      FOR nI := 1 TO LEN(aErr)
         cMsg += aErr[nI] + ";"
      NEXT
      AlertStop(cMsg,App.Cargo:cTitle,,64,{ORANGE})
      lStop := .T.
      ? ATREPL( ";", cMsg, CRLF )
   ENDIF

RETURN lStop

///////////////////////////////////////////////////////////////////////////////////////////////////
FUNCTION myMacro(cRun,lDebug)
   LOCAL lRsc, oError, cRet
   DEFAULT lDebug := .F.

   IF !lDebug
      BEGIN SEQUENCE  WITH { |e|break( e ) }
         cRet := &(cRun)
         lRsc := .T.
      RECOVER USING oError
         lRsc := .F.
      END SEQUENCE
      // результат обработки макро
      IF !lRsc
         cRet := "ERROR! macro= [" + cRun + "]"
      ENDIF
   ELSE
      // отладка
      cRet := &(cRun)
   ENDIF

RETURN cRet

///////////////////////////////////////////////////////////////////////////////////////////////////
FUNCTION _TBrowse_Total_Sum(ob, lSum)
   Local lTot, cTot, cNoT, lNoT, aSum, nRec, oCol, nI, nK := 0
   Local cNam, xVal, nAtPos, nLastPos, lInit, op
   Default lSum := .T.

   AEval(ob:aColumns, {|oc| iif( IsObject(oc:Cargo),, oc:Cargo := oHmgData() ), ;
                      nK += iif( IsLogical(oc:Cargo:lTotal), 1, 0 ) })

   IF ( lInit := nK == 0 )                               // init columns
      op := ob:Cargo:oParam
      cToT := op:cTotal
      IF lToT := ( !Empty(cToT) .and. IsChar(cToT) .and. !Empty(cToT) )
         cToT := "," + upper(cToT) + ","
      ENDIF
      cNoT := op:cNoTotal
      IF lNoT := ( !Empty(cNoT) .and. IsChar(cNoT) .and. !Empty(cNoT) )
         cNoT := "," + upper(cNoT) + ","
      ENDIF
      FOR nK := 1 TO Len(ob:aColumns)
          oCol := ob:aColumns[nK]
          oCol:Cargo:lTotal := .F.
          IF !oCol:lVisible ; LOOP                        // нет суммы итога
          ENDIF
          cNam := ","+upper(oCol:cName)+","
          IF cNam $ ",SELECTOR,ORDKEYNO,ARRAYNO,"
             LOOP                                         // нет суммы итога
          ENDIF
          IF lNoT .and.  cNam $ cNoT ; LOOP //    входит в список, нет итога
          ENDIF
          IF lToT .and. !cNam $ cToT ; LOOP // не входит в список, нет итога
          ENDIF
          xVal := ob:bDataEval(oCol, , nK)
          IF IsNumeric(xVal) ; oCol:Cargo:lTotal := .T.
          ENDIF
      NEXT
   ENDIF

   aSum := array(Len(ob:aColumns)) ; aFill(aSum, 0)

   IF lSum                         // Total
      nAtPos   := ob:nAt
      nLastPos := ob:nLastPos

      IF ob:lIsDbf
         nRec := (ob:cAlias)->( RecNo() )
         (ob:cAlias)->( dbGoTop() )
      ENDIF

      FOR nI := 1 TO ob:nLen
          ob:nAt := nI
          FOR nK := 1 TO Len(ob:aColumns)
              oCol := ob:aColumns[nK]
              IF oCol:Cargo:lTotal
                 xVal := ob:bDataEval(oCol, , nK)
                 IF IsNumeric(xVal) .and. !Empty(xVal) ; aSum[nK] += xVal
                 ENDIF
              ENDIF
          NEXT
          ob:Skip() ; DO EVENTS
      NEXT

      FOR nK := 1 TO Len(ob:aColumns)
          oCol := ob:aColumns[nK]
          IF !oCol:Cargo:lTotal   ; LOOP
          ENDIF
          oCol:cFooting := {|nc,ob|
                Local oc := ob:aColumns[nc], ct := ""
                IF oc:Cargo:lTotal
                   IF !Empty(oc:Cargo:nTotal)
                      ct := cValToChar(oc:Cargo:nTotal) // вывод суммы итога
                   ENDIF
                ENDIF
                IF oCol:Cargo:cFooting != NIL
                   ct := cValToChar(oc:Cargo:cFooting)  // вывод др.данных
                ENDIF
                Return ct
                }
          IF !IsNumeric(aSum[nK]) ; LOOP
          ENDIF
          oCol:Cargo:nTotal := aSum[nK]
      NEXT

      IF ob:lIsDbf ; (ob:cAlias)->( dbGoTo(nRec) ) ; DO EVENTS
      ENDIF

      ob:nAt      := nAtPos
      ob:nLastPos := nLastPos

      ob:DrawFooters() ; DO EVENTS
   ENDIF

RETURN aSum
