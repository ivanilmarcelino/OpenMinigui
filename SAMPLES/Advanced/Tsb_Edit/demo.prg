/*                                                         
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2024 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Фонты в таблице и ширина колонок
 * Использование типов полей в базе '^+=@T' и других
 * Свои окна для обработки типов полей в базе "DTM"
 * Table fonts and column widths
 * Using field types in the database '^+=@T' and others
 * Own windows for processing field types in the "DTM" database
*/
                                     
REQUEST DBFCDX

#define _HMG_OUTLOG
#include "minigui.ch"
#include "TSBrowse.ch"

//////////////////////////////////////////////////////////////////////
PROCEDURE MAIN
   LOCAL oBrw, nY, nX, nW, nH, nG, cAls, aBClr, aBrush, nFSz := 14
   LOCAL aTsbFont, owc

   rddSetDefault( "DBFCDX" )

   SET DELETED   ON
   SET EXCLUSIVE ON
   SET AUTOPEN   ON
   SET DATE      TO GERMAN
   SET EPOCH     TO 2000
   SET CENTURY   ON
   SET EXACT     ON
   SET DECIMALS  TO 4

   SET OOP       ON

   SET NAVIGATION EXTENDED
   SET DEFAULT ICON TO "1MAIN_ICO"
   SET SHOWREDALERT ON        // увеличить фонт для окна "Program Error"

   SET FONT TO 'Arial', nFSz

   //_DefineFont("TsbNorm" , "DejaVu Sans Mono" , nFSz  , .F., .F. )
   _DefineFont("TsbNorm" , "Tahoma"           , nFSz  , .F., .F. )
   _DefineFont("TsbBold" , "Times New Roman"  , nFSz  , .T., .F. )
   _DefineFont("Italic"  , "Tahoma"           , nFSz-2, .F., .T. )
   _DefineFont("ItalBold", "Arial Black"      , nFSz  , .T., .F. )
   _DefineFont("SpecHdr" , "Tahoma"           , nFSz-4, .T., .T. )
   _DefineFont("TsbEdit" , "Arial"            , nFSz  , .F., .T. )
   _DefineFont("DlgFont" , "DejaVu Sans Mono" , nFSz  , .F., .F. )    // фонт окна Alert*

   nY     := nX := nG := 20
   aBClr  := AQUA
   aBrush := {179,230,251}
   cAls   := myOpenDbf("test.dbf")
   //            cell         head       foot     SpecHider   SuperHider   Edit
   aTsbFont := { "TsbNorm", "TsbBold", "TsbBold", "SpecHdr" , "ItalBold", "TsbEdit" } 

   DEFINE WINDOW wMain                 ;
      TITLE "TBROWSE Edit Fields Demo" ;
      BACKCOLOR aBClr                  ;
      MAIN                             ;
      NOMAXIMIZE NOSIZE                ;
      ON INIT    {|| _LogFile(.T., ProcNL(),">>> Start!") , oBrw:Setfocus() } ;
      ON RELEASE {|| _LogFile(.T., ProcNL(),">>> Exit ! Number of changes: ", oBrw:Cargo:nModify) }

      This.Cargo := oHmgData() ; owc := This.Cargo  // для окна создаем объект без переменных (условно пустой)
      owc:aBColor    := This.BackColor   // цвет окна
      owc:oWnd       := This.Object      // запомнить ВЕСЬ объект этого окна

      nW := This.ClientWidth
      nH := This.ClientHeight

      DEFINE TBROWSE oBrw                          ;
             AT nY, nX ALIAS cAls                  ;
             WIDTH nW-nG*2 HEIGHT nH-nG*2 CELL     ;
             FONT       aTsbFont                   ;
             BRUSH      aBrush                     ;
             COLNUMBER  { 1, 40 }                  ; // слева таблицы виртуальная колонка с нумерацией
             FIXED ADJUST COLEMPTY                 ;
             LOADFIELDS                            ; // автоматическое создание столбцов по полям активной базы данных
             ENUMERATOR                            ; // нумерация колонок
             SELECTOR .T.                          ; // первая колонка - селектор записей
             EDIT GOTFOCUSSELECT                   ;
             ON INIT  {|ob| ob:Cargo := oHmgData() } // создаем объект без переменных (условно пустой)

             myTsbSet( oBrw , aTsbFont )
             myTsbColor( oBrw )
             myTsbSuperHd( oBrw )   
             myTsbFont( oBrw )
             myTsbKeyFX( oBrw )
             myTsbEdit( oBrw )
    
      END TBROWSE ON END {|ob| ob:SetNoHoles(), ob:SetFocus() }
      // последние действия с ТСБ
      myTsbEnd(oBrw)

      oBrw:Cargo:nModify := 0                    // изменения в таблице
      oBrw:Cargo:aFont   := aTsbFont             // запомним фонты
      //oBrw:Cargo:ObjWnd:= owc:oWnd             // всё окно на объект Cargo ТСБ    
      oBrw:Cargo:ObjWnd  := This.Object          // или так

      ON KEY F1     ACTION NIL
      ON KEY ESCAPE ACTION iif( oBrw:IsEdit, oBrw:SetFocus(), wMain.Release() )

   END WINDOW

   wMain.Activate

RETURN

///////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbSet( oBrw , aTsbFont)
   LOCAL nHCell := oBrw:nHeightCell + 6  // высота ячеек
   LOCAL oCol, cCol, oDlu, nDlu, n
   LOCAL hFont, aFont, cFont, nFSize, cHead, nWCol, cFrmt

   hFont  := GetFontHandle( aTsbFont[1] )
   aFont  := GetFontParam(hFont)
   cFont  := aFont[1]
   nFSize := aFont[2]
   // подгоним размеры колонок по фонту
   oDlu := _Font2oDlu( aTsbFont[1] )
   nDlu := oDlu:nSize
   // --- варианты задания размера ---
   //?  _HMG_DefaultFontName, _HMG_DefaultFontSize, "nDlu=", nDlu, oTsb:aTsbFont[1]
   //?  "oDlu:H1=",oDlu:H1, oDlu:H1 + 6, oDlu:H(1.25), oDlu:H1 + oDlu:H(0.25)
   IF cFont == "DejaVu Sans Mono"
      // !!! для фонта MONO - DejaVu Sans Mono делаем добавку
      n := iif( nFSize <= 15, 20, iif( nFSize < 20, 40, 50 ) )
      oDlu:nPixWidth    += n
      oDlu:nPixWidthDT  += n
      oDlu:nPixWidthDT1 += n
      oDlu:nPixWidthDT2 += n
   ENDIF
   //
   nHCell := oDlu:H1 + 6              // высота строк в ТСБ
   //                ^^^ - константа
   nHCell := oDlu:H(1.25)             // так правильнее, от размера фонта высота
   //              ^^^^  - пропорция от размера фонта

   WITH OBJECT oBrw
      :nColOrder     := 0
      :lNoChangeOrd  := .F.
      :nWheelLines   := 1
      :lNoGrayBar    := .F.
      :lNoLiteBar    := .F.
      :lNoResetPos   := .F.
      :lNoHScroll    := .T.
      :lNoPopUp      := .T.
      :lNoKeyChar    := .F.          // НЕТ ввода в ячейки от букв, цифр
      :nHeightCell   := nHCell       // высота ячеек = высоте картинки
      :nHeightHead   := nHCell * 1.2 // высота шапки
      :nHeightFoot   := nHCell + 4   // высота подвала
      :nHeightSpecHd := 12           // высота спецхидера ENUMERATOR
      :lFooting      := .T.          // использовать подвал
      :lDrawFooters  := .T.          // рисовать  подвалы
      :nFreeze       := 1            // Заморозить столбец
      :lLockFreeze   := .T.          // Избегать прорисовки курсора на замороженных столбцах
      :nCellMarginLR := 1            // отступ от линии ячейки при прижатии влево, вправо на кол-во пробелов
      :nMemoHV       := 1            // показ 1 строки мемо-поля
      :GetColumn("ORDKEYNO"):nWidth := nHCell + 10   // ширина колонки 1
   END WITH

   FOR EACH oCol IN oBrw:aColumns
      cCol := oCol:cName
      IF cCol == "SELECTOR" .OR. cCol == "ORDKEYNO"
      ELSE
         oCol:cFooting := '"' + oCol:cFieldTyp + '"'
         oCol:nFAlign  := DT_CENTER
      ENDIF
      // !!! для фонта - DejaVu Sans Mono делаем добавку 
      IF oCol:cFieldTyp == "C"
         //oCol:cPicture := Nil
         oCol:nWidth := oCol:ToWidth( iif( oCol:nFieldLen > 50, 50, oCol:nFieldLen ) )
      ELSEIF oCol:cFieldTyp $ "D"
         oCol:nWidth   := oCol:ToWidth(10+2)   // 01.01.2024
         oCol:nAlign   := DT_CENTER
      ELSEIF oCol:cFieldTyp $ "N"
         hFont := oCol:hFont                  // какой фонт в ячейке
         cFrmt := REPL("0",oCol:nFieldLen) + "9" + REPL("0",oCol:nFieldDec)
         cFrmt += "99"
         nWCol := GetTextWidth( Nil, cFrmt, hFont )
         oCol:nWidth := nWCol
      ELSEIF oCol:cFieldTyp $ "T=@"
         //oCol:cPicture := "@R 9999-99-99 99:99:99" // 23 символа
         //oCol:bDecode  := {|tval| iif( tval == hb_CToT(""), "", hb_TtoS(tval) ) }   
         //oCol:bDecode  := {|tval| hb_TtoS(tval) }
         //oCol:nAlign   := DT_LEFT
         // лучше так
         oCol:cPicture := NIL
         oCol:nAlign   := DT_CENTER
         oCol:nWidth   := oCol:ToWidth(26) 
         //^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ - это не работает, если задан oCol:cPicture, он в приоритете
      ELSEIF oCol:cFieldTyp $ "^"
         oCol:bDecode  := {|tval| hb_NtoS(tval) }
         oCol:nAlign   := DT_CENTER
      ELSEIF oCol:cFieldTyp $ "L"
         oCol:nAlign   := DT_CENTER
      ELSEIF oCol:cFieldTyp $ "D"
         oCol:cPicture := Nil
         oCol:nAlign   := DT_CENTER
         oCol:nWidth   := oCol:ToWidth(10)
      ELSEIF oCol:cFieldTyp $ "MV"
         oCol:cPicture := Nil
         oCol:nWidth   := oCol:ToWidth(40)
      ENDIF 
      // выравниваем ширину колонки
      hFont := oCol:hFontHead                  // какой фонт в колонке шапки
      cHead := oCol:cHeading + "H"
      nWCol := GetTextWidth( Nil, cHead, hFont )
      IF oCol:nWidth < nWCol
         oCol:nWidth := nWCol
      ENDIF
   NEXT

RETURN Nil

///////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbColor( oBrw )

   WITH OBJECT oBrw
      :nClrLine              := RGB(180,180,180) // COLOR_GRID
      :SetColor( { 11 }, { { || RGB(0,0,0) } } )
      :SetColor( {  2 }, { { || RGB(255,255,240) } } )
      :SetColor( {  5 }, { { || RGB(0,0,0) } } )
      :SetColor( {  6 }, { { |a,b,c| a:=b, iif( c:nCell == b,  -CLR_HRED        , -RGB(128,225,225) ) } } )
      :SetColor( { 12 }, { { |a,b,c| a:=b, iif( c:nCell == b,  -RGB(128,225,225), -RGB(128,225,225) ) } } )
   END WITH

RETURN Nil

//////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbSuperHd( oBrw )
   LOCAL hFont, nHFont, aFont, cTitle

   aFont  := GetFontParam(oBrw:hFont) 
   cTitle := "Cell font: " + aFont[1] + " " + HB_NtoS(aFont[2])
   cTitle += SPACE(10)
   cTitle += "Editing different types of fields in TBROWSE  (F2-test)"
   hFont  := oBrw:hFontSupHdGet(1)
   nHFont := GetTextHeight( 0, "B", hFont )

   WITH OBJECT oBrw
      // Создаём СУПЕРХИДЕР в таблице размером 0
      :AddSuperHead( 1, :nColCount(), "Super_Header_Table" ) 
      :aSuperhead[ 1, 3 ] := cTitle
      :nHeightSuper := nHFont * 2      // 2 строки
      // задать цвета суперхидеру
      :SetColor( {16}, { { RGB(40,110,212),RGB(0,176,240) }  } ) // 16, фона 
      :SetColor( {17}, { CLR_WHITE                           } ) // 17, текста 
   END WIDTH

RETURN NIL

///////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbFont( oBrw )
   LOCAL hFont, oCol

   hFont := oBrw:aColumns[1]:hFontSpcHd    // 4-special header font
   // установить фонт для 1 колонки таблицы
   oCol := oBrw:GetColumn("ORDKEYNO")
   oCol:nAlign    := DT_CENTER
   oCol:hFont     := hFont     // 1-cells font
   oCol:hFontFoot := hFont     // 3-footer font

RETURN Nil

//////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbKeyFX( oBrw )    // обработка клавиш

   WITH OBJECT oBrw

      // обработка мышки
      :bLDblClick := {|p1,p2,p3,ob| p1:=p2:=p3, ob:PostMsg( WM_KEYDOWN, VK_RETURN, 0 ) }
      :SetAppendMode( .F. )            // запрещена вставка записи в конце базы стрелкой вниз
      :SetDeleteMode( .F. )

      // обработка клавиши ESC и других
      //:UserKeys(VK_ESCAPE, {|ob| _wSend(99, ob:cParentWnd), .F. })
      //:UserKeys(VK_INSERT, {|ob| RecnoInsert(ob), .F. })
      //:UserKeys(VK_DELETE, {|ob| RecnoDelete(ob), .F. })

      // клавиши FXX
      :UserKeys(VK_F2    , {|ob| myTsb_Test( ob ), ob:Setfocus() }) 

   END WITH

RETURN Nil

////////////////////////////////////////////////////////////////////////////
// настройки редактирования, редактирование колонок
STATIC FUNCTION myTsbEdit( oBrw )
   LOCAL oCol, cCol, nI

   ? ProcNL()
   FOR EACH oCol IN oBrw:aColumns
      nI   := hb_EnumIndex(oCol)
      cCol := oCol:cName
      ? "    .",nI, cCol
      IF cCol == "SELECTOR" .OR. cCol == "ORDKEYNO"  ; LOOP
      ENDIF
      oCol:bPrevEdit := {|val, brw| myTsbEditPrev( val, brw ) }  // -> см.ниже
      oCol:bPostEdit := {|val, brw| myTsbEditPost( val, brw ) }  // -> см.ниже
      oCol:lEdit := .T.
      IF oCol:cFieldTyp $ "+^="  // эти поля не редактируются
         oCol:lEdit := .F.
         oCol:nClrFootFore := CLR_WHITE
         oCol:nClrFootBack := CLR_RED
      ENDIF
      ?? oCol:lEdit, oCol:cFieldTyp, oCol:nFieldLen, oCol:nFieldDec, oCol:cHeading
   NEXT

RETURN NIL

////////////////////////////////////////////////////////////////////////////
// блок-кода который ДЕЛАЕМ ПОСЛЕ END TBROWSE
STATIC FUNCTION myTsbEnd( oBrw )
   LOCAL nBCSpH, oCol, nCol, nLen, hFont, nWCol
                                   
   nBCSpH := GetSysColor( COLOR_BTNFACE )   // цвет фона спецхидера таблицы

   // замена строки нумератора колонок на свой цвет, кроме SELECTOR
   oBrw:lClrSelectorHdBack := .T. // background OFF
   FOR EACH oCol IN oBrw:aColumns
      oCol:nClrSpcHdFore := CLR_RED
      oCol:nClrSpcHdBack := nBCSpH
   NEXT

   // подправим виртуальную колонку
   nLen := LEN(HB_NtoS(oBrw:nLen))
   nCol := oBrw:nColumn("ORDKEYNO", .T.)
   IF nCol > 0
      oCol := oBrw:GetColumn("ORDKEYNO")
      oCol:nClrBack      := nBCSpH
      oCol:nClrFore      := CLR_RED
      //oCol:nClrFootBack  := nBCSpH
      oCol:nClrFootFore  := CLR_RED
      oCol:SaveColor()                                           // сохранить цвета колонки
      hFont := oBrw:aColumns[nCol]:hFont                         // какой фонт в колонке
      nWCol := GetTextWidth( Nil, REPL("0", nLen + 2), hFont )   // кол-во знаков + 2 знака
      oCol:nWidth := nWCol                                       // новая ширина
   ENDIF

   // изменение ширины поля типа "+"
   nLen := LEN(HB_NtoS(oBrw:nLen))
   FOR EACH oCol IN oBrw:aColumns
      nCol := hb_EnumIndex(oCol)
      IF oCol:cFieldTyp == "+"
         hFont := oBrw:aColumns[nCol]:hFont                         // какой фонт в колонке
         nWCol := GetTextWidth( Nil, REPL("0", nLen + 4), hFont )   // кол-во знаков + 2 знака + 2 знак отступ
         oCol:nWidth := nWCol                                       // новая ширина
      ENDIF
   NEXT

   IF oBrw:lDrawSuperHd  // Шаг-2
      // увеличить суперхидер до конца колонок
      ATail(oBrw:aSuperHead)[2] := oBrw:nColCount()
   ENDIF

   oBrw:DrawHeaders()   // перечитать суперхидер/шапку/нумератор
   oBrw:Refresh()
   oBrw:SetFocus()
   DO EVENTS

RETURN NIL

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbEditPrev( uVal, oBrw )
   LOCAL nCol, oCol, cNam, cAls, uOld, lRet
   LOCAL cTyp, cMsg, xRet, lWrt, cStr, aRet

   WITH OBJECT oBrw
      nCol  := :nCell
      oCol  := :aColumns[ nCol ]
      cAls  := :cAlias
      cTyp  := oCol:cFieldTyp        // тип обработки колонки
      cNam  := oCol:cName
   END WITH

   uOld := uVal
   ? ProcNL(), nCol, cTyp 
   cStr := 'oCol:bPrevEdit !;'
   cStr += 'oCol:cName = ' + cNam + ";"
   cStr += 'Column field type: "' + cTyp + '" ;'
   cStr += 'NO processing for this field!;'
   lWrt := .T.     // записывать поле
   aRet := {uVal}       // поместим в массив - то что пришло

   IF cTyp $ "NLI^"
      oCol:nClrEditFore := CLR_YELLOW
      oCol:nClrEditBack := CLR_BLACK
   ELSEIF cTyp $ "CMV"
      oCol:nClrEditFore := CLR_BLUE
      oCol:nClrEditBack := CLR_HGRAY
      // пример для своей функции
      IF AT(CRLF,uVal) > 0           // если в поле "C" есть CRLF
         aRet := CellEditMemo(uVal, oBrw)
         lRet := .F.                 // не давать редактировать поле в :get
      ELSEIF cTyp == "M" .OR. cTyp == "V"
         aRet := CellEditMemo(uVal, oBrw) 
         lRet := .F.                 // не давать редактировать поле в :get
      ENDIF
   ELSEIF cTyp $ "=@T" .OR. cTyp $ "D"   
      aRet := CellEdit_DT(oBrw, cTyp, uVal) 
      lRet := .F.             // не давать редактировать поле в :get
   ELSE
      oCol:nClrEditFore := CLR_YELLOW
      oCol:nClrEditBack := CLR_RED
      ? ProcNL(), "uVal=", uVal, HB_ValToExp(uVal)
      //cMsg := ProcNL(0) + ";" + ProcNL(1) + ";;"
      //AlertInfo(cMsg + cStr)
      //lWrt := .F.             // не записывать в ячейку
      //lRet := .F.             // не давать редактировать поле в :get
   ENDIF

   //? ProcNL(), "#######-0", "lWrt=", lWrt, aRet, HB_ValToExp(aRet)
   IF lWrt                                         // записывать ячейку
      IF (oBrw:cAlias)->(RLock())                  // делать самому
         // !!! всегда массив - если пустой, то это ОТКАЗ от ввода
         IF LEN(aRet) > 0                          
            ? ProcNL(), "#######-?", aRet, HB_ValToExp(aRet)
            oBrw:Cargo:nModify ++                  // счётчик-изменения в таблице
            xRet := aRet[1]                              
            oBrw:SetValue(nCol,xRet)
            //(oBrw:cAlias)->KOPERAT  := 555       // кто правил запись
            //(oBrw:cAlias)->DATEVVOD := DATE()    // дата правки
            //(oBrw:cAlias)->TIMEVVOD := 9999      // время правки
            (oBrw:cAlias)->( DbUnlock() )
            (oBrw:cAlias)->( DbCommit() )
         ENDIF
      ELSE
         cMsg := "Recording is locked !; Recno="
         cMsg += HB_NtoS(RECNO()) + ";;" + ProcNL()
         AlertStop( cMsg )
      ENDIF
   ENDIF
   oBrw:DrawSelect()    // перерисовать текущую ячейку таблицы
   oBrw:SetFocus()

   DO EVENTS

RETURN lRet

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbEditPost( uVal, oBrw )
   LOCAL nCol, oCol, cNam, uOld, cAls, lMod
   LOCAL oWnd := _WindowObj(oBrw:cParentWnd)
   LOCAL cTyp, cMsg, cStr

   WITH OBJECT oBrw
      nCol := :nCell
      oCol := :aColumns[ nCol ]
      cNam := oCol:cName
      cTyp := oCol:cFieldTyp        // тип обработки колонки
      uOld := oCol:xOldEditValue    // old value
      lMod := ! uVal == uOld        // .T. - modify value
      cAls := :cAlias
   END WITH

   ? ProcNL(), nCol, cTyp 
   cStr := 'oCol:bPrevEdit !;'
   cStr += 'oCol:cName = ' + cNam
   cStr += ';Column processing type: "' + cTyp + '" ;'

   IF cTyp $ "CNDL"
      // стандартная обработка
   ELSE
      cMsg := ProcNL(0) + ";" + ProcNL(1) + ";;"
      AlertInfo(cMsg + cStr)
      RETURN .F.
   ENDIF
   oBrw:DrawSelect()    // перерисовать текущую ячейку таблицы
   oBrw:SetFocus()

   DO EVENTS

RETURN .T.

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION CellEdit_DT(oBrw,cType,xGet)
   LOCAL oCell  := oBrw:GetCellInfo(oBrw:nRowPos) 
   LOCAL nY     := oCell:nRow + oBrw:nHeightHead
   LOCAL nX     := oCell:nCol 
   LOCAL nWCell := oCell:nWidth - 2
   LOCAL nHCell := oCell:nHeight - 2
   LOCAL oWnd, hWnd, oJWnd, aRet, cForm, nWBtn, nHObj, nHIco, aTime, cVal
   LOCAL cFont, nFSize, aFont, cText, nWDate, dDate1, tDTime, nW, nH
                        
   ? ProcNL(), "cType=", cType, "xGet=", xGet, "VALTYPE=", VALTYPE(xGet) 

   oJWnd := oBrw:Cargo:ObjWnd        // текущее окно    
   cForm := oJWnd:Name 
   nY    += oJWnd:Row 
   nX    += oJWnd:Col + 3
   IF oBrw:lDrawSpecHd  
      nY -= oBrw:nHeightSpecHd    // высота спецхидера ENUMERATOR
   ENDIF

   nY     += IIF( Sys.ClientWidth  <= 720, 4, -3)
   nHCell += IIF( Sys.ClientHeight <= 720, 3, 0 )
   
   aFont  := GetFontParam(oBrw:hFont) 
   cFont  := aFont[1]
   nFSize := aFont[2]

   nHObj  := nHCell - 7 //nFSize * 2
   nHIco  := nHObj - 2
   cText  := "120DECEMBER020240"
   nWDate := GetTxtWidth( cText, nFSize, cFont, .T. ) + 65
   IF cType $ "@T"
      cText  := REPL("0",24) + '0|0'
      nWDate := GetTxtWidth( cText, nFSize, cFont, .T. ) + 55
   ENDIF
   nWBtn  := nHCell + nHCell + 4       // две кнопки
   nW     := nWDate + nWBtn
   aRet   := {}   // всегда массив - пусто, значит отказ от ввода

   // выход за границы экрана/прижимаем к правому концу ячейки
   IF nX + nW > Sys.ClientWidth
      nX := (nWCell + nX) - nW
   ENDIF
   nH := nHCell

   // новое окно в ячейку таблицы
   DEFINE WINDOW Cell AT nY,nX WIDTH nW HEIGHT nH  ;
      MODAL NOCAPTION                              ; 
      FONT cFont SIZE nFSize                       ;
      ON LOSTFOCUS {|| oWnd:Release() }            ;
      ON INIT      {|| DoEvents() }

      oWnd := ThisWindow.Object 
      hWnd := oWnd:Handle 

      IF cType == "D"

         dDate1 := xGet
         IF dDate1 == CTOD('')
            dDate1 := DATE()
         ENDIF

         @ 3, 3 DATEPICKER Date_1 VALUE dDate1 WIDTH nWDate HEIGHT nHObj ;
            DATEFORMAT "dd'.'MMMM' 'yyyy" SHOWNONE 
         nX := This.Date_1.Width + 5

         @ 3, nX BUTTONEX Btn_Ok WIDTH nHObj HEIGHT nHObj CAPTION '' ;
           ICON "iOk32" NOHOTLIGHT NOXPSTYLE HANDCURSOR ;
           ACTION {|| aRet := { This.Date_1.Value } , oWnd:Release() }

         This.Btn_Ok.ImageWidth  := nHIco
         This.Btn_Ok.ImageHeight := nHIco
         This.Btn_Ok.Icon        := LoadIconByName( "iOk32", nHIco, nHIco )

         nX += This.Btn_Ok.Width + 5

         @ 3, nX BUTTONEX Btn_Esc WIDTH nHObj HEIGHT nHObj CAPTION '' ;
           ICON "iCancel32" NOHOTLIGHT NOXPSTYLE HANDCURSOR ;
           ACTION {||  aRet := {} , oWnd:Release() }

         This.Btn_Esc.ImageWidth  := nHIco
         This.Btn_Esc.ImageHeight := nHIco
         This.Btn_Esc.Icon        := LoadIconByName( "iCancel32", nHIco, nHIco )

      ELSEIF cType $ "@T"

         tDTime := xGet
         IF tDTime == hb_CToT("")
            tDTime := hb_DateTime() 
         ENDIF
         dDate1   := hb_TToD(tDTime)
         aTime    := {0,0,0}
         cVal     := hb_TtoS(tDTime)   // 2003 12 20 191944859 
         aTime[1] := VAL(SUBSTR(cVal,9,2))
         aTime[2] := VAL(SUBSTR(cVal,11,2))
         aTime[3] := VAL(SUBSTR(cVal,13,2))

         @ 3, 3 DATEPICKER Date_2 VALUE dDate1 WIDTH nWDate-3 HEIGHT nHObj ;  
           SHOWNONE UPDOWN DATEFORMAT "dd MMMM yyyy' | 'HH:mm:ss" 

         This.Date_2.VALUE := { Year( dDate1 ), Month( dDate1 ), Day( dDate1 ), aTime[1], aTime[2], aTime[3] }
         nX := This.Date_2.Width + 5

         @ 3, nX BUTTONEX Btn_Ok WIDTH nHObj HEIGHT nHObj CAPTION '' ;
           ICON "iOk32" NOHOTLIGHT NOXPSTYLE HANDCURSOR ;
           ACTION {|| tDTime := This.Date_2.Value  ,;
                      aRet   := { tDTime } , oWnd:Release() }

         This.Btn_Ok.ImageWidth  := nHIco
         This.Btn_Ok.ImageHeight := nHIco
         This.Btn_Ok.Icon        := LoadIconByName( "iOk32", nHIco, nHIco )

         nX += This.Btn_Ok.Width + 5

         @ 3, nX BUTTONEX Btn_Esc WIDTH nHObj HEIGHT nHObj CAPTION '' ;
           ICON "iCancel32" NOHOTLIGHT NOXPSTYLE HANDCURSOR ;
           ACTION {|| aRet := {} , oWnd:Release() }

         This.Btn_Esc.ImageWidth  := nHIco
         This.Btn_Esc.ImageHeight := nHIco
         This.Btn_Esc.Icon        := LoadIconByName( "iCancel32", nHIco, nHIco )

      ENDIF

       DRAW LINE IN WINDOW Cell AT 2, 2 TO 2, nW PENCOLOR RED PENWIDTH 4
       DRAW LINE IN WINDOW Cell AT nH-2, 2 TO nH-2, nW PENCOLOR RED PENWIDTH 4
       DRAW LINE IN WINDOW Cell AT 2, 2 TO nH, 2 PENCOLOR RED PENWIDTH 4
       DRAW LINE IN WINDOW Cell AT 2, nW-2 TO nH, nW-2 PENCOLOR RED PENWIDTH 4
 
   END WINDOW 
 
   SetWindowLong(hWnd, GWL_STYLE, WS_BORDER) 
 
   _DefineHotKey ( "CELL" , 0 , VK_ESCAPE , {|| oWnd:Release() } ) 
   _DefineHotKey ( "CELL" , 0 , VK_RETURN , {|| oWnd:Release() } ) 
   Cell.Activate 

RETURN aRet // всегда массив, если пусто - значит отказ от ввода

////////////////////////////////////////////////////////////////////////////
FUNCTION myTsb_Test(oBrw)
   MsgDebug( oBrw:cAlias, "Test something!")
RETURN NIL
