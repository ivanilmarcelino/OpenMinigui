/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Разное / Miscellaneous
*/
#define  _HMG_OUTLOG
#include "hmg.ch"
#include "i_winuser.ch"

////////////////////////////////////////////////////////////////////////////
// Затенение на форме / Darken the form
FUNCTION Darken2Open(hWinHandle)
   //LOCAL hWinHandle := ThisWindow.Handle
   LOCAL aBClr := { BLACK , RED , { 61, 61, 61 }, YELLOW }  // Цвет фона
   LOCAL aColor, nTransparencyLevel := 128                  // Уровень прозрачности

   aColor := aBClr[1]
   // Затенение на форме / Darken the form
   OverlayCreate(hWinHandle, aColor[1], aColor[2], aColor[3], nTransparencyLevel)

   //MsgDebug(hWinHandle, "Color Back =" , aColor, " Transparency level=", nTransparencyLevel)

   //OverlayClose(hWinHandle)

   DO EVENTS

RETURN NIL

////////////////////////////////////////////////////////////////////////////
// Затенение на форме / Darken the form
FUNCTION Darken2Close(hWinHandle)
   OverlayClose(hWinHandle)
   Do Events
   // перересовка объектов на форме
   SendMessage( hWinHandle, WM_PAINT, 0, 0 )
   Do Events
RETURN NIL

///////////////////////////////////////////////////////////////////////////////
FUNCTION myVal2Str(xVal)
   LOCAL cRet := ""

   IF xVal == NIL
      cRet := "Nil"
   ELSEIF IsString(xVal)
      cRet := xVal
   ELSEIF IsArray(xVal)
      cRet := HB_ValToExp(xVal)
   ELSEIF IsObject(xVal)
      cRet := _o2log(xVal, 17, "Object: ", .T. , .T.)
      IF IsString(cRet)
         cRet := ALLTRIM(cRet)
      ELSE
         cRet := cValToChar(cRet)
      ENDIF
   ELSE
      cRet := cValToChar(xVal)
   ENDIF

RETURN cRet

///////////////////////////////////////////////////////////////////////////////
FUNCTION ProcNL(nVal, cMsg)
   Default cMsg := ">>> "
   nVal := iif( Empty(nVal), 0, nVal ) + 1
   cMsg += ProcName(nVal) + "(" + hb_ntos( ProcLine(nVal) ) + ")"
   cMsg += " => " + ProcFile(nVal)
RETURN cMsg

///////////////////////////////////////////////////////////////////////////////////////////////////
FUNCTION myIsFunct(cRun,xPar,nI,cStr)
   LOCAL cFun, lRet, cMsg

   IF !IsChar(xPar) ; xPar := cValToChar(xPar)
   ENDIF

   lRet := .T.
   cFun := SUBSTR(cRun, 1, AT("(",cRun) - 1)
   cMsg := "Ошибка ! Строка: " + HB_NtoS(nI) + ";"
   cMsg += "Нет такой функции "+cFun+"("+xPar+") в ЕХЕ-файле !;"
   cMsg += 'Обратитесь к разработчику программы;;'
   cMsg += cStr + ";;"
   cMsg += ProcNL() + ";" + ProcNL(1)

   IF !hb_IsFunction( cFun )
      AlertStop(cMsg, "Запуск функции", , 64, {RED})
      lRet := .F.
      ? ATREPL( ";", cMsg, CRLF )
   ENDIF

RETURN lRet

///////////////////////////////////////////////////////////////////////////////
// получить Width текста
FUNCTION GetTxtWidth( cText, nFontSize, cFontName, lBold )
   LOCAL hFont, nWidth
   DEFAULT cText     := REPL('A', 2)        ,  ;
           cFontName := _HMG_DefaultFontName,  ;   // из MiniGUI.Init()
           nFontSize := _HMG_DefaultFontSize,  ;   // из MiniGUI.Init()
           lBold     := .F.

   IF Valtype(cText) == 'N'
      cText := repl('A', cText)
   ENDIF

   hFont  := InitFont(cFontName, nFontSize, lBold)
   nWidth := GetTextWidth(0, cText, hFont)         // ширина текста
   DeleteObject (hFont)

RETURN nWidth

////////////////////////////////////////////////////////////////////////////////
FUNCTION myBigSizeLabel(cForm,cObj)
   LOCAL cText, cFont, lBold, nFSize, nWObj, nHobj

   cText   := GetProperty( cForm , cObj, "Value"        )
   cFont   := GetProperty( cForm , cObj, "FontName"     )
   lBold   := GetProperty( cForm , cObj, "FontBold"     )
   nWobj   := GetProperty( cForm , cObj, "ClientWidth"  ) - 2
   nHobj   := GetProperty( cForm , cObj, "ClientHeight" ) - 2
   IF LEN(cText) > 0
      nFSize  := GetFontSize4Text( cText, cFont, , lBold, nWObj, nHobj )
      SetProperty( cForm , cObj, "Fontsize", nFSize ) // change font size
   ENDIF

Return Nil

//////////////////////////////////////////////////////////////////
// Функция вернет максимальный размер фонта
// для заданной строке по ширине и высоте для НЕСКОЛЬКО строк
FUNCTION GetFontSize4Text( cText, cFontName, nFontSize, lBold, nWmax, nHmax )
   LOCAL hFont, nK := 1, cT := "", nHeig, nWidt
   LOCAL nSize := 6 // App.FontSize

   IF CRLF $ cText
      AEval(hb_ATokens(cText, CRLF), {|t,n| nK := Max( nK, n ), cT := iif( Len( t ) > Len( cT ), t, cT ) })
      cText := cT
   ENDIF

//   nSize := nFontSize
   lBold := !Empty(lBold)
   hFont := InitFont( cFontName, nSize, lBold )
   nHeig := GetTextHeight( 0, cText, hFont ) * nK
   nWidt := GetTextWidth ( 0, cText, hFont )
   DeleteObject( hFont )

   IF     nHeig > nHmax .and. nWidt > nWmax
      DO WHILE .T.
         hFont := InitFont( cFontName, nSize, lBold )
         nHeig := GetTextHeight( 0, cText, hFont ) * nK
         nWidt := GetTextWidth ( 0, cText, hFont )
         DeleteObject( hFont )
         IF nHeig <= nHmax .or. nWidt <= nWmax ; nSize ++ ; EXIT
         ENDIF
         nSize --
      ENDDO
   ELSEIF nHeig < nHmax .and. nWidt < nWmax
      DO WHILE .T.
         hFont := InitFont( cFontName, nSize, lBold )
         nHeig := GetTextHeight( 0, cText, hFont ) * nK
         nWidt := GetTextWidth ( 0, cText, hFont )
         DeleteObject( hFont )
         IF nHeig >= nHmax .or. nWidt >= nWmax ; nSize -- ; EXIT
         ENDIF
         nSize ++
      ENDDO
   ENDIF

RETURN iif( Empty(nFontSize), nSize, Min( nFontSize, nSize ) )

//////////////////////////////////////////////////////////////////
// Функция вернет максимальное количество букв "Н" или другое
// для заданной строки: ширина-объекта и ширина-фонта
FUNCTION GetMaxChar4FontWidth( cText, nWidth, cFontName, nFontSize, lBold )
   LOCAL hFont, nWText, nMaxChar
   DEFAULT cText := "x", lBold := .F.

   lBold  := !Empty(lBold)
   hFont  := InitFont( cFontName, nFontSize, lBold )
   nWText := GetTextWidth ( 0, cText, hFont )
   DeleteObject( hFont )
   nMaxChar := INT(nWidth/nWText)

RETURN nMaxChar

/////////////////////////////////////////////////////////////////////////////////////
FUNCTION myTsbListColumn( oBrw )
   LOCAL oCol, nCol, cCol, cSize, cFld, cMsg, cTitle, cTsb

#ifdef KEY_ENG // for this project demo1-en.hbp
   cTitle := 'Info on the list of columns'
#else
   cTitle := 'Инфо по списку колонок'
#endif

   DbSelectArea(oBrw:cAlias)
   cMsg   := "Table alias: " + oBrw:cAlias + ";;"
   FOR nCol := 1 TO Len(oBrw:aColumns)
      oCol  := oBrw:aColumns[ nCol ]
      cCol  := oCol:cName
      cFld  := oCol:cField
      cSize := HB_NtoS( INT(oBrw:GetColSizes()[nCol]) )
      cMsg  += HB_NtoS(nCol) + ") " + cCol + " = " + cSize
      cMsg  += ' ( "'+ cFld + '", "'  + oCol:cFieldTyp + '" '
      cMsg  += HB_NtoS(oCol:nFieldLen)
      cMsg  += ',' + HB_NtoS(oCol:nFieldDec) + ' ) '
      cFld  := cValToChar( oCol:lEdit )    // oCol:cPicture
      cMsg  += ' :lEdit='+ cFld + ';'
   NEXT
   cMsg +=  ";" + REPL("--",20) + "[ TsbArrayLine ]" + REPL("--",20) + ";"
   cTsb := myTsbArrayLine( oBrw , .F., .F.)
   cMsg += cTsb
   AlertInfo(cMsg, cTitle, App.Cargo:cDefAppIcon, 64, {RED})

RETURN Nil

///////////////////////////////////////////////////////////////////////////////
FUNCTION myTsbArrayLine( oBrw , lFlag, lShow)
   LOCAL cMsg, cTitle, aDim, nI, xVal, cVal, nCol, cAls, cFld, cNam, cTyp
   DEFAULT lFlag := .F. , lShow := .T.

#ifdef KEY_ENG // for this project demo1-en.hbp
   cTitle := 'Info on the current table row Info on the current table row'
   cVal   := IIF(oBrw:lIsDbf," - this is a DBF !"," - this is an ARRAY !")
#else
   cTitle := 'Инфо по текущей строке таблицы'
   cVal   := IIF(oBrw:lIsDbf," - это DBF !"," - это МАССИВ !")
#endif

   cMsg := "Table alias: " + oBrw:cAlias + cVal + ";;"
   IF oBrw:lIsDbf
      aDim  := {}
      cAls  := oBrw:cAlias
      FOR nCol := 1 TO oBrw:nColCount()
          cFld := oBrw:aColumns[ nCol ]:cField
          cTyp := oBrw:aColumns[ nCol ]:cFieldTyp
          cNam := oBrw:aColumns[ nCol ]:cName
          IF cNam == "SELECTOR"     ; cVal := cNam
          ELSEIF cNam == "ORDKEYNO" ; cVal := cNam
          ELSE                      ; cVal := (cAls)->&cFld
          ENDIF
          AADD( aDim, { cTyp, cVal, cFld } )
      NEXT
   ELSE
      aDim  := oBrw:aArray[oBrw:nAt]
   ENDIF

   FOR nI := 1 TO Len(aDim)
      cMsg += "(" + STR(nI,2) + ") - "
      xVal := aDim[nI]
      cMsg += "[" + VALTYPE(xVal) + "]   "
      IF IsArray(xVal)
         xVal := HB_ValToExp(xVal)
      ENDIF
      IF !IsString(xVal)
         xVal := myVal2Str(xVal)
      ENDIF
      xVal := ATREPL( ";", xVal, "|" )
      xVal := ATREPL( CRLF, xVal, "|" )
      cMsg += ALLTRIM(xVal) + ";"
   NEXT

   cMsg += REPL("; ",20)

   IF lShow
      AlertInfo(cMsg, cTitle, App.Cargo:cDefAppIcon, 64, {RED} )
   ENDIF

RETURN cMsg

///////////////////////////////////////////////////////////////////////////////
FUNCTION myTsbListFont( oBrw )
   LOCAL cMsg, cTitle, aFont, nI, aFPar, hFont, cFont

#ifdef KEY_ENG // for this project demo1-en.hbp
   cTitle := 'Info on table fonts'
#else
   cTitle := 'Инфо по фонтам таблицы'
#endif

   cMsg   := "Table alias: " + oBrw:cAlias + ";;"
   cMsg   += "     1-Cell: "+hb_valtoexp(GetFontParam(oBrw:hFont)) + ";"
   cMsg   += "     2-Head: "+hb_valtoexp(GetFontParam(oBrw:hFontHead )) + ";"
   cMsg   += "     3-Foot: "+hb_valtoexp(GetFontParam(oBrw:hFontFoot )) + ";"
   cMsg   += "    4-SpcHd: "+hb_valtoexp(GetFontParam(oBrw:hFontSpcHd)) + ";"
   cMsg   += "     5-Edit: "+hb_valtoexp(GetFontParam(oBrw:hFontEdit )) + ";"
   cMsg   += "  6-SuperHd: "+hb_valtoexp(GetFontParam(oBrw:hFontSupHdGet(1))) + ";;"

   cMsg += Replicate( "-._.", 20 ) + ";;"
   cMsg += "1) Height = " + HB_NtoS(oBrw:nHeight) + ";"
   cMsg += "2) HeightHead = " + HB_NtoS(oBrw:nHeightHead) + ";"
   cMsg += "3) HeightSuper = " + HB_NtoS(oBrw:nHeightSuper) + ";"
   cMsg += "4) HeightFoot = " + HB_NtoS(oBrw:nHeightFoot) + ";"
   cMsg += "5) HeightSpecHd = " + HB_NtoS(oBrw:nHeightSpecHd) + ";"
   cMsg += "6) HeightCell = " + HB_NtoS(oBrw:nHeightCell) + ";;"
   cMsg += "Number of rows in the table = " + HB_NtoS(oBrw:nRowCount()) + ";;"
   cMsg += "GetHScrollBarHeight() = " + HB_NtoS(GetHScrollBarHeight()) + ";"
   cMsg += "GetVScrollBarWidth() = " + HB_NtoS(GetVScrollBarWidth()) + ";"
   cMsg += Replicate( "-._.", 20 ) + ";"

   nI := cFont := hFont := aFPar
   aFont := oBrw:Cargo:aFont
   /*FOR nI := 1 TO Len(aFont)
      cFont := aFont[nI]
      hFont := GetFontHandle(cFont)
      aFPar := GetFontParam( hFont )
      cMsg  += "  " + HB_NtoS(nI) + ": "
      cMsg  += cFont + " - ["
      cMsg  += hb_ntos(hFont) + "] - "
      cMsg  += hb_valtoexp(aFPar) + ";"
   NEXT */
   cMsg   += REPL("; ",20)

   AlertInfo(cMsg, cTitle, App.Cargo:cDefAppIcon, 64, {RED})

RETURN Nil

//////////////////////////////////////////////////////////////////
FUNCTION myTsbUseDbf(oBrw)
   LOCAL cMsg, cTitle, cSayFltr, cTsbFilter, cSaySort, cTsbSort

#ifdef KEY_ENG // for this project demo-en.hbp
   cTitle := 'Info on database and indexes'
   cMsg   := 'not defined'
#else
   cTitle := 'Инфо по базе и индексам'
   cMsg   := 'не определено'
#endif

   DbSelectArea(oBrw:cAlias)
   cSayFltr   := oBrw:Cargo:cSayFltr    ; Default cSayFltr   := cMsg
   cTsbFilter := oBrw:Cargo:cTsbFilter  ; Default cTsbFilter := cMsg
   cSaySort   := oBrw:Cargo:cSaySort    ; Default cSaySort   := cMsg
   cTsbSort   := oBrw:Cargo:cTsbSort    ; Default cTsbSort   := cMsg

   cMsg := "Table alias: " + oBrw:cAlias + ";;"
   cMsg += REPL("-",80) + ";"
   cMsg += "FILTER: " + cSayFltr   + ";"
   cMsg += " Table: " + cTsbFilter + ";"
   cMsg += REPL("-",80) + ";"
   cMsg += "  SORTING: " + cSaySort + ";"
   cMsg += "SortTable: " + cTsbSort + ";"
   cMsg += REPL("; ",2)
   cMsg += REPL("-",80) + ";"
   cMsg += myGetIndexUse() + ";"
   cMsg += REPL("-",80) + ";"
   cMsg += Base_Tek("STRING")

   AlertInfo(cMsg, cTitle, App.Cargo:cDefAppIcon, 64, {ORANGE})

RETURN NIL

//////////////////////////////////////////////////////////////////////////////
FUNCTION Button2ZnakUslov(cFH2, cFH1)
   LOCAL aUsl := {}, aZn := {}, Font1, Font2, cRet := ""
   LOCAL cForm := ThisWindow.Name, cObj := This.Name
   LOCAL nZnak := -1  // инициализация
   DEFAULT cFH1 := "", cFH2 := ""

   AADD(aUsl,"  больше (>)          ")  ;  AADD(aZn," > " )
   AADD(aUsl,"  меньше (<)          ")  ;  AADD(aZn," < " )
   AADD(aUsl,"  равно (==)          ")  ;  AADD(aZn," = " )
   AADD(aUsl,"  не равен (#)        ")  ;  AADD(aZn," # " )
   AADD(aUsl,"  больше и равно (>=) ")  ;  AADD(aZn," >= ")
   AADD(aUsl,"  меньше и равно (<=) ")  ;  AADD(aZn," <= ")

   IF LEN(cFH1) > 0
      Font2 := GetFontHandle( cFH2 )
      Font1 := GetFontHandle( cFH1 )
   ELSE
      Font2 := GetFontHandle( "FntCnMn1" )   // фонт-1 в контекстном меню
      Font1 := GetFontHandle( "FntCnMn2" )   // фонт-2 в контекстном меню
   ENDIF

   SET MENUSTYLE EXTENDED
   SetMenuBitmapHeight( 32 )

   DEFINE CONTEXT MENU OF &cForm //Form_Trubka
      MENUITEM  aUsl[1]  ACTION {|| nZnak:=1, SetProperty( cForm, cObj, "Caption", aUsl[1] ) } FONT Font2
      MENUITEM  aUsl[2]  ACTION {|| nZnak:=2, SetProperty( cForm, cObj, "Caption", aUsl[2] ) } FONT Font2
      MENUITEM  aUsl[3]  ACTION {|| nZnak:=3, SetProperty( cForm, cObj, "Caption", aUsl[3] ) } FONT Font2
      MENUITEM  aUsl[4]  ACTION {|| nZnak:=4, SetProperty( cForm, cObj, "Caption", aUsl[4] ) } FONT Font2
      MENUITEM  aUsl[5]  ACTION {|| nZnak:=5, SetProperty( cForm, cObj, "Caption", aUsl[5] ) } FONT Font2
      MENUITEM  aUsl[6]  ACTION {|| nZnak:=6, SetProperty( cForm, cObj, "Caption", aUsl[6] ) } FONT Font2
      SEPARATOR
      MENUITEM  "Выход из меню"  ACTION {|| nZnak:=0, Tone(100,0.1) }  FONT Font1  IMAGE "bExit32"
   END MENU
   _ShowContextMenu(cObj, 0, 0, .f. ) // ПОКАЗ ВЫПАДАЕЩЕГО МЕНЮ

   INKEYGUI(100)

   DEFINE CONTEXT MENU OF &cForm    // deleting menu after exiting
   END MENU

   IF nZnak > 0
      cRet := aZn[nZnak]
      SetProperty(cForm, cObj, "Caption", cRet )
   ENDIF

RETURN cRet


///////////////////////////////////////////////////////////////////////////////
FUNCTION GetTxtHeight( cText, nFontSize, cFontName, lBold )  // получить Height текста
   LOCAL hFont, nHeight
   DEFAULT cText     := "B"                 ,  ;
           cFontName := _HMG_DefaultFontName,  ;   // из MiniGUI.Init()
           nFontSize := _HMG_DefaultFontSize,  ;   // из MiniGUI.Init()
           lBold     := .F.

   hFont := InitFont( cFontName, nFontSize, lBold )
   nHeight := GetTextHeight( 0, cText , hFont )    // высота шрифта
   DeleteObject( hFont )

   RETURN nHeight

* ======================================================================
* При наличии файла добавить число версии в имя
FUNCTION GetFileNameMaskNum( cFile ) //FileNameMaskNum( cFile )
   LOCAL i := 0, cPth, cFil, cExt

   If ! hb_FileExists(cFile); RETURN cFile
   EndIf

   hb_FNameSplit(cFile, @cPth, @cFil, @cExt)

   WHILE ( hb_FileExists( hb_FNameMerge(cPth, cFil + '(' + hb_ntos(++i) + ')', cExt) ) )
   END

   RETURN hb_FNameMerge(cPth, cFil + '(' + hb_ntos(i) + ')', cExt)

* =========================================================================
* При наличии файла добавить число версии в имя файла без расширения файла
FUNCTION GetFileNameMaskNumNotExt( cFile )
   LOCAL i := 0, cPth, cFil, cExt

   If ! hb_FileExists(cFile); RETURN cFile
   EndIf

   hb_FNameSplit(cFile, @cPth, @cFil, @cExt)

   WHILE ( hb_FileExists( hb_FNameMerge(cPth, cFil + '(' + hb_ntos(++i) + ')', cExt) ) )
   END

   RETURN hb_FNameMerge(cPth, cFil + '(' + hb_ntos(i) + ')', cExt)

///////////////////////////////////////////////////////////////////////////////////////////
// Функция проверки установлен ли БОЛЬШОЙ фонт в настройках системы
// Пример вызова:        nSizeFont := IIF(Large2Fonts(),9,11)
FUNCTION Large2Fonts()
LOCAL hDC, nPixelX, lRet := .F.
hDC := CreateDC( "DISPLAY", "", "" )
nPixelX := GetDevCaps( hDC )
DeleteDC( hDc )
IF nPixelX > 100
   lRet := .T.
ENDIF
RETURN (lRet)

///////////////////////////////////////////////////////////////////////////////////////////
// Функция проверки установлен ли БОЛЬШОЙ фонт в настройках системы
// Пример вызова:        nModeLF := LargeFontsMode()
FUNCTION LargeFontsMode()
LOCAL hDC, nPixelX, lRet := .F.
hDC := CreateDC( "DISPLAY", "", "" )
nPixelX := GetDevCaps( hDC )
DeleteDC( hDc )
RETURN nPixelX

///////////////////////////////////////////////////////////////////////////////////////////
// Функция проверки установлен ли БОЛЬШОЙ фонт в настройках системы
// Пример вызова:        nSizeFont := IIF(LargeFonts(),9,11)
FUNCTION LargeFonts()
LOCAL hDC, nPixelX
hDC := CreateDC( "DISPLAY", "", "" )
nPixelX := GetDevCaps( hDC )
DeleteDC( hDc )
RETURN (nPixelX == 120)

#pragma BEGINDUMP
#include <windows.h>
#include "hbapi.h"
HB_FUNC( CREATEDC )
{
   hb_retnl( ( LONG ) CreateDC( hb_parc( 1 ), hb_parc( 2 ), hb_parc( 3 ), 0 ) );
}
HB_FUNC( DELETEDC )
{
   hb_retl( DeleteDC( ( HDC ) hb_parnl( 1 ) ) );
}
HB_FUNC ( GETDEVCAPS )
{
 INT      ix;
 HDC      hdc;
 hdc = ( HDC ) hb_parnl( 1 );

 ix  = GetDeviceCaps( hdc, LOGPIXELSX );

 hb_retni( (UINT) ix );
}
#pragma ENDDUMP

