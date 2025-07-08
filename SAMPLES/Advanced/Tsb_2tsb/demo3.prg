/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2021 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2021 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * ƒве таблицы. ‘ункци€ _TBrowse() дл€ Tsbrowse
 * Two tables. _TBrowse() function for Tsbrowse
*/
//#define _HMG_OUTLOG
#include "hmg.ch"
#include "tsbrowse.ch"
#include "dbinfo.ch"

REQUEST DBFCDX

Function Main()
   LOCAL cFont := "Arial"
   LOCAL nSize := 12
   LOCAL cForm := "wMain"
   LOCAL oBrw1, oBrw2, nY, nX, nH, nW, oTsb1, oTsb2

   RddSetDefault("DBFCDX")

   SET OOP ON

   SET EPOCH   TO 2000
   SET DATE    TO GERMAN
   SET CENTURY ON
   SET AUTOPEN OFF
   SET DELETED OFF

   SET FONT TO cFont, nSize
   // фонт по default дл€ oTsb1, oBrw1
   DEFINE FONT Normal FONTNAME _HMG_DefaultFontName SIZE _HMG_DefaultFontSize
   DEFINE FONT Bold   FONTNAME _HMG_DefaultFontName SIZE _HMG_DefaultFontSize BOLD
   DEFINE FONT Italic FONTNAME _HMG_DefaultFontName SIZE _HMG_DefaultFontSize BOLD ITALIC

   USE ( "CUSTOMER" )  ALIAS CUST1 NEW SHARED
   USE ( "CUSTOMER2" ) ALIAS CUST2 NEW SHARED

   DEFINE WINDOW &cForm TITLE "Two tables with customization" ;
          MAIN NOSIZE TOPMOST ;
          ON INIT    ( This.Topmost := .F. ) ;
          ON RELEASE ( dbCloseAll() )

          This.Maximize

      nY := nX := 0
      nW := This.ClientWidth
      nH := Int( This.ClientHeight / 2 )

      oTsb1 := oHmgData()  // названи€ переменных смотри ф-ю _TBrowse()
      oBrw1 := _TBrowse( oTsb1, "CUST1", "Brw_1", nY, nX, nW, nH )

      nY += nH + 1
      nH -= 1

      oTsb2 := Tbrowse_OnInit("CUST2")         // начальные данные дл€ таблицы
      oBrw2 := _TBrowse( oTsb2, "CUST2", "Brw_2", nY, nX, nW, nH )
      Tbrowse_Customization(oBrw2, oTsb2)       // донастройка таблицы

      oBrw1:SetFocus() ; DO EVENTS

      ON KEY TAB       ACTION {|cf| cf := ThisWindow.FocusedControl, ;
                              iif( cf == "Brw_1", This.Brw_2.SetFocus, This.Brw_1.SetFocus ) }
      ON KEY SHIFT+TAB ACTION {|cf| cf := ThisWindow.FocusedControl, ;
                              iif( cf == "Brw_1", This.Brw_2.SetFocus, This.Brw_1.SetFocus ) }
      ON KEY ESCAPE    ACTION ( iif( oBrw1:IsEdit, oBrw1:SetFocus(), ;
                              iif( oBrw2:IsEdit, oBrw2:SetFocus(), ;
                              ThisWindow.Release ) ) )
      ON KEY F1 ACTION NIL

   END WINDOW

   ACTIVATE WINDOW wMain

RETURN NIL

*----------------------------------------------------------------------------*
FUNCTION Tbrowse_OnInit(uAlias)    // начальные данные дл€ таблицы
   LOCAL cTitle, cFont, nSize, oTsb

   IF HB_ISCHAR( uAlias ) ; dbSelectArea( uAlias )
   ENDIF

   oTsb := oHmgData()  // названи€ переменных смотреть ф-ю _TBrowse()

   // фонт дл€ oTsb2, oBrw2
   cFont := "DejaVu Sans Mono"
   nSize := 14
   DEFINE FONT Tsb_Normal  FONTNAME cFont SIZE nSize
   DEFINE FONT Tsb_Bold    FONTNAME cFont SIZE nSize BOLD
   DEFINE FONT Tsb_Italic  FONTNAME cFont SIZE 10    BOLD ITALIC

   oTsb:aBrush     := GetSysColor( COLOR_BTNFACE )
   oTsb:aFont      := {"Tsb_Normal", "Tsb_Bold", "Tsb_Bold", "Tsb_Italic", "Tsb_Bold" }
   oTsb:aNumber    := { 1, 50 }
   oTsb:uSelector  := 20
   oTsb:aFoot      := .T.
   oTsb:aEdit      := .T.
   oTsb:lSpecHd    := .T.
   // блок инициализации
   oTsb:bInit      := {|ob,op|
         ob:GetColumn("ORDKEYNO"):hFont := GetFontHandle(op:aFont[4])    // "Italic"
         // это как пример центровки колонок
         AEval(ob:aColumns, {|oc| oc:nAlign  := iif( oc:cFieldTyp $ "DLT^=@", DT_CENTER, oc:nAlign ) }) // это как пример
         AEval(ob:aColumns, {|oc| oc:nFAlign := oc:nAlign }) // это как пример, перенесли Align на Footer
         AEval(ob:aColumns, {|oc| oc:nWidth  += iif( oc:cFieldTyp $ "T=@", 15, 0 ) }) // увеличили width у TimeStamp
         // задать высоту объектов таблицы
         ob:nHeightSpecHd := 16                             // высота спецхидера ENUMERATOR
         ob:nHeightCell   += 4                              // добавим пикселей к высоте €чеек
         ob:nHeightHead   := GetFontHeight(op:aFont[2])     // высота шапки
         ob:nHeightFoot   := GetFontHeight(op:aFont[4]) + 6 // высота подвала
         Return Nil
         }

   // —оздаЄм —”ѕ≈–’»ƒ≈– в таблице
   cTitle          := "Example! CodePage: " + DBINFO(DBI_CODEPAGE)
   cTitle          += " , Alias: " + ALIAS() + " , " + DBINFO(DBI_FULLPATH)
   cTitle          += " , " + RddName()
   oTsb:cSupHd     := cTitle          // заголовок суперхидера
   oTsb:nSupHdBack := CLR_CYAN        // цвет фона суперхидера
   oTsb:nSupHdFore := CLR_YELLOW      // цвет текста суперхидера
   // основной блок
   oTsb:bBody := {|ob,op|
         Local nFrom, nTo, nI, nOrdKeyNo

         nOrdKeyNo := ob:nColumn( "ORDKEYNO", .T. )
         nFrom := nTo := nI := 1
         IF nOrdKeyNo > 0
            nTo := nOrdKeyNo
         ENDIF

         IF nOrdKeyNo > 0
            ADD SUPER HEADER TO ob FROM nFrom TO nTo Color op:nSupHdFore, op:nSupHdBack
            nFrom := nTo + 1
            nI    += 1
         ENDIF

         nTo := ob:nColCount()
         ADD SUPER HEADER TO ob FROM nFrom TO nTo Color op:nSupHdFore, op:nSupHdBack ;
                   TITLE " " + op:cSupHd HORZ DT_CENTER

         ob:nHeightSuper := 28                    // высота суперхидера
         Return Nil
         }

RETURN oTsb

*----------------------------------------------------------------------------*
FUNCTION Tbrowse_Customization( oBrw, oTsb )    // донастройка таблицы
   LOCAL oCol, nClrNoDbf, cCol, cTyp, nI

   // цвета изменить
   nClrNoDbf     := GetSysColor( COLOR_BTNFACE )
   oCol          := oBrw:GetColumn("ORDKEYNO")
   oCol:nClrBack := nClrNoDbf

   // изменение цвета спецхидера - ENUMERATOR (нумераци€ колонок)
   FOR EACH oCol IN oBrw:aColumns
      oCol:nClrSpcHdBack := nClrNoDbf     // ::aColorsBack[ 18 ]
      oCol:nClrSpcHdFore := CLR_RED       // ::aColorsBack[ 19 ]
   NEXT

   EVal( oTsb:bSpecHdEnum, oBrw, oTsb, "#" )   //!!! применение блока перенумерации SpecHd

   // дл€ удалЄнных записей
   oBrw:SetColor( { CLR_PANE   }, { {|nr,nc,ob| myColorCell(nr,nc,ob, CLR_GRAY , CLR_WHITE, nClrNoDbf) }} ) // 2 , фона в €чейках таблицы
   oBrw:SetColor( { CLR_TEXT   }, { {|nr,nc,ob| myColorCell(nr,nc,ob, CLR_HGRAY, CLR_BLACK, CLR_RED  ) }} ) // 1 , текста в €чейках таблицы

   // цвет фона шапки таблицы + подвала списка колонок
   FOR nI := oBrw:nColumn("ORDKEYNO") TO Len( oBrw:aColumns )
      oCol := oBrw:aColumns[ nI ]
      cTyp := oCol:cFieldTyp
      IF cTyp $ "+=^" .OR. nI == oBrw:nColCount() // Type: [+] [=] [^]
         oCol:nClrHeadBack  := CLR_ORANGE
         oCol:nClrFootBack  := CLR_ORANGE
      ENDIF
   NEXT

   // «апрет правки колонок типа "+=^"
   AEval(oBrw:aColumns, {|oc| oc:lEdit := iif( oc:cFieldTyp $ "+=^", .F., oc:lEdit )})

   // Ћевый верхний + нижний уголок - specialаselector header background color
   oBrw:nClrSelectorHdBack := nClrNoDbf

   // подвал таблицы - изменить
   FOR EACH oCol IN oBrw:aColumns
      cCol := oCol:cName
      IF cCol == "SELECTOR" .OR. cCol == "ORDKEYNO"
      ELSE
         oCol:hFontFoot    := GetFontHandle(oTsb:aFont[4])
         oCol:nClrFootFore := CLR_RED
         oCol:cFooting     := cCol
         //oCol:nFAlign    := DT_CENTER
      ENDIF
   NEXT

   //  назначаем на суперхидер отдельную функцию Ё“ќ ƒ≈Ћј≈ћ ѕќ—Ћ≈ END TBROWSE
   FOR EACH oCol IN oBrw:aColumns
      // лева€ /*и права€*/ кнопка мышки дл€ шапки таблицы
      oCol:bHLClicked := {|Ypix,Xpix,nAt,ob| iif(Ypix > ob:nHeightSuper, ;
                           MsgDebug("Header:",Ypix,Xpix,nAt,ob:cAlias),  ;
                           MsgDebug("Super:",Ypix,Xpix,nAt,ob:cAlias))}
   NEXT

RETURN NIL

///////////////////////////////////////////////////////////////////////////////
// цвета €чеек в таблице
FUNCTION myColorCell( nAt, nCol, oBrw, nClrDel, nClrIsx, nClrNoDbf )
   LOCAL nColor := nClrIsx, lDel, cCol
   Default nAt := 0 , nCol := 0

   lDel := (oBrw:cAlias)->( DELETED() )
   cCol := oBrw:aColumns[ nCol ]:cName
   IF cCol == "SELECTOR" .OR. cCol == "ORDKEYNO"
      nColor := nClrNoDbf
   ELSEIF lDel // удалена ли запись ?
      nColor := nClrDel
   ENDIF

RETURN nColor
