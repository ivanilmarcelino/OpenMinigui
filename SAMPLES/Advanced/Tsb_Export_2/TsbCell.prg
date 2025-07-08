/*
 * MINIGUI - Harbour Win32 GUI library Demo - Tsbrowse
 *
 * Copyright 2020 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2020 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Использование вспомогательного класса TSBcell для быстрого экспорта данных.
 * Using the auxiliary TSBcell class for quick data export.
 */

#define _HMG_OUTLOG
#define TYPE_EXCEL_FORMULA '#'              // мой тип ФОРМУЛА для экселя

#include "hmg.ch"
#include "TSBrowse.ch"
* ======================================================================
FUNCTION myGetTsbContent(oBrw)     // Содержание таблицы
   LOCAL aTsb, aTsbEnum, aTsbFoot, aTsbHead, aTsbSupH, aTsbCell, cMsg

   CursorWait()
   IF Hb_LangSelect() == "ru.RU1251" ; cMsg := 'Считываю данные с таблицы'
   ELSE                              ; cMsg := 'Read data from the table'
   ENDIF
   WaitThreadCreateIcon( cMsg, )   // запуск со временем

   // нет вывода скрытых колонок
   aTsbEnum := myGetTsbEnum(oBrw)  // массив цвет/фонт/номер нумератора таблицы
   aTsbFoot := myGetTsbFoot(oBrw)  // массив цвет/фонт подвала таблицы
   aTsbHead := myGetTsbHead(oBrw)  // массив цвет/фонт шапки таблицы
   aTsbSupH := myGetTsbSupH(oBrw)  // массив цвет/фонт суперхидера таблицы
   aTsbCell := myGetTsbCell(oBrw)  // массив цвет/фонт ячеек таблицы
   aTsb     := { aTsbSupH, aTsbHead, aTsbEnum, aTsbCell, aTsbFoot }

   WaitThreadCloseIcon()  // kill the window waiting
   CursorArrow()

   RETURN aTsb

* =======================================================================================
// массив цвет/фонт суперхидера таблицы
FUNCTION myGetTsbSupH(oBrw)
   LOCAL aRet, oCel, xVal, aFore, aBack, hFnt, aSup, nFrom, nTo
   LOCAL nCol, oCol, nEnd, nAlign, nVertText

   aRet := {}
   aSup := oBrw:DrawSuper( .F. )
   FOR EACH oCel IN aSup
      hFnt  := oCel:hFont
      aFore := oCel:nClrFore
      aBack := oCel:nClrBack
      xVal  := oCel:cValue
      //   - added the new variables :nFromCol, :nToCol in the class TSBcell.
      nFrom := oCel:nFromCol
      nTo   := oCel:nToCol
      nAlign :=  oCel:nVAlign
      nVertText := oCel:nVertText

      IF nFrom > 0 .AND. nTo > 0

         nEnd := 0

         FOR nCol := nFrom TO nTo
            oCol := oBrw:aColumns[ nCol ]
            If nCol == 1 .and. oBrw:lSelector ; LOOP
            ElseIf ! oCol:lVisible            ; LOOP
            ElseIf oCol:lBitMap               ; LOOP
            EndIf
            // ... обрабатываем тут видимые колонки
            nEnd := nCol
         NEXT
         nTo := nEnd

         IF nEnd > 0
            AADD( aRet, { aFore, aBack, hFnt, xVal, nFrom, nTo, nAlign, nVertText } )
         ENDIF

      ENDIF
      DO EVENTS

   NEXT
   // освобождаем переменные (память)
   AEval( oBrw:aColumns ,{|oc| oc:oCell := NIL, oc:oCellHead := NIL, ;
                           oc:oCellEnum := NIL, oc:oCellFoot := NIL } )
RETURN aRet

* =======================================================================================
// массив цвет/фонт шапки таблицы
FUNCTION myGetTsbHead(oBrw)
   LOCAL aRet, nCol, oCol, oCel, xVal, aFore, aBack, hFnt

   aRet := {}
   oBrw:DrawHeaders( , .F.)  // он создает для Header, SpcHd, Footer, Enum

   IF oBrw:lDrawHeaders
      FOR nCol := 1 TO oBrw:nColCount()
         oCol  := oBrw:aColumns[ nCol ]

         // Колонки, которые не брать
         If nCol == 1 .and. oBrw:lSelector ; LOOP
         ElseIf ! oCol:lVisible            ; LOOP
         ElseIf oCol:lBitMap               ; LOOP
         EndIf

         oCel  := oCol:oCellHead
         hFnt  := oCel:hFont
         aFore := oCel:nClrFore
         aBack := oCel:nClrBack
         xVal  := oCel:cValue
         IF oCel:lMultiLine
            // xVal := StrTran(xVal, CRLF, " ") - не надо
         ENDIF
         AADD( aRet, { aFore, aBack, hFnt, xVal } )
         DO EVENTS
      NEXT
   ENDIF
   // освобождаем переменные (память)
   AEval( oBrw:aColumns ,{|oc| oc:oCell := NIL, oc:oCellHead := NIL, ;
                           oc:oCellEnum := NIL, oc:oCellFoot := NIL } )

RETURN aRet

* =======================================================================================
// массив цвет/фонт/номер нумератора таблицы
FUNCTION myGetTsbEnum(oBrw)
   LOCAL aRet, nCol, oCol, oCel, xVal, aFore, aBack, hFnt, lCol

   aRet := {}
   oBrw:DrawHeaders( , .F.)  // создает для Header, SpcHd, Footer, Enum

   IF oBrw:lDrawSpecHd

      FOR nCol := 1 TO oBrw:nColCount()
         oCol  := oBrw:aColumns[ nCol ]

         // Колонки, которые не брать
         If nCol == 1 .and. oBrw:lSelector ; LOOP
         ElseIf ! oCol:lVisible            ; LOOP
         ElseIf oCol:lBitMap               ; LOOP
         EndIf

         oCel  := oCol:oCellEnum
         hFnt  := oCel:hFont
         aFore := oCel:nClrFore
         aBack := oCel:nClrBack
         xVal  := oCel:cValue
         lCol  := oCol:lVisible
         IF xVal == ""
            xVal := HB_NtoS(nCol)
         ENDIF
         AADD( aRet, { aFore, aBack, hFnt, xVal } )
         DO EVENTS

      NEXT

   ENDIF
   // освобождаем переменные (память)
   AEval( oBrw:aColumns ,{|oc| oc:oCell := NIL, oc:oCellHead := NIL, ;
                           oc:oCellEnum := NIL, oc:oCellFoot := NIL } )

RETURN aRet

* =======================================================================================
// массив фонт/цвет_текст/цвет_фона/значение/тип/формат/имя_поля ячеек таблицы
FUNCTION myGetTsbCell(oBrw)
   LOCAL aRet, aLine, nAt, nCol, oCol, oCel, xVal, aFore, aBack
   LOCAL cName, hFnt, cType, cPict, lCol
   LOCAL nAlign

   aRet := {}

   WITH OBJECT oBrw
   :GoTop()
   :lDrawLine := .F.
   :GoTop()

   FOR nAt := 1 TO :nLen
      :DrawLine()
      aLine := {}
      FOR nCol := 1 TO :nColCount()
         oCol := :aColumns[ nCol ]

         // Колонки, которые не брать
         If nCol == 1 .and. oBrw:lSelector ; LOOP
         ElseIf ! oCol:lVisible            ; LOOP
         ElseIf oCol:lBitMap               ; LOOP
         EndIf

         oCel   := oCol:oCell
         hFnt   := oCel:hFont
         aFore  := oCel:nClrFore
         aBack  := oCel:nClrBack
         nAlign := oCol:nAlign
         Switch nAlign
          case DT_LEFT
            nAlign := "Left"
            Exit
         case DT_CENTER
            nAlign := "Center"
            Exit
         Case DT_RIGHT
            nAlign := "Right"
            Exit
         End switch

         //cType := Valtype(oCel:uValue) - не так
         cType := oCol:cFieldTyp
         cPict := oCol:cPicture
         cName := oCol:cName
         lCol  := oCol:lVisible

         If Valtype(oCel:uValue) == 'L'
            IF Hb_LangSelect() == "ru.RU1251" ; xVal := iif(oCel:uValue,'да','нет')
            ELSE                              ; xVal := iif(oCel:uValue,'yes','no')
            ENDIF
            //xVal  := if(oCel:uValue,'[+]','[ ]')
            cPict := 'XXX'
         Elseif Valtype(oCel:uValue) == 'C'
            xVal  := ALLTRIM(oCel:uValue)
            If SUBSTR(xVal,1,1) = "="        // это формула Excel
               cType := TYPE_EXCEL_FORMULA   // мой формат для формулы
            Endif
         Else
            //xVal  := oCel:cValue - так нельзя ! переводит в текстовый формат
            xVal  := oCel:uValue
            cPict := oCol:cPicture
         Endif

         AADD( aLine, { aFore, aBack, hFnt, xVal, cType, cPict, cName, nAlign } )
         DO EVENTS

      NEXT
      AADD( aRet, aLine )  // строка таблицы
      :GoDown()
      DO EVENTS
   NEXT

   :lDrawLine := .T.
   :Reset()
   // освобождаем переменные (память)
   AEval( :aColumns ,{|oc| oc:oCell := NIL, oc:oCellHead := NIL, ;
                           oc:oCellEnum := NIL, oc:oCellFoot := NIL } )

   END WITH
? "=======", aRet ; ?v aRet

RETURN aRet

* =======================================================================================
// массив цвет/фонт подвала таблицы
FUNCTION myGetTsbFoot(oBrw)
   LOCAL aRet, nCol, oCol, oCel, xVal, aFore, aBack, hFnt, lCol

   aRet := {}
   oBrw:DrawHeaders( , .F.)  // он создает для Header, SpcHd, Footer, Enum
   IF oBrw:lDrawFooters

      FOR nCol := 1 TO oBrw:nColCount()
         oCol  := oBrw:aColumns[ nCol ]

         // Колонки, которые не брать
         If nCol == 1 .and. oBrw:lSelector ; LOOP
         ElseIf ! oCol:lVisible            ; LOOP
         ElseIf oCol:lBitMap               ; LOOP
         EndIf

         oCel  := oCol:oCellFoot
         hFnt  := oCel:hFont
         aFore := oCel:nClrFore
         aBack := oCel:nClrBack
         xVal  := oCel:cValue
         lCol  := oCol:lVisible
         AADD( aRet, { aFore, aBack, hFnt, xVal } )
         DO EVENTS

      NEXT

   ENDIF
   // освобождаем переменные (память)
   AEval( oBrw:aColumns ,{|oc| oc:oCell := NIL, oc:oCellHead := NIL, ;
                           oc:oCellEnum := NIL, oc:oCellFoot := NIL } )
RETURN aRet

* ======================================================================
// извлечь файл картинки из ресурсов программы
FUNCTION myImageReport(cRes)
   LOCAl aImage, cFileLogo, aXY, cMsg, nResult, cFile
   Default cRes := "LogoMG"

   cFile  := cRes + ".png"
   aImage := {}  // нет картинки !  файл лого для экспорта !

   //cFileLogo := GetStartUpFolder() + "\LogoMG.png"
   cFileLogo := GetUserTempFolder() + "\" + cFile
   If !hb_FileExists( cFileLogo )
      nResult := RCDataToFile( cRes, cFileLogo, "PNG" )
      If nResult > 0
      Else
         cMsg := "cRes = '" + cRes + "'" + CRLF
         cMsg += "RCDataToFile() - Code: " + hb_NtoS( nResult ) + CRLF
         cMsg += cFileLogo + CRLF
         MsgStop( cMsg, "Checkout error" )
      Endif
   Endif
   If hb_FileExists( cFileLogo )
      aXY  := hb_GetImageSize( cFileLogo )
      cMsg := cFileLogo + ": " + hb_NtoS( aXY[1] ) + " x " + hb_NtoS( aXY[2] ) + " Pixels"
      //AlertInfo(cMsg)
      aImage := { cFileLogo, aXY[1], aXY[2] }
   Endif

   RETURN aImage

