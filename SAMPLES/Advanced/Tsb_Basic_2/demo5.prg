/*
 * MINIGUI - Harbour Win32 GUI library Demo
 * Copyright 2018 Sergej Kiselev <bilance@bilance.lv>
 *
 * Tsbrowse: Таблица и работа с базой - Seek, Find, Scope, Complex Scope
 * Tsbrowse: Table and work with the base - Seek, Find, Scope, Complex Scope
*/

#define _HMG_OUTLOG

#include "hmg.ch"
#include "TSBrowse.ch"

REQUEST DBFCDX

PROCEDURE Main
   LOCAL oBrw, aAlias, hSpl, x, y
   LOCAL cTitle := "(5) TsBrowse Demo: Seek + Find + Scope + Complex Scope"

   rddSetDefault( 'DBFCDX' )

   SET EPOCH   TO 2000
   SET DATE    TO GERMAN 
   SET CENTURY ON
   SET DELETED ON
   SET AUTOPEN OFF

   SET FONT TO "Arial", 10
   SET DIALOGBOX CENTER OF PARENT
   
   aAlias := UseOpenBase()

   DEFINE WINDOW Form_0          ;
      At 0, 0                    ;
      WIDTH 800                  ;
      HEIGHT 600                 ;
      TITLE cTitle               ;
      ICON "MG_ICO"              ;
      MAIN                       ;
      NOMAXIMIZE NOSIZE          ;
      ON INIT    oBrw:SetFocus() ;      
      ON RELEASE AEval(aAlias, {|wa| dbCloseArea(wa) })

      DEFINE STATUSBAR
         STATUSITEM "Item 1" 
         STATUSITEM cTitle WIDTH 390 FONTCOLOR BLUE
         STATUSITEM "Order: " + HB_NtoS(INDEXORD()) + " " + OrdName(INDEXORD()) WIDTH 140
         KEYBOARD
      END STATUSBAR

      DEFINE SPLITBOX HANDLE hSpl
		DEFINE TOOLBAR ToolBar_1 CAPTION ""               BUTTONSIZE 100,32 FLAT
			BUTTON Seek  CAPTION 'Seek'    PICTURE 'n1'    SEPARATOR WHOLEDROPDOWN
            DEFINE DROPDOWN MENU BUTTON Seek
               ITEM "Seek first 15.10.2018"  IMAGE 'n1' ACTION mySeek(oBrw, 1, .F.)
               ITEM "Seek last  15.10.2018"  IMAGE 'n2' ACTION mySeek(oBrw, 1, .T.)
               SEPARATOR
               ITEM "Seek first 17.10.2018"  IMAGE 'n3' ACTION mySeek(oBrw, 2, .F.)
               ITEM "Seek last  17.10.2018"  IMAGE 'n4' ACTION mySeek(oBrw, 2, .T.)
               SEPARATOR
               ITEM "Seek first 20.10.2018"  IMAGE 'n5' ACTION mySeek(oBrw, 3, .F.)
               ITEM "Seek last  20.10.2018"  IMAGE 'n6' ACTION mySeek(oBrw, 3, .T.)
            END MENU
			BUTTON Find  CAPTION 'Find'    PICTURE 'n2'    SEPARATOR WHOLEDROPDOWN
            DEFINE DROPDOWN MENU BUTTON Find
               ITEM 'Find first "aaa"'       IMAGE 'n1' ACTION myFind(oBrw, 'aaa', .F.)
               ITEM 'Find next  "aaa"'       IMAGE 'n2' ACTION myFind(oBrw, 'aaa', .T.)
               SEPARATOR
               ITEM 'Find first "ccc"'       IMAGE 'n3' ACTION myFind(oBrw, 'ccc', .F.)
               ITEM 'Find next  "ccc"'       IMAGE 'n4' ACTION myFind(oBrw, 'ccc', .T.)
            END MENU
			BUTTON Scope CAPTION 'Scope'   PICTURE 'n3'    SEPARATOR WHOLEDROPDOWN
            DEFINE DROPDOWN MENU BUTTON Scope
               ITEM "Scope first 15.10.2018" IMAGE 'n1' ACTION myScope(oBrw, 1, .F.)
               ITEM "Scope last  15.10.2018" IMAGE 'n2' ACTION myScope(oBrw, 1, .T.)
               SEPARATOR
               ITEM "Scope first 17.10.2018" IMAGE 'n3' ACTION myScope(oBrw, 2, .F.)
               ITEM "Scope last  17.10.2018" IMAGE 'n4' ACTION myScope(oBrw, 2, .T.)
               SEPARATOR
               ITEM "Scope first 20.10.2018" IMAGE 'n5' ACTION myScope(oBrw, 3, .F.)
               ITEM "Scope last  20.10.2018" IMAGE 'n6' ACTION myScope(oBrw, 3, .T.)
               SEPARATOR
               ITEM "Scope first 15.10.2018-17.10.2018" IMAGE 'n7'  ACTION myScope(oBrw, 4, .F.)
               ITEM "Scope last  15.10.2018-17.10.2018" IMAGE 'n8'  ACTION myScope(oBrw, 4, .T.)
               SEPARATOR
               ITEM "Scope first 17.10.2018-20.10.2018" IMAGE 'n9'  ACTION myScope(oBrw, 5, .F.)
               ITEM "Scope last  17.10.2018-20.10.2018" IMAGE 'n10' ACTION myScope(oBrw, 5, .T.)
               SEPARATOR
               ITEM "Reset scope first"     IMAGE 'n11' ACTION myScope(oBrw, 0, .F.)
               ITEM "Reset scope last "     IMAGE 'n12' ACTION myScope(oBrw, 0, .T.)
            END MENU
			BUTTON Scope2 CAPTION 'Complex Scope'   PICTURE 'n4'   SEPARATOR WHOLEDROPDOWN
            DEFINE DROPDOWN MENU BUTTON Scope2
               ITEM "Complex Scope first Nr.=444" IMAGE 'n1' ACTION myScope2(oBrw, 1, .F.)
               ITEM "Complex Scope last  Nr.=444" IMAGE 'n2' ACTION myScope2(oBrw, 1, .T.)
               SEPARATOR
               ITEM "Complex Scope first Nr.=555" IMAGE 'n3' ACTION myScope2(oBrw, 2, .F.)
               ITEM "Complex Scope last  Nr.=555" IMAGE 'n4' ACTION myScope2(oBrw, 2, .T.)
               SEPARATOR
               ITEM "Reset scope first"  IMAGE 'n5' ACTION myScope2(oBrw, 0, .F.)
               ITEM "Reset scope last "  IMAGE 'n6' ACTION myScope2(oBrw, 0, .T.)
            END MENU
			BUTTON Delete CAPTION 'Delete tag'      PICTURE 'n5'   SEPARATOR WHOLEDROPDOWN
            DEFINE DROPDOWN MENU BUTTON Delete
               ITEM "Goto first"          IMAGE 'n1' ACTION myDelete(oBrw, 0, .F.)
               ITEM "Goto last "          IMAGE 'n2' ACTION myDelete(oBrw, 0, .T.)
               SEPARATOR
               ITEM "Set deleted on"      IMAGE 'n3' ACTION myDelete(oBrw, 1, .F.)
               ITEM "Reset view"          IMAGE 'n4' ACTION myDelete(oBrw, 2, .F.)
            END MENU
			BUTTON InfoDb CAPTION 'Info-Dbase'  PICTURE 'n0'   SEPARATOR WHOLEDROPDOWN
            DEFINE DROPDOWN MENU BUTTON InfoDb
               ITEM "Database Information" IMAGE 'n0' ACTION InfoDbase()
            END MENU
		END TOOLBAR
		
		DEFINE TOOLBAR ToolBar_2 CAPTION ""                BUTTONSIZE 42,32 FLAT
			BUTTON Exit  CAPTION 'Exit'    PICTURE 'exit'   ACTION ThisWindow.Release()
		END TOOLBAR
		END SPLITBOX

      x := 5
      y := 5

      y += GetWindowHeight(hSpl)

      oBrw := CreateBrowse(y, x)

   END WINDOW

   Form_0.Center
   Form_0.Activate

RETURN

FUNCTION CreateBrowse( y, x )
   LOCAL nI, aFields, oBrw, oCol

   DEFINE TBROWSE oBrw  AT y, x ;
      OF Form_0 ;
      ALIAS "TEST" ;
      WIDTH  Form_0.Width - 2 * GetBorderWidth() ;
      HEIGHT Form_0.Height - GetTitleHeight() - ;
             Form_0.StatusBar.Height  - 2 * GetBorderHeight() - y - 5  ;
      GRID ;
      COLORS { CLR_BLACK, CLR_BLUE } 

      :SetAppendMode( .F. )      // вставка записи запрещена (в конце базы стрелкой вниз)
      :SetDeleteMode( .T., .T. ) // удаление записи разрешено

      :lNoHScroll  := .T.        // показ горизонтального скролинга
      :lCellBrw    := .F.
      :lInsertMode := .T.        // флаг для переключения режима Вставки при редактировании
      :lPickerMode := .F.        // ввод формата колонки типа ДАТА сделать через цифры

   END TBROWSE

   ADD COLUMN TO TBROWSE oBrw DATA {|| hb_ntoc((oBrw:cAlias)->( OrdKeyNo() )) } ;  
       HEADER "№№" SIZE 40 ;
       COLORS {CLR_BLACK, WHITE} ALIGN DT_CENTER, DT_CENTER, DT_CENTER ;
       NAME NN                             

   // initial columns
   aFields := { "F2", "F1", "F0", "F5","F3", "F4" }
   LoadFields( "oBrw", "Form_0", .F., aFields )

   ADD COLUMN TO TBROWSE oBrw DATA {|| hb_ntoc((oBrw:cAlias)->( RecNo() )) } ;  
       HEADER "Recno" SIZE 70 ;
       COLORS {CLR_BLACK, WHITE} ALIGN DT_CENTER ;
       NAME REC

   // Set columns width
   oBrw:SetColSize( oBrw:nColumn( "F0" ), 60  )
   oBrw:SetColSize( oBrw:nColumn( "F5" ), 60  )
   oBrw:SetColSize( oBrw:nColumn( "F1" ), 80  )
   oBrw:SetColSize( oBrw:nColumn( "F2" ), 200 )
   oBrw:SetColSize( oBrw:nColumn( "F3" ), 80  )
   oBrw:SetColSize( oBrw:nColumn( "F4" ), 70  )

   // Set names for the table header
   oBrw:GetColumn( "F0" ):cHeading := "Nr."  
   oBrw:GetColumn( "F0" ):nAlign   := DT_CENTER   
   oBrw:GetColumn( "F5" ):cHeading := "Room"  
   oBrw:GetColumn( "F5" ):nAlign   := DT_CENTER   
   oBrw:GetColumn( "F2" ):cHeading := "Text"  
   oBrw:GetColumn( "F1" ):cHeading := "Date"      
   oBrw:GetColumn( "F1" ):nAlign   := DT_CENTER   
   oBrw:GetColumn( "F3" ):cHeading := "Number"    
   oBrw:GetColumn( "F4" ):cHeading := "Logical"      

   oBrw:GetColumn('F1'):cPicture := Nil     // пустые поля отображать как пробел

   oBrw:GetColumn('NN'):cFooting := {|nc, ob| nc := ob:nLen, iif( Empty( nc ), '', hb_ntos( nc ) ) }

   oBrw:nWheelLines  := 1
   oBrw:nColOrder    := 0
   oBrw:nClrLine     := COLOR_GRID          // цвет линий между ячейками таблицы
   oBrw:lNoChangeOrd := TRUE                // убрать сортировку по полю
   oBrw:nColOrder    := 0                   // убрать значок сортировки по полю
   oBrw:lCellBrw     := TRUE
   oBrw:lNoVScroll   := TRUE                // отключить показ горизонтального скролинга
   oBrw:hBrush       := CreateSolidBrush( 242, 245, 204 )   // цвет фона под таблицей
   
   // prepare for showing of Double cursor
   AEval( oBrw:aColumns, {| oCol | oCol:lFixLite := .T., ;
                                   oCol:lEdit    := .F., ;
                                   oCol:lOnGotFocusSelect := .T., ;
                                   oCol:lEmptyValToChar   := .T. } )
          // oCol:lOnGotFocusSelect := .T. - включат засинение данных при получении фокуса 
          //   GetBox-ом и сбрасывает, очищает поле при нажатии первого символа 
          // oCol:lEmptyValToChar := .T. - при .T. переводит empty(...) значение поля в ""

   oBrw:nHeightCell += 10        // к высоте ячеек таблицы добавим
   oBrw:nHeightHead += 5         // к высоте шапки таблицы добавим

   // GetBox встраиваем в ячейку, задаем отступы
   oBrw:aEditCellAdjust[1] += 4  // cell_Y + :aEditCellAdjust[1]
   oBrw:aEditCellAdjust[2] += 2  // cell_X + :aEditCellAdjust[2]
   oBrw:aEditCellAdjust[3] -= 5  // cell_W + :aEditCellAdjust[3]
   oBrw:aEditCellAdjust[4] -= 8  // cell_H + :aEditCellAdjust[4]

   oBrw:SetColor( { 1 }, { RGB( 0, 12, 120 ) } )
   oBrw:SetColor( { 2 }, { RGB( 242, 245, 204 ) } )
   oBrw:SetColor( { 5 }, { RGB( 0, 0, 0 ) } )
   oBrw:SetColor( { 6 }, { { | a, b, oBr | IF( oBr:nCell == b, { RGB( 66, 255, 236 ), RGB( 111, 183, 155 ) }, ;
                              { CLR_HRED, CLR_HCYAN } ) } } )  // cursor backcolor

   // ставим цвет по условию
   For nI := 1 To oBrw:nColCount()
      oCol := oBrw:aColumns[ nI ]
      oCol:nClrFore := {|| iif( DELETED(), CLR_YELLOW, CLR_BLACK ) }
      oCol:nClrBack := {|| iif( DELETED(), CLR_GRAY  , RGB( 242, 245, 204 ) ) }
   Next

                              
   oBrw:ResetVScroll()       // показ вертикального скролинга таблицы

   oBrw:lFooting     := .T.                 // использовать подвал таблицы
   oBrw:lDrawFooters := .T.                 // рисовать подвал таблицы
   oBrw:nHeightFoot  := oBrw:nHeightCell-6  // высота строки подвала таблицы
   oBrw:DrawFooters()                       // выполнить прорисовку подвала таблицы

   oBrw:nFreeze     := 1     // Заморозить столбец
   oBrw:lLockFreeze := .T.   // Избегать прорисовки курсора на замороженных столбцах

   oBrw:AdjColumns()
   oBrw:SetNoHoles()         // убрать дырку внизу таблицы перед подвалом

   oBrw:GoPos( 7,3 )         // передвинуть МАРКЕР на 5 строку и 3 колонку

RETURN oBrw

FUNCTION UseOpenBase()
   LOCAL aStr   := {} 
   LOCAL cDbf   := GetStartUpFolder() + "\test5" 
   LOCAL cIndx  := cDbf 
   LOCAL lDbfNo, aChr := {} 
   LOCAL aAlias := {} 
   LOCAL i, c, d, j, n := 0 
   LOCAL a := {'aaa','bbb','ccc','ddd','eee'}
   LOCAL r := {'c','b','a',' '}

   FOR i := 64 TO 240
      AADD( aChr, CHR(i) )
   NEXT
  
   IF ( lDbfNo := ! File( cDbf+'.dbf' ) ) 
      AAdd( aStr, { 'F0', 'N',  7, 0 } ) 
      AAdd( aStr, { 'F1', 'D',  8, 0 } ) 
      AAdd( aStr, { 'F2', 'C', 60, 0 } ) 
      AAdd( aStr, { 'F3', 'N', 10, 2 } ) 
      AAdd( aStr, { 'F4', 'L',  1, 0 } ) 
      AAdd( aStr, { 'F5', 'C',  5, 0 } ) 
      dbCreate( cDbf, aStr ) 
   ENDIF 
  
   IF lDbfNo .OR. !File( cIndx+'.cdx' )
      USE ( cDbf ) ALIAS TEST EXCLUSIVE NEW 
  
      c := CtoD('20.10.2018')
      WHILE TEST->( RecCount() ) < ( 15 * 4 )
         d := c - n++ 
         TEST->( dbAppend() ) 
         TEST->F1 := d
         TEST->F2 := "Line - " + str( n, 3 ) + " " + REPL(aChr[n], 12 )
         TEST->F3 := n 
         TEST->F4 := ( n % 2 ) == 0 
         For i := 1 To Len(a)
             TEST->( dbAppend() )
             TEST->F1 := d
             TEST->F0 := i
             TEST->F2 := a[ i ]
             TEST->F3 := i * 10
         Next
      END 

      n := 10
      c := 10
      j := 1
      GO TOP 
      DO WHILE !EOF()
         i := RECNO()
         TEST->F5 := HB_NtoS(n)
         IF ( i % 2 ) == 0 
            TEST->F5 := HB_NtoS(n) + r[1]
         ENDIF
         IF ( i % 3 ) == 0 
            TEST->F5 := HB_NtoS(n) + r[2]
         ENDIF
         IF ( i % 4 ) == 0 
            TEST->F5 := HB_NtoS(n) + r[3]
         ENDIF
         IF ( i % 5 ) == 0 
            n++
         ENDIF

         IF ( i % 8 ) == 0 .OR. ( i % 9 ) == 0
            TEST->F0 := 444
            TEST->F2 := ALLTRIM(TEST->F2) + "   (444)"
            TEST->F5 := HB_NtoS(c) + r[j]
            j++
            j := IIF(j > LEN(r), 1, j)
            c--
         ENDIF 
         IF ( i % 11 ) == 0 .OR. ( i % 12 ) == 0
            TEST->F0 := 555
            TEST->F2 := ALLTRIM(TEST->F2) + "   (555)"
            TEST->F5 := HB_NtoS(c) + r[j]
            c--
         ENDIF 
         c := IIF(c < 1, 8, c)
         
         IF ( i % 6 ) == 0
            TEST->F2 := " (deleted records)"
            TEST->F1 := CTOD("")
            TEST->F0 := 0
            TEST->F3 := 0
            TEST->F4 := .F.
            TEST->F5 := ""
            DbDelete()
         ENDIF
         SKIP
      ENDDO
  
      GO TOP 
      INDEX ON DTOS(FIELD->F1)+STR(FIELD->F0)  TAG DTN   FOR !Deleted() 
      INDEX ON RECNO()                         TAG DEL   FOR  Deleted()          
      // Необходимо для этого индекса указать длину, иначе нет ясности к какой длине приводить
      // It is necessary to specify the length for this index, otherwise it is not clear what length to bring
      INDEX ON STR(FIELD->F0, 7)+STR(VAL(FIELD->F5), 4)+FIELD->F5 TAG ROOM FOR !Deleted() 
      USE 
   ENDIF 
  
   SET AUTOPEN ON
  
   USE ( cDbf ) ALIAS TEST SHARED NEW 
   If OrdCount() > 0
      OrdSetFocus(1) 
   EndIf
   GO TOP 

   SET AUTOPEN OFF
  
   AADD( aAlias, ALIAS() )

RETURN aAlias

FUNCTION mySeek( oBrw, nDat, lLast )
   LOCAL lRet, cDat, cVal
   LOCAL aDat := { ;
                   CtoD('15.10.2018'), ;
                   CtoD('17.10.2018'), ;
                   CtoD('20.10.2018'), ;
                 }

   DbSetOrder(1)

   cVal := "Order: " + HB_NtoS(INDEXORD()) + " " + OrdName(INDEXORD())
   SetProperty( ThisWindow.Name, "StatusBar" , "Item" , 3, cVal )  

   cDat := DtoS(aDat[ nDat ])
   lRet := oBrw:SeekRec(cDat, .T., lLast)

   oBrw:SetFocus()

RETURN lRet

FUNCTION myFind( oBrw, cTxt, lNext )
   LOCAL lRet, b, cVal, l := len(cTxt)

   DbSetOrder(0)
   oBrw:Refresh()

   cVal := "Order: " + HB_NtoS(INDEXORD()) + " " + OrdName(INDEXORD())
   SetProperty( ThisWindow.Name, "StatusBar" , "Item" , 3, cVal )  

   b := hb_macroblock( 'left(F2, '+hb_ntos(l)+') == "'+cTxt+'"' )
   
   lRet := oBrw:FindRec(b, lNext)

   oBrw:SetFocus()

RETURN lRet

FUNCTION myScope( oBrw, nDat, lBottom )
   LOCAL lRet, cDat, cEnd, cVal
   LOCAL aDat := { ;
                   CtoD('15.10.2018'), ;
                   CtoD('17.10.2018'), ;
                   CtoD('20.10.2018'), ;
                 }

   If empty(nDat)
   ElseIf nDat == 4
      cDat := DtoS(aDat[ 1 ])
      cEnd := DtoS(aDat[ 2 ])
   ElseIf nDat == 5
      cDat := DtoS(aDat[ 2 ])
      cEnd := DtoS(aDat[ 3 ])
   Else
      cDat := DtoS(aDat[ nDat ])
      cEnd := cDat
   EndIf

   DbSetOrder(1)
   cVal := "Order: " + HB_NtoS(INDEXORD()) + " " + OrdName(INDEXORD())
   SetProperty( ThisWindow.Name, "StatusBar" , "Item" , 3, cVal )  

   lRet := oBrw:ScopeRec(cDat, cEnd, lBottom)

   oBrw:SetFocus()

RETURN lRet

FUNCTION myScope2( oBrw, nKey, lBottom )
   LOCAL lRet, cDat, cEnd, cVal
   LOCAL aDat := { 444, 555 }

   // INDEX ON STR(F0, 7)+STR(VAL(F5), 4)+F5 TAG ROOM FOR !Deleted() 
   // выражение для Scope делаем равным индексу
   If empty(nKey)
   ElseIf nKey == 1
      cDat := STR(aDat[ 1 ], 7) 
      cEnd := STR(aDat[ 1 ], 7) 
   ElseIf nKey == 2
      cDat := STR(aDat[ 2 ], 7) 
      cEnd := STR(aDat[ 2 ], 7) 
   Else
    cDat := Nil // STR(aDat[ nKey ])
    cEnd := Nil // cDat
   EndIf

   SET ORDER TO TAG ROOM

   cVal := "Order: " + HB_NtoS(INDEXORD()) + " " + OrdName(INDEXORD())
   SetProperty( ThisWindow.Name, "StatusBar" , "Item" , 3, cVal )  

   lRet := oBrw:ScopeRec(cDat, cEnd, lBottom)

   DO EVENTS

   oBrw:SetFocus()

RETURN lRet

FUNCTION myDelete( oBrw, nKey, lBottom )
   LOCAL lRet, cDat := NIL, cEnd := NIL, cVal
   DEFAULT nKey := 0

   If empty(nKey); SET DELETED  OFF
   Else          ; SET DELETED  ON
   EndIf

   If nKey == 2

      SET ORDER TO 1
      SET SCOPE TO
      GO TOP

      oBrw:Reset() 

   Else

      SET ORDER TO TAG DEL
    
      cVal := "Order: " + HB_NtoS(INDEXORD()) + " " + OrdName(INDEXORD())
      SetProperty( ThisWindow.Name, "StatusBar" , "Item" , 3, cVal )  
    
      lRet := oBrw:ScopeRec(cDat, cEnd, lBottom)

   EndIf

   DO EVENTS

   oBrw:SetFocus()

RETURN lRet


FUNCTION InfoDbase()
RETURN MsgInfo( Base_Current(), "Open databases" )


#include "Dbinfo.ch"
FUNCTION Base_Current()
   LOCAL cMsg, nI, nSel, nOrder, cAlias, cIndx, aIndx := {}

   cAlias := ALIAS()
   nSel := SELECT(cAlias)
   IF nSel == 0
      cMsg := "No open BASE !" + CRLF 
      RETURN cMsg
   ENDIF

   nOrder := INDEXORD()  
   cMsg   := "Open Database - alias: " + cAlias + "   RddName: " + RddName() + CRLF
   cMsg   += "Path to the database - " + DBINFO(DBI_FULLPATH) + CRLF + CRLF
   cMsg   += "Open indexes: "

   IF nOrder == 0
      cMsg += " (no indexes) !" + CRLF 
   ELSE
      cMsg += ' DBOI_ORDERCOUNT: ( ' + HB_NtoS(DBORDERINFO(DBOI_ORDERCOUNT)) + ' )' + CRLF + CRLF
      FOR nI := 1 TO 100
         cIndx := ALLTRIM( DBORDERINFO(DBOI_FULLPATH,,ORDNAME(nI)) )
         IF cIndx == ""
            EXIT
         ELSE
            DBSetOrder( nI )
            cMsg += STR(nI,3) + ') - Index file: ' + DBORDERINFO(DBOI_FULLPATH) + CRLF
            cMsg += '     Index Focus: ' + ORDSETFOCUS() + ",  DBSetOrder(" + HB_NtoS(nI)+ ")" + CRLF
            cMsg += '       Index key: "' + DBORDERINFO( DBOI_EXPRESSION ) + '"' + CRLF
            cMsg += '       FOR index: "' + OrdFor() + '" ' + SPACE(5)
            cMsg += '   DBOI_KEYCOUNT: ( ' + HB_NtoS(DBORDERINFO(DBOI_KEYCOUNT  )) + ' )' + CRLF + CRLF
            AADD( aIndx, STR(nI,3) + "  OrdName: " + OrdName(nI) + "  OrdKey: " + OrdKey(nI) )
         ENDIF
      NEXT
      DBSetOrder( nOrder ) 
      cMsg += "Current index = "+HB_NtoS(nOrder)+" , Index Focus: " + ORDSETFOCUS()
   ENDIF
   cMsg += "   Number of records = " + HB_NtoS(ORDKEYCOUNT()) + CRLF

   RETURN cMsg
