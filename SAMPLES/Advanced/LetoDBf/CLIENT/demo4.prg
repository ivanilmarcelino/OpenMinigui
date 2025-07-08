/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Author: Igor Nazarov + Verchenko Andrey <verchenkoag@gmail.com>
 * Correcting the code by Sergej Kiselev <bilance@bilance.lv>
 * Revised by Grigory Filatov <gfilatov@inbox.ru>
*/
#define _HMG_OUTLOG
#define CaseSensitive      // отключить #define set(...)   LETO_SET(...) !!!

#include "hmg.ch"
#include "TSBrowse.ch"
#include "rddleto.ch"
#include "letofile.ch"

PROCEDURE Demo4( oWnd, cPath )
   LOCAL oBr, aAlias, cForm := 'wDemo4'
   LOCAL g, y, x, w, h, i

   oWnd:SendMsg(20)

   fErase('_Msglog.txt')

   cPath := "//" + cPath + IiF( ":" $ cPath, "", ":" + ALLTRIM( STR( nPort ) ) )
   cPath += Iif( Right(cPath,1) == "/", "", "/" )

   rddSetDefault( 'LETO' )

   IF leto_Connect( cPath ) < 0
      MsgStop('Connect error ( '+cPath+' ) : '+hb_ntos(LETO_CONNECT_ERR())+;
               CRLF+CRLF+upper(LETO_CONNECT_ERR(.T.))+' !', 'LetoDBf')
      RETURN
   ENDIF

   If Empty( aAlias := UseOpenBase() )
      leto_disconnect()
      MsgStop('Base not open !', Procname())
      RETURN
   EndIf

   i := 0; WHILE _IsWindowDefined(cForm); cForm += '_'+hb_ntos(++i); END

   DEFINE WINDOW &cForm ;
      WIDTH 700 ;
      HEIGHT 600 ;
      TITLE cForm + ". TsBrowse demo. Alias: "+Alias() ;
      CHILD ;
      NOMAXIMIZE NOSIZE ;
      ON INIT    oBr:SetFocus() ;      
      ON RELEASE ( AEval( aAlias, {|wa| dbCloseArea(wa) } ), leto_Disconnect() )

   DEFINE STATUSBAR
      STATUSITEM "Item 1" WIDTH 0
      STATUSITEM "Network opening of the database!" WIDTH 290 FONTCOLOR BLUE
   END STATUSBAR

   y := x := 5
   g := 2
   w := 90
   h := 30
   
   DEFINE BUTTONEX Button_Up
      Row    y
      Col    x 
      WIDTH  w
      HEIGHT h
      CAPTION "("+chr(177)+") Up"
      ACTION RecMove(oBr,-1)
      FONTSIZE 10
      FONTBOLD .T.
   END BUTTONEX
   
   x += w + g
   DEFINE BUTTONEX Button_Down
      Row    y
      Col    x
      WIDTH  w
      HEIGHT h
      CAPTION "("+chr(177)+") Down"
      ACTION RecMove(oBr,1)
      FONTSIZE 10
      FONTBOLD .T.
   END BUTTONEX
   
   x += w + g
   DEFINE BUTTONEX Button_Ins
      Row    y
      Col    x
      WIDTH  w
      HEIGHT h
      CAPTION "(+) Insert"
      ACTION RecInsert(oBr)
      FONTSIZE 10
      FONTBOLD .T.
   END BUTTONEX

   x += w + g
   DEFINE BUTTONEX Button_Exit
      Row    y
      Col    x 
      WIDTH  w 
      HEIGHT h
      CAPTION "Exit"
      ACTION ThisWindow.Release()
      FONTSIZE 10
      FONTBOLD .T.
   END BUTTONEX

   x += w + g * 4

   oBr := CreateBrowse()
   
   @ y, x LABEL Rec WIDTH 60 HEIGHT h VALUE 'Recno:' ;
          TRANSPARENT CENTERALIGN VCENTERALIGN
   
   x += This.Rec.Width + g
   DEFINE GETBOX RecNo
      Row    y + 2
      Col    x 
      WIDTH  30
      HEIGHT 24
      VALUE 1
      FONTNAME 'Arial'
      FONTSIZE 9
      TOOLTIP ''
      MAXLENGTH  255
      PICTURE "99"
   END GETBOX

   x += This.RecNo.Width + g
   
   @ y, x LABEL Pos WIDTH 70 HEIGHT h VALUE 'RowPos:' ;
          TRANSPARENT CENTERALIGN VCENTERALIGN

   x += This.Pos.Width + g
   DEFINE GETBOX RowPos
      Row    y + 2
      Col    x 
      WIDTH  30
      HEIGHT 24
      VALUE 1
      FONTNAME 'Arial'
      FONTSIZE 9
      TOOLTIP ''
      MAXLENGTH  255
      PICTURE "99"
   END GETBOX

   x += This.RowPos.Width + g
   DEFINE BUTTONEX Button_Go
      Row    y + 2
      Col    x 
      WIDTH  40 
      HEIGHT 24
      CAPTION "Go"
      ACTION ( oBr:SetFocus(), oBr:GotoRec(This.Recno.Value, This.RowPos.Value ) )
      FONTSIZE 9
      FONTBOLD .F.
   END BUTTONEX

   END WINDOW

   CENTER WINDOW   &cForm
   ACTIVATE WINDOW &cForm

RETURN

FUNCTION CreateBrowse()

   LOCAL aFields, oBrw

   DEFINE TBROWSE oBrw  AT 5 + This.Button_Ins.Height + 5, 5 ;
      ALIAS Alias() ;
      WIDTH  This.Width - 2 * GetBorderWidth() ;
      HEIGHT This.Height - GetTitleHeight() - ;
             This.StatusBar.Height  - 2 * GetBorderHeight() - ;
             This.Button_Ins.Height - 5  ;
      GRID ;
      COLORS { CLR_BLACK, CLR_BLUE } ;
      FONT "Serif_N"  SIZE 8

      :SetAppendMode( .F. )      // вставка записи запрещена (в конце базы стрелкой вниз)
      :SetDeleteMode( .T., .T. ) // удаление записи разрешено

      :lNoHScroll  := .T.        // показ горизонтального скролинга
      :lCellBrw    := .F.
      :lInsertMode := .T.        // флаг для переключения режима Вставки при редактировании
      :lPickerMode := .F.        // ввод формата колонки типа ДАТА сделать через цифры

   END TBROWSE

   ADD COLUMN TO TBROWSE oBrw DATA {|| hb_ntoc((oBrw:cAlias)->( OrdKeyNo() )) } ;  
       HEADER CRLF + "NN" SIZE 40 ;
       COLORS {CLR_BLACK, WHITE} ALIGN DT_CENTER ;
       NAME NN                             

   // initial columns
   aFields := { "F2", "F1", "F3", "F4", "F0" }
   LoadFields( "oBrw", This.Name, .F., aFields )

   ADD COLUMN TO TBROWSE oBrw DATA {|| hb_ntoc((oBrw:cAlias)->( RecNo() )) } ;  
       HEADER CRLF + "Recno" SIZE 40 ;
       COLORS {CLR_BLACK, WHITE} ALIGN DT_CENTER ;
       NAME REC

   // Set columns width
   oBrw:SetColSize( oBrw:nColumn( "F0" ), 60  )
   oBrw:SetColSize( oBrw:nColumn( "F1" ), 90  )
   oBrw:SetColSize( oBrw:nColumn( "F2" ), 200 )
   oBrw:SetColSize( oBrw:nColumn( "F3" ), 90  )
   oBrw:SetColSize( oBrw:nColumn( "F4" ), 80  )

   // Set names for the table header
   oBrw:GetColumn( "F0" ):cHeading := "Key"  
   oBrw:GetColumn( "F2" ):cHeading := "Text"  
   oBrw:GetColumn( "F1" ):cHeading := "Date"      
   oBrw:GetColumn( "F3" ):cHeading := "Number"    
   oBrw:GetColumn( "F4" ):cHeading := "Logical"      

   oBrw:GetColumn('F1'):cPicture := Nil     // пустые поля отображать как пробел

   oBrw:nWheelLines  := 1
   oBrw:nClrLine     := COLOR_GRID          // цвет линий между ячейками таблицы
   oBrw:lNoChangeOrd := TRUE                // убрать сортировку по полю
   oBrw:nColOrder    := 0                   // убрать значок сортировки по полю
   oBrw:lCellBrw     := TRUE
   oBrw:lNoVScroll   := TRUE                // отключить показ горизонтального скролинга
   oBrw:hBrush       := CreateSolidBrush( 242, 245, 204 )   // цвет фона под таблицей

   // prepare for showing of Double cursor
   AEval( oBrw:aColumns, {| oCol | oCol:lFixLite := .T., ;
                                   oCol:lEdit := ! oCol:cName $ ',NN,REC,', ;
                                   oCol:lOnGotFocusSelect := .T.,       ;
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
                              
   oBrw:ResetVScroll()       // показ вертикального скролинга таблицы

   oBrw:lFooting     := .T.  // использовать подвал таблицы
   oBrw:lDrawFooters := .T.  // рисовать подвал таблицы
   oBrw:nHeightFoot  := 6    // высота строки подвала таблицы
   oBrw:DrawFooters()        // выполнить прорисовку подвала таблицы

   oBrw:nFreeze     := 1     // Заморозить столбец
   oBrw:lLockFreeze := .T.   // Избегать прорисовки курсора на замороженных столбцах

   oBrw:SetNoHoles()         // убрать дырку внизу таблицы перед подвалом

   oBrw:GoPos( 7,3 )         // передвинуть МАРКЕР на 5 строку и 3 колонку

RETURN oBrw

FUNCTION UseOpenBase()
   LOCAL aStr   := {} 
   LOCAL cDbf   := "TEST4" 
   LOCAL cAls   := cDbf
   LOCAL cAlias := cDbf
   LOCAL cIndx  := cDbf 
   LOCAL lDbfNo, aChr := {} 
   LOCAL aAlias := {}
   LOCAL i, n   := 0 

   FOR i := 64 TO 240
      AADD( aChr, CHR(i) )
   NEXT
  
   IF ( lDbfNo := ! File( cDbf+'.dbf' ) ) 
      AAdd( aStr, { 'F0', 'N',  7, 2 } ) 
      AAdd( aStr, { 'F1', 'D',  8, 0 } ) 
      AAdd( aStr, { 'F2', 'C', 60, 0 } ) 
      AAdd( aStr, { 'F3', 'N', 10, 2 } ) 
      AAdd( aStr, { 'F4', 'L',  1, 0 } ) 
      dbCreate( cDbf, aStr ) 
   ENDIF 

   i := 0; WHILE Select(cAls) > 0; cAls := cAlias + '_'+hb_ntos(++i); END
  
   IF lDbfNo .OR. !File( cIndx+'.cdx' )
      USE ( cDbf ) ALIAS &cAls EXCLUSIVE NEW 
      If Used()
         i := 0
         WHILE (cAls)->( RecCount() ) < 15
            i++
            (cAls)->( dbAppend() ) 
            (cAls)->F0 := i
            (cAls)->F1 := Date() - n++ 
            (cAls)->F2 := "Line - " + HB_NtoS( n ) + " " + REPL(aChr[n], 12 )
            (cAls)->F3 := n 
            (cAls)->F4 := ( n % 2 ) == 0 
         END 
     
         GO TOP 
         INDEX ON FIELD->F0 TAG IDN FOR !Deleted() 
         INDEX ON RECNO()   TAG DEL FOR  Deleted()          
         USE 
      Else
         RETURN aAlias
      EndIf
   ENDIF 
  
   USE ( cDbf ) ALIAS &cAls SHARED NEW 
   If Used()
      OrdSetFocus('IDN') 
      GO TOP 
      AADD( aAlias, ALIAS() )
   EndIf

RETURN aAlias

STATIC FUNCTION RecInsert(oBrw)
   LOCAL nRow := oBrw:nRowPos
   LOCAL nMax := oBrw:nRowCount()
   LOCAL nPos, lAppend, nRecno := 0
   LOCAL i, k, l

   IF MsgYesNo( "You want to insert record in the table ?", "Сonfirmation", .f. )
      l := 2
      k := int( (oBrw:cAlias)->F0 )
      i := 0
      nRecno := (oBrw:cAlias)->(RecNo())
      DO WHILE !EOF() .and. k == int( (oBrw:cAlias)->F0 )
         i := val( right(hb_ntos( (oBrw:cAlias)->F0 ), l) )
         (oBrw:cAlias)->( dbSkip(1) )
      ENDDO
      (oBrw:cAlias)->( dbGoto(nRecno) )
      i++
      If i <= val( Repl('9', l) )

         k += i / 100
         If ( lAppend := (oBrw:cAlias)->( RecAppend() ) )
            (oBrw:cAlias)->F0 := k
            (oBrw:cAlias)->( DbUnlock() )
            (oBrw:cAlias)->( DbCommit() )
            nRecno := (oBrw:cAlias)->( RecNo() )
            oBrw:nLen := Eval( oBrw:bLogicLen )

            nPos := iif( nRow < nMax , 1, 0 )

            If oBrw:nLen > oBrw:nRowCount()
               oBrw:GotoRec(nRecno, .T.)
            Else
               oBrw:GotoRec(nRecno, nRow + nPos)
            EndIf

            oBrw:nCell := 2                          // передвинуть МАРКЕР на 2 колонку

            DO EVENTS
            oBrw:PostMsg( WM_KEYDOWN, VK_RETURN, 0 ) // послать ENTER для редактирования
         EndIf

         ? "Insert=", nRecno, lAppend

      EndIf
   ENDIF

   oBrw:SetFocus()

RETURN Nil

STATIC FUNCTION RecMove(oBrw, nSkip)
   LOCAL nRec1, oRec1, nKey1, nRec2, oRec2, nKey2
   LOCAL cAls := oBrw:cAlias, lRet := .F.
   LOCAL nRow := oBrw:nRowPos, nPos
   LOCAL nMax := oBrw:nRowCount()
   LOCAL nRec := (cAls)->( RecNo() )

   IF ! MsgYesNo( "You want to "+iif( nSkip > 0, "Down", "Up" )+;
                  " record in the table ?", "Сonfirmation", .f. )
      oBrw:SetFocus()
      RETURN lRet
   ENDIF

   nRec1 := nRec
   nRec2 := nRec1
   oRec1 := (cAls)->( RecGet() )
   nKey1 := oRec1:Get('F0')
   nPos := 0
   If (cAls)->( FLock() )
      If nSkip > 0
         (cAls)->( dbSkip(1) )
         If (cAls)->( !EOF() )
            nRec2 := (cAls)->( RecNo() )
         EndIf
         nRec := nRec2
         nPos := 1
      Else
         (cAls)->( dbSkip(-1) )
         nRec2 := (cAls)->( RecNo() )
         nRec := nRec2
      EndIf
    
      If nRec1 != nRec2
         oRec2 := (cAls)->( RecGet() )
         nKey2 := oRec2:Get('F0')
         oRec2:Set('F0', nKey1)
         oRec1:Set('F0', nKey2)
         (cAls)->( RecPut(oRec1) )
         (cAls)->( dbGoto(nRec1) )
         (cAls)->( RecPut(oRec2) )
         lRet := .T.
      EndIf
      (cAls)->( dbUnLock() )
   Endif

   (cAls)->( dbGoto(nRec) )

   If lRet
      nPos := iif( nRow < nMax, nPos, 0 )
      oBrw:GotoRec(nRec, nRow + nPos)
      DO EVENTS
   EndIf

   oBrw:SetFocus()

RETURN lRet

FUNCTION RecAppend( nWhl )
   LOCAL lAdd
   DEFAULT nWhl := 10

   WHILE nWhl-- > 0
      dbAppend()
      If ( lAdd := ! NetErr() ); EXIT
      EndIf
      wApi_Sleep( 10 )
   ENDDO
   
RETURN lAdd

