/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Author: Igor Nazarov + Verchenko Andrey <verchenkoag@gmail.com>
 * Correcting the code by Sergej Kiselev <bilance@bilance.lv>
 * Revised by Grigory Filatov <gfilatov@inbox.ru>
*/
#define _HMG_OUTLOG

#include "hmg.ch" 
#include "TSBrowse.ch"

REQUEST DBFCDX

PROCEDURE Main
   LOCAL oBr, aAlias
   LOCAL g, y, x, w, h

   rddSetDefault( 'DBFCDX' )

   SET DATE FORMAT 'DD.MM.YYYY'
   SET DELETED ON
   SET AUTOPEN OFF   // запретить автооткрытие индексов вместе с базой

   SET DIALOGBOX CENTER OF PARENT

   aAlias := UseOpenBase()  // открыть базы

   DEFINE WINDOW Form_0 ;
      At 0, 0 ;
      WIDTH 600 ;
      HEIGHT 600 ;
      TITLE "(3) TsBrowse DBASE SHARED Demo" ;
      MAIN ;
      NOMAXIMIZE NOSIZE ;
      ON INIT {|| OnlyOneInstance(oBr) , oBr:SetFocus() } ;      
      ON RELEASE {|| dbCloseArea( aAlias[1] ) }

   DEFINE STATUSBAR
      STATUSITEM "Item 1" WIDTH 0   // предназначена для системных сообщений, не используем
      STATUSITEM "(3) TsBrowse - network opening of the database!" WIDTH 290 FONTCOLOR BLUE
      CLOCK
      KEYBOARD
   END STATUSBAR

   y := x := 5
   g := 2
   w := 90
   h := 30
   
   DEFINE BUTTONEX Button_Help
      Row    y
      Col    x 
      WIDTH  h
      HEIGHT h
      CAPTION "?"
      ACTION MsgAbout()
      FONTSIZE 10
      FONTBOLD .T.
   END BUTTONEX
   
   x += h + g
   DEFINE BUTTONEX Button_Metka
      Row    y
      Col    x 
      WIDTH  w
      HEIGHT h
      CAPTION "(!) Metka"
      ACTION ( iif( oBr:nCell != oBr:nColumn('METKA'), ;
                    oBr:GoPos(oBr:nRowPos, oBr:nColumn('METKA')), ), DoEvents(), ;
                    oBr:PostMsg(WM_KEYDOWN, VK_SPACE, 0), DoEvents(), ;
                    oBr:SetFocus() )
      FONTSIZE 10
      FONTBOLD .T.
   END BUTTONEX
   
   x += w + g
   DEFINE BUTTONEX Button_Clone
      Row    y
      Col    x
      WIDTH  w
      HEIGHT h
      CAPTION "(^) Clone"
      ACTION RecnoClone(oBr, .T.)
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
      ACTION RecnoInsert(oBr)
      FONTSIZE 10
      FONTBOLD .T.
   END BUTTONEX

   x += w + g
   DEFINE BUTTONEX Button_Del
      Row    y
      Col    x 
      WIDTH  w  
      HEIGHT h
      CAPTION "(-) Delete"
      ACTION RecnoDelete(oBr)
      FONTSIZE 10
      FONTBOLD .T.
   END BUTTONEX

   x += w + g
   DEFINE BUTTONEX Button_Refresh
      Row    y
      Col    x 
      WIDTH  w  
      HEIGHT h
      CAPTION "(@) Refresh"
      ACTION RecnoRefresh(oBr)
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
      ACTION Form_0.Release()
      FONTSIZE 10
      FONTBOLD .T.
   END BUTTONEX

   oBr := CreateBrowse()

   END WINDOW
   
   Form_0.Center  
   Form_0.Activate

RETURN


FUNCTION CreateBrowse()

   LOCAL oBrw, aFields
   LOCAL oCol, oMetka := oKeyData()

   Form_0.Button_Clone.Cargo := oMetka
   
   DEFINE TBROWSE oBrw ;
      AT 5 + GetProperty( "Form_0", "Button_Ins", "Height" ) + 5, 5 ;
      ALIAS "TEST" ;
      OF Form_0 ;
      WIDTH Form_0.Width - 2 * GetBorderWidth() ;
      HEIGHT Form_0.Height - GetTitleHeight() - ;
         GetProperty( "Form_0", "StatusBar", "Height" ) - 2 * GetBorderHeight() - ;
         GetProperty( "Form_0", "Button_Ins", "Height" ) - 5  ;
      GRID ;
      COLORS { CLR_BLACK, CLR_BLUE } ;
      FONT "MS Sans Serif" ;
      SIZE 8

      :SetAppendMode( .F. )      // вставка записи запрещена (в конце базы стрелкой вниз)
      :SetDeleteMode( .T., .T. ) // удаление записи разрешено

      :nFireKey    := 0
      :lNoKeyChar  := .T.
      :lNoHScroll  := .T.
      :lCellBrw    := .F.
      :lInsertMode := .T.
      :lPickerMode := .F.        // ввод формата колонки типа ДАТА сделать через цифры

   END TBROWSE

   ADD COLUMN TO TBROWSE oBrw DATA {|| (oBrw:cAlias)->( OrdKeyNo() ) } ;  
       HEADER CRLF + "NN" SIZE 40 ;
       COLORS {CLR_BLACK, WHITE} ALIGN DT_CENTER ;
       NAME NN                             

   // initial columns
   aFields := { "F2", "F1", "F3", "F4" }
   LoadFields( "oBrw", "Form_0", .F., aFields )

   // Set columns width
   oBrw:SetColSize( oBrw:nColumn( "F1" ), 90  )
   oBrw:SetColSize( oBrw:nColumn( "F2" ), 200 )
   oBrw:SetColSize( oBrw:nColumn( "F3" ), 90  )
   oBrw:SetColSize( oBrw:nColumn( "F4" ), 80  )

   // Set names for the table header
   oBrw:GetColumn('NN'):cHeading := "NN"      
   oBrw:GetColumn('F2'):cHeading := "Text"      
   oBrw:GetColumn('F1'):cHeading := "Date"      
   oBrw:GetColumn('F3'):cHeading := "Number"      
   oBrw:GetColumn('F4'):cHeading := "Logical"      

   // prepare for showing of Double cursor
   AEval( oBrw:aColumns, {| oCol | oCol:lFixLite := oCol:lEdit := TRUE, ;
                                   oCol:lOnGotFocusSelect := .T.,       ;
                                   oCol:lEmptyValToChar   := .T.,       ;
                                   oCol:nEditMove := DT_DONT_MOVE } )
       
   ADD COLUMN TO TBROWSE oBrw DATA {|| RecNo() } ;  
       HEADER "Metka" SIZE 60 ;
       COLORS {CLR_BLACK, WHITE} ALIGN DT_CENTER ;
       NAME METKA 
       
   oCol            := oBrw:GetColumn('METKA')
   oCol:cAlias     := oBrw:cAlias
   oCol:lCheckBox  := .T.
   oCol:bDecode    := {|nr| !Empty( oMetka:Get(nr) ) }

   oBrw:GetColumn('F1'):cPicture := Nil

   oBrw:nWheelLines  := 1
   oBrw:nClrLine     := COLOR_GRID
   oBrw:lNoChangeOrd := TRUE
   oBrw:lCellBrw     := TRUE
   oBrw:lNoVScroll   := TRUE
   oBrw:hBrush       := CreateSolidBrush( 242, 245, 204 )

   oBrw:nHeightCell += 10         // к высоте ячеек таблицы добавим
   oBrw:nHeightHead += 5          // к высоте шапки таблицы добавим

   // GetBox встраиваем в ячейку, задаем отступы
   oBrw:aEditCellAdjust[1] += 4  // cell_Y + :aEditCellAdjust[1]
   oBrw:aEditCellAdjust[2] += 2  // cell_X + :aEditCellAdjust[2]
   oBrw:aEditCellAdjust[3] -= 5  // cell_W + :aEditCellAdjust[3]
   oBrw:aEditCellAdjust[4] -= 8  // cell_H + :aEditCellAdjust[4]

   oBrw:SetColor( { 1 }, { RGB( 0, 12, 120 ) } )
   oBrw:SetColor( { 2 }, { RGB( 242, 245, 204 ) } )
   oBrw:SetColor( { 5 }, { RGB( 0, 0, 0 ) } )
   oBrw:SetColor( { 6 }, { { | a, b, oBr | IF( oBr:nCell == b, { RGB( 66, 255, 236 ), RGB( 111, 183, 155 ) }, ;
                              { RGB( 255, 255, 255 ), RGB( 200, 200, 200 ) } ) } } )  // cursor backcolor

   oBrw:UserKeys(VK_SPACE, {|ob,xv| 
                             Local lRet
                             If ob:nCell == ob:nColumn('METKA')
                                xv := (ob:cAlias)->( RecNo() )
                                If Empty( oMetka:Get(xv) )
                                   oMetka:Set(xv, (ob:cAlias)->( RecGet() ))
                                Else
                                   oMetka:Del(xv)
                                Endif
                                ob:DrawSelect()
                                lRet := .F.
                             ElseIf ob:nCell == ob:nColumn('F4')
                                xv := (ob:cAlias)->F4
                                If (ob:cAlias)->( RLock() )
                                   (ob:cAlias)->F4 := !xv  
                                   (ob:cAlias)->( DbUnLock() )
                                EndIf
                                ob:DrawSelect()
                                lRet := .F.
                             EndIf
                             Return lRet
                           })

   oBrw:bLDblClick := {|p1,p2,p3,ob| 
                        p1 := ob:nColumn('METKA')
                        p2 := ob:nColumn('F4')
                        If ob:nCell == p1 .or. ob:nCell == p2
                           p3 := VK_SPACE
                        Else
                           p3 := VK_RETURN
                        EndIf
                        ob:PostMsg( WM_KEYDOWN, p3, 0 )
                        Return Nil
                      }

   oBrw:ResetVScroll()       // показ вертикального скролинга таблицы
   oBrw:oHScroll:SetRange( 0, 0 ) 

   oBrw:lFooting     := .T.  // использовать подвал таблицы
   oBrw:lDrawFooters := .T.  // рисовать подвал таблицы
   oBrw:nHeightFoot  := 6    // высота строки подвала таблицы
   oBrw:DrawFooters()        // выполнить прорисовку подвала таблицы

   oBrw:nFreeze     := 1     // Заморозить столбец
   oBrw:lLockFreeze := .T.   // Избегать прорисовки курсора на замороженных столбцах

   oBrw:SetNoHoles()         // убрать дырку внизу таблицы перед подвалом

   oBrw:GoPos( 5,3 )         // передвинуть МАРКЕР на 5 строку и 3 колонку

RETURN oBrw

// копируется запись в базе и добавляется в конец базы 
// clone line and new entry in the database is added to the end of the database 
STATIC FUNCTION RecnoClone(oBrw, lMsg)
   LOCAL oRec, nRec, aRec
   LOCAL aClone, lClone := .F., nClone := 0
   LOCAL cAls := oBrw:cAlias
   LOCAL oMetka := This.Button_Clone.Cargo
   LOCAL cMsg := "Clone line (^) and insert record in a database ?"

   If Empty( aClone := oMetka:GetAll(.F.) )
      aClone := {{ (cAls)->( RecNo() ), (cAls)->( RecGet() ) }}
   EndIf

   If !Empty( lMsg )
      cMsg   := StrTran( cMsg, '^', hb_ntos(Len(aClone)) )
      lClone := MsgYesNo( cMsg, "Сonfirmation", .f. )
   EndIf

   If ! lClone
      RETURN .F.
   EndIf

// oBrw:bAddBefore := {|ob| oRec := (ob:cAlias)->( RecGet() ) }  // все поля
   oBrw:bAddAfter  := {|ob,ladd| iif( ladd, (ob:cAlias)->( RecPut(oRec) ), ) } // все поля

   FOR EACH aRec IN aClone
       nRec := aRec[1]
       oRec := aRec[2]
       // можно удалить поля (не нужные при clone) или заполнить new значениями 
       // oRec:Del('F1')
       // oRec:Del('F4')
       // oRec:Set('F4', .T.)
       // oRec:Set('F1', Date())
       If oBrw:AppendRow(.T.)
          nClone++
          oMetka:Del(nRec)              // убираем метку
          ? "Clone=", nRec, "==>" , (cAls)->( RecNo() )
       EndIf
   NEXT

   (cAls)->(DbCommit())

   oBrw:bAddBefore := Nil
   oBrw:bAddAfter  := Nil
   oBrw:nCell      := 3   

   If nClone != Len(aClone)
      MsgStop('Selected line (' + hb_ntos(Len(aClone)) + ').' + CRLF + ;
              'Insert record in a database (' + hb_ntos(nClone) + ')', 'ERROR')
   EndIf

   oBrw:SetFocus()

  RETURN .T.

// новая запись в базе добавляется в конец базы и переходим сразу к редактированию
// a new entry in the database is added to the end of the database and go directly to edit
STATIC FUNCTION RecnoInsert(oBrw)
   LOCAL lAppend, nRecno

   IF MsgYesNo( "You want to insert record in the table ?", "Сonfirmation", .f. )
      // встроенный метод для добавления записи
      lAppend := oBrw:AppendRow()
      nRecno := (oBrw:cAlias)->(RecNo())
      ? "Insert=", nRecno, lAppend
      (oBrw:cAlias)->(DbCommit())
      oBrw:ResetVScroll( .T. ) 
      oBrw:oHScroll:SetRange( 0, 0 ) 
      oBrw:nCell := 2                          // передвинуть МАРКЕР на 2 колонку
      DO EVENTS
      oBrw:PostMsg( WM_KEYDOWN, VK_RETURN, 0 ) // послать ENTER для редактирования
   ENDIF

RETURN Nil


STATIC FUNCTION RecnoDelete(oBrw)
   LOCAL lDelete, nRecno := (oBrw:cAlias)->(RecNo())
   LOCAL nRow := oBrw:nRowPos 

   oBrw:aMsg[37] := "You want to delete a record in the table ?" // Удалить запись в таблице ?
   oBrw:aMsg[38] := "Delete row in table ?"          // Удалить ряд ?
   oBrw:aMsg[39] := "Сonfirmation"                   // Подтверждение
   oBrw:aMsg[40] := "The recording is busy and can not be blocked" // Запись занята и не может быть блокирована
   oBrw:aMsg[28] := "Error!"                         // Ошибка

   // встроенный метод для удаления текущей записи
   lDelete := oBrw:DeleteRow()
   ? "Delete=", nRecno, lDelete
   (oBrw:cAlias)->(DbCommit())
   oBrw:ResetVScroll( .T. ) 
   oBrw:oHScroll:SetRange( 0, 0 ) 
   oBrw:SetFocus() 

RETURN Nil


STATIC FUNCTION RecnoRefresh(oBrw)
   LOCAL nRecno

   // если нет редактирования записи юзером то перечитаем базу
   // if there is no editing of record by the user that we will re-read the database
   If empty( oBrw:aColumns[ oBrw:nCell ]:oEdit )
      nRecno := (oBrw:cAlias)->(RecNo())
      oBrw:Reset()
      oBrw:GoToRec( nRecno )
      oBrw:SetFocus() 
      DO EVENTS
   EndIf

RETURN Nil


*----------------------------------------------------------------------------* 
STATIC FUNCTION RecGet() 
*----------------------------------------------------------------------------* 
   LOCAL oRec := oKeyData() 
 
   AEval( Array( FCount() ), {|v,n| v := n, oRec:Set( FieldName( n ), FieldGet( n ) ) } ) 
 
RETURN oRec 
 
*----------------------------------------------------------------------------* 
STATIC FUNCTION RecPut( oRec ) 
*----------------------------------------------------------------------------* 
   LOCAL nCnt := 0 
 
   AEval( oRec:GetAll(.F.), {|a,n| n := FieldPos(a[1]), nCnt += n, ; 
                              iif( n > 0, FieldPut( n, a[2] ), ) } ) 
RETURN nCnt > 0


FUNCTION UseOpenBase()
   LOCAL aStr   := {} 
   LOCAL cDbf   := GetStartUpFolder() + "\TEST" 
   LOCAL cIndx  := cDbf 
   LOCAL aAlias := {} 
   LOCAL n      := 0 
   LOCAL lDbfNo 
  
   IF ( lDbfNo := ! File( cDbf+'.dbf' ) ) 
      AAdd( aStr, { 'F1', 'D',  8, 0 } ) 
      AAdd( aStr, { 'F2', 'C', 60, 0 } ) 
      AAdd( aStr, { 'F3', 'N', 10, 2 } ) 
      AAdd( aStr, { 'F4', 'L',  1, 0 } ) 
      dbCreate( cDbf, aStr ) 
   ENDIF 
  
   IF lDbfNo .OR. !FILE(cIndx+'.cdx')  
      // если нет базы или индекса 
      USE ( cDbf ) ALIAS "TEST" EXCLUSIVE NEW 
  
      WHILE TEST->( RecCount() ) < 10 
         TEST->( dbAppend() ) 
         TEST->F1 := Date() + n++ 
         TEST->F2 := RandStr( 25 )
         TEST->F3 := n 
         TEST->F4 := ( n % 2 ) == 0 
      END 
  
      GO TOP 
      INDEX ON RECNO() TAG NN FOR !Deleted()          
      INDEX ON RECNO() TAG NO FOR  Deleted()          
      USE 
  
   ENDIF 
  
   SET AUTOPEN ON  // команда открытия индексного файла вместе с базой
  
   USE ( cDbf ) ALIAS "TEST" SHARED NEW 
   OrdSetFocus('NN') 
   GO TOP 
  
   AADD( aAlias, ALIAS() )  // запомнить базу для закрытия 

RETURN aAlias


STATIC FUNCTION RandStr( nLen )
   LOCAL cSet  := "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM"
   LOCAL cPass := ""
   LOCAL i

   If pCount() < 1
      cPass := " "
   Else
      FOR i := 1 TO nLen
         cPass += SubStr( cSet, Random( 52 ), 1 )
      NEXT
   EndIf

RETURN cPass

///////////////////////////////////////////////////////////////////////////
#define GW_HWNDFIRST	0
#define GW_HWNDLAST	1
#define GW_HWNDNEXT	2
#define GW_HWNDPREV	3
#define GW_OWNER	4
#define GW_CHILD	5

// Проверка на запуск второй/третьей копии программы
// Check to run the second/third copy of the program
FUNCTION OnlyOneInstance(oBrw)
   LOCAL cTitle, cAppTitle := Form_0.Title 
   LOCAL nH := Form_0.Height , nW := Form_0.Width
   LOCAL nI, nK, hWnd, aWindows := {} 
 
   hWnd := GetWindow( GetForegroundWindow(), GW_HWNDFIRST )
   WHILE hWnd != 0  // Loop through all the windows
      cTitle := GetWindowText( hWnd )
      IF GetWindow( hWnd, GW_OWNER ) = 0 .AND. cTitle == cAppTitle
         AADD( aWindows, { hWnd, cTitle, IsWindowVisible( hWnd ) } )
      ENDIF
      hWnd := GetWindow( hWnd, GW_HWNDNEXT )  // Get the next window
      DO EVENTS
   ENDDO

   IF LEN(aWindows) == 1
      // восстановить окна программы на экране 
       hWnd := aWindows[1,1]
       ShowWindow( hWnd, 6 )      // MINIMIZE windows
       ShowWindow( hWnd, 1 )      // SW_NORMAL windows
       BringWindowToTop( hWnd )   // A window on the foreground
      DO EVENTS
   ELSEIF LEN(aWindows) == 2
      // смена координат окна
      Form_0.Row := 0 
      Form_0.Col := 0
      // смена цвета tsbrowse
      oBrw:SetColor( { 2 }, { RGB( 255,178,178 ) } )
      oBrw:hBrush := CreateSolidBrush( 255,178,178 )
      RecnoRefresh(oBrw)
      // восстановить окна программы на экране 
      FOR nI := 1 TO LEN(aWindows)
          hWnd := aWindows[nI,1]
          ShowWindow( hWnd, 6 )      // MINIMIZE windows
          ShowWindow( hWnd, 1 )      // SW_NORMAL windows
          BringWindowToTop( hWnd )   // A window on the foreground
         DO EVENTS
      NEXT
   ELSEIF LEN(aWindows) == 3
      // смена координат окна
      Form_0.Row := 0 
      Form_0.Col := GetDesktopWidth() - nW
      // смена цвета tsbrowse
      oBrw:SetColor( { 2 }, { RGB( 159,191,236 ) } )
      oBrw:hBrush := CreateSolidBrush( 159,191,236 )
      RecnoRefresh(oBrw)
      // восстановить окна программы на экране 
      FOR nI := 1 TO LEN(aWindows)
          hWnd := aWindows[nI,1]
          ShowWindow( hWnd, 6 )      // MINIMIZE windows
          ShowWindow( hWnd, 1 )      // SW_NORMAL windows
          BringWindowToTop( hWnd )   // A window on the foreground
         DO EVENTS
      NEXT
   ELSE
      nK := LEN(aWindows)
      // смена координат окна
      Form_0.Row := GetDesktopHeight() - 20 * nK - nH
      Form_0.Col := 0 + 20 * nK
      // смена цвета tsbrowse
      oBrw:SetColor( { 2 }, { RGB( 159 - 10 * nK,191,236 ) } )
      oBrw:hBrush := CreateSolidBrush( 159 - 10 * nK,191,236 )
      RecnoRefresh(oBrw)
      // восстановить окна программы на экране 
      FOR nI := 1 TO LEN(aWindows)
          hWnd := aWindows[nI,1]
          ShowWindow( hWnd, 6 )      // MINIMIZE windows
          ShowWindow( hWnd, 1 )      // SW_NORMAL windows
          BringWindowToTop( hWnd )   // A window on the foreground
         DO EVENTS
      NEXT

   ENDIF

RETURN Nil


#define COPYRIGHT  "Author by Andrey Verchenko. Dmitrov, 2018."
#define PRG_NAME   "TsBrowse - network opening of the database !"
#define PRG_VERS   "Version 1.7"
#define PRG_RUN1   "It is necessary to start the program several times and"
#define PRG_RUN2   "you can learn the network behavior of the program !"
#define PRG_INFO1  "Many thanks for your help: Grigory Filatov <gfilatov@inbox.ru>"
#define PRG_INFO2  "Tips and tricks programmers from our forum http://clipper.borda.ru"
#define PRG_INFO3  "SergKis, Igor Nazarov and other..."

FUNCTION MsgAbout()
   RETURN MsgInfo( PadC( PRG_NAME , 70 ) + CRLF +  ;
                   PadC( PRG_VERS , 70 ) + CRLF + CRLF +  ;
                   PadC( PRG_RUN1 , 70 ) + CRLF + ;
                   PadC( PRG_RUN2 , 70 ) + CRLF + CRLF + ;
                   PadC( COPYRIGHT, 70 ) + CRLF + CRLF + ;
                   PadC( PRG_INFO1, 70 ) + CRLF + ;
                   PadC( PRG_INFO2, 70 ) + CRLF + ;
                   PadC( PRG_INFO3, 70 ) + CRLF + CRLF + ;
                   hb_compiler() + CRLF + ;
                   Version() + CRLF + ;
                   MiniGuiVersion() + CRLF + CRLF + ;
                   PadC( "This program is Freeware!", 70 ) + CRLF + ;
                   PadC( "Copying is allowed!", 70 ), "About", "ZZZ_B_ALERT", .F. )
