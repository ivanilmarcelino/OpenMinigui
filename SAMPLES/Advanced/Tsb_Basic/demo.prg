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

STATIC nStaticTime

PROCEDURE Main
   LOCAL oBr, aAlias

   rddSetDefault( 'DBFCDX' )

   SET DATE FORMAT 'DD.MM.YYYY'
   SET DELETED ON
   SET AUTOPEN OFF   // запретить автооткрытие индексов вместе с базой

   SET DIALOGBOX CENTER OF PARENT

   aAlias := UseOpenBase()  // открыть базы

   nStaticTime := SECONDS() // включить время для показа таймера

   DEFINE WINDOW Form_0 ;
      At 0, 0 ;
      WIDTH 600 ;
      HEIGHT 600 ;
      TITLE "(1) TsBrowse DBASE SHARED Demo" ;
      MAIN ;
      NOMAXIMIZE NOSIZE ;
      ON INIT {|| OnlyOneInstance(oBr) , oBr:SetFocus() } ;      
      ON RELEASE {|| dbCloseArea( aAlias[1] ) }

   DEFINE STATUSBAR
      STATUSITEM "Item 1" WIDTH 0   // предназначена для системных сообщений, не используем
      STATUSITEM "(1) TsBrowse - network opening of the database!" WIDTH 290 FONTCOLOR BLUE
      STATUSITEM " 00:00:00"
      KEYBOARD
   END STATUSBAR

   DEFINE BUTTONEX Button_Ins
      Row    5
      Col    5
      WIDTH  110
      HEIGHT 30
      CAPTION "(+) Insert"
      ACTION RecnoInsert(oBr)
      FONTSIZE 10
      FONTBOLD .T.
   END BUTTONEX

   DEFINE BUTTONEX Button_Del
      Row    5
      Col    110 + 5*2
      WIDTH  110  
      HEIGHT 30
      CAPTION "(-) Delete"
      ACTION RecnoDelete(oBr)
      FONTSIZE 10
      FONTBOLD .T.
   END BUTTONEX

   DEFINE BUTTONEX Button_Refresh
      Row    5
      Col    110*2 + 5*3
      WIDTH  110  
      HEIGHT 30
      CAPTION "(@) Refresh"
      ACTION RecnoRefresh(oBr, .f.)
      FONTSIZE 10
      FONTBOLD .T.
   END BUTTONEX

   DEFINE BUTTONEX Button_Help
      Row    5
      Col    110*3 + 5*4
      WIDTH  110
      HEIGHT 30
      CAPTION "(?) Help"
      ACTION MsgAbout()
      FONTSIZE 10
      FONTBOLD .T.
   END BUTTONEX

   DEFINE BUTTONEX Button_Exit
      Row    5
      Col    110*4 + 5*5
      WIDTH  110
      HEIGHT 30
      CAPTION "Exit"
      ACTION Form_0.Release()
      FONTSIZE 10
      FONTBOLD .T.
   END BUTTONEX

   oBr := CreateBrowse()

   // включить таймер 1 раз в полминуты вызов функции
   DEFINE TIMER Timer_1 INTERVAL 30 * 1000 ACTION RecnoRefresh(oBr, .t.)
   // включить таймер 2 для отображения времени Timer_1 каждую секунду
   DEFINE TIMER Timer_2 INTERVAL 1000 ACTION Timer1Show()

   END WINDOW

   DoMethod( "Form_0", "Center" )
   DoMethod( "Form_0", "Activate" )

RETURN


FUNCTION CreateBrowse()

   LOCAL oBrw, aFields

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

      :lNoHScroll  := .T.        // показ горизонтального скролинга
      :lCellBrw    := .F.
      :lInsertMode := .T.        // флаг для переключения режима Вставки при редактировании
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
   oBrw:aColumns[1]:cHeading := "NN"      
   oBrw:aColumns[2]:cHeading := "Text"      
   oBrw:aColumns[3]:cHeading := "Date"      
   oBrw:aColumns[4]:cHeading := "Number"      
   oBrw:aColumns[5]:cHeading := "Logical"      

   oBrw:GetColumn('F1'):cPicture := Nil     // пустые поля отображать как пробел

   oBrw:nWheelLines  := 1
   oBrw:nClrLine     := COLOR_GRID          // цвет линий между ячейками таблицы
   oBrw:lNoChangeOrd := TRUE                // убрать сортировку по полю
   oBrw:nColOrder    := 0                   // убрать значок сортировки по полю
   oBrw:lCellBrw     := TRUE
   oBrw:lNoVScroll   := TRUE                // отключить показ горизонтального скролинга
   oBrw:hBrush       := CreateSolidBrush( 242, 245, 204 )   // цвет фона под таблицей

   // prepare for showing of Double cursor
   AEval( oBrw:aColumns, {| oCol | oCol:lFixLite := oCol:lEdit := TRUE, ;
                                   oCol:lOnGotFocusSelect := .T.,       ;
                                   oCol:lEmptyValToChar   := .T. } )
          // oCol:lOnGotFocusSelect := .T. - включат засинение данных при получении фокуса 
          //   GetBox-ом и сбрасывает, очищает поле при нажатии первого символа 
          // oCol:lEmptyValToChar := .T. - при .T. переводит empty(...) значение поля в ""

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

   oBrw:ResetVScroll()       // показ вертикального скролинга таблицы

   oBrw:lFooting     := .T.  // использовать подвал таблицы
   oBrw:lDrawFooters := .T.  // рисовать подвал таблицы
   oBrw:nHeightFoot  := 6    // высота строки подвала таблицы
   oBrw:DrawFooters()        // выполнить прорисовку подвала таблицы

   oBrw:nFreeze     := 1     // Заморозить столбец
   oBrw:lLockFreeze := .T.   // Избегать прорисовки курсора на замороженных столбцах

   oBrw:SetNoHoles()         // убрать дырку внизу таблицы перед подвалом

   oBrw:GoPos( 5,3 )         // передвинуть МАРКЕР на 5 строку и 3 колонку

RETURN oBrw

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

      // обязательно перечитать состояние вертикального скролинга
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

   // обязательно перечитать состояние вертикального скролинга
   oBrw:ResetVScroll( .T. ) 
   oBrw:oHScroll:SetRange( 0, 0 ) 

   oBrw:SetFocus() 
   DO EVENTS

RETURN Nil


STATIC FUNCTION RecnoRefresh(oBrw, ltimer)
   LOCAL nRecno, cForm := oBrw:cParentWnd

   Default ltimer := .f.
   // если нет редактирования записи юзером то перечитаем базу
   // if there is no editing of record by the user that we will re-read the database
   If empty( oBrw:aColumns[ oBrw:nCell ]:oEdit )
      SetProperty( cForm, "StatusBar" , "Item" , 3, "Re-read database!" )  // показ обновления
      SysRefresh() 
      oBrw:nLen := ( oBrw:cAlias )->( Eval( oBrw:bLogicLen ) ) 
      oBrw:Upstable() 
      oBrw:Refresh(.T., .T.) 
      oBrw:SetFocus() 
      DO EVENTS
   EndIf
   If ltimer
      nStaticTime := SECONDS()  // обновить время
   EndIf

RETURN Nil

    
FUNCTION Timer1Show()  // показ таймера на форме 
    LOCAL cTime

    cTime := " " + SECTOTIME( GetProperty( ThisWindow.Name, "Timer_1", "Value" ) / 1000 - (SECONDS() - nStaticTime) )
    SetProperty ( ThisWindow.Name, "StatusBar" , "Item" , 3, cTime )

RETURN NIL


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
      RecnoRefresh(oBrw,.f.)
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
#define PRG_VERS   "Version 1.9"
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

