/*
 * MINIGUI - Harbour Win32 GUI library Demo
*/
#define _HMG_OUTLOG
#include "hmg.ch"
#include "TSBrowse.ch"
#include "Dbinfo.ch"

REQUEST DBFCDX, DBFFPT

*----------------------------------------------------------------------------*
FUNCTION Main()
*----------------------------------------------------------------------------*
   LOCAL cFont := "Arial", nSize := 12
   LOCAL aBackColor := { 215, 166, 0 }

   rddSetDefault( "DBFCDX" )

   SET CENTURY ON
   SET DATE GERMAN
   SET DELETED ON
   SET EXCLUSIVE ON
   SET EPOCH TO 2000
   SET AUTOPEN ON
   SET EXACT ON
   SET SOFTSEEK ON

   SET OOP ON

   SET NAVIGATION EXTENDED
   SET DEFAULT ICON TO "1MAIN_ICO"

   SET MSGALERT BACKCOLOR TO { 238, 249, 142 } // for HMG_Alert()

   SET FONT TO cFont, nSize

   DEFINE FONT Normal FONTNAME cFont SIZE nSize
   DEFINE FONT Bold FONTNAME cFont SIZE nSize BOLD
   DEFINE FONT Italic FONTNAME cFont SIZE nSize ITALIC
   DEFINE FONT Underline FONTNAME cFont SIZE nSize UNDERLINE

   DEFINE FONT DlgFont FONTNAME "Tahoma" SIZE 16

   DEFINE WINDOW wMain CLIENTAREA 132, 132 TITLE "DEMO" ;
         ICON "1MAIN_ICO" ;
         MAIN NOMAXIMIZE NOMINIMIZE ;
         BACKCOLOR aBackColor ;
         ON INIT _wPost( 1 )
      This.Cargo := oKeyData()

      ( This.Object ):Event( 1, {| ow | myAlert(), _wPost( 99, ow ) } )
      ( This.Object ):Event( 99, {| ow | ow:Release() } )

      DRAW ICON IN WINDOW wMain AT 0, 0 PICTURE "2MAIN_64" WIDTH 128 HEIGHT 128 ;
         COLOR aBackColor

   END WINDOW

   ACTIVATE WINDOW wMain

RETURN NIL

STATIC FUNCTION myAlert()

   LOCAL oWnd := ThisWindow.Object
   LOCAL oCar := oWnd:Cargo

   oCar:cBase := ".\Employee"

   SET MSGALERT BACKCOLOR TO BLUE STOREIN oCar:aBack_Alert
   SET MSGALERT FONTCOLOR TO YELLOW

   oCar:nOldRow_Alert := HMG_Alert_RowStart( 10 )

   USE ( oCar:cBase ) ALIAS KLI NEW SHARED

   SET WINDOW THIS TO ThisWindow.Name

   AlertOKCancel( "MessageBox with imbedded TBROWSE.", "Test Alert", /*def_btn*/, ;
      "Edit32", 32, { LGREEN, RED }, .T. /*topmost*/, {|| bInitAlertTsb( "Normal" ) } )

   SET WINDOW THIS TO

   HMG_Alert_RowStart( oCar:nOldRow_Alert )

   SET MSGALERT BACKCOLOR TO oCar:aBack_Alert[ 1 ]
   SET MSGALERT FONTCOLOR TO oCar:aBack_Alert[ 2 ]

   KLI->( dbCloseArea() )

RETURN NIL

STATIC FUNCTION bInitAlertTsb( cFnt )

   LOCAL cFont := iif( Empty( cFnt ), "DlgFont", cFnt )
   LOCAL nSize := GetFontParam( GetFontHandle( cFont ) )[ 2 ]
   LOCAL oDlu := oDlu4Font( nSize )
   LOCAL cBrw := "Table", oBrw
   LOCAL y1 := This.Say_01.Row + This.Say_01.HEIGHT + oDlu:Top * 2
   LOCAL x := oDlu:Left
   LOCAL w := oDlu:W1 * 10 + x * 2
   LOCAL h := oDlu:H1 * 12
   LOCAL y2 := This.Btn_01.Row + oDlu:Top * 2 + h
   LOCAL y, aClr := {}

   This.Topmost := .F.
   This.Cargo := oKeyData()
   This.HEIGHT := This.HEIGHT + oDlu:Top + h
   This.WIDTH := w + GetBorderWidth() * 2

   This.Btn_02.Row := y2
   This.Btn_02.Col := This.ClientWidth - oDlu:Left - This.Btn_02.WIDTH
   This.Btn_01.Row := y2
   This.Btn_01.Col := This.Btn_02.Col - oDlu:Left - This.Btn_01.WIDTH

   This.Btn_01.ACTION := {|| DoEvents(), _wPost( 3, This.Index ) }
   This.Btn_01.OnGotFocus := {|| DrawRR( RED ) }
   This.Btn_01.OnLostFocus := {|| DrawRR( .F. ) }
   This.Btn_02.OnGotFocus := {|| DrawRR( RED ) }
   This.Btn_02.OnLostFocus := {|| DrawRR( .F. ) }

   This.CENTER

   ( This.Object ):Event( 1, {| oc, ne, ob | oc := ob:Cargo, DrawRR( RED, 3, oc:nY, oc:nX, oc:nH, oc:nW, oc:cWin ), ne := ob } )
   ( This.Object ):Event( 2, {| oc, ne, ob | oc := ob:Cargo, DrawRR( .F., 3, oc:nY, oc:nX, oc:nH, oc:nW, oc:cWin ), ne := ob } )
   ( This.Object ):Event( 3, {|| wApi_Sleep( 100 ), myAlert2() } )
   ( This.Object ):Event( 4, {|| This.Btn_01.SetFocus, DoEvents(), _PushKey( VK_SPACE ) } )

   y2 -= oDlu:Top * 2
   w := This.ClientWidth - x * 2
   h := y2 - y1
   y := y1

   AAdd( aClr, { 6, {| c, n, b | c := n, iif( b:nCell == n, -CLR_BLUE, -RGB( 128, 225, 225 ) ) } } )
   AAdd( aClr, { 12, {| c, n, b | c := n, iif( b:nCell == n, -CLR_BLUE, -RGB( 128, 225, 225 ) ) } } )

   DEFINE TBROWSE &cBrw OBJ oBrw AT y, x WIDTH w HEIGHT h CELL ;
         ALIAS Alias() ;
         FONT { "Normal", "Bold", "Bold", "Italic" } ;
         BRUSH { 255, 255, 230 } ;
         ON GOTFOCUS _wSend( 1, oBrw, oBrw ) ;
         ON LOSTFOCUS _wSend( 2, oBrw, oBrw ) ;
         COLORS aClr ;
         FOOTER .T. ;
         FIXED COLSEMPTY ;
         LOADFIELDS GOTFOCUSSELECT ;
         COLNUMBER { 1, 50 } ;
         ENUMERATOR
      :Cargo := oKeyData()

      :Cargo:nY := y
      :Cargo:nX := x
      :Cargo:nH := h
      :Cargo:nW := w

      ( This.Cargo ):cBrw := cBrw // на окне в Cargo запомнили cBrw имя TsBrowse
      ( This.Cargo ):oBrw := oBrw // на окне в Cargo запомнили oBrw

      :nColOrder := 0 // убрать значок сортировки по полю
      :lNoChangeOrd := .T. // убрать сортировку по полю
      :nWheelLines := 1 // прокрутка колесом мыши
      :lNoGrayBar := .F. // показывать неактивный курсор в таблице
      :lNoLiteBar := .F. // при переключении фокуса на другое окно не убирать "легкий" Bar
                         // строка фокусная, при установленных цветах, прорисовывается,
                         // при .T. прорисовки фокусной строки нет, т.е. все строки
                         // одинаковы на фоне тсб (по установленным цветам), т.е.
                         // нет работы :DrawSelect()
      :lNoResetPos := .F. // предотвращает сброс позиции записи на gotfocus
      :lNoPopUp := .T. // избегает всплывающее меню при щелчке правой кнопкой мыши по заголовку столбца
      :lNoHScroll := .T. // отключаем показ HScroll для коротких по ширине тсб (все колонки входят в показ)
      :nCellMarginLR := 1 // отступ от линии ячейки при прижатии влево, вправо на кол-во пробелов

      :nHeightCell := oDlu:H1 + 2
      :nHeightHead := :nHeightCell

      :GetColumn( "ZIP" ):nWidth += 20
      :GetColumn( "NOTES" ):nWidth *= 0.7

      IF ( :GetAllColsWidth() - 1 ) > ( This.&( cBrw ).ClientWidth ) // колоноки не входят в показ -> HScroll
         :lAdjColumn := .T. // выравнивать последнюю колонку при прорисовке
         :lNoHScroll := .F. // добавить\вкл. ползунок горизонтальный
         :lMoreFields := ( :nColCount() > 30 ) // если колонок больше, то вкл.
                                               // метод работы, что бы не
                                               // зависала прорисовка тсб
      ELSE
         :AdjColumns() // колонки входят в окно тсб, уберем вертикальную "дырку"
                       // распределив ее значение по избранным колонкам, растянув
      ENDIF

      :bLDblClick := {| p1, p2, p3, ob | p1 := p2 := p3 := NIL, ob:PostMsg( WM_KEYDOWN, VK_RETURN, 0 ) }

      :UserKeys( VK_RETURN, {| ob | _wPost( 4, ob, ob ), .F. } )

   END TBROWSE ON END {| ob | ob:SetNoHoles(), ob:SetFocus() }

RETURN NIL

STATIC FUNCTION myAlert2()

   LOCAL cButton := This.Name
   LOCAL oParent := ThisWindow.Object
   LOCAL oCargo := ThisWindow.Cargo
   LOCAL cBrw := oCargo:cBrw
   LOCAL oBrw := This.&( cBrw ).Object
   LOCAL cAls := oBrw:cAlias
   LOCAL cDbf := ( cAls )->( dbInfo( DBI_FULLPATH ) )
   LOCAL nRec := ( cAls )->( RecNo() )
   LOCAL cFld, xVal, aVal

   SET WINDOW THIS TO ThisWindow.Name

   IF ( cAls )->( RLock() )

      oCargo:nMod := 0
      oCargo:aRec := ( cAls )->( aRecCardGet() )

      AlertOKCancel( "Press OK. " + oParent:Name + " " + cButton + " " + This.Name + " " + cBrw + " Customer # " + hb_ntos( nRec ), "Test Alert 2", /*def_btn*/, ;
         "Edit32", 32, { LGREEN, RED }, .T. /*topmost*/, {|| bInitAlertCard( oParent, "Normal" ) } )

      IF oCargo:nMod > 0
         ( cAls )->( dbGoto( nRec ) )
         FOR EACH aVal IN oCargo:aRec
            cFld := aVal[ 1 ]
            xVal := aVal[ 3 ]
            ( cAls )->( FieldPut( FieldPos( cFld ), xVal ) )
         NEXT
         ( cAls )->( dbCommit() )
      ENDIF

      ( cAls )->( dbUnlock() )

   ELSE

      MsgStop( "CARD RecNo = " + hb_ntos( nRec ) + " is not locked !", "ERROR" )

   ENDIF

   SET WINDOW THIS TO

   oBrw:SetFocus()
   oBrw:DrawSelect()

RETURN NIL

STATIC FUNCTION bInitAlertCard( oWnd, cFnt )

   LOCAL oCar := oWnd:Cargo
   LOCAL cFont := iif( Empty( cFnt ), "DlgFont", cFnt )
   LOCAL nSize := GetFontParam( GetFontHandle( cFont ) )[ 2 ]
   LOCAL oDlu := oDlu4Font( nSize )
   LOCAL cBrw := "Card", oBrw, oCol
   LOCAL y1 := This.Say_01.Row + This.Say_01.HEIGHT + oDlu:Top * 2
   LOCAL x := oDlu:Left
   LOCAL w := oDlu:W1 * 10 + x * 2
   LOCAL h := oDlu:H1 * 12
   LOCAL y2 := This.Btn_01.Row + oDlu:Top * 2 + h
   LOCAL y, aClr := {}

   This.Topmost := .F.
   This.Cargo := oKeyData()
   This.HEIGHT := This.HEIGHT + oDlu:Top + h
   This.WIDTH := w + GetBorderWidth() * 2

   This.Btn_01.OnGotFocus := {|| DrawRR( RED ) }
   This.Btn_01.OnLostFocus := {|| DrawRR( .F. ) }
   This.Btn_02.OnGotFocus := {|| DrawRR( RED ) }
   This.Btn_02.OnLostFocus := {|| DrawRR( .F. ) }

   This.Btn_02.Row := y2
   This.Btn_02.Col := This.ClientWidth - oDlu:Left - This.Btn_02.WIDTH
   This.Btn_02.ACTION := {|| oCar:nMod := 0, ThisWindow.Release }
   This.Btn_01.Row := y2
   This.Btn_01.Col := This.Btn_02.Col - oDlu:Left - This.Btn_01.WIDTH
   This.Btn_01.CAPTION := "Save"

   This.Btn_01.SetFocus
   This.Btn_02.SetFocus

   This.Btn_01.Enabled := .F.

   This.CENTER

   ( This.Object ):Event( 1, {| oc, ne, ob | oc := ob:Cargo, DrawRR( RED, 3, oc:nY, oc:nX, oc:nH, oc:nW, oc:cWin ), ne := ob } )
   ( This.Object ):Event( 2, {| oc, ne, ob | oc := ob:Cargo, DrawRR( .F., 3, oc:nY, oc:nX, oc:nH, oc:nW, oc:cWin ), ne := ob } )
   ( This.Object ):Event( 3, {|| This.Btn_01.Enabled := .T. } )

   y2 -= oDlu:Top * 2
   w := This.ClientWidth - x * 2
   h := y2 - y1
   y := y1

   AAdd( aClr, { 6, {| c, n, b | c := n, iif( b:nCell == n, -CLR_BLUE, -RGB( 128, 225, 225 ) ) } } )
   AAdd( aClr, { 12, {| c, n, b | c := n, iif( b:nCell == n, -CLR_BLUE, -RGB( 128, 225, 225 ) ) } } )

   DEFINE TBROWSE &cBrw OBJ oBrw AT y, x WIDTH w HEIGHT h CELL ;
         ALIAS oCar:aRec ;
         VALUE 1 ;
         FONT { "Normal", "Bold", "Bold" } ;
         BRUSH { 255, 255, 230 } ;
         HEADERS { "Name", "Value" } ;
         COLSIZES { oDlu:W2, oDlu:W3 } ;
         COLNAMES { "NAM", "VAL" } ;
         ON GOTFOCUS _wSend( 1, oBrw, oBrw ) ;
         ON LOSTFOCUS _wSend( 2, oBrw, oBrw ) ;
         COLORS aClr ;
         FOOTER .T. ;
         FIXED EDIT ;
         LOADFIELDS GOTFOCUSSELECT ;
         COLNUMBER { 1, 50 } ;
         COLADJUST { "VAL" }
      :Cargo := oKeyData()

      :Cargo:nY := y
      :Cargo:nX := x
      :Cargo:nH := h
      :Cargo:nW := w
      :Cargo:oCar := oCar

      ( This.Cargo ):cBrw := cBrw // на окне в Cargo запомнили cBrw имя TsBrowse

      :nColOrder := 0 // убрать значок сортировки по полю
      :lNoChangeOrd := .T. // убрать сортировку по полю
      :nWheelLines := 1 // прокрутка колесом мыши
      :lNoGrayBar := .F. // показывать неактивный курсор в таблице
      :lNoLiteBar := .F. // при переключении фокуса на другое окно не убирать "легкий" Bar
                         // строка фокусная, при установленных цветах, прорисовывается,
                         // при .T. прорисовки фокусной строки нет, т.е. все строки
                         // одинаковы на фоне тсб (по установленным цветам), т.е.
                         // нет работы :DrawSelect()
      :lNoResetPos := .F. // предотвращает сброс позиции записи на gotfocus
      :lNoPopUp := .T. // избегает всплывающее меню при щелчке правой кнопкой мыши по заголовку столбца
      :lNoHScroll := .T. // отключаем показ HScroll для коротких по ширине тсб (все колонки входят в показ)
      :nCellMarginLR := 1 // отступ от линии ячейки при прижатии влево, вправо на кол-во пробелов

      AEval( :aColumns, {| oc, nc | iif( nc > 1, oc:cPicture := NIL, ) } ) // сбросили Picture колонки

      :nFreeze := :nColumn( "NAM" )
      :nHeightCell := oDlu:H1 + 5
      :nHeightHead := :nHeightCell
      :nHeightFoot := :nHeightCell

      :aEditCellAdjust[ 1 ] := 2 // Row
      :aEditCellAdjust[ 2 ] := 1 // Col
      :aEditCellAdjust[ 4 ] := -4 // Height

      :aCheck := { StockBmp( 6 ), StockBmp( 7 ) }
      :bTSDrawCell := {| ob, ocel |
      IF ocel:nDrawType == 0 .AND. ISLOGICAL( ocel:uValue ) // Line
         ocel:uData := ""
         ocel:hBitMap := ob:aCheck[ iif( ocel:uValue, 1, 2 ) ]
      ENDIF
      RETURN NIL
      }

      oCol := :GetColumn( "VAL" )

      oCol:bPrevEdit := {| cv, ob, nc, oc |
      LOCAL xv, ct, nl, nd
      LOCAL lRet := .T.
      ct := ob:aArray[ ob:nAt ][ 4 ] // Type
      nl := ob:aArray[ ob:nAt ][ 5 ] // Len
      nd := ob:aArray[ ob:nAt ][ 6 ] // Dec
      oc:Cargo := oc:nEditWidth
      IF ct == "L"
         xv := cv
         xv := ! xv
         ob:aArray[ ob:nAt ][ 3 ] := xv
         ob:SetValue( oc, xv )
         ob:Cargo:oCar:nMod += 1
         ob:DrawSelect()
         _wSend( 3, ob )
         lRet := .F.
      ELSEIF ct == "C"
         oc:nEditWidth := oc:nWidth - 2
         oc:cEditPicture := repl( "X", nl )
      ELSEIF ct == "D"
         oc:nEditWidth := oc:nWidth - 2
         oc:cEditPicture := "@D "
      ELSEIF ct == "N"
         oc:nEditWidth := 150
         oc:cEditPicture := repl( "9", nl )
         IF nd > 0
            oc:cEditPicture += "." + repl( "9", nd )
            oc:cEditPicture := Right( oc:cEditPicture, nl )
         ENDIF
      ENDIF
      RETURN lRet
      }

      oCol:bPostEdit := {| cv, ob |
      LOCAL nc, oc, xv, ct, nl, nd
      nc := ob:nCell
      oc := ob:aColumns[ nc ]
      xv := ob:aArray[ ob:nAt ][ 3 ] // value real
      ct := ob:aArray[ ob:nAt ][ 4 ] // Type
      nl := ob:aArray[ ob:nAt ][ 5 ] // Len
      nd := ob:aArray[ ob:nAt ][ 6 ] // Dec
      oc:nEditWidth := oc:Cargo
      IF ct == "N"
         cv := Val( Transform( cv, oc:cEditPicture ) )
         ob:aArray[ ob:nAt ][ 3 ] := cv
         ob:SetValue( oc, cv )
      ELSEIF ct $ "CD"
         ob:aArray[ ob:nAt ][ 3 ] := cv
         ob:SetValue( oc, cv )
      ENDIF
      oc:cEditPicture := NIL
      ob:DrawSelect()
      ob:Cargo:oCar:nMod += 1
      _wSend( 3, ob )
      RETURN NIL
      }

      IF :nLen > :nRowCount()
         :ResetVScroll( .T. )
         :oHScroll:SetRange( 0, 0 )
      ENDIF

   END TBROWSE ON END {| ob | ob:SetNoHoles(), ob:SetFocus() }

   IF ( oCar:oBrw:nCell - 1 ) > oBrw:nRowCount()
      oBrw:GoPos( oBrw:nRowCount(), oBrw:nCell )
      WHILE oBrw:nAt < ( oCar:oBrw:nCell - 1 ) ; oBrw:GoNext()
      END
   ELSE
      oBrw:GoPos( oCar:oBrw:nCell - 1, oBrw:nCell )
   ENDIF

   ON KEY ESCAPE ACTION iif( oBrw:IsEdit, oBrw:PostMsg( WM_KEYDOWN, VK_ESCAPE ), ;
      ( oCar:nMod := 0, ThisWindow.Release ) )

RETURN NIL

*----------------------------------------------------------------------------*
PROCEDURE DrawRR( focus, nPen, t, l, b, r, cWindowName, nCurve )
*----------------------------------------------------------------------------*
   LOCAL aColor

   DEFAULT t := This.Row, l := This.Col, b := This.HEIGHT, r := This.WIDTH
   DEFAULT focus := .F., cWindowName := ThisWindow.Name, nCurve := 5
   DEFAULT nPen := 3

   IF ISARRAY( focus ) ; aColor := focus
   ELSE ; aColor := iif( focus, { 0, 120, 215 }, { 100, 100, 100 } )
   ENDIF

   DRAW ROUNDRECTANGLE IN WINDOW ( cWindowName ) ;
      AT t - 2, l - 2 TO t + b + 2, l + r + 2 ;
      ROUNDWIDTH nCurve ROUNDHEIGHT nCurve ;
      PENCOLOR aColor PENWIDTH nPen

RETURN

*----------------------------------------------------------------------------*
STATIC FUNCTION aRecCardGet()
*----------------------------------------------------------------------------*
   LOCAL aStru := dbStruct()
   LOCAL aRec := Array( Len( aStru ) ), n

   FOR n := 1 TO Len( aStru )
      aRec[ n ] := { aStru[ n ][ 1 ], FieldGet( n ), FieldGet( n ), aStru[ n ][ 2 ], aStru[ n ][ 3 ], aStru[ n ][ 4 ] }
   NEXT

RETURN aRec
