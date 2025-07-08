
#include "MiniGUI.ch"

// ---------------------------------------------------------------------------
//
// MsgTest (uValue, cTitle)
//
// Purpose:  Display dialog box with results of testing
//
// Params:  <uValue> User value to be tested
// -> Can be String, Memo, Numeric, Logic, Date,
// Array, Hash Array expression or Database
// <cTitle> Dialog title
//
// Return: Nothing
//
// ---------------------------------------------------------------------------

FUNCTION MsgTest ( uValue, cTitle )

   cTitle := iif( cTitle == NIL, "", cTitle )

   DO CASE
   CASE Select( uValue ) <> 0
      cTitle := iif( Empty( cTitle ), uValue, cTitle )
      _DbfBrowse( uValue, "Database (" + cTitle + ")" )

   CASE HB_ISNIL( uValue )
      MsgInfo( "NIL Expression", cTitle )

   CASE HB_ISSTRING( uValue ) .AND. Len( uValue ) > 255
      _MemoEdit( uValue, "Long String (" + cTitle + ")" )

   CASE HB_ISSTRING( uValue )
      IF _IsTimeStr( uValue )
         MsgInfo( "Time String Expression = " + uValue + Chr( 32 ) + Chr( 10 ) + "Lenght of Value = " + AllTrim( Str( Len(uValue ) ) ), cTitle )
      ELSE
         MsgInfo( "String Expression = " + uValue + Chr( 32 ) + Chr( 10 ) + "Lenght of Value = " + AllTrim( Str( Len(uValue ) ) ), cTitle )
      ENDIF

   CASE HB_ISNUMERIC( uValue )
      MsgInfo( "Numeric Expression = " + Str( uValue ) + Chr( 32 ) + Chr( 10 ) + "Lenght of Value = " + AllTrim( Str( Len(Str(uValue ) ) ) ), cTitle )

   CASE HB_ISBLOCK( uValue )
      MsgInfo( "CodeBlock Expression", cTitle )

   CASE HB_ISDATE( uValue )
      MsgInfo( "Date Expression = " + DToC( uValue ), cTitle )

   CASE HB_ISLOGICAL( uValue )
      MsgInfo( "Logical Expression = " + iif( uValue, "TRUE", "FALSE" ), cTitle )

   CASE HB_ISMEMO( uValue )
      _MemoEdit( uValue, "Memo (" + cTitle + ")" )

   CASE HB_ISHASH( uValue )
      _HashBrowse( uValue, "Hash Array (" + cTitle + ")" )

   CASE HB_ISARRAY( uValue )
      _ArrBrowse( uValue, "Array (" + cTitle + ")" )

   ENDCASE

RETURN ( NIL )

// -----------------------------------------------------------------------------

FUNCTION _MemoEdit ( uValue, cTitle )

   DEFINE WINDOW oDlgResult ;
         AT 0, 0 ;
         WIDTH 640 HEIGHT 400 ;
         TITLE cTitle ;
         MODAL ;
         NOSIZE

      @ 10, 10 EDITBOX oEditMemo ;
         WIDTH 610 HEIGHT 300 ;
         VALUE uValue ;
         READONLY NOTABSTOP nohscroll

      @ 330, 260 BUTTON oBtnEnd ;
         CAPTION "&End" ;
         action {|| oDlgResult.Release() }

      _DefineHotKey( "oDlgResult",, VK_ESCAPE, {|| oDlgResult.oBtnEnd.Action() } )

   END WINDOW

   oDlgResult.Center()
   oDlgResult.Activate()

RETURN ( NIL )

// -----------------------------------------------------------------------------

STATIC FUNCTION _ArrBrowse ( uArray, cTitle )

   LOCAL x, y
   LOCAL nArrRow
   LOCAL nArrCol
   LOCAL aMatrix
   LOCAL aHeader := {}
   LOCAL aColSize := {}
   LOCAL nWidth
   LOCAL nRow := Application.ROW + Int( Application.Height / 2 ) - Int( 400 / 2 )
   LOCAL nCol := Application.COL + Int( Application.Width / 2 ) - Int( 840 / 2 )

   cTitle := iif( cTitle == NIL, "Array", cTitle )
   nArrRow := Len( uArray )
   IF nArrRow == 0
      MsgInfo( "Array is empty" )
      RETURN ( NIL )
   ENDIF
   nArrCol := iif( ValType( uArray[ 1 ] ) <> "A", 1, Len( uArray[ 1 ] ) )
   IF nArrCol == 0
      MsgInfo( "Array is empty" )
      RETURN ( NIL )
   ENDIF
   ++nArrCol

   aMatrix := Array( nArrRow, nArrCol )
   FOR x = 1 TO nArrRow
      FOR y = 1 TO nArrCol
         DO CASE
         CASE y == 1
            aMatrix[ x, y ] := Str( x, 4 )
         OTHERWISE
            IF nArrCol == 2
               aMatrix[ x, y ] := _Any2Str( uArray[ x ] )
            ELSE
               aMatrix[ x, y ] := _Any2Str( uArray[ x, y - 1 ] )
            ENDIF
         ENDCASE
      NEXT y
   NEXT x

   FOR y = 1 TO nArrCol
      IF y == 1
         AAdd( aHeader, "Row" )
         AAdd( aColSize, 35 )
      ELSE
         AAdd( aHeader, "Col " + AllTrim( Str( y - 1 ) ) )
         nWidth := 0
         FOR x = 1 TO nArrRow
            nWidth := Max( nWidth, Len( aMatrix[ x, y ] ) )
         NEXT x
         AAdd( aColSize, Max( nWidth * 8, 50 ) )
      ENDIF
   NEXT x

   DEFINE WINDOW oDlgResult ;
         AT nRow, nCol ;
         WIDTH 840 ;
         HEIGHT 400 ;
         TITLE cTitle ;
         MODAL

      @ 5, 5 GRID oGrdResult ;
         WIDTH 805 ;
         HEIGHT 300 ;
         HEADERS aHeader ;
         WIDTHS aColSize ;
         ITEMS aMatrix ;
         VALUE 1 ;

         @ 320, 400 BUTTON oBtnEnd ;
         CAPTION "&End" ;
         action {|| oDlgResult.Release }

      _DefineHotKey( "oDlgResult",, VK_ESCAPE, {|| oDlgResult.oBtnEnd.Action() } )

   END WINDOW
   oDlgResult.oBtnEnd.SetFocus

   oDlgResult.Activate()

RETURN ( NIL )

// -----------------------------------------------------------------------------

STATIC FUNCTION _HashBrowse ( uArray, cTitle )

   LOCAL x
   LOCAL nArrRow
   LOCAL nArrCol := 4
   LOCAL aTemp := {}
   LOCAL aMatrix
   LOCAL aHeader := {}
   LOCAL aColSize := {}
   LOCAL nWidth
   LOCAL nRow := Application.ROW + Int( Application.Height / 2 ) - Int( 400 / 2 )
   LOCAL nCol := Application.COL + Int( Application.Width / 2 ) - Int( 840 / 2 )

   cTitle := iif( cTitle == NIL, "Hash Array", cTitle )
   nArrRow := Len( uArray )
   IF nArrRow == 0
      MsgInfo( "Hash Array is empty" )
      RETURN ( NIL )
   ENDIF

   FOR x = 1 TO nArrRow
      AAdd( aTemp, hb_HPairAt( uArray, x ) )
   NEXT x

   aMatrix := Array( nArrRow, nArrCol )
   FOR x = 1 TO nArrRow
      aMatrix[ x, 1 ] := Str( x, 4 )
      aMatrix[ x, 2 ] := _Any2Str( aTemp[ x, 1 ] )
      aMatrix[ x, 3 ] := "=>"
      aMatrix[ x, 4 ] := _Any2Str( aTemp[ x, 2 ] )
   NEXT x

   AAdd( aHeader, "Idx" )
   AAdd( aColSize, 35 )
   AAdd( aHeader, "HKeys" )
   nWidth := 0
   FOR x = 1 TO nArrRow
      nWidth := Max( nWidth, Len( aMatrix[ x, 2 ] ) )
   NEXT x
   AAdd( aColSize, Max( nWidth * 8, 50 ) )
   AAdd( aHeader, "=>" )
   AAdd( aColSize, 35 )
   AAdd( aHeader, "HValues" )
   nWidth := 0
   FOR x = 1 TO nArrRow
      nWidth := Max( nWidth, Len( aMatrix[ x, 2 ] ) )
   NEXT x
   AAdd( aColSize, Max( nWidth * 8, 50 ) )

   DEFINE WINDOW oDlgResult ;
         AT nRow, nCol ;
         WIDTH 840 ;
         HEIGHT 400 ;
         TITLE cTitle ;
         MODAL

      @ 5, 5 GRID oGrdResult ;
         WIDTH 805 ;
         HEIGHT 300 ;
         HEADERS aHeader ;
         WIDTHS aColSize ;
         ITEMS aMatrix ;
         VALUE 1

      @ 320, 400 BUTTON oBtnEnd ;
         CAPTION "&End" ;
         action {|| oDlgResult.Release }

      _DefineHotKey( "oDlgResult",, VK_ESCAPE, {|| oDlgResult.oBtnEnd.Action() } )

   END WINDOW
   oDlgResult.oBtnEnd.SetFocus

   oDlgResult.Activate()

RETURN ( NIL )

// -----------------------------------------------------------------------------

STATIC FUNCTION _DbfBrowse ( cAlias, cTitle )

   LOCAL y
   LOCAL lBrowseSync := _HMG_BROWSESYNCSTATUS
   LOCAL nDbfCol := ( cAlias )->( FCount() )
   LOCAL aHeader := {}
   LOCAL aColSize := {}
   LOCAL aField := {}
   LOCAL aForeClr := {}
   LOCAL nRow := Application.ROW + Int( Application.Height / 2 ) - Int( 400 / 2 )
   LOCAL nCol := Application.COL + Int( Application.Width / 2 ) - Int( 840 / 2 )

   cTitle := iif( cTitle == NIL, "Database", cTitle )

   if ! lBrowseSync
      _HMG_BROWSESYNCSTATUS := .T.
   ENDIF

   FOR y = 1 TO nDbfCol
      AAdd( aHeader, ( cAlias )->( FieldName( y ) ) + " (" + ( cAlias )->( FieldType( y ) ) + ")" )
      AAdd( aField, ( cAlias )->( FieldName( y ) ) )
      AAdd( aColSize, Max( ( cAlias )->( FieldSize(y ) ), Len( aHeader[ y ] ) ) * 9 )
      AAdd( aForeClr, {|| iif( ( cAlias )->( Deleted() ), { 255, 0, 0 }, { 0, 0, 0 } ) } )
   NEXT y

   DEFINE WINDOW oWndResult ;
         AT nRow, nCol ;
         WIDTH 840 ;
         HEIGHT 400 ;
         TITLE cTitle ;
         MODAL

      @ 5, 5 BROWSE oBrwResult ;
         WIDTH 805 ;
         HEIGHT 300 ;
         HEADERS aHeader ;
         WIDTHS aColSize ;
         WORKAREA &( cAlias ) ;
         FIELDS aField ;
         DYNAMICFORECOLOR aForeClr ;
         ON CHANGE {|| oWndResult.oViewRecords.VALUE := AllTrim( Str( ( cAlias )->( RecNo() ) ) ) + " of " + AllTrim( Str( ( cAlias )->( LastRec() ) ) ) }

      @ 315, 10 LABEL oLblFields VALUE "# of fields :" WIDTH 90 HEIGHT 20 RIGHTALIGN
      @ 315, 105 LABEL oViewFields VALUE AllTrim( Str( ( cAlias )->( FCount() ) ) ) WIDTH 75 HEIGHT 20 BOLD backcolor { 255, 255, 255 } clientedge

      @ 335, 10 LABEL oLblRecords VALUE "# of records :" WIDTH 90 HEIGHT 20 RIGHTALIGN
      @ 335, 105 LABEL oViewRecords VALUE AllTrim( Str( ( cAlias )->( RecNo() ) ) ) + " of " + AllTrim( Str( ( cAlias )->( LastRec() ) ) ) WIDTH 75 HEIGHT 20 BOLD backcolor { 255, 255, 255 } clientedge

      @ 335, 185 LABEL oLblDeleted VALUE "(Records displayed in RED are deleted records)" WIDTH 280 HEIGHT 20

      @ 320, 500 BUTTON oBtnView ;
         CAPTION "View" + CRLF + "Record" ;
         action {|| _ViewRecord( cAlias ) }

      @ 320, 650 BUTTON oBtnEnd ;
         CAPTION "End" ;
         action {|| oWndResult.Release }

      _DefineHotKey( "oWndResult",, VK_ESCAPE, {|| oWndResult.oBtnEnd.Action() } )

   END WINDOW
   oWndResult.oBrwResult.SetFocus

   oWndResult.Activate()

   _HMG_BROWSESYNCSTATUS := lBrowseSync

RETURN ( NIL )

// -----------------------------------------------------------------------------

STATIC FUNCTION _IsTimeStr ( uValue )

   LOCAL z
   LOCAL nLen
   LOCAL lReturn := .T.

   uValue := AllTrim( uValue )
   nLen := Len( uValue )
   IF nLen == 0
      RETURN ( .F. )
   ENDIF

   FOR z = 1 TO nLen
      IF SubStr( uValue, z, 1 ) <> ":"
         IF ( Asc( SubStr( uValue, z, 1 ) ) > 57 .OR. Asc( SubStr( uValue, z, 1 ) ) < 48 )
            lReturn := .F.
            EXIT
         ENDIF
      ENDIF
   NEXT z

RETURN ( lReturn )

// -----------------------------------------------------------------------------

STATIC FUNCTION _Any2Str ( xValue, nWidth )

   LOCAL lNumeric := .F.

   DO CASE
   CASE ValType( xValue ) == "C"
      xValue := AllTrim( xValue )
   CASE ValType( xValue ) == "N"
      lNumeric := .T.
      IF xValue - Int( xValue ) == 0
         xValue := AllTrim( Str( xValue, Len( Str(xValue ) ), 0 ) )
      ELSE
         xValue := AllTrim( Str( xValue ) )
      ENDIF
   CASE ValType( xValue ) == "D"
      xValue := DToC( xValue )
   CASE ValType( xValue ) == "L"
      xValue := iif( xValue, ".T.", ".F." )
   CASE ValType( xValue ) == "M"
      xValue := "Memo"
   CASE ValType( xValue ) == "A"
      xValue := "Array"
   CASE ValType( xValue ) == "H"
      xValue := "Hash}"
   CASE ValType( xValue ) == "B"
      xValue := "Block"
   OTHERWISE
      xValue := " "
   ENDCASE

   IF nWidth <> NIL
      IF Len( xValue ) < nWidth
         IF lNumeric
            xValue := PadL( xValue, nWidth )
         ELSE
            xValue := PadR( xValue, nWidth )
         ENDIF
      ENDIF
   ENDIF

RETURN ( xValue )

// ---------------------------------------------------------------------------

STATIC FUNCTION _ViewRecord ( cAlias, cTitle )

   LOCAL x
   LOCAL nTotFields := ( FCount() )
   LOCAL aFieldList := {}

   cAlias := iif( cAlias == NIL, Alias( Select() ), cAlias )
   cTitle := iif( cTitle == NIL, cAlias + ".DBF / Record " + AllTrim( Str( (cAlias )->(RecNo() ) ) ) + iif( ( cAlias )->( Deleted() ), " / Deleted", "" ), cTitle )

   FOR x = 1 TO nTotFields
      AAdd( aFieldList, { ( cAlias )->( FieldName( x ) ), ( cAlias )->( FieldGet( x ) ) } )
   NEXT x
   _ArrBrowse( aFieldList, cTitle )

RETURN ( NIL )
