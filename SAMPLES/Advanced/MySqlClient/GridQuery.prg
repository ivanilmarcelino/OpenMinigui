#include "minigui.ch"

#define PROGRAM 'GridQuery'
#define COMPILE(c_b) &("{|e|" + c_b + "}")

#define MsgAlert( c ) MsgEXCLAMATION( c, PROGRAM, , .f. )
#define MsgInfo( c ) MsgInfo( c, PROGRAM, , .f. )
#define MsgYesNo( c, t ) MsgYesNo( c, t, , , .f. )

DECLARE WINDOW Form_Query

#define Q_FILE  1   // For the aQuery_ array
#define Q_DESC  2
#define Q_EXPR  3

MEMVAR lXPTheme
MEMVAR aEditcontrols

PROCEDURE GridQuery( WindowName, GridName, aGrid )

   LOCAL cField, cComp, cType := "", i
   LOCAL cChar := "", nNum := 0, dDate := CToD( "" ), nLog := 1
   LOCAL cExpr := "", nColCount
   LOCAL aQuery_ := { "", "", "" }
   LOCAL aUndo_ := {}
   LOCAL aFlds_ := {}

   LOCAL aComp_ := { "== Equal", ;
      "<> Not equal", ;
      "<  Less than", ;
      ">  Greater than", ;
      "<= Less or equal", ;
      ">= Greater or equal", ;
      "$  Contains", ;
      "!$ Does not Contain", ;
      "() Is contained in", ;
      '"" Is empty (blank)', ;
      "B  Begins With", ;
      "E  Ends With" }

   PRIVATE lXPTheme := IsThemed()
   PRIVATE aEditcontrols := {}
   aGrid := iif( aGrid != NIL, aGrid, _HMG_aControlMiscData1[ GetProperty ( WindowName, GridName, "Index" ), 4 ] )
   nColCount := GetProperty( windowname, gridname, "ColumnCount" )
   FOR i := 1 TO nColCount
      AAdd( aFlds_, GetProperty( windowname, gridname, "header", i ) )
   NEXT
   cField := aFlds_[ 1 ]
   cComp := aComp_[ 7 ]

   DEFINE WINDOW Form_Query ;
         AT 0, 0 WIDTH 570 HEIGHT 340 + IF( lXPTheme, 8, 0 ) ;
         TITLE 'Query' ;
         ICON "FILTER" ;
         MODAL ;
         ON INIT ( Form_Query.List_1.Setfocus, cType := GetType( cField, aFlds_, @cChar, iif(Len(aGrid) > 0, aGrid[ 1 ], {} ) ), ;
            Form_Query.Text_1.Enabled := ( cType == "C" ), ;
            Form_Query.Text_2.Enabled := ( cType == "N" ), ;
            Form_Query.Date_1.Enabled := ( cType == "D" ), ;
            Form_Query.Combo_1.Enabled := ( cType == "L" ) ) ;
         FONT "MS Sans Serif" ;
         SIZE 8

      DEFINE FRAME Frame_1
         ROW 10
         COL 260
         WIDTH 290
         HEIGHT 135
         CAPTION 'Value'
         OPAQUE .T.
      END FRAME

      DEFINE LABEL Label_1
         ROW 30
         COL 270
         WIDTH 60
         HEIGHT 20
         VALUE 'Character' + ":"
         VISIBLE .T.
      END LABEL

      DEFINE LABEL Label_2
         ROW 60
         COL 270
         WIDTH 60
         HEIGHT 20
         VALUE 'Numeric' + ":"
         VISIBLE .T.
      END LABEL

      DEFINE LABEL Label_3
         ROW 90
         COL 270
         WIDTH 60
         HEIGHT 20
         VALUE 'Date' + ":"
         VISIBLE .T.
      END LABEL

      DEFINE LABEL Label_4
         ROW 120
         COL 270
         WIDTH 60
         HEIGHT 20
         VALUE 'Logical' + ":"
         VISIBLE .T.
      END LABEL

      DEFINE LABEL Label_5
         ROW 6
         COL 12
         WIDTH 80
         HEIGHT 16
         VALUE 'Field'
         VISIBLE .T.
      END LABEL

      DEFINE LABEL Label_6
         ROW 6
         COL 134
         WIDTH 120
         HEIGHT 16
         VALUE 'Comparison'
         VISIBLE .T.
      END LABEL

      DEFINE LISTBOX List_1
         ROW 20
         COL 10
         WIDTH 114
         HEIGHT 130
         ITEMS aFlds_
         VALUE 1
         ONCHANGE ( cField := aFlds_[ This.Value ], cType := GetType( cField, aFlds_, @cChar, aGrid[ 2 ] ), ;
            Form_Query.Text_1.Enabled := ( cType == "C" ), ;
            Form_Query.Text_2.Enabled := ( cType == "N" ), ;
            Form_Query.Date_1.Enabled := ( cType == "D" ), ;
            Form_Query.Combo_1.Enabled := ( cType == "L" ) )
         ONDBLCLICK Form_Query.Button_1.ONCLICK
         TABSTOP .T.
         VISIBLE .T.
         SORT .F.
         MULTISELECT .F.
      END LISTBOX

      DEFINE LISTBOX List_2
         ROW 20
         COL 132
         WIDTH 118
         HEIGHT 160
         ITEMS aComp_
         VALUE 7
         ONCHANGE cComp := aComp_[ This.Value ]
         ONLOSTFOCUS IF( CheckComp( cType, cComp ), , Form_Query.List_2.Setfocus )
         ONDBLCLICK Form_Query.Button_1.ONCLICK
         TABSTOP .T.
         VISIBLE .T.
         SORT .F.
         MULTISELECT .F.
      END LISTBOX

      DEFINE EDITBOX Edit_1
         ROW 190
         COL 10
         WIDTH 240
         HEIGHT 100
         VALUE ""
         ONCHANGE ( cExpr := This.VALUE, ;
            Form_Query.Button_2.Enabled := ( ! Empty( cExpr ) ), ;
            Form_Query.Button_8.Enabled := ( ! Empty( cExpr ) ), ;
            Form_Query.Button_10.Enabled := ( ! Empty( cExpr ) ) )
         ONGOTFOCUS NIL
         ONLOSTFOCUS NIL
         FONTBOLD .T.
         TABSTOP .T.
         VISIBLE .T.
      END EDITBOX

      DEFINE LABEL Label_7
         ROW 175
         COL 12
         WIDTH 100
         HEIGHT 16
         VALUE 'Query expression' + ":"
         VISIBLE .T.
      END LABEL

      DEFINE TEXTBOX Text_1
         ROW 26
         COL 340
         WIDTH 200
         HEIGHT 24
         ONCHANGE cChar := Upper( This.Value )
         ONGOTFOCUS Form_Query.Text_1.Enabled := ( cType == "C" )
         ONENTER ( Form_Query.Button_1.ONCLICK, Form_Query.Button_8.Setfocus )
         FONTBOLD .T.
         TABSTOP .T.
         VISIBLE .T.
         VALUE AllTrim( cChar )
      END TEXTBOX

      DEFINE TEXTBOX Text_2
         ROW 56
         COL 340
         WIDTH 200
         HEIGHT 24
         NUMERIC .T.
         INPUTMASK "9999999.99"
         RIGHTALIGN .T.
         MAXLENGTH 10
         ONCHANGE nNum := This.VALUE
         ONGOTFOCUS Form_Query.Text_2.Enabled := ( cType == "N" )
         ONENTER ( Form_Query.Button_1.ONCLICK, Form_Query.Button_8.Setfocus )
         FONTBOLD .T.
         TABSTOP .T.
         VISIBLE .T.
         VALUE nNum
      END TEXTBOX

      DEFINE DATEPICKER Date_1
         ROW 86
         COL 340
         WIDTH 110
         HEIGHT 24
         VALUE dDate
         SHOWNONE .T.
         UPDOWN .T.
         ONCHANGE dDate := This.VALUE
         ONGOTFOCUS Form_Query.Date_1.Enabled := ( cType == "D" )
         FONTBOLD .T.
         TABSTOP .T.
         VISIBLE .T.
      END DATEPICKER

      DEFINE COMBOBOX Combo_1
         ROW 116
         COL 340
         WIDTH 110
         HEIGHT 60
         ITEMS { "True (.T.)", "False (.F.)" }
         VALUE nLog
         ONCHANGE nLog := This.VALUE
         ONGOTFOCUS Form_Query.Combo_1.Enabled := ( cType == "L" )
         ONENTER ( Form_Query.Button_1.ONCLICK, Form_Query.Button_8.Setfocus )
         FONTBOLD .T.
         TABSTOP .T.
         VISIBLE .T.
      END COMBOBOX

      DEFINE BUTTON Button_1
         ROW 156
         COL 260
         WIDTH 136
         HEIGHT 24
         CAPTION 'A&dd'
         ACTION IF( CheckComp( cType, cComp ), ( AddExpr( @cExpr, aUndo_, cField, cComp, ;
            iif( cType == "C", cChar, iif( cType == "N", nNum, ;
            iif( cType == "D", dDate, ( nLog == 1 ) ) ) ), aFlds_ ), ;
            Form_Query.Button_2.Enabled := ( Len( aUndo_ ) > 0 ), ;
            Form_Query.Button_8.Enabled := ( ! Empty( cExpr ) ), ;
            Form_Query.Button_10.Enabled := ( ! Empty( cExpr ) ) ), Form_Query.List_2.Setfocus )
         ONLOSTFOCUS Form_Query.Button_2.Enabled := ( Len( aUndo_ ) > 0 )
         TABSTOP .T.
         VISIBLE .T.
      END BUTTON

      DEFINE BUTTON Button_2
         ROW 156
         COL 414
         WIDTH 136
         HEIGHT 24
         CAPTION '&Undo'
         ACTION ( Undo( @cExpr, aUndo_ ), ;
            Form_Query.Button_2.Enabled := ( Len( aUndo_ ) > 0 ), ;
            Form_Query.Button_8.Enabled := ( ! Empty( cExpr ) ), ;
            Form_Query.Button_10.Enabled := ( ! Empty( cExpr ) ) )
         ONLOSTFOCUS Form_Query.Button_2.Enabled := ( Len( aUndo_ ) > 0 )
         TABSTOP .T.
         VISIBLE .T.
      END BUTTON

      DEFINE BUTTON Button_3
         ROW 196
         COL 260
         WIDTH 44
         HEIGHT 24
         CAPTION 'and'
         ACTION AddText( @cExpr, aUndo_, " .and. " )
         TABSTOP .T.
         VISIBLE .T.
      END BUTTON

      DEFINE BUTTON Button_4
         ROW 196
         COL 321
         WIDTH 44
         HEIGHT 24
         CAPTION 'or'
         ACTION AddText( @cExpr, aUndo_, " .or. " )
         TABSTOP .T.
         VISIBLE .T.
      END BUTTON

      DEFINE BUTTON Button_5
         ROW 196
         COL 383
         WIDTH 44
         HEIGHT 24
         CAPTION 'not'
         ACTION AddText( @cExpr, aUndo_, " .not. " )
         TABSTOP .T.
         VISIBLE .T.
      END BUTTON

      DEFINE BUTTON Button_6
         ROW 196
         COL 444
         WIDTH 44
         HEIGHT 24
         CAPTION "("
         ACTION AddText( @cExpr, aUndo_, "( " )
         TABSTOP .T.
         VISIBLE .T.
      END BUTTON

      DEFINE BUTTON Button_7
         ROW 196
         COL 505
         WIDTH 44
         HEIGHT 24
         CAPTION ")"
         ACTION AddText( @cExpr, aUndo_, " ) " )
         TABSTOP .T.
         VISIBLE .T.
      END BUTTON

      DEFINE BUTTON Button_8
         ROW 236
         COL 260
         WIDTH 62
         HEIGHT 24
         CAPTION '&Apply'
         ACTION IF( RunQuery( cExpr, WindowName, GridName, aGrid ), Form_Query.Button_9.ONCLICK, )
         ONLOSTFOCUS Form_Query.Button_8.Enabled := ( ! Empty( cExpr ) )
         TABSTOP .T.
         VISIBLE .T.
      END BUTTON

      DEFINE BUTTON Button_9
         ROW 236
         COL 336
         WIDTH 62
         HEIGHT 24
         CAPTION '&Close'
         ACTION ( SetProperty( WindowName, GridName, "Value", 1 ), ;
            DoMethod( WindowName, GridName, 'Refresh' ), ;
            Form_Query.RELEASE, ;
            DoMethod( WindowName, 'Setfocus' ) )
         TABSTOP .T.
         VISIBLE .T.
      END BUTTON

      DEFINE BUTTON Button_10
         ROW 236
         COL 412
         WIDTH 62
         HEIGHT 24
         CAPTION '&Save'
         ACTION SaveQuery( cExpr, aQuery_ )
         ONLOSTFOCUS Form_Query.Button_10.Enabled := ( ! Empty( cExpr ) )
         TABSTOP .T.
         VISIBLE .T.
      END BUTTON

      DEFINE BUTTON Button_11
         ROW 236
         COL 488
         WIDTH 62
         HEIGHT 24
         CAPTION '&Load'
         ACTION iif( LoadQuery( @cExpr, aQuery_ ), ( aUndo_ := {}, ;
            Form_Query.Button_8.Enabled := ( ! Empty( cExpr ) ), ;
            Form_Query.Button_10.Enabled := ( ! Empty( cExpr ) ) ), )
         TABSTOP .T.
         VISIBLE .T.
      END BUTTON

      ON KEY ESCAPE ACTION IF( CheckComp( cType, cComp ), Form_Query.Button_9.ONCLICK, Form_Query.List_2.Setfocus )


   END WINDOW

   Form_Query.Text_1.Enabled := .F.
   Form_Query.Text_2.Enabled := .F.
   Form_Query.Date_1.Enabled := .F.
   Form_Query.Combo_1.Enabled := .F.
   Form_Query.Button_2.Enabled := .F.
   Form_Query.Button_8.Enabled := .F.
   Form_Query.Button_10.Enabled := .F.
   Form_Query.Button_11.Enabled := .F.

   CENTER WINDOW Form_Query

   ACTIVATE WINDOW Form_Query

RETURN



FUNCTION AddText( cExpr, aUndo_, cText )

   cExpr += cText
   AAdd( aUndo_, cText )
   Form_Query.Edit_1.VALUE := cExpr
   DO EVENTS

Return( NIL )


FUNCTION GetType( cField, aFlds_, cChar, aValues )

   LOCAL cType := "C", n

   n := AScan( aFlds_, cField )
   IF Len( aValues ) > 0
      cType := ValType( aValues[ n ] )
      IF cType == "M"
         cType := "C"
      ELSEIF cType == "C"
         cChar := PadR( cChar, Len( aValues[ n ] ) )
      ENDIF
   ENDIF

Return( cType )


FUNCTION CheckComp( cType, cComp )

   LOCAL lOk := .T.
   LOCAL cTemp := Left( cComp, 2 )

   DO CASE
   CASE cType $ "ND"
      IF cTemp $ "$ ()"
         lOk := .F.
      ENDIF
   CASE cType == "L"
      IF cTemp <> "==" .AND. cTemp <> "<>" .AND. cTemp <> '""'
         lOk := .F.
      ENDIF
   OTHERWISE // All are Ok for character variables
      lOk := .T.
   ENDCASE

   if ! lOk
      MsgAlert( "Invalid comparison for selected data type." )
   ENDIF

Return( lOk )


FUNCTION AddExpr( cExpr, aUndo_, cField, cComp, uVal, aFlds_ )

   LOCAL cVT, cTemp := '', nPos
   LOCAL xFieldVal := uVal
   nPos := AScan( aFlds_, cField )
   cComp := AllTrim( Left( cComp, 2 ) )
   cField := 'e[' + hb_ntos( nPos ) + ']'
   IF cComp == '()'
      cTemp := '"' + RTrim( uVal ) + '" $ ' + cField
   ELSEIF cComp == '""'
      cTemp := "empty(" + cField + ")"
   ELSE
      cTemp := cField + ' ' + cComp + ' '
      cVT := ValType( uVal )
      DO CASE
      CASE cComp == "B"
         cTemp := "SUBSTR( Upper(" + cField + "),1," + hb_ntos( Len( AllTrim( uVal ) ) ) + ') =  "' + Upper( AllTrim( uVal ) ) + '"'
      CASE cComp == "E"
         cTemp := "SUBSTR( Upper(" + cField + ")," + hb_ntos( Len( AllTrim( uVal ) ) * -1 ) + ',' + hb_ntos( Len( AllTrim( uVal ) ) ) + ') =  "' + Upper( AllTrim( uVal ) ) + '"'
      CASE cVT == 'C' .AND. '!$' $ cComp
         cTemp := '!"' + PadR( uVal, Len( xFieldVal ) ) + '" $ Upper(' + cField + ')'
      CASE cVT == 'C' .AND. '$' $ cComp
         cTemp := '"' + PadR( uVal, Len( xFieldVal ) ) + '" ' + cComp + ' Upper(' + cField + ')'
      CASE cVT == 'C'
         cTemp += '"' + PadR( uVal, Len( xFieldVal ) ) + '"'
      CASE cVT == 'N'
         cTemp += LTrim( Str( uVal ) )
      CASE cVT == 'D'
         cTemp += 'ctod("' + DToC( uVal ) + '")'
      CASE cVT == "L"
         cTemp += iif( uVal, '.T.', '.F.' )
      ENDCASE
   ENDIF

   cTemp += " "

   AddText( @cExpr, aUndo_, cTemp )

  /*
  if cVT == '()'
     cTemp := 'e[' +hb_ntos(nPos)+'] $ "'+uVal+'"'
  elseif cVT == '""'
    cTemp := "empty(e[" + hb_ntos(nPos) + "])"
  else
    cTemp := cField + ' ' + cVT + ' '
    cVT := valtype(uVal)
    do case
    case cVT == 'C'
        cTemp += '"' + padr(uVal, len(xFieldVal)) + '"'
      case cVT == 'N'
        cTemp += ltrim(str(uVal))
      case cVT == 'D'
        cTemp += 'ctod("' + dtoc(uVal) + '")'
      case cVT == "L"
        cTemp += iif(uVal, '.T.', '.F.')
    endcase
  endif
    *cTemp += alltrim(left(cComp, 2)) + ' Upper(e['+hb_ntos(nPos)+'])'

  cTemp += " "

  AddText(@cExpr, aUndo_, cTemp)
   */

Return( NIL )


FUNCTION Undo( cExpr, aUndo_ )

   LOCAL l := Len( aUndo_ )
   LOCAL x, cTemp := cExpr

   IF ( x := RAt( aUndo_[ l ], cTemp ) ) > 0
      cExpr := InsDel( cTemp, x, Len( aUndo_[ l ] ), "" )
      Form_Query.Edit_1.VALUE := cExpr
      DO EVENTS
   ENDIF

   ASize( aUndo_, l - 1 )

Return( NIL )


FUNCTION RunQuery( cExpr, cWin, GridName, aGrid )

   LOCAL oError, Filterblock
   TRY
      IF _IsControlDefined( GridName, cWin )
         Filterblock := COMPILE( cExpr )
         DoMethod( cWin, GridName, "deleteallitems" )
         AEval( aGrid, {| e | iif( Eval( FilterBlock, e ), DoMethod( cWin, GridName, "AddItem", e ), "" ) } )
      ENDIF
   CATCH oError
      ErrorSys( oError )
   FINALLY
      DoMethod( cWin, GridName, "Refresh" )
   END

Return( .T. )


FUNCTION SaveQuery( /*cExpr, aQuery_*/ )
Return( NIL )


FUNCTION LoadQuery( /*cExpr, aQuery_*/ )
Return( NIL/*lLoaded*/ )


STATIC FUNCTION NotDBQ( cQFile )

   LOCAL lNot := .F.

   IF FieldPos( "FILENAME" ) == 0 .OR. ;
         FieldPos( "DESC" ) == 0 .OR. ;
         FieldPos( "EXPR" ) == 0
      lNot := .T.
      MsgAlert( cQFile + " " + 'is not a DataBase query file' + "." )
   ENDIF

Return( lNot )


STATIC FUNCTION LoadIt( aQuery_ )

   LOCAL lLoaded := .T.
   IF lLoaded
      aQuery_[ Q_FILE ] := AllTrim( QFile->FILENAME )
      aQuery_[ Q_DESC ] := AllTrim( QFile->DESC )
      aQuery_[ Q_EXPR ] := AllTrim( QFile->EXPR ) + " "
   ENDIF

Return( lLoaded )


FUNCTION DelRec()

   LOCAL lDel := .F.
   LOCAL cMsg, cTitle

   IF Deleted()
      cMsg := 'Are you sure you wish to recall this record?'
      cTitle := 'Recall'
   ELSE
      cMsg := 'Are you sure you wish to delete this record?'
      cTitle := 'Delete'
   ENDIF

   IF MsgYesNo( cMsg, cTitle )
      IF Deleted()
         dbRecall()
      ELSE
         dbDelete()
      ENDIF
      lDel := .T.
   ENDIF

Return( lDel )


STATIC FUNCTION QueryError( e )

   LOCAL cMsg := 'Syntax error in Query expression!'

   IF ValType( e:description ) == "C"
      cMsg := e:description
      cMsg += if( ! Empty( e:filename ), ": " + e:filename, ;
         if( ! Empty( e:operation ), ": " + e:operation, "" ) )
   ENDIF
   MsgSTOP( cMsg )
   dbCloseAll()

RETURN NIL

STATIC PROCEDURE Cr_QFile( cQFile )

   LOCAL aArray_ := { { "FILENAME", "C", 12, 0 }, ;
      { "DESC", "C", 80, 0 }, ;
      { "EXPR", "C", 255, 0 } }

   dbCreate( cQFile, aArray_ )

RETURN

STATIC FUNCTION InsDel( cOrig, nStart, nDelete, cInsert )

   LOCAL cLeft := Left( cOrig, nStart - 1 )
   LOCAL cRight := SubStr( cOrig, nStart + nDelete )

Return( cLeft + cInsert + cRight )
