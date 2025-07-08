#include "hmg.ch"
#include "TSBrowse.ch"

REQUEST DBFCDX

FUNCTION Main()

   FIELD FIRST, LAST, STREET, CITY, STATE, ZIP, HIREDATE, MARRIED, AGE, SALARY
   FIELD FIRST2, LAST2, STREET2, CITY2, STATE2, ZIP2, HIREDATE2, MARRIED2, AGE2, SALARY2
   FIELD FIRST3, LAST3, STREET3, CITY3, STATE3, ZIP3, HIREDATE3, MARRIED3, AGE3, SALARY3
   FIELD FIRST4, LAST4, STREET4, CITY4, STATE4, ZIP4, HIREDATE4, MARRIED4, AGE4, SALARY4
   FIELD FIRST5, LAST5, STREET5, CITY5, STATE5, ZIP5, HIREDATE5, MARRIED5, AGE5, SALARY5
   FIELD FIRST6, LAST6, STREET6, CITY6, STATE6, ZIP6, HIREDATE6, MARRIED6, AGE6, SALARY6
   FIELD FIRST7, LAST7, STREET7, CITY7, STATE7, ZIP7, HIREDATE7, MARRIED7, AGE7, SALARY7
   FIELD FIRST8, LAST8, STREET8, CITY8, STATE8, ZIP8, HIREDATE8, MARRIED8, AGE8, SALARY8

   LOCAL obrw, aStruct, cCust, cmiln, n, nLen, a, i, c

   cCust := "customer.dbf"
   cmiln := "custmiln.dbf"

   SET OOP ON

   IF File( cmiln )
      USE ( cmiln ) NEW ALIAS "CUST" VIA "DBFCDX"

   ELSE

      aStruct := hmg_DBfSTRUCT( cCust )
      ASize( aStruct, Len( aStruct ) - 1 )
      nLen := Len( aStruct )
      FOR i := 2 TO 8
         c := Str( i, 1, 0 )
         FOR n := 2 TO nLen
            a := AClone( aStruct[ n ] )
            a[ 1 ] += c
            AAdd( aStruct, a )
         NEXT
      NEXT

      dbCreate( cmiln, aStruct, "DBFCDX", .T., "CUST" )

      FOR n := 1 TO 20 // 2000
         APPEND FROM customer.dbf ;
            FIELDS FIRST, LAST, STREET, CITY, STATE, ZIP, HIREDATE, MARRIED, AGE, SALARY
      NEXT
      GO TOP
      REPLACE ALL FIRST2 WITH FIRST, LAST2 WITH LAST, CITY2 WITH CITY, STATE2 WITH STATE, ;
         ZIP2 WITH ZIP, HIREDATE2 WITH HIREDATE, MARRIED2 WITH MARRIED, ;
         AGE2 WITH AGE, SALARY2 WITH SALARY
      REPLACE ALL FIRST3 WITH FIRST, LAST3 WITH LAST, CITY3 WITH CITY, STATE3 WITH STATE, ;
         ZIP3 WITH ZIP, HIREDATE3 WITH HIREDATE, MARRIED3 WITH MARRIED, ;
         AGE3 WITH AGE, SALARY3 WITH SALARY
      REPLACE ALL FIRST4 WITH FIRST, LAST4 WITH LAST, CITY4 WITH CITY, STATE4 WITH STATE, ;
         ZIP4 WITH ZIP, HIREDATE4 WITH HIREDATE, MARRIED4 WITH MARRIED, ;
         AGE4 WITH AGE, SALARY4 WITH SALARY
      REPLACE ALL FIRST5 WITH FIRST, LAST5 WITH LAST, CITY5 WITH CITY, STATE5 WITH STATE, ;
         ZIP5 WITH ZIP, HIREDATE5 WITH HIREDATE, MARRIED5 WITH MARRIED, ;
         AGE5 WITH AGE, SALARY5 WITH SALARY
      REPLACE ALL FIRST6 WITH FIRST, LAST6 WITH LAST, CITY6 WITH CITY, STATE6 WITH STATE, ;
         ZIP6 WITH ZIP, HIREDATE6 WITH HIREDATE, MARRIED6 WITH MARRIED, ;
         AGE6 WITH AGE, SALARY6 WITH SALARY
      REPLACE ALL FIRST7 WITH FIRST, LAST7 WITH LAST, CITY7 WITH CITY, STATE7 WITH STATE, ;
         ZIP7 WITH ZIP, HIREDATE7 WITH HIREDATE, MARRIED7 WITH MARRIED, ;
         AGE7 WITH AGE, SALARY7 WITH SALARY
      REPLACE ALL FIRST8 WITH FIRST, LAST8 WITH LAST, CITY8 WITH CITY, STATE8 WITH STATE, ;
         ZIP8 WITH ZIP, HIREDATE8 WITH HIREDATE, MARRIED8 WITH MARRIED, ;
         AGE8 WITH AGE, SALARY8 WITH SALARY

      GO TOP
   ENDIF

   SET DELETE ON

   DEFINE WINDOW win_1 AT 0, 0 WIDTH 1004 HEIGHT 541 ;
         MAIN TITLE hb_ntos( LastRec() ) + " Records: 81 Fields: Record Lenght: 1061" NOMAXIMIZE NOSIZE

      DEFINE TOOLBAR ToolBar_1 BUTTONSIZE 8, 36 IMAGESIZE 24, 24 FLAT BORDER

         BUTTON TOP_1 ;
            PICTURE "res\go_first.bmp" ;
            TOOLTIP "Top" ;
            ACTION ( oBrw:GoTop(), oBrw:SetFocus() )

         BUTTON PREV_1 ;
            PICTURE "res\go_prev.bmp" ;
            TOOLTIP "Up" ;
            ACTION ( oBrw:GoUp(), oBrw:SetFocus() )

         BUTTON DOWN_1 ;
            PICTURE "res\go_next.bmp" ;
            TOOLTIP "Down" ;
            ACTION ( oBrw:GoDown(), oBrw:SetFocus() )

         BUTTON BOTTOM_1 ;
            PICTURE "res\go_last.bmp" ;
            TOOLTIP "Bottom" ;
            ACTION ( oBrw:GoBottom(), oBrw:SetFocus() ) SEPARATOR

         BUTTON NEW_1 ;
            PICTURE "res\frm_new.bmp" ;
            TOOLTIP "Add" ;
            ACTION ( ( oBrw:cAlias )->( dbAppend() ), oBrw:GoToRec( ( oBrw:cAlias )->( RecNo() ), .T. ), oBrw:SetFocus() )

         BUTTON EDIT_1 ;
            PICTURE "res\frm_edit.bmp" ;
            TOOLTIP "Edit" ;
            ACTION ( oBrw:PostMsg( WM_KEYDOWN, VK_F10, 0 ), oBrw:SetFocus() )

         BUTTON DELETE_1 ;
            PICTURE "res\frm_delete.bmp" ;
            TOOLTIP "Delete" ;
            ACTION ( iif( MsgYesNo( "Delete Record ?", , .T. ), oBrw:DeleteRow(), NIL ), oBrw:SetFocus() ) SEPARATOR

         BUTTON PRINT_1 ;
            PICTURE "res\frm_print.bmp" ;
            TOOLTIP "Report" ;
            ACTION PrintData( oBrw ) SEPARATOR

         BUTTON EXIT_1 ;
            PICTURE "res\frm_exit.bmp" ;
            TOOLTIP "Exit" ;
            ACTION Win_1.Release

      END TOOLBAR

      DEFINE TBROWSE obrw AT 56, 20 ;
         CELLED SELECTOR "res\pointer.bmp" ;
         COLORS CLR_BLACK, CLR_WHITE, CLR_BLACK, { RGB( 231, 242, 255 ), GetSysColor( COLOR_GRADIENTINACTIVECAPTION ) } ;
         ALIAS "CUST" ;
         WIDTH win_1.Width -40 - GetBorderWidth() / 2 HEIGHT 420 ;
         FONT "Arial" ;
         SIZE 9 ;
         ON INIT {| ob | TsbCreate( ob, .T. ) }

         :Cargo := oKeyData()

      END TBROWSE ON END {| ob | TsbCreate( ob, .F. ) }

      obrw:Cargo:aColumns := AClone( obrw:aColumns )

      ON KEY CONTROL + 1 ACTION _wPost( 1, , "1" )
      ON KEY CONTROL + 2 ACTION _wPost( 1, , "2" )
      ON KEY CONTROL + 3 ACTION _wPost( 1, , "3" )
      ON KEY CONTROL + 4 ACTION _wPost( 1, , "4" )
      ON KEY CONTROL + 5 ACTION _wPost( 1, , "5" )
      ON KEY CONTROL + 6 ACTION _wPost( 1, , "6" )
      ON KEY CONTROL + 7 ACTION _wPost( 1, , "7" )
      ON KEY CONTROL + 8 ACTION _wPost( 1, , "8" )
      ON KEY CONTROL + 0 ACTION _wPost( 1, , "0" )

      ( This.Object ):Event( 1, {| ow, ky, cn |
                                  LOCAL ob := This.obrw.Object
                                  LOCAL ac := ob:Cargo:aColumns, ni, oc
                                  LOCAL aCols := {}
                                  ky := Val( cn )
                                  FOR ni := 1 TO Len( ac )
                                     oc := ac[ ni ]
                                     IF ni == 1
                                        AAdd( aCols, ac[ ni ] )
                                     ELSEIF ky == 0
                                        AAdd( aCols, ac[ ni ] )
                                     ELSEIF ky == 1
                                        IF Val( Right( oc:cName, 1 ) ) == 0
                                           AAdd( aCols, ac[ ni ] )
                                        ENDIF
                                     ELSEIF Right( oc:cName, 1 ) == cn
                                        AAdd( aCols, ac[ ni ] )
                                     ENDIF
                                  NEXT
                                  ob:aColumns := aCols
                                  ob:nRowPos := 1
                                  ob:nCell := 2
                                  ob:Reset()
                                  RETURN NIL
                                } )

   END WINDOW

   CENTER WINDOW win_1
   ACTIVATE WINDOW win_1

RETURN NIL

*----------------------------------------
STATIC PROCEDURE TsbCreate( obrw, lInit )
*----------------------------------------
   LOCAL aStruct, cCust, n, nLen, a, i, c
   LOCAL aFields

   IF lInit

      cCust := "customer"
      aStruct := hmg_DBfSTRUCT( cCust )
      ASize( aStruct, Len( aStruct ) - 1 )
      nLen := Len( aStruct )
      FOR i := 2 TO 8
         c := Str( i, 1, 0 )
         FOR n := 2 TO nLen
            a := AClone( aStruct[ n ] )
            a[ 1 ] += c
            AAdd( aStruct, a )
         NEXT
      NEXT

      // initial columns
      aFields := {}
      FOR n := 1 TO Len( aStruct )
         a := aStruct[ n ][ 1 ]
         AAdd( aFields, a )
      NEXT

      LoadFields( "oBrw", "win_1", .T., aFields )

      WITH OBJECT oBrw
         :nHeightCell += 5
         :nHeightHead := oBrw:nHeightCell

         :SetColor( { 5 }, { CLR_WHITE } )
         :SetColor( { 6 }, { RGB( 0, 0, 128 ) } )

         :aColumns[ 1 ]:cPicture := "99,999,999"
         :aColumns[ 1 ]:lEdit := .F.

         :SetAppendMode( .F. )
         :SetDeleteMode( .T., .F. )

         :lNoResetPos := .T.
         :lNoMoveCols := .T.
         :lNoKeyChar := .T.
         :lNoChangeOrd := .T.
         :nFireKey := VK_F10 // default Edit key
      END OBJECT

   ELSE

      obrw:SetNoHoles()
      obrw:SetFocus()

   ENDIF

RETURN

*---------------------------------
STATIC PROCEDURE PrintData( oBrw )
*---------------------------------
   LOCAL aStruct, cCust, n, nLen, a
   LOCAL PrevRec
   LOCAL aHdr := {}
   LOCAL aLen := {}
   LOCAL aHdr1
   LOCAL aTot
   LOCAL aFmt

   cCust := "customer"
   aStruct := hmg_DBfSTRUCT( cCust )
   ASize( aStruct, Len( aStruct ) - 1 )
   nLen := Len( aStruct )
   FOR n := 2 TO nLen
      a := AClone( aStruct[ n ] )
      AAdd( aHdr, a[ 1 ] )
      AAdd( aLen, a[ 3 ] )
   NEXT

   aHdr1 := Array( Len( aHdr ) )
   aTot := Array( Len( aHdr ) )
   aFmt := Array( Len( aHdr ) )
   AFill( aHdr1, '' )
   AFill( aTot, .F. )
   AFill( aFmt, '' )

   PrevRec := ( oBrw:cAlias )->( RecNo() )
   ( oBrw:cAlias )->( dbGoTop() )

   DO REPORT ;
      TITLE Upper( cCust ) + ' Database List' ;
      HEADERS aHdr1, aHdr ;
      FIELDS aHdr ;
      WIDTHS aLen ;
      TOTALS aTot ;
      NFORMATS aFmt ;
      WORKAREA &( oBrw:cAlias ) ;
      LMARGIN 3 ;
      TMARGIN 3 ;
      PAPERSIZE DMPAPER_A4 ;
      PREVIEW

   ( oBrw:cAlias )->( dbGoto( PrevRec ) )

RETURN
