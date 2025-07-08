/*
 * MINIGUI - Harbour Win32 GUI library Demo
 */

#include "minigui.ch"

MEMVAR aRows, nColumn

FUNCTION Main()

   PRIVATE aRows[ 10 ][ 2 ], nColumn := 1

   aRows[ 1 ] := { 'Simpson', 'Homer' }
   aRows[ 2 ] := { 'Mulder', 'Fox' }
   aRows[ 3 ] := { 'Smart', 'Max' }
   aRows[ 4 ] := { 'Grillo', 'Pepe' }
   aRows[ 5 ] := { 'Kirk', 'James' }
   aRows[ 6 ] := { 'Barriga', 'Carlos' }
   aRows[ 7 ] := { 'Flanders', 'Ned' }
   aRows[ 8 ] := { 'Smith', 'John' }
   aRows[ 9 ] := { 'Pedemonti', 'Flavio' }
   aRows[ 10 ] := { 'Gomez', 'Juan' }

   DEFINE WINDOW Form_1 ;
         AT 0, 0 ;
         WIDTH 568 ;
         HEIGHT 430 ;
         TITLE 'Pesquisa incremental no Grid' ;
         MAIN ;
         FONT 'Arial' ;
         SIZE 9 ;
         ON INIT FillGrid()

      DEFINE LABEL Label_1
         ROW 360
         COL 10
         WIDTH 80
         VALUE 'Pesquisando:'
         VCENTERALIGN .T.
      END LABEL

      DEFINE TEXTBOX Text_1
         ROW 360
         COL 95
         WIDTH 150
         ONCHANGE {|| SearchChange() }
      END TEXTBOX

      DEFINE GRID Grid_1
         ROW 10
         COL 10
         WIDTH 420
         HEIGHT 330
         HEADERS { 'Sobrenome', 'Nome' }
         WIDTHS { 140, 140 }
         CELLNAVIGATION .T.
         ON CHANGE iif( GetProperty( "Form_1", "Grid_1", "ItemCount" ) > 0, nColumn := Form_1.Grid_1.VALUE[ 2 ], )
         VALUE { 1, nColumn }
         COLUMNSORT {}
         ONINIT HMG_SortColumn( nColumn )
      END GRID

   END WINDOW

   CENTER WINDOW Form_1
   ACTIVATE WINDOW Form_1

RETURN NIL

/*********************************************************/
PROCEDURE SearchChange()
/*********************************************************/
   LOCAL cTxt, nLen, aRow

   cTxt := GetProperty( 'Form_1', 'Text_1', 'Value' )
   nLen := Len( cTxt )

   Form_1.Grid_1.DeleteAllItems

   // Procura o texto

   FOR EACH aRow IN aRows
      IF Upper( cTxt ) = Upper( Left( aRow[ nColumn ], nLen ) )
         Form_1.Grid_1.AddItem ( aRow )
      ENDIF
   NEXT

   SortColumn( nColumn )

RETURN

/*********************************************************/
PROCEDURE FillGrid()
/*********************************************************/
   Form_1.Grid_1.SetArray ( aRows )

   SortColumn( nColumn )

RETURN

/*********************************************************/
PROCEDURE SortColumn( nColumnNo )
/*********************************************************/
   LOCAL aRows := Form_1.Grid_1.GetArray ()

   Form_1.Grid_1.Refresh
   ASort( aRows, , , {| x, y | x[ nColumnNo ] < y[ nColumnNo ] } )
   Form_1.Grid_1.DeleteAllItems
   AEval( aRows, {| x | Form_1.Grid_1.AddItem( x ) } )
   Form_1.Grid_1.VALUE := { 1, nColumnNo }

RETURN
