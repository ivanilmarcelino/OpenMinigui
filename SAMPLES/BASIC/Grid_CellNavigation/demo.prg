/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Demo was contributed to HMG forum by Edward 11/Nov/2021
 *
 * Adapted for MiniGUI Extended Edition by Grigory Filatov
*/

#include "hmg.ch"

MEMVAR aRows

FUNCTION Main()

   LOCAL i

   PUBLIC aRows[ 21 ][ 3 ]

   aRows[ 1 ] := { 'Simpson', 'Homer', '555-5555' }
   aRows[ 2 ] := { 'Mulder', 'Fox', '324-6432' }
   aRows[ 3 ] := { 'Smart', 'Max', '432-5892' }
   aRows[ 4 ] := { 'Grillo', 'Pepe', '894-2332' }
   aRows[ 5 ] := { 'Kirk', 'James', '346-9873' }
   aRows[ 6 ] := { 'Barriga', 'Carlos', '394-9654' }
   aRows[ 7 ] := { 'Flanders', 'Ned', '435-3211' }
   aRows[ 8 ] := { 'Smith', 'John', '123-1234' }
   aRows[ 9 ] := { 'Pedemonti', 'Flavio', '000-0000' }
   aRows[ 10 ] := { 'Gomez', 'Juan', '583-4832' }
   aRows[ 11 ] := { 'Fernandez', 'Raul', '321-4332' }
   aRows[ 12 ] := { 'Borges', 'Javier', '326-9430' }
   aRows[ 13 ] := { 'Alvarez', 'Alberto', '543-7898' }
   aRows[ 14 ] := { 'Gonzalez', 'Ambo', '437-8473' }
   aRows[ 15 ] := { 'Batistuta', 'Gol', '485-2843' }
   aRows[ 16 ] := { 'Vinazzi', 'Amigo', '394-5983' }
   aRows[ 17 ] := { 'Pedemonti', 'Flavio', '534-7984' }
   aRows[ 18 ] := { 'Samarbide', 'Armando', '854-7873' }
   aRows[ 19 ] := { 'Pradon', 'Alejandra', '???-????' }
   aRows[ 20 ] := { 'Reyes', 'Monica', '432-5836' }
   aRows[ 21 ] := { 'Fernandez', 'two', '0000-0000' }

   FOR i = 1 TO 21
      AAdd ( aRows[ i ], i )
   NEXT

   DEFINE WINDOW Form_1 ;
         AT 0, 0 ;
         WIDTH 800 ;
         HEIGHT 650 ;
         TITLE "CELLNAVIGATION (F9 - On/Off)" ;
         MAIN

      @ 10, 10 GRID Grid_1 ;
         WIDTH 760 ;
         HEIGHT 590 ;
         HEADERS { 'Last Name', 'First Name', 'Phone', "Num" } ;
         WIDTHS { 140, 140, 140, 50 } ;
         ITEMS aRows ;
         VALUE 1 ;
         EDIT ;
         COLUMNVALID { {|| ChangeGridCell() }, {|| ChangeGridCell() }, {|| ChangeGridCell() }, {|| ChangeGridCell() } } ;
         VIRTUAL ;
         ON QueryData QueryTest() ;
         CELLNAVIGATION

      Form_1.Grid_1.ColumnCONTROL ( 4 ) := { "TEXTBOX", "NUMERIC", NIL, NIL }
      Form_1.Grid_1.ColumnJUSTIFY ( 4 ) := GRID_JTFY_RIGHT

      ON KEY F9 ACTION OnF9Key()

   END WINDOW

   CENTER WINDOW Form_1

   ACTIVATE WINDOW Form_1

RETURN NIL


FUNCTION QueryTest()

   LOCAL i := This.QueryRowIndex
   LOCAL j := This.QueryColIndex

   This.QueryData := aRows[ i, j ]

RETURN NIL


FUNCTION ChangeGridCell()

   LOCAL nRow := This.CellRowIndex, nCol := This.CellColIndex
   LOCAL xValue := This.CellValue

   // you can manipulate and/or check values of cells

   // Store cell value into an array
   aRows[ nRow ][ nCol ] := xValue

RETURN .T.


FUNCTION OnF9Key()

   Form_1.Grid_1.CellNavigation := .NOT. ( Form_1.Grid_1.CellNavigation )

   Form_1.Grid_1.Editable := ( Form_1.Grid_1.CellNavigation )

RETURN NIL
