/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Demo was contributed to HMG forum by Edward on Jul 23, 2018
 *
 */

#define _NO_BTN_PICTURE_

#include "hmg.ch"

STATIC bColor := {}
STATIC fColor := {}
STATIC bColorMap := {}


FUNCTION Main()

   LOCAL aRows := Array ( 20 )

   aRows[ 1 ] := { 'Simpson', 'Homer', '555-5555', 1, Time() }
   aRows[ 2 ] := { 'Mulder', 'Fox', '324-6432', 2, Time() }
   aRows[ 3 ] := { 'Smart', 'Max', '432-5892', 3, Time() }
   aRows[ 4 ] := { 'Grillo', 'Pepe', '894-2332', 4, Time() }
   aRows[ 5 ] := { 'Kirk', 'James', '346-9873', 5, Time() }
   aRows[ 6 ] := { 'Barriga', 'Carlos', '394-9654', 6, Time() }
   aRows[ 7 ] := { 'Flanders', 'Ned', '435-3211', 7, Time() }
   aRows[ 8 ] := { 'Smith', 'John', '123-1234', 8, Time() }
   aRows[ 9 ] := { 'Pedemonti', 'Flavio', '000-0000', 9, Time() }
   aRows[ 10 ] := { 'Gomez', 'Juan', '583-4832', 10, Time() }
   aRows[ 11 ] := { 'Fernandez', 'Raul', '321-4332', 11, Time() }
   aRows[ 12 ] := { 'Borges', 'Javier', '326-9430', 12, Time() }
   aRows[ 13 ] := { 'Alvarez', 'Alberto', '543-7898', 13, Time() }
   aRows[ 14 ] := { 'Gonzalez', 'Ambo', '437-8473', 14, Time() }
   aRows[ 15 ] := { 'Batistuta', 'Gol', '485-2843', 15, Time() }
   aRows[ 16 ] := { 'Vinazzi', 'Amigo', '394-5983', 16, Time() }
   aRows[ 17 ] := { 'Pedemonti', 'Flavio', '534-7984', 17, Time() }
   aRows[ 18 ] := { 'Samarbide', 'Armando', '854-7873', 18, Time() }
   aRows[ 19 ] := { 'Pradon', 'Alejandra', '???-????', 19, Time() }
   aRows[ 20 ] := { 'Reyes', 'Monica', '432-5836', 20, Time() }

   LOAD WINDOW Form_1
   Form_1.CENTER
   Form_1.ACTIVATE

RETURN NIL


PROCEDURE cor()

   LOCAL i
   bColor := {|| iif ( This.CellRowIndex / 2 == Int( This.CellRowIndex / 2 ), RED, BLUE ) }
   FOR i = 1 TO Form_1.Grid_1.ColumnCOUNT
      Form_1.Grid_1.ColumnDYNAMICBACKCOLOR ( i ) := bColor
   NEXT i

RETURN


PROCEDURE un_cor()

   LOCAL i
   bColor := {}
   FOR i = 1 TO Form_1.Grid_1.ColumnCOUNT
      Form_1.Grid_1.ColumnDYNAMICBACKCOLOR ( i ) := bColor
   NEXT i

RETURN


FUNCTION ReadColorMap( nCellCol )

   LOCAL nCellRow := This.CellRowIndex
   LOCAL CellColor := WHITE, nPosColorMap

   // Seek Row at Color map
   nPosColorMap := AScan( bColorMap, {| x | x[ 1 ][ 1 ] = nCellRow .AND. x[ 1 ][ 2 ] = 0 } )
   IF nPosColorMap > 0
      CellColor := bColorMap[ nPosColorMap ][ 2 ]
   ENDIF

   // Seek Col at Color map
   nPosColorMap := AScan( bColorMap, {| x | x[ 1 ][ 1 ] = 0 .AND. x[ 1 ][ 2 ] = nCellCol } )
   IF nPosColorMap > 0
      CellColor := bColorMap[ nPosColorMap ][ 2 ]
   ENDIF

   // Seek Cell at Color map
   nPosColorMap := AScan( bColorMap, {| x | x[ 1 ][ 1 ] = nCellRow .AND. x[ 1 ][ 2 ] = nCellCol } )

   IF nPosColorMap > 0
      CellColor := bColorMap[ nPosColorMap ][ 2 ]
   ENDIF

RETURN CellColor


FUNCTION Colour_grid( nWhat, aColor )

   LOCAL nCellRow := GetProperty( "Form_1", "Grid_1", "CellRowFocused" )
   LOCAL nCellCol := GetProperty( "Form_1", "Grid_1", "CellColFocused" )
   LOCAL nPosColorMap, aColorCell, cBlock, i

   // nWhat = 0 -> Cell
   // nWhat = 1 -> Column
   // nWhat = 2 -> Row
   SWITCH nWhat
   CASE 1
      nCellRow := 0
      EXIT
   CASE 2
      nCellCol := 0
      EXIT
   ENDSWITCH

   nPosColorMap := AScan( bColorMap, {| x | x[ 1 ][ 1 ] = nCellRow .AND. x[ 1 ][ 2 ] = nCellCol } )
   aColorCell := { { nCellRow, nCellCol }, aColor }

   IF nPosColorMap == 0
      IF aColor # NIL
         AAdd( bColorMap, aColorCell ) // Add color to map
      ENDIF
   ELSE
      IF aColor == NIL
         ADel( bColorMap, nPosColorMap, .T. ) // remove color from map
      ELSE
         bColorMap[ nPosColorMap ] := aColorCell // replace color at map
      ENDIF
   ENDIF

   FOR i = 1 TO Form_1.Grid_1.ColumnCOUNT
      cBlock := "ReadColorMap(" + AllTrim( Str( i ) ) + ")"
      Form_1.Grid_1.ColumnDYNAMICBACKCOLOR ( i ) := {|| &cBlock }
   NEXT

RETURN NIL
