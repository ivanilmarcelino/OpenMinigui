/*
 * MINIGUI - Harbour Win32 GUI library Demo
 */

#include "hmg.ch"


FUNCTION MAIN

   LOCAL aRows
   LOCAL bColor, fColor
   LOCAL n
   LOCAL flag

   DEFINE WINDOW Form_1 ;
         WIDTH 800 ;
         HEIGHT 480 ;
         TITLE "GRID Extended Test" ;
         MAIN

      aRows := Array ( 20 )
      aRows[ 1 ] := { 'Simpson', 'Homer', '555-5555', 1, Date() }
      aRows[ 2 ] := { 'Mulder', 'Fox', '324-6432', 2, Date() }
      aRows[ 3 ] := { 'Smart', 'Max', '432-5892', 3, Date() }
      aRows[ 4 ] := { 'Grillo', 'Pepe', '894-2332', 4, Date() }
      aRows[ 5 ] := { 'Kirk', 'James', '346-9873', 5, Date() }
      aRows[ 6 ] := { 'Barriga', 'Carlos', '394-9654', 6, Date() }
      aRows[ 7 ] := { 'Flanders', 'Ned', '435-3211', 7, Date() }
      aRows[ 8 ] := { 'Smith', 'John', '123-1234', 8, Date() }
      aRows[ 9 ] := { 'Pedemonti', 'Flavio', '000-0000', 9, Date() }
      aRows[ 10 ] := { 'Gomez', 'Juan', '583-4832', 10, Date() }
      aRows[ 11 ] := { 'Fernandez', 'Raul', '321-4332', 11, Date() }
      aRows[ 12 ] := { 'Borges', 'Javier', '326-9430', 12, Date() }
      aRows[ 13 ] := { 'Alvarez', 'Alberto', '543-7898', 13, Date() }
      aRows[ 14 ] := { 'Gonzalez', 'Ambo', '437-8473', 14, Date() }
      aRows[ 15 ] := { 'Batistuta', 'Gol', '485-2843', 15, Date() }
      aRows[ 16 ] := { 'Vinazzi', 'Amigo', '394-5983', 16, Date() }
      aRows[ 17 ] := { 'Pedemonti', 'Flavio', '534-7984', 17, Date() }
      aRows[ 18 ] := { 'Samarbide', 'Armando', '854-7873', 18, Date() }
      aRows[ 19 ] := { 'Pradon', 'Alejandra', '???-????', 19, Date() }
      aRows[ 20 ] := { 'Reyes', 'Monica', '432-5836', 20, Date() }


      bColor := {|| iif ( This.CellRowIndex / 2 == Int( This.CellRowIndex / 2 ), { 128, 128, 128 }, { 192, 192, 192 } ) }
      fColor := {|| iif ( This.CellRowIndex / 2 == Int( This.CellRowIndex / 2 ), BLUE, RED ) }


      CellNavigationColor ( _SELECTEDCELL_FORECOLOR, BROWN )
      CellNavigationColor ( _SELECTEDCELL_BACKCOLOR, GREEN )
      CellNavigationColor ( _SELECTEDCELL_DISPLAYCOLOR, .T. )


      CellNavigationColor ( _SELECTEDROW_FORECOLOR, YELLOW )
      CellNavigationColor ( _SELECTEDROW_BACKCOLOR, BROWN )
      CellNavigationColor ( _SELECTEDROW_DISPLAYCOLOR, .T. )


      @ 50, 15 GRID Grid_1 ;
         WIDTH 750 ;
         AUTOSIZEHEIGHT 7 ;
         HEADERS { 'Last Name', 'First Name', '*--- Phone ----------------*', 'Row', 'Date' } ;
         WIDTHS { 140, 140, 40, 140, 140 } ;
         ITEMS aRows ;
         BACKCOLOR BLACK ;
         FONTCOLOR WHITE ;
         BOLD ;
         COLUMNCONTROLS { NIL, NIL, NIL, { 'SPINNER', 0, 50 }, { "DATEPICKER", "UPDOWN" } } ;
         VALUE 1 EDIT ;
         DYNAMICBACKCOLOR { bColor, bColor, bColor, bColor, bColor } ;
         DYNAMICFORECOLOR { fColor, fColor, fColor, fColor, fColor } ;
         TOOLTIP 'Editable Grid Control' ;
         CELLNAVIGATION

      Form_1.Grid_1.PAINTDOUBLEBUFFER := .T.

      Form_1.Grid_1.ColumnHEADER ( 1 ) := "--- Last Name ---"
      Form_1.Grid_1.ColumnWIDTH ( 1 ) := 100
      Form_1.Grid_1.ColumnJUSTIFY ( 1 ) := GRID_JTFY_CENTER
      Form_1.Grid_1.ColumnCONTROL ( 1 ) := { 'TEXTBOX', 'CHARACTER', '@!' }
      Form_1.Grid_1.ColumnDYNAMICFORECOLOR ( 1 ) := {|| BLACK }
      Form_1.Grid_1.ColumnDYNAMICBACKCOLOR ( 1 ) := {|| PURPLE }
      Form_1.Grid_1.HeaderDYNAMICFORECOLOR ( 1 ) := {|| BLACK }
      Form_1.Grid_1.HeaderDYNAMICBACKCOLOR ( 1 ) := {|| PURPLE }
      FOR n := 2 TO 5
         Form_1.Grid_1.HeaderDYNAMICFORECOLOR ( n ) := {|| BLACK }
         Form_1.Grid_1.HeaderDYNAMICBACKCOLOR ( n ) := {|| { 192, 192, 192 } }
      NEXT
      Form_1.Grid_1.ColumnVALID ( 1 ) := {|| NIL }
      Form_1.Grid_1.ColumnWHEN ( 1 ) := {|| .F. }
      Form_1.Grid_1.ColumnONHEADCLICK ( 1 ) := {|| MsgInfo ( Form_1.Grid_1.ColumnHEADER( 1 ) ) }

      Form_1.Grid_1.AddItem ( { 'lolo', 'JUAN', '333-9999', 21, Date() } )
      Form_1.Grid_1.AddItem ( { 'Lolo', 'LOLO', '999-9999', 22, Date() - 1 }, NIL ) // Added this item in the END of the GRID
      Form_1.Grid_1.AddItem ( { 'hmg' , 'JUAN', '333-9999', 23, Date() - 1 },   3 ) // Added this item in Row 3

      n := 5
      flag := .T.
      @ 250, 55 BUTTON Button_1 CAPTION "AddCol" ACTION AddColumnEx ( ++n )
      @ 250, 155 BUTTON Button_2 CAPTION "DelCol" ACTION IF ( n > 5, DelColumnEx ( n-- ), NIL )
      @ 250, 255 BUTTON Button_3 CAPTION "Change Col #5" ACTION {|| flag := .NOT. ( flag ), Form_1.Grid_1.ColumnCONTROL ( 5 ) := iif ( flag == .T., { "DATEPICKER", "UPDOWN" }, { "DATEPICKER", "DROPDOWN" } ) }

   END WINDOW

   CENTER WINDOW Form_1

   ACTIVATE WINDOW Form_1

RETURN NIL


PROCEDURE AddColumnEx ( nColumn )

   LOCAL form := "Form_1"
   LOCAL control := "Grid_1"
   LOCAL i := GetControlIndex( control, form )
   LOCAL aItems := {}, n, aEditcontrols, adfc, adbc
   LOCAL aRow
   LOCAL fColor2 := {|| WHITE }
   LOCAL bColor2 := {|| BLACK }

   FOR n := 1 TO GetProperty( form, control, "ItemCount" )
      AAdd( aItems, GetProperty( form, control, "Item", n ) )
   NEXT
   adfc := _HMG_aControlMiscData1[ i ][ _GRID_COLUMN_DYNAMICFORECOLOR_ ]
   AIns( adfc, nColumn, fColor2, .T. )
   _HMG_aControlMiscData1[ i ][ _GRID_COLUMN_DYNAMICFORECOLOR_ ] := adfc
   adbc := _HMG_aControlMiscData1[ i ][ _GRID_COLUMN_DYNAMICBACKCOLOR_ ]
   AIns( adbc, nColumn, bColor2, .T. )
   _HMG_aControlMiscData1[ i ][ _GRID_COLUMN_DYNAMICBACKCOLOR_ ] := adbc
   aEditcontrols := _HMG_aControlMiscData1[ i ][ _GRID_COLUMN_CONTROL_ ]
   AIns( aEditcontrols, nColumn, { 'TEXTBOX', 'NUMERIC' }, .T. )
   _HMG_aControlMiscData1[ i ][ _GRID_COLUMN_CONTROL_ ] := aEditcontrols

   Form_1.Grid_1.AddColumn ( nColumn, "Col" + hb_ntos( nColumn ), 100, 1 )

   DoMethod( form, control, "DisableUpdate" )
   FOR i := 1 TO Len( aItems )
      aRow := aItems[ i ]
      AIns( aRow, nColumn, 0, .T. )
      DoMethod( form, control, "AddItem", aRow )
   NEXT i
   DoMethod( form, control, "EnableUpdate" )

RETURN


PROCEDURE DelColumnEx ( nColumn )

   LOCAL form := "Form_1"
   LOCAL control := "Grid_1"

   LOCAL i := GetControlIndex( control, form ), aRow
   LOCAL aItems := {}, n, aEditcontrols, adbc

   FOR n := 1 TO GetProperty( form, control, "ItemCount" )
      AAdd( aItems, GetProperty( form, control, "Item", n ) )
   NEXT n
   adbc := _HMG_aControlMiscData1[ i ][ _GRID_COLUMN_DYNAMICBACKCOLOR_ ]
   ADel( adbc, nColumn, .T. )
   _HMG_aControlMiscData1[ i ][ _GRID_COLUMN_DYNAMICBACKCOLOR_ ] := adbc
   aEditcontrols := _HMG_aControlMiscData1[ i ][ _GRID_COLUMN_CONTROL_ ]
   ADel( aEditcontrols, nColumn, .T. )
   _HMG_aControlMiscData1[ i ][ _GRID_COLUMN_CONTROL_ ] := aEditcontrols

   Form_1.Grid_1.DeleteColumn ( nColumn )

   DoMethod( form, control, "DisableUpdate" )
   FOR i := 1 TO Len( aItems )
      aRow := aItems[ i ]
      ADel( aRow, nColumn, .T. )
      DoMethod( form, control, "AddItem", aRow )
   NEXT i
   DoMethod( form, control, "EnableUpdate" )

RETURN
