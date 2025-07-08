/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 */

#include "hmg.ch"

FUNCTION Main

   LOCAL aItems := {}

   SET DATE GERMAN

   AAdd ( aItems, { "Carrot", 5, 30, Date() + 1 } )
   AAdd ( aItems, { "Cauliflower", 0, 31, Date() + 2 } )
   AAdd ( aItems, { "Corn", 15, 32, Date() + 3 } )
   AAdd ( aItems, { "Tomato", 0, 33, Date() + 4 } )
   AAdd ( aItems, { "Zucchini", 20, 34, Date() + 5 } )

   DEFINE WINDOW Form_1 ;
         AT 0, 0 ;
         WIDTH 800 ;
         HEIGHT 400 ;
         TITLE 'Grid Inplace Edit Event' ;
         MAIN

      @  10, 10 LABEL Label_1 VALUE "" AUTOSIZE
      @  50, 10 LABEL Label_2 VALUE "" AUTOSIZE
      @ 100, 10 LABEL Label_3 VALUE "" AUTOSIZE
/*
      @ 150, 10 GRID Grid_1 ;
         WIDTH 760 ;
         HEIGHT 200 ;
         HEADERS { 'Character', 'Number', 'Number', 'Date' } ;
         WIDTHS { 140, 140, 140, 140 } ;
         ITEMS aItems ;
         EDIT ;
         CELLNAVIGATION ;
         COLUMNCONTROLS { { 'TEXTBOX', 'CHARACTER' }, { 'TEXTBOX', 'NUMERIC', '9,999' }, { 'SPINNER', 1, 50 }, { 'DATEPICKER', 'DROPDOWN' } } ;
         ON INPLACEEDITEVENT ProcGridInplaceEditEvent()
*/
      DEFINE GRID Grid_1
         ROW 150
         COL 10
         WIDTH 760
         HEIGHT 200
         HEADERS { 'Character', 'Number', 'Number', 'Date' }
         WIDTHS { 140, 140, 140, 140 }
         ITEMS aItems
         ALLOWEDIT .T.
         CELLNAVIGATION .T.
         COLUMNCONTROLS { { 'TEXTBOX', 'CHARACTER' }, { 'TEXTBOX', 'NUMERIC', '9,999' }, { 'SPINNER', 1, 50 }, { 'DATEPICKER', 'DROPDOWN' } }
         ON INPLACEEDITEVENT ProcGridInplaceEditEvent()
      END GRID

   END WINDOW

   CENTER WINDOW Form_1

   ACTIVATE WINDOW Form_1

RETURN NIL


FUNCTION ProcGridInplaceEditEvent()

   STATIC cControlName, cFormParentName

   DO CASE

   CASE This.IsInplaceEditEventInit == .T.

      Form_1.Label_1.VALUE := { "Grid Control: ", This.InplaceEditParentName, ".", This.InplaceEditGridName }

      GetControlNameByHandle ( This.InplaceEditControlHandle, @cControlName, @cFormParentName )

      Form_1.Label_2.VALUE := { "InplaceEdit Control: ", cFormParentName, ".", cControlName, ;
         " --> ", GetControlTypeByIndex ( This.InplaceEditControlIndex ) }

      Form_1.Label_3.VALUE := { "Start Value: ", GetProperty( cFormParentName, cControlName, "VALUE" ) }

   CASE This.IsInplaceEditEventRun == .T.

      Form_1.Label_3.VALUE := { "Current Value: ", GetProperty( cFormParentName, cControlName, "VALUE" ) }

   CASE This.IsInplaceEditEventFinish == .T.

      Form_1.Label_1.VALUE := ""
      Form_1.Label_2.VALUE := ""

      cFormParentName := This.InplaceEditParentName
      cControlName := This.InplaceEditGridName

      Form_1.Label_3.VALUE := { "Final Value: ", GetProperty( cFormParentName, cControlName, "CELL", This.CellRowIndex, This.CellColIndex ) }

   ENDCASE

RETURN NIL
