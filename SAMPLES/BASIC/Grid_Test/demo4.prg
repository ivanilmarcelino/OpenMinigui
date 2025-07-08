/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
*/

#include "hmg.ch"

FUNCTION Main
   LOCAL aItems := {}, i

   AEval( Array(30), {|| AAdd( aItems, { 0, .f., date() } ) } )

   set font to _GetSysFont(), 10

   define window win_1 ;
      width 426 height 705 ;
      title 'Cell Navigation Demo' ;
      main ;
      nomaximize nosize

      define grid grid_1
         row 10
         col 10
         autosizeheight -1
         autosizewidth .t.
         widths { 100, 140, 160 }
         headers { 'Number', 'Logical', 'Date' }
         items aItems
         value {1, 1}
         justify { GRID_JTFY_RIGHT, GRID_JTFY_LEFT, GRID_JTFY_LEFT }
         columncontrols { { 'TEXTBOX', 'NUMERIC', '9999' }, { 'CHECKBOX' , 'Yes' , 'No' }, { 'DATEPICKER', 'DROPDOWN' } }
         allowedit .t.
         cellnavigation .t.
         PaintDoubleBuffer .t.
      end grid

      on key escape action thiswindow.release()

   end window

   win_1.Title := (win_1.Title) + ' -> # visible rows: ' + hb_ntos(GetNumOfVisibleRows('grid_1', 'win_1'))

   win_1.height := win_1.grid_1.height + GetTitleHeight() + 2*GetBorderHeight() + win_1.grid_1.row
   win_1.width := win_1.grid_1.width + 2*GetBorderWidth() + win_1.grid_1.col

   for i:=1 to len(aItems)
      win_1.grid_1.cell(i, 1) := i
      win_1.grid_1.cell(i, 2) := ( i%2 == 0 )
   next

   win_1.center
   win_1.activate

Return Nil


Function GetNumOfVisibleRows ( ControlName , ParentForm )
   LOCAL i

   i := GetControlIndex ( ControlName , ParentForm )

Return ListviewGetCountPerPage ( GetControlHandleByIndex( i ) )
