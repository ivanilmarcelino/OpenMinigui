#include "hmg.ch"

Function Main
   Local aItems := {}

   aeval( array(15), {|| aadd( aItems, { 0, '', '' } ) } )

   SET CELLNAVIGATIONMODE VERTICAL
//   SET CELLNAVIGATION MODE HORIZONTAL

   define window win_1 at 0, 0 width 528 height 300 ;
      title 'Cell Navigation Downwards Demo' ;
      main nomaximize nosize

      define grid grid_1
         row 10
         col 10
         width 501
         height 250
         widths { 80, 200, 200 }
         headers { 'No.', 'Name', 'Description' }
         items aItems
         columncontrols { { 'TEXTBOX', 'NUMERIC', '999' }, { 'TEXTBOX', 'CHARACTER' }, { 'TEXTBOX', 'CHARACTER' } }
         justify { GRID_JTFY_RIGHT, GRID_JTFY_LEFT, GRID_JTFY_LEFT }
         columnwhen { {|| .t. }, {|| win_1.grid_1.cell( GetProperty("win_1", "grid_1", "CellRowFocused"), 1 ) > 0 }, {|| .t. } }
         allowedit .t.
         cellnavigation .t.
         value {1, 1}
      end grid

      ON KEY TAB       ACTION TabAction(.F.)
      ON KEY SHIFT+TAB ACTION TabAction(.T.)
      on key escape action thiswindow.release()

   end window

   win_1.center
   win_1.activate

Return Nil


FUNCTION TabAction( lShift )
   LOCAL nRow      := GetProperty("win_1", "grid_1", "CellRowFocused")
   LOCAL nCol      := GetProperty("win_1", "grid_1", "CellColFocused")
   LOCAL nRowCount := GetProperty("win_1", "grid_1", "ItemCOUNT")
   LOCAL nColCount := GetProperty("win_1", "grid_1", "ColumnCOUNT")

   IF lShift
      IF nCol == 1
         IF nRow > 1
            --nRow
            nCol := nColCount
         ENDIF
      ELSE
         --nCol
      ENDIF
   ELSE
      IF nCol == nColCount
         IF nRow < nRowCount
            ++nRow
            nCol := 1
         ENDIF
      ELSE
         ++nCol
      ENDIF
   ENDIF

   SetProperty("win_1", "grid_1", "VALUE", {nRow, nCol})

RETURN NIL
