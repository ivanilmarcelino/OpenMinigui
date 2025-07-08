#include <hmg.ch>

FUNCTION Main()

   LOCAL aItems := { ;
      { 'Row1, Column1', 'Row1, Column2' }, ;
      { 'Row2, Column1', 'Row2, Column2' }, ;
      { 'Row3, Column1', 'Row3, Column2' }, ;
      { 'Row4, Column1', 'Row4, Column2' }, ;
      { 'Row5, Column1', 'Row5, Column2' }, ;
      { 'Row6, Column1', 'Row6, Column2' }, ;
      { 'Row7, Column1', 'Row7, Column2' }, ;
      { 'Row8, Column1', 'Row8, Column2' }, ;
      { 'Row9, Column1', 'Row9, Column2' } ;
      }

   SET FONT TO "Tahoma", 12

   DEFINE WINDOW MAIN AT 0, 0 WIDTH 640 HEIGHT 480 TITLE 'Grid Row Height demo' MAIN

      DEFINE GRID grid_1
         ROW 10
         COL 10
         WIDTH 545
         HEIGHT 200
         HEADERS { 'Column1', 'Column2' }
         WIDTHS { 200, 200 }
         JUSTIFY { 0, 0 }
         ITEMS aItems
         FONTSIZE 9
      END GRID

      SetGridRowHeight ( this.Name, "Grid_1", 20 )

      DEFINE GRID grid_2
         ROW 230
         COL 10
         WIDTH 545
         HEIGHT 200
         HEADERS { 'Column1', 'Column2' }
         WIDTHS { 200, 200 }
         JUSTIFY { 0, 0 }
         ITEMS aItems
      END GRID

      SetGridRowHeight ( this.Name, "Grid_2", 40 )

      ON KEY ESCAPE ACTION thiswindow.Release()

   END WINDOW

   MAIN.center()
   MAIN.activate()

RETURN NIL

FUNCTION SetGridRowHeight ( cFormName, cGridName, nHeight )

   LOCAL idx
   LOCAL cDummyPNG := "__Dummy_" + cFormName + "_" + cGridName + ".png"
   LOCAL hBitmap := BT_BitmapCreateNew ( 1, nHeight, { 255, 255, 255 } )
   LOCAL Ret := BT_BitmapSaveFile ( hBitmap, cDummyPNG, BT_FILEFORMAT_PNG )

   BT_BitmapRelease ( hBitmap )

   IF Ret
      idx := GetProperty ( cFormName, cGridName, "Index" )
      _HMG_aControlBkColor[ idx ] := { cDummyPNG }
      AddListViewBitmap( _HMG_aControlHandles[ idx ], _HMG_aControlBkColor[ idx ] )
   ENDIF

   DO EVENTS
   hb_FileDelete ( cDummyPNG )

RETURN Ret
