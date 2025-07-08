/*
  MINIGUI - Harbour Win32 GUI library Demo

 */

#include "minigui.ch"

FUNCTION main

   define window sample ;
      width 627 height 612 ;
      title "Custom Progressbar Demo" ;
      main ;
      on gotfocus checkgraph() ;
      on lostfocus checkgraph()

   define button pressme
      row 10
      col 10
      width 80
      caption "Press Me!"
      action ( sample.t1.value := 2 )
   end button

   define tab t1 at 50, 10 width 600 height 520 on change checkgraph()

      define page "Page 1"

      define button b1
         row 40
         col 20
         caption "Test 1"
         action msginfo( "Click 1" )
      end button

      define button b2
         row 40
         col 140
         caption "Test 2"
         action msginfo( "Click 2" )
      end button

      end page

      define page "Page 2"

      end page

   end tab

   end window

   sample.center
   sample.activate

RETURN NIL

FUNCTION docustomprogressbars

   IF sample.t1.value == 2

      custom_progress_bar( "sample", 100, 100, 300, 25, { 255, 0, 0 }, 10, 100 )
      custom_progress_bar( "sample", 150, 100, 300, 25, { 255, 255, 0 }, 40, 100 )
      custom_progress_bar( "sample", 200, 100, 300, 25, { 0, 255, 0 }, 60, 100 )
      custom_progress_bar( "sample", 250, 100, 300, 25, { 0, 0, 255 }, 80, 100 )

      custom_progress_bar( "sample", 300, 150, 25, 250, { 255, 0, 0 }, 10, 100 )
      custom_progress_bar( "sample", 300, 200, 25, 250, { 255, 255, 0 }, 40, 100 )
      custom_progress_bar( "sample", 300, 250, 25, 250, { 0, 255, 0 }, 60, 100 )
      custom_progress_bar( "sample", 300, 300, 25, 250, { 0, 0, 255 }, 80, 100 )

   ENDIF

RETURN NIL


FUNCTION checkgraph

   IF sample.t1.value == 2

      DO EVENTS
      docustomprogressbars()

   ELSE

      ERASE window sample

   ENDIF

RETURN NIL


FUNCTION custom_progress_bar( cWindowName, nRow, nCol, nWidth, nHeight, aColor, nValue, nMax )

   LOCAL nStartRow, nStartCol, nFinishRow, nFinishCol

   // borders
   DRAW RECTANGLE IN WINDOW &cWindowName AT nRow, nCol TO nRow + nHeight, nCol + nWidth PENCOLOR { 128, 128, 128 } FILLCOLOR { 255, 255, 255 }
   DRAW LINE IN WINDOW &cWindowName At nRow, nCol TO nRow + nHeight, nCol PENCOLOR { 64, 64, 64 } PENWIDTH 1
   DRAW LINE IN WINDOW &cWindowName At nRow, nCol TO nRow, nCol + nWidth PENCOLOR { 64, 64, 64 } PENWIDTH 1

   // progress bar
   IF nWidth > nHeight  // Horizontal Progress Bar

      nStartRow := nRow + 1
      nStartCol := nCol + 1
      nFinishRow := nRow + nHeight - 1
      nFinishCol := nCol + 1 + ( ( nWidth - 2 ) * nValue / nMax )

   ELSE  // Vertical Progress Bar

      nStartRow := nRow + nHeight - 1
      nStartCol := nCol + 1
      nFinishRow := nStartRow - ( ( nHeight - 2 ) * nValue / nMax )
      nFinishCol := nCol + nWidth - 1

   ENDIF

   DRAW RECTANGLE IN WINDOW &cWindowName AT nStartRow, nStartCol TO nFinishRow, nFinishCol PENCOLOR aColor FILLCOLOR aColor

RETURN NIL
