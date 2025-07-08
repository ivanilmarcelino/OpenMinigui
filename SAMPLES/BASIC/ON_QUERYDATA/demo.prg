/*
 * MiniGUI Virtual Grid Demo
 *
 */

#include "minigui.ch"

FUNCTION Main()

   LOCAL aItem, n
   LOCAL aData := Array( 1000, 2 )

   AEval( aData, {| e | e[ 1 ] := Str( Random( 1000 ) ), e[ 2 ] := Str( Random( 1000 ) ) } )

   // add the value of the checkbox to the 1st position of the existing array
   FOR EACH aItem IN aData
      hb_AIns( aItem, 1, .F., .T. )
   NEXT

   DEFINE WINDOW Form_1 ;
         CLIENTAREA 420, 370 ;
         TITLE 'Sorting Columns In A Virtual Grid' ;
         MAIN

      DEFINE MAIN MENU
         DEFINE POPUP 'File'
            MENUITEM 'Exit' ACTION ThisWindow.Release()
         END POPUP
      END MENU

      @ 10, 10 GRID Grid_1 ;
         WIDTH 400 ;
         HEIGHT 330 ;
         HEADERS { '', 'Column 2', 'Column 3' } ;
         WIDTHS { 0, 140, 140 } ;
         VIRTUAL ;
         ITEMCOUNT Len( aData ) ;
         ON QUERYDATA QueryTest( aData ) ;
         IMAGE { "no", "ok" } ;
         ON DBLCLICK SelectRow( aData )

      // add sorting by all columns of the grid when clicking on the header
      FOR n = 1 TO Form_1.Grid_1.ColumnCOUNT
         Form_1.Grid_1.ColumnONHEADCLICK( n ) := {| nCol | SortThisColumn( aData, nCol ) }
      NEXT

   END WINDOW

   CENTER WINDOW Form_1

   ACTIVATE WINDOW Form_1

RETURN NIL


PROCEDURE QueryTest( aArray )

   IF This.QueryColIndex == 1

      IF aArray[ This.QueryRowIndex ][ This.QueryColIndex ]
         This.QueryData := 1
      ELSE
         This.QueryData := 0
      ENDIF

   ELSE

      This.QueryData := aArray[ This.QueryRowIndex ][ This.QueryColIndex ]

   ENDIF

RETURN


PROCEDURE SelectRow( aArray )

   LOCAL aRow
   LOCAL nPos := This.VALUE

   IF nPos != 0

      aRow := aArray[ nPos ]
      IF aRow[ 1 ]
         aRow[ 1 ] := .F.
      ELSE
         aRow[ 1 ] := .T.
      ENDIF
      aArray[ nPos ] := aRow

      DoMethod( "Form_1", "Grid_1", "Refresh" )

   ENDIF

RETURN


STATIC FUNCTION SortThisColumn( aArray, nEle )

   LOCAL cEle := hb_ntos( nEle )
   LOCAL cBlock

   STATIC lAscend := .T.

   cBlock := "{|x, y| x[" + cEle + "]" + iif( lAscend, "<", ">" ) + "y[" + cEle + "]}"

#ifndef __XHARBOUR__
   ASort( aArray, , , Eval( hb_macroBlock( cBlock ) ) )
#else
   ASort( aArray, , , &( cBlock ) )
#endif
   Form_1.Grid_1.Refresh

   lAscend := ! lAscend

RETURN NIL
