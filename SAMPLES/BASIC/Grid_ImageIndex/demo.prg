/*
 * HMG - Harbour Win32 GUI library Demo
 *
 * Copyright 2014 Dr. Claudio Soto <srvet@adinet.com.uy>
 */

#include "hmg.ch"


FUNCTION MAIN

   LOCAL aImages := { "_Carrot.png", "_Cauliflower.png", "_Corn.png", "_Tomato.png", "_Zucchini.png" }
   // ImageIndex -->    0              1                   2            3              4

   LOCAL b := { || iif ( This.CellValue == 0 , RED , BLACK ) }
   LOCAL aItems := {}

   AAdd ( aItems, { 0, "Carrot", 5 } )
   AAdd ( aItems, { 1, "Cauliflower", 0 } )
   AAdd ( aItems, { 2, "Corn", 15 } )
   AAdd ( aItems, { 3, "Tomato", 0 } )
   AAdd ( aItems, { 4, "Zucchini", 20 } )

   DEFINE WINDOW Form_1 ;
         WIDTH 600 ;
         HEIGHT 400 ;
         MAIN

      @ 10, 10 GRID Grid_1 ;
         WIDTH 550 ;
         HEIGHT 330 ;
         HEADERS { '', 'Product', 'Stock' } ;
         WIDTHS { 0, 250, 150 } ;
         ITEMS aItems ;
         IMAGE aImages ;
         DYNAMICFORECOLOR { , , b } ;
         EDIT ;
         CELLNAVIGATION ;
         COLUMNCONTROLS { NIL, NIL, { 'TEXTBOX', 'NUMERIC' } } ;
         FONT "Calibri" SIZE 11 BOLD ITALIC

   END WINDOW

   Form_1.Grid_1.ColumnJUSTIFY ( 3 ) := GRID_JTFY_RIGHT

   CENTER WINDOW Form_1

   ACTIVATE WINDOW Form_1

RETURN NIL
