/*
 * HMG - Harbour Win32 GUI library Demo
 *
 * Copyright 2014 Dr. Claudio Soto <srvet@adinet.com.uy>
 */

#include "hmg.ch"


Function Main

   LOCAL b := { || iif ( This.CellValue == 0 , RED , BLACK ) }
   LOCAL aItems := {}

   AADD (aItems, {"Carrot",        5, "A"})
   AADD (aItems, {"Cauliflower",   0, "B"})
   AADD (aItems, {"Corn",         15, "C"})
   AADD (aItems, {"Tomato",        0, "D"})
   AADD (aItems, {"Zucchini",     20, "E"})

   DEFINE FONT fItalic FONTNAME "Arial" SIZE 12 ITALIC UNDERLINE
   DEFINE FONT fBold FONTNAME "Calibri" SIZE 12 BOLD

   DEFINE WINDOW Form_1 ;
      AT 0,0 ;
      WIDTH 600 ;
      HEIGHT 400 ;
      MAIN 

      @ 10,10 GRID Grid_1 ;
         WIDTH 550 ;
         HEIGHT 330 ;
         HEADERS {'Product','Stock','Supplier'} ;
         WIDTHS {250,150,100};
         ITEMS aItems;
         DYNAMICFORECOLOR { , b , } ;
         EDIT;
         CELLNAVIGATION;
         COLUMNCONTROLS { NIL, {'TEXTBOX','NUMERIC'}, NIL } ;
         FONT "Calibri" SIZE 11

         Form_1.Grid_1.ColumnJUSTIFY (2) := GRID_JTFY_RIGHT
         Form_1.Grid_1.ColumnJUSTIFY (3) := GRID_JTFY_CENTER

         // Dynamic Header
         Form_1.Grid_1.HeaderDYNAMICFONT (1) := {|| "fItalic" }
         Form_1.Grid_1.HeaderDYNAMICFONT (3) := {|| "fBold" }

         Form_1.Grid_1.HeaderDYNAMICFORECOLOR (1) := {|| HeaderForeColor() }
         Form_1.Grid_1.HeaderDYNAMICFORECOLOR (2) := {|| HeaderForeColor() }
         Form_1.Grid_1.HeaderDYNAMICFORECOLOR (3) := {|| HeaderForeColor() }

   END WINDOW

   CENTER WINDOW Form_1

   ACTIVATE WINDOW Form_1

Return Nil


Function HeaderForeColor

   LOCAL aColor

   IF This.CellColIndex == 1
      aColor := BLUE
   ELSEIF This.CellColIndex == 2
      aColor := RED
   ELSE
      aColor := NIL
   ENDIF

Return aColor
