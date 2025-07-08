/*
 * MiniGUI USERTYPE Grid Demo 2
 * by Adam Lubszczyk <adam_l@poczta.onet.pl>
 *
 * Enhanced by Ivanil Marcelino <ivanil@linkbr.com.br>
*/

#include "minigui.ch"

STATIC aCol1Rows := { 1, 1, 1 } // value of 1-st column

PROCEDURE Main

   DEFINE WINDOW Form_1 ;
      AT 0, 0 ;
      WIDTH 640 ;
      HEIGHT 400 ;
      TITLE "DYNAMIC in GRID INPLACE EDIT" ;
      MAIN

   DEFINE STATUSBAR
      STATUSITEM ""
   END STATUSBAR

   @ 10, 10 GRID Grid_1 ;
      WIDTH 620 ;
      HEIGHT 330 ;
      HEADERS { 'Column 1 (COMBOBOX)', 'Column 2 (DYNAMIC)', 'Info (No Edit)' } ;
      ON CHANGE SetStatusMsg(Form_1.Grid_1.item(Form_1.Grid_1.value)[3]) ;
      WIDTHS { 160, 160, 200 } ;
      ITEMS { { 1, 1, 'row 1 - variable COMBOBOX' }, ;
              { 1, 1, 'row 2 - variable COMBOBOX' }, ;
              { 1, 1, 'row 3 - variable types' } ;
            } ;
      EDIT ;
      COLUMNWHEN { {|| .T. }, {|| .T. }, {|| .F. } } ;
      INPLACE { ;
         { 'COMBOBOX', { "Number", "Words", "Logic" },{|v|SetStatusMsg(v) }}, ; /*A more generic function to simulate onchange*/
         { 'DYNAMIC', {| r, c| MyDynEdit( r, c ) } ,{|v|SetStatusMsg(v)}} ;  /*A more generic function to simulate onchange*/
      } ;
      COLUMNVALID { {|| Col1Change() } }

   END WINDOW

   CENTER WINDOW Form_1

   ACTIVATE WINDOW Form_1

RETURN

// ***********************************
// Function used by codeblock from 'DYNAMIC'
// return normal array used in INPLACE EDIT
FUNCTION MyDynEdit( nRow, nCol )

   LOCAL aRet
   LOCAL nOpt1

   HB_SYMBOL_UNUSED( nCol )

   nOpt1 := aCol1Rows[ nRow ] // selected option in column 1

   /*Simulation of function at the individual level overlapping a generic.*/

   IF nRow == 1 .OR. nRow == 2
      IF nOpt1 == 1 // Number
         aRet := { 'COMBOBOX', { "1", "2", "3" } ,{|v|SetStatusMsg(v)}}
      ELSEIF nOpt1 == 2 // Words
         aRet := { 'COMBOBOX', { "Harbour", "Mini", "GUI", "User" },{|v|SetStatusMsg(v)} }
      ELSEIF nOpt1 == 3 // Logic
         aRet := { 'COMBOBOX', { "True", "False" },{||SetStatusMsg(This.value)} }
      ENDIF
   ENDIF

   IF nRow == 3
      IF nOpt1 == 1 // Number
         aRet := { 'SPINNER', -10, 10,{|v|SetStatusMsg(v)} }
      ELSEIF nOpt1 == 2 // Words
         aRet := { 'COMBOBOX', { "Harbour", "Mini", "GUI", "User" } ,{|v|SetStatusMsg(v)}}
      ELSEIF nOpt1 == 3 // Logic
         aRet := { 'CHECKBOX', "True", "False",{|v|SetStatusMsg(v)} }
      ENDIF
   ENDIF

RETURN aRet

// *******************************
FUNCTION Col1Change()

   LOCAL r := This.CellRowIndex
   LOCAL v := This.CellValue

   // update public array with copy of values column 1
   aCol1Rows[ r ] := v

RETURN .T.

// *******************************
FUNCTION SetStatusMsg( cMsg )

   Form_1.StatusBar.Item( 1 ) := cValToChar( cMsg )

RETURN .T.
