/*
 * MINIGUI - Harbour Win32 GUI library Demo
 */

#include "hmg.ch"

MEMVAR lFirst
MEMVAR lTextBoxGotFocus
MEMVAR nLen
MEMVAR aRows

FUNCTION Main()

   PRIVATE lFirst := .T.
   PRIVATE lTextBoxGotFocus := .F.
   PRIVATE nLen := 0

   PRIVATE aRows[ 20 ][ 3 ]

   aRows [ 1 ] := { 'Simpson', 'Homer', '555-5555' }
   aRows [ 2 ] := { 'Mulder', 'Fox', '324-6432' }
   aRows [ 3 ] := { 'Smart', 'Max', '432-5892' }
   aRows [ 4 ] := { 'Grillo', 'Pepe', '894-2332' }
   aRows [ 5 ] := { 'Kirk', 'James', '346-9873' }
   aRows [ 6 ] := { 'Barriga', 'Carlos', '394-9654' }
   aRows [ 7 ] := { 'Flanders', 'Ned', '435-3211' }
   aRows [ 8 ] := { 'Smith', 'John', '123-1234' }
   aRows [ 9 ] := { 'Pedemonti', 'Flavio', '000-0000' }
   aRows[ 10 ] := { 'Gomez', 'Juan', '583-4832' }
   aRows[ 11 ] := { 'Fernandez', 'Raul', '321-4332' }
   aRows[ 12 ] := { 'Borges', 'Javier', '326-9430' }
   aRows[ 13 ] := { 'Alvarez', 'Alberto', '543-7898' }
   aRows[ 14 ] := { 'Gonzalez', 'Ambo', '437-8473' }
   aRows[ 15 ] := { 'Batistuta', 'Gol', '485-2843' }
   aRows[ 16 ] := { 'Vinazzi', 'Amigo', '394-5983' }
   aRows[ 17 ] := { 'Pedemonti', 'Flavio', '534-7984' }
   aRows[ 18 ] := { 'Samarbide', 'Armando', '854-7873' }
   aRows[ 19 ] := { 'Pradon', 'Alejandra', '???-????' }
   aRows[ 20 ] := { 'Reyes', 'Monica', '432-5836' }

   aSort( aRows, , , { | x, y | x[ 1 ] < y[ 1 ] })

   DEFINE WINDOW Form_1 ;
         AT 0, 0 ;
         WIDTH 568 ;
         HEIGHT 430 ;
         TITLE 'Incremental Search in Grid' ;
         MAIN ;
         FONT 'Arial' ;
         SIZE 9 ;
         ON INIT FillGrid()

      DEFINE LABEL Label_1
         ROW 10
         COL 10
         WIDTH 60
         VALUE 'Search :'
         VCENTERALIGN .T.
      END LABEL

      DEFINE TEXTBOX Text_1
         ROW 10
         COL 70
         WIDTH 150
         ONGOTFOCUS lTextBoxGotFocus := .T.
         ONLOSTFOCUS lTextBoxGotFocus := .F.
         ONCHANGE SearchChange()
      END TEXTBOX

      DEFINE GRID Grid_1
         ROW 50
         COL 10
         WIDTH 532
         HEIGHT 330
         HEADERS { 'SurName', 'Name', 'Phone' }
         WIDTHS { 180, 180, 150 }
         ONCHANGE SearchChange()
         COLUMNSORT { 1, 0, 0 }
         ONINIT HMG_SortColumn( 1 )
      END GRID

      ON KEY BACK ACTION textboxbackspace()

   END WINDOW

   CENTER WINDOW Form_1
   ACTIVATE WINDOW Form_1

RETURN NIL

/*********************************************************/
PROCEDURE SearchChange()
/*********************************************************/
   LOCAL cTxt, cArrowsFind, nX, nSelectText

   // set Insert Edit mode
   IF _HMG_IsXPorLater .AND. ! IsInsertActive()
      KeyToggleNT( VK_INSERT )
   ENDIF
   // don't process again if it is second time onchange event fired
   IF .NOT. lFirst
      lFirst := .T.
      RETURN
   ENDIF
   Form_1.Grid_1.DeleteAllItems
   cTxt := GetProperty( 'Form_1', 'Text_1', 'Value' )
   nLen := Len( cTxt )
   // don't do anything if there is no text in Text_1
   IF Len( cTxt ) == 0
      For nX := 1 To Len( aRows )
         Form_1.Grid_1.AddItem ( aRows [ nX ] )
      Next
   ELSE
      // Find text
      FOR nX := 1 TO Len( aRows )
         IF Upper( cTxt ) = Upper( Left( aRows[ nX ][ 1 ], nLen ) )
            Form_1.Grid_1.AddItem( aRows[ nX ] )
         ENDIF
      NEXT
      IF Form_1.Grid_1.ItemCount > 0
         cArrowsFind := AllTrim( Form_1.Grid_1.Cell( 1, 1 ) )
         // calculate what part of text should be selected
         nSelectText := Len( cArrowsFind )
         IF lFirst
            lFirst := .F.
         ENDIF
         Form_1.Text_1.VALUE := cArrowsFind
         Form_1.Text_1.CaretPos := nLen
         TextBoxEditSetSel( 'form_1', 'text_1', nSelectText, nLen )
      ENDIF
   ENDIF

RETURN

/*********************************************************/
PROCEDURE FillGrid()
/*********************************************************/
   Form_1.Grid_1.SetArray( aRows )

RETURN

/*********************************************************/
FUNCTION textboxbackspace
/*********************************************************/
   LOCAL cText, cTxt, nX

   IF lTextBoxGotFocus
      nLen--
      cText := GetProperty( 'Form_1', 'Text_1', 'Value' )
      TextBoxEditSetSel( 'Form_1', 'Text_1', Len( cText ), nLen )
      cTxt := Upper( Left( cText, nLen ) )
      Form_1.Grid_1.DeleteAllItems()
      FOR nX := 1 TO Len( aRows )
         IF Upper( cTxt ) == Upper( Left( aRows[ nX ][ 1 ], nLen ) )
            Form_1.Grid_1.AddItem ( aRows[ nX ] )
         ENDIF
      NEXT
   ENDIF
   IF nLen == 0
      Form_1.Text_1.VALUE := ''
   ENDIF

RETURN NIL

#define EM_SETSEL  177
/*********************************************************/
FUNCTION TextBoxEditSetSel( cParent, cControl, nStart, nEnd )
/*********************************************************/
   SendMessage( GetControlHandle( cControl, cParent ), EM_SETSEL, nStart, nEnd )

RETURN NIL
