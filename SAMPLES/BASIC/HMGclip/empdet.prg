//
// Samples of the Forms
//

#include "minigui.ch"

DECLARE WINDOW Main

PROCEDURE empdet

   LOCAL cName       := 'Fred Bloggs Jr.'
   LOCAL dDOB        := Date() -20000
   LOCAL aStatus     := { 'Unknown', 'Married', 'Single' }
   LOCAL cStatus     := 'Single'
   LOCAL nSalary     := 95000
   LOCAL lEmployed   := .F.

   DEFINE WINDOW EmpWin ;
      AT Main.Row + GetTitleHeight() + GetMenuBarHeight() + 22, Main.Col + 28 ;
      WIDTH 376 HEIGHT 278 ;
      TITLE "Edit Employee Details" ;
      MODAL ;
      FONT "Arial" SIZE 10

   @ 01, 08 LABEL Label_0 VALUE "Employee Details" FONT "Arial Bold" SIZE 12 AUTOSIZE FONTCOLOR GRAY

   @ 50, 10  LABEL Label_1 VALUE "Name:" WIDTH 100 RIGHTALIGN VCENTERALIGN FONTCOLOR GRAY
   @ 50, 120 GETBOX Edit_1 VALUE cName PICTURE '@K' FONT "Arial" SIZE 12 ON LOSTFOCUS cName := This.Value

   @ 75, 10  LABEL Label_2 VALUE "Birth Date:" WIDTH 100 RIGHTALIGN VCENTERALIGN FONTCOLOR GRAY
   @ 75, 120 GETBOX Edit_2 VALUE dDOB PICTURE '@KD' FONT "Arial" SIZE 12 ON LOSTFOCUS dDOB := This.Value

   @ 100, 10  LABEL Label_3 VALUE "Status:" WIDTH 100 RIGHTALIGN VCENTERALIGN FONTCOLOR GRAY
   @ 100, 120 COMBOBOX Edit_3 ITEMS aStatus VALUE AScan( aStatus, cStatus ) ON LOSTFOCUS cStatus := aStatus[This.Value]

   @ 125, 10  LABEL Label_4 VALUE "Salary:" WIDTH 100 RIGHTALIGN VCENTERALIGN FONTCOLOR GRAY
   @ 125, 120 GETBOX Edit_4 VALUE nSalary PICTURE '@K 999,999' FONT "Arial" SIZE 12 ON LOSTFOCUS nSalary := This.Value

   @ 150, 120 CHECKBOX Edit_5 CAPTION "Employed?" VALUE lEmployed ON LOSTFOCUS lEmployed := This.Value

   @ 200, 14  BUTTON Button_1 CAPTION "OK" WIDTH 70 HEIGHT 24 ;
              ACTION  {|| AlertInfo( cName + ';' + ;
                          DToC( dDOB ) + ';' + ;
                          iif( ValType( cStatus ) == 'N', Str( cStatus ), cStatus ) + ';' + ;
                          Str( nSalary ) + ';' + ;
                          iif( lEmployed, 'Y', 'N' ), ;
                          'Information' ), EmpWin.Release }

   @ 200, 14+78  BUTTON Button_2 CAPTION "Notes" WIDTH 70 HEIGHT 24 ;
              ACTION  {|| empnote() }

   @ 200, 14+2*78  BUTTON Button_3 CAPTION "Cancel" WIDTH 70 HEIGHT 24 ;
              ACTION  {|| EmpWin.Release }

   ON KEY ESCAPE ACTION EmpWin.Release

   END WINDOW

   EmpWin.Activate()

RETURN


FUNCTION empnote()

   LOCAL cMemo := 'These are employee notes. Asdasd asdasdasd asdas as dasdasd asdasd asdas dasd asdas dasd.'

   DEFINE WINDOW Notes ;
      AT EmpWin.Row + GetTitleHeight() + 22, EmpWin.Col + 28 ;
      WIDTH 376 HEIGHT 278 ;
      TITLE "Edit Employee Notes" ;
      MODAL ;
      FONT "Arial" SIZE 10

   @ 25, 5  LABEL Label_1 VALUE "Notes:" WIDTH 45 FONTCOLOR GRAY
   @ 25, 50 EDITBOX Edit_1 VALUE cMemo WIDTH 295 HEIGHT 145 NOHSCROLL

   @ 200, 50  BUTTON Button_1 CAPTION "OK" WIDTH 70 HEIGHT 24 ;
              ACTION  {|| cMemo := Notes.Edit_1.Value, ;
                          AlertInfo( cMemo, 'Save' ), Notes.Release }

   @ 200, 50+78  BUTTON Button_2 CAPTION "Cancel" WIDTH 70 HEIGHT 24 ;
              ACTION  {|| Notes.Release }

   @ 200, 50+2*78  BUTTON Button_3 CAPTION "Today" WIDTH 70 HEIGHT 24 ;
              ACTION  {|| EBPaste( Notes.Edit_1.Value, '[' + SubStr( CDoW( Date() ), 1, 3 ) + ' ' + DToS( Date() ) + ' ' + SubStr( Time(), 1, 5 ) + ']' ) }

   ON KEY ESCAPE ACTION Notes.Release

   END WINDOW

   Notes.Activate()

RETURN NIL


FUNCTION EBPaste( cValue, cText )

   Notes.Edit_1.Value := cText + cValue

RETURN NIL


// The example demonstrates how radio button groups can be integrated
// in the Get list and are filled with radio buttons.
FUNCTION RadioButton()

   LOCAL aRadio1[3]
   LOCAL aRadio2[4]
   LOCAL cName := "Fred's Brother        "
   LOCAL nRB1, nRB2

   DEFINE WINDOW RButtons ;
      AT Main.Row + GetTitleHeight() + GetMenuBarHeight() + 68, Main.Col + 42 ;
      WIDTH 376 HEIGHT 446 ;
      TITLE "Radio Buttons" ;
      MODAL ;
      FONT "Arial" SIZE 10 ;
      ON INIT RButtons.Radio_1.Setfocus()

   @ 40, 85 FRAME Frame_1 WIDTH 80 HEIGHT 95 CAPTION "Colour" FONTCOLOR GRAY

   aRadio1[ 1 ] := "Red"
   aRadio1[ 2 ] := "Green"
   aRadio1[ 3 ] := "Blue"

   nRB1 := 1            //default to first item.

   DEFINE RADIOGROUP Radio_1
	ROW 55
	COL 95
	OPTIONS aRadio1
	VALUE nRB1
	WIDTH 60
	FONTCOLOR BLACK
	TOOLTIP 'Radio Group Control 1'
	ON CHANGE nRB1 := This.Value
   END RADIOGROUP

   @ 135, 10 LABEL Label_1 VALUE "Purser:" WIDTH 65 RIGHTALIGN FONTCOLOR GRAY
   @ 135, 85 GETBOX Edit_1 VALUE cName WIDTH 220 PICTURE '@K' FONT "Arial" SIZE 12

   @ 165, 180 FRAME Frame_2 WIDTH 90 HEIGHT 120 CAPTION "Note" FONTCOLOR GRAY

   aRadio2[ 1 ] := "Five"
   aRadio2[ 2 ] := "Ten"
   aRadio2[ 3 ] := "Twnety"
   aRadio2[ 4 ] := "Fifty"

   nRB2 := 2            //default to 2nd item.

   DEFINE RADIOGROUP Radio_2
	ROW 180
	COL 190
	OPTIONS aRadio2
	VALUE nRB2
	WIDTH 60
	FONTCOLOR BLACK
	TOOLTIP 'Radio Group Control 2'
	ON CHANGE nRB2 := This.Value
   END RADIOGROUP

   @ 360, 85  BUTTON Button_1 CAPTION "Save" WIDTH 70 HEIGHT 24 ;
              ACTION  {|| AlertInfo( 'Colour' + Str( nRB1 ) + ';Note' + Str( nRB2 ) ), RButtons.Release }

   @ 360, 85+78  BUTTON Button_2 CAPTION "Cancel" WIDTH 70 HEIGHT 24 ;
              ACTION  {|| RButtons.Release }

   ON KEY ESCAPE ACTION RButtons.Release

   END WINDOW

   RButtons.Activate()

RETURN NIL


// EOF: EMPDET.PRG
