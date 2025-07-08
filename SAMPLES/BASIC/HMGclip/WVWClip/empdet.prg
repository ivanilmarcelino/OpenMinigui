//
// Copyright 2016 Ashfaq Sial
//

#include "inkey.ch"
#include "wvwstd.ch"

PROCEDURE empdet

   LOCAL nWinNum
   LOCAL GetList     := {}

   LOCAL cName       := 'Fred Bloggs Jr.'
   LOCAL dDOB        := Date() -20000
   LOCAL cStatus     := 'Single'
   LOCAL nSalary     := 95000
   LOCAL lEmployed   := .F.
   LOCAL lSave       := .F.

   LOCAL nBtn        := 0

   nWinNum := WOpen( 2, 3, 11, 38, "Edit Employee Details", GetList )

   @ 00, 01 SAY 'Employee Details' FONT { 'Arial Bold', 20 } COLOUR 'N+/W'

   @ 02, 12 GET cName      CAPTION 'Name:'         PICTURE '@K'
   @ 03, 12 GET dDOB       CAPTION 'Birth Date:'   PICTURE '@D'
   @ 04, 12, 08, 19 GET cStatus LISTBOX { 'Unknown', 'Married', 'Single' } ;
         DROPDOWN CAPTION 'Status:'
   @ 05, 12 GET nSalary    CAPTION 'Salary:'       PICTURE '999,999'
   @ 06, 12 GET lEmployed  CHECKBOX CAPTION 'Employed?'

   @ 08, 02 GET lSave      PUSHBUTTON  CAPTION 'OK' ;
         STATE {|| nBtn := 1, hb_keyPut( K_CTRL_W ) }

   @ 08, 10 GET lSave      PUSHBUTTON  CAPTION 'Notes' ;
         STATE {|| empnote() }

   @ 08, 18 GET lSave      PUSHBUTTON  CAPTION 'Cancel' ;
         STATE {|| hb_keyPut( K_ESC ) }

   READ

   DO CASE

   CASE nBtn == 1
      Alert( cName + ';' + ;
         DToC( dDOB ) + ';' + ;
         iif( ValType( cStatus ) == 'N', Str( cStatus ), cStatus ) + ';' + ;
         Str( nSalary ) + ';' + ;
         iif( lEmployed, 'Y', 'N' ), ;
         'Information' )

   ENDCASE

   WClose( nWinNum )

   RETURN


FUNCTION empnote()

   LOCAL nWinNum
   LOCAL GetList     := {}
   LOCAL lSave       := .F.

   LOCAL nBtn := 0
   LOCAL cMemo := 'These are employee notes. Asdasd asdasdasd asdas as dasdasd asdasd asdas dasd asdas dasd.'

   nWinNum := WOpen( 4, 5, 13, 40, "Edit Employee Notes.", GetList )

   @ 1, 5, 6, 35 GET cMemo EDITBOX CAPTION 'Notes:' FONT { "Arial", 16 }

   @ 8,  5 GET lSave      PUSHBUTTON  CAPTION 'OK' ;
         STATE {|| nBtn := 1, hb_keyPut( K_CTRL_W ) }

   @ 8, 13 GET lSave      PUSHBUTTON  CAPTION 'Cancel' ;
         STATE {|| hb_keyPut( K_ESC ) }

   @ 8, 21 GET lSave      PUSHBUTTON  CAPTION 'Today' ;
         STATE {|| EBPaste( GetList[1], '[' + SubStr( CDoW( Date() ), 1, 3 ) + ' ' + DToS( Date() ) + ' ' + SubStr( Time(), 1, 5 ) + ']' ) }

   READ SAVE

   IF nBtn == 1
      cMemo :=  GetList[1]:cargo:GetText()
      Alert( cMemo, 'Save' )
   ENDIF

   WClose( nWinNum )

   RETURN NIL

FUNCTION EBPaste( oGet, cText )

   oGet:cargo:SetFocus()
   oGet:cargo:PasteText( cText )

   RETURN NIL


// The example demonstrates how radio button groups can be integrated
// in the Get list and are filled with radio buttons.
FUNCTION RadioButton()

   LOCAL nWinNum
   LOCAL GetList     := {}

   LOCAL aRadio1[3]
   LOCAL aRadio2[4]
   LOCAL cName := "Fred's Brother        "
   LOCAL nBtn := 0, nRB1, nRB2
   LOCAL lSave := .F.

   nWinNum := WOpen( 4, 5, 20, 40, "Radio Buttons", GetList )

   aRadio1[ 1 ] := "Red"
   aRadio1[ 2 ] := "Green"
   aRadio1[ 3 ] := "Blue"

   nRB1 := 1            //default to first item.

   aRadio2[ 1 ] := "Five"
   aRadio2[ 2 ] := "Ten"
   aRadio2[ 3 ] := "Twnety"
   aRadio2[ 4 ] := "Fifty"

   nRB2 := 2            //default to 2nd item.

   @  2, 8,  4, 14 GET nRB1 ;
             RADIOGROUP aRadio1 ;
                CAPTION "Colour" ;
                OFFSET {-5,-2,2,2}

   @  5, 8 GET cName CAPTION 'Purser:'

   @  7, 18,  10, 25 GET nRB2 ;
             RADIOGROUP aRadio2 ;
                CAPTION "Note" ;
                OFFSET {-5,-2,2,2}

   @ 15,  8 GET lSave      PUSHBUTTON  CAPTION 'Save' ;
         STATE {|| nBtn := 1, hb_keyPut( K_CTRL_W ) }

   @ 15, 16 GET lSave      PUSHBUTTON  CAPTION 'Cancel' ;
         STATE {|| hb_keyPut( K_ESC ) }

   READ

   IF nBtn == 1
      Alert( 'Colour'+Str( nRB1 )+ ';Note'+str(nRB2) )
   ENDIF

   WClose( nWinNum )

   RETURN NIL


// EOF: EMPDET.PRG

