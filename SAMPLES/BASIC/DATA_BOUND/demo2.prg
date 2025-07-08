/*
 * MiniGUI DATA-BOUND Controls Demo
 * (c) 2003 Roberto Lopez
 *
 * Revised by Grigory Filatov, 2006-2021
 */

#include "hmg.ch"

STATIC FirstEdit

PROCEDURE Main

   SET DELETED ON

   SET FONT TO _GetSysFont(), 11

   DEFINE WINDOW Win_1 ;
      WIDTH 428 ;
      HEIGHT 400 ;
      TITLE 'Data-Bound Controls Test' ;
      MAIN ;
      NOMAXIMIZE ;
      NOSIZE ;
      ON INIT OpenTables() ;
      ON RELEASE CloseTables()

      CREATE_CONTROLS()

   END WINDOW

   CREATE_TOOLBAR()

   ACTIVATE WINDOW Win_1 ON INIT This.Center()

RETURN


PROCEDURE CREATE_TOOLBAR

   DEFINE TOOLBAR NUL OF Win_1 BUTTONSIZE 20, 20 FLAT BOTTOM

   BUTTON TOP ;
      TOOLTIP '&Top' ;
      PICTURE 'primero.bmp' ;
      ACTION  ( TEST->( dbGoTop() ), Refresh() )

   BUTTON PREVIOUS ;
      TOOLTIP '&Previous' ;
      PICTURE 'anterior.bmp' ;
      ACTION  ( TEST->( dbSkip( -1 ) ), Refresh() )

   BUTTON NEXT ;
      TOOLTIP '&Next' ;
      PICTURE 'siguiente.bmp' ;
      ACTION  ( TEST->( dbSkip( 1 ) ), iif ( TEST->( Eof() ), TEST->( dbGoBottom() ), Nil ), Refresh() )

   BUTTON BOTTOM ;
      TOOLTIP '&Bottom' ;
      PICTURE 'ultimo.bmp' ;
      ACTION  ( TEST->( dbGoBottom() ), Refresh() ) GROUP

   BUTTON ADD ;
      TOOLTIP '&Append' ;
      PICTURE 'agregar.bmp' ;
      ACTION  New()

   BUTTON DEL ;
      TOOLTIP '&Delete' ;
      PICTURE 'suprimir.bmp' ;
      ACTION  DelRec() GROUP

   BUTTON SAVE ;
      TOOLTIP '&Save' ;
      PICTURE 'guardar.bmp' ;
      ACTION  Save()

   BUTTON UNDO ;
      TOOLTIP '&Undo' ;
      PICTURE 'deshacer.bmp' ;
      ACTION  Refresh()

   END TOOLBAR

RETURN


PROCEDURE CREATE_CONTROLS

   LOCAL Ctrl, cWindowName := ThisWindow.Name
   LOCAL lChecked
   LOCAL nEditHeight := _HMG_DefaultFontSize * 2 - 1

   @  10, 20 LABEL NUL VALUE 'EDIT test' WIDTH 380 CENTERALIGN

   @  60, 20 LABEL NUL VALUE 'Code:' WIDTH 100 RIGHTALIGN
   @  90, 20 LABEL NUL VALUE 'First Name:' WIDTH 100 RIGHTALIGN
   @ 120, 20 LABEL NUL VALUE 'Last Name:' WIDTH 100 RIGHTALIGN
   @ 150, 20 LABEL NUL VALUE 'Birth Date:' WIDTH 100 RIGHTALIGN
   @ 180, 20 LABEL NUL VALUE 'Married:' WIDTH 100 RIGHTALIGN
   @ 208, 20 LABEL NUL VALUE 'Bio:' WIDTH 100 RIGHTALIGN

   @ 60, 130 TEXTBOX NUL;
      WIDTH 100 ;
      HEIGHT nEditHeight ;
      FIELD TEST->CODE ;
      NUMERIC ;
      MAXLENGTH 10 ;
      ON GOTFOCUS ( FirstEdit := This.Name, DrawRR ( .T. ) ) ;
      ON LOSTFOCUS DrawRR( .F. ) ;
      BACKCOLOR WHITE NOBORDER

   @ 90, 130 TEXTBOX NUL;
      WIDTH 250 ;
      HEIGHT nEditHeight ;
      FIELD TEST->FIRST ;
      MAXLENGTH 30 ;
      ON GOTFOCUS DrawRR ( .T. ) ;
      ON LOSTFOCUS DrawRR( .F. ) ;
      BACKCOLOR WHITE NOBORDER

   @ 120, 130 TEXTBOX NUL;
      WIDTH 250 ;
      HEIGHT nEditHeight ;
      FIELD TEST->LAST ;
      MAXLENGTH 30 ;
      ON GOTFOCUS DrawRR ( .T. ) ;
      ON LOSTFOCUS DrawRR( .F. ) ;
      BACKCOLOR WHITE NOBORDER

   @ 150, 130 DATEPICKER NUL ;
      WIDTH 130 ;
      HEIGHT nEditHeight ;
      FIELD TEST->BIRTH ;
      SHOWNONE ;
      ON GOTFOCUS DrawRR ( .T. ) ;
      ON LOSTFOCUS DrawRR( .F. )

   @ 180, 130 SWITCHER NUL ;
      HEIGHT 46 IMAGE { 'MINIGUI_SWITCH_ON', 'MINIGUI_SWITCH_OFF' } ;
      LEFTCHECK ;
      FIELD TEST->MARRIED ;
      ONCLICK ( Ctrl := This.Name, lChecked := Win_1.(Ctrl).Checked, Win_1.(Ctrl).Value := iif( lChecked, 'No', 'Yes' ), ;
         Win_1.(Ctrl).Checked := ! lChecked )
/*
   DEFINE SWITCHER CHECK_5
	ROW	180
	COL	130
	LEFTCHECK .T.
	FIELD TEST->MARRIED
	ONCLICK ( lChecked := Win_1.Check_5.Checked, Win_1.Check_5.Value := iif( lChecked, 'No', 'Yes' ), ;
		Win_1.Check_5.Checked := !lChecked )
   END SWITCHER

   @ 208, 130 EDITBOX EDIT_6 ;
      WIDTH 250 ;
      FIELD TEST->BIO ;
      HEIGHT 100 ;
      NOHSCROLL ;
      ON GOTFOCUS DrawRR ( .T. ) ;
      ON LOSTFOCUS DrawRR( .F. ) ;
      BACKCOLOR WHITE ;
      MAXLENGTH 1024*1024 NOBORDER
*/
   DEFINE EDITBOX NUL
      ROW 208
      COL 130
      WIDTH 250
      FIELD TEST->BIO
      HEIGHT 100
      NOHSCROLLBAR .T.
      ON GOTFOCUS DrawRR ( .T. )
      ON LOSTFOCUS DrawRR( .F. )
      BACKCOLOR WHITE
      MAXLENGTH 1024*1024
      NOBORDER .T.
   END EDITBOX

   FOR EACH Ctrl IN HMG_GetFormControls( cWindowName )

      IF ! ( "LABEL" $ GetControlType( Ctrl, cWindowName ) .OR. "CHECK" $ GetControlType( Ctrl, cWindowName ) )
         DrawRR( , This.&(Ctrl).Row, This.&(Ctrl).Col, This.&(Ctrl).Height, This.&(Ctrl).Width )
      ENDIF

   NEXT

RETURN


PROCEDURE DrawRR( focus, t, l, b, r, cWindowName, nCurve )

   LOCAL aColor

   DEFAULT t := This.Row, l := This.Col, b := This.Height, r := This.Width
   DEFAULT focus := .F., cWindowName := ThisWindow.Name, nCurve := 10

   IF ISARRAY( focus )
      aColor := focus
   ELSE
      aColor := iif( focus, { 0, 120, 215 }, { 100, 100, 100 } )
   ENDIF

   DRAW ROUNDRECTANGLE IN WINDOW (cWindowName) ;
      AT t - 2, l - 2 TO t + b + 2, l + r + 2 ;
      ROUNDWIDTH nCurve ;
      ROUNDHEIGHT nCurve ;
      PENCOLOR aColor

RETURN


PROCEDURE Refresh

   LOCAL Ctrl, cWindowName := ThisWindow.Name

   FOR EACH Ctrl IN HMG_GetFormControls( cWindowName )

      IF ( "EDIT" $ GetControlType( Ctrl, cWindowName ) .OR. ;
         "TEXT" $ GetControlType( Ctrl, cWindowName ) .OR. ;
         "DATE" $ GetControlType( Ctrl, cWindowName ) .OR. ;
         "CHECK" $ GetControlType( Ctrl, cWindowName ) )
         Win_1.(Ctrl).Refresh
      ENDIF

      IF "CHECK" $ GetControlType( Ctrl, cWindowName )
         Win_1.(Ctrl).Value := iif( TEST->MARRIED, 'Yes', 'No' )
      ENDIF

   NEXT

   Win_1.(FirstEdit).SetFocus

RETURN


PROCEDURE Save

   LOCAL Ctrl, cWindowName := ThisWindow.Name

   IF TEST->( NetRecLock() )

      FOR EACH Ctrl IN HMG_GetFormControls( cWindowName )

         IF ( "EDIT" $ GetControlType( Ctrl, cWindowName ) .OR. ;
            "TEXT" $ GetControlType( Ctrl, cWindowName ) .OR. ;
            "DATE" $ GetControlType( Ctrl, cWindowName ) .OR. ;
            "CHECK" $ GetControlType( Ctrl, cWindowName ) )
            This.&(Ctrl).Save
         ENDIF

      NEXT

      TEST->( dbRUnLock() )

   ENDIF

   Refresh()

RETURN


PROCEDURE New

   LOCAL n

   TEST->( dbGoBottom() )

   n := TEST->CODE

   TEST->( NetAppend() )

   TEST->CODE := ++n

   Refresh()

RETURN


PROCEDURE DelRec

   IF TEST->( NetDelete() )

      TEST->( dbRUnLock() )

   ENDIF

   WHILE TEST->( Deleted() )

      TEST->( dbSkip( -1 ) )

   END

   Refresh()

RETURN


PROCEDURE OpenTables

   LOCAL Ctrl, cWindowName := ThisWindow.Name

   USE TEST SHARED

   INDEX ON FIELD->CODE TO TEST MEMORY

   GO TOP

   FOR EACH Ctrl IN HMG_GetFormControls( cWindowName )

      IF "CHECK" $ GetControlType( Ctrl, cWindowName )
         Win_1.(Ctrl).Value := iif( TEST->MARRIED, 'Yes', 'No' )
      ENDIF

   NEXT

   SELECT 0

RETURN


PROCEDURE CloseTables

   CLOSE test

RETURN
