/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * (c) 2014 Grigory Filatov <gfilatov@inbox.ru>
 */

#include "minigui.ch"
#include "dbstruct.ch"

DECLARE WINDOW Form_PrgBar

*------------------------------------------------------------------------------*
PROCEDURE Main
*------------------------------------------------------------------------------*

   SET DEFAULT Icon TO GetStartupFolder() + "\Main.ico"

   DEFINE WINDOW Form1 ;
      AT 0, 0 ;
      WIDTH 600 HEIGHT 400 ;
      TITLE "ProgressBar Demo" ;
      MAIN ;
      ON INIT filltable( 1000 )

   DEFINE BUTTON Button_1
      ROW 20
      COL 20
      WIDTH 80
      HEIGHT 28
      CAPTION "Process 1"
      ACTION NtxCreate( FIELD( 9 ), FIELD( 9 ) )
      DEFAULT .T.
   END BUTTON

   DEFINE BUTTON Button_2
      ROW 20
      COL 120
      WIDTH 80
      HEIGHT 28
      CAPTION "Process 2"
      ACTION SkipTest()
   END BUTTON

   DEFINE BUTTON Button_3
      ROW 20
      COL 220
      WIDTH 80
      HEIGHT 28
      CAPTION "Process 3"
      ACTION ArrayTest()
   END BUTTON

   ON KEY ESCAPE ACTION ThisWindow.Release

   END WINDOW

   CENTER WINDOW Form1
   ACTIVATE WINDOW Form1

RETURN

*------------------------------------------------------------------------------*
FUNCTION NtxCreate( cField, cNtxName )
*------------------------------------------------------------------------------*

   CreateProgressBar( "Create index " + cNtxName + " for database " + Alias() + "..." )

   Form_PrgBar.Closable := .F.

   INDEX ON &cField TO ( cNtxName ) EVAL NtxProgress() EVERY LastRec() / 10

   Form_PrgBar.PrgBar_1.Value := 100
   Form_PrgBar.Label_1.Value := "Completed 100%"
   // final waiting
   INKEYGUI( 800 )

   Form_PrgBar.Closable := .T.
   SET INDEX TO

   CloseProgressBar()

RETURN NIL

*------------------------------------------------------------------------------*
FUNCTION NtxProgress()
*------------------------------------------------------------------------------*
   LOCAL nComplete := Int( ( RecNo() / LastRec() ) * 100 )

   Form_PrgBar.PrgBar_1.Value := nComplete
   Form_PrgBar.Label_1.Value := "Completed " + + hb_ntos( nComplete ) + "%"
   // refreshing
   INKEYGUI( 100 )

RETURN .T.

*------------------------------------------------------------------------------*
FUNCTION SkipTest()
*------------------------------------------------------------------------------*
   LOCAL nComplete

   CreateProgressBar( "Database processing..." )

   GO TOP
   DO WHILE !Eof()
      nComplete := Int( ( RecNo() / LastRec() ) * 100 )
      IF nComplete % 10 == 0
         IF IsWindowDefined( Form_PrgBar )
            Form_PrgBar.PrgBar_1.Value := nComplete
            Form_PrgBar.Label_1.Value := "Completed " + + hb_ntos( nComplete ) + "%"
         ELSE
            EXIT
         ENDIF
         // refreshing
         INKEYGUI()
      ENDIF
      dbSkip()
   ENDDO
   // final waiting
   INKEYGUI( 800 )

   CloseProgressBar()

RETURN NIL

*------------------------------------------------------------------------------*
FUNCTION ArrayTest()
*------------------------------------------------------------------------------*

   LOCAL nComplete
   LOCAL aTest := {}
   LOCAL n     := 1
   LOCAL nSize := 2000

   CreateProgressBar( "Array processing..." )

   DO WHILE n <= nSize
      nComplete := Int( ( n / nSize ) * 100 )
      IF nComplete % 10 == 0
         IF IsWindowDefined( Form_PrgBar )
            Form_PrgBar.PrgBar_1.Value := nComplete
            Form_PrgBar.Label_1.Value := "Completed " + + hb_ntos( nComplete ) + "%"
         ELSE
            EXIT
         ENDIF
         // refreshing
         INKEYGUI()
      ENDIF
      n++
   ENDDO
   // final waiting
   INKEYGUI( 800 )

   CloseProgressBar()

RETURN NIL

*------------------------------------------------------------------------------*
FUNCTION CreateProgressBar( cTitle )
*------------------------------------------------------------------------------*

   DEFINE WINDOW Form_PrgBar ;
      ROW 0 COL 0 ;
      WIDTH 428 HEIGHT 200 ;
      TITLE cTitle ;
      WINDOWTYPE MODAL ;
      NOSIZE ;
      FONT 'Tahoma' SIZE 11

   @ 10, 80 ANIMATEBOX Avi_1 ;
      WIDTH 260 HEIGHT 40 ;
      FILE 'filecopy.avi' ;
      AUTOPLAY TRANSPARENT NOBORDER

   @ 75, 10 LABEL Label_1 ;
      WIDTH 400 HEIGHT 20 ;
      VALUE ''            ;
      CENTERALIGN VCENTERALIGN

   @ 105, 20 PROGRESSBAR PrgBar_1 ;
      RANGE 0, 100 ;
      VALUE 0      ;
      WIDTH 380 HEIGHT 34

   END WINDOW

   Form_PrgBar.Center
   Activate Window Form_PrgBar NoWait

RETURN NIL

*------------------------------------------------------------------------------*
FUNCTION CloseProgressBar()
*------------------------------------------------------------------------------*

   IF IsWindowDefined( Form_PrgBar )
      Form_PrgBar.Release
   ENDIF

   DO MESSAGE LOOP

RETURN NIL

#translate dbCreate(<file>, <struct>) => hb_dbCreateTemp(<file>, <struct>)
*------------------------------------------------------------------------------*
PROCEDURE filltable ( nCount )
*------------------------------------------------------------------------------*
   LOCAL aDbf[ 11 ][ 4 ], i

   IF !File( 'test.dbf' )
      aDbf[ 1 ][ DBS_NAME ] := "First"
      aDbf[ 1 ][ DBS_TYPE ] := "Character"
      aDbf[ 1 ][ DBS_LEN ]  := 20
      aDbf[ 1 ][ DBS_DEC ]  := 0
      //
      aDbf[ 2 ][ DBS_NAME ] := "Last"
      aDbf[ 2 ][ DBS_TYPE ] := "Character"
      aDbf[ 2 ][ DBS_LEN ]  := 20
      aDbf[ 2 ][ DBS_DEC ]  := 0
      //
      aDbf[ 3 ][ DBS_NAME ] := "Street"
      aDbf[ 3 ][ DBS_TYPE ] := "Character"
      aDbf[ 3 ][ DBS_LEN ]  := 30
      aDbf[ 3 ][ DBS_DEC ]  := 0
      //
      aDbf[ 4 ][ DBS_NAME ] := "City"
      aDbf[ 4 ][ DBS_TYPE ] := "Character"
      aDbf[ 4 ][ DBS_LEN ]  := 30
      aDbf[ 4 ][ DBS_DEC ]  := 0
      //
      aDbf[ 5 ][ DBS_NAME ] := "State"
      aDbf[ 5 ][ DBS_TYPE ] := "Character"
      aDbf[ 5 ][ DBS_LEN ]  := 2
      aDbf[ 5 ][ DBS_DEC ]  := 0
      //
      aDbf[ 6 ][ DBS_NAME ] := "Zip"
      aDbf[ 6 ][ DBS_TYPE ] := "Character"
      aDbf[ 6 ][ DBS_LEN ]  := 10
      aDbf[ 6 ][ DBS_DEC ]  := 0
      //
      aDbf[ 7 ][ DBS_NAME ] := "Hiredate"
      aDbf[ 7 ][ DBS_TYPE ] := "Date"
      aDbf[ 7 ][ DBS_LEN ]  := 8
      aDbf[ 7 ][ DBS_DEC ]  := 0
      //
      aDbf[ 8 ][ DBS_NAME ] := "Married"
      aDbf[ 8 ][ DBS_TYPE ] := "Logical"
      aDbf[ 8 ][ DBS_LEN ]  := 1
      aDbf[ 8 ][ DBS_DEC ]  := 0
      //
      aDbf[ 9 ][ DBS_NAME ] := "Age"
      aDbf[ 9 ][ DBS_TYPE ] := "Numeric"
      aDbf[ 9 ][ DBS_LEN ]  := 2
      aDbf[ 9 ][ DBS_DEC ]  := 0
      //
      aDbf[ 10 ][ DBS_NAME ] := "Salary"
      aDbf[ 10 ][ DBS_TYPE ] := "Numeric"
      aDbf[ 10 ][ DBS_LEN ]  := 6
      aDbf[ 10 ][ DBS_DEC ]  := 0
      //
      aDbf[ 11 ][ DBS_NAME ] := "Notes"
      aDbf[ 11 ][ DBS_TYPE ] := "Character"
      aDbf[ 11 ][ DBS_LEN ]  := 70
      aDbf[ 11 ][ DBS_DEC ]  := 0

      dbCreate( "test", aDbf )
   ENDIF

   IF Select( 'test' ) == 0
      dbUseArea( .T.,, 'test' )
   ENDIF

   IF LastRec() == 0
      FOR i := 1 TO nCount
         APPEND BLANK

         REPLACE   first      WITH   'first'  + Str( i )
         REPLACE   last       WITH   'last'   + Str( i )
         REPLACE   street     WITH   'street' + Str( i )
         REPLACE   city       WITH   'city'   + Str( i )
         REPLACE   state      WITH   Chr( hb_RandomInt( 65, 90 ) ) + Chr( hb_RandomInt( 65, 90 ) )
         REPLACE   zip        WITH   AllTrim( Str( hb_RandomInt( 9999 ) ) )
         REPLACE   hiredate   WITH   Date() - 21000 + i
         REPLACE   married    WITH   ( hb_RandomInt() == 1 )
         REPLACE   age        WITH   int( ( date() - FIELD->hiredate ) / 365 )
         REPLACE   salary     WITH   hb_RandomInt( 10000 )
         REPLACE   notes      WITH   'notes' + Str( i )
      NEXT i
   ENDIF

   GO TOP

RETURN
