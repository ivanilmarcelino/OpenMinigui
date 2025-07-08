/*
  Name: SET SHOWDETAILERROR ON | OFF Demo
  Description: Enable/Disable "Show Detail Error"
  Note: The change of the switch on|off function "on the fly"
  Author: Antonio Novo <antonionovo@gmail.com>
  Date Created: 14/11/2005
  Modified: 05/04/2024
*/

#define _HMG_OUTLOG
#include "minigui.ch"

#translate SetInitErrorTitle( <bAction> )  => _HMG_bOnErrorInit := <bAction>
#translate SetPostErrorAction( <bAction> ) => _HMG_bOnErrorExit := <bAction>

// --------------------------------------------------------------------
FUNCTION Main( NoShowRedAlert )

   LOCAL Pepe

   Set ShowDetailError OFF
   Set ShowRedAlert ON

   IF hb_defaultValue( NoShowRedAlert, .F. )
      SET ShowRedAlert OFF
   ELSE
      SetInitErrorTitle( {| v | v += "A Run-Time Error Has Occured !;", ;
         v += "*** IT IS NECESSARY TO CONTACT THE PROGRAMMER ***;;", v } )
      SetPostErrorAction( hb_macroBlock( "MyExitError()" ) )
   ENDIF

   DEFINE WINDOW Form1 ;
         WIDTH 400 ;
         HEIGHT 200 ;
         TITLE "SET SHOWDETAILERROR ON | OFF Demo" ;
         MAIN ;
         NOMAXIMIZE NOSIZE

      @ 10, 10 BUTTON Button1 CAPTION "Set ShowDetailError On" ;
         ACTION {|| _lShowDetailError( .T. ), Date() == 1 } WIDTH 200 HEIGHT 40
      @ 50, 10 BUTTON Button2 CAPTION "Set ShowDetailError Off" ;
         ACTION {|| _lShowDetailError( .F. ), Pepe++ } WIDTH 200 HEIGHT 40
      @ 90, 10 BUTTON Button3 CAPTION "Show Detail Error Status" ;
         ACTION {|| MsgInfo( "Detail Error is " + iif( _lShowDetailError(), "ON", "OFF" ) ) } WIDTH 200 HEIGHT 40

   END WINDOW

   CENTER WINDOW Form1

   ACTIVATE WINDOW Form1

RETURN NIL

// --------------------------------------------------------------------
FUNCTION MyExitError() // call my function on program termination error

   LOCAL n

   ? "=====> ERROR program ! MyExitError() <====="
   FOR n := 0 TO 9
      ? ProcNL( n )
   NEXT

   dbCloseAll()

RETURN NIL

// --------------------------------------------------------------------
FUNCTION ProcNL( nVal )

RETURN "Called from: " + ProcName( ++nVal ) + "(" + hb_ntos( ProcLine( nVal ) ) + ") --> " + ProcFile( nVal )
