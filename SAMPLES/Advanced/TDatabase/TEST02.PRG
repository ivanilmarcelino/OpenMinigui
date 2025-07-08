/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Program : Test02.prg
 * Purpose : A MDI Browse with fields from a related file
 * Notes   : I expect the seek() method is faster since you are only
 *           keeping one record in synch.
*/

// An example relation in Clipper
/*
use arcust new
index on custno to temp
use invmast new
set relation to custno into arcust

list invmast->invno,invmast->custno,arcust->custno,arcust->company

*/


#include "minigui.ch"


FUNCTION Main()

   DEFINE WINDOW Form_1 ;
         CLIENTAREA 640, 480 ;
         TITLE 'MDI demo' ;
         MAIN ;
         MDI ;
         NOMAXIMIZE NOSIZE ;
         ON INIT InvMDI()

   END WINDOW

   CENTER WINDOW Form_1
   ACTIVATE WINDOW Form_1

RETURN NIL


// --- Invoice browse
FUNCTION InvMDI()

   LOCAL a_fields, a_width, a_headers, i
   LOCAL oCust, oInvoice

   oCust := tdata():new(, "cust" )
   oCust:use()
   oCust:createIndex( "cust",, "custno" )
   oInvoice := tdata():new(, "invmast" )
   oInvoice:use()

   a_fields := {}
   FOR i := 1 TO oInvoice:lastRec()
      AAdd( a_fields, { oInvoice:invno, oInvoice:custno, ( oCust:seek( oInvoice:custno ), oCust:custno ), oCust:company } )
      oInvoice:skip()
   NEXT
   a_headers := { "Invoice", "CustNo", "CustNo", "Company" }
   a_width := { 100, 100, 100, 300 }

   DEFINE WINDOW ChildMdi ;
         TITLE "Invoice browse" ;
         MDICHILD ;
         ON INIT ( WinChildMaximize(), ResizeEdit() ) ;
         ON RELEASE ( oInvoice:end(), oCust:end() )

      @ 0, 0 GRID BrwInv ;
         WIDTH 200 ;
         HEIGHT 200 ;
         HEADERS a_headers ;
         WIDTHS a_width ;
         ITEMS a_fields

   END WINDOW

RETURN NIL


#define WM_MDIMAXIMIZE 0x0225

FUNCTION WinChildMaximize()

   LOCAL ChildHandle

   ChildHandle := GetActiveMdiHandle()
   IF AScan ( _HMG_aFormHandles, ChildHandle ) > 0
      SendMessage( _HMG_MainClientMDIHandle, WM_MDIMAXIMIZE, ChildHandle, 0 )
   ENDIF

RETURN NIL


PROCEDURE ResizeEdit()

   LOCAL ChildHandle, ChildName
   LOCAL i

   ChildHandle := GetActiveMdiHandle()
   i := AScan ( _HMG_aFormHandles, ChildHandle )
   IF i > 0
      ChildName := _HMG_aFormNames[ i ]
      ResizeChildEdit( ChildName )
   ENDIF

RETURN


PROCEDURE ResizeChildEdit( ChildName )

   LOCAL hwndCln, actpos := { 0, 0, 0, 0 }
   LOCAL i, w, h

   i := AScan ( _HMG_aFormNames, ChildName )
   IF i > 0
      hwndCln := _HMG_aFormHandles[ i ]
      GetClientRect( hwndCln, actpos )
      w := actpos[ 3 ] - actpos[ 1 ]
      h := actpos[ 4 ] - actpos[ 2 ]
      _SetControlHeight( "BrwInv", ChildName, h )
      _SetControlWidth( "BrwInv", ChildName, w )
   ENDIF

RETURN
