/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Author: Igor Nazarov
 *
 * Revised by Grigory Filatov <gfilatov@inbox.ru>
*/

#include "minigui.ch"
#include "tsbrowse.ch"

REQUEST DBFCDX

SET PROCEDURE TO TS_ListBoxEx.prg

STATIC oBrw

PROCEDURE Main()

   rddSetDefault( 'DBFCDX' )
   SET CENTURY ON
   SET DELETED ON
   SET DATE GERMAN


   USE BASE SHARED NEW ALIAS "BS"
   USE CUSTOMER SHARED NEW ALIAS "RF" INDEX CUSTOMER
   RF->( ordSetFocus( "CUSTNO" ) )

   DEFINE WINDOW Form_0 ;
      AT 0, 0 ;
      WIDTH 800 ;
      HEIGHT 600 ;
      TITLE "TS_ListBox DEMO" ;
      MAIN ;
      FONT 'Tahoma' SIZE 10

   DEFINE TBROWSE oBrw  At 25, 5 ALIAS "BS" ;
      OF FORM_0 ;
      WIDTH Form_0.Width - 30 ;
      HEIGHT Form_0.Height -70 ;

   END TBROWSE

   oBrw:LoadFields( TRUE )
   oBrw:lCellBrw := TRUE
   oBrw:nHeightCell    := 20
   oBrw:nHeightHead    := 20
   oBrw:nHeightFoot    := 20

   oBrw:hBrush := CreateSolidBrush( 255, 255, 225 )
   oBrw:SetColor( { 2 }, { {|| RGB( 255, 255, 225 ) } } )

   oBrw:SetColSize( "CUSTNO",    760 )
   oBrw:GetColumn(  "CUSTNO" ):cHeading  := "Customer"
   oBrw:GetColumn(  "CUSTNO" ):bData     := {|| SeekName( BS->CUSTNO )  }
   oBrw:GetColumn(  "CUSTNO" ):lEdit     := TRUE
   oBrw:GetColumn(  "CUSTNO" ):nAlign    := DT_LEFT
   oBrw:GetColumn(  "CUSTNO" ):bPrevEdit := {| xVal, oBrw | TS_PrevEdit( xVal, oBrw ) }

   END WINDOW

   Form_0.Center
   Form_0.Activate

RETURN

FUNCTION SeekName( nID )

   LOCAL cRet := ""
   IF RF->( dbSeek( nID, FALSE ) )
      cRet := RF->COMPANY
   END

RETURN cRet


FUNCTION TS_PrevEdit( xVal, oBrw )

   LOCAL cAlias := oBrw:cAlias
   LOCAL lRet := TRUE
   LOCAL xLbx

   USE CUSTOMER SHARED NEW ALIAS "T1" INDEX CUSTOMER
   T1->( ordSetFocus( "CUSTNO" ) )
   ( cAlias )->( RLock() )

   LOCATE FOR Alltrim( T1->COMPANY ) == Alltrim( xVal )

   xLbx              := LBX():New()
   xLbx:cAlias       := "T1"
   xLbx:cRetField    := "CUSTNO"
   xLbx:aHeaders     := { 'Company', 'Address', 'City' }
   xLbx:aWidth       := { 200, 200, 200 }
   xLbx:aAlign       := { DT_LEFT, DT_LEFT, DT_LEFT }
   xLbx:aField       := { 'COMPANY', 'ADDR1', 'CITY' }
   xLbx:nHeightCell  := 20
   xLbx:nHeightHead  := 19
   xLbx:nHeightFoot  := 0
   xLbx:bPostBlock   := {|| NIL }
   xLbx:bSearch      := {| a, b|  Search( a, b ) }
   xLbx:ListBox( oBrw, xVal )

   T1->( dbCloseArea() )
   ( cAlias )->( dbUnlock() )
   lRet := FALSE

RETURN lRet


FUNCTION Search( oBrw, cStr )

   LOCAL cFilter

   cStr := AllTrim( cStr )
   cFilter := "[" + Upper( cStr ) + "] $ UPPER(COMPANY)" + " .OR. "  + "[" + Upper( cStr ) + "] $ UPPER(ADDR1)" + " .OR. "  + "[" + Upper( cStr ) + "] $ UPPER(CITY)"

   IF Empty( cStr )
      oBrw:FilterData( cStr )
   ELSE
      oBrw:FilterData( cFilter )
   ENDIF

RETURN NIL
