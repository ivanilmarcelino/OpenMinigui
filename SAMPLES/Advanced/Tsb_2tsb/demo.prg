/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 */

#include "hmg.ch"
#include "tsbrowse.ch"

REQUEST DBFCDX

Function Main()
   LOCAL cFont := "Arial"
   LOCAL nSize := 12
   LOCAL cForm := "wMain"
   LOCAL oBrw1, oBrw2, nY, nX, nH, nW
   LOCAL oTsb1, oTsb2

   rddSetDefault( "DBFCDX" )

   SET EPOCH   TO 2000
   SET DATE    TO GERMAN
   SET CENTURY ON
   SET AUTOPEN OFF
   SET DELETED OFF

   SET FONT TO cFont, nSize

   USE ( "CUSTOMER" )  ALIAS CUST1  NEW SHARED
   USE ( "CUSTOMER" )  ALIAS CUST2  NEW SHARED

   DEFINE WINDOW &cForm TITLE "Demo 2 TBrowse" ;
          MAIN NOSIZE TOPMOST ;
          ON INIT    ( This.Topmost := .F. ) ;
          ON RELEASE ( dbCloseAll() )

      This.Maximize

      nY := nX := 0
      nW := This.ClientWidth
      nH := Int( This.ClientHeight / 2 )

      oTsb1 := oHmgData()
      oTsb1:aEdit := .T.
      oTsb1:aNumber := { 1, 70 }
      oTsb1:uSelector := 20

      oBrw1 := _TBrowse( oTsb1, "CUST1", "Brw_1", nY, nX, nW, nH )

      nY += nH + 1
      nH -= 1

      oTsb2 := oHmgData()
      oTsb2:aEdit := .T.
      oTsb2:aNumber := { 1, 70 }
      oTsb2:uSelector := 20

      oBrw2 := _TBrowse( oTsb2, "CUST2", "Brw_2", nY, nX, nW, nH )

      oBrw1:SetFocus()

      ON KEY F1     ACTION NIL
      ON KEY TAB    ACTION { |cf| cf := ThisWindow.FocusedControl, ;
                             iif( cf == "Brw_1", This.Brw_2.SetFocus, This.Brw_1.SetFocus ) }
      ON KEY ESCAPE ACTION ( iif( oBrw1:IsEdit, oBrw1:SetFocus(), ;
                             iif( oBrw2:IsEdit, oBrw2:SetFocus(), ;
                             ThisWindow.Release ) ) )

   END WINDOW

   ACTIVATE WINDOW &cForm

RETURN NIL
