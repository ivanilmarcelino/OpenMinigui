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
   LOCAL nY, nX, nH, nW
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
          This.Cargo := oHmgData()

      This.Maximize

      nY := nX := 0
      nW := This.ClientWidth
      nH := Int( This.ClientHeight / 2 )

      oTsb1 := oHmgData()
      oTsb1:cBrw    := "Brw_1"
      oTsb1:cAlias  := "CUST1"
      oTsb1:aEdit   := .T.
      oTsb1:aFoot   := .T.
      oTsb1:lZebra  := .T.
      oTsb1:aNumber := { 1, 60 }
      oTsb1:uSelector := 20
      oTsb1:nY := nY
      oTsb1:nX := nX
      oTsb1:nW := nW
      oTsb1:nH := nH
      oTsb1:bGotFocus := {|ob|
                          Local owc
                          IF IsObject(ob)
                             SET WINDOW THIS TO ob:cParentWnd
                             owc := This.Cargo
                             owc:nBrw := ob:Cargo:nBrw
                             SET WINDOW THIS TO 
                          ENDIF
                          Return Nil
                          }

      nY += nH 
      nH -= 1

      oTsb2 := oTsb1:Clone()
      oTsb2:cBrw   := "Brw_2"
      oTsb2:cAlias := "CUST2"
      oTsb2:lZebra := .F.
      oTsb2:lChess := .T.
      oTsb2:nY     := nY
      oTsb2:nH     := nH

      This.Cargo:aBrw := _TBrowse({ oTsb1, oTsb2 })
      This.Cargo:nBrw := 1

      This.Cargo:aBrw[ This.Cargo:nBrw ]:SetFocus()

      ON KEY F1     ACTION NIL
      ON KEY TAB    ACTION {|| 
                            Local ab := ThisWindow.Cargo:aBrw, ob
                            Local nb := ThisWindow.Cargo:nBrw + 1
                            nb := iif( nb > Len(ab), 1, nb )
                            ob := ab[ nb ]
                            ob:SetFocus()
                            Return Nil
                            }
      ON KEY ESCAPE ACTION {||
                            Local ab := ThisWindow.Cargo:aBrw, ob
                            Local nb := ThisWindow.Cargo:nBrw 
                            ob := ab[ nb ]
                            IF ob:IsEdit
                               ob:SetFocus()
                            ELSE
                               ThisWindow.Release
                            ENDIF
                            Return Nil
                            }

   END WINDOW

   ACTIVATE WINDOW &cForm

RETURN NIL
