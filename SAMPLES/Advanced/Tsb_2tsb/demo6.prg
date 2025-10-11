/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 */
#define  _HMG_OUTLOG

#include "hmg.ch"
#include "tsbrowse.ch"
#include "dbinfo.ch"

REQUEST DBFCDX

Function Main()
   LOCAL cFont := "Arial"
   LOCAL nSize := 12
   LOCAL cForm := "wMain"
   LOCAL nY, nX, nH, nW
   LOCAL oTsb1, oTsb2, oTsb3, oTsb4

   rddSetDefault( "DBFCDX" )

   SET EPOCH   TO 2000
   SET DATE    TO GERMAN
   SET CENTURY ON
   SET AUTOPEN OFF
   SET DELETED OFF

   App.Cargo := oHmgData() ; oac := App.Cargo
   //
   oac:oTsb := oHmgData()
   oac:oTsb:aEdit     := .T.
   oac:oTsb:aFoot     := .T.
   oac:oTsb:aNumber   := { 1, App.Object:W(0.5) }
   oac:oTsb:uSelector := 20
   oac:oTsb:lHide     := .T.
   oac:oTsb:bGotFocus := {|ob|
                      Local owc
                      IF IsObject(ob)
                         SET WINDOW THIS TO ob:cParentWnd
                         owc := This.Cargo
                         owc:nBrw := ob:Cargo:nBrw
                         SET WINDOW THIS TO 
                      ENDIF
                      Return Nil
                      }
   oac:oTsb:lSpecHd      := .T.
   oac:oTsb:lSuperHd     := .T.
   oac:oTsb:nHeightSuper := GetFontHeight("Bold") + 4
   oac:oTsb:bSuperHdSet  := {|a,cMsg|
                             cMsg := a[1] + "." + a[2] + " -> " 
                             cMsg += Lower((a[2])->( dbInfo( DBI_FULLPATH ) ))
                             Return cMsg + " "
                             }
   oac:oTsb:nHeightHead  := App.Object:H(1.1)
   oac:oTsb:nHeightCell  := App.Object:H(1.1)
   oac:oTsb:nHeightFoot  := App.Object:H(1.1)
   oac:oTsb:nHeightSuper := App.Object:H(1.2)
   //
   SET FONT TO cFont, nSize

   _DefineFont("Normal"  , cFont, nSize  , .F., .F. )
   _DefineFont("Bold"    , cFont, nSize  , .T., .F. )
   _DefineFont("Italic"  , cFont, nSize-4, .F., .T. )

   USE ( "CUSTOMER"  )  ALIAS CUST1  NEW SHARED
   USE ( "CUSTOMER2" )  ALIAS CUST2  NEW SHARED
   USE ( "CUSTOMER3" )  ALIAS CUST3  NEW SHARED
   USE ( "CUSTOMER3" )  ALIAS CUST4  NEW SHARED

   DEFINE WINDOW &cForm TITLE "Demo 4 TBrowse. DEMO6" ;
          MAIN NOSIZE TOPMOST ;
          ON INIT    {|nb| This.Topmost := .F., nb := This.Cargo:nBrw , ;
                           AEval(This.Cargo:aBrw, {|obr| obr:Show() }), ;
                           This.Cargo:aBrw[nb]:SetFocus(), ;
                           DoEvents() } ;
          ON RELEASE ( dbCloseAll() )
          This.Cargo := oHmgData()

      DEFINE STATUSBAR BOLD
         STATUSITEM ""
         STATUSITEM MiniGuiVersion() WIDTH This.ClientWidth * 0.5
      END STATUSBAR

      This.Maximize

      nY := nX := 0
      nW := This.ClientWidth  * 0.5
      nH := This.ClientHeight * 0.5

      oTsb1 := App.Cargo:oTsb:Clone()
      oTsb1:cBrw     := "Brw_1"
      oTsb1:uAlias   := "CUST1"
      oTsb1:lZebra   := .T.
      oTsb1:cSuperHd := EVal(oTsb1:bSuperHdSet, {oTsb1:cBrw, oTsb1:uAlias})
      oTsb1:aSuperHdColor := {CLR_YELLOW, {CLR_HMAGENTA, CLR_BLUE} }
      oTsb1:cSuperHd += " Draw zebra line: " + iif( oTsb1:lZebra, "YES", "NO" )
      //
      oTsb1:nY := nY
      oTsb1:nX := nX
      oTsb1:nW := nW
      oTsb1:nH := nH

      oTsb2 := oTsb1:Clone()
      oTsb2:cBrw     := "Brw_2"
      oTsb2:uAlias   := "CUST2"
      oTsb2:lChess   := .T.
      oTsb2:cSuperHd := EVal(oTsb2:bSuperHdSet, {oTsb2:cBrw, oTsb2:uAlias})
      oTsb2:aSuperHdColor := {CLR_HBLUE, CLR_YELLOW}
      oTsb2:cSuperHd += " Draw chess column: " + iif( oTsb2:lChess, "YES", "NO" )
      //
      oTsb2:nY := oTsb1:nY + oTsb1:nH
      oTsb2:nH -= This.StatusBar.Height

      oTsb3 := oTsb1:Clone()
      oTsb3:cBrw     := "Brw_3"
      oTsb3:uAlias   := "CUST3"
      oTsb3:lZebra   := .T.
      oTsb3:aMoveCol := {"COUNTRY", "COMPANY"}
      oTsb3:cSuperHd := EVal(oTsb3:bSuperHdSet, {oTsb3:cBrw, oTsb3:uAlias})
      oTsb3:aSuperHdColor := {CLR_YELLOW, CLR_HBLUE}
      oTsb3:lZebraLine    := .F.                      // only columns
      oTsb3:lZebraGroup   := .T.
      oTsb3:cZebraGroup   := "COUNTRY"
      oTsb3:cSuperHd += " Draw group line: " + iif( oTsb3:lZebraLine, "YES", "NO" )
      //
      oTsb3:nX := oTsb1:nX + oTsb1:nW

      oTsb4 := oTsb2:Clone()
      oTsb4:cBrw     := "Brw_4"
      oTsb4:uAlias   := "CUST4"
      oTsb4:lZebra   := .T.
      oTsb4:aMoveCol := {"COUNTRY", "COMPANY"}
      oTsb4:cSuperHd := EVal(oTsb4:bSuperHdSet, {oTsb4:cBrw, oTsb4:uAlias})
      oTsb4:aSuperHdColor := {CLR_YELLOW, CLR_HBLUE}
      oTsb4:lZebraLine    := .T.                      // all columns
      oTsb4:lZebraGroup   := .T.
      oTsb4:cZebraGroup   := "COUNTRY"
      oTsb4:cSuperHd += " Draw group line: " + iif( oTsb4:lZebraLine, "YES", "NO" )
      //
      oTsb4:nX := oTsb2:nX + oTsb2:nW

      This.Cargo:aBrw := _TBrowse({ oTsb1, oTsb2, oTsb3, oTsb4 })
      This.Cargo:nBrw := 1

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
