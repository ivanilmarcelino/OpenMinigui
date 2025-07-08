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
   LOCAL oTsb1, oTsb2, oTsb3

   rddSetDefault( "DBFCDX" )

   SET EPOCH   TO 2000
   SET DATE    TO GERMAN
   SET CENTURY ON
   SET AUTOPEN OFF
   SET DELETED OFF

   SET FONT TO cFont, nSize

   _DefineFont("Normal"  , cFont, nSize  , .F., .F. )
   _DefineFont("Bold"    , cFont, nSize  , .T., .F. )
   _DefineFont("Italic"  , cFont, nSize-4, .F., .T. )

   USE ( "CUSTOMER"  )  ALIAS CUST1  NEW SHARED
   USE ( "CUSTOMER"  )  ALIAS CUST2  NEW SHARED
   USE ( "CUSTOMER2" )  ALIAS CUST3  NEW SHARED

   DEFINE WINDOW &cForm TITLE "Demo 3 TBrowse" ;
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

      oTsb1 := oHmgData()
      oTsb1:cBrw    := "Brw_1"
      oTsb1:uAlias  := "CUST1"
      oTsb1:aEdit   := .T.
      oTsb1:aFoot   := .T.
      oTsb1:lZebra  := .T.
      oTsb1:aNumber := { 1, 50 }
      oTsb1:uSelector := 20
      oTsb1:nY := nY
      oTsb1:nX := nX
      oTsb1:nW := nW
      oTsb1:nH := nH
      oTsb1:bInit     := {|ob| ob:Hide() }
      oTsb1:bAfter    := {|ob|
                          Local oc := ob:aColumns[1]
                          oc:nClrBack := {|na,nc,obr|
                                          Local ocol := obr:aColumns[nc]
                                          Local nclr := oCol:nClrHeadBack
                                          IF (obr:cAlias)->( Deleted() )
                                             nclr := CLR_HGRAY
                                             na := nc
                                          ENDIF
                                          Return nclr
                                          }
                          Return Nil
                          }
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
      oTsb1:lSpecHd  := .T.
      oTsb1:lSuperHd := .T.
      oTsb1:nHeightSuper  := GetFontHeight("Bold") + 4
      oTsb1:cSuperHd      := oTsb1:cBrw + "." + oTsb1:uAlias + " -> " + ;
                             Lower((oTsb1:uAlias)->( dbInfo( DBI_FULLPATH ) ))
      oTsb1:aSuperHdColor := {CLR_YELLOW, {CLR_HMAGENTA, CLR_BLUE} }

      oTsb2 := oTsb1:Clone()
      oTsb2:cBrw   := "Brw_2"
      oTsb2:uAlias := "CUST2"
      oTsb2:lZebra := .F.
      oTsb2:lChess := .T.
      oTsb2:nY     := oTsb1:nY + oTsb1:nH
      oTsb2:nH     -= This.StatusBar.Height
      oTsb2:cSuperHd      := oTsb2:cBrw + "." + oTsb2:uAlias + " -> " + ;
                             Lower((oTsb2:uAlias)->( dbInfo( DBI_FULLPATH ) )) 
      oTsb2:aSuperHdColor := {CLR_HBLUE, CLR_YELLOW}

      oTsb3 := oTsb1:Clone()
      oTsb3:cBrw   := "Brw_3"
      oTsb3:uAlias := "CUST3"
      oTsb3:nY     := oTsb1:nY
      oTsb3:nX     := oTsb1:nX + oTsb1:nW
      oTsb3:nH     := NIL // This.ClientHeight
      oTsb3:cSuperHd      := oTsb3:cBrw + "." + oTsb3:uAlias + " -> " + ;
                             Lower((oTsb3:uAlias)->( dbInfo( DBI_FULLPATH ) ))
      oTsb3:aSuperHdColor := {CLR_YELLOW, CLR_HBLUE}
      oTsb3:nHeightSuper  += 4

      This.Cargo:aBrw := _TBrowse({ oTsb1, oTsb2, oTsb3 })
      This.Cargo:nBrw := 1

      AEval(This.Cargo:aBrw, {|ob,oc| 
                               oc := ob:GetColumn("CUSTNO")
                               oc:cFooting := ob:cControlName+"."+ob:cAlias
                               Return Nil
                               })

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
