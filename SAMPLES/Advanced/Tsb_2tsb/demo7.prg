/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 */
#define  _HMG_OUTLOG

#include "hmg.ch"
#include "tsbrowse.ch"
#include "dbinfo.ch"

REQUEST DBFCDX

FUNCTION Main()
   LOCAL cFont := "Arial"
   LOCAL nSize := 12
   LOCAL cForm := "wMain"
   LOCAL oTsb1, oTsb2, oTsb3

   rddSetDefault( "DBFCDX" )

   SET EPOCH   TO 2000
   SET DATE    TO GERMAN
   SET CENTURY ON
   SET AUTOPEN OFF
   SET DELETED OFF
   SET OOP     ON

   SET FONT TO cFont, nSize

   _DefineFont("Normal"  , cFont, nSize  , .F., .F. )
   _DefineFont("Bold"    , cFont, nSize  , .T., .F. )
   _DefineFont("Italic"  , cFont, nSize-4, .F., .T. )

   USE ( "CUSTOMER"  )  ALIAS CUST1  NEW SHARED
   USE ( "CUSTOMER"  )  ALIAS CUST2  NEW SHARED
   USE ( "CUSTOMER2" )  ALIAS CUST3  NEW SHARED

   DEFINE WINDOW &cForm TITLE "Demo 3 TBrowse" ;
          MAIN NOSIZE TOPMOST   ;
          ON INIT    _wPost( 0) ;
          ON RELEASE _wSend(90)
          This.Cargo := oHmgData()

      DEFINE STATUSBAR BOLD
         STATUSITEM "*** W A I T ***"
         STATUSITEM "Click me  ===>" WIDTH This.ClientWidth * 0.1
         STATUSITEM ""               WIDTH This.ClientWidth * 0.1 ACTION _wPost(1,,1)
         STATUSITEM ""               WIDTH This.ClientWidth * 0.1 ACTION _wPost(1,,2)
         STATUSITEM ""               WIDTH This.ClientWidth * 0.1 ACTION _wPost(1,,3)
         STATUSITEM MiniGuiVersion() WIDTH This.ClientWidth * 0.4
      END STATUSBAR

      This.Maximize

      oTsb1 := oHmgData()
      oTsb1:cBrw    := "Brw_1"
      oTsb1:uAlias  := "CUST1"
      oTsb1:aEdit   := .T.
      oTsb1:aFoot   := .T.
      oTsb1:lZebra  := .T.
      oTsb1:aNumber := { 1, 50 }
      oTsb1:uSelector := 20
      oTsb1:bInit     := {|ob| ob:Hide() }
      oTsb1:bAfter    := {|ob|
                          Local oc := ob:aColumns[1]
                          oc:nClrBack := {|na,nc,obr|
                                          Local ocol := obr:aColumns[nc]
                                          Local nclr := ocol:nClrHeadBack
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
      oTsb2:cSuperHd      := oTsb2:cBrw + "." + oTsb2:uAlias + " -> " + ;
                             Lower((oTsb2:uAlias)->( dbInfo( DBI_FULLPATH ) )) 
      oTsb2:aSuperHdColor := {CLR_HBLUE, CLR_YELLOW}

      oTsb3 := oTsb1:Clone()
      oTsb3:cBrw   := "Brw_3"
      oTsb3:uAlias := "CUST3"
      oTsb3:cSuperHd      := oTsb3:cBrw + "." + oTsb3:uAlias + " -> " + ;
                             Lower((oTsb3:uAlias)->( dbInfo( DBI_FULLPATH ) ))
      oTsb3:aSuperHdColor := {CLR_YELLOW, CLR_HBLUE}
      oTsb3:nHeightSuper  += 4

      This.Cargo:aBrw := _TBrowse({ oTsb1, oTsb2, oTsb3 })
      This.Cargo:nBrw := 1

      AEval(This.Cargo:aBrw, {|ob,nn|
                               Local oc, ow, cn
                               ow := _WindowObj(ob:cParentWnd)
                               oc := ob:GetColumn("CUSTNO")
                               cn := ob:cControlName
                               oc:cFooting := cn + "."+ob:cAlias
                               ow:StatusBar:Say(cn + " show", 2 + nn)
                               Return Nil
                               })

      ON KEY F1     ACTION NIL
      ON KEY TAB    ACTION {|| 
                            Local ab := ThisWindow.Cargo:aBrw
                            Local nb := ThisWindow.Cargo:nBrw + 1
                            nb := iif( nb > Len(ab), 1, nb )
                            _wPost(1, , nb)
                            Return Nil
                            }
      ON KEY ESCAPE ACTION {||
                            Local nb := ThisWindow.Cargo:nBrw, ob
                            ob := ThisWindow.Cargo:aBrw[ nb ]
                            IF ob:IsEdit ; ob:SetFocus()
                            ELSE         ; _wPost(99)    //ThisWindow.Release
                            ENDIF
                            Return Nil
                            }

      WITH OBJECT This.Object
      :Event( 0, {|ow       | 
                              This.Topmost := .F.
                              ow:StatusBar:Say("", 1)
                              Default ow:Cargo:nBrw := 1
                              _wPost(1)
                              Return Nil
                              })
      :Event( 1, {|ow,ky,nbr| 
                              Local ob
                              Default nbr := ow:Cargo:nBrw
                              ow:Cargo:nBrw := nbr
                              AEval(ow:Cargo:aBrw, {|ob| ob:Hide() })
                              ob := ow:Cargo:aBrw[ nbr ]
                              ob:Show()
                              ob:SetFocus()
                              DO EVENTS
                              ky := nbr
                              Return Nil
                              })
      :Event(90, {|ow       | ow:Hide(), dbCloseAll() })
      :Event(99, {|ow       | ow:Release()            })
      END WITH

   END WINDOW

   ACTIVATE WINDOW &cForm

RETURN NIL
