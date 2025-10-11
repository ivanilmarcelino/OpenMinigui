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
   LOCAL cFont := "Arial", nSize := 12
   LOCAL cForm := "wMain"
   LOCAL oTsb1, oTsb2, oTsb3, oTsb4, oac, owc

   rddSetDefault( "DBFCDX" )

   SET EPOCH   TO 2000
   SET DATE    TO GERMAN
   SET CENTURY ON
   SET AUTOPEN OFF
   SET DELETED OFF
   SET OOP     ON

   App.Cargo := oHmgData() ; oac := App.Cargo
   //
   oac:oTsb := oHmgData()
   oac:oTsb:aEdit     := .T.
   oac:oTsb:aFoot     := .T.
   oac:oTsb:aNumber   := { 1, App.Object:W(0.6), DT_RIGHT, 7 }
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
                             Return cMsg
                             }
   oac:oTsb:nHeightHead  := App.Object:H(1.2)
   oac:oTsb:nHeightCell  := App.Object:H(1.2)
   oac:oTsb:nHeightFoot  := App.Object:H(1.2)
   oac:oTsb:nHeightSuper := App.Object:H(1.3)
   oac:oTsb:l_Log_Out    := .F.
   oac:oTsb:b_Log_Out    := {|ob,op,cp|
              IF !IsChar(cp) ; cp := " "
              ENDIF
              IF IsObject(ob)
                 cp += " "+ob:ClassName+" "+ob:cAlias+" "+ob:cParentWnd
              ENDIF
              ? "===>", ob, op, cp
              IF     "CRE" $ cp
                 _o2log(op,25, ">>> oTsb: ", .T.)
              ELSEIF "INI" $ cp
                 _o2log(op,25, ">>> oParam: ", .T.)
              ELSEIF "BOD" $ cp
                 _o2log(op,25, ">>> oParam: ", .T.)
              ELSEIF "AFT" $ cp
                 _o2log(op,25, ">>> oParam: ", .T.)
              ELSEIF "END" $ cp
                 _o2log(op,25, ">>> oParam: ", .T.)
              ELSE
                 _o2log(ob:Cargo,25, ">>> oBrw:Cargo: ", .T.)
              ENDIF
              Return Nil
              }
   //
   SET FONT TO cFont, nSize
   //
   _DefineFont("Normal"  , cFont, nSize  , .F., .F. )
   _DefineFont("Bold"    , cFont, nSize  , .T., .F. )
   _DefineFont("Italic"  , cFont, nSize-4, .F., .T. )

   USE ( "CUSTOMER"  )  ALIAS CUST1  NEW SHARED
   USE ( "CUSTOMER2" )  ALIAS CUST2  NEW SHARED
   USE ( "CUSTOMER3" )  ALIAS CUST3  NEW SHARED
   USE ( "CUSTOMER3" )  ALIAS CUST4  NEW SHARED

   DEFINE WINDOW &cForm TITLE "Demo 4 TBrowse. DEMO7" ;
          MAIN NOSIZE TOPMOST   ;
          ON INIT    _wPost( 0) ;
          ON RELEASE _wSend(90)
          This.Cargo := oHmgData() ; owc := This.Cargo

      owc:lInit := .F.

      DEFINE STATUSBAR BOLD
         STATUSITEM "*** W A I T ***"
         STATUSITEM ""               WIDTH This.ClientWidth * 0.1 ACTION _wPost(1)
         STATUSITEM ""               WIDTH This.ClientWidth * 0.1 ACTION _wPost(1,,1)
         STATUSITEM ""               WIDTH This.ClientWidth * 0.1 ACTION _wPost(1,,2)
         STATUSITEM ""               WIDTH This.ClientWidth * 0.1 ACTION _wPost(1,,3)
         STATUSITEM ""               WIDTH This.ClientWidth * 0.1 ACTION _wPost(1,,4)
         STATUSITEM MiniGuiVersion() WIDTH This.ClientWidth * 0.4
      END STATUSBAR

      This.Maximize

      oTsb1 := App.Cargo:oTsb:Clone()
      oTsb1:cBrw     := "Brw_1"
      oTsb1:uAlias   := "CUST1"
      oTsb1:lZebra   := .T.
      oTsb1:cSuperHd := EVal(oTsb1:bSuperHdSet, {oTsb1:cBrw, oTsb1:uAlias})
      oTsb1:aSuperHdColor := {CLR_YELLOW, {CLR_HMAGENTA, CLR_BLUE} }

      oTsb2 := App.Cargo:oTsb:Clone()
      oTsb2:cBrw     := "Brw_2"
      oTsb2:uAlias   := "CUST2"
      oTsb2:lChess   := .T.
      oTsb2:cSuperHd := EVal(oTsb2:bSuperHdSet, {oTsb2:cBrw, oTsb2:uAlias})
      oTsb2:aSuperHdColor := {CLR_HBLUE, CLR_YELLOW}

      oTsb3 := App.Cargo:oTsb:Clone()
      oTsb3:cBrw     := "Brw_3"
      oTsb3:uAlias   := "CUST3"
      oTsb3:lZebra   := .T.
      oTsb3:aMoveCol := {"COUNTRY", "ADDR1"}
      oTsb3:cSuperHd := EVal(oTsb3:bSuperHdSet, {oTsb3:cBrw, oTsb3:uAlias})
      oTsb3:aSuperHdColor := {CLR_YELLOW, CLR_HBLUE}
      oTsb3:lZebraLine    := .F.                      // only columns
      oTsb3:lZebraGroup   := .T.
      oTsb3:cZebraGroup   := "COUNTRY"
      oTsb3:aZebraGroup    := { CLR_HGRAY, CLR_WHITE, CLR_GRAY }
      oTsb3:cSuperHd += " Draw group line: " + iif( oTsb3:lZebraLine, "YES", "NO" )

      oTsb4 := App.Cargo:oTsb:Clone()
      oTsb4:cBrw     := "Brw_4"
      oTsb4:uAlias   := "CUST4"
      oTsb4:lZebra   := .T.
      oTsb4:aMoveCol := {"COUNTRY", "ADDR1"}
      oTsb4:cSuperHd := EVal(oTsb4:bSuperHdSet, {oTsb4:cBrw, oTsb4:uAlias})
      oTsb4:aSuperHdColor := {CLR_YELLOW, CLR_HBLUE}
      oTsb4:lZebraLine    := .T.                      // all columns
      oTsb4:lZebraGroup   := .T.
      oTsb4:cZebraGroup   := "COUNTRY"
      oTsb4:aZebraGroup    := { CLR_HGRAY, CLR_WHITE, CLR_GRAY }
      oTsb4:cSuperHd += " Draw group line: " + iif( oTsb4:lZebraLine, "YES", "NO" )

      This.Cargo:aBrw := _TBrowse({ oTsb1, oTsb2, oTsb3, oTsb4 })
      This.Cargo:nBrw := 1

      AEval(This.Cargo:aBrw, {|ob,nn|
                               Local ow, cn
                               ow := _WindowObj(ob:cParentWnd)
                               cn := ob:cControlName
                               ow:StatusBar:Say(" " + cn + " show", 2 + nn)
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
                   Local owc := ow:Cargo
                   This.Topmost := .F.
                   ow:StatusBar:Say("", 1)
                   ow:StatusBar:Say("Click me", 2)
                   owc:lInit := .T.          // init window end
                   Default owc:nBrw := 1
                   _wPost(1,, owc:nBrw)
                   Return Nil
                   })
      :Event( 1, {|ow,ky,nbr| 
                   Local ob, nb, owc := ow:Cargo
                   IF Empty(owc:lInit) ; Return Nil
                   ENDIF
                   IF Empty(nbr)
                      nbr := owc:nBrw + 1
                      nbr := iif( nbr > Len(owc:aBrw), 1, nbr )
                   ENDIF
                   owc:nBrw := nbr
                   AEval(owc:aBrw, {|ob| ob:Hide() })
                   ob := owc:aBrw[ nbr ]
                   ob:Show()
                   ob:SetFocus()
                   DO EVENTS
                   FOR nb := 1 TO Len(owc:aBrw)
                       ky := ow:StatusBar:Get(2 + nb)
                       IF left(ky, 1) == ">", ky := subs(ky, 2)
                       ENDIF
                       IF     nb == nbr          ; ky := ">" + ky
                       ELSEIF left(ky, 1) != " " ; ky := " " + ky
                       ENDIF
                       ow:StatusBar:Say(ky, 2 + nb)
                   NEXT
                   Return Nil
                   })
      :Event(90, {|ow| ow:Hide(), dbCloseAll() })
      :Event(99, {|ow| ow:Release()            })
      END WITH

   END WINDOW

   ACTIVATE WINDOW &cForm

RETURN NIL
