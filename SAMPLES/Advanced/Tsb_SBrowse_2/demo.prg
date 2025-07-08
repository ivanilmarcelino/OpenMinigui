/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
*/

#include "hmg.ch"
#include "tsbrowse.ch"

REQUEST DBFCDX

Function Main()

   LOCAL cFont := "DejaVu Sans Mono"
   LOCAL nSize := 16
   LOCAL bSetUp, bSet, bRec, lNoMain
   LOCAL nW, nH, oDlu

   rddSetDefault( "DBFCDX" )

   SET EPOCH   TO 2000
   SET DATE    TO GERMAN
   SET CENTURY ON
   SET AUTOPEN OFF
   SET DELETED OFF

   SET NAVIGATION EXTENDED
   SET DEFAULT ICON TO "HMG_ICO"

   SET FONT TO cFont, nSize

   DEFINE FONT Normal FONTNAME cFont SIZE nSize
   DEFINE FONT Bold   FONTNAME cFont SIZE nSize BOLD

   IF ( lNoMain := Empty( App.Handle ) )
      SET WINDOW MAIN OFF
   ENDIF

   bSet := {|ob,um|
             LOCAL oc, fn
             IF Empty( um )                         // before END TBROWSE
             ELSE                                   // after  END TBROWSE
                fn := GetFontHandle( "Bold" )
                FOR EACH oc IN ob:aColumns
                   IF oc:cName == "SELECTOR" ; LOOP
                   ENDIF
                   oc:hFontHead := fn
                   IF oc:cName == "ORDKEYNO"
                      oc:hFont := fn
                      oc:hFontFoot := fn
                   ENDIF
                NEXT
                ob:SetNoHoles()
                ob:SetFocus()
             ENDIF
             RETURN .T.
           }

   bRec := {|ob,um|
             LOCAL oc, fn
             IF Empty( um )                         // before END TBROWSE
                ob:InsColNumber( 60, 1, "ORDKEYNO" )
                ob:lFooting := ob:lDrawFooters := .T.
                ob:nHeightFoot := ob:nHeightCell
             ELSE                                   // after  END TBROWSE
                fn := GetFontHandle( "Bold" )
                FOR EACH oc IN ob:aColumns
                    IF oc:cName == "SELECTOR" ; LOOP
                    ELSEIF oc:cName == "ORDKEYNO"
                      oc:hFont      := fn
                       oc:hFontFoot := fn
                       oc:nAlign    := DT_CENTER
                    ENDIF
                    oc:hFontHead := fn
                NEXT
                ob:nFreeze := ob:nColumn( "ORDKEYNO" )
                ob:SetNoHoles()
                ob:GoRight()
                DO EVENTS
                ob:SetFocus()
             ENDIF
             RETURN .T.
           }

   bSetUp := { bSet, , bRec }

   USE CUSTOMER ALIAS CUST SHARED NEW

   nW := Sys.ClientWidth

   oDlu := oDlu4Font( _HMG_DefaultFontSize )

   nH := oDlu:H1 * ( FCount() + 4 ) + 5
   nH := iif( nH > Sys.ClientHeight, Sys.ClientHeight, nH )
   nH := { Sys.ClientHeight, nH }

   SBrowse( Alias(), "DEMO. Test new SBrowse", bSetUp, , nW, nH, , .T., .T. )

   IF lNoMain
      SET WINDOW MAIN ON
   ENDIF

RETURN NIL
