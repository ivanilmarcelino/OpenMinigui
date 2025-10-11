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
   LOCAL cForm := "wMain", oTsb, oBrw
   LOCAL cAls  := "CUST1", cBrw := "oBrw"
   LOCAL cTabl := "Employee", a, k, y, x, w, h
   LOCAL cTitl := " Filter FTS. " + MiniGuiVersion()
   LOCAL oap := App.Object, b1, b2
   //
   USE ( cTabl )  ALIAS ( cAls )  NEW SHARED
   //
   oTsb := oHmgData()
   oTsb:aSizeLen := {}
   FOR EACH a IN (cAls)->( dbStruct() )
       k := iif( Len(a[1]) > a[3], Len(a[1]), a[3] )
       k := iif( k < 10, 10, k )
       k := iif( a[1] == "NOTES", 15, k )
       IF k > 10 .and. a[2] == "C" ; AAdd(oTsb:aSizeLen, int(k * 0.5))
       ELSE                        ; AAdd(oTsb:aSizeLen, k)
       ENDIF
   NEXT
   //
   DEFINE WINDOW &cForm TITLE "DEMO. TBrowse DBF. " + cTitl   ;
          AT 0,0 WIDTH Sys.ClientWidth HEIGHT Sys.ClientHeight ;
          MAIN NOSIZE TOPMOST  ;
          ON INIT    ( This.Topmost := .F.,        ;
                       This.Cargo:oBrw:SetFocus(), ;
                       This.Cargo:lOnInit := .T.)  ;
          ON RELEASE ( dbCloseAll() )
          This.Cargo := oHmgData() ; This.Cargo:lOnInit := .F.
      //
      oTsb := oTsb_Def(oTsb)
      //
      oTsb:cSuperHd      := EVal(oTsb:bSuperHdSet, {cBrw, cAls})
      oTsb:aSuperHdColor := {CLR_HBLUE, CLR_YELLOW}
      //
      y := oap:nMargHeight
      x := oap:nMargWidth

      @ y, x LABEL Lbl_1 VALUE "Filter FTS (only symbolic fields)" ;
             WIDTH oap:W3 HEIGHT oap:H1 VCENTERALIGN RIGHTALIGN

      x += This.Lbl_1.Width + oap:nGapsWidth

      b1 := {|cget|
             Local  ob := ThisWindow.Cargo:oBrw
             IF ThisWindow.Cargo:lOnInit
                IF "1" $ cget ; This.FTS_2.Value := ""
                ELSE          ; This.FTS_1.Value := ""
                ENDIF
             ENDIF
             Return Nil
             }
      b2 := {||
             Local cVal := alltrim( This.Value )
             Local nLen := Len( cVal ), lDraw := .T.
             Local oBrw
             IF ThisWindow.Cargo:lOnInit
                oBrw := ThisWindow.Cargo:oBrw
                IF nLen == 0 
                   IF oBrw:Cargo:lFilter
                      oBrw:FilterFTS()
                      oBrw:Cargo:lFilter := .F.
                   ENDIF
                ELSEIF nLen > 2
                   oBrw:FilterFTS( cVal, .T., , , .T. )
                   oBrw:Cargo:lFilter := .T.
                ELSE
                   lDraw := .F.
                ENDIF
                IF lDraw
                   oBrw:CalcTotal( oBrw:Cargo:cTotal, oBrw:Cargo:cNoTotal)
                   DO EVENTS
                ENDIF
             ENDIF
             Return Nil
             }

      @ y, x GETBOX FTS_1 WIDTH oap:W2 HEIGHT oap:H1  VALUE " " ;
           PICTURE "@K "+Repl("X", 30) NOTABSTOP ;
           ACTION      This.Value := ""          ;
           IMAGE       { "bDelRed24","" }        ; 
           BUTTONWIDTH oap:H1                    ;
           ON GOTFOCUS EVal(b1, This.Name)       ;
           ON CHANGE   EVal(b2)

      x += This.FTS_1.Width + oap:nGapsWidth

      @ y, x LABEL Lbl_2 VALUE "Filter FTS (all fields)" ;
             WIDTH oap:W(2.5) HEIGHT oap:H1 VCENTERALIGN RIGHTALIGN

      x += This.Lbl_2.Width + oap:nGapsWidth

      @ y, x GETBOX FTS_2 WIDTH oap:W2 HEIGHT oap:H1  VALUE " " ;
           PICTURE "@K "+Repl("X", 30) NOTABSTOP ;
           ACTION      This.Value := ""          ;
           IMAGE       { "bDelRed24","" }        ; 
           BUTTONWIDTH oap:H1                    ;
           ON GOTFOCUS EVal(b1, This.Name)       ;
           ON CHANGE   EVal(b2)
      
      y += oap:H1 + oap:nGapsHeight
      x := oap:nMargWidth
      w := This.ClientWidth  - x * 2
      h := This.ClientHeight - ( y + oap:nMargHeight)

      This.Cargo:oBrw := _TBrowse( oTsb, cAls, cBrw, y, x, w, h )

      ON KEY F1     ACTION NIL
      ON KEY ESCAPE ACTION {|ob| ob := ThisWindow.Cargo:oBrw, ;
             iif( ob:IsEdit, ob:SetFocus(), ThisWindow.Release ) }

   END WINDOW

   ACTIVATE WINDOW &cForm

RETURN NIL

FUNCTION oTsb_Def(oTsb)

   Default oTsb := oHmgData()

   oTsb:lZebra       := .T.
   oTsb:aEdit        := .F.
   oTsb:aFoot        := .T.
   oTsb:aNumber      := { 1, App.Object:W(0.6), DT_CENTER, 6 } // 1 or 6 or 7
   oTsb:uSelector    := 20
   oTsb:lSpecHd      := .T.
   oTsb:lSuperHd     := .T.
   oTsb:bSuperHdSet  := {|a,cMsg|
                         cMsg := a[1] + "." + a[2] + " -> " 
                         cMsg += Lower((a[2])->( dbInfo( DBI_FULLPATH ) ))
                         Return cMsg + " "
                         }
   //
   oTsb:nHeightHead  := App.Object:H(1.1)
   oTsb:nHeightCell  := App.Object:H(1.1)
   oTsb:nHeightFoot  := App.Object:H(1.1)
   oTsb:nHeightSuper := App.Object:H(1.2)
   //
   oTsb:b_Init_Def := {|ob| //,op|
          ob:MoveColumn(ob:nColumn("MARRIED" ), ob:nColumn("STREET"))
          ob:MoveColumn(ob:nColumn("HIREDATE"), ob:nColumn("STREET"))
          ob:MoveColumn(ob:nColumn("AGE"     ), ob:nColumn("STREET"))
          ob:Cargo:lFilter := .F.
          Return Nil
          }
   //
   oTsb:b_After_Def := {|ob| //,op|
          // пример параметров 
          //ob:Cargo:cTotal   := "AGE" // задания списка итога по колонкам
          //ob:Cargo:cNoTotal := "AGE" // списка обхода итога по колонкам
          //
          ob:CalcTotal( ob:Cargo:cTotal, ob:Cargo:cNoTotal)
          Return Nil
          }
   //
RETURN oTsb

INIT PROCEDURE Sets_ENV()
   LOCAL cFont := "Arial", nSize := 12

   rddSetDefault( "DBFCDX" )

   SET EPOCH   TO 2000
   SET DATE    TO GERMAN
   SET CENTURY ON
   SET AUTOPEN OFF
   SET DELETED OFF
   //
   Set ShowRedAlert On 
   //
   SET MULTIPLE QUIT WARNING  
   SET DEFAULT ICON TO "1MG"
   //
   IF     Sys.DesktopWidth >= 1920 ; nSize += 4 
   ELSEIF Sys.DesktopWidth >  1280 ; nSize += 2 
   ENDIF
   //
   SET FONT TO cFont, nSize

   _DefineFont("Normal"  , cFont, nSize  , .F., .F. )
   _DefineFont("Bold"    , cFont, nSize  , .T., .F. )
   _DefineFont("Italic"  , cFont, nSize-4, .F., .T. )
   // Alert* font
   _DefineFont("DlgFont" , cFont, nSize+2, .F., .F. )

RETURN
