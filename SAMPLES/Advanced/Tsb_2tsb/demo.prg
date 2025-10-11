/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 */

#include "hmg.ch"
#include "tsbrowse.ch"
#include "dbinfo.ch"

REQUEST DBFCDX

Function Main()
   LOCAL cFont := "Arial", nSize := 12
   LOCAL cForm := "wMain", oTsb, oBrw
   LOCAL cAls  := "CUST1", cBrw := "oBrw"
   LOCAL cTabl := "CUSTOMER2"
   LOCAL cTitl := " Mouse (Right, Left) click events" 

   rddSetDefault( "DBFCDX" )

   SET EPOCH   TO 2000
   SET DATE    TO GERMAN
   SET CENTURY ON
   SET AUTOPEN OFF
   SET DELETED OFF
   //
   SET FONT TO cFont, nSize

   _DefineFont("Normal"  , cFont, nSize  , .F., .F. )
   _DefineFont("Bold"    , cFont, nSize  , .T., .F. )
   _DefineFont("Italic"  , cFont, nSize-4, .F., .T. )
   //
   USE ( cTabl )  ALIAS ( cAls )  NEW SHARED

   DEFINE WINDOW &cForm TITLE "Demo TBrowse. DEMO. " + cTitl   ;
          AT 0,0 WIDTH Sys.ClientWidth HEIGHT Sys.ClientHeight ;
          MAIN NOSIZE TOPMOST  ;
          ON INIT    ( This.Topmost := .F. ) ;
          ON RELEASE ( dbCloseAll() )

      oTsb := oTsb_Def()
      //
      oTsb:cSuperHd      := EVal(oTsb:bSuperHdSet, {cBrw, cAls})
      oTsb:aSuperHdColor := {CLR_HBLUE, CLR_YELLOW}

      oBrw := _TBrowse( oTsb, cAls, cBrw ) ; oBrw:SetFocus()

      ON KEY F1     ACTION NIL
      ON KEY ESCAPE ACTION iif( oBrw:IsEdit, oBrw:SetFocus(), ThisWindow.Release ) 

   END WINDOW

   ACTIVATE WINDOW &cForm

RETURN NIL

FUNCTION oTsb_Def(oTsb)

   Default oTsb := oHmgData()

   oTsb:lZebra       := .T.
   oTsb:aEdit        := .F.
   oTsb:aFoot        := .T.
   oTsb:aNumber      := { 1, App.Object:W(0.6), DT_RIGHT, 6 } // 1 or 6 or 7
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
   // 1 - line: Cell. Left mouse click
   oTsb:bLClicked := {|nrp,ncp,nfl,ob| bMClicked(.F., 1, ob, nrp, ncp, nfl) }
   /*
   oTsb:bLClicked := {|nrp,ncp,nfl,ob| 
        Local nRow, nCol, cTxt := 'Cell. Left mouse click'
        nRow  := ob:GetTxtRow(nrp)       // table row cursor number      
        nCol  := Max(ob:nAtCol(ncp), 1 ) // cursor column number in table
        MsgDebug(ob:cControlName, cTxt, nRow, nCol)
        Return Nil
        }
   */
   // 1 - line: Cell. Right mouse click
   oTsb:bRClicked := {|nrp,ncp,nfl,ob| bMClicked(.T., 1, ob, nrp, ncp, nfl) }
   /*
   oTsb:bRClicked := {|nrp,ncp,nfl,ob| 
        Local nRow, nCol, cTxt := 'Cell. Right mouse click'
        nRow  := ob:GetTxtRow(nrp)       // table row cursor number      
        nCol  := Max(ob:nAtCol(ncp), 1 ) // cursor column number in table
        MsgDebug(ob:cControlName, cTxt, nRow, nCol)
        Return Nil
        }
   */
   // 0 - line: [Super] Header. Left mouse click
   oTsb:bHLClicked := {|nrp,ncp,nat,ob| bMClicked(.F., 0, ob, nrp, ncp, nat) }
   /*
   oTsb:bHLClicked := {|nrp,ncp,nat,ob| 
        Local nRow, nCol, cTxt := 'Header. Left mouse click'
        IF nrp < ob:nHeightSuper ; cTxt := "Super " + cTxt
        ENDIF
        nRow := ob:GetTxtRow(nrp)       // table row cursor number      
        nCol := Max(ob:nAtCol(ncp), 1 ) // cursor column number in table
        MsgDebug(ob:cControlName, cTxt, nRow, nCol)
        Return Nil
        }
   */
   // 0 - line: [Super] Header. Right mouse click
   oTsb:bHRClicked := {|nrp,ncp,nat,ob| bMClicked(.T., 0, ob, nrp, ncp, nat) }
   /*
   oTsb:bHRClicked := {|nrp,ncp,nat,ob|  
        Local nRow, nCol, cTxt := 'Header. Right mouse click'
        IF nrp < ob:nHeightSuper ; cTxt := "Super " + cTxt
        ENDIF
        nRow := ob:GetTxtRow(nrp)       // table row cursor number      
        nCol := Max(ob:nAtCol(ncp), 1 ) // cursor column number in table
        MsgDebug(ob:cControlName, cTxt, nRow, nCol)
        Return Nil
        }
   */
   // 3 - line: Special Header. Left mouse click
   oTsb:bSLClicked := {|nrp,ncp,nat,ob| bMClicked(.F., 3, ob, nrp, ncp, nat) }
   /*
   oTsb:bSLClicked := {|nrp,ncp,nat,ob| 
        Local nRow, nCol, cTxt := 'Special Header. Left mouse click'
        nRow := ob:GetTxtRow(nrp)       // table row cursor number      
        nCol := Max(ob:nAtCol(ncp), 1 ) // cursor column number in table
        MsgDebug(ob:cControlName, cTxt, nRow, nCol)
        Return Nil
        }
   */
   // 3 - line: Special Header. Right mouse click
   oTsb:bSRClicked := {|nrp,ncp,nat,ob| bMClicked(.T., 3, ob, nrp, ncp, nat) }
   /*
   oTsb:bSRClicked := {|nrp,ncp,nat,ob| 
        Local nRow, nCol, cTxt := 'Special Header. Right mouse click'
        nRow := ob:GetTxtRow(nrp)       // table row cursor number      
        nCol := Max(ob:nAtCol(ncp), 1 ) // cursor column number in table
        MsgDebug(ob:cControlName, cTxt, nRow, nCol)
        Return Nil
        }
   */
   // 2 - line: Footer. Left mouse click
   oTsb:bFLClicked := {|nrp,ncp,nat,ob| bMClicked(.F., 2, ob, nrp, ncp, nat) }  
   /*
   oTsb:bFLClicked := {|nrp,ncp,nat,ob|  
        Local nRow, nCol, cTxt := 'Footer. Left mouse click'
        nRow  := ob:GetTxtRow(nrp)       // table row cursor number      
        nCol  := Max(ob:nAtCol(ncp), 1 ) // cursor column number in table
        MsgDebug(ob:cControlName, cTxt, nRow, nCol)
        Return Nil
        }
   */
   // 2 - line: Footer. Right mouse click
   oTsb:bFRClicked := {|nrp,ncp,nat,ob| bMClicked(.T., 2, ob, nrp, ncp, nat) }
   /*
   oTsb:bFRClicked := {|nrp,ncp,nat,ob| 
        Local nRow, nCol, cTxt := 'Footer. Right mouse click'
        nRow := ob:GetTxtRow(nrp)       // table row cursor number      
        nCol := Max(ob:nAtCol(ncp), 1 ) // cursor column number in table
        MsgDebug(ob:cControlName, cTxt, nRow, nCol)
        Return Nil
        }
   */
RETURN oTsb

FUNCTION bMClicked(lRight, nLine, oBrw, nYpix, nXpix, nAt)
   LOCAL nRow, nCol, cTxt, cMsg := "^1. ^2 mouse click", nFlag, lSupr
   LOCAL aMsg := {"HEADER", "CELL", "FOOTER", "SPEC. HEADER"}
   //    nLine ->     0        1        2            3
   nLine += 1 ; cTxt := StrTran( cMsg, "^1", aMsg[ nLine ] )
   nFlag := iif( nLine == 2, nAt, 0 )
   lSupr := nLine == 1 .and. nYpix < oBrw:nHeightSuper

   IF lSupr ; cTxt := "SUPER " + cTxt
   ENDIF

   nRow := oBrw:GetTxtRow (nYpix)       // table row cursor number      
   nCol := Max(oBrw:nAtCol(nXpix), 1 )  // cursor column number in table
   
   cTxt := StrTran( cTxt, "^2", iif( lRight, "Right", "Left" ) )
   
   MsgDebug(oBrw:cControlName, cTxt, nRow, nCol)

RETURN Nil
