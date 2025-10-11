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
   LOCAL cForm := "wMain"
   LOCAL nY, nX, nH, nW, a
   LOCAL oTsb1, oTsb2, aFile := {}, cFile

   USE ( "CUSTOMER3" )  ALIAS CUST_1  NEW SHARED
   INDEX ON CUSTNO TAG ID
   SET ORDER TO 0
   GO TOP
   AAdd( aFile, Select2Mem(.F.) )    // RecNo() != 0
   SET ORDER TO 1
   GO TOP

   USE ( "CUSTOMER3" )  ALIAS CUST_2  NEW SHARED
   SET ORDER TO 0
   GO TOP
   AAdd( aFile, Select2Mem(.T.) )    // RecNo() == 0
   SET ORDER TO 1
   GO TOP

   USE ( aFile[1] ) ALIAS CUST1 NEW SHARED
   SET RELATION TO ROWID INTO CUST_1
   GO TOP 
   USE ( aFile[2] ) ALIAS CUST2 NEW SHARED
   SET RELATION TO ROWID INTO CUST_2
   GO TOP 

   DEFINE WINDOW &cForm TITLE "Demo 2 TBrowse. DEMO8" ;
          MAIN NOSIZE TOPMOST ;
          ON INIT    ( This.Topmost := .F., _wPost(0) ) ;
          ON RELEASE ( This.Hide, _wSend(90) )
          This.Cargo := oHmgData()

      This.Maximize

      This.Cargo:aFile := aFile

      nY := nX := 0
      nW := This.ClientWidth
      nH := Int( This.ClientHeight / 2 )
      // 1.
      oTsb1 := App.Cargo:oTsb:Clone()
      oTsb1:cBrw   := "Brw_1"
      oTsb1:uAlias := "CUST1"
      oTsb1:nY     := nY
      oTsb1:nX     := nX
      oTsb1:nW     := nW
      oTsb1:nH     := nH
      // 
      oTsb1:aMoveCol  := {"COUNTRY", "ADDR1"}
      // SET RELATION ...
      oTsb1:aRelation := {"CUSTNO", ; // field name by relationship          
                          .T.     , ; // lock edit                           
                          "CUST_1", ; // alias relation                      
                          NIL     , ; // array of field names by relationship
                          NIL     , ; // array of column names               
                          NIL }       // column headers array                
      //
      // SuperHeader
      a := { oTsb1:cBrw, oTsb1:uAlias, oTsb1:aRelation }
      oTsb1:cSuperHd      := EVal(oTsb1:bSuperHdTxt, a, 1)
      oTsb1:aSuperHdColor := {CLR_YELLOW, CLR_HBLUE}
      oTsb1:lZebraLine    := .T.                      // all columns
      //
      nY += nH 
      nH -= 1
      // 2.
      oTsb2 := App.Cargo:oTsb:Clone()
      oTsb2:cBrw   := "Brw_2"
      oTsb2:uAlias := "CUST2"
      oTsb2:nY     := nY
      oTsb2:nH     := nH
      // 
      oTsb2:aMoveCol := {"COUNTRY", "COMPANY"}
      // SET RELATION ...
      oTsb2:aRelation := {"CUSTNO", ; // field name by relationship
                          .F.     , ; // lock edit
                          "CUST_2", ; // alias relation
                          NIL     , ; // array of field names by relationship
                          NIL     , ; // array of column names
                          NIL }       // column headers array
      // SuperHeader
      a := { oTsb2:cBrw, oTsb2:uAlias, oTsb2:aRelation }
      oTsb2:cSuperHd      := EVal(oTsb2:bSuperHdTxt, a, 2)
      oTsb2:aSuperHdColor := {CLR_HBLUE, CLR_YELLOW}
      oTsb2:lZebraLine    := .F.                      // only columns

      // create TBrowse 1,2
      This.Cargo:aBrw := _TBrowse({ oTsb1, oTsb2 })
      //
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
                            IF ob:IsEdit ; ob:SetFocus()
                            ELSE         ; _wSend(99)
                            ENDIF
                            Return Nil
                            }

      WITH OBJECT This.Object
       :Event( 0, {|ow| AEval(ow:Cargo:aBrw, {|ob| ob:Show() }), DoEvents() })
       :Event(90, {|ow| 
                   dbCloseAll()
                   hb_FileDelete("*.cdx") 
                   AEval(ow:Cargo:aFile, {|cf| dbDrop(cf, cf, "DBFCDX") })
                   FErase("mem")
                   Return Nil
                   })
       :Event(99, {|ow| ow:Release() })
      END WITH

   END WINDOW

   ACTIVATE WINDOW &cForm

RETURN NIL

STATIC FUNCTION Select2Mem(lMode, cFld)
   LOCAL nOld  := Select(), nFld, aRec, nRec
   LOCAL aRecs := {}, cAls  := Alias()
   LOCAL cFile := "mem:" + "Demo8_" + cAls
   Default cFld := "CUSTNO", lMode := .T.

   DO EVENTS
   nFld := FieldPos(cFld)
   GO TOP
   DO WHILE !EOF()
      DO EVENTS
      nRec := 0
      IF lMode
         IF RecNo() %2 == 0 ; nRec := RecNo()
         ENDIF
      ELSE
         IF RecNo() %2 != 0 ; nRec := RecNo()
         ENDIF
      ENDIF
      IF nRec > 0
         AAdd(aRecs, { FieldGet(nFld), Deleted() })
      ENDIF
      SKIP
   ENDDO
   GO TOP
   DO EVENTS
   dbDrop(cFile, cFile, "DBFCDX")
   dbCreate( cFile, {{"ROWID", "N", 10, 0}}, "DBFCDX", .T., cAls + "_" )
   FOR EACH aRec IN aRecs
       dbAppend()
       FieldPut(1, aRec[1])
       IF ! Empty( aRec[2] ) ; dbDelete()
       ENDIF
   NEXT
   dbGoTop()
   dbCloseArea()
   dbSelectArea(nOld)

RETURN cFile

*----------------------------------------------------------------------------*
INIT PROCEDURE Sets_ENV()
*----------------------------------------------------------------------------*
   LOCAL cFont := "Arial", nSize := 12, oac

   rddSetDefault( "DBFCDX" )

   SET DECIMALS  TO 4
   SET EPOCH     TO 2000
   SET DATE      TO GERMAN
   SET CENTURY   ON
   SET DELETED   OFF
   SET AUTOPEN   ON
   SET EXACT     ON
   SET EXCLUSIVE ON
   SET SOFTSEEK  ON
   SET OOP ON
   SET TOOLTIPSTYLE BALLOON

   SET MULTIPLE QUIT WARNING 
   SET WINDOW MAIN OFF
   SET NAVIGATION EXTENDED
   SET WINDOW MODAL PARENT HANDLE ON
   SET ShowRedAlert ON

   App.Cargo := oHmgData() ; oac := App.Cargo

   oac:lLogDel   := .T. 
   oac:cLogFile  := hb_FNameExtSet( App.ExeName, ".log" )
   oac:cPathDbf  := ".\"
   oac:nMenuBmpH := 24 

   SET FONT TO cFont, nSize

   _DefineFont("Normal"  , cFont, nSize  , .F., .F. )
   _DefineFont("Bold"    , cFont, nSize  , .T., .F. )
   _DefineFont("Italic"  , cFont, nSize-4, .F., .T. )
   // Alert* font
   _DefineFont("DlgFont" , cFont, nSize+2, .F., .F. )


   _SetGetLogFile( oac:cLogFile )

   IF oac:lLogDel ; hb_FileDelete( oac:cLogFile )
   ENDIF

   SET MENUSTYLE EXTENDED
   SetMenuBitmapHeight( oac:nMenuBmpH )
   //
   oac:oTsb := oHmgData()
   oac:oTsb:aEdit     := .F.
   oac:oTsb:aFoot     := .T.
   oac:oTsb:aNumber     := { 1, App.Object:W(0.6), DT_RIGHT, 6 }
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
   oac:oTsb:lZebra      := .T.
   oac:oTsb:lZebraLine  := .T.                      // all columns
   oac:oTsb:lZebraGroup := .T.
   oac:oTsb:cZebraGroup := "COUNTRY"
   //
   oac:oTsb:nHeightCell := App.Object:H(1.2)
   oac:oTsb:nHeightHead := App.Object:H(1.2)
   oac:oTsb:lSuperHd    := .T. 
   oac:oTsb:bSuperHdTxt := {|a,n| 
                            Local cMsg, cTxt
                            cMsg := a[1] + "." + a[2] + " -> " 
                            cMsg += Lower((a[2])->( dbInfo( DBI_FULLPATH ) )) 
                            cTxt := "SELECT: RecNo() %2 == 0"
                            IF n != 1 ; cTxt := StrTran(cTxt, "==", "!=" )
                            ENDIF
                            cMsg += space(3) + cTxt + space(3) 
                            cMsg += "RELATION TO ROWID INTO " + a[3][3]  
                            cMsg += " EDIT - " + iif( Empty(a[3][2]), "NO", "YES" )
                            Return cMsg
                            }

RETURN
