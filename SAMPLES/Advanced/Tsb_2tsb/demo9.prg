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
   LOCAL cForm := "wMain"
   LOCAL nY, nX, nH, nW
   LOCAL oTsb1, oTsb2, aFile := {}, cFile, aAls := {}
   LOCAL cAls1 := "CUST_1" , cAls2 := "CUST_2"
   LOCAL cDbf  := "CUSTOMER3", cID := "CUSTNO"
   LOCAL cSel1 := "RecNo() %2 != 0"
   LOCAL cSel2 := "RecNo() %2 == 0"
   LOCAL cTitl := " Select, Relation and Edit. " + MiniGuiVersion()

   Sets_TSB()           // App.Cargo:oTsb create

   cFile := App.Cargo:cPathDbf + cDbf

   USE ( cFile )  ALIAS ( cAls1 )  NEW SHARED
   AAdd( aFile, Select2Mem(cSel1, cID) ) 
   AAdd( aAls , StrTran(cAls1, "_", "") )
   GO TOP

   USE ( cFile )  ALIAS ( cAls2 )  NEW SHARED
   AAdd( aFile, Select2Mem(cSel2, cID) )    
   AAdd( aAls , StrTran(cAls2, "_", "") )
   GO TOP

   USE ( aFile[1] ) ALIAS ( aAls[1] ) NEW 
   GO TOP 
   USE ( aFile[2] ) ALIAS ( aAls[2] ) NEW 
   GO TOP 

   DEFINE WINDOW &cForm TITLE "Demo 2 TBrowse. DEMO9 " + cTitl ;
          MAIN NOSIZE TOPMOST ;
          ON INIT    ( This.Topmost := .F., _wPost(0) ) ;
          ON RELEASE ( This.Hide, _wSend(90) )
          This.Cargo := oHmgData()

      This.Maximize

      This.Cargo:aFile := aFile

      nY := nX := 0
      nW := This.ClientWidth
      nH := Int( This.ClientHeight / 2 )

      oTsb1 := App.Cargo:oTsb:Clone()
      oTsb1:cBrw     := "Brw_1"
      oTsb1:uAlias   := aAls[1]
      // SET RELATION ...
      oTsb1:cAlsFld  := cAls1      // relation alias
      oTsb1:aRelation := {cID  , ; // field name by relationship
                          .T.  , ; // lock edit
                          cAls1, ; // alias relation
                          NIL  , ; // array of field names by relationship
                          NIL  , ; // array of column names
                          NIL }    // column headers array
      oTsb1:nY := nY
      oTsb1:nX := nX
      oTsb1:nW := nW
      oTsb1:nH := nH
      oTsb1:cSuperHd := oTsb1:cBrw + "." + oTsb1:uAlias + " -> " + ;
                        Lower((oTsb1:uAlias)->( dbInfo( DBI_FULLPATH ) )) + ;
                        space(3) + "SELECT: " + cSel1 + space(3) + ;
                        "RELATION: TO ROWID INTO " +oTsb1:cAlsFld + ;
                        space(3) + "EDIT: "
      oTsb1:cSuperHd += iif( oTsb1:aRelation[2], "TRUE", "FALSE" ) 
      oTsb1:aSuperHdColor := {CLR_YELLOW, CLR_HBLUE}

      nY += nH 
      nH -= 1

      oTsb2 := App.Cargo:oTsb:Clone()
      oTsb2:cBrw     := "Brw_2"
      oTsb2:uAlias   := aAls[2]
      // SET RELATION ...
      oTsb2:cAlsFld   := cAls2     // relation alias
      oTsb2:aRelation := {cID  , ; // field name by relationship
                          .T.  , ; // lock edit
                          cAls2, ; // alias relation
                          NIL  , ; // array of field names by relationship
                          NIL  , ; // array of column names
                          NIL }    // column headers array
      oTsb2:nY := nY
      oTsb2:nH := nH
      oTsb2:cSuperHd := oTsb2:cBrw + "." + oTsb2:uAlias + " -> " + ;
                        Lower((oTsb2:uAlias)->( dbInfo( DBI_FULLPATH ) )) + ;
                        space(3) + "SELECT: " + cSel2 + space(3) + ;
                        "RELATION: TO ROWID INTO " +oTsb2:cAlsFld + ;
                        space(3) + "EDIT: "
      oTsb2:cSuperHd += iif( oTsb2:aRelation[2], "TRUE", "FALSE" )
      oTsb2:aSuperHdColor := {CLR_HBLUE, CLR_YELLOW}

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
                            IF ob:IsEdit ; ob:SetFocus()
                            ELSE         ; _wSend(99)
                            ENDIF
                            Return Nil
                            }

      WITH OBJECT This.Object
       :Event( 0, {|ow| AEval(ow:Cargo:aBrw, {|ob| ob:Show() }), DoEvents() })
       :Event(90, {|ow| 
                   dbCloseAll()
                   AEval(ow:Cargo:aFile, {|cf| dbDrop(cf, cf, "DBFCDX") })
                   FErase("mem")  
                   Return Nil
                   })
       :Event(99, {|ow| ow:Release() })
      END WITH

   END WINDOW

   ACTIVATE WINDOW &cForm

RETURN NIL

STATIC FUNCTION Select2Mem(bMode, cFld)
   LOCAL nOld  := Select(), nFld, aRec, nRec
   LOCAL aRecs := {}, cAls := Alias()
   LOCAL cFile := "mem:" + "Demo9_" + cAls
   Default cFld := "CUSTNO"

   IF IsChar( bMode ) ; bMode := &("{|| "+bMode + " }")
   ENDIF

   DO EVENTS
   nFld := FieldPos(cFld)
   GO TOP
   DO WHILE !EOF()
      DO EVENTS
      nRec := iif( EVal( bMode ), RecNo(), 0 )
      IF nRec > 0
         AAdd(aRecs, { RecNo(), Deleted() })
      ENDIF
      SKIP
   ENDDO
   GO TOP
   DO EVENTS
   dbDrop(cFile, cFile, "DBFCDX")
   dbCreate( cFile, {{"ROWNR", "N", 10, 0}}, "DBFCDX", .T., cAls + "_" )
   FOR EACH aRec IN aRecs
       dbAppend()
       FieldPut(1, aRec[1])
       IF ! Empty( aRec[2] ) ; dbDelete()
       ENDIF
   NEXT
   dbGoTop()
   dbCloseArea()
   dbSelectArea(nOld)
   DO EVENTS

RETURN cFile

STATIC FUNCTION Sets_TSB( oTsb )
   LOCAL oac := App.Cargo

   DEFAULT oac:oTsb := oTsb
   DEFAULT oac:oTsb := oHmgData()

   oac:oTsb:aEdit       := .F.
   oac:oTsb:aFoot       := .T.
   oac:oTsb:aNumber     := { 1, App.Object:W(0.6), DT_RIGHT, 7 }
   oac:oTsb:uSelector   := 20
   oac:oTsb:lHide       := .T.
   oac:oTsb:lZebra      := .T.
   oac:oTsb:lZebraLine  := .T.                      // all columns
   oac:oTsb:lZebraGroup := .T.
   oac:oTsb:cZebraGroup :=  "COUNTRY"
   oac:oTsb:aMoveCol    := {"COUNTRY", "ADDR1"}
   oac:oTsb:lSuperHd    := .T.
   oac:oTsb:b_Init_Def  := {|ob|
                             Local blk := {|obr|
                                       Local cAls := obr:Cargo:oParam:cAlsFld
                                       (cAls)->( dbGoTo((obr:cAlias)->ROWNR) )
                                       Return Nil
                                       }
                             ob:bOnDrawLine := { blk }
                             Return Nil
                             }
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
   oac:oTsb:nHeightHead  := App.Object:H(1.1)
   oac:oTsb:nHeightCell  := App.Object:H(1.1)
   oac:oTsb:nHeightFoot  := App.Object:H(1.1)
   oac:oTsb:nHeightSuper := App.Object:H(1.2)

   oTsb := oac:oTsb

RETURN oTsb

INIT PROCEDURE Sets_ENV()
   LOCAL cFont := "Arial", nSize := 12, oac

   rddSetDefault( "DBFCDX" )

   SET DECIMALS  TO 4
   SET EPOCH     TO 2000
   SET DATE      TO GERMAN
   SET CENTURY   ON
   SET DELETED   OFF
   SET AUTOPEN   OFF
   SET EXACT     ON
   SET EXCLUSIVE ON
   SET SOFTSEEK  ON
   SET OOP ON
   SET TOOLTIPSTYLE BALLOON

   SET MULTIPLE QUIT WARNING 
   SET NAVIGATION EXTENDED
   SET WINDOW MODAL PARENT HANDLE ON
   SET ShowRedAlert ON

   App.Cargo := oHmgData() ; oac := App.Cargo

   oac:lLogDel   := .T. 
   oac:cLogFile  := hb_FNameExtSet( App.ExeName, ".log" )
   oac:cPathDbf  := ".\"

   SET FONT TO cFont, nSize

   _DefineFont("Normal"  , cFont, nSize  , .F., .F. )
   _DefineFont("Bold"    , cFont, nSize  , .T., .F. )
   _DefineFont("Italic"  , cFont, nSize-4, .F., .T. )
   // Alert* font
   _DefineFont("DlgFont" , cFont, nSize+2, .F., .F. )

   _SetGetLogFile( oac:cLogFile )

   IF oac:lLogDel ; hb_FileDelete( oac:cLogFile )
   ENDIF

RETURN
