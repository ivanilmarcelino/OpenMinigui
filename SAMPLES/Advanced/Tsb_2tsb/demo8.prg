/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 */
#define  _HMG_OUTLOG

#include "hmg.ch"
#include "tsbrowse.ch"
#include "dbinfo.ch"

REQUEST DBFCDX

/*
 * FUNCTION Main()
 *
 * Demonstrates the use of two TBrowse objects to display data from two related DBF files.
 *
 * Purpose:
 *   This function creates a main window containing two TBrowse objects. Each TBrowse displays data from a different DBF file ("CUSTOMER2" accessed via aliases CUST1 and CUST2).
 *   The TBrowse objects are related through a relation set up between the DBF files.
 *   The function demonstrates how to use the _TBrowse() function to manage multiple TBrowse objects within a single window and how to switch focus between them using the TAB key.
 *   It also shows how to handle the ESCAPE key to exit edit mode or close the window.
 *   The function uses temporary DBF files created in memory to filter the data displayed in each TBrowse (one showing records where RecNo() % 2 != 0, the other where RecNo() % 2 == 0).
 *   This example showcases the power of HMG Extended in creating complex data-driven user interfaces.
 *
 * Notes:
 *   The function relies on the Sets_TSB() function to configure the TBrowse objects.
 *   The CUSTOMER2.DBF file must exist in the same directory as the executable.
 *   The temporary DBF files are deleted when the window is closed.
 */
FUNCTION Main()
   LOCAL cForm := "wMain"
   LOCAL nY, nX, nH, nW
   LOCAL oTsb1, oTsb2, aFile := {}, cFile, aAls := {}
   LOCAL cAls1 := "CUST_1" , cAls2 := "CUST_2"
   LOCAL cDbf  := "CUSTOMER2", cID := "CUSTNO"
   LOCAL cSel1 := "RecNo() %2 != 0"
   LOCAL cSel2 := "RecNo() %2 == 0"
   LOCAL cTitl := " Select, Relation and Edit. " + MiniGuiVersion()

   Sets_TSB()           // App.Cargo:oTsb create

   cFile := App.Cargo:cPathDbf + cDbf

   USE ( cFile )  ALIAS ( cAls1 )  NEW SHARED
   INDEX ON &cID TAG ID
   SET ORDER TO 1
   GO TOP
   AAdd( aFile, Select2Mem(cSel1, cID) ) 
   AAdd( aAls , StrTran(cAls1, "_", "") )
   GO TOP

   USE ( cFile )  ALIAS ( cAls2 )  NEW SHARED
   SET ORDER TO 1
   GO TOP
   AAdd( aFile, Select2Mem(cSel2, cID) )    
   AAdd( aAls , StrTran(cAls2, "_", "") )
   GO TOP

   USE ( aFile[1] ) ALIAS ( aAls[1] ) NEW SHARED
   SET RELATION TO ROWID INTO &cAls1
   GO TOP 
   USE ( aFile[2] ) ALIAS ( aAls[2] ) NEW SHARED
   SET RELATION TO ROWID INTO &cAls2
   GO TOP 

   DEFINE WINDOW &cForm TITLE "Demo 2 TBrowse." + cTitl ;
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
      oTsb1:cAlsFld  := cAls1                 // relation
      oTsb1:cAlsKey  := NIL                   // field relation
      oTsb1:lAlsEdit := !Empty(oTsb1:cAlsKey) // lock edit
      oTsb1:aSelFld  := NIL                   // FieldNames relation, array
      oTsb1:nY := nY
      oTsb1:nX := nX
      oTsb1:nW := nW
      oTsb1:nH := nH
      oTsb1:lSuperHd := .T.
      oTsb1:cSuperHd := oTsb1:cBrw + "." + oTsb1:uAlias + " -> " + ;
                        Lower((oTsb1:uAlias)->( dbInfo( DBI_FULLPATH ) )) + ;
                        space(3) + "SELECT: " + cSel1 + space(3) + ;
                        "RELATION: TO ROWID INTO " +oTsb1:cAlsFld + ;
                        space(3) + "EDIT: "
      oTsb1:cSuperHd += iif( oTsb1:lAlsEdit, "TRUE", "FALSE" ) 
      oTsb1:aSuperHdColor := {CLR_YELLOW, CLR_HBLUE}

      nY += nH 
      nH -= 1

      oTsb2 := App.Cargo:oTsb:Clone()
      oTsb2:cBrw     := "Brw_2"
      oTsb2:uAlias   := aAls[2]
      oTsb2:cAlsFld  := cAls2                 // relation
      oTsb2:cAlsKey  := cID                   // field relation
      oTsb2:lAlsEdit := !Empty(oTsb2:cAlsKey) // lock edit
      oTsb2:aSelFld  := NIL                   // FieldNames relation, array 
      oTsb2:lZebra   := .F.
      oTsb2:lChess   := .T.
      oTsb2:nY := nY
      oTsb2:nH := nH
      oTsb2:lSuperHd := .T.
      oTsb2:cSuperHd := oTsb2:cBrw + "." + oTsb2:uAlias + " -> " + ;
                        Lower((oTsb2:uAlias)->( dbInfo( DBI_FULLPATH ) )) + ;
                        space(3) + "SELECT: " + cSel2 + space(3) + ;
                        "RELATION: TO ROWID INTO " +oTsb2:cAlsFld + ;
                        space(3) + "EDIT: "
      oTsb2:cSuperHd += iif( oTsb2:lAlsEdit, "TRUE", "FALSE" )
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

/*
 * STATIC FUNCTION Select2Mem(bMode, cFld)
 *
 * Creates a temporary in-memory DBF file containing records from the current work area based on a filter condition.
 *
 * Parameters:
 *   bMode (BLOCK): A code block (closure) that defines the filter condition.  It should evaluate to .T. for records to be included in the temporary DBF.
 *   cFld (CHARACTER, optional): The field to copy to the new DBF. Defaults to "CUSTNO".
 *
 * Returns:
 *   CHARACTER: The alias of the newly created in-memory DBF file (e.g., "mem:ALIAS").
 *
 * Purpose:
 *   This function is used to create temporary DBF files in memory that contain a subset of the data from an existing DBF file.
 *   This allows for filtering and displaying specific records in TBrowse objects without modifying the original DBF file.
 *   The function iterates through the records in the current work area, applies the filter condition defined by the bMode code block, and copies the specified field (cFld) and deletion status to the new in-memory DBF.
 *   The new DBF file is created with a single field named "ROWID" of type Numeric.
 *   This function is crucial for creating dynamic views of data based on specific criteria.
 *
 * Notes:
 *   The function uses the dbDrop() function to delete any existing DBF file with the same alias before creating the new one.
 *   The function uses dbCreate() to create the new in-memory DBF file.
 *   The function uses dbAppend() and FieldPut() to add records to the new DBF file.
 *   The function uses dbDelete() to mark records as deleted in the new DBF file if they were deleted in the original DBF file.
 *   The function restores the original work area after creating the new DBF file.
 *   The bMode parameter *must* be a valid code block that can be evaluated in the context of the current work area.
 */
STATIC FUNCTION Select2Mem(bMode, cFld)
   LOCAL nOld  := Select()
   LOCAL aRecs := {}, cAls := Alias()
   LOCAL cFile := "mem:" + cAls, nFld, aRec, nRec
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
   DO EVENTS

RETURN cFile

/*
 * STATIC FUNCTION Sets_TSB( oTsb )
 *
 * Configures default settings for TBrowse objects used in the application.
 *
 * Parameters:
 *   oTsb (OBJECT, optional): An existing TBrowse object to configure. If NIL, a new oHmgData() object is created.
 *
 * Returns:
 *   OBJECT: The configured TBrowse object (either the passed object or the newly created one).
 *
 * Purpose:
 *   This function centralizes the configuration of common TBrowse settings, promoting code reuse and consistency.
 *   It sets properties such as zebra striping, column numbering, and event handlers for initialization, after-browse operations, and focus changes.
 *   The function also defines closures (code blocks) for customizing the appearance and behavior of the TBrowse objects.
 *   This allows for dynamic modification of the TBrowse based on the data being displayed.
 *   By encapsulating these settings, the application can easily maintain a uniform look and feel for all TBrowse controls.
 *
 * Notes:
 *   The function uses the App.Cargo object to store the TBrowse settings.
 *   The bInit closure is responsible for loading the fields into the TBrowse and handling column customization.
 *   The bAfter closure is responsible for customizing the appearance of the TBrowse after it has been loaded.
 *   The bGotFocus closure is responsible for setting the focus to the TBrowse object and updating the window title.
 */
STATIC FUNCTION Sets_TSB( oTsb )
   LOCAL oac := App.Cargo

   DEFAULT oac:oTsb := oTsb, oac:oTsb := oHmgData()

   oac:oTsb:aEdit     := .F.
   oac:oTsb:aFoot     := .T.
   oac:oTsb:lZebra    := .T.
   oac:oTsb:aNumber   := { 1, App.Object:W(0.5) }
   oac:oTsb:uSelector := 20
   oac:oTsb:bInit     := {|ob,op|
                           Local cn := op:cAlsKey, lEdit, oc
                           ob:Hide()
                           lEdit := !Empty(op:lAlsEdit)
                           ob:LoadFields(lEdit, op:aSelFld, op:cAlsFld)
                           IF !Empty(cn) .and. ob:nColumn(cn, .T.) > 0
                              ob:DelColumn(cn)
                           ENDIF
                           IF lEdit ; ob:lRecLockArea := lEdit
                           ENDIF
                           Return Nil
                           }
   oac:oTsb:bAfter    := {|ob|
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

   oac:oTsb:nHeightCell := App.Object:H(1.2)
   oac:oTsb:nHeightHead := App.Object:H(1.2)

   oTsb := oac:oTsb

RETURN oTsb

/*
 * INIT PROCEDURE Sets_ENV()
 *
 * Initializes the application environment, setting various system parameters and defining fonts.
 *
 * Purpose:
 *   This procedure sets up the Harbour MiniGUI Extended Edition (HMG Extended) environment for the application.
 *   It configures the default RDD (Replaceable Database Driver), date and time formats, display settings, and other system parameters.
 *   It also defines custom fonts for use throughout the application, ensuring a consistent look and feel.
 *   The procedure also sets up logging functionality, creating a log file to record application events and errors.
 *   This initialization ensures that the application operates correctly and provides a consistent user experience.
 *
 * Notes:
 *   The procedure uses the rddSetDefault() function to set the default RDD to DBFCDX.
 *   The procedure uses the SET command to configure various system parameters.
 *   The procedure uses the _DefineFont() function to define custom fonts.
 *   The procedure uses the _SetGetLogFile() function to set the log file.
 *   The procedure uses the hb_FileDelete() function to delete the log file if it already exists.
 */
INIT PROCEDURE Sets_ENV()
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
