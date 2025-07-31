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
 * Demonstrates the use of TBrowse controls with data selection, relation, and editing capabilities.
 *
 * Purpose:
 *   This function demonstrates the use of two TBrowse controls to display and interact with data from a DBF file.
 *   It performs the following steps:
 *     1. Sets up the TBrowse environment using Sets_TSB().
 *     2. Opens two instances of the "CUSTOMER2" DBF file with different aliases and selection criteria.
 *     3. Creates two memory tables based on the selection criteria.
 *     4. Defines a main window with two TBrowse controls, each displaying data from one of the memory tables.
 *     5. Configures the TBrowse controls with specific properties, including aliases, relation fields, and editability.
 *     6. Sets up key bindings for navigation and exiting the application.
 *     7. Activates the main window, making it visible to the user.
 *
 * Notes:
 *   - The "CUSTOMER2" DBF file is assumed to exist in the application's directory.
 *   - The Sets_TSB() function is responsible for initializing the TBrowse environment and setting default properties.
 *   - The Select2Mem() function creates a memory table based on a selection criteria.
 *   - The application uses the App.Cargo object to store application-wide data, such as the TBrowse objects and file paths.
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
    //oTsb1:aSelFld  := NIL                   // FieldNames relation, array
      oTsb1:aSelFld  := {"COUNTRY", "STATE", "CITY", "COMPANY", "ADDR1"}
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
 * Creates a memory table containing records selected from the current database based on a given condition.
 *
 * Parameters:
 *   bMode (BLOCK or CHARACTER): A code block or character expression that defines the selection criteria.
 *                               If a code block, it should evaluate to .T. for records to be included.
 *                               If a character expression, it is converted to a code block.
 *   cFld (CHARACTER, optional): The name of the field to be used as the ROWID in the memory table. Defaults to "CUSTNO".
 *
 * Returns:
 *   CHARACTER: The alias of the newly created memory table.
 *
 * Purpose:
 *   This function is used to create a temporary memory table containing a subset of records from a DBF file,
 *   based on a specified selection criteria. This allows for filtering and manipulating data without directly
 *   modifying the original DBF file. The memory table includes a "ROWNR" field that stores the original record number
 *   from the DBF file, enabling a relation between the memory table and the original DBF.
 *
 * Notes:
 *   - The function uses dbCreate() to create the memory table, which is automatically opened.
 *   - The function uses dbAppend() and FieldPut() to add records to the memory table.
 *   - The function uses dbDelete() to mark records as deleted in the memory table if they were deleted in the original DBF.
 *   - The function uses dbDrop() to delete the memory table if it already exists.
 *   - The function uses dbCloseArea() to close the memory table after it has been created.
 *   - The function uses dbSelectArea() to restore the original selected work area.
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

/*
 * STATIC FUNCTION Sets_TSB( oTsb )
 *
 * Configures the default settings and event handlers for TBrowse objects used in the application.
 *
 * Parameters:
 *   oTsb (OBJECT, optional): An existing TBrowse object to configure. If not provided, a new oHmgData() object is created.
 *
 * Returns:
 *   OBJECT: The configured TBrowse object (either the provided object or the newly created one).
 *
 * Purpose:
 *   This function centralizes the configuration of TBrowse objects, ensuring consistency across the application.
 *   It sets default properties such as editability, footer visibility, zebra striping, and column numbering.
 *   It also defines event handlers for initialization, drawing lines, and gaining focus.
 *   This function promotes code reusability and simplifies the creation of TBrowse controls.
 *
 * Notes:
 *   - The function uses the App.Cargo object to store application-wide data, such as the default TBrowse settings.
 *   - The bInit codeblock is executed when the TBrowse object is initialized. It loads the fields to be displayed and sets up the record locking area.
 *   - The bOnDrawLine codeblock is executed when a line is drawn in the TBrowse object. It positions the cursor in the related DBF file.
 *   - The bAfter codeblock is executed after the TBrowse object is displayed. It sets the background color of deleted records.
 *   - The bGotFocus codeblock is executed when the TBrowse object gains focus. It sets the active window and updates the application's current TBrowse object.
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
                           IF lEdit ; ob:lRecLockArea := lEdit
                           ENDIF
                           ob:bOnDrawLine := {|obr|
                                     Local cAls := obr:Cargo:oParam:cAlsFld
                                     (cAls)->( dbGoTo((obr:cAlias)->ROWNR) )
                                     Return Nil
                                     }
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
 * Initializes the application environment, setting various system settings and defining fonts.
 *
 * Purpose:
 *   This procedure sets up the application's environment by configuring various system settings,
 *   such as the default RDD, date format, decimal precision, and font settings. It also defines
 *   application-specific settings, such as the log file path and whether to delete the log file on startup.
 *   This ensures that the application runs consistently across different systems and configurations.
 *
 * Notes:
 *   - The procedure sets the default RDD to DBFCDX, which is a common RDD for DBF files.
 *   - The procedure sets the date format to German.
 *   - The procedure sets various other system settings, such as SET DELETED OFF, SET EXACT ON, and SET SOFTSEEK ON.
 *   - The procedure defines three fonts: "Normal", "Bold", and "Italic".
 *   - The procedure creates an oHmgData() object and stores it in App.Cargo for application-wide data storage.
 *   - The procedure sets the log file path and whether to delete the log file on startup.
 */
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
