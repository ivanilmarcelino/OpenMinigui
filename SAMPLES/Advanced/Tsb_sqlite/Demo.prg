#include "minigui.ch"
#include "TSBrowse.ch"
#include "hbsqlit3.ch"
#include "dbinfo.ch"

ANNOUNCE RDDSYS
REQUEST SDDSQLITE3, SQLMIX, DBFNTX

*--------------------------------------------------------*
FUNCTION Main()
*--------------------------------------------------------*
   LOCAL cFileDb := hb_DirBase() + "Employee.s3db"
   LOCAL cTable := "Employee"

   rddSetDefault( "SQLMIX" )
   SET DELETED ON

   DEFINE WINDOW Form_0 ;
         AT 0, 0 ;
         WIDTH 640 HEIGHT 480 ;
         TITLE 'SQLITE3 Database Driver Demo' ;
         MAIN NOMAXIMIZE ;
         ON INIT InitDb( cFileDb, cTable )

      DEFINE MAIN MENU

         DEFINE POPUP 'File'
            ITEM "Exit" ACTION ThisWindow.Release()
         END POPUP
         DEFINE POPUP 'Tests'
            MENUITEM 'Editable' ACTION BrowseTable( cTable, 1 ) NAME BRW1
            MENUITEM 'Sbrowse' ACTION BrowseTable( cTable, 2 ) NAME BRW2
         END POPUP

      END MENU

   END WINDOW

   CENTER WINDOW Form_0

   ACTIVATE WINDOW Form_0

RETURN NIL

*--------------------------------------------------------*
FUNCTION InitDb( cFileDb, cTable )
*--------------------------------------------------------*
   LOCAL lRet := .T.

   if ! File( cFileDb )
      if ! CreatefromDBF( cFileDb, cTable )
         MsgStop( "Unable create SQL Database file from EMPLOYEE.DBF!", "Error" )
         lRet := .F.
      ENDIF
   ENDIF
   IF lRet
      IF rddInfo( RDDI_CONNECT, { "SQLITE3", hb_DirBase() + "Employee.s3db" } ) == 0
         MsgStop( "Unable connect to the server!", "Error" )
         lRet := .F.
      ENDIF
   ENDIF
   Form_0.Brw1.Enabled := lRet
   Form_0.Brw2.Enabled := lRet

RETURN NIL

*--------------------------------------------------------*
FUNCTION BrowseTable( cTable, mod )
*--------------------------------------------------------*
   LOCAL cSelect, bSetup, oBrw
   LOCAL nWinWidth := getdesktopwidth() * 0.8
   LOCAL nWinHeight := getdesktopheight() * 0.8
   LOCAL cTitle := "Table: " + cTable

   IF mod == 1
      cSelect := "SELECT * FROM " + cTable

      dbUseArea( .T.,, cSelect, "TABLE",,, "UTF8" )

      SELECT TABLE

      DEFINE WINDOW Form_1 ;
            AT 0, 0 ;
            WIDTH nWinWidth ;
            HEIGHT nWinHeight ;
            TITLE cTitle ;
            CHILD BACKCOLOR RGB( 191, 219, 255 )

         DEFINE TBROWSE oBrw AT 10, 10 ALIAS "TABLE" WIDTH nWinWidth - 26 HEIGHT nWinHeight - 70 ;
               AUTOCOLS SELECTOR 20 EDITABLE CELLED

            AEval( oBrw:aColumns, {| oCol | oCol:bPostEdit := {| uVal, oBr, lApp | SqlUpdate( uVal, oBr:nCell - 1, cTable, lApp ) } } )

            oBrw:SetAppendMode( .T. )
            oBrw:SetDeleteMode( .T., .T., {|| SqlDelete( cTable ) } )

            oBrw:nHeightCell += 2
            oBrw:nHeightHead += 5

         END TBROWSE

      END WINDOW

      CENTER WINDOW Form_1
      ACTIVATE WINDOW Form_1

      CLOSE TABLE
   ELSE
      bSetup := {| oBrw | SetMyBrowser( oBrw ) }
      SBrowse( cTable, cTitle, bSetup,, nWinWidth, nWinHeight, .T. )
   ENDIF

RETURN NIL

*--------------------------------------------------------*
STATIC FUNCTION SetMyBrowser( oBrw )
*--------------------------------------------------------*
   oBrw:nHeightCell += 5
   oBrw:nHeightHead += 5
   oBrw:nClrFocuFore := CLR_BLACK
   oBrw:nClrFocuBack := COLOR_GRID

RETURN .F.

*--------------------------------------------------------*
FUNCTION SqlUpdate( uVal, nCol, cTable, lApp )
*--------------------------------------------------------*
   LOCAL cQuery, i, cFldName, nStart, cQuery2
   LOCAL aDbStru := dbStruct()

   IF lApp
      cQuery := "INSERT INTO " + cTable + " ( "
      cQuery2 := " ) values ( "
      FOR i := 1 TO Len( aDbStru )
         cFldName := aDbStru[ i, 1 ]
         IF i > 1
            cQuery += " , "
            cQuery2 += " , "
         ENDIF
         cQuery += cFldName
         cQuery2 += c2sql( &cFldName )
      NEXT
      cQuery += cQuery2 + " )"

   ELSE
      nStart := if( nCol == 1, 2, 1 )
      cFldName := aDbStru[ nCol, 1 ]
      cQuery := "UPDATE " + cTable + " SET " + cFldName + " = " + c2sql( &cFldName ) + " WHERE "
      FOR i := 1 TO Len( aDbStru )
         IF i != nCol
            cFldName := aDbStru[ i, 1 ]
            IF i > nStart
               cQuery += " AND "
            ENDIF
            cQuery += cFldName + " = " + c2sql( &cFldName )
         ENDIF
      NEXT
      cQuery += " "
   ENDIF

   If ! rddInfo( RDDI_EXECUTE, cQuery )
      MsgStop( "Can't update record in table " + cTable + " !", "Error" )
      RETURN .F.
   ENDIF

RETURN .T.

*--------------------------------------------------------*
FUNCTION SqlDelete( cTable )
*--------------------------------------------------------*
   LOCAL cQuery, i, cFldName
   LOCAL aDbStru := dbStruct()

   cQuery := "DELETE FROM " + cTable + " WHERE "
   FOR i := 1 TO Len( aDbStru )
      cFldName := aDbStru[ i, 1 ]
      IF i > 1
         cQuery += " AND "
      ENDIF
      cQuery += cFldName + " = " + c2sql( &cFldName )
   NEXT
   cQuery += " "

   If ! rddInfo( RDDI_EXECUTE, cQuery )
      MsgStop( "Can't Delete record in table " + cTable + " !", "Error" )
      RETURN .F.
   ENDIF

RETURN .T.


*--------------------------------------------------------*
FUNCTION CreatefromDBF( cSbase, cDBase )
*--------------------------------------------------------*
   LOCAL cHeader, cQuery := "", NrReg := 0, cTable := cFileNoExt( cDBase )
   LOCAL lCreateIfNotExist := .T., cOldRdd, db, lRet := .F.

   IF ! File( hb_DirBase() + cDBase + ".dbf" )
      RETURN .F.
   ENDIF
   cTable := cDBase
   db := sqlite3_open( cSbase, lCreateIfNotExist )
   IF ! DB_IS_OPEN( db )
      MsgStop( "Can't open/create " + cSbase, "Error" )
      RETURN .F.
   ENDIF
   sqlite3_exec( db, "PRAGMA auto_vacuum=0" )

   // Create table
   IF ( rddSetDefault() != "DBFNTX" )
      cOldRdd := rddSetDefault( "DBFNTX" )
   ENDIF

   dbUseArea( .T., 'DBFNTX', cTable )
   cHeader := QueryCrea( cTable, 0 )
   IF sqlite3_exec( db, cHeader ) == SQLITE_OK
      GO TOP
      DO While ! Eof()
         cQuery += QueryCrea( cTable, 1 )
         NrReg++
         SKIP
      ENDDO
      USE

      IF sqlite3_exec( db, ;
            "BEGIN TRANSACTION;" + ;
            cQuery + ;
            "COMMIT;" ) == SQLITE_OK

         MsgInfo( AllTrim( Str( NrReg ) ) + " records added to table " + cTable, "Result" )
         lRet := .T.
      ENDIF

   ENDIF
   rddSetDefault( cOldRdd )

RETURN .T.

*--------------------------------------------------------*
FUNCTION QueryCrea( cDBase, met )
*--------------------------------------------------------*
   LOCAL cQuery := "", i
   LOCAL cFldName, cFldType, cFldLen, cFldDec
   LOCAL aDbStru := dbStruct()

   DO CASE
   CASE met == 0
      cQuery := "CREATE TABLE IF NOT EXISTS " + cDBase + " ( "
      FOR i := 1 TO Len( aDbStru )

         cFldName := aDbStru[ i, 1 ]
         cFldType := aDbStru[ i, 2 ]
         cFldLen := aDbStru[ i, 3 ]
         cFldDec := aDbStru[ i, 4 ]

         IF i > 1
            cQuery += ", "
         ENDIF
         cQuery += AllTrim( cFldName ) + " "

         DO CASE
         CASE cFldType = "C"
            cQuery += "CHAR(" + LTrim( Str( cFldLen ) ) + ")"
         CASE cFldType = "D"
            cQuery += "DATE"
         CASE cFldType = "T"
            cQuery += "DATETIME"
         CASE cFldType = "N"
            IF cFldDec > 0 .OR. cFldLen > 5
               cQuery += "FLOAT"
            ELSE
               cQuery += "INTEGER"
            ENDIF
         CASE cFldType = "F"
            cQuery += "FLOAT"
         CASE cFldType = "I"
            cQuery += "INTEGER"
         CASE cFldType = "B"
            cQuery += "DOUBLE"
         CASE cFldType = "Y"
            cQuery += "FLOAT"
         CASE cFldType = "L"
            cQuery += "BOOL"
         CASE cFldType = "M"
            cQuery += "TEXT"
         CASE cFldType = "G"
            cQuery += "BLOB"
         OTHERWISE
            msginfo( "Invalid Field Type: " + cFldType )
            RETURN NIL
         ENDCASE
      NEXT

      cQuery += ")"

   CASE met == 1

      cQuery := "INSERT INTO " + cDBase + " VALUES ("
      FOR i := 1 TO Len( aDbStru )
         cFldName := aDbStru[ i, 1 ]
         IF i > 1
            cQuery += ", "
         ENDIF
         cQuery += c2sql( &cFldName )
      NEXT
      cQuery += "); "

   ENDCASE

RETURN cQuery

*--------------------------------------------------------*
FUNCTION c2Sql( Value )
*--------------------------------------------------------*
   LOCAL cValue := ""
   LOCAL cdate := ""

   IF ValType( value ) == "C" .AND. Len( AllTrim( value ) ) > 0
      Value := StrTran( Value, "'", "''" )
   ENDIF
   DO CASE
   CASE ValType( Value ) == "N"
      cValue := hb_ntos( Value )
   CASE ValType( Value ) == "D"
      if ! Empty( Value )
         cdate := DToS( value )
         cValue := "'" + SubStr( cDate, 1, 4 ) + "-" + SubStr( cDate, 5, 2 ) + "-" + SubStr( cDate, 7, 2 ) + "'"
      ELSE
         cValue := "''"
      ENDIF
   CASE ValType( Value ) $ "CM"
      IF Empty( Value )
         cValue = "''"
      ELSE
         cValue := "'" + Value + "'"
      ENDIF
   CASE ValType( Value ) == "L"
      cValue := AllTrim( Str( iif( Value == .F., 0, 1 ) ) )
   OTHERWISE
      cValue := "''"
   ENDCASE

RETURN cValue
