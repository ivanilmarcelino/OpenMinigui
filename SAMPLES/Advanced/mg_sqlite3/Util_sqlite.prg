/*
 * SQLite3 Demo
 *
 * Copyright 2007 Grigory Filatov <gfilatov@inbox.ru>
 * SAMPLES\Advanced\Sqlite_2
 * Editing the code 03.10.2024 Verchenko Andrey <verchenkoag@gmail.com>
*/
#include "minigui.ch"
#include "hbsqlit3.ch"
*---------------------------------------------------------------------------
FUNCTION SQLiteVersion()
RETURN "Version library = " + sqlite3_libversion() + ;
       "  (" + HB_NtoS( sqlite3_libversion_number() ) + ")"

*---------------------------------------------------------------------------
* Uses a (special) master table where the names of all tables are stored
* Returns an array with names of tables inside of the database
*---------------------------------------------------------------------------
FUNCTION SQLITE_TABLES(cFile)
  LOCAL aTables, cStatement
  LOCAL lCreateIfNotExist := .f.
  LOCAL db := sqlite3_open( cFile, lCreateIfNotExist )

  cStatement := "SELECT name FROM sqlite_master "      +;
                "WHERE type IN ('table','view') "      +;
                "AND name NOT LIKE 'sqlite_%' "        +;
                "UNION ALL "                           +;
                "SELECT name FROM sqlite_temp_master " +;
                "WHERE type IN ('table','view') "      +;
                "ORDER BY 1;"

  IF DB_IS_OPEN( db )
    aTables := SQLITE_QUERY( db, cStatement )
    db      := NIL // close database
  ENDIF

RETURN( aTables )

*---------------------------------------------------------------------------
* Uses a (special) master table where the names of all tables are stored
* Returns an array with names of indexes inside of the database
*---------------------------------------------------------------------------
FUNCTION SQLITE_INDEXES( cTable, cFile )
   LOCAL aIndexes, cStatement
   LOCAL lCreateIfNotExist := .f.
   LOCAL db := sqlite3_open( cFile, lCreateIfNotExist )

   cStatement := "SELECT name FROM sqlite_master "      +;
                 "WHERE type='index' "                  +;
                 "AND name NOT LIKE 'sqlite_%' "        +;
                 "AND tbl_name='" + cTable + "' "       +;
                 "UNION ALL "                           +;
                 "SELECT name FROM sqlite_temp_master " +;
                 "WHERE type='index' "                  +;
                 "AND tbl_name='" + cTable + "' "       +;
                 "ORDER BY 1;"

   IF DB_IS_OPEN( db )
     aIndexes := SQLITE_QUERY( db, cStatement )
     db := NIL // close database
   ENDIF

RETURN( aIndexes )

*---------------------------------------------------------------------------
* Uses a (special) master table where the names of all tables are stored
*---------------------------------------------------------------------------
 FUNCTION SQLITE_TABLEEXISTS( cTable )
  LOCAL cStatement, lRet := .f.
  LOCAL lCreateIfNotExist := .f.
  LOCAL db := sqlite3_open( "test.db", lCreateIfNotExist )

  cStatement := "SELECT name FROM sqlite_master "    +;
                "WHERE type ='table' AND tbl_name='" +;
                cTable + "'"

  IF DB_IS_OPEN( db )
    lRet := ( LEN( SQLITE_QUERY( db, cStatement ) ) > 0 )
    db := NIL // close database
  ENDIF

RETURN( lRet )

*---------------------------
 FUNCTION SQLITE_DROPTABLE()
*---------------------------------------------------------------------------
* Deletes a table from current database
* WARNING !!   It deletes forever...
*---------------------------------------------------------------------------
  LOCAL db, lRet := .F.
  LOCAL cTable := "" //ShowTables()

  IF !EMPTY(cTable)
   IF MsgYesNo("The selected table will be erased" + CRLF + ;
      "without any choice to recover." + CRLF + CRLF + ;
      "       Continue ?", "Warning!" )

      db := sqlite3_open_v2( "test.db", SQLITE_OPEN_READWRITE + SQLITE_OPEN_EXCLUSIVE )
      IF DB_IS_OPEN( db )
         IF sqlite3_exec( db, "drop table " + cTable ) == SQLITE_OK
            IF sqlite3_exec( db, "vacuum" ) == SQLITE_OK
               lRet := .T.
            ENDIF
         ENDIF
      ENDIF
   ENDIF
  ENDIF

RETURN lRet

*---------------------------------
 FUNCTION SQLITE_FIELDS( cTable )
*---------------------------------------------------------------------------
* Returns an unidimensional array with field names only
*---------------------------------------------------------------------------
  LOCAL aFields := {}, cStatement := "SELECT * FROM " + cTable
  LOCAL lCreateIfNotExist := .f.
  LOCAL db := sqlite3_open( "test.db", lCreateIfNotExist )
  LOCAL stmt, nCCount, nI

  stmt := sqlite3_prepare( db, cStatement )

  sqlite3_step( stmt )
  nCCount := sqlite3_column_count( stmt )

  IF nCCount > 0
   FOR nI := 1 TO nCCount
           AADD( aFields, sqlite3_column_name( stmt, nI ) )
   NEXT nI
  ENDIF

  sqlite3_finalize( stmt )

RETURN( aFields )

*--------------------------------
 FUNCTION SQLITE_COLUMNS( cTable )
*---------------------------------------------------------------------------
* Returns an 2-dimensional array with field names and types
*---------------------------------------------------------------------------
  LOCAL aCType :=  { "SQLITE_INTEGER", "SQLITE_FLOAT", "SQLITE_TEXT", "SQLITE_BLOB", "SQLITE_NULL" }
  LOCAL aFields := {}, cStatement := "SELECT * FROM " + cTable
  LOCAL lCreateIfNotExist := .f.
  LOCAL db := sqlite3_open( "test.db", lCreateIfNotExist )
  LOCAL stmt, nCCount, nI, nCType

  stmt := sqlite3_prepare( db, cStatement )

  sqlite3_step( stmt )
  nCCount := sqlite3_column_count( stmt )

  IF nCCount > 0
   FOR nI := 1 TO nCCount
      nCType := sqlite3_column_type( stmt, nI )
           AADD( aFields, { sqlite3_column_name( stmt, nI ), aCType[ nCType ] } )
   NEXT nI
  ENDIF

  sqlite3_finalize( stmt )

RETURN( aFields )

*--------------------------------------
 FUNCTION SQLITE_QUERY( db, cStatement )
*---------------------------------------------------------------------------
  LOCAL stmt, nCCount, nI, nCType
  LOCAL aRet := {}

  stmt := sqlite3_prepare( db, cStatement )

  IF STMT_IS_PREPARED( stmt )
    DO WHILE sqlite3_step( stmt ) == SQLITE_ROW
   nCCount := sqlite3_column_count( stmt )

   IF nCCount > 0
      FOR nI := 1 TO nCCount
         nCType := sqlite3_column_type( stmt, nI )

         SWITCH nCType
            CASE SQLITE_NULL
                    AADD( aRet, "NULL")
               EXIT

            CASE SQLITE_FLOAT
            CASE SQLITE_INTEGER
                    AADD( aRet, LTRIM(STR( sqlite3_column_int( stmt, nI ) )) )
               EXIT

            CASE SQLITE_TEXT
                  AADD( aRet, sqlite3_column_text( stmt, nI ) )
               EXIT
         END SWITCH
      NEXT nI
   ENDIF
    ENDDO
    sqlite3_finalize( stmt )
  ENDIF

RETURN( aRet )

*---------------------------------
 FUNCTION SQLITE_RECCOUNT( cTable, cFile )
*---------------------------------------------------------------------------
* Returns an records count in the specified table of SQLITE database
*---------------------------------------------------------------------------
  LOCAL aRet, cStatement := "SELECT count(*) FROM " + cTable
  LOCAL lCreateIfNotExist := .f.
  LOCAL db := sqlite3_open( cFile, lCreateIfNotExist )

  aRet := SQLITE_QUERY( db, cStatement )
  db   := NIL // close database

RETURN( Val(aRet[ 1 ]) )

// Returns an SQL string with clipper value converted ie. Date() -> "'YYYY-MM-DD'"
FUNCTION ClipValue2SQL( Value )

   SWITCH ValType( Value )
   CASE "N"
      RETURN hb_ntos( Value )

   CASE "D"
      IF Empty( Value )
         RETURN "''"
      ELSE
         /* SQL dates are like YYYY-MM-DD */
         RETURN "'" + StrZero( Year( Value ), 4 ) + "-" + StrZero( Month( Value ), 2 ) + "-" + StrZero( Day( Value ), 2 ) + "'"
      ENDIF

   CASE "C"
   CASE "M"
      IF Empty( Value )
         RETURN "''"
      ELSE
         RETURN "'" + value + "'"
      ENDIF

   CASE "L"
      RETURN iif( Value, "1", "0" )

   CASE "U"
      RETURN "NULL"

   ENDSWITCH

   RETURN "''"       // NOTE: Here we lose values we cannot convert
