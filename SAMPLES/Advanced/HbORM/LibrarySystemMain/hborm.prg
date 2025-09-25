/*

 BadaSystem
 Program       : HbORM - Harbour Object-Relational Mapping
 Module        : Class HbORM
 Compiler      : MINIGUI - Harbour Win32 GUI
 Compiler-C    : BCC 32 bit
 Author        : Marcos Jarrin
 Email         : marvijarrin@gmail.com
 Date          : 10/07/2025
 Update        : 06/08/2025
 Rev           : 0.1
 Description: A basic Object-Relational Mapping (ORM) library for working with DBF/CDX tables in Harbour.
               Provides methods to interact with DBF files, including opening, creating, indexing,
               and manipulating records.

*/

#include "hbclass.ch"
#include "fileio.ch"
#include "minigui.ch"

/**
 * Class HbORM
 * description Main ORM class for handling DBF/CDX table operations in Harbour.
 *              Provides methods for table management, record manipulation, and index handling.
 */
CLASS HbORM
   EXPORTED:
      VAR cTable      AS STRING    // Name of the DBF table file
      VAR cAlias      AS STRING    // Alias for the table
      VAR aStructure  AS ARRAY     // Array containing the table's structure
      VAR lOpen       AS LOGICAL   // Indicates whether the table is open (true) or closed (false)
      VAR cPath       AS STRING    // Path to the directory containing the table
      VAR aIndexes    AS ARRAY     // Array storing index information (tag, key, condition, uniqueness)

      METHOD New( cTable, cAlias, cPath ) CONSTRUCTOR
      METHOD Exists()
      METHOD Open(lShared)
      METHOD Close()
      METHOD Create( aStruct )
      METHOD AddIndex( cTag, cKey, cFor, lUnique )
      METHOD OpenIndexes()
      METHOD Find( xKey, cTag, lLast )
      METHOD Seek( xKey, cTag, lLast )
      METHOD Skip( nRecords )
      METHOD GoTo( nRecord )
      METHOD GoTop()
      METHOD GoBottom()
      METHOD RecCount()
      METHOD RecNo()
      METHOD Eof()
      METHOD Bof()
      METHOD GetValue( cField )
      METHOD GetRow()
      METHOD SetValue( cField, xValue )
      METHOD GetStruct()
      METHOD Insert( hData )
      METHOD Update( hData )
      METHOD Delete()
      METHOD Pack()
      METHOD Zap()
      METHOD GetError()
      METHOD SetOrder(cOrderField)

   PROTECTED:
      VAR cError      // Stores the last error message
      
      METHOD SetError( cErrorMsg )
ENDCLASS

/**
 * method New
 * description Constructor for the HbORM class. Initializes the ORM object with table name, alias, and path.
 * param {string} cTable - Name of the DBF table file (without extension)
 * param {string} [cAlias] - Optional alias for the table; defaults to cTable if not provided
 * param {string} [cPath] - Optional path to the table directory; defaults to current directory if not provided
 * returns {object} Self - Returns the initialized HbORM object
 */
METHOD New( cTable, cAlias, cPath ) CLASS HbORM

   LOCAL    cDefaultPath := "data\"

   // Initialize instance variables
   ::cTable     := cTable
   ::cAlias     := IIF( cAlias == NIL, cTable, cAlias )
   ::cPath      := IIF( cPath  == NIL, cDefaultPath, cPath )
   ::aStructure := {}
   ::aIndexes   := {}
   ::lOpen      := .F.
   ::cError     := ""

   cDefaultPath := CurDrive() + ":\" + CurDir() + "\" + ::cPath
   SET DEFAULT TO (cDefaultPath)
   SET PATH TO &(cDefaultPath)

   RETURN Self

/**
 * method Exists
 * description Checks if the DBF table file exists in the file system.
 * returns {logical} - True if the table file exists, false otherwise
 */
METHOD Exists() CLASS HbORM

   LOCAL cFullPath := ::cPath + ::cTable + ".dbf"

   // Check if the file exists
   IF FILE( cFullPath )
      RETURN .T.
   ENDIF

   // Also check without extension for compatibility
   IF FILE( ::cPath + ::cTable )
      RETURN .T.
   ENDIF

   ::SetError( "File not found: " + cFullPath )

   RETURN .F.

/**
 * method Open
 * description Opens the DBF table in shared or exclusive mode.
 * param {logical} [lShared=.T.] - Whether to open the table in shared mode (true) or exclusive mode (false)
 * returns {logical} - True if the table was opened successfully, false otherwise
 */
METHOD Open(lShared) CLASS HbORM

   LOCAL lSuccess  := .F.
   LOCAL cFullPath := ::cPath + ::cTable + ".dbf"

   DEFAULT lShared := .T.

   IF ::lOpen
      ::SetError( "The table is already open" )
      RETURN .F.
   ENDIF

   IF !FILE( cFullPath )
      ::SetError( "File not found: " + cFullPath )
      RETURN .F.
   ENDIF

   IF select(::cAlias) != 0
      ::cAlias := ::cAlias+ alltrim(str(HB_Random(99),2))
   ENDIF

   IF lShared
      USE (cFullPath) ALIAS (::cAlias) INDEX (::cTable) SHARED NEW
   ELSE
      USE (cFullPath) ALIAS (::cAlias) INDEX (::cTable) EXCLUSIVE NEW
   END

   IF USED()
      ::lOpen := .T.
      ::aStructure := DBSTRUCT()

      // Open indexes if they exist
      IF LEN( ::aIndexes ) > 0
         ::OpenIndexes()
      ENDIF

      lSuccess := .T.
   ELSE
      ::SetError( "Could not open the table: " + cFullPath )
   ENDIF

   RETURN lSuccess


/**
 * method Close
 * description Closes the currently open DBF table.
 * returns {logical} - True if the table was closed successfully, false if it was not open
 */
METHOD Close() CLASS HbORM

   IF !::lOpen
      ::SetError( "The table is not open" )
      RETURN .F.
   ENDIF
   
   SELECT (::cAlias)
   USE
   
   ::lOpen := .F.
   
   RETURN .T.

/**
 * method Create
 * description Creates a new DBF table with the specified structure.
 * param {array} aStruct - Array defining the table structure (field names, types, lengths, etc.)
 * returns {logical} - True if the table was created successfully, false otherwise
 */
METHOD Create( aStruct ) CLASS HbORM

   LOCAL cFullPath := ::cPath + ::cTable + ".dbf"
   LOCAL lSuccess := .F.
   
   IF ::lOpen
      ::SetError( "The table is already open; close it before creating a new one" )
      RETURN .F.
   ENDIF
   
   IF FILE( cFullPath )
      ::SetError( "The file already exists: " + cFullPath )
      RETURN .F.
   ENDIF
   
   IF EMPTY( aStruct )
      ::SetError( "The structure cannot be empty" )
      RETURN .F.
   ENDIF
   
   DBCREATE( cFullPath, aStruct, , .T., ::cAlias )
   ::lOpen := .T.

   IF FILE( cFullPath )
      ::aStructure := aStruct
      lSuccess := .T.
   ELSE
      ::SetError( "Could not create the table: " + cFullPath )
   ENDIF
   
   RETURN lSuccess

/**
 * method AddIndex
 * description Adds an index to the table, either immediately or when the table is opened.
 * param {string} cTag - Name of the index tag
 * param {string} cKey - Key expression for the index
 * param {string} [cFor] - Optional FOR condition for the index
 * param {logical} [lUnique=.F.] - Whether the index is unique
 * returns {logical} - True if the index was added successfully, false otherwise
 */
METHOD AddIndex( cTag, cKey, cFor, lUnique ) CLASS HbORM

   LOCAL cCdxFile := ::cTable
   LOCAL lSuccess := .F.
   
   DEFAULT cFor := ""
   DEFAULT lUnique := .F.
   
   IF EMPTY( cTag ) .OR. EMPTY( cKey )
      ::SetError( "Tag name and key expression are required" )
      RETURN .F.
   ENDIF

   // Store index information
   AADD( ::aIndexes, { cTag, cKey, cFor, lUnique } )
   
   // If the table is open, create the index immediately
   IF ::lOpen
      SELECT (::cAlias)
      
      IF !FILE( cCdxFile )
         IF EMPTY(cFor)
            DbCreateIndex( cCdxFile, cKey, , lUnique, cTag )
         ELSE
            INDEX ON &cKey TAG &cTag TO &cCdxFile FOR &cFor
         ENDIF
      ELSE
         IF EMPTY(cFor)
            DbCreateIndex( , cKey, , lUnique, cTag )
         ELSE
            INDEX ON &cKey TAG &cTag FOR &cFor
         ENDIF
      ENDIF
      
      lSuccess := .T.
   ELSE
      lSuccess := .T.  // Will be created when the table is opened
   ENDIF
   
   RETURN lSuccess

/**
 * method OpenIndexes
 * description Opens all indexes associated with the table.
 * returns {logical} - True if indexes were opened successfully, false otherwise
 */
METHOD OpenIndexes() CLASS HbORM

   LOCAL cCdxFile := ::cPath + ::cTable + ".cdx"

   IF !::lOpen
      ::SetError("The table is not open")
      RETURN .F.
   ENDIF

   IF FILE(cCdxFile)
      SELECT (::cAlias)
      SET INDEX TO (cCdxFile)
      RETURN .T.
   ENDIF

   RETURN .F.


/**
 * method Find
 * description Searches for a record by key value in the specified index.
 * param {any} xKey - Key value to search for
 * param {string} [cTag] - Optional index tag to use for the search
 * returns {logical} - True if the record was found, false otherwise
 */
METHOD Find( xKey, cTag, lLast ) CLASS HbORM

   DEFAULT lLast := .F.

   IF !::lOpen
      ::SetError( "The table is not open" )
      RETURN .F.
   ENDIF
   
   SELECT (::cAlias)
   
   IF cTag != NIL
      ORDSETFOCUS( cTag )
   ENDIF
   
   RETURN DBSEEK( xKey, , lLast )

/**
 * method Seek
 * description Alias for Find method; searches for a record by key value.
 * param {any} xKey - Key value to search for
 * param {string} [cTag] - Optional index tag to use for the search
 * returns {logical} - True if the record was found, false otherwise
 */
METHOD Seek( xKey, cTag, lLast ) CLASS HbORM
   RETURN ::Find( xKey, cTag, lLast )

/**
 * method Skip
 * description Moves the record pointer forward or backward by the specified number of records.
 * param {numeric} [nRecords=1] - Number of records to skip (positive for forward, negative for backward)
 * returns {logical} - True if the skip was successful, false if the table is not open
 */
METHOD Skip( nRecords ) CLASS HbORM
   DEFAULT nRecords TO 1
   
   IF !::lOpen
      ::SetError( "The table is not open" )
      RETURN .F.
   ENDIF
   
   SELECT (::cAlias)
   DBSKIP( nRecords )
   
   RETURN .T.

/**
 * method GoTo
 * description Moves the record pointer to the specified record number.
 * param {numeric} nRecord - Record number to move to
 * returns {logical} - True if the operation was successful, false if the table is not open
 */
METHOD GoTo( nRecord ) CLASS HbORM
   IF !::lOpen
      ::SetError( "The table is not open" )
      RETURN .F.
   ENDIF
   
   SELECT (::cAlias)
   DBGOTO( nRecord )
   
   RETURN .T.

/**
 * method GoTop
 * description Moves the record pointer to the first record in the table.
 * returns {logical} - True if the operation was successful, false if the table is not open
 */
METHOD GoTop() CLASS HbORM
   IF !::lOpen
      ::SetError( "The table is not open" )
      RETURN .F.
   ENDIF
   
   SELECT (::cAlias)
   DBGOTOP()
   
   RETURN .T.

/**
 * method GoBottom
 * description Moves the record pointer to the last record in the table.
 * returns {logical} - True if the operation was successful, false if the table is not open
 */
METHOD GoBottom() CLASS HbORM

   IF !::lOpen
      ::SetError( "The table is not open" )
      RETURN .F.
   ENDIF
   
   SELECT (::cAlias)
   DBGOBOTTOM()
   
   RETURN .T.

/**
 * method RecCount
 * description Returns the total number of records in the table.
 * returns {numeric} - Number of records, or 0 if the table is not open
 */
METHOD RecCount() CLASS HbORM

   IF !::lOpen
      ::SetError( "The table is not open" )
      RETURN 0
   ENDIF
   
   SELECT (::cAlias)
   RETURN LASTREC()

/**
 * method RecNo
 * description Returns the current record number.
 * returns {numeric} - Current record number, or 0 if the table is not open
 */
METHOD RecNo() CLASS HbORM

   IF !::lOpen
      ::SetError( "The table is not open" )
      RETURN 0
   ENDIF
   
   SELECT (::cAlias)
   RETURN RECNO()

/**
 * method Eof
 * description Checks if the record pointer is at the end of the table.
 * returns {logical} - True if at the end of the table or not open, false otherwise
 */
METHOD Eof() CLASS HbORM

   IF !::lOpen
      ::SetError( "The table is not open" )
      RETURN .T.
   ENDIF
   
   SELECT (::cAlias)
   RETURN EOF()

/**
 * method Bof
 * description Checks if the record pointer is at the beginning of the table.
 * returns {logical} - True if at the beginning of the table or not open, false otherwise
 */
METHOD Bof() CLASS HbORM

   IF !::lOpen
      ::SetError( "The table is not open" )
      RETURN .T.
   ENDIF
   
   SELECT (::cAlias)
   RETURN BOF()

/**
 * method GetValue
 * description Retrieves the value of the specified field in the current record.
 * param {string} cField - Name of the field
 * returns {any} - Value of the field, or NIL if the table is not open or the field does not exist
 */
METHOD GetValue( cField ) CLASS HbORM

   IF !::lOpen
      ::SetError( "The table is not open" )
      RETURN NIL
   ENDIF
   
   SELECT (::cAlias)
   
   IF EMPTY( FIELDPOS( cField ) )
      ::SetError( "The field does not exist: " + cField )
      RETURN NIL
   ENDIF
   
   RETURN FIELDGET( FIELDPOS( cField ) )

/**
 * method GetRow
 * description Retrieves all field values of the current record as a hash.
 * returns {hash} - Hash containing field names and their values, or NIL if no current record
 */
METHOD GetRow() CLASS HbORM

   LOCAL hRow := {=>}
   LOCAL nField

   IF !::lOpen
      ::SetError( "The table is not open" )
      RETURN NIL
   ENDIF

   SELECT (::cAlias)

   IF ::Eof() .OR. ::Bof()
      ::SetError( "No current record" )
      RETURN NIL
   ENDIF

   // Optimized version using AEval
   AEval(::aStructure, {|aField| hRow[aField[1]] := FIELDGET(FIELDPOS(aField[1]))})

   RETURN hRow

/**
 * method SetValue
 * description Sets the value of a field in the current record.
 * param {string} cField - Name of the field
 * param {any} xValue - Value to set
 * returns {logical} - True if the value was set successfully, false otherwise
 */
METHOD SetValue( cField, xValue ) CLASS HbORM
   LOCAL nPos
   
   IF !::lOpen
      ::SetError( "The table is not open" )
      RETURN .F.
   ENDIF
   
   SELECT (::cAlias)
   
   nPos := FIELDPOS( cField )
   IF EMPTY( nPos )
      ::SetError( "The field does not exist: " + cField )
      RETURN .F.
   ENDIF
   
   IF RLOCK()
      FIELDPUT( nPos, xValue )
      DBUNLOCK()
      RETURN .T.
   ELSE
      ::SetError( "Could not lock the record" )
   ENDIF
   
   RETURN .F.

/**
 * method GetStruct
 * description Returns the structure of the table.
 * returns {array} - Array containing the table's structure
 */
METHOD GetStruct() CLASS HbORM
   RETURN ::aStructure

/**
 * method Insert
 * description Inserts a new record with the provided data.
 * param {hash} hData - Hash containing field names and values to insert
 * returns {logical} - True if the record was inserted successfully, false otherwise
 */
METHOD Insert( hData ) CLASS HbORM

   LOCAL cField, xValue
   LOCAL lSuccess := .F.
   LOCAL nPos

   IF !::lOpen
      ::SetError( "The table is not open" )
      RETURN .F.
   ENDIF
   
   SELECT (::cAlias)
   
   APPEND BLANK
   
   IF RLOCK()
      lSuccess := .T.
      
      // Iterate through the hash to set values
      HEval( hData, {|cField, xValue|
         nPos := FIELDPOS( cField )

         IF nPos > 0
            FIELDPUT( nPos, xValue )
         ELSE
            ::SetError( "Field not found: " + cField )
            lSuccess := .F.
            BREAK
         ENDIF
         RETURN NIL
      })

      DBUNLOCK()
   ELSE
      ::SetError( "Could not lock the record" )
   ENDIF
   
   RETURN lSuccess

/**
 * method Update
 * description Updates the current record with the provided data.
 * param {hash} hData - Hash containing field names and values to update
 * returns {logical} - True if the record was updated successfully, false otherwise
 */
METHOD Update(hData) CLASS HbORM

   LOCAL lSuccess := .F.
   LOCAL lError := .F.
   LOCAL cError := ""

   IF !::lOpen
      ::SetError("The table is not open")
      RETURN .F.
   ENDIF

   SELECT (::cAlias)

   IF RLOCK()
      lSuccess := .T.

      // Iterate through the hash using HEval()
      HEval(hData, {|cField, xValue|
         IF !lError  // Only continue if no prior errors
            IF !EMPTY(FIELDPOS(cField))
               FIELDPUT(FIELDPOS(cField), xValue)
            ELSE
               cError := "Field not found: " + cField
               lError := .T.
               lSuccess := .F.
            ENDIF
         ENDIF
         RETURN NIL
      })

      // Set error message if an error occurred
      IF lError
         ::SetError(cError)
      ENDIF

      DBUNLOCK()
   ELSE
      ::SetError("Could not lock the record")
   ENDIF

   RETURN lSuccess

/**
 * method Delete
 * description Marks the current record as deleted.
 * returns {logical} - True if the record was marked for deletion, false otherwise
 */
METHOD Delete() CLASS HbORM

   IF !::lOpen
      ::SetError( "The table is not open" )
      RETURN .F.
   ENDIF
   
   SELECT (::cAlias)
   
   IF RLOCK()
      DBDELETE()
      DBUNLOCK()
      RETURN .T.
   ELSE
      ::SetError( "Could not lock the record" )
   ENDIF
   
   RETURN .F.

/**
 * method Pack
 * description Physically removes records marked as deleted from the table.
 * returns {logical} - True if the pack operation was successful, false if the table is not open
 */
METHOD Pack() CLASS HbORM

   IF !::lOpen
      ::SetError( "The table is not open" )
      RETURN .F.
   ENDIF
   
   SELECT (::cAlias)
   PACK
   
   RETURN .T.

/**
 * method Zap
 * description Deletes all records from the table.
 * returns {logical} - True if the zap operation was successful, false if the table is not open
 */
METHOD Zap() CLASS HbORM

   IF !::lOpen
      ::SetError( "The table is not open" )
      RETURN .F.
   ENDIF
   
   SELECT (::cAlias)
   ZAP
   
   RETURN .T.

/**
 * method SetError
 * description Sets the last error message for the ORM.
 * param {string} cErrorMsg - Error message to store
 * returns {NIL}
 */
METHOD SetError( cErrorMsg ) CLASS HbORM
   ::cError := cErrorMsg
   RETURN NIL

/**
 * method GetError
 * description Retrieves the last error message.
 * returns {string} - The last error message
 */
METHOD GetError() CLASS HbORM
   RETURN ::cError

/**
 * method SetOrder
 * description Sets the order of records using an existing index or creates a new one.
 * param {string} cOrderField - Field name to order by
 * returns {logical} - True if the order was set successfully, false if the table is not open
 */
METHOD SetOrder(cOrderField) CLASS HbORM

   LOCAL cTag := Upper(cOrderField)
   LOCAL lFound := .F.
   LOCAL nIndex

   IF !::lOpen
      ::SetError("The table is not open")
      RETURN .F.
   ENDIF

   SELECT (::cAlias)

   // Check if the index already exists
   FOR nIndex := 1 TO Len(::aIndexes)
      IF ::aIndexes[nIndex][1] == cTag
         lFound := .T.
         EXIT
      ENDIF
   NEXT

   // If not found, create the index automatically
   IF !lFound
      ::AddIndex(cTag, cOrderField)
   ENDIF

   // Set the order
   ORDSETFOCUS(cTag)

   RETURN .T.
