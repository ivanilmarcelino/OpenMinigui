/*
   Demo Program: Integration of MiniGUI and MySQL
   Description: This program demonstrates how to use Harbour MiniGUI
      to create a GUI application that connects to a MySQL database.
*/

#include "hmg.ch"
#include "mysql.ch"

FUNCTION Main()

   LOCAL cServer   := "localhost"
   LOCAL cUser     := "root"
   LOCAL cPassword := "mypass"
   LOCAL cDatabase := "world"
   LOCAL nPort     := 3306
   LOCAL nFlags    := 0
   LOCAL nConn     := 0

   DEFINE WINDOW Form_1 ;
      AT 0,0 ;
      WIDTH 640 ;
      HEIGHT 480 ;
      TITLE 'MySQL Integration with HMG Extended' ;
      MAIN

      DEFINE BUTTON Button_Connect
         ROW    10
         COL    10
         WIDTH  120
         HEIGHT 24
         CAPTION 'Connect'
         ACTION  ConnectToDatabase( @nConn, cServer, cUser, cPassword, cDatabase, nPort, nFlags )
      END BUTTON

      DEFINE BUTTON Button_Retrieve
         ROW    10
         COL    140
         WIDTH  120
         HEIGHT 24
         CAPTION 'Retrieve Data'
         ACTION  RetrieveAndDisplayData( nConn )
      END BUTTON
      Form_1.Button_Retrieve.Enabled := .F.

      DEFINE GRID Grid_Data
         ROW    50
         COL    10
         WIDTH  620
         HEIGHT 400
         //HEADERS {}
         //WIDTHS {}
         READONLY .T.
         CELLNAVIGATION .T.
      END GRID

   END WINDOW

   CENTER WINDOW Form_1
   ACTIVATE WINDOW Form_1

RETURN

// Function to connect to MySQL database
PROCEDURE ConnectToDatabase( nConn, cServer, cUser, cPassword, cDatabase, nPort, nFlags )

   nConn := MYSQL_REAL_CONNECT( cServer, cUser, cPassword, nPort, nFlags )

   IF ! Empty( nConn )
      //MsgInfo( "Connection to MySQL is successful!" )
      IF mysql_select_db( nConn, cDatabase ) == 0  /* Database exists */
         MsgInfo( "Connection to database "+ Upper(cDatabase) + " is successful!" )
      ELSE
         MsgStop( "Error connecting to database!", "MySQL Error" )
         RETURN
      ENDIF
      Form_1.Button_Connect.Enabled := .F.
      Form_1.Button_Retrieve.Enabled := .T.
      Form_1.Button_Retrieve.SetFocus()
   ELSE
      MsgStop( "Connection to MySQL failed: " + MYSQL_ERROR( nConn ), "MySQL Error" )
   ENDIF

RETURN

// Function to retrieve and display data
PROCEDURE RetrieveAndDisplayData( nConn )

   LOCAL cSQL      := "SELECT * FROM City"
   LOCAL nResult   := 0
   LOCAL aRow      := {}
   LOCAL nFields   := 0
   LOCAL i         := 0
   LOCAL aStruct   := {}
   LOCAL aData     := {}
   LOCAL aHeaders  := {}
   LOCAL aWidths   := {}

   ExecuteQuery( nConn, cSQL )

   nResult := MYSQL_USE_RESULT( nConn )

   IF ! Empty( nResult )

      nFields := GetProperty( "Form_1", "Grid_Data", "ColumnCount" )

      DO WHILE nFields > 0
         Form_1.Grid_Data.DeleteColumn( nFields )
         nFields--
      ENDDO

      nFields := MYSQL_NUM_FIELDS( nResult )

      // Get Headers
      FOR i := 1 TO nFields
         AADD( aStruct, MYSQL_FETCH_FIELD( nResult ) )
         AADD( aHeaders, aStruct[ i ][ MYSQL_FS_NAME ] )
         AADD( aWidths, Max( 100, aStruct[ i ][ MYSQL_FS_LENGTH ] * 5 ) ) // Adjust width as needed
         Form_1.Grid_Data.AddColumn( i, aHeaders[ i ], aWidths[ i ], LToN( SQLTypeToHarb( aStruct[ i ] ) == "N" ) )
      NEXT

      WHILE .T.
         aRow := MYSQL_FETCH_ROW( nResult )

         IF aRow[ 1 ] == NIL
            EXIT
         ENDIF

         AADD( aData, aRow )
      ENDDO

      //MYSQL_FREE_RESULT( nResult )

      i := Form_1.Grid_Data.INDEX
      _HMG_aControlMiscData1[ i ][ 2 ] := aWidths // Set grid column widths

      Form_1.Grid_Data.SetArray( aData )          // Set grid data
      Form_1.Grid_Data.Refresh()

   ELSE
      MsgStop( "No results found." )
   ENDIF

RETURN

// Function to execute a SQL query
PROCEDURE ExecuteQuery( nConn, cSQL )

   LOCAL nSuccess := MYSQL_QUERY( nConn, cSQL )

   IF nSuccess > 0
      MsgStop( "Query failed: " + MYSQL_ERROR( nConn ), "MySQL Error" )
   ENDIF

RETURN

// Helper function
STATIC FUNCTION SQLTypeToHarb( aField )

   SWITCH aField[ MYSQL_FS_TYPE ]
   CASE MYSQL_TYPE_STRING
      RETURN "C"

   CASE MYSQL_TYPE_TINY_BLOB
   CASE MYSQL_TYPE_VARCHAR
   CASE MYSQL_TYPE_VAR_STRING
      RETURN iif( aField[ MYSQL_FS_LENGTH ] <= 255, "C", "M" )

   CASE MYSQL_TYPE_BLOB
   CASE MYSQL_TYPE_MEDIUM_BLOB
      RETURN "M"

   CASE MYSQL_TYPE_LONG_BLOB
      RETURN "W"

   CASE MYSQL_TYPE_DATE
      RETURN "D"

   CASE MYSQL_TYPE_TIME
   CASE MYSQL_TYPE_DATETIME
   CASE MYSQL_TYPE_TIMESTAMP
      RETURN "T"

   CASE MYSQL_TYPE_BIT
      RETURN "L"

   CASE MYSQL_TYPE_SHORT
   CASE MYSQL_TYPE_TINY
   CASE MYSQL_TYPE_INT24
   CASE MYSQL_TYPE_LONG
   CASE MYSQL_TYPE_LONGLONG
   CASE MYSQL_TYPE_FLOAT
   CASE MYSQL_TYPE_DOUBLE
   CASE MYSQL_TYPE_NEWDECIMAL
      RETURN "N"

   ENDSWITCH

   RETURN "U"
