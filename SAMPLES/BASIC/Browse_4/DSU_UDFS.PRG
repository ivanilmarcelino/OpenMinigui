/*
 * This is an original work by Peter Townsend and is
 * hereby placed in the public domain.
 */

#include "fileio.ch"
#include "common.ch"

#translate ntosz( <n>, <l> ) => Str( <n>, <l>, 0 )

#define TYPELEN 1               // No. chars for variable TYPE
#define VALUELEN 5              // No. bytes for variable LENGTH
#define NELEMLEN 6              // No. bytes for NO. OF ELEMENTS in array
#define DATELEN 8               // No. bytes for a DATE variable
#define LOGICALLEN 1            // No. bytes for a LOGIC variable
#define BLOCKMSG "CANNOT STORE BLOCK"   // Used in place of the BLOCK type
#define OBJECTMSG "CANNOT STORE OBJECT" // Used in place of the OBJECT type

FUNCTION SaveArry( aArray, cFile )

   LOCAL nHandle, lAbort
   DEFAULT cFile TO "STORED.ARR"
   lAbort := .F.
   IF ValType( aArray ) $ "CDLN"
      aArray := { aArray }
   ENDIF
   IF ValType( aArray ) != "A"
      lAbort := .T.
   ENDIF
   IF ! lAbort
      nHandle := FCreate( cFile )
      SaveLevel( aArray, nHandle )
      FClose( nHandle )
   ENDIF
   IF lAbort
      IF File( cFile )
         DELETE File ( cFile )
      ENDIF
   ENDIF

RETURN( File( cFile ) )

STATIC FUNCTION SaveLevel( pArray, nHandle )

   LOCAL cType, nElems, nElemCntr, nNumLen, cStr
   nElems := Len( pArray )
   FWrite( nHandle, ntosz( nElems, NELEMLEN ) )
   FOR nElemCntr := 1 TO nElems
      cType := ValType( pArray[ nElemCntr ] )
      FWrite( nHandle, cType )
      DO CASE
      CASE cType == "A"
         // Array
         SaveLevel( pArray[ nElemCntr ], nHandle )
      CASE cType == "B"
         // Block
         FWrite( nHandle, ntosz( Len( BLOCKMSG ), VALUELEN ) )
         FWrite( nHandle, BLOCKMSG )
      CASE cType == "C"
         // Character
         FWrite( nHandle, ntosz( Len( pArray[ nElemCntr ] ), VALUELEN ) )
         FWrite( nHandle, pArray[ nElemCntr ] )
      CASE cType == "D"
         // Date
         FWrite( nHandle, ntosz( DATELEN, VALUELEN ) )
         FWrite( nHandle, DToS( pArray[ nElemCntr ] ) )
      CASE cType == "L"
         // Logical
         FWrite( nHandle, ntosz( LOGICALLEN, VALUELEN ) )
         FWrite( nHandle, IF( pArray[ nElemCntr ], "T", "F" ) )
      CASE cType == "M"
         // Memo (should never be used)
         FWrite( nHandle, ntosz( Len( pArray[ nElemCntr ] ), VALUELEN ) )
         FWrite( nHandle, pArray[ nElemCntr ] )
      CASE cType == "N"
         // Number
         cStr := AllTrim( Transform( pArray[ nElemCntr ], "@B" ) )
         nNumLen := Len( cStr )
         FWrite( nHandle, ntosz( nNumLen, VALUELEN ) )
         FWrite( nHandle, cStr )
      CASE cType == "O"
         // Object
         FWrite( nHandle, ntosz( Len( OBJECTMSG ), VALUELEN ) )
         FWrite( nHandle, OBJECTMSG )
      ENDCASE
   NEXT

RETURN( NIL )


FUNCTION LoadArry( cFile )

   LOCAL nHandle, lOK, aRetVal
   DEFAULT cFile TO "STORED.ARR"
   lOK := .F.
   aRetVal := NIL // Unnecessary, but sets a value which may be used.
   nHandle := -1 // Set to the error value for an open.
   IF File( cFile )
      nHandle := FOpen( cFile, FO_READ )
      lOK := ( nHandle > -1 )
   ENDIF
   IF lOK
      aRetVal := LoadLevel( nHandle ) // Recursively process the array.
   ENDIF
   IF ( nHandle > -1 )
      FClose( nHandle )
   ENDIF

RETURN( aRetVal )

STATIC FUNCTION LoadLevel( nHandle )

   LOCAL aRetVal, cBuffer, lFinished, nElems, nCntr
   LOCAL cType, nLength
   aRetVal := {}
   cBuffer := Space( NELEMLEN )
   lFinished := ! ReadFile( nHandle, @cBuffer, NELEMLEN )
   IF ! lFinished
      nElems := Val( cBuffer )
      FOR nCntr := 1 TO nElems
         lFinished := ! ReadFile( nHandle, @cBuffer, TYPELEN )
         IF ! lFinished
            cType := cBuffer
            IF ( cType != "A" )
               lFinished := ! ReadFile( nHandle, @cBuffer, VALUELEN )
               IF ! lFinished
                  nLength := Val( cBuffer )
                  lFinished := ! ReadFile( nHandle, @cBuffer, nLength )
               ENDIF
            ENDIF
            DO CASE
            CASE lFinished
               // Can't read file, so do nothing.
            CASE cType == "A"
               // Array
               AAdd( aRetVal, LoadLevel( nHandle ) )
            CASE ( cType $ "BCMO" )
               // Block, Character, Memo, Object
               AAdd( aRetVal, cBuffer )
            CASE cType == "D"
               // Date
               AAdd( aRetVal, SToD( cBuffer ) )
            CASE cType == "L"
               // Logical
               AAdd( aRetVal, ( cBuffer == "T" ) )
            CASE cType == "N"
               // Number
               AAdd( aRetVal, Val( cBuffer ) )
            ENDCASE
         ENDIF
         IF lFinished // Can't read file, so
            nCntr := nElems + 1 // force exit from loop
         ENDIF
      NEXT
   ENDIF

RETURN( IF( Len( aRetVal ) > 0, aRetVal, NIL ) )

STATIC FUNCTION ReadFile( nHandle, cBuffer, pBytes )

   cBuffer := Space( pBytes )

RETURN( ( FRead( nHandle, @cBuffer, pBytes ) == pBytes ) )


#define OM_CLOSED      1
#define OM_SHARED      2
#define OM_EXCLUSIVE   3

FUNCTION OpenMode( cFileName )

   LOCAL hFile, nRetVal
   IF !( "." $ cFileName )
      cFileName += ".DBF"
   ENDIF
   nRetVal := OM_EXCLUSIVE
   //
   IF ! File( cFileName )
      nRetVal := OM_CLOSED
   ELSE
      hFile := FOpen( cFileName, FO_READ + FO_EXCLUSIVE )
      IF hFile != F_ERROR
         FClose( hFile )
         nRetVal := OM_CLOSED
      ELSE
         hFile := FOpen( cFileName, FO_READ + FO_SHARED )
         IF hFile != F_ERROR
            FClose( hFile )
            nRetVal := OM_SHARED
         ENDIF
      ENDIF
   ENDIF

RETURN( nRetVal )


#define DSU_FILE "DSETUP.ARR"
#define DSU_DBF 1    // DBF name                    within DBF info
#define DSU_RDD 2    // Replaceable Database driver within DBF info
#define DSU_MOD 3    // Open mode                   within DBF info
#define DSU_REC 4    // Current record no           within DBF info
#define DSU_LOK 5    // Records locked              within DBF info
#define DSU_FLT 6    // Filter in use               within DBF info
#define DSU_REL 7    // Relations                   within DBF info
#define DSU_BAG 8    // Index names                 within DBF info
#define DSU_ORD 9    // Controlling index           within DBF info
#define DSU_QTY 9    // No of items maintained in DBF info

FUNCTION DSU_Save( cFile )

   LOCAL nDbfCntr, aDBInfo, nCrntArea, aDBUnit
   DEFAULT cFile TO DSU_FILE
   //
   // Save environment
   //
   nCrntArea := Select()
   //
   // Load database information for each used area.
   //
   aDBInfo := {}
   nDbfCntr := 1
   dbSelectArea( nDbfCntr )
   DO WHILE Used()
      aDBUnit := Array( DSU_QTY )
      aDBUnit[ DSU_DBF ] := Alias()
      aDBUnit[ DSU_RDD ] := rddName()
      aDBUnit[ DSU_MOD ] := ( OpenMode( Alias() ) == OM_SHARED )
      aDBUnit[ DSU_REC ] := RecNo()
      aDBUnit[ DSU_LOK ] := dbRLockList()
      aDBUnit[ DSU_FLT ] := dbFilter()
      aDBUnit[ DSU_REL ] := RelsGet()
      aDBUnit[ DSU_BAG ] := OrdBagsGet()
      aDBUnit[ DSU_ORD ] := IndexOrd()
      AAdd( aDBInfo, aDBUnit )
      nDbfCntr++
      dbSelectArea( nDbfCntr )
   ENDDO
   //
   // Save array to disk.
   //
   SaveArry( aDBInfo, cFile )
   //
   // Restore environment.
   //
   dbSelectArea( nCrntArea )

RETURN( Len( aDBInfo ) > 0 )


FUNCTION DSU_Rest( cFile )

   LOCAL aDbfList, nDbfCntr, aDBUnit, lSuccess, aLokList, nLokCntr, aRelations
   DEFAULT cFile TO DSU_FILE
   lSuccess := .T.
   aDbfList := {}
   IF File( cFile )
      aDbfList := LoadArry( cFile )
      IF aDbfList == NIL
         lSuccess := .F.
      ENDIF
   ELSE
      lSuccess := .F.
   ENDIF
   IF lSuccess
      dbCloseAll()
      aRelations := Array( Len( aDbfList ) )
      //
      // Restore database files
      //
      FOR nDbfCntr := 1 TO Len( aDbfList )
         aDBUnit := aDbfList[ nDbfCntr ]
         dbSelectArea( nDbfCntr )
         dbUseArea( .F., aDBUnit[ DSU_RDD ], aDBUnit[ DSU_DBF ], ;
            , aDBUnit[ DSU_MOD ] )
         IF Used()
            //
            // Restore indexes
            //
            IF aDBUnit[ DSU_BAG ] != NIL
               IF OrdBagsSet( aDBUnit[ DSU_BAG ] )
                  dbSetOrder( aDBUnit[ DSU_ORD ] )
               ELSE
                  lSuccess := .F.
               ENDIF
            ENDIF
            //
            // Re-lock locked records.
            //
            aLokList := aDBUnit[ DSU_LOK ]
            IF aLokList != NIL
               FOR nLokCntr := 1 TO Len( aLokList )
                  IF ! dbRLock( aLokList[ nLokCntr ] )
                     lSuccess := .F.
                  ENDIF
               NEXT
            ENDIF
            //
            // Re-setablish filter condition.
            //
            IF aDBUnit[ DSU_FLT ] != NIL
               SET FILTER TO &( aDBUnit[ DSU_FLT ] )
            ENDIF
            //
            // Restore record position.
            //
            dbGoto( aDBUnit[ DSU_REC ] )
            //
            // Save relationship info to array for setting later.
            //
            aRelations[ nDbfCntr ] := { Select(), aDBUnit[ DSU_REL ] }
         ELSE
            ? "Cannot open database " + aDBUnit[ DSU_DBF ]
            lSuccess := .F.
         ENDIF
      NEXT
      //
      // If all database/index opens successful, set relationships.
      //
      IF lSuccess
         FOR nDbfCntr := 1 TO Len( aRelations )
            IF ! Empty( aRelations[ nDbfCntr ][ 2 ] )
               SELECT ( aRelations[ nDbfCntr ][ 1 ] )
               IF ! RelsSet( aRelations[ nDbfCntr ][ 2 ] )
                  lSuccess := .F.
               ENDIF
            ENDIF
         NEXT
      ENDIF
   ENDIF

RETURN( lSuccess )


FUNCTION OrdBagsGet()

   LOCAL aBagList, nBagCntr
   aBagList := {}
   nBagCntr := 1
   DO WHILE ! Empty( ordBagName( nBagCntr ) )
      AAdd( aBagList, ordBagName( nBagCntr ) )
      nBagCntr++
   ENDDO

RETURN( aBagList )


FUNCTION OrdBagsSet( aBagList )

   LOCAL nBagCntr, lSuccess
   lSuccess := .T.
   ordListClear()
   FOR nBagCntr := 1 TO Len( aBagList )
      ordListAdd( aBagList[ nBagCntr ] )
      IF ordBagName( nBagCntr ) != aBagList[ nBagCntr ]
         lSuccess := .F.
      ENDIF
   NEXT

RETURN( lSuccess )


FUNCTION RelsGet()

   LOCAL aRelList, nRelCntr
   aRelList := {}
   nRelCntr := 1
   DO WHILE ! Empty( dbRelation( nRelCntr ) )
      AAdd( aRelList, { dbRSelect( nRelCntr ), dbRelation( nRelCntr ) } )
      nRelCntr++
   ENDDO

RETURN( aRelList )


FUNCTION RelsSet( aRelList )

   LOCAL nRelCntr, lSuccess, cExpr, cAlias
   lSuccess := .T.
   dbClearRelation()
   FOR nRelCntr := 1 TO Len( aRelList )
      cAlias := Alias( aRelList[ nRelCntr ][ 1 ] )
      IF Empty( Alias() ) .OR. Empty( cAlias )
         // The pair of databases necessary aren't open.
         lSuccess := .F.
      ELSE
         cExpr := aRelList[ nRelCntr ][ 2 ]
         SET RELATION TO &( cExpr ) ;
            INTO ( cAlias ) ADDITIVE
         IF Empty( dbRelation( nRelCntr ) )
            // The relation wasn't set.
            lSuccess := .F.
         ENDIF
      ENDIF
   NEXT

RETURN( lSuccess )
