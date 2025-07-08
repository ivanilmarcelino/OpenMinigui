/*
WARNING : USE AT YOUR OWN RISK
          TESTED FOR CLIPPER/dBASE/FOXPRO FILES CREATED THROUGH RDDs
*
Function dbFieldRename
Purpose    : Renames Field(s) of Clipper .DBF
Parameters : cDBF    : File Name including absolute path and extension
             cOldFieldName
             cNewFieldName
Returns    : Success
*
AUTHOR : NITIN C. CHAUHAN
DATED  : 16 DEC 1998
*/

#include 'fileio.ch'

#define FIELD_BLOCK_SIZE 32
#define FIELD_NAME_SIZE  11

FUNCTION dbFieldRename( cDBF, cOldFieldName, cNewFieldName )

   LOCAL lSuccess := .F.
   LOCAL nPos     := 0                 // Field Position
   LOCAL cBuffer                       // Buffer
   LOCAL nFBSize  := FIELD_BLOCK_SIZE  // Field Block Size
   LOCAL aFlds    := {}                // Holds List of Filelds
   LOCAL nHandle  := FOpen( cDBF, FO_READWRITE )

   IF nHandle # F_ERROR

      // Skip Header information 32 bytes
      FSeek( nHandle, FIELD_BLOCK_SIZE, FS_SET )

      DO WHILE nFBSize == FIELD_BLOCK_SIZE

         cBuffer := Space( nFBSize )
         nFBSize := FRead( nHandle, @cBuffer, nFBSize )

         // Not checking for read bytes
         IF SubStr( cBuffer, 1, 1 ) == Chr( 13 )

            // End of fields information, Data offset starts
            EXIT

         ENDIF

         // Add Field Name into Lookup array
         AAdd( aFlds, ;
            PadR( SubStr( SubStr( cBuffer, 1, FIELD_NAME_SIZE ), 1, ;
            At( Chr( 0 ), SubStr( cBuffer, 1, FIELD_NAME_SIZE ) ) - 1 ), 10 ) )
      ENDDO

      nPos := AScan( aFlds, PadR( Upper( cOldFieldName ), 10 ) )

      IF nPos > 0

         // Move to Position of the Old Field Name
         FSeek( nHandle, nPos * FIELD_BLOCK_SIZE, FS_SET )

         // Write to Header
         FWrite( nHandle, PadR( Upper( cNewFieldName ), 10 ) + Chr( 0 ), FIELD_NAME_SIZE )
         lSuccess := .T.

      ENDIF

      FClose( nHandle )

   ENDIF

RETURN lSuccess
