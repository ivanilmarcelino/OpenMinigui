#include <hmg.ch>

*----------------------------------------------------------------------------*
FUNCTION Query_001( cQueryString )
*----------------------------------------------------------------------------*
   LOCAL aRecordSet := {}, cIndex

   cQueryString := Upper( AllTrim ( cQueryString ) )

   cIndex := TempFile ( , 'ntx' )

   USE TEST SHARED

   INDEX ON TEST->LAST TO ( cIndex ) FOR Upper( AllTrim ( TEST->LAST ) ) = cQueryString .AND. ( .NOT. Deleted() )

   GO TOP

   DO WHILE .NOT. Eof()

      AAdd( aRecordSet, ;
         { Str( RecNo() ), TEST->LAST, TEST->FIRST, TEST->STREET, TEST->CITY, TEST->STATE, TEST->ZIP, ;
         DToC( TEST->HIREDATE ), IF( TEST->MARRIED, '.T.', '.F.' ), Str( TEST->AGE ), Str( TEST->SALARY ) } )

      SKIP

   ENDDO

   USE

   DELETE FILE ( cIndex )

RETURN aRecordSet

*----------------------------------------------------------------------------*
FUNCTION Query_002( nRecNo )
*----------------------------------------------------------------------------*
   LOCAL lResult

   USE TEST SHARED

   dbGoto( nRecNo )

   IF .NOT. Deleted()

      DO WHILE .NOT. RLock() ; ENDDO

      dbDelete()

      UNLOCK

      lResult := .T.

   ELSE

      lResult := .F.

   ENDIF

   USE

RETURN lResult

*----------------------------------------------------------------------------*
FUNCTION Query_003( aValues )
*----------------------------------------------------------------------------*
   LOCAL I

   USE TEST SHARED

   APPEND BLANK

   FOR I := 1 TO Len( aValues )

      FieldPut( I, aValues[ I ] )

   NEXT I

   USE

RETURN NIL

*----------------------------------------------------------------------------*
FUNCTION Query_004( nRecNo, aValues )
*----------------------------------------------------------------------------*
   LOCAL I

   USE TEST SHARED

   dbGoto( nRecNo )

   DO WHILE .NOT. RLock() ; ENDDO

   FOR I := 1 TO Len( aValues )

      FieldPut( I, aValues[ I ] )

   NEXT I

   UNLOCK

   USE

RETURN NIL
