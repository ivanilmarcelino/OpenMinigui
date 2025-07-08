#ifdef __XHARBOUR__
#define __SYSDATA__
#endif

#include "minigui.ch"
#include "dbinfo.ch"
#include "fileio.ch"

*=============================================================================*
*                          Auxiliary Functions
*=============================================================================*

/*
   cFieldList is a comma delimited list of fields, f.e. "First,Last,Birth,Age".
*/
*-----------------------------------------------------------------------------*
FUNCTION HMG_DbfToArray( cFieldList, bFor, bWhile, nNext, nRec, lRest )
*-----------------------------------------------------------------------------*
   LOCAL aRet := {}
   LOCAL nRecNo := RecNo()
   LOCAL bLine

   IF Empty( cFieldList )
      cFieldList := ""
      AEval( dbStruct(), {| a | cFieldList += "," + a[ 1 ] } )
      cFieldList := SubStr( cFieldList, 2 )
   ENDIF

   bLine := &( "{||{" + cFieldList + "}}" )

   dbEval( { || AAdd( aRet, Eval( bLine ) ) }, bFor, bWhile, nNext, nRec, lRest )

   dbGoto( nRecNo )

RETURN aRet

*-----------------------------------------------------------------------------*
FUNCTION HMG_ArrayToDbf( aData, cFieldList, bProgress )
*-----------------------------------------------------------------------------*
   LOCAL aFldName, aFieldPos, aFieldTyp
   LOCAL aRow
   LOCAL uVal
   LOCAL nCols, nRows
   LOCAL nCol, nRow
   LOCAL lFldName
   LOCAL lSuccess := .T.

   IF ISARRAY( cFieldList )
      aFldName := cFieldList
   ELSEIF ISCHARACTER( cFieldList )
      aFldName := hb_ATokens( cFieldList, ',' )
   ENDIF

   lFldName := ( Empty( aFldName ) )
   nCols := iif( lFldName, FCount(), Min( FCount(), Len( aFldName ) ) )
   aFieldPos := Array( nCols )
   aFieldTyp := Array( nCols )
   FOR nCol := 1 TO nCols
      aFieldPos[ nCol ] := iif( lFldName, nCol, FieldPos( aFldName[ nCol ] ) )
      aFieldTyp[ nCol ] := iif( lFldName, FieldType( nCol ), FieldType( aFieldPos[ nCol ] ) )
   NEXT

   nRows := Len( aData )
   IF ISBLOCK( bProgress )
      Eval( bProgress, 0, nRows )
   ENDIF

   FOR nRow := 1 TO nRows
      aRow := aData[ nRow ]

      IF ! NetAppend()
         lSuccess := .F.
         EXIT
      ENDIF

      FOR nCol := 1 TO nCols
         IF ! Empty( aFieldPos[ nCol ] )

            IF ! Empty( uVal := aRow[ nCol ] )

               IF ! ( aFieldTyp[ nCol ] $ '+@' )
                  IF ValType( uVal ) != aFieldTyp[ nCol ]
                     uVal := ConvertType( uVal, aFieldTyp[ nCol ] )
                  ENDIF

                  IF ! Empty( uVal )
                     FieldPut( aFieldPos[ nCol ], uVal )
                  ENDIF
               ENDIF

            ENDIF

         ENDIF
      NEXT nCol

      dbUnlock()
      IF ISBLOCK( bProgress )
         Eval( bProgress, nRow, nRows )
      ENDIF
   NEXT nRow

RETURN lSuccess

#ifdef __XHARBOUR__
#include "hbcompat.ch"
#endif
*-----------------------------------------------------------------------------*
STATIC FUNCTION ConvertType( uVal, cTypeDst )
*-----------------------------------------------------------------------------*
   LOCAL cTypeSrc := ValType( uVal )

   IF cTypeDst != cTypeSrc

      DO CASE
      CASE cTypeDst $ 'CM'
         uVal := hb_ValToStr( uVal )

      CASE cTypeDst == 'D'
         DO CASE
         CASE cTypeSrc == 'T'
            uVal := SToD( Left( hb_TToS( uVal ), 8 ) )
         CASE cTypeSrc == 'C'
            uVal := CToD( uVal )
         OTHERWISE
            uVal := BLANK_DATE
         ENDCASE

      CASE cTypeDst == 'L'
         DO CASE
         CASE cTypeSrc $ 'LN'
            uVal := ! Empty( uVal )
         CASE cTypeSrc == 'C'
            uVal := Upper( uVal ) $ "Y,YES,T,.T.,TRUE"
         OTHERWISE
            uVal := .F.
         ENDCASE

      CASE cTypeDst == 'N'
         DO CASE
         CASE cTypeSrc == 'C'
            uVal := Val( uVal )
         OTHERWISE
            uVal := 0
         ENDCASE

      CASE cTypeDst == 'T'
         DO CASE
         CASE cTypeSrc == 'D'
            uVal := hb_SToT( DToS( uVal ) + "000000.000" )
         CASE cTypeSrc == 'C'
            uVal := hb_CToT( uVal )
         OTHERWISE
            uVal := hb_CToT( '' )
         ENDCASE

      OTHERWISE
         uVal := NIL
      ENDCASE

   ENDIF

RETURN uVal

*-----------------------------------------------------------------------------*
FUNCTION HMG_DbfToExcel( cFieldList, aHeader, bFor, bWhile, nNext, nRec, lRest )
*-----------------------------------------------------------------------------*
   LOCAL nRecNo := RecNo()
   LOCAL bLine
   LOCAL oExcel, oBook, oSheet, oRange
   LOCAL nCols
   LOCAL nRow := 1

   IF Empty( cFieldList )
      cFieldList := ""
      AEval( dbStruct(), { | x | cFieldList += "," + x[ 1 ] } )
      cFieldList := SubStr( cFieldList, 2 )
   ENDIF

   hb_default( @aHeader, hb_ATokens( cFieldList, "," ) )

   TRY
      oExcel := CreateObject( "Excel.Application" )
   CATCH
      MsgAlert( "Excel not installed", "Warning" )
      RETURN .F.
   END

   oBook := oExcel:WorkBooks:Add()
   oSheet := oBook:ActiveSheet

   nCols := Len( aHeader )
   oRange := oSheet:Range( oSheet:Columns( nRow ), oSheet:Columns( nCols ) )

   oExcel:ScreenUpdating := .F.

   oRange:Rows( nRow ):Value := aHeader
   oRange:Rows( nRow ):Font:Bold := .T.

   bLine := &( "{||{" + cFieldList + "}}" )
   IF Empty( bWhile ) .AND. Empty( nNext ) .AND. Empty( nRec ) .AND. Empty( lRest )
      dbGoTop()
   ENDIF

   dbEval( { || oRange:Rows( ++nRow ):Value := Eval( bLine ), nRow }, bFor, bWhile, nNext, nRec, lRest )
   dbGoto( nRecNo )

   oRange:AutoFit()

   oExcel:ScreenUpdating := .T.
   oExcel:Visible := .T.

RETURN .T.

#define FIELD_ENTRY_SIZE 32
#define BUFFER_SIZE 32
*-----------------------------------------------------------------------------*
FUNCTION HMG_DbfStruct( cFileName )
*-----------------------------------------------------------------------------*
   LOCAL aStruct := {}
   LOCAL hFile, aFieldInfo
   LOCAL cBuffer := Space( BUFFER_SIZE )

   IF Set( _SET_DEFEXTENSIONS )
      cFileName := hb_FNameExtSetDef( cFileName, ".dbf" )
   ENDIF

   IF ( hFile := FOpen( cFileName, FO_SHARED ) ) >= 0

      IF FRead( hFile, @cBuffer, BUFFER_SIZE ) == FIELD_ENTRY_SIZE

         DO WHILE ( FRead( hFile, @cBuffer, BUFFER_SIZE ) == FIELD_ENTRY_SIZE .AND. !( cBuffer = Chr( 13 ) ) )

            aFieldInfo := Array( 4 )

            aFieldInfo[ 1 ] := Upper( BeforAtNum( Chr( 0 ), cBuffer, 1 ) )
            aFieldInfo[ 2 ] := SubStr( cBuffer, 12, 1 )

            IF aFieldInfo[ 2 ] == 'C'

               aFieldInfo[ 3 ] := Bin2I( SubStr( cBuffer, 17, 2 ) )
               aFieldInfo[ 4 ] := 0

            ELSE

               aFieldInfo[ 3 ] := Asc( SubStr( cBuffer, 17, 1 ) )
               aFieldInfo[ 4 ] := Asc( SubStr( cBuffer, 18, 1 ) )

            ENDIF

            AAdd( aStruct, aFieldInfo )

         ENDDO

      ENDIF

      FClose( hFile )

   ELSE

      aStruct := NIL

   ENDIF

RETURN aStruct

*-----------------------------------------------------------------------------*
FUNCTION HMG_RecToHash( cFieldList, cNames )
*-----------------------------------------------------------------------------*
   LOCAL hRec := { => }
   LOCAL aVals, aNames

   HSetCaseMatch( hRec, .F. )

   IF Empty( cFieldList )
      cFieldList := ""
      AEval( dbStruct(), {| a | cFieldList += "," + a[ 1 ] } )
      cFieldList := SubStr( cFieldList, 2 )
   ENDIF

   DEFAULT cNames := cFieldList

   aNames := hb_ATokens( cNames, ',' )

   aVals := &( '{' + cFieldList + '}' )

   AEval( aVals, {| u, i | hSet( hRec, aNames[ i ], u ) }, , Len( aNames ) )

RETURN hRec

*-----------------------------------------------------------------------------*
FUNCTION HMG_HashToRec( hRec, cFieldList )
*-----------------------------------------------------------------------------*
   LOCAL lShared := dbInfo( DBI_SHARED )
   LOCAL lLocked := .F.
   LOCAL lSaved := .F.
   LOCAL aFlds

   IF ! lShared .OR. ;
         ( dbInfo( DBI_ISFLOCK ) .OR. dbRecordInfo( DBRI_LOCKED, RecNo() ) ) .OR. ;
         ( lLocked := dbRLock( RecNo() ) )

      IF Empty( cFieldList )
         hb_HEval( hRec, {| k, v | FieldPut( FieldPos( k ), v ) } )
      ELSE
         aFlds := hb_ATokens( cFieldList, ',' )
         hb_HEval( hRec, {| k, v, p | HB_SYMBOL_UNUSED( k ), FieldPut( FieldPos( aFlds[ p ] ), v ) }, , Len( aFlds ) )
      ENDIF

      IF lLocked
         dbRUnlock( RecNo() )
      ENDIF

      lSaved := .T.

   ENDIF

RETURN lSaved

*-----------------------------------------------------------------------------*
PROCEDURE DbfCopyRec( cnTargetArea, lAppend )
*-----------------------------------------------------------------------------*
   LOCAL nFieldsCnt := FCount()
   LOCAL nCnt
   LOCAL cFieldName
   LOCAL nFieldPos
   LOCAL xFieldValue

   IF ISLOGICAL( lAppend ) .AND. lAppend

      ( cnTargetArea )->( dbAppend() )

   ENDIF

   FOR nCnt := 1 TO nFieldsCnt

      cFieldName := FieldName( nCnt )

      IF ( nFieldPos := ( cnTargetArea )->( FieldPos( cFieldName ) ) ) > 0 .AND. ;
         ValType( xFieldValue := FieldGet( nCnt ) ) == ValType( ( cnTargetArea )->( FieldGet( nFieldPos ) ) )

         ( cnTargetArea )->( FieldPut( nFieldPos, xFieldValue ) )

      ENDIF

   NEXT

RETURN

*-----------------------------------------------------------------------------*
FUNCTION DbfModStru( cDbfName, aModStru )
*-----------------------------------------------------------------------------*
   LOCAL nSize
   LOCAL cBuffer := Space( BUFFER_SIZE )
   LOCAL cBuffSize
   LOCAL hDbfHandle
   LOCAL nErrorCode
   LOCAL nStru

   nSize := Len( aModStru ) * BUFFER_SIZE
   cBuffSize := Space( nSize )
   hDbfHandle := FOpen( cDbfName, FO_EXCLUSIVE + FO_READWRITE )
   nErrorCode := FError()

   IF nErrorCode == 0

      IF FRead( hDbfHandle, @cBuffer, BUFFER_SIZE ) <> FIELD_ENTRY_SIZE

         nErrorCode := FError()

      ELSEIF FRead( hDbfHandle, @cBuffSize, nSize ) <> nSize

         nErrorCode := FError()

      ELSE

         FOR nStru := 1 TO Len( aModStru )

            cBuffSize := Stuff( cBuffSize, 1 + BUFFER_SIZE * ( nStru - 1 ), 10, PadR( aModStru[ nStru, 1 ], 10 ) )
            cBuffSize := Stuff( cBuffSize, 12 + BUFFER_SIZE * ( nStru - 1 ), 1, PadR( aModStru[ nStru, 2 ], 1 ) )

         NEXT

         FSeek( hDbfHandle, BUFFER_SIZE, FS_SET )

         IF FWrite( hDbfHandle, cBuffSize, nSize ) <> nSize

            nErrorCode := FError()

         ENDIF

      ENDIF

      FClose( hDbfHandle )

   ENDIF

RETURN nErrorCode
