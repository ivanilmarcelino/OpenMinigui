#ifndef HB_SYMBOL_UNUSED
#  define HB_SYMBOL_UNUSED( symbol ) ( ( symbol ) )
#endif


FUNCTION _GetTextHeight( hwnd, hDC )

   HB_SYMBOL_UNUSED( hwnd )

RETURN GetTextMetric( hDC )[ 1 ]


FUNCTION _InvertRect( hDC, aRec ) // Temporary

   LOCAL bRec

   bRec := { aRec[ 2 ], aRec[ 1 ], aRec[ 4 ], aRec[ 3 ] }
   InvertRect( hDC, bRec )

RETURN NIL


FUNCTION OSend( oObject, cMsg, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10 )

   LOCAL nParams := PCount() - 2
   LOCAL uResult

   IF "(" $ cMsg
      cMsg = StrTran( cMsg, "()", "" )
      DO CASE
      CASE nParams ==  0 ; uResult := oObject:&( cMsg )()
      CASE nParams ==  1 ; uResult := oObject:&( cMsg )( u1 )
      CASE nParams ==  2 ; uResult := oObject:&( cMsg )( u1, u2 )
      CASE nParams ==  3 ; uResult := oObject:&( cMsg )( u1, u2, u3 )
      CASE nParams ==  4 ; uResult := oObject:&( cMsg )( u1, u2, u3, u4 )
      CASE nParams ==  5 ; uResult := oObject:&( cMsg )( u1, u2, u3, u4, u5 )
      CASE nParams ==  6 ; uResult := oObject:&( cMsg )( u1, u2, u3, u4, u5, u6 ) 
      CASE nParams ==  7 ; uResult := oObject:&( cMsg )( u1, u2, u3, u4, u5, u6, u7 )
      CASE nParams ==  8 ; uResult := oObject:&( cMsg )( u1, u2, u3, u4, u5, u6, u7, u8 )
      CASE nParams ==  9 ; uResult := oObject:&( cMsg )( u1, u2, u3, u4, u5, u6, u7, u8, u9 )
      CASE nParams == 10 ; uResult := oObject:&( cMsg )( u1, u2, u3, u4, u5, u6, u7, u8, u9, u10 )
      ENDCASE
      RETURN uResult
   ELSE
      IF SubStr( cMsg, 1, 1 ) == "_"
         RETURN oObject:&( SubStr( cMsg, 2 ) ) := u1
      ELSE
         RETURN oObject:&( cMsg )
      ENDIF
   ENDIF

RETURN NIL


FUNCTION ASave( aArray )

   LOCAL n 
   LOCAL cType 
   LOCAL uData
   LOCAL cInfo := ""

   FOR n := 1 TO Len( aArray )
      cType := ValType( aArray[ n ] )
      DO CASE
         CASE cType == "A" ; cInfo += ASave( aArray[ n ] )
         CASE cType == "O" ; cInfo += aArray[ n ]:Save()
      OTHERWISE
         cInfo += ( cType + I2Bin( Len( uData := cValToChar( aArray[ n ] ) ) ) + uData )
      ENDCASE
   NEXT

RETURN "A" + I2Bin( 2 + Len( cInfo ) ) + I2Bin( Len( aArray ) ) + cInfo


FUNCTION ARead( cInfo )

   LOCAL nPos    := 4
   LOCAL nLen
   LOCAL n
   LOCAL aArray
   LOCAL cType
   LOCAL cBuffer

   nLen   := Bin2I( SubStr( cInfo, nPos, 2 ) )
   nPos   += 2
   aArray := Array( nLen )

   FOR n := 1 TO Len( aArray )
      cType   := SubStr( cInfo, nPos++, 1 )
      nLen    := Bin2I( SubStr( cInfo, nPos, 2 ) )
      nPos    += 2
      cBuffer := SubStr( cInfo, nPos, nLen )
      nPos    += nLen

      DO CASE
         CASE cType == "A" ; aArray[ n ] := ARead( "A" + I2Bin( nLen ) + cBuffer )
         CASE cType == "O" ; aArray[ n ] := ORead( cBuffer )
         CASE cType == "C" ; aArray[ n ] := cBuffer
         CASE cType == "D" ; aArray[ n ] := CToD( cBuffer )
         CASE cType == "L" ; aArray[ n ] := ( cBuffer == ".T." ) 
         CASE cType == "N" ; aArray[ n ] := Val( cBuffer )
      ENDCASE
   NEXT

RETURN aArray


FUNCTION ORead( cInfo )

   LOCAL nLen
   LOCAL cClassName
   LOCAL oObj
   LOCAL nPos := 1

   nLen       := Bin2I( SubStr( cInfo, nPos, 2 ) )
   nPos       += 2
   cClassName := SubStr( cInfo, nPos, nLen )
   nPos       += nLen
   oObj       := &( cClassName + "()" )

   oObj:New()
   oObj:Load( SubStr( cInfo, nPos ) )

RETURN oObj