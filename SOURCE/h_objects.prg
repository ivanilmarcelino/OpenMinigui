/*
 * MINIGUI - Harbour Win32 GUI library source code
 *
 * Copyright 2017-2025 Aleksandr Belov, Sergej Kiselev <bilance@bilance.lv>
*/

#include "minigui.ch"

#include "i_winuser.ch"
#ifdef __XHARBOUR__
# include "hbcompat.ch"
#else
# xtranslate hb_HSetCaseMatch( <x>[, <z>] ) => ( hb_HCaseMatch( <x>[, <z>] ), <x> )
#endif
#include "hbclass.ch"

#define _METHOD METHOD

STATIC o_AppDlu2Pixel := NIL

///////////////////////////////////////////////////////////////////////////////
CLASS THmgData
///////////////////////////////////////////////////////////////////////////////

   PROTECTED:
   VAR l_l_Upp AS LOGICAL
   VAR a_a_Key INIT hb_Hash()

   EXPORTED:
   VAR Cargo

   METHOD New( lUpper ) INLINE ( ::l_l_Upp := ! Empty( lUpper ), Self ) CONSTRUCTOR

  _METHOD Set( xKey, xVal, lJson )
  _METHOD Get( xKey, xDef, lJson ) 
  _METHOD Json( cJson, lMode )
   METHOD Del( Key )  INLINE ( iif( ::Pos( Key ) > 0, hb_HDel( ::a_a_Key, ::Upp( Key ) ), Nil ), Self )
   METHOD Pos( Key )  INLINE hb_HPos( ::a_a_Key, ::Upp( Key ) )
   METHOD Upp( Key )  INLINE iif( HB_ISCHAR( Key ) .AND. ::l_l_Upp, Upper( Key ), Key )
   METHOD Len()       INLINE Len( ::a_a_Key )

   METHOD Keys()      INLINE hb_HKeys( ::a_a_Key )
   METHOD Values()    INLINE hb_HValues( ::a_a_Key )
   METHOD CloneHash() INLINE hb_HClone( ::a_a_Key )
   METHOD Clone()     INLINE __objClone( Self )
  _METHOD Do( xKey, xVal )

  _METHOD GetAll( lAll )
  _METHOD Eval( Block )
#if 0
  _METHOD Destroy()
#endif

   ERROR HANDLER ControlAssign

ENDCLASS
///////////////////////////////////////////////////////////////////////////////

METHOD Set( xKey, xVal, lJson ) CLASS THmgData

   LOCAL k, v

   IF PCount() == 0 ; RETURN Self
   ENDIF

   IF !Empty( lJson ) 
      IF HB_ISCHAR( xKey ) 
         k := SubStr( xKey, At( "{", xKey ) )
         k := Left( k, RAt( "}", k ) )
         xKey := hb_jsonDecode( k )
         DEFAULT xVal := .T.
      ELSEIF HB_ISCHAR( xVal )
         k := SubStr( xVal, At( "{", xVal ) )
         k := Left( k, RAt( "}", k ) )
         xVal := oHmgData()
         xVal:Set( hb_jsonDecode( k ), .T.)
      ENDIF
      k := NIL
   ENDIF

   IF HB_ISHASH( xKey )
      IF HB_ISLOGICAL( xVal ) .AND. xVal
         FOR EACH k, v IN hb_HKeys( xKey ), hb_HValues( xKey )
            hb_HSet( ::a_a_Key, ::Upp( k ), v )
         NEXT
      ELSE
         ::a_a_Key := xKey
      ENDIF
   ELSEIF HB_ISARRAY( xKey ) .AND. HB_ISARRAY( xVal )
      FOR EACH k, v IN xKey, xVal
         DEFAULT k := hb_enumIndex( k )
         hb_HSet( ::a_a_Key, ::Upp( k ), v )
      NEXT
   ELSEIF HB_ISARRAY( xKey )
      FOR EACH v IN xKey
         IF HB_ISARRAY( v ) .AND. Len( v ) > 1
            hb_HSet( ::a_a_Key, ::Upp( v[1] ), v[2] )
         ENDIF
      NEXT
   ELSE
      hb_HSet( ::a_a_Key, ::Upp( xKey ), xVal )
   ENDIF

RETURN Self

METHOD Get( xKey, xDef, lJson ) CLASS THmgData
   LOCAL x

   IF xKey == NIL
      RETURN ::a_a_Key
   ELSEIF HB_ISLOGICAL( xKey ) .OR. xKey == NIL
      DEFAULT xDef := xKey
      RETURN ::Json( ::a_a_Key, xDef )
   ENDIF

   IF !Empty( lJson ) .AND. ::Pos( xKey ) > 0
      x := hb_HGetDef( ::a_a_Key, ::Upp( xKey ), NIL )
      IF HB_ISOBJECT( x ) .AND. ","+x:ClassName+"," $ ",THMGDATA,TKEYDATA,"
         RETURN ::Json( x, xDef )
      ENDIF
   ENDIF

RETURN hb_HGetDef( ::a_a_Key, ::Upp( xKey ), xDef )

METHOD Json( cJson, lMode ) CLASS THmgData

   IF cJson == NIL .OR. HB_ISLOGICAL( cJson ) 
      lMode := !Empty( cJson )
      RETURN hb_jsonEncode( ::a_a_Key, lMode )

   ELSEIF HB_ISOBJECT( cJson )
      lMode := !Empty( lMode )
      RETURN hb_jsonEncode( cJson:CloneHash(), lMode )

   ELSEIF HB_ISHASH( cJson )
      lMode := !Empty( lMode )
      RETURN hb_jsonEncode( cJson, lMode )

   ELSEIF HB_ISCHAR( cJson )
      cJson := SubStr( cJson, At( "{", cJson ) )
      cJson := Left ( cJson, RAt( "}", cJson ) )
      ::Set( hb_jsonDecode( cJson ), !Empty( lMode ) )

   ENDIF

RETURN Self

METHOD Do( xKey, xVal ) CLASS THmgData
   LOCAL x, b := ::Get( xKey )

   IF HB_ISARRAY( xVal ) 
   ELSEIF xVal == NIL  ; xVal := {}
   ELSE                ; xVal := { xVal }
   ENDIF
   
   AAdd( xVal, Self )

   IF HB_ISBLOCK( b )
      x := EVal( b, xVal )
   ELSEIF HB_ISCHAR( b )  // Can't be a static function
      IF !Empty( b ) .AND. hb_IsFunction( b )
         x := hb_ExecFromArray( b, xVal )
      ELSE
         x := "#@" + b
      ENDIF
   ELSE
      x := "#@" + cValToChar( b )
   ENDIF

RETURN x

METHOD GetAll( lAll ) CLASS THmgData

   LOCAL aRet := {}

   IF HB_ISLOGICAL( lAll ) .AND. lAll
      ::Eval( {| val | AAdd( aRet, val ) } )
   ELSE
      ::Eval( {| val, key | AAdd( aRet, { key, val } ) } )
   ENDIF

RETURN aRet

METHOD Eval( Block ) CLASS THmgData

   LOCAL i, b := HB_ISBLOCK( Block )
   LOCAL l := HB_ISLOGICAL( Block ) .AND. Block
   LOCAL a := iif( b, NIL, Array( 0 ) )

   FOR i := 1 TO ::Len()
      IF b ; Eval( Block, hb_HValueAt( ::a_a_Key, i ), hb_HKeyAt( ::a_a_Key, i ), i )
      ELSEIF l ; AAdd( a, hb_HValueAt( ::a_a_Key, i ) )
      ELSE ; AAdd( a, { hb_HValueAt( ::a_a_Key, i ), hb_HKeyAt( ::a_a_Key, i ), i } )
      ENDIF
   NEXT

RETURN a

#if 0
METHOD Destroy() CLASS THmgData

   LOCAL i, k, o

   IF HB_ISHASH( ::a_a_Key )
      FOR i := 1 TO Len( ::a_a_Key )
         k := hb_HKeyAt( ::a_a_Key, i )
         hb_HSet( ::a_a_Key, k, Nil )
         hb_HDel( ::a_a_Key, k )
      NEXT
   ENDIF

   IF HB_ISOBJECT( ::Cargo ) .AND. ::Cargo:ClassName == ::ClassName
      o := ::Cargo
      IF HB_ISHASH( o:a_a_Key )
         FOR i := 1 TO Len( o:a_a_Key )
            k := hb_HKeyAt( o:a_a_Key, i )
            hb_HSet( o:a_a_Key, k, Nil )
            hb_HDel( o:a_a_Key, k )
         NEXT
      ENDIF
   ENDIF

RETURN NIL

#endif

METHOD ControlAssign( xValue ) CLASS THmgData

   LOCAL cMessage, uRet, lError

   cMessage := __GetMessage()
   lError := .T.

   IF PCount() == 0
      uRet := ::Get( cMessage )
      lError := .F.
   ELSEIF PCount() == 1
      ::Set( SubStr( cMessage, 2 ), xValue )
      uRet := ::Get( cMessage )
      lError := .F.
   ENDIF

   IF lError
      uRet := NIL
      ::MsgNotFound( cMessage )
   ENDIF

RETURN uRet

#ifdef __XHARBOUR__
*-----------------------------------------------------------------------------*
STATIC FUNCTION hb_HGetDef( hHash, xKey, xDef )
*-----------------------------------------------------------------------------*
   LOCAL nPos := HGetPos( hHash, xKey )

RETURN iif( nPos > 0, HGetValueAt( hHash, nPos ), xDef )

#endif

#ifdef _OBJECT_

#ifndef __XHARBOUR__
///////////////////////////////////////////////////////////////////////////////
CLASS TIniData INHERIT THmgData
///////////////////////////////////////////////////////////////////////////////

   VAR oIni
   VAR hKeys                     INIT { => }
   VAR hLens                     INIT { => }
   VAR hSect                     INIT { => }
   VAR cBOM          AS STRING   INIT hb_utf8Chr( 0xFEFF )
   VAR cData         AS STRING   INIT ""
   VAR lData         AS LOGICAL  INIT .F.
   VAR cIni          AS STRING   INIT ""
   VAR lIni          AS LOGICAL  INIT .F.
   VAR lUtf          AS LOGICAL  INIT .F.
   VAR lUtf8         AS LOGICAL  INIT .F.
   VAR cCommentChar  AS STRING   INIT ";"
   VAR cCommentBegin AS STRING   INIT ""
   VAR cCommentEnd   AS STRING   INIT ""
   VAR lAutoMain     AS LOGICAL  INIT .F.
   VAR lMacro        AS LOGICAL  INIT .F.
   VAR lYesNo        AS LOGICAL  INIT .F.
   VAR aYesNo        AS ARRAY    INIT { "Yes", "No" }
   VAR nMinKey       AS NUMERIC  INIT 10      // Min. length for keys 
   VAR nMaxVal       AS NUMERIC  INIT 20      // Max. length Value for note
   VAR cSrcReplChar  AS STRING   INIT ""      // Original replacement character
   VAR cOutReplChar  AS STRING   INIT ""      // Replacement by output symbol

   METHOD New( cIni, lMacro, lUtf8, cChar, cData, cSrcChar, cOutChar ) INLINE ( ::Super:New( .T. ), ;
          ::Def( cIni, lMacro, lUtf8, cChar, cData, cSrcChar, cOutChar ), Self ) CONSTRUCTOR

  _METHOD Def( cIni, lMacro, lUtf8, cChar, cData, cSrcChar, cOutChar )
  _METHOD Read( cIniNew )
  _METHOD Write( cFile, lUtf8 )
  _METHOD ToValue( cStr )
  _METHOD ToString( xVal )

END CLASS

METHOD Def( cIni, lMacro, lUtf8, cChar, cData, cSrcChar, cOutChar ) CLASS TIniData

   ::cData  := hb_defaultValue( cData, ::cData )
   ::lData  := ! Empty( ::cData ) .AND. HB_ISCHAR( ::cData )
   ::cIni   := hb_defaultValue( cIni, ::cIni )
   ::lMacro := hb_defaultValue( lMacro, ::lMacro )
   ::lUtf8  := ! Empty( lUtf8 )
   ::lUtf   := ( Set( _SET_CODEPAGE ) == "UTF8" )
   ::cCommentChar := hb_defaultValue( cChar, ::cCommentChar )

   IF HB_ISCHAR( cSrcChar ) .AND. HB_ISCHAR( cOutChar )
      ::cSrcReplChar := cSrcChar
      ::cOutReplChar := cOutChar
   ENDIF

   IF ! Empty( ::cIni ) .AND. ! ::lData
      IF ! hb_FileExists( ::cIni )
         hb_memoWrit( ::cIni, iif( ::lUtf8, ::cBOM, "" ) + CRLF )
      ENDIF
      ::lIni := hb_FileExists( ::cIni )
   ENDIF

RETURN Self

METHOD Read( cIniNew ) CLASS TIniData

   LOCAL hIni, cStr, cBuf, aBuf, nBuf, cSec, hSec, oSec, nLen := 1024
   LOCAL cChr := ::cCommentChar, xVal, hKey, nKey, cNote, hHash
   LOCAL cIni := ::cIni, lIni := ::lIni
   LOCAL lIniNew := .F.

   IF HB_ISCHAR( cIniNew )
      IF ( lIniNew := hb_FileExists( cIniNew ) )
         cIni := cIniNew
         lIni := lIniNew
      ELSE
         RETURN Self
      ENDIF
   ENDIF

   IF ::lData .AND. ! lIniNew

      ::lUtf8 := Left( ::cData, Len( ::cBOM ) ) == ::cBOM
      cBuf := Left( ::cData, nLen )
      aBuf := hb_ATokens( cBuf, CRLF )
      FOR EACH cBuf IN aBuf
         IF Left( cBuf, 1 ) == "#"
            IF ! ::lUtf .AND. ::lUtf8
               ::cCommentBegin := hb_UTF8ToStr( cBuf )
            ELSE
               ::cCommentBegin := cBuf
            ENDIF
            EXIT
         ENDIF
      NEXT
      cBuf := Right( ::cData, nLen )
      aBuf := hb_ATokens( cBuf, CRLF )
      FOR nBuf := Len( aBuf ) TO 1 STEP -1
         cBuf := aBuf[ nBuf ]
         IF Left( cBuf, 1 ) == "#"
            IF ! ::lUtf .AND. ::lUtf8
               ::cCommentEnd := hb_UTF8ToStr( cBuf )
            ELSE
               ::cCommentEnd := cBuf
            ENDIF
            EXIT
         ENDIF
      NEXT

      hHash := hb_HSetCaseMatch( hb_iniReadStr( ::cData, , , ::lAutoMain ), .T. )

   ELSEIF lIni .AND. ( hIni := FOpen( cIni, 2 ) ) > 0

      cStr := Space( Len( ::cBOM ) )
      cBuf := Space( nLen )
      FRead( hIni, @cStr, Len( ::cBOM ) )
      FSeek( hIni, 0, 0 )
      FRead( hIni, @cBuf, nLen )
      ::lUtf8 := ( cStr == ::cBOM )
      aBuf := hb_ATokens( cBuf, CRLF )
      FOR EACH cBuf IN aBuf
         IF Left( cBuf, 1 ) == "#"
            IF ! ::lUtf .AND. ::lUtf8
               ::cCommentBegin := hb_UTF8ToStr( cBuf )
            ELSE
               ::cCommentBegin := cBuf
            ENDIF
            EXIT
         ENDIF
      NEXT
      cBuf := Space( nLen )
      FSeek( hIni, -nLen, 2 )
      FRead( hIni, @cBuf, nLen )
      aBuf := hb_ATokens( cBuf, CRLF )
      FOR nBuf := Len( aBuf ) TO 1 STEP -1
         cBuf := aBuf[ nBuf ]
         IF Left( cBuf, 1 ) == "#"
            IF ! ::lUtf .AND. ::lUtf8
               ::cCommentEnd := hb_UTF8ToStr( cBuf )
            ELSE
               ::cCommentEnd := cBuf
            ENDIF
            EXIT
         ENDIF
      NEXT
      FClose( hIni )

      hHash := hb_HSetCaseMatch( hb_iniRead( cIni, , , ::lAutoMain ), .T. )

   ENDIF

   FOR EACH cSec, hSec IN hb_HKeys( hHash ), hb_HValues( hHash )
      IF ! ::lUtf .AND. ::lUtf8
         cSec := hb_UTF8ToStr( cSec )
      ENDIF
      hb_HSet( ::hSect, Upper( cSec ), cSec )
      oSec := oHmgData()
      nKey := 0
      hKey := { => }
      FOR EACH cStr, cBuf IN hb_HKeys( hSec ), hb_HValues( hSec )
         cNote := ""
         IF ! ::lUtf .AND. ::lUtf8
            cStr := hb_UTF8ToStr( cStr )
            cBuf := hb_UTF8ToStr( cBuf )
         ENDIF
         IF Left( cBuf, 2 ) == "{|" .OR. Left( cBuf, 3 ) == "{ |"
            cNote := cBuf
         ENDIF
         IF ! Empty( cChr ) .AND. ( nBuf := At( cChr, cBuf ) ) > 0
            IF !( Left( cBuf, 2 ) == "{|" .OR. Left( cBuf, 3 ) == "{ |" )
               cNote := subs( cBuf, nBuf )
            ENDIF
            cBuf := AllTrim( Left( cBuf, nBuf - 1 ) )
         ENDIF

         nKey := Max( nKey, Len( cStr ) )
         hb_HSet( hKey, Upper( cStr ), { cStr, cNote } )
         IF ::lMacro .AND. ! HB_ISNIL( xVal := ::ToValue( cBuf ) )
            oSec:Set( cStr, xVal )
         ELSE
            oSec:Set( cStr, cBuf )
         ENDIF
      NEXT

      cSec := Upper( cSec )
      ::Set( cSec, oSec )
      hb_HSet( ::hKeys, cSec, hKey )
      hb_HSet( ::hLens, cSec, nKey )
   NEXT

   hHash := NIL

RETURN Self

METHOD ToValue( cStr ) CLASS TIniData

   LOCAL xVal

   IF Empty( cStr ) ; RETURN cStr
   ENDIF

   IF !Empty( ::cSrcReplChar )
      cStr := StrTran( cStr, ::cSrcReplChar, ::cOutReplChar )
   ENDIF

   IF Left(cStr, 1) == "{"  .AND. Right(cStr, 1) == "}" .OR. ;
      Left(cStr, 1) == "'"  .AND. Right(cStr, 1) == "'" .OR. ;
      Left(cStr, 1) == '"'  .AND. Right(cStr, 1) == '"' .OR. ;
      Left(cStr, 2) == 'e"' .AND. Right(cStr, 1) == '"' .OR. ;
      Left(cStr, 2) == 't"' .AND. Right(cStr, 1) == '"' .OR. ;
      Left(cStr, 4) == '0d20' .AND. Len(cStr) == 10
      BEGIN SEQUENCE WITH { |e|break(e) }
         xVal := &(cStr)
      END SEQUENCE
      IF Left(cStr, 2) == 't"' .AND. Right(cStr, 1) == '"' .AND. ;
         Valtype(xVal) == "T" .AND. Len( SubStr(cStr, 3) ) == 11
         xVal := hb_TtoD( xVal )
      ENDIF
   ELSEIF Left(cStr, 2) $ "j{J{" .AND. Right(cStr, 1) == "}" // json
      IF "<" $ cStr .AND. ">" $ cStr
         cStr := StrTran(cStr, "<", "[")
         cStr := StrTran(cStr, ">", "]")
      ENDIF
      xVal := oHmgData()
      xVal:Set(cStr, .T., .T.)
   ELSEIF hb_ntos(Val(cStr)) == cStr
      xVal := Val(cStr)
   ELSEIF cStr == "T" .OR. cStr == ".T." .OR. cStr == ".t." .OR. ;
          cStr == "Y" .OR. cStr == ::aYesNo[1]  // "Yes"
      xVal := .T.
   ELSEIF cStr == "F" .OR. cStr == ".F." .OR. cStr == ".f." .OR. ;
          cStr == "N" .OR. cStr == ::aYesNo[2]  // "No"
      xVal := .F.
   ELSE
      xVal := cStr
   ENDIF

RETURN xVal

METHOD ToString( xVal ) CLASS TIniData

   LOCAL cStr, lE := .F.

   IF HB_ISCHAR( xVal )
      cStr := Alltrim( xVal )
      IF ! Empty(cStr)
         IF CRLF $ cStr
            lE := .T.
            cStr := StrTran( cStr, CRLF, "\r\n" )
         ENDIF
         IF chr(9) $ cStr
            lE := .T.
            cStr := StrTran( cStr, chr(9), "\t" )
         ENDIF
         IF lE
            IF Left ( cStr, 1 ) == '"' ; cStr := SubStr( cStr, 2 )
            ENDIF
            IF Right( cStr, 1 ) == '"' ; cStr := Left( cStr, Len(cStr) - 1 )
            ENDIF
            cStr := 'e"' + cStr + '"'
         ENDIF
      ENDIF
   ELSEIF HB_ISLOGICAL( xVal ) .AND. ::lYesNo
      cStr := ::aYesNo[ iif( xVal, 1, 2 ) ]
   ELSEIF HB_ISOBJECT( xVal )
      IF "," + xVal:ClassName + "," $ ",THMGDATA,TKEYDATA,"
         cStr := "j" + xVal:Get(.F.)
         IF "[" $ cStr .AND. "]" $ cStr
            cStr := StrTran(cStr, "[", "<")
            cStr := StrTran(cStr, "]", ">")
         ENDIF
      ELSE
         cStr := "O:" + xVal:ClassName 
      ENDIF
   ELSE
      cStr := hb_valtoexp( xVal )
   ENDIF

RETURN cStr

METHOD Write( cFile, lUtf8 ) CLASS TIniData

   LOCAL lRet, aSec, cSec, oSec, hSec, hKey, nLen
   LOCAL hIni := { => }, cKey, cVal, xVal, cStr, lBlk
   LOCAL cIni := "_" + DtoS( Date() ) + "_" + StrTran( hb_ntos( Seconds() ), ".", "" ) + "_" + ".ini"
   LOCAL cBegin := "", cEnd := ""
   LOCAL cSek

   DEFAULT cFile := ::cIni, lUtf8 := ::lUtf8

   FOR EACH cSec IN ::Keys()
       cSek := hb_HGetDef( ::hSect, Upper( cSec ), cSec )
       oSec := ::Get( cSec )
       hSec := { => }
       hKey := hb_hSetCaseMatch( hb_HGetDef( ::hKeys, cSec, { => } ), .T. )
       nLen := hb_HGetDef( hb_hSetCaseMatch( ::hLens, .T. ), cSec,   11 ) 
       nLen := Max( nLen, ::nMinKey ) + 1
       FOR EACH aSec IN oSec:GetAll()
           cKey := aSec[1]
           xVal := aSec[2]
           lBlk := HB_ISBLOCK( xVal )
           cVal := ::ToString( xVal )
           IF ! ::lUtf .AND. lUtf8
              cKey := hb_StrToUtf8( cKey )
              cVal := hb_StrToUtf8( cVal )
           ENDIF
           cStr := hb_HGetDef( hKey, cKey, Nil )
           IF HB_ISARRAY( cStr ) .AND. Len( cStr ) > 1
              cKey := cStr[1]
              IF ! Empty( cStr[2] )
                 IF lBlk
                    cVal := iif( ! ::lUtf .AND. lUtf8, hb_StrToUtf8( cStr[2] ), cStr[2] )
                 ELSE
                    IF Len( cVal ) <= ::nMaxVal
                       cVal := Left( cVal + Space( ::nMaxVal ), ::nMaxVal )
                    ENDIF
                    cVal += space(3) + iif( ! ::lUtf .AND. lUtf8, hb_StrToUtf8( cStr[2] ), cStr[2] )
                 ENDIF
              ENDIF
           ENDIF
           IF nLen > Len( cKey ) ; cKey := Left( cKey + space( nLen ), nLen )
           ENDIF
           hb_HSet( hSec, cKey, " " + cVal )
       NEXT
       hb_HSet( hIni, cSek, hSec )
   NEXT

   IF ! Empty( ::cCommentBegin )
      cBegin += iif( Left( ::cCommentBegin, 1 ) == "#", "", "#" ) + ::cCommentBegin
      IF ! ::lUtf .AND. lUtf8
         cBegin := hb_StrToUtf8( cBegin )
      ENDIF
   ENDIF

   cBegin := iif( lUtf8, ::cBOM + CRLF, "" ) + cBegin

   IF ! Empty( ::cCommentEnd )
      cEnd += CRLF + iif( Left( ::cCommentEnd, 1 ) == "#", "", "#" ) + ::cCommentEnd
      IF ! ::lUtf .AND. lUtf8
         cEnd := hb_StrToUtf8( cEnd )
      ENDIF
   ENDIF

   IF Empty( cBegin ) ; cBegin := Nil
   ENDIF

   IF Empty( cEnd ) ; cEnd := Nil
   ENDIF

   hb_iniWrite( cIni, hIni, cBegin, cEnd, ::lAutoMain )

   IF hb_vfExists( cFile )
      lRet := Empty( hb_vfErase( cFile ) )
      IF lRet
         lRet := Empty( hb_vfRename( cIni, cFile ) )
      ENDIF
   ELSE
      lRet := Empty( hb_vfRename( cIni, cFile ) )
   ENDIF

RETURN lRet

#endif

*-----------------------------------------------------------------------------*
FUNCTION oDlu4Font( nFontSize, lDlu2Pix, nPrcW, nPrcH )
*-----------------------------------------------------------------------------*
   LOCAL nProcW, nProcH, aDim
   LOCAL aScale := { {  8,  85,  75}, ;
                     {  9,  90,  85}, ;
                     { 10,  95,  85}, ;
                     { 11, 100,  90}, ;
                     { 12, 110,  95}, ;
                     { 13, 115, 100}, ;
                     { 14, 120, 110}, ;
                     { 15, 130, 110}, ;
                     { 16, 140, 120}, ;
                     { 17, 145, 120}, ;
                     { 18, 150, 130}, ;
                     { 19, 160, 130}, ;
                     { 20, 170, 145}, ;
                     { 21, 175, 145}, ;
                     { 22, 180, 150}, ;
                     { 23, 190, 155}, ;
                     { 24, 200, 160}, ;
                     { 25, 205, 170}, ;
                     { 26, 210, 180}  ;
                   }

   IF HB_ISCHAR( nFontSize )
      nFontSize := _GetFontSize( nFontSize, "Main" )
   ENDIF

   DEFAULT lDlu2Pix := .T., nFontSize := 11

   IF nFontSize < aScale[ 1 ][ 1 ]         ; nFontSize := aScale[ 1 ][ 1 ]
   ELSEIF nFontSize > ATail( aScale )[ 1 ] ; nFontSize := ATail( aScale )[ 1 ]
   ENDIF

   FOR EACH aDim IN aScale
      IF nFontSize == aDim[ 1 ]
         nProcW := aDim[ 2 ]
         nProcH := aDim[ 3 ]
         EXIT
      ENDIF
   NEXT

   DEFAULT nProcW := 100   , nProcH := 100
   DEFAULT nPrcW  := nProcW, nPrcH  := nProcH

   IF lDlu2Pix
      RETURN TDlu2Pix():New( nPrcW, nPrcH, nFontSize )
   ENDIF

RETURN { nPrcW, nPrcH }

*-----------------------------------------------------------------------------*
FUNCTION oDlu2Pixel( nPrcW, nPrcH, nFontSize )
*-----------------------------------------------------------------------------*

   LOCAL aPrcWH

   IF HB_ISNUMERIC( nFontSize )
      aPrcWH := oDlu4Font( nFontSize, .F. )
      nPrcW  := aPrcWH[ 1 ]
      nPrcH  := aPrcWH[ 2 ]
   ENDIF

   IF PCount() > 0

      IF HB_ISARRAY( nPrcW )
         ASize( nPrcW, 2 )
         nPrcH := nPrcW[ 2 ]
         nPrcW := nPrcW[ 1 ]
      ELSEIF HB_ISCHAR ( nPrcW )
         nPrcW := hb_ATokens( nPrcW, ',' )
         ASize( nPrcW, 2 )
         nPrcH := Val( nPrcW[ 2 ] )
         nPrcW := Val( nPrcW[ 1 ] )
      ENDIF

      nPrcH := iif( Empty( nPrcH ) .OR. nPrcH < 0, NIL, nPrcH )
      nPrcW := iif( Empty( nPrcW ) .OR. nPrcW < 0, NIL, nPrcW )

      IF o_AppDlu2Pixel != NIL
         hb_default( @nPrcW, o_AppDlu2Pixel:nScaleWidth )
         hb_default( @nPrcH, o_AppDlu2Pixel:nScaleHeight )
      ENDIF

   ENDIF

   hb_default( @nPrcW, 100 )
   hb_default( @nPrcH, 100 )

   IF o_AppDlu2Pixel == NIL

      o_AppDlu2Pixel := TDlu2Pix():New( nPrcW, nPrcH, hb_defaultValue( nFontSize, _HMG_DefaultFontSize ) )

      If ! o_AppDlu2Pixel:IsError
         o_AppDlu2Pixel:Create()
      ENDIF

   ELSEIF PCount() > 0

      o_AppDlu2Pixel:UnitsToPixels( nPrcW, nPrcH )

   ENDIF

RETURN o_AppDlu2Pixel

*-----------------------------------------------------------------------------*
FUNCTION _App_Dlu2Pix_Events_( hWnd, nMsg, wParam, lParam )
*-----------------------------------------------------------------------------*

   LOCAL h, nRet := 0
   STATIC o_app

   IF HB_ISOBJECT( hWnd )
      MESSAGEONLY _App_Dlu2Pix_App_ EVENTS _App_Dlu2Pix_Events_ TO h
      o_app := hWnd
      o_app:Handle := h
      RETURN nRet
   ELSEIF ! HB_ISOBJECT( o_app )
      RETURN nRet
   ENDIF

   IF nMsg == o_app:Wm_nApp
      o_app:Event( wParam, lParam, o_app:oParam:Get( wParam ) )
      nRet := 1
   ENDIF

RETURN nRet

///////////////////////////////////////////////////////////////////////////////
CLASS TDlu2Pix
///////////////////////////////////////////////////////////////////////////////

   VAR Cargo
   VAR oCargo  INIT oKeyData()
   VAR oProp   INIT oKeyData()
   VAR oEvent  INIT oKeyData()
   VAR oParam  INIT oKeyData()
   VAR oEvents INIT oHmgData()

   VAR hWnd INIT 0
   VAR cError  INIT "Application events are not created!"
   VAR lError  INIT .F.
   VAR lAction INIT .T.

   VAR nUnitWidth    INIT 50 // width  controls GetBox, Button, ...
   VAR nUnitHeight      INIT 14 // height controls GetBox, Button, ...
   VAR nUnitHeight2  INIT 24 // 2height controls GetBox, Button, ...
   VAR nUnitGapsWidth  INIT 4  // width  space between controls
   VAR nUnitGapsHeight INIT 4  // height space between controls
   VAR nUnitMargWidth  INIT 7  // Left, Right margin
   VAR nUnitMargHeight INIT 7  // Top, Bottom margin
   VAR nUnitWidthDT  INIT 50 // for data
   VAR nUnitWidthDT1 INIT 60 // for data 50 * 1.2. MontCalendar
   VAR nUnitWidthDT2 INIT 75 // for data 50 * 1.3. MontCalendar + Week

   VAR nScaleWidth  INIT 100 // % width
   VAR nScaleHeight INIT 100 // % height

   VAR nPixWidth    INIT 0
   VAR nPixHeight   INIT 0
   VAR nPixHeight2  INIT 0
   VAR nPixWidthDT  INIT 0
   VAR nPixWidthDT1 INIT 0
   VAR nPixWidthDT2 INIT 0
   VAR nGapsWidth   INIT 0
   VAR nGapsHeight  INIT 0
   VAR nMargWidth   INIT 0
   VAR nMargHeight  INIT 0

   VAR nY INIT 0
   VAR nX INIT 0

   VAR nL INIT 0
   VAR nT INIT 0
   VAR nR INIT 0
   VAR nB INIT 0
   VAR nSize INIT 0

   METHOD New( nPrcW, nPrcH, nSize ) INLINE ( ::nScaleWidth := hb_defaultValue( nPrcW, 100 ), ;
      ::nScaleHeight := hb_defaultValue( nPrcH, 100 ), ::nSize := hb_defaultValue( nSize, ::nSize ), ;
      ::UnitsToPixels(), ::oEvent:Obj := Self, Self ) CONSTRUCTOR
  _METHOD UnitsToPixels( nPrcW, nPrcH )
   METHOD DLU2PixH( nHeight, nPrc ) INLINE Round( ( UnitsToPixelsY( nHeight ) * 13 * nPrc ) / 1500, 0 )
   METHOD DLU2PixW( nWidth, nPrc ) INLINE Round( ( UnitsToPixelsX( nWidth ) * 13 * nPrc ) / 1500, 0 )
  _METHOD Kfc( nKfcW, nKfcH )
  _METHOD ToVal( nKfc, nVal )
  _METHOD GetGaps( aGaps, oWnd )
  _METHOD Event( Key, p1, p2, p3 )
   METHOD TextWidth( cText, nSize, cFont, lBold, cChar )
   METHOD Breadth( nW, k )

  _METHOD D ( nKfc )
   METHOD W ( nKfc ) INLINE ::ToVal( nKfc, ::nPixWidth )
   METHOD H ( nKfc ) INLINE ::ToVal( nKfc, ::nPixHeight )
   METHOD H_( nKfc ) INLINE ::ToVal( nKfc, ::nPixHeight2 )
   METHOD G ( nKfc, lW ) INLINE iif( Empty( lW ), ::GW( nKfc ), ::GH( nKfc ) )
   METHOD GW( nKfc ) INLINE ::ToVal( nKfc, ::nGapsWidth )
   METHOD GH( nKfc ) INLINE ::ToVal( nKfc, ::nGapsHeight )
   METHOD M ( nKfc, lW ) INLINE iif( Empty( lW ), ::MW( nKfc ), ::MH( nKfc ) )
   METHOD MW( nKfc ) INLINE ::ToVal( nKfc, ::nMargWidth )
   METHOD MH( nKfc ) INLINE ::ToVal( nKfc, ::nMargHeight )

   ASSIGN Handle( hWnd ) INLINE ( ::hWnd := hWnd, ::lError := Empty( hWnd ), ;
          iif( ::lError, MsgMiniGuiError( ::cError ), ) )
   ACCESS IsError    INLINE ::lError
   ACCESS Wm_nApp    INLINE WM_APP_LAUNCH
   ACCESS IsMsg          INLINE ( ::lAction .AND. ! ::lError )

   ACCESS Action          INLINE ::lAction
   ASSIGN Action( lAction ) INLINE ::lAction := !( Empty( lAction ) )

   ACCESS GapsWidth INLINE ::GW()
   ACCESS GapsHeight INLINE ::GH()
   ACCESS Left INLINE ::MW()
   ACCESS Top  INLINE ::MH()
   ACCESS Right  INLINE ::MW()
   ACCESS Bottom INLINE ::MH()

   ACCESS O  INLINE ::oCargo
   ACCESS P  INLINE ::oProp

   ACCESS Y  INLINE ::nY
   ASSIGN Y( y ) INLINE ::nY := y
   ACCESS X  INLINE ::nX
   ASSIGN X( x ) INLINE ::nX := x

   ACCESS LTRB INLINE { ::nL, ::nT, ::nR, ::nB }

   ACCESS L  INLINE ::nL
   ASSIGN L( n ) INLINE ::nL := n
   ACCESS T  INLINE ::nT
   ASSIGN T( n ) INLINE ::nT := n
   ACCESS R  INLINE ::nR
   ASSIGN R( n ) INLINE ::nR := n
   ACCESS B  INLINE ::nB
   ASSIGN B( n ) INLINE ::nB := n

   ACCESS W1     INLINE ::W ( 1 )
   ACCESS W2     INLINE ::W ( 2 )
   ACCESS W3     INLINE ::W ( 3 )
   ACCESS W4     INLINE ::W ( 4 )
   ACCESS W5     INLINE ::W ( 5 )

   ACCESS H1     INLINE ::H ()
   ACCESS H2     INLINE ::H_()
   ACCESS H3     INLINE ::H1 + ::H2
   ACCESS H4     INLINE ::H2 + ::H2
   ACCESS H5     INLINE ::H1 + ::H4

   ACCESS D1     INLINE ::D ( 1 )
   ACCESS D2     INLINE ::D ( 2 )
   ACCESS D3     INLINE ::D ( 3 )

   METHOD Create() INLINE _App_Dlu2Pix_Events_( Self )

   METHOD Post ( nKey, nPar, xPar ) INLINE ::PostMsg( nKey, nPar, xPar )
   METHOD PostMsg( nKey, nPar, xPar ) INLINE ( nPar := hb_defaultValue( nPar, 0 ), ;
      iif( ::IsMsg, ( ::oParam:Set( nKey, xPar ), ;
      PostMessage( ::hWnd, ::Wm_nApp, nKey, nPar ) ), Nil ) )
   METHOD Send ( nKey, nPar, xPar ) INLINE ::SendMsg( nKey, nPar, xPar )
   METHOD SendMsg( nKey, nPar, xPar ) INLINE ( nPar := hb_defaultValue( nPar, 0 ), ;
      iif( ::IsMsg, ( ::oParam:Set( nKey, xPar ), ;
      SendMessage( ::hWnd, ::Wm_nApp, nKey, nPar ) ), Nil ) )

ENDCLASS

METHOD Event( Key, p1, p2, p3 ) CLASS TDlu2Pix

   LOCAL cMsg := ""

   IF ::lError
      cMsg := ::cError

   ELSEIF HB_ISBLOCK( p1 )
      IF HB_ISARRAY( Key )
         cMsg := Key[2]
         Key  := Key[1]
         IF !Empty( cMsg )
            ::oEvents:Set( cMsg, Key )
            ::oEvents:Set( Key, cMsg )
         ENDIF
      ENDIF
      ::oEvent:Set( Key, p1 )

   ELSE
      IF HB_ISCHAR( Key ) ; Key := ::oEvents:Get( Key, Key )
      ENDIF
      p2 := hb_defaultValue( p2, ::oParam:Get( Key ) )
      ::oEvent:Do( Key, p1, p2, p3 )
   ENDIF

RETURN cMsg

METHOD UnitsToPixels( nPrcW, nPrcH ) CLASS TDlu2Pix

   DEFAULT nPrcW := hb_defaultValue( nPrcW, ::nScaleWidth ), ;
           nPrcH := hb_defaultValue( nPrcH, ::nScaleHeight )

   ::nScaleWidth  := nPrcW
   ::nScaleHeight := nPrcH

   ::nPixWidth := ::DLU2PixW( ::nUnitWidth, nPrcW )
   ::nPixHeight := ::DLU2PixH( ::nUnitHeight, nPrcH )
   ::nPixHeight2 := ::DLU2PixH( ::nUnitHeight2, nPrcH )

   ::nGapsWidth := ::DLU2PixW( ::nUnitGapsWidth, nPrcW )
   ::nGapsHeight := ::DLU2PixH( ::nUnitGapsHeight, nPrcH )
   ::nMargWidth := ::DLU2PixW( ::nUnitMargWidth, nPrcW )
   ::nMargHeight := ::DLU2PixH( ::nUnitMargHeight, nPrcH )

   ::nPixWidthDT := ::DLU2PixW( ::nUnitWidthDT, nPrcW )
   ::nPixWidthDT1 := ::DLU2PixH( ::nUnitWidthDT1, nPrcH )
   ::nPixWidthDT2 := ::DLU2PixH( ::nUnitWidthDT2, nPrcH )

RETURN NIL

METHOD Kfc( nKfcW, nKfcH ) CLASS TDlu2Pix

   If ! Empty( nKfcW )
      ::nPixWidth += Int( ::nPixWidth * nKfcW )
      ::nPixWidthDT += Int( ::nPixWidthDT * nKfcW )
      ::nPixWidthDT1 += Int( ::nPixWidthDT1 * nKfcW )
      ::nPixWidthDT2 += Int( ::nPixWidthDT2 * nKfcW )
      ::nGapsWidth += Int( ::nGapsWidth * nKfcW )
      ::nMargWidth += Int( ::nMargWidth * nKfcW )
   ENDIF

   If ! Empty( nKfcH )
      ::nPixHeight += Int( ::nPixHeight * nKfcH )
      ::nPixHeight2 += Int( ::nPixHeight2 * nKfcH )
      ::nGapsHeight += Int( ::nGapsHeight * nKfcW )
      ::nMargHeight += Int( ::nMargHeight * nKfcW )
   ENDIF

RETURN NIL

METHOD ToVal( nKfc, nVal ) CLASS TDlu2Pix

   IF HB_ISNUMERIC( nKfc ) .AND. nKfc > 0
      nVal := Int( nKfc * nVal )
   ENDIF

RETURN nVal

METHOD GetGaps( aGaps, oWnd ) CLASS TDlu2Pix

   LOCAL oApp, nGapW, nGapH, n

   If HB_ISCHAR( oWnd ); oWnd := _WindowObj( oWnd )
   EndIf

   oApp  := iif( Empty( oWnd ), Self           , oWnd:App )
   nGapW := iif( Empty( oWnd ), oApp:GapsWidth , oWnd:GapsWidth  )
   nGapH := iif( Empty( oWnd ), oApp:GapsHeight, oWnd:GapsHeight )

   If HB_ISNUMERIC( aGaps )
      n   := aGaps
      aGaps := Array( 4 )
      AFill( aGaps, n )
   EndIf

   DEFAULT aGaps := { 0, 0, nGapW, nGapH }

   ::nL := 0
   ::nT := 0
   ::nR := 0
   ::nB := 0

   If Len(aGaps) == 2
      If ! HB_ISNUMERIC( aGaps[1] ); aGaps[1] := nGapW
      EndIf
      If ! HB_ISNUMERIC( aGaps[2] ); aGaps[2] := nGapH
      EndIf
      ::nL := aGaps[1]
      ::nR := aGaps[1]
      ::nT := aGaps[2]
      ::nB := aGaps[2]
   Else
      If Len( aGaps ) != 4; ASize( aGaps, 4 )
      EndIf
      If ! HB_ISNUMERIC( aGaps[1] ); aGaps[1] := nGapW
      EndIf
      If ! HB_ISNUMERIC( aGaps[2] ); aGaps[2] := nGapH
      EndIf
      If ! HB_ISNUMERIC( aGaps[3] ); aGaps[3] := nGapW
      EndIf
      If ! HB_ISNUMERIC( aGaps[4] ); aGaps[4] := nGapH
      EndIf
      ::nL := aGaps[1]
      ::nT := aGaps[2]
      ::nR := aGaps[3]
      ::nB := aGaps[4]
   EndIf

   If '.' $ hb_ntos( ::nL ); ::nL := oApp:GW( ::nL )
   EndIf

   If '.' $ hb_ntos( ::nT); ::nT := oApp:GH( ::nT )
   EndIf

   If '.' $ hb_ntos( ::nR ); ::nR := oApp:GW( ::nR )
   EndIf

   If '.' $ hb_ntos( ::nB ); ::nB := oApp:GH( ::nB )
   EndIf

RETURN ( ::LTRB )

METHOD TextWidth( cText, nSize, cFont, lBold, cChar ) CLASS TDlu2Pix

   LOCAL hFont, nWidth, cTxt := "", cTmp

   IF HB_ISARRAY( cText )
      FOR EACH cTmp IN cText
         IF !HB_ISCHAR( cTmp ) ; cTmp := cValToChar( cTmp )
         ENDIF
         IF Len( cTmp ) > Len( cTxt ) ; cTxt := cTmp
         ENDIF
      NEXT
      cText := cTxt
   ELSEIF HB_ISNUMERIC( cText ) ; cText := Replicate( cChar, cText )
   ENDIF
   cChar := hb_defaultValue( cChar, 'A' )
   cText := hb_defaultValue( cText, Replicate( cChar, 2 ) )
   lBold := hb_defaultValue( lBold, .F. )
   cFont := hb_defaultValue( cFont, _HMG_DefaultFontName )
   nSize := hb_defaultValue( nSize, iif( Empty( ::nSize ), _HMG_DefaultFontSize, ::nSize ) )

   hFont := InitFont( cFont, nSize, lBold )
   nWidth := GetTextWidth( Nil, cText, hFont )
   DeleteObject( hFont )

RETURN nWidth

METHOD Breadth( nW, k ) CLASS TDlu2Pix

   LOCAL nWidth := 0, cW := "", cTmp

   IF HB_ISARRAY( nW )
      FOR EACH cTmp IN nW
          IF Len( cTmp ) > Len( cW ) ; cW := cTmp
          ENDIF
      NEXT
      nW := ::TextWidth( cW )
   ELSEIF HB_ISCHAR( nW )
      nW := ::TextWidth( nW )
   ENDIF

   WHILE nW > ( nWidth += ::W( hb_defaultValue( k, 0.5 ) ) )
   END

RETURN nWidth

METHOD D( nKfc ) CLASS TDlu2Pix

   LOCAL nVal := ::nPixWidthDT

   IF HB_ISNUMERIC( nKfc ) .AND. nKfc > 0
      IF nKfc == 1 ; nVal := ::nPixWidthDT
      ELSEIF nKfc == 2 ; nVal := ::nPixWidthDT1
      ELSEIF nKfc == 3 ; nVal := ::nPixWidthDT2
      ELSE ; nVal := Int( nKfc * nVal )
      ENDIF
   ENDIF

RETURN nVal

///////////////////////////////////////////////////////////////////////////////
CLASS TWndData
///////////////////////////////////////////////////////////////////////////////

   PROTECTED:
   VAR cVar INIT ''
   VAR cName INIT ''
   VAR cType INIT ''
   VAR nIndex INIT 0
   VAR nHandle INIT 0
   VAR nParent INIT 0
   VAR cChr INIT ','
   VAR lAction INIT .T.

   VAR oStatusBar AS OBJECT
   VAR oProp AS OBJECT
   VAR oName AS OBJECT
   VAR oHand AS OBJECT

   VAR nY INIT 0
   VAR nX INIT 0
   VAR nLeft INIT 0
   VAR nTop INIT 0
   VAR nRight INIT 0
   VAR nBottom INIT 0
   VAR nGapWidth INIT 0
   VAR nGapHeight INIT 0

   EXPORTED:
   VAR oApp AS OBJECT
   VAR oCargo AS OBJECT
   VAR oUserKeys AS OBJECT
   VAR oEvent AS OBJECT
   VAR oOnEventBlock AS OBJECT
   VAR oParam AS OBJECT
   VAR oEvents AS OBJECT
   VAR cProcFile INIT ""
   VAR cProcName INIT ""
   VAR nProcLine INIT 0

   METHOD New() INLINE ( ::oApp := oDlu2Pixel(), Self ) CONSTRUCTOR

   METHOD Def( nIndex, cName, nHandle, nParent, cType, cVar ) INLINE ( ;
      ::nIndex := nIndex, ::cName := cName, ::nHandle := nHandle, ;
      ::nParent := nParent, ::cType := cType, ::cVar := cVar, ;
      ::oCargo := oHmgData(), ::oOnEventBlock := oKeyData( Self, .T. ), ;
      ::oEvent := oKeyData( Self ), ::oUserKeys := oHmgData(), ;
      ::oName := oHmgData(), ::oHand := oHmgData(), ;
      ::oProp := oHmgData(), ::oParam := oHmgData(), ;
      ::oEvents := oHmgData(), hmg_SetWindowObject( ::nHandle, Self ), ;
      ::nLeft := ::oApp:Left, ::nRight := ::oApp:Right, ;
      ::nTop := ::oApp:Top, ::nBottom := ::oApp:Bottom, ;
      ::nGapWidth := ::oApp:GapsWidth, ;
      ::nGapHeight := ::oApp:GapsHeight, ;
      Self )

   ACCESS Left INLINE ::nLeft
   ASSIGN Left ( n ) INLINE ::nLeft := n
   ACCESS Top INLINE ::nTop
   ASSIGN Top ( n ) INLINE ::nTop := n
   ACCESS Right INLINE ::nRight
   ASSIGN Right ( n ) INLINE ::nRight := n
   ACCESS Bottom INLINE ::nBottom
   ASSIGN Bottom( n ) INLINE ::nBottom := n

   ACCESS GapsWidth INLINE ::nGapWidth
   ASSIGN GapsWidth ( n ) INLINE ::nGapWidth := n
   ACCESS GapsHeight INLINE ::nGapHeight
   ASSIGN GapsHeight( n ) INLINE ::nGapHeight := n

   ACCESS App      INLINE ::oApp

   ACCESS LTRB   INLINE ::oApp:LTRB

   ACCESS L      INLINE ::oApp:nL
   ASSIGN L( n )   INLINE ::oApp:nL := n
   ACCESS T      INLINE ::oApp:nT
   ASSIGN T( n )   INLINE ::oApp:nT := n
   ACCESS R      INLINE ::oApp:nR
   ASSIGN R( n )   INLINE ::oApp:nR := n
   ACCESS B      INLINE ::oApp:nB
   ASSIGN B( n )   INLINE ::oApp:nB := n

   ACCESS AO         INLINE ::oApp:oCargo
   ACCESS AP         INLINE ::oApp:oProp

   ACCESS O      INLINE ::oCargo
   ACCESS P      INLINE ::oProp

   ACCESS WO         INLINE ::oCargo
   ACCESS WP         INLINE ::oProp

   ACCESS Y      INLINE ::nY
   ASSIGN Y( y )   INLINE ::nY := y
   ACCESS X      INLINE ::nX
   ASSIGN X( x )   INLINE ::nX := x

   METHOD GetGaps( aGaps, oWnd ) INLINE ::oApp:GetGaps( aGaps, oWnd )

   METHOD W ( nKfc ) INLINE ::oApp:W ( nKfc )
   ACCESS W1         INLINE ::oApp:W1
   ACCESS W2         INLINE ::oApp:W2
   ACCESS W3         INLINE ::oApp:W3
   ACCESS W4         INLINE ::oApp:W4
   ACCESS W5         INLINE ::oApp:W5

   METHOD H ( nKfc ) INLINE ::oApp:H ( nKfc )
   ACCESS H1         INLINE ::oApp:H1
   ACCESS H2         INLINE ::oApp:H2
   ACCESS H3         INLINE ::oApp:H3
   ACCESS H4         INLINE ::oApp:H4
   ACCESS H5         INLINE ::oApp:H5

   METHOD D ( nKfc ) INLINE ::oApp:D ( nKfc )
   ACCESS D1         INLINE ::oApp:D1
   ACCESS D2         INLINE ::oApp:D2
   ACCESS D3         INLINE ::oApp:D3

   METHOD GW( nKfc ) INLINE ::oApp:GW( nKfc )
   METHOD GH( nKfc ) INLINE ::oApp:GH( nKfc )

   METHOD MW( nKfc ) INLINE ::oApp:MW( nKfc )
   METHOD MH( nKfc ) INLINE ::oApp:MH( nKfc )

   ACCESS Index           INLINE ::nIndex
   ACCESS Name        INLINE ::cName
   ACCESS Handle        INLINE ::nHandle
   ACCESS Parent        INLINE ::nParent
   ACCESS Type        INLINE ::cType
   ACCESS VarName     INLINE ::cVar
   ACCESS FocusedControl  INLINE _GetFocusedControl ( ::cName )
   ACCESS Row           INLINE GetWindowRow ( ::nHandle )
   ASSIGN Row ( nVal )  INLINE _SetWindowSizePos( ::cName, nVal, , , )
   ACCESS Col           INLINE GetWindowCol ( ::nHandle )
   ASSIGN Col ( nVal )  INLINE _SetWindowSizePos( ::cName, , nVal, , )
   ACCESS Width           INLINE GetWindowWidth ( ::nHandle )
   ASSIGN Width ( nVal )  INLINE _SetWindowSizePos( ::cName, , , nVal, )
   ACCESS Height        INLINE GetWindowHeight ( ::nHandle )
   ASSIGN Height( nVal )  INLINE _SetWindowSizePos( ::cName, , , , nVal )
   ACCESS ClientWidth     INLINE _GetClientRect ( ::nHandle )[ 3 ]
   ACCESS ClientHeight  INLINE _GetClientRect ( ::nHandle )[ 4 ]
   ACCESS Title           INLINE GetWindowText ( ::nHandle )
   ASSIGN Title( cVal ) INLINE SetWindowText ( ::nHandle, cVal )
   ACCESS Enabled     INLINE IsWindowEnabled ( ::nHandle )
   ASSIGN Enabled( xVal ) INLINE iif( Empty( xVal ), DisableWindow ( ::nHandle ), EnableWindow ( ::nHandle ) )
   METHOD Enabler( cName, xVal ) INLINE SetProperty( ::cName, cName, "Enabled", ! Empty( xVal ) )

   ACCESS BackColor             INLINE  GetProperty( ::cName, 'BACKCOLOR' )
   ASSIGN BackColor( Val )     INLINE  SetProperty( ::cName, 'BACKCOLOR', Val )

   ACCESS Cargo INLINE _WindowCargo( Self )
   ASSIGN Cargo( xVal ) INLINE _WindowCargo( Self, xVal )

   ACCESS IsWindow INLINE .T.
   ACCESS IsControl INLINE .F.
   ACCESS Chr INLINE ::cChr
   ASSIGN Chr( cChr ) INLINE ::cChr := iif( HB_ISCHAR( cChr ), cChr, ::cChr )

   ACCESS Action INLINE ::lAction
   ASSIGN Action( lAction ) INLINE ::lAction := !( Empty( lAction ) )

   ACCESS StatusBar INLINE ::oStatusBar
   ACCESS HasStatusBar INLINE ! Empty( ::oStatusBar )
   ACCESS bOnEvent INLINE ::oOnEventBlock

   ACCESS WM_nMsgW INLINE WM_WND_LAUNCH
   ACCESS WM_nMsgC INLINE WM_CTL_LAUNCH

   METHOD SetProp( xKey, xVal ) INLINE ::oProp:Set( xKey, xVal )
   METHOD GetProp( xKey ) INLINE iif( xKey == NIL, ::oProp, ::oProp:Get( xKey ) )
   METHOD DelProp( xKey ) INLINE ::oProp:Del( xKey )
   METHOD AllProp( lArray ) INLINE ::oProp:GetAll( lArray )

   METHOD UserKeys( Key, Block, p2, p3 ) INLINE iif( HB_ISBLOCK( Block ), ::oUserKeys:Set( Key, Block ), ;
      iif( ::lAction, ::oUserKeys:Do( Key, Block, p2, p3 ), Nil ) )

   METHOD Post ( nKey, nHandle, xPar ) INLINE ::PostMsg( nKey, nHandle, xPar )
   METHOD PostMsg( nKey, nHandle, xPar ) INLINE iif( ::lAction, ( ::oParam:Set( nKey, xPar ), ;
      PostMessage( ::nHandle, ::WM_nMsgW, nKey, hb_defaultValue( nHandle, 0 ) ) ), Nil )
   METHOD Send ( nKey, nHandle, xPar ) INLINE ::SendMsg( nKey, nHandle, xPar )
   METHOD SendMsg( nKey, nHandle, xPar ) INLINE iif( ::lAction, ( ::oParam:Set( nKey, xPar ), ;
      SendMessage( ::nHandle, ::WM_nMsgW, nKey, hb_defaultValue( nHandle, 0 ) ) ), Nil )

   METHOD Release() INLINE iif( ::IsWindow, ;
      iif( ::lAction, PostMessage( ::nHandle, WM_CLOSE, 0, 0 ), Nil ), Nil )

   METHOD Maximize() INLINE ShowWindow( ::nHandle, SW_MAXIMIZE )
   METHOD Minimize() INLINE ShowWindow( ::nHandle, SW_MINIMIZE )
   METHOD Restore()  INLINE ShowWindow( ::nHandle, SW_RESTORE )
   METHOD Show() INLINE _ShowWindow( ::cName )
   METHOD Hide() INLINE _HideWindow( ::cName )
   METHOD SetFocus( xName ) INLINE iif( Empty( xName ), SetFocus( ::nHandle ), ;
      iif( HB_ISOBJECT( ::GetObj( xName ) ), ::GetObj( xName ):SetFocus(), DoMethod( ::cName, xName, "SetFocus" ) ) )
   METHOD SetSize( y, x, w, h ) INLINE _SetWindowSizePos( ::cName, y, x, w, h )

   _METHOD Event( Key, Block, p2, p3 )
   _METHOD DoEvent( Key, nHandle )
   _METHOD GetListType()
   _METHOD GetObj4Type( cType, lEque )
   _METHOD GetObj4Name( cName )

   METHOD GetObj( xName ) INLINE iif( HB_ISCHAR( xName ), ::oName:Get( Upper( xName ) ), ;
      ::oHand:Get( xName ) )
   // Destructor
   METHOD Destroy() INLINE ( ;
      ::oCargo := iif( HB_ISOBJECT( ::oCargo ), ::oCargo:Destroy(), Nil ), ;
      ::oEvent := iif( HB_ISOBJECT( ::oEvent ), ::oEvent:Destroy(), Nil ), ;
      ::oOnEventBlock := iif( HB_ISOBJECT( ::oOnEventBlock ), ::oOnEventBlock:Destroy(), Nil ), ;
      ::oStatusBar := iif( HB_ISOBJECT( ::oStatusBar ), ::oStatusBar:Destroy(), Nil ), ;
      ::oName := iif( HB_ISOBJECT( ::oName ), ::oName:Destroy(), Nil ), ;
      ::oHand := iif( HB_ISOBJECT( ::oHand ), ::oHand:Destroy(), Nil ), ;
      ::oProp := iif( HB_ISOBJECT( ::oProp ), ::oProp:Destroy(), Nil ), ;
      ::oParam := iif( HB_ISOBJECT( ::oParam ), ::oParam:Destroy(), Nil ), ;
      ::oUserKeys := iif( HB_ISOBJECT( ::oUserKeys ), ::oUserKeys:Destroy(), Nil ), ;
      ::nIndex := ::nParent := ::cType := ::cName := ::cVar := ::cChr := NIL, ;
      hmg_DelWindowObject( ::nHandle ), ::nHandle := Nil )

#ifndef __XHARBOUR__
   DESTRUCTOR DestroyObject()
#endif

   ERROR HANDLER ControlAssign

ENDCLASS
///////////////////////////////////////////////////////////////////////////////

METHOD Event ( Key, Block, p2, p3 ) CLASS TWndData

   LOCAL cKey

   IF HB_ISBLOCK( Block )
      IF HB_ISARRAY( Key )
         cKey := Key[2]
         Key  := Key[1]
         IF !Empty( cKey ) ; ::oEvents:Set( cKey, Key )
         ENDIF
      ENDIF
      ::oEvent:Set( Key, Block )
   ELSE
      IF HB_ISCHAR( Key ) ; Key := ::oEvents:Get( Key, Key )
      ENDIF
      IF Key != NIL .AND. ::lAction
         ::oEvent:Do( Key, Block, p2, p3 )
      ENDIF
   ENDIF

RETURN Self

METHOD ControlAssign( xValue ) CLASS TWndData

   LOCAL cMessage, uRet, lError, o

   cMessage := __GetMessage()
   lError := .T.

   IF PCount() == 0
      o := ::GetObj( cMessage )
      IF HB_ISOBJECT( o )
         uRet := _GetValue( , , o:nIndex )
         lError := .F.
      ENDIF
   ELSEIF PCount() == 1
      o := ::GetObj( SubStr( cMessage, 2 ) )
      IF HB_ISOBJECT( o )
         _SetValue( , , xValue, o:nIndex )
         uRet := _GetValue( , , o:nIndex )
         lError := .F.
      ENDIF
   ENDIF

   IF lError
      uRet := NIL
      ::MsgNotFound( cMessage )
   ENDIF

RETURN uRet

METHOD GetListType() CLASS TWndData

   LOCAL oType := oKeyData()
   LOCAL aType

   ::oName:Eval( {| o | oType:Set( o:cType, o:cType ) } )
   aType := oType:Eval( .T. )
   oType:Destroy()
   oType := NIL

RETURN aType

METHOD GetObj4Type( cType, lEque ) CLASS TWndData

   LOCAL aObj := {}, aRet := {}, o

   IF ! Empty( cType )
      hb_default( @lEque, .T. )
      IF ::cChr $ cType ; lEque := .F.
      ENDIF
      FOR EACH cType IN hb_ATokens( Upper( cType ), ::cChr )
         ::oName:Eval( {| oc | iif( lEque, iif( cType == oc:cType, AAdd( aObj, oc ), ), ;
            iif( cType $ oc:cType, AAdd( aObj, oc ), ) ) } )
      NEXT
      FOR EACH o IN aObj
         IF _IsControlDefined( o:Name, o:Window:Name )
            aAdd( aRet, o )
         ENDIF
      NEXT
   ENDIF

RETURN aRet

METHOD GetObj4Name( cName ) CLASS TWndData

   LOCAL aObj := {}

   IF ! Empty( cName )
      FOR EACH cName IN hb_ATokens( Upper( cName ), ::cChr )
         ::oName:Eval( {| oc | iif( _IsControlDefined( oc:Name, oc:Window:Name ), ;
            iif( cName $ Upper( oc:cName ), AAdd( aObj, oc ), Nil ), Nil ) } )
      NEXT
   ENDIF

RETURN aObj

METHOD DoEvent ( Key, nHandle ) CLASS TWndData

   LOCAL o := Self
   LOCAL i := o:Index
   LOCAL w := o:IsWindow
   LOCAL p := o:oParam:Get( Key )

   IF ! Empty( nHandle )
      IF nHandle > 0 .AND. nHandle <= Len( _HMG_aControlHandles ) // control index
         IF hmg_IsWindowObject( _HMG_aControlHandles[ nHandle ] )
            o := hmg_GetWindowObject( _HMG_aControlHandles[ nHandle ] )
            i := o:Index
            w := o:IsWindow
         ELSE
            i := nHandle
            w := .F.
         ENDIF
      ELSEIF hmg_IsWindowObject( nHandle ) // control handle
         o := hmg_GetWindowObject( nHandle )
         i := o:Index
         w := o:IsWindow
      ENDIF
   ENDIF

   IF w
      RETURN Do_WindowEventProcedure ( ::oEvent:Get( Key ), i, o, Key, p )
   ENDIF

RETURN Do_ControlEventProcedure( ::oEvent:Get( Key ), i, o, Key, p )

#ifndef __XHARBOUR__
METHOD PROCEDURE DestroyObject() CLASS TWndData

   ::Destroy()

RETURN
#endif

///////////////////////////////////////////////////////////////////////////////
CLASS TCnlData INHERIT TWndData
///////////////////////////////////////////////////////////////////////////////

   PROTECTED:
   VAR oWin AS OBJECT

   EXPORTED:
   METHOD New( oWnd ) INLINE ( ::Super:New(), ::oWin := oWnd, Self ) CONSTRUCTOR

   METHOD Def( nIndex, cName, nHandle, nParent, cType, cVar ) INLINE ( ;
      ::Super:Def( nIndex, cName, nHandle, nParent, cType, cVar ), ;
      ::Set(), hmg_SetWindowObject( ::nHandle, Self ), ;
      Self )

   ACCESS Row INLINE _GetControlRow ( ::cName, ::oWin:Name )
   ASSIGN Row ( nVal ) INLINE _SetControlRow ( ::cName, ::oWin:Name, nVal )
   ACCESS Col INLINE _GetControlCol ( ::cName, ::oWin:Name )
   ASSIGN Col ( nVal ) INLINE _SetControlCol ( ::cName, ::oWin:Name, nVal )
   ACCESS Width INLINE _GetControlWidth ( ::cName, ::oWin:Name )
   ASSIGN Width ( nVal ) INLINE _SetControlWidth ( ::cName, ::oWin:Name, nVal )
   ACCESS Height INLINE _GetControlHeight( ::cName, ::oWin:Name )
   ASSIGN Height( nVal ) INLINE _SetControlHeight( ::cName, ::oWin:Name, nVal )

   ACCESS Align INLINE GetProperty( ::oWin:Name, ::cName, 'ALIGNMENT' )
   ASSIGN Align( cAlign ) INLINE SetProperty( ::oWin:Name, ::cName, 'ALIGNMENT', cAlign )

   ACCESS BackColor                     INLINE  GetProperty( ::oWin:cName, ::cName, 'BACKCOLOR'    )
   ASSIGN BackColor( Val )            INLINE  SetProperty( ::oWin:cName, ::cName, 'BACKCOLOR', Val )
   ACCESS FontColor                     INLINE  GetProperty( ::oWin:cName, ::cName, 'FONTCOLOR'    )
   ASSIGN FontColor( Val )            INLINE  SetProperty( ::oWin:cName, ::cName, 'FONTCOLOR', Val )

   ACCESS Title INLINE ::oWin:cTitle
   ACCESS Caption INLINE _GetCaption ( ::cName, ::oWin:cName )
   ACCESS Cargo INLINE _ControlCargo( Self )
   ASSIGN Cargo( xVal ) INLINE _ControlCargo( Self, , xVal )

   ACCESS WO INLINE ::oWin:oCargo
   ACCESS WP INLINE ::oWin:oProp

   ACCESS Window INLINE ::oWin
   ACCESS IsWindow INLINE .F.
   ACCESS IsControl INLINE .T.

   METHOD PostMsg( nKey, xPar ) INLINE iif( ::oWin:Action, ( ::oParam:Set( nKey, xPar ), ;
      PostMessage( ::oWin:nHandle, ::WM_nMsgC, nKey, ::nHandle ) ), Nil )
   METHOD Post ( nKey, xPar ) INLINE ::PostMsg( nKey, xPar )
   METHOD SendMsg( nKey, xPar ) INLINE iif( ::oWin:Action, ( ::oParam:Set( nKey, xPar ), ;
      SendMessage( ::oWin:nHandle, ::WM_nMsgC, nKey, ::nHandle ) ), Nil )
   METHOD Send ( nKey, xPar ) INLINE ::SendMsg( nKey, xPar )

   METHOD Set() INLINE ( iif( HB_ISOBJECT( ::oWin:oName ), ::oWin:oName:Set( Upper( ::cName ), Self ), ), ;
      iif( HB_ISOBJECT( ::oWin:oHand ), ::oWin:oHand:Set( ::nHandle, Self ), ) )
   METHOD Del() INLINE ( iif( HB_ISOBJECT( ::oWin:oName ), ;
      iif( HB_ISCHAR( ::cName ), ::oWin:oName:Del( Upper( ::cName ) ), ), ), ;
      iif( HB_ISOBJECT( ::oWin:oHand ), ;
      iif( HB_ISNUMERIC( ::nHandle ), ::oWin:oHand:Del( ::nHandle ), ), ) )

   METHOD Get( xName ) INLINE iif( HB_ISCHAR( xName ), ::oWin:oName:Get( Upper( xName ) ), ;
      ::oWin:oHand:Get( xName ) )

   METHOD GetListType() INLINE ::oWin:GetListType()
   METHOD GetObj4Type( cType, lEque ) INLINE ::oWin:GetObj4Type( cType, lEque )
   METHOD GetObj4Name( cName ) INLINE ::oWin:GetObj4Name( cName )
   METHOD SetProp( xKey, xVal ) INLINE ::oWin:oProp:Set( xKey, xVal )
   METHOD GetProp( xKey ) INLINE iif( PCount() > 0, ::oWin:oProp:Get( xKey ), ::oWin:oProp )
   METHOD DelProp( xKey ) INLINE ::oWin:oProp:Del( xKey )

   ACCESS Value INLINE _GetValue( , , ::nIndex )
   ASSIGN Value( xVal ) INLINE ( _SetValue( , , xVal, ::nIndex ), ;
      _GetValue( , , ::nIndex ) )

   METHOD SetFocus() INLINE _SetFocus ( ::cName, ::oWin:cName )
   METHOD Refresh() INLINE _Refresh ( ::nIndex )
   METHOD SetSize( y, x, w, h ) INLINE _SetControlSizePos( ::cName, ::oWin:cName, y, x, w, h )

   METHOD Disable( nPos ) INLINE _DisableControl( ::cName, ::oWin:cName, nPos )
   METHOD Enable ( nPos ) INLINE _EnableControl ( ::cName, ::oWin:cName, nPos )
   METHOD Enabled( nPos ) INLINE _IsControlEnabled ( ::cName, ::oWin:cName, nPos )

   METHOD Restore() INLINE ::Show()
   METHOD Show() INLINE _ShowControl ( ::cName, ::oWin:cName )
   METHOD Hide() INLINE _HideControl ( ::cName, ::oWin:cName )

   _METHOD DoEvent ( Key, nHandle )

   // Destructor
   METHOD Destroy() INLINE ( ::Del(), ;
      ::oCargo := iif( HB_ISOBJECT( ::oCargo ), ::oCargo:Destroy(), Nil ), ;
      ::oEvent := iif( HB_ISOBJECT( ::oEvent ), ::oEvent:Destroy(), Nil ), ;
      ::oOnEventBlock := iif( HB_ISOBJECT( ::oOnEventBlock ), ::oOnEventBlock:Destroy(), Nil ), ;
      ::oUserKeys := iif( HB_ISOBJECT( ::oUserKeys ), ::oUserKeys:Destroy(), Nil ), ;
      ::oName := iif( HB_ISOBJECT( ::oName ), ::oName:Destroy(), Nil ), ;
      ::oHand := iif( HB_ISOBJECT( ::oHand ), ::oHand:Destroy(), Nil ), ;
      ::nParent := ::nIndex := ::cName := ::cType := ::cVar := ::cChr := NIL, ;
      hmg_DelWindowObject( ::nHandle ), ::nHandle := Nil )

#ifndef __XHARBOUR__
   DESTRUCTOR DestroyObject()
#endif

ENDCLASS
///////////////////////////////////////////////////////////////////////////////

METHOD DoEvent ( Key, nHandle ) CLASS TCnlData

   LOCAL o := iif( hmg_IsWindowObject( nHandle ), hmg_GetWindowObject( nHandle ), Self )

RETURN Do_ControlEventProcedure( ::oEvent:Get( Key ), o:Index, o, Key, ::oParam:Get( Key ) )

#ifndef __XHARBOUR__
METHOD PROCEDURE DestroyObject() CLASS TCnlData

   ::Destroy()

RETURN
#endif
///////////////////////////////////////////////////////////////////////////////
CLASS TGetData INHERIT TCnlData
///////////////////////////////////////////////////////////////////////////////

   PROTECTED:
   VAR oGetBox AS OBJECT

   EXPORTED:
   METHOD New( oWnd, oGet ) INLINE ( ::Super:New( oWnd ), ::oGetBox := oGet, Self ) CONSTRUCTOR

   METHOD Def( nIndex, cName, nHandle, nParent, cType, cVar ) INLINE ( ;
      ::Super:Def( nIndex, cName, nHandle, nParent, cType, cVar ), ;
      ::Set(), hmg_SetWindowObject( ::nHandle, Self ), ;
      Self )

   ACCESS Caption INLINE ::oWin:cName + "." + ::cName
   ACCESS Get INLINE ::oGetBox

   ACCESS VarGet INLINE _GetValue( , , ::nIndex )
   ASSIGN VarPut( xVal ) INLINE ( _SetValue( , , xVal, ::nIndex ), ;
      _GetValue( , , ::nIndex ) )

   METHOD SetKeyEvent( nKey, bKey, lCtrl, lShift, lAlt ) INLINE ::Get:SetKeyEvent( nKey, bKey, lCtrl, lShift, lAlt )
   METHOD SetDoubleClick( bBlock )                     INLINE ::Get:SetKeyEvent( , bBlock )

   METHOD Destroy() INLINE ::oGetBox := ::Super:Destroy()

ENDCLASS

///////////////////////////////////////////////////////////////////////////////
CLASS TStbData INHERIT TCnlData
///////////////////////////////////////////////////////////////////////////////

   EXPORTED:
   METHOD New( oWnd ) INLINE ( ::Super:New( oWnd ), ::oWin:oStatusBar := iif( Empty( ::oWin:oStatusBar ), ;
      Self, ::oWin:oStatusBar ), Self ) CONSTRUCTOR

   METHOD Def( nIndex, cName, nHandle, nParent, cType, cVar ) INLINE ( ;
      ::Super:Def( nIndex, cName, nHandle, nParent, cType, cVar ), ;
      ::Set(), hmg_SetWindowObject( ::nHandle, Self ), ;
      Self )

   METHOD Get ( nItem ) INLINE _GetItem( ::cName, ::oWin:cName, hb_defaultValue( nItem, 1 ) )
   METHOD Say ( cText, nItem ) INLINE _SetItem( ::cName, ::oWin:cName, hb_defaultValue( nItem, 1 ), ;
      hb_defaultValue( cText, '' ) )

   METHOD Icon ( cIcon, nItem ) INLINE SetStatusItemIcon( ::nHandle, hb_defaultValue( nItem, 1 ), cIcon )

   METHOD Width ( nItem, nWidth ) INLINE iif( HB_ISNUMERIC( nWidth ) .AND. nWidth > 0, ;
      _SetStatusWidth ( ::oWin:cName, hb_defaultValue( nItem, 1 ), nWidth ), ;
      _GetStatusItemWidth( ::oWin:nHandle, nItem ) )

   METHOD Action( nItem, bBlock ) INLINE _SetStatusItemAction( hb_defaultValue( nItem, 1 ), bBlock, ;
      ::oWin:nHandle )

ENDCLASS

///////////////////////////////////////////////////////////////////////////////
CLASS TTsbData INHERIT TCnlData
///////////////////////////////////////////////////////////////////////////////

   PROTECTED:
   VAR oTBrowse AS OBJECT

   EXPORTED:
   METHOD New( oWnd, oTsb ) INLINE ( ::Super:New( oWnd ), ::oTBrowse := oTsb, Self ) CONSTRUCTOR

   METHOD Def( nIndex, cName, nHandle, nParent, cType, cVar ) INLINE ( ;
      ::Super:Def( nIndex, cName, nHandle, nParent, cType, cVar ), ;
      ::Set(), hmg_SetWindowObject( ::nHandle, Self ), ;
      Self )

   ACCESS Caption INLINE ::oWin:cName + "." + ::cName
   ACCESS Tsb INLINE ::oTBrowse

   METHOD Enable () INLINE ::oTBrowse:lEnabled := .T.
   METHOD Disable() INLINE ::oTBrowse:lEnabled := .F.
   METHOD Enabled ( lEnab ) INLINE ::oTBrowse:Enabled( lEnab )
   METHOD Refresh( lPaint ) INLINE ::oTBrowse:Refresh( lPaint )
   METHOD Restore() INLINE ::oTBrowse:Show()
   METHOD Show() INLINE ::oTBrowse:Show()
   METHOD Hide() INLINE ::oTBrowse:Hide()
   METHOD SetFocus() INLINE ::oTBrowse:SetFocus()

   METHOD OnEvent( nMsg, wParam, lParam ) INLINE ::Tsb:HandleEvent( nMsg, wParam, lParam )
   METHOD Destroy() INLINE ::oTBrowse := ::Super:Destroy()

ENDCLASS

///////////////////////////////////////////////////////////////////////////////
CLASS TWmEData
///////////////////////////////////////////////////////////////////////////////

   PROTECTED:
   VAR oObj AS OBJECT
   VAR aMsg INIT hb_Hash()
   VAR lMsg INIT .F.

   EXPORTED:
   METHOD New( o ) INLINE ( ::oObj := o, Self ) CONSTRUCTOR

   ACCESS IsEvent INLINE ::lMsg
   METHOD Set( nMsg, Block ) INLINE ( hb_HSet ( ::aMsg, nMsg, Block ), ::lMsg := Len( ::aMsg ) > 0 )
   METHOD Get( nMsg, Def ) INLINE hb_HGetDef( ::aMsg, nMsg, Def )
   METHOD Del( nMsg ) INLINE ( hb_HDel ( ::aMsg, nMsg ), ::lMsg := Len( ::aMsg ) > 0 )

   _METHOD Do( nMsg, wParam, lParam )
   _METHOD Destroy()

ENDCLASS
///////////////////////////////////////////////////////////////////////////////

METHOD Do( nMsg, wParam, lParam ) CLASS TWmEData

   LOCAL o, r, b := ::Get( nMsg )

   IF HB_ISBLOCK( b )
      o := ::oObj
      IF o:IsWindow
         r := Do_WindowEventProcedure ( b, o:Index, o, nMsg, wParam, lParam ) // {|ow,nm,wp,lp| ... }
      ELSE
         r := Do_ControlEventProcedure( b, o:Index, o, nMsg, wParam, lParam ) // {|oc,nm,wp,lp| ... }
      ENDIF
   ENDIF

RETURN iif( Empty( r ), 0, 1 )

METHOD Destroy() CLASS TWmEData

   LOCAL i, k

   IF HB_ISHASH( ::aMsg )
      FOR i := 1 TO Len( ::aMsg )
         k := hb_HKeyAt( ::aMsg, i )
         hb_HSet( ::aMsg, k, Nil )
         hb_HDel( ::aMsg, k )
      NEXT
   ENDIF

   ::oObj := ::aMsg := NIL

RETURN NIL

///////////////////////////////////////////////////////////////////////////////
CLASS TKeyData
///////////////////////////////////////////////////////////////////////////////

   PROTECTED:
   VAR oObj AS OBJECT
   VAR aKey INIT hb_Hash()
   VAR lKey INIT .F.

   EXPORTED:
   VAR Cargo

   METHOD New() INLINE ( Self ) CONSTRUCTOR

   METHOD Def( o ) INLINE ( ::Obj := o, Self )

   METHOD Set( Key, Block ) INLINE ( iif( HB_ISHASH( Key ), ::aKey := Key, hb_HSet ( ::aKey, Key, Block ) ), ;
          ::lKey := ( ::Len > 0 ) )
   METHOD Get( Key, Def ) INLINE hb_HGetDef( ::aKey, Key, Def )
   METHOD Del( Key ) INLINE ( iif( ::Len > 0, hb_HDel ( ::aKey, Key ), ), ::lKey := Len( ::aKey ) > 0 )
   METHOD Pos( Key ) INLINE hb_HPos( ::aKey, Key )

   METHOD Do ( Key, p1, p2, p3 ) BLOCK {| Self, Key, p1, p2, p3, b | b := ::Get( Key ), ;
      iif( HB_ISBLOCK( b ), Eval( b, ::oObj, Key, p1, p2, p3 ), Nil ) }

   ACCESS Obj INLINE ::oObj
   ASSIGN Obj( o ) INLINE ::oObj := iif( HB_ISOBJECT( o ), o, Self )
   ACCESS Len INLINE Len( ::aKey )
   ACCESS IsEvent INLINE ::lKey
   ASSIGN KeyUpper( lUpper ) INLINE hb_HCaseMatch( ::aKey, ! Empty( lUpper ) )
   METHOD ISBLOCK( Key ) INLINE HB_ISBLOCK( ::Get( Key ) )
   METHOD Json( cJson )  INLINE iif( HB_ISCHAR( cJson ), ( cJson := SubStr( cJson, At( "{", cJson ) ), ;
                                                           cJson := Left( cJson, RAt( "}", cJson ) ), ;
                                                           ::aKey := hb_jsonDecode( cJson ), Self ), ;
                                                           hb_jsonEncode( ::aKey, !Empty( cJson ) ) )
   METHOD Keys()       INLINE hb_HKeys( ::aKey )
   METHOD Values()       INLINE hb_HValues( ::aKey )
   METHOD CloneHash()    INLINE hb_HClone( ::aKey )
   METHOD Clone()    INLINE __objClone( Self )
#ifndef __XHARBOUR__
   METHOD Sort()       INLINE ::aKey := hb_HSort( ::aKey )
#endif
   METHOD Fill( xVal )  INLINE hb_HFill( ::aKey, xVal )

   _METHOD GetAll( lAll )
   _METHOD Eval( Block )
   _METHOD Sum( Key, xSum )
   _METHOD Destroy()

   ERROR HANDLER ControlAssign

ENDCLASS
///////////////////////////////////////////////////////////////////////////////

METHOD GetAll( lAll ) CLASS TKeyData

   LOCAL aRet := {}

   IF HB_ISLOGICAL( lAll ) .AND. lAll
      ::Eval( {| val | AAdd( aRet, val ) } )
   ELSE
      ::Eval( {| val, Key | AAdd( aRet, { Key, val } ) } )
   ENDIF

RETURN aRet

METHOD Eval( Block ) CLASS TKeyData

   LOCAL i, b := HB_ISBLOCK( Block )
   LOCAL l := HB_ISLOGICAL( Block ) .AND. Block
   LOCAL a := iif( b, NIL, Array( 0 ) )

   FOR i := 1 TO ::Len
      IF b ; Eval( Block, hb_HValueAt( ::aKey, i ), hb_HKeyAt( ::aKey, i ), i )
      ELSEIF l ; AAdd( a, hb_HValueAt( ::aKey, i ) )
      ELSE ; AAdd( a, { hb_HValueAt( ::aKey, i ), hb_HKeyAt( ::aKey, i ), i } )
      ENDIF
   NEXT

RETURN a

METHOD Sum( Key, xSum ) CLASS TKeyData

   LOCAL Sum := ::Get( Key, 0 )

   IF HB_ISNUMERIC( xSum )
      IF HB_ISNUMERIC( sum ) ; Sum += xSum
      ELSE ; Sum := xSum
      ENDIF
      ::Set( Key, sum )
   ELSEIF HB_ISARRAY( xSum )
      IF HB_ISARRAY( sum ) .AND. Len( sum ) == Len( xSum )
         AEval( xSum, {| s, i | Sum[ i ] := iif( HB_ISNUMERIC( s ), Sum[ i ] + s, s ) } )
      ELSE
         Sum := xSum
      ENDIF
      ::Set( Key, sum )
   ENDIF

RETURN NIL

METHOD Destroy() CLASS TKeyData

   LOCAL i, k, o

   IF HB_ISHASH( ::aKey )
      FOR i := 1 TO Len( ::aKey )
         k := hb_HKeyAt( ::aKey, i )
         hb_HSet( ::aKey, k, Nil )
         hb_HDel( ::aKey, k )
      NEXT
   ENDIF

   IF HB_ISOBJECT( ::Cargo ) .AND. ::Cargo:ClassName == ::ClassName
      o := ::Cargo
      IF HB_ISHASH( o:aKey )
         FOR i := 1 TO Len( o:aKey )
            k := hb_HKeyAt( o:aKey, i )
            hb_HSet( o:aKey, k, Nil )
            hb_HDel( o:aKey, k )
         NEXT
      ENDIF
   ENDIF

   ::oObj := ::aKey := ::Cargo := NIL

RETURN NIL

METHOD ControlAssign( xValue ) CLASS TKeyData

   LOCAL cMessage, uRet, lError

   cMessage := __GetMessage()
   lError := .T.

   IF PCount() == 0
      uRet := ::Get( cMessage )
      lError := .F.
   ELSEIF PCount() == 1
      ::Set( SubStr( cMessage, 2 ), xValue )
      uRet := ::Get( cMessage )
      lError := .F.
   ENDIF

   IF lError
      uRet := NIL
      ::MsgNotFound( cMessage )
   ENDIF

RETURN uRet

///////////////////////////////////////////////////////////////////////////////
CLASS TThrData
///////////////////////////////////////////////////////////////////////////////

   PROTECTED:
   VAR oObj AS OBJECT
   VAR aKey INIT hb_Hash()
   VAR lMT  AS LOGICAL INIT hb_mtvm()
   VAR lUpp AS LOGICAL INIT .T.

   SYNC METHOD SGD( n, k, v )

   EXPORTED:
   VAR Cargo

   METHOD New( lUpp ) INLINE ( ::lUpp := iif( lUpp == Nil, ::lUpp, !Empty( lUpp ) ), Self ) CONSTRUCTOR

   METHOD Def( o, lVmMt ) INLINE ( ::Obj := o, ::MT := lVmMt, Self )

   METHOD Set( Key, Val ) INLINE iif( ::lMT, ::SGD( 1, Key, Val ), hb_HSet ( ::aKey, ::Upp(Key), Val ) )
   METHOD Get( Key, Def ) INLINE iif( ::lMT, ::SGD( 2, Key, Def ), hb_HGetDef( ::aKey, ::Upp(Key), Def ) )
   METHOD Del( Key )    INLINE iif( ::lMT, ::SGD( 3, Key ), ;
      iif( hb_HHasKey( ::aKey, ::Upp(Key) ), hb_HDel ( ::aKey, ::Upp(Key) ), Nil ) )

   METHOD Do ( Key, p1, p2, p3 ) BLOCK {| Self, Key, p1, p2, p3, b | b := ::Get( Key ), ;
      iif( HB_ISBLOCK( b ), Eval( b, ::oObj, Key, p1, p2, p3 ), Nil ) }
   ACCESS MT INLINE ::lMT
   ASSIGN MT( lVmMt ) INLINE ::lMT := iif( HB_ISLOGICAL( lVmMt ), lVmMt, ::lVmMt )
   ACCESS Obj INLINE ::oObj
   ASSIGN Obj( o ) INLINE ::oObj := iif( HB_ISOBJECT( o ), o, Self )
   METHOD ISBLOCK( Key ) INLINE HB_ISBLOCK( ::Get( Key ) )
   METHOD Upp( Key ) INLINE iif( HB_ISCHAR( Key ) .AND. ::lUpp, Upper( Key ), Key )
   METHOD Pos( Key ) INLINE  iif( ::lMT, ::SGD(0, Key), hb_HPos( ::aKey, ::Upp( Key ) ) )
   METHOD Len()         INLINE iif( ::lMT, ::SGD(5), Len( ::aKey ) )
   METHOD Keys()       INLINE iif( ::lMT, ::SGD(6), hb_HKeys( ::aKey ) )
   METHOD Values()      INLINE iif( ::lMT, ::SGD(7), hb_HValues( ::aKey ) )
   METHOD CloneHash()   INLINE iif( ::lMT, ::SGD(8), hb_HClone( ::aKey ) )
#ifndef __XHARBOUR__
   METHOD Sort()       INLINE iif( ::lMT, ::SGD(9), ::aKey := hb_HSort( ::aKey ) )
#endif
   METHOD Json( cJson ) INLINE ::SGD(10, cJson)

   _METHOD GetAll( lAll )
   _METHOD Eval( Block )
   _METHOD Sum( Key, xSum )

   ERROR HANDLER ControlAssign

ENDCLASS
///////////////////////////////////////////////////////////////////////////////

METHOD SGD( n, k, v ) CLASS TThrData
   LOCAL x, y

   SWITCH n
   CASE 0
      RETURN hb_HPos( ::aKey, ::Upp( k ) )
   CASE 1
      IF HB_ISHASH( k )
         IF HB_ISLOGICAL( v ) .AND. v
            FOR EACH x, y IN hb_HKeys( k ), hb_HValues( k )
               hb_HSet( ::aKey, ::Upp( x ), y )
            NEXT
         ELSE
            ::aKey := k
         ENDIF
      ELSE
         hb_HSet( ::aKey, ::Upp( k ), v )
      ENDIF
      EXIT
   CASE 2
      RETURN hb_HGetDef( ::aKey, ::Upp( k ), v )
   CASE 3
      k := ::Upp( k )
      IF hb_HHasKey( ::aKey, k )
         hb_HDel ( ::aKey, k )
      ENDIF
      EXIT
   CASE 4
      k := ::Upp( k )
      RETURN { hb_HKeyAt( ::aKey, k ), hb_HValueAt( ::aKey, k ) }
   CASE 5
      RETURN Len( ::aKey )
   CASE 6
      RETURN hb_HKeys( ::aKey )
   CASE 7
      RETURN hb_HValues( ::aKey )
   CASE 8
      RETURN hb_HClone( ::aKey )
   CASE 9
#ifndef __XHARBOUR__
      ::aKey := hb_HSort( ::aKey )
#endif
   CASE 10
      IF HB_ISCHAR( k )
         k := SubStr( k, At( "{", k ) )
         k := Left ( k, RAt( "}", k ) )
         ::aKey := hb_jsonDecode( k )
         RETURN Self
      ELSE
         RETURN hb_jsonEncode( ::aKey, !Empty( k ) )
      ENDIF
   END SWITCH

RETURN NIL

METHOD GetAll( lAll ) CLASS TThrData

   LOCAL aRet := {}

   IF HB_ISLOGICAL( lAll ) .AND. lAll
      ::Eval( {| val | AAdd( aRet, val ) } )
   ELSE
      ::Eval( {| val, Key | AAdd( aRet, { Key, val } ) } )
   ENDIF

RETURN aRet

METHOD Eval( Block ) CLASS TThrData

   LOCAL m, i, b := HB_ISBLOCK( Block )
   LOCAL l := HB_ISLOGICAL( Block ) .AND. Block
   LOCAL a := iif( b, NIL, Array( 0 ) )

   FOR i := 1 TO ::Len()
      IF ::lMT
         m := ::SGD( 4, i )
         IF b ; Eval( Block, m[ 2 ], m[ 1 ], i )
         ELSEIF l ; AAdd( a, m[ 2 ] )
         ELSE ; AAdd( a, { m[ 2 ], m[ 1 ], i } )
         ENDIF
      ELSE
         IF b ; Eval( Block, hb_HValueAt( ::aKey, i ), hb_HKeyAt( ::aKey, i ), i )
         ELSEIF l ; AAdd( a, hb_HValueAt( ::aKey, i ) )
         ELSE ; AAdd( a, { hb_HValueAt( ::aKey, i ), hb_HKeyAt( ::aKey, i ), i } )
         ENDIF
      ENDIF
   NEXT

RETURN a

METHOD Sum( Key, xSum ) CLASS TThrData

   LOCAL Sum := ::Get( Key, 0 )

   IF HB_ISNUMERIC( xSum )
      IF HB_ISNUMERIC( sum ) ; Sum += xSum
      ELSE ; Sum := xSum
      ENDIF
      ::Set( Key, sum )
   ELSEIF HB_ISARRAY( xSum )
      IF HB_ISARRAY( sum ) .AND. Len( sum ) == Len( xSum )
         AEval( xSum, {| s, i | Sum[ i ] := iif( HB_ISNUMERIC( s ), Sum[ i ] + s, s ) } )
      ELSE
         Sum := xSum
      ENDIF
      ::Set( Key, sum )
   ENDIF

RETURN NIL

METHOD ControlAssign( xValue ) CLASS TThrData

   LOCAL cMessage, uRet, lError

   cMessage := __GetMessage()
   lError := .T.

   IF PCount() == 0
      uRet := ::Get( cMessage )
      lError := .F.
   ELSEIF PCount() == 1
      ::Set( SubStr( cMessage, 2 ), xValue )
      uRet := ::Get( cMessage )
      lError := .F.
   ENDIF

   IF lError
      uRet := NIL
      ::MsgNotFound( cMessage )
   ENDIF

RETURN uRet

*-----------------------------------------------------------------------------*
FUNCTION oWndData( nIndex, cName, nHandle, nParent, cType, cVar )
*-----------------------------------------------------------------------------*
   LOCAL o

   DEFAULT nIndex := 0, ;
      cName := '', ;
      nHandle := 0, ;
      nParent := 0, ;
      cType := '', ;
      cVar := ''

   IF Empty( nIndex ) .OR. Empty( nHandle ) .OR. Empty( cName )
      RETURN o
   ENDIF

   o := TWndData():New():Def( nIndex, cName, nHandle, nParent, cType, cVar )

RETURN o

*-----------------------------------------------------------------------------*
FUNCTION oCnlData( nIndex, cName, nHandle, nParent, cType, cVar, oWin )
*-----------------------------------------------------------------------------*
   LOCAL o, ob

   DEFAULT nIndex := 0, ;
      cName := '', ;
      nHandle := 0, ;
      nParent := 0, ;
      cType := '', ;
      cVar := ''

   IF Empty( nIndex ) .OR. Empty( nHandle ) .OR. Empty( nParent ) .OR. Empty( cName ) ; RETURN o
   ENDIF

   DEFAULT oWin := hmg_GetWindowObject( nParent )

   IF HB_ISOBJECT( oWin )

      IF cType == 'TBROWSE'
         ob := _HMG_aControlIds[ nIndex ]
         o := TTsbData():New( oWin, ob ):Def( nIndex, cName, nHandle, nParent, cType, cVar )
      ELSEIF cType == 'GETBOX'
         ob := _HMG_aControlHeadClick[ nIndex ]
         o := TGetData():New( oWin, ob ):Def( nIndex, cName, nHandle, nParent, cType, cVar )
      ELSEIF cType == 'STATUSBAR'
         o := TStbData():New( oWin ):Def( nIndex, cName, nHandle, nParent, cType, cVar )
      ELSE
         o := TCnlData():New( oWin ):Def( nIndex, cName, nHandle, nParent, cType, cVar )
      ENDIF

   ENDIF

RETURN o

*-----------------------------------------------------------------------------*
FUNCTION oKeyData( Obj, Event )
*-----------------------------------------------------------------------------*
   LOCAL o

   IF HB_ISNIL ( Event ) ; o := TKeyData():New():Def( Obj )
   ELSEIF HB_ISLOGICAL( Event ) .AND. Event ; o := TWmEData():New( Obj )
#if 0
   ELSE ; o := TThrData():New():Def( Obj, hb_mtvm() )
#endif
   ENDIF

RETURN o

#endif
