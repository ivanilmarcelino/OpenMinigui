/*----------------------------------------------------------------------------
MINIGUI - Harbour Win32 GUI library source code

Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
http://harbourminigui.googlepages.com/

INI Files support procedures
(c) 2003 Grigory Filatov
(c) 2003 Janusz Pora

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along with
   this software; see the file COPYING. If not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA (or
   visit the web site http://www.gnu.org/).

   As a special exception, you have permission for additional uses of the text
   contained in this release of Harbour Minigui.

   The exception is that, if you link the Harbour Minigui library with other
   files to produce an executable, this does not by itself cause the resulting
   executable to be covered by the GNU General Public License.
   Your use of that executable is in no way restricted on account of linking the
   Harbour-Minigui library code into it.

   Parts of this project are based upon:

   "Harbour GUI framework for Win32"
   Copyright 2001 Alexander S.Kresin <alex@kresin.ru>
   Copyright 2001 Antonio Linares <alinares@fivetech.com>
   www - https://harbour.github.io/

   "Harbour Project"
   Copyright 1999-2025, https://harbour.github.io/

   "WHAT32"
   Copyright 2002 AJ Wos <andrwos@aust1.net>

   "HWGUI"
   Copyright 2001-2021 Alexander S.Kresin <alex@kresin.ru>

 ---------------------------------------------------------------------------*/

#include 'minigui.ch'
#include 'fileio.ch'

#if defined( __XHARBOUR__ ) .OR. ( __HARBOUR__ - 0 < 0x030200 )
  #xtranslate hb_ULeft( <c>, <n> ) => Left( <c>, <n> )
  #xtranslate hb_ULen( <c> ) => Len( <c> )
  #xtranslate hb_USubStr( <c>, <n> [, <e>] ) => SubStr( <c>, <n> [, <e>] )
#endif

*-----------------------------------------------------------------------------*
FUNCTION _SetGetLogFile( cFile )
*-----------------------------------------------------------------------------*
   LOCAL cVarName := "_HMG_" + SubStr( ProcName(), 8 )
   LOCAL cOld := _AddNewGlobal( cVarName, NIL )

   IF cFile != NIL
      _SetGetGlobal( cVarName, cFile )
      RETURN cFile
   ENDIF

RETURN cOld

*-----------------------------------------------------------------------------*
#ifndef __XHARBOUR__
FUNCTION _LogFile( lCrLf, ... )
#else
FUNCTION _LogFile( ... )
#endif
*-----------------------------------------------------------------------------*
   LOCAL hFile, i, xVal, cTp
   LOCAL aParams := hb_AParams()
   LOCAL nParams := Len( aParams )
   LOCAL cFile := hb_defaultValue( _SetGetLogFile(), GetStartUpFolder() + hb_ps() + "_MsgLog.txt" )
#ifdef __XHARBOUR__
   LOCAL lCrLf
#endif
    IF ! IsErrorLogActive()
       RETURN .F.
    ENDIF
    IF nParams > 0
      IF HB_ISCHAR( aParams[ 1 ] )
         aParams[ 1 ] := { .T., aParams[ 1 ] }
      ENDIF
      IF HB_ISARRAY( aParams[ 1 ] )
         IF Len( aParams[ 1 ] ) > 1
            IF HB_ISLOGICAL( aParams[ 1 ][ 1 ] )     // { .T.\.F. , cFile }
               cTp := aParams[ 1 ][ 2 ]
               aParams[ 1 ] := aParams[ 1 ][ 1 ]
            ELSEIF HB_ISLOGICAL( aParams[ 1 ][ 2 ] ) // { cFile , .T.\.F. }
               cTp := aParams[ 1 ][ 1 ]
               aParams[ 1 ] := aParams[ 1 ][ 2 ]
            ELSE                // errors
               aParams[ 1 ] := .T.
            ENDIF
            IF !Empty( cTp )
               IF !hb_ps() $ cTp
                  cTp := GetStartUpFolder() + hb_ps() + cTp
               ENDIF
               cFile := cTp
            ENDIF
         ELSE                   // errors
            aParams[ 1 ] := .T.
         ENDIF
         cTp := NIL
#ifndef __XHARBOUR__
         lCrlf := aParams[ 1 ]
#endif
      ENDIF
   ENDIF
   IF !Empty( cFile )
      hFile := iif( File( cFile ), FOpen( cFile, FO_READWRITE ), FCreate( cFile, FC_NORMAL ) )
      IF hFile == F_ERROR
         RETURN .F.
      ENDIF
      FSeek( hFile, 0, FS_END )
      IF nParams > 1
#ifdef __XHARBOUR__
         lCrLf := aParams[ 1 ]
#endif
         IF ( lCrLf := hb_defaultValue( lCrLf, .T. ) )
            FWrite( hFile, CRLF, 2 )
         ENDIF
         IF nParams == 2 .AND. HB_ISNIL( aParams[ 2 ] ) .AND. lCrLf
         ELSE
            FOR i := 2 TO nParams
               xVal := aParams[ i ]
               cTp  := ValType( xVal )
               IF     cTp == 'C' ; xVal := iif( Empty( xVal ), "'" + "'", Trim( xVal ) )
               ELSEIF cTp == 'N' ; xVal := hb_ntos( xVal )
               ELSEIF cTp == 'L' ; xVal := iif( xVal, ".T.", ".F." )
#ifdef __XHARBOUR__
               ELSEIF cTp == 'D' ; xVal := DToC( xVal )
#else
               ELSEIF cTp == 'D' ; xVal := hb_DToC( xVal, 'DD.MM.YYYY' )
#endif
               ELSEIF cTp == 'A' ; xVal := "ARRAY["  + hb_ntos( Len( xVal ) ) + "]"
               ELSEIF cTp == 'H' ; xVal :=  "HASH["  + hb_ntos( Len( xVal ) ) + "]"
               ELSEIF cTp == 'B' ; xVal := "'" + "B" + "'"
               ELSEIF cTp == 'T' ; xVal := hb_TSToStr( xVal, .T. )
               ELSEIF cTp == 'U' ; xVal := 'NIL'
               ELSE              ; xVal := "'" + cTp + "'"
               ENDIF
               FWrite( hFile, xVal + Chr( 9 ) )
            NEXT
         ENDIF
      ELSE
         FWrite( hFile, CRLF, 2 )
      ENDIF
      FClose( hFile )
   ENDIF

RETURN .T.

*-----------------------------------------------------------------------------*
FUNCTION _BeginIni( cIniFile )
*-----------------------------------------------------------------------------*
   LOCAL hFile

   IF At( "\", cIniFile ) == 0
      cIniFile := hb_DirBase() + cIniFile
   ENDIF

   IF Set( _SET_CODEPAGE ) == "UTF8"

      hFile := iif( File( cIniFile ), FOpen( cIniFile, FO_READ + FO_SHARED ), HMG_CreateFile_UTF16LE_BOM( cIniFile ) )
      IF hFile == F_ERROR
         MsgInfo( "Error opening a file INI. DOS ERROR: " + hb_ntos( FError() ) )
         Return( -1 )
      ELSE
         _HMG_ActiveIniFile := cIniFile
      ENDIF

      FClose( hFile )

   ELSE

#if defined( __XHARBOUR__ ) .OR. ( __HARBOUR__ - 0 < 0x030200 )
      hFile := iif( File( cIniFile ), FOpen( cIniFile, FO_READ + FO_SHARED ), FCreate( cIniFile ) )
      IF hFile == F_ERROR
#else
      hFile := hb_vfOpen( cIniFile, iif( hb_vfExists( cIniFile ), FO_READ + FO_SHARED, FO_CREAT + FO_READWRITE ) )
      IF hFile == NIL
#endif
         MsgInfo( "Error opening a file INI. DOS ERROR: " + hb_ntos( FError() ) )
         Return( -1 )
      ELSE
         _HMG_ActiveIniFile := cIniFile
      ENDIF
#if defined( __XHARBOUR__ ) .OR. ( __HARBOUR__ - 0 < 0x030200 )
      FClose( hFile )
#else
      hb_vfClose( hFile )
#endif
   ENDIF

RETURN( 0 )

*-----------------------------------------------------------------------------*
FUNCTION _GetIni( cSection, cEntry, cDefault, uVar )
*-----------------------------------------------------------------------------*
   LOCAL cVar As String

   IF !Empty( _HMG_ActiveIniFile )
      __defaultNIL( @cDefault, cVar )
      __defaultNIL( @uVar, cDefault )
      cVar := GetPrivateProfileString( cSection, cEntry, xChar( cDefault ), _HMG_ActiveIniFile )
   ELSE
      IF cDefault != NIL
         cVar := xChar( cDefault )
      ENDIF
   ENDIF

   uVar := xValue( cVar, ValType( uVar ) )

RETURN uVar

*-----------------------------------------------------------------------------*
FUNCTION _SetIni( cSection, cEntry, cValue )
*-----------------------------------------------------------------------------*
   LOCAL ret As Logical

   IF !Empty( _HMG_ActiveIniFile )
      ret := WritePrivateProfileString( cSection, cEntry, xChar( cValue ), _HMG_ActiveIniFile )
   ENDIF

RETURN ret

*-----------------------------------------------------------------------------*
FUNCTION _DelIniEntry( cSection, cEntry )
*-----------------------------------------------------------------------------*
   LOCAL ret As Logical

   IF !Empty( _HMG_ActiveIniFile )
      ret := DelIniEntry( cSection, cEntry, _HMG_ActiveIniFile )
   ENDIF

RETURN ret

*-----------------------------------------------------------------------------*
FUNCTION _DelIniSection( cSection )
*-----------------------------------------------------------------------------*
   LOCAL ret As Logical

   IF !Empty( _HMG_ActiveIniFile )
      ret := DelIniSection( cSection, _HMG_ActiveIniFile )
   ENDIF

RETURN ret

*-----------------------------------------------------------------------------*
FUNCTION _EndIni()
*-----------------------------------------------------------------------------*
   _HMG_ActiveIniFile := ''

RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION GetBeginComment
*-----------------------------------------------------------------------------*
   LOCAL aLines, nLen, i, lTest := .T., cComment := ""

   IF ! Empty( _HMG_ActiveIniFile )
      aLines := hb_ATokens( StrTran( MemoRead( _HMG_ActiveIniFile ), CRLF, Chr( 10 ) ), Chr( 10 ) )

      IF ( nLen := Len( aLines ) ) > 0
         FOR i := 1 TO nLen
            aLines[ i ] := AllTrim( aLines[ i ] )
            IF lTest
               IF hb_ULeft( aLines[ i ], 1 ) $ "#;"
                  cComment := aLines[ i ]
                  lTest := .F.
               ELSEIF ! Empty( aLines[ i ] )
                  lTest := .F.
               ENDIF
            ELSE
               EXIT
            ENDIF
         NEXT i
      ENDIF
   ENDIF

RETURN SubStr( cComment, 2 )

*-----------------------------------------------------------------------------*
FUNCTION GetEndComment
*-----------------------------------------------------------------------------*
   LOCAL aLines, nLen, i, lTest := .T., cComment := ""

   IF ! Empty( _HMG_ActiveIniFile )
      aLines := hb_ATokens( StrTran( MemoRead( _HMG_ActiveIniFile ), CRLF, Chr( 10 ) ), Chr( 10 ) )

      IF ( nLen := Len( aLines ) ) > 0
         FOR i := nLen TO 1 STEP -1
            aLines[ i ] := AllTrim( aLines[ i ] )
            IF lTest
               IF hb_ULeft( aLines[ i ], 1 ) $ "#;"
                  cComment := aLines[ i ]
                  lTest := .F.
               ELSEIF ! Empty( aLines[ i ] )
                  lTest := .F.
               ENDIF
            ELSE
               EXIT
            ENDIF
         NEXT i
      ENDIF
   ENDIF

RETURN SubStr( cComment, 2 )

*-----------------------------------------------------------------------------*
FUNCTION SetBeginComment( cComment )
*-----------------------------------------------------------------------------*
   LOCAL aLines, nLen, i, lTest := .T., cMemo := ""

   hb_default( @cComment, "" )

   IF ! Empty( _HMG_ActiveIniFile )
      aLines := hb_ATokens( StrTran( MemoRead( _HMG_ActiveIniFile ), CRLF, Chr( 10 ) ), Chr( 10 ) )

      IF ( nLen := Len( aLines ) ) > 0 .AND. Len( ATail( aLines ) ) == 0
         ASize( aLines, nLen - 1 )
         nLen--
      ENDIF
      IF nLen > 0
         FOR i := 1 TO nLen
            aLines[ i ] := AllTrim( aLines[ i ] )
            IF lTest
               IF hb_ULeft( aLines[ i ], 1 ) $ "#;"
                  IF Empty( cComment )
                     aLines[ i ] := ""
                  ELSE
                     IF ! hb_ULeft( cComment := AllTrim( cComment ), 1 ) $ "#;"
                        cComment := "#" + cComment
                     ENDIF
                     aLines[ i ] := cComment + CRLF
                  ENDIF
                  lTest := .F.
               ELSEIF Empty( aLines[ i ] )
                  aLines[ i ] += CRLF
               ELSEIF Empty( cComment )
                  aLines[ i ] += CRLF
                  lTest := .F.
               ELSE
                  AAdd( aLines, NIL )
                  nLen++
                  AIns( aLines, i )
                  IF ! hb_ULeft( cComment := AllTrim( cComment ), 1 ) $ "#;"
                     cComment := "#" + cComment
                  ENDIF
                  aLines[ i ] := cComment + CRLF
                  lTest := .F.
               ENDIF
            ELSE
               aLines[ i ] += CRLF
            ENDIF
            cMemo := cMemo + aLines[ i ]
         NEXT i
         hb_MemoWrit( _HMG_ActiveIniFile, cMemo )
      ENDIF
   ENDIF

RETURN cComment

*-----------------------------------------------------------------------------*
FUNCTION SetEndComment( cComment )
*-----------------------------------------------------------------------------*
   LOCAL aLines, nLen, i, lTest := .T., cMemo := ""

   hb_default( @cComment, "" )
   cComment := AllTrim( cComment )

   IF ! Empty( _HMG_ActiveIniFile )
      aLines := hb_ATokens( StrTran( MemoRead( _HMG_ActiveIniFile ), CRLF, Chr( 10 ) ), Chr( 10 ) )
      nLen := Len( aLines )
      IF nLen > 0 .AND. hb_ULen( ATail( aLines ) ) == 0
         ASize( aLines, nLen - 1 )
         nLen--
      ENDIF
      IF nLen > 0
         FOR i := nLen TO 1 STEP -1
            aLines[ i ] := AllTrim( aLines[ i ] )
            IF lTest
               IF Empty( aLines[ i ] )
                  // Remove empty trailing lines
               ELSEIF hb_ULeft( aLines[ i ], 1 ) $ "#;"
                  IF Empty( cComment )
                     // Remove previous comment
                  ELSE
                     // Replace previous comment
                     IF ! hb_ULeft( cComment, 1 ) $ "#;"
                        cComment := "#" + cComment
                     ENDIF
                     cMemo := cComment + CRLF
                  ENDIF
                  lTest := .F.
               ELSEIF Empty( cComment )
                  // Do not add comment
                  lTest := .F.
               ELSE
                  // Add comment as the last line
                  IF ! hb_ULeft( cComment, 1 ) $ "#;"
                     cComment := "#" + cComment
                  ENDIF
                  cMemo := CRLF + cComment + CRLF
                  // Add line
                  cMemo := aLines[ i ] + CRLF + cMemo
                  lTest := .F.
               ENDIF
            ELSE
               // Add line
               cMemo := aLines[ i ] + CRLF + cMemo
            ENDIF
         NEXT i
         IF hb_ULeft( cMemo, Len( CRLF ) ) == CRLF
            cMemo := SubStr( cMemo, Len( CRLF ) + 1 )
         ENDIF
         hb_MemoWrit( _HMG_ActiveIniFile, cMemo )
      ENDIF
   ENDIF

RETURN cComment

*-----------------------------------------------------------------------------*
FUNCTION xChar( xValue )
*-----------------------------------------------------------------------------*
   LOCAL cType := ValType( xValue )
   LOCAL cValue := "", nDecimals := Set( _SET_DECIMALS )

   DO CASE
   CASE cType $  "CM"; cValue := xValue
   CASE cType == "N" ; nDecimals := iif( xValue == Int( xValue ), 0, nDecimals ) ; cValue := LTrim( Str( xValue, 20, nDecimals ) )
   CASE cType == "D" ; cValue := DToS( xValue )
   CASE cType == "L" ; cValue := iif( xValue, "T", "F" )
   CASE cType == "A" ; cValue := AToC( xValue )
   CASE cType $  "UE"; cValue := "NIL"
   CASE cType == "B" ; cValue := "{|| ... }"
   CASE cType == "O" ; cValue := "{" + xValue:className + "}"
   ENDCASE

RETURN cValue

*-----------------------------------------------------------------------------*
FUNCTION xValue( cValue, cType )
*-----------------------------------------------------------------------------*
   LOCAL xValue

   DO CASE
   CASE cType $  "CM"; xValue := cValue
   CASE cType == "D" ; xValue := SToD( cValue )
   CASE cType == "N" ; xValue := Val( cValue )
   CASE cType == "L" ; xValue := ( cValue == 'T' )
   CASE cType == "A" ; xValue := CToA( cValue )
   OTHERWISE         ; xValue := NIL             // Nil, Block, Object
   ENDCASE

RETURN xValue

*-----------------------------------------------------------------------------*
FUNCTION AToC( aArray )
*-----------------------------------------------------------------------------*
   LOCAL elem, cElement, cType, cArray := ""

   FOR EACH elem IN aArray
      cElement := xChar( elem )
      IF ( cType := ValType( elem ) ) == "A"
         cArray += cElement
      ELSE
         cArray += hb_ULeft( cType, 1 ) + Str( hb_ULen( cElement ), 4 ) + cElement
      ENDIF
   NEXT

RETURN( "A" + Str( hb_ULen( cArray ), 4 ) + cArray )

*-----------------------------------------------------------------------------*
FUNCTION CToA( cArray )
*-----------------------------------------------------------------------------*
   LOCAL cType, nLen, aArray := {}

   cArray := hb_USubStr( cArray, 6 )    // strip off array and length
   WHILE hb_ULen( cArray ) > 0
      nLen := Val( hb_USubStr( cArray, 2, 4 ) )
      IF ( cType := hb_ULeft( cArray, 1 ) ) == "A"
         AAdd( aArray, CToA( hb_USubStr( cArray, 1, nLen + 5 ) ) )
      ELSE
         AAdd( aArray, xValue( hb_USubStr( cArray, 6, nLen ), cType ) )
      ENDIF
      cArray := hb_USubStr( cArray, 6 + nLen )
   END

RETURN aArray

// JK HMG 1.0 experimental build 6
*-----------------------------------------------------------------------------*
FUNCTION _GetSectionNames( cIniFile )
*-----------------------------------------------------------------------------*
   // return 1-dimensional array with section list in cIniFile
   // or empty array if no sections are present
   LOCAL aSectionList := {}, aLista

   IF At( "\", cIniFile ) == 0
      cIniFile := hb_DirBase() + cIniFile
   ENDIF

   IF File( cIniFile )
      aLista := _GetPrivateProfileSectionNames( cIniFile )
      IF ! Empty( aLista )
         AEval( aLista, {|cVal| iif( Empty( cVal ), , AAdd( aSectionList, cVal ) ) } )
      ENDIF
   ELSE
      MsgStop( "Can`t open " + cIniFile, "Error" )
   ENDIF

RETURN aSectionList

*-----------------------------------------------------------------------------*
FUNCTION _GetSection( cSection, cIniFile )
*-----------------------------------------------------------------------------*
   // return 2-dimensional array with {key,value} pairs from section cSection in cIniFile
   LOCAL aKeyValueList := {}, aLista, i, n

   IF At( "\", cIniFile ) == 0
      cIniFile := hb_DirBase() + cIniFile
   ENDIF

   IF File( cIniFile )
      aLista := _GetPrivateProfileSection( cSection, cIniFile )
      IF ! Empty( aLista )
         FOR i := 1 TO Len( aLista )
            IF ( n := At( "=", aLista[ i ] ) ) > 0
               AAdd( aKeyValueList, { Left( aLista[ i ], n - 1 ), SubStr( aLista[ i ], n + 1 ) } )
            ENDIF
         NEXT i
      ENDIF
   ELSE
      MsgStop( "Can`t open " + cIniFile, "Error" )
   ENDIF

RETURN aKeyValueList
