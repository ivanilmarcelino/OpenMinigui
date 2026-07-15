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
   Copyright 1999-2026, https://harbour.github.io/

   "WHAT32"
   Copyright 2002 AJ Wos <andrwos@aust1.net>

   "HWGUI"
   Copyright 2001-2021 Alexander S.Kresin <alex@kresin.ru>

---------------------------------------------------------------------------*/

#include 'minigui.ch'
#include 'fileio.ch'

/* 
 * Compatibility Layer:
 * These translations ensure that Unicode-aware string functions (hb_U*) 
 * map to standard string functions in older Harbour versions or xHarbour, 
 * maintaining codebase portability.
 */
#if defined( __XHARBOUR__ ) .OR. ( __HARBOUR__ - 0 < 0x030200 )
  #xtranslate hb_ULeft( <c>, <n> ) => Left( <c>, <n> )
  #xtranslate hb_ULen( <c> ) => Len( <c> )
  #xtranslate hb_USubStr( <c>, <n> [, <e>] ) => SubStr( <c>, <n> [, <e>] )
#endif

/*-----------------------------------------------------------------------------*
 * FUNCTION _SetGetLogFile( cFile )
 * 
 * Purpose: 
 *    Manages the global state for the application's log file path.
 * Parameters:
 *    cFile (String, Optional): The full path to the log file to be set.
 * Returns:
 *    The previous log file path if setting a new one, or the current path if cFile is NIL.
 * Side Effects:
 *    Creates or updates a global variable prefixed with "_HMG_" to persist state.
 *-----------------------------------------------------------------------------*/
FUNCTION _SetGetLogFile( cFile )
   // Generate a unique global variable name based on the calling procedure
   LOCAL cVarName := "_HMG_" + SubStr( ProcName(), 8 )
   // Initialize the global variable if it doesn't exist and retrieve current value
   LOCAL cOld := _AddNewGlobal( cVarName, NIL )

   IF cFile != NIL
      _SetGetGlobal( cVarName, cFile )
      RETURN cFile
   ENDIF

RETURN cOld

/*-----------------------------------------------------------------------------*
 * FUNCTION _LogFile( lCrLf, ... )
 * 
 * Purpose: 
 *    Writes formatted debug or status information to a log file.
 * Parameters:
 *    lCrLf (Logical): If .T., prepends a NewLine to the entry.
 *    ... (Variadic): Multiple values of any type to be logged.
 * Returns:
 *    .T. if successful, .F. if logging is disabled or file access fails.
 * Reasoning:
 *    This function handles complex parameter parsing to allow developers to 
 *    pass filenames or formatting flags within the first argument.
 *-----------------------------------------------------------------------------*/
#ifndef __XHARBOUR__
FUNCTION _LogFile( lCrLf, ... )
#else
FUNCTION _LogFile( ... )
#endif
   LOCAL hFile, i, xVal, cTp
   LOCAL aParams := hb_AParams()
   LOCAL nParams := Len( aParams )
   // Default log file is "_MsgLog.txt" in the application startup folder
   LOCAL cFile := hb_defaultValue( _SetGetLogFile(), GetStartUpFolder() + hb_ps() + "_MsgLog.txt" )
#ifdef __XHARBOUR__
   LOCAL lCrLf
#endif

    // Global check to see if logging is enabled in the HMG environment
    IF ! IsErrorLogActive()
       RETURN .F.
    ENDIF

    /* 
     * Parameter Normalization:
     * Handles cases where the first parameter might be a filename string, 
     * an array containing config, or a logical flag.
     */
    IF nParams > 0
      IF HB_ISCHAR( aParams[ 1 ] )
         aParams[ 1 ] := { .T., aParams[ 1 ] }
      ENDIF
      IF HB_ISARRAY( aParams[ 1 ] )
         IF Len( aParams[ 1 ] ) > 1
            IF HB_ISLOGICAL( aParams[ 1 ][ 1 ] )     // Format: { .T./.F. , cFile }
               cTp := aParams[ 1 ][ 2 ]
               aParams[ 1 ] := aParams[ 1 ][ 1 ]
            ELSEIF HB_ISLOGICAL( aParams[ 1 ][ 2 ] ) // Format: { cFile , .T./.F. }
               cTp := aParams[ 1 ][ 1 ]
               aParams[ 1 ] := aParams[ 1 ][ 2 ]
            ELSE
               aParams[ 1 ] := .T.
            ENDIF
            IF !Empty( cTp )
               // Ensure the path is absolute
               IF !hb_ps() $ cTp
                  cTp := GetStartUpFolder() + hb_ps() + cTp
               ENDIF
               cFile := cTp
            ENDIF
         ELSE
            aParams[ 1 ] := .T.
         ENDIF
         cTp := NIL
#ifndef __XHARBOUR__
         lCrlf := aParams[ 1 ]
#endif
      ENDIF
   ENDIF

   IF !Empty( cFile )
      // Open existing file for appending or create a new one
      hFile := iif( File( cFile ), FOpen( cFile, FO_READWRITE ), FCreate( cFile, FC_NORMAL ) )
      IF hFile == F_ERROR
         RETURN .F.
      ENDIF
      
      // Move pointer to end of file for appending
      FSeek( hFile, 0, FS_END )

      IF nParams > 1
#ifdef __XHARBOUR__
         lCrLf := aParams[ 1 ]
#endif
         IF ( lCrLf := hb_defaultValue( lCrLf, .T. ) )
            FWrite( hFile, CRLF, 2 )
         ENDIF

         // Iterate through variadic arguments and serialize them based on type
         IF nParams == 2 .AND. HB_ISNIL( aParams[ 2 ] ) .AND. lCrLf
            // Skip if only a CRLF was intended
         ELSE
            FOR i := 2 TO nParams
               xVal := aParams[ i ]
               cTp  := ValType( xVal )
               
               // Type-specific string conversion for the log output
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
               
               // Use Tab separation between logged values
               FWrite( hFile, xVal + Chr( 9 ) )
            NEXT
         ENDIF
      ELSE
         FWrite( hFile, CRLF, 2 )
      ENDIF
      FClose( hFile )
   ENDIF

RETURN .T.

/*-----------------------------------------------------------------------------*
 * FUNCTION _BeginIni( cIniFile )
 * 
 * Purpose: 
 *    Initializes an INI file for reading or writing.
 * Parameters:
 *    cIniFile (String): The name or path of the INI file.
 * Returns:
 *    0 on success, -1 on failure.
 * Side Effects:
 *    Sets the global _HMG_ActiveIniFile to track the current INI context.
 * Reasoning:
 *    Handles UTF-8 encoding requirements and ensures the file exists 
 *    before subsequent Get/Set operations.
 *-----------------------------------------------------------------------------*/
FUNCTION _BeginIni( cIniFile )
   LOCAL hFile

   // If no path is provided, default to the application's base directory
   IF At( "\", cIniFile ) == 0
      cIniFile := hb_DirBase() + cIniFile
   ENDIF

   // Special handling for UTF8 environments to ensure proper Byte Order Mark (BOM)
   IF Set( _SET_CODEPAGE ) == "UTF8"
      hFile := iif( File( cIniFile ), FOpen( cIniFile, FO_READ + FO_SHARED ), HMG_CreateFile_UTF16LE_BOM( cIniFile ) )
      IF hFile == F_ERROR
         MsgInfo( "Error opening a file INI. DOS ERROR: " + hb_ntos( FError() ) )
         Return -1
      ELSE
         _HMG_ActiveIniFile := cIniFile
      ENDIF
      FClose( hFile )
   ELSE
      // Standard ANSI/OEM file handling
#if defined( __XHARBOUR__ ) .OR. ( __HARBOUR__ - 0 < 0x030200 )
      hFile := iif( File( cIniFile ), FOpen( cIniFile, FO_READ + FO_SHARED ), FCreate( cIniFile ) )
      IF hFile == F_ERROR
#else
      // Use Harbour's Virtual File System (VFS) for better compatibility in newer versions
      hFile := hb_vfOpen( cIniFile, iif( hb_vfExists( cIniFile ), FO_READ + FO_SHARED, FO_CREAT + FO_READWRITE ) )
      IF hFile == NIL
#endif
         MsgInfo( "Error opening a file INI. DOS ERROR: " + hb_ntos( FError() ) )
         Return -1
      ELSE
         _HMG_ActiveIniFile := cIniFile
      ENDIF
#if defined( __XHARBOUR__ ) .OR. ( __HARBOUR__ - 0 < 0x030200 )
      FClose( hFile )
#else
      hb_vfClose( hFile )
#endif
   ENDIF

RETURN 0

/*-----------------------------------------------------------------------------*
 * FUNCTION _GetIni( cSection, cEntry, cDefault, uVar )
 * 
 * Purpose: 
 *    Retrieves a value from the currently active INI file.
 * Parameters:
 *    cSection (String): The [Section] name.
 *    cEntry   (String): The Key name within the section.
 *    cDefault (Mixed):  The default value if the key is not found.
 *    uVar     (Mixed):  Reference variable used to determine the expected return type.
 * Returns:
 *    The value found in the INI, cast to the type of uVar.
 *-----------------------------------------------------------------------------*/
FUNCTION _GetIni( cSection, cEntry, cDefault, uVar )
   LOCAL cVar As String

   IF !Empty( _HMG_ActiveIniFile )
      __defaultNIL( @cDefault, cVar )
      __defaultNIL( @uVar, cDefault )
      // Calls the Windows API wrapper to fetch the string value
      cVar := GetPrivateProfileString( cSection, cEntry, xChar( cDefault ), _HMG_ActiveIniFile )
   ELSE
      IF cDefault != NIL
         cVar := xChar( cDefault )
      ENDIF
   ENDIF

   // Convert the string from the INI back into the original data type (Date, Numeric, etc.)
   uVar := xValue( cVar, ValType( uVar ) )

RETURN uVar

/*-----------------------------------------------------------------------------*
 * FUNCTION _SetIni( cSection, cEntry, cValue )
 * 
 * Purpose: 
 *    Writes a value to the currently active INI file.
 * Parameters:
 *    cSection (String): The [Section] name.
 *    cEntry   (String): The Key name.
 *    cValue   (Mixed):  The value to write (will be serialized to string).
 * Returns:
 *    Logical: .T. if successful.
 *-----------------------------------------------------------------------------*/
FUNCTION _SetIni( cSection, cEntry, cValue )
   LOCAL ret As Logical

   IF !Empty( _HMG_ActiveIniFile )
      // Convert Harbour type to string before writing to the text-based INI
      ret := WritePrivateProfileString( cSection, cEntry, xChar( cValue ), _HMG_ActiveIniFile )
   ENDIF

RETURN ret

/*-----------------------------------------------------------------------------*
 * FUNCTION _DelIniEntry( cSection, cEntry )
 * 
 * Purpose: 
 *    Deletes a specific key from a section in the active INI file.
 *-----------------------------------------------------------------------------*/
FUNCTION _DelIniEntry( cSection, cEntry )
   LOCAL ret As Logical

   IF !Empty( _HMG_ActiveIniFile )
      ret := DelIniEntry( cSection, cEntry, _HMG_ActiveIniFile )
   ENDIF

RETURN ret

/*-----------------------------------------------------------------------------*
 * FUNCTION _DelIniSection( cSection )
 * 
 * Purpose: 
 *    Deletes an entire section and all its keys from the active INI file.
 *-----------------------------------------------------------------------------*/
FUNCTION _DelIniSection( cSection )
   LOCAL ret As Logical

   IF !Empty( _HMG_ActiveIniFile )
      ret := DelIniSection( cSection, _HMG_ActiveIniFile )
   ENDIF

RETURN ret

/*-----------------------------------------------------------------------------*
 * FUNCTION _EndIni()
 * 
 * Purpose: 
 *    Closes the INI session by clearing the active file reference.
 *-----------------------------------------------------------------------------*/
FUNCTION _EndIni()
   _HMG_ActiveIniFile := ''
RETURN NIL

/*-----------------------------------------------------------------------------*
 * FUNCTION GetBeginComment()
 * 
 * Purpose: 
 *    Extracts the first comment line (starting with # or ;) from the INI file.
 * Reasoning: 
 *    Standard INI APIs ignore file-level comments. This manually parses the 
 *    file to retrieve header information.
 *-----------------------------------------------------------------------------*/
FUNCTION GetBeginComment
   LOCAL aLines, nLen, i, lTest := .T., cComment := ""

   IF ! Empty( _HMG_ActiveIniFile )
      // Read file and split into lines
      aLines := hb_ATokens( StrTran( MemoRead( _HMG_ActiveIniFile ), CRLF, Chr( 10 ) ), Chr( 10 ) )

      IF ( nLen := Len( aLines ) ) > 0
         FOR i := 1 TO nLen
            aLines[ i ] := AllTrim( aLines[ i ] )
            IF lTest
               // Check for standard INI comment characters
               IF hb_ULeft( aLines[ i ], 1 ) $ "#;"
                  cComment := aLines[ i ]
                  lTest := .F.
               ELSEIF ! Empty( aLines[ i ] )
                  // Stop if we hit actual data before a comment
                  lTest := .F.
               ENDIF
            ELSE
               EXIT
            ENDIF
         NEXT i
      ENDIF
   ENDIF

RETURN SubStr( cComment, 2 )

/*-----------------------------------------------------------------------------*
 * FUNCTION GetEndComment()
 * 
 * Purpose: 
 *    Extracts the last comment line from the INI file.
 *-----------------------------------------------------------------------------*/
FUNCTION GetEndComment
   LOCAL aLines, nLen, i, lTest := .T., cComment := ""

   IF ! Empty( _HMG_ActiveIniFile )
      aLines := hb_ATokens( StrTran( MemoRead( _HMG_ActiveIniFile ), CRLF, Chr( 10 ) ), Chr( 10 ) )

      IF ( nLen := Len( aLines ) ) > 0
         // Iterate backwards from the end of the file
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

/*-----------------------------------------------------------------------------*
 * FUNCTION SetBeginComment( cComment )
 * 
 * Purpose: 
 *    Inserts or replaces a comment at the very top of the INI file.
 * Parameters:
 *    cComment (String): The text to insert as a comment.
 *-----------------------------------------------------------------------------*/
FUNCTION SetBeginComment( cComment )
   LOCAL aLines, nLen, i, lTest := .T., cMemo := ""

   hb_default( @cComment, "" )

   IF ! Empty( _HMG_ActiveIniFile )
      aLines := hb_ATokens( StrTran( MemoRead( _HMG_ActiveIniFile ), CRLF, Chr( 10 ) ), Chr( 10 ) )

      // Clean up trailing empty tokens from hb_ATokens
      IF ( nLen := Len( aLines ) ) > 0 .AND. Len( ATail( aLines ) ) == 0
         ASize( aLines, nLen - 1 )
         nLen--
      ENDIF

      IF nLen > 0
         FOR i := 1 TO nLen
            aLines[ i ] := AllTrim( aLines[ i ] )
            IF lTest
               IF hb_ULeft( aLines[ i ], 1 ) $ "#;"
                  // Replace existing comment
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
                  // Insert new comment before the first data line
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
            cMemo += aLines[ i ]
         NEXT i
         hb_MemoWrit( _HMG_ActiveIniFile, cMemo )
      ENDIF
   ENDIF

RETURN cComment

/*-----------------------------------------------------------------------------*
 * FUNCTION SetEndComment( cComment )
 * 
 * Purpose: 
 *    Inserts or replaces a comment at the very bottom of the INI file.
 *-----------------------------------------------------------------------------*/
FUNCTION SetEndComment( cComment )
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
                  // Skip trailing whitespace
               ELSEIF hb_ULeft( aLines[ i ], 1 ) $ "#;"
                  // Replace existing trailing comment
                  IF !Empty( cComment )
                     IF ! hb_ULeft( cComment, 1 ) $ "#;"
                        cComment := "#" + cComment
                     ENDIF
                     cMemo := cComment + CRLF
                  ENDIF
                  lTest := .F.
               ELSEIF Empty( cComment )
                  lTest := .F.
               ELSE
                  // Append new comment to the end
                  IF ! hb_ULeft( cComment, 1 ) $ "#;"
                     cComment := "#" + cComment
                  ENDIF
                  cMemo := CRLF + cComment + CRLF
                  cMemo := aLines[ i ] + CRLF + cMemo
                  lTest := .F.
               ENDIF
            ELSE
               cMemo := aLines[ i ] + CRLF + cMemo
            ENDIF
         NEXT i
         // Clean up leading CRLF if necessary
         IF hb_ULeft( cMemo, Len( CRLF ) ) == CRLF
            cMemo := SubStr( cMemo, Len( CRLF ) + 1 )
         ENDIF
         hb_MemoWrit( _HMG_ActiveIniFile, cMemo )
      ENDIF
   ENDIF

RETURN cComment

/*-----------------------------------------------------------------------------*
 * FUNCTION xChar( xValue )
 * 
 * Purpose: 
 *    Converts any Harbour data type into a string representation suitable 
 *    for storage in a text file (Serialization).
 * Parameters:
 *    xValue (Mixed): The value to convert.
 * Returns:
 *    String: The serialized value.
 *-----------------------------------------------------------------------------*/
FUNCTION xChar( xValue )
   LOCAL cType := ValType( xValue )
   LOCAL cValue := "", nDecimals := Set( _SET_DECIMALS )

   DO CASE
   CASE cType $  "CM"; cValue := xValue
   CASE cType == "N" ; nDecimals := iif( xValue == Int( xValue ), 0, nDecimals ) ; cValue := LTrim( Str( xValue, 20, nDecimals ) )
   CASE cType == "D" ; cValue := DToS( xValue ) // Use YYYYMMDD for unambiguous storage
   CASE cType == "L" ; cValue := iif( xValue, "T", "F" )
   CASE cType == "A" ; cValue := AToC( xValue ) // Recursive array serialization
   CASE cType $  "UE"; cValue := "NIL"
   CASE cType == "B" ; cValue := "{|| ... }"    // Codeblocks cannot be easily serialized
   CASE cType == "O" ; cValue := "{" + xValue:className + "}"
   ENDCASE

RETURN cValue

/*-----------------------------------------------------------------------------*
 * FUNCTION xValue( cValue, cType )
 * 
 * Purpose: 
 *    Converts a string back into its original Harbour data type (Deserialization).
 * Parameters:
 *    cValue (String): The string from the INI file.
 *    cType  (String): The target type code (e.g., 'N', 'D', 'L').
 * Returns:
 *    Mixed: The value cast to the requested type.
 *-----------------------------------------------------------------------------*/
FUNCTION xValue( cValue, cType )
   LOCAL xValue

   DO CASE
   CASE cType $  "CM"; xValue := cValue
   CASE cType == "D" ; xValue := SToD( cValue )
   CASE cType == "N" ; xValue := Val( cValue )
   CASE cType == "L" ; xValue := ( cValue == 'T' )
   CASE cType == "A" ; xValue := CToA( cValue )
   OTHERWISE         ; xValue := NIL
   ENDCASE

RETURN xValue

/*-----------------------------------------------------------------------------*
 * FUNCTION AToC( aArray )
 * 
 * Purpose: 
 *    Serializes an array into a structured string.
 * Reasoning: 
 *    Uses a custom format: "A" + Length + TypePrefix + ElementLength + ElementValue.
 *    This allows for nested arrays and mixed-type elements within the INI.
 *-----------------------------------------------------------------------------*/
FUNCTION AToC( aArray )
   LOCAL elem, cElement, cType, cArray := ""

   FOR EACH elem IN aArray
      cElement := xChar( elem )
      IF ( cType := ValType( elem ) ) == "A"
         cArray += cElement
      ELSE
         // Format: [Type(1)][Len(4)][Value(n)]
         cArray += hb_ULeft( cType, 1 ) + Str( hb_ULen( cElement ), 4 ) + cElement
      ENDIF
   NEXT

RETURN( "A" + Str( hb_ULen( cArray ), 4 ) + cArray )

/*-----------------------------------------------------------------------------*
 * FUNCTION CToA( cArray )
 * 
 * Purpose: 
 *    Deserializes a structured string back into a Harbour array.
 *-----------------------------------------------------------------------------*/
FUNCTION CToA( cArray )
   LOCAL cType, nLen, aArray := {}

   // Strip the 'A' prefix and the total length header
   cArray := hb_USubStr( cArray, 6 )
   
   WHILE hb_ULen( cArray ) > 0
      // Extract element length from the 4-digit header
      nLen := Val( hb_USubStr( cArray, 2, 4 ) )
      IF ( cType := hb_ULeft( cArray, 1 ) ) == "A"
         // Recursively handle nested arrays
         AAdd( aArray, CToA( hb_USubStr( cArray, 1, nLen + 5 ) ) )
      ELSE
         AAdd( aArray, xValue( hb_USubStr( cArray, 6, nLen ), cType ) )
      ENDIF
      // Move to the next element in the string
      cArray := hb_USubStr( cArray, 6 + nLen )
   END

RETURN aArray

/*-----------------------------------------------------------------------------*
 * FUNCTION _GetSectionNames( cIniFile )
 * 
 * Purpose: 
 *    Retrieves a list of all section names in an INI file.
 * Returns: 
 *    A 1D array of strings.
 *-----------------------------------------------------------------------------*/
FUNCTION _GetSectionNames( cIniFile )
   LOCAL aSectionList := {}, aLista

   IF At( "\", cIniFile ) == 0
      cIniFile := hb_DirBase() + cIniFile
   ENDIF

   IF File( cIniFile )
      aLista := _GetPrivateProfileSectionNames( cIniFile )
      IF ! Empty( aLista )
         // Filter out empty entries
         AEval( aLista, {|cVal| iif( Empty( cVal ), , AAdd( aSectionList, cVal ) ) } )
      ENDIF
   ELSE
      MsgStop( "Cant open " + cIniFile, "Error" )
   ENDIF

RETURN aSectionList

/*-----------------------------------------------------------------------------*
 * FUNCTION _GetSection( cSection, cIniFile )
 * 
 * Purpose: 
 *    Retrieves all key-value pairs within a specific section.
 * Returns: 
 *    A 2D array: { {key1, val1}, {key2, val2}, ... }
 *-----------------------------------------------------------------------------*/
FUNCTION _GetSection( cSection, cIniFile )
   LOCAL aKeyValueList := {}, aLista, i, n

   IF At( "\", cIniFile ) == 0
      cIniFile := hb_DirBase() + cIniFile
   ENDIF

   IF File( cIniFile )
      aLista := _GetPrivateProfileSection( cSection, cIniFile )
      IF ! Empty( aLista )
         FOR i := 1 TO Len( aLista )
            // Split the "Key=Value" string returned by the API
            IF ( n := At( "=", aLista[ i ] ) ) > 0
               AAdd( aKeyValueList, { Left( aLista[ i ], n - 1 ), SubStr( aLista[ i ], n + 1 ) } )
            ENDIF
         NEXT i
      ENDIF
   ELSE
      MsgStop( "Cant open " + cIniFile, "Error" )
   ENDIF

RETURN aKeyValueList
