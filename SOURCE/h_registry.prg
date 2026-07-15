/*----------------------------------------------------------------------------
MINIGUI - Harbour Win32 GUI library source code

Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
http://harbourminigui.googlepages.com/

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

#include "hbclass.ch"

/*----------------------------------------------------------------------*
 * Compatibility
 *----------------------------------------------------------------------*/

#if defined( __XHARBOUR__ ) .OR. ( __HARBOUR__ - 0 < 0x030200 )
   #xtranslate hb_default( @<v>, <x> )     => ;
      iif( StrTran( ValType( <v> ), "M", "C" ) == StrTran( ValType( <x> ), "M", "C" ), Nil, <v> := <x> )
   #xtranslate hb_defaultValue( <v>, <x> ) => ;
      iif( StrTran( ValType( <v> ), "M", "C" ) == StrTran( ValType( <x> ), "M", "C" ), <v>, <x> )
#endif

/*----------------------------------------------------------------------*
 * Registry constants
 *----------------------------------------------------------------------*/

#define KEY_READ        25
#define KEY_WRITE       6
#define KEY_ALL_ACCESS  63

#define REG_SZ          1
#define REG_DWORD       4

#define ERROR_SUCCESS   0
#define KEY_WOW64_64KEY 0x0100
#define UINT32_MAX      4294967296

/*----------------------------------------------------------------------*
 * TReg32 Class
 *----------------------------------------------------------------------*/

CLASS TReg32
   EXPORTED:
      VAR cRegKey
      VAR nHandle
      VAR nDisposition
      VAR nError
      VAR lError

      METHOD New( nKey, cRegKey, lShowError )
      METHOD Create( nKey, cRegKey, lShowError )
      METHOD Get( cRegVar, uVar )
      METHOD Set( cRegVar, uVar )
      METHOD Delete( cRegVar )
      METHOD Close()
   PROTECTED:
      METHOD ShowErrorIf( nError, lShowError, cContext )
ENDCLASS

/*----------------------------------------------------------------------*
 * Helpers
 *----------------------------------------------------------------------*/

STATIC FUNCTION _RegistryAccess()
   RETURN iif( IsWow64(), hb_bitOr( KEY_ALL_ACCESS, KEY_WOW64_64KEY ), KEY_ALL_ACCESS )

/*----------------------------------------------------------------------*
 * Unsigned DWORD conversion
 *----------------------------------------------------------------------*/
STATIC FUNCTION Bin2U( cBinary )
   LOCAL nValue := Bin2L( cBinary )
   RETURN iif( nValue < 0, nValue + UINT32_MAX, nValue )

/*----------------------------------------------------------------------*
 * Registry -> Harbour conversion
 *----------------------------------------------------------------------*/
STATIC FUNCTION _ConvertValueFromReg( cType, uValue )
   SWITCH cType
   CASE "N" ; RETURN Bin2U( uValue )
   CASE "D" ; RETURN CToD( uValue )
   CASE "L" ; RETURN Upper( uValue ) == ".T."
   ENDSWITCH
   RETURN uValue

/*----------------------------------------------------------------------*
 * Harbour -> Registry conversion
 *----------------------------------------------------------------------*/
STATIC FUNCTION _ConvertValueToReg( cType, uValue )
   SWITCH cType
   CASE "D" ; RETURN DToC( uValue )
   CASE "L" ; RETURN iif( uValue, ".T.", ".F." )
   ENDSWITCH
   RETURN uValue

/*----------------------------------------------------------------------*
 * Initialize value by Harbour type
 *----------------------------------------------------------------------*/
STATIC FUNCTION _InitValueByType( cType )
   DO CASE
   CASE cType == "N" ; RETURN 0
   CASE cType == "D" ; RETURN CToD( "" )
   CASE cType == "L" ; RETURN .F.
   ENDCASE
   RETURN ""

/*----------------------------------------------------------------------*
 * Methods
 *----------------------------------------------------------------------*/

/*----------------------------------------------------------------------*
 * Open existing registry key
 *----------------------------------------------------------------------*/
METHOD New( nKey, cRegKey, lShowError ) CLASS TReg32
   LOCAL nHandle := 0, nResult

   hb_default( @cRegKey, "" )

   ::cRegKey      := ""
   ::nHandle      := 0
   ::nDisposition := 0
   ::nError       := ERROR_SUCCESS
   ::lError       := .F.

   nResult := RegOpenKeyExA( nKey, cRegKey, 0, _RegistryAccess(), @nHandle )
   IF nResult != ERROR_SUCCESS
      nResult := RegOpenKeyExA( nKey, cRegKey, 0, KEY_READ, @nHandle )
   ENDIF

   ::nError := nResult
   ::lError := ( nResult != ERROR_SUCCESS )

   IF ::lError
      ::ShowErrorIf( nResult, lShowError, "opening" )
   ELSE
      ::cRegKey := cRegKey
      ::nHandle := nHandle
   ENDIF
RETURN Self

/*----------------------------------------------------------------------*
 * Create registry key
 *----------------------------------------------------------------------*/
METHOD Create( nKey, cRegKey, lShowError ) CLASS TReg32
   LOCAL nHandle := 0, nDisposition, nResult

   hb_default( @cRegKey, "" )

   ::cRegKey      := ""
   ::nHandle      := 0
   ::nDisposition := 0
   ::nError       := ERROR_SUCCESS
   ::lError       := .F.

   nResult := RegCreateKey( nKey, cRegKey, @nHandle, @nDisposition )

   ::nError := nResult
   ::lError := ( nResult != ERROR_SUCCESS )

   IF ::lError
      ::ShowErrorIf( nResult, lShowError, "creating" )
   ELSE
      ::nError := RegOpenKeyExA( nKey, cRegKey, 0, _RegistryAccess(), @nHandle )
      ::lError := ( ::nError != ERROR_SUCCESS )

      IF ::lError
         ::ShowErrorIf( ::nError, lShowError, "opening" )
      ELSE
         ::cRegKey      := cRegKey
         ::nHandle      := nHandle
         ::nDisposition := nDisposition
      ENDIF
   ENDIF
RETURN Self

/*----------------------------------------------------------------------*
 * Show registry error
 *----------------------------------------------------------------------*/
METHOD ShowErrorIf( nError, lShowError, cContext ) CLASS TReg32
   hb_default( @lShowError, .T. )
   IF lShowError .AND. nError != ERROR_SUCCESS
      MsgStop( "Error " + cContext + " " + ::ClassName() + " object (" + hb_ntos( nError ) + ")" )
   ENDIF
RETURN NIL

/*----------------------------------------------------------------------*
 * Read registry value
 *----------------------------------------------------------------------*/
METHOD Get( cRegVar, uVar ) CLASS TReg32
   LOCAL cData := "", nType := 0, nLen := 0, cExpectedType

   IF ! ::lError
      hb_default( @cRegVar, "" )
      cExpectedType := ValType( uVar )

      ::nError := RegQueryValueExA( ::nHandle, cRegVar, 0, @nType, @cData, @nLen )
      IF ::nError == ERROR_SUCCESS
         uVar := _ConvertValueFromReg( cExpectedType, cData )
      ENDIF
   ENDIF
RETURN uVar

/*----------------------------------------------------------------------*
 * Write registry value
 *----------------------------------------------------------------------*/
METHOD Set( cRegVar, uVar ) CLASS TReg32
   LOCAL cType := ValType( uVar ), nType

   IF ! ::lError
      hb_default( @cRegVar, "" )
      nType := iif( cType == "N", REG_DWORD, REG_SZ )

      IF cType != "N"
         uVar := _ConvertValueToReg( cType, uVar )
      ENDIF

      ::nError := RegSetValueExA( ::nHandle, cRegVar, 0, nType, @uVar )
   ENDIF
RETURN NIL

/*----------------------------------------------------------------------*
 * Delete registry value
 *----------------------------------------------------------------------*/
METHOD Delete( cRegVar ) CLASS TReg32
   IF ! ::lError
      ::nError := RegDeleteValueA( ::nHandle, hb_defaultValue( cRegVar, "" ) )
   ENDIF
RETURN NIL

/*----------------------------------------------------------------------*
 * Close registry handle
 *----------------------------------------------------------------------*/
METHOD Close() CLASS TReg32
   IF HB_ISNUMERIC( ::nHandle ) .AND. ::nHandle != 0
      ::nError := RegCloseKey( ::nHandle )
      ::nHandle := 0
   ENDIF
RETURN NIL

/*----------------------------------------------------------------------*
 * High-level functions
 *----------------------------------------------------------------------*/

/*----------------------------------------------------------------------*
 * Registry object wrapper
 *----------------------------------------------------------------------*/
STATIC FUNCTION WithRegistry( nKey, cRegKey, bAction, uDefault )
   LOCAL oReg, uResult := uDefault
   oReg := TReg32():New( nKey, cRegKey, .F. )
   IF ! oReg:lError
      uResult := Eval( bAction, oReg )
   ENDIF
   oReg:Close()
RETURN uResult

/*----------------------------------------------------------------------*
 * Check whether a registry key exists
 *----------------------------------------------------------------------*/
FUNCTION IsRegistryKey( nKey, cRegKey )
RETURN WithRegistry( nKey, cRegKey, {|oReg| ! oReg:lError }, .F. )

/*----------------------------------------------------------------------*
 * Create registry key
 *----------------------------------------------------------------------*/
FUNCTION CreateRegistryKey( nKey, cRegKey )
   LOCAL oReg := TReg32():Create( nKey, cRegKey, .F. )
   LOCAL lSuccess := ! oReg:lError
   oReg:Close()
RETURN lSuccess

/*----------------------------------------------------------------------*
 * Read registry value
 *----------------------------------------------------------------------*/
FUNCTION GetRegistryValue( nKey, cRegKey, cRegVar, cType )
   hb_default( @cRegVar, "" )
   hb_default( @cType, "C" )
RETURN WithRegistry( nKey, cRegKey, {|oReg| oReg:Get( cRegVar, _InitValueByType( cType ) ) } )

/*----------------------------------------------------------------------*
 * Write registry value
 *----------------------------------------------------------------------*/
FUNCTION SetRegistryValue( nKey, cRegKey, cRegVar, uValue )
   hb_default( @cRegVar, "" )
RETURN WithRegistry( nKey, cRegKey, {|oReg| oReg:Set( cRegVar, uValue ), oReg:nError == ERROR_SUCCESS } )

/*----------------------------------------------------------------------*
 * Delete registry value
 *----------------------------------------------------------------------*/
FUNCTION DeleteRegistryVar( nKey, cRegKey, cRegVar )
   hb_default( @cRegVar, "" )
RETURN WithRegistry( nKey, cRegKey, {|oReg| oReg:Delete( cRegVar ), oReg:nError == ERROR_SUCCESS } )

/*----------------------------------------------------------------------*
 * Delete registry key
 *----------------------------------------------------------------------*/
FUNCTION DeleteRegistryKey( nKey, cRegKey )
RETURN RegDeleteKey( nKey, cRegKey ) == ERROR_SUCCESS


#pragma BEGINDUMP

#include <mgdefs.h>

/*----------------------------------------------------------------------*
 * IsWow64Process dynamic import
 *
 * Loaded dynamically to maintain compatibility with older versions of
 * Windows where IsWow64Process may not exist.
 *----------------------------------------------------------------------*/
extern HB_PTRUINT wapi_GetProcAddress( HMODULE hModule, LPCSTR lpProcName );

typedef BOOL ( WINAPI *LPFN_ISWOW64PROCESS )( HANDLE, PBOOL );

/*----------------------------------------------------------------------*
 * ISWOW64()
 *
 * Returns:
 *    .T.  -> current 32-bit process is running under WOW64
 *    .F.  -> native 32-bit Windows or API unavailable
 *----------------------------------------------------------------------*/
HB_FUNC_STATIC( ISWOW64 )
{
   BOOL bIsWow64 = FALSE;
   LPFN_ISWOW64PROCESS pIsWow64Process =
      ( LPFN_ISWOW64PROCESS ) wapi_GetProcAddress(
         GetModuleHandle( "kernel32" ), "IsWow64Process" );

   if( pIsWow64Process != NULL )
      pIsWow64Process( GetCurrentProcess(), &bIsWow64 );

   hb_retl( bIsWow64 );
}

#pragma ENDDUMP