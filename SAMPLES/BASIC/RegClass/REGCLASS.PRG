/*

   Registry class for Xbase++

(C) Thomas Braun Softwareentwicklung
    Hindenburgstr. 69
    D-72336 Balingen
    GERMANY

    e-mail   : tb@software-braun.de
    Homepage : http://www.software-braun.de

Feel free to use the source below in any project you
like as long as you keep the above copyright note.

The most current version of this class can be found on
my homepage.

Any comments and enhancements are always welcome.

If you have any questions regarding this source feel free
to contact me via e-mail.

---------------------------------------------------------------------------------------
CHANGE HISTORY (sorry german guys, only in english ;-) :

30.09.2001 Extended RegOpenKeyEx to support Windows NT / 2000 access right settings
           for registry keys. Versions before this change allways tried to open the
           registry key with maximum access rights which is not possible under NT
           for "regular" users but only for users with administrator privilege.

23.09.2001 Two new defines and some helpful functions from Scott McNay (see reghelp.prg)

02.10.1999 New iVar cSaveStringTypeAs
           New ACCESS/ASSIGN SaveStringType

29.08.1999 Fixed a weird "duplicate variable declaration" error with Xbase++ SL2 1.20.204

21.08.1999 removed a left over "Return lSuccess" in xbpReg:DelKey()
           Fixed bug in :Init() method with SET EXACT ON. Init is now saving
           the current EXACT-state and restoring it when finished. (Thanks to
           Ken Levitt for this)


18.04.1999 Added Section "CHANGE HISTORY"
           New iVar cReadBinaryTypeAs
           New ACCESS/ASSIGN ReadBinType
           New Method ::ValueList() for retrieving a 2-dim array that contains all
                                    value names - value pairs. (so now you can query if
                                    a value already exists)
           several minor bugfixes regarding the useage under WindowsNT
           (thanks to Axel Zimelka for this one and Johan Droskie for some
            other comments)

*/


#include "common.ch"
#include "dll.ch"
#include "regclass.ch"
#include "hbclass.ch"

#xtranslate Var2Bin     => HB_Serialize
#xtranslate Bin2Var     => HB_Deserialize

#define KEY_WOW64_64KEY    0x0100


/*

 Registry class, contains information and access functions for a single
 registry key. The name of the key must be set with the init method.

 The manipulation methods then allow to add/delete keys and values

*/
CLASS XbpReg

   HIDDEN:
   METHOD RegOpenKeyEx, RegCreateKeyEx, RegQueryInfoKey, RegCloseKey
   METHOD RegEnumKeyEx, RegEnumValue
   VAR nHKey // numerical handle from regclass.ch for the root key
   VAR cKey // Name of Subkey to open
   VAR nKeyHandle // Handle to currently open key
   VAR lStatus // State of Key´s (see comment in the init method)
   VAR nCallSuccess // return value of all Reg...DLL functions
   VAR nDllHandle // DLL handle
   VAR xValue // general purpose iVar
   VAR cClass
   VAR nClass
   VAR nSubKeys
   VAR nMaxSubKeyLen
   VAR nMaxClassLen
   VAR nValues
   VAR nMaxValueNameLen
   VAR nMaxValueLen
   VAR nSecurityDescriptor
   VAR cLastWriteTime
   VAR nLastWriteTime
   VAR cReadBinaryTypeAs // Datatype to be retrieved from REG_BINARY registry entry
   VAR cSaveStringTypeAs // Datatype to be saved to registry

   EXPORTED:
   METHOD Init, Create
   METHOD NewKey, DelKey
   METHOD GetValue, SetValue, DelValue
   METHOD KeyList, MoveUp, MoveDown
   METHOD ValueList

   /*ACCESS ASSIGN*/ METHOD Standard SETGET
   /*ACCESS ASSIGN*/ METHOD ReadBinType SETGET // get/set ::cReadBinaryTypeAs
   /*ACCESS ASSIGN*/ METHOD SaveStringType SETGET // get/set ::cSaveStringTypeAs
   /*ACCESS*/ METHOD Status
   /*ACCESS*/ METHOD KeyName

ENDCLASS

/*

   Init method. First determine the needed root key which is then stored in ::nHKey

   Determine subkey name and store into ::cKey

   The member function ::Status() returns a logical value that states if the key already
   exists in the registry (.T.) or not (.F.)

   If lCreate is set to .T. (default = .F.) and the key does not exist, it is
   created in the init method.

   Otherwise it has to be created via the :create() method.

*/
METHOD XbpReg:Init( cRegKey, lCreate, lReInit )

   LOCAL nPos
   /*
      Turning EXACT off, otherwise some string compares will fail...
   */
   LOCAL lSetExact := Set( _SET_EXACT, .F. )

   DEFAULT cRegKey TO ""
   DEFAULT lCreate TO .F.
   DEFAULT lReInit TO .F.

   ::lStatus := .F.
   ::nDllHandle := 0
   ::cClass := ""
   ::nClass := 0
   ::nSubKeys := 0
   ::nMaxSubKeyLen := 0
   ::nMaxClassLen := 0
   ::nValues := 0
   ::nMaxValueNameLen := 0
   ::nMaxValueLen := 0
   ::nSecurityDescriptor := 0
   ::cLastWriteTime := ""
   ::nLastWriteTime := 0

   // Keep setting when doing :MoveUp / :MoveDown
   If ! lReInit
      ::cReadBinaryTypeAs := "A" // Read Binary data into an array
      ::cSaveStringTypeAs := "C" // Save Binary data into an array
   ENDIF

   If ! Empty( cRegKey )
      IF cRegKey = "\" // remove backslash
         cRegKey := SubStr( cRegKey, 2 )
      ENDIF

      nPos := At( "\", cRegKey ) // det. name of subkey

      DO CASE
      CASE cRegKey = "HKEY_LOCAL_MACHINE"
         ::nHKey := HKEY_LOCAL_MACHINE

      CASE cRegKey = "HKEY_CLASSES_ROOT"
         ::nHKey := HKEY_CLASSES_ROOT

      CASE cRegKey = "HKEY_USERS"
         ::nHKey := HKEY_USERS

      CASE cRegKey = "HKEY_CURRENT_USER"
         ::nHKey := HKEY_CURRENT_USER

      CASE cRegKey = "HKEY_CURRENT_CONFIG"
         ::nHKey := HKEY_CURRENT_CONFIG

      CASE cRegKey = "HKEY_DYN_DATA"
         ::nHKey := HKEY_DYN_DATA

      OTHERWISE
         ::nHKey := HKEY_NO_KEY

      ENDCASE

      IF ::nHKey # HKEY_NO_KEY // if root key exists

         IF nPos # 0
            ::cKey := SubStr( cRegKey, nPos + 1 )
         ELSE
            ::cKey := ""
         ENDIF

         ::RegOpenKeyEx( ::cKey, KEY_QUERY_VALUE ) // open key

         IF ::nCallSuccess = ERROR_SUCCESS
            ::lStatus := .T.
         ELSE
            IF lCreate
               ::RegCreateKeyEx( ::cKey )
               IF ::nCallSuccess = ERROR_SUCCESS
                  ::lStatus := .T.
               ENDIF
            ENDIF
         ENDIF

         ::RegCloseKey()

      ENDIF
   ENDIF

   Set( _SET_EXACT, lSetExact )

RETURN self

/*

   Create registry key. Only works if the key does not exist (::Status() = .F.)

   Create is not needed if the Key exists or you call the init method with
   lCreate == .T.

*/
METHOD XbpReg:Create

   IF ::lStatus == .F. .AND. ::nHKey # HKEY_NO_KEY

      ::RegOpenKeyEx( ::cKey, KEY_CREATE_SUB_KEY ) // open registry key

      IF ::nCallSuccess = ERROR_SUCCESS
         ::lStatus := .T.
      ELSE
         ::RegCreateKeyEx( ::cKey )
         IF ::nCallSuccess = ERROR_SUCCESS
            ::lStatus := .T.
         ENDIF
      ENDIF

      ::RegCloseKey() // close registry key

   ENDIF

RETURN self

/*

   Open registry key

*/
METHOD XbpReg:RegOpenKeyEx( cSubKey, nAccess )

   LOCAL nNewKeyHandle := 0

   DEFAULT nAccess TO KEY_ALL_ACCESS

   nAccess := iif( IsWow64(), hb_bitOr( nAccess, KEY_WOW64_64KEY ), nAccess )

   ::nCallSuccess := 1 // 1 = no success; 0 = success
   IF Empty( ::nDllHandle )
      ::nDllHandle := DllLoad( "ADVAPI32.DLL" )
   ENDIF

   IF ::nhKey # HKEY_NO_KEY
      ::nCallSuccess := DllCall( ::nDllHandle, DLL_STDCALL, "RegOpenKeyExA", ::nhKey, ;
         cSubKey, 0, nAccess, @nNewKeyHandle )

      IF ::nCallSuccess = ERROR_SUCCESS
         ::nKeyHandle := nNewKeyHandle
         ::RegQueryInfoKey()
      ENDIF

   ENDIF

RETURN ::nCallSuccess

/*

   Close registry key

*/
METHOD XbpReg:RegCloseKey

   IF Empty( ::nDllHandle )
      ::nDllHandle := DllLoad( "ADVAPI32.DLL" )
   ENDIF

   ::nCallSuccess := DllCall( ::nDllHandle, DLL_STDCALL, "RegCloseKey", ::nKeyHandle )
   IF ::nCallSuccess = ERROR_SUCCESS
      ::nKeyHandle := HKEY_NO_KEY
   ENDIF

   If ! Empty( ::nDllHandle )
      DllUnload( ::nDllHandle )
      ::nDllHandle := 0
   ENDIF

RETURN ::nCallSuccess

/*

   Create new key

*/
METHOD XbpReg:RegCreateKeyEx( cSubKey )

   LOCAL nNewKeyHandle := 0, nDisposition := 0
   ::nCallSuccess := 1 // 1 = no success; 0 = success

   IF Empty( ::nDllHandle )
      ::nDllHandle := DllLoad( "ADVAPI32.DLL" )
   ENDIF

   ::nCallSuccess := DllCall( ::nDllHandle, DLL_STDCALL, "RegCreateKeyExA", ::nHKey, ;
      cSubKey, 0, "", REG_OPTION_NON_VOLATILE, ;
      iif( IsWow64(), hb_bitOr( KEY_ALL_ACCESS, KEY_WOW64_64KEY ), KEY_ALL_ACCESS ), ;
      0, @nNewKeyHandle, @nDisposition )

   IF ::nCallSuccess = ERROR_SUCCESS
      ::nKeyHandle := nNewKeyHandle
      ::RegQueryInfoKey()
      ::RegCloseKey()
   ENDIF

RETURN ::nCallSuccess


/*

   Create subkey below existing key

*/
METHOD XbpReg:NewKey( cSubKey, lKeepOld )

   LOCAL cOldKey := ::KeyName(), lSuccess

   DEFAULT cSubKey TO ""
   DEFAULT lKeepOld TO .T.

   If ! Empty( cSubkey )
      ::Init( cOldKey + "\" + cSubKey, .T., .T. )
      If ! ::lStatus
         ::Init( cOldKey,, .T. )
      ELSE
         lSuccess := .T.
         IF lKeepOld
            ::Init( cOldKey,, .T. )
         ENDIF
      ENDIF
   ENDIF

RETURN lSuccess

/*

   Delete subkey (including all contained values and subkeys)

*/
METHOD XbpReg:DelKey( cSubKey )

   LOCAL lRet := .F.

   ::nCallSuccess := 1 // 1 = no success; 0 = success

   IF Empty( ::nDllHandle )
      ::nDllHandle := DllLoad( "ADVAPI32.DLL" )
   ENDIF

   ::RegOpenKeyEx( ::cKey, KEY_ALL_ACCESS )
   IF ::nCallSuccess = ERROR_SUCCESS
      ::nCallSuccess := DllCall( ::nDllHandle, DLL_STDCALL, "RegDeleteKeyA", ::nKeyHandle, ;
         cSubKey )

      lRet := ( ::nCallSuccess == ERROR_SUCCESS )

   ENDIF
   ::RegCloseKey()

RETURN lRet

/*

   Gather information about registry key

*/
METHOD XbpReg:RegQueryInfoKey

   LOCAL lpClass := "", lpcbClass := 0, lpReserved := 0, lpcSubKeys := 0
   LOCAL lpcbMaxSubKeyLen := 0, lpcbMaxClassLen := 0
   LOCAL lpcValues := 0, lpcbMaxValueNameLen := 0, lpcbMaxValueLen := 0
   LOCAL lpcbSecurityDescriptor := 0, lpftLastWriteTime := "        "

   ::nCallSuccess := 1 // 1 = no success; 0 = success

   IF Empty( ::nDllHandle )
      ::nDllHandle := DllLoad( "ADVAPI32.DLL" )
   ENDIF

   ::nCallSuccess := DllCall( ::nDllHandle, DLL_STDCALL, "RegQueryInfoKeyA", ::nKeyHandle, ;
      @ lpClass, ;
      @ lpcbClass, ;
      lpReserved, ;
      @ lpcSubKeys, ;
      @ lpcbMaxSubKeyLen, ;
      @ lpcbMaxClassLen, ;
      @ lpcValues, ;
      @ lpcbMaxValueNameLen, ;
      @ lpcbMaxValueLen, ;
      @ lpcbSecurityDescriptor, ;
      @ lpftLastWriteTime )

   IF ::nCallSuccess = ERROR_SUCCESS
      ::cClass := lpClass
      ::nClass := lpcbClass
      ::nSubKeys := lpcSubKeys
      ::nMaxSubKeyLen := lpcbMaxSubKeyLen
      ::nMaxClassLen := lpcbMaxClassLen
      ::nValues := lpcValues
      ::nMaxValueNameLen := lpcbMaxValueNameLen
      ::nMaxValueLen := lpcbMaxValueLen

      // the following is only valid under WIN/NT
      ::nSecurityDescriptor := lpcbSecurityDescriptor
      ::cLastWriteTime := lpftLastWriteTime
      ::nLastWriteTime := Bin2U( SubStr( ::cLastWriteTime, 1, 4 ) ) + ;
         Bin2U( SubStr( ::cLastWriteTime, 5, 4 ) ) * 2 ^ 32
   ENDIF

RETURN ::nCallSuccess

/*

   Enumerate subkeys, key has to be opened before with ::RegOpenKeyEX

*/
METHOD xbpReg:RegEnumKeyEx( nIndex )

   LOCAL cBuffer := Space( ::nMaxSubKeyLen + 1 )
   LOCAL nBufLen := ::nMaxSubKeyLen + 1
   LOCAL lpftLastWriteTime := "        "

   IF Empty( ::nDllHandle )
      ::nDllHandle := DllLoad( "ADVAPI32.DLL" )
   ENDIF

   ::nCallSuccess := DllCall( ::nDllHandle, DLL_STDCALL, "RegEnumKeyExA", ::nKeyHandle, ;
      nIndex, @cBuffer, @nBufLen, 0, 0, 0, @lpftLastWriteTime )

   IF ::nCallSuccess = ERROR_SUCCESS
      ::cLastWriteTime := lpftLastWriteTime
      ::nLastWriteTime := Bin2U( SubStr( ::cLastWriteTime, 1, 4 ) ) + ;
         Bin2U( SubStr( ::cLastWriteTime, 5, 4 ) ) * 2 ^ 32
   ENDIF

RETURN Trim( cBuffer )

/*

   Enumerate values, key has to be opened before with ::RegOpenKeyEx

*/
METHOD xbpReg:RegEnumValue( nIndex, lWithValue )

   LOCAL cBuffer1 := Space( ::nMaxValueNameLen + 1 )
   LOCAL nBufLen1 := ::nMaxValueNameLen + 1
   LOCAL cBuffer2 := Space( ::nMaxValueLen + 1 )
   LOCAL nBufLen2 := ::nMaxValueLen + 1
   LOCAL nType := 0
   LOCAL aRet := {}

   DEFAULT lWithValue TO .T.

   IF Empty( ::nDllHandle )
      ::nDllHandle := DllLoad( "ADVAPI32.DLL" )
   ENDIF

   ::xValue := NIL
   ::nCallSuccess := DllCall( ::nDllHandle, DLL_STDCALL, "RegEnumValueA", ::nKeyHandle, ;
      nIndex, @cBuffer1, @nBufLen1, 0, @nTYPE, @cBuffer2, @nBufLen2 )

   IF ::nCallSuccess = ERROR_SUCCESS
      AAdd( aRet, Trim( SubStr( cBuffer1, 1, nBufLen1 ) ) )

      IF lWithValue
         DO CASE
         CASE nType == REG_SZ
            ::xValue := SubStr( cBuffer2, 1, nBufLen2 - 1 )

         CASE nType == REG_DWORD
            ::xValue := Bin2L( cBuffer2 )

         CASE nType == REG_BINARY
            DO CASE
            CASE ::cReadBinaryTypeAs = "A"
               ::xValue := Bin2Var( cBuffer2 )

            CASE ::cReadBinaryTypeAs = "C"
               ::xValue := cBuffer2

            CASE ::cReadBinaryTypeAs = "N"
               ::xValue := Bin2L( cBuffer2 )
            ENDCASE

         ENDCASE
      ENDIF

      AAdd( aRet, ::xValue )

   ENDIF

RETURN aRet


/*

   Get a named value

*/
METHOD XbpReg:GetValue( cName )

   LOCAL xRet := Space( ::nMaxValueLen ), nRet := ::nMaxValueLen
   LOCAL nType := 0
   LOCAL cTemp, nPos1, nPos2

   ::nCallSuccess := 1 // 1 = no success; 0 = success

   IF Empty( ::nDllHandle )
      ::nDllHandle := DllLoad( "ADVAPI32.DLL" )
   ENDIF

   ::xValue := NIL

   ::RegOpenKeyEx( ::cKey, KEY_READ )
   IF ::nCallSuccess = ERROR_SUCCESS

      ::nCallSuccess := DllCall( ::nDllHandle, DLL_STDCALL, "RegQueryValueExA", ::nKeyHandle, ;
         @ cNAME, 0, @nTYPE, @xRet, @nRet )

      IF ::nCallSuccess = ERROR_SUCCESS
         DO CASE
         CASE nType == REG_SZ
            ::xValue := SubStr( xRet, 1, nRet - 1 )

         CASE nType == REG_DWORD
            ::xValue := Bin2L( xRet )

         CASE nType == REG_BINARY
            DO CASE
            CASE ::cReadBinaryTypeAs = "A"
               ::xValue := Bin2Var( xRet )

            CASE ::cReadBinaryTypeAs = "C"
               ::xValue := xRet

            CASE ::cReadBinaryTypeAs = "N"
               ::xValue := Bin2L( xRet )
            ENDCASE

         CASE nType == REG_EXPAND_SZ
            ::xValue := SubStr( xRet, 1, nRet - 1 )
            DO WHILE "%" $ ::xValue
               nPos1 := AtNum( "%", ::xValue, 1 )
               nPos2 := AtNum( "%", ::xValue, 2 )
               IF nPos2 = 0
                  EXIT
               ENDIF
               cTemp := SubStr( ::xValue, nPos1 + 1, nPos2 - nPos1 - 1 )
               IF nPos1 == 1
                  ::xValue := GetEnv( cTemp ) + SubStr( ::xValue, nPos2 + 1 )
               ELSE
                  ::xValue := SubStr( ::xValue, 1, nPos1 - 1 ) + ;
                     GetEnv( cTemp ) + SubStr( ::xValue, nPos2 + 1 )
               ENDIF
            ENDDO
         ENDCASE
      ENDIF
   ENDIF
   ::RegCloseKey()

RETURN ::xValue

/*
   Write/create a named value
*/
METHOD XbpReg:SetValue( cName, xValue )

   LOCAL nLen := 0, nType := REG_NONE

   ::nCallSuccess := 1 // 1 = no success; 0 = success

   IF Empty( ::nDllHandle )
      ::nDllHandle := DllLoad( "ADVAPI32.DLL" )
   ENDIF

   DO CASE
   CASE ValType( xValue ) = "C"
      DO CASE
      CASE ::cSaveStringTypeAs = "C" // Save string as string (default)
         nLen := Len( xValue ) + 1
         nType := REG_SZ

      CASE ::cSaveStringTypeAs = "B" // Save string as binary value
         nLen := Len( xValue )
         nType := REG_BINARY
      ENDCASE

   CASE ValType( xValue ) = "N"
      nLen := 4
      xValue := L2Bin( xValue )
      nType := REG_DWORD

   CASE ValType( xValue ) = "A"
      xValue := Var2Bin( xValue )
      nLen := Len( xValue )
      nType := REG_BINARY

   ENDCASE

   ::RegOpenKeyEx( ::cKey, KEY_WRITE + KEY_QUERY_VALUE )
   IF ::nCallSuccess = ERROR_SUCCESS
      ::nCallSuccess := DllCall( ::nDllHandle, DLL_STDCALL, "RegSetValueExA", ::nKeyHandle, ;
         @ cNAME, 0, nType, @xVALUE, nLen )

      IF ::nCallSuccess = ERROR_SUCCESS
      ENDIF
   ENDIF
   ::RegCloseKey()

RETURN ::nCallSuccess

/*

   Delete value

*/
METHOD XbpReg:DelValue( cName )

   LOCAL lRet := .F.

   ::nCallSuccess := 1 // 1 = no success; 0 = success

   IF Empty( ::nDllHandle )
      ::nDllHandle := DllLoad( "ADVAPI32.DLL" )
   ENDIF

   ::RegOpenKeyEx( ::cKey, KEY_ALL_ACCESS )
   IF ::nCallSuccess = ERROR_SUCCESS

      ::nCallSuccess := DllCall( ::nDllHandle, DLL_STDCALL, "RegDeleteValueA", ::nKeyHandle, ;
         @ cName )

      lRet := ( ::nCallSuccess == ERROR_SUCCESS )

   ENDIF
   ::RegCloseKey()

RETURN lRet


/*

   Get/Set "standard" value of the registry key for example see
   \HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion\TimeZones

*/
METHOD XbpReg:Standard( xValue )

   IF xValue # NIL
      ::SetValue( "", xValue )
   ENDIF

RETURN ::GetValue( "" )

/*

   ACCESS/ASSIGN to get/set the Xbase++ datatype that should be used when reading
   binary data from the registry. The standard value is "A" so that data with the
   REG_BINARY type attribute gets transferred into an array via BIN2VAR.

   Allowed values are "C" = retrieve as string
                      "A" = retrieve into an array via BIN2VAR()
                      "N" = retrieve into a numeric variable via BIN2L()

   (Arrays are always stored as REG_BINARY into the registry via VAR2BIN when
    using ::SetValue())

*/
METHOD XbpReg:ReadBinType( xValue )

   IF xValue # NIL
      IF ValType( xValue ) = "C"
         ::cReadBinaryTypeAs := xValue
      ENDIF
   ENDIF

RETURN ::cReadBinaryTypeAs

/*

   ACCESS/ASSIGN to get/set the registry datatype that should be used when writing
   string type Xbase++ data to the registry. The standard value is "C" so strings will
   be saved as REG_SZ = zero terminated strings

   Allowed values are "C" = save as REG_SZ
                      "B" = save as REG_BINARY

   (Arrays are always stored into the registry as REG_BINARY via VAR2BIN when using
    ::SetValue())

*/
METHOD XbpReg:SaveStringType( xValue )

   IF xValue # NIL
      IF ValType( xValue ) = "C"
         ::cSaveStringTypeAs := xValue
      ENDIF
   ENDIF

RETURN ::cSaveStringTypeAs

/*

   Get object status, .F. if key does not exist

*/
METHOD XbpReg:Status
RETURN ::lStatus


/*

   Return complete registry key "path"

*/
METHOD XbpReg:KeyName

   LOCAL cRet := "\"

   DO CASE
   CASE ::nHKey = HKEY_LOCAL_MACHINE
      cRet += "HKEY_LOCAL_MACHINE"

   CASE ::nHKey = HKEY_CLASSES_ROOT
      cRet += "HKEY_CLASSES_ROOT"

   CASE ::nHKey = HKEY_USERS
      cRet += "HKEY_USERS"

   CASE ::nHKey = HKEY_CURRENT_USER
      cRet += "HKEY_CURRENT_USER"

   ENDCASE

   cRet += iif( ! Empty( ::cKey ), "\" + ::cKey, "" )

RETURN cRet



/*

   List all subkeys (returns an array)
   Uses ::RegEnumKeyEx()

*/
METHOD XbpReg:KeyList

   LOCAL nIndex, aRet := {}, cEnumKey

   ::nCallSuccess := 1 // 1 = no success; 0 = success

   IF Empty( ::nDllHandle )
      ::nDllHandle := DllLoad( "ADVAPI32.DLL" )
   ENDIF

   ::RegOpenKeyEx( ::cKey, KEY_READ ) // Key öffnen, Fehlercode steht in ::nCallSuccess
   IF ::nCallSuccess = ERROR_SUCCESS

      nIndex := ::nSubKeys

      DO WHILE nIndex >= 0

         If ! Empty( cEnumKey := ::RegEnumKeyEx( nIndex ) )
            AAdd( aRet, cEnumKey )
         ENDIF

         nIndex--
      ENDDO

   ENDIF
   ::RegCloseKey()

   IF Len( aRet ) # 0
      aRet := ASort( aRet,,, {| c1, c2 | Upper( c1 ) < Upper( c2 ) } )
   ENDIF

RETURN aRet

/*

   List all values (returns an 2-dimensional array)
   Set lWithValues to .F. if you only need the value names, not the value itself
   Default is .T.

   Uses ::RegEnumValue()

*/
METHOD XbpReg:ValueList( lWithValues )

   LOCAL nIndex, aRet := {}, aEnumVal

   DEFAULT lWithValues TO .T.

   ::nCallSuccess := 1 // 1 = no success; 0 = success

   IF Empty( ::nDllHandle )
      ::nDllHandle := DllLoad( "ADVAPI32.DLL" )
   ENDIF

   ::RegOpenKeyEx( ::cKey, KEY_READ ) // Key öffnen, Fehlercode steht in ::nCallSuccess
   IF ::nCallSuccess = ERROR_SUCCESS

      nIndex := ::nValues
      IF nIndex # 0
         ::RegEnumValue( 0 )
      ENDIF

      DO WHILE nIndex >= 0

         IF Len( aEnumVal := ::RegEnumValue( nIndex, lWithValues ) ) # 0
            AAdd( aRet, aEnumVal )
         ENDIF

         nIndex--
      ENDDO

   ENDIF
   ::RegCloseKey()

   IF Len( aRet ) # 0
      aRet := ASort( aRet,,, {| c1, c2 | Upper( c1[ 1 ] ) < Upper( c2[ 1 ] ) } )
   ENDIF

RETURN aRet

/*

   Move one level up in the key hierarchy
   (similar to the dos command CD..)

*/
METHOD xbpReg:MoveUp

   LOCAL cNewKey, nPos, lSuccess := .F.

   nPos := RAt( "\", ::KeyName() )

   IF nPos > 1
      cNewKey := Left( ::KeyName, nPos - 1 )
      ::Init( cNewKey,, .T. )
      lSuccess := ::lStatus
   ENDIF

RETURN lSuccess

/*

   Move down one level

*/
METHOD xbpReg:MoveDown( cSubkey )

   LOCAL cOldKey := ::KeyName(), lSuccess := .F.

   DEFAULT cSubKey TO ""
   If ! Empty( cSubkey )
      ::Init( cOldKey + "\" + cSubKey,, .T. )
      If ! ::lStatus
         ::Init( cOldKey,, .T. )
      ELSE
         lSuccess := .T.
      ENDIF
   ENDIF

RETURN lSuccess


STATIC FUNCTION ATNUM( cSearch, cString, nCount )

   LOCAL nPos, nCount2, nRet := -1

   IF PCount() <= 3 .AND. nCount >= 0

      nPos := 1
      nCount2 := 0

      DO WHILE .T.
         nRet := At( cSearch, SubStr( cString, nPos ) )
         IF nRet # 0
            nCount2++
            IF nCount2 == nCount
               nRet += nPos - 1
               EXIT
            ELSE
               nPos += nRet
            ENDIF
         ELSE
            EXIT
         ENDIF
      ENDDO

   ENDIF

RETURN nRet

#pragma BEGINDUMP

#include <mgdefs.h>

extern HB_PTRUINT wapi_GetProcAddress( HMODULE hModule, const char * lpProcName );

typedef BOOL ( WINAPI *LPFN_ISWOW64PROCESS ) ( HANDLE, PBOOL );

HB_FUNC_STATIC( ISWOW64 )
{
   BOOL bIsWow64 = FALSE;

   LPFN_ISWOW64PROCESS fnIsWow64Process;

   fnIsWow64Process = ( LPFN_ISWOW64PROCESS ) wapi_GetProcAddress( GetModuleHandle( "kernel32" ), "IsWow64Process" );
   if( NULL != fnIsWow64Process )
   {
      fnIsWow64Process( GetCurrentProcess(), &bIsWow64 );
   }

   hb_retl( bIsWow64 );
}

#pragma ENDDUMP
