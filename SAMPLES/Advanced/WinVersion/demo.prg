/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
 *
 * Copyright 2007-2022 Grigory Filatov <gfilatov@gmail.com>
*/

#include "minigui.ch"

#define NTRIM( n ) hb_ntos( n )

*--------------------------------------------------------*
PROCEDURE Main
*--------------------------------------------------------*
/*
   * Purpose: This is the main procedure of the application. It creates a window
   *          and displays information about the Windows version.
   *
   * Explanation:
   *   - It first calls GetInfoString() to retrieve a string containing various
   *     Windows version details.
   *   - Then, it defines a main window (Form_1) with an edit box (Edit_1) to
   *     display the information string.
   *   - The window is centered on the screen and activated to be displayed.
   *   - The iif( IsWin9X() .OR. IsServer(), 10, 0 ) part adjusts the window height
   *     based on whether the OS is Win9x or a Server to account for potential
   *     differences in title bar height or other system metrics.
   *   - NOMAXIMIZE and NOSIZE prevent the user from maximizing or resizing the window.
*/
   LOCAL cValue := GetInfoString()

   DEFINE WINDOW Form_1 ;
         AT 0, 0 ;
         WIDTH 260 HEIGHT 474 - iif( IsWin9X() .OR. IsServer(), 10, 0 ) ;
         TITLE 'WinVersion Test' ;
         ICON 'DEMO.ICO' ;
         MAIN ;
         NOMAXIMIZE NOSIZE ;
         FONT 'MS Sans Serif' SIZE 8

      @ 0, 0 EDITBOX Edit_1 ;
         WIDTH Form_1.WIDTH - 2 * GetBorderWidth() + 2 ;
         HEIGHT Form_1.HEIGHT - GetTitleHeight() - 2 * GetBorderHeight() + 2 ;
         VALUE cValue ;
         MAXLENGTH 1024 ;
         NOHSCROLL

   END WINDOW

   Form_1.Center()

   Form_1.Activate()

RETURN

*--------------------------------------------------------*
FUNCTION GetInfoString()
*--------------------------------------------------------*
/*
   * Purpose: This function retrieves various Windows version information and
   *          formats it into a single string for display.
   *
   * Parameters: None
   *
   * Return Value: A string containing Windows version information.
   *
   * Explanation:
   *   - It calls various functions (GetMajorVersion(), GetMinorVersion(), etc.)
   *     to retrieve individual pieces of Windows version information.
   *   - It then concatenates these pieces of information into a single string,
   *     along with labels and carriage return/line feed characters (CRLF) for
   *     formatting.
   *   - The resulting string is returned.
*/
   LOCAL nMajorVersion := GetMajorVersion()
   LOCAL nMinorVersion := GetMinorVersion()
   LOCAL nBuildNumber := GetBuildNumber()
   LOCAL nPlatformId := GetPlatformId()
   LOCAL nServicePack := GetServicePackNT()
   LOCAL cServicePack := GetServicePackString()
   LOCAL cWinVersion := GetWinVersionString()
   LOCAL bWin95 := IsWin95()
   LOCAL bWin98 := IsWin98()
   LOCAL bWinME := IsWinME()
   LOCAL bWin9X := IsWin9X()
   LOCAL bWinNT := IsWinNT()
   LOCAL bWinNT4 := IsWinNT4()
   LOCAL bWin2000 := IsWin2K()
   LOCAL bWin2KorLater := IsWin2KorLater()
   LOCAL bWin2003 := IsWin2003()
   LOCAL bXP := ( IsWinXP() .AND. GetMajorVersion() == 5 )
   LOCAL bXPorLater := IsWinXPorLater()
   LOCAL bXPHome := ( IsWinXPHome() .AND. GetMajorVersion() == 5 )
   LOCAL bXPPro := ( IsWinXPPro() .AND. GetMajorVersion() == 5 )
   LOCAL bXPSP2 := IsWinXPSP2()
   LOCAL bXPSP3 := IsWinXPSP3()
   LOCAL bMediaCenter := ( IsMediaCenter() .AND. GetMajorVersion() == 5 )
   LOCAL bVista := IsWinVista()
   LOCAL bVistaorLater := IsVistaOrLater()
   LOCAL bServer := IsServer()
   LOCAL bWin7 := IsWin7()
   LOCAL bWin8orLater := IsWin8OrLater()
   LOCAL bWin8 := IsWin8()
   LOCAL bWin81 := IsWin81()
   LOCAL bWin10 := IsWin10()
   LOCAL bWin10orLater := IsWin10OrLater()
   LOCAL bWin11 := IsWin11()
   LOCAL cRetString := ""

   cRetString += "major version = " + NTRIM( nMajorVersion ) + CRLF
   cRetString += "minor version = " + NTRIM( nMinorVersion ) + CRLF
   cRetString += "build number = " + NTRIM( nBuildNumber ) + CRLF
   cRetString += "platform id = " + NTRIM( nPlatformId ) + CRLF
   cRetString += "service pack = " + NTRIM( nServicePack ) + CRLF
   cRetString += "service pack string = " + LTrim( cServicePack ) + CRLF
   cRetString += "version string = " + LTrim( cWinVersion ) + CRLF
   cRetString += "Win95 = " + IF( bWin95, "true", "false" ) + CRLF
   cRetString += "Win98 = " + IF( bWin98, "true", "false" ) + CRLF
   cRetString += "WinME = " + IF( bWinME, "true", "false" ) + CRLF
   cRetString += "Win9X = " + IF( bWin9X, "true", "false" ) + CRLF
   cRetString += "WinNT = " + IF( bWinNT, "true", "false" ) + CRLF
   cRetString += "WinNT4 = " + IF( bWinNT4, "true", "false" ) + CRLF
   cRetString += "Win2000 = " + IF( bWin2000, "true", "false" ) + CRLF
   cRetString += "Win2K or later = " + IF( bWin2KorLater, "true", "false" ) + CRLF
   cRetString += "Win2003 = " + IF( bWin2003, "true", "false" ) + CRLF
   cRetString += "WinXP = " + IF( bXP, "true", "false" ) + CRLF
   cRetString += "WinXP or later = " + IF( bXPorLater, "true", "false" ) + CRLF
   cRetString += "WinXP Home = " + IF( bXPHome, "true", "false" ) + CRLF
   cRetString += "WinXP Pro = " + IF( bXPPro, "true", "false" ) + CRLF
   cRetString += "WinXP SP2 = " + IF( bXPSP2, "true", "false" ) + CRLF
   cRetString += "WinXP SP3 = " + IF( bXPSP3, "true", "false" ) + CRLF
   cRetString += "Media Center = " + IF( bMediaCenter, "true", "false" ) + CRLF
   cRetString += "Vista = " + IF( bVista, "true", "false" ) + CRLF
   cRetString += "Vista or later = " + IF( bVistaorLater, "true", "false" ) + CRLF
   cRetString += "Server = " + IF( bServer, "true", "false" ) + CRLF
   cRetString += "Win7 = " + IF( bWin7, "true", "false" ) + CRLF
   cRetString += "Win8 or later = " + IF( bWin8orLater, "true", "false" ) + CRLF
   cRetString += "Win8 = " + IF( bWin8, "true", "false" ) + CRLF
   cRetString += "Win8.1 = " + IF( bWin81, "true", "false" ) + CRLF
   cRetString += "Win10 = " + IF( bWin10, "true", "false" ) + CRLF
   cRetString += "Win10 or later = " + IF( bWin10orLater, "true", "false" ) + CRLF
   cRetString += "Win11 = " + IF( bWin11, "true", "false" )

RETURN cRetString

*--------------------------------------------------------*
FUNCTION GetMajorVersion()
*--------------------------------------------------------*
/*
   * Purpose: This function retrieves the major version number of the Windows operating system.
   *
   * Parameters: None
   *
   * Return Value: The major version number as a numeric value.
   *
   * Explanation:
   *   - It calls the GetWinVersionInfo() function to retrieve an array containing
   *     various Windows version details.
   *   - It then returns the first element of the array, which represents the
   *     major version number.
*/
   LOCAL aVer := GetWinVersionInfo()

RETURN aVer[ 1 ]

*--------------------------------------------------------*
FUNCTION GetMinorVersion()
*--------------------------------------------------------*
/*
   * Purpose: This function retrieves the minor version number of the Windows operating system.
   *
   * Parameters: None
   *
   * Return Value: The minor version number as a numeric value.
   *
   * Explanation:
   *   - It calls the GetWinVersionInfo() function to retrieve an array containing
   *     various Windows version details.
   *   - It then returns the second element of the array, which represents the
   *     minor version number.
*/
   LOCAL aVer := GetWinVersionInfo()

RETURN aVer[ 2 ]

*--------------------------------------------------------*
FUNCTION GetBuildNumber()
*--------------------------------------------------------*
/*
   * Purpose: This function retrieves the build number of the Windows operating system.
   *
   * Parameters: None
   *
   * Return Value: The build number as a numeric value.
   *
   * Explanation:
   *   - It calls the GetWinVersionInfo() function to retrieve an array containing
   *     various Windows version details.
   *   - It then returns the third element of the array, which represents the
   *     build number.
*/
   LOCAL aVer := GetWinVersionInfo()

RETURN aVer[ 3 ]

*--------------------------------------------------------*
FUNCTION GetPlatformId()
*--------------------------------------------------------*
/*
   * Purpose: This function retrieves the platform ID of the Windows operating system.
   *
   * Parameters: None
   *
   * Return Value: The platform ID as a numeric value.
   *
   * Explanation:
   *   - It calls the GetWinVersionInfo() function to retrieve an array containing
   *     various Windows version details.
   *   - It then returns the fourth element of the array, which represents the
   *     platform ID.
*/
   LOCAL aVer := GetWinVersionInfo()

RETURN aVer[ 4 ]

*--------------------------------------------------------*
FUNCTION GetWinVersionString()
*--------------------------------------------------------*
/*
   * Purpose: This function retrieves the Windows version string.
   *
   * Parameters: None
   *
   * Return Value: The Windows version string.
   *
   * Explanation:
   *   - It calls the WinVersion() function to retrieve an array containing
   *     the Windows version string and the service pack string.
   *   - It then returns the first element of the array, which represents the
   *     Windows version string.
*/
   LOCAL aVer := WinVersion()

RETURN aVer[ 1 ]

*--------------------------------------------------------*
FUNCTION GetServicePackString()
*--------------------------------------------------------*
/*
   * Purpose: This function retrieves the service pack string of the Windows operating system.
   *
   * Parameters: None
   *
   * Return Value: The service pack string.
   *
   * Explanation:
   *   - It calls the WinVersion() function to retrieve an array containing
   *     the Windows version string and the service pack string.
   *   - It then returns the second element of the array, which represents the
   *     service pack string.
*/
   LOCAL aVer := WinVersion()

RETURN aVer[ 2 ]

*--------------------------------------------------------*
FUNCTION GetServicePackNT()
*--------------------------------------------------------*
/*
   * Purpose: This function retrieves the service pack number as a numeric value for NT-based systems.
   *
   * Parameters: None
   *
   * Return Value: The service pack number as a numeric value, or 0 if not applicable.
   *
   * Explanation:
   *   - It checks if the operating system is Windows 2000 or later using IsWin2KorLater().
   *   - If it is, it retrieves the service pack string using GetServicePackString(),
   *     extracts the last character (which is assumed to be the service pack number),
   *     and converts it to a numeric value using Val().
   *   - If it is not Windows 2000 or later, it returns 0.
*/

RETURN iif( IsWin2KorLater(), Val( Right( Trim( GetServicePackString() ), 1 ) ), 0 )

*--------------------------------------------------------*
FUNCTION IsWinXPHome()
*--------------------------------------------------------*
/*
   * Purpose: This function determines if the operating system is Windows XP Home Edition.
   *
   * Parameters: None
   *
   * Return Value: .T. (TRUE) if the operating system is Windows XP Home Edition, .F. (FALSE) otherwise.
   *
   * Explanation:
   *   - It calls the WinVersion() function to retrieve an array containing Windows version information.
   *   - It checks if the operating system is Windows XP using IsWinXP().
   *   - If it is, it checks if the string "Home" is present in the fourth element of the array
   *     (which is assumed to contain the edition information).
   *   - It returns .T. if both conditions are true, .F. otherwise.
*/
   LOCAL aVer := WinVersion()

RETURN iif( IsWinXP(), "Home" $ aVer[ 4 ], .F. )

*--------------------------------------------------------*
FUNCTION IsWinXPPro()
*--------------------------------------------------------*
/*
   * Purpose: This function determines if the operating system is Windows XP Professional Edition.
   *
   * Parameters: None
   *
   * Return Value: .T. (TRUE) if the operating system is Windows XP Professional Edition, .F. (FALSE) otherwise.
   *
   * Explanation:
   *   - It calls the WinVersion() function to retrieve an array containing Windows version information.
   *   - It checks if the operating system is Windows XP using IsWinXP().
   *   - If it is, it checks if the string "Pro" is present in the fourth element of the array
   *     (which is assumed to contain the edition information).
   *   - It returns .T. if both conditions are true, .F. otherwise.
*/
   LOCAL aVer := WinVersion()

RETURN iif( IsWinXP(), "Pro" $ aVer[ 4 ], .F. )

*--------------------------------------------------------*
FUNCTION IsWinXPSP2()
*--------------------------------------------------------*
/*
   * Purpose: This function determines if the operating system is Windows XP Service Pack 2.
   *
   * Parameters: None
   *
   * Return Value: .T. (TRUE) if the operating system is Windows XP Service Pack 2, .F. (FALSE) otherwise.
   *
   * Explanation:
   *   - It checks if the operating system is Windows XP using IsWinXP().
   *   - If it is, it retrieves the service pack number using GetServicePackNT() and checks if it is equal to 2.
   *   - It returns .T. if both conditions are true, .F. otherwise.
*/

RETURN iif( IsWinXP(), GetServicePackNT() == 2, .F. )

*--------------------------------------------------------*
FUNCTION IsWinXPSP3()
*--------------------------------------------------------*
/*
   * Purpose: This function determines if the operating system is Windows XP Service Pack 3.
   *
   * Parameters: None
   *
   * Return Value: .T. (TRUE) if the operating system is Windows XP Service Pack 3, .F. (FALSE) otherwise.
   *
   * Explanation:
   *   - It checks if the operating system is Windows XP using IsWinXP().
   *   - If it is, it retrieves the service pack number using GetServicePackNT() and checks if it is equal to 3.
   *   - It returns .T. if both conditions are true, .F. otherwise.
*/

RETURN iif( IsWinXP(), GetServicePackNT() == 3, .F. )

*--------------------------------------------------------*
FUNCTION IsServer()
*--------------------------------------------------------*
/*
   * Purpose: This function determines if the operating system is a Windows Server version.
   *
   * Parameters: None
   *
   * Return Value: .T. (TRUE) if the operating system is a Windows Server version, .F. (FALSE) otherwise.
   *
   * Explanation:
   *   - It checks if the operating system is Vista or later using IsVistaOrLater().
   *   - If it is, it retrieves the Windows version string using GetWinVersionString() and checks if it contains the string "Server".
   *   - It returns .T. if both conditions are true, .F. otherwise.
*/

RETURN iif( IsVistaOrLater(), 'Server' $ GetWinVersionString(), .F. )

*--------------------------------------------------------*
FUNCTION IsWin7()
*--------------------------------------------------------*
/*
   * Purpose: This function determines if the operating system is Windows 7.
   *
   * Parameters: None
   *
   * Return Value: .T. (TRUE) if the operating system is Windows 7, .F. (FALSE) otherwise.
   *
   * Explanation:
   *   - It checks if the operating system is Vista or later using IsVistaOrLater().
   *   - If it is, it retrieves the Windows version string using GetWinVersionString() and checks if it contains the string "7".
   *   - It returns .T. if both conditions are true, .F. otherwise.
*/

RETURN iif( IsVistaOrLater(), '7' $ GetWinVersionString(), .F. )

*--------------------------------------------------------*
FUNCTION IsWin8()
*--------------------------------------------------------*
/*
   * Purpose: This function determines if the operating system is Windows 8.
   *
   * Parameters: None
   *
   * Return Value: .T. (TRUE) if the operating system is Windows 8, .F. (FALSE) otherwise.
   *
   * Explanation:
   *   - It checks if the operating system is Windows 8 or later using IsWin8OrLater().
   *   - If it is, it retrieves the major and minor version numbers using GetMajorVersion() and GetMinorVersion()
   *     and checks if they are equal to 6 and 2, respectively.
   *   - It returns .T. if both conditions are true, .F. otherwise.
*/

RETURN iif( IsWin8OrLater(), GetMajorVersion() == 6 .AND. GetMinorVersion() == 2, .F. )

*--------------------------------------------------------*
FUNCTION IsWin81()
*--------------------------------------------------------*
/*
   * Purpose: This function determines if the operating system is Windows 8.1.
   *
   * Parameters: None
   *
   * Return Value: .T. (TRUE) if the operating system is Windows 8.1, .F. (FALSE) otherwise.
   *
   * Explanation:
   *   - It checks if the operating system is Windows 8 or later using IsWin8OrLater().
   *   - If it is, it retrieves the major and minor version numbers using GetMajorVersion() and GetMinorVersion()
   *     and checks if they are equal to 6 and 3, respectively.
   *   - It returns .T. if both conditions are true, .F. otherwise.
*/

RETURN iif( IsWin8OrLater(), GetMajorVersion() == 6 .AND. GetMinorVersion() == 3, .F. )

*--------------------------------------------------------*
FUNCTION IsWin10()
*--------------------------------------------------------*
/*
   * Purpose: This function determines if the operating system is Windows 10.
   *
   * Parameters: None
   *
   * Return Value: .T. (TRUE) if the operating system is Windows 10, .F. (FALSE) otherwise.
   *
   * Explanation:
   *   - It retrieves the major version number using GetMajorVersion() and checks if it is equal to 10.
   *   - It retrieves the build number using GetBuildNumber() and checks if it is less than 22000.
   *   - It returns .T. if both conditions are true, .F. otherwise.
   *   - Note: Build number check is used to differentiate between Win10 and Win11.
*/

RETURN ( GetMajorVersion() == 10 .AND. GetBuildNumber() < 22000 )

*--------------------------------------------------------*
FUNCTION IsWin11()
*--------------------------------------------------------*
/*
   * Purpose: This function determines if the operating system is Windows 11.
   *
   * Parameters: None
   *
   * Return Value: .T. (TRUE) if the operating system is Windows 11, .F. (FALSE) otherwise.
   *
   * Explanation:
   *   - It calls the hb_osIsWin11() function (a Harbour API function) to determine if the OS is Windows 11.
   *   - It returns the result of this function call.
*/

RETURN hb_osIsWin11()


#pragma BEGINDUMP

#define SM_MEDIACENTER          87

#include <windows.h>
#include "hbapiitm.h"

/*
 * Purpose: Retrieves the operating system version information.
 *
 * Parameters:
 *   - pOSvi: A pointer to an OSVERSIONINFO structure where the version information will be stored.
 *
 * Return Value: None
 *
 * Explanation:
 *   - This function populates the OSVERSIONINFO structure pointed to by pOSvi with the operating system's version details.
 *   - It first sets the dwOSVersionInfoSize member of the structure to the size of the structure itself, which is required by GetVersionEx.
 *   - Then, it calls the GetVersionEx function to retrieve the version information and store it in the structure.
 */
static void getwinver(  OSVERSIONINFO * pOSvi )
{
  pOSvi->dwOSVersionInfoSize = sizeof( OSVERSIONINFO );
  GetVersionEx ( pOSvi );
}

HB_FUNC( ISWINNT )
/*
 * Purpose: Determines if the operating system is Windows NT-based.
 *
 * Parameters: None
 *
 * Return Value: None (Harbour function, returns value via hb_retl)
 *
 * Explanation:
 *   - Retrieves the OS version information using GetVersionEx.
 *   - Checks the dwPlatformId field to see if it matches VER_PLATFORM_WIN32_NT.
 *   - Returns .T. if it's an NT-based system, .F. otherwise.
 */
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT );
}

HB_FUNC( ISWIN9X )
/*
 * Purpose: Determines if the operating system is Windows 9x-based (95, 98, ME).
 *
 * Parameters: None
 *
 * Return Value: None (Harbour function, returns value via hb_retl)
 *
 * Explanation:
 *   - Retrieves the OS version information using GetVersionEx.
 *   - Checks the dwPlatformId field to see if it matches VER_PLATFORM_WIN32_WINDOWS.
 *   - Returns .T. if it's a 9x-based system, .F. otherwise.
 */
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS );
}

HB_FUNC( ISWIN95 )
/*
 * Purpose: Determines if the operating system is Windows 95.
 *
 * Parameters: None
 *
 * Return Value: None (Harbour function, returns value via hb_retl)
 *
 * Explanation:
 *   - Retrieves the OS version information using GetVersionEx.
 *   - Checks the dwPlatformId, dwMajorVersion, and dwMinorVersion fields to see if they match the values for Windows 95.
 *   - Returns .T. if it's Windows 95, .F. otherwise.
 */
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS
        && osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 0 );
}

HB_FUNC( ISWIN98 )
/*
 * Purpose: Determines if the operating system is Windows 98.
 *
 * Parameters: None
 *
 * Return Value: None (Harbour function, returns value via hb_retl)
 *
 * Explanation:
 *   - Retrieves the OS version information using GetVersionEx.
 *   - Checks the dwPlatformId, dwMajorVersion, and dwMinorVersion fields to see if they match the values for Windows 98.
 *   - Returns .T. if it's Windows 98, .F. otherwise.
 */
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS
        && osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 10 );
}

HB_FUNC( ISWINME )
/*
 * Purpose: Determines if the operating system is Windows ME (Millennium Edition).
 *
 * Parameters: None
 *
 * Return Value: None (Harbour function, returns value via hb_retl)
 *
 * Explanation:
 *   - Retrieves the OS version information using GetVersionEx.
 *   - Checks the dwPlatformId, dwMajorVersion, and dwMinorVersion fields to see if they match the values for Windows ME.
 *   - Returns .T. if it's Windows ME, .F. otherwise.
 */
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS
        && osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 90 );
}

HB_FUNC( ISWINNT351 )
/*
 * Purpose: Determines if the operating system is Windows NT 3.51.
 *
 * Parameters: None
 *
 * Return Value: None (Harbour function, returns value via hb_retl)
 *
 * Explanation:
 *   - Retrieves the OS version information using GetVersionEx.
 *   - Checks the dwPlatformId, dwMajorVersion, and dwMinorVersion fields to see if they match the values for Windows NT 3.51.
 *   - Returns .T. if it's Windows NT 3.51, .F. otherwise.
 */
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT
        && osvi.dwMajorVersion == 3 && osvi.dwMinorVersion == 51 );
}

HB_FUNC( ISWINNT4 )
/*
 * Purpose: Determines if the operating system is Windows NT 4.0.
 *
 * Parameters: None
 *
 * Return Value: None (Harbour function, returns value via hb_retl)
 *
 * Explanation:
 *   - Retrieves the OS version information using GetVersionEx.
 *   - Checks the dwPlatformId, dwMajorVersion, and dwMinorVersion fields to see if they match the values for Windows NT 4.0.
 *   - Returns .T. if it's Windows NT 4.0, .F. otherwise.
 */
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT
        && osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 0 );
}

HB_FUNC( ISWIN2K )
/*
 * Purpose: Determines if the operating system is Windows 2000 (Windows NT 5.0).
 *
 * Parameters: None
 *
 * Return Value: None (Harbour function, returns value via hb_retl)
 *
 * Explanation:
 *   - Retrieves the OS version information using GetVersionEx.
 *   - Checks the dwMajorVersion and dwMinorVersion fields to see if they match the values for Windows 2000.
 *   - Returns .T. if it's Windows 2000, .F. otherwise.
 */
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 0 );
}

HB_FUNC( ISWINXP )
/*
 * Purpose: Determines if the operating system is Windows XP (Windows NT 5.1).
 *
 * Parameters: None
 *
 * Return Value: None (Harbour function, returns value via hb_retl)
 *
 * Explanation:
 *   - Retrieves the OS version information using GetVersionEx.
 *   - Checks the dwMajorVersion and dwMinorVersion fields to see if they match the values for Windows XP.
 *   - Returns .T. if it's Windows XP, .F. otherwise.
 */
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 1 );
}

HB_FUNC( ISWIN2003 )
/*
 * Purpose: Determines if the operating system is Windows Server 2003 (Windows NT 5.2).
 *
 * Parameters: None
 *
 * Return Value: None (Harbour function, returns value via hb_retl)
 *
 * Explanation:
 *   - Retrieves the OS version information using GetVersionEx.
 *   - Checks the dwMajorVersion and dwMinorVersion fields to see if they match the values for Windows Server 2003.
 *   - Returns .T. if it's Windows Server 2003, .F. otherwise.
 */
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwMajorVersion == 5 && osvi.dwMinorVersion == 2 );
}

HB_FUNC( ISWINVISTA )
/*
 * Purpose: Determines if the operating system is Windows Vista (Windows NT 6.0).
 *
 * Parameters: None
 *
 * Return Value: None (Harbour function, returns value via hb_retl)
 *
 * Explanation:
 *   - Retrieves the OS version information using GetVersionEx.
 *   - Checks the dwMajorVersion and dwMinorVersion fields to see if they match the values for Windows Vista.
 *   - Returns .T. if it's Windows Vista, .F. otherwise.
 */
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwMajorVersion == 6 && osvi.dwMinorVersion == 0 );
}

HB_FUNC( ISWIN2KORLATER )
/*
 * Purpose: Determines if the operating system is Windows 2000 or later.
 *
 * Parameters: None
 *
 * Return Value: None (Harbour function, returns value via hb_retl)
 *
 * Explanation:
 *   - Retrieves the OS version information using GetVersionEx.
 *   - Checks the dwMajorVersion field to see if it's greater than or equal to 5 (Windows 2000).
 *   - Returns .T. if it's Windows 2000 or later, .F. otherwise.
 */
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( osvi.dwMajorVersion >= 5 );
}

HB_FUNC( ISWIN8ORLATER )
/*
 * Purpose: Determines if the operating system is Windows 8 or later.
 *
 * Parameters: None
 *
 * Return Value: None (Harbour function, returns value via hb_retl)
 *
 * Explanation:
 *   - Retrieves the OS version information using GetVersionEx.
 *   - Checks the dwMajorVersion and dwMinorVersion fields to see if they match the values for Windows 8 or later.
 *   - Windows 8 has version 6.2, Windows 8.1 has version 6.3, and Windows 10 has version 10.0.
 *   - Returns .T. if it's Windows 8 or later, .F. otherwise.
 */
{
  OSVERSIONINFO osvi;
  getwinver( &osvi );
  hb_retl( (osvi.dwMajorVersion >= 6 && osvi.dwMinorVersion > 1) || osvi.dwMajorVersion == 10 );
}

HB_FUNC( GETWINVERSIONINFO )
/*
 * Purpose: Retrieves detailed operating system version information and returns it as an array.
 *
 * Parameters: None
 *
 * Return Value: None (Harbour function, returns value via hb_itemReturn)
 *   - Returns a Harbour array containing the following elements:
 *     - Element 1: dwMajorVersion (Numeric)
 *     - Element 2: dwMinorVersion (Numeric)
 *     - Element 3: dwBuildNumber (Numeric)
 *     - Element 4: dwPlatformId (Numeric)
 *     - Element 5: szCSDVersion (Character) - Service Pack information
 *
 * Explanation:
 *   - This function retrieves the OS version information using GetVersionEx and packages it into a Harbour array.
 *   - It allocates a new Harbour array with 5 elements.
 *   - It populates the array with the major version, minor version, build number, platform ID, and CSD version (service pack information).
 *   - For Windows 9x systems, it extracts the lower word of the build number.
 *   - Finally, it returns the array to the Harbour environment. The array is released after being returned.
 */
{
  OSVERSIONINFO osvi;
  PHB_ITEM pArray = hb_itemArrayNew( 5 );
  getwinver( &osvi );
  hb_itemPutNL( hb_arrayGetItemPtr( pArray, 1 ), osvi.dwMajorVersion );
  hb_itemPutNL( hb_arrayGetItemPtr( pArray, 2 ), osvi.dwMinorVersion );
  if ( osvi.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS )
  {
    osvi.dwBuildNumber = LOWORD( osvi.dwBuildNumber );
  }
  hb_itemPutNL( hb_arrayGetItemPtr( pArray, 3 ), osvi.dwBuildNumber  );
  hb_itemPutNL( hb_arrayGetItemPtr( pArray, 4 ), osvi.dwPlatformId   );
  hb_itemPutC(  hb_arrayGetItemPtr( pArray, 5 ), osvi.szCSDVersion   );
  hb_itemRelease( hb_itemReturn( pArray) );
}

HB_FUNC( ISMEDIACENTER )
/*
 * Purpose: Determines if the operating system is Windows Media Center.
 *
 * Parameters: None
 *
 * Return Value: None (Harbour function, returns value via hb_retl)
 *
 * Explanation:
 *   - This function uses the GetSystemMetrics API to check for the SM_MEDIACENTER flag.
 *   - If the flag is set, it indicates that the operating system is Windows Media Center.
 *   - Returns .T. if it's Windows Media Center, .F. otherwise.
 */
{
  if (GetSystemMetrics(SM_MEDIACENTER))
    hb_retl( TRUE );
  else
    hb_retl( FALSE );
}

#pragma ENDDUMP
