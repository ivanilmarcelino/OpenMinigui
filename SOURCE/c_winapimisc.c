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
    Copyright 1999-2025, https://harbour.github.io/

    "WHAT32"
    Copyright 2002 AJ Wos <andrwos@aust1.net>

    "HWGUI"
    Copyright 2001-2021 Alexander S.Kresin <alex@kresin.ru>

 ---------------------------------------------------------------------------*/
#include <mgdefs.h>

#if defined( _MSC_VER )
#pragma warning( disable : 4996 )
#endif
#include <commctrl.h>
#include <lmcons.h>
#include <shellapi.h>
#include <shlobj.h>
#include <shlwapi.h>
#include <wchar.h>

#include "hbapierr.h"
#include "hbapiitm.h"

#if defined( __XHARBOUR__ ) || ( __HARBOUR__ - 0 < 0x030200 )
#define HB_FILE_TYPE_MAX   128
#else

/* this has to be declared before hbapifs.h is included */
#define _HB_FILE_INTERNAL_
#endif
#include "hbapifs.h"
#include "inkey.ch"

#if defined( __XHARBOUR__ )
#define HB_LONGLONG  LONGLONG
extern HB_EXPORT void      hb_evalBlock0( PHB_ITEM pCodeBlock );
#endif
extern HB_EXPORT BOOL      Array2Rect( PHB_ITEM aRect, RECT *rc );
extern HB_EXPORT PHB_ITEM  Rect2Array( RECT *rc );
extern void                hmg_ErrorExit( LPCTSTR lpMessage, DWORD dwError, BOOL bExit );

typedef HMODULE ( __stdcall *SHGETFOLDERPATH ) ( HWND, int, HANDLE, DWORD, LPTSTR );

#ifdef UNICODE
LPWSTR   AnsiToWide( LPCSTR );
LPSTR    WideToAnsi( LPWSTR );
#endif
BOOL     SysRefresh( void );

// Minigui Resources control system
void     RegisterResource( HANDLE hResource, LPCSTR szType );

/*
   wapi_GetProcAddress

   Retrieves the address of an exported function or variable from the specified DLL.

   Parameters:
     hModule: A handle to the DLL module.
     lpProcName: The name of the function or variable, or the function's ordinal value.

   Returns:
     The address of the exported function or variable, or NULL if the function fails.
*/
HB_PTRUINT wapi_GetProcAddress( HMODULE hModule, LPCSTR lpProcName )
{
   FARPROC  pProc;

   pProc = GetProcAddress( hModule, lpProcName );
   return( HB_PTRUINT ) pProc;
}

/*
   WAITRUNPIPE

   Executes a command in a separate process and redirects its standard output and standard error to a file.

   This function is useful for running console applications and capturing their output for later analysis or display.

   Parameters:
     cCommand: The command line to execute.
     nShowWindow: The show window flag to pass to CreateProcess (e.g., SW_SHOWNORMAL, SW_HIDE).
     cFile: The name of the file to which the output will be written.

   Returns:
     None.  The function returns -1 if an error occurs during pipe creation or process execution.
*/
HB_FUNC( WAITRUNPIPE )
{
   STARTUPINFO          StartupInfo;
   PROCESS_INFORMATION  ProcessInfo;
   HANDLE               ReadPipeHandle;
   HANDLE               WritePipeHandle;  // not used here
   char                 *Data;

#ifndef UNICODE
   LPSTR                lpCommandLine = ( char * ) hb_parc( 1 );
#else
   LPWSTR               lpCommandLine = AnsiToWide( ( char * ) hb_parc( 1 ) );
#endif
   const char           *szFile = ( const char * ) hb_parc( 3 );
   HB_FHANDLE           nHandle;
   SECURITY_ATTRIBUTES  sa;

   ZeroMemory( &sa, sizeof( SECURITY_ATTRIBUTES ) );
   sa.nLength = sizeof( SECURITY_ATTRIBUTES );
   sa.bInheritHandle = 1;
   sa.lpSecurityDescriptor = NULL;

   memset( &StartupInfo, 0, sizeof( StartupInfo ) );
   memset( &ProcessInfo, 0, sizeof( ProcessInfo ) );

   if( !hb_fsFile( szFile ) )
   {
      nHandle = hb_fsCreate( szFile, 0 );
   }
   else
   {
      nHandle = hb_fsOpen( szFile, 2 );
      hb_fsSeek( nHandle, 0, 2 );
   }

   if( !CreatePipe( &ReadPipeHandle, &WritePipeHandle, &sa, 0 ) )
   {
      hb_retni( -1 );
      return;
   }

   ProcessInfo.hProcess = INVALID_HANDLE_VALUE;
   ProcessInfo.hThread = INVALID_HANDLE_VALUE;
   StartupInfo.dwFlags = STARTF_USESHOWWINDOW | STARTF_USESTDHANDLES;
   StartupInfo.wShowWindow = hmg_par_WORD( 2 );
   StartupInfo.hStdOutput = WritePipeHandle;
   StartupInfo.hStdError = WritePipeHandle;

   if( !CreateProcess( NULL, lpCommandLine, 0, 0, FALSE, CREATE_NEW_CONSOLE | NORMAL_PRIORITY_CLASS, 0, 0, &StartupInfo, &ProcessInfo ) )
   {
      hb_retni( -1 );
      return;
   }
   else
   {
#ifdef UNICODE
      hb_xfree( lpCommandLine );
#endif
   }

   Data = ( char * ) hb_xgrab( 1024 );
   for( ;; )
   {
      DWORD BytesRead;
      DWORD TotalBytes;
      DWORD BytesLeft;

      // Check for the presence of data in the pipe
      if( !PeekNamedPipe( ReadPipeHandle, Data, sizeof( Data ), &BytesRead, &TotalBytes, &BytesLeft ) )
      {
         hb_retni( -1 );
         return;
      }

      // If there is bytes, read them
      if( BytesRead )
      {
         if( !ReadFile( ReadPipeHandle, Data, sizeof( Data ) - 1, &BytesRead, NULL ) )
         {
            hb_retni( -1 );
            return;
         }

         Data[BytesRead] = TEXT( '\0' );
         hb_fsWriteLarge( nHandle, Data, BytesRead );
      }
      else
      {
         // Is the console app terminated?
         if( WaitForSingleObject( ProcessInfo.hProcess, 0 ) == WAIT_OBJECT_0 )
         {
            break;
         }
      }
   }

   CloseHandle( ProcessInfo.hThread );
   CloseHandle( ProcessInfo.hProcess );
   CloseHandle( ReadPipeHandle );
   CloseHandle( WritePipeHandle );
   hb_fsClose( nHandle );
   hb_xfree( Data );
}

/*
   COPYRTFTOCLIPBOARD

   Copies an RTF (Rich Text Format) string to the Windows clipboard.

   Parameters:
     cRtfText: The RTF string to copy to the clipboard.

   Returns:
     None.
*/
HB_FUNC( COPYRTFTOCLIPBOARD ) // CopyRtfToClipboard(cRtfText) store cRTFText in Windows clipboard
{
   HGLOBAL     hglbCopy;
   char        *lptstrCopy;
   UINT        cf;
   const char  *cStr = HB_ISCHAR( 1 ) ? hb_parc( 1 ) : "";
   int         nLen = ( int ) strlen( cStr );

   if( ( nLen == 0 ) || !OpenClipboard( GetActiveWindow() ) )
   {
      return;
   }

   // Get Clipboard format id for RTF.
   cf = RegisterClipboardFormat( TEXT( "Rich Text Format" ) );
   if( cf == 0 )
   {
      return;
   }

   EmptyClipboard();

   hglbCopy = GlobalAlloc( GMEM_MOVEABLE | GMEM_DDESHARE, ( nLen + 4 ) * sizeof( TCHAR ) );
   if( hglbCopy == NULL )
   {
      GlobalFree( hglbCopy );
      CloseClipboard();
      return;
   }

   lptstrCopy = ( char * ) GlobalLock( hglbCopy );
   memcpy( lptstrCopy, cStr, nLen * sizeof( TCHAR ) );
   lptstrCopy[nLen] = ( TCHAR ) 0;  // NULL character
   GlobalUnlock( hglbCopy );

   if( SetClipboardData( cf, hglbCopy ) )
   {
      CloseClipboard();
   }
}

/*
   COPYTOCLIPBOARD

   Copies a text string to the Windows clipboard.

   Parameters:
     cText: The text string to copy to the clipboard.
     nFormat: (Optional) The clipboard format. Defaults to CF_TEXT if not provided.

   Returns:
     None.
*/
HB_FUNC( COPYTOCLIPBOARD ) // CopyToClipboard(cText) store cText in Windows clipboard
{
   HGLOBAL     hglbCopy;
   char        *lptstrCopy;

   const char  *cStr = HB_ISCHAR( 1 ) ? hb_parc( 1 ) : "";
   int         nLen = ( int ) strlen( cStr );

   if( ( nLen == 0 ) || !OpenClipboard( GetActiveWindow() ) )
   {
      return;
   }

   EmptyClipboard();

   hglbCopy = GlobalAlloc( GMEM_DDESHARE, ( nLen + 1 ) * sizeof( TCHAR ) );
   if( hglbCopy == NULL )
   {
      GlobalFree( hglbCopy );
      CloseClipboard();
      return;
   }

   lptstrCopy = ( char * ) GlobalLock( hglbCopy );
   memcpy( lptstrCopy, cStr, nLen * sizeof( TCHAR ) );
   lptstrCopy[nLen] = ( TCHAR ) 0;  // null character
   GlobalUnlock( hglbCopy );

   if( SetClipboardData( HB_ISNUM( 2 ) ? hmg_par_UINT( 2 ) : CF_TEXT, hglbCopy ) )
   {
      CloseClipboard();
   }
}

/*
   RETRIEVETEXTFROMCLIPBOARD

   Retrieves text from the Windows clipboard.

   Parameters:
     None.

   Returns:
     The text string from the clipboard, or NULL if the clipboard is empty or does not contain text.
*/
HB_FUNC( RETRIEVETEXTFROMCLIPBOARD )
{
   HGLOBAL  hClipMem;
   LPSTR    lpClip;

   if( IsClipboardFormatAvailable( CF_TEXT ) && OpenClipboard( GetActiveWindow() ) )
   {
      hClipMem = GetClipboardData( CF_TEXT );
      if( hClipMem )
      {
         lpClip = ( LPSTR ) GlobalLock( hClipMem );
         if( lpClip )
         {
            hb_retc( lpClip );
            GlobalUnlock( hClipMem );
         }
         else
         {
            hb_retc( "" );
         }
      }
      else
      {
         hb_retc_null();
      }

      CloseClipboard();
   }
   else
   {
      hb_retc_null();
   }
}

/*
   CLEARCLIPBOARD

   Clears the contents of the Windows clipboard.

   Parameters:
     hWnd: The handle of the window to associate with the clipboard operation.

   Returns:
     TRUE if the clipboard was successfully cleared, FALSE otherwise.
*/
HB_FUNC( CLEARCLIPBOARD )
{
   if( OpenClipboard( hmg_par_raw_HWND( 1 ) ) )
   {
      EmptyClipboard();
      CloseClipboard();
      hb_retl( TRUE );
   }
   else
   {
      hb_retl( FALSE );
   }
}

/*
   GETBLUE

   Extracts the blue component from a COLORREF value.

   Parameters:
     color: The COLORREF value.

   Returns:
     The blue component (0-255).
*/
HB_FUNC( GETBLUE )
{
   hmg_ret_UINT( GetBValue( hmg_par_COLORREF( 1 ) ) );
}

/*
   GETRED

   Extracts the red component from a COLORREF value.

   Parameters:
     color: The COLORREF value.

   Returns:
     The red component (0-255).
*/
HB_FUNC( GETRED )
{
   hmg_ret_UINT( GetRValue( hmg_par_COLORREF( 1 ) ) );
}

/*
   GETGREEN

   Extracts the green component from a COLORREF value.

   Parameters:
     color: The COLORREF value.

   Returns:
     The green component (0-255).
*/
HB_FUNC( GETGREEN )
{
   hmg_ret_UINT( GetGValue( hmg_par_COLORREF( 1 ) ) );
}

/*
   GETKEYSTATE

   Retrieves the status of a specified virtual key.

   Parameters:
     nVirtKey: The virtual key code.

   Returns:
     The key state. The high-order bit is 1 if the key is down, 0 if it is up.
     The low-order bit is 1 if the key is toggled.
*/
HB_FUNC( GETKEYSTATE )
{
   hmg_ret_NI( GetKeyState( hb_parni( 1 ) ) );
}

/*
   GETASYNCKEYSTATE

   Retrieves the status of a specified virtual key asynchronously.

   Parameters:
     nVirtKey: The virtual key code.

   Returns:
     The key state. The high-order bit is 1 if the key is down, 0 if it is up.
*/
HB_FUNC( GETASYNCKEYSTATE )
{
   hmg_ret_NI( GetAsyncKeyState( hb_parni( 1 ) ) );
}

/*
   HMG_KEYBOARDCLEARBUFFER

   Clears the keyboard buffer by removing all pending keyboard messages.

   Parameters:
     None.

   Returns:
     None.
*/
HB_FUNC( HMG_KEYBOARDCLEARBUFFER )
{
   MSG   Msg;

   while( PeekMessage( &Msg, NULL, WM_KEYFIRST, WM_KEYLAST, PM_REMOVE ) );
}

/*
   HMG_MOUSECLEARBUFFER

   Clears the mouse buffer by removing all pending mouse messages.

   Parameters:
     None.

   Returns:
     None.
*/
HB_FUNC( HMG_MOUSECLEARBUFFER )
{
   MSG   Msg;

   while( PeekMessage( &Msg, NULL, WM_MOUSEFIRST, WM_MOUSELAST, PM_REMOVE ) );
}

#ifndef USER_TIMER_MINIMUM
#define USER_TIMER_MINIMUM 0x0000000A
#endif
#ifndef USER_TIMER_MAXIMUM
#define USER_TIMER_MAXIMUM 0x7FFFFFFF
#endif

/*
   INKEYGUI

   Waits for a key press or a specified time interval in a GUI environment.

   This function provides a way to pause execution and wait for user input (key press or mouse click)
   or for a timer to expire.  It's designed to be used in GUI applications where the standard
   INKEY() function might not work correctly due to the message loop.

   Parameters:
     nElapse: (Optional) The time interval to wait in milliseconds. If 0, waits indefinitely.
              Defaults to USER_TIMER_MINIMUM (10ms) if not specified.

   Returns:
     The virtual key code of the key that was pressed, or 0 if the timer expired.
     Returns K_LBUTTONDOWN or K_RBUTTONDOWN if a left or right mouse button was clicked, respectively.
*/
HB_FUNC( INKEYGUI )
{
   UINT     uElapse = hb_parnidef( 1, USER_TIMER_MINIMUM );
   UINT_PTR uTimer;
   MSG      Msg;
   BOOL     bRet, bBreak = FALSE;
   UINT     uRet = 0;

   if( uElapse == 0 )
   {
      uElapse = USER_TIMER_MAXIMUM;
   }

   uTimer = SetTimer( NULL, 0, uElapse, NULL );

   while( ( bRet = GetMessage( &Msg, NULL, 0, 0 ) ) != 0 )
   {
      if( bRet == -1 )
      {
         // handle the error and possibly exit
         hmg_ErrorExit( TEXT( "INKEYGUI" ), 0, TRUE );
      }
      else
      {
         switch( Msg.message )
         {
            case WM_KEYDOWN:
            case WM_SYSKEYDOWN:
               bBreak = TRUE;
               uRet = ( UINT ) Msg.wParam;
               break;

            case WM_TIMER:
               bBreak = ( Msg.wParam == uTimer );
               break;

            case WM_LBUTTONDOWN:
            case WM_RBUTTONDOWN:
               bBreak = TRUE;
               uRet = ( Msg.message == WM_LBUTTONDOWN ) ? K_LBUTTONDOWN : K_RBUTTONDOWN;
               PostMessage( Msg.hwnd, Msg.message, Msg.wParam, Msg.lParam );
               break;
         }
      }

      if( bBreak )
      {
         KillTimer( NULL, uTimer );
         break;
      }
      else
      {
         TranslateMessage( &Msg );  // Translates virtual key codes
         DispatchMessage( &Msg );   // Dispatches message to window
      }
   }

   hb_retns( uRet );
}

/*
   GETDC

   Retrieves a device context (DC) for a window.

   Parameters:
     hWnd: The handle of the window.

   Returns:
     The handle of the device context.
*/
HB_FUNC( GETDC )
{
   hmg_ret_raw_HDC( GetDC( hmg_par_raw_HWND( 1 ) ) );
}

/*
   RELEASEDC

   Releases a device context (DC), making it available for other applications to use.

   Parameters:
     hWnd: The handle of the window.
     hDC: The handle of the device context.

   Returns:
     TRUE if the DC was successfully released, FALSE otherwise.
*/
HB_FUNC( RELEASEDC )
{
   hb_retl( ReleaseDC( hmg_par_raw_HWND( 1 ), hmg_par_raw_HDC( 2 ) ) );
}

/*
   HIWORD

   Extracts the high-order word from a DWORD value.

   Parameters:
     dwValue: The DWORD value.

   Returns:
     The high-order word.
*/
HB_FUNC( HIWORD )
{
   hmg_ret_WORD( HIWORD( hmg_par_DWORD( 1 ) ) );
}

/*
   LOWORD

   Extracts the low-order word from a DWORD value.

   Parameters:
     dwValue: The DWORD value.

   Returns:
     The low-order word.
*/
HB_FUNC( LOWORD )
{
   hmg_ret_WORD( LOWORD( hmg_par_DWORD( 1 ) ) );
}

/*
   C_GETSPECIALFOLDER

   Retrieves the path of a special folder (e.g., My Documents, Program Files).

   Parameters:
     nFolder: The CSIDL value that identifies the special folder.

   Returns:
     The path of the special folder.
*/
HB_FUNC( C_GETSPECIALFOLDER ) // Contributed By Ryszard Ry ko
{
#ifdef UNICODE
   LPSTR          pStr;
#endif
   TCHAR          *lpBuffer = ( TCHAR * ) hb_xgrab( ( MAX_PATH + 1 ) * sizeof( TCHAR ) );
   LPITEMIDLIST   pidlBrowse;                   // PIDL selected by user
   hb_ret();

   if( SUCCEEDED( SHGetSpecialFolderLocation( GetActiveWindow(), hb_parni( 1 ), &pidlBrowse ) ) )
   {
      if( SHGetPathFromIDList( pidlBrowse, lpBuffer ) )
      {
#ifndef UNICODE
         hb_retc( lpBuffer );
#else
         pStr = hb_osStrU16Decode( lpBuffer );
         hb_retc( pStr );
         hb_xfree( pStr );
#endif
      }
   }

   hb_xfree( lpBuffer );
}

//#define __WIN98__
#ifdef __WIN98__

/*
   C_GETDLLSPECIALFOLDER

   Retrieves the path of a special folder using SHGetFolderPath from SHFolder.dll.

   This function is specifically designed for older systems (like Windows 98) where
   SHGetFolderPath is not directly available in the standard Shell API.

   Parameters:
     nFolder: The CSIDL value that identifies the special folder.

   Returns:
     The path of the special folder, or an empty string if the function fails.
*/
HB_FUNC( C_GETDLLSPECIALFOLDER )
{
   TCHAR    szPath[MAX_PATH];
   HMODULE  hModule = LoadLibrary( TEXT( "SHFolder.dll" ) );

   if( hModule )
   {
      SHGETFOLDERPATH   fnShGetFolderPath = ( SHGETFOLDERPATH ) wapi_GetProcAddress( hModule, "SHGetFolderPathA" );

      if( fnShGetFolderPath )
      {
         if( fnShGetFolderPath( NULL, hb_parni( 1 ), NULL, 0, szPath ) == S_OK )
         {
            hb_retc( szPath );
         }
         else
         {
            hb_retc( "" );
         }
      }

      FreeLibrary( hModule );
   }
}
#endif /* __WIN98__ */

// Memory Management Functions
typedef BOOL ( WINAPI *GetPhysicallyInstalledSystemMemory_ptr ) ( ULONGLONG * );

/*
   GETPHYSICALLYINSTALLEDSYSTEMMEMORY

   Retrieves the amount of physically installed system memory in kilobytes.

   Parameters:
     None.

   Returns:
     The amount of physically installed system memory in kilobytes, or 0 if the function fails.
*/
HB_FUNC( GETPHYSICALLYINSTALLEDSYSTEMMEMORY )
{
   HMODULE  hDll = GetModuleHandle( TEXT( "kernel32.dll" ) );

   hb_retnll( 0 );

   if( NULL != hDll )
   {
      GetPhysicallyInstalledSystemMemory_ptr fn_GetPhysicallyInstalledSystemMemory = ( GetPhysicallyInstalledSystemMemory_ptr ) wapi_GetProcAddress
         (
            hDll,
            "GetPhysicallyInstalledSystemMemory"
         );

      if( NULL != fn_GetPhysicallyInstalledSystemMemory )
      {
         ULONGLONG   ullTotalMemoryInKilobytes;

         if( fn_GetPhysicallyInstalledSystemMemory( &ullTotalMemoryInKilobytes ) )
         {
            hb_retnll( ( HB_LONGLONG ) ullTotalMemoryInKilobytes );
         }
      }
   }
}

typedef BOOL ( WINAPI *GlobalMemoryStatusEx_ptr ) ( MEMORYSTATUSEX * );
#define DIV ( 1024 * 1024 )

/*
   MEMORYSTATUS

   Retrieves information about the system's current memory usage.

   Parameters:
     nInfoType: An integer specifying the type of memory information to retrieve:
       1: Total physical memory (in MB).
       2: Available physical memory (in MB).
       3: Total page file size (in MB).
       4: Available page file size (in MB).
       5: Total virtual memory (in MB).
       6: Available virtual memory (in MB).

   Returns:
     The requested memory information in megabytes (MB).
*/
HB_FUNC( MEMORYSTATUS )
{
   HMODULE  hDll = GetModuleHandle( TEXT( "kernel32.dll" ) );

   HB_RETNL( 0 );

   if( NULL != hDll )
   {
      GlobalMemoryStatusEx_ptr   fn_GlobalMemoryStatusEx = ( GlobalMemoryStatusEx_ptr ) wapi_GetProcAddress( hDll, "GlobalMemoryStatusEx" );

      if( NULL != fn_GlobalMemoryStatusEx )
      {
         MEMORYSTATUSEX mstex;

         mstex.dwLength = sizeof( mstex );

         if( fn_GlobalMemoryStatusEx( &mstex ) )
         {
            switch( hb_parni( 1 ) )
            {
               case 1:
                  hb_retnll( mstex.ullTotalPhys / DIV );
                  break;

               case 2:
                  hb_retnll( mstex.ullAvailPhys / DIV );
                  break;

               case 3:
                  hb_retnll( mstex.ullTotalPageFile / DIV );
                  break;

               case 4:
                  hb_retnll( mstex.ullAvailPageFile / DIV );
                  break;

               case 5:
                  hb_retnll( mstex.ullTotalVirtual / DIV );
                  break;

               case 6:
                  hb_retnll( mstex.ullAvailVirtual / DIV );
                  break;
            }
         }
      }
      else
      {
         MEMORYSTATUS   mst;

         mst.dwLength = sizeof( MEMORYSTATUS );
         GlobalMemoryStatus( &mst );

         switch( hb_parni( 1 ) )
         {
            case 1:
               HB_RETNL( mst.dwTotalPhys / DIV );
               break;

            case 2:
               HB_RETNL( mst.dwAvailPhys / DIV );
               break;

            case 3:
               HB_RETNL( mst.dwTotalPageFile / DIV );
               break;

            case 4:
               HB_RETNL( mst.dwAvailPageFile / DIV );
               break;

            case 5:
               HB_RETNL( mst.dwTotalVirtual / DIV );
               break;

            case 6:
               HB_RETNL( mst.dwAvailVirtual / DIV );
               break;
         }
      }
   }
}

/*
   C_SHELLABOUT

   Displays the ShellAbout dialog box.

   Parameters:
     hWnd: The handle of the parent window.
     szApp: The application name.
     szOtherStuff: Additional information to display in the dialog box.
     hIcon: The handle of the icon to display in the dialog box.

   Returns:
     TRUE if the function succeeds, FALSE otherwise.
*/
HB_FUNC( C_SHELLABOUT )
{
#ifndef UNICODE
   LPCSTR   szApp = hb_parc( 2 );
   LPCSTR   szOtherStuff = hb_parc( 3 );
#else
   LPCWSTR  szApp = AnsiToWide( ( char * ) hb_parc( 2 ) );
   LPCWSTR  szOtherStuff = AnsiToWide( ( char * ) hb_parc( 3 ) );
#endif
   hb_retl( ShellAbout( hmg_par_raw_HWND( 1 ), szApp, szOtherStuff, hmg_par_raw_HICON( 4 ) ) );
#ifdef UNICODE
   hb_xfree( ( TCHAR * ) szApp );
   hb_xfree( ( TCHAR * ) szOtherStuff );
#endif
}

/*
   PAINTBKGND

   Paints the background of a window with a specified color or the default button face color.

   Parameters:
     hWnd: The handle of the window.
     aColor: (Optional) An array containing the RGB color values (e.g., {255, 0, 0} for red).
             If not provided, the background is painted with the default button face color.

   Returns:
     The handle of the brush used to paint the background.
*/
HB_FUNC( PAINTBKGND )
{
   HWND     hwnd;
   HBRUSH   hBrush;
   RECT     recClie;
   HDC      hdc;

   hwnd = hmg_par_raw_HWND( 1 );
   hdc = GetDC( hwnd );

   GetClientRect( hwnd, &recClie );

   if( hb_pcount() > 1 && !HB_ISNIL( 2 ) )
   {
      hBrush = CreateSolidBrush( RGB( HB_PARNI( 2, 1 ), HB_PARNI( 2, 2 ), HB_PARNI( 2, 3 ) ) );
      FillRect( hdc, &recClie, hBrush );
   }
   else
   {
      hBrush = ( HBRUSH ) ( COLOR_BTNFACE + 1 );
      FillRect( hdc, &recClie, hBrush );
   }

   ReleaseDC( hwnd, hdc );

   RegisterResource( hBrush, "BRUSH" );
   hmg_ret_raw_HBRUSH( hBrush );
}

/*
   GETWINDOWSDIR

   Retrieves the path of the Windows directory.

   Parameters:
     None.

   Returns:
     The path of the Windows directory.
*/
HB_FUNC( GETWINDOWSDIR )
{
   TCHAR szBuffer[MAX_PATH + 1] = { 0 };

#ifdef UNICODE
   LPSTR pStr;
#endif
   if( GetWindowsDirectory( szBuffer, MAX_PATH ) )
   {
#ifndef UNICODE
      hb_retc( szBuffer );
#else
      pStr = WideToAnsi( szBuffer );
      hb_retc( pStr );
      hb_xfree( pStr );
#endif
   }
}

/*
   GETSYSTEMDIR

   Retrieves the path of the system directory.

   Parameters:
     None.

   Returns:
     The path of the system directory.
*/
HB_FUNC( GETSYSTEMDIR )
{
   TCHAR szBuffer[MAX_PATH + 1] = { 0 };

#ifdef UNICODE
   LPSTR pStr;
#endif
   if( GetSystemDirectory( szBuffer, MAX_PATH ) )
   {
#ifndef UNICODE
      hb_retc( szBuffer );
#else
      pStr = WideToAnsi( szBuffer );
      hb_retc( pStr );
      hb_xfree( pStr );
#endif
   }
}

/*
   GETTEMPDIR

   Retrieves the path of the temporary directory.

   Parameters:
     None.

   Returns:
     The path of the temporary directory.
*/
HB_FUNC( GETTEMPDIR )
{
   TCHAR szBuffer[MAX_PATH + 1] = { 0 };

#ifdef UNICODE
   LPSTR pStr;
#endif
   if( GetTempPath( MAX_PATH, szBuffer ) )
   {
#ifndef UNICODE
      hb_retc( szBuffer );
#else
      pStr = WideToAnsi( szBuffer );
      hb_retc( pStr );
      hb_xfree( pStr );
#endif
   }
}

/*
   POSTMESSAGE

   Posts a message to the message queue of a specified window.

   Parameters:
     hWnd: The handle of the window.
     nMessage: The message to post.
     wParam: Additional message-specific information.
     lParam: Additional message-specific information.

   Returns:
     A value indicating the success or failure of the operation.
*/
HB_FUNC( POSTMESSAGE )
{
   hmg_ret_L( PostMessage( hmg_par_raw_HWND( 1 ), hmg_par_UINT( 2 ), hmg_par_WPARAM( 3 ), hmg_par_LPARAM( 4 ) ) != 0 );
}

/*
   DEFWINDOWPROC

   Calls the default window procedure to provide default processing for any window messages that an application does not process.
   This ensures that standard window behavior is maintained even if the application doesn't explicitly handle a specific message.

   Input Parameters:
     hWnd: HWND - The handle of the window receiving the message.
     nMessage: UINT - The message identifier (e.g., WM_PAINT, WM_CLOSE).
     wParam: WPARAM - Additional message-specific data.  The exact meaning depends on the message.
     lParam: LPARAM - Additional message-specific data.  The exact meaning depends on the message.

   Return Value:
     LRESULT - The result of the message processing.  The meaning of the return value depends on the message.
*/
HB_FUNC( DEFWINDOWPROC )
{
   hmg_ret_LRESULT( DefWindowProc( hmg_par_raw_HWND( 1 ), hmg_par_UINT( 2 ), hmg_par_WPARAM( 3 ), hmg_par_LPARAM( 4 ) ) );
}

/*
   GETSTOCKOBJECT

   Retrieves a handle to one of the predefined stock objects.  Stock objects are GDI objects (like brushes, pens, fonts) that are commonly used.
   Using stock objects avoids the need to create and manage these objects manually.

   Input Parameters:
     nIndex: int - The type of stock object to retrieve.  This is one of the predefined constants like BLACK_BRUSH, WHITE_PEN, etc.

   Return Value:
     HGDIOBJ - The handle of the stock object.  Returns NULL on failure.
*/
HB_FUNC( GETSTOCKOBJECT )
{
   hmg_ret_raw_HGDIOBJ( GetStockObject( hb_parni( 1 ) ) );
}

/*
   GETNEXTDLGTABITEM

   Retrieves a handle to the next control in a dialog box that has the WS_TABSTOP style.
   This function is used to implement tab-based navigation within a dialog.

   Input Parameters:
     hWndDlg: HWND - The handle of the dialog box.
     hWndCtl: HWND - The handle of the control from which to begin the search.
     bPrevious: BOOL - TRUE to search for the previous control; FALSE to search for the next control.

   Return Value:
     HWND - The handle of the next (or previous) control with the WS_TABSTOP style.  Returns NULL if no such control is found.
*/
HB_FUNC( GETNEXTDLGTABITEM )
{
   hmg_ret_raw_HWND( GetNextDlgTabItem( hmg_par_raw_HWND( 1 ), hmg_par_raw_HWND( 2 ), hb_parl( 3 ) ) );
}

// Define function pointer types for dynamic linking of Windows API functions.
typedef BOOL ( WINAPI *LPFN_ISWOW64PROCESS ) ( HANDLE, PBOOL );
typedef BOOL ( WINAPI *LPFN_WOW64DISABLEWOW64FSREDIRECTION ) ( PVOID * );
typedef BOOL ( WINAPI *LPFN_WOW64REVERTWOW64FSREDIRECTION ) ( PVOID );

/*
   SHELLEXECUTE

   Executes a specified program or opens a file using its associated application.
   This function provides a high-level way to launch processes and interact with the shell.

   Input Parameters:
     hWnd: HWND - A handle to a parent window.  This window receives any message boxes that the executed application produces.  Can be NULL.
     lpOperation: string - The operation to perform (e.g., "open", "print", "explore").  If NULL, the default operation is performed.
     lpFile: string - The file to execute or open.
     lpParameters: string - Parameters to pass to the executable.  If NULL, no parameters are passed.
     lpDirectory: string - The default directory.  If NULL, the current directory is used.
     nShowCmd: int - How the application should be displayed (e.g., SW_SHOWNORMAL, SW_MAXIMIZE).

   Return Value:
     HANDLE - If successful, returns a value greater than 32. If an error occurs, the function returns an error value that is less than or equal to 32.
              Use GetLastError() to get extended error information.
*/
HB_FUNC( SHELLEXECUTE )
{
#ifndef UNICODE
   LPCSTR                              lpOperation = hb_parc( 2 );
   LPCSTR                              lpFile = hb_parc( 3 );
   LPCSTR                              lpParameters = hb_parc( 4 );
   LPCSTR                              lpDirectory = hb_parc( 5 );
#else
   LPCWSTR                             lpOperation = AnsiToWide( ( char * ) hb_parc( 2 ) );
   LPCWSTR                             lpFile = AnsiToWide( ( char * ) hb_parc( 3 ) );
   LPCWSTR                             lpParameters = AnsiToWide( ( char * ) hb_parc( 4 ) );
   LPCWSTR                             lpDirectory = AnsiToWide( ( char * ) hb_parc( 5 ) );
#endif
   LPFN_ISWOW64PROCESS                 fnIsWow64Process;
   BOOL                                bIsWow64 = FALSE;
   LPFN_WOW64DISABLEWOW64FSREDIRECTION fnDisable;
   PVOID                               OldValue = NULL;
   BOOL                                bRestore = FALSE;
   LPFN_WOW64REVERTWOW64FSREDIRECTION  fnRevert;
   HMODULE                             hDll = GetModuleHandle( TEXT( "kernel32.dll" ) );
   HINSTANCE                           hInst;

   // Dynamically load IsWow64Process to check if the process is running under WOW64 (Windows 32-bit on Windows 64-bit).
   fnIsWow64Process = ( LPFN_ISWOW64PROCESS ) wapi_GetProcAddress( hDll, "IsWow64Process" );
   if( NULL != fnIsWow64Process )
   {
      fnIsWow64Process( GetCurrentProcess(), &bIsWow64 );
   }

   // If running under WOW64, disable file system redirection to access the native system directories.
   if( bIsWow64 )
   {
      fnDisable = ( LPFN_WOW64DISABLEWOW64FSREDIRECTION ) wapi_GetProcAddress( hDll, "Wow64DisableWow64FsRedirection" );
      if( NULL != fnDisable )
      {
         if( fnDisable( &OldValue ) )
         {
            bRestore = TRUE;
         }
      }
   }

   CoInitialize( NULL );                        // Initialize COM library for ShellExecute
   hInst = ShellExecute
      (
         hmg_par_raw_HWND( 1 ),
         HB_ISNIL( 2 ) ? NULL : lpOperation,
         lpFile,
         HB_ISNIL( 4 ) ? NULL : lpParameters,
         HB_ISNIL( 5 ) ? NULL : lpDirectory,
         hb_parni( 6 )
      );

   if( ( INT_PTR ) hInst <= SE_ERR_DLLNOTFOUND )
   {
      hb_ret();                                 // Return NULL on failure.
   }
   else
   {
      hmg_ret_raw_HANDLE( hInst );
   }

   hb_idleSleep( 1.0 );                         // Introduce a short delay to allow the launched process to initialize.

   // If file system redirection was disabled, restore it.
   if( bRestore )
   {
      fnRevert = ( LPFN_WOW64REVERTWOW64FSREDIRECTION ) wapi_GetProcAddress( hDll, "Wow64RevertWow64FsRedirection" );
      if( NULL != fnRevert )
      {
         fnRevert( OldValue );
      }
   }

#ifdef UNICODE
   hb_xfree( ( TCHAR * ) lpOperation );
   hb_xfree( ( TCHAR * ) lpFile );
   hb_xfree( ( TCHAR * ) lpParameters );
   hb_xfree( ( TCHAR * ) lpDirectory );
#endif
}

/*
   SHELLEXECUTEEX

   Executes a program using extended parameters, providing more control over the execution process.
   This function uses the SHELLEXECUTEINFO structure to specify various options.

   Input Parameters:
     hWnd: HWND - A handle to a parent window.  This window receives any message boxes that the executed application produces.  Can be NULL.
     lpOperation: string - The operation to perform (e.g., "open", "print", "explore").  If NULL, the default operation is performed.
     lpFile: string - The file to execute or open.
     lpParameters: string - Parameters to pass to the executable.  If NULL, no parameters are passed.
     lpDirectory: string - The default directory.  If NULL, the current directory is used.
     nShowCmd: int - How the application should be displayed (e.g., SW_SHOWNORMAL, SW_MAXIMIZE).

   Return Value:
     HANDLE - If successful, returns the process handle of the newly executed process. Returns NULL on failure.
*/
HB_FUNC( SHELLEXECUTEEX )
{
#ifndef UNICODE
   LPCSTR            lpOperation = hb_parc( 2 );
   LPCSTR            lpFile = hb_parc( 3 );
   LPCSTR            lpParameters = hb_parc( 4 );
   LPCSTR            lpDirectory = hb_parc( 5 );
#else
   LPCWSTR           lpOperation = AnsiToWide( ( char * ) hb_parc( 2 ) );
   LPCWSTR           lpFile = AnsiToWide( ( char * ) hb_parc( 3 ) );
   LPCWSTR           lpParameters = AnsiToWide( ( char * ) hb_parc( 4 ) );
   LPCWSTR           lpDirectory = AnsiToWide( ( char * ) hb_parc( 5 ) );
#endif
   SHELLEXECUTEINFO  SHExecInfo;
   ZeroMemory( &SHExecInfo, sizeof( SHExecInfo ) );

   SHExecInfo.cbSize = sizeof( SHExecInfo );
   SHExecInfo.fMask = SEE_MASK_NOCLOSEPROCESS;  // Request the process handle to be returned.
   SHExecInfo.hwnd = HB_ISNIL( 1 ) ? GetActiveWindow() : hmg_par_raw_HWND( 1 );  // Use active window if hWnd is NIL
   SHExecInfo.lpVerb = HB_ISNIL( 2 ) ? NULL : lpOperation;
   SHExecInfo.lpFile = lpFile;
   SHExecInfo.lpParameters = HB_ISNIL( 4 ) ? NULL : lpParameters;
   SHExecInfo.lpDirectory = HB_ISNIL( 5 ) ? NULL : lpDirectory;
   SHExecInfo.nShow = hb_parni( 6 );

   if( ShellExecuteEx( &SHExecInfo ) )
   {
      hmg_ret_raw_HWND( SHExecInfo.hProcess );  // Return the process handle.
   }
   else
   {
      hb_ret();         // Return NULL on failure.
   }

#ifdef UNICODE
   hb_xfree( ( TCHAR * ) lpOperation );
   hb_xfree( ( TCHAR * ) lpFile );
   hb_xfree( ( TCHAR * ) lpParameters );
   hb_xfree( ( TCHAR * ) lpDirectory );
#endif
}

/*
   WAITRUN

   Executes a command line and waits for the process to terminate.
   This function is useful for running external programs and ensuring they complete before continuing.

   Input Parameters:
     lpCommandLine: string - The command line to execute.
     nShowWindow: int - How the application should be displayed (e.g., SW_SHOWNORMAL, SW_MAXIMIZE).

   Return Value:
     DWORD - The exit code of the process.  Returns -1 if the process could not be created.
*/
HB_FUNC( WAITRUN )
{
   DWORD                dwExitCode;

#ifndef UNICODE
   LPSTR                lpCommandLine = ( char * ) hb_parc( 1 );
#else
   LPWSTR               lpCommandLine = AnsiToWide( ( char * ) hb_parc( 1 ) );
#endif
   STARTUPINFO          stInfo;
   PROCESS_INFORMATION  prInfo;
   BOOL                 bResult;

   ZeroMemory( &stInfo, sizeof( stInfo ) );

   stInfo.cb = sizeof( stInfo );

   stInfo.dwFlags = STARTF_USESHOWWINDOW;

   stInfo.wShowWindow = hmg_par_WORD( 2 );

   // Create the process with a new console and normal priority.
   bResult = CreateProcess( NULL, lpCommandLine, NULL, NULL, TRUE, CREATE_NEW_CONSOLE | NORMAL_PRIORITY_CLASS, NULL, NULL, &stInfo, &prInfo );

#ifdef UNICODE
   hb_xfree( lpCommandLine );
#endif
   if( !bResult )
   {
      hb_retni( -1 );   // Return -1 if process creation failed.
      return;
   }

   WaitForSingleObject( prInfo.hProcess, INFINITE );     // Wait indefinitely for the process to terminate.
   GetExitCodeProcess( prInfo.hProcess, &dwExitCode );   // Get the exit code of the process.
   CloseHandle( prInfo.hThread );   // Close the thread handle.
   CloseHandle( prInfo.hProcess );  // Close the process handle.
   hmg_ret_DWORD( dwExitCode );     // Return the exit code.
}

/*
   WAITRUNTERM

   Executes a command line and waits for the process to terminate, allowing for termination via a callback block.
   This function provides a mechanism to monitor the process and terminate it if necessary.

   Input Parameters:
     lpCommandLine: string - The command line to execute.
     lpCurrentDirectory: string - The current directory for the process.
     nShowWindow: int - How the application should be displayed (e.g., SW_SHOWNORMAL, SW_MAXIMIZE).
     pWaitProc: block - A code block to execute while waiting for the process to terminate.  The block should return .T. to continue waiting, .F. to terminate the process.
     ulWaitMsec: numeric - The number of milliseconds to wait before executing the callback block.

   Return Value:
     DWORD - The exit code of the process.  Returns -2 if the process could not be created, -1 if the process was terminated by the callback.
*/
HB_FUNC( WAITRUNTERM )
{
#ifndef UNICODE
   LPSTR                lpCommandLine = ( char * ) hb_parc( 1 );
   LPCSTR               lpCurrentDirectory = hb_parc( 2 );
#else
   LPWSTR               lpCommandLine = AnsiToWide( ( char * ) hb_parc( 1 ) );
   LPCWSTR              lpCurrentDirectory = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif
   PHB_ITEM             pWaitProc = hb_param( 4, HB_IT_BLOCK );
   ULONG                ulWaitMsec = ( HB_ISNIL( 5 ) ? 2000 : hb_parnl( 5 ) );   // Default wait time is 2000ms.
   BOOL                 bTerm = FALSE;
   BOOL                 bWait;
   ULONG                ulNoSignal;
   DWORD                dwExitCode;
   STARTUPINFO          stInfo;
   PROCESS_INFORMATION  prInfo;
   BOOL                 bResult;

   ZeroMemory( &stInfo, sizeof( stInfo ) );
   stInfo.cb = sizeof( stInfo );
   stInfo.dwFlags = STARTF_USESHOWWINDOW;
   stInfo.wShowWindow = HB_ISNIL( 3 ) ? ( WORD ) 5 : hmg_par_WORD( 3 );

   bResult = CreateProcess
      (
         NULL,
         lpCommandLine,
         NULL,
         NULL,
         TRUE,
         CREATE_NEW_CONSOLE | NORMAL_PRIORITY_CLASS,
         NULL,
         HB_ISNIL( 2 ) ? NULL : lpCurrentDirectory,
         &stInfo,
         &prInfo
      );

#ifdef UNICODE
   hb_xfree( lpCommandLine );
   hb_xfree( ( TCHAR * ) lpCurrentDirectory );
#endif
   if( !bResult )
   {
      hb_retni( -2 );   // Return -2 if process creation failed.
      return;
   }

   if( pWaitProc )
   {
      do
      {
         ulNoSignal = WaitForSingleObject( prInfo.hProcess, ulWaitMsec );  // Wait for the process or the timeout.
         if( ulNoSignal )                 // Timeout occurred.
         {
            hb_evalBlock0( pWaitProc );   // Execute the callback block.
            bWait = hb_parl( -1 );        // Get the return value from the callback block.
            if( !bWait )                  // Callback returned .F., terminate the process.
            {
               if( TerminateProcess( prInfo.hProcess, 0 ) != 0 )
               {
                  bTerm = TRUE;           // Process terminated successfully.
               }
               else
               {
                  bWait = TRUE;           // Termination failed, continue waiting.
               }
            }
         }
         else
         {
            bWait = FALSE;                // Process terminated normally.
         }
      }
      while( bWait );
   }
   else
   {
      WaitForSingleObject( prInfo.hProcess, INFINITE );  // Wait indefinitely if no callback is provided.
   }

   if( bTerm )
   {
      dwExitCode = ( DWORD ) - 1;   // Set exit code to -1 if terminated by the callback.
   }
   else
   {
      GetExitCodeProcess( prInfo.hProcess, &dwExitCode );   // Get the exit code of the process.
   }

   CloseHandle( prInfo.hThread );   // Close the thread handle.
   CloseHandle( prInfo.hProcess );  // Close the process handle.
   hmg_ret_DWORD( dwExitCode );     // Return the exit code.
}

/*
   ISEXERUNNING

   Checks if an executable is already running by attempting to create a named mutex.
   If the mutex already exists, it indicates that the executable is running.

   Input Parameters:
     cExeNameCaseSensitive: string - The name of the executable (case-sensitive).  This is used as the name of the mutex.

   Return Value:
     BOOL - TRUE if the executable is already running, FALSE otherwise.
*/
HB_FUNC( ISEXERUNNING ) // ( cExeNameCaseSensitive ) --> lResult
{
   HANDLE   hMutex = CreateMutex( NULL, FALSE, ( LPTSTR ) hb_parc( 1 ) );  // Create a named mutex.
   hb_retl( GetLastError() == ERROR_ALREADY_EXISTS ); // Check if the mutex already exists.
   if( hMutex != NULL )
   {
      ReleaseMutex( hMutex ); // Release the mutex if it was successfully created.
   }
}

/*
   SETSCROLLPOS

   Sets the position of the scroll box (thumb) in the specified scroll bar.

   Input Parameters:
     hWnd: HWND - Handle to the window containing the scroll bar.
     nBar: int - Specifies the scroll bar to be set. Can be SB_HORZ or SB_VERT.
     nPos: int - Specifies the new position of the scroll box.
     bRedraw: BOOL - Specifies whether the scroll bar should be redrawn to reflect the new position.

   Return Value:
     int - The previous position of the scroll box.
*/
HB_FUNC( SETSCROLLPOS )
{
   hmg_ret_NINT( SetScrollPos( hmg_par_raw_HWND( 1 ), hb_parni( 2 ), hb_parni( 3 ), hb_parl( 4 ) ) );
}

/*
   GETLASTERROR

   Retrieves the calling thread's last-error code value.
   This function is useful for determining the cause of a failure in a Windows API function.

   Input Parameters:
     None

   Return Value:
     DWORD - The last-error code value.
*/
HB_FUNC( GETLASTERROR )
{
   hmg_ret_DWORD( GetLastError() );
}

/*
   CREATEFOLDER

   Creates a new directory.

   Input Parameters:
     lpPathName: string - The path of the directory to create.

   Return Value:
     BOOL - TRUE if the directory was created successfully, FALSE otherwise.
*/
HB_FUNC( CREATEFOLDER )
{
#ifndef UNICODE
   LPCSTR   lpPathName = hb_parc( 1 );
#else
   LPCWSTR  lpPathName = AnsiToWide( hb_parc( 1 ) );
#endif
   hb_retl( CreateDirectory( lpPathName, NULL ) ); // Create the directory.
#ifdef UNICODE
   hb_xfree( ( TCHAR * ) lpPathName );
#endif
}

/*
   SETCURRENTFOLDER

   Sets the current directory for the current process.

   Input Parameters:
     lpPathName: string - The path of the directory to set as the current directory.

   Return Value:
     BOOL - TRUE if the current directory was set successfully, FALSE otherwise.
*/
HB_FUNC( SETCURRENTFOLDER )
{
#ifndef UNICODE
   LPCSTR   lpPathName = hb_parc( 1 );
#else
   LPCWSTR  lpPathName = AnsiToWide( hb_parc( 1 ) );
#endif
   hb_retl( SetCurrentDirectory( lpPathName ) );   // Set the current directory.
#ifdef UNICODE
   hb_xfree( ( TCHAR * ) lpPathName );
#endif
}

/*
   REMOVEFOLDER

   Deletes an existing directory.

   Input Parameters:
     lpPathName: string - The path of the directory to delete.

   Return Value:
     BOOL - TRUE if the directory was deleted successfully, FALSE otherwise.
*/
HB_FUNC( REMOVEFOLDER )
{
#ifndef UNICODE
   LPCSTR   lpPathName = hb_parc( 1 );
#else
   LPCWSTR  lpPathName = AnsiToWide( hb_parc( 1 ) );
#endif
   hb_retl( RemoveDirectory( lpPathName ) );       // Delete the directory.
#ifdef UNICODE
   hb_xfree( ( TCHAR * ) lpPathName );
#endif
}

/*
   GETCURRENTFOLDER

   Retrieves the current directory for the current process.

   Input Parameters:
     None

   Return Value:
     string - The path of the current directory.
*/
HB_FUNC( GETCURRENTFOLDER )
{
   TCHAR Path[MAX_PATH + 1] = { 0 };

#ifdef UNICODE
   LPSTR pStr;
#endif
   GetCurrentDirectory( MAX_PATH, Path );          // Get the current directory.
#ifndef UNICODE
   hb_retc( Path );           // Return the path as a string.
#else
   pStr = WideToAnsi( Path ); // Convert the wide string to an ANSI string.
   hb_retc( pStr );           // Return the ANSI string.
   hb_xfree( pStr );          // Free the allocated memory.
#endif
}

/*
   CREATESOLIDBRUSH

   Creates a solid-color brush.  A brush is a GDI object used to fill the interior of shapes.

   Input Parameters:
     nRed: int - The red component of the color (0-255).
     nGreen: int - The green component of the color (0-255).
     nBlue: int - The blue component of the color (0-255).

   Return Value:
     HBRUSH - The handle of the newly created brush.  Returns NULL on failure.
*/
HB_FUNC( CREATESOLIDBRUSH )
{
   HBRUSH   hBrush = CreateSolidBrush( RGB( hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ) ) ); // Create the solid brush.
   RegisterResource( hBrush, "BRUSH" );      // Register the brush for resource management.
   hmg_ret_raw_HBRUSH( hBrush );             // Return the brush handle.
}

/*
   SETTEXTCOLOR

   Sets the text color for the specified device context.

   Input Parameters:
     hDC: HDC - The handle of the device context.
     nRed: int - The red component of the color (0-255).
     nGreen: int - The green component of the color (0-255).
     nBlue: int - The blue component of the color (0-255).

   Return Value:
     COLORREF - The previous text color.
*/
HB_FUNC( SETTEXTCOLOR )
{
   hmg_ret_COLORREF( SetTextColor( hmg_par_raw_HDC( 1 ), RGB( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ) ) ) );
}

/*
   SETBKCOLOR

   Sets the background color for the specified device context.

   Input Parameters:
     hDC: HDC - The handle of the device context.
     nRed: int - The red component of the color (0-255).
     nGreen: int - The green component of the color (0-255).
     nBlue: int - The blue component of the color (0-255).

   Return Value:
     COLORREF - The previous background color.
*/
HB_FUNC( SETBKCOLOR )
{
   hmg_ret_COLORREF( SetBkColor( hmg_par_raw_HDC( 1 ), RGB( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ) ) ) );
}

/*
   GETSYSCOLOR

   Retrieves the color of the specified system color element.
   System color elements are colors used for various parts of the Windows interface (e.g., window background, button face).

   Input Parameters:
     nIndex: int - The index of the system color element to retrieve.  This is one of the predefined constants like COLOR_WINDOW, COLOR_BTNFACE, etc.

   Return Value:
     DWORD - The RGB color value of the system color element.
*/
HB_FUNC( GETSYSCOLOR )
{
   hmg_ret_DWORD( GetSysColor( hb_parni( 1 ) ) );
}

/*
   WINVERSION

   Retrieves detailed information about the operating system version.
   This function provides a more comprehensive way to determine the OS version than GetVersion.

   Input Parameters:
     None

   Return Value:
     array - A 4-element array containing the following information:
       [1]: string - The OS name (e.g., "Windows 10", "Windows Server 2019").
       [2]: string - The service pack or system release number.
       [3]: string - The build number.
       [4]: string - Extended OS information (e.g., "Professional", "Datacenter Edition").
*/
#if defined( __BORLANDC__ )
#ifndef VER_SUITE_PERSONAL
#define VER_SUITE_PERSONAL 0x00000200
#endif
#ifndef VER_SUITE_BLADE
#define VER_SUITE_BLADE 0x00000400
#endif
#endif
HB_FUNC( WINVERSION )
{
   typedef struct
   {
      DWORD       major;
      DWORD       minor;
      DWORD       buildMin, buildMax;
      BYTE        productType;               /* VER_NT_WORKSTATION or VER_NT_SERVER */
      const char  *name;
   } OSVERSIONMAP;

   static const OSVERSIONMAP  osTable[] =
   {
      /* Windows 11 / 10 / Server family */
      { 10, 0, 22000, 0xFFFFFFFF, VER_NT_WORKSTATION, "Windows 11" },
      { 10, 0, 10240, 21999, VER_NT_WORKSTATION, "Windows 10" },
      { 10, 0, 20348, 0xFFFFFFFF, VER_NT_SERVER, "Windows Server 2022" },
      { 10, 0, 17763, 20347, VER_NT_SERVER, "Windows Server 2019" },
      { 10, 0, 14393, 17762, VER_NT_SERVER, "Windows Server 2016" },

      /* Windows 8.x / Server 2012 */
      { 6, 3, 0, 0, VER_NT_WORKSTATION, "Windows 8.1" },
      { 6, 3, 0, 0, VER_NT_SERVER, "Windows Server 2012 R2" },
      { 6, 2, 0, 0, VER_NT_WORKSTATION, "Windows 8" },
      { 6, 2, 0, 0, VER_NT_SERVER, "Windows Server 2012" },

      /* Windows 7 / Server 2008 R2 */
      { 6, 1, 0, 0, VER_NT_WORKSTATION, "Windows 7" },
      { 6, 1, 0, 0, VER_NT_SERVER, "Windows Server 2008 R2" },

      /* Windows Vista / Server 2008 */
      { 6, 0, 0, 0, VER_NT_WORKSTATION, "Windows Vista" },
      { 6, 0, 0, 0, VER_NT_SERVER, "Windows Server 2008" },

      /* Windows XP / 2003 / 2000 */
      { 5, 2, 0, 0, VER_NT_WORKSTATION, "Windows XP x64 Edition" },
      { 5, 2, 0, 0, VER_NT_SERVER, "Windows Server 2003" },
      { 5, 1, 0, 0, VER_NT_WORKSTATION, "Windows XP" },
      { 5, 0, 0, 0, VER_NT_WORKSTATION, "Windows 2000" },
      { 5, 0, 0, 0, VER_NT_SERVER, "Windows 2000 Server" },

      /* NT4 and earlier */
      { 4, 0, 0, 0, VER_NT_WORKSTATION, "Windows NT Workstation 4.0" },
      { 4, 0, 0, 0, VER_NT_SERVER, "Windows NT Server 4.0" },
   };

   char                 osName[64] = "Unknown Operating System";
   char                 sp[128] = "";
   char                 buildStr[32] = "";
   char                 edition[64] = "";
   BOOL                 filled = FALSE;

   /* --- Preferred path: RtlGetVersion --- */
   HMODULE              hNtdll = GetModuleHandleA( "ntdll.dll" );
   if( hNtdll )
   {
      typedef LONG ( WINAPI *RtlGetVersionPtr ) ( OSVERSIONINFOEXW * );

      RtlGetVersionPtr  pRtlGetVersion = ( RtlGetVersionPtr ) wapi_GetProcAddress( hNtdll, "RtlGetVersion" );

      if( pRtlGetVersion )
      {
         OSVERSIONINFOEXW  osvi;
         ZeroMemory( &osvi, sizeof( osvi ) );
         osvi.dwOSVersionInfoSize = sizeof( osvi );

         if( pRtlGetVersion( &osvi ) == 0 )  /* STATUS_SUCCESS */
         {
            /* --- Lookup from table --- */
            HB_SIZE  i;
            for( i = 0; i < HB_SIZEOFARRAY( osTable ); i++ )
            {
               if( osvi.dwMajorVersion == osTable[i].major && osvi.dwMinorVersion == osTable[i].minor && osvi.wProductType == osTable[i].productType )
               {
                  if
                  (
                     osTable[i].buildMin == 0
                  || ( osvi.dwBuildNumber >= osTable[i].buildMin && osvi.dwBuildNumber <= ( osTable[i].buildMax ? osTable[i].buildMax : osvi.dwBuildNumber ) )
                  )
                  {
                     hb_strncpyTrim( osName, osTable[i].name, sizeof( osName ) );
                     break;
                  }
               }
            }

            /* --- Edition string --- */
            if( osvi.wProductType == VER_NT_WORKSTATION )
            {
               hb_strncpyTrim( edition, ( osvi.wSuiteMask & VER_SUITE_PERSONAL ) ? "Home Edition" : "Professional", sizeof( edition ) );
            }
            else
            {
               if( osvi.wSuiteMask & VER_SUITE_DATACENTER )
               {
                  hb_strncpyTrim( edition, "Datacenter Edition", sizeof( edition ) );
               }
               else if( osvi.wSuiteMask & VER_SUITE_ENTERPRISE )
               {
                  hb_strncpyTrim( edition, "Enterprise Edition", sizeof( edition ) );
               }
               else if( osvi.wSuiteMask & VER_SUITE_BLADE )
               {
                  hb_strncpyTrim( edition, "Web Edition", sizeof( edition ) );
               }
               else
               {
                  hb_strncpyTrim( edition, "Standard Edition", sizeof( edition ) );
               }
            }

            /* --- Service Pack string --- */
            if( osvi.wServicePackMajor > 0 )
            {
#if defined( __MINGW32__ )
               /* Pacified "ISO C does not support the '%S' ms_printf format" warning for MinGW compiler */
               char  csd[128] = "";
               WideCharToMultiByte( CP_UTF8, 0, osvi.szCSDVersion, -1, csd, sizeof( csd ), NULL, NULL );
               hb_snprintf( sp, sizeof( sp ), "Service Pack %d.%d (%s)", osvi.wServicePackMajor, osvi.wServicePackMinor, csd );
#else
               hb_snprintf( sp, sizeof( sp ), "Service Pack %d.%d (%S)", osvi.wServicePackMajor, osvi.wServicePackMinor, osvi.szCSDVersion );
#endif /* __MINGW32__ */
            }
            else
            {
               hb_strncpyTrim( sp, "No Service Pack", sizeof( sp ) );
            }

            /* --- Build string --- */
            {
               DWORD build = osvi.dwBuildNumber;
               if( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT && osvi.dwMajorVersion <= 4 )
               {
                  build &= 0xFFFF;      /* old NT style */
               }

               hb_snprintf( buildStr, sizeof( buildStr ), "%lu", ( unsigned long ) build );
            }

            filled = TRUE;
         }
      }
   }

   /* --- Fallback path: legacy OS (Windows 9x, very old NT) --- */
   if( !filled )
   {
      OSVERSIONINFOA osvi;
      ZeroMemory( &osvi, sizeof( osvi ) );
      osvi.dwOSVersionInfoSize = sizeof( osvi );

      if( !GetVersionExA( &osvi ) )
      {
         hb_reta( 4 );
         HB_STORC( osName, -1, 1 );
         HB_STORC( "", -1, 2 );
         HB_STORC( "", -1, 3 );
         HB_STORC( "", -1, 4 );
         return;
      }

      /* 9x family */
      if( osvi.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS )
      {
         if( osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 0 )
         {
            hb_strncpyTrim( osName, "Windows 95", sizeof( osName ) );
         }
         else if( osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 10 )
         {
            hb_strncpyTrim( osName, "Windows 98", sizeof( osName ) );
         }
         else if( osvi.dwMajorVersion == 4 && osvi.dwMinorVersion == 90 )
         {
            hb_strncpyTrim( osName, "Windows ME", sizeof( osName ) );
         }

         hb_snprintf( buildStr, sizeof( buildStr ), "%lu", ( unsigned long ) ( osvi.dwBuildNumber & 0xFFFF ) );
      }
      else if( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT )
      {
         hb_snprintf( osName, sizeof( osName ), "Windows NT %lu.%lu", ( unsigned long ) osvi.dwMajorVersion, ( unsigned long ) osvi.dwMinorVersion );
         hb_strncpyTrim( sp, osvi.szCSDVersion, sizeof( sp ) );
         hb_snprintf( buildStr, sizeof( buildStr ), "%lu", ( unsigned long ) ( osvi.dwBuildNumber & 0xFFFF ) );
      }
   }

   /* --- Returns { OS name, Service Pack / release, Build, Edition } as Harbour array --- */
   hb_reta( 4 );
   HB_STORC( osName, -1, 1 );
   HB_STORC( sp, -1, 2 );
   HB_STORC( buildStr, -1, 3 );
   HB_STORC( edition, -1, 4 );
}

#if defined( __XHARBOUR__ )

/*
   ISEXE64

   Checks if the application is running in 64-bit mode.

   Input Parameters:
     None

   Return Value:
     .T. (TRUE) if the application is 64-bit, .F. (FALSE) otherwise.
*/
HB_FUNC( ISEXE64 )   // Check if our app is 64 bits
{
   // The size of a pointer is 8 bytes in 64-bit mode and 4 bytes in 32-bit mode.
   hb_retl( ( sizeof( void * ) == 8 ) );
}
#endif

/*
   GETDLLVERSION

   Retrieves the version information of a DLL using the DllGetVersion function.

   Input Parameters:
     1: string - The file name of the DLL.

   Return Value:
     array - A 3-element array containing the following information:
       [1]: numeric - The major version number of the DLL.
       [2]: numeric - The minor version number of the DLL.
       [3]: numeric - The build number of the DLL.
*/
HB_FUNC( GETDLLVERSION )
{
   HMODULE  hModule;
   DWORD    dwMajorVersion = 0;
   DWORD    dwMinorVersion = 0;
   DWORD    dwBuildNumber = 0;

#ifndef UNICODE
   LPCSTR   lpLibFileName = hb_parc( 1 );                // Get the DLL file name from the first parameter (ANSI version).
#else
   LPCWSTR  lpLibFileName = AnsiToWide( hb_parc( 1 ) );  // Get the DLL file name from the first parameter and convert it to a wide string (Unicode version).
#endif

   // Load the specified DLL into memory.
   hModule = LoadLibrary( lpLibFileName );
   if( hModule )
   {
      // Define a pointer to the DllGetVersion function.
      DLLGETVERSIONPROC fnDllGetVersion;

      // Get the address of the DllGetVersion function from the loaded DLL.
      fnDllGetVersion = ( DLLGETVERSIONPROC ) wapi_GetProcAddress( hModule, "DllGetVersion" );

      if( fnDllGetVersion )
      {
         // Create a DLLVERSIONINFO structure to store the version information.
         DLLVERSIONINFO dvi = { 0 };

         // Set the size of the structure.
         dvi.cbSize = sizeof( dvi );

         // Call the DllGetVersion function to retrieve the version information.
         if( fnDllGetVersion( &dvi ) == S_OK )
         {
            // Store the major, minor, and build numbers from the DLLVERSIONINFO structure.
            dwMajorVersion = dvi.dwMajorVersion;
            dwMinorVersion = dvi.dwMinorVersion;
            dwBuildNumber = dvi.dwBuildNumber;
         }
      }
      else        // If the DllGetVersion function is not found.
      {
         hb_errRT_BASE_SubstR( EG_ARG, 3012, "Failed to get DllGetVersion function.", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      }

      // Free the loaded DLL from memory.
      FreeLibrary( hModule );
   }

   // Create an array with 3 elements to store the version information.
   hb_reta( 3 );

   // Store the major, minor, and build numbers in the array.
   HB_STORVNL( dwMajorVersion, -1, 1 );
   HB_STORVNL( dwMinorVersion, -1, 2 );
   HB_STORVNL( dwBuildNumber, -1, 3 );

#ifdef UNICODE
   // Free the allocated memory for the wide string (Unicode version).
   hb_xfree( ( TCHAR * ) lpLibFileName );
#endif
}

/*
   SELECTOBJECT

   Selects an object into a specified device context (DC).

   Input Parameters:
     1: HDC - Handle to the device context.
     2: HGDIOBJ - Handle to the object to be selected.

   Return Value:
     HGDIOBJ - Handle to the previously selected object.  This is important to save so you can restore the DC to its original state.
*/

// Jacek Kubica <kubica@wssk.wroc.pl> HMG 1.0 Experimental Build 9a
HB_FUNC( SELECTOBJECT )
{
   // Call the Windows API SelectObject to select the object into the device context.
   hmg_ret_raw_HGDIOBJ( SelectObject( hmg_par_raw_HDC( 1 ), hmg_par_raw_HGDIOBJ( 2 ) ) );
}

/*
   FILLRECT

   Fills a rectangle using the specified brush.

   Input Parameters:
     1: HWND or HDC - Handle to the window or device context. If a HWND is provided, the function obtains the DC.
     2: array or numeric - Either an array containing the RECT structure (left, top, right, bottom) or the left coordinate of the rectangle.
     3: numeric (optional) - Top coordinate of the rectangle (if using numeric parameters).
     4: numeric (optional) - Right coordinate of the rectangle (if using numeric parameters).
     5: numeric (optional) - Bottom coordinate of the rectangle (if using numeric parameters).
     6: HBRUSH (optional) - Handle to the brush to be used to fill the rectangle. If an array is passed as the second parameter, this is the third parameter.

   Return Value:
     NINT - Nonzero if the function is successful, zero otherwise.
*/
HB_FUNC( FILLRECT )
{
   HWND  hWnd = hmg_par_raw_HWND( 1 );
   HDC   hDC;
   BOOL  bDC = FALSE;

   // Check if the first parameter is a valid window handle.
   if( IsWindow( hWnd ) )
   {
      // If it's a window handle, get the device context for the window.
      hDC = GetDC( hWnd );
      bDC = TRUE; // Flag to indicate that the DC was obtained from a window.
   }
   else
   {
      // If it's not a window handle, assume it's a device context handle.
      hDC = hmg_par_raw_HDC( 1 );
   }

   // Verify that the handle is a valid device context.
   if( GetObjectType( ( HGDIOBJ ) hDC ) == OBJ_DC )
   {
      RECT  rc;
      int   iParam = 6;

      // Check if the second parameter is an array representing a RECT structure.
      if( Array2Rect( hb_param( 2, HB_IT_ANY ), &rc ) )
      {
         // If it's an array, adjust the parameter index for the brush.
         iParam = 3;
      }
      else
      {
         // If it's not an array, assume it's a set of numeric coordinates.
         rc.left = hb_parni( 2 );
         rc.top = hb_parni( 3 );
         rc.right = hb_parni( 4 );
         rc.bottom = hb_parni( 5 );
      }

      // Call the Windows API FillRect to fill the rectangle with the specified brush.
      hmg_ret_NINT( FillRect( hDC, &rc, hmg_par_raw_HBRUSH( iParam ) ) );

      // If the DC was obtained from a window, release the DC.
      if( bDC )
      {
         ReleaseDC( hWnd, hDC );
      }
   }
   else
   {
      // If the handle is not a valid device context, return 0.
      hb_retni( 0 );
   }
}

#if defined( __MINGW32__ )

// Disable strict aliasing warning for MinGW compiler.  This is needed because of the type casting used with FARPROC.
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstrict-aliasing"
#endif /* __MINGW32__ */

#if ( defined( __POCC__ ) && __POCC__ >= 900 )

// Define IN and OUT macros for Parameter passing for POCC compiler
#ifndef _NO_W32_PSEUDO_MODIFIERS
#define IN
#define OUT
#endif
#endif

/*
   IsAppHung

   Determines if a window is hung (unresponsive).

   Input Parameters:
     hWnd: HWND - Handle to the window to check.
     pbHung: PBOOL - Pointer to a BOOL variable to receive the result.

   Return Value:
     BOOL - TRUE if the function succeeds, FALSE otherwise.  Use GetLastError() to get the specific error.
*/
BOOL IsAppHung( IN HWND hWnd, OUT PBOOL pbHung )
{
   OSVERSIONINFO  osvi;
   HINSTANCE      hUser;

   // Check if the window handle is valid.
   if( !IsWindow( hWnd ) )
   {
      // If the window handle is invalid, set the last error and return FALSE.
      return SetLastError( ERROR_INVALID_PARAMETER ), FALSE;
   }

   // Initialize the OSVERSIONINFO structure.
   osvi.dwOSVersionInfoSize = sizeof( osvi );

   // detect OS version
   GetVersionEx( &osvi );

   // get handle of USER32.DLL
   hUser = GetModuleHandle( TEXT( "user32.dll" ) );

   // Different methods are used to detect hung applications based on the OS version.
   if( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT )
   {
      // For Windows NT-based systems, use the IsHungAppWindow function.
      BOOL ( WINAPI *_IsHungAppWindow ) ( HWND );

      // found the function IsHungAppWindow
      *( FARPROC * ) &_IsHungAppWindow = GetProcAddress( hUser, "IsHungAppWindow" );
      if( _IsHungAppWindow == NULL )
      {
         // If the IsHungAppWindow function is not found, set the last error and return FALSE.
         return SetLastError( ERROR_PROC_NOT_FOUND ), FALSE;
      }

      // call the function IsHungAppWindow
      *pbHung = _IsHungAppWindow( hWnd );
   }
   else
   {
      // For Windows 9x-based systems, use the IsHungThread function.
      DWORD dwThreadId = GetWindowThreadProcessId( hWnd, NULL );

      BOOL ( WINAPI *_IsHungThread ) ( DWORD );

      // found the function IsHungThread
      *( FARPROC * ) &_IsHungThread = GetProcAddress( hUser, "IsHungThread" );
      if( _IsHungThread == NULL )
      {
         // If the IsHungThread function is not found, set the last error and return FALSE.
         return SetLastError( ERROR_PROC_NOT_FOUND ), FALSE;
      }

      // call the function IsHungThread
      *pbHung = _IsHungThread( dwThreadId );
   }

   return TRUE;
}

#if defined( __MINGW32__ )

// Restore strict aliasing warning for MinGW compiler.
#pragma GCC diagnostic pop
#endif \
 \
   /* __MINGW32__ */

/*
   ISAPPHUNG

   Determines if a window is hung (unresponsive) and returns the result as a logical value.

   Input Parameters:
     1: HWND - Handle to the window to check.

   Return Value:
     .T. (TRUE) if the window is hung, .F. (FALSE) otherwise.  Displays a warning message if the process is not found.
*/
HB_FUNC( ISAPPHUNG )
{
   BOOL  bIsHung;

   // Call the IsAppHung function to determine if the window is hung.
   if( IsAppHung( hmg_par_raw_HWND( 1 ), &bIsHung ) )
   {
      // If the function succeeds, return the result as a logical value.
      hb_retl( bIsHung );
   }
   else
   {
      // If the function fails, check the last error.
      if( GetLastError() != ERROR_INVALID_PARAMETER )
      {
         // If the error is not ERROR_INVALID_PARAMETER, display a warning message.
         MessageBox( NULL, TEXT( "Process not found" ), TEXT( "Warning" ), MB_OK | MB_ICONWARNING );
      }

      // Return .F. (FALSE) if the function fails.
      hb_retl( HB_FALSE );
   }
}

#ifndef PROCESS_QUERY_LIMITED_INFORMATION

// Define PROCESS_QUERY_LIMITED_INFORMATION if it's not already defined.  This is used for OpenProcess on newer systems.
#define PROCESS_QUERY_LIMITED_INFORMATION ( 0x1000 )
#endif

/*
   EMPTYWORKINGSET

   Removes as many pages as possible from the process working set (clean the working set memory).
   This operation is useful primarily for testing and tuning.

   Input Parameters:
     1: DWORD (optional) - The process ID. If not specified, the current process ID is used.

   Return Value:
     lBoolean - .T. (TRUE) if the function is successful, .F. (FALSE) otherwise.
*/

// EmptyWorkingSet( [ ProcessID ] ) ---> lBoolean
HB_FUNC( EMPTYWORKINGSET )
{
   // It removes as many pages as possible from the process working set (clean the working set memory).
   // This operation is useful primarily for testing and tuning.
   DWORD    ProcessID;
   HANDLE   hProcess;

   // Define a function pointer for the EmptyWorkingSet function.
   typedef BOOL ( WINAPI *Func_EmptyWorkingSet ) ( HANDLE );

   // Static function pointer to avoid loading the library multiple times.
   static Func_EmptyWorkingSet   pEmptyWorkingSet = NULL;

   // Load the Kernel32.dll library and get the address of the K32EmptyWorkingSet function.
   if( pEmptyWorkingSet == NULL )
   {
      HMODULE  hLib = LoadLibrary( TEXT( "Kernel32.dll" ) );
      pEmptyWorkingSet = ( Func_EmptyWorkingSet ) wapi_GetProcAddress( hLib, "K32EmptyWorkingSet" );
   }

   // If K32EmptyWorkingSet is not found in Kernel32.dll, try loading Psapi.dll.
   if( pEmptyWorkingSet == NULL )
   {
      HMODULE  hLib = LoadLibrary( TEXT( "Psapi.dll" ) );
      pEmptyWorkingSet = ( Func_EmptyWorkingSet ) wapi_GetProcAddress( hLib, "K32EmptyWorkingSet" );
   }

   // If the EmptyWorkingSet function is found, call it.
   if( pEmptyWorkingSet != NULL )
   {
      // Get the process ID from the first parameter, or use the current process ID if no parameter is specified.
      ProcessID = HB_ISNUM( 1 ) ? hmg_par_DWORD( 1 ) : GetCurrentProcessId();

      // Open the process with the necessary permissions.
      hProcess = OpenProcess( PROCESS_QUERY_LIMITED_INFORMATION | PROCESS_SET_QUOTA, FALSE, ProcessID );
      if( hProcess != NULL )
      {
         // Call the EmptyWorkingSet function and return the result as a logical value.
         hb_retl( ( BOOL ) pEmptyWorkingSet( hProcess ) );

         // Close the process handle.
         CloseHandle( hProcess );
      }
      else
      {
         // Return .F. (FALSE) if the process could not be opened.
         hb_retl( FALSE );
      }
   }
   else
   {
      // Return .F. (FALSE) if the EmptyWorkingSet function could not be found.
      hb_retl( FALSE );
   }
}

/*
   CLEANPROGRAMMEMORY

   Reduces the working set size of the current process to a minimum.

   Input Parameters:
     None

   Return Value:
     lBoolean - .T. (TRUE) if the function is successful, .F. (FALSE) otherwise.
*/

// Grigory Filatov <gfilatov@gmail.com> HMG 1.1 Experimental Build 10d
HB_FUNC( CLEANPROGRAMMEMORY )
{
   // Call the SetProcessWorkingSetSize function to set the minimum and maximum working set sizes to -1, which effectively trims the working set.
   hb_retl( SetProcessWorkingSetSize( GetCurrentProcess(), ( SIZE_T ) - 1, ( SIZE_T ) - 1 ) );
}

/*
   GETCOMPACTPATH

   Converts a long file path to a compact form that fits within a specified width.

   Input Parameters:
     1: string - The buffer to receive the compact path.
     2: string - The long file path to compact.
     3: INT - The maximum number of characters to fit in the output buffer.
     4: DWORD - Flags that control the compacting process.

   Return Value:
     INT - The length of the compact path, or 0 if the function fails.
*/

// Grigory Filatov <gfilatov@gmail.com> HMG 1.1 Experimental Build 11a
typedef INT ( WINAPI *_GETCOMPACTPATH ) ( LPTSTR pszOut, LPTSTR pszSrc, INT cchMax, DWORD dwFlags );

HB_FUNC( GETCOMPACTPATH )
{
   HINSTANCE   handle = LoadLibrary( TEXT( "shlwapi.dll" ) );

   // Load the shlwapi.dll library.
   if( handle )
   {
      _GETCOMPACTPATH   pFunc;

      // Get the address of the PathCompactPathExA function.
      pFunc = ( _GETCOMPACTPATH ) wapi_GetProcAddress( handle, "PathCompactPathExA" );

      // Call the PathCompactPathExA function and return the result.
      hb_retni( pFunc( ( LPTSTR ) hb_parc( 1 ), ( LPTSTR ) hb_parc( 2 ), hmg_par_INT( 3 ), hmg_par_DWORD( 4 ) ) );

      // Free the loaded library.
      FreeLibrary( handle );
   }
}

/*
   GETSHORTPATHNAME

   Retrieves the short path name of a specified file.

   Parameters:
     1: cLongPath (STRING) - The long file path.

   Returns:
     (NUMERIC) - The length of the short path name. Returns 0 if the function fails.
                 The short path name itself is stored in parameter 2 (passed by reference).

   Purpose:
     This function is used to obtain the 8.3 short file name representation of a given long file name.
     This can be useful for compatibility with older systems or applications that do not support long file names.
*/

// Jacek Kubica <kubica@wssk.wroc.pl> HMG 1.1 Experimental Build 11a
HB_FUNC( GETSHORTPATHNAME )
{
   HB_SIZE  iRet;

#ifndef UNICODE
   char     buffer[MAX_PATH + 1] = { 0 }; // Buffer to store the short path name. Initialized to empty.
   LPCSTR   lpszLongPath = hb_parc( 1 );  // Pointer to the long path name passed as parameter 1.
#else
   TCHAR    buffer[MAX_PATH + 1] = { 0 }; // Buffer to store the short path name (Unicode version). Initialized to empty.
   LPCWSTR  lpszLongPath = AnsiToWide( ( char * ) hb_parc( 1 ) ); // Convert the ANSI long path to a wide character string.
   LPSTR    pStr; // Pointer to store the converted ANSI string from the wide character string.
#endif
   iRet = GetShortPathName( lpszLongPath, buffer, MAX_PATH );  // Call the Windows API to get the short path name.
   if( iRet < MAX_PATH )   // Check if the short path name was successfully retrieved and fits within the buffer.
   {
#ifndef UNICODE
#ifndef __XHARBOUR__
      hb_retni( hb_storclen( buffer, ( HB_SIZE ) iRet, 2 ) );  // Store the short path name in parameter 2 and return its length.
#else
      hb_storclen( buffer, ( HB_SIZE ) iRet, 2 );              // Store the short path name in parameter 2.
      hb_retni( iRet );             // Return the length of the short path name.
#endif
#else
      pStr = WideToAnsi( buffer );  // Convert the wide character short path name to an ANSI string.
      hb_retni( hb_storclen( pStr, ( HB_SIZE ) iRet, 2 ) ); // Store the ANSI short path name in parameter 2 and return its length.
      hb_xfree( pStr );                   // Free the memory allocated for the ANSI string.
#endif
   }
   else                                   // If GetShortPathName fails or the buffer is too small.
   {
#ifndef __XHARBOUR__
      hb_retni( hb_storc( "", 2 ) );      // Store an empty string in parameter 2 and return 0.
#else
      hb_storc( "", 2 );                  // Store an empty string in parameter 2.
      hb_retni( 0 );                      // Return 0 to indicate failure.
#endif
   }

#ifdef UNICODE
   hb_xfree( ( TCHAR * ) lpszLongPath );  // Free the memory allocated for the wide character long path name.
#endif
}

/*
   DRAWTEXT

   Draws formatted text in the specified rectangle.

   Parameters:
     1: hDC (HANDLE) - Handle to the device context.
     2: cText (STRING) - The text to be drawn.
     3: nLeft (NUMERIC) - The left coordinate of the rectangle.
     4: nTop (NUMERIC) - The top coordinate of the rectangle.
     5: nRight (NUMERIC) - The right coordinate of the rectangle.
     6: nBottom (NUMERIC) - The bottom coordinate of the rectangle.
     7: nFormat (NUMERIC) - The formatting options for the text (e.g., DT_CENTER, DT_WORDBREAK).

   Returns:
     None.

   Purpose:
     This function provides a way to draw text within a defined rectangular area, with control over formatting options
     such as alignment, word breaking, and clipping. It uses the Windows API DrawText function.
*/
HB_FUNC( DRAWTEXT )
{
#ifndef UNICODE
   LPCSTR   lpchText = hb_parc( 2 );      // Pointer to the text string (ANSI version).
#else
   LPCWSTR  lpchText = AnsiToWide( ( char * ) hb_parc( 2 ) );  // Pointer to the text string (Unicode version), converted from ANSI.
#endif
   RECT     rc;                     // Rectangle structure to define the drawing area.
   rc.left = hb_parni( 3 );         // Set the left coordinate of the rectangle.
   rc.top = hb_parni( 4 );          // Set the top coordinate of the rectangle.
   rc.right = hb_parni( 5 );        // Set the right coordinate of the rectangle.
   rc.bottom = hb_parni( 6 );       // Set the bottom coordinate of the rectangle.
   DrawText
   (
      hmg_par_raw_HDC( 1 ),         // device context - Handle to the device context obtained from parameter 1.
      lpchText,                     // pointer to string - Pointer to the text string to be drawn.
      ( int ) lstrlen( lpchText ),  // length of  string - Length of the text string.
      &rc,           // rectangle - Pointer to the rectangle structure defining the drawing area.
      hb_parni( 7 )  // draw style - Formatting options for the text.
   );

#ifdef UNICODE
   hb_xfree( ( TCHAR * ) lpchText );         // Free the memory allocated for the wide character text string.
#endif
}

/*
   GETTEXTMETRIC

   Retrieves the text metrics for the current font of the specified device context.

   Parameters:
     1: hDC (HANDLE) - Handle to the device context.

   Returns:
     (ARRAY) - An array containing the following text metrics:
       [1]: tmHeight - The height of characters (ascent + descent).
       [2]: tmAveCharWidth - The average width of characters.
       [3]: tmMaxCharWidth - The width of the widest character.
       [4]: tmAscent - The ascent of characters (units above the base line).
       [5]: tmDescent - The descent of characters (units below the base line).
       [6]: tmInternalLeading - The amount of leading inside the bounds set by tmHeight.
       [7]: tmExternalLeading - The amount of extra leading that the application adds between rows.

   Purpose:
     This function allows you to obtain detailed information about the current font used in a device context.
     This information can be used for precise text layout and measurement.
*/
HB_FUNC( GETTEXTMETRIC )
{
   TEXTMETRIC  tm;                           // Structure to store the text metrics.
   PHB_ITEM    aMetr = hb_itemArrayNew( 7 ); // Create a new Harbour array to store the text metrics.
   if
   (
      GetTextMetrics
         (
            hmg_par_raw_HDC( 1 ),            // handle of device context - Handle to the device context obtained from parameter 1.
            &tm   // address of text metrics structure - Address of the TEXTMETRIC structure to receive the text metrics.
         )
   )
   {
      //tmHeight
      //Specifies the height (ascent + descent) of characters.
      HB_arraySetNL( aMetr, 1, tm.tmHeight );            // Store the tmHeight value in the array at index 1.

      //tmAveCharWidth Specifies the average width of characters in the font
      //(generally defined as the width of the letter x).
      //This value does not include the overhang required for bold or italic characters.
      HB_arraySetNL( aMetr, 2, tm.tmAveCharWidth );      // Store the tmAveCharWidth value in the array at index 2.

      //tmMaxCharWidth
      //Specifies the width of the widest character in the font.
      HB_arraySetNL( aMetr, 3, tm.tmMaxCharWidth );      // Store the tmMaxCharWidth value in the array at index 3.

      //tmAscent
      //Specifies the ascent (units above the base line) of characters.
      HB_arraySetNL( aMetr, 4, tm.tmAscent );            // Store the tmAscent value in the array at index 4.

      //tmDescent
      //Specifies the descent (units below the base line) of characters.
      HB_arraySetNL( aMetr, 5, tm.tmDescent );           // Store the tmDescent value in the array at index 5.

      //tmInternalLeading
      //Specifies the amount of leading (space) inside the bounds set by the tmHeight member.
      //Accent marks and other diacritical characters may occur in this area.
      //The designer may set this member to zero.
      HB_arraySetNL( aMetr, 6, tm.tmInternalLeading );   // Store the tmInternalLeading value in the array at index 6.

      //tmExternalLeading
      //The amount of extra leading (space) that the application adds between rows.
      //Since this area is outside the font, it contains no marks and is not altered by text
      //output calls in either OPAQUE or TRANSPARENT mode.
      //The designer may set this member to zero.
      HB_arraySetNL( aMetr, 7, tm.tmExternalLeading );   // Store the tmExternalLeading value in the array at index 7.
   }

   hb_itemReturnRelease( aMetr );   // Return the array containing the text metrics and release the memory.
}

/*
   _GETCLIENTRECT

   Retrieves the coordinates of a window's client area. The client area excludes the window's title bar, borders, and scroll bars.

   Parameters:
     1: hWnd (HANDLE) - Handle to the window.

   Returns:
     (ARRAY) - An array containing the coordinates of the client rectangle: {Left, Top, Right, Bottom}.
               Returns an error if the window handle is invalid.

   Purpose:
     This function is used to determine the usable area within a window, excluding the window's frame and other non-client elements.
     This is useful for positioning controls and drawing within the window's content area.
*/
HB_FUNC( _GETCLIENTRECT )
{
   RECT  rc;   // Rectangle structure to store the client area coordinates.
   HWND  hWnd = hmg_par_raw_HWND( 1 );             // Handle to the window obtained from parameter 1.
   if( IsWindow( hWnd ) )                          // Check if the window handle is valid.
   {
      GetClientRect( hWnd, &rc );                  // Get the client rectangle coordinates of the window.
      hb_itemReturnRelease( Rect2Array( &rc ) );   // Convert the rectangle structure to a Harbour array and return it.
   }
   else  // If the window handle is invalid.
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, "MiniGUI Err.", HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );   // Raise a runtime error indicating an invalid argument.
   }
}

/*
   ISOEMTEXT

   Checks if a string contains OEM characters (characters with ASCII codes between 128 and 168).

   Parameters:
     1: cString (STRING) - The string to be checked.

   Returns:
     (LOGICAL) - .T. if the string contains OEM characters, .F. otherwise.

   Purpose:
     This function is useful for determining if a string contains characters that are specific to a particular OEM code page.
     This can be important for handling text encoding and display issues.
*/
HB_FUNC( ISOEMTEXT )
{
   LPBYTE   pString = ( LPBYTE ) hb_parc( 1 );        // Pointer to the string passed as parameter 1.
   WORD     w = 0, wLen = ( WORD ) hb_parclen( 1 );   // Initialize the loop counter and get the length of the string.
   BOOL     bOem = FALSE;     // Flag to indicate if an OEM character has been found.
   while( w < wLen && !bOem ) // Iterate through the string until an OEM character is found or the end of the string is reached.
   {
      bOem = pString[w] >= 128 && pString[w] <= 168;  // Check if the current character is an OEM character.
      w++;           // Increment the loop counter.
   }

   hb_retl( bOem );  // Return .T. if an OEM character was found, .F. otherwise.
}

/*
   GETOBJECTTYPE

   Identifies the type of the specified GDI object.

   Parameters:
     1: hGDIObject (HANDLE) - Handle to the GDI object.

   Returns:
     (NUMERIC) - A numeric code representing the object type. Possible values include:
       OBJ_PEN         1
       OBJ_BRUSH       2
       OBJ_DC          3
       OBJ_METADC      4
       OBJ_PAL         5
       OBJ_FONT        6
       OBJ_BITMAP      7
       OBJ_REGION      8
       OBJ_METAFILE    9
       OBJ_MEMDC       10
       OBJ_EXTPEN      11
       OBJ_ENHMETADC   12
       OBJ_ENHMETAFILE 13
       OBJ_COLORSPACE  14

   Purpose:
     This function allows you to determine the type of a GDI object based on its handle.
     This can be useful for validating object types before performing operations on them.
*/
HB_FUNC( GETOBJECTTYPE )
{
   hmg_ret_DWORD( GetObjectType( hmg_par_raw_HGDIOBJ( 1 ) ) ); // Call the Windows API GetObjectType and return the result as a numeric value.
}

/*
   DRAGACCEPTFILES

   Registers whether a window accepts dragged files.

   Parameters:
     1: hWnd (HANDLE) - Handle to the window.
     2: lAccept (LOGICAL) - .T. to accept dragged files, .F. to reject them.

   Returns:
     None.

   Purpose:
     This function enables or disables the ability of a window to receive files dragged from other applications.
     When a window accepts dragged files, it will receive WM_DROPFILES messages when files are dropped onto it.
*/
HB_FUNC( DRAGACCEPTFILES )
{
   DragAcceptFiles( hmg_par_raw_HWND( 1 ), hb_parl( 2 ) );     // Call the Windows API DragAcceptFiles to register the window for drag and drop.
}

/*
   DRAGQUERYFILES

   Retrieves the file names of dropped files from a HDROP handle.

   Parameters:
     1: hDrop (HANDLE) - Handle to the HDROP structure received in the WM_DROPFILES message.

   Returns:
     (ARRAY) - An array containing the file names of the dropped files. The first element of the array
               is the number of files dropped. Subsequent elements contain the file names.

   Purpose:
     This function is used to extract the file names from the HDROP handle that is passed to a window when files are dropped onto it.
     It allows the application to access the dropped files and perform operations on them.
*/
HB_FUNC( DRAGQUERYFILES )
{
   HDROP hDrop = hmg_par_raw_HDROP( 1 );  // Handle to the HDROP structure obtained from parameter 1.
   UINT  iFiles = DragQueryFile( hDrop, 0xFFFFFFFF, NULL, 0 ); // Get the number of files dropped.
   UINT  i;             // Loop counter.
   TCHAR bBuffer[250];  // Buffer to store the file name.
#ifdef UNICODE
   LPSTR pStr;          // Pointer to store the converted ANSI string from the wide character string.
#endif
   hb_reta( iFiles );   // Create a new Harbour array with the size equal to the number of files.
   for( i = 0; i < iFiles; i++ ) // Iterate through the dropped files.
   {
      DragQueryFile( hDrop, i, ( TCHAR * ) bBuffer, 249 );  // Get the file name of the i-th dropped file.
#ifndef UNICODE
      HB_STORC( ( TCHAR * ) bBuffer, -1, i + 1 );           // Store the file name in the array at index i+1.
#else
      pStr = WideToAnsi( bBuffer );             // Convert the wide character file name to an ANSI string.
      HB_STORC( pStr, -1, i + 1 );              // Store the ANSI file name in the array at index i+1.
      hb_xfree( pStr );                         // Free the memory allocated for the ANSI string.
#endif
   }
}

/*
   DRAGFINISH

   Releases the HDROP handle after processing dropped files.

   Parameters:
     1: hDrop (HANDLE) - Handle to the HDROP structure.

   Returns:
     None.

   Purpose:
     This function should be called after processing the dropped files to release the HDROP handle and free the associated resources.
     Failing to call this function can lead to memory leaks.
*/
HB_FUNC( DRAGFINISH )
{
   DragFinish( hmg_par_raw_HDROP( 1 ) );        // Call the Windows API DragFinish to release the HDROP handle.
}

/*
   HMG_CHARSETNAME

   Returns the character set name used by the application (ANSI or UNICODE).

   Parameters:
     None.

   Returns:
     (STRING) - "UNICODE" if the application is compiled with Unicode support, "ANSI" otherwise.

   Purpose:
     This function allows you to determine at runtime whether the application is using ANSI or Unicode character encoding.
     This can be useful for handling text encoding and display issues.
*/
HB_FUNC( HMG_CHARSETNAME )
{
#ifdef UNICODE
   hb_retc( WideToAnsi( TEXT( "UNICODE" ) ) );  // Return "UNICODE" if the application is compiled with Unicode support.
#else
   hb_retc( "ANSI" );                                 // Return "ANSI" if the application is compiled without Unicode support.
#endif
}

/*
   HMG_GETLOCALEINFO

   Retrieves locale information for the user's default locale.

   Parameters:
     1: nLCType (NUMERIC) - The type of locale information to retrieve (e.g., LOCALE_SCOUNTRY, LOCALE_SLANGUAGE).
                             See the Windows API documentation for GetLocaleInfo for a list of valid values.

   Returns:
     (STRING) - The requested locale information as a string.

   Purpose:
     This function allows you to access various locale settings, such as the country code, language name, date format, and currency symbol.
     This information can be used to adapt the application's behavior to the user's locale.
*/
HB_FUNC( HMG_GETLOCALEINFO )
{
   INT      LCType = hb_parni( 1 );                   // The type of locale information to retrieve.
#ifndef UNICODE
   LPSTR    cText;                                    // Pointer to store the locale information string (ANSI version).
#else
   LPWSTR   cText;                                    // Pointer to store the locale information string (Unicode version).
   LPSTR    pStr;                                     // Pointer to store the converted ANSI string from the wide character string.
#endif
   cText = ( LPTSTR ) hb_xgrab( HB_FILE_TYPE_MAX );   // Allocate memory for the locale information string.
   GetLocaleInfo( LOCALE_USER_DEFAULT, LCType, cText, HB_FILE_TYPE_MAX );  // Call the Windows API GetLocaleInfo to retrieve the locale information.
#ifdef UNICODE
   pStr = WideToAnsi( cText );                     // Convert the wide character locale information string to an ANSI string.
   hb_retc( pStr );                                // Return the ANSI locale information string.
   hb_xfree( pStr );                               // Free the memory allocated for the ANSI string.
#else
   hb_retc( cText );                               // Return the ANSI locale information string.
#endif
   hb_xfree( cText );                              // Free the memory allocated for the locale information string.
}

/*
   CreateShortCut (static function)

   Creates a Windows shortcut (.lnk) file.

   Parameters:
     pszTargetfile    (STRING) - File name of the link's target (the application or file the shortcut points to).  Must be a non-empty string.
     pszTargetargs    (STRING) - Command line arguments passed to the link's target. May be an empty string.
     pszLinkfile      (STRING) - File name of the actual link file (.lnk). Must be a non-empty string.
     pszDescription   (STRING) - Description of the linked item (the text displayed when hovering over the shortcut). If this is an empty string, the description is not set.
     iShowmode        (NUMERIC) - ShowWindow() constant for the link's target:
                        1 (SW_SHOWNORMAL) = Normal window.
                        3 (SW_SHOWMAXIMIZED) = Maximized.
                        7 (SW_SHOWMINNOACTIVE) = Minimized.
                      If zero, the showmode is not set (the target will use its default show mode).
     pszCurdir        (STRING) - Working directory of the active link. If this is an empty string, the directory is not set.
     pszIconfile      (STRING) - File name of the icon file used for the link. If this is an empty string, the icon is not set.
     iIconindex       (NUMERIC) - Index of the icon in the icon file. If < 0, the icon is not set.
     wHotKey          (NUMERIC) - The virtual key code is in the low-order byte, and the modifier flags are in the high-order byte.  Used to assign a hotkey to the shortcut.

   Returns:
     (NUMERIC) - HRESULT value. >= 0 for success, < 0 for failure.

   Purpose:
     This function encapsulates the COM API calls required to create a Windows shortcut file.  It creates an IShellLink object,
     sets its properties based on the input parameters, and then saves it to disk as a .lnk file.  It handles both ANSI and Unicode builds.
*/
#ifndef UNICODE
static HRESULT CreateShortCut
   (
      LPSTR pszTargetfile, LPSTR pszTargetargs, LPSTR pszLinkfile, LPSTR pszDescription, int iShowmode, LPSTR pszCurdir, LPSTR pszIconfile, int iIconindex, WORD
         wHotKey
   )
#else
static HRESULT CreateShortCut
   (
      LPWSTR pszTargetfile, LPWSTR pszTargetargs, LPWSTR pszLinkfile, LPWSTR pszDescription, int iShowmode, LPWSTR pszCurdir, LPWSTR pszIconfile, int iIconindex,
         WORD wHotKey
   )
#endif
{
   HRESULT        hRes = E_INVALIDARG;             // Default return value for invalid arguments.  Initialized to an error code.
   IShellLink     *pShellLink = NULL;              // Pointer to IShellLink object (COM interface for creating shortcuts). Initialized to NULL.
   IPersistFile   *pPersistFile = NULL;            // Pointer to IPersistFile object (COM interface for saving files). Initialized to NULL.
   WCHAR          wszLinkfile[MAX_PATH] = { 0 };   // Buffer for wide-char link file name (required for IPersistFile->Save). Initialized to empty.

   // Validate mandatory parameters: target file and link file must be non-empty strings.
   if( pszTargetfile && *pszTargetfile && pszLinkfile && *pszLinkfile )
   {
      // Create IShellLink instance using CoCreateInstance.
      hRes = CoCreateInstance( &CLSID_ShellLink, NULL, CLSCTX_INPROC_SERVER, &IID_IShellLink, ( LPVOID * ) &pShellLink );

      if( SUCCEEDED( hRes ) && pShellLink )        // Check if the IShellLink object was successfully created.
      {
         // Set link properties using the IShellLink interface.
         pShellLink->lpVtbl->SetPath( pShellLink, pszTargetfile );      // Set the target file path.
         pShellLink->lpVtbl->SetArguments( pShellLink, pszTargetargs ); // Set the command line arguments.
         if( pszDescription && *pszDescription )   // If a description is provided.
         {
            pShellLink->lpVtbl->SetDescription( pShellLink, pszDescription ); // Set the description.
         }
         if( iShowmode > 0 )  // If a show mode is specified.
         {
            pShellLink->lpVtbl->SetShowCmd( pShellLink, iShowmode ); // Set the show mode.
         }
         if( pszCurdir && *pszCurdir ) // If a working directory is provided.
         {
            pShellLink->lpVtbl->SetWorkingDirectory( pShellLink, pszCurdir );             // Set the working directory.
         }
         if( pszIconfile && *pszIconfile && iIconindex >= 0 )                             // If an icon file and index are provided.
         {
            pShellLink->lpVtbl->SetIconLocation( pShellLink, pszIconfile, iIconindex );   // Set the icon location.
         }
         if( wHotKey != 0 )   // If a hotkey is provided.
         {
            pShellLink->lpVtbl->SetHotkey( pShellLink, wHotKey ); // Set the hotkey.
         }

         // Save the link using IPersistFile.  First, get the IPersistFile interface from the IShellLink object.
         hRes = pShellLink->lpVtbl->QueryInterface( pShellLink, &IID_IPersistFile, ( LPVOID * ) &pPersistFile );

         if( SUCCEEDED( hRes ) && pPersistFile )                  // Check if the IPersistFile interface was successfully obtained.
         {
#ifndef UNICODE
            MultiByteToWideChar( CP_ACP, 0, pszLinkfile, -1, wszLinkfile, MAX_PATH );  // Convert the ANSI link file name to a wide character string.
#else
            lstrcpy( wszLinkfile, pszLinkfile );   // Copy the Unicode link file name to the wide character buffer.
#endif
            hRes = pPersistFile->lpVtbl->Save( pPersistFile, wszLinkfile, TRUE );   // Save the shortcut file.
            pPersistFile->lpVtbl->Release( pPersistFile );  // Release the IPersistFile object.
         }

         // Release IShellLink object.
         pShellLink->lpVtbl->Release( pShellLink );
      }
   }
   return hRes;            // Return the HRESULT of the operation.
}

/*
   C_CREATELINK

   Creates a Windows shortcut (.lnk) file.  This is the Harbour-callable function.

   Parameters:
     1: szTargetfile    (STRING) - File name of the link's target (the application or file the shortcut points to).
     2: szTargetargs    (STRING) - Command line arguments passed to the link's target.
     3: szLinkfile      (STRING) - File name of the actual link file (.lnk).
     4: szDescription   (STRING) - Description of the linked item (the text displayed when hovering over the shortcut).
     5: iShowmode        (NUMERIC) - ShowWindow() constant for the link's target:
                        1 (SW_SHOWNORMAL) = Normal window.
                        3 (SW_SHOWMAXIMIZED) = Maximized.
                        7 (SW_SHOWMINNOACTIVE) = Minimized.
     6: szCurdir        (STRING) - Working directory of the active link.
     7: szIconfile      (STRING) - File name of the icon file used for the link.
     8: iIconindex       (NUMERIC) - Index of the icon in the icon file.
     9: uVirtualKeyCode (NUMERIC) - Virtual key code for the hotkey.
    10: uModifiers      (NUMERIC) - Modifier flags for the hotkey (e.g., Ctrl, Shift, Alt).

   Returns:
     (NUMERIC) - HRESULT value. >= 0 for success, < 0 for failure.

   Purpose:
     This function provides a Harbour interface to the CreateShortCut function, allowing you to create Windows shortcut files from Harbour code.
     It handles parameter parsing, COM initialization, and error handling.
*/
#if defined( __BORLANDC__ ) && !defined( _WIN64 )
#pragma warn - prc         /* suggest parentheses to clarify precedence */
#endif
HB_FUNC( C_CREATELINK )
{
   int      iShowmode;     /* <Showmode> (optional) */
   int      iIconindex;    /* <Iconindex> (optional) */
   WORD     wHotKey;       /* Virtual key code (optional) */
   BYTE     uVirtualKeyCode;
   BYTE     uModifiers;    /* Modifier flags */
   HRESULT  hRes;          /* Result of calling COM functions */

#ifndef UNICODE
   LPSTR    szTargetfile;  /* <Targetfile> */
   LPSTR    szTargetargs;  /* <Targetargs> */
   LPSTR    szLinkfile;    /* <Linkfile> */
   LPSTR    szDescription; /* <Description> */
   LPSTR    szCurdir;      /* <Curdir> (optional) */
   LPSTR    szIconfile;    /* <Iconfile> (optional) */

   // Assign parameters with fallback values for optional fields
   szTargetfile = ( char * ) hb_parc( 1 );
   szTargetargs = HB_ISCHAR( 2 ) ? ( char * ) hb_parc( 2 ) : "";
   szLinkfile = ( char * ) hb_parc( 3 );
   szDescription = HB_ISCHAR( 4 ) ? ( char * ) hb_parc( 4 ) : "";
   szCurdir = HB_ISCHAR( 6 ) ? ( char * ) hb_parc( 6 ) : "";
   szIconfile = HB_ISCHAR( 7 ) ? ( char * ) hb_parc( 7 ) : "";
#else
   LPWSTR   szTargetfile;  /* <Targetfile> */
   LPWSTR   szTargetargs;  /* <Targetargs> */
   LPWSTR   szLinkfile;    /* <Linkfile> */
   LPWSTR   szDescription; /* <Description> */
   LPWSTR   szCurdir;      /* <Curdir> (optional) */
   LPWSTR   szIconfile;    /* <Iconfile> (optional) */

   // Convert ANSI strings to wide strings for Unicode builds
   szTargetfile = AnsiToWide( ( char * ) hb_parc( 1 ) );
   szTargetargs = HB_ISCHAR( 2 ) ? AnsiToWide( ( char * ) hb_parc( 2 ) ) : TEXT( "" );
   szLinkfile = AnsiToWide( ( char * ) hb_parc( 3 ) );
   szDescription = HB_ISCHAR( 4 ) ? AnsiToWide( ( char * ) hb_parc( 4 ) ) : TEXT( "" );
   szCurdir = HB_ISCHAR( 6 ) ? AnsiToWide( ( char * ) hb_parc( 6 ) ) : TEXT( "" );
   szIconfile = HB_ISCHAR( 7 ) ? AnsiToWide( ( char * ) hb_parc( 7 ) ) : TEXT( "" );
#endif

   // Retrieve optional numeric parameters with defaults
   iShowmode = hb_parnidef( 5, 0 );
   iIconindex = hb_parnidef( 8, 0 );

   // Retrieve virtual key and modifier byte parameters for hotkey
   uVirtualKeyCode = hmg_par_BYTE( 9 );
   uModifiers = hmg_par_BYTE( 10 );
   wHotKey = MAKEWORD( uVirtualKeyCode, uModifiers );

   // Initialize COM library
   hRes = CoInitialize( NULL );
   if( SUCCEEDED( hRes ) )
   {
      // Call CreateShortCut to create the shortcut file
      hRes = CreateShortCut
         (
            szTargetfile,  /* Target file */
            szTargetargs,  /* Target arguments */
            szLinkfile,    /* Shortcut filename */
            szDescription, /* Shortcut description */
            iShowmode,     /* Showmode constant */
            szCurdir,      /* Working directory for linked file */
            szIconfile,    /* Icon file shown for the link */
            iIconindex,    /* Index of icon in the file */
            wHotKey        /* Virtual key code */
         );

      // Return the HRESULT of the operation
      hmg_ret_HRESULT( hRes );

      // Uninitialize COM library
      CoUninitialize();
   }
   else
   {
      // Return the HRESULT if COM initialization failed
      hmg_ret_HRESULT( hRes );
   }
}
