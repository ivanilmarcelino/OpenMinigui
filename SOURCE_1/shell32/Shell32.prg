/*
 ===========================================================================
 Shell32.prg      (c) 2004 Grigory Filatov (Refactored 2025)
 ===========================================================================

   Created   : 08.09.04
   Extended  : 28.04.07
   Section   : Shell Extensions

   This library provides an interface for working with Windows ShellAPI
   functions, allowing for the following operations:
   · Drag-and-drop functionality
   · File association to open files with the correct applications
   · Extraction of icons from executable files
   · File operations (copy, move, delete, rename) via Explorer-like UI

 */

#include "Shell32.ch"

/*
 ===========================================================================
   Converts an array of strings to a null-terminated string required by ShellAPI
 ===========================================================================
 */
STATIC FUNCTION ArrayToShellString( aList )

   LOCAL cResult := ""

   IF HB_ISARRAY( aList )
      AEval( aList, {| x | cResult += x + Chr( 0 ) } )
   ELSEIF HB_ISSTRING( aList )
      cResult := aList
   ELSE
      RETURN Chr( 0 ) + Chr( 0 )
   ENDIF

RETURN cResult + Chr( 0 )

/*
 ===========================================================================
 Function: ShellFiles( hWnd, aSource, aTarget, nOperation, nFlags )

 Description:
  Performs a file system operation (copy, move, delete, rename) on one or more
  files or folders using the Windows ShellAPI function.

 Parameters:
  - hWnd    : Handle to the parent window (default is the active window).
  - aFiles  : A string representing the source file(s) or an array of file names.
  - aTarget : A string representing the target file(s) or an array of file names (optional).
  - nOperation : Operation to perform: FO_MOVE, FO_COPY, FO_DELETE, FO_RENAME.
  - nFlags  : Operation flags, such as FOF_NOCONFIRMATION, FOF_ALLOWUNDO, etc.

 Returns:
  - The result of the Shell operation: 0 if successful, non-zero otherwise.
 ===========================================================================
 */
FUNCTION ShellFiles( hWnd, aSource, aTarget, nOperation, nFlags )

   LOCAL cSource := ArrayToShellString( aSource )
   LOCAL cTarget := ArrayToShellString( aTarget )

   hb_default( @hWnd, GetActiveWindow() )

   __defaultNIL( @nOperation, FO_DELETE )
   __defaultNIL( @nFlags, FOF_ALLOWUNDO )

RETURN ShellFileOperation( hWnd, cSource, cTarget, nOperation, nFlags )

/*
 ===========================================================================
 Function: SHFolderDelete( hWnd, acFolder, lSilent )

 Description:
  This function deletes a folder (or multiple folders) and all its contents
  (files and subdirectories) using the Windows ShellAPI function.

 Parameters:
  - hWnd     : Handle to the parent window (default is the active window).
  - acFolder : A string representing the folder name or an array of folder names.
  - lSilent  : If TRUE (default), no confirmation dialogs are shown.

 Returns:
  - TRUE if the deletion succeeds, FALSE otherwise.
 ===========================================================================
 */
FUNCTION SHFolderDelete( hWnd, acFolder, lSilent )

   LOCAL nFlags := 0

   IF hb_defaultValue( lSilent, .T. )
      nFlags := hb_bitOr( FOF_NOCONFIRMATION, FOF_SILENT ) // No confirmation and silent operation
   ENDIF

RETURN ( ShellFiles( hWnd, acFolder, NIL, FO_DELETE, nFlags ) == 0 )

/*
 ===========================================================================
 Function: SHFileDelete( hWnd, aFiles, lRecycle )

 Description:
  Deletes one or more files using the Windows ShellAPI function. If `lRecycle`
  is TRUE (default), files are moved to the Recycle Bin; otherwise, they are
  permanently deleted.

 Parameters:
  - hWnd    : Handle to the parent window (default is the active window).
  - acFiles : A string representing a file name or an array of file names.
  - lRecycle: If TRUE (default), files are sent to the Recycle Bin.

 Returns:
  - TRUE if the deletion succeeds, FALSE otherwise.
 ===========================================================================
 */
FUNCTION SHFileDelete( hWnd, acFiles, lRecycle )

   LOCAL nFlags := 0

   IF hb_defaultValue( lRecycle, .T. )
      nFlags := FOF_ALLOWUNDO // Allow undo (send to Recycle Bin)
   ENDIF

RETURN ( ShellFiles( hWnd, acFiles, NIL, FO_DELETE, nFlags ) == 0 )

/*
 ===========================================================================
 C level binding
 ===========================================================================
 */
#pragma BEGINDUMP

#include <mgdefs.h>
#include <shellapi.h>

#ifdef UNICODE
   LPWSTR AnsiToWide( LPCSTR );  // Helper function to convert ANSI strings to Unicode
#endif

/*
 ===========================================================================
 Internal Function: ShellFileOperation

 Description:
  This is a low-level interface to the Windows SHFileOperation function,
  which performs various file operations like copy, move, delete, and rename.

 Parameters:
  - hWnd   : Handle to the parent window.
  - acFiles: Source file(s) as a null-terminated string.
  - acTarget: Target file(s) as a null-terminated string (optional).
  - wFunc  : The operation to be performed (FO_MOVE, FO_COPY, etc.).
  - fFlag  : Operation flags for the Shell function (e.g., FOF_NOCONFIRMATION).

 Returns:
  - Result of the SHFileOperation function: 0 for success, non-zero otherwise.
 ===========================================================================
 */
HB_FUNC ( SHELLFILEOPERATION )
{

#ifndef UNICODE
   LPCSTR lpFrom = ( LPCSTR ) hb_parc( 2 );   // Source file(s)
   LPCSTR lpTo = ( LPCSTR ) hb_parc( 3 );     // Target file(s)
#else
   LPCWSTR lpFrom = AnsiToWide( ( char * ) hb_parc( 2 ) );  // Convert source to Unicode
   LPCWSTR lpTo = AnsiToWide( ( char * ) hb_parc( 3 ) );    // Convert target to Unicode
#endif

   SHFILEOPSTRUCT sh;  // Define the structure for the SHFileOperation call

   // Set the parameters for SHFileOperation
   sh.hwnd   = hmg_par_raw_HWND( 1 );         // Parent window handle
   sh.pFrom  = lpFrom;                        // Source file(s)
   sh.pTo    = lpTo;                          // Target file(s)
   sh.wFunc  = hmg_par_UINT( 4 );             // Operation function (move, copy, delete, etc.)
   sh.fFlags = hmg_par_WORD( 5 );             // Flags for the operation
   sh.hNameMappings = 0;                      // No name mappings
   sh.lpszProgressTitle = NULL;               // No progress dialog title

   // Perform the file operation
   hmg_ret_NINT( SHFileOperation( &sh ) );    // Return the result

#ifdef UNICODE
   hb_xfree( ( TCHAR * ) lpFrom );  // Free the memory allocated for the Unicode string
   hb_xfree( ( TCHAR * ) lpTo );    // Free the memory allocated for the Unicode string
#endif
}

#pragma ENDDUMP
