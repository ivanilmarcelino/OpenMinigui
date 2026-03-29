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

#ifdef __XHARBOUR__
   #define __SYSDATA__
#endif

#include 'minigui.ch'
#include "i_winuser.ch"

// The Windows Color Dialog manages a palette of 16 custom colors.
#define MAX_CUSTOM_COLORS 16

/*
 * Function: GetColor
 * Purpose:  Displays the standard Windows Color Selection dialog.
 * 
 * Parameters:
 *    aInitColor    - Array {R, G, B} representing the initial color selected.
 *    aCustomColors - Array of up to 16 RGB values (numeric or {R,G,B} arrays) 
 *                    to populate the "Custom Colors" section.
 *    nFlags        - Numeric bitmask for dialog behavior, or Logical to use defaults.
 * 
 * Returns:
 *    An array {R, G, B} of the selected color, or NIL if the user cancels.
 * 
 * Implementation Note:
 *    HMG Extended uses {R,G,B} arrays for colors, but the underlying Windows 
 *    API uses COLORREF (numeric). This function handles the translation.
 */
FUNCTION GetColor( aInitColor, aCustomColors, nFlags )

   LOCAL aRetVal[3]
   LOCAL nColor, nInitColor, i

   // Convert HMG RGB array to numeric COLORREF for the API call.
   IF IsArrayRGB ( aInitColor )
      nInitColor := RGB ( aInitColor[1], aInitColor[2], aInitColor[3] )
   ENDIF

   // Ensure the custom colors array is properly formatted for the API.
   // The API expects a pointer to an array of 16 COLORREF values.
   IF ISARRAY ( aCustomColors )
      ASize ( aCustomColors, MAX_CUSTOM_COLORS )
      FOR i := 1 TO MAX_CUSTOM_COLORS
          IF IsArrayRGB ( aCustomColors[i] )
             // Convert nested arrays to numeric RGB.
             aCustomColors [i] := RGB ( aCustomColors[i][1], aCustomColors[i][2], aCustomColors[i][3] )
          ELSEIF ! ISNUMERIC ( aCustomColors[i] )
             // Default to the system's button face color if the entry is invalid.
             aCustomColors [i] := GetSysColor ( COLOR_BTNFACE )
          ENDIF
      NEXT
   ENDIF

   // Logic for nFlags:
   // If nFlags is .F., we apply a standard set of flags to prevent full opening 
   // and initialize the RGB values. If .T. or NIL, we let the API use defaults.
   IF ISLOGICAL( nFlags )
      nFlags := iif( nFlags, NIL, hb_BitOr( CC_ANYCOLOR, CC_PREVENTFULLOPEN, CC_RGBINIT ) )
   ENDIF

   // ChooseColor is the low-level HMG wrapper for the WinAPI ChooseColor function.
   IF ( nColor := ChooseColor ( NIL, nInitColor, @aCustomColors, nFlags ) ) != -1
      // Convert the numeric result back to HMG's {R,G,B} array format.
      aRetVal := nRGB2Arr( nColor )
   ENDIF

RETURN aRetVal

/*
 * Function: GetFolder
 * Purpose:  Invokes the modern "Browse for Folder" dialog.
 * 
 * Parameters:
 *    cTitle           - Text displayed in the dialog instructions.
 *    cInitPath        - The directory the dialog should start at.
 *    nFlags           - Configuration flags (e.g., BIF_RETURNONLYFSDIRS).
 *    lNewFolderButton - If .T., shows the "Make New Folder" button (Windows XP+).
 *    nFolderType      - CSIDL constant for special folders (e.g., CSIDL_DESKTOP).
 * 
 * Returns:
 *    String containing the selected path, or an empty string if cancelled.
 */
FUNCTION GetFolder( cTitle, cInitPath, nFlags, lNewFolderButton, nFolderType )

   // BIF_USENEWUI: Provides the resizable dialog with drag-and-drop and new folder button.
   // BIF_VALIDATE: Sends messages to the callback to validate the selection.
   LOCAL nDefaultFlags := hb_BitOr( BIF_USENEWUI, BIF_VALIDATE )

   hb_default( @nFlags, nDefaultFlags )
   hb_default( @lNewFolderButton, .T. )
   hb_default( @cInitPath, "" )

   IF ! lNewFolderButton
      nFlags := hb_BitOr( nFlags, BIF_NONEWFOLDERBUTTON )
   ENDIF

   // C_BrowseForFolder is a C-level wrapper for the SHBrowseForFolder Shell API.
RETURN C_BrowseForFolder( NIL, cTitle, nFlags, nFolderType, cInitPath )

/*
 * Function: BrowseForFolder
 * Purpose:  A simplified or legacy-style wrapper for folder selection.
 * 
 * Parameters:
 *    nFolderType - CSIDL constant.
 *    nFlags      - Dialog configuration flags.
 *    cTitle      - Dialog title.
 *    cInitPath   - Starting directory.
 * 
 * Implementation Note:
 *    Uses BIF_NEWDIALOGSTYLE to ensure the modern Windows look and feel.
 */
FUNCTION BrowseForFolder( nFolderType, nFlags, cTitle, cInitPath )

   LOCAL nDefaultFlags := hb_BitOr( BIF_NEWDIALOGSTYLE, BIF_EDITBOX, BIF_VALIDATE )

RETURN C_BrowseForFolder( NIL, cTitle, hb_defaultValue( nFlags, nDefaultFlags ), nFolderType, cInitPath )

#ifndef __XHARBOUR__

#include "hbwin.ch"

/*
 * Function: GetFile
 * Purpose:  Displays the "Open File" dialog.
 * 
 * Parameters:
 *    acFilter           - Array of pairs: { {"Description", "*.ext"}, ... }
 *    cTitle             - Dialog window title.
 *    cInitDir           - Starting directory.
 *    lMultiSelect       - Allow selecting multiple files.
 *    lNoChangeDirectory - If .T., the OS won't change the process's current directory.
 *    nFilterIndex       - The 1-based index of the filter to show by default.
 * 
 * Returns:
 *    If lMultiSelect is .F.: A string with the full file path.
 *    If lMultiSelect is .T.: An array of strings containing full file paths.
 */
FUNCTION GetFile( acFilter, cTitle, cInitDir, lMultiSelect, lNoChangeDirectory, nFilterIndex )

   LOCAL nFlags := WIN_OFN_EXPLORER // Use the modern Explorer-style dialog.
   LOCAL cRet, aTmp, aResult := {}
   LOCAL i

   hb_default( @lMultiSelect, .F. )
   hb_default( @lNoChangeDirectory, .F. )

   // Bitwise addition of flags based on logical parameters.
   IF lMultiSelect
      nFlags := hb_BitOr( nFlags, WIN_OFN_ALLOWMULTISELECT )
   ENDIF

   IF lNoChangeDirectory
      nFlags := hb_BitOr( nFlags, WIN_OFN_NOCHANGEDIR )
   ENDIF

   // win_GetOpenFileName is the Harbour-native wrapper for GetOpenFileName API.
   cRet := win_GetOpenFileName( @nFlags, cTitle, cInitDir, /*cDefExt*/, BuildFilterString( acFilter ), @nFilterIndex, /*nBufferSize*/, /*cDefName*/ )

   // Handle Multi-Select logic:
   // When multiple files are selected, the API returns a string where the first 
   // part is the directory, followed by null-separated filenames.
   IF hb_bitAnd( nFlags, WIN_OFN_ALLOWMULTISELECT ) != 0

      IF ! Empty( aTmp := hb_ATokens( cRet, Chr( 0 ) ) )
         IF Len( aTmp ) == 1
            // Only one file selected in multi-select mode.
            aResult := { aTmp[1] }
         ELSE
            // Multiple files: aTmp[1] is the path, aTmp[2..N] are filenames.
            FOR i := 2 TO Len( aTmp )
               AAdd( aResult, aTmp[1] + "\" + aTmp[i] )
            NEXT
         ENDIF
      ENDIF

      RETURN aResult

   ENDIF

   // Single file selection returns the full path directly.
RETURN cRet

/*
 * Function: PutFile
 * Purpose:  Displays the "Save File" dialog.
 * 
 * Parameters:
 *    acFilter         - Array of file filters.
 *    cTitle           - Dialog title.
 *    cInitDir         - Starting directory.
 *    lNoChangeCurDir  - Prevent OS from changing the current working directory.
 *    cDefName         - Default filename to suggest.
 *    nFilterIndex     - Default filter index.
 *    lPromptOverwrite - If .T., warns the user if the file already exists.
 * 
 * Returns:
 *    String containing the selected path/filename, or empty if cancelled.
 */
FUNCTION PutFile( acFilter, cTitle, cInitDir, lNoChangeCurDir, cDefName, nFilterIndex, lPromptOverwrite )

   LOCAL nFlags := WIN_OFN_EXPLORER
   LOCAL cRet, cDefExt := ""

   hb_default( @nFilterIndex, 1 )
   hb_default( @lNoChangeCurDir, .F. )
   hb_default( @lPromptOverwrite, .F. )

   IF lNoChangeCurDir
      nFlags := hb_BitOr( nFlags, WIN_OFN_NOCHANGEDIR )
   ENDIF

   IF lPromptOverwrite
      nFlags := hb_BitOr( nFlags, WIN_OFN_OVERWRITEPROMPT )
   ENDIF

   // win_GetSaveFileName is the Harbour-native wrapper for GetSaveFileName API.
   cRet := win_GetSaveFileName( @nFlags, cTitle, cInitDir, cDefExt, BuildFilterString( acFilter ), @nFilterIndex, /*nBufferSize*/, cDefName )

RETURN cRet

/*
 * Helper Function: BuildFilterString
 * Purpose: Constructs the null-delimited filter string for file dialogs.
 */
STATIC FUNCTION BuildFilterString( acFilter )
   LOCAL cFilter := ""

   IF ISARRAY( acFilter )
      AEval( acFilter, {| x | cFilter += x[1] + Chr( 0 ) + x[2] + Chr( 0 ) } )
      cFilter += Chr( 0 )
   ENDIF

RETURN cFilter

#else

/*
 *  File Open/Save Dialog Constants (xHarbour / Legacy Support)
 *  These define the behavior of the common dialogs at the bit level.
 */
#define OFN_READONLY                      1
#define OFN_OVERWRITEPROMPT               2
#define OFN_HIDEREADONLY                  4
#define OFN_NOCHANGEDIR                   8
#define OFN_SHOWHELP                     16
#define OFN_ENABLEHOOK                   32
#define OFN_ENABLETEMPLATE               64
#define OFN_ENABLETEMPLATEHANDLE        128
#define OFN_NOVALIDATE                  256
#define OFN_ALLOWMULTISELECT            512
#define OFN_EXTENSIONDIFFERENT         1024
#define OFN_PATHMUSTEXIST              2048
#define OFN_FILEMUSTEXIST              4096
#define OFN_CREATEPROMPT               8192
#define OFN_SHAREAWARE                16384
#define OFN_NOREADONLYRETURN          32768
#define OFN_NOTESTFILECREATE          65536
#define OFN_NONETWORKBUTTON          131072
#define OFN_NOLONGNAMES              262144  
#define OFN_EXPLORER                 524288  
#define OFN_NODEREFERENCELINKS      1048576
#define OFN_LONGNAMES               2097152  
#define OFN_ENABLEINCLUDENOTIFY     4194304  
#define OFN_ENABLESIZING            8388608
#define OFN_DONTADDTORECENT        33554432
#define OFN_FORCESHOWHIDDEN       268435456  

/*
 * Function: GetFile (xHarbour Version)
 * Purpose:  Compatibility implementation for xHarbour environments.
 */
FUNCTION GetFile( aFilter, cTitle, cIniDir, lMultiSelect, lNoChangeDirectory, nIndex )

   LOCAL cPath, cDefExt := ""
   LOCAL aFiles, cRet, cFile, n, x, c := ''
   LOCAL nFlags := OFN_EXPLORER

   hb_default( @lMultiSelect, .F. )
   hb_default( @lNoChangeDirectory, .F. )

   IF lMultiSelect
      nFlags += OFN_ALLOWMULTISELECT
   ENDIF
   IF lNoChangeDirectory
      nFlags += OFN_NOCHANGEDIR
   ENDIF

   IF aFilter == NIL
      aFilter := {}
   ENDIF
   
   // Build the filter string manually for the WVT wrapper.
   IF HB_ISARRAY( aFilter )
      FOR n := 1 TO Len( aFilter )
         c += aFilter[ n ][ 1 ] + Chr( 0 ) + aFilter[ n ][ 2 ] + Chr( 0 )
         c += Chr( 0 )
      NEXT
   ENDIF

   // Allocate a large buffer if multi-select is enabled, as the API 
   // writes all selected paths into this single string.
   IF WIN_AND( nFlags, OFN_ALLOWMULTISELECT ) > 0
      cFile := Space( 32000 )
   ELSE
      cFile := PadR( Space( 254 ), 255, Chr( 0 ) )
   ENDIF

   // WVT__GetOpenFileName is an internal xHarbour/MiniGUI C wrapper.
   cRet := WVT__GetOpenFileName( NIL, @cFile, cTitle, c, nFlags, cIniDir, cDefExt, @nIndex )

   // Manual parsing of the null-delimited result string for multi-select.
   IF WIN_AND( nFlags, OFN_ALLOWMULTISELECT ) > 0
      n := At( Chr( 0 ) + Chr( 0 ), cFile )
      cFile := Left( cFile, n )
      aFiles := {}
      IF n == 0 
         RETURN ( aFiles )
      END
      
      x := At( Chr( 0 ), cFile ) 
      cPath := Left( cFile, x ) // Extract the base directory.

      cFile := StrTran( cFile, cPath )
      IF ! Empty( cFile ) 
         c := ''
         FOR n := 1 TO Len( cFile )
            IF SubStr( cFile, n, 1 ) == Chr( 0 )
               // Reconstruct full path: Directory + \ + Filename.
               AAdd( aFiles, StrTran( cPath, Chr( 0 ) ) + '\' + c )
               c := ''
               LOOP
            END
            c += SubStr( cFile, n, 1 )
         NEXT
      ELSE
         // Only one file was selected.
         aFiles := { StrTran( cPath, Chr( 0 ) ) }
      ENDIF

      RETURN ( aFiles )
   ENDIF

RETURN ( cRet )

/*
 * Function: Putfile (xHarbour Version)
 * Purpose:  Compatibility implementation for file saving in xHarbour.
 */
FUNCTION Putfile( aFilter, cTitle, cIniDir, lNoChangeCurDir, cFile, nIndex, lPromptOverwrite )

   LOCAL n, c := '', cDefExt := ""
   LOCAL nFlags := OFN_EXPLORER

   hb_default( @nIndex, 1 )
   hb_default( @lNoChangeCurDir, .F. )
   hb_default( @lPromptOverwrite, .F. )

   IF lNoChangeCurDir
      nFlags += OFN_NOCHANGEDIR
   ENDIF

   IF lPromptOverwrite
      nFlags += OFN_OVERWRITEPROMPT
   ENDIF

   IF aFilter == NIL
      aFilter := {}
   END

   // Build the filter string.
   FOR n := 1 TO Len( aFilter )
      c += aFilter[ n ][ 1 ] + Chr( 0 ) + aFilter[ n ][ 2 ] + Chr( 0 )
      c += Chr( 0 )
   NEXT

   // WVT__GetSaveFileName is the internal C wrapper for the Save dialog.
   cFile := WVT__GetSaveFileName( NIL, cFile, cTitle, c, nFlags, cIniDir, cDefExt, @nIndex )

RETURN ( cFile )

#endif

/*
 * Function: GetFont
 * Purpose:  Displays the standard Windows Font Selection dialog.
 * 
 * Parameters:
 *    cInitFontName - Name of the font to pre-select.
 *    nInitFontSize - Size of the font to pre-select.
 *    lBold         - Initial bold state.
 *    lItalic       - Initial italic state.
 *    anInitColor   - Initial color as {R, G, B} array.
 *    lUnderLine    - Initial underline state.
 *    lStrikeOut    - Initial strikeout state.
 *    nCharset      - Initial character set (e.g., ANSI_CHARSET).
 * 
 * Returns:
 *    An array containing:
 *    [1] Font Name (String)
 *    [2] Font Size (Numeric)
 *    [3] Bold (Logical)
 *    [4] Italic (Logical)
 *    [5] Color (Array {R,G,B})
 *    [6] Underline (Logical)
 *    [7] Strikeout (Logical)
 *    [8] Charset (Numeric)
 */
FUNCTION GetFont( cInitFontName, nInitFontSize, lBold, lItalic, anInitColor, lUnderLine, lStrikeOut, nCharset )

   LOCAL RetArray
   LOCAL rgbColor As Numeric

   // Convert HMG RGB array to numeric COLORREF for the API.
   IF IsArrayRGB( anInitColor )
      rgbColor := RGB( anInitColor [1], anInitColor [2], anInitColor [3] )
   ENDIF

   // ChooseFont is the HMG wrapper for the WinAPI ChooseFont function.
   RetArray := ChooseFont( hb_defaultValue( cInitFontName, "" ), ;
      hb_defaultValue( nInitFontSize, 0 ), ;
      hb_defaultValue( lBold, .F. ), ;
      hb_defaultValue( lItalic, .F. ), ;
      rgbColor, ;
      hb_defaultValue( lUnderLine, .F. ), ;
      hb_defaultValue( lStrikeOut, .F. ), ;
      hb_defaultValue( nCharset, 0 ) )

   // Post-processing the return array:
   // The API returns a numeric color, but HMG users expect an {R,G,B} array.
   IF Empty( RetArray [1] )  
      // If the user cancelled, the font name is empty.
      RetArray [5] := { NIL, NIL, NIL }  
   ELSE
      // Convert the numeric RGB result back to an array.
      rgbColor := RetArray [5]  
      RetArray [5] := nRGB2Arr( rgbColor )  
   ENDIF

RETURN RetArray

#ifdef __XHARBOUR__

#pragma BEGINDUMP
/*
 * The following section includes C-level utility functions required 
 * for xHarbour compatibility, specifically for handling Windows 
 * common dialog structures.
 */
#include <wvtutils.c>

#pragma ENDDUMP

#endif