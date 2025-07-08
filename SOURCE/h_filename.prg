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

#ifdef __XHARBOUR__
#define __SYSDATA__
#xtranslate hb_StrShrink( <char> ) => Left( <char>, Len( <char> ) - 1 )
#endif
#include "minigui.ch"

/*-----------------------------------------------------------------------------*
FUNCTION cFilePath ( cPathMask )
*------------------------------------------------------------------------------*
*
*  Description:
*     Extracts the path component from a full file path string.
*
*  Parameters:
*     cPathMask - The full file path string (e.g., "C:\MyFolder\MyFile.txt").
*
*  Return Value:
*     The path component of the file path (e.g., "C:\MyFolder").  The trailing backslash is removed.
*
*  Purpose:
*     This function is used to isolate the directory path from a complete file path.
*     It leverages the hb_FNameSplit function to parse the path and then removes the trailing backslash.
*
*/
FUNCTION cFilePath ( cPathMask )
   LOCAL cPath

   hb_FNameSplit ( cPathMask, @cPath )

RETURN hb_StrShrink( cPath )

/*-----------------------------------------------------------------------------*
FUNCTION cFileNoPath ( cPathMask )
*------------------------------------------------------------------------------*
*
*  Description:
*     Extracts the file name (including extension) from a full file path string.
*
*  Parameters:
*     cPathMask - The full file path string (e.g., "C:\MyFolder\MyFile.txt").
*
*  Return Value:
*     The file name with extension (e.g., "MyFile.txt").
*
*  Purpose:
*     This function is used to retrieve the file name and extension from a complete file path,
*     excluding the directory path. It uses hb_FNameSplit to separate the path, name, and extension,
*     and then concatenates the name and extension.
*
*/
FUNCTION cFileNoPath ( cPathMask )
   LOCAL cName, cExt

   hb_FNameSplit ( cPathMask, , @cName, @cExt )

RETURN ( cName + cExt )

/*-----------------------------------------------------------------------------*
FUNCTION cFileNoExt ( cPathMask )
*------------------------------------------------------------------------------*
*
*  Description:
*     Extracts the file name (without extension) from a full file path string.
*
*  Parameters:
*     cPathMask - The full file path string (e.g., "C:\MyFolder\MyFile.txt").
*
*  Return Value:
*     The file name without extension (e.g., "MyFile").
*
*  Purpose:
*     This function is used to retrieve the file name without its extension from a complete file path.
*     It uses hb_FNameSplit to separate the path, name, and extension, and then returns only the name.
*
*/
FUNCTION cFileNoExt ( cPathMask )
   LOCAL cName

   hb_FNameSplit ( cPathMask, , @cName )

RETURN cName

/*-----------------------------------------------------------------------------*
FUNCTION _GetCompactPath ( cFile, nMax )
*------------------------------------------------------------------------------*
*
*  Description:
*     Compacts a file path string to a specified maximum length, inserting an ellipsis (...) if necessary.
*
*  Parameters:
*     cFile - The file path string to compact.
*     nMax  - The maximum length of the compacted string.
*
*  Return Value:
*     The compacted file path string. If the original string is shorter than nMax, it is returned unchanged.
*     If GetCompactPath fails, the original cFile is returned.
*
*  Purpose:
*     This function is used to display long file paths in a limited space, such as in a listbox or textbox.
*     It uses the Windows API function GetCompactPath to achieve the compaction.
*     The IFNUMERIC check handles cases where nMax might not be numeric, providing a default value.
*
*/
FUNCTION _GetCompactPath ( cFile, nMax )
   LOCAL cShort := Space( IFNUMERIC( nMax, nMax + 1, 64 ) )

RETURN iif( GetCompactPath( @cShort, cFile, IFNUMERIC( nMax, nMax, 63 ), NIL ) > 0, cShort, cFile )

/*-----------------------------------------------------------------------------*
FUNCTION _GetShortPathName ( cPath )
*------------------------------------------------------------------------------*
*
*  Description:
*     Retrieves the short (8.3) form of a file path.
*
*  Parameters:
*     cPath - The long file path string.
*
*  Return Value:
*     The short path name if it exists; otherwise, the original path.
*
*  Purpose:
*     This function is used to obtain the short path name (also known as the 8.3 name) of a file or directory.
*     Short path names are used for compatibility with older systems that do not support long file names.
*     It uses the Windows API function GetShortPathName.
*     If GetShortPathName fails (e.g., if short names are disabled on the volume), the original path is returned.
*
*/
FUNCTION _GetShortPathName ( cPath )
   LOCAL cShortPathName

RETURN iif( GetShortPathName( cPath, @cShortPathName ) > 0, cShortPathName, cPath )
