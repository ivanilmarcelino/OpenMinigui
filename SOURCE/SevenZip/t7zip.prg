/*
 * $Id: t7zip.prg 9365 2011-03-16 03:54:15Z andijahja $
 */

/*
 * Harbour Project source code:
 * SevenZip xHarbour Interface
 *
 * Copyright 2011 Andi Jahja <andi.jahja@yahoo.co.id>
 * www - http://www.xharbour.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "t7zip.ch"

//------------------------------------------------------------------------------
CREATE CLASS T7ZIP

   DATA hWndOwner AS INTEGER INIT 0
   DATA nError AS INTEGER INIT 0
   DATA handle AS INTEGER INIT 0

   DATA lShowProcessDlg AS LOGICAL INIT .F.
   DATA lAlwaysOverWrite AS LOGICAL INIT .T.

   DATA cArcName
   DATA cBuffer AS STRING INIT ""
   DATA nBuffer AS INTEGER INIT 0

   DATA cCompressionMethod AS STRING INIT "PPMd"
   DATA nCompressionMethod AS INTEGER INIT 3

   DATA nZipCompressionLevel AS INTEGER INIT 6

   DATA cCommand AS STRING INIT ""

   DATA nArcType AS INTEGER INIT 1
   DATA cArcType AS STRING INIT "7z"

   DATA aFiles
   DATA cPassword AS STRING INIT ""

   DATA lRecursive AS LOGICAL INIT .F.

   DATA aExcludeFiles
   DATA aVolumes

   DATA lSolid AS LOGICAL INIT .T.
   DATA lMultiCPU AS LOGICAL INIT .F.

   DATA lConvertANSIToOEM AS LOGICAL INIT .T.

   METHOD New() INLINE Self
   METHOD Create()

   METHOD Open() INLINE ;
      ::handle := HB_SevenZipOpenArchive( ::hWndOwner, ::cArcName, 0 )

   METHOD List() INLINE ;
      ::nError := HB_SevenZip( ;
         ::hWndOwner, ;
         'l "' + HB_ANSITOOEM( ::cArcName ) + '"', ;
         @::cBuffer, ;
         ::nBuffer )

   METHOD Test() INLINE ;
      ::nError := HB_SevenZip( ;
         ::hWndOwner, ;
         't "' + HB_ANSITOOEM( ::cArcName ) + '"', ;
         @::cBuffer, ;
         ::nBuffer )

   METHOD Extract( lWithPath ) INLINE ;
      ::nError := HB_SevenZip( ;
         ::hWndOwner, ;
         If( ValType( lWithPath ) == "L" .AND. lWithPath, "x ", "e " ) + ;
         If( ::lAlwaysOverWrite, "-y ", "" ) + ;
         If( ::lShowProcessDlg, "-hide ", "" ) + ;
         '"' + HB_ANSITOOEM( ::cArcName ) + '"', ;
         @::cBuffer, ;
         ::nBuffer )

   METHOD ErrorDescription()

   METHOD Close()                INLINE HB_SevenZipCloseArchive( ::handle )
   METHOD GetArcFileSize()       INLINE HB_SevenZipGetArcfilesize( ::handle )
   METHOD GetArcOriginalSize()   INLINE HB_SevenZipGetArcoriginalsize( ::handle )
   METHOD GetArcCompressedSize() INLINE HB_SevenZipGetArccompressedsize( ::handle )
   METHOD GetArcRatio()          INLINE HB_SevenZipGetArcratio( ::handle )

   METHOD GetOriginalSize()      INLINE HB_SevenZipGetOriginalsize( ::handle )
   METHOD GetCompressedSize()    INLINE HB_SevenZipGetCompressedsize( ::handle )
   METHOD GetRatio()             INLINE HB_SevenZipGetRatio( ::handle )

   METHOD Version() INLINE GetVersion()

END CLASS

//------------------------------------------------------------------------------
METHOD T7ZIP:Create()

   LOCAL cFile
   LOCAL nCPU

   // Currently only supports 7z and zip
   IF ValType( ::nArcType ) != "N" .OR. ;
      ::nArcType < 1 .OR. ;
      ::nArcType > 2

      RETURN ::nError := ERROR_NOT_SUPPORT
   ENDIF

   ::cArcType := aArcType[ ::nArcType ]
   ::cCommand := "a"

   IF !::lShowProcessDlg
      ::cCommand += " -hide"
   ENDIF

   ::cCommand += " -t" + ::cArcType

   SWITCH ::nArcType

      CASE ARCTYPE_ZIP

         IF ::nZipCompressionLevel >= 0 .AND. ;
            ::nZipCompressionLevel <= 9

            ::cCommand += ;
               " -mx" + LTrim( Str( ::nZipCompressionLevel ) )
         ENDIF

         EXIT

#ifndef __XHARBOUR__
      OTHERWISE
#else
      DEFAULT
#endif

         IF ValType( ::nCompressionMethod ) == "N" .AND. ;
            ::nCompressionMethod > 0 .AND. ;
            ::nCompressionMethod <= Len( aArcMethod )

            ::cCompressionMethod := ;
               aArcMethod[ ::nCompressionMethod ]

            ::cCommand += ;
               " -m0=" + ::cCompressionMethod
         ENDIF

         IF ::nZipCompressionLevel >= 0 .AND. ;
            ::nZipCompressionLevel <= 9

            ::cCommand += ;
               " -mx" + LTrim( Str( ::nZipCompressionLevel ) )
         ENDIF

   END

   IF !Empty( ::cPassword )
      ::cCommand += " -p" + AllTrim( ::cPassword )
   ENDIF

   IF ::lRecursive
      ::cCommand += " -r"
   ENDIF

   IF !::lSolid
      ::cCommand += " -ms=off"
   ENDIF

   IF ::lMultiCPU

      ::cCommand += " -mmt"

      nCPU := Val( GetEnv( "NUMBER_OF_PROCESSORS" ) )

      IF nCPU > 2
         ::cCommand += "=" + LTrim( Str( nCPU ) )
      ENDIF

   ENDIF

   // Excluded files
   IF ValType( ::aExcludeFiles ) == "A"

      FOR EACH cFile IN ::aExcludeFiles

         IF "*." $ cFile .OR. ".*" $ cFile
            ::cCommand += " -x!" + AllTrim( cFile )
         ELSE
            ::cCommand += " -xr!" + AllTrim( cFile )
         ENDIF

      NEXT

   ELSEIF ValType( ::aExcludeFiles ) == "C"

      ::cCommand += ;
         " -x!" + AllTrim( ::aExcludeFiles )

   ENDIF

   // Multi-volume archives
   IF ValType( ::aVolumes ) == "A"

      FOR EACH cFile IN ::aVolumes
         ::cCommand += ;
            " -v" + LTrim( Str( cFile ) ) + "b"
      NEXT

   ELSEIF ValType( ::aVolumes ) == "N"

      ::cCommand += ;
         " -v" + LTrim( Str( ::aVolumes ) ) + "b"

   ENDIF

   // Archive name
   ::cCommand += ;
      " " + HB_7ZIPCONVERTFILENAME( ;
         ::cArcName, ;
         ::lConvertANSIToOEM )

   // Files to add
   IF ValType( ::aFiles ) == "A"

      FOR EACH cFile IN ::aFiles

         ::cCommand += ;
            " " + HB_7ZIPCONVERTFILENAME( ;
               cFile, ;
               ::lConvertANSIToOEM )

      NEXT

   ELSEIF ValType( ::aFiles ) == "C"

      ::cCommand += ;
         " " + HB_7ZIPCONVERTFILENAME( ;
            ::aFiles, ;
            ::lConvertANSIToOEM )

   ENDIF

   RETURN ::nError := HB_SevenZip( ;
      ::hWndOwner, ;
      ::cCommand, ;
      @::cBuffer, ;
      ::nBuffer )

//------------------------------------------------------------------------------
METHOD T7ZIP:ErrorDescription()

   LOCAL i

   IF ::nError == 0
      RETURN "ERROR_OK"
   ENDIF

   i := AScan( AERRDEF, { |e| e[ 2 ] == ::nError } )

   IF i > 0
      RETURN AERRDEF[ i ][ 1 ]
   ENDIF

   RETURN "ERROR_UNKNOWN"

//------------------------------------------------------------------------------
STATIC FUNCTION GetVersion()

   LOCAL nVersion    := hb_SevenZipGetVersion()
   LOCAL nSubVersion := hb_SevenZipGetSubVersion()
   LOCAL cVersion    := "Version"

   cVersion += ;
      Str( nVersion / 100, 5, 2 ) + "." + ;
      StrZero( nSubVersion / 100, 5, 2 )

   RETURN cVersion

//------------------------------------------------------------------------------
STATIC FUNCTION GetFileInPath( cFile )

   LOCAL cPath    := GetEnv( "PATH" ) + ";"
   LOCAL lFound   := .F.
   LOCAL nLPos    := 0
   LOCAL nRPos    := 0
   LOCAL cSearch

   DO WHILE nRPos < Len( cPath ) .AND. !lFound

      nRPos := hb_At( ";", cPath, nLPos + 1 )

      cSearch := hb_DirSepAdd( ;
         SubStr( cPath, nLPos + 1, nRPos - nLPos - 1 ) )

      lFound := hb_FileExists( cSearch + cFile )

      nLPos := nRPos

   ENDDO

   RETURN lFound

//------------------------------------------------------------------------------
STATIC FUNCTION HB_7ZIPCONVERTFILENAME( cFileName, lConvert )

   IF lConvert
      RETURN '"' + HB_ANSITOOEM( cFileName ) + '"'
   ENDIF

   RETURN cFileName

//------------------------------------------------------------------------------
#define SEVENZIPDLL "7-zip32.dll"

//------------------------------------------------------------------------------
INIT PROCEDURE _7ZINIT

   IF hb_FileExists( SEVENZIPDLL ) .OR. ;
      GetFileInPath( SEVENZIPDLL )

      INIT7ZIPDLL()
   ENDIF

   RETURN

//------------------------------------------------------------------------------
EXIT PROCEDURE _7ZEXIT

   EXIT7ZIPDLL()

   RETURN
