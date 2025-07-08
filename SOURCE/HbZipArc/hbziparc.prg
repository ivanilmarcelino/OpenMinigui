/*
 * Harbour Project source code:
 * ZipArchive interface compatibility implementation.
 *
 * This code provides an interface for handling Zip file operations,
 * including creating, extracting, and manipulating Zip archives.
 *
 * It defines various functions and procedures for reading, writing,
 * and managing Zip files using the Harbour language.
 *
 * www - https://harbour.github.io/
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2,  or ( at your option )
 * any later version.
 *
 */

#include "common.ch"
#include "directry.ch"
#include "fileio.ch"
#include "hbcompat.ch"
#include "hbmzip.ch"

// Static variables for buffering and settings
STATIC s_nReadBuffer := 32768
STATIC s_cComment
STATIC s_lReadOnly := .F.

PROCEDURE SetZipReadOnly( lReadOnly )
   DEFAULT lReadOnly TO .F.
   s_lReadOnly := lReadOnly
   RETURN

PROCEDURE hb_SetZipComment( cComment )
   IF cComment == NIL .OR. ISCHARACTER( cComment )
      s_cComment := cComment
   ENDIF
   RETURN

FUNCTION hb_GetZipComment( cFileName )
   LOCAL hUnzip
   LOCAL cComment

   IF Set( _SET_DEFEXTENSIONS )
      cFileName := hb_FNameExtSetDef( cFileName, ".zip" )
   ENDIF

   IF !Empty( hUnzip := hb_UnzipOpen( cFileName ) )
      hb_UnzipGlobalInfo( hUnzip, NIL, @cComment )
      hb_UnzipClose( hUnzip )
   ELSE
      cComment := ""
   ENDIF

   RETURN cComment

FUNCTION hb_GetFileCount( cFileName )
   LOCAL hUnzip
   LOCAL nEntries

   IF Set( _SET_DEFEXTENSIONS )
      cFileName := hb_FNameExtSetDef( cFileName, ".zip" )
   ENDIF

   IF !Empty( hUnzip := hb_UnzipOpen( cFileName ) )
      hb_UnzipGlobalInfo( hUnzip, @nEntries, NIL )
      hb_UnzipClose( hUnzip )
   ELSE
      nEntries := 0
   ENDIF

   RETURN nEntries

FUNCTION hb_ZipWithPassword( cFileName )
   LOCAL lCrypted := .F.
   LOCAL hUnzip

   IF Set( _SET_DEFEXTENSIONS )
      cFileName := hb_FNameExtSetDef( cFileName, ".zip" )
   ENDIF

   IF !Empty( hUnzip := hb_UnzipOpen( cFileName ) )
      IF hb_UnzipFileFirst( hUnzip ) == 0
         hb_UnzipFileInfo( hUnzip, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, @lCrypted ) // Check if encrypted
      ENDIF
      hb_UnzipClose( hUnzip )
   ENDIF

   RETURN lCrypted

FUNCTION hb_GetFilesInZip( cFileName, lVerbose )
   LOCAL hUnzip, nErr, aFiles := {}
   LOCAL dDate, cTime, nSize, nCompSize, nInternalAttr, nMethod, lCrypted, cComment, nRatio, nCRC

   IF Set( _SET_DEFEXTENSIONS )
      cFileName := hb_FNameExtSetDef( cFileName, ".zip" )
   ENDIF

   IF !Empty( hUnzip := hb_UnzipOpen( cFileName ) )
      DEFAULT lVerbose TO .F.

      nErr := hb_UnzipFileFirst( hUnzip )
      DO WHILE nErr == 0
         hb_UnzipFileInfo( hUnzip, @cFileName, @dDate, @cTime, @nInternalAttr, NIL, @nMethod, @nSize, @nCompSize, @lCrypted, @cComment, @nCRC )

         IF lVerbose
            IF nSize > 0
               nRatio := 100 - ( ( nCompSize * 100 ) / nSize )
               IF nRatio < 0
                  nRatio := 0
               ENDIF
            ELSE
               nRatio := 0
            ENDIF

            // Add detailed information to the array
            AAdd( aFiles, { cFileName, nSize, nMethod, nCompSize, nRatio, dDate, cTime, hb_numtohex( nCRC, 8 ), nInternalAttr, lCrypted, cComment } )
         ELSE
            AAdd( aFiles, cFileName ) // Add just the file name to the array
         ENDIF

         nErr := hb_UnzipFileNext( hUnzip )
      ENDDO

      hb_UnzipClose( hUnzip )
   ENDIF

   RETURN aFiles

FUNCTION hb_ZipTestPK( cFileName )
   HB_SYMBOL_UNUSED( cFileName )
   /* NOTE: Spanning not supported. */
   RETURN 0

FUNCTION hb_SetDiskZip( bBlock )
   HB_SYMBOL_UNUSED( bBlock )
   /* NOTE: Spanning not supported. */
   RETURN .F.

FUNCTION TransferFromZip( cZipSrc, cZipDst, aFiles )
   HB_SYMBOL_UNUSED( cZipSrc )
   HB_SYMBOL_UNUSED( cZipDst )
   HB_SYMBOL_UNUSED( aFiles )
   /* TODO: Implement. */
   RETURN .F.

PROCEDURE hb_SetBuffer( nWriteBuffer, nExtractBuffer, nReadBuffer )
   HB_SYMBOL_UNUSED( nWriteBuffer )
   HB_SYMBOL_UNUSED( nExtractBuffer )

   IF HB_ISNUMERIC( nReadBuffer ) .AND. nReadBuffer >= 1
      s_nReadBuffer := Min( nReadBuffer, 32768 ) // Limit the buffer size to 32KB
   ENDIF

   RETURN

FUNCTION hb_ZipFile( cFileName, acFiles, nLevel, bUpdate, lOverwrite, cPassword, lWithPath, lWithDrive, bProgress, lFullPath, acExclude )
   LOCAL lRetVal := .T.

   LOCAL hZip
   LOCAL hHandle
   LOCAL nLen
   LOCAL cBuffer := Space( s_nReadBuffer )
   LOCAL cFileToZip
   LOCAL nPos
   LOCAL nRead
   LOCAL cName, cExt, cDrive, cPath
   LOCAL nSize
   LOCAL tTime
   LOCAL nAttr

   LOCAL aExclFile
   LOCAL aProcFile
   LOCAL cFN
   LOCAL aFile

   DEFAULT lOverwrite TO .F.
   DEFAULT lFullPath TO .T.

   IF Set( _SET_DEFEXTENSIONS )
      cFileName := hb_FNameExtSetDef( cFileName, ".zip" )
   ENDIF

   IF lOverwrite .AND. hb_FileExists( cFileName )
      FErase( cFileName )
   ENDIF

   IF !Empty( hZip := hb_ZipOpen( cFileName, iif( ! lOverwrite .AND. hb_FileExists( cFileName ), HB_ZIP_OPEN_ADDINZIP, NIL ) ) )

      DEFAULT acFiles TO {}
      DEFAULT acExclude TO {}
      DEFAULT lWithPath TO .F.
      DEFAULT lWithDrive TO .F.

      IF hb_IsString( acFiles )
         acFiles := { acFiles }
      ENDIF
      IF hb_IsString( acExclude )
         acExclude := { acExclude }
      ENDIF

      /* NOTE: Try not to add the .zip file to itself. */
      hb_FNameSplit( cFileName, NIL, @cName, @cExt )
      aExclFile := { hb_FNameMerge( NIL, cName, cExt ) }
      FOR EACH cFN IN acExclude
         IF "?" $ cFN .OR. "*" $ cFN
            FOR EACH aFile IN Directory( cFN )
               AAdd( aExclFile, aFile[ F_NAME ] )
            NEXT
         ELSE
            AAdd( aExclFile, cFN )
         ENDIF
      NEXT

      aProcFile := {}
      FOR EACH cFN IN acFiles
         IF "?" $ cFN .OR. "*" $ cFN
            FOR EACH aFile IN Directory( cFN )
               IF AScan( aExclFile, {| cExclFile | hb_FileMatch( aFile[ F_NAME ], cExclFile ) } ) == 0
                  AAdd( aProcFile, aFile[ F_NAME ] )
               ENDIF
            NEXT
         ELSE
            hb_FNameSplit( cFN, NIL, @cName, @cExt )
            IF AScan( aExclFile, {| cExclFile | hb_FileMatch( hb_FNameMerge( NIL, cName, cExt ), cExclFile ) } ) == 0
               AAdd( aProcFile, cFN )
            ENDIF
         ENDIF
      NEXT

      aExclFile := NIL

      nPos := 1
      FOR EACH cFileToZip IN aProcFile

         IF ( hHandle := FOpen( cFileToZip, FO_READ ) ) != F_ERROR

            IF hb_IsBlock( bUpdate )
               Eval( bUpdate, cFileToZip, nPos++ )
            ENDIF

            nRead := 0
            nSize := hb_FSize( cFileToZip )

            hb_FGetDateTime( cFileToZip, @tTime )
            hb_fGetAttr( cFileToZip, @nAttr )

            hb_FNameSplit( hb_ANSIToOEM( cFileToZip ), @cPath, @cName, @cExt, @cDrive )
            IF ! lWithDrive .AND. ! Empty( cDrive ) .AND. hb_LeftEq( cPath, cDrive + ":" )
               cPath := SubStr( cPath, Len( cDrive + ":" ) + 1 )
            ENDIF
            hb_ZipFileCreate( hZip, hb_FNameMerge( iif( lWithPath, cPath, NIL ), cName, cExt, iif( lWithDrive, cDrive, NIL ) ),;
                tTime, NIL, nAttr, nAttr, NIL, nLevel, cPassword, iif( Empty( cPassword ), NIL, hb_ZipFileCRC32( cFileToZip ) ), NIL )

            DO WHILE ( nLen := FRead( hHandle, @cBuffer, hb_BLen( cBuffer ) ) ) > 0

               IF hb_IsBlock( bProgress )
                  nRead += nLen
                  Eval( bProgress, nRead, nSize )
               ENDIF

               hb_ZipFileWrite( hZip, cBuffer, nLen )
            ENDDO

            hb_ZipFileClose( hZip )

            FClose( hHandle )

            IF hb_FGetAttr( cFileToZip, @nAttr )
               hb_FSetAttr( cFileToZip, hb_bitAnd( nAttr, hb_bitNot( HB_FA_ARCHIVE ) ) )
            ENDIF
         ELSE
            lRetVal := .F.
         ENDIF
      NEXT

      hb_ZipClose( hZip, s_cComment )
   ELSE
      lRetVal := .F.
   ENDIF

   RETURN lRetVal

FUNCTION hb_UnzipFile( cFileName, bUpdate, lWithPath, cPassword, cPath, acFiles, bProgress )

   LOCAL lRetVal := .T.

   LOCAL hUnzip
   LOCAL nErr
   LOCAL nPos
   LOCAL cZipName
   LOCAL cExtName
   LOCAL cSubPath
   LOCAL cName
   LOCAL cExt
   LOCAL lExtract

   LOCAL hHandle
   LOCAL nSize
   LOCAL nRead
   LOCAL nLen
   LOCAL dDate
   LOCAL cTime
   LOCAL cBuffer := Space( s_nReadBuffer )

   IF hb_IsString( cPath ) .and. lower( cPath ) == "mem:"
      cPath := lower( cPath )
      lWithPath := .F.
   ENDIF

   DEFAULT lWithPath TO .F.

   IF lWithPath .AND. !hb_DirExists( cPath )
      lRetVal := hb_DirBuild( cPath )
   ENDIF

   IF Empty( cPassword )
      cPassword := NIL
   ENDIF

   IF Set( _SET_DEFEXTENSIONS )
      cFileName := hb_FNameExtSetDef( cFileName, ".zip" )
   ENDIF

   IF Empty( hUnzip := hb_UnzipOpen( cFileName ) )
      lRetVal := .F.
   ELSE
      IF hb_IsNumeric( acFiles ) .OR. ;
         hb_IsString( acFiles )
         acFiles := { acFiles }
      ENDIF

      IF Empty( cPath )
         hb_FNameSplit( cFileName, @cPath )
      ENDIF

      IF cPath != "mem:"
         cPath := hb_DirSepAdd( cPath )
      ENDIF

      nPos := 0
      nErr := hb_UnzipFileFirst( hUnzip )
      DO WHILE nErr == 0

         nPos++

         IF hb_UnzipFileInfo( hUnzip, @cZipName, @dDate, @cTime, , , , @nSize ) == 0

            hb_FNameSplit( hb_OEMToANSI( cZipName ), @cSubPath, @cName, @cExt )
            cExtName := hb_FNameMerge( NIL, cName, cExt )

            /* NOTE: As opposed to original hbziparch we don't do a second match without path. */
            lExtract := ( Empty( acFiles ) .OR. ;
               AScan( acFiles, nPos ) > 0 .OR. ;
               AScan( acFiles, {| cMask | hb_FileMatch( cExtName, cMask ) } ) > 0 )

            IF lExtract
               IF cPath == "mem:"
                  cSubPath := ""
                  hb_vfErase( cPath + cSubPath + cExtName )
               ELSE
                  IF ! Empty( cSubPath ) .AND. ! hb_DirExists( cPath + cSubPath ) .AND. ! hb_DirBuild( cPath + cSubPath )
                     lRetVal := .F.
                     EXIT
                  ENDIF
               ENDIF
            ENDIF

            IF lExtract
               IF hb_UnzipFileOpen( hUnzip, cPassword ) != UNZ_OK
                  lRetVal := .F.
                  EXIT
               ENDIF
               cExtName := cPath + cSubPath + cExtName
               cExtName := StrTran(cExtName, "\", hb_ps())
               cExtName := StrTran(cExtName, "/", hb_ps())
               hHandle := hb_vfOpen( cExtName,  hb_BitOr( FO_WRITE, HB_FO_CREAT, HB_FO_EXCL ) )
               IF !Empty( hHandle )
                  nRead := 0
                  DO WHILE ( nLen := hb_unZipFileRead( hUnzip, @cBuffer, hb_BLen( cBuffer ) ) ) > 0
                     IF hb_IsEvalItem( bProgress )
                        nRead += nLen
                        Eval( bProgress, nRead, nSize, cExtName )
                     ENDIF
                     hb_vfWrite( hHandle, cBuffer, nLen )
                  ENDDO

                  hb_UnzipFileClose( hUnzip )
                  hb_vfClose( hHandle )

                  hb_vfTimeSet( cExtName, dDate, cTime )

                  IF hb_IsEvalItem( bUpdate )
                     Eval( bUpdate, cZipName, nPos, cExtName )
                  ENDIF
               ENDIF
            ENDIF
         ENDIF

         nErr := hb_UnzipFileNext( hUnzip )
      ENDDO

      hb_UnzipClose( hUnzip )
   ENDIF

   RETURN lRetVal

FUNCTION hb_UnzipFileIndex( ... )
   RETURN hb_UnzipFile( ... )

FUNCTION hb_UnzipAllFile( ... )
   RETURN hb_UnzipFile( ... )

FUNCTION hb_ZipDeleteFiles( cFileName, acFiles )

   LOCAL lRetVal := .T.
   LOCAL cFileToProc

   IF Set( _SET_DEFEXTENSIONS )
      cFileName := hb_FNameExtSetDef( cFileName, ".zip" )
   ENDIF

   IF hb_IsString( acFiles )
      acFiles := { acFiles }
   ENDIF

   FOR EACH cFileToProc IN acFiles
      lRetVal := lRetVal .AND. ( hb_ZipDeleteFile( cFileName, cFileToProc ) == UNZ_OK )
   NEXT

   RETURN lRetVal

FUNCTION hb_IsZipFile( cFilename )

   LOCAL isZipFile := .F.
   LOCAL ZIP_SIGNATURE := { 'P', 'K', 0x03, 0x04 }
   LOCAL nLength := Len( ZIP_SIGNATURE )
   LOCAL hHandle
   LOCAL cBuffer := Space( nLength )

   TRY
      IF ( hHandle := FOpen( cFilename, FO_READ + FO_SHARED ) ) <> F_ERROR

         IF FSeek( hHandle, 0, FS_END ) > nLength

            FSeek( hHandle, 0, FS_SET )

            IF FRead( hHandle, @cBuffer, nLength ) == nLength

               IF hb_BLeft( cBuffer, 1 ) == ZIP_SIGNATURE[ 1 ] .AND. ;
                     hb_BSubStr( cBuffer, 2, 1 ) == ZIP_SIGNATURE[ 2 ] .AND. ;
                     hb_BSubStr( cBuffer, 3, 1 ) == Chr( ZIP_SIGNATURE[ 3 ] ) .AND. ;
                     hb_BRight( cBuffer, 1 ) == Chr( ZIP_SIGNATURE[ 4 ] )
                  isZipFile := .T.
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   CATCH
      /* if error do nothing */
   FINALLY
      IF hHandle <> NIL
         FClose( hHandle )
      ENDIF
   END

   RETURN isZipFile
