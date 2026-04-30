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
 * the Free Software Foundation; either version 2,  or (at your option)
 * any later version.
 *
 */

#include "common.ch"
#include "directry.ch"
#include "fileio.ch"
#include "hbcompat.ch"
#include "hbmzip.ch"

// Default buffer size for I/O operations to balance memory usage and speed.
#define ZIP_READ_BUFFER  32768

// Module-level state variables
STATIC s_nReadBuffer := ZIP_READ_BUFFER // Current read buffer size
STATIC s_cZipComment                    // Stores the global archive comment during creation

/*
 * PROCEDURE hb_SetZipComment( cComment )
 *
 * Purpose:
 *    Assigns a global comment string to be embedded in the ZIP archive.
 *
 * Parameters:
 *    cComment : String - The text to store. Pass NIL to clear the comment.
 *
 * Side Effects:
 *    Updates the static variable s_cZipComment, which is used when closing an archive.
 */
PROCEDURE hb_SetZipComment( cComment )
   IF cComment == NIL .OR. ISCHARACTER( cComment )
      s_cZipComment := cComment
   ENDIF
RETURN

/*
 * FUNCTION hb_GetZipComment( cFileName )
 *
 * Purpose:
 *    Extracts the global comment from an existing ZIP file.
 *
 * Parameters:
 *    cFileName : String - Path to the ZIP archive.
 *
 * Returns:
 *    String : The archive comment, or an empty string if none exists or file is invalid.
 */
FUNCTION hb_GetZipComment( cFileName )

   LOCAL hUnzip
   LOCAL cComment

   // Ensure the file has a .zip extension if the environment setting allows it.
   cFileName := Zip_EnsureExtension( cFileName )

   // Open the archive in read mode to access metadata.
   IF ! Empty( hUnzip := hb_UnzipOpen( cFileName ) )
      hb_UnzipGlobalInfo( hUnzip, NIL, @cComment )
      hb_UnzipClose( hUnzip )
   ENDIF

   DEFAULT cComment TO ""

RETURN cComment

/*
 * FUNCTION hb_GetFileCount( cFileName )
 *
 * Purpose:
 *    Returns the total number of entries (files and directories) inside the ZIP.
 *
 * Parameters:
 *    cFileName : String - Path to the ZIP archive.
 *
 * Returns:
 *    Numeric : Count of entries. Returns 0 if the file cannot be opened.
 */
FUNCTION hb_GetFileCount( cFileName )

   LOCAL hUnzip
   LOCAL nEntries

   cFileName := Zip_EnsureExtension( cFileName )

   IF ! Empty( hUnzip := hb_UnzipOpen( cFileName ) )
      // Retrieve global info; the first parameter returns the entry count.
      hb_UnzipGlobalInfo( hUnzip, @nEntries, NIL )
      hb_UnzipClose( hUnzip )
   ELSE
      nEntries := 0
   ENDIF

RETURN nEntries

/*
 * FUNCTION hb_ZipWithPassword( cFileName )
 *
 * Purpose:
 *    Checks if the ZIP archive contains encrypted content.
 *
 * Parameters:
 *    cFileName : String - Path to the ZIP archive.
 *
 * Returns:
 *    Logical : .T. if the first file entry is encrypted; otherwise .F.
 *
 * Implementation Note:
 *    This function checks the encryption flag of the first file as a heuristic 
 *    for the whole archive.
 */
FUNCTION hb_ZipWithPassword( cFileName )

   LOCAL lCrypted := .F.
   LOCAL hUnzip

   cFileName := Zip_EnsureExtension( cFileName )

   IF ! Empty( hUnzip := hb_UnzipOpen( cFileName ) )
      // Move to the first file in the central directory.
      IF hb_UnzipFileFirst( hUnzip ) == 0
         // Retrieve file info, specifically the 10th parameter (encryption flag).
         hb_UnzipFileInfo( hUnzip, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, @lCrypted )
      ENDIF
      hb_UnzipClose( hUnzip )
   ENDIF

RETURN lCrypted

/*
 * FUNCTION hb_GetFilesInZip( cFileName, lVerbose )
 *
 * Purpose:
 *    Generates a list of all files contained within the archive.
 *
 * Parameters:
 *    cFileName : String  - Path to the ZIP archive.
 *    lVerbose  : Logical - If .T., returns detailed metadata. If .F., returns only names.
 *
 * Returns:
 *    Array : A list of filenames or a multi-dimensional array of file details.
 */
FUNCTION hb_GetFilesInZip( cFileName, lVerbose )

   LOCAL hUnzip, nErr, aFiles := {}
   LOCAL dDate, cTime, nSize, nCompSize, nInternalAttr, nMethod, lCrypted, cComment, nRatio, nCRC

   cFileName := Zip_EnsureExtension( cFileName )

   IF ! Empty( hUnzip := hb_UnzipOpen( cFileName ) )
      DEFAULT lVerbose TO .F.

      nErr := hb_UnzipFileFirst( hUnzip )
      DO WHILE nErr == 0
         // Extract all available metadata for the current entry.
         hb_UnzipFileInfo( hUnzip, @cFileName, @dDate, @cTime, @nInternalAttr, NIL, @nMethod, @nSize, @nCompSize, @lCrypted, @cComment, @nCRC )

         IF lVerbose
            // Calculate compression ratio for reporting.
            IF nSize > 0
               nRatio := 100 - ( ( nCompSize * 100 ) / nSize )
               IF nRatio < 0
                  nRatio := 0
               ENDIF
            ELSE
               nRatio := 0
            ENDIF

            // Construct a detailed record for the file.
            AAdd( aFiles, { cFileName, nSize, nMethod, nCompSize, nRatio, dDate, cTime, hb_NumToHex( nCRC, 8 ), nInternalAttr, lCrypted, cComment } )
         ELSE
            // Simple mode: just the path/filename.
            AAdd( aFiles, cFileName )
         ENDIF

         nErr := hb_UnzipFileNext( hUnzip )
      ENDDO

      hb_UnzipClose( hUnzip )
   ENDIF

RETURN aFiles

/*
 * PROCEDURE hb_SetBuffer( nWriteBuffer, nExtractBuffer, nReadBuffer )
 *
 * Purpose:
 *    Configures the internal I/O buffer size.
 *
 * Parameters:
 *    nReadBuffer : Numeric - Desired buffer size in bytes.
 *
 * Implementation Note:
 *    The buffer is capped at 32KB (ZIP_READ_BUFFER) to prevent excessive 
 *    memory allocation in constrained environments.
 */
PROCEDURE hb_SetBuffer( nWriteBuffer, nExtractBuffer, nReadBuffer )
   HB_SYMBOL_UNUSED( nWriteBuffer )
   HB_SYMBOL_UNUSED( nExtractBuffer )

   IF HB_ISNUMERIC( nReadBuffer ) .AND. nReadBuffer >= 1
      s_nReadBuffer := Min( nReadBuffer, ZIP_READ_BUFFER )
   ENDIF

RETURN

/*
 * FUNCTION hb_ZipFile( cFileName, acFiles, nLevel, bUpdate, lOverwrite, ... )
 *
 * Purpose:
 *    Creates a new ZIP archive or adds files to an existing one.
 *
 * Parameters:
 *    cFileName  : String   - Target ZIP file.
 *    acFiles    : Array    - List of files/wildcards to include.
 *    nLevel     : Numeric  - Compression level (0-9).
 *    bUpdate    : Codeblock- Callback for file-start events (useful for Progress Bars).
 *    lOverwrite : Logical  - If .T., deletes existing ZIP before starting.
 *    cPassword  : String   - Optional encryption key.
 *    lWithPath  : Logical  - Store relative paths in the ZIP.
 *    bProgress  : Codeblock- Callback for byte-level progress.
 *
 * Returns:
 *    Logical : .T. if successful.
 */
FUNCTION hb_ZipFile( cFileName, acFiles, nLevel, bUpdate, lOverwrite, cPassword, lWithPath, lWithDrive, bProgress, lFullPath, acExclude )

   LOCAL lRetVal := .T.
   LOCAL hZip, hHandle, nLen, nPos, nRead, nSize, tTime, nAttr
   LOCAL cBuffer := Space( s_nReadBuffer )
   LOCAL cFileToZip, cName, cExt, cDrive, cPath, cFN, aFile
   LOCAL aExclFile, aProcFile

   DEFAULT lOverwrite TO .F.
   DEFAULT lFullPath TO .F.

   cFileName := Zip_EnsureExtension( cFileName )

   // Handle overwrite logic before opening the archive handle.
   IF lOverwrite .AND. hb_FileExists( cFileName )
      FErase( cFileName )
   ENDIF

   // Open ZIP for creation or appending.
   IF ! Empty( hZip := hb_ZipOpen( cFileName, iif( ! lOverwrite .AND. hb_FileExists( cFileName ), HB_ZIP_OPEN_ADDINZIP, NIL ) ) )

      DEFAULT acFiles TO {}
      DEFAULT acExclude TO {}
      DEFAULT lWithPath TO .F.
      DEFAULT lWithDrive TO .F.

      IF HB_ISSTRING( acFiles )
         acFiles := { acFiles }
      ENDIF
      IF HB_ISSTRING( acExclude )
         acExclude := { acExclude }
      ENDIF

      /* 
       * Logic: Prevent the ZIP from trying to compress itself if it's in the 
       * same directory as the source files. 
       */
      hb_FNameSplit( cFileName, NIL, @cName, @cExt )
      aExclFile := { hb_FNameMerge( NIL, cName, cExt ) }
      
      // Process exclusion list (supports wildcards).
      FOR EACH cFN IN acExclude
         IF "?" $ cFN .OR. "*" $ cFN
            FOR EACH aFile IN Directory( cFN )
               AAdd( aExclFile, aFile[ F_NAME ] )
            NEXT
         ELSE
            AAdd( aExclFile, cFN )
         ENDIF
      NEXT

      // Expand wildcards in the inclusion list and filter against exclusions.
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

      aExclFile := NIL // Cleanup memory

      nPos := 1
      FOR EACH cFileToZip IN aProcFile

         IF ( hHandle := FOpen( cFileToZip ) ) != F_ERROR

            // Handle absolute path conversion if requested.
            IF lFullPath
               IF ! hb_PathIsAbsolute( cFileToZip )
                  cFileToZip := hb_PathJoin( hb_cwd(), cFileToZip )
               ENDIF
            ENDIF

            // Trigger UI update callback (e.g., updating a label).
            IF HB_ISBLOCK( bUpdate )
               Eval( bUpdate, cFileToZip, nPos++ )
            ENDIF

            nRead := 0
            nSize := hb_FSize( cFileToZip )

            // Preserve original file timestamps and attributes.
            hb_FGetDateTime( cFileToZip, @tTime )
            hb_FGetAttr( cFileToZip, @nAttr )

            // Prepare the internal ZIP path name.
            hb_FNameSplit( hb_ANSIToOEM( cFileToZip ), @cPath, @cName, @cExt, @cDrive )
            IF ! lWithDrive .AND. ! Empty( cDrive ) .AND. hb_LeftEq( cPath, cDrive + ":" )
               cPath := SubStr( cPath, Len( cDrive + ":" ) + 1 )
            ENDIF
            
            // Create the file entry within the ZIP.
            hb_ZipFileCreate( hZip, hb_FNameMerge( iif( lWithPath, cPath, NIL ), cName, cExt, iif( lWithDrive, cDrive, NIL ) ), ;
               tTime, NIL, nAttr, nAttr, NIL, nLevel, cPassword, iif( Empty( cPassword ), NIL, hb_ZipFileCRC32( cFileToZip ) ), NIL )

            // Stream file content into the ZIP using the configured buffer.
            DO WHILE ( nLen := FRead( hHandle, @cBuffer, hb_BLen( cBuffer ) ) ) > 0
               IF HB_ISBLOCK( bProgress )
                  nRead += nLen
                  Eval( bProgress, nRead, nSize )
               ENDIF
               hb_ZipFileWrite( hZip, cBuffer, nLen )
            ENDDO

            hb_ZipFileClose( hZip )
            FClose( hHandle )

            // Clear the Archive attribute on the source file after successful backup.
            IF hb_FGetAttr( cFileToZip, @nAttr )
               hb_FSetAttr( cFileToZip, hb_bitAnd( nAttr, hb_bitNot( HB_FA_ARCHIVE ) ) )
            ENDIF
         ELSE
            lRetVal := .F.
         ENDIF
      NEXT

      // Finalize the ZIP and write the global comment.
      hb_ZipClose( hZip, s_cZipComment )
   ELSE
      lRetVal := .F.
   ENDIF

RETURN lRetVal

/*
 * FUNCTION hb_UnzipFile( cFileName, bUpdate, lWithPath, cPassword, cPath, acFiles, bProgress )
 *
 * Purpose:
 *    Extracts files from a ZIP archive to the disk or memory.
 *
 * Parameters:
 *    cPath   : String - Destination directory. Use "mem:" for Virtual File System extraction.
 *    acFiles : Array  - Specific files to extract. If empty, extracts everything.
 *
 * Implementation Note:
 *    Supports progress tracking via bProgress and bUpdate blocks.
 */
FUNCTION hb_UnzipFile( cFileName, bUpdate, lWithPath, cPassword, cPath, acFiles, bProgress )

   LOCAL lRetVal := .T.
   LOCAL hUnzip, nErr, nPos, cZipName, cExtName, cSubPath, cName, cExt, lExtract
   LOCAL hHandle, nSize, nRead, nLen, dDate, cTime
   LOCAL cBuffer := Space( s_nReadBuffer )

   // Check for Harbour's Virtual File System (VFS) target.
   IF HB_ISSTRING( cPath ) .AND. Lower( cPath ) == "mem:"
      cPath := Lower( cPath )
      lWithPath := .F.
   ENDIF

   DEFAULT lWithPath TO .F.

   // Ensure destination directory exists.
   IF lWithPath .AND. ! hb_DirExists( cPath )
      lRetVal := hb_DirBuild( cPath )
   ENDIF

   IF Empty( cPassword )
      cPassword := NIL
   ENDIF

   cFileName := Zip_EnsureExtension( cFileName )

   IF Empty( hUnzip := hb_UnzipOpen( cFileName ) )
      lRetVal := .F.
   ELSE
      // Normalize file list parameter.
      IF HB_ISNUMERIC( acFiles ) .OR. HB_ISSTRING( acFiles )
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

            // Determine if this specific file should be extracted based on filters.
            lExtract := ( Empty( acFiles ) .OR. ;
               AScan( acFiles, nPos ) > 0 .OR. ;
               AScan( acFiles, {| cMask | hb_FileMatch( cExtName, cMask ) } ) > 0 )

            IF lExtract
               IF cPath == "mem:"
                  cSubPath := ""
                  hb_vfErase( cPath + cSubPath + cExtName )
               ELSE
                  // Create subdirectories if they exist in the ZIP structure.
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
               
               // Normalize path separators for the current OS.
               cExtName := cPath + cSubPath + cExtName
               cExtName := StrTran( cExtName, "\", hb_ps() )
               cExtName := StrTran( cExtName, "/", hb_ps() )
               
               // Open target file (physical or virtual).
               IF ( hHandle := hb_vfOpen( cExtName, hb_bitOr( FO_CREAT, FO_TRUNC, FO_WRITE ) ) ) != NIL
                  nRead := 0
                  DO WHILE ( nLen := hb_unZipFileRead( hUnzip, @cBuffer, hb_BLen( cBuffer ) ) ) > 0
                     IF HB_ISEVALITEM( bProgress )
                        nRead += nLen
                        Eval( bProgress, nRead, nSize, cExtName )
                     ENDIF
                     hb_vfWrite( hHandle, cBuffer, nLen )
                  ENDDO

                  hb_UnzipFileClose( hUnzip )
                  hb_vfClose( hHandle )

                  // Restore original file time to the extracted file.
                  hb_vfTimeSet( cExtName, dDate, cTime )

                  IF HB_ISEVALITEM( bUpdate )
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

/*
 * Alias functions for hb_UnzipFile to maintain compatibility with 
 * various Harbour coding styles.
 */
FUNCTION hb_UnzipFileIndex( ... )
RETURN hb_UnzipFile( ... )

FUNCTION hb_UnzipAllFile( ... )
RETURN hb_UnzipFile( ... )

/*
 * FUNCTION hb_ZipDeleteFiles( cFileName, acFiles )
 *
 * Purpose:
 *    Removes specific entries from an existing ZIP archive.
 *
 * Parameters:
 *    cFileName : String - The ZIP file to modify.
 *    acFiles   : Array  - List of internal paths to delete.
 */
FUNCTION hb_ZipDeleteFiles( cFileName, acFiles )

   LOCAL lRetVal := .T.
   LOCAL cFileToProc

   cFileName := Zip_EnsureExtension( cFileName )

   IF HB_ISSTRING( acFiles )
      acFiles := { acFiles }
   ENDIF

   FOR EACH cFileToProc IN acFiles
      // Logical AND ensures we know if any single deletion failed.
      lRetVal := lRetVal .AND. ( hb_ZipDeleteFile( cFileName, cFileToProc ) == UNZ_OK )
   NEXT

RETURN lRetVal

/*
 * FUNCTION hb_IsZipFile( cFilename )
 *
 * Purpose:
 *    Validates if a file is a true ZIP archive by checking its "Magic Number" signature.
 *
 * Returns:
 *    Logical : .T. if the file starts with the 'PK' (Phil Katz) header.
 */
FUNCTION hb_IsZipFile( cFilename )

   LOCAL cSignature := "PK" + Chr( 0x03 ) + Chr( 0x04 )
   LOCAL nLength := Len( cSignature )
   LOCAL hHandle, cHeader := Space( nLength )
   LOCAL isZipFile := .F.

   TRY
      // Open with shared read access to avoid locking issues.
      IF ( hHandle := FOpen( cFilename, FO_READ + FO_SHARED ) ) <> F_ERROR
         // Ensure file is at least large enough to hold a header.
         IF FSeek( hHandle, 0, FS_END ) > nLength
            FSeek( hHandle, 0, FS_SET )

            IF FRead( hHandle, @cHeader, nLength ) == nLength
               isZipFile := ( cHeader == cSignature )
            ENDIF
         ENDIF
      ENDIF

   CATCH
      // Errors (like access denied) result in .F.
   FINALLY
      IF hHandle <> NIL
         FClose( hHandle )
      ENDIF
   END

RETURN isZipFile

/*
 * STATIC FUNCTION Zip_EnsureExtension( cFileName )
 *
 * Purpose:
 *    Internal helper to append ".zip" if missing, respecting the 
 *    SET DEFEXTENSIONS environment setting.
 */
STATIC FUNCTION Zip_EnsureExtension( cFileName )
RETURN iif( Set( _SET_DEFEXTENSIONS ), hb_FNameExtSetDef( cFileName, ".zip" ), cFileName )

/*
 * FUNCTION hb_PathIsAbsolute( cPath )
 *
 * Purpose:
 *    Determines if a path string is absolute (starts with drive or root separator).
 */
FUNCTION hb_PathIsAbsolute( cPath )

   LOCAL cDir, cDrive

   hb_FNameSplit( cPath, @cDir, , , @cDrive )

RETURN ! Empty( cDrive ) .OR. hb_LeftEq( cDir, hb_ps() )
