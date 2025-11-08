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

#define ZIP_READ_BUFFER  32768

// Static variables for buffering and settings
STATIC s_nReadBuffer := ZIP_READ_BUFFER
STATIC s_cZipComment

/*
 * PROCEDURE hb_SetZipComment( cComment )
 *
 * Sets the global comment for a Zip archive.
 *
 * Parameters:
 *   cComment (CHARACTER): The comment string to be associated with the Zip archive.
 *             If NIL is passed, the comment is cleared.
 *
 * Return Value:
 *   None.
 *
 * Purpose:
 *   This procedure allows setting or clearing the global comment associated with a Zip archive.
 *   The comment can be used to store metadata or descriptive information about the archive.
 *
 * Notes:
 *   The comment is stored in the static variable s_cZipComment and is applied when the Zip archive is closed.
 */
PROCEDURE hb_SetZipComment( cComment )
   IF cComment == NIL .OR. ISCHARACTER( cComment )
      s_cZipComment := cComment
   ENDIF

RETURN

/*
 * FUNCTION hb_GetZipComment( cFileName )
 *
 * Retrieves the global comment from a Zip archive.
 *
 * Parameters:
 *   cFileName (CHARACTER): The name of the Zip archive file.
 *
 * Return Value:
 *   CHARACTER: The global comment associated with the Zip archive. Returns an empty string ("") if the archive does not exist or has no comment.
 *
 * Purpose:
 *   This function allows reading the global comment stored within a Zip archive.
 *   This can be useful for displaying information about the archive to the user or for programmatically accessing metadata.
 *
 * Notes:
 *   The function uses hb_UnzipOpen to open the archive, retrieves the comment using hb_UnzipGlobalInfo, and then closes the archive.
 *   The Zip_EnsureExtension function is used to ensure the filename has the correct ".zip" extension.
 */
FUNCTION hb_GetZipComment( cFileName )

   LOCAL hUnzip
   LOCAL cComment

   cFileName := Zip_EnsureExtension( cFileName )

   IF ! Empty( hUnzip := hb_UnzipOpen( cFileName ) )
      hb_UnzipGlobalInfo( hUnzip, NIL, @cComment )
      hb_UnzipClose( hUnzip )
   ENDIF

   DEFAULT cComment TO ""

RETURN cComment

/*
 * FUNCTION hb_GetFileCount( cFileName )
 *
 * Retrieves the number of files contained within a Zip archive.
 *
 * Parameters:
 *   cFileName (CHARACTER): The name of the Zip archive file.
 *
 * Return Value:
 *   NUMERIC: The number of files in the Zip archive. Returns 0 if the archive does not exist or is empty.
 *
 * Purpose:
 *   This function provides a way to determine the number of files stored within a Zip archive without extracting them.
 *   This can be useful for displaying archive statistics or for pre-allocating memory before extracting files.
 *
 * Notes:
 *   The function uses hb_UnzipOpen to open the archive, retrieves the file count using hb_UnzipGlobalInfo, and then closes the archive.
 *   The Zip_EnsureExtension function is used to ensure the filename has the correct ".zip" extension.
 */
FUNCTION hb_GetFileCount( cFileName )

   LOCAL hUnzip
   LOCAL nEntries

   cFileName := Zip_EnsureExtension( cFileName )

   IF ! Empty( hUnzip := hb_UnzipOpen( cFileName ) )
      hb_UnzipGlobalInfo( hUnzip, @nEntries, NIL )
      hb_UnzipClose( hUnzip )
   ELSE
      nEntries := 0
   ENDIF

RETURN nEntries

/*
 * FUNCTION hb_ZipWithPassword( cFileName )
 *
 * Determines if a Zip archive is password-protected.
 *
 * Parameters:
 *   cFileName (CHARACTER): The name of the Zip archive file.
 *
 * Return Value:
 *   LOGICAL: .T. if the Zip archive is password-protected (encrypted), .F. otherwise.
 *
 * Purpose:
 *   This function allows checking whether a Zip archive requires a password for extraction.
 *   This is useful for prompting the user for a password before attempting to extract the archive's contents.
 *
 * Notes:
 *   The function opens the archive, reads the file information for the first file, and checks the encryption flag.
 *   The Zip_EnsureExtension function is used to ensure the filename has the correct ".zip" extension.
 */
FUNCTION hb_ZipWithPassword( cFileName )

   LOCAL lCrypted := .F.
   LOCAL hUnzip

   cFileName := Zip_EnsureExtension( cFileName )

   IF ! Empty( hUnzip := hb_UnzipOpen( cFileName ) )

      IF hb_UnzipFileFirst( hUnzip ) == 0
         hb_UnzipFileInfo( hUnzip, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, @lCrypted ) // Check if encrypted
      ENDIF
      hb_UnzipClose( hUnzip )
   ENDIF

RETURN lCrypted

/*
 * FUNCTION hb_GetFilesInZip( cFileName, lVerbose )
 *
 * Retrieves a list of files contained within a Zip archive.
 *
 * Parameters:
 *   cFileName (CHARACTER): The name of the Zip archive file.
 *   lVerbose (LOGICAL, optional):  A logical value indicating whether to return detailed file information.
 *                If .T., returns an array of arrays, each containing file details.
 *                If .F. (default), returns an array of filenames.
 *
 * Return Value:
 *   ARRAY: An array containing the list of files in the Zip archive.
 *          If lVerbose is .T., each element of the array is an array containing detailed file information:
 *            { cFileName, nSize, nMethod, nCompSize, nRatio, dDate, cTime, hb_NumToHex( nCRC, 8 ), nInternalAttr, lCrypted, cComment }
 *          If lVerbose is .F., each element of the array is the filename.
 *
 * Purpose:
 *   This function provides a way to list the files stored within a Zip archive.
 *   The lVerbose parameter allows controlling the level of detail returned for each file.
 *   This can be useful for displaying a list of files to the user or for programmatically processing the archive's contents.
 *
 * Notes:
 *   The function opens the archive, iterates through the files, and extracts the desired information.
 *   The Zip_EnsureExtension function is used to ensure the filename has the correct ".zip" extension.
 */
FUNCTION hb_GetFilesInZip( cFileName, lVerbose )

   LOCAL hUnzip, nErr, aFiles := {}
   LOCAL dDate, cTime, nSize, nCompSize, nInternalAttr, nMethod, lCrypted, cComment, nRatio, nCRC

   cFileName := Zip_EnsureExtension( cFileName )

   IF ! Empty( hUnzip := hb_UnzipOpen( cFileName ) )
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
            AAdd( aFiles, { cFileName, nSize, nMethod, nCompSize, nRatio, dDate, cTime, hb_NumToHex( nCRC, 8 ), nInternalAttr, lCrypted, cComment } )
         ELSE
            AAdd( aFiles, cFileName ) // Add just the file name to the array
         ENDIF

         nErr := hb_UnzipFileNext( hUnzip )
      ENDDO

      hb_UnzipClose( hUnzip )
   ENDIF

RETURN aFiles

/*
 * PROCEDURE hb_SetBuffer( nWriteBuffer, nExtractBuffer, nReadBuffer )
 *
 * Sets the buffer sizes for Zip archive operations.
 *
 * Parameters:
 *   nWriteBuffer (NUMERIC): The size of the write buffer (currently unused).
 *   nExtractBuffer (NUMERIC): The size of the extract buffer (currently unused).
 *   nReadBuffer (NUMERIC): The size of the read buffer.  Limited to a maximum of ZIP_READ_BUFFER (32768).
 *
 * Return Value:
 *   None.
 *
 * Purpose:
 *   This procedure allows configuring the buffer sizes used for reading, writing, and extracting Zip archive data.
 *   Adjusting the buffer sizes can potentially improve performance, especially when working with large archives.
 *
 * Notes:
 *   Only the nReadBuffer parameter is currently used. The nWriteBuffer and nExtractBuffer parameters are ignored.
 *   The read buffer size is limited to a maximum of 32KB (ZIP_READ_BUFFER) to prevent excessive memory usage.
 */
PROCEDURE hb_SetBuffer( nWriteBuffer, nExtractBuffer, nReadBuffer )
   HB_SYMBOL_UNUSED( nWriteBuffer )
   HB_SYMBOL_UNUSED( nExtractBuffer )

   IF HB_ISNUMERIC( nReadBuffer ) .AND. nReadBuffer >= 1
      s_nReadBuffer := Min( nReadBuffer, ZIP_READ_BUFFER ) // Limit the buffer size to 32KB
   ENDIF

RETURN

/*
 * FUNCTION hb_ZipFile( cFileName, acFiles, nLevel, bUpdate, lOverwrite, cPassword, lWithPath, lWithDrive, bProgress, lFullPath, acExclude )
 *
 * Creates a Zip archive containing the specified files.
 *
 * Parameters:
 *   cFileName (CHARACTER): The name of the Zip archive file to create.
 *   acFiles (ARRAY or CHARACTER): An array of filenames to include in the archive, or a single filename.  Wildcards are supported.
 *   nLevel (NUMERIC): The compression level (0-9, where 0 is no compression and 9 is maximum compression).
 *   bUpdate (BLOCK, optional): A code block to be executed for each file being added to the archive.  Takes the filename and file number as parameters.
 *   lOverwrite (LOGICAL, optional): A logical value indicating whether to overwrite an existing archive file. Defaults to .F.
 *   cPassword (CHARACTER, optional): A password to encrypt the archive.
 *   lWithPath (LOGICAL, optional): A logical value indicating whether to include the path of the files in the archive. Defaults to .F.
 *   lWithDrive (LOGICAL, optional): A logical value indicating whether to include the drive letter in the path. Defaults to .F.
 *   bProgress (BLOCK, optional): A code block to be executed to display progress. Takes the number of bytes read and the total file size as parameters.
 *   lFullPath (LOGICAL, optional): A logical value indicating whether to use the full path. Defaults to .F.
 *   acExclude (ARRAY or CHARACTER, optional): An array of filenames or wildcard patterns to exclude from the archive, or a single filename/pattern.
 *
 * Return Value:
 *   LOGICAL: .T. if the archive was created successfully, .F. otherwise.
 *
 * Purpose:
 *   This function creates a Zip archive containing the specified files, with options for compression level, password protection, and path inclusion.
 *   It provides a flexible way to create Zip archives from within a Harbour application.
 *
 * Notes:
 *   The function uses hb_ZipOpen to create the archive, hb_ZipFileCreate to add files, and hb_ZipClose to finalize the archive.
 *   Wildcards are supported in the acFiles and acExclude parameters.
 *   The bUpdate and bProgress code blocks allow for custom processing and progress display during the archiving process.
 *   The Zip_EnsureExtension function is used to ensure the filename has the correct ".zip" extension.
 */
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
   DEFAULT lFullPath TO .F.

   cFileName := Zip_EnsureExtension( cFileName )

   IF lOverwrite .AND. hb_FileExists( cFileName )
      FErase( cFileName )
   ENDIF

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

            IF lFullPath
               IF ! hb_PathIsAbsolute( cFileToZip )
                  cFileToZip := hb_PathJoin( hb_cwd(), cFileToZip )
               ENDIF
            ENDIF

            IF HB_ISBLOCK( bUpdate )
               Eval( bUpdate, cFileToZip, nPos++ )
            ENDIF

            nRead := 0
            nSize := hb_FSize( cFileToZip )

            hb_FGetDateTime( cFileToZip, @tTime )
            hb_FGetAttr( cFileToZip, @nAttr )

            hb_FNameSplit( hb_ANSIToOEM( cFileToZip ), @cPath, @cName, @cExt, @cDrive )
            IF ! lWithDrive .AND. ! Empty( cDrive ) .AND. hb_LeftEq( cPath, cDrive + ":" )
               cPath := SubStr( cPath, Len( cDrive + ":" ) + 1 )
            ENDIF
            hb_ZipFileCreate( hZip, hb_FNameMerge( iif( lWithPath, cPath, NIL ), cName, cExt, iif( lWithDrive, cDrive, NIL ) ), ;
               tTime, NIL, nAttr, nAttr, NIL, nLevel, cPassword, iif( Empty( cPassword ), NIL, hb_ZipFileCRC32( cFileToZip ) ), NIL )

            DO WHILE ( nLen := FRead( hHandle, @cBuffer, hb_BLen( cBuffer ) ) ) > 0

               IF HB_ISBLOCK( bProgress )
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

      hb_ZipClose( hZip, s_cZipComment )
   ELSE
      lRetVal := .F.
   ENDIF

RETURN lRetVal

/*
 * FUNCTION hb_UnzipFile( cFileName, bUpdate, lWithPath, cPassword, cPath, acFiles, bProgress )
 *
 * Extracts files from a Zip archive.
 *
 * Parameters:
 *   cFileName (CHARACTER): The name of the Zip archive file to extract.
 *   bUpdate (BLOCK, optional): A code block to be executed for each file being extracted. Takes the Zip filename, file number, and extracted filename as parameters.
 *   lWithPath (LOGICAL, optional): A logical value indicating whether to create subdirectories based on the paths stored in the archive. Defaults to .F.
 *   cPassword (CHARACTER, optional): A password to decrypt the archive, if it is password-protected.
 *   cPath (CHARACTER, optional): The destination directory for the extracted files. If "mem:", files are extracted to memory using virtual file system.
 *   acFiles (ARRAY or CHARACTER, optional): An array of filenames or file indexes to extract, or a single filename/index. If empty, all files are extracted.
 *   bProgress (BLOCK, optional): A code block to be executed to display progress. Takes the number of bytes read, the total file size, and the extracted filename as parameters.
 *
 * Return Value:
 *   LOGICAL: .T. if the extraction was successful, .F. otherwise.
 *
 * Purpose:
 *   This function extracts files from a Zip archive, with options for password protection, path creation, and selective extraction.
 *   It provides a flexible way to extract Zip archives from within a Harbour application.
 *
 * Notes:
 *   The function uses hb_UnzipOpen to open the archive, hb_UnzipFileFirst and hb_UnzipFileNext to iterate through the files, and hb_UnzipFileOpen and hb_unZipFileRead to extract the file data.
 *   The bUpdate and bProgress code blocks allow for custom processing and progress display during the extraction process.
 *   The Zip_EnsureExtension function is used to ensure the filename has the correct ".zip" extension.
 *   If cPath is "mem:", the files are extracted to a virtual file system (VFS) managed by Harbour.
 */
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

   IF HB_ISSTRING( cPath ) .AND. Lower( cPath ) == "mem:"
      cPath := Lower( cPath )
      lWithPath := .F.
   ENDIF

   DEFAULT lWithPath TO .F.

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
      IF HB_ISNUMERIC( acFiles ) .OR. ;
            HB_ISSTRING( acFiles )
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
               cExtName := StrTran( cExtName, "\", hb_ps() )
               cExtName := StrTran( cExtName, "/", hb_ps() )
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
 * FUNCTION hb_UnzipFileIndex( ... )
 *
 * Extracts files from a Zip archive (alias for hb_UnzipFile).
 *
 * Parameters:
 *   ... (VARIABLE):  Accepts the same parameters as hb_UnzipFile.
 *
 * Return Value:
 *   LOGICAL: Returns the same value as hb_UnzipFile.
 *
 * Purpose:
 *   This function is an alias for hb_UnzipFile, providing an alternative name for the same functionality.
 *   It extracts files from a Zip archive based on their index.
 *
 * Notes:
 *   This function simply calls hb_UnzipFile with the provided parameters.
 */
FUNCTION hb_UnzipFileIndex( ... )
RETURN hb_UnzipFile( ... )

/*
 * FUNCTION hb_UnzipAllFile( ... )
 *
 * Extracts all files from a Zip archive (alias for hb_UnzipFile).
 *
 * Parameters:
 *   ... (VARIABLE):  Accepts the same parameters as hb_UnzipFile.
 *
 * Return Value:
 *   LOGICAL: Returns the same value as hb_UnzipFile.
 *
 * Purpose:
 *   This function is an alias for hb_UnzipFile, providing an alternative name for the same functionality.
 *   It extracts all files from a Zip archive.
 *
 * Notes:
 *   This function simply calls hb_UnzipFile with the provided parameters.
 */
FUNCTION hb_UnzipAllFile( ... )
RETURN hb_UnzipFile( ... )

/*
 * FUNCTION hb_ZipDeleteFiles( cFileName, acFiles )
 *
 * Deletes files from a Zip archive.
 *
 * Parameters:
 *   cFileName (CHARACTER): The name of the Zip archive file.
 *   acFiles (ARRAY or CHARACTER): An array of filenames to delete from the archive, or a single filename.
 *
 * Return Value:
 *   LOGICAL: .T. if all files were deleted successfully, .F. otherwise.
 *
 * Purpose:
 *   This function allows deleting specific files from a Zip archive.
 *   This can be useful for updating or modifying the contents of an existing archive.
 *
 * Notes:
 *   The function iterates through the list of files to delete and calls hb_ZipDeleteFile for each file.
 *   The Zip_EnsureExtension function is used to ensure the filename has the correct ".zip" extension.
 */
FUNCTION hb_ZipDeleteFiles( cFileName, acFiles )

   LOCAL lRetVal := .T.
   LOCAL cFileToProc

   cFileName := Zip_EnsureExtension( cFileName )

   IF HB_ISSTRING( acFiles )
      acFiles := { acFiles }
   ENDIF

   FOR EACH cFileToProc IN acFiles
      lRetVal := lRetVal .AND. ( hb_ZipDeleteFile( cFileName, cFileToProc ) == UNZ_OK )
   NEXT

RETURN lRetVal

/*
 * FUNCTION hb_IsZipFile( cFilename )
 *
 * Checks if a file is a valid Zip archive based on its signature.
 *
 * Parameters:
 *   cFilename (CHARACTER): The name of the file to check.
 *
 * Return Value:
 *   LOGICAL: .T. if the file is a valid Zip archive, .F. otherwise.
 *
 * Purpose:
 *   This function determines whether a given file is a valid Zip archive by checking for the standard Zip file signature ("PK\003\004") at the beginning of the file.
 *   This can be useful for validating file types before attempting to extract or process them as Zip archives.
 *
 * Notes:
 *   The function reads the first four bytes of the file and compares them to the Zip file signature.
 *   It handles potential file errors using a TRY...CATCH...FINALLY block.
 */
FUNCTION hb_IsZipFile( cFilename )

   LOCAL cSignature := "PK" + Chr( 0x03 ) + Chr( 0x04 )
   LOCAL nLength := Len( cSignature )
   LOCAL hHandle, cHeader := Space( nLength )
   LOCAL isZipFile := .F.

   TRY
      IF ( hHandle := FOpen( cFilename, FO_READ + FO_SHARED ) ) <> F_ERROR

         IF FSeek( hHandle, 0, FS_END ) > nLength
            FSeek( hHandle, 0, FS_SET )

            IF FRead( hHandle, @cHeader, nLength ) == nLength
               isZipFile := ( cHeader == cSignature )
            ENDIF
         ENDIF
      ENDIF

   CATCH
      // silently ignore errors

   FINALLY
      IF hHandle <> NIL
         FClose( hHandle )
      ENDIF
   END

RETURN isZipFile

/*
 * STATIC FUNCTION Zip_EnsureExtension( cFileName )
 *
 * Ensures that a filename has the ".zip" extension.
 *
 * Parameters:
 *   cFileName (CHARACTER): The filename to check and modify.
 *
 * Return Value:
 *   CHARACTER: The filename with the ".zip" extension added if it was missing, or the original filename if it already had the extension.
 *
 * Purpose:
 *   This function ensures that a filename has the ".zip" extension, adding it if necessary.
 *   It uses the Set( _SET_DEFEXTENSIONS ) setting to determine whether to automatically add the extension.
 *
 * Notes:
 *   This function is used internally to ensure that filenames passed to Zip archive functions have the correct extension.
 */
STATIC FUNCTION Zip_EnsureExtension( cFileName )
RETURN iif( Set( _SET_DEFEXTENSIONS ), hb_FNameExtSetDef( cFileName, ".zip" ), cFileName )

/*
 * FUNCTION hb_PathIsAbsolute( cPath )
 *
 * Checks if a path is absolute.
 *
 * Parameters:
 *   cPath (CHARACTER): The path to check.
 *
 * Return Value:
 *   LOGICAL: .T. if the path is absolute, .F. otherwise.
 *
 * Purpose:
 *   This function determines whether a given path is absolute.
 *
 * Notes:
 *   Uses hb_FNameSplit to determine if the path has a drive or starts with a path separator.
 */
FUNCTION hb_PathIsAbsolute( cPath )

   LOCAL cDir, cDrive

   hb_FNameSplit( cPath, @cDir, , , @cDrive )

RETURN ! Empty( cDrive ) .OR. hb_LeftEq( cDir, hb_ps() )
