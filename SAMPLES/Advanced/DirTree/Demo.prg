/******
*
* MINIGUI - Harbour Win32 GUI library Demo
*
* Build tree of folders, files and archives
*
* (c) 2008-2009 Vladimir Chumachenko <ChVolodymyr@yandex.ru>
*
* Last modifed 2021.11.19 by Grigory Filatov <gfilatov@inbox.ru>
*
*/


/*
Change history.

+ added
* changed
- deleted

August 2009

* Files from the Zip archive are extracted and opened normally.
  Solved by replacing the hbzlib.lib library with ziparchive.lib and calling
  HB_OEMtoANSI () procedures during processing of technical file lines
  information created by 7-Zip
* to get a table of contents of Zip archives instead of the ZipIndex() function
  uses the standard function HB_GetFilesInZip(). ZipIndex() is left in a
  program text in a commented form (just in case).
+ latest versions of 7-Zip add technical information about
  contents of the archive name of the archive itself, as a result of which in the tree
  superfluous elements are created. Therefore, when processing strings containing
  the name of the archive being processed is ignored.
+ interruption of the operation of tree formation

October 2008

The initial version.
*/


#include "Directry.ch"
#include "Minigui.ch"

// Program files of the 7-Zip archiver

#define FULL_7Z             '7z.exe'         // Full version
#define DLL_7Z              '7z.dll'         // Library to the full version
#define ALONE_7Z            '7za.exe'        // Console option

#define TEMP_FOLDER         ( GetTempFolder() + '\' )
#define TMP_ARC_INDEX       ( TEMP_FOLDER + '_Arc_.lst' )     // Temporary file for displaying archive contents

// Abort processing

#translate BREAK_ACTION_()                                                                                 ;
      => lBreak := MsgYesNo( 'Stop operation?', 'Confirm action', .T., , .F., .F. )

// Change the processing button

// 1) Initial value: start scan
#translate SET_DOSCAN_() ;
      => wMain.ButtonEX_1.CAPTION := 'Scan' ;
         ; wMain.ButtonEX_1.PICTURE := 'OK' ;
         ; wMain.ButtonEX_1.ACTION := {|| BuildTree() }
// 2) Building a tree: aborting processing
#translate SET_STOP_SCAN_() ;
      => wMain.ButtonEX_1.CAPTION := '[Esc] Stop' ;
         ; wMain.ButtonEX_1.PICTURE := 'STOP' ;
         ; wMain.ButtonEX_1.ACTION := {|| BREAK_ACTION_() }

#translate SetEnvironmentVariable( <n>, <v> ) ;
      => hb_SetEnv( <n>, <v> )

STATIC cApp7z := '' // 7-Zip Archiver
STATIC cOSPaths := '' // Value of the PATH system variable (used only for the full 7-Zip version)

MEMVAR lBreak // Abort Processing


/******
*
*    Directory and files tree
*
*/

PROCEDURE Main()

   LOCAL cSysPath := Upper( GetEnv( 'PATH' ) ), ;
      cPath7z := '', ;
      oReg

   SET FONT TO 'Tahoma', 9

/* To work with archives (except Zip) we use 7-Zip. We check one of the options:
   - the archiver is installed (full version);
   - the files 7z.exe and 7z.dll are placed in the program directory (the archiver is not installed, but
   the functionality is almost the same as in the installation version);
   - the console version is located in the program directory (7za.exe)
   Choose the best option. To do this, check the availability of the necessary programs. Accommodation
   the full version of 7-Zip (preferred use) is searched through the registry entry
   In addition, when using 7-Zip, located in Program Files, there are some disadvantages
   command line - a command is not executed in which both the program name and the parameter file
   names with spaces are used:
   %COMSPEC% /C "%\ProgramFiles%\7-Zip\7z.exe" L -slt "Some data.7z"
   To work around this situation, add the search path 7z.exe to the PATH system variable, and after
   completion of processing - restore the original PATH value.
   If 7z.exe and 7z.dll are in the program directory, the PATH value does not change. */

   // For Zip archives always use the built-in features.

   Open registry oReg KEY HKEY_LOCAL_MACHINE SECTION 'Software\7-Zip'
   GET VALUE cPath7z NAME 'Path' OF oReg
   CLOSE registry oReg

   If ! Empty( cPath7z ) // Installed Version

      cPath7z := Upper( cPath7z )

      If !( cPath7z $ cSysPath )

         cOSPaths := cSysPath

         If !( Right( cOSPaths, 1 ) == ';' )
            cOSPaths += ';'
         ENDIF

         cOSPaths += ( cPath7z + '\' )
         cApp7z := FULL_7Z

      ENDIF

   ELSEIF ( File( FULL_7Z ) .AND. File( DLL_7Z ) ) // The directory with the program contains 7z.exe and 7z.dll
      cApp7z := FULL_7Z

   ELSEIF File( ALONE_7Z ) // The program directory contains 7za.exe
      cApp7z := ALONE_7Z

   ENDIF

   LOAD WINDOW Demo AS wMain

   wMain.BtnTextBox_1.VALUE := GetMyDocumentsFolder() // The default directory for scanning

   // If a console version of 7-Zip is detected, we expand the list of available archive formats,
   // although, for example, to process RAR you need to use the full version of 7-Zip

   If ! Empty( cApp7z )

      // Supported archive types for full and console versions

      IF ( cApp7z == FULL_7Z )
         wMain.Combo_1.AddItem( 'ZIP; 7Z; RAR; CAB; ARJ; LZH' )
      ELSE
         wMain.Combo_1.AddItem( 'ZIP; 7Z' )
      ENDIF

      wMain.Combo_1.VALUE := 2

   ENDIF

   SET_DOSCAN_()

   wMain.ButtonEX_2.Enabled := .F.
   wMain.ButtonEX_3.Enabled := .F.
   wMain.ButtonEX_4.Enabled := .F.

   CENTER WINDOW wMain
   ACTIVATE WINDOW wMain

RETURN

***** End of Main ******


/******
*
*       SelectDir()
*
*       Select a directory to scan
*
*/

STATIC PROCEDURE SelectDir

   LOCAL cPath := AllTrim( wMain.BtnTextBox_1.Value )

   If ! Empty( cPath := GetFolder( 'Select folder', cPath ) )
      wMain.BtnTextBox_1.VALUE := cPath
   ENDIF

RETURN

***** End of SelectDir ******


/******
*
*       BuildTree()
*
*       Building a tree
*
*/

STATIC PROCEDURE BuildTree

   LOCAL cPath := wMain.BtnTextBox_1.VALUE, ;
      cSavePath := ''

   PRIVATE lBreak := .F. // Abort Processing
   SET_STOP_SCAN_()
   ON KEY ESCAPE OF wMain ACTION BREAK_ACTION_()

   If ! Empty( cPath )

      // To use the installation version of 7-Zip, change the system
      // variable PATH

      If ! Empty( cOSPaths )
         cSavePath := GetEnv( 'PATH' )
         SetEnvironmentVariable( 'PATH', cOSPaths )
      ENDIF

      wMain.Tree_1.DeleteAllItems
      wMain.Tree_1.DisableUpdate

      // First, add the root node

      Node wMain.BtnTextBox_1.VALUE Images { 'STRUCTURE' }
         ScanDir( cPath )
      END Node
      wMain.StatusBar.Item( 1 ) := ''

      // Restore the original value of the system variable PATH (if it
      // was changed).

      If ! Empty( cSavePath )
         SetEnvironmentVariable( 'PATH', cSavePath )
      ENDIF

      wMain.Tree_1.Expand( 1 )
      wMain.Tree_1.EnableUpdate

      wMain.Tree_1.VALUE := 1
      wMain.Tree_1.SetFocus

      IF ( wMain.Tree_1.ItemCount > 1 )
         wMain.ButtonEX_2.Enabled := .T.
         wMain.ButtonEX_3.Enabled := .T.
         wMain.ButtonEX_4.Enabled := .T.
      ELSE
         wMain.ButtonEX_2.Enabled := .F.
         wMain.ButtonEX_3.Enabled := .F.
         wMain.ButtonEX_4.Enabled := .T.
      ENDIF

   ENDIF

   SET_DOSCAN_()
   RELEASE KEY ESCAPE OF wMain

RETURN

***** End of BuildTree ******


/******
*
*       ScanDir( cPath )
*
*       Directory scan
*
*/

STATIC PROCEDURE ScanDir( cPath )

   LOCAL cMask := AllTrim( wMain.Text_1.Value ), ;
      cAttr := iif( wMain.Check_1.VALUE, 'H', '' ), ;
      aFullList, ;
      aDir := {}, ;
      aFiles, ;
      xItem

   If !( Right( cPath, 1 ) == '\' )
      cPath += '\'
   ENDIF

   BEGIN SEQUENCE

      // Since a mask can be used for sampling, we proceed as follows.
      // 1) We get a list of ALL subdirectories of the selected directory (the mask is not taken into account,
      // since the subdirectories themselves can be ignored)
      // 2) For each subdirectory, a list of files belonging to it is formed
      // TAKING INTO TEMPLATE
      // 3) A subdirectory is not added to the tree if there are no required files and is not allowed
      // add empty directories

      If ! Empty( aFullList := ASort( Directory( cPath, ( 'D' + cAttr ) ),,, ;
            {| x, y | Upper( x[ F_NAME ] ) < Upper( y[ F_NAME ] ) } ) )

         FOR EACH xItem IN aFullList

            IF ( 'D' $ xItem[ F_ATTR ] )
               IF ( !( xItem[ F_NAME ] == '.' ) .AND. !( xItem[ F_NAME ] == '..' ) )
                  AAdd( aDir, xItem[ F_NAME ] )
               ENDIF
            ENDIF

            DO Events

            IF lBreak
               Break
            ENDIF

         NEXT

      ENDIF

      // Process the resulting directory listing. In this case, recursive
      // call the procedure to scan deeper levels

      If ! Empty( aDir )

         FOR EACH xItem IN aDir

            // Before adding the directory node, check for the presence in it
            // files matching the pattern or subdirectories. The names of the files themselves so far
            // not important.

            // Although you can only check for files:
            // If !Empty( Directory( ( cPath + xItem + '\' + cMask ), cAttr ) )
            // Only in this case directories in which there are no files (for a given mask),
            // but there are subdirectories, will not be included in the construction, as well as files located
            // in them.

            IF ( ! Empty( Directory( ( cPath + xItem + '\' + cMask ), cAttr ) ) .OR. ;
                  ( wMain.Check_3.VALUE .AND. ;
                  ! Empty( Directory( ( cPath + xItem ), ( 'D' + cAttr ) ) ) ;
                  ) ;
                  )

               Node xItem
                  ScanDir( cPath + xItem )
               END Node

               DO Events

               IF lBreak
                  Break
               ENDIF

            ENDIF

         NEXT

      ENDIF

      // Add a list of files

      If ! Empty( aFiles := ASort( Directory( ( cPath + cMask ), cAttr ),,, ;
            {| x, y | Upper( x[ F_NAME ] ) < Upper( y[ F_NAME ] ) } ) )

         FOR EACH xItem IN aFiles

            wMain.StatusBar.Item( 1 ) := ( cPath + xItem[ F_NAME ] )

            DO Events

            If ! wMain.Check_2.VALUE // Do not open archives
               TreeItem xItem[ F_NAME ]
            ELSE
               GetArc( cPath, xItem[ F_NAME ] )
            ENDIF

            IF lBreak
               Break
            ENDIF

         NEXT

      ENDIF

   END

RETURN

***** End of ScanDir ******


/******
*
*       GetArc( cPath, cFile )
*
*       Processing archive file
*
*/

STATIC PROCEDURE GetArc( cPath, cFile )

   LOCAL cArcTypes := wMain.Combo_1.DisplayValue, ;
      cExt, ;
      aFileList, ;
      cItem

   hb_FNameSplit( cFile, , , @cExt )

   If ! Empty( cExt := Upper( cExt ) )

      IF ( Left( cExt, 1 ) == '.' )
         cExt := SubStr( cExt, 2 )
      ENDIF

      // If the extension belongs to the archive type, we get the content
      // archive. In this case, ZIP archives are processed by our own means.
      // and the rest - with an external archiver

      If !( cExt $ cArcTypes )
         TreeItem cFile

      ELSE

         // The contents of the archive are obtained in 2 ways: by our own processing
         // ZIP and the launch of 7-Zip. A list of files in the Zip archive can be obtained and
         // ready function HB_GetFilesInZip (cPath + cFile).
         // In previous versions of Harbor, a program crash with the system
         // an error when performing this function on a large number of archives,
         // therefore, the ZipIndex () function was created. But now everything seems to be
         // normal, therefore ZipIndex () is left in the program text, but not used.

         TRY
            // aFileList := Iif( ( cExt == 'ZIP' ), ZipIndex( cPath + cFile ), ArcIndex( cPath + cFile ) )
            aFileList := iif( ( cExt == 'ZIP' ), HB_GetFilesInZip( cPath + cFile ), ArcIndex( cPath + cFile ) )
         CATCH
            aFileList := {}
         END

         If ! Empty( aFileList )

            Node cFile Images iif( ( cExt == 'ZIP' ), { 'ARC_ZIP' }, { 'ARC_7ZIP' } )
               FOR EACH cItem IN aFileList
                  TreeItem hb_OEMToANSI( cItem )
               NEXT
            END Node

         ELSE
            TreeItem cFile

         ENDIF

      ENDIF

   ELSE
      TreeItem cFile

   ENDIF

RETURN

***** End of GetArc ******


// ZipIndex () function was used as an analog of HB_GetFilesInZip (), but
// Harbor versions no longer need it. Left for the story.
#if 0
/******
*
*       ZipIndex( cArcFile ) --> aFiles
*
*       List of files in the ZIP archive
*
*/

STATIC FUNCTION ZipIndex( cArcFile )

   LOCAL aFiles := {}, ;
      hUnzip := HB_UnZipOpen( cArcFile ), ;
      nError, ;
      cFile

   If ! Empty( hUnzip )

      nError := HB_UnZipFileFirst( hUnzip )

      DO WHILE Empty( nError )

         HB_UnZipFileInfo( hUnzip, @cFile )

         AAdd( aFiles, cFile )

         nError := HB_UnZipFileNext( hUnzip )

      ENDDO

      HB_UnZipClose( hUnzip )

   ENDIF

RETURN aFiles

#endif

***** End of ZipIndex ******


/******
*
*       ArcIndex( cArcFile ) --> aFiles
*
*       A list of files in a non-Zip archive
*
*/

STATIC FUNCTION ArcIndex( cArcFile )

   LOCAL aFiles := {}, ;
      cCommand := ( GetEnv( 'COMSPEC' ) + ' /C ' + cApp7z ), ;
      cString, oFile

   // The contents of the archive are displayed in a temporary file and then read for display in
   // program.

   // And the information will not be displayed in the table, but in the technical mode (switch
   // -slt). Then each file file will be described in several lines like this
   // (varies depending on the type of archive):
   // Path = Our archive file
   // Size =
   // Packed Size =
   // Modified =
   // Attributes =
   // CRC =
   // Method =
   // Block =
   // and the name of the archive element will be displayed in the line marked Path =

   cCommand += ( ' L -slt "' + cArcFile + '" > ' + TMP_ARC_INDEX )
   Execute File ( cCommand ) WAIT Hide

   IF File( TMP_ARC_INDEX )

      // Temporary file may not be created, for example, due to errors
      // in the command line. Additionally, it would not hurt to check its size.
      // If zero, then there is nothing in it.

      // Fill The Array

      oFile := TFileRead():New( TMP_ARC_INDEX )
      oFile:Open()

      If ! oFile:Error()

         DO WHILE oFile:MoreToRead()

            If ! Empty( cString := oFile:ReadLine() )

               // Somewhat simplified processing. Just checking does not start
               // whether the line with "Path =" and, if so, then this is the name of the file. At
               // necessary, can be made more complicated. For example, ignore
               // directory names (string "Attributes = D ...." for .7z files)

               IF ( Left( cString, 7 ) == 'Path = ' )

                  cString := AllTrim( SubStr( cString, 8 ) )

                  // The latest versions of 7-Zip are added to the archive content report
                  // name of the archive itself (also in the line "Path =").
                  // Therefore, we introduce a check.

                  If !( Upper( cArcFile ) == Upper( cString ) )
                     AAdd( aFiles, cString )
                  ENDIF

               ENDIF

            ENDIF

         ENDDO

         oFile:Close()

      ENDIF

   ENDIF

   // The temporary file also played a role and may be deleted.

   ERASE ( TMP_ARC_INDEX )

RETURN aFiles

***** End of ArcIndex ******


/******
*
*       ShowTreeNode( nMode )
*
*       Expand (1) or collapse (0) all nodes of the tree
*
*/

STATIC PROCEDURE ShowTreeNode( nMode )

   LOCAL nCount := wMain.Tree_1.ItemCount, ;
      Cycle

   If ! Empty( nCount )

      wMain.Tree_1.DisableUpdate

      FOR Cycle := 1 TO nCount

         // Only process elements that do not have child branches (nodes)

         IF IsTreeNode( 'wMain', 'Tree_1', Cycle )

            IF ( nMode == 1 )
               wMain.Tree_1.Expand( Cycle )
            ELSE
               wMain.Tree_1.Collapse( Cycle )
            ENDIF

         ENDIF

      NEXT

      IF ( nMode == 1 )
         wMain.Tree_1.VALUE := 1
      ENDIF

      wMain.Tree_1.EnableUpdate
      wMain.Tree_1.SetFocus

   ENDIF

RETURN

***** End of ShowTreeNode ******


/******
*
*       IsTreeNode( cFormName, cTreeName, nPos ) --> lIsNode
*
*       Check if a tree element is a node
*
*/

STATIC FUNCTION IsTreeNode( cFormName, cTreeName, nPos )

   LOCAL nVal := GetProperty( cFormName, cTreeName, 'Value' ), ;
      nAmount := GetProperty( cFormName, cTreeName, 'ItemCount' ), ;
      nIndex, ;
      nHandle, ;
      nTreeItemHandle

   IF ( ValType( nPos ) == 'N' )
      IF ( ( nPos > 0 ) .AND. ( nPos <= nAmount ) )
         nVal := nPos
      ENDIF
   ENDIF

   nIndex := GetControlIndex( cTreeName, cFormName )
   nHandle := _HMG_aControlHandles[ nIndex ]
   nTreeItemHandle := _HMG_aControlPageMap[ nIndex, nVal ]

   // A tree element is considered a node if it has subordinate elements

Return ! Empty( TreeView_GetChild( nHandle, nTreeItemHandle ) )

***** End of IsTreeNode ******


/******
*
*       OpenObj( cFormName, cTreeName )
*
*       Open the current object represented by a tree item
*
*/

STATIC PROCEDURE OpenObj( cFormName, cTreeName )

   LOCAL nVal := GetProperty( cFormName, cTreeName, 'Value' ), ;
      nIndex, ;
      nHandle, ;
      nTreeHandle, ;
      nTreeItemHandle, ;
      nTempHandle, ;
      cChain, ;
      aTokens, ;
      cArcName := '', ;
      cElem, ;
      cExt, ;
      cSavePath := '', ;
      cCommand := ( GetEnv( 'COMSPEC' ) + ' /C ' + cApp7z )

   IF Empty( nVal )
      RETURN
   ENDIF

   // Process the branch in reverse order to determine the route to the file

   nTreeHandle := GetControlHandle( cTreeName, cFormName )

   nIndex := GetControlIndex( cTreeName, cFormName )
   nHandle := _HMG_aControlHandles[ nIndex ]

   nTreeItemHandle := _HMG_aControlPageMap[ nIndex, nVal ]

   cChain := TreeView_GetItem( nTreeHandle, nTreeItemHandle )
   nTempHandle := TreeView_GetParent( nHandle, nTreeItemHandle )

   DO while ! Empty( nTempHandle )
      nTreeItemHandle := nTempHandle
      nTempHandle := TreeView_GetParent( nHandle, nTreeItemHandle )
      cChain := ( TreeView_GetItem( nTreeHandle, nTreeItemHandle ) + ;
         iif( Right( TreeView_GetItem( nTreeHandle, nTreeItemHandle ), 1 ) == '\', '', '\' ) + cChain )
   ENDDO

   // The received value may be directory, file or file in the archive. In the latter case
   // the file can be located inside a subdirectory recorded in the archive.

   IF ( hb_DirExists( cChain ) .OR. File( cChain ) )

      // Directory or file. We open an overview for the directory, for the file - run the associated program.
      // !!! Files with the Hidden attribute do not fall into this branch.

      Execute operation 'Open' File ( '"' + cChain + '"' )

   ELSE

      // File in the archive. We break the full name and determine the name of the archive itself.

      aTokens := hb_ATokens( cChain, '\' )

      FOR EACH cElem IN aTokens

         If ! Empty( cArcName )
            cArcName += '\'
         ENDIF

         cArcName += cElem

         IF File( cArcName )
            EXIT
         ENDIF

      NEXT

      // Exclude the name of the archive from the resulting description line.

      cChain := SubStr( cChain, ( Len( cArcName ) + 1 ) ) // Now this is the name of the file in the archive

      IF ( Left( cChain, 1 ) == '\' )
         cChain := SubStr( cChain, 2 )
      ENDIF

      // Run a file check again, since this
      // branch gets processing files with the attribute "Hidden".

      IF File( cArcName )

         hb_FNameSplit( cArcName, , , @cExt )

         If ! Empty( cExt := Upper( cExt ) )

            IF ( Left( cExt, 1 ) == '.' )
               cExt := SubStr( cExt, 2 )
            ENDIF

            IF ( cExt == 'ZIP' )

               // ZIP archives are processed by our own means.

               // !!! Files in the archive with the Cyrillic alphabet in the name of the internal ZIP are not extracted.
               // Better Use 7-Zip

               IF HB_UnZipFile( cArcName,,,, TEMP_FOLDER, cChain )

                  // Subdirectories in the Zip archive can be separated by a forward slash, therefore
                  // convert the path.

                  cChain := Slashs( cChain )

                  // Run the associated viewer, wait for it to complete
                  // and delete the extracted file (if necessary, the directory).

                  // !!! Data in the archive is not updated.

                  ShowFile( cChain )

                  If ! Empty( nVal := At( '\', cChain ) )

                     cChain := Left( cChain, ( nVal - 1 ) )

                     IF hb_DirExists( TEMP_FOLDER + cChain )
                        DirRemove( TEMP_FOLDER + cChain )
                     ENDIF

                  ENDIF

               ENDIF

            ELSE

               // Processing 7-Zip

               If ! Empty( cOSPaths )
                  cSavePath := GetEnv( 'PATH' )
                  SetEnvironmentVariable( 'PATH', cOSPaths )
               ENDIF

               cCommand += ( ' E -y -o' + TEMP_FOLDER + ' "' + cArcName + '" "' + cChain + '"' )

               Execute File ( cCommand ) WAIT Hide

               If ! Empty( cSavePath )
                  SetEnvironmentVariable( 'PATH', cSavePath )
               ENDIF

               cChain := Slashs( cChain )

               // Here you only need to browse by file name.

               If ! Empty( nVal := ( RAt( '\', cChain ) ) )
                  cChain := SubStr( cChain, ( nVal + 1 ) )
               ENDIF

               ShowFile( cChain )

            ENDIF

         ENDIF

      ENDIF

   ENDIF

RETURN

***** End of OpenObj ******


/******
*
*       Slashs( cPath ) --> cPath
*
*       Change directory separators used in
*       archives, on system
*
*/

STATIC FUNCTION Slashs( cPath )

   If ! Empty( At( '/', cPath ) )
      cPath := StrTran( cPath, '/', '\' )
   ENDIF

RETURN cPath

***** End of Slashs ******


/******
*
*       ShowFile( cChain )
*
*       Open the file extracted from the archive.
*       Once the viewing is completed, the file is deleted.
*
*/

STATIC PROCEDURE ShowFile( cChain )

   LOCAL cExt, ;
      cApp, ;
      nPos

   // If the received file name contains part of the path (saved in the archive),
   // need to get rid of it.

   IF ( ( nPos := RAt( '\', cChain ) ) > 0 )
      cChain := SubStr( cChain, ( nPos + 1 ) )
   ENDIF

   IF File( TEMP_FOLDER + cChain )

      hb_FNameSplit( cChain, , , @cExt )

      // Define the associated program and open the file in it.
      // If the program will not be matched, simply try
      // execute the extracted file (as an option, you can assign
      // program in which all files will be opened).

      If ! Empty( cApp := GetOpenCommand( cExt ) )
         Execute File ( cApp + ' "' + TEMP_FOLDER + cChain + '"' ) WAIT
      ELSE
         Execute File ( TEMP_FOLDER + cChain ) WAIT
      ENDIF

      Erase( TEMP_FOLDER + cChain )

   ENDIF

RETURN

***** End of ShowFile ******


/******
*
*       GetOpenCommand( cExt )
*
*       Definition of an extension related program.
*
*/

STATIC FUNCTION GetOpenCommand( cExt )

   LOCAL oReg, ;
      cVar1, ;
      cVar2 := '', ;
      nPos

   If ! IsChar( cExt )
      RETURN ''
   ENDIF

   // Operating principle. In HKEY_CLASSES_ROOT we are looking for a branch that matches the passed
   // extension (with leading point) and determine the name of the file type (parameter
   // "(Default)". For example, for the extension "jpg" we look for HKEY_CLASSES_ROOT\.jpg
   // and get the name of the association - "jpegfile".
   // In the same branch HKEY_CLASSES_ROOT, we are looking for the start line of the program associated with
   // this file type (HKEY_CLASSES_ROOT\<association name>\shell\open\command)
   // For example, HKEY_CLASSES_ROOT\jpegfile\shell\open\command
   // The value of the parameter ((Default) "contains an open command of this type
   // file: "C:\\Program Files\\Internet Explorer\\iexplore.exe\" -nohome

   IF ( ! Left( cExt, 1 ) == '.' )
      cExt := ( '.' + cExt )
   ENDIF

   oReg := TReg32() : New( HKEY_CLASSES_ROOT, cExt, .F. )
   cVar1 := RTrim( StrTran( oReg : Get( NIL, '' ), Chr( 0 ), ' ' ) ) // The key value is "(Default)"
   oReg : Close()

   If ! Empty( cVar1 )

      oReg := TReg32() : New( HKEY_CLASSES_ROOT, ( cVar1 + '\shell\open\command' ), .F. )
      cVar2 := RTrim( StrTran( oReg : Get( NIL, '' ), Chr( 0 ), ' ' ) ) // The key value is "(Default)"
      oReg : Close()

      // Processing instructions for passing parameters to the associated program

      IF ( nPos := RAt( ' %1', cVar2 ) ) > 0 // he parameter is not surrounded by quotation marks (Notepad)
         cVar2 := SubStr( cVar2, 1, nPos )

      ELSEIF ( nPos := RAt( '"%', cVar2 ) ) > 0 // Parameters of the form"% 1 ","% L ", etc. (with quotes)
         cVar2 := SubStr( cVar2, 1, ( nPos - 1 ) )

      ELSEIF ( nPos := RAt( '%', cVar2 ) ) > 0 // Parameters of the form "% 1", "% L", etc. (without quotes)
         cVar2 := SubStr( cVar2, 1, ( nPos - 1 ) )

      ELSEIF ( nPos := RAt( ' /', cVar2 ) ) > 0 // Insert "/"
         cVar2 := SubStr( cVar2, 1, ( nPos - 1 ) )

      ENDIF

   ENDIF

RETURN RTrim( cVar2 )

***** End of GetOpenCommand ******
