/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Access to 7z archives by 7-zip32.dll
 * (c) 2008 Vladimir Chumachenko <ChVolodymyr@yandex.ru>
 *
 * Last Revised by Grigory Filatov 03/10/2017
*/

// Complementary libraries:
// xhb.lib, hbdll32.lib

// Complementary header files

#include "CStruct.ch"                      // from Harbour\Contrib\xHB
#include "HBCTypes.ch"                     // from Harbour\Contrib\xHB
#include "WinTypes.ch"                     // from Harbour\Contrib\xHB

#include "MiniGUI.ch"


#define ALONE_7Z          '7za.exe'        // console variant of 7-Zip archiver


STATIC cPath7z := ''      // Full path to installed 7-Zip archiver


// C-structure, used in SevenZipFindFirst(), SevenZipFindNext()

pragma pack( 4 )

#define FNAME_MAX32       512

typedef struct { ;
      DWORD dwOriginalSize;
      DWORD dwCompressedSize;
      DWORD dwCRC;
      UINT uFlag;
      UINT uOSType;
      WORD wRatio;
      WORD wDate;
      WORD wTime;
      char szFileName[ FNAME_MAX32 + 1 ];
      char dummy1[ 3 ];
      char szAttribute[ 8 ];
      char szMode[ 8 ];
      } INDIVIDUALINFO, * PINDIVIDUALINFO;



/******
*
*       Access to 7z, zip archives with using of the dynamic library
*       7-zip32.dll (Japanese http://www.csdinc.co.jp/archiver/lib/
*       English http://www.csdinc.co.jp/archiver/lib/main-e.html)
*
*/

PROCEDURE Main
LOCAL oReg

// Generate the full name of the installed 7-Zip through a registry entry

OPEN REGISTRY oReg KEY HKEY_CURRENT_USER Section 'Software\7-Zip'

   GET VALUE cPath7z NAME 'Path' OF oReg

CLOSE REGISTRY oReg

IF !Empty( cPath7z )
   cPath7z := hb_DirSepAdd( cPath7z )
   cPath7z += '7z.exe'
ELSE
   // Error - there is no corresponding registry entry
   MsgAlert( 'The 7-Zip archiver is not found.', 'Alert' )
ENDIF

IF ( !File( cPath7z  ) .AND. ;
         !File( ALONE_7Z )   ;
         )

// If there is no installed 7-Zip and console version of the archiver,
// then further actions are prohibited.
// Although there is a nuance: using one 7-zip32.dll without 7-Zip
// allows viewing the archive, but does not allow creating or
// extract files from it.

   MsgStop( 'The required programs are not found.', 'Error' )
   QUIT

ENDIF

SET FONT TO 'Tahoma', 9

DEFINE WINDOW wMain                                         ;
      At 0, 0                                               ;
      Width 553 + iif( IsSeven(), GetBorderWidth() -2, 0 )  ;
      Height 432 + iif( IsSeven(), GetBorderHeight() -2, 0 );
      Title 'Demo 7-Zip interaction'                        ;
      Icon 'main.ico'                                       ;
      Main                                                  ;
      NoMaximize

   DEFINE TAB tbMain ;
      at 5, 5   ;
      Width 535 ;
      Height 370

   DEFINE PAGE 'Archive'

// Display the contents of the selected archive.

   @ 30, 5 Grid grdContent            ;
      Width 520                  ;
      Height 285                 ;
      Headers { 'Name' }         ;
      Widths { 400 }             ;
      Multiselect

// Operations for opening the archive, extracting and creating

   @ 330, 15 ButtonEx btnCreate  ;
      Caption 'Create'    ;
      Action RunTest( 1 ) ;
      Tooltip 'Create archive'

   @ 330, 220 ButtonEx btnView    ;
      Caption 'View'      ;
      Action RunTest( 2 ) ;
      Tooltip 'View 7z/zip archive'

   @ 330, 415 ButtonEx btnExtract ;
      Caption 'Extract'   ;
      Action RunTest( 3 ) ;
      Tooltip 'Extract file(s) from archive'

   END PAGE

// Some processing settings

   DEFINE PAGE 'Options'

// Select a demonstration option

   @ 30, 5 Frame frmSelectTest   ;
      Caption 'Select test' ;
      Width 520             ;
      Height 65             ;
      Bold                  ;
      FontColor BLUE

   @ 55, 15 RadioGroup rdgSelectTest                      ;
      Options { '7-zip32.dll', '7-Zip', '7za.exe' } ;
      Width 100                                     ;
      Spacing 20                                    ;
      ON Change wMain.btnExtract.Enabled := .F.     ;
      Horizontal

// Common parameters

   @ 110, 5 Frame frmCommon  ;
      Caption 'Common' ;
      Width 520        ;
      Height 65        ;
      Bold             ;
      FontColor BLUE

// Display processing

   @ 135, 15 CheckBox cbxHide           ;
      Caption 'Hide progressbar' ;
      Width 124                  ;
      Value .T.

// extraction options

   @ 185, 5 Frame frmExtract  ;
      Caption 'Extract' ;
      Width 520         ;
      Height 65         ;
      Bold              ;
      FontColor BLUE

// Maintain directory structure when retrieving

   @ 210, 15 CheckBox cbxExtract                     ;
      Caption 'Extract files with full paths' ;
      Width 176                               ;
      Value .T.

// Answer Yes to all questions during processing

   @ 210, 200 CheckBox cbxYesAll                    ;
      Caption 'Assume (Yes) on all queries' ;
      Width 190

// Useful links

   @ 260, 5 Frame frmLinks  ;
      Caption 'Links' ;
      Width 520       ;
      Height 100      ;
      Bold            ;
      FontColor BLUE
   @ 285, 15 LABEL lbl7z   ;
      Value '7-Zip' ;
      Width 120     ;
      Height 15
   @ 285, 140 Hyperlink hl7z                 ;
      Value 'http://www.7-zip.org'   ;
      Address 'http://www.7-zip.org' ;
      HandCursor
   @ 305, 15 LABEL lblDLL_JA                ;
      Value '7-Zip32.dll (Japanese)' ;
      Width 120                      ;
      Height 15
   @ 305, 140 Hyperlink hlDLL_JA                              ;
      Value 'http://www.csdinc.co.jp/archiver/lib/'   ;
      Address 'http://www.csdinc.co.jp/archiver/lib/' ;
      Width 270 HandCursor
   @ 325, 15 LABEL lblDLL_EN               ;
      Value '7-Zip32.dll (English)' ;
      Width 120                     ;
      Height 15
   @ 325, 140 Hyperlink hlDLL_EN                                         ;
      Value 'http://www.csdinc.co.jp/archiver/lib/main-e.html'   ;
      Address 'http://www.csdinc.co.jp/archiver/lib/main-e.html' ;
      Width 270 HandCursor

   END PAGE

   END TAB

   DEFINE STATUSBAR
      StatusItem ''
      StatusItem '' Width 120
      StatusItem '' Width 40
      StatusItem '' Width 130
   END STATUSBAR

END WINDOW

// Set access to test cases

IF !File( cPath7z )

   // Only launching the console version of the archiver is available

   wMain.rdgSelectTest.Enabled( 1 ) := .F.
   wMain.rdgSelectTest.Enabled( 2 ) := .F.
   wMain.rdgSelectTest.Value := 3

   IF !File( ALONE_7Z )

      // No files needed. Forbid everything

      wMain.rdgSelectTest.Enabled := .F.
      wMain.rdgSelectTest.Value   := 0

   ENDIF

ELSE

// If there are no the dynamic library and console version of the archiver
// then you can see only the action of the installed version of 7-Zip

   wMain.rdgSelectTest.Value := 2

   IF !File( '7-zip32.dll' )
      wMain.rdgSelectTest.Enabled( 1 ) := .F.
   ELSE
      wMain.rdgSelectTest.Value := 1
   ENDIF

   IF !File( ALONE_7Z )
      wMain.rdgSelectTest.Enabled( 3 ) := .F.
   ENDIF

ENDIF

wMain.btnExtract.Enabled := .F.

CENTER WINDOW wMain
ACTIVATE WINDOW wMain

RETURN

***** End of Main ******


/******
*
*       RunTest( nChoice )
*
*       Start processing. What will be processed
*       set by rdgSelectTest selector
*/

STATIC PROCEDURE RunTest( nChoice )

   LOCAL nSelected := wMain.rdgSelectTest.Value

   DO CASE
   CASE ( nChoice == 1 ) // Create Archive

      IF ( nSelected == 1 )
         // Process 7-zip32.dll
         CreateArc()
      ELSE
         // Run 7z.exe or 7za.exe
         CreateArcExternal()
      ENDIF

   CASE ( nChoice == 2 ) // View Content

      IF ( nSelected == 1 )
         ViewArc()
      ELSE
         ViewArcExternal()
      ENDIF

   CASE ( nChoice == 3 ) // Extract Files

      IF ( nSelected == 1 )
         ExtractArc()
      ELSE
         ExtractArcExternal()
      ENDIF

   ENDCASE

RETURN

***** End of RunTest ******


/******
*
*       ShowStatus( cFile, cCount, cType, cVersion )
*
*       Display status bar items
*
*/

STATIC PROCEDURE ShowStatus( cFile, cCount, cType, cVersion )

   wMain.StatusBar.Item (1) := cFile    // Processed file
   wMain.StatusBar.Item (2) := cCount   // Files in the archive
   wMain.StatusBar.Item (3) := cType    // Archive type
   wMain.StatusBar.Item (4) := cVersion // Procedure Information

RETURN

***** End of ShowStatus ******


// ------------------------------------------------ --------------
// Procedure block for 7-zip32.dll
// ------------------------------------------------ --------------

/******
*
*       Version7zip() --> cVersion
*
*       Version of the archiver 7-zip and 7-zip32.dll
*
*/

STATIC FUNCTION Version7zip

   LOCAL nVersion := SevenZipGetVersion(), ;    // 7-zip
      nSubversion := SevenZipGetSubVersion(), ; // 7-zip32.dll
      cVersion    := 'Version '

   cVersion += ( Str( ( nVersion / 100 ), 5, 2 ) + '.' + StrZero( ( nSubversion / 100 ), 5, 2 ) )

RETURN cVersion

***** End of Version7zip ******


/******
*
*       CreateArc()
*
*       Create archive
*
*/

STATIC PROCEDURE CreateArc

   LOCAL aSource := GetFile( { { 'All files', '*.*' } }, ;
      'Select file(s)', ;
      GetCurrentFolder(), .T., .T.  ;
      ), ;
      cArcFile, ;
      cType     := '', ;
      cCommand  := 'A ', ;
      nDLLHandle

   IF !Empty( aSource )

      cArcFile := PutFile ( { { '7-zip', '*.7z' }, { 'Zip', '*.zip' } }, ;
         'Create archive', ;
         GetCurrentFolder(), ;
         .T.                                          ;
         )

      IF !Empty( cArcFile )

         // Define the type of archive. The default is 7z, so
         // remember only in case of change in the dialog box.

         IF ( Upper( Right( cArcFile, 3 ) ) == 'ZIP' )
            cType := 'zip'
         ENDIF

         // Build a command line to pass to the DLL

         IF wMain.cbxHide.Value
            cCommand += '-hide '  // Do not display the process
         ENDIF

         IF !Empty( cType )
            cCommand += '-tzip '  // In ZIP format
         ENDIF

         cCommand += ( cArcFile + ' ' )

         // Specify files to process

         AEval( aSource, {| elem | cCommand += ( '"' + elem + '" ' ) } )

         cCommand := RTrim( cCommand )

         IF !( ( nDLLHandle := LoadLibrary( '7-zip32.dll' ) ) > 0 )
            MsgStop( "Can't load 7-zip32.dll.", 'Error' )
         ELSE
            DllCall( nDLLHandle, DC_CALL_STD, 'SevenZip', _HMG_MainHandle, cCommand )
            FreeLibrary( nDLLHandle )

            // Fill In The Status Bar

            ShowStatus( cArcFile, '', iif( Empty( cType ), '7z', 'zip' ), Version7zip() )

         ENDIF

      ENDIF

   ENDIF

RETURN

***** End of CreateArc ******


/******
*
*       ViewArc()
*
*       Open archive and fill in a table of contents
*
*/

STATIC PROCEDURE ViewArc

   LOCAL cFile      := GetFile( { { '7-zip', '*.7z' }, { 'Zip', '*.zip' } }, ;
      'Select archive', ;
      GetCurrentFolder(), ;
      .F., .T.                                     ;
      ), ;
      nDLLHandle, ;
      nArcHandle, ;
      nResult, ;
      cValue, ;
      nCount    := 0, ;
      cType     := '', ;
      oInfo, ;
      pInfo, ;
      aFiles    := {}

   IF Empty( cFile )
      RETURN
   ENDIF

   IF !( ( nDLLHandle := LoadLibrary( '7-zip32.dll' ) ) > 0 )
      MsgStop( "Can't load 7-zip32.dll.", 'Error' )
      RETURN
   ENDIF

   nArcHandle := DllCall( nDLLHandle, DC_CALL_STD, 'SevenZipOpenArchive', _HMG_MainHandle, cFile, 0 )   // Открыть архив

   IF Empty( nArcHandle )
      MsgStop( cFile + ' not opened.', 'Error' )
      RETURN
   ENDIF

   nCount  := DllCall( nDLLHandle, DC_CALL_STD, 'SevenZipGetFileCount', cFile )  // Количество элементов в архиве
   nResult := DllCall( nDLLHandle, DC_CALL_STD, 'SevenZipGetArchiveType', cFile )  // Тип архива

   DO CASE
   CASE ( nResult == 1 )
      cType := 'ZIP'

   CASE ( nResult == 2 )
      cType := '7Z'

   CASE ( nResult == -1 )
      // Processing error
      cType := 'Error'

   CASE ( nResult == 0 )
      // Unsupported type. While trying to open something
      // except for 7z and Zip, the SevenZipOpenArchive () function will be
      // return an error.
      cType := '???'

   ENDCASE

   // Initialization of the structure necessary for processing archive elements and
   // pointer (for passing to the DLL)

   oInfo := ( STRUCT INDIVIDUALINFO )
   pInfo := oInfo : GetPointer()

   // Looking for the 1st file. If the search result does not matter, pass pInfo
   // can be omitted.

   DllCall( nDLLHandle, DC_CALL_STD, 'SevenZipFindFirst', nArcHandle, '*', pInfo )

   // Reset The Pointer

   oInfo := oInfo : Pointer( pInfo )

   cValue := Space( FNAME_MAX32 )
   DllCall( nDLLHandle, DC_CALL_STD, 'SevenZipGetFileName', nArcHandle, @cValue, FNAME_MAX32 )

   IF !Empty( cValue )

      // Fill out the form table. First, we enter the values into an array,
      // sort and pass the Grid

      AAdd( aFiles, { cValue } )

      DO WHILE ( ( nResult := DllCall( nDLLHandle, DC_CALL_STD, 'SevenZipFindNext', nArcHandle, pInfo ) ) == 0 )

         cValue := Space( FNAME_MAX32 )
         DllCall( nDLLHandle, DC_CALL_STD, 'SevenZipGetFileName', nArcHandle, @cValue, FNAME_MAX32 )

         AAdd( aFiles, { cValue } )

      ENDDO

      wMain.grdContent.DeleteAllItems

      ASort( aFiles,,, {| x, y | x[ 1 ] < y[ 1 ] } )

      wMain.grdContent.DisableUpdate
      AEval( aFiles, {| elem | wMain.grdContent.AddItem( elem ) } )
      wMain.grdContent.EnableUpdate
      wMain.grdContent.Value := { 1 }

   ENDIF

   // Close the archive file, unload the library

   DllCall( nDLLHandle, DC_CALL_STD, 'SevenZipCloseArchive', nArcHandle )
   FreeLibrary( nDLLHandle )

   // Fill In The Status Bar

   ShowStatus( cFile, ( 'Count files: ' + LTrim( Str( nCount ) ) ), cType, Version7zip() )

   IF ( wMain.grdContent.ItemCount > 0 )
      wMain.btnExtract.Enabled := .T.
   ENDIF

RETURN

***** End of ViewArc ******


/******
*
*       ExtractArc()
*
*       Extract files from archive
*
*/

STATIC PROCEDURE ExtractArc

   LOCAL aPos := wMain.grdContent.Value, ;
      cDir, ;
      cCommand, ;
      nPos, ;
      cFile, ;
      nDLLHandle

   IF Empty( aPos )
      MsgStop( 'Select item(s), please!', 'Error' )
      RETURN
   ENDIF

   IF !Empty( cDir := GetFolder( 'Extract file(s) to' ) )

      // Retrieve while maintaining directory structure or not

      cCommand := ( iif( wMain.cbxExtract.Value, 'x', 'e' ) + ' ' )

      IF wMain.cbxHide.Value

         // Do not display the process. But if you need to rewrite
         // existing files, the corresponding request anyway
         // will be output.

         cCommand += '-hide '

      ENDIF

      // Overwrite existing files without warning

      IF wMain.cbxYesAll.Value
         cCommand += '-y '
      ENDIF

      cCommand += ( '-o' + cDir + ' ' ) // Where to extract

      // Do not forget to add the name of the archive containing the extracted files

      // cCommand += ( '"' + AllTrim( wMain.Statusbar.Item( 1 ) ) + '" ' )
      cCommand += ( AllTrim( wMain.Statusbar.Item( 1 ) ) + ' ' )

      // Add the extracted files. To simplify processing:
      // if the number of marked items is equal to the total
      // quantity, it makes no sense to do an exhaustive search.

      IF ( Len( aPos ) == wMain.grdContent.ItemCount )
         cCommand += '*.*'
      ELSE

         FOR EACH nPos In aPos

            // Items containing only the directory name, skip

            cFile := AllTrim( wMain.grdContent.Item( nPos )[ 1 ] )

            IF !( Right( cFile, 1 ) == '\' )
               // cCommand += ( '"' + cFile + '" ' )
               cCommand += ( cFile + ' ' )
            ENDIF

         NEXT

         cCommand := RTrim( cCommand )

      ENDIF

      IF !( ( nDLLHandle := LoadLibrary( '7-zip32.dll' ) ) > 0 )
         MsgStop( "Can't load 7-zip32.dll.", 'Error' )
      ELSE
         DllCall( nDLLHandle, DC_CALL_STD, 'SevenZip', _HMG_MainHandle, cCommand )
         FreeLibrary( nDLLHandle )
         MsgInfo( "Extraction is successfully.", 'Result' )
      ENDIF

   ENDIF

RETURN

***** End of ExtractArc ******


// Procedures in 7-zip32.dll

// Version and subversion of the library

DECLARE DLL_TYPE_WORD SevenZipGetVersion() in 7-zip32.dll
DECLARE DLL_TYPE_WORD SevenZipGetSubVersion() in 7-zip32.dll


// --------------------------------------------------------------
// Procedure block for 7-zip 7za.exe
// --------------------------------------------------------------

/******
*
*       CreateArcExternal()
*
*       Create archive
*
*/

STATIC PROCEDURE CreateArcExternal

   LOCAL aSource := GetFile( { { 'All files', '*.*' } }, ;
      'Select file(s)', ;
      GetCurrentFolder(), .T., .T.  ;
      ), ;
      cArcFile, ;
      nPos, ;
      cExt, ;
      cType     := '', ;
      cCommand  := ' A '

   IF !Empty( aSource )

      // Addressing directly to 7-Zip itself allows you to create
      // more types of archives

      cArcFile := PutFile ( { { '7-zip', '*.7z'    }, ;
         { 'Zip', '*.zip'   }, ;
         { 'GZip', '*.gzip'  }, ;
         { 'BZip2', '*.bzip2' }, ;
         { 'Tar', '*.tar'   }  ;
         }, ;
         'Create archive', ;
         GetCurrentFolder(), ;
         .T.                       ;
         )

      IF !Empty( cArcFile )

         // Define the type of archive. The default is 7z, so
         // remember only in case of change in the dialog box.

         nPos := RAt( '.', cArcFile )
         cExt := Upper( Right( cArcFile, ( Len( cArcFile ) - nPos ) ) )

         IF !( cExt == '7Z' )
            cType := cExt
         ENDIF

         // Build the command line

         IF !Empty( cType )
            cCommand += ( '-t' + cType + ' ' )
         ENDIF

         cCommand += ( cArcFile + ' ' )

         // Specify files to process

         AEval( aSource, {| elem | cCommand += ( '"' + elem + '" ' ) } )

         // Run either the installed archiver or console
         // version located in the folder with the demo program

         IF ( wMain.rdgSelectTest.Value == 2 )
            cCommand := ( cPath7z + cCommand )
         ELSE
            cCommand := ( ALONE_7Z + cCommand )
         ENDIF

         cCommand := RTrim( cCommand )

         // Run in standby mode for the end of processing. If
         // while the archiver window itself is hidden (for aesthetics, because the window
         // console), to display that the work is being performed (if
         // the archive is large), you can display some kind of information window,
         // for example with a timer.

         // There is another option: for 7-Zip, run not% ProgramFiles% \ 7-Zip \ 7z.exe,
         // and% ProgramFiles% \ 7-Zip \ 7zG.exe is the graphical interface of the archiver.
         // Get the weird little progress bar on the screen.

         IF wMain.cbxHide.Value
            Execute File ( cCommand ) WAIT Hide
         ELSE
            Execute File ( cCommand ) Wait
         ENDIF

         // Fill In The Status Bar

         ShowStatus( cArcFile, '', iif( Empty( cType ), '7Z', cType ), ;
            iif( ( wMain.rdgSelectTest.Value == 2 ), '7-Zip', '7za' ) )

      ENDIF

   ENDIF

RETURN

***** End of CreateArcExternal ******


/******
*
*       ViewArcExternal()
*
*       Open archive and fill in a table of contents
*
*/

STATIC PROCEDURE ViewArcExternal
   // aFiles - a set of supported archive types. The base accept set for
   // console version (7za.exe), because its capabilities are more modest.
   LOCAL aFilters := { { '7-zip', '*.7z'   }, ;
      { 'Zip', '*.zip'  }, ;
      { 'Cab', '*.cab'  }, ;
      { 'GZip', '*.gzip' }, ;
      { 'Tar', '*.tar'  } ;
      }, ;
      cFile, ;
      aFiles    := {}, ;
      cCommand, ;
      cTmpFile := '_Arc_.lst',; // Or GetTempFolder () + '\ _Arc_.lst'
      oFile, ;
      cString

   // Add archive types that the full version can work with (not all,
   // specified in the documentation, of course)

   IF ( wMain.rdgSelectTest.Value == 2 )
      AAdd( aFilters, { 'Rar', '*.rar' } )
      AAdd( aFilters, { 'Arj', '*.arj' } )
      AAdd( aFilters, { 'Chm', '*.chm' } )
      AAdd( aFilters, { 'Lzh', '*.lzh' } )
   ENDIF

   IF Empty( cFile := GetFile( aFilters, 'Select archive', GetCurrentFolder(), .F., .T. ) )
      RETURN
   ENDIF

   // The contents of the archive are displayed in a temporary file and then read for display in
   // program.

   // You can, of course, use cmd.exe instead of GetEnv ('COMSPEC'), but
   // the name of the shell may be different in older versions of Windows

   cCommand := GetEnv( 'COMSPEC' ) + ' /C '

   IF ( wMain.rdgSelectTest.Value == 2 )
      // Quotation marks do not hurt, because Program Files has a space in the name.
      // Here you need to use exactly% ProgramFiles% \ 7-Zip \ 7z.exe, because
      // graphical version of 7zG.exe does not support redirecting output to a file
      cCommand := ( cCommand + '"' + cPath7z + '"' )
   ELSE
      cCommand := ( cCommand + ALONE_7Z )
   ENDIF

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

   // Temporary content file is better, of course, to create in
   // system folder of temporary files (GetTempFolder () + '\' + cTmpFile)

   cCommand += ( ' L -slt ' + cFile + ' > ' + cTmpFile )

   Execute File ( cCommand ) WAIT Hide

   // A more refined solution would be to redirect the output of the console program
   // WinAPI function (use CreatePipe and work with it as usual
   // file), and not create a temporary file, but I'm not that subtle expert.

   IF File( cTmpFile )

      // Temporary file may not be created, for example, due to the errors
      // in the command line. Additionally, it would not hurt to check its size.
      // If zero, then there is nothing in it.

      // Fill The Array

      oFile := TFileRead():New( cTmpFile )
      oFile:Open()

      IF !oFile:Error()

         DO WHILE oFile:MoreToRead()

            IF !Empty( cString := oFile:ReadLine() )

               // Several simplified processing. Just checking does not start
               // whether the line with "Path =" and, if so, then this is the file name. At
               // necessary, can be made more complicated. For example, ignore
               // directory names (line "Attributes = D ...." for .7z files)

               IF ( Left( cString, 7 ) == 'Path = ' )
                  cString := AllTrim( SubStr( cString, 8 ) )
                  IF !( cString == cFile )
                     AAdd( aFiles, { cString } )
                  ENDIF
               ENDIF

            ENDIF

         ENDDO

         oFile:Close()

         IF !Empty( aFiles )

            wMain.grdContent.DeleteAllItems

            ASort( aFiles,,, {| x, y | x[ 1 ] < y[ 1 ] } )

            wMain.grdContent.DisableUpdate
            AEval( aFiles, {| elem | wMain.grdContent.AddItem( elem ) } )
            wMain.grdContent.EnableUpdate
            wMain.grdContent.Value := { 1 }

            // Fill in the status bar (it will store the name of the read
            // archive needed to extract files)

            ShowStatus( cFile, ( 'Count files: ' + LTrim( Str( Len( aFiles ) ) ) ), ;
               Upper( Right( cFile, ( Len( cFile ) - RAt( '.', cFile ) ) ) ), ;
               iif( ( wMain.rdgSelectTest.Value == 2 ), '7-Zip', '7za' ) )

         ENDIF

      ENDIF

      IF ( wMain.grdContent.ItemCount > 0 )
         wMain.btnExtract.Enabled := .T.
      ENDIF

   ENDIF

   // The temporary file also played a role. deleted. Team not
   // causes an error even if the deleted file does not exist.

   FErase ( cTmpFile )

RETURN

***** End of ViewArcExternal ******


/******
*
*       ExtractArcExternal()
*
*       Extract files from archive
*
*/

STATIC PROCEDURE ExtractArcExternal

   LOCAL aPos := wMain.grdContent.Value, ;
      cDir, ;
      cCommand, ;
      nPos, ;
      cFile

   IF Empty( aPos )
      MsgStop( 'Select item(s), please!', 'Error' )
      RETURN
   ENDIF

   IF !Empty( cDir := GetFolder( 'Extract file(s) to' ) )

      // Retrieve while maintaining directory structure or not

      cCommand := ( iif( wMain.cbxExtract.Value, 'X', 'E' ) + ' ' )

      // Overwrite existing files without warning

      IF wMain.cbxYesAll.Value
         cCommand += '-y '
      ENDIF

      cCommand += ( '-o' + cDir + ' ' ) // Where to extract

      cCommand += ( AllTrim( wMain.Statusbar.Item( 1 ) ) + ' ' )

      IF ( Len( aPos ) == wMain.grdContent.ItemCount )

         cCommand += '*.*'

      ELSE

         FOR EACH nPos In aPos

            // Items which containing only the directory name, skip

            cFile := AllTrim( wMain.grdContent.Item( nPos )[ 1 ] )

            IF !( Right( cFile, 1 ) == '\' )
               cCommand += ( cFile + ' ' )
            ENDIF

         NEXT

         cCommand := RTrim( cCommand )

      ENDIF

      IF ( wMain.rdgSelectTest.Value == 2 )

         // If instead of 7z.exe use 7zG.exe, it will be displayed
         // operation indicator

         cCommand := ( cPath7z + ' ' + cCommand )
      ELSE
         cCommand := ( ALONE_7Z + ' ' + cCommand )
      ENDIF

      // Do it.

      IF wMain.cbxHide.Value .AND. !wMain.cbxYesAll.Value
         Execute File ( cCommand ) WAIT Hide
      ELSE
         Execute File ( cCommand ) Wait
      ENDIF

      MsgInfo( 'Extraction is successfully.', 'Result' )

   ENDIF

RETURN

***** End of ExtractArcExternal ******
