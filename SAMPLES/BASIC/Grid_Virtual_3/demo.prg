/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Demo was contributed to HMG forum by Edward 29/Nov/2021
 *
 * Adapted for MiniGUI Extended Edition by Grigory Filatov
 */

#pragma -w3
#pragma -es2

#include "hmg.ch"
#include "Fileio.CH"
#include "hbthread.ch"

#xtranslate Sleep( <t> ) => wapi_Sleep( <t> )

REQUEST HB_LANG_DEWIN
REQUEST HB_CODEPAGE_DEWIN

SET PROCEDURE TO vfFileRead

STATIC lCheck_1, lCheck_2, lCheck_3

/*
 *  FUNCTION Main
 *
 *  This is the main function of the application. It defines the main window,
 *  its controls (menu, labels, checkboxes, statusbar, progressbar), and
 *  sets up the initial state of the application. It also handles the activation
 *  of the main window, starting the GUI event loop.
 */
FUNCTION Main

   LOCAL cFileToOpen := 'TestBIG.dat'
   LOCAL cDelimiter := Chr ( 9 )

   Set( _SET_CODEPAGE, "DEWIN" )
   hb_langSelect( 'DEWIN' )

   DEFINE WINDOW Form_1 ;
         AT 0, 0 ;
         WIDTH 1100 ;
         HEIGHT 660 ;
         TITLE 'Large text file viewer.' ;
         MAIN ;
         ON INIT Prepare( cFileToOpen, cDelimiter )

      DEFINE MAIN MENU
         DEFINE POPUP 'File'
            MENUITEM 'Open file (without counting the number of lines) - incremental reading file.' ACTION OpenFile ( cFileToOpen, cDelimiter, 0 )
            MENUITEM 'Open file (count the number of lines before browsing) - static file loading.' ACTION OpenFile ( cFileToOpen, cDelimiter, 1 )
            MENUITEM 'Open file (count the number of lines in another thread) - dynamic file loading.' ACTION OpenFile ( cFileToOpen, cDelimiter, 2 )
         END POPUP
         DEFINE POPUP 'Test'
            MENUITEM 'Test hb_F* functions' ACTION Test ( 1, cFileToOpen )
            MENUITEM 'Test vfFileRead Class' ACTION Test ( 2, cFileToOpen )
         END POPUP
      END MENU

      @ 10, 880 LABEL Label_1 WIDTH 230 VALUE ""
      @ 50, 880 LABEL Label_2 WIDTH 230 VALUE ""
      @ 90, 880 LABEL Label_3 WIDTH 230 VALUE ""

      @ 150, 880 CHECKBOX Check_1 CAPTION 'Get ListViewGetCountPerPage' WIDTH 230 VALUE .T. ON CHANGE lCheck_1 := Form_1.Check_1.VALUE
      @ 180, 880 CHECKBOX Check_2 CAPTION 'Get Form_1.Grid_1.Height' WIDTH 230 VALUE .T. ON CHANGE lCheck_2 := Form_1.Check_2.VALUE
      @ 210, 880 CHECKBOX Check_3 CAPTION 'Get Form_1.Grid_1.ItemCount' WIDTH 230 VALUE .T. ON CHANGE lCheck_3 := Form_1.Check_3.VALUE

      Form_1.Check_1.Visible := .F.
      Form_1.Check_2.Visible := .F.
      Form_1.Check_3.Visible := .F.

      lCheck_1 := Form_1.Check_1.VALUE
      lCheck_2 := Form_1.Check_2.VALUE
      lCheck_3 := Form_1.Check_3.VALUE

      DEFINE STATUSBAR
         STATUSITEM "" WIDTH Form_1.Width / 2
         STATUSITEM "" WIDTH Form_1.Width / 2
      END STATUSBAR

      ShowProgressBar( 1, , , ( Form_1.Width / 2 ) + 5, Form_1.Width / 2 - 30 ) // init progress bar
      ShowProgressBar( 0 ) // hide progress bar

   END WINDOW

   CENTER WINDOW Form_1
   ACTIVATE WINDOW Form_1

RETURN NIL

/*
 *  FUNCTION QueryFile( oFile, cDelimiter )
 *
 *  This function is called by the grid control to retrieve data for each cell.
 *  It reads a line from the file, splits it into fields based on the delimiter,
 *  and returns the appropriate field for the requested row and column. It also
 *  implements incremental file reading to handle large files efficiently.
 *
 *  Parameters:
 *      oFile       - vfFileRead object representing the file to read from.
 *      cDelimiter  - Character used to separate fields in the file.
 *
 *  Return:
 *      NIL
 *
 *  Note:
 *      This function uses static variables to cache the fields of the current
 *      line, improving performance by avoiding rereading the same line multiple
 *      times when querying different columns.
 */
FUNCTION QueryFile( oFile, cDelimiter )

   LOCAL nRecord := This.QueryRowIndex
   LOCAL nCol := This.QueryColIndex
   LOCAL nListRows := LISTVIEWGETCOUNTPERPAGE ( Form_1.Grid_1.Handle ) // Number of visible lines in the grid
   LOCAL nItemCount := Form_1.Grid_1.ItemCount
   LOCAL nIncrementalRead := 1000
   LOCAL aMemAlloc := {}

   STATIC aFields := {}

   IF oFile == NIL
      RETURN NIL
   ENDIF

   IF oFile:CurrentLine() <> nRecord // Prevents the current row from being reread when query to display the next column (speeding up)
      oFile:GoTo ( nRecord )
      aFields := hb_ATokens ( oFile:ReadLine(), cDelimiter )
      aMemAlloc := GetProcessMemoryInfo()
   ENDIF

   This.QueryData := IF ( Len ( aFields ) < nCol, "", aFields[ nCol ] )

   IF nRecord > nItemCount - nListRows .AND. oFile:nLastLine = NIL // incremental reading file
      oFile:Skip ( nIncrementalRead )
      IF oFile:CurrentLine() > nItemCount
         Form_1.Grid_1.ItemCount := oFile:CurrentLine() // Changing the ItemCount causes the Grid view to scroll to the position of the selected row
         Form_1.Grid_1.VALUE := nRecord // Re-scroll to the position nRecord of displayed line before the ItemCount was changed.
      ENDIF
      oFile:Skip ( 0 - nIncrementalRead )
   ENDIF

   IF Len ( aMemAlloc ) == 9 .AND. aMemAlloc[ 3 ] / 1024 ^ 2 > 50
      EmptyWorkingSet() // Flush the current working set size
      // HMG_GarbageCall()  <- CAUTION!!!! DO NOT call HMG_GarbageCall () or Release Memory, this may cause hangon the application when using the vertical slider
   ENDIF

RETURN NIL

/*
 *  FUNCTION MemoryLeakQuery()
 *
 *  This function is designed to test for potential memory leaks by repeatedly
 *  calling functions that might allocate memory without releasing it. It's
 *  triggered by checkboxes on the form, allowing the user to enable or disable
 *  specific tests.
 *
 *  Parameters:
 *      None
 *
 *  Return:
 *      NIL
 */
FUNCTION MemoryLeakQuery()

   IF lCheck_1
      ListViewGetCountPerPage ( Form_1.Grid_1.Handle ) // Number of visible lines in the grid
   ENDIF
   IF lCheck_2
      Form_1.Grid_1.HEIGHT
   ENDIF
   IF lCheck_3
      Form_1.Grid_1.ItemCount
   ENDIF

   This.QueryData := Str ( This.QueryRowIndex )

RETURN NIL

/*
 *  FUNCTION OpenFile( cFileToOpen, cDelimiter, nMode )
 *
 *  This function opens a file and displays its contents in a grid control.
 *  It supports different modes of operation:
 *      0 - Incremental reading (without counting lines beforehand).
 *      1 - Static loading (counting lines before displaying).
 *      2 - Dynamic loading (counting lines in a separate thread).
 *
 *  Parameters:
 *      cFileToOpen - The name of the file to open.
 *      cDelimiter  - The delimiter used to separate fields in the file.
 *      nMode       - The mode of operation (0, 1, or 2).
 *
 *  Return:
 *      oFile - vfFileRead object representing the opened file, or NIL if an error occurred.
 */
FUNCTION OpenFile ( cFileToOpen, cDelimiter, nMode )

   LOCAL aColumns, aHeaders := {}, aWidths := {}, i, oFile
   LOCAL pMutexCount, nListRows
      DEFAULT nMode := 0

   IF File ( cFileToOpen )

      oFile := vfFileRead():New( cFileToOpen, , , )
      oFile:Open()

      IF oFile:Error()
         MsgStop ( "Error " + Str( oFile:ErrorNo() ), "Error" )
         RETURN NIL
      ENDIF

      IF IsControlDefined ( Grid_1, Form_1 )
         Form_1.Grid_1.RELEASE
      ENDIF

      // Preparing columns (based on the first row from the file)
      aColumns := hb_ATokens ( oFile:ReadLine(), cDelimiter )

      FOR i := 1 TO Len( aColumns )
         AAdd( aHeaders, 'Col #' + AllTrim( Str( i ) ) )
         AAdd( aWidths, Max( 20, Len( aColumns[ i ] ) * 9 ) )
      NEXT i

      IF nMode == 1 // Calculate the number of rows before displaying the Grid
         WAIT WINDOW "Counting the number of records ..." NOWAIT
         ShowProgressBar( 2 ) // show progress bar
         Form_1.StatusBar.Item( 1 ) := "Counting the number of records ... This may take some time .... "

         // code block executed while the CountLines() method is running
         oFile:exGauge := {| nPos, nLastPos, nLines, cState | ( Form_1.StatusBar.Item( 1 ) := "Counting the number of records: " + AllTrim ( Str ( nLines ) ) + "... This may take some time ... " + cState, Form_1.PBar.VALUE := nPos / nLastPos * 100, doEvents() ) }
         oFile:CountLines()
         Form_1.StatusBar.Item( 1 ) := ""
         ShowProgressBar( 0 ) // hide progress bar

         WAIT CLEAR
      ENDIF

      @ 10, 10 GRID Grid_1 ;
         OF Form_1 ;
         WIDTH 850 ;
         HEIGHT 530 ;
         VALUE 1 ;
         HEADERS aHeaders ;
         WIDTHS aWidths ;
         VIRTUAL ;
         ITEMCOUNT 0 ;
         ON QUERYDATA If ( nMode < 3, QueryFile( oFile, cDelimiter ), MemoryLeakQuery() )

      nListRows := LISTVIEWGETCOUNTPERPAGE ( Form_1.Grid_1.Handle ) // Number of visible lines in the grid
      Form_1.Grid_1.PaintDoubleBuffer := .T.

      IF nMode == 2 .AND. hb_mtvm() // other thread
         pMutexCount := hb_mutexCreate() // Mutex used to transfer values between threads
         hb_threadStart( HB_THREAD_INHERIT_MEMVARS /* Required to access Form_1 on another thread*/, @MT_Count(), cFileToOpen, oFile, pMutexCount )
      ENDIF

      IF oFile:nLastLine = NIL // Incremental reading of the file. If we do not know the number of the last row, we try to read the next ones (twice as many as the number of rows visible in the grid)
         oFile:Goto ( nListRows * 2 )
         IF .NOT. oFile:nLastLine = NIL // End of file has been reached
            Form_1.Grid_1.ItemCount := oFile:nLastLine
         ELSE
            Form_1.Grid_1.ItemCount := nListRows * 2
         ENDIF
      ELSE
         Form_1.Grid_1.ItemCount := oFile:nLastLine
      ENDIF

      Form_1.Check_1.Visible := .F.
      Form_1.Check_2.Visible := .F.
      Form_1.Check_3.Visible := .F.

      Form_1.Grid_1.VALUE := 1
      Form_1.Grid_1.Setfocus

   ENDIF

RETURN oFile

/*
 *  FUNCTION MT_Count( cFileToOpen, oFileGrid, pMutexCount )
 *
 *  This function counts the number of lines in a file in a separate thread.
 *  It uses a mutex to communicate the progress of the counting process to the
 *  main thread, allowing the UI to be updated without blocking.
 *
 *  Parameters:
 *      cFileToOpen - The name of the file to open.
 *      oFileGrid   - The vfFileRead object associated with the grid control.
 *      pMutexCount - A mutex used to synchronize access to shared data between threads.
 *
 *  Return:
 *      NIL
 */
FUNCTION MT_Count( cFileToOpen, oFileGrid, pMutexCount )

   // Calculating the number of lines in a separate thread.

   LOCAL xResp, pThID := hb_threadSelf(), nThread
   LOCAL oFileMT, nCurrDispRec

   // To avoid skipping lines in the grid while counting lines, let's open the file in another object.
   IF File ( cFileToOpen )
      oFileMT := vfFileRead():New( cFileToOpen )
      oFileMT:Open()

      IF oFileMT:Error()
         MsgStop ( "Error " + Str( oFileMT:ErrorNo() ), "Error" )
         hb_threadDetach( pThID ) // close thread handle
         hb_threadQuitRequest( pThID ) // terminate thread
         oFileMT:Close()
         RETURN NIL
      ENDIF

      ShowProgressBar( 2 ) // show progress bar
      Form_1.StatusBar.Item( 1 ) := "Background line counting. This may take some time .... "

      // Call a counting function (Run_MT_Count()) in a separate thread
      // (I can't use the CountLines() method itself as a thread)
      nThread := hb_threadStart( @Run_MT_Count(), oFileMT, pMutexCount )

      DO WHILE hb_mutexSubscribe( pMutexCount, /* 0.00000001 */, @xResp )

         // After Mutex is notified, the xResp variable should contain a matrix with variables:
         // 1st - current position of the file pointer,
         // 2nd - last position of the file pointer,
         // 3th - number of counted records,
         // 4th - status

         Form_1.PBar.VALUE := xResp[ 2 ] / xResp[ 3 ] * 100
         Form_1.StatusBar.Item( 1 ) := "Background line counting. This may take some time ... Counts: " + AllTrim( Str( xResp[ 1 ] ) )

         IF IsControlDefined ( Grid_1, Form_1 )
            nCurrDispRec := oFileGrid:CurrentLine()
            SetProperty( 'Form_1', 'Grid_1', 'ItemCount', xResp[ 1 ] ) // Changing the ItemCount causes the Grid view to scroll to the position of the selected row
            SetProperty( 'Form_1', 'Grid_1', 'Value', nCurrDispRec ) // Re-scroll to the position of the last displayed line before the ItemCount was changed.

            // We count the lines in the secondary object, therefore we have to rewrite the record map to the object displayed in the Grid.
            oFileGrid:PutRecordsMap ( oFileMT:GetRecordsMap() )

            // We do the same with the variable: nLastLine
            oFileGrid:nLastLine := oFileMT:nLastLine

         ENDIF

         DO Events
         hb_ReleaseCPU()

         IF xResp[ 4 ] == 'Done'

            // Counting finished, I am sending the final results.

            IF IsControlDefined ( Grid_1, Form_1 )
               nCurrDispRec := oFileGrid:CurrentLine()
               SetProperty( 'Form_1', 'Grid_1', 'ItemCount', xResp[ 1 ] ) // Changing the ItemCount causes the Grid view to scroll to the position of the selected row
               SetProperty( 'Form_1', 'Grid_1', 'Value', nCurrDispRec ) // Re-scroll to the position of the last displayed line before the ItemCount was changed.

               // We count the lines in the secondary object, therefore we have to rewrite the record map to the object displayed in the Grid.
               oFileGrid:PutRecordsMap ( oFileMT:GetRecordsMap() )

               // We do the same with the variable: nLastLine
               oFileGrid:nLastLine := oFileMT:nLastLine
            ENDIF
            EXIT
         ENDIF

      ENDDO

      Form_1.StatusBar.Item( 1 ) := ""
      ShowProgressBar( 0 ) // hide

      oFileMT:Close()

      // Close Run_MT_Count Thread
      hb_threadDetach( nThread ) // close thread handle
      hb_threadQuitRequest( nThread ) // terminate thread

      // Close mySelf Thread
      hb_threadDetach( pThID ) // close thread handle
      hb_threadQuitRequest( pThID ) // terminate thread

   ENDIF

RETURN NIL

/*
 *  FUNCTION Run_MT_Count( oFile, pMutex )
 *
 *  This function is executed in a separate thread and calls the CountLines()
 *  method of the vfFileRead object to count the number of lines in the file.
 *  It uses a code block (exGauge) to report the progress of the counting
 *  process to the main thread via a mutex.
 *
 *  Parameters:
 *      oFile   - The vfFileRead object representing the file to count lines in.
 *      pMutex  - A mutex used to synchronize access to shared data between threads.
 *
 *  Return:
 *      NIL
 */
FUNCTION Run_MT_Count( oFile, pMutex )
   // Calling the CountLines() method, you can declare as an argument
   // a block of code that will be executed while the method is running.
   // Here we are passing values to another thread using Mutex.
   oFile:CountLines( {| nPos, nLastPos, nLines, cState | hb_mutexNotify( pMutex, { nLines, nPos, nLastPos, cState } ) } /* exGauge */ )

RETURN NIL

/*
 *  FUNCTION Refr_Mem_Stat()
 *
 *  This function continuously updates the memory statistics displayed on the
 *  form. It retrieves the total application memory, available application
 *  memory, and working memory set, and updates the corresponding labels on
 *  the form every 500 milliseconds.
 */
FUNCTION Refr_Mem_Stat()

   DO WHILE .T.

      Form_1.Label_1.VALUE := "Total app memory: " + AllTrim( Str ( GlobalMemoryStatusEx () [ 6 ] / 1024 ^ 2 ) ) + " MB "
      Form_1.Label_2.VALUE := "Available app memory: " + AllTrim( Str ( GlobalMemoryStatusEx () [ 7 ] / 1024 ^ 2 ) ) + " MB "
      Form_1.Label_3.VALUE := "Working memory set: " + AllTrim( Str ( GetProcessMemoryInfo () [ 3 ] / 1024 ^ 2 ) ) + " MB "

      Sleep ( 500 )

   ENDDO

RETURN NIL

/*
 *  FUNCTION Prepare( cFileToOpen, cDelimiter )
 *
 *  This function prepares the application by creating a test file if it
 *  doesn't already exist. It also starts a thread to continuously refresh
 *  the memory statistics displayed on the form.
 *
 *  Parameters:
 *      cFileToOpen - The name of the test file to create.
 *      cDelimiter  - The delimiter used to separate fields in the test file.
 *
 *  Return:
 *      NIL
 */
FUNCTION Prepare ( cFileToOpen, cDelimiter )

   LOCAL hTestFile, i, x, nMaxRec := 70000000, aTestRec

   RELEASE Memory

   hb_threadStart( HB_THREAD_INHERIT_MEMVARS /* Required to access Form_1 on another thread*/, @Refr_Mem_Stat() )

   IF ! File ( cFileToOpen )
      Msginfo ( "A test file will be prepared, this may take some time, please be patient ... " )
      nMaxRec := Val( InputBox( 'Number of records', 'Prepare a sample data file.', AllTrim( Str(nMaxRec ) ) ) )
      aTestRec := { "DP/DP_Kamat_HD.Soll_Umin_HD_Bediener", "DB_Baggerpumpe_1.Ist_Strom_BP1", "DB_Analoganzeigen_Pult.Sandmenge_m3_h_Visu", ;
         "DP/DP_Kamat_HD.Soll_Umin_HD_Kamat", "DB_Analoganzeigen_Pult.Druck_Schneidrad_REAL_VI", "Füllstand_Druck_Land.Fuell_Sumpf_min_unter", "DB_Analoganzeigen_Pult.Speisedruck_REAL_Visu", ;
         "DB_Analoganzeigen_Pult.Foerdermenge_t_h_Visu", "DB_Analoganzeigen_Pult.Druck_vor_BP2_REAL_Visu ", "DB_Analoganzeigen_Pult.Druck_SW_BB_REAL_VISU", ;
         "DB_Analoganzeigen_Pult.Foerderdichte_REAL_Visu", "Füllstand_Druck_Land.Fuell_Sumpf_Land_Differe", "DB_Analoganzeigen_Pult.Druck_nach_BP1_real_Visu", ;
         "DB_Analoganzeigen_Pult.Druck_Hyd_Pumpe_1_Visu", "Füllstand_Druck_Land.Fuell_Sumpf_zeit1", "DP/DP_Kamat_HD.Hochdruck_bar", "DB_Zähler Winden.Zaehler_STP_Mitte_7", ;
         "DB_Zähler Winden.Zaehler_SW_StB_7", "DB_Baggerpumpe_2.Ist_U_min_BP2", "DB_Analoganzeigen_Pult.Saugdruck_BP1_Real_Visu", "DB_Baggerpumpe_1.Ist_U_min_BP1", ;
         "DB_Analoganzeigen_Pult.Foerdergeschw_REAL_Visu", "DB_Zähler Winden.Zaehler_SW_BB_7", "DB_Analoganzeigen_Pult.Druck_Hyd_Pumpe_2_3_Visu", "DB_Baggerpumpe_2.Ist_Strom_BP2", ;
         "DB_Analoganzeigen_Pult.Seillast_REAL_VISU", "DP/DP_Kamat_HD.Ist_U_min_HD_PUMPE", "DB_Analoganzeigen_Pult.Druck_nach_BP2_real_Visu", "DB_Analoganzeigen_Pult.Tiefe_Leiter_PED550_Visu", ;
         "Füllstand_Druck_Land.Druck_vor_BP2_Land_bar_r", "Füllstand_Druck_Land.Fuell_Vorlage_maximu_aus", "DB_Analoganzeigen_Pult.Druck_SW_STB_REAL_VISU", "DB_Zähler Winden.Zaehler_STP_BB_7", "DP/DP_Kamat_HD.Kamat_HD_PUMPE_Soll_Umin", ;
         "DB_Leiterwinde.Baggertiefe_WSP", "Füllstand_Druck_Land.Fuell_Sumpf_minimu_ein", "DB_Analoganzeigen_Pult.Sandmenge_t_tag_Visu", "Füllstand_Druck_Land.Fuell_Sumpf_Land_real", ;
         "DB_Zähler Winden.Zaehler_STP_StB_7", "Füllstand_Druck_Land.Fuell_Sumpf_Land_U_min", "Füllstand_Druck_Land.Druck_nach_BP2_La_bar_r", "DB_Analoganzeigen_Pult.Druck_vor_BP1_REAL_Visu", "Tiefenberechnung.Baggertiefe" }

      WAIT WINDOW "Preparing a test file. This may take some time .... " NOWAIT

      ShowProgressBar( 2 ) // show progress bar
      Form_1.StatusBar.Item( 1 ) := "Preparing a test file. This may take some time .... "

      hTestFile := hb_FCreate( cFileToOpen, FC_NORMAL, FO_EXCLUSIVE )

      x := 0
      FOR i := 1 TO nMaxRec
         x++
         IF x > Len( aTestRec )
            x := 1
         ENDIF
         FWrite( hTestFile, StrZero ( i, 8 ) + cDelimiter + '"' + aTestRec[ x ] + '"' + cDelimiter + '"' + hb_TSToStr( hb_DateTime() ) + '"' + cDelimiter + AllTrim ( Str ( hb_rand32() ) ) + cDelimiter + AllTrim( Str( hb_RandomInt() ) ) + cDelimiter + AllTrim( Str( hb_TToN(hb_DateTime() ) ) ) + CRLF )
         IF i % ( nMaxRec / 100 ) == 0
            WAIT WINDOW "Preparing a test file. This may take some time .... " + Str ( i / nMaxRec * 100 ) + " % " NOWAIT
            Form_1.PBar.VALUE := i / nMaxRec * 100
            DO Events
         ENDIF
      NEXT i
      FClose( hTestFile )

      Form_1.StatusBar.Item( 1 ) := ""
      ShowProgressBar( 0 ) // hide progress bar

      WAIT CLEAR
   ENDIF

RETURN NIL

/*
 *  FUNCTION Test( nTest, cFileToOpen )
 *
 *  This function provides two testing modes for file access:
 *      1 - Tests the hb_F* functions (Harbour file functions).
 *      2 - Tests the vfFileRead class.
 *
 *  Parameters:
 *      nTest       - The test mode (1 or 2).
 *      cFileToOpen - The name of the file to test.
 *
 *  Return:
 *      NIL
 */
FUNCTION Test ( nTest, cFileToOpen )

   LOCAL oFile, nStartTime

   IF nTest = 1

      hb_FUSE( cFileToOpen )

      HB_FGoTo ( 1 )
      MessageRecNo( 1, HB_FReadLn () )

      HB_FGoTo ( 2 )
      MessageRecNo( 2, HB_FReadLn () )

      HB_FGoTo ( 3 )
      MessageRecNo( 3, HB_FReadLn () )

      HB_FGoTo ( 4 )
      MessageRecNo( 4, HB_FReadLn () )

      HB_FGoTo ( 5 )
      MessageRecNo( 5, HB_FReadLn () )

      HB_FGoTo ( 2 )
      MessageRecNo( 2, HB_FReadLn () )

      HB_FGoTo ( 3 )
      MessageRecNo( 3, HB_FReadLn () )

      WAIT WINDOW "Counting the number of records ..." NOWAIT

      MsgInfo ( Str( HB_FLastRec() ) + " records have been enumerated." )

      WAIT CLEAR

      hb_FUSE()

   ELSE

      oFile := vfFileRead():New( cFileToOpen )
      oFile:Open()

      IF oFile:Error()
         MsgStop ( "Error " + Str( oFile:ErrorNo() ) )
         RETURN NIL
      ENDIF

      oFile:GoTo( 1 )
      MessageRecNo( 1, oFile:ReadLine() )

      oFile:GoTo( 2 )
      MessageRecNo( 2, oFile:ReadLine() )

      oFile:GoTo( 3 )
      MessageRecNo( 3, oFile:ReadLine() )

      oFile:GoTo( 4 )
      MessageRecNo( 4, oFile:ReadLine() )

      oFile:GoTo( 5 )
      MessageRecNo( 5, oFile:ReadLine() )

      oFile:GoTo( 2 )
      MessageRecNo( 2, oFile:ReadLine() )

      oFile:GoTo( 3 )
      MessageRecNo( 3, oFile:ReadLine() )

      WAIT WINDOW "Counting the number of records ..." NOWAIT

      ShowProgressBar( 2 ) // show progress bar
      Form_1.StatusBar.Item( 1 ) := "Counting the number of records ... This may take some time .... "

      // code block executed while the CountLines() method is running
      oFile:exGauge := {| nPos, nLastPos, nLines, cState | ( Form_1.StatusBar.Item( 1 ) := "Counting the number of records: " + AllTrim ( Str ( nLines ) ) + "... This may take some time ... " + cState, Form_1.PBar.VALUE := nPos / nLastPos * 100, DoEvents() ) }

      nStartTime := Seconds()
      MsgInfo ( Str( oFile:CountLines() ) + " records have been enumerated is seconds: " + AllTrim ( Str( Seconds() - nStartTime ) ) )

      Form_1.StatusBar.Item( 1 ) := ""
      ShowProgressBar( 0 ) // hide progress bar

      WAIT CLEAR

      oFile:Close()

   ENDIF

RETURN NIL

/*
 *  FUNCTION MessageRecNo( nValidRec, cRec )
 *
 *  This function checks if the record number in a given record matches the
 *  expected record number. It displays a message indicating whether the
 *  record number is correct or invalid.
 *
 *  Parameters:
 *      nValidRec - The expected record number.
 *      cRec      - The record to check.
 *
 *  Return:
 *      NIL
 */
FUNCTION MessageRecNo( nValidRec, cRec )
   IF Val( cRec ) == nValidRec
      MsgInfo ( cRec, "The record number is correct." )
   ELSE
      MsgStop ( cRec, "The record number is invalid!, should be " + AllTrim( Str( nValidRec ) ) )
   ENDIF

RETURN NIL

/*
 *  FUNCTION ShowProgressBar( nMode, nMin, nMax, nCol, nLenght )
 *
 *  This function manages the visibility and properties of a progress bar
 *  control. It allows initializing, showing, hiding, and setting the range
 *  and value of the progress bar.
 *
 *  Parameters:
 *      nMode   - The mode of operation:
 *                  1 - Initialize the progress bar.
 *                  2 - Set/show the progress bar.
 *                  3 - Release the progress bar.
 *                  0 or other - Hide the progress bar.
 *      nMin    - The minimum value of the progress bar (default: 1).
 *      nMax    - The maximum value of the progress bar (default: 100).
 *      nCol    - The column position of the progress bar (default: 20).
 *      nLenght - The length of the progress bar (default: 740).
 *
 *  Return:
 *      NIL
 */
FUNCTION ShowProgressBar( nMode, nMin, nMax, nCol, nLenght )
   DEFAULT nMode := 1 // 1 = init, 2 = set/show, 3 = close, 0/other = hide
   DEFAULT nMin := 1, nMax := 100
   DEFAULT nCol := 20, nLenght := 740
   DO CASE
   CASE nMode = 1
      DEFINE PROGRESSBAR PBar
         PARENT Form_1
         ROW 5
         COL nCol
         WIDTH nLenght
         HEIGHT 12
         RANGEMIN nMin
         RANGEMAX nMax
         VALUE nMin
         TOOLTIP ""
         HELPID NIL
         VISIBLE .F.
         SMOOTH .T.
         VERTICAL .F.
         BACKCOLOR NIL
         FORECOLOR NIL
      END PROGRESSBAR

      /* put the progress bar in the status bar */
      SETPARENT( Form_1.PBar.Handle, Form_1.STATUSBAR.Handle )

   CASE nMode = 3
      Form_1.PBar.RELEASE

   CASE nMode = 2
      Form_1.PBar.RANGEMIN := nMin
      Form_1.PBar.RANGEMAX := nMax
      Form_1.PBar.VALUE := nMin
      Form_1.PBar.Visible := .T.
      DO EVENTS

      OTHER
      Form_1.PBar.Visible := .F.
      DO EVENTS
   ENDCASE

RETURN NIL


#pragma BEGINDUMP

#include <windows.h>
#include <psapi.h>
#include <hbapi.h>


//        GlobalMemoryStatusEx () --> return array with 7 numbers
HB_FUNC ( GLOBALMEMORYSTATUSEX )
{
   MEMORYSTATUSEX statex;
   statex.dwLength = sizeof (MEMORYSTATUSEX);
   GlobalMemoryStatusEx (&statex);   // reflects the state of memory at the time of the call
   hb_reta (7);
   hb_storvnll ( statex.dwMemoryLoad     , -1, 1 );   // approximate percentage of physical memory that is in use (0 indicates no memory use and 100 indicates full memory use)
   hb_storvnll ( statex.ullTotalPhys     , -1, 2 );   // amount of actual physical memory, in bytes
   hb_storvnll ( statex.ullAvailPhys     , -1, 3 );   // amount of physical memory currently available, in bytes
   hb_storvnll ( statex.ullTotalPageFile , -1, 4 );   // current committed memory limit for the system or the current process, whichever is smaller, in bytes
   hb_storvnll ( statex.ullAvailPageFile , -1, 5 );   // maximum amount of memory the current process can commit, in bytes
   hb_storvnll ( statex.ullTotalVirtual  , -1, 6 );   // size of the user-mode portion of the virtual address space of the calling process, in bytes
   hb_storvnll ( statex.ullAvailVirtual  , -1, 7 );   // amount of unreserved and uncommitted memory currently in the user-mode portion of the virtual address space of the calling process, in bytes
}


//        SetParent (hWndChild, hWndNewParent)
//        GetProcessMemoryInfo ( [ ProcessID ] )  --> return array with 9 numbers
HB_FUNC ( GETPROCESSMEMORYINFO )
{
   typedef BOOL (WINAPI *Func_GetProcessMemoryInfo) (HANDLE,PPROCESS_MEMORY_COUNTERS,DWORD);
   static Func_GetProcessMemoryInfo pGetProcessMemoryInfo = NULL;

   PROCESS_MEMORY_COUNTERS pmc;

   DWORD ProcessID;

   HANDLE hProcess;

   if (pGetProcessMemoryInfo == NULL)
   {   HMODULE hLib = LoadLibrary (TEXT("Kernel32.dll"));
       pGetProcessMemoryInfo = (Func_GetProcessMemoryInfo) GetProcAddress(hLib, "K32GetProcessMemoryInfo");
   }

   if (pGetProcessMemoryInfo == NULL)
   {   HMODULE hLib = LoadLibrary (TEXT("Psapi.dll"));
       pGetProcessMemoryInfo = (Func_GetProcessMemoryInfo) GetProcAddress(hLib, "GetProcessMemoryInfo");
   }

   if (pGetProcessMemoryInfo == NULL)
       return;

   ProcessID = HB_ISNUM (1) ? (DWORD) hb_parnl(1) : GetCurrentProcessId();

   hProcess = OpenProcess (PROCESS_QUERY_INFORMATION | PROCESS_VM_READ, FALSE, ProcessID);
   if (NULL == hProcess)
       return;

   pmc.cb = sizeof(pmc);
   if (pGetProcessMemoryInfo (hProcess, &pmc, sizeof(pmc)))
   {
       hb_reta (9);
       hb_storvnll ( pmc.PageFaultCount             , -1, 1 );   // The number of page faults (Numero de fallos de pagina)
       hb_storvnll ( pmc.PeakWorkingSetSize         , -1, 2 );
       hb_storvnll ( pmc.WorkingSetSize             , -1, 3 );   // The current working set size, in bytes (Cantidad de memoria fisica usada actualmente por el proceso)
       hb_storvnll ( pmc.QuotaPeakPagedPoolUsage    , -1, 4 );
       hb_storvnll ( pmc.QuotaPagedPoolUsage        , -1, 5 );   // The current paged pool usage, in bytes (Uso actual del bloque de memoria paginado)
       hb_storvnll ( pmc.QuotaPeakNonPagedPoolUsage , -1, 6 );
       hb_storvnll ( pmc.QuotaNonPagedPoolUsage     , -1, 7 );   // The current nonpaged pool usage, in bytes (Uso actual del bloque de memoria no paginado)
       hb_storvnll ( pmc.PagefileUsage              , -1, 8 );   // Total amount of memory that the memory manager has committed for the running this process, in bytes (Cantidad de memoria virtual reservada por el sistema para el proceso)
       hb_storvnll ( pmc.PeakPagefileUsage          , -1, 9 );
   }

   CloseHandle( hProcess );
}

#pragma ENDDUMP

// *********************** THE END *********************
