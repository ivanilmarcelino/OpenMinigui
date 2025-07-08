/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * February 15, 2025
 * Created by Pierpaolo Martinello assistenza@pcmodula.it
 * and based on DirWatcher
 * Copyright 2016 P.Chornyj <myorg63@mail.ru>
 *
 */

#define _HMG_OUTLOG

#include "minigui.ch"
#include "hbthread.ch"
#include "fileio.ch"
#include "WinPrint.ch"

#define INFINITE        0xFFFFFFFF

// WaitForSingleObject() returns value
#define WAIT_TIMEOUT    0x00000102
#define WAIT_ABANDONED  0x00000080
#define WAIT_OBJECT_0   0x00000000
#define WAIT_OBJECT_1   ( WAIT_OBJECT_0 + 1 )

// for wapi_Find*ChangeNotification
#define FILE_NOTIFY_CHANGE_FILE_NAME  0x00000001
#define FILE_NOTIFY_CHANGE_DIR_NAME   0x00000002
#define FILE_NOTIFY_CHANGE_ATTRIBUTES 0x00000004
#define FILE_NOTIFY_CHANGE_SIZE       0x00000008
#define FILE_NOTIFY_CHANGE_LAST_WRITE 0x00000010
#define FILE_NOTIFY_CHANGE_SECURITY   0x00000100

#define MAX_LINE 146
#define MAX_PORT 98

#define PROGRAM 'HMG Spooler'
#define COPYRIGHT ' Pierpaolo Martinello, 2025'

STATIC lStartWatch   := .F.
STATIC lTimerRunning := .F.
STATIC hMainWin
STATIC hStart
/*
*/
FUNCTION Main( cDir )

   LOCAL cLogFile := hb_dirbase()+hb_FNameName( exename())+".log"
   SET MULTIPLE OFF

   IF Filesize(cLogFile) > 1000000
      hb_FileDelete( cLogFile )
   EndIF

   Touch( cLogFile )

   SET LOGFILE TO (cLogFile)

   SET DEFAULT ICON TO "MAIN"

   hMainWin  := WIN_N2P( Application.Handle )

   DEFINE WINDOW Form_1 ;
          AT 0, 0 ;
          WIDTH 0 HEIGHT 0 ;
          TITLE PROGRAM ;
          MAIN NOSHOW ;
          NOTIFYICON "MAIN" ;
          NOTIFYTOOLTIP PROGRAM + ": Right Click for Menu" ;
          ON NOTIFYCLICK  ShowInfo() ;
          ON INIT ( hStart := ReadIni( cDir ) , if (hStart['lAuto'] ,StartWatch( cDir ), NIL ) ) ;

   DEFINE NOTIFY MENU
          ITEM '&Start Watch' IMAGE "HP_PRINT2" ACTION StartWatch( cDir )
          SEPARATOR
          ITEM '&Restart Print queues' IMAGE "STARTUP";
               Action StartUpSpooler()
          SEPARATOR
          ITEM '&Clean Print queues' IMAGE "M_CLEAN";
               Action CLeanQueues()
          SEPARATOR
          ITEM '&Mail to author...' IMAGE "MAIL" ;
               ACTION ShellExecute( 0, "open", "rundll32.exe", ;
               "url.dll,FileProtocolHandler " + ;
               "mailto:assistenza@pcmodula.it?cc=&bcc=" + ;
               "&subject=HMG%20Spooler%20Feedback:" + ;
               "&body=How%20are%20you%2C%20Pierpaolo%3F%0A"+;
               "I%20am%20using%20HmgSpooler!%0A%0A"+;
               "Regards.%0A"+getusername(), , 1 )
          ITEM '&About...' IMAGE 'M_INFO' ACTION  AlertInfo( "About " + PROGRAM + ' version 1.0' + ;
                                                  CRLF + "Copyright " + Chr( 169 ) + COPYRIGHT,"About",,,.T.)
          SEPARATOR
          ITEM 'E&xit 'IMAGE 'M_EXIT'  ACTION Form_1.Release
   END MENU

   END WINDOW

   ACTIVATE WINDOW Form_1

   RETURN NIL
/*
*/
*------------------------------------------------------------------------------*
PROCEDURE ReadIni( cNewDir )
*------------------------------------------------------------------------------*
   STATIC cFolder := "", cNewFolder := ""
   LOCAL cExePath := Left( ExeName(), RAt( "\", ExeName() ) )
   LOCAL IniFile  := ChangeFileExt( hb_ProgName(), '.ini' )
   LOCAL Filelist := "Pdf,Txt"
   LOCAL tDelay   := 0
   LOCAL YMsg     := .T.
   LOCAL Clean    := .T.
   LOCAL lAuto    := .F.
   LOCAL ldialog  := .F.
   LOCAL Printer  := Getdefaultprinter()
   LOCAL hRtv     := HASH()

   HB_hCaseMatch(hRtv,.F.)

   IF Empty( cNewDir )
      IF File ( IniFile )
         BEGIN INI FILE IniFile
               GET cNewFolder SECTION "WorkDir" ENTRY "Folder" Default cExePath
               GET Filelist   SECTION "WorkDir" ENTRY "Type" Default "Pdf,Txt"
               GET tDelay     SECTION "WorkDir" ENTRY "Delay" Default  0
               GET YMsg       SECTION "WorkDir" ENTRY "Msg" Default "T"
               Get Printer    SECTION "Workdir" ENTRY "Printer" Default Getdefaultprinter()
               GET ldialog    SECTION "WorkDir" ENTRY "Dialog" Default "F"
               GET Clean      SECTION "WorkDir" ENTRY "Clean" Default "T"
               GET lAuto      SECTION "WorkDir" ENTRY "Auto" Default "F"
         END INI
      EndIF
   Else
      cNewFolder := cNewDir
   EndIF

   IF !Empty( cNewFolder ) .and. hb_DirExists( cNewFolder )
      cFolder := cNewFolder
   Else
      cFolder := cExePath
   EndIF

   IF !File( IniFile ) .or. ( cNewFolder != hb_dirbase() )
      BEGIN INI File IniFile
            SET SECTION "WorkDir" ENTRY "Folder" to cFolder
            SET SECTION "WorkDir" ENTRY "Type" TO Filelist
            SET SECTION "WorkDir" ENTRY "Delay" TO Tdelay
            SET SECTION "WorkDir" ENTRY "Msg" TO YMsg
            SET SECTION "Workdir" ENTRY "Printer" TO Printer
            SET SECTION "WorkDir" ENTRY "Dialog" TO lDialog
            SET SECTION "WorkDir" ENTRY "Clean" TO Clean
            SET SECTION "WorkDir" ENTRY "Auto" TO lAuto
      END INI
   EndIF

   //Transform into array and insert the point
   Filelist := hb_atokens(Filelist,",")
   aeval (Filelist,{|x,y| Filelist[y] := "."+x } )

   hRtv['cFolder']   := cFolder
   hRtv['aFileType'] := Filelist
   hRtv['tDelay']    := Min( 50, tDelay )
   hRtv['lMsg']      := Ymsg
   hRtv['cPrinter']  := Printer
   hRtv['lClean']    := Clean
   hRtv['lAuto']     := lAuto
   hRtv['ldialog']   := LDialog

Return hRtv
/*
*/
PROCEDURE StartWatch( cDir )

   LOCAL cDirectory := hStart['cFolder']

   StartUpSpooler( .T. )

   IF !lStartWatch

      FORM_1.NOTIFYTOOLTIP := PROGRAM +"  >> WATCH ACTIVE <<"

      lStartWatch := .T.

      IF hb_DirExists( cDirectory )
         IF hb_FileExists( cDirectory + "quit" )
            hb_FileDelete( cDirectory + "quit" )
         EndIF

         hb_threadDetach( hb_threadStart( HB_THREAD_INHERIT_MEMVARS, @WatchDirectory(), cDirectory ) )
      EndIF

   EndIF

   RETURN
/*
*/
FUNCTION WatchDirectory( cDir )

   LOCAL pChangeHandle
   LOCAL nWaitStatus
   LOCAL lRunAnyway := .T.
   LOCAL lOnce := .T.
   LOCAL aFilesPrev := {}
   LOCAL aFilesCurr := {}
   LOCAL aNewFiles
   LOCAL aDeletedFiles
   LOCAL fn
   LOCAL Prg2Print
   LOCAL Param

   aFilesPrev :=Directory(cDir + "*.*")

   // watch file name changes ( file was CREATED, RENAMED or DELETED)
   pChangeHandle := wapi_FindFirstChangeNotification( cDir, .F., FILE_NOTIFY_CHANGE_FILE_NAME )

   IF INVALID_HANDLE_VALUE( pChangeHandle )
      ? "ERROR: FindFirstChangeNotification function failed."
      RETURN wapi_GetLastError()
   EndIF

   // Change notification is set. Now we can wait on notification handle.
   DO WHILE lRunAnyway

      IF lOnce
         ? hb_StrFormat( "%1$s Waiting for notification for " + cDir, hb_TtoC(hb_DateTime(),"DD-MM-YYYY","HH:MM:SS") )
         lOnce := .F.
      EndIF

      // IF the function succeeds, the return value indicates
      // the event that caused the function to return.
      nWaitStatus = wapi_WaitForSingleObject( pChangeHandle, INFINITE )

      SWITCH nWaitStatus

      CASE WAIT_OBJECT_0
         // A file was CREATED, RENAMED or DELETED in the directory.
         // _Refresh_ this directory and _restart_ the notification.

         aFilesCurr    :={}
         aFilesCurr := Directory(cDir + "*.*")
         aNewFiles     := DifferenzeArrays(aFilesCurr, aFilesPrev)
         aDeletedFiles := DifferenzeArrays(aFilesPrev, aFilesCurr)

         RefreshDirectory( cDir, @lRunAnyway ,@adeletedFiles, @aNewFiles)

         Take_action( aNewFiles )

         aFilesPrev := aFilesCurr

         IF lRunAnyway
            IF ! wapi_FindNextChangeNotification( pChangeHandle )
               ? "ERROR: FindNextChangeNotification function failed."
               RETURN wapi_GetLastError()
            EndIF
         Else
            wapi_FindCloseChangeNotification( pChangeHandle )
         EndIF

         EXIT

      CASE WAIT_TIMEOUT
         // A timeout occurred, this would happen IF some value other
         // than INFINITE is used in the Wait call and no changes occur.
         // In a single-threaded environment you might not want an
         // INFINITE wait.
         ? "No changes in the timeout period."
         EXIT

      OTHERWISE
         ? "ERROR: Unhandled nWaitStatus."
         RETURN wapi_GetLastError()

      ENDSWITCH

   END WHILE

   ? "EXIT: Quit signal was received."

RETURN 0
/*
*/
PROCEDURE RefreshDirectory( cDir, lRunAnyway , adeletedFiles , aNewFiles )

   // This is where you might place code to refresh your
   // directory listing, but not the subtree because it
   // would not be necessary.
   Local nF
   LOCAL aDetect := directory(hStart['cFolder']+"*.prg")

   for nf = 1 to len(adeletedfiles)
       ? hb_StrFormat( "%1$s was deleted at %2$s", adeletedfiles[nf] ,hb_TtoC(hb_DateTime(),"DD-MM-YYYY","HH:MM:SS") )
   Next

   for nf = 1 to len(aNewFiles)
       ? hb_StrFormat( "%1$s was added at %2$s", aNewFiles[nf] ,hb_TtoC(hb_DateTime(),"DD-MM-YYYY","HH:MM:SS") )
   Next

   IF hb_FileExists( cDir + "quit" )
      lRunAnyway := .F.
   EndIF

RETURN
/*
*/
*------------------------------------------------------------------------------*
PROCEDURE Take_action( aNewFiles )
*------------------------------------------------------------------------------*
   LOCAL Fn, cTmpName
   // Now perform the printing command
   For Fn = 1 to len(aNewFiles)

      IF hStart['lMsg']
       ShowInfo(CHR(9)+aNewFiles[Fn]+CRLF+CRLF+[ Will be sent to the printer:]+CRLF+CRLF;
               +hStart['cPrinter']+ if(hStart['lClean'],[ and deleted.],[.]) )
      EndIF

      cTmpName :=Lower( hb_FnameExt(aNewFiles[Fn]) )

      DO EVENTS ; wApi_Sleep(hStart['tDelay'])

      Do case
         case cTmpName == ".txt"
              TxtPrint(aNewFiles[Fn],.F.,hStart['lDialog'] ) // cFileName, lprew, lDialog
         Case cTmpName == ".pdf"
              PdfPrint (aNewFiles[Fn], , hStart['lDialog'] ) // cFileName, lprew, lDialog
         OTherwise
              // msgstop(cTmpname)
              // OPEN FILE // Work in progress
              ShowInfo( "A new file has been added !")
      EndCase

      wApi_Sleep(hStart['tDelay'])

      ? hb_StrFormat( "%1$s was printed at %2$s", aNewFiles[Fn] ,hb_TtoC(hb_DateTime(),"DD-MM-YYYY","HH:MM:SS.sss" ) )

      // Delete the printed file
      IF hStart['lClean']   // default yes
         hb_FileDelete ( hStart['cFolder']+aNewFiles[Fn] )
      EndIF
   Next

   // End of controls spooler

Return
/*
*/
*------------------------------------------------------------------------------*
FUNCTION DifferenzeArrays(aArray1, aArray2)
*------------------------------------------------------------------------------*
    LOCAL aDiff := {}
    LOCAL lFound := .F.
    Local eSub1, eSub2, eFt

    FOR EACH eSub1 IN aArray1
        lFound := .F.

        FOR EACH eSub2 IN aArray2
            IF ArraysEqual(eSub1, eSub2 )
                lFound := .T.
                EXIT
            ENDIF
        NEXT

        IF !lFound
           // NOW i apply, the Log filters
           For each eFt in hStart['aFileType']
               IF Lower( hb_FNameExt(eSub1[1] ) ) == Lower( eFt )
                IF Atail(aDiff) != eFt
                   AADD(aDiff, eSub1[1])
                EndIF
             EndIF
           Next
        ENDIF
    NEXT

RETURN aDiff
/*
*/
*------------------------------------------------------------------------------*
FUNCTION ArraysEqual(aSubArray1, aSubArray2)
*------------------------------------------------------------------------------*
    LOCAL lEqual := .T.
    LOCAL nIndex, uElem1

    IF LEN(aSubArray1) != LEN(aSubArray2)
        RETURN .F.
    ENDIF

    FOR EACH uElem1 IN aSubArray1
        nIndex := ASCAN(aSubArray1, uElem1 )
        IF aSubArray1[nIndex] != aSubArray2[nIndex]
            lEqual := .F.
            EXIT
        ENDIF
    NEXT

RETURN lEqual
/*
*/
*------------------------------------------------------------------------------*
FUNCTION touch(cFile)
*------------------------------------------------------------------------------*
    LOCAL nHandle

    // Tries to open the file in writing only mode
    nHandle := FOpen(cFile, FO_WRITE)

    // IF the file already exists, close it and go out
    IF nHandle > 0
        FClose(nHandle)
        RETURN .T. // Return .t. (true) IF the file existed
    EndIF

    // IF the file does not exist, create it empty
    nHandle := FCreate(cFile)

    // IF the creation is successful, close the file and go out
    IF nHandle > 0
        FClose(nHandle)
        RETURN .T. // Returns .t. (true) IF the file has been created
    EndIF

    // IF an error occurs, return .F. (false)
RETURN .F.
/*
*/
*------------------------------------------------------------------------------*
Procedure TxtPrint( cFile ,lPrev, lDialog )
*------------------------------------------------------------------------------*
    LOCAL hbprn, cLine, nLines := 0, nLine := 0, ;
          nPage := 1, nMaxWidth := 0, nMaxLine, nCnt, cFontName, nFontsize

    DEFAULT lPrev TO .T., lDialog TO .F.

    cTexto := Memoread( hStart['cFolder']+cFile )

    IF ISOEMTEXT(substr(cTexto,5,50))
            cFontname := 'Terminal'
            nFontsize := 12
    Else
            cFontname := 'Arial'
            nFontsize := 10
    EndIF

    IF !empty(cTexto)
        cTexto := IF( ISOEMTEXT(substr(cTexto,5,50)), HB_OEMTOANSI(cTexto), cTexto )
        nLines := MLCOUNT( cTexto, MAX_LINE )
        For nCnt := 1 To nLines
            nMaxWidth := MAX( nMaxWidth, Len( RTrim( MemoLine(cTexto, MAX_LINE, nCnt) ) ) )
        Next
        INIT PRINTSYS

        IF lPrev
            IF lDialog
               SELECT BY DIALOG PREVIEW
            Else
               SELECT PRINTER hStart['cPrinter'] PREVIEW
            EndIF

            //SET PREVIEW RECT Frm1.Row,Frm1.Col,(Frm1.Row)+Frm1.Height,(Frm1.Col)+Max(740,Frm1.Width)
        Else
            IF lDialog
               SELECT BY DIALOG
            Else
               SELECT PRINTER hStart['cPrinter']
            EndIF
        EndIF

        IF EMPTY(HBPRNERROR)
            DEFINE FONT "f0" NAME cFontName SIZE nFontSize
            SELECT FONT "f0"
            START DOC NAME cFile
            IF nMaxWidth < MAX_PORT
                SET PAGE ORIENTATION DMORIENT_PORTRAIT PAPERSIZE DMPAPER_A4 FONT "f0"
            Else
                SET PAGE ORIENTATION DMORIENT_LANDSCAPE PAPERSIZE DMPAPER_A4 FONT "f0"
            EndIF

            IF lprev
                IF HBPRNMAXCOL > MAX_LINE
                    MsgExclamation('Text width is greater than page width')
                EndIF
            EndIF

            nMaxLine := HBPRNMAXROW - 1

            START PAGE

            // @nLine, nMaxWidth / IF(nMaxWidth < MAX_PORT, 1.5, 2) SAY "- " + Ltrim(Str(nPage++)) + " -" TO PRINT

            For nCnt := 1 To nLines

                cLine := RTrim( MemoLine(cTexto, MAX_LINE, nCnt) )

                @++nLine, 6 SAY cLine TO PRINT

                  IF nCnt == nLines .OR. nLine > nMaxLine
                      IF nCnt # nLines
                        END PAGE
                        nLine := 0
                        START PAGE
                        // @nLine, nMaxWidth / IF(nMaxWidth < MAX_PORT, 1.5, 2) SAY "- " + Ltrim(Str(nPage++)) + " -" TO PRINT
                    EndIF
                EndIF
            Next

            END PAGE
            END DOC
            RELEASE PRINTSYS
        EndIF
    EndIF
return
/*
*/
*------------------------------------------------------------------------------*
PROCEDURE PdfPrint (cPdf, lprev, lDialog)
*------------------------------------------------------------------------------*
   LOCAL cParams,param

   DEFAULT lPrev TO .T., lDialog to .F.

   IF cPdf == NIL
      Return Nil
   EndIF

   IF lDialog
      cParams := [ -print-dialog  "]
      cParams += hStart['cFolder']+cPdf+["]
   Else
       cParams := [ -print-to ]
       cParams += ["]+hStart['cPrinter']+[" "]
       cParams += hStart['cFolder']+cPdf+["]
       cParams += [ -print-settings "paper=A4,fit" ]
   Endif

   cParams += [ -exit-when-done ]

   ShellExecute(, "OPEN", "SumatraPDF\SumatraPDF.exe", cParams,, 1)

Return
/*
*/
*------------------------------------------------------------------------------*
PROCEDURE CleanQueues( )
*------------------------------------------------------------------------------*
Local act, e, ft, aGen := directory(hStart['cFolder'] +"*.*")
      act := AlertOkcancel ("This opration will be delete all files in the print queue !")

   if act
      For each E in aGen
          For each ft in hStart['aFileType']
              IF Lower(hb_FNameExt(e[1])) == lower(ft)
                 hb_FileDelete(hStart['cFolder']+e[1] )
              EndIF
          Next
      Next
   Endif

Return
/*
*/
*------------------------------------------------------------------------------*
PROCEDURE StartUpSpooler( NoMsg )
*------------------------------------------------------------------------------*
Local act , e , ft , lOldMsg , aAct := {} , aGen := directory(hStart['cFolder'] +"*.*")

Default NoMsg To .F.
lOldMsg := hStart['lMsg']

// count for available files to print
For each E in aGen
    For each ft in hStart['aFileType']
        IF Lower(hb_FNameExt(e[1])) == lower(ft)
           aadd(aAct, e[1] )
        EndIF
    Next
Next

if len( aAct ) > 0
   if NoMsg .or. hStart['lAuto'] .and. !hStart['lMsg']
      act := .T.
   Else
      act := AlertOkcancel ("This opration will be execute the " + HB_nTos( len( Aact ) )+" old print queue(s) !")
   Endif
   if act
      hStart['lMsg'] := .F.
      Take_action( aAct )
   Endif
   hStart['lMsg'] := lOldMsg
Else
   ShowInfo([ There are no print queues. ] )
Endif
Return
/*
*/
*------------------------------------------------------------------------------*
Procedure ShowInfo(cMsg)
*------------------------------------------------------------------------------*
Default cMsg to "This Service is "+IF ( lStartWatch ,"Active !"+ CRLF+" on "+hStart['cFolder'],"Stopped !")

   if IsWin10OrLater ()
      MsgBalloon( cMsg ,"")
      inkeyGui(1)
      ActivateNotifyMenu()
   Else
      MessageBoxTimeOut( space(8) + cMsg , PROGRAM ,0,3000 )
   Endif

Return
/*
*/
// Notify Icon Infotip flags
#define NIIF_NONE       0x00000000
// icon flags are mutualy exclusive
// and take only the lowest 2 bits
#define NIIF_INFO       0x00000001
#define NIIF_WARNING    0x00000002
#define NIIF_ERROR      0x00000003
*--------------------------------------------------------*
Static Procedure MsgBalloon( cMessage, cTitle, nIconIndex )
*--------------------------------------------------------*
    Local i := GetFormIndex( "Form_1" )

    Default cMessage := "Prompt", cTitle := PROGRAM, nIconIndex := NIIF_INFO

    ShowNotifyInfo( _HMG_aFormhandles[i], .F. , NIL, NIL, NIL, NIL, 0 )

    ShowNotifyInfo( _HMG_aFormhandles[i], .T. , ;
        LoadTrayIcon( GetInstance(), _HMG_aFormNotifyIconName[i] ), ;
        _HMG_aFormNotifyIconToolTip[i], cMessage, cTitle, nIconIndex )

Return
/*
*/
*--------------------------------------------------------*
Static Procedure ActivateNotifyMenu()
*--------------------------------------------------------*
    Local i := GetFormIndex( "Form_1" )

    ShowNotifyInfo( _HMG_aFormhandles[i], .F. , NIL, NIL, NIL, NIL, 0 )

    ShowNotifyIcon( _HMG_aFormhandles[i], .T. , LoadTrayIcon( GetInstance(), ;
        _HMG_aFormNotifyIconName[i] ), _HMG_aFormNotifyIconToolTip[i] )
Return
/*
*/
#pragma BEGINDUMP

#define _WIN32_IE      0x0500
#define _WIN32_WINNT   0x0400

#include <windows.h>
#include <hbapi.h>

#include "hbwapi.h"

/*
 HANDLE WINAPI
    FindFirstChangeNotification( LPCTSTR lpPathName, BOOL bWatchSubtree, DWORD dwNotifyFilter );
*/
HB_FUNC( WAPI_FINDFIRSTCHANGENOTIFICATION )
{
   if( HB_ISCHAR( 1 ) )
   {
      HANDLE handle;
      void   *hText;

      handle = FindFirstChangeNotification( HB_PARSTRDEF( 1, &hText, NULL ),
                                            hbwapi_par_BOOL( 2 ), hbwapi_par_DWORD( 3 ) );
      hb_strfree( hText );

      if ( INVALID_HANDLE_VALUE == handle )
      {
         hbwapi_SetLastError( GetLastError() );

         hb_retptr( NULL );
      }
      else
      {
         hbwapi_ret_raw_HANDLE( handle );
      }
   }
   else
   {
      hb_retptr( NULL );
   }
}

//BOOL FindNextChangeNotification(HANDLE hChangeHandle);
HB_FUNC( WAPI_FINDNEXTCHANGENOTIFICATION )
{
   HANDLE handle = hbwapi_par_raw_HANDLE( 1 );

   if( handle )
   {
      hbwapi_ret_L( FindNextChangeNotification( handle ) );
   }
   else
   {
      hb_retl( HB_FALSE );
   }
}

//BOOL WINAPI FindCloseChangeNotification( HANDLE hChangeHandle );
HB_FUNC( WAPI_FINDCLOSECHANGENOTIFICATION )
{
   HANDLE handle = hbwapi_par_raw_HANDLE( 1 );

   if( handle )
   {
      hbwapi_ret_L( FindCloseChangeNotification( handle ) );
   }
   else
   {
      hb_retl( HB_FALSE );
   }
}

HB_FUNC( INVALID_HANDLE_VALUE )
{
   HANDLE handle = hbwapi_par_raw_HANDLE( 1 );

   if ( NULL != handle )
   {
      hbwapi_ret_L( ( INVALID_HANDLE_VALUE == handle )  );
   }
}

static void ShowNotifyInfo(HWND hWnd, BOOL bAdd, HICON hIcon, LPSTR szText, LPSTR szInfo, LPSTR szInfoTitle, DWORD nIconIndex);

HB_FUNC ( SHOWNOTIFYINFO )
{
    ShowNotifyInfo( (HWND) hb_parnl(1), (BOOL) hb_parl(2), (HICON) hb_parnl(3), (LPSTR) hb_parc(4),
            (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (DWORD) hb_parnl(7) );
}

static void ShowNotifyInfo(HWND hWnd, BOOL bAdd, HICON hIcon, LPSTR szText, LPSTR szInfo, LPSTR szInfoTitle, DWORD nIconIndex)
{
    NOTIFYICONDATA nid;

    ZeroMemory( &nid, sizeof(nid) );

    nid.cbSize        = sizeof(NOTIFYICONDATA);
    nid.hIcon        = hIcon;
    nid.hWnd        = hWnd;
    nid.uID            = 0;
    nid.uFlags        = NIF_INFO | NIF_TIP | NIF_ICON;
    nid.dwInfoFlags        = nIconIndex;

    lstrcpy( nid.szTip, TEXT(szText) );
    lstrcpy( nid.szInfo, TEXT(szInfo) );
    lstrcpy( nid.szInfoTitle, TEXT(szInfoTitle) );

    if(bAdd)
        Shell_NotifyIcon( NIM_ADD, &nid );
    else
        Shell_NotifyIcon( NIM_DELETE, &nid );

    if(hIcon)
        DestroyIcon( hIcon );
}

#pragma ENDDUMP