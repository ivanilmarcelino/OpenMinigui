/*
  File date changer utility by Martinello Pierpaolo
  Creation Date 27/04/2024
  From the tests carried out I found that the function hb_FSetDateTime()
  it doesn't change all the dates, so I decided to force the replacement
  applying a modified version of hb_fsSetFileTime().
  This version uses Tsbrowse instead of a Grid component
*/

#include "minigui.ch"
#include "tsbrowse.ch"
#include <Fileio.ch>

MEMVAR cErrType, Grid1, aGrid

FUNCTION Main()

   LOCAL nFSize := 16, cFName := "Arial", Dfirst
   LOCAL nWDate, nWTime, x, y, nG, x2, x3, nBw, cSay, nHObj
   PUBLIC Grid1
   PRIVATE aGrid := { {,,,} }

   SET CENTURY ON
   SET DATE FRENCH
   SET TOOLTIP BALLOON ON

   SET NAVIGATION EXTENDED

   nWDate := 230
   nWTime := 110
   nG := 20
   nHObj := nFSize * 2
   nBw := 110

   DEFINE WINDOW Form_1 ;
         AT 0, 0 ;
         WIDTH 690 ;
         HEIGHT Min ( nHObj * 24, 555 ) ;
         TITLE "MiniGUI: Files Date Time Change By Pierpaolo Martinello 2024" ;
         ICON "MAIN" ;
         MAIN ;
         NOSIZE NOMAXIMIZE ;
         FONT cFName SIZE nFSize ;
         ON INIT ONOFF()

      This.OnDropFiles := {| aFiles | ResolveDrop( "Form_1", HMG_GetFormControls( "Form_1" ), aFiles ) }

      y := 90
      x := nG + 10

      cSay := "FileName(s):"
      @ y, x LABEL Label_0 VALUE cSay WIDTH 150 HEIGHT nHObj

      x2 := ( Form_1.Label_0.COL ) + ( Form_1.label_0.WIDTH ) + 15

      @ 3, x2 - 5 FRAME Frame_1 CAPTION " Drag file(s) here " WIDTH 480 HEIGHT 205 SIZE nFsize - 6

      @ 215, x LABEL Label_P VALUE "" AUTOSIZE SIZE nFsize - 6 BOLD FONTCOLOR BLUE

      DEFINE TBrowse Grid1 AT 22, x2 OF Form_1 ;
            WIDTH 470 ;
            HEIGHT 180 ;
            SIZE nfsize - 3 ;
            ON CHANGE SeeProperty() ;
            NOLINES ;
            HEADERS AFill( Array( 4 ), Space( 150 ) ) ARRAY {}
      END TBROWSE

      Grid1:lDrawheaders := .F.

      Grid1:cToolTip := {| oBr, nCol, nRow | if( ! Empty( Grid1:aArray[ 1, 1 ] ) .AND. nRow > 0 .AND. nrow <= Len( Grid1:aArray ) ;
         , ( dFirst := cFilenoPath( Grid1:aArray[ nrow, 1 ] ) + " " + Chr( 9 ) + Grid1:aArray[ nrow, 2 ] + CRLF + Grid1:aArray[ nrow, 3 ] + Chr( 9 ) + Grid1:aArray[ nrow, 4 ], ;
         +PadL( dFirst, 140, " " ) ), "" ) }

      Y := 290

      @ y-135 , x  Label Lbl_Rpt VALUE "Request the" AUTOSIZE
      @ y-145+nHObj , x  CHECKBOX CK_Rpt CAPTION "error report" WIDTH 140 HEIGHT nHObj TOOLTIP "Request a detailed report of erroneous changes" LEFTJUSTIFY

      cSay := "Created Date:"
      @ y, x LABEL Label_1 VALUE cSay WIDTH 150 HEIGHT nHObj

      x2 := ( Form_1.Label_1.COL ) + ( Form_1.label_1.WIDTH ) + 10

      @ y -nG * 2.5, x2 BUTTON Button_1 CAPTION "Add Files" ;
                        WIDTH nBW HEIGHT nHObj + 10 ; // 35 ;
                        ACTION AddFile()

      @ y -nG * 2.5, x2 + 250 BUTTON Button_2 CAPTION "Clear List" ;
                              WIDTH nWTime HEIGHT nHObj + 10 ;
                              ACTION ( aGrid := {{,,,}} , Grid1:SetArray( aGrid ), OnOff() )

      @ y, x2 DATEPICKER Date_1 VALUE Date() WIDTH nWDate HEIGHT nHObj DATEFORMAT "  dd'.'MMMM' 'yyyy"

      x2 := Form_1.Date_1.COL + Form_1.Date_1.WIDTH + nG

      @ y, x2 TIMEPICKER Time_1 WIDTH nWtime HEIGHT nHObj

      x3 := x2 + Form_1.time_1.WIDTH + nG

      @ y, x3 CHECKBOX CK_1 CAPTION "Change" WIDTH 95 HEIGHT nHObj ;
              ON CHANGE ( Form_1.Date_1.enabled := Form_1.CK_1.VALUE, ;
              Form_1.time_1.enabled := Form_1.CK_1.VALUE, ;
              IF ( Form_1.CK_1.VALUE, Form_1.Button_3.Enabled := .T., NIL ) )

      y += Form_1.Date_1.HEIGHT + nG

      cSay := "Modified Date:"
      @ y, x LABEL Label_2 VALUE cSay WIDTH 150 HEIGHT nHObj

      @ y, ( Form_1.Date_1.COL ) DATEPICKER Date_2 VALUE Date() WIDTH nWDate HEIGHT nHObj ;
                                 DATEFORMAT "  dd'.'MMMM' 'yyyy"

      @ y, x2 TIMEPICKER Time_2 WIDTH nWtime HEIGHT nHObj

      @ y, x3 CHECKBOX CK_2 CAPTION "Change" WIDTH 95 HEIGHT nHObj ;
              ON CHANGE ( Form_1.Date_2.enabled := Form_1.CK_2.VALUE, ;
              Form_1.time_2.enabled := Form_1.CK_2.VALUE, ;
              IF ( Form_1.CK_2.VALUE, Form_1.Button_3.Enabled := .T., NIL ) )

      y += Form_1.Label_2.HEIGHT + nG

      cSay := "Accessed Date:"

      @ y, x LABEL Label_3 VALUE cSay WIDTH 150 HEIGHT nHObj

      @ y, ( Form_1.Date_1.col ) DATEPICKER Date_3 VALUE Date() WIDTH nWDate HEIGHT nHObj ;
                                 DATEFORMAT "  dd'.'MMMM' 'yyyy"

      @ y, x2 TIMEPICKER Time_3 WIDTH nWtime HEIGHT nHObj

      @ y, x3 CHECKBOX CK_3 CAPTION "Change" WIDTH 95 HEIGHT nHObj ;
              ON CHANGE ( Form_1.Date_3.enabled := Form_1.CK_3.VALUE, ;
              Form_1.time_3.enabled := Form_1.CK_3.VALUE, ;
              IF ( Form_1.CK_3.VALUE, Form_1.Button_3.Enabled := .T., NIL ) )

      y += Form_1.Date_3.HEIGHT + nG * 1.5

      nBW := ( Form_1.Date_1.col ) + ( Form_1.Date_1.width ) - x

      @ y, x BUTTON Button_3 CAPTION "Change Files Date" WIDTH nBW HEIGHT nHObj + 10 ACTION ChangeDate()

      @ y, x2 BUTTON Button_4 CAPTION "&Exit" WIDTH nWTime HEIGHT nHObj + 10 ACTION ( Form_1.Release )

      DEFINE CONTEXT MENU CONTROL GRID1
             MENUITEM "Add new file(s)" ACTION AddFile()
             MENUITEM "Remove select file from list" ACTION Removeitem()
      END MENU

      ON KEY F1 ACTION NIL // Disable F1
      ON KEY ESCAPE ACTION ThisWindow.Release

   END WINDOW

   CENTER WINDOW Form_1
   ACTIVATE WINDOW Form_1

RETURN NIL

FUNCTION RemoveItem( argL )

   DEFAULT argl TO .F.

   IF Len( aGrid ) > 1
      IF argL
         aGrid := { {,,,} }
      ELSE
         hb_ADel( aGrid, GRID1:nLogicPos(), .T. )
      ENDIF
   ELSE
      aGrid := { {,,,} }
   ENDIF
   Grid1:SetArray( aGrid )
   OnOff()

RETURN .T.

FUNCTION ONOFF( argl )

   DEFAULT argl TO .F.

   Form_1.Date_1.enabled := .F.
   Form_1.Date_2.enabled := .F.
   Form_1.Date_3.enabled := .F.
   Form_1.Time_1.enabled := .F.
   Form_1.Time_2.enabled := .F.
   Form_1.Time_3.enabled := .F.
   Form_1.CK_1.VALUE := .F.
   Form_1.CK_2.VALUE := .F.
   Form_1.CK_3.VALUE := .F.

   IF Len( aGrid ) < 2 .AND. Empty( Form_1.Grid1.value ) .OR. argl
      Form_1.Button_3.Enabled := argl
      Form_1.CK_1.Enabled := argl
      Form_1.CK_2.Enabled := argl
      Form_1.CK_3.Enabled := argl
      Form_1.Button_2.enabled := argl
   ENDIF
   Form_1.Label_P.VALUE := ""

RETURN NIL

/* Drop Event Processing */
FUNCTION ResolveDrop( cForm, aCtrl, aFiles )

   LOCAL mx, my, ni, tx, ty, bx, by, ct
   LOCAL aRect := { 0, 0, 0, 0 }
   LOCAL aCtlPos := {}
   LOCAL cTarget := ""

   my := GetCursorRow() /* Mouse y position on desktop */
   mx := GetCursorCol() /* Mouse x position on desktop */

   FOR ni := 1 TO Len( aCtrl )
      GetWindowRect( GetControlHandle( aCtrl[ ni ], cForm ), aRect )
      AAdd( aCtlPos, { aCtrl[ ni ], aRect[ 1 ], aRect[ 2 ], aRect[ 3 ], aRect[ 4 ] } )
   NEXT ni

   ni := 0
   DO WHILE ni++ < Len( aCtlPos )
      tx := aCtlPos[ ni, 2 ] /* Top-Left Corner x */
      ty := aCtlPos[ ni, 3 ] /* Top-Left Corner y */
      bx := aCtlPos[ ni, 4 ] /* Right-Bottom Corner x */
      by := aCtlPos[ ni, 5 ] /* Right-Bottom Corner y */
      IF mx >= tx .AND. mx <= bx .AND. my >= ty .AND. my <= by
         cTarget := aCtlPos[ ni, 1 ]
         EXIT
      ENDIF
   ENDDO

   IF Len( cTarget ) > 0
      cTarget := Upper( cTarget )
      ct := GetControlType( cTarget, cForm )
      IF CT == "FRAME"
         IF ValType( aFiles ) == "A"
            AddFile( afiles )
         ENDIF
      ENDIF
   ENDIF

RETURN NIL

FUNCTION CheckList( cArgIn )

   LOCAL aList := AClone( aGrid ), cRtv, lrt := .F.

   AEval( aGrid, {| x | IF ( x[ 1 ] == cArgin, ( cRtv := '', MsgExclamation(x[ 1 ] + CRLF + CRLF + Chr(9 ) + "Is already present!" ) ), NIL ) } )
   IF ISNIL( cRtv )
      cRtv := cArgin
   ENDIF

   IF isDirectory( cArgIn )
      MsgStop( "Operation not allowed on directories:" + CRLF + CRLF + cArgin, "FileDate Changer Error" )
      cRtv := ""
   ENDIF

RETURN cRtv

STATIC FUNCTION AddFile( cFilename )

   DEFAULT cFilename TO ""

   IF Empty( cFilename )
      cFileName := GetFile( { { 'All Files', '*.*' } }, 'Open File(s)', GetCurrentFolder(), .T., .T., "Result" )
   ENDIF
   IF Len( cFileName ) > 0
      IF ATail( agrid )[ 1 ] == NIL
         hb_ADel( aGrid, 1, .T. )
      ENDIF
      AEval( cFileName, {| x, i | cFileName[ i ] := StrTran( x, "\\", "\" ) } )
      AEval( cFileName, {| x | IF ( Empty( CheckList( x ) ), NIL, AAdd( aGRID, { x ;
         , "Creation date " + GETFILEDATETIME( x, .T. ), "Last edit " + GETFILEDATETIME( x,, .T. ), "Last access " + GETFILEDATETIME( x,,, .T. ) } ) ) } )
      Grid1:SetArray( aGrid )
      IF Len( aGRID ) > 0
         Form_1.CK_1.Enabled := .T.
         Form_1.CK_2.Enabled := .T.
         Form_1.CK_3.Enabled := .T.
         Form_1.Button_2.enabled := .T.
         Form_1.Button_3.Enabled := .T.
      ENDIF
   ENDIF

RETURN NIL

STATIC FUNCTION SeeProperty()

   IF Empty( Grid1:GetValue ( 1 ) )
      Form_1.Label_P.VALUE := ""
   ELSE
      Form_1.Label_P.VALUE := Grid1:GetValue ( 2 ) + [   -   ] + Grid1:GetValue ( 3 ) + [   -   ] + Grid1:GetValue( 4 )
   ENDIF

RETURN NIL

STATIC FUNCTION ChangeDate()

   LOCAL aList := AClone( GRID1:aArray ), aSect, n, nF, aRef := { "1", "2", "3" }
   LOCAL aCtnErr := {}, nErr := 0, nAN := 0, cEtitle := ""
   PRIVATE cErrType := ""
   aSect := { { 0, 0, 0 }, { 0, 0, 0 }, { 0, 0, 0 } }

   nF := Len( aList )
   IF nF < 1
      MsgStop( "Add at least one file before using this button !", "Action Rejected" )
      RETURN .F.
   ENDIF

   IF Form_1.CK_1.VALUE
      aSect[ 1 ] := { 1, 0, 0 }
   ENDIF
   IF Form_1.CK_2.VALUE
      aSect[ 2 ] := { 0, 1, 0 }
   ENDIF
   IF Form_1.CK_3.VALUE
      aSect[ 3 ] := { 0, 0, 1 }
   ENDIF

   // check for Selected action
   AEval( ASECT, {| X | IF ( hb_AScan( X, 1 ) > 0, nErr++, NIL ) } )
   IF nErr == 0
      MsgExclamation( "No date type chosen !", "Abort" )
      RETURN NIL
   ENDIF
   nErr := 0

   FOR n := 1 TO nF
      // Execute only the selected changes
      AEval( aRef, {| x, y | IF ( AScan( aSect[ y ], 1 ) = y, if( SetFileDateTime(aList[ n,1 ], DT2A(x ), aSect[ y ] ), NIL, ( nErr++, IF ( hb_AScan( aCtnErr, alist[ n,1 ] ) < 1, AAdd( aCtnErr,alist[ n,1 ] ),NIL ) ) ), NIL ) } )
      nAN := Len( aCtnErr )
      IF nAN > 0 .AND. ! Empty( cErrType )
         aCtnErr[ nAN ] += " [ " + cErrType + " ]"
         cErrType := ""
      ENDIF
   NEXT
   IF nErr = 0
      MsgInfo ( "Successfully changed the dates of file(s)", "FileDate Changer" )
      RemoveItem( .T. )
   ELSE
      cEtitle := "Edit successful but there are errors for:"
      MSGExclamation( AEval( aCtnErr, {| X, Y | aCtnErr[ Y ] += crlf } ), cEtitle )
   ENDIF
   RELEASE cErrType

RETURN NIL

FUNCTION DT2A( cArg1 ) // Date and Time To one Single Array

   LOCAL aRtv

   aRtv := hb_ATokens( hb_StrReplace( hb_DToC( Form_1.&("Date_" + cArg1 ).VALUE, "YYYY,MM,DD" ), { ":" => "," } ), "," )
   AEval( hb_ATokens( hb_StrReplace( Form_1.&("Time_" + cArg1 ).VALUE, { ":" => "," } ), "," ), {| X | AAdd( aRtv, X ) } )
   AEval( aRtv, {| X, Y | aRtv[ Y ] := Val( X ) } )

RETURN aRtv

FUNCTION SetFileDateTime( cFilename, aDateTime, aSet )

   LOCAL nYear, nMonth, nDay, nHour, nMinute, nSecond
   LOCAL cTime, lATimec, lWTime, cTerr
   LOCAL lSuccess, asect

   DEFAULT aSet To { .F., .F., .F. }
   aSect := AClone( aSet )

   // Extract the values from the aDateTime array
   nYear := aDateTime[ 1 ]
   nMonth := aDateTime[ 2 ]
   nDay := aDateTime[ 3 ]
   nHour := aDateTime[ 4 ]
   nMinute := aDateTime[ 5 ]
   nSecond := aDateTime[ 6 ]

   cTime := aSect[ 1 ]
   lWTime := aSect[ 2 ]
   lATimec := aSect[ 3 ]

   // Calling function C to convert and set the date and time
   lSuccess := SetFileTimeC( cFilename, nYear, nMonth, nDay, nHour, nMinute, nSecond, cTime, lATimec, lWTime )
   IF Lsuccess = .F.
      DO CASE
      CASE cTime = 1
         cTerr := "Creation Date of:"
         cErrType += "C"

      CASE lATimec = 1
         cTerr := "Accessed Date of:"
         cErrType += "A"

      CASE lWTime = 1
         cTerr := "Modified Date of:"
         cErrType += "M"

      ENDCASE

      IF Form_1.CK_Rpt.VALUE
         MsgExclamation( "It was not possible to change the " + cTerr + CRLF + CRLF + cFIlename, "FileDate Changer Single Action Error" )
      ENDIF

   ENDIF

RETURN lSuccess

#pragma BEGINDUMP

#include <windows.h>
#include "hbapi.h"

HB_FUNC(SETFILETIMEC) {
    // Retrieves the parameters passed by Harbor
    const char *cFileName = hb_parc(1);
    int nYear   = hb_parni(2);
    int nMonth  = hb_parni(3);
    int nDay    = hb_parni(4);
    int nHour   = hb_parni(5);
    int nMinute = hb_parni(6);
    int nSecond = hb_parni(7);
    int cTime   = hb_parni(8);
    int lATime  = hb_parni(9);
    int lWTime  = hb_parni(10);
    int fresult = 0 ;

    // Prepare the SYSTEMTIME structure with the provided values
    SYSTEMTIME st;
    FILETIME ftLocal, ft;

    st.wYear   = (WORD)nYear;
    st.wMonth  = (WORD)nMonth;
    st.wDay    = (WORD)nDay;
    st.wHour   = (WORD)nHour;
    st.wMinute = (WORD)nMinute;
    st.wSecond = (WORD)nSecond;
    st.wMilliseconds = 0;

    // Convert SYSTEMTIME to FILETIME
    if (SystemTimeToFileTime(&st, &ftLocal)) {
        // Convert local FILETIME to UTC FILETIME
        if (LocalFileTimeToFileTime(&ftLocal, &ft)) {
            // Set the date and time of the file
            HANDLE hFile = CreateFile(cFileName, GENERIC_WRITE, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
            if (hFile != INVALID_HANDLE_VALUE) {
                // SetFileTime("filename.txt", creationTime, lastAccessTime, lastWriteTime);
                if (cTime == 1) {
                   if (!SetFileTime(hFile,  &ft , NULL,  NULL) ) {
                      fresult ++ ;
                   }
                }
                if (lATime == 1) {
                   if (!SetFileTime(hFile, NULL, &ft ,  NULL) ) {
                   fresult ++ ;
                   }
                }
                if (lWTime == 1) {
                   if (!SetFileTime(hFile, NULL,  NULL, &ft ) ) {
                   fresult ++ ;
                   }
                }
                CloseHandle(hFile);
                if (fresult == 0) {
                   hb_retl(HB_TRUE);
                   return;
                }
                CloseHandle(hFile);
            }
        }
    }

    hb_retl(HB_FALSE);
}

HB_FUNC( GETFILEDATETIME )
{
   HANDLE hFile;
   FILETIME Ft;
   SYSTEMTIME St;
   CHAR StrTime[33];

   hFile = CreateFile(hb_parc(1), GENERIC_READ, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
   if (hFile == INVALID_HANDLE_VALUE)
   {
     sprintf(StrTime, "00:00:00.000");
     hb_retc( StrTime );
   }
   else
   {
    if ( hb_parl(2) )                            // voglio la data di creazione
    {
       GetFileTime(hFile,&Ft,NULL,NULL);
    }
    if ( hb_parl(3) )                            // voglio la data di Modifica
    {
       GetFileTime(hFile,NULL,NULL,&Ft );
    }
    if ( hb_parl(4) )                            // voglio la data di scrittura
    {
       GetFileTime(hFile,NULL,&Ft,NULL );
    }
    CloseHandle(hFile);
    FileTimeToLocalFileTime(&Ft,&Ft);
    FileTimeToSystemTime(&Ft,&St);
    sprintf(StrTime, "%02d/%02d/%04d %02d:%02d:%02d",(int) St.wDay,(int) St.wMonth,(int) St.wYear,(int) St.wHour,(int) St.wMinute,(int) St.wSecond ,(int) St.wMilliseconds );
    hb_retc( StrTime );
   }
}

#pragma ENDDUMP
