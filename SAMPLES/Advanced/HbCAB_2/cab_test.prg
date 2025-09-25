#include "minigui.ch"

#define CAB_FILE_NAME "cabtest1.cab"

STATIC aLog
STATIC cs

FUNCTION Main()

   DEFINE WINDOW Form_1 ;
         TITLE "CAB Compression & Extraction" ;
         WIDTH 535 ;
         HEIGHT 500 ;
         MAIN ;
         ON RELEASE CleanUp()

      @ 10, 20 FRAME Group1 ;
         WIDTH 480 ;
         HEIGHT 345 ;
         CAPTION "CAB Creation, Info && Extraction"

      @ 30, 30 BUTTON BtnStart ;
         CAPTION "Create CAB File" ;
         WIDTH 140 ;
         ACTION DoCreateCab()

      @ 30, 190 BUTTON BtnInfo ;
         CAPTION "Show CAB Info" ;
         WIDTH 140 ;
         ACTION ShowCabInfo()

      @ 30, 340 BUTTON BtnExtract ;
         CAPTION "Extract CAB File" ;
         WIDTH 140 ;
         ACTION DoExtractCab()

      @ 80, 30 LISTBOX lbLog ;
         WIDTH 460 ;
         HEIGHT 260 ;
         ITEMS aLog ;
         FONT "Courier New" SIZE 9

      @ 360, 20 LABEL lblStatus ;
         WIDTH 460 ;
         HEIGHT 30 ;
         VALUE "Status: Ready" ;
         FONT "Arial" SIZE 9

   END WINDOW

   CENTER WINDOW Form_1
   ACTIVATE WINDOW Form_1

RETURN NIL

// ---------------------------
// Create CAB from sample files
// ---------------------------
FUNCTION DoCreateCab()

   LOCAL aFile := { "cabtest1.txt", "cabtest2.txt", "cabtest3.txt", "cabtest4.txt" }
   LOCAL cResult, bCallBack, i

   // Create dummy files
   FOR i := 1 TO Len( aFile )
      IF ! File( aFile[ i ] )
         MemoWrit( aFile[ i ], "Dummy content for " + aFile[ i ] )
      ENDIF
   NEXT

   cs := { 0, 0 }
   aLog := {}
   DoMethod( "Form_1", "lbLog", "SETARRAY", aLog )

   bCallBack := {| p1, p2, p3, p4, p5 | ;
      IF( p5 == 1, FILE_PLACED( p1, p2, p3, p4 ), STATUS( p1, p2, p3 ) ) }

   cResult := HB_CREATECAB( ;
      aFile, ;
      "CABTEST", ;
      NIL, ;
      500000, ;
      1000000, ;
      bCallBack, ;
      54321, ;
      "xHarbour" )

   Form_1.lblStatus.VALUE := "Status: " + cResult

   IF cResult != "OK"
      MsgStop( "CAB creation failed: " + cResult )
   ELSE
      MsgInfo( "CAB file created: " + CAB_FILE_NAME )
   ENDIF

RETURN NIL

// ---------------------------
// Show metadata from CAB file
// ---------------------------
FUNCTION ShowCabInfo()

   LOCAL cText
   LOCAL aInfo := HB_CABINFO( CAB_FILE_NAME )

   IF ValType( aInfo ) == "A" .AND. Len( aInfo ) == 9
      cText := "CAB File Info:" + CRLF
      cText += "File Name ..........: " + aInfo[ 1 ] + CRLF
      cText += "Size (bytes) .......: " + AllTrim( Str( aInfo[ 2 ] ) ) + CRLF
      cText += "Number of Folders ..: " + AllTrim( Str( aInfo[ 3 ] ) ) + CRLF
      cText += "Number of Files ....: " + AllTrim( Str( aInfo[ 4 ] ) ) + CRLF
      cText += "Cabinet Set ID .....: " + AllTrim( Str( aInfo[ 5 ] ) ) + CRLF
      cText += "Cabinet Number .....: " + AllTrim( Str( aInfo[ 6 ] ) ) + CRLF
      cText += "Has RESERVE area ...: " + IF( aInfo[ 7 ] == 1, "Yes", "No" ) + CRLF
      cText += "Chained to Prev ....: " + IF( aInfo[ 8 ] == 1, "Yes", "No" ) + CRLF
      cText += "Chained to Next ....: " + IF( aInfo[ 9 ] == 1, "Yes", "No" ) + CRLF

      MsgInfo( cText, "CAB Info" )
   ELSE
      MsgStop( "Failed to read CAB info." )
   ENDIF

RETURN NIL

// ---------------------------
// Extract CAB using callback
// ---------------------------
FUNCTION DoExtractCab()

   LOCAL cResult

   aLog := {}
   AAdd( aLog, "Starting extraction..." )
   DoMethod( "Form_1", "lbLog", "SETARRAY", aLog )

   IF File( CAB_FILE_NAME )
      hb_DirBuild( "Extracted\" )
   ENDIF

   cResult := HB_DECOMPRESSCAB( CAB_FILE_NAME, "Extracted\", {| a, b, c, d, e, f, g | MYCALLBACK( a, b, c, d, e, f, g ) } )

   AAdd( aLog, "Extraction Result: " + cResult )
   DoMethod( "Form_1", "lbLog", "SETARRAY", aLog )

   Form_1.lblStatus.VALUE := "Extract: " + cResult

   IF cResult == "OK"
      MsgInfo( "Extraction completed to Extracted\" )
   ELSE
      MsgStop( "Extraction failed: " + cResult )
   ENDIF

RETURN NIL

// ---------------------------
// Called when file is added
// ---------------------------
STATIC FUNCTION FILE_PLACED( pCab, pFile, pSize, pCont )

   LOCAL cLog := "Placed: " + PadR( pFile, 20 ) + " (" + AllTrim( Str( pSize ) ) + ") in " + pCab

   IF pCont
      cLog += " (continued)"
   ENDIF

   AAdd( aLog, cLog )
   DoMethod( "Form_1", "lbLog", "SETARRAY", aLog )

RETURN 0

// ---------------------------
// Called during compression
// ---------------------------
STATIC FUNCTION STATUS( nStatus, nComp, nUncomp )

   LOCAL nPerc
   LOCAL cLog

   IF nStatus == 0
      cs[ 1 ] += nComp
      cs[ 2 ] += nUncomp
      cLog := "Compressing: " + hb_ntos( cs[ 1 ] ) + " / " + hb_ntos( cs[ 2 ] )
   ELSEIF nStatus == 1
      nPerc := get_percentage( nComp, nUncomp )
      cLog := "Packing to CAB: " + hb_ntos( nPerc ) + "%"
   ENDIF

   AAdd( aLog, cLog )
   DoMethod( "Form_1", "lbLog", "SETARRAY", aLog )

RETURN 0

// ---------------------------
// Helper function
// ---------------------------
STATIC FUNCTION get_percentage( a, b )
RETURN iif( b == 0, 0, ( a * 100 ) / b )

/* State of extracting process for callback function */
#define CABINET_INFO    0
#define PARTIAL_FILE    1
#define COPY_FILE       2
#define CLOSE_FILE_INFO 3
#define NEXT_CABINET    4

// ---------------------------
// User-defined decompression callback
// ---------------------------
STATIC FUNCTION MYCALLBACK( ;
      nNotificationType, ;
      cNextCabinet, ;
      cNextDisk, ;
      cCabinetPath, ;
      nCabinetID, ;
      nNoCabinet, ;
      nUncompressedFileSize )

   LOCAL cLog := ""

   DO CASE
   CASE nNotificationType == CABINET_INFO
      cLog := "CAB Info: ID=" + hb_ntos( nCabinetID ) + ", Cab#=" + hb_ntos( nNoCabinet )
   CASE nNotificationType == PARTIAL_FILE
      cLog := "Partial File: " + cNextCabinet + " starts in " + cNextDisk
   CASE nNotificationType == COPY_FILE
      cLog := "Copying: " + cNextCabinet + " (" + hb_ntos( nUncompressedFileSize ) + ")"
   CASE nNotificationType == CLOSE_FILE_INFO
      cLog := "Close File: " + cNextCabinet
   CASE nNotificationType == NEXT_CABINET
      cLog := "Next CAB: " + cNextCabinet + " on disk " + cNextDisk
   ENDCASE

   AAdd( aLog, cLog )
   DoMethod( "Form_1", "lbLog", "SETARRAY", aLog )

RETURN 0

// ---------------------------
// Removes test files
// ---------------------------
STATIC PROCEDURE CleanUp()

   LOCAL aFiles := { "cabtest1.txt", "cabtest2.txt", "cabtest3.txt", "cabtest4.txt" }
   AEval( aFiles, {| c | hb_FileDelete( c ) } )

RETURN
