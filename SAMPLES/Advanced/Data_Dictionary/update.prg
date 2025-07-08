/*

 BadaSystem
 Program       : update
 Modulo        : update.prg
 Compilador    : MINIGUI - Harbour Win32 GUI
 Link          : BCC 32 bit
 Autor         : Marcos Jarrín
 email         : marvijarrin@gmail.com
 website       : badasystem.org
 Date          : 17/04/2021
 Update        : 16/06/2022

*/

#include "minigui.ch"
#include "badasystem.ch"
#include "inkey.ch"
#include "dbstruct.ch"

DECLARE WINDOW Form_PrgBar

STATIC snError := 0
STATIC scFileName := ""

//
// Update table with the new structure that is passed in the parameter paNewStruct
//
FUNCTION badaUpdate( pcOldFile, pcNewFile, paNewStruct )

   LOCAL nRecno := 0
   LOCAL nRecCount := 0
   LOCAL nComplete := 0
   LOCAL aCvtBlks := {}
   LOCAL nTotalRecord := 0
   LOCAL nOrigArea := Select()

   IF ! File( "dataset.dbf" )
      msgbox( "The structures do not exist execute the first option" )
      RETURN NIL
   ENDIF

   snError := 0

   BEGIN SEQUENCE

      IF .NOT. dbUseArea( .T.,, pcOldFile, "OLDFILE" )
         scFileName := pcOldFile
         break
      ENDIF

      dbCreate( pcNewFile, paNewStruct )

      IF .NOT. File( pcNewFile )
         snError := BADASERR_CREATE
         scFileName := pcNewFile
         break
      ENDIF

      IF .NOT. dbUseArea( .T.,, pcNewFile, "NEWFILE", .T. )
         scFileName := pcNewFile
         break
      ENDIF

      aCvtBlks := oldfile->( sCvtBlks( paNewStruct ) )

      oldfile->( ordSetFocus( 0 ) )
      nTotalRecord := oldfile->( LastRec() )
      oldfile->( dbGoTop() )

      CreateProgressBar( "Processing..." )

      WHILE .NOT. oldfile->( Eof() )

         nComplete := Int( ( oldfile->( RecNo() ) / nTotalRecord ) * 100 )
         IF nComplete % 10 == 0
            IF IsWindowDefined( Form_PrgBar )
               Form_PrgBar.PrgBar_1.VALUE := nComplete
               Form_PrgBar.Label_1.VALUE := "Completed " + +hb_ntos( nComplete ) + "%"
            ELSE
               EXIT
            ENDIF
            // refreshing
            INKEYGUI()
         ENDIF

         IF .NOT. oldfile->( sConvert( aCvtBlks ) )
            scFileName := pcNewFile
            EXIT
         ENDIF

         oldfile->( dbSkip() )

      ENDDO

      CloseProgressBar()

   END SEQUENCE

   IF Select( "OLDFILE" ) > 0
      oldfile->( dbCloseArea() )
   ENDIF

   IF Select( "NEWFILE" ) > 0
      newfile->( dbCloseArea() )
   ENDIF

   SELECT ( nOrigArea )

RETURN snError == BADAERR_NONE


//
// append one record from the old database to the new database (convert data
// type as necessary)
//
STATIC FUNCTION sConvert( paCvtBlks )

   LOCAL lSuccess := .F.
   LOCAL nFlds := Len( paCvtBlks )
   LOCAL nI := 0

   BEGIN SEQUENCE

      newfile->( dbAppend() )

      FOR nI := 1 TO nFlds

         IF ValType( paCvtBlks[ nI ] ) == "B"
            newfile->( FieldPut( nI, Eval( paCvtBlks[ nI ] ) ) )
         ENDIF

      NEXT nI

      lSuccess := .T.

   END SEQUENCE

RETURN lSuccess


//
//
//
STATIC FUNCTION sCvtBlks( paNewStruct )

   LOCAL cLeft := "{||"
   LOCAL cRight := "}"

   LOCAL aCvtBlks := {}
   LOCAL aOldStruct := dbStruct()
   LOCAL nFlds := Len( paNewStruct )
   LOCAL nOldPos := 0
   LOCAL cFldName := ""
   LOCAL bCvt := {|| nil }
   LOCAL cBlock := ""
   LOCAL nI := 0

   FOR nI := 1 TO nFlds

      cFldName := upTrim( paNewStruct[ nI, DBS_NAME ] )

      nOldPos := AScan( aOldStruct, {| x | upTrim( x[ DBS_NAME ] ) == cFldName } )

      IF nOldPos = 0
         AAdd( aCvtBlks, nil )
         LOOP
      ENDIF

      cBlock := ""

      DO CASE

      CASE paNewStruct[ nI, DBS_TYPE ] == "C"

         cBlock += "__Cvt2Char( " + ;
            Alias() + "->" + FieldName( nOldPos ) + ", " + ;
            nTrim( paNewStruct[ nI, DBS_LEN ] ) + ")"


      CASE paNewStruct[ nI, DBS_TYPE ] == "M"

         cBlock += "__Cvt2Memo( " + ;
            Alias() + "->" + FieldName( nOldPos ) + ")"


      CASE paNewStruct[ nI, DBS_TYPE ] == "L"

         cBlock += "__Cvt2Log( " + ;
            Alias() + "->" + FieldName( nOldPos ) + ")"


      CASE paNewStruct[ nI, DBS_TYPE ] == "N"

         cBlock += "__Cvt2Nmbr( " + ;
            Alias() + "->" + FieldName( nOldPos ) + ", " + ;
            nTrim( paNewStruct[ nI, DBS_LEN ] ) + "," + ;
            nTrim( paNewStruct[ nI, DBS_DEC ] ) + ")"


      CASE paNewStruct[ nI, DBS_TYPE ] == "D"

         cBlock += "__Cvt2Date( " + ;
            Alias() + "->" + FieldName( nOldPos ) + ")"

      ENDCASE

      bCvt := &( cLeft + cBlock + cRight )

      AAdd( aCvtBlks, bCvt )

   NEXT nI

RETURN aCvtBlks

//
// function for internal use
//
FUNCTION __Cvt2Char( pxOldData, pnMaxLen )

   LOCAL cNewData := ""

   DO CASE

   CASE ValType( pxOldData ) == "L"

      IF pxOldData
         cNewData := "T"
      ELSE
         cNewData := "F"
      ENDIF

   CASE ValType( pxOldData ) == "M"

      cNewData := pxOldData


   CASE ValType( pxOldData ) == "C"

      cNewData := pxOldData


   CASE ValType( pxOldData ) == "N"

      cNewData := AllTrim( Str( pxOldData ) )


   CASE ValType( pxOldData ) == "D"

      cNewData := DToC( pxOldData )

   ENDCASE

RETURN Left( cNewData, pnMaxLen )

//
// function for internal use
//
FUNCTION __Cvt2Memo( pxOldData )

   LOCAL cNewData := ""

   DO CASE

   CASE ValType( pxOldData ) == "L"

      IF pxOldData
         cNewData := "T"
      ELSE
         cNewData := "F"
      ENDIF

   CASE ValType( pxOldData ) == "M"

      cNewData := pxOldData

   CASE ValType( pxOldData ) == "C"

      cNewData := pxOldData

   CASE ValType( pxOldData ) == "N"

      cNewData := AllTrim( Str( pxOldData ) )

   CASE ValType( pxOldData ) == "D"

      cNewData := DToC( pxOldData )

   ENDCASE

RETURN cNewData

//
// function for internal use
//
FUNCTION __Cvt2Log( pxOldData )

   LOCAL lNewData := .F.

   DO CASE

   CASE ValType( pxOldData ) == "L"

      lNewData := pxOldData

   CASE ValType( pxOldData ) $ "MC"

      IF Left( pxOldData, 1 ) $ "Tt"
         lNewData := .T.
      ELSE
         lNewData := .F.
      ENDIF

   CASE ValType( pxOldData ) == "N"

      IF Int( pxOldData ) == 0
         lNewData := .F.
      ELSE
         lNewData := .T.
      ENDIF

   CASE ValType( pxOldData ) == "D"

      IF pxOldData = CToD( "  /  /  " )
         lNewData := .F.
      ELSE
         lNewData := .T.
      ENDIF

   ENDCASE

RETURN lNewData


//
// function for internal use
//
FUNCTION __Cvt2Nmbr( pxOldData, pnLen, pnDec )

   LOCAL nNewData := 0
   LOCAL cPic := ""

   DO CASE

   CASE ValType( pxOldData ) == "L"

      IF pxOldData
         nNewData := 1
      ELSE
         nNewData := 0
      ENDIF

   CASE ValType( pxOldData ) $ "MC"

      nNewData := Val( pxOldData )

   CASE ValType( pxOldData ) == "C"

      nNewData := Val( pxOldData )

   CASE ValType( pxOldData ) == "N"

      nNewData := pxOldData

   CASE ValType( pxOldData ) == "D"

      nNewData := 0

   ENDCASE

   cPic := Replicate( "9", pnDec )

   IF .NOT. Empty( cPic )
      cPic := "." + cPic
   ENDIF

   cPic := PadL( cPic, pnLen, "9" )

   nNewData := Val( Transform( nNewData, cPic ) )

RETURN nNewData


//
// function for internal use
//
FUNCTION __Cvt2Date( pxOldData )

   LOCAL dNewData := .F.

   DO CASE

   CASE ValType( pxOldData ) == "L"

      dNewData := CToD( "  /  /  " )

   CASE ValType( pxOldData ) $ "MC"

      dNewData := CToD( Left( pxOldData, 8 ) )

   CASE ValType( pxOldData ) == "N"

      dNewData := CToD( "  /  /  " )

   CASE ValType( pxOldData ) == "D"

      dNewData := pxOldData

   ENDCASE

RETURN dNewData


//
// Create window with process bar
//
PROCEDURE CreateProgressBar( cTitle )

   DEFINE WINDOW Form_PrgBar ;
         ROW 0 COL 0 ;
         WIDTH 428 HEIGHT 200 ;
         TITLE cTitle ;
         WINDOWTYPE MODAL ;
         NOSIZE ;
         FONT 'Tahoma' SIZE 11

      @ 10, 80 ANIMATEBOX Avi_1 ;
         WIDTH 260 HEIGHT 40 ;
         FILE 'filecopy.avi' ;
         AUTOPLAY TRANSPARENT NOBORDER

      @ 75, 10 LABEL Label_1 ;
         WIDTH 400 HEIGHT 20 ;
         VALUE '' ;
         CENTERALIGN VCENTERALIGN

      @ 105, 20 PROGRESSBAR PrgBar_1 ;
         RANGE 0, 100 ;
         VALUE 0 ;
         WIDTH 380 HEIGHT 34

   END WINDOW

   Form_PrgBar.CENTER
   ACTIVATE WINDOW Form_PrgBar NoWait

RETURN

//
// Close window with process bar
//
PROCEDURE CloseProgressBar()

   IF IsWindowDefined( Form_PrgBar )
      Form_PrgBar.RELEASE
   ENDIF

   DO MESSAGE LOOP

RETURN
