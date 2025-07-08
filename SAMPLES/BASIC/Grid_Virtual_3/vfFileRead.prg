
*******************************************************************
*   vfFileRead Class                                              *
*******************************************************************

#include "minigui.ch"
#include "hbclass.ch"
#include "fileio.ch"

#define vf_DEF_READ_SIZE  	4096
#define vf_DELIMITER		hb_eol()
#define vf_REC_MAP_STEP		100000

CREATE CLASS vfFileRead

   VAR cFile                   
   VAR pHandle                 
   VAR nError                  
   VAR cDelim                  
   VAR nReadSize               
   VAR lEOF
   VAR lBOF
   VAR nCurrLine
   VAR nLastLine
   VAR exGauge
   VAR aRecordsMap INIT {} PROTECTED
   VAR nRecordsMapStep PROTECTED
   
   METHOD New( cFile, nSize, cDelimiter, nRecMapStep )		// Create a new object (file name, buffer size , delimiter, map record indexed step)
   METHOD Open( nMode )								// Open the file ( nMode )
   METHOD Close()									// Close the file
   METHOD ReadLine()								// Read line of current record (when opened, current record is first)
   METHOD GoTop()									// Go to first record
   METHOD GoBottom()								// Go to last record
   METHOD GoTo( nLine )								// Go to <nLine> record
   METHOD Skip( nSkip )								// Skip by <nSkip> records
   METHOD IsEOF()									// Returns .T. if the current record is the last
   METHOD IsBOF()									// Returns .T. if the current record is the first
   METHOD Error()									// Returns .T. an error occurred
   METHOD ErrorNo()									// Returns the error number
   METHOD GetPointer()								// Reads the current position of the file pointer
   METHOD SetPointer( nPointer )						// Sets the file pointer to the indicated position
   METHOD GetLastLinePointer()						// Reads the file pointer position for the last record
   METHOD CurrentLine()								// Reads the current record
   METHOD CountLines( xGauge )						// Calculates the number of lines (records), as a parameter you can pass a block of code executed during the counting operation
   METHOD GetRecordsMap()							// Gets a map of records
   METHOD PutRecordsMap( aMap )						// Puts a map of records
     
END CLASS

*******************************************************************

METHOD New( cFile, nSize, cDelimiter, nRecMapStep ) CLASS vfFileRead

Default nRecMapStep := vf_REC_MAP_STEP

nRecMapStep := Max ( nRecMapStep, 10000 )

IF nSize == NIL .OR. nSize < 1
	nSize := vf_DEF_READ_SIZE
ENDIF

::cFile     := cFile
::pHandle   := NIL
::nError    := 0
::nReadSize := nSize
::cDelim    := cDelimiter
::lEOF	  := .F.
::lBOF	  := .F.
::nCurrLine := 0
::nLastLine := NIL
::aRecordsMap:={}
::nRecordsMapStep := nRecMapStep
   
RETURN Self

*******************************************************************

METHOD Open( nMode ) CLASS vfFileRead

Local cLine
IF ::pHandle == NIL

	IF nMode == NIL
		nMode := FO_READ + FO_SHARED   // Default to shared read-only mode
	ENDIF
	::pHandle := hb_vfOpen( ::cFile, nMode )
	::aRecordsMap:={}
	IF ::pHandle == NIL
		::nError    := FError()       
		::lBOF      := .F.
		::nCurrLine := 0
	ELSE
		::nError    := 0
		::lBOF      := .T.
		::nCurrLine := 1
		
		//delimiter detection
		IF ::cDelim == NIL
			::cDelim := vf_DELIMITER
			cLine := hb_vfReadLen( ::pHandle, 64*1024 /* ::nReadSize */ )
			::GoTop()
			DO CASE
				CASE hb_AT( CHR (13) + CRLF, cLine ) > 0		//CRCRLF
					::cDelim := CHR (13) + CRLF
				CASE hb_AT( CRLF, cLine ) > 0					//CRLF
					::cDelim := CRLF
				CASE hb_AT( CHR (13), cLine ) > 0				//CR
					::cDelim := CHR (13)
				CASE hb_AT( CHR (10), cLine ) > 0				//LF
					::cDelim := CHR (10)
			ENDCASE
		ENDIF                      

	ENDIF
ELSE
      // The file is already open, so rewind to the beginning.
	IF !::GoTop()
      	::lBOF      := .F.
      	::nCurrLine := 0
      	::nError    := FError()
	ELSE
		::lBOF      := .T.
		::nCurrLine := 1
	ENDIF
ENDIF
   
RETURN Self

*******************************************************************

METHOD Close() CLASS vfFileRead

IF .NOT. ::pHandle == NIL
	hb_vfClose( ::pHandle )
	::pHandle := NIL           // The file is no longer open
ENDIF
::nError    := FError()
::lEOF      := .F.
::lBOF      := .F.
::nCurrLine := 0
::nLastLine := NIL

RETURN Self

*******************************************************************

METHOD ReadLine() CLASS vfFileRead

Local cLine       := ""
Local nCurrentPos := ::GetPointer()

DO WHILE !hb_vfEof( ::pHandle )
	cLine += hb_vfReadLen( ::pHandle, ::nReadSize )
	IF hb_AT( ::cDelim, cLine ) > 0
		cLine := Left ( cLine, hb_AT( ::cDelim, cLine ) - 1 )
		EXIT
	ENDIF
ENDDO

::SetPointer ( nCurrentPos )
::nError := FError()

RETURN cLine

*******************************************************************

METHOD GoTop() CLASS vfFileRead

::lEOF      := .F.
::lBOT      := .T.
::nCurrLine := 1

RETURN hb_vfSeek( ::pHandle, 0, 0 ) == 0

*******************************************************************

METHOD GoBottom() CLASS vfFileRead

hb_vfSeek( ::pHandle, 0, 2 )
::Skip( -1 )
::lEOF      := .T.
::lBOT      := .F.
::nCurrLine := ::nLastLine
::nError    := FError()
      
RETURN FError() == 0   

*******************************************************************

METHOD GoTo( nLine ) CLASS vfFileRead

Local nToSkip ,nIntStep, nPos

DEFAULT nLine := ::nCurrLine

IF !hb_isNumeric (nLine)
	RETURN Self
ENDIF

IF ABS ( nLine - ::nCurrLine ) >= ::nRecordsMapStep		//only if we skip by indexed steps
	nIntStep := ( INT( nLine / ::nRecordsMapStep ) + IF ( nLine % ::nRecordsMapStep < (::nRecordsMapStep / 2), 0, 1 ) ) * ::nRecordsMapStep
	DO WHILE nIntStep >= 0
		nPos := hb_AScan( ::aRecordsMap, { |x|  Bin2ULL ( ULLUnShrink ( x [ 1 ] ) ) == nIntStep } )
		IF nPos > 0
			::nCurrLine := Bin2ULL ( ULLUnShrink ( ::aRecordsMap [ nPos ] [ 1 ] ) )
			::SetPointer( Bin2ULL ( ULLUnShrink ( ::aRecordsMap [ nPos ] [ 2 ] ) ) )
			EXIT
		ENDIF
		//Try with previous indexed step
		nIntStep -= ::nRecordsMapStep
	ENDDO
ENDIF

nToSkip := nLine - ::nCurrLine

DO CASE
	CASE hb_isNil( ::nCurrLine )			//We do not know the current line number, so start at the beginning of the file
		::GoTop()
		::Skip ( nLine - 1 )
	CASE nLine <= 1					//Line number less than 1, so start at the beginning of the file
		::GoTop()
	CASE hb_isNumeric( ::nLastLine ) .AND. nLine >= ::nLastLine		//Go beyond the known last line number, so go to the end.
		::GoBottom()
	CASE hb_isNumeric( ::nLastLine ) .AND. ::nLastLine - nLine < ABS ( nToSkip ) 	//We know the number of the last record, the nToSkip is greater than the difference between the last and the target record - so skip backward from the end (less skips)
		::GoBottom()
		::Skip( 0 - (::nLastLine - nLine ) )
	CASE nLine < 0 - nToSkip				//The target line is closer to the start than the nToSkip, so start at the beginning of the file.
		::GoTop()
		::Skip ( nLine - 1 )
	OTHER							
		::Skip ( nToSkip )             
ENDCASE
::nError := FError()

RETURN Self

*******************************************************************

METHOD Skip ( nSkip ) CLASS vfFileRead

Local cLine     := ""
Local lRun      := .T.
Local nSkips    := 0
Local nBuffSize := 64 * 1024	//::nReadSize  //A larger buffer speeds up the process
Local nLenDelim := Len (::cDelim)
Local nStart, nEnd, nBackOf, nPos, nPointer
Default nSkip   := 1

IF nSkip == 0
	RETURN Self
ENDIF

::lEOF := .F.
::lBOF := .F.

DO WHILE lRun
                                                            
	IF nSkip > 0
		IF hb_vfEof( ::pHandle )
			::Skip ( -1 )
			::nLastLine := ::nCurrLine
			::lEOF      := .T.
			EXIT
		ENDIF
		nStart   := 1
		nPointer := ::GetPointer()
		cLine    := hb_vfReadLen( ::pHandle, nBuffSize )
		DO WHILE Right (cLine, 1) $ ::cDelim .AND. !hb_vfEof( ::pHandle )		//Avoid splitting of end-of-line marks in the buffer.
			cLine += hb_vfReadLen( ::pHandle, nLenDelim )
		ENDDO
		DO WHILE ( nPos := hb_AT( ::cDelim, cLine, nStart ) ) > 0
			nSkips ++
			IF hb_isNumeric( ::nCurrLine )
				::nCurrLine ++
				IF ::nCurrLine % ::nRecordsMapStep == 0 .AND. hb_AScan( ::aRecordsMap, { |x|  Bin2ULL ( ULLUnShrink ( x [ 1 ] ) ) == ::nCurrLine } ) == 0
					AADD( ::aRecordsMap, { ULLShrink( ULL2Bin( ::nCurrLine ) ), ULLShrink( ULL2Bin( nPointer + nPos  + nLenDelim - 1 ) ) } )
				ENDIF
				
			ENDIF
			nStart := nPos + nLenDelim
			IF nSkips == nSkip
				hb_vfSeek( ::pHandle, 0 - ( Len ( cLine ) - nStart + 1), 1 )
				lRun := .F.
				IF hb_vfEof( ::pHandle )
					::Skip ( -1 )
					::nLastLine := ::nCurrLine
					::lEOF      := .T.
				ENDIF
				EXIT
			ENDIF
		ENDDO
	ELSE			// -skip
	
		IF ::GetPointer() == 0		//top of file
			::lBOF      := .T.
			::nCurrLine := 1
			EXIT
		ENDIF
		cLine := ""
		
		DO WHILE ( nPos := hb_AT( ::cDelim, cLine ) ) == 0 .AND. hb_vfSeek( ::pHandle, 0, 1 ) <> 0		//Avoid splitting of end-of-line marks in the buffer.
			nBackOf  := Min( nBuffSize, ::GetPointer() )
			hb_vfSeek( ::pHandle, 0 - nBackOf, 1 )
			cLine    := hb_vfReadLen( ::pHandle, nBackOf ) + cLine
			hb_vfSeek( ::pHandle, 0 - nBackOf, 1 )
		ENDDO
		IF nPos == 0
			nPos := 1
		ENDIF
		
		nStart := nPos
		
		cLine  := SubStr ( cLine, nStart )
		hb_vfSeek( ::pHandle, nStart - 1, 1 )
		
		nPointer := ::GetPointer()
		nEnd     := Len( cLine )
		
		IF Right( cLine, nLenDelim) == ::cDelim
			nEnd -= nLenDelim
		ENDIF
	
		DO WHILE (nPos := hb_RAT( ::cDelim, cLine, 1, nEnd ) ) > 0
			nSkips --
			IF hb_isNumeric( ::nCurrLine )
				::nCurrLine --
				IF ::nCurrLine % ::nRecordsMapStep == 0 .AND. hb_AScan( ::aRecordsMap, { |x|  Bin2ULL ( ULLUnShrink ( x [ 1 ] ) ) == ::nCurrLine } ) == 0
					AADD( ::aRecordsMap, { ULLShrink( ULL2Bin( ::nCurrLine ) ), ULLShrink( ULL2Bin( nPointer + nPos + nLenDelim -1 ) ) } )
				ENDIF
			ENDIF
			
			nEnd := nPos - 1		//nLenDelim

			IF nSkips == nSkip
				hb_vfSeek( ::pHandle, 0 + nPos + nLenDelim - 1 , 1 )
				lRun := .F.
				EXIT
			ENDIF
		ENDDO
	ENDIF
ENDDO

cLine := ''
::nError := FError()

RETURN Self

*******************************************************************

METHOD IsEOF() CLASS vfFileRead

IF !::lEOF .AND. hb_IsNumeric ( ::nCurrLine ) .AND. hb_IsNumeric ( ::nLastLine ) .AND. ::nCurrLine == ::nLastLine
	::lEOF := .T.
ENDIF

IF !::lEOF .AND. ::GetPointer() == ::GetLastLinePointer()
	::lEOF := .T.
ENDIF

RETURN hb_vfEof( ::pHandle ) .OR. ::lEOF

*******************************************************************

METHOD IsBOF() CLASS vfFileRead

RETURN ::GetPointer() == 0 .OR. ::lBOF

*******************************************************************

METHOD Error() CLASS vfFileRead

RETURN FError() != 0

*******************************************************************

METHOD ErrorNo() CLASS vfFileRead

RETURN ::nError

*******************************************************************

METHOD GetPointer() CLASS vfFileRead
	
RETURN hb_vfSeek( ::pHandle, 0, 1 )

*******************************************************************

METHOD SetPointer( nPointer ) CLASS vfFileRead

hb_vfSeek( ::pHandle, nPointer, 0 )
::nError := FError()

RETURN ::nError == 0

*******************************************************************

METHOD GetLastLinePointer() CLASS vfFileRead

Local nCurrentPointer := ::GetPointer()
Local nCurrentLine    := ::nCurrLine
Local lCurrentEOF     := ::lEOF
Local nLastLinePointer
::GoBottom()
nLastLinePointer := ::GetPointer()
::SetPointer ( nCurrentPointer )	//roll back pointer
::nCurrLine := nCurrentLine			//roll back ::nCurrLine
::lEOF := lCurrentEOF				//roll back ::lEOF
::nError := FError()

RETURN nLastLinePointer

*******************************************************************

METHOD CurrentLine() CLASS vfFileRead

RETURN ::nCurrLine

*******************************************************************

METHOD CountLines( xGauge ) CLASS vfFileRead

Local cLine       := ""
Local nCurrentPos := ::GetPointer()
Local nLastPos    := hb_vfSeek( ::pHandle, 0, 2 )
Local nSkips      := 0
Local nlDelim     := Len ( ::cDelim )
Local nPrevPos    := 0
Local nBuffSize   := 64 * 1024
Local exCurrentGauge := ::exGauge
Local nPointer    := 0
Local nStart, nPos

IF HB_ISEVALITEM( xGauge )
	::exGauge := xGauge
ENDIF

::aRecordsMap := {}
::GoTop()

DO WHILE !hb_vfEof( ::pHandle )
	nStart   := 1
	nPointer := ::GetPointer()
	cLine    := hb_vfReadLen( ::pHandle, nBuffSize )
	DO WHILE Right (cLine, 1) $ ::cDelim .AND. !hb_vfEof( ::pHandle )	//Avoid splitting of end-of-line marks in the buffer.
		cLine += hb_vfReadLen( ::pHandle, nlDelim )
	ENDDO
	DO WHILE (nPos := hb_AT( ::cDelim, cLine, nStart )) > 0
		nSkips ++
		IF nSkips % ::nRecordsMapStep == 0
			AADD( ::aRecordsMap, { ULLShrink( ULL2Bin( nSkips ) ), ULLShrink( ULL2Bin( nPointer + nPrevPos - 1 /* pointer starts from 0 not from 1 like AT( ) */ ) ) } )
			
			IF HB_ISEVALITEM( ::exGauge )
				Eval( ::exGauge, nPointer + nPrevPos - 1, nLastPos, nSkips , 'Processing', Self )
			ENDIF
		
		ENDIF
			
		nStart   := nPos + nlDelim
		nPrevPos := nStart

	ENDDO
ENDDO

IF .NOT. Right( cLine, nlDelim ) == ::cDelim .AND. nLastPos > 0		//The last line does not end with a delimiter. Make it the last record.
	nSkips ++
ENDIF

IF HB_ISEVALITEM( ::exGauge )
	Eval( ::exGauge, nPointer + nPrevPos - 1, nLastPos, nSkips , 'Done', Self )
ENDIF

::nLastLine := nSkips
::SetPointer ( nCurrentPos )
::nError    := FError()
::exGauge   := exCurrentGauge

cLine := ""

RETURN nSkips

*******************************************************************

METHOD GetRecordsMap() CLASS vfFileRead

RETURN ::aRecordsMap

*******************************************************************

METHOD PutRecordsMap( aMap ) CLASS vfFileRead

IF aMap == NIL .OR. !hb_isArray( aMap )
	RETURN Self
ENDIF
::aRecordsMap := aMap

RETURN Self 

*******************************************************************

Function ULLShrink( cBin )
RETURN REMRIGHT( cBin, CHR(0) )

*******************************************************************

Function ULLUnShrink( cBin )
RETURN PADRIGHT( cBin, 8, CHR(0) )

*******************************************************************  

//Support Unsigned Long Long
#pragma BEGINDUMP   
 

#include "hbapi.h"
#include "hbapiitm.h"

HB_FUNC( BIN2ULL )
{
   PHB_ITEM pItem    = hb_param( 1, HB_IT_STRING );
   HB_U64   uiResult = 0;

   if( pItem )
   {
      HB_SIZE nLen = hb_itemGetCLen( pItem );
      if( nLen )
      {
         const char * pszString = hb_itemGetCPtr( pItem );
         uiResult = HB_GET_LE_UINT64( pszString );
          }
   }
   hb_retnint( uiResult );
}

HB_FUNC( ULL2BIN )
{
   char   szResult[ 8 ];
   HB_U64 uiValue = ( HB_U64 ) hb_parnint( 1 );

   HB_PUT_LE_UINT64( szResult, uiValue );
   hb_retclen( szResult, 8 );
}

#pragma ENDDUMP

************************ THE END *********************