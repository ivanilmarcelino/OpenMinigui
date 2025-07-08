/*
---------------------------------------------------------------------------¬
¦        Class: RichText                                                   ¦
¦  Description: System for generating RTF output files                     ¦
¦     Language: Clipper/Fivewin                                            ¦
¦      Version: 1.00 -- This is a usable, but abridged, version.           ¦
¦               A more capable version will be made available for a        ¦
¦               reasonable cost.                                           ¦
¦         Date: 10/24/97                                                   ¦
¦       Author: Tom Marchione                                              ¦
¦     Internet: tmarchione@compuserve.com                                  ¦
¦                                                                          ¦
¦    Copyright: (C) 1997, Thomas R. Marchione                              ¦
¦                                                                          ¦
¦   Warranties: None. The code has not been rigorously tested in a formal  ¦
¦               development environment, and is offered as-is.  The author ¦
¦               assumes no responsibility for its use, or for any          ¦
¦               consequences that may arise from its use.                  ¦
¦                                                                          ¦
¦    Revisions:                                                            ¦
¦                                                                          ¦
¦    DATE       AUTHOR  COMMENTS                                           ¦
¦--------------------------------------------------------------------------¦
¦    01/28/97   TRM     Date of initial release                            ¦
¦    02/03/97   TRM     Added first cut at RTFMerge() function             ¦
¦    02/05/97   TRM     Added RTFHeader() and IsRTF() functions            ¦
¦    02/05/97   TRM     Released Version 0.91                              ¦
¦    07/09/97   TRM     Tweaked handling of CHR(13) and CHR(9) in ::Write()¦
¦    07/09/97   TRM     Added Table of Contents support                    ¦
¦    07/09/97   TRM     Added simple page numbering support                ¦
¦    10/08/97   TRM     Added support for optional file checks             ¦
¦                                                                          ¦
L---------------------------------------------------------------------------
*/

#include "hmg.ch"
#include "hbclass.ch"
#include "Richtext.ch"

CLASS RichText

	DATA cFileName
	DATA hFile
	DATA nFontSize
	DATA aTranslate
	DATA nFontNum
	DATA nScale
	DATA lTrimSpaces

	// Table Management
	DATA cTblHAlign, nTblFntNum, nTblFntSize, nTblRows, nTblColumns
	DATA nTblRHgt, aTableCWid, cRowBorder, cCellBorder, nCellPct
	DATA lTblNoSplit, nTblHdRows, nTblHdHgt, nTblHdPct, nTblHdFont
	DATA nTblHdFSize
	DATA cCellAppear, cHeadAppear
	DATA cCellHAlign, cHeadHAlign
	DATA nCurrRow, nCurrColumn


	// Methods for opening & closing output file, and setting defaults
	METHOD New( cFileName, aFontData, nFontSize, nScale, aHigh, ;
				lWarn, lGetFile, cPath ) CONSTRUCTOR
	METHOD End() INLINE ::TextCode( "par\pard" ), ::CloseGroup(), FCLOSE(::hFile)


	// Core methods for writing control codes & data to the output file
	METHOD TextCode( cCode ) INLINE FWRITE(::hFile, FormatCode(cCode) )
	METHOD NumCode( cCode, nValue, lScale )
	METHOD LogicCode( cCode, lTest )
	METHOD Write( xData, lCodesOK )


	// Groups and Sections (basic RTF structures)
	METHOD OpenGroup() INLINE FWRITE( ::hFile, "{" )
	METHOD CloseGroup() INLINE FWRITE( ::hFile, "}" )

	METHOD NewSection( lLandscape, nColumns, nLeft, nRight, nTop, nBottom, ;
				nWidth, nHeight, cVertAlign, lDefault )


	// Higher-level page setup methods
	METHOD PageSetup( nLeft, nRight, nTop, nBottom, nWidth, nHeight, ;
				nTabWidth, lLandscape, cVertAlign, cPgNumPos, lPgNumTop, ;
				lNoWidow )

	METHOD BeginHeader() INLINE ::OpenGroup(), ::TextCode("header \pard")
	METHOD EndHeader() INLINE ::TextCode("par"), ::CloseGroup()
	METHOD BeginFooter() INLINE ::OpenGroup(), ::TextCode("footer \pard")
	METHOD EndFooter() INLINE ::TextCode("par"), ::CloseGroup()

	METHOD Paragraph( cText, nFontNumber, nFontSize, cAppear, ;
				cHorzAlign, aTabPos, nIndent, nFIndent, nRIndent, nSpace, ;
				lSpExact, nBefore, nAfter, lNoWidow, lBreak, ;
				lBullet, cBulletChar, lHang, lDefault, lNoPar, nTCLevel )


	// Table Management
	METHOD DefineTable( cTblHAlign, nTblFntNum, nTblFntSize, ;
		cCellAppear, cCellHAlign, nTblRows, ;
		nTblColumns, nTblRHgt, aTableCWid, cRowBorder, cCellBorder, nCellPct, ;
		lTblNoSplit, nTblHdRows, nTblHdHgt, nTblHdPct, nTblHdFont, ;
		nTblHdFSize, cHeadAppear, cHeadHAlign )

	METHOD BeginRow() INLINE ::TextCode( "trowd" ), ::nCurrRow += 1
	METHOD EndRow()   INLINE ::TextCode( "row" )

	METHOD WriteCell( cText, nFontNumber, nFontSize, cAppear, cHorzAlign, ;
				nSpace, lSpExact, cCellBorder, nCellPct, lDefault, ;
				lMrgColumns, nMrgColumns )


	// Methods for formatting data

	METHOD Appearance( cAppear )
	METHOD HAlignment( cAlign )
	METHOD LineSpacing( nSpace, lSpExact )
	METHOD Borders( cEntity, cBorder )
	METHOD NewFont( nFontNumber )
	METHOD SetFontSize( nFontSize )
	METHOD NewLine() INLINE FWRITE(::hFile, CRLF), ::TextCode( "par")
	METHOD NewPage() INLINE ::TextCode( "page" + CRLF )
	METHOD PageNumber( cHorzAlign ) // 7/9/97

	// General service methods

	METHOD BorderCode( cBorderID )


	// Miscellaneous functions

	// FUNCTION RTFHeader( cFile )
	// FUNCTION IsRTF( cFile )
	// FUNCTION RTFMerge( cPriFile, nSkip, xRTF, cClipDelims )
	// FUNCTION IntlTranslate()
	// FUNCTION NewBase( nDec, nBase )

ENDCLASS




METHOD New( cFileName, aFontData, nFontSize, nScale, aHigh, ;
		lWarn, lGetFile, cPath ) CLASS RichText
*********************************************************************
* Description:  Initialize a new RTF object, and create an associated
*               file, with a valid RTF header.
*
* Arguments:    
*               
* Return:       
*               
*--------------------------------------------------------------------
* Date       Developer   Comments
* 01/06/97   TRM         Creation
* 10/08/97   TRM         Added optional check for file existence
*********************************************************************
LOCAL i, lOK := .T.
LOCAL cTopFile  := "rtf1\ansi\deff0\deftab720"
LOCAL cColors   := "colortbl\red0\green0\blue0;"
LOCAL cAuthor   := "info\author RichText() Class for Clipper/Fivewin"
LOCAL cGetTitle := "Enter a File Name for the Report"

DEFAULT ;
	cFileName := "REPORT.RTF", ;
	aFontData := { "Courier New" }, ;
	nFontSize := 12, ; // 10/8/97 -- Typo! Previously was not part of DEFAULT
	nScale    := INCH_TO_TWIP, ;
	lWarn     := .F., ;
	lGetFile  := .F.

::cFileName := cFileName
::nFontSize := nFontSize
::nScale    := nScale
::hFile     := -2 // 10/8/97

::lTrimSpaces := .F.

IF VALTYPE(aHigh) == "A"
	::aTranslate := aHigh
ENDIF

// 10/8/97
// Optionally allow the user to choose a file name,
// and force a specific path.

IF lGetFile
	::cFileName := GetFile( , ;
		cGetTitle, cFilePath(::cFileName),, .F. ;
		)
ENDIF


IF !EMPTY( ::cFileName )

	IF !EMPTY(cPath)
		::cFileName := cPath + cFileNoPath(::cFileName)
	ENDIF

	// If no extension specified in file name, use ".RTF"
	IF !("." $ ::cFileName)
	   ::cFileName += ".RTF"
	ENDIF   

	IF lWarn .AND. FILE( ::cFileName )
		lOK := MsgYesNo( "File " + ::cFileName + " already exists.  Overwrite?", ;
					"File Exists" )
	ENDIF

	IF lOK
		// Create/open a file for writing
		::hFile := FCREATE(::cFileName)
	ENDIF

ENDIF


IF ::hFile >= 0

	// Generate RTF file header

	// This opens the top-most level group for the report
	// This group must be explicitly closed by the application!

	::OpenGroup()

		::TextCode( cTopFile )

		// Generate a font table, and write it to the header
		::nFontNum := LEN(aFontData)
		::OpenGroup()
		::TextCode( "fonttbl" )
		FOR i := 1 TO ::nFontNum
			::OpenGroup()
			::NewFont( i )
			::TextCode( "fnil" )
			::Write( aFontData[i] + ";" )
			::CloseGroup()
		NEXT
		::CloseGroup()

		// Use default color info, for now...
		::OpenGroup()
			::TextCode( cColors )
		::CloseGroup()

		// 10/25/97 -- Add file author info
		::OpenGroup()
			::TextCode( cAuthor )
		::CloseGroup()


	// NOTE:  At this point, we have an open group (the report itself)
	// that must be closed at the end of the report.

ENDIF

RETURN Self
**************************  END OF New()  ***************************






METHOD PageSetup( nLeft, nRight, nTop, nBottom, nWidth, nHeight, ;
				nTabWidth, lLandscape, cVertAlign, ;
				cPgNumPos, lPgNumTop, lNoWidow ) CLASS RichText
*********************************************************************
* Description:  Define default page setup info for file
*               This information is placed in the "document formatting
*               group" of the RTF file, except for vertical alignment,
*               which, if supplied, is treated as a new section.
* Arguments:    
* Return:       
*               
*--------------------------------------------------------------------
* Date       Developer   Comments
* 01/06/97   TRM         Creation
*
*********************************************************************

DEFAULT lLandscape := .F.
DEFAULT lNoWidow := .F.
DEFAULT cPgNumPos := "BOTTOM"
DEFAULT lPgNumTop := .F.


// Note -- "landscape" should not be specified here if landscape and
// portrait orientations are to be mixed.  If "landscape' is specified,
// the paper width and height should also be specified, and consistent
// (i.e., with landscape/letter, width==11 and height==8.5)

::LogicCode( "landscape", lLandscape )
::NumCode( "paperw", nWidth )
::NumCode( "paperh", nHeight )

::LogicCode( "widowctrl", lNoWidow )
::NumCode( "margl", nLeft )
::NumCode( "margr", nRight )
::NumCode( "margt", nTop )
::NumCode( "margb", nBottom )
::NumCode( "deftab", nTabWidth )


// Vertical alignment and page number position are "section-specific"
// codes.  But we'll put them here anyway for now...

IF !EMPTY( cVertAlign )
	::TextCode( "vertal" + LOWER( LEFT(cVertAlign,1) ) )
ENDIF

// Set the initial font size
::SetFontSize(::nFontSize)

// Forget page numbers for now...


RETURN NIL
**********************  END OF PageSetup()  *************************







METHOD Paragraph( cText, nFontNumber, nFontSize, cAppear, ;
				cHorzAlign, aTabPos, nIndent, nFIndent, nRIndent, nSpace, ;
				lSpExact, nBefore, nAfter, lNoWidow, lBreak, ;
				lBullet, cBulletChar, lHang, lDefault, lNoPar, ;
				nTCLevel ) CLASS RichText
*********************************************************************
* Description:  Write a new, formatted paragraph to the output file.
* Arguments:    
* Return:       
*               
*--------------------------------------------------------------------
* Date       Developer   Comments
* 01/12/97   TRM         Creation
* 07/09/97   TRM         Added nTCLevel
*********************************************************************
DEFAULT ;
	lDefault := .F., ;
	lNoWidow := .F., ;
	lBreak := .F., ;
	lBullet := .F., ;
	lHang := .F., ;
	cAppear := "", ;
	cHorzAlign := "", ;
	cBulletChar := "\bullet", ;
	lNoPar := .F.

::LogicCode("pagebb", lBreak)

IF !lNoPar
	::TextCode( "par" )
ENDIF

::LogicCode( "pard", lDefault )
::NewFont( nFontNumber )
::SetFontSize( nFontSize )
::Appearance( cAppear )
::HAlignment( cHorzAlign )

IF VALTYPE( aTabPos ) == "A"
	AEVAL( aTabPos, { |x| ::NumCode("tx", x) } )
ENDIF

::NumCode( "li", nIndent )
::NumCode( "fi", nFIndent )
::NumCode( "ri", nRIndent )
::LineSpacing( nSpace, lSpExact )

::NumCode( "sb", nBefore )
::NumCode( "sa", nAfter )

::LogicCode("keep", lNoWidow)

IF lBullet
	::OpenGroup()
		::TextCode( "*" )
		::TextCode( "pnlvlblt" )
		::LogicCode( "pnhang", lHang )
		::TextCode( "pntxtb " + cBulletChar )
	::CloseGroup()
ENDIF

::Write( cText )

// 7/9/97
IF VALTYPE( nTCLevel ) == "N"
	::OpenGroup()
		::TextCode( "v" ) // this hides the following text
		::TextCode( "tc" )
		::Write( cText )
		::NumCode( "tcl", nTCLevel, .F. )
	//	::TextCode( "v0" ) // this turns off hidden attribute
	::CloseGroup()
ENDIF

RETURN NIL
**********************  END OF Paragraph()  *************************









METHOD SetFontSize( nFontSize ) CLASS RichText
*********************************************************************
* Description:    Size in points -- must double value because
*                 RTF font sizes are expressed in half-points
* Arguments:    
* Return:       
*               
*--------------------------------------------------------------------
* Date       Developer   Comments
* 01/06/97   TRM         Creation
*
*********************************************************************

IF VALTYPE( nFontSize ) == "N"
	::nFontSize := nFontSize
	::NumCode( "fs", ::nFontSize*2, .F. )
ENDIF

RETURN NIL
**********************  END OF SetFontSize()  ***********************










METHOD Write( xData, lCodesOK ) CLASS RichText
*********************************************************************
* Description:  Write data to output file, accounting for any characters
*               above ASCII 127 (RTF only deals with 7-bit characters
*               directly) -- 8-bit characters must be handled as hex data.
* Arguments:    
* Return:       
*               
*--------------------------------------------------------------------
* Date       Developer   Comments
* 01/06/97   TRM         Creation
*********************************************************************
LOCAL cWrite := ""
LOCAL i, cChar, nChar
LOCAL cString := cValToChar( xData )
LOCAL aCodes := { "\", "{", "}" }

DEFAULT lCodesOK := .F.

IF ::lTrimSpaces
	cString := RTRIM( cString )
ENDIF


FOR i := 1 TO LEN(cString)

	cChar := SUBSTR(cString, i, 1)
	nChar := ASC(cChar)

	IF nChar < 128

		IF nChar > 91

			// Process special RTF symbols
			IF !lCodesOK
				IF ASCAN( aCodes, cChar ) > 0
					cChar := "\" + cChar
				ENDIF
			ENDIF

		ELSEIF nChar < 33

			// 7/9/97
			// 1. Don't convert hard returns to "\par" if we're processing
			//    RTF data (i.e., lCodesOK), since some RTF's have
			//    stray returns in them.
			// 2. Convert all tabs to "\tab", since MS-Word doesn't like CHR(9)'s

			IF !lCodesOK .AND. nChar == 13 // Turn carriage returns into new paragraphs
				cChar := "\par " 
			ELSEIF nChar == 9 // 7/9/97 -- Convert tabs
				cChar := "\tab "
			ELSEIF nChar == 10 // Ignore line feeds
				LOOP
			ENDIF

		ENDIF

		cWrite += cChar

	ELSE

		// We have a high-order character, which is a no-no in RTF.
		// If no international translation table for high-order characters
		// is specified, write data verbatim in hex format.  If a
		// translation table is specified, look up the appropriate
		// hex value to write.

		IF EMPTY( ::aTranslate )
			// Ignore soft line breaks
			IF nChar == 141
				LOOP
			ELSE
				cWrite += "'" + LOWER( NewBase( nChar, 16 ) )
			ENDIF
		ELSE
			cWrite += ::aTranslate[ ASC(cChar)-127 ]
		ENDIF

	ENDIF

NEXT

::OpenGroup()
FWRITE(::hFile, cWrite )
::CloseGroup()

RETURN NIL
*************************  END OF Write()  **************************









METHOD NumCode( cCode, nValue, lScale ) CLASS RichText
*********************************************************************
* Description:  Write an RTF code with a numeric parameter
*               to the output file,
*
*               NOTE: Most RTF numeric measurements must be specified
*               in "Twips" (1/20th of a point, 1/1440 of an inch).
*               However, the interface layer of the RichText class
*               defaults to accept inches.  Therefore, all such
*               measurements must be converted to Twips.
*
* Arguments:    
* Return:       
*               
*--------------------------------------------------------------------
* Date       Developer   Comments
* 01/12/97   TRM         Creation
*
*********************************************************************
LOCAL cWrite := ""

IF VALTYPE(cCode) == "C" .AND. VALTYPE(nValue) == "N"

	cCode := FormatCode(cCode)

	cWrite += cCode

	DEFAULT lScale := .T.
	IF lScale
		nValue := INT( nValue * ::nScale )
	ENDIF
	cWrite += ALLTRIM(STR(nValue)) //+ " "

	FWRITE(::hFile, cWrite )

ENDIF

RETURN NIL
***********************  END OF NumCode()  *************************







METHOD LogicCode( cCode, lTest ) CLASS RichText
*********************************************************************
* Description:  Write an RTF code if the supplied value is true
*
* Arguments:    
* Return:       
*               
*--------------------------------------------------------------------
* Date       Developer   Comments
* 01/12/97   TRM         Creation
*
*********************************************************************

IF VALTYPE(cCode) == "C" .AND. VALTYPE(lTest) == "L"
	IF lTest
		::TextCode( cCode )
	ENDIF
ENDIF

RETURN NIL
***********************  END OF LogicCode()  *************************









FUNCTION FormatCode( cCode )
*********************************************************************
* Description:  Remove extraneous spaces from a code, and make sure
*               that it has a leading backslash ("\").
* Arguments:    
* Return:       
*               
*--------------------------------------------------------------------
* Date       Developer   Comments
* 01/12/97   TRM         Creation
*
*********************************************************************
cCode := ALLTRIM(cCode)
IF !( LEFT(cCode, 1) == "\" )
	cCode := "\" + cCode
ENDIF

RETURN cCode
***********************  END OF FormatCode()  ***********************






METHOD DefineTable( cTblHAlign, nTblFntNum, nTblFntSize, ;
		cCellAppear, cCellHAlign, nTblRows, ;
		nTblColumns, nTblRHgt, aTableCWid, cRowBorder, cCellBorder, nCellPct, ;
		lTblNoSplit, nTblHdRows, nTblHdHgt, nTblHdPct, nTblHdFont, ;
		nTblHdFSize, cHeadAppear, cHeadHAlign ) CLASS RichText
*********************************************************************
* Description:  Define the default setup for a table.
*               This simply saves the parameters to the object's
*               internal instance variables.
* Arguments:    
* Return:       
*               
*--------------------------------------------------------------------
* Date       Developer   Comments
* 01/18/97   TRM         Creation
*
*********************************************************************
LOCAL i
DEFAULT ;
	cTblHAlign := "CENTER", ;
	nTblFntNum := 1, ;
	nTblFntSize := ::nFontSize, ;
	nTblRows := 1, ;
	nTblColumns:= 1, ;
	nTblRHgt := NIL, ;
	aTableCWid := ARRAY(nTblColumns), ; // see below
	cRowBorder := "NONE", ;
	cCellBorder := "SINGLE", ;
	nCellPct := 0, ;
	lTblNoSplit := .F., ;
	nTblHdRows := 0, ;
	nTblHdHgt := nTblRHgt, ;
	nTblHdPct := .1, ;
	nTblHdFont := nTblFntNum, ;
	nTblHdFSize := ::nFontSize + 2

	IF aTableCWid[1] == NIL
		AFILL( aTableCWid, 6.5/nTblColumns )
	ELSEIF VALTYPE(aTableCWid[1]) == "A"
		aTableCWid := ACLONE(aTableCWid[1])
	ENDIF

	// Turn independent column widths into "right boundary" info...
	FOR i := 2 TO LEN( aTableCWid )
		aTableCWid[i] += aTableCWid[i-1]
	NEXT

::cTblHAlign := LOWER( LEFT(cTblHAlign, 1) )
::nTblFntNum := nTblFntNum
::nTblFntSize := nTblFntSize
::cCellAppear := cCellAppear
::cCellHAlign := cCellHAlign
::nTblRows := nTblRows
::nTblColumns:= nTblColumns
::nTblRHgt := nTblRHgt
::aTableCWid := aTableCWid
::cRowBorder := ::BorderCode( cRowBorder )
::cCellBorder := ::BorderCode( cCellBorder )
::nCellPct := IIF( nCellPct < 1, nCellPct*10000, nCellPct*100 )
::lTblNoSplit := lTblNoSplit
::nTblHdRows := nTblHdRows
::nTblHdHgt := nTblHdHgt
::nTblHdPct := IIF( nTblHdPct < 1, nTblHdPct*10000, nTblHdPct*100 )
::nTblHdFont := nTblHdFont
::nTblHdFSize := nTblHdFSize
::cHeadAppear := cHeadAppear
::cHeadHAlign := cHeadHAlign

::nCurrColumn := 0
::nCurrRow    := 0

RETURN NIL
**********************  END OF DefineTable()  ***********************











METHOD WriteCell( cText, nFontNumber, nFontSize, cAppear, cHorzAlign, ;
				nSpace, lSpExact, cCellBorder, nCellPct, lDefault, ;
				lMrgColumns, nMrgColumns ) CLASS RichText
*********************************************************************
* Description:  Write a formatted cell of data to the current row
*               of the current table.  Also takes care of the logic
*               required for headers & header formatting.
* Arguments:    
* Return:       
*               
*--------------------------------------------------------------------
* Date       Developer   Comments
* 01/19/97   TRM         Creation
*
*********************************************************************
LOCAL i

DEFAULT ;
	cText := "", ;
	lDefault := .F., ;
	lMrgColumns := .F., ;
	nMrgColumns := ::nTblColumns

// Increment/reset the column #
IF ::nCurrColumn == ::nTblColumns
	::nCurrColumn := 1
ELSE
	::nCurrColumn += 1
ENDIF


// Apply any one-time formatting for header/body

IF ::nCurrColumn == 1

	IF ::nCurrRow == 0 .AND. ::nTblHdRows > 0

		// Start a separate group for the header rows
		::OpenGroup()
		::BeginRow()

		// We need to apply header formats
		// The "\trgaph108" & "trleft-108" are the defaults used by MS-Word,
		// so if it's good enough for Word, it's good enough for me...

		::TextCode( "trgaph108\trleft-108" )
		::TextCode( "trq" + ::cTblHAlign )
		::Borders( "tr", ::cRowBorder )
		::NumCode( "trrh", ::nTblHdHgt )
		::TextCode( "trhdr" )
		::LogicCode( "trkeep", ::lTblNoSplit )

		// Set the default border & width info for each header cell
		FOR i := 1 TO LEN( ::aTableCWid )
			::NumCode( "clshdng", ::nTblHdPct, .F. )
			::Borders( "cl", ::cCellBorder )
			::NumCode("cellx", ::aTableCWid[i] )
		NEXT

		// Identify the header-specific font
		::NewFont( ::nTblHdFont )
		::SetFontSize( ::nTblHdFSize )
		::Appearance( ::cHeadAppear )
		::HAlignment( ::cHeadHAlign )

		::TextCode( "intbl" )

	ELSEIF ::nCurrRow == ::nTblHdRows

		// The header rows are over,
		// so we need to apply formats to the body of the table.

		// First close the header section, if one exists
		IF ::nTblHdRows > 0
			::EndRow()
			::CloseGroup()
		ENDIF

		::BeginRow()
		::TextCode( "trgaph108\trleft-108" )
		::TextCode( "trq" + ::cTblHAlign )
		::Borders( "tr", ::cRowBorder )
		::NumCode( "trrh", ::nTblRHgt )
		::LogicCode( "trkeep", ::lTblNoSplit )

		// Set the default shading, border & width info for each body cell
		FOR i := 1 TO LEN( ::aTableCWid )
			::NumCode( "clshdng", ::nCellPct, .F. )
			::Borders( "cl", ::cCellBorder )
			::NumCode("cellx", ::aTableCWid[i] )
		NEXT

		// Write the body formatting codes
		::NewFont( ::nTblFntNum )
		::SetFontSize( ::nTblFntSize )
		::Appearance( ::cCellAppear )
		::HAlignment( ::cCellHAlign )

		::TextCode( "intbl" )

	ELSE

		// End of a row of the table body.
		::EndRow()

		// Prepare the next row for inclusion in table
		::TextCode( "intbl" )

	ENDIF

ENDIF



// Apply any cell-specific formatting, and write the text

::OpenGroup()

	::LogicCode( "pard", lDefault )
	::NewFont( nFontNumber )
	::SetFontSize( nFontSize )
	::Appearance( cAppear )
	::HAlignment( cHorzAlign )
	::LineSpacing( nSpace, lSpExact )
	::Borders( "cl", cCellBorder )
	::NumCode( "clshdng", nCellPct, .F. )

	// Now write the text
	::Write( cText )

::CloseGroup()

// Close the cell
::TextCode( "cell" )

RETURN NIL
***********************  END OF WriteCell()  ************************









METHOD NewSection( lLandscape, nColumns, nLeft, nRight, nTop, nBottom, ;
				nWidth, nHeight, cVertAlign, lDefault ) CLASS RichText
*********************************************************************
* Description:  Open a new section, with optional new formatting
*               properties.
*               
* Arguments:    
* Return:       
*               
*--------------------------------------------------------------------
* Date       Developer   Comments
* 01/26/97   TRM         Creation
*********************************************************************
DEFAULT lDefault := .F.

//::OpenGroup()
::TextCode( "sect" )

IF lDefault
	::TextCode( "sectd" )
ENDIF

::LogicCode( "lndscpsxn", lLandscape )
::NumCode( "cols", nColumns, .F. )
::NumCode( "marglsxn", nLeft )
::NumCode( "margrsxn", nRight )
::NumCode( "margtsxn", nTop )
::NumCode( "margbsxn", nBottom )
::NumCode( "pgwsxn", nWidth )
::NumCode( "pghsxn", nHeight )

IF !EMPTY( cVertAlign )
	::TextCode( "vertal" + LOWER( LEFT(cVertAlign,1) ) )
ENDIF

RETURN NIL
***********************  END OF NewSection()  **********************







METHOD NewFont( nFontNumber ) CLASS RichText
*********************************************************************
* Description:  Change the current font.
*               Converts app-level font number into RTF font number.
* Arguments:    
* Return:       
*               
*--------------------------------------------------------------------
* Date       Developer   Comments
* 01/19/97   TRM         Creation
*
*********************************************************************

IF !EMPTY(nFontNumber) .AND. nFontNumber <= ::nFontNum
	::NumCode( "f", nFontNumber-1, .F. )
ENDIF

RETURN NIL
************************  END OF NewFont()  *************************








METHOD Appearance( cAppear ) CLASS RichText
*********************************************************************
* Description:  Change the "appearance" (bold, italic, etc.)
*               Appearance codes are concatenable at the app-level
*               and already contain backslashes.
* Arguments:    
* Return:       
*               
*--------------------------------------------------------------------
* Date       Developer   Comments
* 01/19/97   TRM         Creation
*
*********************************************************************

// Special case (see .CH file) -- first remove leading slash ...ugh.
IF !EMPTY(cAppear)
	::TextCode( SUBSTR(cAppear, 2) )
ENDIF

RETURN NIL
***********************  END OF Appearance()  ***********************







METHOD HAlignment( cAlign ) CLASS RichText
*********************************************************************
* Description:  Change the horizontal alignment
* Arguments:    
* Return:       
*               
*--------------------------------------------------------------------
* Date       Developer   Comments
* 01/19/97   TRM         Creation
*
*********************************************************************
IF !EMPTY(cAlign)
	::TextCode( "q" + LOWER(LEFT(cAlign,1)) )
ENDIF

RETURN NIL
**********************  END OF HAlignment()  ************************








METHOD LineSpacing( nSpace, lSpExact ) CLASS RichText
*********************************************************************
* Description:  Change the line spacing (spacing can either be "exact"
*               or "multiple" (of single spacing).  If exact, the units
*               of the supplied value must be converted to twips.
* Arguments:    
* Return:       
*               
*--------------------------------------------------------------------
* Date       Developer   Comments
* 01/19/97   TRM         Creation
*
*********************************************************************
DEFAULT lSpExact := .F.

::NumCode( "sl", nSpace, lSpExact )
IF !EMPTY( nSpace )
	::NumCode( "slmult", IIF( lSpExact, 0, 1 ), .F. )
ENDIF


RETURN NIL
**********************  END OF LineSpacing()  ***********************








METHOD Borders( cEntity, cBorder ) CLASS RichText
*********************************************************************
* Description:  Apply borders to rows or cells.  Currently limited to
*               one type of border per rectangle.
* Arguments:    
* Return:       
*               
*--------------------------------------------------------------------
* Date       Developer   Comments
* 01/19/97   TRM         Creation
*
*********************************************************************
LOCAL i, aBorder := { "t", "b", "l", "r" }

IF VALTYPE( cBorder ) == "C"
	FOR i := 1 TO 4
		::TextCode( cEntity + "brdr" + aBorder[i] + "\brdr" + cBorder )
	NEXT
ENDIF

RETURN NIL
************************  END OF Borders()  *************************






METHOD BorderCode( cBorderID ) CLASS RichText
*********************************************************************
* Description:  Convert an application-level border ID into
*               a valid RTF border code.
* Arguments:    
* Return:       
*               
*--------------------------------------------------------------------
* Date       Developer   Comments
* 01/19/97   TRM         Creation
*
*********************************************************************
LOCAL cBorderCode := "", n
LOCAL aBorder := ;
	{ ;
	{ "NONE",        NIL   }, ;
	{ "SINGLE",      "s"   }, ;
	{ "DOUBLETHICK", "th"  }, ;
	{ "SHADOW",      "sh"  }, ;
	{ "DOUBLE",      "db"  }, ;
	{ "DOTTED",      "dot" }, ;
	{ "DASHED",      "dash"}, ;
	{ "HAIRLINE",    "hair"}  ;
	}

cBorderID := UPPER( RTRIM(cBorderID) )

n := ASCAN( aBorder, { |x| x[1] == cBorderID } ) 

IF n > 0
	cBorderCode := aBorder[n][2]
ENDIF

RETURN cBorderCode
************************  END  OF BorderCode()  *********************







METHOD PageNumber( cHorzAlign ) CLASS RichText
*********************************************************************
* Description:  Insert a page number field.
*               Best used within headers and footers.
* Arguments:    
* Return:       
*               
*--------------------------------------------------------------------
* Date       Developer   Comments
* 07/09/97   TRM         Creation
* 07/09/97   TRM         NOTE: cHorzAlign not supported
*
*********************************************************************

DEFAULT cHorzAlign := ""

::OpenGroup()

	::HAlignment( cHorzAlign ) // not sure if this works

	::TextCode( "field" )
	::OpenGroup()
		::TextCode( "*\fldinst PAGE  \" )
		::TextCode( "* MERGEFORMAT  " )
	::CloseGroup()
	::OpenGroup()
		::TextCode( "fldrslt " ) // don't include a result for now
	::CloseGroup()
::CloseGroup()


RETURN NIL
************************  END  OF PageNumber()  *********************









FUNCTION RTFHeader( cFile )
*********************************************************************
* Description:  Return the header portion of an RTF file.  If the
*               return value is empty, it's not an RTF.
*
*               This algorithm is not fool-proof, but it's good
*               enough to handle most real-world RTFs.
*
* Arguments:    
* Return:       
*               
*--------------------------------------------------------------------
* Date       Developer   Comments
* 02/05/97   TRM         Creation
*
*********************************************************************
LOCAL hFile
LOCAL cBuffer := ""
LOCAL cCurrCmd := ""
LOCAL nGrpLevel := 0, nGrpCmd := 0
LOCAL nRealBytes := 0, nRead := 0
LOCAL aHeadCmd := ;
	{ "\rtf", "\colortbl", "\fonttbl", "\info", ;
	  "\filetbl", "\stylesheet", "\revtbl" }


IF IsRTF( cFile )

	hFile := FOPEN( cFile )

	DO WHILE .T.

		cBuffer := FREADSTR(hFile, 1)
		IF cBuffer == ""
			EXIT
		ENDIF
		++nRead

		IF cBuffer $ "{}\; "

			// If currently parsing an RTF command, terminate it.

			IF !EMPTY( cCurrCmd )

				// We're done if this is the first command in a group,
				// but the command is not a standard header command.
				// NOTE: This logic assumes that there is at least one
				// valid header group after the "\rtf..." sequence.
				// It also assumes that the first command in a header group
				// is the group identifier

				IF nGrpLevel < 2 .AND. nGrpCmd == 1 .AND. ;
					ASCAN( aHeadCmd, cCurrCmd ) == 0 .AND. ;
					LEFT( cCurrCmd, 4 ) != aHeadCmd[1]

					// This is not a valid header group, so we assume that
					// the header is complete, and quit.
					EXIT

				ENDIF

				cCurrCmd := ""

			ENDIF

			DO CASE

				CASE cBuffer == "{"
					++nGrpLevel
					nGrpCmd := 0 // count # of commands found in group

				CASE cBuffer == "}"
					--nGrpLevel
					nRealBytes := nRead // Mark spot where last group ended.
					nGrpCmd := 0 // count # of commands found in group

				CASE cBuffer == "\"
					cCurrCmd += cBuffer
					++nGrpCmd

			ENDCASE

		ELSEIF !EMPTY( cCurrCmd )

			// We assume that anything other character is part of the command
			cCurrCmd += cBuffer

		ENDIF

	ENDDO

	// Now read the header bytes from the beginning.
	// Inefficient, but in the vast scheme of things, who really cares?

	cBuffer := ""
	IF nRealBytes > 0
		FSEEK( hFile, 0 )
		cBuffer := FREADSTR( hFile, nRealBytes )
	ENDIF

	FCLOSE( hFile )

ENDIF

RETURN cBuffer
***********************  END OF RTFHeader()  ************************







FUNCTION IsRTF( cFile )
*********************************************************************
* Description:  Determine if a file is an RTF file
* Arguments:    
* Return:       
*               
*--------------------------------------------------------------------
* Date       Developer   Comments
* 02/05/97   TRM         Creation
*
*********************************************************************
LOCAL lRTF := .F., hFile, cBuffer

hFile := FOPEN( cFile )

IF hFile >= 0

	FSEEK( hFile, 0 )
	cBuffer := FREADSTR( hFile, 5 )

	IF cBuffer == "{\rtf"
		lRTF := .T.
	ENDIF
	FCLOSE( hFile )

ENDIF

RETURN lRTF
***********************  END OF IsRTF()  ************************












FUNCTION RTFMerge( cPriFile, nSkip, xRTF, cClipDelims )
*********************************************************************
* Description:  Merge data into the format specified in <cPriFile>,
*               which contains Clipper expressions delimited by
*               <cClipDelims>.  The intent is that <cPriFile> will
*               be an RTF file, though it doesn't have to be.
*
*               <nSkip> specifies the number of bytes to skip at
*               the top of the file, before reading the merge data.
*               This allows you to skip the RTF header, for example,
*               so that the merged output doesn't have multiple headers.
*
*               <xRTF> may be either a RichText() object, or just an
*               open file handle.
*
*               <cClipDelims> defaults to "<[]>".  If specified, it
*               must be exactly 4 characters (4 make it easier to
*               specify unique delimiters).  The first two bytes
*               specify the opening delimiter, and the last two specify
*               the closing delimiter. These are the delimiters that
*               must be used in <cPriFile> to designate Clipper
*               expressions.  For example <[Myarea->Name]>, or
*               <[DBSKIP()]>.  The supplied expressions must be
*               macro-compilable.  Currently, no error handling is
*               supplied for bad expressions.
*               
*               Inspired by a request from Michael Mozina on the
*               Compuserve Clipper Forum.
*               
* Arguments:    
* Return:       
*               
*--------------------------------------------------------------------
* Date       Developer   Comments
* 02/02/97   TRM         Creation
*
*********************************************************************
LOCAL hPriFile, aDelims, cBuffer, cPrevBuff := "", cOrigBuff
LOCAL nCurrCode := 0, cExpr := "", lCodeTest
LOCAL lRTF := VALTYPE(xRTF) == "O"
LOCAL xEval
LOCAL nRead := 0, nTotBytes

DEFAULT cClipDelims := "<[]>"

hPriFile := FOPEN( cPriFile )

IF hPriFile < 0
	MsgInfo( "Could not open primary merge file: " + cPriFile + "." )
ELSE

	IF !( LEN(cCLipDelims) == 4 )

		MsgInfo( "Bad delimiters specified for merge file." )

	ELSE

		// File is open, and delimiters are OK, so let's move forward.

		// See how many bytes we have.
		nTotBytes := FSEEK( hPriFile, 0, 2 )

		// Reposition to the top of the file.
		FSEEK( hPriFile, 0, 0 )

		// If an initial offset is specified, process it.
		// For RTF's, this is used to skip the header.
		IF VALTYPE( nSkip ) == "N"
			nRead := FSEEK( hPriFile, nSkip )
		ENDIF

		// Dump the delimiters into an array.
		aDelims := ;
			{ ;
			SUBSTR( cClipDelims, 1, 1 ), ;
			SUBSTR( cClipDelims, 2, 1 ), ;
			SUBSTR( cClipDelims, 3, 1 ), ;
			SUBSTR( cClipDelims, 4, 1 )  ;
			}

		// Loop through primary merge file.
		// For RTF's, we want to stop one byte before the end.

		DO WHILE nRead < nTotBytes-1

			++nRead

			cBuffer := FREADSTR( hPriFile, 1 )
			cOrigBuff := cBuffer

			// If we think we're reading a Clipper expression,
			// this determines if we've encountered the next
			// expected part of the sequence.

			lCodeTest := ( cBuffer == aDelims[nCurrCode+1] )

			IF lCodeTest

				// We have met a delimiter for a Clipper expression

				IF nCurrCode < 3
					++nCurrCode
				ELSE

					// We have completed the Clipper expression, so let's
					// evaluate it and dump the result to the output file.

					// First remove extraneous spaces.
					cExpr := ALLTRIM( cExpr )

					// Now macro compile & evaluate it.
					// [NOTE: An error trap for bad expressions would be a nice
					// touch someday -- but this is "proof of concept" right now].

					// eventually, set error handler to catch bad expressions...
					xEval := EVAL( { || &cExpr } )
					// ...reset original error handler

					// If the expression returns a character value, we interpret
					// it as text; otherwise, we just move on.  This allows us to
					// embed directions like "DBSKIP()" in the primary file.

					IF VALTYPE( xEval ) == "C"
						cBuffer := xEval
					ELSE
						cBuffer := ""
					ENDIF

					// Reset the placeholder & flags for the next expression.
					cExpr   := ""
					nCurrCode := 0
					lCodeTest := .F.

				ENDIF

			ELSE

				// It's not a valid delimiter, or is out of sequence.
				// If we were expecting the second in a two-character sequence,
				// we have to treat the first character as simple data now.

				IF nCurrCode == 1 .OR. nCurrcode == 3
					--nCurrCode
					cBuffer := cPrevBuff + cBuffer
				ENDIF

			ENDIF

			// If it wasn't a non-terminating code, we write
			// the data to the output file.
			// We either have "constant" data, or part of a
			// Clipper expression.

			IF !lCodeTest
				IF nCurrCode == 2
					// We're in the process of reading a Clipper expression,
					// so just add to it.
					cExpr += cBuffer
				ELSE
					// We have constant data, so just pass it to the output.
					IF lRTF
						// If we're reading an RTF file, we allow RTF codes
						// in the output, as-is (second parameter is .T.).
						xRTF:Write( cBuffer, .T. )
					ELSE
						// If not an RTF, just write the output directly
						FWRITE( xRTF, cBuffer )
					ENDIF
				ENDIF
			ENDIF

			cPrevBuff := cOrigBuff

		ENDDO

	ENDIF

	FCLOSE( hPriFile )

ENDIF

RETURN NIL
**************************  END  OF RTFMerge()  ***********************









FUNCTION IntlTranslate()
*********************************************************************
* Description:  Example of an array that could be used to map
*               high-order characters.
* Arguments:    
* Return:       
*               
*--------------------------------------------------------------------
* Date       Developer   Comments
* 01/06/97   TRM         Creation
*
*********************************************************************
LOCAL i
LOCAL aTranslate[128]
LOCAL aHighTable := ;
	{ ;
	"\'fc", "\'e9", "\'e2", "\'e4", "\'e0", "\'e5", "\'e7", "\'ea", ;
	"\'eb", "\'e8", "\'ef", "\'ee", "\'ec", "\'c4", "\'c5", "\'c9", ;
	"\'e6", "\'c6", "\'f4", "\'f6", "\'f2", "\'fb", "\'f9", "\'ff", ;
	"\'d6", "\'dc", "\'a2", "\'a3", "\'a5", "\'83", "\'ed", "\'e1", ;
	"\'f3", "\'fa", "\'f1", "\'d1", "\'aa", "\'ba", "\'bf" ;
	}

AFILL( aTranslate, "" )

FOR i := 1 TO LEN( aHighTable )
	aTranslate[i] := aHighTable[i]
NEXT

RETURN aTranslate
**********************  END OF IntlTranslate()  *********************








FUNCTION NewBase( nDec, nBase )
*********************************************************************
* Description:  Convert a decimal numeric to a string in another
*               base system
* Arguments:    
* Return:       
*               
*--------------------------------------------------------------------
* Date       Developer   Comments
* 01/12/97   TRM         Creation
*
*********************************************************************
LOCAL cNewBase := "", nDividend, nRemain, lContinue := .T., cRemain

DO WHILE lContinue

	nDividend := INT( nDec / nBase )
	nRemain := nDec % nBase

	IF nDividend >= 1
		nDec := nDividend
	ELSE
		lContinue := .F.
	ENDIF

	IF nRemain < 10
		cRemain := ALLTRIM(STR(nRemain,2,0))
	ELSE	
		cRemain := CHR( nRemain + 55 )
	ENDIF

	cNewBase := cRemain + cNewBase

ENDDO

RETURN cNewBase
************************  END OF NewBase()  *************************


