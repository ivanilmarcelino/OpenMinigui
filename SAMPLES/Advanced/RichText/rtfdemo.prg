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
¦    (see class code for change summary)                                   ¦
¦                                                                          ¦
¦    10/24/97   TRM     Added demostration of page numbering               ¦
¦                       and table of contents.                             ¦
¦                                                                          ¦
L---------------------------------------------------------------------------
*/


#include "minigui.ch"
#include "richtext.ch"

DECLARE WINDOW Form_PrgBar


FUNCTION Main()
*********************************************************************
* Description:  Demo of selected features of RichText() Class.
*               This demo will create a multi-page RTF file with
*               various fonts, page orientations, layouts, etc.
*               Requires files FLOWERS.DBF & DBT and VEGGIES.DBF
* Arguments:    
* Return:       
*               
*--------------------------------------------------------------------
* Date       Developer   Comments
* 01/28/97   TRM         Creation
*
*********************************************************************
LOCAL cOutFile := "RTFDEMO.RTF"

SET WINDOW MAIN OFF

IF FILE( "FLOWERS.DBF" ) .AND. FILE( "VEGGIES.DBF" )

	CreateProgressBar( "Generating sample reports..." )

	GardenDoc( , cOutFile )

	CloseProgressBar()

	AlertInfo( "Formatting complete.;  To see the output, open files " + ;
			cOutFile + " and MERGEOUT.RTF ;in a word processor. ;" + ;
			"[NOTE: System was tested with Microsoft Word]." )

	AlertInfo( "To see the Table of Contents in MS-Word, refer to the " + ;
			"MS-Word documentation.;  Be sure to tell MS-Word to generate " + ;
			'the table from "table entries", rather than from "styles".' )

ELSE

	MsgStop( "Files FLOWERS.DBF & DBT and VEGGIES.DBF are required for demo." )

ENDIF

RETURN NIL
************************  END OF RTFDemo()  **************************







STATIC FUNCTION GardenDoc( nComplete, cOutFile)
*********************************************************************
* Description:  Demo of selected features of RichText() Class
* Arguments:    
* Return:       
*               
*--------------------------------------------------------------------
* Date       Developer   Comments
* 01/28/97   TRM         Creation
* 02/03/97   TRM         Added ::Merge() demo
*********************************************************************
LOCAL oRTF, n := 0
LOCAL nTotal := 5

//	Open the output file & set some defaults

	oRTF := SetupRTF( cOutFile )


	IF oRTF:hFile >= 0  // avoid a crash if the file was already open.

	//	Demonstrate basics -- fonts, text appearance, hanging indents, etc.

		CoverPage( oRTF )
		nComplete := Int( ( ++n / nTotal ) * 100 )
		IF IsWindowDefined( Form_PrgBar )
			Form_PrgBar.PrgBar_1.Value := nComplete
			Form_PrgBar.Label_1.Value := "Completed " + + hb_ntos( nComplete ) + "%"
		ENDIF
		// refreshing
		INKEYGUI( 20 )

	//	Demonstrate a simple DBF output

		DBFToRTF( oRTF, "VEGGIES.DBF", .F. )
		nComplete := Int( ( ++n / nTotal ) * 100 )
		IF IsWindowDefined( Form_PrgBar )
			Form_PrgBar.PrgBar_1.Value := nComplete
			Form_PrgBar.Label_1.Value := "Completed " + + hb_ntos( nComplete ) + "%"
		ENDIF
		// refreshing
		INKEYGUI( 20 )

	//	Demonstrate mixed orientation (i.e, change to landscape)
	//	and a DBF with memos.

		DBFToRTF( oRTF, "FLOWERS.DBF", .T. )
		nComplete := Int( ( ++n / nTotal ) * 100 )
		IF IsWindowDefined( Form_PrgBar )
			Form_PrgBar.PrgBar_1.Value := nComplete
			Form_PrgBar.Label_1.Value := "Completed " + + hb_ntos( nComplete ) + "%"
		ENDIF
		// refreshing
		INKEYGUI( 20 )


	//	Demonstrate same memo text formatted in snaked columns
	//	in portrait orientation

		FlowerColumns( oRTF, "FLOWERS.DBF" )
		nComplete := Int( ( ++n / nTotal ) * 100 )
		IF IsWindowDefined( Form_PrgBar )
			Form_PrgBar.PrgBar_1.Value := nComplete
			Form_PrgBar.Label_1.Value := "Completed " + + hb_ntos( nComplete ) + "%"
		ENDIF
		// refreshing
		INKEYGUI( 20 )

	//	2/3/97
	//	Demonstrate ::Merge() capability

		MergeDemo()
		nComplete := Int( ( ++n / nTotal ) * 100 )
		IF IsWindowDefined( Form_PrgBar )
			Form_PrgBar.PrgBar_1.Value := nComplete
			Form_PrgBar.Label_1.Value := "Completed " + + hb_ntos( nComplete ) + "%"
		ENDIF
		// final waiting
		INKEYGUI( 1000 )


	//	Close the output file
		CLOSE RTF oRTF

	ENDIF

RETURN NIL
**********************  END OF GardenDoc()  **********************







STATIC FUNCTION SetupRTF( cOutFile)
*********************************************************************
* Description:  
* Arguments:    
* Return:       
*               
*--------------------------------------------------------------------
* Date       Developer   Comments
* 01/28/97   TRM         Creation
*
*********************************************************************
LOCAL oRTF

DEFINE RTF oRTF FILE cOutFile ;
	FONTS "Times New Roman", "Arial", "Courier New" ;
	FONTSIZE 12 ;
	TWIPFACTOR 1440 ;
	WARNOVERWRITE // Warn if output file already exists

IF oRTF:hFile >= 0

	// Trim trailing spaces from data, to save file space.
	oRTF:lTrimSpaces := .T.

	DEFINE PAGESETUP oRTF MARGINS 1.75, 1.75, 1, 1 ;
		TABWIDTH .5 ;
		ALIGN CENTER

	BEGIN HEADER oRTF
		NEW PARAGRAPH oRTF TEXT "RichText() Sample Report " + DTOC(DATE()) ;
			FONTSIZE 14 ;
			ALIGN CENTER
	END HEADER oRTF

	BEGIN FOOTER oRTF

		NEW PARAGRAPH oRTF TEXT "" ALIGN CENTER

		// 10/24/97 -- demonstrate page numbering
		INSERT PAGENUMBER oRTF

	END FOOTER oRTF

ENDIF

RETURN oRTF
**********************  END OF SetupRTF()  ***********************







STATIC FUNCTION CoverPage( oRTF )
*********************************************************************
* Description:  Generate a cover page.
* Arguments:    
* Return:       
*               
*--------------------------------------------------------------------
* Date       Developer   Comments
* 01/28/97   TRM         Creation
*
*********************************************************************
LOCAL i
LOCAL aTitle[3]
LOCAL aBullet[5]

//	First, load some text...

aTitle[1] := "RichText() Sample Report Summary"

aTitle[2] := "NOTE: This report includes an automated table of contents. " + ;
				"If you are using MS-Word, you can create the table of contents " + ;
				"via the Insert\Indexes and Tables... option. Be sure to tell " + ;
				'MS-Word to generate the table from "table entries", rather ' + ;
				'than from "styles".'

aBullet[1] := "This report is a demonstration of some of the capabilities " + ;
			"of the RichText() class for Clipper & Fivewin, Version 1.0, written " + ;
			"by Tom Marchione.  This is the free version, which contains a " + ;
			"basic feature set (please review the README file for certain " + ;
			"minor usage restrictions). If you have any comments or questions, " + ;
			"feel free to send an E-Mail to tmarchione@compuserve.com."

aBullet[2] := "RichText() lets you generate reports to RTF files, like this " + ;
			"one.  RTF files can be read by most word processors, so this " + ;
			"is a way to move fully-formatted information into word processor " + ;
			"format, without lots of extra spaces and carriage returns.  " + ;
			"The class can form the basis of a true database publishing system."

aBullet[3] := "RichText() is not meant to be a front-line report engine in " + ;
			"its current form, in the sense that database programmers " + ;
			"expect report generators to have certain standard features.  " + ;
			"Nevertheless, it can be very useful for meeting specialized, " + ;
			"custom reporting requirements, particularly if you need to edit or " + ;
			"manipulate the output."

aBullet[4] := "In its current form, the system is fairly quirky.  " + ;
			"The good news is that, generally, you can get exactly what you " + ;
			"want in one or two code iterations.  I plan to address " + ;
			"various usability issues in future versions."

aBullet[5] := "The pages that follow contain some examples of the types of " + ;
			"things that can be done with RichText().  Remember, RichText() " + ;
			"is designed to link to a word processor, so its capabilities " + ;
			"focus on standard word processing features, rather than the " + ;
			"kinds of things that are important in a standard report " + ;
			"generator.  Hope you find it useful!"


// Write the title lines

NEW PARAGRAPH oRTF TEXT aTitle[1] ;
	FONTNUMBER 1 ;
	FONTSIZE 18 ;
	APPEARANCE BOLD_ON ;
	ALIGN CENTER ;
	SETDEFAULT ;
	TOCLEVEL 1  // 10/24/97 -- mark this for the Table of Contents.

NEW PARAGRAPH oRTF TEXT ""

NEW PARAGRAPH oRTF TEXT aTitle[2] ;
	FONTSIZE 10 ;
	APPEARANCE BOLD_OFF + ITALIC_ON ;
	ALIGN LEFT ;
	INDENT -.5 ;
	RIGHTINDENT -.5

NEW PARAGRAPH oRTF TEXT "" ;
	APPEARANCE BOLD_OFF + ITALIC_OFF ;
	SETDEFAULT


// Write the bullet items

FOR i := 1 TO LEN( aBullet )

	NEW PARAGRAPH oRTF TEXT aBullet[i] ;
		FONTNUMBER 2 ;
		FONTSIZE 11 ;
		ALIGN LEFT ;
		INDENT .25 ;
		FIRSTINDENT -.25 ;
		BULLETED ;
		SPACEBEFORE .4 ;
		SETDEFAULT

NEXT

NEW PARAGRAPH oRTF TEXT "" SETDEFAULT

RETURN NIL
************************  END OF CoverPage()  ***********************









STATIC FUNCTION DBFToRTF( oRTF, cFile, lLandScape, cTitle )
*********************************************************************
* Description:  Format specified DBF into an RTF Table
* Arguments:    
* Return:       
*               
*--------------------------------------------------------------------
* Date       Developer   Comments
* 01/28/97   TRM         Creation
*
*********************************************************************
LOCAL i, nWidth, aStruc
LOCAL aFldNames := {}, aColWidth := {}
LOCAL nTotWidth := 0, cName

USE (cFile) ALIAS MyFile NEW EXCLUSIVE

aStruc := DBSTRUCT()

FOR i := 1 TO LEN( aStruc )

	IF LEFT(aStruc[i][2], 1) == "M"
		// Default memo Columns to 4 inches wide
		nWidth := 4
	ELSE
		// Default non-memo columns to 1/10th of field width
		nWidth := (aStruc[i][3] + aStruc[i][4])/10

		// Stretch column width to width of header, if necessary
		nWidth := MAX( nWidth, LEN(ALLTRIM(aStruc[i][1]))/9 )
		
		// Place a limit of 5 inches on the width of any single column
		nWidth := MIN( nWidth, 5 )
	ENDIF

	nTotWidth += nWidth
	IF nTotWidth <= 10
		AADD( aFldNames, ALLTRIM( aStruc[i][1] ) )
		AADD( aColWidth, nWidth )
	ELSE
		EXIT // For demo, only include enough columns as will fit
	ENDIF
	
NEXT
aStruc := NIL


// Begin a new section of the document

IF lLandScape
	NEW SECTION oRTF ;
		LANDSCAPE ;
		PAGEWIDTH 11 ;
		PAGEHEIGHT 8.5 ;
		MARGINS .5, .5, .5, .5 ;
		ALIGN CENTER ;
		SETDEFAULT
ELSE
	NEW SECTION oRTF ;
		PAGEWIDTH 8.5 ;
		PAGEHEIGHT 11 ;
		MARGINS .5, .5, .5, .5 ;
		ALIGN CENTER ;
		SETDEFAULT
ENDIF


// Add a title, for use in the table of contents

IF EMPTY( cTitle )
	cTitle := "Sample Table Derived From " + cFile
ENDIF

NEW PARAGRAPH oRTF TEXT cTitle ;
	FONTNUMBER 1 ;
	FONTSIZE 18 ;
	APPEARANCE BOLD_ON ;
	ALIGN CENTER ;
	SETDEFAULT ;
	TOCLEVEL 1

NEW PARAGRAPH oRTF TEXT ""
NEW PARAGRAPH oRTF TEXT ""

// Define the table

DEFINE TABLE oRTF ;              // Specify the RTF object
	ALIGN CENTER ;                // Center table horizontally on page
	FONTNUMBER 2 ;                // Use font #2 for the body rows
	FONTSIZE 9 ;                  // Use 9 Pt. font for the body rows
	CELLAPPEAR BOLD_OFF ;         // Normal cells unbolded
	CELLHALIGN LEFT ;             // Text in normal cells aligned left
	COLUMNS LEN(aFldNames) ;      // Table has n Columns
	CELLWIDTHS aColWidth ;        // Array of column widths
	ROWHEIGHT .25 ;               // Minimum row height is .25"
	CELLBORDERS SINGLE ;          // Outline cells with thin border
	HEADERROWS 1 ;                // One row to be treated as the header
		HEADERHEIGHT .5 ;          // Header rows are min. .5" high
		HEADERSHADE 25 ;           // Header shading is 25%
		HEADERFONT 1 ;             // Use font #1 for the header
		HEADERFONTSIZE 10 ;        // Use 10 Pt. font for the header
		HEADERAPPEAR BOLD_ON ;     // Header cells are bold
		HEADERHALIGN CENTER        // Text in header cells is centered

// Write the header row, using field names as titles
FOR i := 1 TO oRTF:nTblColumns

	// Abbreviate column headers that are disproportionately long
	cName := aFldNames[i]
	IF LEN( cName ) > aColWidth[i]
		cName := LEFT( cName, aColWidth[i]-1 ) + "."
	ENDIF
	WRITE CELL oRTF TEXT cName

NEXT

// Write the data rows
DO WHILE !EOF()
	FOR i := 1 TO oRTF:nTblColumns
		WRITE CELL oRTF TEXT FIELDGET(i)
	NEXT
	DBSKIP()
ENDDO

// Close the table
CLOSE TABLE oRTF

MyFile->(DBCLOSEAREA())

RETURN NIL
***********************  END OF DBFToRTF()  *********************





STATIC FUNCTION FlowerColumns( oRTF, cFile )
*********************************************************************
* Description:  Format FLOWERS.DBF memo data as prose in snaking
*               columns.
* Arguments:    
* Return:       
*               
*--------------------------------------------------------------------
* Date       Developer   Comments
* 01/28/97   TRM         Creation
*
*********************************************************************
LOCAL cText, i := 0

USE (cFile) ALIAS MyFile NEW EXCLUSIVE

oRTF:lTrimSpaces := .F.

// Begin a new section of the document, in order
// to switch page orientation back to portrait.

NEW SECTION oRTF ;
	COLUMNS 2 ;
	PAGEWIDTH 8.5 ;
	PAGEHEIGHT 11 ;
	MARGINS .75, .75, 1, 1 ;
	ALIGN TOP ;
	SETDEFAULT

DO WHILE !EOF()

	++i
	IF i > 1
		NEW PARAGRAPH oRTF TEXT ""
	ENDIF

	NEW PARAGRAPH oRTF TEXT RTRIM(MyFile->Name) + ".  " ;
		FONTNUMBER 1 ;
		FONTSIZE 14 ;
		APPEARANCE BOLD_ON + ITALIC_ON ;
		ALIGN JUSTIFY ;
		SPACEBEFORE IIF( i == 1, 0, .4 ) ;
		SETDEFAULT ;
		TOCLEVEL 2 ;
		NORETURN

	cText := MyFile->Descriptio

	NEW PARAGRAPH oRTF TEXT cText ;
		APPEARANCE BOLD_OFF + ITALIC_OFF ;
		NORETURN

	DBSKIP()

ENDDO

MyFile->(DBCLOSEAREA())

RETURN NIL
***********************  END OF FlowerColumns()  *********************







FUNCTION MergeDemo()
*********************************************************************
* Description:  Demonstrate capabilities of RTFMerge() function
* Arguments:    
* Return:       
*               
*--------------------------------------------------------------------
* Date       Developer   Comments
* 02/03/97   TRM         Creation
* 02/05/97   TRM         It finally does something...
*
*********************************************************************
LOCAL cInFile := "MERGEIN.RTF"
LOCAL cOutFile := "MERGEOUT.RTF"
LOCAL hOutFile
LOCAL cHeader, nLenHead
LOCAL cFile := "VEGGIES.DBF"

USE (cFile) ALIAS MyFile NEW EXCLUSIVE

// Identify the header length of the primary merge file,
// because we need to handle the header separately
cHeader := RTFHeader( cInFile )
nLenHead := LEN( cHeader )

// First create the output file and transfer the header to it.
hOutFile := FCREATE(cOutFile)
FWRITE( hOutFile, cHeader )

// Now merge away!
// (only process 3 records for demo purposes)
DO WHILE RECNO() < 4

	RTFMerge( cInFile, nLenHead, hOutFile )

	// Write a hard page break to the file
	FWRITE( hOutFile, "\par\page" + CRLF )

	DBSKIP()

ENDDO

FWRITE( hOutFile, "}" )
FCLOSE( cOutFile )

MyFile->(DBCLOSEAREA())

RETURN NIL
*********************** END OF MergeDemo()  *************************




*------------------------------------------------------------------------------*
FUNCTION CreateProgressBar( cTitle )
*------------------------------------------------------------------------------*

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
      VALUE ''            ;
      CENTERALIGN VCENTERALIGN

   @ 105, 20 PROGRESSBAR PrgBar_1 ;
      RANGE 0, 100 ;
      VALUE 0      ;
      WIDTH 380 HEIGHT 34

   END WINDOW

   Form_PrgBar.Center
   Activate Window Form_PrgBar NoWait

RETURN NIL

*------------------------------------------------------------------------------*
FUNCTION CloseProgressBar()
*------------------------------------------------------------------------------*

   IF IsWindowDefined( Form_PrgBar )
      Form_PrgBar.Release
   ENDIF

   DO MESSAGE LOOP

RETURN NIL
