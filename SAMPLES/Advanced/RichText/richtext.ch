/*
ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
³        Class: RichText                                                   ³
³  Description: System for generating RTF output files                     ³
³     Language: Clipper/Fivewin                                            ³
³      Version: 1.00 -- This is a usable, but abridged, version.           ³
³               A more capable version will be made available for a        ³
³               reasonable cost.                                           ³
³         Date: 10/24/97                                                   ³
³       Author: Tom Marchione                                              ³
³     Internet: tmarchione@compuserve.com                                  ³
³                                                                          ³
³    Copyright: (C) 1997, Thomas R. Marchione                              ³
³                                                                          ³
³   Warranties: None. The code has not been rigorously tested in a formal  ³
³               development environment, and is offered as-is.  The author ³
³               assumes no responsibility for its use, or for any          ³
³               consequences that may arise from its use.                  ³
³                                                                          ³
³    Revisions:                                                            ³
³                                                                          ³
³    DATE       AUTHOR  COMMENTS                                           ³
³ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ³
³    (see class code for change summary)                                   ³
ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
*/


#define INCH_TO_TWIP 1440

// Text Styles
// These are special cases, defined with the RTF backslash, so that
// they can be easily concatenated at the application level.

#define BOLD_ON       "\b"
#define ITALIC_ON     "\i"
#define UNDERLINE_ON  "\ul"
#define CAPS_ON       "\caps"

#define STYLE_OFF     "0"
#define BOLD_OFF      BOLD_ON + STYLE_OFF
#define ITALIC_OFF    ITALIC_ON + STYLE_OFF
#define UNDERLINE_OFF UNDERLINE_ON + STYLE_OFF
#define CAPS_OFF      CAPS_ON + STYLE_OFF


// DEFAULTS:

// Font:        Courier New
// Font Size:   12 Point
// Units:       Inches (i.e., same as "TWIPFACTOR INCH_TO_TWIP")
// Page Setup:  Standard RTF file format defaults


#xcommand DEFINE RTF [<oRTF>] ;
		[ <filename: FILE, FILENAME> <cFileName> ] ;
		[ <fontname: FONTS, FONTNAMES> <aFontData,...> ] ;
		[ FONTSIZE <nFontSize> ] ;
		[ TWIPFACTOR <nScale> ] ;
		[ HIGHORDERMAP <aHigh> ] ;
		[ <lWarn: WARNOVERWRITE> ] ; // added 10/8/97
		[ <lGetFile: GETFILENAME> FORCEPATH <cPath> ] ; // added 10/8/97
	=> ;
		[ <oRTF> := ] RichText():New( <cFileName>, [\{<aFontData>\}], ;
					<nFontSize>, <nScale>, <aHigh>, <.lWarn.>, ;
					<.lGetFile.>, <cPath> )

#xcommand CLOSE RTF <oRTF> => <oRTF>:End()


// If used, DEFINE PAGESETUP should come immediately after DEFINE RTF
// NOTE: Page numbering is not supported yet in base class

#xcommand DEFINE PAGESETUP <oRTF> ;
		[ MARGINS <nLeft>, <nRight>, <nTop>, <nBottom> ] ;
		[ PAGEWIDTH <nWidth> ] ;
		[ PAGEHEIGHT <nHeight> ] ;
		[ TABWIDTH <nTabWidth> ] ;
		[ <landscape: LANDSCAPE> ] ;
		[ <lNoWidow: NOWIDOW> ] ;
		[ ALIGN <vertalign: TOP, BOTTOM, CENTER, JUSTIFY> ] ;
		[ PAGENUMBERS <cPgnumPos: LEFT, RIGHT, CENTER> ] ; // not supported
		[ <lPgnumTop: PAGENUMBERTOP> ] ; // not supported
	=> ;
		<oRTF>:PageSetup( <nLeft>, <nRight>, <nTop>, <nBottom>, ;
				 <nWidth>, <nHeight>, <nTabWidth>, <.landscape.>, <.lNoWidow.>, ;
				 <"vertalign"> , <cPgnumPos>, <.lPgnumTop.> )


// Use these to enclose data to be included in headers & footers
#xcommand BEGIN HEADER <oRTF> => <oRTF>:BeginHeader()
#xcommand END HEADER <oRTF> => <oRTF>:EndHeader()

#xcommand BEGIN FOOTER <oRTF> => <oRTF>:BeginFooter()
#xcommand END FOOTER <oRTF> => <oRTF>:EndFooter()



// Use this to write formatted text within a paragraph
#xcommand WRITE TEXT <oRTF> ;
		[ TEXT <cText> ] ;
		[ FONTNUMBER <nFontNumber> ] ;
		[ FONTSIZE <nFontSize> ] ;
		[ APPEARANCE <cAppear> ] ;
		[ <lDefault: SETDEFAULT > ] ; // This is very quirky in this version...
	=> ;
		<oRTF>:Paragraph( <cText>, <nFontNumber>, <nFontSize>, <cAppear>, ;
				,,,,,,,,,,,,,, <.lDefault.>, .T. )

// Use this to write an entire paragraph, with optional formatting.
#xcommand NEW PARAGRAPH <oRTF> ;
		[ TEXT <cText> ] ;
		[ FONTNUMBER <nFontNumber> ] ;
		[ FONTSIZE <nFontSize> ] ;
		[ APPEARANCE <cAppear> ] ;
		[ ALIGN <cHorzAlign: LEFT, RIGHT, CENTER, JUSTIFY> ] ;
		[ TABSTOPS <aTabPos,...> ] ;
		[ <indent: INDENT, LEFTINDENT> <nIndent> ] ;
		[ FIRSTINDENT <nFIndent> ] ;
		[ RIGHTINDENT <nRIndent> ] ;
		[ LINESPACE <nSpace> [ <lSpExact: ABSOLUTE>] ] ;
		[ SPACEBEFORE <nBefore> ] ;
		[ SPACEAFTER <nAfter> ] ;
		[ <lNoWidow: NOWIDOW> ] ;
		[ <lBreak: NEWPAGE > ] ;
		[ <lBullet: BULLET, BULLETED > [ BULLETCHAR <cBulletChar> ];
			[ HANGING <lHang> ] ] ;
		[ <lDefault: SETDEFAULT > ] ;
		[ <lNoPar: NORETURN> ] ; // This can have uneven results.
		[ TOCLEVEL <nTCLevel> ] ;
	=> ;
		<oRTF>:Paragraph( <cText>, <nFontNumber>, <nFontSize>, <cAppear>, ;
				<"cHorzAlign">, [\{<aTabPos>\}], <nIndent>, ;
				<nFIndent>, <nRIndent>, <nSpace>, <.lSpExact.>, ;
				<nBefore>, <nAfter> , <.lNoWidow.>, <.lBreak.>, ;
				<.lBullet.>, <cBulletChar>, <.lHang.>, <.lDefault.>, <.lNoPar.>, ;
				<nTCLevel> )


// Use this to begin a new table
#xcommand DEFINE TABLE <oRTF> ;
		[ ALIGN <cHorzAlign: LEFT, RIGHT, CENTER> ] ;
		[ FONTNUMBER <nFontNumber> ] ;
		[ FONTSIZE <nFontSize> ] ;
		[ CELLAPPEAR <cCellAppear> ] ;
		[ CELLHALIGN <cCellHAlign: LEFT, RIGHT, CENTER> ] ;
		[ ROWS <nRows> ] ;
		[ COLUMNS <nColumns> ] ;
		[ CELLWIDTHS <aColWidths,...> ] ;
		[ ROWHEIGHT <nHeight> ] ;
		[ ROWBORDERS <cRowBorder: SINGLE, DOUBLETHICK, SHADOW, DOUBLE, ;
			DOTTED, DASHED, HAIRLINE > ] ;
		[ CELLBORDERS <cCellBorder: SINGLE, DOUBLETHICK, SHADOW, DOUBLE, ;
			DOTTED, DASHED, HAIRLINE > ] ;
		[ CELLSHADE <nCellPct> ] ;
		[ <lNoSplit: NOSPLIT> ] ;
		[ HEADERROWS <nHeadRows> ;
			[ HEADERHEIGHT <nHeadHgt> ] ;
			[ HEADERSHADE <nHeadPct> ] ;
			[ HEADERFONT <nHeadFont> ] ;
			[ HEADERFONTSIZE <nHFontSize> ] ;
			[ HEADERAPPEAR <cHeadAppear> ] ;
			[ HEADERHALIGN <cHeadHAlign: LEFT, RIGHT, CENTER> ] ;
		] ;
	=> ;
		<oRTF>:DefineTable( <"cHorzAlign">, <nFontNumber>, <nFontSize>, ;
				<cCellAppear>, <"cCellHAlign">, <nRows>, ;
				<nColumns>, <nHeight>, [\{<aColWidths>\}], <"cRowBorder">, ;
				<"cCellBorder">, <nCellPct>, <.lNoSplit.>, <nHeadRows>, ;
				<nHeadHgt>, <nHeadPct>, <nHeadFont>, <nHFontSize>, ;
				<cHeadAppear>, <"cHeadHAlign"> )


#xcommand CLOSE TABLE oRTF => oRTF:EndRow() ; oRTF:TextCode("pard")


// Use this to begin/end a row of the table
// NOTE: After the first row, the class will automatically
// start new rows as necessary, based on # of columns

#xcommand BEGIN ROW oRTF => oRTF:BeginRow()
#xcommand END ROW oRTF => oRTF:EndRow()


// Use this to write the next cell in a table
#xcommand WRITE CELL <oRTF> ;
		[ TEXT <cText> ] ;
		[ FONTNUMBER <nFontNumber> ] ;
		[ FONTSIZE <nFontSize> ] ;
		[ APPEARANCE <cAppear> ] ;
		[ ALIGN <cHorzAlign: LEFT, RIGHT, CENTER, JUSTIFY> ] ;
		[ LINESPACE <nSpace> [ <lSpExact: ABSOLUTE>] ] ;
		[ CELLBORDERS <cCellBorder: SINGLE, DOUBLETHICK, SHADOW, DOUBLE, ;
			DOTTED, DASHED, HAIRLINE > ] ;
		[ CELLSHADE <nCellPct> ] ;
		[ <lDefault: SETDEFAULT > ] ;
	=> ;
		<oRTF>:WriteCell( <cText>, <nFontNumber>, <nFontSize>, <cAppear>, ;
				<"cHorzAlign">, <nSpace>, <lSpExact>, <"cCellBorder">, ;
				<nCellPct>, <.lDefault.> )


// Use this to begin a new section -- for example, to change the page
// orientation, or the paper size, or the number of columns.

#xcommand NEW SECTION oRTF ;
		[ <landscape: LANDSCAPE> ] ;
		[ COLUMNS <nColumns> ] ;
		[ MARGINS <nLeft>, <nRight>, <nTop>, <nBottom> ] ;
		[ PAGEWIDTH <nWidth> ] ;
		[ PAGEHEIGHT <nHeight> ] ;
		[ ALIGN <vertalign: TOP, BOTTOM, CENTER, JUSTIFY> ] ;
		[ <lDefault: SETDEFAULT > ] ;
	=> ;
		oRTF:NewSection( <.landscape.>, <nColumns>, ;
				<nLeft>, <nRight>, <nTop>, <nBottom>, ;
				<nWidth>, <nHeight>, <"vertalign">, <.lDefault.> )


// Use this to insert a page number (a "field" in MS-Word)
// Good for use within headers and footers
// NOTE: alignment not supported yet.
#xcommand INSERT PAGENUMBER <oRTF> ;
		[ ALIGN <cHorzAlign: LEFT, RIGHT, CENTER> ] ;
	=> ;
		<oRTF>:PageNumber(<"cHorzAlign">)

