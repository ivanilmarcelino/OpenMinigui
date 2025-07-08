Modified command:
@ <Row>, <Col> HPDFPRINT [DATE] <xData> TO <nToRow>, <nToCol>
	[FONT <cFontName>]
	[SIZE <nFontSize>]
	[BOLD [IF lBold]]
	[ITALIC [IF lItalic]]
	[UNDERLINE [IF lUnderline]]
	[STRIKEOUT [IF lStrikeout]]
	[COLOR <aColor>]
	[RIGHT | CENTER | JUSTIFY]
	[WRAP]
	[FONTSIZEFIT | HEIGHTFIT]
	[GETBOTTOM <@nBottRowVar> ]

WRAP option: word-wrap even when there is no split character.
FONTSIZEFIT option: adjusting the font size (by reducing) so that all content fits within the declared area.
HEIGHTFIT option: adjusting the height of the area depending on: content, font, font size and set line spacing.
The FONTSIZEFIT and HEIGHTFIT options can not be used simultaneously.
Option GETBOTTOM <@nBottRowVar>: the variable <nBottRowVar> will get the value of the position of the bottom row of the printed area by reference. Value expressed in mm.

New feature:
_HMG_HPDF_GetHeight_MULTILINE_PRINT (<cText>, [<nLenght>], [<cFontName>], [<nFontSize>], [<lBold>], [<lItalic>], [<lWrap>]) --> nAreaHeight

Returns the calculated height of the printed area. Value expressed in mm. The height of the area depends on the function parameters and on the set line spacing ( SET HPDFPAGE LINESPACING TO <nSpacing> )
Arguments:
<cText> - printed content
<nLenght> - length of the line in mm (width of the area)
<cFontName> - font name
<nFontSize> - font size
<lBold> - .T. if the text is in bold
<lItalic> - .T. if the text is italic
<lWrap> - .T. if the word-wrap, even if there is no split character.
