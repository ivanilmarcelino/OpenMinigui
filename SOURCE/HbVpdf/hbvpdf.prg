#include "hbvpdf.ch"
#include "fileio.ch"

// Compatibility macro for default NIL handling
#if defined( __XHARBOUR__ ) .OR. ( __HARBOUR__ - 0 < 0x030200 )
# xtranslate __defaultNIL( @<v>, <x> ) => ( <v> := iif( <v> == NIL, <x>, <v> ) )
#endif

// Define a constant for carriage return and line feed
#define CRLF ( Chr( 13 ) + Chr( 10 ) )

// Static array to hold report data
STATIC s_aReport

/*
 * Function: pdfInit
 * Description: Initializes the PDF report array.
 * Returns: The initialized report array.
 */
FUNCTION pdfInit()
   s_aReport := Array( PARAMLEN )

RETURN s_aReport

/*
 * Function: pdfWidth
 * Description: Sets the width of the report.
 * Parameters:
 *   _nWidth - The width to set.
 * Returns: NIL
 */
FUNCTION pdfWidth( _nWidth )
   s_aReport[ REPORTWIDTH ] := _nWidth

RETURN NIL

/*
 * Function: pdfTextWidth
 * Description: Calculates the width of a string in the PDF.
 * Parameters:
 *   cStr - The string to calculate the width for.
 * Returns: The width of the string.
 */
FUNCTION pdfTextWidth( cStr )
RETURN pdfLen( cStr ) / 25.4

/*
 * Function: pdfAtSay
 * Description: Places a string at a specific position in the PDF.
 * Parameters:
 *   cString - The string to place.
 *   nRow - The row position.
 *   nCol - The column position.
 *   cUnits - The units for the position.
 *   lExact - Whether to use exact positioning.
 *   cId - An identifier for the string.
 * Returns: NIL
 */
FUNCTION pdfAtSay( cString, nRow, nCol, cUnits, lExact, cId )

   LOCAL _nFont, lReverse, nAt
   __defaultNIL( @nRow, s_aReport[ REPORTLINE ] )
   __defaultNIL( @cUnits, "R" )
   __defaultNIL( @lExact, .F. )
   __defaultNIL( @cId, "" )

   IF s_aReport[ HEADEREDIT ]
      RETURN pdfHeader( "PDFATSAY", cId, { cString, nRow, nCol, cUnits, lExact } )
   ENDIF

   // Replace page number placeholder with actual page number
   IF ( nAt := At( "#pagenumber#", cString ) ) > 0
      cString := Left( cString, nAt - 1 ) + hb_ntos( pdfPageNumber() ) + SubStr( cString, nAt + 12 )
   ENDIF

   lReverse := .F.

   // Convert units if necessary
   IF cUnits == "M"
      nRow := pdfM2Y( nRow )
      nCol := pdfM2X( nCol )
   ELSEIF cUnits == "R"
      IF ! lExact
         pdfCheckLine( nRow )
         nRow := nRow + s_aReport[ PDFTOP ]
      ENDIF
      nRow := pdfR2D( nRow )
      nCol := pdfM2X( s_aReport[ PDFLEFT ] ) + ;
         nCol * 100.00 / s_aReport[ REPORTWIDTH ] * ;
         ( s_aReport[ PAGEX ] - pdfM2X( s_aReport[ PDFLEFT ] ) * 2 - 9.0 ) / 100.00
   ENDIF

   // Process the string if it's not empty
   IF ! Empty( cString )
      cString := pdfStringB( cString )

      // Handle reverse and underline formatting
      IF Right( cString, 1 ) == Chr( 255 ) // reverse
         cString := Left( cString, Len( cString ) - 1 )
         pdfBox( s_aReport[ PAGEY ] - nRow - s_aReport[ FONTSIZE ] + 2.0, nCol, s_aReport[ PAGEY ] - nRow + 2.0, nCol + pdfM2X( pdfLen( cString ) ) + 1,, 100, "D" )
         s_aReport[ PAGEBUFFER ] += " 1 g "
         lReverse := .T.
      ELSEIF Right( cString, 1 ) == Chr( 254 ) // underline
         cString := Left( cString, Len( cString ) - 1 )
         pdfBox( s_aReport[ PAGEY ] - nRow + 0.5, nCol, s_aReport[ PAGEY ] - nRow + 1, nCol + pdfM2X( pdfLen( cString ) ) + 1,, 100, "D" )
      ENDIF

      // Handle color text
      IF ( nAt := At( Chr( 253 ), cString ) ) > 0 // some color text inside
         s_aReport[ PAGEBUFFER ] += CRLF + ;
            Chr_RGB( SubStr( cString, nAt + 1, 1 ) ) + " " + ;
            Chr_RGB( SubStr( cString, nAt + 2, 1 ) ) + " " + ;
            Chr_RGB( SubStr( cString, nAt + 3, 1 ) ) + " rg "
         cString := Stuff( cString, nAt, 4, "" )
      ENDIF

      // Handle font settings
      _nFont := AScan( s_aReport[ FONTS ], {| arr | arr[ 1 ] == s_aReport[ FONTNAME ] } )
      IF !( s_aReport[ FONTNAME ] == s_aReport[ FONTNAMEPREV ] )
         s_aReport[ FONTNAMEPREV ] := s_aReport[ FONTNAME ]
         s_aReport[ PAGEBUFFER ] += CRLF + "BT /Fo" + hb_ntos( _nFont ) + " " + LTrim( Transform( s_aReport[ FONTSIZE ], "999.99" ) ) + " Tf " + LTrim( Transform( nCol, "9999.99" ) ) + " " + LTrim( Transform( nRow, "9999.99" ) ) + " Td (" + cString + ") Tj ET"
      ELSEIF s_aReport[ FONTSIZE ] != s_aReport[ FONTSIZEPREV ]
         s_aReport[ FONTSIZEPREV ] := s_aReport[ FONTSIZE ]
         s_aReport[ PAGEBUFFER ] += CRLF + "BT /Fo" + hb_ntos( _nFont ) + " " + LTrim( Transform( s_aReport[ FONTSIZE ], "999.99" ) ) + " Tf " + LTrim( Transform( nCol, "9999.99" ) ) + " " + LTrim( Transform( nRow, "9999.99" ) ) + " Td (" + cString + ") Tj ET"
      ELSE
         s_aReport[ PAGEBUFFER ] += CRLF + "BT " + LTrim( Transform( nCol, "9999.99" ) ) + " " + LTrim( Transform( nRow, "9999.99" ) ) + " Td (" + cString + ") Tj ET"
      ENDIF

      IF lReverse
         s_aReport[ PAGEBUFFER ] += " 0 g "
      ENDIF
   ENDIF

RETURN NIL

/*
 * Function: pdfBold
 * Description: Sets the font to bold.
 * Returns: NIL
 */
FUNCTION pdfBold()
   IF pdfGetFontInfo( "NAME" ) == "Times"
      s_aReport[ FONTNAME ] := 2
   ELSEIF pdfGetFontInfo( "NAME" ) == "Helvetica"
      s_aReport[ FONTNAME ] := 6
   ELSE
      s_aReport[ FONTNAME ] := 10 // Courier
   ENDIF
   AAdd( s_aReport[ PAGEFONTS ], s_aReport[ FONTNAME ] )
   IF AScan( s_aReport[ FONTS ], {| arr | arr[ 1 ] == s_aReport[ FONTNAME ] } ) == 0
      AAdd( s_aReport[ FONTS ], { s_aReport[ FONTNAME ], ++s_aReport[ NEXTOBJ ] } )
   ENDIF

RETURN NIL

/*
 * Function: pdfBoldItalic
 * Description: Sets the font to bold italic.
 * Returns: NIL
 */
FUNCTION pdfBoldItalic()
   IF pdfGetFontInfo( "NAME" ) == "Times"
      s_aReport[ FONTNAME ] := 4
   ELSEIF pdfGetFontInfo( "NAME" ) == "Helvetica"
      s_aReport[ FONTNAME ] := 8
   ELSE
      s_aReport[ FONTNAME ] := 12
   ENDIF
   AAdd( s_aReport[ PAGEFONTS ], s_aReport[ FONTNAME ] )
   IF AScan( s_aReport[ FONTS ], {| arr | arr[ 1 ] == s_aReport[ FONTNAME ] } ) == 0
      AAdd( s_aReport[ FONTS ], { s_aReport[ FONTNAME ], ++s_aReport[ NEXTOBJ ] } )
   ENDIF

RETURN NIL

/*
 * Function: pdfBookAdd
 * Description: Adds a bookmark to the PDF.
 * Parameters:
 *   cTitle - The title of the bookmark.
 *   nLevel - The level of the bookmark.
 *   nPage - The page number.
 *   nLine - The line number.
 * Returns: NIL
 */
FUNCTION pdfBookAdd( cTitle, nLevel, nPage, nLine )
   AAdd( s_aReport[ BOOKMARK ], { nLevel, AllTrim( cTitle ), 0, 0, 0, 0, 0, 0, nPage, iif( nLevel == 1, s_aReport[ PAGEY ], s_aReport[ PAGEY ] - nLine * 72 / s_aReport[ LPI ] ) } )

RETURN NIL

/*
 * Function: pdfBookClose
 * Description: Closes the bookmark section.
 * Returns: NIL
 */
FUNCTION pdfBookClose()
   s_aReport[ BOOKMARK ] := NIL

RETURN NIL

/*
 * Function: pdfBookCount
 * Description: Counts the number of bookmarks at a specific level.
 * Parameters:
 *   nRecno - The record number.
 *   nCurLevel - The current level.
 * Returns: The count of bookmarks.
 */
STATIC FUNCTION pdfBookCount( nRecno, nCurLevel )

   LOCAL nTempLevel, nCount := 0, nLen := Len( s_aReport[ BOOKMARK ] )
   ++nRecno
   WHILE nRecno <= nLen
      nTempLevel := s_aReport[ BOOKMARK ][ nRecno ][ BOOKLEVEL ]
      IF nTempLevel <= nCurLevel
         EXIT
      ELSE
         IF nCurLevel + 1 == nTempLevel
            ++nCount
         ENDIF
      ENDIF
      ++nRecno
   ENDDO

RETURN -1 * nCount

/*
 * Function: pdfBookFirst
 * Description: Finds the first bookmark at a specific level.
 * Parameters:
 *   nRecno - The record number.
 *   nCurLevel - The current level.
 *   nObj - The object number.
 * Returns: The object number of the first bookmark.
 */
STATIC FUNCTION pdfBookFirst( nRecno, nCurLevel, nObj )

   LOCAL nFirst := 0, nLen := Len( s_aReport[ BOOKMARK ] )
   ++nRecno
   IF nRecno <= nLen
      IF nCurLevel + 1 == s_aReport[ BOOKMARK ][ nRecno ][ BOOKLEVEL ]
         nFirst := nRecno
      ENDIF
   ENDIF

RETURN iif( nFirst == 0, nFirst, nObj + nFirst )

/*
 * Function: pdfBookLast
 * Description: Finds the last bookmark at a specific level.
 * Parameters:
 *   nRecno - The record number.
 *   nCurLevel - The current level.
 *   nObj - The object number.
 * Returns: The object number of the last bookmark.
 */
STATIC FUNCTION pdfBookLast( nRecno, nCurLevel, nObj )

   LOCAL nLast := 0, nLen := Len( s_aReport[ BOOKMARK ] )
   ++nRecno
   IF nRecno <= nLen
      IF nCurLevel + 1 == s_aReport[ BOOKMARK ][ nRecno ][ BOOKLEVEL ]
         WHILE nRecno <= nLen .AND. nCurLevel + 1 <= s_aReport[ BOOKMARK ][ nRecno ][ BOOKLEVEL ]
            IF nCurLevel + 1 == s_aReport[ BOOKMARK ][ nRecno ][ BOOKLEVEL ]
               nLast := nRecno
            ENDIF
            ++nRecno
         ENDDO
      ENDIF
   ENDIF

RETURN iif( nLast == 0, nLast, nObj + nLast )

/*
 * Function: pdfBookNext
 * Description: Finds the next bookmark at the same level.
 * Parameters:
 *   nRecno - The record number.
 *   nCurLevel - The current level.
 *   nObj - The object number.
 * Returns: The object number of the next bookmark.
 */
STATIC FUNCTION pdfBookNext( nRecno, nCurLevel, nObj )

   LOCAL nTempLevel, nNext := 0, nLen := Len( s_aReport[ BOOKMARK ] )
   ++nRecno
   WHILE nRecno <= nLen
      nTempLevel := s_aReport[ BOOKMARK ][ nRecno ][ BOOKLEVEL ]
      IF nCurLevel > nTempLevel
         EXIT
      ELSEIF nCurLevel == nTempLevel
         nNext := nRecno
         EXIT
      ELSE
         // keep going
      ENDIF
      ++nRecno
   ENDDO

RETURN iif( nNext == 0, nNext, nObj + nNext )

/*
 * Function: pdfBookOpen
 * Description: Opens the bookmark section.
 * Returns: NIL
 */
FUNCTION pdfBookOpen()
   s_aReport[ BOOKMARK ] := {}

RETURN NIL

/*
 * Function: pdfBookParent
 * Description: Finds the parent bookmark.
 * Parameters:
 *   nRecno - The record number.
 *   nCurLevel - The current level.
 *   nObj - The object number.
 * Returns: The object number of the parent bookmark.
 */
STATIC FUNCTION pdfBookParent( nRecno, nCurLevel, nObj )

   LOCAL nTempLevel
   LOCAL nParent := 0
   --nRecno
   WHILE nRecno > 0
      nTempLevel := s_aReport[ BOOKMARK ][ nRecno ][ BOOKLEVEL ]
      IF nTempLevel < nCurLevel
         nParent := nRecno
         EXIT
      ENDIF
      --nRecno
   ENDDO

RETURN iif( nParent == 0, nObj - 1, nObj + nParent )

/*
 * Function: pdfBookPrev
 * Description: Finds the previous bookmark at the same level.
 * Parameters:
 *   nRecno - The record number.
 *   nCurLevel - The current level.
 *   nObj - The object number.
 * Returns: The object number of the previous bookmark.
 */
STATIC FUNCTION pdfBookPrev( nRecno, nCurLevel, nObj )

   LOCAL nTempLevel
   LOCAL nPrev := 0
   --nRecno
   WHILE nRecno > 0
      nTempLevel := s_aReport[ BOOKMARK ][ nRecno ][ BOOKLEVEL ]
      IF nCurLevel > nTempLevel
         EXIT
      ELSEIF nCurLevel == nTempLevel
         nPrev := nRecno
         EXIT
      ELSE
         // keep going
      ENDIF
      --nRecno
   ENDDO

RETURN iif( nPrev == 0, nPrev, nObj + nPrev )

/*
 * Function: pdfBox
 * Description: Draws a box in the PDF.
 * Parameters:
 *   x1 - The x-coordinate of the top-left corner.
 *   y1 - The y-coordinate of the top-left corner.
 *   x2 - The x-coordinate of the bottom-right corner.
 *   y2 - The y-coordinate of the bottom-right corner.
 *   nBorder - The border width.
 *   nShade - The shading percentage.
 *   cUnits - The units for the coordinates.
 *   cColor - The color of the box.
 *   cId - An identifier for the box.
 * Returns: NIL
 */
FUNCTION pdfBox( x1, y1, x2, y2, nBorder, nShade, cUnits, cColor, cId )

   LOCAL cBoxColor

   __defaultNIL( @nBorder, 0 )
   __defaultNIL( @nShade, 0 )
   __defaultNIL( @cUnits, "M" )
   __defaultNIL( @cColor, "" )

   // Set box color
   cBoxColor := ""
   IF ! Empty( cColor )
      cBoxColor := " " + Chr_RGB( SubStr( cColor, 2, 1 ) ) + " " + ;
         Chr_RGB( SubStr( cColor, 3, 1 ) ) + " " + ;
         Chr_RGB( SubStr( cColor, 4, 1 ) ) + " rg "
      IF Empty( AllTrim( cBoxColor ) )
         cBoxColor := ""
      ENDIF
   ENDIF

   // Handle header edit mode
   IF s_aReport[ HEADEREDIT ]
      RETURN pdfHeader( "PDFBOX", cId, { x1, y1, x2, y2, nBorder, nShade, cUnits } )
   ENDIF

   // Draw the box based on units
   IF cUnits == "M"
      y1 += 0.5
      y2 += 0.5
      IF nShade > 0
         s_aReport[ PAGEBUFFER ] += CRLF + Transform( 1.00 - nShade / 100.00, "9.99" ) + " g " + cBoxColor + hb_ntos( pdfM2X( y1 ) ) + " " + hb_ntos( pdfM2Y( x1 ) ) + " " + hb_ntos( pdfM2X( y2 - y1 ) ) + " -" + hb_ntos( pdfM2X( x2 - x1 ) ) + " re f 0 g"
      ENDIF
      IF nBorder > 0
         s_aReport[ PAGEBUFFER ] += CRLF + "0 g " + hb_ntos( pdfM2X( y1 ) ) + " " + hb_ntos( pdfM2Y( x1 ) ) + " " + hb_ntos( pdfM2X( y2 - y1 ) ) + " -" + hb_ntos( pdfM2X( nBorder ) ) + " re f"
         s_aReport[ PAGEBUFFER ] += CRLF + "0 g " + hb_ntos( pdfM2X( y2 - nBorder ) ) + " " + hb_ntos( pdfM2Y( x1 ) ) + " " + hb_ntos( pdfM2X( nBorder ) ) + " -" + hb_ntos( pdfM2X( x2 - x1 ) ) + " re f"
         s_aReport[ PAGEBUFFER ] += CRLF + "0 g " + hb_ntos( pdfM2X( y1 ) ) + " " + hb_ntos( pdfM2Y( x2 - nBorder ) ) + " " + hb_ntos( pdfM2X( y2 - y1 ) ) + " -" + hb_ntos( pdfM2X( nBorder ) ) + " re f"
         s_aReport[ PAGEBUFFER ] += CRLF + "0 g " + hb_ntos( pdfM2X( y1 ) ) + " " + hb_ntos( pdfM2Y( x1 ) ) + " " + hb_ntos( pdfM2X( nBorder ) ) + " -" + hb_ntos( pdfM2X( x2 - x1 ) ) + " re f"
      ENDIF
   ELSEIF cUnits == "D"// "Dots"
      IF nShade > 0
         s_aReport[ PAGEBUFFER ] += CRLF + Transform( 1.00 - nShade / 100.00, "9.99" ) + " g " + cBoxColor + hb_ntos( y1 ) + " " + hb_ntos( s_aReport[ PAGEY ] - x1 ) + " " + hb_ntos( y2 - y1 ) + " -" + hb_ntos( x2 - x1 ) + " re f 0 g"
      ENDIF
      IF nBorder > 0
         s_aReport[ PAGEBUFFER ] += CRLF + "0 g " + hb_ntos( y1 ) + " " + hb_ntos( s_aReport[ PAGEY ] - x1 ) + " " + hb_ntos( y2 - y1 ) + " -" + hb_ntos( nBorder ) + " re f"
         s_aReport[ PAGEBUFFER ] += CRLF + "0 g " + hb_ntos( y2 - nBorder ) + " " + hb_ntos( s_aReport[ PAGEY ] - x1 ) + " " + hb_ntos( nBorder ) + " -" + hb_ntos( x2 - x1 ) + " re f"
         s_aReport[ PAGEBUFFER ] += CRLF + "0 g " + hb_ntos( y1 ) + " " + hb_ntos( s_aReport[ PAGEY ] - x2 + nBorder ) + " " + hb_ntos( y2 - y1 ) + " -" + hb_ntos( nBorder ) + " re f"
         s_aReport[ PAGEBUFFER ] += CRLF + "0 g " + hb_ntos( y1 ) + " " + hb_ntos( s_aReport[ PAGEY ] - x1 ) + " " + hb_ntos( nBorder ) + " -" + hb_ntos( x2 - x1 ) + " re f"
      ENDIF
   ENDIF

RETURN NIL

/*
 * Function: pdfBox1
 * Description: Draws a box with specific border and fill colors.
 * Parameters:
 *   nTop - The top position.
 *   nLeft - The left position.
 *   nBottom - The bottom position.
 *   nRight - The right position.
 *   nBorderWidth - The border width.
 *   cBorderColor - The border color.
 *   cBoxColor - The fill color.
 * Returns: NIL
 */
FUNCTION pdfBox1( nTop, nLeft, nBottom, nRight, nBorderWidth, cBorderColor, cBoxColor )

   __defaultNIL( @nBorderWidth, 0.5 )
   __defaultNIL( @cBorderColor, Chr( 0 ) + Chr( 0 ) + Chr( 0 ) )
   __defaultNIL( @cBoxColor, Chr( 255 ) + Chr( 255 ) + Chr( 255 ) )

   s_aReport[ PAGEBUFFER ] += CRLF + ;
      Chr_RGB( SubStr( cBorderColor, 1, 1 ) ) + " " + ;
      Chr_RGB( SubStr( cBorderColor, 2, 1 ) ) + " " + ;
      Chr_RGB( SubStr( cBorderColor, 3, 1 ) ) + ;
      " RG" + ;
      CRLF + ;
      Chr_RGB( SubStr( cBoxColor, 1, 1 ) ) + " " + ;
      Chr_RGB( SubStr( cBoxColor, 2, 1 ) ) + " " + ;
      Chr_RGB( SubStr( cBoxColor, 3, 1 ) ) + ;
      " rg" + ;
      CRLF + hb_ntos( nBorderWidth ) + " w" + ;
      CRLF + hb_ntos( nLeft + nBorderWidth / 2 ) + " " + ;
      CRLF + hb_ntos( s_aReport[ PAGEY ] - nBottom + nBorderWidth / 2 ) + " " + ;
      CRLF + hb_ntos( nRight - nLeft - nBorderWidth ) + ;
      CRLF + hb_ntos( nBottom - nTop - nBorderWidth ) + " " + ;
      " re" + ;
      CRLF + "B"

RETURN NIL

/*
 * Function: pdfCenter
 * Description: Centers a string at a specific position in the PDF.
 * Parameters:
 *   cString - The string to center.
 *   nRow - The row position.
 *   nCol - The column position.
 *   cUnits - The units for the position.
 *   lExact - Whether to use exact positioning.
 *   cId - An identifier for the string.
 * Returns: NIL
 */
FUNCTION pdfCenter( cString, nRow, nCol, cUnits, lExact, cId )

   LOCAL nLen, nAt

   __defaultNIL( @nRow, s_aReport[ REPORTLINE ] )
   __defaultNIL( @cUnits, "R" )
   __defaultNIL( @lExact, .F. )
   __defaultNIL( @nCol, iif( cUnits == "R", s_aReport[ REPORTWIDTH ] / 2, s_aReport[ PAGEX ] / 72 * 25.4 / 2 ) )

   IF s_aReport[ HEADEREDIT ]
      RETURN pdfHeader( "PDFCENTER", cId, { cString, nRow, nCol, cUnits, lExact } )
   ENDIF

   // Replace page number placeholder with actual page number
   IF ( nAt := At( "#pagenumber#", cString ) ) > 0
      cString := Left( cString, nAt - 1 ) + hb_ntos( pdfPageNumber() ) + SubStr( cString, nAt + 12 )
   ENDIF

   nLen := pdfLen( cString ) / 2

   IF cUnits == "R"
      IF ! lExact
         pdfCheckLine( nRow )
         nRow := nRow + s_aReport[ PDFTOP ]
      ENDIF
   ENDIF

   pdfAtSay( cString, pdfR2M( nRow ), iif( cUnits == "R", s_aReport[ PDFLEFT ] + ( s_aReport[ PAGEX ] / 72 * 25.4 - 2 * s_aReport[ PDFLEFT ] ) * nCol / s_aReport[ REPORTWIDTH ], nCol ) - nLen, "M", lExact )

RETURN NIL

/*
 * Function: pdfCheckLine
 * Description: Checks if a line is within the printable area.
 * Parameters:
 *   nRow - The row to check.
 * Returns: NIL
 */
STATIC FUNCTION pdfCheckLine( nRow )
   IF nRow + s_aReport[ PDFTOP ] > s_aReport[ PDFBOTTOM ]
      pdfNewPage()
      nRow := s_aReport[ REPORTLINE ]
   ENDIF
   s_aReport[ REPORTLINE ] := nRow

RETURN NIL

/*
 * Function: pdfClose
 * Description: Closes the PDF document.
 * Returns: NIL
 */
FUNCTION pdfClose()

   LOCAL nI, cTemp, nCurLevel, nObj1, nLast, nCount, nFirst, nRecno, nBooklen

   pdfClosePage()

   // Kids
   s_aReport[ REFS ][ 2 ] := s_aReport[ DOCLEN ]
   cTemp := ;
      "1 0 obj" + CRLF + ;
      "<<" + CRLF + ;
      "/Type /Pages /Count " + hb_ntos( s_aReport[ REPORTPAGE ] ) + CRLF + ;
      "/Kids ["
   FOR nI := 1 TO s_aReport[ REPORTPAGE ]
      cTemp += " " + hb_ntos( s_aReport[ PAGES ][ nI ] ) + " 0 R"
   NEXT
   cTemp += " ]" + CRLF + ;
      ">>" + CRLF + ;
      "endobj" + CRLF
   s_aReport[ DOCLEN ] += Len( cTemp )
   FWrite( s_aReport[ HANDLE ], cTemp )

   // Info
   ++s_aReport[ REPORTOBJ ]
   AAdd( s_aReport[ REFS ], s_aReport[ DOCLEN ] )
   cTemp := hb_ntos( s_aReport[ REPORTOBJ ] ) + " 0 obj" + CRLF + ;
      "<<" + CRLF + ;
      "/Producer ()" + CRLF + ;
      "/Title ()" + CRLF + ;
      "/Author ()" + CRLF + ;
      "/Creator ()" + CRLF + ;
      "/Subject ()" + CRLF + ;
      "/Keywords ()" + CRLF + ;
      "/CreationDate (D:" + Str( Year( Date() ), 4 ) + PadL( Month( Date() ), 2, "0" ) + PadL( Day( Date() ), 2, "0" ) + SubStr( Time(), 1, 2 ) + SubStr( Time(), 4, 2 ) + SubStr( Time(), 7, 2 ) + ")" + CRLF + ;
      ">>" + CRLF + ;
      "endobj" + CRLF
   s_aReport[ DOCLEN ] += Len( cTemp )
   FWrite( s_aReport[ HANDLE ], cTemp )

   // Root
   ++s_aReport[ REPORTOBJ ]
   AAdd( s_aReport[ REFS ], s_aReport[ DOCLEN ] )
   cTemp := hb_ntos( s_aReport[ REPORTOBJ ] ) + " 0 obj" + CRLF + ;
      "<< /Type /Catalog /Pages 1 0 R /Outlines " + hb_ntos( s_aReport[ REPORTOBJ ] + 1 ) + " 0 R" + iif( ( nBookLen := Len( s_aReport[ BOOKMARK ] ) ) > 0, " /PageMode /UseOutlines", "" ) + " >>" + CRLF + "endobj" + CRLF
   s_aReport[ DOCLEN ] += Len( cTemp )
   FWrite( s_aReport[ HANDLE ], cTemp )

   ++s_aReport[ REPORTOBJ ]
   nObj1 := s_aReport[ REPORTOBJ ]

   IF nBookLen > 0
      nRecno := 1
      nFirst := s_aReport[ REPORTOBJ ] + 1
      nLast := 0
      nCount := 0
      WHILE nRecno <= nBookLen
         nCurLevel := s_aReport[ BOOKMARK ][ nRecno ][ BOOKLEVEL ]
         s_aReport[ BOOKMARK ][ nRecno ][ BOOKPARENT ] := pdfBookParent( nRecno, nCurLevel, s_aReport[ REPORTOBJ ] )
         s_aReport[ BOOKMARK ][ nRecno ][ BOOKPREV ] := pdfBookPrev( nRecno, nCurLevel, s_aReport[ REPORTOBJ ] )
         s_aReport[ BOOKMARK ][ nRecno ][ BOOKNEXT ] := pdfBookNext( nRecno, nCurLevel, s_aReport[ REPORTOBJ ] )
         s_aReport[ BOOKMARK ][ nRecno ][ BOOKFIRST ] := pdfBookFirst( nRecno, nCurLevel, s_aReport[ REPORTOBJ ] )
         s_aReport[ BOOKMARK ][ nRecno ][ BOOKLAST ] := pdfBookLast( nRecno, nCurLevel, s_aReport[ REPORTOBJ ] )
         s_aReport[ BOOKMARK ][ nRecno ][ BOOKCOUNT ] := pdfBookCount( nRecno, nCurLevel )
         IF nCurLevel == 1
            nLast := nRecno
            ++nCount
         ENDIF
         ++nRecno
      ENDDO
      nLast += s_aReport[ REPORTOBJ ]
      cTemp := hb_ntos( s_aReport[ REPORTOBJ ] ) + " 0 obj" + CRLF + "<< /Type /Outlines /Count " + hb_ntos( nCount ) + " /First " + hb_ntos( nFirst ) + " 0 R /Last " + hb_ntos( nLast ) + " 0 R >>" + CRLF + "endobj"
      AAdd( s_aReport[ REFS ], s_aReport[ DOCLEN ] )
      s_aReport[ DOCLEN ] += Len( cTemp )
      FWrite( s_aReport[ HANDLE ], cTemp )
      ++s_aReport[ REPORTOBJ ]
      nRecno := 1
      FOR nI := 1 TO nBookLen
         cTemp := CRLF + hb_ntos( s_aReport[ REPORTOBJ ] + nI - 1 ) + " 0 obj" + CRLF + ;
            "<<" + CRLF + ;
            "/Parent " + hb_ntos( s_aReport[ BOOKMARK ][ nRecno ][ BOOKPARENT ] ) + " 0 R" + CRLF + ;
            "/Dest [" + hb_ntos( s_aReport[ PAGES ][ s_aReport[ BOOKMARK ][ nRecno ][ BOOKPAGE ] ] ) + " 0 R /XYZ 0 " + hb_ntos( s_aReport[ BOOKMARK ][ nRecno ][ BOOKCOORD ] ) + " 0]" + CRLF + ;
            "/Title (" + AllTrim( s_aReport[ BOOKMARK ][ nRecno ][ BOOKTITLE ] ) + ")" + CRLF + ;
            iif( s_aReport[ BOOKMARK ][ nRecno ][ BOOKPREV ] > 0, "/Prev " + hb_ntos( s_aReport[ BOOKMARK ][ nRecno ][ BOOKPREV ] ) + " 0 R" + CRLF, "" ) + ;
            iif( s_aReport[ BOOKMARK ][ nRecno ][ BOOKNEXT ] > 0, "/Next " + hb_ntos( s_aReport[ BOOKMARK ][ nRecno ][ BOOKNEXT ] ) + " 0 R" + CRLF, "" ) + ;
            iif( s_aReport[ BOOKMARK ][ nRecno ][ BOOKFIRST ] > 0, "/First " + hb_ntos( s_aReport[ BOOKMARK ][ nRecno ][ BOOKFIRST ] ) + " 0 R" + CRLF, "" ) + ;
            iif( s_aReport[ BOOKMARK ][ nRecno ][ BOOKLAST ] > 0, "/Last " + hb_ntos( s_aReport[ BOOKMARK ][ nRecno ][ BOOKLAST ] ) + " 0 R" + CRLF, "" ) + ;
            iif( s_aReport[ BOOKMARK ][ nRecno ][ BOOKCOUNT ] != 0, "/Count " + hb_ntos( s_aReport[ BOOKMARK ][ nRecno ][ BOOKCOUNT ] ) + CRLF, "" ) + ;
            ">>" + CRLF + "endobj" + CRLF
         AAdd( s_aReport[ REFS ], s_aReport[ DOCLEN ] + 2 )
         s_aReport[ DOCLEN ] += Len( cTemp )
         FWrite( s_aReport[ HANDLE ], cTemp )
         ++nRecno
      NEXT
      pdfBookClose()
      s_aReport[ REPORTOBJ ] += nBookLen - 1
   ELSE
      cTemp := hb_ntos( s_aReport[ REPORTOBJ ] ) + " 0 obj" + CRLF + "<< /Type /Outlines /Count 0 >>" + CRLF + "endobj" + CRLF
      AAdd( s_aReport[ REFS ], s_aReport[ DOCLEN ] )
      s_aReport[ DOCLEN ] += Len( cTemp )
      FWrite( s_aReport[ HANDLE ], cTemp )
   ENDIF

   cTemp := CRLF
   s_aReport[ DOCLEN ] += Len( cTemp )
   ++s_aReport[ REPORTOBJ ]
   cTemp += "xref" + CRLF + ;
      "0 " + hb_ntos( s_aReport[ REPORTOBJ ] ) + CRLF + ;
      PadL( s_aReport[ REFS ][ 1 ], 10, "0" ) + " 65535 f" + CRLF
   FOR nI := 2 TO Len( s_aReport[ REFS ] )
      cTemp += PadL( s_aReport[ REFS ][ nI ], 10, "0" ) + " 00000 n" + CRLF
   NEXT
   cTemp += "trailer << /Size " + hb_ntos( s_aReport[ REPORTOBJ ] ) + " /Root " + hb_ntos( nObj1 - 1 ) + " 0 R /Info " + hb_ntos( nObj1 - 2 ) + " 0 R >>" + CRLF + ;
      "startxref" + CRLF + ;
      hb_ntos( s_aReport[ DOCLEN ] ) + CRLF + ;
      "%%EOF" + CRLF
   FWrite( s_aReport[ HANDLE ], cTemp )

#if 0
   IF s_aReport[ OPTIMIZE ]
      pdfOptimize() coming !
   ENDIF

#endif

   FClose( s_aReport[ HANDLE ] )
   s_aReport := NIL

RETURN NIL

/*
 * Function: pdfClosePage
 * Description: Closes the current page in the PDF.
 * Returns: NIL
 */
STATIC FUNCTION pdfClosePage()

   LOCAL cTemp, cBuffer, nBuffer, nRead, nI, k, nImage, nFont, nImageHandle

   AAdd( s_aReport[ REFS ], s_aReport[ DOCLEN ] )
   AAdd( s_aReport[ PAGES ], s_aReport[ REPORTOBJ ] + 1 )

   cTemp := ;
      hb_ntos( ++s_aReport[ REPORTOBJ ] ) + " 0 obj" + CRLF + ;
      "<<" + CRLF + ;
      "/Type /Page /Parent 1 0 R" + CRLF + ;
      "/Resources " + hb_ntos( ++s_aReport[ REPORTOBJ ] ) + " 0 R" + CRLF + ;
      "/MediaBox [ 0 0 " + LTrim( Transform( s_aReport[ PAGEX ], "9999.99" ) ) + " " + ;
      LTrim( Transform( s_aReport[ PAGEY ], "9999.99" ) ) + " ]" + CRLF + ;
      "/Contents " + hb_ntos( ++s_aReport[ REPORTOBJ ] ) + " 0 R" + CRLF + ;
      ">>" + CRLF + ;
      "endobj" + CRLF
   s_aReport[ DOCLEN ] += Len( cTemp )
   FWrite( s_aReport[ HANDLE ], cTemp )

   AAdd( s_aReport[ REFS ], s_aReport[ DOCLEN ] )
   cTemp := ;
      hb_ntos( s_aReport[ REPORTOBJ ] - 1 ) + " 0 obj" + CRLF + ;
      "<<" + CRLF + ;
      "/ColorSpace << /DeviceRGB /DeviceGray >>" + CRLF + ;
      "/ProcSet [ /PDF /Text /ImageB /ImageC ]"

   IF Len( s_aReport[ PAGEFONTS ] ) > 0
      cTemp += CRLF + ;
         "/Font" + CRLF + ;
         "<<"
      FOR nI := 1 TO Len( s_aReport[ PAGEFONTS ] )
         nFont := AScan( s_aReport[ FONTS ], {| arr | arr[ 1 ] == s_aReport[ PAGEFONTS ][ nI ] } )
         cTemp += CRLF + "/Fo" + hb_ntos( nFont ) + " " + hb_ntos( s_aReport[ FONTS ][ nFont ][ 2 ] ) + " 0 R"
      NEXT
      cTemp += CRLF + ">>"
   ENDIF

   IF Len( s_aReport[ PAGEIMAGES ] ) > 0
      cTemp += CRLF + "/XObject" + CRLF + "<<"
      FOR nI := 1 TO Len( s_aReport[ PAGEIMAGES ] )
         nImage := AScan( s_aReport[ IMAGES ], {| arr | arr[ 1 ] == s_aReport[ PAGEIMAGES ][ nI ][ 1 ] } )
         IF nImage == 0
            AAdd( s_aReport[ IMAGES ], { s_aReport[ PAGEIMAGES ][ nI ][ 1 ], ++s_aReport[ NEXTOBJ ], pdfImageInfo( s_aReport[ PAGEIMAGES ][ nI ][ 1 ] ) } )
            nImage := Len( s_aReport[ IMAGES ] )
         ENDIF
         cTemp += CRLF + "/Image" + hb_ntos( nImage ) + " " + hb_ntos( s_aReport[ IMAGES ][ nImage ][ 2 ] ) + " 0 R"
      NEXT
      cTemp += CRLF + ">>"
   ENDIF

   cTemp += CRLF + ">>" + CRLF + "endobj" + CRLF
   s_aReport[ DOCLEN ] += Len( cTemp )
   FWrite( s_aReport[ HANDLE ], cTemp )

   AAdd( s_aReport[ REFS ], s_aReport[ DOCLEN ] )
   cTemp := hb_ntos( s_aReport[ REPORTOBJ ] ) + " 0 obj << /Length " + ;
      hb_ntos( s_aReport[ REPORTOBJ ] + 1 ) + " 0 R >>" + CRLF + ;
      "stream"
   s_aReport[ DOCLEN ] += Len( cTemp )
   FWrite( s_aReport[ HANDLE ], cTemp )

   IF Len( s_aReport[ PAGEIMAGES ] ) > 0
      cTemp := ""
      FOR nI := 1 TO Len( s_aReport[ PAGEIMAGES ] )
         cTemp += CRLF + "q"
         nImage := AScan( s_aReport[ IMAGES ], {| arr | arr[ 1 ] == s_aReport[ PAGEIMAGES ][ nI ][ 1 ] } )
         cTemp += CRLF + hb_ntos( iif( s_aReport[ PAGEIMAGES ][ nI ][ 5 ] == 0, pdfM2X( s_aReport[ IMAGES ][ nImage ][ 3 ][ IMAGE_WIDTH ] / s_aReport[ IMAGES ][ nImage ][ 3 ][ IMAGE_XRES ] * 25.4 ), s_aReport[ PAGEIMAGES ][ nI ][ 5 ] ) ) + ;
            " 0 0 " + ;
            hb_ntos( iif( s_aReport[ PAGEIMAGES ][ nI ][ 4 ] == 0, pdfM2X( s_aReport[ IMAGES ][ nImage ][ 3 ][ IMAGE_HEIGHT ] / s_aReport[ IMAGES ][ nImage ][ 3 ][ IMAGE_YRES ] * 25.4 ), s_aReport[ PAGEIMAGES ][ nI ][ 4 ] ) ) + ;
            " " + hb_ntos( s_aReport[ PAGEIMAGES ][ nI ][ 3 ] ) + ;
            " " + hb_ntos( s_aReport[ PAGEY ] - s_aReport[ PAGEIMAGES ][ nI ][ 2 ] - ;
            iif( s_aReport[ PAGEIMAGES ][ nI ][ 4 ] == 0, pdfM2X( s_aReport[ IMAGES ][ nImage ][ 3 ][ IMAGE_HEIGHT ] / s_aReport[ IMAGES ][ nImage ][ 3 ][ IMAGE_YRES ] * 25.4 ), s_aReport[ PAGEIMAGES ][ nI ][ 4 ] ) ) + " cm"
         cTemp += CRLF + "/Image" + hb_ntos( nImage ) + " Do"
         cTemp += CRLF + "Q"
      NEXT
      s_aReport[ PAGEBUFFER ] := cTemp + s_aReport[ PAGEBUFFER ]
   ENDIF

   cTemp := s_aReport[ PAGEBUFFER ]
   cTemp += CRLF + "endstream" + CRLF + ;
      "endobj" + CRLF
   s_aReport[ DOCLEN ] += Len( cTemp )
   FWrite( s_aReport[ HANDLE ], cTemp )

   AAdd( s_aReport[ REFS ], s_aReport[ DOCLEN ] )
   cTemp := hb_ntos( ++s_aReport[ REPORTOBJ ] ) + " 0 obj" + CRLF + ;
      hb_ntos( Len( s_aReport[ PAGEBUFFER ] ) ) + CRLF + ;
      "endobj" + CRLF
   s_aReport[ DOCLEN ] += Len( cTemp )
   FWrite( s_aReport[ HANDLE ], cTemp )

   FOR nI := 1 TO Len( s_aReport[ FONTS ] )
      IF s_aReport[ FONTS ][ nI ][ 2 ] > s_aReport[ REPORTOBJ ]
         AAdd( s_aReport[ REFS ], s_aReport[ DOCLEN ] )
         cTemp := ;
            hb_ntos( s_aReport[ FONTS ][ nI ][ 2 ] ) + " 0 obj" + CRLF + ;
            "<<" + CRLF + ;
            "/Type /Font" + CRLF + ;
            "/Subtype /Type1" + CRLF + ;
            "/Name /Fo" + hb_ntos( nI ) + CRLF + ;
            "/BaseFont /" + s_aReport[ TYPE1 ][ s_aReport[ FONTS ][ nI ][ 1 ] ] + CRLF + ;
            "/Encoding /WinAnsiEncoding" + CRLF + ;
            ">>" + CRLF + ;
            "endobj" + CRLF
         s_aReport[ DOCLEN ] += Len( cTemp )
         FWrite( s_aReport[ HANDLE ], cTemp )
      ENDIF
   NEXT

   FOR nI := 1 TO Len( s_aReport[ IMAGES ] )
      IF s_aReport[ IMAGES ][ nI ][ 2 ] > s_aReport[ REPORTOBJ ]
         AAdd( s_aReport[ REFS ], s_aReport[ DOCLEN ] )
         cTemp := ;
            hb_ntos( s_aReport[ IMAGES ][ nI ][ 2 ] ) + " 0 obj" + CRLF + ;
            "<<" + CRLF + ;
            "/Type /XObject" + CRLF + ;
            "/Subtype /Image" + CRLF + ;
            "/Name /Image" + hb_ntos( nI ) + CRLF + ;
            "/Filter [" + iif( At( ".jpg", Lower( s_aReport[ IMAGES ][ nI ][ 1 ] ) ) > 0, " /DCTDecode", "" ) + " ]" + CRLF + ;
            "/Width " + hb_ntos( s_aReport[ IMAGES ][ nI ][ 3 ][ IMAGE_WIDTH ] ) + CRLF + ;
            "/Height " + hb_ntos( s_aReport[ IMAGES ][ nI ][ 3 ][ IMAGE_HEIGHT ] ) + CRLF + ;
            "/BitsPerComponent " + hb_ntos( s_aReport[ IMAGES ][ nI ][ 3 ][ IMAGE_BITS ] ) + CRLF + ;
            "/ColorSpace /" + iif( s_aReport[ IMAGES ][ nI ][ 3 ][ IMAGE_SPACE ] == 1, "DeviceGray", "DeviceRGB" ) + CRLF + ;
            "/Length " + hb_ntos( s_aReport[ IMAGES ][ nI ][ 3 ][ IMAGE_LENGTH ] ) + CRLF + ;
            ">>" + CRLF + ;
            "stream" + CRLF
         s_aReport[ DOCLEN ] += Len( cTemp )
         FWrite( s_aReport[ HANDLE ], cTemp )
         nImageHandle := FOpen( s_aReport[ IMAGES ][ nI ][ 1 ] )
         FSeek( nImageHandle, s_aReport[ IMAGES ][ nI ][ 3 ][ IMAGE_FROM ] )
         nBuffer := 8192
         cBuffer := Space( nBuffer )
         k := 0
         WHILE k < s_aReport[ IMAGES ][ nI ][ 3 ][ IMAGE_LENGTH ]
            IF k + nBuffer <= s_aReport[ IMAGES ][ nI ][ 3 ][ IMAGE_LENGTH ]
               nRead := nBuffer
            ELSE
               nRead := s_aReport[ IMAGES ][ nI ][ 3 ][ IMAGE_LENGTH ] - k
            ENDIF
            FRead( nImageHandle, @cBuffer, nRead )
            s_aReport[ DOCLEN ] += nRead
            FWrite( s_aReport[ HANDLE ], cBuffer, nRead )
            k += nRead
         ENDDO
         FClose( nImageHandle )
         cTemp := CRLF + "endstream" + CRLF + ;
            "endobj" + CRLF
         s_aReport[ DOCLEN ] += Len( cTemp )
         FWrite( s_aReport[ HANDLE ], cTemp )
      ENDIF
   NEXT

   s_aReport[ REPORTOBJ ] := s_aReport[ NEXTOBJ ]
   s_aReport[ NEXTOBJ ] := s_aReport[ REPORTOBJ ] + 4
   s_aReport[ PAGEBUFFER ] := ""

RETURN NIL

/*
 * Function: pdfGetFontInfo
 * Description: Gets information about the current font.
 * Parameters:
 *   cParam - The parameter to get ("NAME" or "TYPE").
 * Returns: The requested font information.
 */
STATIC FUNCTION pdfGetFontInfo( cParam )

   LOCAL cRet
   IF cParam == "NAME"
      IF Left( s_aReport[ TYPE1 ][ s_aReport[ FONTNAME ] ], 5 ) == "Times"
         cRet := "Times"
      ELSEIF Left( s_aReport[ TYPE1 ][ s_aReport[ FONTNAME ] ], 9 ) == "Helvetica"
         cRet := "Helvetica"
      ELSE
         cRet := "Courier"
      ENDIF
   ELSE // size
      cRet := Int( ( s_aReport[ FONTNAME ] - 1 ) % 4 )
   ENDIF

RETURN cRet

/*
 * Function: pdfImage
 * Description: Inserts an image into the PDF.
 * Parameters:
 *   cFile - The image file.
 *   nRow - The row position.
 *   nCol - The column position.
 *   cUnits - The units for the position.
 *   nHeight - The height of the image.
 *   nWidth - The width of the image.
 *   cId - An identifier for the image.
 * Returns: NIL
 */
FUNCTION pdfImage( cFile, nRow, nCol, cUnits, nHeight, nWidth, cId )

   __defaultNIL( @nRow, s_aReport[ REPORTLINE ] )
   __defaultNIL( @nCol, 0 )
   __defaultNIL( @nHeight, 0 )
   __defaultNIL( @nWidth, 0 )
   __defaultNIL( @cUnits, "R" )
   __defaultNIL( @cId, "" )

   IF s_aReport[ HEADEREDIT ]
      RETURN pdfHeader( "PDFIMAGE", cId, { cFile, nRow, nCol, cUnits, nHeight, nWidth } )
   ENDIF

   // Convert units if necessary
   IF cUnits == "M"
      nRow := s_aReport[ PAGEY ] - pdfM2Y( nRow )
      nCol := pdfM2X( nCol )
      nHeight := s_aReport[ PAGEY ] - pdfM2Y( nHeight )
      nWidth := pdfM2X( nWidth )
   ELSEIF cUnits == "R"
      nRow := s_aReport[ PAGEY ] - pdfR2D( nRow )
      nCol := pdfM2X( s_aReport[ PDFLEFT ] ) + ;
         nCol * 100.00 / s_aReport[ REPORTWIDTH ] * ;
         ( s_aReport[ PAGEX ] - pdfM2X( s_aReport[ PDFLEFT ] ) * 2 - 9.0 ) / 100.00
      nHeight := s_aReport[ PAGEY ] - pdfR2D( nHeight )
      nWidth := pdfM2X( s_aReport[ PDFLEFT ] ) + ;
         nWidth * 100.00 / s_aReport[ REPORTWIDTH ] * ;
         ( s_aReport[ PAGEX ] - pdfM2X( s_aReport[ PDFLEFT ] ) * 2 - 9.0 ) / 100.00
   ELSEIF cUnits == "D"
   ENDIF

   AAdd( s_aReport[ PAGEIMAGES ], { cFile, nRow, nCol, nHeight, nWidth } )

RETURN NIL

/*
 * Function: pdfItalic
 * Description: Sets the font to italic.
 * Returns: NIL
 */
FUNCTION pdfItalic()
   IF pdfGetFontInfo( "NAME" ) == "Times"
      s_aReport[ FONTNAME ] := 3
   ELSEIF pdfGetFontInfo( "NAME" ) == "Helvetica"
      s_aReport[ FONTNAME ] := 7
   ELSE
      s_aReport[ FONTNAME ] := 11
   ENDIF
   AAdd( s_aReport[ PAGEFONTS ], s_aReport[ FONTNAME ] )
   IF AScan( s_aReport[ FONTS ], {| arr | arr[ 1 ] == s_aReport[ FONTNAME ] } ) == 0
      AAdd( s_aReport[ FONTS ], { s_aReport[ FONTNAME ], ++s_aReport[ NEXTOBJ ] } )
   ENDIF

RETURN NIL

/*
 * Function: pdfLen
 * Description: Calculates the length of a string in the current font.
 * Parameters:
 *   cString - The string to calculate the length for.
 * Returns: The length of the string.
 */
FUNCTION pdfLen( cString )

   LOCAL nWidth := 0.00, nI, nLen, nArr, nAdd := ( s_aReport[ FONTNAME ] - 1 ) % 4
   nLen := Len( cString )
   IF Right( cString, 1 ) == Chr( 255 ) .OR. Right( cString, 1 ) == Chr( 254 ) // reverse or underline
      --nLen
   ENDIF
   IF pdfGetFontInfo( "NAME" ) == "Times"
      nArr := 1
   ELSEIF pdfGetFontInfo( "NAME" ) == "Helvetica"
      nArr := 2
   ELSE
      nArr := 3
   ENDIF
   IF ! Empty( s_aReport[ FONTWIDTH ] )
      FOR nI := 1 TO nLen
         nWidth += s_aReport[ FONTWIDTH ][ nArr ][ ( Asc( SubStr( cString, nI, 1 ) ) - 32 ) * 4 + 1 + nAdd ] * 25.4 * s_aReport[ FONTSIZE ] / 720.00 / 100.00
      NEXT
   ENDIF

RETURN nWidth

/*
 * Function: pdfM2R
 * Description: Converts millimeters to rows.
 * Parameters:
 *   mm - The value in millimeters.
 * Returns: The value in rows.
 */
STATIC FUNCTION pdfM2R( mm )
RETURN Int( s_aReport[ LPI ] * mm / 25.4 )

/*
 * Function: pdfM2X
 * Description: Converts millimeters to PDF units.
 * Parameters:
 *   n - The value in millimeters.
 * Returns: The value in PDF units.
 */
STATIC FUNCTION pdfM2X( n )
RETURN n * 72 / 25.4

/*
 * Function: pdfM2Y
 * Description: Converts millimeters to PDF units (Y coordinate).
 * Parameters:
 *   n - The value in millimeters.
 * Returns: The value in PDF units.
 */
STATIC FUNCTION pdfM2Y( n )
RETURN s_aReport[ PAGEY ] - n * 72 / 25.4

/*
 * Function: pdfNewLine
 * Description: Moves to a new line in the PDF.
 * Parameters:
 *   n - The number of lines to move.
 * Returns: The new line number.
 */
FUNCTION pdfNewLine( n )
   __defaultNIL( @n, 1 )
   IF s_aReport[ REPORTLINE ] + n + s_aReport[ PDFTOP ] > s_aReport[ PDFBOTTOM ]
      pdfNewPage()
      s_aReport[ REPORTLINE ] += 1
   ELSE
      s_aReport[ REPORTLINE ] += n
   ENDIF

RETURN s_aReport[ REPORTLINE ]

/*
 * Function: pdfNewPage
 * Description: Creates a new page in the PDF.
 * Parameters:
 *   _cPageSize - The page size.
 *   _cPageOrient - The page orientation.
 *   _nLpi - The lines per inch.
 *   _cFontName - The font name.
 *   _nFontType - The font type.
 *   _nFontSize - The font size.
 * Returns: NIL
 */
FUNCTION pdfNewPage( _cPageSize, _cPageOrient, _nLpi, _cFontName, _nFontType, _nFontSize )
   __defaultNIL( @_cPageSize, s_aReport[ PAGESIZE ] )
   __defaultNIL( @_cPageOrient, s_aReport[ PAGEORIENT ] )
   __defaultNIL( @_nLpi, s_aReport[ LPI ] )
   __defaultNIL( @_cFontName, pdfGetFontInfo( "NAME" ) )
   __defaultNIL( @_nFontType, pdfGetFontInfo( "TYPE" ) )
   __defaultNIL( @_nFontSize, s_aReport[ FONTSIZE ] )

   IF ! Empty( s_aReport[ PAGEBUFFER ] )
      pdfClosePage()
   ENDIF

   s_aReport[ PAGEFONTS ] := {}
   s_aReport[ PAGEIMAGES ] := {}
   ++s_aReport[ REPORTPAGE ]
   pdfPageSize( _cPageSize )
   pdfPageOrient( _cPageOrient )
   pdfSetLPI( _nLpi )
   pdfSetFont( _cFontName, _nFontType, _nFontSize )
   pdfDrawHeader()
   s_aReport[ REPORTLINE ] := 0
   s_aReport[ FONTNAMEPREV ] := 0
   s_aReport[ FONTSIZEPREV ] := 0

RETURN NIL

/*
 * Function: pdfNormal
 * Description: Sets the font to normal.
 * Returns: NIL
 */
FUNCTION pdfNormal()
   IF pdfGetFontInfo( "NAME" ) == "Times"
      s_aReport[ FONTNAME ] := 1
   ELSEIF pdfGetFontInfo( "NAME" ) == "Helvetica"
      s_aReport[ FONTNAME ] := 5
   ELSE
      s_aReport[ FONTNAME ] := 9
   ENDIF
   AAdd( s_aReport[ PAGEFONTS ], s_aReport[ FONTNAME ] )
   IF AScan( s_aReport[ FONTS ], {| arr | arr[ 1 ] == s_aReport[ FONTNAME ] } ) == 0
      AAdd( s_aReport[ FONTS ], { s_aReport[ FONTNAME ], ++s_aReport[ NEXTOBJ ] } )
   ENDIF

RETURN NIL

/*
 * Function: pdfOpen
 * Description: Opens a new PDF document.
 * Parameters:
 *   cFile - The file name.
 *   nLen - The report width.
 *   lOptimize - Whether to optimize the PDF.
 * Returns: NIL
 */
FUNCTION pdfOpen( cFile, nLen, lOptimize )

   LOCAL cTemp, nI, nJ, n1, n2 := 896, n12

   __defaultNIL( @nLen, 200 )
   __defaultNIL( @lOptimize, .F. )

   s_aReport[ FONTNAME ] := 1
   s_aReport[ FONTSIZE ] := 10
   s_aReport[ LPI ] := 6
   s_aReport[ PAGESIZE ] := "LETTER"
   s_aReport[ PAGEORIENT ] := "P"
   s_aReport[ PAGEX ] := 8.5 * 72
   s_aReport[ PAGEY ] := 11.0 * 72
   s_aReport[ REPORTWIDTH ] := nLen
   s_aReport[ REPORTPAGE ] := 0
   s_aReport[ REPORTLINE ] := 0
   s_aReport[ FONTNAMEPREV ] := 0
   s_aReport[ FONTSIZEPREV ] := 0
   s_aReport[ PAGEBUFFER ] := ""
   s_aReport[ REPORTOBJ ] := 1
   s_aReport[ DOCLEN ] := 0
   s_aReport[ TYPE1 ] := { "Times-Roman", "Times-Bold", "Times-Italic", "Times-BoldItalic", "Helvetica", "Helvetica-Bold", "Helvetica-Oblique", "Helvetica-BoldOblique", "Courier", "Courier-Bold", "Courier-Oblique", "Courier-BoldOblique" }
   s_aReport[ MARGINS ] := .T.
   s_aReport[ HEADEREDIT ] := .F.
   s_aReport[ NEXTOBJ ] := 0
   s_aReport[ PDFTOP ] := 1
   s_aReport[ PDFLEFT ] := 10
   s_aReport[ PDFBOTTOM ] := s_aReport[ PAGEY ] / 72 * s_aReport[ LPI ] - 1
   s_aReport[ HANDLE ] := FCreate( cFile )
   s_aReport[ PAGES ] := {}
   s_aReport[ REFS ] := { 0, 0 }
   s_aReport[ BOOKMARK ] := {}
   s_aReport[ HEADER ] := {}
   s_aReport[ FONTS ] := {}
   s_aReport[ IMAGES ] := {}
   s_aReport[ PAGEIMAGES ] := {}
   s_aReport[ PAGEFONTS ] := {}

   cTemp := vpdf_FontsDat()
   n1 := Len( cTemp ) / ( 2 * n2 )
   s_aReport[ FONTWIDTH ] := Array( n1, n2 )
   s_aReport[ OPTIMIZE ] := lOptimize
   s_aReport[ NEXTOBJ ] := s_aReport[ REPORTOBJ ] + 4
   n12 := 2 * n2
   FOR nI := 1 TO n1
      FOR nJ := 1 TO n2
         s_aReport[ FONTWIDTH ][ nI ][ nJ ] := Bin2I( SubStr( cTemp, ( nI - 1 ) * n12 + ( nJ - 1 ) * 2 + 1, 2 ) )
      NEXT
   NEXT

   s_aReport[ DOCLEN ] := 0
   cTemp := "%PDF-1.3" + CRLF
   s_aReport[ DOCLEN ] += Len( cTemp )
   FWrite( s_aReport[ HANDLE ], cTemp )

RETURN NIL

/*
 * Function: pdfPageSize
 * Description: Sets the page size.
 * Parameters:
 *   _cPageSize - The page size.
 *   _nWidth - The width.
 *   _nHeight - The height.
 * Returns: NIL
 */
FUNCTION pdfPageSize( _cPageSize, _nWidth, _nHeight )

   LOCAL nSize, aSize, nWidth, nHeight

   aSize := { ;
      { "LETTER", 8.50, 11.00 }, ;
      { "LEGAL", 8.50, 14.00 }, ;
      { "LEDGER", 11.00, 17.00 }, ;
      { "EXECUTIVE", 7.25, 10.50 }, ;
      { "A4", 8.27, 11.69 }, ;
      { "A3", 11.69, 16.54 }, ;
      { "JIS B4", 10.12, 14.33 }, ;
      { "JIS B5", 7.16, 10.12 }, ;
      { "JPOST", 3.94, 5.83 }, ;
      { "JPOSTD", 5.83, 7.87 }, ;
      { "COM10", 4.12, 9.50 }, ;
      { "MONARCH", 3.87, 7.50 }, ;
      { "C5", 6.38, 9.01 }, ;
      { "DL", 4.33, 8.66 }, ;
      { "B5", 6.93, 9.84 }, ;
      { "USSTDFOLD", 14.87, 11.00 } }

   __defaultNIL( @_cPageSize, "LETTER" )

   IF Empty( _nWidth ) .OR. Empty( _nHeight )
      nSize := AScan( aSize, {| arr | arr[ 1 ] == _cPageSize } )
      IF nSize == 0
         nSize := 1
      ENDIF
      s_aReport[ PAGESIZE ] := aSize[ nSize ][ 1 ]
      nWidth := aSize[ nSize ][ 2 ]
      nHeight := aSize[ nSize ][ 3 ]
   ELSE
      _nWidth := Val( Str( _nWidth ) )
      _nHeight := Val( Str( _nHeight ) )
      nSize := AScan( aSize, {| arr | ( arr[ 2 ] == _nWidth ) .AND. ( arr[ 3 ] == _nHeight ) } )
      IF nSize == 0
         nSize := AScan( aSize, {| arr | ( arr[ 3 ] == _nWidth ) .AND. ( arr[ 2 ] == _nHeight ) } )
      ENDIF
      IF nSize == 0
         nSize := 1
      ENDIF
      s_aReport[ PAGESIZE ] := aSize[ nSize ][ 1 ]
      nWidth := _nWidth
      nHeight := _nHeight
   ENDIF

   IF s_aReport[ PAGEORIENT ] == "P"
      s_aReport[ PAGEX ] := nWidth * 72
      s_aReport[ PAGEY ] := nHeight * 72
   ELSE
      s_aReport[ PAGEX ] := nHeight * 72
      s_aReport[ PAGEY ] := nWidth * 72
   ENDIF

RETURN NIL

/*
 * Function: pdfPageOrient
 * Description: Sets the page orientation.
 * Parameters:
 *   _cPageOrient - The page orientation.
 * Returns: NIL
 */
FUNCTION pdfPageOrient( _cPageOrient )
   __defaultNIL( @_cPageOrient, "P" )
   s_aReport[ PAGEORIENT ] := _cPageOrient
   pdfPageSize( s_aReport[ PAGESIZE ] )

RETURN NIL

/*
 * Function: pdfR2D
 * Description: Converts rows to PDF units (Y coordinate).
 * Parameters:
 *   nRow - The row number.
 * Returns: The value in PDF units.
 */
STATIC FUNCTION pdfR2D( nRow )
RETURN s_aReport[ PAGEY ] - nRow * 72 / s_aReport[ LPI ]

/*
 * Function: pdfR2M
 * Description: Converts rows to millimeters.
 * Parameters:
 *   nRow - The row number.
 * Returns: The value in millimeters.
 */
STATIC FUNCTION pdfR2M( nRow )
RETURN 25.4 * nRow / s_aReport[ LPI ]

/*
 * Function: pdfPageNumber
 * Description: Gets or sets the page number.
 * Parameters:
 *   n - The page number to set.
 * Returns: The current page number.
 */
FUNCTION pdfPageNumber( n )
   __defaultNIL( @n, 0 )
   IF n > 0
      s_aReport[ REPORTPAGE ] := n
   ENDIF

RETURN s_aReport[ REPORTPAGE ]

/*
 * Function: pdfReverse
 * Description: Reverses the text color.
 * Parameters:
 *   cString - The string to reverse.
 * Returns: The reversed string.
 */
FUNCTION pdfReverse( cString )
RETURN cString + Chr( 255 )

/*
 * Function: pdfRJust
 * Description: Right-justifies a string at a specific position in the PDF.
 * Parameters:
 *   cString - The string to right-justify.
 *   nRow - The row position.
 *   nCol - The column position.
 *   cUnits - The units for the position.
 *   lExact - Whether to use exact positioning.
 *   cId - An identifier for the string.
 * Returns: NIL
 */
FUNCTION pdfRJust( cString, nRow, nCol, cUnits, lExact, cId )

   LOCAL nLen, nAdj := 1.0, nAt

   __defaultNIL( @nRow, s_aReport[ REPORTLINE ] )
   __defaultNIL( @cUnits, "R" )
   __defaultNIL( @lExact, .F. )

   IF s_aReport[ HEADEREDIT ]
      RETURN pdfHeader( "PDFRJUST", cId, { cString, nRow, nCol, cUnits, lExact } )
   ENDIF

   // Replace page number placeholder with actual page number
   IF ( nAt := At( "#pagenumber#", cString ) ) > 0
      cString := Left( cString, nAt - 1 ) + hb_ntos( pdfPageNumber() ) + SubStr( cString, nAt + 12 )
   ENDIF

   nLen := pdfLen( cString )

   IF cUnits == "R"
      IF ! lExact
         pdfCheckLine( nRow )
         nRow := nRow + s_aReport[ PDFTOP ]
      ENDIF
   ENDIF

   pdfAtSay( cString, pdfR2M( nRow ), iif( cUnits == "R", s_aReport[ PDFLEFT ] + ( s_aReport[ PAGEX ] / 72 * 25.4 - 2 * s_aReport[ PDFLEFT ] ) * nCol / s_aReport[ REPORTWIDTH ] - nAdj, nCol ) - nLen, "M", lExact )

RETURN NIL

/*
 * Function: pdfSetFont
 * Description: Sets the font.
 * Parameters:
 *   _cFont - The font name.
 *   _nType - The font type.
 *   _nSize - The font size.
 *   cId - An identifier for the font.
 * Returns: NIL
 */
FUNCTION pdfSetFont( _cFont, _nType, _nSize, cId )

   __defaultNIL( @_cFont, "Times" )
   __defaultNIL( @_nType, 0 )
   __defaultNIL( @_nSize, 10 )

   IF s_aReport[ HEADEREDIT ]
      RETURN pdfHeader( "PDFSETFONT", cId, { _cFont, _nType, _nSize } )
   ENDIF

   _cFont := Upper( _cFont )
   s_aReport[ FONTSIZE ] := _nSize

   IF _cFont == "TIMES"
      s_aReport[ FONTNAME ] := _nType + 1
   ELSEIF _cFont == "HELVETICA"
      s_aReport[ FONTNAME ] := _nType + 5
   ELSE
      s_aReport[ FONTNAME ] := _nType + 9
   ENDIF

   AAdd( s_aReport[ PAGEFONTS ], s_aReport[ FONTNAME ] )
   IF AScan( s_aReport[ FONTS ], {| arr | arr[ 1 ] == s_aReport[ FONTNAME ] } ) == 0
      AAdd( s_aReport[ FONTS ], { s_aReport[ FONTNAME ], ++s_aReport[ NEXTOBJ ] } )
   ENDIF

RETURN NIL

/*
 * Function: pdfSetLPI
 * Description: Sets the lines per inch.
 * Parameters:
 *   _nLpi - The lines per inch.
 * Returns: NIL
 */
FUNCTION pdfSetLPI( _nLpi )

   LOCAL cLpi := hb_ntos( _nLpi )

   __defaultNIL( @_nLpi, 6 )

   cLpi := iif( cLpi $ "1;2;3;4;6;8;12;16;24;48", cLpi, "6" )
   s_aReport[ LPI ] := Val( cLpi )
   pdfPageSize( s_aReport[ PAGESIZE ] )

RETURN NIL

/*
 * Function: pdfStringB
 * Description: Escapes special characters in a string.
 * Parameters:
 *   cString - The string to escape.
 * Returns: The escaped string.
 */
FUNCTION pdfStringB( cString )
   cString := StrTran( cString, "(", "\(" )
   cString := StrTran( cString, ")", "\)" )

RETURN cString

/*
 * Function: pdfTextCount
 * Description: Counts the number of lines a text will occupy.
 * Parameters:
 *   cString - The text to count.
 *   nTop - The top position.
 *   nLeft - The left position.
 *   nLength - The length of the text.
 *   nTab - The tab size.
 *   nJustify - The justification.
 *   cUnits - The units for the position.
 * Returns: The number of lines.
 */
FUNCTION pdfTextCount( cString, nTop, nLeft, nLength, nTab, nJustify, cUnits )
RETURN pdfText( cString, nTop, nLeft, nLength, nTab, nJustify, cUnits, .F. )

/*
 * Function: pdfText
 * Description: Places text in the PDF.
 * Parameters:
 *   cString - The text to place.
 *   nTop - The top position.
 *   nLeft - The left position.
 *   nLength - The length of the text.
 *   nTab - The tab size.
 *   nJustify - The justification.
 *   cUnits - The units for the position.
 *   cColor - The color of the text.
 *   lPrint - Whether to print the text.
 * Returns: The number of lines.
 */
FUNCTION pdfText( cString, nTop, nLeft, nLength, nTab, nJustify, cUnits, cColor, lPrint )

   LOCAL cDelim := Chr( 0 ) + Chr( 9 ) + Chr( 10 ) + Chr( 13 ) + Chr( 26 ) + Chr( 32 ) + Chr( 138 ) + Chr( 141 )
   LOCAL nI, cTemp, cToken, k, nL, nRow, nLines, nLineLen, nStart
   LOCAL lParagraph, nSpace, nNew, nTokenLen, nCRLF, nTokens, nLen

   __defaultNIL( @nTab, - 1 )
   __defaultNIL( @cUnits, "R" )
   __defaultNIL( @nJustify, 4 ) // justify
   __defaultNIL( @lPrint, .T. )
   __defaultNIL( @cColor, "" )

   IF cUnits == "M"
      nTop := pdfM2R( nTop )
   ELSEIF cUnits == "R"
      nLeft := pdfX2M( pdfM2X( s_aReport[ PDFLEFT ] ) + ;
         nLeft * 100.00 / s_aReport[ REPORTWIDTH ] * ;
         ( s_aReport[ PAGEX ] - pdfM2X( s_aReport[ PDFLEFT ] ) * 2 - 9.0 ) / 100.00 )
   ENDIF

   s_aReport[ REPORTLINE ] := nTop - 1
   nSpace := pdfLen( " " )
   nLines := 0
   nCRLF := 0
   nNew := nTab
   cString := AllTrim( cString )
   nTokens := NumToken( cString, cDelim )
   nStart := 1

   IF nJustify == 1 .OR. nJustify == 4
      nLeft := nLeft
   ELSEIF nJustify == 2
      nLeft := nLeft - nLength / 2
   ELSEIF nJustify == 3
      nLeft := nLeft - nLength
   ENDIF

   nLineLen := nSpace * nNew - nSpace
   lParagraph := .T.
   nI := 1

   WHILE nI <= nTokens
      cToken := Token( cString, cDelim, nI )
      nTokenLen := pdfLen( cToken )
      nLen := Len( cToken )

      IF nLineLen + nSpace + nTokenLen > nLength
         IF nStart == nI // single word > nLength
            k := 1
            WHILE k <= nLen
               cTemp := ""
               nLineLen := 0.00
               nL := nLeft
               IF lParagraph
                  nLineLen += nSpace * nNew
                  IF nJustify != 2
                     nL += nSpace * nNew
                  ENDIF
                  lParagraph := .F.
               ENDIF
               IF nJustify == 2
                  nL := nLeft + ( nLength - pdfLen( cTemp ) ) / 2
               ELSEIF nJustify == 3
                  nL := nLeft + nLength - pdfLen( cTemp )
               ENDIF
               WHILE k <= nLen .AND. ( ( nLineLen += pdfLen( SubStr( cToken, k, 1 ) ) ) <= nLength )
                  nLineLen += pdfLen( SubStr( cToken, k, 1 ) )
                  cTemp += SubStr( cToken, k, 1 )
                  ++k
               ENDDO
               IF Empty( cTemp )
                  cTemp := SubStr( cToken, k, 1 )
                  ++k
               ENDIF
               ++nLines
               IF lPrint
                  nRow := pdfNewLine( 1 )
                  pdfAtSay( cColor + cTemp, pdfR2M( nRow + s_aReport[ PDFTOP ] ), nL, "M" )
               ENDIF
            ENDDO
            ++nI
            nStart := nI
         ELSE
            pdfTextPrint( nI - 1, nLeft, @lParagraph, nJustify, nSpace, nNew, nLength, @nLineLen, @nLines, @nStart, cString, cDelim, cColor, lPrint )
         ENDIF
      ELSEIF ( nI == nTokens ) .OR. ( nI < nTokens .AND. ( nCRLF := pdfTextNextPara( cString, cDelim, nI ) ) > 0 )
         IF nI == nTokens
            nLineLen += nSpace + nTokenLen
         ENDIF
         pdfTextPrint( nI, nLeft, @lParagraph, nJustify, nSpace, nNew, nLength, @nLineLen, @nLines, @nStart, cString, cDelim, cColor, lPrint )
         ++nI
         IF nCRLF > 1
            nLines += nCRLF - 1
         ENDIF
         IF lPrint
            pdfNewLine( nCRLF - 1 )
         ENDIF
      ELSE
         nLineLen += nSpace + nTokenLen
         ++nI
      ENDIF
   ENDDO

RETURN nLines

/*
 * Function: pdfTextPrint
 * Description: Prints a line of text.
 * Parameters:
 *   nI - The token index.
 *   nLeft - The left position.
 *   lParagraph - Whether it's a new paragraph.
 *   nJustify - The justification.
 *   nSpace - The space width.
 *   nNew - The new paragraph indent.
 *   nLength - The line length.
 *   nLineLen - The current line length.
 *   nLines - The number of lines.
 *   nStart - The start token index.
 *   cString - The text string.
 *   cDelim - The delimiter.
 *   cColor - The color of the text.
 *   lPrint - Whether to print the text.
 * Returns: NIL
 */
STATIC FUNCTION pdfTextPrint( nI, nLeft, lParagraph, nJustify, nSpace, nNew, nLength, nLineLen, nLines, nStart, cString, cDelim, cColor, lPrint )

   LOCAL nFinish, nL, nB, nJ, cToken, nRow

   nFinish := nI
   nL := nLeft
   IF lParagraph
      IF nJustify != 2
         nL += nSpace * nNew
      ENDIF
   ENDIF
   IF nJustify == 3
      nL += nLength - nLineLen
   ELSEIF nJustify == 2
      nL += ( nLength - nLineLen ) / 2
   ENDIF
   ++nLines
   IF lPrint
      nRow := pdfNewLine( 1 )
   ENDIF
   nB := nSpace
   IF nJustify == 4
      nB := ( nLength - nLineLen + ( nFinish - nStart ) * nSpace ) / ( nFinish - nStart )
   ENDIF
   FOR nJ := nStart TO nFinish
      cToken := Token( cString, cDelim, nJ )
      IF lPrint
         pdfAtSay( cColor + cToken, pdfR2M( nRow + s_aReport[ PDFTOP ] ), nL, "M" )
      ENDIF
      nL += pdfLen ( cToken ) + nB
   NEXT
   nStart := nFinish + 1
   lParagraph := .F.
   nLineLen := 0.00
   nLineLen += nSpace * nNew

RETURN NIL

/*
 * Function: pdfTextNextPara
 * Description: Checks if the next token is a new paragraph.
 * Parameters:
 *   cString - The text string.
 *   cDelim - The delimiter.
 *   nI - The token index.
 * Returns: The number of new paragraphs.
 */
STATIC FUNCTION pdfTextNextPara( cString, cDelim, nI )

   LOCAL nAt, cAt, nCRLF, nNew, nRat, nRet := 0

   // check if next spaces paragraph(s)
   nAt := AtToken( cString, cDelim, nI ) + Len( Token( cString, cDelim, nI ) )
   cAt := SubStr( cString, nAt, AtToken( cString, cDelim, nI + 1 ) - nAt )
   nCRLF := NumAt( Chr( 13 ) + Chr( 10 ), cAt )
   nRat := RAt( Chr( 13 ) + Chr( 10 ), cAt )
   nNew := Len( cAt ) - nRat - iif( nRat > 0, 1, 0 )
   IF nCRLF > 1 .OR. ( nCRLF == 1 .AND. nNew > 0 )
      nRet := nCRLF
   ENDIF

RETURN nRet

/*
 * Function: pdfUnderline
 * Description: Underlines a string.
 * Parameters:
 *   cString - The string to underline.
 * Returns: The underlined string.
 */
FUNCTION pdfUnderline( cString )
RETURN cString + Chr( 254 )

/*
 * Function: pdfX2M
 * Description: Converts PDF units to millimeters.
 * Parameters:
 *   n - The value in PDF units.
 * Returns: The value in millimeters.
 */
STATIC FUNCTION pdfX2M( n )
RETURN n * 25.4 / 72

/*
 * Function: TimeAsAMPM
 * Description: Converts time to AM/PM format.
 * Parameters:
 *   cTime - The time to convert.
 * Returns: The time in AM/PM format.
 */
STATIC FUNCTION TimeAsAMPM( cTime )
   IF Val( cTime ) < 12
      cTime += " am"
   ELSEIF Val( cTime ) == 12
      cTime += " pm"
   ELSE
      cTime := Str( Val( cTime ) - 12, 2 ) + SubStr( cTime, 3 ) + " pm"
   ENDIF
   cTime := Left( cTime, 5 ) + SubStr( cTime, 10 )

RETURN cTime

/*
 * Function: pdfOpenHeader
 * Description: Opens a header file.
 * Parameters:
 *   cFile - The header file.
 * Returns: NIL
 */
FUNCTION pdfOpenHeader( cFile )

   LOCAL nAt
   __defaultNIL( @cFile, "" )
   IF ! Empty( cFile )
      cFile := AllTrim( cFile )
      IF Len( cFile ) > 12 .OR. ;
            At( " ", cFile ) > 0 .OR. ;
            ( At( " ", cFile ) == 0 .AND. Len( cFile ) > 8 ) .OR. ;
            ( ( nAt := At( ".", cFile ) ) > 0 .AND. Len( SubStr( cFile, nAt + 1 ) ) > 3 )
         COPY FILE ( cFile ) TO temp.tmp
         cFile := "temp.tmp"
      ENDIF
      s_aReport[ HEADER ] := File2Array( cFile )
   ELSE
      s_aReport[ HEADER ] := {}
   ENDIF
   s_aReport[ MARGINS ] := .T.

RETURN NIL

/*
 * Function: pdfEditOnHeader
 * Description: Enables header editing.
 * Returns: NIL
 */
FUNCTION pdfEditOnHeader()
   s_aReport[ HEADEREDIT ] := .T.
   s_aReport[ MARGINS ] := .T.

RETURN NIL

/*
 * Function: pdfEditOffHeader
 * Description: Disables header editing.
 * Returns: NIL
 */
FUNCTION pdfEditOffHeader()
   s_aReport[ HEADEREDIT ] := .F.
   s_aReport[ MARGINS ] := .T.

RETURN NIL

/*
 * Function: pdfCloseHeader
 * Description: Closes the header.
 * Returns: NIL
 */
FUNCTION pdfCloseHeader()
   s_aReport[ HEADER ] := {}
   s_aReport[ MARGINS ] := .F.

RETURN NIL

/*
 * Function: pdfDeleteHeader
 * Description: Deletes a header.
 * Parameters:
 *   cId - The header identifier.
 * Returns: The number of headers remaining.
 */
FUNCTION pdfDeleteHeader( cId )

   LOCAL nRet := -1, nId
   cId := Upper( cId )
   nId := AScan( s_aReport[ HEADER ], {| arr | arr[ 3 ] == cId } )
   IF nId > 0
      nRet := Len( s_aReport[ HEADER ] ) - 1
      ADel( s_aReport[ HEADER ], nId )
      ASize( s_aReport[ HEADER ], nRet )
      s_aReport[ MARGINS ] := .T.
   ENDIF

RETURN nRet

/*
 * Function: pdfEnableHeader
 * Description: Enables a header.
 * Parameters:
 *   cId - The header identifier.
 * Returns: NIL
 */
FUNCTION pdfEnableHeader( cId )

   LOCAL nId
   cId := Upper( cId )
   nId := AScan( s_aReport[ HEADER ], {| arr | arr[ 3 ] == cId } )
   IF nId > 0
      s_aReport[ HEADER ][ nId ][ 1 ] := .T.
      s_aReport[ MARGINS ] := .T.
   ENDIF

RETURN NIL

/*
 * Function: pdfDisableHeader
 * Description: Disables a header.
 * Parameters:
 *   cId - The header identifier.
 * Returns: NIL
 */
FUNCTION pdfDisableHeader( cId )

   LOCAL nId
   cId := Upper( cId )
   nId := AScan( s_aReport[ HEADER ], {| arr | arr[ 3 ] == cId } )
   IF nId > 0
      s_aReport[ HEADER ][ nId ][ 1 ] := .F.
      s_aReport[ MARGINS ] := .T.
   ENDIF

RETURN NIL

/*
 * Function: pdfSaveHeader
 * Description: Saves the header to a file.
 * Parameters:
 *   cFile - The file to save to.
 * Returns: NIL
 */
FUNCTION pdfSaveHeader( cFile )
   Array2File( "temp.tmp", s_aReport[ HEADER ] )
   COPY FILE temp.tmp TO ( cFile )

RETURN NIL

/*
 * Function: pdfHeader
 * Description: Adds a header to the PDF.
 * Parameters:
 *   cFunction - The function name.
 *   cId - The header identifier.
 *   arr - The header data.
 * Returns: The header identifier.
 */
FUNCTION pdfHeader( cFunction, cId, arr )

   LOCAL nId, nI, nLen, nIdLen
   nId := 0
   IF ! Empty( cId )
      cId := Upper( cId )
      nId := AScan( s_aReport[ HEADER ], {| arr | arr[ 3 ] == cId } )
   ENDIF
   IF nId == 0
      nLen := Len( s_aReport[ HEADER ] )
      IF Empty( cId )
         cId := cFunction
         nIdLen := Len( cId )
         FOR nI := 1 TO nLen
            IF s_aReport[ HEADER ][ nI ][ 2 ] == cId
               IF Val( SubStr( s_aReport[ HEADER ][ nI ][ 3 ], nIdLen + 1 ) ) > nId
                  nId := Val( SubStr( s_aReport[ HEADER ][ nI ][ 3 ], nIdLen + 1 ) )
               ENDIF
            ENDIF
         NEXT
         ++nId
         cId += hb_ntos( nId )
      ENDIF
      AAdd( s_aReport[ HEADER ], { .T., cFunction, cId } )
      ++nLen
      FOR nI := 1 TO Len( arr )
         AAdd( s_aReport[ HEADER ][ nLen ], arr[ nI ] )
      NEXT
   ELSE
      ASize( s_aReport[ HEADER ][ nId ], 3 )
      FOR nI := 1 TO Len( arr )
         AAdd( s_aReport[ HEADER ][ nId ], arr[ nI ] )
      NEXT
   ENDIF

RETURN cId

/*
 * Function: pdfDrawHeader
 * Description: Draws the header on the PDF.
 * Returns: NIL
 */
FUNCTION pdfDrawHeader()

   LOCAL nI, _nFont, _nSize, nLen := Len( s_aReport[ HEADER ] )

   IF nLen > 0
      _nFont := s_aReport[ FONTNAME ]
      _nSize := s_aReport[ FONTSIZE ]
      FOR nI := 1 TO nLen
         IF s_aReport[ HEADER ][ nI ][ 1 ]
            DO CASE
            CASE s_aReport[ HEADER ][ nI ][ 2 ] == "PDFATSAY"
               pdfAtSay( s_aReport[ HEADER ][ nI ][ 4 ], s_aReport[ HEADER ][ nI ][ 5 ], s_aReport[ HEADER ][ nI ][ 6 ], s_aReport[ HEADER ][ nI ][ 7 ], s_aReport[ HEADER ][ nI ][ 8 ], s_aReport[ HEADER ][ nI ][ 3 ] )
            CASE s_aReport[ HEADER ][ nI ][ 2 ] == "PDFCENTER"
               pdfCenter( s_aReport[ HEADER ][ nI ][ 4 ], s_aReport[ HEADER ][ nI ][ 5 ], s_aReport[ HEADER ][ nI ][ 6 ], s_aReport[ HEADER ][ nI ][ 7 ], s_aReport[ HEADER ][ nI ][ 8 ], s_aReport[ HEADER ][ nI ][ 3 ] )
            CASE s_aReport[ HEADER ][ nI ][ 2 ] == "PDFRJUST"
               pdfRJust( s_aReport[ HEADER ][ nI ][ 4 ], s_aReport[ HEADER ][ nI ][ 5 ], s_aReport[ HEADER ][ nI ][ 6 ], s_aReport[ HEADER ][ nI ][ 7 ], s_aReport[ HEADER ][ nI ][ 8 ], s_aReport[ HEADER ][ nI ][ 3 ] )
            CASE s_aReport[ HEADER ][ nI ][ 2 ] == "PDFBOX"
               pdfBox( s_aReport[ HEADER ][ nI ][ 4 ], s_aReport[ HEADER ][ nI ][ 5 ], s_aReport[ HEADER ][ nI ][ 6 ], s_aReport[ HEADER ][ nI ][ 7 ], s_aReport[ HEADER ][ nI ][ 8 ], s_aReport[ HEADER ][ nI ][ 9 ], s_aReport[ HEADER ][ nI ][ 10 ], s_aReport[ HEADER ][ nI ][ 3 ] )
            CASE s_aReport[ HEADER ][ nI ][ 2 ] == "PDFSETFONT"
               pdfSetFont( s_aReport[ HEADER ][ nI ][ 4 ], s_aReport[ HEADER ][ nI ][ 5 ], s_aReport[ HEADER ][ nI ][ 6 ], s_aReport[ HEADER ][ nI ][ 3 ] )
            CASE s_aReport[ HEADER ][ nI ][ 2 ] == "PDFIMAGE"
               pdfImage( s_aReport[ HEADER ][ nI ][ 4 ], s_aReport[ HEADER ][ nI ][ 5 ], s_aReport[ HEADER ][ nI ][ 6 ], s_aReport[ HEADER ][ nI ][ 7 ], s_aReport[ HEADER ][ nI ][ 8 ], s_aReport[ HEADER ][ nI ][ 9 ], s_aReport[ HEADER ][ nI ][ 3 ] )
            ENDCASE
         ENDIF
      NEXT
      s_aReport[ FONTNAME ] := _nFont
      s_aReport[ FONTSIZE ] := _nSize
      IF s_aReport[ MARGINS ]
         pdfMargins()
      ENDIF
   ELSE
      IF s_aReport[ MARGINS ]
         s_aReport[ PDFTOP ] := 1
         s_aReport[ PDFLEFT ] := 10
         s_aReport[ PDFBOTTOM ] := s_aReport[ PAGEY ] / 72 * s_aReport[ LPI ] - 1
         s_aReport[ MARGINS ] := .F.
      ENDIF
   ENDIF

RETURN NIL

/*
 * Function: pdfMargins
 * Description: Sets the margins for the PDF.
 * Parameters:
 *   nTop - The top margin.
 *   nLeft - The left margin.
 *   nBottom - The bottom margin.
 * Returns: NIL
 */
FUNCTION pdfMargins( nTop, nLeft, nBottom )

   LOCAL nI, nLen := Len( s_aReport[ HEADER ] ), nTemp, aTemp, nHeight

   __defaultNIL( @nTop, 1 )
   __defaultNIL( @nLeft, 10 )
   __defaultNIL( @nBottom, s_aReport[ PAGEY ] / 72 * s_aReport[ LPI ] - 1 )  // bottom, default "LETTER", "P", 6

   s_aReport[ PDFTOP ] := nTop
   s_aReport[ PDFLEFT ] := nLeft
   s_aReport[ PDFBOTTOM ] := nBottom

   FOR nI := 1 TO nLen
      IF s_aReport[ HEADER ][ nI ][ 1 ]
         IF s_aReport[ HEADER ][ nI ][ 2 ] == "PDFSETFONT"
         ELSEIF s_aReport[ HEADER ][ nI ][ 2 ] == "PDFIMAGE"
            IF s_aReport[ HEADER ][ nI ][ 8 ] == 0
               aTemp := pdfImageInfo( s_aReport[ HEADER ][ nI ][ 4 ] )
               nHeight := aTemp[ IMAGE_HEIGHT ] / aTemp[ IMAGE_YRES ] * 25.4
               IF s_aReport[ HEADER ][ nI ][ 7 ] == "D"
                  nHeight := pdfM2X( nHeight )
               ENDIF
            ELSE
               nHeight := s_aReport[ HEADER ][ nI ][ 8 ]
            ENDIF
            IF s_aReport[ HEADER ][ nI ][ 7 ] == "M"
               nTemp := s_aReport[ PAGEY ] / 72 * 25.4 / 2
               IF s_aReport[ HEADER ][ nI ][ 5 ] < nTemp
                  nTemp := ( s_aReport[ HEADER ][ nI ][ 5 ] + nHeight ) * s_aReport[ LPI ] / 25.4
                  IF nTemp > s_aReport[ PDFTOP ]
                     s_aReport[ PDFTOP ] := nTemp
                  ENDIF
               ELSE
                  nTemp := s_aReport[ HEADER ][ nI ][ 5 ] * s_aReport[ LPI ] / 25.4
                  IF nTemp < s_aReport[ PDFBOTTOM ]
                     s_aReport[ PDFBOTTOM ] := nTemp
                  ENDIF
               ENDIF
            ELSEIF s_aReport[ HEADER ][ nI ][ 7 ] == "D"
               nTemp := s_aReport[ PAGEY ] / 2
               IF s_aReport[ HEADER ][ nI ][ 5 ] < nTemp
                  nTemp := ( s_aReport[ HEADER ][ nI ][ 5 ] + nHeight ) * s_aReport[ LPI ] / 72
                  IF nTemp > s_aReport[ PDFTOP ]
                     s_aReport[ PDFTOP ] := nTemp
                  ENDIF
               ELSE
                  nTemp := s_aReport[ HEADER ][ nI ][ 5 ] * s_aReport[ LPI ] / 72
                  IF nTemp < s_aReport[ PDFBOTTOM ]
                     s_aReport[ PDFBOTTOM ] := nTemp
                  ENDIF
               ENDIF
            ENDIF
         ELSEIF s_aReport[ HEADER ][ nI ][ 2 ] == "PDFBOX"
            IF s_aReport[ HEADER ][ nI ][ 10 ] == "M"
               nTemp := s_aReport[ PAGEY ] / 72 * 25.4 / 2
               IF s_aReport[ HEADER ][ nI ][ 4 ] < nTemp .AND. ;
                     s_aReport[ HEADER ][ nI ][ 6 ] < nTemp
                  nTemp := s_aReport[ HEADER ][ nI ][ 6 ] * s_aReport[ LPI ] / 25.4
                  IF nTemp > s_aReport[ PDFTOP ]
                     s_aReport[ PDFTOP ] := nTemp
                  ENDIF
               ELSEIF s_aReport[ HEADER ][ nI ][ 4 ] < nTemp .AND. ;
                     s_aReport[ HEADER ][ nI ][ 6 ] > nTemp
                  nTemp := ( s_aReport[ HEADER ][ nI ][ 4 ] + s_aReport[ HEADER ][ nI ][ 8 ] ) * s_aReport[ LPI ] / 25.4
                  IF nTemp > s_aReport[ PDFTOP ]
                     s_aReport[ PDFTOP ] := nTemp
                  ENDIF
                  nTemp := ( s_aReport[ HEADER ][ nI ][ 6 ] - s_aReport[ HEADER ][ nI ][ 8 ] ) * s_aReport[ LPI ] / 25.4
                  IF nTemp < s_aReport[ PDFBOTTOM ]
                     s_aReport[ PDFBOTTOM ] := nTemp
                  ENDIF
               ELSEIF s_aReport[ HEADER ][ nI ][ 4 ] > nTemp .AND. ;
                     s_aReport[ HEADER ][ nI ][ 6 ] > nTemp
                  nTemp := s_aReport[ HEADER ][ nI ][ 4 ] * s_aReport[ LPI ] / 25.4
                  IF nTemp < s_aReport[ PDFBOTTOM ]
                     s_aReport[ PDFBOTTOM ] := nTemp
                  ENDIF
               ENDIF
            ELSEIF s_aReport[ HEADER ][ nI ][ 10 ] == "D"
               nTemp := s_aReport[ PAGEY ] / 2
               IF s_aReport[ HEADER ][ nI ][ 4 ] < nTemp .AND. ;
                     s_aReport[ HEADER ][ nI ][ 6 ] < nTemp
                  nTemp := s_aReport[ HEADER ][ nI ][ 6 ] / s_aReport[ LPI ]
                  IF nTemp > s_aReport[ PDFTOP ]
                     s_aReport[ PDFTOP ] := nTemp
                  ENDIF
               ELSEIF s_aReport[ HEADER ][ nI ][ 4 ] < nTemp .AND. ;
                     s_aReport[ HEADER ][ nI ][ 6 ] > nTemp
                  nTemp := ( s_aReport[ HEADER ][ nI ][ 4 ] + s_aReport[ HEADER ][ nI ][ 8 ] ) / s_aReport[ LPI ]
                  IF nTemp > s_aReport[ PDFTOP ]
                     s_aReport[ PDFTOP ] := nTemp
                  ENDIF
                  nTemp := ( s_aReport[ HEADER ][ nI ][ 6 ] - s_aReport[ HEADER ][ nI ][ 8 ] ) / s_aReport[ LPI ]
                  IF nTemp < s_aReport[ PDFBOTTOM ]
                     s_aReport[ PDFBOTTOM ] := nTemp
                  ENDIF
               ELSEIF s_aReport[ HEADER ][ nI ][ 4 ] > nTemp .AND. ;
                     s_aReport[ HEADER ][ nI ][ 6 ] > nTemp
                  nTemp := s_aReport[ HEADER ][ nI ][ 4 ] / s_aReport[ LPI ]
                  IF nTemp < s_aReport[ PDFBOTTOM ]
                     s_aReport[ PDFBOTTOM ] := nTemp
                  ENDIF
               ENDIF
            ENDIF
         ELSE
            IF s_aReport[ HEADER ][ nI ][ 7 ] == "R"
               nTemp := s_aReport[ HEADER ][ nI ][ 5 ]
               IF s_aReport[ HEADER ][ nI ][ 5 ] > s_aReport[ PAGEY ] / 72 * s_aReport[ LPI ] / 2
                  IF nTemp < s_aReport[ PDFBOTTOM ]
                     s_aReport[ PDFBOTTOM ] := nTemp
                  ENDIF
               ELSE
                  IF nTemp > s_aReport[ PDFTOP ]
                     s_aReport[ PDFTOP ] := nTemp
                  ENDIF
               ENDIF
            ELSEIF s_aReport[ HEADER ][ nI ][ 7 ] == "M"
               nTemp := s_aReport[ HEADER ][ nI ][ 5 ] * s_aReport[ LPI ] / 25.4
               IF s_aReport[ HEADER ][ nI ][ 5 ] > s_aReport[ PAGEY ] / 72 * 25.4 / 2
                  IF nTemp < s_aReport[ PDFBOTTOM ]
                     s_aReport[ PDFBOTTOM ] := nTemp
                  ENDIF
               ELSE
                  IF nTemp > s_aReport[ PDFTOP ]
                     s_aReport[ PDFTOP ] := nTemp
                  ENDIF
               ENDIF
            ELSEIF s_aReport[ HEADER ][ nI ][ 7 ] == "D"
               nTemp := s_aReport[ HEADER ][ nI ][ 5 ] / s_aReport[ LPI ]
               IF s_aReport[ HEADER ][ nI ][ 5 ] > s_aReport[ PAGEY ] / 2
                  IF nTemp < s_aReport[ PDFBOTTOM ]
                     s_aReport[ PDFBOTTOM ] := nTemp
                  ENDIF
               ELSE
                  IF nTemp > s_aReport[ PDFTOP ]
                     s_aReport[ PDFTOP ] := nTemp
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   NEXT
   s_aReport[ MARGINS ] := .F.

RETURN NIL

/*
 * Function: pdfCreateHeader
 * Description: Creates a header for the PDF.
 * Parameters:
 *   _file - The file to save the header to.
 *   _size - The page size.
 *   _orient - The page orientation.
 *   _lpi - The lines per inch.
 *   _width - The report width.
 * Returns: NIL
 */
FUNCTION pdfCreateHeader( _file, _size, _orient, _lpi, _width )

   LOCAL s_aReportStyle := { ;
      { 1, 2, 3, 4, 5, 6 }, ;
      { 2.475, 4.0, 4.9, 6.4, 7.5, 64.0 }, ;
      { 3.3, 5.4, 6.5, 8.6, 10.0, 85.35 }, ;
      { 2.475, 4.0, 4.9, 6.4, 7.5, 48.9 }, ;
      { 3.3, 5.4, 6.5, 8.6, 10.0, 65.2 }, ;
      { 2.475, 4.0, 4.9, 6.4, 7.5, 82.0 }, ;
      { 3.3, 5.4, 6.5, 8.6, 10.0, 109.35 } }
   LOCAL nStyle := 1, nAdd := 0.00

   __defaultNIL( @_SIZE, s_aReport[ PAGESIZE ] )
   __defaultNIL( @_orient, s_aReport[ PAGEORIENT ] )
   __defaultNIL( @_lpi, s_aReport[ LPI ] )
   __defaultNIL( @_WIDTH, 200 )

   IF _size == "LETTER"
      IF _orient == "P"
         IF _lpi == 6
            nStyle := 2
         ELSEIF _lpi == 8
            nStyle := 3
         ENDIF
      ELSEIF _orient == "L"
         IF _lpi == 6
            nStyle := 4
         ELSEIF _lpi == 8
            nStyle := 5
         ENDIF
      ENDIF
   ELSEIF _size == "LEGAL"
      IF _orient == "P"
         IF _lpi == 6
            nStyle := 6
         ELSEIF _lpi == 8
            nStyle := 7
         ENDIF
      ELSEIF _orient == "L"
         IF _lpi == 6
            nStyle := 4
         ELSEIF _lpi == 8
            nStyle := 5
         ENDIF
      ENDIF
   ENDIF

   pdfEditOnHeader()
   IF _size == "LEGAL"
      nAdd := 76.2
   ENDIF

   IF _orient == "P"
      pdfBox( 5.0, 5.0, 274.0 + nAdd, 210.0, 1.0 )
      pdfBox( 6.5, 6.5, 272.5 + nAdd, 208.5, 0.5 )
      pdfBox( 11.5, 9.5, 22.0, 205.5, 0.5, 5 )
      pdfBox( 23.0, 9.5, 33.5, 205.5, 0.5, 5 )
      pdfBox( 34.5, 9.5, 267.5 + nAdd, 205.5, 0.5 )
   ELSE
      pdfBox( 5.0, 5.0, 210.0, 274.0 + nAdd, 1.0 )
      pdfBox( 6.5, 6.5, 208.5, 272.5 + nAdd, 0.5 )
      pdfBox( 11.5, 9.5, 22.0, 269.5 + nAdd, 0.5, 5 )
      pdfBox( 23.0, 9.5, 33.5, 269.5 + nAdd, 0.5, 5 )
      pdfBox( 34.5, 9.5, 203.5, 269.5 + nAdd, 0.5 )
   ENDIF

   pdfSetFont( "Helvetica", BOLD, 10 )
   pdfAtSay( "Test Line 1", s_aReportStyle[ nStyle ][ 1 ], 1, "R", .T. )
   pdfSetFont( "Times", BOLD, 18 )
   pdfCenter( "Test Line 2", s_aReportStyle[ nStyle ][ 2 ],, "R", .T. )
   pdfSetFont( "Times", BOLD, 12 )
   pdfCenter( "Test Line 3", s_aReportStyle[ nStyle ][ 3 ],, "R", .T. )
   pdfSetFont( "Helvetica", BOLD, 10 )
   pdfAtSay( "Test Line 4", s_aReportStyle[ nStyle ][ 4 ], 1, "R", .T. )
   pdfSetFont( "Helvetica", BOLD, 10 )
   pdfAtSay( "Test Line 5", s_aReportStyle[ nStyle ][ 5 ], 1, "R", .T. )
   pdfAtSay( DToC( Date() ) + " " + TimeAsAMPM( Time() ), s_aReportStyle[ nStyle ][ 6 ], 1, "R", .T. )
   pdfRJust( "Page: #pagenumber#", s_aReportStyle[ nStyle ][ 6 ], s_aReport[ REPORTWIDTH ], "R", .T. )

   pdfEditOffHeader()
   pdfSaveHeader( _file )

RETURN NIL

/*
 * Function: pdfImageInfo
 * Description: Gets information about an image.
 * Parameters:
 *   cFile - The image file.
 * Returns: An array with image information.
 */
FUNCTION pdfImageInfo( cFile )

   LOCAL cTemp := Upper( SubStr( cFile, RAt( ".", cFile ) + 1 ) ), aTemp := {}

   DO CASE
   CASE cTemp == "TIF"
      aTemp := pdfTIFFInfo( cFile )
   CASE cTemp == "JPG"
      aTemp := pdfJPEGInfo( cFile )
   ENDCASE

RETURN aTemp

/*
 * Function: pdfTIFFInfo
 * Description: Gets information about a TIFF image.
 * Parameters:
 *   cFile - The TIFF file.
 * Returns: An array with TIFF information.
 */
FUNCTION pdfTIFFInfo( cFile )

   LOCAL c40 := Chr( 0 ) + Chr( 0 ) + Chr( 0 ) + Chr( 0 )
   LOCAL aCount := { 1, 1, 2, 4, 8, 1, 1, 2, 4, 8, 4, 8 }
   LOCAL nTemp, nHandle, cValues, c2, nFieldType, nCount, nPos, nTag, nValues
   LOCAL nOffset, cTemp, cIFDNext, nIFD, nFields, nn
   LOCAL nWidth := 0, nHeight := 0, nBits := 0, nFrom := 0, nLength := 0, xRes := 0, yRes := 0, aTemp := {}, nSpace

   nHandle := FOpen( cFile )
   c2 := "  "
   FRead( nHandle, @c2, 2 )
   FRead( nHandle, @c2, 2 )
   cIFDNext := "    "
   FRead( nHandle, @cIFDNext, 4 )
   cTemp := Space( 12 )

   WHILE !( cIFDNext == c40 )
      nIFD := Bin2L( cIFDNext )
      FSeek( nHandle, nIFD )
      FRead( nHandle, @c2, 2 )
      nFields := Bin2I( c2 )
      FOR nn := 1 TO nFields
         FRead( nHandle, @cTemp, 12 )
         nTag := Bin2W( SubStr( cTemp, 1, 2 ) )
         nFieldType := Bin2W( SubStr( cTemp, 3, 2 ) )
         nCount := Bin2L( SubStr( cTemp, 5, 4 ) )
         nOffset := Bin2L( SubStr( cTemp, 9, 4 ) )

         IF nCount > 1 .OR. nFieldType == RATIONAL .OR. nFieldType == SRATIONAL
            nPos := filepos( nHandle )
            FSeek( nHandle, nOffset )
            nValues := nCount * aCount[ nFieldType ]
            cValues := Space( nValues )
            FRead( nHandle, @cValues, nValues )
            FSeek( nHandle, nPos )
         ELSE
            cValues := SubStr( cTemp, 9, 4 )
         ENDIF

         IF nFieldType == ASCII
            --nCount
         ENDIF

         DO CASE
         CASE nTag == 256
            IF nFieldType == SHORT
               nWidth := Bin2W( SubStr( cValues, 1, 2 ) )
            ELSEIF nFieldType == LONG
               nWidth := Bin2L( SubStr( cValues, 1, 4 ) )
            ENDIF
         CASE nTag == 257
            IF nFieldType == SHORT
               nHeight := Bin2W( SubStr( cValues, 1, 2 ) )
            ELSEIF nFieldType == LONG
               nHeight := Bin2L( SubStr( cValues, 1, 4 ) )
            ENDIF
         CASE nTag == 258
            nTemp := 0
            IF nFieldType == SHORT
               nTemp := Bin2W( cValues )
            ENDIF
            nBits := nTemp
         CASE nTag == 262
            nTemp := -1
            IF nFieldType == SHORT
               nTemp := Bin2W( cValues )
            ENDIF
            IF nTemp != 0 .AND. nTemp != 1 .AND. nTemp != 2 .AND. nTemp != 3
            ENDIF
         CASE nTag == 273
            IF nFieldType == SHORT
               nFrom := Bin2W( SubStr( cValues, 1, 2 ) )
            ELSEIF nFieldType == LONG
               nFrom := Bin2L( SubStr( cValues, 1, 4 ) )
            ENDIF
         CASE nTag == 279
            IF nFieldType == SHORT
               nLength := Bin2W( SubStr( cValues, 1, 2 ) )
            ELSEIF nFieldType == LONG
               nLength := Bin2L( SubStr( cValues, 1, 4 ) )
            ENDIF
            nLength *= nCount
         CASE nTag == 282
            IF nFieldType == RATIONAL
               xRes := Bin2L( SubStr( cValues, 1, 4 ) )
            ENDIF
         CASE nTag == 283
            IF nFieldType == RATIONAL
               yRes := Bin2L( SubStr( cValues, 1, 4 ) )
            ENDIF
         ENDCASE
      NEXT
      FRead( nHandle, @cIFDNext, 4 )
   ENDDO

   FClose( nHandle )
   AAdd( aTemp, nWidth )
   AAdd( aTemp, nHeight )
   AAdd( aTemp, xRes )
   AAdd( aTemp, yRes )
   AAdd( aTemp, nBits )
   AAdd( aTemp, nFrom )
   AAdd( aTemp, nLength )
   nSpace := 0
   AAdd( aTemp, nSpace )

RETURN aTemp

/*
 * Function: pdfJPEGInfo
 * Description: Gets information about a JPEG image.
 * Parameters:
 *   cFile - The JPEG file.
 * Returns: An array with JPEG information.
 */
FUNCTION pdfJPEGInfo( cFile )

   LOCAL c255, nAt, nHandle
   LOCAL nWidth, nHeight, nBits := 8, nFrom := 0, nLength, xRes, yRes, aTemp := {}
   LOCAL nBuffer := 20000
   LOCAL nSpace

   nHandle := FOpen( cFile )
   c255 := Space( nBuffer )
   FRead( nHandle, @c255, nBuffer )
   xRes := Asc( SubStr( c255, 15, 1 ) ) * 256 + Asc( SubStr( c255, 16, 1 ) )
   yRes := Asc( SubStr( c255, 17, 1 ) ) * 256 + Asc( SubStr( c255, 18, 1 ) )
   nAt := RAt( Chr( 255 ) + Chr( 192 ), c255 ) + 5
   nHeight := Asc( SubStr( c255, nAt, 1 ) ) * 256 + Asc( SubStr( c255, nAt + 1, 1 ) )
   nWidth := Asc( SubStr( c255, nAt + 2, 1 ) ) * 256 + Asc( SubStr( c255, nAt + 3, 1 ) )
   nSpace := Asc( SubStr( c255, nAt + 4, 1 ) )
   nLength := FileSize( nHandle )
   FClose( nHandle )

   AAdd( aTemp, nWidth )
   AAdd( aTemp, nHeight )
   AAdd( aTemp, xRes )
   AAdd( aTemp, yRes )
   AAdd( aTemp, nBits )
   AAdd( aTemp, nFrom )
   AAdd( aTemp, nLength )
   AAdd( aTemp, nSpace )

RETURN aTemp

/*
 * Function: FilePos
 * Description: Gets the current file position.
 * Parameters:
 *   nHandle - The file handle.
 * Returns: The current file position.
 */
STATIC FUNCTION FilePos( nHandle )
RETURN FSeek( nHandle, 0, FS_RELATIVE )

/*
 * Function: Chr_RGB
 * Description: Converts a character to an RGB value.
 * Parameters:
 *   cChar - The character to convert.
 * Returns: The RGB value.
 */
STATIC FUNCTION Chr_RGB( cChar )
RETURN Str( Asc( cChar ) / 255, 4, 2 )

/*
 * Function: NumToken
 * Description: Counts the number of tokens in a string.
 * Parameters:
 *   cString - The string to count.
 *   cDelimiter - The delimiter.
 * Returns: The number of tokens.
 */
STATIC FUNCTION NumToken( cString, cDelimiter )
RETURN AllToken( cString, cDelimiter )

/*
 * Function: Token
 * Description: Gets a token from a string.
 * Parameters:
 *   cString - The string to get the token from.
 *   cDelimiter - The delimiter.
 *   nPointer - The token index.
 * Returns: The token.
 */
STATIC FUNCTION Token( cString, cDelimiter, nPointer )
RETURN AllToken( cString, cDelimiter, nPointer, 1 )

/*
 * Function: AtToken
 * Description: Gets the position of a token in a string.
 * Parameters:
 *   cString - The string to get the position from.
 *   cDelimiter - The delimiter.
 *   nPointer - The token index.
 * Returns: The position of the token.
 */
STATIC FUNCTION AtToken( cString, cDelimiter, nPointer )
RETURN AllToken( cString, cDelimiter, nPointer, 2 )

/*
 * Function: AllToken
 * Description: Gets all tokens from a string.
 * Parameters:
 *   cString - The string to get the tokens from.
 *   cDelimiter - The delimiter.
 *   nPointer - The token index.
 *   nAction - The action to perform.
 * Returns: The tokens or their positions.
 */
STATIC FUNCTION AllToken( cString, cDelimiter, nPointer, nAction )

   LOCAL nTokens := 0, nPos := 1, nLen := Len( cString ), nStart, cRet

   __defaultNIL( @cDelimiter, Chr( 0 ) + Chr( 9 ) + Chr( 10 ) + Chr( 13 ) + Chr( 26 ) + Chr( 32 ) + Chr( 138 ) + Chr( 141 ) )
   __defaultNIL( @nAction, 0 )
   /*
     nAction == 0 - numtoken
     nAction == 1 - token
     nAction == 2 - attoken
    */
   WHILE nPos <= nLen
      IF ! SubStr( cString, nPos, 1 ) $ cDelimiter
         nStart := nPos
         WHILE nPos <= nLen .AND. ! SubStr( cString, nPos, 1 ) $ cDelimiter
            ++nPos
         ENDDO
         ++nTokens
         IF nAction > 0
            IF nPointer == nTokens
               IF nAction == 1
                  cRet := SubStr( cString, nStart, nPos - nStart )
               ELSE
                  cRet := nStart
               ENDIF
               EXIT
            ENDIF
         ENDIF
      ENDIF
      IF SubStr( cString, nPos, 1 ) $ cDelimiter
         WHILE nPos <= nLen .AND. SubStr( cString, nPos, 1 ) $ cDelimiter
            ++nPos
         ENDDO
      ENDIF
      cRet := nTokens
   ENDDO

RETURN cRet

/*
 * Function: NumAt
 * Description: Counts the number of occurrences of a substring in a string.
 * Parameters:
 *   cSearch - The substring to search for.
 *   cString - The string to search in.
 * Returns: The number of occurrences.
 */
STATIC FUNCTION NumAt( cSearch, cString )

   LOCAL n := 0, nAt, nPos := 0

   WHILE ( nAt := At( cSearch, SubStr( cString, nPos + 1 ) ) ) > 0
      nPos += nAt
      ++n
   ENDDO

RETURN n

/*
 * Function: FileSize
 * Description: Gets the size of a file.
 * Parameters:
 *   nHandle - The file handle.
 * Returns: The size of the file.
 */
STATIC FUNCTION FileSize( nHandle )

   LOCAL nCurrent
   LOCAL nLength

   nCurrent := FilePos( nHandle )
   nLength := FSeek( nHandle, 0, FS_END )
   FSeek( nHandle, nCurrent )

RETURN nLength

/*
 * Function: Array2File
 * Description: Writes an array to a file.
 * Parameters:
 *   cFile - The file to write to.
 *   aRay - The array to write.
 *   nDepth - The depth of the array.
 *   hFile - The file handle.
 * Returns: The number of bytes written.
 */
STATIC FUNCTION Array2File( cFile, aRay, nDepth, hFile )

   LOCAL nBytes := 0
   LOCAL i

   nDepth := iif( HB_ISNUMERIC( nDepth ), nDepth, 0 )
   IF hFile == NIL
      IF ( hFile := FCreate( cFile, FC_NORMAL ) ) == F_ERROR
         RETURN nBytes
      ENDIF
   ENDIF
   nDepth++
   nBytes += WriteData( hFile, aRay )
   IF HB_ISARRAY( aRay )
      FOR i := 1 TO Len( aRay )
         nBytes += Array2File( cFile, aRay[ i ], nDepth, hFile )
      NEXT
   ENDIF
   nDepth--
   IF nDepth == 0
      FClose( hFile )
   ENDIF

RETURN nBytes

/*
 * Function: WriteData
 * Description: Writes data to a file.
 * Parameters:
 *   hFile - The file handle.
 *   xData - The data to write.
 * Returns: The number of bytes written.
 */
STATIC FUNCTION WriteData( hFile, xData )

   LOCAL cData := ValType( xData )

   IF HB_ISSTRING( xData )
      cData += I2Bin( Len( xData ) ) + xData
   ELSEIF HB_ISNUMERIC( xData )
      cData += I2Bin( Len( hb_ntos( xData ) ) ) + hb_ntos( xData )
   ELSEIF HB_ISDATE( xData )
      cData += I2Bin( 8 ) + DToS( xData )
   ELSEIF HB_ISLOGICAL( xData )
      cData += I2Bin( 1 ) + iif( xData, "T", "F" )
   ELSEIF HB_ISARRAY( xData )
      cData += I2Bin( Len( xData ) )
   ELSE
      cData += I2Bin( 0 )
   ENDIF

RETURN FWrite( hFile, cData, Len( cData ) )

/*
 * Function: File2Array
 * Description: Reads an array from a file.
 * Parameters:
 *   cFile - The file to read from.
 *   nLen - The length of the array.
 *   hFile - The file handle.
 * Returns: The array.
 */
STATIC FUNCTION File2Array( cFile, nLen, hFile )

   LOCAL cData, cType, nDataLen, nBytes
   LOCAL nDepth := 0
   LOCAL aRay := {}

   IF hFile == NIL
      IF ( hFile := FOpen( cFile, FO_READ ) ) == F_ERROR
         RETURN aRay
      ENDIF
      cData := Space( 3 )
      FRead( hFile, @cData, 3 )
      IF !( Left( cData, 1 ) == "A" )
         RETURN aRay
      ENDIF
      nLen := Bin2I( Right( cData, 2 ) )
   ENDIF
   DO WHILE nDepth < nLen
      cData := Space( 3 )
      nBytes := FRead( hFile, @cData, 3 )
      IF nBytes < 3
         EXIT
      ENDIF
      cType := PadL( cData, 1 )
      nDataLen := Bin2I( Right( cData, 2 ) )
      IF !( cType == "A" )
         cData := Space( nDataLen )
         nBytes := FRead( hFile, @cData, nDataLen )
         IF nBytes < nDataLen
            EXIT
         ENDIF
      ENDIF
      nDepth++
      AAdd( aRay, NIL )
      IF cType == "C"
         aRay[ nDepth ] := cData
      ELSEIF cType == "N"
         aRay[ nDepth ] := Val( cData )
      ELSEIF cType == "D"

#ifdef __XHARBOUR__
         aRay[ nDepth ] := CToD( Left( cData, 4 ) + "/" + SubStr( cData, 5, 2 ) + "/" + SubStr( cData, 7, 2 ) )
#else
         aRay[ nDepth ] := hb_SToD( cData )
#endif
      ELSEIF cType == "L"
         aRay[ nDepth ] := ( cData == "T" )
      ELSEIF cType == "A"
         aRay[ nDepth ] := File2Array(, nDataLen, hFile )
      ENDIF
   ENDDO
   IF cFile != NIL
      FClose( hFile )
   ENDIF

RETURN aRay
