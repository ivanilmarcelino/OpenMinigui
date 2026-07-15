/*
 * (c) Alfredo Arteaga, 2001-2002 ( Original Idea )
 *
 * (c) Grigory Filatov, 2003-2004 ( Translation for MiniGUI )
 *
 * (c) Siri Rathinagiri, 2016 ( Adaptation for Draw in Bitmap )
 */

#ifdef __XHARBOUR__
#pragma -w2
#define __MINIPRINT__
#endif

#include "hmg.ch"

#ifdef _HMG_COMPAT_

#define COLOR_WINDOWTEXT     8
#define COLOR_BTNFACE       15

*=============================================================================*
*                          Public Functions
*=============================================================================*

/*
 * FUNCTION HMG_Graph( nWidth, nHeight, aData, cTitle, aYVals, nBarD, nWideB, nSep, aTitleColor, nXRanges, ;
 *      l3D, lGrid, lxGrid, lyGrid, lxVal, lyVal, lLegends, aSeries, aColors, nType, lViewVal, cPicture, nLegendsWidth, lNoborder )
 *
 * Draws a graph onto a bitmap using the provided data and parameters.
 *
 * Parameters:
 *   nWidth        : The width of the graph area in pixels.
 *   nHeight       : The height of the graph area in pixels.
 *   aData         : A two-dimensional array containing the data to be plotted. Each row represents a series, and each column represents a data point.
 *   cTitle        : The title of the graph (optional, defaults to "").
 *   aYVals        : An array of labels for the Y-axis (optional).
 *   nBarD         : The depth of the bars in 3D mode (optional, defaults to a reasonable value).
 *   nWideB        : The width of the bars in bar graph mode (optional, defaults to a reasonable value).
 *   nSep          : The separation between bars in bar graph mode (optional, defaults to 0).
 *   aTitleColor   : The color of the graph title (optional, defaults to the system window text color).
 *   nXRanges      : The number of ranges to display on the X-axis (optional).
 *   l3D           : A logical value indicating whether to draw the graph in 3D mode (.T.) or not (.F.).
 *   lGrid         : A logical value indicating whether to draw grid lines (.T.) or not (.F.).
 *   lxGrid        : A logical value indicating whether to draw X-axis grid lines (.T.) or not (.F.).
 *   lyGrid        : A logical value indicating whether to draw Y-axis grid lines (.T.) or not (.F.).
 *   lxVal         : A logical value indicating whether to display X-axis values (.T.) or not (.F.).
 *   lyVal         : A logical value indicating whether to display Y-axis values (.T.) or not (.F.).
 *   lLegends      : A logical value indicating whether to display legends (.T.) or not (.F.).
 *   aSeries       : An array of strings containing the names of the data series.
 *   aColors       : An array of colors to use for each data series.
 *   nType         : The type of graph to draw (BARS, LINES, or POINTS).
 *   lViewVal      : A logical value indicating whether to display the values on the graph (.T.) or not (.F.).
 *   cPicture      : A picture string used to format the values displayed on the graph (optional, defaults to "999,999.99").
 *   nLegendsWidth : The width of the legends area in pixels (optional, defaults to 50).
 *   lNoborder     : A logical value indicating whether to draw a border around the graph (.F.) or not (.T.).
 *
 * Returns:
 *   hBitmap       : A handle to the bitmap containing the drawn graph.
 *
 * Purpose:
 *   This function provides a flexible way to generate various types of graphs (bar, line, point) directly onto a bitmap.
 *   It's used to visually represent data in a graphical format within HMG applications, allowing for customization
 *   of appearance through various parameters like colors, titles, legends, and 3D effects.  For example, it can be used to
 *   display sales trends, performance metrics, or any other data that benefits from visual representation.
 *
 * Notes:
 *   - The lengths of the aSeries and aData arrays must match.
 *   - The length of the aColors array should be greater than or equal to the length of the aSeries array.
 *   - The function relies on several auxiliary functions (DrawWindowBoxInBitmap, DrawRectInBitmap, etc.) to perform the actual drawing operations.
 *   - The function uses GetSysColor to determine the default colors for window text and button faces.
 */
FUNCTION HMG_Graph( nWidth, nHeight, aData, cTitle, aYVals, nBarD, nWideB, nSep, aTitleColor, nXRanges, ;
      l3D, lGrid, lxGrid, lyGrid, lxVal, lyVal, lLegends, aSeries, aColors, nType, lViewVal, cPicture, nLegendsWidth, lNoborder )
                 
   LOCAL hBitmap, hDC, BTStruct
   LOCAL nTop := 0
   LOCAL nLeft := 0
   LOCAL nBottom := nHeight
   LOCAL nRight := nWidth
   LOCAL nI, nJ, nPos, nMax, nMin, nMaxBar, nDeep
   LOCAL nRange, nSeries, nResH, nResV, nWide, aPoint
   LOCAL nXMax, nXMin, nHigh, nRel, nZero, nRPos, nRNeg
   LOCAL nClrFore  := GetSysColor( COLOR_WINDOWTEXT ), lRedraw
   LOCAL aClrFore := nRGB2Arr( nClrFore )
   LOCAL nClrBack := GetSysColor( COLOR_BTNFACE )
   LOCAL aClrBack := nRGB2Arr( nClrBack )

   DEFAULT cTitle := "", nSep := 0, nLegendsWidth := 50, cPicture := "999,999.99"

   IF ( Len ( aSeries ) != Len ( aData ) ) .OR. ( Len ( aSeries ) > Len ( aColors ) )
      MsgMiniGuiError( "DRAW GRAPH: 'Series' / 'SerieNames' / 'Colors' arrays size mismatch." )
   ENDIF

   hBitmap := BT_BitmapCreateNew ( nWidth, nHeight, aClrBack )
   hDC := BT_CreateDC( hBitmap, BT_HDC_BITMAP, @BTStruct )

   IF lGrid
      lxGrid := lyGrid := .T.
   ENDIF

   IF nBottom != NIL .AND. nRight != NIL
      nHeight := nBottom - nTop / 2
      nWidth  := nRight - nLeft / 2
   ENDIF

   nTop    := nTop + 1 + iif( Empty( cTitle ), 30, 44 )                        // Top gap
   nLeft   := nLeft + 1 + iif( lxVal, 30 + nLegendsWidth + nBarD, 30 + nBarD ) // Left
   nBottom := nHeight - 2 - iif( lyVal, 40, 30 )                               // Bottom
   nRight  := nWidth - 2 - iif( lLegends, 30 + nLegendsWidth, 30 )             // Right

   l3D     := iif( nType == POINTS, .F., l3D )
   nDeep   := iif( l3D, nBarD, 1 )
   nMaxBar := nBottom - nTop - nDeep - 5
   nResH   := nResV := 1
   nWide   := ( nRight - nLeft ) * nResH / ( nMax( aData ) + 1 ) * nResH

   // Graph area
   //
   IF !lNoborder
      DrawWindowBoxInBitmap( hDC, Max( 1, nTop - 44 ), Max( 1, nLeft - 80 - nBarD ), nHeight - 1, nWidth - 1 )
   ENDIF

   // Back area
   //
   IF l3D
      drawrectInBitmap( hDC, nTop + 1, nLeft, nBottom - nDeep, nRight, WHITE )
   ELSE
      drawrectInBitmap( hDC, nTop - 5, nLeft, nBottom, nRight, WHITE )
   ENDIF

   IF l3D
      // Bottom area
      FOR nI := 1 TO nDeep + 1
         DrawLineInBitmap( hDC, nBottom - nI, nLeft - nDeep + nI, nBottom - nI, nRight - nDeep + nI, WHITE )
      NEXT nI

      // Lateral
      FOR nI := 1 TO nDeep
         DrawLineInBitmap( hDC, nTop + nI, nLeft - nI, nBottom - nDeep + nI, nLeft - nI, SILVER )
      NEXT nI

      // Graph borders
      FOR nI := 1 TO nDeep+1
         DrawLineInBitmap( hDC, nBottom-nI     , nLeft-nDeep+nI-1 , nBottom-nI     , nLeft-nDeep+nI  , GRAY )
         DrawLineInBitmap( hDC, nBottom-nI     , nRight-nDeep+nI-1, nBottom-nI     , nRight-nDeep+nI , BLACK )
         DrawLineInBitmap( hDC, nTop+nDeep-nI+1, nLeft-nDeep+nI-1 , nTop+nDeep-nI+1, nLeft-nDeep+nI  , BLACK )
         DrawLineInBitmap( hDC, nTop+nDeep-nI+1, nLeft-nDeep+nI-3 , nTop+nDeep-nI+1, nLeft-nDeep+nI-2, BLACK )
      NEXT nI

      FOR nI := 1 TO nDeep+2
         DrawLineInBitmap( hDC, nTop+nDeep-nI+1, nLeft-nDeep+nI-3, nTop+nDeep-nI+1, nLeft-nDeep+nI-2 , BLACK )
         DrawLineInBitmap( hDC, nBottom+ 2-nI+1, nRight-nDeep+nI , nBottom+ 2-nI+1, nRight-nDeep+nI-2, BLACK )
      NEXT nI

      DrawLineInBitmap( hDC, nTop         , nLeft        , nTop           , nRight       , BLACK )
      DrawLineInBitmap( hDC, nTop- 2      , nLeft        , nTop- 2        , nRight+ 2    , BLACK )
      DrawLineInBitmap( hDC, nTop         , nLeft        , nBottom-nDeep  , nLeft        , GRAY  )
      DrawLineInBitmap( hDC, nTop+nDeep   , nLeft-nDeep  , nBottom        , nLeft-nDeep  , BLACK )
      DrawLineInBitmap( hDC, nTop+nDeep   , nLeft-nDeep-2, nBottom+ 2     , nLeft-nDeep-2, BLACK )
      DrawLineInBitmap( hDC, nTop         , nRight       , nBottom-nDeep  , nRight       , BLACK )
      DrawLineInBitmap( hDC, nTop- 2      , nRight+ 2    , nBottom-nDeep+2, nRight+ 2    , BLACK )
      DrawLineInBitmap( hDC, nBottom-nDeep, nLeft        , nBottom-nDeep  , nRight       , GRAY  )
      DrawLineInBitmap( hDC, nBottom      , nLeft-nDeep  , nBottom        , nRight-nDeep , BLACK )
      DrawLineInBitmap( hDC, nBottom+ 2   , nLeft-nDeep-2, nBottom+ 2     , nRight-nDeep , BLACK )
   ENDIF

   // Graph info
   //
   IF !Empty( cTitle )
      DrawTextInBitmap( hDC, nTop - 33 * nResV, ( nWidth - GetTextWidth( hDC, cTitle ) / iif( Len( cTitle ) > 40, 10, 12 ) ) / 2 + 1, cTitle, _HMG_DefaultFontName, _HMG_DefaultFontSize + 3, aTitleColor, 2 )
   ENDIF

   // Legends
   //
   IF lLegends
      nPos := nTop
      FOR nI := 1 TO Len( aSeries )
         DrawBarInBitmap( hDC, nRight + ( ( _HMG_DefaultFontSize - 1 ) * nResH ), nPos + _HMG_DefaultFontSize * nResV,;
            ( _HMG_DefaultFontSize - 1 ) * nResH, ( _HMG_DefaultFontSize - 2 ) * nResV, l3D, 1, aColors[nI] )
         DrawTextInBitmap( hDC, nPos, nRight + ( 4 + 2 * _HMG_DefaultFontSize ) * nResH, aSeries[ nI ], _HMG_DefaultFontName, _HMG_DefaultFontSize - 1, aClrFore, 0 )
         nPos += ( _HMG_DefaultFontSize + 7 ) * nResV
      NEXT nI
   ENDIF

   // Max, Min values
   //
   nMax := nMin := 0
   FOR nJ := 1 TO Len( aSeries )
      FOR nI := 1 TO Len( aData[nJ] )
         nMax := Max( aData[nJ][nI], nMax )
         nMin := Min( aData[nJ][nI], nMin )
      NEXT nI
   NEXT nJ

   nXMax   := iif( nMax > 0, DetMaxVal( nMax ), 0 )
   nXMin   := iif( nMin < 0, DetMaxVal( nMin ), 0 )
   nHigh   := nXMax + nXMin
   nMax    := Max( nXMax, nXMin )

   nRel    := nMaxBar / nHigh
   nMaxBar := nMax * nRel

   // Zero position
   //
   IF nXMax > nXMin
      nZero := nTop + nMaxBar + nDeep + 5
   ELSE
      nZero := nBottom - nMaxBar - 5
   ENDIF
   IF l3D
      FOR nI := 1 TO nDeep + 1
         DrawLineInBitmap( hDC, nZero - nI + 1, nLeft - nDeep + nI, nZero - nI + 1, nRight - nDeep + nI, SILVER )
      NEXT nI
      FOR nI := 1 TO nDeep + 1
         DrawLineInBitmap( hDC, nZero - nI + 1, nLeft - nDeep + nI - 1 , nZero - nI + 1, nLeft - nDeep + nI , GRAY )
         DrawLineInBitmap( hDC, nZero - nI + 1, nRight - nDeep + nI - 1, nZero - nI + 1, nRight - nDeep + nI, BLACK )
      NEXT nI
      DrawLineInBitmap( hDC, nZero - nDeep, nLeft, nZero - nDeep, nRight, GRAY )
   ENDIF

   aPoint := Array( Len( aSeries ), Len( aData[1] ), 2 )
   nRange := nMax / nXRanges

   // xLabels
   //
   nRPos := nRNeg := nZero - nDeep
   FOR nI := 0 TO nXRanges
      IF lxVal
         IF nRange * nI <= nXMax
            DrawTextInBitmap( hDC, nRPos, nLeft - nDeep - 70, Transform( nRange * nI, cPicture ), _HMG_DefaultFontName, _HMG_DefaultFontSize - 1, aClrFore )
         ENDIF
         IF nRange * ( - nI ) >= nXMin * ( -1 )
           DrawTextInBitmap( hDC, nRNeg, nLeft - nDeep - 70, Transform( nRange *- nI, cPicture ), _HMG_DefaultFontName, _HMG_DefaultFontSize - 1, aClrFore )
         ENDIF
      ENDIF
      IF lxGrid
         IF nRange * nI <= nXMax
            IF l3D
               FOR nJ := 0 TO nDeep + 1
                  DrawLineInBitmap( hDC, nRPos + nJ, nLeft - nJ - 1, nRPos + nJ, nLeft - nJ, BLACK )
               NEXT nJ
            ENDIF
            DrawLineInBitmap( hDC, nRPos, nLeft, nRPos, nRight, BLACK )
         ENDIF
         IF nRange *- nI >= nXMin *- 1
            IF l3D
               FOR nJ := 0 TO nDeep + 1
                  DrawLineInBitmap( hDC, nRNeg + nJ, nLeft - nJ - 1, nRNeg + nJ, nLeft - nJ, BLACK )
               NEXT nJ
            ENDIF
            DrawLineInBitmap( hDC, nRNeg, nLeft, nRNeg, nRight, BLACK )
         ENDIF
      ENDIF
      nRPos -= ( nMaxBar / nXRanges )
      nRNeg += ( nMaxBar / nXRanges )
   NEXT nI

   IF lYGrid
      nPos := iif( l3D, nTop, nTop - 5 )
      nI := nLeft + nWide
      FOR nJ := 1 TO nMax( aData )
         DrawlineInBitmap( hDC, nBottom - nDeep, nI, nPos, nI, { 100, 100, 100 } )
         DrawlineInBitmap( hDC, nBottom, nI - nDeep, nBottom - nDeep, nI, { 100, 100, 100 } )
         nI += nWide
      NEXT
   ENDIF

   nRange := Len( aData[1] )
   nSeries := Len( aSeries )
   DO WHILE .T.    // Bar adjust
      nPos := nLeft + nWide / 2
      nPos += ( nWide + nSep ) * ( nSeries + 1 ) * nRange
      IF nPos > nRight
         nWide--
      ELSE
         EXIT
      ENDIF
   ENDDO

   nMin := nMax / nMaxBar
   nPos := nLeft + ( nWide + nSep ) / 2            // first point graph

   // yLabels
   //
   IF lyVal .AND. Len( aYVals ) > 0
      nWideB := ( nRight - nLeft ) / ( nMax( aData ) + 1 )
      nI := nLeft + nWideB
      FOR nJ := 1 TO nMax( aData )
         DrawTextInBitmap( hDC, nBottom + 8, nI - iif( l3D, nDeep, nDeep + 8 ), aYVals[ nJ ], _HMG_DefaultFontName, _HMG_DefaultFontSize - 1, aClrFore )
         nI += nWideB
      NEXT
   ENDIF

   // Bars
   //
   IF nType == BARS
      nPos := nLeft + ( nWide + nSep ) / 2
      lRedraw := ( nSeries == 1 .AND. Len( aColors ) >= nRange )
      FOR nI := 1 TO nRange
         FOR nJ := 1 TO nSeries
            DrawBarInBitmap( hDC, nPos, iif( l3D, nZero, nZero - 1 ), aData[nJ,nI] / nMin + nDeep, nWide, l3D, nDeep, ;
               aColors[ iif(lRedraw, nI, nJ) ] )
            nPos += nWide + nSep
         NEXT nJ
         nPos += nWide + nSep
      NEXT nI
   ENDIF

   // Lines
   //
   IF nType == LINES
      nWideB := ( nRight - nLeft ) / ( nMax( aData ) + 1 )
      nPos := nLeft + nWideB
      FOR nI := 1 TO nRange
         FOR nJ := 1 TO nSeries
            IF !l3D
               DrawPointInBitmap( hDC, nType, nPos, nZero, aData[nJ,nI] / nMin + nDeep, aColors[nJ] )
            ENDIF
            aPoint[nJ,nI,2] := nPos
            aPoint[nJ,nI,1] := nZero - ( aData[nJ,nI] / nMin + nDeep )
         NEXT nJ
         nPos += nWideB
      NEXT nI

      FOR nI := 1 TO nRange - 1
         FOR nJ := 1 TO nSeries
            IF l3D
               drawpolygonInBitmap( hDC, { { aPoint[nJ,nI,1],aPoint[nJ,nI,2] }, { aPoint[nJ,nI+1,1],aPoint[nJ,nI+1,2] }, ;
                  { aPoint[nJ,nI+1,1] - nDeep, aPoint[nJ,nI+1,2] + nDeep }, { aPoint[nJ,nI,1] - nDeep, aPoint[nJ,nI,2] + nDeep }, ;
                  { aPoint[nJ,nI,1], aPoint[nJ,nI,2] } }, , , aColors[nJ] )
            ELSE
               DrawLineInBitmap( hDC, aPoint[nJ,nI,1], aPoint[nJ,nI,2], aPoint[nJ,nI+1,1], aPoint[nJ,nI+1,2], aColors[nJ] )
            ENDIF
         NEXT nI
      NEXT nI

   ENDIF

   // Points
   //
   IF nType == POINTS
      nWideB := ( nRight - nLeft ) / ( nMax( aData ) + 1 )
      nPos := nLeft + nWideB
      FOR nI := 1 TO nRange
         FOR nJ := 1 TO nSeries
            DrawPointInBitmap( hDC, nType, nPos, nZero, aData[nJ,nI] / nMin + nDeep, aColors[nJ] )
         NEXT nJ
         nPos += nWideB
      NEXT nI
   ENDIF

   IF lViewVal
      IF nType == BARS
         nPos := nLeft + nWide + ( nWide + nSep ) * ( nSeries / 2 )
      ELSE
         nWideB := ( nRight - nLeft ) / ( nMax( aData ) + 1 )
         nPos := nLeft + nWideB
      ENDIF
      FOR nI := 1 TO nRange
         FOR nJ := 1 TO nSeries
            DrawTextInBitmap( hDC, nZero - ( aData[nJ,nI] / nMin + 2 * nDeep ), iif( nType == BARS, nPos - iif( l3D, 38, 18 ), nPos + 8 ), Transform( aData[nJ,nI], cPicture ), _HMG_DefaultFontName, _HMG_DefaultFontSize - 1, aClrFore )
            nPos += iif( nType == BARS, nWide + nSep, 0 )
         NEXT nJ
         IF nType == BARS
            nPos += nWide + nSep
         ELSE
            nPos += nWideB
         ENDIF
      NEXT nI
   ENDIF

   IF l3D
      DrawLineInBitmap( hDC, nZero, nLeft - nDeep, nZero, nRight - nDeep, BLACK )
   ELSE
      IF nXMax <> 0 .AND. nXMin <> 0
         DrawLineInBitmap( hDC, nZero - 1, nLeft + 1, nZero - 1, nRight - 1, RED )
      ENDIF
   ENDIF

   BT_DeleteDC( BTstruct )

RETURN hBitmap

/*
 * FUNCTION HMG_PieGraph( nWidth, nHeight, series, aname, colors, ctitle, aTitleColor, depth, l3d, lxval, lsleg, lNoborder, cPicture, placement )
 *
 * Draws a pie chart onto a bitmap using the provided data and parameters.
 *
 * Parameters:
 *   nWidth      : The width of the pie chart area in pixels.
 *   nHeight     : The height of the pie chart area in pixels.
 *   series      : An array of numerical values representing the data for each slice of the pie chart.
 *   aname       : An array of strings containing the names of each slice in the pie chart.
 *   colors      : An array of colors to use for each slice of the pie chart.
 *   ctitle      : The title of the pie chart (optional).
 *   aTitleColor : The color of the pie chart title (optional).
 *   depth       : The depth of the pie chart in 3D mode (optional).
 *   l3d         : A logical value indicating whether to draw the pie chart in 3D mode (.T.) or not (.F.).
 *   lxval       : A logical value indicating whether to display the values in the legends (.T.) or not (.F.).
 *   lsleg       : A logical value indicating whether to display the legends (.T.) or not (.F.).
 *   lNoborder   : A logical value indicating whether to draw a border around the pie chart (.F.) or not (.T.).
 *   cPicture    : A picture string used to format the values displayed in the legends (optional, defaults to "999,999.99").
 *   placement   : The placement of the legends ("bottom" or "right", optional, defaults to "bottom").
 *
 * Returns:
 *   hBitmap     : A handle to the bitmap containing the drawn pie chart.
 *
 * Purpose:
 *   This function generates a pie chart on a bitmap, providing a visual representation of data as proportions of a whole.
 *   It's used to display data where the relative size of each category is important.  For example, it can be used to
 *   show market share, budget allocation, or survey results. The function allows for customization of the chart's
 *   appearance, including 3D effects, legends, and value display.
 *
 * Notes:
 *   - The lengths of the series, aname, and colors arrays should match.
 *   - The placement parameter determines the location of the legends.
 *   - The function uses several auxiliary functions (DrawWindowBoxInBitmap, DrawRectInBitmap, drawpieInBitmap, etc.) to perform the actual drawing operations.
 */
FUNCTION HMG_PieGraph( nWidth, nHeight, series, aname, colors, ctitle, aTitleColor, depth, l3d, lxval, lsleg, lNoborder, cPicture, placement )

   LOCAL fromrow := 0
   LOCAL fromcol := 0
   LOCAL torow := nHeight
   LOCAL tocol := nWidth
   LOCAL topleftrow
   LOCAL topleftcol
   LOCAL toprightrow
   LOCAL toprightcol
   LOCAL bottomrightrow
   LOCAL bottomrightcol
   LOCAL bottomleftrow
   LOCAL bottomleftcol
   LOCAL middletopcol
   LOCAL middleleftrow
   LOCAL middleleftcol
   LOCAL middlebottomcol
   LOCAL middlerightrow
   LOCAL middlerightcol
   LOCAL fromradialrow
   LOCAL fromradialcol
   LOCAL toradialrow
   LOCAL toradialcol
   LOCAL degrees := {}
   LOCAL cumulative := {}
   LOCAL j, i, sum, ser_sum := 0
   LOCAL shadowcolor
   LOCAL previos_cumulative := -1
   LOCAL toright := .F.  // default is bottom legend
   LOCAL hDC, hBitmap, BTStruct
   LOCAL nClrBack := GetSysColor( COLOR_BTNFACE )
   LOCAL aClrBack := nRGB2Arr( nClrBack )

   DEFAULT cPicture := "999,999.99"

   shadowcolor := GetProperty( ThisWindow.Name, "BackColor" )
   IF shadowcolor[1] # -1 .AND. shadowcolor[2] # -1 .AND. shadowcolor[3] # -1
      aClrBack := shadowcolor
   ENDIF

   hBitmap := BT_BitmapCreateNew ( nWidth, nHeight, aClrBack )
   hDC := BT_CreateDC( hBitmap, BT_HDC_BITMAP, @BTStruct )

   IF !lNoborder
      DrawWindowBoxInBitmap( hDC, fromrow, fromcol, torow - 1, tocol - 1 )
   ENDIF

   ctitle := AllTrim( ctitle )
   IF Len( ctitle ) > 0
      DrawTextInBitmap( hDC, fromrow + 11, ( tocol - fromcol ) / 2, ctitle, _HMG_DefaultFontName, _HMG_DefaultFontSize + 3, aTitleColor, 2 )
      fromrow += 25 + _HMG_DefaultFontSize
   ENDIF

   IF lsleg
      toright := ( "RIGHT" $ Upper( hb_defaultValue( placement, "bottom" ) ) )
      IF toright
         tocol := 2 / 3 * tocol + 10
      ELSE
         IF Len( aname ) * 20 > ( torow - fromrow - 60 )
            lsleg := .F.
            MsgAlert( "No space for showing legends", "Pie Graph" )
         ELSE
            torow -= ( Len( aname ) * 20 )
         ENDIF
      ENDIF
   ENDIF

   drawrectInBitmap( hDC, fromrow + 11, fromcol + 11, torow - 11, tocol - 11, WHITE )
   DrawWindowBoxInBitmap( hDC, fromrow + 10, fromcol + 10, torow - 11, tocol - 11 )

   IF l3d
      torow -= depth
   ENDIF

   fromcol += 25
   tocol -= 25
   torow -= 25
   fromrow += 25

   topleftrow := fromrow
   topleftcol := fromcol
   toprightrow := fromrow
   toprightcol := tocol
   bottomrightrow := torow
   bottomrightcol := tocol
   bottomleftrow := torow
   bottomleftcol := fromcol
   middletopcol := fromcol + Int( tocol - fromcol ) / 2
   middleleftrow := fromrow + Int( torow - fromrow ) / 2
   middleleftcol := fromcol
   middlebottomcol := fromcol + Int( tocol - fromcol ) / 2
   middlerightrow := fromrow + Int( torow - fromrow ) / 2
   middlerightcol := tocol

   torow := torow + 1
   tocol := tocol + 1

   AEval( series, {|i| ser_sum += i } )
   AEval( series, {|i| AAdd( degrees, Round( i / ser_sum * 360, 0 ) ) } )
   sum := 0
   AEval( degrees, {|i| sum += i } )
   IF sum <> 360
      degrees[len(degrees)] += 360 - sum
   ENDIF
   sum := 0
   AEval( degrees, {|i| sum += i, AAdd( cumulative,sum ) } )

   fromradialrow := middlerightrow
   fromradialcol := middlerightcol
   FOR i := 1 TO Len( cumulative )
      shadowcolor := { iif( colors[i,1] > 50, colors[i,1] - 50, 0 ), iif( colors[i,2] > 50, colors[i,2] - 50, 0 ), iif( colors[i,3] > 50, colors[i,3] - 50, 0 ) }
      IF cumulative[i] == previos_cumulative
         LOOP  // fixed by Roberto Lopez
      ENDIF
      previos_cumulative := cumulative[i]
      DO CASE
      CASE cumulative[i] <= 45
         toradialcol := middlerightcol
         toradialrow := middlerightrow - Round( cumulative[i] / 45 * ( middlerightrow - toprightrow ), 0 )
         drawpieInBitmap( hDC, fromrow, fromcol, torow, tocol, fromradialrow, fromradialcol, toradialrow, toradialcol, , , colors[i] )
         fromradialrow := toradialrow
         fromradialcol := toradialcol
      CASE cumulative[i] <= 90 .AND. cumulative[i] > 45
         toradialrow := toprightrow
         toradialcol := toprightcol - Round( ( cumulative[i] - 45 ) / 45 * ( toprightcol - middletopcol ), 0 )
         drawpieInBitmap( hDC, fromrow, fromcol, torow, tocol, fromradialrow, fromradialcol, toradialrow, toradialcol, , , colors[i] )
         fromradialrow := toradialrow
         fromradialcol := toradialcol
      CASE cumulative[i] <= 135 .AND. cumulative[i] > 90
         toradialrow := topleftrow
         toradialcol := middletopcol - Round( ( cumulative[i] - 90 ) / 45 * ( middletopcol - topleftcol ), 0 )
         drawpieInBitmap( hDC, fromrow, fromcol, torow, tocol, fromradialrow, fromradialcol, toradialrow, toradialcol, , , colors[i] )
         fromradialrow := toradialrow
         fromradialcol := toradialcol
      CASE cumulative[i] <= 180 .AND. cumulative[i] > 135
         toradialcol := topleftcol
         toradialrow := topleftrow + Round( ( cumulative[i] - 135 ) / 45 * ( middleleftrow - topleftrow ), 0 )
         drawpieInBitmap( hDC, fromrow, fromcol, torow, tocol, fromradialrow, fromradialcol, toradialrow, toradialcol, , , colors[i] )
         fromradialrow := toradialrow
         fromradialcol := toradialcol
      CASE cumulative[i] <= 225 .AND. cumulative[i] > 180
         toradialcol := topleftcol
         toradialrow := middleleftrow + Round( ( cumulative[i] - 180 ) / 45 * ( bottomleftrow - middleleftrow ), 0 )
         IF l3d
            FOR j := 1 TO depth
               drawarcInBitmap( hDC, fromrow + j, fromcol, torow + j, tocol, fromradialrow + j, fromradialcol, toradialrow + j, toradialcol, shadowcolor )
            NEXT j
         ENDIF
         drawpieInBitmap( hDC, fromrow, fromcol, torow, tocol, fromradialrow, fromradialcol, toradialrow, toradialcol, , , colors[i] )
         fromradialrow := toradialrow
         fromradialcol := toradialcol
      CASE cumulative[i] <= 270 .AND. cumulative[i] > 225
         toradialrow := bottomleftrow
         toradialcol := bottomleftcol + Round( ( cumulative[i] - 225 ) / 45 * ( middlebottomcol - bottomleftcol ), 0 )
         IF l3d
            FOR j := 1 TO depth
               drawarcInBitmap( hDC, fromrow + j, fromcol, torow + j, tocol, fromradialrow + j, fromradialcol, toradialrow + j, toradialcol, shadowcolor )
            NEXT j
         ENDIF
         drawpieInBitmap( hDC, fromrow, fromcol, torow, tocol, fromradialrow, fromradialcol, toradialrow, toradialcol, , , colors[i] )
         fromradialrow := toradialrow
         fromradialcol := toradialcol
      CASE cumulative[i] <= 315 .AND. cumulative[i] > 270
         toradialrow := bottomleftrow
         toradialcol := middlebottomcol + Round( ( cumulative[i] - 270 ) / 45 * ( bottomrightcol - middlebottomcol ), 0 )
         IF l3d
            FOR j := 1 TO depth
               drawarcInBitmap( hDC, fromrow + j, fromcol, torow + j, tocol, fromradialrow + j, fromradialcol, toradialrow + j, toradialcol, shadowcolor )
            NEXT j
         ENDIF
         drawpieInBitmap( hDC, fromrow, fromcol, torow, tocol, fromradialrow, fromradialcol, toradialrow, toradialcol, , , colors[i] )
         fromradialrow := toradialrow
         fromradialcol := toradialcol
      CASE cumulative[i] <= 360 .AND. cumulative[i] > 315
         toradialcol := bottomrightcol
         toradialrow := bottomrightrow - Round( ( cumulative[i] - 315 ) / 45 * ( bottomrightrow - middlerightrow ), 0 )
         IF l3d
            FOR j := 1 TO depth
               drawarcInBitmap( hDC, fromrow + j, fromcol, torow + j, tocol, fromradialrow + j, fromradialcol, toradialrow + j, toradialcol, shadowcolor )
            NEXT j
         ENDIF
         drawpieInBitmap( hDC, fromrow, fromcol, torow, tocol, fromradialrow, fromradialcol, toradialrow, toradialcol, , , colors[i] )
         fromradialrow := toradialrow
         fromradialcol := toradialcol
      ENDCASE
      IF l3d
         drawlineInBitmap( hDC, middleleftrow, middleleftcol, middleleftrow + depth, middleleftcol, BLACK )
         drawlineInBitmap( hDC, middlerightrow, middlerightcol, middlerightrow + depth, middlerightcol, BLACK )
         drawarcInBitmap( hDC, fromrow + depth, fromcol, torow + depth, tocol, middleleftrow + depth, middleleftcol, middlerightrow + depth, middlerightcol )
      ENDIF
   NEXT i
   IF lsleg
      IF toright
         fromrow := topleftrow
         fromcol := toprightcol + 25
      ELSE
         fromrow := torow + 20 + iif( l3d, depth, 0 )
      ENDIF
      FOR i := 1 TO Len( aname )
         drawrectInBitmap( hDC, fromrow + 1, fromcol + 1, fromrow + 14, fromcol + 14, colors[i] )
         DrawWindowBoxInBitmap( hDC, fromrow, fromcol, fromrow + 14, fromcol + 14 )
         drawtextinbitmap( hDC, fromrow, fromcol + 20, aname[ i ] + iif( lxval, " - " + LTrim( Transform( series[i], cPicture ) ) + " (" + LTrim( Str( series[i] / ser_sum * 100, 6, 2 ) ) + " %)", "" ), _HMG_DefaultFontName, _HMG_DefaultFontSize - 1, iif( RGB( colors[i][1], colors[i][2], colors[i][3] ) == RGB( 255, 255, 255 ), BLACK, colors[i] ) )
         fromrow += 20
      NEXT i
   ENDIF

   BT_DeleteDC( BTstruct )
 
RETURN hBitmap

*=============================================================================*
*                          Auxiliary Functions
*=============================================================================*

/*
 * PROCEDURE DrawWindowBoxInBitmap( hDC, row, col, rowr, colr, nPenWidth )
 *
 * Draws a rectangle (window box) on a bitmap using the specified parameters.
 *
 * Parameters:
 *   hDC       : The handle to the device context of the bitmap.
 *   row       : The top row coordinate of the rectangle.
 *   col       : The left column coordinate of the rectangle.
 *   rowr      : The bottom row coordinate of the rectangle.
 *   colr      : The right column coordinate of the rectangle.
 *   nPenWidth : The width of the pen used to draw the rectangle's border.
 *
 * Returns:
 *   None. This procedure draws directly onto the bitmap associated with the hDC.
 *
 * Purpose:
 *   This procedure provides a simple way to draw a rectangular outline on a bitmap.
 *   It's used to visually represent window boundaries or other rectangular elements
 *   within a graphical representation. It leverages the BT_DrawRectangle function
 *   from the HMG Extended Bitmap Tools library for the actual drawing operation.
 *
 * Notes:
 *   The color of the rectangle is hardcoded to BLACK.
 */
STATIC PROCEDURE DrawWindowBoxInBitmap( hDC, row, col, rowr, colr, nPenWidth )

   BT_DrawRectangle ( hDC, Row, Col, Colr - col, rowr - row, BLACK, nPenWidth )

RETURN

/*
 * PROCEDURE DrawRectInBitmap( hDC, row, col, row1, col1, aColor, nPenWidth )
 *
 * Draws a filled rectangle on a bitmap using the specified parameters.
 *
 * Parameters:
 *   hDC       : The handle to the device context of the bitmap.
 *   row       : The top row coordinate of the rectangle.
 *   col       : The left column coordinate of the rectangle.
 *   row1      : The bottom row coordinate of the rectangle.
 *   col1      : The right column coordinate of the rectangle.
 *   aColor    : An array representing the RGB color of the rectangle (e.g., {Red, Green, Blue}).
 *   nPenWidth : The width of the pen used to draw the rectangle's border.
 *
 * Returns:
 *   None. This procedure draws directly onto the bitmap associated with the hDC.
 *
 * Purpose:
 *   This procedure provides a way to draw a filled rectangle on a bitmap with a specified color.
 *   It's used to create solid color blocks or to fill areas within a graphical representation.
 *   It leverages the BT_DrawFillRectangle function from the HMG Extended Bitmap Tools library
 *   for the actual drawing operation.
 */
STATIC PROCEDURE DrawRectInBitmap( hDC, row, col, row1, col1, aColor, nPenWidth )

   BT_DrawFillRectangle ( hDC, Row, Col, col1 - col, row1 - row, aColor, aColor, nPenWidth )

RETURN

/*
 * PROCEDURE DrawLineInBitmap( hDC, Row1, Col1, Row2, Col2, aColor, nPenWidth )
 *
 * Draws a line on a bitmap using the specified parameters.
 *
 * Parameters:
 *   hDC       : The handle to the device context of the bitmap.
 *   Row1      : The row coordinate of the starting point of the line.
 *   Col1      : The column coordinate of the starting point of the line.
 *   Row2      : The row coordinate of the ending point of the line.
 *   Col2      : The column coordinate of the ending point of the line.
 *   aColor    : An array representing the RGB color of the line (e.g., {Red, Green, Blue}).
 *   nPenWidth : The width of the pen used to draw the line.
 *
 * Returns:
 *   None. This procedure draws directly onto the bitmap associated with the hDC.
 *
 * Purpose:
 *   This procedure provides a way to draw a line on a bitmap with a specified color and thickness.
 *   It's used to create lines, borders, or other linear elements within a graphical representation.
 *   It leverages the BT_DrawLine function from the HMG Extended Bitmap Tools library
 *   for the actual drawing operation.
 */
STATIC PROCEDURE DrawLineInBitmap( hDC, Row1, Col1, Row2, Col2, aColor, nPenWidth )

   BT_DrawLine ( hDC, Row1, Col1, Row2, Col2, aColor, nPenWidth )

RETURN

/*
 * PROCEDURE DrawTextInBitmap( hDC, Row, Col, cText, cFontName, nFontSize, aColor, nAlign )
 *
 * Draws text on a bitmap using the specified parameters.
 *
 * Parameters:
 *   hDC       : The handle to the device context of the bitmap.
 *   Row       : The row coordinate of the top-left corner of the text.
 *   Col       : The column coordinate of the top-left corner of the text.
 *   cText     : The text string to be drawn.
 *   cFontName : The name of the font to use (e.g., "Arial").
 *   nFontSize : The size of the font in points.
 *   aColor    : An array representing the RGB color of the text (e.g., {Red, Green, Blue}).
 *   nAlign    : (Optional) An integer representing the text alignment.
 *               0: Left alignment (default).
 *               1: Right alignment.
 *               2: Center alignment.
 *
 * Returns:
 *   None. This procedure draws directly onto the bitmap associated with the hDC.
 *
 * Purpose:
 *   This procedure provides a way to draw text on a bitmap with a specified font, size, color, and alignment.
 *   It's used to add labels, annotations, or other textual information to a graphical representation.
 *   It leverages the BT_DrawText function from the HMG Extended Bitmap Tools library
 *   for the actual drawing operation.
 *
 * Notes:
 *   The text background is set to transparent.
 */
STATIC PROCEDURE DrawTextInBitmap( hDC, Row, Col, cText, cFontName, nFontSize, aColor, nAlign )

   DEFAULT nAlign := 0
   SWITCH nAlign
   CASE 0
      BT_DrawText ( hDC, Row, Col, cText, cFontName, nFontSize, aColor, , BT_TEXT_TRANSPARENT )
      EXIT
   CASE 1
      BT_DrawText ( hDC, Row, Col, cText, cFontName, nFontSize, aColor, , , BT_TEXT_RIGHT + BT_TEXT_TOP )
      EXIT
   CASE 2
      BT_DrawText ( hDC, Row, Col, cText, cFontName, nFontSize, aColor, , BT_TEXT_TRANSPARENT, BT_TEXT_CENTER + BT_TEXT_TOP )
      EXIT
   ENDSWITCH

RETURN

/*
 * PROCEDURE DrawPointInBitmap( hDC, nGraphType, nY, nX, nHigh, aColor )
 *
 * Draws a point or a small circle on a bitmap, depending on the graph type.
 *
 * Parameters:
 *   hDC        : The handle to the device context of the bitmap.
 *   nGraphType : An integer representing the graph type.  It determines whether to draw a point or a circle.
 *                Assumes POINTS and LINES are constants defined elsewhere.
 *   nY         : The Y coordinate of the point.
 *   nX         : The X coordinate of the point.
 *   nHigh      : A size parameter used to adjust the size and position of the point/circle.
 *   aColor     : An array representing the RGB color of the point/circle (e.g., {Red, Green, Blue}).
 *
 * Returns:
 *   None. This procedure draws directly onto the bitmap associated with the hDC.
 *
 * Purpose:
 *   This procedure provides a way to draw data points on a bitmap, typically as part of a graph or chart.
 *   The nGraphType parameter allows for different visual representations of the points (e.g., small circles for lines, larger circles for individual points).
 *   It uses the DrawCircleInBitmap procedure to draw the circles.
 *
 * Notes:
 *   The exact meaning of nGraphType, POINTS, and LINES depends on the context of the larger application.
 *   The offsets and sizes used in the DrawCircleInBitmap calls are hardcoded and may need adjustment depending on the desired visual appearance.
 */
STATIC PROCEDURE DrawPointInBitmap( hDC, nGraphType, nY, nX, nHigh, aColor )

   IF nGraphType == POINTS
      DrawCircleInBitmap( hDC, nX - nHigh - 3, nY - 3, 8, aColor )
   ELSEIF nGraphType == LINES
      DrawCircleInBitmap( hDC, nX - nHigh - 2, nY - 2, 6, aColor )
   ENDIF

RETURN

/*
 * PROCEDURE DrawCircleInBitmap( hDC, nCol, nRow, nWidth, aColor, nPenWidth )
 *
 * Draws a filled circle (ellipse) on a bitmap using the specified parameters.
 *
 * Parameters:
 *   hDC       : The handle to the device context of the bitmap.
 *   nCol      : The column coordinate of the top-left corner of the bounding rectangle of the circle.
 *   nRow      : The row coordinate of the top-left corner of the bounding rectangle of the circle.
 *   nWidth    : The width (and height, since it's a circle) of the bounding rectangle of the circle.
 *   aColor    : An array representing the RGB color of the circle (e.g., {Red, Green, Blue}).
 *   nPenWidth : (Optional) The width of the pen used to draw the circle's border. If omitted, the default pen width is used.
 *
 * Returns:
 *   None. This procedure draws directly onto the bitmap associated with the hDC.
 *
 * Purpose:
 *   This procedure provides a way to draw a filled circle on a bitmap with a specified color.
 *   It's used to represent data points, markers, or other circular elements within a graphical representation.
 *   It leverages the BT_DrawFillEllipse function from the HMG Extended Bitmap Tools library
 *   for the actual drawing operation.
 */
STATIC PROCEDURE DrawCircleInBitmap( hDC, nCol, nRow, nWidth, aColor, nPenWidth )

   BT_DrawFillEllipse( hDC, nCol, nRow, nWidth, nWidth, aColor, aColor, nPenWidth )

RETURN

/*
 * PROCEDURE DrawBarInBitmap( hDC, nY, nX, nHigh, nWidth, l3DView, nDeep, aColor )
 *
 * Draws a bar (rectangle) on a bitmap, optionally with a 3D effect, using the specified parameters.
 *
 * Parameters:
 *   hDC     : The handle to the device context of the bitmap.
 *   nY      : The Y coordinate of the top of the bar.
 *   nX      : The X coordinate of the left side of the bar.
 *   nHigh   : The height of the bar.
 *   nWidth  : The width of the bar.
 *   l3DView : A logical value indicating whether to draw the bar with a 3D effect (.T.) or not (.F.).
 *   nDeep   : The depth of the 3D effect (if l3DView is .T.).
 *   aColor  : An array representing the RGB color of the bar (e.g., {Red, Green, Blue}).
 *
 * Returns:
 *   None. This procedure draws directly onto the bitmap associated with the hDC.
 *
 * Purpose:
 *   This procedure provides a way to draw bars on a bitmap, typically as part of a bar chart or histogram.
 *   The optional 3D effect adds visual depth to the bars.
 *   It uses BT_DrawGradientFillVertical for the main bar fill and DrawPolygonInBitmap and DrawBoxInBitmap for the 3D effect.
 *
 * Notes:
 *   The color shading for the 3D effect is hardcoded and may need adjustment depending on the desired visual appearance.
 *   The ClrShadow function is assumed to be defined elsewhere and used to calculate shadow colors.
 */
STATIC PROCEDURE DrawBarInBitmap( hDC, nY, nX, nHigh, nWidth, l3DView, nDeep, aColor )

   LOCAL nColTop, nShadow, nShadow2, nH := nHigh

   nColTop := ClrShadow( RGB( aColor[ 1 ], aColor[ 2 ], aColor[ 3 ] ), 20 )
   nShadow := ClrShadow( nColTop, 20 )
   nShadow2 := ClrShadow( nColTop, 40 )
   nColTop := { GetRed( nColTop ), GetGreen( nColTop ), GetBlue( nColTop ) }
   nShadow := { GetRed( nShadow ), GetGreen( nShadow ), GetBlue( nShadow ) }
   nShadow2 := { GetRed( nShadow2 ), GetGreen( nShadow2 ), GetBlue( nShadow2 ) }
   BT_DrawGradientFillVertical( hDC, nX + nDeep - nHigh, nY, nWidth + 1, nHigh - nDeep, aColor, nShadow2 )
   IF l3DView
      // Lateral
      DrawPolygonInBitmap( hDC, { { nX -1, nY + nWidth + 1 }, { nX + nDeep - nHigh, nY + nWidth + 1 }, ;
         { nX - nHigh + 1, nY + nWidth + nDeep }, { nX - nDeep, nY + nWidth + nDeep }, ;
         { nX -1, nY + nWidth + 1 } }, nShadow,, nShadow )
      // Superior
      nHigh   := Max( nHigh, nDeep )
      DrawPolygonInBitmap( hDC, { { nX - nHigh + nDeep, nY + 1 }, { nX - nHigh + nDeep, nY + nWidth + 1 }, ;
         { nX - nHigh + 1, nY + nWidth + nDeep }, { nX - nHigh + 1, nY + nDeep }, ;
         { nX - nHigh + nDeep, nY + 1 } }, nColTop,, nColTop )
      // Border
      DrawBoxInBitmap( hDC, nY, nX, nH, nWidth, l3DView, nDeep )
   ENDIF

RETURN

/*
 * PROCEDURE DrawArcInBitmap( hDC, row, col, row1, col1, rowr, colr, rowr1, colr1, penrgb, penwidth )
 *
 * Draws an arc on a bitmap using the specified parameters.
 *
 * Parameters:
 *   hDC       : The handle to the device context of the bitmap.
 *   row       : The row coordinate of the top-left corner of the bounding rectangle.
 *   col       : The column coordinate of the top-left corner of the bounding rectangle.
 *   row1      : The row coordinate of the bottom-right corner of the bounding rectangle.
 *   col1      : The column coordinate of the bottom-right corner of the bounding rectangle.
 *   rowr      : The row coordinate of the start angle of the arc.
 *   colr      : The column coordinate of the start angle of the arc.
 *   rowr1     : The row coordinate of the end angle of the arc.
 *   colr1     : The column coordinate of the end angle of the arc.
 *   penrgb    : (Optional) An array representing the RGB color of the arc's pen (e.g., {Red, Green, Blue}). Defaults to BLACK if NIL.
 *   penwidth  : (Optional) The width of the pen used to draw the arc. Defaults to 1 if NIL.
 *
 * Returns:
 *   None. This procedure draws directly onto the bitmap associated with the hDC.
 *
 * Purpose:
 *   This procedure provides a way to draw an arc on a bitmap with a specified color and thickness.
 *   It leverages the BT_DrawArc function from the HMG Extended Bitmap Tools library
 *   for the actual drawing operation.
 */
STATIC PROCEDURE DrawArcInBitmap( hDC, row, col, row1, col1, rowr, colr, rowr1, colr1, penrgb, penwidth )

   IF penrgb == NIL
      penrgb := BLACK
   ENDIF
   IF penwidth == NIL
      penwidth := 1
   ENDIF

   BT_DrawArc ( hDC, row, col, row1, col1, rowr, colr, rowr1, colr1, penrgb, penwidth )

RETURN

/*
 * PROCEDURE DrawPieInBitmap( hDC, row, col, row1, col1, rowr, colr, rowr1, colr1, penrgb, penwidth, fillrgb )
 *
 * Draws a pie slice on a bitmap using the specified parameters.
 *
 * Parameters:
 *   hDC       : The handle to the device context of the bitmap.
 *   row       : The row coordinate of the top-left corner of the bounding rectangle.
 *   col       : The column coordinate of the top-left corner of the bounding rectangle.
 *   row1      : The row coordinate of the bottom-right corner of the bounding rectangle.
 *   col1      : The column coordinate of the bottom-right corner of the bounding rectangle.
 *   rowr      : The row coordinate of the start angle of the pie slice.
 *   colr      : The column coordinate of the start angle of the pie slice.
 *   rowr1     : The row coordinate of the end angle of the pie slice.
 *   colr1     : The column coordinate of the end angle of the pie slice.
 *   penrgb    : (Optional) An array representing the RGB color of the pie slice's pen (e.g., {Red, Green, Blue}). Defaults to BLACK if NIL.
 *   penwidth  : (Optional) The width of the pen used to draw the pie slice's border. Defaults to 1 if NIL.
 *   fillrgb   : (Optional) An array representing the RGB color used to fill the pie slice (e.g., {Red, Green, Blue}). Defaults to WHITE if NIL.
 *
 * Returns:
 *   None. This procedure draws directly onto the bitmap associated with the hDC.
 *
 * Purpose:
 *   This procedure provides a way to draw a pie slice on a bitmap with a specified color and thickness.
 *   It leverages the BT_DrawPie function from the HMG Extended Bitmap Tools library
 *   for the actual drawing operation.
 */
STATIC PROCEDURE DrawPieInBitmap( hDC, row, col, row1, col1, rowr, colr, rowr1, colr1, penrgb, penwidth, fillrgb )

   IF penrgb == NIL
      penrgb := BLACK
   ENDIF
   IF penwidth == NIL
      penwidth := 1
   ENDIF
   IF fillrgb == NIL
      fillrgb := WHITE
   ENDIF

   BT_DrawPie ( hDC, row, col, row1, col1, rowr, colr, rowr1, colr1, penrgb, penwidth, fillrgb )

RETURN

/*
 * PROCEDURE DrawPolygonInBitmap( hDC, apoints, penrgb, penwidth, fillrgb )
 *
 * Draws a polygon on a bitmap using the specified parameters.
 *
 * Parameters:
 *   hDC       : The handle to the device context of the bitmap.
 *   apoints   : An array of arrays, where each inner array represents a point (row, column) of the polygon.
 *               For example: { {row1, col1}, {row2, col2}, {row3, col3} }.
 *   penrgb    : (Optional) An array representing the RGB color of the polygon's pen (e.g., {Red, Green, Blue}). Defaults to BLACK if NIL.
 *   penwidth  : (Optional) The width of the pen used to draw the polygon's border. Defaults to 1 if NIL.
 *   fillrgb   : (Optional) An array representing the RGB color used to fill the polygon (e.g., {Red, Green, Blue}). Defaults to WHITE if NIL.
 *
 * Returns:
 *   None. This procedure draws directly onto the bitmap associated with the hDC.
 *
 * Purpose:
 *   This procedure provides a way to draw a polygon on a bitmap with a specified color and thickness.
 *   It leverages the BT_DrawPolygon function from the HMG Extended Bitmap Tools library
 *   for the actual drawing operation.
 */
STATIC PROCEDURE DrawPolygonInBitmap( hDC, apoints, penrgb, penwidth, fillrgb )

   LOCAL xarr := {}
   LOCAL yarr := {}
   LOCAL x

   IF penrgb == NIL
      penrgb := BLACK
   ENDIF
   IF penwidth == NIL
      penwidth := 1
   ENDIF
   IF fillrgb == NIL
      fillrgb := WHITE
   ENDIF
   FOR x := 1 TO LEN( apoints )
      AAdd( xarr, apoints[ x, 2 ] )
      AAdd( yarr, apoints[ x, 1 ] )
   NEXT x

   BT_DrawPolygon ( hDC, yarr, xarr, penrgb, penwidth, fillrgb )

RETURN

/*
 * PROCEDURE DrawBoxInBitmap( hDC, nY, nX, nHigh, nWidth, l3DView, nDeep )
 *
 * Draws a box (rectangle) on a bitmap, optionally with a 3D effect, using the specified parameters.
 *
 * Parameters:
 *   hDC     : The handle to the device context of the bitmap.
 *   nY      : The Y coordinate of the top of the box.
 *   nX      : The X coordinate of the left side of the box.
 *   nHigh   : The height of the box (used for 3D effect).
 *   nWidth  : The width of the box.
 *   l3DView : A logical value indicating whether to draw the box with a 3D effect (.T.) or not (.F.).
 *   nDeep   : The depth of the 3D effect (if l3DView is .T.).
 *
 * Returns:
 *   None. This procedure draws directly onto the bitmap associated with the hDC.
 *
 * Purpose:
 *   This procedure provides a way to draw a box on a bitmap, optionally with a 3D effect.
 *   It's used to create visual containers or to highlight areas within a graphical representation.
 *   It uses DrawLineInBitmap to draw the lines that make up the box and its 3D effect.
 *
 * Notes:
 *   The color of the box and its shadow are hardcoded to BLACK.
 *   The 3D effect is achieved by drawing additional lines to simulate depth.
 */
STATIC PROCEDURE DrawBoxInBitmap( hDC, nY, nX, nHigh, nWidth, l3DView, nDeep )

   // Set Border
   DrawLineInBitmap( hDC, nX, nY, nX - nHigh + nDeep, nY, BLACK )  // LEFT
   DrawLineInBitmap( hDC, nX, nY + nWidth, nX - nHigh + nDeep, nY + nWidth, BLACK )  // RIGHT
   DrawLineInBitmap( hDC, nX - nHigh + nDeep, nY, nX - nHigh + nDeep, nY + nWidth, BLACK )  // Top
   DrawLineInBitmap( hDC, nX, nY, nX, nY + nWidth, BLACK )                          // Bottom
   IF l3DView
      // Set shadow
      DrawLineInBitmap( hDC, nX - nHigh + nDeep, nY + nWidth, nX - nHigh, nY + nDeep + nWidth, BLACK )
      DrawLineInBitmap( hDC, nX, nY + nWidth, nX - nDeep, nY + nWidth + nDeep, BLACK )
      IF nHigh > 0
         DrawLineInBitmap( hDC, nX - nDeep, nY + nWidth + nDeep, nX - nHigh, nY + nWidth + nDeep, BLACK )
         DrawLineInBitmap( hDC, nX - nHigh, nY + nDeep, nX - nHigh, nY + nWidth + nDeep, BLACK )
         DrawLineInBitmap( hDC, nX - nHigh + nDeep, nY, nX - nHigh, nY + nDeep, BLACK )
      ELSE
         DrawLineInBitmap( hDC, nX - nDeep, nY + nWidth + nDeep, nX - nHigh + 1, nY + nWidth + nDeep, BLACK )
         DrawLineInBitmap( hDC, nX, nY, nX - nDeep, nY + nDeep, BLACK )
      ENDIF
   ENDIF

RETURN

#endif