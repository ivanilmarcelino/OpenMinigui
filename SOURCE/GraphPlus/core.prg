/*
 * HMG GraphPlus Library 2021
 * Author: S. Rathinagiri <srathinagiri@gmail.com>
 *
 * Following is the copyright notice for Treemap algorithm used in this Library
 * Copyright (C) 2001 by University of Maryland, College Park, MD 20742, USA
 * and Martin Wattenberg, <w@bewitched.com>
 * All rights reserved.
 * Authors: Benjamin B. Bederson and Martin Wattenberg
 * http://www.cs.umd.edu/hcil/treemaps
 */


#ifdef __XHARBOUR__
#define __SYSDATA__
#define __MINIPRINT__
#endif

#include <hmg.ch>
#include <GraphPlus.ch>
#include "hbclass.ch"

CLASS GraphPlus

   DATA aData INIT {} // Graph data
   DATA aLegends INIT {}
   DATA aCategories INIT {} // Horizontal Axis
   DATA nGraphType INIT GT_DEFAULT // COLUMN
   DATA cTitle INIT '' // Graph Title
   DATA cXTitle INIT '' // Horizontal Title
   DATA cYTitle INIT '' // Vertical Title
   DATA lXRotate INIT .F. // xValues Rotate
   DATA nHeight INIT 600 // Graph Height
   DATA nWidth INIT 600 // Graph Width
   DATA lShowLegends INIT .F. // Show legends
   DATA lDataLabels INIT .F. // show data labels
   DATA lHGrid INIT .T. // Horizontal Grid
   DATA lVGrid INIT .T. // Vertical Grid
   DATA lShowValues INIT .T. // Show Value Axis
   DATA lShowCategories INIT .T. // show categories
   DATA nLegendPos INIT LEGEND_ON_BOTTOM // 0 Right 1 Bottom
   DATA lOutBorder INIT .T. // draw outside border
   DATA aColors INIT {} // aColors
   DATA aTitleFont INIT { 'Arial', 12, .T., .F., .F., .F. }
   DATA aLegendFont INIT { 'Arial', 8, .F., .F., .F., .F. }
   DATA aBackColor INIT CLR_WHITE
   DATA aTitleColor INIT { 0, 0, 255 }
   DATA aTextColor INIT { 0, 0, 0 }
   DATA aGridLineColor INIT { 200, 200, 200 }
   DATA nLegendBoxSize INIT 10
   DATA nLegendWidth INIT 0
   DATA nColorTheme INIT 1
   DATA nUpper INIT 0
   DATA nLower INIT 0
   DATA lCategoryBelow INIT .T.
   DATA nRange INIT 0
   DATA nPixelperPoint INIT 0
   DATA nLineWidth INIT 2
   DATA nZeroPos INIT 0
   DATA nVDivision INIT 5
   DATA nHDivision INIT 5
   DATA nUpperDivision INIT 0
   DATA nLowerDivision INIT 0
   DATA cPicture INIT ''
   DATA nCategoryRow INIT 0
   DATA nScaleItemRow INIT 0
   DATA nPieGap INIT 3
   DATA nBarGapRatio INIT 0.2 // in percentage 20%
   DATA nGapWidthRatio INIT 1 // in percentage 200%
   DATA nImageHeight INIT 600
   DATA nImageWidth INIT 600
   DATA nPointSize INIT 10
   DATA lGraphRotated INIT .F.
   DATA nMaxScaleWidth INIT 0
   DATA cPictureX INIT ''
   DATA cPictureY INIT ''
   DATA lScatterLine INIT .F.
   DATA aSunBurstData INIT {}
   DATA nLevels INIT 0
   DATA aLevelHeaders INIT {}
   DATA aLevelValues INIT {}
   DATA aTotalItems INIT {}
   DATA nLabelHeight INIT 0
   DATA aTreeMapRect INIT {}
   DATA aTreeMapData INIT {}

   DATA hBitMap INIT NIL // Bitmap to store Graph
   DATA hDC INIT NIL
   DATA BTStruct INIT NIL
   DATA nTop INIT 10
   DATA nLeft INIT 10
   DATA nRight INIT 590
   DATA nBottom INIT 590
   DATA aScale INIT {}
   DATA aColorThemes INIT { { CLR_HAVELOCK_BLUE, CLR_WESTSIDE, CLR_LIGHT_GREY, CLR_ORANGE_YELLOW, CLR_PICTON_BLUE, CLR_APPLE }, ;
      { CLR_HAVELOCK_BLUE, CLR_LIGHT_GREY, CLR_PICTON_BLUE, CLR_DARKER_BLUE, CLR_DARK_GREY, CLR_ENDEAVOUR }, ;
      { CLR_WESTSIDE, CLR_ORANGE_YELLOW, CLR_APPLE, CLR_SADDLE_BROWN, CLR_LIGHT_BROWN, CLR_DELL }, ;
      { CLR_APPLE, CLR_PICTON_BLUE, CLR_ORANGE_YELLOW, CLR_DELL, CLR_ENDEAVOUR, CLR_LIGHT_BROWN }, ;
      { CLR_HAVELOCK_BLUE }, ;
      { CLR_WESTSIDE }, ;
      }
   DATA lLighter INIT .T.

   METHOD New
   METHOD Draw
   METHOD Bitmap
   METHOD DrawColumnGraph
   METHOD DrawBarGraph
   METHOD DrawLineGraph
   METHOD DrawPieGraph
   METHOD DrawPointsGraph
   METHOD DrawFunnelGraph
   METHOD DrawStackedColumnGraph
   METHOD DrawStackedBarGraph
   METHOD DrawAreaGraph
   METHOD DrawScatterXYGraph
   METHOD DrawDoughnutGraph
   METHOD DrawSunBurstGraph
   METHOD DrawSunBurstLegends
   METHOD DrawWaterfallGraph
   METHOD DrawTreeMapGraph

   METHOD DrawLegends
   METHOD InitColors
   METHOD DrawXYTitles
   METHOD DrawCategories
   METHOD DrawVerticalScale
   METHOD DrawBarScale
   METHOD DrawHorizontalGrid
   METHOD DrawVerticalGrid
   METHOD SwitchRowsToColumns
   METHOD FindPieCoordinates
   METHOD FillSunBurstData
   METHOD FillTreeMapData

   METHOD StripTreemap
   METHOD LayoutStrip
   METHOD ComputeHorizontalBoxLayout
   METHOD ComputeSize
   METHOD ComputeAverageAspectRatio
   METHOD ComputeAspectRatio
   METHOD Save
   METHOD GraphData SETGET
   METHOD GridData SETGET
   METHOD Legends SETGET
   METHOD Picture SETGET
   METHOD PictureX SETGET
   METHOD PictureY SETGET
   METHOD Categories SETGET
   METHOD GraphColors SETGET
   METHOD GraphType SETGET
   METHOD Title SETGET
   METHOD XTitle SETGET
   METHOD YTitle SETGET
   METHOD Height SETGET
   METHOD Width SETGET
   METHOD ShowLegends SETGET
   METHOD ShowLabels SETGET
   METHOD ShowCategories SETGET
   METHOD ShowHGrid SETGET
   METHOD ShowVGrid SETGET
   METHOD ShowValues SETGET
   METHOD LegendPos SETGET
   METHOD ColorTheme SETGET
   METHOD BackColor SETGET
   METHOD BarGapRatio SETGET
   METHOD BarGapWidthRatio SETGET
   METHOD TitleFont SETGET
   METHOD LegendFont SETGET
   METHOD ScatterLine SETGET
   METHOD VDivisions SETGET
   METHOD HDivisions SETGET
   METHOD TotalItems SETGET

END CLASS

METHOD New CLASS GraphPlus
RETURN self

METHOD GraphData( aData ) CLASS GraphPlus
   IF PCount() == 0
      RETURN ::aData
   ELSE
      IF ValType( aData[ 1 ] ) == 'A' // 2D array
         ::aData := aData
      ELSE
         ::aData := { aData } // 1D - make it 2D
      ENDIF
   ENDIF

RETURN Self

METHOD GridData( cFormName, cControlName ) CLASS GraphPlus

   LOCAL i
   LOCAL j
   LOCAL nRowCount := GetProperty( cFormName, cControlName, 'ITEMCOUNT' )
   LOCAL nColumnCount
   LOCAL aLineData
   LOCAL aLine := {}
   IF PCount() == 0
      RETURN ::aData
   ELSE
      ASize( ::aData, 0 )
      ASize( ::aLegends, 0 )
      ASize( ::aCategories, 0 )
      nColumnCount := GetProperty( cFormName, cControlName, 'COLUMNCOUNT' )
      FOR i := 1 TO nColumnCount
         IF i > 1
            AAdd( ::aCategories, GetProperty( cFormName, cControlName, 'COLUMNHEADER', i ) )
         ENDIF
      NEXT i
      IF nRowCount > 0
         FOR i := 1 TO nRowCount
            aLineData := GetProperty( cFormName, cControlName, 'ITEM', i )
            ASize( aLine, 0 )
            FOR j := 1 TO nColumnCount
               IF j == 1
                  AAdd( ::aLegends, aLineData[ j ] )
               ELSE
                  AAdd( aLine, Val( AllTrim( cValToChar( aLineData[ j ] ) ) ) )
               ENDIF
            NEXT j
            AAdd( ::aData, AClone( aLine ) )
         NEXT i
      ENDIF
   ENDIF

RETURN self

METHOD Bitmap( nWidth, nHeight ) CLASS GraphPlus
   IF PCount() == 0
      RETURN ::hBitMap
   ELSE
      RETURN BT_BitmapCopyAndResize( ::hBitmap, nWidth, nHeight )
   ENDIF

RETURN ::hBitMap

METHOD GraphColors( aColors ) CLASS GraphPlus
   IF PCount() == 0
      RETURN ::aColors
   ELSE
      ::aColors := aColors
   ENDIF

RETURN Self

METHOD Legends( aLegends ) CLASS GraphPlus
   IF PCount() == 0
      RETURN ::aLegends
   ELSE
      ::aLegends := aLegends
   ENDIF

RETURN Self

METHOD Categories( aCategories ) CLASS GraphPlus
   IF PCount() == 0
      RETURN ::aCategories
   ELSE
      ::aCategories := aCategories
   ENDIF

RETURN Self

METHOD BackColor( aBackColor ) CLASS GraphPlus
   IF PCount() == 0
      RETURN ::aBackColor
   ELSE
      ::aBackColor := aBackColor
   ENDIF

RETURN Self

METHOD GraphType( nGraphType ) CLASS GraphPlus
   IF PCount() == 0
      RETURN ::nGraphType
   ELSE
      ::nGraphType := nGraphType
      IF ::nGraphType == 5 .OR. ::nGraphType == 6 .OR. ::nGraphType == 8 // bar or funnel or stacked bar
         ::lGraphRotated := .T.
      ELSE
         ::lGraphRotated := .F.
      ENDIF
   ENDIF

RETURN Self

METHOD Title( cTitle ) CLASS GraphPlus
   IF PCount() == 0
      RETURN ::cTitle
   ELSE
      ::cTitle := cTitle
   ENDIF

RETURN Self

METHOD XTitle( cXTitle ) CLASS GraphPlus
   IF PCount() == 0
      RETURN ::cXTitle
   ELSE
      ::cXTitle := cXTitle
   ENDIF

RETURN Self

METHOD YTitle( cYTitle ) CLASS GraphPlus
   IF PCount() == 0
      RETURN ::cYTitle
   ELSE
      ::cYTitle := cYTitle
   ENDIF

RETURN Self

METHOD BarGapRatio( nBarGapRatio ) CLASS GraphPlus
   IF PCount() == 0
      RETURN ::nBarGapRatio
   ELSE
      ::nBarGapRatio := nBarGapRatio
   ENDIF

RETURN Self

METHOD BarGapWidthRatio( nBarGapWidthRatio ) CLASS GraphPlus
   IF PCount() == 0
      RETURN ::nGapWidthRatio
   ELSE
      ::nGapWidthRatio := nBarGapWidthRatio
   ENDIF

RETURN Self

METHOD TitleFont( aTitleFont ) CLASS GraphPlus
   IF PCount() == 0
      RETURN ::aTitleFont
   ELSE
      ::aTitleFont := aTitleFont
   ENDIF

RETURN Self

METHOD LegendFont( aLegendFont ) CLASS GraphPlus
   IF PCount() == 0
      RETURN ::aLegendFont
   ELSE
      ::aLegendFont := aLegendFont
   ENDIF

RETURN Self

METHOD Height( nHeight ) CLASS GraphPlus
   IF PCount() == 0
      RETURN ::nImageHeight
   ELSE
      ::nImageHeight := nHeight
   ENDIF

RETURN Self

METHOD Width( nWidth ) CLASS GraphPlus
   IF PCount() == 0
      RETURN ::nImageWidth
   ELSE
      ::nImageWidth := nWidth
   ENDIF

RETURN Self

METHOD ShowLegends( lShowLegends ) CLASS GraphPlus
   IF PCount() == 0
      RETURN ::lShowLegends
   ELSE
      ::lShowLegends := lShowLegends
   ENDIF

RETURN Self

METHOD ShowLabels( lDataLabels ) CLASS GraphPlus
   IF PCount() == 0
      RETURN ::lDataLabels
   ELSE
      ::lDataLabels := lDataLabels
   ENDIF

RETURN Self

METHOD ShowCategories( lShowCategories ) CLASS GraphPlus
   IF PCount() == 0
      RETURN ::lShowCategories
   ELSE
      ::lShowCategories := lShowCategories
   ENDIF

RETURN Self


METHOD ShowValues( lShowValues ) CLASS GraphPlus
   IF PCount() == 0
      RETURN ::lShowValues
   ELSE
      ::lShowValues := lShowValues
   ENDIF

RETURN Self

METHOD ShowHGrid( lHGrid ) CLASS GraphPlus
   IF PCount() == 0
      RETURN ::lHGrid
   ELSE
      ::lHGrid := lHGrid
   ENDIF

RETURN Self

METHOD ShowVGrid( lVGrid ) CLASS GraphPlus
   IF PCount() == 0
      RETURN ::lVGrid
   ELSE
      ::lVGrid := lVGrid
   ENDIF

RETURN Self

METHOD Picture( cPicture ) CLASS GraphPlus
   IF PCount() == 0
      RETURN ::cPicture
   ELSE
      ::cPicture := cPicture
   ENDIF

RETURN Self

METHOD PictureX( cPicture ) CLASS GraphPlus
   IF PCount() == 0
      RETURN ::cPictureX
   ELSE
      ::cPictureX := cPicture
   ENDIF

RETURN Self

METHOD PictureY( cPicture ) CLASS GraphPlus
   IF PCount() == 0
      RETURN ::cPictureY
   ELSE
      ::cPictureY := cPicture
   ENDIF

RETURN Self

METHOD ScatterLine( lScatterLine ) CLASS GraphPlus
   IF PCount() == 0
      RETURN ::lScatterLine
   ELSE
      ::lScatterLine := lScatterLine
   ENDIF

RETURN Self

METHOD VDivisions( nVDivision ) CLASS GraphPlus
   IF PCount() == 0
      RETURN ::nVDivision
   ELSE
      ::nVDivision := nVDivision
   ENDIF

RETURN Self

METHOD HDivisions( nHDivision ) CLASS GraphPlus
   IF PCount() == 0
      RETURN ::nHDivision
   ELSE
      ::nHDivision := nHDivision
   ENDIF

RETURN Self

METHOD TotalItems( aTotalItems ) CLASS GraphPlus
   IF PCount() == 0
      RETURN ::aTotalItems
   ELSE
      ::aTotalItems := aTotalItems
   ENDIF

RETURN Self

METHOD LegendPos( nLegendPos ) CLASS GraphPlus
   IF PCount() == 0
      RETURN ::nLegendPos
   ELSE
      ::nLegendPos := nLegendPos
   ENDIF

RETURN Self

METHOD ColorTheme( nColorTheme ) CLASS GraphPlus
   IF PCount() == 0
      RETURN ::nColorTheme
   ELSE
      ::nColorTheme := nColorTheme
      ASize( ::aColors, 0 ) // reset all colors if any
   ENDIF

RETURN Self


METHOD Draw CLASS GraphPlus

   LOCAL aSize, nLowerUpperRatio
   LOCAL nDistance
   LOCAL nZero
   LOCAL nMaxWidth
   LOCAL i
   LOCAL nMax, nMin, nI, nJ
   LOCAL nDivision, cDivision, nDecimalLength, nDecimal, cDecimal
   LOCAL nPositiveSum, nNegativeSum
   LOCAL aTemp, aBalances, nBalance
   IF hmg_len( ::aData ) == 0
      // nothing to draw!
      msgstop( 'No Graph data!' )
      RETURN NIL
   ENDIF
   ::nTop := 10
   ::nLeft := 10
   ::nHeight := ::nImageHeight
   ::nWidth := ::nImageWidth
   ::nBottom := ::nHeight - 10
   ::nRight := ::nWidth - 10
   // init bitmap
   ::hBitmap := BT_BitmapCreateNew ( ::nWidth, ::nHeight, ::aBackColor )
   ::hDC := BT_CreateDC( ::hBitmap, BT_HDC_BITMAP, @::BTStruct )
   ::aTextColor := GetNegativeColor ( ::aBackColor )
   IF hmg_len( ::aLegends ) == 0
      IF ::nGraphType == GT_WATERFALL // only three legends for waterfall
         ::aLegends := { 'Increase', 'Decrease', 'Total' }
      ELSE
         FOR i := 1 TO hmg_len( ::aData )
            AAdd( ::aLegends, "Legend " + AllTrim( Str( i ) ) )
         NEXT i
      ENDIF
   ENDIF
   IF hmg_len( ::aCategories ) == 0
      // categories not defined
      FOR i := 1 TO hmg_len( ::aData )
         AAdd( ::aCategories, 'Category ' + AllTrim( Str( i ) ) )
      NEXT i
   ENDIF
   // find datalabel height
   aSize := BT_DrawTextSize ( ::hDC, '0', ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ) )
   ::nLabelHeight := aSize[ 2 ]
   // draw outside border
   IF ::lOutBorder
      BT_DrawRectangle ( ::hDC, 0, 0, ::nWidth - 1, ::nHeight - 1, ::aTextColor, 1 )
   ENDIF
   ::InitColors() // init colors
   IF ::nGraphType == GT_FUNNEL // Funnel Chart
      ::DrawFunnelGraph()
      BT_DeleteDC( ::BTstruct )
      RETURN NIL
   ENDIF
   IF ::nGraphType == GT_SCATTERXY
      ::DrawScatterXYGraph()
      BT_DeleteDC( ::BTstruct )
      RETURN NIL
   ENDIF

   // draw legends
   IF ::lShowLegends .AND. hmg_len( ::aLegends ) > 0
      IF .NOT. ( ::nGraphType == GT_SUNBURST .OR. ::nGraphType == GT_TREEMAP )// don't draw now
         ::DrawLegends()
      ENDIF
   ENDIF
   // draw title
   IF hmg_len( ::cTitle ) > 0
      aSize := BT_DrawTextSize ( ::hDC, ::cTitle, ::aTitleFont[ 1 ], ::aTitleFont[ 2 ], if( ::aTitleFont[ 3 ], BT_TEXT_BOLD, nil ) )
      BT_DrawText ( ::hDC, ::nTop / 2, ( ::nWidth - if( ::lShowLegends .AND. ::nLegendPos < 1, ::nLegendWidth, 0 ) ) / 2, ::cTitle, ::aTitleFont[ 1 ], ::aTitleFont[ 2 ], ::aTitleColor, ::aBackColor, if( ::aTitleFont[ 3 ], BT_TEXT_BOLD, nil ), BT_TEXT_CENTER )
      ::nTop := ::nTop + aSize[ 2 ]
   ENDIF
   IF .NOT. ( ::nGraphType == GT_PIE .OR. ::nGraphType == GT_DOUGHNUT .OR. ::nGraphType == GT_SUNBURST .OR. ::nGraphType == GT_TREEMAP )
      ::DrawXYTitles() // draw x/y titles
      // draw graph border
      // BT_DrawRectangle (::hDC, ::nTop, ::nLeft, ::nRight - ::nLeft , ::nBottom - ::nTop, ::aTextColor, 1 )

      // find max and min value
      IF .NOT. ( ::nGraphType == GT_STACKEDCOLUMN .OR. ::nGraphType == GT_STACKEDBAR )
         IF ::nGraphType <> GT_WATERFALL
            nMax := 0
            nMin := 0
            FOR nJ := 1 TO HMG_LEN( ::aData )
               FOR nI := 1 TO HMG_LEN( ::aData[ nJ ] )
                  nMax := Max( ::aData[ nJ ][ nI ], nMax )
                  nMin := Min( ::aData[ nJ ][ nI ], nMin )
               NEXT nI
            NEXT nJ
         ELSE
            aTemp := {}
            aBalances := {}
            FOR i := 1 TO hmg_len( ::aData[ 1 ] )
               AAdd( aTemp, .F. )
            NEXT i
            FOR i := 1 TO hmg_len( ::aTotalItems )
               aTemp[ ::aTotalItems[ i ] ] := .T.
            NEXT i
            // find balances
            AAdd( aBalances, ::aData[ 1, 1 ] ) // first item
            nBalance := ::aData[ 1, 1 ]
            FOR i := 2 TO hmg_len( ::aData[ 1 ] )
               IF .NOT. aTemp[ i ] // totalitem
                  nBalance := nBalance + ::aData[ 1, i ] // increase or decrease
               ENDIF
               AAdd( aBalances, nBalance )
            NEXT i
            AAdd( ::aData, AClone( aTemp ) )
            AAdd( ::aData, AClone( aBalances ) )
            nMax := 0
            nMin := 0
            FOR i := 1 TO hmg_len( aBalances )
               nMax := Max( nMax, aBalances[ i ] )
               nMin := Min( nMin, aBalances[ i ] )
            NEXT i
         ENDIF
      ELSE
         nMax := 0
         nMin := 0
         FOR nJ := 1 TO hmg_len( ::aData[ 1 ] )
            nPositiveSum := 0
            nNegativeSum := 0
            FOR nI := 1 TO hmg_len( ::aData )
               IF ::aData[ nI, nJ ] > 0
                  nPositiveSum += ::aData[ nI, nJ ]
               ELSE
                  nNegativeSum += ::aData[ nI, nJ ]
               ENDIF
            NEXT nI
            nMax := Max( nMax, nPositiveSum )
            nMin := Min( nMin, nNegativeSum )
         NEXT nJ
      ENDIF

      IF nMax > 0
         ::nUpper := nMax
      ELSE
         ::nUpper := 0
      ENDIF
      IF nMin < 0
         ::nLower := nMin
      ELSE
         ::nLower := 0
      ENDIF
      ::nRange := ::nUpper - ::nLower
      IF ::nUpper <> 0 .AND. ::nLower <> 0
         IF Abs( ::nLower ) < ::nUpper
            nLowerUpperRatio := Abs( ::nLower ) / ::nRange
            ::nLowerDivision := ::nVDivision * nLowerUpperRatio
            IF ::nLowerDivision - Int( ::nLowerDivision ) > 0.0
               ::nLowerDivision := Int( ::nLowerDivision ) + 1
            ENDIF
            ::nUpperDivision := ::nVDivision - ::nLowerDivision
            nDivision := ::nUpper / ::nUpperDivision
         ELSE
            nLowerUpperRatio := Abs( ::nUpper ) / ::nRange
            ::nUpperDivision := ::nVDivision * nLowerUpperRatio
            IF ::nUpperDivision - Int( ::nUpperDivision ) > 0.0
               ::nUpperDivision := Int( ::nUpperDivision ) + 1
            ENDIF
            ::nLowerDivision := ::nVDivision - ::nUpperDivision
            nDivision := Abs( ::nLower ) / ::nLowerDivision
         ENDIF
      ELSE
         nDivision := ::nRange / ::nVDivision
         IF ::nUpper > 0
            ::nUpperDivision := ::nVDivision
            ::nLowerDivision := 0
         ELSE
            ::nLowerDivision := ::nVDivision
            ::nUpperDivision := 0
         ENDIF
      ENDIF
      IF Int( nDivision ) > 0
         cDivision := AllTrim( Str( Int( nDivision ), 14, 0 ) )
         IF hmg_len( cDivision ) > 1 // if more than 1 digits round to power of 10s
            nDivision := ( Val( Left( cDivision, 2 ) ) + 1 ) * 10 ^ ( hmg_len( cDivision ) - 2 )
         ELSE
            IF nDivision > Int( nDivision )
               nDivision++
            ENDIF
         ENDIF
      ELSE //
         cDivision := Transform( nDivision, '9.9999999' )
         nDecimalLength := 8 - hmg_len( AllTrim( Str( Val( Right( cDivision, 7 ) ) ) ) )
         nDecimal := Val( Right( cDivision, 7 ) )
         cDecimal := hb_ntos( nDecimal )
         nDivision := ( Val( AllTrim( Left( cDecimal, 1 ) ) ) + 1 ) / ( 10 ^ nDecimalLength )
      ENDIF
      ::nRange := ::nVDivision * nDivision
      IF hmg_len( ::cPicture ) == 0
         ::cPicture := Replicate( '9', Max( hmg_len( AllTrim( Str( ::nUpper ) ) ), hmg_len( AllTrim( Str( ::nLower ) ) ) ) )
      ENDIF
      // find scales
      ASize( ::aScale, 0 )
      nDistance := ::nRange / ::nVDivision
      FOR i := ::nUpperDivision TO 1 STEP -1
         AAdd( ::aScale, AllTrim( Transform( nDistance * i, ::cPicture ) ) )
      NEXT i
      AAdd( ::aScale, AllTrim( Transform( 0, ::cPicture ) ) )
      nZero := hmg_len( ::aScale )
      FOR i := 1 TO ::nLowerDivision
         AAdd( ::aScale, '-' + AllTrim( Transform( nDistance * i, ::cPicture ) ) )
      NEXT i
      // find maxscaleitemwidth
      nMaxWidth := 0
      FOR i := 1 TO hmg_len( ::aScale )
         aSize := BT_DrawTextSize ( ::hDC, ::aScale[ i ], ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], iif( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ) )
         nMaxWidth := Max( nMaxWidth, aSize[ 1 ] )
      NEXT i
      ::nMaxScaleWidth := nMaxWidth
      // draw all items other than graph
      ::nTop := ::nTop + 20
      ::nBottom := ::nBottom - 20
      IF ::lShowCategories
         ::DrawCategories()
      ENDIF
      ::nRight := ::nRight - iif( ::nLegendPos == 1 .AND. ::lGraphRotated, iif( Len( ::cPicture ) < 8, 20, 40 ), 5 )
      // scale
      IF ::lShowValues
         IF .NOT. ::lGraphRotated // not bar
            ::DrawVerticalScale()
         ELSE
            ::DrawBarScale()
         ENDIF
      ENDIF
      ::nLeft := ::nLeft + 5
      // find zero position
      DO CASE
      CASE nZero == 1 // all are negative!
         IF .NOT. ::lGraphRotated // bar
            ::nZeroPos := ::nTop
         ELSE
            ::nZeroPos := ::nRight
         ENDIF
      CASE nZero == hmg_len( ::aScale ) // all are positive!
         IF .NOT. ::lGraphRotated // other than bar
            ::nZeroPos := ::nBottom
         ELSE
            ::nZeroPos := ::nLeft
         ENDIF
      OTHERWISE // zero position in the middles
         IF .NOT. ::lGraphRotated // other than bar
            ::nZeroPos := ::nTop + ( ( ::nBottom - ::nTop ) / ::nVDivision * ( nZero - 1 ) )
            ::nPixelperPoint := ( ::nBottom - ::nTop ) / ::nRange
         ELSE // bar
            ::nZeroPos := ::nRight - ( ( ::nRight - ::nLeft ) / ::nVDivision * ( nZero - 1 ) )
            ::nPixelperPoint := ( ::nRight - ::nLeft ) / ::nRange
         ENDIF
      ENDCASE
      ::nZeroPos := Int( ::nZeroPos )
      IF ::lHGrid
         ::DrawHorizontalGrid()
      ENDIF
      IF ::lVGrid
         ::DrawVerticalGrid()
      ENDIF
   ENDIF
   DO CASE
   CASE ::nGraphType == GT_COLUMNS
      ::DrawColumnGraph()
   CASE ::nGraphType == GT_LINE
      ::DrawLineGraph()
   CASE ::nGraphType == GT_POINTS
      ::DrawPointsGraph()
   CASE ::nGraphType == GT_PIE
      ::DrawPieGraph()
   CASE ::nGraphType == GT_BAR
      ::DrawBarGraph()
   CASE ::nGraphType == GT_STACKEDCOLUMN
      ::DrawStackedColumnGraph()
   CASE ::nGraphType == GT_STACKEDBAR
      ::DrawStackedBarGraph()
   CASE ::nGraphType == GT_AREA
      ::DrawAreaGraph()
   CASE ::nGraphType == GT_DOUGHNUT
      ::DrawDoughnutGraph()
   CASE ::nGraphType == GT_SUNBURST
      ::DrawSunBurstGraph()
   CASE ::nGraphType == GT_WATERFALL
      ::DrawWaterfallGraph()
   CASE ::nGraphType == GT_TREEMAP
      ::DrawTreeMapGraph()
   ENDCASE
   BT_DeleteDC( ::BTstruct )

RETURN NIL

METHOD DrawColumnGraph CLASS GraphPlus

   LOCAL nCategories := 0
   LOCAL nRow, nCol, nData
   LOCAL nTotalBars
   LOCAL nAvailableWidth
   LOCAL nTotalBarRatio
   LOCAL nCategoryRatio

   LOCAL nBarWidth
   LOCAL nCategoryWidth
   LOCAL nBarGap
   LOCAL i, j, nBarHeight

   IF hmg_len( ::aData ) > 0
      IF ValType( ::aData[ 1 ] ) == 'A'
         nCategories := hmg_len( ::aData[ 1 ] )
      ENDIF
   ENDIF
   IF nCategories == 0
      RETURN NIL
   ENDIF
   nTotalBars := nCategories * hmg_len( ::aData )
   nAvailableWidth := ::nRight - ::nLeft
   nTotalBarRatio := nTotalBars * 1 // 100%
   nTotalBarRatio := nTotalBarRatio + ( nTotalBarRatio - nCategories ) * ::nBarGapRatio
   nCategoryRatio := nCategories * ::nGapWidthRatio // n Times of Bar size for each category
   nTotalBarRatio := nTotalBarRatio + nCategoryRatio

   nBarWidth := nAvailableWidth / nTotalBarRatio // 100% width
   nCategoryWidth := nBarWidth * ::nGapWidthRatio
   nBarGap := nBarWidth * ::nBarGapRatio

   nCol := ::nLeft + ( nCategoryWidth / 2 )
   nRow := ::nZeroPos
   FOR i := 1 TO hmg_len( ::aData[ 1 ] )
      FOR j := 1 TO hmg_len( ::aData )
         nData := ::aData[ j, i ]
         nBarHeight := Int( ( ::nBottom - ::nTop ) * ( Abs( nData ) / ::nRange ) )
         IF nData > 0
            // positive
            BT_DrawFillRectangle ( ::hDC, nRow - nBarHeight, nCol, nBarWidth, nBarHeight, ::aColors[ j ], ::aColors[ j ], 1 )
            IF ::lDataLabels
               BT_DrawText ( ::hDC, nRow - nBarHeight - 15, nCol + ( nBarWidth / 2 ), AllTrim( Transform( nData, ::cPicture ) ), ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], ::aTextColor, ::aBackColor, if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ), BT_TEXT_CENTER )
            ENDIF
         ELSE
            // negative
            BT_DrawFillRectangle ( ::hDC, nRow, nCol, nBarWidth, nBarHeight, ::aColors[ j ], ::aColors[ j ], 1 )
            IF ::lDataLabels
               BT_DrawText ( ::hDC, nRow + nBarHeight + 3, nCol + ( nBarWidth / 2 ), if( nData < 0, '-', '' ) + AllTrim( Transform( Abs( nData ), ::cPicture ) ), ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], ::aTextColor, ::aBackColor, if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ), BT_TEXT_CENTER )
            ENDIF
         ENDIF
         nCol := nCol + nBarWidth
         IF j < hmg_len( ::aData )
            nCol := nCol + nBarGap
         ENDIF
      NEXT j
      nCol := nCol + nCategoryWidth
   NEXT i

RETURN NIL

METHOD DrawVerticalScale CLASS GraphPlus

   LOCAL nScaleItems
   LOCAL nRow
   LOCAL nAvailableHeight
   LOCAL nTotalBarRatio
   LOCAL nScaleItemHeight
   LOCAL aSize, i
   LOCAL nMaxWidth := 0
   nScaleItems := hmg_len( ::aScale )
   IF nScaleItems == 0
      RETURN NIL
   ENDIF
   nAvailableHeight := ::nBottom - ::nTop
   nTotalBarRatio := nScaleItems - 1 // equal space between scales
   nScaleItemHeight := nAvailableHeight / nTotalBarRatio
   nRow := ::nTop
   FOR i := 1 TO hmg_len( ::aScale )
      aSize := BT_DrawTextSize ( ::hDC, ::aScale[ i ], ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ) )
      nMaxWidth := Max( nMaxWidth, aSize[ 1 ] )
   NEXT i
   ::nLeft := ::nLeft + nMaxWidth
   FOR i := 1 TO hmg_len( ::aScale )
      BT_DrawText ( ::hDC, nRow + 2, ::nLeft, ::aScale[ i ], ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], ::aTextColor, ::aBackColor, if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ), BT_TEXT_BASELINE + BT_TEXT_RIGHT )
      nRow := nRow + nScaleItemHeight
   NEXT i

RETURN NIL

METHOD DrawHorizontalGrid CLASS GraphPlus

   LOCAL i
   LOCAL nHeight := ( ::nBottom - ::nTop ) / ::nVDivision
   LOCAL nRow
   nRow := ::nTop
   IF .NOT. ::lGraphRotated // not bar
      FOR i := 1 TO hmg_len( ::aScale )
         BT_DrawLine ( ::hDC, nRow, ::nLeft, nRow, ::nRight, ::aGridLineColor, ::nLineWidth )
         nRow := nRow + nHeight
      NEXT i
   ELSE
      nHeight := ( ::nBottom - ::nTop ) / hmg_len( ::aCategories )
      nRow := ::nTop
      FOR i := 1 TO hmg_len( ::aCategories )
         BT_DrawLine ( ::hDC, nRow, ::nLeft, nRow, ::nRight, ::aGridLineColor, ::nLineWidth )
         nRow := nRow + nHeight
      NEXT i
      BT_DrawLine ( ::hDC, nRow, ::nLeft, nRow, ::nRight, ::aGridLineColor, ::nLineWidth )
   ENDIF

RETURN NIL

METHOD DrawVerticalGrid CLASS GraphPlus

   LOCAL i
   LOCAL nCategories
   LOCAL nCol, nWidth
   LOCAL nAvailableWidth
   LOCAL nTotalBarRatio
   LOCAL nCategoryWidth
   IF .NOT. ::lGraphRotated // columns and like
      nCategories := hmg_len( ::aData[ 1 ] )
      IF nCategories == 0
         RETURN NIL
      ENDIF
      nAvailableWidth := ::nRight - ::nLeft
      IF ::nGraphType <> GT_AREA // Area charts starts at left and ends at right
         nTotalBarRatio := nCategories // equal space between categories
         nCategoryWidth := nAvailableWidth / nTotalBarRatio // 100% width
      ELSE
         nTotalBarRatio := nCategories - 1 // equal space between categories
         nCategoryWidth := nAvailableWidth / nTotalBarRatio // 100% width
      ENDIF
      BT_DrawLine ( ::hDC, ::nTop, ::nLeft, ::nBottom, ::nLeft, ::aGridLineColor, ::nLineWidth )
      nCol := ::nLeft
      FOR i := 1 TO nCategories
         nCol := nCol + nCategoryWidth
         BT_DrawLine ( ::hDC, ::nTop, nCol, ::nBottom, nCol, ::aGridLineColor, ::nLineWidth )
      NEXT i
   ELSE
      nWidth := ( ::nRight - ::nLeft ) / ( hmg_len( ::aScale ) - 1 )
      nCol := ::nLeft
      FOR i := 1 TO hmg_len( ::aScale )
         BT_DrawLine ( ::hDC, ::nTop, nCol, ::nBottom, nCol, ::aGridLineColor, ::nLineWidth )
         nCol := nCol + nWidth
      NEXT i
      BT_DrawLine ( ::hDC, ::nTop, nCol, ::nBottom, nCol, ::aGridLineColor, ::nLineWidth )
   ENDIF

RETURN NIL

METHOD DrawBarScale CLASS GraphPlus

   LOCAL i
   LOCAL nScaleItems
   LOCAL nCol
   LOCAL nAvailableWidth
   LOCAL nTotalBarRatio
   LOCAL nScaleItemWidth
   nScaleItems := hmg_len( ::aScale )
   IF nScaleItems == 0
      RETURN NIL
   ENDIF
   nAvailableWidth := ::nRight - ::nLeft
   nTotalBarRatio := nScaleItems - 1 // equal space between categories
   nScaleItemWidth := nAvailableWidth / nTotalBarRatio // 100% width
   nCol := ::nLeft
   FOR i := hmg_len( ::aScale ) TO 1 STEP -1
      BT_DrawText ( ::hDC, ::nBottom, nCol, ::aScale[ i ], ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], ::aTextColor, ::aBackColor, if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ), BT_TEXT_CENTER )
      nCol := nCol + nScaleItemWidth
   NEXT i

RETURN NIL


METHOD DrawLineGraph CLASS GraphPlus

   LOCAL nCategories := 0
   LOCAL nRow, nCol, nData, nFirstCol, nPrevRow := 0, nPrevCol := 0
   LOCAL nAvailableWidth
   LOCAL nTotalBarRatio

   LOCAL nBarWidth
   LOCAL nCategoryWidth
   LOCAL i, j, nBarHeight

   IF hmg_len( ::aData ) > 0
      IF ValType( ::aData[ 1 ] ) == 'A'
         nCategories := hmg_len( ::aData[ 1 ] )
      ENDIF
   ENDIF
   IF nCategories == 0
      RETURN NIL
   ENDIF
   nAvailableWidth := ::nRight - ::nLeft
   nTotalBarRatio := nCategories // equal space between categories

   nBarWidth := nAvailableWidth / nTotalBarRatio // 100% width
   nCategoryWidth := nBarWidth
   nFirstCol := ::nLeft + ( nCategoryWidth / 2 )
   nRow := ::nZeroPos
   FOR j := 1 TO hmg_len( ::aData )
      nCol := nFirstCol
      FOR i := 1 TO hmg_len( ::aData[ 1 ] )
         nData := ::aData[ j, i ]
         nBarHeight := Int( ( ::nBottom - ::nTop ) * ( Abs( nData ) / ::nRange ) )
         IF nData > 0
            // positive
            BT_DrawFillEllipse ( ::hDC, nRow - nBarHeight - ::nPointSize / 2, nCol - ::nPointSize / 2, ::nPointSize, ::nPointSize, ::aColors[ j ], ::aColors[ j ], 1 )
            IF ::lDataLabels
               BT_DrawText ( ::hDC, nRow - nBarHeight - 15 - ::nPointSize / 2, nCol, AllTrim( Transform( nData, ::cPicture ) ), ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], ::aTextColor, ::aBackColor, if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ), BT_TEXT_CENTER )
            ENDIF
            IF i <> 1 // first point
               BT_DrawLine ( ::hDC, nPrevRow, nPrevCol, nRow - nBarHeight, nCol, ::aColors[ j ], ::nLineWidth )
            ENDIF
            nPrevRow := nRow - nBarHeight
            nPrevCol := nCol
         ELSE
            // negative
            BT_DrawFillEllipse ( ::hDC, nRow + nBarHeight - ::nPointSize / 2, nCol - ::nPointSize / 2, ::nPointSize, ::nPointSize, ::aColors[ j ], ::aColors[ j ], 1 )
            IF ::lDataLabels
               BT_DrawText ( ::hDC, nRow + nBarHeight + 3 + ::nPointSize / 2, nCol, if( nData < 0, '-', '' ) + AllTrim( Transform( Abs( nData ), ::cPicture ) ), ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], ::aTextColor, ::aBackColor, if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ), BT_TEXT_CENTER )
            ENDIF
            IF i <> 1
               BT_DrawLine ( ::hDC, nPrevRow, nPrevCol, nRow + nBarHeight, nCol, ::aColors[ j ], ::nLineWidth )
            ENDIF
            nPrevRow := nRow + nBarHeight
            nPrevCol := nCol
         ENDIF
         nCol := nCol + nCategoryWidth
      NEXT i
   NEXT j

RETURN NIL

METHOD DrawPointsGraph CLASS GraphPlus

   LOCAL nCategories := 0
   LOCAL nRow, nCol, nData
   LOCAL nAvailableWidth
   LOCAL nTotalBarRatio

   LOCAL nBarWidth
   LOCAL nCategoryWidth
   LOCAL i, j, nBarHeight

   IF hmg_len( ::aData ) > 0
      IF ValType( ::aData[ 1 ] ) == 'A'
         nCategories := hmg_len( ::aData[ 1 ] )
      ENDIF
   ENDIF
   IF nCategories == 0
      RETURN NIL
   ENDIF
   nAvailableWidth := ::nRight - ::nLeft
   nTotalBarRatio := nCategories // equal space between categories

   nBarWidth := nAvailableWidth / nTotalBarRatio // 100% width
   nCategoryWidth := nBarWidth

   nCol := ::nLeft + ( nCategoryWidth / 2 )
   nRow := ::nZeroPos
   FOR i := 1 TO hmg_len( ::aData[ 1 ] )
      FOR j := 1 TO hmg_len( ::aData )
         nData := ::aData[ j, i ]
         nBarHeight := Int( ( ::nBottom - ::nTop ) * ( Abs( nData ) / ::nRange ) )
         IF nData > 0
            // positive
            BT_DrawFillEllipse ( ::hDC, nRow - nBarHeight - ::nPointSize / 2, nCol - ::nPointSize / 2, ::nPointSize, ::nPointSize, ::aColors[ j ], ::aColors[ j ], 1 )
            IF ::lDataLabels
               BT_DrawText ( ::hDC, nRow - nBarHeight - 15 - ::nPointSize / 2, nCol, AllTrim( Transform( nData, ::cPicture ) ), ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], ::aTextColor, ::aBackColor, if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ), BT_TEXT_CENTER )
            ENDIF
         ELSE
            // negative
            BT_DrawFillEllipse ( ::hDC, nRow + nBarHeight - ::nPointSize / 2, nCol - ::nPointSize / 2, ::nPointSize, ::nPointSize, ::aColors[ j ], ::aColors[ j ], 1 )
            IF ::lDataLabels
               BT_DrawText ( ::hDC, nRow + nBarHeight + 3 + ::nPointSize / 2, nCol, if( nData < 0, '-', '' ) + AllTrim( Transform( Abs( nData ), ::cPicture ) ), ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], ::aTextColor, ::aBackColor, if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ), BT_TEXT_CENTER )
            ENDIF
         ENDIF
      NEXT j
      nCol := nCol + nCategoryWidth
   NEXT i

RETURN NIL

METHOD DrawPieGraph CLASS GraphPlus

   LOCAL aPoints
   LOCAL aDegrees := {}
   LOCAL aCumulative := {}
   LOCAL i, sum := 0, lsum := 0
   LOCAL cValue, n
   LOCAL previous_cumulative
   LOCAL aSerieValues := {}
   LOCAL nChartSize
   LOCAL aLabelCumulative := {}
   LOCAL ser_sum := 0
   LOCAL nFromRadialRow, nFromRadialCol
   LOCAL nToRadialCol, nToRadialRow

   FOR i := 1 TO hmg_len( ::aData[ 1 ] )
      AAdd( aSerieValues, ::aData[ 1, i ] )
   NEXT i
   ::nLeft += 25
   ::nRight -= 25
   ::nBottom -= 25
   ::nTop += 25
   nChartSize := Min( ::nRight - ::nLeft, ::nBottom - ::nTop )
   aPoints := ::FindPieCoordinates( nChartSize )
   FOR i := 1 TO HMG_LEN( aSerieValues )
      ser_sum += aSerieValues[ i ]
   NEXT i
   FOR i := 1 TO HMG_LEN( aSerieValues )
      AAdd( aDegrees, Round( aSerieValues[ i ] / ser_sum * 360, 0 ) )
   NEXT i
   sum := 0
   FOR i := 1 TO HMG_LEN( aDegrees )
      sum += aDegrees[ i ]
   NEXT i
   IF sum <> 360
      aDegrees[ HMG_LEN( aDegrees ) ] := aDegrees[ HMG_LEN( aDegrees ) ] + ( 360 - sum )
   ENDIF

   sum := 0
   lSum := 0
   FOR i := 1 TO HMG_LEN( aDegrees )
      sum := sum + aDegrees[ i ]
      AAdd( aCumulative, sum )
      lSum := lSum + ( aDegrees[ i ] / 2 )
      AAdd( aLabelCumulative, lSum )
      lSum := lSum + ( aDegrees[ i ] / 2 )
   NEXT i
   FOR i := 1 TO hmg_len( aCumulative )
      IF aCumulative[ i ] > 90
         aCumulative[ i ] := 450 - aCumulative[ i ]
      ELSE
         aCumulative[ i ] := 90 - aCumulative[ i ]
      ENDIF
      IF aLabelCumulative[ i ] > 90
         aLabelCumulative[ i ] := 450 - aLabelCumulative[ i ]
      ELSE
         aLabelCumulative[ i ] := 90 - aLabelCumulative[ i ]
      ENDIF
   NEXT i


   previous_cumulative := -1

   nFromRadialRow := aPoints[ 3 ] // toprightrow
   nFromRadialCol := aPoints[ 9 ] // middletopcol
   FOR i := 1 TO HMG_LEN( aCumulative )
      IF aDegrees[ i ] > 0
         IF aCumulative[ i ] == previous_cumulative
            LOOP
         ENDIF
         previous_cumulative := aCumulative[ i ]
         DO CASE
         CASE aCumulative[ i ] <= 45
            nToRadialCol := aPoints[ 14 ] // middlerightcol
            nToRadialRow := aPoints[ 13 ] - Round( aCumulative[ i ] / 45 * ( aPoints[ 13 ] - aPoints[ 3 ] ), 0 )
         CASE aCumulative[ i ] <= 90 .AND. aCumulative[ i ] > 45
            nToRadialRow := aPoints[ 3 ]
            nToRadialCol := aPoints[ 4 ] - Round( ( aCumulative[ i ] - 45 ) / 45 * ( aPoints[ 4 ] - aPoints[ 9 ] ), 0 )
         CASE aCumulative[ i ] <= 135 .AND. aCumulative[ i ] > 90
            nToRadialRow := aPoints[ 1 ]
            nToRadialCol := aPoints[ 9 ] - Round( ( aCumulative[ i ] - 90 ) / 45 * ( aPoints[ 9 ] - aPoints[ 2 ] ), 0 )
         CASE aCumulative[ i ] <= 180 .AND. aCumulative[ i ] > 135
            nToRadialCol := aPoints[ 2 ]
            nToRadialRow := aPoints[ 1 ] + Round( ( aCumulative[ i ] - 135 ) / 45 * ( aPoints[ 10 ] - aPoints[ 1 ] ), 0 )
         CASE aCumulative[ i ] <= 225 .AND. aCumulative[ i ] > 180
            nToRadialCol := aPoints[ 2 ]
            nToRadialRow := aPoints[ 10 ] + Round( ( aCumulative[ i ] - 180 ) / 45 * ( aPoints[ 7 ] - aPoints[ 10 ] ), 0 )
         CASE aCumulative[ i ] <= 270 .AND. aCumulative[ i ] > 225
            nToRadialRow := aPoints[ 7 ]
            nToRadialCol := aPoints[ 8 ] + Round( ( aCumulative[ i ] - 225 ) / 45 * ( aPoints[ 12 ] - aPoints[ 8 ] ), 0 )
         CASE aCumulative[ i ] <= 315 .AND. aCumulative[ i ] > 270
            nToRadialRow := aPoints[ 7 ]
            nToRadialCol := aPoints[ 12 ] + Round( ( aCumulative[ i ] - 270 ) / 45 * ( aPoints[ 6 ] - aPoints[ 12 ] ), 0 )
         CASE aCumulative[ i ] <= 360 .AND. aCumulative[ i ] > 315
            nToRadialCol := aPoints[ 6 ]
            nToRadialRow := aPoints[ 5 ] - Round( ( aCumulative[ i ] - 315 ) / 45 * ( aPoints[ 5 ] - aPoints[ 13 ] ), 0 )
         ENDCASE
         BT_DrawPie ( ::hDC, aPoints[ 1 ], aPoints[ 2 ], aPoints[ 5 ], aPoints[ 4 ], nToRadialRow, nToRadialCol, nFromRadialRow, nFromRadialCol, ::aBackColor, ::nPieGap, ::aColors[ i ] )
         nFromRadialRow := nToRadialRow
         nFromRadialCol := nToRadialCol
      ENDIF
   NEXT i
   IF ::lDataLabels
      n := 1
      nChartSize := nChartSize / 1.5
      aPoints := ::FindPieCoordinates( nChartSize )
      previous_cumulative := -1
      FOR i := 1 TO HMG_LEN( aLabelCumulative )
         IF aDegrees[ i ] > 0
            IF aLabelCumulative[ i ] == previous_cumulative
               LOOP
            ENDIF
            cValue := AllTrim( Transform( aSerieValues[ i ], ::cPicture ) ) + ' (' + AllTrim( Transform( aSerieValues[ i ] / ser_sum * 100, '999.99' ) ) + '%)'
            previous_cumulative := aLabelCumulative[ i ]
            DO CASE
            CASE aLabelCumulative[ i ] <= 45
               nToRadialCol := aPoints[ 14 ] // middlerightcol
               nToRadialRow := aPoints[ 13 ] - Round( aLabelCumulative[ i ] / 45 * ( aPoints[ 13 ] - aPoints[ 3 ] ), 0 )
               IF n++ > 1
                  nToRadialRow -= 20
               ENDIF
            CASE aLabelCumulative[ i ] <= 90 .AND. aLabelCumulative[ i ] > 45
               nToRadialRow := aPoints[ 3 ]
               nToRadialCol := aPoints[ 4 ] - Round( ( aLabelCumulative[ i ] - 45 ) / 45 * ( aPoints[ 4 ] - aPoints[ 9 ] ), 0 )
               IF n++ > 1
                  nToRadialRow -= 20
               ENDIF
            CASE aLabelCumulative[ i ] <= 135 .AND. aLabelCumulative[ i ] > 90
               nToRadialRow := aPoints[ 1 ]
               nToRadialCol := aPoints[ 9 ] - Round( ( aLabelCumulative[ i ] - 90 ) / 45 * ( aPoints[ 9 ] - aPoints[ 2 ] ), 0 )
               IF n++ > 3
                  nToRadialRow -= 20
               ENDIF
            CASE aLabelCumulative[ i ] <= 180 .AND. aLabelCumulative[ i ] > 135
               nToRadialCol := aPoints[ 2 ]
               nToRadialRow := aPoints[ 1 ] + Round( ( aLabelCumulative[ i ] - 135 ) / 45 * ( aPoints[ 10 ] - aPoints[ 1 ] ), 0 )
               IF n++ > 4
                  nToRadialRow -= 20
               ENDIF
            CASE aLabelCumulative[ i ] <= 225 .AND. aLabelCumulative[ i ] > 180
               nToRadialCol := aPoints[ 2 ]
               nToRadialRow := aPoints[ 10 ] + Round( ( aLabelCumulative[ i ] - 180 ) / 45 * ( aPoints[ 7 ] - aPoints[ 10 ] ), 0 )
               IF n++ > 5
                  nToRadialRow -= 20
               ENDIF
            CASE aLabelCumulative[ i ] <= 270 .AND. aLabelCumulative[ i ] > 225
               nToRadialRow := aPoints[ 7 ]
               nToRadialCol := aPoints[ 8 ] + Round( ( aLabelCumulative[ i ] - 225 ) / 45 * ( aPoints[ 12 ] - aPoints[ 8 ] ), 0 )
               IF n++ > 6
                  nToRadialRow -= 20
               ENDIF
            CASE aLabelCumulative[ i ] <= 315 .AND. aLabelCumulative[ i ] > 270
               nToRadialRow := aPoints[ 7 ]
               nToRadialCol := aPoints[ 12 ] + Round( ( aLabelCumulative[ i ] - 270 ) / 45 * ( aPoints[ 6 ] - aPoints[ 12 ] ), 0 )
               IF n++ > 7
                  nToRadialRow -= 20
               ENDIF
            CASE aLabelCumulative[ i ] <= 360 .AND. aLabelCumulative[ i ] > 315
               nToRadialCol := aPoints[ 6 ]
               nToRadialRow := aPoints[ 5 ] - Round( ( aLabelCumulative[ i ] - 315 ) / 45 * ( aPoints[ 5 ] - aPoints[ 13 ] ), 0 )
               IF n > 8
                  nToRadialRow -= 20
               ENDIF
            ENDCASE
            BT_DrawText ( ::hDC, nToRadialRow, nToRadialCol, cValue, ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], ::aTextColor, ::aColors[ i ], if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ), BT_TEXT_CENTER )
         ENDIF
      NEXT i
   ENDIF

RETURN NIL

METHOD DrawDoughnutGraph CLASS GraphPlus

   LOCAL nFromRadialRow, nFromRadialCol, nToRadialRow, nToRadialCol
   LOCAL aDegrees := {}
   LOCAL aCumulative := {}
   LOCAL j, i, sum := 0, lsum := 0
   LOCAL previous_cumulative
   LOCAL aSerieValues := {}
   LOCAL nChartSize
   LOCAL aLabelCumulative := {}
   LOCAL ser_sum := 0, cValue
   LOCAL nCircles := hmg_len( ::aCategories )
   LOCAL nCircleRatio := nCircles + ( nCircles * ::nBarGapRatio )
   LOCAL nPieWidth
   LOCAL nHoleSize
   LOCAL nCenterRow
   LOCAL nCenterCol
   LOCAL aPoints
   LOCAL nOriginalSize

   ::nLeft += 25
   ::nRight -= 25
   ::nBottom -= 25
   ::nTop += 25
   nChartSize := Min( ::nRight - ::nLeft, ::nBottom - ::nTop )
   nOriginalSize := nChartSize
   nPieWidth := nChartSize / nCircleRatio
   nHoleSize := nPieWidth * ::nBarGapRatio
   IF hmg_len( ::aData ) == 0
      RETURN NIL
   ENDIF
   IF .NOT. ( ValType( ::aData ) == 'A' .AND. hmg_len( ::aData[ 1 ] ) > 0 )
      RETURN NIL
   ENDIF
   FOR j := 1 TO hmg_len( ::aData[ 1 ] )
      ASize( aSerieValues, 0 )
      ASize( aDegrees, 0 )
      ASize( aCumulative, 0 )
      ASize( aLabelCumulative, 0 )
      FOR i := 1 TO hmg_len( ::aData )
         AAdd( aSerieValues, ::aData[ i, j ] )
      NEXT i
      ser_sum := 0
      FOR i := 1 TO HMG_LEN( aSerieValues )
         ser_sum += aSerieValues[ i ]
      NEXT i
      FOR i := 1 TO HMG_LEN( aSerieValues )
         AAdd( aDegrees, Round( aSerieValues[ i ] / ser_sum * 360, 0 ) )
      NEXT i
      sum := 0
      FOR i := 1 TO HMG_LEN( aDegrees )
         sum += aDegrees[ i ]
      NEXT i
      IF sum <> 360
         aDegrees[ HMG_LEN( aDegrees ) ] := aDegrees[ HMG_LEN( aDegrees ) ] + ( 360 - sum )
      ENDIF

      sum := 0
      lSum := 0
      FOR i := 1 TO HMG_LEN( aDegrees )
         sum := sum + aDegrees[ i ]
         AAdd( aCumulative, sum )
         lSum := lSum + ( aDegrees[ i ] / 2 )
         AAdd( aLabelCumulative, lSum )
         lSum := lSum + ( aDegrees[ i ] / 2 )
      NEXT i
      FOR i := 1 TO hmg_len( aCumulative )
         IF aCumulative[ i ] > 90
            aCumulative[ i ] := 450 - aCumulative[ i ]
         ELSE
            aCumulative[ i ] := 90 - aCumulative[ i ]
         ENDIF
         IF aLabelCumulative[ i ] > 90
            aLabelCumulative[ i ] := 450 - aLabelCumulative[ i ]
         ELSE
            aLabelCumulative[ i ] := 90 - aLabelCumulative[ i ]
         ENDIF
      NEXT i
      previous_cumulative := -1
      aPoints := ::FindPieCoordinates( nChartSize )
      nFromRadialRow := aPoints[ 3 ] // toprightrow
      nFromRadialCol := aPoints[ 9 ] // middletopcol
      FOR i := 1 TO HMG_LEN( aCumulative )
         IF aDegrees[ i ] > 0
            IF aCumulative[ i ] == previous_cumulative
               LOOP
            ENDIF
            previous_cumulative := aCumulative[ i ]
            DO CASE
            CASE aCumulative[ i ] <= 45
               nToRadialCol := aPoints[ 14 ] // middlerightcol
               nToRadialRow := aPoints[ 13 ] - Round( aCumulative[ i ] / 45 * ( aPoints[ 13 ] - aPoints[ 3 ] ), 0 )
            CASE aCumulative[ i ] <= 90 .AND. aCumulative[ i ] > 45
               nToRadialRow := aPoints[ 3 ]
               nToRadialCol := aPoints[ 4 ] - Round( ( aCumulative[ i ] - 45 ) / 45 * ( aPoints[ 4 ] - aPoints[ 9 ] ), 0 )
            CASE aCumulative[ i ] <= 135 .AND. aCumulative[ i ] > 90
               nToRadialRow := aPoints[ 1 ]
               nToRadialCol := aPoints[ 9 ] - Round( ( aCumulative[ i ] - 90 ) / 45 * ( aPoints[ 9 ] - aPoints[ 2 ] ), 0 )
            CASE aCumulative[ i ] <= 180 .AND. aCumulative[ i ] > 135
               nToRadialCol := aPoints[ 2 ]
               nToRadialRow := aPoints[ 1 ] + Round( ( aCumulative[ i ] - 135 ) / 45 * ( aPoints[ 10 ] - aPoints[ 1 ] ), 0 )
            CASE aCumulative[ i ] <= 225 .AND. aCumulative[ i ] > 180
               nToRadialCol := aPoints[ 2 ]
               nToRadialRow := aPoints[ 10 ] + Round( ( aCumulative[ i ] - 180 ) / 45 * ( aPoints[ 7 ] - aPoints[ 10 ] ), 0 )
            CASE aCumulative[ i ] <= 270 .AND. aCumulative[ i ] > 225
               nToRadialRow := aPoints[ 7 ]
               nToRadialCol := aPoints[ 8 ] + Round( ( aCumulative[ i ] - 225 ) / 45 * ( aPoints[ 12 ] - aPoints[ 8 ] ), 0 )
            CASE aCumulative[ i ] <= 315 .AND. aCumulative[ i ] > 270
               nToRadialRow := aPoints[ 7 ]
               nToRadialCol := aPoints[ 12 ] + Round( ( aCumulative[ i ] - 270 ) / 45 * ( aPoints[ 6 ] - aPoints[ 12 ] ), 0 )
            CASE aCumulative[ i ] <= 360 .AND. aCumulative[ i ] > 315
               nToRadialCol := aPoints[ 6 ]
               nToRadialRow := aPoints[ 5 ] - Round( ( aCumulative[ i ] - 315 ) / 45 * ( aPoints[ 5 ] - aPoints[ 13 ] ), 0 )
            ENDCASE
            // draw pie
            BT_DrawPie ( ::hDC, aPoints[ 1 ], aPoints[ 2 ], aPoints[ 5 ], aPoints[ 4 ], nToRadialRow, nToRadialCol, nFromRadialRow, nFromRadialCol, ::aBackColor, ::nPieGap, ::aColors[ i ] )
            nFromRadialRow := nToRadialRow
            nFromRadialCol := nToRadialCol
         ENDIF
      NEXT i
      nChartSize := nChartSize - nPieWidth
      aPoints := ::FindPieCoordinates( nChartSize )
      // draw the hole!
      nCenterRow := aPoints[ 10 ] - nChartSize / 2
      nCenterCol := aPoints[ 9 ] - nChartSize / 2
      BT_DrawFillEllipse ( ::hDC, nCenterRow, nCenterCol, nChartSize, nChartSize, ::aBackColor, ::aBackColor, 1 )

      nChartSize := nChartSize - nHoleSize
   NEXT j
   IF ::lDataLabels
      nChartSize := nOriginalSize
      FOR j := 1 TO hmg_len( ::aData[ 1 ] )
         ASize( aSerieValues, 0 )
         ASize( aDegrees, 0 )
         ASize( aCumulative, 0 )
         ASize( aLabelCumulative, 0 )
         FOR i := 1 TO hmg_len( ::aData )
            AAdd( aSerieValues, ::aData[ i, j ] )
         NEXT i
         ser_sum := 0
         FOR i := 1 TO HMG_LEN( aSerieValues )
            ser_sum += aSerieValues[ i ]
         NEXT i
         FOR i := 1 TO HMG_LEN( aSerieValues )
            AAdd( aDegrees, Round( aSerieValues[ i ] / ser_sum * 360, 0 ) )
         NEXT i
         sum := 0
         FOR i := 1 TO HMG_LEN( aDegrees )
            sum += aDegrees[ i ]
         NEXT i
         IF sum <> 360
            aDegrees[ HMG_LEN( aDegrees ) ] := aDegrees[ HMG_LEN( aDegrees ) ] + ( 360 - sum )
         ENDIF

         sum := 0
         lSum := 0
         FOR i := 1 TO HMG_LEN( aDegrees )
            sum := sum + aDegrees[ i ]
            AAdd( aCumulative, sum )
            lSum := lSum + ( aDegrees[ i ] / 2 )
            AAdd( aLabelCumulative, lSum )
            lSum := lSum + ( aDegrees[ i ] / 2 )
         NEXT i
         FOR i := 1 TO hmg_len( aCumulative )
            IF aCumulative[ i ] > 90
               aCumulative[ i ] := 450 - aCumulative[ i ]
            ELSE
               aCumulative[ i ] := 90 - aCumulative[ i ]
            ENDIF
            IF aLabelCumulative[ i ] > 90
               aLabelCumulative[ i ] := 450 - aLabelCumulative[ i ]
            ELSE
               aLabelCumulative[ i ] := 90 - aLabelCumulative[ i ]
            ENDIF
         NEXT i
         aPoints := ::FindPieCoordinates( nChartSize - nPieWidth / 2 )
         previous_cumulative := -1
         FOR i := 1 TO HMG_LEN( aCumulative )
            IF aDegrees[ i ] > 0
               IF aLabelCumulative[ i ] == previous_cumulative
                  LOOP
               ENDIF
               cValue := AllTrim( Transform( aSerieValues[ i ], ::cPicture ) ) + ' (' + AllTrim( Transform( aSerieValues[ i ] / ser_sum * 100, '999.99' ) ) + '%)'
               previous_cumulative := aLabelCumulative[ i ]
               DO CASE
               CASE aLabelCumulative[ i ] <= 45
                  nToRadialCol := aPoints[ 14 ] // middlerightcol
                  nToRadialRow := aPoints[ 13 ] - Round( aLabelCumulative[ i ] / 45 * ( aPoints[ 13 ] - aPoints[ 3 ] ), 0 )
               CASE aLabelCumulative[ i ] <= 90 .AND. aLabelCumulative[ i ] > 45
                  nToRadialRow := aPoints[ 3 ]
                  nToRadialCol := aPoints[ 4 ] - Round( ( aLabelCumulative[ i ] - 45 ) / 45 * ( aPoints[ 4 ] - aPoints[ 9 ] ), 0 )
               CASE aLabelCumulative[ i ] <= 135 .AND. aLabelCumulative[ i ] > 90
                  nToRadialRow := aPoints[ 1 ]
                  nToRadialCol := aPoints[ 9 ] - Round( ( aLabelCumulative[ i ] - 90 ) / 45 * ( aPoints[ 9 ] - aPoints[ 2 ] ), 0 )
               CASE aLabelCumulative[ i ] <= 180 .AND. aLabelCumulative[ i ] > 135
                  nToRadialCol := aPoints[ 2 ]
                  nToRadialRow := aPoints[ 1 ] + Round( ( aLabelCumulative[ i ] - 135 ) / 45 * ( aPoints[ 10 ] - aPoints[ 1 ] ), 0 )
               CASE aLabelCumulative[ i ] <= 225 .AND. aLabelCumulative[ i ] > 180
                  nToRadialCol := aPoints[ 2 ]
                  nToRadialRow := aPoints[ 10 ] + Round( ( aLabelCumulative[ i ] - 180 ) / 45 * ( aPoints[ 7 ] - aPoints[ 10 ] ), 0 )
               CASE aLabelCumulative[ i ] <= 270 .AND. aLabelCumulative[ i ] > 225
                  nToRadialRow := aPoints[ 7 ]
                  nToRadialCol := aPoints[ 8 ] + Round( ( aLabelCumulative[ i ] - 225 ) / 45 * ( aPoints[ 12 ] - aPoints[ 8 ] ), 0 )
               CASE aLabelCumulative[ i ] <= 315 .AND. aLabelCumulative[ i ] > 270
                  nToRadialRow := aPoints[ 7 ]
                  nToRadialCol := aPoints[ 12 ] + Round( ( aLabelCumulative[ i ] - 270 ) / 45 * ( aPoints[ 6 ] - aPoints[ 12 ] ), 0 )
               CASE aLabelCumulative[ i ] <= 360 .AND. aLabelCumulative[ i ] > 315
                  nToRadialCol := aPoints[ 6 ]
                  nToRadialRow := aPoints[ 5 ] - Round( ( aLabelCumulative[ i ] - 315 ) / 45 * ( aPoints[ 5 ] - aPoints[ 13 ] ), 0 )
               ENDCASE
               BT_DrawText ( ::hDC, nToRadialRow, nToRadialCol, cValue, ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], ::aTextColor, ::aColors[ i ], if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ), BT_TEXT_CENTER )
            ENDIF
         NEXT i
         nChartSize := nChartSize - nPieWidth
         nChartSize := nChartSize - nHoleSize
      NEXT j
   ENDIF

RETURN NIL

METHOD DrawSunBurstGraph CLASS GraphPlus

   LOCAL nFromRadialRow, nFromRadialCol, nToRadialRow, nToRadialCol
   LOCAL aDegrees := {}
   LOCAL aCumulative := {}
   LOCAL j, i, SUM := 0, lsum := 0
   LOCAL cValue
   LOCAL previous_cumulative
   LOCAL aSerieValues := {}
   LOCAL nChartSize
   LOCAL aLabelCumulative := {}
   LOCAL ser_sum := 0
   LOCAL nCircles
   LOCAL nCircleRatio
   LOCAL nPieWidth
   LOCAL nHoleSize
   LOCAL nCenterRow
   LOCAL nCenterCol
   LOCAL aPoints
   LOCAL nOriginalSize
   LOCAL aColor
   LOCAL aLevelValue
   LOCAL aLegends := {}, aColors := {}
   ::nLevels := hmg_len( ::aData[ 1 ] ) - 1
   nCircles := ::nLevels
   nCircleRatio := nCircles + ( nCircles * ::nBarGapRatio )
   // convert data
   ::FillSunBurstData()
   ::InitColors()
   IF ::lShowLegends // now draw legends after finding legends
      FOR j := ::nLevels TO 1 STEP -1
         ASize( aLegends, 0 )
         ASize( aColors, 0 )
         FOR i := 1 TO hmg_len( ::aLevelHeaders[ j ] )
            AAdd( aLegends, ::aLevelHeaders[ j, i ] )
            aLevelValue := ::aLevelValues[ j, i ]
            aColor := ::aColors[ aLevelValue[ 1 ] ]
            AAdd( aColors, aColor )
         NEXT i
         ::DrawSunBurstLegends( AClone( aLegends ), AClone( aColors ) )
      NEXT j
   ENDIF
   ::nLeft += 25
   ::nRight -= 25
   ::nBottom -= 25
   ::nTop += 25
   nChartSize := Min( ::nRight - ::nLeft, ::nBottom - ::nTop )
   nOriginalSize := nChartSize
   nPieWidth := nChartSize / nCircleRatio
   nHoleSize := nPieWidth * ::nBarGapRatio
   IF hmg_len( ::aData ) == 0
      RETURN NIL
   ENDIF
   FOR j := hmg_len( ::aLevelValues ) TO 1 STEP -1
      ASize( aSerieValues, 0 )
      ASize( aDegrees, 0 )
      ASize( aCumulative, 0 )
      ASize( aLabelCumulative, 0 )
      FOR i := 1 TO hmg_len( ::aLevelValues[ j ] )
         aLevelValue := ::aLevelValues[ j, i ]
         AAdd( aSerieValues, aLevelValue[ 2 ] )
      NEXT i
      ser_sum := 0
      FOR i := 1 TO HMG_LEN( aSerieValues )
         ser_sum += aSerieValues[ i ]
      NEXT i
      FOR i := 1 TO HMG_LEN( aSerieValues )
         AAdd( aDegrees, Round( aSerieValues[ i ] / ser_sum * 360, 2 ) )
      NEXT i
      SUM := 0
      FOR i := 1 TO HMG_LEN( aDegrees )
         SUM += aDegrees[ i ]
      NEXT i
      IF SUM <> 360
         aDegrees[ HMG_LEN( aDegrees ) ] := aDegrees[ HMG_LEN( aDegrees ) ] + ( 360 - sum )
      ENDIF

      SUM := 0
      lSum := 0
      FOR i := 1 TO HMG_LEN( aDegrees )
         SUM := SUM + aDegrees[ i ]
         AAdd( aCumulative, sum )
         lSum := lSum + ( aDegrees[ i ] / 2 )
         AAdd( aLabelCumulative, lSum )
         lSum := lSum + ( aDegrees[ i ] / 2 )
      NEXT i
      FOR i := 1 TO hmg_len( aCumulative )
         IF aCumulative[ i ] > 90
            aCumulative[ i ] := 450 - aCumulative[ i ]
         ELSE
            aCumulative[ i ] := 90 - aCumulative[ i ]
         ENDIF
         IF aLabelCumulative[ i ] > 90
            aLabelCumulative[ i ] := 450 - aLabelCumulative[ i ]
         ELSE
            aLabelCumulative[ i ] := 90 - aLabelCumulative[ i ]
         ENDIF
      NEXT i
      previous_cumulative := -1
      aPoints := ::FindPieCoordinates( nChartSize )
      nFromRadialRow := aPoints[ 3 ] // toprightrow
      nFromRadialCol := aPoints[ 9 ] // middletopcol
      FOR i := 1 TO HMG_LEN( aCumulative )
         IF aCumulative[ i ] == previous_cumulative
            LOOP
         ENDIF
         previous_cumulative := aCumulative[ i ]
         DO CASE
         CASE aCumulative[ i ] <= 45
            nToRadialCol := aPoints[ 14 ] // middlerightcol
            nToRadialRow := aPoints[ 13 ] - Round( aCumulative[ i ] / 45 * ( aPoints[ 13 ] - aPoints[ 3 ] ), 0 )
         CASE aCumulative[ i ] <= 90 .AND. aCumulative[ i ] > 45
            nToRadialRow := aPoints[ 3 ]
            nToRadialCol := aPoints[ 4 ] - Round( ( aCumulative[ i ] - 45 ) / 45 * ( aPoints[ 4 ] - aPoints[ 9 ] ), 0 )
         CASE aCumulative[ i ] <= 135 .AND. aCumulative[ i ] > 90
            nToRadialRow := aPoints[ 1 ]
            nToRadialCol := aPoints[ 9 ] - Round( ( aCumulative[ i ] - 90 ) / 45 * ( aPoints[ 9 ] - aPoints[ 2 ] ), 0 )
         CASE aCumulative[ i ] <= 180 .AND. aCumulative[ i ] > 135
            nToRadialCol := aPoints[ 2 ]
            nToRadialRow := aPoints[ 1 ] + Round( ( aCumulative[ i ] - 135 ) / 45 * ( aPoints[ 10 ] - aPoints[ 1 ] ), 0 )
         CASE aCumulative[ i ] <= 225 .AND. aCumulative[ i ] > 180
            nToRadialCol := aPoints[ 2 ]
            nToRadialRow := aPoints[ 10 ] + Round( ( aCumulative[ i ] - 180 ) / 45 * ( aPoints[ 7 ] - aPoints[ 10 ] ), 0 )
         CASE aCumulative[ i ] <= 270 .AND. aCumulative[ i ] > 225
            nToRadialRow := aPoints[ 7 ]
            nToRadialCol := aPoints[ 8 ] + Round( ( aCumulative[ i ] - 225 ) / 45 * ( aPoints[ 12 ] - aPoints[ 8 ] ), 0 )
         CASE aCumulative[ i ] <= 315 .AND. aCumulative[ i ] > 270
            nToRadialRow := aPoints[ 7 ]
            nToRadialCol := aPoints[ 12 ] + Round( ( aCumulative[ i ] - 270 ) / 45 * ( aPoints[ 6 ] - aPoints[ 12 ] ), 0 )
         CASE aCumulative[ i ] <= 360 .AND. aCumulative[ i ] > 315
            nToRadialCol := aPoints[ 6 ]
            nToRadialRow := aPoints[ 5 ] - Round( ( aCumulative[ i ] - 315 ) / 45 * ( aPoints[ 5 ] - aPoints[ 13 ] ), 0 )
         ENDCASE
         // draw pie
         aLevelValue := ::aLevelValues[ j, i ]
         aColor := ::aColors[ aLevelValue[ 1 ] ]
         BT_DrawPie ( ::hDC, aPoints[ 1 ], aPoints[ 2 ], aPoints[ 5 ], aPoints[ 4 ], nToRadialRow, nToRadialCol, nFromRadialRow, nFromRadialCol, ::aBackColor, ::nPieGap, aColor )
         nFromRadialRow := nToRadialRow
         nFromRadialCol := nToRadialCol
      NEXT i
      nChartSize := nChartSize - nPieWidth
      aPoints := ::FindPieCoordinates( nChartSize )
      // draw the hole!
      nCenterRow := aPoints[ 10 ] - nChartSize / 2
      nCenterCol := aPoints[ 9 ] - nChartSize / 2
      BT_DrawFillEllipse ( ::hDC, nCenterRow, nCenterCol, nChartSize, nChartSize, ::aBackColor, ::aBackColor, 1 )

      nChartSize := nChartSize - nHoleSize
   NEXT j
   IF ::lDataLabels
      nChartSize := nOriginalSize
      FOR j := hmg_len( ::aLevelValues ) TO 1 STEP -1
         ASize( aSerieValues, 0 )
         ASize( aDegrees, 0 )
         ASize( aCumulative, 0 )
         ASize( aLabelCumulative, 0 )
         FOR i := 1 TO hmg_len( ::aLevelValues[ j ] )
            aLevelValue := ::aLevelValues[ j, i ]
            AAdd( aSerieValues, aLevelValue[ 2 ] )
         NEXT i
         ser_sum := 0
         FOR i := 1 TO HMG_LEN( aSerieValues )
            ser_sum += aSerieValues[ i ]
         NEXT i
         FOR i := 1 TO HMG_LEN( aSerieValues )
            AAdd( aDegrees, Round( aSerieValues[ i ] / ser_sum * 360, 2 ) )
         NEXT i
         SUM := 0
         FOR i := 1 TO HMG_LEN( aDegrees )
            SUM += aDegrees[ i ]
         NEXT i
         IF SUM <> 360
            aDegrees[ HMG_LEN( aDegrees ) ] := aDegrees[ HMG_LEN( aDegrees ) ] + ( 360 - sum )
         ENDIF

         SUM := 0
         lSum := 0
         FOR i := 1 TO HMG_LEN( aDegrees )
            SUM := SUM + aDegrees[ i ]
            AAdd( aCumulative, sum )
            lSum := lSum + ( aDegrees[ i ] / 2 )
            AAdd( aLabelCumulative, lSum )
            lSum := lSum + ( aDegrees[ i ] / 2 )
         NEXT i
         FOR i := 1 TO hmg_len( aCumulative )
            IF aCumulative[ i ] > 90
               aCumulative[ i ] := 450 - aCumulative[ i ]
            ELSE
               aCumulative[ i ] := 90 - aCumulative[ i ]
            ENDIF
            IF aLabelCumulative[ i ] > 90
               aLabelCumulative[ i ] := 450 - aLabelCumulative[ i ]
            ELSE
               aLabelCumulative[ i ] := 90 - aLabelCumulative[ i ]
            ENDIF
         NEXT i
         aPoints := ::FindPieCoordinates( nChartSize - nPieWidth / 2 )
         previous_cumulative := -1
         FOR i := 1 TO HMG_LEN( aCumulative )
            IF aLabelCumulative[ i ] == previous_cumulative
               LOOP
            ENDIF
            cValue := AllTrim( Transform( aSerieValues[ i ], ::cPicture ) ) + ' (' + AllTrim( Transform( aSerieValues[ i ] / ser_sum * 100, '999.99' ) ) + '%)'
            previous_cumulative := aLabelCumulative[ i ]
            DO CASE
            CASE aLabelCumulative[ i ] <= 45
               nToRadialCol := aPoints[ 14 ] // middlerightcol
               nToRadialRow := aPoints[ 13 ] - Round( aLabelCumulative[ i ] / 45 * ( aPoints[ 13 ] - aPoints[ 3 ] ), 0 )
            CASE aLabelCumulative[ i ] <= 90 .AND. aLabelCumulative[ i ] > 45
               nToRadialRow := aPoints[ 3 ]
               nToRadialCol := aPoints[ 4 ] - Round( ( aLabelCumulative[ i ] - 45 ) / 45 * ( aPoints[ 4 ] - aPoints[ 9 ] ), 0 )
            CASE aLabelCumulative[ i ] <= 135 .AND. aLabelCumulative[ i ] > 90
               nToRadialRow := aPoints[ 1 ]
               nToRadialCol := aPoints[ 9 ] - Round( ( aLabelCumulative[ i ] - 90 ) / 45 * ( aPoints[ 9 ] - aPoints[ 2 ] ), 0 )
            CASE aLabelCumulative[ i ] <= 180 .AND. aLabelCumulative[ i ] > 135
               nToRadialCol := aPoints[ 2 ]
               nToRadialRow := aPoints[ 1 ] + Round( ( aLabelCumulative[ i ] - 135 ) / 45 * ( aPoints[ 10 ] - aPoints[ 1 ] ), 0 )
            CASE aLabelCumulative[ i ] <= 225 .AND. aLabelCumulative[ i ] > 180
               nToRadialCol := aPoints[ 2 ]
               nToRadialRow := aPoints[ 10 ] + Round( ( aLabelCumulative[ i ] - 180 ) / 45 * ( aPoints[ 7 ] - aPoints[ 10 ] ), 0 )
            CASE aLabelCumulative[ i ] <= 270 .AND. aLabelCumulative[ i ] > 225
               nToRadialRow := aPoints[ 7 ]
               nToRadialCol := aPoints[ 8 ] + Round( ( aLabelCumulative[ i ] - 225 ) / 45 * ( aPoints[ 12 ] - aPoints[ 8 ] ), 0 )
            CASE aLabelCumulative[ i ] <= 315 .AND. aLabelCumulative[ i ] > 270
               nToRadialRow := aPoints[ 7 ]
               nToRadialCol := aPoints[ 12 ] + Round( ( aLabelCumulative[ i ] - 270 ) / 45 * ( aPoints[ 6 ] - aPoints[ 12 ] ), 0 )
            CASE aLabelCumulative[ i ] <= 360 .AND. aLabelCumulative[ i ] > 315
               nToRadialCol := aPoints[ 6 ]
               nToRadialRow := aPoints[ 5 ] - Round( ( aLabelCumulative[ i ] - 315 ) / 45 * ( aPoints[ 5 ] - aPoints[ 13 ] ), 0 )
            ENDCASE
            aLevelValue := ::aLevelValues[ j, i ]
            aColor := ::aColors[ aLevelValue[ 1 ] ]
            BT_DrawText ( ::hDC, nToRadialRow, nToRadialCol, cValue, ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], ::aTextColor, aColor, if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ), BT_TEXT_CENTER )
         NEXT i
         nChartSize := nChartSize - nPieWidth
         nChartSize := nChartSize - nHoleSize
      NEXT j
   ENDIF

RETURN NIL

METHOD DrawWaterfallGraph CLASS GraphPlus

   LOCAL nCategories := 0
   LOCAL nRow, nCol, nData
   LOCAL nTotalBars
   LOCAL nAvailableWidth
   LOCAL nTotalBarRatio
   LOCAL nCategoryRatio

   LOCAL nBarWidth
   LOCAL nCategoryWidth
   LOCAL aColor
   LOCAL nBarHeight, i

   IF hmg_len( ::aData ) > 0
      IF ValType( ::aData[ 1 ] ) == 'A'
         nCategories := hmg_len( ::aData[ 1 ] )
      ENDIF
   ENDIF
   IF nCategories == 0
      RETURN NIL
   ENDIF
   nTotalBars := nCategories
   nAvailableWidth := ::nRight - ::nLeft
   nCategoryRatio := nCategories * ::BarGapRatio // n Times of Bar size for each category
   nTotalBarRatio := nTotalBars + nCategoryRatio

   nBarWidth := nAvailableWidth / nTotalBarRatio // 100% width
   nCategoryWidth := nBarWidth * ::nBarGapRatio

   nCol := ::nLeft + ( nCategoryWidth / 2 )
   nRow := ::nZeroPos
   FOR i := 1 TO hmg_len( ::aData[ 1 ] )
      nData := ::aData[ 1, i ]
      nBarHeight := Int( ( ::nBottom - ::nTop ) * ( Abs( nData ) / ::nRange ) )
      IF ::aData[ 2, i ] // totalitem
         aColor := ::aColors[ 3 ] // total color
         IF nData > 0
            // positive
            BT_DrawFillRectangle ( ::hDC, ::nZeroPos - nBarHeight, nCol, nBarWidth, nBarHeight, aColor, aColor, 1 )
            IF ::lDataLabels
               BT_DrawText ( ::hDC, ::nZeroPos - nBarHeight + 5, nCol + ( nBarWidth / 2 ), AllTrim( Transform( nData, ::cPicture ) ), ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], ::aTextColor, aColor, if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ), BT_TEXT_CENTER )
            ENDIF
            nRow := ::nZeroPos - nBarHeight // this row becomes the starting row for next increase/decrease
         ELSE
            // negative
            BT_DrawFillRectangle ( ::hDC, ::nZeroPos, nCol, nBarWidth, nBarHeight, aColor, aColor, 1 )
            IF ::lDataLabels
               BT_DrawText ( ::hDC, ::nZeroPos + nBarHeight - ::nLabelHeight, nCol + ( nBarWidth / 2 ), if( nData < 0, '-', '' ) + AllTrim( Transform( Abs( nData ), ::cPicture ) ), ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], ::aTextColor, aColor, if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ), BT_TEXT_CENTER )
            ENDIF
            nRow := ::nZeroPos + nBarHeight // this row becomes the starting row for next increase/decrease
         ENDIF
      ELSE
         IF ::aData[ 1, i ] > 0.0 // increase
            aColor := ::aColors[ 1 ] // color for increase
            BT_DrawFillRectangle ( ::hDC, nRow - nBarHeight, nCol, nBarWidth, nBarHeight, aColor, aColor, 1 )
            IF ::lDataLabels
               BT_DrawText ( ::hDC, nRow - nBarHeight + 5, nCol + ( nBarWidth / 2 ), AllTrim( Transform( nData, ::cPicture ) ), ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], ::aTextColor, aColor, if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ), BT_TEXT_CENTER )
            ENDIF
            nRow := nRow - nBarHeight // this row becomes starting point for next increase/decrease
         ELSE // decrease
            aColor := ::aColors[ 2 ] // color for increase
            BT_DrawFillRectangle ( ::hDC, nRow, nCol, nBarWidth, nBarHeight, aColor, aColor, 1 )
            IF ::lDataLabels
               BT_DrawText ( ::hDC, nRow + nBarHeight - ::nLabelHeight, nCol + ( nBarWidth / 2 ), '-' + AllTrim( Transform( Abs( nData ), ::cPicture ) ), ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], ::aTextColor, aColor, if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ), BT_TEXT_CENTER )
            ENDIF
            nRow := nRow + nBarHeight // this row becomes starting point for next
         ENDIF
      ENDIF
      nCol := nCol + nBarWidth
      nCol := nCol + nCategoryWidth
   NEXT i

RETURN NIL

METHOD DrawTreeMapGraph CLASS GraphPlus

   LOCAL aPositions, nHead, cCurHeader, nCount
   LOCAL nRow, nCol, nWidth, nHeight
   LOCAL nFound
   LOCAL aDegrees := {}
   LOCAL aCumulative := {}
   LOCAL j, i, k
   LOCAL aLabelCumulative := {}
   LOCAL aColor
   LOCAL aLevelValue, aLevelHeadValue
   LOCAL nStart, cLabel
   ::nLevels := hmg_len( ::aData[ 1 ] ) - 1
   // convert data
   ::FillTreeMapData()
   ::InitColors()
   IF ::lShowLegends
      ::DrawLegends()
   ENDIF
   ::nLeft += 25
   ::nRight -= 25
   ::nBottom -= 25
   ::nTop += 25
   IF hmg_len( ::aData ) == 0
      RETURN NIL
   ENDIF
   aPositions := { ::nTop, ::nLeft, ::nRight - ::nLeft, ::nBottom - ::nTop }
   FOR j := 1 TO hmg_len( ::aLevelValues )
      ASize( ::aTreeMapData, 0 )
      ASize( aDegrees, 0 )
      ASize( aCumulative, 0 )
      ASize( aLabelCumulative, 0 )
      ASize( ::aTreeMapRect, 0 )
      IF j > 1 // sub level
         nHead := 1
         aLevelHeadValue := ::aLevelValues[ j - 1, nHead ]
         cCurHeader := aLevelHeadValue[ 3 ]
         nStart := 1
         nCount := 0
         aPositions := aLevelHeadValue[ 5 ]
         aColor := ::aColors[ ::aLevelValues[ j, 1 ][ 1 ] ]
         FOR i := 1 TO hmg_len( ::aLevelValues[ j ] )
            aLevelValue := ::aLevelValues[ j, i ]
            IF aLevelValue[ 4 ] == cCurHeader
               AAdd( ::aTreeMapData, aLevelValue[ 2 ] )
               AAdd( ::aTreeMapRect, { 0, 0, 0, 0 } )
               nCount++
            ELSE // same level another header
               // drawtreemap now
               ::StripTreemap( aPositions[ 1 ], aPositions[ 2 ], aPositions[ 3 ], aPositions[ 4 ] )
               FOR k := nStart TO nStart + nCount - 1
                  AAdd( ::aLevelValues[ j, k ], AClone( ::aTreeMapRect[ k - nStart + 1 ] ) )
               NEXT k
               FOR k := 1 TO hmg_len( ::aTreeMapRect )
                  nRow := ::aTreeMapRect[ k, 1 ]
                  nCol := ::aTreeMapRect[ k, 2 ]
                  nWidth := ::aTreeMapRect[ k, 3 ]
                  nHeight := ::aTreeMapRect[ k, 4 ]
                  BT_DrawFillRectangle ( ::hDC, nRow, nCol, nWidth, nHeight, aColor, ::aBackColor, 2 )
                  // show data label if required
                  IF j == hmg_len( ::aLevelValues ) // only in last level
                     IF ::lDataLabels // show labels
                        cLabel := ::aLevelValues[ j, nStart + k - 1 ][ 3 ]
                        nFound := hb_UAt( ': ', cLabel )
                        IF nFound > 0
                           cLabel := hb_USubStr( cLabel, nFound + 2 )
                        ENDIF
                        BT_DrawTextEx ( ::hDC, nRow + 2, nCol + 2, nWidth - 4, nHeight, AllTrim( cLabel ), ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], ::aTextColor, aColor, if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ) )
                     ENDIF
                  ENDIF
               NEXT k
               nStart := nStart + nCount
               nCount := 0
               nHead++
               ASize( ::aTreeMapData, 0 )
               ASize( aDegrees, 0 )
               ASize( aCumulative, 0 )
               ASize( aLabelCumulative, 0 )
               ASize( ::aTreeMapRect, 0 )
               IF nHead <= hmg_len( ::aLevelValues[ j - 1 ] )
                  aLevelHeadValue := ::aLevelValues[ j - 1, nHead ]
                  cCurHeader := aLevelHeadValue[ 3 ]
                  aPositions := aLevelHeadValue[ 5 ]
               ENDIF
               AAdd( ::aTreeMapData, aLevelValue[ 2 ] )
               AAdd( ::aTreeMapRect, { 0, 0, 0, 0 } )
               nCount++
               aColor := ::aColors[ ::aLevelValues[ j, i ][ 1 ] ]
            ENDIF
         NEXT i
         // for the last one
         ::StripTreemap( aPositions[ 1 ], aPositions[ 2 ], aPositions[ 3 ], aPositions[ 4 ] )
         FOR k := nStart TO nStart + nCount - 1
            AAdd( ::aLevelValues[ j, k ], AClone( ::aTreeMapRect[ k - nStart + 1 ] ) )
         NEXT k
         FOR k := 1 TO hmg_len( ::aTreeMapRect )
            nRow := ::aTreeMapRect[ k, 1 ]
            nCol := ::aTreeMapRect[ k, 2 ]
            nWidth := ::aTreeMapRect[ k, 3 ]
            nHeight := ::aTreeMapRect[ k, 4 ]
            BT_DrawFillRectangle ( ::hDC, nRow, nCol, nWidth, nHeight, aColor, ::aBackColor, 2 )
            IF j == hmg_len( ::aLevelValues ) // only in last level
               IF ::lDataLabels // show labels
                  cLabel := ::aLevelValues[ j, nStart + k - 1 ][ 3 ]
                  nFound := hb_UAt( ': ', cLabel )
                  IF nFound > 0
                     cLabel := hb_USubStr( cLabel, nFound + 2 )
                  ENDIF
                  BT_DrawTextEx ( ::hDC, nRow + 2, nCol + 2, nWidth - 4, nHeight, AllTrim( cLabel ), ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], ::aTextColor, aColor, if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ) )
               ENDIF
            ENDIF
         NEXT k
      ELSE
         FOR i := 1 TO hmg_len( ::aLevelValues[ j ] )
            aLevelValue := ::aLevelValues[ j, i ]
            AAdd( ::aTreeMapData, aLevelValue[ 2 ] )
            AAdd( ::aTreeMapRect, { 0, 0, 0, 0 } )
         NEXT i
         ::StripTreemap( aPositions[ 1 ], aPositions[ 2 ], aPositions[ 3 ], aPositions[ 4 ] )
         FOR i := 1 TO hmg_len( ::aLevelValues[ j ] )
            AAdd( ::aLevelValues[ j, i ], AClone( ::aTreeMapRect[ i ] ) )
         NEXT i
         FOR k := 1 TO hmg_len( ::aTreeMapRect )
            nRow := ::aTreeMapRect[ k, 1 ]
            nCol := ::aTreeMapRect[ k, 2 ]
            nWidth := ::aTreeMapRect[ k, 3 ]
            nHeight := ::aTreeMapRect[ k, 4 ]
            BT_DrawFillRectangle ( ::hDC, nRow, nCol, nWidth, nHeight, ::aColors[ ::aLevelValues[ j, k ][ 1 ] ], ::aBackColor, 2 )
            IF j == hmg_len( ::aLevelValues ) // if only one level
               IF ::lDataLabels // show labels
                  cLabel := ::aLevelValues[ j, k ][ 3 ]
                  nFound := hb_UAt( ': ', cLabel )
                  IF nFound > 0
                     cLabel := hb_USubStr( cLabel, nFound + 2 )
                  ENDIF
                  BT_DrawTextEx ( ::hDC, nRow + 2, nCol + 2, nWidth - 4, nHeight, AllTrim( cLabel ), ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], ::aTextColor, ::aColors[ ::aLevelValues[ j, k ][ 1 ] ], if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ) )
               ENDIF
            ENDIF
         NEXT k
      ENDIF
   NEXT j

RETURN NIL

METHOD DrawBarGraph CLASS GraphPlus

   LOCAL nCategories := 0
   LOCAL nRow, nCol, nData
   LOCAL nTotalBars
   LOCAL nAvailableHeight
   LOCAL nTotalBarRatio
   LOCAL nCategoryRatio

   LOCAL nBarWidth
   LOCAL nBarHeight
   LOCAL nCategoryHeight
   LOCAL nBarGap
   LOCAL i, j

   IF hmg_len( ::aData ) > 0
      IF ValType( ::aData[ 1 ] ) == 'A'
         nCategories := hmg_len( ::aData[ 1 ] )
      ENDIF
   ENDIF
   IF nCategories == 0
      RETURN NIL
   ENDIF
   nTotalBars := nCategories * hmg_len( ::aData )
   nAvailableHeight := ::nBottom - ::nTop
   nTotalBarRatio := nTotalBars * 1 // 100%
   nTotalBarRatio := nTotalBarRatio + ( nTotalBarRatio - nCategories ) * ::nBarGapRatio
   nCategoryRatio := nCategories * ::nGapWidthRatio // n Times of Bar size for each category
   nTotalBarRatio := nTotalBarRatio + nCategoryRatio

   nBarHeight := nAvailableHeight / nTotalBarRatio // 100% width
   nCategoryHeight := nBarHeight * ::nGapWidthRatio
   nBarGap := nBarHeight * ::nBarGapRatio

   nRow := ::nTop + ( nCategoryHeight / 2 )
   nCol := ::nZeroPos
   FOR i := 1 TO hmg_len( ::aData[ 1 ] )
      FOR j := 1 TO hmg_len( ::aData )
         nData := ::aData[ j, i ]
         nBarWidth := Int( ( ::nRight - ::nLeft ) * ( Abs( nData ) / ::nRange ) )
         IF nData > 0
            // positive
            BT_DrawFillRectangle ( ::hDC, nRow, nCol, nBarWidth, nBarHeight, ::aColors[ j ], ::aColors[ j ], 1 )
            IF ::lDataLabels
               BT_DrawText ( ::hDC, nRow + ( nBarHeight / 2 ), nCol + nBarWidth - 5, AllTrim( Transform( nData, ::cPicture ) ), ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], ::aTextColor, ::aColors[ j ], if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ), BT_TEXT_BASELINE + BT_TEXT_RIGHT )
            ENDIF
         ELSE
            // negative
            BT_DrawFillRectangle ( ::hDC, nRow, nCol - nBarWidth, nBarWidth, nBarHeight, ::aColors[ j ], ::aColors[ j ], 1 )
            IF ::lDataLabels
               BT_DrawText ( ::hDC, nRow + ( nBarHeight / 2 ), nCol - nBarWidth + 5, if( nData < 0, '-', '' ) + AllTrim( Transform( Abs( nData ), ::cPicture ) ), ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], ::aTextColor, ::aColors[ j ], if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ), BT_TEXT_BASELINE + BT_TEXT_LEFT )
            ENDIF
         ENDIF
         nRow := nRow + nBarHeight
         IF j < hmg_len( ::aData )
            nRow := nRow + nBarGap
         ENDIF
      NEXT j
      nRow := nRow + nCategoryHeight
   NEXT i

RETURN NIL

METHOD DrawFunnelGraph CLASS GraphPlus

   LOCAL nCategories
   LOCAL nRow, nData
   LOCAL nTotalBars
   LOCAL nAvailableHeight
   LOCAL nTotalBarRatio
   LOCAL nCategoryRatio

   LOCAL nBarWidth
   LOCAL nBarHeight
   LOCAL nCategoryHeight
   LOCAL aFunnelData := AClone( ::aData[ 1 ] )
   LOCAL nMax, aSize, nWidth, nFromCol, i
   nCategories := hmg_len( aFunnelData )
   // find max value
   nMax := 0
   FOR i := 1 TO HMG_LEN( aFunnelData )
      nMax := Max( aFunnelData[ i ], nMax )
   NEXT i
   IF nMax <= 0
      msgstop( 'Funnel can not be drawn for negative or zero values!' )
      RETURN NIL
   ENDIF
   // title height
   IF hmg_len( ::cTitle ) > 0
      aSize := BT_DrawTextSize ( ::hDC, ::cTitle, ::aTitleFont[ 1 ], ::aTitleFont[ 2 ], if( ::aTitleFont[ 3 ], BT_TEXT_BOLD, nil ) )
      ::nTop := ::nTop + aSize[ 2 ]
   ENDIF
   IF ::lShowCategories
      ::DrawCategories()
   ENDIF
   IF hmg_len( ::cPicture ) == 0
      ::cPicture := Replicate( '9', hmg_len( AllTrim( Str( nMax ) ) ) )
   ENDIF

   ::nRight := ::nRight - 10
   ::nLeft := ::nLeft + 5
   ::nTop := ::nTop + 10
   nTotalBars := hmg_len( aFunnelData )
   nAvailableHeight := ::nBottom - ::nTop
   nTotalBarRatio := nTotalBars * 1 // 100%
   nCategoryRatio := nCategories * ::nGapWidthRatio // n Times of Bar size for each category
   nTotalBarRatio := nTotalBarRatio + nCategoryRatio

   nBarHeight := nAvailableHeight / nTotalBarRatio // 100% width
   nCategoryHeight := nBarHeight * ::nGapWidthRatio
   // draw left vertical line
   BT_DrawLine ( ::hDC, ::nTop, ::nLeft, ::nBottom, ::nLeft, ::aGridLineColor, ::nLineWidth )
   nWidth := ( ::nRight - ::nLeft ) - 10
   nRow := ::nTop
   FOR i := 1 TO hmg_len( aFunnelData )
      nData := aFunnelData[ i ]
      nBarWidth := nWidth * ( nData / nMax )
      nFromCol := ::nLeft + 5 + ( nWidth - nBarWidth ) / 2
      IF nData > 0 // ( only for non negative )
         BT_DrawFillRectangle ( ::hDC, nRow, nFromCol, nBarWidth, nBarHeight, ::aColors[ 1 ], ::aColors[ 1 ], 1 )
         BT_DrawText ( ::hDC, ( nRow + nBarHeight / 2 ), ::nLeft + 5 + ( nWidth / 2 ), AllTrim( Transform( aFunnelData[ i ], ::cPicture ) ), ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], ::aBackColor, ::aColors[ 1 ], if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ), BT_TEXT_BASELINE + BT_TEXT_CENTER )
      ENDIF
      nRow := nRow + nBarHeight + nCategoryHeight
   NEXT i
   // title
   IF hmg_len( ::cTitle ) > 0
      BT_DrawText ( ::hDC, 5, ::nLeft + 5 + ( nWidth / 2 ), ::cTitle, ::aTitleFont[ 1 ], ::aTitleFont[ 2 ], ::aTitleColor, ::aBackColor, if( ::aTitleFont[ 3 ], BT_TEXT_BOLD, nil ), BT_TEXT_CENTER )
   ENDIF

RETURN NIL

METHOD DrawStackedColumnGraph CLASS GraphPlus

   LOCAL nCategories := 0
   LOCAL nRow, nCol, nData
   LOCAL nTotalBars
   LOCAL nAvailableWidth
   LOCAL nTotalBarRatio
   LOCAL nCategoryRatio

   LOCAL nBarWidth
   LOCAL nBarHeight
   LOCAL nCategoryWidth
   LOCAL nNegativeRow, nPositiveRow
   LOCAL i, j

   IF hmg_len( ::aData ) > 0
      IF ValType( ::aData[ 1 ] ) == 'A'
         nCategories := hmg_len( ::aData[ 1 ] )
      ENDIF
   ENDIF
   IF nCategories == 0
      RETURN NIL
   ENDIF
   nTotalBars := nCategories
   nAvailableWidth := ::nRight - ::nLeft
   nTotalBarRatio := nTotalBars * 1 // 100%
   nCategoryRatio := nCategories * ::nGapWidthRatio // n Times of Bar size for each category
   nTotalBarRatio := nTotalBarRatio + nCategoryRatio

   nBarWidth := nAvailableWidth / nTotalBarRatio // 100% width
   nCategoryWidth := nBarWidth * ::nGapWidthRatio

   nCol := ::nLeft + ( nCategoryWidth / 2 )
   nRow := ::nZeroPos
   FOR i := 1 TO hmg_len( ::aData[ 1 ] )
      nPositiveRow := nRow
      nNegativeRow := nRow
      FOR j := 1 TO hmg_len( ::aData )
         nData := ::aData[ j, i ]
         nBarHeight := Int( ( ::nBottom - ::nTop ) * ( Abs( nData ) / ::nRange ) )
         IF nData > 0
            // positive
            BT_DrawFillRectangle ( ::hDC, nPositiveRow - nBarHeight, nCol, nBarWidth, nBarHeight, ::aColors[ j ], ::aColors[ j ], 1 )
            IF ::lDataLabels
               BT_DrawText ( ::hDC, nPositiveRow - nBarHeight + 5, nCol + ( nBarWidth / 2 ), AllTrim( Transform( nData, ::cPicture ) ), ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], ::aTextColor, ::aColors[ j ], if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ), BT_TEXT_CENTER )
            ENDIF
            nPositiveRow := nPositiveRow - nBarHeight
         ELSE
            // negative
            BT_DrawFillRectangle ( ::hDC, nNegativeRow, nCol, nBarWidth, nBarHeight, ::aColors[ j ], ::aColors[ j ], 1 )
            IF ::lDataLabels
               BT_DrawText ( ::hDC, nNegativeRow + nBarHeight - 5, nCol + ( nBarWidth / 2 ), if( nData < 0, '-', '' ) + AllTrim( Transform( Abs( nData ), ::cPicture ) ), ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], ::aTextColor, ::aColors[ j ], if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ), BT_TEXT_BOTTOM + BT_TEXT_CENTER )
            ENDIF
            nNegativeRow := nNegativeRow + nBarHeight
         ENDIF
      NEXT j
      nCol := nCol + nBarWidth + nCategoryWidth
   NEXT i

RETURN NIL

METHOD DrawStackedBarGraph CLASS GraphPlus

   LOCAL nCategories := 0
   LOCAL nRow, nCol, nData
   LOCAL nTotalBars
   LOCAL nAvailableHeight
   LOCAL nTotalBarRatio
   LOCAL nCategoryRatio

   LOCAL nBarWidth
   LOCAL nBarHeight
   LOCAL nCategoryHeight
   LOCAL nNegativeCol, nPositiveCol
   LOCAL i, j

   IF hmg_len( ::aData ) > 0
      IF ValType( ::aData[ 1 ] ) == 'A'
         nCategories := hmg_len( ::aData[ 1 ] )
      ENDIF
   ENDIF
   IF nCategories == 0
      RETURN NIL
   ENDIF
   nTotalBars := nCategories
   nAvailableHeight := ::nBottom - ::nTop
   nTotalBarRatio := nTotalBars * 1 // 100%
   nCategoryRatio := nCategories * ::nGapWidthRatio // n Times of Bar size for each category
   nTotalBarRatio := nTotalBarRatio + nCategoryRatio

   nBarHeight := nAvailableHeight / nTotalBarRatio // 100% width
   nCategoryHeight := nBarHeight * ::nGapWidthRatio

   nRow := ::nTop + ( nCategoryHeight / 2 )
   nCol := ::nZeroPos
   FOR i := 1 TO hmg_len( ::aData[ 1 ] )
      nPositiveCol := nCol
      nNegativeCol := nCol
      FOR j := 1 TO hmg_len( ::aData )
         nData := ::aData[ j, i ]
         nBarWidth := Int( ( ::nRight - ::nLeft ) * ( Abs( nData ) / ::nRange ) )
         IF nData > 0
            // positive
            BT_DrawFillRectangle ( ::hDC, nRow, nPositiveCol, nBarWidth, nBarHeight, ::aColors[ j ], ::aColors[ j ], 1 )
            IF ::lDataLabels
               BT_DrawText ( ::hDC, nRow + ( nBarHeight / 2 ), nPositiveCol + nBarWidth - 5, AllTrim( Transform( nData, ::cPicture ) ), ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], ::aTextColor, ::aColors[ j ], if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ), BT_TEXT_BASELINE + BT_TEXT_RIGHT )
            ENDIF
            nPositiveCol := nPositiveCol + nBarWidth
         ELSE
            // negative
            BT_DrawFillRectangle ( ::hDC, nRow, nNegativeCol - nBarWidth, nBarWidth, nBarHeight, ::aColors[ j ], ::aColors[ j ], 1 )
            IF ::lDataLabels
               BT_DrawText ( ::hDC, nRow + ( nBarHeight / 2 ), nNegativeCol - nBarWidth + 5, if( nData < 0, '-', '' ) + AllTrim( Transform( Abs( nData ), ::cPicture ) ), ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], ::aTextColor, ::aColors[ j ], if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ), BT_TEXT_BASELINE + BT_TEXT_LEFT )
            ENDIF
            nNegativeCol := nNegativeCol - nBarWidth
         ENDIF
      NEXT j
      nRow := nRow + nBarHeight + nCategoryHeight
   NEXT i

RETURN NIL

METHOD DrawAreaGraph CLASS GraphPlus

   LOCAL nCategories := 0
   LOCAL nRow, nCol, nData, nFirstCol, nPrevRow
   LOCAL nAvailableWidth
   LOCAL nTotalBarRatio

   LOCAL nBarWidth
   LOCAL nBarHeight
   LOCAL nCategoryWidth
   LOCAL aRows := {}
   LOCAL aCols := {}
   LOCAL i, j

   IF hmg_len( ::aData ) > 0
      IF ValType( ::aData[ 1 ] ) == 'A'
         nCategories := hmg_len( ::aData[ 1 ] )
      ENDIF
   ENDIF
   IF nCategories == 0
      RETURN NIL
   ENDIF
   nAvailableWidth := ::nRight - ::nLeft
   nTotalBarRatio := nCategories - 1 // equal space between categories

   nBarWidth := nAvailableWidth / nTotalBarRatio // 100% width
   nCategoryWidth := nBarWidth
   nFirstCol := ::nLeft
   nRow := ::nZeroPos
   FOR j := 1 TO hmg_len( ::aData )
      nCol := nFirstCol
      ASize( aRows, 0 )
      ASize( aCols, 0 )
      AAdd( aRows, ::nZeroPos ) // starting point at zero
      AAdd( aCols, ::nLeft ) // starting point at left
      FOR i := 1 TO hmg_len( ::aData[ 1 ] )
         nData := ::aData[ j, i ]
         nBarHeight := Int( ( ::nBottom - ::nTop ) * ( Abs( nData ) / ::nRange ) )
         IF nData > 0
            IF ::lDataLabels
               BT_DrawText ( ::hDC, nRow - nBarHeight - 15 - ::nPointSize / 2, nCol, AllTrim( Transform( nData, ::cPicture ) ), ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], ::aTextColor, ::aBackColor, if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ), BT_TEXT_CENTER )
            ENDIF
            nPrevRow := nRow - nBarHeight
         ELSE
            IF ::lDataLabels
               BT_DrawText ( ::hDC, nRow + nBarHeight + 3 + ::nPointSize / 2, nCol, if( nData < 0, '-', '' ) + AllTrim( Transform( Abs( nData ), ::cPicture ) ), ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], ::aTextColor, ::aBackColor, if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ), BT_TEXT_CENTER )
            ENDIF
            nPrevRow := nRow + nBarHeight
         ENDIF
         AAdd( aCols, nCol )
         AAdd( aRows, nPrevRow )
         nCol := nCol + nCategoryWidth
      NEXT i
      AAdd( aCols, ::nRight ) // end point in the right
      AAdd( aRows, ::nZeroPos ) // end point zero pos
      BT_DrawPolygon ( ::hDC, aRows, aCols, ::aColors[ j ], 1, ::aColors[ j ] )
   NEXT j

   // data labels...
   IF ::lDataLabels
      nRow := ::nZeroPos
      FOR j := 1 TO hmg_len( ::aData )
         nCol := nFirstCol
         FOR i := 1 TO hmg_len( ::aData[ 1 ] )
            nData := ::aData[ j, i ]
            nBarHeight := Int( ( ::nBottom - ::nTop ) * ( Abs( nData ) / ::nRange ) )
            IF nData > 0 // positive
               BT_DrawText ( ::hDC, nRow - nBarHeight - 15 - ::nPointSize / 2, nCol, AllTrim( Transform( nData, ::cPicture ) ), ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], ::aTextColor, ::aBackColor, if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ), BT_TEXT_CENTER )
            ELSE // negative
               BT_DrawText ( ::hDC, nRow + nBarHeight + 3 + ::nPointSize / 2, nCol, if( nData < 0, '-', '' ) + AllTrim( Transform( Abs( nData ), ::cPicture ) ), ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], ::aTextColor, ::aBackColor, if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ), BT_TEXT_CENTER )
            ENDIF
            nCol := nCol + nCategoryWidth
         NEXT i
      NEXT j
   ENDIF

RETURN NIL


METHOD DrawScatterXYGraph CLASS GraphPlus

   LOCAL aSize
   LOCAL nRow, nCol
   LOCAL nAvailableWidth, nScaleItemWidth
   LOCAL aScaleX := {}, aScaleY := {}
   LOCAL nMaxY, nMaxX, nMinY, nMinX, nUpperX, nUpperY, nLowerX, nLowerY, nRangeX, nRangeY, ;
      nLowerUpperRatioX, nLowerUpperRatioY, nLowerDivisionX, nLowerDivisionY, nUpperDivisionX, nUpperDivisionY, ;
      nDivisionX, nDivisionY, nDistanceX, nDistanceY, cDivisionX, cDivisionY, nDecimalX, nDecimalY, cDecimalX, cDecimalY, ;
      nDecimalLengthX, nDecimalLengthY, nZeroX, nZeroY, nZeroRowPos, nZeroColPos, nMaxWidthY, nScaleItems, ;
      nAvailableHeight, nTotalBarRatio, nScaleItemHeight, nHeight, nWidth, nDataX, nDataY, nMaxHeightX, nPrevRow, nPrevCol
   LOCAL i

   // draw title
   IF hmg_len( ::cTitle ) > 0
      aSize := BT_DrawTextSize ( ::hDC, ::cTitle, ::aTitleFont[ 1 ], ::aTitleFont[ 2 ], if( ::aTitleFont[ 3 ], BT_TEXT_BOLD, nil ) )
      BT_DrawText ( ::hDC, ::nTop / 2, ::nWidth / 2, ::cTitle, ::aTitleFont[ 1 ], ::aTitleFont[ 2 ], ::aTitleColor, ::aBackColor, if( ::aTitleFont[ 3 ], BT_TEXT_BOLD, nil ), BT_TEXT_CENTER )
      ::nTop := ::nTop + aSize[ 2 ]
   ENDIF
   ::DrawXYTitles() // draw x/y titles
   IF hmg_len( ::aData ) < 2
      msginfo( 'Not Enough data for XY!' )
      RETURN NIL
   ENDIF
   IF hmg_len( ::aData[ 1 ] ) <> hmg_len( ::aData[ 2 ] )
      msginfo( 'X and Y axis data should be in equal numbers!' )
      RETURN NIL
   ENDIF
   // draw graph border
   // BT_DrawRectangle (::hDC, ::nTop, ::nLeft, ::nRight - ::nLeft , ::nBottom - ::nTop, ::aTextColor, 1 )
   // find max and min value for both x and y
   nMaxX := 0
   nMaxY := 0
   nMinX := 0
   nMinY := 0
   FOR i := 1 TO hmg_len( ::aData[ 1 ] )
      nMaxX := Max( ::aData[ 1, i ], nMaxX )
      nMinX := Min( ::aData[ 1, i ], nMinX )
   NEXT i
   FOR i := 1 TO hmg_len( ::aData[ 2 ] )
      nMaxY := Max( ::aData[ 2, i ], nMaxY )
      nMinY := Min( ::aData[ 2, i ], nMinY )
   NEXT i
   IF nMaxX > 0
      nUpperX := nMaxX
   ELSE
      nUpperX := 0
   ENDIF
   IF nMinX < 0
      nLowerX := nMinX
   ELSE
      nLowerX := 0
   ENDIF
   IF nMaxY > 0
      nUpperY := nMaxY
   ELSE
      nUpperY := 0
   ENDIF
   IF nMinY < 0
      nLowerY := nMinY
   ELSE
      nLowerY := 0
   ENDIF
   nRangeX := nUpperX - nLowerX
   nRangeY := nUpperY - nLowerY
   IF nUpperX <> 0 .AND. nLowerX <> 0
      IF Abs( nLowerX ) < nUpperX
         nLowerUpperRatioX := Abs( nLowerX ) / nRangeX
         nLowerDivisionX := ::nVDivision * nLowerUpperRatioX
         IF nLowerDivisionX - Int( nLowerDivisionX ) > 0.0
            nLowerDivisionX := Int( nLowerDivisionX ) + 1
         ENDIF
         nUpperDivisionX := ::nVDivision - nLowerDivisionX
         nDivisionX := nUpperX / nUpperDivisionX
      ELSE
         nLowerUpperRatioX := Abs( nUpperX ) / nRangeX
         nUpperDivisionX := ::nVDivision * nLowerUpperRatioX
         IF nUpperDivisionX - Int( nUpperDivisionX ) > 0.0
            nUpperDivisionX := Int( nUpperDivisionX ) + 1
         ENDIF
         nLowerDivisionX := ::nVDivision - nUpperDivisionX
         nDivisionX := Abs( nLowerX ) / nLowerDivisionX
      ENDIF
   ELSE
      nDivisionX := nRangeX / ::nVDivision
      IF nUpperX > 0
         nUpperDivisionX := ::nVDivision
         nLowerDivisionX := 0
      ELSE
         nLowerDivisionX := ::nVDivision
         nUpperDivisionX := 0
      ENDIF
   ENDIF
   IF nUpperY <> 0 .AND. nLowerY <> 0
      IF Abs( nLowerY ) < nUpperY
         nLowerUpperRatioY := Abs( nLowerY ) / nRangeY
         nLowerDivisionY := ::nHDivision * nLowerUpperRatioY
         IF nLowerDivisionY - Int( nLowerDivisionY ) > 0.0
            nLowerDivisionY := Int( nLowerDivisionY ) + 1
         ENDIF
         nUpperDivisionY := ::nHDivision - nLowerDivisionY
         nDivisionY := nUpperY / nUpperDivisionY
      ELSE
         nLowerUpperRatioY := Abs( nUpperY ) / nRangeY
         nUpperDivisionY := ::nHDivision * nLowerUpperRatioY
         IF nUpperDivisionY - Int( nUpperDivisionY ) > 0.0
            nUpperDivisionY := Int( nUpperDivisionY ) + 1
         ENDIF
         nLowerDivisionY := ::nHDivision - nUpperDivisionY
         nDivisionY := Abs( nLowerY ) / nLowerDivisionY
      ENDIF
   ELSE
      nDivisionY := nRangeY / ::nHDivision
      IF nUpperY > 0
         nUpperDivisionY := ::nHDivision
         nLowerDivisionY := 0
      ELSE
         nLowerDivisionX := ::nHDivision
         nUpperDivisionX := 0
      ENDIF
   ENDIF
   IF Int( nDivisionX ) > 0
      cDivisionX := AllTrim( Str( Int( nDivisionX ), 14, 0 ) )
      IF hmg_len( cDivisionX ) > 1 // if more than 1 digits round to power of 10s
         IF SubStr( cDivisionX, 2, 1 ) <> '0'
            nDivisionX := ( Val( Left( cDivisionX, 2 ) ) + 1 ) * 10 ^ ( hmg_len( cDivisionX ) - 2 )
         ENDIF
      ELSE
         IF nDivisionX > Int( nDivisionX )
            nDivisionX++
         ENDIF
      ENDIF
   ELSE //
      cDivisionX := Transform( nDivisionX, '9.9999999' )
      nDecimalLengthX := 8 - hmg_len( AllTrim( Str( Val( Right( cDivisionX, 7 ) ) ) ) )
      nDecimalX := Val( Right( cDivisionX, 7 ) )
      cDecimalX := AllTrim( Str( nDecimalX ) )
      nDivisionX := ( Val( AllTrim( Left( cDecimalX, 1 ) ) ) + 1 ) / ( 10 ^ nDecimalLengthX )
   ENDIF
   IF Int( nDivisionY ) > 0
      cDivisionY := AllTrim( Str( Int( nDivisionY ), 14, 0 ) )
      IF hmg_len( cDivisionY ) > 1 // if more than 1 digits round to power of 10s
         IF SubStr( cDivisionY, 2, 1 ) <> '0'
            nDivisionY := ( Val( Left( cDivisionY, 2 ) ) + 1 ) * 10 ^ ( hmg_len( cDivisionY ) - 2 )
         ENDIF
      ELSE
         IF nDivisionY > Int( nDivisionY )
            nDivisionY++
         ENDIF
      ENDIF
   ELSE //
      cDivisionY := Transform( nDivisionY, '9.9999999' )
      nDecimalLengthY := 8 - hmg_len( AllTrim( Str( Val( Right( cDivisionY, 7 ) ) ) ) )
      nDecimalY := Val( Right( cDivisionY, 7 ) )
      cDecimalY := AllTrim( Str( nDecimalY ) )
      nDivisionY := ( Val( AllTrim( Left( cDecimalY, 1 ) ) ) + 1 ) / ( 10 ^ nDecimalLengthY )
   ENDIF
   nRangeX := ::nVDivision * nDivisionX
   nRangeY := ::nHDivision * nDivisionY
   IF hmg_len( ::cPictureX ) == 0 .AND. hmg_len( ::cPictureY ) == 0 .AND. hmg_len( ::cPicture ) > 0 // common picture defined by the user.
      ::cPictureX := ::cPicture
      ::cPictureY := ::cPicture
   ENDIF
   IF hmg_len( ::cPictureX ) == 0
      ::cPictureX := Replicate( '9', Max( hmg_len( AllTrim( Str( nUpperX ) ) ), hmg_len( AllTrim( Str( nLowerX ) ) ) ) )
   ENDIF
   IF hmg_len( ::cPictureY ) == 0
      ::cPictureY := Replicate( '9', Max( hmg_len( AllTrim( Str( nUpperX ) ) ), hmg_len( AllTrim( Str( nLowerX ) ) ) ) )
   ENDIF
   // find scales
   ASize( aScaleX, 0 )
   ASize( aScaleY, 0 )
   nDistanceX := nRangeX / ::nVDivision
   nDistanceY := nRangeY / ::nHDivision
   FOR i := nUpperDivisionX TO 1 STEP -1
      AAdd( aScaleX, AllTrim( Transform( nDistanceX * i, ::cPictureX ) ) )
   NEXT i
   AAdd( aScaleX, AllTrim( Transform( 0, ::cPictureX ) ) )
   nZeroX := hmg_len( aScaleX )
   FOR i := 1 TO nLowerDivisionX
      AAdd( aScaleX, '-' + AllTrim( Transform( nDistanceX * i, ::cPictureX ) ) )
   NEXT i
   FOR i := nUpperDivisionY TO 1 STEP -1
      AAdd( aScaleY, AllTrim( Transform( nDistanceY * i, ::cPictureY ) ) )
   NEXT i
   AAdd( aScaleY, AllTrim( Transform( 0, ::cPictureY ) ) )
   nZeroY := hmg_len( aScaleY )
   FOR i := 1 TO nLowerDivisionY
      AAdd( aScaleY, '-' + AllTrim( Transform( nDistanceY * i, ::cPictureY ) ) )
   NEXT i
   // find maxscaleitemwidthY
   nMaxWidthY := 0
   FOR i := 1 TO hmg_len( aScaleY )
      aSize := BT_DrawTextSize ( ::hDC, aScaleY[ i ], ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ) )
      nMaxWidthY := Max( nMaxWidthY, aSize[ 1 ] )
   NEXT i
   nMaxHeightX := 0
   FOR i := 1 TO hmg_len( aScaleX )
      aSize := BT_DrawTextSize ( ::hDC, aScaleX[ i ], ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ) )
      nMaxHeightX := Max( nMaxHeightX, aSize[ 1 ] )
   NEXT i

   // draw all items other than graph
   ::nTop := ::nTop + 20
   ::nBottom := ::nBottom - 20
   IF ::lShowValues
      IF nZeroX == hmg_len( aScaleX ) // X Zero at left
         ::nLeft := ::nLeft + nMaxWidthY
      ENDIF
      IF nZeroY == 1 // Y Zero at Bottom
         ::nBottom := ::nBottom - nMaxHeightX
      ENDIF
   ENDIF

   ::nRight := ::nRight - 20
   ::nLeft := ::nLeft + 5

   // find zeroRow position
   DO CASE
   CASE nZeroY == 1 // all are negative!
      nZeroRowPos := ::nTop
   CASE nZeroY == hmg_len( aScaleY ) // all are positive!
      nZeroRowPos := ::nBottom
   OTHERWISE // zero position in the middles
      nZeroRowPos := ::nTop + ( ( ::nBottom - ::nTop ) / ::nHDivision * ( nZeroY - 1 ) )
   ENDCASE
   nZeroRowPos := Int( nZeroRowPos )

   // find zeroCol position
   DO CASE
   CASE nZeroX == 1 // all are negative!
      nZeroColPos := ::nRight
   CASE nZeroX == hmg_len( aScaleX ) // all are positive!
      nZeroColPos := ::nLeft
   OTHERWISE // zero position in the middles
      nZeroColPos := ::nRight - ( ( ::nRight - ::nLeft ) / ::nVDivision * ( nZeroX - 1 ) )
   ENDCASE
   // Draw Grids
   IF ::lHGrid
      nHeight := ( ::nBottom - ::nTop ) / ::nHDivision
      nRow := ::nTop
      FOR i := 1 TO hmg_len( aScaleY )
         BT_DrawLine ( ::hDC, nRow, ::nLeft, nRow, ::nRight, ::aGridLineColor, ::nLineWidth )
         nRow := nRow + nHeight
      NEXT i
   ENDIF

   IF ::lVGrid
      nWidth := ( ::nRight - ::nLeft ) / ::nVDivision
      BT_DrawLine ( ::hDC, ::nTop, ::nLeft, ::nBottom, ::nLeft, ::aGridLineColor, ::nLineWidth )
      nCol := ::nLeft
      FOR i := 1 TO hmg_len( aScaleX )
         nCol := nCol + nWidth
         BT_DrawLine ( ::hDC, ::nTop, nCol, ::nBottom, nCol, ::aGridLineColor, ::nLineWidth )
      NEXT i
   ENDIF
   IF ::lShowValues
      // draw X Axis Values
      nCol := ::nLeft
      nScaleItems := hmg_len( aScaleX )
      nAvailableWidth := ::nRight - ::nLeft
      nTotalBarRatio := nScaleItems - 1 // equal space between categories
      nScaleItemWidth := nAvailableWidth / nTotalBarRatio // 100% width
      FOR i := hmg_len( aScaleX ) TO 1 STEP -1
         BT_DrawText ( ::hDC, nZeroRowPos + 4, nCol, aScaleX[ i ], ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], ::aTextColor, ::aBackColor, if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ), BT_TEXT_CENTER )
         nCol := nCol + nScaleItemWidth
      NEXT i
      // Draw Y Axis Values
      nScaleItems := hmg_len( aScaleY )
      nAvailableHeight := ::nBottom - ::nTop
      nTotalBarRatio := nScaleItems - 1 // equal space between scales
      nScaleItemHeight := nAvailableHeight / nTotalBarRatio
      nRow := ::nTop
      FOR i := 1 TO hmg_len( aScaleY )
         IF i <> nZeroY
            BT_DrawText ( ::hDC, nRow - 4, nZeroColPos - 5, aScaleY[ i ], ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], ::aTextColor, ::aBackColor, if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ), BT_TEXT_BASELINE + BT_TEXT_RIGHT )
         ENDIF
         nRow := nRow + nScaleItemHeight
      NEXT i
   ENDIF
   FOR i := 1 TO hmg_len( ::aData[ 1 ] )
      nDataX := ::aData[ 1, i ]
      nDataY := ::aData[ 2, i ]
      nWidth := Int( ( ::nRight - ::nLeft ) * ( Abs( nDataX ) / nRangeX ) )
      nHeight := Int( ( ::nBottom - ::nTop ) * ( Abs( nDataY ) / nRangeY ) )
      IF nDataX > 0
         nCol := nZeroColPos + nWidth - ::nPointSize / 2
      ELSE
         nCol := nZeroColPos - nWidth - ::nPointSize / 2
      ENDIF
      IF nDataY > 0
         nRow := nZeroRowPos - nHeight - ::nPointSize / 2
      ELSE
         nRow := nZeroRowPos + nHeight - ::nPointSize / 2
      ENDIF
      BT_DrawFillEllipse ( ::hDC, nRow, nCol, ::nPointSize, ::nPointSize, ::aColors[ 1 ], ::aColors[ 1 ], 1 )
      IF ::lDataLabels
         BT_DrawText ( ::hDC, nRow - 15 - ::nPointSize / 2, nCol, '(' + AllTrim( Transform( nDataX, ::cPictureX ) ) + ', ' + AllTrim( Transform( nDataY, ::cPictureY ) ) + ')', ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], ::aTextColor, ::aBackColor, if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ), BT_TEXT_CENTER )
      ENDIF
      IF ::lScatterLine
         IF i <> 1
            BT_DrawLine ( ::hDC, nPrevRow, nPrevCol, nRow + ::nPointSize / 2, nCol + ::nPointSize / 2, ::aColors[ 1 ], ::nLineWidth )
         ENDIF
      ENDIF
      nPrevRow := nRow + ::nPointSize / 2
      nPrevCol := nCol + ::nPointSize / 2
   NEXT i

RETURN NIL


METHOD Save( cFileName ) CLASS GraphPlus
   IF PCount() == 0
      cFileName := GetFile ()
   ENDIF
   cFileName := AllTrim( cFileName )
   IF hmg_len( cFileName ) > 0
      IF File( cFileName )
         IF .NOT. MsgYesNo( 'File already Exists! Do you want to overwrite?' )
            RETURN NIL
         ENDIF
      ENDIF
      BT_BitmapSaveFile( ::hBitMap, cFileName, BT_FILEFORMAT_PNG )
   ENDIF

RETURN NIL


METHOD DrawLegends CLASS GraphPlus

   LOCAL nMaxWidth := 0, i, j, nMaxHeight := 0
   LOCAL nLegends := hmg_len( ::aLegends )
   LOCAL aSize
   LOCAL nColumns, nRows
   LOCAL nRow
   LOCAL nCol
   FOR i := 1 TO nLegends
      aSize := BT_DrawTextSize ( ::hDC, ::aLegends[ i ], ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ) )
      nMaxWidth := Max( nMaxWidth, aSize[ 1 ] )
      nMaxHeight := Max( nMaxHeight, aSize[ 2 ] )
   NEXT i
   nMaxWidth := ( nMaxWidth + ::nLegendBoxSize + 10 ) // per legend
   nMaxHeight := nMaxHeight + 10 // per legend
   ::nLegendWidth := nMaxWidth

   // find columns/rows
   IF ::nLegendPos == 1 // bottom
      nColumns := Int( ( ::nWidth - 20 ) / nMaxWidth )
      IF nColumns == 0 // legend length is not fitting!
         nColumns := 1
      ENDIF
      nColumns := Min( nColumns, nLegends )
      nRows := Int( nLegends / nColumns ) + ( if( Mod( nLegends, nColumns ) > 0, 1, 0 ) )
   ELSE // right
      nColumns := 1
      nRows := nLegends
      IF nLegends * nMaxHeight > ( ::nBottom - ::nTop )
         // all the legends can not fit!
         nRows := Int( ( ::nBottom - ::nTop ) / nMaxHeight )
      ENDIF
   ENDIF
   // draw legends
   IF ::nLegendPos == 1 // bottom
      ::nBottom := ::nBottom - ( nRows * nMaxHeight )
      nRow := ::nBottom + 5
      i := 1
      j := 1
      nCol := ( ::nWidth - nColumns * ::nLegendWidth ) / 2
      DO WHILE i <= nLegends
         IF j > nColumns
            j := 1
            nRow := nRow + nMaxHeight
            nCol := ( ::nWidth - nColumns * ::nLegendWidth ) / 2
         ENDIF
         BT_DrawFillRectangle ( ::hDC, nRow, nCol, ::nLegendBoxSize, Max( nMaxHeight - 10, ::nLegendBoxSize ), ::aColors[ i ], ::aColors[ i ], 1 )
         BT_DrawText ( ::hDC, nRow, nCol + ::nLegendBoxSize + 5, ::aLegends[ i ], ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], ::aTextColor, ::aBackColor, if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ) )
         nCol := nCol + nMaxWidth
         j++
         i++
      ENDDO
   ELSE // right
      ::nRight := ::nRight - nMaxWidth
      nRow := ::nTop + 5
      nCol := ::nRight + 5
      FOR i := 1 TO nRows
         BT_DrawFillRectangle ( ::hDC, nRow, nCol, ::nLegendBoxSize, Max( nMaxHeight - 10, ::nLegendBoxSize ), ::aColors[ i ], ::aColors[ i ], 1 )
         BT_DrawText ( ::hDC, nRow, nCol + ::nLegendBoxSize + 5, ::aLegends[ i ], ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], ::aTextColor, ::aBackColor, if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ) )
         nRow := nRow + nMaxHeight
      NEXT i
   ENDIF

RETURN NIL

METHOD DrawSunBurstLegends( aLegends, aColors ) CLASS GraphPlus

   LOCAL nMaxWidth := 0, i, j, nMaxHeight := 0
   LOCAL nLegends := hmg_len( aLegends )
   LOCAL aSize
   LOCAL nColumns, nRows
   LOCAL nRow
   LOCAL nCol
   FOR i := 1 TO nLegends
      aSize := BT_DrawTextSize ( ::hDC, aLegends[ i ], ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ) )
      nMaxWidth := Max( nMaxWidth, aSize[ 1 ] )
      nMaxHeight := Max( nMaxHeight, aSize[ 2 ] )
   NEXT i
   nMaxWidth := ( nMaxWidth + ::nLegendBoxSize + 10 ) // per legend
   nMaxHeight := nMaxHeight + 10 // per legend
   ::nLegendWidth := nMaxWidth

   // find columns/rows
   IF ::nLegendPos == 1 // bottom
      nColumns := Int( ( ::nWidth - 20 ) / nMaxWidth )
      IF nColumns == 0 // legend length is not fitting!
         nColumns := 1
      ENDIF
      nColumns := Min( nColumns, nLegends )
      nRows := Int( nLegends / nColumns ) + ( if( Mod( nLegends, nColumns ) > 0, 1, 0 ) )
   ELSE // right
      nColumns := 1
      nRows := nLegends
      IF nLegends * nMaxHeight > ( ::nBottom - ::nTop )
         // all the legends can not fit!
         nRows := Int( ( ::nBottom - ::nTop ) / nMaxHeight )
      ENDIF
   ENDIF
   // draw legends
   IF ::nLegendPos == 1 // bottom
      ::nBottom := ::nBottom - ( nRows * nMaxHeight )
      nRow := ::nBottom + 5
      i := 1
      j := 1
      nCol := ( ::nWidth - nColumns * ::nLegendWidth ) / 2
      DO WHILE i <= nLegends
         IF j > nColumns
            j := 1
            nRow := nRow + nMaxHeight
            nCol := ( ::nWidth - nColumns * ::nLegendWidth ) / 2
         ENDIF
         BT_DrawFillRectangle ( ::hDC, nRow, nCol, ::nLegendBoxSize, Max( nMaxHeight - 10, ::nLegendBoxSize ), aColors[ i ], aColors[ i ], 1 )
         BT_DrawText ( ::hDC, nRow, nCol + ::nLegendBoxSize + 5, aLegends[ i ], ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], ::aTextColor, ::aBackColor, if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ) )
         nCol := nCol + nMaxWidth
         j++
         i++
      ENDDO
   ELSE // right
      ::nRight := ::nRight - nMaxWidth
      nRow := ::nTop + 5
      nCol := ::nRight + 5
      FOR i := 1 TO nRows
         BT_DrawFillRectangle ( ::hDC, nRow, nCol, ::nLegendBoxSize, Max( nMaxHeight - 10, ::nLegendBoxSize ), aColors[ i ], aColors[ i ], 1 )
         BT_DrawText ( ::hDC, nRow, nCol + ::nLegendBoxSize + 5, aLegends[ i ], ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], ::aTextColor, ::aBackColor, if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ) )
         nRow := nRow + nMaxHeight
      NEXT i
   ENDIF

RETURN NIL


METHOD DrawCategories CLASS GraphPlus

   LOCAL nCategories
   LOCAL nRow, nCol
   LOCAL nAvailableWidth, nAvailableHeight
   LOCAL nTotalBarRatio
   LOCAL aSize, nMaxWidth, nMaxHeight, nCategoryHeight, i

   LOCAL nBarHeight
   LOCAL nCategoryWidth
   nCategories := hmg_len( ::aCategories )
   IF nCategories == 0
      RETURN NIL
   ENDIF
   IF .NOT. ::lGraphRotated // columns and like
      nAvailableWidth := ::nRight - ( ::nLeft + ::nMaxScaleWidth + 5 )
      IF ::nGraphType <> GT_AREA // area charts start from left and ends at right.
         nTotalBarRatio := nCategories // equal space between categories
         nCategoryWidth := nAvailableWidth / nTotalBarRatio // 100% width
         nCol := ::nLeft + ::nMaxScaleWidth + 5 + ( nCategoryWidth / 2 )
      ELSE
         nTotalBarRatio := nCategories - 1// equal space between categories
         nCategoryWidth := nAvailableWidth / nTotalBarRatio // 100% width
         nCol := ::nLeft + ::nMaxScaleWidth + 5
      ENDIF
      nMaxHeight := 0

      FOR i := 1 TO hmg_len( ::aCategories )
         aSize := BT_DrawTextSize ( ::hDC, ::aCategories[ i ], ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ) )
         nMaxHeight := Max( nMaxHeight, aSize[ 2 ] )
      NEXT i
      FOR i := 1 TO hmg_len( ::aCategories )
         BT_DrawText ( ::hDC, ::nBottom, nCol, ::aCategories[ i ], ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], ::aTextColor, ::aBackColor, if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ), BT_TEXT_CENTER )
         nCol := nCol + nCategoryWidth
      NEXT i
      ::nBottom := ::nBottom - nMaxHeight
   ELSE
      nAvailableHeight := ::nBottom - ::nTop
      nTotalBarRatio := nCategories // equal space between categories

      nBarHeight := nAvailableHeight / nTotalBarRatio // 100% width
      nCategoryHeight := nBarHeight

      nRow := ::nTop + ( nCategoryHeight / 2 )
      nMaxWidth := 0

      FOR i := 1 TO hmg_len( ::aCategories )
         aSize := BT_DrawTextSize ( ::hDC, ::aCategories[ i ], ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ) )
         nMaxWidth := Max( nMaxWidth, aSize[ 1 ] )
      NEXT i
      nCol := ::nLeft + nMaxWidth
      FOR i := 1 TO hmg_len( ::aCategories )
         BT_DrawText ( ::hDC, nRow + 2, nCol, ::aCategories[ i ], ::aLegendFont[ 1 ], ::aLegendFont[ 2 ], ::aTextColor, ::aBackColor, if( ::aLegendFont[ 3 ], BT_TEXT_BOLD, nil ), BT_TEXT_BASELINE + BT_TEXT_RIGHT )
         nRow := nRow + nCategoryHeight
      NEXT i
      ::nLeft := ::nLeft + nMaxWidth
      ::nLeft := ::nLeft + 5
   ENDIF

RETURN NIL

METHOD DrawXYTitles CLASS GraphPlus

   LOCAL aSize
   LOCAL cXTitle := ::cXTitle
   LOCAL cYTitle := ::cYTitle
   IF ::lGraphRotated // bar graph -> Swap x and y
      cYTitle := ::cXTitle
      cXTitle := ::cYTitle
   ENDIF
   IF hmg_len( cXTitle ) > 0
      aSize := BT_DrawTextSize ( ::hDC, cXTitle, ::aTitleFont[ 1 ], ::aTitleFont[ 2 ] - 2, if( ::aTitleFont[ 3 ], BT_TEXT_BOLD, nil ) )
      BT_DrawText ( ::hDC, ::nBottom - aSize[ 2 ], ( ::nRight - ::nLeft ) / 2, cXTitle, ::aTitleFont[ 1 ], ::aTitleFont[ 2 ] - 2, ::aTitleColor, ::aBackColor, if( ::aTitleFont[ 3 ], BT_TEXT_BOLD, nil ), BT_TEXT_CENTER )
      ::nBottom := ::nBottom - aSize[ 2 ]
   ENDIF
   IF hmg_len( cYTitle ) > 0
      aSize := BT_DrawTextSize ( ::hDC, cYTitle, ::aTitleFont[ 1 ], ::aTitleFont[ 2 ] - 2, if( ::aTitleFont[ 3 ], BT_TEXT_BOLD, nil ) )
      BT_DrawText ( ::hDC, ( ::nBottom - ::nTop ) / 2, ::nLeft, cYTitle, ::aTitleFont[ 1 ], ::aTitleFont[ 2 ] - 2, ::aTitleColor, ::aBackColor, if( ::aTitleFont[ 3 ], BT_TEXT_BOLD, nil ), BT_TEXT_CENTER, BT_TEXT_VERTICAL_ASCENDANT )
      ::nLeft := ::nLeft + aSize[ 2 ]
   ENDIF

RETURN NIL

METHOD InitColors CLASS GraphPlus

   LOCAL i, j, nLegends, nFactor, nCurFactor := 5
   LOCAL aHSL
   LOCAL aColorTheme
   nLegends := hmg_len( ::aLegends )
   IF hmg_len( ::aColors ) = 0 .OR. hmg_len( ::aColors ) < nLegends
      ASize( ::aColors, 0 )
      aColorTheme := ::aColorThemes[ ::nColorTheme ]
      IF nLegends <= hmg_len( aColorTheme )
         FOR i := 1 TO nLegends
            AAdd( ::aColors, aColorTheme[ i ] )
         NEXT i
      ELSE
         FOR i := 1 TO hmg_len( aColorTheme )
            AAdd( ::aColors, aColorTheme[ i ] )
         NEXT i
         i := hmg_len( aColorTheme ) + 1
         j := 1
         nFactor := 5
         DO WHILE i <= hmg_len( ::aLegends )
            IF j > hmg_len( aColorTheme )
               j := 1
               nCurFactor *= nFactor
            ENDIF
            aHSL := GP_RGB2HSL( aColorTheme[ j ] )
            aHSL[ 3 ] := aHSL[ 3 ] + iif( ::lLighter, nCurFactor, -nCurFactor )
            AAdd( ::aColors, GP_HSL2RGB( aHSL ) )
            j++
            i++
         ENDDO
      ENDIF
   ENDIF

RETURN NIL

METHOD SwitchRowsToColumns

   LOCAL aTemp := {}, aLine := {}
   LOCAL cTemp
   LOCAL i
   LOCAL j
   // transpose data
   FOR i := 1 TO hmg_len( ::aData[ 1 ] )
      ASize( aLine, 0 )
      FOR j := 1 TO hmg_len( ::aData )
         AAdd( aLine, ::aData[ j, i ] )
      NEXT j
      AAdd( aTemp, AClone( aLine ) )
   NEXT i
   ASize( ::aData, 0 )
   ::aData := AClone( aTemp )
   // switch legends and categories
   aTemp := AClone( ::aCategories )
   ::aCategories := AClone( ::aLegends )
   ::aLegends := AClone( aTemp )
   // switch xy titles
   cTemp := ::cXTitle
   ::cXTitle := ::cYTitle
   ::cYTitle := cTemp

RETURN NIL

METHOD FindPieCoordinates( nChartSize ) CLASS GraphPlus

   LOCAL aPoints := Array( 14 )
   LOCAL fromcol, tocol, torow, fromrow
   AFill( aPoints, 0 )
   fromcol := ::nLeft + ( ( ::nRight - ::nLeft ) - nChartSize ) / 2
   tocol := ::nRight - ( ( ::nRight - ::nLeft ) - nChartSize ) / 2
   torow := ::nBottom - ( ( ::nBottom - ::nTop ) - nChartSize ) / 2
   fromrow := ::nTop + ( ( ::nBottom - ::nTop ) - nChartSize ) / 2
   aPoints[ 1 ] := fromrow // topleftrow
   aPoints[ 2 ] := fromcol // topleftcol
   aPoints[ 3 ] := fromrow // toprightrow
   aPoints[ 4 ] := tocol // toprightcol
   aPoints[ 5 ] := torow // bottomrightrow
   aPoints[ 6 ] := tocol // bottomrightcol
   aPoints[ 7 ] := torow // bottomleftrow
   aPoints[ 8 ] := fromcol // bottomleftcol
   aPoints[ 9 ] := fromcol + Int( tocol - fromcol ) / 2 // middletopcol
   aPoints[ 10 ] := fromrow + Int( torow - fromrow ) / 2 // middleleftrow
   aPoints[ 11 ] := fromcol // middleleftcol
   aPoints[ 12 ] := fromcol + Int( tocol - fromcol ) / 2 // middlebottomcol
   aPoints[ 13 ] := fromrow + Int( torow - fromrow ) / 2 // middlerightrow
   aPoints[ 14 ] := tocol // middlerightcol

RETURN AClone( aPoints )

METHOD FillSunBurstData() CLASS GraphPlus

   LOCAL i, j, k
   LOCAL cHeader
   LOCAL nColorID := 1
   LOCAL aCurTotal := Array( ::nLevels )
   LOCAL aCurHeader := {}
   LOCAL aConcatData := AClone( ::aData )
   AFill( aCurTotal, 0 )
   ASize( ::aLegends, 0 )
   ASize( ::aLevelHeaders, 0 )
   ASize( ::aLevelValues, 0 )
   FOR i := 1 TO hmg_len( aConcatData )
      FOR j := 1 TO ::nLevels
         cHeader := ''
         FOR k := 1 TO j
            cHeader := cHeader + if( hmg_len( AllTrim( cHeader ) ) > 0, ': ', '' ) + ::aData[ i, k ]
         NEXT k
         aConcatData[ i, j ] := cHeader
      NEXT j
   NEXT i
   FOR i := 1 TO ::nLevels
      AAdd( aCurHeader, aConcatData[ 1, i ] )
      AAdd( ::aLevelHeaders, {} )
      AAdd( ::aLevelValues, {} )
   NEXT i
   FOR i := 1 TO hmg_len( aConcatData )
      FOR j := ::nLevels TO 1 STEP -1
         IF aConcatData[ i, j ] <> aCurHeader[ j ]
            AAdd( ::aLevelValues[ j ], { nColorID, aCurTotal[ j ] } )
            AAdd( ::aLevelHeaders[ j ], ::aData[ i - 1, j ] ) // aCurHeader[ j ] )
            IF j == 1
               nColorID := nColorID + 1
               AAdd( ::aLegends, aCurHeader[ j ] )
            ENDIF
            aCurHeader[ j ] := aConcatData[ i, j ]
            aCurTotal[ j ] := aConcatData[ i, ::nLevels + 1 ]
         ELSE
            aCurTotal[ j ] := aCurTotal[ j ] + aConcatData[ i, ::nLevels + 1 ]
         ENDIF
      NEXT j
   NEXT i
   // last row!
   FOR j := 1 TO ::nLevels
      AAdd( ::aLevelValues[ j ], { nColorID, aCurTotal[ j ] } )
      AAdd( ::aLevelHeaders[ j ], ::aData[ i - 1, j ] )
   NEXT j
   AAdd( ::aLegends, aCurHeader[ 1 ] )

RETURN NIL

METHOD FillTreeMapData() CLASS GraphPlus

   LOCAL i, j, k, cHeader
   LOCAL nColorID := 1
   LOCAL aCurTotal := Array( ::nLevels )
   LOCAL aCurHeader := {}
   LOCAL aConcatData := AClone( ::aData )
   AFill( aCurTotal, 0 )
   ASize( ::aLegends, 0 )
   ASize( ::aLevelHeaders, 0 )
   ASize( ::aLevelValues, 0 )
   FOR i := 1 TO hmg_len( aConcatData )
      FOR j := 1 TO ::nLevels
         cHeader := ''
         FOR k := 1 TO j
            cHeader := cHeader + if( hmg_len( AllTrim( cHeader ) ) > 0, ': ', '' ) + ::aData[ i, k ]
         NEXT k
         aConcatData[ i, j ] := cHeader
      NEXT j
   NEXT i
   FOR i := 1 TO ::nLevels
      AAdd( aCurHeader, aConcatData[ 1, i ] )
      AAdd( ::aLevelHeaders, {} )
      AAdd( ::aLevelValues, {} )
   NEXT i
   FOR i := 1 TO hmg_len( aConcatData )
      FOR j := ::nLevels TO 1 STEP -1
         IF aConcatData[ i, j ] <> aCurHeader[ j ]
            AAdd( ::aLevelValues[ j ], { nColorID, aCurTotal[ j ], aCurHeader[ j ], if( j > 1, aCurHeader[ j - 1 ], '' ) } )
            AAdd( ::aLevelHeaders[ j ], ::aData[ i - 1, j ] ) // aCurHeader[ j ] )
            IF j == 1
               nColorID := nColorID + 1
               AAdd( ::aLegends, aCurHeader[ j ] )
            ENDIF
            aCurHeader[ j ] := aConcatData[ i, j ]
            aCurTotal[ j ] := aConcatData[ i, ::nLevels + 1 ]
         ELSE
            aCurTotal[ j ] := aCurTotal[ j ] + aConcatData[ i, ::nLevels + 1 ]
         ENDIF
      NEXT j
   NEXT i
   // last row!
   FOR j := 1 TO ::nLevels
      AAdd( ::aLevelValues[ j ], { nColorID, aCurTotal[ j ], aCurHeader[ j ], if( j > 1, aCurHeader[ j - 1 ], '' ) } )
      AAdd( ::aLevelHeaders[ j ], ::aData[ i - 1, j ] )
   NEXT j
   AAdd( ::aLegends, aCurHeader[ 1 ] )

RETURN NIL


/**
 * Copyright (C) 2001 by University of Maryland, College Park, MD 20742, USA
 * and Martin Wattenberg, w@bewitched.com
 * All rights reserved.
 * Authors: Benjamin B. Bederson and Martin Wattenberg
 * http://www.cs.umd.edu/hcil/treemaps
 */

METHOD StripTreemap( nRow1, nCol1, nWidth1, nHeight1 ) CLASS GraphPlus

   LOCAL aLayoutBox := { nRow1, nCol1, nWidth1, nHeight1 } // rectangle with 4 items row, col, width and height
   LOCAL aBounds := AClone( aLayoutBox )

   LOCAL lLookAhead := .T.
   LOCAL i
   LOCAL ntotalSize := 0
   LOCAL nArea, nScaleFactor
   LOCAL nFinishedIndex := 1
   LOCAL nNumItems, nNumItems2
   LOCAL nAR2a, nAR2b
   LOCAL nHeight
   LOCAL nRowOffset := 0
   LOCAL aBox, aRect

   FOR i := 1 TO hmg_len( ::aTreeMapData )
      nTotalSize := nTotalSize + ::aTreeMapData[ i ]
   NEXT i
   nArea := aLayoutBox[ 3 ] * aLayoutBox[ 4 ]
   nScaleFactor := ( nArea / nTotalSize ) ^ 0.5
   aBox := AClone( aLayoutBox )
   aBox[ 1 ] := aBox[ 1 ] / nScaleFactor
   aBox[ 2 ] := aBox[ 2 ] / nScaleFactor
   aBox[ 3 ] := aBox[ 3 ] / nScaleFactor
   aBox[ 4 ] := aBox[ 4 ] / nScaleFactor
   DO WHILE nFinishedIndex <= hmg_len( ::aTreeMapData )
      // Layout strip
      nNumItems = ::LayoutStrip( aBox, nFinishedIndex )
      // Lookahead to second strip
      IF lLookAhead
         IF ( ( nFinishedIndex + nNumItems ) <= hmg_len( ::aTreeMapData ) )
            // Layout 2nd strip and compute AR of first strip plus 2nd strip
            nNumItems2 = ::LayoutStrip( aBox, nFinishedIndex + nNumItems )
            nAR2a = ::ComputeAverageAspectRatio( nFinishedIndex, nNumItems + nNumItems2 )
            // Layout 1st and 2nd strips together
            ::ComputeHorizontalBoxLayout( aBox, nFinishedIndex, nNumItems + nNumItems2 )
            nAR2b = ::ComputeAverageAspectRatio( nFinishedIndex, nNumItems + nNumItems2 )
            // msginfo("F: numItems2 = " + str( nNumItems2 ) + ", ar2a="+ str( nAR2a ) +", ar2b="+str( nAR2b) )
            IF nAR2b < nAR2a
               nNumItems := nNumItems + nNumItems2
               // msginfo("G: numItems = " + str( nNumItems) )
            ELSE
               ::computeHorizontalBoxLayout( aBox, nFinishedIndex, nNumItems )
               // msginfo("H: backup numItems = " + str(nNumItems))
            ENDIF
         ENDIF
      ENDIF
      FOR i := nFinishedIndex TO nFinishedIndex + nNumItems - 1
         ::aTreeMapRect[ i, 1 ] := ::aTreeMapRect[ i, 1 ] + nRowOffset
      NEXT i
      nHeight := ::aTreeMapRect[ nFinishedIndex, 4 ]
      nRowoffset := nRowOffset + nHeight
      aBox[ 1 ] := aBox[ 1 ] + nHeight
      aBox[ 4 ] := aBox[ 4 ] - nHeight
      nFinishedIndex := nFinishedIndex + nNumItems
   ENDDO
   FOR i := 1 TO hmg_len( ::aTreeMapData )
      aRect := ::aTreeMapRect[ i ]
      aRect[ 1 ] := aRect[ 1 ] * nScaleFactor
      aRect[ 2 ] := aRect[ 2 ] * nScaleFactor
      aRect[ 3 ] := aRect[ 3 ] * nScaleFactor
      aRect[ 4 ] := aRect[ 4 ] * nScaleFactor
      aRect[ 1 ] := aRect[ 1 ] + aBounds[ 1 ]
      aRect[ 2 ] := aRect[ 2 ] + aBounds[ 2 ]
      ::aTreeMapRect[ i ] := AClone( aRect )
   NEXT i

RETURN NIL

METHOD LayoutStrip( aBox, nIndex ) CLASS GraphPlus

   LOCAL nNumItems := 0
   LOCAL nPrevAR := 2 ^ 20
   LOCAL nAR
   //LOCAL nWidth
   //nPrevAr := nAR
   nNumItems := nNumItems + 1
   ::ComputeHorizontalBoxLayout( aBox, nIndex, nNumItems )
   nAR := ::ComputeAverageAspectRatio( nIndex, nNumItems )
   DO WHILE ( nAR < nPrevAR ) .AND. ( ( nIndex + nNumItems ) <= hmg_len( ::aTreeMapData ) )
      nPrevAr := nAR
      nNumItems := nNumItems + 1
      ::ComputeHorizontalBoxLayout( aBox, nIndex, nNumItems )
      nAR := ::ComputeAverageAspectRatio( nIndex, nNumItems )
   ENDDO
   IF nAR >= nPrevAR
      nNumItems := nNumItems - 1
      ::ComputeHorizontalBoxLayout( aBox, nIndex, nNumItems )
      ::ComputeAverageAspectRatio( nIndex, nNumItems )
      // debug("L.2: backup: numItems="+numItems);
   ENDIF

RETURN nNumItems

METHOD ComputeHorizontalBoxLayout( aBox, nIndex, nNumItems ) CLASS GraphPlus

   LOCAL i
   LOCAL nTotalSize := ::ComputeSize( nIndex, nNumItems )
   LOCAL nHeight := nTotalSize / aBox[ 3 ]
   LOCAL nWidth := 0
   LOCAL nCol := 0
   FOR i := nIndex TO nIndex + nNumItems - 1
      nWidth := ::aTreeMapData[ i ] / nHeight
      ::aTreeMapRect[ i ] := { 0, nCol, nWidth, nHeight }
      nCol := nCol + nWidth
   NEXT i

RETURN nWidth

METHOD ComputeSize( nIndex, nNum ) CLASS GraphPlus

   LOCAL nSize := 0
   LOCAL i
   FOR i := nIndex TO nIndex + nNum - 1
      // if i + nIndex <= hmg_len( ::aTreeMapData )
      nSize := nSize + ::aTreeMapData[ i ]
      // endif
   NEXT i

RETURN nSize

METHOD ComputeAverageAspectRatio( nIndex, nNumItems ) CLASS GraphPlus

   LOCAL nAR
   LOCAL nTAR := 0
   LOCAL nW
   LOCAL nH
   LOCAL i
   FOR i := nIndex TO nIndex + nNumItems - 1
      nW := ::aTreeMapRect[ i, 3 ]
      nH := ::aTreeMapRect[ i, 4 ]
      nAR := Max( nW / nH, nH / nW )
      nTAR := nTAR + nAR
   NEXT i
   nTAR := nTAR / nNumItems

RETURN nTAR


METHOD ComputeAspectRatio( nIndex ) CLASS GraphPlus

   LOCAL nW := ::aTreeMapRect[ 1, nIndex ][ 3 ]
   LOCAL nH := ::aTreeMapRect[ 1, nIndex ][ 4 ]
   LOCAL nAR := Max( nW / nH, nH / nW )

RETURN nAR


FUNCTION findboundary( nNum )

   LOCAL nE, nMax, nMan, nVal, nOffset

   nE := 10
   nVal := 0
   nNum := Abs( nNum )

   DO WHILE .T.

      nMax := 10 ** nE

      IF Int( nNum / nMax ) > 0

         nMan := ( nNum / nMax ) - Int( nNum / nMax )
         nOffset := 1
         nOffset := IF( nMan <= .75, .75, nOffset )
         nOffset := IF( nMan <= .50, .50, nOffset )
         nOffset := IF( nMan <= .25, .25, nOffset )
         nOffset := IF( nMan <= .00, .00, nOffset )
         nVal := ( Int( nNum / nMax ) + nOffset ) * nMax
         EXIT

      ENDIF

      nE--

   ENDDO

RETURN ( nVal )


FUNCTION GetNegativeColor( aRGB )

   LOCAL aNewRGB
   aNewRGB := { 255 - aRGB[ 1 ], 255 - aRGB[ 2 ], 255 - aRGB[ 3 ] }

RETURN aNewRGB

FUNCTION GP_RGB2HSL( aRGB )

   LOCAL nMax, nMin, nDelta
   LOCAL nH, nS, nL, nR, nG, nB

   nR := aRGB[ 1 ] / 255
   nG := aRGB[ 2 ] / 255
   nB := aRGB[ 3 ] / 255
   nMax := Max( nR, Max( nG, nB ) )
   nMin := Min( nR, Min( nG, nB ) )
   nDelta := nMax - nMin

   IF nDelta == 0
      nH := 0
   ELSEIF ( nMax == nR )
      nH := ( ( nG - nB ) / nDelta ) % 6
   ELSEIF ( nMax == nG )
      nH := ( nB - nR ) / nDelta + 2
   ELSE
      nH := ( nR - nG ) / nDelta + 4
   ENDIF
   nH := Int( nH * 60 )
   IF ( nH < 0 )
      nH += 360
   ENDIF
   nL := ( nMax + nMin ) / 2
   nS := iif( nDelta == 0, 0, nDelta / ( 1 - Abs( 2 * nL - 1 ) ) )
   nS := nS * 100
   nL := nL * 100

RETURN { nH, nS, nL }

FUNCTION GP_HSL2RGB( aHSL )

   LOCAL nR := 0, nG := 0, nB := 0, nH, nS, nL, c, x, m
   nH := aHSL[ 1 ]
   nS := aHSL[ 2 ] / 100
   nL := aHSL[ 3 ] / 100
   c := ( 1 - Abs( 2 * nL - 1 ) ) * nS
   x := c * ( 1 - Abs( ( nH / 60 ) % 2 - 1 ) )
   m := nL - c / 2
   IF ( 0 <= nH .AND. nH < 60 )
      nR := c
      nG := x
      nB := 0
   ELSEIF ( 60 <= nH .AND. nH < 120 )
      nR := x
      nG := c
      nB := 0
   ELSEIF ( 120 <= nH .AND. nH < 180 )
      nR := 0
      nG := c
      nB := x
   ELSEIF ( 180 <= nH .AND. nH < 240 )
      nR := 0
      nG := x
      nB := c
   ELSEIF ( 240 <= nH .AND. nH < 300 )
      nR := x
      nG := 0
      nB := c
   ELSEIF ( 300 <= nH .AND. nH < 360 )
      nR := c
      nG := 0
      nB := x
   ENDIF
   nR := Int( ( nR + m ) * 255 )
   nG := Int( ( nG + m ) * 255 )
   nB := Int( ( nB + m ) * 255 )

RETURN { nR, nG, nB }

FUNCTION degreetoradian( nDegree )
RETURN ( nDegree * 0.0174532925 )

FUNCTION radiantodegree( nRadian )
RETURN ( nRadian / 0.0174532925 )
