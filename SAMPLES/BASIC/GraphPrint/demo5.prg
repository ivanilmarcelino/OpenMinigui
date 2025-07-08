/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
*/

#include "hmg.ch"
#include "GraphPlus.ch"

// define the static arrays for graph show
STATIC aSeries
STATIC aCountries
STATIC aSerieNames
STATIC aColors
STATIC oObj

/////////////////////////////////////////////////////////////
FUNCTION Main

   // create graph object
   oObj := GraphPlus():New()

   // set the series data
   aSeries := { ;
      { 1300, 1350, 1250, 1000, 1350, 1000, 950, 1100, 1200, 1150, 1600, 950, 1400, 750, 700, 500, 700, 900, ;
        840, 700, 900, 850, 700, 600, 440 }, ;
      { 6300-1300-4723, 4400-1350-2672, 4300-1250-2811, 3800-1000-2549, 3700-1350-2038, 3660-1000-2357, 3400-950-2120, ;
        3300-1100-1953, 3250-1200-1780, 3000-1150-1572, 2950-1600-1054, 2900-950-1620, 2880-1400-1252, 2860-750-1939, ;
        2850-700-1816, 2840-500-2026, 2640-700-1833, 2630-900-1519, 2620-840-1611, 2500-700-1586, 2480-900-1308, ;
        2460-850-1436, 2440-700-1565, 2400-600-1541, 2100-440-1496 }, ;
      { 4723, 2672, 2811, 2549, 2038, 2357, 2120, 1953, 1780, 1572, 1054, 1620, 1252, 1939, 1816, 2026, 1833, 1519, ;
        1611, 1586, 1308, 1436, 1565, 1541, 1496 } ;
      }

   aCountries := { ;
      "Switzerland", ;
      "Luxembourg", ;
      "United States", ;
      "Denmark", ;
      "Singapore", ;
      "Australia", ;
      "Norway", ;
      "Qatar", ;
      "Iceland", ;
      "Netherlands", ;
      "Hong Kong", ;
      "United Arab Emirates", ;
      "Ireland", ;
      "Finland", ;
      "Germany", ;
      "Japan", ;
      "Sweden", ;
      "United Kingdom", ;
      "New Zealand", ;
      "France", ;
      "Israel", ;
      "Canada", ;
      "Belgium", ;
      "Austria", ;
      "South Korea" ;
      }

   // set the series names
   aSerieNames := { ;
      "Rent (1 bedroom apartment outside of centre)", ;
      "Utilities and internet", ;
      "Disposable income" ;
      }

   // set the colors
   aColors := { ;
      { 80, 128, 190 }, ;
      { 190, 75, 75 }, ;
      { 150, 190, 80 } ;
      }

   SET FONT TO "Arial", 8

   DEFINE WINDOW m ;
      AT 0, 0 ;
      WIDTH 910 HEIGHT 960 + GetTitleHeight() + GetBorderHeight() ;
      MAIN ;
      TITLE "Stacked Bar Graph Demo" ;
      BACKCOLOR { 216, 208, 200 } ;
      ON INIT showbar()

   DEFINE IMAGE chart
      ROW 4
      COL 4
      WIDTH 884
      HEIGHT 944
      STRETCH .T.
   END IMAGE

   END WINDOW

   m.Center()
   m.Activate()

RETURN NIL

/////////////////////////////////////////////////////////////
FUNCTION showbar

   IF ! Empty( oObj:hBitmap )
      DeleteObject( oObj:hBitmap )
      oObj:hBitmap := NIL
   ENDIF

   WITH OBJECT oObj
      :Width := m.chart.Width
      :Height := m.chart.Height
      :GraphData := aSeries
      :Categories := aCountries
      :Legends := aSerieNames
      :GraphColors := aColors
      :Title := 'Disposable incomes of countries with highest average monthly salaries in 2020 (in USD)'
      :GraphType := GT_STACKEDBAR
      :ShowHGrid := .T.
      :ShowLegends := .T.
      :LegendPos := LEGEND_ON_BOTTOM
      :LegendFont := CREATE ARRAY FONT (_HMG_DefaultFontName) SIZE (_HMG_DefaultFontSize + 2) BOLD .F.
      :TitleFont := CREATE ARRAY FONT (_HMG_DefaultFontName) SIZE (_HMG_DefaultFontSize + 6) BOLD .T.
      :aTitleColor := BLACK
      :BarGapRatio := -0.4
      :Draw()
      SetProperty( ThisWindow.Name, 'chart', 'HBITMAP', :Bitmap )
   ENDWITH

RETURN NIL
