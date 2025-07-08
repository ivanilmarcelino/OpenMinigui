/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
*/

#include "hmg.ch"
#include "GraphPlus.ch"

// define the static arrays for graph show
STATIC aSeries
STATIC aDatabases
STATIC aColors
STATIC oObj

/////////////////////////////////////////////////////////////
FUNCTION Main

   aSeries := {}
   aDatabases := {}

   USE DATA NEW
   GO TOP
   DO WHILE !Eof()
      AAdd( aSeries, DATA->SCORE )
      AAdd( aDatabases, DATA->DB )
      SKIP
   ENDDO
   CLOSE DATA

   // create graph object
   oObj := GraphPlus():New()

   // set the colors
   aColors := { ;
      { 29, 129, 162 }, ;
      }

   SET FONT TO "Arial", 10

   DEFINE WINDOW Graph ;
      AT 0, 0 ;
      WIDTH 910 HEIGHT 660 + GetTitleHeight() + GetBorderHeight() ;
      MAIN ;
      TITLE "Bar Graph Demo" ;
      BACKCOLOR { 216, 208, 200 } ;
      ON INIT showbar()

   DEFINE IMAGE chart
      ROW 4
      COL 4
      WIDTH 884
      HEIGHT 644
      STRETCH .T.
   END IMAGE

      ON KEY ESCAPE ACTION Graph.Release
   END WINDOW

   Graph.Center()
   Graph.Activate()

RETURN NIL

/////////////////////////////////////////////////////////////
FUNCTION showbar

   IF ! Empty( oObj:hBitmap )
      DeleteObject( oObj:hBitmap )
      oObj:hBitmap := NIL
   ENDIF

   WITH OBJECT oObj
      :Width := Graph.chart.Width
      :Height := Graph.chart.Height
      :GraphData := aSeries
      :Categories := aDatabases
      :GraphColors := aColors
      :Title := 'Top 10 Databases in 2022'
      :GraphType := GT_BAR
      :ShowLabels := .T.
      :ShowHGrid := .F.
      :ShowVGrid := .T.
      :LegendFont := CREATE ARRAY FONT (_HMG_DefaultFontName) SIZE (_HMG_DefaultFontSize + 2) BOLD .F.
      :TitleFont := CREATE ARRAY FONT (_HMG_DefaultFontName) SIZE (_HMG_DefaultFontSize + 6) BOLD .T.
      :aTitleColor := BLACK
      :BarGapRatio := 20
      :BarGapWidthRatio := 0.5
      :nVDivision := 3
      :Draw()
      SetProperty( ThisWindow.Name, 'chart', 'HBITMAP', :Bitmap )
   ENDWITH

RETURN NIL
