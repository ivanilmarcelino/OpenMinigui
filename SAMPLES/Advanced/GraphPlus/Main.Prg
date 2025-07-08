#include <hmg.ch>
#include <GraphPlus.ch>

MEMVAR aGraphObjects

DECLARE WINDOW gallery

FUNCTION MAIN

   PRIVATE aGraphObjects := {}
   DEFINE WINDOW chart AT 0, 0 WIDTH 890 HEIGHT 800 TITLE 'GraphPlus Demo' MAIN
      DEFINE TAB tab1 AT 10, 10 WIDTH 860 HEIGHT 650 ON CHANGE if( chart.tab1.VALUE == 1, gallery.SETFOCUS, NIL )
         DEFINE PAGE 'HMG Dashboard'
            DEFINE WINDOW gallery ;
                  ROW 30 ;
                  COL 10 ;
                  WIDTH 820 ;
                  HEIGHT 610 ;
                  VIRTUALHEIGHT 4000 ;
                  WINDOWTYPE PANEL
               DEFINE IMAGE image1
                  ROW 30
                  COL 10
                  WIDTH 400
                  HEIGHT 300
                  ONCLICK chart.tab1.VALUE := 2
               END IMAGE
               DEFINE IMAGE image2
                  ROW 30
                  COL 410
                  WIDTH 400
                  HEIGHT 300
                  ONCLICK chart.tab1.VALUE := 3
               END IMAGE
               DEFINE IMAGE image3
                  ROW 340
                  COL 10
                  WIDTH 400
                  HEIGHT 300
                  ONCLICK chart.tab1.VALUE := 4
               END IMAGE
               DEFINE IMAGE image4
                  ROW 340
                  COL 410
                  WIDTH 400
                  HEIGHT 300
                  ONCLICK chart.tab1.VALUE := 5
               END IMAGE
               DEFINE IMAGE image5
                  ROW 650
                  COL 10
                  WIDTH 400
                  HEIGHT 300
                  ONCLICK chart.tab1.VALUE := 6
               END IMAGE
               DEFINE IMAGE image6
                  ROW 650
                  COL 410
                  WIDTH 400
                  HEIGHT 300
                  ONCLICK chart.tab1.VALUE := 7
               END IMAGE
               DEFINE IMAGE image7
                  ROW 960
                  COL 10
                  WIDTH 400
                  HEIGHT 300
                  ONCLICK chart.tab1.VALUE := 8
               END IMAGE
               DEFINE IMAGE image8
                  ROW 960
                  COL 410
                  WIDTH 400
                  HEIGHT 75
                  ONCLICK chart.tab1.VALUE := 9
               END IMAGE
               DEFINE IMAGE image9
                  ROW 1270
                  COL 10
                  WIDTH 400
                  HEIGHT 300
                  ONCLICK chart.tab1.VALUE := 10
               END IMAGE
               DEFINE IMAGE image10
                  ROW 1270
                  COL 410
                  WIDTH 400
                  HEIGHT 300
                  ONCLICK chart.tab1.VALUE := 11
               END IMAGE
               DEFINE IMAGE image11
                  ROW 1580
                  COL 10
                  WIDTH 400
                  HEIGHT 300
                  ONCLICK chart.tab1.VALUE := 12
               END IMAGE
               DEFINE IMAGE image12
                  ROW 1580
                  COL 410
                  WIDTH 400
                  HEIGHT 300
                  ONCLICK chart.tab1.VALUE := 13
               END IMAGE
               DEFINE IMAGE image13
                  ROW 1890
                  COL 10
                  WIDTH 400
                  HEIGHT 300
                  ONCLICK chart.tab1.VALUE := 14
               END IMAGE
               DEFINE IMAGE image14
                  ROW 1890
                  COL 410
                  WIDTH 400
                  HEIGHT 300
                  ONCLICK chart.tab1.VALUE := 15
               END IMAGE
               DEFINE IMAGE image15
                  ROW 2200
                  COL 10
                  WIDTH 400
                  HEIGHT 300
                  ONCLICK chart.tab1.VALUE := 16
               END IMAGE
               DEFINE IMAGE image16
                  ROW 2200
                  COL 410
                  WIDTH 400
                  HEIGHT 300
                  ONCLICK chart.tab1.VALUE := 17
               END IMAGE
               DEFINE IMAGE image17
                  ROW 2510
                  COL 10
                  WIDTH 400
                  HEIGHT 300
                  ONCLICK chart.tab1.VALUE := 18
               END IMAGE
            END WINDOW
         END PAGE
         DEFINE PAGE 'Column'
            DEFINE IMAGE chart1
               ROW 30
               COL 30
               WIDTH 800
               HEIGHT 600
            END IMAGE
         END PAGE
         DEFINE PAGE 'Points'
            DEFINE IMAGE chart2
               ROW 30
               COL 30
               WIDTH 800
               HEIGHT 600
            END IMAGE
         END PAGE
         DEFINE PAGE 'Line'
            DEFINE IMAGE chart3
               ROW 30
               COL 30
               WIDTH 800
               HEIGHT 600
            END IMAGE
         END PAGE
         DEFINE PAGE 'Pie'
            DEFINE IMAGE chart4
               ROW 30
               COL 30
               WIDTH 800
               HEIGHT 600
            END IMAGE
         END PAGE
         DEFINE PAGE 'Bars'
            DEFINE IMAGE chart5
               ROW 30
               COL 30
               WIDTH 800
               HEIGHT 600
            END IMAGE
         END PAGE
         DEFINE PAGE 'Bar'
            DEFINE IMAGE chart6
               ROW 30
               COL 30
               WIDTH 800
               HEIGHT 600
            END IMAGE
         END PAGE
         DEFINE PAGE 'Funnel'
            DEFINE IMAGE chart7
               ROW 30
               COL 30
               WIDTH 800
               HEIGHT 600
            END IMAGE
         END PAGE
         DEFINE PAGE 'Grid Data'
            DEFINE IMAGE chart8
               ROW 30
               COL 30
               WIDTH 800
               HEIGHT 400
            END IMAGE
            DEFINE GRID DATA
               ROW 440
               COL 30
               WIDTH 400
               HEIGHT 180
               HEADERS { 'Period', 'Sales', 'Profit' }
               WIDTHS { 100, 120, 120 }
               JUSTIFY { 0, 1, 1 }
               ITEMS { { 'January', '15000', '2500' }, { 'February', '14000', '2800' }, ;
                  { 'March', '10000', '-2000' }, ;
                  { 'April', '16000', '3500' }, ;
                  { 'May', '15800', '3000' }, ;
                  { 'June', '16400', '4000' }, ;
                  { 'July', '15300', '2800' } }
               VALUE 1
            END GRID
         END PAGE
         DEFINE PAGE 'Stacked Column'
            DEFINE IMAGE chart9
               ROW 30
               COL 30
               WIDTH 800
               HEIGHT 600
            END IMAGE
         END PAGE
         DEFINE PAGE 'Stacked Bar'
            DEFINE IMAGE chart10
               ROW 30
               COL 30
               WIDTH 800
               HEIGHT 600
            END IMAGE
         END PAGE
         DEFINE PAGE 'Area'
            DEFINE IMAGE chart11
               ROW 30
               COL 30
               WIDTH 800
               HEIGHT 600
            END IMAGE
         END PAGE
         DEFINE PAGE 'Scatter XY'
            DEFINE IMAGE chart12
               ROW 30
               COL 30
               WIDTH 800
               HEIGHT 600
            END IMAGE
         END PAGE
         DEFINE PAGE 'Doughnut'
            DEFINE IMAGE chart13
               ROW 30
               COL 30
               WIDTH 800
               HEIGHT 600
            END IMAGE
         END PAGE
         DEFINE PAGE 'Sun burst'
            DEFINE IMAGE chart14
               ROW 30
               COL 30
               WIDTH 800
               HEIGHT 600
            END IMAGE
         END PAGE
         DEFINE PAGE 'Waterfall 1'
            DEFINE IMAGE chart15
               ROW 30
               COL 30
               WIDTH 800
               HEIGHT 600
            END IMAGE
         END PAGE
         DEFINE PAGE 'Waterfall 2'
            DEFINE IMAGE chart16
               ROW 30
               COL 30
               WIDTH 800
               HEIGHT 600
            END IMAGE
         END PAGE
         DEFINE PAGE 'Tree Map'
            DEFINE IMAGE chart17
               ROW 30
               COL 30
               WIDTH 800
               HEIGHT 600
            END IMAGE
         END PAGE
      END TAB
      DEFINE BUTTON savefiles
         ROW 720
         COL 10
         WIDTH 80
         CAPTION 'Save All'
         ACTION saveallfiles()
      END BUTTON
      DEFINE BUTTON switch
         ROW 720
         COL 110
         WIDTH 180
         CAPTION 'Switch Rows and Columns'
         ACTION switchrows()
      END BUTTON
      DEFINE LABEL pieholelabel
         ROW 685
         COL 10
         WIDTH 150
         VALUE 'DoughNut hole size (%)'
      END LABEL
      DEFINE slider piehole
         ROW 685
         COL 160
         WIDTH 100
         RANGEMIN 0
         RANGEMAX 200
         ONCHANGE holesizechanged()
      END slider
      DEFINE CHECKBUTTON legends
         ROW 690
         COL 300
         WIDTH 120
         CAPTION 'Show Legends'
         ONCHANGE drawcharts()
      END CHECKBUTTON
      DEFINE CHECKBUTTON horizontalgrid
         ROW 690
         COL 430
         WIDTH 120
         CAPTION 'Horizontal Grid'
         ONCHANGE drawcharts()
      END CHECKBUTTON
      DEFINE CHECKBUTTON verticalgrid
         ROW 690
         COL 560
         WIDTH 120
         CAPTION 'Vertical Grid'
         ONCHANGE drawcharts()
      END CHECKBUTTON
      DEFINE CHECKBUTTON legendsbottom
         ROW 690
         COL 690
         WIDTH 120
         CAPTION 'Legends in Bottom'
         ONCHANGE drawcharts()
      END CHECKBUTTON
      DEFINE CHECKBUTTON datalabels
         ROW 720
         COL 300
         WIDTH 120
         CAPTION 'Data Labels'
         ONCHANGE drawcharts()
      END CHECKBUTTON
      DEFINE CHECKBUTTON categories
         ROW 720
         COL 430
         WIDTH 120
         CAPTION 'Categories'
         ONCHANGE drawcharts()
      END CHECKBUTTON
      DEFINE CHECKBUTTON values
         ROW 720
         COL 560
         WIDTH 120
         CAPTION 'Values'
         ONCHANGE drawcharts()
      END CHECKBUTTON
      DEFINE COMBOBOX themeselector
         ROW 720
         COL 690
         WIDTH 120
         ITEMS { 'Color Palette 1', 'Color Palette 2', 'Color Palette 3', 'Color Palette 4', 'Color Palette 5', 'Color Palette 6', 'Custom' }
         ONCHANGE drawcharts()
      END COMBOBOX
      DEFINE TIMER dummy INTERVAL 200 ACTION gallery.SETFOCUS ONCE
   END WINDOW
   initcharts()
   StopControlEventProcedure ( 'legends', 'chart', .T. )
   StopControlEventProcedure ( 'horizontalgrid', 'chart', .T. )
   StopControlEventProcedure ( 'verticalgrid', 'chart', .T. )
   StopControlEventProcedure ( 'legendsbottom', 'chart', .T. )
   StopControlEventProcedure ( 'datalabels', 'chart', .T. )
   StopControlEventProcedure ( 'categories', 'chart', .T. )
   StopControlEventProcedure ( 'values', 'chart', .T. )
   StopControlEventProcedure ( 'piehole', 'chart', .T. )
   StopControlEventProcedure ( 'themeselector', 'chart', .T. )
   chart.legends.VALUE := .T.
   chart.horizontalgrid.VALUE := .T.
   chart.verticalgrid.VALUE := .T.
   chart.legendsbottom.VALUE := .T.
   chart.datalabels.VALUE := .F.
   chart.categories.VALUE := .T.
   chart.values.VALUE := .T.
   chart.piehole.VALUE := 20
   chart.themeselector.VALUE := 1
   StopControlEventProcedure ( 'legends', 'chart', .F. )
   StopControlEventProcedure ( 'horizontalgrid', 'chart', .F. )
   StopControlEventProcedure ( 'verticalgrid', 'chart', .F. )
   StopControlEventProcedure ( 'legendsbottom', 'chart', .F. )
   StopControlEventProcedure ( 'datalabels', 'chart', .F. )
   StopControlEventProcedure ( 'categories', 'chart', .F. )
   StopControlEventProcedure ( 'piehole', 'chart', .F. )
   StopControlEventProcedure ( 'values', 'chart', .F. )
   StopControlEventProcedure ( 'themeselector', 'chart', .F. )
   drawcharts()
   chart.CENTER
   chart.ACTIVATE

RETURN NIL

FUNCTION initcharts

   LOCAL oObj := NIL
   LOCAL aDataX, aDataY
   LOCAL i, aCategories
   ASize( aGraphObjects, 0 )
   // hmgcolumn
   oObj := GraphPlus():New()
   WITH OBJECT oObj
      :GraphData := { { 14280, 20420, 12870, 25347, 7640 }, ;
         { 8350, 10315, 15870, 5347, 12340 }, ;
         { 12345, -8945, 10560, 15600, 17610 } }
      :Legends := { "Serie 1", "Serie 2", "Serie 3" }
      :Categories := { "January", "February", "March", "April", "May" }
      :Title := 'Column Chart'
      :Picture := '99,999.99'
      :BarGapRatio := -0.4
   ENDWITH
   AAdd( aGraphObjects, oObj )
   // hmgpoints
   oObj := GraphPlus():New()
   WITH OBJECT oObj
      :GraphData := { { 14280, 20420, 12870, 25347, 7640 }, ;
         { 8350, 10315, 15870, 5347, 12340 }, ;
         { 12345, -8945, 10560, 15600, 17610 } }
      :Legends := { "Serie 1", "Serie 2", "Serie 3" }
      :Categories := { "January", "February", "March", "April", "May" }
      :Title := 'Points Chart'
      :Picture := '99,999.99'
      :GraphType := GT_POINTS
   ENDWITH
   // hmgline
   AAdd( aGraphObjects, oObj )
   oObj := GraphPlus():New()
   WITH OBJECT oObj
      :GraphData := { { 14280, 20420, 12870, 25347, 7640 }, ;
         { 8350, 10315, 15870, 5347, 12340 }, ;
         { 12345, -8945, 10560, 15600, 17610 } }
      :Legends := { "Serie 1", "Serie 2", "Serie 3" }
      :Categories := { "January", "February", "March", "April", "May" }
      :Title := 'Line Chart'
      :Picture := '99,999.99'
      :GraphType := GT_LINE
   ENDWITH
   AAdd( aGraphObjects, oObj )
   // pie
   oObj := GraphPlus():New()
   WITH OBJECT oObj
      :GraphData := { 13876405, 13404512, 3397445, 2697803, 2245456, 2166422, 2128265, 1860504, 1314459, 1257990 }
      :Legends := { "China", "India", "United States of America", "Indonesia", "Brazil", "Pakistan", "Nigeria", "Bangladesh", "Russia", "Mexico" }
      :Title := 'Population Top 10 Countries'
      :GraphType := GT_PIE
   ENDWITH
   AAdd( aGraphObjects, oObj )

   // bars
   oObj := GraphPlus():New()
   WITH OBJECT oObj
      :GraphData := { { 13876405, 13404512, 3397445, 2697803, 2245456, 2166422, 2128265, 1860504, 1314459, 1257990 }, ;
         { 16642318, 3049704, 22675271, 1158783, 1491772, 296000, 514049, 352908, 1710734, 1192480 } }
      :Categories := { "China", "India", "USA", "Indonesia", "Brazil", "Pakistan", "Nigeria", "Bangladesh", "Russia", "Mexico" }
      :Legends := { "Population in Hundreds", "GDP in Million $" }
      :Title := 'World Population - Top 10'
      :BarGapRatio := 0
      :BarGapWidthRatio := 0.1
      :GraphType := GT_COLUMNS
   ENDWITH
   AAdd( aGraphObjects, oObj )

   // hmgbar
   oObj := GraphPlus():New()
   WITH OBJECT oObj
      :GraphData := { 1387640576, 1340451200, 339744544, 269780352, 224545616, 216642256, 212826576, 186050400, 131445920, 125799016 }
      :Categories := { "China", "India", "United States of America", "Indonesia", "Brazil", "Pakistan", "Nigeria", "Bangladesh", "Russia", "Mexico" }
      :Legends := { "Population in Hundreds" }
      :Title := 'World Population - Top 10'
      :BarGapRatio := 0
      :BarGapWidthRatio := 0.1
      :LegendFont := array FONT 'Arial' SIZE 10
      :GraphType := GT_BAR
   ENDWITH
   AAdd( aGraphObjects, oObj )
   // funnel
   oObj := GraphPlus():New()
   WITH OBJECT oObj
      :GraphData := { 8000, 6000, 4500, 2000, 1200, 900 }
      :Categories := { "Awareness", "Interest", "Consideration", "Intent", "Evaluation", "Purchases" }
      :Title := 'Sales Funnel'
      :BarGapRatio := 0
      :BarGapWidthRatio := 0.1
      :GraphType := GT_FUNNEL
   ENDWITH
   AAdd( aGraphObjects, oObj )
   // grid data
   oObj := GraphPlus():new()
   WITH OBJECT oObj
      :GridData( 'chart', 'data' )
      :Width := 800
      :Height := 400
      :SwitchRowstoColumns()
      :Title := 'Data from Grid!'
   ENDWITH
   AAdd( aGraphObjects, oObj )
   // stacked column
   oObj := GraphPlus():New()
   WITH OBJECT oObj
      :GraphData := { { 14280, -20420, 12870, 25347, 7640 }, ;
         { 8350, 20315, 15870, 5347, 12340 }, ;
         { 12345, -8945, 10560, 15600, 17610 } }
      :Legends := { "Serie 1", "Serie 2", "Serie 3" }
      :Categories := { "January", "February", "March", "April", "May" }
      :Title := 'Stacked Column'
      :Picture := '99,999'
      :BarGapRatio := -0.4
      :GraphType := GT_STACKEDCOLUMN
   ENDWITH
   AAdd( aGraphObjects, oObj )

   // stacked bar
   oObj := GraphPlus():New()
   WITH OBJECT oObj
      :GraphData := { { 14280, -20420, 12870, 25347, 7640 }, ;
         { 8350, 20315, 15870, 5347, 12340 }, ;
         { 12345, -8945, 10560, 15600, 17610 } }
      :Legends := { "Serie 1", "Serie 2", "Serie 3" }
      :Categories := { "January", "February", "March", "April", "May" }
      :Title := 'Stacked Bar'
      :Picture := '99,999'
      :BarGapRatio := -0.4
      :GraphType := GT_STACKEDBAR
   ENDWITH
   AAdd( aGraphObjects, oObj )
   // Area
   oObj := GraphPlus():New()
   WITH OBJECT oObj
      :GraphData := { { 800, 1000, 1100, 1040, 980 }, ;
         { 650, 780, 1300, 840, 700 }, ;
         { 150, 220, -200, 200, 280 } }
      :Legends := { "Sales", "Cost", "Profit" }
      :Categories := { "January", "February", "March", "April", "May" }
      :Title := 'Area Chart'
      :Picture := '99,999'
      :BarGapRatio := -0.4
      :GraphType := GT_AREA
   ENDWITH
   AAdd( aGraphObjects, oObj )

   // ScatterXY
   oObj := GraphPlus():New()
   WITH OBJECT oObj
      // data for y = X^2
      aDataX := {}
      aDataY := {}
      FOR i := -10 TO 10
         AAdd( aDataX, i )
         AAdd( aDataY, ( i * i ) )
      NEXT i
      :GraphData := { aDataX, ;
         aDataY }
      :Title := 'ScatterXY Chart'
      :XTitle := 'x'
      :YTitle := 'y = x^2'
      :PictureX := '999'
      :PictureY := '999'
      :GraphType := GT_SCATTERXY
      :ScatterLine := .T.
      :VDivisions := 10
      :HDivisions := 10
   ENDWITH
   AAdd( aGraphObjects, oObj )

   // DoughNut
   oObj := GraphPlus():New()
   WITH OBJECT oObj
      :GraphData := { { 1000, 2000, 3000, 3000, 2000, 1000 }, ;
         { 2000, 1000, 2000, 1000, 2000, 1000 }, ;
         { 3000, 2000, 1000, 1000, 2000, 3000 } }
      :Legends := { "Product 1", "Product 2", "Product 3" }
      :Categories := { "India", "Srilanka", "Japan", "Malaysia", "Bangladesh", "Pakistan" }
      :Title := 'Doughnut Chart'
      :GraphType := GT_DOUGHNUT
      :BarGapRatio := 0.2
   ENDWITH
   AAdd( aGraphObjects, oObj )

   // SunBurst
   oObj := GraphPlus():New()
   WITH OBJECT oObj
      :GraphData := { { "Material", "Core", "Graphite", 400 }, ;
         { "Material", "Core", "Clay", 200 }, ;
         { "Material", "Core", "Water", 50 }, ;
         { "Material", "Core", "Wood", 250 }, ;
         { "Material", "Packing", "Board", 100 }, ;
         { "Material", "Packing", "Plastic Cover", 90 }, ;
         { "Material", "Packing", "Corrugated Box", 25 }, ;
         { "Labour", "Skilled", "Foreman", 150 }, ;
         { "Labour", "Skilled", "Machine Operator", 170 }, ;
         { "Labour", "Semi-Skilled", "Wood Cutter", 100 }, ;
         { "Labour", "Semi-Skilled", "Slot making", 140 }, ;
         { "Labour", "Unskilled", "Loadman", 75 }, ;
         { "Labour", "Unskilled", "Cleaner", 50 }, ;
         { "Overheads", "Direct", "Power", 150 }, ;
         { "Overheads", "Direct", "Factory Rent", 100 }, ;
         { "Overheads", "Indirect", "Salary", 150 }, ;
         { "Overheads", "Indirect", "Insurance", 40 } }
      :Title := 'Sun burst Chart - Pencil Cost'
      :GraphType := GT_SUNBURST
      :BarGapRatio := 0.3
   ENDWITH
   AAdd( aGraphObjects, oObj )

   // Waterfall1
   oObj := GraphPlus():New()
   WITH OBJECT oObj
      :GraphData := { 15000, -10000, 15000, -12000, -6000, 7000, 9000, -6000, 4000, -20000, -4000 }
      :TotalItems := { 1, 11 }
      :GraphColors := { { 0, 255, 0 }, { 255, 0, 0 }, { 0, 0, 255 } }
      :Categories := { 'Opening', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Closing' }
      :Legends := { 'Debit', 'Credit', 'Balance' }
      :Title := 'Waterfall Model Chart - Transactions in account X'
      :GraphType := GT_WATERFALL
      :BarGapRatio := 0.2
   ENDWITH
   AAdd( aGraphObjects, oObj )

   // Waterfall2
   aCategories := {}
   oObj := GraphPlus():New()
   WITH OBJECT oObj
      :GraphData := { -13, 0, 0, 40, -38, 17, 62, 22, 1, 0, 22, -6, -20, 3, 25, 5, 0, 56, -7, -5, 17, 3, 6, 0, 1, 33, -16, 6, -10, 7, 0 }
      :GraphColors := { { 0, 255, 0 }, { 255, 0, 0 }, { 0, 0, 255 } }
      FOR i := 1 TO 31
         AAdd( aCategories, PadL( i, 2, 0 ) )
      NEXT i
      :Categories := aCategories
      :Legends := { 'Increase', 'Decrease', 'Balance' }
      :Title := 'Waterfall Model Chart - Gold Prices movement - May, 2021'
      :GraphType := GT_WATERFALL
      :BarGapRatio := 0
   ENDWITH
   AAdd( aGraphObjects, oObj )

   // TreeMap
   oObj := GraphPlus():New()
   WITH OBJECT oObj
      :GraphData := { { "Material", "Core", "Graphite", 400 }, ;
         { "Material", "Core", "Clay", 200 }, ;
         { "Material", "Core", "Water", 50 }, ;
         { "Material", "Core", "Wood", 250 }, ;
         { "Material", "Packing", "Board", 100 }, ;
         { "Material", "Packing", "Plastic Cover", 90 }, ;
         { "Material", "Packing", "Corrugated Box", 25 }, ;
         { "Labour", "Skilled", "Foreman", 150 }, ;
         { "Labour", "Skilled", "Machine Operator", 170 }, ;
         { "Labour", "Semi-Skilled", "Wood Cutter", 100 }, ;
         { "Labour", "Semi-Skilled", "Slot making", 140 }, ;
         { "Labour", "Unskilled", "Loadman", 75 }, ;
         { "Labour", "Unskilled", "Cleaner", 50 }, ;
         { "Overheads", "Direct", "Power", 150 }, ;
         { "Overheads", "Direct", "Factory Rent", 100 }, ;
         { "Overheads", "Indirect", "Salary", 150 }, ;
         { "Overheads", "Indirect", "Insurance", 40 } }
      :Title := 'Tree Map Chart - Pencil Cost'
      :GraphType := GT_TREEMAP
      :BarGapRatio := 0.3
      :ShowLegends := .T.
      :ShowLabels := .T.
      :LegendPos := LEGEND_ON_BOTTOM
   ENDWITH
   AAdd( aGraphObjects, oObj )

RETURN NIL

FUNCTION drawcharts

   LOCAL nTotalTabs := hmg_len( aGraphObjects )
   LOCAL oObj := NIL
   LOCAL cControl, cThumbnail
   LOCAL nImageWidth, nImageHeight
   LOCAL i
   FOR i := 1 TO nTotalTabs
      oObj := aGraphObjects[ i ]
      cControl := 'chart' + AllTrim( Str( i, 2, 0 ) )
      cThumbnail := 'image' + AllTrim( Str( i, 2, 0 ) )
      nImageHeight := if( i <> 8, 600, 400 ) // for grid data
      nImageWidth := 800
      WITH OBJECT oObj
         :Width := nImageWidth
         :Height := nImageHeight
         :ShowLegends := chart.legends.VALUE
         :ShowLabels := chart.datalabels.VALUE
         :ShowCategories := chart.categories.VALUE
         :ShowValues := chart.values.VALUE
         :LegendPos := if( chart.legendsbottom.VALUE, LEGEND_ON_BOTTOM, LEGEND_ON_RIGHT )
         :ShowHGrid := chart.horizontalgrid.VALUE
         :ShowVGrid := chart.verticalgrid.VALUE
         :LegendFont := CREATE array FONT 'Arial' SIZE 12 BOLD .F.
         :TitleFont := CREATE array FONT 'Arial' SIZE 16 BOLD .T.
         IF chart.themeselector.VALUE <= 6
            :ColorTheme := chart.themeselector.VALUE
         ELSE
            :GraphColors := { { 0, 0, 255 }, { 0, 100, 255 }, { 100, 200, 255 }, { 255, 100, 0 }, { 0, 255, 100 }, { 255, 255, 100 }, { 200, 200, 200 }, { 200, 100, 100 }, { 255, 200, 100 }, { 60, 60, 60 } }
         ENDIF
         :Draw()
         SetProperty( 'chart', cControl, 'HBITMAP', :Bitmap )
         SetProperty( 'gallery', cThumbnail, 'HBITMAP', :Bitmap( 400, if( i <> 8, 300, 150 ) ) )
      ENDWITH
   NEXT i

RETURN NIL

FUNCTION switchrows

   LOCAL nTabNo := chart.tab1.VALUE - 1
   LOCAL oObj := NIL
   LOCAL cControl := 'chart'
   IF nTabNo == 0
      msginfo( 'Select a chart to switch rows/columns' )
      RETURN NIL
   ENDIF
   IF .NOT. ( nTabNo == 4 .OR. nTabNo == 7 .OR. nTabNo == 12 .OR. nTabNo >= 14 ) // pie, funnel and scatterxy chart
      oObj := aGraphObjects[ nTabNo ]
      cControl := cControl + AllTrim( Str( nTabNo, 2, 0 ) )
      IF oObj <> NIL
         WITH OBJECT oObj
            :SwitchRowstoColumns()
            :Draw()
            SetProperty( 'Chart', cControl, 'HBITMAP', :Bitmap() )
         ENDWITH
      ENDIF
   ENDIF

RETURN NIL

FUNCTION holesizechanged

   LOCAL oObj := NIL
   LOCAL cControl := 'chart'
   LOCAL cThumbnail := 'image'
   LOCAL i
   IF hmg_len( aGraphObjects ) >= 14
      FOR i := 13 TO 14
         oObj := aGraphObjects[ i ] // doughnut and sunburst
         oObj:BarGapRatio := chart.piehole.value / 100
         oObj:Draw()
         SetProperty( 'Chart', cControl + hb_ntos( i ), 'HBITMAP', oObj:Bitmap() )
         SetProperty( 'gallery', cThumbnail + hb_ntos( i ), 'HBITMAP', oObj:Bitmap( 400, 300 ) )
      NEXT i
   ENDIF

RETURN NIL

FUNCTION saveallfiles

   LOCAL oObj := NIL
   LOCAL i
   FOR i := 1 TO hmg_len( aGraphObjects )
      oObj := aGraphObjects[ i ]
      oObj:save( 'GraphPlus' + PadL( i, 2, '0' ) + '.png' )
   NEXT i

RETURN NIL
