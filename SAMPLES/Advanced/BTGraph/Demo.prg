/*
  MINIGUI - Harbour Win32 GUI library Demo

  Author: Siri Rathinagiri <srgiri@dataone.in>

  Revised by Grigory Filatov <gfilatov@inbox.ru>
*/

#include "hmg.ch"


FUNCTION Main

   set font to "Arial", 9

   define window graph at 0, 0 width 1000 height 700 title 'Bos Taurus Graph' main

      define label selecttype
         row 10
         col 45
         width 115
         value 'Select Graph Type'
         vcenteralign .T.
      end label

      define combobox graphtype
         row 10
         col 160
         width 100
         items { 'Bar', 'Lines', 'Points', 'Pie' }
         onchange drawgraph()
      end combobox

      define checkbox enable3d
         row 10
         col 280
         width 100
         caption 'Enable 3D'
         onchange drawgraph()
         value .T.
      end checkbox

      define button Button_1
         row 10
         col 400
         caption 'Save as PNG'
         action ProcSaveGraph()
      end button

      define image grapharea
         row 50
         col 50
         width 600
         height 600
         stretch .T.
      end image

   end window

   graph.graphtype.value := 1

   graph.center
   graph.activate

RETURN NIL


FUNCTION drawgraph

   LOCAL nImageWidth := graph.grapharea.width
   LOCAL nImageHeight := graph.grapharea.height
   LOCAL aSerieValues, aSerieYNames
   LOCAL hBitmap

   IF graph.graphtype.value == 0
      RETURN NIL
   ENDIF

   IF graph.graphtype.value == 4 // pie

      GRAPH BITMAP PIE ;
            SIZE        nImageWidth, nImageHeight ;
            SERIEVALUES { 1500,        1800,        200,         500,         800 } ;
            SERIENAMES  { "Product 1", "Product 2", "Product 3", "Product 4", "Product 5" } ;
            SERIECOLORS { RED,         BLUE,        YELLOW,      GREEN,       ORANGE } ;
            PICTURE     "9,999" ;
            TITLE       "Sales" ;
;//            TITLECOLOR  BLACK ;
            DEPTH       25 ;
            3DVIEW      graph.Enable3D.VALUE ;
            SHOWXVALUES .T. ;
            SHOWLEGENDS .T. ;
;//            NOBORDER    .F. ;
            STOREIN     hBitmap

   ELSE

      #define COLOR1   { 128, 128, 255 }
      #define COLOR2   { 255, 102,  10 }
      #define COLOR3   {  55, 201,  48 }

      aSerieValues := { { 14280,  20420,  12870,  25347,   7640 },;
                        {  8350,  10315,  15870,   5347,  12340 },;
                        { 12345,  -8945,  10560,  15600,  17610 } }

      aSerieYNames :=   { "Jan",  "Feb",  "Mar",  "Apr",  "May" } 

      GRAPH BITMAP      graph.GraphType.VALUE ;  // constants: BARS = 1, LINES = 2, POINTS = 3 are defined in i_graph.ch 
            SIZE        nImageWidth, nImageHeight ;
            SERIEVALUES aSerieValues ;
            SERIENAMES  { "Serie 1", "Serie 2", "Serie 3"} ;
            SERIECOLORS { COLOR1,    COLOR2,    COLOR3   } ;
            SERIEYNAMES aSerieYNames ;
            PICTURE     "99,999.99" ;
            TITLE       "Sample Graph" ;
;//            TITLECOLOR  BLACK ;
            HVALUES     5 ;
            BARDEPTH    15 ; 
            BARWIDTH    15 ;
;//            SEPARATION  NIL ;
            LEGENDWIDTH 50 ;
            3DVIEW      graph.Enable3D.VALUE ;
            SHOWGRID    .T. ;
;//            SHOWXGRID   .T. ;
;//            SHOWYGRID   .T. ;
            SHOWVALUES  .T. ;
            SHOWXVALUES .T. ;
            SHOWYVALUES .T. ;               
            SHOWLEGENDS .T. ; 
;//            NOBORDER    .F. ;
            STOREIN     hBitmap 

   ENDIF

   graph.grapharea.HBITMAP := hBitmap   // Assign hBitmap to the IMAGE control

RETURN NIL


PROCEDURE ProcSaveGraph()

   LOCAL cFileName
   LOCAL hBitmap := graph.grapharea.HBITMAP   // Gets the value of hBitmap from the IMAGE control

   IF hBitmap <> 0 .AND. graph.GraphType.VALUE > 0

      cFileName := "Graph_" + graph.GraphType.ITEM( graph.GraphType.VALUE ) + IIF( graph.Enable3D.VALUE, "3D", "2D") + ".PNG"
      BT_BitmapSaveFile( hBitmap, cFileName, BT_FILEFORMAT_PNG )

      MsgInfo( "Save as: " + cFileName )

   ENDIF

RETURN
