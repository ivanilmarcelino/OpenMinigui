#include "minigui.ch"
#include "GraphPlus.ch"

Function Main()
   LOCAL oGraph

   // Create the main application window
   DEFINE WINDOW Win_1 ;
      AT 0, 0 ;
      WIDTH 700 ;
      HEIGHT 700 ;
      TITLE "GraphPlus Example" ;
      MAIN

      // Create an area to display the graph
      DEFINE IMAGE graph
         ROW 10
         COL 30
         WIDTH 625
         HEIGHT 600
         STRETCH .T.
      END IMAGE

      // Create the graph object
      oGraph := GraphPlus():New()

      // Configure the graph properties
      WITH OBJECT oGraph
         :aData := { { 15, 30, 45 }, { 20, 35, 50 } } // Two datasets
         :aCategories := { "Q1", "Q2", "Q3" } // X-axis categories
         :aLegends := { "Product A", "Product B" } // Legends
         :cTitle := "Quarterly Sales Report" // Graph Title
         :cXTitle := "Quarters" // X-axis Title
         :cYTitle := "Sales (in Thousands)" // Y-axis Title
         :nGraphType := GT_COLUMNS // Columns graph
         :nImageHeight := Win_1.graph.Height
         :nImageWidth := Win_1.graph.Width
         :lShowLegends := .T. // Show legends
      END OBJECT

      // Add a button to draw the graph
      @ 620, 240 BUTTON BtnDraw ;
         CAPTION "Draw Graph" ;
         ACTION ( oGraph:Draw(), ;
                  SetProperty( 'Win_1', 'graph', 'HBITMAP', oGraph:Bitmap ) )

      // Add a button to save the graph
      @ 620, 350 BUTTON BtnSave ;
         CAPTION "Save Graph" ;
         ACTION SaveGraph( oGraph )

   END WINDOW

   // Activate the window and start the event loop
   ACTIVATE WINDOW Win_1

Return


FUNCTION SaveGraph( oGraph )

   LOCAL cFileName := PutFile( {{ "PNG Files (*.png)", "*.png" }}, "Save Graph As" )

   IF Empty( cFileName )
      RETURN NIL // User canceled save
   ENDIF

   // Ensure file has .png extension
   IF ! ".png" $ Lower(cFileName)
      cFileName += ".png"
   ENDIF

   oGraph:Save( cFileName )

   MsgInfo( "Graph saved as: " + cFileName )

RETURN NIL
