/*
 * HMG Drawing demo
*/

ANNOUNCE RDDSYS

#include <minigui.ch>

PROCEDURE Main()

   DEFINE WINDOW Draw ;
         AT 0, 0 ;
         WIDTH 600 HEIGHT 600 ;
         TITLE "DRAWING EXAMPLES" ;
         MAIN ;
         NOMAXIMIZE ;
         NOSIZE ;
         ON INIT Draw()

      DEFINE STATUSBAR
      END STATUSBAR

   END WINDOW

   Draw.Center()
   Draw.Activate()

RETURN


FUNCTION draw()

   LOCAL cForm := "draw"

   // Diagonal
   DRAW LINE IN WINDOW draw AT 10, 10 TO 500, 500
   AddDelGraph( cForm )
   showstatus( "LINE DIAGONAL", 1 )

   // rectangle diameter 400
   DRAW RECTANGLE IN WINDOW draw AT 100, 100 TO 500, 500

   // quarter circle quadrant 1 diameter 400
   DRAW ARC IN WINDOW draw AT 100, 100 TO 500, 500 FROM RADIAL 300, 500 TO RADIAL 100, 300
   AddDelGraph( cForm )
   showstatus( "ARC in quadrant 1", 2 )

   // quarter circle quadrant 2 diameter 400
   DRAW ARC IN WINDOW draw AT 100, 100 TO 500, 500 FROM RADIAL 100, 300 TO RADIAL 300, 100
   AddDelGraph( cForm )
   showstatus( "ARC in quadrant 2", 2 )

   // quarter circle quadrant 3 diameter 400
   DRAW ARC IN WINDOW draw AT 100, 100 TO 500, 500 FROM RADIAL 300, 100 TO RADIAL 500, 300
   AddDelGraph( cForm )
   showstatus( "ARC in quadrant 3", 2 )

   // quarter circle quadrant 4 diameter 400
   DRAW ARC IN WINDOW draw AT 100, 100 TO 500, 500 FROM RADIAL 500, 300 TO RADIAL 300, 500
   AddDelGraph( cForm )
   showstatus( "ARC in quadrant 4", 3 )

   // Delete all lines except rectangle and redraw graph
   AddDelGraph( cForm, "D" )
   AddDelGraph( cForm, "R" )

   // demos with line of end points RADIAL, these connected to the center of the circle=center of the rectangle
   // intersect the circle, there are the beginning/end points of the arc

   // DRAW RECTANGLE IN WINDOW draw AT 100,100 TO 500,500
   // diagonals
   draw line IN WINDOW draw AT 100, 100 TO 500, 500
   draw line IN WINDOW draw AT 500, 100 TO 100, 500
   // radial end points of the draw arc
   draw line IN WINDOW draw AT 500, 550 TO 100, 50
   AddDelGraph( cForm )
   // connect end points to the center of the circle
   draw line IN WINDOW draw AT 500, 550 TO 300, 300
   AddDelGraph( cForm )
   draw line IN WINDOW draw AT 100, 50 TO 300, 300
   AddDelGraph( cForm )

   DRAW ARC IN WINDOW draw AT 100, 100 TO 500, 500 FROM RADIAL 500, 550 TO RADIAL 100, 50
   AddDelGraph( cForm )
   showstatus( "ARC with RADIAL-points connected with the center of the RECTANGLE=center of the ARC", 5 )

   // Delete some lines and redraw graph
   AddDelGraph( cForm, "D" )
   AddDelGraph( cForm, "R" )

   // DRAW RECTANGLE IN WINDOW draw AT 100,100 TO 500,500
   // diagonals
   // draw line in window draw at 100,100 to 500,500
   // draw line in window draw at 500,100 to 100,500
   // radial end points of the draw arc
   draw line IN WINDOW draw AT 350, 550 TO 50, 350
   // connect end points to circle center
   draw line IN WINDOW draw AT 350, 550 TO 300, 300
   draw line IN WINDOW draw AT 50, 350 TO 300, 300

   DRAW ARC IN WINDOW draw AT 100, 100 TO 500, 500 FROM RADIAL 350, 550 TO RADIAL 50, 350
   showstatus( "ARC with RADIAL-points connected with the center of the RECTANGLE=center of the ARC", NIL )

RETURN NIL


FUNCTION showstatus( cMsg, nSec )

   draw.statusbar.item( 1 ) := cMsg
   HMG_SysWait( nSec ) // delay in seconds

RETURN NIL


FUNCTION AddDelGraph( window, cAction )

   LOCAL w := GetFormIndex ( window )
   LOCAL nLast := Len( _HMG_aFormGraphTasks[ w ] )
   LOCAL i

   STATIC myGraph := {}

   IF cAction == NIL
      cAction := "A"
   ELSE
      cAction := Upper( Left( cAction, 1 ) )
   ENDIF

   IF _HMG_aFormDeleted[ w ] == .F.

      IF cAction == "A" // add position and identy my graph
         AAdd( myGraph, { nLast, __vmItemID( _HMG_aFormGraphTasks[ w, nLast ] ) } )

      ELSEIF cAction == "R" // redraw all graph
         FOR i := 1 TO Len( _HMG_aFormGraphTasks[ w ] )
            Eval( _HMG_aFormGraphTasks[ w, i ] )
         NEXT

      ELSEIF cAction == "D" // delete my graph from MiniGUI list
         ASort( myGraph,,, {| x, y | ( x[ 1 ] > y[ 1 ] ) } )
         FOR i := 1 TO Len( myGraph )
            IF myGraph[ i, 1 ] <= Len( _HMG_aFormGraphTasks[ w ] )
               IF __vmItemID( _HMG_aFormGraphTasks[ w, myGraph[ i, 1 ] ] ) == myGraph[ i, 2 ]
                  ADel( _HMG_aFormGraphTasks[ w ], myGraph[ i, 1 ], .T. )
               ENDIF
            ENDIF
         NEXT
         ASize( myGraph, 0 )
         RedrawWindow( GetFormHandle( window ) )

      ENDIF

      RETURN .T.

   ENDIF

RETURN .F.
