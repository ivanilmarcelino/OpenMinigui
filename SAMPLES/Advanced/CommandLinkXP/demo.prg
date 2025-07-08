/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * (c) 2016 Grigory Filatov <gfilatov@inbox.ru>
 *
*/

#include "minigui.ch"

STATIC lBorder := .F.

PROCEDURE Main()

   LOCAL aCaptions := { "Add a local printer", ;
                        "Add a network, wireless or Bluetooth printer" }
   LOCAL aNotes := { "Use this option only if you don't have a USB printer. (Windows automatically installs USB printers when you plug them in.)", ;
                     "Make sure that your computer is connected to the network, or that your Bluetooth or wireless printer is turned on." }, ;
      i, cImage, cLabel, cLabel2, nPos := 94

   DEFINE WINDOW Win_1 ;
         AT 0, 0 ;
         WIDTH 615 + iif( ISVISTAORLATER(), 1, 2 ) * GetBorderWidth() ;
         HEIGHT GetTitleHeight() + 416 + GetBorderHeight() ;
         TITLE "Add Printer" ;
         ICON "zzz_PrintIcon" ;
         MAIN NOMINIMIZE NOMAXIMIZE NOSIZE ;
         BACKCOLOR iif( ISVISTAORLATER(), { 233, 236, 216 }, Nil ) ;
         FONT "MS Sans Serif" SIZE 9

      DRAW ICON IN WINDOW Win_1 AT 4, 12 ;
         PICTURE "ZZZ_PRINTICON"

      @ 8, 40 LABEL Label_Title VALUE ThisWindow.TITLE ;
         AUTOSIZE ;
         BOLD ;
         TRANSPARENT

      @ 32, GetBorderWidth() / 2 FRAME Frame_1 CAPTION "" WIDTH Win_1.WIDTH - 2 * GetBorderWidth() HEIGHT Win_1.HEIGHT - 78 - GetTitleHeight()

      @ 50, 36 LABEL Label_Title_2 VALUE "What type of printer do you want to install?" ;
         AUTOSIZE ;
         BOLD ;
         TRANSPARENT

      FOR i := 1 TO Len( aCaptions )

         cImage := "Image_" + Str( i, 1 )
         @ nPos, 48 IMAGE &cImage ;
            PICTURE "arrow.bmp" ;
            WIDTH 16 ;
            HEIGHT 16 ;
            TRANSPARENT ;
            ACTION DoAction( Val( Right( This.NAME, 1 ) ) )

         cLabel := "Button_" + Str( i, 1 )
         @ nPos, 70 LABEL &cLabel VALUE aCaptions[ i ] ;
            WIDTH 500 ;
            HEIGHT 18 ;
            BOLD ;
            TRANSPARENT ;
            ACTION DoAction( Val( Right( This.NAME, 1 ) ) ) ;
            ON MOUSEHOVER iif( lBorder, , CreateBtnBorder( "Win_1", This.ROW - 12, This.COL - 38, This.ROW + This.HEIGHT + 42, This.COL + This.WIDTH + 14 ) ) ;
            ON MOUSELEAVE ( lBorder := .F., AddDelGraph( "Win_1", "D" ), AddDelGraph( "Win_1", "R" ) )

         cLabel2 := "Note_" + Str( i, 1 )
         @ nPos + 18, 70 LABEL &cLabel2 VALUE aNotes[ i ] ;
            WIDTH 500 ;
            HEIGHT 46 ;
            TRANSPARENT ;
            ACTION DoAction( Val( Right( This.NAME, 1 ) ) ) ;
            ON MOUSEHOVER iif( lBorder, , CreateBtnBorder( "Win_1", This.ROW - 12 - 18, This.COL - 38, This.ROW - 4 + This.HEIGHT, This.COL + This.WIDTH + 14 ) ) ;
            ON MOUSELEAVE ( lBorder := .F., AddDelGraph( "Win_1", "D" ), AddDelGraph( "Win_1", "R" ) )

         nPos += 98

      NEXT

      SetWindowCursor( GetControlHandle( "Image_1", "Win_1" ), "MINIGUI_FINGER" )

      @ Win_1.HEIGHT - 38 - GetTitleHeight(), Win_1.WIDTH - 152 BUTTON Button_3 ;
         CAPTION '&Next' ;
         ACTION _dummy() ;
         WIDTH 64 ;
         HEIGHT 23

      Win_1.Button_3.Enabled := .F.

      @ Win_1.HEIGHT - 38 - GetTitleHeight(), Win_1.WIDTH - 82 BUTTON Button_4 ;
         CAPTION '&Cancel' ;
         ACTION ThisWindow.RELEASE ;
         WIDTH 64 ;
         HEIGHT 23

   END WINDOW

   CENTER WINDOW Win_1

   ACTIVATE WINDOW Win_1

RETURN


FUNCTION CreateBtnBorder( cWin, t, l, b, r )

   lBorder := .T.

   Rc_Cursor( "MINIGUI_FINGER" )

   DRAW PANEL ;
      IN WINDOW &cWin ;
      AT t, l ;
      TO b, r

   AddDelGraph( cWin )

RETURN NIL


FUNCTION DoAction( nMode )

   switch nMode

   CASE 1
      MsgInfo( "Click 1" )
      EXIT

   CASE 2

      MsgInfo( "Click 2" )

   END switch

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
