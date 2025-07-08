/*
 * HMG - Harbour Win32 GUI library Demo
 *
 * Copyright 2014-2021 Grigory Filatov <gfilatov@inbox.ru>
*/

#include "minigui.ch"

FUNCTION Main

   DEFINE WINDOW Win_1 ;
      AT 0, 0 ;
      WIDTH 450 HEIGHT 340 ;
      TITLE 'Rating Test' ;
      ICON 'star.ico' ;
      MAIN ;
      ON RELEASE _ReleaseRating( 'Win_1', 'Rate_1' ) ;
      BACKCOLOR WHITE

   DEFINE MAINMENU
      DEFINE POPUP "File"
         MENUITEM "Get Value" ONCLICK MsgInfo( Win_1.Rate_1.Value )
         MENUITEM "Set Value" ONCLICK ( Win_1.Rate_1.Value := 5, RefreshRating( 'Win_1', 'Rate_1' ) )
         SEPARATOR
         MENUITEM "Hide Rating" ONCLICK HideRating( 'Win_1', 'Rate_1' )
         MENUITEM "Show Rating" ONCLICK ShowRating( 'Win_1', 'Rate_1' )
         SEPARATOR
         MENUITEM "Exit" ONCLICK ThisWindow.Release()
      END POPUP
   END MENU

   DEFINE RATING Rate_1
      ROW 100
      COL 100
      WIDTH 20
      HEIGHT 20
      STARS 10
      VALUE 0
      BORDER .T.
      ON CHANGE {|pressed| SetProperty( 'Win_1', 'Label_1', 'Value', hb_ntos( pressed * 10 ) + " %" ) }
   END RATING

   DEFINE LABEL Label_1
      ROW 100
      COL 310
      WIDTH 82
      HEIGHT 20
      VALUE "0 %"
      VCENTERALIGN .T.
      TRANSPARENT .T.
   END LABEL

   DEFINE BUTTON Button_1
      ROW 140
      COL 100
      WIDTH 200
      HEIGHT 28
      CAPTION "Clear Rating"
      ACTION ( ClearRating( 'Win_1', 'Rate_1' ), Win_1.Rate_1.Value := 0, Win_1.Label_1.Value := "0 %" )
      FLAT .T.
   END BUTTON

   END WINDOW

   Win_1.Center()
   ACTIVATE WINDOW Win_1

RETURN NIL


FUNCTION HideRating( cWindow, cControl )

   LOCAL i, img_name
   LOCAL nCount := GetControlId ( cControl, cWindow )

   Win_1.Label_1.Hide

   FOR i := 1 TO nCount
      img_name := cWindow + "_" + cControl + "_" + hb_ntos( i )
      DoMethod( cWindow, img_name, 'Hide' )
   NEXT

   EraseWindow( cWindow )

RETURN NIL


FUNCTION ShowRating( cWindow, cControl )

   LOCAL i, img_name, x, y, h, nSpace, col
   LOCAL nCount := GetControlId ( cControl, cWindow )

   Win_1.Label_1.Show

   FOR i := 1 TO nCount
      img_name := cWindow + "_" + cControl + "_" + hb_ntos( i )
      DoMethod( cWindow, img_name, 'Show' )
   NEXT

   y := GetProperty( cWindow, cWindow + "_" + cControl + "_1", 'Row' )
   x := GetProperty( cWindow, cWindow + "_" + cControl + "_1", 'Col' )
   h := GetProperty( cWindow, cWindow + "_" + cControl + "_1", 'Height' )
   nSpace := 0
   col := GetProperty( cWindow, cWindow + "_" + cControl + "_10", 'Col' ) + ;
      GetProperty( cWindow, cWindow + "_" + cControl + "_1", 'Width' )

   DRAW RECTANGLE ;
      IN WINDOW &cWindow ;
      AT y - 1, x - 1 ;
      TO y + h + 1, col - nSpace + 1 ;
      PENCOLOR { 192, 192, 192 }

RETURN NIL
