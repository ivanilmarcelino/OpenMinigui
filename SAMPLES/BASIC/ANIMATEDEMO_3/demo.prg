/*
 * Harbour MiniGUI Animation Demo
 */

#include "minigui.ch"
#include "Directry.ch"

PROCEDURE Main()

   DEFINE WINDOW Win_1 ;
         CLIENTAREA 220, 130 ;
         TITLE 'Animation Demo' ;
         ICON "MAIN.ICO" ;
         WINDOWTYPE MAIN ;
         ON INIT Image_OnInit( This.Name )

      DEFINE MAIN MENU
         DEFINE POPUP 'Info'
            ITEM 'About ..' ACTION MsgInfo( 'Animation Demo' )
            SEPARATOR
            ITEM 'Exit' ACTION Win_1.RELEASE
         END POPUP
      END MENU

      Win_1.MinButton := .F.
      Win_1.MaxButton := .F.
      Win_1.ClientHeight := ( Win_1.ClientHeight ) + GetMenuBarHeight()

      DEFINE IMAGE Image_1
         ROW 15
         COL 15
         WIDTH -1
         HEIGHT -1
         PICTURE NIL
         STRETCH .T.
      END IMAGE

      DEFINE IMAGE Image_2
         ROW 5
         COL 100
         WIDTH -1
         HEIGHT -1
         PICTURE NIL
         STRETCH .T.
      END IMAGE

   END WINDOW

   CENTER WINDOW Win_1
   ACTIVATE WINDOW Win_1

RETURN

#define IMG_PATH                'Images\'

PROCEDURE Image_OnInit( cWin )

   LOCAL aImgFiles
   STATIC aFrames := {}, aImages := {}, i1 := 1, i2 := 1

   aImgFiles := Directory( IMG_PATH + 'WolkingMan?.png' )
   AEval( aImgFiles, {| elem | AAdd( aFrames, ( IMG_PATH + elem[ F_NAME ] ) ) } )

   DEFINE TIMER Timer_1
      PARENT &cWin
      INTERVAL 60
      ACTION ( Win_1.Image_1.PICTURE := aFrames[ i1++ ], i1 := iif( i1 > Len( aFrames ), 1, i1 ) )
   END TIMER

   aImgFiles := Directory( IMG_PATH + 'User?.png' )
   AEval( aImgFiles, {| elem | AAdd( aImages, ( IMG_PATH + elem[ F_NAME ] ) ) } )

   DEFINE TIMER Timer_2
      PARENT &cWin
      INTERVAL 80
      ACTION ( Win_1.Image_2.PICTURE := aImages[ i2++ ], i2 := iif( i2 > Len( aImages ), 1, i2 ) )
   END TIMER

RETURN
