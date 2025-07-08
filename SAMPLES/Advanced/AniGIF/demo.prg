/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Based upon a sample Minigui\Samples\Advanced\AnimatedGif
 * Author: P.Chornyj <myorg63@mail.ru>
 */

#include "minigui.ch"

FUNCTION Main()

   LOCAL oGif, oGif2, oGif3

   SET MULTIPLE OFF WARNING

   DEFINE WINDOW Form_Main ;
         TITLE 'Gif89 Demo' ;
         MAIN ;
         BACKCOLOR SILVER ;
         NOMAXIMIZE NOSIZE

      DEFINE MAIN MENU

         DEFINE POPUP "&File"

            MENUITEM '&Play' ACTION iif( ! oGif:IsRunning(), oGif:Play(), )
            MENUITEM '&Stop' ACTION iif( oGif:IsRunning(), oGif:Stop(), )
            SEPARATOR
            MENUITEM "E&xit" ACTION ThisWindow.Release()

         END POPUP

         DEFINE POPUP "&?"

            MENUITEM "GIF1 &Info" ACTION ShowInfo( oGif, "1" )
            MENUITEM "GIF2 &Info" ACTION ShowInfo( oGif2, "2" )
            MENUITEM "GIF3 &Info" ACTION ShowInfo( oGif3, "3" )

         END POPUP

      END MENU

      @ 20, 10 ANIGIF Gif_1 OBJ oGif  PARENT Form_Main PICTURE "ani1" WIDTH 128 HEIGHT 128 BACKGROUNDCOLOR WHITE
      @ 20, 10 ANIGIF Gif_2 OBJ oGif2 PARENT Form_Main PICTURE "ani2" WIDTH 128 HEIGHT 128 BACKGROUNDCOLOR WHITE
      @ 20, 10 ANIGIF Gif_3 OBJ oGif3 PARENT Form_Main PICTURE "ani3" WIDTH 128 HEIGHT 128 BACKGROUNDCOLOR WHITE

   END WINDOW

   Form_Main.WIDTH := Max( 520, Form_Main.Gif_1.Width + 2 * GetBorderWidth() + 40 )
   Form_Main.HEIGHT := GetTitleHeight() + Form_Main.Gif_1.Height + 2 * GetBorderHeight() + 50
   Form_Main.Gif_1.Col := ( Form_Main.Width - Form_Main.Gif_1.Width - GetBorderWidth() ) / 4 - 65 + 1
   oGif:Update()
   Form_Main.Gif_2.Col := ( Form_Main.WIDTH - Form_Main.Gif_2.Width - GetBorderWidth() ) / 2 + 1
   oGif2:Update()
   Form_Main.Gif_3.Col := ( Form_Main.WIDTH - Form_Main.Gif_3.Width - GetBorderWidth() ) - 30 + 1
   oGif3:Update()

   DRAW PANEL IN WINDOW Form_Main ;
      AT Form_Main.Gif_1.Row - 2, Form_Main.Gif_1.Col - 2 ;
      TO Form_Main.Gif_1.Row + Form_Main.Gif_1.Height, Form_Main.Gif_1.Col + Form_Main.Gif_1.Width

   DRAW PANEL IN WINDOW Form_Main ;
      AT Form_Main.Gif_2.Row - 2, Form_Main.Gif_2.Col - 2 ;
      TO Form_Main.Gif_2.Row + Form_Main.Gif_2.Height, Form_Main.Gif_2.Col + Form_Main.Gif_2.Width

   DRAW PANEL IN WINDOW Form_Main ;
      AT Form_Main.Gif_3.Row - 2, Form_Main.Gif_3.Col - 2 ;
      TO Form_Main.Gif_3.Row + Form_Main.Gif_3.Height, Form_Main.Gif_3.Col + Form_Main.Gif_3.Width

   CENTER WINDOW Form_Main

   ACTIVATE WINDOW Form_Main

RETURN NIL


PROCEDURE ShowInfo( oObj, c )

   IF oObj:nTotalFrames > 1
      oObj:Stop()
      MsgInfo( ;
         "Picture name"  + Chr( 9 ) + ": " + cFileNoPath( oObj:cFileName ) + CRLF + ;
         "Image Width"   + Chr( 9 ) + ": " + hb_ntos( Form_Main.( "Gif_" + c ).Width ) + CRLF + ;
         "Image Height"  + Chr( 9 ) + ": " + hb_ntos( Form_Main.( "Gif_" + c ).Height ) + CRLF + ;
         "Total Frames"  + Chr( 9 ) + ": " + hb_ntos( oObj:nTotalFrames ) + CRLF + ;
         "Current Frame" + Chr( 9 ) + ": " + hb_ntos( oObj:nCurrentFrame ), ;
         "GIF Info" )
      oObj:Play()
   ENDIF

RETURN
