/*
 * MINIGUI - Harbour Win32 GUI library
 *
*/

#include "minigui.ch"

STATIC oButton, oLbl

FUNCTION Main()

   DEFINE WINDOW MainForm ;
      AT 0, 0 ;
      WIDTH 480 ;
      HEIGHT 200 ;
      TITLE "OnMouseHover Control Event" ;
      MAIN ;
      ON INIT SetCursorPos( GetDesktopWidth()/2, GetDesktopHeight()/2 - 20 ) ;
      ON MOUSEMOVE {|| OnMouseMoveWindow()}

      @ 10, 10 BUTTONEX NUL ;
         WIDTH 150 ;
         HEIGHT 40 ;
         CAPTION "&Exit" ;
         ACTION MainForm.Release() ;
         FONT "Tahoma" SIZE 9 ;
         ON MOUSEHOVER {|| OnMouseHoverButton()}
      oButton := HMG_GetFormControls( ThisWindow.NAME, "OBUTTON" )[1]

      @ 10, 200 LABEL NUL ;
         WIDTH 200 ;
         VALUE "" ;
         TRANSPARENT
      oLbl := HMG_GetFormControls( ThisWindow.NAME, "LABEL" )[1]

      @ 70, 10 LABEL NUL ;
         WIDTH 200 ;
         HEIGHT 100 ;
         VALUE "Move the mouse around the window and watch " + ;
               "what happens when you hover the button."

      ON KEY ESCAPE ACTION MainForm.Release()
   END WINDOW

   CENTER WINDOW MainForm
   ACTIVATE WINDOW MainForm

RETURN Nil

FUNCTION OnMouseMoveWindow

   this.(oLbl).Value := "Form - Row " + ;
                   hb_ntos(_HMG_MouseRow) + ;
                   " Col " + hb_ntos(_HMG_MouseCol)
   IF this.(oButton).FontSize != 9
      this.(oButton).FontSize := 9
   ENDIF

RETURN Nil

FUNCTION OnMouseHoverButton

   this.(oLbl).Value := ""
   this.(oButton).FontSize := 18

RETURN Nil

/*
 * EOF
 */
