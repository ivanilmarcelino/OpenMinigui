/*
 * MINIGUI - Harbour Win32 GUI library Demo
*/

#include "minigui.ch"

FUNCTION Main

   DEFINE WINDOW Form1 ;
         AT 0, 0 ;
         WIDTH 300 ;
         HEIGHT 300 ;
         TITLE "MiniGUI Demo" ;
         MAIN ;
         NOMAXIMIZE NOSIZE

      @ 5, 10 FRAME Frame1 CAPTION "Frame" WIDTH 200 HEIGHT 150 FONTCOLOR BLACK

      @ 30, 20 LABEL Label1 VALUE "User Name" AUTOSIZE
      @ 55, 20 TEXTBOX TextBox1 VALUE ""

      Form1.Label1.Cargo := "TextBox1" // f.e., store the TextBox name in the label's cargo

      @ 90, 20 LABEL Label2 VALUE "Computer Name" AUTOSIZE
      @ 115, 20 TEXTBOX TextBox2 VALUE ""

      Form1.Label2.Cargo := "TextBox2"

      @ 180, 20 BUTTON Button1 CAPTION "Get Names" ON CLICK _GetNames()
      @ 220, 20 BUTTON Button2 CAPTION "Set Names" ON CLICK _SetNames()

      @ 180, 150 BUTTON Button3 CAPTION "Disable Frame" ON CLICK DisableFrame()
      @ 220, 150 BUTTON Button4 CAPTION "Enable Frame" ON CLICK EnableFrame()

   END WINDOW

   Form1.Frame1.Cargo := { "Label1", "TextBox1", "Label2", "TextBox2" } // store the Label and TextBox names in the frame's cargo

   CENTER WINDOW Form1

   ACTIVATE WINDOW Form1

RETURN NIL


STATIC FUNCTION _GetNames()

   Form1.TextBox1.Value := GetUserName()
   Form1.TextBox2.Value := GetComputerName()

RETURN NIL


STATIC FUNCTION _SetNames()

   LOCAL cBoxName1, cBoxName2

   cBoxName1 := Form1.Label1.Cargo // to take the TextBox name from the label's cargo
   cBoxName2 := Form1.Label2.Cargo

   Form1.&( cBoxName1 ).Value := GetUserName()
   Form1.&( cBoxName2 ).Value := GetComputerName()

RETURN NIL


STATIC FUNCTION DisableFrame()

   LOCAL aControls, i

   Form1.Frame1.Enabled := .F.

   aControls := Form1.Frame1.Cargo // array of child control's names in this frame

   FOR i := 1 TO Len( aControls )
      SetProperty( "Form1", aControls[ i ], "Enabled", .F. )
   NEXT

RETURN NIL


STATIC FUNCTION EnableFrame()

   LOCAL aControls, i

   Form1.Frame1.Enabled := .T.

   aControls := Form1.Frame1.Cargo

   FOR i := 1 TO Len( aControls )
      SetProperty( "Form1", aControls[ i ], "Enabled", .T. )
   NEXT

RETURN NIL
