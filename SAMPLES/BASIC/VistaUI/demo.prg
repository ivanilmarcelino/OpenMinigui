/*
 * Program   : DEMO.PRG
 * Aim       : How to use new vista buttons
 * Date      : 08/03/2017
 * Author(s) : Grigory Filatov - MiniGUI Team
 * Copyright : (c) 2017 All Rights Reserved.
 */

#include "minigui.ch"

// ----------------------------------------------------------------------------
FUNCTION Main()
// ----------------------------------------------------------------------------
   LOCAL cWinTitle := "Vista UI Sample"

   // Set tooltip max width of line

   SET TOOLTIP MAXWIDTH TO 128

   // Font object creations

   DEFINE FONT ObjFONT1 FONTNAME "MS Sans Serif" SIZE 28 BOLD
   DEFINE FONT ObjFONT2 FONTNAME "MS Sans Serif" SIZE 12 BOLD

   // Main window creation

   DEFINE WINDOW WndMain ;
      MAIN ;
      WIDTH 442 HEIGHT 300 ;
      TITLE cWinTitle ;
      NOMAXIMIZE NOSIZE ;
      ON INTERACTIVECLOSE MainDoExit()

      @ 005, 020 LABEL lblTitle VALUE "Windows Vista new UI" ;
         WIDTH 410 HEIGHT 35 ;
         FONT "ObjFONT1"

      IF isVistaCompatible()

         @ 60, 100 CLBUTTON NUL WIDTH 250 HEIGHT 80 ;
            CAPTION "Vista DEF_Command Link" ;
            NOTETEXT "Note" ;
            ACTION MsgInfo( This.Name ) ;
            DEFAULT

         @ 145, 100 CLBUTTON NUL WIDTH 250 HEIGHT 40 ;
            CAPTION "Vista Command Link" ;
            NOTETEXT NIL ;
            ACTION MsgInfo( This.Name )

         @ 190, 100 SPLITBUTTON NUL WIDTH 250 HEIGHT 40 ;
            CAPTION "Split Button" ;
            ACTION MsgInfo( This.Name ) ;
            FONT "ObjFONT2" ;
            TOOLTIP "Vista Split Button" + CRLF + ;
              "Multi line tooltip"

         DEFINE DROPDOWN MENU BUTTON ( HMG_GetFormControls( This.Name, "SPBUTTON" )[ 1 ] )
            MENUITEM "Drop Down Menu 1" ACTION MsgInfo( "Button Drop Down Menu 1" )
            MENUITEM "Drop Down Menu 2" ACTION MsgInfo( "Button Drop Down Menu 2" )
         END MENU

      ENDIF

   END WINDOW

   CENTER WINDOW WndMain

   ACTIVATE WINDOW WndMain

RETURN NIL

// ----------------------------------------------------------------------------
FUNCTION MainDoExit()
// ----------------------------------------------------------------------------
   IF MsgYesNo( ;
         "Do you want to quit ?", ;
         "Question" )
      RETURN( .T. ) // Terminate current app.
   ENDIF

RETURN( .F. ) // Do not quit, keep on current task

// ----------------------------------------------------------------------------
FUNCTION IsVistaCompatible()
// ----------------------------------------------------------------------------

RETURN IsWinNT() .AND. IsVistaOrLater()
