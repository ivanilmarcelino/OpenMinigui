/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 */

#include "minigui.ch"

FUNCTION MAIN

   SET TOOLTIPBALLOON ON

   DEFINE WINDOW Form_1 ;
      AT 0, 0 ;
      WIDTH 640 HEIGHT 480 ;
      TITLE 'MiniGUI ToolBar ImageList Demo' ;
      MAIN

   DEFINE STATUSBAR
      STATUSITEM 'HMG Power Ready!' DEFAULT
   END STATUSBAR

   DEFINE MAIN MENU
      DEFINE POPUP '&Project'
         MENUITEM "&New..." ACTION MsgInfo( "New" ) MESSAGE "New"
         MENUITEM "&Open..." ACTION MsgInfo( "Open" ) MESSAGE "Open"
         SEPARATOR
         MENUITEM "&Exit..." ACTION MsgInfo( "End" ) MESSAGE "Exit"
      END POPUP
      DEFINE POPUP '&Edit'
         MENUITEM "&Search..." ACTION MsgInfo( "Search" )
         MENUITEM "&Print..." ACTION MsgInfo( "Print" )
      END POPUP
      DEFINE POPUP '&Utilities'
         MENUITEM "&Disable button..." ACTION Form_1.Button_3.Enabled := .F.
         MENUITEM "&Enable button..." ACTION Form_1.Button_3.Enabled := .T.
      END POPUP
   END MENU

   // First we build an ImageList with all the bitmaps 
   DEFINE IMAGELIST ImageList_1 ;
      BUTTONSIZE 32, 32 ;
      IMAGE {} ;
      MASK // it is needed for transparent bitmaps look

   ADD MASKEDIMAGE "new"      COLOR { 255, 0, 255 } TO ImageList_1 OF Form_1
   ADD MASKEDIMAGE "open"     COLOR { 255, 0, 255 } TO ImageList_1 OF Form_1
   ADD MASKEDIMAGE "check"    COLOR { 255, 0, 255 } TO ImageList_1 OF Form_1
   ADD MASKEDIMAGE "search"   COLOR { 255, 0, 255 } TO ImageList_1 OF Form_1
   ADD MASKEDIMAGE "print"    COLOR { 255, 0, 255 } TO ImageList_1 OF Form_1
   ADD MASKEDIMAGE "internet" COLOR { 255, 0, 255 } TO ImageList_1 OF Form_1
   ADD MASKEDIMAGE "keys"     COLOR { 255, 0, 255 } TO ImageList_1 OF Form_1
   ADD MASKEDIMAGE "quit"     COLOR { 255, 0, 255 } TO ImageList_1 OF Form_1

   // Now we create the toolbar and add the buttons
   
   DEFINE TOOLBAREX ToolBar_1 BUTTONSIZE 75, 48 IMAGELIST 'ImageList_1' FLAT BORDER
   
   BUTTON Button_1  ;
      CAPTION '&New project' ;
      PICTUREINDEX 0 ;
      TOOLTIP 'New' ;
      ACTION Form_1.Button_1.Caption := "Hello" // Modify first button text

   BUTTON Button_2  ;
      CAPTION '&Open project' ;
      PICTUREINDEX 1 ;
      TOOLTIP 'Open' ;
      ACTION Form_1.Button_2.Value := .T. ;
      CHECK ;
      SEPARATOR

   BUTTON Button_3  ;
      CAPTION '&Menu' ;
      PICTUREINDEX 2 ;
      TOOLTIP 'Menu' ;
      ACTION MsgInfo( "Menu" ) DROPDOWN

   DEFINE DROPDOWN MENU BUTTON Button_3
      MENUITEM "One"   ACTION MsgInfo( "One" )
      MENUITEM "Two"   ACTION MsgInfo( "Two" )
      MENUITEM "Three" ACTION MsgInfo( "Three" )
   END MENU

   BUTTON Button_4  ;
      CAPTION '&Search' ;
      PICTUREINDEX 3 ;
      TOOLTIP 'Search' ;
      ACTION Form_1.Button_2.Value := .F. // MsgInfo( "Search" )
   
   BUTTON Button_5  ;
      CAPTION '&Print a report' ;
      PICTUREINDEX 4 ;
      TOOLTIP 'Print a report' ;
      ACTION MsgInfo( "Print" ) ;
      SEPARATOR

   BUTTON Button_6  ;
      CAPTION 'Up&grade' ;
      PICTUREINDEX 5 ;
      TOOLTIP 'Search for new versions' ;
      ACTION MsgInfo( "Upgrade" )
   
   BUTTON Button_7  ;
      CAPTION '&Users' ;
      PICTUREINDEX 6 ;
      TOOLTIP 'Users management' ;
      ACTION MsgInfo( "Users" ) ;
      SEPARATOR

   BUTTON Button_8  ;
      CAPTION '&Exit' ;
      PICTUREINDEX 7 ;
      TOOLTIP 'End Application' ;
      ACTION ThisWindow.Release

   END TOOLBAR

   END WINDOW

   MAXIMIZE WINDOW Form_1

   ACTIVATE WINDOW Form_1

RETURN NIL
