//
// Copyright 2016 Ashfaq Sial
//

/* To avoid inclusion of <winuser.h> */
#define MF_ENABLED       0
#define MF_DISABLED      2

#define MF_STRING        0
#define MF_POPUP        16
#define MF_SEPARATOR  2048

FUNCTION MainMenu()

   LOCAL oTopBar
   LOCAL oPopUp

   oTopBar := wvw_CreateMenu()

   // File
   oPopUp := wvw_CreatePopupMenu()

   wvw_AppendMenu( oPopUp, MF_ENABLED + MF_STRING, 1101, "Employee Details" )
   wvw_AppendMenu( oPopUp, MF_ENABLED + MF_STRING, 1102, "Radio Buttons" )
   wvw_AppendMenu( oPopUp, MF_SEPARATOR )
   wvw_AppendMenu( oPopUp, MF_ENABLED + MF_STRING, 1103, "Exit" )
   wvw_MenuItemBitmap( oPopUp, 1103, 102 )

   wvw_AppendMenu( oTopBar, MF_ENABLED + MF_POPUP, oPopUp, "&File" )

   // Help
   oPopUp := wvw_CreatePopupMenu()

   wvw_AppendMenu( oPopUp, MF_DISABLED + MF_STRING, 1201, "Check for Updates" )
   wvw_AppendMenu( oPopUp, MF_ENABLED + MF_STRING, 1202, "About" )

   wvw_AppendMenu( oTopBar, MF_ENABLED + MF_POPUP, oPopUp, "&Help" )

   wvw_SetMenu( 0, oTopBar )

   RETURN NIL

// EOF: WVWMENU.PRG
