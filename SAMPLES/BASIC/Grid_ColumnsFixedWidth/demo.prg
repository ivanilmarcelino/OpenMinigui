/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 */

#include "minigui.ch"

FUNCTION Main()

   LOCAL aMinMaxInfo := {}, i, nWidth

   DEFINE WINDOW Main_WA ;
         MAIN ;
         CLIENTAREA 440, 206 ;
         TITLE 'Grid Columns With Fixed Width' ;
         MINWIDTH 456 - iif( IsThemed(), 0, 2 * GetBorderWidth() ) ;
         MINHEIGHT 244 - iif( IsThemed(), 0, 2 * GetBorderHeight() )

      DEFINE GRID Users_GR
         ROW 10
         COL 10
         WIDTH 420
         HEIGHT 120
         WIDTHS { 130, 140, 70 }
         ITEMS { { 'John', 'Brown', '37' }, { 'Peter', 'Green', '29' }, { 'Eric', 'Pink', '45' } }
         JUSTIFY { GRID_JTFY_LEFT, GRID_JTFY_LEFT, GRID_JTFY_RIGHT }
         CELLNAVIGATION .F.
         LOCKCOLUMNS 1
      END GRID

      DEFINE BUTTON SetText_BU
         ROW 140
         COL 160
         WIDTH 130
         HEIGHT 28
         CAPTION 'Change header text'
         ACTION ( SetHeaderText(), Main_WA.Users_GR.SETFOCUS )
      END BUTTON

      DEFINE STATUSBAR
         STATUSITEM hb_ntos( Main_WA.Users_GR.ColumnWIDTH( 1 ) )
         STATUSITEM hb_ntos( Main_WA.Users_GR.ColumnWIDTH( 2 ) ) WIDTH 155
         STATUSITEM hb_ntos( Main_WA.Users_GR.ColumnWIDTH( 3 ) ) WIDTH 140
      END STATUSBAR
   END WINDOW

   FOR i := 1 TO Main_WA.Users_GR.COLUMNCOUNT
      nWidth := Main_WA.Users_GR.COLUMNWIDTH( i )
      AAdd( aMinMaxInfo, { nWidth, nWidth } )
   NEXT

   Main_WA.Users_GR.COLUMNWIDTHLIMITS := aMinMaxInfo
   Main_WA.Users_GR.VALUE := 1

   Main_WA.CENTER
   Main_WA.ACTIVATE

RETURN NIL


FUNCTION SetHeaderText()

   STATIC nType := 0

   IF nType == 0
      Main_WA.Users_GR.Header( 1 ) := 'First name'
      Main_WA.Users_GR.Header( 2 ) := 'Last name'
      Main_WA.Users_GR.Header( 3 ) := 'Age'
      nType := 1
   ELSE
      Main_WA.Users_GR.Header( 1 ) := 'Column 1'
      Main_WA.Users_GR.Header( 2 ) := 'Column 2'
      Main_WA.Users_GR.Header( 3 ) := 'Column 3'
      nType := 0
   ENDIF

RETURN NIL
