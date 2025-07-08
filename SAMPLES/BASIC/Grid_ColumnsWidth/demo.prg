/*
 * MINIGUI - Harbour Win32 GUI library Demo
 * 
 * Try to change a column's width by dragging a divider in the Grid header
 * Demo was contributed to HMG forum by KDJ 13/Nov/2016
 *
 * Adapted for MiniGUI Extended Edition by Grigory Filatov
 */

#include "minigui.ch"

FUNCTION Main()

  LOCAL aMinMaxInfo := {}

  AAdd(aMinMaxInfo, {50, 150})
  AAdd(aMinMaxInfo, {90, 200})
  AAdd(aMinMaxInfo, {30, 90})

  DEFINE WINDOW Main_WA;
    MAIN;
    CLIENTAREA 440, 206;
    TITLE 'Grid Columns Width - see in StatusBar';
    MINWIDTH 456 - iif(IsThemed(), 0, 2*GetBorderWidth());
    MINHEIGHT 244 - iif(IsThemed(), 0, 2*GetBorderHeight())

    DEFINE GRID Users_GR
      ROW            10
      COL            10
      WIDTH          420
      HEIGHT         120
      WIDTHS         { 130, 140, 70 }
      ITEMS          { {'John', 'Brown', '37'}, {'Peter', 'Green', '29'}, {'Eric', 'Pink', '45'} }
      JUSTIFY        { GRID_JTFY_LEFT, GRID_JTFY_LEFT, GRID_JTFY_RIGHT }
      CELLNAVIGATION .F.
      COLUMNWIDTHLIMITS aMinMaxInfo
      ON DRAGHEADERITEMS UpdateStatus()
    END GRID

    DEFINE BUTTON SetText_BU
      ROW      140
      COL      160
      WIDTH    130
      HEIGHT    28
      CAPTION 'Change header text'
      ACTION   SetHeaderText()
    END BUTTON
  
    DEFINE STATUSBAR
      STATUSITEM ''
      STATUSITEM '' WIDTH 155
      STATUSITEM '' WIDTH 140
    END STATUSBAR
  END WINDOW

  UpdateStatus()
  Main_WA.Users_GR.VALUE := 1

  Main_WA.CENTER
  Main_WA.ACTIVATE

RETURN NIL


FUNCTION UpdateStatus()

  Main_WA.STATUSBAR.Item(1) := hb_ntos(Main_WA.Users_GR.ColumnWIDTH(1)) + '  (range 50 - 150)'
  Main_WA.STATUSBAR.Item(2) := hb_ntos(Main_WA.Users_GR.ColumnWIDTH(2)) + '  (range 90 - 200)'
  Main_WA.STATUSBAR.Item(3) := hb_ntos(Main_WA.Users_GR.ColumnWIDTH(3)) + '  (range 30 - 90)'

RETURN NIL


FUNCTION SetHeaderText()

  STATIC nType := 0

  IF nType == 0
    Main_WA.Users_GR.Header(1) := 'First name'
    Main_WA.Users_GR.Header(2) := 'Last name'
    Main_WA.Users_GR.Header(3) := 'Age'
    nType := 1
  ELSE
    Main_WA.Users_GR.Header(1) := 'Column 1'
    Main_WA.Users_GR.Header(2) := 'Column 2'
    Main_WA.Users_GR.Header(3) := 'Column 3'
    nType := 0
  ENDIF

RETURN NIL
