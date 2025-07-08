Overview

The code defines a set of functions to create, manage, and draw owner-drawn buttons with custom shapes, colors, and fonts. The buttons can have rounded corners or be elliptical. The implementation includes functions to handle the button's creation, release, position, visibility, enable/disable state, focus, caption, font, shape, color, and drawing.

Key Functions

1. OBTN_Create: Creates an owner-drawn button.
2. OBTN_Release: Releases (destroys) the owner-drawn button.
3. OBTN_Handle: Returns the handle of the owner-drawn button.
4. OBTN_Pos: Gets or sets the position and size of the button.
5. OBTN_Visible: Gets or sets the visibility of the button.
6. OBTN_Enable: Gets or sets the enabled state of the button.
7. OBTN_Focus: Gets or sets the focus state of the button.
8. OBTN_Caption: Gets or sets the caption (text) of the button.
9. OBTN_Font: Gets or sets the font of the button.
10. OBTN_Shape: Gets or sets the shape of the button (rectangle with rounded corners or ellipse).
11. OBTN_Color: Gets or sets the colors of the button (enabled, disabled, focused).
12. OBTN_Draw: Draws the owner-drawn button.

Detailed Explanation

1. OBTN_Create

FUNCTION OBTN_Create(cForm, nID, cCaption, nRow, nCol, nWidth, nHeight, lEnabled, lVisible, lTabStop, nShape, aColor, aFont)
   LOCAL nHButton := _OwnButtonCreate(GetFormHandle(cForm), nID, cCaption, nRow, nCol, nWidth, nHeight, lEnabled, lVisible, lTabStop)
   ...
   RETURN nHButton

- Purpose: Creates an owner-drawn button with specified properties.
- Parameters:
  - cForm: Name of the parent form.
  - nID: ID of the button.
  - cCaption: Caption (text) of the button.
  - nRow, nCol, nWidth, nHeight: Position and size of the button.
  - lEnabled: Enabled state of the button.
  - lVisible: Visibility of the button.
  - lTabStop: Tab stop state of the button.
  - nShape: Shape of the button (rectangle with rounded corners or ellipse).
  - aColor: Colors of the button (enabled, disabled, focused).
  - aFont: Font of the button.
- Returns: Handle of the created button.

2. OBTN_Release

FUNCTION OBTN_Release(cForm, nID)
   LOCAL nHParent := GetFormHandle(cForm)
   LOCAL nHButton := GetDlgItem(nHParent, nID)
   ...
   RETURN NIL

- Purpose: Releases (destroys) the owner-drawn button.
- Parameters:
  - cForm: Name of the parent form.
  - nID: ID of the button.
- Returns: NIL.

3. OBTN_Handle

FUNCTION OBTN_Handle(cForm, nID)
   RETURN GetDlgItem(GetFormHandle(cForm), nID)

- Purpose: Returns the handle of the owner-drawn button.
- Parameters:
  - cForm: Name of the parent form.
  - nID: ID of the button.
- Returns: Handle of the button.

4. OBTN_Pos

FUNCTION OBTN_Pos(cForm, nID, nRow, nCol, nWidth, nHeight)
   LOCAL nHParent := GetFormHandle(cForm)
   LOCAL nHButton := GetDlgItem(nHParent, nID)
   ...
   RETURN OBTN_Pos(cForm, nID)

- Purpose: Gets or sets the position and size of the button.
- Parameters:
  - cForm: Name of the parent form.
  - nID: ID of the button.
  - nRow, nCol, nWidth, nHeight: Position and size of the button.
- Returns: Array with the position and size of the button.

5. OBTN_Visible

FUNCTION OBTN_Visible(cForm, nID, lVisible)
   LOCAL nHButton := GetDlgItem(GetFormHandle(cForm), nID)
   ...
   RETURN IsWindowVisible(nHButton)

- Purpose: Gets or sets the visibility of the button.
- Parameters:
  - cForm: Name of the parent form.
  - nID: ID of the button.
  - lVisible: Visibility of the button.
- Returns: Visibility state of the button.

6. OBTN_Enable

FUNCTION OBTN_Enable(cForm, nID, lEnable)
   LOCAL nHButton := GetDlgItem(GetFormHandle(cForm), nID)
   ...
   RETURN IsWindowEnabled(nHButton)

- Purpose: Gets or sets the enabled state of the button.
- Parameters:
  - cForm: Name of the parent form.
  - nID: ID of the button.
  - lEnable: Enabled state of the button.
- Returns: Enabled state of the button.

7. OBTN_Focus

FUNCTION OBTN_Focus(cForm, nID, lFocus)
   LOCAL nHButton := GetDlgItem(GetFormHandle(cForm), nID)
   ...
   RETURN (GetFocus() == nHButton)

- Purpose: Gets or sets the focus state of the button.
- Parameters:
  - cForm: Name of the parent form.
  - nID: ID of the button.
  - lFocus: Focus state of the button.
- Returns: Focus state of the button.

8. OBTN_Caption

FUNCTION OBTN_Caption(cForm, nID, cCaption)
   LOCAL nHButton := GetDlgItem(GetFormHandle(cForm), nID)
   ...
   RETURN GetWindowText(nHButton)

- Purpose: Gets or sets the caption (text) of the button.
- Parameters:
  - cForm: Name of the parent form.
  - nID: ID of the button.
  - cCaption: Caption (text) of the button.
- Returns: Caption (text) of the button.

9. OBTN_Font

FUNCTION OBTN_Font(cForm, nID, aFont, lRedraw)
   STATIC hFont := { => }
   ...
   RETURN iif(hb_HHasKey(hFont, cForm) .AND. hb_HHasKey(hFont[cForm], nID), hFont[cForm][nID], NIL)

- Purpose: Gets or sets the font of the button.
- Parameters:
  - cForm: Name of the parent form.
  - nID: ID of the button.
  - aFont: Font of the button.
  - lRedraw: Redraw the button if the font is changed.
- Returns: Font of the button.

10. OBTN_Shape

FUNCTION OBTN_Shape(cForm, nID, nShape, lRedraw)
   STATIC hShape := { => }
   ...
   RETURN nShape

- Purpose: Gets or sets the shape of the button (rectangle with rounded corners or ellipse).
- Parameters:
  - cForm: Name of the parent form.
  - nID: ID of the button.
  - nShape: Shape of the button.
  - lRedraw: Redraw the button if the shape is changed.
- Returns: Shape of the button.

11. OBTN_Color

FUNCTION OBTN_Color(cForm, nID, aColor, lRedraw)
   STATIC hColor := { => }
   ...
   RETURN iif(hb_HHasKey(hColor, cForm) .AND. hb_HHasKey(hColor[cForm], nID), hColor[cForm][nID], NIL)

- Purpose: Gets or sets the colors of the button (enabled, disabled, focused).
- Parameters:
  - cForm: Name of the parent form.
  - nID: ID of the button.
  - aColor: Colors of the button.
  - lRedraw: Redraw the button if the colors are changed.
- Returns: Colors of the button.

12. OBTN_Draw

FUNCTION OBTN_Draw(nHParent, nID, nDRAWITEMSTRUCT)
   LOCAL cForm := GetFormNameByIndex(GetFormIndexByHandle(nHParent))
   LOCAL nShape := OBTN_Shape(cForm, nID)
   LOCAL aColor := OBTN_Color(cForm, nID)
   LOCAL aFont := OBTN_Font(cForm, nID)
   ...
   RETURN NIL

- Purpose: Draws the owner-drawn button.
- Parameters:
  - nHParent: Handle of the parent form.
  - nID: ID of the button.
  - nDRAWITEMSTRUCT: DRAWITEMSTRUCT structure.
- Returns: NIL.

Internal Functions

The internal functions _OwnButtonCreate and _OwnButtonDraw are implemented in C and handle the creation and drawing of the owner-drawn button. These functions use the Windows API to create the button and draw it with the specified shape, colors, and font.

Example Usage

Here's an example of how to use these functions to create and manage an owner-drawn button:

#include "hmg.ch"
#include "i_winuser.ch"

#define IDC_BTN_1   1001
#define IDC_BTN_2   1002
#define IDC_BTN_3   1003
#define IDC_BTN_4   1004
#define IDC_BTN_5   1005

PROCEDURE Main()

   // Create the main window
   DEFINE WINDOW MainWin;
      WIDTH 300;
      HEIGHT 200;
      TITLE "Owner-Drawn Button Example";
      MAIN;
      ON INIT SetFocusToButton("MainWin", IDC_BTN_1)

   END WINDOW

   // Create the owner-drawn button
   OBTN_Create("MainWin", IDC_BTN_1, "Rounded Button", 50, 50, 200, 50, .T., .T., .T., 10, { { 0x000000, 0xFFFFFF, 0x000000 }, { 0x808080, 0XE0E0E0, 0x808080 }, { 0x000000, 0xFFFFFF, 0x000000 } }, { "Arial", 12, .F., .F., .F., .F. } )

   InstallEventHandler("EventHandler")

   // Center the main window
   CENTER WINDOW MainWin
   ACTIVATE WINDOW MainWin

RETURN

FUNCTION EventHandler(nHWnd, nMsg, nWParam, nLParam)
  LOCAL nID

  IF nHWnd == MainWin.HANDLE
    IF nMsg == WM_DRAWITEM
      IF (nWParam >= IDC_BTN_1) .and. (nWParam <= IDC_BTN_5)
        OBTN_Draw(nHWnd, nWParam, nLParam)
      ENDIF

    ELSEIF nMsg == WM_COMMAND
      nID := LoWord(nWParam)

      IF nID == IDOK
        nID := GetDlgCtrlID(GetFocus())
      ENDIF

      SWITCH nID
        CASE IDC_BTN_1
          MsgBox("Button 1 was pressed.")
          EXIT
        CASE IDC_BTN_2
          MsgBox("Button 2 was pressed.")
          EXIT
        CASE IDC_BTN_3
          MsgBox("Button 3 was pressed.")
          EXIT
        CASE IDC_BTN_4
          MsgBox("Button 4 was pressed.")
          EXIT
        CASE IDC_BTN_5
      ENDSWITCH
    ENDIF

  ENDIF

RETURN NIL


This example creates a main window with an owner-drawn button that has rounded corners. The button's shape, colors, and font are specified when the button is created. The main loop handles events and keeps the application running.

By following this implementation, you can create and manage owner-drawn buttons with custom shapes, colors, and fonts in Harbour MiniGUI.