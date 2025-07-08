/*

  Owner buttons demonstration

*/

#include "hmg.ch"
#include "i_winuser.ch"

#define IDC_BTN_1   1001
#define IDC_BTN_2   1002
#define IDC_BTN_3   1003
#define IDC_BTN_4   1004
#define IDC_BTN_5   1005
#define IDC_BTN_YES 1101
#define IDC_BTN_NO  1102


FUNCTION Main()
  LOCAL aBtnColor := {{0xFF0000, 0x00FFFF, 0x000000}, NIL, {0x0000FF, 0x00FF00, 0xFF000000}}
  LOCAL aBtnFont  := {"Arial", 9, .F., .F., .F., .F.}

  DEFINE WINDOW Form1;
    MAIN;
    WIDTH  380;
    HEIGHT 440;
    TITLE  "Owner buttons";
    NOSIZE;
    NOMAXIMIZE;
    NOMINIMIZE;
    ON INIT SetFocusToButton("Form1", IDC_BTN_5)

    DEFINE MAINMENU
      DEFINE POPUP "Button &1"
        MENUITEM "Move"           ACTION MoveButton("Form1", IDC_BTN_1)
        MENUITEM "Resize"         ACTION ResizeButton("Form1", IDC_BTN_1)
        SEPARATOR
        MENUITEM "Show"           ACTION ShowButton("Form1", IDC_BTN_1, .T.)
        MENUITEM "Hide"           ACTION ShowButton("Form1", IDC_BTN_1, .F.)
        SEPARATOR
        MENUITEM "Enable"         ACTION EnableButton("Form1", IDC_BTN_1, .T.)
        MENUITEM "Disable"        ACTION EnableButton("Form1", IDC_BTN_1, .F.)
        SEPARATOR
        MENUITEM "Change shape"   ACTION ChangeButtonShape("Form1", IDC_BTN_1)
        MENUITEM "Change caption" ACTION ChangeButtonCaption("Form1", IDC_BTN_1)
        MENUITEM "Change font"    ACTION ChangeButtonFont("Form1", IDC_BTN_1)
        SEPARATOR
        MENUITEM "Set focus"      ACTION SetFocusToButton("Form1", IDC_BTN_1)
        MENUITEM "Release"        ACTION ReleaseButton("Form1", IDC_BTN_1)
      END POPUP
      DEFINE POPUP "Button &2"
        MENUITEM "Move"           ACTION MoveButton("Form1", IDC_BTN_2)
        MENUITEM "Resize"         ACTION ResizeButton("Form1", IDC_BTN_2)
        SEPARATOR
        MENUITEM "Show"           ACTION ShowButton("Form1", IDC_BTN_2, .T.)
        MENUITEM "Hide"           ACTION ShowButton("Form1", IDC_BTN_2, .F.)
        SEPARATOR
        MENUITEM "Enable"         ACTION EnableButton("Form1", IDC_BTN_2, .T.)
        MENUITEM "Disable"        ACTION EnableButton("Form1", IDC_BTN_2, .F.)
        SEPARATOR
        MENUITEM "Change shape"   ACTION ChangeButtonShape("Form1", IDC_BTN_2)
        MENUITEM "Change caption" ACTION ChangeButtonCaption("Form1", IDC_BTN_2)
        MENUITEM "Change font"    ACTION ChangeButtonFont("Form1", IDC_BTN_2)
        SEPARATOR
        MENUITEM "Set focus"      ACTION SetFocusToButton("Form1", IDC_BTN_2)
        MENUITEM "Release"        ACTION ReleaseButton("Form1", IDC_BTN_2)
      END POPUP
      DEFINE POPUP "Button &3"
        MENUITEM "Move"           ACTION MoveButton("Form1", IDC_BTN_3)
        MENUITEM "Resize"         ACTION ResizeButton("Form1", IDC_BTN_3)
        SEPARATOR
        MENUITEM "Show"           ACTION ShowButton("Form1", IDC_BTN_3, .T.)
        MENUITEM "Hide"           ACTION ShowButton("Form1", IDC_BTN_3, .F.)
        SEPARATOR
        MENUITEM "Enable"         ACTION EnableButton("Form1", IDC_BTN_3, .T.)
        MENUITEM "Disable"        ACTION EnableButton("Form1", IDC_BTN_3, .F.)
        SEPARATOR
        MENUITEM "Change shape"   ACTION ChangeButtonShape("Form1", IDC_BTN_3)
        MENUITEM "Change caption" ACTION ChangeButtonCaption("Form1", IDC_BTN_3)
        MENUITEM "Change font"    ACTION ChangeButtonFont("Form1", IDC_BTN_3)
        SEPARATOR
        MENUITEM "Set focus"      ACTION SetFocusToButton("Form1", IDC_BTN_3)
        MENUITEM "Release"        ACTION ReleaseButton("Form1", IDC_BTN_3)
      END POPUP
      DEFINE POPUP "Button &4"
        MENUITEM "Move"           ACTION MoveButton("Form1", IDC_BTN_4)
        MENUITEM "Resize"         ACTION ResizeButton("Form1", IDC_BTN_4)
        SEPARATOR
        MENUITEM "Show"           ACTION ShowButton("Form1", IDC_BTN_4, .T.)
        MENUITEM "Hide"           ACTION ShowButton("Form1", IDC_BTN_4, .F.)
        SEPARATOR
        MENUITEM "Enable"         ACTION EnableButton("Form1", IDC_BTN_4, .T.)
        MENUITEM "Disable"        ACTION EnableButton("Form1", IDC_BTN_4, .F.)
        SEPARATOR
        MENUITEM "Change shape"   ACTION ChangeButtonShape("Form1", IDC_BTN_4)
        MENUITEM "Change caption" ACTION ChangeButtonCaption("Form1", IDC_BTN_4)
        MENUITEM "Change font"    ACTION ChangeButtonFont("Form1", IDC_BTN_4)
        SEPARATOR
        MENUITEM "Set focus"      ACTION SetFocusToButton("Form1", IDC_BTN_4)
        MENUITEM "Release"        ACTION ReleaseButton("Form1", IDC_BTN_4)
      END POPUP
      DEFINE POPUP "Button &5"
        MENUITEM "Move"           ACTION MoveButton("Form1", IDC_BTN_5)
        MENUITEM "Resize"         ACTION ResizeButton("Form1", IDC_BTN_5)
        SEPARATOR
        MENUITEM "Show"           ACTION ShowButton("Form1", IDC_BTN_5, .T.)
        MENUITEM "Hide"           ACTION ShowButton("Form1", IDC_BTN_5, .F.)
        SEPARATOR
        MENUITEM "Enable"         ACTION EnableButton("Form1", IDC_BTN_5, .T.)
        MENUITEM "Disable"        ACTION EnableButton("Form1", IDC_BTN_5, .F.)
        SEPARATOR
        MENUITEM "Change shape"   ACTION ChangeButtonShape("Form1", IDC_BTN_5)
        MENUITEM "Change caption" ACTION ChangeButtonCaption("Form1", IDC_BTN_5)
        MENUITEM "Change font"    ACTION ChangeButtonFont("Form1", IDC_BTN_5)
        SEPARATOR
        MENUITEM "Set focus"      ACTION SetFocusToButton("Form1", IDC_BTN_5)
        MENUITEM "Release"        ACTION ReleaseButton("Form1", IDC_BTN_5)
      END POPUP
      DEFINE POPUP "&All buttons"
        MENUITEM "Change color"   ACTION ChangeButtonsColor("Form1")
      END POPUP
    END MENU
  END WINDOW

  OBTN_Create("Form1", IDC_BTN_1, "button &1",                     20, 20, 120,  30, .T., .T., .T.,  0, aBtnColor, aBtnFont)
  OBTN_Create("Form1", IDC_BTN_2, "button &2",                     70, 20, 120,  30, .T., .T., .T.,  8, aBtnColor, aBtnFont)
  OBTN_Create("Form1", IDC_BTN_3, "button &3",                    120, 20, 120,  30, .T., .T., .T., 30, aBtnColor, aBtnFont)
  OBTN_Create("Form1", IDC_BTN_4, e"button &4\nmultiline",        170, 20, 120,  60, .T., .T., .T.,  8, aBtnColor, aBtnFont)
  OBTN_Create("Form1", IDC_BTN_5, e"button &5\ncircular\n<quit>", 250, 20, 120, 120, .T., .T., .T., -1, aBtnColor, aBtnFont)

  InstallEventHandler("EventHandler")

  Form1.CENTER
  Form1.ACTIVATE

RETURN NIL


FUNCTION EventHandler(nHWnd, nMsg, nWParam, nLParam)
  LOCAL nID

  IF nHWnd == Form1.HANDLE
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
        CASE IDCANCEL
          MessageBoxQuit()
          EXIT
      ENDSWITCH

    ELSEIF nMsg == WM_CLOSE
      MessageBoxQuit()
      RETURN 1
    ENDIF

  ELSEIF IsWindowDefined("Form2") .and. (nHWnd == GetProperty("Form2", "HANDLE"))
    IF nMsg == WM_DRAWITEM
      IF (nWParam == IDC_BTN_YES) .or. (nWParam == IDC_BTN_NO)
        OBTN_Draw(nHWnd, nWParam, nLParam)
      ENDIF

    ELSEIF nMsg == WM_COMMAND
      nID := LoWord(nWParam)

      IF nID == IDOK
        nID := GetDlgCtrlID(GetFocus())
      ENDIF

      SWITCH nID
        CASE IDC_BTN_YES
          Form1.RELEASE
          EXIT
        CASE IDC_BTN_NO
        CASE IDCANCEL
          DoMethod("Form2", "RELEASE")
          EXIT
      ENDSWITCH
    ENDIF
  ENDIF

RETURN NIL


FUNCTION MoveButton(cForm, nID)
  LOCAL aPos

  IF OBTN_Handle(cForm, nID) == 0
    MsgBox("Button does not exist!")
  ELSE
    aPos    := OBTN_Pos(cForm, nID)
    aPos[2] := If(aPos[2] == 20, 210, 20)
    OBTN_Pos(cForm, nID, aPos[1], aPos[2], aPos[3], aPos[4])
  ENDIF

RETURN NIL


FUNCTION ResizeButton(cForm, nID)
  LOCAL aPos

  IF OBTN_Handle(cForm, nID) == 0
    MsgBox("Button does not exist!")
  ELSE
    aPos    := OBTN_Pos(cForm, nID)
    aPos[3] := If(aPos[3] == 120, 140, 120)
    OBTN_Pos(cForm, nID, aPos[1], aPos[2], aPos[3], aPos[4])
  ENDIF

RETURN NIL


FUNCTION ShowButton(cForm, nID, lShow)

  IF OBTN_Handle(cForm, nID) == 0
    MsgBox("Button does not exist!")
  ELSE
    OBTN_Visible(cForm, nID, lShow)
  ENDIF

RETURN NIL


FUNCTION EnableButton(cForm, nID, lShow)

  IF OBTN_Handle(cForm, nID) == 0
    MsgBox("Button does not exist!")
  ELSE
    OBTN_Enable(cForm, nID, lShow)
  ENDIF

RETURN NIL


FUNCTION ChangeButtonShape(cForm, nID)
  LOCAL nShape

  IF OBTN_Handle(cForm, nID) == 0
    MsgBox("Button does not exist!")
  ELSE
    nShape := OBTN_Shape(cForm, nID)

    SWITCH nID
      CASE IDC_BTN_1
        nShape := If(nShape == 0, 8, 0)
        EXIT
      CASE IDC_BTN_2
        nShape := If(nShape == 0, 8, 0)
        EXIT
      CASE IDC_BTN_3
        nShape := If(nShape == 0, 30, 0)
        EXIT
      CASE IDC_BTN_4
        nShape := If(nShape == 0, 8, 0)
        EXIT
      CASE IDC_BTN_5
        nShape := If(nShape == 0, -1, 0)
        EXIT
    ENDSWITCH

    OBTN_Shape(cForm, nID, nShape, .T.)

  ENDIF

RETURN NIL


FUNCTION ChangeButtonCaption(cForm, nID)
  LOCAL cCaption

  IF OBTN_Handle(cForm, nID) == 0
    MsgBox("Button does not exist!")
  ELSE
    cCaption := OBTN_Caption(cForm, nID)
    OBTN_Caption(cForm, nID, If(HMG_IsLower(cCaption), HMG_Upper(cCaption), HMG_Lower(cCaption)))
  ENDIF

RETURN NIL


FUNCTION ChangeButtonFont(cForm, nID)
  LOCAL aFont

  IF OBTN_Handle(cForm, nID) == 0
    MsgBox("Button does not exist!")
  ELSE
    aFont    := OBTN_Font(cForm, nID)
    aFont[2] := If(aFont[2] == 9, 11, 9)
    aFont[3] := If(aFont[3], .F., .T.)

    OBTN_Font(cForm, nID, aFont, .T.)
  ENDIF

RETURN NIL


FUNCTION SetFocusToButton(cForm, nID)

  IF OBTN_Handle(cForm, nID) == 0
    MsgBox("Button does not exist!")
  ELSEIF ! OBTN_Visible(cForm, nID)
    MsgBox("Button is hidden!")
  ELSEIF ! OBTN_Enable(cForm, nID)
    MsgBox("Button is disabled!")
  ELSE
    OBTN_Focus(cForm, nID, .T.)
  ENDIF

RETURN NIL


FUNCTION ReleaseButton(cForm, nID)

  IF OBTN_Handle(cForm, nID) == 0
    MsgBox("Button does not exist!")
  ELSE
    OBTN_Release(cForm, nID)
  ENDIF

RETURN NIL


FUNCTION ChangeButtonsColor(cForm)
  LOCAL nID
  LOCAL aColor

  FOR nID := IDC_BTN_1 TO IDC_BTN_5
    IF OBTN_Handle(cForm, nID) != 0
      aColor := OBTN_Color(cForm, nID)

      IF aColor[1][1] == 0xFF0000
        aColor := {{0x0000FF, 0xFFC040, 0x000000}, {0x404040, 0X808080, 0x404040}, {0x00FF00, 0x0000FF, 0xFF000000}}
      ELSE
        aColor := {{0xFF0000, 0x00FFFF, 0x000000}, {0x808080, 0XE0E0E0, 0x808080}, {0x0000FF, 0x00FF00, 0xFF000000}}
      ENDIF

      OBTN_Color(cForm, nID, aColor, .T.)
    ENDIF
  NEXT

  IF ValType(aColor) == "U"
    MsgBox("All buttons are released!")
  ENDIF

RETURN NIL


FUNCTION MessageBoxQuit()
  LOCAL aBtnFont := {"Arial", 9, .T., .F., .F., .F.}

  DEFINE WINDOW Form2;
    WIDTH  200;
    HEIGHT 120;
    TITLE  "Owner buttons";
    MODAL;
    NOSIZE

    DEFINE LABEL Msg_LA
      ROW     15
      COL     15
      WIDTH  170
      HEIGHT  13
      VALUE  "Do you want to quit program?"
    END LABEL
  END WINDOW

  OBTN_Create("Form2", IDC_BTN_YES, "Yes", 50,  10, 80,  26, .T., .T., .T.,  8, {{0x0000FF, 0xFFFFFF, 0x000000}, NIL, {0x0000FF, 0xFFFFFF, 0x0080FF}}, aBtnFont)
  OBTN_Create("Form2", IDC_BTN_NO,  "No",  50, 100, 80,  26, .T., .T., .T.,  8, {{0x008000, 0xFFFFFF, 0x000000}, NIL, {0x008000, 0xFFFFFF, 0x00FF00}}, aBtnFont)

  CENTER WINDOW Form2 IN Form1
  Form2.ACTIVATE

RETURN NIL
