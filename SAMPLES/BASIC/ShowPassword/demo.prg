/*
 * HMG TextBox Password Demo
*/

#include 'hmg.ch'
#include "i_winuser.ch"

FUNCTION Main()

  DEFINE WINDOW Main_WA;
    MAIN;
    ROW    100;
    COL    100;
    WIDTH  225;
    HEIGHT 150;
    TITLE  'TextBox Password Test';
    NOSIZE;
    NOMAXIMIZE;
    NOMINIMIZE

    DEFINE LABEL Pass_LA
      ROW     08
      COL     10
      WIDTH  200
      HEIGHT  16
      VALUE  'Type password:'
    END LABEL

    DEFINE TEXTBOX Pass_TE
      ROW        26
      COL        10
      WIDTH     200
      HEIGHT     21
      VALUE     'my password'
      PASSWORD  .T.
    END TEXTBOX

    DEFINE CHECKBOX Pass_CBO
      ROW       55
      COL       10
      WIDTH    200
      HEIGHT    16
      CAPTION  'Show password'
      ONCHANGE ShowPassword( "Pass_TE", This.VALUE )
    END CHECKBOX

    DEFINE BUTTON OK_BU
      ROW     85
      COL     25
      WIDTH   80
      HEIGHT  25
      CAPTION 'OK'
      ACTION  Main_WA.RELEASE
    END BUTTON

    DEFINE BUTTON Cancel_BU
      ROW     85
      COL    115
      WIDTH   80
      HEIGHT  25
      CAPTION 'Cancel'
      ACTION  Main_WA.RELEASE
    END BUTTON

    ON KEY ESCAPE ACTION ThisWindow.RELEASE

  END WINDOW

  Main_WA.ACTIVATE

RETURN NIL


#define EM_SETPASSWORDCHAR      0x00CC

FUNCTION ShowPassword( cTextBox, lShowPass )

  LOCAL cParent
  LOCAL nRow
  LOCAL nCol
  LOCAL nWidth
  LOCAL nHeight
  LOCAL cPass

  //cParent := GetParentFormName( This.(cTextBox).INDEX )
  cParent := ThisWindow.NAME

  If lShowPass
     ChangeStyle( This.(cTextBox).HANDLE, , ES_PASSWORD )
     SendMessage( This.(cTextBox).HANDLE, EM_SETPASSWORDCHAR, 0, 0 )
     This.(cTextBox).REFRESH
  Else
     nRow := This.(cTextBox).ROW
     nCol := This.(cTextBox).COL
     nWidth := This.(cTextBox).WIDTH
     nHeight := This.(cTextBox).HEIGHT
     cPass := This.(cTextBox).VALUE

     This.(cTextBox).RELEASE
     DoEvents()

     DEFINE TEXTBOX (cTextBox)
       PARENT   (cParent)
       ROW      nRow
       COL      nCol
       WIDTH    nWidth
       HEIGHT   nHeight
       VALUE    cPass
       PASSWORD !lShowPass
     END TEXTBOX
  EndIf

RETURN NIL
