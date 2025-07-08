/*
 * MINIGUI - Harbour Win32 GUI library Demo
 * 
 * Sample was contributed to HMG forum by KDJ
 *
 * This example shows how to:
 * - display only monospace (fixed-width) fonts,
 * - change initial position of dialog,
 * - change dialog title.
 *
 * Adapted for MiniGUI Extended Edition by Grigory Filatov
*/

#include 'minigui.ch'


FUNCTION Main()

  DEFINE WINDOW MainWnd;
    ROW      0;
    COL      0;
    WIDTH  760;
    HEIGHT 520;
    TITLE  "Set edit font";
    MAIN;
    NOSIZE;
    NOMAXIMIZE;
    NOMINIMIZE;
    ON INIT SetEditText()

    DEFINE EDITBOX EdBox
      ROW           10
      COL           10
      WIDTH         735
      HEIGHT        450
      FONTNAME      "Arial"
      FONTSIZE      12
      FONTBOLD      .F.
      FONTITALIC    .F.
      FONTUNDERLINE .F.
      FONTSTRIKEOUT .F.
      FONTCOLOR     {255, 0 , 0}
    END EDITBOX

    DEFINE MAINMENU
      DEFINE POPUP "&Font"
        MENUITEM "All fonts without effects"       ACTION ChangeEditFont(.F., .F.)
        MENUITEM "All fonts with effects"          ACTION ChangeEditFont(.T., .F.)
        SEPARATOR
        MENUITEM "Monospace fonts without effects" ACTION ChangeEditFont(.F., .T.)
        MENUITEM "Monospace fonts with effects"    ACTION ChangeEditFont(.T., .T.)
      END POPUP
    END MENU

    ON KEY ESCAPE ACTION ThisWindow.RELEASE
  END WINDOW

  MainWnd.CENTER
  MainWnd.ACTIVATE

RETURN NIL


FUNCTION SetEditText()
  LOCAL aColor := MainWnd.EdBox.FONTCOLOR
  LOCAL cText  := "* Current edit font *" + CRLF + CRLF

  cText += "Name:        " + MainWnd.EdBox.FONTNAME + CRLF + ;
           "Size:        " + hb_NtoS(MainWnd.EdBox.FONTSIZE) + CRLF + ;
           "Bold:        " + If(MainWnd.EdBox.FONTBOLD,      "Yes", "No") + CRLF + ;
           "Italic:      " + If(MainWnd.EdBox.FONTITALIC,    "Yes", "No") + CRLF + ;
           "Underline:   " + If(MainWnd.EdBox.FONTUNDERLINE, "Yes", "No") + CRLF + ;
           "Strikeout:   " + If(MainWnd.EdBox.FONTSTRIKEOUT, "Yes", "No") + CRLF + ;
           "Color array: " + hb_ValToExp(aColor) + CRLF + ;
           "Color RGB:   " + "0x" + hb_NumToHex((AtoRGB(aColor)), 6) + CRLF

  MainWnd.EdBox.VALUE := cText

RETURN NIL


FUNCTION ChangeEditFont(lEffects, lMonospace)
  LOCAL aFont := GetFont2(MainWnd.EdBox.FONTNAME, ;
                          MainWnd.EdBox.FONTSIZE, ;
                          MainWnd.EdBox.FONTBOLD, ;
                          MainWnd.EdBox.FONTITALIC, ;
                          If(lEffects, MainWnd.EdBox.FONTUNDERLINE, NIL), ;
                          If(lEffects, MainWnd.EdBox.FONTSTRIKEOUT, NIL), ;
                          If(lEffects, AtoRGB(MainWnd.EdBox.FONTCOLOR), NIL), ;
                          lMonospace, ;
                          2, ;
                          NIL, ;
                          NIL, ;
                          If(lMonospace, 2, 0), ;
                          " (monospace)")

  IF ! Empty(aFont[1])
    MainWnd.EdBox.FONTNAME   := aFont[1]
    MainWnd.EdBox.FONTSIZE   := aFont[2]
    MainWnd.EdBox.FONTBOLD   := aFont[3]
    MainWnd.EdBox.FONTITALIC := aFont[4]

    IF lEffects
      MainWnd.EdBox.FONTUNDERLINE := aFont[5]
      MainWnd.EdBox.FONTSTRIKEOUT := aFont[6]
      MainWnd.EdBox.FONTCOLOR     := RGBtoA(aFont[7])
    ENDIF

    SetEditText()
  ENDIF

RETURN NIL


FUNCTION AtoRGB(aColor)

RETURN RGB(aColor[1], aColor[2], aColor[3])


FUNCTION RGBtoA(nRGB)

RETURN {GetRed(nRGB), GetGreen(nRGB), GetBlue(nRGB)}


#pragma BEGINDUMP

#include <mgdefs.h>
#include <commdlg.h>

#ifdef UNICODE
   LPWSTR AnsiToWide( LPCSTR );
   LPSTR  WideToAnsi( LPWSTR );
#endif


typedef struct {
  INT   SetPos;   // 0 - not change dialog position; 1 - set to (xPos, yPos); 2 - center dialog in parent window
  INT   xPos;     //
  INT   yPos;     //
  INT   SetTitle; // 0 - not change dialog title; 1 - set title to (*Title); 2 - add (*Title) in end of title
  TCHAR *Title;   //
} CF_CUSTDATA, *LPCF_CUSTDATA;


UINT_PTR CALLBACK CFHookProc(HWND hWnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
  HB_SYMBOL_UNUSED(wParam);

  if (uMsg == WM_INITDIALOG)
  {
    LPCHOOSEFONT  lpCF = (LPCHOOSEFONT)  lParam;
    LPCF_CUSTDATA lpCD = (LPCF_CUSTDATA) lpCF->lCustData;

    if (lpCD->SetPos > 0)
    {
      RECT rcWin;
      RECT rcOwn;

      GetWindowRect(hWnd, &rcWin);

      if (lpCD->SetPos == 1)
      {
        MoveWindow(hWnd,
                   lpCD->xPos,
                   lpCD->yPos,
                   rcWin.right  - rcWin.left,
                   rcWin.bottom - rcWin.top,
                   0);
      }
      else
      {
        GetWindowRect(lpCF->hwndOwner, &rcOwn);

        //center window
        MoveWindow(hWnd,
                   rcOwn.left + ((rcOwn.right  - rcOwn.left) - (rcWin.right  - rcWin.left)) / 2,
                   rcOwn.top  + ((rcOwn.bottom - rcOwn.top)  - (rcWin.bottom - rcWin.top))  / 2,
                   rcWin.right  - rcWin.left,
                   rcWin.bottom - rcWin.top,
                   0);
      }
    }

    if (lpCD->SetTitle > 0)
    {
      if (lpCD->SetTitle == 1)
      {
        SetWindowText(hWnd, lpCD->Title);
      }
      else
      {
        INT   AddTextLen = lstrlen(lpCD->Title) + 1;
        INT   WinTextLen = GetWindowTextLength(hWnd);
        TCHAR  WinText[256];
        INT   i;

        WinTextLen = GetWindowText(hWnd, WinText, WinTextLen + 1);

        for (i = 0; i < AddTextLen; ++i)
          WinText[WinTextLen + i] = lpCD->Title[i];

        SetWindowText(hWnd, WinText);
      }
    }
  }

  return 0;
}


       //GetFont2([cFontName], [nFontSize], [lBold], [lItalic], [lUnderLine], [lStrikeOut], [nColor], [lMonospace], [nSetPos], [nRow], [nCol], [nSetTitle], [cTitle])
HB_FUNC( GETFONT2 )
{
#ifdef UNICODE
  LPSTR pStr;
  LPWSTR pWStr;
#endif
  HWND        hWnd = GetActiveWindow();
  HDC         hDC;
  CF_CUSTDATA cd;
  LOGFONT     lf;
  CHOOSEFONT  cf;

  if (hWnd == NULL)
    hWnd = GetDesktopWindow();

  hDC = GetDC(hWnd);

  ZeroMemory(&lf, sizeof(lf));
  ZeroMemory(&cf, sizeof(cf));

  cd.SetPos   = hb_parni(9);
  cd.xPos     = hb_parni(11);
  cd.yPos     = hb_parni(10);
  cd.SetTitle = hb_parni(12);
#ifdef UNICODE
  pWStr = AnsiToWide( hb_parc( 13 ) );
  cd.Title    = (TCHAR*) (HB_ISCHAR(13) ? pWStr : TEXT(""));
  hb_xfree( pWStr );
#else
  cd.Title    = (TCHAR*) (HB_ISCHAR(13) ? hb_parc(13) : "");
#endif

  lf.lfHeight    = HB_ISNUM(2) ? (-MulDiv(hb_parnl(2), GetDeviceCaps(hDC, LOGPIXELSY), 72)) : 0;
  lf.lfWeight    = hb_parl(3) ? FW_BOLD : FW_NORMAL;

  if( HB_ISNIL( 4 ) )
    lf.lfItalic = NULL;
  else
    lf.lfItalic = ( BYTE ) hb_parl(4);

  if( HB_ISNIL( 5 ) )
    lf.lfUnderline = NULL;
  else
    lf.lfUnderline = ( BYTE ) hb_parl(5);

  if( HB_ISNIL( 6 ) )
    lf.lfStrikeOut = NULL;
  else
    lf.lfStrikeOut = ( BYTE ) hb_parl(6);

  lf.lfCharSet   = DEFAULT_CHARSET;

  if (HB_ISCHAR(1))
  {
#ifdef UNICODE
    pWStr = AnsiToWide( hb_parc( 1 ) );
    lstrcpy( lf.lfFaceName, pWStr );
    hb_xfree( pWStr );
#else
    lstrcpy(lf.lfFaceName, hb_parc(1));
#endif
  }

  cf.lStructSize = sizeof(CHOOSEFONT);
  cf.hwndOwner   = hWnd;
  cf.lpLogFont   = &lf;
  cf.Flags       = CF_ENABLEHOOK | CF_FORCEFONTEXIST | CF_INITTOLOGFONTSTRUCT | CF_SCREENFONTS;
  cf.rgbColors   = hb_parni(7);
  cf.lCustData   = (LPARAM) &cd;
  cf.lpfnHook    = CFHookProc;

  if (HB_ISLOG(5) || HB_ISLOG(6) || HB_ISNUM(7))
    cf.Flags |= CF_EFFECTS;

  if (hb_parl(8))
    cf.Flags |= CF_FIXEDPITCHONLY;

  if (ChooseFont(&cf))
  {
    hb_reta(7);
#ifndef UNICODE
    HB_STORC  (lf.lfFaceName,                                            -1, 1);
#else
    pStr = WideToAnsi( lf.lfFaceName );
    HB_STORC  (pStr,                                                     -1, 1);
    hb_xfree( pStr );
#endif
    hb_storvnl(-MulDiv(lf.lfHeight, 72, GetDeviceCaps(hDC, LOGPIXELSY)), -1, 2); 
    hb_storvl ((lf.lfWeight >= FW_SEMIBOLD),                             -1, 3); 
    hb_storvl (lf.lfItalic,                                              -1, 4); 
    hb_storvl (lf.lfUnderline,                                           -1, 5); 
    hb_storvl (lf.lfStrikeOut,                                           -1, 6); 
    hb_storvni(cf.rgbColors,                                             -1, 7); 
  }
  else
  {
    hb_reta(7);
    HB_STORC  ("",     -1, 1);
    hb_storvnl(0,      -1, 2); 
    hb_storvl (0,      -1, 3); 
    hb_storvl (0,      -1, 4); 
    hb_storvl (0,      -1, 5); 
    hb_storvl (0,      -1, 6); 
    hb_storvni(0,      -1, 7); 
  }

  ReleaseDC(hWnd, hDC);
}

#pragma ENDDUMP
