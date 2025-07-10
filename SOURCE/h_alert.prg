/*----------------------------------------------------------------------------
MINIGUI - Harbour Win32 GUI library source code

Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
http://harbourminigui.googlepages.com/

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2 of the License, or (at your option) any later
version.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along with
   this software; see the file COPYING. If not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA (or
   visit the web site http://www.gnu.org/).

   As a special exception, you have permission for additional uses of the text
   contained in this release of Harbour Minigui.

   The exception is that, if you link the Harbour Minigui library with other
   files to produce an executable, this does not by itself cause the resulting
   executable to be covered by the GNU General Public License.
   Your use of that executable is in no way restricted on account of linking the
   Harbour-Minigui library code into it.

   Parts of this project are based upon:

   "Harbour GUI framework for Win32"
   Copyright 2001 Alexander S.Kresin <alex@kresin.ru>
   Copyright 2001 Antonio Linares <alinares@fivetech.com>
   www - https://harbour.github.io/

   "Harbour Project"
   Copyright 1999-2025, https://harbour.github.io/

   "WHAT32"
   Copyright 2002 AJ Wos <andrwos@aust1.net>

   "HWGUI"
   Copyright 2001-2021 Alexander S.Kresin <alex@kresin.ru>

 ---------------------------------------------------------------------------*/

#ifdef __XHARBOUR__
#define __MINIPRINT__
#else
   SET PROCEDURE TO alerts.prg
#endif

#include "hmg.ch"
#include "i_winuser.ch"

/*
   Author    : Francisco Garcia Fernandez
   Objective : Simulate Clipper ALERT function

   Syntax    : HMG_Alert( cText, [<aOptions>], [<cTitle>], [<nType>], [<cIcoName>], [<nIcoSize>], [<aBtnColors>], [<bInit>], [<lClosable>], [<cFontName>] )

   Last Modified by Grigory Filatov at 31-05-2025
*/

#define MARGIN          32
#define MARGIN_ICON     70
#define VMARGIN_BUTTON  4
#define HMARGIN_BUTTON  22
#define SEP_BUTTON      10
#define TAB             Chr( 9 )

#define aBackColor          s_Config [1]
#define aFontColor          s_Config [2]
#define cLineSeparator      s_Config [3]
#define nMaxLineLen         s_Config [4]
#define cFontNameAlert      s_Config [5]

STATIC s_Config := { NIL, NIL, ";|", 79, "DlgFont" }

/*-----------------------------------------------------------------------------*
FUNCTION HMG_Alert( cMsg, aOptions, cTitle, nType, cIcoFile, nIcoSize, aBtnColors, bInit, lClosable, cFontName )
*------------------------------------------------------------------------------*
*
*  Description:
*     Displays a modal alert dialog box with a message, options, title, icon, and customizable features.
*     This function provides a way to present information to the user and receive a response through button clicks.
*     It simulates the functionality of the Clipper ALERT function within the HMG Extended environment.
*
*  Parameters:
*     cMsg      - The message to display in the alert dialog.  Multiple lines can be separated by a delimiter character (defined by cLineSeparator).  Data Type: CHARACTER.
*     aOptions  - An array of strings representing the button captions.  If a numeric value is passed instead of an array, it's interpreted as the number of seconds to wait before automatically closing the dialog (only if no button is pressed). Data Type: ARRAY or NUMERIC (optional).
*     cTitle    - The title of the alert dialog window.  If NIL, defaults to a predefined message ( _HMG_MESSAGE [10] ). Data Type: CHARACTER (optional).
*     nType     - An integer representing the type of system icon to display (1=Warning, 2=Question, 3=Information, 4=Error).  If aOptions has only one element, nType defaults to 1; if aOptions has two or more elements, nType defaults to 2. Data Type: NUMERIC (optional).
*     cIcoFile  - The file name of a custom icon to display.  If NIL, defaults to a system icon based on nType.  Can also be a system icon constant (SYSICO_WARN, SYSICO_QUES, SYSICO_INFO, SYSICO_ERROR). Data Type: CHARACTER or NUMERIC (optional).
*     nIcoSize  - The size of the icon to display (32, 48, or 64 pixels).  Defaults to 32 if NIL. Data Type: NUMERIC (optional).
*     aBtnColors- An array of RGB color arrays (e.g., { {255,0,0}, {0,255,0} }) to customize the background color of each button.  The array length must match the number of buttons defined in aOptions. Data Type: ARRAY (optional).
*     bInit     - A code block to execute after the dialog is defined but before it's activated.  This allows for further customization of the dialog's appearance or behavior. Data Type: BLOCK (optional).
*     lClosable - A logical value indicating whether the dialog can be closed by clicking the close button (X) on the title bar.  Defaults to .F. (false) if NIL. Data Type: LOGICAL (optional).
*     cFontName - The name of the font to use for the dialog's text and button captions.  Defaults to cFontNameAlert (defined globally). Data Type: CHARACTER (optional).
*
*  Return Value:
*     The Cargo property of the button that was clicked. If the dialog is closed via the close button (X) or Escape key, the return value is 0. Data Type: USUAL.
*     The function also sets the global variable _HMG_ModalDialogReturn to the Cargo property of the button clicked.
*
*  Purpose:
*     This function provides a standardized way to display alert messages to the user within an HMG Extended application.
*     It encapsulates the creation and management of a modal dialog window, including the message text, button options, icon, and other visual elements.
*     It simplifies the process of presenting information and receiving user input through button clicks, making it easier to create consistent and user-friendly interfaces.
*
*  Notes:
*     - The function relies on several global variables (e.g., s_Config, _HMG_ModalDialogReturn, _HMG_IsModalActive) for configuration and state management.
*     - The appearance of the dialog is influenced by system settings (e.g., colors, fonts).
*     - The function attempts to handle different screen resolutions and font sizes to ensure proper display.
*     - The use of _SetGetGlobal is crucial for managing global state within the HMG environment.
*     - The function uses hb_FNameSplit to check if a window with the same name already exists.
*     - The function uses a timer to automatically close the dialog if a numeric value is passed as aOptions.
*/
FUNCTION HMG_Alert( cMsg, aOptions, cTitle, nType, cIcoFile, nIcoSize, aBtnColors, bInit, lClosable, cFontName )
   LOCAL nLineas
   LOCAL aIcon := { SYSICO_WARN, SYSICO_QUES, SYSICO_INFO, SYSICO_ERROR }
   LOCAL lFont := .F.
   LOCAL lEmpty := ( Empty( aOptions ) .OR. ISNUMERIC( aOptions ) )
   LOCAL cDelim, cOldDelim
   LOCAL cForm := "oDlg"
   LOCAL nMaxLen := 0
   LOCAL hPrevious

   IF _SetGetGlobal( "_HMG_IsWin10" ) == NIL
      STATIC _HMG_IsWin10 AS GLOBAL VALUE IsWin10OrLater()
   ENDIF

   STATIC _HMG_PressButton AS GLOBAL VALUE .F.

   IF _IsWindowDefined( cForm )
      nLineas := 0
      WHILE _IsWindowDefined( cForm := "oDlg" + hb_ntos( ++nLineas ) )
      END
   ENDIF

   hb_default( @aBackColor, nRGB2Arr( GetSysColor( COLOR_BTNFACE ) ) )
   hb_default( @aFontColor, nRGB2Arr( GetSysColor( COLOR_BTNTEXT ) ) )
   __defaultNIL( @cTitle, _HMG_MESSAGE [10] )
   __defaultNIL( @aOptions, { "&OK" } )
   hb_default( @lClosable, .F. )
   hb_default( @cFontName, cFontNameAlert )

   IF ISARRAY( aOptions )
      DEFAULT nType := iif( Len( aOptions ) > 1, 2, 1 )
   ELSE
      DEFAULT nType := 1
   ENDIF

#ifdef _HMG_COMPAT_
   CHECK TYPE cMsg AS USUAL, ;
      aOptions AS USUAL, ;
      cTitle AS CHARACTER, ;
      nType AS NUMERIC, ;
      cIcoFile AS USUAL, ;
      nIcoSize AS USUAL, ;
      aBtnColors AS USUAL, ;
      bInit AS USUAL, ;
      lClosable AS LOGICAL, ;
      cFontName AS CHARACTER
#endif
   IF nType < 1 .OR. nType > 4
      nType := 1
   ENDIF

   __defaultNIL( @cIcoFile, aIcon[ nType ] )
   hb_default( @nIcoSize, 32 )

   IF GetFontHandle( cFontName ) == 0
      lFont := .T.
      DEFINE FONT ( cFontName ) FONTNAME GetDefaultFontName() SIZE GetDefaultFontSize() - iif( _SetGetGlobal( "_HMG_IsWin10" ), 1, 0 )
   ENDIF

   cMsg := cValToChar( cMsg )

   IF Set( _SET_DELIMITERS ) .AND. ! Empty( cLineSeparator )
      cOldDelim := Set( _SET_DELIMCHARS )
      Set( _SET_DELIMCHARS, cLineSeparator )
#ifdef __XHARBOUR__
      cDelim := SubStr( Set( _SET_DELIMCHARS ), iif( Upper( Left( Set ( _SET_LANGUAGE ), 2 ) ) == "EL", 2, 1 ), 1 )
#else
      cDelim := SubStr( Set( _SET_DELIMCHARS ), iif( "el" $ hb_UserLang(), 2, 1 ), 1 )
#endif
      cMsg := StrTran( cMsg, cDelim, CRLF )
      Set( _SET_DELIMCHARS, cOldDelim )
   ENDIF

   nLineas := MLCount( cMsg )

   IF TAB $ cMsg
      cMsg := StrTran( cMsg, TAB, Space( 3 ) )
   ENDIF

   AEval( hb_ATokens( cMsg, CRLF ), {| ct | nMaxLen := Max( nMaxLen, Len( Trim( ct ) ) ) } )

   IF lEmpty
      lClosable := .T.
      _HMG_ModalDialogReturn := 0
   ELSE
      hb_default( @_HMG_ModalDialogReturn, 0 )
   ENDIF

   hPrevious := iif( _HMG_BeginWindowMDIActive, GetActiveMdiHandle(), GetActiveWindow() )

   DEFINE WINDOW ( cForm ) ;
      WIDTH 0 HEIGHT 0 ;
      TITLE cTitle ;
      MODAL NOSIZE ;
      BACKCOLOR aBackColor ;
      ON INTERACTIVECLOSE ( _SetGetGlobal( "_HMG_PressButton" ) .OR. lClosable ) ;
      ON RELEASE iif( ! _SetGetGlobal( "_HMG_PressButton" ) .AND. lClosable, _HMG_ModalDialogReturn := 0, NIL )
#ifdef _OBJECT_
      This.Cargo := oHmgData()
#endif
      FillDlg( cMsg, aOptions, nLineas, cIcoFile, nIcoSize, aBtnColors, bInit, lClosable, cFontName, nMaxLen )
   END WINDOW

   ACTIVATE WINDOW ( cForm )

   IF lFont
      RELEASE FONT ( cFontName )
   ENDIF

   IF hPrevious != NIL
      IF _HMG_IsModalActive
         EnableWindow( hPrevious )
      ENDIF
      SetFocus( hPrevious )
   ENDIF

RETURN _HMG_ModalDialogReturn

/*-----------------------------------------------------------------------------*
FUNCTION HMG_Alert_MaxLines( nLines, nWide )
*------------------------------------------------------------------------------*
*
*  Description:
*     Sets or retrieves the maximum number of lines to display in the alert dialog and the maximum line length.
*     This function allows controlling the size and formatting of the alert message.
*
*  Parameters:
*     nLines - The maximum number of lines to display in the alert dialog. If NIL, the function returns the current value. Data Type: NUMERIC (optional).
*     nWide  - The maximum length of each line in the alert dialog. If NIL, the function only affects the maximum number of lines. Data Type: NUMERIC (optional).
*
*  Return Value:
*     The previous value of the maximum number of lines. Data Type: NUMERIC.
*
*  Purpose:
*     This function provides a way to limit the amount of text displayed in the alert dialog, preventing it from becoming too large or unwieldy.
*     It's useful for ensuring that the dialog fits within the screen boundaries and remains readable.
*     It also allows setting the maximum line length to prevent lines from wrapping awkwardly.
*
*  Notes:
*     - The function uses global variables to store the maximum number of lines and the maximum line length.
*     - The maximum line length is limited to 255 characters.
*/
FUNCTION HMG_Alert_MaxLines( nLines, nWide )
   LOCAL cVarName := "_" + ProcName()
   LOCAL nOldLines := _AddNewGlobal( cVarName, 20 )

   IF HB_ISNUMERIC( nLines ) .AND. nLines > 0
      _SetGetGlobal( cVarName, nLines )
   ENDIF

   IF HB_ISNUMERIC( nWide ) .AND. nWide > 0 .AND. nWide < 255
      nMaxLineLen := nWide
   ENDIF

RETURN nOldLines

/*-----------------------------------------------------------------------------*
FUNCTION HMG_Alert_RowStart( nRow )
*------------------------------------------------------------------------------*
*
*  Description:
*     Sets or retrieves the starting row position for the content within the alert dialog.
*     This function allows fine-tuning the vertical placement of the message and other elements.
*
*  Parameters:
*     nRow - The starting row position for the content. If NIL, the function returns the current value. Data Type: NUMERIC (optional).
*
*  Return Value:
*     The previous value of the starting row position. Data Type: NUMERIC.
*
*  Purpose:
*     This function provides a way to adjust the vertical alignment of the content within the alert dialog.
*     It's useful for creating a more visually appealing and balanced layout.
*
*  Notes:
*     - The function uses a global variable to store the starting row position.
*     - The row position must be a non-negative number.
*/
FUNCTION HMG_Alert_RowStart( nRow )
   LOCAL cVarName := "_" + ProcName()
   LOCAL nOldRow := _AddNewGlobal( cVarName, 0 )

   IF HB_ISNUMERIC( nRow ) .AND. nRow >= 0
      _SetGetGlobal( cVarName, nRow )
   ENDIF

RETURN nOldRow

/*-----------------------------------------------------------------------------*
FUNCTION HMG_Alert_FontName( cFontName )
*------------------------------------------------------------------------------*
*
*  Description:
*     Sets or retrieves the font name used for the alert dialog's text.
*     This function allows customizing the font to match the application's overall style.
*
*  Parameters:
*     cFontName - The name of the font to use. If NIL, the function returns the current value. Data Type: CHARACTER (optional).
*
*  Return Value:
*     The previous value of the font name. Data Type: CHARACTER.
*
*  Purpose:
*     This function provides a way to change the font used in the alert dialog, allowing for visual consistency with the rest of the application.
*
*  Notes:
*     - The function uses a global variable (cFontNameAlert) to store the font name.
*/
FUNCTION HMG_Alert_FontName( cFontName )
   LOCAL cOldFont := cFontNameAlert

   IF HB_ISCHAR( cFontName )
      cFontNameAlert := cFontName
   ENDIF

RETURN cOldFont

/*-----------------------------------------------------------------------------*
FUNCTION HMG_Alert_Separator( cSeparator )
*------------------------------------------------------------------------------*
*
*  Description:
*     Sets or retrieves the line separator character used to split the alert message into multiple lines.
*     This function allows customizing the delimiter used to separate lines in the message text.
*
*  Parameters:
*     cSeparator - The line separator character. If NIL, the function returns the current value. Data Type: CHARACTER (optional).
*
*  Return Value:
*     The previous value of the line separator character. Data Type: CHARACTER.
*
*  Purpose:
*     This function provides a way to customize the character used to separate lines in the alert message, allowing for flexibility in formatting the message text.
*
*  Notes:
*     - The function uses a global variable (cLineSeparator) to store the line separator character.
*/
FUNCTION HMG_Alert_Separator( cSeparator )
   LOCAL cOldSep := cLineSeparator

   IF HB_ISCHAR( cSeparator )
      cLineSeparator := cSeparator
   ENDIF

RETURN cOldSep

/*-----------------------------------------------------------------------------*
STATIC FUNCTION FillDlg( cMsg, aOptions, nLineas, cIcoFile, nIcoSize, aBtnColors, bBlock, lClosable, cFont, nMaxLen )
*------------------------------------------------------------------------------*
*
*  Description:
*     Fills the alert dialog window with the message, options, icon, and other visual elements.
*     This function is responsible for creating and positioning the controls within the dialog.
*
*  Parameters:
*     cMsg      - The message to display in the alert dialog. Data Type: CHARACTER.
*     aOptions  - An array of strings representing the button captions. Data Type: ARRAY or NUMERIC.
*     nLineas   - The number of lines in the message. Data Type: NUMERIC.
*     cIcoFile  - The file name of a custom icon to display. Data Type: CHARACTER or NUMERIC.
*     nIcoSize  - The size of the icon to display. Data Type: NUMERIC.
*     aBtnColors- An array of RGB color arrays to customize the background color of each button. Data Type: ARRAY (optional).
*     bBlock    - A code block to execute after the dialog is defined but before it's activated. Data Type: BLOCK (optional).
*     lClosable - A logical value indicating whether the dialog can be closed by clicking the close button. Data Type: LOGICAL.
*     cFont     - The name of the font to use for the dialog's text and button captions. Data Type: CHARACTER.
*     nMaxLen   - The maximum length of a line in the message. Data Type: NUMERIC.
*
*  Return Value:
*     NIL.
*
*  Purpose:
*     This function is the core of the HMG_Alert function, responsible for creating and arranging the GUI elements within the alert dialog.
*     It calculates the dimensions of the dialog based on the message length, button captions, and icon size.
*     It creates the necessary controls (labels, buttons, icons) and positions them appropriately within the dialog window.
*     It also handles the customization of button colors and the execution of the initialization block.
*
*  Notes:
*     - The function relies heavily on calculations to determine the optimal size and layout of the dialog.
*     - It uses system metrics (e.g., border width, title height) to ensure proper alignment and spacing.
*     - The function handles different icon sizes and button color customizations.
*     - The use of BUTTONEX allows for extended button customization (e.g., XP styles).
*     - The function uses a code block (bBlock) to allow for further customization of the dialog.
*     - The function uses _SetGetGlobal to access and modify global variables.
*     - The function uses _IsControlDefined to check if a timer control is defined.
*/
STATIC FUNCTION FillDlg( cMsg, aOptions, nLineas, cIcoFile, nIcoSize, aBtnColors, bBlock, lClosable, cFont, nMaxLen )
   LOCAL hWnd
   LOCAL hDC
   LOCAL hDlgFont
   LOCAL aBut := {}
   LOCAL cForm := ThisWindow.Name
   LOCAL cLblName
   LOCAL cBtnName
   LOCAL nRow := HMG_Alert_RowStart()
   LOCAL nCol := MARGIN * 0.6
   LOCAL nOpc := 1
   LOCAL nMaxLin := 0
   LOCAL nMaxBoton := 0
   LOCAL nMaxLines := HMG_Alert_MaxLines()
   LOCAL nMaxHeight
   LOCAL nMaxWidth
   LOCAL nLenBotones
   LOCAL nLenaOp
   LOCAL nWidthCli, nHeightCli
   LOCAL nWidthDlg, nHeightDlg
   LOCAL nChrHeight
   LOCAL nHeightBtn
   LOCAL nVMARGIN_BUTTON := VMARGIN_BUTTON
   LOCAL nSeconds
   LOCAL n
   LOCAL lIsWin10 := _SetGetGlobal( "_HMG_IsWin10" )
   LOCAL lExt
#ifdef _OBJECT_
   LOCAL nY, nX, cIco
#endif

#ifdef _HMG_COMPAT_
   CHECK TYPE cMsg AS CHARACTER, ;
      aOptions AS USUAL, ;
      nLineas AS NUMERIC, ;
      cIcoFile AS USUAL, ;
      nIcoSize AS NUMERIC, ;
      aBtnColors AS USUAL, ;
      bBlock AS USUAL, ;
      lClosable AS LOGICAL, ;
      cFont AS CHARACTER, ;
      nMaxLen AS NUMERIC
#endif
   IF ISNUMERIC( aOptions )
      nSeconds := aOptions
      aOptions := { "&OK" }
      DEFINE TIMER oTimer OF ( cForm ) INTERVAL nSeconds * 1000 ACTION ( _SetGetGlobal( "_HMG_PressButton", .T. ), ThisWindow.Release() )
#ifdef _OBJECT_
      This.Cargo:oTimer := "TIMER"
#endif
      This.oTimer.Enabled := .F.
   ENDIF

   nLenaOp := iif( ISARRAY( aOptions ), Len( aOptions ), 1 )

   IF ( lExt := ( ISARRAY( aBtnColors ) .AND. Len( aBtnColors ) == nLenaOp ) )
      nVMARGIN_BUTTON := 3 * VMARGIN_BUTTON
   ENDIF

   hDlgFont := GetFontHandle( cFont )

   // calculate the column of the text output

   IF nIcoSize > 0
      nCol := MARGIN_ICON + iif( nIcoSize == 32, 0, MARGIN_ICON / iif( nIcoSize == 64, 2.8, 3.2 ) )
   ENDIF

   hWnd := This.Handle
   hDC := GetDC( hWnd )

   // calculate the character height for the dialog font

   nChrHeight := GetTextHeight( hDC, aOptions[ 1 ], hDlgFont ) + nVMARGIN_BUTTON / 2

   // calculate the maximum width of the lines

   nMaxWidth := GetFontWidth( cFont, nMaxLen )
   IF GetTextWidth( hDC, Space( 10 ), hDlgFont ) != GetTextWidth( hDC, Replicate( "B", 10 ), hDlgFont )
      nMaxWidth *= 0.7
   ENDIF

   FOR n := 1 TO nLineas
      nMaxLin := Max( nMaxLin, GetTextWidth( hDC, AllTrim( MemoLine( cMsg, nMaxLineLen, n ) ), hDlgFont ) )
   NEXT

   // calculate the maximum width of the buttons

   FOR n := 1 TO nLenaOp
      nMaxBoton := Max( nMaxBoton, GetTextWidth( hDC, aOptions[ n ], hDlgFont ) )
   NEXT

   ReleaseDC( hWnd, hDC )

   nMaxBoton += ( HMARGIN_BUTTON * iif( ! lExt .AND. lIsWin10 .AND. nLenAop > 2, 1.1, iif( nLenAop > 1, 2, 3 ) ) )

   // calculate the width of the options + their separations

   nLenBotones := ( nMaxBoton + SEP_BUTTON ) * nLenaOp

   nHeightBtn := nVMARGIN_BUTTON + nChrHeight + nVMARGIN_BUTTON

   // calculate the width of the client area

   IF nMaxWidth > nMaxLin
      nMaxLin := nMaxWidth
   ENDIF

   nWidthCli := Max( MARGIN_ICON + nMaxLin + MARGIN, MARGIN + nLenBotones + MARGIN - HMARGIN_BUTTON ) + iif( nIcoSize > 48, MARGIN / 4, 0 )
   nWidthDlg := nWidthCli + GetBorderWidth() + iif( nLineas > nMaxLines, MARGIN * 1.5, 0 )

   IF nWidthDlg > GetDesktopRealWidth()
      nMaxWidth := nWidthDlg - GetDesktopRealWidth()
      nWidthDlg := GetDesktopRealWidth()
   ELSE
      nMaxWidth := 0
   ENDIF

   nHeightCli := ( Min( nMaxLines, nLineas ) + iif( nLineas == 1, 4, 3 ) ) * nChrHeight + nVMARGIN_BUTTON + nHeightBtn + GetBorderHeight()
   nHeightDlg := nHeightCli + GetTitleHeight() + SEP_BUTTON + GetBorderHeight() / iif( lIsWin10, 2.5, 1 )

   IF ( MSC_VER() > 0 .OR. _HMG_IsBcc77 ) .AND. _HMG_IsThemed
      nWidthDlg += GetBorderWidth() + 2
      nHeightDlg += GetBorderHeight() + 2
   ENDIF

   IF nHeightDlg > GetDesktopRealHeight()
      n := 0
      WHILE ( nHeightDlg - ( nChrHeight * ( ++n ) ) ) > GetDesktopRealHeight()
      END
      nMaxHeight := nChrHeight * n
      nMaxLines  -= n
      nHeightDlg -= nMaxHeight
      nHeightCli -= nMaxHeight
   ENDIF

   This.Width := nWidthDlg
   This.Height := nHeightDlg

   IF Empty( nRow )
      nRow := nChrHeight
   ENDIF

   IF nLineas > 1

      IF nLineas > nMaxLines

         cLblName := "Say_01"

         @ nRow + GetBorderHeight(), nCol EDITBOX ( cLblName ) VALUE AllTrim( cMsg ) OF ( cForm ) ;
            FONT cFont WIDTH nWidthCli - nCol + iif( nLineas < nMaxLines + 5, 0.9, 1 ) * MARGIN - nMaxWidth ;
            HEIGHT nChrHeight * nMaxLines + GetBorderHeight() ;
            FONTCOLOR aFontColor BACKCOLOR aBackColor READONLY NOHSCROLL
#ifdef _OBJECT_
         This.Cargo:Set( cLblName, This.(cLblName).Type )
#endif
      ELSE

         FOR n := 1 TO nLineas

            cLblName := "Say_" + StrZero( n, 2 )

            @ nRow * ( n + iif( nLineas == 1, .5, 0 ) ) + GetBorderHeight(), nCol ;
               LABEL ( cLblName ) VALUE AllTrim( MemoLine( cMsg, nMaxLineLen, n ) ) OF ( cForm ) ;
               FONT cFont WIDTH nWidthCli - nCol - GetBorderWidth() - MARGIN / 4 - nMaxWidth ;
               HEIGHT nChrHeight ;
               FONTCOLOR aFontColor BACKCOLOR aBackColor VCENTERALIGN
#ifdef _OBJECT_
            This.Cargo:Set( cLblName, This.(cLblName).Type )
#endif
         NEXT n

      ENDIF

   ELSE

      cLblName := "Say_01"

      @ nRow + GetBorderHeight(), nCol LABEL ( cLblName ) VALUE AllTrim( cMsg ) OF ( cForm ) ;
         FONT cFont WIDTH nWidthCli - nCol - GetBorderWidth() - MARGIN / 4 HEIGHT Max( nChrHeight, nIcoSize ) ;
         FONTCOLOR aFontColor BACKCOLOR aBackColor VCENTERALIGN
#ifdef _OBJECT_
      This.Cargo:Set( cLblName, This.(cLblName).Type )
#endif
   ENDIF

   IF nIcoSize > 0

#ifdef _OBJECT_
      nY   := nRow + GetBorderHeight()
      cIco := "ICON"
#endif
      IF ISNUMBER( cIcoFile )

         IF IsHIcon( cIcoFile )
#ifdef _OBJECT_
            nX := MARGIN / iif( nIcoSize == 32, 1.4, iif( nIcoSize == 48, 1.7, 2 ) )
#endif
            DRAW ICON IN WINDOW ( cForm ) ;
               AT nRow + GetBorderHeight(), MARGIN / iif( nIcoSize == 32, 1.4, iif( nIcoSize == 48, 1.7, 2 ) ) ;
               HICON cIcoFile WIDTH nIcoSize HEIGHT nIcoSize TRANSPARENT
         ELSE
#ifdef _OBJECT_
            nX := MARGIN / 1.4
            cIco := "SYSICON " + hb_ntos( cIcoFile )
#endif
            DRAW SYSICON IN WINDOW ( cForm ) ;
               AT nRow + GetBorderHeight(), MARGIN / 1.4 ;
               ICON cIcoFile WIDTH nIcoSize HEIGHT nIcoSize TRANSPARENT
         ENDIF
      ELSE
#ifdef _OBJECT_
         nX := MARGIN / iif( nIcoSize == 32, 1.4, iif( nIcoSize == 48, 1.7, 2 ) )
#endif
         DRAW ICON IN WINDOW ( cForm ) ;
            AT nRow + GetBorderHeight(), MARGIN / iif( nIcoSize == 32, 1.4, iif( nIcoSize == 48, 1.7, 2 ) ) ;
            PICTURE cIcoFile WIDTH nIcoSize HEIGHT nIcoSize TRANSPARENT

      ENDIF

#ifdef _OBJECT_
      This.Cargo:Draw := { cIco, nY, nX, cIcoFile, nIcoSize }
#endif
   ENDIF

#ifdef _OBJECT_
   This.Cargo:lExtButton := lExt
#endif
   FOR n := 1 TO nLenaOp

      cBtnName := "Btn_" + StrZero( n, 2 )

      AAdd( aBut, cBtnName )

      IF lExt

         @ 0, 0 BUTTONEX ( cBtnName ) OF ( cForm ) CAPTION aOptions[ n ] ;
            FONTCOLOR aFontColor BACKCOLOR aBtnColors[ n ] NOXPSTYLE HANDCURSOR ;
            FONT cFont WIDTH nMaxBoton HEIGHT nVMARGIN_BUTTON + nChrHeight + nVMARGIN_BUTTON ;
            ACTION ( _HMG_ModalDialogReturn := This.Cargo, _SetGetGlobal( "_HMG_PressButton", .T. ), ThisWindow.Release() )
#ifdef _OBJECT_
         This.Cargo:Set( cBtnName, This.(cBtnName).Type )
#endif
      ELSE

         @ 0, 0 BUTTON ( cBtnName ) OF ( cForm ) CAPTION aOptions[ n ] ;
            FONT cFont WIDTH nMaxBoton HEIGHT nVMARGIN_BUTTON + nChrHeight + nVMARGIN_BUTTON ;
            ACTION ( _HMG_ModalDialogReturn := This.Cargo, _SetGetGlobal( "_HMG_PressButton", .T. ), ThisWindow.Release() )
#ifdef _OBJECT_
         This.Cargo:Set( cBtnName, This.(cBtnName).Type )
#endif
      ENDIF

      This.( aBut[ nOpc ] ).Cargo := nOpc++

   NEXT n

   nOpc := 1

   FOR n := nLenaOp TO 1 STEP -1
      This.( aBut[ n ] ).Row := nHeightCli + SEP_BUTTON + GetBorderHeight() / iif( lIsWin10, 2.5, .9 ) - nChrHeight - nHeightBtn
      This.( aBut[ n ] ).Col := nWidthCli - nMaxWidth + iif( nLineas > nMaxLines, MARGIN * 1.5, 0 ) + iif( lIsWin10, 0, GetBorderWidth() / 2 ) - ( nMaxBoton + SEP_BUTTON ) * nOpc++
   NEXT n

   This.Closable := lClosable

   This.( aBut[ Max( 1, Min( nLenaOp, _HMG_ModalDialogReturn ) ) ] ).SetFocus()

   This.Center()

   IF lClosable
      ON KEY ESCAPE OF ( cForm ) ACTION ( _HMG_ModalDialogReturn := 0, _SetGetGlobal( "_HMG_PressButton", .T. ), ThisWindow.Release() )
   ENDIF

   IF HB_ISBLOCK( bBlock )
      Do_WindowEventProcedure( bBlock, This.Index, 'WINDOW_INIT' )
   ENDIF

   IF _IsControlDefined( "oTimer", cForm )
      This.oTimer.Enabled := .T.
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------*
FUNCTION _SetMsgAlertColors( aBackClr, aFontClr )
*------------------------------------------------------------------------------*
*  Description:
*     Sets the background and font colors for message alerts.
*
*  Parameters:
*     aBackClr - An array representing the new background color. If NIL, the background color is not changed.
*     aFontClr - An array representing the new font color. If NIL, the font color is not changed.
*
*  Return Value:
*     An array containing the old background and font colors: { aBackColor, aFontColor }.
*
*  Purpose:
*     This function allows the user to customize the colors used in message alerts.
*     It stores the previous colors and updates the global variables aBackColor and aFontColor with the new values if provided.
*     This is useful for providing visual consistency or highlighting alerts based on their severity or context.
*
*  HMG Extended Features Used:
*     Uses global variables aBackColor and aFontColor, which are likely used by other alert-related functions within the HMG Extended framework.
*
*  Side Effects:
*     Modifies the global variables aBackColor and aFontColor if the corresponding input parameters are not NIL.
*
*  Notes:
*     The format of the color arrays (aBackClr and aFontClr) is assumed to be compatible with HMG Extended's color representation (e.g., RGB values).
*/
FUNCTION _SetMsgAlertColors( aBackClr, aFontClr )
   LOCAL aOldClrs := { aBackColor, aFontColor }

   IF aBackClr != NIL
      aBackColor := aBackClr
   ENDIF

   IF aFontClr != NIL
      aFontColor := aFontClr
   ENDIF

RETURN aOldClrs

#ifdef _HMG_COMPAT_

/*-----------------------------------------------------------------------------*
PROCEDURE HMG_CheckType( lSoft, ... )  (xHarbour version)
OR
PROCEDURE HMG_CheckType( ... ) (non-xHarbour version)
*------------------------------------------------------------------------------*
*  Description:
*     Performs runtime type checking of function parameters.
*
*  Parameters:
*     lSoft (xHarbour only) - A logical value indicating whether to perform "soft" type checking. If .T., NIL values are allowed even if a specific type is expected.
*     ... - A variable number of parameters, each representing a type check specification. Each specification is an array with the following structure:
*           { cTypeDef, cValType, cVarName }
*           - cTypeDef: A string representing the expected data type (e.g., "ARRAY", "NUMERIC", "CHARACTER").
*           - cValType: A single-character string representing the expected data type (e.g., "A", "N", "C").
*           - cVarName: A string representing the name of the variable being checked.
*
*  Return Value:
*     None.  This procedure does not return a value.
*
*  Purpose:
*     This procedure is designed to enforce type safety in HMG Extended applications.
*     It checks if the actual data type of each function parameter matches the expected data type specified in the type check specification.
*     If a type mismatch is detected, an error message is displayed using MsgMiniGuiError.
*     This helps to prevent runtime errors caused by incorrect data types being passed to functions.
*
*  Notes:
*     The lSoft parameter is only used in xHarbour.  In non-xHarbour versions, it is omitted.
*     The aType array defines the mapping between type names (e.g., "ARRAY") and type codes (e.g., "A").
*     The __enumindex() method is used to get the index of the current element in the aParams array.
*     The "USUAL" type allows any data type to be passed.
*/
#ifndef __XHARBOUR__
PROCEDURE HMG_CheckType( lSoft, ... )
#else
PROCEDURE HMG_CheckType( ... )
#endif
   LOCAL i, j
   // aData := { cTypeDef, cValType, cVarName }
   LOCAL aData
   // aType := { cTypeDef, cValType }
   LOCAL aType := { ;
      { "ARRAY", "A" }, ;
      { "BLOCK", "B" }, ;
      { "CHARACTER", "C" }, ;
      { "DATE", "D" }, ;
      { "HASH", "H" }, ;
      { "LOGICAL", "L" }, ;
      { "NIL", "U" }, ;
      { "NUMERIC", "N" }, ;
      { "MEMO", "M" }, ;
      { "POINTER", "P" }, ;
      { "SYMBOL", "S" }, ;
      { "TIMESTAMP", "T" }, ;
      { "OBJECT", "O" }, ;
      { "USUAL", "" } }
   LOCAL aParams := hb_AParams()
#ifdef __XHARBOUR__
   LOCAL lSoft

   lSoft := aParams[ 1 ]
#endif
   hb_ADel( aParams, 1, .T. )  // Remove lSoft parameter from the array

   FOR EACH aData IN aParams

      IF Upper( AllTrim( aData[ 1 ] ) ) <> "USUAL"

         IF !( lSoft .AND. AllTrim( aData[ 2 ] ) == "U" )

            i := AScan( aType, {| x | x[ 1 ] == Upper( AllTrim( aData[ 1 ] ) ) } )

            IF i == 0 .OR. aType[ i ][ 2 ] <> aData[ 2 ]

               j := AScan( aType, {| x | x[ 2 ] == aData[ 2 ] } )

               MsgMiniGuiError( "CHECK TYPE ( Param # " + hb_ntos( aData:__enumindex() ) + " ) : " + AllTrim( aData[ 3 ] ) + " is declared as " + Upper( AllTrim( aData[ 1 ] ) ) + " but it have type " + aType[ j ][ 1 ] + "." )

            ENDIF

         ENDIF

      ENDIF

   NEXT

RETURN

#endif

*=============================================================================*
*                          Auxiliary Functions
*=============================================================================*

/*-----------------------------------------------------------------------------*
FUNCTION GetDesktopRealWidth()
*------------------------------------------------------------------------------*
*  Description:
*     Gets the width of the usable desktop area (excluding taskbar and other docked windows).
*
*  Parameters:
*     None.
*
*  Return Value:
*     A numeric value representing the width of the usable desktop area in pixels.
*
*  Purpose:
*     This function is used to determine the available screen space for displaying windows and other GUI elements.
*     It retrieves the desktop area using GetDesktopArea() and calculates the width by subtracting the left coordinate from the right coordinate.
*     This is useful for positioning windows and controls within the visible screen area.
*/
FUNCTION GetDesktopRealWidth()

   LOCAL a := GetDesktopArea()

RETURN ( a[ 3 ] - a[ 1 ] )

/*-----------------------------------------------------------------------------*
FUNCTION GetDesktopRealHeight()
*------------------------------------------------------------------------------*
*  Description:
*     Gets the height of the usable desktop area (excluding taskbar and other docked windows).
*
*  Parameters:
*     None.
*
*  Return Value:
*     A numeric value representing the height of the usable desktop area in pixels.
*
*  Purpose:
*     This function is used to determine the available screen space for displaying windows and other GUI elements.
*     It retrieves the desktop area using GetDesktopArea() and calculates the height by subtracting the top coordinate from the bottom coordinate.
*     This is useful for positioning windows and controls within the visible screen area.
*/
FUNCTION GetDesktopRealHeight()

   LOCAL a := GetDesktopArea()

RETURN ( a[ 4 ] - a[ 2 ] )

#ifdef __XHARBOUR__
#include "alerts.prg"
#endif
