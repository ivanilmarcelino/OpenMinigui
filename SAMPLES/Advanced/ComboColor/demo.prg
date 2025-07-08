/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2002 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
 *
 * Fixing the example 2023 Verchenko Andrey <verchenkoag@gmail.com>
 *
 * The purpose of this demo is to showcase how to use comboboxes to select colors,
 * including HMG colors, system colors, and Windows 10 specific colors. It also demonstrates
 * how to enable/disable the dark theme in Windows 10.
 */

#include "minigui.ch"
#include "i_winuser.ch"

*------------------------------------------------
FUNCTION Main()
/*
 *  This is the main function of the application. It defines and activates the main window,
 *  including its controls (labels, comboboxes, checkbox), menu, and imagelists.
 *  It also sets up event handlers for the controls.
 */
*------------------------------------------------
   LOCAL n, nW, nH, nG, cVal, lDark, aSysColor10 := {}
   LOCAL aColorName := {}, aSysColorName := {}
   LOCAL aColor, aSysColor, aColorWin10
   LOCAL BmpW := 16
   LOCAL BmpH := 16
   LOCAL nHCmb := 500

   aColor := ColorHMG()
   aSysColor := ColorHMGSys()
   aColorWin10 := ColorHMGWin10()

   AEval( aColor, {| x | AAdd( aColorName, x[ 2 ] ) } )
   AEval( aSysColor, {| x | AAdd( aSysColorName, x[ 2 ] ) } )
   AEval( aColorWin10, {| x | AAdd( aSysColor10, x[ 2 ] + " - registry key: " + x[ 3 ] ) } )

   DEFINE WINDOW Form_1 ;
         AT 0, 0 ;
         WIDTH 500 HEIGHT 300 ;
         TITLE 'Harbour MiniGUI Combo Color Demo - by Janusz Pora' ;
         MAIN NOMAXIMIZE NOSIZE ;
         ON INIT {|| Form_1.Label_Down.BACKCOLOR := iif( IsWin10OrLater(), Color10_ActiveCaption(), ;
         HMG_n2RGB( GetSysColor( COLOR_ACTIVECAPTION ) ) ) }

      nW := This.ClientWidth
      nH := This.ClientHeight
      nG := GetTitleHeight()

      DEFINE MAIN MENU
         POPUP '&File'
            ITEM 'Get HMG Color' ACTION GetHMGColor( aColor )
            ITEM 'Get System Color' ACTION GetSystemColor( aSysColor )
            ITEM 'Get Win10 Color'  ACTION GetWin10Color( aColorWin10 )
            SEPARATOR
            ITEM '&Exit' ACTION Form_1.RELEASE
         END POPUP
         POPUP '&Help'
            ITEM '&About' ACTION MsgInfo ( "MiniGUI Combo Color Demo" )
         END POPUP

      END MENU


      DEFINE IMAGELIST Imagelst_1 ;
         BUTTONSIZE BmpW, BmpH ;
         IMAGE {}

      FOR n := 1 TO Len( aColor )
         HMG_SetColorBtm( aColor[ n, 1 ], 0, BmpW, BmpH )
         HMG_SetColorBtm( aColor[ n, 1 ], 1, BmpW, BmpH )
         HMG_SetColorBtm( aColor[ n, 1 ], 0, BmpW, BmpH )
      NEXT

      DEFINE IMAGELIST Imagelst_2 ;
         BUTTONSIZE BmpW, BmpH ;
         IMAGE {}

      FOR n := 1 TO Len( aSysColor )
         HMG_SetSysColorBtm( aSysColor[ n, 1 ], 0, BmpW, BmpH )
         HMG_SetSysColorBtm( aSysColor[ n, 1 ], 1, BmpW, BmpH )
         HMG_SetSysColorBtm( aSysColor[ n, 1 ], 0, BmpW, BmpH )
      NEXT

      DEFINE IMAGELIST Imagelst_3 ;
         BUTTONSIZE BmpW, BmpH ;
         IMAGE {}

      FOR n := 1 TO Len( aColorWin10 )
         HMG_SetColor10Btm( aColorWin10[ n, 1 ], 0, BmpW, BmpH )
         HMG_SetColor10Btm( aColorWin10[ n, 1 ], 1, BmpW, BmpH )
         HMG_SetColor10Btm( aColorWin10[ n, 1 ], 0, BmpW, BmpH )
      NEXT

      @ 10, 20 LABEL Label_1 VALUE "HMG Colors ComboColor" AUTOSIZE

      @ 33, 20 COMBOBOXEX ComboEx_1 ;
         WIDTH 150 HEIGHT nHCmb ;
         ITEMS aColorName ;
         VALUE 1 ;
         ON ENTER GetHMGColor( aColor ) ;
         FONT 'MS Sans Serif' SIZE 9 ;
         IMAGELIST "Imagelst_1" ;
         TOOLTIP "Extend Combo HMG color" ;
         ON CHANGE {| i | i := Form_1.ComboEx_1.VALUE, ;
         Form_1.Label_Down.BACKCOLOR := aColor[ i, 1 ] }

      @ 10, 190 LABEL Label_2 VALUE "System Colors ComboColor" AUTOSIZE

      @ 33, 200 COMBOBOXEX ComboEx_2 ;
         WIDTH 270 HEIGHT nHCmb ;
         ITEMS aSysColorName ;
         VALUE 1 ;
         ON ENTER GetSystemColor( aSysColor ) ;
         FONT 'MS Sans serif' SIZE 9 ;
         IMAGELIST "Imagelst_2" ;
         TOOLTIP "Extend Combo System Color" ;
         ON CHANGE {| i | i := Form_1.ComboEx_2.VALUE, ;
         Form_1.Label_Down.BACKCOLOR := HMG_n2RGB( GetSysColor( aSysColor[ i, 1 ] ) ) }

      @ 80, 20 LABEL Label_3 VALUE "Windows 10 and higher" AUTOSIZE

      @ 103, 20 COMBOBOXEX ComboEx_3 ;
         WIDTH 450 HEIGHT nHCmb ;
         ITEMS aSysColor10 ;
         VALUE 1 ;
         ON ENTER GetWin10Color( aColorWin10 ) ;
         FONT 'MS Sans serif' SIZE 9 ;
         IMAGELIST "Imagelst_3" ;
         TOOLTIP "Windows 10 Combo System Color" ;
         ON CHANGE {| i | i := Form_1.ComboEx_3.VALUE, ;
         Form_1.Label_Down.BACKCOLOR := aColorWin10[ i, 1 ] }

      cVal := HMG_AppsUseTheme( .T. )

      @ 80, 210 LABEL Label_4 VALUE "Theme windows: " + cVal AUTOSIZE

      @ 160, 20 CHECKBOX Chk_1 CAPTION "Enable dark theme Windows 10" ;
         VALUE lDark WIDTH 300 HEIGHT 20 FONTCOLOR BLACK TRANSPARENT ;
         ON CHANGE {|| lDark := This.VALUE, WinDarkTheme( lDark ) }

      cVal := "(if the checkbox is not enabled in the color settings: Window titles and window borders)"
      @ 180, 40 LABEL Label_5 VALUE cVal WIDTH nW - 40 HEIGHT 40 FONTCOLOR RED TRANSPARENT

      @ nH - nG - GetMenuBarHeight(), 0 LABEL Label_Down VALUE "" WIDTH nW HEIGHT nG ;
         BACKCOLOR WHITE

   END WINDOW

   Form_1.CENTER
   Form_1.ACTIVATE

RETURN NIL

*------------------------------------------------
FUNCTION WinDarkTheme( lDark )
/*
 *  This function sets the dark theme for the window.
 *  It calls the SetWindowDarkTheme function (defined in the #pragma section) to apply the dark theme.
 *  This function is crucial for enabling or disabling the dark theme in Windows 10 applications,
 *  providing a more visually appealing experience for users who prefer dark mode.
 *
 *  Input parameters:
 *      lDark - A logical value indicating whether to enable (TRUE) or disable (FALSE) the dark theme.
 *
 *  Return value:
 *      NIL
 */
*------------------------------------------------
   SetWindowDarkTheme( _HMG_MainHandle, 0, lDark )

RETURN NIL

*------------------------------------------------
FUNCTION HMG_AppsUseTheme( lRet )
/*
 *  This function reads the Windows registry to determine whether the application is using the light or dark theme.
 *  It retrieves the value of the "AppsUseLightTheme" registry key, which indicates the user's preferred app theme.
 *  This information is used to adjust the application's appearance to match the user's system settings.
 *
 *  Input parameters:
 *      lRet - A logical value indicating whether to return the theme name (TRUE) or the registry value (FALSE).
 *             Defaults to TRUE if not provided.
 *
 *  Return value:
 *      If lRet is TRUE: Returns a character string indicating the theme ("Dark theme" or "Light theme").
 *      If lRet is FALSE: Returns the integer value read from the registry (0 for dark theme, 1 for light theme).
 */
*------------------------------------------------
   LOCAL i, cRet

   DEFAULT lRet := .T.
   i := win_regRead( "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Themes\Personalize\AppsUseLightTheme" )
   IF i == 0
      cRet := "Dark theme"
   ELSE
      cRet := "Light theme"
   ENDIF

RETURN iif( lRet, cRet, i )

*------------------------------------------------
FUNCTION Color10_ActiveCaption
/*
 *  This function determines the color of the active window caption in Windows 10 and later.
 *  It reads values from the Windows registry to determine the color based on the current theme settings.
 *  The function first checks the "ColorPrevalence" flag to determine whether to use the accent color or the default theme color.
 *  This ensures that the application uses the correct color for the active window caption, regardless of the user's theme settings.
 *
 *  Input parameters:
 *      None
 *
 *  Return value:
 *      An array containing the RGB color values for the active window caption.
 */
*------------------------------------------------
   LOCAL i, xKey, aClr
   // If the ColorPrevalence flag is set to 0 then the color of the window is either black or white.
   // If the flag is set to 1 then the window color is taken from the AccentColor value.
   i := win_regRead( "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\DWM\ColorPrevalence" )
   IF i == 1
      xKey := win_regRead( "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\DWM\AccentColor" )
      aClr := HMG_n2RGB( xKey )
   ELSE
      IF HMG_AppsUseTheme( .F. ) == 0
         aClr := BLACK
      ELSE
         aClr := WHITE
      ENDIF
   ENDIF

RETURN aClr

*------------------------------------------------
FUNCTION Color10_ActiveCaptionKey( i )
/*
 *  This function returns either BLACK or WHITE based on the input parameter.
 *  It's used to determine the active caption color when the ColorPrevalence flag is 0.
 *  This function provides a simple way to select the appropriate color based on the theme.
 *
 *  Input parameters:
 *      i - An integer value. If i is 0, returns BLACK; otherwise, returns WHITE.
 *
 *  Return value:
 *      BLACK or WHITE color constant.
 */
RETURN iif( i == 0, BLACK, WHITE )

*------------------------------------------------
FUNCTION HMG_SetColorBtm( aColor, bChecked, BmpWidh, BmpHeight )
/*
 *  This function creates a color bitmap and adds it to the imagelist.
 *  It's used to display the colors in the comboboxes.
 *  The function creates a bitmap with the specified color and an optional checkmark,
 *  and then adds it to the imagelist associated with the combobox. This allows the combobox
 *  to display a visual representation of the color options.
 *
 *  Input parameters:
 *      aColor - An array containing the RGB color values.
 *      bChecked - A logical value indicating whether to draw a checkmark on the bitmap.
 *      BmpWidh - The width of the bitmap.
 *      BmpHeight - The height of the bitmap.
 *
 *  Return value:
 *      NIL
 */
*------------------------------------------------
   LOCAL hImage, hImageLst, nColor
   hImageLst := This.imagelst_1.Handle
   nColor := RGB( aColor[ 1 ], aColor[ 2 ], aColor[ 3 ] )
   hImage := CreateColorBMP( ThisWindow.Handle, BmpWidh, BmpHeight, nColor, bChecked )
   IL_AddMaskedIndirect( hImageLst, hImage, , BmpWidh, BmpHeight, 1 )

RETURN NIL


*------------------------------------------------
FUNCTION HMG_SetSysColorBtm( COLOR, bChecked, BmpWidh, BmpHeight )
/*
 *  This function creates a color bitmap for a system color and adds it to the imagelist.
 *  It's used to display the system colors in the comboboxes.
 *  This function is similar to HMG_SetColorBtm, but it uses system color constants
 *  to retrieve the color values. This allows the combobox to display system-defined colors,
 *  such as the active window caption color.
 *
 *  Input parameters:
 *      COLOR - The system color constant (e.g., COLOR_ACTIVECAPTION).
 *      bChecked - A logical value indicating whether to draw a checkmark on the bitmap.
 *      BmpWidh - The width of the bitmap.
 *      BmpHeight - The height of the bitmap.
 *
 *  Return value:
 *      NIL
 */
*------------------------------------------------
   LOCAL hImage, hImageLst, nColor
   hImageLst := This.imagelst_2.Handle
   nColor := GetSysColor( Color )
   hImage := CreateColorBMP( ThisWindow.Handle, BmpWidh, BmpHeight, nColor, bChecked )
   IL_AddMaskedIndirect( hImageLst, hImage, , BmpWidh, BmpHeight, 1 )

RETURN NIL

*------------------------------------------------
FUNCTION HMG_SetColor10Btm( aColor, bChecked, BmpWidh, BmpHeight )
/*
 *  This function creates a color bitmap for a Windows 10 color and adds it to the imagelist.
 *  It's used to display the Windows 10 colors in the comboboxes.
 *  This function is similar to HMG_SetColorBtm, but it uses Windows 10 color values
 *  retrieved from the registry. This allows the combobox to display Windows 10-specific colors,
 *  such as the accent color.
 *
 *  Input parameters:
 *      aColor - An array containing the RGB color values.
 *      bChecked - A logical value indicating whether to draw a checkmark on the bitmap.
 *      BmpWidh - The width of the bitmap.
 *      BmpHeight - The height of the bitmap.
 *
 *  Return value:
 *      NIL
 */
*------------------------------------------------
   LOCAL hImage, hImageLst, nColor
   hImageLst := This.imagelst_3.Handle
   nColor := RGB( aColor[ 1 ], aColor[ 2 ], aColor[ 3 ] )
   hImage := CreateColorBMP( ThisWindow.Handle, BmpWidh, BmpHeight, nColor, bChecked )
   IL_AddMaskedIndirect( hImageLst, hImage, , BmpWidh, BmpHeight, 1 )

RETURN NIL

*------------------------------------------------
FUNCTION GetHMGColor( aColor )
/*
 *  This function displays information about the selected HMG color in a message box.
 *  It retrieves the selected color from the combobox and displays its name and RGB values in a message box.
 *  This provides the user with detailed information about the selected color.
 *
 *  Input parameters:
 *      aColor - An array containing the HMG color definitions.
 *
 *  Return value:
 *      NIL
 */
*------------------------------------------------
   LOCAL nPos, cStr, aColorHMG
   nPos := Form_1.ComboEx_1.VALUE
   aColorHMG := aColor[ nPos, 1 ]
   IF nPos > 0
      cStr := aColor[ nPos, 2 ] + '  { ' + AllTrim( Str( aColorHMG[ 1 ] ) ) + ',' + AllTrim( Str( aColorHMG[ 2 ] ) ) + ',' + AllTrim( Str( aColorHMG[ 3 ] ) ) + ' }'
      MsgInfo( cStr, 'Selected color' )
   ENDIF

RETURN NIL

*------------------------------------------------
FUNCTION GetWin10Color( aColor )
/*
 *  This function displays information about the selected Windows 10 color in a message box.
 *  It retrieves the selected color from the combobox and displays its name and RGB values in a message box.
 *  This provides the user with detailed information about the selected Windows 10 color.
 *
 *  Input parameters:
 *      aColor - An array containing the Windows 10 color definitions.
 *
 *  Return value:
 *      NIL
 */
*------------------------------------------------
   LOCAL nPos, cStr, aColorWin
   nPos := Form_1.ComboEx_3.VALUE
   aColorWin := aColor[ nPos, 1 ]
   IF nPos > 0
      cStr := aColor[ nPos, 2 ] + '  { ' + AllTrim( Str( aColorWin[ 1 ] ) ) + ',' + AllTrim( Str( aColorWin[ 2 ] ) ) + ',' + AllTrim( Str( aColorWin[ 3 ] ) ) + ' }'
      MsgInfo( cStr, 'Selected color' )
   ENDIF

RETURN NIL

*------------------------------------------------
FUNCTION GetSystemColor( aColor )
/*
 *  This function displays information about the selected system color in a message box.
 *  It retrieves the selected color from the combobox and displays its name and RGB values in a message box.
 *  This provides the user with detailed information about the selected system color.
 *
 *  Input parameters:
 *      aColor - An array containing the system color definitions.
 *
 *  Return value:
 *      NIL
 */
*------------------------------------------------
   LOCAL nPos, cStr, nColorSys
   nPos := Form_1.ComboEx_2.VALUE
   nColorSys := GetSysColor ( aColor[ nPos, 1 ] )
   IF nPos > 0
      cStr := aColor[ nPos, 2 ] + '  { ' + AllTrim( Str( GetRed( nColorSys ) ) ) + ',' + AllTrim( Str( GetGreen( nColorSys ) ) ) + ',' + AllTrim( Str( GetBlue( nColorSys ) ) ) + ' }'
      MsgInfo( cStr, 'Selected color' )
   ENDIF

RETURN NIL

*------------------------------------------------
FUNCTION ColorHMG()
/*
 *  This function defines an array of HMG colors.
 *  It creates an array containing the color constants and their names.
 *  This array is used to populate the HMG color combobox.
 *
 *  Input parameters:
 *      None
 *
 *  Return value:
 *      An array containing the HMG color definitions. Each element in the array is an array
 *      containing the color constant and its name (e.g., { YELLOW, "YELLOW" }).
 */
*------------------------------------------------
   LOCAL aColor := ;
      { { YELLOW, "YELLOW" }, ;
      { PINK, "PINK" }, ;
      { RED, "RED" }, ;
      { FUCHSIA, "FUCHSIA" }, ;
      { BROWN, "BROWN" }, ;
      { ORANGE, "ORANGE" }, ;
      { GREEN, "GREEN" }, ;
      { PURPLE, "PURPLE" }, ;
      { BLACK, "BLACK" }, ;
      { WHITE, "WHITE" }, ;
      { GRAY, "GRAY" }, ;
      { BLUE, "BLUE" }, ;
      { SILVER, "SILVER" }, ;
      { MAROON, "MAROON" }, ;
      { OLIVE, "OLIVE" }, ;
      { LGREEN, "LGREEN" }, ;
      { AQUA, "AQUA" }, ;
      { NAVY, "NAVY" }, ;
      { TEAL, "TEAL" } }

RETURN aColor

*------------------------------------------------
FUNCTION ColorHMGSys()
/*
 *  This function defines an array of system colors.
 *  It creates an array containing the system color constants and their names.
 *  This array is used to populate the system color combobox.
 *
 *  Input parameters:
 *      None
 *
 *  Return value:
 *      An array containing the system color definitions. Each element in the array is an array
 *      containing the system color constant and its name (e.g., { COLOR_ACTIVECAPTION, "Active window caption" }).
 */
*------------------------------------------------
   LOCAL aColor := ;
      { { COLOR_GRADIENTACTIVECAPTION, "GRADIENTACTIVECAPTION" }, ;
      { COLOR_GRADIENTINACTIVECAPTION, "GRADIENTINACTIVECAPTION" }, ;
      { COLOR_3DDKSHADOW, "Dark shadow for 3D display elements" }, ;
      { COLOR_BTNFACE, "Face color for 3D display elements" }, ;
      { COLOR_BTNHIGHLIGHT, "Highlight color for 3D display elements" }, ;
      { COLOR_3DLIGHT, "Light color for 3D display elements" }, ;
      { COLOR_BTNSHADOW, "Shadow color for 3D display elements" }, ;
      { COLOR_ACTIVEBORDER, "Active window border" }, ;
      { COLOR_ACTIVECAPTION, "Active window caption" }, ;
      { COLOR_APPWORKSPACE, "Background color of MDI applications" }, ;
      { COLOR_BACKGROUND, "Desktop" }, ;
      { COLOR_BTNTEXT, "Text on push buttons" }, ;
      { COLOR_CAPTIONTEXT, "Text in caption, size, and scroll arrow box" }, ;
      { COLOR_GRAYTEXT, "Grayed (disabled) text" }, ;
      { COLOR_HIGHLIGHT, "Item(s) selected in a control" }, ;
      { COLOR_HIGHLIGHTTEXT, "Text of item(s) selected in a control" }, ;
      { COLOR_INACTIVEBORDER, "Inactive window border" }, ;
      { COLOR_INACTIVECAPTION, "Inactive window caption" }, ;
      { COLOR_INACTIVECAPTIONTEXT, "Color of text in an inactive caption" }, ;
      { COLOR_INFOBK, "Background color for tooltip controls" }, ;
      { COLOR_INFOTEXT, "Text color for tooltip controls" }, ;
      { COLOR_MENU, "Menu background" }, ;
      { COLOR_MENUTEXT, "Text in menus" }, ;
      { COLOR_SCROLLBAR, "Scroll bar gray area" }, ;
      { COLOR_WINDOW, "Window background" }, ;
      { COLOR_WINDOWFRAME, "Window frame" }, ;
      { COLOR_WINDOWTEXT, "Text in windows" } }

RETURN aColor

*------------------------------------------------
FUNCTION ColorHMGWin10()
/*
 *  This function defines an array of Windows 10 colors.
 *  It creates an array containing the color values (obtained from registry or calculated), their names, and the registry keys
 *  associated with the colors. This array is used to populate the Windows 10 color combobox.
 *
 *  Input parameters:
 *      None
 *
 *  Return value:
 *      An array containing the Windows 10 color definitions. Each element in the array is an array
 *      containing the color value (obtained from registry or calculated), its name, and the registry key
 *      associated with the color (e.g., { Color10_ActiveCaption(), "Active window caption", "AccentColor" }).
 */
*------------------------------------------------
   LOCAL aColor := ;
      { { Color10_ActiveCaption(), "Active window caption", "AccentColor" }, ;
      { Color10_ActiveCaptionKey( 0 ), "Active window caption: dark theme", "ColorPrevalence = 0" }, ;
      { Color10_ActiveCaptionKey( 1 ), "Active window caption: light theme", "ColorPrevalence = 0" } }

RETURN aColor

*------------------------------------------------

#pragma BEGINDUMP

#include <mgdefs.h>
#include <commctrl.h>

extern HB_PTRUINT wapi_GetProcAddress( HMODULE hModule, LPCSTR lpProcName );
typedef HRESULT (WINAPI *DwmSetWindowAttributeCallback)(HWND hwnd, DWORD dwAttribute, LPCVOID pvAttribute, DWORD cbAttribute);

static HINSTANCE libDWM;
static DwmSetWindowAttributeCallback DwmSetWindowAttribute;

HB_FUNC ( SETWINDOWDARKTHEME )
/*
 *  This function sets the dark mode theme for a given window.
 *  It uses the DwmSetWindowAttribute function from dwmapi.dll to apply the dark theme.
 *  This function directly interacts with the Windows API to change the window's appearance.
 *
 *  Input parameters:
 *      handle - The handle of the window to apply the dark theme to.
 *      isLegacy - A logical value indicating whether to use the legacy dark theme (Windows 10 versions before 2004).
 *      isDarkMode - A logical value indicating whether to enable (TRUE) or disable (FALSE) the dark theme.
 *
 *  Return value:
 *      None (void). The function modifies the window's appearance directly.
 */
{
   BOOL isDarkMode;

   HWND handle;
   BOOL isLegacy;
   DWORD code;

   handle = (HWND) HB_PARNL( 1 );
   isLegacy = hb_parl( 2 );
   isDarkMode = hb_parl( 3 );

   if (libDWM == NULL) libDWM = LoadLibrary(TEXT("dwmapi.dll"));

   if (DwmSetWindowAttribute == NULL) {
      DwmSetWindowAttribute = (DwmSetWindowAttributeCallback) wapi_GetProcAddress(libDWM, "DwmSetWindowAttribute");
   }

   // Windows 10 versions before 2004 = 19
   // Windows 10 like 22H2 and Windows 11 = 20
   code = ( isLegacy == 0 ? 20 : 19 );

   DwmSetWindowAttribute(handle, code, &isDarkMode, sizeof(isDarkMode));

   SendMessage(handle, WM_NCACTIVATE, 0, 0);
   SendMessage(handle, WM_NCACTIVATE, 1, 0);
}

static void GoToPoint( HDC hDC, int ix, int iy )
/*
 *  This function moves the current position of the pen in the specified device context (DC) to a new location.
 *  It's a helper function used by CreateColorBMP to draw the checkmark.
 *
 *  Input parameters:
 *      hDC - A handle to the device context.
 *      ix - The x-coordinate of the new location.
 *      iy - The y-coordinate of the new location.
 *
 *  Return value:
 *      None (void). The function modifies the device context directly.
 */
{
   POINT pt;
   MoveToEx( hDC, ix, iy, &pt );
}


HB_FUNC ( CREATECOLORBMP )
/*
 *  This function creates a color bitmap with an optional checkmark.
 *  It's used to generate the images displayed in the comboboxes, providing a visual representation of the colors.
 *
 *  Input parameters:
 *      hwnd - The handle of the window.
 *      BmpWidh - The width of the bitmap.
 *      BmpHeight - The height of the bitmap.
 *      nColor - The color of the bitmap.
 *      bChecked - A logical value indicating whether to draw a checkmark on the bitmap.
 *
 *  Return value:
 *      HBITMAP - A handle to the created bitmap.
 */
{
   HBRUSH hOldBrush;
   HBRUSH hColorBrush;
   HBRUSH hBlackBrush = CreateSolidBrush( RGB( 0, 0, 0 ) );
   HBRUSH hWhiteBrush = CreateSolidBrush( RGB( 255, 255, 255 ) );
   HPEN   hBlackPen   = CreatePen( PS_SOLID, 1, RGB( 0, 0, 0 ) );

   RECT rect;
   HBITMAP hBmp;
   COLORREF clr   = hb_parnl( 4 );
   int bChecked   = hb_parnl( 5 );
   int width      = hb_parni( 2 );
   int height     = hb_parni( 3 );
   HWND handle    = (HWND) HB_PARNL( 1 );
   HDC imgDC      = GetDC ( handle );
   HDC tmpDC      = CreateCompatibleDC(imgDC);

   if( ( width==0 ) & ( height==0 ) )
   {
      width  = 16;
      height = 16;
   }

   SetRect(&rect,0,0,width,height);   // Size Bmp

   hBmp=CreateCompatibleBitmap(imgDC,width,height);
   SelectObject(tmpDC,hBmp);

   hOldBrush = SelectObject( tmpDC, hWhiteBrush );

   FillRect( tmpDC, &rect, hWhiteBrush );

   rect.left   += 1 ;
   rect.top    += 1 ;
   rect.right  -= 1 ;
   rect.bottom -= 1 ;
   FillRect( tmpDC, &rect, hBlackBrush );

   rect.top    += 1 ;
   rect.left   += 1 ;
   rect.right  -= 1 ;
   rect.bottom -= 1 ;

   hColorBrush = CreateSolidBrush( clr );

   SelectObject( tmpDC, hColorBrush );

   FillRect( tmpDC, &rect, hColorBrush );

   rect.top    += 1 ;
   rect.right  -= 4 ;
   rect.bottom -= 1 ;

   if( bChecked == 1 )
   {
      GoToPoint( tmpDC, rect.right, rect.top );

      SelectObject( tmpDC, hBlackPen );

      LineTo( tmpDC, rect.right - 4 , rect.bottom - 3 );
      LineTo( tmpDC, rect.right - 7, rect.bottom - 6 );

      GoToPoint( tmpDC, rect.right, rect.top + 1);
      LineTo( tmpDC, rect.right - 4 , rect.bottom - 2 );
      LineTo( tmpDC, rect.right - 7, rect.bottom - 5 );

      GoToPoint( tmpDC, rect.right, rect.top + 2);
      LineTo( tmpDC, rect.right - 4 , rect.bottom - 1 );
      LineTo( tmpDC, rect.right - 7, rect.bottom - 4 );
   }

   SelectObject( tmpDC, hOldBrush );
   DeleteObject( hBlackBrush );
   DeleteObject( hWhiteBrush );
   DeleteObject( hColorBrush );
   DeleteObject( hBlackPen );


   DeleteDC( imgDC );
   DeleteDC( tmpDC );

   HB_RETNL( ( LONG_PTR ) hBmp );
}

HB_FUNC( IL_ADDMASKEDINDIRECT )
/*
 *  This function adds a masked bitmap to an image list.
 *  It's used to add the color bitmaps created by CreateColorBMP to the imagelist,
 *  allowing them to be displayed in the comboboxes. The mask ensures that the background
 *  of the bitmap is transparent.
 *
 *  Input parameters:
 *      hwnd - The handle of the image list.
 *      himage - The handle of the bitmap to add.
 *      color - The color to use as the mask.  CLR_NONE indicates no mask.
 *      ix - The width of each image in the bitmap.
 *      iy - The height of each image in the bitmap.
 *      imagecount - The number of images in the bitmap.
 *
 *  Return value:
 *      LRESULT - The index of the image added to the image list, or -1 if the function fails.
 */
{
   BITMAP   bm;
   HBITMAP  himage = ( HBITMAP ) HB_PARNL( 2 );
   COLORREF clrBk   = CLR_NONE;
   LRESULT  lResult = -1;
   int      ic      = 1;

   if( hb_parnl( 3 ) )
      clrBk = ( COLORREF ) hb_parnl( 3 );

   if( hb_parni( 6 ) )
      ic = hb_parni( 6 );

   if( GetObject( himage, sizeof( BITMAP ), &bm ) != 0 )
   {
      if( ( hb_parni( 4 ) * ic == bm.bmWidth ) & ( hb_parni( 5 ) == bm.bmHeight ) )
         lResult = ImageList_AddMasked( ( HIMAGELIST ) HB_PARNL( 1 ), himage, clrBk );

      DeleteObject( himage );
   }

   hb_retni( lResult );
}

#pragma ENDDUMP
