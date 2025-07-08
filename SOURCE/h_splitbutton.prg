/*
 * MINIGUI - Harbour Win32 GUI library source code
 *
 * Copyright 2017 Grigory Filatov <gfilatov@gmail.com>
*/

#include "minigui.ch"

#ifdef _USERINIT_
*------------------------------------------------------------------------------*
INIT PROCEDURE _InitSPButton
*------------------------------------------------------------------------------*

   InstallEventHandler  ( 'SPButtonEventHandler' )
   InstallMethodHandler ( 'Release', 'ReleaseSPButtonImageList' )
   InstallMethodHandler ( 'SetFocus', 'SPButtonSetFocus' )
   InstallMethodHandler ( 'Enable', 'SPButtonEnable' )
   InstallMethodHandler ( 'Disable', 'SPButtonDisable' )
   InstallPropertyHandler ( 'Icon', 'SetSPButtonPicture', 'GetSPButtonPicture' )

RETURN

*------------------------------------------------------------------------------*
PROCEDURE _DefineSplitButton ( cName, nRow, nCol, cCaption, bAction, cParent, ;
   lDefault, w, h, tooltip, fontname, fontsize, bold, italic, underline, strikeout, cIcon )
*------------------------------------------------------------------------------*
   LOCAL hControlHandle, hParentFormHandle
   LOCAL FontHandle
   LOCAL mVar
   LOCAL nId
   LOCAL k

   IF _HMG_BeginWindowActive
      cParent := _HMG_ActiveFormName
   ENDIF

   // If defined inside a Tab structure, adjust position and determine cParent

   IF _HMG_FrameLevel > 0
      nCol += _HMG_ActiveFrameCol[ _HMG_FrameLevel ]
      nRow += _HMG_ActiveFrameRow[ _HMG_FrameLevel ]
      cParent := _HMG_ActiveFrameParentFormName[ _HMG_FrameLevel ]
   ENDIF

   IF .NOT. _IsWindowDefined ( cParent )
      MsgMiniGuiError( "Window: " + cParent + " is not defined." )
   ENDIF

   IF ISCHAR ( cName ) .AND. cName == "0"
      cName := HMG_GetUniqueName()
   ENDIF

   IF _IsControlDefined ( cName, cParent )
      MsgMiniGuiError ( "Control: " + cName + " Of " + cParent + " Already defined." )
   ENDIF

   hb_default( @w, 148 )
   hb_default( @h, 38 )

   IF ( FontHandle := GetFontHandle( FontName ) ) != 0
      GetFontParamByRef( FontHandle, @FontName, @FontSize, @bold, @italic, @underline, @strikeout )
   ENDIF

   mVar := '_' + cParent + '_' + cName

   k := _GetControlFree()
   nId := _GetId()

   hParentFormHandle := GetFormHandle ( cParent )

   hControlHandle := InitSplitButton ( ;
      hParentFormHandle, ;
      nRow, ;
      nCol, ;
      cCaption, ;
      lDefault, ;
      w, h, ;
      nId ;
      )

   IF _HMG_BeginTabActive
      AAdd ( _HMG_ActiveTabCurrentPageMap, hControlHandle )
   ENDIF

   IF FontHandle != 0
      _SetFontHandle( hControlHandle, FontHandle )
   ELSE
      __defaultNIL( @FontName, _HMG_DefaultFontName )
      __defaultNIL( @FontSize, _HMG_DefaultFontSize )
      FontHandle := _SetFont ( hControlHandle, FontName, FontSize, bold, italic, underline, strikeout )
   ENDIF

#ifdef _NAMES_LIST_
   _SetNameList( mVar , k )
#else
   Public &mVar. := k
#endif

   _HMG_aControlType [k] := 'SPBUTTON'
   _HMG_aControlNames [k] := cName
   _HMG_aControlHandles [k] := hControlHandle
   _HMG_aControlParenthandles [k] := hParentFormHandle
   _HMG_aControlIds [k] :=  nId
   _HMG_aControlProcedures [k] := bAction
   _HMG_aControlPageMap [k] :=  {}
   _HMG_aControlValue [k] :=  Nil
   _HMG_aControlInputMask [k] :=  ""
   _HMG_aControllostFocusProcedure [k] :=  ""
   _HMG_aControlGotFocusProcedure [k] :=  ""
   _HMG_aControlChangeProcedure [k] :=  ""
   _HMG_aControlDeleted [k] :=  .F.
   _HMG_aControlBkColor [k] :=   Nil
   _HMG_aControlFontColor [k] :=  Nil
   _HMG_aControlDblClick [k] :=  ""
   _HMG_aControlHeadClick [k] :=  {}
   _HMG_aControlRow [k] := nRow
   _HMG_aControlCol [k] := nCol
   _HMG_aControlWidth [k] := w
   _HMG_aControlHeight [k] := h
   _HMG_aControlSpacing [k] := 0
   _HMG_aControlContainerRow [k] :=  iif ( _HMG_FrameLevel > 0, _HMG_ActiveFrameRow[ _HMG_FrameLevel ], -1 )
   _HMG_aControlContainerCol [k] :=  iif ( _HMG_FrameLevel > 0, _HMG_ActiveFrameCol[ _HMG_FrameLevel ], -1 )
   _HMG_aControlPicture [k] :=  Nil
   _HMG_aControlContainerHandle [k] :=  0
   _HMG_aControlFontName [k] :=  fontname
   _HMG_aControlFontSize [k] :=  fontsize
   _HMG_aControlFontAttributes [k] := { bold, italic, underline, strikeout }
   _HMG_aControlToolTip [k] :=  tooltip
   _HMG_aControlRangeMin [k] :=  0
   _HMG_aControlRangeMax [k] :=  0
   _HMG_aControlCaption [k] :=  cCaption
   _HMG_aControlVisible [k] :=  .T.
   _HMG_aControlHelpId [k] :=  0
   _HMG_aControlFontHandle [k] :=  FontHandle
   _HMG_aControlBrushHandle [k] :=  0
   _HMG_aControlEnabled [k] :=  .T.
   _HMG_aControlMiscData1 [k] := 0
   _HMG_aControlMiscData2 [k] := ''

   IF _HMG_lOOPEnabled
      Eval ( _HMG_bOnControlInit, k, mVar )
   ENDIF

   IF tooltip != NIL
      SetToolTip ( hControlHandle , tooltip , GetFormToolTipHandle ( cParent ) )
   ENDIF

   IF ! Empty( cIcon )
      _HMG_aControlPicture [k] := cIcon
      _HMG_aControlBrushHandle [k] := SPButton_SetIcon( hControlHandle, cIcon )
   ENDIF

RETURN

#define WM_COMMAND   0x0111
#define BN_CLICKED   0
#define WM_NOTIFY    0x004E
#define BCN_FIRST    -1250
#define BCN_DROPDOWN (BCN_FIRST + 0x0002)
*------------------------------------------------------------------------------*
FUNCTION SPButtonEventHandler ( hWnd, nMsg, wParam, lParam )
*------------------------------------------------------------------------------*
   LOCAL xRetVal := Nil
   LOCAL i

   HB_SYMBOL_UNUSED( hWnd )

   IF nMsg == WM_NOTIFY

      IF GetNotifyCode ( lParam ) == BCN_DROPDOWN  // Notify for dropdown button
         xRetVal := 0
         LaunchDropdownMenu( GetHwndFrom ( lParam ) )
      ENDIF

   ELSEIF nMsg == WM_COMMAND

      i := AScan ( _HMG_aControlHandles, lParam )

      IF i > 0 .AND. _HMG_aControlType[ i ] == 'SPBUTTON'

         IF HiWord ( wParam ) == BN_CLICKED
            xRetVal := 0
            _DoControlEventProcedure ( _HMG_aControlProcedures[ i ], i )
         ENDIF

      ENDIF

   ENDIF

RETURN xRetVal

#define BM_SETSTYLE        244
#define BS_SPLITBUTTON     0x0000000C
#define BS_DEFSPLITBUTTON  0x0000000D
*------------------------------------------------------------------------------*
PROCEDURE SPButtonSetFocus ( cWindow, cControl )
*------------------------------------------------------------------------------*
   LOCAL hWnd
   LOCAL ParentFormHandle
   LOCAL ControlCount
   LOCAL x

   IF GetControlType ( cControl, cWindow ) == 'SPBUTTON'

      _HMG_UserComponentProcess := .T.

      hWnd := GetControlHandle ( cControl, cWindow )

      ControlCount := Len ( _HMG_aControlNames )
      ParentFormHandle := _HMG_aControlParentHandles [ GetControlIndex ( cControl, cWindow ) ]
      FOR x := 1 TO ControlCount
         IF _HMG_aControlType [x] == 'SPBUTTON'
            IF _HMG_aControlParentHandles [x] == ParentFormHandle
               SendMessage ( _HMG_aControlHandles [x], BM_SETSTYLE, BS_SPLITBUTTON, LOWORD( 1 ) )
            ENDIF
         ENDIF
      NEXT

      SetFocus( hWnd )
      SendMessage ( hWnd, BM_SETSTYLE, BS_DEFSPLITBUTTON, LOWORD( 1 ) )

   ELSE

      _HMG_UserComponentProcess := .F.

   ENDIF

RETURN

*------------------------------------------------------------------------------*
PROCEDURE SPButtonEnable ( cWindow, cControl )
*------------------------------------------------------------------------------*

   IF GetControlType ( cControl, cWindow ) == 'SPBUTTON'

      EnableWindow ( GetControlHandle ( cControl, cWindow ) )

      _HMG_UserComponentProcess := .T.

   ELSE

      _HMG_UserComponentProcess := .F.

   ENDIF

RETURN

*------------------------------------------------------------------------------*
PROCEDURE SPButtonDisable ( cWindow, cControl )
*------------------------------------------------------------------------------*

   IF GetControlType ( cControl, cWindow ) == 'SPBUTTON'

      DisableWindow ( GetControlHandle ( cControl, cWindow ) )

      _HMG_UserComponentProcess := .T.

   ELSE

      _HMG_UserComponentProcess := .F.

   ENDIF

RETURN

*------------------------------------------------------------------------------*
PROCEDURE SetSPButtonPicture ( cWindow, cControl, cProperty, cIcon )
*------------------------------------------------------------------------------*
   LOCAL i
   cProperty := NIL // Unused variable

   IF GetControlType ( cControl, cWindow ) == 'SPBUTTON'

      i := GetControlIndex ( cControl, cWindow )

      _HMG_aControlPicture[ i ] := cIcon

      IF ! Empty( _HMG_aControlBrushHandle[ i ] )
         IMAGELIST_DESTROY ( _HMG_aControlBrushHandle[ i ] )
      ENDIF

      _HMG_aControlBrushHandle[ i ] := SPButton_SetIcon( GetControlHandle ( cControl, cWindow ), cIcon )

      _HMG_UserComponentProcess := .T.

   ELSE

      _HMG_UserComponentProcess := .F.

   ENDIF

RETURN

*------------------------------------------------------------------------------*
FUNCTION GetSPButtonPicture ( cWindow, cControl )
*------------------------------------------------------------------------------*
   LOCAL RetVal

   IF GetControlType ( cControl, cWindow ) == 'SPBUTTON'

      _HMG_UserComponentProcess := .T.

      RetVal := _GetPicture ( cControl, cWindow )

   ELSE

      _HMG_UserComponentProcess := .F.

   ENDIF

RETURN RetVal

*------------------------------------------------------------------------------*
PROCEDURE ReleaseSPButtonImageList ( cWindow, cControl )
*------------------------------------------------------------------------------*
   LOCAL i

   IF _IsControlDefined ( cControl, cWindow ) .AND. GetControlType ( cControl, cWindow ) == 'SPBUTTON'

      i := GetControlIndex ( cControl, cWindow )

      IF ! Empty( _HMG_aControlBrushHandle[ i ] )
         IMAGELIST_DESTROY ( _HMG_aControlBrushHandle[ i ] )
      ENDIF

      _HMG_UserComponentProcess := .T.

   ELSE

      _HMG_UserComponentProcess := .F.

   ENDIF

RETURN

*------------------------------------------------------------------------------*
STATIC FUNCTION LaunchDropdownMenu( nHwnd )
*------------------------------------------------------------------------------*
   LOCAL aPos := {0, 0, 0, 0}
   LOCAL i

   IF ( i := AScan ( _HMG_aControlHandles, nHwnd ) ) > 0

      GetWindowRect( nHwnd, /*@*/aPos )

      TrackPopupMenu( _HMG_aControlRangeMax[ i ], aPos[ 1 ] + 1, aPos[ 2 ] + _HMG_aControlHeight[ i ], _HMG_aControlParentHandles[ i ] )

   ENDIF

RETURN NIL

*------------------------------------------------------------------------------*
* Low Level C Routines
*------------------------------------------------------------------------------*

#pragma BEGINDUMP

#define BS_SPLITBUTTON     0x0000000C
#define BS_DEFSPLITBUTTON  0x0000000D

#include <mgdefs.h>
#include <commctrl.h>
#if ( defined( __BORLANDC__ ) && __BORLANDC__ < 1410 )
// Button Class Name
#define WC_BUTTON              "Button"
#endif

#ifdef UNICODE
LPWSTR AnsiToWide( LPCSTR );
#endif

HINSTANCE GetResources( void );

HB_FUNC( INITSPLITBUTTON )
{
#ifndef UNICODE
   LPCSTR lpWindowName = hb_parc( 4 );
#else
   LPWSTR lpWindowName = AnsiToWide( ( char * ) hb_parc( 4 ) );
#endif

   DWORD Style = hb_parl( 5 ) ? BS_DEFSPLITBUTTON : BS_SPLITBUTTON;

   hmg_ret_raw_HWND
      (
         CreateWindow
            (
         WC_BUTTON,
         lpWindowName,
         Style | WS_CHILD | WS_CLIPCHILDREN | WS_CLIPSIBLINGS | BS_PUSHBUTTON | BS_CENTER | BS_TEXT | WS_VISIBLE | WS_TABSTOP,
         hb_parni( 3 ),
         hb_parni( 2 ),
         hb_parni( 6 ),
         hb_parni( 7 ),
         hmg_par_raw_HWND( 1 ),
         hmg_par_raw_HMENU( 8 ),
         GetModuleHandle( NULL ),
         NULL
            )
      );
}

#ifndef BCM_FIRST
#define BCM_FIRST         0x1600
#define BCM_SETIMAGELIST  ( BCM_FIRST + 0x0002 )
#endif

#if ( defined( __BORLANDC__ ) && __BORLANDC__ < 1410 ) || ( defined ( __MINGW32__ ) && defined ( __MINGW32_VERSION ) )
typedef struct
{
   HIMAGELIST himl;
   RECT       margin;
   UINT       uAlign;
} BUTTON_IMAGELIST, * PBUTTON_IMAGELIST;

#if (_WIN32_WINNT >= 0x501)
#define BUTTON_IMAGELIST_ALIGN_LEFT     0
#define BUTTON_IMAGELIST_ALIGN_RIGHT    1
#define BUTTON_IMAGELIST_ALIGN_TOP      2
#define BUTTON_IMAGELIST_ALIGN_BOTTOM   3
#define BUTTON_IMAGELIST_ALIGN_CENTER   4       // Doesn't draw text
#endif
#endif

HB_FUNC( SPBUTTON_SETICON )
{
   HICON            hIcon;
   BITMAP           bm;
   ICONINFO         sIconInfo;
   HIMAGELIST       himl = ( HIMAGELIST ) NULL;
   BUTTON_IMAGELIST bi;
#ifndef UNICODE
   LPCTSTR lpIconName = hb_parc( 2 );
#else
   LPWSTR  lpIconName = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif

   hIcon = ( HICON ) LoadImage( GetResources(), lpIconName, IMAGE_ICON, 0, 0, LR_DEFAULTCOLOR );

   if( hIcon == NULL )
   {
      hIcon = ( HICON ) LoadImage( 0, lpIconName, IMAGE_ICON, 0, 0, LR_LOADFROMFILE | LR_DEFAULTCOLOR );
   }

   if( GetIconInfo( hIcon, &sIconInfo ) )
   {
      GetObject( sIconInfo.hbmColor, sizeof( BITMAP ), ( LPVOID ) &bm );

      if( sIconInfo.hbmMask )
      {
         DeleteObject( sIconInfo.hbmMask );
      }

      if( sIconInfo.hbmColor )
      {
         DeleteObject( sIconInfo.hbmColor );
      }

      himl = ImageList_Create( bm.bmWidth, bm.bmHeight, ILC_COLOR32 | ILC_MASK, 1, 0 );

      ImageList_AddIcon( himl, hIcon );

      DestroyIcon( hIcon );

      bi.himl          = himl;
      bi.margin.top    = 4;
      bi.margin.bottom = 4;
      bi.margin.left   = 4;
      bi.margin.right  = 4;
      bi.uAlign        = BUTTON_IMAGELIST_ALIGN_LEFT;

      SendMessage( hmg_par_raw_HWND( 1 ), ( UINT ) BCM_SETIMAGELIST, ( WPARAM ) 0, ( LPARAM ) &bi );
   }

   hmg_ret_raw_HANDLE( himl );
}

#pragma ENDDUMP

#endif
