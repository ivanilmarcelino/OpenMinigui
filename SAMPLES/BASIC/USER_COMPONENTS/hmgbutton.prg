#include "i_winuser.ch"

#define BUTTON_IMAGELIST_ALIGN_LEFT   0
#define BUTTON_IMAGELIST_ALIGN_RIGHT  1
#define BUTTON_IMAGELIST_ALIGN_TOP    2
#define BUTTON_IMAGELIST_ALIGN_BOTTOM 3
#define BUTTON_IMAGELIST_ALIGN_CENTER 4

*------------------------------------------------------------------------------*
INIT PROCEDURE _InitHMGButton
*------------------------------------------------------------------------------*

   InstallEventHandler ( 'HMGButtonEventHandler' )
   InstallMethodHandler ( 'SetFocus', 'HMGButtonSetFocus' )
   InstallMethodHandler ( 'Enable', 'HMGButtonEnable' )
   InstallMethodHandler ( 'Disable', 'HMGButtonDisable' )
   InstallPropertyHandler ( 'Handle', 'SetHMGButtonHandle', 'GetHMGButtonHandle' )
   InstallPropertyHandler ( 'Caption', 'SetHMGButtonCaption', 'GetHMGButtonCaption' )
   InstallPropertyHandler ( 'Picture', 'SetHMGButtonPicture', 'GetHMGButtonPicture' )

RETURN

*------------------------------------------------------------------------------*
PROCEDURE _DefineMixedButton ( ControlName, ParentForm, x, y, caption, ;
      ProcedureName, w, h, FontName, FontSize, tooltip, ;
      gotfocus, lostfocus, flat, notabstop, HelpId, ;
      invisible, bold, italic, underline, strikeout, ;
      picture, alignment, multiline, notrans )
*------------------------------------------------------------------------------*
   LOCAL ControlHandle, ParentFormHandle, k, cMacroVar, FontHandle, aRet

   IF _HMG_BeginWindowActive
      ParentForm := _HMG_ActiveFormName
   ENDIF

   IF .NOT. _IsWindowDefined ( ParentForm )
      MsgMiniGuiError( "Window: " + ParentForm + " is not defined." )
   ENDIF

   IF _IsControlDefined ( ControlName, ParentForm )
      MsgMiniGuiError ( "Control: " + ControlName + " Of " + ParentForm + " Already defined." )
   ENDIF

   DEFAULT w TO 100
   DEFAULT h TO 28
   DEFAULT lostfocus TO ""
   DEFAULT gotfocus TO ""
   DEFAULT invisible TO FALSE

   IF ( FontHandle := GetFontHandle( FontName ) ) != 0
      GetFontParamByRef( FontHandle, @FontName, @FontSize, @bold, @italic, @underline, @strikeout )
   ENDIF

   IF ValType ( alignment ) = 'U'
      alignment := BUTTON_IMAGELIST_ALIGN_TOP
   ELSEIF ValType ( alignment ) = 'C'
      SWITCH AllTrim( HMG_UPPER( alignment ) )
      CASE 'LEFT'
         alignment := BUTTON_IMAGELIST_ALIGN_LEFT
         EXIT
      CASE 'RIGHT'
         alignment := BUTTON_IMAGELIST_ALIGN_RIGHT
         EXIT
      CASE 'TOP'
         alignment := BUTTON_IMAGELIST_ALIGN_TOP
         EXIT
      CASE 'BOTTOM'
         alignment := BUTTON_IMAGELIST_ALIGN_BOTTOM
         EXIT
      DEFAULT
         alignment := BUTTON_IMAGELIST_ALIGN_TOP
      ENDSWITCH
   ELSE
      alignment := BUTTON_IMAGELIST_ALIGN_TOP
   ENDIF

   cMacroVar := '_' + ParentForm + '_' + ControlName
   k := _GetControlFree()
   ParentFormHandle := GetFormHandle ( ParentForm )
   aRet := InitMixedButton ( ParentFormHandle, caption, 0, x, y, w, h, '', 0, FLAT, NOTABSTOP, invisible, picture, alignment, multiline, notrans )

   ControlHandle := aRet[ 1 ]

   IF FontHandle != 0
      _SetFontHandle( ControlHandle, FontHandle )
   ELSE
      __defaultNIL( @FontName, _HMG_DefaultFontName )
      __defaultNIL( @FontSize, _HMG_DefaultFontSize )
      FontHandle := _SetFont ( ControlHandle, FontName, FontSize, bold, italic, underline, strikeout )
   ENDIF

   IF _HMG_BeginTabActive
      AAdd ( _HMG_ActiveTabCurrentPageMap, Controlhandle )
   ENDIF

   IF tooltip != NIL
      SetToolTip ( ControlHandle, tooltip, GetFormToolTipHandle ( ParentForm ) )
   ENDIF

#ifdef _NAMES_LIST_
   _SetNameList( cMacroVar, k )

#else
   PUBLIC &cMacroVar. := k

#endif

   _HMG_aControlType[ k ] := 'HMGBUTTON'
   _HMG_aControlNames[ k ] := ControlName
   _HMG_aControlHandles[ k ] := ControlHandle
   _HMG_aControlParenthandles[ k ] := ParentFormHandle
   _HMG_aControlIds[ k ] := 0
   _HMG_aControlProcedures[ k ] := ProcedureName
   _HMG_aControlPageMap[ k ] := {}
   _HMG_aControlValue[ k ] := NIL
   _HMG_aControlInputMask[ k ] := ""
   _HMG_aControllostFocusProcedure[ k ] := lostfocus
   _HMG_aControlGotFocusProcedure[ k ] := gotfocus
   _HMG_aControlChangeProcedure[ k ] := ""
   _HMG_aControlDeleted[ k ] := .F.
   _HMG_aControlBkColor[ k ] := NIL
   _HMG_aControlFontColor[ k ] := NIL
   _HMG_aControlDblClick[ k ] := notrans
   _HMG_aControlHeadClick[ k ] := {}
   _HMG_aControlRow[ k ] := y
   _HMG_aControlCol[ k ] := x
   _HMG_aControlWidth[ k ] := w
   _HMG_aControlHeight[ k ] := h
   _HMG_aControlSpacing[ k ] := alignment
   _HMG_aControlContainerRow[ k ] := -1
   _HMG_aControlContainerCol[ k ] := -1
   _HMG_aControlPicture[ k ] := picture
   _HMG_aControlContainerHandle[ k ] := 0
   _HMG_aControlFontName[ k ] := FontName
   _HMG_aControlFontSize[ k ] := FontSize
   _HMG_aControlFontAttributes[ k ] := { bold, italic, underline, strikeout }
   _HMG_aControlToolTip[ k ] := tooltip
   _HMG_aControlRangeMin[ k ] := 0
   _HMG_aControlRangeMax[ k ] := 0
   _HMG_aControlCaption[ k ] := caption
   _HMG_aControlVisible[ k ] := .T.
   _HMG_aControlHelpId[ k ] := HelpId
   _HMG_aControlFontHandle[ k ] := FontHandle
   _HMG_aControlBrushHandle[ k ] := aRet[ 2 ]
   _HMG_aControlEnabled[ k ] := .T.
   _HMG_aControlMiscData1[ k ] := 0
   _HMG_aControlMiscData2[ k ] := ''

RETURN

*------------------------------------------------------------------------------*
FUNCTION HMGButtonEventhandler ( hWnd, nMsg, wParam, lParam )
*------------------------------------------------------------------------------*
   LOCAL i
   LOCAL RetVal := NIL

   hWnd := NIL // Unused variable

   IF nMsg == WM_COMMAND

      i := AScan ( _HMG_aControlHandles, lParam )

      IF i > 0 .AND. _HMG_aControlType[ i ] == 'HMGBUTTON'

         * Button Click ........................................

         IF HiWord ( wParam ) == BN_CLICKED
            RetVal := 0
            _DoControlEventProcedure ( _HMG_aControlProcedures[ i ], i )
         ENDIF

         * Button LostFocus ....................................

         IF HiWord ( wParam ) == BN_KILLFOCUS
            RetVal := 0
            _DoControlEventProcedure ( _HMG_aControllostFocusProcedure [i] , i )
         ENDIF
 
         * Button GotFocus .....................................

         IF HiWord ( wParam ) == BN_SETFOCUS
            RetVal := 0
            _DoControlEventProcedure ( _HMG_aControlGotFocusProcedure [i] , i )
         ENDIF

      ENDIF

   ENDIF

RETURN RetVal

*------------------------------------------------------------------------------*
PROCEDURE HMGButtonSetFocus ( cWindow, cControl )
*------------------------------------------------------------------------------*
   LOCAL hWnd, ParentFormHandle
   LOCAL ControlCount
   LOCAL x

   IF GetControlType ( cControl, cWindow ) == 'HMGBUTTON'

      _HMG_UserComponentProcess := .T.

      hWnd := GetControlHandle ( cControl, cWindow )
      ControlCount := Len ( _HMG_aControlNames )
      ParentFormHandle := _HMG_aControlParentHandles[ GetControlIndex ( cControl, cWindow ) ]
      FOR x := 1 TO ControlCount
         IF _HMG_aControlType[ x ] == 'HMGBUTTON'
            IF _HMG_aControlParentHandles[ x ] == ParentFormHandle
               SendMessage ( _HMG_aControlHandles[ x ], BM_SETSTYLE, LOWORD ( BS_PUSHBUTTON ), 1 )
            ENDIF
         ENDIF
      NEXT
      SetFocus( hWnd )
      SendMessage ( hWnd, BM_SETSTYLE, LOWORD ( BS_DEFPUSHBUTTON ), 1 )

   ELSE

      _HMG_UserComponentProcess := .F.

   ENDIF

RETURN

*------------------------------------------------------------------------------*
PROCEDURE HMGButtonEnable ( cWindow, cControl )
*------------------------------------------------------------------------------*

   IF GetControlType ( cControl, cWindow ) == 'HMGBUTTON'

      EnableWindow ( GetControlHandle ( cControl, cWindow ) )

      _HMG_UserComponentProcess := .T.

   ELSE

      _HMG_UserComponentProcess := .F.

   ENDIF

RETURN

*------------------------------------------------------------------------------*
PROCEDURE HMGButtonDisable ( cWindow, cControl )
*------------------------------------------------------------------------------*

   IF GetControlType ( cControl, cWindow ) == 'HMGBUTTON'

      DisableWindow ( GetControlHandle ( cControl, cWindow ) )

      _HMG_UserComponentProcess := .T.

   ELSE

      _HMG_UserComponentProcess := .F.

   ENDIF

RETURN

*------------------------------------------------------------------------------*
FUNCTION SetHMGButtonHandle ( cWindow, cControl )
*------------------------------------------------------------------------------*

   IF GetControlType ( cControl, cWindow ) == 'HMGBUTTON'

      MsgExclamation ( 'This Property is Read Only!', 'Warning' )

      _HMG_UserComponentProcess := .T.

   ELSE

      _HMG_UserComponentProcess := .F.

   ENDIF

RETURN NIL

*------------------------------------------------------------------------------*
FUNCTION GetHMGButtonHandle ( cWindow, cControl )
*------------------------------------------------------------------------------*
   LOCAL RetVal := NIL

   IF GetControlType ( cControl, cWindow ) == 'HMGBUTTON'

      _HMG_UserComponentProcess := .T.
      RetVal := GetControlHandle ( cControl, cWindow )

   ELSE

      _HMG_UserComponentProcess := .F.

   ENDIF

RETURN RetVal

*------------------------------------------------------------------------------*
FUNCTION SetHMGButtonCaption ( cWindow, cControl, cProperty, cValue )
*------------------------------------------------------------------------------*
   cProperty := NIL // Unused variable

   IF GetControlType ( cControl, cWindow ) == 'HMGBUTTON'

      _HMG_UserComponentProcess := .T.

      SetWindowText ( GetControlHandle ( cControl, cWindow ), cValue )

   ELSE

      _HMG_UserComponentProcess := .F.

   ENDIF

RETURN NIL

*------------------------------------------------------------------------------*
FUNCTION GetHMGButtonCaption ( cWindow, cControl )
*------------------------------------------------------------------------------*
   LOCAL RetVal := NIL

   IF GetControlType ( cControl, cWindow ) == 'HMGBUTTON'

      _HMG_UserComponentProcess := .T.

      RetVal := GetWindowText ( GetControlHandle ( cControl, cWindow ) )

   ELSE

      _HMG_UserComponentProcess := .F.

   ENDIF

RETURN RetVal

*------------------------------------------------------------------------------*
PROCEDURE SetHMGButtonPicture ( cWindow, cControl, cProperty, cBitmap )
*------------------------------------------------------------------------------*
   LOCAL i
   cProperty := NIL // Unused variable

   IF GetControlType ( cControl, cWindow ) == 'HMGBUTTON'

      i := GetControlIndex ( cControl, cWindow )

      _HMG_aControlPicture[ i ] := cBitmap

      IF ! Empty( _HMG_aControlBrushHandle[ i ] )
         IMAGELIST_DESTROY ( _HMG_aControlBrushHandle[ i ] )
      ENDIF

      _HMG_aControlBrushHandle[ i ] := _SETMIXEDBTNPICTURE( GetControlHandle ( cControl, cWindow ), cBitmap, _HMG_aControlDblClick[ i ], _HMG_aControlSpacing[ i ] )

      _HMG_UserComponentProcess := .T.

   ELSE

      _HMG_UserComponentProcess := .F.

   ENDIF

RETURN

*------------------------------------------------------------------------------*
FUNCTION GetHMGButtonPicture ( cWindow, cControl )
*------------------------------------------------------------------------------*
   LOCAL RetVal

   IF GetControlType ( cControl, cWindow ) == 'HMGBUTTON'

      _HMG_UserComponentProcess := .T.

      RetVal := _GetPicture ( cControl, cWindow )

   ELSE

      _HMG_UserComponentProcess := .F.

   ENDIF

RETURN RetVal

*------------------------------------------------------------------------------*
// Low Level C Routines
*------------------------------------------------------------------------------*

#pragma BEGINDUMP

#include <mgdefs.h>
#include <commctrl.h>

HINSTANCE   GetInstance( void );
HIMAGELIST  HMG_SetButtonImageList( HWND hButton, const char *FileName, int Transparent, UINT uAlign );

HB_FUNC( INITMIXEDBUTTON )
{
   HWND        hwnd = hmg_par_raw_HWND( 1 );
   const char  *FileName = hb_parc( 13 );
   HIMAGELIST  hImageList;
   HWND        hButton;

   int         Style = BS_NOTIFY | WS_CHILD | BS_PUSHBUTTON;
   int         Transparent = hb_parl( 16 ) ? 0 : 1;

   if( hb_parl( 10 ) )
   {
      Style = Style | BS_FLAT;
   }

   if( !hb_parl( 11 ) )
   {
      Style = Style | WS_TABSTOP;
   }

   if( !hb_parl( 12 ) )
   {
      Style = Style | WS_VISIBLE;
   }

   if( hb_parl( 15 ) )
   {
      Style = Style | BS_MULTILINE;
   }

   hButton = CreateWindowEx
      (
         0,
         WC_BUTTON,
         hb_parc( 2 ),
         Style,
         hb_parni( 4 ),
         hb_parni( 5 ),
         hb_parni( 6 ),
         hb_parni( 7 ),
         hwnd,
         hmg_par_raw_HMENU( 3 ),
         GetInstance(),
         NULL
      );

   hImageList = HMG_SetButtonImageList( hButton, FileName, Transparent, hb_parni( 14 ) );

   hb_reta( 2 );
   hmg_storvnl_HANDLE( hButton, -1, 1 );
   hmg_storvnl_HANDLE( hImageList, -1, 2 );
}

#pragma ENDDUMP
