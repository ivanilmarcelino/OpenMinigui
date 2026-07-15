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
   Copyright 1999-2026, https://harbour.github.io/

   "WHAT32"
   Copyright 2002 AJ Wos <andrwos@aust1.net>

   "HWGUI"
   Copyright 2001-2021 Alexander S.Kresin <alex@kresin.ru>

 ---------------------------------------------------------------------------*/

#include "minigui.ch"

#define HOTKEY_ID_MAX  49151

*-----------------------------------------------------------------------------*
STATIC FUNCTION _FindHotKey( hParent, nMod, nKey )
*-----------------------------------------------------------------------------*

   LOCAL cType
   LOCAL nIndex

   FOR EACH cType IN _HMG_aControlType

      nIndex := hb_enumindex( cType )

      IF cType == "HOTKEY" .AND. ;
            _HMG_aControlParentHandles[ nIndex ] == hParent .AND. ;
            _HMG_aControlPageMap[ nIndex ] == nMod .AND. ;
            _HMG_aControlValue[ nIndex ] == nKey

         RETURN nIndex

      ENDIF

   NEXT

RETURN 0


*-----------------------------------------------------------------------------*
FUNCTION _DefineHotKey( cParentForm, nMod, nKey, bAction )
*-----------------------------------------------------------------------------*

   LOCAL hParent := 0

   LOCAL nId
   LOCAL nControl

   LOCAL lSuccess

   /* Resolve active form / MDI context */

   IF _HMG_BeginWindowMDIActive .AND. ;
         Empty( _HMG_ActiveFormName )

      hParent := GetActiveMdiHandle()

      IF hParent == 0

         cParentForm := _HMG_MainClientMDIName

      ELSE

         cParentForm := ;
            _GetWindowProperty( hParent, "PROP_FORMNAME" )

      ENDIF

   ELSEIF _HMG_BeginWindowActive

      cParentForm := _HMG_ActiveFormName

   ENDIF

   /* Validation */

   IF HB_ISNIL( cParentForm )

      MsgMiniGuiError( ;
         "ON KEY: Parent Window is Not specified." )

   ENDIF

   IF !_IsWindowDefined( cParentForm )

      MsgMiniGuiError( ;
         "Window " + cParentForm + " is not defined." )

   ENDIF

   /* Replace existing hotkey */

   IF _GetHotKeyBlock( cParentForm, nMod, nKey ) != NIL

      _ReleaseHotKey( cParentForm, nMod, nKey )

   ENDIF

   /* Resolve parent handle */

   IF hParent == 0
      hParent := GetFormHandle( cParentForm )
   ENDIF

   /* Register hotkey */

   nId := _GetId( HOTKEY_ID_MAX )

   lSuccess := ;
      InitHotKey( ;
         hParent, ;
         nMod, ;
         nKey, ;
         nId )

   /* Allocate internal control slot */

   nControl := _GetControlFree()

   /* Identity */

   _HMG_aControlType[ nControl ]          := "HOTKEY"
   _HMG_aControlNames[ nControl ]         := ""

   _HMG_aControlHandles[ nControl ]       := 0
   _HMG_aControlParentHandles[ nControl ] := hParent

   _HMG_aControlIds[ nControl ] := nId

   /* Events */

   _HMG_aControlProcedures[ nControl ] := bAction

   _HMG_aControlLostFocusProcedure[ nControl ] := ""
   _HMG_aControlGotFocusProcedure[ nControl ]  := ""
   _HMG_aControlChangeProcedure[ nControl ]    := ""

   /* Internal HOTKEY storage */

   /*
      PageMap stores modifier flags.
   */

   _HMG_aControlPageMap[ nControl ] := nMod

   /*
      Value stores virtual key code.
   */

   _HMG_aControlValue[ nControl ] := nKey

   _HMG_aControlInputMask[ nControl ] := ""

   /* Runtime state */

   _HMG_aControlDeleted[ nControl ] := .F.
   _HMG_aControlEnabled[ nControl ] := .T.
   _HMG_aControlVisible[ nControl ] := .T.

   /* Appearance */

   _HMG_aControlBkColor[ nControl ]   := NIL
   _HMG_aControlFontColor[ nControl ] := NIL

   _HMG_aControlPicture[ nControl ] := ""

   _HMG_aControlFontName[ nControl ] := ""
   _HMG_aControlFontSize[ nControl ] := 0

   _HMG_aControlFontAttributes[ nControl ] := ;
      { .F., .F., .F., .F. }

   _HMG_aControlToolTip[ nControl ] := ""
   _HMG_aControlCaption[ nControl ] := ""

   _HMG_aControlFontHandle[ nControl ]  := 0
   _HMG_aControlBrushHandle[ nControl ] := 0

   /* Geometry */

   _HMG_aControlRow[ nControl ]    := 0
   _HMG_aControlCol[ nControl ]    := 0

   _HMG_aControlWidth[ nControl ]  := 0
   _HMG_aControlHeight[ nControl ] := 0

   _HMG_aControlSpacing[ nControl ] := 0

   _HMG_aControlContainerRow[ nControl ] := 0
   _HMG_aControlContainerCol[ nControl ] := 0

   _HMG_aControlContainerHandle[ nControl ] := 0

   /* Miscellaneous */

   _HMG_aControlRangeMin[ nControl ] := 0
   _HMG_aControlRangeMax[ nControl ] := 0

   _HMG_aControlDblClick[ nControl ] := ""
   _HMG_aControlHeadClick[ nControl ] := {}

   _HMG_aControlHelpId[ nControl ] := 0

   _HMG_aControlMiscData1[ nControl ] := 0
   _HMG_aControlMiscData2[ nControl ] := ""

RETURN lSuccess


*-----------------------------------------------------------------------------*
PROCEDURE _ReleaseHotKey( cParentForm, nMod, nKey )
*-----------------------------------------------------------------------------*

   LOCAL hParent := GetFormHandle( cParentForm )
   LOCAL nIndex

   nIndex := _FindHotKey( hParent, nMod, nKey )

   IF nIndex != 0

      _EraseControl( ;
         nIndex, ;
         GetFormIndex( cParentForm ) )

   ENDIF

RETURN


*-----------------------------------------------------------------------------*
FUNCTION _GetHotKeyBlock( cParentForm, nMod, nKey )
*-----------------------------------------------------------------------------*

   LOCAL hParent := GetFormHandle( cParentForm )
   LOCAL nIndex

   nIndex := _FindHotKey( hParent, nMod, nKey )

   IF nIndex != 0
      RETURN _HMG_aControlProcedures[ nIndex ]
   ENDIF

RETURN NIL


*-----------------------------------------------------------------------------*
PROCEDURE _PushKey( nKey )
*-----------------------------------------------------------------------------*

   Keybd_Event( nKey, .F. )   // KeyDown
   Keybd_Event( nKey, .T. )   // KeyUp

RETURN


/* 
  HMG_PressKey( nVK1, nVK2, ... ) --> { nVK1, nVK2, ... }
  by Dr. Claudio Soto, April 2016
 */
*-----------------------------------------------------------------------------*
FUNCTION HMG_PressKey( ... )
*-----------------------------------------------------------------------------*

   LOCAL aVK := {}

   LOCAL nVK
   LOCAL i

   /* Press keys */

   FOR i := 1 TO PCount()

      nVK := PValue( i )

      IF ISNUMERIC( nVK )

         AAdd( aVK, nVK )

         Keybd_Event( nVK, .F. )   // KeyDown

      ELSE

         MsgMiniGuiError( ;
            "HMG_PressKey: Invalid parameter." )

      ENDIF

   NEXT

   /* Release keys in reverse order */

   FOR i := Len( aVK ) TO 1 STEP -1
      Keybd_Event( aVK[ i ], .T. )   // KeyUp
   NEXT

RETURN aVK


*-----------------------------------------------------------------------------*
FUNCTION _SetHotKeyByName( cParentForm, cKey, bAction )
*-----------------------------------------------------------------------------*

   LOCAL aKey
   LOCAL lSuccess := .F.

   IF _HMG_BeginWindowActive
      cParentForm := _HMG_ActiveFormName
   ENDIF

   IF Empty( cParentForm )

      MsgMiniGuiError( ;
         "ON KEY: Parent Window is Not specified." )

   ENDIF

   IF ! Empty( cKey ) .AND. ;
         ISCHARACTER( cKey )

      aKey := _DetermineKey( cKey )

      IF aKey[1] != 0

         IF ! HB_ISNIL( ;
               _GetHotKeyBlock( ;
                  cParentForm, ;
                  aKey[2], ;
                  aKey[1] ) )

            MsgMiniGuiError( ;
               "The hotkey " + cKey + ;
               " is Already defined." )

         ENDIF

         lSuccess := ;
            _DefineHotKey( ;
               cParentForm, ;
               aKey[2], ;
               aKey[1], ;
               bAction )

      ELSE

         MsgMiniGuiError( ;
            "The hotkey " + cKey + ;
            " is not valid." )

      ENDIF

   ENDIF

RETURN lSuccess


*-----------------------------------------------------------------------------*
FUNCTION _DetermineKey( cKey )
*-----------------------------------------------------------------------------*

   LOCAL aKey := { 0, 0 }

   LOCAL nAlt   := 0
   LOCAL nCtrl  := 0
   LOCAL nShift := 0
   LOCAL nWin   := 0

   LOCAL nPos

   LOCAL cRemaining := Upper( cKey )
   LOCAL cToken

   /* Initialize key table */

   IF _SetGetGlobal( "_HMG_aKeyTables" ) == NIL

      STATIC _HMG_aKeyTables AS GLOBAL VALUE ;
      { ;
      "LBUTTON", "RBUTTON", "CANCEL", "MBUTTON", ;
      "XBUTTON1", "XBUTTON2", ".7", "BACK", ;
      "TAB", ".10", ".11", "CLEAR", ;
      "RETURN", ".14", ".15", "SHIFT", ;
      "CONTROL", "MENU", "PAUSE", "CAPITAL", ;
      "KANA", ".22", "JUNJA", "FINAL", ;
      "HANJA", ".26", "ESCAPE", "CONVERT", ;
      "NONCONVERT", "ACCEPT", "MODECHANGE", "SPACE", ;
      "PRIOR", "NEXT", "END", "HOME", ;
      "LEFT", "UP", "RIGHT", "DOWN", ;
      "SELECT", "PRINT", "EXECUTE", "SNAPSHOT", ;
      "INSERT", "DELETE", "HELP", "0", ;
      "1", "2", "3", "4", ;
      "5", "6", "7", "8", ;
      "9", ".58", ".59", ".60", ;
      ".61", ".62", ".63", ".64", ;
      "A", "B", "C", "D", ;
      "E", "F", "G", "H", ;
      "I", "J", "K", "L", ;
      "M", "N", "O", "P", ;
      "Q", "R", "S", "T", ;
      "U", "V", "W", "X", ;
      "Y", "Z", "LWIN", "RWIN", ;
      "APPS", ".94", "SLEEP", "NUMPAD0", ;
      "NUMPAD1", "NUMPAD2", "NUMPAD3", "NUMPAD4", ;
      "NUMPAD5", "NUMPAD6", "NUMPAD7", "NUMPAD8", ;
      "NUMPAD9", "MULTIPLY", "ADD", "SEPARATOR", ;
      "SUBTRACT", "DECIMAL", "DIVIDE", "F1", ;
      "F2", "F3", "F4", "F5", ;
      "F6", "F7", "F8", "F9", ;
      "F10", "F11", "F12", "F13", ;
      "F14", "F15", "F16", "F17", ;
      "F18", "F19", "F20", "F21", ;
      "F22", "F23", "F24", ".136", ;
      ".137", ".138", ".139", ".140", ;
      ".141", ".142", ".143", "NUMLOCK", "SCROLL", ;
      ".146", ".147", ".148", ".149", ;
      ".150", ".151", ".152", ".153", ;
      ".154", ".155", ".156", ".157", ;
      ".158", ".159", "LSHIFT", "RSHIFT", ;
      "LCONTROL", "RCONTROL", "LMENU", "RMENU" ;
      }

   ENDIF

   /* Parse key string */

   DO WHILE ! Empty( cRemaining )

      nPos := At( "+", cRemaining )

      IF nPos == 0

         cRemaining := AllTrim( cRemaining )

         nPos := ;
            AScan( ;
               _SetGetGlobal( "_HMG_aKeyTables" ), ;
               {|c| cRemaining == c } )

         cRemaining := ""

         IF nPos != 0

            aKey := ;
               { ;
               nPos, ;
               nAlt + nCtrl + nShift + nWin ;
               }

         ENDIF

      ELSE

         cToken := ;
            AllTrim( Left( cRemaining, nPos - 1 ) )

         cRemaining := ;
            SubStr( cRemaining, nPos + 1 )

         DO CASE

         CASE cToken == "ALT"

            nAlt := MOD_ALT

            /*
               Internal compatibility logic.
            */

            IF nCtrl != 0
               nAlt := MOD_SHIFT
            ENDIF

         CASE cToken == "CTRL" .OR. ;
               cToken == "CONTROL"

            nCtrl := MOD_CONTROL

         CASE cToken == "SHIFT" .OR. ;
               cToken == "SHFT"

            nShift := MOD_SHIFT

            /*
               Internal compatibility logic.
            */

            IF nCtrl != 0
               nShift := MOD_ALT
            ENDIF

         CASE cToken == "WIN"

            nWin := MOD_WIN

         OTHERWISE

            /* Invalid modifier */

            cRemaining := ""

         ENDCASE

      ENDIF

   ENDDO

RETURN aKey