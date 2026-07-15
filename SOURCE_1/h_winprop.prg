/*
   MINIGUI - Harbour Win32 GUI library source code

   Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
   http://harbourminigui.googlepages.com/

   This    program  is  free  software;  you can redistribute it and/or modify
   it under  the  terms  of the GNU General Public License as published by the
   Free  Software   Foundation;  either  version 2 of the License, or (at your
   option) any later version.

   This   program   is   distributed  in  the hope that it will be useful, but
   WITHOUT    ANY    WARRANTY;    without   even   the   implied  warranty  of
   MERCHANTABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE. See the GNU General
   Public License for more details.

   You   should  have  received a copy of the GNU General Public License along
   with   this   software;   see  the  file COPYING. If not, write to the Free
   Software   Foundation,   Inc.,   59  Temple  Place,  Suite  330, Boston, MA
   02111-1307 USA (or visit the web site http://www.gnu.org/).

   As   a   special  exception, you have permission for additional uses of the
   text  contained  in  this  release  of  Harbour Minigui.

   The   exception   is that,   if   you  link  the  Harbour  Minigui  library
   with  other    files   to  produce   an   executable,   this  does  not  by
   itself   cause  the   resulting   executable    to   be  covered by the GNU
   General  Public  License.  Your    use  of that   executable   is   in   no
   way  restricted on account of linking the Harbour-Minigui library code into
   it.

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

   Parts  of  this  code  is contributed and used here under permission of his
   author: Copyright 2016 (C) P.Chornyj <myorg63@mail.ru>
 */

#include "minigui.ch"

/*
 * Window Property Management Functions
 * Used to attach, retrieve, and remove custom data from Window handles.
 */

/*---------------------------------------------------------------------------*/
PROCEDURE _SetWindowProp ( xParentForm, cPropName, xValue, lDirect )
/*---------------------------------------------------------------------------*/
   LOCAL cParentFormName := ""
   LOCAL nHandle         := _GetFormHandle ( xParentForm, @cParentFormName )

   hb_default( @lDirect, .F. )

   IF ! SetProp( nHandle, cPropName, xValue, lDirect )
      MsgMiniGuiError( "Property " + cPropName + " in Window " + cParentFormName + " could not be set/defined." )
   ENDIF

RETURN

/*---------------------------------------------------------------------------*/
FUNCTION _GetWindowProp ( xParentForm, cPropName, lDirect )
/*---------------------------------------------------------------------------*/
   LOCAL cParentFormName := ""
   LOCAL nHandle         := _GetFormHandle ( xParentForm, @cParentFormName )
   LOCAL xValue

   hb_default( @lDirect, .F. )

   xValue := GetProp( nHandle, cPropName, lDirect )

   IF HB_ISNIL( xValue )
      MsgMiniGuiError( "Property " + cPropName + " in Window " + cParentFormName + " is not defined." )
   ENDIF

RETURN xValue

/*---------------------------------------------------------------------------*/
FUNCTION _RemoveWindowProp ( xParentForm, cPropName, lNoFree )
/*---------------------------------------------------------------------------*/
   hb_default( @lNoFree, .F. )
RETURN RemoveProp( _GetFormHandle ( xParentForm ), cPropName, lNoFree )

/*---------------------------------------------------------------------------*/
FUNCTION _EnumWindowProps( xParentForm )
/*---------------------------------------------------------------------------*/
RETURN EnumProps( _GetFormHandle ( xParentForm ) )

/*---------------------------------------------------------------------------*/
STATIC FUNCTION _GetFormHandle ( xParentForm, cParentFormName )
/*---------------------------------------------------------------------------*/
   LOCAL nHandle := xParentForm

   // 1. Resolve NIL to the currently active window context
   IF nHandle == NIL
      IF _HMG_BeginWindowMdiActive
         nHandle := GetActiveMdiHandle()
      ELSEIF _HMG_BeginDialogActive
         nHandle := _HMG_ActiveDialogName
      ELSEIF _HMG_BeginWindowActive
         nHandle := _HMG_ActiveFormName
      ENDIF
   ENDIF

   // 2. Resolve Name (String) to Handle (HWND)
   IF HB_ISSTRING( nHandle )
      IF ! _IsWindowDefined ( nHandle )
         MsgMiniGuiError( "Window: " + nHandle + " is not defined." )
      ENDIF
      
      cParentFormName := nHandle
      nHandle         := GetFormHandle ( nHandle )
   ENDIF

RETURN nHandle
