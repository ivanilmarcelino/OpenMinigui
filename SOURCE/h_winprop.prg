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
    Copyright 1999-2025, https://harbour.github.io/

    "WHAT32"
    Copyright 2002 AJ Wos <andrwos@aust1.net>

    "HWGUI"
    Copyright 2001-2021 Alexander S.Kresin <alex@kresin.ru>

   Parts  of  this  code  is contributed and used here under permission of his
   author: Copyright 2016 (C) P.Chornyj <myorg63@mail.ru>
 */

#include "minigui.ch"

/*-----------------------------------------------------------------------------*
PROCEDURE _SetWindowProp ( xParentForm, cPropName, xValue, lDirect )
*------------------------------------------------------------------------------*
*
*  Description:
*     Sets a property for a specified window.
*
*  Parameters:
*     xParentForm - The handle or name of the parent form (window) to which the property will be attached.  Can be a window handle (HWND) or a string representing the form's name.
*     cPropName   - The name of the property to set (string). This is the key used to identify the property.
*     xValue      - The value to assign to the property.  Can be any data type (e.g., string, numeric, object).
*     lDirect     - (Optional) A logical value indicating whether to set the property directly. Defaults to .F. (False).  If .T. (True), the property is set directly without any additional processing.
*
*  Return Value:
*     None. This procedure does not return a value.
*
*  Purpose:
*     This procedure allows you to associate arbitrary data (properties) with a window.  It's a mechanism for storing and retrieving information related to a specific window.
*     It's used to extend the functionality of a window by attaching custom data or settings. For example, you might use it to store the original size of a window,
*     a user-specific setting, or a pointer to a data structure.
*
*  Notes:
*     If the specified property name already exists, its value will be overwritten.
*     If the window handle is invalid or the window is not found, an error message will be displayed using MsgMiniGuiError.
*     The lDirect parameter is used internally by HMG and should generally be left at its default value (.F.) unless you have a specific reason to use direct property setting.
*
*/
PROCEDURE _SetWindowProp ( xParentForm, cPropName, xValue, lDirect )
   LOCAL cParentFormName := ""

   xParentForm := _GetFormHandle ( xParentForm, @cParentFormName )

   IF ! SetProp( xParentForm, cPropName, xValue, hb_defaultValue( lDirect, .F. ) )
      MsgMiniGuiError( "Property " + cPropName + " in Window " + cParentFormName + " is not defined." )
   ENDIF

RETURN

/*-----------------------------------------------------------------------------*
FUNCTION _GetWindowProp ( xParentForm, cPropName, lDirect )
*------------------------------------------------------------------------------*
*
*  Description:
*     Retrieves the value of a property associated with a specified window.
*
*  Parameters:
*     xParentForm - The handle or name of the parent form (window) from which to retrieve the property. Can be a window handle (HWND) or a string representing the form's name.
*     cPropName   - The name of the property to retrieve (string). This is the key used to identify the property.
*     lDirect     - (Optional) A logical value indicating whether to get the property directly. Defaults to .F. (False). If .T. (True), the property is retrieved directly without any additional processing.
*
*  Return Value:
*     The value of the property associated with the window. The data type of the return value depends on the data type of the property that was originally set.
*     If the property is not found, an error message is displayed using MsgMiniGuiError, and NIL is implicitly returned.
*
*  Purpose:
*     This function is used to retrieve data that has been previously associated with a window using _SetWindowProp. It allows you to access custom data or settings
*     that are specific to a particular window. For example, you might use it to retrieve a user-specific setting that was stored when the window was created.
*
*  Notes:
*     If the specified property name does not exist for the given window, an error message will be displayed using MsgMiniGuiError.
*     The lDirect parameter is used internally by HMG and should generally be left at its default value (.F.) unless you have a specific reason to use direct property retrieval.
*
*/
FUNCTION _GetWindowProp ( xParentForm, cPropName, lDirect )
   LOCAL cParentFormName := ""
   LOCAL xValue

   xParentForm := _GetFormHandle ( xParentForm, @cParentFormName )

   xValue := GetProp( xParentForm, cPropName, hb_defaultValue( lDirect, .F. ) )

   IF HB_ISNIL( xValue )
      MsgMiniGuiError( "Property " + cPropName + " in Window " + cParentFormName + " is not defined." )
   ENDIF

RETURN xValue

/*-----------------------------------------------------------------------------*
FUNCTION _RemoveWindowProp ( xParentForm, cPropName, lNoFree )
*------------------------------------------------------------------------------*
*
*  Description:
*     Removes a property from a specified window.
*
*  Parameters:
*     xParentForm - The handle or name of the parent form (window) from which to remove the property. Can be a window handle (HWND) or a string representing the form's name.
*     cPropName   - The name of the property to remove (string). This is the key used to identify the property.
*     lNoFree     - (Optional) A logical value indicating whether the property's value should be freed. Defaults to .F. (False). If .T. (True), the property's value is not freed from memory.
*
*  Return Value:
*     The return value of the RemoveProp function, which is a Windows API function.  It typically returns a non-zero value if the property was successfully removed, and zero otherwise.
*
*  Purpose:
*     This function is used to remove a property that was previously associated with a window using _SetWindowProp. It's used to clean up resources or to remove data that is no longer needed.
*     For example, you might use it to remove a temporary setting that was stored for a specific operation.
*
*  Notes:
*     The lNoFree parameter controls whether the memory associated with the property's value is freed when the property is removed.  If the property's value is a pointer to dynamically allocated memory,
*     setting lNoFree to .T. will prevent that memory from being freed, which can lead to memory leaks if not handled carefully.  Generally, you should leave lNoFree at its default value (.F.)
*     to ensure that memory is properly managed.
*
*/
FUNCTION _RemoveWindowProp ( xParentForm, cPropName, lNoFree )

RETURN RemoveProp( _GetFormHandle ( xParentForm ), cPropName, hb_defaultValue( lNoFree, .F. ) )

/*-----------------------------------------------------------------------------*
FUNCTION _EnumWindowProps( xParentForm )
*------------------------------------------------------------------------------*
*
*  Description:
*     Enumerates the properties associated with a specified window.
*
*  Parameters:
*     xParentForm - The handle or name of the parent form (window) whose properties are to be enumerated. Can be a window handle (HWND) or a string representing the form's name.
*
*  Return Value:
*     The return value of the EnumProps function, which is a Windows API function.  The exact return value and its meaning depend on the callback function used by EnumProps.
*     In HMG Extended, this function is typically used internally with a callback function that processes each property.
*
*  Purpose:
*     This function is used to iterate through all the properties that have been associated with a window using _SetWindowProp. It's useful for tasks such as saving the window's state,
*     debugging, or performing operations on all properties of a window.
*
*  Notes:
*     The EnumProps function requires a callback function to be defined, which will be called for each property of the window.  The callback function receives the property name and value as arguments.
*     The specific implementation of the callback function determines how the properties are processed.
*
*/
FUNCTION _EnumWindowProps( xParentForm )

RETURN EnumProps( _GetFormHandle ( xParentForm ) )

/*-----------------------------------------------------------------------------*
STATIC FUNCTION _GetFormHandle ( xParentForm, cParentFormName )
*------------------------------------------------------------------------------*
*
*  Description:
*     Retrieves the window handle (HWND) for a given form (window), based on either the form's handle or its name.
*
*  Parameters:
*     xParentForm     - The handle or name of the parent form (window). Can be a window handle (HWND) or a string representing the form's name. If NIL, the function attempts to determine the active form.
*     cParentFormName - A variable passed by reference (@cParentFormName). If xParentForm is a string (form name), this variable will be populated with the form name.
*
*  Return Value:
*     The window handle (HWND) of the specified form.
*
*  Purpose:
*     This function is a utility function used internally by other HMG Extended functions to obtain the window handle for a given form. It handles cases where the form is specified by its handle, its name, or when no form is explicitly specified (in which case it tries to determine the active form).
*     It simplifies the process of working with windows by allowing functions to accept either a window handle or a window name as input.
*
*  Notes:
*     If xParentForm is NIL, the function checks for an active MDI window, an active form, or an active dialog.
*     If xParentForm is a string and the window with that name is not defined, an error message is displayed using MsgMiniGuiError.
*     The STATIC keyword indicates that this function is only accessible within the current source file.
*
*/
STATIC FUNCTION _GetFormHandle ( xParentForm, cParentFormName )

   IF xParentForm == NIL

      IF _HMG_BeginWindowMdiActive

         xParentForm := GetActiveMdiHandle()

      ELSE

         IF _HMG_BeginWindowActive .OR. _HMG_BeginDialogActive
            xParentForm := iif( _HMG_BeginDialogActive, _HMG_ActiveDialogName, _HMG_ActiveFormName )
         ENDIF

      ENDIF

   ENDIF

   IF HB_ISSTRING( xParentForm )

      IF ! _IsWindowDefined ( xParentForm )
         MsgMiniGuiError( "Window: " + xParentForm + " is not defined." )
      ENDIF

      cParentFormName := xParentForm
      xParentForm := GetFormHandle ( cParentFormName )

   ENDIF

RETURN xParentForm
