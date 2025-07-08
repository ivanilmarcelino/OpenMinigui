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

#include "minigui.ch"

#ifdef _USERINIT_

#include "i_winuser.ch"

ANNOUNCE CLASS_TACTIVEX

*-----------------------------------------------------------------------------*
INIT PROCEDURE _InitActiveX
*-----------------------------------------------------------------------------*

   InstallMethodHandler ( 'Release', 'ReleaseActiveX' )
   InstallPropertyHandler ( 'XObject', 'SetActiveXObject', 'GetActiveXObject' )

RETURN

*-----------------------------------------------------------------------------*
PROCEDURE _DefineActivex ( cControlName, cParentForm, nRow, nCol, nWidth, nHeight, cProgId, aEvents, clientedge )
*-----------------------------------------------------------------------------*
   LOCAL nControlHandle, nParentFormHandle
   LOCAL mVar
   LOCAL k
   LOCAL oActiveX
   LOCAL oOle
   LOCAL nAtlDllHandle

   // If defined inside DEFINE WINDOW structure, determine cParentForm
   IF _HMG_BeginWindowActive .OR. _HMG_BeginDialogActive
      cParentForm := iif( _HMG_BeginDialogActive, _HMG_ActiveDialogName, _HMG_ActiveFormName )
   ENDIF

   // If defined inside a Tab structure, adjust position and determine cParentForm
   IF _HMG_FrameLevel > 0
      nCol += _HMG_ActiveFrameCol[ _HMG_FrameLevel ]
      nRow += _HMG_ActiveFrameRow[ _HMG_FrameLevel ]
      cParentForm := _HMG_ActiveFrameParentFormName[ _HMG_FrameLevel ]
   ENDIF

   IF .NOT. _IsWindowDefined ( cParentForm )
      MsgMiniGuiError ( "Window: " + cParentForm + " is not defined." )
   ENDIF

   IF _IsControlDefined ( cControlName, cParentForm )
      MsgMiniGuiError ( "Control: " + cControlName + " Of " + cParentForm + " Already defined." )
   ENDIF

   IF ! ISCHARACTER ( cProgId )
      MsgMiniGuiError ( "Control: " + cControlName + " Of " + cParentForm + " PROGID Property Invalid Type." )
   ENDIF

   IF Empty ( cProgId )
      MsgMiniGuiError ( "Control: " + cControlName + " Of " + cParentForm + " PROGID Can't be empty." )
   ENDIF

   // Define public variable associated with control
   mVar := '_' + cParentForm + '_' + cControlName

   // Init ActiveX object

   oActiveX := TActiveX():New( cParentForm, cProgId, nRow, nCol, nWidth, nHeight )

   // Create OLE control

   oOle := oActiveX:Load()

   nControlHandle := oActiveX:hWnd
   nAtlDllHandle := oActiveX:hAtl

   IF ! Empty( oActiveX:hSink )
      IF ISARRAY ( aEvents ) .AND. Len ( aEvents ) > 0 .AND. ISARRAY ( aEvents [1] )
         AEval ( aEvents, { | x | oActiveX:EventMap( x [1], x [2] ) } )
      ENDIF
   ENDIF

   IF _HMG_BeginTabActive
      AAdd ( _HMG_ActiveTabCurrentPageMap, nControlhandle )
   ENDIF

   IF hb_defaultValue( clientedge, .F. )
      ChangeStyle ( nControlHandle, WS_EX_CLIENTEDGE, , .T. )
   ENDIF

   nParentFormHandle := GetFormHandle ( cParentForm )

   k := _GetControlFree()

#ifdef _NAMES_LIST_
   _SetNameList( mVar , k )
#else
   Public &mVar. := k
#endif

   _HMG_aControlType[ k ] := "ACTIVEX"
   _HMG_aControlNames[ k ] :=  cControlName
   _HMG_aControlHandles[ k ] := nControlHandle
   _HMG_aControlParenthandles[ k ] := nParentFormHandle
   _HMG_aControlIds[ k ] :=  oActiveX
   _HMG_aControlProcedures[ k ] :=  ""
   _HMG_aControlPageMap[ k ] :=  aEvents
   _HMG_aControlValue[ k ] :=  Nil
   _HMG_aControlInputMask[ k ] :=  ""
   _HMG_aControllostFocusProcedure[ k ] :=  ""
   _HMG_aControlGotFocusProcedure[ k ] :=  ""
   _HMG_aControlChangeProcedure[ k ] :=  ""
   _HMG_aControlDeleted[ k ] :=  .F.
   _HMG_aControlBkColor[ k ] :=   Nil
   _HMG_aControlFontColor[ k ] :=  Nil
   _HMG_aControlDblClick[ k ] :=  ""
   _HMG_aControlHeadClick[ k ] :=  {}
   _HMG_aControlRow[ k ] := nRow
   _HMG_aControlCol[ k ] := nCol
   _HMG_aControlWidth[ k ] := nWidth
   _HMG_aControlHeight[ k ] := nHeight
   _HMG_aControlSpacing[ k ] := 0
   _HMG_aControlContainerRow[ k ] :=  iif ( _HMG_FrameLevel > 0, _HMG_ActiveFrameRow[ _HMG_FrameLevel ], -1 )
   _HMG_aControlContainerCol[ k ] :=  iif ( _HMG_FrameLevel > 0, _HMG_ActiveFrameCol[ _HMG_FrameLevel ], -1 )
   _HMG_aControlPicture[ k ] :=  ""
   _HMG_aControlContainerHandle[ k ] :=  0
   _HMG_aControlFontName[ k ] :=  Nil
   _HMG_aControlFontSize[ k ] :=  Nil
   _HMG_aControlFontAttributes[ k ] :=  {}
   _HMG_aControlToolTip[ k ] :=  ''
   _HMG_aControlRangeMin[ k ] :=  0
   _HMG_aControlRangeMax[ k ] :=  0
   _HMG_aControlCaption[ k ] :=   ''
   _HMG_aControlVisible[ k ] :=  .T.
   _HMG_aControlHelpId[ k ] :=  nAtlDllHandle
   _HMG_aControlFontHandle[ k ] :=  Nil
   _HMG_aControlBrushHandle[ k ] :=  0
   _HMG_aControlEnabled[ k ] :=  .T.
   _HMG_aControlMiscData1[ k ] := oOle
   _HMG_aControlMiscData2[ k ] := ''

RETURN

*-----------------------------------------------------------------------------*
PROCEDURE ReleaseActiveX ( cWindow, cControl )
*-----------------------------------------------------------------------------*
   LOCAL oActiveX

   IF _IsControlDefined ( cControl, cWindow ) .AND. GetControlType ( cControl, cWindow ) == 'ACTIVEX'

      oActiveX := _HMG_aControlIds[ GetControlIndex ( cControl, cWindow ) ]

      IF ISOBJECT ( oActiveX )
         oActiveX:Release()
      ENDIF

      _HMG_UserComponentProcess := .T.

   ELSE

      _HMG_UserComponentProcess := .F.

   ENDIF

RETURN

*-----------------------------------------------------------------------------*
FUNCTION SetActiveXObject ( cWindow, cControl )
*-----------------------------------------------------------------------------*

   IF GetControlType ( cControl, cWindow ) == 'ACTIVEX'

      MsgExclamation ( 'This Property is Read Only!', 'Warning' )

      _HMG_UserComponentProcess := .T.

   ELSE

      _HMG_UserComponentProcess := .F.

   ENDIF

RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION GetActiveXObject ( cWindow, cControl )
*-----------------------------------------------------------------------------*
   LOCAL RetVal

   IF GetControlType ( cControl, cWindow ) == 'ACTIVEX'

      _HMG_UserComponentProcess := .T.

      RetVal := _GetControlObject ( cControl, cWindow )

   ELSE

      _HMG_UserComponentProcess := .F.

   ENDIF

RETURN RetVal

*-----------------------------------------------------------------------------*
FUNCTION _GetControlObject ( ControlName, ParentForm )
*-----------------------------------------------------------------------------*
   LOCAL i

   IF ( i := GetControlIndex ( ControlName , ParentForm ) ) == 0
      RETURN NIL
   ENDIF

RETURN ( _HMG_aControlMiscData1[ i ] )

/*
   Marcelo Torres, Noviembre de 2006.
   TActivex para [x]Harbour Minigui.
   Adaptacion del trabajo de:
   ---------------------------------------------
   Oscar Joel Lira Lira [oSkAr]
   Clase TAxtiveX_FreeWin para Fivewin
   Noviembre 8 del 2006
   email: oscarlira78@hotmail.com
   http://freewin.sytes.net
   CopyRight 2006 Todos los Derechos Reservados
   ---------------------------------------------
*/

#include "hbclass.ch"

/*
   TActiveX Class:
   This class facilitates the use and management of ActiveX controls in a Harbour application.
   It provides methods for initializing, resizing, hiding, showing, and interacting with an ActiveX control.
*/

CLASS TActiveX

   /* Instance Variables */
   DATA oOle           // Holds the OLE object created from the ActiveX control.
   DATA hWnd           // The handle of the ActiveX control's window.
   DATA cWindowName    // The name of the parent window containing the ActiveX control.
   DATA cProgId        // The ProgID of the ActiveX control.
   DATA hSink INIT NIL // Connection point sink for event handling (if applicable).
   DATA hAtl INIT NIL  // Internal reference for the ActiveX ATL object.
   DATA nRow           // Row position of the ActiveX control.
   DATA nCol           // Column position of the ActiveX control.
   DATA nWidth         // Width of the ActiveX control.
   DATA nHeight        // Height of the ActiveX control.
   DATA nOldWinWidth   // Previous window width (used for adjustments).
   DATA nOldWinHeight  // Previous window height (used for adjustments).
   DATA bHide INIT .F. // Tracks whether the control is hidden.

   /* Event Management */
   DATA aAxEv     INIT {} // Array of event IDs handled by the control.
   DATA aAxExec   INIT {} // Array of event execution handlers.

   /* Methods */
   // Initializes the ActiveX control with the specified parameters.
   METHOD New( cWindowName, cProgId, nRow, nCol, nWidth, nHeight )

   // Loads and initializes the ActiveX control in the specified window.
   METHOD Load()

   // Resizes the ActiveX control and updates its dimensions.
   METHOD ReSize( nRow, nCol, nWidth, nHeight )

   // Hides the ActiveX control.
   METHOD Hide()

   // Shows the ActiveX control.
   METHOD Show()

   // Releases the ActiveX control, destroying its window and cleaning up resources.
   METHOD Release()

   // Refreshes the ActiveX control by hiding and showing it again.
   METHOD Refresh()

   // Adjusts the control's size and position based on the parent window's dimensions.
   METHOD Adjust()

   // Returns the current row position of the control.
   METHOD GetRow()

   // Returns the current column position of the control.
   METHOD GetCol()

   // Returns the current width of the control.
   METHOD GetWidth()

   // Returns the current height of the control.
   METHOD GetHeight()

   // Maps events to handlers for the ActiveX control.
   METHOD EventMap( nMsg, xExec, oSelf )

   // Handles errors occurring during ActiveX interaction.
#if defined( __XHARBOUR__ )
   ERROR HANDLER OnError( p1, p2, p3, p4, p5 ) 
#else
   ERROR HANDLER OnError( ... ) 
#endif

ENDCLASS

/* Method Definitions */

/*
   Initializes the ActiveX control and its dimensions, using default values if necessary.
*/
METHOD New( cWindowName, cProgId, nRow, nCol, nWidth, nHeight ) CLASS TActiveX
   if( Empty( nRow )    , nRow    := 0 , )
   if( Empty( nCol )    , nCol    := 0 , )
   if( Empty( nWidth )  , nWidth  := GetProperty( cWindowName , "width" ) , )
   if( Empty( nHeight ) , nHeight := GetProperty( cWindowName , "Height" ) , )
   ::nRow := nRow
   ::nCol := nCol
   ::nWidth := nWidth
   ::nHeight := nHeight
   ::cWindowName := cWindowName
   ::cProgId := cProgId
   ::nOldWinWidth := GetProperty( cWindowName, "width" )
   ::nOldWinHeight := GetProperty( cWindowName, "Height" )

RETURN Self

/*
   Loads the ActiveX control, creates its window, and connects events.
*/
METHOD Load() CLASS TActiveX
   LOCAL oError
   LOCAL xObjeto, hSink
   LOCAL nHandle := GetFormHandle( ::cWindowName )

   AtlAxWinInit()
   ::hWnd := CreateWindowEx( nHandle, ::cProgId )
   MoveWindow( ::hWnd, ::nCol, ::nRow, ::nWidth, ::nHeight, .T. )
   xObjeto := AtlAxGetDisp( ::hWnd )
   ::hAtl := xObjeto
   TRY
      ::oOle := CreateObject( xObjeto )
   CATCH oError
      MsgInfo( oError:description )
   END
   IF SetupConnectionPoint( ::hAtl, @hSink, ::aAxEv, ::aAxExec ) == S_OK
      ::hSink := hSink
   ENDIF

RETURN ::oOle

METHOD ReSize( nRow, nCol, nWidth, nHeight ) CLASS TActiveX
   IF !::bHide
      MoveWindow( ::hWnd, nCol, nRow, nWidth, nHeight, .T. )
   ENDIF
   ::nRow := nRow
   ::nCol := nCol
   ::nWidth := nWidth
   ::nHeight := nHeight
   ::nOldWinWidth := GetProperty( ::cWindowName, "width" )
   ::nOldWinHeight := GetProperty( ::cWindowName, "Height" )

RETURN .T.

METHOD Adjust() CLASS TActiveX

   LOCAL nAuxRight, nAuxBottom

   nAuxRight := ( ::nOldWinWidth - ( ::nWidth + ::nCol ) )
   nAuxBottom := ( ::nOldWinHeight - ( ::nHeight + ::nRow ) )
   MoveWindow( ::hWnd, ::nCol, ::nRow, GetProperty( ::cWindowName, "width" ) - ::nCol - nAuxRight, GetProperty( ::cWindowName, "height" ) - ::nRow - nAuxBottom, .T. )
   ::nWidth := GetProperty( ::cWindowName, "width" ) - ::nCol - nAuxRight
   ::nHeight := GetProperty( ::cWindowName, "height" ) - ::nRow - nAuxBottom
   ::nOldWinWidth := GetProperty( ::cWindowName, "width" )
   ::nOldWinHeight := GetProperty( ::cWindowName, "Height" )

RETURN .T.

METHOD GetRow() CLASS TActiveX
RETURN ::nRow

METHOD GetCol() CLASS TActiveX
RETURN ::nCol

METHOD GetWidth() CLASS TActiveX
RETURN ::nWidth

METHOD GetHeight() CLASS TActiveX
RETURN ::nHeight

METHOD Hide() CLASS TActiveX
   MoveWindow( ::hWnd, 0, 0, 0, 0, .T. )
   ::bHide := .T.

RETURN .T.

METHOD Show() CLASS TActiveX
   MoveWindow( ::hWnd, ::nCol, ::nRow, ::nWidth, ::nHeight, .T. )
   ::bHide := .F.

RETURN .T.

METHOD Release() CLASS TActiveX
   IF ::hWnd != NIL
      DestroyWindow( ::hWnd )
   ENDIF
   IF !Empty( ::hSink )
      ShutdownConnectionPoint( ::hSink )
   ENDIF
   ReleaseDispatch( ::hAtl )
   AtlAxWinEnd()

RETURN .T.

METHOD Refresh() CLASS TActiveX
   ::Hide()
   ::Show()

RETURN .T.

METHOD EventMap( nMsg, xExec, oSelf )

   LOCAL nAt

   nAt := AScan( ::aAxEv, nMsg )
   IF nAt == 0
      AAdd( ::aAxEv, nMsg )
      AAdd( ::aAxExec, { NIL, NIL } )
      nAt := Len( ::aAxEv )
   ENDIF
   ::aAxExec[ nAt ] := { xExec, oSelf }

RETURN NIL

#if defined( __XHARBOUR__ )
METHOD OnError( p1, p2, p3, p4, p5 ) 
#else
METHOD OnError( ... ) 
#endif
   LOCAL cMethod := __GetMessage() 

#if defined( __XHARBOUR__ )
   p1 := p1 ; p2 := p2 ; p3 := p3 ; p4 := p4 ; p5 := p5
#endif
   IF cMethod[ 1 ] == "_" 
      cMethod := Right( cMethod, 2 ) 
   ENDIF 
   hb_ExecFromArray( ::oOle, cMethod, hb_aParams() ) 

RETURN NIL 

/*
 * C-level
 */
#pragma BEGINDUMP

#ifndef CINTERFACE
#define CINTERFACE   1                    // Enable C-style interfaces if not already defined
#endif
#ifndef NONAMELESSUNION
#define NONAMELESSUNION                   // Enable non-anonymous unions if not already defined
#endif
#include <mgdefs.h>                       // Include MiniGUI definitions
#include <commctrl.h>                     // Include common controls for GUI elements
#include <ocidl.h>                        // Include OLE Control Interface definitions
#include <hbvm.h>                         // Harbour Virtual Machine interface
#include <hbapiitm.h>                     // Harbour API item management
#ifdef UNICODE
LPWSTR            AnsiToWide( LPCSTR );   // Declaration of a function to convert ANSI to wide strings for Unicode
#endif

// Declaration of an external function to retrieve a function address from a DLL
extern HB_PTRUINT wapi_GetProcAddress( HMODULE hModule, LPCSTR lpProcName );

// Typedefs for function pointers used to initialize and manage ActiveX controls
typedef HRESULT ( WINAPI *LPAtlAxWinInit ) ( void );                 // Typedef for AtlAxWinInit function pointer
typedef HRESULT ( WINAPI *LPAtlAxGetControl ) ( HWND, IUnknown ** ); // Typedef for AtlAxGetControl function pointer

// Static global variables to hold handles and function pointers for ActiveX library
static HMODULE    hAtl = NULL;                  // Handle to Atl.dll
LPAtlAxWinInit    AtlAxWinInit;                 // Pointer to AtlAxWinInit function
LPAtlAxGetControl AtlAxGetControl;

// Pointer to AtlAxGetControl function
// Initializes the ActiveX library by loading Atl.dll and getting function pointers
static void _Ax_Init( void )
{
   if( !hAtl )                                  // If the library is not already loaded
   {
      hAtl = LoadLibrary( TEXT( "Atl.Dll" ) );  // Load Atl.dll library
      AtlAxWinInit = ( LPAtlAxWinInit ) wapi_GetProcAddress( hAtl, "AtlAxWinInit" );            // Get AtlAxWinInit function address
      AtlAxGetControl = ( LPAtlAxGetControl ) wapi_GetProcAddress( hAtl, "AtlAxGetControl" );   // Get AtlAxGetControl function address
      ( AtlAxWinInit ) (); // Initialize ActiveX (AtlAxWinInit function)
   }
}

// Harbour function to initialize the ActiveX library by calling _Ax_Init
HB_FUNC( ATLAXWININIT )
{
   _Ax_Init();
}

// Harbour function to free the ActiveX library
HB_FUNC( ATLAXWINEND )
{
   if( hAtl )              // If the library handle exists
   {
      FreeLibrary( hAtl ); // Free the Atl.dll library
      hAtl = NULL;         // Reset the handle to NULL
   }
}

// Harbour function to get IDispatch interface from an ActiveX control
HB_FUNC( ATLAXGETDISP ) // hWnd -> pDisp
{
   IUnknown    *pUnk;   // Pointer to IUnknown interface
   IDispatch   *pDisp;  // Pointer to IDispatch interface
   _Ax_Init();          // Ensure ActiveX library is initialized
   AtlAxGetControl( hmg_par_raw_HWND( 1 ), &pUnk );            // Get the IUnknown interface of the control
#if defined( __cplusplus )
   pUnk->QueryInterface( IID_IDispatch, ( void ** ) &pDisp );  // Query for IDispatch in C++
#else
   pUnk->lpVtbl->QueryInterface( pUnk, &IID_IDispatch, ( void ** ) &pDisp );  // Query for IDispatch in C-style
#endif
   pUnk->lpVtbl->Release( pUnk );   // Release the IUnknown interface
   hmg_ret_raw_HANDLE( pDisp );     // Return the IDispatch interface to the caller
}

// Harbour function to create an ActiveX window using a ProgID
HB_FUNC_STATIC( CREATEWINDOWEX ) // ( hWnd, cProgId ) -> hActiveXWnd
{
#ifndef UNICODE
   LPCSTR   lpWindowName = hb_parc( 2 );  // Get window name in ANSI
#else
   LPWSTR   lpWindowName = AnsiToWide( ( char * ) hb_parc( 2 ) ); // Convert window name to Unicode if applicable
#endif
   hmg_ret_raw_HWND
   (
      CreateWindowEx
         (
            0,
            TEXT( "AtlAxWin" ),     // Extended window creation with AtlAxWin class
            lpWindowName,
            WS_VISIBLE | WS_CHILD,  // Window styles for visibility and child placement
            0,
            0,
            0,
            0, // Position and size (defaults to 0 here)
            hmg_par_raw_HWND( 1 ),  // Parent window handle
            0,
            0,
            NULL
         )
   ); // No additional parameters
}

// Conditional include for hash-based event handling if __USEHASHEVENTS is defined
#ifdef __USEHASHEVENTS
#include <hashapi.h>
#endif

//------------------------------------------------------------------------------
// Prototype for function that converts OLE Variant to Harbour item
HRESULT  hb_oleVariantToItem( PHB_ITEM pItem, VARIANT *pVariant );

// Definition of a custom IDispatch-based COM interface for event handling
#undef INTERFACE
#define INTERFACE IEventHandler

DECLARE_INTERFACE_( INTERFACE, IDispatch )
{
   // IUnknown methods
   STDMETHOD ( QueryInterface ) ( THIS_ REFIID, void ** ) PURE;

   STDMETHOD_ ( ULONG, AddRef ) ( THIS ) PURE;

   STDMETHOD_ ( ULONG, Release ) ( THIS ) PURE;

   // IDispatch methods
   STDMETHOD_ ( ULONG, GetTypeInfoCount ) ( THIS_ UINT * ) PURE;

   STDMETHOD_ ( ULONG, GetTypeInfo ) ( THIS_ UINT, LCID, ITypeInfo ** ) PURE;

   STDMETHOD_ ( ULONG, GetIDsOfNames ) ( THIS_ REFIID, LPOLESTR *, UINT, LCID, DISPID * ) PURE;

   STDMETHOD_ ( ULONG, Invoke ) ( THIS_ DISPID, REFIID, LCID, WORD, DISPPARAMS *, VARIANT *, EXCEPINFO *, UINT * ) PURE;
};

// Explanation of the IEventHandler structure and its extended version
// IEventHandler starts with a pointer to its VTable, a required structure
// for all COM objects. The extended struct (MyRealIEventHandler) contains
// additional private members but is presented as an IEventHandler to external
// applications.
// Extended structure for event handler with private data members
typedef struct
{
   IEventHandler     *lpVtbl;             // Pointer to virtual function table (VTable) for IEventHandler
   DWORD             count;               // Reference count for memory management
   IConnectionPoint  *pIConnectionPoint;  // Pointer to connection point for event handling
   DWORD             dwEventCookie;       // Event subscription identifier
   IID               device_event_interface_iid;   // Interface identifier for the device event
   PHB_ITEM          pEvents;       // Harbour item to store events
#ifndef __USEHASHEVENTS
   PHB_ITEM          pEventsExec;   // Harbour item for direct event execution if not using hash events
#endif
} MyRealIEventHandler;

//------------------------------------------------------------------------------
// Here are IEventHandler's functions.
//------------------------------------------------------------------------------
// Every COM object's interface must have the 3 functions QueryInterface(),
// AddRef(), and Release().
// IEventHandler's QueryInterface()
static HRESULT STDMETHODCALLTYPE QueryInterface( IEventHandler *self, REFIID vTableGuid, void **ppv )
{
   // Check if the GUID matches IEvenetHandler VTable's GUID. We gave the C variable name
   // IID_IEventHandler to our VTable GUID. We can use an OLE function called
   // IsEqualIID to do the comparison for us. Also, if the caller passed a
   // IUnknown GUID, then we'll likewise return the IEventHandler, since it can
   // masquerade as an IUnknown object too. Finally, if the called passed a
   // IDispatch GUID, then we'll return the IExample3, since it can masquerade
   // as an IDispatch too
   if( IsEqualIID( vTableGuid, &IID_IUnknown ) )
   {
      *ppv = ( IUnknown * ) self;

      // Increment the count of callers who have an outstanding pointer to self object
      self->lpVtbl->AddRef( self );
      return S_OK;
   }

   if( IsEqualIID( vTableGuid, &IID_IDispatch ) )
   {
      *ppv = ( IDispatch * ) self;
      self->lpVtbl->AddRef( self );
      return S_OK;
   }

   if( IsEqualIID( vTableGuid, &( ( ( MyRealIEventHandler * ) self )->device_event_interface_iid ) ) )
   {
      *ppv = ( IDispatch * ) self;
      self->lpVtbl->AddRef( self );
      return S_OK;
   }

   // We don't recognize the GUID passed to us. Let the caller know self,
   // by clearing his handle, and returning E_NOINTERFACE.
   *ppv = 0;
   return E_NOINTERFACE;
}

//------------------------------------------------------------------------------
// IEventHandler's AddRef()
static ULONG STDMETHODCALLTYPE AddRef( IEventHandler *self )
{
   // Increment IEventHandler's reference count, and return the updated value.
   // NOTE: We have to typecast to gain access to any data members. These
   // members are not defined  (so that an app can't directly access them).
   // Rather they are defined only above in our MyRealIEventHandler
   // struct. So typecast to that in order to access those data members
   return ++( ( MyRealIEventHandler * ) self )->count;
}

//------------------------------------------------------------------------------
// IEventHandler's Release()
static ULONG STDMETHODCALLTYPE Release( IEventHandler *self )
{
   if( --( ( MyRealIEventHandler * ) self )->count == 0 )
   {
      GlobalFree( self );
      return 0;
   }

   return( ( MyRealIEventHandler * ) self )->count;
}

//------------------------------------------------------------------------------
// IEventHandler's GetTypeInfoCount()
static ULONG STDMETHODCALLTYPE GetTypeInfoCount( IEventHandler *self, UINT *pCount )
{
   HB_SYMBOL_UNUSED( self );
   HB_SYMBOL_UNUSED( pCount );
   return( ULONG ) E_NOTIMPL;
}

//------------------------------------------------------------------------------
// IEventHandler's GetTypeInfo()
static ULONG STDMETHODCALLTYPE GetTypeInfo( IEventHandler *self, UINT itinfo, LCID lcid, ITypeInfo **pTypeInfo )
{
   HB_SYMBOL_UNUSED( self );
   HB_SYMBOL_UNUSED( itinfo );
   HB_SYMBOL_UNUSED( lcid );
   HB_SYMBOL_UNUSED( pTypeInfo );
   return( ULONG ) E_NOTIMPL;
}

//------------------------------------------------------------------------------
// IEventHandler's GetIDsOfNames()
static ULONG STDMETHODCALLTYPE GetIDsOfNames( IEventHandler *self, REFIID riid, LPOLESTR *rgszNames, UINT cNames, LCID lcid, DISPID *rgdispid )
{
   HB_SYMBOL_UNUSED( self );
   HB_SYMBOL_UNUSED( riid );
   HB_SYMBOL_UNUSED( rgszNames );
   HB_SYMBOL_UNUSED( cNames );
   HB_SYMBOL_UNUSED( lcid );
   HB_SYMBOL_UNUSED( rgdispid );
   return( ULONG ) E_NOTIMPL;
}

//------------------------------------------------------------------------------
// IEventHandler's Invoke()
// self is where the action happens
// self function receives events (by their ID number) and distributes the processing
// or them or ignores them
static ULONG STDMETHODCALLTYPE Invoke
(
   IEventHandler  *self,
   DISPID         dispid,
   REFIID         riid,
   LCID           lcid,
   WORD           wFlags,
   DISPPARAMS     *params,
   VARIANT        *result,
   EXCEPINFO      *pexcepinfo,
   UINT           *puArgErr
)
{
   PHB_ITEM pItem;
   int      iArg, i;
   PHB_ITEM pItemArray[32];         // max 32 parameters?
   PHB_ITEM *pItems;
   HB_SIZE  ulPos;
   PHB_ITEM Key;

   Key = hb_itemNew( NULL );

   // We implement only a "default" interface
   if( !IsEqualIID( riid, &IID_NULL ) )
   {
      return( ULONG ) DISP_E_UNKNOWNINTERFACE;
   }

   HB_SYMBOL_UNUSED( lcid );
   HB_SYMBOL_UNUSED( wFlags );
   HB_SYMBOL_UNUSED( result );
   HB_SYMBOL_UNUSED( pexcepinfo );
   HB_SYMBOL_UNUSED( puArgErr );

   // delegate work to somewhere else in PRG
   //***************************************
#ifdef __USEHASHEVENTS
   if( hb_hashScan( ( ( MyRealIEventHandler * ) self )->pEvents, hb_itemPutNL( Key, dispid ), &ulPos ) )
   {
      PHB_ITEM pArray = hb_hashGetValueAt( ( ( MyRealIEventHandler * ) self )->pEvents, ulPos );
#else
   #if defined( __XHARBOUR__ )
      ulPos = hb_arrayScan( ( ( MyRealIEventHandler * ) self )->pEvents, hb_itemPutNL( Key, dispid ), NULL, NULL, 0, 0 );
   #else
   ulPos = hb_arrayScan( ( ( MyRealIEventHandler * ) self )->pEvents, hb_itemPutNL( Key, dispid ), NULL, NULL, 0 );
   #endif
   if( ulPos )
   {
      PHB_ITEM pArray = hb_arrayGetItemPtr( ( ( MyRealIEventHandler * ) self )->pEventsExec, ulPos );
#endif
      PHB_ITEM pExec = hb_arrayGetItemPtr( pArray, 1 );

      if( pExec )
      {
         if( hb_vmRequestReenter() )
         {
            switch( hb_itemType( pExec ) )
            {
               case HB_IT_BLOCK:
                  {
#ifdef __XHARBOUR__
                     hb_vmPushSymbol( &hb_symEval );
#else
                     hb_vmPushEvalSym();

#endif
                     hb_vmPush( pExec );
                     break;
                  }

               case HB_IT_STRING:
                  {
                     PHB_ITEM pObject = hb_arrayGetItemPtr( pArray, 2 );
                     hb_vmPushSymbol( hb_dynsymSymbol( hb_dynsymFindName( hb_itemGetCPtr( pExec ) ) ) );

                     if( HB_IS_OBJECT( pObject ) )
                     {
                        hb_vmPush( pObject );
                     }
                     else
                     {
                        hb_vmPushNil();
                     }
                     break;
                  }

               case HB_IT_POINTER:
                  {
                     hb_vmPushSymbol( hb_dynsymSymbol( ( ( PHB_SYMB ) pExec )->pDynSym ) );
                     hb_vmPushNil();
                     break;
                  }
            }

            iArg = params->cArgs;
            for( i = 1; i <= iArg; i++ )
            {
               pItem = hb_itemNew( NULL );
               hb_oleVariantToItem( pItem, &( params->rgvarg[iArg - i] ) );
               pItemArray[i - 1] = pItem;

               // set bit i
               //ulRefMask |= ( 1L << ( i - 1 ) );
            }

            if( iArg )
            {
               pItems = pItemArray;
               if( iArg )
               {
                  for( i = 0; i < iArg; i++ )
                  {
                     hb_vmPush( ( pItems )[i] );
                  }
               }
            }

            // execute
            hb_vmDo( ( USHORT ) iArg );

            // En caso de que los parametros sean pasados por referencia
            for( i = iArg; i > 0; i-- )
            {
               if( ( ( &( params->rgvarg[iArg - i] ) )->n1.n2.vt & VT_BYREF ) == VT_BYREF )
               {
                  switch( ( &( params->rgvarg[iArg - i] ) )->n1.n2.vt )
                  {
                     //case VT_UI1|VT_BYREF:
                     //   *((&(params->rgvarg[iArg-i]))->n1.n2.n3.pbVal) = va_arg(argList,unsigned char*);  //pItemArray[i-1]
                     //   break;
                     case VT_I2 | VT_BYREF:
                        *( ( &( params->rgvarg[iArg - i] ) )->n1.n2.n3.piVal ) = ( short ) hb_itemGetNI( pItemArray[i - 1] );
                        break;

                     case VT_I4 | VT_BYREF:
                        *( ( &( params->rgvarg[iArg - i] ) )->n1.n2.n3.plVal ) = ( long ) hb_itemGetNL( pItemArray[i - 1] );
                        break;

                     case VT_R4 | VT_BYREF:
                        *( ( &( params->rgvarg[iArg - i] ) )->n1.n2.n3.pfltVal ) = ( float ) hb_itemGetND( pItemArray[i - 1] );
                        break;

                     case VT_R8 | VT_BYREF:
                        *( ( &( params->rgvarg[iArg - i] ) )->n1.n2.n3.pdblVal ) = ( double ) hb_itemGetND( pItemArray[i - 1] );
                        break;

                     case VT_BOOL | VT_BYREF:
                        *( ( &( params->rgvarg[iArg - i] ) )->n1.n2.n3.pboolVal ) = ( VARIANT_BOOL ) ( hb_itemGetL( pItemArray[i - 1] ) ? 0xFFFF : 0 );
                        break;

                     //case VT_ERROR|VT_BYREF:
                     //   *((&(params->rgvarg[iArg-i]))->n1.n2.n3.pscode) = va_arg(argList, SCODE*);
                     //   break;
                     case VT_DATE | VT_BYREF:
                        *( ( &( params->rgvarg[iArg - i] ) )->n1.n2.n3.pdate ) = ( DATE ) ( double ) ( hb_itemGetDL( pItemArray[i - 1] ) - 2415019 );
                        break;

                        //case VT_CY|VT_BYREF:
                        //   *((&(params->rgvarg[iArg-i]))->n1.n2.n3.pcyVal) = va_arg(argList, CY*);
                        //   break;
                        //case VT_BSTR|VT_BYREF:
                        //   *((&(params->rgvarg[iArg-i]))->n1.n2.n3.pbstrVal = va_arg(argList, BSTR*);
                        //   break;
                        //case VT_UNKNOWN|VT_BYREF:
                        //   pArg->ppunkVal = va_arg(argList, LPUNKNOWN*);
                        //   break;
                        //case VT_DISPATCH|VT_BYREF:
                        //   pArg->ppdispVal = va_arg(argList, LPDISPATCH*);
                        //   break;
                  }
               }
            }

            hb_vmRequestRestore();
         }
      }
   }

   hb_itemRelease( Key );

   return S_OK;
}

//------------------------------------------------------------------------------
// Here's IEventHandler's VTable. It never changes so we can declare it static
static const IEventHandlerVtbl   IEventHandler_Vtbl = { QueryInterface, AddRef, Release, GetTypeInfoCount, GetTypeInfo, GetIDsOfNames, Invoke };

//------------------------------------------------------------------------------
// constructor
// params:
// device_interface        - refers to the interface type of the COM object (whose event we are trying to receive).
// device_event_interface  - indicates the interface type of the outgoing interface supported by the COM object.
//                           This will be the interface that must be implemented by the Sink object.
//                           is essentially derived from IDispatch, our Sink object (self IEventHandler)
//                           is also derived from IDispatch.
typedef IEventHandler            device_interface;

// Hash  // SetupConnectionPoint( oOle:hObj, @hSink, hEvents )             -> nError
// Array // SetupConnectionPoint( oOle:hObj, @hSink, aEvents, aExecEvent ) -> nError

HB_FUNC( SETUPCONNECTIONPOINT )
{
   // Declaration of local variables
   IConnectionPointContainer  *pIConnectionPointContainerTemp = NULL;
   IUnknown                   *pIUnknown = NULL;
   IConnectionPoint           *m_pIConnectionPoint = NULL;
   IEnumConnectionPoints      *m_pIEnumConnectionPoints;
   HRESULT                    hr;
   IID                        rriid = { 0 };
   register IEventHandler     *selfobj;
   DWORD                      dwCookie = 0;

   device_interface           *pdevice_interface = ( device_interface * ) HB_PARNL( 1 );
   MyRealIEventHandler        *pThis;

   // Allocate memory for the IEventHandler object (as a MyRealIEventHandler).
   // Intentional misrepresentation of size to fit within allocated memory.
   selfobj = ( IEventHandler * ) GlobalAlloc( GMEM_FIXED, sizeof( MyRealIEventHandler ) );

   if( !selfobj )
   {
      // Memory allocation failed, return error code.
      hr = E_OUTOFMEMORY;
   }
   else
   {
      // Set up the IEventHandler object by assigning the correct VTable and initializing the reference count.
      selfobj->lpVtbl = ( IEventHandlerVtbl * ) &IEventHandler_Vtbl;
      ( ( MyRealIEventHandler * ) selfobj )->count = 0;

      // Assign the default interface GUID for events to IDispatch
      ( ( MyRealIEventHandler * ) selfobj )->device_event_interface_iid = IID_IDispatch;

      // Query the IUnknown pointer of self, used later to connect to the device_interface's Connection Point.
      hr = selfobj->lpVtbl->QueryInterface( selfobj, &IID_IUnknown, ( void ** ) ( void * ) &pIUnknown );
      if( hr == S_OK && pIUnknown )
      {
         // Query the device interface for its connection point container interface
         hr = pdevice_interface->lpVtbl->QueryInterface
            (
               pdevice_interface,
               &IID_IConnectionPointContainer,
               ( void ** ) ( void * ) &pIConnectionPointContainerTemp
            );

         if( hr == S_OK && pIConnectionPointContainerTemp )
         {
            // Retrieve the list of connection points available in the interface.
            hr = pIConnectionPointContainerTemp->lpVtbl->EnumConnectionPoints( pIConnectionPointContainerTemp, &m_pIEnumConnectionPoints );

            if( hr == S_OK && m_pIEnumConnectionPoints )
            {
               do
               {
                  // Move to the next available connection point.
                  hr = m_pIEnumConnectionPoints->lpVtbl->Next( m_pIEnumConnectionPoints, 1, &m_pIConnectionPoint, NULL );
                  if( hr == S_OK )
                  {
                     // Get the interface GUID from the connection point.
                     if( m_pIConnectionPoint->lpVtbl->GetConnectionInterface( m_pIConnectionPoint, &rriid ) == S_OK )
                     {
                        break;
                     }
                  }
               }
               while( hr == S_OK );

               // Release the enumerator once finished.
               m_pIEnumConnectionPoints->lpVtbl->Release( m_pIEnumConnectionPoints );
            }

            pIConnectionPointContainerTemp->lpVtbl->Release( pIConnectionPointContainerTemp );
            pIConnectionPointContainerTemp = NULL;
         }

         if( hr == S_OK && m_pIConnectionPoint )
         {
            if( hr == S_OK )
            {
               // Set the device event interface GUID to the retrieved GUID.
               ( ( MyRealIEventHandler * ) selfobj )->device_event_interface_iid = rriid;
            }

            // Connect to the connection point and store the connection cookie.
            hr = m_pIConnectionPoint->lpVtbl->Advise( m_pIConnectionPoint, pIUnknown, &dwCookie );
            ( ( MyRealIEventHandler * ) selfobj )->pIConnectionPoint = m_pIConnectionPoint;
            ( ( MyRealIEventHandler * ) selfobj )->dwEventCookie = dwCookie;
         }

         // Release the IUnknown pointer.
         pIUnknown->lpVtbl->Release( pIUnknown );
         pIUnknown = NULL;
      }
   }

   if( selfobj )
   {
      // If successful, store the events list from parameters and assign to the object.
      pThis = ( MyRealIEventHandler * ) selfobj;

#ifndef __USEHASHEVENTS
      pThis->pEventsExec = hb_itemNew( hb_param( 4, HB_IT_ANY ) );
#endif
      pThis->pEvents = hb_itemNew( hb_param( 3, HB_IT_ANY ) );
      HB_STORNL( ( LONG_PTR ) pThis, 2 );
   }

   // Return the HRESULT result code from the setup process.
   hb_retnl( hr );
}

//------------------------------------------------------------------------------
// Disconnect and clean up the connection point, removing the event sink.
//------------------------------------------------------------------------------
HB_FUNC( SHUTDOWNCONNECTIONPOINT )
{
   MyRealIEventHandler  *self = ( MyRealIEventHandler * ) HB_PARNL( 1 );

   if( self->pIConnectionPoint )
   {
      // Unadvise the connection point, releasing the event sink.
      self->pIConnectionPoint->lpVtbl->Unadvise( self->pIConnectionPoint, self->dwEventCookie );
      self->dwEventCookie = 0;

      // Release the connection point object.
      self->pIConnectionPoint->lpVtbl->Release( self->pIConnectionPoint );
      self->pIConnectionPoint = NULL;
   }
}

//------------------------------------------------------------------------------
// Release a previously acquired IDispatch interface pointer to clean up.
//------------------------------------------------------------------------------
HB_FUNC( RELEASEDISPATCH )
{
   IDispatch   *pObj;

   // Retrieve the IDispatch pointer from parameters and release it.
   pObj = ( IDispatch * ) HB_PARNL( 1 );
   pObj->lpVtbl->Release( pObj );
}

#pragma ENDDUMP

#endif
