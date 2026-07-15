/*
 * MINIGUI - Harbour Win32 GUI library source code
 *
 * Copyright 2017-2025 Aleksandr Belov, Sergej Kiselev <bilance@bilance.lv>
 */

#include "minigui.ch"

*-----------------------------------------------------------------------------*
FUNCTION _WindowCargo( FormName, xValue )
*-----------------------------------------------------------------------------*
#ifdef _OBJECT_
   LOCAL o := iif( HB_ISOBJECT( FormName ), FormName, _WindowObj( FormName ) )
   LOCAL i := iif( HB_ISOBJECT( o ), o:Index, GetFormIndex( FormName ) )
#else
   LOCAL i := GetFormIndex( FormName )
#endif

   IF i > 0
      IF PCount() > 1;     _HMG_aFormMiscData2[ i ] := xValue
      ELSE        ; RETURN _HMG_aFormMiscData2[ i ]
      ENDIF
   ENDIF

RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION _ControlCargo( ControlName, FormName, xValue )
*-----------------------------------------------------------------------------*
#ifdef _OBJECT_
   LOCAL o := iif( HB_ISOBJECT( ControlName ), ControlName, _ControlObj( ControlName, FormName ) )
   LOCAL i := iif( HB_ISOBJECT( o ), o:Index, GetControlIndex( ControlName, FormName ) )
#else
   LOCAL i := GetControlIndex( ControlName, FormName )
#endif

   IF i > 0
      IF PCount() > 2;     _HMG_aControlMiscData2[ i ] := xValue
      ELSE        ; RETURN _HMG_aControlMiscData2[ i ]
      ENDIF
   ENDIF

RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION Do_ControlEventProcedure( bBlock, i, p1, p2, p3, p4 )
*-----------------------------------------------------------------------------*
   LOCAL RetVal

   IF HB_ISBLOCK( bBlock ) .AND. i > 0

      _PushEventInfo()

      _HMG_ThisFormIndex := AScan( _HMG_aFormHandles, _HMG_aControlParentHandles[ i ] )
      _HMG_ThisType := 'C'
      _HMG_ThisIndex := i
      _HMG_ThisFormName := _HMG_aFormNames[ _HMG_ThisFormIndex ]
      _HMG_ThisControlName := _HMG_aControlNames[ _HMG_ThisIndex ]

      RetVal := Eval( bBlock, p1, p2, p3, p4 )

      _PopEventInfo()

   ENDIF

RETURN RetVal

*-----------------------------------------------------------------------------*
FUNCTION Do_WindowEventProcedure( bBlock, i, p1, p2, p3, p4 )
*-----------------------------------------------------------------------------*
   LOCAL RetVal

   IF HB_ISBLOCK( bBlock ) .AND. i > 0

      _PushEventInfo()

      _HMG_ThisFormIndex := i
      _HMG_ThisEventType := ''
      _HMG_ThisType := 'W'
      _HMG_ThisIndex := i
      _HMG_ThisFormName := _HMG_aFormNames[ _HMG_ThisFormIndex ]
      _HMG_ThisControlName := ""

      RetVal := Eval( bBlock, p1, p2, p3, p4 )

      _PopEventInfo()

   ENDIF

RETURN RetVal

#ifdef __XHARBOUR__
   #xtranslate hb_ValToExp( [<x,...>] ) => ValToPrgExp( <x> )
#endif
*----------------------------------------------------------------------------*
FUNCTION _o2Log( o, nLen, cMsg, lExt, cLog )
*----------------------------------------------------------------------------*
   LOCAL a, b, c, i, j, k := pCount()
   LOCAL aTmp, xTmp, cTmp, l := .F., xRet := .T.
   LOCAL lLog := IsErrorLogActive()

   IF ValType( cLog ) $ "LND"
      l := .T.
      cLog := hb_FNameDir( _SetGetLogFile() ) + "_" + hb_ntos( Seconds() )
      _HMG_CreateErrorLog := .T.
   ENDIF

   DEFAULT lExt := .F., cLog := _SetGetLogFile()

   IF HB_ISCHAR( o )
      IF ( i := At( "*", o ) ) > 0
         c := o
         o := Upper( o )
         b := SubStr( o + " ", i + 1 )
         
         IF Left( o, i ) $ "F*,FORM*,FORMS*"
            DEFAULT nLen := 12
            o := {}
            b := iif( Empty( b ), "", Upper( AllTrim( b ) ) )
            FOR EACH a IN HMG_GetForms( , .T. )
                IF iif( Empty( b ), .T., a:Type $ b )
                   AAdd( o, { a:Type, IsWindowVisible( a:Handle ), ;
                              a:Index, a:Name, a:Handle, a:Title, ;
                              a:cProcFile, a:cProcName, a:nProcLine } )
                ENDIF
            NEXT
            c := iif( Empty( b ), "", "<" + c + ">" + " " )
            DEFAULT cMsg := "==> aForms: " + c
            k := 3
         ELSEIF Left( o, i ) $ "P*,PROC*,PROCNL*"       
            DEFAULT nLen := 25
            b := Val( b )
            i := 0
            o := oHmgData()
            WHILE ( ++i < 100 )
               IF Empty( j := ProcName( i ) ) ; EXIT
               ENDIF
               o:Set( TR0( ProcFile( i ), nLen - 7 ) + Str( ProcLine( i ), 7 ), j )
               IF b > 0 .AND. i >= b ; EXIT
               ENDIF
            END
            DEFAULT cMsg := "==> ProcNL: "
            k := 3
         ENDIF
      ENDIF
   ENDIF

   DEFAULT nLen := 19

   IF k > 2 .AND. HB_ISCHAR( cMsg ) ; _LogFile( {.T., cLog}, cMsg )
   ELSEIF !Empty( cMsg )            ; _LogFile( {.T., cLog}, NIL )
   ENDIF

   IF HB_ISOBJECT( o )
      _LogFile( {.F., cLog}, "O:" + o:ClassName )
      IF o:ClassName $ "THMGDATA,TKEYDATA,TTHRDATA,TINIDATA"
         _LogFile( {.F., cLog}, o:GetAll() )
         IF o:ClassName == "TINIDATA"
            _LogFile( {.F., cLog}, o:cIni )
         ENDIF
         FOR EACH a IN o:GetAll()
             i := hb_enumIndex( a )
             b := iif( HB_ISCHAR( a[1] ), TR0( a[1], nLen ), a[1] )
             _LogFile( {.T., cLog}, TR0( i, nLen - 1 ) + ".", b, "=" )
             IF HB_ISOBJECT( a[2] )
                IF lExt
                   _o2Log( a[2], nLen + 5, , , cLog )
                ELSE
                   _LogFile( {.F., cLog}, "O:" + a[2]:ClassName )
                   IF a[2]:ClassName $ "THMGDATA,TKEYDATA,TTHRDATA,TINIDATA"
                      _LogFile( {.F., cLog}, a[2]:GetAll() )
                      IF a[2]:ClassName == "TINIDATA"
                         _LogFile( {.F., cLog}, a[2]:cIni )
                      ENDIF
                   ELSEIF a[2]:ClassName == "TWNDDATA"
                      _LogFile( {.F., cLog}, a[2]:Name, a[2]:Type )
                   ELSEIF a[2]:ClassName $ "TCNLDATA,TTSBDATA,TGETDATA,TSTBDATA"
                      _LogFile( {.F., cLog}, a[2]:Window, a[2]:Name, a[2]:Type )
                   ELSEIF a[2]:ClassName == "TSBROWSE"
                     _LogFile( {.F., cLog}, a[2]:cControlName, a[2]:cParentWnd, a[2]:cAlias )
                   ENDIF 
                ENDIF 
             ELSEIF HB_ISARRAY( a[2] ) .AND. lExt
                aTmp := {}
                FOR EACH xTmp IN a[2]
                    IF HB_ISOBJECT( xTmp )
                       cTmp := "O:" + xTmp:ClassName
                       IF xTmp:ClassName $ "THMGDATA,TKEYDATA,TTHRDATA,TINIDATA"
                          cTmp += " "+"ARRAY["  + hb_ntos( xTmp:Len() ) + "]"
                          IF xTmp:ClassName == "TINIDATA"
                             cTmp += " "+xTmp:cIni
                          ENDIF
                       ELSEIF xTmp:ClassName == "TWNDDATA"
                          cTmp += " "+xTmp:Name+" "+xTmp:Type
                       ELSEIF xTmp:ClassName $ "TCNLDATA,TTSBDATA,TGETDATA,TSTBDATA"
                          cTmp += " "+xTmp:Window+" "+xTmp:Name+" "+xTmp:Type
                       ELSEIF xTmp:ClassName == "TSBROWSE"
                          cTmp += " "+xTmp:cControlName+" "+xTmp:cParentWnd+" "+xTmp:cAlias
                       ENDIF
                       AAdd( aTmp, cTmp )
                    ELSE
                       AAdd( aTmp, xTmp )
                    ENDIF
                NEXT
                _LogFile( {.F., cLog}, hb_valtoexp( aTmp ) )
             ELSE
                _LogFile( {.F., cLog}, a[2] )
             ENDIF
         NEXT

      ELSEIF o:ClassName == "TWNDDATA"
         _LogFile( {.F., cLog}, o:Name, o:Type )

      ELSEIF o:ClassName $ "TCNLDATA,TTSBDATA,TGETDATA,TSTBDATA"
         _LogFile( {.F., cLog}, o:Window, o:Name, o:Type )

      ELSEIF o:ClassName == "TSBROWSE"
         _LogFile( {.F., cLog}, o:cControlName, o:cParentWnd, o:cAlias )
      ENDIF

   ELSEIF HB_ISARRAY( o )
      _LogFile( {.F., cLog}, o )

      FOR EACH a IN o
          i := hb_enumIndex( a )
          IF ! HB_ISARRAY( a )
             j := TR0( i, nLen - 1 ) + "." + " " + '"' + ValType( a ) + '"'
             _LogFile( {.T., cLog}, j + " -> " )
          ENDIF
          IF HB_ISOBJECT( a )
             _o2Log( a, nLen + 5, , lExt, cLog )
          ELSEIF HB_ISARRAY( a )
             _LogFile( {.T., cLog}, TR0( i, nLen - 1 ) + "." )
             IF lExt
                _LogFile( {.F., cLog}, a )
                FOR EACH j IN a
                    k := hb_enumIndex( j )
                    IF ValType( j ) $ "AO"
                       _o2Log( j, nLen + 5, , , cLog )
                    ELSE
                       _LogFile( {.T., cLog}, TR0( k, nLen - 1 + 5 ) + ".", hb_valtoexp( j ) )
                    ENDIF
                NEXT
             ELSE
                _LogFile( {.F., cLog}, hb_valtoexp( a ) )
             ENDIF
          ELSE
             _LogFile( {.F., cLog}, a )
          ENDIF
      NEXT

   ELSE
      _LogFile( {.F., cLog}, o )
   ENDIF

   IF l .AND. hb_FileExists( cLog )
      xRet := hb_MemoRead( cLog )
      hb_FileDelete( cLog )
      _HMG_CreateErrorLog := lLog
   ENDIF

RETURN xRet

*----------------------------------------------------------------------------*
STATIC FUNCTION TR0( cTxt, nLen, cSim )
*----------------------------------------------------------------------------*
   IF !HB_ISCHAR( cTxt ) ; cTxt := cValToChar( cTxt )
   ENDIF

   DEFAULT nLen := Len( cTxt )

   IF cSim == Nil; cSim := " "
   ENDIF

RETURN PadL( AllTrim( cTxt ), nLen, cSim )

*----------------------------------------------------------------------------*
FUNCTION _wPost( nEvent, nIndex, xParam )
*----------------------------------------------------------------------------*
#ifdef _OBJECT_
   LOCAL oWnd, cForm

   IF HB_ISOBJECT( nIndex )
      IF nIndex:ClassName == 'TSBROWSE'
         cForm := nIndex:cParentWnd
         nIndex := GetControlIndex( cForm, nIndex:cControlName )
      ELSE
         cForm := nIndex:Name
         nIndex := NIL
      ENDIF
   ELSEIF HB_ISCHAR( nIndex )
      cForm := nIndex
      nIndex := NIL
   ELSE
      cForm := _HMG_THISFORMNAME
   ENDIF

   IF _HMG_lOOPEnabled
      IF ! Empty( cForm ) .AND. HB_ISCHAR( cForm )
         oWnd := _WindowObj( cForm )
         IF HB_ISOBJECT( oWnd )
            IF HB_ISCHAR( nEvent )
               nEvent := oWnd:oEvents:Get( nEvent, nEvent )
            ENDIF
            IF HB_ISNUMERIC( nEvent )
               DoEvents() ; oWnd:PostMsg( nEvent, nIndex, xParam )
            ENDIF
         ENDIF
      ENDIF
   ENDIF
#else
   HB_SYMBOL_UNUSED( nEvent )
   HB_SYMBOL_UNUSED( nIndex )
   HB_SYMBOL_UNUSED( xParam )
#endif

RETURN NIL

*----------------------------------------------------------------------------*
FUNCTION _wSend( nEvent, nIndex, xParam )
*----------------------------------------------------------------------------*
#ifdef _OBJECT_
   LOCAL oWnd, cForm

   IF HB_ISOBJECT( nIndex )
      IF nIndex:ClassName == 'TSBROWSE'
         cForm := nIndex:cParentWnd
         nIndex := GetControlIndex( cForm, nIndex:cControlName )
      ELSE
         cForm := nIndex:Name
         nIndex := NIL
      ENDIF
   ELSEIF HB_ISCHAR( nIndex )
      cForm := nIndex
      nIndex := NIL
   ELSE
      cForm := _HMG_THISFORMNAME
   ENDIF

   IF _HMG_lOOPEnabled
      IF ! Empty( cForm ) .AND. HB_ISCHAR( cForm )
         oWnd := _WindowObj( cForm )
         IF HB_ISOBJECT( oWnd )
            IF HB_ISCHAR( nEvent )
               nEvent := oWnd:oEvents:Get( nEvent, nEvent )
            ENDIF
            IF HB_ISNUMERIC( nEvent )
               DoEvents() ; oWnd:SendMsg( nEvent, nIndex, xParam )
            ENDIF
         ENDIF
      ENDIF
   ENDIF
#else
   HB_SYMBOL_UNUSED( nEvent )
   HB_SYMBOL_UNUSED( nIndex )
   HB_SYMBOL_UNUSED( xParam )
#endif

RETURN NIL

#ifdef _OBJECT_

*-----------------------------------------------------------------------------*
FUNCTION _WindowObj( FormName )
*-----------------------------------------------------------------------------*
   LOCAL h := iif( HB_ISNUMERIC( FormName ), FormName, GetFormHandle( FormName ) )

RETURN hmg_GetWindowObject( h )

*-----------------------------------------------------------------------------*
FUNCTION _ControlObj( ControlName, FormName )
*-----------------------------------------------------------------------------*
   LOCAL h := iif( HB_ISNUMERIC( ControlName ), ControlName, ;
      GetControlHandle( ControlName, FormName ) )

   IF ISARRAY( h )
      h := h[ 1 ]
   ENDIF

RETURN hmg_GetWindowObject( h )

*-----------------------------------------------------------------------------*
FUNCTION _oThis( oThis, lSets ) // Snapshot of current data This or Sets
*-----------------------------------------------------------------------------*
   LOCAL c, i := 0, k, o := oHmgData()

   IF HB_ISLOGICAL( oThis )
      lSets := oThis
      oThis := Nil
   ENDIF

   DEFAULT lSets := .F.

   o:FormIndex      := _HMG_ThisFormIndex
   o:EventType      := _HMG_ThisEventType
   o:Type           := _HMG_ThisType
   o:Index          := _HMG_ThisIndex
   o:FormName       := _HMG_ThisFormName
   o:ControlName    := _HMG_ThisControlName
   o:FocusedForm    := ""
   o:FocusedControl := ""
   o:FocusedControlIndex := 0

   DEFAULT o:FormName := "", o:ControlName := ""

   IF !Empty( c := GetFocus() )
      IF ( k := AScan( _HMG_aControlHandles, c ) ) > 0
         o:FocusedControlIndex := k
         o:FocusedControl := _HMG_aControlNames [ k ]
         c := _HMG_aControlParentHandles [ k ]
         IF ( k := AScan( _HMG_aFormHandles, c ) ) > 0
            o:FocusedForm := _HMG_aFormNames [ k ]
         ENDIF
      ELSEIF ( k := AScan( _HMG_aFormHandles, c ) ) > 0
         o:FocusedForm := _HMG_aFormNames [ k ]
      ENDIF
      IF !Empty( o:FocusedForm ) .AND. o:FormName != o:FocusedForm
         IF HB_ISLOGICAL( lSets ) .AND. ( k := GetFormIndex( o:FocusedForm ) ) > 0
            // Sets objects variable
            o:FormName       := o:FocusedForm
            o:FormIndex      := k
            o:EventType      := ""
            o:Type           := iif( _HMG_aFormType [ k ] == "C", "W", _HMG_aFormType [ k ] )
            o:Index          := k
            o:ControlName    := ""
            // Sets This variable
            IF lSets                
               _HMG_ThisFormIndex   := o:FormIndex
               _HMG_ThisEventType   := o:EventType
               _HMG_ThisType        := o:Type
               _HMG_ThisIndex       := o:Index
               _HMG_ThisFormName    := o:FormName
               _HMG_ThisControlName := o:ControlName
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   k := iif( Empty( _HMG_ThisControlName ), o:FocusedControlIndex, _HMG_ThisIndex )
   IF !Empty( k )
      o:ControlCargo        := _HMG_aControlMiscData2     [ k ]
      o:ControlHandle       := _HMG_aControlHandles       [ k ]
      o:ControlParentHandle := _HMG_aControlParenthandles [ k ]
      o:ControlParentName   := GetParentFormName( k )
   ENDIF
   IF _HMG_ThisFormIndex > 0
      o:FormCargo        := _HMG_aFormMiscData2    [ _HMG_ThisFormIndex ]
      o:FormHandle       := _HMG_aFormHandles      [ _HMG_ThisFormIndex ]
      o:FormParentHandle := _HMG_aFormParentHandle [ _HMG_ThisFormIndex ]
      GetFormNameByHandle ( o:FormParentHandle, @c )
      o:FormParentName := c
      o:FormObject := _WindowObj( o:FormHandle )
   ENDIF

   IF HB_ISOBJECT( oThis )
      IF   oThis:ClassName == 'TSBROWSE'
         oThis := oThis:cParentWnd
      ELSEIF oThis:ClassName == 'TWNDDATA'
         oThis := oThis:Name
      ENDIF
   ENDIF

   IF HB_ISCHAR( oThis ) ; oThis := GetFormIndex( oThis )
   ENDIF

   IF HB_ISNUMERIC( oThis ) .AND. oThis > 0 ; i := oThis
   ENDIF

   IF i > 0
      _HMG_ThisFormIndex   := i
      _HMG_ThisEventType   := ""
      _HMG_ThisType        := iif( _HMG_aFormType[ i ] == "C", "W", _HMG_aFormType[ i ] )
      _HMG_ThisIndex       := i
      _HMG_ThisFormName    := _HMG_aFormNames[ i ]
      _HMG_ThisControlName := ""
   ELSEIF HB_ISOBJECT( oThis ) .AND. oThis:ClassName $ "THMGDATA,TKEYDATA,TTHRDATA"
      IF !Empty( oThis:FormIndex )
         _HMG_ThisFormIndex   := oThis:FormIndex
         _HMG_ThisEventType   := oThis:EventType
         _HMG_ThisType        := oThis:Type
         _HMG_ThisIndex       := oThis:Index
         _HMG_ThisFormName    := oThis:FormName
         _HMG_ThisControlName := oThis:ControlName
      ENDIF
   ENDIF

RETURN o

*-----------------------------------------------------------------------------*
FUNCTION _pPost( nEvent, nParam, xParam )
*-----------------------------------------------------------------------------*
   LOCAL oApp := oDlu2Pixel()

   DEFAULT nParam := 0

   IF oApp:IsError ; RETURN oApp:cError
   ELSEIF oApp:IsMsg
      IF HB_ISCHAR( nEvent )
         nEvent := oApp:oEvents:Get( nEvent, nEvent )
      ENDIF
      IF HB_ISNUMERIC( nEvent )
         DoEvents() ; oApp:PostMsg( nEvent, nParam, xParam )
      ENDIF
   ENDIF

RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION _pSend( nEvent, nParam, xParam )
*-----------------------------------------------------------------------------*
   LOCAL oApp := oDlu2Pixel()

   DEFAULT nParam := 0

   IF oApp:IsError ; RETURN oApp:cError
   ELSEIF oApp:IsMsg
      IF HB_ISCHAR( nEvent )
         nEvent := oApp:oEvents:Get( nEvent, nEvent )
      ENDIF
      IF HB_ISNUMERIC( nEvent )
         oApp:SendMsg( nEvent, nParam, xParam )
      ENDIF
   ENDIF

RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION Do_Obj( nHandle, bBlock, p1, p2, p3 )
*-----------------------------------------------------------------------------*
   LOCAL o

   IF HB_ISCHAR( nHandle )
      nHandle := GetFormHandle( nHandle )
   ENDIF

   IF hmg_IsWindowObject( nHandle )
      o := hmg_GetWindowObject( nHandle )
      IF ISBLOCK( bBlock )
         IF o:IsWindow                               // set the environment This window
            RETURN Do_WindowEventProcedure ( bBlock, o:Index, o, p1, p2, p3 )
         ELSE                                        // set the environment This control
            RETURN Do_ControlEventProcedure( bBlock, o:Index, o, p1, p2, p3 )
         ENDIF
      ELSEIF bBlock != NIL                           // do not change the environment This
         RETURN o:Event( bBlock, o, p1, p2, p3 )     // bBlock - execution key
      ENDIF
   ENDIF

RETURN o

*----------------------------------------------------------------------------*
FUNCTION oRecGet( aFieldList, oRec )
*----------------------------------------------------------------------------*
   LOCAL cFld, nPos

   DEFAULT oRec := oHmgData()

   IF HB_ISCHAR( aFieldList )
      aFieldList := AllTrim( aFieldList )
      IF Left( aFieldList, 1 ) == "{" .AND. Right( aFieldList, 1 ) == "}"
         aFieldList := &( aFieldList )  // after hb_valtoexp(...)
      ELSE
         aFieldList := hb_ATokens( aFieldList, "," )
      ENDIF
   ENDIF

   IF HB_ISARRAY( aFieldList )
      FOR EACH cFld IN aFieldList
          IF Empty( cFld ) ; LOOP
          ELSEIF HB_ISARRAY( cFld ) ; cFld := cFld[1]  // array as dbStruct()
          ENDIF
          cFld := Upper( AllTrim( cFld ) )
          IF ( nPos := FieldPos( cFld ) ) > 0
             oRec:Set( cFld, FieldGet( nPos ) )
          ENDIF
      NEXT
   ELSE
      AEval( dbStruct(), {|a, n| oRec:Set( a[1], FieldGet( n ) ) } )
   ENDIF

RETURN oRec

*----------------------------------------------------------------------------*
FUNCTION oRecPut( oRec, aFieldList )
*----------------------------------------------------------------------------*
   LOCAL aFld, cFld, nPos, xVal, nCnt := 0

   IF HB_ISCHAR( aFieldList )
      aFieldList := AllTrim( aFieldList )
      IF Left( aFieldList, 1 ) == "{" .AND. Right( aFieldList, 1 ) == "}"
         aFieldList := &( aFieldList )  // after hb_valtoexp(...)
      ELSE
         aFieldList := hb_ATokens( aFieldList, "," )
      ENDIF
   ENDIF

   IF ISARRAY( aFieldList )

      FOR EACH cFld IN aFieldList
          IF HB_ISARRAY(cFld) ; cFld := aFld[1] // array as dbStruct()
          ENDIF
          IF Empty( cFld ) .OR. !HB_ISCHAR( cFld )
             LOOP
          ENDIF
          cFld := Upper( AllTrim( cFld ) )
          xVal := oRec:Get( cFld )
          IF xVal == NIL ; LOOP                 // no key or value
          ENDIF
          IF ( nPos := FieldPos( cFld ) ) > 0
             IF FieldType( nPos ) $ "+^="
                LOOP                            // write protection
             ENDIF
             FieldPut( nPos, xVal )
             nCnt++
          ENDIF
      NEXT

   ELSE

      FOR EACH aFld IN oRec:GetAll()
          cFld := aFld[1]
          IF !HB_ISCHAR( cFld ) .OR. aFld[2] == NIL
             LOOP
          ENDIF
          IF ( nPos := FieldPos( cFld ) ) > 0
             IF FieldType( nPos ) $ "+^="
                LOOP                            // write protection
             ENDIF
             FieldPut( nPos, aFld[2] )
             nCnt++
          ENDIF
      NEXT

   ENDIF

RETURN nCnt > 0

*-----------------------------------------------------------------------------*
FUNCTION Do_OnWndInit( i, cVar )
*-----------------------------------------------------------------------------*
   LOCAL nIndex := i
   LOCAL cName := _HMG_aFormNames[ i ]
   LOCAL nHandle := _HMG_aFormHandles[ i ]
   LOCAL nParent := _HMG_aFormParentHandle[ i ]
   LOCAL cType := _HMG_aFormType[ i ]

RETURN oWndData( nIndex, cName, nHandle, nParent, cType, cVar )

*-----------------------------------------------------------------------------*
FUNCTION Do_OnWndRelease( i )
*-----------------------------------------------------------------------------*
   LOCAL o
   LOCAL hWnd := _HMG_aFormHandles[ i ]

   IF hmg_IsWindowObject( hWnd )
      o := hmg_GetWindowObject( hWnd )
      IF __objHasMethod( o, 'Del' )
         o:Del()
      ENDIF
      IF __objHasMethod( o, 'Destroy' )
         o:Destroy()
      ENDIF
      RETURN .T.
   ENDIF

RETURN .F.

*-----------------------------------------------------------------------------*
FUNCTION Do_OnCtlInit( i, cVar )
*-----------------------------------------------------------------------------*
   LOCAL nCtlIndex := i
   LOCAL cCtlName  := _HMG_aControlNames[ i ]
   LOCAL nHandle  := iif( ISARRAY( _HMG_aControlHandles[ i ] ), ;
      _HMG_aControlHandles[ i ][ 1 ], _HMG_aControlHandles[ i ] )
   LOCAL nParent  := _HMG_aControlParentHandles[ i ]
   LOCAL cFormName := GetParentFormName( i )
   LOCAL cCtlType  := iif( Empty( cFormName ), _HMG_aControlType[ i ], ;
      GetProperty( cFormName, cCtlName, "Type" ) )

RETURN oCnlData( nCtlIndex, cCtlName, nHandle, nParent, cCtlType, cVar )

*-----------------------------------------------------------------------------*
FUNCTION Do_OnCtlRelease( i )
*-----------------------------------------------------------------------------*
   LOCAL o
   LOCAL hWnd := _HMG_aControlHandles[ i ]

   IF hmg_IsWindowObject( hWnd )
      o := hmg_GetWindowObject( hWnd )
      IF __objHasMethod( o, 'Del' )
         o:Del()
      ENDIF
      IF __objHasMethod( o, 'Destroy' )
         o:Destroy()
      ENDIF
      RETURN .T.
   ENDIF

RETURN .F.

*-----------------------------------------------------------------------------*
FUNCTION Do_OnWndLaunch( hWnd, nMsg, wParam, lParam )
*-----------------------------------------------------------------------------*
   IF hmg_IsWindowObject ( hWnd )
      hmg_GetWindowObject( hWnd ):DoEvent( wParam, lParam )
   ENDIF

   HB_SYMBOL_UNUSED( nMsg )

RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION Do_OnCtlLaunch( hWnd, nMsg, wParam, lParam )
*-----------------------------------------------------------------------------*
   HB_SYMBOL_UNUSED( nMsg )

   IF ! Empty( lParam )
      hWnd := lParam
   ENDIF

   IF hmg_IsWindowObject ( hWnd )
      hmg_GetWindowObject( hWnd ):DoEvent( wParam, lParam )
   ENDIF

RETURN NIL

#pragma BEGINDUMP

#include <mgdefs.h>
#include "hbapiitm.h"
#include <commctrl.h>

HB_FUNC( HMG_SETWINDOWOBJECT )
{
   PHB_ITEM pObject;
   HWND hWnd = hmg_par_raw_HWND( 1 );

   if( IsWindow( hWnd ) )
   {
      pObject = ( PHB_ITEM ) hb_param( 2, HB_IT_OBJECT );

      if( pObject && HB_IS_OBJECT( pObject ) )
      {
         pObject = hb_itemNew( pObject );

         hb_gcLock( pObject );    // Ref++

         SetWindowLongPtr( hWnd, GWLP_USERDATA, ( LPARAM ) pObject );

         hb_retl( TRUE );
      }
      else
         hb_retl( FALSE );
   }
   else
      hb_retl( FALSE );
}

HB_FUNC( HMG_DELWINDOWOBJECT )
{
   PHB_ITEM pObject;
   HWND hWnd = hmg_par_raw_HWND( 1 );

   if( IsWindow( hWnd ) )
   {
      pObject = ( PHB_ITEM ) GetWindowLongPtr( hWnd, GWLP_USERDATA );

      SetWindowLongPtr( hWnd, GWLP_USERDATA, 0 );

      if( pObject && HB_IS_OBJECT( pObject ) )
      {
         hb_gcUnlock( pObject ); // Ref --
         hb_itemRelease( pObject );
      }
   }
}

HB_FUNC( HMG_GETWINDOWOBJECT )
{
   HWND hWnd = hmg_par_raw_HWND( 1 );

   if( IsWindow( hWnd ) )
      hb_itemReturn( ( PHB_ITEM ) GetWindowLongPtr( hWnd, GWLP_USERDATA ) );
   else
      hb_ret();
}

HB_FUNC( HMG_ISWINDOWOBJECT )
{
   PHB_ITEM pObject;

   HWND hWnd = hmg_par_raw_HWND( 1 );

   if( IsWindow( hWnd ) )
   {
      pObject = ( PHB_ITEM ) GetWindowLongPtr( hWnd, GWLP_USERDATA );

      hb_retl( pObject && HB_IS_OBJECT( pObject ) );
   }
   else
      hb_retl( FALSE );
}

#pragma ENDDUMP

#endif
