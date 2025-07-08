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

----------------------------------------------------------------------------*/

#include "minigui.ch"

#ifdef _DBFBROWSE_

#include "i_winuser.ch"
#include "dbstruct.ch"
#include "dbinfo.ch"

#define IS_SQLRDD ( Select() > 0 .AND. ( RddName()=="SQLRDD" .OR. RddName()=="SQLEX" ) )

#ifndef __XHARBOUR__
   SET PROCEDURE TO netfuncs.prg
#endif

#if defined( __XHARBOUR__ ) .OR. ( __HARBOUR__ - 0 < 0x030200 )
#xtranslate hb_StrShrink( <char>, <n> ) => Left( <char>, Len( <char> ) - <n> )
#xtranslate hb_UAt( <c>, <n> ) => At( <c>, <n> )
#xtranslate hb_ULeft( <c>, <n> ) => Left( <c>, <n> )
#xtranslate hb_URight( <c>, <n> ) => Right( <c>, <n> )
#xtranslate hb_ULen( <c> ) => Len( <c> )
#endif

#ifdef HMG_LEGACY_ON
   MEMVAR aresult, l, aWhen, aWhenVarNames
#endif

*-----------------------------------------------------------------------------*
FUNCTION _DefineBrowse ( ControlName, ParentFormName, x, y, w, h, aHeaders, aWidths, aFields, value, ;
      fontname, fontsize, tooltip, change, dblclick, aheadclick, gotfocus, lostfocus, workarea, ;
      delete, nogrid, aimage, ajust, helpid, bold, italic, underline, strikeout, break, ;
      backcolor, fontcolor, lock, inplace, novscroll, appendable, readonly, valid, validmessages, ;
      edit, dynamicforecolor, dynamicbackcolor, awhenfields, nid, aimageheader, notabstop, ;
      inputitems, displayitems, doublebuffer, columnsort, bInit, aPict, aInputMask )
*-----------------------------------------------------------------------------*
   LOCAL ParentFormHandle, ControlHandle, FontHandle
   LOCAL blInit
   LOCAL mVar
   LOCAL k
   LOCAL DeltaWidth
   LOCAL Style
   LOCAL i
   LOCAL lsort
   LOCAL lDialogInMemory
   LOCAL oc := NIL, ow := NIL

#ifdef _OBJECT_
   ow := oDlu2Pixel()
#endif

   IF ( FontHandle := GetFontHandle( FontName ) ) != 0
      GetFontParamByRef( FontHandle, @FontName, @FontSize, @bold, @italic, @underline, @strikeout )
   ENDIF

   IF _HMG_BeginWindowActive .OR. _HMG_BeginDialogActive
      ParentFormName := iif( _HMG_BeginDialogActive, _HMG_ActiveDialogName, _HMG_ActiveFormName )
      __defaultNIL( @FontName, _HMG_ActiveFontName )
      __defaultNIL( @FontSize, _HMG_ActiveFontSize )
   ENDIF

   IF _HMG_FrameLevel > 0 .AND. ! _HMG_ParentWindowActive
      x := x + _HMG_ActiveFrameCol[ _HMG_FrameLevel ]
      y := y + _HMG_ActiveFrameRow[ _HMG_FrameLevel ]
      ParentFormName := _HMG_ActiveFrameParentFormName[ _HMG_FrameLevel ]
   ENDIF

   lDialogInMemory := _HMG_DialogInMemory

   IF .NOT. _IsWindowDefined ( ParentFormName ) .AND. .NOT. lDialogInMemory
      MsgMiniGuiError ( _HMG_BRWLangError[ 1 ] + IFNIL( ParentFormName, "Parent", ParentFormName ) + _HMG_BRWLangError[ 2 ], .F. )
   ENDIF

   IF ISCHAR ( ControlName ) .AND. ControlName == "0"
      ControlName := HMG_GetUniqueName()
   ENDIF

   IF _IsControlDefined ( ControlName, ParentFormName ) .AND. .NOT. lDialogInMemory
      MsgMiniGuiError ( _HMG_BRWLangError[ 4 ] + ControlName + _HMG_BRWLangError[ 5 ] + ParentFormName + _HMG_BRWLangError[ 6 ], .F. )
   ENDIF

#ifndef HMG_LEGACY_ON
   IF hb_defaultValue( edit, .F. )
      inplace := .T.
   ENDIF
#endif
   hb_default( @value, 0 )
   hb_default( @aFields, {} )
   /* code borrowed from ooHG project */
   IF ! HB_ISARRAY( aJust )
      aJust := AFill( Array( Len( aFields ) ), 0 )
   ELSE
      IF Len( aJust ) < Len( aFields )
         ASize( aJust, Len( aFields ) )
      ENDIF
      AEval( aJust, {| x, i | aJust[ i ] := iif( HB_ISNUMERIC( x ), x, 0 ) } )
   ENDIF
   /* end code borrowed */
   // aPict array verify
   IF HB_ISARRAY( aPict )
      IF Len( aPict ) < Len( aFields )
         ASize( aPict, Len( aFields ) )
      ENDIF
      AEval( aPict, {| x, i | aPict[ i ] := x } )
   ELSE
      aPict := AFill( Array( Len( aFields ) ), NIL )
   ENDIF

   __defaultNIL( @aImage, {} )
   __defaultNIL( @aImageHeader, {} )

   DeltaWidth := iif( novscroll, 0, GETVSCROLLBARWIDTH() )

   __defaultNIL( @change, "" )
   __defaultNIL( @dblclick, "" )
   __defaultNIL( @aHeadClick, {} )

   hb_default( @notabstop, .F. )
   hb_default( @doublebuffer, .F. )
   lsort := ( ISARRAY( columnsort ) )

   mVar := '_' + ParentFormName + '_' + ControlName
   k := _GetControlFree()

   IF _HMG_BeginDialogActive

      ParentFormHandle := _HMG_ActiveDialogHandle
      style := LVS_SINGLESEL + LVS_SHOWSELALWAYS + WS_BORDER + WS_CHILD + WS_VISIBLE + LVS_REPORT

      IF ! notabstop
         Style += WS_TABSTOP
      ENDIF

      IF lDialogInMemory // Dialog Template

         blInit := {| x, y, z | InitDialogBrowse( x, y, z ) }
         AAdd( _HMG_aDialogItems, { nId, k, "SysListView32", style, 0, x, y, w - DeltaWidth, h, "", HelpId, tooltip, fontname, fontsize, bold, italic, underline, strikeout, blInit, _HMG_BeginTabActive, .F., _HMG_ActiveTabPage } )

      ELSE

         ControlHandle := GetDialogItemHandle( ParentFormHandle, nId )
         SetWindowStyle ( ControlHandle, style, .T. )

         x := GetWindowCol ( Controlhandle )
         y := GetWindowRow ( Controlhandle )
         w := GetWindowWidth ( Controlhandle )
         h := GetWindowHeight ( Controlhandle )

      ENDIF

   ELSE

      ParentFormHandle := GetFormHandle ( ParentFormName )

      hb_default( @w, 240 )
      hb_default( @h, 120 )

      IF x == NIL .OR. y == NIL

         // If splitboxed force no vertical scrollbar
         novscroll := .T.

         IF _HMG_SplitLastControl == 'TOOLBAR'
            Break := .T.
         ENDIF

         _HMG_SplitLastControl := 'GRID'

         i := GetFormIndex ( ParentFormName )

         IF i > 0

            ControlHandle := InitBrowse ( ParentFormHandle, 0, x, y, w - DeltaWidth, h, NoTabStop ) // Browse+

            x := GetWindowCol ( Controlhandle )
            y := GetWindowRow ( Controlhandle )

            AddSplitBoxItem ( Controlhandle, _HMG_aFormReBarHandle[ i ], w, break, , , , _HMG_ActiveSplitBoxInverted )

         ENDIF

      ELSE

         ControlHandle := InitBrowse ( ParentFormHandle, 0, x, y, w - DeltaWidth, h, NoTabStop ) // Browse+

      ENDIF

   ENDIF

   IF .NOT. lDialogInMemory

      IF IsArrayRGB ( backcolor )
         ListView_SetBkColor ( ControlHandle, backcolor[ 1 ], backcolor[ 2 ], backcolor[ 3 ] )
         ListView_SetTextBkColor ( ControlHandle, backcolor[ 1 ], backcolor[ 2 ], backcolor[ 3 ] )
      ENDIF

      IF IsArrayRGB ( fontcolor )
         ListView_SetTextColor ( ControlHandle, fontcolor[ 1 ], fontcolor[ 2 ], fontcolor[ 3 ] )
      ENDIF

      IF lsort
         aHeadClick := Array( Len( aHeaders ) )
         AEval( aHeadClick, {| x, i | aHeadClick[ i ] := {| n | HMG_SetOrder( n ) }, HB_SYMBOL_UNUSED( x ) } )
      ENDIF

      IF FontHandle != 0
         _SetFontHandle( ControlHandle, FontHandle )
      ELSE
         __defaultNIL( @FontName, _HMG_DefaultFontName )
         __defaultNIL( @FontSize, _HMG_DefaultFontSize )
         FontHandle := _SetFont ( ControlHandle, fontname, fontsize, bold, italic, underline, strikeout )
      ENDIF

      IF tooltip != NIL
         SetToolTip ( ControlHandle, tooltip, GetFormToolTipHandle ( ParentFormName ) )
      ENDIF

   ENDIF

#ifdef _NAMES_LIST_
   _SetNameList( mVar, k )
#else
   PUBLIC &mVar. := k
#endif

   _HMG_aControlType[ k ] := "BROWSE"
   _HMG_aControlNames[ k ] := ControlName
   _HMG_aControlHandles[ k ] := ControlHandle
   _HMG_aControlParenthandles[ k ] := ParentFormHandle
   _HMG_aControlIds[ k ] := nId
   _HMG_aControlProcedures[ k ] := aWidths
   _HMG_aControlPageMap[ k ] := aHeaders
   _HMG_aControlValue[ k ] := value
   _HMG_aControlInputMask[ k ] := Lock
   _HMG_aControllostFocusProcedure[ k ] := lostfocus
   _HMG_aControlGotFocusProcedure[ k ] := gotfocus
   _HMG_aControlChangeProcedure[ k ] := change
   _HMG_aControlDeleted[ k ] := .F.
   _HMG_aControlBkColor[ k ] := aImage // Browse+
   _HMG_aControlFontColor[ k ] := inplace
   _HMG_aControlDblClick[ k ] := dblclick
   _HMG_aControlHeadClick[ k ] := aHeadClick
   _HMG_aControlRow[ k ] := y
   _HMG_aControlCol[ k ] := x
   _HMG_aControlWidth[ k ] := w
   _HMG_aControlHeight[ k ] := h
   _HMG_aControlSpacing[ k ] := _NoQuote ( WorkArea )
   _HMG_aControlContainerRow[ k ] := iif ( _HMG_FrameLevel > 0, _HMG_ActiveFrameRow[ _HMG_FrameLevel ], -1 )
   _HMG_aControlContainerCol[ k ] := iif ( _HMG_FrameLevel > 0, _HMG_ActiveFrameCol[ _HMG_FrameLevel ], -1 )
   _HMG_aControlPicture[ k ] := aImageHeader // Browse+
   _HMG_aControlContainerHandle[ k ] := 0
   _HMG_aControlFontName[ k ] := fontname
   _HMG_aControlFontSize[ k ] := fontsize
   _HMG_aControlFontAttributes[ k ] := { bold, italic, underline, strikeout }
   _HMG_aControlToolTip[ k ] := tooltip
   _HMG_aControlRangeMin[ k ] := aFields
   _HMG_aControlRangeMax[ k ] := {} // Rows array
   _HMG_aControlCaption[ k ] := aHeaders
   _HMG_aControlVisible[ k ] := .T.
   _HMG_aControlHelpId[ k ] := HelpId
   _HMG_aControlFontHandle[ k ] := FontHandle
   _HMG_aControlBrushHandle[ k ] := 0
   _HMG_aControlEnabled[ k ] := .T.
   _HMG_aControlMiscData1[ k ] := { ;
      0, ; // 1
      appendable, ;              // 2
      readonly, ;                // 3
      valid, ;                   // 4
      validmessages, ;           // 5
      edit, ;                    // 6
      nogrid, ;                  // 7
      novscroll, ;               // 8
      dynamicforecolor, ;        // 9
      dynamicbackcolor, ;        // 10
      aWhenFields, ;             // 11
      delete, ;                  // 12
      inputitems, ;              // 13
      displayitems, ;            // 14
      0, ;                       // 15
      aJust, ;                   // 16
      NIL, ;                     // 17
      NIL, ;                     // 18
      doublebuffer, ;            // 19
      iif( lsort, Array( Len( aHeaders ) ), 0 ), ;  // 20
      aPict, ;                   // 21 add jsz
      aInputMask }               // 22
   _HMG_aControlMiscData2[ k ] := ''

   IF .NOT. lDialogInMemory

      IF lsort

         AFill( _HMG_aControlMiscData1[ k ][ 20 ], .T. )

         IF Len( columnsort ) > 0

            FOR i := 1 TO Min( Len( columnsort ), Len( _HMG_aControlMiscData1[ k ][ 20 ] ) )

               IF ISLOGICAL( columnsort[ i ] )
                  _HMG_aControlMiscData1[ k ][ 20 ][ i ] := columnsort[ i ]
               ENDIF

            NEXT i

         ENDIF

         HMG_OrdCreate( k )

      ENDIF

      InitDialogBrowse( ParentFormName, ControlHandle, k )

   ENDIF

   IF _HMG_lOOPEnabled
      Eval ( _HMG_bOnControlInit, k, mVar )

#ifdef _OBJECT_
      ow := _WindowObj ( ParentFormHandle )
      oc := _ControlObj( ControlHandle )
#endif
   ENDIF

   Do_ControlEventProcedure ( bInit, k, ow, oc )

RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION InitDialogBrowse( ParentName, ControlHandle, i )
*-----------------------------------------------------------------------------*
   LOCAL ParentFormHandle, ScrollBarHandle, ScrollBarButtonHandle
   LOCAL aJust, aImageHeader
   LOCAL wBitmap
   LOCAL hsum := 0
   LOCAL nogrid, doublebuffer
   LOCAL x, w, y, h, z

   x := _HMG_aControlCol[ i ]
   w := _HMG_aControlWidth[ i ]
   y := _HMG_aControlRow[ i ]
   h := _HMG_aControlHeight[ i ]

   ParentFormHandle := _HMG_aControlParenthandles[ i ]
   nogrid := _HMG_aControlMiscData1[ i, 7 ]
   aJust := _HMG_aControlMiscData1[ i, 16 ]
   doublebuffer := _HMG_aControlMiscData1[ i, 19 ]
   aImageHeader := _HMG_aControlPicture[ i ]

   SendMessage( ControlHandle, LVM_SETEXTENDEDLISTVIEWSTYLE, 0, iif( nogrid, 0, LVS_EX_GRIDLINES ) + ;
      iif( doublebuffer, LVS_EX_DOUBLEBUFFER, 0 ) + LVS_EX_FULLROWSELECT + LVS_EX_INFOTIP + LVS_EX_HEADERDRAGDROP )

   wBitmap := iif( Len( _HMG_aControlBkColor[ i ] ) > 0, AddListViewBitmap( ControlHandle, _HMG_aControlBkColor[ i ] ), 0 ) // Add Bitmap Column
   _HMG_aControlProcedures[ i, 1 ] := Max ( _HMG_aControlProcedures[ i, 1 ], wBitmap + GetBorderWidth() / 2 ) // Set Column 1 width to Bitmap width

   IF Len( aImageHeader ) > 0
      _HMG_aControlMiscData1[ i, 15 ] := AddListViewBitmapHeader( ControlHandle, aImageHeader ) // Add Header Bitmaps
   ENDIF

   InitListViewColumns ( ControlHandle, _HMG_aControlCaption[ i ], _HMG_aControlProcedures[ i ], aJust )

   // Add to browselist array to update on window activation
   AAdd ( _HMG_aFormBrowseList[ GetFormIndex ( ParentName ) ], i )

   FOR z := 1 TO Len ( _HMG_aControlProcedures[ i ] )
      hsum += ListView_GetColumnWidth ( _HMG_aControlHandles[ i ], z - 1 )
      _HMG_aControlProcedures[ i ][ z ] := ListView_GetColumnWidth ( _HMG_aControlHandles[ i ], z - 1 )
   NEXT z

   IF Len( aImageHeader ) == Len ( _HMG_aControlPageMap[ i ] )
      FOR z := 1 TO Len ( _HMG_aControlPageMap[ i ] )
         SetGridColumnHeaderImage ( _HMG_aControlHandles[ i ], z, z, ( aJust[ z ] == 1 ) )
      NEXT z
   ENDIF

   // Add Vertical scrollbar
   IF _HMG_aControlMiscData1[ i, 8 ] == .F.

      IF hsum > w - GETVSCROLLBARWIDTH() - 4
         ScrollBarHandle := InitVScrollBar ( ParentFormHandle, x + w - GETVSCROLLBARWIDTH(), y, GETVSCROLLBARWIDTH(), h - GETHSCROLLBARHEIGHT() )
         ScrollBarButtonHandle := InitVScrollBarButton ( ParentFormHandle, x + w - GETVSCROLLBARWIDTH(), y + h - GETHSCROLLBARHEIGHT(), GETVSCROLLBARWIDTH(), GETHSCROLLBARHEIGHT() )
      ELSE
         ScrollBarHandle := InitVScrollBar ( ParentFormHandle, x + w - GETVSCROLLBARWIDTH(), y, GETVSCROLLBARWIDTH(), h )
         ScrollBarButtonHandle := InitVScrollBarButton ( ParentFormHandle, x + w - GETVSCROLLBARWIDTH(), y + h - GETHSCROLLBARHEIGHT(), 0, 0 )
      ENDIF

      IF _HMG_BeginTabActive
         AAdd ( _HMG_ActiveTabCurrentPageMap, { ControlHandle, ScrollBarHandle, ScrollBarButtonHandle } )
      ENDIF

   ELSE

      ScrollBarHandle := 0

      IF _HMG_BeginTabActive
         AAdd ( _HMG_ActiveTabCurrentPageMap, ControlHandle )
      ENDIF

   ENDIF

   _HMG_aControlIds[ i ] := ScrollBarHandle
   _HMG_aControlMiscData1[ i ][ 1 ] := ScrollBarButtonHandle

   _BrowseRefresh( '', '', i )

   IF Len( _HMG_aDialogTemplate ) != 0 .AND. _HMG_aDialogTemplate[ 3 ] // Modal
      _HMG_aControlDeleted[ i ] := .T.
   ENDIF

RETURN NIL

*-----------------------------------------------------------------------------*
STATIC PROCEDURE HMG_OrdCreate( i )
*-----------------------------------------------------------------------------*
   LOCAL _Alias
   LOCAL _BrowseArea
   LOCAL nColumn
   LOCAL aFields
   LOCAL cField
   LOCAL cOrdKey

   _Alias := Alias()
   _BrowseArea := _HMG_aControlSpacing[ i ]

   IF Select( _BrowseArea ) == 0
      RETURN
   ENDIF

   SELECT &_BrowseArea

   aFields := _HMG_aControlRangeMin[ i ]

   ordListClear()

   ordCondSet( , , .T. /*All*/, , , , RecNo(), , , , , , , , , , , .T. /*Memory*/, , )

   FOR nColumn := 1 TO Len( aFields )

      IF _HMG_aControlMiscData1[ i ][ 20 ][ nColumn ] == .F. .OR. FieldPos( aFields[ nColumn ] ) == 0
         ordCreate( , 'Bag' + StrZero( Random( 999999 ), 6 ), 'Field->' + FieldName( 1 ) )
      ELSE
         cField := FieldName( FieldPos( aFields[ nColumn ] ) )
         cOrdKey := Alias() + '->' + cField

#ifndef __XHARBOUR__
         ordCreate( , cField, cOrdKey, hb_macroBlock( cOrdKey ), .F. /*lUnique*/ )
#else
         ordCreate( , cField, cOrdKey, &( '{|| ' + cOrdKey + '}' ), .F. /*lUnique*/ )
#endif
      ENDIF

   NEXT

   ordSetFocus( 0 )
   GO TOP

   RestoreWorkArea( _Alias )

RETURN

*-----------------------------------------------------------------------------*
STATIC PROCEDURE RestoreWorkArea( _Alias )
*-----------------------------------------------------------------------------*
   IF Select( _Alias ) != 0
      dbSelectArea( _Alias )
   ELSE
      dbSelectArea( 0 )
   ENDIF

RETURN

*-----------------------------------------------------------------------------*
PROCEDURE HMG_SetOrder( nColumn, lDescend )
*-----------------------------------------------------------------------------*
   LOCAL cFormName := ThisWindow.Name
   LOCAL cControlName := This.Name
   LOCAL _Alias
   LOCAL _BrowseArea
   LOCAL _BrowseHandle
   LOCAL nOrder
   LOCAL nRecord
   LOCAL i := GetControlIndex( cControlName, cFormName )

   IF i == 0 .OR. nColumn == 0 // add jsz
      RETURN
   ENDIF

   IF _HMG_aControlMiscData1[ i ][ 20 ][ nColumn ] == .T.

      _Alias := Alias()
      _BrowseArea := _HMG_aControlSpacing[ i ]

      IF Select( _BrowseArea ) == 0
         RETURN
      ENDIF

      SELECT &_BrowseArea

      nOrder := ordNumber( ordSetFocus() )
      nRecord := RecNo()

      _BrowseHandle := _HMG_aControlHandles[ i ]

      ListView_SetSortHeader( _BrowseHandle, nOrder, 0, _HMG_IsThemed )

      IF ! ISLOGICAL( lDescend )
         lDescend := iif( nOrder == nColumn, ! ordDescend( nOrder ), .F. )
      ENDIF

      nOrder := nColumn

      ListView_SetSortHeader( _BrowseHandle, nColumn, iif( lDescend, -1, 1 ), _HMG_IsThemed )

      ordSetFocus( nOrder )
      ordDescend( nOrder, NIL, lDescend )

      GO nRecord

      RestoreWorkArea( _Alias )

      _BrowseRefresh( '', '', i )

   ENDIF

RETURN

*-----------------------------------------------------------------------------*
PROCEDURE _BrowseUpdate( ControlName, ParentName, z )
*-----------------------------------------------------------------------------*
   LOCAL aDisplayItems, aDisplayItemsLengths, aProcessDisplayItems
   LOCAL aTemp, _BrowseRecMap := {}
   LOCAL cTemp, Fields
   LOCAL First, image, _Rec
   LOCAL dBc, dFc
   LOCAL processdbc, processdfc
   LOCAL ColorMap, ColorRow
   LOCAL fcolormap, fcolorrow
   LOCAL lFound
   LOCAL PageLength
   LOCAL i, x, j, k
   LOCAL aPict

   i := iif( PCount() == 2, GetControlIndex ( ControlName, ParentName ), z )

   IF Select() == 0
      RETURN
   ENDIF

   Fields := _HMG_aControlRangeMin[ i ]

   aDisplayItems := _HMG_aControlMiscData1[ i ][ 14 ]

   aPict := _HMG_aControlMiscData1[ i ][ 21 ] // add jsz

   aProcessDisplayItems := Array ( Len ( Fields ) )
   aDisplayItemsLengths := Array ( Len ( Fields ) )

   AFill ( aProcessDisplayItems, .F. )
   AFill ( aDisplayItemsLengths, 0 )

   IF ISARRAY ( aDisplayItems )

      FOR k := 1 TO Len ( aProcessDisplayItems )

         IF ISARRAY ( aDisplayItems[ k ] )
            aProcessDisplayItems[ k ] := .T.
            aDisplayItemsLengths[ k ] := Len ( aDisplayItems[ k ] )
         ENDIF

      NEXT k

   ENDIF

   dfc := _HMG_aControlMiscData1[ i, 9 ]
   processdfc := ISARRAY ( dfc )

   dbc := _HMG_aControlMiscData1[ i, 10 ]
   processdbc := ISARRAY ( dbc )

   _HMG_aControlContainerHandle[ i ] := 0

   First := iif( Len( _HMG_aControlBkColor[ i ] ) == 0, 1, 2 ) // Browse+ ( 2= bitmap definido, se cargan campos a partir de 2º )

   ListViewReset ( _HMG_aControlhandles[ i ] )

   PageLength := ListViewGetCountPerPage ( _HMG_aControlhandles[ i ] )

   IF processdfc
      fcolormap := {}
      fcolorrow := {}
   ENDIF

   IF processdbc
      colormap := {}
      colorrow := {}
   ENDIF

   FOR EACH x IN Array( PageLength )

      aTemp := {}

      IF First == 2
         cTemp := Fields[ 1 ]

         SWITCH Left( Type ( cTemp ), 1 )

         CASE 'N'
            image := &cTemp
            EXIT
         CASE 'L'
            image := iif( &cTemp, 1, 0 )
            EXIT
         CASE 'U'
            image := iif( ValType( &cTemp ) == 'N', &cTemp, iif( ValType( &cTemp ) == 'L', iif( &cTemp, 1, 0 ), 0 ) )
            EXIT
         DEFAULT
            image := 0

         END SWITCH

         AAdd ( aTemp, NIL )

         IF processdbc

            IF Len ( dbc ) == Len ( Fields )
               AAdd ( colorrow, -1 )
            ENDIF

         ENDIF

         IF processdfc

            IF Len ( dfc ) == Len ( Fields )
               AAdd ( fcolorrow, -1 )
            ENDIF

         ENDIF

      ENDIF

      FOR EACH cTemp IN Fields

         j := hb_enumindex( cTemp )

         IF j >= First

            IF aProcessDisplayItems[ j ] == .T.
               lFound := .F.
               FOR k := 1 TO aDisplayItemsLengths[ j ]
                  IF aDisplayItems[ j ][ k ][ 2 ] == &cTemp
                     AAdd ( aTemp, RTrim ( aDisplayItems[ j ][ k ][ 1 ] ) )
                     lFound := .T.
                     EXIT
                  ENDIF
               NEXT k

               IF ! lFound
                  AAdd ( aTemp, '' )
               ENDIF
            ELSE
               AAdd ( aTemp, _GetBrowseFieldValue( cTemp, aPict[ j ] ) ) // add jsz
            ENDIF

            IF processdfc

               IF Len ( dfc ) == Len ( Fields )
                  AAdd ( fcolorrow, iif( ISBLOCK ( dfc[ j ] ), _teval ( dfc[ j ] ), -1 ) )
               ENDIF

            ENDIF

            IF processdbc

               IF Len ( dbc ) == Len ( Fields )
                  AAdd ( colorrow, iif( ISBLOCK ( dbc[ j ] ), _teval ( dbc[ j ] ), -1 ) )
               ENDIF

            ENDIF

         ENDIF

      NEXT

      AddListViewItems ( _HMG_aControlhandles[ i ], aTemp, Image )

      _Rec := RecNo()

      AAdd ( _BrowseRecMap, _Rec )

      IF processdfc
         AAdd ( fcolormap, fcolorrow )
         fcolorrow := {}
      ENDIF

      IF processdbc
         AAdd ( colormap, colorrow )
         colorrow := {}
      ENDIF

      SKIP

      IF EOF()
         _HMG_aControlContainerHandle[ i ] := 1
         GO BOTTOM
         EXIT
      ENDIF

   NEXT

   IF processdfc
      _HMG_aControlMiscData1[ i ][ 17 ] := fcolormap
   ENDIF

   IF processdbc
      _HMG_aControlMiscData1[ i ][ 18 ] := colormap
   ENDIF

   _HMG_aControlRangeMax[ i ] := _BrowseRecMap

RETURN

*-----------------------------------------------------------------------------*
FUNCTION _GetBrowseFieldValue ( cTemp, cPict ) // add jsz   param
*-----------------------------------------------------------------------------*
   LOCAL cRet := 'Nil'
   LOCAL aFldInfo := _TypeEx( cTemp )
   LOCAL cType := aFldInfo [1]

   SWITCH Left( cType, 1 )

   CASE 'N'
   CASE '+'
   CASE 'F'
   CASE 'I'
   CASE 'Y'
      cRet := Transform ( &cTemp, cPict ) // add jsz
      EXIT
   CASE 'B'
      cRet := Round( &cTemp, Min( Set( _SET_DECIMALS ), aFldInfo [2] ) )
      cRet := hb_ntos( iif( Int ( cRet ) == cRet, Int( cRet ), cRet ) )
      DO WHILE "." $ cRet .AND. Right( cRet, 1 ) == "0"
         cRet := hb_StrShrink( cRet, 1 )
      ENDDO
      EXIT
   CASE 'D'
      cRet := DToC( &cTemp )
      EXIT
   CASE 'T'
      cRet := hb_TSToStr( &cTemp, .T. )
      EXIT
   CASE 'C'
      cRet := Transform( RTrim( &cTemp ), cPict ) // add jsz
      EXIT
   CASE 'L'
      cRet := iif( &cTemp, '.T.', '.F.' )
      EXIT
   CASE 'M'
      cRet := iif( Empty( &cTemp ), '<memo>', '<Memo>' )
      EXIT
   CASE 'V'
   CASE '@'
      cRet := Transform( RTrim( hb_ValToStr( &cTemp ) ), cPict ) // add jsz
      EXIT
   CASE 'G'
      cRet := '<General>'
      EXIT
   DEFAULT
      IF cType == 'UE'
         cRet := '<R-Next>'
      ELSEIF cType == 'UI'
         cRet := _GetBrowseFnValue( cTemp, cPict ) // add jsz
      ENDIF

   END SWITCH

RETURN cRet

*-----------------------------------------------------------------------------*
FUNCTION _GetBrowseFnValue ( cTemp, cPict ) // add jsz   param
*-----------------------------------------------------------------------------*
   LOCAL cRet := 'Nil'

   SWITCH ValType ( cTemp )

   CASE 'N'
      cRet := Transform ( &cTemp, cPict ) // add jsz
      EXIT
   CASE 'D'
      cRet := DToC ( &cTemp )
      EXIT
   CASE 'L'
      cRet := iif ( &cTemp, '.T.', '.F.' )
      EXIT
   CASE 'C'
      cRet := Transform ( RTrim ( &cTemp ), cPict ) // add jsz
      EXIT
   CASE 'M'
      cRet := '<Memo>'

   END SWITCH

RETURN cRet

*-----------------------------------------------------------------------------*
STATIC FUNCTION _TypeEx ( cTemp )
*-----------------------------------------------------------------------------*
   LOCAL aRet := Array( 2 )
   LOCAL aStruct
   LOCAL cAlias
   LOCAL nFieldPos

   aStruct := dbStruct()

   IF ( cAlias := Alias() ) $ cTemp
      nFieldPos := AScan ( aStruct, {| x | cAlias + "->" + x[ DBS_NAME ] == Upper( cTemp ) } )
   ELSE
      nFieldPos := AScan ( aStruct, {| x | x[ DBS_NAME ] == Upper( cTemp ) } )
   ENDIF

   aRet[ 1 ] := iif( nFieldPos > 0, aStruct[ nFieldPos ][ DBS_TYPE ], Type( cTemp ) )
   aRet[ 2 ] := iif( nFieldPos > 0, aStruct[ nFieldPos ][ DBS_DEC ], nFieldPos )

RETURN aRet

*-----------------------------------------------------------------------------*
PROCEDURE _BrowseNext ( ControlName, ParentForm, z )
*-----------------------------------------------------------------------------*
   LOCAL _Alias, _RecNo, _BrowseHandle, _BrowseArea, _BrowseRecMap, _DeltaScroll
   LOCAL PageLength
   LOCAL i, s

   i := iif( PCount() == 2, GetControlIndex ( ControlName, ParentForm ), z )

   _BrowseHandle := _HMG_aControlHandles[ i ]
   _DeltaScroll := ListView_GetSubItemRect ( _BrowseHandle, 0, 0 )

   _BrowseRecMap := _HMG_aControlRangeMax[ i ]

   PageLength := LISTVIEWGETCOUNTPERPAGE ( _BrowseHandle )

   s := LISTVIEW_GETFIRSTITEM ( _BrowseHandle )

   IF s == PageLength

      IF _HMG_aControlContainerHandle[ i ] != 0
         RETURN
      ENDIF

      _Alias := Alias()
      _BrowseArea := _HMG_aControlSpacing[ i ]
      IF SELECT ( _BrowseArea ) == 0
         RETURN
      ENDIF
      SELECT &_BrowseArea
      _RecNo := RecNo()

      GO _BrowseRecMap[ PageLength ]

      _BrowseUpdate( '', '', i )
      _BrowseVscrollUpdate( i )

      ListView_Scroll( _BrowseHandle, _DeltaScroll[ 2 ] * ( -1 ), 0 )
      ListView_SetCursel ( _BrowseHandle, Len( _HMG_aControlRangeMax[ i ] ) )

      GO _RecNo
      RestoreWorkArea( _Alias )

   ELSE

      ListView_SetCursel ( _BrowseHandle, Len( _BrowseRecMap ) )
      _BrowseVscrollFastUpdate ( i, PageLength - s )

   ENDIF

   _BrowseOnChange ( i )

RETURN

*-----------------------------------------------------------------------------*
PROCEDURE _BrowsePrior ( ControlName, ParentForm, z )
*-----------------------------------------------------------------------------*
   LOCAL _Alias, _RecNo, _BrowseHandle, _BrowseArea, _BrowseRecMap, _DeltaScroll
   LOCAL i

   i := iif( PCount() == 2, GetControlIndex ( ControlName, ParentForm ), z )

   _BrowseHandle := _HMG_aControlHandles[ i ]
   _DeltaScroll := ListView_GetSubItemRect ( _BrowseHandle, 0, 0 )

   _BrowseRecMap := _HMG_aControlRangeMax[ i ]

   IF LISTVIEW_GETFIRSTITEM ( _BrowseHandle ) == 1

      _Alias := Alias()
      _BrowseArea := _HMG_aControlSpacing[ i ]
      IF SELECT ( _BrowseArea ) == 0
         RETURN
      ENDIF
      SELECT &_BrowseArea
      _RecNo := RecNo()

      GO _BrowseRecMap[ 1 ]
      SKIP -LISTVIEWGETCOUNTPERPAGE ( _BrowseHandle ) + 1

      _BrowseVscrollUpdate( i )
      _BrowseUpdate( '', '', i )

      ListView_Scroll( _BrowseHandle, _DeltaScroll[ 2 ] * ( -1 ), 0 )

      GO _RecNo
      RestoreWorkArea( _Alias )

   ELSE

      _BrowseVscrollFastUpdate ( i, 1 - LISTVIEW_GETFIRSTITEM ( _BrowseHandle ) )

   ENDIF

   ListView_SetCursel ( _BrowseHandle, 1 )

   _BrowseOnChange ( i )

RETURN

*-----------------------------------------------------------------------------*
PROCEDURE _BrowseHome ( ControlName, ParentForm, z )
*-----------------------------------------------------------------------------*
   LOCAL _Alias, _RecNo, _BrowseHandle, _BrowseArea, _DeltaScroll
   LOCAL i

   i := iif( PCount() == 2, GetControlIndex ( ControlName, ParentForm ), z )

   _BrowseHandle := _HMG_aControlHandles[ i ]
   _DeltaScroll := ListView_GetSubItemRect ( _BrowseHandle, 0, 0 )

   _Alias := Alias()
   _BrowseArea := _HMG_aControlSpacing[ i ]

   IF SELECT ( _BrowseArea ) == 0
      RETURN
   ENDIF

   SELECT &_BrowseArea

   _RecNo := RecNo()
   GO TOP

   _BrowseVscrollUpdate( i )
   _BrowseUpdate( '', '', i )

   ListView_Scroll( _BrowseHandle, _DeltaScroll[ 2 ] * ( -1 ), 0 )

   GO _RecNo
   RestoreWorkArea( _Alias )

   ListView_SetCursel ( _BrowseHandle, 1 )

   _BrowseOnChange ( i )

RETURN

*-----------------------------------------------------------------------------*
PROCEDURE _BrowseEnd ( ControlName, ParentForm, z )
*-----------------------------------------------------------------------------*
   LOCAL _Alias, _RecNo, _BrowseHandle, _BrowseArea, _DeltaScroll, _BottomRec
   LOCAL i

   i := iif( PCount() == 2, GetControlIndex ( ControlName, ParentForm ), z )

   _BrowseHandle := _HMG_aControlHandles[ i ]
   _DeltaScroll := ListView_GetSubItemRect ( _BrowseHandle, 0, 0 )

   _Alias := Alias()
   _BrowseArea := _HMG_aControlSpacing[ i ]

   IF SELECT ( _BrowseArea ) == 0
      RETURN
   ENDIF

   SELECT &_BrowseArea

   _RecNo := RecNo()
   GO BOTTOM
   _BottomRec := RecNo()

   _BrowseVscrollUpdate( i )
   SKIP -LISTVIEWGETCOUNTPERPAGE ( _BrowseHandle ) + 1

   _BrowseUpdate( '', '', i )
   ListView_Scroll( _BrowseHandle, _DeltaScroll[ 2 ] * ( -1 ), 0 )

   GO _RecNo
   RestoreWorkArea( _Alias )

   ListView_SetCursel ( _BrowseHandle, AScan ( _HMG_aControlRangeMax[ i ], _BottomRec ) )

   _BrowseOnChange ( i )

RETURN

*-----------------------------------------------------------------------------*
PROCEDURE _BrowseUp ( ControlName, ParentForm, z )
*-----------------------------------------------------------------------------*
   LOCAL _Alias, _RecNo, _BrowseHandle, _BrowseArea, _BrowseRecMap, _DeltaScroll
   LOCAL i, s

   i := iif( PCount() == 2, GetControlIndex ( ControlName, ParentForm ), z )

   _BrowseHandle := _HMG_aControlHandles[ i ]
   _DeltaScroll := ListView_GetSubItemRect ( _BrowseHandle, 0, 0 )

   _BrowseRecMap := _HMG_aControlRangeMax[ i ]

   s := LISTVIEW_GETFIRSTITEM ( _BrowseHandle )

   IF s == 1
      _Alias := Alias()
      _BrowseArea := _HMG_aControlSpacing[ i ]

      IF SELECT ( _BrowseArea ) == 0
         RETURN
      ENDIF

      SELECT &_BrowseArea
      _RecNo := RecNo()

      GO _BrowseRecMap[ 1 ]
      SKIP -1

      IF !( _BrowseRecMap[ 1 ] == RecNo() ) // BAA 18-Mar-2012
         _BrowseVscrollUpdate( i )
         _BrowseUpdate( '', '', i )
         ListView_Scroll( _BrowseHandle, _DeltaScroll[ 2 ] * ( -1 ), 0 )
      ENDIF

      GO _RecNo
      RestoreWorkArea( _Alias )

      ListView_SetCursel ( _BrowseHandle, 1 )

   ELSE

      IF _HMG_ActiveDlgProcHandle == 0
         ListView_SetCursel ( _BrowseHandle, s - 1 )
      ENDIF
      _BrowseVscrollFastUpdate ( i, -1 )

   ENDIF

   _BrowseOnChange ( i )

   IF _HMG_ActiveMDIChildIndex > 0 // BAA 15-Apr-2012
      ListView_SetCursel ( _BrowseHandle, s )
   ENDIF

RETURN

*-----------------------------------------------------------------------------*
PROCEDURE _BrowseDown ( ControlName, ParentForm, z )
*-----------------------------------------------------------------------------*
   LOCAL _Alias, _RecNo, _BrowseHandle, _BrowseArea, _BrowseRecMap, _DeltaScroll
   LOCAL PageLength
   LOCAL i, s

   i := iif( PCount() == 2, GetControlIndex ( ControlName, ParentForm ), z )

   _BrowseHandle := _HMG_aControlHandles[ i ]
   _DeltaScroll := ListView_GetSubItemRect ( _BrowseHandle, 0, 0 )

   _BrowseRecMap := _HMG_aControlRangeMax[ i ]

   s := LISTVIEW_GETFIRSTITEM ( _BrowseHandle )

   PageLength := LISTVIEWGETCOUNTPERPAGE ( _BrowseHandle )

   IF s == PageLength

      IF _HMG_aControlContainerHandle[ i ] != 0
         RETURN
      ENDIF

      _Alias := Alias()
      _BrowseArea := _HMG_aControlSpacing[ i ]

      IF SELECT ( _BrowseArea ) == 0
         RETURN
      ENDIF

      SELECT &_BrowseArea
      _RecNo := RecNo()

      GO _BrowseRecMap[ 1 ]
      SKIP

      _BrowseUpdate( '', '', i )
      _BrowseVscrollUpdate( i )

      ListView_Scroll( _BrowseHandle, _DeltaScroll[ 2 ] * ( -1 ), 0 )

      GO _RecNo
      RestoreWorkArea( _Alias )

      ListView_SetCursel ( _BrowseHandle, Len( _HMG_aControlRangeMax[ i ] ) )

   ELSE

      IF _HMG_ActiveDlgProcHandle == 0
         ListView_SetCursel ( _BrowseHandle, s + 1 )
      ENDIF
      _BrowseVscrollFastUpdate ( i, 1 )

   ENDIF

   _BrowseOnChange ( i )

   IF _HMG_ActiveMDIChildIndex > 0 // BAA 15-Apr-2012
      ListView_SetCursel ( _BrowseHandle, s )
   ENDIF

RETURN

*-----------------------------------------------------------------------------*
PROCEDURE _BrowseRefresh ( ControlName , ParentForm , z )
*-----------------------------------------------------------------------------*
   LOCAL _Alias , _RecNo , _BrowseHandle , _BrowseArea , _DeltaScroll
   LOCAL i , s , v

   i := iif( PCount() == 2 , GetControlIndex ( ControlName , ParentForm ) , z )

   v := _BrowseGetValue ( '', '', i )

   _BrowseHandle := _HMG_aControlHandles [i]
   _DeltaScroll := ListView_GetSubItemRect ( _BrowseHandle , 0 , 0 )

   s := LISTVIEW_GETFIRSTITEM ( _BrowseHandle )

   _Alias := Alias()
   _BrowseArea := _HMG_aControlSpacing [i]

   IF Select ( _BrowseArea ) == 0
      ListViewReset ( _BrowseHandle )
      RETURN
   ENDIF

   Select &_BrowseArea
   _RecNo := RecNo()

   IF v <= 0
      v := _RecNo
   ENDIF

   GO v

   IF s == 1 .OR. s == 0

      IF IS_SQLRDD .AND. ! Empty( dbFilter() )
         SKIP
         GO TOP
      ELSE

#ifndef __XHARBOUR__
         IF ( ! Empty( dbFilter() ) .AND. ! Eval( hb_macroBlock( dbFilter() ) ) )
#else
         IF ( ! Empty( dbFilter() ) .AND. ! &( dbFilter() ) )
#endif
            SKIP
         ENDIF
      ENDIF

      IF IndexOrd() != 0
         IF ordKeyVal() == NIL .OR. ! Empty( ordScope() )
            GO TOP
         ENDIF
      ENDIF

      IF SET ( _SET_DELETED )
         IF Deleted()
            GO TOP
         ENDIF
      ENDIF

   ENDIF

   IF EOF()

      ListViewReset ( _BrowseHandle )

      GO _RecNo
      RestoreWorkArea( _Alias )

      RETURN

   ENDIF

   _BrowseVscrollUpdate( i )

   IF s != 0
      SKIP -s + 1
   ENDIF

   _BrowseUpdate( '', '', i )

   ListView_Scroll( _BrowseHandle , _DeltaScroll[2] * ( -1 ) , 0 )
   ListView_SetCursel ( _BrowseHandle , AScan ( _HMG_aControlRangeMax [i] , v ) )

   GO _RecNo
   RestoreWorkArea( _Alias )

RETURN

*-----------------------------------------------------------------------------*
PROCEDURE _BrowseSetValue ( ControlName , ParentForm , Value , z , mp )
*-----------------------------------------------------------------------------*
   LOCAL _Alias , _RecNo , _BrowseHandle , _BrowseArea , _DeltaScroll
   LOCAL i , m

   IF Value <= 0
      RETURN
   ENDIF

   i := iif( z == NIL , GetControlIndex ( ControlName , ParentForm ) , z )

   IF _HMG_ThisEventType == 'BROWSE_ONCHANGE'
      IF i == _HMG_THISINDEX
         MsgMiniGuiError ( "BROWSE: Value property can't be changed inside ONCHANGE event." )
      ENDIF
   ENDIF

   _Alias := Alias()
   _BrowseArea := _HMG_aControlSpacing [i]

   IF Select ( _BrowseArea ) == 0
      RETURN
   ENDIF

   _BrowseHandle := _HMG_aControlHandles [i]

   IF Value == ( _BrowseArea )->( RecCount() ) + 1
      _HMG_aControlValue [i] := Value
      ListViewReset ( _BrowseHandle )
      _BrowseOnChange ( i )
      RETURN
   ENDIF

   IF Value > ( _BrowseArea )->( RecCount() ) + 1
      RETURN
   ENDIF

   m := iif( mp == NIL , Int ( ListViewGetCountPerPage ( _BrowseHandle ) / 2 ) , mp )

   _DeltaScroll := ListView_GetSubItemRect ( _BrowseHandle , 0 , 0 )

   Select &_BrowseArea

   _RecNo := RecNo()

   GO Value

   IF IS_SQLRDD .AND. ! Empty( dbFilter() )
      SKIP
      IF EOF()
         GO _RecNo
         RestoreWorkArea( _Alias )

         RETURN

      ENDIF
   ELSE

#ifndef __XHARBOUR__
      IF ( ! Empty( dbFilter() ) .AND. ! Eval( hb_macroBlock( dbFilter() ) ) )
#else
      IF ( ! Empty( dbFilter() ) .AND. ! &( dbFilter() ) )
#endif
         GO _RecNo
         RestoreWorkArea( _Alias )

         RETURN

      ENDIF
   ENDIF

   IF EOF()

      GO _RecNo
      RestoreWorkArea( _Alias )

      RETURN

   ELSE

      IF PCount() < 5
         _BrowseVscrollUpdate( i )
      ENDIF

      SKIP -m + 1

   ENDIF

   _HMG_aControlValue [i] := Value
   _BrowseUpdate( '' , '' , i )
   GO _RecNo
   RestoreWorkArea( _Alias )

   ListView_Scroll( _BrowseHandle , _DeltaScroll[2] * ( -1 ) , 0 )
   ListView_SetCursel ( _BrowseHandle , AScan ( _HMG_aControlRangeMax [i] , Value ) )

   _HMG_ThisEventType := 'BROWSE_ONCHANGE'
   _BrowseOnChange ( i )
   _HMG_ThisEventType := ''

RETURN

*-----------------------------------------------------------------------------*
FUNCTION _BrowseGetValue ( ControlName , ParentForm , z )
*-----------------------------------------------------------------------------*
   LOCAL _BrowseRecMap , _BrowseArea
   LOCAL i

   i := iif( PCount() == 2 , GetControlIndex ( ControlName , ParentForm ) , z )

   _BrowseArea := _HMG_aControlSpacing [i]

   IF Select ( _BrowseArea ) == 0
      RETURN 0
   ENDIF

   _BrowseRecMap := _HMG_aControlRangeMax [i]

   IF LISTVIEW_GETFIRSTITEM ( _HMG_aControlHandles [i] ) != 0
      RETURN _BrowseRecMap [ LISTVIEW_GETFIRSTITEM ( _HMG_aControlHandles [i] ) ]
   ENDIF

RETURN 0

*-----------------------------------------------------------------------------*
FUNCTION _BrowseDelete (  ControlName , ParentForm , z  )
*-----------------------------------------------------------------------------*
   LOCAL _BrowseRecMap , _Alias , _RecNo , _BrowseArea , lock
   LOCAL Value
   LOCAL i

   i := iif( PCount() == 2 , GetControlIndex ( ControlName , ParentForm ) , z )

   IF LISTVIEW_GETFIRSTITEM ( _HMG_aControlHandles [i] ) == 0
      RETURN NIL
   ENDIF

   _BrowseRecMap := _HMG_aControlRangeMax [i]

   Value := _BrowseRecMap [ LISTVIEW_GETFIRSTITEM ( _HMG_aControlHandles [i] ) ]

   IF Value == 0
      RETURN NIL
   ENDIF

   _Alias := Alias()
   lock := _HMG_aControlInputMask [i]
   _BrowseArea := _HMG_aControlSpacing [i]

   IF Select ( _BrowseArea ) == 0
      RETURN NIL
   ENDIF

   Select &_BrowseArea
   _RecNo := RecNo()

   IF ! lock .AND. ( ( _BrowseArea )->( dbInfo( DBI_SHARED ) ) .OR. IS_SQLRDD )
      lock := .T.
   ENDIF

   GO Value

   IF .NOT. Deleted()

      IF lock

         IF NetDelete()
            dbRUnlock()
            SKIP

            IF EOF()
               GO BOTTOM
            ELSEIF .NOT. SET ( _SET_DELETED )
               SKIP -1
            ENDIF
         ELSE
            MsgStop( _HMG_BRWLangError [9], _HMG_BRWLangMessage [2] )
         ENDIF

      ELSE

         DELETE
         SKIP

         IF EOF()
            GO BOTTOM
         ELSEIF .NOT. SET ( _SET_DELETED )
            SKIP -1
         ENDIF

      ENDIF

      _BrowseSetValue( '' , '' , RecNo() , i , LISTVIEW_GETFIRSTITEM ( _HMG_aControlHandles [i] ) )

   ENDIF

   GO _RecNo
   RestoreWorkArea( _Alias )

RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION _BrowseEdit ( GridHandle, aValid, aValidMessages, aReadOnly, lock, append, inplace, InputItems )
*-----------------------------------------------------------------------------*
   LOCAL BrowseArea, i

#ifdef HMG_LEGACY_ON
   LOCAL actpos := { 0, 0, 0, 0 }, h, GRow, GCol, GWidth, COL, ControlName, j, FormName, Item
   LOCAL Title, aLabels, aInitValues := {}, aFormats := {}, aResults, z, tvar, BackRec, aStru
   LOCAL svar, q, BackArea, TmpNames := {}, NewRec := 0, MixedFields := .F.
   PRIVATE aWhen, aWhenVarNames
#else
   hb_default( @inplace, .T. )
#endif
   IF LISTVIEW_GETFIRSTITEM ( GridHandle ) == 0
      IF ISLOGICAL ( append )
         IF ! append
            RETURN NIL
         ENDIF
      ENDIF
   ENDIF

   i := AScan ( _HMG_aControlHandles, GridHandle )
   BrowseArea := _HMG_aControlSpacing[ i ]

   IF ( BrowseArea )->( dbInfo( DBI_ISDBF ) ) .AND. ( BrowseArea )->( dbInfo( DBI_ISREADONLY ) )
      RETURN NIL
   ENDIF

   IF inplace .OR. _HMG_MainClientMDIHandle != 0 // GF HMG 64
      _BrowseInPlaceEdit ( GridHandle, aValid, aValidMessages, aReadOnly, lock, append, InputItems )

#ifdef HMG_LEGACY_ON
   ELSE

      BackArea := Alias()

      i := AScan ( _HMG_aControlHandles, GridHandle )

      aWhen := _HMG_aControlMiscData1[ i ][ 11 ]

      ControlName := _HMG_aControlNames[ i ]

      j := AScan ( _HMG_aFormHandles, _HMG_aControlParentHandles[ i ] )

      FormName := _HMG_aFormNames[ j ]

      Item := _GetValue ( ControlName, FormName )

      h := GridHandle

      GetWindowRect( h, actpos )

      GRow := actpos[ 2 ]
      GCol := actpos[ 1 ]
      GWidth := actpos[ 3 ] - actpos[ 1 ]

      COL := GCol + ( GWidth - 310 ) / 2

      IF ISLOGICAL ( append )
         Title := _HMG_BRWLangButton[ iif( append, 1, 2 ) ]
      ELSE
         Title := _HMG_BRWLangButton[ 2 ]
      ENDIF

      aLabels := _HMG_aControlCaption[ i ]

      BrowseArea := _HMG_aControlSpacing[ i ]
      SELECT &BrowseArea

      BackRec := RecNo()

      IF ISLOGICAL ( append )
         IF append
            GO BOTTOM
            SKIP
         ELSE
            GO Item
         ENDIF
      ELSE
         GO Item
      ENDIF

      FOR EACH tvar IN _HMG_aControlRangeMin[ i ]

         IF ValType ( &tvar ) == 'C'
            AAdd ( aInitValues, AllTrim( &tvar ) )
         ELSE
            AAdd ( aInitValues, &tvar )
         ENDIF

      NEXT

      FOR z := 1 TO Len ( _HMG_aControlRangeMin[ i ] )

         tvar := Upper ( _HMG_aControlrangeMin[ i ][ z ] )

         q := hb_UAt ( '>', tvar )

         IF q == 0

            SELECT &BrowseArea
            aStru := dbStruct ()

            AAdd ( TmpNames, 'MemVar' + BrowseArea + tvar )

         ELSE

            svar := hb_ULeft ( tvar, q - 2 )
            SELECT &svar
            aStru := dbStruct()

            tvar := hb_URight ( tvar, hb_ULen ( tvar ) - q )

            AAdd ( TmpNames, 'MemVar' + svar + tvar )

            IF Upper( svar ) != Upper( BrowseArea )
               MixedFields := .T.
            ENDIF

         ENDIF

         IF ISLOGICAL ( append )
            IF append
               IF MixedFields
                  MsgMiniGuiError( _HMG_BRWLangError[ 8 ], .F. )
               ENDIF
            ENDIF
         ENDIF

         q := .F.

         FOR EACH Item IN aStru

            IF Item[ DBS_NAME ] == tvar
               q := .T.
               SWITCH Item[ DBS_TYPE ]
               CASE 'N'
                  IF Item[ DBS_DEC ] == 0
                     AAdd ( aFormats, Replicate( '9', Item[ DBS_LEN ] ) )
                  ELSEIF Item[ DBS_DEC ] > 0
                     AAdd ( aFormats, Replicate( '9', ( Item[ DBS_LEN ] - Item[ DBS_DEC ] - 1 ) ) + '.' + Replicate( '9', Item[ DBS_DEC ] ) )
                  ENDIF
                  EXIT
               CASE 'C'
               CASE 'M'
                  AAdd ( aFormats, Item[ DBS_LEN ] )
                  EXIT
               CASE 'D'
               CASE 'L'
                  AAdd ( aFormats, NIL )
                  EXIT
               CASE '+'
                  q := .F.
               END SWITCH
            ENDIF

         NEXT

         IF ! q // field not found, but maybe an expression (readonly hopefully!)
            // force to readonly
            IF aReadOnly == NIL
               aReadonly := Array( Len ( _HMG_aControlRangeMin[ i ] ) )
               AFill( aReadonly, .F. )
               aReadonly[ z ] := .T.
            ELSEIF aReadOnly[ z ] == .F.
               aReadonly[ z ] := .T.
            ENDIF
            // add a length to aFormats
            IF ISCHARACTER ( aInitValues[ z ] )
               AAdd ( aFormats, Max ( 1, Len( aInitValues[ z ] ) ) )
            ELSEIF ISNUMERIC ( aInitValues[ z ] )
               aInitValues[ z ] := Str( aInitValues[ z ] ) // type conversion doesn't matter, field should be readonly
               AAdd ( aFormats, Max ( 1, Len( aInitValues[ z ] ) ) )
            ELSE
               AAdd ( aFormats, NIL )
            ENDIF
         ENDIF

      NEXT z

      aWhenVarNames := tmpnames

      SELECT &BrowseArea

      IF lock
         IF ! NetRecLock()
            MsgAlert ( _HMG_BRWLangError[ 9 ], _HMG_BRWLangError[ 10 ] )
            GO BackRec
            RestoreWorkArea( BackArea )
            RETURN NIL
         ENDIF
      ENDIF

      aResults := _EditRecord ( Title, aLabels, aInitValues, aFormats, GRow, COL, aValid, TmpNames, aValidMessages, aReadOnly, actpos[ 4 ] - actpos[ 2 ] )

      IF ValType ( aResults[ 1 ] ) != 'U'

         IF ISLOGICAL ( append )
            IF append
               APPEND BLANK
               NewRec := RecNo()
            ENDIF
         ENDIF

         IF lock
            NetRecLock()
         ENDIF

         FOR z := 1 TO Len ( aResults )

            IF aReadOnly == NIL .OR. aReadOnly[ z ] == .F.

               tvar := _HMG_aControlRangeMin[ i ][ z ]
               REPLACE &tvar WITH aResults[ z ]

            ENDIF

         NEXT z

         _BrowseRefresh ( '', '', i )

      ENDIF

      IF lock
         dbRUnlock()
      ENDIF

      GO BackRec
      RestoreWorkArea( BackArea )

      _SetFocus( ControlName, FormName )

      IF ISLOGICAL ( append )
         IF append
            IF NewRec != 0
               _SetValue ( ControlName, FormName, NewRec )
            ENDIF
         ENDIF
      ENDIF

#endif
   ENDIF

RETURN NIL

#ifdef HMG_LEGACY_ON
*-----------------------------------------------------------------------------*
FUNCTION _EditRecord ( Title, aLabels, aValues, aFormats, row, col, aValid, TmpNames, aValidMessages, aReadOnly, h )
*-----------------------------------------------------------------------------*
   LOCAL i, ControlRow, e := 0, LN, CN, ControlFocused := 'Control_1', th, lFirstEnabledControl := .T.

   PRIVATE l := Len ( aLabels )
   PRIVATE aResult[ l ]

   FOR i := 1 TO l

      IF ValType ( aValues[ i ] ) == 'C'

         IF ValType ( aFormats[ i ] ) == 'N'

            IF aFormats[ i ] > 32
               e++
            ENDIF

         ENDIF

      ENDIF

      IF ValType ( aValues[ i ] ) == 'M'
         e++
      ENDIF

   NEXT i

   th := ( l * 30 ) + ( e * 60 ) + 30

   IF TH < H
      TH := H + 1
   ENDIF

   DEFINE WINDOW _EditRecord ;
         AT row, col ;
         WIDTH 310 ;
         HEIGHT h - 19 + GetTitleHeight() ;
         TITLE Title ;
         MODAL NOSIZE ;
         ON INIT _SetFocus ( ControlFocused, '_Split_1' )

      ON KEY ALT + O ACTION _EditRecordOk ( aValid, TmpNames, aValidMessages )
      ON KEY ALT + C ACTION _EditRecordCancel()

      DEFINE SPLITBOX

         DEFINE WINDOW _Split_1 ;
               WIDTH 310 ;
               HEIGHT H - 90 ;
               VIRTUAL HEIGHT TH ;
               SPLITCHILD NOCAPTION FONT 'Arial' SIZE 10 BREAK FOCUSED

            ON KEY ALT + O ACTION _EditRecordOk ( aValid, TmpNames, aValidMessages )
            ON KEY ALT + C ACTION _EditRecordCancel()

            ControlRow := 10

            FOR i := 1 TO l

               LN := 'Label_' + hb_ntos( i )
               CN := 'Control_' + hb_ntos( i )

               @ ControlRow, 10 LABEL &LN OF _Split_1 VALUE aLabels[ i ] WIDTH 90

               SWITCH ValType ( aValues[ i ] )
               CASE 'L'

                  @ ControlRow, 120 CHECKBOX &CN OF _Split_1 CAPTION '' VALUE aValues[ i ] ;
                     ON LOSTFOCUS _WHENEVAL()
                  ControlRow += 30
                  EXIT
               CASE 'D'

                  @ ControlRow, 120 DATEPICKER &CN OF _Split_1 VALUE aValues[ i ] WIDTH 140 ;
                     ON LOSTFOCUS _WHENEVAL() ;
                     SHOWNONE
                  ControlRow += 30
                  EXIT
               CASE 'N'

                  IF ValType ( aFormats[ i ] ) == 'A'
                     @ ControlRow, 120 COMBOBOX &CN OF _Split_1 ITEMS aFormats[ i ] VALUE aValues[ i ] WIDTH 140 FONT 'Arial' SIZE 10 ;
                        ON GOTFOCUS ( LN := _Split_1.FocusedControl, ;
                        SendMessage ( GetControlHandle( LN, '_Split_1' ), EM_SETSEL, 0, -1 ) ) ;
                        ON LOSTFOCUS _WHENEVAL()

                  ELSEIF ValType ( aFormats[ i ] ) == 'C'

                     @ ControlRow, 120 GETBOX &CN OF _Split_1 VALUE aValues[ i ] WIDTH 140 FONT 'Arial' SIZE 10 PICTURE aFormats[ i ] ;
                        ON GOTFOCUS ( LN := _Split_1.FocusedControl, ;
                        SendMessage ( GetControlHandle( LN, '_Split_1' ), EM_SETSEL, 0, -1 ) ) ;
                        ON LOSTFOCUS _WHENEVAL()
                  ELSE
                     @ ControlRow, 120 GETBOX &CN OF _Split_1 VALUE aValues[ i ] WIDTH 140 FONT 'Arial' SIZE 10 ;
                        ON GOTFOCUS ( LN := _Split_1.FocusedControl, ;
                        SendMessage ( GetControlHandle( LN, '_Split_1' ), EM_SETSEL, 0, -1 ) ) ;
                        ON LOSTFOCUS _WHENEVAL()
                  ENDIF
                  ControlRow += 30
                  EXIT
               CASE 'C'

                  IF ValType ( aFormats[ i ] ) == 'N'
                     IF aFormats[ i ] <= 32
                        @ ControlRow, 120 GETBOX &CN OF _Split_1 VALUE aValues[ i ] WIDTH 140 FONT 'Arial' SIZE 10 PICTURE Replicate( "X", aFormats[ i ] ) ;
                           ON GOTFOCUS ( LN := _Split_1.FocusedControl, ;
                           SendMessage ( GetControlHandle( LN, '_Split_1' ), EM_SETSEL, 0, -1 ) ) ;
                           ON LOSTFOCUS _WHENEVAL()
                        ControlRow += 30
                     ELSE
                        _Split_1.&( LN ).HEIGHT := 90
                        @ ControlRow, 120 EDITBOX &CN OF _Split_1 WIDTH 140 HEIGHT 90 VALUE aValues[ i ] FONT 'Arial' SIZE 10 MAXLENGTH aFormats[ i ] ;
                           ON LOSTFOCUS _WHENEVAL()
                        ControlRow += 94
                     ENDIF
                  ENDIF
                  EXIT
               CASE 'M'

                  @ ControlRow, 120 EDITBOX &CN OF _Split_1 WIDTH 140 HEIGHT 90 VALUE aValues[ i ] FONT 'Arial' SIZE 10 ON LOSTFOCUS _WHENEVAL()
                  ControlRow += 94

               ENDSWITCH

               IF aReadOnly != NIL
                  IF aReadOnly[ i ]
                     _DisableControl ( CN, '_Split_1' )
                  ELSEIF lFirstEnabledControl
                     lFirstEnabledControl := .F.
                     ControlFocused := CN
                  ENDIF
               ENDIF

            NEXT i

         END WINDOW

         DEFINE WINDOW _Split_2 ;
               WIDTH 300 ;
               HEIGHT 50 ;
               SPLITCHILD NOCAPTION FONT 'Arial' SIZE 10 BREAK

            @ 10, 40 BUTTON BUTTON_1 ;
               OF _Split_2 ;
               CAPTION _HMG_BRWLangButton[ 4 ] ;
               ACTION _EditRecordOk ( aValid, TmpNames, aValidMessages )

            @ 10, 150 BUTTON BUTTON_2 ;
               OF _Split_2 ;
               CAPTION _HMG_BRWLangButton[ 3 ] ;
               ACTION _EditRecordCancel()

         END WINDOW

      END SPLITBOX

   END WINDOW

   ACTIVATE WINDOW _EditRecord

RETURN ( aResult )

*-----------------------------------------------------------------------------*
STATIC PROCEDURE _WHENEVAL ()
*-----------------------------------------------------------------------------*
   LOCAL ControlName
   LOCAL mVar
   LOCAL Result
   LOCAL i, x

   IF ValType ( aWhen ) == 'A'

      IF Len ( aWhen ) >= l

         FOR x := 1 TO l

            IF ValType ( aWhen[ x ] ) != 'U'

               ControlName := 'Control_' + AllTrim ( Str ( x ) )
               Result := _GetValue ( ControlName, '_Split_1' )

               mVar := aWhenVarNames[ x ]
               &mVar := Result

            ENDIF

         NEXT x


         FOR i := 1 TO l

            IF ValType ( aWhen[ i ] ) == 'B'

               ControlName := 'Control_' + AllTrim ( Str ( i ) )

               IF Eval ( aWhen[ i ] )
                  _EnableControl ( ControlName, '_Split_1' )
               ELSE
                  _DisableControl ( ControlName, '_Split_1' )
               ENDIF

            ENDIF

         NEXT i

      ENDIF

   ENDIF

RETURN
*-----------------------------------------------------------------------------*
STATIC FUNCTION _EditRecordOk ( aValid, TmpNames, aValidMessages )
*-----------------------------------------------------------------------------*
   LOCAL ControlName
   LOCAL mVar
   LOCAL i, l

   l := Len ( aResult )

   FOR i := 1 TO l

      ControlName := 'Control_' + AllTrim ( Str ( i ) )
      aResult[ i ] := _GetValue ( ControlName, '_Split_1' )

      IF ValType ( aValid ) != 'U'

         mVar := TmpNames[ i ]
         &mVar := aResult[ i ]

      ENDIF

   NEXT i

   IF ValType ( aValid ) == 'A'

      FOR i := 1 TO l

         IF ValType ( aValid[ i ] ) == 'B'

            IF ! Eval ( aValid[ i ] )

               IF ValType ( aValidMessages ) == 'A'

                  IF ValType ( aValidMessages[ i ] ) != 'U'

                     MsgAlert ( aValidMessages[ i ], _HMG_BRWLangError[ 10 ] )

                  ELSE

                     MsgAlert ( _HMG_BRWLangError[ 11 ], _HMG_BRWLangError[ 10 ] )

                  ENDIF

               ELSE

                  MsgAlert ( _HMG_BRWLangError[ 11 ], _HMG_BRWLangError[ 10 ] )

               ENDIF

               _SetFocus ( 'Control_' + hb_ntos( i ), '_Split_1' )

               RETURN NIL

            ENDIF

         ENDIF

      NEXT i

   ENDIF

   RELEASE WINDOW _EditRecord

RETURN NIL
*-----------------------------------------------------------------------------*
STATIC FUNCTION _EditRecordCancel ()
*-----------------------------------------------------------------------------*

   AEval( aResult, {| x, i | HB_SYMBOL_UNUSED( x ), aResult[ i ] := NIL } )

   RELEASE WINDOW _EditRecord

RETURN NIL

#endif

*-----------------------------------------------------------------------------*
STATIC FUNCTION _BrowseInPlaceEdit ( GridHandle, aValid, aValidMessages, aReadOnly, lock, append, aInputItems )
*-----------------------------------------------------------------------------*
   LOCAL GridCol, GridRow, i, nrec, _GridWorkArea, BackArea, BackRec
   LOCAL _GridFields, FieldName, CellData := '', CellColIndex
   LOCAL aStruct, width, decimals, sFieldname, ControlType
   LOCAL Ldelta := 0, aTemp, E, r, p, lInputItems := .F., aItems := {}, aValues := {}
   LOCAL aEnabledTypes := { "N", "C", "D", "L", "M" }
   LOCAL aInputMask

   IF _HMG_ThisEventType == 'BROWSE_WHEN'
      MsgMiniGuiError( "BROWSE: Editing within WHEN event procedure is not allowed." )

   ELSEIF _HMG_ThisEventType == 'BROWSE_VALID'
      MsgMiniGuiError( "BROWSE: Editing within VALID event procedure is not allowed." )
   ENDIF

   IF append
      i := AScan ( _HMG_aControlHandles, GridHandle )
      _BrowseInPlaceAppend ( '', '', i )
      RETURN NIL
   ENDIF

   IF This.CellRowIndex != LISTVIEW_GETFIRSTITEM ( GridHandle )
      RETURN NIL
   ENDIF

   i := AScan ( _HMG_aControlhandles, GridHandle )

   _GridWorkArea := _HMG_aControlSpacing[ i ]

   _GridFields := _HMG_aControlRangeMin[ i ]

   CellColIndex := This.CellColIndex

   IF CellColIndex < 1 .OR. CellColIndex > Len ( _GridFields )
      RETURN NIL
   ENDIF

   IF Len ( _HMG_aControlBkColor[ i ] ) > 0 .AND. CellColIndex == 1
      PlayHand()
      RETURN NIL
   ENDIF

   aInputMask := _HMG_aControlMiscData1[ i ][ 22 ]

   IF ISARRAY ( aInputItems )

      IF Len ( aInputItems ) >= CellColIndex

         IF ISARRAY ( aInputItems[ CellColIndex ] )
            lInputItems := .T.
         ENDIF

      ENDIF

   ENDIF

   IF ISARRAY ( aReadOnly )

      IF Len ( aReadOnly ) >= CellColIndex

         IF aReadOnly[ CellColIndex ] != NIL

            IF aReadOnly[ CellColIndex ]
               _HMG_IPE_CANCELLED := .F.
               RETURN NIL
            ENDIF

         ENDIF

      ENDIF

   ENDIF

   FieldName := _GridFields[ CellColIndex ]

   IF AScan( aEnabledTypes, ( _GridWorkArea )->( _TypeEx( FieldName )[ 1 ] ) ) < 1
      MsgAlert ( "Edit of this field is not supported.", _HMG_BRWLangError[ 10 ] )
      RETURN NIL
   ENDIF

   IF ( r := hb_UAt ( '>', FieldName ) ) != 0
      sFieldName := Right ( FieldName, Len( Fieldname ) - r )
      p := Left ( FieldName, r - 2 )
      IF !( Upper( p ) == "FIELD" ) .AND. !( Upper( p ) == "_FIELD" )
         _GridWorkArea := p
      ENDIF
   ELSE
      sFieldName := FieldName
   ENDIF

   // If the specified area does not exists, when return
   IF SELECT ( _GridWorkArea ) == 0
      RETURN NIL
   ENDIF

   // Save Original WorkArea
   BackArea := Alias()
   // Selects Grid's WorkArea
   SELECT &_GridWorkArea
   // Save Original Record Pointer
   BackRec := RecNo()

   IF _GridWorkArea == _HMG_aControlSpacing[ i ]
      nRec := _GetValue ( '', '', i )
      GO nRec
   ENDIF

   IF ! lock .AND. ( ( _GridWorkArea )->( dbInfo( DBI_SHARED ) ) .OR. IS_SQLRDD )
      lock := .T.
   ENDIF

   // If LOCK clause is present, try to lock.
   IF lock

      IF ! ( _GridWorkArea )->( NetRecLock() )
         _HMG_IPE_CANCELLED := .T.
         MsgAlert( _HMG_BRWLangError[ 9 ], _HMG_BRWLangError[ 10 ] )
         // Restore Original Record Pointer
         GO BackRec
         // Restore Original WorkArea
         RestoreWorkArea( BackArea )
         RETURN NIL
      ENDIF

   ENDIF

   aTemp := _HMG_aControlMiscData1[ i ][ 11 ]

   IF ISARRAY ( aTemp )

      IF Len ( aTemp ) >= Len ( _GridFields )

         IF ISBLOCK ( aTemp[ CellColIndex ] )
            _HMG_ThisEventType := 'BROWSE_WHEN'
            E := Eval ( aTemp[ CellColIndex ] )
            _HMG_ThisEventType := ''
            IF ISLOGICAL( E ) .AND. ! E
               PlayHand()
               // Restore Original Record Pointer
               GO BackRec
               // Restore Original WorkArea
               RestoreWorkArea( BackArea )
               _HMG_IPE_CANCELLED := .F.
               RETURN NIL
            ENDIF
            IF Alias() <> _GridWorkArea
               SELECT &_GridWorkArea
            ENDIF
            IF ISNUMBER ( nRec ) .AND. RecNo() <> nRec
               GO nRec
            ENDIF
            IF GetControlHandle ( _GetFocusedControl ( ( r := GetParentFormName( i ) ) ), r ) <> GridHandle
               SetFocus ( GridHandle )
            ENDIF
         ENDIF

      ENDIF

   ENDIF

   CellData := &FieldName

   aStruct := dbStruct()

   IF ( r := FieldPos ( sFieldName ) ) > 0
      width := aStruct[ r ][ DBS_LEN ]
      decimals := aStruct[ r ][ DBS_DEC ]
   ENDIF

   GridRow := GetWindowRow ( GridHandle )
   GridCol := GetWindowCol ( GridHandle )

   IF lInputItems

      ControlType := 'X'
      Ldelta := 1

   ELSE

      p := Type ( FieldName )
      SWITCH p

      CASE 'C'
      CASE 'D'
      CASE 'M'
         ControlType := p
         EXIT
      CASE 'L'
         ControlType := p
         Ldelta := 1
         EXIT
      CASE 'N'
         ControlType := IFEMPTY( decimals, 'I', 'F' )

      END SWITCH

   ENDIF

   _HMG_InplaceParentHandle := iif( _HMG_BeginWindowMDIActive, GetActiveMdiHandle(), GetActiveWindow() )

   IF ControlType == 'M'

      r := InputBox ( '', _HMG_aControlCaption[ i ][ CellColIndex ], StrTran( CellData, Chr( 141 ), ' ' ), , , .T. )

      IF _HMG_DialogCancelled
         _HMG_IPE_CANCELLED := .T.
      ELSE
         REPLACE &FieldName WITH r
         _HMG_IPE_CANCELLED := .F.
      ENDIF

      IF lock
         ( _GridWorkArea )->( dbCommit() )
         ( _GridWorkArea )->( dbRUnlock() )
      ENDIF

   ELSE

      DEFINE WINDOW _InPlaceEdit ;
            AT This.CellRow + GridRow - _HMG_aControlRow[ i ] - 1, This.CellCol + GridCol - _HMG_aControlCol[ i ] + 2 ;
            WIDTH This.CellWidth ;
            HEIGHT This.CellHeight + 6 + Ldelta ;
            MODAL ;
            NOCAPTION ;
            NOSIZE

         ON KEY CONTROL + U ACTION iif( _IsWindowActive( '_InPlaceEdit' ), ;
            _InPlaceEdit.Control_1.value := iif( ControlType == 'L', iif ( CellData, 1, 2 ), CellData ), NIL )
         ON KEY RETURN ACTION iif( _IsWindowActive( '_InPlaceEdit' ), ;
            _InPlaceEditOk ( i, _InPlaceEdit.Control_1.value, aValid, CellColIndex, ;
            sFieldName, _GridWorkArea, aValidMessages, lock, ControlType, aInputItems ), NIL )
         ON KEY ESCAPE ACTION ( _HMG_IPE_CANCELLED := .T., iif( lock, dbUnlock(), NIL ), ;
            iif( _IsWindowActive( '_InPlaceEdit' ), _InPlaceEdit.RELEASE, NIL ) )

         IF lInputItems

            // Fill Items Array
            AEval( aInputItems[ CellColIndex ], {| p | AAdd ( aItems, p[ 1 ] ) } )
            // Fill Values Array
            AEval( aInputItems[ CellColIndex ], {| p | AAdd ( aValues, p[ 2 ] ) } )

            r := AScan ( aValues, CellData )

            DEFINE COMBOBOX Control_1
               ROW 0
               COL 0
               ITEMS aItems
               WIDTH This.CellWidth
               VALUE iif ( Empty( r ), 1, r )
               FONTNAME _hmg_aControlFontName[ i ]
               FONTSIZE _hmg_aControlFontSize[ i ]
            END COMBOBOX

         ELSEIF ControlType == 'C'

            CellData := RTrim ( CellData )

            DEFINE TEXTBOX Control_1
               ROW 0
               COL 0
               WIDTH This.CellWidth
               HEIGHT This.CellHeight + 6
               VALUE CellData
               MAXLENGTH width
               IF ISARRAY ( AINPUTMASK )
                  IF Len ( AINPUTMASK ) >= CellColIndex
                     IF ValType ( AINPUTMASK[ CellColIndex ] ) == 'C' .AND. ! Empty ( AINPUTMASK[ CellColIndex ] )
                        INPUTMASK AINPUTMASK[ CellColIndex ]
                     ENDIF
                  ENDIF
               ENDIF
               FONTNAME _hmg_aControlFontName[ i ]
               FONTSIZE _hmg_aControlFontSize[ i ]
            END TEXTBOX

         ELSEIF ControlType == 'D'

            DEFINE DATEPICKER Control_1
               ROW 0
               COL 0
               HEIGHT This.CellHeight + 6
               WIDTH This.CellWidth
               VALUE CellData
               UPDOWN .T.
               SHOWNONE .T.
               FONTNAME _hmg_aControlFontName[ i ]
               FONTSIZE _hmg_aControlFontSize[ i ]
            END DATEPICKER

         ELSEIF ControlType == 'L'

            DEFINE COMBOBOX Control_1
               ROW 0
               COL 0
               ITEMS { '.T.', '.F.' }
               WIDTH This.CellWidth
               VALUE iif ( CellData, 1, 2 )
               FONTNAME _hmg_aControlFontName[ i ]
               FONTSIZE _hmg_aControlFontSize[ i ]
            END COMBOBOX

         ELSEIF ControlType == 'I'

            DEFINE TEXTBOX Control_1
               ROW 0
               COL 0
               NUMERIC .T.
               WIDTH This.CellWidth
               HEIGHT This.CellHeight + 6
               VALUE CellData
               IF ISARRAY ( AINPUTMASK )
                  IF Len ( AINPUTMASK ) >= CellColIndex
                     IF ValType ( AINPUTMASK[ CellColIndex ] ) == 'C' .AND. ! Empty ( AINPUTMASK[ CellColIndex ] )
                        INPUTMASK AINPUTMASK[ CellColIndex ]
                     ELSE
                        MAXLENGTH width
                     ENDIF
                  ELSE
                     MAXLENGTH width
                  ENDIF
               ELSE
                  MAXLENGTH width
               ENDIF
               FONTNAME _hmg_aControlFontName[ i ]
               FONTSIZE _hmg_aControlFontSize[ i ]
            END TEXTBOX

         ELSEIF ControlType == 'F'

            DEFINE TEXTBOX Control_1
               ROW 0
               COL 0
               NUMERIC .T.
               IF ISARRAY ( AINPUTMASK )
                  IF Len ( AINPUTMASK ) >= CellColIndex
                     IF ValType ( AINPUTMASK[ CellColIndex ] ) == 'C' .AND. ! Empty ( AINPUTMASK[ CellColIndex ] )
                        INPUTMASK AINPUTMASK[ CellColIndex ]
                     ELSE
                        INPUTMASK Replicate ( '9', width - decimals - 1 ) + '.' + Replicate ( '9', Decimals )
                     ENDIF
                  ELSE
                     INPUTMASK Replicate ( '9', width - decimals - 1 ) + '.' + Replicate ( '9', Decimals )
                  ENDIF
               ELSE
                  INPUTMASK Replicate ( '9', width - decimals - 1 ) + '.' + Replicate ( '9', Decimals )
               ENDIF
               WIDTH This.CellWidth
               HEIGHT This.CellHeight + 6
               VALUE CellData
               FONTNAME _hmg_aControlFontName[ i ]
               FONTSIZE _hmg_aControlFontSize[ i ]
            END TEXTBOX

         ENDIF

      END WINDOW

      _SetFocus ( 'Control_1', '_InPlaceEdit' )

      ACTIVATE WINDOW _InPlaceEdit

   ENDIF

   _MdiWindowsActivate ( _HMG_InplaceParentHandle ) // GF HMG 47

   _HMG_InplaceParentHandle := 0

   SetFocus ( GridHandle )

   // Restore Original Record Pointer
   GO BackRec
   // Restore Original WorkArea
   RestoreWorkArea( BackArea )

RETURN NIL

*-----------------------------------------------------------------------------*
STATIC PROCEDURE _InPlaceEditOk ( i, r, aValid, CellColIndex, sFieldName, AreaName, aValidMessages, lock, ControlType, aInputItems )
*-----------------------------------------------------------------------------*
   LOCAL mVar
   LOCAL TmpName
   LOCAL b
   LOCAL Result

   IF ControlType == 'X' .OR. ControlType == 'L'

      IF SendMessage ( GetControlHandle ( 'Control_1', '_InPlaceEdit' ), CB_GETDROPPEDSTATE, 0, 0 ) == 1

         SendMessage ( GetControlHandle ( 'Control_1', '_InPlaceEdit' ), CB_SHOWDROPDOWN, 0, 0 )
         InsertReturn()
         RETURN

      ENDIF

   ENDIF

   IF ISARRAY ( aValid )

      IF Len ( aValid ) >= CellColIndex

         IF aValid[ CellColIndex ] != NIL

            Result := _GetValue ( 'Control_1', '_InPlaceEdit' )

            IF ControlType == 'L'
               Result := ( Result == 1 )
            ELSEIF ControlType == 'X'
               Result := aInputItems[ CellColIndex ][ r ][ 2 ]
            ENDIF

            TmpName := 'MemVar' + AreaName + sFieldname
            mVar := TmpName
            &mVar := Result

            _HMG_ThisEventType := 'BROWSE_VALID'

            b := Eval ( aValid[ CellColIndex ] )

            _HMG_ThisEventType := ''

            IF ISLOGICAL( b ) .AND. ! b

               IF ISARRAY ( aValidMessages )

                  IF Len ( aValidMessages ) >= CellColIndex

                     IF aValidMessages[ CellColIndex ] != NIL

                        IF ISCHARACTER ( aValidMessages[ CellColIndex ] )

                           MsgAlert ( aValidMessages[ CellColIndex ], _HMG_BRWLangError[ 10 ] )

                        ELSEIF ISBLOCK ( aValidMessages[ CellColIndex ] )

                           Eval ( aValidMessages[ CellColIndex ], Result )

                        ENDIF

                     ELSE

                        MsgAlert ( _HMG_BRWLangError[ 11 ], _HMG_BRWLangError[ 10 ] )

                     ENDIF

                  ELSE

                     MsgAlert ( _HMG_BRWLangError[ 11 ], _HMG_BRWLangError[ 10 ] )

                  ENDIF

               ELSE

                  MsgAlert ( _HMG_BRWLangError[ 11 ], _HMG_BRWLangError[ 10 ] )

               ENDIF

            ELSE

               _InPlaceEditSave ( i, sFieldname, AreaName, r, lock, ControlType, aInputItems, CellColIndex )

            ENDIF

         ELSE

            _InPlaceEditSave ( i, sFieldname, AreaName, r, lock, ControlType, aInputItems, CellColIndex )

         ENDIF

      ENDIF

   ELSE

      _InPlaceEditSave ( i, sFieldname, AreaName, r, lock, ControlType, aInputItems, CellColIndex )

   ENDIF

   _HMG_IPE_CANCELLED := ( lock .AND. NetError() )

RETURN

*-----------------------------------------------------------------------------*
STATIC PROCEDURE _InPlaceEditSave ( i, FieldName, Alias, r, lock, ControlType, aInputItems, CellColIndex )
*-----------------------------------------------------------------------------*

   IF lock

      IF !( Alias )->( NetRecLock() )
         MsgAlert ( _HMG_BRWLangError[ 9 ], _HMG_BRWLangError[ 10 ] )
         RETURN
      ENDIF

   ENDIF

   IF ISNUMERIC( r )

      IF ControlType == 'L'
         r := ( r == 1 )
      ELSEIF ControlType == 'X'
         r := aInputItems[ CellColIndex ][ r ][ 2 ]
      ENDIF

   ENDIF

   FieldName := Alias + "->" + FieldName
   REPLACE &FieldName WITH r

   IF lock
      IF IS_SQLRDD
         ( Alias )->( dbCommit() )
      ENDIF
      ( Alias )->( dbRUnlock() )
   ENDIF

   _BrowseRefresh ( '', '', i )

   _InPlaceEdit.RELEASE

RETURN

*-----------------------------------------------------------------------------*
PROCEDURE ProcessInPlaceKbdEdit( i )
*-----------------------------------------------------------------------------*
   LOCAL IPE_MAXCOL
   LOCAL TmpRow
   LOCAL r, xs, xd

   IF LISTVIEW_GETFIRSTITEM ( _HMG_aControlHandles[ i ] ) == 0
      RETURN
   ENDIF

   IPE_MAXCOL := Len ( _HMG_aControlRangeMin[ i ] )

   DO WHILE .T.

      TmpRow := LISTVIEW_GETFIRSTITEM ( _HMG_aControlHandles[ i ] )

      IF TmpRow != _HMG_IPE_ROW

         _HMG_IPE_ROW := TmpRow
         _HMG_IPE_COL := iif( Len ( _HMG_aControlBkColor[ i ] ) > 0, 2, 1 )

      ENDIF

      _HMG_ThisItemRowIndex := _HMG_IPE_ROW
      _HMG_ThisItemColIndex := _HMG_IPE_COL

      IF _HMG_IPE_COL == 1
         r := LISTVIEW_GETITEMRECT ( _HMG_aControlHandles[ i ], _HMG_IPE_ROW - 1 )
      ELSE
         r := LISTVIEW_GETSUBITEMRECT ( _HMG_aControlHandles[ i ], _HMG_IPE_ROW - 1, _HMG_IPE_COL - 1 )
      ENDIF

      xs := ( _HMG_aControlCol[ i ] + r[ 2 ] + r[ 3 ] ) - ( _HMG_aControlCol[ i ] + _HMG_aControlWidth[ i ] )

      xd := 20

      IF xs > -xd
         ListView_Scroll( _HMG_aControlHandles[ i ], xs + xd, 0 )
      ELSE

         IF r[ 2 ] < 0
            ListView_Scroll( _HMG_aControlHandles[ i ], r[ 2 ], 0 )
         ENDIF

      ENDIF

      IF _HMG_IPE_COL == 1
         r := LISTVIEW_GETITEMRECT ( _HMG_aControlHandles[ i ], _HMG_IPE_ROW - 1 )
      ELSE
         r := LISTVIEW_GETSUBITEMRECT ( _HMG_aControlHandles[ i ], _HMG_IPE_ROW - 1, _HMG_IPE_COL - 1 )
      ENDIF

      _HMG_ThisItemCellRow := _HMG_aControlRow[ i ] + r[ 1 ]
      _HMG_ThisItemCellCol := _HMG_aControlCol[ i ] + r[ 2 ]
      _HMG_ThisItemCellWidth := r[ 3 ]
      _HMG_ThisItemCellHeight := r[ 4 ]

      _BrowseEdit ( _hmg_acontrolhandles[ i ], _HMG_acontrolmiscdata1[ i ][ 4 ], _HMG_acontrolmiscdata1[ i ][ 5 ], _HMG_acontrolmiscdata1[ i ][ 3 ], _HMG_aControlInputMask[ i ], .F., _HMG_aControlFontColor[ i ], _HMG_acontrolmiscdata1[ i ][ 13 ] )

      _HMG_ThisIndex := 0
      _HMG_ThisType := ''

      _HMG_ThisItemRowIndex := 0
      _HMG_ThisItemColIndex := 0
      _HMG_ThisItemCellRow := 0
      _HMG_ThisItemCellCol := 0
      _HMG_ThisItemCellWidth := 0
      _HMG_ThisItemCellHeight := 0

      IF _HMG_IPE_CANCELLED

         IF _HMG_IPE_COL == IPE_MAXCOL

            _HMG_IPE_COL := iif( Len ( _HMG_aControlBkColor[ i ] ) > 0, 2, 1 )

            ListView_Scroll( _HMG_aControlHandles[ i ], -10000, 0 )

         ENDIF

         EXIT

      ELSE

         _HMG_IPE_COL++

         IF _HMG_IPE_COL > IPE_MAXCOL

            _HMG_IPE_COL := iif( Len ( _HMG_aControlBkColor[ i ] ) > 0, 2, 1 )

            ListView_Scroll( _HMG_aControlHandles[ i ], -10000, 0 )

            EXIT

         ENDIF

      ENDIF

   ENDDO

RETURN

*-----------------------------------------------------------------------------*
STATIC PROCEDURE _BrowseSync ( i )
*-----------------------------------------------------------------------------*
   LOCAL _Alias, _BrowseArea, _RecNo, _CurrentValue

   _Alias := Alias()
   _BrowseArea := _HMG_aControlSpacing[ i ]
   IF SELECT ( _BrowseArea ) == 0
      RETURN
   ENDIF
   SELECT &_BrowseArea
   _RecNo := RecNo()

   _CurrentValue := _BrowseGetValue ( '', '', i )

   IF _RecNo != _CurrentValue
      GO _CurrentValue
   ENDIF

   RestoreWorkArea( _Alias )

RETURN

*-----------------------------------------------------------------------------*
PROCEDURE _BrowseOnChange ( i )
*-----------------------------------------------------------------------------*

   IF _HMG_BrowseSyncStatus
      _BrowseSync ( i )
   ENDIF

   _DoControlEventProcedure ( _HMG_aControlChangeProcedure[ i ], i )

RETURN

*-----------------------------------------------------------------------------*
STATIC PROCEDURE _BrowseInPlaceAppend ( ControlName, ParentForm, z )
*-----------------------------------------------------------------------------*
   LOCAL _Alias, _BrowseArea, _RecNo, _NewRec
   LOCAL aTemp
   LOCAL i

   i := iif( PCount() == 2, GetControlIndex ( ControlName, ParentForm ), z )

   _Alias := Alias()
   _BrowseArea := _HMG_aControlSpacing[ i ]

   IF SELECT ( _BrowseArea ) == 0
      RETURN
   ENDIF

   SELECT &_BrowseArea

   _RecNo := RecNo()
   GO BOTTOM

   _NewRec := RecCount() + 1

   IF LISTVIEWGETITEMCOUNT( _HMG_aControlhandles[ i ] ) != 0
      _BrowseVscrollUpdate( i )
      SKIP -LISTVIEWGETCOUNTPERPAGE ( _HMG_aControlhandles[ i ] ) + 2
      _BrowseUpdate( '', '', i )
   ENDIF

   NetAppend()

   GO _RecNo
   RestoreWorkArea( _Alias )

   IF ! NetError()
      aTemp := Array ( Len ( _HMG_aControlRangeMin[ i ] ) )
      AFill ( aTemp, '' )
      AAdd ( _HMG_aControlRangeMax[ i ], _NewRec )

      AddListViewItems ( _HMG_aControlHandles[ i ], aTemp, 0 )

      ListView_SetCursel ( _HMG_aControlHandles[ i ], Len ( _HMG_aControlRangeMax[ i ] ) )

      _BrowseOnChange ( i )
   ENDIF

   _HMG_IPE_ROW := 1
   _HMG_IPE_COL := 1

RETURN

*-----------------------------------------------------------------------------*
STATIC PROCEDURE _BrowseVscrollUpdate ( i )
*-----------------------------------------------------------------------------*
   LOCAL ActualRecord
   LOCAL RecordCount
   LOCAL KeyCount

   // If vertical scrollbar is used it must be updated
   IF _HMG_aControlIds[ i ] != 0

      KeyCount := ordKeyCount()
      IF KeyCount > 0
         ActualRecord := ordKeyNo()
         RecordCount := KeyCount
      ELSE
         ActualRecord := RecNo()
         RecordCount := RecCount()
      ENDIF

      _HMG_aControlBrushHandle[ i ] := RecordCount

      IF RecordCount < 100
         SetScrollRange ( _HMG_aControlIds[ i ], SB_CTL, 1, RecordCount, .T. )
         SetScrollPos ( _HMG_aControlIds[ i ], SB_CTL, ActualRecord, .T. )
      ELSE
         SetScrollRange ( _HMG_aControlIds[ i ], SB_CTL, 1, 100, .T. )
         SetScrollPos ( _HMG_aControlIds[ i ], SB_CTL, Int ( ActualRecord * 100 / RecordCount ), .T. )
      ENDIF

   ENDIF

RETURN

*-----------------------------------------------------------------------------*
PROCEDURE _BrowseVscrollFastUpdate ( i, d )
*-----------------------------------------------------------------------------*
   LOCAL ActualRecord
   LOCAL RecordCount

   // If vertical scrollbar is used it must be updated
   IF _HMG_aControlIds[ i ] != 0

      RecordCount := _HMG_aControlBrushHandle[ i ]

      IF .NOT. ISNUMBER( RecordCount ) .OR. RecordCount == 0
         RETURN
      ENDIF

      IF RecordCount < 100
         ActualRecord := GetScrollPos( _HMG_aControlIds[ i ], 2 )
         ActualRecord := ActualRecord + d
         SetScrollRange ( _HMG_aControlIds[ i ], SB_CTL, 1, RecordCount, .T. )
         SetScrollPos ( _HMG_aControlIds[ i ], SB_CTL, ActualRecord, .T. )
      ENDIF

   ENDIF

RETURN

*-----------------------------------------------------------------------------*
FUNCTION _SetGetBrowseProperty ( ControlName, ParentForm, nId, Value )
*-----------------------------------------------------------------------------*
   LOCAL i := GetControlIndex ( ControlName, ParentForm )
   LOCAL RetVal := .T.

   IF i > 0 .AND. _HMG_aControlType[ i ] == 'BROWSE'

      IF PCount() > 3
         _HMG_aControlMiscData1[ i ][ nId ] := Value
      ELSE
         RetVal := _HMG_aControlMiscData1[ i ][ nId ]
      ENDIF

   ENDIF

RETURN RetVal

#endif
