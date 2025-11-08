/*---------------------------------------------------------------------------
 MINIGUI - Harbour Win32 GUI library source code
 Adaptation FiveWin Class TSBrowse 9.0
---------------------------------------------------------------------------*/

#include "minigui.ch"
#include "hbclass.ch"
#include "dbinfo.ch"
#include "TSBrowse.ch"

// 03.10.2012
// If it's uncommented:
// - select a row by Shift+DblClick
// - evaluate the block bPreSelect before selection (check of condition for selection)
// #define __EXT_SELECTION__

// 26.05.2017
// If it's uncommented:
// - extended user keys handling
// - evaluate the code block by a key from a hash
#define __EXT_USERKEYS__

EXTERN OrdKeyNo, OrdKeyCount, OrdKeyGoto

#define SB_VERT             1
#define PBM_SETPOS       1026
#define EM_SETSEL         177
#define EM_SETREADONLY    207

#define VK_CONTEXT         93
#define WS_3DLOOK           4  // 0x4L
#define WM_SETFONT         48  // 0x0030
#define WM_LBUTTONDBLCLK  515  // 0x203

// mouse wheel Windows message
#define WM_MOUSEWHEEL     522

// let's save DGroup space
// ahorremos espacio para DGroup
#define nsCol        asTSB[1]
#define nsWidth      asTSB[2]
#define nsOldPixPos  asTSB[3]
#define bCxKeyNo     asTSB[4]
#define bCmKeyNo     asTSB[5]
#define nGap         asTSB[6]
#define nNewEle      asTSB[7]
#define nKeyPressed  asTSB[8]
#define lNoAppend    asTSB[9]
#define nInstance    asTSB[10]

// api maximal vertical scrollbar position
#define MAX_POS                         65535

#define xlWorkbookNormal                -4143
#define xlContinuous                    1
#define xlHAlignCenterAcrossSelection   7

#ifdef __XHARBOUR__
#include "hbcompat.ch"
#xtranslate hb_Sec( [<x>] ) => Secs( <x> )
#endif

#xtranslate _DbSkipper => DbSkipper

MEMVAR _TSB_aControlhWnd
MEMVAR _TSB_aControlObjects
MEMVAR _TSB_aClientMDIhWnd

STATIC asTSB := { NIL, NIL, 0, NIL, NIL, 0, 0, NIL, NIL, NIL }
STATIC hToolTip := 0

*-----------------------------------------------------------------------------*
FUNCTION _DefineTBrowse( ControlName, ParentFormName, nCol, nRow, nWidth, nHeight, ;
      aHeaders, aWidths, bFields, value, fontname, fontsize, tooltip, change, ;
      bDblclick, aHeadClick, gotfocus, lostfocus, uAlias, delete, lNogrid, ;
      aImages, aJust, HelpId, bold, italic, underline, strikeout, break, ;
      backcolor, fontcolor, lock, cell, nStyle, appendable, readonly, ;
      valid, validmessages, aColors, uWhen, nId, aFlds, cMsg, lRePaint, ;
      lEnum, lAutoSearch, uUserSearch, lAutoFilter, uUserFilter, aPicture, ;
      lTransparent, uSelector, lEditable, lAutoCol, aColSel, bInit, ;
      lLoad, lDblCursor, aNames, aFooters, nColNumber, aBrush, aEdit, Adjust, ;
      lAdjust, lEmptyValToChar, lOnGotFocusSelect )
*-----------------------------------------------------------------------------*
   LOCAL oBrw, ParentFormHandle, mVar, k
   LOCAL ControlHandle, FontHandle, blInit, aBmp := {}
   LOCAL bRClick, bLClick, hCursor, update, nLineStyle := 1
   LOCAL aTmpColor := Array( 20 ), aClr
   LOCAL i, nColums, nLen, aColNumber := Array( 5 )
   // BK
   LOCAL j, n, t, aFont, aFonts := {}, aArray
   LOCAL cFontHead, cFontFoot
   LOCAL hFontHead, hFontFoot

   LOCAL oc := NIL, ow := NIL

#ifdef _OBJECT_
   ow := oDlu2Pixel()
#endif

   DEFAULT nRow := 0, ;
      nCol := 0, ;
      nHeight := 120, ;
      nWidth := 240, ;
      value := 0, ;
      aImages := {}, ;
      aHeadClick := {}, ;
      aFlds := {}, ;
      aHeaders := {}, ;
      aWidths := {}, ;
      aPicture := {}, ;
      aJust := {}, ;
      hCursor := 0, ;
      cMsg := "", ;
      update := .F., ;
      lNogrid := .F., ;
      lock := .F., ;
      appendable := .F., ;
      lEnum := .F., ;
      lAutoSearch := .F., ;
      lAutoFilter := .F., ;
      lAutoCol := .F.

   HB_SYMBOL_UNUSED( break )
   HB_SYMBOL_UNUSED( validmessages )

   IF lNogrid
      nLineStyle := 0
   ENDIF

   IF Len( aHeaders ) > 0 .AND. ValType( aHeaders[ 1 ] ) == 'A'
      aHeaders := aHeaders[ 1 ]
   ENDIF

   IF Len( aWidths ) > 0 .AND. ValType( aWidths[ 1 ] ) == 'A'
      aWidths := aWidths[ 1 ]
   ENDIF

   IF Len( aPicture ) > 0 .AND. ValType( aPicture[ 1 ] ) == 'A'
      aPicture := aPicture[ 1 ]
   ENDIF

   IF Len( aFlds ) > 0 .AND. ValType( aFlds[ 1 ] ) == 'A'
      aFlds := aFlds[ 1 ]
   ENDIF

   IF ValType( aColSel ) != 'U' .AND. ValType( aColSel ) == 'A'
      IF ValType( aColSel[ 1 ] ) == 'A'
         aColSel := aColSel[ 1 ]
      ENDIF
   ENDIF

   IF HB_ISARRAY( aColors ) .AND. Len( aColors ) > 0 .AND. ValType( aColors[ 1 ] ) == 'A'
      aColors := aColors[ 1 ]
   ENDIF

   IF ISCHAR( uAlias ) .AND. ! Empty( lLoad ) .AND. Empty( aColSel )
      aHeaders := {}
      aNames := {}
      aColSel := {}
      ( uAlias )->( AEval( Array( FCount() ), {| cn, nn | cn := FieldName( nn ), ;
         AAdd( aHeaders, cn ), AAdd( aNames, cn ), AAdd( aColSel, cn ) } ) )
      IF cell .AND. Empty( aColors )
         aColors := {}
         AAdd( aColors, { CLR_FOCUSF, {| c, n, b | c := n, iif( b:nCell == n, GetSysColor( COLOR_WINDOWTEXT ), GetSysColor( COLOR_CAPTIONTEXT ) ) } } )
         AAdd( aColors, { CLR_FOCUSB, {| c, n, b | c := n, iif( b:nCell == n, GetSysColor( COLOR_ACTIVECAPTION ), -GetSysColor( COLOR_ACTIVECAPTION ) ) } } )
      ENDIF
   ENDIF

   /* BK  18.05.2015 */
   IF ValType( uWhen ) == 'B'
      IF ValType( readonly ) != 'A'
         readonly := ! Eval( uWhen )
      ENDIF
      uWhen := NIL
   ENDIF

   IF ValType( valid ) == 'B'
      valid := Eval( valid )
   ENDIF

   // BK
   IF ! Empty( FontName ) .AND. HB_ISARRAY( FontName )
      AEval( FontName, {| cf | AAdd( aFonts, cf ) } )
      aFont := ASize( aFonts, 6 )
      FontName := aFont[ 1 ]
      cFontHead := aFont[ 2 ]
      cFontFoot := aFont[ 3 ]
      IF ! Empty( cFontHead )
         hFontHead := GetFontHandle( cFontHead )
      ENDIF
      IF ! Empty( cFontFoot )
         hFontFoot := GetFontHandle( cFontFoot )
      ENDIF
      IF Empty( cFontFoot ) .AND. ! Empty( cFontHead )
         hFontFoot := hFontHead
      ENDIF
   ENDIF
   /* BK end */
   IF ( FontHandle := GetFontHandle( FontName ) ) != 0
      GetFontParamByRef( FontHandle, @FontName, @FontSize, @bold, @italic, @underline, @strikeout )
   ENDIF

   IF Type( '_TSB_aControlhWnd' ) != 'A'
      PUBLIC _TSB_aControlhWnd := {}, _TSB_aControlObjects := {}, _TSB_aClientMDIhWnd := {}
   ENDIF

   IF HB_ISARRAY( aColors ) .AND. Len( aColors ) > 0
      IF HB_ISARRAY( aColors[ 1 ] )
         FOR EACH aClr IN aColors
            IF HB_ISNUMERIC( aClr[ 1 ] ) .AND. aClr[ 1 ] > 0 .AND. aClr[ 1 ] <= Len( aTmpColor )
               aTmpColor[ aClr[ 1 ] ] := aClr[ 2 ]
            ENDIF
         NEXT
      ELSE
         AEval( aColors, {| bColor, nEle | aTmpColor[ nEle ] := bColor } )
      ENDIF
   ENDIF

   IF ValType( fontcolor ) != "U"
      aTmpColor[ 1 ] := RGB( fontcolor[ 1 ], fontcolor[ 2 ], fontcolor[ 3 ] )
   ENDIF

   IF ValType( backcolor ) != "U"
      aTmpColor[ 2 ] := RGB( backcolor[ 1 ], backcolor[ 2 ], backcolor[ 3 ] )
   ENDIF

   IF _HMG_BeginWindowActive .OR. _HMG_BeginDialogActive
      IF _HMG_BeginWindowMDIActive
         ParentFormHandle := GetActiveMdiHandle()
         ParentFormName := _GetWindowProperty( ParentFormHandle, "PROP_FORMNAME" )
      ELSE
         ParentFormName := iif( _HMG_BeginDialogActive, _HMG_ActiveDialogName, _HMG_ActiveFormName )
      ENDIF
      IF .NOT. Empty( _HMG_ActiveFontName ) .AND. ValType( FontName ) == "U"
         FontName := _HMG_ActiveFontName
      ENDIF
      IF .NOT. Empty( _HMG_ActiveFontSize ) .AND. ValType( FontSize ) == "U"
         FontSize := _HMG_ActiveFontSize
      ENDIF
   ENDIF

   IF _HMG_FrameLevel > 0
      nCol += _HMG_ActiveFrameCol[ _HMG_FrameLevel ]
      nRow += _HMG_ActiveFrameRow[ _HMG_FrameLevel ]
      ParentFormName := _HMG_ActiveFrameParentFormName[ _HMG_FrameLevel ]
   ENDIF

   IF .NOT. _IsWindowDefined( ParentFormName ) .AND. .NOT. _HMG_DialogInMemory
      MsgMiniGuiError( "Window: " + ParentFormName + " is not defined." )
   ENDIF

   IF ISCHAR ( ControlName ) .AND. ControlName == "0"
      ControlName := HMG_GetUniqueName()
   ENDIF

   IF _IsControlDefined( ControlName, ParentFormName ) .AND. .NOT. _HMG_DialogInMemory
      MsgMiniGuiError( "Control: " + ControlName + " Of " + ParentFormName + " already defined." )
   ENDIF

   IF aImages != NIL .AND. ValType( aImages ) == 'A'
      aBmp := Array( Len( aImages ) )
      AEval( aImages, {| cImage, nEle | aBmp[ nEle ] := LoadImage( cImage ) } )
   ENDIF

   mVar := '_' + ParentFormName + '_' + ControlName
   k := _GetControlFree()

   IF _HMG_BeginDialogActive

      ParentFormHandle := _HMG_ActiveDialogHandle

      nStyle := WS_CHILD + WS_TABSTOP + WS_VISIBLE + WS_CAPTION + WS_BORDER + WS_SYSMENU + WS_THICKFRAME

      IF _HMG_DialogInMemory // Dialog Template
         IF GetClassInfo( GetInstance(), ControlName ) == NIL
            IF ! Register_Class( ControlName, CreateSolidBrush( GetRed( GetSysColor( COLOR_BTNFACE ) ), GetGreen( GetSysColor( COLOR_BTNFACE ) ), GetBlue( GetSysColor( COLOR_BTNFACE ) ) ) )
               RETURN NIL
            ENDIF
         ENDIF
         blInit := {| x, y, z | InitDialogBrowse( x, y, z ) }
         AAdd( _HMG_aDialogItems, { nId, k, ControlName, nStyle, 0, nCol, nRow, nWidth, nHeight, "", HelpId, tooltip, fontname, fontsize, bold, italic, underline, strikeout, blInit, _HMG_BeginTabActive, .F., _HMG_ActiveTabPage } )
         IF _HMG_aDialogTemplate[ 3 ] // Modal
            RETURN NIL
         ENDIF

      ELSE

         ControlHandle := GetDialogItemHandle( ParentFormHandle, nId )
         SetWindowStyle( ControlHandle, nStyle, .T. )

         nCol := GetWindowCol( Controlhandle )
         nRow := GetWindowRow( Controlhandle )
         nWidth := GetWindowWidth( Controlhandle )
         nHeight := GetWindowHeight( Controlhandle )
      ENDIF

   ELSE
      // BK
      IF HB_ISOBJECT( uAlias )
         lAutoCol  := .T.
         lEditable := .T.
      ELSEIF HB_ISARRAY( uAlias ) .AND. Len( uAlias ) > 0 .AND. ValType( uAlias[ 1 ] ) == 'A'
         aArray := uAlias
         uAlias := NIL
      ENDIF
      IF HB_ISLOGICAL( aEdit )
         lEditable := aEdit
         aEdit := NIL
      ENDIF
      /* BK end */
      ParentFormHandle := GetFormHandle( ParentFormName )
      hToolTip := GetFormToolTipHandle( ParentFormName )

      oBrw := TSBrowse():New( ControlName, nRow, nCol, nWidth, nHeight, ;
         bFields, aHeaders, aWidths, ParentFormName, ;
         change, bDblClick, bRClick, fontname, fontsize, ;
         hCursor, aTmpColor, aBmp, cMsg, update, uAlias, uWhen, value, cell, ;
         nStyle, bLClick, aFlds, aHeadClick, nLineStyle, lRePaint, ;
         delete, aJust, lock, appendable, lEnum, ;
         lAutoSearch, uUserSearch, lAutoFilter, uUserFilter, aPicture, ;
         lTransparent, uSelector, lEditable, lAutoCol, aColSel, tooltip, aBrush )

      IF HB_ISARRAY( aFont ) .AND. Len( aFont ) > 3
         IF HB_ISCHAR( aFont[ 4 ] )
            oBrw:hFontSpcHd := GetFontHandle( aFont[ 4 ] )
         ENDIF
         IF HB_ISCHAR( aFont[ 5 ] )
            oBrw:hFontSupHd := GetFontHandle( aFont[ 5 ] )
         ENDIF
         IF HB_ISCHAR( aFont[ 6 ] )
            oBrw:hFontEdit := GetFontHandle( aFont[ 6 ] )
         ENDIF
      ENDIF
      // BK
      IF HB_ISARRAY( aArray )
         oBrw:SetArrayTo( aArray, { hFontHead, hFontFoot }, aHeaders, aWidths, aFooters, aPicture, aJust, aNames )
      ELSE
         IF ! Empty( hFontHead )
            oBrw:hFontHead := hFontHead
         ENDIF
         IF ! Empty( hFontFoot )
            oBrw:hFontFoot := hFontFoot
         ENDIF
      ENDIF
      /* BK end */

      ControlHandle := oBrw:hWnd

      IF ValType( gotfocus ) != "U"
         oBrw:bGotFocus := gotfocus
      ENDIF
      IF ValType( lostfocus ) != "U"
         oBrw:bLostFocus := lostfocus
      ENDIF
      IF ! lRePaint
         _HMG_ActiveTBrowseName := ControlName
         _HMG_ActiveTBrowseHandle := ControlHandle
         _HMG_BeginTBrowseActive := .T.
      ENDIF
      // BK
      IF ! Empty( lLoad ) .AND. oBrw:nDataType == DATATYPE_RDD

         oBrw:LoadFields( ! Empty( lEditable ) )

         IF ( n := Len( oBrw:aColumns ) ) > 0
            IF HB_ISARRAY( aHeaders )
               j := Min( Len( aHeaders ), n )
               FOR t := 1 TO j
                  IF aHeaders[ t ] != NIL
                     IF HB_ISCHAR( aHeaders[ t ] ) .AND. ";" $ aHeaders[ t ]
                        aHeaders[ t ] := StrTran( aHeaders[ t ], ";", Chr( 13 ) )
                     ENDIF
                     oBrw:aColumns[ t ]:cHeading := aHeaders[ t ]
                  ENDIF
               NEXT
            ENDIF
            IF HB_ISARRAY( aWidths )
               j := Min( Len( aWidths ), n )
               FOR t := 1 TO j
                  IF aWidths[ t ] != NIL
                     oBrw:aColumns[ t ]:nWidth := aWidths[ t ]
                  ENDIF
               NEXT
            ENDIF
            IF HB_ISARRAY( aJust )
               j := Min( Len( aJust ), n )
               FOR t := 1 TO j
                  IF aJust[ t ] != NIL
                     oBrw:aColumns[ t ]:nAlign := aJust[ t ]
                     oBrw:aColumns[ t ]:nFAlign := aJust[ t ]
                  ENDIF
               NEXT
               aJust := NIL
            ENDIF
            IF HB_ISARRAY( aPicture )
               j := Min( Len( aPicture ), n )
               FOR t := 1 TO j
                  IF aPicture[ t ] != NIL
                     oBrw:aColumns[ t ]:cPicture := aPicture[ t ]
                  ENDIF
               NEXT
            ENDIF
         ENDIF

         oBrw:nHeightCell += 4
      ENDIF

      IF ( nColums := Len( oBrw:aColumns ) ) > 0 /* BK  18.05.2015 */
         IF ValType( readonly ) == 'A' // sets oCol:bWhen
            nLen := Min( Len( readonly ), nColums )
            FOR i := 1 TO nLen
               IF ValType( readonly[ i ] ) == 'B'
                  oBrw:aColumns[ i ]:bWhen := readonly[ i ]
               ELSEIF readonly[ i ] == NIL .OR. Empty( readonly[ i ] )
                  oBrw:aColumns[ i ]:bWhen := {|| .T. }
                  oBrw:aColumns[ i ]:cWhen := '{||.T.}'
               ELSE
                  oBrw:aColumns[ i ]:bWhen := {|| .F. }
                  oBrw:aColumns[ i ]:cWhen := '{||.F.}'
               ENDIF
            NEXT
         ENDIF

         IF ValType( valid ) == 'A' // sets oCol:bValid
            nLen := Min( Len( valid ), nColums )
            FOR i := 1 TO nLen
               IF ValType( valid[ i ] ) == 'B'
                  oBrw:aColumns[ i ]:bValid := valid[ i ]
               ENDIF
            NEXT
         ENDIF
         // BK
         n := nColums

         IF ! HB_ISARRAY( aArray )
            IF HB_ISARRAY( aNames )
               j := Min( Len( aNames ), n )
               FOR t := 1 TO j
                  IF ! Empty( aNames[ t ] ) .AND. HB_ISCHAR( aNames[ t ] )
                     oBrw:aColumns[ t ]:cName := aNames[ t ]
                  ENDIF
               NEXT
            ENDIF

            IF HB_ISLOGICAL( aFooters ) .AND. aFooters
               aFooters := Array( n )
               AFill( aFooters, " " )
            ENDIF

            IF HB_ISARRAY( aFooters )
               j := Min( Len( aFooters ), n )
               FOR t := 1 TO j
                  IF aFooters[ t ] != NIL
                     IF HB_ISCHAR( aFooters[ t ] ) .AND. ";" $ aFooters[ t ]
                        aFooters[ t ] := StrTran( aFooters[ t ], ";", Chr( 13 ) )
                     ENDIF
                     oBrw:aColumns[ t ]:cFooting := aFooters[ t ]
                  ELSE
                     oBrw:aColumns[ t ]:cFooting := " "
                  ENDIF
               NEXT
               oBrw:lDrawFooters := .T.
               oBrw:lFooting := .T.
               oBrw:nHeightFoot := oBrw:nHeightCell
            ENDIF
         ENDIF

         IF HB_ISARRAY( aEdit )
            j := Min( Len( aEdit ), n )
            FOR t := 1 TO j
               IF aEdit[ t ] != NIL
                  oBrw:aColumns[ t ]:lEdit := ! Empty( aEdit[ t ] )
               ENDIF
            NEXT
         ENDIF

         IF ! Empty( lDblCursor )
            AEval( oBrw:aColumns, {| oCol | oCol:lFixLite := .T. } )
         ENDIF
         IF ! Empty( lEmptyValToChar )
            AEval( oBrw:aColumns, {| oCol | oCol:lEmptyValToChar := .T. } )
         ENDIF
         IF ! Empty( lOnGotFocusSelect )
            AEval( oBrw:aColumns, {| oCol | oCol:lOnGotFocusSelect := .T. } )
         ENDIF

         IF nColNumber != NIL
            IF HB_ISLOGICAL( nColNumber )
               nColNumber := iif( nColNumber, 1, NIL )
            ELSEIF HB_ISARRAY( nColNumber )
               IF Len( nColNumber ) > 1
                  ASize( nColNumber, 5 )
                  aColNumber[ 2 ] := nColNumber[ 2 ]  // nWidth
                  aColNumber[ 3 ] := nColNumber[ 3 ]  // nAlign
                  aColNumber[ 4 ] := nColNumber[ 4 ]  // Bitmap
                  aColNumber[ 5 ] := nColNumber[ 5 ]  // cName
                  nColNumber := nColNumber[ 1 ]
               ELSE
                  nColNumber := 1
               ENDIF
               aColNumber[ 1 ] := nColNumber
               DEFAULT aColNumber[ 2 ] := 80
            ENDIF
         ENDIF

         IF HB_ISNUMERIC( nColNumber )
            nColNumber := iif( nColNumber > 0 .AND. nColNumber <= n, nColNumber, 1 )

            oBrw:InsColNumber( aColNumber[ 2 ], nColNumber, aColNumber[ 5 ], aColNumber[ 3 ], aColNumber[ 4 ] )

            oBrw:nCell := nColNumber + 1
            oBrw:nFreeze := nColNumber
            oBrw:lLockFreeze := .T.
         ENDIF

         IF !( Adjust == NIL .AND. lAdjust == NIL )

            IF HB_ISLOGICAL( lAdjust ) .AND. lAdjust
               Adjust := lAdjust
            ENDIF
            IF Adjust != NIL
               oBrw:AdjColumns( Adjust )
            ENDIF
         ENDIF
      ENDIF
      /* BK end */
   ENDIF

   IF .NOT. _HMG_DialogInMemory

      IF _HMG_BeginTabActive
         AAdd( _HMG_ActiveTabCurrentPageMap, Controlhandle )
      ENDIF

      IF FontHandle != 0
         _SetFontHandle( ControlHandle, FontHandle )
         oBrw:hFont := FontHandle
      ELSE
         IF ValType( fontname ) == "U"
            fontname := _HMG_DefaultFontName
         ENDIF
         IF ValType( fontsize ) == "U"
            fontsize := _HMG_DefaultFontSize
         ENDIF
         oBrw:hFont := _SetFont( ControlHandle, fontname, fontsize, bold, italic, underline, strikeout )
      ENDIF

   ENDIF

#ifdef _NAMES_LIST_
   _SetNameList( mVar , k )
#else
   PUBLIC &mVar. := k
#endif

   _HMG_aControlType[ k ] := "TBROWSE"
   _HMG_aControlNames[ k ] := ControlName
   _HMG_aControlHandles[ k ] := ControlHandle
   _HMG_aControlParenthandles[ k ] := ParentFormHandle
   _HMG_aControlIds[ k ] := oBrw
   _HMG_aControlProcedures[ k ] := bDblclick
   _HMG_aControlPageMap[ k ] := aHeaders
   _HMG_aControlValue[ k ] := value
   _HMG_aControlInputMask[ k ] := Lock
   _HMG_aControllostFocusProcedure[ k ] := lostfocus
   _HMG_aControlGotFocusProcedure[ k ] := gotfocus
   _HMG_aControlChangeProcedure[ k ] := change
   _HMG_aControlDeleted[ k ] := .F.
   _HMG_aControlBkColor[ k ] := aImages
   _HMG_aControlFontColor[ k ] := NIL
   _HMG_aControlDblClick[ k ] := bDblclick
   _HMG_aControlHeadClick[ k ] := aHeadClick
   _HMG_aControlRow[ k ] := nRow
   _HMG_aControlCol[ k ] := nCol
   _HMG_aControlWidth[ k ] := nWidth
   _HMG_aControlHeight[ k ] := nHeight
   _HMG_aControlSpacing[ k ] := uAlias
   _HMG_aControlContainerRow[ k ] := iif( _HMG_FrameLevel > 0, _HMG_ActiveFrameRow[ _HMG_FrameLevel ], -1 )
   _HMG_aControlContainerCol[ k ] := iif( _HMG_FrameLevel > 0, _HMG_ActiveFrameCol[ _HMG_FrameLevel ], -1 )
   _HMG_aControlPicture[ k ] := delete
   _HMG_aControlContainerHandle[ k ] := 0
   _HMG_aControlFontName[ k ] := fontname
   _HMG_aControlFontSize[ k ] := fontsize
   _HMG_aControlFontAttributes[ k ] := { bold, italic, underline, strikeout }
   _HMG_aControlToolTip[ k ] := tooltip
   _HMG_aControlRangeMin[ k ] := 0
   _HMG_aControlRangeMax[ k ] := {}
   _HMG_aControlCaption[ k ] := aHeaders
   _HMG_aControlVisible[ k ] := .T.
   _HMG_aControlHelpId[ k ] := HelpId
   _HMG_aControlFontHandle[ k ] := oBrw:hFont
   _HMG_aControlBrushHandle[ k ] := 0
   _HMG_aControlEnabled[ k ] := .T.
   _HMG_aControlMiscData1[ k ] := 0
   _HMG_aControlMiscData2[ k ] := ''

   IF _HMG_lOOPEnabled
      Eval( _HMG_bOnControlInit, k, mVar )
#ifdef _OBJECT_
      ow := _WindowObj( ParentFormHandle )
      oc := _ControlObj( ControlHandle )
#endif
   ENDIF

   Do_ControlEventProcedure( bInit, k, oBrw, ow, oc )

RETURN oBrw

*-----------------------------------------------------------------------------*
FUNCTION _EndTBrowse( bEnd )
*-----------------------------------------------------------------------------*
   LOCAL i, nLastCol, oBrw
   LOCAL oc := NIL, ow := NIL

#ifdef _OBJECT_
   ow := oDlu2Pixel()
#endif

   IF _HMG_BeginTBrowseActive
      i := AScan( _HMG_aControlHandles, _HMG_ActiveTBrowseHandle )
      IF i > 0
         oBrw := _HMG_aControlIds[ i ]
         oBrw:lRePaint := .T.
         oBrw:Display()
         // Addition by SergKis
         IF ( nLastCol := oBrw:nColCount() ) > 0 .AND. ! Empty( oBrw:oPhant )
            oc := ATail( oBrw:aColumns )
            oBrw:oPhant:nClrHeadBack := iif( ValType( oc:nClrHeadBack ) == "B", Eval( oc:nClrHeadBack, nLastCol, oBrw ), oc:nClrHeadBack )
            oBrw:oPhant:nClrFootBack := iif( ValType( oc:nClrFootBack ) == "B", Eval( oc:nClrFootBack, nLastCol, oBrw ), oc:nClrFootBack )
            oBrw:Refresh()
            oc := NIL
         ENDIF
         // End of addition
         _HMG_ActiveTBrowseName := ""
         _HMG_ActiveTBrowseHandle := 0
         _HMG_BeginTBrowseActive := .F.

#ifdef _OBJECT_
         IF _HMG_lOOPEnabled
            ow := _WindowObj( _HMG_aControlParenthandles[ i ] )
            oc := _ControlObj( _HMG_aControlHandles[ i ] )
         ENDIF
#endif
         Do_ControlEventProcedure( bEnd, i, oBrw, ow, oc )
      ENDIF
   ENDIF

RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION LoadFields( ControlName, ParentForm, lEdit, aFieldNames )
*-----------------------------------------------------------------------------*
   LOCAL ix, oBrw

   DEFAULT lEdit := .F.

   ix := GetControlIndex( ControlName, ParentForm )
   IF ix > 0
      oBrw := _HMG_aControlIds[ ix ]
      IF ISARRAY( aFieldNames )
         oBrw:aColSel := aFieldNames
      ENDIF
      oBrw:LoadFields( lEdit )
   ELSE
      MsgMiniGuiError( "Can not found the control: " + ControlName + " of " + ParentForm )
   ENDIF

RETURN oBrw

*-----------------------------------------------------------------------------*
FUNCTION SetArray( ControlName, ParentForm, Arr, lAutoCols, aHead, aSizes )
*-----------------------------------------------------------------------------*
   LOCAL ix, oBrw

   ix := GetControlIndex( ControlName, ParentForm )
   IF ix > 0
      oBrw := _HMG_aControlIds[ ix ]
      oBrw:SetArray( Arr, lAutoCols, aHead, aSizes )
   ELSE
      MsgMiniGuiError( "Can not found the control: " + ControlName + " of " + ParentForm )
   ENDIF

RETURN oBrw

*-----------------------------------------------------------------------------*
FUNCTION SetArrayTo( ControlName, ParentForm, Arr, uFontHF, aHead, aSizes, uFooter, aPicture, aAlign, aName )
*-----------------------------------------------------------------------------*
   LOCAL ix, oBrw

   ix := GetControlIndex( ControlName, ParentForm )
   IF ix > 0
      oBrw := _HMG_aControlIds[ ix ]
      oBrw:SetArrayTo( Arr, uFontHF, aHead, aSizes, uFooter, aPicture, aAlign, aName )
   ELSE
      MsgMiniGuiError( "Can not found the control: " + ControlName + " of " + ParentForm )
   ENDIF

RETURN oBrw

* ============================================================================
* TSBrowse.PRG Version 9.0 Nov/30/2009
* ============================================================================

/* This Classs is a recapitulation of the code adapted by Luis Krause Mantilla,
   of FiveWin classes: TCBrowse, TWBrowse, TCColumn and Clipper Wrapers in C
   that support the visual Windows interface.

   Originally TCBrowse was a Sub-Class of TWBrowse, with this work, we have the
   new class "TSBrowse" that is no more a Sub-Class. Now, TSBrowse is an
   independent control that inherits directly from TControl class.

   My work has mainly consisted on putting pieces together with some extra from
   my own crop.

   Credits:
      Luis Krause Mantilla
      Selim Anter
      Stan Littlefield
      Marshall Thomas
      Eric Yang
      John Stolte
      Harry Van Tassell
      Martin Vogel
      Katy Hayes
      Jose Gimenez
      Hernan Diego Ceccarelli ( some ideas taked from his TWBrowse )
      Antonio Carlos Pantaglione ( Toninho@fwi.com.br )
      TSBtnGet is an adaptation of the Ricardo Ramirez TBtnGet Class
      Gianni Santamarina
      Ralph del Castillo
      Daniel Andrade
      Yamil Bracho
      Victor Manuel Tomás (VikThor)
      FiveTechSoft (original classes)

      Many thanks to all of them.

      Regards.

      Manuel Mercado.  July 15th, 2004

   ¡ Aquí vamos ! | ¡ Here we go !...  */

// ----------------------------------------------------------------------------//

CLASS TSBrowse FROM TControl

   CLASSDATA lRegistered AS LOGICAL

   CLASSDATA aProperties AS ARRAY INIT { "aColumns", "cVarName", "nTop", "nLeft", "nWidth", "nHeight" }

   CLASSDATA lVScroll, lHScroll

   // Hash-based data resreshing   // SergKis & Igor Nazarov
   DATA lFastDrawCell AS LOGICAL INIT .F.
   DATA aFastDrawCell INIT hb_Hash()
   DATA lFastDrawClear AS LOGICAL INIT .T.

   // Browser Data Type
   DATA nDataType AS NUMERIC // Data type to be used
                             // If navigation codeblocks are not specified then get automatically
                             // initialiated when adjusting the browse depending on this DATA value
   DATA lIsArr AS LOGICAL INIT .F. READONLY // browsing an array
   DATA lIsDbf AS LOGICAL INIT .F. READONLY // browsed object is a database
   DATA lIsTxt AS LOGICAL INIT .F. READONLY // browsing a text file

   DATA aActions // actions to be executed on header's click
   DATA aCheck // stock bitmaps for check box
   DATA aColors // the whole colors kit
   DATA aColSel // automatic selected columns creation with databases or recordsets
   DATA aArray AS ARRAY // browsed array
   DATA aArray_FTS      // browsed array for filter FTS
   DATA aBitmaps AS ARRAY INIT {} // array with bitmaps handles
   DATA aDefault AS ARRAY INIT {} // default values in append mode
   DATA aClipBoard // used by RButtonDown method

   DATA aColSizes // the core of TSBrowse
   DATA aColumns // the core of TSBrowse
   DATA aHeaders // the core of TSBrowse

   DATA aDefValue AS ARRAY INIT {} // for array in append mode
   DATA aIcons AS ARRAY INIT {} // array with icons names
   DATA aImages AS ARRAY INIT {} // array with bitmaps names
   DATA aJustify // compatibility with TWBrowse
   DATA aLine // bLine as array
   DATA aMsg AS ARRAY INIT {} // multi lingual feature
   DATA aKeyRemap AS ARRAY INIT {} // to prevalidate keys at KeyChar method
   DATA aPostList // used by ComboWBlock function
   DATA aRowPosAtRec // array with recno or nat of a current showed page   //SergKis
   DATA lRowPosAtRec AS LOGICAL INIT .F. // flag to activate a filling of aRowPosAtRec array
   DATA lShared AS LOGICAL INIT .F. // dbf file opened shared at .T.
   DATA aSelected // selected items in select mode
   DATA aSortBmp // stock bitmaps for sort in headers
   DATA aSuperHead // array with SuperHeads properties
   DATA aTags // array with dbf index tags
   DATA aFormatPic // array of picture clause
   DATA aPopupCol AS ARRAY INIT {} // User PopUp menu in Columns ({0} -> all)
   DATA aEditCellAdjust AS ARRAY INIT { 0, 0, 0, 0 } // array for correction of edit cell position
   DATA aDrawCols AS ARRAY INIT {} // list of columns in display
   DATA aOldParams
   DATA aOldEnabled

   DATA aAdsFieldTypes AS ARRAY INIT { ;
      { "CICHARACTER", "C" }, ; // CiCharacter
      { "C:U", "C" }, ;         // nChar
      { "C:B", "C" }, ;         // Raw
      { "Q", "C" }, ;           // VarCharFox
      { "Q:U", "C" }, ;         // nVarChar
      { "Q:B", "C" }, ;         // VarBinaryFox
      { "I", "N" }, ;           // Integer, ShortInt, LongInt
      { "B", "N" }, ;           // Double
      { "Y", "N" }, ;           // Money
      { "Z", "N" }, ;           // Curdouble
      { "M:U", "M" }, ;         // nMemo
      { "W", "M" }, ;           // Binary
      { "P", "M" } ;            // Image
   }

#ifdef __EXT_USERKEYS__
   DATA aUserKeys INIT hb_Hash()
   DATA lUserKeys INIT .F.
#endif

   DATA bBof // codeblock to check if we are before the first record
   DATA bEof // codeblock to check if we are beyond the last record
   DATA bAddRec // custom function for adding record (with your own message)
   DATA bAddBefore // evaluated before adding record
   DATA bAddAfter // evaluated after  adding record
   DATA bBitMapH // bitmap handle
   DATA bContext // evaluates windows keyboard context key
   DATA bBookMark // for xBrowse compatibility
   DATA bDelete // evaluated after user deletes a row with lCanDelete mode
   DATA bDelBefore // evaluated before user deletes (if RLock mode)
   DATA bDelAfter // evaluated after  user deletes (if RLock mode)
   DATA bEvents // custom function for events processing
   DATA bFileLock // custom function for locking database (with your own message)
   DATA bGoToPos // scrollbar block
   DATA bFilter // a simple filter tool
   DATA bIconDraw, bIconText // icons drawing directives
   DATA bInit // code block to be evaluated on init
   DATA bKeyCount // ADO keycount block
   DATA bLine, bSkip, bGoTop, bGoBottom, ;
      bLogicLen // navigation codeblocks
   DATA bChange //
   DATA bKeyNo // logical position on indexed databases
   DATA bOnDraw // evaluated in DrawSelect()
   DATA bOnDrawLine // evaluated in DrawLine()
   DATA bOnEscape // to do something when browse ends through escape key
   DATA bPostDel // evaluated after record deletion
   DATA bRecLock // custom function for locking record (with your own message)
   DATA bRecNo // retrieves or changes physical record position
   DATA bSeekChange // used by seeking feature

#ifdef __EXT_SELECTION__
   DATA bPreSelect // to be evaluated before selection for
                   // check of condition in select mode.
                   // Must return .T. or .F.
#endif
   DATA bSelected // to be evaluated in select mode
   DATA bSetOrder // used by seeking feature
   DATA bTagOrder // to restore index on GotFocus
   DATA bLineDrag // evaluated after a dividing line dragging
   DATA bColDrag // evaluated after a column dragging
   DATA bUserSearch // user code block for AutoSearch
   DATA bUserFilter // user code block for AutoFilter
   DATA bUserPopupItem // user code block for UserPopup
   DATA bUserKeys // user code block to change the
                  // behavior of pressed keys
   DATA bEditLog // user code block for logging
   DATA bOnResizeEnter // On Resize action at startup
   DATA bOnResizeExit // On Resize action at exit
   DATA bTSDrawCell // On TSDrawCell(...) execute

   DATA cAlias // data base alias or "ARRAY" or "TEXT_"
   DATA cDriver // RDD in use
   DATA cField, uValue1, uValue2 // SetFilter Params
   DATA cOrderType // index key type for seeking
   DATA cPrefix // used by TSBrowse search feature
   DATA cSeek // used by TSBrowse search feature
   DATA cFont
   DATA cChildControl
   DATA cArray
   DATA cToolTip // tooltip when mouse is over Cells
   DATA nToolTip AS NUMERIC INIT 0
   DATA nToolTipRow AS NUMERIC INIT 0
   DATA nToolTipLen AS NUMERIC INIT 512
   DATA nToolTipTime // in seconds

   DATA nIdColumn AS NUMERIC INIT 0 // ID columns counter

   DATA Cargo // for your own use

   DATA hBmpCursor AS NUMERIC // bitmap cursor for first column
   DATA hFontEdit AS NUMERIC // edition font
   DATA hFontHead AS NUMERIC // header font
   DATA hFontFoot AS NUMERIC // footer font
   DATA hFontSpcHd AS NUMERIC // special header font
   DATA hFontSupHd // super header font

   DATA l3DLook AS LOGICAL INIT .F. READONLY // internally control state of ::Look3D() in "Phantom" column
   DATA lHitTop, lHitBottom, lCaptured, lMChange // browsing flags

   DATA lAdjColumn AS LOGICAL INIT .F. // column expands to flush table window right
   DATA lAppendMode AS LOGICAL INIT .F. READONLY // automatic append flag
   DATA lAutoCol // automatic columns generation from AUTOCOLS clause
   DATA lAutoEdit AS LOGICAL INIT .F. // activates continuous edition mode
   DATA lAutoSkip AS LOGICAL INIT .F. // compatibility with TCBrowse
   DATA lCanAppend AS LOGICAL INIT .F. READONLY // activates auto append mode
   DATA lCanDelete AS LOGICAL INIT .F. HIDDEN // activates delete capability
   DATA lCanSelect AS LOGICAL INIT .F. // activates select mode
   DATA lCellBrw // celled browse flag
   DATA lCellStyle AS LOGICAL INIT .F. // compatibility with TCBrowse
   DATA lChanged AS LOGICAL INIT .F. // field has changed indicator
   DATA lClipMore AS LOGICAL INIT .F. // ClipMore RDD
   DATA lColDrag AS LOGICAL // dragging feature
   DATA lConfirm AS LOGICAL INIT .T. HIDDEN // ask for user confirm to delete a row
   DATA lDescend AS LOGICAL INIT .F. // descending indexes
   DATA lDestroy // flag to destroy bitmap created for selected records
   DATA lDontChange // avoids user to change line with mouse or keyboard
   DATA lDrawHeaders AS LOGICAL INIT .T. // condition for headers drawing
   DATA lDrawFooters // condition for footers drawing
   DATA lDrawSelect AS LOGICAL INIT .F. // flag for selected row drawing
   DATA lDrawLine AS LOGICAL INIT .T. // flag for cells row drawing
   DATA lEditable AS LOGICAL // editabe cells in automatic columns creation
   DATA lEditing AS LOGICAL INIT .F. READONLY // to avoid lost focus at editing time
   DATA lDrawSuperHd AS LOGICAL INIT .F. // condition for SuperHeader drawing
   DATA lDrawSpecHd AS LOGICAL INIT .F. // condition for SpecHeader drawing
   DATA lEditingHd AS LOGICAL INIT .F. READONLY // to avoid lost focus at editing time SpecHd
   DATA lEditableHd AS LOGICAL INIT .F. // activates edition mode of SpecHd on init
   DATA lFilterMode AS LOGICAL INIT .F. READONLY // index based filters with NTX RDD
   DATA lAutoSearch AS LOGICAL INIT .F. READONLY // condition for SuperHeader as AutoSearch
   DATA lAutoFilter AS LOGICAL INIT .F. READONLY // condition for SuperHeader as AutoFilter
   DATA lHasChgSpec AS LOGICAL INIT .F. // SpecHeader data has changed flag for further actions
   DATA lFirstFocus HIDDEN // controls some actions on init
   DATA lFirstPaint // controls some actions on init
   DATA lFixCaret AS LOGICAL // TSGet fix caret at editing time
   DATA lFooting AS LOGICAL // indicates footers can be drawn
   DATA lNoPaint // to avoid unnecessary painting
   DATA lGrasp AS LOGICAL INIT .F. READONLY // used by drag & drop feature
   DATA lHasChanged AS LOGICAL INIT .F. // browsed data has changed flag for further actions
   DATA lHasFocus AS LOGICAL INIT .F. // focused flag
   DATA lIconView AS LOGICAL INIT .F. // compatibility with TCBrowse
   DATA lInitGoTop // go to top on init, default = .T.

   DATA lLineDrag AS LOGICAL // TSBrowse dragging feature
   DATA lLiteBar AS LOGICAL INIT .F. // show solid color bar
   DATA lLockFreeze AS LOGICAL // avoids cursor positioning on frozen columns
   DATA lMoveCols AS LOGICAL // Choose between moving or exchanging columns (::moveColumn() or ::exchange())
   DATA lNoChangeOrd AS LOGICAL // avoids changing active order by double clicking on headers
   DATA lNoExit AS LOGICAL INIT .F. // prevents edit exit with arrow keys
   DATA lNoGrayBar AS LOGICAL // don't show inactive cursor
   DATA lNoHScroll AS LOGICAL // disables horizontal scroll bar
   DATA lNoKeyChar AS LOGICAL INIT .F. // no input of key char
   DATA lNoLiteBar AS LOGICAL // no cursor
   DATA lNoMoveCols AS LOGICAL // avoids resize or move columns by the user
   DATA lNoPopup AS LOGICAL INIT .T. // avoids popup menu when right click the column's header
   DATA lPopupActiv AS LOGICAL INIT .F. // defined popup menu when right click the column's header
   DATA lPopupUser AS LOGICAL INIT .F. // activates user defined popup menu
   DATA lNoResetPos AS LOGICAL // prevents to reset record position on gotfocus
   DATA lNoVScroll AS LOGICAL // disables vertical scroll bar
   DATA lLogicDrop AS LOGICAL // compatibility with TCBrowse
   DATA lPageMode AS LOGICAL INIT .F. // paging mode flag
   DATA lPainted AS LOGICAL // controls some actions on init
   DATA lRePaint AS LOGICAL // bypass paint if false
   DATA lPostEdit // to detect postediting
   DATA lPostEditGo AS LOGICAL INIT .T. // to control postediting VK_UP,VK_RIGHT,VK_LEFT,VK_DOWN
   DATA lUndo AS LOGICAL INIT .F. // used by RButtonDown method
   DATA lUpdated AS LOGICAL INIT .F. // replaces lEditCol return value
   DATA lUpperSeek AS LOGICAL INIT .T. // controls if char expresions are seek in uppercase or not
   DATA lSeek AS LOGICAL INIT .T. // activates TSBrowse seeking feature
   DATA lSelector AS LOGICAL INIT .F. // automatic first column with pointer bitmap
   DATA lCheckBoxAllReturn INIT .F. // flag for switching of Enter mode at editing of all checkboxes
   DATA lRecLockArea AS LOGICAL INIT .F. // flag to lock record for oCol:cArea alias       //SergKis
   DATA lInsertMode // flag for switching of Insert mode at editing   //Naz
   DATA lTransparent // flag for transparent browses
   DATA lEnabled AS LOGICAL INIT .T. // enable/disable TSBrowse for displaying data    //JP 1.55
   DATA lPickerMode AS LOGICAL INIT .T. // enable/disable DatePicker Mode in inplace Editing  //MWS Sep 20/07
   DATA lShowNone AS LOGICAL INIT .T. // enable/disable DatePicker ShowNone in inplace Editing
   DATA lUpDown AS LOGICAL INIT .F. // enable/disable UpDown DatePicker in inplace Editing
   DATA lPhantArrRow AS LOGICAL INIT .F. // Flag for initial empty row in array
   DATA lEnum AS LOGICAL INIT .F. // activates SpecHeader as Enumerator
   DATA lDestroyAll AS LOGICAL INIT .F. // flag to destroy all bitmaps created with using of function LoadImage()
   DATA lMoreFields AS LOGICAL INIT .F. // flag to activate of support for multi-column tables

   DATA nAdjColumn AS NUMERIC // column expands to flush table window right
   DATA nAligBmp AS NUMERIC INIT 0 // bitmap layout in selected cell
   DATA nCell AS NUMERIC // actual column
   DATA nClrHeadBack, nClrHeadFore // headers colors
   DATA nClrFocuBack, nClrFocuFore // focused cell colors
   DATA nClrEditBack, nClrEditFore // editing cell colors
   DATA nClrFootBack, nClrFootFore // footers colors
   DATA nClrSeleBack, nClrSeleFore // selected cell no focused
   DATA nClrOrdeBack, nClrOrdeFore // order control column colors
   DATA nClrSpcHdBack, nClrSpcHdFore, nClrSpcHdActive // special headers colors
   DATA nClrSelectorHdBack // special selector header background color
   DATA nClrSelectorFtBack // special selector footer background color
   DATA lClrSelectorHdBack AS LOGICAL INIT .T. // special selector header background color
   DATA nClrLine // grid line color
   DATA nColOrder AS NUMERIC // compatibility with TCBrowse
   DATA nColPos AS NUMERIC INIT 0 // grid column position
   DATA nColSel AS NUMERIC INIT 0 // column to mark in selected records
   DATA nColSpecHd AS NUMERIC // activatec editing column of SpecHeader
   DATA nDragCol AS NUMERIC INIT 0 HIDDEN // drag & drop  feature
   DATA nFireKey // key to start edition, defaults to VK_F2
   DATA nFirstKey AS NUMERIC INIT 0 HIDDEN // First logic pos in filtered databases
   DATA nFreeze AS NUMERIC // 0,1,2.. freezes left most columns
   DATA nHeightCell AS NUMERIC INIT 0 // resizable cell height
   DATA nHeightHead AS NUMERIC INIT 0 // "    header  "
   DATA nHeightFoot AS NUMERIC INIT 0 // "    footer  "
   DATA nHeightSuper AS NUMERIC INIT 0 // "    Superhead  "
   DATA nHeightSpecHd AS NUMERIC INIT 0 // "    Special header  "
   DATA nCellMarginLR // space margin left or right cell
   DATA nIconPos // compability with TCBrowse
   DATA nLastPainted AS NUMERIC INIT 0 HIDDEN // last painted nRow
   DATA nLastPos AS NUMERIC // last record position before lost focus
   DATA nLastnAt AS NUMERIC INIT 0 HIDDEN // last ::nAt value before lost focus
   DATA nLen AS NUMERIC // total number of browsed items
   DATA nLineStyle // user definable grid lines style
   DATA nMaxFilter // maximum number of records to count on index based filters
   DATA nPopupActiv AS NUMERIC // last activated user popup menu

   DATA nMemoHE // memo sizes on edit and view mode
   DATA nMemoWE // Height in lines and Width in pixels
   DATA nMemoHV // default: 3 lines height and 200 pixels width
   DATA nMemoWV

   DATA nOldCell HIDDEN // to control column bGotfocus
   DATA nOffset AS NUMERIC INIT 0 HIDDEN // offset marker for text viewer
   DATA nPaintRow AS NUMERIC // row being painted in DrawLine Method
   DATA nPhantom AS NUMERIC INIT PHCOL_GRID // controls drawing state for "Phantom" column (-1 or -2) inside ::Look3D()
   DATA nPrevRec // internally used to go previous record back
   DATA nRowPos, nAt AS NUMERIC INIT 0 // grid row positions
   DATA nSelWidth // Selector column's width
   DATA nLenPos AS NUMERIC INIT 0 // total number of browsed items in Window  JP 1.31
   DATA nWheelLines // lines to scroll with mouse wheel action
   DATA nFontSize // New from HMG
   DATA nMinWidthCols AS NUMERIC INIT 4 // minimal columns width at resizing  GF 1.96
   DATA nUserKey // user key to change the behavior of pressed keys
   DATA nSortColDir AS NUMERIC INIT 0 // Sorting table columns ascending or descending
   DATA nClr_Gray AS NUMERIC INIT CLR_GRAY
   DATA nClr_HGray AS NUMERIC INIT CLR_HGRAY
   DATA nClr_Lines AS NUMERIC INIT GetSysColor( COLOR_BTNSHADOW )
   DATA nCntKeysLR AS NUMERIC INIT 0
   DATA nMaxKeysLR AS NUMERIC INIT 3
   DATA nCntScroll AS NUMERIC INIT 0
   DATA nMaxScroll AS NUMERIC INIT 3
   DATA oGet // get object
   DATA oPhant // phantom column
   DATA oRSet // recordset toleauto object
   DATA oTxtFile AS OBJECT // for text files browsing (TTxtFile() class)

   DATA uBmpSel // bitmap to show in selected records
   DATA uLastTag // last TagOrder before losing focus
   VAR nLapsus AS NUMERIC INIT 0 PROTECTED

   // ---------------------------------------------------------------------------------------------
   METHOD New( cControlName, nRow, nCol, nWidth, nHeight, bLine, aHeaders, aColSizes, cParentWnd, ;
      bChange, bLDblClick, bRClick, cFont, nFontSize, hCursor, aColors, aImages, cMsg, ;
      lUpdate, uAlias, bWhen, nValue, lCellBrw, nStyle, bLClick, aLine, ;
      aActions, nLineStyle, lRePaint, lDelete, aJust, lLock, lAppend, lEnum, ;
      lAutoSearch, uUserSearch, lAutoFilter, uUserFilter, aPicture, ;
      lTransparent, uSelector, lEditable, lAutoCol, aColSel, cTooltip, aBrush ) CONSTRUCTOR

   METHOD AddColumn( oColumn )

   METHOD AppendRow( lUnlock ) // SergKis & Igor Nazarov

   METHOD AddSuperHead( nFromCol, nToCol, uHead, nHeight, aColors, l3dLook, ;
      uFont, uBitMap, lAdjust, lTransp, ;
      lNoLines, nHAlign, nVAlign )

   METHOD BeginPaint() INLINE iif( ::lRepaint, ::Super:BeginPaint(), 0 )

   METHOD BugUp() INLINE ::UpStable()

   METHOD BiClr( uClrOdd, uClrPair )

   METHOD Bof() INLINE iif( ::bBoF != NIL, Eval( ::bBof ), .F. )

   METHOD ChangeFont( hFont, nColumn, nLevel )

   METHOD DbSkipper( nToSkip )

   METHOD Default()

   METHOD Del( nItem )

   METHOD DeleteRow( lAll, lUpStable )

   METHOD DelColumn( nPos )

   METHOD Destroy()

   METHOD Display()

   METHOD DrawFooters( lDrawCell ) INLINE ::DrawHeaders( .T., lDrawCell )

   METHOD DrawIcons()

   METHOD DrawLine( xRow, lDrawCell )

   METHOD DrawPressed( nCell, lPressed )

   METHOD DrawSelect( xRow, lDrawCell )

   METHOD DrawSuper( lDrawCell )

   METHOD DrawHeaders( lFooters, lDrawCell )

   METHOD Edit( uVar, nCell, nKey, nKeyFlags, cPicture, bValid, nClrFore, nClrBack )

   METHOD EditExit( nCol, nKey, uVar, bValid, lLostFocus )

   METHOD EndPaint() INLINE iif( ::lRePaint, ::Super:EndPaint(), ( ::lRePaint := .T., 0 ) )

   METHOD Eof() INLINE iif( ::bEoF != NIL, Eval( ::bEof ), .F. )

   METHOD Excel2( cFile, lActivate, hProgress, cTitle, lSave, bPrintRow )

   METHOD ExcelOle( cXlsFile, lActivate, hProgress, cTitle, hFont, lSave, bExtern, aColSel, bPrintRow )

   METHOD Exchange( nCol1, nCol2 ) INLINE ::SwitchCols( nCol1, nCol2 ), ::SetFocus()

   METHOD ExpLocate( cExp, nCol )

   METHOD ExpSeek( cExp, lSoft )

   METHOD FastDrawClear( cCell ) // SergKis & Igor Nazarov

   METHOD FreezeCol( lNext )

   METHOD GetAllColsWidth()

   METHOD GetColSizes() INLINE iif( ValType( ::aColSizes ) == "A", ::aColSizes, Eval( ::aColSizes ) )

   METHOD GetColumn( nCol )

   METHOD AdjColumns( aColumns, nDelta )

   METHOD GetDlgCode( nLastKey )

   METHOD GetRealPos( nRelPos )

   METHOD GetTxtRow( nRowPix ) INLINE RowFromPix( ::hWnd, nRowPix, ::nHeightCell, ;
      iif( ::lDrawHeaders, ::nHeightHead, 0 ), ;
      iif( ::lFooting .AND. ::lDrawFooters, ::nHeightFoot, 0 ), ;
      iif( ::lDrawHeaders, ::nHeightSuper, 0 ), ;
      iif( ::lDrawSpecHd, ::nHeightSpecHd, 0 ) )

   METHOD GoBottom()

   METHOD GoDown()

   METHOD GoEnd()

   METHOD GoHome()

   METHOD GoLeft()

   METHOD GoNext()

   METHOD GoPos( nNewRow, nNewCol )

   METHOD GoRight()

   METHOD GotFocus( hCtlLost )

   METHOD GoTop()

   METHOD GotoRec( nRec, nRowPos ) // Igor Nazarov

   METHOD GoUp()

   METHOD HandleEvent( nMsg, nWParam, nLParam )

   METHOD HiliteCell( nCol, nColPix )

   METHOD HScroll( nWParam, nLParam )

   METHOD HThumbDrag( nPos )

   METHOD InsColumn( nPos, oColumn )

   METHOD InsColNumber( nWidth, nColumn, cName, nAlign, uBitmap )

   METHOD Insert( cItem, nAt )

   METHOD AddItem( cItem )

   METHOD IsColVisible( nCol )

   METHOD IsColVis2( nCol )

   METHOD IsEditable( nCol ) INLINE ::lCellBrw .AND. ::aColumns[ nCol ]:lEdit .AND. ;
      ( ::aColumns[ nCol ]:bWhen == NIL .OR. Eval( ::aColumns[ nCol ]:bWhen, Self ) )

   METHOD TSDrawCell( oCol, oCell )

   // SergKis addition
   ACCESS IsEdit INLINE ! Empty( ::aColumns[ ::nCell ]:oEdit )
   ACCESS Tsb    INLINE ::oWnd
   ACCESS nAtPos INLINE iif( ::nDataType == DATATYPE_RDD, ( ::cAlias )->( RecNo() ), ::nAt )
   ACCESS IsRowPosAtRec INLINE ;
      ( ::nLen > 0 .AND. ::lRowPosAtRec .AND. HB_ISARRAY( ::aRowPosAtRec ) .AND. Len( ::aRowPosAtRec ) > 0 )

   METHOD KeyChar( nKey, nFlags )

   METHOD KeyDown( nKey, nFlags )

   METHOD KeyUp( nKey, nFlags )

   METHOD LButtonDown( nRowPix, nColPix, nKeyFlags )

   METHOD LButtonUp( nRowPix, nColPix, nFlags )

   METHOD lCloseArea() INLINE ;
      iif( ::nDataType == DATATYPE_RDD .AND. ! Empty( ::cAlias ), ( ( ::cAlias )->( dbCloseArea() ), ;
      ::cAlias := "", .T. ), .F. )

   METHOD LDblClick( nRowPix, nColPix, nKeyFlags )

   METHOD lEditCol( uVar, nCol, cPicture, bValid, nClrFore, nClrBack )

   METHOD lIgnoreKey( nKey, nFlags )

   METHOD LoadFields( lEditable, aColSel, cAlsSel, aNameSel, aHeadSel ) // SergKis addition

   METHOD LoadRecordSet()

   METHOD LoadRelated( cAlias, lEditable, aNames, aHeaders )

   METHOD Look3D( lOnOff, nColumn, nLevel, lPhantom )

   METHOD LostFocus( hCtlFocus )

   METHOD lRSeek( uData, nFld, lSoft )

   METHOD MButtonDown( nRow, nCol, nKeyFlags )

   METHOD MoreFields( nMsg, nWParam ) // SergKis

   METHOD MouseMove( nRowPix, nColPix, nKeyFlags )

   METHOD MouseWheel( nKeys, nDelta, nXPos, nYPos )

   METHOD MoveColumn( nColPos, nNewPos )

   METHOD nAtCol( nColPixel, lActual )

   METHOD nAtColActual( nColPixel ) // SergKis & Igor Nazarov

   METHOD nAtIcon( nRow, nCol )

   METHOD nColCount() INLINE Len( ::aColumns )

   METHOD nColumn( cName, lPos ) INLINE _nColumn( Self, cName, ! Empty( lPos ) ) // SergKis

   METHOD nField( cName )

   METHOD nLogicPos()

   METHOD nRowCount() INLINE CountRows( ::hWnd, ::nHeightCell, iif( ::lDrawHeaders, ::nHeightHead, 0 ), ;
      iif( ::lFooting .AND. ::lDrawFooters, ::nHeightFoot, 0 ), ;
      iif( ::lDrawHeaders, ::nHeightSuper, 0 ), ;
      iif( ::lDrawSpecHd, ::nHeightSpecHd, 0 ) )

   METHOD PageUp( nLines )

   METHOD PageDown( nLines )

   METHOD Paint()

   METHOD PanHome()

   METHOD PanEnd()

   METHOD PanLeft()

   METHOD PanRight()

   METHOD PostEdit( uTemp, nCol, bValid )

   METHOD RButtonDown( nRowPix, nColPix, nFlags )

   METHOD Refresh( lPaint, lRecount )

   METHOD RelPos( nLogicPos )

   METHOD Report( cTitle, aCols, lPreview, lMultiple, lLandscape, lFromPos, aTotal )

   METHOD Reset( lBottom )

   METHOD ResetSeek()

   METHOD ResetVScroll( lInit )

   METHOD ReSize( nSizeType, nWidth, nHeight )

   METHOD TSBrwScroll( nDir ) INLINE TSBrwScroll( ::hWnd, nDir, ::hFont, ;
      ::nHeightCell, iif( ::lDrawHeaders, ::nHeightHead, 0 ), ;
      iif( ValType( ::lDrawFooters ) == "L" .AND. ;
      ::lDrawFooters, ::nHeightFoot, 0 ), ::nHeightSuper, ::nHeightSpecHd )

   METHOD Seek( nKey )

   METHOD Selection()

   METHOD Set3DText( lOnOff, lRaised, nColumn, nLevel, nClrLight, nClrShadow )

   METHOD SetAlign( nColumn, nLevel, nAlign )

   METHOD SetAppendMode( lMode )

   METHOD SetArray( aArray, lAutoCols, aHead, aSizes )

   METHOD SetArrayTo( aArray, uFontHF, aHead, aSizes, uFooter, aPicture, aAlign, aName )

   METHOD SetBtnGet( nColumn, cResName, bAction, nBmpWidth )

   METHOD SetColMsg( cMsg, cEditMsg, nCol )

   METHOD SetColor( xColor1, xColor2, nColumn )

   METHOD SetColSize( nCol, nWidth )

   METHOD SetColumns( aData, aHeaders, aColSizes )

   METHOD SetDeleteMode( lOnOff, lConfirm, bDelete, bPostDel )

   METHOD SetHeaders( nHeight, aCols, aTitles, aAlign, al3DLook, aFonts, aActions )

   METHOD SetData( nColumn, bData, aList )

   METHOD SetFilter( cField, uVal1, uVal2 )

   METHOD SetFont( hFont )

   METHOD SetIndexCols( nCol1, nCol2, nCol3, nCol4, nCol5 )

   METHOD SetItems( aItems ) INLINE ::SetArray( aItems, .T. )

   METHOD SetDBF( cAlias )

   METHOD OnReSize( nWidth, nHeight, lTop )

   METHOD SetNoHoles( nDelta, lSet ) // BK

   METHOD SetOrder( nColumn, cPrefix, lDescend )

   METHOD SetRecordSet( oRSet )

   METHOD SetSelectMode( lOnOff, bSelected, uBmpSel, nColSel, nAlign )

   METHOD SetSpinner( nColumn, lOnOff, bUp, bDown, bMin, bMax )

#ifdef __DEBUG__
   METHOD ShowSizes()
#endif

   METHOD Skip( n )

   METHOD SortArray( nCol, lDescend )

   METHOD SwitchCols( nCol1, nCol2 )

   METHOD SyncChild( aoChildBrw, abAction )

   METHOD UpAStable()

   METHOD UpRStable( nRecNo )

   METHOD UpStable()

   METHOD Proper( cString )

   METHOD VertLine( nColPixPos, nColInit, nGapp )

   METHOD VScroll( nMsg, nPos )

   METHOD Enabled( lEnab ) // JP 1.55

   METHOD HideColumns( nColumn, lHide ) // JP 1.58

   METHOD AutoSpec( nCol )

   METHOD RefreshARow( xRow ) // JP 1.88

   METHOD UserPopup( bUserPopupItem, aColumn ) // JP 1.92

   METHOD GetCellInfo( nRowPos, nCell, lColSpecHd )

   METHOD GetCellSize( nRowPos, nCell, lColSpecHd ) // 10/09/2021

   METHOD CellMarginLeftRight( nJ, oColumn, cData, nAlign, lMultiLine, nOut )

   METHOD SetGetValue( xCol, xVal ) INLINE ::bDataEval( xCol, xVal )

   METHOD SetValue( xCol, xVal ) INLINE ::SetGetValue( xCol, xVal )

   METHOD GetValue( xCol ) INLINE ::SetGetValue( xCol )

   METHOD bDataEval( oCol, xVal, nCol )

   METHOD GetValProp( xVal, xDef, nCol, nAt )

   METHOD ToolTipSet( nToolTipTime, nToolTipLen )

   METHOD nClrBackArr( aClrBack, nCol, nAt )

   METHOD nColorGet ( xVal, nCol, nAt, lPos )
   METHOD nAlignGet ( xVal, nCol, xDef )
   METHOD cPictureGet ( xVal, nCol )
   METHOD hFontGet ( xVal, nCol )
   METHOD hFontHeadGet ( xVal, nCol )
   METHOD hFontFootGet ( xVal, nCol )
   METHOD hFontSpcHdGet ( xVal, nCol )
   METHOD hFontSupHdGet ( nCol, aSuperHead )
   METHOD cTextSupHdGet ( nCol, aSuperHead )
   METHOD nForeSupHdGet ( nCol, aSuperHead )
   METHOD nBackSupHdGet ( nCol, aSuperHead )
   METHOD nAlignSupHdGet( nCol, lHAlign, aSuperHead )
   METHOD hFontSupHdSet ( nCol, uFont )
   METHOD cTextSupHdSet ( nCol, cText )
   METHOD nForeSupHdSet ( nCol, nClrText )
   METHOD nBackSupHdSet ( nCol, nClrPane )
   METHOD nAlignSupHdSet( nCol, lHAlign, nHAlign )

   METHOD GetDeltaLen( nCol, nStartCol, nMaxWidth, aColSizes )

#ifdef __EXT_USERKEYS__
   METHOD UserKeys( nKey, bKey, lCtrl, lShift )
#endif

   METHOD FilterData( cFilter, lBottom, lFocus )
   METHOD FilterFTS( cFind, lUpper, lBottom, lFocus, lAll )
   METHOD FilterFTS_Line( cFind, lUpper, lAll )
   METHOD CalcTotal( cTotal, cNoTotal, lDraw, lPicture )

   METHOD SeekRec( xVal, lSoftSeek, lFindLast, nRowPos )
   METHOD FindRec( Block, lNext, nRowPos )
   METHOD ScopeRec( xScopeTop, xScopeBottom, lBottom )

ENDCLASS

* ============================================================================
* METHOD TSBrowse:New() Version 9.0 Nov/30/2009
* ============================================================================

METHOD New( cControlName, nRow, nCol, nWidth, nHeight, bLine, aHeaders, aColSizes, cParentWnd, ;
      bChange, bLDblClick, bRClick, cFont, nFontSize, ;
      hCursor, aColors, aImages, cMsg, lUpdate, uAlias, ;
      bWhen, nValue, lCellBrw, nStyle, bLClick, aLine, ;
      aActions, nLineStyle, lRePaint, lDelete, aJust, ;
      lLock, lAppend, lEnum, lAutoSearch, uUserSearch, lAutoFilter, uUserFilter, aPicture, ;
      lTransparent, uSelector, lEditable, lAutoCol, aColSel, cTooltip, aBrush ) CLASS TSBrowse

   LOCAL aSuperHeaders, ParentHandle, ;
      aTmpColor := Array( 20 ), ;
      cAlias := "", ;
      lSuperHeader := .F., ;
      hFont, aClr

   IF HB_ISARRAY( aHeaders ) .AND. Len( aHeaders ) > 0 .AND. aHeaders[ 1 ] == NIL
      aHeaders := NIL
   ENDIF

   IF HB_ISARRAY( aColSel ) .AND. Len( aColSel ) > 0 .AND. aColSel[ 1 ] == NIL
      aColSel := NIL
   ENDIF

   DEFAULT nRow := 0, ;
      nCol := 0, ;
      nHeight := 100, ;
      nWidth := 100, ;
      nLineStyle := LINES_ALL, ;
      aLine := {}, ;
      aImages := {}, ;
      cFont := _HMG_ActiveFontName, ;
      nFontSize := _HMG_ActiveFontSize, ;
      nValue := 0, ;
      lDelete := .F., ;
      lAutoFilter := .F., ;
      lRepaint := .T., ;
      lAppend := .F., ;
      lLock := .F., ;
      lEnum := .F., ;
      lAutoSearch := .F., ;
      lTransparent := .F., ;
      lEditable := .F., ;
      lUpdate := .F., ;
      aColSizes := {}, ;
      lCellBrw := lEditable

   DEFAULT nStyle := nOr( WS_CHILD, WS_BORDER, WS_VISIBLE, WS_CLIPCHILDREN, WS_TABSTOP, WS_3DLOOK )

   aTmpColor[  1 ] := GetSysColor( COLOR_WINDOWTEXT )    // nClrText
   aTmpColor[  2 ] := GetSysColor( COLOR_WINDOW )        // nClrPane
   aTmpColor[  3 ] := GetSysColor( COLOR_BTNTEXT )       // nClrHeadFore
   aTmpColor[  4 ] := GetSysColor( COLOR_BTNFACE )       // nClrHeadBack
   aTmpColor[  5 ] := GetSysColor( COLOR_CAPTIONTEXT )   // nClrForeFocu
   aTmpColor[  6 ] := GetSysColor( COLOR_ACTIVECAPTION ) // nClrFocuBack
   aTmpColor[  7 ] := GetSysColor( COLOR_WINDOWTEXT )    // nClrEditFore
   aTmpColor[  8 ] := GetSysColor( COLOR_WINDOW )        // nClrEditBack
   aTmpColor[  9 ] := GetSysColor( COLOR_BTNTEXT )       // nClrFootFore
   aTmpColor[ 10 ] := GetSysColor( COLOR_BTNFACE )       // nClrFootBack
   aTmpColor[ 11 ] := CLR_HGRAY                          // nClrSeleFore inactive focused
   aTmpColor[ 12 ] := CLR_GRAY                           // nClrSeleBack inactive focused
   aTmpColor[ 13 ] := GetSysColor( COLOR_BTNTEXT )       // nClrOrdeFore
   aTmpColor[ 14 ] := GetSysColor( COLOR_BTNFACE )       // nClrOrdeBack
   aTmpColor[ 15 ] := GetSysColor( COLOR_BTNSHADOW )     // nClrLine
   aTmpColor[ 16 ] := GetSysColor( COLOR_BTNTEXT )       // nClrSupHeadFore
   aTmpColor[ 17 ] := GetSysColor( COLOR_BTNFACE )       // nClrSupHeadBack
   aTmpColor[ 18 ] := GetSysColor( COLOR_BTNTEXT )       // nClrSpecHeadFore
   aTmpColor[ 19 ] := GetSysColor( COLOR_BTNFACE )       // nClrSpecHeadBack
   aTmpColor[ 20 ] := CLR_HRED                           // nClrSpecHeadActive

   IF lAutoFilter
      aTmpColor[ 19 ] := GetSysColor( COLOR_WINDOW )
   ELSEIF lAutoSearch
      aTmpColor[ 19 ] := GetSysColor( COLOR_INFOBK )
   ENDIF

   IF HB_ISARRAY( aColors ) .AND. Len( aColors ) > 0
      IF HB_ISARRAY( aColors[ 1 ] )
         FOR EACH aClr IN aColors
            IF HB_ISNUMERIC( aClr[ 1 ] ) .AND. aClr[ 1 ] > 0 .AND. aClr[ 1 ] <= Len( aTmpColor )
               IF aClr[ 2 ] != NIL
                  aTmpColor[ aClr[ 1 ] ] := aClr[ 2 ]
               ENDIF
            ENDIF
         NEXT
      ELSE
         AEval( aColors, {| bColor, nEle | iif( bColor != NIL, aTmpColor[ nEle ] := bColor, ) } )
      ENDIF
   ENDIF

   IF ValType( uAlias ) == "A"
      ::nDataType := DATATYPE_ARRAY
      cAlias := "ARRAY"
      ::cArray := uAlias
      ::aArray := {}
   ELSEIF ValType( uAlias ) == "C" .AND. "." $ uAlias
      ::nDataType := DATATYPE_TEXT
      cAlias := "TEXT_" + AllTrim( uAlias )
   ELSEIF ValType( uAlias ) == "C"
      ::nDataType := DATATYPE_RDD
      cAlias := Upper( uAlias )
   ELSEIF ValType( uAlias ) == "O"
      IF "OLEAUTO" $ Upper( uAlias:ClassName() )
         ::nDataType := DATATYPE_ADO
         cAlias := "ADO_"
         ::oRSet := uAlias
      ENDIF
#ifdef __XHARBOUR__
   ELSEIF ValType( uAlias ) == "H"
      ::nDataType := DATATYPE_ARRAY
      cAlias := "ARRAY"
      uAlias := aHash2Array( uAlias )
#endif
   ENDIF

   IF _HMG_BeginWindowActive
      cParentWnd := _HMG_ActiveFormName
   ENDIF

   IF _HMG_BeginWindowMDIActive
      ParentHandle := GetActiveMdiHandle()
      cParentWnd := _GetWindowProperty( ParentHandle, "PROP_FORMNAME" )
   ELSE
      ParentHandle := GetFormHandle( cParentWnd )
   ENDIF

   DO CASE
   CASE ValType( uSelector ) == "C"
      ::lSelector := .T.
      ::hBmpCursor := LoadImage( uSelector )
   CASE ValType( uSelector ) == "N"
      ::lSelector := .T.
      ::hBmpCursor := StockBmp( 3 )
      ::nSelWidth := uSelector
   CASE ValType( uSelector ) == "L" .AND. uSelector
      ::lSelector := .T.
      ::hBmpCursor := StockBmp( 3 )
   CASE uSelector != NIL
      ::lSelector := .T.
   ENDCASE

   ::oWnd := Self

   ::cCaption := ""
   ::cTooltip := ctooltip

   ::nTop := nRow
   ::nLeft := nCol
   ::nBottom := ::nTop + nHeight - 1
   ::nRight := ::nLeft + nWidth - 1
   // JP
   ::oWnd:hWnd := ParentHandle
   ::hWndParent := ParentHandle
   ::cControlName := cControlName
   ::cParentWnd := cParentWnd

   ::lHitTop := .F.
   ::lHitBottom := .F.
   ::lFocused := .F.
   ::lCaptured := .F.
   ::lMChange := .T.
   ::nRowPos := 1
   ::nAt := 1
   ::nColPos := 1
   ::nStyle := nStyle
   ::lRePaint := lRePaint
   ::lNoHScroll := .F.
   ::lNoVScroll := .F.
   ::lNoLiteBar := .F.
   ::lNoGrayBar := .F.
   ::lLogicDrop := .T. // 1.54
   ::lColDrag := .F.
   ::lLineDrag := .F.
   ::nFreeze := 0
   ::aColumns := {}
   ::nColOrder := 0
   ::cOrderType := ""
   ::lFooting := .F.
   ::nCell := 1
   ::lCellBrw := lCellBrw
   ::lMoveCols := .F.
   ::lLockFreeze := .F.
   ::lCanAppend := lAppend
   ::lCanDelete := lDelete
   ::lAppendMode := .F.
   ::aImages := aImages
   ::aBitmaps := aImages // {} if aImages = array handles !!
   ::nId := ::GetNewId()
   ::cAlias := cAlias
   ::bLine := bLine
   ::aLine := aLine
   ::lAutoEdit := .F.
   ::lAutoSkip := .F.
   ::lIconView := .F.
   ::lCellStyle := .F.
   ::nIconPos := 0
   ::lMChange := .T.
   ::bChange := bChange
   ::bLClicked := bLClick
   ::bLDblClick := bLDblClick
   ::bRClicked := bRClick
   ::aHeaders := aHeaders
   ::aColSizes := aColSizes
   ::aFormatPic := iif( ISARRAY( aPicture ), aPicture, {} )
   ::aJustify := aJust
   ::nLen := 0
   ::lCaptured := .F.
   ::lPainted := .F.
   ::lNoResetPos := .T.
   ::hCursor := hCursor
   ::cMsg := cMsg
   ::lUpdate := lUpdate
   ::bWhen := bWhen
   ::aColSel := aColSel
   ::aActions := aActions
   ::aColors := aTmpColor
   ::nLineStyle := nLineStyle
   ::aSelected := {}
   ::aSuperHead := {}
   ::lFixCaret := .F.
   ::lEditable := lEditable
   ::cFont := cFont
   ::nFontSize := nFontSize
   ::lTransparent := lTransparent
   ::lAutoCol := lAutoCol
   ::bRecLock := iif( lLock, {|| ( ::cAlias )->( RLock() ) }, ::bRecLock )
   ::lEnum := lEnum
   ::lAutoSearch := lAutoSearch
   ::lAutoFilter := lAutoFilter
   ::lEditableHd := lAutoSearch .OR. lAutoFilter
   ::lDrawSpecHd := lEnum .OR. lAutoSearch .OR. lAutoFilter
   ::bUserSearch := iif( lAutoSearch, uUserSearch, ::bUserSearch )
   ::bUserFilter := iif( lAutoFilter, uUserFilter, ::bUserFilter )

   ::SetColor( , aTmpColor )

   ::bBitMapH := &( "{| oBmp | iif( oBmp != Nil, oBmp:hBitMap, 0 )}" )

   IF ! EmptyAlias( ::cAlias ) .AND. AND( ::nDataType, DATATYPE_ARRAY ) == 0 .AND. ;
      AND( ::nDataType, DATATYPE_TEXT ) == 0 .AND. AND( ::nDataType, DATATYPE_ADO ) == 0
      ::lIsDbf := .T.
   ENDIF

   ::lIsArr := AND( ::nDataType, DATATYPE_ARRAY ) != 0

   ::aMsg := LoadMsg()

   IF ValType( ::oWnd:hWnd ) != 'U'
      ::Create( ::cControlName )

      IF ::hFont != NIL
         ::SetFont( ::hFont )
         ::nHeightCell := ::nHeightHead := SBGetHeight( ::hWnd, ::hFont, 0 )
      ELSE
         hFont := InitFont( ::cFont, ::nFontSize ) // SergKis addition
         ::nHeightCell := ::nHeightHead := GetTextHeight( 0, "B", hFont ) + 1
         DeleteObject( hFont )
      ENDIF
      ::nHeightFoot := 0
      ::nHeightSpecHd := iif( ::lEditableHd, ::nHeightHead, 0 )

      ::lVisible = .T.
      ::lValidating := .F.
      IF ::lIsArr // JP 1.66
         ::lFirstPaint := .T.
      ENDIF

      IF aHeaders != NIL .AND. ValType( aHeaders ) == 'A'
         AEval( aHeaders, {| cHeader | lSuperHeader := ( At( '~', cHeader ) != 0 ) .OR. lSuperHeader } )
         IF lSuperHeader
            aSuperHeaders := IdentSuper( aHeaders, Self )
         ENDIF
      ENDIF

      ::Default()

      IF aSuperHeaders != NIL .AND. ValType( aSuperHeaders ) == 'A'
         AEval( aSuperHeaders, {| aHead | ::AddSuperHead( aHead[ 2 ], aHead[ 3 ], aHead[ 1 ], ;
            ::nHeightSuper, { aTmpColor[ 16 ], aTmpColor[ 17 ], aTmpColor[ 15 ] }, ;
            .F., iif( ::hFont != NIL, ::hFont, 0 ) ) } )

         ::SetColor( , aTmpColor )
      ENDIF

      IF HB_ISARRAY( aBrush ) .AND. Len( aBrush ) > 2
         IF ::hBrush != NIL
            DeleteObject( ::hBrush )
         ENDIF
         ::hBrush := CreateSolidBrush( aBrush[ 1 ], aBrush[ 2 ], aBrush[ 3 ] )
      ENDIF
   ELSE
      ::lVisible = .F.
   ENDIF

   ctooltip := ::cToolTip
   IF ValType( ctooltip ) == "B"
      ctooltip := Eval( ctooltip, Self )
   ENDIF

   SetToolTip( ::hWnd, cToolTip, hToolTip )
   TTM_SetMaxTipWidth( hToolTip, ::nToolTipLen )

   IF nValue > 0 .AND. nValue <= ::nLen
      IF Len( ::aColumns ) > 0 // JP 1.59
         ::GoPos( nValue )
      ELSE
         ::nAt := nValue
      ENDIF
      IF nValue > 0 .AND. nValue <= Eval( ::bLogicLen ) // JP 1.59
         Eval( ::bGoToPos, nValue )
      ENDIF
      ::lInitGoTop := .F.
      ::Super:Refresh( .T. )
   ENDIF

RETURN Self

* ============================================================================
* METHOD TSBrowse:AddColumn() Version 9.0 Nov/30/2009
* ============================================================================

METHOD AddColumn( oColumn ) CLASS TSBrowse

   LOCAL nHeight, nAt, cHeading, cRest, nOcurs, ;
      hFont := iif( ::hFont != NIL, ::hFont, 0 )

   DEFAULT ::aColSizes := {}

   IF oColumn:lDefineColumn
      oColumn:DefColor( Self, oColumn:aColors )
      oColumn:DefFont( Self )
   ENDIF

   oColumn:nId := ++::nIdColumn

   IF ::lDrawHeaders
      cHeading := iif( ValType( oColumn:cHeading ) == "B", Eval( oColumn:cHeading, ::nColCount() + 1, Self ), oColumn:cHeading )

      IF ValType( cHeading ) == "C" .AND. ( nAt := At( Chr( 13 ), cHeading ) ) > 0
         nOcurs := 1
         cRest := SubStr( cHeading, nAt + 2 )

         WHILE ( nAt := At( Chr( 13 ), cRest ) ) > 0
            nOcurs++
            cRest := SubStr( cRest, nAt + 2 )
         ENDDO

         nHeight := SBGetHeight( ::hWnd, iif( oColumn:hFontHead != NIL, oColumn:hFontHead, hFont ), 0 )
         nHeight *= ( nOcurs + 1 )

         IF ( nHeight + 1 ) > ::nHeightHead
            ::nHeightHead := nHeight + 1
         ENDIF
      ENDIF
   ENDIF

   IF ValType( oColumn:cFooting ) $ "CB"
      ::lDrawFooters := iif( ::lDrawFooters == NIL, .T., ::lDrawFooters )
      ::lFooting := ::lDrawFooters

      cHeading := iif( ValType( oColumn:cFooting ) == "B", Eval( oColumn:cFooting, ::nColCount() + 1, Self ), oColumn:cFooting )

      IF ValType( cHeading ) == "C" .AND. ( nAt := At( Chr( 13 ), cHeading ) ) > 0
         nOcurs := 1
         cRest := SubStr( cHeading, nAt + 2 )

         WHILE ( nAt := At( Chr( 13 ), cRest ) ) > 0
            nOcurs++
            cRest := SubStr( cRest, nAt + 2 )
         ENDDO

         nHeight := SBGetHeight( ::hWnd, iif( oColumn:hFontFoot != NIL, oColumn:hFontFoot, hFont ), 0 )
         nHeight *= ( nOcurs + 1 )

         IF ( nHeight + 1 ) > ::nHeightHead
            ::nHeightFoot := nHeight + 1
         ENDIF
      ELSE
         nHeight := SBGetHeight( ::hWnd, iif( oColumn:hFontFoot != NIL, oColumn:hFontFoot, hFont ), 0 ) + 1
         IF nHeight > ::nHeightFoot .AND. ::lFooting
            ::nHeightFoot := nHeight
         ENDIF
      ENDIF
   ENDIF

   AAdd( ::aColumns, oColumn )

   IF Len( ::aColSizes ) < Len( ::aColumns )
      AAdd( ::aColSizes, oColumn:nWidth )
   ENDIF

   IF ATail( ::aColSizes ) == NIL .AND. Len( ::aColSizes ) > 0
      ::aColSizes[ Len( ::aColSizes ) ] := oColumn:nWidth
   ENDIF

   IF ::aPostList != NIL // from ComboWBlock function

      IF ATail( ::aColumns ):lComboBox

         IF ValType( ::aPostList[ 1 ] ) == "A"
            ATail( ::aColumns ):aItems := ::aPostList[ 1 ]
            ATail( ::aColumns ):aData := ::aPostList[ 2 ]
            ATail( ::aColumns ):cDataType := ValType( ::aPostList[ 2, 1 ] )
         ELSE
            ATail( ::aColumns ):aItems := AClone( ::aPostList )
         ENDIF
      ENDIF

      ::aPostList := NIL

   ENDIF

RETURN Self

* ============================================================================
* METHOD TSBrowse:FastDrawClear()  by SergKis
* ============================================================================

METHOD FastDrawClear( cCell ) CLASS TSBrowse

   LOCAL oCell, oCol

   DEFAULT cCell := ::nAtPos

   IF ISNUMERIC( cCell ) ; cCell := hb_ntos( cCell )
   ENDIF

   IF ! ::lFastDrawCell

   ELSEIF ISLOGICAL( cCell ) .AND. cCell
      ::aFastDrawCell := hb_Hash()

   ELSEIF ISCHAR( cCell )
      IF "." $ cCell
         oCell := hb_HGetDef( ::aFastDrawCell, cCell, NIL )
         IF oCell != Nil
            hb_HDel( ::aFastDrawCell, cCell )
         ENDIF
      ELSE
         FOR EACH oCol IN ::aColumns
             oCell := hb_HGetDef( ::aFastDrawCell, cCell + "." + hb_ntos( oCol:nId ), NIL )
             IF oCell != Nil
                hb_HDel( ::aFastDrawCell, cCell + "." + hb_ntos( oCol:nId ) )
             ENDIF
         NEXT
      ENDIF

   ENDIF

RETURN Self

* ============================================================================
* METHOD TSBrowse:AppendRow()  by SergKis & Igor Nazarov
* ============================================================================

METHOD AppendRow( lUnlock ) CLASS TSBrowse

   LOCAL cAlias, bAddRec, xRet
   LOCAL lAdd := .F., lUps := .F.
   LOCAL lNoGray := ::lNoGrayBar

   IF ::lIsDbf
      cAlias := ::cAlias
   ENDIF

   IF HB_ISBLOCK( ::bAddBefore )
      xRet := Eval( ::bAddBefore, Self )
      IF HB_ISLOGICAL( xRet ) .AND. ! xRet
         lUps := .T.
      ENDIF
   ENDIF

   DO CASE
   CASE lUps
      // Processing bAddBefore result
   CASE ::lIsDbf

      bAddRec := iif( ! Empty( ::bAddRec ), ::bAddRec, {|| ( cAlias )->( dbAppend() ), ! NetErr() } )

      IF Eval( bAddRec, Self )
         lUps := lAdd := .T.
      ENDIF

   CASE ::lIsArr

      bAddRec := iif( ! Empty( ::bAddRec ), ::bAddRec, {|| AAdd( ::aArray, AClone( ::aDefValue ) ), .T. } )

      IF Eval( bAddRec, Self )
         lUps := lAdd := .T.
      ENDIF

   ENDCASE

   IF HB_ISBLOCK( ::bAddAfter )
      Eval( ::bAddAfter, Self, lAdd )
   ENDIF

   IF lAdd .AND. ::lIsDbf .AND. ! Empty( lUnlock )
      ( cAlias )->( dbUnlock() )
   ENDIF

   IF lUps
      SysRefresh()
      IF ::lIsDbf
         ::lNoGrayBar := .T.
         ::nLen := ( cAlias )->( Eval( ::bLogicLen ) )
         ::Upstable()
         ::lNoGrayBar := lNoGray
      ELSEIF ::lIsArr
         ::nLen := Len( ::aArray )
         ::nAt := ::nLen
         ::nRowPos := ::nRowCount()
      ENDIF
      ::Refresh( .T., .T. )
   ENDIF

   ::SetFocus()

RETURN lAdd

* ============================================================================
* METHOD TSBrowse:bDataEval()  by SergKis
* ============================================================================

METHOD bDataEval( oCol, xVal, nCol ) CLASS TSBrowse

   LOCAL cAlias, lNoAls

   IF ! HB_ISOBJECT( oCol )
      nCol := iif( HB_ISCHAR( oCol ), ::nColumn( oCol ), oCol )
      oCol := ::aColumns[ nCol ]
   ENDIF

   cAlias := oCol:cAlias
   lNoAls := ( Empty( cAlias ) .OR. '->' $ oCol:cField )

   IF xVal == NIL // FieldGet
      DEFAULT nCol := ::nCell
      IF HB_ISBLOCK( oCol:bValue )
         IF lNoAls ; xVal := Eval( oCol:bValue, NIL, Self, nCol, oCol )
         ELSE ; xVal := ( cAlias )->( Eval( oCol:bValue, NIL, Self, nCol, oCol ) )
         ENDIF
      ELSE
         IF lNoAls ; xVal := Eval( oCol:bData )
         ELSE ; xVal := ( cAlias )->( Eval( oCol:bData ) )
         ENDIF
      ENDIF
      IF HB_ISBLOCK( oCol:bDecode )
         IF lNoAls ; xVal := Eval( oCol:bDecode, xVal, Self, nCol, oCol )
         ELSE ; xVal := ( cAlias )->( Eval( oCol:bDecode, xVal, Self, nCol, oCol ) )
         ENDIF
      ENDIF
   ELSE // FieldPut
      DEFAULT nCol := ::nCell
      IF HB_ISBLOCK( oCol:bEncode )
         IF lNoAls ; xVal := Eval( oCol:bEncode, xVal, Self, nCol, oCol )
         ELSE ;      xVal := ( cAlias )->( Eval( oCol:bEncode, xVal, Self, nCol, oCol ) )
         ENDIF
      ENDIF
      IF HB_ISBLOCK( oCol:bValue )
         IF lNoAls ; xVal := Eval( oCol:bValue, xVal, Self, nCol, oCol )
         ELSE ;      xVal := ( cAlias )->( Eval( oCol:bValue, xVal, Self, nCol, oCol ) )
         ENDIF
      ELSE
         IF lNoAls ;               Eval( oCol:bData, xVal )
         ELSE ;      ( cAlias )->( Eval( oCol:bData, xVal ) )
         ENDIF
      ENDIF
   ENDIF

RETURN xVal

* ============================================================================
* METHOD TSBrowse:MoreFields()  by SergKis
* ============================================================================

METHOD MoreFields( nMsg, nWParam ) CLASS TSBrowse

   LOCAL nRet := 0, nCol, nKey
   LOCAL lCtrl := _GetKeyState( VK_CONTROL )
   LOCAL lShift := _GetKeyState( VK_SHIFT )
   LOCAL lAlt := _GetKeyState( VK_MENU )

   IF lCtrl .OR. lShift .OR. lAlt ; RETURN nRet
   ENDIF

   nCol := ::nCell
   nKey := Loword( nWParam )

   IF nMsg == WM_KEYDOWN

      IF nKey == VK_LEFT .AND. nCol > ::nFreeze + 1
         ::nCntKeysLR++
         IF ::nCntKeysLR >= ::nMaxKeysLR
            nRet := 1
            ::nCntKeysLR := 0
         ENDIF

      ELSEIF nKey == VK_RIGHT .AND. nCol < ::nColCount()
         ::nCntKeysLR++
         IF ::nCntKeysLR >= ::nMaxKeysLR
            nRet := 1
            ::nCntKeysLR := 0
         ENDIF
      ENDIF

   ELSEIF nMsg == WM_KEYUP

      ::nCntKeysLR := 0

   ELSEIF nMsg == WM_HSCROLL

      IF ! ::lDontchange .AND. ::lEnabled
         IF nKey == SB_LINEUP ; ::nCntScroll++
         ELSEIF nKey == SB_LINEDOWN ; ::nCntScroll++
         ELSE ; ::nCntScroll := 0
         ENDIF
         IF ::nCntScroll >= ::nMaxScroll
            nRet := 1
            ::nCntScroll := 0
         ENDIF
      ENDIF

   ENDIF

RETURN nRet

* ============================================================================
* METHOD TSBrowse:GetValProp()  by SergKis
* ============================================================================

METHOD GetValProp( xVal, xDef, nCol, nAt ) CLASS TSBrowse

   DEFAULT nCol := ::nCell

   IF HB_ISBLOCK( xVal )
      IF nAt == NIL ; xVal := Eval( xVal, nCol, Self )
      ELSE ; xVal := Eval( xVal, nAt, nCol, Self )
      ENDIF
   ENDIF

   IF xVal == NIL ; xVal := xDef
   ENDIF

RETURN xVal

* ============================================================================
* METHOD TSBrowse:ToolTipSet()  by SergKis
* ============================================================================

METHOD ToolTipSet( nToolTipTime, nToolTipLen ) CLASS TSBrowse

   IF ISNUMERIC( nToolTipLen ) .AND. nToolTipLen > 0
      ::nToolTipLen := nToolTipLen
      TTM_SetMaxTipWidth( hToolTip, ::nToolTipLen )
   ENDIF

   IF ISNUMERIC( nToolTipTime ) .AND. nToolTipTime > 0
      ::nToolTipTime := nToolTipTime
      TTM_SetDelayTime( hToolTip, TTDT_AUTOPOP, ::nToolTipTime * 1000 )
   ENDIF

RETURN NIL

* ============================================================================
* METHOD TSBrowse:hFontGet()  by SergKis
* ============================================================================

METHOD hFontGet( xVal, nCol ) CLASS TSBrowse

   LOCAL xDef := iif( ::hFont == NIL, 0, ::hFont )

   IF HB_ISOBJECT( xVal ) ; xVal := xVal:hFont
   ENDIF

   xVal := ::GetValProp( xVal, xDef, nCol, ::nAt )

   IF HB_ISOBJECT( xVal ) ; xVal := xVal:hFont
   ENDIF

   IF xVal == NIL ; xVal := xDef
   ENDIF

RETURN xVal

* ============================================================================
* METHOD TSBrowse:hFontHeadGet()  by SergKis
* ============================================================================

METHOD hFontHeadGet( xVal, nCol ) CLASS TSBrowse

   LOCAL xDef := iif( ::hFont == NIL, 0, ::hFont )

   IF HB_ISOBJECT( xVal ) ; xVal := xVal:hFontHead
   ENDIF

   xVal := ::GetValProp( xVal, xDef, nCol, 0 )

   IF HB_ISOBJECT( xVal ) ; xVal := xVal:hFontHead
   ENDIF

   IF xVal == NIL ; xVal := xDef
   ENDIF

RETURN xVal

* ============================================================================
* METHOD TSBrowse:hFontFootGet()  by SergKis
* ============================================================================

METHOD hFontFootGet( xVal, nCol ) CLASS TSBrowse

   LOCAL xDef := iif( ::hFont == NIL, 0, ::hFont )

   IF HB_ISOBJECT( xVal ) ; xVal := xVal:hFontFoot
   ENDIF

   xVal := ::GetValProp( xVal, xDef, nCol, 0 )

   IF HB_ISOBJECT( xVal ) ; xVal := xVal:hFontFoot
   ENDIF

   IF xVal == NIL ; xVal := xDef
   ENDIF

RETURN xVal

* ============================================================================
* METHOD TSBrowse:hFontSpcHdGet()  by SergKis
* ============================================================================

METHOD hFontSpcHdGet( xVal, nCol ) CLASS TSBrowse

   LOCAL xDef := iif( ::hFont == NIL, 0, ::hFont )

   IF HB_ISOBJECT( xVal ) ; xVal := xVal:hFontSpcHd
   ENDIF

   xVal := ::GetValProp( xVal, xDef, nCol, 0 )

   IF HB_ISOBJECT( xVal ) ; xVal := xVal:hFontSpcHd
   ENDIF

   IF xVal == NIL ; xVal := xDef
   ENDIF

RETURN xVal

* ============================================================================
* METHOD TSBrowse:hFontSupHdGet()  by SergKis
* ============================================================================

METHOD hFontSupHdGet( nCol, aSuperHead ) CLASS TSBrowse

   LOCAL xDef := iif( ::hFont == NIL, 0, ::hFont )
   LOCAL xVal
   DEFAULT nCol := 1, aSuperHead := ::aSuperHead

   IF nCol > 0 .AND. nCol <= Len( aSuperHead )
      xDef := ::GetValProp( aSuperHead[ 1, 7 ], xDef, 1 )
      xVal := ::GetValProp( aSuperHead[ nCol, 7 ], xDef, nCol )
   ENDIF

   IF xVal == NIL ; xVal := xDef
   ENDIF

RETURN xVal

* ============================================================================
* METHOD TSBrowse:cTextSupHdGet()  by SergKis
* ============================================================================

METHOD cTextSupHdGet( nCol, aSuperHead ) CLASS TSBrowse

   LOCAL xDef := '', xVal
   DEFAULT nCol := 1, aSuperHead := ::aSuperHead

   IF nCol > 0 .AND. nCol <= Len( aSuperHead )
      xVal := ::GetValProp( aSuperHead[ nCol, 3 ], xDef, nCol )
   ENDIF

   IF xVal == NIL ; xVal := xDef
   ENDIF

RETURN xVal

* ============================================================================
* METHOD TSBrowse:nForeSupHdGet()  by SergKis
* ============================================================================

METHOD nForeSupHdGet( nCol, aSuperHead ) CLASS TSBrowse

   LOCAL xDef := ::nClrText, xVal
   DEFAULT nCol := 1, aSuperHead := ::aSuperHead

   IF nCol > 0 .AND. nCol <= Len( aSuperHead )
      xDef := ::GetValProp( aSuperHead[ 1, 4 ], xDef, 1 )
      xVal := ::GetValProp( aSuperHead[ nCol, 4 ], xDef, nCol )
   ENDIF

   IF xVal == NIL ; xVal := xDef
   ENDIF

RETURN xVal

* ============================================================================
* METHOD TSBrowse:nBackSupHdGet()  by SergKis
* ============================================================================

METHOD nBackSupHdGet( nCol, aSuperHead ) CLASS TSBrowse

   LOCAL xDef := ::nClrPane, xVal
   LOCAL nPos := 0

   IF HB_ISNUMERIC( aSuperHead )
      nPos := aSuperHead
      aSuperHead := NIL
   ENDIF

   DEFAULT nCol := 1, aSuperHead := ::aSuperHead

   IF nCol > 0 .AND. nCol <= Len( aSuperHead )
      xDef := ::GetValProp( aSuperHead[ 1, 5 ], xDef, 1 )
      xVal := ::GetValProp( aSuperHead[ nCol, 5 ], xDef, nCol )
   ENDIF

   IF xVal == NIL ; xVal := xDef
   ENDIF

   IF HB_ISARRAY( xVal )
      xVal := ::nClrBackArr( xVal, nCol )
      IF nPos > 0
         IF Empty( xVal[ 1 ] )
            nPos := 2
         ENDIF
         xVal := xVal[ iif( nPos == 1, 1, 2 ) ]
      ENDIF
   ENDIF

RETURN xVal

* ============================================================================
* METHOD TSBrowse:nAlignSupHdGet()  by SergKis
* ============================================================================

METHOD nAlignSupHdGet( nCol, lHAlign, aSuperHead ) CLASS TSBrowse

   LOCAL xDef := DT_CENTER, xVal, nPos

   DEFAULT nCol := 1, lHAlign := .T., aSuperHead := ::aSuperHead

   IF nCol > 0 .AND. nCol <= Len( aSuperHead )
      nPos := iif( lHAlign, 12, 13 )
      xDef := ::GetValProp( aSuperHead[ 1, nPos ], xDef, 1 )
      xVal := ::GetValProp( aSuperHead[ nCol, nPos ], xDef, nCol )
   ENDIF

   IF xVal == NIL ; xVal := xDef
   ENDIF

RETURN xVal

* ============================================================================
* METHOD TSBrowse:hFontSupHdSet()  by SergKis
* ============================================================================

METHOD hFontSupHdSet( nCol, uFont )

   DEFAULT nCol := 1

   IF nCol > 0 .AND. nCol <= Len( ::aSuperHead )
      ::aSuperHead[ nCol, 7 ] := uFont
   ENDIF

RETURN NIL

* ============================================================================
* METHOD TSBrowse:cTextSupHdSet()  by SergKis
* ============================================================================

METHOD cTextSupHdSet( nCol, cText )

   DEFAULT nCol := 1

   IF nCol > 0 .AND. nCol <= Len( ::aSuperHead )
      ::aSuperHead[ nCol, 3 ] := cText
   ENDIF

RETURN NIL

* ============================================================================
* METHOD TSBrowse:nForeSupHdSet()  by SergKis
* ============================================================================

METHOD nForeSupHdSet( nCol, nClrText )

   DEFAULT nCol := 1

   IF nCol > 0 .AND. nCol <= Len( ::aSuperHead )
      ::aSuperHead[ nCol, 4 ] := nClrText
   ENDIF

RETURN NIL

* ============================================================================
* METHOD TSBrowse:nBackSupHdSet()  by SergKis
* ============================================================================

METHOD nBackSupHdSet( nCol, nClrPane )

   DEFAULT nCol := 1

   IF nCol > 0 .AND. nCol <= Len( ::aSuperHead )
      ::aSuperHead[ nCol, 5 ] := nClrPane
   ENDIF

RETURN NIL

* ============================================================================
* METHOD TSBrowse:nAlignSupHdSet()  by SergKis
* ============================================================================

METHOD nAlignSupHdSet( nCol, lHAlign, nHAlign )

   LOCAL nPos
   DEFAULT nCol := 1, lHAlign := .T.

   IF nCol > 0 .AND. nCol <= Len( ::aSuperHead )
      nPos := iif( lHAlign, 12, 13 )
      ::aSuperHead[ nCol, nPos ] := nHAlign
   ENDIF

RETURN NIL

* ============================================================================
* METHOD TSBrowse:GetDeltaLen()  by SergKis
* ============================================================================

METHOD GetDeltaLen( nCol, nStartCol, nMaxWidth, aColSizes ) CLASS TSBrowse

   LOCAL nDeltaLen := 0

   IF ::lAdjColumn .AND. nCol < Len( ::aColumns )
      IF aColSizes[ nCol ] == NIL
         aColSizes[ nCol ] := 0
      ENDIF
      IF aColSizes[ nCol + 1 ] == NIL
         aColSizes[ nCol + 1 ] := 0
      ENDIF
      IF ( nStartCol + aColSizes[ nCol ] + aColSizes[ nCol + 1 ] ) > nMaxWidth
         nDeltaLen := nMaxWidth - ( nStartCol + aColSizes[ nCol ] )
      ENDIF
   ENDIF

RETURN nDeltaLen

* ============================================================================
* METHOD TSBrowse:nAlignGet()  by SergKis
* ============================================================================

METHOD nAlignGet( xVal, nCol, xDef ) CLASS TSBrowse

RETURN ::GetValProp( xVal, hb_defaultValue( xDef, DT_LEFT ), nCol )

* ============================================================================
* METHOD TSBrowse:nColorGet()  by SergKis
* ============================================================================

METHOD nColorGet( xVal, nCol, nAt, lPos ) CLASS TSBrowse

   LOCAL xDef := ::nClrPane
   LOCAL nPos := 0

   IF lPos != NIL
      nPos := iif( Empty( lPos ), 2, 1 )
   ENDIF

   xVal := ::GetValProp( xVal, xDef, nCol, nAt )

   IF ValType( xVal ) == "A"
      xVal := ::nClrBackArr( xVal, nCol, nAt )
      IF nPos > 0
         xVal := xVal[ nPos ]
      ENDIF
   ENDIF

RETURN xVal

* ============================================================================
* METHOD TSBrowse:cPictureGet()  by SergKis
* ============================================================================

METHOD cPictureGet( xVal, nCol ) CLASS TSBrowse

   IF HB_ISOBJECT( xVal ) ; xVal := xVal:cPicture
   ENDIF

RETURN ::GetValProp( xVal, NIL, nCol, ::nAt )

* ============================================================================
* METHOD TSBrowse:nClrBackArr()  by SergKis
* ============================================================================

METHOD nClrBackArr( aClrBack, nCol, nAt ) CLASS TSBrowse

   LOCAL nClrBack, nClrTo
   DEFAULT nCol := ::nCell

   nClrBack := aClrBack[ 1 ]
   nClrTo := aClrBack[ 2 ]

   IF HB_ISBLOCK( nClrTo )
      IF nAt == NIL ; nClrTo := Eval( nClrTo, nCol, Self )
      ELSE ; nClrTo := Eval( nClrTo, nAt, nCol, Self )
      ENDIF
   ENDIF

   IF HB_ISBLOCK( nClrBack )
      IF nAt == NIL ; nClrBack := Eval( nClrBack, nCol, Self )
      ELSE ; nClrBack := Eval( nClrBack, nAt, nCol, Self )
      ENDIF
   ENDIF

   IF nAt != NIL .AND. nCol == 1 .AND. ! Empty( ::hBmpCursor )
      nClrTo *= -1
   ENDIF

RETURN { nClrBack, nClrTo }

* ============================================================================
* METHOD TSBrowse:AddSuperHead() Version 9.0 Nov/30/2009
* ============================================================================

METHOD AddSuperHead( nFromCol, nToCol, uHead, nHeight, aColors, l3dLook, uFont, uBitMap, lAdjust, lTransp, ;
      lNoLines, nHAlign, nVAlign, nBmpMask ) CLASS TSBrowse // SergKis 11.11.21

   LOCAL cHeading, nAt, nLheight, nOcurs, cRest, nLineStyle, nClrText, nClrBack, nClrLine, ;
      hFont := iif( ::hFont != NIL, ::hFont, 0 )

   DEFAULT lAdjust := .F., ;
      l3DLook := ::aColumns[ nFromCol ]:l3DLookHead, ;
      nHAlign := DT_CENTER, ;
      nVAlign := DT_CENTER, ;
      lTransp := .T., ;
      uHead := "", ;
      uFont := ::hFontSupHd, ;
      nBmpMask := 0x008800C6      // SRCAND

   IF ValType( nFromCol ) == "C"
      nFromCol := ::nColumn( nFromCol )
   ENDIF

   IF ValType( nToCol ) == "C"
      nToCol := ::nColumn( nToCol )
   ENDIF

   hFont := iif( ! Empty( uFont ), iif( ValType( uFont ) == "O", uFont:hFont, uFont ), hFont )

   IF ! Empty( ::aColumns ) .AND. Empty( hFont )
      hFont := iif( ValType( ::aColumns[ nFromCol ]:hFontHead ) == "O", ::aColumns[ nFromCol ]:hFontHead, ;
         iif( ::aColumns[ nFromCol ]:hFontHead != NIL, ::aColumns[ nFromCol ]:hFontHead, hFont ) )
   ENDIF

   IF ValType( aColors ) == "A"
      ASize( aColors, 3 )

      IF ! Empty( ::aColumns )
         nClrText := iif( aColors[ 1 ] != NIL, aColors[ 1 ], ::aColumns[ nFromCol ]:nClrHeadFore )
         nClrBack := iif( aColors[ 2 ] != NIL, aColors[ 2 ], ::aColumns[ nFromCol ]:nClrHeadBack )
         nClrLine := iif( aColors[ 3 ] != NIL, aColors[ 3 ], ::nClrLine )
      ELSE
         nClrText := iif( aColors[ 1 ] != NIL, aColors[ 1 ], ::nClrHeadFore )
         nClrBack := iif( aColors[ 2 ] != NIL, aColors[ 2 ], ::nClrHeadBack )
         nClrLine := iif( aColors[ 3 ] != NIL, aColors[ 3 ], ::nClrLine )
      ENDIF
   ELSE
      IF ! Empty( ::aColumns )
         nClrText := ::aColumns[ nFromCol ]:nClrHeadFore
         nClrBack := ::aColumns[ nFromCol ]:nClrHeadBack
         nClrLine := ::nClrLine
      ELSE
         nClrText := ::nClrHeadFore
         nClrBack := ::nClrHeadBack
         nClrLine := ::nClrLine
      ENDIF
   ENDIF

   IF uBitMap != NIL .AND. ValType( uBitMap ) != "L"

      DEFAULT lNoLines := .T.
      cHeading := iif( ValType( uBitMap ) == "B", Eval( uBitMap ), uBitMap )
      cHeading := iif( ValType( cHeading ) == "O", Eval( ::bBitMapH, cHeading ), cHeading )
      IF Empty( cHeading )
         MsgStop( "The resource could not be found for TBROWSE !" + CRLF + CRLF + ProcNL(0) + CRLF + ProcNL(1), "Error" )
         RETURN NIL
      ENDIF
      nLHeight := SBmpHeight( cHeading )

      IF nHeight != NIL
         IF nHeight < nLHeight .AND. lAdjust
            nLHeight := nHeight
         ELSEIF nHeight > nLheight
            nLHeight := nHeight
         ENDIF
      ENDIF

      IF ( nLHeight + 1 ) > ::nHeightSuper
         ::nHeightSuper := nLHeight + 1
      ENDIF

   ELSE
      uBitMap := NIL
   ENDIF

   cHeading := iif( ValType( uHead ) == "B", Eval( uHead ), uHead )

   DO CASE

   CASE ValType( cHeading ) == "C" .AND. ( nAt := At( Chr( 13 ), cHeading ) ) > 0

      DEFAULT lNoLines := .F.

      nOcurs := 1
      cRest := SubStr( cHeading, nAt + 2 )

      WHILE ( nAt := At( Chr( 13 ), cRest ) ) > 0
         nOcurs++
         cRest := SubStr( cRest, nAt + 2 )
      ENDDO

      nLheight := SBGetHeight( ::hWnd, hFont, 0 )
      nLheight *= ( nOcurs + 1 )
      nLheight := iif( nHeight == NIL .OR. nLheight > nHeight, nLheight, nHeight )

      IF ( nLheight + 1 ) > ::nHeightSuper
         ::nHeightSuper := nLHeight + 1
      ENDIF

   CASE ValType( cHeading ) == "C"

   DEFAULT lNoLines := .F.

      nLheight := SBGetHeight( ::hWnd, hFont, 0 )
      nLheight := iif( nHeight == NIL .OR. nLheight > nHeight, nLheight, nHeight )

      IF ( nLheight + 1 ) > ::nHeightSuper
         ::nHeightSuper := nLHeight + 1
      ENDIF

   CASE ValType( cHeading ) == "N" .OR. ValType( cHeading ) == "O"

      DEFAULT lNoLines := .T.
      uBitMap := uHead

      IF ValType( cHeading ) == "O"
         uHead := Eval( ::bBitMapH, cHeading )
      ENDIF

      nLheight := SBmpHeight( uHead )
      uHead := ""

      IF nHeight != NIL
         IF nHeight < nLHeight .AND. lAdjust
            nLheight := nHeight
         ELSEIF nHeight > nLheight
            nLheight := nHeight
         ENDIF
      ENDIF

      IF ( nLheight + 1 ) > ::nHeightSuper
         ::nHeightSuper := nLHeight + 1
      ENDIF

   ENDCASE

   nLineStyle := iif( lNoLines, 0, 1 )
                       //   1        2       3       4         5         6       7       8        9        10
   AAdd( ::aSuperHead, { nFromCol, nToCol, uHead, nClrText, nClrBack, l3dLook, hFont, uBitMap, lAdjust, nLineStyle, ;
      nClrLine, nHAlign, nVAlign, lTransp, nBmpMask } ) // SergKis 11.11.21
      //  11       12       13       14       15
   ::lDrawSuperHd := ( Len( ::aSuperHead ) > 0 )

RETURN Self

* ============================================================================
* METHOD TSBrowse:BiClr() Version 9.0 Nov/30/2009
* ============================================================================

METHOD BiClr( uClrOdd, uClrPair ) CLASS TSBrowse

   uClrOdd := iif( ValType( uClrOdd ) == "B", Eval( uClrOdd, Self ), ;
      uClrOdd )

   uClrPair := iif( ValType( uClrPair ) == "B", Eval( uClrPair, Self ), ;
      uClrPair )

RETURN iif( ::nAt % 2 > 0, uClrOdd, uClrPair )

* ============================================================================
* METHOD TSBrowse:ChangeFont() Version 9.0 Nov/30/2009
* ============================================================================

METHOD ChangeFont( hFont, nColumn, nLevel ) CLASS TSBrowse

   LOCAL nEle, ;
      lDrawFooters := iif( ::lDrawFooters != NIL, ::lDrawFooters, .F. )

   DEFAULT nColumn := 0 // all columns

   IF nColumn == 0

      IF nLevel == NIL

         FOR nEle := 1 TO Len( ::aColumns )
            ::aColumns[ nEle ]:hFont := hFont
         NEXT

         IF ::lDrawHeaders

            FOR nEle := 1 TO Len( ::aColumns )
               ::aColumns[ nEle ]:hFontHead := hFont
            NEXT

         ENDIF

         IF ::lFooting .AND. lDrawFooters

            FOR nEle := 1 TO Len( ::aColumns )
               ::aColumns[ nEle ]:hFontFoot := hFont
            NEXT

         ENDIF

         IF ::lDrawSuperHd

            FOR nEle := 1 TO Len( ::aSuperHead )
               ::aSuperHead[ nEle, 7 ] := hFont
            NEXT

         ENDIF

      ELSE

         DO CASE

         CASE nLevel == 1 // nLevel 1 = Cells

            FOR nEle := 1 TO Len( ::aColumns )
               ::aColumns[ nEle ]:hFont := hFont
            NEXT

         CASE nLevel == 2 .AND. ::lDrawHeaders // nLevel 2 = Headers

            FOR nEle := 1 TO Len( ::aColumns )
               ::aColumns[ nEle ]:hFontHead := hFont
            NEXT

         CASE nLevel == 3 .AND. ::lFooting .AND. lDrawFooters // nLevel 3 = Footers

            FOR nEle := 1 TO Len( ::aColumns )
               ::aColumns[ nEle ]:hFontFoot := hFont
            NEXT

         CASE nLevel == 4 .AND. ::lDrawSuperHd // nLevel 4 = SuperHeaders

            FOR nEle := 1 TO Len( ::aSuperHead )
               ::aSuperHead[ nEle, 7 ] := hFont
            NEXT

         ENDCASE

      ENDIF

   ELSE

      IF nLevel == NIL

         ::aColumns[ nColumn ]:hFont := hFont

         IF ::lDrawHeaders
            ::aColumns[ nColumn ]:hFontHead := hFont
         ENDIF

         IF ::lFooting .AND. lDrawFooters
            ::aColumns[ nColumn ]:hFontFoot := hFont
         ENDIF

         IF ::lDrawSuperHd
            ::aSuperHead[ nColumn, 7 ] := hFont
         ENDIF

      ELSE

         DO CASE
         CASE nLevel == 1 // nLevel 1 = Cells
            ::aColumns[ nColumn ]:hFont := hFont
         CASE nLevel == 2 .AND. ::lDrawHeaders // nLevel 2 = Headers
            ::aColumns[ nColumn ]:hFontHead := hFont
         CASE nLevel == 3 .AND. ::lFooting .AND. lDrawFooters // nLevel 3 = Footers
            ::aColumns[ nColumn ]:hFontFoot := hFont
         CASE nLevel == 4 .AND. ::lDrawSuperHd // nLevel 4 = SuperHeaders
            ::aSuperHead[ nColumn, 7 ] := hFont
         ENDCASE
      ENDIF

   ENDIF

   IF ::lPainted
      SetHeights( Self )
      ::Refresh( .F. ,, .F. )
   ENDIF

RETURN Self

* ============================================================================
* METHOD TSBrowse:DbSkipper() Version 9.0 Nov/30/2009
* ============================================================================

METHOD DbSkipper( nToSkip ) CLASS TSBrowse

   LOCAL nSkipped := 0, ;
      nRecNo := ( ::cAlias )->( RecNo() )

   DEFAULT nToSkip := 0, ;
      ::nAt := 1

   IF nToSkip == 0 .OR. ( ::cAlias )->( LastRec() ) == 0
      // ( ::cAlias )->( dbSkip( 0 ) )
   ELSEIF nToSkip > 0 .AND. !( ::cAlias )->( Eof() ) // going down

      WHILE nSkipped < nToSkip

         ( ::cAlias )->( dbSkip( 1 ) )

         IF ::bFilter != NIL
            While ! Eval( ::bFilter ) .AND. !( ::cAlias )->( Eof() )
               ( ::cAlias )->( dbSkip( 1 ) )
            ENDDO
         ENDIF

         IF ( ::cAlias )->( Eof() )

            IF ::lAppendMode
               nSkipped++
            ELSE
               ( ::cAlias )->( dbSkip( -1 ) )
            ENDIF

            EXIT
         ENDIF

         nSkipped++
      ENDDO

   ELSEIF nToSkip < 0 .AND. !( ::cAlias )->( Bof() ) // going up

      WHILE nSkipped > nToSkip

         ( ::cAlias )->( dbSkip( -1 ) )

         IF ::bFilter != NIL .AND. !( ::cAlias )->( Bof() )
            While ! Eval( ::bFilter ) .AND. !( ::cAlias )->( Bof() )
               ( ::cAlias )->( dbSkip( -1 ) )
            ENDDO

            IF ( ::cAlias )->( Bof() )
               ( ::cAlias )->( dbGoto( nRecNo ) )
               RETURN nSkipped
            ENDIF
         ENDIF

         IF ( ::cAlias )->( Bof() )
            ( ::cAlias )->( dbGoTop() )
            EXIT
         ENDIF

         nSkipped--
      ENDDO

   ENDIF

   ::nAt += nSkipped

RETURN nSkipped

* ============================================================================
* METHOD TSBrowse:Default() Version 9.0 Nov/30/2009
* ============================================================================

METHOD Default() CLASS TSBrowse

   LOCAL nI, nTemp, nElements, aFields, nHeight, nMin, nMax, nPage, bBlock, aJustify, cBlock, nTxtWid, ;
      nWidth := 0, ;
      cAlias := Alias(), ;
      nMaxWidth := ::nWidth() - iif( ::oVScroll != NIL, GetSysMetrics( 2 ), 0 ), ;
      hFont := iif( ::oFont != NIL, ::oFont:hFont, 0 ), ;
      nAdj := ::nAdjColumn, ;
      lAutocol := iif( ::lAutoCol == NIL, .F., ::lAutocol )

   DEFAULT ::aHeaders := {}, ;
      ::aColSizes := {}, ;
      ::nOldCell := 1, ;
      ::lIsTxt := AND( ::nDataType, DATATYPE_TEXT ) != 0
      ::lIsArr := AND( ::nDataType, DATATYPE_ARRAY ) != 0

   IF Empty( ::nDataType ) .OR. ::bLine == NIL .AND. Empty( ::aColumns )

      IF Empty( ::cAlias )
         ::cAlias := cAlias
      ELSE
         cAlias := ::cAlias
      ENDIF

      IF ! EmptyAlias( ::cAlias )

         IF ! ::lIsArr .AND. ! ::lIsTxt .AND. lAutoCol
            IF ::lIsDbf
               ::nDataType := DATATYPE_RDD
               IF Empty( ::nLen )
                  ::SetDbf()
               ENDIF
               ::LoadFields()
            ELSEIF AND( ::nDataType, DATATYPE_ADO ) != 0
               ::nDataType := DATATYPE_ADO
               IF Empty( ::nLen )
                  ::SetRecordSet()
               ENDIF
               IF ::nLen == 0
                  MsgStop( "Cannot create a TBROWSE object for an empty ADO dataset." )
               ELSE
                  ::LoadRecordSet()
               ENDIF
            ENDIF
         ENDIF

         IF ::lIsArr
            ::nDataType := DATATYPE_ARRAY
            IF Len( ::cArray ) == 0 .AND. ValType( ::aHeaders ) == "A"
               ::cArray := Array( 1, Len( ::aHeaders ) )
               AEval( ::aHeaders, {| cHead, nEle | ::cArray[ 1, nEle ] := "???", HB_SYMBOL_UNUSED( cHead ) } )
               ::lPhantArrRow := .T.
            ENDIF
            IF Len( ::cArray ) > 0
               IF ValType( ::cArray[ 1 ] ) != "A"
                  ::SetItems( ::cArray )
               ELSE
                  ::SetArray( ::cArray, .T. )
               ENDIF
            ENDIF
            IF ::lPhantArrRow
               ::Deleterow( .T. )
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   ::lFirstPaint := .F.

   IF ::bLine != NIL .AND. Empty( ::aColumns )

      DEFAULT nElements := Len( Eval( ::bLine ) )

      aJustify := AFill( Array( nElements ), 0 )

      IF Len( ::aHeaders ) < nElements

         ::aHeaders := Array( nElements )
         FOR nI := 1 TO nElements
            IF At( '->', FieldName( nI ) ) == 0
               ::aHeaders[ nI ] := ( cAlias )->( FieldName( nI ) )
            ELSE
               ::aHeaders[ nI ] := FieldName( nI )
            ENDIF
         NEXT
      ENDIF

      IF ValType( ::aColSizes ) == "B"
         ::aColSizes := Eval( ::aColSizes )
      ENDIF

      aFields := Eval( ::bLine )

      IF Len( ::GetColSizes() ) < nElements
         ::aColSizes := AFill( Array( nElements ), 0 )

         nTxtWid := SBGetHeight( ::hWnd, hFont, 1 )

         FOR nI := 1 TO nElements
            ::aColSizes[ nI ] := iif( ValType( aFields[ nI ] ) != "C", 16, ; // Bitmap handle
            ( nTxtWid * Max( Len( ::aHeaders[ nI ] ), Len( aFields[ nI ] ) ) + 1 ) )
         NEXT

      ENDIF

      FOR nI := 1 TO nElements

         IF ValType( aFields[ nI ] ) == "N" .OR. ValType( aFields[ nI ] ) == "D"
            aJustify[ nI ] := 2
         ELSEIF ValType( aFields[ nI ] ) == "B"

            IF ValType( Eval( aFields[ nI ] ) ) == "N" .OR. ValType( Eval( aFields[ nI ] ) ) == "D"

               aJustify[ nI ] := 2
            ELSE
               aJustify[ nI ] := 0
            ENDIF
         ELSE
            aJustify[ nI ] := 0
         ENDIF
      NEXT

      ASize( ::aFormatPic, nElements ) // make sure they match sizes

      FOR nI := 1 TO nElements

         bBlock := iif( ValType( Eval( ::bLine )[ nI ] ) == "B", Eval( ::bLine )[ nI ], MakeBlock( Self, nI ) )
         cBlock := iif( ValType( Eval( ::bLine )[ nI ] ) == "B", ::aLine[ nI ], ;
            "{||" + cValToChar( ::aLine[ nI ] ) + "}" )
         ::AddColumn( TSColumn():New( ::aHeaders[ nI ], bBlock, ::aFormatPic[ nI ], { ::nClrText, ::nClrPane, ;
            ::nClrHeadFore, ::nClrHeadBack, ::nClrFocuFore, ::nClrFocuBack }, ;
            { aJustify[ nI ], 1 }, ::aColSizes[ nI ],, ;
            ::lEditable .OR. ValType( Eval( ::bLine )[ nI ] ) == "B",,,,,,, ;
            5,, { .F., .T. },, Self, cBlock ) )

         IF At( '->', ::aLine[ nI ] ) == 0
            ATail( ::aColumns ):cData := ::cAlias + "->" + ::aLine[ nI ]
         ELSE
            ATail( ::aColumns ):cData := ::aLine[ nI ]
         ENDIF

      NEXT
   ENDIF

   ::lIsDbf := ! EmptyAlias( ::cAlias ) .AND. ! ::lIsArr .AND. ! ::lIsTxt .AND. AND( ::nDataType, DATATYPE_ADO ) == 0

   IF ! Empty( ::aColumns )
      ASize( ::aColSizes, Len( ::aColumns ) ) // make sure they match sizes
   ENDIF

   IF ::lIsDbf
      ::nDataType := DATATYPE_RDD
      IF Empty( ::nLen )
         ::SetDbf()
      ENDIF
   ENDIF

   // rebuild build the aColSize, it's needed to Horiz Scroll etc
   // and expand selected column to flush table window right

   FOR nI := 1 TO Len( ::aColumns )

      nTemp := ( ::aColSizes[ nI ] := iif( ::aColumns[ nI ]:lVisible, ::aColumns[ nI ]:nWidth, 0 ) ) // JP 1.58

      IF ! Empty( nAdj ) .AND. ( nWidth + nTemp > nMaxWidth )

         IF nAdj < nI
            ::aColumns[ nAdj ]:nWidth := ::aColSizes[ nAdj ] += ( nMaxWidth - nWidth )
         ENDIF

         nAdj := 0
      ENDIF

      nWidth += nTemp

      IF ::lIsDbf .AND. ! Empty( ::aColumns[ nI ]:cOrder ) .AND. ! ::aColumns[ nI ]:lEdit

         IF ::nColOrder == 0
            ::SetOrder( nI )
         ENDIF

         ::aColumns[ nI ]:lIndexCol := .T.
      ENDIF

      IF ValType( ::aColumns[ nI ]:cFooting ) $ "CB" // informs browse that it has footings to display

         ::lDrawFooters := iif( ::lDrawFooters == NIL, .T., ::lDrawFooters )
         ::lFooting := ::lDrawFooters
         nHeight := SBGetHeight( ::hWnd, iif( ::aColumns[ nI ]:hFontFoot != NIL, ;
            ::aColumns[ nI ]:hFontFoot, hFont ), 0 ) + 1
         IF nHeight > ::nHeightFoot .AND. ::lFooting
            ::nHeightFoot := nHeight
         ENDIF
      ENDIF

   NEXT

   // now catch the odd-ball where last column doesn't fill box
   IF ! Empty( nAdj ) .AND. nWidth < nMaxWidth .AND. nAdj < nI
      ::aColumns[ nAdj ]:nWidth := ::aColSizes[ nAdj ] += ( nMaxWidth - nWidth )
   ENDIF

   IF ::bLogicLen != NIL
      ::nLen := iif( ::lIsDbf, ( ::cAlias )->( Eval( ::bLogicLen ) ), Eval( ::bLogicLen ) )
   ENDIF

   IF ! ::lNoVScroll
      IF ::nLen <= ::nRowCount()
         nMin := nMax := 0
      ELSE
         nMax := Min( ::nLen, MAX_POS )
         nPage := Min( ::nRowCount(), ::nLen )
      ENDIF

      ::oVScroll := TSBScrlBar():WinNew( nMin, nMax, nPage, .T., Self )
   ENDIF

   IF ! Empty( ::cAlias ) .AND. ::cAlias != "ADO_" .AND. ::bKeyNo != NIL
      ::ResetVScroll( .T. )
   ENDIF

   IF ! ::lNoHScroll

      IF ! Empty( ::cAlias ) .AND. ::lIsTxt .AND. ::oTxtFile != NIL
         nTxtWid := Max( 1, GetTextWidth( 0, "B", hFont ) )
         nMin := 1
         nMax := ::oTxtFile:nMaxLineLength - Int( nMaxWidth / nTxtWid )
         ::oHScroll := TSBScrlBar():WinNew( nMin, nMax,, .F., Self )
      ELSEIF ::lCellBrw
         nMin := Min( 1, Len( ::aColumns ) )
         nMax := Len( ::aColumns )
         ::oHScroll := TSBScrlBar():WinNew( nMin, nMax,, .F., Self )
      ENDIF
   ENDIF

   FOR nI := 1 TO Len( ::aColumns )

      IF ::aColumns[ nI ]:hFont == NIL
         ::aColumns[ nI ]:hFont := ::hFont
      ENDIF

      IF ::aColumns[ nI ]:hFontHead == NIL
         ::aColumns[ nI ]:hFontHead := ::hFont
      ENDIF

      IF ::aColumns[ nI ]:hFontFoot == NIL
         ::aColumns[ nI ]:hFontFoot := ::hFont
      ENDIF

      IF ::lLockFreeze .AND. ::nFreeze >= nI
         ::aColumns[ nI ]:lNoHilite := .T.
      ENDIF

   NEXT

   ::nHeightHead := iif( ::lDrawHeaders, ::nHeightHead, 0 )
   ::nHeightFoot := iif( ::lFooting .AND. ::lDrawFooters, ::nHeightFoot, 0 )
   ::nHeightSpecHd := iif( ::nHeightSpecHd == 0, SBGetHeight( ::hWnd, hFont, 0 ), ::nHeightSpecHd )
   ::nHeightSpecHd := iif( ::lDrawSpecHd, ::nHeightSpecHd, 0 )

   IF ! ::lNoVScroll
      nPage := Min( ::nRowCount(), ::nLen )
      ::oVScroll:SetPage( nPage, .T. )
   ENDIF

   IF ! ::lNoHScroll .AND. ::oHScroll != NIL
      nPage := 1
      ::oHScroll:SetPage( nPage, .T. )
   ENDIF

   IF Len( ::aColumns ) > 0
      ::HiliteCell( Max( ::nCell, ::nFreeze + 1 ) )
   ENDIF

   ::nOldCell := ::nCell
   ::nLapsus := Seconds()

   IF ::nLen == 0
      ::nLen := iif( ::bLogicLen == NIL, Eval( ::bLogicLen := {|| ( cAlias )->( LastRec() ) } ), Eval( ::bLogicLen ) )
   ENDIF

RETURN Self

* ============================================================================
* METHOD TSBrowse:Del() Version 9.0 Nov/30/2009
// Only for ARRAY browse. (ListBox behavior)
* ============================================================================

METHOD Del( nItem ) CLASS TSBrowse

   DEFAULT nItem := ::nAt

   IF ! ::lIsArr
      RETURN Self
   ENDIF

   hb_ADel( ::aArray, nItem, .T. )

   ::nLen := Eval( ::bLogicLen )
   ::nAt := Min( nItem, ::nLen )

   /* added by Pierpaolo Martinello 29/04/2019 */
   ::lHasChanged := .T.

   ::Refresh( .T., .T. )

RETURN Self

* ============================================================================
* METHOD TSBrowse:DelColumn() Version 9.0 Nov/30/2009
* ============================================================================

METHOD DelColumn( nPos ) CLASS TSBrowse

   LOCAL oCol, nMin, nMax, nI, ;
      nLen := Len( ::aSuperHead )

   DEFAULT nPos := 1

   IF Len( ::aColumns ) == 1 // cannot delete last column
      RETURN NIL // ... or Nil if last column
   ENDIF

   IF ValType( nPos ) == "C"
      nPos := ::nColumn( nPos ) // 23.07.2015
   ENDIF

   IF nPos < 1
      nPos := 1
   ELSEIF nPos > Len( ::aColumns )
      nPos := Len( ::aColumns )
   ENDIF

   oCol := ::aColumns[ nPos ]
   hb_ADel( ::aColumns, nPos, .T. )
   hb_ADel( ::aColSizes, nPos, .T. )
   hb_ADel( ::aHeaders, nPos, .T. )
   hb_ADel( ::aFormatPic, nPos, .T. )
   hb_ADel( ::aJustify, nPos, .T. )
   hb_ADel( ::aDefValue, nPos, .T. )

   IF ::lSelector .AND. nPos == 1
      RETURN NIL
   ENDIF

   IF ::nColOrder == nPos // deleting a ::SetOrder() column
      ::nColOrder := 0 // to avoid runtime error
      ::cOrderType := ""
   ELSEIF ::nColOrder != 0 .AND. ::nColOrder > nPos .AND. ::nColOrder <= Len( ::aColumns )
      ::nColOrder--
   ENDIF

   IF ::nCell > Len( ::aColSizes )
      IF ! ::IsColVisible( ::nCell - 1 )
         ::GoLeft()
      ELSE
         ::nCell--
      ENDIF
   ENDIF

   ::HiliteCell( ::nCell ) // make sure we have a hilited cell

   IF ! ::lNoHScroll
      nMin := Min( 1, Len( ::aColumns ) )
      nMax := Len( ::aColumns )
      ::oHScroll := TSBScrlBar():WinNew( nMin, nMax,, .F., Self )
      ::oHScroll:SetRange( 1, Len( ::aColumns ) )
      ::oHScroll:SetPage( 1, .T. )

      IF ::nCell == Len( ::aColSizes )
         ::oHScroll:GoBottom()
      ELSE
         ::oHScroll:SetPos( ::nCell )
      ENDIF
   ENDIF

   IF ! Empty( ::aSuperHead )
      FOR nI := 1 TO nLen
         IF nPos >= ::aSuperHead[ nI, 1 ] .AND. nPos <= ::aSuperHead[ nI, 2 ]

            ::aSuperHead[ nI, 2 ] --

            IF ::aSuperHead[ nI, 2 ] < ::aSuperHead[ nI, 1 ]
               ASize( ADel( ::aSuperHead, nI ), Len( ::aSuperHead ) - 1 )
            ENDIF

         ELSEIF nPos < ::aSuperHead[ nI, 1 ]
            ::aSuperHead[ nI, 1 ] --
            ::aSuperHead[ nI, 2 ] --
         ENDIF
      NEXT
   ENDIF

   ::SetFocus()
   ::Refresh( .F. ,, .F. )

RETURN oCol

* ============================================================================
* METHOD TSBrowse:DeleteRow() Version 9.0 Nov/30/2009
* ============================================================================

METHOD DeleteRow( lAll, lUpStable ) CLASS TSBrowse

   LOCAL lRecall, nAt, nRowPos, nRecNo, lRefresh, cAlias, lEval, uTemp, cMsg

   IF ( ! ::lCanDelete .OR. ::nLen == 0 ) .AND. ! ::lPhantArrRow // Modificado por Carlos - Erro Keychar
      RETURN .F.
   ENDIF

   IF ::lIsDbf
      cAlias := ::cAlias
      nRecNo := ( cAlias )->( RecNo() )
   ENDIF

   lRecall := ! Set( _SET_DELETED )
   DEFAULT lAll := .F., lUpStable := ! lRecall

   IF ! ::lIsTxt

      IF ::lConfirm .AND. ! lAll
         cMsg := iif( ::lIsDbf, ::aMsg[ 37 ], ::aMsg[ 38 ] )
         IF ::lIsDbf
            IF lRecall .AND. ( cAlias )->( Deleted() )
               cMsg := ::aMsg[ 46 ]
            ENDIF
         ENDIF
         IF ! MsgYesNo( cMsg, ::aMsg[ 39 ] )
            RETURN .F.
         ENDIF
      ENDIF

      IF ::lAppendMode
         RETURN .F.
      ENDIF

      ::SetFocus()

      IF ::lIsDbf
         ( cAlias )->( dbGoto( nRecNo ) )
      ENDIF

      DO CASE

      CASE ::lIsDbf

         lEval := .T.

         IF ::bDelete != NIL
            lEval := Eval( ::bDelete, nRecNo, Self )
         ENDIF

         IF ValType( lEval ) == "L" .AND. ! lEval
            RETURN .F.
         ENDIF

         IF !( "SQL" $ ::cDriver )
            IF !( cAlias )->( RLock() )
               MsgStop( ::aMsg[ 40 ], ::aMsg[ 28 ] )
               RETURN .F.
            ENDIF
         ENDIF

         IF ::bDelBefore != NIL
            lEval := Eval( ::bDelBefore, nRecNo, Self )
            IF ValType( lEval ) == "L" .AND. ! lEval
               IF !( "SQL" $ ::cDriver )
                  ( cAlias )->( dbUnlock() )
               ENDIF
               RETURN .F.
            ENDIF
         ENDIF

         IF !( cAlias )->( Deleted() )
            ( cAlias )->( dbDelete() )

            IF ::bDelAfter != NIL
               Eval( ::bDelAfter, nRecNo, Self )
            ENDIF

            IF !( "SQL" $ ::cDriver )
               ( cAlias )->( dbUnlock() )
            ENDIF

         ELSEIF lRecall
            ( cAlias )->( dbRecall() )
            IF !( "SQL" $ ::cDriver )
               ( cAlias )->( dbUnlock() )
            ENDIF
         ENDIF

         ::nLen := ( cAlias )->( Eval( ::bLogicLen ) )

         IF lUpStable
            ( cAlias )->( dbSkip() )
            lRefresh := ( cAlias )->( Eof() )
            ( cAlias )->( dbSkip( -1 ) )
            ::nRowPos -= iif( lRefresh .AND. ;
               !( cAlias )->( Bof() ), 1, 0 )
            ::Refresh( .T. )
         ENDIF

         IF ::lCanAppend .AND. ::nLen == 0
            ::nRowPos := ::nColPos := 1
            ::PostMsg( WM_KEYDOWN, VK_DOWN, nMakeLong( 0, 0 ) )
         ENDIF

         IF ::bPostDel != NIL
            Eval( ::bPostDel, Self )
         ENDIF

         ::lHasChanged := .T.
         ::DrawSelect()

      CASE ::lIsArr

         nAt := ::nAt
         nRowPos := ::nRowPos
         lEval := .T.

         IF ::bDelete != NIL .AND. ! ::lPhantArrRow
            lEval := Eval( ::bDelete, nAt, Self, lAll )
         ENDIF

         IF ValType( lEval ) == "L" .AND. ! lEval
            RETURN .F.
         ENDIF

         IF lAll
            ::aArray := {}
            ::aSelected := {}
            IF ::nColOrder != 0
               ::aColumns[ ::nColOrder ]:cOrder := ""
               ::aColumns[ ::nColOrder ]:lDescend := NIL
               ::nColOrder := 0
            ENDIF
         ELSE
            hb_ADel( ::aArray, nAt, .T. )
            IF ::lCanSelect .AND. Len( ::aSelected ) > 0
               IF ( uTemp := AScan( ::aSelected, nAt ) ) > 0
                  hb_ADel( ::aSelected, uTemp, .T. )
               ENDIF
               AEval( ::aSelected, {| x, nEle | ::aSelected[ nEle ] := iif( x > nAt, x - 1, x ) } )
            ENDIF
         ENDIF

         IF Len( ::aArray ) == 0
            ::aArray := { AClone( ::aDefValue ) }
            ::lPhantArrRow := .T.
            IF ::aArray[ 1, 1 ] == NIL
               hb_ADel( ::aArray[ 1 ], 1, .T. )
            ENDIF
         ENDIF

         IF ::bPostDel != NIL
            Eval( ::bPostDel, Self )
         ENDIF

         ::lHasChanged := .T.
         ::nLen := Len( ::aArray )
         ::nAt := Min( nAt, ::nLen )
         ::nRowPos := Min( nRowPos, ::nLen )

         ::Refresh( ::nLen < ::nRowCount() )
         ::DrawSelect()
         IF lAll
            ::DrawHeaders()
         ENDIF

      ENDCASE

   ELSE
      ::SetFocus()
      ::DrawSelect()
   ENDIF

RETURN ::lHasChanged

* ============================================================================
* METHOD TSBrowse:Destroy() Version 9.0 Nov/30/2009
* ============================================================================

METHOD Destroy() CLASS TSBrowse

   LOCAL oCol

   DEFAULT ::lDestroy := .F.

   IF ::uBmpSel != NIL .AND. ::lDestroy
      DeleteObject( ::uBmpSel )
   ENDIF

   IF ::hBrush != NIL // Alen Uzelac 13.09.2012
      DeleteObject( ::hBrush )
   ENDIF

   IF ::oCursor != NIL // GF 29.02.2016
      ::oCursor:End()
   ENDIF

   IF ::hBmpCursor != NIL
      DeleteObject( ::hBmpCursor )
   ENDIF

   IF ValType( ::aSortBmp ) == "A" .AND. ! Empty( ::aSortBmp )
      AEval( ::aSortBmp, {| hBmp | iif( Empty( hBmp ), , DeleteObject( hBmp ) ) } )
   ENDIF

   IF ValType( ::aCheck ) == "A" .AND. ! Empty( ::aCheck )
      AEval( ::aCheck, {| hBmp | iif( Empty( hBmp ), , DeleteObject( hBmp ) ) } )
   ENDIF

   IF Len( ::aColumns ) > 0
      FOR EACH oCol IN ::aColumns
         IF ValType( oCol:aCheck ) == "A"
            AEval( oCol:aCheck, {| hBmp | iif( Empty( hBmp ), , DeleteObject( hBmp ) ) } )
         ENDIF
         IF ValType( oCol:aBitMaps ) == "A"
            AEval( oCol:aBitMaps, {| hBmp | iif( Empty( hBmp ), , DeleteObject( hBmp ) ) } )
         ENDIF
         IF ! ::lDestroyAll
            LOOP
         ENDIF
         IF ! Empty( oCol:uBmpCell ) .AND. ! HB_ISBLOCK( oCol:uBmpCell )
            DeleteObject( oCol:uBmpCell )
         ENDIF
         IF ! Empty( oCol:uBmpHead ) .AND. ! HB_ISBLOCK( oCol:uBmpHead )
            DeleteObject( oCol:uBmpHead )
         ENDIF
         IF ! Empty( oCol:uBmpSpcHd ) .AND. ! HB_ISBLOCK( oCol:uBmpSpcHd )
            DeleteObject( oCol:uBmpSpcHd )
         ENDIF
         IF ! Empty( oCol:uBmpFoot ) .AND. ! HB_ISBLOCK( oCol:uBmpFoot )
            DeleteObject( oCol:uBmpFoot )
         ENDIF
      NEXT
   ENDIF

   IF ::lDestroyAll
      IF ValType( ::aSuperHead ) == "A" .AND. ! Empty( ::aSuperHead )
         AEval( ::aSuperHead, {| a | iif( Empty( a[ 8 ] ) .OR. HB_ISBLOCK( a[ 8 ] ), , DeleteObject( a[ 8 ] ) ) } )
      ENDIF
   ENDIF

   IF ValType( ::aBitMaps ) == "A" .AND. ! Empty( ::aBitMaps )
      AEval( ::aBitMaps, {| hBmp | iif( Empty( hBmp ), , DeleteObject( hBmp ) ) } )
   ENDIF

#ifndef _TSBFILTER7_
   IF ::lFilterMode
      ::lFilterMode := .F.
      IF Select( ::cAlias ) != 0
         ::SetFilter()
      ENDIF
   ENDIF
#endif
   ::hWnd := 0

RETURN 0

* ============================================================================
* METHOD TSBrowse:Display() Version 9.0 Nov/30/2009
* ============================================================================

METHOD Display() CLASS TSBrowse

   DEFAULT ::lFirstPaint := .T.

   IF Empty( ::aColumns ) .AND. ! ::lFirstPaint
      RETURN 0
   ENDIF

   ::BeginPaint()
   ::Paint()
   ::EndPaint()

RETURN 0

* ============================================================================
* METHOD TSBrowse:DrawHeaders() Version 9.0 Nov/30/2009
* ============================================================================

METHOD DrawHeaders( lFooters, lDrawCell ) CLASS TSBrowse

   LOCAL nI, nJ, nBegin, nStartCol, oColumn, l3DLook, nClrFore, lAdjBmp, nAlign, nClrBack, hFont, cFooting, ;
      cHeading, hBitMap, nLastCol, lMultiLine, nVertText, nClrTo, lOpaque, lBrush, nClrToS, nClrBackS, lOrder, lDescend, ;
      nMaxWidth := ::nWidth(), ;
      aColSizes := AClone( ::aColSizes ), ; // use local copies for speed
      nHeightHead := ::nHeightHead, ;
      nHeightFoot := ::nHeightFoot, ;
      nHeightSpecHd := ::nHeightSpecHd

   LOCAL nHeightSuper := ::nHeightSuper, ;
      nVAlign := 1, ;
      l3DText, nClr3dL, nClr3dS

   LOCAL hWnd := ::hWnd, ;
      hDC := ::hDc, ;
      nClrText := ::nClrText, ;
      nClrPane := ::nClrPane, ;
      nClrHeadFore := ::nClrHeadFore, ;
      nClrHeadBack := ::nClrHeadBack, ;
      nClrFootFore := ::nClrFootFore, ;
      nClrFootBack := ::nClrFootBack, ;
      nClrOrdeFore := ::nClrOrdeFore, ;
      nClrOrdeBack := ::nClrOrdeBack, ;
      nClrSpcHdFore := iif( ::lEnum, ::nClrHeadFore, ::nClrText ), ;
      nClrSpcHdBack := iif( ::lEnum, ::nClrHeadBack, ::nClrPane ), ;
      nClrSpcHdAct := ::nClrSpcHdActive, ;
      nClrLine := ::nClrLine, nLineStyle

   LOCAL nDeltaLen, uTmp, nBitmapMask // SergKis 11.11.21

   DEFAULT lFooters := .F., lDrawCell := ::lDrawLine

   IF Empty( ::aColumns )
      RETURN Self
   ENDIF

   IF ::aColSizes == NIL .OR. Len( ::aColSizes ) < Len( ::aColumns )
      ::aColSizes := {}
      FOR nI := 1 TO Len( ::aColumns )
         AAdd( ::aColSizes, iif( ::aColumns[ nI ]:lVisible, ::aColumns[ nI ]:nWidth, 0 ) ) // JP 1.58
      NEXT
   ENDIF

   IF ::lMChange // GF 1.96
      FOR nI := 1 TO Len( ::aColumns )
         IF ::aColumns[ nI ]:lVisible
            aColSizes[ nI ] := Max( ::aColumns[ nI ]:nWidth, ::nMinWidthCols )
            ::aColumns[ nI ]:nWidth := aColSizes[ nI ]
            ::aColSizes[ nI ] := aColSizes[ nI ]
         ENDIF
      NEXT
   ENDIF

   nI := Len( ::aColumns )                                                              // SergKis 11.11.21
   nClrBack := iif( ::nPhantom == -1, ATail( ::aColumns ):nClrHeadBack, nClrPane )
   nClrBack := iif( ValType( nClrBack ) == "B", Eval( nClrBack, nI, Self ), nClrBack )  // SergKis 11.11.21
   nClrFore := iif( ::nPhantom == -1, ATail( ::aColumns ):nClrFootBack, nClrPane )
   nClrFore := iif( ValType( nClrFore ) == "B", Eval( nClrFore, nI, Self ), nClrFore )  // SergKis 11.11.21
   l3DLook  := iif( ::nPhantom == -1, ATail( ::aColumns ):l3DLookHead, .F. )

   IF ::oPhant == NIL
      // "Phantom" column; :nPhantom hidden IVar
      ::oPhant := TSColumn():New( "", ; // cHeading
         {|| "" }, ; // bdata
         NIL, ; // cPicture
         { nClrText, nClrPane,, ;
         nClrBack,,,,,, nClrFore }, ; // aColors
         NIL, ; // aAlign
         ::nPhantom, ; // nWidth
         NIL, ; // lBitMap
         NIL, ; // lEdit
         NIL, ; // bValid
         .T., ; // lNoLite
         NIL, ; // cOrder
         NIL, ; // cFooting
         NIL, ; // bPrevEdit
         NIL, ; // bPostEdit
         NIL, ; // nEditMove
         NIL, ; // lFixLite
         { l3DLook, l3DLook }, ;
         NIL, ;
         Self )
      ::oPhant:cName := "oPhant"
      ::oPhant:nId := -1
   ELSE
      ::oPhant:nClrFore := nClrText
      ::oPhant:nClrBack := nClrBack
      ::oPhant:nWidth := ::nPhantom
      ::oPhant:l3DLookHead := l3DLook
   ENDIF

   nLastCol := Len( ::aColumns ) + 1
   AAdd( aColSizes, ::nPhantom )

   nJ := nStartCol := 0

   nBegin := Min( iif( ::nColPos <= ::nFreeze, ( ::nColPos := ::nFreeze + 1, ::nColPos - ::nFreeze ), ;
      ::nColPos - ::nFreeze ), nLastCol )

   IF Empty( ::aColumns )
      RETURN Self
   ENDIF

   IF ! Empty( ::aSuperHead ) .AND. ! lFooters
      ::DrawSuper()
   ENDIF

   IF ! lDrawCell
      nBegin := 1
      nLastCol := ::nColCount()
   ENDIF

   FOR nI := nBegin TO nLastCol

      IF nStartCol >= nMaxWidth .AND. lDrawCell
         EXIT
      ENDIF

      nJ := iif( nI < ::nColPos, nJ + 1, nI )

      oColumn := iif( nJ > Len( ::aColumns ), ::oPhant, ::aColumns[ nJ ] )

      nDeltaLen := ::GetDeltaLen( nJ, nStartCol, nMaxWidth, aColSizes )

      IF ::lDrawHeaders .AND. ! lFooters

         nVertText := 0
         lOrder := ::nColOrder == nJ
         lDescend := oColumn:lDescend

         IF LoWord( oColumn:nHAlign ) == DT_VERT
            cHeading := "Arial"

            hFont := InitFont( cHeading, -11, .F., .F., .F., .F., 900 )

            nVAlign := 2
            nVertText := 1

         ELSE

            hFont := ::hFontHeadGet( oColumn, nJ )
         ENDIF

         l3DLook := oColumn:l3DLookHead
         nAlign := ::nAlignGet( oColumn:nHAlign, nJ, DT_CENTER )

         IF ( nClrFore := iif( ::nColOrder == nI, oColumn:nClrOrdeFore, ;
               oColumn:nClrHeadFore ) ) == NIL
            nClrFore := iif( ::nColOrder == nI, nClrOrdeFore, ;
               nClrHeadFore )
         ENDIF

         nClrFore := ::GetValProp( nClrFore, nClrFore, nJ )

         IF !( nJ == 1 .AND. ::lSelector )
            IF ( nClrBack := iif( ::nColOrder == nI, oColumn:nClrOrdeBack, oColumn:nClrHeadBack ) ) == NIL
               nClrBack := iif( ::nColOrder == nI, nClrOrdeBack, nClrHeadBack )
            ENDIF
          ELSEIF ! ::lClrSelectorHdBack
             nClrBack := oColumn:nClrHeadBack
          ELSEIF ::nClrSelectorHdBack != NIL
             nClrBack := ::nClrSelectorHdBack
          ELSE
             nClrBack := ATail( ::aColumns ):nClrHeadBack
         ENDIF

         nClrBack := ::GetValProp( nClrBack, nClrBack, nJ )

         lBrush := ValType( nClrBack ) == "O"

         IF ValType( nClrBack ) == "A"
            nClrBack := ::nClrBackArr( nClrBack, nJ )
            nClrTo := nClrBack[ 2 ]
            nClrBack := nClrBack[ 1 ]
         ELSE
            nClrTo := nClrBack
         ENDIF
         IF lOrder
            DEFAULT lDescend := .F., ::aSortBmp := { StockBmp( 4 ), StockBmp( 5 ) }
            hBitMap := ::aSortBmp[ iif( lDescend, 2, 1 ) ]
            nAlign := nMakeLong( iif( nAlign == DT_RIGHT, DT_LEFT, nAlign ), DT_RIGHT )
         ELSE
            hBitMap := iif( ValType( oColumn:uBmpHead ) == "B", Eval( oColumn:uBmpHead, nJ, Self ), oColumn:uBmpHead )
            hBitMap := iif( ValType( hBitMap ) == "O", Eval( ::bBitMapH, hBitMap ), hBitMap )
         ENDIF

         cHeading := iif( ValType( oColumn:cHeading ) == "B", Eval( oColumn:cHeading, nJ, Self ), oColumn:cHeading )
         uTmp := cHeading
         lAdjBmp := oColumn:lAdjBmpHead
         lOpaque := .T.
         lMultiLine := ( ValType( cHeading ) == "C" .AND. At( Chr( 13 ), cHeading ) > 0 )
         DEFAULT hBitMap := 0

         IF lMultiLine
            nVAlign := DT_TOP
         ENDIF

         IF oColumn:l3DTextHead != NIL
            l3DText := oColumn:l3DTextHead
            nClr3dL := oColumn:nClr3DLHead
            nClr3dS := oColumn:nClr3DSHead
            nClr3dL := iif( ValType( nClr3dL ) == "B", Eval( nClr3dL, 0, nJ, Self ), nClr3dL )
            nClr3dS := iif( ValType( nClr3dS ) == "B", Eval( nClr3dS, 0, nJ, Self ), nClr3dS )
         ELSE
            l3DText := nClr3dL := nClr3dS := NIL
         ENDIF

         nLineStyle := 1

         IF HB_ISNUMERIC( oColumn:nHLineStyle )
            nLineStyle := oColumn:nHLineStyle
         ENDIF

         IF nAlign != DT_CENTER .AND. ::nCellMarginLR != NIL
            cHeading := ::CellMarginLeftRight( nJ, cHeading, oColumn, nAlign, lMultiLine, 0 )
         ENDIF
         // SergKis 11.11.21
         nBitmapMask := oColumn:nBmpMaskHead

         IF Empty( oColumn:oCellHead )
            oColumn:oCellHead := TSBcell():New()
         ENDIF

         oColumn:oCellHead:nRow := 0
         oColumn:oCellHead:nCol := nStartCol
         oColumn:oCellHead:nWidth := aColSizes[ nJ ] + nDeltaLen
         oColumn:oCellHead:nHeight := ::nHeightHead
         oColumn:oCellHead:nCell := nJ
         oColumn:oCellHead:uValue := uTmp
         oColumn:oCellHead:lDrawLine := .F.

         oColumn:oCellHead:hWnd := hWnd // 1
         oColumn:oCellHead:hDC := hDC // 2
         oColumn:oCellHead:xRow := 0 // 3
         oColumn:oCellHead:nStartCol := nStartCol // 4
         oColumn:oCellHead:nSize := aColSizes[ nJ ] + nDeltaLen // 5 aColSizes[ nJ ] + nDeltaLen
         oColumn:oCellHead:uData := cHeading // 6
         oColumn:oCellHead:nAlign := nAlign // 7
         oColumn:oCellHead:nClrFore := nClrFore // 8
         oColumn:oCellHead:nClrBack := nClrBack // 9
         oColumn:oCellHead:hFont := hFont // 10
         oColumn:oCellHead:hBitMap := hBitMap // 11
         oColumn:oCellHead:nHeightCell := nHeightHead // 12
         oColumn:oCellHead:l3DLook := l3DLook // 13 oColumn:l3DLook
         oColumn:oCellHead:nLineStyle := nLineStyle // 14
         oColumn:oCellHead:nClrLine := nClrLine // 15
         oColumn:oCellHead:nDrawType := 1 // 16 line/header/footer/super
         oColumn:oCellHead:nHeightHead := nHeightHead // 17
         oColumn:oCellHead:nHeightFoot := nHeightFoot // 18
         oColumn:oCellHead:nHeightSuper := nHeightSuper // 19
         oColumn:oCellHead:nHeightSpecHd := nHeightSpecHd // 20
         oColumn:oCellHead:lAdjBmp := lAdjBmp // 21
         oColumn:oCellHead:lMultiline := lMultiline // 22
         oColumn:oCellHead:nVAlign := nVAlign // 23
         oColumn:oCellHead:nVertText := nVertText // 24
         oColumn:oCellHead:nClrTo := nClrTo // 25
         oColumn:oCellHead:lOpaque := lOpaque // 26
         oColumn:oCellHead:hBrush := iif( lBrush, nClrBack:hBrush, 0 ) // 27
         oColumn:oCellHead:l3DText := l3DText // 28  3D text
         oColumn:oCellHead:nClr3dL := nClr3dL // 29  3D text light color
         oColumn:oCellHead:nClr3dS := nClr3dS // 30  3D text shadow color
         oColumn:oCellHead:nCursor := 0 // 31  Rect cursor
         oColumn:oCellHead:lInvertColor := .F. // 32  Invert color
         oColumn:oCellHead:nBitmapMask := nBitmapMask // SergKis 11.11.21 33

         IF lDrawCell
            ::TSDrawCell( oColumn:oCellHead, oColumn )
         ENDIF

         nVAlign := 1

         IF LoWord( oColumn:nHAlign ) == DT_VERT
            DeleteObject( hFont )
         ENDIF

      ENDIF

      IF ::lDrawSpecHd

         hFont := ::hFontSpcHdGet( oColumn, nJ )
         nAlign := ::nAlignGet( oColumn:nSAlign, nJ, DT_CENTER )

         l3DLook := oColumn:l3DLookHead

         IF ( nClrFore := iif( ::nColOrder == nI, oColumn:nClrOrdeFore, oColumn:nClrSpcHdFore ) ) == NIL
            nClrFore := iif( ::nColOrder == nI, nClrOrdeFore, nClrSpcHdFore )
         ENDIF

         nClrFore := ::GetValProp( nClrFore, nClrFore, nJ )

         IF nI == nBegin .AND. ::lSelector
            IF ! ::lClrSelectorHdBack
               nClrBacks := oColumn:nClrSpcHdBack
            ELSEIF ::nClrSelectorHdBack != NIL
               nClrBacks := ::nClrSelectorHdBack
            ELSE
               nClrBacks := nClrHeadBack
            ENDIF
         ELSE
            nClrBacks := iif( ::nPhantom == -1, ATail( ::aColumns ):nClrSpcHdBack, nClrPane )
         ENDIF

         nClrBackS := ::GetValProp( nClrBackS, nClrBackS, nJ )

         lBrush := ValType( nClrBackS ) == "O"

         IF ValType( nClrBackS ) == "A"
            nClrBackS := ::nClrBackArr( nClrBackS, nJ )
            nClrToS := nClrBackS[ 2 ]
            nClrBackS := nClrBackS[ 1 ]
         ELSE
            nClrToS := nClrBackS
         ENDIF
         uTmp := NIL
         IF ::lEnum
            cHeading := hb_ntos( nJ - iif( ::lSelector, 1, 0 ) )
            IF ! Empty( oColumn:cSpcHeading )
               uTmp := iif( ValType( oColumn:cSpcHeading ) == "B", Eval( oColumn:cSpcHeading, nJ, Self ), oColumn:cSpcHeading )
               cHeading := iif( HB_ISNUMERIC( uTmp ), hb_ntos( uTmp ), iif( HB_ISCHAR( uTmp ), uTmp, "" ) )
            ENDIF
            IF nI == nBegin .AND. ::lSelector .OR. nI == nLastCol
               cHeading := ""
            ENDIF
            uTmp := cHeading
         ELSE
            cHeading := iif( ValType( oColumn:cSpcHeading ) == "B", Eval( oColumn:cSpcHeading, nJ, Self ), oColumn:cSpcHeading )
            uTmp := cHeading
            IF Empty( oColumn:cPicture )
               cHeading := iif( ValType( cHeading ) != "C", cValToChar( cHeading ), cHeading )
            ELSE
               cHeading := iif( cHeading == NIL, "", Transform( cHeading, oColumn:cPicture ) )
            ENDIF

            nAlign := ::nAlignGet( oColumn:nAlign, nJ, DT_CENTER )
            nClrBackS := iif( Empty( cHeading ), nClrBackS, CLR_HRED )
            nClrBackS := iif( oColumn:lEditSpec, nClrBackS, nClrBack )
            nClrToS := iif( oColumn:lEditSpec, nClrToS, nClrTo )
         ENDIF
         IF nI == nLastCol
            nClrBackS := iif( ::nPhantom == PHCOL_GRID, nClrBackS, ::nClrPane )
            nClrTo := nClrBackS
         ENDIF
         hBitMap := iif( ValType( oColumn:uBmpSpcHd ) == "B", Eval( oColumn:uBmpSpcHd, nJ, Self ), oColumn:uBmpSpcHd )
         hBitMap := iif( ValType( hBitMap ) == "O", Eval( ::bBitMapH, hBitMap ), hBitMap )
         lAdjBmp := oColumn:lAdjBmpSpcHd
         lOpaque := .T.
         DEFAULT hBitMap := 0

         IF oColumn:l3DTextHead != NIL
            l3DText := oColumn:l3DTextSpcHd
            nClr3dL := oColumn:nClr3DLSpcHd
            nClr3dS := oColumn:nClr3DSSpcHd
            nClr3dL := iif( ValType( nClr3dL ) == "B", Eval( nClr3dL, 0, nJ, Self ), nClr3dL )
            nClr3dS := iif( ValType( nClr3dS ) == "B", Eval( nClr3dS, 0, nJ, Self ), nClr3dS )
         ELSE
            l3DText := nClr3dL := nClr3dS := NIL
         ENDIF

         nLineStyle := 1

         IF HB_ISNUMERIC( oColumn:nSLineStyle )
            nLineStyle := oColumn:nSLineStyle
         ENDIF

         IF nAlign != DT_CENTER .AND. ::nCellMarginLR != NIL
            cHeading := ::CellMarginLeftRight( nJ, cHeading, oColumn, nAlign, lMultiLine, 0 )
         ENDIF
         // SergKis 11.11.21
         nBitmapMask := oColumn:nBmpMaskSpcHd

         IF Empty( oColumn:oCellEnum )
            oColumn:oCellEnum := TSBcell():New()
         ENDIF

         oColumn:oCellEnum:nRow := 0
         oColumn:oCellEnum:nCol := nStartCol
         oColumn:oCellEnum:nWidth := aColSizes[ nJ ] + nDeltaLen
         oColumn:oCellEnum:nHeight := ::nHeightSpecHd
         oColumn:oCellEnum:nCell := nJ
         oColumn:oCellEnum:uValue := uTmp
         oColumn:oCellEnum:lDrawLine := .F.
         oColumn:oCellEnum:hWnd := hWnd // 1
         oColumn:oCellEnum:hDC := hDC // 2
         oColumn:oCellEnum:xRow := 0 // 3
         oColumn:oCellEnum:nStartCol := nStartCol // 4
         oColumn:oCellEnum:nSize := aColSizes[ nJ ] + nDeltaLen // 5 aColSizes[ nJ ] + nDeltaLen
         oColumn:oCellEnum:uData := cHeading // 6
         oColumn:oCellEnum:nAlign := nAlign // 7
         oColumn:oCellEnum:nClrFore := nClrFore // 8
         oColumn:oCellEnum:nClrBack := nClrBackS // 9
         oColumn:oCellEnum:hFont := hFont // 10
         oColumn:oCellEnum:hBitMap := hBitMap // 11
         oColumn:oCellEnum:nHeightCell := 0 // 12
         oColumn:oCellEnum:l3DLook := l3DLook // 13 oColumn:l3DLook
         oColumn:oCellEnum:nLineStyle := nLineStyle // 14
         oColumn:oCellEnum:nClrLine := nClrLine // 15
         oColumn:oCellEnum:nDrawType := 4 // 16 line/header/footer/super
         oColumn:oCellEnum:nHeightHead := nHeightHead // 17
         oColumn:oCellEnum:nHeightFoot := nHeightFoot // 18
         oColumn:oCellEnum:nHeightSuper := nHeightSuper // 19
         oColumn:oCellEnum:nHeightSpecHd := nHeightSpecHd // 20
         oColumn:oCellEnum:lAdjBmp := lAdjBmp // 21
         oColumn:oCellEnum:lMultiline := .F. // 22
         oColumn:oCellEnum:nVAlign := nVAlign // 23
         oColumn:oCellEnum:nVertText := 0 // 24
         oColumn:oCellEnum:nClrTo := nClrToS // 25
         oColumn:oCellEnum:lOpaque := lOpaque // 26
         oColumn:oCellEnum:hBrush := iif( lBrush, nClrBack:hBrush, 0 ) // 27  iif( lBrush, nClrBack:hBrush, 0 )
         oColumn:oCellEnum:l3DText := l3DText // 28  3D text
         oColumn:oCellEnum:nClr3dL := nClr3dL // 29  3D text light color
         oColumn:oCellEnum:nClr3dS := nClr3dS // 30  3D text shadow color
         oColumn:oCellEnum:nCursor := 0 // 31  Rect cursor
         oColumn:oCellEnum:lInvertColor := .F. // 32  Invert color
         oColumn:oCellHead:nBitmapMask := nBitmapMask // SergKis 11.11.21 33

         IF lDrawCell
            ::TSDrawCell( oColumn:oCellEnum, oColumn )
         ENDIF

      ENDIF

      IF ::lFooting .AND. ::lDrawFooters

         hFont := ::hFontFootGet( oColumn, nJ )
         nAlign := ::nAlignGet( oColumn:nFAlign, nJ, DT_CENTER )
         l3DLook := oColumn:l3DLookFoot

         ::oPhant:l3DLookFoot := l3DLook

         nClrFore := iif( oColumn:nClrFootFore != NIL, oColumn:nClrFootFore, nClrFootFore )
         nClrFore := ::GetValProp( nClrFore, nClrFore, nJ )

         IF !( nJ == 1 .AND. ::lSelector ) // JP
            nClrBack := iif( oColumn:nClrFootBack != NIL, oColumn:nClrFootBack, nClrFootBack )
         ELSEIF ! ::lClrSelectorHdBack
            nClrBack := oColumn:nClrFootBack
         ELSEIF ::nClrSelectorFtBack != NIL
            nClrBack := ::nClrSelectorFtBack
         ELSEIF ::nClrSelectorHdBack != NIL
            nClrBack := ::nClrSelectorHdBack
         ELSE
            nClrBack := ATail( ::aColumns ):nClrFootBack
         ENDIF
         nClrBack := ::GetValProp( nClrBack, nClrBack, nJ )

         lBrush := ValType( nClrBack ) == "O"

         IF ValType( nClrBack ) == "A"
            nClrBack := ::nClrBackArr( nClrBack, nJ )
            nClrTo := nClrBack[ 2 ]
            nClrBack := nClrBack[ 1 ]
         ELSE
            nClrTo := nClrBack
         ENDIF

         IF nI == nBegin .AND. ::lSelector // JP
            cFooting := ""
         ELSE
            cFooting := iif( ValType( oColumn:cFooting ) == "B", Eval( oColumn:cFooting, nJ, Self ), oColumn:cFooting )
         ENDIF

         IF ValType( cFooting ) == "O"
            oColumn:uBmpFoot := cFooting
            cFooting := ""
         ENDIF

         uTmp := cFooting
         hBitMap := iif( ValType( oColumn:uBmpFoot ) == "B", Eval( oColumn:uBmpFoot, nJ, Self ), oColumn:uBmpFoot )
         hBitMap := iif( ValType( hBitMap ) == "O", Eval( ::bBitMapH, hBitMap ), hBitMap )
         lOpaque := .T.
         lAdjBmp := oColumn:lAdjBmpFoot
         lMultiLine := ValType( cFooting ) == "C" .AND. At( Chr( 13 ), cFooting ) > 0
         DEFAULT hBitMap := 0

         IF oColumn:l3DTextFoot != NIL
            l3DText := oColumn:l3DTextFoot
            nClr3dL := oColumn:nClr3DLFoot
            nClr3dS := oColumn:nClr3DSFoot
            nClr3dL := iif( ValType( nClr3dL ) == "B", Eval( nClr3dL, 0, nJ, Self ), nClr3dL )
            nClr3dS := iif( ValType( nClr3dS ) == "B", Eval( nClr3dS, 0, nJ, Self ), nClr3dS )
         ELSE
            l3DText := nClr3dL := nClr3dS := NIL
         ENDIF

         nLineStyle := 1

         IF HB_ISNUMERIC( oColumn:nFLineStyle )
            nLineStyle := oColumn:nFLineStyle
         ENDIF
         // SergKis 11.11.21
         nBitmapMask := oColumn:nBmpMaskFoot

         IF nAlign != DT_CENTER .AND. ::nCellMarginLR != NIL
            cFooting := ::CellMarginLeftRight( nJ, cFooting, oColumn, nAlign, lMultiLine, 0 )
         ENDIF

         IF Empty( oColumn:oCellFoot )
            oColumn:oCellFoot := TSBcell():New()
         ENDIF

         oColumn:oCellFoot:nRow := ::nRowCount()
         oColumn:oCellFoot:nCol := nStartCol
         oColumn:oCellFoot:nWidth := aColSizes[ nJ ] + nDeltaLen
         oColumn:oCellFoot:nHeight := ::nHeightFoot
         oColumn:oCellFoot:nCell := nJ
         oColumn:oCellFoot:uValue := uTmp
         oColumn:oCellFoot:lDrawLine := .F.

         oColumn:oCellFoot:hWnd := hWnd // 1
         oColumn:oCellFoot:hDC := hDC // 2
         oColumn:oCellFoot:xRow := ::nRowCount() // 3
         oColumn:oCellFoot:nStartCol := nStartCol // 4
         oColumn:oCellFoot:nSize := aColSizes[ nJ ] + nDeltaLen // 5 aColSizes[ nJ ] + nDeltaLen
         oColumn:oCellFoot:uData := cFooting // 6
         oColumn:oCellFoot:nAlign := nAlign // 7
         oColumn:oCellFoot:nClrFore := nClrFore // 8
         oColumn:oCellFoot:nClrBack := nClrBack // 9
         oColumn:oCellFoot:hFont := hFont // 10
         oColumn:oCellFoot:hBitMap := hBitMap // 11
         oColumn:oCellFoot:nHeightCell := nHeightFoot // 12
         oColumn:oCellFoot:l3DLook := l3DLook // 13 oColumn:l3DLook
         oColumn:oCellFoot:nLineStyle := nLineStyle // 14
         oColumn:oCellFoot:nClrLine := nClrLine // 15
         oColumn:oCellFoot:nDrawType := 2 // 16 line/header/footer/super
         oColumn:oCellFoot:nHeightHead := nHeightHead // 17
         oColumn:oCellFoot:nHeightFoot := nHeightFoot // 18
         oColumn:oCellFoot:nHeightSuper := nHeightSuper // 19
         oColumn:oCellFoot:nHeightSpecHd := nHeightSpecHd // 20
         oColumn:oCellFoot:lAdjBmp := lAdjBmp // 21
         oColumn:oCellFoot:lMultiline := lMultiline // 22
         oColumn:oCellFoot:nVAlign := nVAlign // 23
         oColumn:oCellFoot:nVertText := 0 // 24
         oColumn:oCellFoot:nClrTo := nClrTo // 25
         oColumn:oCellFoot:lOpaque := lOpaque // 26
         oColumn:oCellFoot:hBrush := iif( lBrush, nClrBack:hBrush, 0 ) // 27  iif( lBrush, nClrBack:hBrush, 0 )
         oColumn:oCellFoot:l3DText := l3DText // 28  3D text
         oColumn:oCellFoot:nClr3dL := nClr3dL // 29  3D text light color
         oColumn:oCellFoot:nClr3dS := nClr3dS // 30  3D text shadow color
         oColumn:oCellFoot:nCursor := 0 // 31  Rect cursor
         oColumn:oCellFoot:lInvertColor := .F. // 32  Invert color
         oColumn:oCellFoot:nBitmapMask := nBitmapMask // SergKis 11.11.21 33

         IF lDrawCell
            ::TSDrawCell( oColumn:oCellFoot, oColumn )
         ENDIF

      ENDIF

      nStartCol += aColSizes[ nJ ] + nDeltaLen

   NEXT

RETURN Self

* ============================================================================
* METHOD TSBrowse:DrawIcons() Version 9.0 Nov/30/2009
* ============================================================================

METHOD DrawIcons() CLASS TSBrowse

   LOCAL cText, ;
      nWidth := ::nWidth(), ;
      nHeight := ::nHeight(), ;
      nRow := 10, ;
      nCol := 10, ;
      n := 1

   LOCAL nIcons := Int( nWidth / 50 ) * Int( nHeight / 50 ), ;
      hIcon := ExtractIcon( "user.exe", 0 )

   SetBkColor( ::hDC, CLR_BLUE )
   SetTextColor( ::hDC, CLR_WHITE )

   WHILE n <= nIcons .AND. !( ::cAlias )->( Eof() )
      IF ::bIconDraw != NIL .AND. ::aIcons != NIL
         hIcon := ::aIcons[ Eval( ::bIconDraw, Self ) ]
      ENDIF

      DrawIcon( ::hDC, nRow, nCol, hIcon )

      IF ::bIconText != NIL
         cText := cValToChar( Eval( ::bIconText, Self ) )
      ELSE
         cText := Str( ( ::cAlias )->( RecNo() ) )
      ENDIF

      DrawText( ::hDC, cText, { nRow + 35, nCol - 5, nRow + 48, nCol + 40 }, 1 )

      nCol += 50

      IF nCol >= nWidth - 32
         nRow += 50
         nCol := 10
      ENDIF

      ( ::cAlias )->( dbSkip() )
      n++

   ENDDO

   ( ::cAlias )->( dbSkip( 1 - n ) )

RETURN NIL

* ============================================================================
* METHOD TSBrowse:DrawLine() Version 9.0 Nov/30/2009
* ============================================================================

METHOD DrawLine( xRow, lDrawCell ) CLASS TSBrowse

   LOCAL nI, nJ, nBegin, nStartCol, oColumn, hBitMap, cPicture, hFont, nClrTo, nClrFore, nClrBack, uData, nLastCol, ;
      lAdjBmp, lMultiLine, nAlign, lOpaque, lBrush, lCheck, uBmpCell, nVertText, lSelected, ;
      nVAlign := 1, ;
      nMaxWidth := ::nWidth(), ;
      nRowPos := ::nRowPos, ;
      nClrText := ::nClrText, ;
      nClrPane := ::nClrPane

   LOCAL l3DText, nClr3dL, nClr3dS, l3DLook
   LOCAL aBitMaps, lCheckVal := .F., cColAls
   LOCAL nDeltaLen, xData, nAt, oCell, cCell, lCell, nBitmapMask  // SergKis 11.11.21

   LOCAL aColSizes := AClone( ::aColSizes ), ;
      hWnd := ::hWnd, ;
      hDC := ::hDC, ;
      nLineStyle, ;
      nClrLine := ::nClrLine, ;
      nHeightCell := ::nHeightCell, ;
      nHeightHead := iif( ::lDrawHeaders, ::nHeightHead, 0 ), ;
      nHeightFoot := iif( ::lDrawFooters != NIL .AND. ::lDrawFooters, ::nHeightFoot, 0 ), ;
      nHeightSuper := iif( ::lDrawHeaders, ::nHeightSuper, 0 ), ;
      nHeightSpecHd := iif( ::lDrawSpecHd, ::nHeightSpecHd, 0 )

   IF Empty( ::aColumns )
      RETURN NIL
   ENDIF

   DEFAULT xRow := iif( ::lDrawHeaders, Max( 1, nRowPos ), nRowPos ), lDrawCell := ::lDrawLine

   ::nPaintRow := xRow
   lSelected := ::lCanSelect .AND. ( AScan( ::aSelected, ::nAtPos ) ) > 0

   nClrBack := iif( ::nPhantom = -1, ATail( ::aColumns ):nClrBack, nClrPane )
   nClrBack := iif( ValType( nClrBack ) == "B", Eval( nClrBack, ::nAt, Len( ::aColumns ), Self ), nClrBack )
   l3DLook := iif( ::nPhantom == -1, ATail( ::aColumns ):l3DLook, .F. )

   IF ::lRowPosAtRec
      IF Empty( ::aRowPosAtRec ) .OR. Len( ::aRowPosAtRec ) != ::nRowCount()
         ::aRowPosAtRec := Array( ::nRowCount() )
         AFill( ::aRowPosAtRec, 0 )
      ENDIF
      IF ::nLen == 0 .OR. xRow == 0 .OR. xRow > Len( ::aRowPosAtRec )
      ELSE
         ::aRowPosAtRec[ xRow ] := ( nAt := ::nAtPos )
      ENDIF
   ENDIF

   IF ::nLen > 0

      IF nAt == NIL
         nAt := ::nAtPos
      ENDIF

      IF ::oPhant == NIL
         // "Phantom" column; :nPhantom hidden IVar
         ::oPhant := TSColumn():New( "", ; // cHeading
            {|| "" }, ; // bdata
            NIL, ; // cPicture
            { nClrText, nClrBack }, ; // aColors
            NIL, ; // aAlign
            ::nPhantom, ; // nWidth
            NIL, ; // lBitMap
            NIL, ; // lEdit
            NIL, ; // bValid
            .T., ; // lNoLite
            NIL, ; // cOrder
            NIL, ; // cFooting
            NIL, ; // bPrevEdit
            NIL, ; // bPostEdit
            NIL, ; // nEditMove
            NIL, ; // lFixLite
            { l3DLook }, ;
            NIL, ;
            Self )
         ::oPhant:cName := "oPhant"
         ::oPhant:nId := -1
      ELSE
         ::oPhant:nClrFore := nClrText
         ::oPhant:nClrBack := nClrBack
         ::oPhant:nWidth := ::nPhantom
         ::oPhant:l3DLook := l3DLook
      ENDIF

      AAdd( aColSizes, ::nPhantom )

      nJ := nStartCol := 0
      nLastCol := Len( ::aColumns ) + 1
      nBegin := Min( iif( ::nColPos <= ::nFreeze, ( ::nColPos := ::nFreeze + 1, ;
         ::nColPos - ::nFreeze ), ::nColPos - ::nFreeze ), nLastCol )

      IF ::bOnDrawLine != NIL
         IF IsArray( ::bOnDrawLine )
            FOR nI := 1 TO Len( ::bOnDrawLine )
                IF IsBlock( ::bOnDrawLine[ nI ] )
                   IF ! Empty( Eval( ::bOnDrawLine[ nI ], Self, xRow ) )
                      RETURN Self
                   ENDIF
                ENDIF
            NEXT
         ELSEIF ! Empty( Eval( ::bOnDrawLine, Self, xRow ) )
            RETURN Self
         ENDIF
      ENDIF

      IF ! lDrawCell
         nBegin := 1
         nLastCol := ::nColCount()
      ENDIF

      FOR nI := nBegin TO nLastCol

         IF nStartCol >= nMaxWidth .AND. lDrawCell
            EXIT
         ENDIF

         nJ := iif( nI < ::nColPos, nJ + 1, nI )

         lSelected := iif( nJ == nLastCol, .F., lSelected )
         oColumn := iif( nJ > Len( ::aColumns ), ::oPhant, ::aColumns[ nJ ] )
         nDeltaLen := ::GetDeltaLen( nJ, nStartCol, nMaxWidth, aColSizes )

         cCell := hb_ntos( nAt ) + "." + hb_ntos( oColumn:nId )
         lCell := .F.

         IF ::lFastDrawCell
            oCell := hb_HGetDef( ::aFastDrawCell, cCell, NIL )
            lCell := ISOBJECT( oCell )
         ENDIF

         nLineStyle := iif( HB_ISNUMERIC( oColumn:nLineStyle ), oColumn:nLineStyle, ::nLineStyle )
         cPicture := ::cPictureGet( oColumn, nJ )
         hFont := ::hFontGet( oColumn, nJ )
         cColAls := iif( '->' $ oColumn:cField, NIL, oColumn:cAlias )

         IF ValType( oColumn:bSeek ) == 'B'
            IF cColAls != NIL
               ( cColAls )->( Eval( oColumn:bSeek, Self, nJ ) )
            ELSE
               Eval( oColumn:bSeek, Self, nJ )
            ENDIF
         ENDIF

         IF ::lIsArr .AND. ( ::lAppendMode .OR. ::nAt > Len( ::aArray ) )
            uData := "" // append mode for arrays
         ELSEIF lCell
            uData := oCell:uValue
         ELSE
            uData := ::bDataEval( oColumn, , nJ )
         ENDIF

         xData := uData

         lMultiLine := ValType( uData ) == "C" .AND. At( Chr( 13 ), uData ) > 0

         nVertText := 0
         lCheck := ( oColumn:lCheckBox .AND. ValType( uData ) == "L" .AND. oColumn:lVisible )

         IF lCheck .AND. ValType( uData ) == "L"
            cPicture := ""
            nVertText := iif( uData, 3, 4 )
            lCheckVal := uData
         ENDIF

         nAlign := oColumn:nAlign
         uBmpCell := oColumn:uBmpCell

         IF nJ == ::nColSel .AND. ::uBmpSel != NIL .AND. lSelected
            uBmpCell := ::uBmpSel
            nAlign := nMakeLong( LoWord( nAlign ), ::nAligBmp )
         ELSEIF oColumn:lBitMap .AND. ValType( uData ) == "N"
            aBitMaps := iif( ValType( oColumn:aBitMaps ) == "A", oColumn:aBitMaps, ::aBitMaps )
            IF ! Empty( aBitMaps ) .AND. uData > 0 .AND. uData <= Len( aBitMaps )
               uBmpCell := aBitMaps[ uData ]
            ENDIF
            nAlign := nMakeLong( oColumn:nAlign, oColumn:nAlign )
            uData := ""
         ElseIF ! lCheck .AND. oColumn:lEmptyValToChar .AND. Empty( uData )
            uData := ""
         ELSEIF lCell
            uData := oCell:cValue
         ELSEIF Empty( cPicture ) .OR. lMultiLine
            IF ValType( uData ) != "C"
               IF ValType( uData ) == "L"
                  uData := ::aMsg[ iif( uData, 1, 2 ) ]
               ELSE
                  uData := cValToChar( uData )
               ENDIF
            ENDIF
         ELSE
            uData := iif( uData == NIL, "", Transform( uData, cPicture ) )
         ENDIF

         nAlign := ::nAlignGet( oColumn:nAlign, nJ, DT_LEFT )

         IF ( nClrFore := oColumn:nClrFore ) == NIL .OR. ( lSelected .AND. ::uBmpSel == Nil )
            nClrFore := iif( ! lSelected, nClrText, ::nClrSeleFore )
         ENDIF

         nClrFore := ::GetValProp( nClrFore, nClrFore, nJ, ::nAt )

         IF ( nClrBack := oColumn:nClrBack ) == NIL .OR. ;
               ( lSelected .AND. ::uBmpSel == Nil )
            nClrBack := iif( ! lSelected, nClrPane, ::nClrSeleBack )
         ENDIF

         nClrBack := ::GetValProp( nClrBack, nClrBack, nJ, ::nAt )

         lBrush := ValType( nClrBack ) == "O"

         IF ValType( nClrBack ) == "A"
            nClrBack := ::nClrBackArr( nClrBack, nJ, ::nAt )
            nClrTo := nClrBack[ 2 ]
            nClrBack := nClrBack[ 1 ]
         ELSE
            nClrTo := nClrBack
         ENDIF

         hBitMap := iif( ValType( uBmpCell ) == "B", Eval( uBmpCell, nJ, Self ), uBmpCell )
         hBitMap := iif( ValType( hBitMap ) == "O", Eval( ::bBitMapH, hBitMap ), hBitMap )
         DEFAULT hBitMap := 0
         lAdjBmp := oColumn:lAdjBmp
         lOpaque := .T. // JP

         IF lCheck
            IF lCell
               hBitMap := oCell:hBitMap
               nAlign := oCell:nAlign
            ELSE
               DEFAULT ::aCheck := { StockBmp( 6 ), StockBmp( 7 ) }
               IF ValType( oColumn:aCheck ) == "A"
                  hBitMap := oColumn:aCheck[ iif( lCheckVal, 1, 2 ) ]
               ELSE
                  hBitMap := ::aCheck[ iif( lCheckVal, 1, 2 ) ]
               ENDIF
               nAlign := nMakeLong( DT_CENTER, DT_CENTER )
            ENDIF
            uData := ""
         ENDIF

         IF oColumn:l3DTextCell != NIL
            l3DText := oColumn:l3DTextCell
            nClr3dL := oColumn:nClr3DLCell
            nClr3dS := oColumn:nClr3DSCell
            nClr3dL := iif( ValType( nClr3dL ) == "B", Eval( nClr3dL, ::nAt, nJ, Self ), nClr3dL )
            nClr3dS := iif( ValType( nClr3dS ) == "B", Eval( nClr3dS, ::nAt, nJ, Self ), nClr3dS )
         ELSE
            l3DText := nClr3dL := nClr3dS := NIL
         ENDIF

         IF ! lCell
            IF nAlign != DT_CENTER .AND. ::nCellMarginLR != NIL
               uData := ::CellMarginLeftRight( nJ, uData, oColumn, nAlign, lMultiLine, 0 )
            ENDIF
         ENDIF
         // SergKis 11.11.21
         nBitmapMask := oColumn:nBmpMaskCell

         IF Empty( oColumn:oCell )
            oColumn:oCell := TSBcell():New()
         ENDIF

         oCell := oColumn:oCell
         WITH OBJECT oCell
            :nRow := xRow
            :nCol := nStartCol
            :nWidth := aColSizes[ nJ ] + nDeltaLen
            :nHeight := ::nHeightCell
            :nCell := nJ
            :uValue := xData
            :lDrawLine := .T. // DrawLine()

            :hWnd := hWnd // 1
            :hDC := hDC // 2
            :xRow := xRow // 3
            :nStartCol := nStartCol // 4
            :nSize := aColSizes[ nJ ] + nDeltaLen // 5 aColSizes[ nJ ] + nDeltaLen
            :uData := uData // 6
            :nAlign := nAlign // 7
            :nClrFore := nClrFore // 8
            :nClrBack := nClrBack // 9
            :hFont := hFont // 10
            :hBitMap := hBitMap // 11
            :nHeightCell := nHeightCell // 12
            :l3DLook := oColumn:l3DLook // 13 oColumn:l3DLook
            :nLineStyle := nLineStyle // 14
            :nClrLine := nClrLine // 15
            :nDrawType := 0 // 16 line/header/footer/super
            :nHeightHead := nHeightHead // 17
            :nHeightFoot := nHeightFoot // 18
            :nHeightSuper := nHeightSuper // 19
            :nHeightSpecHd := nHeightSpecHd // 20
            :lAdjBmp := lAdjBmp // 21
            :lMultiline := lMultiline // 22
            :nVAlign := nVAlign // 23
            :nVertText := nVertText // 24
            :nClrTo := nClrTo // 25
            :lOpaque := lOpaque // 26
            :hBrush := iif( lBrush, nClrBack:hBrush, 0 ) // 27
            :l3DText := l3DText // 28  3D text
            :nClr3dL := nClr3dL // 29  3D text light color
            :nClr3dS := nClr3dS // 30  3D text shadow color
            :nCursor := 0 // 31  Rect cursor
            :lInvertColor := .F. // 32  Invert color
            :nBitmapMask := nBitmapMask // SergKis 11.11.21 33
         END WITH

         IF ISBLOCK( oColumn:bDrawCell )
            Eval( oColumn:bDrawCell, Self, oColumn:oCell, oColumn )
         ENDIF
         IF lDrawCell
            ::TSDrawCell( oColumn:oCell, oColumn )
         ENDIF
         IF ::lFastDrawCell .AND. ! lCell
            hb_HSet( ::aFastDrawCell, cCell, __objClone( oColumn:oCell ) )
         ENDIF

         nStartCol += aColSizes[ nJ ] + nDeltaLen

      NEXT

   ENDIF

RETURN Self

* ============================================================================
* METHOD TSBrowse:TSDrawCell()  by SergKis
* ============================================================================

METHOD TSDrawCell( oCell, oCol ) CLASS TSBrowse

   LOCAL lDraw

   IF ISBLOCK( ::bTSDrawCell )
      lDraw := Eval( ::bTSDrawCell, Self, oCell, oCol )
      IF ISLOGICAL( lDraw ) .AND. Empty( lDraw )
         RETURN .F.
      ENDIF
   ENDIF

   lDraw := TSDrawCell( oCell:hWnd, ;  // 1
   oCell:hDC, ;  // 2
   oCell:nRow, ; // 3
   oCell:nCol, ;  // 4
   oCell:nSize, ; // 5 aColSizes[ nJ ] + nDeltaLen
   oCell:uData, ;  // 6 cHeading
   oCell:nAlign, ; // 7
   oCell:nClrFore, ;  // 8
   oCell:nClrBack, ; // 9
   oCell:hFont, ;  // 10
   oCell:hBitMap, ; // 11
   oCell:nHeightCell, ;  // 12 nHeightHead
   oCell:l3DLook, ;  // 13
   oCell:nLineStyle, ; // 14
   oCell:nClrLine, ;  // 15
   oCell:nDrawType, ; // 16 1=Header 2=Footer 3=Super 4=Special
   oCell:nHeightHead, ;  // 17
   oCell:nHeightFoot, ;  // 18
   oCell:nHeightSuper, ;  // 19
   oCell:nHeightSpecHd, ;  // 20
   oCell:lAdjBmp, ;  // 21
   oCell:lMulTiLine, ; // 22
   oCell:nVAlign, ;  // 23
   oCell:nVertText, ; // 24
   oCell:nClrTo, ;  // 25
   oCell:lOpaque, ; // 26
   oCell:hBrush, ;  // 27
   oCell:l3DText, ; // 28  3D text
   oCell:nClr3dL, ;  // 29  3D text light color
   oCell:nClr3dS, ; // 30  3D text shadow color
   oCell:nCursor, ;  // 31
   oCell:lInvertColor, ; // 32
   oCell:nBitmapMask ) // SergKis 11.11.21 33

RETURN lDraw

* ============================================================================
* METHOD TSBrowse:CellMarginLeftRight()  by SergKis
* ============================================================================

METHOD CellMarginLeftRight( nJ, cData, oColumn, nAlign, lMultiLine, nOut ) CLASS TSBrowse

   LOCAL nCellMarginLR, aTmp, cTmp, nK, nN, cBuf
   LOCAL uTmp := ::nCellMarginLR

   nCellMarginLR := If( ValType( uTmp ) == "B", Eval( uTmp, nJ, Self, oColumn, nAlign, nOut ), uTmp )

   IF HB_ISNUMERIC ( nCellMarginLR ) ; cBuf := Space( nCellMarginLR )
   ELSEIF HB_ISCHAR( nCellMarginLR ) ; cBuf := nCellMarginLR
   ENDIF

   IF HB_ISCHAR( cBuf ) .AND. Len( cBuf ) > 0
      DEFAULT cData := "", lMultiLine := ( CRLF $ cData )
      IF lMultiLine
         aTmp := hb_ATokens( cData, CRLF )
         cData := ''
         nN := Len( aTmp )
         FOR nK := 1 TO nN
            cTmp := aTmp[ nK ]
            IF nAlign == DT_LEFT ; cTmp := cBuf + cTmp
            ELSEIF nAlign == DT_RIGHT ; cTmp += cBuf
            ENDIF
            cData += cTmp + iif( nK == nN, "", CRLF )
         NEXT
      ELSE
         IF nAlign == DT_LEFT ; cData := cBuf + cData
         ELSEIF nAlign == DT_RIGHT ; cData += cBuf
         ENDIF
      ENDIF
   ENDIF

RETURN cData

* ============================================================================
* METHOD TSBrowse:DrawPressed() Version 9.0 Nov/30/2009
// Header pressed effect
* ============================================================================

METHOD DrawPressed( nCell, lPressed ) CLASS TSBrowse

   LOCAL nI, nLeft, nTop, nBottom, nRight, hDC, hOldPen
   LOCAL hGrayPen := CreatePen( PS_SOLID, 1, ::nClrLine ), ;
      hWhitePen := CreatePen( PS_SOLID, 1, GetSysColor( COLOR_BTNHIGHLIGHT ) )

   DEFAULT lPressed := .T.

   nKeyPressed := NIL

   IF Empty( nCell ) .OR. nCell > Len( ::aColumns ) .OR. ! ::lDrawHeaders
      RETURN Self
   ELSEIF ! lPressed .AND. ! ::aColumns[ nCell ]:l3DLookHead
      ::DrawHeaders()
      RETURN Self
   ENDIF

   hDC := GetDC( ::hWnd )
   nLeft := 0

   IF ::nFreeze > 0
      FOR nI := 1 TO Min( ::nFreeze, nCell - 1 )
         nLeft += ::GetColSizes()[ nI ]
      NEXT
   ENDIF

   FOR nI := ::nColPos TO nCell - 1
      nLeft += ::GetColSizes()[ nI ]
   NEXT

   nTop := ::nHeightSuper
   nTop -= iif( nTop > 0, 1, 0 )
   nRight := nLeft + ::aColSizes[ nCell ]
   nBottom := nTop + ::nHeightHead
   hOldPen := SelectObject( hDC, iif( lPressed, hGrayPen, hWhitePen ) )

   MoveTo( hDC, nLeft, nBottom )
   LineTo( hDC, nLeft, nTop )
   LineTo( hDC, nRight, nTop )
   SelectObject( hDC, iif( lPressed, hWhitePen, hGrayPen ) )
   MoveTo( hDC, nLeft, nBottom - 1 )
   LineTo( hDC, nRight - 1, nBottom - 1 )
   LineTo( hDC, nRight - 1, nTop - 1 )
   SelectObject( hDC, hOldPen )
   DeleteObject( hGrayPen )
   DeleteObject( hWhitePen )
   ReleaseDC( ::hWnd, hDC )

   IF lPressed
      nKeyPressed := nCell
   ENDIF

RETURN Self

* ============================================================================
* METHOD TSBrowse:DrawSelect()  Version 9.0 Nov/30/2009
* ============================================================================

METHOD DrawSelect( xRow, lDrawCell ) CLASS TSBrowse

   LOCAL nI, nJ, nBegin, nStartCol, oColumn, nLastCol, hBitMap, hFont, nAlign, cPicture, nClrFore, nClrBack, ;
      lNoLite, uData, l3DLook, lMulti, nClrTo, lOpaque, lBrush, nCursor, lCheck, uBmpCell, cMsg, lAdjBmp, ;
      lSelected, ;
      nVertText := 0, ;
      nMaxWidth := ::nWidth(), ; // use local copies for speed
   nRowPos := ::nRowPos, ;
      aColSizes := AClone( ::aColSizes ), ;
      hWnd := ::hWnd, ;
      hDC := ::hDc, ;
      lFocused := ::lFocused := ( GetFocus() == ::hWnd ), ;
      nVAlign := 1

   LOCAL l3DText, nClr3dL, nClr3dS
   LOCAL aBitMaps, lCheckVal := .F., cColAls
   LOCAL nDeltaLen, lDraw := .F., xData
   LOCAL nAt, oCell, cCell, lCell, nBitmapMask // SergKis 11.11.21

   LOCAL nClrText := ::nClrText, ;
      nClrPane := ::nClrPane, ;
      nClrFocuFore := ::nClrFocuFore, ;
      nClrFocuBack := ::nClrFocuBack, ;
      nClrLine := ::nClrLine, ;
      nLineStyle, ;
      nClrSeleBack := ::nClrSeleBack, ;
      nClrSeleFore := ::nClrSeleFore, ;
      nHeightCell := ::nHeightCell, ;
      nHeightHead := iif( ::lDrawHeaders, ::nHeightHead, 0 ), ;
      nHeightFoot := iif( ::lDrawFooters != NIL .AND. ::lDrawFooters, ::nHeightFoot, 0 ), ;
      nHeightSuper := iif( ::lDrawHeaders, ::nHeightSuper, 0 ), ;
      nHeightSpecHd := iif( ::lDrawSpecHd, ::nHeightSpecHd, 0 )

   DEFAULT xRow := nRowPos, lDrawCell := .T.

   ::nPaintRow := xRow
   ::aDrawCols := {}

   IF Empty( ::aColumns )
      RETURN Self
   ENDIF

   IF _HMG_MainClientMDIHandle != 0 .AND. ! lFocused .AND. ::hWndParent == GetActiveMdiHandle()
      lFocused := .T.
   ENDIF

   ::lDrawSelect := .T.
   lSelected := ::lCanSelect .AND. ( AScan( ::aSelected, ::nAtPos ) > 0 )

   IF ( ::lNoLiteBar .OR. ( ::lNoGrayBar .AND. ! lFocused ) ) .AND. Empty( ::hBmpCursor )
      ::DrawLine() // don't want hilited cursor bar of any color
   ELSEIF ::nLen > 0

      nClrBack := iif( ::nPhantom = -1 .AND. ! Empty( ::aColumns ), ATail( ::aColumns ):nClrBack, nClrPane )
      nClrBack := iif( ValType( nClrBack ) == "B", Eval( nClrBack, ::nAt, Len( ::aColumns ), Self ), nClrBack )
      l3DLook := iif( ::nPhantom = -1 .AND. ! Empty( ::aColumns ), ATail( ::aColumns ):l3DLook, .F. )

      IF ::oPhant == NIL
         // "Phantom" column; :nPhantom hidden IVar
         ::oPhant := TSColumn():New( "", ; // cHeading
            {|| "" }, ; // bdata
            NIL, ; // cPicture
            { nClrText, nClrBack }, ; // aColors
            NIL, ; // aAlign
            ::nPhantom, ; // nWidth
            NIL, ; // lBitMap
            NIL, ; // lEdit
            NIL, ; // bValid
            .T., ; // lNoLite
            NIL, ; // cOrder
            NIL, ; // cFooting
            NIL, ; // bPrevEdit
            NIL, ; // bPostEdit
            NIL, ; // nEditMove
            NIL, ; // lFixLite
            { l3DLook }, ;
            NIL, ;
            Self )
         ::oPhant:cName := "oPhant"
         ::oPhant:nId := -1
      ELSE
         ::oPhant:nClrFore := nClrText
         ::oPhant:nClrBack := nClrBack
         ::oPhant:nWidth := ::nPhantom
         ::oPhant:l3DLook := l3DLook
      ENDIF

      AAdd( aColSizes, ::nPhantom )
      nJ := nStartCol := 0
      nLastCol := Len( ::aColumns ) + 1
      nBegin := Min( iif( ::nColPos <= ::nFreeze, ( ::nColPos := ::nFreeze + 1, ::nColPos - ::nFreeze ), ;
         ::nColPos - ::nFreeze ), nLastCol )

      IF ::lRowPosAtRec .AND. ::nRowCount() > 0
         IF Empty( ::aRowPosAtRec ) .OR. Len( ::aRowPosAtRec ) != ::nRowCount()
            ::aRowPosAtRec := Array( ::nRowCount() )
            AFill( ::aRowPosAtRec, 0 )
         ENDIF
         IF xRow == 0 .OR. xRow > Len( ::aRowPosAtRec )
         ELSE
            ::aRowPosAtRec[ xRow ] := ( nAt := ::nAtPos )
         ENDIF
      ENDIF

      IF ::bOnDrawLine != NIL
         IF IsArray( ::bOnDrawLine )
            FOR nI := 1 TO Len( ::bOnDrawLine )
                IF IsBlock( ::bOnDrawLine[ nI ] )
                   IF ! Empty( Eval( ::bOnDrawLine[ nI ], Self, xRow ) )
                      RETURN Self
                   ENDIF
                ENDIF
            NEXT
         ELSEIF ! Empty( Eval( ::bOnDrawLine, Self, xRow ) )
            RETURN Self
         ENDIF
      ENDIF

      IF ! ::lDrawLine
         nBegin := 1
         nLastCol := ::nColCount()
      ENDIF

      IF nAt == NIL
         nAt := ::nAtPos
      ENDIF

      FOR nI := nBegin TO nLastCol

         IF nStartCol >= nMaxWidth .AND. ::lDrawLine
            EXIT
         ENDIF

         nJ := iif( nI < ::nColPos, nJ + 1, nI )
         oColumn := iif( nJ > Len( ::aColumns ), ::oPhant, ::aColumns[ nJ ] )
         nLineStyle := ::nLineStyle
         nDeltaLen := ::GetDeltaLen( nJ, nStartCol, nMaxWidth, aColSizes )

         cCell := hb_ntos( nAt ) + "." + hb_ntos( oColumn:nId )
         lCell := .F.

         IF ::lFastDrawCell
            oCell := hb_HGetDef( ::aFastDrawCell, cCell, NIL )
            lCell := ISOBJECT( oCell )
         ENDIF

         nLineStyle := iif( HB_ISNUMERIC( oColumn:nLineStyle ), oColumn:nLineStyle, ::nLineStyle )
         hFont := ::hFontGet( oColumn, nJ )
         lAdjBmp := oColumn:lAdjBmp
         nAlign := oColumn:nAlign
         lOpaque := .T.
         lMulti := .F.
         cColAls := iif( '->' $ oColumn:cField, NIL, oColumn:cAlias )

         IF nJ == 1 .AND. ! Empty( ::hBmpCursor )

            uBmpCell := ::hBmpCursor
            uData := ""
            xData := ""
            nAlign := nMakeLong( oColumn:nAlign, oColumn:nAlign )
            lNoLite := .T.
            lAdjBmp := .F.
            lCheck := .F.

         ELSE

            IF ValType( oColumn:bSeek ) == 'B'
               IF cColAls != NIL
                  ( cColAls )->( Eval( oColumn:bSeek, Self, nJ ) )
               ELSE
                  Eval( oColumn:bSeek, Self, nJ )
               ENDIF
            ENDIF

            IF ::lIsArr .AND. ( ::lAppendMode .OR. ::nAt > Len( ::aArray ) )
               uData := "" // append mode for arrays
            ELSEIF lCell
               uData := oCell:uValue
            ELSE
               uData := ::bDataEval( oColumn, , nJ )
            ENDIF

            xData := uData
            lMulti := ValType( uData ) == "C" .AND. At( Chr( 13 ), uData ) > 0
            cPicture := ::cPictureGet( oColumn, nJ )
            lCheck := ( oColumn:lCheckBox .AND. ValType( uData ) == "L" .AND. oColumn:lVisible )
            lNoLite := oColumn:lNoLite
            nVertText := 0

            IF lCheck
               cPicture := ""
               nVertText := iif( uData, 3, 4 )
               lCheckVal := uData
            ENDIF

            uBmpCell := oColumn:uBmpCell

            IF nJ == ::nColSel .AND. ::uBmpSel != NIL .AND. lSelected
               uBmpCell := ::uBmpSel
               nAlign := nMakeLong( LoWord( nAlign ), ::nAligBmp )
            ELSEIF oColumn:lBitMap .AND. ValType( uData ) == "N"
               aBitMaps := iif( ValType( oColumn:aBitMaps ) == "A", oColumn:aBitMaps, ::aBitMaps )
               IF ! Empty( aBitMaps ) .AND. uData > 0 .AND. uData <= Len( aBitMaps )
                  uBmpCell := aBitMaps[ uData ]
               ENDIF
               nAlign := nMakeLong( LoWord( nAlign ), nAlign )
               uData := ""
            ELSEIF ! lCheck .AND. oColumn:lEmptyValToChar .AND. Empty( uData )
               uData := ""
            ELSEIF lCell
               uData := oCell:cValue
            ELSEIF Empty( cPicture ) .OR. lMulti
               IF ValType( uData ) != "C"
                  IF ValType( uData ) == "L"
                     uData := ::aMsg[ iif( uData, 1, 2 ) ]
                  ELSE
                     uData := cValToChar( uData )
                  ENDIF
               ENDIF
            ELSE
               uData := iif( uData == NIL, "", Transform( uData, cPicture ) )
            ENDIF
         ENDIF

         nAlign := ::nAlignGet( oColumn:nAlign, nJ, DT_LEFT )

         IF lNoLite
            IF ::lLiteBar
               nClrFore := ::GetValProp( oColumn:nClrFocuFore, nClrText, nJ, ::nAt )
               nClrBack := ::GetValProp( oColumn:nClrFocuBack, nClrPane, nJ, ::nAt )
               IF ! Empty( oColumn:cName ) .AND. oColumn:cName == "oPhant"
                  nClrBack := nClrPane
               ELSEIF ValType( nClrBack ) == "N" .AND. nClrBack < 0
                  nClrBack *= -1
               ENDIF
            ELSE
               nClrFore := ::GetValProp( oColumn:nClrFore, nClrText, nJ, ::nAt )
               nClrBack := ::GetValProp( oColumn:nClrBack, nClrPane, nJ, ::nAt )
            ENDIF

            nCursor := 0
         ELSE
            IF ( nClrFore := iif( lFocused, oColumn:nClrFocuFore, oColumn:nClrSeleFore ) ) == NIL
               nClrFore := iif( lFocused, nClrFocuFore, nClrSeleFore )
            ENDIF

            nClrFore := ::GetValProp( nClrFore, nClrFore, nJ, ::nAt )

            IF ( nClrBack := iif( lFocused, oColumn:nClrFocuBack, oColumn:nClrSeleBack ) ) == NIL
               nClrBack := iif( lFocused, nClrFocuBack, nClrSeleBack )
            ENDIF

            nClrBack := ::GetValProp( nClrBack, nClrBack, nJ, ::nAt )

            IF ValType( nClrBack ) == "N" .AND. nClrBack < 0
               nCursor := Abs( nClrBack )
               nClrBack := ::GetValProp( oColumn:nClrBack, nClrPane, nJ, ::nAt )
            ELSE
               nCursor := 0
            ENDIF
         ENDIF

         IF ValType( nClrBack ) == "A"
            nClrBack := ::nClrBackArr( nClrBack, nJ, ::nAt )
            nClrTo := nClrBack[ 2 ]
            nClrBack := nClrBack[ 1 ]
         ELSE
            nClrTo := nClrBack
         ENDIF

         lBrush := ValType( nClrBack ) == "O"
         l3DLook := oColumn:l3DLook

         hBitMap := iif( ValType( uBmpCell ) == "B" .AND. ! ::lPhantArrRow, Eval( uBmpCell, nJ, Self ), uBmpCell )
         hBitMap := iif( ValType( hBitMap ) == "O" .AND. ! ::lPhantArrRow, Eval( ::bBitMapH, hBitMap ), hBitMap )

         DEFAULT hBitMap := 0

         IF lCheck
            IF lCell
               hBitMap := oCell:hBitMap
               nAlign := oCell:nAlign
            ELSE
               DEFAULT ::aCheck := { StockBmp( 6 ), StockBmp( 7 ) }
               IF ValType( oColumn:aCheck ) == "A"
                  hBitMap := oColumn:aCheck[ iif( lCheckVal, 1, 2 ) ]
               ELSE
                  hBitMap := ::aCheck[ iif( lCheckVal, 1, 2 ) ]
               ENDIF
               nAlign := nMakeLong( DT_CENTER, DT_CENTER )
            ENDIF
            uData := ""
         ENDIF

         IF oColumn:l3DTextCell != NIL
            l3DText := oColumn:l3DTextCell
            nClr3dL := oColumn:nClr3DLCell
            nClr3dS := oColumn:nClr3DSCell
            nClr3dL := iif( ValType( nClr3dL ) == "B", Eval( nClr3dL, ::nAt, nJ, Self ), nClr3dL )
            nClr3dS := iif( ValType( nClr3dS ) == "B", Eval( nClr3dS, ::nAt, nJ, Self ), nClr3dS )
         ELSE
            l3DText := nClr3dL := nClr3dS := NIL
         ENDIF

         oColumn:nEditWidthDraw := 0

         IF nDeltaLen > 0
            oColumn:nEditWidthDraw := aColSizes[ nJ ] + nDeltaLen
         ENDIF

         IF ! lCell
            IF nAlign != DT_CENTER .AND. ::nCellMarginLR != NIL
               uData := ::CellMarginLeftRight( nJ, uData, oColumn, nAlign, lMulti, 0 )
            ENDIF
         ENDIF
         // SergKis 11.11.21
         nBitmapMask := oColumn:nBmpMaskCell

         IF Empty( oColumn:oCell )
            oColumn:oCell := TSBcell():New()
         ENDIF

         oCell := oColumn:oCell
         WITH OBJECT oCell
            :nRow := nRowPos
            :nCol := nStartCol
            :nWidth := aColSizes[ nJ ] + nDeltaLen
            :nHeight := ::nHeightCell
            :nCell := nJ
            :uValue := xData
            :lDrawLine := .F. // DrawSelect()

            :hWnd := hWnd // 1
            :hDC := hDC // 2
            :xRow := nRowPos // 3
            :nStartCol := nStartCol // 4
            :nSize := aColSizes[ nJ ] + nDeltaLen // 5 aColSizes[ nJ ] + nDeltaLen
            :uData := uData // 6
            :nAlign := nAlign // 7
            :nClrFore := nClrFore // 8
            :nClrBack := nClrBack // 9
            :hFont := hFont // 10
            :hBitMap := hBitMap // 11
            :nHeightCell := nHeightCell // 12
            :l3DLook := l3DLook // 13 oColumn:l3DLook
            :nLineStyle := nLineStyle // 14
            :nClrLine := nClrLine // 15
            :nDrawType := 0 // 16 line/header/footer/super
            :nHeightHead := nHeightHead // 17
            :nHeightFoot := nHeightFoot // 18
            :nHeightSuper := nHeightSuper // 19
            :nHeightSpecHd := nHeightSpecHd // 20
            :lAdjBmp := lAdjBmp // 21
            :lMultiline := lMulti // 22
            :nVAlign := nVAlign // 23
            :nVertText := nVertText // 24
            :nClrTo := nClrTo // 25
            :lOpaque := lOpaque // 26
            :hBrush := iif( lBrush, nClrBack:hBrush, 0 ) // 27
            :l3DText := l3DText // 28  3D text
            :nClr3dL := nClr3dL // 29  3D text light color
            :nClr3dS := nClr3dS // 30  3D text shadow color
            :nCursor := nCursor // 31  Rect cursor
            :lInvertColor := !( ::lCellBrw .AND. nJ != ::nCell ) // 32  Invert color
            :nBitmapMask := nBitmapMask // SergKis 11.11.21 33
         END WITH

         IF ISBLOCK( oColumn:bDrawCell )
            Eval( oColumn:bDrawCell, Self, oColumn:oCell, oColumn )
         ENDIF
         IF lDrawCell .AND. ::lDrawLine
            lDraw := ::TSDrawCell( oColumn:oCell, oColumn )
         ELSE
            lDraw := .T.
         ENDIF
         IF ::lFastDrawCell .AND. ! lCell
            hb_HSet( ::aFastDrawCell, cCell, __objClone( oColumn:oCell ) )
         ENDIF

         nStartCol += aColSizes[ nJ ] + nDeltaLen

         IF lDraw
            AAdd( ::aDrawCols, nJ )
         ENDIF

      NEXT

   ENDIF

   IF ::bOnDraw != NIL
      Eval( ::bOnDraw, Self )
   ENDIF

   IF ::lCellBrw
      cMsg := iif( ! Empty( ::AColumns[ ::nCell ]:cMsg ), ::AColumns[ ::nCell ]:cMsg, ::cMsg )
      cMsg := iif( ValType( cMsg ) == "B", Eval( cMsg, Self, ::nCell ), cMsg )

      IF ! Empty( cMsg )
         ::SetMsg( cMsg )
      ELSE
         ::nStatusItem := 0
      ENDIF
   ENDIF

   ::lDrawSelect := .F.

RETURN Self

* ============================================================================
* METHOD TSBrowse:DrawSuper() Version 9.0 Nov/30/2009
* ============================================================================

METHOD DrawSuper( lDrawCell ) CLASS TSBrowse

   LOCAL nI, nJ, nBegin, nStartCol, l3DLook, nClrFore, lAdjBmp, nClrTo, lOpaque, nClrBack, hFont, cHeading, hBitMap, ;
      lMulti, nHAlign, nVAlign, nWidth, nS, nLineStyle, lBrush, ;
      nMaxWidth := ::nWidth(), ;
      aColSizes := AClone( ::aColSizes ), ; // use local copies for speed
      aSuperHead := AClone( ::aSuperHead ), ;
      nHeightHead := ::nHeightHead, ;
      nHeightFoot := ::nHeightFoot, ;
      nHeightSuper := ::nHeightSuper, ;
      nHeightSpecHd := ::nHeightSpecHd

   LOCAL hWnd := ::hWnd, ;
      hDC := ::hDc, ;
      nClrText := ::nClrText, ;
      nClrPane := ::nClrPane, ;
      nClrLine := ::nClrLine

   LOCAL l3DText, nClr3dL, nClr3dS, oCol, aDrawCols, oSupHd, aSupHd, nBitmapMask // SergKis 11.11.21

   IF Empty( ::aColumns )
      RETURN NIL
   ENDIF

   DEFAULT lDrawCell := ::lDrawLine, nBitmapMask := 0x008800C6 // SergKis 11.11.21 SRCAND

   ::DrawSelect( , .F. )
   aDrawCols := ::aDrawCols // create current draw columns array

   nClrFore := ::nForeSupHdGet( 1, aSuperHead )
   nClrBack := ::nBackSupHdGet( 1, aSuperHead )
   l3DLook := aSuperHead[ 1, 6 ]
   hFont := ::hFontSupHdGet( 1, aSuperHead )
   nLineStyle := aSuperHead[ 1, 10 ]
   nClrLine := aSuperHead[ 1, 11 ]

   nBegin := nI := 1

   WHILE nI <= Len( aSuperHead )

      IF aSuperHead[ nI, 1 ] > nBegin
         nJ := aSuperHead[ nI, 1 ] - 1
         ASize( aSuperHead, Len( aSuperHead ) + 1 )
         AIns( aSuperHead, nI )
         aSuperHead[ nI ] := { nBegin, nJ, "", nClrFore, nClrBack, l3DLook, hFont, .F., .F., nLineStyle, ;
            nClrLine, 1, 1, .F., nBitmapMask } // SergKis 11.11.21
         nBegin := nJ + 1
      ELSE
         nBegin := aSuperHead[ nI++, 2 ] + 1
      ENDIF

   ENDDO

   nI := Len( aSuperHead )
   nClrFore := ::nForeSupHdGet( nI, aSuperHead )
   nClrBack := ::nBackSupHdGet( nI, aSuperHead )
   l3DLook := aSuperHead[ nI, 6 ]
   hFont := ::hFontSupHdGet( nI, aSuperHead )
   nLineStyle := aSuperHead[ nI, 10 ]
   nClrLine := aSuperHead[ nI, 11 ]

   IF ( nI := ATail( aSuperHead )[ 2 ] ) < Len( ::aColumns )
      AAdd( aSuperHead, { nI + 1, Len( ::aColumns ), "", nClrFore, nClrBack, l3DLook, hFont, .F., .F., nLineStyle, ;
         nClrLine, 1, 1, .F., nBitmapMask } ) // SergKis 11.11.21
   ENDIF

   nStartCol := nWidth := 0

   IF ::lAdjColumn

      nS := 1

      FOR nI := 1 TO Len( ::aColumns )
         oCol := ::aColumns[ nI ]
         IF oCol:nEditWidthDraw > 0
            aColSizes[ nI ] := oCol:nEditWidthDraw - iif( ::lNoVScroll, GetVScrollBarWidth(), 0 )
         ELSE
            aColSizes[ nI ] := oCol:nWidth
         ENDIF
      NEXT

      FOR nI := 1 TO Len( aSuperHead )
         FOR nJ := aSuperHead[ nI, 1 ] TO aSuperHead[ nI, 2 ]
            IF nI == 1 .AND. AScan( aDrawCols, nJ ) > 0
               nWidth += aColSizes[ nJ ]
            ENDIF
         NEXT
      NEXT

   ELSE

      nBegin := iif( ::nColPos == ::nFreeze + 1, ::nColPos - ::nFreeze, ::nColPos )

      FOR nS := 1 TO Len( aSuperHead )

         IF nBegin >= aSuperHead[ nS, 1 ] .AND. nBegin <= aSuperHead[ nS, 2 ]

            DO CASE
            CASE nBegin > aSuperHead[ nS, 1 ] .AND. nS == 1
               FOR nJ := aSuperHead[ nS, 1 ] TO nBegin - 1
                  nStartCol -= ::aColSizes[ nJ ]
               NEXT

               FOR nJ := aSuperHead[ nS, 1 ] TO aSuperHead[ nS, 2 ]
                  nWidth += aColSizes[ nJ ]
               NEXT

            CASE nBegin > aSuperHead[ nS, 1 ] .AND. nS > 1
               FOR nJ := 1 TO ::nFreeze
                  nStartCol += ::aColSizes[ nJ ]
               NEXT

               FOR nJ := nBegin TO aSuperHead[ nS, 2 ]
                  nWidth += aColSizes[ nJ ]
               NEXT

            OTHERWISE
               IF nBegin > 1
                  FOR nJ := 1 TO ::nFreeze
                     nStartCol += ::aColSizes[ nJ ]
                  NEXT
               ENDIF

               FOR nJ := aSuperHead[ nS, 1 ] TO aSuperHead[ nS, 2 ]
                  nWidth += aColSizes[ nJ ]
               NEXT

            ENDCASE

            EXIT

         ENDIF

      NEXT

   ENDIF

   FOR nI := nS TO Len( aSuperHead ) + 1

      IF nStartCol > nMaxWidth .AND. lDrawCell
         EXIT
      ENDIF

      IF nI <= Len( aSuperHead )
         nClrFore := ::nForeSupHdGet( nI, aSuperHead )
         nClrBack := ::nBackSupHdGet( nI, aSuperHead )
         lBrush := ValType( nClrBack ) == "O"

         IF ValType( nClrBack ) == "A"
            nClrBack := ::nClrBackArr( nClrBack, nI )
            nClrTo := nClrBack[ 2 ]
            nClrBack := nClrBack[ 1 ]
         ELSE
            nClrTo := nClrBack
         ENDIF

         cHeading := ::cTextSupHdGet( nI, aSuperHead )
         lMulti := ValType( cHeading ) == "C" .AND. At( Chr( 13 ), cHeading ) > 0

         l3DLook := aSuperHead[ nI, 6 ]
         hFont := ::hFontSupHdGet( nI, aSuperHead )
         hBitMap := aSuperHead[ nI, 8 ]
         hBitMap := iif( ValType( hBitMap ) == "B", Eval( hBitMap ), hBitMap )
         hBitMap := iif( ValType( hBitMap ) == "O", Eval( ::bBitMapH, hBitMap ), hBitMap )
         lAdjBmp := aSuperHead[ nI, 9 ]
         nLineStyle := aSuperHead[ nI, 10 ]
         nClrLine := aSuperHead[ nI, 11 ]
         nHAlign := aSuperHead[ nI, 12 ]
         nVAlign := aSuperHead[ nI, 13 ]
         lOpaque := aSuperHead[ nI, 14 ]
         // SergKis 11.11.21
         nBitmapMask := aSuperHead[ nI, 15 ]

         DEFAULT hBitMap := 0, ;
            lOpaque := .T.
            lOpaque := ! lOpaque
      ELSE
         cHeading := ""
         nWidth := ::nPhantom
         hBitmap := 0
         lOpaque := .F.
         nClrBack := iif( ::nPhantom == -2, nClrPane, ATail( aSuperHead )[ 5 ] )
         nClrBack := ::GetValProp( nClrBack, nClrBack, nI )
         IF ValType( nClrBack ) == "A"
            nClrBack := ::nClrBackArr( nClrBack, nI )
            nClrTo := nClrBack[ 2 ]
            nClrBack := nClrBack[ 1 ]
         ELSE
            nClrTo := nClrBack
         ENDIF
      ENDIF

      IF nI <= Len( aSuperHead ) .AND. ::aColumns[ aSuperHead[ nI, 1 ] ]:l3DTextHead != NIL
         l3DText := ::aColumns[ aSuperHead[ nI, 1 ] ]:l3DTextHead
         nClr3dL := ::aColumns[ aSuperHead[ nI, 1 ] ]:nClr3DLHead
         nClr3dS := ::aColumns[ aSuperHead[ nI, 1 ] ]:nClr3DSHead
         nClr3dL := iif( ValType( nClr3dL ) == "B", Eval( nClr3dL, 0, nStartCol ), nClr3dL )
         nClr3dS := iif( ValType( nClr3dS ) == "B", Eval( nClr3dS, 0, nStartCol ), nClr3dS )
      ELSE
         l3DText := nClr3dL := nClr3dS := NIL
      ENDIF

      IF aSupHd == NIL
         aSupHd := {}
      ENDIF

      oSupHd := TSBcell():New()

      oSupHd:nRow := 0
      oSupHd:nCol := nStartCol
      oSupHd:nWidth := nWidth
      oSupHd:nHeight := ::nHeightSuper
      oSupHd:nCell := nI
      oSupHd:uValue := cHeading
      oSupHd:lDrawLine := .F. // DrawLine()

      IF nI <= Len( aSuperHead )
         oSupHd:nFromCol := aSuperHead[ nI, 1 ]
         oSupHd:nToCol := aSuperHead[ nI, 2 ]
         oSupHd:nBitmapMask := nBitmapMask // SergKis 11.11.21 33
      ENDIF

      oSupHd:hWnd := hWnd // 1
      oSupHd:hDC := hDC // 2
      oSupHd:xRow := 0 // 3
      oSupHd:nStartCol := nStartCol // 4
      oSupHd:nSize := nWidth // 5
      oSupHd:uData := cHeading // 6
      oSupHd:nAlign := nHAlign // 7
      oSupHd:nClrFore := nClrFore // 8
      oSupHd:nClrBack := nClrBack // 9
      oSupHd:hFont := hFont // 10
      oSupHd:hBitMap := hBitMap // 11
      oSupHd:nHeightCell := nHeightHead // 12
      oSupHd:l3DLook := l3DLook // 13 oColumn:l3DLook
      oSupHd:nLineStyle := nLineStyle // 14
      oSupHd:nClrLine := nClrLine // 15
      oSupHd:nDrawType := 3 // 16 line/header/footer/super
      oSupHd:nHeightHead := nHeightHead // 17
      oSupHd:nHeightFoot := nHeightFoot // 18
      oSupHd:nHeightSuper := nHeightSuper // 19
      oSupHd:nHeightSpecHd := nHeightSpecHd // 20
      oSupHd:lAdjBmp := lAdjBmp // 21
      oSupHd:lMultiline := lMulti // 22 Multiline text
      oSupHd:nVAlign := nVAlign // 23
      oSupHd:nVertText := 0 // 24 nVertLine
      oSupHd:nClrTo := nClrTo // 25
      oSupHd:lOpaque := lOpaque // 26
      oSupHd:hBrush := iif( lBrush, nClrBack:hBrush, 0 ) // 27
      oSupHd:l3DText := l3DText // 28  3D text
      oSupHd:nClr3dL := nClr3dL // 29  3D text light color
      oSupHd:nClr3dS := nClr3dS // 30  3D text shadow color
      oSupHd:nCursor := 0 // 31  Rect cursor
      oSupHd:lInvertColor := .F. // 32  Invert color

      AAdd( aSupHd, oSupHd )

      IF lDrawCell
         ::TSDrawCell( oSupHd )
      ENDIF

      nStartCol += nWidth

      nWidth := 0

      IF nI < Len( aSuperHead )

         FOR nJ := aSuperHead[ nI + 1, 1 ] TO aSuperHead[ nI + 1, 2 ]
            IF ::lAdjColumn
               IF AScan( aDrawCols, nJ ) > 0
                  nWidth += aColSizes[ nJ ]
               ENDIF
            ELSE
               nWidth += aColSizes[ nJ ]
            ENDIF
         NEXT

      ENDIF

   NEXT

RETURN aSupHd

* ============================================================================
* METHOD TSBrowse:Edit() Version 9.0 Nov/30/2009
* ============================================================================

METHOD Edit( uVar, nCell, nKey, nKeyFlags, cPicture, bValid, nClrFore, nClrBack ) CLASS TSBrowse

   LOCAL nRow, nHeight, cType, uValue, nI, aGet, oCol, cMsg, aRct, bChange, lSpinner, bUp, bDown, ;
      bMin, bMax, nStartX, nWidth, lCombo, lMulti, nCol, lLogicDrop, lPicker, nTxtHeight, hFont, ix
   LOCAL cWnd := ::cControlName
   LOCAL nK, aKey, oGet, cTmp

   DEFAULT nCell := ::nCell, ;
      ::lPostEdit := .F., ;
      ::lNoPaint := .F.

   IF ::lPhantArrRow
      RETURN NIL
   ENDIF

   oCol := ::aColumns[ nCell ]
   oCol:xOldEditValue := ::bDataEval( oCol, , nCell ) // Igor Nazarov

   DEFAULT ::nHeightSuper := 0, ;
      nKey := VK_RETURN, ;
      nKeyFlags := 0, ;
      uVar := ::bDataEval( oCol ), ;
      cPicture := oCol:cPicture, ;
      bValid := oCol:bValid, ;
      nClrFore := oCol:nClrEditFore, ;
      nClrBack := oCol:nClrEditBack

   IF ValType( ::lInsertMode ) == "L" // Igor Nazarov
      IF IsInsertActive() != ::lInsertMode
         iif( _HMG_IsXPorLater, KeyToggleNT( VK_INSERT ), KeyToggle( VK_INSERT ) )
      ENDIF
   ENDIF

   uValue := uVar
   cType := iif( Empty( oCol:cDataType ), ValType( uValue ), oCol:cDataType )
   IF cType != "M" .AND. ::lIsArr .AND. oCol:cDataType # ValType( uValue ) // GF 15/07/2009
      cType := ValType( uValue )
      oCol:cDataType := cType
   ENDIF
   cMsg := oCol:cMsgEdit
   bChange := oCol:bChange
   lSpinner := oCol:lSpinner
   bUp := oCol:bUp
   bDown := oCol:bDown
   bMin := oCol:bMin
   bMax := oCol:bMax
   nStartX := 0
   lCombo := lMulti := .F.
   ::oGet := ::bValid // JP
   ::bValid := {|| ! ::lEditing }
   // JP 1.58
   IF ! oCol:lVisible
      ::lChanged := .F.
      ::lPostEdit := .F.
      ::oWnd:nLastKey := VK_RIGHT
      ::PostEdit( uVar, nCell, bValid )
      RETURN NIL
   ENDIF
   // End
   IF oCol:bPassWord != NIL
      IF ! Eval( oCol:bPassWord, uValue, nCell, ::nAt, Self )
         RETURN NIL
      ENDIF
   ENDIF

   lLogicDrop := ::lLogicDrop
   lPicker := ::lPickerMode // MWS Sep 20/07
   IF cType == 'T'
      lPicker := ( hb_Hour( uValue ) == 0 .AND. hb_Minute( uValue ) == 0 .AND. hb_Sec( uValue ) == 0 )
   ENDIF

   ::lEditing := .T.
   ::lHitBottom := .F.

   IF ::nLen > 0
      ::lNoPaint := .T.
   ENDIF

   IF oCol:bPrevEdit != NIL
      IF ::lIsArr .AND. ( ::lAppendMode .OR. ::nAt > Len( ::aArray ) ) // append mode for arrays
      ELSEIF nKey != VK_RETURN // GF 15-10-2015
         uVar := Eval( oCol:bPrevEdit, uValue, Self, nCell, oCol )
         IF ValType( uVar ) == "A"
            uValue := uVar[ 2 ]
            uVar := uVar[ 1 ]
         ENDIF
         IF ValType( uVar ) == "L" .AND. ! uVar
            nKey := VK_RETURN
         ENDIF
      ENDIF
   ENDIF

   ::FastDrawClear( hb_ntos( ::nAtPos ) + "." + hb_ntos( oCol:nId ) )

   cMsg := iif( ValType( cMsg ) == "B", Eval( cMsg, Self, nCell ), cMsg )

   IF cType == "L" .AND. oCol:lCheckBox

      IF nKey != VK_RETURN .OR. ! oCol:lCheckBoxNoReturn .OR. ::lCheckBoxAllReturn

         IF Upper( Chr( nKey ) ) $ "YCST1"
            ::lChanged := ( uVar == .F. )
            uVar := .T.
         ELSEIF Upper( Chr( nKey ) ) $ "FN0"
            ::lChanged := ( uVar == .T. )
            uVar := .F.
         ELSEIF nKey == VK_SPACE .OR. nKey == VK_RETURN
            uVar := ! uValue
            ::lChanged := .T.
         ELSE
            RETURN 0
         ENDIF

         ::lHasChanged := iif( ::lChanged, .T., ::lHasChanged )
         ::oWnd:nLastKey := VK_RETURN
         ::PostEdit( uVar, nCell )
         ::lPostEdit := .F.
         RETURN 0

      ELSE

         ::lPostEdit := .T.
         ::lChanged := .F.
         ::oWnd:nLastKey := nKey
         ::PostEdit( uValue, nCell )
         ::lPostEdit := .F.
         RETURN 0

      ENDIF
   ENDIF

   IF oCol:bExtEdit != NIL // external edition
      ::lNoPaint := ::lEditing := .F.
      uVar := Eval( oCol:bExtEdit, uValue, Self )
      ::lChanged := ( ValType( uVar ) != ValType( uValue ) .OR. uVar != uValue )
      ::lPostEdit := .T.
      ::oWnd:nLastKey := VK_RETURN
      ::PostEdit( uVar, nCell, bValid )
      RETURN NIL
   ENDIF

   hFont := iif( oCol:hFontEdit != NIL, oCol:hFontEdit, ;
      iif( oCol:hFont != NIL, oCol:hFont, ::hFont ) )

   IF oCol:oEdit != NIL
      oCol:oEdit:End()
      oCol:oEdit := NIL
   ENDIF

   IF ::nFreeze > 0
      FOR nI := 1 TO Min( ::nFreeze, nCell - 1 )
         nStartX += ::GetColSizes()[ nI ]
      NEXT
   ENDIF

   FOR nI := ::nColPos TO nCell - 1
      nStartX += ::GetColSizes()[ nI ]
   NEXT

   nClrFore := iif( ValType( nClrFore ) == "B", ;
      Eval( nClrFore, ::nAt, nCell, Self ), nClrFore )

   nClrBack := iif( ValType( nClrBack ) == "B", ;
      Eval( nClrBack, ::nAt, nCell, Self ), nClrBack )

   IF ::nColSpecHd != 0
      nRow := ::nHeightHead + ::nHeightSuper + iif( oCol:l3DLook, 2, 0 )
      nCol := nStartX + iif( oCol:l3DLook, 2, 0 )
      nWidth := ::GetColSizes()[ nCell ] - iif( oCol:l3DLook, 2, 1 )
      nHeight := ::nHeightSpecHd - iif( oCol:l3DLook, 1, -1 )
   ELSE
      nRow := ::nRowPos - 1
      nRow := ( nRow * ::nHeightCell ) + ::nHeightHead + ;
         ::nHeightSuper + ::nHeightSpecHd + iif( oCol:l3DLook, 2, 0 )
      nCol := nStartX + iif( oCol:l3DLook, 2, 0 )
      nWidth := ::GetColSizes()[ nCell ] - iif( oCol:l3DLook, 2, 0 )
      nHeight := ::nHeightCell - iif( oCol:l3DLook, 1, -1 )
   ENDIF

   IF oCol:nEditWidthDraw > 0
      nWidth := oCol:nEditWidthDraw
      IF ! ::lNoVScroll
         nWidth -= GetVScrollBarWidth()
      ENDIF
   ENDIF

   IF oCol:cResName != NIL .OR. oCol:lBtnGet

      nRow += ::aEditCellAdjust[ 1 ]
      nCol += ::aEditCellAdjust[ 2 ]
      nWidth += ::aEditCellAdjust[ 3 ]
      nHeight += ::aEditCellAdjust[ 4 ]

      IF oCol:nEditWidth > 0
         nWidth := oCol:nEditWidth
      ENDIF

      IF oCol:nEditHeight > 0
         nHeight := oCol:nEditHeight
      ENDIF

      IF oCol:nEditRow > 0
         nRow := oCol:nEditRow
      ENDIF

      IF oCol:nEditCol > 0
         nCol := oCol:nEditCol
      ENDIF

      ::cChildControl := HMG_GetUniqueName( "BtnBox" )

      oCol:oEdit := TBtnBox():New( nRow, nCol, bSETGET( uValue ), Self, nWidth, nHeight, ;
         cPicture, nClrFore, nClrBack, hFont, ::cChildControl, cWnd, ;
         cMsg, bChange, bValid, oCol:cResName, oCol:bAction, ;
         lSpinner .AND. cType $ "ND", bUp, bDown, ;
         bMin, bMax, oCol:nBmpWidth, nCell )

      oCol:oEdit:lAppend := ::lAppendMode
      oCol:oEdit:Hide()

   ELSEIF ( cType == "C" .AND. Chr( 13 ) $ uValue ) .OR. cType == "M" .OR. oCol:lEditBox

      IF oCol:lEditBox .AND. ! Empty( uValue := Trim( uValue ) )
         IF Len( oCol:cEditBoxSep ) > 0 .AND. oCol:cEditBoxSep != CRLF .AND. oCol:cEditBoxSep $ uValue
            uValue := StrTran( uValue, oCol:cEditBoxSep, CRLF )
         ENDIF
         IF oCol:nEditBoxWrap > 0
            cTmp := uValue
            nK := MLCount( cTmp, oCol:nEditBoxWrap, , .T. )
            uValue := ""
            FOR nI := 1 TO nK
               uValue += Trim( MemoLine( cTmp, oCol:nEditBoxWrap, nI, , .T. ) )
               IF nI != nK
                  uValue += CRLF
               ENDIF
            NEXT
         ENDIF
      ENDIF

      IF ::nMemoHE == NIL

         IF ! Empty( uValue )
            nHeight := Max( 5, StrCharCount( uValue, Chr( 10 ) ) )
         ELSE
            nHeight := 5
         ENDIF

      ELSE
         nHeight := ::nMemoHE
      ENDIF

      aRct := ::GetCliRect( ::hWnd )
      IF ::nMemoWE == NIL .OR. Empty( ::nMemoWE )
         nWidth := Max( nWidth, GetTextWidth( 0, SubStr( uValue, 1, ;
            At( Chr( 13 ), uValue ) - 1 ), ;
            iif( hFont != NIL, hFont, 0 ) ) )
         nWidth := Min( nWidth, Int( aRct[ 3 ] * .8 ) )
      ELSE
         nWidth := ::nMemoWE
      ENDIF

      nTxtHeight := SBGetHeight( ::hWnd, iif( hFont != NIL, hFont, 0 ), 0 )

      WHILE ( nRow + ( nTxtHeight * nHeight ) ) > aRct[ 4 ]
         nRow -= nTxtHeight
      ENDDO

      nI := nCol + nWidth - aRct[ 3 ]
      nCol -= iif( nI <= 0, 0, nI )
      nCol := Max( 10, nCol )
      nHeight *= nTxtHeight

      nRow += ::aEditCellAdjust[ 1 ]
      nCol += ::aEditCellAdjust[ 2 ]
      nWidth += ::aEditCellAdjust[ 3 ]
      nHeight += ::aEditCellAdjust[ 4 ]

      ::cChildControl := HMG_GetUniqueName( "EditBox" )

      oCol:oEdit := TSMulti():New( nRow, nCol, bSETGET( uValue ), Self, nWidth, nHeight, ;
         hFont, nClrFore, nClrBack, ::cChildControl, cWnd )
      oCol:oEdit:bGotFocus := {|| oCol:oEdit:HideSel(), oCol:oEdit:SetPos( 0 ) }
      lMulti := .T.
      IF oCol:lEditBoxROnly
         oCol:oEdit:SendMsg( EM_SETREADONLY, 1, 0 )
      ENDIF
      oCol:oEdit:Hide()

   ELSEIF ( cType == "L" .AND. lLogicDrop ) .OR. oCol:lComboBox

      lCombo := .T.

      IF oCol:lComboBox

         aGet := oCol:aItems
         IF Empty( aGet )
            RETURN NIL
         ENDIF

         IF nKey == VK_RETURN
            IF oCol:cDataType != NIL .AND. oCol:cDataType == "N"
               IF oCol:aData <> NIL
                  uValue := Max( 1, AScan( aGet, uValue ) )
               ELSE
                  uValue := iif( uValue < 1 .OR. uValue > Len( aGet ), 1, uValue )
               ENDIF
            ELSE
               uValue := Max( 1, AScan( aGet, uValue ) )
            ENDIF
         ELSE
            uValue := Max( 1, AScan( aGet, Upper( Chr( nKey ) ) ) )
         ENDIF

         IF ValType( ::bDataEval( oCol ) ) == "N"
            nWidth := 0
            AEval( aGet, {| x | nWidth := Max( Len( x ), nWidth ) } )
            nWidth := Max( GetTextWidth( 0, Replicate( 'B', nWidth ), hFont ), oCol:nWidth )
         ENDIF

         nHeight := Max( 10, Min( 10, Len( aGet ) ) ) * ::nHeightCell

      ELSE

         aGet := { ::aMsg[ 1 ], ::aMsg[ 2 ] }

         IF nKey == VK_RETURN
            uValue := iif( uValue, 1, 2 )
         ELSE
            uValue := Max( 1, AScan( aGet, Upper( Chr( nKey ) ) ) )
         ENDIF

         nHeight := ::nHeightCell * 4 // 1.54

      ENDIF

      nRow += ::aEditCellAdjust[ 1 ]
      nCol += ::aEditCellAdjust[ 2 ]
      nWidth += ::aEditCellAdjust[ 3 ]
      nHeight += ::aEditCellAdjust[ 4 ]

      IF oCol:nEditWidth > 0
         nWidth := oCol:nEditWidth
      ENDIF

      IF oCol:nEditHeight > 0
         nHeight := oCol:nEditHeight
      ENDIF

      IF oCol:nEditRow > 0
         nRow := oCol:nEditRow
      ENDIF

      IF oCol:nEditCol > 0
         nCol := oCol:nEditCol
      ENDIF

      ::cChildControl := HMG_GetUniqueName( "ComboBox" )

      oCol:oEdit := TComboBox():New( nRow, nCol, bSETGET( uValue ), aGet, nWidth, nHeight, ;
         Self, bChange, nClrFore, nClrBack, hFont, cMsg, ::cChildControl, cWnd )

      oCol:oEdit:lAppend := ::lAppendMode

   ELSEIF ( cType $ "DT" ) .AND. lPicker // MWS Sep 20/07

      nRow -= 2
      nHeight := Max( ::nHeightCell, 19 )

      nRow += ::aEditCellAdjust[ 1 ]
      nCol += ::aEditCellAdjust[ 2 ]
      nWidth += ::aEditCellAdjust[ 3 ]
      nHeight += ::aEditCellAdjust[ 4 ]

      IF oCol:nEditWidth > 0
         nWidth := oCol:nEditWidth
      ENDIF
      IF oCol:nEditHeight > 0
         nHeight := oCol:nEditHeight
      ENDIF
      IF oCol:nEditRow > 0
         nRow := oCol:nEditRow
      ENDIF
      IF oCol:nEditCol > 0
         nCol := oCol:nEditCol
      ENDIF

      ::cChildControl := HMG_GetUniqueName( "DatePicker" )

      oCol:oEdit := TDatePicker():New( nRow, nCol, bSETGET( uValue ), Self, nWidth, nHeight, ;
         cPicture,, nClrFore, nClrBack, hFont, ::cChildControl,, cWnd, ;
         cMsg,,,,, bChange,,, ::lShowNone, ::lUpDown )
      oCol:oEdit:Hide()

   ELSE

      ix := GetControlIndex ( cWnd, ::cParentWnd )
      IF _HMG_aControlContainerRow[ ix ] == -1
         nRow += ::nTop - 1
         nCol += ::nLeft
      ELSE
         nRow += _HMG_aControlRow[ ix ] - 1
         nCol += _HMG_aControlCol[ ix ]
      ENDIF

      nRow += ::aEditCellAdjust[ 1 ]
      nCol += ::aEditCellAdjust[ 2 ]
      nWidth += ::aEditCellAdjust[ 3 ] + 2
      nHeight += ::aEditCellAdjust[ 4 ] + 2

      IF oCol:nEditWidth > 0
         nWidth := oCol:nEditWidth
      ENDIF
      IF oCol:nEditHeight > 0
         nHeight := oCol:nEditHeight
      ENDIF
      IF oCol:nEditRow > 0
         nRow := oCol:nEditRow
      ENDIF
      IF oCol:nEditCol > 0
         nCol := oCol:nEditCol
      ENDIF

      IF oCol:cEditPicture != NIL
         cPicture := oCol:cEditPicture
      ENDIF

      ::cChildControl := HMG_GetUniqueName( "GetBox" )

      oCol:oEdit := TGetBox():New( nRow, nCol, bSETGET( uValue ), Self, nWidth, nHeight, ;
         cPicture,, nClrFore, nClrBack, hFont, ::cChildControl, cWnd, ;
         cMsg,,,,, bChange, .T.,, lSpinner .AND. cType $ "ND", bUp, bDown, ;
         bMin, bMax, oCol:lNoMinus )

      IF oCol:nEditAlign != NIL
         _SetAlign( ::cChildControl, ::cParentWnd, { "LEFT", "CENTER", "RIGHT" }[ oCol:nEditAlign + 1 ] )
      ENDIF

      IF ! Empty( oCol:aKeyEvent )
         oGet := oCol:oEdit:oGet
         FOR nK := 1 TO Len( oCol:aKeyEvent )
            aKey := oCol:aKeyEvent[ nK ]
            IF HB_ISNUMERIC( aKey[ 1 ] )
               oGet:SetKeyEvent( aKey[ 1 ], aKey[ 2 ], aKey[ 3 ], aKey[ 4 ], aKey[ 5 ] )
            ENDIF
         NEXT
      ENDIF

   ENDIF

   IF oCol:oEdit != NIL

      oCol:oEdit:oBrw := Self
      oCol:oEdit:oCol := oCol
      oCol:oEdit:nCol := nCell

      oCol:oEdit:bLostFocus := {| nKey | ::EditExit( nCell, nKey, uValue, bValid, .F. ) }

      oCol:oEdit:bKeyDown := {| nKey, nFlags, lExit | iif( lExit != NIL .AND. lExit, ;
         ::EditExit( nCell, nKey, uValue, bValid ), Nil ), HB_SYMBOL_UNUSED( nFlags ) }

      DO CASE
      CASE "TBTNBOX" $ Upper( oCol:oEdit:ClassName() )
         oCol:oEdit:bLostFocus := NIL

      CASE "TGETBOX" $ Upper( oCol:oEdit:ClassName() )
         ix := GetControlIndex ( ::cChildControl, ::cParentWnd )
         _HMG_InteractiveCloseStarted := .T.
         IF ix > 0
            IF oCol:lOnGotFocusSelect
               IF ValType( uValue ) == "C"
                  _HMG_aControlGotFocusProcedure[ ix ] := {|| SendMessage( _HMG_aControlHandles[ ix ], EM_SETSEL, 0, iif( Empty( uValue ), -1, Len( Trim(uValue ) ) ) ) }
               ELSEIF ValType( uValue ) $ "ND"
                  _HMG_aControlGotFocusProcedure[ ix ] := {|| SendMessage( _HMG_aControlHandles[ ix ], EM_SETSEL, 0, -1 ) }
               ENDIF
            ENDIF
            _HMG_aControlLostFocusProcedure[ ix ] := {| nKey | ::EditExit( nCell, nKey, uValue, bValid, .F. ) }
         ENDIF
         IF Empty( ::bLostFocus )
            ::bLostFocus := {|| iif( _HMG_InteractiveCloseStarted, _HMG_InteractiveCloseStarted := .F., ) }
         ENDIF
      ENDCASE

      oCol:oEdit:SetFocus()

      IF nKey != NIL .AND. nKey > 31

         IF ! lCombo .AND. ! lMulti
            ::KeyChar( nKey, nKeyFlags ) // 1.53
         ENDIF

      ENDIF

      IF oCol:oEdit != NIL
         oCol:oEdit:Show()
      ENDIF

      ::SetMsg( oCol:cMsgEdit )

      IF oCol:bEditing != NIL
         Eval( oCol:bEditing, uValue, Self )
      ENDIF

   ENDIF

RETURN NIL

* ============================================================================
* METHOD TSBrowse:EditExit() Version 7.0 Jul/15/2004
* ============================================================================

METHOD EditExit( nCol, nKey, uVar, bValid, lLostFocus ) CLASS TSBrowse

   LOCAL uValue, cType, oCol, lCombo, cMsg, ix, lSpinner

   DEFAULT lLostFocus := .F., ;
      ::lPostEdit := .F., ;
      nCol := ::nCell, ;
      nKey := 0

   oCol := ::aColumns[ nCol ]
   lSpinner := oCol:lSpinner
   lCombo := ValType( oCol:oEdit ) == "O" .AND. "COMBO" $ Upper( oCol:oEdit:ClassName() )

   IF ValType( oCol:oEdit ) == "O"
      DO CASE
      CASE "TGETBOX" $ Upper( oCol:oEdit:ClassName() )
         ix := GetControlIndex ( ::cChildControl, ::cParentWnd )
         nKey := _HMG_aControlMiscData1[ ix ][ 3 ]
         SetFocus( ::hWnd ) // JP 1.59

      CASE "TBTNBOX" $ Upper( oCol:oEdit:ClassName() ) .AND. lSpinner
         IF oCol:oEdit:hWndChild != NIL
            PostMessage( oCol:oEdit:hWndChild, WM_CLOSE )
         ENDIF
         SetFocus( ::hWnd )
      ENDCASE
   ENDIF

   IF nKey == 0
      lLostFocus := .T.
   ENDIF

   IF ! lLostFocus .AND. nKey > 0 .AND. ( nKey != VK_ESCAPE .OR. ::nColSpecHd != 0 ) .AND. ;
         ValType( oCol:oEdit ) == "O"

      ::lPostEdit := .T.
      HideCaret( oCol:oEdit:hWnd )

      IF lCombo

         cType := iif( oCol:cDataType != NIL, oCol:cDataType, ValType( uVar ) )

         IF cType == "L"
            uValue := ( oCol:oEdit:nAt == 1 )
            uVar := iif( ValType( uVar ) == "C", ( AScan( oCol:aItems, uVar ) == 1 ), ;
               iif( ValType( uVar ) == "N", ( uVar == 1 ), uVar ) )
         ELSE
            IF oCol:aData != NIL
               IF ValType( uValue ) == "N" .AND. ValType( uVar ) == "C"
                  uVar := AScan( oCol:aData, uVar )
               ENDIF
               ::lChanged := ( oCol:oEdit:nAt != uVar ) // JP 69
               uValue := oCol:aData[ oCol:oEdit:nAt ]
            ELSE
               IF cType == "N"
                  uValue := oCol:oEdit:nAt
               ELSE
                  uValue := oCol:oEdit:aItems[ oCol:oEdit:nAt ]
                  uVar := SubStr( oCol:oEdit:aItems[ uVar ], 1, Len( uValue ) ) // 1.54
               ENDIF
               ::lChanged := ( uValue != uVar ) // JP 69
            ENDIF
            ::lHasChanged := iif( ::lChanged, .T., ::lHasChanged )
         ENDIF

         ::oWnd:nLastKey := nKey

         IF oCol:bEditEnd != NIL
            Eval( oCol:bEditEnd, uValue, Self, .T. )
         ENDIF

         IF ::nColSpecHd != 0
            ::aColumns[ nCol ]:cSpcHeading := uValue
            oCol:oEdit:End()
            oCol:oEdit := NIL
            ::lEditing := ::lPostEdit := ::lNoPaint := .F.
            ::nColSpecHd := 0
            RETURN NIL
         ENDIF

         IF ValType( oCol:oEdit ) == "O"
            ::lAppendMode := oCol:oEdit:lAppend
            oCol:oEdit:Move( 1500, 0 )
            oCol:oEdit:End()
         ENDIF
         ::PostEdit( uValue, nCol, bValid )
         oCol:oEdit := NIL
         ::oWnd:bValid := ::oGet

         cMsg := iif( ! Empty( oCol:cMsg ), oCol:cMsg, ::cMsg )
         IF ValType( cMsg ) == "B"
            cMsg := Eval( cMsg, Self, ::nCell )
         ENDIF
         ::SetMsg( cMsg )
         RETURN NIL

      ENDIF

      uValue := oCol:oEdit:VarGet()
      IF ::nColSpecHd != 0
         uValue := iif( nKey != VK_ESCAPE, uValue, "" )
         ::aColumns[ nCol ]:cSpcHeading := uValue
         ::lChanged := ValType( uValue ) != ValType( uVar ) .OR. uValue != uVar
         oCol:oEdit:End()
         oCol:oEdit := NIL
         ::lEditing := ::lPostEdit := ::lNoPaint := .F.
         ::nColSpecHd := 0
         ::lHasChgSpec := ::lChanged
         ::AutoSpec( nCol )
         RETURN NIL
      ELSE
         IF oCol:bCustomEdit != NIL
            uValue := Eval( oCol:bCustomEdit, uValue, oCol:oEdit, Self )
         ENDIF

         IF ::lAppendMode .AND. Empty( oCol:oEdit:VarGet() ) .AND. nKey != VK_RETURN
            bValid := {|| .F. }
            IF ::nLenPos > ::nRowCount() // JP 1.50
               ::nLenPos--
            ENDIF
            ::nRowPos := ::nLenPos // JP 1.31
         ENDIF

         IF oCol:bEditEnd != NIL
            Eval( oCol:bEditEnd, uValue, Self, .T. )
         ENDIF

         ::lChanged := ValType( uValue ) != ValType( uVar ) .OR. uValue != uVar
         ::lHasChanged := iif( ::lChanged, .T., ::lHasChanged )
         ::oWnd:nLastKey := nKey

         oCol:oEdit:End()
         ::PostEdit( uValue, nCol, bValid )
         oCol:oEdit := NIL
         ::oWnd:bValid := ::oGet

         cMsg := iif( ! Empty( oCol:cMsg ), oCol:cMsg, ::cMsg )
         IF ValType( cMsg ) == "B"
            cMsg := Eval( cMsg, Self, ::nCell )
         ENDIF
         ::SetMsg( cMsg )
      ENDIF

   ELSE

      IF ::lPostEdit
         RETURN NIL
      ENDIF

      IF oCol:bEditEnd != NIL .AND. ValType( oCol:oEdit ) == "O"
         Eval( oCol:bEditEnd, uValue, Self, .F. )
      ENDIF

      IF ValType( oCol:oEdit ) == "O"
         IF lCombo
            ::lAppendMode := oCol:oEdit:lAppend
         ENDIF
         oCol:oEdit:End()
         oCol:oEdit := NIL
      ENDIF

      ::oWnd:nLastKey := VK_ESCAPE
      ::lChanged := .F.
      ::lEditing := .F.

      IF ::lAppendMode
         IF ::lIsArr .AND. ::nAt > Len( ::aArray ) // JP 74
            ::nAt--
            ::Refresh( .T. )
            ::HiliteCell( ::nCell )
         ENDIF

         ::lAppendMode := .F.
         ::lHitBottom := .F.
         ::lNoPaint := .F.

         IF ::nLen <= ::nRowCount()
            ::Refresh( .T. )
         ELSEIF ! ::lCanAppend
            ::GoBottom()
         ENDIF

      ENDIF
      cMsg := iif( ! Empty( oCol:cMsg ), oCol:cMsg, ::cMsg )
      cMsg := iif( ValType( cMsg ) == "B", Eval( cMsg, Self, ::nCell ), cMsg )
      ::SetMsg( cMsg )

   ENDIF

RETURN NIL

* ============================================================================
* METHOD TSBrowse:AutoSpec() Version 1.83 Adaption HMG  01/01/2010
* ============================================================================

METHOD AutoSpec( nCol )

   LOCAL cExp, uExp, nPos, acSpecHdr := {}
   LOCAL lIsAdo := AND( ::nDataType, DATATYPE_ADO ) != 0
   LOCAL bError := ErrorBlock( {| x | Break( x ) } )

   IF ::lAutoSearch
      IF ::bUserSearch != NIL
         AEval( ::aColumns, {| x | AAdd( acSpecHdr, x:cSpcHeading ) } )
         Eval( ::bUserSearch, nCol, acSpecHdr, Self )
      ELSE
         cExp := BuildAutoSeek( Self )
         IF ! Empty( cExp )
            IF ! lIsAdo
               BEGIN SEQUENCE
                  uExp := &( cExp )
               RECOVER
                  ErrorBlock( bError )
               END SEQUENCE
            ENDIF
            IF ::lIsArr
               IF ( nPos := Eval( uExp, Self ) ) != 0
                  ::nAt := nPos
                  IF ::bChange != NIL
                     Eval( ::bChange, Self, 0 )
                  ENDIF
                  ::Refresh( .T. )
               ELSE
                  Tone( 500, 1 )
               ENDIF
            ENDIF
            IF ::lIsDbf .OR. AND( ::nDataType, DATATYPE_ADO ) != 0
               IF ::lHasChgSpec
                  Eval( ::bGoTop )
               ENDIF
               IF ::ExpLocate( cExp, nCol )
                  ::Refresh( .T. )
               ELSE
                  Tone( 500, 1 )
               ENDIF
            ENDIF
         ENDIF
      ENDIF

   ELSEIF ::lAutoFilter
      IF ::bUserFilter != NIL
         AEval( ::aColumns, {| x | AAdd( acSpecHdr, x:cSpcHeading ) } )
         cExp := Eval( ::bUserFilter, nCol, acSpecHdr, Self )
      ELSE
         cExp := BuildAutoFilter( Self )
      ENDIF
      if ! Empty( cExp )
         IF ! lIsAdo
            BEGIN SEQUENCE
               uExp := &( "{||(" + Trim( cExp ) + ")}" )
            RECOVER
               ErrorBlock( bError )
               RETURN NIL
            END SEQUENCE
         ENDIF
         IF ::lIsDbf
            ::bFilter := uExp
            dbSetFilter( uExp, cExp )
            ( ::cAlias )->( dbGoTop() )
            ::GoTop()
            ::lHitBottom := .F.
            ::nRowPos := ::nAt := ::nLastnAt := 1
            ::lHitTop := .T.
            ::Refresh( .T. )
         ELSE
            IF lIsAdo
               ::nRowPos := RSetFilter( Self, cExp )
               ::GoTop()
               ::ResetVScroll()
               ::Refresh( .T. )
            ENDIF
         ENDIF
      ELSE
         IF ::lIsDbf
            dbClearFilter()
            ::bFilter := NIL
            ::GoTop()
            ::UpStable()
            ::Refresh( .T. )
         ELSE
            IF lIsAdo
               ::nRowPos := RSetFilter( Self, "" )
               ::GoTop()
               ::ResetVScroll()
               ::Refresh( .T. )
            ENDIF
         ENDIF
      ENDIF
   ENDIF
   ::lHasChgSpec := .F.

RETURN NIL

* ============================================================================
* METHOD TSBrowse:Excel2() Version 7.0 Jul/15/2004
* ============================================================================

METHOD Excel2( cFile, lActivate, hProgress, cTitle, lSave, bPrintRow ) CLASS TSBrowse

   LOCAL i, nRow, uData, nAlign, nEvery, nPic, nFont, cWork, aFontTmp, nHandle, nTotal, nSkip, nCol, ;
      hFont := iif( ::hFont != NIL, ::hFont, 0 ), ;
      nLine := 1, ;
      nCount := 0, ;
      aLen := {}, ;
      aPic := {}, cPic, anPic := {}, cType, cc, cPic1, ;
      aFont := {}, ;
      nRecNo := iif( ::lIsDbf, ( ::cAlias )->( RecNo() ), 0 ), ;
      nAt := ::nAt, ;
      nOldRow := ::nLogicPos(), ;
      nOldCol := ::nCell

   LOCAL nCntCols := ::nColCount(), oCol, ;
      lDrawFooters := iif( ::lDrawFooters != NIL, ::lDrawFooters, .F. ), ;
      aFntLine := Array( nCntCols ), ;
      aFntHead := Array( nCntCols ), ;
      aFntFoot := Array( nCntCols ), ;
      hFntTitle

   DEFAULT nInstance := 0

   IF HB_ISARRAY( cTitle ) .AND. Len( cTitle ) > 1
      hFntTitle := cTitle[ 2 ]
      cTitle := cTitle[ 1 ]
   ENDIF

   DEFAULT cFile := "Book1.xls", ;
      lActivate := .T., ;
      cTitle := "", ;
      lSave := .F., ;
      cTitle := "", ;
      hFntTitle := hFont

   CursorWait()

   FOR i := 1 TO nCntCols
      oCol := ::aColumns[ i ]
      aFntLine[ i ] := oCol:hFont
      aFntHead[ i ] := oCol:hFontHead
      aFntFoot[ i ] := oCol:hFontFoot
      IF HB_ISBLOCK( oCol:hFont )
         oCol:hFont := hFont
      ENDIF
      IF HB_ISBLOCK( oCol:hFontHead )
         oCol:hFontHead := hFont
      ENDIF
      IF HB_ISBLOCK( oCol:hFontFoot )
         oCol:hFontFoot := hFont
      ENDIF
   NEXT

   ::lNoPaint := .F.
   nInstance++
   cWork := GetStartupFolder() + "\Book" + LTrim( Str( nInstance ) ) + ".xls"
   ::SetFocus()
   cFile := ::Proper( StrTran( AllTrim( Upper( cFile ) ), ".XLS" ) + ".XLS" )
   cTitle := AllTrim( cTitle )

   FOR nCol := 1 TO Len( ::aColSizes )

      AAdd( aLen, Max( 1, Round( ::aColSizes[ nCol ] / ;
         GetTextWidth( 0, "B", hFont ), 0 ) ) )

      cPic := ''
      cType := iif( Empty( ::aColumns[ nCol ]:cDataType ), ValType( ::bDataEval( ::aColumns[ nCol ] ) ), ::aColumns[ nCol ]:cDataType )
      IF cType == 'N' .AND. ! Empty( ::aColumns[ nCol ]:cPicture )
         FOR i := 1 TO Len( ::aColumns[ nCol ]:cPicture )
            cc := SubStr( ::aColumns[ nCol ]:cPicture, i, 1 )
            IF cc == '9' ;   cc := '0' ;   ENDIF
            IF cc == '.' ;   cc := ',' ;   ENDIF
            IF cc == '@' .OR. cc == 'K' .OR. cc == 'Z' ;   LOOP ;   ENDIF
            if ! Empty( cPic ) ; cPic += cc
            ELSEIF cc # '0' ; cPic += ( '#0' + cc )
            ENDIF
         NEXT
         IF Empty( cPic )
            cPic := '#0'
         ENDIF
         IF "@Z " $ ::aColumns[ nCol ]:cPicture .OR. Len( cPic ) > 3
            IF "," $ cPic
               cPic1 := SubStr( cPic, 2, At( ",", cPic ) - 2 )
               cPic := StrTran( cPic1, '0', '#' ) + SubStr( cPic, At( ",", cPic ) - 1, Len( cPic ) - 2 )
            ELSE
               cPic1 := SubStr( cPic, 2, Len( cPic ) - 2 )
               cPic := StrTran( cPic1, '0', '#' ) + '0'
            ENDIF
         ENDIF
      ENDIF
      nPic := iif( ! Empty( cPic ), AScan( aPic, {| x | x == cPic } ), 0 )
      IF nPic == 0
         AAdd( aPic, cPic ) ;  nPic := Len( aPic )
      ENDIF

      AAdd( anPIC, nPic )

   NEXT

   IF ( nHandle := FCreate( cWork, 0 ) ) < 0
      MsgStop( "Can't create XLS file", cWork )
      RETURN NIL
   ENDIF

   FWrite( nHandle, BiffRec( 9 ) )
   // set CodePage
   FWrite( nHandle, BiffRec( 66, GetACP() ) )
   FWrite( nHandle, BiffRec( 12 ) )
   FWrite( nHandle, BiffRec( 13 ) )

   IF ::hFont != NIL
      AAdd( aFont, GetFontParam( ::hFont ) )
   ELSE
      IF ( hFont := GetFontHandle( ::cFont ) ) != 0
         AAdd( aFont, GetFontParam( hFont ) )
      ENDIF
   ENDIF

   FOR nCol := 1 TO Len( ::aColumns )

      IF ::aColumns[ nCol ]:lBitMap
         LOOP
      ENDIF

      IF Empty( ::aColumns[ nCol ]:hFont ) .AND. Empty( ::aColumns[ nCol ]:hFontHead )
         LOOP
      ENDIF

      hFont := ::aColumns[ nCol ]:hFont

      IF hFont != NIL
         aFontTmp := GetFontParam( hFont )
         IF AScan( aFont, {| e | e[ 1 ] == aFontTmp[ 1 ] .AND. e[ 2 ] == aFontTmp[ 2 ] .AND. ;
               e[ 3 ] == aFontTmp[ 3 ] .AND. e[ 4 ] == aFontTmp[ 4 ] .AND. ;
               e[ 5 ] == aFontTmp[ 5 ] .AND. e[ 6 ] == aFontTmp[ 6 ] } ) == 0

            AAdd( aFont, aFontTmp )
         ENDIF
      ENDIF

      IF hFont != NIL .AND. hFntTitle != hFont
         hFont := hFntTitle
         aFontTmp := GetFontParam( hFont )
         IF AScan( aFont, {| e | e[ 1 ] == aFontTmp[ 1 ] .AND. e[ 2 ] == aFontTmp[ 2 ] .AND. ;
               e[ 3 ] == aFontTmp[ 3 ] .AND. e[ 4 ] == aFontTmp[ 4 ] .AND. ;
               e[ 5 ] == aFontTmp[ 5 ] .AND. e[ 6 ] == aFontTmp[ 6 ] } ) == 0

            AAdd( aFont, aFontTmp )
         ENDIF
      ENDIF

      hFont := ::aColumns[ nCol ]:hFontHead

      IF hFont != NIL
         aFontTmp := GetFontParam( hFont )
         IF AScan( aFont, {| e | e[ 1 ] == aFontTmp[ 1 ] .AND. e[ 2 ] == aFontTmp[ 2 ] .AND. ;
               e[ 3 ] == aFontTmp[ 3 ] .AND. e[ 4 ] == aFontTmp[ 4 ] .AND. ;
               e[ 5 ] == aFontTmp[ 5 ] .AND. e[ 6 ] == aFontTmp[ 6 ] } ) == 0

            AAdd( aFont, aFontTmp )
         ENDIF
      ENDIF

      hFont := ::aColumns[ nCol ]:hFontFoot

      IF hFont != NIL
         aFontTmp := GetFontParam( hFont )
         IF AScan( aFont, {| e | e[ 1 ] == aFontTmp[ 1 ] .AND. e[ 2 ] == aFontTmp[ 2 ] .AND. ;
               e[ 3 ] == aFontTmp[ 3 ] .AND. e[ 4 ] == aFontTmp[ 4 ] .AND. ;
               e[ 5 ] == aFontTmp[ 5 ] .AND. e[ 6 ] == aFontTmp[ 6 ] } ) == 0

            AAdd( aFont, aFontTmp )
         ENDIF
      ENDIF

   NEXT

   IF Len( aFont ) > 4
      ASize( aFont, 4 )
   ENDIF

   IF ! Empty( aFont )
      FOR nCol := 1 TO Len( aFont )
         FWrite( nHandle, BiffRec( 49, aFont[ nCol ] ) )
      NEXT
   ENDIF

   FWrite( nHandle, BiffRec( 31, 1 ) )
   FWrite( nHandle, BiffRec( 30, "General" ) )

   IF ! Empty( aPic )
      AEval( aPic, {| e | FWrite( nHandle, BiffRec( 30, e ) ) } )
   ENDIF

   AEval( aLen, {| e, n | FWrite( nHandle, BiffRec( 36, e, n - 1, n - 1 ) ) } )
   IF hProgress != NIL
      nTotal := ( ::nLen + 1 ) * Len( ::aColumns )
      SetProgressBarRange( hProgress, 1, nTotal )
      SendMessage( hProgress, PBM_SETPOS, 0, 0 )
      nEvery := Max( 1, Int( nTotal * .02 ) ) // refresh hProgress every 2 %
   ENDIF

   IF ::lIsDbf
      ( ::cAlias )->( Eval( ::bGoTop ) )
   ENDIF

   FOR nRow := 1 To ( ::nLen )

      IF nRow == 1

         IF ! Empty( cTitle )
            cTitle := StrTran( cTitle, CRLF, Chr( 10 ) )
            nAlign := iif( Chr( 10 ) $ cTitle, 5, 1 )
            hFont := hFntTitle
            aFontTmp := GetFontParam( hFont )
            nFont := AScan( aFont, {| e | e[ 1 ] == aFontTmp[ 1 ] .AND. e[ 2 ] == aFontTmp[ 2 ] .AND. ;
               e[ 3 ] == aFontTmp[ 3 ] .AND. e[ 4 ] == aFontTmp[ 4 ] .AND. ;
               e[ 5 ] == aFontTmp[ 5 ] .AND. e[ 6 ] == aFontTmp[ 6 ] } )
            // set Row Height
            FWrite( nHandle, BiffRec( 37, ::nHeightCell * iif( ::nHeightCell < 30, 20, 14 ) ) )
            FWrite( nHandle, BiffRec( 4, cTitle, 0, 0,, nAlign,, Max( 0, nFont - 1 ) ) )
            nLine := 3
         ENDIF

         // set Row Height
         FWrite( nHandle, BiffRec( 37, ::nHeightHead * iif( ::nHeightHead < 30, 20, 14 ) ) )

         FOR nCol := 1 TO Len( ::aColumns )

            IF ::aColumns[ nCol ]:lBitMap
               LOOP
            ENDIF

            uData := iif( ValType( ::aColumns[ nCol ]:cHeading ) == "B", ;
               Eval( ::aColumns[ nCol ]:cHeading, nCol, Self ), ;
               ::aColumns[ nCol ]:cHeading )

            IF ValType( uData ) != "C"
               LOOP
            ENDIF

            uData := Trim( StrTran( uData, CRLF, Chr( 10 ) ) )
            nAlign := Min( LoWord( ::aColumns[ nCol ]:nHAlign ), 2 )
            nAlign := iif( Chr( 10 ) $ uData, 4, nAlign )
            hFont := ::aColumns[ nCol ]:hFontHead
            aFontTmp := GetFontParam( hFont )
            nFont := AScan( aFont, {| e | e[ 1 ] == aFontTmp[ 1 ] .AND. e[ 2 ] == aFontTmp[ 2 ] .AND. ;
               e[ 3 ] == aFontTmp[ 3 ] .AND. e[ 4 ] == aFontTmp[ 4 ] .AND. ;
               e[ 5 ] == aFontTmp[ 5 ] .AND. e[ 6 ] == aFontTmp[ 6 ] } )

            FWrite( nHandle, BiffRec( 4, uData, nLine - 1, nCol - 1, .T., nAlign + 1,, ;
               Max( 0, nFont - 1 ) ) )

            IF hProgress != NIL

               IF nCount % nEvery == 0
                  SendMessage( hProgress, PBM_SETPOS, nCount, 0 )
               ENDIF

               nCount++

            ENDIF

         NEXT

         ++nLine

      ENDIF

      IF bPrintRow != NIL .AND. ! Eval( bPrintRow, nRow )
         ::Skip( 1 )
         LOOP
      ENDIF

      IF nRow == 2
         // set Row Height
         FWrite( nHandle, BiffRec( 37, ::nHeightCell * iif( ::nHeightCell < 30, 20, 14 ) ) )
      ENDIF

      FOR nCol := 1 TO Len( ::aColumns )

         IF ::aColumns[ nCol ]:lBitMap
            LOOP
         ENDIF

         uData := ::bDataEval( ::aColumns[ nCol ] )
         nAlign := LoWord( ::aColumns[ nCol ]:nAlign )
         hFont := ::aColumns[ nCol ]:hFont
         aFontTmp := GetFontParam( hFont )
         nFont := AScan( aFont, {| e | e[ 1 ] == aFontTmp[ 1 ] .AND. e[ 2 ] == aFontTmp[ 2 ] .AND. ;
            e[ 3 ] == aFontTmp[ 3 ] .AND. e[ 4 ] == aFontTmp[ 4 ] .AND. ;
            e[ 5 ] == aFontTmp[ 5 ] .AND. e[ 6 ] == aFontTmp[ 6 ] } )

         nPic := iif( ! Empty( ::aColumns[ nCol ]:cPicture ), anPIC[ nCol ], Nil )

         IF ValType( uData ) == "N"
            FWrite( nHandle, BiffRec( 3, uData, nLine - 1, nCol - 1, .T., nAlign + 1, nPic, ;
               Max( 0, nFont - 1 ) ) )
         ELSE
            uData := Trim( StrTran( cValToChar( uData ), CRLF, Chr( 10 ) ) )
            nAlign := iif( Chr( 10 ) $ uData, 4, nAlign )
            FWrite( nHandle, BiffRec( 4, uData, nLine - 1, nCol - 1, .T., nAlign + 1, nPic, ;
               Max( 0, nFont - 1 ) ) )
         ENDIF

         IF hProgress != NIL

            IF nCount % nEvery == 0
               SendMessage( hProgress, PBM_SETPOS, nCount, 0 )
            ENDIF

            nCount++

         ENDIF

      NEXT

      nSkip := ::Skip( 1 )

      ++nLine
      SysRefresh()
      IF nSkip == 0
         EXIT
      ENDIF

   NEXT

   IF ::lFooting .AND. lDrawFooters

      FOR nCol := 1 TO Len( ::aColumns )

         uData := iif( ValType( ::aColumns[ nCol ]:cFooting ) == "B", ;
            Eval( ::aColumns[ nCol ]:cFooting, nCol, Self ), ;
            ::aColumns[ nCol ]:cFooting )

         IF ValType( uData ) != "C"
            uData := " "
         ENDIF

         uData := Trim( StrTran( uData, CRLF, Chr( 10 ) ) )
         nAlign := Min( LoWord( ::aColumns[ nCol ]:nFAlign ), 2 )
         nAlign := iif( Chr( 10 ) $ uData, 4, nAlign )
         hFont := ::aColumns[ nCol ]:hFontFoot
         aFontTmp := GetFontParam( hFont )
         nFont := AScan( aFont, {| e | e[ 1 ] == aFontTmp[ 1 ] .AND. e[ 2 ] == aFontTmp[ 2 ] .AND. ;
            e[ 3 ] == aFontTmp[ 3 ] .AND. e[ 4 ] == aFontTmp[ 4 ] .AND. ;
            e[ 5 ] == aFontTmp[ 5 ] .AND. e[ 6 ] == aFontTmp[ 6 ] } )

         FWrite( nHandle, BiffRec( 4, uData, nLine - 1, nCol - 1, .T., nAlign + 1,, ;
            Max( 0, nFont - 1 ) ) )

         IF hProgress != NIL

            IF nCount % nEvery == 0
               SendMessage( hProgress, PBM_SETPOS, nCount, 0 )
            ENDIF

            nCount++

         ENDIF

      NEXT

      ++nLine

   ENDIF

   FWrite( nHandle, BiffRec( 10 ) )
   FClose( nHandle )

   FOR i := 1 TO nCntCols
      oCol := ::aColumns[ i ]
      oCol:hFont := aFntLine[ i ]
      oCol:hFontHead := aFntHead[ i ]
      oCol:hFontFoot := aFntFoot[ i ]
   NEXT

   IF hProgress != NIL
      SendMessage( hProgress, PBM_SETPOS, nTotal, 0 )
   ENDIF

   IF lSave
      FileRename( Self, cWork, cFile )
   ENDIF

   CursorArrow()

   IF ::lIsDbf
      ( ::cAlias )->( dbGoto( nRecNo ) )
      ::GoPos( nOldRow, nOldCol )
   ENDIF

   ::nAt := nAt

   IF lActivate
      ShellExecute( 0, "Open", iif( lSave, cFile, cWork ),,, 3 )
   ENDIF

   ::Display()

   IF hProgress != NIL
      SendMessage( hProgress, PBM_SETPOS, 0, 0 )
   ENDIF

RETURN NIL

* ============================================================================
* METHOD TSBrowse:ExcelOle() Version 9.0 Nov/30/2009
// Requires TOleAuto class
// Many thanks to Victor Manuel Tomás for the core of this method
* ============================================================================

METHOD ExcelOle( cXlsFile, lActivate, hProgress, cTitle, hFont, lSave, bExtern, aColSel, bPrintRow ) CLASS TSBrowse

   LOCAL oExcel, oBook, oSheet, nRow, nCol, uData, nEvery, oRange, cRange, cCell, cLet, nColHead, ;
      cText, nStart, nTotal, aFont, aRepl, hFntTitle, ;
      nLine := 1, ;
      nCount := 0, ;
      nRecNo := ( ::cAlias )->( RecNo() ), ;
      nAt := ::nAtPos, ;
      aCol := { 26, 52, 78, 104, 130, 156 }, ;
      aLet := { "", "A", "B", "C", "D", "E" }, ;
      nOldRow := ::nLogicPos(), ;
      nOldCol := ::nCell, ;
      lSelector := ::lSelector
   LOCAL nTmp, cTmp

   DEFAULT lActivate := Empty( cXlsFile ), ;
      cTitle := ""

   DEFAULT lSave := ! lActivate .AND. ! Empty( cXlsFile ), ;
      cXlsFile := "", ;
      hFntTitle := hFont

   CursorWait()

   IF lSelector
      ::aClipBoard := { ColClone( ::aColumns[ 1 ], Self ), 1, "" }
      ::DelColumn( 1 )
   ENDIF

   cLet := aLet[ AScan( aCol, {| e | Len( iif( aColSel != NIL, aColSel, ::aColumns ) ) <= e } ) ]

   IF ! Empty( cLet )
      nCol := AScan( aLet, cLet ) - 1
      cLet += HeadXls( Len( iif( aColSel != NIL, aColSel, ::aColumns ) ) - aCol[ Max( 1, nCol ) ] )
   ELSE
      cLet := HeadXls( Len( iif( aColSel != NIL, aColSel, ::aColumns ) ) )
   ENDIF

   aRepl := {}

   ::lNoPaint := .F.

   IF hProgress != NIL
      nTotal := ( ::nLen + 1 ) * Len( ::aColumns ) + 30
      SetProgressBarRange( hProgress, 1, nTotal )
      SendMessage( hProgress, PBM_SETPOS, 0, 0 )
      nEvery := Max( 1, Int( nTotal * .02 ) ) // refresh hProgress every 2 %
   ENDIF

   IF ! Empty( cXlsFile )
      IF At( "\", cXlsFile ) > 0
         cXlsFile := cFilePath( cXlsFile ) + "\" + cFileNoExt( cXlsFile )
      ELSE
         cXlsFile := cFileNoExt( cXlsFile )
      ENDIF
   ENDIF

   IF HB_ISARRAY( cTitle ) .AND. Len( cTitle ) > 1
      hFntTitle := cTitle[ 2 ]
      cTitle := cTitle[ 1 ]
   ENDIF

   cTitle := AllTrim( cTitle )

   TRY
      oExcel := CreateObject( "Excel.Application" )
   CATCH
      MsgStop( "Excel is not available. [" + Ole2TxtError() + "]", "Error" )
      RETURN NIL
   END TRY

   IF hProgress != NIL
      nCount -= 15
      SendMessage( hProgress, PBM_SETPOS, nCount, 0 )
   ENDIF

   oExcel:WorkBooks:Add()
   oBook := oExcel:ActiveWorkBook()
   oSheet := oExcel:ActiveSheet()

   oExcel:Visible := .F. // .T. Show Excel on the screen for debugging
   oExcel:DisplayAlerts := .F. // remove Excel warnings

   IF hProgress != NIL
      nCount -= 15
      SendMessage( hProgress, PBM_SETPOS, nCount, 0 )
   ENDIF

   ( ::cAlias )->( Eval( ::bGoTop ) )

   cText := ""

   FOR nRow := 1 TO ::nLen

      IF nRow == 1

         IF ! Empty( cTitle )
            oSheet:Cells( nLine++, 1 ):Value := AllTrim( cTitle )
            oSheet:Range( "A1:" + cLet + "1" ):HorizontalAlignment := xlHAlignCenterAcrossSelection
            IF hFntTitle != NIL
               aFont := GetFontParam( hFntTitle )
               oRange := oSheet:Range( "A1:" + cLet + "1" )
               oRange:Font:Name := aFont[ 1 ]
               oRange:Font:Size := aFont[ 2 ]
               oRange:Font:Bold := aFont[ 3 ]
            ENDIF
            ++nLine
            nStart := nLine
         ELSE
            nStart := nLine
         ENDIF

         IF ::lDrawSuperHd

            FOR nCol := 1 TO Len( ::aSuperHead )
               uData := iif( ValType( ::aSuperhead[ nCol, 3 ] ) == "B", Eval( ::aSuperhead[ nCol, 3 ] ), ;
                  ::aSuperhead[ nCol, 3 ] )
               oSheet:Cells( nLine, ::aSuperHead[ nCol, 1 ] ):Value := uData
               cRange := HeadXls( ::aSuperHead[ nCol, 1 ] ) + LTrim( Str( nLine ) ) + ":" + ;
                  HeadXls( ::aSuperHead[ nCol, 2 ] ) + LTrim( Str( nLine ) )
               oSheet:Range( cRange ):Borders():LineStyle := xlContinuous
               oSheet:Range( cRange ):HorizontalAlignment := xlHAlignCenterAcrossSelection
               oRange := oSheet:Range( cRange )
               IF hFont != NIL
                  aFont := GetFontParam( hFont )
                  oRange:Font:Name := aFont[ 1 ]
                  oRange:Font:Size := aFont[ 2 ]
                  oRange:Font:Bold := aFont[ 3 ]
               ENDIF
            NEXT

            nStart := nLine++
         ENDIF

         nColHead := 0

         FOR nCol := 1 TO Len( ::aColumns )

            IF aColSel != NIL .AND. AScan( aColSel, nCol ) == 0
               LOOP
            ENDIF

            uData := iif( ValType( ::aColumns[ nCol ]:cHeading ) == "B", Eval( ::aColumns[ nCol ]:cHeading ), ;
               ::aColumns[ nCol ]:cHeading )

            IF ValType( uData ) != "C"
               LOOP
            ENDIF

            uData := StrTran( uData, CRLF, Chr( 10 ) )
            nColHead++
            oSheet:Cells( nLine, nColHead ):Value := uData

            IF hProgress != NIL

               IF nCount % nEvery == 0
                  SendMessage( hProgress, PBM_SETPOS, nCount, 0 )
               ENDIF

               nCount++
            ENDIF
         NEXT

         nStart := nLine + 1

      ENDIF

      IF bPrintRow != NIL .AND. ! Eval( bPrintRow, nRow )
         ::Skip( 1 )
         LOOP
      ENDIF

      FOR nCol := 1 TO Len( ::aColumns )
         IF aColSel != NIL .AND. AScan( aColSel, nCol ) == 0
            LOOP
         ENDIF

         uData := ::bDataEval( ::aColumns[ nCol ] )

         IF ValType( uData ) == "C"
            oSheet:Cells( nLine, nCol ):NumberFormat := "@"

            IF At( CRLF, uData ) > 0
               uData := StrTran( uData, CRLF, "&&" )

               IF AScan( aRepl, nCol ) == 0
                  AAdd( aRepl, nCol )
               ENDIF
            ENDIF

            IF ::aColumns[ nCol ]:cPicture != NIL .AND. uData != NIL
               uData := Transform( uData, ::aColumns[ nCol ]:cPicture )
            ENDIF
         ELSEIF ValType( uData ) == "N"
            IF ::aColumns[ nCol ]:cPicture != NIL
               uData := Transform( uData, ::aColumns[ nCol ]:cPicture )
            ELSE
               uData := cValToChar( uData )
            ENDIF

            cTmp := "#0"
            IF ( nTmp := At( ".", uData ) ) > 0
               cTmp := Replicate( "#", nTmp - 2 ) + '0,' + Replicate( "0", Len( uData ) - nTmp )
            ENDIF

            oSheet:Cells( nLine, nCol ):NumberFormat := cTmp
         ELSE
            IF ::aColumns[ nCol ]:cPicture != NIL .AND. uData != NIL
               uData := Transform( uData, ::aColumns[ nCol ]:cPicture )
            ENDIF

            uData := iif( ValType( uData ) == "D", DToC( uData ), ;
               iif( ValType( uData ) == "L", iif( uData, ".T.", ".F." ), cValToChar( uData ) ) )
         ENDIF

         cText += Trim( uData ) + Chr( 9 )

         IF hProgress != NIL

            IF nCount % nEvery == 0
               SendMessage( hProgress, PBM_SETPOS, nCount, 0 )
            ENDIF

            nCount++
         ENDIF
      NEXT

      ::Skip( 1 )
      cText += Chr( 13 )

      ++nLine

      /*
         Cada 20k volcamos el texto a la hoja de Excel , usando el portapapeles , algo muy rapido y facil ;-)
         Every 20k set text into excel sheet , using Clipboard , very easy and faster.
      */

      IF Len( cText ) > 20000
         CopyToClipboard( cText )
         cCell := "A" + AllTrim( Str( nStart ) )
         oRange := oSheet:Range( cCell )
         oRange:Select()
         oSheet:Paste()
         cText := ""
         nStart := nLine + 1
      ENDIF

   NEXT

   IF AScan( ::aColumns, {| o | o:cFooting != Nil } ) > 0

      FOR nCol := 1 TO Len( ::aColumns )

         IF ( aColSel != NIL .AND. AScan( aColSel, nCol ) == 0 ) .OR. ::aColumns[ nCol ]:cFooting == NIL
            LOOP
         ENDIF

         uData := iif( ValType( ::aColumns[ nCol ]:cFooting ) == "B", ;
            Eval( ::aColumns[ nCol ]:cFooting, nCol, Self ), ::aColumns[ nCol ]:cFooting )
         uData := cValTochar( uData )
         uData := StrTran( uData, CRLF, Chr( 10 ) )
         oSheet:Cells( nLine + 1, nCol ):Value := uData
      NEXT
   ENDIF

   IF Len( cText ) > 0
      CopyToClipboard( cText )
      cCell := "A" + AllTrim( Str( nStart ) )
      oRange := oSheet:Range( cCell )
      oRange:Select()
      oSheet:Paste()
      cText := ""
   ENDIF

   nLine := iif( ! Empty( cTitle ), 3, 1 )
   nLIne += iif( ! Empty( ::aSuperHead ), 1, 0 )
   cRange := "A" + LTrim( Str( nLine ) ) + ":" + cLet + AllTrim( Str( oSheet:UsedRange:Rows:Count() ) )
   oRange := oSheet:Range( cRange )

   IF hFont != NIL // let the programmer to decide the font he wants, otherwise use Excel's default
      aFont := GetFontParam( hFont )
      oRange:Font:Name := aFont[ 1 ]
      oRange:Font:Size := aFont[ 2 ]
      oRange:Font:Bold := aFont[ 3 ]
   ENDIF

   IF ! Empty( aRepl )
      FOR nCol := 1 TO Len( aRepl )
         oSheet:Columns( HeadXls( aRepl[ nCol ] ) ):Replace( "&&", Chr( 10 ) )
      NEXT
   ENDIF

   IF bExtern != NIL
      Eval( bExtern, oSheet, Self )
   ENDIF

   oRange:Borders():LineStyle := xlContinuous
   oRange:Columns:AutoFit()

   IF ! Empty( aRepl )
      FOR nCol := 1 TO Len( aRepl )
         oSheet:Columns( HeadXls( aRepl[ nCol ] ) ):WrapText := .T.
      NEXT
   ENDIF

   IF lSelector
      ::InsColumn( ::aClipBoard[ 2 ], ::aClipBoard[ 1 ] )
      ::lNoPaint := .F.
   ENDIF

   oSheet:Range( "A1" ):Select()

   IF hProgress != NIL
      SendMessage( hProgress, PBM_SETPOS, nTotal, 0 )
   ENDIF

   IF ::lIsDbf
      ( ::cAlias )->( dbGoto( nRecNo ) )
      ::GoPos( nOldRow, nOldCol )
   ENDIF

   ::nAt := nAt

   IF ! Empty( cXlsFile ) .AND. lSave

      oBook:SaveAs( cXlsFile, xlWorkbookNormal )

      IF ! lActivate
         CursorArrow()
         oExcel:Application:Quit()
         ::Reset()
         RETURN NIL
      ENDIF
   ENDIF

   CursorArrow()

   IF lActivate
      oExcel:Visible := .T.
      TRY
         IF Val( oExcel:Version ) < 12 // Excel 2003
            _Minimize( oExcel:hWnd )
            _Maximize( oExcel:hWnd )
         ENDIF
         BringWindowToTop( oExcel:hWnd )
      CATCH
      END TRY
   ELSE
      oExcel:Application:Quit()
   ENDIF

   ::Reset()

   IF hProgress != NIL
      SendMessage( hProgress, PBM_SETPOS, 0, 0 )
   ENDIF

RETURN NIL

* ============================================================================
* METHOD TSBrowse:ExpLocate() Version 9.0 Nov/30/2009
* ============================================================================

METHOD ExpLocate( cExp, nCol ) CLASS TSBrowse

   LOCAL uExp, bExp, ;
      nLines := ::nRowCount(), ;
      nRecNo := ( ::cAlias )->( RecNo() ), ;
      lIsAdo := AND( ::nDataType, DATATYPE_ADO ) != 0, ;
      bError := ErrorBlock( {| x | Break( x ) } )

   ::lValidating := .T.

   IF ! lIsAdo
      BEGIN SEQUENCE
         uExp := &( cExp )
      RECOVER
         ErrorBlock( bError )
         Tone( 500, 1 )
         RETURN .F.
      END SEQUENCE

      ErrorBlock( bError )

      bExp := &( "{||(" + Trim( cExp ) + ")}" )

      IF Eval( bExp )
         ::Skip( 1 )
      ENDIF
   ENDIF

   IF lIsAdo
      RSetLocate( Self, cExp, ::aColumns[ nCol ]:lDescend )
   ELSE
      ( ::cAlias )->( __dbLocate( bExp, cExp,,, .T. ) )

      IF ( ::cAlias )->( Eof() )
         ( ::cAlias )->( dbGoto( nRecNo ) )
         Tone( 500, 1 )
         RETURN .F.
      ENDIF
   ENDIF

   IF nRecNo != ( ::cAlias )->( RecNo() ) .AND. ::nLen > nLines

      nRecNo := ( ::cAlias )->( RecNo() )
      ( ::cAlias )->( dbSkip( nLines - ::nRowPos ) )

      IF ( ::cAlias )->( Eof() )

         Eval( ::bGoBottom )
         ::nRowPos := nLines
         ::nAt := ::nLogicPos()

         WHILE ::nRowPos > 1 .AND. ( ::cAlias )->( RecNo() ) != nRecNo
            ::Skip( -1 )
            ::nRowPos--
         ENDDO
      ELSE
         ( ::cAlias )->( dbGoto( nRecNo ) )
         ::nAt := ::nLogicPos()
      ENDIF

      ::Refresh( .F. )
      ::ResetVScroll()
   ELSEIF nRecNo != ( ::cAlias )->( RecNo() )
      nRecNo := ( ::cAlias )->( RecNo() )
      Eval( ::bGoTop )
      ::nAt := ::nRowPos := 1

      WHILE nRecNo != ( ::cAlias )->( RecNo() )
         ::Skip( 1 )
         ::nRowPos++
      ENDDO
      ::Refresh( .F. )
      ::ResetVScroll()
   ENDIF

   IF ::bChange != NIL
      Eval( ::bChange, Self, 0 )
   ENDIF

   IF ::lIsArr .AND. ::bSetGet != NIL
      IF ValType( Eval( ::bSetGet ) ) == "N"
         Eval( ::bSetGet, ::nAt )
      ELSEIF ::nLen > 0
         Eval( ::bSetGet, ::aArray[ ::nAt, 1 ] )
      ELSE
         Eval( ::bSetGet, "" )
      ENDIF
   ENDIF

   ::lHitTop := ::lHitBottom := .F.

RETURN .T.

* ============================================================================
* METHOD TSBrowse:GotoRec()  by Igor Nazarov
* ============================================================================

METHOD GotoRec( nRec, nRowPos ) CLASS TSBrowse

   LOCAL cAlias
   LOCAL nSkip
   LOCAL n
   LOCAL nRecSave
   LOCAL lRet := .F.
   LOCAL lReCount := .F.

   IF ::lIsDbf

      lRet := .T.
      cAlias := ::cAlias
      ::nLastPos := ( cAlias )->( RecNo() )

      IF HB_ISLOGICAL( nRowPos ) .AND. nRowPos .AND. ::nLen > ::nRowCount()
         nRecSave := ::nLastPos
         ( cAlias )->( dbGoto( nRec ) )
         ::DbSkipper( ::nRowCount() - ::nRowPos )

         IF ( cAlias )->( Eof() )
            Eval( ::bGoBottom )
            ::nRowPos := ::nRowCount()

            DO WHILE ::nRowPos > 1 .AND. ( cAlias )->( RecNo() ) != nRec
               ::DbSkipper( -1 )
               ::nRowPos--
            ENDDO
         ELSE
            ( cAlias )->( dbGoto( nRecSave ) )
         ENDIF
      ENDIF

      hb_default( @nRowPos, ::nRowPos )

      ( cAlias )->( dbGoto( nRec ) )
      nSkip := Abs( ::DbSkipper( -( nRowPos - 1 ) ) )

      nRecSave := ( cAlias )->( RecNo() )
      nRowPos := Min( nSkip + 1, nRowPos )

      ( cAlias )->( dbGoto( nRec ) )
      n := ::DbSkipper( ::nRowCount() - nRowPos )

      IF n < ::nRowCount() - nRowPos
         lReCount := .T.
      ENDIF

      ( cAlias )->( dbGoto( nRecSave ) )
      ::nRowPos := nRowPos
      ::Refresh( lReCount, lReCount )
      ::Skip( nSkip )

      ::ResetVscroll()

      IF ::bChange != NIL
         Eval( ::bChange, Self, 0 )
      ENDIF

      ::lHitTop := ::lHitBottom := .F.
      SysRefresh()

   ELSEIF ::lIsArr

      hb_default( @nRowPos, ::nRowPos )
      IF nRec > ::nLen
         nRec := ::nLen
      ENDIF
      IF nRec == ::nLen
         ::GoBottom()
      ELSEIF nRec == 1
         ::GoTop()
      ELSE
         ::GoPos( nRec )
         ::nAt := nRec
         ::nRowPos := iif( nRowPos > ::nRowCount(), ::nRowCount(), nRowPos )
         ::lHitTop := ::lHitBottom := .F.
      ENDIF
      ::Refresh( ::nRowCount() < ::nLen )
      IF ::bChange != NIL
         Eval( ::bChange, Self, 0 )
      ENDIF
      SysRefresh()

   ENDIF

RETURN lRet

* ============================================================================
* METHOD TSBrowse:SeekRec()  by Igor Nazarov
* ============================================================================

METHOD SeekRec( xVal, lSoftSeek, lFindLast, nRowPos ) CLASS TSBrowse

   LOCAL cAlias, nRecOld, lRet := .F.

   DEFAULT lSoftSeek := .T., lFindLast := .T., nRowPos := ::nRowPos

   cAlias := ::cAlias
   nRecOld := ( cAlias )->( RecNo() )

   IF ( cAlias )->( dbSeek( xVal, lSoftSeek, lFindLast ) )
      ::GoToRec( ( cAlias )->( RecNo() ), nRowPos )
      lRet := .T.
   ELSE
      ( cAlias )->( dbGoto( nRecOld ) )
   ENDIF

RETURN lRet

* ============================================================================
* METHOD TSBrowse:FindRec()  by Igor Nazarov
* ============================================================================

METHOD FindRec( Block, lNext, nRowPos ) CLASS TSBrowse

   LOCAL i
   LOCAL n := 0
   LOCAL cAlias
   LOCAL nRecOld
   LOCAL lArr := HB_ISARRAY( Block )
   LOCAL lRet := .F.

   DEFAULT lNext := .F., nRowPos := ::nRowPos

   cAlias := ::cAlias
   nRecOld := ( cAlias )->( RecNo() )

   IF lNext
      ( cAlias )->( dbSkip( 1 ) )
   ELSE
      ( cAlias )->( dbGoTop() )
   ENDIF

   DO WHILE ( cAlias )->( ! Eof() )
      n++
      IF lArr
         FOR i := 1 TO Len( Block )
            lRet := ! Empty( Eval( Block[ i ], Self, i ) )
            IF lRet
               EXIT
            ENDIF
         NEXT
      ELSE
         lRet := ! Empty( Eval( Block, Self, 0 ) )
      ENDIF
      IF lRet
         EXIT
      ENDIF
      DO EVENTS
      ( cAlias )->( dbSkip( 1 ) )
   ENDDO

   IF lRet
      ::GoToRec( ( cAlias )->( RecNo() ), nRowPos )
   ELSE
      ( cAlias )->( dbGoto( nRecOld ) )
   ENDIF

RETURN lRet

* ============================================================================
* METHOD TSBrowse:ScopeRec()  by Igor Nazarov
* ============================================================================

METHOD ScopeRec( xScopeTop, xScopeBottom, lBottom ) CLASS TSBrowse

   LOCAL cAlias := ::cAlias

   ( cAlias )->( ordScope( 0, xScopeTop ) )
   ( cAlias )->( ordScope( 1, xScopeBottom ) )

   ::Reset( lBottom )

RETURN NIL

* ============================================================================
* METHOD TSBrowse:ExpSeek() Version 9.0 Nov/30/2009
* ============================================================================

METHOD ExpSeek( cExp, lSoft ) CLASS TSBrowse

   LOCAL nQuote, uExp, cType, ;
      nRecNo := ( ::cAlias )->( RecNo() ), ;
      nLines := ::nRowCount(), ;
      bError := ErrorBlock( {| x | Break( x ) } )

   IF !( Alias() == ::cAlias )
      MsgInfo( "TsBrowse ExpSeek " + ::aMsg[ 25 ] + "'" + Alias() + "' != '" + ::cAlias + "'", ::aMsg[ 28 ] )
   ENDIF

   BEGIN SEQUENCE
      cType := ValType( Eval( &( "{||" + ( ::cAlias ) + "->(" + ( ::cAlias )->( IndexKey() ) + ")}" ) ) )
   Recover
      ErrorBlock( bError )
      Tone( 500, 1 )
      RETURN .F.
   END SEQUENCE

   ::lValidating := .T.

   nQuote := At( '"', cExp )
   nQuote := iif( nQuote == 0, At( "'", cExp ), nQuote )

   cExp := iif( cType == "C" .AND. nQuote == 0, '"' + Trim( cExp ) + '"', ;
      iif( cType == "D" .AND. At( "CTOD", Upper( cExp ) ) == 0, 'CtoD( "' + AllTrim( cExp ) + '")', ;
      iif( cType == "N", AllTrim( cExp ), iif( cType == "L", iif( AllTrim( cExp ) == "T", ".T.", ".F." ), ;
      AllTrim( cExp ) ) ) ) )

   BEGIN SEQUENCE
      uExp := &( cExp )
   Recover
      ErrorBlock( bError )
      Tone( 500, 1 )
      RETURN .F.
   END SEQUENCE

   ErrorBlock( bError )

   ( ::cAlias )->( dbSeek( uExp, lSoft ) )

   IF ( ::cAlias )->( Eof() )
      ( ::cAlias )->( dbGoto( nRecNo ) )
      Tone( 500, 1 )
      RETURN .F.
   ENDIF

   IF nRecNo != ( ::cAlias )->( RecNo() ) .AND. ::nLen > nLines

      nRecNo := ( ::cAlias )->( RecNo() )
      ( ::cAlias )->( dbSkip( nLines - ::nRowPos ) )

      IF ( ::cAlias )->( Eof() )

         Eval( ::bGoBottom )
         ::nRowPos := nLines
         ::nAt := ::nLogicPos()

         WHILE ::nRowPos > 1 .AND. ( ::cAlias )->( RecNo() ) != nRecNo
            ::Skip( -1 )
            ::nRowPos--
         ENDDO
      ELSE
         ( ::cAlias )->( dbGoto( nRecNo ) )
         ::nAt := ::nLogicPos()
      ENDIF

      ::Refresh( .F. )
      ::ResetVScroll()

   ELSEIF nRecNo != ( ::cAlias )->( RecNo() )

      nRecNo := ( ::cAlias )->( RecNo() )
      Eval( ::bGoTop )
      ::nAt := ::nRowPos := 1

      WHILE nRecNo != ( ::cAlias )->( RecNo() )
         ::Skip( 1 )
         ::nRowPos++
      ENDDO

      ::Refresh( .F. )
      ::ResetVScroll()

   ENDIF

   IF ::bChange != NIL
      Eval( ::bChange, Self, 0 )
   ENDIF

   IF ::lIsArr .AND. ::bSetGet != NIL
      IF ValType( Eval( ::bSetGet ) ) == "N"
         Eval( ::bSetGet, ::nAt )
      ELSEIF ::nLen > 0
         Eval( ::bSetGet, ::aArray[ ::nAt, 1 ] )
      ELSE
         Eval( ::bSetGet, "" )
      ENDIF
   ENDIF

   ::lHitTop := ::lHitBottom := .F.

RETURN .T.

* ============================================================================
* METHOD TSBrowse:FreezeCol() Version 9.0 Nov/30/2009
* ============================================================================

METHOD FreezeCol( lNext ) CLASS TSBrowse

   LOCAL nFreeze := ::nFreeze

   DEFAULT lNext := .T.

   IF lNext // freeze next available column
      ::nFreeze := Min( nFreeze + 1, Len( ::aColumns ) )
   ELSE // unfreeze previous column
      ::nFreeze := Max( nFreeze - 1, 0 )
   ENDIF

   IF ::nFreeze != nFreeze // only update if necessary
      iif( ! lNext, ::PanHome(), Nil )
      ::HiliteCell( ::nFreeze + 1 )
   ENDIF

RETURN Self

* ============================================================================
* METHOD TSBrowse:GetAllColsWidth() Version 9.0 Nov/30/2009
* ============================================================================

METHOD GetAllColsWidth() CLASS TSBrowse

   LOCAL nWidth := 0, ;
      nPos, ;
      nLen := Len( ::aColumns )

   FOR nPos := 1 TO nLen
      nWidth += ::aColumns[ nPos ]:nWidth
   NEXT

RETURN nWidth

* ============================================================================
* METHOD TSBrowse:GetColumn() Version 9.0 Nov/30/2009
* ============================================================================

METHOD GetColumn( nCol ) CLASS TSBrowse

   DEFAULT nCol := 1

   IF HB_ISSTRING( nCol ) // 14.07.2015
      nCol := ::nColumn( nCol )
   ELSE
      IF nCol < 1
         nCol := 1
      ELSEIF nCol > Len( ::aColumns )
         nCol := Len( ::aColumns )
      ENDIF
   ENDIF

RETURN ::aColumns[ nCol ] // returns a Column object

* ============================================================================
* METHOD TSBrowse:AdjColumns() Version 9.0 Mar/20/2018
* ============================================================================

METHOD AdjColumns( aColumns, nDelta ) CLASS TSBrowse

   LOCAL c, i, k, n, s, w, obr := Self
   LOCAL nVisible := 0, aVisible := {}, aCol := {}

   DEFAULT nDelta := 1

   IF HB_ISLOGICAL( aColumns )
      IF ! aColumns
         ::lAdjColumn := .T.
         RETURN NIL
      ENDIF
      aColumns := NIL
   ENDIF

   IF Empty( aColumns )
      aColumns := Array( ::nColCount() )
      AEval( aColumns, {| xv, nn | xv := nn, aColumns[ nn ] := xv } )
   ENDIF

   IF HB_ISNUMERIC( aColumns )
      AAdd( aCol, aColumns )
   ELSEIF HB_ISCHAR( aColumns )
      AAdd( aCol, ::nColumn( aColumns ) )
   ELSE
      AEval( aColumns, {| xv | AAdd( aCol, iif( HB_ISCHAR( xv ), obr:nColumn( xv ), xv ) ) } )
   ENDIF

   AEval( ::aColumns, {| oc | nVisible += iif( oc:lVisible, oc:nWidth, 0 ) } )
   AEval( aCol, {| nc | iif( obr:aColumns[ nc ]:lVisible, AAdd( aVisible, nc ), Nil ) } )

   w := GetWindowWidth( ::hWnd ) - nVisible - nDelta - iif( ::lNoVScroll, 0, GetVScrollBarWidth() )

   IF w > 0
      k := Len( aVisible )
      n := Int( w / k )
      s := 0

      FOR i := 1 TO k
         c := aVisible[ i ]
         IF i == 1 .AND. ::lSelector
            LOOP
         ELSEIF i == k
            ::aColumns[ c ]:nWidth += ( w - s )
         ELSE
            s += n
            ::aColumns[ c ]:nWidth += n
         ENDIF
      NEXT
   ENDIF

RETURN NIL

* ============================================================================
* METHOD TSBrowse:GetDlgCode() Version 9.0 Nov/30/2009
* ============================================================================

METHOD GetDlgCode( nLastKey ) CLASS TSBrowse

   IF nLastKey == VK_ESCAPE .AND. ::bOnEscape != NIL
      Eval( ::bOnEscape, Self )
   ENDIF

   IF ! ::oWnd:lValidating
      IF nLastKey == VK_UP .OR. nLastKey == VK_DOWN .OR. nLastKey == VK_RETURN .OR. nLastKey == VK_TAB .OR. ;
            nLastKey == VK_ESCAPE
         ::oWnd:nLastKey := nLastKey
      ELSE
         ::oWnd:nLastKey := 0
      ENDIF
   ENDIF

RETURN iif( IsWindowEnabled( ::hWnd ) .AND. nLastKey != VK_ESCAPE, DLGC_WANTALLKEYS, 0 )

* ============================================================================
* METHOD TSBrowse:GetRealPos() Version 9.0 Nov/30/2009
* ============================================================================

METHOD GetRealPos( nRelPos ) CLASS TSBrowse

   LOCAL nLen

   IF ::nLen == 0
      ::nLen := iif( ::lIsDbf, ( ::cAlias )->( Eval( ::bLogicLen ) ), Eval( ::bLogicLen ) )
   ENDIF

   nLen := ::nLen

   nRelPos := iif( nLen > MAX_POS, Int( ( nRelPos / MAX_POS ) * nLen ), nRelPos )

RETURN nRelPos

* ============================================================================
* METHOD TSBrowse:GoBottom() Version 9.0 Nov/30/2009
* ============================================================================

METHOD GoBottom() CLASS TSBrowse

   LOCAL nLines := ::nRowCount()

   IF ::nLen == 0
      ::nLen := iif( ::lIsDbf, ( ::cAlias )->( Eval( ::bLogicLen ) ), Eval( ::bLogicLen ) )
   ENDIF
   IF ::nLenPos == 0
      ::nLenPos := Min( nLines, ::nLen )
   ENDIF
   IF ::nLen < 1
      RETURN Self
   ENDIF

   ::lAppendMode := .F.
   ::ResetSeek()

   IF ! ::lHitBottom

      Eval( ::bGoBottom )

      IF ::bFilter != NIL
         While ! Eval( ::bFilter ) .AND. ! Bof()
            ( ::cAlias )->( dbSkip( -1 ) )
         ENDDO
      ENDIF

      ::lHitBottom := .T.
      ::lHitTop := .F.
      ::nRowPos := Min( nLines, ::nLenPos ) // JP 1.31
      ::nAt := ::nLastnAt := ::nLogicPos()
      ::nLenPos := ::nRowPos

      IF ::lIsDbf
         ::nLastPos := ( ::cAlias )->( RecNo() )
      ENDIF

      IF ::bChange != NIL
         Eval( ::bChange, Self, ::oWnd:nLastKey )
      ENDIF

      IF ::oVScroll != NIL
         ::oVScroll:SetPos( ::oVScroll:nMax )
      ENDIF

      ::Refresh( ::nLen < nLines )

      IF ::lIsArr .AND. ::bSetGet != NIL
         IF ValType( Eval( ::bSetGet ) ) == "N"
            Eval( ::bSetGet, ::nAt )
         ELSEIF ::nLen > 0
            Eval( ::bSetGet, ::aArray[ ::nAt, 1 ] )
         ELSE
            Eval( ::bSetGet, "" )
         ENDIF
      ENDIF

   ENDIF

RETURN Self

* ============================================================================
* METHOD TSBrowse:GoDown() Version 9.0 Nov/30/2009
* ============================================================================

METHOD GoDown() CLASS TSBrowse

   LOCAL nFirst
   LOCAL lRePaint
   LOCAL nLines := ::nRowCount()
   LOCAL lEditable := .F.
   LOCAL lTranspar := ::lTransparent .AND. ( ::hBrush != NIL .OR. ::oWnd:hBrush != Nil )

   IF ::nLen < 1 // for empty dbfs or arrays
      IF ::lCanAppend
         ::nRowPos := 1
      ELSE
         RETURN Self
      ENDIF
   ENDIF

   ::ResetSeek()

   IF ::nLen <= nLines .AND. ::lNoLiteBar
      RETURN Self
   ENDIF

   IF ::lNoLiteBar
      nFirst := nLines - ::nRowPos
      ::Skip( nFirst )
      ::nRowPos := nLines
   ENDIF

   ::nRowPos := Max( 1, ::nRowPos )
   AEval( ::aColumns, {| o | iif( ::lCanAppend .AND. o:lEdit, lEditable := .T., Nil ) } )

   IF ! ::lAppendMode
      ::nPrevRec := ::nAtPos
   ENDIF

   IF ! ::lHitBottom

      IF ! ::lAppendMode .AND. ::nRowPos < nLines .AND. ! ::lIsTxt // 14.07.2015
         ::DrawLine()
      ENDIF

      IF ::Skip( 1 ) == 1 .OR. ::lAppendMode
         ::lHitTop := .F.

         IF ::nRowPos < nLines
            ::nRowPos++
         ELSE
            IF ::lPageMode
               lRePaint := iif( ( ::nLogicPos + nLines - 1 ) > ::nLen, .T., .F. )
               ::nRowPos := 1
               ::Refresh( lRePaint )
            ELSE
               IF lTranspar
                  ::Paint()
               ELSE
                  ::nRowPos := nLines + iif( nLines == 1, 1, 0 )
                  ::TSBrwScroll( 1 )
                  ::Skip( -1 )
                  ::DrawLine( ::nRowPos - 1 ) // added 10.07.2015
                  ::Skip( 1 )
                  IF ::lRowPosAtRec .AND. Len( ::aRowPosAtRec ) > 0
                     hb_ADel( ::aRowPosAtRec, 1, .T. )
                     AAdd( ::aRowPosAtRec, ::nAtPos )
                  ENDIF
               ENDIF
            ENDIF
         ENDIF

#ifdef _TSBFILTER7_
      ELSEIF ::lFilterMode .AND. ::nPrevRec != NIL
         ( ::cAlias )->( dbGoto( ::nPrevRec ) )
#else
      ELSE
         Eval( ::bGoBottom )
         ::lHitBottom := .T.
#endif
      ENDIF

      IF ! ::lAppendMode .AND. ! ::lEditing
         ::DrawSelect()
      ENDIF

      IF ::oVScroll != NIL .AND. ! ::lAppendMode
         ::oVScroll:SetPos( ::RelPos( ::nLogicPos() ) )
      ENDIF

      IF ! ::lHitBottom .AND. ! ::lAppendMode .AND. ::bChange != NIL
         Eval( ::bChange, Self, ::oWnd:nLastKey )
      ENDIF

      ::nAt := ::nLogicPos()

   ELSEIF ::lCanAppend .AND. lEditable .AND. ! ::lAppendMode

      ::lAppendMode := .T.
      nFirst := 0
      AEval( ::aColumns, {| oCol, nCol | nFirst := iif( ::IsEditable( nCol ) .AND. nFirst == 0, nCol, nFirst ), HB_SYMBOL_UNUSED( oCol ) } )
      IF nFirst == 0
         ::lAppendMode := .F.
         ::lHitTop := ::lHitBottom := .F.
         RETURN Self
      ELSEIF ::lSelector .AND. nFirst == 1
         nFirst++
      ENDIF

      IF ::nCell != nFirst .AND. ! ::IsColVisible( nFirst )

         DO WHILE ! ::IsColVisible( nFirst )
            ::nColPos += iif( nFirst < ::nCell, -1, 1 )
         ENDDO
      ENDIF

      ::lHitTop := ::lHitBottom := .F.
      ::nCell := nFirst
      ::nLenPos := ::nRowPos // JP 1.31

      IF ::lIsArr
         ::lAppendMode := .F.
         ::DrawLine()
         ::lAppendMode := .T.
      ELSE
         ::DrawLine()
      ENDIF

      ::GoDown() // recursive call to force entry to itself
      ::nLastKey := ::oWnd:nLastKey := VK_RETURN
      ::DrawLine()

      IF ! ::lAutoEdit
         ::PostMsg( WM_KEYDOWN, VK_RETURN, nMakeLong( 0, 0 ) )
      ENDIF

   ENDIF

RETURN Self

* ============================================================================
* METHOD TSBrowse:GoEnd() Version 9.0 Nov/30/2009
* ============================================================================

METHOD GoEnd() CLASS TSBrowse

   LOCAL nTxtWid
   LOCAL nI
   LOCAL nLastCol
   LOCAL nBegin
   LOCAL nWidth := ::nWidth() - iif( ::oVScroll != NIL, GetSysMetrics( 2 ), 0 )
   LOCAL nWide := 0
   LOCAL nCols := Len( ::aColumns )
   LOCAL nPos := ::nColPos

   IF ::lIsTxt
      nTxtWid := Max( 1, GetTextWidth( 0, "B", iif( ::hFont != NIL, ::hFont, 0 ) ) )

      IF ::nAt < ::oTxtFile:nMaxLineLength - Int( nWidth / nTxtWid )
         ::nAt := ::oTxtFile:nMaxLineLength - Int( nWidth / nTxtWid )
         ::Refresh( .F. )
         IF ::oHScroll != NIL
            ::oHScroll:SetPos( ::nAt )
         ENDIF
      ENDIF

      RETURN Self
   ENDIF

   nLastCol := Len( ::aColumns )
   nBegin := Min( iif( ::nColPos <= ::nFreeze, ( ::nColPos := ::nFreeze + 1, ::nColPos - ::nFreeze ), ;
      ::nColPos - ::nFreeze ), nLastCol )

   nWide := 0

   FOR nI := nBegin TO nPos
      nWide += ::aColSizes[ nI ]
   NEXT

   nBegin := ::nCell

   FOR nI := nPos + 1 TO nCols
      IF nWide + ::aColSizes[ nI ] <= nWidth // only if column if fully visible
         nWide += ::aColSizes[ nI ]
         ::nCell := nI
      ELSE
         EXIT
      ENDIF
   NEXT

   IF nBegin == ::nCell
      RETURN Self
   ENDIF

   IF ::lCellBrw

      IF ::aColumns[ ::nCell ]:bGotFocus != NIL .AND. ::nOldCell != ::nCell
         Eval( ::aColumns[ ::nCell ]:bGotFocus, ::nOldCell, ::nCell, Self )
      ENDIF

      IF ::aColumns[ ::nOldCell ]:bLostFocus != NIL .AND. ::nOldCell != ::nCell
         Eval( ::aColumns[ ::nOldCell ]:bLostFocus, ::nCell, ::nOldCell, Self )
      ENDIF

   ENDIF

   ::Refresh( .F. )

   IF ::oHScroll != NIL
      ::oHScroll:SetPos( ::nCell )
   ENDIF

   ::nOldCell := ::nCell
   ::HiliteCell( ::nCell )

RETURN Self

* ============================================================================
* METHOD TSBrowse:GoHome() Version 9.0 Nov/30/2009
* ============================================================================

METHOD GoHome() CLASS TSBrowse

   ::nOldCell := ::nCell

   IF ::lIsTxt

      IF ::nAt > 1
         ::nAt := 1
         ::Refresh( .F. )
         IF ::oHScroll != NIL
            ::oHScroll:SetPos( ::nAt )
         ENDIF
      ENDIF
      RETURN Self
   ENDIF

   ::nCell := ::nColPos

   IF ::nCell == ::nOldCell
      RETURN Self
   ENDIF

   IF ::lCellBrw

      IF ::aColumns[ ::nCell ]:bGotFocus != NIL .AND. ::nOldCell != ::nCell
         Eval( ::aColumns[ ::nCell ]:bGotFocus, ::nOldCell, ::nCell, Self )
      ENDIF

      IF ::aColumns[ ::nOldCell ]:bLostFocus != NIL .AND. ::nOldCell != ::nCell
         Eval( ::aColumns[ ::nOldCell ]:bLostFocus, ::nCell, ::nOldCell, Self )
      ENDIF

   ENDIF

   ::Refresh( .F. )

   IF ::oHScroll != NIL
      ::oHScroll:SetPos( ::nCell )
   ENDIF

   IF ::aColumns[ ::nCell ]:lVisible == .F.
      ::GoRight()
   ENDIF

   ::nOldCell := ::nCell
   ::HiliteCell( ::nCell )

RETURN Self

* ============================================================================
* METHOD TSBrowse:GoLeft() Version 9.0 Nov/30/2009
* ============================================================================

METHOD GoLeft() CLASS TSBrowse

   LOCAL nCell, nSkip, ;
      lLock := ::nFreeze > 0 .AND. ::lLockFreeze, ;
      lDraw := .F.

   ::nOldCell := ::nCell

   IF ::lIsTxt
      IF ::nOffset > 5
         ::nOffset -= 5
      ELSE
         ::nOffset := 1
      ENDIF
      ::Refresh( .F. )

      IF ::oHScroll != NIL
         ::oHScroll:SetPos( ::nOffset )
      ENDIF
      RETURN Self
   ENDIF

   ::ResetSeek()

   IF ::lCellBrw

      nCell := ::nCell
      nSkip := 0

      WHILE nCell > ( iif( lLock, ::nFreeze + 1, 1 ) )

         nCell--
         nSkip++

         IF ! ::aColumns[ nCell ]:lNoHilite
            EXIT
         ENDIF

      ENDDO

      IF nSkip == 0
         RETURN Self
      ENDIF

      WHILE ::nColPos > ( ::nFreeze + 1 ) .AND. ! ::IsColVisible( nCell )
         lDraw := .T.
         ::nColPos--
      ENDDO

      ::nCell := nCell

      IF lDraw
         ::Refresh( .F.,, .F. )
      ENDIF

      IF ::aColumns[ ::nCell ]:bGotFocus != NIL .AND. ::nOldCell != ::nCell
         Eval( ::aColumns[ ::nCell ]:bGotFocus, ::nOldCell, ::nCell, Self )
      ENDIF

      IF ::aColumns[ ::nOldCell ]:bLostFocus != NIL .AND. ::nOldCell != ::nCell
         Eval( ::aColumns[ ::nOldCell ]:bLostFocus, ::nCell, ::nOldCell, Self )
      ENDIF

      iif( ::oHScroll != NIL, ::oHScroll:SetPos( ::nCell ), Nil )
      ::nOldCell := ::nCell
      ::HiliteCell( ::nCell )
      ::DrawSelect()
      IF ::aColumns[ ::nCell ]:lVisible == .F.
         IF ::nCell == 1
            ::GoRight()
         ELSE
            ::GoLeft()
         ENDIF
      ENDIF

   ELSE

      IF ::nCell > ( ::nFreeze + 1 )

         ::nColPos := ::nCell := ::nFreeze + 1
         ::Refresh( .F.,, .F. )

         IF ::oHScroll != NIL
            ::oHScroll:GoTop()
         ENDIF

      ENDIF

   ENDIF

RETURN Self

* ============================================================================
* METHOD TSBrowse:GoNext() Version 9.0 Nov/30/2009
// Post-edition cursor movement.  Cursor goes to next editable cell, right
// or first-down according to the position of the last edited cell.
// This method is activated when the MOVE clause of ADD COLUMN command is
// set to 5 ( DT_MOVE_NEXT )
* ============================================================================

METHOD GoNext() CLASS TSBrowse

   LOCAL nEle, ;
      nFirst := 0

   ::nOldCell := ::nCell

   FOR nEle := ( ::nCell + 1 ) TO Len( ::aColumns )

      IF ::IsEditable( nEle ) .AND. ! ::aColumns[ nEle ]:lNoHiLite .AND. ::aColumns[ nEle ]:lVisible
         nFirst := nEle
         EXIT
      ENDIF
   NEXT

   IF nFirst > 0
      IF ::IsColVisible( nFirst )

         ::nCell := nFirst
         ::HiLiteCell( ::nCell )

         IF ! ::lAutoEdit
            ::DrawSelect()
         ELSE
            ::DrawLine()
         ENDIF
      ELSE
         While ! ::IsColVisible( nFirst ) .AND. ::nColPos < nFirst
            ::nColPos++
         ENDDO

         ::lNoPaint := .F.
         ::nCell := nFirst
         ::HiliteCell( nFirst )
         ::Refresh( .F. )
      ENDIF

      IF ::oHScroll != NIL
         ::oHScroll:SetPos( ::nCell )
      ENDIF

      IF ::aColumns[ ::nCell ]:bGotFocus != NIL .AND. ::nOldCell != ::nCell
         Eval( ::aColumns[ ::nCell ]:bGotFocus, ::nOldCell, ::nCell, Self )
      ENDIF

      IF ::aColumns[ ::nOldCell ]:bLostFocus != NIL .AND. ::nOldCell != ::nCell
         Eval( ::aColumns[ ::nOldCell ]:bLostFocus, ::nCell, ::nOldCell, Self )
      ENDIF

      ::nOldCell := ::nCell
      RETURN Self
   ENDIF

   AEval( ::aColumns, {| oCol, nCol | nFirst := iif( ::IsEditable( nCol ) .AND. nFirst == 0, ;
      nCol, nFirst ), HB_SYMBOL_UNUSED( oCol ) } )

   IF nFirst == 0
      RETURN Self
   ENDIF

   IF ::IsColVisible( nFirst )
      ::nCell := nFirst
      ::lNoPaint := .F.
   ELSE

      ::nColPos := Min( nFirst, ::nFreeze + 1 )
      ::nCell := nFirst

      WHILE ::nColPos < ::nCell .AND. ! ::IsColVisible( ::nCell )
         ::nColPos++
      ENDDO

      ::lNoPaint := .F.
      ::Refresh( .F. )

   ENDIF

   ::HiliteCell( ::nCell )

   iif( ::oHScroll != NIL, ::oHScroll:SetPos( ::nCell ), Nil )

   IF ::aColumns[ ::nCell ]:bGotFocus != NIL .AND. ::nOldCell != ::nCell
      Eval( ::aColumns[ ::nCell ]:bGotFocus, ::nOldCell, ::nCell, Self )
   ENDIF

   IF ::aColumns[ ::nOldCell ]:bLostFocus != NIL .AND. ::nOldCell != ::nCell
      Eval( ::aColumns[ ::nOldCell ]:bLostFocus, ::nCell, ::nOldCell, Self )
   ENDIF

   IF ::lAutoEdit
      SysRefresh()
   ENDIF

   ::nOldCell := ::nCell
   ::lHitBottom := ( ::nAt == ::nLen )
   IF ::lHitBottom
      SysRefresh()
   ENDIF
   ::GoDown()

RETURN Self

* ============================================================================
* METHOD TSBrowse:GoPos() Version 9.0 Nov/30/2009
* ============================================================================

METHOD GoPos( nNewRow, nNewCol ) CLASS TSBrowse

   LOCAL nSkip, cAlias, nRecNo, ;
      nTotRow := ::nRowCount(), ;
      nOldRow := ::nLogicPos(), ;
      nOldCol := ::nCell

   DEFAULT nNewRow := ::nLogicPos, ;
      nNewCol := ::nCell

   ::lNoPaint := ::lFirstFocus := .F.

   IF ValType( nNewRow ) != "N" .OR. ValType( nNewCol ) != "N" .OR. ;
         nNewCol > Len( ::aColumns ) .OR. nNewRow > ::nLen .OR. ;
         nNewCol <= 0 .OR. nNewRow <= 0

      Tone( 500, 1 )
      RETURN NIL

   ENDIF

   cAlias := ::cAlias

   nSkip := nNewRow - nOldRow

   IF ( ::nRowPos + nSkip ) <= nTotRow .AND. ( ::nRowPos + nSkip ) >= 1

      ::Skip( nSkip )
      ::nRowPos += nSkip

   ELSEIF ! ::lIsDbf
      ::nAt := nNewRow
   ELSEIF Empty( ::nLogicPos() )

      WHILE ::nAt != nNewRow

         IF ::nAt < nNewRow
            ::Skip( 1 )
         ELSE
            ::Skip( -1 )
         ENDIF

      ENDDO

   ELSEIF ! Empty( ::nLogicPos() )

      ( cAlias )->( dbSkip( nSkip ) )
      ::nAt := ::nLogicPos()

   ELSE
      ( cAlias )->( Eval( ::bGoToPos, nNewRow ) )
      ::nAt := ::nLogicPos()
   ENDIF

   IF nNewRow != nOldRow .AND. ::nLen > nTotRow .AND. nNewRow > nTotRow

      IF ::lIsDbf

         nRecNo := ( cAlias )->( RecNo() )

         ( cAlias )->( dbSkip( nTotRow - ::nRowPos ) )

         IF ( cAlias )->( Eof() )

            Eval( ::bGoBottom )
            ::nRowPos := nTotRow

            WHILE ::nRowPos > 1 .AND. ( cAlias )->( RecNo() ) != nRecNo
               ::Skip( -1 )
               ::nRowPos--
            ENDDO

         ELSE
            ( cAlias )->( dbGoto( nRecNo ) )
         ENDIF

      ELSE

         IF ( ::nAt + nTotRow - ::nRowPos ) > ::nLen

            Eval( ::bGoBottom )
            ::nRowPos := nTotRow

            WHILE ::nRowPos > 1 .AND. ::nAt != nNewRow
               ::Skip( -1 )
               ::nRowPos--
            ENDDO

         ENDIF

      ENDIF

   ELSEIF nNewRow != nOldRow .AND. ::nLen > nTotRow

      IF ::lIsDbf

         nRecNo := ( cAlias )->( RecNo() )
         Eval( ::bGoTop )
         ::nRowPos := ::nAt := 1

         DO WHILE ::nRowPos < nTotRow .AND. ( cAlias )->( RecNo() ) != nRecNo
            ::Skip( 1 )
            ::nRowPos++
         ENDDO

      ELSE

         Eval( ::bGoTop )
         ::nRowPos := ::nAt := 1

         DO WHILE ::nRowPos < nTotRow .AND. ::nAt != nNewRow
            ::Skip( 1 )
            ::nRowPos++
         ENDDO

      ENDIF

   ENDIF

   IF nNewCol != nOldCol

      While ! ::IsColVisible( nNewCol ) .AND. ::nColpos >= 1 .AND. ::nColPos < Len( ::aColumns )

         IF nNewCol < ::nCell
            ::nColPos--
         ELSE
            ::nColPos++
         ENDIF

      ENDDO

   ENDIF

   ::nCell := nNewCol
   ::HiliteCell( ::nCell )
   ::Refresh( .F. )

   IF ::bChange != NIL .AND. nNewRow != nOldRow
      Eval( ::bChange, Self, 0 )
   ENDIF

   IF ::oVScroll != NIL .AND. nNewRow != nOldRow
      ::oVScroll:SetPos( ::RelPos( ::nLogicPos() ) )
   ENDIF

   IF ::oHScroll != NIL .AND. nNewCol != nOldCol
      ::oHScroll:SetPos( nNewCol )
   ENDIF

   ::lHitTop := ::nAt == 1

   IF ::lIsArr .AND. ::bSetGet != NIL
      IF ValType( Eval( ::bSetGet ) ) == "N"
         Eval( ::bSetGet, ::nAt )
      ELSEIF ::nLen > 0
         Eval( ::bSetGet, ::aArray[ ::nAt, 1 ] )
      ELSE
         Eval( ::bSetGet, "" )
      ENDIF
   ENDIF

RETURN NIL

* ============================================================================
* METHOD TSBrowse:GoRight() Version 9.0 Nov/30/2009
* ============================================================================

METHOD GoRight() CLASS TSBrowse

   LOCAL nTxtWid, nWidth, nCell, nSkip, lRefresh := .F.

   ::nOldCell := ::nCell
   nWidth := ::nWidth() - iif( ::oVScroll != NIL, GetSysMetrics( 2 ), 0 )

   IF ::lIsTxt
      nTxtWid := Max( 1, GetTextWidth( 0, "B", iif( ::hFont != NIL, ::hFont, 0 ) ) )
      IF ::nOffset < ::oTxtFile:nMaxLineLength - Int( nWidth / nTxtWid )
         ::nOffset += 5
         ::Refresh( .F. )
         IF ::oHScroll != NIL
            ::oHScroll:SetPos( ::nOffset )
         ENDIF
      ENDIF
      RETURN Self
   ENDIF

   ::ResetSeek()

   IF ::lCellBrw

      IF ::nCell == Len( ::aColumns ) .AND. ;  // avoid undesired displacement  //::GetColSizes()
         ::IsColVisible( ::nCell )
         RETURN Self
      ENDIF

      nCell := ::nCell
      nSkip := 0

      WHILE nCell < Len( ::aColumns )
         nCell++
         nSkip++
         IF nCell <= Len( ::aColumns ) .AND. ! ::aColumns[ nCell ]:lNoHilite
            EXIT
         ENDIF
      ENDDO

      IF nCell > Len( ::aColumns )
         RETURN Self
      ENDIF

      WHILE nSkip > 0
         ::nCell++
         nSkip--
      ENDDO

      IF ! ::lFastDrawCell
         lRefresh := ( ::lCanAppend .AND. ::lIsArr )
      ENDIF

      While ! ::IsColVisible( ::nCell ) .AND. ::nColPos < ::nCell
         ::nColPos++
         lRefresh := .T.
      ENDDO

      ::HiliteCell( ::nCell )

      IF ::aColumns[ ::nCell ]:bGotFocus != NIL .AND. ::nOldCell != ::nCell
         Eval( ::aColumns[ ::nCell ]:bGotFocus, ::nOldCell, ::nCell, Self )
      ENDIF

      IF ::aColumns[ ::nOldCell ]:bLostFocus != NIL .AND. ::nOldCell != ::nCell
         Eval( ::aColumns[ ::nOldCell ]:bLostFocus, ::nCell, ::nOldCell, Self )
      ENDIF

      IF lRefresh
         ::lNoPaint := .F.
         ::Refresh( .F.,, .F. )
      ELSEIF ! ::lEditing
         ::DrawSelect()
      ENDIF

      iif( ::oHScroll != NIL, ::oHScroll:SetPos( ::nCell ), Nil )

      ::nOldCell := ::nCell
      IF ::aColumns[ ::nCell ]:lVisible == .F.
         IF ::nCell == Len( ::aColumns )
            ::GoLeft()
         ELSE
            ::GoRight()
         ENDIF
      ENDIF

   ELSE

      IF ::nCell == Len( ::aColumns ) .AND. ;  // avoid undesired displacement  //::GetColSizes()
         ::IsColVisible( ::nCell )
         RETURN Self
      ENDIF

      IF ::oHScroll != NIL
         ::HScroll( SB_PAGEDOWN )
      ENDIF

   ENDIF

RETURN Self

* ============================================================================
* METHOD TSBrowse:GotFocus() Version 9.0 Nov/30/2009
* ============================================================================

METHOD GotFocus( hCtlLost ) CLASS TSBrowse

   LOCAL cMsg

   DEFAULT ::lPostEdit := .F., ;
      ::lFirstFocus := .T., ;
      ::lNoPaint := .F., ;
      ::lInitGoTop := .T.

   IF ::lEditing .OR. ::lPostEdit
      RETURN 0
   ENDIF

   ::lFocused := .T.
   ::oWnd:hCtlFocus := ::hWnd

   IF ::bGotFocus != NIL
      Eval( ::bGotFocus, Self, hCtlLost )
   ENDIF

   IF ::lIsDbf .AND. ::lPainted .AND. ! ::lFirstFocus .AND. ! ::lNoResetPos .AND. ! ::lValidating .AND. ! ::lNoPaint .AND. ! ::lCanAppend

      IF ::uLastTag != NIL
         ( ::cAlias )->( Eval( ::bTagOrder, ::uLastTag ) )
      ENDIF

      ( ::cAlias )->( dbGoto( ::nLastPos ) )
      ::nAt := ::nLastnAt

   ELSEIF ::lIsDbf .AND. ::lFirstFocus .AND. ! ::lNoResetPos .AND. ! ::lValidating

      IF ::lPainted
         ::GoTop()
      ELSE
         ( ::cAlias )->( Eval( ::bGoTop ) )
      ENDIF

      ::nLastPos := ( ::cAlias )->( RecNo() )
      ::nAt := ::nLastnAt := ::nLogicPos()

   ELSEIF ::lFirstFocus .AND. ! ::lValidating .AND. ! ::lNoResetPos // JP 1.70

      IF ::lPainted
         ::GoTop()
      ELSE
         IF ::lIsDbf
            ( ::cAlias )->( Eval( ::bGoTop ) )
         ELSE
            Eval( ::bGoTop )
         ENDIF
      ENDIF

   ENDIF

   ::lFirstFocus := .F.

   IF ::nLen > 0 .AND. ! EmptyAlias( ::cAlias ) .AND. ! ::lIconView .AND. ::lPainted
      ::DrawSelect()
   ENDIF

   ::lHasFocus := .T.
   ::lValidating := .F.

   IF ::lCellBrw .AND. ::lPainted
      cMsg := iif( ! Empty( ::AColumns[ ::nCell ]:cMsg ), ::AColumns[ ::nCell ]:cMsg, ::cMsg )
      cMsg := iif( ValType( cMsg ) == "B", Eval( cMsg, Self, ::nCell ), cMsg )
      ::SetMsg( cMsg )
   ENDIF

RETURN 0

* ============================================================================
* METHOD TSBrowse:GoTop() Version 9.0 Nov/30/2009
* ============================================================================

METHOD GoTop() CLASS TSBrowse

   LOCAL nAt := ::nAt, ;
      nLines := ::nRowCount()

   IF ::nLen == 0
      ::nLen := iif( ::lIsDbf, ( ::cAlias )->( Eval( ::bLogicLen ) ), Eval( ::bLogicLen ) )
   ENDIF

   IF ::nLen < 1
      RETURN Self
   ENDIF

   ::lAppendMode := .F.
   ::ResetSeek()

   IF ! ::lHitTop

      IF ::lPainted .AND. nAt < nLines
         ::DrawLine()
      ENDIF

      Eval( ::bGoTop )

      IF ::bFilter != NIL
         While ! Eval( ::bFilter ) .AND. ! Eof()
            ( ::cAlias )->( dbSkip( 1 ) )
         ENDDO
      ENDIF

      ::lHitBottom := .F.
      ::nRowPos := ::nAt := ::nLastnAt := 1
      ::lHitTop := .T.

      IF ::lIsDbf
         ::nLastPos := ( ::cAlias )->( RecNo() )
      ENDIF

      IF ::lPainted
         IF nAt < nLines
            ::DrawSelect()
            ::Refresh( .F. )
         ELSE
            ::Refresh( ::nLen < nLines )
         ENDIF
      ENDIF

      IF ::oVScroll != NIL
         ::oVScroll:GoTop()
      ENDIF

      IF ::lPainted .AND. ::bChange != NIL
         Eval( ::bChange, Self, VK_UP )
      ENDIF

      IF ::lIsArr .AND. ::bSetGet != NIL
         IF ValType( Eval( ::bSetGet ) ) == "N"
            Eval( ::bSetGet, ::nAt )
         ELSEIF ::nLen > 0
            Eval( ::bSetGet, ::aArray[ ::nAt, 1 ] )
         ELSE
            Eval( ::bSetGet, "" )
         ENDIF
      ENDIF

      ::HiliteCell( ::nCell )

   ENDIF

RETURN Self

* ============================================================================
* METHOD TSBrowse:GoUp() Version 9.0 Nov/30/2009
* ============================================================================

METHOD GoUp() CLASS TSBrowse

   LOCAL nSkipped, ;
      nLines := ::nRowCount(), ;
      lTranspar := ::lTransparent .AND. ( ::hBrush != NIL .OR. ::oWnd:hBrush != Nil )

   IF ::nLen < 1

      IF ::lCanAppend .AND. ::lAppendMode // append mode being canceled
         ::lAppendMode := ::lHitBottom := .F.
         ::nRowPos-- // for empty dbfs
      ELSE
         RETURN Self
      ENDIF
   ENDIF

   ::ResetSeek()

   IF ::nLen <= nLines .AND. ::lNoLiteBar
      RETURN Self
   ENDIF

   IF ::lNoLiteBar
      nSkipped := 1 - ::nRowPos
      ::Skip( nSkipped )
      ::nRowPos := 1
   ENDIF

   IF ::lAppendMode .AND. ::lFilterMode .AND. ::nPrevRec != NIL
      ( ::cAlias )->( dbGoto( ::nPrevRec ) )
   ENDIF

   IF ! ::lHitTop

      IF ! ::lAppendMode .AND. ::nRowPos > 1 // 14.07.2015
         ::DrawLine()
      ENDIF

      IF ::Skip( -1 ) == -1

         ::lHitBottom := .F.

         IF ::nRowPos > 1

            IF ! ::lAppendMode .OR. ( ::lAppendMode .AND. ::nLen < nLines )
               ::nRowPos--
            ENDIF

            IF ::lAppendMode

               IF ::lFilterMode
                  ::Skip( 1 )
               ENDIF

               ::Refresh( iif( ::nLen < nLines, .T., .F. ) )
               ::HiliteCell( ::nCell )
            ENDIF

         ELSE

            IF ::lPageMode
               ::nRowPos := nLines
               ::Refresh( .F. )
            ELSE
               IF ! lTranspar
                  ::lRePaint := .F.
                  ::TSBrwScroll( -1 )
                  ::Skip( 1 )
                  ::DrawLine( 2 )
                  ::Skip( -1 )
                  IF ::lRowPosAtRec .AND. Len( ::aRowPosAtRec ) > 0
                     ASize( ::aRowPosAtRec, Len( ::aRowPosAtRec ) - 1 )
                     hb_AIns( ::aRowPosAtRec, 1, ::nAtPos, .T. )
                  ENDIF
               ELSE
                  ::Paint()
               ENDIF
            ENDIF

         ENDIF

      ELSEIF ! ::lAppendMode
         ::lHitTop := .T.
      ENDIF

      ::DrawSelect()

      IF ::oVScroll != NIL .AND. ! ::lAppendMode
         ::oVScroll:SetPos( ::RelPos( ::nLogicPos() ) )
      ENDIF

      IF ::bChange != NIL
         Eval( ::bChange, Self, VK_UP )
      ENDIF

   ENDIF

   ::lAppendMode := .F.
   ::nPrevRec := NIL

RETURN Self

* ==============================================================================
* METHOD TSBrowse:KeyChar()  Version 9.0 Nov/30/2009
* ==============================================================================

METHOD KeyChar( nKey, nFlags ) CLASS TSBrowse

   LOCAL cComp, lProcess, cTypeCol
   LOCAL ix, lNoKeyChar := ::lNoKeyChar

   DEFAULT ::nUserKey := nKey

   cTypeCol := iif( ::nLen == 0, "U", ValType( ::bDataEval( ::aColumns[ ::nCell ] ) ) ) // Modificado por Carlos

   IF cTypeCol == "L" .AND. ::aColumns[ ::nCell ]:lCheckBox .AND. nKey == VK_SPACE
      lNoKeyChar := .F.
   ENDIF

   IF ::nUserKey == 255 .OR. ! ::lEnabled .OR. lNoKeyChar // from KeyDown() method
      RETURN 0
   ENDIF

   IF ::lAppendMode
      RETURN 0
   ENDIF

   ::lNoPaint := .F.

   IF Upper( ::aMsg[ 1 ] ) == "YES"
      cComp := "TFYN10"
   ELSE
      cComp := "TF10" + SubStr( ::aMsg[ 1 ], 1, 1 ) + SubStr( ::aMsg[ 2 ], 1, 1 )
   ENDIF

   /* Next lines were added to filter keys according with
      the data type of columns to start editing cells. Also were defined
      two static functions _IsChar() and _IsNumeric() to do the job */

   lProcess := iif( ( cTypeCol == "C" .OR. cTypeCol == "M" ) .AND. _IsChar( nKey ), .T., ;
      iif( ( cTypeCol == "N" .OR. cTypeCol == "D" ) .AND. _IsNumeric( nKey ), .T., ;
      iif( ( cTypeCol == "L" .AND. Upper( Chr( nKey ) ) $ cComp ) .OR. ;
      ( cTypeCol == "L" .AND. ::aColumns[ ::nCell ]:lCheckBox .AND. nKey == VK_SPACE ), .T., .F. ) ) )

   // here we process direct cell editing with keyboard, not just the Enter key

   IF lProcess .AND. ::IsEditable( ::nCell ) .AND. ! ::aColumns[ ::nCell ]:lSeek

      IF ::aColumns[ ::nCell ]:oEdit == NIL
         ::Edit( , ::nCell, nKey, nFlags )
      ELSE
         ix := ::aColumns[ ::nCell ]:oEdit:Atx
         IF ix > 0
            PostMessage( _HMG_aControlHandles[ ix ], WM_CHAR, nKey, nFlags )
         ENDIF
      ENDIF

   ELSEIF ::nColCount() > 0 .AND. ::aColumns[ ::nCell ]:lSeek .AND. ( nKey >= 32 .OR. nKey == VK_BACK )
      ::Seek( nKey )
   ELSEIF lProcess .AND. ::lEditableHd .AND. nKey >= 32
      IF ::aColumns[ ::nCell ]:oEditSpec == NIL
         IF ::IsEditable( ::nCell )
            ::Edit( , ::nCell, nKey, nFlags )
         ENDIF
      ELSE
         ix := ::aColumns[ ::nCell ]:oEditSpec:Atx
         IF ix > 0
            PostMessage( _HMG_aControlHandles[ ix ], WM_CHAR, nKey, nFlags )
         ENDIF
      ENDIF
   ELSE
      ::Super:KeyChar( nKey, nFlags )
   ENDIF

RETURN 0

* ============================================================================
* METHOD TSBrowse:KeyDown() Version 7.0 Jul/15/2004
* ============================================================================

METHOD KeyDown( nKey, nFlags ) CLASS TSBrowse

   LOCAL uTemp, uVal, uValue, uReturn, cType, ;
      lEditable := .F., ;
      nFireKey := ::nFireKey, ;
      nCol := ::nCell

   IF ! ::lEnabled
      RETURN 0
   ENDIF

   DEFAULT nFireKey := VK_F2

   ::lNoPaint := .F.
   ::oWnd:nLastKey := ::nLastKey := ::nUserKey := nKey

#ifdef __EXT_USERKEYS__
   IF ::lUserKeys
      uTemp := hb_ntos( nKey )
      uTemp += iif( _GetKeyState( VK_CONTROL ), "#", "" )
      uTemp += iif( _GetKeyState( VK_SHIFT ), "^", "" )
      uVal := hb_HGetDef( ::aUserKeys, uTemp, NIL )
      IF ! HB_ISBLOCK( uVal )
         uTemp := 'other'
         uVal := hb_HGetDef( ::aUserKeys, uTemp, NIL )
      ENDIF
      IF HB_ISBLOCK( uVal )
         uReturn := Eval( uVal, Self, nKey, uTemp )
         IF uTemp == 'other' .AND. ! HB_ISLOGICAL( uReturn )
            uReturn := .T.
         ENDIF
         IF uReturn == NIL .OR. HB_ISLOGICAL( uReturn ) .AND. ! uReturn
            ::nLastKey := 255
            RETURN 0
         ENDIF
         uReturn := NIL
      ENDIF
      uTemp := NIL
   ENDIF
#endif

   IF ::bUserKeys != NIL

      uReturn := Eval( ::bUserKeys, nKey, nFlags, Self )

      IF uReturn != NIL .AND. ValType( uReturn ) == "N" .AND. uReturn < 200 // interpreted as a virtual key code to
         nKey := uReturn // change the original key pressed
      ELSEIF uReturn != NIL .AND. ValType( uReturn ) == "L" .AND. ! uReturn
         ::nUserKey := 255 // want to inhibit the KeyDown and KeyChar Methods for key pressed
         RETURN 0
      ENDIF
   ENDIF

   lEditable := ( ::nColCount() > 0 .AND. ::IsEditable( nCol ) )

   IF ! lEditable .AND. ::lCanAppend
      AEval( ::aColumns, {| o, n | iif( ::IsEditable( n ), lEditable := .T., Nil ), HB_SYMBOL_UNUSED( o ) } )
   ENDIF

   DO CASE // Maintain Clipper behavior for key navigation
   CASE ::lIgnoreKey( nKey, nFlags ) // has to go before any other case statement
      ::SuperKeyDown( nKey, nFlags, Self )

   CASE _GetKeyState( VK_CONTROL ) .AND. _GetKeyState( VK_SHIFT ) // Ctrl+Shift+key
      IF nKey == VK_RIGHT // Ctrl+Shift+Right Arrow
         ::aColumns[ nCol ]:nEditMove := DT_MOVE_RIGHT
      ELSEIF nKey == VK_DOWN // Ctrl+Shift+Down Arrow
         ::aColumns[ nCol ]:nEditMove := DT_MOVE_DOWN
      ELSEIF nKey == VK_UP // Ctrl+Shift+Up Arrow
         ::aColumns[ nCol ]:nEditMove := DT_MOVE_UP
      ELSEIF nKey == VK_LEFT // Ctrl+Shift+Left Arrow
         ::aColumns[ nCol ]:nEditMove := DT_MOVE_LEFT
      ENDIF

   CASE _GetKeyState( VK_CONTROL )
      IF nKey == VK_HOME // Ctrl+Home
         ::GoTop()
         ::PanHome()
      ELSEIF nKey == VK_END // Ctrl+End
         IF ::lHitBottom
            ::PanEnd()
         ELSE
            ::GoBottom()
         ENDIF
      ELSEIF nKey == VK_PRIOR // Ctrl+PgUp
         ::GoTop()
      ELSEIF nKey == VK_NEXT // Ctrl+PgDn
         ::GoBottom()
      ELSEIF nKey == VK_LEFT // Ctrl+Left
         ::PanLeft()
      ELSEIF nKey == VK_RIGHT // Ctrl+Right
         ::PanRight()
      ELSEIF lEditable .AND. nKey == VK_PASTE
         ::Edit( uTemp, nCol, VK_PASTE, nFlags )
      ELSEIF ::lCellBrw .AND. ( nKey == VK_COPY .OR. nKey == VK_INSERT )
         uTemp := cValToChar( ::bDataEval( ::aColumns[ nCol ] ) )
         CopyToClipboard( uTemp )
         SysRefresh()
      ELSE
         ::SuperKeyDown( nKey, nFlags, Self )
      ENDIF

   CASE _GetKeyState( VK_SHIFT ) .AND. nKey < 48 .AND. nKey != VK_SPACE

      IF nKey == VK_HOME // Shift+Home
         ::PanHome()
      ELSEIF nKey == VK_END // Shift+End
         ::PanEnd()
      ELSEIF lEditable .AND. nKey == VK_INSERT
         ::Edit( uTemp, nCol, VK_PASTE, nFlags )
      ELSEIF ( nKey == VK_DOWN .OR. nKey == VK_UP ) .AND. ::lCanSelect
         ::Selection()
         IF nKey == VK_UP
            ::GoUp()
         ELSEIF nKey == VK_DOWN
            ::GoDown()
         ENDIF
      ELSE
         ::SuperKeyDown( nKey, nFlags, Self )
      ENDIF

   CASE nKey == VK_HOME
      ::GoTop()

   CASE nKey == VK_END
      ::GoBottom()

   CASE ::lEditableHd .AND. ( nKey == VK_RETURN .OR. nKey == nFireKey ) .AND. ::nColSpecHd != 0

      uTemp := ::aColumns[ ::nColSpecHd ]:cSpcHeading

      nCol := ::nColSpecHd
      IF Empty( uTemp )
         cType := ValType( ::bDataEval( ::aColumns[ nCol ] ) )
         IF cType $ "CM"
            uTemp := Space( Len( ::bDataEval( ::aColumns[ nCol ] ) ) )
         ELSEIF cType == "N"
            uTemp := 0
         ELSEIF cType == "D"
            uTemp := CToD( "" )
         ELSEIF cType == "L"
            uTemp := .F.
         ENDIF
      ENDIF

      IF ::nColSpecHd != 0
         ::Edit( uTemp, nCol, nKey, nFlags )
      ENDIF

   CASE lEditable .AND. ( nKey == VK_RETURN .OR. nKey == nFireKey )

      IF nKey == nFireKey
         nKey := VK_RETURN
      ENDIF

      IF ::nColSpecHd != 0
         RETURN 0
      ENDIF

      IF ::nRowPos == 0
         IF ::nLen == 0 .AND. ! ::lCanAppend
            RETURN 0
         ENDIF
      ENDIF

      ::oWnd:nLastKey := nKey

      IF ::aColumns[ nCol ]:bPrevEdit != NIL

         IF ::lIsArr .AND. ( ::lAppendMode .OR. ::nAt > Len( ::aArray ) ) // append mode for arrays
         ELSE // GF 16-05-2008
            uVal := ::bDataEval( ::aColumns[ nCol ] )
            uVal := Eval( ::aColumns[ nCol ]:bPrevEdit, uVal, Self, nCol, ::aColumns[ nCol ] )
            IF ValType( uVal ) == "A"
               uValue := uVal[ 2 ]
               uVal := uVal[ 1 ]
            ENDIF
            IF ValType( uVal ) == "L" .AND. ! uVal
               RETURN 0
            ENDIF
         ENDIF

      ENDIF

      IF ::lAppendMode .AND. ::lIsArr

         IF ! Empty( ::aDefault )

            IF Len( ::aDefault ) < Len( ::aColumns )
               ASize( ::aDefault, Len( ::aColumns ) )
            ENDIF

            uTemp := iif( ::aDefault[ nCol ] == NIL, ;
               iif( ::aDefValue[ 1 ] == NIL, ;
               ::aDefValue[ nCol + 1 ], ;
               ::aDefValue[ nCol ] ), ;
               iif( ValType( ::aDefault[ nCol ] ) == "B", ;
               Eval( ::aDefault[ nCol ], Self ), ::aDefault[ nCol ] ) )
         ELSE

            uTemp := iif( nCol <= Len( ::aDefValue ), ;
               ::aDefValue[ nCol ], Space( 10 ) )

         ENDIF

      ELSEIF ::lAppendMode .AND. ::aDefault != NIL

         IF Len( ::aDefault ) < Len( ::aColumns )
            ASize( ::aDefault, Len( ::aColumns ) )
         ENDIF

         uTemp := iif( ::aDefault[ nCol ] != NIL, iif( ValType( ::aDefault[ nCol ] ) == "B", ;
            Eval( ::aDefault[ nCol ], Self ), ::aDefault[ nCol ] ), ::bDataEval( ::aColumns[ nCol ] ) )
      ELSE

         uTemp := iif( uValue == NIL, ::bDataEval( ::aColumns[ nCol ] ), uValue )

      ENDIF

      IF ::lCellBrw .AND. ::aColumns[ nCol ]:lEdit // JP v.1.1
         ::Edit( uTemp, nCol, nKey, nFlags )
      ENDIF

#ifndef __EXT_SELECTION__
   CASE ::lCanSelect .AND. ! lEditable .AND. nKey == VK_SPACE // Added 27.09.2012
      ::Selection()
      ::GoDown()
#endif
   CASE nKey == VK_UP
      ::GoUp()
   CASE nKey == VK_DOWN
      ::GoDown()
   CASE nKey == VK_LEFT
      ::GoLeft()
   CASE nKey == VK_RIGHT
      ::GoRight()
   CASE nKey == VK_PRIOR
      nKeyPressed := .T.
      ::PageUp()
   CASE nKey == VK_NEXT
      nKeyPressed := .T.
      ::PageDown()
   CASE nKey == VK_DELETE .AND. ::lCanDelete
      ::DeleteRow()
   CASE nKey == VK_CONTEXT .AND. nFlags == 22872065
      IF ::bContext != NIL
         Eval( ::bContext, ::nRowPos, ::nColPos, Self )
      ENDIF

#ifndef __EXT_SELECTION__
   CASE ! ::lCellbrw .AND. ( nKey == VK_RETURN .OR. nKey == VK_SPACE ) .AND. ::bLDblClick != NIL // 14.07.2015
      Eval( ::bLDblClick, NIL, nKey, nFlags, Self )
#endif
   OTHERWISE
      ::SuperKeyDown( nKey, nFlags, Self )
   ENDCASE

RETURN 0

#ifdef __EXT_USERKEYS__
* ============================================================================
* METHOD TSBrowse:UserKeys()  by SergKis
* ============================================================================

METHOD UserKeys( nKey, bKey, lCtrl, lShift ) CLASS TSBrowse

   LOCAL cKey := 'other', uVal

   IF HB_ISBLOCK( bKey ) // set a codeblock on key in a hash
      IF ! Empty( nKey )
         IF HB_ISNUMERIC( nKey )
            cKey := hb_ntos( nKey )
            cKey += iif( Empty( lCtrl ), '', '#' )
            cKey += iif( Empty( lShift ), '', '^' )
         ELSEIF HB_ISCHAR( nKey )
            cKey := nKey
         ENDIF
      ENDIF
      hb_HSet( ::aUserKeys, cKey, bKey )
      ::lUserKeys := ( Len( ::aUserKeys ) > 0 )
   ELSE // execute a codeblock by key from a hash
      IF HB_ISNUMERIC( nKey )
         cKey := hb_ntos( nKey )
      ELSEIF HB_ISCHAR( nKey )
         cKey := nKey
      ENDIF
      IF ::lUserKeys // allowed of setting of the codeblocks
         uVal := hb_HGetDef( ::aUserKeys, cKey, NIL )
         IF HB_ISBLOCK( uVal )
            cKey := Eval( uVal, Self, nKey, cKey, bKey, lCtrl, lShift )
         ENDIF
      ENDIF
   ENDIF

RETURN cKey

#endif

* ============================================================================
* METHOD TSBrowse:Selection()  Version 9.0 Nov/30/2009
* ============================================================================

METHOD Selection() CLASS TSBrowse

   LOCAL uTemp, uVal

#ifdef __EXT_SELECTION__
   LOCAL lCan := .T.

   IF HB_ISBLOCK( ::bPreSelect )
      lCan := Eval( ::bPreSelect, ::nAt )
      IF ! HB_ISLOGICAL( lCan )
         lCan := .T.
      ENDIF
   ENDIF

   IF lCan
#endif
      uVal := ::nAtPos

      IF ( uTemp := AScan( ::aSelected, uVal ) ) > 0
         hb_ADel( ::aSelected, uTemp, .T. )
         ::DrawSelect()

         IF ::bSelected != NIL
            Eval( ::bSelected, Self, uVal, .F. )
         ENDIF

      ELSE

         AAdd( ::aSelected, uVal )
         ::DrawSelect()

         IF ::bSelected != NIL
            Eval( ::bSelected, Self, uVal, .T. )
         ENDIF

      ENDIF

#ifdef __EXT_SELECTION__
   ENDIF
#endif

RETURN Self

* ============================================================================
* METHOD TSBrowse:KeyUp()  Version 9.0 Nov/30/2009
* ============================================================================

METHOD KeyUp( nKey, nFlags ) CLASS TSBrowse

   IF ! ::lEnabled
      RETURN 0
   ENDIF

   IF lNoAppend != NIL
      ::lCanAppend := .T.
      lNoAppend := NIL
   ENDIF

   IF nKeyPressed != NIL

      ::Refresh( .F. )
      nKeyPressed := NIL

      IF ::bChange != NIL
         Eval( ::bChange, Self, nKey )
      ENDIF

   ENDIF

RETURN ::Super:KeyUp( nKey, nFlags )

* ============================================================================
* METHOD TSBrowse:LButtonDown() Version 9.0 Nov/30/2009
* ============================================================================

METHOD LButtonDown( nRowPix, nColPix, nKeyFlags ) CLASS TSBrowse

   LOCAL nClickRow, nSkipped, nI, lHeader, lFooter, nIcon, nAtCol, bLClicked, lMChange, lSpecHd, ;
      nColPixPos := 0, ;
      uPar1 := nRowPix, ;
      uPar2 := nColPix, ;
      nColInit := ::nColPos - 1, ;
      lDrawRow := .F., ;
      lDrawCol := .F., ;
      nLines := ::nRowCount(), ;
      nCol := 0, ;
      oCol, ix

   DEFAULT ::lDontChange := .F.

   IF ! ::lEnabled .OR. ::lDontChange
      RETURN 0
   ENDIF

   IF EmptyAlias( ::cAlias )
      RETURN 0
   ENDIF

   ::lNoPaint := .F.

   IF ::nFreeze > 0
      FOR nI := 1 TO ::nFreeze
         nColPixPos += ::GetColSizes()[ nI ]
      NEXT
   ENDIF

   nClickRow := ::GetTxtRow( nRowPix )
   nAtCol := Max( ::nAtColActual( nColPix ), 1 ) // JP 1.31
   lHeader := nClickRow == 0 .AND. ::lDrawHeaders
   lFooter := nClickRow == -1 .AND. iif( ::lDrawFooters != NIL, ::lDrawFooters, .F. )
   lSpecHd := nClickRow == -2 .AND. iif( ::lDrawSpecHd != NIL, ::lDrawSpecHd, .F. )
   ::oWnd:nLastKey := 0

   IF ::aColumns[ nAtCol ]:lNoHilite .AND. ! lHeader .AND. ! lFooter

      IF nAtCol <= ::nFreeze .AND. ::lLockFreeze
         nAtCol := ::nFreeze + 1
      ENDIF
   ENDIF

   IF ::oWnd:hCtlFocus != NIL .AND. ::oWnd:hCtlFocus != ::hWnd
      IF nClickRow == -2 .AND. ::nColSpecHd > 0
         oCol := ::oWnd:aColumns[ ::nColSpecHd ]
      ELSE
         oCol := ::oWnd:aColumns[ ::oWnd:nCell ]
      ENDIF

      IF oCol:oEdit != NIL
         // JP 1.40-64
         IF ( nClickRow == ::nRowPos .AND. nAtCol == ::oWnd:nCell ) .OR. ;
               ( nClickRow == -2 .AND. ::lDrawSpecHd .AND. nAtCol == ::nColSpecHd )

            DO CASE
            CASE "TSMULTI" $ Upper( oCol:oEdit:ClassName() )
               RETURN 0
            CASE "TCOMBOBOX" $ Upper( oCol:oEdit:ClassName() )
               PostMessage( ::oWnd:hCtlFocus, WM_LBUTTONDOWN, nKeyFlags, nMakeLong( nColPix - nCol, nRowPix ) )
               RETURN 0
            CASE "TBTNBOX" $ Upper( oCol:oEdit:ClassName() ) // JP 1.64
               PostMessage( ::oWnd:hCtlFocus, WM_LBUTTONDOWN, nKeyFlags, nMakeLong( nColPix - nCol, nRowPix ) )
               RETURN 0
            OTHERWISE
               ix := GetControlIndex( ::cChildControl, ::cParentWnd )
               IF ix > 0
                  nCol := _HMG_aControlCol[ ix ]
               ENDIF
               IF nCol > nColPix
                  nCol := 0
               ENDIF
               PostMessage( ::oWnd:hCtlFocus, WM_LBUTTONDOWN, nKeyFlags, nMakeLong( nColPix - nCol, nRowPix ) )
               RETURN 0
            ENDCASE
         ELSE
            DO CASE
            CASE "TSMULTI" $ Upper( oCol:oEdit:ClassName() )
               IF oCol:oEdit:bLostFocus != NIL
                  Eval( oCol:oEdit:bLostFocus, VK_ESCAPE )
               ENDIF
            CASE "TBTNBOX" $ Upper( oCol:oEdit:ClassName() )
               IF oCol:oEdit:bLostFocus != NIL
                  Eval( oCol:oEdit:bLostFocus, VK_ESCAPE )
               ELSE
                  Eval( oCol:oEdit:bKeyDown, VK_ESCAPE, 0, .T. )
               ENDIF
            OTHERWISE
               IF oCol:oEdit:bLostFocus != NIL
                  Eval( oCol:oEdit:bLostFocus, VK_ESCAPE )
               ENDIF
            ENDCASE
            SetFocus( ::hWnd )
            ::Refresh( .T. )
         ENDIF
         // end
      ENDIF
   ENDIF

   IF ::lIconView

      IF ( nIcon := ::nAtIcon( nRowPix, nColPix ) ) != 0
         ::DrawIcon( nIcon )
      ENDIF

      RETURN NIL

   ENDIF

   SetFocus( ::hWnd )

   IF ::nLen < 1 .OR. ::lIsTxt
      IF ! ::lCanAppend .OR. ::lIsTxt
         RETURN 0
      ELSEIF nClickRow > 0
         ::PostMsg( WM_KEYDOWN, VK_DOWN, nMakeLong( 0, 0 ) )
         RETURN 0
      ENDIF
   ENDIF

   IF lHeader .AND. ValType( nKeyFlags ) == "N"
      lMChange := ::lMChange
      ::lMChange := .F.

      IF ::nHeightSuper == 0 .OR. ( ::nHeightSuper > 0 .AND. nRowPix >= ::nHeightSuper )
         ::DrawPressed( nAtCol )
      ENDIF

      IF ::aActions != NIL .AND. nAtCol <= Len( ::aActions )

         IF ::aActions[ nAtCol ] != NIL
            ::DrawHeaders()
            Eval( ::aActions[ nAtCol ], Self, uPar1, uPar2 )

            IF ::oWnd:hCtlFocus != NIL .AND. ::oWnd:hCtlFocus != ::hWnd
               CursorArrow()
               RETURN 0
            ENDIF

            ::DrawHeaders()
         ENDIF
      ENDIF

      ::lMChange := lMChange

   ELSEIF lFooter

      lMChange := ::lMChange
      ::lMChange := .F.

      IF ::aColumns[ nAtCol ]:bFLClicked != NIL

         Eval( ::aColumns[ nAtCol ]:bFLClicked, uPar1, uPar2, ::nAt, Self )

         IF ::oWnd:hCtlFocus != NIL .AND. ::oWnd:hCtlFocus != ::hWnd
            RETURN 0
         ENDIF

      ENDIF

      ::lMChange := lMChange
      ::DrawFooters()

   ELSEIF lSpecHd .AND. ::lEditableHd

      lMChange := ::lMChange
      ::lMChange := .F.
      IF ::aColumns[ nAtCol ]:bSLClicked != NIL

         Eval( ::aColumns[ nAtCol ]:bSLClicked, uPar1, uPar2, ::nAt, Self )

         IF ::oWnd:hCtlFocus != NIL .AND. ::oWnd:hCtlFocus != ::hWnd
            RETURN 0
         ENDIF
      ELSE
         IF ::lEditingHd
            oCol := ::oWnd:aColumns[ ::oWnd:nCell ]
            IF oCol:oEditSpec != NIL
               ix := GetControlIndex( ::cChildControl, ::cParentWnd )
               IF ix > 0
                  nCol := _HMG_aControlCol[ ix ]
               ENDIF
               IF nCol > nColPix
                  nCol := 0
               ENDIF
               PostMessage( ::oWnd:hCtlFocus, WM_LBUTTONDOWN, nKeyFlags, nMakeLong( nColPix - nCol, nRowPix ) )
               RETURN 0
            ENDIF
         ENDIF
      ENDIF

      ::lMChange := lMChange
      ::DrawHeaders()

   ELSEIF lSpecHd .AND. ::aColumns[ nAtCol ]:bSLClicked != NIL  // SergKis 11.11.21

      Eval( ::aColumns[ nAtCol ]:bSLClicked, uPar1, uPar2, ::nAt, Self )

   ENDIF

   IF ::lMChange .AND. nClickRow == 0 .AND. ! ::lNoMoveCols

      IF AScan( ::GetColSizes(), {| nColumn | nColPixPos += nColumn, nColInit++, ;
            nColPix >= nColPixPos - 2 .AND. nColPix <= nColPixPos + 2 }, ::nColPos ) != 0

         ::lLineDrag := .T.
         ::VertLine( nColPixPos, nColInit, nColPixPos - nColPix )
      ELSE
         ::lColDrag := .T.
         ::nDragCol := ::nAtCol( nColPix )
      ENDIF

      IF ! ::lCaptured
         ::lCaptured := .T.
         ::Capture()
      ENDIF

   ENDIF

   IF nClickRow > 0 .AND. nClickRow != ::nRowPos .AND. nClickRow < ( nLines + 1 )

      ::ResetSeek()
      ::DrawLine()

      nSkipped := ::Skip( nClickRow - ::nRowPos )
      ::nRowPos += nSkipped

      IF ::oVScroll != NIL
         ::oVScroll:SetPos( ::RelPos( ::nLogicPos() ) )
      ENDIF

      lDrawRow := .T.
      ::lHitTop := .F.
      ::lHitBottom := .F.
      ::lAppendMode := .F.

      IF ::bChange != NIL
         Eval( ::bChange, Self, ::oWnd:nLastKey )
      ENDIF

   ENDIF

   IF nClickRow > 0 .OR. ( ! ::lDrawHeaders .AND. nClickRow >= 0 )

      bLClicked := iif( ::aColumns[ nAtCol ]:bLClicked != NIL, ::aColumns[ nAtCol ]:bLClicked, ::bLClicked )

      IF !( ::lLockFreeze .AND. ::nAtCol( nColPix, .T. ) <= ::nFreeze )
         lDrawCol := ::HiliteCell( ::nCell, nColPix )
      ENDIF

      IF bLClicked != NIL
         Eval( bLClicked, uPar1, uPar2, nKeyFlags, Self )
      ENDIF

      IF ! ::lNoHScroll .AND. ::oHScroll != NIL .AND. lDrawCol
         ::oHScroll:SetPos( ::nCell )
      ENDIF

   ENDIF

   IF lDrawRow .OR. lDrawCol
      ::DrawSelect()
   ENDIF

   IF ::lCellBrw

      IF ::aColumns[ ::nCell ]:bGotFocus != NIL .AND. ::nOldCell != ::nCell
         Eval( ::aColumns[ ::nCell ]:bGotFocus, ::nOldCell, ::nCell, Self )
      ENDIF

      IF ::aColumns[ ::nOldCell ]:bLostFocus != NIL .AND. ::nOldCell != ::nCell
         Eval( ::aColumns[ ::nOldCell ]:bLostFocus, ::nCell, ::nOldCell, Self )
      ENDIF

      ::nOldCell := ::nCell

   ENDIF

   ::lGrasp := ! lHeader

   IF ::oWnd:hCtlFocus != NIL .AND. ::oWnd:hCtlFocus == ::hWnd
      ::lMouseDown := .T.
   ENDIF

RETURN 0

* ============================================================================
* METHOD TSBrowse:LButtonUp() Version 9.0 Nov/30/2009
* ============================================================================

METHOD LButtonUp( nRowPix, nColPix, nFlags ) CLASS TSBrowse

   LOCAL nClickRow, nDestCol

   DEFAULT ::lDontChange := .F.

   IF ! ::lEnabled .OR. ::lDontChange
      RETURN 0
   ENDIF

   IF nKeyPressed != NIL
      ::DrawPressed( nKeyPressed, .F. )
   ENDIF

   IF ::lCaptured
      ::lCaptured := .F.
      ReleaseCapture()

      IF ::lLineDrag
         ::lLineDrag := .F.
         ::VertLine()
      ELSE
         ::lColDrag := .F.
         nClickRow := ::GetTxtRow( nRowPix )
         nDestCol := ::nAtCol( nColPix )

         // we gotta be on header row within listbox and not same colm
         IF nClickRow == 0 .OR. nClickRow == -2
            IF nColPix > ::nLeft .AND. ::nDragCol != nDestCol

               IF ::lMoveCols
                  ::MoveColumn( ::nDragCol, nDestCol )
               ELSE
                  ::Exchange( ::nDragCol, nDestCol )
               ENDIF

               IF ValType( ::bColDrag ) == "B"
                  Eval( ::bColDrag, nDestCol, ::nDragCol, Self )
               ENDIF
            ELSEIF ::nDragCol = nDestCol

               IF ::aColumns[ nDestCol ]:bHLClicked != NIL
                  ::DrawHeaders()
                  Eval( ::aColumns[ nDestCol ]:bHLClicked, nRowPix, nColPix, ::nAt, Self )
                  ::DrawHeaders()
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   ::lGrasp := .F.
   ::Super:LButtonUp( nRowPix, nColPix, nFlags )

RETURN 0

* ============================================================================
* METHOD TSBrowse:LDblClick() Version 9.0 Nov/30/2009
* ============================================================================

METHOD LDblClick( nRowPix, nColPix, nKeyFlags ) CLASS TSBrowse

   LOCAL nClickRow := ::GetTxtRow( nRowPix ), ;
      nCol := ::nAtColActual( nColPix ), ;
      uPar1 := nRowPix, ;
      uPar2 := nColPix

   ::oWnd:nLastKey := 0

   IF ! ::lEnabled .OR. ::lDontChange
      RETURN Self
   ENDIF

   IF ( nClickRow == ::nRowPos .AND. nClickRow > 0 ) .OR. ( nClickRow == ::nRowPos .AND. ! ::lDrawHeaders )

      IF ::lCellBrw .AND. ::IsEditable( nCol )

         ::nColSpecHd := 0
         IF ValType( ::bDataEval( ::aColumns[ nCol ] ) ) == "L" .AND. ;
               ::aColumns[ nCol ]:lCheckBox // virtual checkbox
            ::PostMsg( WM_CHAR, VK_SPACE, 0 )
         ELSEIF ::aColumns[ nCol ]:oEdit != NIL
            ::PostMsg( WM_KEYDOWN, VK_RETURN, 0 )
         ELSEIF ::bLDblClick != NIL
            Eval( ::bLDblClick, uPar1, uPar2, nKeyFlags, Self )
         ELSE
            ::PostMsg( WM_KEYDOWN, VK_RETURN, 0 )
         ENDIF

         RETURN 0

#ifndef __EXT_SELECTION__
      ELSEIF ::lCanSelect .AND. ::bUserKeys == NIL // Added 28.09.2012
         ::Selection()
#endif
      ELSEIF ::bLDblClick != NIL
         Eval( ::bLDblClick, uPar1, uPar2, nKeyFlags, Self )
      ENDIF

   ELSEIF nClickRow == 0 .AND. ::lDrawHeaders .AND. ! ::lNoChangeOrd // GF 1.71

      IF ::bLDblClick != NIL .AND. Empty( ::aActions )        // SergKis 11.11.21
         Eval( ::bLDblClick, uPar1, uPar2, nKeyFlags, Self )
      ELSE
         ::SetOrder( ::nAtCol( nColPix, ! ::lSelector ) )
      ENDIF
#if 0
   ELSEIF nClickRow == -1 .AND. ! Empty( ::lDrawFooters ) // Added 19.05.2020

      IF ::bLDblClick != NIL
         Eval( ::bLDblClick, uPar1, uPar2, nKeyFlags, Self )
      ENDIF
#endif
   ELSEIF nClickRow == -2 .AND. ::lDrawSpecHd // SergKis 11.11.21

      IF ::aColumns[ nCol ]:lEditSpec .AND. ( ::lAutoSearch .OR. ::lAutoFilter )
         ::nColSpecHd := Min( iif( nCol <= ::nFreeze, ::nFreeze + 1, ::nAtCol( nColPix ) ), Len( ::aColumns ) )
         ::PostMsg( WM_KEYDOWN, VK_RETURN, 0 )
         RETURN 0
      ENDIF

      IF ::bLDblClick != NIL
         Eval( ::bLDblClick, uPar1, uPar2, nKeyFlags, Self )
      ENDIF

   ENDIF

RETURN Self

* ============================================================================
* METHOD TSBrowse:LoadFields() Version 9.0 Nov/30/2009 // modified by SergKis
* ============================================================================

METHOD LoadFields( lEditable, aColSel, cAlsSel, aNameSel, aHeadSel ) CLASS TSBrowse

   LOCAL n, nE, cHeading, nAlign, nSize, cData, cType, nDec, hFont, cPicture, ;
      cBlock, nCols, aNames, cKey, ;
      aColSizes := ::aColSizes, ;
      cOrder, nEle, ;
      cAlias, cName, aStru, ;
      aAlign := { "LEFT", "CENTER", "RIGHT", "VERT" }
   LOCAL cTmp := "", cHead, hFontH, aAdsType := ::aAdsFieldTypes, nType

   DEFAULT lEditable := ::lEditable, ;
      aColSizes := {}

   cAlias := iif( HB_ISCHAR( cAlsSel ), cAlsSel, ::cAlias )
   aStru := ( cAlias )->( dbStruct() )
   aNames := iif( HB_ISARRAY( aColSel ), aColSel, ::aColSel )
   nCols := iif( aNames == NIL, ( cAlias )->( FCount() ), Len( aNames ) )
   aColSizes := iif( Len( ::aColumns ) == Len( aColSizes ), NIL, aColSizes )

   IF HB_ISARRAY( aNames )
      FOR n := 1 TO nCols
         nE := ( cAlias )->( FieldPos( aNames[ n ] ) )
         IF nE == 0
            cTmp += hb_ntos( n ) + ". " + aNames[ n ] + CRLF
         ENDIF
      NEXT
      IF Len( cTmp ) > 0
         MsgStop( "Fields could not be found for " + cAlias + CRLF + CRLF + cTmp, "Error" )
      ENDIF
   ENDIF

   FOR n := 1 TO nCols

      nE := iif( aNames == NIL, n, ( cAlias )->( FieldPos( aNames[ n ] ) ) )

      IF ValType( ::aHeaders ) == "A" .AND. ! Empty( ::aHeaders ) .AND. n <= Len( ::aHeaders )
         cHeading := ::aHeaders[ n ]
         cHead := cHeading
      ELSE
         cHeading := ::Proper( ( cAlias )->( Field( nE ) ) )
      ENDIF

      IF HB_ISARRAY( aHeadSel ) .AND. Len( aHeadSel ) > 0 .AND. n <= Len( aHeadSel ) .AND. aHeadSel[ n ] != NIL
         cHeading := aHeadSel[ n ]
         cHead := cHeading
      ENDIF

      IF CRLF $ cHeading
         cData := ""
         FOR EACH cTmp IN hb_ATokens( cHeading, CRLF )
            IF Len( cTmp ) > Len( cData )
               cData := cTmp
            ENDIF
         NEXT
         cHeading := cData
         cData := NIL
      ENDIF

      IF ( nEle := AScan( ::aTags, {| e | Upper( cHeading ) $ Upper( e[ 2 ] ) } ) ) > 0
         cOrder := ::aTags[ nEle, 1 ]
         cKey := ( cAlias )->( ordKey() )

         IF Upper( cHeading ) $ Upper( cKey )
            ::nColOrder := iif( Empty( ::nColOrder ), Len( ::aColumns ) + 1, ::nColOrder )
         ENDIF
      ELSE
         cOrder := ""
      ENDIF

      nAlign := iif( ::aJustify != NIL .AND. Len( ::aJustify ) >= nE, ::aJustify[ nE ], ;
         iif( ( cAlias )->( ValType( FieldGet( nE ) ) ) == "N", 2, ;
         iif( ( cAlias )->( ValType( FieldGet( nE ) ) ) $ "DL", 1, 0 ) ) )

      nAlign := iif( ValType( nAlign ) == "L", iif( nAlign, 2, 0 ), ;
         iif( ValType( nAlign ) == "C", AScan( aAlign, nAlign ) - 1, nAlign ) )

      nSize := iif( ! aColSizes == NIL .AND. Len( aColsizes ) >= nE, aColSizes[ nE ], Nil )

      cType := aStru[ nE, 2 ]
      IF ( nType := AScan( aAdsType, {| e | e[ 1 ] == cType } ) ) > 0
         cType := aAdsType[ nType, 2 ]
      ENDIF
      cPicture := NIL
      IF cType == "C"
         cPicture := "@K " + Replicate( 'X', aStru[ nE, 3 ] )
      ELSEIF cType == "N"
         cPicture := Replicate( '9', aStru[ nE, 3 ] )
         IF aStru[ nE, 4 ] > 0
            cPicture := SubStr( cPicture, 1, aStru[ nE, 3 ] - aStru[ nE, 4 ] - 1 ) + '.' + Replicate( '9', aStru[ nE, 4 ] )
         ENDIF
         cPicture := "@K " + cPicture
      ELSEIF cType $ "^+"
         cPicture := Replicate( '9', 10 )
      ENDIF

      IF nSize == NIL
         cData := ( cAlias )->( FieldGet( nE ) )
         IF cType != ValType( cData ) .AND. nType > 0
            cType := ValType( cData )
         ENDIF
         nSize := aStru[ nE, 3 ]
         nDec := aStru[ nE, 4 ]
         hFont := iif( ::hFont != NIL, ::hFont, 0 )
         hFontH := iif( ::hFontHead != NIL, ::hFontHead, ::hFont )

         IF cType == "C"
            cData := PadR( Trim( cData ), nSize, "B" )
            nSize := GetTextWidth( 0, cData, hFont )
         ELSEIF cType == "N"
            cData := StrZero( cData, nSize, nDec )
            nSize := GetTextWidth( 0, cData, hFont )
         ELSEIF cType == "D"
            cData := cValToChar( iif( Empty( cData ), Date(), cData ) )
            nSize := Int( GetTextWidth( 0, cData + "BB", hFont ) ) + iif( lEditable, 30, 0 )
         ELSEIF cType == "M"
            nSize := iif( ::nMemoWV == NIL, 200, ::nMemoWV )
         ELSEIF cType $ "=@T"
            cPicture := NIL
            nSize := GetTextWidth( 0, Replicate( "9", 24 ), hFont )
         ELSEIF cType $ "^+"
            nSize := GetTextWidth( 0, Replicate( "9", 10 ), hFont )
         ELSE
            cData := cValToChar( cData )
            nSize := GetTextWidth( 0, cData, hFont )
         ENDIF

         nSize := Max( GetTextWidth( 0, Replicate( "B", Len( cHeading ) + 1 ), hFontH ), nSize )
         nSize += iif( ! Empty( cOrder ), 14, 0 )

      ELSEIF ValType( ::aColSizes ) == "A" .AND. ! Empty( ::aColSizes ) .AND. n <= Len( ::aColSizes )
         nSize := ::aColSizes[ n ]
      ENDIF

      IF ValType( ::aColSizes ) == "A" .AND. n <= Len( ::aColSizes ) .AND. Empty( ::aColSizes[ n ] )
         ::aColSizes[ n ] := nSize
      ENDIF

      IF ValType( ::aFormatPic ) == "A" .AND. ! Empty( ::aFormatPic ) .AND. n <= Len( ::aFormatPic ) .AND. !( cType $ "=@T" )
         cPicture := ::aFormatPic[ n ]
      ENDIF

      IF HB_ISCHAR( cHead )
         cHeading := cHead
      ENDIF

      cBlock := 'FieldWBlock("' + aStru[ nE, 1 ] + '",Select("' + cAlias + '"))'
      ::AddColumn( TSColumn():New( cHeading, FieldWBlock( aStru[ nE, 1 ], Select( cAlias ) ), cPicture, ;
         { ::nClrText, ::nClrPane }, { nAlign, DT_CENTER }, nSize,, lEditable,,, cOrder,,,, ;
         5,,,, Self, cBlock ) )

      cName := ( cAlias )->( FieldName( nE ) )

      ATail( ::aColumns ):cData := cAlias + "->" + FieldName( nE )
      ATail( ::aColumns ):cArea := cAlias // 06.08.2019
      ATail( ::aColumns ):cField := ( cAlias )->( FieldName( nE ) ) // 08.06.2018
      ATail( ::aColumns ):cFieldTyp := aStru[ nE, 2 ] // 18.07.2018
      ATail( ::aColumns ):nFieldLen := aStru[ nE, 3 ] // 18.07.2018
      ATail( ::aColumns ):nFieldDec := aStru[ nE, 4 ] // 18.07.2018

      IF aStru[ nE, 2 ] $ "=@T^+"
         ATail( ::aColumns ):lEdit := .F.
      ENDIF

      IF HB_ISARRAY( aNameSel ) .AND. Len( aNameSel ) > 0 .AND. n <= Len( aNameSel )
         IF HB_ISCHAR( aNameSel[ n ] ) .AND. ! Empty( aNameSel[ n ] )
            cName := aNameSel[ n ]
         ENDIF
      ENDIF

      ATail( ::aColumns ):cName := cName

      IF cType == "L"
         ATail( ::aColumns ):lCheckBox := .T.
      ENDIF

      IF ! Empty( cOrder )
         ATail( ::aColumns ):lIndexCol := .T.
      ENDIF

   NEXT

   IF ::nLen == 0
      cAlias := ::cAlias
      ::nLen := iif( ::bLogicLen == NIL, Eval( ::bLogicLen := {|| ( cAlias )->( LastRec() ) } ), Eval( ::bLogicLen ) )
   ENDIF

RETURN Self

* ============================================================================
* METHOD TSBrowse:nAtCol() Version 9.0 Nov/30/2009
* ============================================================================

METHOD nAtCol( nColPixel, lActual ) CLASS TSBrowse

   LOCAL nColumn := ::nColPos - 1, ;
      aSizes := ::GetColSizes(), ;
      nI, ;
      nPos := 0

   DEFAULT lActual := .F.

   IF ::nFreeze > 0

      IF lActual
         nColumn := 0
      ELSE
         FOR nI := 1 TO ::nFreeze
            nPos += aSizes[ nI ]
         NEXT
      ENDIF
   ENDIF

   WHILE nPos < nColPixel .AND. nColumn < ::nColCount()
      IF ::aColumns[ nColumn + 1 ]:lVisible // skip hidden columns
         nPos += aSizes[ nColumn + 1 ]
      ENDIF
      nColumn++
   ENDDO

RETURN nColumn

* ============================================================================
* METHOD TSBrowse:nAtColActual()
* ============================================================================

METHOD nAtColActual( nColPixel ) CLASS TSBrowse

   LOCAL nColumn := 0, ;
      aSizes := ::GetColSizes(), ;
      nI, ;
      nColPix := 0

   FOR nI := 1 TO ::nFreeze
      IF nColPixel > nColPix
         nColumn := nI
      ENDIF
      nColPix += aSizes[ nI ]
   NEXT

   FOR nI := 1 TO ::nColCount()
      IF nI > ::nFreeze
         IF nColPixel > nColPix
            nColumn++
         ENDIF
         nColPix += iif( ::IsColVis2( nI ), aSizes[ nI ], 0 )
      ENDIF
   NEXT

RETURN nColumn

* ============================================================================
* METHOD TSBrowse:nAtIcon() Version 9.0 Nov/30/2009
* ============================================================================

METHOD nAtIcon( nRow, nCol ) CLASS TSBrowse

   LOCAL nIconsByRow := Int( ::nWidth() / 50 )

   nRow -= 9
   nCol -= 1

   IF ( nCol % 50 ) >= 9 .AND. ( nCol % 50 ) <= 41
      RETURN Int( ( nIconsByRow * Int( nRow / 50 ) ) + Int( nCol / 50 ) ) + 1
   ELSE
      RETURN 0
   ENDIF

RETURN NIL

* ============================================================================
* METHOD TSBrowse:nLogicPos() Version 9.0 Nov/30/2009
* ============================================================================

METHOD nLogicPos() CLASS TSBrowse

   LOCAL cAlias, cOrderName, nLogicPos

   DEFAULT ::lIsDbf := .F., ;
      ::lIsTxt := .F.

   IF ! ::lIsDbf

      IF ::lIsTxt
         ::nAt := ::oTxtFile:RecNo()
      ENDIF

      IF AND( ::nDataType, DATATYPE_ADO ) != 0
         RETURN Eval( ::bKeyNo )
      ENDIF

      RETURN ::nAt

   ENDIF

   cAlias := ::cAlias

   cOrderName := iif( ::bTagOrder != NIL, ( cAlias )->( Eval( ::bTagOrder ) ), Nil )
   nLogicPos := ( cAlias )->( Eval( ::bKeyNo, NIL, Self ) )

   IF ::lFilterMode
      nLogicPos := nLogicPos - ::nFirstKey
   ENDIF

   nLogicPos := iif( nLogicPos <= 0, ::nLen + 1, nLogicPos )

RETURN nLogicPos

* ============================================================================
* METHOD TSBrowse:HandleEvent() Version 9.0 Nov/30/2009
* ============================================================================

METHOD HandleEvent( nMsg, nWParam, nLParam ) CLASS TSBrowse

   LOCAL nDelta, ix

   DEFAULT ::lNoPaint := .F., ;
      ::lDontChange := .F.

   IF ::lMoreFields
      IF ! Empty( ::MoreFields( nMsg, nWParam ) )
         RETURN 1
      ENDIF
   ENDIF
   IF HB_ISBLOCK( ::bEvents )
      IF ! Empty( Eval( ::bEvents, Self, nMsg, nWParam, nLParam ) )
         RETURN 1
      ENDIF
   ENDIF
   IF nMsg == WM_SETFOCUS .AND. ! ::lPainted
      RETURN 0
   ELSEIF nMsg == WM_GETDLGCODE
      RETURN ::GetDlgCode( nWParam )
   ELSEIF nMsg == WM_CHAR .AND. ::lEditing
      RETURN 0
   ELSEIF nMsg == WM_CHAR
      RETURN ::KeyChar( nWParam, nLParam )
   ELSEIF nMsg == WM_KEYDOWN .AND. ::lDontChange
      RETURN 0
   ELSEIF nMsg == WM_KEYDOWN
      RETURN ::KeyDown( nWParam, nLParam )
   ELSEIF nMsg == WM_KEYUP
      RETURN ::KeyUp( nWParam, nLParam )
   ELSEIF nMsg == WM_VSCROLL
      IF ::lDontchange
         RETURN NIL
      ENDIF
      IF nLParam == 0 .AND. ::lEnabled
         RETURN ::VScroll( Loword( nWParam ), HiWord( nWParam ) )
      ENDIF
   ELSEIF nMsg == WM_HSCROLL
      IF ! ::lEnabled
         RETURN 0
      ELSEIF ::lDontchange
         RETURN NIL
      ENDIF
      RETURN ::HScroll( Loword( nWParam ), HiWord( nWParam ) )
   ELSEIF nMsg == WM_ERASEBKGND .AND. ! ::lEditing
      ::lNoPaint := .F.
   ELSEIF nMsg == WM_DESTROY
      IF ! Empty( ::aColumns ) .AND. ::aColumns[ ::nCell ]:oEdit != NIL
         ix := ::aColumns[ ::nCell ]:oEdit:Atx
         IF ix > 0
            PostMessage( _HMG_aControlHandles[ ix ], WM_KEYDOWN, VK_ESCAPE, 0 )
         ENDIF
      ENDIF
#ifdef __EXT_SELECTION__
   ELSEIF nMsg == WM_LBUTTONDBLCLK .AND. _GetKeyState( VK_SHIFT )
      IF ::lCanSelect .AND. ! ::lEditable
         ::Selection()
      ENDIF
#endif
   ELSEIF nMsg == WM_LBUTTONDBLCLK
      RETURN ::LDblClick( HiWord( nLParam ), LoWord( nLParam ), nWParam )

   ELSEIF nMsg == WM_MOUSEWHEEL
      IF ::hWnd != 0 .AND. ::hWnd == GetFocus() .AND. ::lEnabled .AND. ! ::lDontChange
         nDelta := Bin2I( I2Bin( HiWord( nWParam ) ) ) / 120
         ::MouseWheel( nMsg, nDelta, LoWord( nLParam ), HiWord( nLParam ) )
      ENDIF
      RETURN 0

   ENDIF

RETURN ::Super:HandleEvent( nMsg, nWParam, nLParam )

* ============================================================================
* METHOD TSBrowse:HiliteCell() Version 9.0 Nov/30/2009
* ============================================================================

METHOD HiliteCell( nCol, nColPix ) CLASS TSBrowse

   LOCAL nI, nAbsCell, nRelCell, nNowPos, nOldPos, nLeftPix, ;
      lDraw := .F., ;
      lMove := .T.

   DEFAULT nCol := 1

   IF ! ::lCellBrw .AND. nColPix == NIL // if not browsing cell-style AND no nColPix, ignore call.
      RETURN lDraw                      // nColPix NOT nil means called from ::LButtonDown()
   ENDIF

   IF nCol < 1
      nCol := 1
   ELSEIF nCol > Len( ::aColumns )
      nCol := Len( ::aColumns )
   ENDIF

   IF Len( ::aColumns ) > 0

      IF nColPix != NIL // used internally by ::LButtonDown() only
         nAbsCell := ::nAtCol( nColPix, .F. )
         nRelCell := ::nAtCol( nColPix, .T. )

         IF nAbsCell >= ::nFreeze .AND. nRelCell <= ::nFreeze
            nNowPos := nRelCell
         ELSE
            nNowPos := nAbsCell
         ENDIF

         nOldPos := ::nCell

         IF ::nFreeze > 0 .AND. nOldPos < nNowPos .AND. ::lLockFreeze // frozen col and going right
            nNowPos := nAbsCell
            lMove := ( nOldPos > ::nFreeze )
         ENDIF

         IF nOldPos < nNowPos // going right
            nLeftPix := 0

            FOR nI := nOldPos TO nNowPos - 1
               lDraw := .T.
               ::nCell++
               nLeftPix += ::aColSizes[ nI ] // we need to know pixels left of final cell...
            NEXT

            ::nCell := iif( ::nCell > Len( ::aColumns ), Len( ::aColumns ), ::nCell )

            IF ::nWidth() < ( nLeftPix + ::aColSizes[ ::nCell ] ) .AND. ::nColPos < Len( ::aColumns ) .AND. lMove
               ::nColPos++
               ::Refresh( .F. )
            ENDIF
         ELSEIF nNowPos < nOldPos // going left

            FOR nI := nNowPos TO nOldPos - 1
               lDraw := .T.
               ::nCell--
            NEXT

            ::nCell := iif( ::nCell < 1, 1, ::nCell )
         ENDIF

         nCol := ::nCell
      ELSE
         lDraw := !( ::nCell == nCol )

         ::nColPos := Max( ::nFreeze + 1, ::nColPos )

         IF ::nFreeze > 0 .AND. ::lLockFreeze
            ::nCell := Max( nCol, ::nFreeze + 1 )
         ELSEIF ::nCell != nCol
            ::nCell := nCol
         ENDIF

         IF ! ::lNoHScroll .AND. ::oHScroll != NIL .AND. lDraw

            ::oHScroll:SetPos( nCol )

            IF ::lPainted
               ::Refresh( .F. )
            ENDIF

         ENDIF
      ENDIF

      IF ::lCellBrw
         // unhilite all columns EXCEPT those with "double cursor" (permanent) effect
         AEval( ::aColumns, {| oColumn | oColumn:lNoLite := ! oColumn:lFixLite } ) // allways .T. if no double cursor
         ::aColumns[ nCol ]:lNoLite := .F.
      ENDIF
   ENDIF

RETURN lDraw

* ============================================================================
* METHOD TSBrowse:HScroll() Version 9.0 Nov/30/2009
* ============================================================================

METHOD HScroll( nWParam, nLParam ) CLASS TSBrowse

   LOCAL nCol, nMsg, nPos

   nMsg := nWParam
   nPos := nLParam

   ::lNoPaint := .F.

   IF GetFocus() != ::hWnd
      SetFocus( ::hWnd )
   ENDIF

   DO CASE
   CASE nMsg == SB_LINEUP
      ::GoLeft()

   CASE nMsg == SB_LINEDOWN
      ::GoRight()

   CASE nMsg == SB_PAGEUP
      ::PanLeft()

   CASE nMsg == SB_PAGEDOWN
      nCol := ::nColPos + 1

      WHILE ::IsColVisible( nCol ) .AND. nCol <= Len( ::aColumns )
         ++nCol
      ENDDO

      IF nCol < Len ( ::aColumns )
         ::nColPos := ::nCell := nCol
         ::Refresh( .F. )
         ::oHScroll:SetPos( nCol )
      ELSE

         nCol := Len( ::aColumns )
         While ! ::IsColVisible( nCol ) .AND. ::nColPos < nCol
            ::nColPos++
         ENDDO
         ::nCell := nCol
         ::oHScroll:GoBottom()
         ::Refresh( .F. )
      ENDIF

      IF ::lCellBrw
         ::HiLiteCell( ::nCell )
      ENDIF

   CASE nMsg == SB_TOP
      ::PanHome()

   CASE nMsg == SB_BOTTOM
      ::PanEnd()

   CASE nMsg == SB_THUMBPOSITION
      ::HThumbDrag( nPos )

   CASE nMsg == SB_THUMBTRACK
      ::HThumbDrag( nPos )

   ENDCASE

RETURN 0

* ============================================================================
* METHOD TSBrowse:HThumbDrag() Version 9.0 Nov/30/2009
* ============================================================================

METHOD HThumbDrag( nPos ) CLASS TSBrowse

   LOCAL nI, nLeftPix, nColPos, ;
      nWidth := ::nWidth() - iif( ::oVScroll != NIL, GetSysMetrics( 2 ), 0 )

   IF ::oHScroll != NIL .AND. ! Empty( nPos )

      IF nPos >= Len( ::aColumns )
         IF ::IsColVisible( Len( ::aColumns ) )
            ::nCell := Len( ::aColumns )
            ::HiliteCell( ::nCell )
            ::Refresh( .F. )
         ELSE
            ::PanEnd()
         ENDIF
         ::oHScroll:GoBottom()
         RETURN Self
      ENDIF

      IF ::lIsTxt
         ::oHScroll:SetPos( ::nAt := nPos )
      ELSE
         IF ::lLockFreeze .AND. nPos <= ::nFreeze // watch out for frozen columns
            ::oHScroll:SetPos( ::nCell := ::nFreeze + 1 )
         ELSE
            ::oHScroll:SetPos( ::nCell := Min( nPos, Len( ::aColumns ) ) )
         ENDIF

         nLeftPix := 0 // check for frozen columns,

         FOR nI := 1 TO ::nFreeze // if any
            nLeftPix += ::aColSizes[ nI ]
         NEXT

         nColPos := ::nCell

         FOR nI := ::nCell TO 1 STEP -1 // avoid extra scrolling
            IF nLeftPix + ::aColSizes[ nI ] < nWidth // to the right of the
               nLeftPix += ::aColSizes[ nI ] // last cell (column)
               nColPos := nI
            ELSE
               EXIT
            ENDIF
         NEXT

         ::nColPos := nColPos
         ::HiliteCell( ::nCell )
      ENDIF

      ::Refresh( .F. )
   ENDIF

RETURN Self

* ============================================================================
* METHOD TSBrowse:InsColumn() Version 9.0 Nov/30/2009
* ============================================================================

METHOD InsColumn( nPos, oColumn ) CLASS TSBrowse

   LOCAL nI, ;
      nCell := ::nCell

   IF oColumn == NIL // if no Column object supplied
      RETURN NIL // return nil instead of reference to object
   ENDIF

   IF oColumn:lDefineColumn
      oColumn:DefColor( Self, oColumn:aColors )
      oColumn:DefFont ( Self )
   ENDIF

   DEFAULT nPos := 1

   IF ValType( nPos ) == "C"
      nPos := ::nColumn( nPos )
   ENDIF

   IF nPos < 1
      nPos := 1
   ELSEIF nPos > Len( ::aColumns ) + 1
      nPos := Len( ::aColumns ) + 1
   ENDIF

   ASize( ::aColumns, Len( ::aColumns ) + 1 )
   AIns( ::aColumns, nPos )
   ::aColumns[ nPos ] := oColumn

   ASize( ::aColSizes, Len( ::aColSizes ) + 1 )
   AIns( ::aColSizes, nPos )
   ::aColsizes[ nPos ] := iif( oColumn:lVisible, oColumn:nWidth, 0 )

   IF nPos == 1 .AND. Len( ::aColumns ) > 1 .AND. ::lSelector
      RETURN NIL
   ENDIF

   IF ! Empty( oColumn:cName ) .AND. oColumn:cName == "SELECTOR"
//      oColumn:nId := 0
   ELSE
      oColumn:nId := ++::nIdColumn
   ENDIF

   iif( nCell != nPos, ::nCell := iif( ::lPainted, nPos, nCell ), Nil )

   IF ! Empty( oColumn:cOrder ) // if column has a TAG, we
      ::SetOrder( nPos ) // set it as controling order
   ELSEIF ::nColOrder != 0 .AND. nPos <= ::nColOrder // if left of current order
      ::nColOrder++ // adjust position
   ENDIF

   IF ::lPainted
      ::HiliteCell( ::nCell )

      IF ::oHScroll != NIL
         ::oHScroll:SetRange( 1, Len( ::aColumns ) )
         ::oHScroll:SetPos( ::nCell )
      ENDIF

      IF ! Empty( ::aSuperHead )

         FOR nI := 1 TO Len( ::aSuperHead )

            IF nPos >= ::aSuperHead[ nI, 1 ] .AND. nPos <= ::aSuperHead[ nI, 2 ]
               ::aSuperHead[ nI, 2 ] ++
            ELSEIF nPos < ::aSuperHead[ nI, 1 ]
               ::aSuperHead[ nI, 1 ] ++
               ::aSuperHead[ nI, 2 ] ++
            ENDIF
         NEXT
      ENDIF

      ::Refresh( .F. )
      ::SetFocus()
   ENDIF

RETURN oColumn // returns reference to Column object

* ============================================================================
* METHOD TSBrowse:InsColNumber()
* ============================================================================

METHOD InsColNumber( nWidth, nColumn, cName, nAlign, uBitmap ) CLASS TSBrowse

   LOCAL oCol

   hb_default( @nWidth , 80 )
   hb_default( @nColumn, 1 )
   hb_default( @cName, iif( ::lIsDbf, "ORDKEYNO", "ARRAYNO" ) )

   IF ::lIsDbf

      DEFINE COLUMN oCol DATA 'hb_ntos(iif( IndexOrd() > 0, ORDKEYNO(), RecNo() ))' ;
         HEADER '#' ;
         FOOTER ' ' ;
         ALIGN 1, 1, 1 ;
         WIDTH nWidth ;
         PICTURE '9999999' ;
         MOVE 0 ;
         DBLCURSOR ;
         NAME &(cName)

      oCol:lEdit := .F.
      oCol:cAlias := ::cAlias
      oCol:cFooting := {| nc, ob | nc := ob:nLen, iif( Empty( nc ), '', hb_ntos( nc ) ) }

      IF !Empty( uBitmap ) .AND. Valtype( uBitmap ) $ "NC"
         IF Valtype( uBitmap ) == "N"
            oCol:aBitMaps := { Nil, StockBmp ( uBitmap ) }
         ELSE
            oCol:aBitMaps := { Nil, LoadImage( uBitmap ) }
         ENDIF
         oCol:uBmpCell := {|nc,ob,oc| oc := ob:aColumns[ nc ], ;
                                      nc := iif( (ob:cAlias)->(Deleted()), 2, 1 ), ;
                                      oc:aBitMaps[ nc ] }
      ENDIF
#ifndef __XHARBOUR__
      oCol:cData := 'hb_macroblock("' + oCol:cField + '")'
      oCol:bData := hb_macroBlock( oCol:cField )
#else
      oCol:cData := '{|| ' + oCol:cField + '}'
      oCol:bData := &( '{|| ' + oCol:cField + '}' )
#endif
   ELSEIF ::lIsArr

      DEFINE COLUMN oCol DATA {|| NIL } ;
         HEADER '#' ;
         FOOTER ' ' ;
         ALIGN 1, 1, 1 ;
         WIDTH nWidth ;
         PICTURE '9999999' ;
         MOVE 0 ;
         DBLCURSOR ;
         NAME &(cName)

      oCol:cFooting := {| nc, ob | nc := ob:nLen, iif( Empty( nc ), '', hb_ntos( nc ) ) }
      oCol:bValue := {| xx, ob | xx := ob, hb_ntos( ob:nAt ) }

   ENDIF

   oCol:lEmptyValToChar := .T.
   oCol:cFieldTyp := 'N'
   oCol:nFieldLen := 10
   oCol:nFieldDec := 0

   IF HB_ISNUMERIC( nAlign ) .AND. nAlign >= DT_LEFT .AND. nAlign <= DT_RIGHT
      oCol:nSAlign := nAlign
      oCol:nAlign  := nAlign
      oCol:nFAlign := nAlign
   ENDIF

   IF nColumn > 0 .AND. nColumn <= Len( ::aColumns )
      ::InsColumn( nColumn, oCol )
   ENDIF

RETURN oCol

* ============================================================================
* METHOD TSBrowse:IsColVisible() Version 9.0 Nov/30/2009
* ============================================================================

METHOD IsColVisible( nCol ) CLASS TSBrowse

   LOCAL nCols, nFirstCol, nLastCol, nWidth, nBrwWidth, xVar, ;
      aColSizes := ::GetColSizes()

   nCols := Len( aColSizes )
   nFirstCol := ::nColPos
   nLastCol := nFirstCol
   nWidth := 0
   nBrwWidth := ::nWidth() - iif( ::oVScroll != NIL, GetSysMetrics( 2 ), 0 )

   IF nCol < ::nColPos .OR. ::nColPos <= 0
      RETURN .F.
   ENDIF

   xVar := 1

   WHILE xVar <= ::nFreeze
      nWidth += aColSizes[ xVar++ ]
   ENDDO

   WHILE nWidth < nBrwWidth .AND. nLastCol <= nCol .AND. nLastCol <= nCols
      nWidth += aColSizes[ nLastCol ]
      nLastCol++
   ENDDO

   IF nCol <= --nLastCol
      Return ! nWidth > nBrwWidth
   ENDIF

RETURN .F.

* ============================================================================
* METHOD TSBrowse:IsColVis2() Version 9.0 Nov/30/2009
* ============================================================================

METHOD IsColVis2( nCol ) CLASS TSBrowse

   LOCAL nCols, nFirstCol, nLastCol, nBrwWidth, ;
      nWidth := 0, ;
      aColSizes := ::GetColSizes()

   nCols := Len( aColSizes )
   nFirstCol := ::nColPos
   nLastCol := nFirstCol

   // mt differs from iscolvisible here - allows for frozen column
   IF ::nFreeze > 0
      AEval( aColSizes, {| nSize | nWidth += nSize }, 1, ::nFreeze )
   ENDIF

   nBrwWidth := ::nWidth() - iif( ::oVScroll != NIL, GetSysMetrics( 2 ), 0 )

   IF nCol < ::nColPos
      RETURN .F.
   ENDIF

   DO WHILE nWidth < nBrwWidth .AND. nLastCol <= nCols
      nWidth += aColSizes[ nLastCol ]
      nLastCol++
   ENDDO

   IF nCol <= --nLastCol
      // mt differs from new iscolvisible here
      RETURN .T.
   ENDIF

RETURN .F.

* ============================================================================
* METHOD TSBrowse:Insert() Version 9.0 Nov/30/2009
* ============================================================================

METHOD Insert( cItem, nAt ) CLASS TSBrowse

   LOCAL nMin, nMax, nPage

   DEFAULT nAt := ::nAt
   DEFAULT cItem := AClone( ::aDefValue )

   IF ! ::lIsArr
      RETURN NIL
   ENDIF

   IF ValType( cItem ) == "A" .AND. cItem[ 1 ] == NIL
      hb_ADel( cItem, 1, .T. )
   ENDIF

   ASize( ::aArray, Len( ::aArray ) + 1 )
   nAt := Max( 1, nAt )
   AIns( ::aArray, nAt )
   ::aArray[ nAt ] := iif( ValType( cItem ) == "A", cItem, { cItem } )

   ::nLen := Eval( ::bLogicLen )

   IF ::lNoVScroll
      IF ::nLen > ::nRowCount()
         ::lNoVScroll := .F.
      ENDIF
   ENDIF

   IF ! ::lNoVScroll
      nMin := Min( 1, ::nLen )
      nMax := Min( ::nLen, MAX_POS )
      nPage := Min( ::nRowCount(), ::nLen )
      ::oVScroll := TSBScrlBar():WinNew( nMin, nMax, nPage, .T., Self )
   ENDIF

   ::Refresh( .T. )

RETURN Self

* ============================================================================
* METHOD TSBrowse:AddItem() Version 7.0 Oct/10/2007
* ============================================================================

METHOD AddItem( cItem ) CLASS TSBrowse

   LOCAL nMin, nMax, nPage

   DEFAULT cItem := AClone( ::aDefValue )

   IF ! ::lIsArr
      RETURN NIL
   ENDIF

   IF ValType( cItem ) == "A" .AND. cItem[ 1 ] == NIL
      hb_ADel( cItem, 1, .T. )
   ENDIF

   cItem := iif( ValType( cItem ) == "A", cItem, { cItem } )

   IF ::lPhantArrRow .AND. Len( ::aArray ) == 1
      ::SetArray( { cItem }, .T. )
      ::lPhantArrRow := .F.
   ELSEIF Len( ::aArray ) == 0
      ::SetArray( { cItem }, .T. )
   ELSE
      AAdd( ::aArray, cItem )
   ENDIF

   ::nLen := Eval( ::bLogicLen )

   IF ::lNoVScroll
      IF ::nLen > ::nRowCount()
         ::lNoVScroll := .F.
      ENDIF
   ENDIF

   IF ! ::lNoVScroll
      nMin := Min( 1, ::nLen )
      nMax := Min( ::nLen, MAX_POS )
      nPage := Min( ::nRowCount(), ::nLen )
      ::oVScroll := TSBScrlBar():WinNew( nMin, nMax, nPage, .T., Self )
   ENDIF

   ::Refresh( .T. )

RETURN Self

* ============================================================================
* METHOD TSBrowse:lEditCol() Version 7.0 Jul/15/2004
* ============================================================================

METHOD lEditCol( uVar, nCol, cPicture, bValid, nClrFore, nClrBack ) CLASS TSBrowse

RETURN ::Edit( uVar, nCol,,, cPicture, bValid, nClrFore, nClrBack ) // just for compatibility

* ============================================================================
* METHOD TSBrowse:lIgnoreKey() Version 9.0 Nov/30/2009
// Checks if any of the predefined navigation keys has been remapped so as to
// ignore its default behavior and forward it to TWindow class in case a new
// behavior has been defined in ::bKeyDown. Uses the new nested array IVar
// ::aKeyRemap which has the following structure (data type in parens):
//
// { { VK_ to ignore(n), alone(l), ctrl(l), shift(l), alt(l), ctrl+shift(l), bBlock }, ... }
//
// Example:  AAdd( ::aKeyRemap, { VK_END, .T., .F., .T., .F., .F., { || Tone(600) } } )
// will ignore End key alone and with shift+end combinations
//
// It's the programmer's (ie you!) responsibility to make sure each subarray
// has 6 elements of the specified data type or kaboom!
// This method is called by ::KeyDown() which provides nKey and nFlags.
// If called directly, you must specify the parameter nKey (nFlags is always ignored)
* ============================================================================

METHOD lIgnoreKey( nKey, nFlags ) CLASS TSBrowse

   LOCAL lIgnore := .F., ;
      nAsync := 2, ; // key by itself
   nIgnore := AScan( ::aKeyRemap, {| aRemap | aRemap[ 1 ] == nKey } )

   HB_SYMBOL_UNUSED( nFlags )

   IF nIgnore > 0

      IF _GetKeyState( VK_CONTROL ) .AND. _GetKeyState( VK_SHIFT )
         nAsync := 6
      ELSEIF _GetKeyState( VK_CONTROL )
         nAsync := 3
      ELSEIF _GetKeyState( VK_SHIFT )
         nAsync := 4
      ELSEIF _GetKeyState( VK_MENU ) // alt key
         nAsync := 5
      ENDIF

      lIgnore := ::aKeyRemap[ nIgnore, nAsync ]

      IF lIgnore .AND. ValType( ::aKeyRemap[ nIgnore, 7 ] ) == "B"
         Eval( ::aKeyRemap[ nIgnore, 7 ] )
      ENDIF

   ENDIF

RETURN lIgnore

* ============================================================================
* METHOD TSBrowse:LoadRecordSet() Version 9.0 Nov/30/2009
* ============================================================================

METHOD LoadRecordSet() CLASS TSBrowse

   LOCAL n, nE, cHeading, nAlign, aColSizes, cData, cType, nDec, hFont, cBlock, nType, nWidth, ;
      cOrder := Upper( ::oRSet:Sort ), ;
      aAlign := { "LEFT", "CENTER", "RIGHT", "VERT" }, ;
      nCols := ::oRSet:Fields:Count(), ;
      aRName := {}, ;
      aNames := ::aColSel

   IF ! Empty( aNames )
      FOR n := 1 TO nCols
         AAdd( aRName, ::oRSet:Fields( n - 1 ):Name )
      NEXT

      nCols := Len( aNames )
   ENDIF

   cOrder := AllTrim( StrTran( StrTran( cOrder, "ASC" ), "DESC" ) )
   aColSizes := iif( Len( ::aColumns ) == Len( ::aColSizes ), NIL, ::aColSizes )

   FOR n := 1 TO nCols

      nE := iif( Empty( aNames ), n - 1, AScan( aRName, {| e | Upper( e ) == Upper( aNames[ n ] ) } ) - 1 )

      cHeading := iif( ! Empty( ::aHeaders ) .AND. Len( ::aHeaders ) >= n, ::aHeaders[ n ], ;
         ::Proper( ::oRSet:Fields( nE ):Name ) )

      nAlign := iif( ::aJustify != NIL .AND. Len( ::aJustify ) >= n, ::aJustify[ n ], ;
         iif( ValType( ::oRSet:Fields( nE ):Value ) == "N", 2, ;
         iif( ValType( ::oRSet:Fields( nE ):Value ) == "L", 1, 0 ) ) )

      nAlign := iif( ValType( nAlign ) == "L", iif( nAlign, 2, 0 ), ;
         iif( ValType( nAlign ) == "C", AScan( aAlign, nAlign ) - 1, nAlign ) )

      nWidth := iif( ! aColSizes == NIL .AND. Len( aColsizes ) >= n, aColSizes[ n ], Nil )

      IF nWidth == NIL
         cData := ::oRSet:Fields( nE ):Value
         cType := ClipperFieldType( nType := ::oRSet:Fields( nE ):Type )
         IF ValType( cType ) != "C"
            // msginfo(::oRSet:Fields( nE ):Name, cType )
            LOOP
         ENDIF

         nWidth := iif( cType == "N", ::oRSet:Fields( nE ):Precision, ::oRSet:Fields( nE ):DefinedSize )
         nDec := iif( cType != "N", 0, iif( nType == adCurrency, 2, ;
            iif( AScan( { adDecimal, adNumeric, adVarNumeric }, nType ) > 0, ::oRSet:Fields( nE ):NumericScale, ;
            0 ) ) )
         hFont := iif( ::oFont != NIL, ::oFont:hFont, 0 )

         IF cType == "C" .AND. ValType( cData ) == "C"
            cData := PadR( Trim( cData ), nWidth, "B" )
            nWidth := GetTextWidth( 0, cData, hFont )
         ELSEIF cType == "N"
            cData := StrZero( Val( Replicate( "4", nWidth ) ), nDec )
            nWidth := GetTextWidth( 0, cData, hFont )
         ELSEIF cType == "D"
            cData := cValToChar( iif( ! Empty( cData ), cData, Date() ) )
            nWidth := Int( GetTextWidth( 0, cData, hFont ) ) + 22
         ELSEIF cType == "M"
            // cData := cValToChar( cData )
            nWidth := iif( ::nMemoWV == NIL, 200, ::nMemoWV )
         ELSE
            cData := cValToChar( cData )
            nWidth := GetTextWidth( 0, cData, hFont )
         ENDIF
      ENDIF

      nWidth := Max( nWidth, GetTextWidth( 0, cHeading, hFont ) )
      cBlock := 'AdoGenFldBlk( Self:oRS, ' + LTrim( Str( nE ) ) + ' )'
      ::AddColumn( TSColumn():New( cHeading, AdoGenFldBlk( ::oRSet, nE ),, { ::nClrText, ::nClrPane }, ;
         { nAlign, DT_CENTER }, nWidth,, ::lEditable,,, ::oRSet:Fields( nE ):Name,,,, ;
         5,,,, Self, cBlock ) )
      ATail( ::aColumns ):cDatatype := cType
      ATail( ::aColumns ):Cargo := ::oRSet:Fields( nE ):Name
      ATail( ::aColumns ):lEdit := .F.

      IF cOrder == Upper( cHeading )
         ::nColOrder := Len( ::aColumns )
         ATail( ::aColumns ):cOrder := cOrder
      ENDIF
   NEXT

RETURN Self

* ============================================================================
* METHOD TSBrowse:LoadRelated() Version 9.0 Nov/30/2009
* ============================================================================

METHOD LoadRelated( cAlias, lEditable, aNames, aHeaders ) CLASS TSBrowse

   LOCAL n, nE, cHeading, nAlign, nSize, cData, cType, nDec, hFont, aStru, nArea, nFields, cBlock

   DEFAULT lEditable := .F.

   IF Empty( cAlias )
      RETURN Self
   ENDIF

   cAlias := AllTrim( cAlias )
   nArea := Select( cAlias )
   aStru := ( cAlias )->( dbStruct() )
   nFields := iif( aNames == NIL, ( cAlias )->( FCount() ), Len( aNames ) )

   FOR n := 1 TO nFields

      nE := iif( aNames == NIL, n, ( cAlias )->( FieldPos( aNames[ n ] ) ) )

      cHeading := iif( aHeaders != NIL .AND. Len( aHeaders ) >= n, ;
         aHeaders[ n ], cAlias + "->" + ;
         ::Proper( ( cAlias )->( Field( nE ) ) ) )

      nAlign := iif( ( cAlias )->( ValType( FieldGet( nE ) ) ) == "N", 2, ;
         iif( ( cAlias )->( ValType( FieldGet( nE ) ) ) == "L", 1, 0 ) )

      cData := ( cAlias )->( FieldGet( nE ) )
      cType := ValType( cData )
      nSize := aStru[ nE, 3 ]
      nDec := aStru[ nE, 4 ]
      hFont := iif( ::hFont != NIL, ::hFont, 0 )

      IF cType == "C"
         cData := PadR( Trim( cData ), nSize, "B" )
         nSize := GetTextWidth( 0, cData, hFont )
      ELSEIF cType == "N"
         cData := StrZero( cData, nSize, nDec )
         nSize := GetTextWidth( 0, cData, hFont )
      ELSEIF cType == "D"
         cData := cValToChar( iif( ! Empty( cData ), cData, Date() ) )
         nSize := Int( GetTextWidth( 0, cData, hFont ) * 1.15 )
      ELSE
         cData := cValToChar( cData )
         nSize := GetTextWidth( 0, cData, hFont )
      ENDIF

      nSize := Max( GetTextWidth( 0, Replicate( "B", Len( cHeading ) ), ;
         hFont ), nSize )
      cBlock := 'FieldWBlock( "' + ( cAlias )->( Field( nE ) ) + '", Select( "' + ;
         cAlias + '" ) )'
      ::AddColumn( TSColumn():New( cHeading, FieldWBlock( ( cAlias )->( Field( nE ) ), nArea ),, ;
         { ::nClrText, ::nClrPane }, { nAlign, DT_CENTER }, nSize,, lEditable,,,,,,, ;
         5,,,, Self, cBlock ) )

      ATail( ::aColumns ):cAlias := cAlias
      ATail( ::aColumns ):cData := cAlias + "->" + FieldName( nE )
      ATail( ::aColumns ):cField := cAlias + "->" + FieldName( nE )
      ATail( ::aColumns ):cName := cAlias + "->" + ( cAlias )->( FieldName( nE ) )

      ATail( ::aColumns ):cArea := cAlias
      ATail( ::aColumns ):cFieldTyp := aStru[ nE, 2 ]
      ATail( ::aColumns ):nFieldLen := aStru[ nE, 3 ]
      ATail( ::aColumns ):nFieldDec := aStru[ nE, 4 ]

   NEXT

RETURN Self

* ============================================================================
* METHOD TSBrowse:Look3D() Version 9.0 Nov/30/2009
* ============================================================================

METHOD Look3D( lOnOff, nColumn, nLevel, lPhantom ) CLASS TSBrowse

   DEFAULT lOnOff := .T., ;
      nColumn := 0, ;
      lPhantom := .T., ;
      nLevel := 0

   ::l3DLook := lOnOff // used internally

   IF nColumn > 0
      IF nLevel = 1 .OR. nLevel = 0
         ::aColumns[ nColumn ]:l3DLook := lOnOff
      ENDIF

      IF nLevel = 2 .OR. nLevel = 0
         ::aColumns[ nColumn ]:l3DLookHead := lOnOff
      ENDIF

      IF nLevel = 3 .OR. nLevel = 0
         ::aColumns[ nColumn ]:l3DLookFoot := lOnOff
      ENDIF
   ELSE
      IF nLevel > 0
         AEval( ::aColumns, {| oCol | iif( nLevel = 1, oCol:l3DLook := lOnOff, iif( nLevel = 2, ;
            oCol:l3DLookHead := lOnOff, oCol:l3DLookFoot := lOnOff ) ) } )
      ELSE
         AEval( ::aColumns, {| oCol | oCol:l3DLook := lOnOff, oCol:l3DLookHead := lOnOff, ;
            oCol:l3DLookFoot := lOnOff } )
      ENDIF
   ENDIF

   IF lPhantom
      ::nPhantom := PHCOL_GRID
   ELSE
      ::nPhantom := PHCOL_NOGRID
   ENDIF

   ::Refresh( .T. )

RETURN Self

* ============================================================================
* METHOD TSBrowse:LostFocus() Version 9.0 Nov/30/2009
* ============================================================================

METHOD LostFocus( hCtlFocus ) CLASS TSBrowse

   LOCAL nRecNo, uTag

   DEFAULT ::aControls := {}

   IF ::lEditing .AND. Len( ::aControls ) > 0 .AND. ;
         hCtlFocus == ::aControls[ 1 ]
      RETURN 0
   ENDIF

   IF ::lEditing

      IF ::aColumns[ ::nCell ]:oEdit != NIL
         IF IsControlDefined( ::cChildControl, ::cParentWnd )
            ::aColumns[ ::nCell ]:oEdit:End()
            ::aColumns[ ::nCell ]:oEdit := NIL
         ENDIF
      ENDIF
      ::lEditing := ::lPostEdit := .F.
   ENDIF

   ::lNoPaint := .F.
   ::lFocused := .F.

   IF ! Empty( ::bLostFocus )
      Eval( ::bLostFocus, hCtlFocus, Self )
   ENDIF

   IF ::nLen > 0 .AND. ! EmptyAlias( ::cAlias ) .AND. ! ::lIconView

      IF ::lIsDbf .AND. ( ::cAlias )->( RecNo() ) != ::nLastPos

         IF ::bTagOrder != NIL .AND. ::uLastTag != NIL
            uTag := ( ::cAlias )->( Eval( ::bTagOrder ) )
            ( ::cAlias )->( Eval( ::bTagOrder, ::uLastTag ) )
         ENDIF

         nRecNo := ( ::cAlias )->( RecNo() )
         ( ::cAlias )->( dbGoto( ::nLastPos ) )

      ENDIF

      IF ::lPainted
         ::DrawSelect()
      ENDIF

      IF nRecNo != NIL

         IF uTag != NIL
            ( ::cAlias )->( Eval( ::bTagOrder, uTag ) )
         ENDIF

         ( ::cAlias )->( dbGoto( nRecNo ) )
      ENDIF
   ENDIF

   ::lHasFocus := .F.

RETURN 0

* ============================================================================
* METHOD TSBrowse:MButtonDown() Version 9.0 Nov/30/2009
* ============================================================================

METHOD MButtonDown( nRow, nCol, nKeyFlags ) CLASS TSBrowse

   IF ::bMButtonDown != NIL
      Eval( ::bMButtonDown, nRow, nCol, nKeyFlags )
   ENDIF

RETURN 0

* ============================================================================
* METHOD TSBrowse:MouseMove() Version 9.0 Nov/30/2009
* ============================================================================

METHOD MouseMove( nRowPix, nColPix, nKeyFlags ) CLASS TSBrowse

   LOCAL nI, nIcon, lMChange, nFirst, nLast, nDestCol, ;
      cMsg := ::cMsg, ;
      nColPixPos := 0, ;
      lFrozen := .F., ;
      nColumn := Max( 1, ::nAtColActual( nColPix ) ), ;
      nRowLine := ::GetTxtRow( nRowPix ), ;
      lHeader := ( nRowLine == 0 .OR. nRowLine == -2 ), ;
      cToolTip

   DEFAULT ::lMouseDown := .F., ;
      ::lNoMoveCols := .F., ;
      ::lDontChange := .F.

   IF EmptyAlias( ::cAlias ) .OR. Empty( ::aColumns )
      RETURN 0
   ENDIF

   IF ::lIconView

      IF ( nIcon := ::nAtIcon( nRowPix, nColPix ) ) != 0

         IF ::nIconPos != 0 .AND. ::nIconPos != nIcon
            ::DrawIcon( ::nIconPos )
         ENDIF

         ::nIconPos := nIcon
         ::DrawIcon( nIcon, .T. )
         CursorHand()
         RETURN 0
      ENDIF
   ENDIF

   IF ::nFreeze > 0

      FOR nI := 1 TO ::nFreeze
         nColPixPos += ::GetColSizes()[ nI ]
      NEXT

      IF nColPix < nColPixPos
         lFrozen := .T.
      ENDIF
   ENDIF

   IF nColumn <= ::nColCount()

      IF lHeader .AND. ! Empty( ::aColumns ) .AND. ! Empty( ::aColumns[ nColumn ]:cToolTip )
         cToolTip := ::aColumns[ nColumn ]:cToolTip // column's header tooltip
      ELSE
         cToolTip := ::cToolTip // grid's tooltip
      ENDIF

      hToolTip := GetFormToolTipHandle( ::cParentWnd )

      IF ( ::nToolTip != nColumn .OR. nRowLine != ::nToolTipRow ) .AND. ;
            IsWindowHandle( ::hWnd ) .AND. IsWindowHandle( hToolTip )

         IF ValType( ctooltip ) == "B"
            cToolTip := Eval( cToolTip, Self, nColumn, nRowLine )
         ENDIF

         SetToolTip( ::hWnd, cToolTip, hToolTip )
         SysRefresh()

         ::nToolTipRow := nRowLine
      ENDIF

      ::nToolTip := nColumn
   ENDIF

   IF ! ::lGrasp .AND. ( lFrozen .OR. ! lHeader .OR. ! ::lMChange )
      // don't allow MouseMove to drag/resize columns
      // unless in header row and not in frozen zone
      IF ::oCursor != NIL
         SetResCursor( ::oCursor:hCursor )
      ELSE
         CursorArrow()
      ENDIF

      IF ::lCaptured
         IF ::lLineDrag
            ::VertLine()
            ::lLineDrag := .F.
         ENDIF

         ReleaseCapture()
         ::lColDrag := ::lCaptured := ::lMouseDown := .F.
      ELSEIF ::lDontChange
         CursorStop()
         RETURN 0
      ENDIF

      lMChange := ::lMChange // save it for restore
      ::lMChange := .F.

      IF ::lCellBrw .AND. ! Empty( ::aColumns[ nColumn ]:cMsg )
         ::cMsg := ::aColumns[ nColumn ]:cMsg
      ELSE
         ::cMsg := cMsg
      ENDIF

      ::cMsg := iif( ValType( ::cMsg ) == "B", Eval( ::cMsg, Self, Max( 1, ::nAtCol( nColPix ) ) ), ::cMsg )
      ::Super:MouseMove( nRowPix, nColPix, nKeyFlags )
      ::lMChange := lMChange
      ::cMsg := cMsg
      RETURN 0
   ENDIF

   IF ::lMChange .AND. ! ::lNoMoveCols .AND. ! ::lDontChange
      IF lHeader
         IF ! Empty( ::aSuperHead ) .AND. ! ::lLineDrag

            nFirst := 0
            nLast := 0
            AEval( ::aSuperHead, {| aSup, nCol | nFirst := iif( ::nDragCol >= aSup[ 1 ] .AND. ::nDragCol <= aSup[ 2 ], nCol, nFirst ), ;
               nLast := Max( nLast, aSup[ 2 ] ) } )

            nDestCol := ::nAtCol( nColPix )
            IF nLast < nDestCol
               nLast := nFirst + 1
            ELSE
               AEval( ::aSuperHead, {| aSup, nCol | nlast := iif( nDestCol >= aSup[ 1 ] .AND. nDestCol <= aSup[ 2 ], nCol, nlast ) } )
            ENDIF
            IF nLast != nFirst
               ::lGrasp := .F.
               CursorHand()
               ::lColDrag := ::lCaptured := ::lMouseDown := .F.
            ENDIF
         ENDIF
         IF ::lGrasp // avoid dragging between header & rows
            ::lGrasp := .F.
            CursorArrow() // restore default cursor
         ENDIF

         IF ::lColDrag
            CursorSize()
         ELSE
            IF ::lLineDrag
               ::VertLine( nColPix )
               CursorWE()
            ELSE
               IF AScan( ::GetColSizes(), {| nColumn | nColPixPos += nColumn, ;
                     nColPix >= nColPixPos - 2 .AND. nColPix <= nColPixPos + 2 }, ::nColPos ) != 0
                  CursorWE()
               ELSE
                  CursorHand()
               ENDIF
            ENDIF
         ENDIF
      ELSEIF ::lGrasp
         ::lCaptured := ::lColDrag := .F. // to avoid collision with header/column dragging
         ::lMouseDown := .T. // has to be down until dragging finishes
         ::Super:MouseMove( nRowPix, nColPix, nKeyFlags )
      ELSE
         IF ::oCursor != NIL
            SetResCursor( ::oCursor:hCursor )
         ELSE
            CursorArrow()
         ENDIF
      ENDIF
   ELSE
      IF ::lDontChange
         CursorStop()
      ELSE
         CursorArrow()
      ENDIF
   ENDIF

RETURN 0

* ============================================================================
* METHOD TSBrowse:MouseWheel() Version 9.0 Nov/30/2009
* ============================================================================

METHOD MouseWheel( nKeys, nDelta, nXPos, nYPos ) CLASS TSBrowse

   LOCAL nWParam, ;
      aCoors := { 0, 0, 0, 0 }

   HB_SYMBOL_UNUSED( nKeys )

   GetWindowRect( ::hWnd, aCoors )

   IF ::nWheelLines == NIL
      ::nWheelLines := GetWheelScrollLines()
   ENDIF

   IF nYPos >= aCoors[ 2 ] .AND. nXPos >= aCoors[ 1 ] .AND. ;
         nYPos <= ( aCoors[ 4 ] ) .AND. nXPos <= ( aCoors[ 3 ] )

      IF ( nDelta ) > 0

         IF ! Empty( ::nWheelLines )
            nWParam := SB_LINEUP
            nDelta := ::nWheelLines * nDelta
         ELSE
            nWParam := SB_PAGEUP
         ENDIF

      ELSE

         IF ! Empty( ::nWheelLines )
            nWParam := SB_LINEDOWN
            nDelta := ::nWheelLines * Abs( nDelta )
         ELSE
            nWParam := SB_PAGEDOWN
            nDelta := Abs( nDelta )
         ENDIF

      ENDIF

      WHILE nDelta > 1
         ::VScroll( nWParam, 0 )
         nDelta--
      ENDDO
   ENDIF

RETURN ::VScroll( nWParam, 0 )

* ============================================================================
* METHOD TSBrowse:MoveColumn() Version 9.0 Nov/30/2009
* ============================================================================

METHOD MoveColumn( nColPos, nNewPos ) CLASS TSBrowse

   LOCAL oCol, cOrder, ;
      nMaxCol := Len( ::aColumns ), ;
      nOrder := ::nColOrder, ;
      lCurOrder := ( ::nColOrder == nColPos )

   LOCAL lSetOrder := ;
      ( nOrder != nColPos .AND. nOrder != nNewPos .AND. ::nColOrder == nNewPos )

   IF HB_ISCHAR( nColPos )
      nColPos := ::nColumn( nColPos, .T. )
   ENDIF

   IF ! Empty( nColPos ) .AND. ! Empty( nNewPos ) .AND. ;
         nColPos > ::nFreeze .AND. nNewPos > ::nFreeze .AND. ;
         nColPos <= nMaxCol .AND. nNewPos <= nMaxCol

      oCol := ::aColumns[ nColPos ]
      cOrder := oCol:cOrder
      oCol:cOrder := NIL // avoid ::InsColumn() from seting order...

      ::DelColumn( nColPos )
      ::InsColumn( nNewPos, oCol )
      ::aColumns[ nNewPos ]:cOrder := cOrder // ...restore ::cOrder (if any)

      IF lSetOrder
         ::SetOrder( nNewPos - 1 )
      ELSEIF lCurOrder
         ::SetOrder( nNewPos )
      ELSEIF nOrder != 0
         // check if current order is in between moving columns
         IF nOrder >= nColPos .AND. nOrder <= nNewPos // left to right movement
            nOrder--
         ELSEIF nOrder <= nColPos .AND. nOrder >= nNewPos // right to left movement
            nOrder++
         ENDIF
         ::SetOrder( nOrder )
      ENDIF
   ENDIF

RETURN Self

* ============================================================================
* METHOD TSBrowse:PageDown() Version 9.0 Nov/30/2009
* ============================================================================

METHOD PageDown( nLines ) CLASS TSBrowse

   LOCAL nSkipped, nI, ;
      lPageMode := ::lPageMode, ;
      nTotLines := ::nRowCount()

   DEFAULT nLines := nTotLines

   ::lAppendMode := .F.
   ::ResetSeek()

   IF ::nLen == 0
      ::nLen := iif( ::lIsDbf, ( ::cAlias )->( Eval( ::bLogicLen ) ), Eval( ::bLogicLen ) )
   ENDIF

   IF ::nLen < 1
      RETURN NIL
   ENDIF

   IF ! ::lHitBottom

      ::nPrevRec := ::nAtPos

      IF lPageMode .AND. ::nRowPos < nLines
         nSkipped := ::Skip( nLines - ::nRowPos )
         lPageMode := .F.
      ELSE
         nSkipped = ::Skip( ( nLines * 2 ) - ::nRowPos )
      ENDIF

      IF nSkipped != 0
         ::lHitTop = .F.
      ENDIF

      DO CASE

      CASE nSkipped == 0
         ::lHitBottom := .T.
         RETURN NIL

      CASE nSkipped < nLines .AND. ! lPageMode

         nI := ::nAtPos

         IF ::lIsDbf
            ( ::cAlias )->( dbGoto( ::nPrevRec ) )
         ELSE
            ::nAt := ::nPrevRec
         ENDIF

         ::DrawLine()

         IF ::lIsDbf
            ( ::cAlias )->( dbGoto( nI ) )
         ELSE
            ::nAt := nI
         ENDIF

         IF nLines - ::nRowPos < nSkipped

            nKeyPressed := NIL
            ::Skip( -( nLines ) )

            FOR nI = 1 To ( nLines - 1 )
               ::Skip( 1 )
               ::DrawLine( nI )
            NEXT

            ::Skip( 1 )
         ENDIF

         ::nRowPos = Min( ::nRowPos + nSkipped, nTotLines )

      CASE nSkipped < nLines .AND. lPageMode
         ::Refresh( .T. )

      OTHERWISE

         FOR nI = nLines TO 1 STEP -1
            ::Skip( -1 )
         NEXT

         ::Skip( ::nRowPos )
         ::lRepaint := .T. // JP 1.31
      ENDCASE

      IF nKeyPressed == NIL

         ::Refresh( ::nLen < nTotLines,, .F. ) // Haz 09/10/2020

         IF ::bChange != NIL
            Eval( ::bChange, Self, VK_NEXT )
         ENDIF

      ELSEIF nSkipped >= nLines
         ::DrawSelect()

      ELSE

         nKeyPressed := NIL
         ::DrawSelect()

         IF ::bChange != NIL
            Eval( ::bChange, Self, VK_NEXT ) // Haz 13/04/2017
         ENDIF

      ENDIF

      IF ::oVScroll != NIL
         IF ! ::lHitBottom
            ::oVScroll:SetPos( ::RelPos( ::nLogicPos() ) )
         ELSE
            ::oVScroll:GoBottom()
         ENDIF
      ENDIF

      IF ::lRepaint .AND. ::nRowPos == nTotLines
         IF ::bChange != NIL
            Eval( ::bChange, Self, VK_NEXT ) // GF 15/01/2009
         ENDIF
      ENDIF

   ENDIF

RETURN NIL

* ============================================================================
* METHOD TSBrowse:PageUp() Version 9.0 Nov/30/2009
* ============================================================================

METHOD PageUp( nLines ) CLASS TSBrowse

   LOCAL nSkipped, nRecNo

   DEFAULT nLines := ::nRowCount()

   ::lHitBottom := .F.
   ::lAppendMode := .F.
   ::ResetSeek()

   IF ::lPageMode .AND. ::nRowPos > 1

      ::DrawLine()
      ::nRowPos := 1
      ::Refresh( .F. ,, .F. )

      IF ::oVScroll != NIL
         ::oVScroll:SetPos( ::RelPos( ::nLogicPos() ) )
      ENDIF

      IF ::bChange != NIL
         Eval( ::bChange, Self, VK_PRIOR )
      ENDIF

      RETURN Self

   ENDIF

   ::nPrevRec := ::nAtPos
   nSkipped := ::Skip( -nLines )

   IF ::nLen == 0
      ::nLen := iif( ::lIsDbf, ( ::cAlias )->( Eval( ::bLogicLen ) ), ;
         Eval( ::bLogicLen ) )
   ENDIF

   IF ::nLen < 1
      RETURN NIL
   ENDIF

   IF ! ::lHitTop

      IF nSkipped == 0
         ::lHitTop := .T.
      ELSE

         IF -nSkipped < nLines .OR. ::nAt == 1 // 14.07.2015

            nRecNo := ::nAtPos

            IF ::lIsDbf
               ( ::cAlias )->( dbGoto( ::nPrevRec ) )
            ELSE
               ::nAt := ::nPrevRec
            ENDIF

            ::DrawLine()
            ::nRowPos := 1
            ::Refresh( .F. ) // GF 14/01/2009

            IF ::lIsDbf
               ( ::cAlias )->( dbGoto( nRecNo ) )
            ELSE
               ::nAt := nRecNo
            ENDIF

            IF ::oVScroll != NIL
               ::oVScroll:SetPos( 1 )
            ENDIF

         ELSE

            IF ::oVScroll != NIL
               ::oVScroll:SetPos( ::RelPos( ::nLogicPos() ) )
            ENDIF

         ENDIF

         IF nKeyPressed == NIL

            ::Refresh( .F. )

            IF ::bChange != NIL
               Eval( ::bChange, Self, VK_PRIOR )
            ENDIF

         ELSE

            ::DrawSelect()

            IF -nSkipped < nLines
               nKeyPressed := NIL

               IF ::bChange != NIL
                  Eval( ::bChange, Self, VK_PRIOR ) // GF 15/01/2009
               ENDIF

            ENDIF

         ENDIF

      ENDIF

   ELSE

      IF ::oVScroll != NIL
         ::oVScroll:GoTop()
      ENDIF

   ENDIF

RETURN NIL

* ============================================================================
* METHOD TSBrowse:Paint() Version 9.0 Nov/30/2009
* ============================================================================

METHOD Paint() CLASS TSBrowse

   LOCAL lAppendMode, nRecNo, uTag, oCol, ;
      nColPos := ::nColPos, ;
      nI := 1, ;
      nLines, ;
      nSkipped := 1

   DEFAULT ::lPostEdit := .F., ;
      ::lFirstPaint := .T., ;
      ::lNoPaint := .F., ;
      ::lInitGoTop := .T., ;
      ::nFreeze := 0

   IF ! ::lRepaint
      RETURN 0
   ENDIF

   IF ::lEditing .AND. ! ::lPostEdit .AND. ::nLen > 0
      RETURN 0
   ELSEIF ::lEditing .OR. ::lNoPaint
      IF ::lDrawHeaders
         ::DrawHeaders()
      ENDIF

      ::DrawSelect()
      RETURN 0
   ENDIF

   IF ::lFirstPaint
      ::Default()
   ENDIF

   ::nRowPos := iif( ::lDrawHeaders, Max( 1, ::nRowPos ), ::nRowPos )

   IF ::lIconView
      ::DrawIcons()
      RETURN 0
   ENDIF

   IF Empty( ::aColumns )
      RETURN NIL
   ENDIF

   IF ! ::lPainted
      SetHeights( Self )

      IF ::lSelector
         DEFAULT ::nSelWidth := Max( nBmpWidth( ::hBmpCursor ), Min( ::nHeightHead, 25 ) )

         oCol := ColClone( ::aColumns[ 1 ], Self )
         oCol:bData := {|| "" }
         oCol:cHeading := ""
         oCol:nWidth := ::nSelWidth
         oCol:lNoHilite := .T.
         oCol:lFixLite := Empty( ::hBmpCursor )
         oCol:nClrBack := oCol:nClrHeadBack
         oCol:cName := "SELECTOR"
         ::InsColumn( 1, oCol )
         ::nFreeze++
         ::lLockFreeze := .T.
         ::HiliteCell( Max( ::nCell, ::nFreeze + 1 ) )

         IF ! Empty( ::nColOrder )
            ::nColOrder++
            ::SetOrder( ::nColOrder )
         ENDIF
      ENDIF

      nLines := Min( ::nLen + iif( ::lAppendMode .AND. ! ::lIsArr, 1, 0 ), ::nRowCount() )

      IF ::nLen <= nLines .AND. ::nAt > ::nRowPos
         ::nRowPos := ::nAt
      ENDIF

      IF ::lInitGoTop
         ::GoTop()
      ENDIF

      IF ! ::lNoHScroll .AND. ::oHScroll != NIL
         ::oHScroll:SetRange( 1, Len( ::aColumns ) )
      ENDIF

      IF ::bInit != NIL
         Eval( ::bInit, Self )
      ENDIF
   ENDIF

   IF ::lDrawHeaders .AND. ::nHeightSuper > 0
      ::nColPos := ::nFreeze + 1
      ::DrawSuper()
      ::nColPos := nColPos
   ENDIF

   IF ::lDrawHeaders
      ::nRowPos := Max( 1, ::nRowPos )
      ::DrawHeaders()
   ENDIF

   IF ::lIsDbf .AND. ::lPainted .AND. ! ::lFocused .AND. Select( ::cAlias ) > 0 .AND. ;
         ( ::cAlias )->( RecNo() ) != ::nLastPos

      IF ::bTagOrder != NIL .AND. ::uLastTag != NIL
         uTag := ( ::cAlias )->( Eval( ::bTagOrder ) )
         ( ::cAlias )->( Eval( ::bTagOrder, ::uLastTag ) )
      ENDIF

      nRecNo := ( ::cAlias )->( RecNo() )
      ( ::cAlias )->( dbGoto( ::nLastPos ) )

   ENDIF

   IF ::lAppendMode .AND. ::lFilterMode
      Eval( ::bGoBottom )
   ENDIF

   ::Skip( 1 - ::nRowPos )

   IF ::lIsArr
      lAppendMode := ::lAppendMode
      ::lAppendMode := .F.
   ENDIF

   ::nLastPainted := 0

   nLines := Min( ::nLen + iif( ::lAppendMode .AND. ! ::lIsArr, 1, 0 ), ::nRowCount() )

   WHILE nI <= nLines .AND. nSkipped == 1

      IF ::nRowPos == nI
         ::DrawSelect()
      ELSE
         ::DrawLine( nI )
      ENDIF

      ::nLastPainted := nI
      nSkipped := ::Skip( 1 )

      IF nSkipped == 1
         nI++
      ENDIF
   ENDDO

   ::nLenPos := nI // JP 1.31

   IF ::lIsArr
      ::lAppendMode := lAppendMode
   ENDIF

   IF ::lAppendMode
      nI := Max( 1, --nI )
   ENDIF

   ::Skip( ::nRowPos - nI )

   IF ::nLen < ::nRowPos
      ::nRowPos := ::nLen
   ENDIF

   IF ::lAppendMode .AND. ::nLen == ::nRowPos .AND. ::nRowPos < nLines
      ::DrawLine( ++::nRowPos )
   ENDIF

   IF ! ::lPainted
      IF ::bChange != NIL .AND. ! ::lPhantArrRow
         Eval( ::bChange, Self, 0 )
      ENDIF

      IF ::lIsDbf .AND. ! ::lNoResetPos

         IF ::bTagOrder != NIL
            ::uLastTag := ( ::cAlias )->( Eval( ::btagOrder ) )
         ENDIF

         ::nLastPos := ( ::cAlias )->( RecNo() )
      ENDIF

      IF ::lCanAppend .AND. ::lHasFocus .AND. ! ::lAppendMode .AND. ::nLen == 0
         ::lHitBottom := .T.
         ::PostMsg( WM_KEYDOWN, VK_DOWN, 0 )
      ELSE
         ::lNoPaint := .T.
      ENDIF
   ENDIF

   ::lPainted := .T.

   IF nRecNo != NIL

      IF uTag != NIL
         ( ::cAlias )->( Eval( ::bTagOrder, uTag ) )
      ENDIF

      ( ::cAlias )->( dbGoto( nRecNo ) )
   ENDIF

RETURN 0

* ============================================================================
* METHOD TSBrowse:PanEnd() Version 9.0 Nov/30/2009
* ============================================================================

METHOD PanEnd() CLASS TSBrowse

   LOCAL nI, nTxtWid, ;
      nWidth := ::nWidth() - iif( ::oVScroll != NIL, GetSysMetrics( 2 ), 0 ), ;
      nWide := 0, ;
      nCols := Len( ::aColumns )

   ::nOldCell := ::nCell

   IF ::lIsTxt
      nTxtWid := Max( 1, GetTextWidth( 0, "B", iif( ::hFont != NIL, ::hFont, 0 ) ) )
      ::nAt := ::oTxtFile:nMaxLineLength - Int( nWidth / nTxtWid )
      ::Refresh( .F. ,, .F. )

      IF ! ::lNoHScroll .AND. ::oHScroll != NIL
         ::oHScroll:setPos( ::nAt )
      ENDIF

      RETURN Self
   ENDIF

   ::nColPos := nCols
   ::nCell := nCols
   nWide := ::aColSizes[ nCols ]

   FOR nI := nCols - 1 TO 1 STEP -1
      IF nWide + ::aColSizes[ nI ] <= nWidth
         nWide += ::aColSizes[ nI ]
         ::nColPos--
      ELSE
         EXIT
      ENDIF
   NEXT

   IF ::lCellBrw

      IF ::aColumns[ ::nCell ]:bGotFocus != NIL .AND. ::nOldCell != ::nCell
         Eval( ::aColumns[ ::nCell ]:bGotFocus, ::nOldCell, ::nCell, Self )
      ENDIF

      IF ::aColumns[ ::nOldCell ]:bLostFocus != NIL .AND. ::nOldCell != ::nCell
         Eval( ::aColumns[ ::nOldCell ]:bLostFocus, ::nCell, ::nOldCell, Self )
      ENDIF

      ::nOldCell := ::nCell
   ENDIF

   ::Refresh( .F. )

   IF ! ::lNoHScroll .AND. ::oHScroll != NIL
      IF ::oHScroll:nMax != nCols
         ::oHScroll:SetRange( 1, nCols )
      ENDIF
      ::oHScroll:GoBottom()
   ENDIF

   ::HiliteCell( nCols )

RETURN Self

* ============================================================================
* METHOD TSBrowse:PanHome() Version 9.0 Nov/30/2009
* ============================================================================

METHOD PanHome() CLASS TSBrowse

   LOCAL nColChk
   LOCAL nEle

   ::nOldCell := ::nCell

   IF ::lIsTxt

      ::nAt := 1
      ::Refresh( .F. )

      IF ::oHScroll != NIL
         ::oHScroll:setPos( ::nAt )
      ENDIF

      RETURN Self
   ENDIF

   FOR nEle := 1 TO Len( ::aColumns )
      IF ! ::aColumns[ nEle ]:lNoHilite
         nColChk := nEle
         EXIT
      ENDIF
   NEXT

   ::nColPos := 1
   ::nCell := nColChk

   IF ::lCellBrw

      IF ::aColumns[ ::nCell ]:bGotFocus != NIL .AND. ::nOldCell != ::nCell
         Eval( ::aColumns[ ::nCell ]:bGotFocus, ::nOldCell, ::nCell, Self )
      ENDIF

      IF ::aColumns[ ::nOldCell ]:bLostFocus != NIL .AND. ::nOldCell != ::nCell
         Eval( ::aColumns[ ::nOldCell ]:bLostFocus, ::nCell, ::nOldCell, Self )
      ENDIF

      ::nOldCell := ::nCell
   ENDIF

   IF ! ::lEditing
      ::Refresh( .F. )
   ELSE
      ::Paint()
   ENDIF

   IF ::oHScroll != NIL
      ::oHScroll:SetPos( ::nCell )
   ENDIF

   ::HiliteCell( ::nCell )

RETURN Self

* ============================================================================
* METHOD TSBrowse:PanLeft() Version 9.0 Nov/30/2009
* ============================================================================

METHOD PanLeft() CLASS TSBrowse

   LOCAL nI, ;
      nWidth := ::nWidth() - iif( ::oVScroll != NIL, GetSysMetrics( 2 ), 0 ), ;
      nWide := 0, ;
      nCols := Len( ::aColumns ), ;
      nPos := ::nColPos, ;
      nFirst := iif( ::lLockFreeze .AND. ::nFreeze > 0, ::nFreeze + 1, 1 )

   ::nOldCell := ::nCell

   IF ::lIsTxt

      IF ::nAt > 11
         ::nAt := ::nAt - 10
      ELSE
         ::nAt := 1
      ENDIF
      IF ::oHScroll != NIL
         ::oHScroll:SetPos( ::nAt )
      ENDIF
      ::Refresh( .F. )
      RETURN Self
   ENDIF

   IF ::nFreeze >= nCols
      RETURN Self
   ENDIF

   AEval( ::aColSizes, {| nSize | nWide += nSize } )

   IF nWide <= nWidth // browse fits all inside
      ::nCell := nFirst // window or dialog
      ::nColPos := 1
   ELSE
      nWide := ::aColSizes[ nPos ]
      FOR nI := nPos - 1 TO nFirst STEP -1
         IF nWide <= nWidth
            nWide += ::aColSizes[ nI ]
            ::nCell := nI
         ELSE
            EXIT
         ENDIF
      NEXT
      ::nColPos := ::nCell // current column becomes first in offset
   ENDIF

   IF ! ::lCellBrw .AND. ! ::lLockFreeze .AND. ;  // for frozen columns
      ::nFreeze > 0 .AND. ::nCell - 1 == 1
      ::nCell := ::nColPos := ::nFreeze
   ENDIF

   IF ::nCell != ::nOldCell
      ::Refresh( .F. ,, .F. )
   ENDIF

   IF ::lCellBrw

      IF ::aColumns[ ::nCell ]:bGotFocus != NIL .AND. ::nOldCell != ::nCell
         Eval( ::aColumns[ ::nCell ]:bGotFocus, ::nOldCell, ::nCell, Self )
      ENDIF

      IF ::aColumns[ ::nOldCell ]:bLostFocus != NIL .AND. ::nOldCell != ::nCell
         Eval( ::aColumns[ ::nOldCell ]:bLostFocus, ::nCell, ::nOldCell, Self )
      ENDIF

      ::nOldCell := ::nCell

   ENDIF

   IF ::oHScroll != NIL
      ::oHScroll:SetPos( ::nCell )
   ENDIF

   IF ::lCellBrw
      ::HiliteCell( ::nCell )
   ENDIF

RETURN Self

* ============================================================================
* METHOD TSBrowse:PanRight() Version 9.0 Nov/30/2009
* ============================================================================

METHOD PanRight() CLASS TSBrowse

   LOCAL nTxtWid, nI, ;
      nWidth := ::nWidth() - iif( ::oVScroll != NIL, GetSysMetrics( 2 ), 0 ), ;
      nWide := 0, ;
      nCols := Len( ::aColumns ), ;
      nPos := ::nColPos

   ::nOldCell := ::nCell

   IF ::lIsTxt

      nTxtWid := Max( 1, GetTextWidth( 0, "B", iif( ::hFont != NIL, ::hFont, 0 ) ) )

      IF ::nAt < ::oTxtFile:nMaxLineLength - 21 - Int( nWidth / nTxtWid )
         ::nAt := ::nAt + 20
      ELSE
         ::nAt := ::oTxtFile:nMaxLineLength - Int( nWidth / nTxtWid )
      ENDIF

      IF ::oHScroll != NIL
         ::oHScroll:SetPos( ::nAt )
      ENDIF

      ::Refresh( .F. )
      RETURN Self
   ENDIF

   IF ::nFreeze >= nCols
      RETURN Self
   ENDIF

   AEval( ::aColSizes, {| nSize | nWide += nSize }, nPos )

   IF ::nFreeze > 0
      AEval( ::aColSizes, {| nSize | nWide += nSize }, 1, ::nFreeze )
   ENDIF

   IF nWide <= nWidth // we're in last columns (including the current one),
      ::nCell := nCols // so just ::nCell changes, not ::nColPos
   ELSE
      nWide := 0
      AEval( ::aColSizes, {| nSize | nWide += nSize }, nPos + 1 )

      IF ::nFreeze > 0
         AEval( ::aColSizes, {| nSize | nWide += nSize }, 1, ::nFreeze )
      ENDIF

      IF nWide <= nWidth .AND. nPos < nCols // the remaining columns are added
         ::nCell := nPos + 1 // the last in the browse
         nPos := nCols // so as to avoid For..Next
      ENDIF

      nWide := ::aColSizes[ nPos ]

      FOR nI := nPos + 1 TO nCols
         IF ( nWide + ::aColSizes[ nI ] <= nWidth ) .OR. ;
               ( nI - nPos < 2 .AND. nWide + ::aColSizes[ nI ] > nWidth ) // two consecutive very wide columns
            nWide += ::aColSizes[ nI ]
            ::nCell := nI
         ELSE
            EXIT
         ENDIF
      NEXT
      ::nColPos := ::nCell // last column becomes first in offset
   ENDIF

   IF ! ::lCellBrw .AND. ;                       // for frozen columns
         iif( ::nFreeze > 0, ::IsColVis2( nCols ), ::IsColVisible( nCols ) )

      ::nCell := nCols

   ENDIF

   IF ::nCell != ::nOldCell
      ::Refresh( .F. ,, .F. )
   ENDIF

   IF ::lCellBrw

      IF ::aColumns[ ::nCell ]:bGotFocus != NIL .AND. ::nOldCell != ::nCell
         Eval( ::aColumns[ ::nCell ]:bGotFocus, ::nOldCell, ::nCell, Self )
      ENDIF

      IF ::aColumns[ ::nOldCell ]:bLostFocus != NIL .AND. ::nOldCell != ::nCell
         Eval( ::aColumns[ ::nOldCell ]:bLostFocus, ::nCell, ::nOldCell, Self )
      ENDIF

      ::nOldCell := ::nCell

   ENDIF

   IF ::oHScroll != NIL
      ::oHScroll:SetPos( ::nCell )
   ENDIF

   ::HiliteCell( ::nCell )

RETURN Self

* ============================================================================
* METHOD TSBrowse:Proper() Version 9.0 Nov/30/2009
* ============================================================================

METHOD Proper( cString ) CLASS TSBrowse

   LOCAL nPos, cChr, cTxt, cAnt, ;
      nLen := Len( cString )

   cString := Lower( Trim( cString ) ) + " "

   nPos := 1
   cAnt := "."
   cTxt := ""

   WHILE nPos <= Len( cString )
      cChr := SubStr( cString, nPos, 1 )
      cTxt += iif( cAnt $ '".,-/ (', Upper( cChr ), iif( Asc( cChr ) == 209, Chr( 241 ), cChr ) )
      cAnt := cChr
      nPos++
   ENDDO

   cTxt := StrTran( cTxt, ::aMsg[ 51 ], Lower( ::aMsg[ 51 ] ) )
   cTxt := StrTran( cTxt, ::aMsg[ 52 ], Lower( ::aMsg[ 52 ] ) )
   cTxt := StrTran( cTxt, ::aMsg[ 53 ], Lower( ::aMsg[ 53 ] ) )
   cTxt := StrTran( cTxt, ::aMsg[ 54 ], Lower( ::aMsg[ 54 ] ) )
   cTxt := StrTran( cTxt, ::aMsg[ 55 ], Lower( ::aMsg[ 55 ] ) )

RETURN PadR( cTxt, nLen )

* ============================================================================
* METHOD TSBrowse:PostEdit() Version 9.0 Nov/30/2009
* ============================================================================

METHOD PostEdit( uTemp, nCol, bValid ) CLASS TSBrowse

   LOCAL aMoveCell, bRecLock, bAddRec, cAlias, uRet, xNewEditValue, lLockArea, cArea, ;
      nLastKey := ::oWnd:nLastKey, ;
      lAppend := ::lAppendMode

   cAlias := iif( ::lIsDbf .AND. ::aColumns[ nCol ]:cAlias != NIL, ::aColumns[ nCol ]:cAlias, ::cAlias )

   aMoveCell := { {|| ::GoRight() }, {|| ::GoDown() }, {|| ::GoLeft() }, {|| ::GoUp() }, {|| ::GoNext() } }

   bRecLock := iif( ! Empty( ::bRecLock ), ::bRecLock, {|| ( cAlias )->( RLock() ) } )

   bAddRec := iif( ! Empty( ::bAddRec ), ::bAddRec, {|| ( cAlias )->( dbAppend() ), ! NetErr() } )

   cArea := ::aColumns[ nCol ]:cArea

   lLockArea := ( ::lRecLockArea .AND. ! Empty( cArea ) .AND. Select( cArea ) > 0 )

   IF bValid != NIL

      uRet := Eval( bValid, uTemp, Self )

      IF ValType( uRet ) == "B"
         uRet := Eval( uRet, uTemp, Self )
      ENDIF

      IF ! uRet

         Tone( 500, 1 )
         ::lEditing := ::lPostEdit := ::lNoPaint := .F.

         IF lAppend
            ::lAppendMode := .F.
            ::lHitBottom := .F.
            ::GoBottom()
            ::HiliteCell( nCol )
            lNoAppend := .T.
            ::lCanAppend := .F.
         ELSE
            ::DrawSelect()
         ENDIF

         RETURN NIL

      ENDIF

   ENDIF

   IF ::lIsDbf

      IF Eval( iif( ! ::lAppendMode, bRecLock, bAddRec ), uTemp )

         IF lLockArea
            IF ( cArea )->( RLock() )
               ::bDataEval( ::aColumns[ nCol ], uTemp, nCol )
            ENDIF
         ELSE
            ::bDataEval( ::aColumns[ nCol ], uTemp, nCol )
         ENDIF

         SysRefresh()

         IF lAppend

            IF ! Empty( ::aDefault )
               ASize( ::aDefault, Len( ::aColumns ) )
               AEval( ::aDefault, {| e, n | iif( e != NIL .AND. n != nCol, iif( ValType( e ) == "B", ;
                  ::bDataEval( ::aColumns[ n ], Eval( e, Self ), n ), ;
                  ::bDataEval( ::aColumns[ n ], e, n ) ), Nil ) } )
               ::DrawLine()
            ENDIF

#ifdef _TSBFILTER7_
            IF ::lFilterMode .AND. ::aColumns[ nCol ]:lIndexCol

               IF &( ( cAlias )->( IndexKey() ) ) >= ::uValue1 .AND. ;
                     &( ( cAlias )->( IndexKey() ) ) <= ::uValue2

                  ::nLen := ( cAlias )->( Eval( ::bLogicLen() ) )
                  ::nRowPos := ::nAt := Min( ::nRowCount, ::nLen )
               ELSE
                  ::lChanged := .F.
                  ( cAlias )->( dbGoto( ::nPrevRec ) )
                  ::Refresh( .F. )
               ENDIF

            ELSE
               ::nLen++
            ENDIF
#else
            ::nLen++
#endif
            ::lAppendMode := .F.

            IF ::nRowPos == 0 .AND. ::lDrawHeaders
               ::nRowPos := ::nAt := 1
            ENDIF

            IF ::oVScroll != NIL
               ::oVScroll:SetRange( 1, Max( 1, ::nLen ) )
               ::oVScroll:SetPos( ::RelPos( ::nLogicPos() ) )
            ENDIF

            ::DrawLine( ::nRowPos )
         ENDIF

         IF ::aColumns[ nCol ]:bPostEdit != NIL
            Eval( ::aColumns[ nCol ]:bPostEdit, uTemp, Self, lAppend )
         ENDIF

         ::lEditing := .F.
         ::lPostEdit := .F.
         ::lUpdated := .T.
         IF lLockArea
            ( cArea )->( dbUnlock() )
            ( cArea )->( dbSkip( 0 ) )
         ELSEIF !( "SQL" $ ::cDriver )
            ( cAlias )->( dbUnlock() )
         ENDIF

         IF lAppend
            IF ::bChange != NIL
               Eval( ::bChange, Self, ::oWnd:nLastKey )
            ENDIF

            lAppend := .F.
            AEval( ::aColumns, {| oC | lAppend := iif( oC:lIndexCol, .T., lAppend ) } )
         ENDIF

         IF ( ::aColumns[ nCol ]:lIndexCol .AND. ::lChanged ) .OR. lAppend
            ::lNoPaint := .F.

#ifdef _TSBFILTER7_
            IF ::lFilterMode
               IF &( ( cAlias )->( IndexKey() ) ) >= ::uValue1 .AND. ;
                  &( ( cAlias )->( IndexKey() ) ) <= ::uValue2
                  ::UpStable()
               ELSE
                  ::Reset()
               ENDIF
            ELSE
               ::UpStable()
            ENDIF
#else
            ::UpStable()
#endif
         ENDIF

         ( cAlias )->( dbSkip( 0 ) ) // refresh relations just in case that a relation field changes

         xNewEditValue := ::bDataEval( ::aColumns[ nCol ], , nCol )

         IF HB_ISBLOCK( ::bEditLog ) .AND. ::aColumns[ nCol ]:xOldEditValue != xNewEditValue
            Eval( ::bEditLog, ::aColumns[ nCol ]:xOldEditValue, xNewEditValue, Self )
         ENDIF

         ::SetFocus()

         IF nLastKey == VK_UP .AND. ::lPostEditGo
            ::GoUp()
         ELSEIF nLastkey == VK_RIGHT .AND. ::lPostEditGo
            ::GoRight()
         ELSEIF nLastkey == VK_LEFT .AND. ::lPostEditGo
            ::GoLeft()
         ELSEIF nLastkey == VK_DOWN .AND. ::lPostEditGo
            ::GoDown()
            ::Refresh( .F. )
         ELSEIF ::aColumns[ nCol ]:nEditMove >= 1 .AND. ::aColumns[ nCol ]:nEditMove <= 5 // excel-like behavior post-edit movement
            Eval( aMoveCell[ ::aColumns[ nCol ]:nEditMove ] )
            ::DrawSelect()
            IF ! ::lAppendMode
               ::Refresh( .F. )
            ENDIF
         ELSEIF ::aColumns[ nCol ]:nEditMove == 0 .AND. ! ::lAutoEdit
            ::DrawSelect()
         ENDIF

         ::oWnd:nLastKey := NIL

         IF ::lAutoEdit .AND. ! lAppend
            SysRefresh()

            IF ! ::aColumns[ ::nCell ]:lCheckBox
               ::PostMsg( WM_KEYDOWN, VK_RETURN, nMakeLong( 0, 0 ) )
            ENDIF

            RETURN NIL
         ENDIF

      ELSE
         IF ! lAppend .AND. Empty( ::bRecLock )
            MsgStop( ::aMsg[ 3 ], ::aMsg[ 4 ] )
         ELSEIF lAppend .AND. Empty( ::bAddRec )
            MsgStop( ::aMsg[ 5 ], ::aMsg[ 4 ] )
         ENDIF
      ENDIF

   ELSE

      IF lAppend .AND. ::lIsArr
         // when ::aDefValue[1] == Nil, it flags this element
         // as the equivalent of the "record no" for the array
         // this is why ::aDefValue will have one more element
         // than Len( ::aArray )
         ::lAppendMode := .F.
         AAdd( ::aArray, Array( Len( ::aDefValue ) - iif( ::aDefValue[ 1 ] == NIL, 1, 0 ) ) )
         ::nAt := ::nLen := Len( ::aArray )

         IF ::oVScroll != NIL
            ::oVScroll:SetRange( 1, Max( 1, ::nLen ) )
            ::oVScroll:GoBottom()
         ENDIF

         AEval( ATail( ::aArray ), ;
            {| uVal, n | ::aArray[ ::nAt, n ] := ::aDefValue[ n + iif( ::aDefValue[ 1 ] == NIL, 1, 0 ) ], HB_SYMBOL_UNUSED( uVal ) } )

      ENDIF

      ::bDataEval( ::aColumns[ nCol ], uTemp, nCol )
      SysRefresh()

      IF ::aColumns[ nCol ]:bPostEdit != NIL
         Eval( ::aColumns[ nCol ]:bPostEdit, uTemp, Self, lAppend )
      ENDIF

      ::lEditing := .F.
      ::lPostEdit := .F.

      IF lAppend
         IF ! Empty( ::aDefault )
            ASize( ::aDefault, Len( ::aColumns ) )
            AEval( ::aDefault, {| e, n | iif( e != NIL .AND. n != nCol, iif( ValType( e ) == "B", ;
               ::bDataEval( ::aColumns[ n ], Eval( e, Self ), n ), ::bDataEval( ::aColumns[ n ], e, n ) ), Nil ) } )
         ENDIF
         ::DrawLine()
      ENDIF

      IF lAppend .AND. ::nLen <= ::nRowCount()
         ::Refresh( .T.,, .F. )
         ::nRowPos := Min( ::nRowCount(), ::nLen )
      ENDIF

      IF lAppend .AND. ::bChange != NIL
         Eval( ::bChange, Self, ::oWnd:nLastKey )
      ENDIF

      xNewEditValue := ::bDataEval( ::aColumns[ nCol ], , nCol )

      IF HB_ISBLOCK( ::bEditLog ) .AND. ::aColumns[ nCol ]:xOldEditValue != xNewEditValue
         Eval( ::bEditLog, ::aColumns[ nCol ]:xOldEditValue, xNewEditValue, Self )
      ENDIF

      ::SetFocus()

      IF nLastKey == VK_UP .AND. ::lPostEditGo
         ::GoUp()
      ELSEIF nLastkey == VK_RIGHT .AND. ::lPostEditGo
         ::GoRight()
      ELSEIF nLastkey == VK_LEFT .AND. ::lPostEditGo
         ::GoLeft()
      ELSEIF nLastkey == VK_DOWN .AND. ::lPostEditGo
         ::GoDown()
         ::Refresh( .F. )
      ELSEIF ::aColumns[ nCol ]:nEditMove >= 1 .AND. ::aColumns[ nCol ]:nEditMove <= 5 // excel-like behaviour post-edit movement
         Eval( aMoveCell[ ::aColumns[ nCol ]:nEditMove ] )
      ELSEIF ::aColumns[ nCol ]:nEditMove == 0
         ::DrawSelect()
      ENDIF

      ::oWnd:nLastKey := NIL

      IF ::lAutoEdit .AND. ! lAppend
         SysRefresh()
         IF ! ::aColumns[ ::nCell ]:lCheckBox
            ::PostMsg( WM_KEYDOWN, VK_RETURN, nMakeLong( 0, 0 ) )
         ENDIF
      ENDIF

   ENDIF

RETURN NIL

* ============================================================================
* METHOD TSBrowse:RButtonDown() Version 9.0 Nov/30/2009
* ============================================================================

METHOD RButtonDown( nRowPix, nColPix, nFlags ) CLASS TSBrowse

   LOCAL nRow, nCol, nSkipped, bRClicked, lHeader, lFooter, lSpecHd, ;
      uPar1 := nRowPix, ;
      uPar2 := nColPix

   HB_SYMBOL_UNUSED( nFlags )

   IF ! ::lEnabled
      RETURN 0
   ENDIF

   DEFAULT ::lNoPopup := .T., ;
      ::lNoMoveCols := .F.

   ::lNoPaint := .F.

   ::SetFocus()
   ::oWnd:nLastKey := 0

   IF ::nLen < 1
      RETURN 0
   ENDIF

   nRow := ::GetTxtRow( nRowPix )
   nCol := ::nAtColActual( nColPix )
   lHeader := nRow == 0
   lFooter := nRow == -1
   lSpecHd := nRow == -2

   IF nRow > 0

      IF ::lPopupActiv
         _ShowControlContextMenu( ::cControlName, ::cParentWnd, .F. )
      ENDIF

      IF ::lDontChange
         RETURN 0
      ENDIF

      IF nCol <= ::nFreeze .AND. ::lLockFreeze
         RETURN 0
      ENDIF

      ::ResetSeek()
      ::DrawLine()

      nSkipped := ::Skip( nRow - ::nRowPos )
      ::nRowPos += nSkipped
      ::nCell := nCol

      IF ! ::lNoVScroll .AND. ::oVScroll != NIL
         ::oVScroll:SetPos( ::RelPos( ::nLogicPos() ) )
      ENDIF

      IF ! ::lNoHScroll .AND. ::oHScroll != NIL
         ::oHScroll:SetPos( ::nCell )
      ENDIF

      IF nSkipped != 0 .AND. ::bChange != NIL
         Eval( ::bChange, Self, ::oWnd:nLastKey )
      ENDIF

      IF ::lCellBrw

         IF ::aColumns[ ::nCell ]:bGotFocus != NIL .AND. ::nOldCell != ::nCell
            Eval( ::aColumns[ ::nCell ]:bGotFocus, ::nOldCell, ::nCell, Self )
         ENDIF

         IF ::aColumns[ ::nOldCell ]:bLostFocus != NIL .AND. ::nOldCell != ::nCell
            Eval( ::aColumns[ ::nOldCell ]:bLostFocus, ::nCell, ::nOldCell, Self )
         ENDIF

         ::nOldCell := ::nCell
         ::HiliteCell( ::nCell )

      ENDIF

      ::DrawSelect()
      bRClicked := iif( ::aColumns[ nCol ]:bRClicked != NIL, ;
         ::aColumns[ nCol ]:bRClicked, ::bRClicked )

      IF bRClicked != NIL
         Eval( bRClicked, uPar1, uPar2, ::nAt, Self )
      ENDIF
      RETURN 0

   ELSEIF lHeader

      IF ::lPopupActiv
         IF ::lPopupUser
            IF AScan( ::aPopupCol, nCol ) > 0 .OR. ::aPopupCol[ 1 ] == 0
               IF ::nPopupActiv == nCol
                  _ShowControlContextMenu( ::cControlName, ::cParentWnd, .T. )
               ELSE
                  IF ::bUserPopupItem != NIL
                     Eval( ::bUserPopupItem, nCol )
                     ::nPopupActiv := nCol
                  ENDIF
               ENDIF
            ELSE
               _ShowControlContextMenu( ::cControlName, ::cParentWnd, .F. )
            ENDIF
         ELSE
            _ShowControlContextMenu( ::cControlName, ::cParentWnd, .T. )
         ENDIF
      ENDIF

      IF ::aColumns[ nCol ]:bHRClicked != NIL
         Eval( ::aColumns[ nCol ]:bHRClicked, uPar1, uPar2, ::nAt, Self )
      ENDIF

   ELSEIF lSpecHd
      IF ::aColumns[ nCol ]:bSRClicked != NIL
         Eval( ::aColumns[ nCol ]:bSRClicked, uPar1, uPar2, ::nAt, Self )
      ENDIF

   ELSEIF lFooter
      IF ::aColumns[ nCol ]:bFRClicked != NIL
         Eval( ::aColumns[ nCol ]:bFRClicked, uPar1, uPar2, ::nAt, Self )
      ENDIF
   ENDIF

   IF nCol <= ::nFreeze .OR. ::lNoPopup
      RETURN 0
   ENDIF

   IF ::lPopupUser
      IF ::bUserPopupItem != NIL
         IF ! ::lPopupActiv
            IF AScan( ::aPopupCol, nCol ) > 0 .OR. ::aPopupCol[ 1 ] == 0
               _DefineControlContextMenu( ::cControlName, ::cParentWnd )

               Eval( ::bUserPopupItem, nCol )

               END MENU
               ::nPopupActiv := nCol
               ::lPopupActiv := .T.
            ENDIF
         ENDIF
      ENDIF

   ELSEIF ! ::lNoMoveCols

      IF ! ::lPopupActiv
         _DefineControlContextMenu( ::cControlName, ::cParentWnd )
         IF ::lUnDo
            MENUITEM ::aMsg[ 6 ] + Space( 1 ) + ::aClipBoard[ 3 ] ;
               ACTION {|| ::InsColumn( ::aClipBoard[ 2 ], ;
               ::aClipBoard[ 1 ] ), ;
               ::nCell := ::aClipBoard[ 2 ], ::Refresh( .T. ), ;
               ::aClipBoard := NIL, ::lUnDo := .F. } NAME M_UNDO
         ELSE
            MENUITEM ::aMsg[ 6 ] ACTION NIL DISABLED NAME M_UNDO
         ENDIF
         MENUITEM ::aMsg[ 7 ] ;
            ACTION {|| ::aClipBoard := { ColClone( ::aColumns[ nCol ], Self ), nCol, ::aMsg[ 12 ] } } NAME M_COPY

         MENUITEM ::aMsg[ 8 ] ;
            ACTION {|| ::aClipBoard := { ColClone( ::aColumns[ nCol ], Self ), nCol, ::aMsg[ 13 ] }, ;
            ::DelColumn( nCol ), ::Refresh( .T. ), ::lUnDo := .T. } NAME M_CUT
         IF ::aClipBoard != NIL .AND. ::aClipBoard[ 3 ] != ::aMsg[ 12 ]
            MENUITEM ::aMsg[ 9 ] ;
               ACTION {|| ::InsColumn( nCol, ::aClipBoard[ 1 ] ), ::nCell := nCol, ::Refresh( .T. ), ;
               ::aClipBoard := NIL, ::lUnDo := .F. } NAME M_PASTE
         ELSE
            MENUITEM ::aMsg[ 9 ] ACTION NIL DISABLED NAME M_PASTE
         ENDIF
         SEPARATOR

         MENUITEM ::aMsg[ 10 ] ;
            ACTION {|| ::aClipBoard := { ColClone( ::aColumns[ nCol ], Self ), nCol, ::aMsg[ 11 ] }, ;
            ::DelColumn( nCol ), ::Refresh( .T. ), ::lUnDo := .T. } NAME M_DEL

         END MENU
         ::lPopupActiv := .T.
      ELSE
         IF ::lUnDo
            _ModifyMenuItem( "M_UNDO", ::cParentWnd, ::aMsg[ 6 ] + Space( 1 ) + ::aClipBoard[ 3 ], ;
               {|| ::InsColumn( ::aClipBoard[ 2 ], ;
               ::aClipBoard[ 1 ] ), ;
               ::nCell := ::aClipBoard[ 2 ], ::Refresh( .T. ), ;
               ::aClipBoard := NIL, ::lUnDo := .F. }, "M_UNDO", "" )
            _EnableMenuItem( "M_UNDO", ::cParentWnd )
         ELSE
            _ModifyMenuItem( "M_UNDO", ::cParentWnd, ::aMsg[ 6 ], NIL, "M_UNDO", "" )
            _DisableMenuItem( "M_UNDO", ::cParentWnd )
         ENDIF
         _ModifyMenuItem( "M_COPY", ::cParentWnd, ::aMsg[ 7 ], ;
            {|| ::aClipBoard := { ColClone( ::aColumns[ nCol ], Self ), nCol, ::aMsg[ 12 ] } }, "M_COPY", "" )
         _ModifyMenuItem( "M_CUT", ::cParentWnd, ::aMsg[ 8 ], ;
            {|| ::aClipBoard := { ColClone( ::aColumns[ nCol ], Self ), nCol, ::aMsg[ 13 ] }, ;
            ::DelColumn( nCol ), ::Refresh( .T. ), ::lUnDo := .T. }, "M_CUT", "" )
         IF ::aClipBoard != NIL .AND. ::aClipBoard[ 3 ] != ::aMsg[ 12 ]
            _ModifyMenuItem( "M_PASTE", ::cParentWnd, ::aMsg[ 9 ], ;
               {|| ::InsColumn( nCol, ::aClipBoard[ 1 ] ), ::nCell := nCol, ::Refresh( .T. ), ;
               ::aClipBoard := NIL, ::lUnDo := .F. }, "M_PASTE", "" )
            _EnableMenuItem( "M_PASTE", ::cParentWnd )
         ELSE
            _ModifyMenuItem( "M_PASTE", ::cParentWnd, ::aMsg[ 9 ], NIL, "M_PASTE", "" )
            _DisableMenuItem( "M_PASTE", ::cParentWnd )
         ENDIF
         _ModifyMenuItem( "M_DEL", ::cParentWnd, ::aMsg[ 10 ], ;
            {|| ::aClipBoard := { ColClone( ::aColumns[ nCol ], Self ), nCol, ::aMsg[ 11 ] }, ;
            ::DelColumn( nCol ), ::Refresh( .T. ), ::lUnDo := .T. }, "M_DEL", "" )
      ENDIF
   ENDIF

RETURN 0

* ============================================================================
* METHOD TSBrowse:Refresh() Version 9.0 Nov/30/2009
* ============================================================================

METHOD Refresh( lPaint, lRecount, lClearHash ) CLASS TSBrowse

   LOCAL nOldRec

   DEFAULT lPaint := .T., ;
      lRecount := .F., ;
      lClearHash := ::lFastDrawClear

   IF lClearHash
      ::aFastDrawCell := hb_Hash()
   ENDIF

   IF ::lFirstPaint == NIL .OR. ::lFirstPaint
      RETURN 0
   ENDIF

   IF lRecount .OR. Empty( ::nLen )
      ::nLen := iif( ::lIsDbf, ( ::cAlias )->( Eval( ::bLogicLen ) ), Eval( ::bLogicLen ) )
   ENDIF

   ::lNoPaint := .F.

   IF ::lIsDbf .AND. ::lShared .AND. ::IsRowPosAtRec
      nOldRec := ( ::cAlias )->( RecNo() )
      ( ::cAlias )->( dbGoto( ::aRowPosAtRec[ 1 ] ) )
      IF nOldRec != ::aRowPosAtRec[ 1 ]
         ( ::cAlias )->( dbGoto( nOldRec ) )
      ENDIF
   ENDIF

RETURN ::Super:Refresh( lPaint )

* ============================================================================
* METHOD TSBrowse:RelPos() Version 9.0 Nov/30/2009
// Calculates the relative position of vertical scroll box in huge databases
* ============================================================================

METHOD RelPos( nLogicPos ) CLASS TSBrowse

   LOCAL nRet

   IF ::nLen > MAX_POS
      nRet := Int( nLogicPos * ( MAX_POS / ::nLen ) )
   ELSE
      nRet := nLogicPos
   ENDIF

RETURN nRet

* ============================================================================
* METHOD TSBrowse:Report() Version 9.0 Nov/30/2009
* ============================================================================

METHOD Report( cTitle, aCols, lPreview, lMultiple, lLandscape, lFromPos, aTotal ) CLASS TSBrowse

   LOCAL nI, nRecNo, nSize, cAlias, ;
      aHeader1 := {}, ;
      aHeader2 := {}, ;
      aFields := {}, ;
      aWidths := {}, ;
      aFormats := {}, ;
      hFont := ::hFont, ;
      lSelector := ::lSelector

#ifdef _TSBFILTER7_
   LOCAL cFilterBlock
#endif

   DEFAULT cTitle := ::oWnd:GetText(), ;
      lPreview := .T., ;
      lMultiple := .F., ;
      lLandscape := .F., ;
      lFromPos := .F.

   ::lNoPaint := .F.

   IF lSelector
      ::aClipBoard := { ColClone( ::aColumns[ 1 ], Self ), 1, "" }
      ::DelColumn( 1 )
   ENDIF

   IF aCols == NIL

      aCols := {}

      FOR nI := 1 TO Len( ::aColumns )
         AAdd( aCols, nI )
      NEXT

   ENDIF

   IF aTotal == NIL
      aTotal := AFill( Array( Len( aCols ) ), .F. )
   ELSE
      IF Len( aTotal ) <> Len( aCols )
         ASize( aTotal, Len( aCols ) )
      ENDIF
      FOR nI := 1 TO Len( aCols )
         IF aTotal[ nI ] == NIL
            aTotal[ nI ] := .F.
         ENDIF
      NEXT
   ENDIF

   IF ::lIsDbf
      nRecNo := ( ::cAlias )->( RecNo() )
      cAlias := ::cAlias
   ENDIF

   IF lFromPos
      ::lNoResetPos := .F.
   ELSE
      ::GoTop()
   ENDIF
   Eval( ::bGoTop )

   FOR nI := 1 TO Len( aCols )

      IF ! ::aColumns[ aCols[ nI ] ]:lBitMap

         nSize := Max( 1, Round( ::aColSizes[ aCols[ nI ] ] / ;
            GetTextWidth( 0, "b", iif( hFont != NIL, ;
            hFont, ::hFont ) ), 0 ) )
         AAdd( aWidths, nSize )
         AAdd( aHeader1, "" )
         AAdd( aHeader2, ::aColumns[ aCols[ nI ] ]:cHeading )
         AAdd( aFields, ::aColumns[ aCols[ nI ] ]:cData )
         AAdd( aFormats, iif( ::aColumns[ aCols[ nI ] ]:cPicture != NIL, ::aColumns[ aCols[ nI ] ]:cPicture, '' ) ) // Nil or char.

      ENDIF

   NEXT

#ifdef _TSBFILTER7_
   IF ::lIsDbf .AND. ::lFilterMode

      cFilterBlock := BuildFiltr( ::cField, ::uValue1, ::uValue2, SELF )

      ( cAlias )->( dbSetFilter( &( cFilterBlock ), cFilterBlock ) )
      ( cAlias )->( dbGoTop() )

   ENDIF
#endif

   IF ::lIsDbf

      EasyReport( cTitle + "|" + ::aMsg[ 20 ] + Space( 1 ) + ;
         DToC( Date() ) + " - " + ::aMsg[ 22 ] + Space( 1 ) + Time(), ;
         aHeader1, aHeader2, ;
         aFields, aWidths, aTotal, , .F., lPreview,,,,,, ;
         lMultiple,,, ;
         lLandscape,, .T., ;
         cAlias,, aFormats, ;
         DMPAPER_A4,, .T. )

      ( ::cAlias )->( dbGoto( nRecNo ) )
   ENDIF

#ifdef _TSBFILTER7_
   IF ::lIsDbf .AND. ::lFilterMode
      ( cAlias )->( dbClearFilter() )
   ENDIF
#endif

   IF lSelector
      ::InsColumn( ::aClipBoard[ 2 ], ::aClipBoard[ 1 ] )
      ::lNoPaint := .F.
   ENDIF

   ::lHitTop := .F.
   ::SetFocus()

RETURN Self

* ============================================================================
* METHOD TSBrowse:Reset() Version 9.0 Nov/30/2009
* ============================================================================

METHOD Reset( lBottom ) CLASS TSBrowse

   LOCAL nMin, nMax, nPage

   DEFAULT lBottom := .F., ;
      ::lInitGoTop := .F.

   IF ::lIsDbf
      ::nLen := ( ::cAlias )->( Eval( ::bLogicLen ) )
   ELSE
      ::nLen := Eval( ::bLogicLen )
   ENDIF

   IF ! ::lNoVScroll .AND. ::oVScroll != NIL

      IF ::nLen <= ::nRowCount()
         ::oVScroll:SetRange( 0, 0 )
      ELSE
         nMin := Min( 1, ::nLen )
         nMax := Min( ::nLen, MAX_POS )
         nPage := Min( ::nRowCount(), ::nLen )
         ::oVScroll:SetRange( nMin, nMax )
      ENDIF

   ENDIF

   ::lNoPaint := .F.
   ::lHitTop := ::lHitBottom := .F.
   ::nColPos := 1

   IF lBottom
      ::GoBottom()
   ELSEIF ::lInitGoTop
      ::GoTop()
   ENDIF

   ::Refresh( .T., .T. )

   IF ::bChange != NIL
      Eval( ::bChange, Self, 0 )
   ENDIF

RETURN Self


* ============================================================================
* METHOD TSBrowse:ResetVScroll() Version 9.0 Nov/30/2009
* ============================================================================

METHOD ResetVScroll( lInit ) CLASS TSBrowse

   LOCAL nMin, nMax, nPage, ;
      nLogicPos := ::nLogicPos()

   DEFAULT lInit := .F.

   IF ::nLen <= ::nRowCount()
      nMin := nMax := 0
   ELSE
      nMin := Min( 1, ::nLen )
      nMax := Min( ::nLen, MAX_POS )
   ENDIF

   IF lInit .AND. ! ::lNoVScroll

      IF ::oVScroll == NIL
         nPage := Min( ::nRowCount(), ::nLen )
         ::oVScroll := TSBScrlBar ():WinNew( nMin, nMax, nPage, .T., Self )
      ELSE
         ::oVScroll:SetRange( nMin, nMax )
      ENDIF

      ::oVScroll:SetPos( ::RelPos( nLogicPos ) )
   ELSEIF ::oVScroll != NIL
      ::oVScroll:SetRange( nMin, nMax )
      ::oVScroll:SetPos( ::RelPos( ::nLogicPos() ) )
   ENDIF

RETURN Self

* ============================================================================
* METHOD TSBrowse:ResetSeek() Version 9.0 Nov/30/2009
* ============================================================================

METHOD ResetSeek() CLASS TSBrowse

   IF ::nColOrder > 0
      IF ::cOrderType == "D"
         ::cSeek := "  /  /    "
      ELSE
         ::cSeek := ""
      ENDIF
   ENDIF

   IF ::bSeekChange != NIL
      Eval( ::bSeekChange )
   ENDIF

   nNewEle := 0

RETURN ::cSeek

* ============================================================================
* METHOD TSBrowse:ReSize() Version 9.0 Nov/30/2009
* ============================================================================

METHOD ReSize( nSizeType, nWidth, nHeight ) CLASS TSBrowse

   LOCAL nTotPix := 0

   IF Empty( ::aColSizes )
      RETURN NIL
   ENDIF

   AEval( ::aColumns, {| oCol | iif( oCol:lVisible, nTotPix += oCol:nWidth, Nil ) } ) // 14.07.2015

   IF ::lEditing .AND. ::aColumns[ ::nCell ]:oEdit != NIL .AND. IsWindowHandle( ::aColumns[ ::nCell ]:oEdit:hWnd )
      SendMessage( ::aColumns[ ::nCell ]:oEdit:hWnd, WM_KEYDOWN, VK_ESCAPE, 0 )
   ENDIF

   IF ! Empty( ::nAdjColumn )
      ::nAdjColumn := Min( Len( ::aColumns ), ::nAdjColumn )
   ENDIF

   ::nRowPos := Min( ::nRowPos, Max( ::nRowCount(), 1 ) )

   IF ! Empty( ::nAdjColumn ) .AND. nTotPix != nWidth
      ::aColumns[ ::nAdjColumn ]:nWidth := ;
         ::aColSizes[ ::nAdjColumn ] += ( nWidth - nTotPix )
   ENDIF

RETURN ::Super:ReSize( nSizeType, nWidth, nHeight )

* ============================================================================
* METHOD TSBrowse:Seek() Version 9.0 Nov/30/2009
* ============================================================================

METHOD Seek( nKey ) CLASS TSBrowse

   LOCAL nIdxLen, cPrefix, lFound, lEoF, nRecNo, ;
      nLines := ::nRowCount(), ;
      lTrySeek := .T., ;
      cSeek := ::cSeek, ;
      xSeek

   IF ( Seconds() - ::nLapsus ) > 3 .OR. ( Seconds() - ::nLapsus ) < 0
      ::cSeek := cSeek := ""
   ENDIF

   ::nLapsus := Seconds()
   cPrefix := iif( ::cPrefix == NIL, "", iif( ValType( ::cPrefix ) == "B", Eval( ::cPrefix, Self ), ::cPrefix ) )

   IF ::nColOrder > 0 .AND. ::lIsDbf
      lTrySeek := .T.

      IF ::cOrderType == "C"
         nIdxLen := ( ::cAlias )->( Len( Eval( &( "{||" + IndexKey() + "}" ) ) ) )
      ENDIF

      IF nKey == VK_BACK
         IF ::cOrderType == "D"
            cSeek := DateSeek( cSeek, nKey )
         ELSE
            cSeek := Left( cSeek, Len( cSeek ) - 1 )
         ENDIF
      ELSE
         IF ::cOrderType == "D"
            cSeek := DateSeek( cSeek, nKey )
         ELSEIF ::cOrderType == "N"
            /* only  0..9, minus and dot*/
            IF ( nKey >= 48 .AND. nKey <= 57 ) .OR. ( nKey >= 45 .OR. ;
                  nKey <= 46 )
               cSeek += Chr( nKey )
            ELSE
               Tone( 500, 1 )
               lTrySeek := .F.
            ENDIF
         ELSEIF ::cOrderType == "C"
            IF Len( cSeek ) < nIdxLen
               cSeek += iif( ::lUpperSeek, Upper( Chr( nKey ) ), Chr( nKey ) )
            ELSE
               Tone( 500, 1 )
               lTrySeek := .F.
            ENDIF
         ENDIF
      ENDIF

      IF ::cOrderType == "C"
         xSeek := cPrefix + cSeek
      ELSEIF ::cOrderType == "N"
         xSeek := Val( cSeek )
      ELSEIF ::cOrderType == "D"
         xSeek := cPrefix + DToS( CToD( cSeek ) )
      ELSE
         xSeek := cPrefix + cSeek
      ENDIF

      IF !( ::cOrderType == "D" .AND. Len( RTrim( cSeek ) ) < Len( DToC( Date() ) ) ) .AND. lTrySeek

         nRecNo := ( ::cAlias )->( RecNo() )
         lFound := ( ::cAlias )->( dbSeek( xSeek, .T. ) )
         lEoF := ( ::cAlias )->( Eof() )

         IF lEoF .OR. ( ::cOrderType == "C" .AND. ! lFound )
            ( ::cAlias )->( dbGoto( nRecNo ) )
         ENDIF

         IF ( ::cOrderType == "C" .AND. ! lFound ) .OR. lEof
            Tone( 500, 1 )

            IF ::cOrderType == "D"
               ::cSeek := DateSeek( cSeek, VK_BACK )
            ELSE
               ::cSeek := Left( cSeek, Len( cSeek ) - 1 )
            ENDIF

            RETURN Self

         ELSEIF ! lFound

            IF ::cOrderType == "N"
               ( ::cAlias )->( dbSeek( Val( cSeek ) * 10, .T. ) )
               xSeek := ( ::cAlias )->( Eval( &( "{||" + IndexKey() + "}" ) ) )
               xSeek := Val( Right( LTrim( Str( xSeek ) ), Len( cSeek ) + 1 ) )

               IF xSeek > ( ( Val( cSeek ) * 10 ) + 9 )
                  Tone( 500, 1 )
                  ( ::cAlias )->( dbGoto( nRecNo ) )
                  ::cSeek := Left( cSeek, Len( cSeek ) - 1 )
                  RETURN Self
               ENDIF

            ENDIF

            ::cSeek := cSeek
            ( ::cAlias )->( dbGoto( nRecNo ) )
            RETURN Self
         ELSE
            ::lHitBottom := ::lHitTop := .F.

            IF nRecNo != ( ::cAlias )->( RecNo() ) .AND. ::nLen > nLines
               nRecNo := ( ::cAlias )->( RecNo() )
               ( ::cAlias )->( dbSkip( nLines - ::nRowPos ) )

               IF ( ::cAlias )->( Eof() )
                  Eval( ::bGoBottom )
                  ::nRowPos := nLines

                  WHILE ::nRowPos > 1 .AND. ( ::cAlias )->( RecNo() ) != nRecNo
                     ::Skip( -1 )
                     ::nRowPos--
                  ENDDO
               ELSE
                  ( ::cAlias )->( dbGoto( nRecNo ) )
                  ::Upstable()
               ENDIF

               ::Refresh( .F. )
               ::ResetVScroll()
            ELSEIF nRecNo != ( ::cAlias )->( RecNo() )
               nRecNo := ( ::cAlias )->( RecNo() )
               Eval( ::bGoTop )
               ::nAt := ::nRowPos := 1

               WHILE nRecNo != ( ::cAlias )->( RecNo() )
                  ::Skip( 1 )
                  ::nRowPos++
               ENDDO

               ::Refresh( .F. )
               ::ResetVScroll()
            ENDIF

            IF ::bChange != NIL
               Eval( ::bChange, Self, 0 )
            ENDIF
         ENDIF
      ENDIF

      ::cSeek := cSeek

      IF ::bSeekChange != NIL
         Eval( ::bSeekChange )
      ENDIF

   ELSEIF ::nColOrder > 0 .AND. ::lIsArr
      lTrySeek := .T.
      nIdxLen := Len( cValToChar( ::aArray[ ::nAt, ::nColOrder ] ) )

      IF nKey == VK_BACK

         IF ::cOrderType == "D"
            cSeek := DateSeek( cSeek, nKey )
         ELSE
            cSeek := Left( cSeek, Len( cSeek ) - 1 )
         ENDIF
      ELSE
         IF ::cOrderType == "D"
            cSeek := DateSeek( cSeek, nKey )
         ELSEIF ::cOrderType == "N"
            /* only  0..9, minus and dot*/
            IF ( nKey >= 48 .AND. nKey <= 57 ) .OR. ( nKey >= 45 .OR. ;
                  nKey <= 46 )
               cSeek += Chr( nKey )
            ELSE
               Tone( 500, 1 )
               lTrySeek := .F.
            ENDIF
         ELSEIF ::cOrderType == "C"
            IF Len( cSeek ) < nIdxLen
               cSeek += iif( ::lUpperSeek, Upper( Chr( nKey ) ), Chr( nKey ) )
            ELSE
               Tone( 500, 1 )
               lTrySeek := .F.
            ENDIF
         ENDIF
      ENDIF

      IF ::cOrderType == "C"
         xSeek := cPrefix + cSeek
      ELSEIF ::cOrderType == "N"
         xSeek := Val( cSeek )
      ELSEIF ::cOrderType == "D"
         xSeek := cPrefix + DToS( CToD( cSeek ) )
      ELSE
         xSeek := cPrefix + cSeek
      ENDIF

      IF !( ::cOrderType == "D" .AND. Len( RTrim( cSeek ) ) < Len( DToC( Date() ) ) ) .AND. lTrySeek
         nRecNo := ::nAt
         lFound := lASeek( xSeek,, Self )

         IF ! lFound
            ::nAt := nRecNo
         ENDIF

         IF ::cOrderType == "C" .AND. ! lFound
            Tone( 500, 1 )
            ::cSeek := Left( cSeek, Len( cSeek ) - 1 )
            RETURN Self
         ELSEIF ! lFound
            IF ::cOrderType == "N"
               lASeek( Val( cSeek ) * 10, .T., Self )
               xSeek := ::aArray[ ::nAt, ::nColOrder ]
               xSeek := Val( Right( LTrim( Str( xSeek ) ), Len( cSeek ) + 1 ) )

               IF xSeek > ( ( Val( cSeek ) * 10 ) + 9 )
                  Tone( 500, 1 )
                  ::cSeek := Left( cSeek, Len( cSeek ) - 1 )
                  ::nAt := nRecNo
                  RETURN Self
               ENDIF
            ENDIF

            ::cSeek := cSeek
            ::nAt := nRecNo
            RETURN Self
         ELSE
            IF nRecNo != ::nAt .AND. ::nLen > nLines
               nRecNo := ::nAt
               ::nAt += ( nLines - ::nRowPos )

               IF ::nAt > ::nLen
                  Eval( ::bGoBottom )
                  ::nRowPos := nLines

                  WHILE ::nRowPos > 1 .AND. ::nAt != nRecNo
                     ::Skip( -1 )
                     ::nRowPos--
                  ENDDO
               ELSE
                  ::nAt := nRecNo
               ENDIF

               ::Refresh( .F. )
               ::ResetVScroll()
            ELSEIF nRecNo != ::nAt
               nRecNo := ::nAt
               Eval( ::bGoTop )
               ::nAt := ::nRowPos := 1

               WHILE nRecNo != ::nAt
                  ::Skip( 1 )
                  ::nRowPos++
               ENDDO

               ::Refresh( .F. )
               ::ResetVScroll()
            ENDIF

            IF ::bChange != NIL
               Eval( ::bChange, Self, 0 )
            ENDIF
         ENDIF
      ENDIF

      ::cSeek := cSeek

      IF ::bSeekChange != NIL
         Eval( ::bSeekChange )
      ENDIF
   ENDIF

   IF ! lTrySeek
      ::ResetSeek()
   ENDIF

   IF ::lIsArr .AND. ::bSetGet != NIL
      IF ValType( Eval( ::bSetGet ) ) == "N"
         Eval( ::bSetGet, ::nAt )
      ELSEIF ::nLen > 0
         Eval( ::bSetGet, ::aArray[ ::nAt, 1 ] )
      ELSE
         Eval( ::bSetGet, "" )
      ENDIF
   ENDIF

RETURN Self

* ============================================================================
* METHOD TSBrowse:Set3DText() Version 9.0 Nov/30/2009
* ============================================================================

METHOD Set3DText( lOnOff, lRaised, nColumn, nLevel, nClrLight, ;
      nClrShadow ) CLASS TSBrowse

   LOCAL nEle

   DEFAULT lOnOff := .T., ;
      lRaised := .T., ;
      nClrLight := GetSysColor( COLOR_BTNHIGHLIGHT ), ;
      nClrShadow := GetSysColor( COLOR_BTNSHADOW )

   IF Empty( ::aColumns )
      RETURN Self
   ENDIF

   IF ! lOnOff
      IF Empty( nColumn )
         IF Empty( nLevel )
            FOR nEle := 1 TO Len( ::aColumns )
               ::aColumns[ nEle ]:l3DTextCell := NIL
               ::aColumns[ nEle ]:l3DTextHead := NIL
               ::aColumns[ nEle ]:l3DTextFoot := NIL
            NEXT
         ELSEIF nLevel == 1
            FOR nEle := 1 TO Len( ::aColumns )
               ::aColumns[ nEle ]:l3DTextCell := NIL
            NEXT
         ELSEIF nLevel == 2
            FOR nEle := 1 TO Len( ::aColumns )
               ::aColumns[ nEle ]:l3DTextHead := NIL
            NEXT
         ELSE
            FOR nEle := 1 TO Len( ::aColumns )
               ::aColumns[ nEle ]:l3DTextFoot := NIL
            NEXT
         ENDIF

      ELSEIF Empty( nLevel )
         ::aColumns[ nColumn ]:l3DTextCell := NIL
         ::aColumns[ nColumn ]:l3DTextHead := NIL
         ::aColumns[ nColumn ]:l3DTextFoot := NIL
      ELSEIF nLevel == 1
         ::aColumns[ nColumn ]:l3DTextCell := NIL
      ELSEIF nLevel == 2
         ::aColumns[ nColumn ]:l3DTextHead := NIL
      ELSE
         ::aColumns[ nColumn ]:l3DTextFoot := NIL
      ENDIF

      RETURN Self

   ENDIF

   IF Empty( nColumn )
      IF Empty( nLevel )
         FOR nEle := 1 TO Len( ::aColumns )
            ::aColumns[ nEle ]:l3DTextCell := lRaised
            ::aColumns[ nEle ]:l3DTextHead := lRaised
            ::aColumns[ nEle ]:l3DTextFoot := lRaised
            ::aColumns[ nEle ]:nClr3DLCell := nClrLight
            ::aColumns[ nEle ]:nClr3DLHead := nClrLight
            ::aColumns[ nEle ]:nClr3DLFoot := nClrLight
            ::aColumns[ nEle ]:nClr3DSCell := nClrShadow
            ::aColumns[ nEle ]:nClr3DSHead := nClrShadow
            ::aColumns[ nEle ]:nClr3DSFoot := nClrShadow
         NEXT
      ELSEIF nLevel == 1
         FOR nEle := 1 TO Len( ::aColumns )
            ::aColumns[ nEle ]:l3DTextCell := lRaised
            ::aColumns[ nEle ]:nClr3DLCell := nClrLight
            ::aColumns[ nEle ]:nClr3DSCell := nClrShadow
         NEXT
      ELSEIF nLevel == 2
         FOR nEle := 1 TO Len( ::aColumns )
            ::aColumns[ nEle ]:l3DTextHead := lRaised
            ::aColumns[ nEle ]:nClr3DLHead := nClrLight
            ::aColumns[ nEle ]:nClr3DSHead := nClrShadow
         NEXT

      ELSE
         FOR nEle := 1 TO Len( ::aColumns )
            ::aColumns[ nEle ]:l3DTextFoot := lRaised
            ::aColumns[ nEle ]:nClr3DLFoot := nClrLight
            ::aColumns[ nEle ]:nClr3DSFoot := nClrShadow
         NEXT
      ENDIF
   ELSE
      IF Empty( nLevel )
         ::aColumns[ nColumn ]:l3DTextCell := lRaised
         ::aColumns[ nColumn ]:l3DTextHead := lRaised
         ::aColumns[ nColumn ]:l3DTextFoot := lRaised
         ::aColumns[ nColumn ]:nClr3DLCell := nClrLight
         ::aColumns[ nColumn ]:nClr3DLHead := nClrLight
         ::aColumns[ nColumn ]:nClr3DLFoot := nClrLight
         ::aColumns[ nColumn ]:nClr3DSCell := nClrShadow
         ::aColumns[ nColumn ]:nClr3DSHead := nClrShadow
         ::aColumns[ nColumn ]:nClr3DSFoot := nClrShadow
      ELSEIF nLevel == 1
         ::aColumns[ nColumn ]:l3DTextCell := lRaised
         ::aColumns[ nColumn ]:nClr3DLCell := nClrLight
         ::aColumns[ nColumn ]:nClr3DSCell := nClrShadow
      ELSEIF nLevel == 2
         ::aColumns[ nColumn ]:l3DTextHead := lRaised
         ::aColumns[ nColumn ]:nClr3DLHead := nClrLight
         ::aColumns[ nColumn ]:nClr3DSHead := nClrShadow
      ELSE
         ::aColumns[ nColumn ]:l3DTextFoot := lRaised
         ::aColumns[ nColumn ]:nClr3DLFoot := nClrLight
         ::aColumns[ nColumn ]:nClr3DSFoot := nClrShadow
      ENDIF
   ENDIF

RETURN Self

* ============================================================================
* METHOD TSBrowse:SetAlign() Version 9.0 Nov/30/2009
* ============================================================================

METHOD SetAlign( nColumn, nLevel, nAlign ) CLASS TSBrowse

   DEFAULT nColumn := 0, ;
      nLevel := 0, ;
      nAlign := DT_LEFT

   IF nColumn > 0
      IF nLevel > 0
         DO CASE
         CASE nLevel == 1
            ::aColumns[ nColumn ]:nAlign := nAlign
         CASE nLevel == 2
            ::aColumns[ nColumn ]:nHAlign := nAlign
         OTHERWISE
            ::aColumns[ nColumn ]:nFAlign := nAlign
         ENDCASE
      ELSE
         ::aColumns[ nColumn ]:nAlign := ::aColumns[ nColumn ]:nHAlign := ;
            ::aColumns[ nColumn ]:nFAlign := nAlign
      ENDIF
   ELSE
      IF nLevel > 0
         AEval( ::aColumns, {| oCol | iif( nLevel = 1, oCol:nAlign := nAlign, iif( nLevel = 2, oCol:nHAlign := nAlign, ;
            oCol:nFAlign := nAlign ) ) } )
      ELSE
         AEval( ::aColumns, {| oCol | oCol:nAlign := nAlign, oCol:nHAlign := nAlign, oCol:nFAlign := nAlign } )
      ENDIF
   ENDIF

   IF ::lPainted
      ::Refresh( .T. )
   ENDIF

RETURN Self

* ============================================================================
* METHOD TSBrowse:SetAppendMode() Version 9.0 Nov/30/2009
// Enables append mode in TSBrowse for DBF's and arrays.
// At least one column in TSBrowse must have oCol:lEdit set to TRUE in order
// to work and like direct cell editing.
* ============================================================================

METHOD SetAppendMode( lMode ) CLASS TSBrowse

   LOCAL lPrevMode := ::lCanAppend

   DEFAULT lMode := ! ::lCanAppend, ;
      ::lIsTxt := AND( ::nDataType, DATATYPE_TEXT ) != 0

   IF ! ::lIsTxt
      ::lCanAppend := lMode
   ENDIF

RETURN lPrevMode

* ============================================================================
* METHOD TSBrowse:SetArray() Version 9.0 Nov/30/2009
* ============================================================================

METHOD SetArray( aArray, lAutoCols, aHead, aSizes ) CLASS TSBrowse

   LOCAL nColumns, nI, cType, nMax, bData, cHead, ;
      lListBox := Len( aArray ) > 0 .AND. ValType( aArray[ 1 ] ) != "A"

   DEFAULT aArray := {}, ;
      lAutoCols := ::lAutoCol, ;
      aHead := ::aHeaders, ;
      aSizes := ::aColSizes

   IF lListBox
      ::aArray := {}

      FOR nI := 1 TO Len( aArray )
         AAdd( ::aArray, { aArray[ nI ] } )
      NEXT
   ELSE
      ::aArray := aArray
   ENDIF

   // default values for array elements used during append mode
   // The user MUST AIns() as element no. 1 to ::aDefValue
   // a nil value when using the actual element no (::nAt)
   // when browsing arrays like this:
   // AIns( ASize( ::aDefValue, Len( ::aDefValue ) + 1 ), 1 )
   // AFTER calling ::SetArray()

   nColumns := iif( ! Empty( aHead ), Len( aHead ), iif( ! Empty( ::aArray ), Len( ::aArray[ 1 ] ), 0 ) )
   ::aDefValue := Array( Len( aArray[ 1 ] ) )

   FOR nI := 1 TO nColumns
      cType := ValType( ::aArray[ 1, nI ] )

      IF cType $ "CM"
         ::aDefValue[ nI ] := Space( Len( ::aArray[ 1, nI ] ) )
      ELSEIF cType == "N"
         ::aDefValue[ nI ] := 0
      ELSEIF cType == "D"
         ::aDefValue[ nI ] := CToD( "" )
      ELSEIF cType == "T"

#ifdef __XHARBOUR__
         ::aDefValue[ nI ] := CToT( "" )
#else
         ::aDefValue[ nI ] := hb_CToT( "" )
#endif
      ELSEIF cType == "L"
         ::aDefValue[ nI ] := .F.
      ELSE // arrays, objects and codeblocks not allowed
         ::aDefValue[ nI ] := "???" // not user editable data type
      ENDIF
   NEXT

   ::nAt := 1
   ::bKeyNo := {| n | iif( n == NIL, ::nAt, ::nAt := n ) }
   ::cAlias := "ARRAY" // don't change name, used in method Default()
   ::nDataType := DATATYPE_ARRAY
   ::lIsArr := .T.
   ::lIsDbf := .F.
   ::nLen := Eval( ::bLogicLen := {|| Len( ::aArray ) + iif( ::lAppendMode, 1, 0 ) } )
   ::bGoTop := {|| ::nAt := 1 }
   ::bGoBottom := {|| ::nAt := Eval( ::bLogicLen ) }
   ::bSkip := {| nSkip, nOld | nOld := ::nAt, ::nAt += nSkip, ::nAt := Min( Max( ::nAt, 1 ), ::nLen ), ::nAt - nOld }
   ::bGoToPos := {| n | Eval( ::bKeyNo, n ) }
   ::bBof := {|| ::nAt < 1 }
   ::bEof := {|| ::nAt > Len( ::aArray ) }

   ::lHitTop := .F.
   ::lHitBottom := .F.
   ::nRowPos := 1
   ::nColPos := 1
   ::nCell := 1

   ::HiliteCell( 1 )
   DEFAULT lAutocols := ( ! Empty( aHead ) .AND. ! Empty( aSizes ) )

   IF lAutoCols .AND. Empty( ::aColumns ) .AND. lListBox
      nMax := ( ::nRight - ::nLeft + 1 ) * iif( Empty( ::hWnd ), 2, 1 )
      ::lDrawHeaders := .F.
      ::nLineStyle := LINES_NONE
      ::lNoHScroll := .T.
      ::AddColumn( TSColumn():New( NIL, ArrayWBlock( Self, 1 ),,,, nMax,, ::lEditable,,,,,,,,,,, Self, ;
         "ArrayWBlock(::oBrw,1)" ) )
   ELSEIF lAutoCols .AND. Empty( ::aColumns ) .AND. ValType( ::aArray[ 1 ] ) == "A"
      IF Empty( aHead )
         aHead := AutoHeaders( Len( ::aArray[ 1 ] ) )
      ENDIF

      IF aSizes != NIL .AND. ValType( aSizes ) != "A"
         aSizes := AFill( Array( Len( ::aArray[ 1 ] ) ), nValToNum( aSizes ) )
      ELSEIF ValType( aSizes ) == "A" .AND. ! Empty( aSizes )
         IF Len( aSizes ) < nColumns
            nI := Len( aSizes ) + 1
            ASize( aSizes, nColumns )
            AFill( aSizes, aSizes[ 1 ], nI )
         ENDIF
      ELSE
         aSizes := NIL
      ENDIF

      IF ! ::lCellBrw .AND. aSizes == NIL
         ::lNoHScroll := .T.
      ENDIF

      FOR nI := 1 TO nColumns

         bData := ArrayWBlock( Self, nI )
         cHead := cValToChar( aHead[ nI ] )

         IF Empty( aSizes )
            nMax := Max( GetTextWidth( 0, cValToChar( Eval( bData ) ) ), GetTextWidth( 0, cHead ) )
            nMax := Max( nMax, 70 )
         ELSE
            nMax := aSizes[ nI ]
         ENDIF

         ::AddColumn( TSColumn():New( cHead, bData,,,, nMax,, ::lEditable,,,,,,,,,,, Self, ;
            "ArrayWBlock(::oBrw," + LTrim( Str( nI ) ) + ")" ) )
      NEXT
   ENDIF

   IF ::lPhantArrRow .AND. Len( ::aArray ) > 1
      ::lPhantArrRow := .F.
   ENDIF

   ::lNoPaint := .F.
   ::ResetVScroll( .T. )

   IF ::lPainted
      ::GoTop()
      ::Refresh()
   ENDIF

RETURN Self

* ============================================================================

METHOD SetArrayTo( aArray, uFontHF, aHead, aSizes, uFooter, aPicture, aAlign, aName ) CLASS TSBrowse

   LOCAL nColumns, nI, cType, nMax, bData, cHead
   LOCAL nN, cData, aDefMaxVal, aDefMaxLen, aDefType, aDefAlign, aDefFooter, oCol, ;
      nAlign, aAligns, lFooter := .F., cFooter, nFooter, cTemp, cPict, ;
      hFont := iif( ::hFont != NIL, ::hFont, 0 ), ;
      lFont := ( hFont != 0 ), hFontHead := hFont, hFontFoot := hFont

   DEFAULT aHead := AClone( ::aHeaders ), ;
      aSizes := AClone( ::aColSizes ), ;
      aPicture := AClone( ::aFormatPic ), ;
      aAlign := iif( ISARRAY( ::aJustify ), AClone( ::aJustify ), {} ), ;
      aName := {}

   IF ValType( uFontHF ) == 'N' .AND. uFontHF != 0
      hFontHead := uFontHF
      hFontFoot := uFontHF
   ELSEIF ValType( uFontHF ) == 'A' .AND. Len( uFontHF ) >= 2
      IF ValType( uFontHF[ 1 ] ) == 'N' .AND. uFontHF[ 1 ] != 0
         hFontHead := uFontHF[ 1 ]
      ENDIF
      IF ValType( uFontHF[ 2 ] ) == 'N' .AND. uFontHF[ 2 ] != 0
         hFontFoot := uFontHF[ 2 ]
      ENDIF
   ENDIF

   IF hFontHead != NIL
      ::hFontHead := hFontHead
   ENDIF
   IF hFontFoot != NIL
      ::hFontFoot := hFontFoot
   ENDIF

   ::aArray := aArray
   ::lPickerMode := .F.

   nColumns := iif( ! Empty( aHead ), Len( aHead ), iif( ! Empty( ::aArray ), Len( ::aArray[ 1 ] ), 0 ) )

   ::aDefValue := Array( iif( ! Empty( ::aArray ), Len( aArray[ 1 ] ), 0 ) )

   aDefMaxVal := Array( nColumns )
   aDefType := Array( nColumns )
   aDefAlign := Array( nColumns )
   aDefMaxLen := Array( nColumns )

   AFill( aDefMaxLen, 0 )

   IF Len( aPicture ) != nColumns
      ASize( aPicture, nColumns )
   ENDIF

   IF Len( aAlign ) != nColumns
      ASize( aAlign, nColumns )
   ENDIF

   IF Len( aName ) != nColumns
      ASize( aName, nColumns )
   ENDIF

   FOR nI := 1 TO nColumns
      cType := ValType( ::aArray[ 1, nI ] )
      aDefType[ nI ] := cType

      IF cType $ "CM"
         ::aDefValue[ nI ] := Space( Len( ::aArray[ 1, nI ] ) )
         aDefMaxVal[ nI ] := Trim( ::aArray[ 1, nI ] )
         aDefMaxLen[ nI ] := iif( CRLF $ aDefMaxVal[ nI ], 0, Len( aDefMaxVal[ nI ] ) )
         aDefAlign[ nI ] := DT_LEFT
      ELSEIF cType == "N"
         ::aDefValue[ nI ] := 0
         aDefMaxVal[ nI ] := cValToChar( ::aArray[ 1, nI ] )
         aDefMaxLen[ nI ] := Len( aDefMaxVal[ nI ] )
         aDefAlign[ nI ] := DT_RIGHT
      ELSEIF cType == "D"
         ::aDefValue[ nI ] := CToD( "" )
         aDefMaxVal[ nI ] := cValToChar( ::aArray[ 1, nI ] )
         aDefMaxLen[ nI ] := Len( aDefMaxVal[ nI ] )
         aDefAlign[ nI ] := DT_CENTER
      ELSEIF cType == "T"

#ifdef __XHARBOUR__
         ::aDefValue[ nI ] := CToT( "" )
#else
         ::aDefValue[ nI ] := hb_CToT( "" )
#endif
         aDefMaxVal[ nI ] := cValToChar( ::aArray[ 1, nI ] )
         aDefMaxLen[ nI ] := Len( aDefMaxVal[ nI ] )
         aDefAlign[ nI ] := DT_LEFT
      ELSEIF cType == "L"
         ::aDefValue[ nI ] := .F.
         aDefMaxVal[ nI ] := cValToChar( ::aArray[ 1, nI ] )
         aDefMaxLen[ nI ] := Len( aDefMaxVal[ nI ] )
         aDefAlign[ nI ] := DT_CENTER
      ELSE // arrays, objects and codeblocks not allowed
         ::aDefValue[ nI ] := "???" // not user editable data type
         aDefMaxVal[ nI ] := "???"
         aDefMaxLen[ nI ] := 0
         aDefAlign[ nI ] := DT_LEFT
      ENDIF
   NEXT

   ::nAt := 1
   ::bKeyNo := {| n | iif( n == NIL, ::nAt, ::nAt := n ) }
   ::cAlias := "ARRAY" // don't change name, used in method Default()
   ::nDataType := DATATYPE_ARRAY
   ::lIsArr := .T.
   ::lIsDbf := .F.
   ::nLen := Eval( ::bLogicLen := {|| Len( ::aArray ) + iif( ::lAppendMode, 1, 0 ) } )
   ::bGoTop := {|| ::nAt := 1 }
   ::bGoBottom := {|| ::nAt := Eval( ::bLogicLen ) }
   ::bSkip := {| nSkip, nOld | nOld := ::nAt, ::nAt += nSkip, ::nAt := Min( Max( ::nAt, 1 ), ::nLen ), ::nAt - nOld }
   ::bGoToPos := {| n | Eval( ::bKeyNo, n ) }
   ::bBof := {|| ::nAt < 1 }
   ::bEof := {|| ::nAt > Len( ::aArray ) }

   ::lHitTop := .F.
   ::lHitBottom := .F.
   ::nRowPos := 1
   ::nColPos := 1
   ::nCell := 1

   ::HiliteCell( 1 )

   aDefFooter := Array( nColumns )
   AFill( aDefFooter, "" )

   IF ValType( uFooter ) == "L"
      lFooter := uFooter
   ELSEIF ValType( uFooter ) == "A"
      lFooter := .T.
      FOR nI := 1 TO Min( nColumns, Len( uFooter ) )
         aDefFooter[ nI ] := cValToChar( uFooter[ nI ] )
      NEXT
   ENDIF

   IF Empty( aHead )
      aHead := AutoHeaders( Len( ::aArray[ 1 ] ) )
   ENDIF

   IF aSizes != NIL .AND. ValType( aSizes ) != "A"
      aSizes := AFill( Array( Len( ::aArray[ 1 ] ) ), nValToNum( aSizes ) )
   ELSEIF ValType( aSizes ) == "A" .AND. ! Empty( aSizes )
      IF Len( aSizes ) < nColumns
         nI := Len( aSizes ) + 1
         ASize( aSizes, nColumns )
         AFill( aSizes, aSizes[ 1 ], nI )
      ENDIF
   ELSE
      aSizes := NIL
   ENDIF

   FOR nI := 1 TO Len( ::aArray )
      FOR nN := 1 TO nColumns
         IF HB_ISCHAR( ::aArray[ nI, nN ] ) .AND. CRLF $ ::aArray[ nI, nN ]
            cData := ""
            AEval( hb_aTokens( ::aArray[ nI, nN ], CRLF), {|x| x := Trim( x ), cData := iif( Len( x ) > Len( cData ), x, cData ) } )
         ELSE
            cData := cValToChar( ::aArray[ nI, nN ] )
         ENDIF
         IF Len( cData ) > Len( aDefMaxVal[ nN ] )
            IF aDefType[ nN ] == "C"
               aDefMaxVal[ nN ] := Trim( cData )
               aDefMaxLen[ nN ] := Max( aDefMaxLen[ nN ], Len( aDefMaxVal[ nN ] ) )
            ELSE
               aDefMaxVal[ nN ] := cData
               aDefMaxLen[ nN ] := Max( aDefMaxLen[ nN ], Len( cData ) )
            ENDIF
         ENDIF
      NEXT
   NEXT

   ::aHeaders := Array( nColumns )
   ::aColSizes := Array( nColumns )
   ::aFormatPic := Array( nColumns )
   ::aJustify := Array( nColumns )

   FOR nI := 1 TO nColumns

      bData := ArrayWBlock( Self, nI )
      cHead := cValToChar( aHead[ nI ] )
      nAlign := aDefAlign[ nI ]
      cPict := NIL

      IF aDefType[ nI ] == "C"
         IF ValType( aPicture[ nI ] ) == "C" .AND. Len( aPicture[ nI ] ) > 0
            cTemp := iif( Left( aPicture[ nI ], 2 ) == "@K", SubStr( aPicture[ nI ], 4 ), aPicture[ nI ] )
         ELSE
            cTemp := Replicate( "X", aDefMaxLen[ nI ] )
         ENDIF
         IF Len( cTemp ) > Len( ::aDefValue[ nI ] )
            ::aDefValue[ nI ] := Space( Len( cTemp ) )
         ENDIF
         cPict := Replicate( "X", Len( ::aDefValue[ nI ] ) )
      ELSEIF aDefType[ nI ] == "N"
         IF ValType( aPicture[ nI ] ) == "C"
            cPict := aPicture[ nI ]
         ELSE
            cPict := Replicate( "9", aDefMaxLen[ nI ] )
            IF ( nN := At( ".", aDefMaxVal[ nI ] ) ) > 0
               cTemp := SubStr( aDefMaxVal[ nI ], nN )
               cPict := Left( cPict, Len( cPict ) - Len( cTemp ) ) + "." + Replicate( "9", Len( cTemp ) - 1 )
            ENDIF
         ENDIF
      ENDIF

      IF ValType( aAlign[ nI ] ) == "N" .AND. ( aAlign[ nI ] == DT_LEFT .OR. ;
            aAlign[ nI ] == DT_CENTER .OR. ;
            aAlign[ nI ] == DT_RIGHT )
         nAlign := aAlign[ nI ]
      ENDIF

      IF lFooter
         aAligns := { nAlign, DT_CENTER, nAlign }
         cFooter := aDefFooter[ nI ]
         IF CRLF $ cFooter
            cTemp := ""
            AEval( hb_ATokens( cFooter, CRLF ), {| x | cTemp := iif( Len( x ) > Len( cTemp ), x, cTemp ) } )
         ELSE
            cTemp := cFooter
         ENDIF
         nFooter := GetTextWidth( 0, cTemp, hFontFoot )
      ELSE
         aAligns := { nAlign, DT_CENTER }
         cFooter := NIL
         nFooter := 0
      ENDIF

      IF CRLF $ cHead
         cTemp := ""
         AEval( hb_ATokens( cHead, CRLF ), {| x | cTemp := iif( Len( x ) > Len( cTemp ), x, cTemp ) } )
      ELSE
         cTemp := cHead
      ENDIF

      nMax := Max( GetTextWidth( 0, aDefMaxVal[ nI ] + 'W', hFont ), GetTextWidth( 0, cTemp, hFontHead ) )
      nMax := Max( nMax + GetBorderWidth(), 32 )
      nMax := Max( nMax, nFooter )

      IF ! Empty( aSizes )
         IF ValType( aSizes[ nI ] ) == 'N' .AND. aSizes[ nI ] > 0
            nMax := aSizes[ nI ]
         ELSEIF ValType( aSizes[ nI ] ) == 'C'
            nMax := GetTextWidth( 0, aSizes[ nI ], hFont )
         ENDIF
      ENDIF

      ::aHeaders[ nI ] := cHead
      ::aColSizes[ nI ] := nMax
      ::aFormatPic[ nI ] := cPict
      ::aJustify[ nI ] := aAligns

      oCol := TSColumn():New( cHead, bData, cPict,, aAligns, nMax,, ::lEditable,,,, cFooter,,,,,,, ;
         Self, "ArrayWBlock(::oBrw," + LTrim( Str( nI ) ) + ")" )
      IF lFont
         oCol:hFontHead := hFontHead
         IF lFooter
            oCol:hFontFoot := hFontFoot
         ENDIF
      ENDIF

      IF aDefType[ nI ] == "L"
         oCol:lCheckBox := .T.
         oCol:nEditMove := 0
      ENDIF

      IF ! Empty( aName[ nI ] ) .AND. ValType( aName[ nI ] ) == "C"
         oCol:cName := aName[ nI ]
      ENDIF

      ::AddColumn( oCol )
   NEXT

   ::lNoPaint := .F.
   ::ResetVScroll( .T. )

   IF ::lPainted
      ::GoTop()
      ::Refresh()
   ENDIF

RETURN Self

* ============================================================================
* METHOD TSBrowse:SetBtnGet() Version 9.0 Nov/30/2009
* ============================================================================

METHOD SetBtnGet( nColumn, cResName, bAction, nBmpWidth ) CLASS TSBrowse

   DEFAULT nBmpWidth := 16

   nColumn := iif( ValType( nColumn ) == "C", ::nColumn( nColumn ), nColumn )

   IF nColumn == NIL .OR. nColumn > Len( ::aColumns ) .OR. nColumn <= 0
      RETURN Self
   ENDIF

   ::aColumns[ nColumn ]:cResName := cResName
   ::aColumns[ nColumn ]:bAction := bAction
   ::aColumns[ nColumn ]:nBmpWidth := nBmpWidth
   ::aColumns[ nColumn ]:lBtnGet := .T.

RETURN Self

* ============================================================================
* METHOD TSBrowse:SetColMsg() Version 9.0 Nov/30/2009
* ============================================================================

METHOD SetColMsg( cMsg, cEditMsg, nCol ) CLASS TSBrowse

   IF Empty( ::aColumns ) .OR. ( cMsg == NIL .AND. cEditMsg == Nil )
      RETURN Self
   ENDIF

   IF nCol == NIL
      AEval( ::aColumns, {| e | iif( cMsg != NIL, e:cMsg := cMsg, Nil ), iif( cEditMsg != NIL, e:cMsgEdit := cEditMsg, ;
         Nil ) } )
   ELSE
      IF cMsg != NIL
         ::aColumns[ nCol ]:cMsg := cMsg
      ENDIF

      IF cEditMsg != NIL
         ::aColumns[ nCol ]:cMsgEdit := cEditMsg
      ENDIF
   ENDIF

RETURN Self

* ============================================================================
* METHOD TSBrowse:SetColor() Version 9.0 Nov/30/2009
* ============================================================================

METHOD SetColor( xColor1, xColor2, nColumn ) CLASS TSBrowse

   LOCAL nEle, nI, nColor

   DEFAULT xColor1 := { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20 }, ;
      nColumn := 0, ;
      ::lTransparent := .F.

   IF ( Empty( ::aColumns ) .AND. nColumn > 0 )
      RETURN NIL
   ENDIF

   IF ValType( xColor1 ) == "A" .AND. ValType( xColor2 ) == "A" .AND. Len( xColor1 ) > Len( xColor2 )
      RETURN NIL
   ENDIF

   IF ValType( xColor1 ) == "N" .AND. ValType( xColor2 ) == "N" .AND. nColumn == 0
      RETURN ::SetColor( xColor1, xColor2 ) // only nClrText and nClrPane
   ENDIF

   IF Len( ::aColumns ) == 0 .AND. ! ::lTransparent .AND. ::hBrush == NIL
      nColor := iif( ValType( xColor2[ 2 ] ) == "B", Eval( xColor2[ 2 ], 1, 1, Self ), xColor2[ 2 ] )
      ::hBrush := CreateSolidBrush( GetRed( nColor ), GetGreen( nColor ), GetBlue( nColor ) )
   ENDIF

   IF nColumn == 0 .AND. ValType( xColor2[ 1 ] ) == "N" .AND. ValType( xColor1 ) == "A" .AND. xColor1[ 1 ] == 1 .AND. ;
         Len( xColor1 ) > 1 .AND. ValType( xColor2 ) == "A" .AND. ValType( xColor2[ 2 ] ) == "N" .AND. xColor1[ 2 ] == 2
      nColor := iif( ValType( xColor2[ 2 ] ) == "B", Eval( xColor2[ 2 ], 1, 1, Self ), xColor2[ 2 ] )
      ::Super:SetColor( xColor2[ 1 ], nColor )
   ENDIF

   IF ValType( xColor1 ) == "N"
      xColor1 := { xColor1 }
      xColor2 := { xcolor2 }
   ENDIF

   FOR nEle := 1 TO Len( xColor1 )

      DO CASE

      CASE xColor1[ nEle ] == 1
         IF nColumn == 0

            FOR nI := 1 TO Len( ::aColumns )
               ::aColumns[ nI ]:nClrFore := xColor2[ nEle ]
            NEXT

            IF Empty( ::aColumns )
               ::nClrText := xColor2[ nEle ]
            ENDIF

         ELSE
            ::aColumns[ nColumn ]:nClrFore := xColor2[ nEle ]
         ENDIF

      CASE xColor1[ nEle ] == 2
         IF nColumn == 0

            FOR nI := 1 TO Len( ::aColumns )
               ::aColumns[ nI ]:nClrBack := xColor2[ nEle ]
            NEXT

            IF Empty( ::aColumns )
               ::nClrPane := xColor2[ nEle ]
            ENDIF

         ELSE
            ::aColumns[ nColumn ]:nClrBack := xColor2[ nEle ]
         ENDIF

      CASE xColor1[ nEle ] == 3
         IF nColumn == 0

            FOR nI := 1 TO Len( ::aColumns )
               ::aColumns[ nI ]:nClrHeadFore := xColor2[ nEle ]
            NEXT

            IF Empty( ::aColumns )
               ::nClrHeadFore := xColor2[ nEle ]
            ENDIF

         ELSE
            ::aColumns[ nColumn ]:nClrHeadFore := xColor2[ nEle ]
         ENDIF

      CASE xColor1[ nEle ] == 4
         IF nColumn == 0

            FOR nI := 1 TO Len( ::aColumns )
               ::aColumns[ nI ]:nClrHeadBack := xColor2[ nEle ]
            NEXT

            IF Empty( ::aColumns )
               ::nClrHeadBack := xColor2[ nEle ]
            ENDIF

         ELSE
            ::aColumns[ nColumn ]:nClrHeadBack := xColor2[ nEle ]
         ENDIF

      CASE xColor1[ nEle ] == 5
         IF nColumn == 0

            FOR nI := 1 TO Len( ::aColumns )
               ::aColumns[ nI ]:nClrFocuFore := xColor2[ nEle ]
            NEXT

            IF Empty( ::aColumns )
               ::nClrFocuFore := xColor2[ nEle ]
            ENDIF

         ELSE
            ::aColumns[ nColumn ]:nClrFocuFore := xColor2[ nEle ]
         ENDIF

      CASE xColor1[ nEle ] == 6
         IF nColumn == 0

            FOR nI := 1 TO Len( ::aColumns )
               ::aColumns[ nI ]:nClrFocuBack := xColor2[ nEle ]
            NEXT

            IF Empty( ::aColumns )
               ::nClrFocuBack := xColor2[ nEle ]
            ENDIF

         ELSE
            ::aColumns[ nColumn ]:nClrFocuBack := xColor2[ nEle ]
         ENDIF

      CASE xColor1[ nEle ] == 7
         IF nColumn == 0

            FOR nI := 1 TO Len( ::aColumns )
               ::aColumns[ nI ]:nClrEditFore := xColor2[ nEle ]
            NEXT

            IF Empty( ::aColumns )
               ::nClrEditFore := xColor2[ nEle ]
            ENDIF

         ELSE
            ::aColumns[ nColumn ]:nClrEditFore := xColor2[ nEle ]
         ENDIF

      CASE xColor1[ nEle ] == 8
         IF nColumn == 0

            FOR nI := 1 TO Len( ::aColumns )
               ::aColumns[ nI ]:nClrEditBack := xColor2[ nEle ]
            NEXT

            IF Empty( ::aColumns )
               ::nClrEditBack := xColor2[ nEle ]
            ENDIF

         ELSE
            ::aColumns[ nColumn ]:nClrEditBack := xColor2[ nEle ]
         ENDIF

      CASE xColor1[ nEle ] == 9
         IF nColumn == 0

            FOR nI := 1 TO Len( ::aColumns )
               ::aColumns[ nI ]:nClrFootFore := xColor2[ nEle ]
            NEXT

            IF Empty( ::aColumns )
               ::nClrFootFore := xColor2[ nEle ]
            ENDIF

         ELSE
            ::aColumns[ nColumn ]:nClrFootFore := xColor2[ nEle ]
         ENDIF

      CASE xColor1[ nEle ] == 10
         IF nColumn == 0

            FOR nI := 1 TO Len( ::aColumns )
               ::aColumns[ nI ]:nClrFootBack := xColor2[ nEle ]
            NEXT

            IF Empty( ::aColumns )
               ::nClrFootBack := xColor2[ nEle ]
            ENDIF

         ELSE
            ::aColumns[ nColumn ]:nClrFootBack := xColor2[ nEle ]
         ENDIF

      CASE xColor1[ nEle ] == 11
         IF nColumn == 0

            FOR nI := 1 TO Len( ::aColumns )
               ::aColumns[ nI ]:nClrSeleFore := xColor2[ nEle ]
            NEXT

            IF Empty( ::aColumns )
               ::nClrSeleFore := xColor2[ nEle ]
            ENDIF

         ELSE
            ::aColumns[ nColumn ]:nClrSeleFore := xColor2[ nEle ]
         ENDIF

      CASE xColor1[ nEle ] == 12
         IF nColumn == 0

            FOR nI := 1 TO Len( ::aColumns )
               ::aColumns[ nI ]:nClrSeleBack := xColor2[ nEle ]
            NEXT

            IF Empty( ::aColumns )
               ::nClrSeleBack := xColor2[ nEle ]
            ENDIF

         ELSE
            ::aColumns[ nColumn ]:nClrSeleBack := xColor2[ nEle ]
         ENDIF

      CASE xColor1[ nEle ] == 13
         IF nColumn == 0

            FOR nI := 1 TO Len( ::aColumns )
               ::aColumns[ nI ]:nClrOrdeFore := xColor2[ nEle ]
            NEXT

            IF Empty( ::aColumns )
               ::nClrOrdeFore := xColor2[ nEle ]
            ENDIF

         ELSE
            ::aColumns[ nColumn ]:nClrOrdeFore := xColor2[ nEle ]
         ENDIF

      CASE xColor1[ nEle ] == 14
         IF nColumn == 0

            FOR nI := 1 TO Len( ::aColumns )
               ::aColumns[ nI ]:nClrOrdeBack := xColor2[ nEle ]
            NEXT

            IF Empty( ::aColumns )
               ::nClrOrdeBack := xColor2[ nEle ]
            ENDIF

         ELSE
            ::aColumns[ nColumn ]:nClrOrdeBack := xColor2[ nEle ]
         ENDIF

      CASE xColor1[ nEle ] == 16
         IF nColumn == 0
            FOR nI := 1 TO Len( ::aSuperHead )
               ::aSuperHead[ nI, 5 ] := xColor2[ nEle ]
            NEXT

         ELSE
            ::aSuperHead[ nColumn, 5 ] := xColor2[ nEle ]
         ENDIF

      CASE xColor1[ nEle ] == 17
         IF nColumn == 0

            FOR nI := 1 TO Len( ::aSuperHead )
               ::aSuperHead[ nI, 4 ] := xColor2[ nEle ]
            NEXT

         ELSE
            ::aSuperHead[ nColumn, 4 ] := xColor2[ nEle ]
         ENDIF

      CASE xColor1[ nEle ] == 18
         IF nColumn == 0

            FOR nI := 1 TO Len( ::aColumns )
               ::aColumns[ nI ]:nClrSpcHdFore := xColor2[ nEle ]
            NEXT
            IF Empty( ::aColumns )
               ::nClrSpcHdFore := xColor2[ nEle ]
            ENDIF

         ELSE
            ::aColumns[ nColumn ]:nClrSpcHdFore := xColor2[ nEle ]
         ENDIF

      CASE xColor1[ nEle ] == 19
         IF nColumn == 0

            FOR nI := 1 TO Len( ::aColumns )
               ::aColumns[ nI ]:nClrSpcHdBack := xColor2[ nEle ]
            NEXT

            IF Empty( ::aColumns )
               ::nClrSpcHdBack := xColor2[ nEle ]
            ENDIF

         ELSE
            ::aColumns[ nColumn ]:nClrSpcHdBack := xColor2[ nEle ]
         ENDIF

      CASE xColor1[ nEle ] == 20

         IF nColumn == 0

            FOR nI := 1 TO Len( ::aColumns )
               ::aColumns[ nI ]:nClrSpcHdActive := xColor2[ nEle ]
            NEXT

            IF Empty( ::aColumns )
               ::nClrSpcHdActive := xColor2[ nEle ]
            ENDIF

         ELSE
            ::aColumns[ nColumn ]:nClrSpcHdActive := xColor2[ nEle ]
         ENDIF

      OTHERWISE
         ::nClrLine := xColor2[ nEle ]
      ENDCASE

   NEXT

   IF ::lPainted
      ::Refresh( .T. )
   ENDIF

RETURN 0

* ============================================================================
* METHOD TSBrowse:SetColumns() Version 9.0 Nov/30/2009
* ============================================================================

METHOD SetColumns( aData, aHeaders, aColSizes ) CLASS TSBrowse

   LOCAL aFields, nElements, n

   nElements := Len( aData )

   ::aHeaders := iif( aHeaders != NIL, aHeaders, ::aHeaders )
   ::aColSizes := iif( aColSizes != NIL, aColSizes, {} )
   ::bLine := {|| _aData( aData ) }
   ::aJustify := AFill( Array( nElements ), .F. )

   IF Len( ::GetColSizes() ) < nElements
      ::aColSizes := AFill( Array( nElements ), 0 )
      aFields := Eval( ::bLine )

      FOR n := 1 TO nElements
         ::aColSizes[ n ] := iif( ValType( aFields[ n ] ) != "C", 16, ; // Bitmap handle
         GetTextWidth( 0, Replicate( "B", Max( Len( ::aHeaders[ n ] ), ;
            Len( aFields[ n ] ) ) + 1 ), ;
            iif( ! Empty( ::hFont ), ::hFont, 0 ) ) )
      NEXT
   ENDIF

   IF ::oHScroll != NIL
      ::oHScroll:nMax := ::GetColSizes()
   ENDIF

RETURN Self

* ============================================================================
* METHOD TSBrowse:SetColSize() Version 9.0 Nov/30/2009
* ============================================================================

METHOD SetColSize( nCol, nWidth ) CLASS TSBrowse

   LOCAL nI, nSize

   IF ValType( nCol ) == "A"
      FOR nI := 1 TO Len( nCol )
         nSize := iif( ValType( nWidth ) == "A", nWidth[ nI ], nWidth )
         ::aColumns[ nCol[ nI ] ]:nWidth := nSize
         ::aColSizes[ nCol[ nI ] ] := iif( ::aColumns[ nCol[ nI ] ]:lVisible, ::aColumns[ nCol[ nI ] ]:nWidth, 0 )
      NEXT
   ELSE
      IF ValType( nCol ) == "C" // 14.07.2015
         nCol := AScan( ::aColumns, {| oCol | Upper( oCol:cName ) == Upper( nCol ) } )
      ENDIF
      ::aColumns[ nCol ]:nWidth := nWidth
      ::aColSizes[ nCol ] := iif( ::aColumns[ nCol ]:lVisible, ::aColumns[ nCol ]:nWidth, 0 )
   ENDIF

   IF ::lPainted
      ::Refresh( .T. )
   ENDIF

RETURN Self

* ============================================================================
* METHOD TSBrowse:SetData() Version 9.0 Nov/30/2009
* ============================================================================

METHOD SetData( nColumn, bData, aList ) CLASS TSBrowse

   IF ValType( nColumn ) == "C"
      nColumn := ::nColumn( nColumn ) // 21.07.2015
   ENDIF

   IF ValType( nColumn ) != "N" .OR. nColumn <= 0
      RETURN NIL
   ENDIF

   IF aList != NIL

      IF ValType( aList[ 1 ] ) == "A"
         ::aColumns[ nColumn ]:aItems := aList[ 1 ]
         ::aColumns[ nColumn ]:aData := aList[ 2 ]
         ::aColumns[ nColumn ]:cDataType := ValType( aList[ 2, 1 ] )
      ELSE
         ::aColumns[ nColumn ]:aItems := aList
      ENDIF

      ::aColumns[ nColumn ]:lComboBox := .T.

   ENDIF

   IF bData != NIL

      IF ValType( bData ) == "B"
         ::aColumns[ nColumn ]:bData := bData
      ELSE
         ::aColumns[ nColumn ]:bData := {|| ( bData ) }
      ENDIF

   ENDIF

RETURN Self

* ============================================================================
* METHOD TSBrowse:SetDbf() Version 9.0 Nov/30/2009
* ============================================================================

METHOD SetDbf( cAlias ) CLASS TSBrowse

   LOCAL cAdsKeyNo, cAdsKeyCount, nTags, nEle

   DEFAULT ::cAlias := cAlias
   DEFAULT ::cAlias := Alias()

   IF Empty( ::cAlias )
      RETURN NIL
   ENDIF

   cAlias := ::cAlias
   ::cDriver := ( cAlias )->( rddName() )

   DEFAULT ::bGoTop := {|| ( cAlias )->( dbGoTop() ) }, ;
      ::bGoBottom := {|| ( cAlias )->( dbGoBottom() ) }, ;
      ::bSkip := {| n | iif( n == NIL, n := 1, Nil ), ::DbSkipper( n ) }, ;
      ::bBof := {|| ( cAlias )->( Bof() ) }, ;
      ::bEof := {|| ( cAlias )->( Eof() ) }

   IF "ADS" $ ::cDriver
      cAdsKeyNo := "{| n, oBrw | iif( n == Nil, Round( " + cAlias + "->( ADSGetRelKeyPos() ) * oBrw:nLen, 0 ), " + ;
         cAlias + "->( ADSSetRelKeyPos( n / oBrw:nLen ) ) ) }"

      cAdsKeyCount := "{|cTag| " + cAlias + "->( ADSKeyCount(cTag,, 1 ) ) }"

      DEFAULT ::bKeyNo := &cAdsKeyNo, ;
         ::bKeyCount := &cAdsKeyCount, ;
         ::bLogicLen := &cAdsKeyCount, ;
         ::bTagOrder := {| uTag | ( cAlias )->( ordSetFocus( uTag ) ) }, ;
         ::bGoToPos := {| n | Eval( ::bKeyNo, n, Self ) }
   ELSE
      DEFAULT ::bKeyNo := {| n | ( cAlias )->( iif( n == NIL, iif( IndexOrd() > 0, ordKeyNo(), RecNo() ), ;
         iif( IndexOrd() > 0, ordKeyGoto( n ), dbGoto( n ) ) ) ) }, ;
         ::bKeyCount := {|| ( cAlias )->( iif( IndexOrd() > 0, ordKeyCount(), LastRec() ) ) }, ;
         ::bLogicLen := {|| ( cAlias )->( iif( IndexOrd() == 0, LastRec(), ordKeyCount() ) ) }, ;
         ::bTagOrder := {| uTag | ( cAlias )->( ordSetFocus( uTag ) ) }, ;
         ::bGoToPos := {| n | Eval( ::bKeyNo, n ) }
      ::lShared := ( cAlias )->( dbInfo( DBI_SHARED ) ) /* is the file opened shared ? */
   ENDIF

   nTags := ( cAlias )->( ordCount() )
   ::aTags := {}

   FOR nEle := 1 TO nTags
      AAdd( ::aTags, { ( cAlias )->( ordName( nEle ) ), ( cAlias )->( ordKey( nEle ) ) } )
   NEXT
   IF "SQL" $ ::cDriver
      Eval( ::bGoToPos, 100 )
      ::bGoBottom := {|| CursorWait(), ( cAlias )->( dbGoBottom() ), CursorArrow() }
      ::bRecLock := {|| .T. }
   ENDIF

   ::nLen := Eval( ::bLogicLen )
   ::ResetVScroll( .T. )

RETURN Self

* ============================================================================
* METHOD TSBrowse:SetDeleteMode()  Version 9.0 Nov/30/2009
* ============================================================================

METHOD SetDeleteMode( lOnOff, lConfirm, bDelete, bPostDel ) CLASS TSBrowse

   DEFAULT lOnOff := .T., ;
      lConfirm := .T.

   ::lCanDelete := lOnOff
   ::lConfirm := lConfirm
   ::bDelete := bDelete
   ::bPostDel := bPostDel

RETURN Self

* ============================================================================
* METHOD TSBrowse:SetFilter() Version 7.0 Jul/15/2004
* ============================================================================

METHOD SetFilter( cField, uVal1, uVal2 ) CLASS TSBrowse

   LOCAL cIndexType, cAlias, ;
      lWasFiltered := ::lFilterMode

   DEFAULT uVal2 := uVal1

   IF ValType( uVal2 ) == "A"
      ::bFilter := uVal2[ 2 ]
      uVal2 := uVal2[ 1 ]
   ENDIF

   ::cField := cField
   ::uValue1 := uVal1
   ::uValue2 := uVal2
   ::lFilterMode := ! Empty( cField )

   cAlias := ::cAlias

   IF ::lFilterMode
      ::lDescend := ( ::uValue2 < ::uValue1 )

      cIndexType := ( cAlias )->( ValType( &( IndexKey() ) ) )

      IF ( ::cAlias )->( ValType( &cField ) ) != cIndexType .OR. ;
            ValType( uVal1 ) != cIndexType .OR. ;
            ValType( uVal2 ) != cIndexType

         MsgInfo( ::aMsg[ 27 ], ::aMsg[ 28 ] )

         ::lFilterMode := .F.

      ENDIF

   ENDIF

#ifdef _TSBFILTER7_
   // Posibility of using FILTERs based on INDEXES!!!

   ::bGoTop := iif( ::lFilterMode, {|| BrwGoTop( Self ) }, ;
      {|| ( cAlias )->( dbGoTop() ) } )

   ::bGoBottom := iif( ::lFilterMode, {|| BrwGoBottom( uVal2, Self ) }, ;
      {|| ( cAlias )->( dbGoBottom() ) } )

   ::bSkip := iif( ::lFilterMode, BuildSkip( ::cAlias, cField, uVal1, uVal2, Self ), ;
      {| n | ::dbSkipper( n ) } )

#else
   IF ::lFilterMode
      ( ::cAlias )->( ordScope( 0, ::uValue1 ) )
      ( ::cAlias )->( ordScope( 1, ::uValue2 ) )
   ELSE
      ( ::cAlias )->( ordScope( 0, Nil ) )
      ( ::cAlias )->( ordScope( 1, Nil ) )
   ENDIF
#endif
   IF ::bLogicLen != NIL
      ::nLen := iif( ::lIsDbf, ( ::cAlias )->( Eval( ::bLogicLen ) ), ;
         Eval( ::bLogicLen ) )
   ENDIF

   ::ResetVScroll( .T. )
   ::lHitTop := .F.
   ::lHitBottom := .F.

   IF ::lFilterMode .OR. lWasFiltered
      ::GoTop()
   ENDIF

RETURN NIL

* ============================================================================
* METHOD TSBrowse:FilterData()  by SergKis
* ============================================================================

METHOD FilterData( cFilter, lBottom, lFocus ) CLASS TSBrowse

   LOCAL nLen := 0, cAlias := ::cAlias

   IF ! Empty( cFilter )
      ( cAlias )->( dbSetFilter( &( "{||" + cFilter + "}" ), cFilter ) )
   ELSE
      ( cAlias )->( dbClearFilter() )
   ENDIF

   ( cAlias )->( dbGoTop() )
   DO WHILE ( cAlias )->( ! Eof() )
      SysRefresh()
      nLen++
      ( cAlias )->( dbSkip( 1 ) )
   ENDDO

   ::bLogicLen := {|| nLen }

   ::lInitGoTop := .T.
   ::Reset( lBottom )

   IF ! Empty( lFocus )
      ::SetFocus()
   ENDIF

RETURN NIL

* ============================================================================
* METHOD TSBrowse:FilterFTS()  by SergKis
* ============================================================================

METHOD FilterFTS( cFind, lUpper, lBottom, lFocus, lAll ) CLASS TSBrowse

   LOCAL nLen := 0, cAlias := ::cAlias, ob := Self
   LOCAL aArray, aLine, nLine, nCol, oCol, xVal, lRet
   LOCAL nAtPos, nLastPos, aFind, nFind := 0
   DEFAULT lUpper := .T., lAll := .F.

   IF !HB_ISCHAR( cFind ) .or. Len( cFind ) == 0
      RETURN nFind
   ENDIF

   IF lUpper
      cFind := Upper( cFind )
   ENDIF

   IF Left( cFind, 1 ) == " " 
      aFind := hb_ATokens( substr( cFind, 2 ) )
   ELSE
      aFind := { cFind }
   ENDIF

   IF ::lIsDbf
      IF ! Empty( cFind )
         ( cAlias )->( dbSetFilter( {|| ob:FilterFTS_Line( cFind, lUpper, lAll, ob ) }, ;
            "ob:FilterFTS_Line( cFind, lUpper, lAll, ob)" ) )
      ELSE
         ( cAlias )->( dbClearFilter() )
      ENDIF

      ( cAlias )->( dbGoTop() )
      DO While !( cAlias )->( Eof() )
         SysRefresh()
         nLen++
         ( cAlias )->( dbSkip( 1 ) )
      ENDDO

      nFind := nLen

      ::bLogicLen := {|| nLen }

      ::lInitGoTop := .T.

      ::Reset( lBottom )

   ELSEIF ::lIsArr .AND. ! Empty( cFind )
      IF ::aArray_FTS == NIL
         ::aArray_FTS := ::aArray
      ELSE
         ::aArray := ::aArray_FTS
      ENDIF
      nAtPos   := ::nAt
      nLastPos := ::nLastPos
      aArray   := {}
      FOR EACH aLine IN ::aArray
          nLine := hb_enumindex( aLine )
          ::nAt := nLine
          FOR EACH oCol IN ::aColumns
              nCol := hb_enumindex( oCol )
              IF nCol == 1 .AND. ::lSelector ; LOOP
              ELSEIF !Empty(oCol:cName) .AND. oCol:cName == "ARRAYNO" ; LOOP
              ELSEIF ! oCol:lVisible ; LOOP
              ELSEIF oCol:lBitMap ; LOOP
              ENDIF
              xVal := ob:bDataEval( oCol, , nCol )
              IF lAll .AND. ! HB_ISCHAR( xVal )
                 IF HB_ISLOGICAL( xVal )
                    xVal := iif( xVal, ".T.", ".F." )
                 ELSE
                    xVal := cValToChar( xVal )
                 ENDIF
              ENDIF
              IF HB_ISCHAR( xVal )
                 FOR EACH cFind IN aFind
                     IF lUpper
                        lRet := cFind $ Upper( xVal )
                     ELSE
                        lRet := cFind $ xVal
                     ENDIF
                     IF !lRet
                        EXIT
                     ENDIF
                 NEXT
                 IF lRet
                    AAdd( aArray, aLine )
                    EXIT
                 ENDIF
              ENDIF
          NEXT
          ::Skip()
      NEXT
      ::nAt      := nAtPos
      ::nLastPos := nLastPos
      IF ( nFind := Len( aArray ) ) > 0
         ::aArray := aArray
         ::Reset( lBottom )
      ELSEIF IsArray( ::aArray_FTS )
         ::aArray := { Array( Len( ::aArray_FTS[1] ) ) }
         ::Reset( lBottom )
      ENDIF

   ELSEIF ::lIsArr .AND. Empty( cFind )
      IF IsArray( ::aArray_FTS )
         ::aArray := ::aArray_FTS
         ::aArray_FTS := NIL
      ENDIF
      nFind := Len( ::aArray )
      ::Reset( lBottom )

   ENDIF

   IF ! Empty( lFocus )
      ::SetFocus()
   ENDIF

RETURN nFind

* ============================================================================
* METHOD TSBrowse:FilterFTS_Line()  by SergKis
* ============================================================================

METHOD FilterFTS_Line( cFind, lUpper, lAll ) CLASS TSBrowse

   LOCAL nCol, oCol, xVal, lRet := .F., aFind
   DEFAULT lUpper := .T., lAll := .F.

   IF Left( cFind, 1 ) == " "
      aFind := hb_ATokens( substr( cFind, 2 ) )
   ELSE
      aFind := { cFind }
   ENDIF

   FOR nCol := 1 TO Len( ::aColumns )
      oCol := ::aColumns[ nCol ]
      IF nCol == 1 .AND. ::lSelector ; LOOP
      ELSEIF !Empty(oCol:cName) .AND. oCol:cName == "ORDKEYNO" ; LOOP
      ELSEIF ! oCol:lVisible ; LOOP
      ELSEIF oCol:lBitMap ; LOOP
      ENDIF
      xVal := ::bDataEval( oCol, , nCol )
      IF lAll .AND. ! HB_ISCHAR( xVal )
         IF HB_ISLOGICAL( xVal )
            xVal := iif( xVal, ".T.", ".F." )
         ELSE
            xVal := cValToChar( xVal )
         ENDIF
      ENDIF
      IF HB_ISCHAR( xVal )
         FOR EACH cFind IN aFind
             IF lUpper
                lRet := cFind $ Upper( xVal )
             ELSE
                lRet := cFind $ xVal
             ENDIF
             IF !lRet
                EXIT
             ENDIF
         NEXT
         IF lRet
            EXIT
         ENDIF
      ENDIF
   NEXT

RETURN lRet

* ============================================================================
* METHOD TSBrowse:CalcTotal()  by SergKis
* ============================================================================

METHOD CalcTotal( cTotal, cNoTotal, lDraw, lPicture ) CLASS TSBrowse
   LOCAL nCols := Len( ::aColumns )
   LOCAL lToT := .F., cToT, aSum := Array( nCols )
   LOCAL lNoT := .F., cNoT, aToT := Array( nCols )
   LOCAL nSum := 0, nRec, oCol, nI, nK
   LOCAL cNam, xVal, nAtPos, nLastPos

   IF ISLOGICAL( cNoTotal )
      lDraw    := cNoTotal
      cNoTotal := NIL
   ELSEIF ISLOGICAL( cTotal )
      lDraw  := cTotal
      cTotal := NIL
   ENDIF

   DEFAULT lDraw := .T., lPicture := .F.

   aFill( aSum, 0 )
   aFill( aToT, .F. )

   IF IsArray( cTotal )
      cToT := ""
      nK := Len( cTotal )
      FOR nI := 1 TO nK
          IF !Empty( cTotal[ nI ] )
             cToT += cTotal[ nI ] + iif( nI < nK, ",", "" )
          ENDIF
      NEXT
      cTotal := cToT
      cToT := NIL
   ENDIF

   IF IsArray( cNoTotal )
      cNoT := ""
      nK := Len( cNoTotal )
      FOR nI := 1 TO nK
          IF !Empty( cNoTotal[ nI ] )
             cNoT += cNoTotal[ nI ] + iif( nI < nK, ",", "" )
          ENDIF
      NEXT
      cNoTotal := cNoT
      cNoT := NIL
   ENDIF
   
   IF IsChar( cTotal )
      cToT := Upper( StrTran( cTotal, " ", "" ) )
      IF lToT := ( !Empty( cToT ) .AND. IsChar( cToT ) )
         cToT := "," + Upper( cToT ) + ","
      ENDIF
   ENDIF
   IF IsChar( cNoTotal )
      cNoT := Upper( StrTran( cNoTotal, " ", "" ) )
      IF lNoT := ( !Empty( cNoT ) .AND. IsChar( cNoT ) )
         cNoT := "," + Upper( cNoT ) + ","
      ENDIF
   ENDIF

   FOR nK := 1 TO nCols
       oCol := ::aColumns[nK]
       IF !oCol:lVisible ; LOOP
       ENDIF
       cNam := "," + Upper( oCol:cName ) + ","
       IF cNam $ ",SELECTOR,ORDKEYNO,ARRAYNO," ; LOOP
       ENDIF
       IF lNoT .AND.  cNam $ cNoT ; LOOP
       ENDIF
       IF lToT .AND. !cNam $ cToT ; LOOP
       ENDIF
       xVal := ::bDataEval( oCol, , nK )
       IF IsNumeric( xVal )
          aToT[nK] := .T.
          nSum ++
       ENDIF
   NEXT

   IF nSum > 0
      nAtPos   := ::nAt
      nLastPos := ::nLastPos

      IF ::lIsDbf
         nRec := ( ::cAlias )->( RecNo() )
         ( ::cAlias )->( dbGoTop() )
      ENDIF

      FOR nI := 1 TO ::nLen
          ::nAt := nI
          FOR nK := 1 TO Len( ::aColumns )
              IF !aToT[nK] ; LOOP
              ENDIF
              oCol := ::aColumns[nK]
              xVal := ::bDataEval( oCol, , nK )
              IF IsNumeric( xVal ) .AND. !Empty( xVal ) ; aSum[nK] += xVal
              ENDIF
          NEXT
          ::Skip()
          DO EVENTS
      NEXT

      IF ::lIsDbf
         ( ::cAlias )->( dbGoTo( nRec ) )
         DO EVENTS
      ENDIF

      ::nAt      := nAtPos
      ::nLastPos := nLastPos

      IF lDraw
         FOR nK := 1 TO nCols
             oCol := ::aColumns[nK]
             IF !aTot[nK] ; LOOP
             ENDIF
             IF Empty( aSum[nK] )
                xVal := ""
             ELSEIF lPicture .AND. !Empty( oCol:cPicture )
                xVal := AllTrim( Transform( aSum[nK], oCol:cPicture ) )
             ELSE
                xVal := hb_ntos( aSum[nK] )
             ENDIF
             oCol:cFooting := xVal
         NEXT
         ::Refresh( .F. )
         DO EVENTS
      ENDIF
   ENDIF

RETURN aSum

* ============================================================================
* METHOD TSBrowse:SetFont() Version 7.0 Jul/15/2004
* ============================================================================

METHOD SetFont( hFont ) CLASS TSBrowse

   IF hFont != NIL
      ::hFont := hFont
      SendMessage( ::hWnd, WM_SETFONT, hFont )
   ENDIF

RETURN 0

* ============================================================================
* METHOD TSBrowse:SetHeaders() Version 9.0 Nov/30/2009
* ============================================================================

METHOD SetHeaders( nHeight, aCols, aTitles, aAlign, al3DLook, aFonts, aActions ) CLASS TSBrowse

   LOCAL nI

   IF nHeight != NIL
      ::nHeightHead := nHeight
   ENDIF

   IF aCols == NIL

      aCols := {}

      DO CASE

      CASE ValType( aTitles ) == "A"
         FOR nI := 1 TO Len( aTitles )
            AAdd( aCols, nI )
         NEXT

      CASE ValType( aActions ) == "A"
         FOR nI := 1 TO Len( aActions )
            AAdd( aCols, nI )
         NEXT

      CASE ValType( aAlign ) == "A"
         FOR nI := 1 TO Len( aAlign )
            AAdd( aCols, nI )
         NEXT

      CASE ValType( al3DLook ) == "A"
         FOR nI := 1 TO Len( al3DLook )
            AAdd( aCols, nI )
         NEXT

      CASE ValType( aFonts ) == "A"
         FOR nI := 1 TO Len( aFonts )
            AAdd( aCols, nI )
         NEXT

      OTHERWISE
         RETURN NIL
      ENDCASE

   ENDIF

   FOR nI := 1 TO Len( aCols )

      IF aTitles != NIL
         ::aColumns[ aCols[ nI ] ]:cHeading := aTitles[ nI ]
      ENDIF

      IF aAlign != NIL
         ::aColumns[ aCols[ nI ] ]:nHAlign := iif( ValType( aAlign ) == "A", aAlign[ nI ], aAlign )
      ENDIF

      IF al3DLook != NIL
         ::aColumns[ aCols[ nI ] ]:l3DLookHead := iif( ValType( al3DLook ) == "A", al3DLook[ nI ], al3DLook )
      ENDIF

      IF aFonts != NIL
         ::aColumns[ aCols[ nI ] ]:hFontHead := iif( ValType( aFonts ) == "A", aFonts[ nI ], aFonts )
      ENDIF

      IF aActions != NIL
         ::aColumns[ aCols[ nI ] ]:bAction := aActions[ nI ]
      ENDIF

   NEXT

   ::DrawHeaders()

RETURN Self

* ============================================================================
* METHOD TSBrowse:SetIndexCols() Version 9.0 Nov/30/2009
* ============================================================================

METHOD SetIndexCols( nCol1, nCol2, nCol3, nCol4, nCol5 ) CLASS TSBrowse

   LOCAL aCol

   DEFAULT nCol2 := 0, ;
      nCol3 := 0, ;
      nCol4 := 0, ;
      nCol5 := 0

   IF ValType( nCol1 ) == "A"
      AEval( nCol1, {| nCol | ::aColumns[ nCol ]:lIndexCol := .T. } )
   ELSE
      aCol := { nCol1, nCol2, nCol3, nCol4, nCol5 }
      AEval( aCol, {| nCol | iif( nCol > 0, ::aColumns[ nCol ]:lIndexCol := .T., Nil ) } )
   ENDIF

RETURN NIL

* ============================================================================
* METHOD TSBrowse:OnReSize()  by SergKis
* ============================================================================

METHOD OnReSize( nWidth, nHeight, lTop ) CLASS TSBrowse

   LOCAL nCnt, aCol, nCol, oCol, nKfc, lRet := .F.
   LOCAL nColMaxKfc := 0, nW, nS, nN
   LOCAL nTop := iif( Empty( lTop ), ::nTop, 0 )

   IF Empty( nWidth )
      nWidth := GetWindowWidth( ::hWnd )
   ENDIF

   IF Empty( nHeight )
      nHeight := GetWindowHeight( ::hWnd )
      lTop := .T.
      nTop := 0
   ENDIF

   IF _HMG_MouseState == 1

      aCol := Array( Len( ::aColumns ) )
      nW := _GetClientRect( ::hWnd )[ 3 ]

      IF ! ::lNoVScroll .AND. ::nLen > ::nRowCount()
         nW -= GetVScrollBarWidth()
      ENDIF

      FOR nCol := 1 TO Len( ::aColumns )
         oCol := ::aColumns[ nCol ]
         aCol[ nCol ] := 0
         IF nCol == 1 .AND. ::lSelector ; LOOP
         ELSEIF oCol:lBitMap ; LOOP
         ENDIF
         aCol[ nCol ] := oCol:nWidth / nW
      NEXT

      ::aOldParams[ 7 ] := AClone( aCol )

      IF ISBLOCK( ::bOnResizeEnter )
         Eval( ::bOnResizeEnter, Self )
      ENDIF

   ELSEIF _HMG_MouseState == 0 .AND. ISARRAY( ::aOldParams[ 7 ] )

      aCol := ::aOldParams[ 7 ]
      nCnt := Min( Len( aCol ), Len( ::aColumns ) )
      nKfc := 0
      lRet := .T.

      FOR nCol := 1 TO nCnt
         oCol := ::aColumns[ nCol ]
         IF nCol == 1 .AND. ::lSelector ; LOOP
         ELSEIF ! oCol:lVisible ; LOOP
         ELSEIF oCol:lBitMap ; LOOP
         ENDIF
         IF aCol[ nCol ] > nKfc
            nColMaxKfc := nCol
            nKfc := aCol[ nCol ]
         ENDIF
      NEXT

      ::lEnabled := .F.
      ::Move( ::nLeft, ::nTop, nWidth, nHeight - nTop, .T. )

      nW := _GetClientRect( ::hWnd )[ 3 ]
      nN := nS := 0

      IF ! ::lNoVScroll .AND. ::nLen > ::nRowCount()
         nW -= GetVScrollBarWidth()
      ENDIF

      FOR nCol := 1 TO nCnt
         oCol := ::aColumns[ nCol ]
         IF nCol == 1 .AND. ::lSelector ; LOOP
         ELSEIF ! oCol:lVisible ; LOOP
         ELSEIF oCol:lBitMap ; LOOP
         ENDIF
         nKfc := aCol[ nCol ]
         oCol:nWidth := Int( nKfc * nW )
         nS += oCol:nWidth
         nN := nCol
      NEXT

      nN := iif( nColMaxKfc > 0, nColMaxKfc, nN )
      ::aColumns[ nN ]:nWidth += ( nW - nS )
      ::lEnabled := .T.

      IF ISBLOCK( ::bOnResizeExit )
         Eval( ::bOnResizeExit, Self )
      ENDIF

      ::SetNoHoles()

   ENDIF

RETURN lRet

* ============================================================================
* METHOD TSBrowse:SetNoHoles() adjusts TBrowse height to the whole cells amount
* ============================================================================

METHOD SetNoHoles( nDelta, lSet ) CLASS TSBrowse

   LOCAL nH, nK, nHeight, nHole, nCol, oCol, aRect

   DEFAULT nDelta := 0, lSet := .T.

   IF ISARRAY( ::aOldParams ) .AND. Len( ::aOldParams ) > 4
      ::nHeightSuper := ::aOldParams[ 1 ]
      ::nHeightHead := ::aOldParams[ 2 ]
      ::nHeightSpecHd := ::aOldParams[ 3 ]
      ::nHeightCell := ::aOldParams[ 4 ]
      ::nHeightFoot := ::aOldParams[ 5 ]
   ENDIF

   nHole := _GetClientRect( ::hWnd )[ 4 ] - ;
      ::nHeightHead - ::nHeightSuper - ;
      ::nHeightFoot - ::nHeightSpecHd

   nHole -= ( Int( nHole / ::nHeightCell ) * ::nHeightCell )
   nHole -= nDelta
   nHeight := nHole

   IF lSet

      nH := iif( ::nHeightSuper > 0, 1, 0 ) + ;
         iif( ::nHeightHead > 0, 1, 0 ) + ;
         iif( ::nHeightSpecHd > 0, 1, 0 ) + ;
         iif( ::nHeightFoot > 0, 1, 0 )

      IF nH > 0

         nK := Int( nHole / nH )

         IF ::nHeightFoot > 0
            ::nHeightFoot += nK
            nHole -= nK
         ENDIF
         IF ::nHeightSuper > 0
            ::nHeightSuper += nK
            nHole -= nK
         ENDIF
         IF ::nHeightSpecHd > 0
            ::nHeightSpecHd += nK
            nHole -= nK
         ENDIF
         IF ::nHeightHead > 0
            ::nHeightHead += nHole
         ENDIF

      ELSE

         SetProperty( ::cParentWnd, ::cControlName, "Height", ;
            GetProperty( ::cParentWnd, ::cControlName, "Height" ) - nHole )

      ENDIF

      IF Empty( ::aOldParams )

         ::Display()

         aRect := _GetClientRect( ::hWnd )

         IF ! ::lNoVScroll .AND. ::nLen > ::nRowCount()
            aRect[ 3 ] -= GetVScrollBarWidth()
         ENDIF

         ::aOldParams := Array( 7 )
         ::aOldParams[ 1 ] := ::nHeightSuper
         ::aOldParams[ 2 ] := ::nHeightHead
         ::aOldParams[ 3 ] := ::nHeightSpecHd
         ::aOldParams[ 4 ] := ::nHeightCell
         ::aOldParams[ 5 ] := ::nHeightFoot
         ::aOldParams[ 6 ] := { aRect[ 3 ], aRect[ 4 ] } // client { width, height }
         ::aOldParams[ 7 ] := Array( Len( ::aColumns ) )

         FOR nCol := 1 TO Len( ::aColumns )
            ::aOldParams[ 7 ][ nCol ] := 0
            oCol := ::aColumns[ nCol ]
            IF nCol == 1 .AND. ::lSelector ; LOOP
            ELSEIF oCol:lBitMap ; LOOP
            ENDIF
            IF ! Empty( ::aOldParams[ 6 ][ 1 ] )
               ::aOldParams[ 7 ][ nCol ] := oCol:nWidth / ::aOldParams[ 6 ][ 1 ]
            ENDIF
         NEXT

      ELSE

         IF ::lEnabled
            ::lEnabled := .F.
         ENDIF
         ::Paint()
         ::lEnabled := .T.

         ::Refresh( .F. ,, .F. )

      ENDIF

   ENDIF

RETURN nHeight

* ============================================================================
* METHOD TSBrowse:SetOrder() Version 9.0 Nov/30/2009
* ============================================================================

METHOD SetOrder( nColumn, cPrefix, lDescend ) CLASS TSBrowse

   LOCAL nDotPos, cAlias, nRecNo, ;
      lReturn := .F., ;
      oColumn := ::aColumns[ nColumn ]

   DEFAULT ::lIsArr := AND( ::nDataType, DATATYPE_ARRAY ) != 0

   IF nColumn == NIL .OR. nColumn > Len( ::aColumns )
      RETURN .F.
   ENDIF

   ::lNoPaint := .F.

   IF ::lIsDbf .AND. ! Empty( oColumn:cOrder )

      IF nColumn == ::nColOrder .OR. oColumn:lDescend == NIL
         IF lDescend == NIL
            lDescend := iif( Empty( ::nColOrder ) .OR. oColumn:lDescend == NIL, .F., ! oColumn:lDescend )
         ENDIF
         IF oColumn:lNoDescend // SergKis addition
            lDescend := .F.
         ELSE
            ( ::cAlias )->( ordDescend( ,, lDescend ) )
         ENDIF
         oColumn:lDescend := lDescend
         ::nColOrder := nColumn
         ::lHitTop := ::lHitBottom := .F.
         ::nAt := ::nLastnAt := Eval( ::bKeyNo, NIL, Self )
      ENDIF

      cAlias := ::cAlias

      IF ::bKeyNo == NIL
         ::SetDbf()
      ENDIF

      IF ( nDotPos := At( ".", oColumn:cOrder ) ) > 0 // in case TAG has an extension (ie .NTX)
         oColumn:cOrder := SubStr( oColumn:cOrder, 1, nDotPos - 1 )
      ENDIF

      ::uLastTag := oColumn:cOrder

      ( cAlias )->( Eval( ::bTagOrder, oColumn:cOrder ) )

      IF Empty( ( cAlias )->( IndexKey() ) )
         ::cOrderType := ""
      ELSE
         ::cOrderType := ( cAlias )->( ValType( &( IndexKey() ) ) )
      ENDIF

      ::UpStable()
      ::ResetVScroll()
      ::nColOrder := nColumn
      ::ResetSeek()
      ::nAt := ::nLastnAt := Eval( ::bKeyNo, NIL, Self )
      ::HiLiteCell( nColumn )

      IF ::bSetOrder != NIL
         Eval( ::bSetOrder, Self, ( cAlias )->( Eval( ::bTagOrder ) ), nColumn )
      ENDIF

      lReturn := .T.

      IF cPrefix != NIL
         ::cPrefix := cPrefix
      ENDIF

      ::aColumns[ nColumn ]:lSeek := ::lSeek // GF 1.71

   ELSEIF ::lIsArr

      IF nColumn <= Len( ::aArray[ 1 ] ) .AND. oColumn:lIndexCol
         ::cOrderType := ValType( ::aArray[ ::nAt, nColumn ] )

         IF nColumn == ::nColOrder .OR. Empty( oColumn:cOrder ) .OR. oColumn:lDescend == NIL
            IF lDescend == NIL
               lDescend := iif( Empty( oColumn:cOrder ) .OR. oColumn:lDescend == NIL, .F., ! oColumn:lDescend )
            ENDIF
            IF oColumn:lNoDescend // SergKis addition
               lDescend := .F.
            ENDIF
            oColumn:lDescend := lDescend
            ::nColOrder := nColumn

            IF ::bSetOrder != NIL
               Eval( ::bSetOrder, Self, nColumn )
            ELSE
               ::SortArray( nColumn, lDescend )
            ENDIF

            IF ::lPainted
               ::UpAStable()
               ::Refresh()
               ::HiliteCell( nColumn )
            ENDIF

            ::ResetVScroll()
            oColumn:lSeek := .T.
            oColumn:cOrder := "Order"
            RETURN .T.
         ELSE
            ::nColOrder := nColumn
         ENDIF

      ENDIF

      ::ResetSeek()
      lReturn := .T.

      IF cPrefix != NIL
         ::cPrefix := cPrefix
      ENDIF

      IF ::bSetGet != NIL
         IF ValType( Eval( ::bSetGet ) ) == "N"
            Eval( ::bSetGet, ::nAt )
         ELSEIF ::nLen > 0
            Eval( ::bSetGet, ::aArray[ ::nAt, 1 ] )
         ELSE
            Eval( ::bSetGet, "" )
         ENDIF
      ENDIF

   ELSEIF ::oRSet != NIL .AND. ! Empty( oColumn:cOrder )

      IF nColumn == ::nColOrder .OR. oColumn:lDescend == NIL
         IF lDescend == NIL
            lDescend := iif( Empty( ::nColOrder ) .OR. oColumn:lDescend == NIL, .F., ! oColumn:lDescend )
         ENDIF

         nRecNo := Eval( ::bRecNo )
         ::oRSet:Sort := Upper( oColumn:cOrder ) + iif( lDescend, " DESC", " ASC" )

         oColumn:lDescend := lDescend
         ::UpRStable( nRecNo )
         ::ResetVScroll()
         ::nColOrder := nColumn
         ::ResetSeek()
         ::lHitTop := ::lHitBottom := .F.
         ::nAt := ::nLastnAt := Eval( ::bKeyNo )
         ::HiLiteCell( nColumn )
         ::Refresh()
         RETURN .T.
      ENDIF

      ::uLastTag := oColumn:cOrder
      ::nColOrder := nColumn
      ::HiLiteCell( nColumn )
      ::nAt := ::nLastnAt := Eval( ::bKeyNo )
      lReturn := .T.

   ENDIF

   ::lHitTop := ::lHitBottom := .F.

RETURN lReturn

* ============================================================================
* METHOD TSBrowse:SetSelectMode() Version 9.0 Nov/30/2009
* ============================================================================

METHOD SetSelectMode( lOnOff, bSelected, uBmpSel, nColSel, nAlign ) CLASS TSBrowse

   DEFAULT lOnOff := .T., ;
      nColSel := 1, ;
      nAlign := DT_RIGHT

   ::lCanSelect := lOnOff
   ::bSelected := bSelected
   ::aSelected := {}

   IF ::lCanSelect .AND. ;
         ( uBmpSel == NIL .OR. Empty( nColSel ) .OR. nColSel > Len( ::aColumns ) )
      RETURN Self
   ENDIF

   IF ::lCanSelect .AND. uBmpSel != NIL

      IF ValType( uBmpSel ) != "C"
         ::uBmpSel := uBmpSel
      ELSE
         ::uBmpSel := LoadImage( uBmpSel )
         ::lDestroy := .T.
      ENDIF

      ::nColSel := nColSel
      ::nAligBmp := nAlign

   ELSEIF ::uBmpSel != NIL

      IF ::lDestroy
         DeleteObject( ::uBmpSel )
      ENDIF

      ::lDestroy := .F.
      ::uBmpSel := NIL
      ::nColSel := 0
      ::nAligBmp := 0

   ENDIF

RETURN Self

* ============================================================================
* METHOD TSBrowse:SetSpinner() Version 9.0 Nov/30/2009
* ============================================================================

METHOD SetSpinner( nColumn, lOnOff, bUp, bDown, bMin, bMax ) CLASS TSBrowse

   DEFAULT lOnOff := .T., ;
      bUp := {| oGet | oGet++ }, ;
      bDown := {| oGet | oGet++ }

   IF nColumn == NIL .OR. nColumn > Len( ::aColumns ) .OR. nColumn <= 0
      RETURN Self
   ENDIF

   ::aColumns[ nColumn ]:lSpinner := lOnOff
   ::aColumns[ nColumn ]:bUp := bUp
   ::aColumns[ nColumn ]:bDown := bDown
   ::aColumns[ nColumn ]:bMin := bMin
   ::aColumns[ nColumn ]:bMax := bMax
   ::aColumns[ nColumn ]:lBtnGet := .T.

RETURN Self

#ifdef __DEBUG__
* ============================================================================
* METHOD TSBrowse:ShowSizes() Version 9.0 Nov/30/2009
* ============================================================================

METHOD ShowSizes() CLASS TSBrowse

   LOCAL cText := "", ;
      nTotal := 0, ;
      aTemp := ::GetColSizes()

   AEval( aTemp, {| e, n | nTotal += e, cText += ( iif( ValType( ::aColumns[ n ]:cHeading ) == "C", ;
      ::aColumns[ n ]:cHeading, ::aMsg[ 24 ] + Space( 1 ) + LTrim( Str( n ) ) ) + ": " + ;
      Str( e, 3 ) + " Pixels" + CRLF ) } )

   cText += CRLF + "Total " + Str( nTotal, 4 ) + " Pixels" + CRLF + ::aMsg[ 25 ] + Space( 1 ) + ;
      Str( ::nWidth(), 4 ) + " Pixels" + CRLF

   MsgInfo( cText, ::aMsg[ 26 ] )

RETURN Self

#endif

* ============================================================================
* METHOD TSBrowse:Skip() Version 9.0 Nov/30/2009
* ============================================================================

METHOD Skip( n ) CLASS TSBrowse

   LOCAL nSkipped

   DEFAULT n := 1

   IF ::bSkip != NIL
      nSkipped := Eval( ::bSkip, n )
   ELSE
      nSkipped := ::DbSkipper( n )
   ENDIF

   IF ::lIsDbf
      ::nLastPos := ( ::cAlias )->( RecNo() )
   ENDIF

   ::nLastnAt := ::nAt

   IF ::lIsArr .AND. ::bSetGet != NIL
      IF ValType( Eval( ::bSetGet ) ) == "N"
         Eval( ::bSetGet, ::nAt )
      ELSEIF ::nLen > 0
         Eval( ::bSetGet, ::aArray[ ::nAt, 1 ] )
      ELSE
         Eval( ::bSetGet, "" )
      ENDIF
   ENDIF

RETURN nSkipped

* ============================================================================
* METHOD TSBrowse:SortArray() Version 9.0 Nov/30/2009
* ============================================================================

METHOD SortArray( nCol, lDescend ) CLASS TSBrowse

   LOCAL aLine := ::aArray[ ::nAt ]

   DEFAULT nCol := ::nColOrder

   IF lDescend == NIL
      IF ::aColumns[ nCol ]:lDescend == NIL
         lDescend := .F.
      ELSE
         lDescend := ::aColumns[ nCol ]:lDescend
      ENDIF
   ENDIF

   CursorWait()

   ::aColumns[ nCol ]:lDescend := lDescend

   IF ::lSelector .AND. nCol > 1
      nCol--
   ENDIF

   IF lDescend
      IF ValType( ::aColumns[ nCol ]:bArraySortDes ) == "B"
         ::aArray := ASort( ::aArray, NIL, NIL, ::aColumns[ nCol ]:bArraySortDes )
      ELSE
         ::aArray := ASort( ::aArray, NIL, NIL, {| x, y | x[ nCol ] > y[ nCol ] } )
      ENDIF
   ELSE
      IF ValType( ::aColumns[ nCol ]:bArraySort ) == "B"
         ::aArray := ASort( ::aArray, NIL, NIL, ::aColumns[ nCol ]:bArraySort )
      ELSE
         ::aArray := ASort( ::aArray, NIL, NIL, {| x, y | x[ nCol ] < y[ nCol ] } )
      ENDIF
   ENDIF

   ::nAt := AScan( ::aArray, {| e | lAEqual( e, aLine ) } )
   CursorHand()

RETURN Self

* ============================================================================
* METHOD TSBrowse:SwitchCols() Version 9.0 Nov/30/2009
// This method is dedicated to John Stolte by the 'arry
* ============================================================================

METHOD SwitchCols( nCol1, nCol2 ) CLASS TSBrowse

   LOCAL oHolder, nHolder, nMaxCol := Len( ::aColumns )

   IF nCol1 > ::nFreeze .AND. nCol2 > ::nFreeze .AND. ;
         nCol1 <= nMaxCol .AND. nCol2 <= nMaxCol

      oHolder := ::aColumns[ nCol1 ]
      nHolder := ::aColSizes[ nCol1 ]

      ::aColumns[ nCol1 ] := ::aColumns[ nCol2 ]
      ::aColSizes[ nCol1 ] := ::aColSizes[ nCol2 ]

      ::aColumns[ nCol2 ] := oHolder
      ::aColSizes[ nCol2 ] := nHolder

      IF ::nColOrder == nCol1
         ::nColOrder := nCol2
      ENDIF

   ENDIF

   IF ::lPainted
      ::Refresh( .F. )
   ENDIF

RETURN Self

* ============================================================================
* METHOD TSBrowse:SyncChild() Version 9.0 Nov/30/2009
* ============================================================================

METHOD SyncChild( aoChildBrw, abAction ) CLASS TSBrowse

   IF aoChildBrw != NIL
      IF ValType( aoChildBrw ) == "O"
         aoChildBrw := { aoChildBrw }
      ENDIF

      DEFAULT abAction := Array( Len( aoChildBrw ) )

      IF ValType( abAction ) == "B"
         abAction := { abAction }
      ENDIF

      ::bChange := {|| ;
         AEval( aoChildBrw, {| oChild, nI | iif( ! Empty( oChild:cAlias ), ;
         ( oChild:lHitTop := .F., oChild:goTop(), ;
         iif( abAction[ nI ] != NIL, Eval( abAction[ nI ], Self, oChild ), Nil ), ;
         oChild:reset() ), Nil ) } ) }
   ENDIF

RETURN Self

* ============================================================================
* METHOD TSBrowse:UpStable() Version 9.0 Nov/30/2009
* ============================================================================

METHOD UpStable() CLASS TSBrowse

   LOCAL nRow := ::nRowPos, ;
      nRecNo := ( ::cAlias )->( RecNo() ), ;
      nRows := ::nRowCount(), ;
      n := 1, ;
      lSkip := .T., ;
      bChange := ::bChange, ;
      nLastPos := ::nLastPos, ;
      lPainted := ::lPainted

   IF ::nLen > nRows

      ( ::cAlias )->( dbSkip( nRows - nRow ) )

      IF ( ::cAlias )->( Eof() )
         Eval( ::bGoBottom )
         ::nRowPos := nRows

         WHILE ::nRowPos > 1 .AND. ( ::cAlias )->( RecNo() ) != nRecNo
            ::Skip( -1 )
            ::nRowPos--
         ENDDO

         ::Refresh( .F. )
         ::ResetVScroll()
         RETURN Self
      ELSE
         ( ::cAlias )->( dbGoto( nRecNo ) )
      ENDIF

   ENDIF

   DEFAULT ::lInitGoTop := .T.

   ::lPainted := ::lInitGoTop

   IF ::lPainted .AND. ::bChange != NIL
      Eval( ::bChange, Self, VK_UP )
   ENDIF

   ::bChange := NIL
   ::lHitTop := .F.
   ::lHitBottom := .F.

   ::GoTop()

   ::lPainted := lPainted

   While !( ::cAlias )->( Eof() )

      IF n > nRows
         ::nRowPos := nRow
         lSkip := .F.
         EXIT
      ENDIF

      IF nRecNo == ( ::cAlias )->( RecNo() )
         ::nRowPos := n
         EXIT
      ELSE
         ::Skip( 1 )
      ENDIF

      n++
   ENDDO

   IF lSkip
      ::Skip( -::nRowPos )
   ENDIF

   ( ::cAlias )->( dbGoto( nRecNo ) ) // restores Record position
   ::nLastPos := nLastPos
   ::nAt := ::nLastnAt := ::nLogicPos()
   ::lHitTop := ( ::nAt == 1 )

   IF ::oVScroll != NIL .AND. ! Empty( ::bKeyNo ) // restore scrollbar thumb
      ::oVScroll:SetPos( ::RelPos( ( ::cAlias )->( Eval( ::bKeyNo, NIL, Self ) ) ) )
   ENDIF

   ::bChange := bChange

   IF ::lPainted
      ::Refresh( iif( ::nLen < nRows, .T., .F. ) )
   ENDIF

RETURN Self

* ============================================================================
* METHOD TSBrowse:VertLine() Version 9.0 Nov/30/2009
// Thanks to Gianni Santamarina
* ============================================================================

METHOD VertLine( nColPixPos, nColInit, nGapp ) CLASS TSBrowse

   LOCAL hDC := GetDC( ::hWnd )
   LOCAL aRect := ::GetRect()

   IF nColInit != NIL
      nsCol := nColInit
      nsWidth := nColPixPos
      nGap := iif( ! Empty( nGapp ), nGapp, 0 )
      nsOldPixPos := 0
      _InvertRect( ::hDC, { 0, nsWidth - ::aColSizes[ nsCol ] - 2, aRect[ 4 ], nsWidth - ::aColSizes[ nsCol ] + 2 } )
   ENDIF

   IF nColPixPos == NIL .AND. nColInit == NIL // We have finish dragging
      ::aColSizes[ nsCol ] -= ( nsWidth - nsOldPixPos )
      ::aColumns[ nsCol ]:nWidth -= ( nsWidth - nsOldPixPos )
      ::Refresh()

      IF ValType( ::bLineDrag ) == "B"
         Eval( ::bLineDrag, nsCol, ( nsOldPixPos - nsWidth ), Self )
      ENDIF
   ENDIF

   aRect := ::GetRect()

   IF nsOldPixPos != 0 .AND. nColPixPos != NIL .AND. nColPixPos != nsOldPixPos
      _InvertRect( hDC, { 0, nsOldPixPos - 2, aRect[ 4 ], nsOldPixPos + 2 } )
      nsOldPixPos := 0
   ENDIF

   IF nColPixPos != NIL .AND. nColPixPos != nsOldPixPos
      nColPixPos := Max( nColPixPos, 10 )
      _InvertRect( hDC, { 0, nColPixPos - 2 + nGap, aRect[ 4 ], nColPixPos + 2 + nGap } )
      nsOldPixPos := nColPixPos + nGap
   ENDIF

   ReleaseDC( ::hWnd, hDC )

RETURN NIL

* ============================================================================
* METHOD TSBrowse:VScroll() Version 9.0 Nov/30/2009
* ============================================================================

METHOD VScroll( nMsg, nPos ) CLASS TSBrowse

   LOCAL oCol, ;
      nLines := Min( ::nLen, ::nRowCount() )

   ::lNoPaint := .F.

   IF ::oWnd:hCtlFocus != NIL .AND. ::oWnd:hCtlFocus != ::hWnd
      oCol := ::oWnd:aColumns[ ::oWnd:nCell ]
      IF oCol:oEdit != NIL .AND. nPos == 0
         IF "TBTNBOX" $ Upper( oCol:oEdit:ClassName() )
            Eval( oCol:oEdit:bKeyDown, VK_ESCAPE, 0, .T. )
         ENDIF
      ENDIF
   ENDIF
   IF GetFocus() != ::hWnd
      SetFocus( ::hWnd )
   ENDIF

   DO CASE
   CASE nMsg == SB_LINEUP
      ::GoUp()

   CASE nMsg == SB_LINEDOWN
      IF ! ::lHitBottom
         ::GoDown()
      ENDIF

   CASE nMsg == SB_PAGEUP
      ::PageUp()

   CASE nMsg == SB_PAGEDOWN
      ::PageDown()

   CASE nMsg == SB_TOP
      ::GoTop()

   CASE nMsg == SB_BOTTOM
      ::GoBottom()

   CASE nMsg == SB_THUMBPOSITION

      IF ::nLen == 0
         ::nLen := iif( ::lIsDbf, ( ::cAlias )->( Eval( ::bLogicLen ) ), Eval( ::bLogicLen ) )
      ENDIF

      IF ::nLen < 1
         RETURN 0
      ENDIF

      IF nPos == 1
         ::lHitTop := .F.
         ::GoTop()
         RETURN 0
      ELSEIF nPos >= ::oVScroll:GetRange()[ 2 ]
         ::lHitBottom := .F.
         ::GoBottom()
         RETURN 0
      ELSE
         ::lHitTop := .F.
         ::lHitBottom := .F.
      ENDIF

      ::nAt := ::nLogicPos()
      ::oVScroll:SetPos( ::RelPos( ::nAt ) )

      IF ( nPos - ::oVScroll:nMin ) < nLines
         ::nRowPos := 1
      ENDIF

      IF ( ::oVScroll:nMax - nPos ) < Min( nLines, ::nLen )
         ::nRowPos := Min( nLines, ::nLen ) - ( ::oVScroll:nMax - nPos )
      ENDIF

      ::Refresh( .F. )

      IF ::lIsDbf
         ::nLastPos := ( ::cAlias )->( RecNo() )
      ENDIF

      IF ::bChange != NIL
         Eval( ::bChange, Self, 0 )
      ENDIF

   CASE nMsg == SB_THUMBTRACK

      IF ::lIsDbf
         ::GoPos( ::GetRealPos( nPos ) )
         ::Skip( ( ::GetRealPos( nPos ) - ::GetRealPos( ::oVScroll:GetPos() ) ) )
      ELSE
         IF ::bGoToPos != NIL
            ( ::cAlias )->( Eval( ::bGoToPos, ::GetRealPos( nPos ) ) )
         ELSE
            ::Skip( ( ::GetRealPos( nPos ) - ::GetRealPos( ::oVScroll:GetPos() ) ) )
         ENDIF

         IF nPos == 1
            ::lHitTop := .F.
            ::GoTop()
            RETURN 0
         ELSEIF nPos >= ::oVScroll:GetRange()[ 2 ]
            ::lHitBottom := .F.
            ::GoBottom()
            RETURN 0
         ELSE
            ::lHitTop := .F.
            ::lHitBottom := .F.
         ENDIF

         IF ::nLen >= nLines
            IF ( ::nLen - ::nAt ) <= nLines
               ::nRowPos := nLines - ( ::nLen - ::nAt )
            ELSEIF ::nLen == ::nAt
               ::nRowPos := nLines
            ELSE
               ::nRowPos := 1
            ENDIF
         ENDIF
         ::Refresh( .F. )
         SysRefresh()
      ENDIF
   OTHERWISE
      RETURN NIL
   ENDCASE

RETURN 0

* ============================================================================
* METHOD TSBrowse:Enabled() Version 7.0 Adaptation Version
* ============================================================================

METHOD Enabled( lEnab ) CLASS TSBrowse

   LOCAL nI

   DEFAULT lEnab := .T.

   IF ValType( lEnab ) == "L"

      IF ! lEnab

         IF ::lEnabled
            ::aOldEnabled := { ::hBrush, {}, ::nClrPane, {}, ::nClrLine, NIL }
            IF ::lSelector .AND. ::lDrawSpecHd
              ::aOldEnabled[ 6 ] := ::nClrSpcHdBack
            ENDIF
            FOR nI := 1 TO Len( ::aColumns )
               AAdd( ::aOldEnabled[ 2 ], ::aColumns[ nI ]:Clone() )
               ::aColumns[ nI ]:SaveColor()
            NEXT
            IF ::lDrawSuperHd
               AEval( ::aSuperHead, {| AS | AAdd( ::aOldEnabled[ 4 ], { AS[ 4 ], AS[ 5 ], AS[ 11 ] } ) } )
            ENDIF
            IF ! Empty( ::oPhant )
               ::oPhant:SaveColor()
               ::oPhant:nClrHeadBack := ::nCLR_HGRAY
               ::oPhant:nClrFootBack := ::nCLR_HGRAY
            ENDIF
         ENDIF

         ::lEnabled := .F.
         ::SetColor( { 2 }, { ::nCLR_HGRAY } )
         ::SetColor( { 3, 4 }, { ::nCLR_GRAY, ::nCLR_HGRAY } )
         ::SetColor( { 9, 10 }, { ::nCLR_GRAY, ::nCLR_HGRAY } )
         ::SetColor( { 16, 17 }, { ::nCLR_GRAY, ::nCLR_HGRAY } )
         ::SetColor( { 18, 19 }, { ::nCLR_GRAY, ::nCLR_HGRAY } )
         ::nClrPane := ::nCLR_HGRAY
         ::nClrLine := ::nCLR_Lines
         ::hBrush := CreateSolidBrush( GetRed( ::nClrPane ), GetGreen( ::nClrPane ), GetBlue( ::nClrPane ) )
         IF ::lSelector .AND. ::lDrawSpecHd
            ::nClrSpcHdBack := ::nCLR_HGRAY
         ENDIF

      ELSE

         IF ! ::lEnabled
            FOR nI := 1 TO Len( ::aColumns )
               ::aColumns[ nI ]:RestColor()
               // SetColor( , ::aColumns[ nI ]:aColors, nI )
            NEXT
            IF ! Empty( ::oPhant )
               ::oPhant:RestColor()
            ENDIF
            IF HB_ISARRAY( ::aOldEnabled ) .AND. ! Empty( ::aOldEnabled[ 1 ] )
               AEval( ::aOldEnabled[ 2 ], {| oc, nc | ::aColumns[ nc ] := oc:Clone() } )
               DeleteObject( ::hBrush )
               ::hBrush := ::aOldEnabled[ 1 ]
               ::nClrPane := ::aOldEnabled[ 3 ]
               ::nClrLine := ::aOldEnabled[ 5 ]
               IF ::lDrawSuperHd
                  AEval( ::aOldEnabled[ 4 ], {| AS, ns | ::aSuperHead[ ns ][ 4 ] := AS[ 1 ], ;
                     ::aSuperHead[ ns ][ 5 ] := AS[ 2 ], ;
                     ::aSuperHead[ ns ][ 11 ] := AS[ 3 ] } )
               ENDIF
            ENDIF
            IF ::lSelector .AND. ::lDrawSpecHd
               ::nClrSpcHdBack := ::aOldEnabled[ 6 ]
            ENDIF
         ENDIF

         ::lEnabled := .T.

      ENDIF

      ::Refresh()

   ENDIF

RETURN 0

* ============================================================================
* METHOD TSBrowse:HideColumns() Version 7.0 Adaptation Version
* ============================================================================

METHOD HideColumns( nColumn, lHide ) CLASS TSBrowse

   LOCAL aColumn, nI, nJ, lPaint := .F.

   DEFAULT lHide := .T.

   IF ValType( nColumn ) == "C"
      nColumn := ::nColumn( nColumn ) // 21.07.2015
   ENDIF

   IF Empty( ::aColumns ) .AND. nColumn > 0
      RETURN NIL
   ENDIF

   aColumn := iif( ValType( nColumn ) == "N", { nColumn }, nColumn )

   FOR nI := 1 TO Len( aColumn )

      IF ( nJ := aColumn[ nI ] ) <= Len( ::aColumns )

         lPaint := .T.
         IF lHide
            ::aColSizes[ nJ ] := 0
         ELSE
            ::aColSizes[ nJ ] := ::aColumns[ nJ ]:nWidth
         ENDIF
         ::aColumns[ nJ ]:lVisible := ! lHide

      ENDIF

   NEXT

   ::Refresh( lPaint )

RETURN NIL

* ============================================================================
* METHOD TSBrowse:UserPopup() Version 9.0 Adaptation Version
* ============================================================================

METHOD UserPopup( bUserPopupItem, aColumn ) CLASS TSBrowse

   IF ValType( aColumn ) != "A"
      aColumn := iif( ValType( aColumn ) == "N", { aColumn }, { 0 } )
   ENDIF

   ::bUserPopupItem := iif( ValType( bUserPopupItem ) == "B", bUserPopupItem, ;
      {|| ( bUserPopupItem ) } )
   ::lNoPopup := .F.
   ::lPopupUser := .T.
   ::aPopupCol := aColumn

RETURN 0


* ============================================================================
// TSBrowse   Functions
* ============================================================================

* ============================================================================
* FUNCTION TSBrowse _aData() Version 9.0 Nov/30/2009
// Called from METHOD SetCols()
* ============================================================================

STATIC FUNCTION _aData( aFields )

   LOCAL aFld, nFor, nLen

   nLen := Len( aFields )
   aFld := Array( nLen )

   FOR nFor := 1 TO nLen
      aFld[ nFor ] := Eval( aFields[ nFor ] )
   NEXT

RETURN aFld

#ifdef __DEBUG__
* ============================================================================
* FUNCTION TSBrowse AClone() Version 9.0 Nov/30/2009
* ============================================================================

STATIC FUNCTION AClone( aSource )

   LOCAL aTarget := {}

   AEval( aSource, {| e | AAdd( aTarget, e ) } )

RETURN aTarget

#endif

* ============================================================================
* FUNCTION TSBrowse:AutoHeaders() Version 9.0 Nov/30/2009
// Excel's style column's heading
* ============================================================================

STATIC FUNCTION AutoHeaders( nCols )

   LOCAL nEle, aHead, nChg, cHead, ;
      aBCD := {}

   FOR nEle := 65 TO 90
      AAdd( aBCD, Chr( nEle ) )
   NEXT

   IF nCols <= 26
      ASize( aBCD, nCols )
      RETURN aBCD
   ENDIF

   aHead := AClone( aBCD )
   nCols -= 26
   cHead := "A"
   nChg := 1

   WHILE nCols > 0

      FOR nEle := 1 TO Min( 26, nCols )
         AAdd( aHead, cHead + aBCD[ nEle ] )
      NEXT

      nCols -= 26

      IF Asc( SubStr( cHead, nChg, 1 ) ) == 90
         IF nChg > 1
            nChg--
         ELSE
            cHead := Replicate( Chr( 65 ), Len( cHead ) + 1 )
            nChg := Len( cHead )
         ENDIF
      ENDIF

      cHead := Stuff( cHead, nChg, 1, Chr( Asc( SubStr( cHead, nChg, 1 ) ) + 1 ) )

   ENDDO

RETURN aHead

* ============================================================================
* FUNCTION TSBrowse lASeek() Version 9.0 Nov/30/2009
// Incremental searching in arrays
* ============================================================================

STATIC FUNCTION lASeek( uSeek, lSoft, oBrw )

   LOCAL nEle, uData, ;
      lFound := .F., ;
      aArray := oBrw:aArray, ;
      nCol := oBrw:nColOrder, ;
      nRecNo := oBrw:nAt

   DEFAULT lSoft := .F.

   FOR nEle := Max( 1, nNewEle ) TO Len( aArray )

      uData := aArray[ nEle, nCol ]
      uData := iif( oBrw:lUpperSeek, Upper( cValToChar( uData ) ), cValToChar( uData ) )

      IF ! lSoft
         IF uData = cValToChar( uSeek )
            lFound := .T.
            EXIT
         ENDIF
      ELSE
         IF uData >= cValToChar( uSeek )
            lFound := .T.
            EXIT
         ENDIF
      ENDIF

   NEXT

   IF lFound .AND. nEle <= oBrw:nLen
      oBrw:nAt := nEle
      nNewEle := nEle
   ELSE
      oBrw:nAt := nRecNo
   ENDIF

RETURN lFound

#ifdef _TSBFILTER7_
* ============================================================================
* FUNCTION TSBrowse BrwGoBottom() Version 9.0 Nov/30/2009
// Used by METHOD SetFilter() to set the bottom limit in an "Index Based"
// filtered database
* ============================================================================

STATIC FUNCTION BrwGoBottom( uExpr, oBrw )

   IF ValType( uExpr ) == "C"
      ( oBrw:cAlias )->( dbSeek( SubStr( uExpr, 1, Len( uExpr ) - 1 ) + ;
         Chr( Asc( SubStr( uExpr, Len( uExpr ) ) ) + ;
         iif( ! oBrw:lDescend, 1, -1 ) ), .T. ) )
   ELSE
      ( oBrw:cAlias )->( dbSeek( uExpr + iif( ! oBrw:lDescend, 1, -1 ), .T. ) )
   ENDIF

   IF ( oBrw:cAlias )->( Eof() )
      ( oBrw:cAlias )->( dbGoBottom() )
   ELSE
      ( oBrw:cAlias )->( dbSkip( -1 ) )
   ENDIF

   WHILE oBrw:bFilter != NIL .AND. ! Eval( oBrw:bFilter ) .AND. !( oBrw:cAlias )->( Bof() )
      ( oBrw:cAlias )->( dbSkip( -1 ) )
   ENDDO

RETURN NIL

* ============================================================================
* FUNCTION TSBrowse BrwGoTop() Version 7.0 Jul/15/2004
// Used by METHOD SetFilter() to set the top limit in an "Index Based"
// filtered database
* ============================================================================

STATIC FUNCTION BrwGoTop( oBrw )

   ( oBrw:cAlias )->( dbSeek( oBrw:uValue1, .T. ) )

   WHILE oBrw:bFilter != NIL .AND. ! Eval( oBrw:bFilter ) .AND. !( oBrw:cAlias )->( Eof() )
      ( oBrw:cAlias )->( dbSkip( 1 ) )
   ENDDO

RETURN NIL

* ============================================================================
* FUNCTION TSBrowse BuildSkip() Version 7.0 Jul/15/2004
// Used by METHOD SetFilter(). Returns a block to be used on skipping records
// in an "Index Based" filtered database
* ============================================================================

STATIC FUNCTION BuildSkip( cAlias, cField, uValue1, uValue2, oTb )

   LOCAL bSkipBlock, lDescend := oTb:lDescend, ;
      cType := ValType( uValue1 )

   DO CASE
   CASE cType == "C"
      IF ! lDescend
         bSkipBlock := &( "{|| " + cField + ">= '" + uValue1 + "' .AND. " + ;
            cField + "<= '" + uValue2 + "' }" )
      ELSE
         bSkipBlock := &( "{|| " + cField + "<= '" + uValue1 + "' .AND. " + ;
            cField + ">= '" + uValue2 + "' }" )
      ENDIF
   CASE cType == "D"
      IF ! lDescend
         bSkipBlock := &( "{|| " + cField + ">= CToD( '" + DToC( uValue1 ) + "') .AND. " + ;
            cField + "<= CToD( '" + DToC( uValue2 ) + "') }" )
      ELSE
         bSkipBlock := &( "{|| " + cField + "<= CToD( '" + DToC( uValue1 ) + "') .AND. " + ;
            cField + ">= CToD( '" + DToC( uValue2 ) + "') }" )
      ENDIF

   CASE cType == "N"
      IF ! lDescend
         bSkipBlock := &( "{|| " + cField + ">= " + cValToChar( uValue1 ) + " .AND. " + ;
            cField + "<= " + cValToChar( uValue2 ) + " }" )
      ELSE
         bSkipBlock := &( "{|| " + cField + "<= " + cValToChar( uValue1 ) + " .AND. " + ;
            cField + ">= " + cValToChar( uValue2 ) + " }" )
      ENDIF

   CASE cType == "L"
      IF ! lDescend
         bSkipBlock := &( "{|| " + cField + ">= " + cValToChar( uValue1 ) + " .AND. " + ;
            cField + "<= " + cValToChar( uValue2 ) + " }" )
      ELSE
         bSkipBlock := &( "{|| " + cField + "<= " + cValToChar( uValue1 ) + " .AND. " + ;
            cField + ">= " + cValToChar( uValue2 ) + " }" )
      ENDIF
   ENDCASE

RETURN {| n | ( cAlias )->( BrwGoTo( n, bSkipBlock, oTb ) ) }

* ============================================================================
* FUNCTION TSBrowse BuildFiltr() Version 1.47 Adaption HMG
// Used in Report by Function dbSetFilter. Returns a string used for create bBlock of Filter
* ============================================================================

STATIC FUNCTION BuildFiltr( cField, uValue1, uValue2, oTb )

   LOCAL cFiltrBlock, lDescend := oTb:lDescend, ;
      cType := ValType( uValue1 )

   DO CASE
   CASE cType == "C"
      IF ! lDescend
         cFiltrBlock := "{||" + cField + ">= '" + uValue1 + "' .AND." + ;
            cField + "<= '" + uValue2 + "' }"
      ELSE
         cFiltrBlock := "{||" + cField + "<= '" + uValue1 + "' .AND." + ;
            cField + ">= '" + uValue2 + "' }"
      ENDIF
   CASE cType == "D"
      IF ! lDescend
         cFiltrBlock := "{||" + cField + ">= CToD( '" + DToC( uValue1 ) + "') .AND." + ;
            cField + "<= CToD( '" + DToC( uValue2 ) + "') }"
      ELSE
         cFiltrBlock := "{||" + cField + "<= CToD( '" + DToC( uValue1 ) + "') .AND." + ;
            cField + ">= CToD( '" + DToC( uValue2 ) + "') }"
      ENDIF

   CASE cType == "N"
      IF ! lDescend
         cFiltrBlock := "{||" + cField + ">= " + cValToChar( uValue1 ) + " .AND." + ;
            cField + "<= " + cValToChar( uValue2 ) + " }"
      ELSE
         cFiltrBlock := "{||" + cField + "<= " + cValToChar( uValue1 ) + " .AND." + ;
            cField + ">= " + cValToChar( uValue2 ) + " }"
      ENDIF

   CASE cType == "L"
      IF ! lDescend
         cFiltrBlock := "{||" + cField + ">= " + cValToChar( uValue1 ) + " .AND." + ;
            cField + "<= " + cValToChar( uValue2 ) + " }"
      ELSE
         cFiltrBlock := "{||" + cField + "<= " + cValToChar( uValue1 ) + " .AND." + ;
            cField + ">= " + cValToChar( uValue2 ) + " }"
      ENDIF
   ENDCASE

RETURN cFiltrBlock

#endif

* ============================================================================
* FUNCTION TSBrowse BuildAutoSeek() Version 1.47 Adaption HMG
// Used in AutoSeek by Functions SpecHeader. Returns a string used for create bBlock of Locate
* ============================================================================

STATIC FUNCTION BuildAutoSeek( oTb )

   LOCAL nCol, nLen, cType, uValue, cField, cLocateBlock := "", cComp, cand

   IF oTb:lIsArr

      FOR nCol := 1 TO Len( oTb:aColumns )
         uValue := oTb:aColumns[ nCol ]:cSpcHeading
         cType := ValType( uValue )
         IF ! Empty( uValue )
            IF Empty( cLocateBlock )
               DO CASE
               CASE cType == "C"
                  uValue := RTrim( uValue )
                  nLen := Len( uValue )
                  cLocateBlock := "{|oTb|Ascan ( oTb:aArray, {|x,y| substr(x[" + LTrim( Str( nCol ) ) + "],1," + ;
                     LTrim( Str( nLen ) ) + " ) == '" + uValue + "'"
               CASE cType == "N" .OR. cType == "L"
                  cLocateBlock := "{|oTb|Ascan ( oTb:aArray, {|x,y| x[" + LTrim( Str( nCol ) ) + "] == " + ;
                     cValToChar( uValue )
               CASE cType == "D"
                  cLocateBlock := "{|oTb|Ascan ( oTb:aArray, {|x,y| x[" + LTrim( Str( nCol ) ) + "] == " + ;
                     "CToD( '" + DToC( uValue ) + "' )"
               ENDCASE
            ELSE
               DO CASE
               CASE cType == "C"
                  uValue := RTrim( uValue )
                  nLen := Len( uValue )
                  cLocateBlock += " .AND. substr(x[" + LTrim( Str( nCol ) ) + "],1," + ;
                     LTrim( Str( nLen ) ) + " ) == '" + uValue + "'"
               CASE cType == "N" .OR. cType == "L"
                  cLocateBlock = " .AND. x[" + LTrim( Str( nCol ) ) + "] == " + ;
                     cValToChar( uValue )
               CASE cType == "D"
                  cLocateBlock += " .AND. x[" + LTrim( Str( nCol ) ) + "] == " + ;
                     "CToD( '" + DToC( uValue ) + "' )"
               ENDCASE
            ENDIF
         ENDIF
      NEXT

      cLocateBlock += iif( ! Empty( cLocateBlock ), "}, oTB:nAT + 1 ) }", "" )

   ENDIF

   IF oTb:lIsDbf

      FOR nCol := 1 TO Len( oTb:aColumns )
         uValue := oTb:aColumns[ nCol ]:cSpcHeading
         cField := oTb:aColumns[ nCol ]:cData
         cType := ValType( uValue )
         IF ! Empty( uValue )
            cAnd := iif( Empty( cLocateBlock ), "", " .AND. " )
            DO CASE
            CASE cType == "C"
               uValue := RTrim( uValue )
               nLen := Len( uValue )
               cLocateBlock += cAnd + " substr(" + cField + ",1," + ;
                  LTrim( Str( nLen ) ) + " ) == '" + uValue + "'"
            CASE cType == "N" .OR. cType == "L"
               cLocateBlock += cAnd + cField + " == " + cValToChar( uValue )
            CASE cType == "D"
               cLocateBlock += cAnd + cField + " == " + "CToD( '" + DToC( uValue ) + "' )"
            ENDCASE
         ENDIF
      NEXT

   ENDIF

   IF AND( oTb:nDataType, DATATYPE_ADO ) != 0

      FOR nCol := 1 TO Len( oTb:aColumns )
         uValue := oTb:aColumns[ nCol ]:cSpcHeading
         cField := oTb:aColumns[ nCol ]:cOrder
         cType := ValType( uValue )
         IF ! Empty( uValue )
            if ! Empty( cLocateBlock ) // Only a single-column name may be specified in cLocateBlock.
               Tone( 500, 1 )
               RETURN cLocateBlock
            ENDIF
            DO CASE
            CASE cType == "C"
               cComp := iif( At( '*', uValue ) != 0, " LIKE '", " = '" )
               uValue := RTrim( uValue )
               cLocateBlock := cField + cComp + uValue + "'"
            CASE cType == "N" .OR. cType == "L"
               cLocateBlock := cField + " = " + cValToChar( uValue )
            CASE cType == "D"
               cLocateBlock := cField + " = #" + DToC( uValue ) + "# "
            ENDCASE
         ENDIF
      NEXT

   ENDIF

RETURN cLocateBlock

* ============================================================================
* FUNCTION TSBrowse BuildAutoFiltr() Version 1.47 Adaption HMG
// Used in AutoFilter by Functions SpecHeader. Returns a string used for create bBlock of Filter
* ============================================================================

STATIC FUNCTION BuildAutoFilter( oTb )

   LOCAL nCol, nLen, cType, uValue, cField, cFilterBlock := "", cAnd, cComp

   IF oTb:lIsDbf

      FOR nCol := 1 TO Len( oTb:aColumns )
         uValue := oTb:aColumns[ nCol ]:cSpcHeading
         cField := oTb:aColumns[ nCol ]:cData
         cType := ValType( uValue )
         IF ! Empty( uValue )
            cAnd := iif( Empty( cFilterBlock ), "", " .AND. " )
            DO CASE
            CASE cType == "C"
               uValue := RTrim( uValue )
               nLen := Len( uValue )
               cFilterBlock += cAnd + " substr(" + cField + ",1," + ;
                  LTrim( Str( nLen ) ) + " ) == '" + uValue + "'"
            CASE cType == "N" .OR. cType == "L"
               cFilterBlock += cAnd + cField + " == " + cValToChar( uValue )
            CASE cType == "D"
               cFilterBlock += cAnd + cField + " == " + "CToD( '" + DToC( uValue ) + "' )"
            ENDCASE
         ENDIF
      NEXT

   ENDIF

   IF AND( oTb:nDataType, DATATYPE_ADO ) != 0

      FOR nCol := 1 TO Len( oTb:aColumns )
         uValue := oTb:aColumns[ nCol ]:cSpcHeading
         cField := oTb:aColumns[ nCol ]:cOrder
         cType := ValType( uValue )
         IF ! Empty( uValue )
            cAnd := iif( Empty( cFilterBlock ), "", " AND " )
            DO CASE
            CASE cType == "C"
               cComp := iif( At( '*', uValue ) != 0, " LIKE '", " = '" )
               uValue := RTrim( uValue )
               cFilterBlock += cAnd + cField + cComp + uValue + "'"

            CASE cType == "N" .OR. cType == "L"
               cFilterBlock += cAnd + cField + " = " + cValToChar( uValue )

            CASE cType == "D"
               cFilterBlock += cAnd + cField + " = #" + DToC( uValue ) + "# "
            ENDCASE
         ENDIF
      NEXT

   ENDIF

RETURN cFilterBlock

#ifdef _TSBFILTER7_
* ============================================================================
* FUNCTION TSBrowse BrwGoto() Version 7.0 Jul/15/2004
// Executes the action defined into the block created with FUNCTION BuildSkip()
* ============================================================================

STATIC FUNCTION BrwGoTo( n, bWhile, oTb )

   LOCAL nSkipped := 0, ;
      nDirection := iif( n > 0, 1, -1 )

   WHILE nSkipped != n .AND. Eval( bWhile ) .AND. !( oTb:cAlias )->( Eof() ) .AND. !( oTb:cAlias )->( Bof() )
      ( oTb:cAlias )->( dbSkip( nDirection ) )
      nSkipped += nDirection

      IF oTb:bFilter != NIL
         While ! Eval( oTb:bFilter ) .AND. !( oTb:cAlias )->( Eof() ) .AND. !( oTb:cAlias )->( Bof() )
            ( oTb:cAlias )->( dbSkip( nDirection ) )
         ENDDO
      ENDIF
   ENDDO

   DO CASE
   CASE ( oTb:cAlias )->( Eof() )
      ( oTb:cAlias )->( dbSkip( -1 ) )

      WHILE oTb:bFilter != NIL .AND. ! Eval( oTb:bFilter ) .AND. !( oTb:cAlias )->( Bof() )
         ( oTb:cAlias )->( dbSkip( -1 ) )
      ENDDO

      IF ! oTb:lAppendMode
         nSkipped += -nDirection
      ELSE
         IF oTb:nPrevRec == NIL
            oTb:nPrevRec := ( oTb:cAlias )->( RecNo() )
         ENDIF
         ( oTb:cAlias )->( dbGoto( 0 ) ) // phantom record
      ENDIF

   CASE ( oTb:cAlias )->( Bof() )
      ( oTb:cAlias )->( dbGoto( ( oTb:cAlias )->( RecNo() ) ) )

      WHILE oTb:bFilter != NIL .AND. ! Eval( oTb:bFilter ) .AND. !( oTb:cAlias )->( Eof() )
         ( oTb:cAlias )->( dbSkip( 1 ) )
      ENDDO

      nSkipped++

   Case ! Eval( bWhile )
      IF nDirection == 1 .AND. oTb:lAppendMode
         IF oTb:nPrevRec == NIL
            ( oTb:cAlias )->( dbSkip( -1 ) )
            oTb:nPrevRec := ( oTb:cAlias )->( RecNo() )
         ENDIF
         ( oTb:cAlias )->( dbGoto( 0 ) ) // phantom record
      ELSE
         ( oTb:cAlias )->( dbSkip( -nDirection ) )
         WHILE oTb:bFilter != NIL .AND. ! Eval( oTb:bFilter ) .AND. ;
               !( oTb:cAlias )->( Bof() ) .AND. !( oTb:cAlias )->( Eof() )
            ( oTb:cAlias )->( dbSkip( -nDirection ) )
         ENDDO

         nSkipped += -nDirection
      ENDIF
   ENDCASE

RETURN nSkipped

#endif

* ============================================================================
* FUNCTION TSBrowse:DateSeek() Version 9.0 Nov/30/2009
* ============================================================================

STATIC FUNCTION DateSeek( cSeek, nKey )

   LOCAL cChar := Chr( nKey ), ;
      nSpace := At( " ", cSeek ), ;
      cTemp := ""

   /* only  0..9 */
   IF nKey >= 48 .AND. nKey <= 57
      IF nSpace <> 0
         cTemp := Left( cSeek, nSpace - 1 )
         cTemp += cChar
         cTemp += SubStr( cSeek, nSpace + 1, Len( cSeek ) )
         cSeek := cTemp
      ELSE
         cSeek := cSeek
         Tone( 500, 1 )
      ENDIF
   ELSEIF nKey == VK_BACK
      IF nSpace == 4 .OR. nSpace == 7
         cTemp := Left( cSeek, nSpace - 3 )
         cTemp += " "
         cTemp += SubStr( cSeek, nSpace - 1, Len( cSeek ) )
      ELSEIF nSpace == 0
         cTemp := Left( cSeek, Len( cSeek ) - 1 )
      ELSEIF nSpace == 1
         cTemp := cSeek
      ELSE
         cTemp := Left( cSeek, nSpace - 2 )
         cTemp += " "
         cTemp += SubStr( cSeek, nSpace, Len( cSeek ) )
      ENDIF
      cSeek := PadR( cTemp, 10 )
   ELSE
      Tone( 500, 1 )
   ENDIF

RETURN cSeek

* ============================================================================
* FUNCTION TSBrowse EmptyAlias() Version 9.0 Nov/30/2009
* Returns .T. if cAlias is not a constant "ARRAY" (browsing an array),
* or a constant "TEXT_" (browsing a text file), or an active database alias.
* ============================================================================

STATIC FUNCTION EmptyAlias( cAlias )

   LOCAL lEmpty := .T.

   IF ! Empty( cAlias )

      IF cAlias == "ARRAY" .OR. "TEXT_" $ cAlias .OR. "ADO_" $ cAlias
         lEmpty := .F.
      ELSE
         BEGIN SEQUENCE WITH {| o | Break( o ) }
            IF ( cAlias )->( Used() )
               lEmpty := .F.
            ENDIF
         END SEQUENCE
      ENDIF

   ENDIF

RETURN lEmpty

* ============================================================================
* FUNCTION TSBrowse IsChar() Version 9.0 Nov/30/2009
* Used by METHOD KeyChar() to filter keys according to the field type
* Clipper's function IsAlpha() doesn't fit the purpose in some cases
* ============================================================================

STATIC FUNCTION _IsChar( nKey )

RETURN ( nKey >= 32 .AND. nKey <= hb_cdpCharMax() )

* ============================================================================
* FUNCTION TSBrowse IsNumeric() Version 9.0 Nov/30/2009
* Function used by METHOD KeyChar() to filter keys according to the field type
* Clipper's function IsDigit() doesn't fit the purpose in some cases
* ============================================================================

STATIC FUNCTION _IsNumeric( nKey )

RETURN ( Chr( nKey ) $ ".+-0123456789" )

* ============================================================================
* FUNCTION TSBrowse MakeBlock() Version 9.0 Nov/30/2009
* Called from METHOD Default() to assign data to columns
* ============================================================================

STATIC FUNCTION MakeBlock( Self, nI )

RETURN {|| Eval( ::bLine )[ nI ] }

* ============================================================================
* FUNCTION TSBrowse nValToNum() Version 9.0 Nov/30/2009
* Converts any type variables value into numeric
* ============================================================================

FUNCTION nValToNum( uVar )

   LOCAL nVar := iif( ValType( uVar ) == "N", uVar, ;
      iif( ValType( uVar ) == "C", Val( StrTran( AllTrim( uVar ), "," ) ), ;
      iif( ValType( uVar ) == "L", iif( uVar, 1, 0 ), ;
      iif( ValType( uVar ) == "D", Val( DToS( uVar ) ), 0 ) ) ) )

RETURN nVar

* ============================================================================
* METHOD TSBrowse:SetRecordSet() Version 9.0 Nov/30/2009
* ============================================================================

METHOD SetRecordSet( oRSet ) CLASS TSBrowse

   DEFAULT ::oRSet := oRSet, ;
      ::bGoTop := {|| ::oRSet:MoveFirst() }, ;
      ::bGoBottom := {|| ::oRSet:MoveLast() }, ;
      ::bKeyCount := {|| ::oRSet:RecordCount() }, ;
      ::bBof := {|| ::oRSet:Bof() }, ;
      ::bEof := {|| ::oRSet:Eof() }, ;
      ::bSkip := {| n | RSetSkip( ::oRSet, iif( n == NIL, 1, n ) ) }, ;
      ::bKeyNo := {| n | iif( n == NIL, ::oRSet:AbsolutePosition, ::oRSet:AbsolutePosition := n ) }, ;
      ::bLogicLen := {|| ::oRSet:RecordCount() }, ;
      ::bGoToPos := {| n | Eval( ::bKeyNo, n ) }

   ::bRecNo := {| n | iif( n == NIL, iif( ::oRSet:RecordCount() > 0, ::oRSet:BookMark, 0 ), ;
      iif( ::oRSet:RecordCount() > 0, ( ::oRSet:BookMark := n ), 0 ) ) }
   ::nLen := Eval( ::bLogicLen )
   ::ResetVScroll( .T. )

RETURN Self

* ============================================================================
* FUNCTION TSBrowse RSetSkip() Version 9.0 Nov/30/2009
* ============================================================================

STATIC FUNCTION RSetSkip( oRSet, n )

   LOCAL nRecNo := oRSet:AbsolutePosition

   oRSet:Move( n )

   IF oRSet:Eof()
      oRSet:MoveLast()
   ELSEIF oRSet:Bof()
      oRSet:MoveFirst()
   ENDIF

RETURN oRSet:AbsolutePosition - nRecNo

* ============================================================================
* FUNCTION TSBrowse RSetLocate() Version 9.0 Nov/30/2009
* ============================================================================

STATIC FUNCTION RSetLocate( oTb, cFindCriteria, lDescend, lContinue )

   LOCAL nRecNo
   LOCAL oRSet := oTb:oRSet

   DEFAULT lDescend := .F., lContinue := .T.

   nRecNo := oRSet:AbsolutePosition
   IF ! lContinue
      oRSet:MoveFirst()
   ENDIF
   IF lDescend
      oRSet:Find( cFindCriteria, 1, adSearchBackward )
   ELSE
      oRSet:Find( cFindCriteria, 1, adSearchForward )
   ENDIF
   IF oRSet:Eof()
      Eval( oTb:bGoToPos, nRecNo )
      Tone( 500, 1 )
   ELSEIF oRSet:Bof()
      Eval( oTb:bGoToPos, nRecNo )
      Tone( 500, 1 )
   ENDIF

RETURN oRSet:AbsolutePosition - nRecNo

* ============================================================================
* FUNCTION TSBrowse RSetFilter() Version 9.0 Nov/30/2009
* ============================================================================

STATIC FUNCTION RSetFilter( oTb, cFilterCriteria )

   LOCAL nRecNo
   LOCAL oRSet := oTb:oRSet

   IF ! Empty( cFilterCriteria )
      oRSet:Filter := cFilterCriteria
      IF oRSet:Eof()
         Tone( 500, 1 )
         oRSet:Filter := adFilterNone
      ENDIF
   ELSE
      oRSet:Filter := adFilterNone
   ENDIF
   nRecNo := oRSet:AbsolutePosition

RETURN nRecNo

* ============================================================================
* FUNCTION TSBrowse SetHeights() Version 7.0 Jul/15/2004
* ============================================================================

STATIC FUNCTION SetHeights( oBrw )

   LOCAL nEle, nHeight, nHHeight, oColumn, nAt, cHeading, cRest, nOcurs, hFont, ;
      lDrawFooters := iif( oBrw:lDrawFooters != NIL, oBrw:lDrawFooters, .F. )

   DEFAULT oBrw:nLineStyle := LINES_ALL

   IF oBrw:lDrawHeaders
      nHHeight := oBrw:nHeightHead

      FOR nEle := 1 TO Len( oBrw:aColumns )

         oColumn := oBrw:aColumns[ nEle ]
         cHeading := iif( ValType( oColumn:cHeading ) == "B", Eval( oColumn:cHeading, nEle, oBrw ), oColumn:cHeading )
         hFont := iif( oColumn:hFontHead != NIL, oColumn:hFontHead, oBrw:hFont )
         hFont := iif( ValType( hFont ) == "B", Eval( hFont, 0, nEle, oBrw ), hFont )
         hFont := iif( hFont == NIL, 0, hFont )

         IF ValType( cHeading ) == "C" .AND. ;
               ( nAt := At( Chr( 13 ), cHeading ) ) > 0

            nOcurs := 1
            cRest := SubStr( cHeading, nAt + 2 )

            WHILE ( nAt := At( Chr( 13 ), cRest ) ) > 0
               nOcurs++
               cRest := SubStr( cRest, nAt + 2 )
            ENDDO

            nHeight := SBGetHeight( oBrw:hWnd, hFont, 0 )
            nHeight *= ( nOcurs + 1 )

            IF ( nHeight + 1 ) > nHHeight
               nHHeight := nHeight + 1
            ENDIF

         ELSEIF ValType( cHeading ) == "C" .AND. LoWord( oBrw:aColumns[ nEle ]:nHAlign ) == DT_VERT

            nHeight := GetTextWidth( oBrw:hDC, cHeading, hFont )

            IF nHeight > nHHeight
               nHHeight := nHeight
            ENDIF

         ENDIF

      NEXT

      oBrw:nHeightHead := nHHeight
   ELSE
      oBrw:nHeightHead := 0
   ENDIF

   IF oBrw:lFooting .AND. lDrawFooters

      nHHeight := oBrw:nHeightFoot

      FOR nEle := 1 TO Len( oBrw:aColumns )

         oColumn := oBrw:aColumns[ nEle ]
         cHeading := iif( ValType( oColumn:cFooting ) == "B", Eval( oColumn:cFooting, nEle, oBrw ), oColumn:cFooting )
         hFont := iif( oColumn:hFontFoot != NIL, oColumn:hFontFoot, iif( oBrw:hFont != NIL, oBrw:hFont, 0 ) )

         hFont := iif( ValType( hFont ) == "B", Eval( hFont, 0, nEle, oBrw ), hFont )
         hFont := iif( hFont == NIL, 0, hFont )

         IF ValType( cHeading ) == "C" .AND. ( nAt := At( Chr( 13 ), cHeading ) ) > 0

            nOcurs := 1
            cRest := SubStr( cHeading, nAt + 2 )

            WHILE ( nAt := At( Chr( 13 ), cRest ) ) > 0
               nOcurs++
               cRest := SubStr( cRest, nAt + 2 )
            ENDDO

            nHeight := SBGetHeight( oBrw:hWnd, hFont, 0 )
            nHeight *= ( nOcurs + 1 )

            IF ( nHeight + 1 ) > nHHeight
               nHHeight := nHeight + 1
            ENDIF

         ENDIF

      NEXT

      oBrw:nHeightFoot := nHHeight
   ELSE
      oBrw:nHeightFoot := 0
   ENDIF

   // Now for cells

   nHHeight := oBrw:nHeightCell

   FOR nEle := 1 TO Len( oBrw:aColumns )

      oColumn := oBrw:aColumns[ nEle ]
      cHeading := oBrw:bDataEval( oColumn )
      hFont := iif( oColumn:hFont != NIL, oColumn:hFont, oBrw:hFont )
      hFont := iif( ValType( hFont ) == "B", Eval( hFont, 1, nEle, oBrw ), hFont )
      hFont := iif( hFont == NIL, 0, hFont )

      IF ValType( cHeading ) == "C" .AND. At( Chr( 13 ), cHeading ) > 0 .OR. ;
            ValType( cHeading ) == "M" .OR. oColumn:cDataType != NIL .AND. oColumn:cDataType == "M"

         DEFAULT cHeading := ""
         IF Empty( oBrw:nMemoHV )
            IF At( Chr( 13 ), cHeading ) > 0
               oBrw:nMemoHV := Len( hb_ATokens( cHeading, Chr( 13 ) ) )
            ENDIF
         ENDIF
         DEFAULT oBrw:nMemoHV := 2
         nHeight := SBGetHeight( oBrw:hWnd, hFont, 0 )
         nHeight *= oBrw:nMemoHV
         nHeight += iif( oBrw:nLineStyle != 0 .AND. oBrw:nLineStyle != 2, 1, 0 )
      ELSE
         nHeight := SBGetHeight( oBrw:hWnd, hFont, 0 )
         nHeight += iif( oBrw:nLineStyle != 0 .AND. oBrw:nLineStyle != 2, 1, 0 )
      ENDIF

      nHHeight := Max( nHeight, nHHeight )

   NEXT

   oBrw:nHeightCell := nHHeight

RETURN NIL

* ============================================================================
* FUNCTION TSBrowse FileRename() Version 9.0 Nov/30/2009
* ============================================================================

STATIC FUNCTION FileRename( oBrw, cOldName, cNewName, lErase )

   LOCAL nRet, lNew := File( cNewName )

   DEFAULT lErase := .T.

   IF ! File( cOldName )
      MsgStop( oBrw:aMsg[ 29 ] + CRLF + AllTrim( cOldName ) + oBrw:aMsg[ 30 ], oBrw:aMsg[ 28 ] )
      Return( -1 )
   ENDIF

   IF lErase .AND. lNew

      IF FErase( cNewName ) < 0
         MsgStop( oBrw:aMsg[ 11 ] + Space( 1 ) + AllTrim( cNewName ), ;
            oBrw:aMsg[ 28 ] + Space( 1 ) + LTrim( Str( FError() ) ) )
         Return( -1 )
      ENDIF

   ELSEIF ! lErase .AND. lNew
      Return( -1 )
   ENDIF

   nRet := MoveFile( cOldName, cNewName )

   IF File( cOldName ) .AND. File( cNewName )
      FErase( cOldName )
   ENDIF

RETURN nRet

//--------------------------------------------------------------------------------------------------------------------//

STATIC FUNCTION AdoGenFldBlk( oRS, nFld )

RETURN {| uVar | iif( uVar == NIL, oRs:Fields( nFld ):Value, oRs:Fields( nFld ):Value := uVar ) }

//--------------------------------------------------------------------------------------------------------------------//

STATIC FUNCTION ClipperFieldType( nType )

   LOCAL aNum := { adSmallInt, adInteger, adSingle, adDouble, adUnsignedTinyInt, adTinyInt, adUnsignedSmallInt, ;
      adUnsignedInt, adBigInt, adUnsignedBigInt, adNumeric, adVarNumeric, adCurrency }, ;
      cType

   DO CASE
   CASE AScan( { adChar, adWChar, adVarChar, adVarWChar }, nType ) > 0
      cType := "C"
   CASE AScan( aNum, nType ) > 0
      cType := "N"
   CASE nType == 11
      cType := "L"
   CASE AScan( { adDate, adDBDate }, nType ) > 0
      cType := "D"
   CASE nType == adLongVarWChar
      cType := "M"
   OTHERWISE
      cType := nType
   END CASE

RETURN cType

* ============================================================================
* FUNCTION TSBrowse IdentSuper() Version 7.0
* ============================================================================

STATIC FUNCTION IdentSuper( aHeaders, oBrw )

   LOCAL nI := 1, cSuper, cOldSuper := '', nFromCol := 1, nToCol
   LOCAL aSuperHeaders := {}, nSel := 0

   IF oBrw:lSelector
      nSel := 1
   ENDIF

   WHILE nI <= Len( aHeaders )
      IF ValType( aHeaders[ nI ] ) == "C" .AND. At( '~', aHeaders[ nI ] ) > 0
         cSuper := SubStr( aHeaders[ nI ], At( '~', aHeaders[ nI ] ) + 1 )
         aHeaders[ nI ] := SubStr( aHeaders[ nI ], 1, At( '~', aHeaders[ nI ] ) - 1 )
      ELSE
         cSuper := ''
      ENDIF
      IF !( cOldSuper == cSuper )
         nToCol := nI - 1
         AAdd( aSuperHeaders, { cOldSuper, nFromCol + nSel, nToCol + nSel } )
         cOldSuper := cSuper
         nFromCol := nI
      ELSEIF nI == Len( aHeaders )
         AAdd( aSuperHeaders, { cOldSuper, nFromCol + nSel, nI + nSel } )
      ENDIF
      nI++
   ENDDO

RETURN aSuperHeaders

* ============================================================================
* METHOD TSBrowse:RefreshARow() Version 9.0 Nov/30/2009   JP Ver 1.90
* ============================================================================

METHOD RefreshARow( xRow ) CLASS TSBrowse

   LOCAL nRow := ::nRowPos, nSkip, ;
      nRows := ::nRowCount(), ;
      nAt := ::nAt, ;
      nLine

   DEFAULT xRow := nAt

   IF xRow == nAt
      ::Refresh( .F. )
   ELSEIF xRow >= nAt - nRow + 1 .AND. xRow <= nAt + nRows - nRow
      nLine := xRow - ( nAt - nRow )
      nSkip := nline - nRow
      ::Skip( nSkip )
      ::DrawLine( nLine )
      ::Skip( -nSkip )
   ENDIF

RETURN NIL

* ============================================================================
* METHOD TSBrowse:UpAStable() Version 9.0 Nov/30/2009
* ============================================================================

METHOD UpAStable() CLASS TSBrowse

   LOCAL nRow := ::nRowPos, ;
      nRows := ::nRowCount(), ;
      n := 1, ;
      lSkip := .T., ;
      bChange := ::bChange, ;
      nLastPos := ::nLastPos, ;
      nAt := ::nAt, ;
      nRecNo

   ::nLen := Len( ::aArray )
   ::nAt  := Min( ::nLen, nAt )
   nRecNo := iif( ::nAt > 0, ::aArray[ ::nAt ], 0 )

   IF ::nLen > nRows
      nAt += ( nRows - nRow )

      IF nAt >= ::nLen
         Eval( ::bGoBottom )
         ::nRowPos := nRows

         WHILE ::nRowPos > 1 .AND. ! lAEqual( ::aArray[ ::nAt ], nRecNo )
            ::Skip( -1 )
            ::nRowPos--
         ENDDO

         ::Refresh( .F. )
         ::ResetVScroll()
         RETURN Self
      ENDIF
   ENDIF

   ::bChange := NIL
   ::lHitTop := .F.
   ::lHitBottom := .F.
   Eval( ::bGoTop )

   While ! ::Eof()

      IF n > nRows
         ::nRowPos := nRow
         lSkip := .F.
         EXIT
      ENDIF

      IF lAEqual( ::aArray[ ::nAt ], nRecNo )
         ::nRowPos := n
         EXIT
      ELSE
         ::Skip( 1 )
      ENDIF

      n++
   ENDDO

   IF lSkip
      ::Skip( -::nRowPos )
   ENDIF

   ::nLastPos := nLastPos
   ::nAt := AScan( ::aArray, {| e | lAEqual( e, nRecNo ) } )
   ::lHitTop := .F.

   IF ::oVScroll != NIL .AND. nRow != ::nRowPos
      ::oVScroll:SetPos( ::RelPos( ::nLogicPos() ) )
   ENDIF

   ::bChange := bChange

   IF ::lPainted
      ::Refresh( iif( ::nLen < nRows, .T., .F. ) )
   ENDIF

RETURN Self

* =================================================================================
* METHOD TSBrowse:lSeek() Version 9.0 Nov/30/2009 dichotomic search with recordsets
* =================================================================================

METHOD lRSeek( uData, nFld, lSoft ) CLASS TSBrowse

   LOCAL nCen, ;
      lFound := .F., ;
      nInf := 1, ;
      nSup := ::nLen, ;
      nRecNo := Eval( ::bKeyNo )

   DEFAULT lSoft := .F.

   WHILE nInf <= nSup
      nCen := Int( ( nSup + nInf ) / 2 )
      Eval( ::bGoToPos, nCen )

      IF ( lFound := iif( lSoft, uData = ::oRSet:Fields( nFld ):Value, uData == ::oRSet:Fields( nFld ):Value ) )
         EXIT
      ELSEIF uData > ::oRSet:Fields( nFld ):Value
         nInf := nCen + 1
      ELSE
         nSup := nCen - 1
      ENDIF
   ENDDO

   IF ! lFound
      Eval( ::bGoToPos, nRecNo )
   ENDIF

RETURN lFound

* ================================================================================
* METHOD TSBrowse:UpRStable() Version 9.0 Nov/30/2009 recorset cursor repositioned
* ================================================================================

METHOD UpRStable( nRecNo ) CLASS TSBrowse

   LOCAL nRow := ::nRowPos, ;
      nRows := ::nRowCount(), ;
      n := 1, ;
      lSkip := .T., ;
      bChange := ::bChange, ;
      nLastPos := ::nLastPos

   Eval( ::bRecNo, nRecNo )

   IF ::nLen > nRows
      ::oRSet:Move( nRows - nRow )

      IF Eval( ::bBof ) // ::EoF()
         Eval( ::bGoBottom )
         ::nRowPos := nRows
         WHILE ::nRowPos > 1 .AND. nRecNo != Eval( ::bRecNo )
            ::Skip( -1 )
            ::nRowPos--
         ENDDO

         ::Refresh( .F. )
         ::ResetVScroll()
         RETURN Self
      ELSE
         Eval( ::bGoToPos, nRecNo )
      ENDIF
   ENDIF

   ::bChange := NIL
   ::lHitTop := .F.
   ::lHitBottom := .F.
   ::GoTop()

   While ! ::Eof()

      IF n > nRows
         ::nRowPos := nRow
         lSkip := .F.
         EXIT
      ENDIF

      IF nRecNo == Eval( ::bRecNo )
         ::nRowPos := n
         EXIT
      ELSE
         ::Skip( 1 )
      ENDIF

      n++
   ENDDO

   IF lSkip
      ::Skip( -::nRowPos )
   ENDIF

   Eval( ::bRecNo, nRecNo ) // restores Record position
   ::nLastPos := nLastPos
   ::nAt := ::nLastnAt := ::nLogicPos()
   ::lHitTop := .F.

   IF ::oVScroll != NIL .AND. ! Empty( ::bKeyNo ) // restore scrollbar thumb
      ::oVScroll:SetPos( ::RelPos( ( ::cAlias )->( Eval( ::bKeyNo ) ) ) )
   ENDIF

   ::bChange := bChange

   IF ::lPainted
      ::Refresh( iif( ::nLen < nRows, .T., .F. ) )
   ENDIF

RETURN Self

* ===================================================================================================
* METHOD TSBrowse:nField() Version 9.0 Nov/30/2009 returns field number from field name in recordsets
* ===================================================================================================

METHOD nField( cName ) CLASS TSBrowse

   LOCAL nEle, ;
      nCount := ::oRSet:Fields:Count()

   FOR nEle := 1 TO nCount
      IF Upper( ::oRSet:Fields( nEle - 1 ):Name ) == Upper( cName )
         EXIT
      ENDIF
   NEXT

RETURN iif( nEle <= nCount, nEle - 1, -1 )

* ===================================================================================================
* Auxiliary TSBcell class
* ===================================================================================================

CLASS TSBcell

   VAR Cargo
   VAR nRow AS NUMERIC INIT 0
   VAR nCol AS NUMERIC INIT 0
   VAR nWidth AS NUMERIC INIT 0
   VAR nHeight AS NUMERIC INIT 0
   VAR nFromCol AS NUMERIC INIT 0
   VAR nToCol AS NUMERIC INIT 0
   VAR nCell
   VAR uValue
   VAR lDrawLine

   VAR hWnd // 1
   VAR hDC // 2
   VAR xRow // 3
   VAR nStartCol // 4
   VAR nSize // 5 aColSizes[ nJ ] + nDeltaLen
   VAR uData // 6
   VAR nAlign // 7
   VAR nClrFore // 8
   VAR nClrBack // 9
   VAR hFont // 10
   VAR hBitMap // 11
   VAR nHeightCell // 12
   VAR l3DLook // 13 oColumn:l3DLook
   VAR nLineStyle // 14
   VAR nClrLine // 15
   VAR nDrawType // 16 line/header/footer/super
   VAR nHeightHead // 17
   VAR nHeightFoot // 18
   VAR nHeightSuper // 19
   VAR nHeightSpecHd // 20
   VAR lAdjBmp // 21
   VAR lMultiline // 22
   VAR nVAlign // 23
   VAR nVertText // 24
   VAR nClrTo // 25
   VAR lOpaque // 26
   VAR hBrush // 27  iif( lBrush, nClrBack:hBrush, 0 )
   VAR l3DText // 28  3D text
   VAR nClr3dL // 29  3D text light color
   VAR nClr3dS // 30  3D text shadow color
   VAR nCursor // 31  Rect cursor
   VAR lInvertColor // 32  Invert color
   VAR nBitmapMask INIT 0x008800C6 // 33 - SergKis 11.11.21 - SRCAND

   METHOD New() INLINE ( Self )
   ACCESS cValue INLINE ::uData

ENDCLASS

* ===================================================================================================
* METHOD TSBrowse:GetCellInfo() returns the cell coordinates for auxiliary TSBcell class
* ===================================================================================================

METHOD GetCellInfo( nRowPos, nCell, lColSpecHd ) CLASS TSBrowse

   LOCAL nI, ix, nStartX := 0, oCol, cBrw, cForm
   LOCAL nRow, nCol, nWidth, nHeight
   LOCAL lHead := .F., lFoot := .F.
   LOCAL oCell := TSBcell():New()

   IF HB_ISLOGICAL( nRowPos )
      IF nRowPos ; lHead := .T.
      ELSE ; lFoot := .T.
      ENDIF
      nRowPos := NIL
      lColSpecHd := .F.
   ENDIF

   DEFAULT nRowPos := ::nRowPos, ;
      nCell := ::nCell, ;
      lColSpecHd := .F.

   cForm := ::cParentWnd
   cBrw := ::cControlName
   oCol := ::aColumns[ nCell ]

   IF ::nFreeze > 0
      FOR nI := 1 TO Min( ::nFreeze, nCell - 1 )
         nStartX += ::GetColSizes()[ nI ]
      NEXT
   ENDIF

   FOR nI := ::nColPos TO nCell - 1
      nStartX += ::GetColSizes()[ nI ]
   NEXT

   IF lColSpecHd
      nRow := ::nHeightHead + ::nHeightSuper + iif( oCol:l3DLook, 2, 0 )
      nCol := nStartX + iif( oCol:l3DLook, 2, 0 )
      nWidth := ::GetColSizes()[ nCell ] - iif( oCol:l3DLook, 2, 1 )
      nHeight := ::nHeightSpecHd - iif( oCol:l3DLook, 1, -1 )
   ELSE
      nRow := nRowPos - 1
      nRow := ( nRow * ::nHeightCell ) + ::nHeightHead + ;
         ::nHeightSuper + ::nHeightSpecHd + iif( oCol:l3DLook, 2, 0 )
      nCol := nStartX + iif( oCol:l3DLook, 2, 0 )
      nWidth := ::GetColSizes()[ nCell ] - iif( oCol:l3DLook, 2, 0 )
      nHeight := ::nHeightCell - iif( oCol:l3DLook, 1, -1 )
   ENDIF

   IF oCol:nEditWidthDraw > 0
      nWidth := oCol:nEditWidthDraw
      IF ! ::lNoVScroll
         nWidth -= GetVScrollBarWidth()
      ENDIF
   ENDIF

   IF lHead
      nRow := ::nHeightSuper + iif( oCol:l3DLook, 2, 0 ) + 1
      nHeight := ::nHeightHead
   ELSEIF lFoot
      nRow := _GetClientRect( ::hWnd )[ 4 ] - ::nHeightFoot + 1
      nHeight := ::nHeightFoot
   ENDIF

   ix := GetControlIndex( cBrw, cForm )
   IF _HMG_aControlContainerRow[ ix ] == -1
      nRow += ::nTop - 1
      nCol += ::nLeft
   ELSE
      nRow += _HMG_aControlRow[ ix ] - 1
      nCol += _HMG_aControlCol[ ix ]
   ENDIF

   nRow += ::aEditCellAdjust[ 1 ]
   nCol += ::aEditCellAdjust[ 2 ]
   nWidth += ::aEditCellAdjust[ 3 ] + 2
   nHeight += ::aEditCellAdjust[ 4 ]

   oCell:nRow := nRow
   oCell:nCol := nCol
   oCell:nWidth := nWidth
   oCell:nHeight := nHeight

RETURN oCell

* ===================================================================================================
* METHOD TSBrowse:GetCellSize() returns the cell coordinates taking into account a position of the parent window
* ===================================================================================================

METHOD GetCellSize( nRowPos, nCell, lColSpecHd ) CLASS TSBrowse

   LOCAL nI, nStartX := 0, oCol, cBrw, cForm
   LOCAL nRow, nCol, nWidth, nHeight
   LOCAL lHead := .F., lFoot := .F.
   LOCAL oCell := TSBcell():New()
   LOCAL aRect := { 0, 0, 0, 0 }, y, x

   GetWindowRect( ::hWnd, aRect )

   y := aRect[ 2 ]
   x := aRect[ 1 ]

   IF HB_ISLOGICAL( nRowPos )
      IF nRowPos ; lHead := .T.
      ELSE ; lFoot := .T.
      ENDIF
      nRowPos := NIL
      lColSpecHd := .F.
   ENDIF

   DEFAULT nRowPos := ::nRowPos, ;
      nCell := ::nCell, ;
      lColSpecHd := .F.

   cForm := ::cParentWnd
   cBrw := ::cControlName
   oCol := ::aColumns[ nCell ]

   IF ::nFreeze > 0
      FOR nI := 1 TO Min( ::nFreeze, nCell - 1 )
         nStartX += ::GetColSizes()[ nI ]
      NEXT
   ENDIF

   FOR nI := ::nColPos TO nCell - 1
      nStartX += ::GetColSizes()[ nI ]
   NEXT

   IF lColSpecHd
      nRow := ::nHeightHead + ::nHeightSuper + iif( oCol:l3DLook, 2, 0 )
      nCol := nStartX + iif( oCol:l3DLook, 2, 0 )
      nWidth := ::GetColSizes()[ nCell ] - iif( oCol:l3DLook, 2, 1 )
      nHeight := ::nHeightSpecHd - iif( oCol:l3DLook, 1, -1 )
   ELSE
      nRow := nRowPos - 1
      nRow := ( nRow * ::nHeightCell ) + ::nHeightHead + ;
         ::nHeightSuper + ::nHeightSpecHd + iif( oCol:l3DLook, 1, 0 )
      nCol := nStartX + iif( oCol:l3DLook, 2, -1 )
      nWidth := ::GetColSizes()[ nCell ] - iif( oCol:l3DLook, 4, 1 )
      nHeight := ::nHeightCell - iif( oCol:l3DLook, 3, -1 )
   ENDIF

   IF oCol:nEditWidthDraw > 0
      nWidth := oCol:nEditWidthDraw
      IF ! ::lNoVScroll
         nWidth -= GetVScrollBarWidth()
      ENDIF
   ENDIF

   IF lHead
      nRow := ::nHeightSuper + iif( oCol:l3DLook, 2, 0 ) + 1
      nHeight := ::nHeightHead
   ELSEIF lFoot
      nRow := _GetClientRect( ::hWnd )[ 4 ] - ::nHeightFoot + 1
      nHeight := ::nHeightFoot
   ENDIF

   oCell:nRow := nRow + y
   oCell:nCol := nCol + x
   oCell:nWidth := nWidth + 2
   oCell:nHeight := nHeight

RETURN oCell

* ============================================================================
* FUNCTION lAEqual() Version 9.0 Nov/30/2009 arrays comparison
* ============================================================================

FUNCTION lAEqual( aArr1, aArr2 )

   LOCAL nEle

   IF Empty( aArr1 ) .AND. Empty( aArr2 )
      RETURN .T.
   ELSEIF Empty( aArr1 ) .OR. Empty( aArr2 )
      RETURN .F.
   ELSEIF ValType( aArr1 ) != "A" .OR. ValType( aArr2 ) != "A"
      RETURN .F.
   ELSEIF Len( aArr1 ) != Len( aArr2 )
      RETURN .F.
   ENDIF

   FOR nEle := 1 TO Len( aArr1 )

      IF ValType( aArr1[ nEle ] ) == "A" .AND. ! lAEqual( aArr1[ nEle ], aArr2[ nEle ] )
         RETURN .F.
      ELSEIF ValType( aArr1[ nEle ] ) != ValType( aArr2[ nEle ] )
         RETURN .F.
      ELSEIF !( aArr1[ nEle ] == aArr2[ nEle ] )
         RETURN .F.
      ENDIF
   NEXT

RETURN .T.

* ============================================================================
* FUNCTION StockBmp() Version 9.0 Nov/30/2009
* ============================================================================

FUNCTION StockBmp( uAnsi, oWnd, cPath, lNew )

   LOCAL cBmp, nHandle, nWrite, hBmp, cBmpFile, cName, ;
      cTmp := AllTrim( GetEnv( "TMP" ) )

   LOCAL aStock := { "42 4D F6 00 00 00 00 00 00 00 76 00 00 00 28 00" + ; // calendar
      "00 00 10 00 00 00 10 00 00 00 01 00 04 00 00 00" + ;
      "00 00 80 00 00 00 C4 0E 00 00 C4 0E 00 00 00 00" + ;
      "00 00 00 00 00 00 00 00 00 00 00 00 80 00 00 80" + ;
      "00 00 00 80 80 00 80 00 00 00 80 00 80 00 80 80" + ;
      "00 00 80 80 80 00 C0 C0 C0 00 00 00 FF 00 00 FF" + ;
      "00 00 00 FF FF 00 FF 00 00 00 FF 00 FF 00 FF FF" + ;
      "00 00 FF FF FF 00 88 88 88 88 88 88 88 88 88 88" + ;
      "88 88 88 88 88 88 88 77 77 77 77 77 77 78 80 00" + ;
      "00 00 00 00 00 08 80 77 77 77 07 77 77 08 80 FF" + ;
      "FF F7 FF FF FF 08 80 FF FF F7 FF FF FF 08 80 F0" + ;
      "00 07 F0 00 0F 08 80 F9 99 F7 FF 99 9F 08 80 FF" + ;
      "9F F7 FF 99 FF 08 80 FF 9F F7 FF F9 9F 08 80 F9" + ;
      "9F 00 0F 99 9F 08 80 F9 9F F7 FF 99 9F 08 80 FF" + ;
      "FF F7 FF FF FF 08 80 00 00 08 00 00 00 08 88 88" + ;
      "88 88 88 88 88 88", ;
      "42 4D F6 00 00 00 00 00 00 00 76 00 00 00 28 00" + ; // spinner
      "00 00 10 00 00 00 10 00 00 00 01 00 04 00 00 00" + ;
      "00 00 80 00 00 00 00 00 00 00 00 00 00 00 00 00" + ;
      "00 00 00 00 00 00 00 00 00 00 00 00 80 00 00 80" + ;
      "00 00 00 80 80 00 80 00 00 00 80 00 80 00 80 80" + ;
      "00 00 80 80 80 00 C0 C0 C0 00 00 00 FF 00 00 FF" + ;
      "00 00 00 FF FF 00 FF 00 00 00 FF 00 FF 00 FF FF" + ;
      "00 00 FF FF FF 00 FF FF FF FF FF FF FF FF FE EE" + ;
      "EE EE EE EE EE FF EE EE EE E6 EE EE EE EF EE EE" + ;
      "EE 66 6E EE EE EF EE EE E6 66 66 EE EE EF EE EE" + ;
      "66 6E 66 6E EE EF EE EE EE EE EE EE EE EF 88 88" + ;
      "88 88 88 88 88 8F EE EE EE EE EE EE EE EF EE EE" + ;
      "66 6E 66 6E EE EF EE EE E6 66 66 EE EE EF EE EE" + ;
      "EE 66 6E EE EE EF EE EE EE E6 EE EE EE EF EE EE" + ;
      "EE EE EE EE EE EF FE EE EE EE EE EE EE FF FF FF" + ;
      "FF FF FF FF FF FF", ;
      "42 4D F6 00 00 00 00 00 00 00 76 00 00 00 28 00" + ; // selector
      "00 00 10 00 00 00 10 00 00 00 01 00 04 00 00 00" + ;
      "00 00 80 00 00 00 C4 0E 00 00 C4 0E 00 00 00 00" + ;
      "00 00 00 00 00 00 00 00 00 00 00 00 80 00 00 80" + ;
      "00 00 00 80 80 00 80 00 00 00 80 00 80 00 80 80" + ;
      "00 00 C0 C0 C0 00 80 80 80 00 00 00 FF 00 00 FF" + ;
      "00 00 00 FF FF 00 FF 00 00 00 FF 00 FF 00 FF FF" + ;
      "00 00 FF FF FF 00 77 77 77 77 77 77 77 77 77 77" + ;
      "77 77 77 77 77 77 77 77 77 77 77 77 77 77 77 77" + ;
      "77 70 77 77 77 77 77 77 77 70 07 77 77 77 77 77" + ;
      "77 70 00 77 77 77 77 77 77 70 00 07 77 77 77 77" + ;
      "77 70 00 00 77 77 77 77 77 70 00 00 07 77 77 77" + ;
      "77 70 00 00 77 77 77 77 77 70 00 07 77 77 77 77" + ;
      "77 70 00 77 77 77 77 77 77 70 07 77 77 77 77 77" + ;
      "77 70 77 77 77 77 77 77 77 77 77 77 77 77 77 77" + ;
      "77 77 77 77 77 77", ;
      "42 4D DE 00 00 00 00 00 00 00 76 00 00 00 28 00" + ; // sort ascend
      "00 00 0D 00 00 00 0D 00 00 00 01 00 04 00 00 00" + ;
      "00 00 68 00 00 00 00 00 00 00 00 00 00 00 00 00" + ;
      "00 00 00 00 00 00 00 00 00 00 00 00 80 00 00 80" + ;
      "00 00 00 80 80 00 80 00 00 00 80 00 80 00 80 80" + ;
      "00 00 80 80 80 00 C0 C0 C0 00 00 00 FF 00 00 FF" + ;
      "00 00 00 FF FF 00 FF 00 00 00 FF 00 FF 00 FF FF" + ;
      "00 00 FF FF FF 00 88 88 88 88 88 88 80 00 88 88" + ;
      "88 88 88 88 80 00 88 88 88 88 88 88 80 00 88 87" + ;
      "FF FF FF F8 80 00 88 87 78 88 8F F8 80 00 88 88" + ;
      "78 88 8F 88 80 00 88 88 77 88 FF 88 80 00 88 88" + ;
      "87 88 F8 88 80 00 88 88 87 7F F8 88 80 00 88 88" + ;
      "88 7F 88 88 80 00 88 88 88 88 88 88 80 00 88 88" + ;
      "88 88 88 88 80 00 88 88 88 88 88 88 80 00", ;
      "42 4D DE 00 00 00 00 00 00 00 76 00 00 00 28 00" + ; // sort descend
      "00 00 0D 00 00 00 0D 00 00 00 01 00 04 00 00 00" + ;
      "00 00 68 00 00 00 00 00 00 00 00 00 00 00 00 00" + ;
      "00 00 00 00 00 00 00 00 00 00 00 00 80 00 00 80" + ;
      "00 00 00 80 80 00 80 00 00 00 80 00 80 00 80 80" + ;
      "00 00 80 80 80 00 C0 C0 C0 00 00 00 FF 00 00 FF" + ;
      "00 00 00 FF FF 00 FF 00 00 00 FF 00 FF 00 FF FF" + ;
      "00 00 FF FF FF 00 88 88 88 88 88 88 80 00 88 88" + ;
      "88 88 88 88 80 00 88 88 88 88 88 88 80 00 88 88" + ;
      "88 7F 88 88 80 00 88 88 87 7F F8 88 80 00 88 88" + ;
      "87 88 F8 88 80 00 88 88 77 88 FF 88 80 00 88 88" + ;
      "78 88 8F 88 80 00 88 87 78 88 8F F8 80 00 88 87" + ;
      "77 77 77 F8 80 00 88 88 88 88 88 88 80 00 88 88" + ;
      "88 88 88 88 80 00 88 88 88 88 88 88 80 00", ;
      "42 4D 4E 01 00 00 00 00 00 00 76 00 00 00 28 00" + ; // check box checked
      "00 00 12 00 00 00 12 00 00 00 01 00 04 00 00 00" + ;
      "00 00 D8 00 00 00 00 00 00 00 00 00 00 00 00 00" + ;
      "00 00 00 00 00 00 00 00 00 00 00 00 80 00 00 80" + ;
      "00 00 00 80 80 00 80 00 00 00 80 00 80 00 80 80" + ;
      "00 00 80 80 80 00 C0 C0 C0 00 00 00 FF 00 00 FF" + ;
      "00 00 00 FF FF 00 FF 00 00 00 FF 00 FF 00 FF FF" + ;
      "00 00 FF FF FF 00 88 88 88 88 88 88 88 88 88 00" + ;
      "00 00 8F FF FF FF FF FF FF FF F8 00 00 00 8F 00" + ;
      "00 00 00 00 00 00 F8 00 00 00 8F 08 88 88 88 88" + ;
      "88 80 F8 00 00 00 8F 08 88 8F 88 88 88 80 F8 00" + ;
      "00 00 8F 08 88 F0 F8 88 88 80 F8 00 00 00 8F 08" + ;
      "8F 00 0F 88 88 80 F8 00 00 00 8F 08 F0 08 00 F8" + ;
      "88 80 F8 00 00 00 8F 08 00 88 80 0F 88 80 F8 00" + ;
      "00 00 8F 08 88 88 88 00 F8 80 F8 00 00 00 8F 08" + ;
      "88 88 88 80 0F 80 F8 00 00 00 8F 08 88 88 88 88" + ;
      "00 F0 F8 00 00 00 8F 08 88 88 88 88 80 80 F8 00" + ;
      "00 00 8F 08 88 88 88 88 88 80 F8 00 00 00 8F 08" + ;
      "88 88 88 88 88 80 F8 00 00 00 8F 00 00 00 00 00" + ;
      "00 00 F8 00 00 00 8F FF FF FF FF FF FF FF F8 00" + ;
      "00 00 88 88 88 88 88 88 88 88 88 00 00 00", ;
      "42 4D 4E 01 00 00 00 00 00 00 76 00 00 00 28 00" + ; // check box unchecked
      "00 00 12 00 00 00 12 00 00 00 01 00 04 00 00 00" + ;
      "00 00 D8 00 00 00 00 00 00 00 00 00 00 00 00 00" + ;
      "00 00 00 00 00 00 00 00 00 00 00 00 80 00 00 80" + ;
      "00 00 00 80 80 00 80 00 00 00 80 00 80 00 80 80" + ;
      "00 00 80 80 80 00 C0 C0 C0 00 00 00 FF 00 00 FF" + ;
      "00 00 00 FF FF 00 FF 00 00 00 FF 00 FF 00 FF FF" + ;
      "00 00 FF FF FF 00 88 88 88 88 88 88 88 88 88 00" + ;
      "00 00 8F FF FF FF FF FF FF FF F8 00 00 00 8F 00" + ;
      "00 00 00 00 00 00 F8 00 00 00 8F 08 88 88 88 88" + ;
      "88 80 F8 00 00 00 8F 08 88 88 88 88 88 80 F8 00" + ;
      "00 00 8F 08 88 88 88 88 88 80 F8 00 00 00 8F 08" + ;
      "88 88 88 88 88 80 F8 00 00 00 8F 08 88 88 88 88" + ;
      "88 80 F8 00 00 00 8F 08 88 88 88 88 88 80 F8 00" + ;
      "00 00 8F 08 88 88 88 88 88 80 F8 00 00 00 8F 08" + ;
      "88 88 88 88 88 80 F8 00 00 00 8F 08 88 88 88 88" + ;
      "88 80 F8 00 00 00 8F 08 88 88 88 88 88 80 F8 00" + ;
      "00 00 8F 08 88 88 88 88 88 80 F8 00 00 00 8F 08" + ;
      "88 88 88 88 88 80 F8 00 00 00 8F 00 00 00 00 00" + ;
      "00 00 F8 00 00 00 8F FF FF FF FF FF FF FF F8 00" + ;
      "00 00 88 88 88 88 88 88 88 88 88 00 00 00" }

   LOCAL aStkName := { "SCalen.bmp", "SSpinn.bmp", "SSelec.bmp", "SSAsc.bmp", "SSDesc.bmp", "SCheck.bmp", "SUncheck.bmp" }

   DEFAULT uAnsi := aStock[ 1 ], ;
      cPath := "", ;
      lNew := .F.

   HB_SYMBOL_UNUSED( oWnd )

   IF ValType( uAnsi ) == "N" .AND. uAnsi <= Len( aStkName )
      cName := aStkName[ uAnsi ]
      uAnsi := StrTran( aStock[ uAnsi ], " " )
   ELSEIF ValType( uAnsi ) == "N"
      uAnsi := StrTran( aStock[ 1 ], " " ) // calendar
      cName := aStkName[ 1 ]
   ELSE
      uAnsi := StrTran( uAnsi, " " )
      cName := iif( ! Empty( cPath ) .AND. ".BMP" $ Upper( cPath ), "", "STmp.bmp" )
   ENDIF

   cBmp := cAnsi2Bmp( uAnsi )

   IF Empty( cTmp )
      IF Empty( cTmp := AllTrim( GetEnv( "TEMP" ) ) )
         cTmp := CurDir()
      ENDIF
   ENDIF

   cBmpFile := iif( ! Empty( cPath ), cPath + iif( Right( cPath ) != "\", "\", "" ), cTmp + "\" ) + cName
   cBmpFile := StrTran( cBmpFile, "\\", "\" )

   IF ! File( cBmpFile )

      IF ( nHandle := FCreate( cBmpFile ) ) < 0
         RETURN NIL
      ENDIF

      nWrite := FWrite( nHandle, cBmp, Len( cBmp ) )

      IF nWrite < Len( cBmp )
         FClose( nHandle )
         RETURN NIL
      ENDIF

      FClose( nHandle )
   ENDIF

   hBmp := LoadImage( cBmpFile )

   FErase( cBmpFile )

RETURN hBmp

* ============================================================================
* FUNCTION cAnsi2Bmp() Version 9.0 Nov/30/2009
* ============================================================================

STATIC FUNCTION cAnsi2Bmp( cAnsi )

   LOCAL cLong, ;
      cBmp := ""

   WHILE Len( cAnsi ) >= 8
      cLong := Left( cAnsi, 8 )
      cBmp += cHex2Bin( cAnsi2Hex( cLong ) )
      cAnsi := Stuff( cAnsi, 1, 8, "" )
   ENDDO

   IF ! Empty( cAnsi )
      cBmp += cHex2Bin( cAnsi2Hex( PadR( cAnsi, 4, "0" ) ) )
   ENDIF

RETURN cBmp

* ============================================================================
* FUNCTION cAnsi2Hex() Version 9.0 Nov/30/2009
* ============================================================================

STATIC FUNCTION cAnsi2Hex( cAnsi )

   LOCAL cDig, ;
      cHex := ""

   cAnsi := AllTrim( cAnsi )

   WHILE Len( cAnsi ) >= 2
      cDig := Left( cAnsi, 2 )
      cHex := cDig + cHex
      cAnsi := Stuff( cAnsi, 1, 2, "" )
   ENDDO

RETURN cHex

* ============================================================================
* FUNCTION cHex2Bin() Version 9.0 Nov/30/2009
* ============================================================================

STATIC FUNCTION cHex2Bin( cHex )

   LOCAL nPos, nEle, ;
      nExp := 0, ;
      nDec := 0, ;
      aHex := { "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F" }

   cHex := AllTrim( cHex )

   FOR nPos := Len( cHex ) TO 1 STEP -1
      nEle := Max( 0, AScan( aHex, SubStr( cHex, nPos, 1 ) ) - 1 )
      nDec += ( nEle * ( 16 ** nExp ) )
      nExp++
   NEXT

RETURN iif( Len( cHex ) > 4, L2Bin( Int( nDec ) ), iif( Len( cHex ) > 2, I2Bin( Int( nDec ) ), Chr( Int( nDec ) ) ) )

* ============================================================================
* FUNCTION nBmpWidth() Version 9.0 Nov/30/2009
* ============================================================================

STATIC FUNCTION nBmpWidth( hBmp )

RETURN GetBitmapSize( hBmp ) [1]

* ============================================================================
* FUNCTION _nColumn() Version 9.0 Nov/30/2009
* ============================================================================

FUNCTION _nColumn( oBrw, cName, lPos )

   LOCAL nPos := AScan( oBrw:aColumns, {| oCol | Upper( oCol:cName ) == Upper( cName ) } )

RETURN iif( Empty( lPos ), Max( nPos, 1 ), nPos )

//--------------------------------------------------------------------------------------------------------------------//

STATIC FUNCTION HeadXls( nCol )

RETURN iif( nCol > 26, Chr( Int( ( nCol - 1 ) / 26 ) + 64 ), '' ) + Chr( ( nCol - 1 ) % 26 + 65 )


#ifdef __XHARBOUR__

FUNCTION aHash2Array( uAlias )

   LOCAL nEle, ;
      aArr := {}

   FOR nEle := 1 TO Len( uAlias )
      AAdd( aArr, { HGetKeyAt( uAlias, nEle ), HGetValueAt( uAlias, nEle ) } )
   NEXT

RETURN aArr

FUNCTION hb_HGetDef( hHash, xKey, xDef )

   LOCAL nPos := HGetPos( hHash, xKey )

RETURN iif( nPos > 0, HGetValueAt( hHash, nPos ), xDef )

#endif

//--------------------------------------------------------------------------------------------------------------------//

STATIC FUNCTION ProcNL( nVal )

   DEFAULT nVal := 0
   nVal++

RETURN "Called from: " + ProcName( nVal ) + "(" + hb_ntos( ProcLine( nVal ) ) + ") --> " + ProcFile( nVal )
