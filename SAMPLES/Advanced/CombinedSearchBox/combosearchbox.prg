/*
  MINIGUI - Harbour Win32 GUI library Demo/Sample

  Copyright 2002-09 Roberto Lopez <harbourminigui@gmail.com>
  http://harbourminigui.googlepages.com

  CSBox ( Combined Search Box )

  Started by Bicahi Esgici <esgici@gmail.com>

  Enhanced by S.Rathinagiri <srgiri@dataone.in>

  Revised by Grigory Filatov <gfilatov@inbox.ru>

  Revised By Pierpaolo Martinello pier.martinello [at] alice.it

*/

#include "minigui.ch"

PROCEDURE _DefineComboSearchBox( cCSBoxName, ;
      cCSBoxParent, ;
      cCSBoxCol, ;
      cCSBoxRow, ;
      cCSBoxWidth, ;
      cCSBoxHeight, ;
      cCSBoxValue, ;
      cFontName, ;
      nFontSize, ;
      cToolTip, ;
      nMaxLenght, ;
      lUpper, ;
      lLower, ;
      lNumeric, ;
      bLostFocus, ;
      bGotFocus, ;
      bEnter, ;
      lRightAlign, ;
      nHelpId, ;
      lBold, ;
      lItalic, ;
      lUnderline, ;
      aBackColor, ;
      aFontColor, ;
      lNoTabStop, ;
      aArray, ;
      bSaveList )

   LOCAL cParentName := ''

   DEFAULT cCSBoxWidth  := 120
   DEFAULT cCSBoxHeight := 24
   DEFAULT cCSBoxValue  := ""
   DEFAULT bGotFocus    := ""
   DEFAULT bLostFocus   := ""
   DEFAULT nMaxLenght   := 255
   DEFAULT lUpper       := .F.
   DEFAULT lLower       := .F.
   DEFAULT lNumeric     := .F.
   DEFAULT bEnter       := ""

   IF _HMG_BeginWindowActive
      cParentName := _HMG_ActiveFormName
   ELSE
      cParentName := cCSBoxParent
   ENDIF

   DEFINE TEXTBOX &cCSBoxName
      PARENT &cCSBoxParent
      ROW cCSBoxRow
      COL cCSBoxCol
      WIDTH cCSBoxWidth
      HEIGHT cCSBoxHeight
      VALUE cCSBoxValue
      FONTNAME cFontName
      FONTSIZE nFontSize
      TOOLTIP cToolTip
      MAXLENGTH nMaxLenght
      UPPERCASE lUpper
      LOWERCASE lLower
      NUMERIC lNumeric
      ONLOSTFOCUS iif( ISBLOCK( bLostFocus ), Eval( bLostFocus ), NIL )
      ONGOTFOCUS iif( ISBLOCK( bGotFocus ), Eval( bGotFocus ), NIL )
      ONENTER iif( ISBLOCK( bEnter ), Eval( bEnter ), NIL )
      ONCHANGE CreateCSBox( cParentName, cCSBoxName, aArray, cCSBoxRow, cCSBoxCol, bSaveList )
      RIGHTALIGN lRightAlign
      HELPID nHelpId
      FONTBOLD lBold
      FONTITALIC lItalic
      FONTUNDERLINE lUnderline
      BACKCOLOR aBackColor
      FONTCOLOR aFontColor
      TABSTOP lNoTabStop
   END TEXTBOX

RETURN // _DefineComboSearchBox()
/*
*/
*------------------------------------------------------------------------------*
STATIC PROCEDURE CreateCSBox( cParentName, cCSBoxName, aitems, cCSBoxRow, cCSBoxCol, bSaveList )
*------------------------------------------------------------------------------*
   LOCAL nFormRow := thisWindow.ROW
   LOCAL nFormCol := thisWindow.COL
   LOCAL nControlRow := this.ROW + 3 + if ( isVistaOrLater(), GetBorderHeight() / 2, 0 )
   LOCAL nControlCol := this.COL + 2 + if ( os_isWinXP(), -1, 1 )

   LOCAL nControlWidth := this.WIDTH - if ( _HMG_AutoAdjust, 1, 0 )
   LOCAL nControlHeight := this.HEIGHT
   LOCAL cCurValue := this.VALUE
   LOCAL cFontname := this.FONTNAME
   LOCAL nFontsize := this.FONTSIZE
   LOCAL cTooltip := this.TOOLTIP
   LOCAL lFontbold := this.FONTBOLD
   LOCAL lFontitalic := this.fontitalic
   LOCAL lFontunderline := this.fontunderline
   LOCAL aBackcolor := this.BACKCOLOR
   LOCAL aFontcolor := this.FONTCOLOR
   LOCAL aResults := {}
   LOCAL nContIndx := GetControlIndex( this.NAME, thiswindow.name )
   LOCAL nItemNo
   LOCAL nListBoxHeight
   LOCAL nCaret := this.CaretPos

   LOCAL cCSBxName := 'frm' + cCSBoxName

   IF ! Empty( cCurValue )

      IF _HMG_aControlContainerRow[ nContIndx ] # -1
         ncontrolrow += _HMG_aControlContainerRow[ nContIndx ]
         ncontrolcol += _HMG_aControlContainerCol[ nContIndx ]
      ENDIF

      FOR nItemNo := 1 TO Len( aitems )
         IF Upper( aitems[ nItemNo ] ) == Upper( cCurValue )
            EXIT // item selected already
         ENDIF
         IF Upper( Left( aitems[ nItemNo ], Len( cCurValue ) ) ) == Upper( cCurValue )
            AAdd( aResults, aitems[ nItemNo ] )
         ENDIF
      NEXT nItemNo

      IF Len( aResults ) > 0

         nListBoxHeight := Max( Min( ( Len(aResults ) * 16 ) + 6, thiswindow.HEIGHT - nControlRow - nControlHeight - 14 ), 40 )

         DEFINE WINDOW &cCSBxName ;
               AT nFormRow + nControlRow + GetTitleHeight(), nFormCol + nControlCol + GetBorderWidth() / 2 ;
               WIDTH nControlWidth + 2 ;
               HEIGHT nListBoxHeight ;
               MODAL ;
               NOCAPTION ;
               NOSIZE ;
               ON INIT SetProperty( cCSBxName, '_cstext', "CaretPos", nCaret )

            ON KEY UP ACTION _CSDoUpKey()
            ON KEY DOWN ACTION _CSDoDownKey()
            ON KEY ESCAPE ACTION _CSDoEscKey( cParentName, cCSBoxName )
            ON KEY DELETE ACTION _DelArgList( aItems )

            DEFINE TEXTBOX _cstext
               ROW 1
               COL 1
               WIDTH nControlWidth
               HEIGHT nControlHeight
               FONTNAME cFontname
               FONTSIZE nFontsize
               TOOLTIP cTooltip
               FONTBOLD lFontbold
               FONTITALIC lFontitalic
               FONTUNDERLINE lFontunderline
               BACKCOLOR aBackcolor
               FONTCOLOR aFontcolor
               ON CHANGE _CSTextChanged( cParentName, aitems )
               ON ENTER _CSItemSelected( cParentName, cCSBoxName, aitems, bSaveList )
            END TEXTBOX

            DEFINE LISTBOX _cslist
               ROW nControlHeight
               COL 1
               WIDTH nControlWidth
               HEIGHT nListBoxHeight
               ITEMS aResults
               FONTNAME cFontname
               FONTSIZE nFontsize
               ON DBLCLICK _CSItemSelected( cParentName, cCSBoxName )
               VALUE 1
            END LISTBOX

         END WINDOW

         SetProperty( cCSBxName, '_cstext', "VALUE", cCurValue )

         ACTIVATE WINDOW &cCSBxName

      ENDIF

   ENDIF

RETURN // CreateCSBox()
/*
*/
*------------------------------------------------------------------------------*
STATIC PROCEDURE _CSTextChanged( cParentName, aItems )
*------------------------------------------------------------------------------*
   LOCAL cCurValue := GetProperty( ThisWindow.NAME, '_cstext', "VALUE" )
   LOCAL aResults := {}
   LOCAL nListBoxHeight
   LOCAL nParentHeight := GetProperty( cParentName, "HEIGHT" )
   LOCAL nParentRow := GetProperty( cParentName, "ROW" )

   DoMethod( ThisWindow.NAME, "_csList", 'DeleteAllItems' )

   // 06/01/2020 Changed By Pierpaolo
   AEval( aItems, {| x | if( Upper( Left(x, Len(cCurValue ) ) ) == Upper( cCurValue ), AAdd( aResults, x ), NIL ) } )

   AEval( aResults, {| x | DoMethod( ThisWindow.NAME, "_csList", 'AddItem', x ) } )

   SetProperty( ThisWindow.NAME, "_csList", "VALUE", 1 )
   // 06/01/2020 End of Change
   //
   nListBoxHeight := Max( Min( ( Len(aResults ) * 16 ) + 6, ( nParentHeight + nParentRow - ;
      GetProperty( ThisWindow.NAME, 'ROW' ) - ;
      GetProperty( ThisWindow.NAME, "_csText", 'ROW' ) - ;
      GetProperty( ThisWindow.NAME, "_csText", 'HEIGHT' ) - 14 ) ), 40 )

   SetProperty( ThisWindow.NAME, "_csList", "HEIGHT", nListBoxHeight - GetBorderHeight() )
   SetProperty( ThisWindow.NAME, "HEIGHT", nListBoxHeight + GetProperty( ThisWindow.NAME, '_cstext', "HEIGHT" ) - GetBorderHeight() )

RETURN // _CSTextChanged()
/*
*/
*------------------------------------------------------------------------------*
STATIC FUNCTION _CSItemSelected( cParentName, cTxBName, aitems, bSaveList )
*------------------------------------------------------------------------------*
   LOCAL nListValue
   LOCAL cListItem
   LOCAL cRtv := GetProperty( ThisWindow.NAME, '_csList', "VALUE" )
   LOCAL cCv := GetProperty( thiswindow.NAME, "_CSText", "VALUE" )

   IF cRtv > 0

      nListValue := GetProperty( ThisWindow.NAME, '_csList', "VALUE" )
      cListItem := GetProperty( ThisWindow.NAME, '_csList', "ITEM", nListValue )

      SetProperty( cParentName, cTxBName, "VALUE", cListItem )

      SetProperty( cParentName, cTxBName, "CARETPOS", ;
         Len( GetProperty( ThisWindow.NAME, '_csList', "ITEM", cRtv ) ) )

      DoMethod( ThisWindow.NAME, "Release" )
      // 06/01/2020 Add By Pierpaolo
      DoMethod( cParentName, cTxBName, 'SetFocus' )
      _PushKey( VK_RETURN )

   ELSE

      IF msgYesNo( cCv + CRLF + CRLF + " Accept this new value ?", "Question!", .T. )

         SetProperty( cParentName, cTxBName, "VALUE", cCv )

         SetProperty( cParentName, cTxBName, "CARETPOS", Len( cCv ) )

         AAdd ( aitems, cCv )

         ASort ( aitems )

         _CSTextChanged( cParentName, aitems )

         _CSItemSelected( cParentName, cTxBName, aitems )

      ELSE
         DoMethod( cParentName, cTxBName, 'SetFocus' )
      ENDIF

   ENDIF

RETURN aitems // _CSItemSelected()
/*
*/
*------------------------------------------------------------------------------*
STATIC PROCEDURE _CSDoUpKey()
*------------------------------------------------------------------------------*

   IF GetProperty( ThisWindow.NAME, '_csList', "ItemCount" ) > 0 .AND. ;
         GetProperty( ThisWindow.NAME, '_csList', "VALUE" ) > 1

      SetProperty( ThisWindow.NAME, '_csList', "VALUE", GetProperty( ThisWindow.NAME, '_csList', "VALUE" ) - 1 )

   ENDIF

RETURN // _CSDoUpKey()
/*
*/
*------------------------------------------------------------------------------*
STATIC PROCEDURE _CSDoDownKey()
*------------------------------------------------------------------------------*
   IF GetProperty( ThisWindow.NAME, '_csList', "ItemCount" ) > 0 .AND. ;
         GetProperty( ThisWindow.NAME, '_csList', "VALUE" ) < ;
         GetProperty( ThisWindow.NAME, '_csList', "ItemCount" )

      SetProperty( ThisWindow.NAME, '_csList', "VALUE", GetProperty( ThisWindow.NAME, '_csList', "VALUE" ) + 1 )

   ENDIF

RETURN // _CSDoDownKey()
/*
*/
*------------------------------------------------------------------------------*
STATIC PROCEDURE _CSDoEscKey( cParentName, cCSBoxName )
*------------------------------------------------------------------------------*
   SetProperty( cParentName, cCSBoxName, "VALUE", '' )

   DoMethod( ThisWindow.NAME, "Release" )

RETURN // _CSDoEscKey()
/*
      * 06/01/2020 Add By Pierpaolo
*/
*------------------------------------------------------------------------------*
STATIC PROCEDURE _DelArgList( aList )
*------------------------------------------------------------------------------*
   LOCAL nVal := GetProperty( ThisWindow.NAME, '_csList', "VALUE" )
   LOCAL nDval := AScan( aList, GetProperty( ThisWindow.NAME, '_csList', "ITEM", nval ) )

   IF GetProperty( ThisWindow.NAME, '_csList', "ItemCount" ) > 0 .AND. ;
         nVal <= GetProperty( ThisWindow.NAME, '_csList', "ItemCount" )

      IF msgYesNo( aList[ nDval ] + CRLF + CRLF + " Delete this value ?", "Question!", .T. )
         alist := hb_ADel( aList, ndval, .T. )
         DoMethod( ThisWindow.NAME, '_csList', "deleteitem", nval )
      ENDIF
   ENDIF

RETURN
