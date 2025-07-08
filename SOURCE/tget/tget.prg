/*
 * $Id: tget.prg,v1.116 2007/04/20 19:47:45 vszakats Exp $
 */

/*
 * Harbour Project source code:
 * Get Class
 *
 * Copyright 1999 Ignacio Ortiz de Z£niga <ignacio@fivetech.com>
 * www - https://harbour.github.io/
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "SET_COMPILE_HMG_UNICODE.ch"

#include "minigui.ch"
#include "hbclass.ch"

#include "color.ch"
#include "setcurs.ch"
#include "getexit.ch"
#include "inkey.ch"
#include "button.ch"
#include "hblang.ch"

#if ( __HARBOUR__ - 0 < 0x030200 )
  #xtranslate hb_UAt( <c>, <n> ) => At( <c>, <n> )
  #xtranslate hb_ULeft( <c>, <n> ) => Left( <c>, <n> )
  #xtranslate hb_URight( <c>, <n> ) => Right( <c>, <n> )
  #xtranslate hb_ULen( <c> ) => Len( <c> )
  #xtranslate hb_UPadL( <c>, <n> ) => PadL( <c>, <n> )
  #xtranslate hb_UPadR( <c>, <n> [, <l>] ) => PadR( <c>, <n>[, <l>] )
  #xtranslate hb_USubStr( <c>, <n> [, <e>] ) => SubStr( <c>, <n> [, <e>] )
#endif

/* TODO: :posInBuffer( <nRow>, <nCol> ) --> nPos
         Determines a position within the edit buffer based on screen
         coordinates.
         Xbase++ compatible method */

#define GET_CLR_UNSELECTED      0
#define GET_CLR_ENHANCED        1

#define WM_LBUTTONDBLCLK        0x0203

#xtranslate _GetKeyState( <VKey> ) ;
      => ;
      CheckBit( GetKeyState( <VKey> ), 32768 )

/*-------------------------------------------------------------------------*/

CLASS HMG_Get

   EXPORTED:

   DATA BadDate        INIT .F.
   DATA Buffer
   DATA Cargo
   DATA Changed        INIT .F.
   DATA Clear          INIT .F.
   DATA Col
   DATA DecPos         INIT 0 // ; CA-Cl*pper NG says that it contains NIL, but in fact it contains zero.
   DATA ExitState
   DATA HasFocus       INIT .F.
   DATA Minus          INIT .F.
   DATA Name
   DATA Initial
   DATA Original
   DATA Pos            INIT 0
   DATA PostBlock
   DATA PreBlock
   DATA Reader
   DATA Rejected       INIT .F.
   DATA Row
   DATA SubScript
   DATA TypeOut        INIT .F.
#ifdef HB_COMPAT_C53
   DATA Control
   DATA Message
   DATA Caption        INIT ""
   DATA CapRow         INIT 0
   DATA CapCol         INIT 0
#endif
   DATA aKeyEvent      INIT {}

   PROTECTED:

   DATA cColorSpec
   DATA cPicture
   DATA bBlock
   DATA cType

   DATA cPicMask       INIT ""
   DATA cPicFunc       INIT ""
   DATA nMaxLen
   DATA lEdit          INIT .F.
   DATA lDecRev        INIT .F.
   DATA lPicComplex    INIT .F.
   DATA nDispLen
   DATA nDispPos       INIT 1
   DATA nOldPos        INIT 0
   DATA lCleanZero     INIT .F.
   DATA cDelimit
   DATA nMaxEdit
   DATA lMinus         INIT .F.
   DATA lMinusPrinted  INIT .F.
   DATA xVarGet

   VISIBLE:

   /* NOTE: This method is a Harbour extension */
   METHOD New( nRow, nCol, bVarBlock, cVarName, cPicture, cColorSpec )

   METHOD Assign()
#ifdef HB_COMPAT_XPP
   MESSAGE _Assign METHOD Assign()
#endif
   METHOD Type()
#ifdef HB_COMPAT_C53
   METHOD HitTest( nMRow, nMCol )
#endif
   METHOD Block( bBlock ) SETGET
   METHOD ColorSpec( cColorSpec ) SETGET
   METHOD Picture( cPicture ) SETGET
   /* NOTE: lForced is an undocumented Harbour parameter. Should not be used by app code. */
   METHOD Display( lForced )
   METHOD ColorDisp( cColorSpec )
   METHOD KillFocus()
   METHOD Reset()
   METHOD SetFocus()
   METHOD Undo()
   METHOD UnTransform( cBuffer )
   METHOD UpdateBuffer()
#ifdef HB_C52_UNDOC
   METHOD Reform()
#endif

   METHOD VarGet()
   /* NOTE: lReFormat is an undocumented Harbour parameter. Should not be used by app code. */
   METHOD VarPut( xValue, lReFormat )

   METHOD End()
#ifdef HB_COMPAT_XPP
   MESSAGE _End METHOD End()
#endif
   METHOD Home()
   MESSAGE Left( lDisplay ) METHOD _Left( lDisplay )
   MESSAGE Right( lDisplay ) METHOD _Right( lDisplay )
   METHOD ToDecPos()
   METHOD WordLeft()
   METHOD WordRight()

   /* NOTE: lDisplay is an undocumented Harbour parameter. Should not be used by app code. */
   METHOD BackSpace( lDisplay )
   MESSAGE Delete( lDisplay ) METHOD _Delete( lDisplay )
   METHOD DelEnd()
   METHOD DelLeft()
   METHOD DelRight()
   METHOD DelWordLeft()
   METHOD DelWordRight()

   METHOD Insert( cChar )
   METHOD OverStrike( cChar )

   METHOD SetKeyEvent( nKey, bKey, lCtrl, lShift, lAlt )
   METHOD DoKeyEvent ( nKey )

   METHOD Refresh()

   PROTECTED:

   METHOD DeleteAll()
   METHOD IsEditable( nPos )
   METHOD Input( cChar )
   METHOD PutMask( xValue, lEdit )
   METHOD FirstEditable()
   METHOD LastEditable()

ENDCLASS

/*-------------------------------------------------------------------------*/

METHOD New( nRow, nCol, bVarBlock, cVarName, cPicture, cColorSpec ) CLASS HMG_Get

   DEFAULT nRow       TO Row()
   DEFAULT nCol       TO Col()
   DEFAULT cVarName   TO ""
   DEFAULT bVarBlock  TO iif( ISCHARACTER( cVarName ), MemVarBlock( cVarName ), NIL )
   DEFAULT cColorSpec TO hb_ColorIndex( SetColor(), CLR_UNSELECTED ) + "," + hb_ColorIndex( SetColor(), CLR_ENHANCED )

   ::Row           := nRow
   ::Col           := nCol
   ::bBlock        := bVarBlock
   ::Name          := cVarName
   ::Picture       := cPicture
   ::ColorSpec     := cColorSpec
   IF Set( _SET_DELIMITERS )
      ::cDelimit   := Set( _SET_DELIMCHARS )
   ENDIF

RETURN Self

/*-------------------------------------------------------------------------*/

METHOD Assign() CLASS HMG_Get

   IF ::HasFocus
      ::VarPut( ::UnTransform(), .F. )
   ENDIF

RETURN Self

/*-------------------------------------------------------------------------*/

METHOD UpdateBuffer() CLASS HMG_Get

   IF ::HasFocus
      ::Buffer := ::PutMask( ::VarGet() )
      ::Display()
   ELSE
      ::PutMask( ::VarGet() )
   ENDIF

RETURN Self

/*-------------------------------------------------------------------------*/

#ifdef HB_C52_UNDOC

METHOD Reform() CLASS HMG_Get

   IF ::HasFocus
      ::Buffer := ::PutMask( ::xVarGet, .F. )
   ENDIF

RETURN Self

#endif

/*-------------------------------------------------------------------------*/

METHOD Display( lForced ) CLASS HMG_Get

   LOCAL nOldCursor := SetCursor( SC_NONE )
   LOCAL cBuffer
   LOCAL nDispPos

   DEFAULT lForced TO .T.

   IF ::Buffer == NIL
      ::cType    := ValType( ::xVarGet )
      ::picture  := ::cPicture
   ENDIF

   IF ::HasFocus
      cBuffer := ::Buffer
   ELSE
      cBuffer := ::PutMask( ::VarGet() )
   ENDIF

   IF ::nMaxLen == NIL
      ::nMaxLen := iif( cBuffer == NIL, 0, hb_ULen( cBuffer ) )
   ENDIF
   IF ::nDispLen == NIL
      ::nDispLen := ::nMaxLen
   ENDIF

   IF ::cType == "N" .AND. ::HasFocus .AND. ! ::lMinusPrinted .AND. ;
         ::DecPos != 0 .AND. ::lMinus .AND. ;
         ::Pos > ::DecPos .AND. Val( hb_ULeft( cBuffer, ::DecPos - 1 ) ) == 0

      // display "-." only in case when value on the left side of
      // the decimal point is equal 0
      cBuffer := hb_USubStr( cBuffer, 1, ::DecPos - 2 ) + "-." + hb_USubStr( cBuffer, ::DecPos + 1 )
   ENDIF

   IF ::nDispLen != ::nMaxLen .AND. ::Pos != 0 // ; has scroll?
      IF ::nDispLen > 8
         nDispPos := Max( 1, Min( ::Pos - ::nDispLen + 4, ::nMaxLen - ::nDispLen + 1 ) )
      ELSE
         nDispPos := Max( 1, Min( ::Pos - Int( ::nDispLen / 2 ), ::nMaxLen - ::nDispLen + 1 ) )
      ENDIF
   ELSE
      nDispPos := 1
   ENDIF

   IF cBuffer != NIL .AND. ( lForced .OR. ( nDispPos != ::nOldPos ) )
      DispOutAt( ::Row, ::Col + iif( ::cDelimit == NIL, 0, 1 ), ;
         hb_USubStr( cBuffer, nDispPos, ::nDispLen ), ;
         hb_ColorIndex( ::cColorSpec, iif( ::HasFocus, GET_CLR_ENHANCED, GET_CLR_UNSELECTED ) ) )
      IF ::cDelimit != NIL
         DispOutAt( ::Row, ::Col, Left( ::cDelimit, 1 ), hb_ColorIndex( ::cColorSpec, iif( ::HasFocus, GET_CLR_ENHANCED, GET_CLR_UNSELECTED ) ) )
         DispOutAt( ::Row, ::Col + ::nDispLen + 1, hb_USubStr( ::cDelimit, 2, 1 ), hb_ColorIndex( ::cColorSpec, iif( ::HasFocus, GET_CLR_ENHANCED, GET_CLR_UNSELECTED ) ) )
      ENDIF
   ENDIF

   IF ::Pos != 0
      SetPos( ::Row, ::Col + ::Pos - nDispPos + iif( ::cDelimit == NIL, 0, 1 ) )
   ENDIF

   ::nOldPos := nDispPos

   SetCursor( nOldCursor )

RETURN Self

/*-------------------------------------------------------------------------*/

METHOD ColorDisp( cColorSpec ) CLASS HMG_Get

   ::ColorSpec := cColorSpec
   ::Display()

RETURN Self

/*-------------------------------------------------------------------------*/

METHOD End() CLASS HMG_Get

   LOCAL nLastCharPos, nPos, nFor

   IF ::HasFocus
      nLastCharPos := Min( hb_ULen( RTrim( ::Buffer ) ) + 1, ::nMaxEdit )
      IF ::Pos != nLastCharPos
         nPos := nLastCharPos
      ELSE
         nPos := ::nMaxEdit
      ENDIF
      FOR nFor := nPos TO ::FirstEditable() STEP -1
         IF ::IsEditable( nFor )
            ::Pos := nFor
            EXIT
         ENDIF
      NEXT
      ::Clear := .F.
      ::Display( .F. )
   ENDIF

RETURN Self

/*-------------------------------------------------------------------------*/

METHOD Home() CLASS HMG_Get

   IF ::HasFocus
      ::Pos := ::FirstEditable()
      ::Clear := .F.
      ::Display( .F. )
   ENDIF

RETURN Self

/*-------------------------------------------------------------------------*/

METHOD Reset() CLASS HMG_Get

   IF ::HasFocus
      ::Buffer   := ::PutMask( ::VarGet(), .F. )
      ::Pos      := ::FirstEditable() // ; Simple 0 in CA-Cl*pper
      ::Clear    := ( "K" $ ::cPicFunc .OR. ::cType == "N" )
      ::lEdit    := .F.
      ::Minus    := .F.
      ::Rejected := .F.
      ::TypeOut  := ( ::Pos == 0 ) // ; Simple .f. in CA-Cl*pper
      ::Display()
   ENDIF

RETURN Self

/*-------------------------------------------------------------------------*/

METHOD Undo() CLASS HMG_Get

   IF ::HasFocus
      ::VarPut( ::Initial )
      ::Reset()
      ::Changed := .F.
   ENDIF

RETURN Self

/*-------------------------------------------------------------------------*/

METHOD SetFocus() CLASS HMG_Get

   LOCAL lWasNIL
   LOCAL xVarGet

   IF ::HasFocus
      RETURN Self
   ENDIF

   lWasNIL := ::Buffer == NIL
   xVarGet := ::VarGet()

   ::HasFocus   := .T.
   ::Rejected   := .F.

   ::Original   := xVarGet
   ::Initial    := xVarGet
   ::cType      := ValType( xVarGet )
   ::Buffer     := ::PutMask( xVarGet, .F. )
   ::Picture    := ::cPicture
   ::Changed    := .F.
   ::Clear      := ( "K" $ ::cPicFunc .OR. ::cType == "N" )
   ::lEdit      := .F.
   ::Pos        := ::FirstEditable()
   ::TypeOut    := ( ::Pos == 0 )

   IF ::cType == "N"
      ::DecPos := hb_UAt( iif( ::lDecRev .OR. "E" $ ::cPicFunc, ",", "." ), ::Buffer )
      IF ::DecPos == 0
         ::DecPos := hb_ULen( ::Buffer ) + 1
      ENDIF
      ::lMinus := ( xVarGet < 0 )
   ELSE
      ::DecPos := 0 // ; CA-Cl*pper NG says that it contains NIL, but in fact it contains zero.
   ENDIF

   ::lMinusPrinted := .F.
   ::Minus     := .F.
   ::BadDate   := ( ::cType == "D" ) .AND. IsBadDate( ::Buffer, ::cPicFunc )

   IF lWasNIL .AND. ::Buffer != NIL
      IF ::nDispLen == NIL
         ::nDispLen := ::nMaxLen
      ENDIF
   ENDIF

   ::Display()

RETURN Self

/*-------------------------------------------------------------------------*/

METHOD KillFocus() CLASS HMG_Get

   LOCAL lHadFocus

   IF ::lEdit
      ::Assign()
   ENDIF

   lHadFocus := ::HasFocus

   ::HasFocus := .F.
   ::Pos      := 0
   ::Clear    := .F.
   ::Minus    := .F.
   ::Changed  := .F.
   ::DecPos   := 0 // ; CA-Cl*pper NG says that it contains NIL, but in fact it contains zero.

   IF lHadFocus
      ::Display()
   ENDIF

   ::xVarGet  := NIL
   ::Original := NIL
   ::Buffer   := NIL

RETURN Self

/*-------------------------------------------------------------------------*/

METHOD VarPut( xValue, lReFormat ) CLASS HMG_Get

   LOCAL aSubs, nLen, aValue
   LOCAL i

   DEFAULT lReFormat TO .T.

   IF ISBLOCK( ::bBlock )
      IF ::SubScript == NIL
         Eval( ::bBlock, xValue )
      ELSE
         aSubs := ::SubScript
         nLen := Len( aSubs )
         aValue := Eval( ::bBlock )
         FOR i := 1 TO nLen - 1
            aValue := aValue[ aSubs[ i ] ]
         NEXT
         aValue[ aSubs[ i ] ] := xValue
      ENDIF

      IF lReFormat
         ::cType   := ValType( xValue )
         ::xVarGet := xValue
         ::lEdit   := .F.
         ::Changed := .T.
         ::Picture := ::cPicture
      ENDIF
   ENDIF

RETURN xValue

/*-------------------------------------------------------------------------*/

METHOD VarGet() CLASS HMG_Get

   LOCAL aSubs, nLen, aValue
   LOCAL i
   LOCAL xValue

   IF ISBLOCK( ::bBlock )
      IF ::SubScript == NIL
         xValue := Eval( ::bBlock )
      ELSE
         aSubs := ::SubScript
         nLen := Len( aSubs )
         aValue := Eval( ::bBlock )
         FOR i := 1 TO nLen - 1
            aValue := aValue[ aSubs[ i ] ]
         NEXT
         xValue := aValue[ aSubs[ i ] ]
      ENDIF
   ELSE
      xValue := NIL
   ENDIF

   ::xVarGet := xValue

RETURN xValue

/*-------------------------------------------------------------------------*/

METHOD UnTransform( cBuffer ) CLASS HMG_Get

   LOCAL xValue
   LOCAL cChar
   LOCAL nFor

   DEFAULT cBuffer TO ::Buffer

   DO CASE
   CASE ::cType == "C"

      IF "R" $ ::cPicFunc
         FOR nFor := 1 TO hb_ULen( ::cPicMask )
            cChar := hb_USubStr( ::cPicMask, nFor, 1 )
            IF !cChar $ "ANX9#!"
               cBuffer := hb_USubStr( cBuffer, 1, nFor - 1 ) + Chr( 1 ) + hb_USubStr( cBuffer, nFor + 1 )
            ENDIF
         NEXT
         cBuffer := hb_UPadR( StrTran( cBuffer, Chr( 1 ), "" ), hb_ULen( ::Original ) )
      ENDIF
#ifdef UNICODE
      IF "!" $ ::cPicFunc .OR. hb_USubStr( ::cPicMask, 1, 1 ) == "!"
         cBuffer := hmg_Upper( cBuffer )
      ENDIF
#endif
      xValue := cBuffer

   CASE ::cType == "N"

      IF "X" $ ::cPicFunc
         IF hb_URight( cBuffer, 2 ) == "DB"
            ::lMinus := .T.
         ENDIF
      ENDIF

      IF ! ::lMinus
         FOR nFor := 1 TO ::nMaxLen
            IF ::IsEditable( nFor ) .AND. IsDigit( hb_USubStr( cBuffer, nFor, 1 ) )
               EXIT
            ENDIF
            IF hb_USubStr( cBuffer, nFor, 1 ) $ "-(" .AND. hb_USubStr( cBuffer, nFor, 1 ) != hb_USubStr( ::cPicMask, nFor, 1 )
               ::lMinus := .T.
               EXIT
            ENDIF
         NEXT
      ENDIF

      cBuffer := Space( ::FirstEditable() - 1 ) + hb_USubStr( cBuffer, ::FirstEditable(), ::LastEditable() - ::FirstEditable() + 1 )

      IF "D" $ ::cPicFunc
         FOR nFor := ::FirstEditable() TO ::LastEditable()
            IF ! ::IsEditable( nFor )
               cBuffer := hb_ULeft( cBuffer, nFor - 1 ) + Chr( 1 ) + hb_USubStr( cBuffer, nFor + 1 )
            ENDIF
         NEXT
      ELSE
         IF "E" $ ::cPicFunc .OR. ::lDecRev
            cBuffer := hb_ULeft( cBuffer, ::FirstEditable() - 1 ) + StrTran( hb_USubStr( cBuffer, ::FirstEditable(), ::LastEditable() - ::FirstEditable() + 1 ), ".", " " ) + hb_USubStr( cBuffer, ::LastEditable() + 1 )
            cBuffer := hb_ULeft( cBuffer, ::FirstEditable() - 1 ) + StrTran( hb_USubStr( cBuffer, ::FirstEditable(), ::LastEditable() - ::FirstEditable() + 1 ), ",", "." ) + hb_USubStr( cBuffer, ::LastEditable() + 1 )
         ELSE
            cBuffer := hb_ULeft( cBuffer, ::FirstEditable() - 1 ) + StrTran( hb_USubStr( cBuffer, ::FirstEditable(), ::LastEditable() - ::FirstEditable() + 1 ), ",", " " ) + hb_USubStr( cBuffer, ::LastEditable() + 1 )
         ENDIF

         FOR nFor := ::FirstEditable() TO ::LastEditable()
            IF ! ::IsEditable( nFor ) .AND. hb_USubStr( cBuffer, nFor, 1 ) != "."
               cBuffer := hb_ULeft( cBuffer, nFor - 1 ) + Chr( 1 ) + hb_USubStr( cBuffer, nFor + 1 )
            ENDIF
         NEXT
      ENDIF

      cBuffer := StrTran( cBuffer, Chr( 1 ), "" )

      cBuffer := StrTran( cBuffer, "$", " " )
      cBuffer := StrTran( cBuffer, "*", " " )
      cBuffer := StrTran( cBuffer, "-", " " )
      cBuffer := StrTran( cBuffer, "(", " " )
      cBuffer := StrTran( cBuffer, ")", " " )

      cBuffer := hb_UPadL( StrTran( cBuffer, " ", "" ), hb_ULen( cBuffer ) )

      IF ::lMinus
         FOR nFor := 1 TO hb_ULen( cBuffer )
            IF IsDigit( hb_USubStr( cBuffer, nFor, 1 ) )
               EXIT
            ENDIF
         NEXT
         nFor--
         IF nFor > 0
            cBuffer := hb_ULeft( cBuffer, nFor - 1 ) + "-" + hb_USubStr( cBuffer, nFor + 1 )
         ELSE
            cBuffer := "-" + cBuffer
         ENDIF
      ENDIF

#if ( __HARBOUR__ - 0 ) < 0x030200
      xValue  := Val( cBuffer )
#else
      xValue  := hb_Val( cBuffer )
#endif

   CASE ::cType == "L"
      cBuffer := Upper( cBuffer )
      xValue := "T" $ cBuffer .OR. "Y" $ cBuffer .OR. hb_langMessage( HB_LANG_ITEM_BASE_TEXT + 1 ) $ cBuffer

   CASE ::cType == "D"
      IF "E" $ ::cPicFunc
         cBuffer := hb_USubStr( cBuffer, 4, 3 ) + hb_USubStr( cBuffer, 1, 3 ) + hb_USubStr( cBuffer, 7 )
      ENDIF
      IF cBuffer != NIL
         xValue := CToD( cBuffer )
      ENDIF

   ENDCASE

RETURN xValue

/*-------------------------------------------------------------------------*/

METHOD OverStrike( cChar ) CLASS HMG_Get

   IF ! ::HasFocus
      RETURN Self
   ENDIF

   IF ::cType == "N" .AND. ! ::lEdit .AND. ::Clear
      ::Pos := ::FirstEditable()
   ENDIF

   IF ::Pos > ::nMaxEdit
      ::Rejected := .T.
      RETURN Self
   ENDIF

   cChar := ::Input( hb_ULeft( cChar, 1 ) )

   IF cChar == ""
      ::Rejected := .T.
      RETURN Self
   ELSE
      ::Rejected := .F.
   ENDIF

   IF ::Clear .AND. ::Pos == ::FirstEditable()
      ::DeleteAll()
      ::Clear := .F.
   ENDIF

   ::lEdit := .T.

   IF ::Pos == 0
      ::Pos := 1
   ENDIF

   DO WHILE ! ::IsEditable( ::Pos ) .AND. ::Pos <= ::nMaxEdit
      ::Pos++
   ENDDO

   IF ::Pos > ::nMaxEdit
      ::Pos := ::FirstEditable()
   ENDIF
   ::Buffer := hb_USubStr( ::Buffer, 1, ::Pos - 1 ) + cChar + hb_USubStr( ::Buffer, ::Pos + 1 )

   ::Changed := .T.

   ::Right( .F. )

   ::BadDate := ( ::cType == "D" ) .AND. IsBadDate( ::Buffer, ::cPicFunc )

   ::Display()

RETURN Self

/*-------------------------------------------------------------------------*/

METHOD Insert( cChar ) CLASS HMG_Get

   LOCAL n
   LOCAL nMaxEdit

   IF ! ::HasFocus
      RETURN Self
   ENDIF

   nMaxEdit := ::nMaxEdit

   IF ::cType == "N" .AND. ! ::lEdit .AND. ::Clear
      ::Pos := ::FirstEditable()
   ENDIF

   IF ::Pos > ::nMaxEdit
      ::Rejected := .T.
      RETURN Self
   ENDIF

   cChar := ::Input( hb_ULeft( cChar, 1 ) )

   IF cChar == ""
      ::Rejected := .T.
      RETURN Self
   ELSE
      ::Rejected := .F.
   ENDIF

   if ::Clear .AND. ::Pos == ::FirstEditable()
      ::DeleteAll()
      ::Clear := .F.
   ENDIF

   ::lEdit := .T.

   IF ::Pos == 0
      ::Pos := 1
   ENDIF

   DO WHILE ! ::IsEditable( ::Pos ) .AND. ::Pos <= ::nMaxEdit
      ::Pos++
   ENDDO

   IF ::Pos > ::nMaxEdit
      ::Pos := ::FirstEditable()
   ENDIF

   IF ::lPicComplex
      // Calculating diferent nMaxEdit for ::lPicComplex

      FOR n := ::Pos TO nMaxEdit
         IF ! ::IsEditable( n )
            EXIT
         ENDIF
      NEXT
      nMaxEdit := n
      ::Buffer := hb_ULeft( hb_USubStr( ::Buffer, 1, ::Pos - 1 ) + cChar + ;
         hb_USubStr( ::Buffer, ::Pos, nMaxEdit - 1 - ::Pos ) + ;
         hb_USubStr( ::Buffer, nMaxEdit ), ::nMaxLen )
   ELSE
      ::Buffer := hb_ULeft( hb_USubStr( ::Buffer, 1, ::Pos - 1 ) + cChar + hb_USubStr( ::Buffer, ::Pos ), ::nMaxEdit )
   ENDIF

   ::Changed := .T.

   ::Right( .F. )

   ::BadDate := ( ::cType == "D" ) .AND. IsBadDate( ::Buffer, ::cPicFunc )

   ::Display()

RETURN Self

/*-------------------------------------------------------------------------*/

METHOD _Right( lDisplay ) CLASS HMG_Get

   LOCAL nPos

   IF ! ::HasFocus
      RETURN Self
   ENDIF

   DEFAULT lDisplay TO .T.

   ::TypeOut := .F.
   ::Clear   := .F.

   IF ::Pos == ::nMaxEdit
      ::TypeOut := .T.
      RETURN Self
   ENDIF

   nPos := ::Pos + 1

   DO WHILE ! ::IsEditable( nPos ) .AND. nPos <= ::nMaxEdit
      nPos++
   ENDDO

   IF nPos <= ::nMaxEdit
      ::Pos := nPos
   ELSE
      ::TypeOut := .T.
   ENDIF

   IF lDisplay
      ::Display( .F. )
   ENDIF

RETURN Self

/*-------------------------------------------------------------------------*/

METHOD _Left( lDisplay ) CLASS HMG_Get

   LOCAL nPos

   IF ! ::HasFocus
      RETURN Self
   ENDIF

   DEFAULT lDisplay TO .T.

   ::TypeOut := .F.
   ::Clear   := .F.

   IF ::Pos == ::FirstEditable()
      ::TypeOut := .T.
      RETURN Self
   ENDIF

   nPos := ::Pos - 1

   DO WHILE ! ::IsEditable( nPos ) .AND. nPos > 0
      nPos--
   ENDDO

   IF nPos > 0
      ::Pos := nPos
   ELSE
      ::TypeOut := .T.
   ENDIF

   IF lDisplay
      ::Display( .F. )
   ENDIF

RETURN Self

/*-------------------------------------------------------------------------*/

METHOD WordLeft() CLASS HMG_Get

   LOCAL nPos

   IF ! ::HasFocus
      RETURN Self
   ENDIF

   ::TypeOut := .F.
   ::Clear   := .F.

   IF ::Pos == ::FirstEditable()
      ::TypeOut := .T.
      RETURN Self
   ENDIF

   nPos := ::Pos - 1

   DO WHILE nPos > 0
      IF hb_USubStr( ::Buffer, nPos, 1 ) == " "
         DO WHILE nPos > 0 .AND. hb_USubStr( ::Buffer, nPos, 1 ) == " "
            nPos--
         ENDDO
         DO WHILE nPos > 0 .AND. !( hb_USubStr( ::Buffer, nPos, 1 ) == " " )
            nPos--
         ENDDO
         IF nPos > 0
            nPos++
         ENDIF
         EXIT
      ENDIF
      nPos--
   ENDDO

   IF nPos < 1
      nPos := 1
   ENDIF

   IF nPos > 0
      ::Pos := nPos
   ENDIF

   ::Display( .F. )

RETURN Self

/*-------------------------------------------------------------------------*/

METHOD WordRight() CLASS HMG_Get

   LOCAL nPos

   IF ! ::HasFocus
      RETURN Self
   ENDIF

   ::TypeOut := .F.
   ::Clear   := .F.

   IF ::Pos == ::nMaxEdit
      ::TypeOut := .T.
      RETURN Self
   ENDIF

   nPos := ::Pos + 1

   DO WHILE nPos <= ::nMaxEdit
      IF hb_USubStr( ::Buffer, nPos, 1 ) == " "
         DO WHILE nPos <= ::nMaxEdit .AND. hb_USubStr( ::Buffer, nPos, 1 ) == " "
            nPos++
         ENDDO
         EXIT
      ENDIF
      nPos++
   ENDDO

   IF nPos > ::nMaxEdit
      nPos := ::nMaxEdit
   ENDIF

   IF nPos <= ::nMaxEdit
      ::Pos := nPos
   ENDIF

   ::Display( .F. )

RETURN Self

/*-------------------------------------------------------------------------*/

METHOD ToDecPos() CLASS HMG_Get

   IF ! ::HasFocus
      RETURN Self
   ENDIF

   IF ::Pos == ::FirstEditable()
      ::DeleteAll()
   ENDIF

   ::Clear   := .F.
   ::lEdit   := .T.
   ::Buffer  := ::PutMask( ::UnTransform(), .F. )
   ::Changed := .T.

   IF ::DecPos != 0
      IF ::DecPos == hb_ULen( ::cPicMask )
         ::Pos := ::DecPos - 1   // 9999.
      ELSE
         ::Pos := ::DecPos + 1   // 9999.9
      ENDIF
   ELSE
      ::Pos := ::nDispLen
   ENDIF

   ::Display()

RETURN Self

/*-------------------------------------------------------------------------*/

METHOD IsEditable( nPos ) CLASS HMG_Get

   LOCAL cChar

   IF Empty( ::cPicMask )
      RETURN .T.
   ENDIF

   IF ::nMaxEdit == NIL .OR. nPos > ::nMaxEdit
      RETURN .F.
   ENDIF

   cChar := hb_USubStr( ::cPicMask, nPos, 1 )

   DO CASE
   CASE ::cType == "C"
      RETURN cChar $ "!ANX9#"
   CASE ::cType == "N"
      RETURN cChar $ "9#$*"
   CASE ::cType == "D"
      RETURN cChar == "9"
   CASE ::cType == "L"
      RETURN cChar $ "LY#"   /* Clipper 5.2 undocumented: # allow T,F,Y,N for Logical [ckedem] */
   ENDCASE

RETURN .F.

/*-------------------------------------------------------------------------*/

METHOD Input( cChar ) CLASS HMG_Get

   LOCAL cPic

   DO CASE
   CASE ::cType == "N"

      DO CASE
      CASE cChar == "-"
         ::lMinus := .T.  /* The minus symbol can be written in any place */
         ::Minus := .T.

      CASE cChar $ ".,"
         ::toDecPos()
         RETURN ""

      CASE !( cChar $ "0123456789+" )
         RETURN ""
      ENDCASE

   CASE ::cType == "D"

      IF !( cChar $ "0123456789" )
         RETURN ""
      ENDIF

   CASE ::cType == "L"

      IF !( Upper( cChar ) $ "YNTF" )
         RETURN ""
      ENDIF

   ENDCASE

   IF ! Empty( ::cPicFunc )
      cChar := hb_ULeft( Transform( cChar, ::cPicFunc ), 1 ) // Left needed for @D
#ifdef UNICODE
   IF "!" $ ::cPicFunc
      cChar := hmg_Upper( cChar )
   ENDIF
#endif
   ENDIF

   IF ! Empty( ::cPicMask )
      cPic  := hb_asciiUpper( hb_USubStr( ::cPicMask, ::Pos, 1 ) )

      // cChar := Transform( cChar, cPic )
      // Above line eliminated because some get picture template symbols for
      // numeric input not work in text input. eg: $ and *

      DO CASE
      CASE cPic == "A"
         IF ! IsAlpha( cChar )
            cChar := ""
         ENDIF

      CASE cPic == "N"
         IF ! IsAlpha( cChar ) .AND. ! IsDigit( cChar )
            cChar := ""
         ENDIF

      CASE cPic == "9"
         IF ! IsDigit( cChar ) .AND. ! cChar $ "-+"
            cChar := ""
         ENDIF
         IF ::cType != "N" .AND. cChar $ "-+"
            cChar := ""
         ENDIF

#ifdef UNICODE
      CASE cPic == "!"
         cChar := hmg_Upper( cChar )
#endif
         /* Clipper 5.2 undocumented: # allow T,F,Y,N for Logical [ckedem] */
      CASE cPic == "L" .OR. ( cPic == "#" .AND. ::cType == "L" )
         IF !( Upper( cChar ) $ "YNTF" + ;
               hb_langMessage( HB_LANG_ITEM_BASE_TEXT + 1 ) + ;
               hb_langMessage( HB_LANG_ITEM_BASE_TEXT + 2 ) )
            cChar := ""
         ENDIF

      CASE cPic == "#"
         IF ! IsDigit( cChar ) .AND. !( cChar == " " ) .AND. !( cChar $ ".+-" )
            cChar := ""
         ENDIF

      CASE cPic == "Y"
         IF !( Upper( cChar ) $ "YN" )
            cChar := ""
         ENDIF

      CASE ( cPic == "$" .OR. cPic == "*" ) .AND. ::cType == "N"
         IF ! IsDigit( cChar ) .AND. cChar != "-"
            cChar := ""
         ENDIF
      OTHERWISE
         cChar := Transform( cChar, cPic )
      END CASE
   ENDIF

RETURN cChar

/*-------------------------------------------------------------------------*/

METHOD PutMask( xValue, lEdit ) CLASS HMG_Get

   LOCAL cChar
   LOCAL cBuffer
   LOCAL cPicFunc
   LOCAL cMask
   LOCAL nFor

   IF ::cType == NIL
      // Not initialized yet
      ::Original := ::VarGet()
      ::cType    := ValType( ::Original )
      ::Picture  := ::cPicture
   ENDIF

   cPicFunc := ::cPicFunc
   cMask    := ::cPicMask

   DEFAULT xValue TO ::VarGet()
   DEFAULT lEdit  TO ::HasFocus

   IF xValue == NIL .OR. ValType( xValue ) $ "AB"
      ::nMaxLen := 0
      RETURN NIL
   ENDIF

   IF ::HasFocus
      cPicFunc := StrTran( cPicfunc, "B", "" )
      IF cPicFunc == "@"
         cPicFunc := ""
      ENDIF
   ENDIF

   IF lEdit .AND. ::lEdit
      IF ( "*" $ cMask ) .OR. ( "$" $ cMask )
         cMask := StrTran( StrTran( cMask, "*", "9" ), "$", "9" )
      ENDIF
   ENDIF

   cBuffer := Transform( xValue, ;
      iif( Empty( cPicFunc ), ;
      iif( ::lCleanZero .AND. ! ::HasFocus, "@Z ", "" ), ;
      cPicFunc + iif( ::lCleanZero .AND. ! ::HasFocus, "Z", "" ) + " " ) ;
      + cMask )

#ifdef UNICODE
   IF ::cType == "C" .AND. ( "!" $ cPicFunc .OR. ! Empty( cMask ) .AND. SubStr( cMask, 1, 1 ) == "!" )
      cBuffer := hmg_Upper( xValue )
   ENDIF
#endif
   IF ::cType == "N"
      IF ( "(" $ cPicFunc .OR. ")" $ cPicFunc ) .AND. xValue >= 0
         cBuffer += " "
      ENDIF

      IF ( ( "C" $ cPicFunc .AND. xValue <  0 ) .OR. ;
            ( "X" $ cPicFunc .AND. xValue >= 0 ) ) .AND. ;
            !( "X" $ cPicFunc .AND. "C" $ cPicFunc )
         cBuffer += "   "
      ENDIF

      IF xValue < 0
         ::lMinusPrinted := .T.
      ELSE
         ::lMinusPrinted := .F.
      ENDIF
   ENDIF

   ::nMaxLen  := hb_ULen( cBuffer )
   ::nMaxEdit := ::nMaxLen

   IF ::nDispLen == NIL
      ::nDispLen := ::nMaxLen
   ENDIF

   IF lEdit .AND. ::cType == "N" .AND. ! Empty( cMask )
      IF "E" $ cPicFunc
         cMask := hb_ULeft( cMask, ::FirstEditable() - 1 ) + StrTran( hb_USubStr( cMask, ::FirstEditable(), ::LastEditable() - ::FirstEditable() + 1 ), ",", Chr( 1 ) ) + hb_USubStr( cMask, ::LastEditable() + 1 )
         cMask := hb_ULeft( cMask, ::FirstEditable() - 1 ) + StrTran( hb_USubStr( cMask, ::FirstEditable(), ::LastEditable() - ::FirstEditable() + 1 ), ".", ","      ) + hb_USubStr( cMask, ::LastEditable() + 1 )
         cMask := hb_ULeft( cMask, ::FirstEditable() - 1 ) + StrTran( hb_USubStr( cMask, ::FirstEditable(), ::LastEditable() - ::FirstEditable() + 1 ), Chr( 1 ), "." ) + hb_USubStr( cMask, ::LastEditable() + 1 )
      ENDIF
      FOR nFor := 1 TO ::nMaxLen
         cChar := hb_USubStr( cMask, nFor, 1 )
         IF cChar $ ",." .AND. hb_USubStr( cBuffer, nFor, 1 ) $ ",."
            cBuffer := hb_USubStr( cBuffer, 1, nFor - 1 ) + cChar + hb_USubStr( cBuffer, nFor + 1 )
         ENDIF
      NEXT
      IF ::lEdit .AND. Empty( xValue )
         cBuffer := StrTran( cBuffer, "0", " " )
      ENDIF
      IF ::lDecRev
         cBuffer := hb_ULeft( cBuffer, ::FirstEditable() - 1 ) + StrTran( hb_USubStr( cBuffer, ::FirstEditable(), ::LastEditable() - ::FirstEditable() + 1 ), ",", Chr( 1 ) ) + hb_USubStr( cBuffer, ::LastEditable() + 1 )
         cBuffer := hb_ULeft( cBuffer, ::FirstEditable() - 1 ) + StrTran( hb_USubStr( cBuffer, ::FirstEditable(), ::LastEditable() - ::FirstEditable() + 1 ), ".", ","      ) + hb_USubStr( cBuffer, ::LastEditable() + 1 )
         cBuffer := hb_ULeft( cBuffer, ::FirstEditable() - 1 ) + StrTran( hb_USubStr( cBuffer, ::FirstEditable(), ::LastEditable() - ::FirstEditable() + 1 ), Chr( 1 ), "." ) + hb_USubStr( cBuffer, ::LastEditable() + 1 )
      ENDIF
   ENDIF

   IF ::cType == "N"
      IF "(" $ ::cPicFunc .OR. ")" $ ::cPicFunc
         ::nMaxEdit--
      ENDIF
      IF "C" $ ::cPicFunc .OR. "X" $ ::cPicFunc
         ::nMaxEdit -= 3
      ENDIF
   ENDIF

   IF ::cType == "D" .AND. ::BadDate
      cBuffer := ::Buffer
   ENDIF

   ::nMaxLen := hb_ULen( cBuffer )

RETURN cBuffer

/*-------------------------------------------------------------------------*/

METHOD BackSpace( lDisplay ) CLASS HMG_Get

   LOCAL nPos := ::Pos
   LOCAL nMinus

   DEFAULT lDisplay TO .T.

   IF nPos > 1 .AND. nPos == ::FirstEditable() .AND. ::lMinus

      /* To delete the parenthesis (negative indicator) in a non editable position */

      nMinus := hb_UAt( "(", hb_USubStr( ::Buffer, 1, nPos - 1 ) )

      IF nMinus > 0 .AND. hb_USubStr( ::cPicMask, nMinus, 1 ) != "("

         ::lEdit := .T.

         ::Buffer := hb_USubStr( ::Buffer, 1, nMinus - 1 ) + " " + ;
            hb_USubStr( ::Buffer, nMinus + 1 )

         ::Changed := .T.

         IF lDisplay
            ::Display()
         ENDIF

         RETURN Self

      ENDIF

   ENDIF

   ::Left()

   IF ::Pos < nPos
      ::Delete( lDisplay )
   ENDIF

RETURN Self

/*-------------------------------------------------------------------------*/

METHOD _Delete( lDisplay ) CLASS HMG_Get

   LOCAL nMaxLen := ::nMaxLen, n

   DEFAULT lDisplay TO .T.

   ::Clear := .F.
   ::lEdit := .T.

   IF ::lPicComplex
      // Calculating diferent nMaxLen for ::lPicComplex
      FOR n := ::Pos TO nMaxLen
         IF ! ::IsEditable( n )
            EXIT
         ENDIF
      NEXT
      nMaxLen := n - 1
   ENDIF

   IF ::cType == "N" .AND. hb_USubStr( ::Buffer, ::Pos, 1 ) $ "(-"
      ::lMinus := .F.
   ENDIF

   ::Buffer := hb_UPadR( hb_USubStr( ::Buffer, 1, ::Pos - 1 ) + ;
      hb_USubStr( ::Buffer, ::Pos + 1, nMaxLen - ::Pos ) + " " + ;
      hb_USubStr( ::Buffer, nMaxLen + 1 ), ::nMaxLen )

   IF ::cType == "D"
      ::BadDate := IsBadDate( ::Buffer, ::cPicFunc )
   ELSE
      ::BadDate := .F.
   ENDIF

   ::Changed := .T.

   IF lDisplay
      ::Display()
   ENDIF

RETURN Self

/*-------------------------------------------------------------------------*/

METHOD DeleteAll() CLASS HMG_Get

   LOCAL xValue

   ::lEdit := .T.

   DO CASE
   CASE ::cType == "C"
      xValue := Space( ::nMaxlen )
   CASE ::cType == "N"
      xValue   := 0
      ::lMinus := .F.
   CASE ::cType == "D"
      xValue := CToD( "" )
      ::BadDate := .F.
   CASE ::cType == "L"
      xValue := .F.
   ENDCASE

   ::Buffer := ::PutMask( xValue, .T. )
   ::Pos    := ::FirstEditable()

RETURN Self

/*-------------------------------------------------------------------------*/

METHOD DelEnd() CLASS HMG_Get

   LOCAL nPos := ::Pos

   IF ! ::HasFocus
      RETURN Self
   ENDIF

   ::Pos := ::nMaxEdit

   ::Delete( .F. )
   DO WHILE ::Pos > nPos
      ::BackSpace( .F. )
   ENDDO

   ::Display()

RETURN Self

/*-------------------------------------------------------------------------*/

METHOD DelLeft() CLASS HMG_Get

   ::Left( .F. )
   ::Delete( .F. )
   ::Right()

RETURN Self

/*-------------------------------------------------------------------------*/

METHOD DelRight() CLASS HMG_Get

   ::Right( .F. )
   ::Delete( .F. )
   ::Left()

RETURN Self

/*-------------------------------------------------------------------------*/

/* NOTE ::WordLeft()
        ::DelWordRight() */

METHOD DelWordLeft() CLASS HMG_Get

   IF ! ::HasFocus
      RETURN Self
   ENDIF

   IF !( hb_USubStr( ::Buffer, ::Pos, 1 ) == " " )
      IF hb_USubStr( ::Buffer, ::Pos - 1, 1 ) == " "
         ::BackSpace( .F. )
      ELSE
         ::WordRight()
         ::Left()
      ENDIF
   ENDIF

   IF hb_USubStr( ::Buffer, ::Pos, 1 ) == " "
      ::Delete( .F. )
   ENDIF

   DO WHILE ::Pos > 1 .AND. !( hb_USubStr( ::Buffer, ::Pos - 1, 1 ) == " " )
      ::BackSpace( .F. )
   ENDDO

   ::Display()

RETURN Self

/*-------------------------------------------------------------------------*/

METHOD DelWordRight() CLASS HMG_Get

   IF ! ::HasFocus
      RETURN Self
   ENDIF

   ::TypeOut := .F.
   ::Clear   := .F.

   IF ::Pos == ::nMaxEdit
      ::TypeOut := .T.
      RETURN Self
   ENDIF

   DO WHILE ::Pos <= ::nMaxEdit .AND. !( hb_USubStr( ::Buffer, ::Pos, 1 ) == " " )
      ::Delete( .F. )
   ENDDO

   IF ::Pos <= ::nMaxEdit
      ::Delete( .F. )
   ENDIF

   ::Display()

RETURN Self

/*-------------------------------------------------------------------------*/

/* The METHOD ColorSpec and DATA cColorSpec allow to replace the
 * property ColorSpec for a function to control the content and
 * to carry out certain actions to normalize the data.
 * The particular case is that the function receives a single color and
 * be used for GET_CLR_UNSELECTED and GET_CLR_ENHANCED.
 */

METHOD ColorSpec( cColorSpec ) CLASS HMG_Get

   LOCAL cClrUnSel
   LOCAL cClrEnh

   IF cColorSpec != NIL

      cClrUnSel := iif( !Empty( hb_ColorIndex( cColorSpec, GET_CLR_UNSELECTED ) ), ;
         hb_ColorIndex( cColorSpec, GET_CLR_UNSELECTED ), ;
         hb_ColorIndex( SetColor(), CLR_UNSELECTED ) )

      cClrEnh   := iif( !Empty( hb_ColorIndex( cColorSpec, GET_CLR_ENHANCED ) ), ;
         hb_ColorIndex( cColorSpec, GET_CLR_ENHANCED ), ;
         cClrUnSel )

      ::cColorSpec := cClrUnSel + "," + cClrEnh

   ENDIF

RETURN ::cColorSpec

/*-------------------------------------------------------------------------*/

/* The METHOD Picture and DATA cPicture allow to replace the
 * property Picture for a function to control the content and
 * to carry out certain actions to normalize the data.
 * The particular case is that the Picture is loaded later on
 * to the creation of the object, being necessary to carry out
 * several tasks to adjust the internal data of the object.
 */

METHOD Picture( cPicture ) CLASS HMG_Get

   LOCAL cChar
   LOCAL nAt
   LOCAL nFor
   LOCAL cNum

   IF cPicture != NIL

      ::nDispLen := NIL

      ::cPicture := cPicture

      cNum := ""

      IF hb_ULeft( cPicture, 1 ) == "@"

         nAt := hb_UAt( " ", cPicture )

         IF nAt == 0
            ::cPicFunc := hb_asciiUpper( cPicture )
            ::cPicMask := ""
         ELSE
            ::cPicFunc := hb_asciiUpper( hb_USubStr( cPicture, 1, nAt - 1 ) )
            ::cPicMask := hb_USubStr( cPicture, nAt + 1 )
         ENDIF

         IF "D" $ ::cPicFunc

            ::cPicMask := Set( _SET_DATEFORMAT )
            ::cPicMask := StrTran( ::cPicmask, "y", "9" )
            ::cPicMask := StrTran( ::cPicmask, "Y", "9" )
            ::cPicMask := StrTran( ::cPicmask, "m", "9" )
            ::cPicMask := StrTran( ::cPicmask, "M", "9" )
            ::cPicMask := StrTran( ::cPicmask, "d", "9" )
            ::cPicMask := StrTran( ::cPicmask, "D", "9" )

         ENDIF

         IF ( nAt := hb_UAt( "S", ::cPicFunc ) ) > 0
            FOR nFor := nAt + 1 TO hb_ULen( ::cPicFunc )
               IF ! IsDigit( hb_USubStr( ::cPicFunc, nFor, 1 ) )
                  EXIT
               ELSE
                  cNum += hb_USubStr( ::cPicFunc, nFor, 1 )
               ENDIF
            NEXT
            IF Val( cNum ) > 0
               ::nDispLen := Val( cNum )
            ENDIF
            ::cPicFunc := hb_USubStr( ::cPicFunc, 1, nAt - 1 ) + hb_USubStr( ::cPicFunc, nFor )
         ENDIF

         IF "Z" $ ::cPicFunc
            ::lCleanZero := .T.
         ELSE
            ::lCleanZero := .F.
         ENDIF
         ::cPicFunc := StrTran( ::cPicFunc, "Z", "" )

         IF ::cPicFunc == "@"
            ::cPicFunc := ""
         ENDIF
      ELSE
         ::cPicFunc   := ""
         ::cPicMask   := cPicture
         ::lCleanZero := .F.
      ENDIF

      IF ::cType == NIL
         ::Original := ::xVarGet
         ::cType    := ValType( ::Original )
      ENDIF

      IF ::cType == "D"
         ::cPicMask := LTrim( ::cPicMask )
      ENDIF

      // Comprobar si tiene la , y el . cambiado (Solo en Xbase++)

      ::lDecRev := "," $ Transform( 1.1, "9.9" )

      // Generate default picture mask if not specified

      IF Empty( ::cPicMask )

         DO CASE
         CASE ::cType == "D"

            ::cPicMask := Set( _SET_DATEFORMAT )
            ::cPicMask := StrTran( ::cPicmask, "y", "9" )
            ::cPicMask := StrTran( ::cPicmask, "Y", "9" )
            ::cPicMask := StrTran( ::cPicmask, "m", "9" )
            ::cPicMask := StrTran( ::cPicmask, "M", "9" )
            ::cPicMask := StrTran( ::cPicmask, "d", "9" )
            ::cPicMask := StrTran( ::cPicmask, "D", "9" )

         CASE ::cType == "N"

            cNum := Str( ::xVarGet )
            IF ( nAt := hb_UAt( iif( ::lDecRev, ",", "." ), cNum ) ) > 0
               ::cPicMask := Replicate( "9", nAt - 1 ) + iif( ::lDecRev, ",", "." )
               ::cPicMask += Replicate( "9", hb_ULen( cNum ) - hb_ULen( ::cPicMask ) )
            ELSE
               ::cPicMask := Replicate( "9", hb_ULen( cNum ) )
            ENDIF

         CASE ::cType == "C" .AND. ::cPicFunc == "@9"
            ::cPicMask := Replicate( "9", hb_ULen( ::xVarGet ) )
            ::cPicFunc := ""

         ENDCASE

      ENDIF

      // Comprobar si tiene caracteres embebidos no modificables en la plantilla

      ::lPicComplex := .F.

      IF ! Empty( ::cPicMask )
         FOR nFor := 1 TO hb_ULen( ::cPicMask )
            cChar := hb_USubStr( ::cPicMask, nFor, 1 )
            IF !( cChar $ "!ANX9#" )
               ::lPicComplex := .T.
               EXIT
            ENDIF
         NEXT
      ENDIF

      IF ::HasFocus
         IF ::cType == "N"
            ::DecPos := hb_UAt( iif( ::lDecRev .OR. "E" $ ::cPicFunc, ",", "." ), ;
               Transform( 1, iif( Empty( ::cPicFunc ), "", ::cPicFunc + " " ) + ::cPicMask ) )
            IF ::DecPos == 0
               ::DecPos := hb_ULen( ::Buffer ) + 1
            ENDIF
         ELSE
            ::DecPos := 0 // ; CA-Cl*pper NG says that it contains NIL, but in fact it contains zero.
         ENDIF
      ENDIF

      IF ::nDispLen == NIL
         ::nDispLen := ::nMaxLen
      ENDIF

   ENDIF

RETURN ::cPicture

/*-------------------------------------------------------------------------*/

METHOD SetKeyEvent( nKey, bKey, lCtrl, lShift, lAlt ) CLASS HMG_Get

   LOCAL n, cKey

   cKey := hb_ntos( iif( HB_ISNUMERIC( nKey ), nKey, WM_LBUTTONDBLCLK ) )
   cKey += iif( Empty( lCtrl ), '', '#' )
   cKey += iif( Empty( lShift ), '', '^' )
   cKey += iif( Empty( lAlt  ), '', '@' )

   IF ( n := AScan( ::aKeyEvent, {| a | a[ 1 ] == cKey } ) ) > 0
      ::aKeyEvent[ n ] := { cKey, bKey }
   ELSE
      AAdd( ::aKeyEvent, { cKey, bKey } )
   ENDIF

RETURN NIL

/*-------------------------------------------------------------------------*/

METHOD DoKeyEvent( nKey ) CLASS HMG_Get

   LOCAL n, r := 0, cKey := hb_ntos( nKey )

   IF Len( ::aKeyEvent ) > 0

      cKey += iif( _GetKeyState( VK_CONTROL ), '#', '' )
      cKey += iif( _GetKeyState( VK_SHIFT   ), '^', '' )
      cKey += iif( _GetKeyState( VK_MENU    ), '@', '' )
      IF ( n := AScan( ::aKeyEvent, {| a | a[ 1 ] == cKey } ) ) > 0
         IF HB_ISBLOCK( ::aKeyEvent[ n ][ 2 ] )
#ifdef _NAMES_LIST_
            Do_ControlEventProcedure( ::aKeyEvent[ n ][ 2 ], _GetNameList( ::name ), Self, nKey, cKey )
#else
            Do_ControlEventProcedure( ::aKeyEvent[ n ][ 2 ], __mvGet( ::name ), Self, nKey, cKey )
#endif
            r := 1
         ENDIF
      ENDIF

   ENDIF

RETURN r

/*-------------------------------------------------------------------------*/

METHOD Refresh() CLASS HMG_Get

   ::UpdateBuffer()
   _DispGetBoxText( ::Control, ::Buffer )

RETURN NIL

/*-------------------------------------------------------------------------*/

METHOD Type() CLASS HMG_Get

   ::cType := ValType( iif( ::HasFocus, ::xVarGet, ::VarGet() ) )

RETURN ::cType

/*-------------------------------------------------------------------------*/

/* The METHOD Block and DATA bBlock allow to replace the
 * property Block for a function to control the content and
 * to carry out certain actions to normalize the data.
 * The particular case is that the Block is loaded later on
 * to the creation of the object, being necessary to carry out
 * several tasks to adjust the internal data of the object
 * to display correctly.
 */

METHOD Block( bBlock ) CLASS HMG_Get

   IF bBlock != NIL .AND. ! ::HasFocus

      ::bBlock   := bBlock
      ::cType    := ValType( ::Original )
      ::xVarGet  := NIL

   ENDIF

RETURN ::bBlock

/*-------------------------------------------------------------------------*/

#ifdef HB_COMPAT_C53

METHOD HitTest( nMRow, nMCol ) CLASS HMG_Get

   IF ::Row != nMRow
      RETURN HTNOWHERE
   ENDIF

   IF nMCol >= ::Col .AND. ;
         nMCol <= ::Col + ::nDispLen + iif( ::cDelimit == NIL, 0, 2 )
      RETURN HTCLIENT
   ENDIF

RETURN HTNOWHERE

#endif

/*-------------------------------------------------------------------------*/

METHOD FirstEditable() CLASS HMG_Get

   LOCAL nFor

   IF ::nMaxLen != NIL

      IF ::IsEditable( 1 )
         RETURN 1
      ENDIF

      FOR nFor := 2 TO ::nMaxLen
         IF ::IsEditable( nFor )
            RETURN nFor
         ENDIF
      NEXT

   ENDIF

   ::TypeOut := .T.

RETURN 0

/*-------------------------------------------------------------------------*/

METHOD LastEditable() CLASS HMG_Get

   LOCAL nFor

   IF ::nMaxLen != NIL

      FOR nFor := ::nMaxLen TO 1 STEP -1
         IF ::IsEditable( nFor )
            RETURN nFor
         ENDIF
      NEXT

   ENDIF

   ::TypeOut := .T.

RETURN 0

/*-------------------------------------------------------------------------*/

STATIC FUNCTION IsBadDate( cBuffer, cPicFunc )

   LOCAL nFor, nLen

   IF "E" $ cPicFunc
      cBuffer := hb_USubStr( cBuffer, 4, 3 ) + hb_USubStr( cBuffer, 1, 3 ) + hb_USubStr( cBuffer, 7 )
   ENDIF

   IF cBuffer == NIL .OR. ! Empty( CToD( cBuffer ) )
      RETURN .F.
   ENDIF

   nLen := hb_ULen( cBuffer )

   FOR nFor := 1 TO nLen
      IF IsDigit( hb_USubStr( cBuffer, nFor, 1 ) )
         RETURN .T.
      ENDIF
   NEXT

RETURN .F.
