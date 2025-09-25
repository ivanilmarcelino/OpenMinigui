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
   Copyright 1999-2021, https://harbour.github.io/

   "WHAT32"
   Copyright 2002 AJ Wos <andrwos@aust1.net>

   "HWGUI"
   Copyright 2001-2021 Alexander S.Kresin <alex@kresin.ru>

----------------------------------------------------------------------------*/

#ifdef __XHARBOUR__
#define __MINIPRINT__
#else
  SET PROCEDURE TO alerts.prg
#endif

#include "hmg.ch"
#include "i_winuser.ch"
/*
   Author    : Francisco Garcia Fernandez
   Objective : Simulate Clipper ALERT function
             : HMG_Alert( cText, [<aOptions>], [<cTitle>], [<nType>], [<cIcoName>], [<nIcoSize>], [<aBtnColors>], [<bInit>], [<lClosable>], [<cFontName>] )

             cText      -> As in Clipper, let's separate the lines with semicolon

             aOptions   -> same as in Clipper
                        -> if you pass a numeric value
                           wait so many seconds and cancel only
                           if a button is pressed, it stops waiting and ends

             cTitle     -> the title of the window, by default it leaves "Attention"

             nType      -> 1, 2, 3, 4
                           if aOptions has only one and is not passed nType
                           this one is equal 1
                           if aOptions has two or more and is not passed nType
                           this is equal 2

             cIcoName   -> optional an icon's name defined by user

             nIcoSize   -> 32 [default], 48, 64 optional an icon's size

             aBtnColors -> optional array of colors for the buttons

             bInit      -> optional initial block of code for additional tuning
             
             lClosable ->
             cFontName ->
             
             New: 
             aListImage -> List {img1,img2,imgn}

   Last Modified by Grigory Filatov at 12-07-2021
   
   Last Modified by Ivanil Marcelino at 07-17-2025
*/

#define MARGIN          32
#define MARGIN_ICON     70
#define VMARGIN_BUTTON  4
#define HMARGIN_BUTTON  22
#define SEP_BUTTON      4
#define TAB             Chr( 9 )

STATIC lIsWin10, lPressButton
STATIC aBackColor, aFontColor
Declare window &(cForm)
*-----------------------------------------------------------------------------*
FUNCTION HMG_Alert( cMsg, aOptions, cTitle, nType, cIcoFile, nIcoSize, aBtnColors, bInit, lClosable, cFontName, aListImage,nSeconds )
*-----------------------------------------------------------------------------*
   LOCAL nLineas
   LOCAL aIcon := { "ALERT", "QUESTION", "INFO", "STOP" }
   LOCAL lFont := .F.
   LOCAL lEmpty := ( Empty( aOptions ) .OR. ISNUMERIC( aOptions ) )
   *LOCAL cDelim, cOldDelim
   LOCAL cForm := "oDlg"
   LOCAL nMaxLen := 0

   IF _IsWindowDefined( cForm )
      nLineas := 0
      WHILE _IsWindowDefined( cForm := "oDlg" + hb_ntos( ++nLineas ) )
      END
   ENDIF

   lPressButton := .F.
   lIsWin10 := hb_osIsWin10()

   hb_default( @aBackColor, nRGB2Arr( GetSysColor( COLOR_BTNFACE ) ) )
   hb_default( @aFontColor, nRGB2Arr( GetSysColor( COLOR_BTNTEXT ) ) )
   __defaultNIL( @cTitle, _HMG_MESSAGE [11] /*"Attention"*/ )
   __defaultNIL( @aOptions, { "&OK" } )
   hb_default( @lClosable, .F. )
   hb_default( @cFontName, "DlgFont" )
   hb_default( @nSeconds, 0)

   IF ISARRAY( aOptions )
      DEFAULT nType := iif( Len( aOptions ) > 1, 2, 1 )
   ELSE
      DEFAULT nType := 1
   ENDIF

#ifdef _HMG_COMPAT_
   CHECK TYPE cMsg AS USUAL, ;
      aOptions AS USUAL, ;
      cTitle AS CHARACTER, ;
      nType AS NUMERIC, ;
      cIcoFile AS USUAL, ;
      nIcoSize AS USUAL, ;
      aBtnColors AS USUAL, ;
      bInit AS USUAL, ;
      lClosable AS LOGICAL, ;
      cFontName AS CHARACTER,;
      aListImage AS USUAL,;
      nSeconds as Numeric
#endif
   IF nType < 1 .OR. nType > 4
      nType := 1
   ENDIF

   AEval( aIcon, {| x, i | aIcon[ i ] := "ZZZ_B_" + x } )

   __defaultNIL( @cIcoFile, aIcon[ nType ] )
   hb_default( @nIcoSize, 32 )

   cMsg := cValToChar( cMsg )

   nLineas := MLCount( cMsg )

   IF TAB $ cMsg
      cMsg := StrTran( cMsg, TAB, Space( 3 ) )
   ENDIF

   AEval( hb_ATokens( cMsg, CRLF ), {| ct | nMaxLen := Max( nMaxLen, Len( Trim( ct ) ) ) } )

   IF lEmpty
      lClosable := .T.
      _HMG_ModalDialogReturn := 0
   ELSE
      hb_default( @_HMG_ModalDialogReturn, 0 )
   ENDIF

   DEFINE WINDOW ( cForm ) WIDTH 0 HEIGHT 0 TITLE cTitle MODAL NOSIZE BACKCOLOR aBackColor ;
      ON INTERACTIVECLOSE ( lPressButton .OR. lClosable ) ;
      ON RELEASE iif( ! lPressButton .AND. lClosable, _HMG_ModalDialogReturn := 0, NIL )

      FillDlg( cMsg, aOptions, nLineas, cIcoFile, nIcoSize, aBtnColors, bInit, lClosable, cFontName, nMaxLen,aListImage,nSeconds  )

   END WINDOW

   ACTIVATE WINDOW ( cForm )

   IF lFont
      RELEASE FONT ( cFontName )
   ENDIF

RETURN _HMG_ModalDialogReturn

*-----------------------------------------------------------------------------*
FUNCTION HMG_Alert_MaxLines( nMaxLines )
*-----------------------------------------------------------------------------*
   STATIC s_nMaxLines := 20

   LOCAL nOldLines := s_nMaxLines

   IF HB_ISNUMERIC( nMaxLines ) .AND. nMaxLines > 0
      s_nMaxLines := nMaxLines
   ENDIF

RETURN nOldLines

*-----------------------------------------------------------------------------*
FUNCTION HMG_Alert_RowStart( nRow )
*-----------------------------------------------------------------------------*
   STATIC s_nRowStart := 0

   LOCAL nOldRow := s_nRowStart

   IF HB_ISNUMERIC( nRow ) .AND. nRow >= 0
      s_nRowStart := nRow
   ENDIF

RETURN nOldRow

*-----------------------------------------------------------------------------*
STATIC FUNCTION FillDlg( cMsg, aOptions, nLineas, cIcoFile, nIcoSize, aBtnColors, bBlock, lClosable, cFont, nMaxLen,aListImage,nSeconds )
*-----------------------------------------------------------------------------*
   LOCAL hWnd
   LOCAL hDC
   LOCAL hDlgFont
   LOCAL cForm := ThisWindow.Name
   LOCAL cBtnName
   LOCAL nRow := HMG_Alert_RowStart()
   LOCAL nCol := MARGIN * 0.6
   LOCAL nMaxLin := 0
   LOCAL nMaxBoton := 0
   LOCAL nMaxLines := HMG_Alert_MaxLines()
   LOCAL nMaxHeight
   LOCAL nMaxWidth
   LOCAL nLenBotones
   LOCAL nLenaOp
   LOCAL nWidthCli, nHeightCli
   LOCAL nWidthDlg, nHeightDlg
   LOCAL nChrHeight
   LOCAL nHeightBtn
   LOCAL nVMARGIN_BUTTON //:= VMARGIN_BUTTON
   LOCAL n,nColButton
   Local cTitle,nSec,cFocus

   //Compatibilidade com versoes anteriores
   IF ISNUMERIC( aOptions )
      nSeconds :=  aOptions
      aOptions := { "&OK" }
   endif
   
   nLenaOp :=  Len( aOptions )

   nVMARGIN_BUTTON := 3 * VMARGIN_BUTTON

   hDlgFont := GetFontHandle( cFont )

   // calculate the column of the text output

   IF nIcoSize > 0
      nCol := MARGIN_ICON + iif( nIcoSize == 32, 0, MARGIN_ICON / iif( nIcoSize == 64, 2.8, 3.2 ) )
   ENDIF

   hWnd := This.Handle
   hDC := GetDC( hWnd )

   // calculate the character height for the dialog font

   nChrHeight := GetTextHeight( hDC, aOptions[ 1 ], hDlgFont ) + nVMARGIN_BUTTON / 2

   // calculate the maximum width of the lines

   nMaxWidth := GetFontWidth( cFont, nMaxLen )
   IF GetTextWidth( hDC, Space( 10 ), hDlgFont ) != GetTextWidth( hDC, Replicate( "B", 10 ), hDlgFont )
      nMaxWidth *= 0.7
   ENDIF

   FOR n := 1 TO nLineas
      nMaxLin := Max( nMaxLin, GetTextWidth( hDC, AllTrim( MemoLine( cMsg,, n ) ), hDlgFont ) )
   NEXT

   // calculate the maximum width of the buttons

   FOR n := 1 TO nLenaOp
      nMaxBoton := Max( nMaxBoton, GetTextWidth( hDC, aOptions[ n ], hDlgFont ) )
   NEXT

   ReleaseDC( hWnd, hDC )

   nMaxBoton += ( HMARGIN_BUTTON * iif( lIsWin10 .AND. nLenAop > 2, 1.1, iif( nLenAop > 1, 2, 3 ) ) )

   // calculate the width of the options + their separations

   nLenBotones := ( nMaxBoton + SEP_BUTTON ) * nLenaOp

   nHeightBtn := nVMARGIN_BUTTON + nChrHeight 

   // calculate the width of the client area

   IF nMaxWidth > nMaxLin
      nMaxLin := nMaxWidth
   ENDIF

   nWidthCli := Max( MARGIN_ICON + nMaxLin + MARGIN, MARGIN + nLenBotones + MARGIN - HMARGIN_BUTTON ) + iif( nIcoSize > 48, MARGIN / 4, 0 )
   nWidthDlg := nWidthCli + GetBorderWidth() + iif( nLineas > nMaxLines, MARGIN * 1.5, 0 )

   IF nWidthDlg > GetDesktopRealWidth()
      nMaxWidth := nWidthDlg - GetDesktopRealWidth()
      nWidthDlg := GetDesktopRealWidth()
   ELSE
      nMaxWidth := 0
   ENDIF
   
   nWidthDlg += iif( nLineas < nMaxLines,40,0) //barra

   nHeightCli := ( Min( nMaxLines, nLineas ) + iif( nLineas == 1, 4, 3 ) ) * nChrHeight + nVMARGIN_BUTTON + nHeightBtn + GetBorderHeight()
   nHeightDlg := nHeightCli + GetTitleHeight() + SEP_BUTTON + GetBorderHeight() / iif( lIsWin10, 2.5, 1 )

   IF MSC_VER() > 0 .AND. _HMG_IsThemed
      nWidthDlg += 10
      nHeightDlg += 10
   ENDIF

   IF nHeightDlg > GetDesktopRealHeight()
      n := 0
      WHILE ( nHeightDlg - ( nChrHeight * ( ++n ) ) ) > GetDesktopRealHeight()
      END
      nMaxHeight := nChrHeight * n
      nMaxLines  -= n
      nHeightDlg -= nMaxHeight
      nHeightCli -= nMaxHeight
   ENDIF

   This.Width := nWidthDlg

   This.Height := nHeightDlg

   IF Empty( nRow )
      nRow := nChrHeight
   ENDIF

   
   DEFINE EDITBOX MsgAlert
      PARENT  &( cForm ) 
      ROW nRow + GetBorderHeight()
      COL nCol
      Width  nWidthCli - nCol + (iif( nLineas < nMaxLines + 5, 0.9, 1 ) * MARGIN) - nMaxWidth
      HEIGHT nChrHeight * iif( nLineas < nMaxLines,nLineas,nMaxLines) + GetBorderHeight() 
      VALUE  AllTrim( cMsg )
      READONLY  TRUE
      FONTNAME  cFont
      BACKCOLOR  aBackColor
      FONTCOLOR  aFontColor
      MAXLENGTH  65000
      NOTABSTOP  TRUE
      NOHSCROLLBAR  TRUE
      NOVSCROLLBAR  (nLineas <= nMaxLines)
   END EDITBOX  
   

   SetWindowLong( &( cForm ).MsgAlert.Handle, GWL_EXSTYLE, hb_bitAnd( GetWindowLong(&( cForm ).MsgAlert.Handle, GWL_EXSTYLE), hb_bitNot(WS_EX_CLIENTEDGE) ) )
   SetWindowPos( &( cForm ).MsgAlert.Handle, 0, 0,0,0,0, SWP_NOMOVE + SWP_NOSIZE + SWP_NOZORDER + SWP_FRAMECHANGED )

   IF nIcoSize > 0

      DRAW ICON IN WINDOW ( cForm ) ;
         AT nRow + GetBorderHeight(), MARGIN / iif( nIcoSize == 32, 1.4, iif( nIcoSize == 48, 1.7, 2 ) ) ;
         PICTURE cIcoFile ;
         WIDTH nIcoSize ;
         HEIGHT nIcoSize ;
         TRANSPARENT

   ENDIF
   
   nColButton := (nWidthDlg - nLenBotones) / 2
   
   FOR n := 1 TO nLenaOp

      cBtnName := "Btn_" + StrZero( n, 2 )

      DEFINE BUTTONEX &( cBtnName )
         PARENT  &( cForm )                     
         Row  nHeightCli + SEP_BUTTON + GetBorderHeight() / iif( lIsWin10, 2.5, .9 ) - nChrHeight - nHeightBtn                           
         Col  nColButton                           
         Width  nMaxBoton                          
         Height  nVMARGIN_BUTTON + nChrHeight                          
         CAPTION  aOptions[ n ]                      
         ACTION  ( _HMG_ModalDialogReturn := This.Cargo, lPressButton := .T., ThisWindow.Release() )                     
         FONTNAME  cFont                    
         FONTCOLOR  aFontColor  
         BACKCOLOR  IIF(!empty(aBtnColors),aBtnColors[ n ],Nil)        
         NOXPSTYLE  TRUE                   
         HANDCURSOR TRUE
         if ISARRAY(aListImage) .and. n<=len(aListImage).and.!empty(aListImage[n])
            PICTURE aListImage[n]
            IMAGEHEIGHT (nVMARGIN_BUTTON + nChrHeight -8)
            IMAGEWIDTH  (nVMARGIN_BUTTON + nChrHeight -8)   
         endif
      END BUTTONEX      
      nColButton += ( nMaxBoton + SEP_BUTTON )
      
      This.&( cBtnName ).Cargo := n
      if _HMG_ModalDialogReturn=n
         cFocus := cBtnName
      endif

   NEXT n
   hb_default(@cFocus,"Btn_01")

   &(cform).Closable := lClosable

   &(cform).(cFocus).SetFocus()

   &(cform).Center()

   IF lClosable
      ON KEY ESCAPE OF ( cForm ) ACTION ( _HMG_ModalDialogReturn := 0, lPressButton := .T., ThisWindow.Release() )
   ENDIF

   IF HB_ISBLOCK( bBlock )
      &(cform).OnInit := bBlock
      /*Pode ter sido modificado em OnInit*/
      cTitle := &(cform).Title
   ENDIF

   if nSeconds>0
      nSec:= nSeconds
      DEFINE TIMER oTimer OF ( cForm ) INTERVAL  1000 ACTION (&(cform).title:=cTitle+" ( "+hb_ntos(--nSec)+" )",IIF( nSec=0,(lPressButton := .T., ThisWindow.Release()),Nil))
   ENDIF

RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION _SetMsgAlertColors( aBackClr, aFontClr )
*-----------------------------------------------------------------------------*
   LOCAL aOldClrs := { aBackColor, aFontColor }

   IF aBackClr != NIL
      aBackColor := aBackClr
   ENDIF

   IF aFontClr != NIL
      aFontColor := aFontClr
   ENDIF

RETURN aOldClrs

#ifdef _HMG_COMPAT_

*-----------------------------------------------------------------------------*
#ifndef __XHARBOUR__
PROCEDURE HMG_CheckType( lSoft, ... )
#else
PROCEDURE HMG_CheckType( ... )
#endif
*-----------------------------------------------------------------------------*
   LOCAL i, j
   // aData := { cTypeDef, cValType, cVarName }
   LOCAL aData
   // aType := { cTypeDef, cValType }
   LOCAL aType := { ;
      { "ARRAY", "A" }, ;
      { "BLOCK", "B" }, ;
      { "CHARACTER", "C" }, ;
      { "DATE", "D" }, ;
      { "HASH", "H" }, ;
      { "LOGICAL", "L" }, ;
      { "NIL", "U" }, ;
      { "NUMERIC", "N" }, ;
      { "MEMO", "M" }, ;
      { "POINTER", "P" }, ;
      { "SYMBOL", "S" }, ;
      { "TIMESTAMP", "T" }, ;
      { "OBJECT", "O" }, ;
      { "USUAL", "" } }
   LOCAL aParams := hb_AParams()
#ifdef __XHARBOUR__
   LOCAL lSoft

   lSoft := aParams[ 1 ]
#endif
   hb_ADel( aParams, 1, .T. )    // Remove lSoft parameter from the array

   FOR EACH aData IN aParams

      IF Upper( AllTrim( aData[ 1 ] ) ) <> "USUAL"

         IF !( lSoft .AND. AllTrim( aData[ 2 ] ) == "U" )

            i := AScan( aType, {| x | x[ 1 ] == Upper( AllTrim( aData[ 1 ] ) ) } )

            IF i == 0 .OR. aType[ i ][ 2 ] <> aData[ 2 ]

               j := AScan( aType, {| x | x[ 2 ] == aData[ 2 ] } )

               MsgMiniGuiError( "CHECK TYPE ( Param # " + hb_ntos( aData:__enumindex() ) + " ) : " + AllTrim( aData[ 3 ] ) + " is declared as " + Upper( AllTrim( aData[ 1 ] ) ) + " but it have type " + aType[ j ][ 1 ] + "." )

            ENDIF

         ENDIF

      ENDIF

   NEXT

RETURN

#endif

*=============================================================================*
*                          Auxiliary Functions
*=============================================================================*

FUNCTION GetDesktopRealWidth()

   LOCAL a := GetDesktopArea()

RETURN( a [3] - a [1] )

FUNCTION GetDesktopRealHeight()

   LOCAL a := GetDesktopArea()

RETURN( a [4] - a [2] )

#ifdef __XHARBOUR__
#include "alerts.prg"
#endif
