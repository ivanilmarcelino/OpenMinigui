/*
 * Project:
 * File: TS_ListBoxEx.prg
 * Description:
 * Author:
 * Date: 05-12-2018
 */

#include "hmg.ch"
#include "tsbrowse.ch"


#define GWL_EXSTYLE              (-20)
#define WS_EX_DLGMODALFRAME 0x00000001
#define WS_EX_LEFTSCROLLBAR 0x00004000
#define WS_EX_TOOLWINDOW    0x00000080

#define TB_HEIGHTCELL               20

STATIC oWLbx, oLbx

#include "hbclass.ch"

CREATE CLASS Lbx

   VAR oBrw
   VAR oBrwParent
   VAR oWLbx
   VAR oLbx
   VAR aHeaders INIT {}
   VAR aFooters INIT {}
   VAR aData INIT {}
   VAR aAlign INIT {}
   VAR aFAlign INIT {}
   VAR aHAlign INIT {}
   VAR aWidth INIT {}
   VAR aField INIT {}
   VAR aFont INIT {}

   VAR cAlias INIT "LBX"
   VAR cRetField INIT "ID"

   VAR bPostBlock INIT NIL
   VAR bSearch INIT NIL

   VAR nHeightCell INIT 20
   VAR nHeightHead INIT 20
   VAR nHeightFoot INIT 20

   METHOD New()
   METHOD ListBox( oBrw, xVal )
   METHOD Release( oBrw )
   METHOD UserKeys( nKey, nFlg, oBrw )

ENDCLASS

METHOD Lbx:New()

RETURN SELF

METHOD Lbx:ListBox( oBrw, xVal )

   LOCAL nRecCount := 0
   LOCAL nCol := 0
   LOCAL nWidth := 0
   LOCAL nHeight := 0
   LOCAL nRow := 0
   LOCAL oCell
   LOCAL aWRect := { 0, 0, 0, 0 }
   LOCAL aCRect := { 0, 0, 0, 0 }
   LOCAL i := 0
   LOCAL nWh := 0
   LOCAL lM := _HMG_IsModalActive
   LOCAL hM := _HMG_ActiveModalHandle
   LOCAL hWnd

   HB_SYMBOL_UNUSED( xVal )

   _HMG_IsModalActive := .F.
   _HMG_ActiveModalHandle := 0

   IF ! Empty( ::aWidth )
      AEval( ::aWidth, {| e | nWh += e + 1 } )
   ENDIF

   SET OOP ON

   GetWindowRect( oBrw:hWnd, aWRect )

   oCell := oBrw:GetCellinfo( oBrw:nRowPos, oBrw:nCell, FALSE )
   nCol := oCell:nCol + aWRect[ 1 ] - oBrw:nLeft
   nRow := oCell:nRow + aWRect[ 2 ] - oBrw:nTop
   nWidth := if( ! Empty( ::aWidth ), nWh, oCell:nWidth )
   nHeight := oCell:nHeight

   // Поправка на координаты при использовании TAB
   IF __objHasData( oBrw, 'nColShift' )
      IF oBrw:nColShift <> NIL
         nCol := nCol - oBrw:nColShift
      ENDIF
   ENDIF

   IF __objHasData( oBrw, 'nRowShift' )
      IF oBrw:nRowShift <> NIL
         nRow := nRow - oBrw:nRowShift
      ENDIF
   ENDIF

   // Если выезжает за низ окна
   IF Get_DeskTopHeight() - ( nRow + nHeight ) < Min( ( ( ::cAlias )->( RecCount() ) + 2 ) * ::nHeightCell, 300 ) // Нужно показывать вверх
      nRow := nRow - Min( ( ::cAlias )->( RecCount() + 2 ) * ( ::nHeightCell ) + ( ::nHeightCell ), 300 ) - oBrw:nHeightCell
   ENDIF

   // Если выезжает справа -  двигать окно влево ( не сделано )

   DEFINE WINDOW LBEX ;
         AT nRow + nHeight, nCol ;
         WIDTH nWidth ;
         HEIGHT Min( ( ::cAlias )->( RecCount() + 2 ) * ( ::nHeightCell ) + ( ::nHeightCell ) + ( ::nHeightHead ), 310 ) ;
         NOCAPTION ;
         CHILD ;
         ON LOSTFOCUS {|| oWLbx:Release() } ;
         ON RELEASE {|| ::Release( oBrw ) }

      oWLbx := This.OBJECT

   END WINDOW

   hWnd := GetFormHandle( "LBEX" )
   SetWindowLong( hWnd, GWL_EXSTYLE, WS_EX_TOOLWINDOW )
   SetWindowLong( hWnd, GWL_STYLE, WS_DLGFRAME )

      DEFINE TBROWSE oLbx AT 25, 0 ALIAS ::cAlias ;
         OF LBEX ;
         WIDTH oWLbx:Width - 3 ;
         HEIGHT oWLbx:Height - 35 ;

      END TBROWSE

      ::oBrw := oLbx
      ::oBrwParent := oBrw

      SetWindowLong ( ::oBrw:hWnd, GWL_EXSTYLE, WS_EX_STATICEDGE )

      IF HB_ISARRAY( ::aField ) .AND. Len( ::aField ) > 0
         ::oBrw:aColSel := ::aField
      ENDIF

      ::oBrw:LoadFields( TRUE )
      ::oBrw:lCellBrw := TRUE
      ::oBrw:nHeightCell := ::nHeightCell
      ::oBrw:nHeightHead := ::nHeightHead
      ::oBrw:nHeightFoot := ::nHeightFoot
      ::oBrw:lNoHScroll := TRUE


      ::oBrw:bLDblClick := {|| ;
         ( ::oBrwParent:cAlias )->&( ::oBrwParent:GetColumn( ::oBrwParent:nCell ):cName ) := ( ::oBrw:cAlias )->&( ::cRetField ), ;
         oWLbx:Release() ;
         }

      ::oBrw:bUserKeys := {| nKy, nFl, oBr | ::UserKeys( nKy, nFl, oBr ) }

      ::oBrw:SetColor( { 6 }, { {| a, b, c | IF( c:nCell == b, -CLR_HRED, -RGB( 128, 225, 225 ) ) } } ) // фон курсора
      ::oBrw:SetColor( { 2 }, { {|| GetSysColor( COLOR_BTNFACE ) } }, ) // фон
      ::oBrw:hBrush := CreateSolidBrush( ToRGB( GetSysColor( COLOR_BTNFACE ) )[ 1 ], ;
         ToRGB( GetSysColor( COLOR_BTNFACE ) )[ 2 ], ;
         ToRGB( GetSysColor( COLOR_BTNFACE ) )[ 3 ] )


      IF HB_ISARRAY( ::aWidth ) .AND. Len( ::aWidth ) > 0
         FOR i := 1 TO Len( ::aWidth )
            ::oBrw:SetColSize( i, ::aWidth[ i ] )
         END
      ELSE
         ::oBrw:SetColSize( 1, nWidth )
      END

      IF HB_ISARRAY( ::aHeaders ) .AND. Len( ::aHeaders ) > 0
         FOR i := 1 TO Len( ::aHeaders )
            ::oBrw:aColumns[ i ]:cHeading := ::aHeaders[ i ]
         END
      END

      IF HB_ISARRAY( ::aAlign ) .AND. Len( ::aAlign ) > 0
         FOR i := 1 TO Len( ::aAlign )
            ::oBrw:aColumns[ i ]:nAlign := ::aAlign[ i ]
         END
      END


      AEval( ::oBrw:aColumns(), {| oCol | oCol:nClrSeleBack := oCol:nClrFocuBack, oCol:nClrSeleFore := oCol:nClrFocuFore, oCol:lEdit := FALSE } )

      ::oBrw:Reset()
      ::oBrw:SetNoHoles()
      ::oBrw:SetFocus()

      DEFINE IMAGE Image_1
         PARENT LBEX
         ROW 3
         COL 0
         WIDTH 15
         HEIGHT 15
         PICTURE 'FIND'
         STRETCH .F.
      END IMAGE


      DEFINE GETBOX Text_FTS
         PARENT LBEX
         ROW 0
         COL 16
         WIDTH oWLbx:Width - 42
         HEIGHT ::nHeightCell
         VALUE Space( 20 )
         FONTNAME 'Arial'
         FONTSIZE 9
         FONTBOLD FALSE
         TOOLTIP ''
         READONLY FALSE
         MAXLENGTH 100
         BACKCOLOR { 255, 255, 255 }
         ON CHANGE IF( HB_ISBLOCK( ::bSearch ), Eval( ::bSearch, ::oBrw, AllTrim( This.Value ), 1 ), Nil )
      END GETBOX


      DEFINE BUTTONEX Button_Del
         PARENT LBEX
         ROW 3
         COL oWLbx:Width - 22
         WIDTH 15
         HEIGHT 15
         ACTION {|| ( ::oBrwParent:cAlias )->&( ::oBrwParent:GetColumn( oBrw:nCell ):cName ) := Blank( ( ::oBrwParent:cAlias )->&( ::oBrwParent:GetColumn( oBrw:nCell ):cName ) ), oWLbx:Release() }
         CAPTION ""
         PICTURE "DB_CANCEL"
         TABSTOP .F.
         TOOLTIP "WARNING It will clear a value in a cell of table!"
         FONTNAME "Arial"
         FONTSIZE 9
      END BUTTONEX


      ON KEY ESCAPE OF LBEX ACTION LBEX.RELEASE
      DoMethod( "LBEX", "Text_FTS", "SetFocus" )

   ACTIVATE WINDOW LBEX

   _HMG_IsModalActive := lM
   _HMG_ActiveModalHandle := hM

RETURN NIL

METHOD Release( oBrw ) CLASS Lbx

   oBrw:SetFocus()
   oBrw:DrawSelect()
   IF(HB_IsBlock( ::bPostBlock), Eval(::bPostBlock, oBrw, oLbx ), NIL)

RETURN NIL

METHOD UserKeys( nKey, nFlg, oBrw ) CLASS Lbx

   LOCAL uRet

   nFlg := Nil
   Do Case
   Case nKey == VK_RETURN .OR.  nKey == VK_SPACE
      uRet := .F.
      ( ::oBrwParent:cAlias )->&( ::oBrwParent:GetColumn(::oBrwParent:nCell):cName ) := ( ::oBrw:cAlias )->&(::cRetField)
      oWLbx:Release()

   Case nKey < 48
      oBrw:SetFocus()

   OtherWise
      uRet := .F.
      oBrw:SetFocus()
   EndCase

RETURN uRet


FUNC Get_DeskTopWidth()

   LOCAL aRect := GetDeskTopArea() // { left, top, right, bottom }
   LOCAL nDeskTopWidth := aRect[ 3 ] - aRect[ 1 ]
   LOCAL nDeskTopHeight := aRect[ 4 ] - aRect[ 2 ]

RETURN nDeskTopWidth

FUNC Get_DeskTopHeight()

   LOCAL aRect := GetDeskTopArea() // { left, top, right, bottom }
   LOCAL nDeskTopWidth := aRect[ 3 ] - aRect[ 1 ]
   LOCAL nDeskTopHeight := aRect[ 4 ] - aRect[ 2 ]

RETURN nDeskTopHeight

STAT FUNC ToRGB( nColor )

   LOCAL nR := 0
   LOCAL nG := 0
   LOCAL nB := 0
   LOCAL cColor := NTOC( nColor, 16 )
         /*
         BBGGRR, where XX - number from 00 to FF.
         */
   nR := CTON( SubStr( cColor, 5, 2 ), 16 )
   nG := CTON( SubStr( cColor, 3, 2 ), 16 )
   nB := CTON( SubStr( cColor, 1, 2 ), 16 )

RETURN { nR, nG, nB }
