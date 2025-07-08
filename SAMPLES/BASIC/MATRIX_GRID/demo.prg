/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Demo was contributed to HMG forum by Edward 28/Mar/2019
 *
 */

#include "hmg.ch"

Function Main
Local i, j, xSel
Local aMultiArray:={ 1 , { => } , 'string' , { },  { Nil, Date () } , { 5, 55, 555 }, .T. , { 7 ,  { '7a' , '7b' ,  { '7c' ,  {  77.77 , 777.777 } } } , 777 } , 8, { 1 => "Apples", 2 => "Oranges", "D1" => 0d20180621, "SubHash" => { "S1" => 1, "S2" => 2} , "SubArr" => { "SA1", "SA2", "SA3" } } }
Local aSingleArray:={ 1, 2, 3, 4, 5, 6, 7, 8, 9 }
Local xNOTArray:= 1
Local aEmptyArray:={}
Local aTwoDimArray:=Array(100,10)

SET DATE ANSI
SET CENTURY ON

FOR i = 1 TO LEN( aTwoDimArray )
	DO EVENTS
	FOR j = 1 TO LEN( aTwoDimArray [i] )
		aTwoDimArray [i][j] := ((i - 1) * 10) + j - 1
	NEXT j
NEXT i


DEFINE WINDOW Form_1 ;
	AT 0,0 ;
	WIDTH 450 ;
	HEIGHT 400 ;
	TITLE 'Hello' ;
	MAIN 

	DEFINE MAIN MENU
		DEFINE POPUP 'Matrix Browse'
			MENUITEM 'One-dimensional array' ACTION ( xSel := MatrixBrowse( aSingleArray ), MsgBox ('Selected value ' + hb_valToExp( xSel )) )
			MENUITEM 'Two-dimensional array' ACTION ( xSel := MatrixBrowse( aTwoDimArray, .F. ), MsgBox ('Selected value ' + hb_valToExp( xSel )) )
			MENUITEM 'Multi-dimensional array' ACTION { || ( xSel := MatrixBrowse( aMultiArray ), MsgBox ('Selected value ' + hb_valToExp( xSel )) ) }
			MENUITEM '"_HMG_SYSDATA" array' ACTION { || ( xSel := MatrixBrowse( { _HMG_SYSDATA [260] } ), MsgBox ('Selected value ' + hb_valToExp( xSel )) ) }
			MENUITEM 'Empty array' ACTION ( xSel := MatrixBrowse( aEmptyArray ), MsgBox ('Selected value ' + hb_valToExp( xSel )) )
			MENUITEM 'Not an Array' ACTION ( xSel := MatrixBrowse( xNOTArray ), MsgBox ('Selected value ' + hb_valToExp( xSel )) )
		END POPUP
		
		DEFINE POPUP 'ABrowse'
			MENUITEM 'One-dimensional array' ACTION ( xSel := ABrowse( aSingleArray ), MsgBox ('Selected row ' + hb_valToStr( xSel )) )
			MENUITEM 'Two-dimensional array' ACTION ( xSel := ABrowse( aTwoDimArray, .F. ), MsgBox ('Selected row ' + hb_valToStr( xSel )) )
			MENUITEM 'Multi-dimensional array' ACTION { || ( xSel := ABrowse( aMultiArray ), MsgBox ('Selected row ' + hb_valToStr( xSel )) ) }
			MENUITEM '"_HMG_SYSDATA" array' ACTION { || ( xSel := ABrowse( { _HMG_SYSDATA [260] } ), MsgBox ('Selected row ' + hb_valToStr( xSel )) ) }
			MENUITEM 'Empty array' ACTION ( xSel := ABrowse( aEmptyArray ), MsgBox ('Selected row ' + hb_valToStr( xSel )) )
			MENUITEM 'Not an Array' ACTION ( xSel := ABrowse( xNOTArray ), MsgBox ('Selected row ' + hb_valToStr( xSel )) )
		END POPUP
		
	END MENU

END WINDOW

CENTER WINDOW Form_1

ACTIVATE WINDOW Form_1

Return Nil

*******************************************************************************
Function MatrixBrowse ( aAnyArray , lHeader )
Local nItems, aColumns, cFormName
Local nFormNo := 1
Local nGridMargins := 4, nButtonH := 28, nButtonW := 100, nHeaderH := 24, nCellH := 19, nCellW := 100, nMiddleOfForm 
Local nVScrollW := GetSystemMetrics( 2 /* SM_CXVSCROLL */ )
Local lVScrollVisible
Local _xMatrixValue

Static lBack := .F.

Default lHeader := .T.

IF WIN_OSISXP()
	#ifdef MG_VER_H_	//MiniGui
		nHeaderH := 20
		nCellH := 17
	#else
		nHeaderH := 20
		nCellH := 16
	#endif
ENDIF

IF .NOT. HB_isArray( aAnyArray ) .AND. .NOT. HB_isHash( aAnyArray ) 
	MsgStop ("The variable is not an array and Hash.")
	RETURN Nil
ENDIF

nItems := 1
FOR EACH _xMatrixValue IN aAnyArray
	DO EVENTS
	IF hb_isArray( _xMatrixValue ) .OR. hb_isHash (_xMatrixValue )
		nItems := MAX ( nItems, LEN( _xMatrixValue ) )
	ENDIF
NEXT

_xMatrixValue := Nil

aColumns := Array ( nItems )
AEval( aColumns, { | x, y | aColumns [ y ] := "Column - " + hb_ntos( y ) + " -", x := Nil })

DO WHILE .T.
	DO EVENTS
	cFormName := "_MatrixForm_" + hb_ntos ( nFormNo )
	IF !_IsWindowDefined ( cFormName )
		EXIT
	ENDIF
	nFormNo ++
ENDDO

lVScrollVisible := LEN( aAnyArray ) * nCellH + nGridMargins + IF( lHeader, nHeaderH, 0) + nButtonH + 4 /* space between grid/buttons/form */ > GetDesktopRealHeight()


DEFINE WINDOW &cFormName ;
	AT 0,0 ;
	WIDTH MIN( nItems * nCellW  + nGridMargins + IF( lVScrollVisible, nVScrollW, 0), GetDesktopRealWidth() )  ;
	HEIGHT MIN ( LEN( aAnyArray ) * nCellH + nGridMargins + IF( lHeader, nHeaderH, 0) + nButtonH + 4 /* space between grid/buttons/form */, GetDesktopRealHeight() ) ;
	MODAL ;
	NOSIZE ;
	NOSYSMENU ;
	NOCAPTION
	
	ON KEY ESCAPE ACTION ThisWindow.Release
	
	DEFINE GRID Grid_1
		PARENT &CFormName
		ROW 0 
		COL 0
		WIDTH MIN ( nItems * nCellW  + nGridMargins + IF( lVScrollVisible, nVScrollW, 0) , GetDesktopRealWidth() ) 
		HEIGHT MIN ( LEN( aAnyArray ) * nCellH + nGridMargins + IF(lHeader, nHeaderH, 0) , GetDesktopRealHeight() - nButtonH - 4 /* space between grid/buttons/form */ )
		HEADERS aColumns
		WIDTHS AFill ( Array (nItems), nCellW )
		VALUE { 1, 1 }
		DYNAMICFORECOLOR AFill ( Array (nItems), { || if ( hb_isString(This.CellValue) .AND. Alltrim(This.CellValue) = "{ Inner array }" , RED, BLACK )  } )
		ONDBLCLICK (_xMatrixValue := GetMatrixValue ( aAnyArray, cFormName ) , IF ( .NOT. lBack, ThisWindow.Release, lBack := .F. ) )
		VIRTUAL .T.
		ITEMCOUNT LEN( aAnyArray )
		ONQUERYDATA QueryArray ( aAnyArray )
		SHOWHEADERS lHeader
		CELLNAVIGATION .T.
	END GRID
	
	nMiddleOfForm := (GetProperty( cFormName, 'WIDTH' ) / IF ( nFormNo == 1, 2, 3 ) )
	nButtonW := MIN(nMiddleOfForm - 2, nButtonW)
	
	@ GetProperty( cFormName, 'HEIGHT' ) - nButtonH - 2, nMiddleOfForm - nButtonW - 2 BUTTON B_OK ;
	CAPTION "OK" ;
	ACTION (_xMatrixValue := GetMatrixValue ( aAnyArray, cFormName ) , IF ( .NOT. lBack, ThisWindow.Release, lBack := .F. ) ) ;
	WIDTH nButtonW ;
	HEIGHT nButtonH
	
	@GetProperty( cFormName, 'HEIGHT' )  - nButtonH - 2, nMiddleOfForm + 2 BUTTON B_Cancel ;
	CAPTION "Cancel" ;
	ACTION ThisWindow.Release ;
	WIDTH nButtonW ;
	HEIGHT nButtonH
	
	IF nButtonW < 40
		SetProperty ( cFormName, 'B_Cancel', 'CAPTION', "Abort" )
	ENDIF
	
	IF nFormNo > 1		//Back button
		@GetProperty( cFormName, 'HEIGHT' )  - nButtonH - 2, nMiddleOfForm * 2 + 2 BUTTON B_Back ;
		CAPTION "Back" ;
		ACTION ( lBack := .T., ThisWindow.Release ) ;
		WIDTH nButtonW ;
		HEIGHT nButtonH
	ENDIF
	
END WINDOW

#ifdef MG_VER_H_	//MiniGui
	CENTER WINDOW &cFormName
#else
	CENTER WINDOW &cFormName DESKTOP
#endif

ACTIVATE WINDOW &cFormName

Return _xMatrixValue

*******************************************
Function GetMatrixValue ( aArray, cFormName )
Local nRow := GetProperty(cFormName, 'Grid_1', 'Value' ) [1]
Local nCol := GetProperty(cFormName, 'Grid_1', 'Value' ) [2]
Local _xMatrixValue :=  Nil
Local xCellValue

IF hb_isNumeric ( nRow ) .AND. nRow > 0 .AND. nRow <= LEN( aArray )
	
	xCellValue := GetProperty( cFormName, 'Grid_1', 'Cell', nRow, nCol )

	IF hb_isString( xCellValue ) .AND. Alltrim( xCellvalue ) == '{ Inner array }'
		//show Inner Array
		_xMatrixValue := MatrixBrowse( aArray [nRow] [nCol] )

	ELSEIF hb_isHash( aArray [nRow] ) .AND. LEN ( aArray [nRow ] ) > 0
		IF nCol <= LEN ( aArray [nRow] )
			_xMatrixValue := hb_HValueAt ( aArray [nRow], nCol )
		ENDIF
	ELSEIF hb_isArray( aArray [nRow] ) .AND. LEN ( aArray [nRow ] ) > 0
		IF nCol <= LEN( aArray [nRow] )
			_xMatrixValue := aArray [nRow] [nCol]
		ENDIF
	ELSEIF nCol == 1
		_xMatrixValue := aArray [nRow]
	ENDIF

ENDIF
   
Return _xMatrixValue
******************************************

Procedure QueryArray( aArray )
Local nQRow := This.QueryRowIndex, nQCol := This.QueryColIndex
Local _QVal := "", _QHashKey, _QHashValue
Local _QxValue := aArray [ nQRow ]

IF hb_isHash ( _QxValue ) .AND. Len ( _QxValue ) > 0
	IF nQCol <= Len ( _QxValue )						//checking if the column number doesn't exceed the size of hash
		_QHashKey := hb_HKeyAt( _QxValue, nQCol )
		_QHashValue := hb_HGet( _QxValue, _QHashKey )
		_QVal := hb_ValToExp ( _QHashKey ) + " => "  	
		_QVal += IF( hb_isDate ( _QHashValue ), hb_DtoC ( _QHashValue ), hb_ValToExp ( _QHashValue ) )
	ENDIF
ELSEIF hb_isArray ( _QxValue ) .AND. Len ( _QxValue ) >  0
	IF nQCol <= Len ( _QxValue )						//checking if the column number doesn't exceed the size of array
		IF hb_isArray ( aArray [ nQRow ] [ nQCol ] )	//Inner Array
			_Qval := "{ Inner array }" 			
		ELSE
			_Qval := IF( hb_isDate ( aArray [ nQrow ] [ nQCol ] ), hb_DtoC ( aArray [ nQrow ] [ nQCol ] ), hb_ValToExp ( aArray [ nQrow ] [ nQCol ] ) ) 
		ENDIF
	ENDIF
ELSEIF nQcol == 1									
	_Qval := IF( hb_isDate ( _QxValue ), hb_DtoC ( _QxValue ), hb_ValToExp ( _QxValue ) )
ENDIF

This.QueryData := _Qval
		
Return 
*******************************************************************************

Function ABrowse ( aAnyArray , lHeader )
Local nItems, aColumns
Local nGridMargins := 4, nButtonH := 28, nButtonW := 100, nHeaderH := 24, nCellH := 19, nCellW := 100, nMiddleOfForm 
Local nVScrollW := GetSystemMetrics( 2 /* SM_CXVSCROLL */ )
Local lVScrollVisible

Local _nMatrixValue

Default lHeader := .T.

IF WIN_OSISXP()
	#ifdef MG_VER_H_	//MiniGui
		nHeaderH := 20
		nCellH := 17
	#else
		nHeaderH := 20
		nCellH := 16
	#endif
ENDIF

IF .NOT. HB_isArray( aAnyArray ) .AND. .NOT. HB_isHash( aAnyArray ) 
	MsgStop ("The variable is not an array and Hash.")
	RETURN 0
ENDIF

nItems := 1
FOR EACH _nMatrixValue IN aAnyArray
	DO EVENTS
	IF hb_isArray( _nMatrixValue ) .OR. hb_isHash ( _nMatrixValue )
		nItems := MAX ( nItems, LEN( _nMatrixValue ) )  
	ENDIF
NEXT

_nMatrixValue := 0

aColumns := Array ( nItems )
AEval( aColumns, { | x, y | aColumns [ y ] := "Column - " + hb_ntos( y ) + " -", x := Nil })

lVScrollVisible := LEN( aAnyArray ) * nCellH + nGridMargins + IF( lHeader, nHeaderH, 0) + nButtonH + 4 /* space between grid/buttons/form */ > GetDesktopRealHeight()

DEFINE WINDOW _ABrowseForm_ ;
	AT 0,0 ;
	WIDTH MIN( nItems * nCellW  + nGridMargins + IF( lVScrollVisible, nVScrollW, 0), GetDesktopRealWidth() )  ;
	HEIGHT MIN ( LEN( aAnyArray ) * nCellH + nGridMargins + IF( lHeader, nHeaderH, 0) + nButtonH + 4 /* space between grid/buttons/form */, GetDesktopRealHeight() ) ;
	MODAL ;
	NOSIZE ;
	NOSYSMENU ;
	NOCAPTION 
	
	ON KEY ESCAPE ACTION ThisWindow.Release
	
	DEFINE GRID Grid_1
		PARENT _ABrowseForm_
		ROW 0 
		COL 0
		WIDTH MIN ( nItems * nCellW  + nGridMargins + IF( lVScrollVisible, nVScrollW, 0) , GetDesktopRealWidth() ) 
		HEIGHT MIN ( LEN( aAnyArray ) * nCellH + nGridMargins + IF(lHeader, nHeaderH, 0) , GetDesktopRealHeight() - nButtonH - 4 /* space between grid/buttons/form */ )
		HEADERS aColumns
		WIDTHS AFill ( Array (nItems), nCellW )
		VALUE 1
		DYNAMICFORECOLOR AFill ( Array (nItems), { || if ( hb_isString(This.CellValue) .AND. Alltrim(This.CellValue) = "{ Inner array }" , RED, BLACK )  } )
		ONDBLCLICK (_nMatrixValue := This.Value, ThisWindow.Release)
		VIRTUAL .T.
		ITEMCOUNT LEN( aAnyArray )
		ONQUERYDATA QueryArray ( aAnyArray )
		SHOWHEADERS lHeader
		CELLNAVIGATION .F.
	END GRID
	
	nMiddleOfForm := _ABrowseForm_.WIDTH / 2
	nButtonW := MIN(nMiddleOfForm - 5, nButtonW)
	
	@ _ABrowseForm_.HEIGHT - nButtonH - 2, nMiddleOfForm - nButtonW - 5 BUTTON B_OK ;
	CAPTION "OK" ;
	ACTION (_nMatrixValue := _ABrowseForm_.Grid_1.Value, ThisWindow.Release) ;
	WIDTH nButtonW ;
	HEIGHT nButtonH
	
	@ _ABrowseForm_.HEIGHT - nButtonH - 2, nMiddleOfForm + 5 BUTTON B_Cancel ;
	CAPTION "Cancel" ;
	ACTION ThisWindow.Release ;
	WIDTH nButtonW ;
	HEIGHT nButtonH
    
END WINDOW

#ifdef MG_VER_H_	//MiniGui
	CENTER WINDOW _ABrowseForm_
#else
	CENTER WINDOW _ABrowseForm_ DESKTOP
#endif

ACTIVATE WINDOW _ABrowseForm_

Return _nMatrixValue
