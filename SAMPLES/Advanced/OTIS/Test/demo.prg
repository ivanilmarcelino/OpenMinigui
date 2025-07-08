/*
 * MiniGUI Grid Demo
*/

#include "hmg.ch"

Function Main
	Local bColor, fColor

	SET EXCL OFF
	SET CENTURY ON
	SET DATE GERMAN

	bColor := { || iif ( This.CellRowIndex/2 == int(This.CellRowIndex/2) , { 245,245,245 } , { 255,255,255 } ) }	
	fColor := { || iif ( This.CellRowIndex/2 == int(This.CellRowIndex/2) , { 0,0,150 } , { 0,0,0 } ) }	

	CellNavigationColor (_SELECTEDCELL_BACKCOLOR, { 136, 177, 75 } )

	DEFINE WINDOW Form_1 ;
		AT 0,0 ;
		WIDTH 800 ;
		HEIGHT 510 ;
		TITLE 'Hello World!' ;
		MAIN ;
		ON RELEASE OnExit()

		DEFINE MAIN MENU 
			POPUP 'File'
				MENUITEM 'Otis'	ACTION Otis()
				MENUITEM 'Exit'	ACTION ThisWindow.Release
			END POPUP
		END MENU

		@ 10,10 GRID Grid_1 ;
			WIDTH 760 ;
			AUTOSIZEHEIGHT 21 ;
			HEADERS {'Column 1','Column 2','Column 3','Column 4','Column 5','Column 6'} ;
			WIDTHS {90,180,180,100,90,90};
			CELLNAVIGATION ;
			VALUE { 1 , 1 } ;
			JUSTIFY { BROWSE_JTFY_RIGHT, BROWSE_JTFY_CENTER, BROWSE_JTFY_CENTER, BROWSE_JTFY_CENTER, BROWSE_JTFY_CENTER, BROWSE_JTFY_CENTER } ;
			DYNAMICFORECOLOR { fColor , fColor , fColor , fColor , fColor , fColor } ;
			DYNAMICBACKCOLOR { bColor , bColor , bColor , bColor , bColor , bColor } ;
			LOCKCOLUMNS 1 ;
			FONT 'Courier New' ;
			SIZE 9 

	END WINDOW

	USE TEST SHARED NEW
	INDEX ON TEST->CODE TO CODE
	GO TOP
        WHILE !EOF()
           ADD ITEM  {fieldget(1),fieldget(2),fieldget(3),fieldget(4),fieldget(5),fieldget(6)} TO Grid_1 OF Form_1
           dbSkip(1)
        ENDDO
	GO TOP

	CENTER WINDOW Form_1

	ACTIVATE WINDOW Form_1

Return Nil

Procedure OnExit
	Close TEST
	DELETE FILE CODE.ntx
Return
