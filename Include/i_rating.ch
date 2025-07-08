/*
 * MINIGUI - Harbour Win32 GUI library source code
 * 
 * Copyright 2014-2025 Grigory Filatov <gfilatov@gmail.com>
 */

#xcommand @ <row>, <col> RATING <name> ;
		[ <of:OF, PARENT> <parent> ] ;
		[ WIDTH <w> ] ;
		[ HEIGHT <h> ] ;
		[ <dummy: PICTURE, FILENAME, FILES> <aImages> ] ;
		[ <res: FROM RESOURCE> ] ;
		[ STARS <count> ] ;
		[ <v:RATE, VALUE> <value> ] ;
		[ SPACING <space> ] ;
		[ ON CHANGE <change> ] ;
		[ TOOLTIP <tooltip> ] ;
		[ <border: BORDER> ] ;
		[ <vertical: VERTICAL> ] ;
		[ <readonly: READONLY> ] ;
		[ <invisible: INVISIBLE> ] ;
   =>;
	_DefineRating ( <"name">, <"parent">, <col>, <row>, <w>, <h>, <value>, <aImages>, <count>, <space>, ;
			<tooltip>, <{change}>, <.border.>, <.res.>, <.readonly.>, <.invisible.>, <.vertical.> )
