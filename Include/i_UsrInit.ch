#define BASEDEF_HMGBUTTON

#xcommand @ <row>,<col> HMGBUTTON <name> ;
		[ <dummy1: OF, PARENT> <parent> ] ;
		CAPTION <caption> ;
		PICTURE <bitmap> ;
		[ <alignment:LEFT,RIGHT,TOP,BOTTOM> ] ;
		[ <dummy2: ACTION,ON CLICK,ONCLICK> <action> ];
		[ WIDTH <w> ] ;
		[ HEIGHT <h> ] ;
		[ FONT <font> ] ;
		[ SIZE <size> ] ;
		[ <bold : BOLD> ] ;
		[ <italic : ITALIC> ] ;
		[ <underline : UNDERLINE> ] ;
		[ <strikeout : STRIKEOUT> ] ;
		[ TOOLTIP <tooltip> ] ;
		[ <flat: FLAT> ] ;
		[ ON GOTFOCUS <gotfocus> ] ;
		[ ON LOSTFOCUS <lostfocus> ] ;
		[ <notabstop: NOTABSTOP> ] ;
		[ HELPID <helpid> ] 		;
		[ <invisible: INVISIBLE> ] ;
		[ <multiline: MULTILINE> ] ;
		[ <notrans: NOTRANSPARENT> ] ;
	=>;
	_DefineMixedButton ( <"name">, <"parent">, <col>, <row>, <caption>, <{action}>, <w>, <h>, ;
		<font>, <size>, <tooltip>, <{gotfocus}>, <{lostfocus}>, <.flat.>, <.notabstop.>, <helpid>, ;
 		<.invisible.>, <.bold.>, <.italic.>, <.underline.>, <.strikeout.>, <bitmap>, <"alignment">, <.multiline.>, <.notrans.> ) 

#ifdef PICTALIGNMENT
#undef PICTALIGNMENT
#endif

#xcommand PICTALIGNMENT <alignment:LEFT,RIGHT,TOP,BOTTOM> ;
	=> ;
	_HMG_ActiveControlFormat := <"alignment">

#xcommand END HMGBUTTON ;
	=>;
	_DefineMixedButton ( ;
          _HMG_ActiveControlName,;
          _HMG_ActiveControlOf,;
          _HMG_ActiveControlCol,;
          _HMG_ActiveControlRow,;
          _HMG_ActiveControlCaption ,;
          _HMG_ActiveControlAction ,;
          _HMG_ActiveControlWidth ,;
          _HMG_ActiveControlHeight ,;
          _HMG_ActiveControlFont ,;
          _HMG_ActiveControlSize ,;
          _HMG_ActiveControlTooltip ,;
          _HMG_ActiveControlOnGotfocus ,;
          _HMG_ActiveControlOnLostfocus ,;
          _HMG_ActiveControlFlat ,;
          _HMG_ActiveControlNoTabStop ,;
          _HMG_ActiveControlHelpId ,;
          _HMG_ActiveControlInvisible ,;
          _HMG_ActiveControlFontBold , ;
          _HMG_ActiveControlFontItalic , ;
          _HMG_ActiveControlFontUnderLine ,;
          _HMG_ActiveControlFontStrikeOut ,;
          _HMG_ActiveControlPicture ,;
          _HMG_ActiveControlFormat ,;
          _HMG_ActiveControlBorder ,;
           .not. _HMG_ActiveControlTransparent ) 

#undef BASEDEF_HMGBUTTON


#define BASEDEF_CLBUTTON

#xcommand @ <row>,<col> CLBUTTON <name> ;
		[ <of:OF, PARENT> <parent> ] ;
		[ WIDTH <w> ] ;
		[ HEIGHT <h> ] ;
		CAPTION <caption> ;
		NOTETEXT <notes> ;
		[ <dummy:IMAGE, PICTURE> <cbitmap> ] ;
		ACTION <action> ;
		[ <default: DEFAULT> ] ;
	=>;
	_DefineCLButton(<(name)>,<row>,<col>,<caption>,<notes>,<{action}>,<(parent)>,<.default.>,<w>,<h>,<cbitmap>)

#undef BASEDEF_CLBUTTON


#define BASEDEF_SPBUTTON

#xcommand @ <row>,<col> SPLITBUTTON <name> ;
		[ <of:OF, PARENT> <parent> ] ;
		[ WIDTH <w> ] ;
		[ HEIGHT <h> ] ;
		CAPTION <caption> ;
		[ <dummy:IMAGE, ICON> <cicon> ] ;
		ACTION <action> ;
		[ FONT <font> ] ;
		[ SIZE <size> ] ;
		[ <bold : BOLD> ] ;
		[ <italic : ITALIC> ] ;
		[ <underline : UNDERLINE> ] ;
		[ <strikeout : STRIKEOUT> ] ;
		[ TOOLTIP <tooltip> ] ;
		[ <default: DEFAULT> ] ;
	=>;
	_DefineSplitButton(<(name)>,<row>,<col>,<caption>,<{action}>,<(parent)>,<.default.>,<w>,<h>, ;
		<tooltip>, <font>, <size>, <.bold.>, <.italic.>, <.underline.>, <.strikeout.>, <cicon>)

#undef BASEDEF_SPBUTTON


#define BASEDEF_MYSYSLINK

#xcommand @ <row>,<col> MYSYSLINK <name> ;
		OF <parent> ;
		CAPTION <caption> ;
		ACTION <action> ;
	=>;
	_DefineMySysLink(<(name)>,<row>,<col>,<caption>,<{action}>,<(parent)>)

#undef BASEDEF_MYSYSLINK


#define BASEDEF_ANIMATERES

#command @ <row>,<col> ANIMATERES <name> ;
		[ <dummy1: OF, PARENT> <parent> ] ;
		[ WIDTH <w> ] ;
		[ HEIGHT <h> ] ;
		FILE <file> ;
		ID <n> ;
		[ TOOLTIP <tooltip> ] ;
		[ HELPID <helpid> ] ;
		[ <invisible : INVISIBLE> ] ;
	=>;
	_DefineAnimateRes ( <(name)>, <(parent)>, <col>, <row>, <w>, <h>, <file>, <n>, <tooltip>, <helpid>, <.invisible.> )

#undef BASEDEF_ANIMATERES


#define BASEDEF_WEBCAM

#xcommand @ <row>, <col> WEBCAM <name> ;
		[ <of:OF, PARENT> <parent> ] ;
		[ WIDTH <w> ] ;
		[ HEIGHT <h> ] ;
		[ <lOn: START> ] ;
		[ RATE <rate> ] ;
		[ TOOLTIP <tooltip> ] ;
		[ HELPID <helpid> ] ;
	=>;
	_DefineWebCam ( <(name)>, <(parent)>, <col>, <row>, <w>, <h>, <.lOn.>, <rate>, <tooltip>, <helpid> )

#xcommand CAM_START <start> ;
	=>;
	_HMG_ActiveControlBorder := <start>

#xcommand CAM_RATE <rate> ;
   =>;
   _HMG_ActiveControlValue   := <rate>

#xcommand DEFINE WEBCAM <name> ;
	=>;
	_HMG_ActiveControlName		:= <(name)>	;;
	_HMG_ActiveControlOf		:= ""		;;
	_HMG_ActiveControlRow		:= 0		;;
	_HMG_ActiveControlCol		:= 0		;;
	_HMG_ActiveControlWidth		:= 0		;;
	_HMG_ActiveControlHeight	:= 0		;;
	_HMG_ActiveControlTooltip   := Nil      ;;
	_HMG_ActiveControlHelpId    := Nil      ;;
	_HMG_ActiveControlValue     := Nil		;;
	_HMG_ActiveControlBorder	:= .f.

#xcommand END WEBCAM ;
	=>;
	_DefineWebCam( ;
		_HMG_ActiveControlName , ;
		_HMG_ActiveControlOf , ;
		_HMG_ActiveControlCol , ;
		_HMG_ActiveControlRow , ;
		_HMG_ActiveControlWidth , ;
		_HMG_ActiveControlHeight , ;
		_HMG_ActiveControlBorder , ;
		_HMG_ActiveControlValue , ;
		_HMG_ActiveControlTooltip , ;
		_HMG_ActiveControlHelpId )

#undef BASEDEF_WEBCAM


#define BASEDEF_ACTIVEX

#xcommand @ <row>,<col> ACTIVEX <name> ;
		[ <dummy1: OF, PARENT> <parent> ] ;
		WIDTH <w>  ;
		HEIGHT <h>  ;
		PROGID <progid>  ;
		[ EVENTMAP <aEvents> ]  ;
		[ <clientedge: CLIENTEDGE> ] ;
	=>;
	_DefineActivex( <(name)>, <(parent)>, <row>, <col>, <w>, <h>, <progid>, <aEvents>, <.clientedge.> )

#xcommand PROGID <progid> ;
	=>;
	_HMG_ActiveControlBorder	:= <progid>

#xcommand EVENTMAP <aEvents> ;
   =>;
   _HMG_ActiveControlValue := <aEvents>

#xcommand DEFINE ACTIVEX <name> ;
	=>;
	_HMG_ActiveControlName		:= <(name)>	;;
	_HMG_ActiveControlOf		:= ""		;;
	_HMG_ActiveControlRow		:= 0		;;
	_HMG_ActiveControlCol		:= 0		;;
	_HMG_ActiveControlWidth		:= 0		;;
	_HMG_ActiveControlHeight	:= 0		;;
	_HMG_ActiveControlValue		:= Nil		;;
	_HMG_ActiveControlClientEdge:= .f.		;;
	_HMG_ActiveControlBorder	:= 0

#xcommand END ACTIVEX ;
	=>;
	_DefineActivex( ;
		_HMG_ActiveControlName , ;
		_HMG_ActiveControlOf , ;
		_HMG_ActiveControlRow , ;
		_HMG_ActiveControlCol , ;
		_HMG_ActiveControlWidth , ;
		_HMG_ActiveControlHeight , ;
		_HMG_ActiveControlBorder , ;
		_HMG_ActiveControlValue , ;
		_HMG_ActiveControlClientEdge )

#undef BASEDEF_ACTIVEX
