// Author: Kamil Kalus
// Email: kamilkalus0[at]gmail.com

#xcommand DEFINE SPLITTER <name> ;
    [ AT <row>, <col> ];
    [ ID <nId> ]               ;
    [ <dummy1: OF, PARENT, DIALOG> <parent> ] ;
    [ WIDTH <width> ] ;
    [ HEIGHT <height> ] ;
    [ <vertical: VERTICAL>] ;
    [ <horizontal: HORIZONTAL> ] ;
    [ <rounded: ROUNDED> ] ;
    [ <hideArrow: NOARROW, HIDEARROW> ] ; 
    [ <noHover: NOHOVER> ];
    [ <useGradient: USEGRADIENT> ] ;
    [ GRADIENT <gLeft> , <gMiddle> [, <gRight>] ] ;   
    [ GRADIENTHOVER <gLeftHover> , <gMiddleHover> [, <gRightHover>] ] ;   
    [ COLOR <color> ] ;
    [ COLORHOVER <colorHover> ] ;
    [ BACKCOLOR <bcolor> ] ;
    [ BACKCOLORHOVER <bcolorHover> ] ;
    [ SPLIT <aLeft> FROM <aRight> ] ;
    [ LIMITS <aLimits> ]   ;
    => ;
       [ <name> := ]  _DefineSplitter( <(name)>, <(parent)>, <col>, <row>, <width>, <height>, <.vertical.>, <.horizontal.>, <aLeft>, <aRight>, <aLimits>, <.hideArrow.>, <.noHover.>, ;
        <.useGradient.>, <nId>, <color>, <bcolor>, <colorHover>, <bcolorHover>, <gLeft>, <gMiddle>, <gRight>, <gLeftHover>, <gMiddleHover>, <gRightHover> )
