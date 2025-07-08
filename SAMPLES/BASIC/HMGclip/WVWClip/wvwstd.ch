//
// Copyright 2016 Ashfaq Sial
//

// our preferred method of COMBOBOX behavior
#define WVW_CB_KBD_STANDARD  0
#define WVW_CB_KBD_CLIPPER   1

// Push button width
#define WVW_PB_WIDTH         7

// Text Options
#define TA_LEFT              0
#define TA_RIGHT             2
#define TA_CENTER            6

// Font Weights
#define FW_DONTCARE         0
#define FW_THIN             100
#define FW_EXTRALIGHT       200
#define FW_LIGHT            300
#define FW_NORMAL           400
#define FW_MEDIUM           500
#define FW_SEMIBOLD         600
#define FW_BOLD             700
#define FW_EXTRABOLD        800
#define FW_HEAVY            900

#define PROOF_QUALITY           2

#define STD_FONTNAME        "Arial"
#define STD_FONTSIZE        16

MEMVAR __oObj__
MEMVAR GetList

// xcommand matches all characters. #command only first four.

#xcommand @ <nTop>, <nLeft>, <nBottom>, <nRight> BOX <bt:RAISED,RECESSED> ;
      [OFFSET <aOffset>] ;
      => ;
      AddGuiObj( , {| nWinNum | wvw_DrawBox<bt>( nWinNum, <nTop>, <nLeft>, <nBottom>, <nRight> , ;
                        [<aOffset>] ) ;
                   } ;
               )


#xcommand @ <nTop>, <nLeft>, <nBottom>, <nRight> BOX GROUP ;
      [CAPTION <cCaption> [FONT <hFont>]] ;
      [OFFSET <aOffset>] ;
      => ;
      AddGuiObj( , {| nWinNum | wvw_DrawBoxGroup( nWinNum, <nTop>, <nLeft>, <nBottom>, <nRight>, ;
                        [<aOffset>] ) ;
                   } ;
               ) ; ;
      AddGuiObj( , {| nWinNum | wvw_DrawLabelObj( nWinNum, <nTop>, <nLeft>, <nTop>, <nRight>, [" "+<cCaption>], , , ;
                        wvw_GetRGBColor( hb_ColorToN( Token( SetColor(), "/", 1 ) ) ), ;
                        wvw_GetRGBColor( hb_ColorToN( AtRepl( "*", Token( SetColor(), "/,", 2 ), "+" ) ) ), ;
                        [ <hFont> ], { [<aOffset>\[1\]]-10, [<aOffset>\[2\]]+5, [<aOffset>\[3\]]-10, [<aOffset>\[4\]]-5 } ) ;
                   } ;
               )


#xcommand @ <nRow>, <nCol> SAY <xExpression> ;
      [PICTURE <cPic>] ;
      [FONT <aFont>] ;     /* Makes it different than SAY in std.ch */
      [<jleft:LEFT>] ;
      [COLOUR <cCol>] ;
      => ;
      AddGuiObj( , {| nWinNum | wvw_DrawLabel( nWinNum, <nRow>, <nCol>, Transform(<xExpression>, <cPic>), iif( <.jleft.>, TA_RIGHT, TA_LEFT ), , ;
                        wvw_GetRGBColor( hb_ColorToN( Token( iif( Empty( #<cCol> ), SetColor(), <cCol> ), "/", 1 ) ) ), ;
                        wvw_GetRGBColor( hb_ColorToN( AtRepl( "*", Token( iif( Empty( #<cCol> ), SetColor(), <cCol> ), "/,", 2 ), "+" ) ) ), ;
                        iif( Empty( #<aFont> ), "Arial", <aFont> \[ 1\ ] ), ;
                        iif( Empty( #<aFont> ), 16, <aFont> \[ 2\ ] ), ;
                        Int( Round( iif( Empty( #<aFont> ), 16, <aFont> \[ 2\ ] ) * .4, 0 ) ) ) ;
                   } ;
               )



#xcommand @ <nRow>, <nCol> GET <v> [PICTURE <pic>] ;
      [VALID <valid>] [WHEN <when>] [SEND <snd>] ;
      [CAPTION <cCaption> [FONT <aFont>]] [MESSAGE <msg>] ;
      => ;
      AddGuiObj( , WVWDrawLabel():New( <nRow>, <nCol>-1, <cCaption>, , , , , <aFont> ) ) ; ;
      SetPos( <nRow>, <nCol> ) ; ;
      AAdd( GetList, _GET_( <v>, <"v">, <pic>, <{valid}>, <{when}> ) ) ; ;
    [ ATail( GetList ):message := <msg> ;] [ ATail( GetList ):<snd> ;] ;
      ATail( GetList ):Display()


#xcommand @ <nRow>, <nCol> GET <lVar> CHECKBOX ;
      [CAPTION <cCaption>] ;
      [OFFSET <aOffset>] ;
      => ;
      __oObj__ := WVWCheckBox():New( GetList, <nRow>, <nCol>, <cCaption>, <aOffset> ); ;
      AddGuiCtl( , { || __oObj__:Destroy() } ) ; ;
      __oObj__:SetCheckBox( <lVar> ) ; ;
      SetPos( <nRow>, <nCol> ) ; ;
      AAdd( GetList, _GET_( <lVar>, <"lVar">, "@R Y " + Space( 2 ),, ) ) ; ;
      ATail( GetList ):cargo := __oObj__ ; ;
      ATail( GetList ):reader := { | x | WVWReader( x ) } ; ;
      ATail( GetList ):Display()


#xcommand @ <nTop>, <nLeft>, <nBottom>, <nRight> GET <cVar> EDITBOX ;
      [CAPTION <cCaption> [FONT <aFont>]] ;
      [<noEdit:NOEDIT>] ;
      [OFFSET <aOffset>] ;
      => ;
      AddGuiObj( , WVWDrawLabel():New( <nTop>, <nLeft>-1, <cCaption>, , , , , <aFont> ) ) ; ;
      __oObj__ := wvwEditBox():New( getlist, <nTop>, <nLeft>, <nBottom>, <nRight>, <cVar>, <aOffset>, <.noEdit.> ) ; ;
      AddGuiCtl( , { || __oObj__:Destroy() } ) ; ;
      SetPos( <nTop>, <nLeft> ) ; ;
      AAdd( GetList, _GET_( <cVar>, <"cVar">, Replicate( 'X', <nRight>-<nLeft> ),, ) ) ; ;
      ATail( GetList ):cargo := __oObj__ ; ;
      ATail( GetList ):reader := { | x | WVWReader( x ) } ; ;
      ATail( GetList ):Display()


#xcommand @ <nTop>, <nLeft>, <nBottom>, <nRight> GET <cVar> LISTBOX <aOptions> ;
      [DROPDOWN] ;
      [CAPTION <cCaption> [FONT <aFont>]] ;
      [OFFSET <aOffset>] ;
      => ;
      AddGuiObj( , WVWDrawLabel():New( <nTop>, <nLeft>-1, <cCaption>, , , , , <aFont> ) ) ; ;
      __oObj__ := wvwComboBox():New( getlist, <nTop>, <nLeft>, <nRight>-<nLeft>+1, <aOptions>, WVW_CB_KBD_CLIPPER, <aOffset> ) ; ;
      AddGuiCtl( , { || __oObj__:Destroy() } ) ; ;
      __oObj__:SetIndex( <cVar> ) ; ;
      SetPos( <nTop>, <nLeft> ); ;
      AAdd( GetList, _GET_( <cVar>, <"cVar">, Replicate( iif( ValType( <cVar> ) == "N", "9", "X"), Min( Len( #<cVar> ), <nRight>-<nLeft>+1 ) ),, ) ) ; ;
      ATail( GetList ):cargo := __oObj__ ; ;
      ATail( GetList ):reader := { | x | WVWReader( x ) } ; ;
      ATail( GetList ):Display()


#xcommand @ <nRow>, <nCol> GET <cVar> PUSHBUTTON ;
      [VALID <valid>] [WHEN <when>] ;
      [CAPTION <cCaption>] ;
      [STATE <bAction>] ;
      [OFFSET <aOffset>] ;
      => ;
      __oObj__ := wvwPushButton():New( GetList, <nRow>, <nCol>, <cCaption>, <bAction>, <aOffset> ); ;
      AddGuiCtl( , { || __oObj__:Destroy() } ) ; ;
      SetPos( <nRow>, <nCol> ) ; ;
      AAdd( GetList, _GET_( <cVar>, <"cVar">, "@R Y" + Space( WVW_PB_WIDTH - 1 ), <{valid}>, <{when}> ) ) ; ;
      ATail( GetList ):cargo := __oObj__ ; ;
      ATail( GetList ):reader := { | x | WVWReader( x, <bAction> ) } ; ;
      ATail( GetList ):Display()


#xcommand @ <nTop>, <nLeft>, <nBottom>, <nRight> GET <nVar> RADIOGROUP <aButtons> ;
      [CAPTION <cCaption> [FONT <aFont>]] ;
      [OFFSET <aOffset>] ;
      => ;
      AddGuiObj( , WVWDrawLabel():New( <nTop>-1, <nLeft>, <cCaption>, TA_LEFT, , , , <aFont> ) ) ; ;
      AddGuiObj( , {| | wvw_DrawBoxGroup( , <nTop>, <nLeft>, <nBottom>, <nRight> , ;
                        [<aOffset>] ) ;
                   } ;
               ) ; ;
      __oObj__ := WVWRadioGroup():New( getlist, <nTop>, <nLeft>, <nBottom>, <nRight>, <aButtons>, <aOffset>, <nVar>, <"nVar"> ) ; ;
      AddGuiCtl( , { || __oObj__:Destroy() } ) ; ;
      SetPos( <nTop>, <nLeft>+2 ) ; ;
      AAdd( GetList, _GET_( <nVar>, <"nVar">, '999',, ) ) ; ;
      ATail( GetList ):cargo := __oObj__ ; ;
      ATail( GetList ):reader := { | x | WVWReaderRB( x ) } ; ;
      ATail( GetList ):Display()


#xcommand @ <nRow>, <nCol> LABEL <cLabel> ;
      WIDTH <nWidth> ;
      [FONT <hFont>] ;
      [OFFSET <aOffset>] ;
      => ;
      AddGuiObj( , {| nWinNum | wvw_DrawLabelObj( nWinNum, <nRow>, <nCol>, <nRow>, <nCol>+<nWidth>, <cLabel>, , , ;
                        wvw_GetRGBColor( hb_ColorToN( Token( SetColor(), "/", 1 ) ) ), ;
                        wvw_GetRGBColor( hb_ColorToN( AtRepl( "*", Token( SetColor(), "/,", 2 ), "+" ) ) ), ;
                        [ <hFont> ] [, <aOffset> ] ) ;
                   } ;
               )




#xcommand MENU TO <nOpt> ;
      => ;
      wvw_SetLastMenuEvent( , 0 ) ; ;
      Inkey( 0 ) ; ;
      <nOpt> := wvw_GetLastMenuEvent()


#xcommand ALERT(<cMsg>[, <cHead>]) ;
       => ;
       win_MessageBox( NIL, StrTran(<cMsg>, ";", chr(13) ), <cHead>, 0x00002000 )


// EOF: WVWSTD.CH
