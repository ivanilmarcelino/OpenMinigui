/*
  MINIGUI - Harbour Win32 GUI library Demo/Sample

  Copyright 2002-09 Roberto Lopez <harbourminigui@gmail.com>
  http://harbourminigui.googlepages.com

  CSBox ( Combined Search Box )

  Started by Bicahi Esgici <esgici@gmail.com>

  Enhanced by S.Rathinagiri <rathinagiri@sancharnet.in>
*/

#command @ <row>, <col> COMBOSEARCHBOX <name> ;
      [ <dummy1: OF, PARENT> <parent> ] ;
      [ HEIGHT <height> ] ;
      [ WIDTH <width> ] ;
      [ VALUE <value> ] ;
      [ FONT <fontname> ] ;
      [ SIZE <fontsize> ] ;
      [ <bold : BOLD> ] ;
      [ <italic : ITALIC> ] ;
      [ <underline : UNDERLINE> ] ;
      [ TOOLTIP <tooltip> ] ;
      [ BACKCOLOR <backcolor> ] ;
      [ FONTCOLOR <fontcolor> ] ;
      [ MAXLENGTH <maxlenght> ] ;
      [ <upper: UPPERCASE> ] ;
      [ <lower: LOWERCASE> ] ;
      [ <numeric: NUMERIC> ] ;
      [ ON GOTFOCUS <gotfocus> ] ;
      [ ON LOSTFOCUS <lostfocus> ] ;
      [ ON ENTER <enter> ] ;
      [ <RightAlign: RIGHTALIGN> ] ;
      [ <notabstop: NOTABSTOP> ] ;
      [ HELPID <helpid> ] ;
      [ ITEMS <aitems>  ] ;
      => ;
      _DefineComboSearchBox( <"name">, <"parent">, <col>, <row>, <width>, <height>, <value>, ;
      <fontname>, <fontsize>, <tooltip>, <maxlenght>, ;
      <.upper.>, <.lower.>, <.numeric.>, ;
      <{ lostfocus }>, <{ gotfocus }>, <{ enter }>, ;
      <.RightAlign.>, <helpid>, <.bold.>, <.italic.>, <.underline.>, <backcolor>, <fontcolor>, <.notabstop.>, <aitems> )

#xcommand DEFINE COMBOSEARCHBOX <name> ;
      => ;
      _HMG_ActiveControlName := <"name"> ; ;
      _HMG_ActiveControlOf := NIL ; ;
      _HMG_ActiveControlRow := NIL ; ;
      _HMG_ActiveControlCol := NIL ; ;
      _HMG_ActiveControlHeight := NIL ; ;
      _HMG_ActiveControlWidth := NIL ; ;
      _HMG_ActiveControlValue := NIL ; ;
      _HMG_ActiveControlFont := NIL ; ;
      _HMG_ActiveControlSize := NIL ; ;
      _HMG_ActiveControlFontBold := .F. ; ;
      _HMG_ActiveControlFontItalic := .F. ; ;
      _HMG_ActiveControlFontUnderLine := .F. ; ;
      _HMG_ActiveControlTooltip := NIL ; ;
      _HMG_ActiveControlBackColor := NIL ; ;
      _HMG_ActiveControlFontColor := NIL ; ;
      _HMG_ActiveControlMaxLength := NIL ; ;
      _HMG_ActiveControlUpperCase := .F. ; ;
      _HMG_ActiveControlLowerCase := .F. ; ;
      _HMG_ActiveControlNumeric := .F. ; ;
      _HMG_ActiveControlOnGotFocus := NIL ; ;
      _HMG_ActiveControlOnLostFocus := NIL ; ;
      _HMG_ActiveControlOnEnter := NIL ; ;
      _HMG_ActiveControlHelpId := NIL ; ;
      _HMG_ActiveControlRightAlign := .F. ; ;
      _HMG_ActiveControlNoTabStop := .T. ; ;
      _HMG_ActiveControlItems := NIL


#xcommand END COMBOSEARCHBOX ;
      => ;
      _DefineComboSearchBox( ;
      _HMG_ActiveControlName, ;
      _HMG_ActiveControlOf, ;
      _HMG_ActiveControlCol, ;
      _HMG_ActiveControlRow, ;
      _HMG_ActiveControlWidth, ;
      _HMG_ActiveControlHeight, ;
      _HMG_ActiveControlValue, ;
      _HMG_ActiveControlFont, ;
      _HMG_ActiveControlSize, ;
      _HMG_ActiveControlTooltip, ;
      _HMG_ActiveControlMaxLength, ;
      _HMG_ActiveControlUpperCase, ;
      _HMG_ActiveControlLowerCase, ;
      _HMG_ActiveControlNumeric, ;
      _HMG_ActiveControlOnLostFocus, ;
      _HMG_ActiveControlOnGotFocus, ;
      _HMG_ActiveControlOnEnter, ;
      _HMG_ActiveControlRightAlign, ;
      _HMG_ActiveControlHelpId, ;
      _HMG_ActiveControlFontBold, ;
      _HMG_ActiveControlFontItalic, ;
      _HMG_ActiveControlFontUnderLine, ;
      _HMG_ActiveControlBackColor, ;
      _HMG_ActiveControlFontColor, ;
      _HMG_ActiveControlNoTabStop, ;
      _HMG_ActiveControlItems )
