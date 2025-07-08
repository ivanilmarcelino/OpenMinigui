//

#include "demo.ch"

*-----------------------------------------------------------------------------*
FUNCTION _SetGaps( aGaps, oWnd )
*-----------------------------------------------------------------------------*
RETURN (App.Object):GetGaps( aGaps, oWnd )

*-----------------------------------------------------------------------------*
FUNCTION _Def_Say ( ControlName, x, y, Caption, w, h,                  ;
      fontname, fontsize, bold, BORDER, CLIENTEDGE, HSCROLL, VSCROLL,  ;
      TRANSPARENT, aRGB_bk, aRGB_font, ProcedureName, tooltip, HelpId, ;
      invisible, italic, underline, strikeout, autosize, rightalign,   ;
      centeralign, blink, mouseover, mouseleave, VCenterAlign,         ;
      NoPrefix, nId, aGaps, bInit )
*-----------------------------------------------------------------------------*
   LOCAL o, nLeft, nTop, nRight, nBottom
   LOCAL ow := ThisWindow.Object
   
   VCenterAlign := .T.
   autosize     := .F.
   
   aGaps    := _SetGaps( aGaps, ow )
   nLeft    := aGaps[1]
   nTop     := aGaps[2]
   nRight   := aGaps[3]
   nBottom  := aGaps[4]
   
   WITH OBJECT ow
   
      DEFAULT w := :W(1.5), ;
              h := :H()
   
      :Y += nTop
      :X += nLeft
   
      If '.' $ hb_ntos(w); w := :W( w )
      EndIf

      If '.' $ hb_ntos(h); h := :H( h )
      EndIf
   
      _DefineLabel ( ControlName, :Name, :X, :Y, Caption, w, h, fontname,   ;
         fontsize, bold, BORDER, CLIENTEDGE, HSCROLL, VSCROLL, TRANSPARENT, ;
         aRGB_bk, aRGB_font, ProcedureName, tooltip, HelpId, invisible,     ;
         italic, underline, strikeout, autosize, rightalign, centeralign,   ;
         blink, mouseover, mouseleave, VCenterAlign, NoPrefix, nId )
   
      o        := This.&(ControlName).Object
      o:Left   := nLeft
      o:Top    := nTop
      o:Right  := nRight
      o:Bottom := nBottom
   
      If Empty(y) .and. Empty(x)
         :Y += o:Height
      Else
         If ! Empty(y); :Y += o:Height + nBottom
         EndIf
   
         If ! Empty(x); :X += o:Width  + nRight
         EndIf
      EndIf
   
   END WITH

   Do_ControlEventProcedure( bInit, o:Index, ow, o )
 
RETURN o

*-----------------------------------------------------------------------------*
FUNCTION _Def_Get ( ControlName, x, y, w, h, Value,                         ;
   FontName, FontSize, aToolTip, lPassword, uLostFocus, uGotFocus, uChange, ;
   right, HelpId, readonly, bold, italic, underline, strikeout, field,      ;
   backcolor, fontcolor, invisible, notabstop, nId, valid, cPicture,        ;
   cmessage, cvalidmessage, when, ProcedureName, ProcedureName2, abitmap,   ;
   BtnWidth, lNoMinus, noborder, CenterAlign, aGaps, GotFocusSelect,        ;
   aKeyEvent, dblclick, bInit )
*-----------------------------------------------------------------------------*
   LOCAL nLeft, nTop, nRight, nBottom
   LOCAL i, o, og, ow := ThisWindow.Object
   
   DEFAULT CenterAlign    := .F., ;
           GotFocusSelect := .F.

   If ! Empty(cPicture) .and. '@K ' $ cPicture .and. Empty(uGotFocus)
      cPicture := alltrim(StrTran(cPicture, '@K ', ''))
      If Empty(cPicture)
         cPicture := Nil
      EndIf
      GotFocusSelect := .T.
   EndIf
   
   aGaps    := _SetGaps( aGaps, ow )
   nLeft    := aGaps[1]
   nTop     := aGaps[2]
   nRight   := aGaps[3]
   nBottom  := aGaps[4]
   
   WITH OBJECT ow
   
      DEFAULT w := :W(1.5), ;
              h := :H(), ;
              backcolor := :O:BColorGet, ;
              fontcolor := :O:FColorGet     
   
      :Y += nTop
      :X += nLeft
   
      If '.' $ hb_ntos(w); w := :W( w )
      EndIf

      If '.' $ hb_ntos(h); h := :H( h )
      EndIf
   
      og := _DefineGetBox ( ControlName, :Name, :X, :Y, w, h, Value,           ;
            FontName, FontSize, aToolTip, lPassword, uLostFocus, uGotFocus,    ;
            uChange, right, HelpId, readonly, bold, italic, underline,         ;
            strikeout, field, backcolor, fontcolor, invisible, notabstop, nId, ;
            valid, cPicture, cmessage, cvalidmessage, when, ProcedureName,     ;
            ProcedureName2, abitmap, BtnWidth, lNoMinus, noborder )
   
      o        := This.&(ControlName).Object
      o:Left   := nLeft
      o:Top    := nTop
      o:Right  := nRight
      o:Bottom := nBottom
      i        := o:Index
   
      If CenterAlign
         o:Align := 'CENTER'
      EndIf

      If GotFocusSelect .and. Empty( _HMG_aControlGotFocusProcedure[ i ] )
         Value := o:Value
         If ValType( Value ) == "C"
            _HMG_aControlGotFocusProcedure[ i ] := {|| SendMessage( _HMG_aControlHandles[ i ], EM_SETSEL, 0, If( Empty(Value), -1, Len(Trim(Value))) ) }
         ElseIf ValType( Value ) $ "ND"
            _HMG_aControlGotFocusProcedure[ i ] := {|| SendMessage( _HMG_aControlHandles[ i ], EM_SETSEL, 0, -1 ) }
         EndIf
      EndIf

      If HB_ISARRAY( aKeyEvent ) .and. Len ( aKeyEvent ) > 0
         If ! HB_ISARRAY( aKeyEvent[1] )
           aKeyEvent := { aKeyEvent } 
         EndIf
         AEval( aKeyEvent, {|a| og:SetKeyEvent( a[1], a[2] ) } )
      EndIf

      If HB_ISBLOCK(dblclick)
         og:SetKeyEvent( Nil, dblclick )
      EndIf

      If Empty(y) .and. Empty(x)
         :Y += o:Height
      Else
         If ! Empty(y); :Y += o:Height + nBottom
         EndIf
   
         If ! Empty(x); :X += o:Width  + nRight
         EndIf
      EndIf
      
   END WITH

   Do_ControlEventProcedure( bInit, o:Index, ow, o, og )
 
RETURN og

*-----------------------------------------------------------------------------*
FUNCTION _Def_Btn ( ControlName, x, y, Caption, ProcedureName, w, h,        ;
         fontname, fontsize, tooltip, gotfocus, lostfocus, flat, NoTabStop, ;
         HelpId, invisible, bold, italic, underline, strikeout, multiline,  ;
         default, key, nId, aGaps, bInit )
*-----------------------------------------------------------------------------*
   LOCAL o, nLeft, nTop, nRight, nBottom
   LOCAL ow := ThisWindow.Object
   
   aGaps    := _SetGaps( aGaps, ow )
   nLeft    := aGaps[1]
   nTop     := aGaps[2]
   nRight   := aGaps[3]
   nBottom  := aGaps[4]
   
   WITH OBJECT ow
   
      DEFAULT w := :W(1.5), ;
              h := :H()
   
      :Y += nTop
      :X += nLeft
   
      If '.' $ hb_ntos(w); w := :W( w )
      EndIf

      If '.' $ hb_ntos(h); h := :H( h )
      EndIf
   
      _DefineButton ( ControlName, :Name, :X, :Y, Caption, ProcedureName, w, h, ;
             fontname, fontsize, tooltip, gotfocus, lostfocus, flat, NoTabStop, ;
             HelpId, invisible, bold, italic, underline, strikeout, multiline,  ;
             default, key, nId )
   
      o        := This.&(ControlName).Object
      o:Left   := nLeft
      o:Top    := nTop
      o:Right  := nRight
      o:Bottom := nBottom
      
      If Empty(y) .and. Empty(x)
         :Y += o:Height
      Else
         If ! Empty(y); :Y += o:Height + nBottom
         EndIf
         If ! Empty(x); :X += o:Width  + nRight
         EndIf
      EndIf

   END WITH

   Do_ControlEventProcedure( bInit, o:Index, ow, o )
 
RETURN o

*-----------------------------------------------------------------------------*
FUNCTION _Def_ImgBtn ( ControlName, x, y, Caption, ProcedureName, w, h, image, ;
         tooltip, gotfocus, lostfocus, flat, notrans, HelpId, invisible,       ;
         notabstop, default, icon, extract, nIdx, noxpstyle, key, nId, aGaps,  ;
         bInit )
*-----------------------------------------------------------------------------*
   LOCAL o, nLeft, nTop, nRight, nBottom
   LOCAL ow := ThisWindow.Object
   
   aGaps    := _SetGaps( aGaps, ow )
   nLeft    := aGaps[1]
   nTop     := aGaps[2]
   nRight   := aGaps[3]
   nBottom  := aGaps[4]
   
   WITH OBJECT ow
   
      DEFAULT w := :W(1.5), ;
              h := :H()
   
      :Y += nTop
      :X += nLeft
   
      If '.' $ hb_ntos(w); w := :W( w )
      EndIf

      If '.' $ hb_ntos(h); h := :H( h )
      EndIf
   
      _DefineImageButton ( ControlName, ThisWindow.Name, :X, :Y, Caption, ;
         ProcedureName, w, h, image, tooltip, gotfocus, lostfocus, flat, ;
         notrans, HelpId, invisible, notabstop, default, icon, extract,  ;
         nIdx, noxpstyle, key, nId )
   
      o        := This.&(ControlName).Object
      o:Left   := nLeft
      o:Top    := nTop
      o:Right  := nRight
      o:Bottom := nBottom
      
      If Empty(y) .and. Empty(x)
         :Y += o:Height
      Else
         If ! Empty(y); :Y += o:Height + nBottom
         EndIf
   
         If ! Empty(x); :X += o:Width  + nRight
         EndIf
      EndIf
   
   END WITH

   If HB_ISBLOCK(bInit)
      Do_ControlEventProcedure( bInit, o:Index, ow, o )
   EndIf
 
RETURN o

*-----------------------------------------------------------------------------*
FUNCTION _Def_OwnBtn ( ControlName, x, y, Caption, ProcedureName, w, h, image, ;
   tooltip, gotfocus, lostfocus, flat, notrans, HelpId, invisible, notabstop,  ;
   default, icon, fontname, fontsize, bold, italic, underline, strikeout,      ;
   lvertical, lefttext, uptext, aRGB_bk, aRGB_font, lnohotlight, lnoxpstyle,   ;
   ladjust, handcursor, imagewidth, imageheight, aGradInfo, lhorizontal, aGaps,;
   bInit )
*-----------------------------------------------------------------------------*
   LOCAL o, nLeft, nTop, nRight, nBottom
   LOCAL ow := ThisWindow.Object
   
   ladjust  := .F.
   aGaps    := _SetGaps( aGaps, ow )
   nLeft    := aGaps[1]
   nTop     := aGaps[2]
   nRight   := aGaps[3]
   nBottom  := aGaps[4]
   
   WITH OBJECT ow
   
      DEFAULT w := :W(1.5), ;
              h := :H()
   
      :Y += nTop
      :X += nLeft
   
      If '.' $ hb_ntos(w); w := :W( w )
      EndIf

      If '.' $ hb_ntos(h); h := :H( h )
      EndIf
   
      _DefineOwnerButton ( ControlName, :Name, :X, :Y, Caption, ProcedureName,    ;
         w, h, image, tooltip, gotfocus, lostfocus, flat, notrans, HelpId,      ;
         invisible, notabstop, default, icon, fontname, fontsize, bold, italic, ;
         underline, strikeout, lvertical, lefttext, uptext, aRGB_bk, aRGB_font, ;
         lnohotlight, lnoxpstyle, ladjust, handcursor, imagewidth, imageheight, ;
         aGradInfo, lhorizontal )
   
      o        := This.&(ControlName).Object
      o:Left   := nLeft
      o:Top    := nTop
      o:Right  := nRight
      o:Bottom := nBottom
      
      If Empty(y) .and. Empty(x)
         :Y += o:Height
      Else
         If ! Empty(y); :Y += o:Height + nBottom
         EndIf
   
         If ! Empty(x); :X += o:Width  + nRight
         EndIf
      EndIf
   
   END WITH

   If HB_ISBLOCK(bInit)
      Do_ControlEventProcedure( bInit, o:Index, ow, o )
   EndIf
 
RETURN o

