
#include "MiniGui.ch"

//-----------------------------------------------------------------------------

procedure Main ()
local cText  := "Sometime it is necessary to display a warning message longer than "+        ;
                "few lines fast and easy. This message box just do that. "+                  ;
                "It even allows optional password protection in case of "+                   ;
                "sensitive operations. "+ CRLF +                                             ;
                "It is not very complicated but I find this function useful."+ CRLF + CRLF + ;
                "Syntax: MsgWarning(cTitle, cWarning, cMessage, [cPassword])"

set date format "dd/mm/yyyy"
set century ON
set epoch to Year(Date()) -25

define window oWndTest      ;
       at 0, 0              ;
       width 475 height 400 ;
       title "MsgWarning()" ;
       main nosize

   @  10,  10 editbox oEdtBox         ;
              width 455 height 275    ;
              value cText             ;
              backcolor {250,250,230} ;
              readonly nohscroll notabstop

   @ 300,  75 button oBtnStart      ;
              caption "Start Test"  ;
              width 100 height 30   ;
              font "TAHOMA" size 10 ;
              action {|| Test()}

   @ 300, 225 button oBtnCancel     ;
              caption "Exit (Esc)"  ;
              width 100 height 30   ;
              font "TAHOMA" size 10 ;
              action {|| oWndTest.Release()}

   _DefineHotKey(,, VK_ESCAPE, {|| oWndTest.Release()})

end window

oWndTest.Center()
oWndTest.Activate()

Return

//-----------------------------------------------------------------------------

function Test ()
local cMsg1 := CRLF +                                                                   ;
               "This action will definively close the «2025» season Calendar."+ CRLF +  ;
               "This operation is not reversible."+ CRLF +                              ;
               "To continue the closing procedure you need to enter the"+ CRLF +        ;
               "administrator password."+ CRLF + CRLF +                                 ;
               "Don't tell anyone, but the password is «Cachou»"

local cMsg2 := "This message contains no control characters. It is a long string of charaters with absolutely no formating whatsoever. This is done intensionnaly in order to demonstrate MiniGUI RICHEDITBOXEX wordwrapping and line centering capabilities. "+   ;
               "Of course, it is possible to skip lines by inserting formatting characters, if needed. Otherwise the word wrapping will take place, so will the text centering. In case of a message that is longer than the frame can display, the "+ ;
               "vertical scrolling will allow browsing through the displayed message. Using this function makes it fast and easy. The message can be from an internal variable, a Memo field or a text file. As long as it's a string of characters."

if MsgWarning("Season closing procedure", "Warning", cMsg1, "Cachou")
   MsgWarning("Just a long message", "Attention Attention Attention", cMsg2)
endif

Return (NIL)
