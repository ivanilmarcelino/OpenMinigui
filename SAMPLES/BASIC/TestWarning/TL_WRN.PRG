
#include "HMG.ch"

//-----------------------------------------------------------------------------

function MsgWarning (cTitle, cWarning, cMessage, cPassword)
local cGetPwd := ""
local lReturn := .F.
local nRow := Application.Row + Int(Application.Height /2) - Int(400 /2)
local nCol := Application.Col + Int(Application.Width /2) - Int(840 /2)

define window oDlgWarning          ;
       at nRow, nCol               ;
       width 575 height 325        ;
       title cTitle                ;
       modal nosize

   @  10,  10 label oLblWarning width 550 height 30 ;
              value cWarning                        ;
              font "ARIAL" size 18 bold underline   ;
              fontcolor {255,0,0}                   ;
              blink centeralign

   @  50,  10 richeditboxex oRtfMessage width 550 height 150 ;
              value cMessage                                 ;
              font "ARIAL" size 12 bold                      ;
              backcolor {255,255,255}                        ;
              readonly nohscroll notabstop

   if !Empty(cPassword)
      @ 210, 175 textbox oTxtPassword                                    ;
                 width 200 height 30                                     ;
                 value cGetPwd                                           ;
                 font "ARIAL" size 12 bold                               ;
                 password                                                ;
                 on enter {|| iif(_ValidPwd(This.Value, cPassword),      ;
                                  ;
                                  (lReturn := .T.,                       ;
                                   oDlgWarning.Release()),               ;
                                  ;
                                  (oDlgWarning.oTxtPassword.Value := "", ;
                                   oDlgWarning.oTxtPassword.SetFocus()))}
   endif

   @ 250,  75 buttonex oBtnCancel   ;
              width 175 height 30   ;
              caption "Cancel"      ;
              picture "CANCEL_20"   ;
              font "TAHOMA" size 10 ;
              notabstop             ;
              action {|| lReturn := .F., oDlgWarning.Release()}

   if !Empty(cPassword)
      @ 250, 300 buttonex oBtnAccept                                                  ;
                 width 175 height 30                                                  ;
                 caption "Accept"                                                     ;
                 picture "ACCEPT_20"                                                  ;
                 font "TAHOMA" size 10                                                ;
                 notabstop                                                            ;
                 action {|| iif(_ValidPwd(oDlgWarning.oTxtPassword.Value, cPassword), ;
                                ;
                                (lReturn := .T.,                                      ;
                                 oDlgWarning.Release()),                              ;
                                ;                                                      
                                (oDlgWarning.oTxtPassword.Value := "",                ;
                                 oDlgWarning.oTxtPassword.SetFocus()))}
   else
      @ 250, 300 buttonex oBtnAccept   ;
                 width 175 height 30   ;
                 caption "Accept"      ;
                 picture "ACCEPT_20"   ;
                 font "TAHOMA" size 10 ;
                 notabstop             ;
                 action {|| lReturn := .T., oDlgWarning.Release()}
   endif

   _DefineHotKey("oDlgWarning",, VK_ESCAPE, {|| oDlgWarning.oBtnCancel.OnClick()})

end window

oDlgWarning.oRtfMessage.SelectAll()
oDlgWarning.oRtfMessage.ParaAlignment := RTF_CENTER
oDlgWarning.oRtfMessage.Caretpos := 0

oDlgWarning.Activate()

Return (lReturn)

//-----------------------------------------------------------------------------

static function _ValidPwd (cGetPwd, cPassword)
local lReturn := .T.

if cGetPwd <> cPassword
   MsgExclamation("Wrong password !", "Error")
   lReturn := .F.
endif

Return (lReturn)


