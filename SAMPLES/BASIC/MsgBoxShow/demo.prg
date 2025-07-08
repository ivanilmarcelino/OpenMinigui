ANNOUNCE RDDSYS

#include "minigui.ch"

*------------------------------------------------------------------------------*
PROCEDURE MAIN
*------------------------------------------------------------------------------*

   LOAD WINDOW Demo
   CENTER WINDOW Demo
   ACTIVATE WINDOW Demo

RETURN

*------------------------------------------------------------------------------*
FUNCTION MsgAbout()
*------------------------------------------------------------------------------*

RETURN AlertInfo( ;
      "Message Box Show - FREEWARE" + ";" + ;
      "Copyright " + Chr( 169 ) + " 2007-2019 Grigory Filatov;;" + ;
      "eMail: gfilatov@inbox.ru;;" + ;
      "This program is Freeware.;" + ;
      "Copying is allowed!", ;
      "About", "MAIN", , , , ;
      {|| AEval( HMG_GetFormControls( ThisWindow.Name ), ;
          {|ctl| This.&(ctl).Alignment := "Center" } ) } )

#define MB_OK                                 0
#define MB_OKCANCEL                           1
#define MB_ABORTRETRYIGNORE                   2
#define MB_YESNOCANCEL                        3
#define MB_YESNO                              4
#define MB_RETRYCANCEL                        5
#define MB_CANCELTRYCONTINUE                  6
#define MB_ICONHAND                          16
#define MB_ICONQUESTION                      32
#define MB_ICONEXCLAMATION                   48
#define MB_ICONASTERISK                      64
#define MB_USERICON                         128
#define MB_ICONWARNING              MB_ICONEXCLAMATION
#define MB_ICONERROR                MB_ICONHAND
#define MB_ICONINFORMATION          MB_ICONASTERISK
#define MB_ICONSTOP                 MB_ICONHAND
*------------------------------------------------------------------------------*
FUNCTION Testmessage()
*------------------------------------------------------------------------------*
   LOCAL nStyle := MB_OK
   LOCAL title := Demo.Text_1.Value
   LOCAL message := Demo.Edit_1.Value
   LOCAL icon := Demo.RadioGroup_1.Value
   LOCAL btns := Demo.RadioGroup_2.Value

   SWITCH btns
   CASE 2
      nStyle := MB_OKCANCEL
      EXIT
   CASE 3
      nStyle := MB_ABORTRETRYIGNORE
      EXIT
   CASE 4
      nStyle := MB_YESNOCANCEL
      EXIT
   CASE 5
      nStyle := MB_YESNO
      EXIT
   CASE 6
      nStyle := MB_RETRYCANCEL
   END

   SWITCH icon
   CASE 2
      nStyle += MB_ICONERROR
      EXIT
   CASE 3
      nStyle += MB_ICONWARNING
      EXIT
   CASE 4
      nStyle += MB_ICONINFORMATION
      EXIT
   CASE 5
      nStyle += MB_ICONQUESTION
   END

   MessageBoxIndirect( , message, title, nStyle )

RETURN NIL

*------------------------------------------------------------------------------*
FUNCTION ChangeImage()
*------------------------------------------------------------------------------*
   LOCAL icon := Demo.RadioGroup_1.Value

   SWITCH icon
   CASE 1
      Demo.Image_1.Picture := ""
      Demo.Image_1.Refresh
      EXIT
   CASE 2
      Demo.Image_1.Picture := "STOP"
      EXIT
   CASE 3
      Demo.Image_1.Picture := "EXCL"
      EXIT
   CASE 4
      Demo.Image_1.Picture := "INFO"
      EXIT
   CASE 5
      Demo.Image_1.Picture := "QUES"
   END

RETURN NIL
