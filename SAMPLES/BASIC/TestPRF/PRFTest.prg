
#include "MiniGui.ch"
#include "WinPrint.ch"

//-----------------------------------------------------------------------------

procedure Main ()
local cText  := "Once upon a time there was a nice function called PRINTFILERAW(), "+          ;
                "which was very useful when needed to print a basic text file without "+       ;
                "too much coding. This function was using the printer RAW mode for printing "+ ;
                "documents. But it seems that newer version of printers, especially laser "+   ;
                "printers, no longer support this mode. "+ CRLF +                              ;
                "So I decided to develop a replacement for this function. Using the "+         ;
                "Harbour TFileRead and HBPrinter classes, I was able to do something fast "+   ;
                "and easy to use, like PRINTFILERAW(). I even added some interesting "+        ;
                "options, such as paper size, paper orientation, and font size. This new "+    ;
                "version will word wrapping in case of a long, unformatted string. "+          ;
                "It will also recalculate maximum columns (characters) that can be printed "+  ;
                "depending on the paper size, paper orientation, and font size."+ CRLF + CRLF +;
                "I struggled a bit with how to call this function, and eventually came up "+   ;
                "with PRINTRAWFILE(). Isn't that too confusing ?"+ CRLF + CRLF +               ;
                "Syntax: PrintRawFile(cPrinter, cFileName, [nPaper], [nOrient], [nFSize])"

set date format "dd/mm/yyyy"
set century ON
set epoch to Year(Date()) -25

define window oWndTest        ;
       at 0, 0                ;
       width 420 height 400   ;
       title "PrintRawFile()" ;
       main nosize

   @  10,  10 editbox oEdtBox           ;
              width 400 height 275      ;
              value cText               ;
              backcolor {250,250,230}   ;
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

   _DefineHotKey(this.Name,, VK_ESCAPE, {|| oWndTest.Release()})

end window

oWndTest.Center()
oWndTest.Activate()

Return

//-----------------------------------------------------------------------------

function Test ()
local cPrinter  := GetPrinter()
local cTextFile := "TL_PRF.PRG"

MsgInfo("Printing PORTRAIT at 9pts")
PrintRawFile(cPrinter, cTextFile, DMPAPER_LETTER, DMORIENT_PORTRAIT, 9)
MsgInfo("Printing LANDSCAPE at 12pts")
PrintRawFile(cPrinter, cTextFile, DMPAPER_LETTER, DMORIENT_LANDSCAPE, 12)

Return (NIL)
