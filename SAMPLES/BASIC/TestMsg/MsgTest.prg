REQUEST DBFCDX

#include "MiniGui.ch"

//-----------------------------------------------------------------------------

procedure Main ()
local cText  := "This function detects the type of variable passed and displays "+ ;
                "it's content either in a message box, an editbox or a grid "+     ;
                "depending on what it found."+ CRLF + CRLF +                       ;
                "   String expression             => Message box"+ CRLF +          ;
                "   String expression (255+ Char) => Edit box"+ CRLF +             ;
                "   Memo Text expression          => Edit box"+ CRLF +             ;
                "   Numeric expression            => Message box"+ CRLF +          ;
                "   Logic expression              => Message box"+ CRLF +          ;
                "   Date expression               => Message box"+ CRLF +          ;
                "   Time String expression        => Message box"+ CRLF +          ;
                "   NIL expression                => Message box"+ CRLF +          ;
                "   1D Arrays                     => Grid box"+ CRLF +             ;
                "   2D Arrays                     => Grid box"+ CRLF +             ;
                "   Hash Arrays                   => Grid box"+ CRLF +             ;
                "   Database                      => Browse box"+ CRLF + CRLF +    ;
                "Syntax: MsgTest(xVariable, cTitle)"

set date format "dd/mm/yyyy"
set century ON
set epoch to Year(Date()) -25

// dbUseArea( [<lNewArea>], [<cDriver>], <cName>, [<xcAlias>], [<lShared>], [<lReadonly>] )
dbUseArea(.T., "DBFCDX", "TEST", , .T., .F.)

define window oWndTest      ;
       at 0, 0              ;
       width 400 height 400 ;
       title "MsgTest()"    ;
       main nosize

   @  10,  10 editbox oEdtBox           ;
              width 380 height 275      ;
              value cText               ;
              font "courier new" size 8 ;
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
local cNILStr
local cString    := "My name is Gilbert Vaillancourt       "
local nValue     := 2025
local dDate      := Date()
local tTime      := Time()
local lLogic     := .F.
local aArray     := {"Madame Blue", "Crystal Ball", "Mr. Roboto", "Sweet Mademoiselle", "Paradise Theatre"}
local aMatrix    := {{"Madame Blue", 1, .F.}, {"Crystal Ball", 2, .T.}, {"Mr. Roboto", 3, .T.}, {"Sweet Mademoiselle", 4, .T.}, {"Paradise Theatre", 5, .F.}}
local aHash      := {"Madame Blue" => 1, "Crystal Ball" => 2, "Mr. Roboto" => 3, "Sweet Mademoiselle" => 4, "Paradise Theatre" => 5}

MsgTest(cNILStr, "cNILStr")
MsgTest(cString, "cString")
MsgTest(AllTrim(cString), "AllTrim(cString)")
MsgTest(nValue, "nValue")
MsgTest(dDate, "dDate")
MsgTest(tTime, "tTime")
MsgTest(lLogic, "lLogic")
MsgTest(aArray, "aArray")
MsgTest(aMatrix, "aMatrix")
MsgTest(aHash, "aHash")
MsgTest("TEST")

Return (NIL)
