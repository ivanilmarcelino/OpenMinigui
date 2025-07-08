
#include "minigui.ch"
#include "i_hmgcompat.ch"   // For GetFormNameByIndex()

//-----------------------------------------------------------------------------

procedure Main()

InstallEventHandler("KeyTrap")

define window oWndHelpTest         ;
       at 0,0 width 415 height 200 ;
       title "Help Test"           ;
       main

   @  10,  10 label oLabell value "Help Test from PDF File" autosize bold

   @  40,  10 label oLabel2 value "Press F1 key" autosize clientedge

   @  80,  10 button oBtnDlg1     ;
              caption "Dialog 1"  ;
              width 120 height 25 ;
              action {|| Dialog1()}

   @  80, 140 button oBtnDlg2     ;
              caption "Dialog 2"  ;
              width 120 height 25 ;
              action {|| Dialog2()}

   @  80, 270 button oBtnDirect      ;
              caption "Restrictions" ;
              width 120 height 25    ;
              action {|| ShowHelp("Restrictions")}

   @ 115, 140 button oBtnQuit      ;
              caption "Quit"       ;
              width 120 height 25  ;
              action {|| Quit()}

end window

oWndHelpTest.Center()
oWndHelpTest.Activate()

Return

//-----------------------------------------------------------------------------

function Dialog1 ()

define window oWndDlg1             ;
       at 0,0 width 200 height 155 ;
       title "Help Calling"        ;
       modal

   @  10,  10 label oLabell value "HelpTest calling topic" autosize bold

   @  40,  10 label oLabel2 value "Press F1 key" autosize clientedge

   @  80,  40 button oBtnQuit      ;
              caption "Quit"       ;
              width 120 height 25  ;
              action {|| oWndDlg1.Release()}

end window

oWndDlg1.Center()
oWndDlg1.Activate()

Return (NIL)

//-----------------------------------------------------------------------------

function Dialog2 ()

define window oWndDlg2             ;
       at 0,0 width 350 height 175 ;
       title "Help Method"         ;
       modal

   @  10,  10 label oLabell value "HelpTest calling method" autosize bold

   @  40,  10 label oLabel2 value "Press F1 key" autosize clientedge

   @  80,  10 button oBtnQuit      ;
              caption "Quit"       ;
              width 120 height 25  ;
              action {|| oWndDlg2.Release()}

end window

oWndDlg2.Center()
oWndDlg2.Activate()

Return (NIL)

//-----------------------------------------------------------------------------

function Quit ()
local z
local aProcess

if !MsgYesNo("Do you wish to quit the Application ?", "Help Test")
   Return (NIL)
endif

aProcess := GetProcessesNT()
for z = 1 to Len(aProcess)
   if (ValType(aProcess[z]) == "C" .and. "SUMATRAPDF.EXE" $ Upper(aProcess[z]))
      KillProcess(aProcess[z -1])
   endif
next z
ReleaseAllWindows()

Return (NIL)

//-----------------------------------------------------------------------------

function ShowHelp (cTopic)
local cParams

if cTopic == NIL
   cTopic := ThisWindow.Title
endif
cTopic  := iif(cTopic == NIL, "", '"'+ cTopic +'"')
cParams := '-esc-to-exit -named-dest '+ cTopic +' -reuse-instance HELPTEST.PDF'
ShellExecute(, "OPEN", "SumatraPDF\SumatraPDF.exe", cParams,, 1)

Return (NIL)




//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//  KEY HANDLER
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------

function KeyTrap (hWnd, nMsg, wParam, lParam)
local cFormName

if nMsg == 6 // WM_ACTIVATE
   cFormName := GetFormNameByIndex(GetFormIndexByHandle(hWnd))
   _DefineHotKey(cFormName,, VK_F1, {|| ShowHelp()})
endif

Return (NIL)
