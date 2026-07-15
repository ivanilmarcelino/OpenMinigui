@echo off

rem Builds Harbour library HbPrinter.lib.

:OPT
  call ..\..\batch\makelibopt.bat HbPrinter h %1 %2 %3 %4 %5 %6 %7 %8 %9
  if %MV_EXIT%==Y    goto END
  if %MV_DODONLY%==Y goto CLEANUP

:BUILD
  if exist %MV_BUILD%\hbprinter.lib del %MV_BUILD%\hbprinter.lib
  %MV_HRB%\bin\harbour h_winprint.prg -n -w3 -es2 -i%MV_HRB%\include;%MG_ROOT%\include
  %MG_BCC%\bin\bcc32 -c -O2 -tWM -d -6 -OS -I%MV_HRB%\include;%MG_BCC%\include;%MG_ROOT%\include -L%MV_HRB%\lib;%MG_BCC%\lib h_winprint.c c_winprint.c
  %MG_BCC%\bin\tlib %MV_BUILD%\hbprinter.lib +h_winprint.obj +c_winprint.obj
  if exist %MV_BUILD%\hbprinter.bak del %MV_BUILD%\hbprinter.bak

:CLEANUP
  if %MV_DODEL%==N      goto END
  if exist h_winprint.c   del h_winprint.c
  if exist h_winprint.obj del h_winprint.obj
  if exist c_winprint.obj del c_winprint.obj

:END
  call ..\..\batch\makelibend.bat