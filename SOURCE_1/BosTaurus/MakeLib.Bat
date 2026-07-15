@echo off

rem Builds Harbour library BosTaurus.lib.

:OPT
  call ..\..\batch\makelibopt.bat BosTaurus h %1 %2 %3 %4 %5 %6 %7 %8 %9
  if %MV_EXIT%==Y    goto END
  if %MV_DODONLY%==Y goto CLEANUP

:BUILD
  if exist %MV_BUILD%\BosTaurus.lib del %MV_BUILD%\BosTaurus.lib
  %MV_HRB%\bin\harbour h_BosTaurus.prg -n -w3 -es2 -i%MV_HRB%\include;%MG_ROOT%\include
  %MG_BCC%\bin\bcc32 -c -O2 -tWM -d -6 -OS -w -I%MV_HRB%\include;%MG_BCC%\include;%MG_ROOT%\include -L%MV_HRB%\lib;%MG_BCC%\lib h_BosTaurus.c c_BosTaurus.c
  %MG_BCC%\bin\tlib %MV_BUILD%\BosTaurus.lib +h_BosTaurus.obj +c_BosTaurus.obj
  if exist %MV_BUILD%\BosTaurus.bak del %MV_BUILD%\BosTaurus.bak

:CLEANUP
  if %MV_DODEL%==N      goto END
  if exist h_BosTaurus.c   del h_BosTaurus.c
  if exist h_BosTaurus.obj del h_BosTaurus.obj
  if exist c_BosTaurus.obj del c_BosTaurus.obj

:END
  call ..\..\batch\makelibend.bat
