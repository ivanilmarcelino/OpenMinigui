@echo off

rem Builds Harbour library hbcab.lib.

:OPT
  call ..\..\batch\makelibopt.bat hbcab h %1 %2 %3 %4 %5 %6 %7 %8 %9
  if %MV_EXIT%==Y    goto END
  if %MV_DODONLY%==Y goto CLEANUP

:BUILD
  if exist %MV_BUILD%\hbcab.lib del %MV_BUILD%\hbcab.lib
  %MG_BCC%\bin\bcc32 -c -tWM -O2 -d -6 -OS -I.;%MV_HRB%\include;%MG_ROOT%\include cabinet.c
  %MG_BCC%\bin\bcc32 -c -tWM -O2 -d -6 -OS -I.;%MV_HRB%\include;%MG_ROOT%\include compress.c decompress.c
  %MG_BCC%\bin\tlib %MV_BUILD%\hbcab.lib +cabinet +compress +decompress
  if exist %MV_BUILD%\hbcab.bak del %MV_BUILD%\hbcab.bak

:CLEANUP
  if %MV_DODEL%==N        goto END
  if exist cabinet.obj    del cabinet.obj
  if exist compress.obj   del compress.obj
  if exist decompress.obj del decompress.obj

:END
  call ..\..\batch\makelibend.bat