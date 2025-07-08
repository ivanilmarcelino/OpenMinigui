@echo off

rem Builds MiniGui library sevenzip.lib.

:OPT
  call ..\..\batch\makelibopt.bat sevenzip h %1 %2 %3 %4 %5 %6 %7 %8 %9
  if %MV_EXIT%==Y    goto END
  if %MV_DODONLY%==Y goto CLEANUP

:BUILD
  if exist %MV_BUILD%\sevenzip.lib del %MV_BUILD%\sevenzip.lib
  %MV_HRB%\bin\harbour t7zip -n -w3 -es2 -gc0 /i%MV_HRB%\include;%MG_ROOT%\include
  %MG_BCC%\bin\bcc32 -c -tWM -O2 -d -6 -OS -I%MV_HRB%\include;%MG_ROOT%\include t7zip.c
  %MG_BCC%\bin\bcc32 -c -tWM -O2 -d -6 -OS -I.;%MV_HRB%\include;%MG_ROOT%\include -L%MV_HRB%\lib;%MG_BCC%\lib sevenzip.c
  %MG_BCC%\bin\tlib %MV_BUILD%\sevenzip.lib +sevenzip +t7zip
  if exist %MV_BUILD%\sevenzip.bak del %MV_BUILD%\sevenzip.bak

:CLEANUP
  if %MV_DODEL%==N      goto END
  if exist t7zip.c      del t7zip.c
  if exist sevenzip.obj del sevenzip.obj
  if exist t7zip.obj    del t7zip.obj

:END
  call ..\..\batch\makelibend.bat