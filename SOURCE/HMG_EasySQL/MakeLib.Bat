@echo off

rem Builds MiniGui library easy_sql.lib.

:OPT
  call ..\..\batch\makelibopt.bat easy_sql m %1 %2 %3 %4 %5 %6 %7 %8 %9
  if %MV_EXIT%==Y    goto END
  if %MV_DODONLY%==Y goto CLEANUP

:BUILD
  if exist %MV_BUILD%\easy_sql.lib del %MV_BUILD%\easy_sql.lib
  %MV_HRB%\bin\harbour core.prg -n -w3 -es2 -i%MV_HRB%\include;%MG_ROOT%\include
  %MG_BCC%\bin\bcc32 -c -O2 -tWM -d -6 -OS -I%MV_HRB%\include;%MG_BCC%\include -L%MV_HRB%\lib;%MG_BCC%\lib core.c
  %MG_BCC%\bin\tlib %MV_BUILD%\easy_sql.lib +core.obj
  if exist %MV_BUILD%\easy_sql.bak del %MV_BUILD%\easy_sql.bak

:CLEANUP
  if %MV_DODEL%==N      goto END
  if exist core.c   del core.c
  if exist core.obj del core.obj

:END
  call ..\..\batch\makelibend.bat