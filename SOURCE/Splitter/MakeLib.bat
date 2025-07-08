@echo off

rem Builds MiniGui library splitter.lib.

:OPT
  call ..\..\batch\makelibopt.bat Splitter m %1 %2 %3 %4 %5 %6 %7 %8 %9
  if %MV_EXIT%==Y    goto END
  if %MV_DODONLY%==Y goto CLEANUP

:BUILD
  if exist %MV_BUILD%\splitter.lib del %MV_BUILD%\splitter.lib
  %MV_HRB%\bin\harbour h_splitter -n -w3 -es2 -gc0 /i%MV_HRB%\include;%MG_ROOT%\include
  %MG_BCC%\bin\bcc32 -c -tWM -O2 -d -6 -OS -I%MV_HRB%\include;%MG_ROOT%\include h_splitter.c
  %MG_BCC%\bin\bcc32 -c -tWM -O2 -d -6 -OS -Ov -Oi -Oc -I.;%MV_HRB%\include;%MG_ROOT%\include -L%MV_HRB%\lib;%MG_BCC%\lib c_splitter.c
  %MG_BCC%\bin\tlib %MV_BUILD%\splitter.lib +c_splitter	 +h_splitter
  if exist %MV_BUILD%\splitter.bak del %MV_BUILD%\splitter.bak

:CLEANUP
  if %MV_DODEL%==N   goto END
  if exist h_splitter.c   del h_splitter.c
  if exist c_splitter.obj del c_splitter.obj
  if exist h_splitter.obj del h_splitter.obj

:END
  call ..\..\batch\makelibend.bat