@echo off

rem Builds Harbour library FreeImage.lib.

:OPT
  call ..\..\batch\makelibopt.bat freeimage h %1 %2 %3 %4 %5 %6 %7 %8 %9
  if %MV_EXIT%==Y    goto END
  if %MV_DODONLY%==Y goto CLEANUP

:BUILD

  SET PATH=c:\borland\bcc58\bin;%PATH%

  if exist FreeImage.dll implib %MV_BUILD%\freeimage.lib FreeImage.dll
  if not exist FreeImage.dll echo FreeImage.dll is missing -  %MV_BUILD%\freeimage.lib is not created!

:CLEANUP
  if %MV_DODEL%==N goto END

:END
  call ..\..\batch\makelibend.bat