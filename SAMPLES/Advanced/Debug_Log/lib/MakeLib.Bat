@echo off
rem Builds Harbour library hmgaddon.lib.

:SET
  set MG_BCC=C:\Borland\BCC58
  set MG_ROOT=C:\MiniGui
  set MG_HRB=%MG_ROOT%\Harbour
  set MV_HRB=%MG_HRB%

:BUILD
  if exist hmgaddon.lib del hmgaddon.lib

  %MV_HRB%\bin\harbour alertdebug.prg -n -w2 -es2 -i%MV_HRB%\include;%MG_ROOT%\include
  %MV_HRB%\bin\harbour util_font.prg -n -w2 -es2 -i%MV_HRB%\include;%MG_ROOT%\include
  %MV_HRB%\bin\harbour winprocess.prg -n -w2 -es2 -i%MV_HRB%\include;%MG_ROOT%\include
  %MG_BCC%\bin\bcc32 -c -O2 -tWM -d -6 -OS -w -I%MV_HRB%\include;%MG_BCC%\include;%MG_ROOT%\include -L%MV_HRB%\lib;%MG_BCC%\lib alertdebug.c util_font.c winprocess.c
  %MG_BCC%\bin\tlib hmgaddon.lib +alertdebug.obj +util_font.obj +winprocess.obj

:CLEANUP
  if exist alertdebug.c   del alertdebug.c
  if exist alertdebug.obj del alertdebug.obj
  if exist util_font.c    del util_font.c
  if exist util_font.obj  del util_font.obj
  if exist winprocess.c   del winprocess.c
  if exist winprocess.obj del winprocess.obj
  goto :end

:COPYLIB
  if exist hmgaddon.lib move hmgaddon.lib %MG_ROOT%\lib\hmgaddon.lib

:END