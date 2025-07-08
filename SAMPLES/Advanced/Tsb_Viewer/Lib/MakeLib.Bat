@echo off
rem Builds Harbour library TsbViewer.lib.

:SET
  set MG_BCC=C:\Borland\BCC58
  set MG_ROOT=C:\MiniGui
  set MG_HRB=%MG_ROOT%\Harbour
  set MV_HRB=%MG_HRB%

:BUILD
  if exist TsbViewer.lib del TsbViewer.lib

  %MV_HRB%\bin\harbour TsbViewer.prg -n       -es2 -i%MV_HRB%\include;%MG_ROOT%\include
  %MV_HRB%\bin\harbour TsbViewLang.prg -n     -es2 -i%MV_HRB%\include;%MG_ROOT%\include
  %MV_HRB%\bin\harbour TsbViewMenu.prg -n     -es2 -i%MV_HRB%\include;%MG_ROOT%\include
  %MV_HRB%\bin\harbour TsbViewMisc.prg -n     -es2 -i%MV_HRB%\include;%MG_ROOT%\include
  %MG_BCC%\bin\bcc32 -c -O2 -tWM -d -6 -OS -w -I%MV_HRB%\include;%MG_BCC%\include;%MG_ROOT%\include -L%MV_HRB%\lib;%MG_BCC%\lib TsbViewer.c TsbViewLang.c TsbViewMenu.c TsbViewMisc.c
  %MG_BCC%\bin\tlib TsbViewer.lib +TsbViewer.obj +TsbViewLang.obj +TsbViewMenu.obj +TsbViewMisc.obj

:CLEANUP
  if exist TsbViewer.c     del TsbViewer.c
  if exist TsbViewer.obj   del TsbViewer.obj
  if exist TsbViewLang.c   del TsbViewLang.c
  if exist TsbViewLang.obj del TsbViewLang.obj
  if exist TsbViewMenu.c   del TsbViewMenu.c
  if exist TsbViewMenu.obj del TsbViewMenu.obj
  if exist TsbViewMisc.c   del TsbViewMisc.c
  if exist TsbViewMisc.obj del TsbViewMisc.obj

:COPYLIB
  if exist TsbViewer.lib move TsbViewer.lib %MG_ROOT%\lib\TsbViewer.lib

:END