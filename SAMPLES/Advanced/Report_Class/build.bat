@echo off

if .==%1. goto incremental

:full
call ..\..\..\BATCH\hbmk2.bat /e /r test.hbp
goto logit

:incremental
call ..\..\..\BATCH\hbmk2.bat /e test.hbp

:logit
rem grep -i "exe" build.log >yyy.log
rem grep -i "warning " build.log >>yyy.log
rem grep -i "warning:" build.log >>yyy.log
rem grep -i "error:" build.log >>yyy.log
rem grep -i "error " build.log >>yyy.log
rem grep -i "\.ch" build.log >>yyy.log

rem grep -v " not used" yyy.log >xxx.log
rem grep -v " never used" xxx.log
rem time /t
echo Done.

