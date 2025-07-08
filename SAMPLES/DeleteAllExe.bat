:: Delete All Samples
:: 
::
@ echo off


ren Advanced\7-Zip\7za.exe 7za.ex_
ren Advanced\ReStart\restart.exe restart.ex_

REM Add By Pierpaolo May 2018
ren Advanced\ProcInfo\hungtest.exe hungtest.ex_
ren Advanced\NirCmdDll\nircmd.exe nircmd.ex_


del basic\*.exe /s/q
del Advanced\*.exe /s/q


ren Advanced\7-Zip\7za.ex_ 7za.exe
ren Advanced\ReStart\restart.ex_ restart.exe

REM Add By Pierpaolo May 2018
ren Advanced\ProcInfo\hungtest.ex_ hungtest.exe
ren Advanced\NirCmdDll\nircmd.ex_ nircmd.exe


:END
