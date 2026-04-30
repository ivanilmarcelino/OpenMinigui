@echo off

rem SET DO COMPILADOR MINGW
CALL "D:\Prgplus\Tools\Bat\Mingw.bat"
Set basedir=S:\Mega\Dev\32

SET SOURCE=%BaseDir%\%xMingw32%\MINIGUI\SOURCE

SET HB=%basedir%\%xMingw32%\harbour
SET CONTRIB=%hb%\contrib

SET PATH=%HB%\bin;%basedir%\%xMingw32%\mingw32\bin;%PATH%

rem set include=%include%;%contrib%\hbwin;%contrib%\hbzebra;%contrib%\hbhpdf;;%contrib%\xhb




::=======================================================================
::Configure apenas estas variáveis de ambiente; o Hbmk2 resolve automaticamente o restante.
::Configure only these environment variables; Hbmk2 automatically resolves the rest.
::Configure solo estas variables de entorno; Hbmk2 resuelve automáticamente el resto.
::Configura solo queste variabili d'ambiente; Hbmk2 risolve automaticamente il resto.

::Defina seu ambiente 
@SET HB_COMPILER=mingw
@Set HB_ARCHITECTURE=w32
set include=%include%;%contrib%\hbwin;%contrib%\hbzebra;%contrib%\hbhpdf;;%contrib%\xhb


::Defina seu PATH DO HARBOUR
Set MG_HRB=%hB%
::D:\prgplus\git\lib2026\ide\32\tools\Harbour

::Defina PATH DO BORLAND/MINGW
::dowload: https://github.com/brechtsanders/winlibs_mingw/releases/download/
@Set MG_BCC=%basedir%\%xMingw32%\mingw32
::D:\prgplus\git\lib2026\ide\32\tools\mingw32

::/*FIM*/
::=======================================================================
call CriaLibsAll.bat
pause