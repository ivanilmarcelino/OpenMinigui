Echo off
::=======================================================================
::Configure apenas estas variáveis de ambiente; o Hbmk2 resolve automaticamente o restante.
::Configure only these environment variables; Hbmk2 automatically resolves the rest.
::Configure solo estas variables de entorno; Hbmk2 resuelve automáticamente el resto.
::Configura solo queste variabili d'ambiente; Hbmk2 risolve automaticamente il resto.

::Defina seu ambiente 
@SET HB_COMPILER=mingw
@Set HB_ARCHITECTURE=w32

::Defina seu PATH DO HARBOUR
Set MG_HRB=D:\prgplus\Youtube\IDE\Tools\Harbour

::Defina PATH DO BORLAND/MINGW
::dowload: https://github.com/brechtsanders/winlibs_mingw/releases/download/
@Set MG_BCC=D:\prgplus\git\lib2026\ide\32\tools\mingw32

::/*FIM*/
::=======================================================================
call CriaLibsAll.bat
