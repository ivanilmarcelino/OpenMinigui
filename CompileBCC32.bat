::=======================================================================
::Configure apenas estas variáveis de ambiente; o Hbmk2 resolve automaticamente o restante.
::Configure only these environment variables; Hbmk2 automatically resolves the rest.
::Configure solo estas variables de entorno; Hbmk2 resuelve automáticamente el resto.
::Configura solo queste variabili d'ambiente; Hbmk2 risolve automaticamente il resto.

::Defina seu ambiente 
@SET HB_COMPILER=bcc
@Set HB_ARCHITECTURE=w32

::Defina seu PATH DO HARBOUR
@Set MG_HRB=D:\prgplus\git\OpenMinigui\harbour

::Defina PATH DO BORLAND/MINGW
Set MG_BCC=D:\prgplus\Youtube\IDE\Tools\BCC

Set path=%MG_HRB%\BIN;%MG_BCC%\BIN;%path%


::/*FIM*/
::=======================================================================
Call CriaLibsAll.bat
pause
