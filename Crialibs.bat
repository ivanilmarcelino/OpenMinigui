::Under construction;

@echo off
@CHCP 1252 >NUL
CD "%~dp0%"
@SetLocal

::Path do Harbour
Set MG_CMP=MG_CMP
Set MG_HRB=D:\prgplus\Youtube\IDE\Tools\Harbour

::path do Borland
Set MG_BCC=D:\prgplus\Youtube\IDE\Tools\BCC

::Path da Minigui
Set MG_ROOT=%~dp0%
Set MG_LIB=%MG_ROOT%lib
Set MG_XLIB=%MG_ROOT%xlib

set Path=%MG_CMP%\bin;%MG_BCC%\bin;%MG_HRB%\bin;%path%
set Include=%MG_HRB%\include;%MG_CMP%\include;%MG_ROOT%include;%MG_BCC%\include;%MG_BCC%\include\windows\crtl;%MG_BCC%\include\windows\rtl;%MG_BCC%\include\windows\sdk

CD hbmk
CALL hbmk2 hmg.hbp
CALL hbmk2 adordd.hbp
CALL hbmk2 bostaurus.hbp
Call hbmk2 calldll.hbp
Call hbmk2 debugger.hbp
Call hbmk2 dll.hbp
call hbmk2 GraphPlus.hbp
Call hbmk2 hbaes.hbp
Call hbmk2 hbcab.hbp
Call hbmk2 hbComm.hbp
Call hbmk2 HBCrypto.hbp
pause

echo %MG_CMP%
echo %MG_BCC%
echo %MG_ROOT%
pause

:fim
@Set MG_CMP=
@Set MG_BCC=
@Set MG_ROOT=
@EndLocal

