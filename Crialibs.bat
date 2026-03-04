;Under construction;

@echo off
@CHCP 1252 >NUL
CD "%~dp0%"
@SetLocal

;Path do Harbour
Set MG_CMP=MG_CMP
Set MG_HRB=D:\prgplus\Youtube\IDE\Tools\Harbour

;path do Borland
Set MG_BCC=D:\prgplus\Youtube\IDE\Tools\BCC

;Path da Minigui
Set MG_ROOT=%~dp0%
Set MG_LIB=%MG_ROOT%lib
Set MG_XLIB=%MG_ROOT%xlib

set Path=%MG_CMP%\bin;%MG_BCC%\bin;%path%
set Include=%MG_CMP%\include;%MG_ROOT%include;%MG_BCC%\include;%MG_BCC%\include\windows\crtl;%MG_BCC%\include\windows\rtl;%MG_BCC%\include\windows\sdk

echo %MG_ROOT%
echo %mg_lib%
echo %mg_cmp%
echo %mg_hrb%
echo %mg_bcc%
echo %include%
pause

CD SOURCE
CALL MakeAllLibs.bat 
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

