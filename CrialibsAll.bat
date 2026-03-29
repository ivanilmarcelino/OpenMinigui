@echo off
@CHCP 1252 >NUL
CD "%~dp0%"
setlocal EnableDelayedExpansion
set LOG=..\LogGeral.log

::PATH OPENMINIGUI
@Set MG_ROOT=%~dp0%
@Set MG_LIB=%MG_ROOT%lib
@Set MG_XLIB=%MG_ROOT%xlib

@set Path=%MG_CMP%\bin;%MG_BCC%\bin;%MG_HRB%\bin;%path%


CD hbmk
echo ===== INICIO DO BUILD ===== > %LOG%
call :RunStep "Compilando HMG" "hbmk2 hmg.hbp"
call :RunStep "Compilando adordd" "hbmk2 adordd.hbp"
call :RunStep "Compilando bostaurus" "hbmk2 bostaurus.hbp"
call :RunStep "Compilando calldll" "hbmk2 calldll.hbp"
call :RunStep "Compilando debugger" "hbmk2 debugger.hbp"
call :RunStep "Compilando dll" "hbmk2 dll.hbp"
call :RunStep "Compilando GraphPlus" "hbmk2 GraphPlus.hbp"
call :RunStep "Compilando hbaes" "hbmk2 hbaes.hbp"
call :RunStep "Compilando hbcab" "hbmk2 hbcab.hbp"
call :RunStep "Compilando hbComm" "hbmk2 hbComm.hbp"
call :RunStep "Compilando HBCrypto" "hbmk2 HBCrypto.hbp"
call :RunStep "Compilando hbfimage" "hbmk2 hbfimage.hbp"
call :RunStep "Compilando hbgdip" "hbmk2 hbgdip.hbp"
call :RunStep "Compilando hblibxlsxwriter" "hbmk2 hblibxlsxwriter.hbp"
call :RunStep "Compilando nulsys" "hbmk2 nulsys.hbp"
call :RunStep "Compilando HbPrinter" "hbmk2 HbPrinter.hbp"
call :RunStep "Compilando hbsqlit3" "hbmk2 hbsqlit3.hbp"
call :RunStep "Compilando hbvpdf" "hbmk2 hbvpdf.hbp"
call :RunStep "Compilando hbxlsxml" "hbmk2 hbxlsxml.hbp"
call :RunStep "Compilando hbxml" "hbmk2 hbxml.hbp"
call :RunStep "Compilando hbzeegrid" "hbmk2 hbzeegrid.hbp"
call :RunStep "Compilando hbziparc" "hbmk2 hbziparc.hbp"
call :RunStep "Compilando easy_sql" "hbmk2 easy_sql.hbp"
call :RunStep "Compilando hmg_hpdf" "hbmk2 hmg_hpdf.hbp"
call :RunStep "Compilando miniprint" "hbmk2 miniprint.hbp"
call :RunStep "Compilando miniprint2" "hbmk2 miniprint2.hbp"
call :RunStep "Compilando pscript" "hbmk2 pscript.hbp"
call :RunStep "Compilando PdfPrinter" "hbmk2 PdfPrinter.hbp"
call :RunStep "Compilando propgrid" "hbmk2 propgrid.hbp"
call :RunStep "Compilando propsheet" "hbmk2 propsheet.hbp"
call :RunStep "Compilando qhtm" "hbmk2 qhtm.hbp"
call :RunStep "Compilando selector" "hbmk2 selector.hbp"
call :RunStep "Compilando SevenZip" "hbmk2 SevenZip.hbp"
call :RunStep "Compilando Shell32" "hbmk2 Shell32.hbp"
call :RunStep "Compilando socket" "hbmk2 socket.hbp"
call :RunStep "Compilando splitter" "hbmk2 splitter.hbp"
call :RunStep "Compilando sqlite3facade" "hbmk2 sqlite3facade.hbp"
call :RunStep "Compilando tmsagent" "hbmk2 tmsagent.hbp"
call :RunStep "Compilando tsbrowse" "hbmk2 tsbrowse.hbp"
call :RunStep "Compilando winreport" "hbmk2 winreport.hbp"


@Set MG_CMP=
@Set MG_BCC=
@Set MG_ROOT=
echo.
echo BUILD FINALIZADO COM SUCESSO
echo ===== BUILD OK ===== >> %LOG%
@EndLocal
goto :eof



::---------------------------------------
:RunStep
set STEP=%~1
set CMD=%~2

echo.
echo [%STEP%]
echo Executando: %CMD%

echo [%STEP%] >> %LOG%
echo CMD: %CMD% >> %LOG%

call %CMD% >> %LOG% 2>&1

echo *********************************************************************************** >> %LOG%
echo /////////////////////////////////////////////////////////////////////////////////// >> %LOG%
echo *********************************************************************************** >> %LOG%

set RET=%ERRORLEVEL%

if NOT "%RET%"=="0" (
    echo ERRO no passo: %STEP%
    echo Codigo: %RET%

    echo ERRO no passo: %STEP% >> %LOG%
    echo Codigo: %RET% >> %LOG%

    goto error
)

exit /b 0
::---------------------------------------
:error
echo.
echo ===== BUILD FALHOU =====
echo Veja o log: %LOG%
exit /b %RET%

