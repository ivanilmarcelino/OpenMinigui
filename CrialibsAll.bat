@echo off
@CHCP 1252 >NUL
CD "%~dp0%"
setlocal EnableDelayedExpansion

::PATH OPENMINIGUI
@Set MG_ROOT=%~dp0%
@Set MG_LIB=%MG_ROOT%lib
@Set MG_XLIB=%MG_ROOT%xlib
@Set xcmd=-exitstr -gc3 -info -trace -v -jobs=%NUMBER_OF_PROCESSORS% 

set LOG=%MG_ROOT%LogGeral.log



@set Path=%MG_BCC%\bin;%MG_HRB%\bin;%path%
@Set include=%MG_ROOT%include

echo Definição paths: > %LOG%
echo MG_ROOT=%MG_ROOT% >> %LOG%
echo MG_LIB=%MG_LIB% >> %LOG%
echo MG_HRB=%MG_HRB% >> %LOG%
echo MG_BCC=%MG_BCC% >> %LOG%

ECHO HB_COMPILER=%HB_COMPILER% >> %LOG%
ECHO HB_ARCHITECTURE=%HB_ARCHITECTURE% >> %LOG%


CD hbmk
echo ===== INICIO DO BUILD ===== >> %LOG%
call :RunStep "Compilando HMG" "hbmk2 %xcmd% hmg.hbp"
call :RunStep "Compilando adordd" "hbmk2 %xcmd% adordd.hbp"
call :RunStep "Compilando bostaurus" "hbmk2 %xcmd% bostaurus.hbp"
call :RunStep "Compilando calldll" "hbmk2 %xcmd% calldll.hbp"
call :RunStep "Compilando debugger" "hbmk2 %xcmd% debugger.hbp"
call :RunStep "Compilando dll" "hbmk2 %xcmd% dll.hbp"
call :RunStep "Compilando GraphPlus" "hbmk2 %xcmd% GraphPlus.hbp"
call :RunStep "Compilando hbaes" "hbmk2 %xcmd% hbaes.hbp"
call :RunStep "Compilando hbcab" "hbmk2 %xcmd% hbcab.hbp"
call :RunStep "Compilando hbComm" "hbmk2 %xcmd% hbComm.hbp"
call :RunStep "Compilando HBCrypto" "hbmk2 %xcmd% HBCrypto.hbp"
call :RunStep "Compilando hbfimage" "hbmk2 %xcmd% hbfimage.hbp"
call :RunStep "Compilando hbgdip" "hbmk2 %xcmd% hbgdip.hbp"
call :RunStep "Compilando hblibxlsxwriter" "hbmk2 %xcmd% hblibxlsxwriter.hbp"
call :RunStep "Compilando nulsys" "hbmk2 %xcmd% nulsys.hbp"
call :RunStep "Compilando HbPrinter" "hbmk2 %xcmd% HbPrinter.hbp"
call :RunStep "Compilando hbsqlit3" "hbmk2 %xcmd% hbsqlit3.hbp"
call :RunStep "Compilando hbvpdf" "hbmk2 %xcmd% hbvpdf.hbp"
call :RunStep "Compilando hbxlsxml" "hbmk2 %xcmd% hbxlsxml.hbp"
call :RunStep "Compilando hbxml" "hbmk2 %xcmd% hbxml.hbp"
call :RunStep "Compilando hbzeegrid" "hbmk2 %xcmd% hbzeegrid.hbp"
call :RunStep "Compilando hbziparc" "hbmk2 %xcmd% hbziparc.hbp"
call :RunStep "Compilando easy_sql" "hbmk2 %xcmd% easy_sql.hbp"
call :RunStep "Compilando hmg_hpdf" "hbmk2 %xcmd% hmg_hpdf.hbp"
call :RunStep "Compilando miniprint" "hbmk2 %xcmd% miniprint.hbp"
call :RunStep "Compilando miniprint2" "hbmk2 %xcmd% miniprint2.hbp"
call :RunStep "Compilando pscript" "hbmk2 %xcmd% pscript.hbp"
call :RunStep "Compilando PdfPrinter" "hbmk2 %xcmd% PdfPrinter.hbp"
call :RunStep "Compilando propgrid" "hbmk2 %xcmd% propgrid.hbp"
call :RunStep "Compilando propsheet" "hbmk2 %xcmd% propsheet.hbp"
call :RunStep "Compilando qhtm" "hbmk2 %xcmd% qhtm.hbp"
call :RunStep "Compilando selector" "hbmk2 %xcmd% selector.hbp"
call :RunStep "Compilando SevenZip" "hbmk2 %xcmd% SevenZip.hbp"
rem call :RunStep "Compilando Shell32" "hbmk2 %xcmd% Shell32.hbp"
call :RunStep "Compilando socket" "hbmk2 %xcmd% socket.hbp"
call :RunStep "Compilando splitter" "hbmk2 %xcmd% splitter.hbp"
call :RunStep "Compilando sqlite3facade" "hbmk2 %xcmd% sqlite3facade.hbp"
call :RunStep "Compilando tmsagent" "hbmk2 %xcmd% tmsagent.hbp"
call :RunStep "Compilando tsbrowse" "hbmk2 %xcmd% tsbrowse.hbp"
call :RunStep "Compilando winreport" "hbmk2 %xcmd% winreport.hbp"


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
rem set CMD=%~2
set "CMD=%~2"

echo.
echo [%STEP%]
echo Executando: %CMD%

echo [%STEP%] >> %LOG%
echo CMD: %CMD% >> %LOG%

rem call %CMD% >> %LOG% 2>&1
cmd /c %CMD% >> "%LOG%" 2>&1

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



