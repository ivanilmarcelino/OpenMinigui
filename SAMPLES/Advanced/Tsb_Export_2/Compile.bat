call ..\..\..\Batch\Compile.Bat demo %1 /nl %2 %3 %4 %5 %6 %7 %8 %9
call ..\..\..\Batch\Compile.Bat TsbExport %1 /nl %2 %3 %4 %5 %6 %7 %8 %9
call ..\..\..\Batch\Compile.Bat TsbCell %1 /nl %2 %3 %4 %5 %6 %7 %8 %9
call ..\..\..\Batch\Compile.Bat Tsb7xlsOle %1 /nl %2 %3 %4 %5 %6 %7 %8 %9
call ..\..\..\Batch\Compile.Bat Tsb7DocOle %1 /nl %2 %3 %4 %5 %6 %7 %8 %9
call ..\..\..\Batch\Compile.Bat Tsb7odsCalc %1 /nl %2 %3 %4 %5 %6 %7 %8 %9
call ..\..\..\Batch\Compile.Bat Tsb7xml %1 /nl %2 %3 %4 %5 %6 %7 %8 %9
call ..\..\..\Batch\Compile.Bat Tsb7xlsTuning %1 /nl %2 %3 %4 %5 %6 %7 %8 %9
call ..\..\..\Batch\Compile.Bat Secondary_funct %1 /nl %2 %3 %4 %5 %6 %7 %8 %9
call ..\..\..\Batch\Compile.Bat WaitThreadIco %1 /nl %2 %3 %4 %5 %6 %7 %8 %9

call ..\..\..\Batch\Compile.Bat demo %1 /lo /b TsbExport /b TsbCell /b Tsb7xlsOle /b Tsb7DocOle /b Tsb7odsCalc /b Tsb7xml /b Tsb7xlsTuning /b Secondary_funct /b WaitThreadIco /l hbxlsxml /mt %2 %3 %4 %5 %6 %7 %8 %9

call ..\..\..\Batch\Compile.Bat demo %1 /do %2 %3 %4 %5 %6 %7 %8 %9
call ..\..\..\Batch\Compile.Bat TsbExport %1 /do %2 %3 %4 %5 %6 %7 %8 %9
call ..\..\..\Batch\Compile.Bat TsbCell %1 /do %2 %3 %4 %5 %6 %7 %8 %9
call ..\..\..\Batch\Compile.Bat Tsb7xlsOle %1 /do %2 %3 %4 %5 %6 %7 %8 %9
call ..\..\..\Batch\Compile.Bat Tsb7DocOle %1 /do %2 %3 %4 %5 %6 %7 %8 %9
call ..\..\..\Batch\Compile.Bat Tsb7odsCalc %1 /do %2 %3 %4 %5 %6 %7 %8 %9
call ..\..\..\Batch\Compile.Bat Tsb7xml %1 /do %2 %3 %4 %5 %6 %7 %8 %9
call ..\..\..\Batch\Compile.Bat Tsb7xlsTuning %1 /do %2 %3 %4 %5 %6 %7 %8 %9
call ..\..\..\Batch\Compile.Bat Secondary_funct %1 /do %2 %3 %4 %5 %6 %7 %8 %9
call ..\..\..\Batch\Compile.Bat WaitThreadIco %1 /do %2 %3 %4 %5 %6 %7 %8 %9