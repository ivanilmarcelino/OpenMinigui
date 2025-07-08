call ..\..\..\Batch\Compile.Bat TstErrH2 %1 /nl %2 %3 %4 %5 %6 %7 %8 %9
call ..\..\..\Batch\Compile.Bat SErrHnd2 %1 /nl %2 %3 %4 %5 %6 %7 %8 %9
call ..\..\..\Batch\Compile.Bat HL_LibC7 %1 /nl %2 %3 %4 %5 %6 %7 %8 %9

call ..\..\..\Batch\Compile.Bat TstErrH2 %1 /lo /b SErrHnd2 /b HL_LibC7 %2 %3 %4 %5 %6 %7 %8 %9

call ..\..\..\Batch\Compile.Bat TstErrH2 %1 /do %2 %3 %4 %5 %6 %7 %8 %9
call ..\..\..\Batch\Compile.Bat SErrHnd2 %1 /do %2 %3 %4 %5 %6 %7 %8 %9
call ..\..\..\Batch\Compile.Bat HL_LibC7 %1 /do %2 %3 %4 %5 %6 %7 %8 %9
