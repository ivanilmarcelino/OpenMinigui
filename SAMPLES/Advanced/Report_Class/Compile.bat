call ..\..\..\batch\compile.bat test       /nl %1 %2 %3 %4 %5 %6 %7 %8 %9
call ..\..\..\batch\compile.bat REPORT     /nl %1 %2 %3 %4 %5 %6 %7 %8 %9
call ..\..\..\batch\compile.bat COLUMN     /nl %1 %2 %3 %4 %5 %6 %7 %8 %9
call ..\..\..\batch\compile.bat gdevposout /nl %1 %2 %3 %4 %5 %6 %7 %8 %9
call ..\..\..\batch\compile.bat gHBPRINT   /nl %1 %2 %3 %4 %5 %6 %7 %8 %9
call ..\..\..\batch\compile.bat gPDFPRINT  /nl %1 %2 %3 %4 %5 %6 %7 %8 %9
call ..\..\..\batch\compile.bat gHTMPRINT  /nl %1 %2 %3 %4 %5 %6 %7 %8 %9
call ..\..\..\batch\compile.bat gXLSPRINT  /nl %1 %2 %3 %4 %5 %6 %7 %8 %9
call ..\..\..\batch\compile.bat gHtmAbsPosPRINT  /nl %1 %2 %3 %4 %5 %6 %7 %8 %9

call ..\..\..\batch\compile.bat test /lo /b REPORT /b COLUMN /b gdevposout /b gHBPRINT /b gPDFPRINT /b gHTMPRINT /b gXLSPRINT /b gHtmAbsPosPRINT /l hmg_hpdf %1 %2 %3 %4 %5 %6 %7 %8 %9

call ..\..\..\batch\compile.bat test       /do %1 %2 %3 %4 %5 %6 %7 %8 %9
call ..\..\..\batch\compile.bat REPORT     /do %1 %2 %3 %4 %5 %6 %7 %8 %9
call ..\..\..\batch\compile.bat COLUMN     /do %1 %2 %3 %4 %5 %6 %7 %8 %9
call ..\..\..\batch\compile.bat gdevposout /do %1 %2 %3 %4 %5 %6 %7 %8 %9
call ..\..\..\batch\compile.bat gHBPRINT   /do %1 %2 %3 %4 %5 %6 %7 %8 %9
call ..\..\..\batch\compile.bat gPDFPRINT  /do %1 %2 %3 %4 %5 %6 %7 %8 %9
call ..\..\..\batch\compile.bat gHTMPRINT  /do %1 %2 %3 %4 %5 %6 %7 %8 %9
call ..\..\..\batch\compile.bat gXLSPRINT  /do %1 %2 %3 %4 %5 %6 %7 %8 %9
call ..\..\..\batch\compile.bat gHtmAbsPosPRINT  /do %1 %2 %3 %4 %5 %6 %7 %8 %9
