@echo off

rem Builds Harbour library HbCrypto.lib.

:OPT
  call ..\..\batch\makelibopt.bat hbcrypto h %1 %2 %3 %4 %5 %6 %7 %8 %9
  if %MV_EXIT%==Y    goto END
  if %MV_DODONLY%==Y goto CLEANUP
  if %MV_USEXHRB%==N goto BUILD
  echo HbCrypto.lib is not compatible with xHarbour.
  goto END

:BUILD
  if exist %MV_BUILD%\%MV_LIBNAME%.lib del %MV_BUILD%\%MV_LIBNAME%.lib
  %MV_HRB%\bin\harbour scryptcf.prg -n1 -w3 -es2 -i%MV_HRB%\include;%MG_ROOT%\include
  %MG_BCC%\bin\bcc32 -c -O2 -tW -tWM -d -a8 -OS -I%MV_HRB%\include;%MG_BCC%\include -L%MV_HRB%\lib;%MG_BCC%\lib scryptcf.c
  %MG_BCC%\bin\bcc32 -c -O2 -tW -tWM -d -a8 -OS -I%MV_HRB%\include;%MG_BCC%\include -L%MV_HRB%\lib;%MG_BCC%\lib bcrypt.c
  %MG_BCC%\bin\bcc32 -c -O2 -tW -tWM -d -a8 -OS -I%MV_HRB%\include;%MG_BCC%\include -L%MV_HRB%\lib;%MG_BCC%\lib blake2s.c
  %MG_BCC%\bin\bcc32 -c -O2 -tW -tWM -d -a8 -OS -I%MV_HRB%\include;%MG_BCC%\include;./3rd/ed25519;./3rd/scrypt -L%MV_HRB%\lib;%MG_BCC%\lib ed25519.c
  %MG_BCC%\bin\bcc32 -c -O2 -tW -tWM -d -a8 -OS -I%MV_HRB%\include;%MG_BCC%\include -L%MV_HRB%\lib;%MG_BCC%\lib sha3.c
  %MG_BCC%\bin\bcc32 -c -O2 -tW -tWM -d -a8 -OS -I%MV_HRB%\include;%MG_BCC%\include;./3rd/ed25519;./3rd/scrypt -L%MV_HRB%\lib;%MG_BCC%\lib pbkdf2.c 
  %MG_BCC%\bin\bcc32 -c -O2 -tW -tWM -d -a8 -OS -I%MV_HRB%\include;%MG_BCC%\include;./3rd/ed25519;./3rd/scrypt -L%MV_HRB%\lib;%MG_BCC%\lib scrypt.c 
  %MG_BCC%\bin\bcc32 -c -O2 -tW -tWM -d -a8 -OS -I%MV_HRB%\include;%MG_BCC%\include;./3rd/ed25519;./3rd/scrypt -L%MV_HRB%\lib;%MG_BCC%\lib strcmpc.c
  %MG_BCC%\bin\tlib %MV_BUILD%\%MV_LIBNAME%.lib +bcrypt.obj +blake2s.obj +ed25519.obj +sha3.obj +pbkdf2.obj +scrypt.obj +strcmpc.obj +scryptcf.obj

:CLEANUP
  if %MV_DODEL%==N goto END
  if exist scryptcf.c del scryptcf.c
  if exist *.obj      del *.obj

:END
  call ..\..\batch\makelibend.bat