/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2020 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 * Helped and taught by SergKis  http://clipper.borda.ru
 *
*/

#include "hmg.ch"
* ======================================================================
FUNCTION ProcNameLine(nVal)
   DEFAULT nVal := 0
   RETURN "Вызов из: " + ProcName( nVal + 1 ) + "(" + hb_ntos( ProcLine( nVal + 1 ) ) + ") --> " + ProcFile( nVal + 1 )

* ======================================================================
FUNCTION ProcNL(nVal)
   DEFAULT nVal := 0
   RETURN "Вызов из: " + ProcName( nVal + 1 ) + "(" + hb_ntos( ProcLine( nVal + 1 ) ) + ") --> " + ProcFile( nVal + 1 )

* ======================================================================
FUNCTION TotalTimeExports( cMsg, cFile, tTime )
   ? "=> " + cMsg + " " + cFile
   ? "  Total time spent on exports - " + HMG_TimeMS( tTime )
   ? "  ."
   RETURN NIL

* ======================================================================
FUNCTION MG_Stop( cMsg , cVal )
   RETURN AlertStop( cMsg , cVal )

* ======================================================================
* При наличии файла добавить число версии в имя файла
FUNCTION GetFileNameMaskNum( cFile )
   LOCAL i := 0, cPth, cFil, cExt

   If ! hb_FileExists(cFile); RETURN cFile
   EndIf

   hb_FNameSplit(cFile, @cPth, @cFil, @cExt)

   WHILE ( hb_FileExists( hb_FNameMerge(cPth, cFil + '(' + hb_ntos(++i) + ')', cExt) ) )
   END

   RETURN hb_FNameMerge(cPth, cFil + '(' + hb_ntos(i) + ')', cExt)

