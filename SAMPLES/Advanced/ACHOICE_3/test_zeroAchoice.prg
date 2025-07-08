/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * (c) 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 * (c) 2024 Sergej Kiselev <bilance@bilance.lv>
 *
 * ACHOICE() меню на объекте AVI + LABEL / ACHOICE() menu on AVI + LABEL object
*/
#define _HMG_OUTLOG

#include "hmg.ch"

///////////////////////////////////////////////////////////////////
//  nK := ACHOICE(nY,nX,nW,nH, aMenu,,"AchDirect",nK)
FUNCTION TestZeroAchoice()
   LOCAL aRet, aMenu

   aMenu := {{"Selection menu-1" }, ;
             {"Selection menu-2" }, ;
             {"Selection menu-3" }, ;
             {"Selection menu-4" }, ;
             {"Selection menu-5" }, ;
             {"Selection menu-6" }, ;
             {"Exit menu"        }  ;
            }

   ? ProcNL(), "aMenu=", aMenu ; ?v aMenu
   aRet := MenuAviAchoice( aMenu )

RETURN aRet

