/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * (c) 2023 Verchenko Andrey <verchenkoag@gmail.com>
 * (c) 2023 Sergej Kiselev <bilance@bilance.lv>
 *
 * Модуль всех таймеров программы / Module of all program timers
*/
#define _HMG_OUTLOG

#include "minigui.ch"

///////////////////////////////////////////////////////////
// таймер чтения всех работающих в журнале программы
FUNCTION myTimer1(oThis)
   LOCAL cVal, cForm, cForm2, aObjLbl, cRet, nJ, cLen
   LOCAL cWndTmr, cTime, oac := App.Cargo

   oac:nMeter1++
   cForm   := oac:cWinMain
   nJ      := oac:nMeter1
   cWndTmr := oac:cTimer_Wnd      // Form_Timer
   cTime   := "/" + HB_NtoS(oac:nTimer1) + " sec"

   ? "@@@ Timer_1", HB_DATETIME(), ProcNL()
   // o2Log(oThis, 19, "*** oThis =>")
   //? "oThis =", oThis ; ?v oThis:GetAll() ; ?
   ? "   ==>", oac:cFormGotFocus, cForm

   // в качестве примера это
   cVal := "Timer_1(" + HB_NtoS(nJ) + cTime + ")"
   SetProperty(cForm, "Label_1", "Value", cVal)

   // вместо юзеров в базу пишется открытые формы программы
   cRet    := LogUsers(@cLen)         // -> Users.prg
   cForm2  := "Forma_Main"
   aObjLbl :=  oac:aTimerLabel  // на форме Forma_Main Label: { "Label_Bottom1" , "Label_Bottom2", "Label_Bottom3" }
   IF _IsWindowActive( cForm2 )
      cVal := "=> (" + HB_NtoS(nJ) + cTime
      cVal += ") Users["  + cLen + "]: " + cRet
      SetProperty(cForm2, aObjLbl[1], "Value", cVal)
   ENDIF
   ?? "LogUsers()=",cRet

   oThis:xResult := {"Результат:", ProcName(), cVal}

RETURN cRet

///////////////////////////////////////////////////////////
// таймер счёта всех объектов открытых форм
FUNCTION myTimer2(oThis)
   LOCAL cVal, cForm, cForm2, aObjLbl, cRet, nJ
   LOCAL cTime, cWndTmr, oac := App.Cargo

   oac:nMeter2++
   cForm   := oac:cWinMain
   nJ      := oac:nMeter2
   cWndTmr := oac:cTimer_Wnd      // Form_Timer
   cTime   := "/" + HB_NtoS(oac:nTimer2) + " sec"

   ? "@@@ Timer_2", HB_DATETIME(), ProcNL()
   // o2Log(oThis, 19, "*** oThis =>")
   //? "oThis =", oThis ; ?v oThis:GetAll() ; ?
   ? "   ==>", oac:cFormGotFocus, cForm

   // в качестве примера это
   cVal := "Timer_2(" + HB_NtoS(nJ) + cTime + ")"
   SetProperty(cForm, "Label_2", "Value", cVal)

   cRet    := myGetFormsObjects()     // -> util_misc.prg
   cForm2  := "Forma_Main"
   aObjLbl :=  oac:aTimerLabel  // на форме Forma_Main Label: { "Label_Bottom1" , "Label_Bottom2", "Label_Bottom3" }
   IF _IsWindowActive( cForm2 )
      cVal := "=> (" + HB_NtoS(nJ) + cTime
      cVal += ") Object: " + cRet
      SetProperty(cForm2, aObjLbl[2], "Value", cVal)
   ENDIF
   ?? "myGetFormsObjects()=",cRet

   oThis:xResult := {"Результат:", ProcName(), cVal}

RETURN cRet

///////////////////////////////////////////////////////////
FUNCTION myTimer3(oThis)
   LOCAL cVal, cForm, cForm2, aObjLbl, nJ, cFile
   LOCAL cTime, cWndTmr, oac := App.Cargo

   oac:nMeter3++
   cForm   := oac:cWinMain
   cWndTmr := oac:cTimer_Wnd      // Form_Timer
   nJ      := oac:nMeter3
   cFile   := oac:cPathPng
   cFile   += "Screen_" + DTOS( DATE() )
   cFile   += "-" + CharRepl( ":", TIME(), "-" )
   cFile   += ".png"
   cTime   := "/" + HB_NtoS(oac:nTimer3) + " sec"

   ? "@@@ Timer_3", HB_DATETIME(), ProcNL()
   // o2Log(oThis, 19, "*** oThis =>")
   // ? "oThis =", oThis ; ?v oThis:GetAll() ; ?
   ? "   ==>", oac:cFormGotFocus, cFile

   // в качестве примера это
   cVal := "Timer_3(" + HB_NtoS(nJ) + cTime + ") " + cFileNoPath(cFile)
   SetProperty(cForm, "Label_3", "Value", cVal)

   cForm2  := "Forma_Main"
   aObjLbl :=  oac:aTimerLabel  // на форме Forma_Main Label: { "Label_Bottom1" , "Label_Bottom2", "Label_Bottom3" }
   IF _IsWindowActive( cForm2 )
      cVal := "=> (" + HB_NtoS(nJ)
      cVal += cTime + ") " + cFile
      SetProperty(cForm2, aObjLbl[3], "Value", cVal)
   ENDIF

   // запись экрана рабочего стола для контроля работы юзера
   // recording the desktop screen to control the user's work
   BT_BitmapSaveFile(BT_BitmapCaptureDesktop(), cFile, 4)

   oThis:xResult := {"Результат:", ProcName(), cVal}

RETURN ""

/////////////////////////////////////////////////////////////////
FUNCTION myTimer9(oThis)  // резерв - не использую
   LOCAL cVal, cForm, nJ, cWndTmr, cLblTmr, oac := App.Cargo

   oac:nMeter9++
   cForm   := oac:cWinMain
   cWndTmr := oac:cTimer_Wnd      // Form_Timer
   cLblTmr := oac:cTimer_Label    // "Lbl_9"
   nJ      := oac:nMeter9

   ? "@@@ Timer_9", HB_DATETIME(), ProcNL()
   // o2Log(oThis, 19, "*** oThis =>")
   // ? "oThis =", oThis ; ?v oThis:GetAll() ; ?
   ? "   ==>", oac:cFormGotFocus

   // вывести значение в окно Form_Timer
   cVal := "Timer_9 (" + HB_NtoS(oac:nTimer9) + "мин.) -> "
   SetProperty(cWndTmr, cLblTmr, "Value", cVal)

   oThis:xResult := {"Результат:", ProcName(), cVal}

RETURN ""

