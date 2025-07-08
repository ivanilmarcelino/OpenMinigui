/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * (c) 2023 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 * (c) 2023 Sergej Kiselev <bilance@bilance.lv>
 *
 * ACHOICE() меню на объекте ButtonEx / мenu on the ButtonEx object
*/
#include "hmg.ch"
#define MYRED    { 211, 107, 107 }
#define MYBACK   {  10, 141, 115 }
////////////////////////////////////////////////////////////////////////////
//  nK := ACHOICE(nY,nX,nW,nH, aMenu,,"AchDirect",nK)
FUNCTION TestAchoice(nMode,cMenu)
   LOCAL aMenu, aIcon, nHBtn, aMenu2, aIcon2, aIcoExit, aRet, nWinAlign
   LOCAL aParWin, lBtnExit, aParIco, nIcoAlign, lBtnFlat, nG, aDay
   LOCAL aBtnFClr, aBtnBClr, aBtnGrad, nBtnFont, aParIco2, cTitle
   LOCAL nY, nX, nW, nH, lGrdHoriz, lBtnGrad, aParMenu, aIcoWin
   LOCAL aMenu3, aIcon3, aBackClr, aParIco3, lTxtAlign
   DEFAULT cMenu := "cMenu=Nil"

   // все параметры для показа массива
   nHBtn     := 48 + 6                   // ширина кнопок XX иконка + 3*2 отступы
   nG        := 20                       // отступ между кнопками
   lBtnExit  := .T.                      // добавить кнопку Exit
   nBtnFont  := 20                       // размер фонта на кнопках
   lBtnGrad  := .F.                      // НЕТ градиента на кнопках
   // параметры меню
   aParMenu  := { nHBtn, nBtnFont, nG, lBtnExit, lBtnGrad }
   // короткое меню
   aMenu     := TestDim(1,.F.)
   aIcon     := TestDim(2,.F.)
   // большое меню
   aMenu2    := TestDim(1,.T.)
   aIcon2    := TestDim(2,.T.)
   // короткое меню с иконкой на форме
   aMenu3    := TestDim(1,.F.,3)
   aIcon3    := TestDim(2,.F.,3)
   // параметры кнопки Exit
   aBtnFClr  := { WHITE, YELLOW             }   // цвет фонта кнопок
   aBtnBClr  := { MYRED, BLACK              }   // цвет фона кнопок
   aBtnGrad  := { HMG_RGB2n(RED), CLR_WHITE }   // цвет градиента кнопок
   aIcoExit  := { {"iExit1", "iExit2"}, "ВЫХОД / EXIT", aBtnFClr, aBtnBClr, aBtnGrad }   // иконки для выхода
   // параметры других кнопок
   aBtnFClr   := { WHITE, YELLOW      }   // цвет фонта кнопок
   aBtnBClr   := { {94,59,185}, BLACK }   // цвет фона кнопок
   aBtnGrad   := { HMG_RGB2n({0,176,240}) , CLR_WHITE }  // цвет градиента кнопок
   nIcoAlign  := 1        // 1-Left Align Icon, 2-Right Align Icon, 3-Top Align Icon, 4-Bottom Align Icon
   lBtnFlat   := .T.      // нет окантовки вокруг по краям кнопки, F-есть
   lGrdHoriz  := .F.      // для градиента кнопки, направление градиента, F-горизонт, T-вертикальный
   // объединим параметры кнопок
   aParIco    := { aIcon , aIcoExit, aBtnFClr, aBtnBClr, aBtnGrad, nIcoAlign, lBtnFlat, lGrdHoriz }
   aParIco2   := { aIcon2, aIcoExit, aBtnFClr, aBtnBClr, aBtnGrad, nIcoAlign, lBtnFlat, lGrdHoriz }
   nY         := nX := nW := nH := Nil
   cTitle     := "Пример выбора меню / Menu selection example - [" + cMenu + "]"
   aBackClr   := {215,184,236}
   aParWin    := { "Form_List1", cTitle, "iSanta1", aBackClr, nY,nX,nW,nH, aIcoWin }
   aIcoWin    := {}          // нет иконки на форме
   lTxtAlign  := 0           // Align Text: 0-None, 1-Left, 2-Right, 3-Center

   IF nMode == 1                  // Меню без иконки (по умолчанию)
      aParWin[4] := Nil                             // цвет фона всего окна
      aRet       := MenuAchoice(aMenu, aParWin, aParMenu, {}, aIcoWin, lTxtAlign )  

   ELSEIF nMode == 2              // Меню без иконки (с параметрами)
      aParWin[4] := {159,191,236}                  // цвет фона всего окна
      nBtnFont   := 22                             // размер фонта на кнопках
      lBtnExit   := .F.                            // НЕ добавить кнопку Exit
      lBtnGrad   := .F.                            // НЕТ градиента на кнопках
      aParMenu   := { nHBtn, nBtnFont, nG, lBtnExit, lBtnGrad }
      aParIco[1] := aParIco[2] := {}               // иконок на кнопках нет
      aParIco[4] := { {0,176,240}, {94,59,185} }   // цвет фона кнопок
      aParIco[5] := {}                             // БЕЗ градиента кнопок
      aRet       := MenuAchoice(aMenu, aParWin, aParMenu, aParIco, aIcoWin, lTxtAlign  )   

   ELSEIF nMode == 3                  // Меню с иконкой (иконка слева)
      aParIco[3] := { WHITE, YELLOW }              // цвет фонта кнопок
      aRet       := MenuAchoice(aMenu, aParWin, aParMenu, aParIco  )   

   ELSEIF nMode == 4                  // Меню с иконкой (иконка справа)
      aParIco[6] := 2                              // 2-Right Align Icon
      aRet       := MenuAchoice(aMenu, aParWin, aParMenu, aParIco  )   

   ELSEIF nMode == 5                 // Меню с иконкой (иконка вверху)
      nG         := 10                             // отступ между кнопками
      nHBtn      := 64 + nBtnFont * 2              // ширина кнопок
      aParIco[6] := 3                              // 3-Top Align Icon
      aParMenu   := { nHBtn, nBtnFont, nG, lBtnExit, lBtnGrad }
      aRet       := MenuAchoice(aMenu, aParWin, aParMenu, aParIco  )   

   ELSEIF nMode == 6                // Меню с иконкой (иконка внизу)
      nG         := 10                             // отступ между кнопками
      nHBtn      := 64 + nBtnFont * 2              // ширина кнопок
      aParIco[6] := 4                              // 4-Bottom Align Icon
      aParMenu   := { nHBtn, nBtnFont, nG, lBtnExit, lBtnGrad }
      aRet       := MenuAchoice(aMenu, aParWin, aParMenu, aParIco  )   

   ELSEIF nMode == 7                // Меню с иконкой + градиент
      nG         := 10                             // отступ между кнопками
      lBtnGrad   := .T.                            // градиент на кнопках
      nHBtn      := 64 + 6                         // ширина кнопок XX иконка + 3*2 отступы
      nBtnFont   := 22                             // размер фонта на кнопках
      //lBtnExit := .F.                            // НЕ добавить кнопку Exit
      aParMenu   := { nHBtn, nBtnFont, nG, lBtnExit, lBtnGrad }
      aIcoExit   := { {"iExit1", "iExit2"}, "ВЫХОД / EXIT", {MAROON,YELLOW}, aBtnBClr, {HMG_RGB2n(RED),CLR_WHITE} }  // иконки для выхода
      aParIco    := { aIcon , aIcoExit, aBtnFClr, aBtnBClr, aBtnGrad, nIcoAlign, lBtnFlat, lGrdHoriz }
      aParIco[3] := { BLACK, BLUE }                // цвет фонта кнопок
      aRet       := MenuAchoice(aMenu, aParWin, aParMenu, aParIco  )   

   ELSEIF nMode == 8                // Меню с иконкой + градиент HORIZONTAL
      nG         := 10                             // отступ между кнопками
      lBtnGrad   := .T.                            // градиент на кнопках
      nHBtn      := 64 + 6                         // ширина кнопок XX иконка + 3*2 отступы
      nBtnFont   := 22                             // размер фонта на кнопках
      //lBtnExit := .F.                            // НЕ добавить кнопку Exit
      aParMenu   := { nHBtn, nBtnFont, nG, lBtnExit, lBtnGrad }
      aIcoExit   := { {"iExit1", "iExit2"}, "ВЫХОД / EXIT", {MAROON,YELLOW}, aBtnBClr, {HMG_RGB2n(RED),CLR_WHITE} }  // иконки для выхода
      aParIco    := { aIcon , aIcoExit, aBtnFClr, aBtnBClr, aBtnGrad, nIcoAlign, lBtnFlat, lGrdHoriz }
      aParIco[3] := { BLACK, YELLOW }         // цвет фонта кнопок
      aParIco[5] := { HMG_RGB2n(PURPLE) , CLR_WHITE }  // цвет градиента кнопок
      aParIco[8] := .T.                       // для градиента кнопки, направление градиента, F-горизонт, T-вертикальный
      aRet       := MenuAchoice(aMenu, aParWin, aParMenu, aParIco  )   

   ELSEIF nMode == 9                  // Большое меню без иконок
      aParMenu[3] := 10                       // отступ между кнопками
      aRet        := MenuAchoice(aMenu2, aParWin, aParMenu, {}     )   

   ELSEIF nMode == 10                 // Большое меню с иконками
      nG         := 5                         // отступ между кнопками
      lBtnGrad   := .T.                       // градиент на кнопках
      nHBtn      := 64 + 6                    // ширина кнопок XX иконка + 3*2 отступы
      nBtnFont   := 22                        // размер фонта на кнопках
      aParMenu   := { nHBtn, nBtnFont, nG, lBtnExit, lBtnGrad }
      aParIco2[3]:= { BLUE, RED }             // цвет фонта кнопок
      aRet       := MenuAchoice(aMenu2, aParWin, aParMenu, aParIco2 )   
   ELSEIF nMode == 11                   // Меню по массиву (по умолчанию)
      aRet       := MenuAchoice(aMenu)        

   ELSEIF nMode == 12                   // Меню по массиву c иконкой слева на форме
      nWinAlign := 1   // 1-Left Align Icon, 2-Right Align Icon
      aIcoWin   := { "iHP1", 150, 0, 20, nWinAlign }             // иконка на форме
      aParWin   := { "Form_Print", cTitle, "iHP1", TEAL, 80, 150, nW, nH }
      aRet      := MenuAchoice(aMenu3, aParWin, , , aIcoWin)     

   ELSEIF nMode == 13                 // Меню по массиву c иконкой справа на форме
      aBackClr   := {159,191,236}                                // цвет фона всего окна
      lBtnGrad   := .T.                                          // градиент на кнопках
      nG         := 20                                           // отступ между кнопками
      aParMenu   := { nHBtn, nBtnFont, nG, lBtnExit, lBtnGrad }
      aParWin    := { "Form_Print", cTitle, "iHP2", aBackClr, 100, 250, nW, nH }
      nWinAlign  := 2   // 1-Left Align Icon, 2-Right Align Icon
      aIcoWin    := { "iHP2", 128, 0, 20, nWinAlign }            // иконка на форме
      aBtnFClr   := { BLACK, BLUE }                              // цвет фонта кнопок
      aIcoExit   := { {"iExit1", "iExit2"}, "ВЫХОД / EXIT", {MAROON,YELLOW}, aBtnBClr, {HMG_RGB2n(RED),CLR_WHITE} }  // иконки для выхода
      aParIco3   := { aIcon3, aIcoExit, aBtnFClr, aBtnBClr, aBtnGrad, nIcoAlign, lBtnFlat, lGrdHoriz }
      lTxtAlign  := 1        // Align Text: 0-None, 1-Left, 2-Right, 3-Center
      aRet       := MenuAchoice(aMenu3, aParWin, aParMenu, aParIco3, aIcoWin, lTxtAlign)     

   ELSEIF nMode == 14                  // Меню дней недели (выровнять текст по левому краю)
      lTxtAlign  := 1        // Align Text: 0-None, 1-Left, 2-Right, 3-Center
      aDay := TestaDay()
      aRet := MenuAchoice(aDay, , , , , lTxtAlign)        

   ELSEIF nMode == 15                  // Меню дней недели (выровнять текст по правому краю)
      lTxtAlign  := 2        // Align Text: 0-None, 1-Left, 2-Right, 3-Center
      aDay := TestaDay()
      aRet := MenuAchoice(aDay, , , , , lTxtAlign)        

   ENDIF

   //AlertInfo("aRet= " + HB_ValToExp(aRet) + ";;" + ProcNL(), "Return result" )

RETURN aRet

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION TestDim(nMode,lMode,nAdd)
   LOCAL nI, aRet, aMenu, aIcon, aMenu3, aIcon3
   DEFAULT nAdd := 0

   aMenu := { "Selection menu-1,   additional text with spaces,   wide menu bar" ,;
              "Selection menu-2,   additional text with spaces,   wide menu bar" ,;
              "Selection menu-3,   additional text with spaces,   wide menu bar" ,;
              "Selection menu-4,   additional text with spaces,   wide menu bar"    }

   aIcon := { {"iSanta1" ,"iSanta2" }  ,;
              {"iFolder1","iFolder2"}  ,;
              {"iHP1"    ,"iHP2"    }  ,;
              {"iHmg1"   ,"iHmg2"   }      }

   IF nAdd > 0
      aMenu3 := { "Selection menu-1, wide menu bar;second menu line" ,;
                  "Selection menu-2, wide menu bar;second menu line" ,;
                  "Selection menu-3, wide menu bar;second menu line" ,;
                  "Selection menu-4, wide menu bar;second menu line"    }

      aIcon3 := { {"iFolder1","iFolder2" } ,;
                  {"iFolder1","iFolder2" } ,;
                  {"iFolder1","iFolder2" } ,;
                  {"iFolder1","iFolder2" }    }
   ENDIF

   IF lMode
      FOR nI := 1 TO 12
          AADD( aMenu, "Additional selection menu (" + HB_NtoS(nI+4) + ") ......" )
          AADD( aIcon, {"iClock1","iClock2"} )
      NEXT
   ENDIF

   IF nMode == 1
      aRet := IIF(nAdd==0, aMenu, aMenu3)
   ELSE
      aRet := IIF(nAdd==0, aIcon, aIcon3)
   ENDIF

RETURN aRet

////////////////////////////////////////////////////////////////////////////
FUNCTION TestaDay()
   LOCAL nL, nI, aDay

   SET CODEPAGE TO ENGLISH   
   SET LANGUAGE TO ENGLISH   

   nL   := 0
   aDay := ARRAY(7)
   FOR nI := 1 TO LEN(aDay)
      aDay[nI] := HB_NtoS(nI) +  " - " + LOWER( NTOCDOW( nI ) ) + " "
      nL := MAX(LEN(aDay[nI]), nL)
   NEXT
   //FOR nI := 1 TO LEN(aDay)
   //   aDay[nI] := PADR(aDay[nI],nL)
   //NEXT

   SET LANGUAGE TO RUSSIAN
   SET CODEPAGE TO RUSSIAN

RETURN aDay
