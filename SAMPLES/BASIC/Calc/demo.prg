/*
 * MiniGUI Calculator Demo
 *
 */

#include "minigui.ch"

Set Proc To Calc.prg

Function Main

        Load Window Demo As Main

        ON KEY F2 OF Main ACTION RunCalc()

        Main.Center
        Main.Activate

Return Nil

Procedure RunCalc()
         IF IsWindowDefined( "Calc" )
            _RestoreWindow("Calc")
            Return
         EndIF
         Main.Get_1.Value := ShowCalc(Main.Get_1.Value)
         Main.Text_2.Value := System.ClipBoard
Return

