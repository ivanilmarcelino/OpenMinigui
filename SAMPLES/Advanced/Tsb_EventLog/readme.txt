MINIGUI - Harbour Win32 GUI library Demo
Copyright 2025 Sergej Kiselev <bilance@bilance.lv>
Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
--------------------------------------------------------------------------------------------------------------------

Журнал событий в программе - запись действий пользователей выполненой работы.

Статистика выполнения(события программы) по операторам за периоды времени - кнопка "F5 Отчёты".

События программы - справочник смотреть/добавлять: user2log.prg функция EVENTS_Dim().

Аварийная ошибка в программе - смотреть модуль: demo.prg
   _HMG_bOnErrorInit := {|cMsg,oErr,cTxt,cErr| my_ErrorExit(cMsg,oErr,cTxt,cErr) }
   _HMG_bOnErrorExit := {|                   | my_ErrorExit()     }
Сама функция в demo_ErrorLog.prg


--------------------------------------------------------------------------------------------------------------------

The program event log records user actions and completed work.

Execution statistics (program events) by operator over time periods - press the "F5 Reports" button.

Program events - view/add reference: user2log.prg function EVENTS_Dim().

Program emergency - see module: demo.prg
   _HMG_bOnErrorInit := {|cMsg,oErr,cTxt,cErr| my_ErrorExit(cMsg,oErr,cTxt,cErr) }
   _HMG_bOnErrorExit := {| | my_ErrorExit() }
The function itself is in demo_ErrorLog.prg
