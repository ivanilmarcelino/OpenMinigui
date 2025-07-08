/*----------------------------------------------------------------------------
 MINIGUI - Harbour Win32 GUI library source code

   Copyright 2023 Sergej Kiselev <bilance@bilance.lv>
   Copyright 2023 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
   
   Внешние события в программе 
   External events in the program 

---------------------------------------------------------------------------*/

#define WM_COPYDATA                     74

#define WM_ENDSESSION                   0x0016
#define WM_SYSTEMERROR                  0x0017
#define WM_POWERBROADCAST               0x0218   // Событие управления питанием

#define PBT_APMQUERYSUSPEND             0x0000
#define PBT_APMQUERYSTANDBY             0x0001
#define PBT_APMQUERYSUSPENDFAILED       0x0002
#define PBT_APMQUERYSTANDBYFAILED       0x0003
#define PBT_APMSUSPEND                  0x0004    // переход в спящий режим 
#define PBT_APMSTANDBY                  0x0005
#define PBT_APMRESUMECRITICAL           0x0006
#define PBT_APMRESUMESUSPEND            0x0007   // выход из спящего режима, инициированный пользователем (например, нажата клавиша) 
#define PBT_APMRESUMESTANDBY            0x0008
#define PBT_APMRESUMEAUTOMATIC          0x0012   // выход из спящего режима 

