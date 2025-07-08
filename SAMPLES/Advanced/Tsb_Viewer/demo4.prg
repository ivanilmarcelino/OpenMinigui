/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2023 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2023 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Просмотр/правка Dbf файла. Опции/свойства по базе
 * View/edit Dbf file. Options/properties by base
*/

#define  _HMG_OUTLOG      
#include "minigui.ch"

REQUEST HB_LANG_EN
REQUEST HB_CODEPAGE_RU1251, HB_CODEPAGE_RU866, HB_CODEPAGE_UTF8
REQUEST DBFCDX, DBFFPT

FUNCTION Main()
   LOCAL nY, nX, nW, nH, aBClr, cMsg, lCntr, cAls, cDbf, cCodePage
   LOCAL aTsbPar, aWinPar, cLog
         
   SET CODEPAGE TO ENGLISH            
   SET LANGUAGE TO ENGLISH            
                                      
   SET EPOCH   TO 2000
   SET DATE    TO GERMAN
   SET CENTURY ON
   SET NAVIGATION EXTENDED
   SET OOP ON

   RddSetDefault("DBFCDX")
   SET AUTOPEN OFF   // не открывать автоматически индексные файлы
   SET DELETED OFF

   SET FONT TO "DejaVu Sans Mono", 13
   DEFINE FONT DlgFont FONTNAME "DejaVu Sans Mono" SIZE 14   // for HMG_Alert()

   cLog := GetStartUpFolder() + "\_Msg4.log" ;  fErase( cLog )
   SET LOGFILE TO &cLog

   SET WINDOW MAIN OFF 

   cAls      := "ENG_COMPANY"
   cDbf      := GetStartUpFolder() + '\_Engl.dbf'
   cCodePage := hb_SetCodepage()

   IF ! myUseArea( cDbf, cAls, .F., , , )   // ==>> TsbViewMisc.prg
      cMsg := "ERROR opening database!;;" 
      cMsg += cDbf
      AlertStop( cMsg, "Result", "ZZZ_B_STOP64", 64 )
      Quit
   ENDIF
   ? ProcNL(), Alias()

   nY := nX := 0
   nW      := System.ClientWidth 
   nH      := System.ClientHeight   
   lCntr   := .T.
   aBClr   := HMG_ColorWinActiveCaption()                  // color of the Active window caption
   aTsbPar := { cAls, cCodePage, "Checkpoint (4) !" }
   aWinPar := { "WAIT", "", nY, nX, nW, nH, lCntr, aBClr }
   // Call viewer
   TsbViewer( aTsbPar, aWinPar)                            // окно с таблицей / window with table

RETURN NIL

