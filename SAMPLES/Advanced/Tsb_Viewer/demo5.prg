/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2023 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2023 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Просмотр/правка Dbf файла. Опции/свойства по базе
 * View/edit Dbf file. Options/properties by base
*/

#include "minigui.ch"

REQUEST DBFCDX

FUNCTION Main()
   LOCAL cDbf := GetStartUpFolder() + '\_Engl.dbf'
   LOCAL cAls := "Customer"
         
   SET CODEPAGE TO ENGLISH            
   SET LANGUAGE TO ENGLISH            
   RddSetDefault("DBFCDX")

   SET FONT TO "DejaVu Sans Mono", 13

   USE (cDbf) ALIAS (cAls) NEW EXCLUSIVE
   // можно без параметров / calling is possible without parameters
   TsbViewer()   

RETURN NIL

