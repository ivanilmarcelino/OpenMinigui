/*
 * MINIGUI - Harbour Win32 GUI library Demo
 * Copyright 2021 Verchenko Andrey <verchenkoag@gmail.com>
 *
 * Ресурсы языка для TsbViewer() / Language Resources for TsbViewer()
 *
 # Внимание! Этот файл должен быть в кодировке UTF8
 # Attention! This file must be UTF8 encoded
*/

#include "minigui.ch"

///////////////////////////////////////////////////////////////////
FUNCTION myLangeRes(nI, cPrgCP)
   LOCAL xRet, nJ, cLang := hb_CdpSelect()
   DEFAULT cPrgCP := "RU1251"

   IF nI == 1

      IF cLang == "RU1251"
         xRet := "Выход"
      ELSEIF cLang == "UA1251"
         xRet := "Вихід"
      ELSE
         xRet := "Exit"
      ENDIF

   ELSEIF nI == 2

      IF cLang == "RU1251"
         xRet := { "Список открытых БД"        , "Индексы этой базы"     ,;
                   "Переключить индекс базы"                             ,;
                   "Set relation этой базы"    , "Фильтр этой базы"      ,;
                   "Структура этой базы"       , "SEPARATOR"             ,;
                   "Выгрузить CSV"             , "Какой язык в окне ?"   ,;
                   "Инфо о фонтах в программе" , "Инфо о фонтах таблицы" ,;
                   "Список открытых окон" ,;
                   "SEPARATOR"                 , "О программе"              }
      ELSEIF cLang == "UA1251"
         xRet := { "Список відкритих БД "      , "Індекси цієї бази"     ,;
                   "Переключити індекс бази"                             ,;
                   "Set relation цієї бази"    , "Фільтр цієї бази"      ,;
                   "Структура цієї бази "      , "SEPARATOR"             ,;
                   "Вивантажити CSV"           , "Яка мова у вікні ?"    ,;
                   "Інфо про фонти у програмі" , "Інфо про фонти таблиці",;
                   "Список відкритих вікон",;
                   "SEPARATOR"                 , "Про програму" }
      ELSE
         xRet := { "List of open databases"         , "Indexes of this database"         ,;
                   "Toggle database index"                                               ,;
                   "Set relation of this base"      , "Filter of this base"              ,;
                   "Structure of this base"         , "SEPARATOR"                        ,;
                   "Upload CSV"                     , "What language is in the window ?" ,;
                   "Info about fonts in the program", "Info about table fonts"           ,;
                   "List of open windows" ,;
                   "SEPARATOR"                      , "About the program"}
      ENDIF

   ELSEIF nI == 3

      IF cLang == "RU1251"
         xRet := { "Функция TsbViewer(...);; ВЫЙТИ ИЗ ФУНКЦИИ ?" ,;
                   "ЗАКРЫТЬ", "ДА", "НЕТ" }
      ELSEIF cLang == "UA1251"
         xRet := { "Функція TsbViewer(...);; ВИЙТИ З ФУНКЦІЇ ?" ,;
                   "ЗАКРИТИ" , "ТАК", "НІ" }
      ELSE
         xRet := { "Function TsbViewer(...);; Quit this function ?" ,;
                   "CLOSE", "YES", "NO" }
      ENDIF

   ELSEIF nI == 4

      IF cLang == "RU1251"
         xRet := { "Поставить фильтр на столбец"     , "Удалить фильтр из столбца",;
                   "Удалить ВСЕ фильтры по столбцам" , "Инфо по таблице" }
      ELSEIF cLang == "UA1251"
         xRet := { "Поставити фільтр на стовпець"     , "Видалити фільтр зі стовпця",;
                   "Видалити ВСІ фільтри по стовпцях" , "Інфо по таблиці" }
      ELSE
         xRet := {"Filter on column", "Remove filter from column",;
                  "Remove ALL filters by columns" , "Info on the table" }
      ENDIF

   ELSEIF nI == 5

      IF cLang == "RU1251"
         xRet := {"Пользовательский фильтр",;
                  "Необходимо заполнить хотя бы одну строку для фильтра" }
      ELSEIF cLang == "UA1251"
         xRet := {"Користувачський фільтр",;
                  "Необхідно заповнити хоча б один рядок для фільтра" }
      ELSE
         xRet := {"Custom filter",;
                  "You must fill in at least one line for the filter"}
      ENDIF

   ELSEIF nI == 6

      IF cLang == "RU1251"
         xRet := { " равно (==)          " , " не равен (#)        " ,;
                   " больше (>)          " , " меньше (<)          " ,;
                   " больше и равно (>=) " , " меньше и равно (<=) " ,;
                   " содержит ($)     "    }
      ELSEIF cLang == "UA1251"
         xRet := { " одно (==) "          , " не дорівнює (#) "   ,;
                   " більше (>)"          , " менше (<)"          ,;
                   " більше і одно (>=) " , " менше і одно (<=) " ,;
                   " містить ($) "    }
      ELSE
         xRet := {" equal to (==)" , "not equal to (#)",;
                  " more (>)"      , "less (<)"        ,;
                  " greater than and equal (> =) ", "less and equal (<=)",;
                  " contains ($) "       }
      ENDIF

   ELSEIF nI == 7

      IF cLang == "RU1251"
         xRet := { 'И', 'ИЛИ' }
      ELSEIF cLang == "UA1251"
         xRet := { 'І', 'АБО' }
      ELSE
         xRet := {'AND', 'OR'}
      ENDIF

   ELSEIF nI == 8

      IF cLang == "RU1251"
         xRet := "Отмена"
      ELSEIF cLang == "UA1251"
         xRet := "Скасувати"
      ELSE
         xRet := "Cancel"
      ENDIF

   ELSEIF nI == 9

      IF cLang == "RU1251"
         xRet := { "Нет знака условия в первой строке фильтра !" ,;
                   "Нет знака условия во второй строке фильтра !",;
                   "Не заполнена первая строка фильтра !",;
                   "Ошибка в строке фильтра !" }
      ELSEIF cLang == "UA1251"
         xRet := { "Немає умов умов у першому рядку фільтра !" ,;
                   "Немає знаку умови у другому рядку фільтра!",;
                   "Не заповнений перший рядок фільтра!",;
                   "Помилка у рядку фільтра!" }
      ELSE
         xRet := {"No condition sign in the first line of the filter!" ,;
                  "There is no conditional sign in the second line of the filter!" ,;
                  "The first line of the filter is not filled!" ,;
                  "Error in the filter string!" }
      ENDIF

   ELSEIF nI == 10

      IF cLang == "RU1251"
         xRet := { "Пользовательское меню", "Нет открытых индексов по этой базе" }
      ELSEIF cLang == "UA1251"
         xRet := { "Меню користувача", "Немає відкритих індексів по цій базі" }
      ELSE
         xRet := {"User menu", "There are no open indexes for this database"}
      ENDIF

   ELSEIF nI == 11

      IF cLang == "RU1251"
         xRet := { "Удалить последний фильтр столбца",;
                   "Удалить ВСЕ фильтры по столбцам"     }
      ELSEIF cLang == "UA1251"
         xRet := { "Видалити останній фільтр стовпця",;
                    "Видалити ВСІ фільтри по стовпцях" }
      ELSE
         xRet := {"Remove the last column filter" ,;
                  "Remove ALL filters by columns"}
      ENDIF

   ELSEIF nI == 12

      IF cLang == "RU1251"
         xRet := "Построение списка колонки ! Подождите ..."
      ELSEIF cLang == "UA1251"
         xRet := "Побудова списку колонки! Зачекайте... "
      ELSE
         xRet := "Column list building! Wait ... "
      ENDIF

   ELSEIF nI == 13

      IF cLang == "RU1251"
         xRet := { "Копировать в буфер" ,;
                   "Вставить из буфера" ,;
                   "Удалить"            ,;
                   "Редактировать"   }
      ELSEIF cLang == "UA1251"
         xRet := { "Копіювати в буфер" ,;
                   "Вставити з буфера" ,;
                   "Видалити"          ,;
                   "Редагувати"   }
      ELSE
         xRet := { "Copy to clipboard"    ,;
                   "Paste from clipboard" ,;
                   "Delete"               ,;
                   "Edit" }
      ENDIF

   ENDIF

   IF VALTYPE(xRet) == "A"
      For nJ := 1 To Len(xRet)
         xRet[nJ] := hb_Utf8ToStr(xRet[nJ], cPrgCP )
      Next
   ELSE
      xRet := hb_Utf8ToStr(xRet, cPrgCP )
   ENDIF

RETURN xRet

