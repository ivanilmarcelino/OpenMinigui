MINIGUI - Harbour Win32 GUI library Demo
Copyright 2025 Sergej Kiselev <bilance@bilance.lv>
Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region

--------------------------------------------------------------------------------------------------------------------
demo1-en.hbp   проект на английском языке
demo1-ru.hbp   проект на русском языке
При смене языка ОБЯЗАТЕЛЬНО удалить папку OBJ !!! 

Пример работы с объектом Tsbrowse (ТСБ) - карточка одной записи dbf-файла.
Использование типы полей базы (можно добавлять):
   cRType == ""       // без поля - обработка через функцию
   cRType $ "CLDNM"   // известные типы
   cRType == "DMN"    // дата с календарем
   cRType == "DT"     // дата+время
   cRType == "M"      // мемо-поле отдельная форма
   cRType == "A"      // вложенный массив полей базы, например: {"TelFIO","TelFIO3","TelFIO2"}
   cRType == "CALC"   // вложенный справочник - отдельная форма
   cRType == "SPR_A"  // вложенный массив - отдельная форма
   cRType == "SPR_S"  // справочник из dbf файла - контекстное меню, например: {"Master","KMaster","Master",2,"KFIRMA==1"}
   cRType == "K"      // код в базе (как по set relation из другой базы) - обработка через внутреннюю функцию - заполнить (14)

Несколько видов поиска/сортировки массивов и поиск по базе oBrw:FilterFTS()

Справочник выбора товаров, количество, стоимость - запись в 2 мемо-поля: "MOb4orud" и "MKob4or"
   AADD( aDim, {"(*) Works completed/Equipment", ...., "CALC","MOb4orud", .... "Tovar_HMG()"....

Автоформат показа мемо-поля на несколько строк таблицы через функцию MemoLine, работает после перезапуска программы,
смотреть App.Cargo:nMemoChar := CalcMemoLine()

Можно править сортировку показа строк таблицы, через настройку - 3 колонка таблицы.
Она сохраняется в ини-файле GetUserTempFolder() + "\tmp_SortUser.ini"

--------------------------------------------------------------------------------------------------------------------
demo1-en.hbp project in English
demo1-ru.hbp project in Russian
When changing the language, you MUST delete the OBJ folder !!!

An example of working with the Tsbrowse (TSB) object - a card of one dbf-file record.
Using database field types (can be added):

   cRType == ""      // no field - processing via function
   cRType $ "CLDNM"  // known types
   cRType == "DMN"   // date with calendar
   cRType == "DT"    // date+time
   cRType == "M"     // memo field separate form
   cRType == "A"     // nested array of database fields, for example: {"TelFIO","TelFIO3","TelFIO2"}
   cRType == "CALC"  // nested reference - separate form
   cRType == "SPR_A" // nested array - separate form
   cRType == "SPR_S" // reference from dbf file - context menu, for example: {"Master","KMaster","Master",2,"KFIRMA==1"}
   cRType == "K"     // code in the database (as by set relation from another database) - processing via internal function

Several types of array search/sorting and search by database oBrw:FilterFTS()

Product selection directory, quantity, cost - entry in 2 memo fields: "MOb4orud" and "MKob4or"
    AADD( aDim, {"(*) Works completed/Equipment", ...., "CALC","MOb4orud", .... "Tovar_HMG()"....

Auto format display of memo field on several table lines via MemoLine function, works after restarting the program,
see App.Cargo:nMemoChar := CalcMemoLine()

You can edit the sorting of the table rows display, through the setting - 3 table column.
It is saved in the ini-file GetUserTempFolder() + "\tmp_SortUser.ini"
