MINIGUI - Harbour Win32 GUI library Demo
Copyright 2025 Sergej Kiselev <bilance@bilance.lv>
Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
--------------------------------------------------------------------------------------------------------------------
* _TBrowse() Показ файлов .txt .csv .arr
* Конвертация файлов в другие кодировки, включая UTF8
* Экспорт файлов в .xls, .dbf, .arr
* Преобразование колонок в ТСБ, поиск по массиву, итого по колонкам - два варианта

Меню на кнопках - описание через oMenu и показ на форме через функцию TopMenuButtons():
     oMenu := MenuMainBtn()
     TopMenuButtons(owc,oMenu,nY,nX,nW,nH,nG)  

Новые цвета и иконки в функции Alert*()

--------------------------------------------------------------------------------------------------------------------
* _TBrowse() Displays .txt, .csv, and .arr files
* Converts files to other encodings, including UTF8
* Exports files to .xls, .dbf, and .arr
* Converts columns to TSB, searches by array, and displays totals by column - two options

Menu on buttons - description via oMenu and display on the form via the TopMenuButtons() function:
        oMenu := MenuMainBtn()
        TopMenuButtons(owc,oMenu,nY,nX,nW,nH,nG)

New colors and icons in the Alert*() function