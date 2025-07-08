/*
 * MINIGUI - Harbour Win32 GUI library 
 *
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * _TBrowse() –азные функции дл€ редактирований €чеек таблицы
 * _TBrowse() Various functions for editing table cells
*/

//////// for card_array_zaiv.prg ////////////////////////////////////////////////////////////////////
#define ACOL_1   1    // (1)  - колонка показа
#define ACOL_2   2    // (2)  - колонка показа, правка значени€
#define ACOL_3   3    // (3)  - пор€док показа строк в таблице
#define ACOL_4   4    // (4)  - тип обработки €чеек таблицы
#define ACOL_5   5    // (5)  - поле базы в dbf
#define ACOL_6   6    // (6)  - функци€ чтение пол€ (4) и запись в колонку (2)
#define ACOL_7   7    // (7)  - функци€ записи в поле (4) из колонки (2) или (8)-где NIL
#define ACOL_8   8    // (8)  - функци€ дл€ окна редактировани€ переменных дл€ типа CALC,SPR_A,SPR_J,SPR_S
#define ACOL_9   9    // (9)  - доступ редактировани€ €чеек Write/Read
#define ACOL_10  10   // (10) - преобразование в "C" колонки (12)
#define ACOL_11  11   // (11) - преобразование в "C" колонки (13)
#define ACOL_12  12   // (12) - преобразование в "C" колонки (14)
#define ACOL_13  13   // (13) - значение исправленного пол€ {} дл€ типа CALC,SPR_A,SPR_J,SPR_S из (3), в остальных случа€х NIL
#define ACOL_14  14   // (14) - доп.данные дл€ типа (3): SPR_A,CALC,SPR_J,SPR_S,CALC
#define ACOL_15  15   // (15) - доп.данные разные

#define ADIM_SORT 3   // 3-элемент колонки сортировка юзера
