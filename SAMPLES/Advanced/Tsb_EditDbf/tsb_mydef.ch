/*
 * MINIGUI - Harbour Win32 GUI library 
 *
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * _TBrowse() ������ ������� ��� �������������� ����� �������
 * _TBrowse() Various functions for editing table cells
*/

//////// for card_array_zaiv.prg ////////////////////////////////////////////////////////////////////
#define ACOL_1   1    // (1)  - ������� ������
#define ACOL_2   2    // (2)  - ������� ������, ������ ��������
#define ACOL_3   3    // (3)  - ������� ������ ����� � �������
#define ACOL_4   4    // (4)  - ��� ��������� ����� �������
#define ACOL_5   5    // (5)  - ���� ���� � dbf
#define ACOL_6   6    // (6)  - ������� ������ ���� (4) � ������ � ������� (2)
#define ACOL_7   7    // (7)  - ������� ������ � ���� (4) �� ������� (2) ��� (8)-��� NIL
#define ACOL_8   8    // (8)  - ������� ��� ���� �������������� ���������� ��� ���� CALC,SPR_A,SPR_J,SPR_S
#define ACOL_9   9    // (9)  - ������ �������������� ����� Write/Read
#define ACOL_10  10   // (10) - �������������� � "C" ������� (12)
#define ACOL_11  11   // (11) - �������������� � "C" ������� (13)
#define ACOL_12  12   // (12) - �������������� � "C" ������� (14)
#define ACOL_13  13   // (13) - �������� ������������� ���� {} ��� ���� CALC,SPR_A,SPR_J,SPR_S �� (3), � ��������� ������� NIL
#define ACOL_14  14   // (14) - ���.������ ��� ���� (3): SPR_A,CALC,SPR_J,SPR_S,CALC
#define ACOL_15  15   // (15) - ���.������ ������

#define ADIM_SORT 3   // 3-������� ������� ���������� �����
