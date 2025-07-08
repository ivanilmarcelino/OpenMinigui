MINIGUI - Harbour Win32 GUI library Demo
Copyright 2025 Sergej Kiselev <bilance@bilance.lv>
Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region

--------------------------------------------------------------------------------------------------------------------
demo1-en.hbp   ������ �� ���������� �����
demo1-ru.hbp   ������ �� ������� �����
��� ����� ����� ����������� ������� ����� OBJ !!! 

������ ������ � �������� Tsbrowse (���) - �������� ����� ������ dbf-�����.
������������� ���� ����� ���� (����� ���������):
   cRType == ""       // ��� ���� - ��������� ����� �������
   cRType $ "CLDNM"   // ��������� ����
   cRType == "DMN"    // ���� � ����������
   cRType == "DT"     // ����+�����
   cRType == "M"      // ����-���� ��������� �����
   cRType == "A"      // ��������� ������ ����� ����, ��������: {"TelFIO","TelFIO3","TelFIO2"}
   cRType == "CALC"   // ��������� ���������� - ��������� �����
   cRType == "SPR_A"  // ��������� ������ - ��������� �����
   cRType == "SPR_S"  // ���������� �� dbf ����� - ����������� ����, ��������: {"Master","KMaster","Master",2,"KFIRMA==1"}
   cRType == "K"      // ��� � ���� (��� �� set relation �� ������ ����) - ��������� ����� ���������� ������� - ��������� (14)

��������� ����� ������/���������� �������� � ����� �� ���� oBrw:FilterFTS()

���������� ������ �������, ����������, ��������� - ������ � 2 ����-����: "MOb4orud" � "MKob4or"
   AADD( aDim, {"(*) Works completed/Equipment", ...., "CALC","MOb4orud", .... "Tovar_HMG()"....

���������� ������ ����-���� �� ��������� ����� ������� ����� ������� MemoLine, �������� ����� ����������� ���������,
�������� App.Cargo:nMemoChar := CalcMemoLine()

����� ������� ���������� ������ ����� �������, ����� ��������� - 3 ������� �������.
��� ����������� � ���-����� GetUserTempFolder() + "\tmp_SortUser.ini"

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
