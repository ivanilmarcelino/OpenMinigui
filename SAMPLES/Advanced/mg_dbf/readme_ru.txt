MINIGUI - Harbour Win32 GUI library Demo
Copyright 2024 Sergej Kiselev <bilance@bilance.lv>
Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
�������� DBF ������ ��� Harbour/Clipper/DbaseIV/Foxpro/Six
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
������� ������������ ���� �������� DBF-������ �������������� Harbour
   DBFCDX - memo *.fpt (FoxPro compatible)
   DBFNTX - memo *.dbt (Clipper compatible) 
   DBFNSX - memo *.smt (Six compatible) 
   SIXCDX - memo *.fpt/smt (FoxPro compatible) 
   BMDBFCDX - bitmap filter,compatible with dbfcdx
   BMDBFNTX - bitmap filter,compatible with dbfntx
   BMDBFNSX - bitmap filter,compatible with dbfnsx
   BM_DBSEEKWILD - bitmap filter

�������� ��� ������� ������� Harbour (����� 100)
�������� ������ - ��� ����� �� �������/���������� ������
�����/������ �� ���� ����� ����
��� ����� ���������: ������� � ����������
������� � .csv .dbf .sqlite .xls .ods
������� DBF � ������ ��������� - ���� �������
�������� ��������� ������ �����
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
��� �������� ���� � ������������ ��������� ���������� ������� ��� ��������� ��� 
������ ���������. ����� ������� ��������� - ���� "���������", ����� ���� 
"������ ��������� - ������� ��������", ����� ��������� ���� ������������ ���������
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
������� �������������� ����� ����:
    {"ID"     ,"+"  ,  8, 0 } ) // Autoinc - ������������� RDD-�������
    {"VM"     ,"^"  ,  8, 0 } ) // RowVersion - RDD-������� ����������� ������                
    {"FLOG"   ,"L"  ,  1, 0 } ) // Logik                                                     
    {"FNUMBA" ,"N"  , 12, 4 } ) // Numeric                                                   
    {"FNUMBA2","N"  ,  8, 2 } ) // Numeric                                                   
    {"FTEXT"  ,"C"  , 20, 0 } ) // TEXT                                                      
    {"FDATE"  ,"D"  ,  8, 0 } ) // DATE                                                      
    {"TS_0"   ,"="  ,  8, 0 } ) // RDD-TimeStamp ModTime                                     
    {"TS_1"   ,"T"  ,  8, 0 } ) // Time                                                      
    {"TS_2"   ,"@"  ,  8, 0 } ) // TimeStamp                                                 
    {"FMEMO"  ,"M"  , 10, 0 } ) // MEMO                                                      
    {"F_MU"   ,"M:U", 10, 0 } ) // Memo                                                      
    {"F_W"    ,"W"  , 10, 0 } ) // Binary (MEMO)                                             
    {"F_P"    ,"P"  , 10, 0 } ) // Image  (MEMO)                                             
    {"F_CU"   ,"C:U", 20, 0 } ) // nChar  (TEXT)                                             
    {"F_CB"   ,"C:B", 10, 0 } ) // Raw    (TEXT)                                             
    {"F_I"    ,"I"  ,  4, 0 } ) // Integer, ShortInt, LongInt   (Numeric)                    
    {"F_Z"    ,"Z"  ,  8, 0 } ) // Curdouble  (Numeric)                                      
    {"F_B"    ,"B"  ,  8, 0 } ) // Double     (Numeric)                                      
    {"F_Q"    ,"Q"  , 10, 0 } ) // VarCharFox (TEXT)                                         
    {"F_QU"   ,"Q:U", 10, 0 } ) // nVarChar   (TEXT)                                         
    {"F_Y"    ,"Y"  ,  8, 4 } ) // Money      (Numeric)                                      
    {"F_V"    ,"V"  ,  6, 0 } ) // MEMO - Variant 3, 4, 6 or more Variable type Field - Six3 
