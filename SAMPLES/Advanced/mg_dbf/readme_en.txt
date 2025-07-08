MINIGUI - Harbour Win32 GUI library Demo
Copyright 2024 Sergej Kiselev <bilance@bilance.lv>
Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
View DBF files for Harbour/Clipper/DbaseIV/Foxpro/Six
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Simple viewer of all DBF file formats supported by Harbour
   DBFCDX - memo *.fpt (FoxPro compatible)
   DBFNTX - memo *.dbt (Clipper compatible)
   DBFNSX - memo *.smt (Six compatible)
   SIXCDX - memo *.fpt/smt (FoxPro compatible)
   BMDBFCDX - bitmap filter,compatible with dbfcdx
   BMDBFNTX - bitmap filter,compatible with dbfntx
   BMDBFNSX - bitmap filter,compatible with dbfnsx
   BM_DBSEEKWILD - bitmap filter

All Harbour code tables are included (about 100)
Open files - file name in Russian/Ukrainian
Search/filter by all database fields
Two program languages: Russian and English
Export to .csv .dbf .sqlite .xls .ods
Export DBF to another encoding - Export menu
Downloading the structure of the list of fields
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To open a database in a national encoding, you must specify this encoding when
starting the program. After starting the program - Menu "Settings", then menu
"Start program - code page", then select your national encoding
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Formats of supported database fields:
    {"ID"     ,"+"  ,  8, 0 } ) // Autoinc - autoincrement RDD driver
    {"VM"     ,"^"  ,  8, 0 } ) // RowVersion - RDD Write Modification Driver                
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

