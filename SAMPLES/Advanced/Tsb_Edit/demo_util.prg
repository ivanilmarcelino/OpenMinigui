/*                                                         
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
*/
/*
  See example from folder \SAMPLES\BASIC\ExFldTps
  Harbour extended Field Types 
  Type Short
  Code Name     Width (Bytes)     Description
  ---- -------  ----------------- -------------------------------------------------------------------
   D   Date     3, 4 or 8          Date
   M   Memo     4 or 8             Memo
   +   AutoInc  4                  Auto increment
   =   ModTime  8                  Last modified date & time of this record
   ^   RowVers  8                  Row version number; modification count of this record
   @   DayTime  8                  Date & Time
   I   Integer  1, 2, 3, 4 or 8    Signed Integer ( Width :  )" },;
   T   Time     4 or 8             Only time (if width is 4 ) or Date & Time (if width is 8 ) (?)
   V   Variant  3, 4, 6 or more    Variable type Field
   Y   Currency 8                  64 bit integer with implied 4 decimal
   B   Double   8                  Floating point / 64 bit binary

ChangeLog.txt 2009-11-25 19:10 UTC+0200 Mindaugas Kavaliauskas (dbtopas/at/dbtopas.lt)
  * harbour/contrib/rddads/ads1.c
    * changed field type mapping of HB_FT_* define, ADS_* define,
      DBCREATE()/DBSTRUCT() array type member. Now we have mapping
      compatible to DBF files as much as possible.

    ; INCOMPATIBILITIES to old ads1.c:
      * ADS_VARCHAR is mapped to HB_FT_VARLENGTH instead of old mapping
        to HB_FT_MEMO. HB_FT_MEMO has a fixed size in DBF (ex., 10),
        but ADS_VARCHAR is variable length field, so, HB_FT_VARLENGTH
        much more correct
      * fieldinfo method (DBSTRUCT() at .prg level) returns one byte
        field type if possible. The exceptions are RAW, CICHARACTER,
        and VARBINARY fields. So, you'll get a DBF compatible type P
        instead of IMAGE, B instead of DOUBLE, T instead of TIME, etc.
      Other field mappings are extension to existing code and should
      not be seen as incompatibility to end used.

    ; Current field type mappings are:
        C; Character,n     HB_FT_STRING,n                      ADS_STRING
        N; Numeric,n,d     HB_FT_LONG,n,d                      ADS_NUMERIC
        D; Date,n          HB_FT_DATE,3 or 4 or 8              ADS_COMPACTDATE; ADS_DATE
        ShortDate          HB_FT_DATE,3                        ADS_COMPACTDATE
        L; Logical         HB_FT_LOGICAL,1                     ADS_LOGICAL
        M; Memo,n          HB_FT_MEMO,4 or 9 or 8              ADS_MEMO
        B; Double,,d       HB_FT_DOUBLE,8,d                    ADS_DOUBLE
        I; Integer,n       HB_FT_INTEGER, 2 or 4 or 8          ADS_SHORTINT; ADS_INTEGER; ADS_LONGLONG
        ShortInt           HB_FT_INTEGER,2                     ADS_SHORTINT
        Longlong           HB_FT_INTEGER,8                     ADS_LONGLONG
        P; Image           HB_FT_IMAGE,9 or 10                 ADS_IMAGE
        W; Binary          HB_FT_BLOB,4 or 9 or 10             ADS_BINARY
        Y; Money           HB_FT_CURRENCY,8,4                  ADS_MONEY
        Z; CurDouble,,d    HB_FT_CURDOUBLE,8,d                 ADS_CURDOUBLE
        T,4; Time          HB_FT_TIME,4                        ADS_TIME
        @; T,8; TimeStamp  HB_FT_TIMESTAMP,8                   ADS_TIMESTAMP
        +; AutoInc         HB_FT_AUTOINC,4                     ADS_AUTOINC
        ^; RowVersion      HB_FT_ROWVER,8                      ADS_ROWVERSION
        =; ModTime         HB_FT_MODTIME,8                     ADS_MODTIME
        Raw,n              HB_FT_STRING,n (+HB_FF_BINARY)      ADS_RAW
        Q; VarChar,n       HB_FT_VARLENGTH,n                   ADS_VARCHAR; ADS_VARCHAR_FOX
        VarBinary,n        HB_FT_VARLENGTH,n (+HB_FF_BINARY)   ADS_VARBINARY_FOX; ADS_RAW
        CICharacter,n      HB_FT_STRING,n                      ADS_CISTRING

    ; ADS help has many ambiguities. Here is the most important I found:
        * Both ADS_VARCHAR and ADS_VARCHAR_FOX defines exists, but
          VarChar filed type is documented only for VFP DBFs. Does
          commom DBF support this field type?
        * It is not clear what ADS_* define corresponds to VarBinary
          field. Both ADS_VARBINARY_FOX and ADS_RAW does not sound
          to be a good choice.
        * ADS documents field types CharacterNoCPTrans, MemoNoCPTrans,
          VarCharNoCPTrans available in VFP tables, but no corresponding
          ADS_* defines exist. These type are not supported by RDDDADS.
        * Autoincrement field type in DBF is called "Autoinc", in ADT
          "Autoicrement". Test shows that both name are acceptedt by
          AdsCreateTable().
        * ADS_LONGLONG define exists, but LomgLong field type is not
          documented at all. It is not clear how to pass field type to
          AdsCreateTable(): "Integer,8" or "LongLong". Test required!
        * Can extended DBF types like ShortDate and Image be used with
          VFP DBF files? (Some other field typess are defined twice
          both in extended DBF and VFP DBF)
        * Documented field type ShortInteger generates 5012 ADS error on
          AdsCreateTable(). A simple workaround used: "ShortInt" passed
          instead of documented field name.

    ! Some ANSI <-> OEM translations moved under ADS_USE_OEM_TRANSLATION
      switch (the other translations was already there)
*/                                     
#define _HMG_OUTLOG
#include "minigui.ch"
///////////////////////////////////////////////////////////////////
FUNCTION myOpenDbf(cFile)
   LOCAL nI, n1, n2, aDbf := {}
 
   AADD( aDbf , {"ID"     ,"+"  ,  8, 0 } ) // Autoinc - автоинкремент RDD-версия
   AADD( aDbf , {"VM"     ,"^"  ,  8, 0 } ) // RowVersion - RDD-версия модификации записи
   AADD( aDbf , {"FLOG"   ,"L"  ,  1, 0 } ) // Logik 
   AADD( aDbf , {"FNUMBA" ,"N"  , 12, 4 } ) // Numeric
   AADD( aDbf , {"FNUMBA2","N"  ,  8, 2 } ) // Numeric
   AADD( aDbf , {"FTEXT"  ,"C"  , 20, 0 } ) // TEXT
   AADD( aDbf , {"FDATE"  ,"D"  ,  8, 0 } ) // DATE
   AADD( aDbf , {"TS_0"   ,"="  ,  8, 0 } ) // RDD-TimeStamp ModTime 
   AADD( aDbf , {"TS_1"   ,"T"  ,  8, 0 } ) // Time     
   AADD( aDbf , {"TS_2"   ,"@"  ,  8, 0 } ) // TimeStamp
   AADD( aDbf , {"FMEMO"  ,"M"  , 10, 0 } ) // MEMO
   AADD( aDbf , {"F_MU"   ,"M:U", 10, 0 } ) // Memo
   AADD( aDbf , {"F_W"    ,"W"  , 10, 0 } ) // Binary (MEMO)
   AADD( aDbf , {"F_P"    ,"P"  , 10, 0 } ) // Image  (MEMO)
   AADD( aDbf , {"F_CU"   ,"C:U", 20, 0 } ) // nChar  (TEXT)
   AADD( aDbf , {"F_CB"   ,"C:B", 10, 0 } ) // Raw    (TEXT)
   AADD( aDbf , {"F_I"    ,"I"  ,  4, 0 } ) // Integer, ShortInt, LongInt   (Numeric)
   AADD( aDbf , {"F_Z"    ,"Z"  ,  8, 0 } ) // Curdouble  (Numeric)
   AADD( aDbf , {"F_B"    ,"B"  ,  8, 0 } ) // Double     (Numeric)
   AADD( aDbf , {"F_Q"    ,"Q"  , 10, 0 } ) // VarCharFox (TEXT)
   AADD( aDbf , {"F_QU"   ,"Q:U", 10, 0 } ) // nVarChar   (TEXT)
   AADD( aDbf , {"F_Y"    ,"Y"  ,  8, 4 } ) // Money      (Numeric)
   AADD( aDbf , {"F_V"    ,"V"  ,  6, 0 } ) // MEMO - Variant 3, 4, 6 or more Variable type Field - Six3

   IF !FILE(cFile)
      DBCreate( cFile, aDbf , "DBFCDX")
      USE ( cFile ) ALIAS TEMP EXCLUSIVE NEW

      FOR nI := 1 TO 21 

          n1 := hb_RandomInt( 80 )
          n2 := hb_RandomInt( 50 )

          APPEND BLANK  
          //TEMPDBF->ID := nI в поле-автоинкремент записывать нельзя
          TEMP->FDATE   := IIF( nI % 4 == 0, CTOD(""), Date() - (n2 + n1) )
          TEMP->FNUMBA  := IIF( nI % 2 == 0, n2 * n1 * 2, ( n2 + n1 )*10 )
          TEMP->FNUMBA2 := IIF( nI % 4 == 0, n2 * n1, ( n2 + n1 ) )
          TEMP->FLOG    := IIF( nI % 3  == 0, TRUE, FALSE )
          TEMP->TS_1    := hb_DateTime() - n1 * 100  
          TEMP->TS_2    := hb_DateTime() - n2 * 100  

          TEMP->FTEXT   := "Text Recno: " + HB_NtoS(nI) + IIF( nI % 3  == 0, CRLF + "2 stroka", "" ) 
          TEMP->F_CU    := "Text Recno: " + HB_NtoS(nI)
          TEMP->F_CB    := "Text Recno: " + HB_NtoS(nI)

          TEMP->FMEMO   := "Memo field - Recno: " + HB_NtoS(nI)
          TEMP->F_MU    := "Memo field - Recno: " + HB_NtoS(nI)  // nMemo  
          TEMP->F_W     := "Memo field - Recno: " + HB_NtoS(nI)  // Binary 
          TEMP->F_P     := "Memo field - Recno: " + HB_NtoS(nI)  // Image  

          TEMP->F_I     := IIF( nI % 2 == 0, n2 * n1 * 2, ( n2 + n1 )*10 )
          TEMP->F_B     := IIF( nI % 2 == 0, n2 * n1 * 2, ( n2 + n1 )*10 )
          TEMP->F_Y     := IIF( nI % 2 == 0, n1 * .0001, n2 * .0001 )
          TEMP->F_V     := "Type [V] field - Recno: " + HB_NtoS(nI) + CRLF + REPL("a",255)

          IF nI % 3 == 0
             TEMP->FNUMBA := -1 * TEMP->FNUMBA
             TEMP->TS_1   := hb_CToT("") 
          ENDIF

          IF nI % 5 == 0
             TEMP->TS_2 := hb_CToT("") 
             //DbDelete()
          ENDIF

      NEXT
      TEMP->( DbCloseArea() )

   ENDIF

   USE ( cFile ) ALIAS TEST EXCLUSIVE NEW

RETURN ALIAS()

///////////////////////////////////////////////////////////////////////////////
FUNCTION GetTxtWidth( cText, nFontSize, cFontName, lBold )  // получить Width текста
   LOCAL hFont, nWidth
   DEFAULT cText     := REPL('A', 2)        ,  ;
           cFontName := _HMG_DefaultFontName,  ;   // из MiniGUI.Init()
           nFontSize := _HMG_DefaultFontSize,  ;   // из MiniGUI.Init()
           lBold     := .F.

   IF Valtype(cText) == 'N'
      cText := repl('A', cText)
   ENDIF

   hFont  := InitFont(cFontName, nFontSize, lBold)
   nWidth := GetTextWidth(0, cText, hFont)         // ширина текста
   DeleteObject (hFont)

   RETURN nWidth

////////////////////////////////////////////////////////////////
FUNCTION ProcNL(nVal, cMsg)
   Default cMsg := ">>> "
   nVal := iif( Empty(nVal), 0, nVal ) + 1
   cMsg += ProcName(nVal) + "(" + hb_ntos( ProcLine(nVal) ) + ")"
   cMsg += " => " + ProcFile(nVal)
RETURN cMsg

//////////////////////////////////////////////////////////////////
FUNCTION _Font2oDlu( cFontName )
   LOCAL i, f := _HMG_DefaultFontName
   LOCAL o, n := _HMG_DefaultFontSize

   IF !Empty( cFontName )
      IF ( i := GetControlIndex( cFontName, "Main" ) ) > 0
         f := _HMG_aControlFontName[ i ]
         n := _HMG_aControlFontSize[ i ]
      ENDIF
   ENDIF

   o := oDlu4Font( n )

   IF f == "Arial Black"
      i := iif( n < 15, 20, iif( n < 20, 30, 40 ) )
      o:nPixWidth    += i
      o:nPixWidthDT  += i
      o:nPixWidthDT1 += i
      o:nPixWidthDT2 += i
   ENDIF

RETURN o

