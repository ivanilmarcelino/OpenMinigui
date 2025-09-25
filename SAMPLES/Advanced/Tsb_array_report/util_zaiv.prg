/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
*/
#include "MiniGUI.ch"
#include "Dbinfo.ch"

/////////////////////////////////////////////////////////////////////////////////
FUNCTION CreateDbf(cFDbf, cAls, nRec, a2Mast, aVipZa)
   LOCAL aDbf

   aDbf := StructDbf()
   DBCreate( cFDbf, aDbf , "DBFCDX")

   cAls := "TMP_" + cAls
   IF my_Use(cFDbf, , cAls, , , )
      ? SPACE(5) + "Create base: " + cAls, "lUse=", Used(), Alias()
      DBF_WrtValue( cAls, nRec, a2Mast, aVipZa )
      (cAls)->(dbCloseArea())
   ENDIF

RETURN Nil

/////////////////////////////////////////////////////////////////////////
FUNCTION StructDbf()
   LOCAL aDbf := {}

   AADD( aDbf , {"ID"      ,"+", 8, 0 } ) // auto-increment
   AADD( aDbf , {"TS"      ,"=", 8, 0 } ) // RDD-TimeStamp date+time+milliseconds
   AADD( aDbf , {"VM"      ,"^", 8, 0 } ) // RDD-version of record modification
   AADD( aDbf , {"IM"      ,"@", 8, 0 } ) // TimeStamp, fill in yourself
   // list of fields in which the latest actions will be written - user's
   AADD( aDbf , {"DT_USER" ,"N", 3, 0 } ) // user's code
   // list of fields in which the addition/deletion/restoration of a record will be written
   AADD( aDbf , {"DT_ADD"  ,"T", 8, 0 } ) // RDD-TimeStamp date+time+milliseconds
   AADD( aDbf , {"DT_DEL"  ,"T", 8, 0 } ) // RDD-TimeStamp date+time+milliseconds
   AADD( aDbf , {"DT_REST" ,"T", 8, 0 } ) // RDD-TimeStamp date+time+milliseconds
   //
   AADD( aDbf , {"FTEXT"   ,"C", 60, 0 } )  
   AADD( aDbf , {"KCITY"   ,"N",  6, 0 } )  
   AADD( aDbf , {"KSTREET" ,"N",  6, 0 } )  
   AADD( aDbf , {"HOUSE"   ,"C", 15, 0 } )  
   AADD( aDbf , {"Korpus"  ,"C", 15, 0 } )  
   AADD( aDbf , {"Stroen"  ,"C", 15, 0 } )  
   AADD( aDbf , {"Podyezd" ,"C", 15, 0 } )  
   AADD( aDbf , {"Apartmen","C", 15, 0 } )  
   AADD( aDbf , {"MARK"    ,"L",  1, 0 } )  
   AADD( aDbf , {"PRINT"   ,"L",  1, 0 } )  
   AADD( aDbf , {"SAVE"    ,"L",  1, 0 } )  
   AADD( aDbf , {"MAIL"    ,"L",  1, 0 } )  
   AADD( aDbf , {"REM"     ,"C",500, 0 } )  
   //
   AADD( aDbf , {"DATEZA"  ,"D",  8, 0 } )  
   AADD( aDbf , {"NNZA"    ,"N",  8, 0 } )  
   AADD( aDbf , {"KMASTER" ,"N",  3, 0 } )  
   AADD( aDbf , {"KVIPZA"  ,"N",  2, 0 } )  
   AADD( aDbf , {"DateSrok","D",  8, 0 } )  

Return aDbf

/////////////////////////////////////////////////////////////////////////
FUNCTION DBF_WrtValue( cAls, nRec, a2Mast, aVipZa )
   LOCAL nC, nI, n1, n2, n3, n4, d1, nY, aKMast
   LOCAL aChar[125-48], aChar2[90-40]

   aKMast := {}   // коды мастеров
   FOR nI := 1 TO LEN(a2Mast)
     AADD( aKMast, a2Mast[nI,1] )
   NEXT

   // execution codes - we will add 
   AADD( aVipZa, 0 )
   AADD( aVipZa, 8 )
   AADD( aVipZa, 9 )

   FOR nI := 1 TO LEN(aChar)
       aChar[nI] := CHR(nI+47)
   NEXT
   FOR nI := 1 TO LEN(aChar2)
       aChar2[nI] := CHR(nI+64)
   NEXT
   nC := 1

   FOR nI := 1 TO nRec 

       n1 := hb_RandomInt( 80 )
       n2 := hb_RandomInt( 50 )
       n3 := hb_RandomInt( LEN(aKMast) )
       n4 := hb_RandomInt( LEN(aVipZa) )
       nY := IIF( nI > 30000, 2025, 2024 )
       d1 := Date() - (n2 + n1) 
       d1 := CTOD( SUBSTR(DTOC(d1),1,6) + HB_NtoS(nY) )

       APPEND BLANK  
       //cAls->ID      := nI you can't write to the autoincrement field
       (cAls)->FTEXT   := REPLICATE(aChar[nC], 35 ) + " Recno: "+HB_NtoS(nI)+""
       (cAls)->KCITY   := hb_RandomInt( 25 )
       (cAls)->KSTREET := IIF( nI % 3 == 0, n1 , hb_RandomInt( 500 ) )
       (cAls)->MARK    := IIF( nI % 3  == 0, TRUE, FALSE )
       (cAls)->PRINT   := IIF( nI % 8  == 0, TRUE, FALSE )
       (cAls)->SAVE    := IIF( nI % 7  == 0, TRUE, FALSE )
       (cAls)->MAIL    := IIF( nI % 5  == 0, TRUE, FALSE )
       (cAls)->DT_USER := hb_RandomInt( 10 )
       (cAls)->DT_ADD  :=  hb_DateTime() - nI * 100  // date+time of record insertion
       //
       (cAls)->NNZA    := VAL( HB_NtoS(n1) + SUBSTR(DTOC(d1),4,2) )  
       (cAls)->DATEZA  := d1
       (cAls)->DateSrok:= d1 + 5
       (cAls)->KMASTER := aKMast[n3]
       (cAls)->KVIPZA  := aVipZa[n4]

       nC++
       nC := IIF(nC > LEN(aChar),1,nC)

       IF nI % 10 == 0
          DbDelete()
       ENDIF

       dbSkip(0)
       DO EVENTS

   NEXT

Return Nil
