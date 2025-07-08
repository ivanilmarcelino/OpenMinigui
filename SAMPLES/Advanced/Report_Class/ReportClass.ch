//  ReportClass
//  E:\Appl\Harbour_HMG\Report_Class_HMG\
//  Anand K Gupta  Sun, 19 Mar 2017

#define CRLF    chr(13) + chr(10)

#xtranslate ifnil(<param>,<value>) =>  IIF( <param> == NIL, <value> , <param> )

#xtranslate @ <r>,<c> SAY <s> => gDevPos( <r>,<c> ) ; gDevOut( <s> )

