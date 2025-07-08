/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2020 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2020 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * ������� ������� � Excel, Open Office � �����: xls/ods
 * ������������� ���������������� ������ TSBcell ��� �������� �������� ������.
 * Export Excel, Open Office spreadsheets to files: xls/ods
 * Using the auxiliary TSBcell class for quick data export.
*/

#define _HMG_OUTLOG

#include "hmg.ch"
#include "TSBrowse.ch"
* ========================================================================
FUNCTION ToExcel8(oBrw, nView, cFile)
   LOCAL hProgress, tTime, bExternXls, aTsb, aXlsParam, aXlsTitle, aImage
   LOCAL nRecno, aXlsFoot, bExtern2

   nRecno := (oBrw:cAlias)->( RecNo() )
   oBrw:GoTop()  // ������� ��� � ������� ������� �������
   DO EVENTS

   tTime      := HB_DATETIME()
   hProgress  := NIL //test.PBar_1.Handle            // ���� ��� ProgressBar �� ������ �����
   aTsb       := myGetTsbContent(oBrw)               // ���������� �������
   aXlsParam  := myExcelParam(oBrw, cFile)           // ��������� ��� ������
   aXlsTitle  := myReportTitle(nView,"XLS","EXCEL")  // ��������� ������
   aXlsFoot   := myReportFoot(nView,aTsb)            // ������ ������
   aImage     := myImageReport()                     // ��������

   // ������� �������� ������� � ������ ��� � ������ ������� �������
   // ������� �������� - ��� �� ������ � �������, �� � ����� � ������
   // ���� ��������� � �������-��������� ������ (bExtern2) ���� �����

/* ? "------- ��������/check -----------"
? "aTsb="     ,aTsb      ; ?v aTsb      ; ?
? "aXlsParam=",aXlsParam ; ?v aXlsParam ; ?
? "aXlsTitle=",aXlsTitle ; ?v aXlsTitle ; ?
? "aXlsFoot=" ,aXlsFoot  ; ?v aXlsFoot  ; ?
? "aImage="   ,aImage    ; ?v aImage    ; ? */

   IF nView == 1
      bExternXls := nil   // ����������� �������� ����� ��� ���������� oSheet
      aImage     := nil   // �� ����� ��������
      bExtern2   := nil   // �� ����� �����
   ELSEIF nView == 2
      // �������� -> Tsb7xlsOle.prg
      bExternXls := {|oSheet,aTsb,aXlsTitle| ExcelOle7Extern( hProgress, oSheet, aTsb, aXlsTitle) }
      bExtern2   := nil   // �� ����� �����
   ELSEIF nView == 3
      // ������� ������������� ��������� ������ -> TsbXlsTuning.prg
      // ����������� �������� ����� ��� ���������� oSheet
      bExternXls := {|oSheet,aTsb,aXlsTitle| ExcelOle7Extern( hProgress, oSheet, aTsb, aXlsTitle) }
      // ����������� �������� ����� ��� ��������������� ���������� oExcel
      bExtern2   := {|oSheet,oExcel,aTsb,nLinecolor| myTuningExternExcel( hProgress, oSheet, oExcel, aTsb, nLinecolor) }
   ENDIF

   // ��� ������� � ������ -> Tsb7xlsOle.prg
   Brw7XlsOle( aTsb, aXlsParam, aXlsTitle, aXlsFoot, aImage, hProgress, bExternXls, bExtern2 )
   TotalTimeExports("Brw7XlsOle(" + HB_NtoS(nView) + ")=", aXlsParam[1], tTime )

   oBrw:Refresh(.T.)
   oBrw:GoToRec( nRecno )
   oBrw:SetFocus()
   DO EVENTS

   RETURN Nil

* ======================================================================
// ��������� ������ ������/����/����/���
FUNCTION myReportTitle(nView,cPrg,cName)
   LOCAL aTitle, cTitle, aFont, aColor, n1, n2, nG, lRus, ao, cRus, cEng
   DEFAULT cPrg := cName := ""

   IF cPrg == "WORD"   ; nG := 6
   ELSE                ; nG := 0
   ENDIF

   ao     := App.Cargo
   lRus   := iif( ao:cLang == "RU", .T., .F. )
   cRus   := "�������� DBF-����� � " + cPrg
   cEng   := "Download DBF file to " + cPrg
   aTitle := {}
   cTitle := iif(lRus, cRus, cEng)
   aFont  := { "Comic Sans MS", 24 - nG, .f. , .f. }
   aColor := IIF(nView==1,{BLACK,WHITE},{RED,YELLOW})  // ����/��� �����
   n1     := 1                                         // ������ ������
   n2     := 0                                         // 0-���������� ������ �� ����� �������
   AADD( aTitle, {n1,n2, cTitle, aFont, aColor, DT_CENTER } )
   AADD( aTitle, {} )  // �������������� ������

   cRus   := "����� OLE-������ " + cName
   cEng   := "via " + cName + " OLE object"
   cTitle := iif(lRus, cRus, cEng)
   aFont  := { "Times New Roman", 20 - nG, .T. , .f. }
   aColor := { BLACK , SILVER }                    // ����/��� �������
   n1     := 1                                     // ������ ������
   n2     := 0                                     // 0-���������� ������ �� ����� �������
   AADD( aTitle, {n1,n2, cTitle, aFont, aColor, DT_CENTER } )
   AADD( aTitle, {n1,n2, cTitle, aFont, aColor, DT_RIGHT  } )
   AADD( aTitle, {} )  // �������������� ������

   /* IF nView == 2  // ��� �������� ������

      aFont  := { "DejaVu Sans Mono", 14 - nG, .f. , .f. }
      n1     := 2     // ������ ������
      n2     := 4     // ���������� ������
      AADD( aTitle, { n1,n2,"Cell color from 91% and more", aFont, {BLACK,HMG_n2RGB(CLR_GREEN) }, DT_LEFT } )
      AADD( aTitle, { n1,n2,"Cell color from 76% to 91%"  , aFont, {BLACK,HMG_n2RGB(CLR_YELLOW)}, DT_LEFT } )
      AADD( aTitle, { n1,n2,"Cell color 51% to 76%"       , aFont, {BLACK,HMG_n2RGB(RGB(0,176,240)) }, DT_LEFT } )
      AADD( aTitle, { n1,n2,"Cell color less than 51%"    , aFont, {BLACK,HMG_n2RGB(CLR_HRED)  }, DT_LEFT } )
      AADD( aTitle, {} )  // �������������� ������

      n1 := 2 ; n2 := 8
      AADD( aTitle, { n1,n2,"Cell color if there is no debt for the second month", aFont, {BLUE,HMG_n2RGB(RGB(0,255,0))}, DT_LEFT } )
      AADD( aTitle, { n1,n2,"Cell color, if there is a debt for the second month", aFont, {BLUE,HMG_n2RGB(CLR_ORANGE)  }, DT_LEFT } )
      AADD( aTitle, {} )  // �������������� ������

   ENDIF */

   RETURN aTitle

* ======================================================================
// ������ ������/�����/�����
FUNCTION myReportFoot(nView,aTsb,cPrg)
   LOCAL aFoot, cFoot, aFont, aColor, n1, n2, nG
   LOCAL nI, aTsbFoot, aTsbHead
   DEFAULT nView := 1, cPrg := ""

   IF cPrg == "WORD"   ; nG := 6
   ELSE                ; nG := 0
   ENDIF

   aTsbHead := aTsb[2]    // ������ ����/���� ����� �������
   aTsbFoot := aTsb[5]    // ������ ����/���� ������� �������
   aFoot := {}
   AADD( aFoot, {} )   // �������������� ������
   AADD( aFoot, {} )   // �������������� ������

   cFoot    := aTsbFoot[3,4]
   aFont    := { "Comic Sans MS", 16 - nG, .T. , .f. }
   aColor   := { BLACK , WHITE }                     // ����/��� �����
   n1       := 3                                     // ������ ������
   n2       := 5                                     // ���������� ������ ��
   //AADD( aFoot, {n1,n2, cFoot, aFont, aColor, DT_LEFT } )
   AADD( aFoot, {} )  // �������������� ������

   nI := 0
   /*  ----- ������ --------
   FOR nI := 7 TO 9
      cFoot  := "Total - " + StrTran(aTsbHead[nI,4], CRLF, " ") + ": " + aTsbFoot[nI,4]
      aFont  := { "Comic Sans MS", 16 - nG, .T.  , .f. }
      aColor := { BLACK ,  WHITE }                    // ����/��� �������
      n1     := 3                                     // ������ ������
      n2     := 9                                     // ���������� ������ ��
      AADD( aFoot, {n1,n2, cFoot, aFont, aColor, DT_LEFT } )
   NEXT
   AADD( aFoot, {} )  // �������������� ������

   cFoot := "The head of the calving" + SPACE(50) + "/Petrov I.I./"
   aFont  := { "Arial Black", 16 - nG, .T. , .T. }
   aColor := { BLACK ,  WHITE }
   AADD( aFoot, {2,-1, cFoot, aFont, aColor, DT_LEFT } )
   */
   /* IF nView == 2  // ��� �������� ������

      AADD( aFoot, {} )  // �������������� ������
      aFont := { "DejaVu Sans Mono", 14 - nG, .f. , .f. }
      n1    := 2     // ������ ������
      n2    := 8     // ���������� ������ ��
      AADD( aFoot, { n1,n2,"Test color foot - Cell color from 91% and more", aFont, {BLACK,HMG_n2RGB(CLR_GREEN) }, DT_LEFT } )
      AADD( aFoot, { n1,n2,"Test color foot - Cell color from 76% to 91%"  , aFont, {BLACK,HMG_n2RGB(CLR_YELLOW)}, DT_LEFT } )
      AADD( aFoot, { n1,n2,"Test color foot - Cell color 51% to 76%"       , aFont, {BLACK,HMG_n2RGB(RGB(0,176,240)) }, DT_LEFT } )
      AADD( aFoot, { n1,n2,"Test color foot - Cell color less than 51%"    , aFont, {BLACK,HMG_n2RGB(CLR_HRED)  }, DT_LEFT } )
      AADD( aFoot, {} )  // �������������� ������

   ENDIF */

   RETURN aFoot

* ======================================================================
STATIC FUNCTION myExcelParam(oBrw,cFile)
   LOCAL cPath, cXlsFile, aXlsFont, lActivate, lSave, cMaska, cMsg, cNFile
   LOCAL nWidthTsb
   DEFAULT cFile := ""
   cPath     := GetStartUpFolder() + "\"        // ���� ������ �����
   cMaska    := "zTest_7XlsOle"                 // ������ �����
   cXlsFile  := cPath + cMaska + "_" + CharRepl( ".", DTOC( DATE() ), "_" ) + ".xls"
   cXlsFile  := IIF( LEN(cFile) == 0, cXlsFile, cFile )
   cNFile    := hb_FNameName(cXlsFile)          // ������� .xls - �� ����
   cNFile    := CharRepl('.',cNFile,"_")        // '.'  - ������
   cNFile    += ".xls"
   cXlsFile  := cPath + cNFile
   cXlsFile  := GetFileNameMaskNum(cXlsFile)    // �������� ����� ��� �����
   cNFile    := hb_FNameName(cXlsFile)          // ������� .xls
   cXlsFile  := cPath + cNFile
   lActivate := .T.                             // ������� Excel
   lSave     := .T.                             // ��������� ����
   nWidthTsb := oBrw:GetAllColsWidth()          // ������ ���� ������� ������� (�������)
   aXlsFont  := {"DejaVu Sans Mono", 9 }        // ������ ���� ������� ��� Excel
                                                // ��� �����-������ ��������
                                                // ��� �������� �������� ���� �������
                                                // � ����� �������
   // ��������� ��� ����� �� ���������� �����
   // � ������ ������� ���������� ����� � ����� ����� Excel ����� "��������" ��� �����
   IF AtNum( ".", HB_FNameName( cXlsFile ) ) > 0
      cMsg := ProcNL(0) + ';' + ProcNL(1) + ';;'
      cMsg += 'Output File Name - "' + HB_FNameName( cXlsFile ) + '";'
      cMsg += 'contains several signs dot !;'
      cMsg += 'Excel can "truncate" the file name !;;'
      cMsg := AtRepl( ";", cMsg, CRLF )
      AlertStop( cMsg, "Error", "ZZZ_B_STOP64", 64, {RED} )
   ENDIF

   RETURN { cXlsFile, lActivate, lSave, aXlsFont }

* ======================================================================
FUNCTION ToCalc8(oBrw, nView, cFile)
   LOCAL hProgress, tTime, bExternCalc, aTsb, aCalcParam, aCalcTitle, aImage
   LOCAL nRecno, aCalcFoot

   nRecno := (oBrw:cAlias)->( RecNo() )
   oBrw:GoTop()  // ������� ��� � ������� ������� �������
   DO EVENTS
   // ������ ������� �� ������ ������� c ��������� ������
   oBrw:HideColumns( 31, .t.)
   oBrw:HideColumns( 32, .t.)

   tTime      := HB_DATETIME()
   hProgress  := NIL //test.PBar_1.Handle             // ���� ��� ProgressBar �� ������ �����
   aTsb       := myGetTsbContent(oBrw)                // ���������� �������
   aCalcParam := myCalcParam(cFile)                   // ��������� ��� Calc
   aCalcTitle := myReportTitle(nView,"ODS","CALC")    // ��������� Calc
   aCalcFoot  := myReportFoot(nView,aTsb)             // ������ Calc
   aImage     := myImageReport()                      // ��������

? "+++++++++++++++"
? HB_ValToExp(aCalcTitle)
?v  aCalcTitle
? "+++++++++++++++"
? HB_ValToExp(aCalcFoot)
?v aCalcFoot
? "+++++++++++++++"

   // ������� �������� ������� � ������ ��� � ������ ������� �������
   // ������� �������� - ��� �� ������ � �������, �� � ����� � ������
/*
? "------- ��������/check -----------" + ProcNL()
? "aTsb="     ,aTsb      ; ?v aTsb      ; ?
? "aCalcParam=",aCalcParam ; ?v aCalcParam ; ?
? "aCalcTitle=",aCalcTitle ; ?v aCalcTitle ; ?
? "aCalcFoot=" ,aCalcFoot  ; ?v aCalcFoot  ; ?
? "aImage="   ,aImage    ; ?v aImage    ; ?
*/
   IF nView == 1
      bExternCalc := nil   // ����������� �������� ����� ��� ���������� oSheet
      aImage      := nil   // �� ����� ��������
   ELSEIF nView == 2
      bExternCalc := {|oSheet,aTsb,aCalcTitle| CalcOle7Extern( hProgress, oSheet, aTsb, aCalcTitle) }
   ENDIF

   Brw7OleCalc( aTsb, aCalcParam, aCalcTitle, aCalcFoot, aImage, hProgress, bExternCalc )
   TotalTimeExports("Brw7OleCalc(" + HB_NtoS(nView) + ")=", aCalcParam[1], tTime )

   // ������������ ������� �� ������ �������
   oBrw:HideColumns( 31, .f.)
   oBrw:HideColumns( 32, .f.)

   oBrw:Refresh(.T.)
   oBrw:GoToRec( nRecno )
   oBrw:SetFocus()
   DO EVENTS

   RETURN Nil

* ======================================================================
STATIC FUNCTION myCalcParam(cFileNew)
   LOCAL cPath, cFile, aFont, lActivate, lSave, cMaska
   DEFAULT cFileNew := ""

   cPath     := GetStartUpFolder() + "\"        // ���� ������ �����
   cMaska    := "zTest_7Calc"                   // ������ �����
   cFile     := cPath + cMaska + "_" + CharRepl( ".", DTOC( DATE() ), "_" ) + ".ods"
   cFile     := IIF( LEN(cFileNew) == 0, cFile, cFileNew )
   cFile     := GetFileNameMaskNum(cFile)       // �������� ����� ��� �����
   lActivate := .T.                             // ������� Calc
   lSave     := .T.                             // ��������� ����
   aFont     := {"DejaVu Sans Mono", 10 }       // ������ ���� ������� ��� Calc
                                                // ��� �����-������ ��������
                                                // ��� �������� �������� ���� �������
                                                // � ����� �������
   RETURN { cFile, lActivate, lSave, aFont }

* ======================================================================
FUNCTION TotalTimeExports( cMsg, cFile, tTime )
   ? "=> " + cMsg + " " + cFile
   ? "  Total time spent on exports - " + HMG_TimeMS( tTime )
   ? "  ."
   RETURN NIL

