/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2020-2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * ������ � ���� �������� / Working with the export menu
*/

#define _HMG_OUTLOG
#include "minigui.ch"
#include "tsbrowse.ch"
///////////////////////////////////////////////////////////////////////////////
FUNCTION TableToExport(ow,ky,cn,ob)
   LOCAL aMenu, aLang, nBmpSize, nFSize, nChoice, nPos, lExit, cRun, cTable
   LOCAL xRet, cForm, aFntExt, cFunc, cMsg, cMenu, cIcon, cFile, cMaska, cAls

   ? ProcNL(), ow:Name,ky,cn,ob:cAlias,This.&(cn).Caption
   cForm  := ow:Name
   cTable := ow:Cargo:cTable               // ��� �������
   cAls   := ow:Cargo:cAls                 // ��� ������
   cMaska := ow:Cargo:cFile + "." + cAls

   IF App.Cargo:cLang == "RU"
      aLang := { "������� -> " , "������� -> ", "������� -> ", "������� -> " }
   ELSE
      aLang := { "Export -> "  , "Export -> " , "Export -> " , "Export -> "  }
   ENDIF

   aMenu := {}
   cFile := cMaska + ".csv"
   cMenu := cFileNoPath(cFile)
   AADD( aMenu, { "iCsv32", aLang[1]+cMenu         , .F. , "myDbWriteCsv"   , cFile , 1, ow, ob } )    // -> util_dbf.prg
   AADD( aMenu, {                                                                               } )
   cFile := cMaska + ".dbf"
   cMenu := cFileNoPath(cFile)
   AADD( aMenu, { "iDbf32", aLang[2]+cMenu         , .F. , "myDbWriteDbf"   , cFile , 2, ow, ob } )
   AADD( aMenu, {                                                                               } )
   cFile := cMaska + ".xls"
   cMenu := cFileNoPath(cFile)
   AADD( aMenu, { "iXls32", aLang[3]+cMenu+" (OLE)", .F. , "myDbWriteXls"   , cFile , 3, ow, ob } )
   AADD( aMenu, {                                                                               } )
   cFile := cMaska + ".ods"
   cMenu := cFileNoPath(cFile)
   AADD( aMenu, { "iOOCalc32", aLang[4]+cMenu+" (OLE)", .F. , "myDbWriteCalc", cFile , 4, ow, ob } )

   nPos     := 3
   nBmpSize := 32
   nFSize   := 16
   lExit    := .F.
   aFntExt  := { "DejaVu Sans Mono", "Comic Sans MS" }
   nChoice  := DynamicContextMenuExtend( cForm, aMenu, nPos, nBmpSize, nFSize, lExit, aFntExt, "Icon" )
   IF nChoice > 0
      nPos  := aMenu[nChoice,6]
      cMsg  := aLang[nPos]
      cFunc := aMenu[nChoice,4]
      cRun  := cFunc + '("' + cMsg + '")' //+ HB_ValToExp(aMenu[nChoice]) + ')'
      cFile := aMenu[nChoice,5]
      cIcon := aMenu[nChoice,1]
      IF nPos == 1
         xRet := myDbWriteCsv(cFile, cIcon, ow, ob )
      ELSEIF nPos == 2
         xRet := myDbWriteDbf(cFile, cIcon, ow, ob )
      ELSEIF nPos == 3
         xRet := myDbWriteXls(cFile, cIcon, ow, ob )
      ELSEIF nPos == 4
         xRet := myDbWriteOOCalc(cFile, cIcon, ow, ob )
      ELSE
         IF MyIsFunNoRun(cFunc)
            xRet := EVal( hb_MacroBlock( cRun ) , cFile )
         ELSE
            xRet := NIL
         ENDIF
      ENDIF
   ENDIF
   DO EVENTS

RETURN xRet

////////////////////////////////////////////////////////////////
FUNCTION myDbWriteCsv(cFile, cIcon, oWnd, oBrw)      // ��������� CSV
   LOCAL cMsg, cTtl, cFCsv, nRec, cStr, cLng, cLn2
   LOCAL oCol, cCol, cTyp, nI, xVal, aRet, cCdPg, lOpen
   LOCAL cSetCP, cLngSel, cSelCdp, cPath

   PUBLIC aPubFileExport

   ? ProcNL(), cFile, oWnd, oWnd:ClassName, oBrw, oBrw:ClassName

   nRec  := oBrw:Cargo:nTotalRecno

   IF App.Cargo:cLang == "RU"
      cTtl := '�������������� � CSV'
      cMsg := "����� ������� � ������� = " + HB_NtoS( nRec )
      cLng := "���� ������� ������!;"
      cLn2 := "������� ���� ���� ?;"
   ELSE
      cTtl := 'Convert to CSV'
      cMsg := "Total records in the table = " + HB_NtoS( nRec )
      cLng := "File created successfully !;"
      cLn2 := "Open this file ?;"
   ENDIF

   oWnd:Cargo:aFileExport := {}                      // ������� �������� ��������
   M->aPubFileExport := {}                           // ������� �������� ��������

   Form_ExportFile(oWnd, cTtl, cMsg, cFile, cIcon)   // �����-�������

   aRet := M->aPubFileExport                         // ������� �������� �������� - ������� 1
   IF LEN(aRet) == 0
      RETURN .F.
   ENDIF
   cFile   := ALLTRIM(aRet[1])
   cCdPg   := aRet[2]
   lOpen   := aRet[3]
   cFCsv   := cFileNoPath( cFile )
   cPath   := cFilePath( cFile )
   cSetCP  := hb_SetCodepage()
   cSelCdp := hb_CdpSelect()
   cLngSel := Hb_LangSelect()

   WaitWindow( { cTtl, cFile }, .T., 800, 13, NIL, BLACK, App.Cargo:aBCAlert, 14, BLUE, 4 ) // open the wait window

   ? ProcNL() ; ? "    ###["+cFile+"]###########"
   nRec := 0
   HB_FileDelete( cFile )

   DBSELECTAREA(oBrw:cAlias)
   GOTO TOP
   DO WHILE !EOF()
      //? "RecNo = "+HB_NtoS(RECNO())
      cStr := ""
      FOR EACH oCol IN oBrw:aColumns
          nI   := hb_EnumIndex(oCol)
          cCol := oCol:cName //oCol:cField
          cTyp := oCol:cFieldTyp
          //? "   nI=",nI, "cCol=", cCol, cTyp
          IF cCol == "SELECTOR" .OR. cCol == "ORDKEYNO"
          ELSE
             xVal := oBrw:GetValue(nI)
             //?? xVal, VALTYPE(xVal)
             IF VALTYPE(xVal) == "U"
                xVal := ""
             ENDIF
             IF cTyp == "C"
                xVal := ALLTRIM(xVal)
             ELSE
                xVal := cValToChar(xVal)
             ENDIF
             cStr += xVal + ";"
          ENDIF
      NEXT
      cStr := hb_Translate( cStr, cSelCdp, cCdPg )
      STRFILE( cStr + CRLF, cFile, .T. )
      DO EVENTS
      dbSkip()
   ENDDO
   dbGoTop()

   WaitWindow()

   IF lOpen
      cMsg := cLng + cFCsv + ";" + cPath + ";;" + cLn2
      IF AlertYesNo(cMsg, , ,cIcon, 64 , {LGREEN,RED} )
         ShellExecute( 0, "Open", cFile,,, 3 )
      ENDIF
   ELSE
      cMsg := cLng + cFCsv + ";" + cPath
      AlertInfo( cMsg, cTtl, cIcon, 64, {WHITE} )
   ENDIF

   RELEASE aPubFileExport

RETURN NIL

////////////////////////////////////////////////////////////////////////////
FUNCTION myDbWriteDbf(cFile, cIcon, oWnd, oBrw )
   LOCAL xRet, cForm, cMsg, cFDbf, cTtl, aSay, nRec, aStruct, cAls, i
   LOCAL uTyp, xVal, nI, cFld, oCol, cTyp, cDbf, cPath
   LOCAL cLng, cLn2, cLn3, aRet, lOpen, cCdPg, cRdd

   ? ProcNL(), cFile, oWnd, oWnd:ClassName, oBrw, oBrw:ClassName
   // �������� ! ������������ ������� ����� ���� ������ 10 ��������, DBF ����� �� ������������ !
   // Attention! Column names can be longer than 10 characters, DBF DOES NOT SUPPORT this !
   aStruct := oBrw:Cargo:aStruct  // {cFld, cTyp, nLen, nDec} - ��������� ���� ��� ��������
      FOR i := 1 TO Len(aStruct)
         IF AT(" ", aStruct[ i ][ 1 ] ) > 0
            aStruct[ i ][ 1 ] := ATREPL( " ", aStruct[ i ][ 1 ], "_" )
         ENDIF
         IF aStruct[ i ][ 2 ] != "N"
            aStruct[ i ][ 4 ] := 0
         ELSEIF aStruct[ i ][ 2 ] == "N"
            aStruct[ i ][ 4 ] := Set( _SET_DECIMALS )
         ENDIF
         IF aStruct[ i ][ 2 ] == "I"
            aStruct[ i ][ 2 ] := "N"
            aStruct[ i ][ 3 ] += 2
            aStruct[ i ][ 4 ] := 0
         ENDIF
         IF aStruct[ i ][ 2 ] == "V"
            aStruct[ i ][ 2 ] := "M"
            aStruct[ i ][ 3 ] := 10
            aStruct[ i ][ 4 ] := 0
         ENDIF
      NEXT
   //?v aStruct
   cForm     := oWnd:Name
   nRec      := oBrw:Cargo:nTotalRecno

   IF App.Cargo:cLang == "RU"
      cTtl := '�������������� � DBF'
      cMsg := "����� ������� � ������� = " + HB_NtoS( nRec )
      cLng := "���� ������� ������!;"
      cLn2 := "������� ���� ���� ?;"
      cLn3 := "���� �� ������ !;"
   ELSE
      cTtl := 'Convert to DBF'
      cMsg := "Total records in the table = " + HB_NtoS( nRec )
      cLng := "File created successfully !;"
      cLn2 := "Open this file ?;"
      cLn3 := "File NOT created !;"
   ENDIF

   oWnd:Cargo:aFileExport := {}                      // ������� �������� ��������

   Form_ExportFile(oWnd, cTtl, cMsg, cFile, cIcon)   // �����-�������

   aRet := oWnd:Cargo:aFileExport
   IF LEN(aRet) == 0
      RETURN .F.
   ENDIF
   cFile := aRet[1]
   cCdPg := aRet[2]
   lOpen := aRet[3]
   cPath := cFilePath(cFile)
   cRdd  := "DBFCDX"
   aSay  := { cTtl + ' ...', cFile }
   WaitWindow( aSay, .T., 800, 13, NIL, BLACK, App.Cargo:aBCAlert, 14, BLUE, 4 ) // open the wait window

   HB_FileDelete( cFile )
   dbCreate(cFile, aStruct, cRdd)
   cAls := "TEMP"
   //------------- �������/������� dbf ��� ������ ------------
   USE ( cFile ) VIA "DBFCDX" ALIAS &cAls NEW CODEPAGE cCdPg SHARED

   oBrw:GoTop()
   oBrw:lEnabled := .F.  // ����������� ������� ������� (������ �� ������������)

   DBSELECTAREA(oBrw:cAlias)
   dbGoTop()

   DO WHILE !EOF()
      DBSELECTAREA(cAls)
      APPEND BLANK
      FOR EACH oCol IN oBrw:aColumns
          nI   := hb_EnumIndex(oCol)
          cFld := oCol:cName //oCol:cField
          cTyp := oCol:cFieldTyp
          cDbf := FIELDNAME( FIELDPOS(cFld) )
          IF EMPTY(cDbf)
             cDbf := ATREPL( " ", SUBSTR(cFld, 1, 10), "_" )
          ENDIF
          //? nI, "cFld=", cFld, cTyp, "cDbf=", cDbf
          IF cFld == "SELECTOR" .OR. cFld == "ORDKEYNO"
          ELSE
             xVal := oBrw:GetValue(cFld)
             //?? "[", xVal, "]"
             uTyp := FIELDTYPE( FIELDPOS(cFld) )
             //?? uTyp
             IF VALTYPE(xVal) == "U"
                IF uTyp == "C"
                   xVal := ""
                ELSEIF uTyp == "M"
                   xVal := ""
                ELSEIF uTyp == "N"
                   xVal := 0
                ELSEIF uTyp == "L"
                   xVal := .F.
                ELSEIF uTyp == "D"
                   xVal := CTOD("")
                ELSEIF uTyp == "T"
                   xVal := hb_CToT("")
                ENDIF
             ENDIF
             IF ( uTyp == "T" .OR. uTyp == "@" ).AND. VALTYPE(xVal) == "C"
                (cAls)->&cDbf := HB_STOT(xVal)
             ELSE
                (cAls)->&cDbf := xVal
             ENDIF
          ENDIF
      NEXT
      DO EVENTS
      DBSELECTAREA(oBrw:cAlias)
      dbSkip()
   ENDDO
   dbGoTop()

   oBrw:lEnabled := .T.    // �������������� ������� ������� (������ ������������)
   oBrw:Display()
   oBrw:Refresh(.T.)       // ������������ ������ � �������
   oBrw:GoTop()
   oBrw:SetFocus()
   DO EVENTS

   (cAls)->( DbCloseArea() )  // ������� dbf

   WaitWindow()

   cFDbf := cFileNoPath(cFile)
   IF FILE(cFile)
      IF lOpen
         cMsg := cLng + cFDbf + ";" + cPath + ";;" + cLn2
         IF AlertYesNo(cMsg, , ,cIcon, 64 , {LGREEN,RED} )
            ShellExecute( 0, "Open", cFile,,, 3 )
         ENDIF
      ELSE
         cMsg := cLng + cFDbf
         AlertInfo( cMsg, cTtl, cIcon, 64, {WHITE} )
      ENDIF
   ELSE
      cMsg := cLn3 + cFDbf + ";" + cPath
      AlertStop( cMsg, cTtl, "ZZZ_B_STOP64", 64 )
   ENDIF

RETURN xRet

////////////////////////////////////////////////////////////////////////////////////
FUNCTION myDbWriteXls(cFile, cIcon, oWnd, oBrw )
   LOCAL xRet, cForm, cMsg, cFXls, cTtl, aSay, nRec, cPath
   LOCAL lActivate, lSave, cTtlXls, aTitle, hFont, bExtrnXls, aColSel, bPrintRow
   LOCAL cLng, cLn2, cLn3, aRet, lOpen, cCdPg

   ? ProcNL(), cFile, oWnd, oWnd:ClassName, oBrw, oBrw:ClassName
   // �������� ! ��������� ������ 65533 ����� � Excel ������ ! ����������� Excel 2003.
   // Attention ! Upload more than 65533 rows in Excel is NOT possible ! Excel 2003 Restriction.

   cForm     := oWnd:Name
   nRec      := oBrw:Cargo:nTotalRecno
   lActivate := .F.                                         // ������� Excel
   lSave     := .T.                                         // ��������� ����
   cTtlXls   := "_" + Space(20) + "Convert to XLS"
   aTitle    := { cTtlXls, GetFontHandle( "ComSanMS" ) }    // ����� �� ����� ������
   hFont     := GetFontHandle( "Normal" )                   // ������� ���� ���� ��� Excel
   bExtrnXls := nil  // ����������� �������� ����� ��� ���������� oSheet � ������ Tsbrowse
   aColSel   := nil  // ���������� �� �������� �������� (������ �������) ����� � �������
   //aColSel := { 1,2,3,4,5,6,7,8,9,10 } // ������ ������� �������
   bPrintRow := nil  // ���� ���� �� ������ ������, ���������� T/F - ���� .F. ���������� ������

   IF App.Cargo:cLang == "RU"
      cTtl := '�������������� � XLS'
      cMsg := "����� ������� � ������� = " + HB_NtoS( nRec )
      cLng := "���� ������� ������!;"
      cLn2 := "������� ���� ���� ?;"
      cLn3 := "���� �� ������ !;"
   ELSE
      cTtl := 'Convert to XLS'
      cMsg := "Total records in the table = " + HB_NtoS( nRec )
      cLng := "File created successfully !;"
      cLn2 := "Open this file ?;"
      cLn3 := "File NOT created !;"
   ENDIF

   oWnd:Cargo:aFileExport := {}                      // ������� �������� ��������

   Form_ExportFile(oWnd, cTtl, cMsg, cFile, cIcon, .F.)   // �����-������� .F.-��� ����� ���������

   aRet := oWnd:Cargo:aFileExport
   IF LEN(aRet) == 0
      RETURN .F.
   ENDIF
   cFile := aRet[1]
   cCdPg := aRet[2]
   lOpen := aRet[3]

   cPath := cFilePath(cFile) + "\"                      // ���� � �����
   cFXls := hb_FNameName(cFile)                         // .xls - �� ����
   cFXls := CharRepl('.',cFXls,"_")                     // '.'  - ������

   // ��������� ��� ����� �� ���������� �����
   IF AtNum( ".", cFXls ) > 0
      IF App.Cargo:cLang == "RU"
         cMsg := '��� ��������� �����;' + cFXls + ';'
         cMsg += '�������� ��������� ������ ����� "." !;'
         cMsg += 'Excel ����� "��������" ��� ����� !;;'
      ELSE
         cMsg := 'Output File Name;' + cFXls + ';'
         cMsg += 'contains several signs dot "." !;'
         cMsg += 'Excel can "truncate" the file name !;;'
      ENDIF
      cMsg += ProcNL()
      AlertStop( cMsg, cTtl, "ZZZ_B_STOP64", 64 )
   ENDIF

   aSay := { cTtl + ' ...', cFile }
   WaitWindow( aSay, .T., 800, 13, NIL, BLACK, App.Cargo:aBCAlert, 14, BLUE, 4 ) // open the wait window

   HB_FileDelete( cFXls )
   DBSELECTAREA(oBrw:cAlias)
   dbGoTop()

   oBrw:GoTop()
   oBrw:lEnabled := .F.  // ����������� ������� ������� (������ �� ������������)

   oBrw:ExcelOle( cPath + cFXls, lActivate, , aTitle, hFont, lSave, bExtrnXls, aColSel, bPrintRow )

   oBrw:lEnabled := .T.    // �������������� ������� ������� (������ ������������)
   oBrw:Display()
   oBrw:Refresh(.T.)       // ������������ ������ � �������
   oBrw:GoTop()
   oBrw:SetFocus()
   DO EVENTS

   WaitWindow()

   cFile := cPath + cFXls + ".xls"
   cFXls := cFileNoPath(cFile)

   IF FILE(cFile)
      IF lOpen
         cMsg := cLng + cFXls + ";" + cPath + ";;" + cLn2
         IF AlertYesNo(cMsg, , ,cIcon, 64 , {LGREEN,RED} )
            ShellExecute( 0, "Open", cFile,,, 3 )
         ENDIF
      ELSE
         cMsg := cLng + cFXls + ";" + cPath
         AlertInfo( cMsg, cTtl, cIcon, 64, {WHITE} )
      ENDIF
   ELSE
      cMsg := cLn3 + cFXls
      AlertStop( cMsg, cTtl, "ZZZ_B_STOP64", 64 )
   ENDIF

RETURN xRet

//////////////////////////////////////////////////////////////////////////////////
FUNCTION myDbWriteOOCalc(cFile, cIcon, oWnd, oBrw )
   LOCAL xRet, cForm, cMsg, cFOds, cTtl, aSay, nRec, cPath, aFont, aRet, cTitle
   LOCAL lSave, aTitle, aImage, hProgress, bExternCalc, cLng, cLn2, cLn3, n1, n2
   LOCAL lOpen, cCdPg, aTsb, aCalcParam, aCalcTitle, aCalcFoot, aColor, tTime

   ? ProcNL(), cFile, oWnd, oWnd:ClassName, oBrw, oBrw:ClassName
   cForm  := oWnd:Name
   nRec   := oBrw:Cargo:nTotalRecno

   IF App.Cargo:cLang == "RU"
      cTtl := '�������������� � ODS'
      cMsg := "����� ������� � ������� = " + HB_NtoS( nRec )
      cLng := "���� ������� ������!;"
      cLn2 := "������� ���� ���� ?;"
      cLn3 := "���� �� ������ !;"
   ELSE
      cTtl := 'Convert to ODS'
      cMsg := "Total records in the table = " + HB_NtoS( nRec )
      cLng := "File created successfully !;"
      cLn2 := "Open this file ?;"
      cLn3 := "File NOT created !;"
   ENDIF

   // ���.��������� ��� OO-Calc
   aFont  := {"DejaVu Sans Mono", 11 }                 // ������ ���� ������� ��� Calc
   aTitle := {}
   cTitle := cTtl
   aFont  := { "Comic Sans MS", 24, .f. , .f. }
   aColor := {BLACK,WHITE}                             // ����/��� �����
   n1     := 1                                         // ������ ������
   n2     := 0                                         // 0-���������� ������ �� ����� �������
   AADD( aTitle, {n1,n2, cTitle, aFont, aColor, DT_CENTER } )
   AADD( aTitle, {} )                                  // �������������� ������
   lSave     := .T.                                    // ��������� ����

   oWnd:Cargo:aFileExport := {}                           // ������� �������� ��������

   Form_ExportFile(oWnd, cTtl, cMsg, cFile, cIcon, .F.)   // �����-������� .F.-��� ����� ���������

   aRet := oWnd:Cargo:aFileExport
   IF LEN(aRet) == 0
      RETURN .F.
   ENDIF
   cFile := aRet[1]
   cCdPg := aRet[2]
   lOpen := aRet[3]    // ������� ����� ������
   HB_FileDelete( cFile )

   aSay := { cTtl + ' ...', cFile }
   WaitWindow( aSay, .T., 800, 13, NIL, BLACK, App.Cargo:aBCAlert, 14, BLUE, 4 ) // open the wait window

   tTime       := HB_DATETIME()
   aTsb        := myGetTsbContent(oBrw)                // ���������� �������
   aCalcParam  := { cFile, .F., lSave, aFont }         // ��������� ��� Calc
   aCalcTitle  := aTitle                               // ��������� Calc
   aCalcFoot   := {}                                   // ������ Calc
   aImage      := {}
   hProgress   := NIL         // ���� ��� ProgressBar �� ������ �����
   bExternCalc := NIL         // ��� ��������-2

   DBSELECTAREA(oBrw:cAlias)
   oBrw:GoTop()          // ������� ��� � ������� ������� �������
   oBrw:lEnabled := .F.  // ����������� ������� ������� (������ �� ������������)

   Brw7OleCalc( aTsb, aCalcParam, aCalcTitle, aCalcFoot, aImage, hProgress, bExternCalc )

   oBrw:lEnabled := .T.    // �������������� ������� ������� (������ ������������)
   oBrw:Display()
   oBrw:Refresh(.T.)       // ������������ ������ � �������
   oBrw:GoTop()
   oBrw:SetFocus()
   DO EVENTS

   WaitWindow()

   cFOds := cFileNoPath(cFile)
   cPath := cFilePath(cFile)
   IF FILE(cFile)
      IF lOpen
         cMsg := cLng + cFOds + ";" + cPath + ";;" + cLn2
         IF AlertYesNo(cMsg, , ,cIcon, 64 , {LGREEN,RED} )
            ShellExecute( 0, "Open", cFile,,, 3 )
         ENDIF
      ELSE
         cMsg := cLng + cFOds + ";" + cPath
         AlertInfo( cMsg, cTtl, cIcon, 64, {WHITE} )
      ENDIF
   ELSE
      cMsg := cLn3 + cFOds
      AlertStop( cMsg, cTtl, "ZZZ_B_STOP64", 64 )
   ENDIF

RETURN xRet

