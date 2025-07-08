/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * ������� � ���� Sqlite / Export to Sqlite file
*/

#define _HMG_OUTLOG
#include "minigui.ch"
#include <dbstruct.ch>

/////////////////////////////////////////////////////////////////////////
FUNCTION myDbWriteSqlite(cFile, cIcon, oWnd, oBrw)
   LOCAL xRet, cAls, cMsg, cFDbf, cTtl, aSay, nRec, cPath, lDelSkip
   LOCAL cLng, cLn2, cLn3, aRet, lOpen, cCdPg, lSet, nBeg, cSay, cExt
   LOCAL cVal, aStruct, aStruct2 := {}, nLField, nR, I, cMaska, lWrt
   LOCAL oDB, cTableNam, mSql, mFldNm, mFldType, mFldLen, mFldDec

   ? ProcNL(), cFile, oWnd, oWnd:ClassName, oBrw, oBrw:ClassName

   cAls    := oBrw:cAlias
   DbSelectArea(cAls)
   nRec    := LASTREC()       // ���-�� ������� ���� �� ���.�����
   nLField := fCount()        // ���-�� ����� ���� �� ���.�����
   aStruct := DbStruct()
   lSet    := Set( _SET_DELETED, .F. )   // SET DELETED OFF
   cMaska  := '(.dbf -> .sqlite)'

   IF App.Cargo:cLang == "RU"
      cTtl := '�������������� DBF � Sqlite ' + cMaska
      cMsg := "����� ������� � ������� = " + HB_NtoS( nRec )
      cLng := "���� ������� ������!;"
      cLn2 := "������� ���� ���� ?;"
      cLn3 := "���� �� ������ !;"
      cSay := '�������������� DBF � Sqlite'
   ELSE
      cTtl := 'Convert DBF to Sqlite ' + cMaska
      cMsg := "Total records in the table = " + HB_NtoS( nRec )
      cLng := "File created successfully !;"
      cLn2 := "Open this file ?;"
      cLn3 := "File NOT created !;"
      cSay := 'Convert DBF to Sqlite'
   ENDIF

   oWnd:Cargo:aFileExport := {}                                   // ������� �������� ��������
   cFDbf := hb_FNameName(cFile)
   cExt  := hb_FNameExt(cFile)
   cFDbf := LOWER( mySetAlias( cFDbf ) )                          // ������ ������� �����
   cFile := cFilePath(cFile) + "\" + cFDbf + cExt                 // ���� � �����

   Form_ExportFile(oWnd, cTtl, cMsg, cFile, cIcon, .F., "UTF8")   // �����-������� .F.-��� ����� ���������
                                                                  // ��������� ������ UTF8
   aRet := oWnd:Cargo:aFileExport                                 // ������� �������� �������� - ������� 3
   IF LEN(aRet) == 0
      RETURN .F.
   ENDIF
   cFile    := aRet[1]
   cCdPg    := aRet[2]
   lOpen    := aRet[3]
   lDelSkip := aRet[4]
   cFDbf    := mySetAlias( cFileNoPath( cFile ) )
   cPath    := cFilePath(cFile) + "\"                      // ���� � �����

   IF nRec < 1000
      aSay := { cTtl + ' ...', cFile }
      WaitWindow( aSay, .T., 800, 13, NIL, WHITE, App.Cargo:aDlgBColor, 14, BLUE, 4 ) // open the wait window
   ELSE
      WaitThreadCreateIcon( cSay, cMaska )   // ������ ��� �������
   ENDIF

   HB_FileDelete( cFile )
   oBrw:GoTop()
   oBrw:lEnabled := .F.    // ����������� ������� ������� (������ �� ������������)

   oDB := Connect2DB( cPath + cFDbf, .t. )
   cTableNam := ATREPL( "-", hb_FNameName(cFile), "_" )       // cAls - �� ����
   cTableNam := ATREPL( "(", cTableNam, "_" )                 // ������
   cTableNam := ATREPL( ")", cTableNam, "_" )                 // ������

   mSql := "CREATE TABLE IF NOT EXISTS "+cTableNam+" ("
   ///////////////////////////////////////////////////////////////////////////
   // �������� ������ �� ����� ������������ � ������ �� ��������� ����� �������� (storage class):
   // NULL,
   // INTEGER (�������� 1,2,3,4,6 ��� 8 ����),
   // REAL (����� � ��������� ������, 8 ���� � ������� IEEE),
   // TEXT (������ � ������� ������ ����, ������ UTF-8),
   // BLOB (�������� ������, �������� ���� �����).
   For i := 1 to len(aStruct)
      mFldNm   := aStruct[i, DBS_NAME]
      mFldType := aStruct[i, DBS_TYPE]
      mFldLen  := aStruct[i, DBS_LEN]
      mFldDec  := aStruct[i, DBS_DEC]
      IF mFldType $ "PQW,Q:U"
         LOOP
      ELSEIF mFldType $ "M:U,C:U,C:B"
         mFldType := Left(mFldType, 1)
      ENDIF

      Aadd(aStruct2, {mFldNm, mFldType, mFldLen, mFldDec})

      If Len(aStruct2) > 1
         mSql += ", "
      Endif
      mSql += Alltrim(mFldnm)+" "

      do case
      case mFldType = "C"
         mSql += "CHAR("+LTRIM(STR(mFldLen))+")"
      case mFldType = "D"
         mSql += "DATE"
      case mFldType = "T" .or. mFldType = "=" .or. mFldType = "@"
         mSql += "DATETIME"
      case mFldType = "N" .or. mFldType = "^"
         if mFldDec > 0
            mSql += "FLOAT"
         else
            mSql += "INTEGER"
         endif
      case mFldType = "F"
         mSql += "FLOAT"
      case mFldType = "I"
         mSql += "INTEGER"
      case mFldType = "B"
         mSql += "DOUBLE"
      case mFldType = "Y"
         mSql += "FLOAT"
      case mFldType = "L"
         // SQLite �� ����� ���������� ������ �������� Boolean .
         // ������ ����� Boolean �������� �������� ��� ����� ����� 0 (����) � 1 (������).
         // SQLite ���������� �������� ����� "TRUE" � "FALSE" � ������ 3.23.0 (2018-04-02),
         // �� ��� �������� ����� �� ����� ���� �������� ������ ��������������� �����������
         // ��� ������������� ��������� 1 � 0 ��������������.22 ���.2022�.
         mSql += "BOOL"
      case mFldType = "M"
         mSql += "TEXT"
      case mFldType = "V"
         mSql += "TEXT"
      case mFldType = "G"
         mSql += "BLOB"
      case mFldType = "+"
         mSql += "INTEGER PRIMARY KEY AUTOINCREMENT"
      otherwise
         AlertStop("Invalid Field Type: "+mFldType, , 64, {RED})
         return nil
      endcase
   next

   mSql += ")"
   //msginfo(mSql)

   if !miscsql( oDB, mSql )
      AlertStop( 'Table Creation Error!', 'Stop', , 64, {RED} )
      return nil
   endif

   if !miscsql( oDB, 'begin transaction' )
      return nil
   endif

   DO WHILE ! ( cAls )->( EOF() )

      nR := (cAls)->( RecNo() ) // ��� �������

      nBeg := INT( ( nR / nRec ) * 100 )
      IF nBeg % 5 == 0   // ����� ������ 5%
         cVal := SPACE(5) + HB_NtoS(nR) + "/" + HB_NtoS(nRec)
         //SetProperty( aLbl[1], aLbl[2], "Value" , cSay + cVal ) // ������� �������
      ENDIF

      lWrt := .T.
      IF (cAls)->( DELETED() ) .AND. lDelSkip
         lWrt := .F.  // ������� ������
      ENDIF

      IF lWrt
         mSql := "INSERT INTO "+cTableNam+" VALUES "
         msql += "("
         for i := 1 to len(aStruct2)
            mFldNm := aStruct2[i, DBS_NAME]
            if i > 1
               mSql += ", "
            endif
            mSql += c2sql(&mFldNm)
         next
         mSql += ")"
         if !miscsql( oDB, mSql)
            MsgBox("Problem in Query: "+mSql)
            return nil
         endif
      ENDIF

      ( cAls )->( dbSkip(1) )

      DO EVENTS    // ����������� ! ����� ����� �� ��������

   ENDDO

   if !miscsql( oDB, 'end transaction' )
      return nil
   endif

   DbSelectArea(cAls)
   oBrw:lEnabled := .T.    // �������������� ������� ������� (������ ������������)
   oBrw:Display()
   oBrw:Refresh(.T.)       // ������������ ������ � �������
   oBrw:GoTop()
   oBrw:SetFocus()
   DO EVENTS

   IF nRec < 1000
      WaitWindow()
   ELSE
      WaitThreadCloseIcon()  // kill the window waiting
   ENDIF

   Set( _SET_DELETED, lSet )   // SET DELETED ON - ������������

   IF FILE(cPath + cFDbf)
      IF lOpen
         cMsg := cLng + cFDbf + ";"
         cMsg += cPath + ";;" + cLn2
         IF AlertYesNo(cMsg, , ,cIcon, 64 , {LGREEN,RED} )
            ShellExecute( 0, "Open", cPath + cFDbf,,, 3 )
         ENDIF
      ELSE
         cMsg := cLng + cFDbf + ";"
         cMsg += cPath + ";"
         AlertInfo( cMsg, cTtl, cIcon, 64, {WHITE} )
      ENDIF
   ELSE
      cMsg := cLn3 + cFDbf
      AlertStop( cMsg, cTtl, "ZZZ_B_STOP64", 64 )
   ENDIF

RETURN xRet

////////////////////////////////////////////////////////////
FUNCTION connect2db(dbname,lCreate)
   Local dbo1 := sqlite3_open(dbname,lCreate)
   IF Empty( dbo1 )
      AlertStop( "Database could not be connected!",;
                   , "ZZZ_B_STOP64", 64 )
      RETURN nil
   ENDIF
RETURN dbo1

////////////////////////////////////////////////////////////
FUNCTION sql(dbo1,qstr)
   local table := {}
   local stmt
   local currow := nil
   local tablearr := {}
   local rowarr := {}
   local typesarr := {}
   local cdate := ""
   local current := ""
   local i := 0
   local j := 0
   local type1 := ""
   if empty(dbo1)
      AlertStop("Database Connection Error!",,,64,{RED})
      return tablearr
   endif
   table := sqlite3_get_table(dbo1,qstr)
   if sqlite3_errcode(dbo1) > 0 // error
      AlertStop(sqlite3_errmsg(dbo1)+" Query is : "+qstr,,,64,{RED})
      return nil
   endif
   stmt := sqlite3_prepare(dbo1,qstr)
   IF ! Empty( stmt )
      for i := 1 to sqlite3_column_count( stmt )
         type1 := upper(alltrim(sqlite3_column_decltype( stmt,i)))
         do case
            case type1 == "INTEGER" .or. type1 == "REAL" .or. type1 == "FLOAT" .or. type1 == "DOUBLE"
               aadd(typesarr,"N")
            case type1 == "DATE" .or. type1 == "DATETIME"
               aadd(typesarr,"D")
            case type1 == "BOOL"
               aadd(typesarr,"L")
            otherwise
               aadd(typesarr,"C")
         endcase
      next i
   endif
   sqlite3_reset( stmt )
   if len(table) > 1
      asize(tablearr,0)
      for i := 2 to len(table)
         rowarr := table[i]
         for j := 1 to len(rowarr)
            do case
               case typesarr[j] == "D"
                  cDate := substr(rowarr[j],1,4)+substr(rowarr[j],6,2)+substr(rowarr[j],9,2)
                  rowarr[j] := stod(cDate)
               case typesarr[j] == "N"
                  rowarr[j] := val(rowarr[j])
               case typesarr[j] == "L"
                  if val(rowarr[j]) == 1
                     rowarr[j] := .t.
                  else
                     rowarr[j] := .f.
                  endif
            endcase
         next j
         aadd(tablearr,aclone(rowarr))
      next i
   endif
Return tablearr

////////////////////////////////////////////////////////////
Function miscsql(dbo1,qstr)
   if empty(dbo1)
      AlertStop("Database Connection Error!",,,64,{RED})
      return .f.
   endif
   sqlite3_exec(dbo1,qstr)
   if sqlite3_errcode(dbo1) > 0 // error
      AlertStop(sqlite3_errmsg(dbo1)+" Query is : "+qstr,,,64,{RED})
      return .f.
   endif
Return .t.

////////////////////////////////////////////////////////////
Function C2SQL(Value)
   local cValue := ""
   local cdate := ""
   if valtype(value) == "C" .and. len(alltrim(value)) > 0
      value := strtran(value,"'","''")
   endif
   do case
      case Valtype(Value) == "N"
         cValue := AllTrim(Str(Value))
      case Valtype(Value) == "D"
         if !Empty(Value)
            cdate := dtos(value)
            cValue := "'"+substr(cDate,1,4)+"-"+substr(cDate,5,2)+"-"+substr(cDate,7,2)+"'"
         else
            cValue := "''"
         endif
      case Valtype(Value) == "T"
         if !Empty(Value)
            cValue := "'"+HB_TTOC(value, "yyyy-mm-dd", "hh:mm:ss")+"'"
         else
            cValue := "''"
         endif
      case Valtype(Value) $ "CM"
         IF Empty( Value)
            cValue="''"
         ELSE
            cValue := "'" + value + "'"
         ENDIF
      case Valtype(Value) == "L"
         cValue := AllTrim(Str(iif(Value == .F., 0, 1)))
      otherwise
         cValue := "''"       // NOTE: Here we lose values we cannot convert
   endcase
Return cValue
