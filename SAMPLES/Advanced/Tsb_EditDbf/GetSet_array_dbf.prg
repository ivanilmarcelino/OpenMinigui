/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2025 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * _TBrowse() ��������/���������� ������ � dbf
 * _TBrowse() Load/save data to dbf
*/
#define  _HMG_OUTLOG
#include "hmg.ch"
#include "tsbrowse.ch"
#include "tsb_mydef.ch"

#define  THIS_DEBUG  .F.
//////////////////////////////////////////////////////////////////////////////////
FUNCTION ArrayDbfLoad(aXdim,cAls,cIni,aWrtIni,nPage)  // ������ �� ���� � ������
   LOCAL nI, xVal, cField, cFunc, cRType, xDbf, xRet, cMsg, nFld, aPara, aLine
   LOCAL aUSort, aFld, x2Dbf, aVal, nJ, aPict, cName, cDbf, lDbg, nFlag
   LOCAL x15Col, x14Col, x13Col
   DEFAULT cIni := App.Cargo:cIniSortUser
   DEFAULT aWrtIni := {}, nPage := 0  // ��� ������ �������� - ������

   lDbg  := THIS_DEBUG
   DBSELECTAREA(cAls)
   IF lDbg  ; ? ProcNL(), "������ ������ �� ���� "+cAls+" � ������ !"
   ENDIF

   FOR nI := 1 TO LEN(aXdim)

      aLine  := aXdim[nI]
      cName  := aXdim[nI,ACOL_1]
      xVal   := aXdim[nI,ACOL_2]           // ������ ������� ��� ������ �� ����
      cRType := aXdim[nI,ACOL_4]           // ��� ��������� �����
      cField := aXdim[nI,ACOL_5]           // ���� ����
      cFunc  := ALLTRIM(aXdim[nI,ACOL_6])  // ������(4) � ������ � (2)
      x13Col := aXdim[nI,ACOL_13]          // (13) - �������� ������������� ����, ���� NIL, �� ������� �������
      x14Col := aXdim[nI,ACOL_14]          // (14) - ���.������ ��� ���� (3): SPR_A,CALC,SPR_J,SPR_S,CALC
      x15Col := aXdim[nI,ACOL_15]

      aXdim[nI,ACOL_10] := ""   // ��� ��� ������ � �������
      aXdim[nI,ACOL_11] := ""
      aXdim[nI,ACOL_12] := ""
      aVal              := {}   // ������ �������� �����

      IF "LINE" $ cRType ; LOOP
      ENDIF

      IF lDbg  ; ? nI, cName, "[��� ����: "+cRType+"]", cField
      ENDIF

      IF cRType == ""           // ��� ���� - ��������� ����� �������
      ELSEIF cRType $ "CLDNM"   // ��������� ����
      ELSEIF cRType == "DMN"    // ���� � ����������
      ELSEIF cRType == "DT"     // ����+�����
      ELSEIF cRType == "A"      // ��������� ������ ����� ���� {"TelFIO","TelFIO3","TelFIO2"}
      ELSEIF cRType == "CALC"   // ��������� ���������� - ��������� �����
      ELSEIF cRType == "SPR_A"  // ��������� ������ - ��������� �����
      ELSEIF cRType == "SPR_S"  // ���������� �� dbf ����� - ����������� ���� {"Master","KMaster","Master",2,"KFIRMA==1"}
      ELSEIF cRType == "K"      // ��� � ���� �� set relation �� ������ ���� - ��������� ����� ���������� �������
         // ������ �����, ��� ������
      ELSE
         cMsg := '������ !; ��� ��������� ����: "'
         cMsg += cRType + '" !;'
         cMsg += "������: " + HB_NtoS(nI) + " ! "
         cMsg += cName + " ..... ;"
         cMsg += "�������� ��������������"
         cMsg += ProcNL() + ";" + ProcNL(1)
         AlertStop(cMsg, "�������� ������ � DBF " + cAls, , 64, {RED})
         ? ATREPL( ";", cMsg, CRLF )
      ENDIF

      IF cRType == "A"                   // ��� ��������� �����
         aFld  := aXdim[nI,ACOL_5]       // ������ ����� ����
         aPict := aXdim[nI,ACOL_14]      // ����� ������ ��� �������
         IF LEN(aFld) == LEN(aPict)
            cMsg := "������ ! ������: " + HB_NtoS(nI) + ";"
            cMsg += "��� ����������� �������� ��� ���� [A] � �� "+cAls+";"
            cMsg += '������: (5 �������) - {"NKvar","CKvar"} ;'
            cMsg += '       (14 �������) - {"@Z 99999","xxxxx", "/" } - 3 �������� ;;'
            cMsg += ' (5 �������) - ' + HB_ValToExp(aFld)  + ' ;'
            cMsg += '(14 �������) - ' + HB_ValToExp(aPict) + ';;'
            cMsg += HB_ValToExp(aXdim[nI]) + ";;"
            cMsg += ProcNL() + ";" + ProcNL(1) + ";;"
            AlertStop(cMsg, "", , 64, {RED})
            ? ATREPL( ";", cMsg, CRLF )
         ENDIF

         xDbf  := ""
         FOR nJ := 1 TO LEN(aFld)
            cField := aFld[nJ]
            x2Dbf  := FIELDGET( FIELDNUM( cField ) )
            nFld   := FIELDNUM( cField )
            IF nFld == 0 .AND. LEN(cField) > 0
               cMsg := "������ ! ������: " + HB_NtoS(nI) + ";"
               cMsg += "��� ������ ���� ["+cField+"] � �� "+cAls+";"
               cMsg += HB_ValToExp(aXdim[nI]) + ";;"
               cMsg += ProcNL() + ";" + ProcNL(1) + ";;"
               AlertStop(cMsg, "", , 64, {RED})
               ? ATREPL( ";", cMsg, CRLF )
            ENDIF
            AADD( aVal, x2Dbf )  // ������ �������� �����
         NEXT
         xDbf := myDim2Format(aVal,aPict)
      ELSE
         xDbf := FIELDGET( FIELDNUM( cField ) )
         nFld := FIELDNUM( cField )
         IF nFld == 0 .AND. LEN(cField) > 0
            cMsg := "������ ! ������: " + HB_NtoS(nI) + ";"
            cMsg += "��� ������ ���� ["+cField+"] � �� "+cAls+";"
            cMsg += HB_ValToExp(aXdim[nI]) + ";;"
            cMsg += ProcNL() + ";" + ProcNL(1) + ";;"
            IF cRType # "K"
               AlertStop(cMsg, "", , 64, {RED})
            ENDIF
            ? ATREPL( ";", cMsg, CRLF )
            xDbf := "������ ! ��� ������ ���� ["+cField+"] � �� "+cAls+" !"
            aXdim[nI,ACOL_2] := xDbf
            LOOP
         ENDIF
      ENDIF

      IF lDbg  ; ?? "cFunc=",cFunc
      ENDIF
      IF LEN(cFunc) > 0
         IF AT("[",cFunc) > 0
            nFlag := 1            // ����������� ������������ ������� / standard harbour functions
            cFunc := CHARREPL('[',cFunc,'(')
            cFunc := CHARREPL(']',cFunc,')')
         ELSEIF AT("(",cFunc) > 0
            nFlag := 2            // ��� ������� / my functions
         ELSE
            nFlag := 0
            cMsg := "������ ! �� ���������� ������� !;"
            cMsg += "��� ������ () ��� [];"
            cMsg += "�������: " + cFunc + ";;"
            cMsg += ProcNL() + ";" + ProcNL(1)
            AlertStop(cMsg, "�������� ����", , 64, {RED})
            ? ATREPL( ";", cMsg, CRLF )
         ENDIF
         //
         IF myIsFunct(cFunc,xDbf,nI,"������! "+HB_ValToExp(aXdim[nI]) )
            aPara := { cRType, cField, xDbf, x14Col, x15Col, nI, aLine }
            IF nFlag == 2  // ��� �������
               cFunc := SUBSTR(cFunc, 1, AT("(",cFunc) - 1)
               cFunc += "(" + hb_valtoexp(aPara) + ")"      // �������� ��������� ���� ������ �������
            ENDIF
            IF lDbg ; ?? cFunc
            ENDIF

            xRet  := myMacro(cFunc, .F.)   // .T. ����� �������
            IF IsString(xRet) .AND. "ERROR! MACRO=" $ UPPER(xRet)
               ? "___ERROR!___", nI, aXdim[nI,1], xRet
            ENDIF
            IF VALTYPE(xRet) == "A"            // ��� ���� "CALC"
               aXdim[nI,ACOL_2]  := xRet[1]
               aXdim[nI,ACOL_13] := xRet[2]    // ����� ����� ������ �������� ��� ���������� ������
            ELSE                               // � ������ ���� ��� ��������������
               aXdim[nI,ACOL_2] := xRet
            ENDIF
         ENDIF
         IF lDbg  ; ?? "xRet=", xRet
         ENDIF
         // �������������� � "C" �������
         aXdim[nI,ACOL_10] := myVal2Str(aXdim[nI,ACOL_13])
         aXdim[nI,ACOL_11] := myVal2Str(aXdim[nI,ACOL_14])
         aXdim[nI,ACOL_12] := myVal2Str(aXdim[nI,ACOL_15])
      ELSE
         IF cRType $ "CLDNM"
            aXdim[nI,ACOL_2] := xDbf
         ELSEIF cRType == "DMN"  // ���� � ����������
            aXdim[nI,ACOL_2] := xDbf
         ELSEIF cRType == "DT"   // ����+�����
            aXdim[nI,ACOL_2] := HB_TTOC(xDbf)
         ELSEIF cRType == "A"   // ��������� ������
            aXdim[nI,ACOL_13] := aVal    // ����� ����� ������ �������� ��� ���������� ������
            aXdim[nI,ACOL_2]  := xDbf
            // �������������� � "C" �������
            aXdim[nI,ACOL_10] := myVal2Str(aXdim[nI,ACOL_13])
         ELSEIF cRType == "K"            // ��� � ���� �� set relation �� ������ ���� - ������ �����, ��� ������
            IF !IsNumeric(xDbf)
               xDbf := -15
            ENDIF
            // xDbf - ����� �����
            x14Col := aXdim[nI,ACOL_14]          // (14) - ���.������ ��� ���� (K): �� ����������� DBF
            aXdim[nI,ACOL_11] := myVal2Str(aXdim[nI,ACOL_14])
            aXdim[nI,ACOL_13] := HB_NtoS(xDbf)   // ����� ����� ��� �������� ����������� ��� ���������� ������
            aXdim[nI,ACOL_10] := myVal2Str(aXdim[nI,ACOL_13])
            // ��� ��� �������� ���� ����
            nFld := FIELDNUM( cField )
            IF nFld == 0 .AND. LEN(cField) > 0
               cMsg := "������ ! ��� ������ ���� ["+cField+"] � �� "+cAls+" !"
               cDbf := cMsg
            ELSE
               cDbf := GET_from_DBF(xDbf, x14Col)   // �������� �������� - ��� �� set relation
            ENDIF
            aXdim[nI,ACOL_2]  := cDbf
         ENDIF
      ENDIF

      IF cRType == "CALC" .AND. lDbg
         ?? "13", VALTYPE(aXdim[nI,ACOL_13]), aXdim[nI,ACOL_13]
         ?? "14", VALTYPE(aXdim[nI,ACOL_14]), aXdim[nI,ACOL_14]
         ?? "15", VALTYPE(aXdim[nI,ACOL_15]), aXdim[nI,ACOL_15]
      ENDIF

   NEXT

   // ������� ������ � ���-����� - ������� ���������� ������ �����
   IniLoadUserSort(cIni, App.Cargo:cTsbVersion, @aUSort)
   //? ProcNL(), cIni, aUSort, VALTYPE(aUSort)
   IF !IsArray(aUSort)
   ELSEIF LEN(aUSort) == 0
   ELSEIF LEN(aUSort) # LEN(aXdim)
      cMsg := "������ ! ������� ���������� !;"
      cMsg := "������ ���������� ����� ("
      cMsg += HB_NtoS(LEN(aUSort)) + ");"
      cMsg += "�� ����� ������� ����� ������� ("
      cMsg += HB_NtoS(LEN(aXdim)) + ") ;;"
      cMsg += cIni + ";;"
      cMsg += ProcNL() + ";" + ProcNL(1)
      AlertStop(cMsg, "�������� ��������", , 64, {RED})
      ? ATREPL( ";", cMsg, CRLF )
      AADD( aWrtIni, { cFileNoPath(cIni), nPage } )   // ����� �������� ���� ���
   ELSE
      FOR nI := 1 TO LEN(aXdim)
         aXdim[nI,ACOL_3] := aUSort[nI]  // (3)  - ������� ������ ����� � �������
      NEXT
   ENDIF

RETURN aXdim

////////////////////////////////////////////////////////////////////////
// ������ �� ������� � ����
FUNCTION ArrayDbfSave(aArray)
   LOCAL nI, xVal, cField, cFunc, cRType, xRet, cMsg, nFld, cAls
   LOCAL cFType, cXType, x13Col, c9Col, x14Col, aFld, nF, aVal
   LOCAL x15Col, aLine, aPara, cSay, lDbg, aUSort := {}

   lDbg := THIS_DEBUG
   cAls := ALIAS()
   IF lDbg ; ? ProcNL(), "������ �� ������� � ���� !",cAls
   ENDIF

   FOR nI := 1 TO LEN(aArray)

      aLine  := aArray[nI]
      xVal   := aArray[nI,ACOL_2]           // ������ ������� ��� ������ � ����
      cRType := aArray[nI,ACOL_4]           // (4) - ��� ��������� ����� �������
      cField := aArray[nI,ACOL_5]           // (5) ���� ����
      cFunc  := ALLTRIM(aArray[nI,ACOL_7])  // (7) ������ � (4) ����� ������� (6)
      c9Col  := aArray[nI,ACOL_9]           // (9) - ������ �������������� ����� Write/Read
      x13Col := aArray[nI,ACOL_13]          // (13) - �������� ������������� ����, ���� NIL, �� ������� �������
      x14Col := aArray[nI,ACOL_14]          // (14) - ���.������ ��� ���� (3): SPR_A,CALC,SPR_J,SPR_S,CALC
      x15Col := aArray[nI,ACOL_15]          // (15) - ���.������
      AADD(aUSort,aArray[nI,ACOL_3])        // (3)  - ������� ������ ����� � �������
      IF lDbg ; ? nI,aArray[nI,ACOL_1], "[��� ����: "+cRType+"]",c9Col
      ENDIF

      IF "LINE" $ cRType
          IF lDbg ; ?? "[������� ������]"
          ENDIF
          LOOP
      ELSEIF c9Col == "R" .AND. cRType # "K"
          IF lDbg ; ?? "[������� ������]"
          ENDIF
          LOOP
      ENDIF

      IF cRType $ "CLDNM" .OR. cRType == "DMN" .OR. cRType == "DT"
         // ���� ��������� ������, ������ ������������ ��� ������
      ELSE
         IF x13Col == NIL
            IF lDbg ; ?? "x13Col == NIL,  [������� ������]"
            ENDIF
            LOOP  // ���� NIL, �� ������� �������
         ENDIF
      ENDIF

      IF cRType # "A"          // ��� ��������� �����
         nFld   := FIELDNUM( cField )
         IF nFld == 0 .AND. LEN(cField) > 0
            cMsg := "������ ! ������: " + HB_NtoS(nI) + ";"
            cMsg += "��� ������ ���� ["+cField+"] � ��-"+cAls+";"
            cMsg += HB_ValToExp(aArray[nI]) + ";;"
            cMsg += ProcNL() + ";" + ProcNL(1)
            AlertStop(cMsg, "", , 64, {RED})
            ? ATREPL( ";", cMsg, CRLF )
         ENDIF
         cMsg := ""
      ELSE
         cMsg := HB_ValToExp(aArray[nI,ACOL_5])      // (5) ���� ����
      ENDIF

      IF lDbg ; ?? cFunc, cField, cMsg
      ENDIF
      IF LEN(cFunc) > 0
         //
         IF myIsFunct(cFunc,xVal,nI,"������! "+HB_ValToExp(aArray[nI]) )
            IF LEN(cField)  == 0
               // ��� ���� CALC ��� ���� cField = ""
               // for CALC type without field cField = ""
               aPara := { cRType, cField, x13Col, x14Col, nI, aLine }
               cFunc := SUBSTR(cFunc, 1, AT("(",cFunc) - 1)
               cFunc += "(" + HB_ValToExp(aPara) + ")"
               xRet  := myMacro(cFunc)
               IF lDbg ; ?? '[��� ���� cField = ""]', "Func:Write=", cFunc
               ENDIF

            ELSE
               // ��� ���� CALC c ����� cField = "XXXX"
               // for type CALC with field cField = "XXXX"
               aPara  := { xVal, cRType, cField, x13Col, x14Col, x15Col, nI, aLine }
               cFunc  := SUBSTR(cFunc, 1, AT("(",cFunc) - 1)
               cSay   := cFunc + "({...})"
               cFunc  += "(" + HB_ValToExp(aPara) + ")"
               IF lDbg ; ?? '[c ����� cField = "'+cField+'" ]' , '[cFunc = "' + cSay + '" ]'
               ENDIF
               // ������ ������� ��� ���������� ���������� ���� ����
               // launch functions for single saving of a database field
               xRet   := myMacro(cFunc, .T. )   // .T. ����� �������

               cFType := FieldType( FIELDNUM( cField ) )
               cXType := VALTYPE(xRet)
               IF cFType == "M" .AND. cXType == "C"
                  cFType := "C"  // ��� �� ������
               ENDIF
               IF cXType == "C" .AND. xRet == "RECORDS-ARE-ALREADY-CLOSED!"  //"������ ��� ���������!"
                  // ������� ���������� �������� � ������ � ����
                  // skip further checking and writing to the database
               ELSE
                  IF cFType # cXType
                     IF lDbg ; ?? "--- cFType # cXType", cFType, "#", cXType
                     ENDIF

                     IF !IsChar(xRet) ; xRet := cValToChar(xRet)
                     ENDIF
                     cMsg := "������ ! ������: " + HB_NtoS(nI) + ";"
                     cMsg += HB_ValToExp(aArray[nI]) + ";;"
                     cMsg += "������ ���� ���� ���� ["+cFType+"] � �������� ��� ������ ["+cXType+"];"
                     cMsg += "xRet=" + xRet + ";;"
                     cMsg += ProcNL() + ";" + ProcNL(1) + ";" + ProcNL(2)
                     AlertStop(cMsg, "", , 64, {RED})
                     ? ATREPL( ";", cMsg, CRLF )
                  ELSE
                     // ������ � ���� ����� cFunc
                     IF (cAls)->( RLock() )
                        (cAls)->&cField := xRet
                        (cAls)->( DbUnlock() )
                        (cAls)->( DbCommit() )
                        IF lDbg ; ?? "Write:",cField, "->", "xRet=", xRet
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF  // "RECORDS ARE ALREADY CLOSED!"  //"������ ��� ���������!"
            ENDIF
         ELSE
            //MsgDebug(cFunc,"������� ������ � ����:",cField,xRet)
         ENDIF
      ELSE

         // ������ ����� � ���� ���� ��� cFunc
         IF cRType $ "CLDNM" .OR. cRType == "DMN" // ���� � ����������
            IF (cAls)->( RLock() )
               (cAls)->&cField := xVal
               (cAls)->( DbUnlock() )
               (cAls)->( DbCommit() )
            ENDIF
            IF lDbg ; ?? "xVal=", xVal
            ENDIF

         ELSEIF cRType == "A"   // ��������� ������
            aFld := aArray[nI,ACOL_5]        // (5) ������ ����� ����
            aVal := aArray[nI,ACOL_13]      // ����� ������������ ������ �������� ��� ������
            IF (cAls)->( RLock() )
               FOR nF := 1 TO LEN(aFld)
                  xVal   := aVal[nF]
                  cField := aFld[nF]
                  (cAls)->&cField := xVal
                  IF lDbg ; ?? nF, cField+"=", "xVal=", xVal
                  ENDIF
               NEXT
               (cAls)->( DbUnlock() )
               (cAls)->( DbCommit() )
            ENDIF

         ELSEIF cRType  == "DT"  // ���� + �����
            IF (cAls)->( RLock() )
               IF VALTYPE(xVal) != "T"
                  // C  27.03.25 17:31:47.000   -> T  2025-03-27 17:31:47
                  xVal := hb_CToT(xVal)  // ��� ��� !
               ENDIF
               (cAls)->&cField := xVal
               (cAls)->( DbUnlock() )
               (cAls)->( DbCommit() )
            ENDIF
            IF lDbg ; ?? "xVal=", xVal
            ENDIF

         ELSEIF cRType == "SPR_A" .OR. cRType == "SPR_S"
            // (12)  - �������� ������������� ���� "NCA" ��� ���� CALC,SPR_A,SPR_J,SPR_S �� (3)
            // � ��������� ������� NIL
            xVal := x13Col
            IF lDbg ; ?? "Write=", xVal
            ENDIF

            IF xVal == NIL
               ? ProcNL()
               ? "ERROR - line:", nI, aArray[nI,ACOL_1], cRType, "x13Col=",x13Col
               ? "     aArray[nI,14]=", HB_ValToExp(aArray[nI,ACOL_14])
               MsgDebug(ProcNL(),"ERROR - line:", nI, cField, xVal, x13Col)
            ENDIF
            IF (cAls)->( RLock() )
               (cAls)->&cField := xVal
               (cAls)->( DbUnlock() )
               (cAls)->( DbCommit() )
            ENDIF

         ELSEIF cRType == "K"            // ��� � ���� �� set relation �� ������ ���� - ������ �����, ��� ������
            // x13Col - ������, ����� � �����
            xVal := x13Col                    // (13) - �������� ������������� ����
            IF lDbg ; ?? "Write=", xVal, VALTYPE(xVal)
            ENDIF

            IF !IsString(xVal)
               cMsg := "     ������ ���� ������ !"
               ? ProcNL()
               ? "ERROR - line:", nI, aArray[nI,ACOL_1], cRType, "x13Col=",x13Col
               ? cMsg
               MsgDebug(ProcNL(),"ERROR - line:", nI, cMsg, cField, xVal, x13Col)
            ENDIF
            xVal := Val(ALLTRIM(x13Col))
            IF (cAls)->( RLock() )
               (cAls)->&cField := xVal
               (cAls)->( DbUnlock() )
               (cAls)->( DbCommit() )
            ENDIF

         ELSE
            ? ProcNL()
            ? "ERROR - line:", nI, aArray[nI,1], cRType, "x13Col=",x13Col
            ? "     aArray[nI,14]=", HB_ValToExp(aArray[nI,ACOL_14])
            MsgDebug(ProcNL(),"ERROR - line:", nI, "��� ������ ����:", cRType, cField, xVal, x13Col)
         ENDIF
      ENDIF
   NEXT
   IF lDbg ; ? ProcNL(), "����� ������:", cAls
   ENDIF

RETURN NIL

///////////////////////////////////////////////////////////////////////////
// ������ ���������� ������ ����� �������
FUNCTION IniSave(aArray,cIni)
   LOCAL nI, aUSort := {}
   DEFAULT cIni := App.Cargo:cIniSortUser

   FOR nI := 1 TO LEN(aArray)
      AADD(aUSort,aArray[nI,ACOL_3])  // (3)  - ������� ������ ����� � �������
   NEXT
   // �������� ������� ���������� ������ �����
   IniSaveUserSort(cIni, App.Cargo:cTsbVersion, aUSort)

RETURN NIL

//////////////////////////////////////////////////////////////////////////////////////
// �������� ������ � ���-����
STATIC Function IniSaveUserSort(cFileIni, cMetkaIni, aDim)
   LOCAL aSave
   aSave := { aDim, cMetkaIni, App.ExeName }
   HB_MemoWrit( cFileIni, HB_ValToExp(aSave) )
Return Nil

///////////////////////////////////////////////////////////////////////
// ������� ������ � ���-�����
STATIC Function IniLoadUserSort(cFileIni, cMetkaIni, aDim)
   LOCAL cStr, aRet

   IF !FILE(cFileIni)
      aDim := {}
      Return Nil
   ENDIF

   cStr := ALLTRIM( hb_MemoRead(cFileIni) )
   IF LEN(cStr) == 0
     // ��� ������
   ELSE
      // ����� ��� ���������� ������ ��������� ���� ����� ��� ������
      IF AT( "{", cStr ) > 0 .AND. AT( "}", cStr ) > 0 .AND. AT( cMetkaIni, cStr ) > 0
         aRet      := &cStr
         aDim      := aRet[1]
         cMetkaIni := aRet[2]  // ����� ��� ���������� ������ ��������� ���� ����� ��� ������
      ELSE
        // ��� ������
      ENDIF
   ENDIF

Return Nil
