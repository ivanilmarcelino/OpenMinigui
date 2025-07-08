/*
 * MINIGUI - Harbour Win32 GUI library
 *
 * Copyright 2023 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2023 Andrey Verchenko <verchenkoag@gmail.com>. Dmitrov, Russia
 *
 * ��������/���������� ������ � �����, ����������������� �������� Blowfish
 * Encryption/decryption of files and strings, Blowfish cryptographic algorithm
 * -> Harbour\src\rtl\hbbffnc.c
*/
#define _HMG_OUTLOG
#include "minigui.ch"

#define LEN_SPC     50
#define CFG_FILE    ChangeFileExt( Application.ExeName, ".cfg" )
#define CNFG_FILE   ChangeFileExt( Application.ExeName, ".cnfg" )
#define INI_FILE    ChangeFileExt( Application.ExeName, ".ini" )
STATIC cStaticPass := "My_Password_Key_0123456789"
///////////////////////////////////////////////////////////////////////////
Function CnfgFileYes()
   Local cText, cFileCfg, cFileCnfg, cPassword, lOk, lRet, cBfKey, cBuff

   cFileCfg  := CFG_FILE     // ���� ������������ ��������� �� �������������
   cFileCnfg := CNFG_FILE    // ���� ������������ ��������� �������������
   lRet      := .T.
   cPassword := cStaticPass
   // �������� ����� ���������� ���-����
   App.Cargo:cFileCfg  := cFileCfg
   App.Cargo:cFileCnfg := cFileCnfg
   /*
   IF ! File( cFileCfg )
      cText := "[Information]" + CRLF
      cText += "PROGRAM   = " +  App.Cargo:cProgTtlEn + CRLF
      cText += "ABOUTPRG  = " +  App.Cargo:cProgTtlRu + CRLF
      cText += "PROGVERS  = " +  App.Cargo:cProgVersion + CRLF
      cText += "Copyright = " +  App.Cargo:cCopyright + CRLF
      cText += "Email     = " +  App.Cargo:cEmail + CRLF
      cText += "ExeName   = " + Application.ExeName + CRLF
      cText += "Developed_in   = " + MiniGUIVersion() + CRLF
      cText += "xBase_compiler = " + Version()        + CRLF
      cText += "C_compiler     = " + Hb_Compiler()    + CRLF
      cText += "cRemEn = This file contains variables for the program that are not available to users" + CRLF
      cText += "cRemRu = � ���� ����� ���������� ���������� ��� ��������� ������� �� �������� �������������"  + CRLF
      cText += CRLF + CRLF
      cText += ';////////////////////////////////////////////////////////////////////' + CRLF
      cText += '[COM]' + CRLF
      cText += "Ver_Cnfg   = 4.23.0914  ; �� ����� ������ ������ ������������ � ������ ����������" +  CRLF
      cText += "Ver_Date   = 14.09.23   " +  CRLF
      cText += "Ver_FR_dll = 4.11.8" + CRLF
      cText += "Calc_Year  = 2023       ; ������ ������� ������ ����� ����" + CRLF
      cText += CRLF + CRLF
      HB_MemoWrit( cFileCfg, cText )
   ENDIF */

   ? ProcNL(), "CFG_FILE=", cFileNoPath(CFG_FILE)
   IF !FILE(cFileCfg) 
      IF !FILE(cFileCnfg) 
         cText := "There is no config-file for the program !;"
         cText += cFileCnfg + "; No source file - " 
         cText += cFileCfg + ";;" + ProcNL() + ";" + ProcNL(1)
         AlertStop(cText)
         lRet := .F.
         ?? cText
      ENDIF
      // ��� ������� ������� ���������
      App.Cargo:lDebugProg := .F.
   ELSE
      DELETEFILE(cFileCnfg)
      // ������� ����
      cText  := HB_MemoRead( cFileCfg )
      cBfKey := hb_blowfishKey( cPassword )   // ���������� �� ������ cPasswd � ���������� ���� cBfKey 
                                              // ��� ������������ ������������� � ��������� ��������/����������
      cBuff := hb_blowfishEncrypt( cBfKey, cText )  
      HB_MemoWrit(cFileCnfg, cBuff)
      cBfKey := ""
      lOk := FILE(cFileCnfg)  
      ? "hb_blowfishEncrypt() -> lOk=", lOk
      IF lOk
         cText := 'File was successfully hb_blowfishEncrypt() !;'
         cText += cFileCnfg
         //AlertInfo( cText )
         ?? cText
      ENDIF
      // ��� ���������� ������� ���������
      App.Cargo:lDebugProg := .T.
   ENDIF

Return lRet

//////////////////////////////////////////////////////////////////////////
Function CnfgGetParam()
   LOCAL oCnf, oCom, cSec, oSec, aSec, aDim, cVal, nVal, lUtf8, lOk, nI
   LOCAL cFileDcrp, cFileCnfg, cPassword, cMsg, cMemo, cBuff, cBfKey
   LOCAL cKey, xVal, xI, cKeyMsk, cSection, oSection
         
   ? ProcNL(), "CNFG_FILE=", cFileNoPath(CNFG_FILE)
   cFileCnfg := App.Cargo:cFileCnfg 
   cFileDcrp := ChangeFileExt( cFileCnfg, '.Dcrp' )
   lUtf8     := .F.                                    // ����� � ��������� RU1251
   cPassword := cStaticPass
   cBfKey    := hb_blowfishKey( cPassword )   // ���������� �� ������ cPasswd � ���������� ���� cBfKey 
                                              // ��� ������������ ������������� � ��������� ��������/����������
   IF !FILE(cFileCnfg) 
      cMsg := "There is no config-file for the program !;"
      cMsg += cFileCnfg + ";;" + ProcNL() + ";" + ProcNL(1)
      //AlertStop(cMsg)
      ?? cMsg
      RETURN .F.
   ENDIF
   // ���������� ����
   cMemo  := HB_MemoRead( cFileCnfg )
   cBuff  := hb_blowfishDecrypt( cBfKey, cMemo )         
   ? "hb_blowfishDecrypt() -> "
   IF cBuff == NIL
      cMsg := "ERROR ! Couldn't decrypt the file !;"
      cMsg += cFileNoPath(cFileCnfg)
      AlertStop( cMsg , "Critical error")
      lOk := .F.
   ELSE
      cMsg := 'File was successfully decrypted!;'
      cMsg += cFileNoPath(cFileCnfg)
      cMsg += ';LEN(cBuff)=' + HB_NtoS(LEN(cBuff)) 
      //AlertInfo( cMsg )
      lOk := .T.
   ENDIF
   ?? lOk, cMsg   // ���������
   IF ! lOk
      RETURN .F.
   ENDIF
   //HB_MemoWrit(cFileDcrp,cBuff)     // ��������

   //oIni := TIniData():New(cIni, .T.):Read() - �������� ��� ������ ����� ������������� �� �����
   //oIni := TIniData():New(cIni, .F.):Read() - �������� ��� ������ ����� ������ ���� "C"
   //oIni := oIniData( cFileDcrp, .T., lUtf8, ):Read() - ��� ������ �� �����
   //oIni -> ��� hash � ���������� hash �������� � ���������� � ��� hash �������

   // ������� ��������� cnfg-���� � ���������� oCnf �� ���������� cBuff
   oCnf := TIniData():New( , .T., lUtf8, , cBuff ):Read()    

   // ��������� ������ � oCnf
   oCnf:Data_Source := oHmgData()  // ������ �������� (������ ������ �����������)
   oCnf:Data_Source:cFile := cFileNoPath(cFileCnfg)
   oCnf:Data_Source:cPath := cFilePath(cFileCnfg)  
   oCnf:Data_Source:cType := "Decrypt-Buffer"      
   // ���
   IF Empty( oSec := oCnf:Get("Data_Source") )  //!!! ��� ������
      oCnf:Set("Data_Source", oHmgData())       // ������ ��������
   ENDIF
   oCnf:Data_Source:Set("cFile", cFileNoPath(cFileCnfg))
   oCnf:Data_Source:Set("cPath", cFilePath(cFileCnfg))
   oCnf:Data_Source:Set("cType", "Decrypt-Buffer")
   // ���
   IF !Empty( oSec := oCnf:Get("Data_Source") )
      oSec:Set("cFile", cFileNoPath(cFileCnfg))
      oSec:Set("cPath", cFilePath(cFileCnfg))
      oSec:Set("cType", "Decrypt-Buffer")
   ENDIF

   // ��������� cnfg-���� � ���������� App.Cargo
   App.Cargo:oCnf := oCnf

   oCom := oCnf:COMVAR                  // ������ [COMVAR]
   // oCom := App.Cargo:oCnf:COMVAR     // oCom := oCnf:COMVAR

   aSec := oCnf:Keys()                  // ��� ������ ���-�����
   ? ProcNL(), "Dcrp-file " + cFileNoPath(cFileDcrp) ; ? "   All sections =", aSec, hb_valtoexp(aSec)

   FOR EACH cSec IN oCnf:Keys()         // ���������� ������
      oSec := oCnf:Get(cSec)
      //? "   Section: " + cSec, "Number of variables in section", oSec:GetAll()
      //?v oSec:GetAll() ; ?  // ��� �������� ���������� �� ���-�����
   NEXT

   //_o2Log(oCnf, 20, "==> .F. ini: ", .F.) ; ?
   //_o2Log(oCnf, 20, "==> .T. ini: ", .T.) ; ?

   // ������ ���������� - ������ [COMVAR]
   cVal := oCnf:ComVar:Ver_Cnfg      ; Default cVal := "4.00.0000"
   App.Cargo:cVer_Cnfg := cVal

   cVal := oCnf:ComVar:Ver_Date      ; Default cVal := "01.01.23"
   App.Cargo:dVer_Date := CTOD(cVal)

   cVal := oCnf:ComVar:Ver_FR_dll    ; Default cVal := "0.00.0"
   App.Cargo:cVer_FR_dll := cVal

   nVal := oCnf:ComVar:Calc_Year     ; Default nVal := 0
   App.Cargo:nCnfBanYearCalc := nVal

   ? "   ��������� ����������:",App.Cargo:cVer_Cnfg, App.Cargo:dVer_Date
   ?? App.Cargo:cVer_FR_dll, App.Cargo:nCnfBanYearCalc

   //GetIniData(oCnf, cSection, cKey, xDefault)
   // ������ ������� ���������� - ������ [������_��������]  ������ ������ ����� �������
   ? "   ��������� ����������:"
   cSec := "��������_����_���"  ; ?? "������ ["+cSec+"]"
   // �������� ������
   IF CnfgSection(oCnf, cSec)
      cKeyMsk := "������_����_���_" 
      aDim    := GetIniFor(oCnf, cSec, cKeyMsk, {} ) 
      ? ProcNL(), "���� �� �����=", cKeyMsk, "aDim=", aDim ;  ?v aDim
   ENDIF

   cSec := "��������_��_������������"  ; ?? "������ ["+cSec+"]"
   // �������� ������
   IF CnfgSection(oCnf, cSec)
      cKeyMsk := "���� " // ������ ����������
      aDim    := GetIniFor(oCnf, cSec, cKeyMsk, {} ) 
      ? ProcNL(), "���� �� �����=", cKeyMsk, "aDim=", aDim ;  ?v aDim
   ENDIF

   cSec := "��������_��_��������"
   aSec := GetIniData(oCnf, cSec, "����", {})              
   ? "     ����:", VALTYPE(aSec), HB_ValToExp(aSec)
   // �����������-2 ��� �������, ���� ��� �������, �� ������ ������������ � ������
   aSec := GetIniData(oCnf, cSec, "����2", {})              
   ? "     ����2:", VALTYPE(aSec), HB_ValToExp(aSec)

   aDim := {}
   cSec := "��������_��_��������"
   ? ProcNL()
   FOR EACH xI IN Array(1000)
      nI := hb_enumindex(xI)
      cKey := "��������_����_" + HB_NtoS(nI)
      xVal := GetIniData(oCnf, cSec, cKey, {} )              
      //                                   ^^^ - ������� ��� �������� ���� ��� �����
      //                             ^^^^ - ��� ����
      //                       ^^^^ - ��� ������
      //      ^^^^^^^^^^^^^^^ - ������ �������� �����
      IF xVal == NIL ; EXIT
      ENDIF
      IF LEN(xVal) == 0 ; EXIT
      ENDIF
      AADD( aDim, xVal )
   NEXT
   ? ProcNL(), "aDim=", aDim
   ?v aDim 
   // �������� ��������� �������
   cKeyMsk := "��������_����_"
   aDim    := GetIniFor(oCnf, cSec, cKeyMsk, {} ) 
   ? ProcNL(), cKeyMsk, "aDim=", aDim  ;  ?v aDim
   ? _o2log({cSec, cKeyMsk, aDim}, , "==> "+ProcNL()+": ") // ��������

   // --------------------- [��������_��_������������] ----------------------
   cSection := "��������_��_������������"
   ? ProcNL(), "�������� [" + cSection + "]"
   oSection := oCnf:Get(cSection)
   _o2Log(oSection, 15, "==> .T. ini: ", .T.)
   //
   cKeyMsk  := UPPER("���� ")
   ? ProcNL(), '�������� "' + cKeyMsk + '"'
   App.Cargo:aListBase := GetIniFor(oCnf, cSection, cKeyMsk, {} )
   ?v App.Cargo:aListBase

Return .T.

/////////////////////////////////////////////////////////////////////
// ������ ����� ��� �����
Function CnfgWriteParam()
   LOCAL oCnf, oInfo, cInfo := [Information]

   oCnf := App.Cargo:oCnf                    // ����� ����� ������� oCnf � �� ���� ��������

   oInfo := oCnf:Information                 //  ������ [Information]
   oInfo:Developed_in   := MiniGUIVersion()
   oInfo:xBase_compiler := Version()
   oInfo:C_compiler     := Hb_Compiler()
   oInfo:IniEdit        := HB_TTOC( HB_DATETIME() )

   // �������� ����� ���-����
   //cFile := oApp:cIni2
   //? "New file ini =", cFile

   //oCnf:cCommentBegin := "# my Start !"
   //oCnf:cCommentEnd   := "# my Stop !"
   //oCnf:lYesNo := .T.             // Yes ��� No � ���������� ��������� ��� �������� ini ����������
   //oCnf:aYesNo := {"��", "���"}   // Yes ��� No � ���������� ��������� ��� �������� ini

   //oCnf:Write( cFile, .F. )     // �� UTF8, �.�. ��� BOM �� ������ (�� ����� ��� � BOM)
   //oCnf:Write( cFile )            // ��� ������������ ���� UTF8 � BOM

   /*
      oCnf:Write( cFile, .F. )     // �� UTF8, �.�. ��� BOM �� ������
                    ^     ^
   ���� �� �������� cFile,|�� ��� ����� ����, ��� ���� ���������� ��� �������� oCnf
   ���� �� �������� 2-�� -- ��������, �� ��������� ����� �� �� ��� � ��� ��������
   oCnf, �.�. ���� ��� BOM, �� Utf-8, ���� ��� �� ����, �� � ��� :Write() �� �����
   */

   oCnf:Write()     // �� UTF8, �.�. ��� BOM �� ������

Return Nil

///////////////////////////////////////////////////////////////////////
FUNCTION CnfgSection(oIni, cSection, lSay)
   LOCAL oSect, cErr, lRet, cIni, cPath, cType, cMsg, cPnl
   DEFAULT lSay := .T.

   cIni  := cFileNoPath(CNFG_FILE)
   cPath := cFilePath(CNFG_FILE)
   cType := "Decrypt-Buffer"
   cMsg := '{' + cIni + ',' + cPath + ',' + cType + '}'
   cPnl := ProcNL() + ";" + ProcNL(1) + ";" + ProcNL(2)
   cPnl += ";" + ProcNL(3) + ";" + ProcNL(4)
   lRet := .T.

   IF Empty( oSect := oIni:Get(cSection) )      // NIL
      cErr := REPL('*',LEN_SPC) + ';'
      cErr += 'Error! Section [' + cSection + '];'
      cErr += 'There is no such section! ;;'
      cErr += cMsg + ";;"
      cErr += cPnl
      cErr += ";" + REPL('*',LEN_SPC) + ';'
      IF lSay
         AlertStop(cErr, "Error in cnfg-file" )
      ENDIF
      ? AtRepl( ";", cErr, CRLF )
      lRet := .F.
   ENDIF

RETURN lRet

///////////////////////////////////////////////////////////////////////
FUNCTION GetFileCnfg() 
RETURN CNFG_FILE
