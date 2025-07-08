/*
 * MINIGUI - Harbour Win32 GUI library
 *
 * Copyright 2023 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2023 Andrey Verchenko <verchenkoag@gmail.com>. Dmitrov, Russia
 *
*/
#define _HMG_OUTLOG
#include "minigui.ch"

#define LEN_SPC     50
#define INI_FILE  ChangeFileExt( Application.ExeName, ".ini" )
///////////////////////////////////////////////////////////////////////
FUNCTION oIniData( cIni, lMacro, lUtf8, cRazd )
RETURN TIniData():New( cIni, lMacro, lUtf8, cRazd )

///////////////////////////////////////////////////////////////////////
FUNCTION oIniRead( cIni, lMacro, lUtf8, cRazd )
RETURN oIniData( cIni, lMacro, lUtf8, cRazd ):Read()

///////////////////////////////////////////////////////////////////////
// ������� ���������� � object-hash ������ �� �����
// ������ �� ���� ���������� ����� hash ini �� ������ write -> IniWriteParam()
FUNCTION SetIniData( oIni, cSection, cKey, xVal )
   oIni:Get(cSection):Set(cKey, xVal)
RETURN .T.  // ��� oIni:Get(cSection):Get(cKey) != NIL
            // ��� oIni:Get(cSection):Pos(cKey) > 0

///////////////////////////////////////////////////////////////////////
FUNCTION GetIniData2(oIni, cSec, cKey, xDef)
    LOCAL oSec := oIni:Get(cSec, oHmgData())
    LOCAL xRet := oSec:Get(cKey, xDef)

    IF HB_ISBLOCK(xRet) ; xRet := EVal(xRet)
    ENDIF

 RETURN xRet

///////////////////////////////////////////////////////////////////////
FUNCTION GetIniData(oIni, cSection, cKey, xDefault, lSay)
   LOCAL oSect, cErr, xRet, cIni, cPath, cType, cMsg, cPnl
   DEFAULT lSay := .F.

   // ������ DATA_SOURCE ������ ���� �� ���� ������ .Ini .Cfg
   IF ! Empty( oIni:Get("Data_Source") )
      cIni  := oIni:DATA_SOURCE:cFile  // ������ ���� �������� �������, ��� �� ��������,
      cPath := oIni:DATA_SOURCE:cPath  // ��� ������� � ����������� ������
      cType := oIni:DATA_SOURCE:cType
   ELSE
      cIni  := "�� ����������"
      cPath := "�� ����������"
      cType := "�� ����������"
   ENDIF
   cMsg := '{' + cIni + ',' + cPath + ',' + cType + '}'
   cPnl := ProcNL() + ";" + ProcNL(1) + ";" + ProcNL(2)
   cPnl += ";" + ProcNL(3) + ";" + ProcNL(4)

   IF Empty( oSect := oIni:Get(cSection) )      // NIL
      // not found section
      cErr := REPL('*',LEN_SPC) + ';'
      cErr += '������ ! ������ [' + cSection + '];'
      cErr += '��� ����� ������ ! ;;'
      cErr += cMsg + ";;"
      cErr += cPnl
      cErr += ";" + REPL('*',LEN_SPC) + ';'
      AlertStop(cErr, "������ � ���-�����" )
      ? AtRepl( ";", cErr, CRLF )
      RETURN xDefault
   ENDIF

   xRet := oSect:Get(cKey)
   //? ProcNL(), "**************", cKey, "=", xRet
   IF xRet == NIL
      // not found key
      cErr := REPL('*',LEN_SPC) + ';'
      cErr += '������ ! ������ [' + cSection + '];'
      cErr += '��� ����� "' + cKey + '" = ...;'
      cErr += '��������� ���� � ���-����� !;;'
      cErr += cMsg + ";;"
      cErr += ProcNL() + ";" + ProcNL(1) + ";" + ProcNL(2)
      cErr += ";" + REPL('*',LEN_SPC) + ';'
      IF lSay
         AlertStop(cErr, "������ � ���-�����" )
         ? AtRepl( ";", cErr, CRLF )
      ENDIF
      xRet := xDefault
   ELSE
      //xRet := oSect:Get(cKey)
      // xRet := oSect:&(cKey)    // other way
   ENDIF

RETURN xRet

/////////////////////////////////////////////////////////////////////
FUNCTION GetIniFor(oCnf, cSection, cKeyMsk, xDefault, lSay)
   LOCAL nI, xI, xVal, cErr, aDim, cKey, oSec, cPnl
   LOCAL cIni, cPath, cType, cMsg
   DEFAULT cKeyMsk := cSection := "", lSay := .F.

   oSec := oCnf:Get("Data_Source")
   //? ProcNL(), oSec, Empty( oSec )
   //_o2Log(oSec, 10, "==> .T. ini: ", .T.)

   // ������ DATA_SOURCE ������ ���� �� ���� ������ .Ini .Cfg
   IF ! Empty( oCnf:Get("Data_Source") )
      cIni  := oCnf:DATA_SOURCE:cFile  // ������ ���� �������� �������, ��� �� ��������,
      cPath := oCnf:DATA_SOURCE:cPath  // ��� �������� � ����������� ������
      cType := oCnf:DATA_SOURCE:cType
   ELSE
      cIni  := "�� ����������"
      cPath := "�� ����������"
      cType := "�� ����������"
   ENDIF
   cMsg := '{' + cIni + ',' + cPath + ',' + cType + '}'
   cPnl := ProcNL() + ";" + ProcNL(1) + ";" + ProcNL(2)
   cPnl += ";" + ProcNL(3) + ";" + ProcNL(4)

   IF Empty( oCnf:Get(cSection) )
      cErr := REPL('*',LEN_SPC) + ';'
      cErr += cMsg + ";;"
      cErr += '������ [' + cSection + '];'
      cErr += '��� ����� ������ !;'
      cErr += 'cSection = "' + cSection + '"   <<---  ������ !;;'
      cErr += cPnl
      cErr += ";" + REPL('*',LEN_SPC) + ';'
      AlertStop(cErr, "������ � ���-�����" )
      ? AtRepl( ";", cErr, CRLF )
      RETURN {}
   ENDIF

   IF LEN(cKeyMsk) == 0
      cErr := REPL('*',LEN_SPC) + ';'
      cErr += cMsg + ";;"
      cErr += '������ [' + cSection + '];'
      cErr += '��� ����� ����� ����� ! ����� ������ !;'
      cErr += 'cKeyMaska = ""     <<---  ������ !;;'
      cErr += cPnl
      cErr += ";" + REPL('*',LEN_SPC) + ';'
      AlertStop(cErr, "������ � ���-�����" )
      ? AtRepl( ";", cErr, CRLF )
   ENDIF

   aDim := {}
   FOR EACH xI IN Array(1000)
      nI   := hb_enumindex(xI)
      cKey := cKeyMsk + HB_NtoS(nI)
      xVal := GetIniData(oCnf, cSection, cKey, xDefault, lSay)
      //                                       ^^^^ - ������� ��� �������� ���� ��� �����
      //                                 ^^^^ - ��� ����
      //                       ^^^^^^^^ - ��� ������
      //      ^^^^^^^^^^^^^^^ - ������ �������� �����
      //? SPACE(3) + "...", nI, cSection, cKey, xVal
      IF xVal == NIL ; EXIT
      ENDIF
      IF LEN(xVal) == 0 ; EXIT
      ENDIF
      AADD( aDim, xVal )
   NEXT

   IF LEN(aDim) == 0
      cErr := REPL('*',LEN_SPC) + ';'
      cErr += cMsg + ";;"
      cErr += '������ [' + cSection + '];'
      cErr += '�� ���� ������� ���� �� ����� ����� ! ;'
      cErr += 'cKeyMaska = "' + cKeyMsk + '"   <<---  ������ !;;'
      cPnl := ProcNL() + ";" + ProcNL(1) + ";" + ProcNL(2)
      cPnl += ";" + ProcNL(3) + ";" + ProcNL(4)
      cErr += cPnl
      cErr += ";" + REPL('*',LEN_SPC)
      AlertStop(cErr, "������ � ���-�����" )
      ? AtRepl( ";", cErr, CRLF )
   ENDIF

RETURN aDim
/////////////////////////////////////////////////////////////////////
Function IniFileYes()
   Local cText, cFileIni, lRet := .T.

   cFileIni := INI_FILE
   IF ! File( cFileIni )
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
      cText += CRLF + CRLF
      cText += '[COM]' + CRLF
      cText += "DateEdit   = " + HB_TTOC( HB_DATETIME() ) + CRLF
      //HB_MemoWrit( cFileIni, cText )
      lRet := .F.
      cText := "There is no ini-file for the program !;"
      cText += cFileIni + ";;" + ProcNL() + ";" + ProcNL(1)
      AlertStop(cText)
      lRet := .F.
      ?? cText
   ENDIF

   // �������� ����� ���������� ���-����
   App.Cargo:cFileIni := cFileIni

Return lRet

//////////////////////////////////////////////////////////////////////////////
Function IniGetParam()
   LOCAL nMenu, oIni, cSec, oSec, aSec, cPath, aDim, nVal, lVal, cVal, lUtf8
   LOCAL cFileIni

   cFileIni := App.Cargo:cFileIni
   lUtf8    := .F.    // ����� .T. � ���� ������ � UTF-8  // ����� � ��������� RU1251
   // �������� ��� ������ Ini �����, ������� ���. ��� ���
   // �������� ��������� �������� ������ ��� � ������ �������\���� ������, � �� ������ "C", �.�.
   //oIni := TIniData():New(cIni, .T.):Read() - �������� ��� ������ ����� ������������� �� �����
   //oIni := TIniData():New(cIni, .F.):Read() - �������� ��� ������ ����� ������ ���� "C"
   //oIni := oIniData( cFileIni, .T., lUtf8, ):Read() - ��� ������ �� �����
   //oIni -> ��� hash � ���������� hash �������� � ���������� � ��� hash �������
   // --- ��������!
   // ����� ������ �� ������� ������ ini � hash hIni := hb_IniRead(...)
   // ����������� ��� "[", ����������� � ����� ����� �����, �������������� ��� ������ ������,
   // �.�. ����. ������� - ��� ������ [TEST], ����� ����� ����������� �� ������ ";"
   //������ 1 = { "test2.dbf" , "[TEST]"  , "[������]", "test" , "!Deleted[]" }
   // ������ ������ "[]" �� "<>" ����� ����� ������ � ����� �� ������ ";"
   //������ 1 = { "test2.dbf" , "<TEST>"  , "[������]", "test" , "!Deleted<>" }
   // �����������-2 ��� �������, ���� ��� �������, �� ������ ������������ � ������
   // �� ���������� ��� ������������ - myFunColor(), �.�. �� �������� ��� ����������
   //������ 2 = { "funct.dbf" , myFunColor() , "Func2"    , "Func"   , "SHARED" , 3 }
   // ����� ��������� a[2] := Eval( a[2] )
   //������ 3 = { "funct.dbf" , {|| myFunColor() }, "Func2"    , "Func"   , "SHARED" , 3 }
   // ����� ��������� a[2] := Eval( a[2] )

   // ������� ��������� ���-���� � ���������� App.Cargo �� ����� App.Cargo:cFileIni
   oIni := oIniData( cFileIni, .T., lUtf8, ):Read()

   /* �� ���� STATIC/PUBLIC ����������, ����������
   LOCAL oIni := App.Cargo:oIni
   LOCAL oCom := oIni:COM                  // ������ [COM]
   ��� �����, ���� ������ �� ��. ������ �� ����
   LOCAL oCom := App.Cargo:oIni:COM        // oCom := oIni:COM
   LOCAL oPrj := App.Cargo:oIni:MergePrj   // ������ [MergePrj]
   ���
   LOCAL o := oIni:MergePrj */
   /* ------------ ��� ������ ���� ---------
   cDate := "0d"+Dtos(Date())
   cDate := hb_valtoexp(Date())
   cDateTime := hb_Valtoexp(hb_DataTime())
   cDateTime := "t"+hb_TtoS(hb_DataTime())
   */
   // ��������� ������ � oIni
   oIni:Data_Source := oHmgData()  // ������ �������� (������ ������ �����������)
   oIni:Data_Source:cFile := cFileNoPath(cFileIni)
   oIni:Data_Source:cPath := cFilePath(cFileIni)
   oIni:Data_Source:cType := "File"
   // ���
   IF !Empty( oSec := oIni:Get("Data_Source") )
      oIni:Set("Data_Source", oHmgData())  // ������ ��������
      oIni:Data_Source:Set("cFile", cFileNoPath(cFileIni))
      oIni:Data_Source:Set("cPath", cFilePath(cFileIni))
      oIni:Data_Source:Set("cType", "File")
   ENDIF
   // ���
   IF !Empty( oSec := oIni:Get("Data_Source") )
      oSec:Set("cFile", cFileNoPath(cFileIni))
      oSec:Set("cPath", cFilePath(cFileIni))
      oSec:Set("cType", "File")
   ENDIF

   // ��������� ini-���� � ���������� App.Cargo
   App.Cargo:oIni := oIni

   aSec := oIni:Keys()                   // ��� ������ ���-�����
   ? ProcNL(), "Ini-file " + cFileIni ; ? "   All sections =", aSec, hb_valtoexp(aSec)

   FOR EACH cSec IN oIni:Keys()          // ���������� ������
      oSec := oIni:Get(cSec)
      //? "   Section: " + cSec, "Number of variables in section", oSec:GetAll()
      // ?v oSec:GetAll() ; ?  // ��� �������� ���������� �� ���-�����
   NEXT

   //_o2Log(oIni, 15, "==> .F. ini: ", .F.) ; ?

   oSec := oIni:Get("Data_Source")
   //_o2Log(oSec, 10, "==> .T. ini: ", .T.) ; ?

   // ������ ������� ���������� - ������ [PATH]  ������ ������ ����� �������
   App.Cargo:nVerNetworkLocal := oIni:PATH:Get("������_����_���������", "")
   App.Cargo:cDirTemp         := oIni:PATH:Get("��������� �����"      , "")
   App.Cargo:cDirExec         := oIni:PATH:Get("����� � ����������"   , "")
   App.Cargo:cPath_Server     := oIni:PATH:Get("����_��_������"       , "")
   App.Cargo:cPath_Local      := oIni:PATH:Get("����_���������"       , "")
   App.Cargo:lFileSay         := GetIniData( oIni, "PATH", "�����_������", .F. , .T.)
   IF App.Cargo:nVerNetworkLocal == 1
      cPath := App.Cargo:cPath_Server
   ELSE
      cPath := App.Cargo:cPath_Local
   ENDIF
   App.Cargo:cPath_Dbase := cPath + oIni:PATH:Get("���� � �����"    , "")
   App.Cargo:cPath_Index := cPath + oIni:PATH:Get("���� � ��������" , "")
   App.Cargo:cPath_Semaf := cPath + oIni:PATH:Get("���� � ��������" , "")

   ? ProcNL(), "������� ���������� - ������ [PATH]  ������ ������ ����� �������"
   ? SPACE(3)+">", App.Cargo:nVerNetworkLocal
   ?? App.Cargo:cDirTemp
   ?? App.Cargo:cDirExec
   ?? App.Cargo:cPath_Server
   ?? App.Cargo:cPath_Local
   ? SPACE(3)+">", App.Cargo:cPath_Dbase
   ?? App.Cargo:cPath_Index
   ?? App.Cargo:cPath_Semaf
   ?? "�����_������=",App.Cargo:lFileSay

   // ������ ���������� - ������ [PATH_EN]
   nVal := oIni:PATH_EN:Version_network_local   ; Default nVal := 0
   App.Cargo:nVerNetworkLocal  := nVal          // ������_����_���������

   cVal := oIni:PATH_EN:Temporary_folder        ; Default cVal := ""
   App.Cargo:cDirTemp          := cVal          // ��������� �����

   cVal := oIni:PATH_EN:Program_folder          ; Default cVal := ""
   App.Cargo:cDirExec         := cVal           // ����� � ����������

   cVal := oIni:PATH_EN:Path_PCServer           ; Default cVal := ""
   App.Cargo:cPath_Server     := cVal           // ����_��_������

   cVal := oIni:PATH_EN:Path_PCLocal            ; Default cVal := ""
   App.Cargo:cPath_Local      := cVal           // ����_���������

   ? ProcNL(), "������ ���������� - ������ [PATH_EN]"
   ? SPACE(3)+">", App.Cargo:nVerNetworkLocal , nVal
   ?? App.Cargo:cDirTemp
   ?? App.Cargo:cDirExec
   ?? App.Cargo:cPath_Server
   ?? App.Cargo:cPath_Local
   App.Cargo:lFileSay := GetIniData( oIni, "PATH_EN", "Path_lSay", .F. , .T.)
   ?? "Path_lSay=", App.Cargo:lFileSay

   // ������ ���������� - ������ [COM]
   lVal := oIni:Com:lShow_COPYDATA       ; Default lVal := .F.
   App.Cargo:WM_CD_lShow := lVal         // ����� ���� WM_COPYDATA

   lVal := oIni:Com:lCopyDataLog         ; Default lVal := .F.
   App.Cargo:lCopyDataLog := lVal        // ������� ���� _copydata.log

   lVal := oIni:Com:lFileLog             ; Default lVal := .F.
   App.Cargo:lFileLog := lVal            // ����� ���� cFileLog F-�� �������/T-�������

   nVal := oIni:Com:TimerSec             ; Default nVal := 30
   App.Cargo:nTimerSec := nVal           // ������ ��� ����������� ������� Timer_1 ������ 30 ������

   Default App.Cargo:lCopyFile := .F.
   lVal := oIni:Com:lCopyFile            ; Default lVal := .F.
   App.Cargo:lCopyFile := lVal           // ����� ����������� ������ F-��������/T-������

   nMenu := oIni:Com:nWorkMode           ; Default nMenu := 1        // ����� ��������� � ���������
   App.Cargo:nMenuType := nMenu          // �������� - ��� ���� �������� ���������

   // ---------------------- ����� ������� / ���� -----------------------
   nVal := oIni:Com:nModeColor   ; Default nVal := 1          // ����� ������ ���������
   App.Cargo:nModeColor := nVal                               // ��������

   nVal := oIni:Com:nTsbColor1    ; Default nVal := 5460819
   App.Cargo:nTsbColor1 := nVal                                // ��������

   nVal := oIni:Com:nTsbColor2    ; Default nVal := 8421504
   App.Cargo:nTsbColor2 := nVal                                // ��������

   nVal := oIni:Com:nTsbColor3    ; Default nVal := 3881787
   App.Cargo:nTsbColor3 := nVal                                // ��������

   nVal := oIni:Com:nTsbColor4    ; Default nVal := 11075583
   App.Cargo:nTsbColor4 := nVal                                // ��������

   nVal := oIni:Com:nTsbColor5    ; Default nVal := 8388736
   App.Cargo:nTsbColor5 := nVal                                // ��������

   nVal := oIni:Com:nTsbColor6    ; Default nVal := 568567
   App.Cargo:nTsbColor6 := nVal                                // ��������

   nVal := oIni:Com:nTsbClr01     ; Default nVal := 0
   App.Cargo:nTsbClr01 := nVal                                // ��������

   nVal := oIni:Com:nTsbClr01a    ; Default nVal := 16777215
   App.Cargo:nTsbClr01a := nVal                                // ��������

   nVal := oIni:Com:nTsbClr16     ; Default nVal := 6381921
   App.Cargo:nTsbClr16 := nVal                                // ��������

   nVal := oIni:Com:nTsbClr17     ; Default nVal := 4227327
   App.Cargo:nTsbClr17 := nVal                                // ��������

   nVal := oIni:Com:nTsbClr04     ; Default nVal := 4539717
   App.Cargo:nTsbClr04 := nVal                                // ��������

   nVal := oIni:Com:nTsbClr03     ; Default nVal := 33023
   App.Cargo:nTsbClr03 := nVal                                // ��������

   nVal := oIni:Com:nTsbClrSpecHd1 ; Default nVal := 2960685
   App.Cargo:nTsbClrSpecHd1 := nVal                          // ��������

   nVal := oIni:Com:nTsbClrSpecHd2 ; Default nVal := 12632256
   App.Cargo:nTsbClrSpecHd2 := nVal                          // ��������

   nVal := oIni:Com:nTsbClr2Ico    ; Default nVal := 9145227
   App.Cargo:nTsbClr2Ico  := nVal                          // ��������

   nVal := oIni:Com:nClrWinMain1  ; Default nVal := 4144959
   App.Cargo:nClrWinMain1 := nVal                                // ��������

   nVal := oIni:Com:nClrWinMain2  ; Default nVal := 14737632
   App.Cargo:nClrWinMain2 := nVal                                // ��������

   nVal := oIni:Com:nClrWin1Btn   ; Default nVal := 32896
   App.Cargo:nClrWin1Btn := nVal                                // ��������

   nVal := oIni:Com:nClrWinCnfg   ; Default nVal := 6118749
   App.Cargo:nClrWinCnfg := nVal                                // ��������

   aDim := oIni:Com:aTsbMyColor   ; Default aDim := {}
   App.Cargo:aTsbMyColor := aDim                                // ��������

   // ---------------------- ����� ������� -----------------------
   aDim := oIni:Com:aTsbFont_1  ; Default aDim := {"DejaVu Sans Mono", 13, .F., .F., .F., .F., 0, 18, 31, "Norm"} // ���� ������� �������
   App.Cargo:aTsbFont_1 := aDim

   aDim := oIni:Com:aTsbFont_2  ; Default aDim := {"Times New Roman", 13, .T., .F., .F., .F., 0, 18, 31, "Bold"}  // ���� ��������� �������
   App.Cargo:aTsbFont_2 := aDim

   aDim := oIni:Com:aTsbFont_3  ; Default aDim := {"Times New Roman", 12, .F., .F., .F., .F., 0, 18, 31, "Ital"}  // ���� ������� �������
   App.Cargo:aTsbFont_3 := aDim

   aDim := oIni:Com:aTsbFont_4  ; Default aDim := {"Arial", 11, .F., .T., .F., .F., 0, 21, 38, "SpecHd"}          // ���� ����������/����������
   App.Cargo:aTsbFont_4 := aDim

   aDim := oIni:Com:aTsbFont_5  ; Default aDim := {"DejaVu Sans Mono", 12, .T., .F., .F., .F., 0, 21, 38, "SuperHd"} // ���� �����������
   App.Cargo:aTsbFont_5 := aDim

   aDim := oIni:Com:aTsbFont_6  ; Default aDim := {"Arial", 12, .F., .F., .F., .F., 0, 0, 0, "Edit"}              // ���� �������������� ������
   App.Cargo:aTsbFont_6 := aDim

   App.Cargo:aTsbFonts := { App.Cargo:aTsbFont_1, App.Cargo:aTsbFont_2, App.Cargo:aTsbFont_3, ;
                            App.Cargo:aTsbFont_4, App.Cargo:aTsbFont_5, App.Cargo:aTsbFont_6  }     // ��� ����� �������

   // ----------------------- ����� ���� � ������ ���� -----------------------
   aDim := oIni:Com:aFntBtnMain   ; Default aDim := {"DejaVu Sans Mono", 13, .F., .F., .F., .F. }
   App.Cargo:aFntBtnMain := aDim                      // ���� ������ ������� �����
   App.Cargo:cFName      := App.Cargo:aFntBtnMain[1]
   App.Cargo:nFSize      := App.Cargo:aFntBtnMain[2]
   App.Cargo:lFBold      := App.Cargo:aFntBtnMain[3]

   aDim := oIni:Com:aWinFont    ; Default aDim := {"DejaVu Sans Mono", 13, .F., .F., .F., .F. }
   App.Cargo:aWinFont := aDim                         // ���� ��� ����� ���� ����
   App.Cargo:cFName   := App.Cargo:aWinFont[1]
   App.Cargo:nFSize   := App.Cargo:aWinFont[2]

   aDim := oIni:Com:aBtnFont_1  ; Default aDim := {"Comic Sans MS", 14, .T., .F.}
   App.Cargo:aBtnFont_1 := aDim

   aDim := oIni:Com:aBtnFont_2  ; Default aDim := {"Snap ITC"     , 14, .T., .F.}
   App.Cargo:aBtnFont_2 := aDim

   App.Cargo:aBtnFont  := App.Cargo:aBtnFont_1   // ���� ������ ������ ����
   App.Cargo:cFName2   := App.Cargo:aBtnFont[1]
   App.Cargo:nFSize2   := App.Cargo:aBtnFont[2]

   // ����� � ����������� ����
   aDim := oIni:Com:aFontCnMn1  ; Default aDim := {"DejaVu Sans Mono", 13, .F., .F., .F., .F. }
   App.Cargo:aFontCnMn1 := aDim

   aDim := oIni:Com:aFontCnMn2  ; Default aDim := {"Arial", 13, .F., .F., .F., .F. }
   App.Cargo:aFontCnMn2 := aDim

   aDim := oIni:Com:aFontCnMn3  ; Default aDim := {"Comic Sans MS", 14, .F., .F., .F., .F. }
   App.Cargo:aFontCnMn3 := aDim

   aDim := oIni:Com:aFontCnMn4  ; Default aDim := {"Times New Roman", 14, .t., .F., .F., .F. }
   App.Cargo:aFontCnMn4 := aDim

   App.Cargo:aFontCnMn  := { App.Cargo:aFontCnMn1, App.Cargo:aFontCnMn2 ,;
                             App.Cargo:aFontCnMn3, App.Cargo:aFontCnMn4 }  // ��� ����� � ����������� ����

   // --------------------- ������� ������� ----------------------
   lVal := oIni:Com:lTsbCol10    ; Default lVal := .T.         // ����� 10 �������
   App.Cargo:lTsbCol10 := lVal                                 // ��������

Return Nil

/////////////////////////////////////////////////////////////////////
// ������ ���������� � ��� ����
Function IniSetWrite(cSection,cName,xVal)
   LOCAL oIni, oSec, cMsg

   oIni := App.Cargo:oIni                    // ����� ����� ������� oIni � �� ���� ��������

   IF Empty( oSec := oIni:Get(cSection) )    // ��� ������
      oIni:Set(cSection, oHmgData())         // ������ ��������
      cMsg := "�������� ������ [" + cSection + "] � ����;"
      cMsg += INI_FILE + ";;" + ProcNL() + ";" + ProcNL(1)
      AlertInfo(cMsg)
   ENDIF

   IF !Empty( oSec := oIni:Get(cSection) )
      oSec:Set(cName, xVal)
   ENDIF

   IniWriteParam() // ������ ����� ��� �����

RETURN NIL

/////////////////////////////////////////////////////////////////////
// ������ ����� ��� �����
Function IniWriteParam()
   LOCAL oIni, oInfo, cInfo := [Information]

   oIni := App.Cargo:oIni                    // ����� ����� ������� oIni � �� ���� ��������

   oInfo := oIni:Information                 //  ������ [Information]
   oInfo:Developed_in   := MiniGUIVersion()
   oInfo:xBase_compiler := Version()
   oInfo:C_compiler     := Hb_Compiler()
   oInfo:IniEdit        := HB_TTOC( HB_DATETIME() )

   // �������� ����� ���-����
   //cFile := oApp:cIni2
   //? "New file ini =", cFile

   //oIni:cCommentBegin := "# my Start !"
   //oIni:cCommentEnd   := "# my Stop !"
   //oIni:lYesNo := .T.             // Yes ��� No � ���������� ��������� ��� �������� ini ����������
   //oIni:aYesNo := {"��", "���"}   // Yes ��� No � ���������� ��������� ��� �������� ini

   //oIni:Write( cFile, .F. )     // �� UTF8, �.�. ��� BOM �� ������ (�� ����� ��� � BOM)
   //oIni:Write( cFile )            // ��� ������������ ���� UTF8 � BOM

   /*
      oIni:Write( cFile, .F. )     // �� UTF8, �.�. ��� BOM �� ������
                    ^     ^
   ���� �� �������� cFile,|�� ��� ����� ����, ��� ���� ���������� ��� �������� oIni
   ���� �� �������� 2-�� -- ��������, �� ��������� ����� �� �� ��� � ��� ��������
   oIni, �.�. ���� ��� BOM, �� Utf-8, ���� ��� �� ����, �� � ��� :Write() �� �����
   */

   // ������ �������� �� ���������� - ����� ����� :Set(...), �.�.
   // ������ ��� ������
   oIni:PATH:Set("��������� �����"   , App.Cargo:cDirTemp      )
   oIni:PATH:Set("����� � ����������", App.Cargo:cDirExec      )
   oIni:PATH:Set("����_��_������"    , App.Cargo:cPath_Server  )
   oIni:PATH:Set("����_���������"    , App.Cargo:cPath_Local   )

   // ������ �������� �� ����������
   // ������ ��� ������
   oIni:PATH_EN:Temporary_folder := App.Cargo:cDirTemp
   oIni:PATH_EN:Program_folder   := App.Cargo:cDirExec
   oIni:PATH_EN:Path_PCServer    := App.Cargo:cPath_Server
   oIni:PATH_EN:Path_PCLocal     := App.Cargo:cPath_Local

   oIni:Write()     // �� UTF8, �.�. ��� BOM �� ������

Return Nil
