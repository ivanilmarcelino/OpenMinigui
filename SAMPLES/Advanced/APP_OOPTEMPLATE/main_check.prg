/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * (c) 2013-2023 Verchenko Andrey <verchenkoag@gmail.com>
 * Implementation (c) 2013-14 Grigory Filatov <gfilatov@inbox.ru>
 * Fixed (c) 2023 Sergej Kiselev <bilance@bilance.lv>
*/

#include "minigui.ch"

/////////////////////////////////////////////////////////////////////
// ������ ��������/�������� ������ ��� ������� ���������
// List of checks/opening of files when starting the program
Function StartupChecks()
   Local aRun := {}

   IF M->nProgLang == 2  // 1-�������, 2-english, 3-���������� ����
      AADD( aRun, { "Start of programm/������ ���������"                 , "MyStart()"       , 200 } )
      AADD( aRun, { "Dummy procedure 1"                                  , "Dummy_1()"       , 100 } )
      AADD( aRun, { "Dummy procedure 2"                                  , "Dummy_2()"       , 100 } )
      AADD( aRun, { "Loading program paths/settings from *.ini file"     , "myLoadIni()"     , 100 } )
      AADD( aRun, { "Checking the existence of program folders and files", "myCheckDir()"    , 100 } )
      AADD( aRun, { "User password prompt"                               , "myPassword()"    , 100 } )
      AADD( aRun, { "Opening Database:"                                  , "myOpenDbf()"     , 100 } )
      AADD( aRun, { "Checking / copying files:"                          , "myCopyFiles()"   , 100 } )
      AADD( aRun, { "Run message box WM_COPYDATA"                        , "myWndCopyData()" , 150 } )
      AADD( aRun, { "Loading protected variables from a *.cnf file"      , "myLoadCnfg()"    , 100 } )
      AADD( aRun, { "Launch additional *.exe files"                      , "myStart()"       , 150 } )
      //AADD( aRun, { "Run message box TIMER"                            , "myWndTimerShow()", 150 } )  // reserve
      AADD( aRun, { "Starting the main form of the program"              , "myStart()"       , 150 } )
   ELSE
      AADD( aRun, { "������ ���������/������ ���������"                  , "MyStart()"       , 200 } )
      AADD( aRun, { "��������� ��������� 1"                              , "Dummy_1()"       , 100 } )
      AADD( aRun, { "��������� ��������� 2"                              , "Dummy_2()"       , 100 } )
      AADD( aRun, { "�������� �����/�������� ��������� �� *.ini-�����"   , "myLoadIni()"     , 100 } )
      AADD( aRun, { "�������� ������������� ����������� ����� � ������"  , "myCheckDir()"    , 100 } )
      AADD( aRun, { "�������� ���� ������:"                              , "myOpenDbf()"     , 100 } )
      // ���� ������ ������ �� ����, �� ������ ���
      AADD( aRun, { "������ ������ ������������"                         , "myPassword()"    , 100 } )
      AADD( aRun, { "��������/����������� ������:"                       , "myCopyFiles()"   , 100 } )
      AADD( aRun, { "������ ���� ��������� WM_COPYDATA"                  , "myWndCopyData()" , 150 } )
      AADD( aRun, { "�������� ���������� ���������� �� *.cnf-�����"      , "myLoadCnfg()"    , 100 } )
      AADD( aRun, { "������ �������������� *.��� ������"                 , "myExeStart()"    , 150 } )
      //AADD (aRun, { "������ ���� ��������� TIMER"                      , "myWndTimerShow()", 150 } )  // ������
      AADD( aRun, { "������ ������� ����� ���������"                     , "myStart()"       , 150 } )
   ENDIF

Return aRun

/////////////////////////////////////////////////////////////////////
// ��� ������� ��������� (������� ��������������)
// This function is blank (do it yourself)
Function myStart(xVal)
   wApi_Sleep(xVal)  // ������ ��� ������������
Return .t.

/////////////////////////////////////////////////////////////////////
// ��� ������� ��������� (������� ��������������)
// This function is blank (do it yourself)
Function myExeStart(xVal)
   wApi_Sleep(xVal)  // ������ ��� ������������
Return .t.

/////////////////////////////////////////////////////////////////////
// ��� ������� ��������� (������� ��������������)
// This function is blank (do it yourself)
Function Dummy_1(xVal)
   wApi_Sleep(xVal)  // ������ ��� ������������
Return .t.

/////////////////////////////////////////////////////////////////////
// ��� ������� ��������� (������� ��������������)
// This function is blank (do it yourself)
Function Dummy_2(xVal)
   wApi_Sleep(xVal)  // ������ ��� ������������
Return .t.

/////////////////////////////////////////////////////////////////////
// ��� ������� ��������� (������� ��������������)
// This function is blank (do it yourself)
Function myLoadIni(xVal)
   LOCAL lRet
   lRet := IniFileYes()    // configuration file check  -> IniLoad.prg
   IF lRet
      IniGetParam()        // configuration file check  -> IniLoad.prg
   ENDIF
   //IniWriteParam()       // ������ ����� ��� �����   -> IniLoad.prg
   wApi_Sleep(xVal)        // ������ ��� ������������
Return lRet

/////////////////////////////////////////////////////////////////////
// ��� ������� ��������� (������� ��������������)
// This function is blank (do it yourself)
Function myLoadCnfg(xVal)
   LOCAL lRet
   lRet := CnfgFileYes()      // configuration file check  -> IniConfig.prg
   IF lRet
      lRet := CnfgGetParam()  // configuration file check  -> IniConfig.prg
   ENDIF
   //CnfgWriteParam()         // ������ ����� ��� �����   -> IniConfig.prg
   wApi_Sleep(xVal)           // ������ ��� ������������
Return lRet

/////////////////////////////////////////////////////////////////////
// ��� ������� ��������� (������� ��������������)
// This function is blank (do it yourself)
Function myCheckDir(xVal)
   wApi_Sleep(xVal)  // ������ ��� ������������
Return .T.

/////////////////////////////////////////////////////////////////////
// ��� ������� ��������� (������� ��������������)
// This function is blank (do it yourself)
Function myPassword(xVal)
   LOCAL nI, cTitle, cUserName, cPassword, nRet, lRet
   LOCAL cFrm := App.Cargo:cWinMain          // ��� cFrm := oFrm:Name

   lRet   := cUserName := cPassword := .T.
   cTitle := cFileNoPath(App.ExeName)
   // ������������� �� �������� ���� ������� ������������� Users.dbf
   FOR nI := 1 TO 3  // 3-�p����� �p����� ����� ��p���

      // ==>> form_LoginPassw.prg
      nRet := 0 // myGetPassword( cTitle, @cUserName, @cPassword, 0 )
      IF nRet == 0  // ������ ������
         // �������� �� ���� Users.dbf - cUserName, cPassword
         lRet := .T.
         EXIT
      ELSE
         // ������ �� ������ / ����� !
         // AlertStop("����� �� ����� ������ !", cTitle)
         lRet := .F.
         // ������� ���������� ������ .F. �� .T. ��� Nil
         SetProperty(cFrm, "OnInterActiveClose", {||.T.})
         EXIT
      ENDIF

   NEXT
   xVal := 10
   wApi_Sleep(xVal)  // ������ ��� ������������
Return lRet

/////////////////////////////////////////////////////////////////////
// ��� ������� ��������� (������� ��������������)
// This function is blank (do it yourself)
Function MyOpenDbf(xVal)
Local aFilesDbf := {}, nI, cVal

   AADD ( aFilesDbf, "Base01.dbf" )
   AADD ( aFilesDbf, "Base02.dbf" )
   AADD ( aFilesDbf, "Base03.dbf" )
   AADD ( aFilesDbf, "Base04.dbf" )

   cVal := GetProperty("Form_Splash","Label_1","Value")
   For nI := 1 TO LEN(aFilesDbf)
       SetProperty("Form_Splash","Label_1","Value",cVal + "->" + aFilesDbf[nI])
       wApi_Sleep(xVal)  // ������ ��� ������������
   NEXT

Return .T.

/////////////////////////////////////////////////////////////////////
// ��� ������� ��������� (������� ��������������)
// This function is blank (do it yourself)
Function MyCopyFiles(xVal)
Local aFiles := {}, nI, cVal, cMask := "Rep-"

   For nI := 1 TO 5
       AADD ( aFiles, cMask + StrZero(nI,6) + ".txt" )
   NEXT

   cVal := GetProperty("Form_Splash","Label_1","Value")
   For nI := 1 TO LEN(aFiles)
      SetProperty("Form_Splash","Label_1","Value",cVal + "->" + aFiles[nI])
      wApi_Sleep(xVal)  // ������ ��� ������������
   NEXT

Return .T.
