/*
 * MINIGUI - Harbour Win32 GUI library
 * ������ � AVI-�������� / Working with a AVI object
 * �������� ������� ��� �������� / MiniGui preloader for programs
 *
 * Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com>
 * Copyright 2024 Sergej Kiselev <bilance@bilance.lv>
*/
#define _HMG_OUTLOG
#define PRG_TITLE "������ � AVI-�������� / Working with a AVI object"

REQUEST DBFCDX

#include "minigui.ch"
#include "i_winuser.ch"
*----------------------------------------------------------------------------*
FUNCTION MAIN()
*----------------------------------------------------------------------------*
   LOCAL y, x, nW := System.DesktopWidth, nH := 64

   y := x := 0

   DEFINE WINDOW Win_1 CLIENTAREA nW, nH          ;
      TITLE PRG_TITLE                             ;
      MAIN NOMAXIMIZE NOSIZE BACKCOLOR SILVER     ;
      ON INIT {|| SendMessage( Win_1.Handle, WM_PAINT, 0, 0 ) ,;  // ���������� ������ ����� �� �����
                  MG_preloader(), Win_1.Release() }

      DRAW ICON IN WINDOW Win_1 AT y, x ;
        PICTURE '2MG_64' WIDTH 64 HEIGHT 64 COLOR This.BackColor 

   END WINDOW

   ACTIVATE WINDOW Win_1

RETURN Nil

*----------------------------------------------------------------------------*
PROCEDURE MG_preloader()
*----------------------------------------------------------------------------*
   LOCAL cNWin, cTitle, tStart, lFcs, cResAvi, aBackClr, aFontClr   
   LOCAL cDbf, cAls, nSumm, nKolvo, nI, nSeconds, cIndicator

   cDbf := GetStartUpFolder() + '\demo3.dbf'
   cAls := "TEST"
   USE (cDbf) ALIAS (cAls) NEW EXCLUSIVE

   //App.Cargo:lDebug := .F.     // ������� - �� �����������
   tStart   := hb_DateTime()     // ����� ������ ��������
   lFcs     := .F.               // ������� ����� �� ����.����
   cResAvi  := "ZipAVI"
   aBackClr := SILVER   
   aFontClr := RED
   cTitle   := "Database calculation: " + cFileNoPath(cDbf)
   // ������ ���� �������� � �������
   cNWin    := WaitWinAvi( cTitle,tStart,lFcs,cResAvi,aBackClr,aFontClr )  

   DBSELECTAREA(cAls)
   nKolvo   := Lastrec()
   nSeconds := SECONDS()
   nSumm    := 0

   GOTO TOP
   DO WHILE !EOF()

      DBSELECTAREA(cAls)
      nI := Recno()
      IF ABS( SECONDS() - nSeconds ) >= 0.2
         cIndicator := HB_NtoS(nI) + " / " + HB_NtoS(nKolvo)
         _wSend("S_a_y", cNWin, cIndicator)
         // _wSend(3, cNWin, cIndicator)   // ��� ���
         nSeconds := SECONDS()
      ENDIF
      DO EVENTS

      // ����������
      nSumm += (cAls)->F16

      DBSELECTAREA(cAls)
      SKIP
      DO EVENTS

      wApi_Sleep(50)  // ������, � �������� ������� - ����� ������ � ����

   ENDDO
   WaitWinAvi()   // ������� ���� "��������"

   // tStart - �� ����� ������, ����� ����������� ���������� �����
   WaitWinAvi("Recording calculations in the database",tStart,,cResAvi )  // ������ ���� �������� � �������
   DBSELECTAREA(cAls)
   GOTO TOP
   DO WHILE !EOF()
      (cAls)->F01 := (cAls)->F16 / nSumm * 100
      SKIP
      DO EVENTS
      wApi_Sleep(50)  // ������, � �������� ������� - ����� ������ � ����
   ENDDO
   WaitWinAvi()   // ������� ���� "��������"

   // ������� ������� ������
   tStart   := hb_DateTime()     // ����� ������ ��������
   lFcs     := .F.               // ������� ����� �� ����.����
   cResAvi  := "FindFolder"
   aBackClr := ORANGE   
   aFontClr := BLACK
   // ������ ���� �������� � �������
   WaitWinAvi( "Write log to folder",tStart,lFcs,cResAvi,aBackClr,aFontClr )  

   nSeconds := SECONDS()
   FOR nI := 1 TO 200
      IF ABS( SECONDS() - nSeconds ) >= 0.2
         cIndicator := HB_NtoS(nI) + " / 200"
         _wSend("S_a_y", cNWin, cIndicator)
         // _wSend(3, cNWin, cIndicator)   // ��� ���
         nSeconds := SECONDS()
      ENDIF
      // ����������
      wApi_Sleep(20)
      DO EVENTS
   NEXT

   WaitWinAvi()   // ������� ���� "��������"

RETURN 

*----------------------------------------------------------------------------*
INIT PROCEDURE SetsENV()
*----------------------------------------------------------------------------*
   LOCAL cFont := "DejaVu Sans Mono", nSize := 12
   LOCAL cLog := "_msg.log" , cIconDef := "1MG"
/*
   SET CODEPAGE TO RUSSIAN
   SET LANGUAGE TO RUSSIAN
*/
   RddSetDefault("DBFCDX")

   SET DATE     TO GERMAN
   SET DECIMALS TO 4
   SET EPOCH    TO 2000
   SET EXACT    ON
   SET SOFTSEEK ON
   SET CENTURY  ON
   SET AUTOPEN  OFF
   SET DELETED  OFF

   SET OOP ON

   _SetGetLogFile( cLog ) ; fErase( cLog )

   DEFINE FONT DlgFont   FONTNAME cFont SIZE nSize + 4

   SET MSGALERT FONTCOLOR TO BLACK
   SET MSGALERT BACKCOLOR TO GRAY
   SET DEFAULT  ICON      TO cIconDef

RETURN

*----------------------------------------------------------------------------*
FUNCTION ProcNL(nVal, cMsg)
*----------------------------------------------------------------------------*
   Default cMsg := ">>> "//"Call from: "

   nVal := iif( Empty(nVal), 0, nVal ) + 1
   cMsg += ProcName(nVal) + "(" + hb_ntos( ProcLine(nVal) ) + ")"
   cMsg += " => " + ProcFile(nVal)

RETURN cMsg

