MINIGUI - Harbour Win32 GUI library Demo

��������:
~~~~~~~~~~~~
���� ������ Demo_timer.hbp - ��������� ������� ��������� � ���������.
������ ������������� ��� ��������� ��������� ������� ������� �� �������.
��� ����� ��������� � ������������ ��� ���� �������.
������ ������� 23.09 � ����.
���� ������ ������ �� ����� ������ � ������� � 2013 �� 2023 �����. 

� ������� �������� ��������� ���������� ��� ������ ���������:

1) ������ ������ ����� ����� ���������. ����������� HIDE ���� �������,
   ������ main.prg

2) ������ ��������/�������� ������ ��� ������� ���������, �������� 
   ������ main_check.prg

3) ������ ������ ������� ��������� - nMode := App.Cargo:nMode
   ����� ��� ������ � Demo.hbp
   # �������� ��������� ���� �� �������� ����������� ����, ���� ������������ ����� -run
   -runflag=4
   � �������� ������� ������ - Demo_timer.exe 4

4) ������������� ����� ���������� ������ ���������.
   ������� MAIN ����� (MAIN show), �� ������ ���������� - ������ �� ������ �����, 
   ����� ������ Start/
   ���������� ��������� ����� �� ������ Sample-4
   ������ ����� ���������� � ����, ��� ����� ��������� ��������� �� ���������� ���������.

5) ������ ���-����� _msg.log ������ ������ ���������, ����� ������ ���������� � ������ ���.
   ����� ������� ���� ��� ����� ��������� ���������.

6) ������ ���-���� \ErrorsLog\_events-USER-XXX.log ������� ���������, �����/�����/������/������ �����

7) SendMessageData.exe - ��������� ������� ������ ������� ��������� � ���� ���������
   � ������� �������� ��������� �� ������ ��������.

8) ������ ���������� ���-����� ��������� ��������� �� ������ ��������.
   ����� ���������� ��������� � ��������� ���� ���������. 
   ����� ���������� ��������� � ���� ���������. 
   ����� ������� ���� ��� ����� ��������� ���������.

9) ��� ������� � ���������, 4 ������ ��� �������� ������� ��������� � ������ �� ���������
   �� ��������� ��������� �������.

10) ������ ������ � ���� � ��� ���� �� ������ ��� ������. ���� ����������� ������
    ������� ��������� ������� ����� � ��������� �� ���� ������������ ���������.

11) ������������� � ��������� ini-����� �������� ��� ������������ � �������������
    cnfg-���� ��������� ��� ���������� ��������� ������ ����/�������� ��� ������ � �.�.

12) ������ �������� ������ � �������� ��� �������������� ����� ������ ����.

13) �������� ��������� ����� ������������/���������� ��� ������ ��������� WM_COPYDATA
    ��������� ��������� SendMessageData.exe

14) � ��������� ���� ����� ������������ ������ ����� ����� RU � EN, �������� table3.prg
    aCurrLang := { hb_SetCodepage(), hb_CdpSelect(), Hb_LangSelect() }  // ������� ����
    SET CODEPAGE TO UKRAINIAN     // ���������� ���� � ���� ����
    SET LANGUAGE TO UKRAINIAN      
    .....
    oWin:aCurrLang := aCurrLang   // ��������� ������� ����, ��� ����������
                                  // ��������� � �������������� � TsbViewer.prg

���������������:
~~~~~~~~~~~~~
��� ��������� �������� ����������, � ����� �������� ������������ � ����������������,
�� ��� ���, ��� ��� �� ���������� �����-���� �������, � ������� ����� �� ���������
�� ������������� (����� �� ��������� �������� � ��������� ���������).

���������� ����������:
~~~~~~~~~~~~~~~~~~~~
�����: Andrey Verchenko
E-Mail: <verchenkoag@gmail.com>


DESCRIPTION:
~~~~~~~~~~~~
This project Demo_timer.hbp is a ready-made program with timers.
The project demonstrates how to correctly create large projects on MiniGui.
It can be expanded and customized to suit your projects.
MiniGui version 23.09 and higher.
This project was created based on the experience of working with MiniGui from 2013 to 2023.

The project shows some implementations for the program to work:

1) Run ONLY one copy of the program. The HIDE of the launch window is taken into account,
    module main.prg

2) List of checks/opening of files when starting the program, see
    module main_check.prg

3) Different program launch modes - nMode := App.Cargo:nMode
    Build option in Demo.hbp
    # pass a single flag to the output executable if the -run option is used
    -runflag=4
    As an example, launch - Demo_timer.exe 4

4) Use of changing the program screen resolution.
    Call the MAIN form (MAIN show), click the resolution button - change to the desired screen,
    next button Start/
    You can view the result by clicking the Sample-4 button
    You can always see for yourself what the program will look like on the customer�s computer.

5) Recording the log file _msg.log for each program session, outputting the necessary variables to a single log.
    You can delete this log through the program settings.

6) Single log file \ErrorsLog\_events-USER-XXX.log of program events, start/exit/error/sleep mode

7) SendMessageData.exe - a program for sending data to the main program and a message window
    which receives messages from other programs.

8) Recording a separate log file of incoming messages from other programs.
    Display incoming messages in a separate program window.
    Show incoming messages in the program tray.
    You can delete this log through the program settings.

9) Three timers in the program, 4 timers to control program idle time and exit from the program
    after the specified time has elapsed.

10) Recording the error in a file and what was on the screen when the error occurred. Possibility to call
     make an archive of an external program and send it to the program developer�s website.

11) Using an ini file of settings for the user in the program and encrypted
     cnfg program file for placing various menu data/arrays for tables, etc.

12) Different table options and a card for editing one database record.

13) Passing messages between applications/processes using the WM_COPYDATA message
    Separate program SendMessageData.exe

14) In a separate window you can use other languages besides RU and EN, see table3.prg
     aCurrLang := { hb_SetCodepage(), hb_CdpSelect(), Hb_LangSelect() } // current language
     SET CODEPAGE TO UKRAINIAN // additional language in this window
     SET LANGUAGE TO UKRAINIAN
     .....
     oWin:aCurrLang := aCurrLang // remember the current language, this variable
                                   // transmitted and processed in TsbViewer.prg

SPREADING:
~~~~~~~~~~~~~
This program is free and can be freely copied and distributed,
as long as it is not modified in any way and no fees are charged
for distribution (small shipping and handling fee is acceptable).

CONTACT INFORMATION:
~~~~~~~~~~~~~~~~~~~~
Author: Andrey Verchenko
E-Mail: <verchenkoag@gmail.com>
