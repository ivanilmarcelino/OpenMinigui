/*
   Demo for restore a working of the F12 hotkey
   Author: Pablo Cesar Arrascaeta
   http://muzso.hu/2011/12/13/setting-f12-as-a-global-hotkey-in-windows
   http://www.hmgforum.com/viewtopic.php?p=49940#p49940
*/

#include <hmg.ch>

#define KEY_WOW64_64KEY 0x0100

STATIC hexRegKey
STATIC hexRegKeyOld

FUNCTION Main()

   LOCAL lSuccess

   IF hb_osIs64bit()
      hexRegKey := Win_RegRead( "HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows NT\CurrentVersion\AeDebug\UserDebuggerHotKey",, KEY_WOW64_64KEY )
   ELSE
      hexRegKey := Win_RegRead( "HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows NT\CurrentVersion\AeDebug\UserDebuggerHotKey" )
   ENDIF
   hexRegKeyOld := hexRegKey

   DEFINE WINDOW Win_1 AT 157, 162 WIDTH 550 HEIGHT 350 ;
      TITLE "Press F11 and F12 function keys to test" MAIN ;
      ON RELEASE ProcedureOnRelease()

      ON KEY F11 OF Win_1 ACTION MsgInfo( 'TEST F11', "Always Ok" )

      lSuccess := _DefineHotKey( "Win_1", 0, VK_F12, {|| MsgInfo( 'TEST F12', "Ok now" ) } )

      DEFINE LABEL Label_1
        ROW 200
        COL 40
        WIDTH 520
        HEIGHT 48
        VALUE "Before you click the button, try to test by pressing F12 key." + CRLF + If( lSuccess, "Supposed to be working now !!", "Certainly you'll need to reboot your PC and try it again." )
        FONTNAME "Arial"
        FONTSIZE 10
        FONTBOLD .T.
        VISIBLE !( hexRegKey == 0 )
        FONTCOLOR RED
      END LABEL

      DEFINE BUTTON Button_1
        ROW 140
        COL 60
        WIDTH 400
        HEIGHT 28
        ACTION ChngReg()
        CAPTION If( hexRegKey == 0, "Change your UserDebuggerHotKey from your Windows Registry", "Return back UserDebuggerHotKey value" )
      END BUTTON

   END WINDOW

   CENTER WINDOW Win_1
   ACTIVATE WINDOW Win_1

RETURN NIL


FUNCTION ChngReg()

   LOCAL cNewCaption

   IF hexRegKey == 0
      cNewCaption := "Return back UserDebuggerHotKey value"
      hexRegKey := 0x2F
   ELSE
      cNewCaption := "Change your UserDebuggerHotKey from your Windows Registry"
      hexRegKey := 0
      SetProperty( "Win_1", "Label_1", "ENABLED", .T. )
      SetProperty( "Win_1", "Label_1", "CAPTION", "To disable F12 you'll need to reboot your PC." + CRLF + "F12 still working until your rebooting..." )
   ENDIF

   SetProperty( "Win_1", "Button_1", "CAPTION", cNewCaption )

RETURN NIL

/*
FUNCTION SetRegValueAsAdmin( cChngKey )

   LOCAL cCurPath := cFilePath( hb_ProgName() )
   LOCAL cRunParam := cCurPath + "\MyRegWrite.vbs"
   LOCAL cFileRun := GetSystemFolder() + "\CScript.exe"
   LOCAL cRegText := 'Set wshShell = CreateObject( "WScript.Shell" )' + CRLF

   cRegText := cRegText + 'wshShell.RegWrite "HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows NT\CurrentVersion\AeDebug\UserDebuggerHotKey", ' + cChngKey + ', "REG_DWORD"' + CRLF + 'Set wshShell = Nothing'
   hb_MemoWrit( cRunParam, cRegText )

   ShellExecuteEx( , 'runas', cFileRun, cRunParam, , SW_HIDE )

   DO WHILE .T.

      hb_idleSleep( 1 ) // Gives time to run and then deletes the file

      DELETE File( cRunParam )
      If ! File( cRunParam )
         EXIT
      ENDIF

   ENDDO

RETURN NIL
*/

FUNCTION ProcedureOnRelease()

   LOCAL cRegKey := If( hexRegKey == 0, "0", "47" )

   If !( hexRegKey == hexRegKeyOld )
      Win_RegWrite("HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows NT\CurrentVersion\AeDebug\UserDebuggerHotKey", Val(cRegKey), "N", KEY_WOW64_64KEY )
      //SetRegValueAsAdmin( cRegKey )
      hb_idleSleep( .2 )
      MsgExclamation( "You will need to reboot this PC to takes effects.", "Boot required" )
   ENDIF

RETURN NIL
