/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * (c) 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 * (c) 2024 Sergej Kiselev <bilance@bilance.lv>
 *
 * ACHOICE() меню на объекте AVI + LABEL / ACHOICE() menu on AVI + LABEL object
 * работа с MAIN MENU / working with MAIN MENU
*/
#define _HMG_OUTLOG

ANNOUNCE RDDSYS
#define PROGRAM   'ACHOICE() меню на объекте AVI + LABEL / ACHOICE() menu on AVI + LABEL object'
#define PROGVER   'Version 0.3 (18.01.2024)'

#include "hmg.ch"
#include "i_winuser.ch"

////////////////////////////////////////////////////////////////////////////
FUNCTION Main(...)
   LOCAL nH, nW, cVal
   LOCAL cIcon := App.Cargo:cIconDef

   nW    := GetDesktopWidth() + 2 * GetBorderWidth()  // System.DesktopWidth
   nH    := 64 + GetTitleHeight() + GetMenuBarHeight() + GetBorderHeight()*2 + 20
   cVal  := MiniGuiVersion() //+ SPACE(5) + MG_Version(.F.) + SPACE(5) + HB_NtoS(MG_Version(.T.))
   cVal  += CRLF + Version() + CRLF + hb_Ccompiler()

   SET FONT TO _GetSysFont(), 14

   DEFINE WINDOW wMain AT 0, 0 - GetBorderWidth() WIDTH nW HEIGHT nH    ;
      TITLE PROGRAM MAIN BACKCOLOR App.Cargo:aBClrMain                  ;
      ON INIT    _wPost( 0)                                             ; // выполняется после инициализации окна
      ON RELEASE _wSend(90)                                             ; // выполняется перед разрушением окна
      ON INTERACTIVECLOSE ( _wSend("IsWindowClose"), This.Cargo:lClose )  // закрытие окна по [x]

      This.Cargo := oHmgData()
      This.Cargo:cTxtClose := "Close [x]"
      This.Cargo:lClose := .T.

      DEFINE MAIN MENU
         DEFINE POPUP '&Menu examples' FONT "ComSnMs"
            Sets_aMenuItems(.T.)
         END POPUP
         DEFINE POPUP 'Abo&ut'         FONT "ComSnMs"
            MENUITEM '&About the program' ACTION _wPost(20) ICON "i_About32" FONT "ComSnMs"
         END POPUP
      END MENU

      This.Cargo:cIcon := cIcon
      This.Cargo:hIcon := LoadIconByName( cIcon, 64, 64 )

      DRAW ICON IN WINDOW wMain AT 10, 10 HICON This.Cargo:hIcon  WIDTH 64 HEIGHT 64 COLOR This.BackColor

      @ 2, 90 LABEL Buff VALUE cVal WIDTH This.ClientWidth HEIGHT This.ClientHeight FONTCOLOR WHITE TRANSPARENT

      ON KEY F1     ACTION _wPost("About")
      ON KEY ESCAPE ACTION _wPost(99)

      Sets_Events()            // события на этом окне / events on this window

   END WINDOW

   //CENTER WINDOW wMain
   ACTIVATE WINDOW wMain

RETURN NIL

/////////////////////////////////////////////////////////////////
// эта процедура ВСЕГДА стартует ПЕРВОЙ !
// this procedure ALWAYS starts FIRST!
INIT PROCEDURE Sets_ENV()
   LOCAL aBColor   := { 195, 224, 133 }
   LOCAL aBClrMain := { 215, 166,   0 }
   LOCAL o

   SET LANGUAGE TO RUSSIAN // язык
   SET CODEPAGE TO RUSSIAN // кодовая страница

   SET MULTIPLE QUIT

   //RddSetDefault("DBFCDX")    // резерв

   o := SetsEnv()               // -> demo_util.prg

   fErase( o:cLog )

   ? REPL("=",20) + " Start" , HB_DATETIME() , REPL("=",20)
   ? MG_Version(), MG_Version(.T.), MG_Version(.F.)

   o:cTitle        := ALLTRIM(SUBSTR(PROGRAM, RAT( "/", PROGRAM) + 1))
   o:cTitleRu      := SUBSTR(PROGRAM, 1, RAT( "/", PROGRAM) - 1)
   o:cVersion      := PROGVER
   o:cAvtor        := "Copyright 2024 Verchenko Andrey"
   o:cEmail        := "<verchenkoag@gmail.com> Dmitrov, Moscow region"
   o:cPrgInfo1     := "Many thanks for your help: Grigory Filatov <gfilatov@inbox.ru>"
   o:cPrgInfo2     := "Tips and tricks programmers from our forum http://clipper.borda.ru"
   o:aBClrMenu     := aBColor
   o:aBClrMain     := aBClrMain
   o:lDebug        := .T.    // включить отладку в лог-файл

RETURN

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Sets_Events()

   WITH OBJECT This.Object
      :Event({0, "Start"}, {|ow|
                 _wSend("Paint")
                 ow:SetFocus("Buff")
                 DoEvents()
                 wApi_Sleep(50)
                 _PushKey( VK_MENU )
                 _PushKey( VK_RETURN )
                 Return Nil
                 })
      :Event({1, "Paint"}, {|ow| // прорисовка иконки сразу на форме
                 DoEvents()
                 SendMessage( ow:Handle, WM_PAINT, 0, 0 )
                 DoEvents()
                 Return Nil
                 })
      :Event( 2, {|ow,ky,ap| // 3-параметр массив
                 Local aRet, cItm, cCap
                 Local nn := ap[1]
                 Local ct := ap[2]
                 cItm := StrZero(nn, 4)
                 cCap := This.&(cItm).Caption
                 ? "*=> Menu Event:",ky, cItm
                 ? " 1. ap[1]", ct
                 ? " 2.  cCap", cCap
                 SET WINDOW THIS TO ow:Name
                 IF VAL(cItm) == 50
                    aRet := TestZeroAchoice(nn,cCap)
                 ELSE
                    aRet := TestAchoice(nn,cCap)
                 ENDIF
                 SET WINDOW THIS TO
                 // можно обрабатывать aRet в др. событиях
                 ow:Cargo:aRezult := aRet // для других событий окна
                 IF( ky := Empty(aRet) ) ; _wPost(3)
                 ELSE                    ; _wPost(4)
                 ENDIF
                 Return Nil
                 })
      :Event( 3, {|ow|
                 Local aRet := ow:Cargo:aRezult
                 AlertInfo("aRet= " + HB_ValToExp(aRet) + ";;" + ;
                           "NO - SELECTED MODE ! " + ;
                           ProcNL(), "Return result" )
                 _wPost("Start")
                 Return Nil
                 })
      :Event( 4, {|ow|
                 Local aRet := ow:Cargo:aRezult
                 AlertInfo("aRet= " + HB_ValToExp(aRet) + ";;" + ;
                           "YES - SELECTED MODE ! " + ;
                           ProcNL(), "Return result" )
                 _wPost("Start")
                 Return Nil
                 })
      :Event({20, "About"}, {|ow| MsgAbout(ow:Cargo:hIcon,72) } )

      :Event(90, {|ow|
                 DestroyIcon(ow:Cargo:hIcon)  // убить хендл иконки
                 Return Nil
                 })
      :Event({98, "IsWindowClose"}, {|ow|
                 Local o := ow:Cargo
                 o:lClose := AlertYesNo(o:cTxtClose)
                 IF ! o:lClose
                    _wPost("Start")
                 ENDIF
                 Return Nil
                 })
      :Event(99, {|ow| ow:Release() } )  // закрыть окно
   END WITH

RETURN NIL

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Sets_aMenuItems(lCrt)
   LOCAL a, aa
   DEFAULT lCrt := .F.

     // Name       Item                                   Icon         Font
   a := {   ;
         { 1, '&1. Menu without avi (default)'          , "iHP32x1"  , "Bold"    }, ;
         { 2, '&2. Menu without avi (icon on the left)' , "iHP32x2"  , "Bold"    }, ;
         { 3, '&3. Menu with avi (icon on the right)'   , "iHP32x2"  , "DlgFont" }, ;
         { 4, '&4. Menu with avi (avi on the right)'    , "iHP32x2"  , "DlgFont" }, ;
         {  ,                                           ,            ,           }, ;
         { 5, '&5. Menu with avi (icon on the right)'   , "iHP32x1"  , "Normal"  }, ;
         { 6, '&6. Menu with avi (avi on the right)'    , "iHP32x1"  , "Normal"  }, ;
         {  ,                                           ,            ,           }, ;
         { 7, '&7. Large menu without avi (default)'    , "iHP32x2"  , "Bold"    }, ;
         { 8, '&8. Large menu without avi + icon'       , "iHP32x2"  , "Normal"  }, ;
         { 9, '&9. Large menu with avi right + icon'    , "iHP32x2"  , "Bold"    }, ;
         {10, '&'+'A. Large menu with avi left + icon'  , "iHP32x2"  , "Italic"  }, ;
         {                                                                       }, ;
         {50, '&'+'Z. Menu by one-dimensional array'    , ""         , "Bold"    }, ;
         {                                                                       }, ;
         {99, '&Exit'                                   , "i_Exit32" , "DlgFont" }  ;
        }

   IF lCrt
      FOR EACH aa IN a
          IF Empty(aa) .or. Empty(aa[1])
             SEPARATOR
          ELSEIF aa[1] == 99                        // передача массива
             ITEM aa[2] ACTION _wPost(Val(This.Name),, {99, This.Caption}) ;
                          NAME &( StrZero(aa[1], 4) ) ICON aa[3] FONT aa[4]
          ELSE                                      // передача массива
             ITEM aa[2] ACTION {|| _wPost(2,, {Val(This.Name), This.Caption}) } ;
                          NAME &( StrZero(aa[1], 4) ) ICON aa[3] FONT aa[4]
          ENDIF
      NEXT
   ENDIF

RETURN a

