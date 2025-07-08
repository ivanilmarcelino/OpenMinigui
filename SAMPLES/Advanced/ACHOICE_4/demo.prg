/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * (c) 2023 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 * (c) 2023 Sergej Kiselev <bilance@bilance.lv>
 *
 * ACHOICE() меню на объекте ButtonEx / мenu on the ButtonEx object
 * работа с MAIN MENU / working with MAIN MENU
*/
#define _HMG_OUTLOG
#define PROGRAM   'ACHOICE() - меню на объекте ButtonEx / ACHOICE() - мenu on the ButtonEx object'
#define PROGVER   'Version 0.5 (25.01.2024)'

#include "hmg.ch"
#include "i_winuser.ch"
////////////////////////////////////////////////////////////////////
PROCEDURE Main
   LOCAL aBClr, nH, nW, cVal

   aBClr := TEAL
   nW    := GetDesktopWidth() + 2 * GetBorderWidth()  // System.DesktopWidth
   nH    := 64 + GetTitleHeight() + GetMenuBarHeight() + GetBorderHeight()*2 + 20
   cVal  := MiniGuiVersion() + CRLF + Version() + CRLF + hb_Ccompiler()

   SET FONT TO _GetSysFont(), 14

   DEFINE WINDOW Win1 AT 0, 0 - GetBorderWidth() WIDTH nW HEIGHT nH ;
      TITLE PROGRAM MAIN BACKCOLOR aBClr                            ;
      ON INIT    _wPost( 0)                                         ;
      ON RELEASE _wSend(90)
      This.Cargo := oHmgData()

      myTopMainMenu()     // другой вариант создания меню
                          // another option for creating a menu

      This.Cargo:hIcon := LoadIconByName( "1MG", 64, 64 )        // вместо LOCAL hIcon

      DRAW ICON IN WINDOW Win1 AT 5, 5 HICON This.Cargo:hIcon ;  // вместо LOCAL hIcon
                    WIDTH 64 HEIGHT 64 COLOR aBClr

      @ 2, 80 LABEL Buff VALUE cVal WIDTH Win1.ClientWidth HEIGHT Win1.ClientHeight FONTCOLOR WHITE TRANSPARENT

      ON KEY F1     ACTION MsgAbout(This.Cargo:hIcon,64)
      ON KEY ESCAPE ACTION _wPost(99)

      Sets_Events()    // события на этом окне / events on this window

   END WINDOW

   //CENTER WINDOW Win1
   ACTIVATE WINDOW Win1

RETURN

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
   o:cAvtor        := "Copyright 2023-24 Verchenko Andrey"
   o:cEmail        := "<verchenkoag@gmail.com> Dmitrov, Moscow region"
   o:cPrgInfo1     := "Many thanks for your help: Grigory Filatov <gfilatov@inbox.ru>"
   o:cPrgInfo2     := "Tips and tricks programmers from our forum http://clipper.borda.ru"
   o:aBClrMenu     := aBColor
   o:aBClrMain     := aBClrMain
   o:lDebug        := .T.    // включить отладку в лог-файл

RETURN

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTopMainMenu()

      DEFINE MAIN MENU
         DEFINE POPUP '&Menu examples' FONT "ComSnMs"
            Sets_aMenuItems(.T.)
         END POPUP
         POPUP 'Abo&ut'  FONT "ComSnMs"
            MENUITEM '&About the program'  ACTION _wPost(60,,{60, This.Caption}) ;
            NAME "60_About" ICON "i_About32" FONT "DlgFont"
         END POPUP
         POPUP '&Exit'   FONT "ComSnMs"
            MENUITEM '&Exit the program'  ACTION _wPost(99,,{99, This.Caption}) NAME "0099" ICON "i_Exit32" FONT "DlgFont"
         END POPUP
      END MENU

RETURN NIL

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Sets_aMenuItems(lCrt)
   LOCAL a, o, aa, cn
   DEFAULT lCrt := .F.

     // Name       Item                                   Icon        Font
   a := {   ;
         { 1, '&1. Menu without icon (default)'         , "iSanta1" , "Bold"    }, ;
         { 2, '&2. Menu without icon (with parameters)' , "iSanta1" , "Normal"  }, ;
         { 3, '&3. Menu with icons (icon on the left)'  , "iSanta1" , "Bold"    }, ;
         { 4, '&4. Menu with icons (icon on the right)' , "iSanta1" , "Normal"  }, ;
         { 5, '&5. Menu with icons (icon on the top)'   , "iSanta1" , "Bold"    }, ;
         { 6, '&6. Menu with icons (icon on the bottom)', "iSanta1" , "Normal"  }, ;
         { 7, '&7. Menu with icon + gradient '          , "iSanta1" , "Italic"  }, ;
         { 8, '&8. Menu with icon + gradient HORIZONTAL', "iSanta1" , "Italic"  }, ;
         {  ,                                           ,           ,           }, ;
         { 9, '&9. Large menu without icons'            , "iSanta2" , "Bold"    }, ;
         {10, '&'+'A. Large menu with icons + gradient' , "iSanta2" , "Bold"    }, ;
         {11, '&'+'H. Large menu with icon + image'     , "iHP2"    , "Normal"  }, ;
         {  ,                                           ,           ,           }, ;
         {12, '&'+'B. Menu by array (default)'          , "iHp1"    , "DlgFont" }, ;
         {13, '&'+'C. Array menu with icon on the left of the form'                 , "iHp1", "ComSnMs" }, ;
         {14, '&'+'D. Array menu with icon on the right side of the form + gradient', "iHp1", "ComSnMs" }, ;
         {  ,                                           ,           ,           }, ;
         {15, '&'+'E. Days of the week menu (align text left) '                     , "iHp1", "DlgFont" }, ;
         {16, '&'+'F. Days of the week menu (align text right)'                     , "iHp1", "DlgFont" }, ;
         {50, '&'+'Z. Menu by one-dimensional array'                                , ""    , "Bold"    }  ;
        }
        // {                                                                     }, ;
        // {99, '&Exit'                                   , "i_Exit32", "DlgFont"}  ;

   IF lCrt
      This.Object:Cargo:aMenuItems := a     // в Cargo окна запомнили / in Cargo windows remembered
      FOR EACH aa IN a
          IF Empty(aa) .or. Empty(aa[1])
             SEPARATOR
          ELSEIF aa[1] == 99
             ITEM aa[2] ACTION _wPost(Val(This.Name),,This.Caption) ;
                          NAME &( StrZero(aa[1], 4) ) ICON aa[3] FONT aa[4]
          ELSE
             cn := StrZero(aa[1], 4)
             ITEM aa[2] ACTION {|| _wPost(2,,{This.Name, This.Caption}) } ;
                          NAME &(cn) ICON aa[3] FONT aa[4]
             This.&(cn).Cargo := oHmgData()
             o := This.&(cn).Cargo
             o:nPosM := hb_enumindex(aa)
             o:nItem := aa[1]
             o:cText := aa[2]
             o:cIcon := aa[3]
             o:cFont := aa[4]
             o:oWnd  := This.Object
          ENDIF
      NEXT
   ENDIF

RETURN a

////////////////////////////////////////////////////////////////////////////
// события на этом окне / events on this window
STATIC FUNCTION Sets_Events()

      (This.Object):Event( 0, {|ow|
                               _wSend(1)
                               ow:SetFocus("Buff")
                               DoEvents()
                               wApi_Sleep(50)
                               _PushKey( VK_MENU )
                               _PushKey( VK_RETURN )
                               Return Nil
                               })
      (This.Object):Event( 1, {|ow| // прорисовка иконки сразу на форме
                               DoEvents()
                               SendMessage( ow:Handle, WM_PAINT, 0, 0 )
                               DoEvents()
                               Return Nil
                               })
      (This.Object):Event( 2, {|ow,ky,ap|
                               Local cn := ap[1], aRet
                               Local ct := ap[2], cCap
                               Local nn := Val(cn), oCar
                               cCap := This.&(cn).Caption
                               oCar := This.&(cn).Cargo
                               nn   := iif( nn > 0, nn, oCar:nItem )
                               IF App.Cargo:lDebug  
                                  ? "ow:Event = 2: Item name =", cn, "Caption =", cCap
                                  _o2log(oCar,,"==> This."+cn+".Cargo", .T.)
                                  ? "-", oCar:nPosM,"aItem =", hb_valtoexp(ow:Cargo:aMenuItems[ oCar:nPosM ])
                                  ?
                               ENDIF
                               IF VAL(cn) == 50
                                  aRet := TestZero(nn,ct)
                               ELSE
                                  aRet := TestAchoice(nn,ct)
                               ENDIF
                               // можно обрабатывать aRet в др. событиях
                               ow:Cargo:aRezult := aRet // для других событий окна
                               IF( ky := Empty(aRet) ) ; _wPost(3,,ct)
                               ELSE                    ; _wPost(4,,ct)
                               ENDIF
                               Return Nil
                               })
      (This.Object):Event( 3, {|ow,ky,ct|
                               Local aRet := ow:Cargo:aRezult
                               Local cMsg := HB_NtoS(ky)  
                               cMsg := "aRet= "
                               cMsg += ct + CRLF + HB_ValToExp(aRet) + ";;"
                               cMsg += "NO - SELECTED MODE !;;" + ProcNL()
                               AlertInfo( cMsg, "Return result" )
                               _wPost(0)
                               Return Nil
                               })
      (This.Object):Event( 4, {|ow,ky,ct|
                               Local aRet := ow:Cargo:aRezult
                               Local cMsg := HB_NtoS(ky)  
                               cMsg := "aRet= "
                               cMsg += ct + CRLF + HB_ValToExp(aRet) + ";;"
                               cMsg += "YES - SELECTED MODE ! " + ProcNL()
                               AlertInfo( cMsg, "Return result" )
                               _wPost(0)
                               Return Nil
                               })
      (This.Object):Event(60, {|  | MsgAbout()                  } )  // About the program
      (This.Object):Event(90, {|ow| DestroyIcon(ow:Cargo:hIcon) } )  // убить хендл иконки
      (This.Object):Event(99, {|ow| ow:Release()                } )  // закрыть окно

RETURN NIL

