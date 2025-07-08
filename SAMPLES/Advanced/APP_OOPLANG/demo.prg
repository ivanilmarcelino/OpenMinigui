/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2023 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 * Copyright 2023 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2023 Grigory Filatov <gfilatov@inbox.ru>
 *
 * Окна с разными языками / Windows with different languages
*/

#define PROGRAM   "Windows with different languages - 01.12.2023"
#define _HMG_OUTLOG
#include "hmg.ch"

REQUEST HB_CODEPAGE_DEWIN                        // немецкий язык
REQUEST HB_CODEPAGE_RU1251, HB_CODEPAGE_RU866    // русский язык
REQUEST HB_CODEPAGE_UA1251, HB_CODEPAGE_UA866    // украинский язык
REQUEST HB_LANG_BEWIN                            // белорусский язык
REQUEST HB_CODEPAGE_LVWIN, HB_LANG_LV            // латышский язык
REQUEST HB_CODEPAGE_BGWIN                        // болгарский язык

///////////////////////////////////////////////////////////////////
INIT PROCEDURE Sets_EVN()
   LOCAL cFont := "DejaVu Sans Mono", nSize := 14
   LOCAL cLog  := "_msg.log", cLog2 := "_Utf8.log"
   LOCAL cIconDef := "1MG"
   LOCAL aBClrDlg := { 141, 179, 226 }
   LOCAL aBColor  := {  94,  59, 185 } 

   SET MULTIPLE OFF
   SET DATE  TO GERMAN // or SET DATE FORMAT "DD.MM.YYYY"
   SET EPOCH TO 2000
   SET EXACT    ON
   SET SOFTSEEK ON
   SET CENTURY  ON
   // ------------
   SET OOP ON
   // ------------

   // основной Default фонт программы
   SET FONT TO cFont, nSize
   // фонт для HMG_Alert() и Alert...() ф-ий
   DEFINE FONT DlgFont FONTNAME cFont SIZE nSize 

   // латышский фонт программы
   DEFINE FONT LVArial     FONTNAME "Arial" SIZE nSize + 2      CHARSET 186  // BALTIC_CHARSET
   DEFINE FONT LVArialBold FONTNAME "Arial" SIZE nSize + 2 BOLD CHARSET 186  // BALTIC_CHARSET

   // other setting
   SET DEFAULT ICON TO cIconDef
   SET MSGALERT BACKCOLOR TO aBClrDlg
   SET MENUSTYLE  EXTENDED
   SET NAVIGATION EXTENDED
   SET WINDOW MODAL PARENT HANDLE ON  // окна Modal получают родителя - активное окно MiniGui с версии 23.04.4
                                      // Modal windows get a parent - the active MiniGui window since version 23.04.4

   _SetGetLogFile( cLog2 ) ; DELETEFILE(cLog2) // проверка в лог-файл
   ? hb_utf8Chr( 0xFEFF )

   mySetLang(7)                                // латышский язык
   ? hb_StrToUtf8( myGetLang(,"TEXT") )
   ? REPL("-.",40)
   mySetLang(5)                                // сербский язык
   ? hb_StrToUtf8( myGetLang(,"TEXT") )
   ? REPL("-.",40)
   mySetLang(6)                                // немецкий язык
   ? hb_StrToUtf8( myGetLang(,"TEXT") )

   // new log filename for debug output
   _SetGetLogFile( cLog ) ; DELETEFILE(cLog)
   ? MiniGuiVersion()

   mySetLang(0)    // установим язык по умолчанию / set the default language

   App.Cargo := oHmgData()
   App.Cargo:cTitle        := PROGRAM
   App.Cargo:cVersionHb    := Version()
   App.Cargo:cVersionHmg   := MiniGuiVersion()
   App.Cargo:aBColor       := aBColor 
   App.Cargo:cFormGotFocus := ""               // текущий фокус на форме / current focus on form
   App.Cargo:cFName        := cFont
   App.Cargo:nFSize        := nSize
   App.Cargo:aLangOld      := { hb_SetCodepage(), hb_CdpSelect(), Hb_LangSelect() }    // запомним язык по умолчанию

RETURN 

///////////////////////////////////////////////////////////////////
PROCEDURE Main()
   LOCAL i, j, y, x, w, h, p, g
   LOCAL aFntClr, aBtnBClr, nHBtn, aBtn

   y := x := 20 ; nHBtn := 40 ; g := 20
   aFntClr  := { BLUE  , BLACK  }
   aBtnBClr := { SILVER, YELLOW }
   aBtn := { "RUSSIAN" , "UKRAINIAN", "BYELORUSSIAN", "BULGARIAN",;
             "SERBIAN" , "GERMAN", "LATVIAN", "ITALIAN" , "Exit" }
   h := Len(aBtn) * ( nHBtn + g ) + g*3

   DEFINE WINDOW Form_Main AT y,x WIDTH 500 HEIGHT h  ;
      TITLE App.Cargo:cTitle MAIN                     ;
      BACKCOLOR App.Cargo:aBColor                     ;
      ON INIT    {|| _wPost(0,, This.Name) }          ;
      ON RELEASE {|| _wSend(90) /* _wPost(90)-нельзя делать/mustn't do */ }     

      This.Cargo := oHmgData()
      w := This.ClientWidth
      h := nHBtn

      @ 0,0 Label Buff Value "" AUTOSIZE

      FOR i := 1 TO Len(aBtn)
          j := "Btn_" + StrZero(i, 2)
          p := aBtn[i]

         @ y, x BUTTONEX &j WIDTH w - g*2 HEIGHT h CAPTION p     ;
           ICON NIL FLAT NOXPSTYLE HANDCURSOR NOTABSTOP          ;
           BOLD FONTCOLOR aFntClr[1] BACKCOLOR aBtnBClr[1]       ;
           ON MOUSEHOVER ( This.Fontcolor := aFntClr[2], This.Backcolor := aBtnBClr[2] ) ;
           ON MOUSELEAVE ( This.Fontcolor := aFntClr[1], This.Backcolor := aBtnBClr[1] ) ;
           ACTION  {| | This.Enabled := .F., _wPost(This.Cargo:nPost,This.Index) }       ; // в событии будет среда This для кнопки
           ON INIT {|o| // инициализация кнопки
                        This.Cargo := oKeyData()  // создать объект (контейнер) для этой кнопки
                        o := This.Cargo
                        // положим на кнопку нужные данные
                        o:nPost := 1  // событие 1
                        o:nBtn  := i
                        o:cCapt := aBtn[i]
                        o:aBClr := aBtnBClr
                        o:cObj  := This.Name
                        o:cForm := ThisWindow.Name
                        o:oWnd  := ThisWindow.Object  // объект окна
                        o:oCtl  := This.Object        // объект контола кнопки
                        Return Nil
                    }          // ON INIT надо задавать только блоком кода

          // ----- второй вариант как можно делать / second option how to do it ----- 
          IF i % 2 == 0
             This.&(j).Action := {|| This.Enabled := .F., _wPost(2,,This.Name) } // в событии будет среда This для кнопки
          ENDIF

          IF i == Len(aBtn)
             This.&(j).Caption := "Exit"
             This.&(j).Action  := {|| _wPost(99) }
          ENDIF
          y += h + g
      NEXT

      Sets_Event2Main()

   END WINDOW

   ACTIVATE WINDOW Form_Main

Return

///////////////////////////////////////////////////////////////////
STATIC FUNCTION Sets_Event2Main()
   LOCAL o := This.Object

   o:Event( 0, {|ow|// start ON INIT
                    Local o, aButt, aType
                    aType := ow:GetListType()                  // список типов контролов на окне
                    aButt := ow:GetObj4Type("OBUTTON")         // список объектов кнопок тип OBUTTON
                    ? ProcNL(), "aType=", aType, HB_ValToExp(aType), "aButt=",aButt
                    FOR EACH o IN ow:GetObj4Type("LAB,BUTT")   // объекты LABEL, BUTTON и OBUTTON
                        ? "   " + HB_NtoS(hb_enumindex(o)), o:Type, o:Name
                    NEXT
                    ow:SetFocus('Buff') 
                    Return Nil
                 })

   o:Event( 1, {|obtn,ky,cn| // button 1,3,5              
                          Local o := This.Cargo     // берём данные с кнопки то что положили ранее
                          Local cBtn := o:cObj
                          Local oWnd := o:oWnd
                          Local ow   := obtn:Window
                          Default cn := obtn:Name
                          ? ProcNL(), obtn,"ky=", ky, "cn=", cn, "|", ow:Name, o:nBtn, cBtn, o:cCapt, o:cForm
                          _o2Log(o,  20, "==> .T. o := This.Cargo: ", .T.)        // проверка в лог-файл
                          my_Standard(o) 
                          oWnd:Enabler(cBtn, .T.)
                          oWnd:SetFocus('Buff')
                          Return Nil
                 })
   // ----- второй вариант как можно делать / second option how to do it ----- 
   o:Event( 2, {|ow,ky,cn| // button 2,4,6              
                          Local o, cBtn   
                          // берём данные с кнопки то что положили ранее
                          o := GetProperty(ow:Name, cn, "Cargo")
                          cBtn := o:cObj
                          ? ProcNL(), ow, "ky=", ky, "cn=", cn, "|", ow:Name, o:nBtn, cBtn, o:cCapt, o:cForm
                          _o2Log(o,  20, "==> .T. o := This.Cargo: ", .T.)        // проверка в лог-файл
                          my_Standard(o) 
                          ow:Enabler(cBtn, .T.)
                          ow:SetFocus('Buff')
                          Return Nil
                 })

   o:Event(90, {|ow,ky| _LogFile(.T., ">>> ON RELEASE WINDOW: "+ow:Name+" - close .EXE", ky )  } )

   o:Event(99, {|ow| ow:Release() })

RETURN Nil

///////////////////////////////////////////////////////////////////
FUNCTION my_Standard(oBtn)
   LOCAL cTitle, aBClr, nY, nX, nW, nH, hW, nI, nG, cForm, cText
   LOCAL nLang, cSetCP, cSelCdp, cLngSel, cMsg, aCurrLang, cCapt
   LOCAL cFontName := App.Cargo:cFName

   // текущий язык программы / current program language 
   aCurrLang := { hb_SetCodepage(), hb_CdpSelect(), Hb_LangSelect() }  
   //cMsg := "Current program language;"
   //cMsg += HB_ValToExp(aCurrLang) + ";;"
   //cMsg += ProcNL() + ";" + ProcNL(1)
   //AlertInfo(cMsg)

   nI    := oBtn:nBtn   // номер кнопки
   cForm := "Form_" + HB_NtoS(nI)

   IF _IsWindowDefined(cForm)
      hW := GetFormHandle(cForm)
      ? "*Form =", cForm, hW
      AlertStop("Window [" + cForm + "] is already open !")
      RETURN Nil
   ENDIF

   cTitle  := "WINDOW STANDARD " + cForm 
   aBClr   := {178,162,199}
   nY      := nI * 30 
   nX      := GetProperty( oBtn:cForm, "Col" )  
   nX      += GetProperty( oBtn:cForm, "Width" ) + nI * 30 
   nW      := nH := 530
   nG      := 20
   cCapt   := oBtn:cCapt 
   nLang   := oBtn:nBtn         // номер кнопки
   mySetLang(nLang)             // установить язык по кнопке
   cSetCP  := hb_SetCodepage()
   cSelCdp := hb_CdpSelect()
   cLngSel := Hb_LangSelect()
   cMsg    := SPACE(3)+ "[ " + cSetCP + "/" + cSelCdp + + "/"
   cMsg    += cLngSel + " ]"

   IF nLang == 7  // латышский фонт программы
      cFontName := "LvArialBold"
   ENDIF

   DEFINE WINDOW &cForm AT nY,nX WIDTH nW HEIGHT nH ;
      TITLE cTitle + cMsg                           ;
      WINDOWTYPE STANDARD TOPMOST                   ;
      NOMAXIMIZE NOSIZE                             ;
      BACKCOLOR aBClr                               ;
      ON INIT      {|| This.Topmost := .F., DoEvents(), _wPost(0) }  ;
      ON GOTFOCUS  {|| App.Cargo:cFormGotFocus := This.Name ,;          // returning focus to the form
                       myLangRecover(cForm,cSetCP,cSelCdp,cLngSel) } ;  
      ON LOSTFOCUS {|| myLangRecoverLost(This.Name  ,;                  // removing focus from a form
                       This.Cargo:aCurrLang) }                       ;  
      ON RELEASE   {|| _wSend(9) /*_wPost(9)-нельзя делать*/ }           // executed before destroying the window

      nY := nX := 0
      nX += nG
      nW := This.ClientWidth  
      nH := This.ClientHeight 

      This.Cargo := oHmgData()
      This.Cargo:aCurrLang := aCurrLang           // запомнить текущий язык программы
                                                  // remember the current program language
      
      @ 0,0 Label Buff Value cCapt WIDTH nW HEIGHT 28 SIZE 20 ;
            FONT cFontName CENTERALIGN VCENTERALIGN 
      nY += This.Buff.Height + 5

      cMsg := hb_langErrMsg(2) + ":"
      @ nY, nG Label Label_1 Value cMsg WIDTH nW-nG*2 HEIGHT App.Cargo:nFSize*1.3 ;
               FONT cFontName VCENTERALIGN TRANSPARENT
      nY += This.Label_1.Height + 2

      cMsg := hb_langErrMsg(3) + SPACE(30)
      @ nY, nG GETBOX Get_1 WIDTH nW-nG*2 HEIGHT App.Cargo:nFSize*1.9 VALUE cMsg ;
               FONT cFontName 
      nY += This.Get_1.Height + nG/2

      cText := "What language is in the window ?"
      @ nY, nX BUTTON Btn_1 CAPTION cText WIDTH nW-nG*2 HEIGHT 40 ;
               FONT cFontName ;
               ACTION {|| This.Enabled := .F., _wPost(1,,This.Name) }
      nY += This.Btn_1.Height + 5

      cText := myGetLang(cTitle,"TEXT")      // Какой язык в окне / What language is in the window ?

      @ nY, nX EDITBOX Edit_Memo WIDTH nW-nG*2 HEIGHT nH - nG - nY ;
        VALUE cText READONLY FONT cFontName NOHSCROLL SIZE 10                         

      IF nLang == 7  // латышский фонт программы
         // Надо в Region ставить латышский язык LV1257
         hb_memowrit("_lv.txt", hb_utf8Chr( 0xFEFF )+CRLF+hb_StrToUtf8(cText))
         This.Edit_Memo.Fontcolor := BLUE
         This.Label_1.Fontcolor   := BLUE
         This.Get_1.Fontcolor     := BLUE
         This.Btn_1.Fontcolor     := BLUE
      ENDIF

      WITH OBJECT This.Object
         :Event(0, {|ow,ky,cn| ow:SetFocus('Buff') , ky := cn                     } )
         :Event(1, {|ow,ky,cn| ky := This.Btn_1.Caption , myGetLang(ky,"SAY") ,;
                               ow:Enabler(cn, .T.) , ow:SetFocus('Buff')          } )
         //:Event(9, {|| mySetLang(0, "switch to English", "вариант-1")           } )
         :Event(9, {|ow,ky,cn| myLangOld(ow,ky,cn)                                } )
      END WITH

   END WINDOW

   ACTIVATE WINDOW &cForm

RETURN Nil

//////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myLangOld(ow,ky,cn)             // восстановить язык по умолчанию
   LOCAL p1,p2, aLang := App.Cargo:aLangOld
   ? ProcNL() , "=== ON RELEASE: "+ow:Name, ow,ky,cn, HB_ValToExp(aLang)
   IF HB_IsArray(aLang)
      IF LEN(aLang) >= 3
         p1 := SUBSTR(aLang[3],1,AT(".",aLang[3])-1)
         p2 := SUBSTR(aLang[3],AT(".",aLang[3])+1)
         ?? "|", p1, p2
         IF LEN(p1) > 0 .AND. LEN(p2) > 0
            hb_LangSelect(p1,p2)   
         ENDIF
         hb_SetCodepage(aLang[1])
         hb_CdpSelect(aLang[2])
         DO EVENTS
      ENDIF
   ENDIF
   ?? "|check:", hb_LangSelect(), hb_SetCodepage(), hb_CdpSelect()
RETURN NIL

///////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myLangRecover(cForm,cSetCP,cSelCdp,cLngSel) // возврат фокуса на форму
   LOCAL p1, p2
   p1 := SUBSTR(cLngSel,1,AT(".",cLngSel)-1)
   p2 := SUBSTR(cLngSel,AT(".",cLngSel)+1)
   hb_LangSelect(p1,p2)   
   hb_SetCodepage(cSetCP)
   hb_CdpSelect(cSelCdp)
   DO EVENTS
   ? ProcNL(), "cForm=",cForm,"|", p1, p2, "|",cSetCP,cSelCdp,cLngSel
   ? "        check:", hb_LangSelect(), hb_SetCodepage(), hb_CdpSelect()
RETURN NIL

//////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myLangRecoverLost(cForm,aLang)   // снятие фокуса с формы
   LOCAL p1,p2, aLangOld := App.Cargo:aLangOld
   // aLang := aLangOld - можно и так делать / you can do that too 
   ? ProcNL(), "cForm=",cForm, HB_ValToExp(aLang)
   IF HB_IsArray(aLang)
      IF LEN(aLang) >= 3
         p1 := SUBSTR(aLang[3],1,AT(".",aLang[3])-1)
         p2 := SUBSTR(aLang[3],AT(".",aLang[3])+1)
         ?? "|", p1, p2
         IF LEN(p1) > 0 .AND. LEN(p2) > 0
            hb_LangSelect(p1,p2)   
         ENDIF
         hb_SetCodepage(aLang[1])
         hb_CdpSelect(aLang[2])
         DO EVENTS
      ENDIF
   ENDIF
   ?? "|check:"
   ?? hb_LangSelect(), hb_SetCodepage(), hb_CdpSelect()

RETURN NIL

////////////////////////////////////////////////////////////////
FUNCTION mySetLang(nVal)      
   DEFAULT nVal := 0

   IF nVal == 0
      SET CODEPAGE TO ENGLISH       // аналог HB_CDPSELECT( "EN" )
      SET LANGUAGE TO ENGLISH       // аналог HB_LANGSELECT( "EN" )
   ELSEIF nVal == 1
      SET CODEPAGE TO RUSSIAN
      SET LANGUAGE TO RUSSIAN
   ELSEIF nVal == 2
      SET CODEPAGE TO UKRAINIAN     // аналог hb_SetCodepage( "UA1251" )
      SET LANGUAGE TO UKRAINIAN     // аналог hb_CdpSelect( "UA1251" )
   ELSEIF nVal == 3
      SET CODEPAGE TO RUSSIAN       // не нашёл белорусскую кодовую страницу
      SET LANGUAGE TO BYELORUSSIAN
   ELSEIF nVal == 4
      SET CODEPAGE TO BULGARIAN
      SET LANGUAGE TO BULGARIAN
   ELSEIF nVal == 5
      SET CODEPAGE TO SERBIAN
      hb_SetCodepage( "SRWIN" )
      hb_LangSelect( "SRWIN" ) 
   ELSEIF nVal == 6
      SET CODEPAGE TO GERMAN
      SET LANGUAGE TO GERMAN
   ELSEIF nVal == 7
       hb_cdpSelect( "LVWIN" ) 
       hb_SetCodepage( "LVWIN" )
       hb_LangSelect( "lv" ) 
   ELSEIF nVal == 8
      SET CODEPAGE TO ITALIAN
      SET LANGUAGE TO ITALIAN
   ENDIF

RETURN Nil

////////////////////////////////////////////////////////////////
FUNCTION myGetLang(cTitle,cParRet)      // Какой язык в окне ?
   LOCAL cMsg, nI
   DEFAULT cParRet := "SAY"

   cMsg := "hb_SetCodepage()= " + hb_SetCodepage() + ";"
   cMsg += "hb_CdpSelect()  = " + hb_CdpSelect() + ";"
   cMsg += "hb_LangSelect() = " + hb_LangSelect() + ";"
   cMsg += "hb_langName()   = " + hb_langName() + ";"
   cMsg += "hb_langMessage()= " + hb_langMessage() + ";"
   cMsg += "hb_langErrMsg(1)= " + hb_langErrMsg(1) + ";;"

   FOR nI := 1 TO 12 
      cMsg += HB_NtoS(nI) +  ") " + LOWER( NTOCMONTH( nI ) ) + ";"
   NEXT 
   cMsg +=  ";"
   FOR nI := 1 TO 7
      cMsg += HB_NtoS(nI) +  ") " + LOWER( NTOCDOW( nI ) ) + ";"
   NEXT 

   IF cParRet == "SAY"
      cMsg += REPL("; ",20)
      AlertInfo( cMsg, cTitle, , , {RED} )
   ELSEIF cParRet == "DEBUG"
      cMsg := AtRepl( ";", cMsg, CRLF )
      MsgInfo( cMsg, cTitle )
   ELSE
      cMsg := AtRepl( ";", cMsg, CRLF )
   ENDIF

RETURN cMsg

/////////////////////////////////////////////////////////////////////////
FUNCTION ProcNL(nVal)
DEFAULT nVal := 0
RETURN "Call from: " + ProcName( nVal + 1 ) + "(" + ;
    hb_ntos( ProcLine( nVal + 1 ) ) + ") => " + ProcFile( nVal + 1 )
