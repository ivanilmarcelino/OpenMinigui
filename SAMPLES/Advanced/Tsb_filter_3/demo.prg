/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2024 Sergej Kiselev <bilance@bilance.lv>
 *
 * ‘ильтр на две базы / Filter for two bases - oBrw:FilterFTS
*/
#define _HMG_OUTLOG

#include "hmg.ch"
#include "tsbrowse.ch"

REQUEST DBFCDX
#define PROGRAM  "Testing oBrw:FilterFTS in Tsbrowse"
#define PROGVER  "Version 0.3 (31.10.2024)"

Function Main()
   LOCAL cFont := "Arial"
   LOCAL nSize := 12
   LOCAL cForm := "wMain"
   LOCAL oBrw1, oBrw2, nY, nX, nH, nW, nG := 0
   LOCAL oTsb1, oTsb2, oGet, o, a
   LOCAL cLogFile := "_msg.log"

   rddSetDefault( "DBFCDX" )

   SET EPOCH   TO 2000
   SET DATE    TO GERMAN
   SET CENTURY ON
   SET AUTOPEN OFF
   SET DELETED OFF
   SET OOP ON

   SET FONT TO cFont, nSize
   SET DEFAULT ICON TO "1MG"
   // Default font
   _DefineFont("DefFont" , cFont, nSize  , .F., .F. )
   // TsBrowse                            bold italic
   _DefineFont("Normal"  , cFont, nSize  , .F., .F. )
   _DefineFont("Bold"    , cFont, nSize  , .T., .F. )
   _DefineFont("Italic"  , cFont, nSize-2, .F., .T. )
   // Alert* font
   _DefineFont("DlgFont" , cFont, nSize  , .F., .F. )

   HMG_Alert_MaxLines(, 110)

   _SetGetLogFile( cLogFile )
   hb_FileDelete( cLogFile )
   ? MiniGuiVersion()

   USE ( "DBF\CUSTOMER"  )  ALIAS CUST1  NEW SHARED
   USE ( "DBF\CUSTOMER2" )  ALIAS CUST2  NEW SHARED

   DEFINE WINDOW &cForm                  ;
      TITLE PROGRAM + SPACE(5) + PROGVER ;
      MAIN NOSIZE TOPMOST                ;
      ON INIT    ( This.Topmost := .F. ) ;
      ON RELEASE ( dbCloseAll() )

      This.Cargo := oHmgData()
      This.Maximize

      nY := 0 ; nX := 0
      nW := GetFontWidth ("DefFont", 20)
      nH := GetFontHeight("DefFont") + 4

      @ nY, nX GETBOX FTS OBJ oGet HEIGHT nH WIDTH nW VALUE "" FONT "DefFont" ;
               NOTABSTOP  BACKCOLOR RED PICTURE "XXXXXXXXXXXXXXXXXXXX"  ;
               ACTION  _wPost(10, , This.Name) ;
               ACTION2 ( This.Value := "", _wPost(10, , This.Name) ) ;
               IMAGE {"br_view", "br_no"} ;
               ON INIT {|og| This.Cargo := og }

      This.Cargo:oGet := oGet
      This.Cargo:cGet := "FTS"

      nY := nH + nG ; nX := 0
      nW := This.ClientWidth
      nH := Int( This.ClientHeight / 2 )

      oTsb1 := oHmgData()
      oTsb1:aFont     := {"Normal", "Bold", "Bold", "Italic"}
      oTsb1:aFooter   := .T.
      oTsb1:aEdit     := .T.
      oTsb1:aNumber   := { 1, 40, DT_RIGHT, "br_no" }
      oTsb1:uSelector := 20
      oTsb1:lSpecHd   := .T.
      oTsb1:lZebra    := .T.
      oTsb1:bGotFocus := {|ob| iif( IsObject(ob), _wPost(11, ob:cParentWnd, ob),) }
      a := {}
      // 1 , текста €чеек
      AAdd(a, { CLR_TEXT, {|nr,nc,ob| nr := CLR_GRAY, nc := CLR_BLACK, ;
                            iif( (ob:cAlias)->(DELETED()), nr, nc ) } })
      // 2 , фона в €чейках таблицы
      AAdd(a, { CLR_PANE, {|nr,nc,ob,nd| nr := CLR_BLACK, nc := CLR_WHITE, nd := CLR_HGRAY, ;
                            iif( (ob:cAlias)->(DELETED()), nr, iif( ob:nAt % 2 == 0, nc, nd ) ) } } )
      AAdd(a, { CLR_HEADF, CLR_WHITE                })  // 3 , текста шапки таблицы
      AAdd(a, { CLR_HEADB, {CLR_BLUE,CLR_HMAGENTA}  })  // 4 , фона шапки таблицы
      AAdd(a, { CLR_EDITF, CLR_YELLOW               })  // 7 , текста редактируемого пол€
      AAdd(a, { CLR_EDITB, CLR_HRED                 })  // 8 , фона редактируемого пол€
      AAdd(a, { CLR_FOOTF, CLR_ORANGE               })  // 9 , текста подвала таблицы
      AAdd(a, { CLR_FOOTB, {CLR_MAGENTA,CLR_BLUE}   })  // 10, фона подвала таблицы
      AAdd(a, { CLR_SPCF , CLR_YELLOW               })  // 18, specheader text - нумератор
      AAdd(a, { CLR_SPCB , CLR_GRAY                 })  // 19, specheader back - нумератор
      oTsb1:aColorAdd := a

      oTsb1:nHeightCell    := 28
      oTsb1:nHeightFoot    := 30
      oTsb1:nHeightHead    := 30
      oTsb1:nHeightSpecHd  := 16
      oTsb1:nHeightSuperHd := 24
      oTsb1:cSuperHd       := "Test Tsbrowse - 1"
      oTsb1:lSuperHd       := .T.
      oTsb1:aSuperHdColor  := {CLR_YELLOW, {CLR_HMAGENTA,CLR_BLUE} }   // цвет: текст и фон суперхидера

      oBrw1 := _TBrowse( oTsb1, "CUST1", "Brw_1", nY, nX, nW, nH )

      oBrw1:Cargo:nBrw := 1
      _o2log(oBrw1:Cargo,, "oBrw1 = ", .T.)
      oBrw1:GetColumn("ORDKEYNO"):nClrBack := CLR_YELLOW                    // цвет фона 1-колонки
      oBrw1:GetColumn("ORDKEYNO"):hFont    := GetFontHandle("Italic")
      oBrw1:nClrLine := CLR_ORANGE
      IF IsArray(oBrw1:aCheck)
         AEval(oBrw1:aCheck, {|h| iif( Empty(h), , DeleteObject(h) ) })
      ENDIF
      oBrw1:aCheck   := { LoadImage("bCheckT24"), LoadImage("bCheckF24") }  // замен€ем колонку CHECKBOX на свои картинки
      oBrw1:Refresh()

      nY += nH + 1
      nH -= 1

      oTsb2 := oHmgData()
      oTsb2:aFont     := {"Normal", "Bold", "Bold", "Italic"}
      oTsb2:aFooter   := .T.
      oTsb2:aEdit     := .T.
      oTsb2:aNumber   := { 1, 40, DT_RIGHT, "br_no" }
      oTsb2:uSelector := 20
      oTsb2:bGotFocus := {|ob| iif( IsObject(ob), _wPost(11, ob:cParentWnd, ob),) }
      oTsb2:lZebra    := .T.
      oTsb2:aZebra    := { ORANGE, SILVER }
      oTsb2:lSuperHd  := .T.
      oTsb2:cSuperHd  := "Test Tsbrowse - 2"
      oTsb2:aSuperHdColor := {CLR_YELLOW, {CLR_ORANGE,CLR_RED} }   // цвет: текст и фон суперхидера

      oBrw2 := _TBrowse( oTsb2, "CUST2", "Brw_2", nY, nX, nW, nH )

      oBrw2:Cargo:nBrw := 2
      _o2log(oBrw2:Cargo,, "oBrw2 = ", .T.)

      This.Cargo:aBrw := { oBrw1, oBrw2 }

      oBrw1:SetFocus()

      This.Cargo:oBrw := oBrw1
      This.Cargo:nBrw := oBrw1:Cargo:nBrw

      ON KEY F1     ACTION NIL
      ON KEY F6     ACTION {||
                            Local ow := ThisWindow.Object
                            Local cf := ThisWindow.FocusedControl
                            IF This.&(cf).Type == "TBROWSE"
                               cf := ow:Cargo:cGet
                               This.&(cf).Value := ""
                               This.&(cf).SetFocus
                               DO EVENTS
                            ENDIF
                            Return Nil
                            }
      ON KEY TAB    ACTION {|cf| cf := ThisWindow.FocusedControl, ;
                            iif( cf == "Brw_1", This.Brw_2.SetFocus, This.Brw_1.SetFocus ) }
      ON KEY ESCAPE ACTION {||
                            Local cf := ThisWindow.FocusedControl
                            Local ob := ThisWindow.Cargo:oBrw
                            IF This.&(cf).Type != "TBROWSE" ; ob:SetFocus()
                            ELSEIF oBrw1:IsEdit             ; oBrw1:SetFocus()
                            ELSEIF oBrw2:IsEdit             ; oBrw2:SetFocus()
                            ELSE                            ; ThisWindow.Release
                            ENDIF
                            Return Nil
                            }
      o := This.Object

      o:Event(10, {|ow,ky,cn|
                    Local cv, ob := ow:Cargo:oBrw
                    cv := AllTrim( This.&(cn).Value )
                    IF !Empty(cv)
                       cv := upper(cv)
                    ENDIF
                    ob:FilterFTS(cv, .T.)
                    ob:SetFocus()
                    DO EVENTS
                    ky := ow
                    Return Nil
                    })

      o:Event(11, {|ow,ky,ob|
                    IF IsObject(ob)
                       ow:Cargo:oBrw := ob
                       ow:Cargo:nBrw := ob:Cargo:nBrw
                    ENDIF
                    ky := ow
                    Return Nil
                    })

   END WINDOW

   ACTIVATE WINDOW &cForm

RETURN NIL
