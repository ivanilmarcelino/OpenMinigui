/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2024 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Фильтр на базу / Filter on the base - oBrw:FilterData
*/
#define _HMG_OUTLOG

#include "hmg.ch"
#include "tsbrowse.ch"

REQUEST HB_CODEPAGE_UTF8, HB_CODEPAGE_RU866, HB_CODEPAGE_RU1251
REQUEST DBFNTX, DBFCDX, DBFFPT

#define PROGRAM  "Testing oBrw:FilterData in Tsbrowse"
#define PROGVER  "Version 0.39 (31.10.2024)"
#define LANG_PRG "EN"  // "EN" English interface-lang

FUNCTION Main()
   LOCAL aRet, cForm := "wMain", aBColor := {81,158,211}
   LOCAL nY, nX, nG, nH, nW, nW2, nW3, nH2, aItg, o, owc
   LOCAL aUse1, cBrw1, oBrw1, oTsb1, a4Clr1
   LOCAL aUse2, cBrw2, oBrw2, oTsb2, a4Clr2

   ? ProcNL(), MiniGUIVersion(), "_SET_DELETED=", Set(_SET_DELETED) , App.Cargo:cLang

   aRet := TestUseBase(1)  // открыть базы
   IF LEN(aRet) == 0
      QUIT
   ENDIF
   aUse1  := aRet[1]
   aUse2  := aRet[2]
   // цвета таблицы: 1(шапка+подвал),2(строка %1),3(строка %2),4(фон под таблицей)
   a4Clr1 := { { 48,153,219}, {143,189,219} , {186,226,251} , {145,209,249} }
   a4Clr2 := { {192, 21,133}, {255,192,203} , {255,105,180} , {234,155,168} }

   nY := nX := 0   ; nG := 20
   nW := Sys.ClientWidth
   nH := Sys.ClientHeight

   DEFINE WINDOW &cForm AT nY, nX WIDTH nW HEIGHT nH ;
      TITLE PROGRAM + SPACE(5) + PROGVER             ;
      MAIN NOSIZE TOPMOST                            ;
      BACKCOLOR aBColor                              ;
      ON INIT    _wPost( 0)                          ;
      ON RELEASE _wSend(90)

      This.Cargo := oHmgData() ; owc := This.Cargo  // для окна создаем объект без переменных (условно пустой)
      owc:aBColor  := This.BackColor   // цвет окна
      owc:ahIcoDel := {}               // для удаления хендлов иконок с формы
      owc:nG       := nG

      nY  := nX := nG
      nW  := This.ClientWidth
      nH  := This.ClientHeight
      nW2 := INT( (nW - nG*3) * 0.6 )    // ширина Tsb-1
      nW3 := INT( (nW - nG*3) * 0.4 )    // ширина Tsb-2
      nH2 := App.Cargo:nFontSize*2       // высота GetBox

      cBrw1 := "Brw_1"
      oTsb1 := TsbParam(cBrw1,a4Clr1,aUse1,1)
      // отрабатывают после запуска функции _TBrowse(...), т.е. внутри этой функции
      oTsb1:bInit := {|ob,op| myTsbInit(ob,op)   }
      // функция в библиотеке \MiniGUI\SOURCE\TsBrowse\h_controlmisc2.prg
      oBrw1 := _TBrowse( oTsb1, oTsb1:cAls, cBrw1, nY, nX, nW2, nH-nG*2 )
      //
      oBrw1:Cargo := oHmgData()
      oBrw1:Cargo:nBrw := 1
      oBrw1:Cargo:aFldItg := {"FSize"}
      This.Cargo:oBrw1 := oBrw1
      aItg := Itogo_Dbf( oBrw1:Cargo:aFldItg, oBrw1:cAlias)   // расчёты итого
      oBrw1:Cargo:aItogo := aItg
      TsbFoot_GetBox(oBrw1)      // ввод строки поиска в подвале таблицы
      myTsbClick(oBrw1, 19)      // шапка/подвал/колонки - функции обработки _wPost(19)
      oBrw1:GetColumn("ORDKEYNO"):nClrBack := oBrw1:GetColumn("ORDKEYNO"):nClrSpcHdBack   // цвет фона 1-колонки
      oBrw1:GetColumn("ORDKEYNO"):nClrFore := oBrw1:GetColumn("ORDKEYNO"):nClrSpcHdFore   // цвет текста 1-колонки
      oBrw1:GetColumn("ORDKEYNO"):hFont    := GetFontHandle("SpecHdr")
      oBrw1:GetColumn("ORDKEYNO"):hFont    := oBrw1:aColumns[2]:hFontSpcHd

      cBrw2 := "Brw_2"
      oTsb2 := TsbParam(cBrw2,a4Clr2,aUse2,2)
      // функция в библиотеке \MiniGUI\SOURCE\TsBrowse\h_controlmisc2.prg
      oBrw2 := _TBrowse( oTsb2, oTsb2:cAls, cBrw2, nY, nW2+nG*2, nW3, nH-nG*2 )
      //
      oBrw2:Cargo := oHmgData()
      oBrw2:Cargo:nBrw := 2
      oBrw2:Cargo:aFldItg := {"FSize"}
      This.Cargo:oBrw2 := oBrw2
      aItg := Itogo_Dbf( oBrw2:Cargo:aFldItg, oBrw2:cAlias)   // расчёты итого
      oBrw2:Cargo:aItogo := aItg
      TsbFoot_GetBox(oBrw2)      // ввод строки поиска в подвале таблицы
      myTsbClick(oBrw2, 19)      // шапка/подвал/колонки - функции обработки _wPost(19)
      oBrw2:GetColumn("ORDKEYNO"):nClrBack := oBrw2:GetColumn("ORDKEYNO"):nClrSpcHdBack   // цвет фона 1-колонки
      oBrw2:GetColumn("ORDKEYNO"):nClrFore := oBrw2:GetColumn("ORDKEYNO"):nClrSpcHdFore   // цвет текста 1-колонки
      oBrw2:GetColumn("ORDKEYNO"):hFont    := GetFontHandle("SpecHdr")
      //oBrw2:GetColumn("ORDKEYNO"):hFont  := oBrw2:aColumns[2]:hFontSpcHd

      // ------ ! важно ! -----
      This.Cargo:aBrw := { oBrw1, oBrw2 }

      oBrw1:SetFocus()

      This.Cargo:oBrw := oBrw1
      This.Cargo:nBrw := oBrw1:Cargo:nBrw

      ON KEY F1     ACTION NIL
      ON KEY ESCAPE ACTION {|| _wPost(89)  }
      ON KEY TAB    ACTION { |cf| cf := ThisWindow.FocusedControl, ;
                              _LogFile(.T., ProcNL(),">>> TAB  FocusedControl=", cf ) ,;
                              iif( cf == "Brw_1", This.Brw_2.SetFocus, This.Brw_1.SetFocus ) }
      o := This.Object

      // инициализация после построения окна
      o:Event( 0, {|ow| This.Topmost := .F. , _wSend(2, ow) /*, ow:Cargo:oBrw:SetFocus()*/  } )

      o:Event( 2, {|ow| // итого по двум таблицам
                        Local ob1 := ow:Cargo:oBrw1
                        Local ob2 := ow:Cargo:oBrw2
                        myTsbItogo(ob1)
                        myTsbItogo(ob2)
                        DO EVENTS
                        Return Nil
                        })

      o:Event(11, {|ow,ky,ob| // смотреть oTsb:bGotFocus
                              IF IsObject(ob)
                                 ow:Cargo:oBrw := ob
                                 ow:Cargo:nBrw := ob:Cargo:nBrw
                              ENDIF
                              ky := ow
                              Return Nil
                              })

     o:Event(19, {|ow,ky,ob| // GetBox-1-2 в подвале таблицы
                              Tsb_Foot_FName(ob, 20)
                              ky := ow
                              Return Nil
                              })

      o:Event(20, {|ow,ky,cn| // GetBox-1-2 в подвале таблицы
                              Local cv, ob := ow:Cargo:oBrw, cf := ""
                              ?  ProcNL() , ow, ky, cn, IIF( IsArray(cn), HB_ValToExp(cn), "")
                              ob := iif( cn[2] == 1, ow:Cargo:oBrw1, ow:Cargo:oBrw2 )
                              cv := AllTrim( cn[1] )
                              IF !Empty(cv)
                                 cv := upper(cv)
                                 cf := '"'+cv+'"'+" $ upper(FNAME)"
                              ELSE
                                 cf := ""
                              ENDIF
                              ? "     cv=", cv, "cf=",cf
                              ob:FilterData(cf)                               // фильтр на базу
                              aItg := Itogo_Dbf(ob:Cargo:aFldItg, ob:cAlias)  // расчёты итого
                              ?? "aItg=", hb_ValtoExp(aItg), ob:cAlias
                              ob:Cargo:aItogo := aItg                         // сохраним в Cargo
                              //ob:FilterFTS(cv, .T.)  // !!! нельзя, падает прога !!!
                              _wPost(22, ow)     // показ итого по фильтру
                              ob:SetFocus()
                              DO EVENTS
                              IF ob:nLen == 0
                                 //Tsb_Foot_FName(ob, 20)  // показ окна ввода и потом _wPost(20)
                                 _wPost(19, , ob)           // показ окна ввода
                              ENDIF
                              ky := ow
                              Return Nil
                              })

      o:Event(22, {|ow| // показ итого по фильтру таблицы
                        Local ob := ow:Cargo:oBrw
                        myTsbItogo(ob)
                        Return Nil
                        })

      o:Event(89, {|ow,ky| // ESC
                           Local cf := ThisWindow.FocusedControl
                           Local ob := ThisWindow.Cargo:oBrw
                           ?  ProcNL(), "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                           ?? "FocusedControl", cf
                           IF This.&(cf).Type != "TBROWSE" ; ob:SetFocus()
                           ELSEIF oBrw1:IsEdit             ; oBrw1:SetFocus()
                           ELSEIF oBrw2:IsEdit             ; oBrw2:SetFocus()
                           ELSE                            ; ThisWindow.Release
                           ENDIF
                           Return Nil
                           })

      o:Event(90, {|ow,ky| // Release
                           Local cMsg
                           ow:Hide()
                           DO EVENTS
                           ?  ( cMsg := ProcNL() )
                           ?? "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                           ?  Repl(".", Len(cMsg)), "=> RELEASE WINDOW <=", ow:Name
                           Return Nil
                           })

      o:Event(99, {|ow| ow:Release()        })

   END WINDOW

     CENTER WINDOW &cForm
   ACTIVATE WINDOW &cForm

RETURN NIL

//////////////////////////////////////////////////////////////////
STATIC FUNCTION TsbParam(cBrw, a4Clr, aUse, nBrw)
   LOCAL oTsb, cAls, nClr1, nClr2

   oTsb  := oHmgData()
   oTsb:nBrw           := nBrw
   oTsb:cBrw           := cBrw
   //                         cell     Head    foot     SpecHider  SuperHider   Edit
   oTsb:aFont          := { "Normal", "Bold", "Italic", "SpecHdr" , "SuperHd", "TsbEdit" }
   oTsb:aNumber        := { 1, 40 }
   oTsb:uSelector      := 20
   oTsb:lFooting       := .T.                            // поставить в таблице подвал
   oTsb:aFoot          := .T.                            // заполнить подвал
   oTsb:nHeightCell    := 28                             // высота ячеек
   oTsb:nHeightFoot    := 30                             // высота подвала
   oTsb:nHeightHead    := 30                             // высота шапки
   oTsb:lSpecHd        := .T.                            // поставить в таблице нумератор
   oTsb:nHeightSpecHd  := 12                             // высота нумератора
   oTsb:lSuperHd       := .T.                            // поставить в таблице суперхидер
   oTsb:cSuperHd       := cFileNoPath(aUse[2]) + "  / Alias: " + aUse[1]
   oTsb:cSuperHd       += "  / RDD: " + aUse[4] + "  / CdPg: " + aUse[5]
   oTsb:cSuperHd       += "  / INDEX: " + HB_NtoS(OrdCount())
   oTsb:nHeightSuperHd := 24
   nClr1 := HMG_RGB2n(a4Clr[1])    // цвет фона шапка+подвал
   nClr2 := RGB( 48, 29,26)        // серо-черный фон
   oTsb:aSuperHdColor  := {CLR_WHITE, { nClr1, nClr2 } }   // цвет: текст и фон суперхидера

   oTsb:aEdit       := .T.                            // редактировать колонки
   oTsb:a4Clr       := a4Clr                          // сохраним 4 цвета таблицы
   oTsb:aBrush      := a4Clr[4]                       // цвет фона под таблицей
   oTsb:aColor      := Color4Tsb(a4Clr,oTsb)          // цвета таблицы: 1(шапка+подвал),2(строка %1),3(строка %2)
   //oTsb:aField    := {"FBMP","FNAME","FSIZE" ,"FDT"       ,"FCOPY","RBMP1","RBMP2" }  // не использую
   //oTsb:aEdit     := { .F.  , .F.   , .F.    , .F.        , .T.   , .T.   , .T.    }  // редактировать колонки
   //oTsb:aName     := oTsb:aField
   oTsb:bGotFocus   := {|ob| iif( IsObject(ob), _wPost(11, ob:cParentWnd, ob),) }

   cAls := aUse[1]
   oTsb:cAls := cAls
   DbSelectArea(cAls)
   // при создании TBrowse надо ставить правильный тэг индекса
   // TBrowse его удерживает, если не использовать привязку тэгов к колонкам
   OrdSetFocus("ATR_NAME")   // ставим сразу здесь !!!

RETURN oTsb

////////////////////////////////////////////////////////////////////////////
// ввод строки поиска в подвале таблицы
STATIC FUNCTION TsbFoot_GetBox(oBrw)
   LOCAL cMsg, lRus, oCol

   lRus := IIF( App.Cargo:cLang == "RU", .T., .F. )
   cMsg := IIF( lRus, "Поиск:", "Search:" )
   oCol := oBrw:GetColumn("FNAME")
   oCol:cFooting     := cMsg
   oCol:nFAlign      := DT_LEFT
   oCol:nClrFootFore := CLR_YELLOW
   //oBrw:GetColumn("FNAME"):nClrFootBack := CLR_WHITE

   // --- хранилище картинок, удаляется после закрытия объекта автоматом ---
   oBrw:aBitMaps := { LoadImage("bSearch32"), LoadImage("bSearch32x2") ,;
                      LoadImage("bSearch32x3"),  LoadImage("pSearch32") }
   // картинки PNG с прозрачностью не надо делать для
   // :nBmpMaskXXXX := 0x00CC0020    // SRCCOPY
   oBrw:Cargo:hFoot := oBrw:aBitMaps[3]
   // маска показа картинок
   //oCol:nBmpMaskHead  := 0x00CC0020   // SRCCOPY - резерв
   //oCol:nBmpMaskFoot  := 0x00CC0020   // SRCCOPY - резерв
   //oCol:nBmpMaskHead  := 0x00BB0226   // MERGEPAINT
   oCol:nBmpMaskFoot    := 0x00BB0226   // MERGEPAINT
   //oCol:nBmpMaskSpcHd := 0x00CC0020   // SRCCOPY
   //oCol:nBmpMaskCell  := 0x00CC0020   // SRCCOPY - ячейки таблицы пропустить
   //oCol:nBmpMaskCell  := 0x00BB0226   // MERGEPAINT - ячейки таблицы пропустить

   // картинка в подвале колонок таблицы
   oCol:uBmpFoot  := {|nc,ob| nc := ob:Cargo, nc:hFoot  }
   oCol:nFAlign   := nMakeLong( DT_LEFT, DT_LEFT  )

RETURN NIL

*----------------------------------------------------------------------------*
INIT PROCEDURE Sets_ENV()
*----------------------------------------------------------------------------*
   LOCAL o, cIni  := hb_FNameExtSet( App.ExeName, ".ini" )

   SET CODEPAGE TO RUSSIAN
   SET LANGUAGE TO RUSSIAN

   rddSetDefault( "DBFCDX" )

   SET DECIMALS  TO 4
   SET EPOCH     TO 2000
   SET DATE      TO GERMAN
   SET CENTURY   ON
   SET DELETED   OFF
   SET AUTOPEN   OFF
   SET EXACT     ON
   SET EXCLUSIVE ON
   SET SOFTSEEK  ON
   SET OOP ON
   Set ShowRedAlert On        // увеличить фонт для окна "Program Error"

   SET WINDOW MAIN OFF

   IF !HB_ISOBJECT( App.Cargo ) ; App.Cargo := oHmgData()
   ENDIF
   o := App.Cargo
   o:tStart       := hb_DateTime()              // start time
   o:cPathTemp    := GetUserTempFolder() + "\TMP\"
   o:cPathDbf     := GetStartUpFolder()  + "\DBF\"
   o:cLogFile     := "_msg2.log"
   o:lLogDel      := .T.
   o:cLang        := LANG_PRG
   o:cFontName    := "Tahoma"
   o:nFontSize    := 14
   o:cDlgFont     := "DejaVu Sans Mono"
   o:nDlgSize     := o:nFontSize + 2
   o:aDlgBColor   := { 141, 179, 226 }     // Alert* BackColor
   o:aDlgFColor   := {  0 ,  0 ,  0  }     // Alert* FontColor
   o:cDefAppIcon  := "1MG48"
   o:lDebug       := .T.
   o:nMenuBmpHeight := 32

   _SetGetLogFile( o:cLogFile )

   IF o:lLogDel ; hb_FileDelete( o:cLogFile )
   ENDIF

   IF o:lDebug ; SET LOGERROR ON
   ELSE        ; SET LOGERROR OFF
   ENDIF

   // Default font
   SET FONT TO o:cFontName , o:nFontSize
   // TsBrowse                                       bold italic
   _DefineFont("Normal"  , o:cFontName    , o:nFontSize  , .F., .F. )
   _DefineFont("Bold"    , o:cFontName    , o:nFontSize3 , .T., .F. )
   _DefineFont("Italic"  , o:cFontName    , o:nFontSize-2, .F., .T. )
   _DefineFont("SuperHd" , "Comic Sans MS", o:nFontSize  , .T., .F. )
   _DefineFont("SpecHdr" , o:cFontName    , o:nFontSize-4, .T., .T. )
   _DefineFont("TsbEdit" , o:cFontName3   , o:nFontSize  , .F., .T. )
   // Menu* font
   _DefineFont("ComSanMS", o:cFontName2 , o:nFontSize+2 , .F., .F. )
   // Alert* font
   _DefineFont("DlgFont" , o:cDlgFont , o:nDlgSize   , .F., .F. )
   // Alert* colors
   SET MSGALERT BACKCOLOR  TO o:aDlgBColor
   SET MSGALERT FONTCOLOR  TO o:aDlgFColor
   //
   SET DEFAULT ICON TO o:cDefAppIcon
   SET WINDOW MODAL PARENT HANDLE ON
   SET TOOLTIPSTYLE BALLOON
   SET NAVIGATION EXTENDED
   SET MENUSTYLE  EXTENDED
   SetMenuBitmapHeight( o:nMenuBmpHeight )
   ? MiniGuiVersion()

RETURN

//////////////////////////////////////////////////////////////////
STATIC FUNCTION Color4Tsb(aClr)             // цвета таблицы
   LOCAL aColors, nPane2, nPane3, nPane, nHead1, nHead2, nBCSpH
   //                     1           2           3             4
   // aClr[4] цвета:  фона окна| шапка+подвал | строка %1 | строка %2 и под таблицей

   nPane   := HMG_RGB2n(aClr[2])  // цвет фона таблицы
   nPane2  := HMG_RGB2n(aClr[3])  // строка % 2
   nPane3  := CLR_BLACK           // удалённая запись
   nHead1  := HMG_RGB2n(aClr[1])  // цвет фона шапка+подвал
   nHead2  := RGB( 48, 29,26)     // серо-черный фон
   nBCSpH  := GetSysColor( COLOR_BTNFACE )   // цвет фона спецхидера таблицы
   aColors := {}
   //AAdd( aColors, { CLR_TEXT  , {|| CLR_BLACK             } } )      // 1 , текста в ячейках таблицы
   //AAdd( aColors, { CLR_PANE  , {|| RGB(247,239,221)      } } )      // 2 , фона в ячейках таблицы
   // включаем условия показа
   AAdd( aColors, { CLR_TEXT  , {|nr,nc,ob| nr:=nc, iif( (ob:cAlias)->(DELETED()), CLR_GRAY, CLR_BLACK ) } } ) // 1
   AAdd( aColors, { CLR_PANE  , {|nr,nc,ob| // 2 , фона в ячейках таблицы
                                            Local nClr, nFClr := (ob:cAlias)->( FIELDNUM("FCLR1") )
                                            Local nFClr2 := (ob:cAlias)->( FIELDNUM("FCLR2") )
                                            Local nFClr3 := (ob:cAlias)->( FIELDNUM("FCLR3") )
                                            Local oCol
                                            //? ProcNL(), "nAt=", nr, "nCol=", nc
                                            nr := nc  // nr - здесь не использую
                                            IF nc > 2
                                               oCol := ob:GetColumn(nc)
                                               //?? oCol:cName
                                            ENDIF
                                            nClr := CLR_HRED
                                            If ob:nAt % 2 == 0
                                               nClr := nPane2
                                            Else
                                               nClr := nPane
                                            Endif
                                            If (ob:cAlias)->(DELETED())
                                               nClr := nPane3
                                            Endif
                                            If nFClr > 0
                                               If (ob:cAlias)->FCLR1 == "+"
                                                  nClr := CLR_YELLOW  // новая запись
                                               ElseIf (ob:cAlias)->FCLR1 == "*"
                                                  nClr := CLR_HMAGENTA  // разница
                                               Endif
                                            Endif
                                            If nFClr2 > 0
                                               If (ob:cAlias)->FCLR2 == "*" .AND. nc == 5  //FSIZE
                                                  nClr := CLR_WHITE  // разница
                                               Endif
                                            Endif
                                            If nFClr3 > 0
                                               If (ob:cAlias)->FCLR3 == "*" .AND. nc == 6  // FDT
                                                  nClr := CLR_WHITE  // разница
                                               Endif
                                            Endif
                                            Return nClr
                                            } } )    // 2 , фона в ячейках таблицы
   AAdd( aColors, { CLR_HEADF , {|| CLR_YELLOW            } } )        // 3 , текста шапки таблицы
   AAdd( aColors, { CLR_HEADB , {|| { nHead2, nHead1 }    } } )        // 4 , фона шапки таблицы
   //AAdd( aColors, { CLR_FOCUSF, {|| CLR_BLACK } } )                  // 5 , текста курсора в ячейках с фокусом
   AAdd( aColors, { CLR_FOCUSF, {|nr,nc,ob| nr:=nc, iif( (ob:cAlias)->(DELETED()), CLR_WHITE, CLR_BLACK ) } } )  // 5 , текста курсора в ячейках с фокусом
   AAdd( aColors, { CLR_FOCUSB, {|a,b,c| a := b, If( c:nCell == b, -CLR_HRED, -CLR_BLUE ) } } ) // 6 , фона курсора
   //AAdd( aColors, { CLR_FOCUSB, {|nr,nc,ob| myFocusB(nr,nc,ob,-CLR_HRED,-CLR_BLUE,-CLR_YELLOW) } } ) // 6 , фона курсора

   AAdd( aColors, { CLR_EDITF , {|| CLR_BLUE                } } )      // 7 , текста редактируемого поля
   AAdd( aColors, { CLR_EDITB , {|| CLR_HRED                } } )      // 8 , фона редактируемого поля
   AAdd( aColors, { CLR_FOOTF , {|| CLR_WHITE               } } )      // 9 , текста подвала таблицы
   AAdd( aColors, { CLR_FOOTB , {|| { nHead1, nHead2 }      } } )      // 10, фона подвала таблицы
   AAdd( aColors, { CLR_SELEF , {|| CLR_YELLOW   }            } )      // 11, текста неактивного курсора (selected cell no focused)
   AAdd( aColors, { CLR_SELEB , {|| { CLR_BLUE, CLR_GRAY  } } } )      // 12, фона неактивного курсора (selected cell no focused)
   AAdd( aColors, { CLR_ORDF  , {|| CLR_WHITE  }              } )      // 13, текста шапки выбранного индекса
   AAdd( aColors, { CLR_ORDB  , {|| CLR_RED    }              } )      // 14, фона шапки выбранного индекса
   AAdd( aColors, { CLR_LINE  , {|| CLR_WHITE  }              } )      // 15, линий между ячейками таблицы
   AAdd( aColors, { CLR_SUPF  , {|| { nHead1, nHead2 }     }  } )      // 16, фона суперхидер
   AAdd( aColors, { CLR_SUPB  , {|| CLR_HRED   }              } )      // 17, текста суперхидер
   AAdd( aColors, { CLR_SPCF  , {|| CLR_RED    }              } )      // 18, specheader text
   AAdd( aColors, { CLR_SPCB  , {|| nBCSpH     }              } )      // 19, specheader back
   AAdd( aColors, { CLR_SPCA  , {|| CLR_GREEN  }              } )      // 20, active specheader back

RETURN aColors

/////////////////////////////////////////////////////////////////////
STATIC FUNCTION Itogo_Dbf(aFld, cAls, aWait)  // расчёты итого
   LOCAL nLen := 0, nRec, aItg, aPos, nPos
   LOCAL nOld := Select(), nCnt := 0, nSum
   Default cAls := Alias(), aWait := .F.

   IF !Empty(aWait)
      IF HB_ISLOGICAL(aWait)
         aWait := "Wait processing ..."
      ENDIF
      WaitWindow( aWait, .T. , 600, 16, NIL, BLUE, App.Cargo:aBClrMain )
   ENDIF

   dbSelectArea( cAls )

   nRec := RecNo()
   aItg := Array(Len(aFld)) ; aFill(aItg, 0)
   aPos := {} ; AEval(aFld, {|cn| AAdd(aPos, FieldPos(cn)) })

   DO EVENTS
   GO TOP
   DO WHILE ! EOF()
      nCnt++
      DO EVENTS
      FOR EACH nPos IN aPos
          IF nPos > 0 .and. HB_ISNUMERIC( nSum := FieldGet( nPos ) )
             aItg[ hb_EnumIndex(nPos) ] += nSum
          ENDIF
      NEXT
      SKIP
   ENDDO

   DbGoTo( nRec )       ; DO EVENTS

   IF !Empty(aWait)     ; WaitWindow()
   ENDIF

   dbSelectArea( nOld ) ; DO EVENTS

RETURN aItg //{ nCnt, aItg }

//////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbItogo( oBrw )  // подвал - ТОЛЬКО показ
   LOCAL aItg  := oBrw:Cargo:aItogo
   LOCAL aFoot := oBrw:Cargo:aFldItg  // {"FSize"}
   LOCAL nCol, cVal

   ? ProcNL(), "##", oBrw, oBrw:Classname, oBrw:cAlias
   ? "     aItg=",aItg, HB_ValToExp(aItg)

   nCol := oBrw:nColumn(aFoot[1], .T.)
   cVal := ALLTRIM( Transform( aItg[1], "999 999 999 999" ) )
   oBrw:aColumns[1]:cFooting    := {|nc,ob| nc := ob:nLen, iif( Empty(nc), "", hb_NtoS(nc) ) }
   //oBrw:aColumns[3]:cFooting  := {|nc   | nc := aItg[1], iif( Empty(nc), "", hb_NtoS(nc) ) }
   oBrw:aColumns[nCol]:cFooting := cVal

   oBrw:DrawFooters() ; DO EVENTS

RETURN Nil

/////////////////////////////////////////////////////////////////////////////////////
//  назначаем на шапку и подвал отдельную функцию и колонки таблицы
STATIC FUNCTION myTsbClick( oBrw , nPost)
   LOCAL oCol

   //  назначаем на суперхидер отдельную функцию ЭТО ДЕЛАЕМ ПОСЛЕ END TBROWSE
   FOR EACH oCol IN oBrw:aColumns
      // левая и правая кнопка мышки для шапки таблицы
      //oCol:bHLClicked := {|Ypix,Xpix,nAt,ob| iif( Ypix > ob:nHeightSuper, ;
      //                     Tsb_Header(1,"Header!",Ypix,Xpix,nAt,ob) ,;
      //                     Tsb_SuperHd(1,"Super!",Ypix,Xpix,nAt,ob) ) }
      //oCol:bHRClicked := {|Ypix,Xpix,nAt,ob| iif( Ypix > ob:nHeightSuper, ;
      //                     Tsb_Header(2,"Header!",Ypix,Xpix,nAt,ob) ,;
      //                     Tsb_SuperHd(2,"Super!",Ypix,Xpix,nAt,ob) ) }
      // левая и правая кнопка мышки для подвала и колонки таблицы
      oCol:bFLClicked := {|nrp,ncp,nat,obr| Tsb_Foot(1,nPost,obr,nrp,ncp,nat) }
      oCol:bFRClicked := {|nrp,ncp,nat,obr| Tsb_Foot(2,nPost,obr,nrp,ncp,nat) }
      //oCol:bLClicked:= {|nrp,ncp,nat,obr| Tsb_Cell(1,obr,nrp,ncp,nat) }
      //oCol:bRClicked  := {|nrp,ncp,nat,obr| Tsb_Cell(2,obr,nrp,ncp,nat) }
      // настройка для SpecHd таблицы
      //oCol:bSLClicked := {|nrp,ncp,nat,obr| Tsb_SpcHd(1,nrp,ncp,nat,obr) }
      //oCol:bSRClicked := {|nrp,ncp,nat,obr| Tsb_SpcHd(2,nrp,ncp,nat,obr) }
   NEXT

RETURN Nil

/////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Tsb_Foot( nClick, nPost, oBrw, nRowPix, nColPix, nAt )
   LOCAL nRow  := oBrw:GetTxtRow(nRowPix)       // номер строки курсора в таблице
   LOCAL nCol  := Max(oBrw:nAtCol(nColPix), 1)  // номер колонки курсора в таблице
   LOCAL nCell := oBrw:nCell                    // номер ячейки в таблице
   LOCAL cNam  := {'Left mouse', 'Right mouse'}[ nClick ]
   LOCAL cNCol := oBrw:aColumns[nCol]:cName
   LOCAL cMs, cRW, cCV, xVal, cMsg, cCol

   cMs   := "Mouse y:x = " + hb_ntos(nRowPix) + ":" + hb_ntos(nColPix)
   cRW   := "Cell position row/column: " + hb_ntos(nAt) + '/' + hb_ntos(nCell)
   xVal  := oBrw:GetValue(nCell)
   cCV   := "Get cell value: [" + cValToChar(xVal) + "]"
   cCol  := "Columns: " + cNCol
   cMsg  := cNam + ";" + cMs + ";" + cRW + ";" + cCV + ";" + cCol + ";;"
   //AlertInfo(cMsg,"Footer Table")

   IF cNCol == "FNAME"
      //Tsb_Foot_FName(oBrw,nPost)
      _wPost(nPost, oBrw:cParentWnd, oBrw)  // nPost = 19
   ENDIF

RETURN Nil

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Tsb_Foot_FName(oBrw, nPost)
   LOCAL cCV, cForm, cGet, aRet, cVal, cFoot, cMsg

   cForm := oBrw:cParentWnd
   cFoot := oBrw:GetColumn("FNAME"):cFooting
   cFoot := SUBSTR( cFoot, 1 , AT(":",cFoot) + 1 )
   cCV   := oBrw:GetColumn("FNAME"):cFooting
   cCV   := ALLTRIM( SUBSTR( cCV, AT(":",cFoot) + 1 ) )

   SET WINDOW THIS TO cForm          // ОБЯЗАТЕЛЬНО !!!
   aRet := Tsb_Foot_Get(oBrw,cCV)
   SET WINDOW THIS TO

   oBrw:Cargo:cFilter := cVal        // можно и так

   cVal := IIF(LEN(aRet)==0, "", ALLTRIM(aRet[1]) )
   cMsg := " " + cVal

   oBrw:GetColumn("FNAME"):cFooting  := cFoot + cMsg

   DO EVENTS

   ? ProcNL(), nPost, cGet, cForm, "cVal=["+cVal+"]"
   ? "  _wPost(", nPost, oBrw:cParentWnd, "{",cVal,oBrw:Cargo:nBrw,"} )"
   _wPost(nPost, oBrw:cParentWnd, {cVal,oBrw:Cargo:nBrw} )  //  событие

RETURN Nil

////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Tsb_Foot_Get(oBrw,cGet)
   LOCAL oWnd, hWnd, oRWnd, aRet, nHBtn, cFont, nFSize, aFont, hFont, nG, oCol
   LOCAL nY, nX, nW, nW2, nH, nHIco, aGBox, aBClr, lHScr, cMsg, oCel, nCol

   aRet   := {}   // всегда массив - пусто, значит отказ от ввода
   aFont  := GetFontParam(oBrw:hFont)
   cFont  := aFont[1]
   nFSize := aFont[2]
   oRWnd  := _WindowObj(oBrw:cParentWnd)
   nG     := oRWnd:Cargo:nG
   // горизонтальный скролинг
   lHScr := .T.
   IF oBrw:oHScroll:nMin == 0 .and. oBrw:oHScroll:nMax == 0
      lHScr := .F.
   ENDIF
   // координаты ячейки в которой GetBox
   nCol  := oBrw:nColumn("FNAME", .T.)
   oCel  := oBrw:GetCellInfo( nCol )
   oCol  := oBrw:GetColumn("FNAME")
   hFont := oCol:hFontFoot            // 3-footer font
   cMsg  := oCol:cFooting
   //
   nH    := oBrw:nHeightCell
   nX    := oRWnd:Col + oCel:nCol + 2
   nX    += GetTextWidth( NIL, cMsg, hFont )
   nX    += nG + 32  // 32 - это картинка
   nW    := 250
   ? ProcNL(), oBrw:cAlias, oBrw:oHScroll:nMin, oBrw:oHScroll:nMax, "lHScr=",lHScr
   //
   nY    := oRWnd:Row + oBrw:nBottom - oBrw:nHeightFoot + 6
   nY    += IIF( !lHScr, GetHScrollBarHeight(), 0 )
   aGBox := { nY, nX, nW, oBrw:nHeightFoot }  // для построения окна внутри подвала таблицы
   ?? "aGBox=",HB_ValToExp(aGBox)
   // координаты подвала в которой GetBox
   nY     := aGBox[1]
   nX     := aGBox[2]
   nW2    := aGBox[3]
   nH     := aGBox[4]
   nHBtn  := nH - 2*2
   nW     := aGBox[3] + nHBtn*2 + 8
   nHIco  := nHBtn - 2*2
   aBClr  := IIF( oBrw:Cargo:nBrw == 1, RED, WHITE )

   // новое окно в подвал таблицы
   DEFINE WINDOW Cell AT nY,nX WIDTH nW HEIGHT nH+2 ;
      MODAL NOCAPTION                               ;
      FONT cFont SIZE nFSize                        ;
      BACKCOLOR aBClr                               ;
      ON LOSTFOCUS {|| oWnd:Release() }             ;
      ON INIT      {|| DoEvents(), This.Gb_Val.Setfocus }

      oWnd := ThisWindow.Object
      hWnd := oWnd:Handle
      nW   := This.ClientWidth + 10
      nH   := This.ClientHeight + 10
      nX   := nY := 2

      @ nY, nX GETBOX Gb_Val WIDTH nW2 HEIGHT nHBtn VALUE cGet ;
        NOTABSTOP BACKCOLOR YELLOW FONTCOLOR BLUE PICTURE REPL("X",25)

      nX += This.Gb_Val.Width + 1

      @ nY, nX BUTTONEX Btn_Ok WIDTH nHBtn HEIGHT nHBtn CAPTION '' ;
        ICON "iOk32" NOHOTLIGHT NOXPSTYLE HANDCURSOR NOTABSTOP ;
        ACTION {|| aRet := { This.Gb_Val.Value } , oWnd:Release() }

      nX += This.Btn_Ok.Width + 1

      This.Btn_Ok.ImageWidth  := nHIco
      This.Btn_Ok.ImageHeight := nHIco
      This.Btn_Ok.Icon        := LoadIconByName( "iOk32", nHIco, nHIco )

      @ nY, nX BUTTONEX Btn_Esc WIDTH nHBtn HEIGHT nHBtn CAPTION '' ;
        ICON "iCancel32" NOHOTLIGHT NOXPSTYLE HANDCURSOR NOTABSTOP ;
        ACTION {||  aRet := {} , oWnd:Release() }

      This.Btn_Esc.ImageWidth  := nHIco
      This.Btn_Esc.ImageHeight := nHIco
      This.Btn_Esc.Icon        := LoadIconByName( "iCancel32", nHIco, nHIco )

   END WINDOW

   SetWindowLong(hWnd, GWL_STYLE, WS_BORDER)

   _DefineHotKey ( "CELL" , 0 , VK_ESCAPE , {|| oWnd:Release() } )
   _DefineHotKey ( "CELL" , 0 , VK_RETURN , {|| aRet := { This.Gb_Val.Value } , oWnd:Release() } )
   Cell.Activate

RETURN aRet // всегда массив, если пусто - значит отказ от ввода

////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbInit( oBrw, oTsb )  // настройки
   Local nI, cCol, oCol, nWCol, cFoot, hFont, nFSize, aFont, cFont, cHead

   ? ProcNL() , oBrw, oBrw:ClassName, oTsb, oTsb:ClassName
   aFont  := GetFontParam(oBrw:hFont)
   cFont  := aFont[1]
   nFSize := aFont[2]

   WITH OBJECT oBrw
      :nCellMarginLR :=  1           // отступ от линии ячейки при прижатии влево, вправо на кол-во пробелов
      :nMemoHV       :=  1           // показ одной строки мемо-поля
   END WITH

   FOR EACH oCol IN oBrw:aColumns
      nI   := hb_EnumIndex(oCol)
      cCol := oCol:cName
      IF cCol == "SELECTOR" .OR. cCol == "ORDKEYNO"
      ELSEIF cCol == "FSIZE"
         oCol:nWidth  := oCol:ToWidth(11)
         oCol:nFAlign := DT_CENTER
         // выровним ширину колонки
         hFont := oCol:hFontFoot                  // какой фонт в колонке подвала
         cFoot := REPL("0",oCol:nFieldLen) + "."
         cFoot += REPL("0",oCol:nFieldDec) + "99"
         nWCol := GetTextWidth( Nil, cFoot, hFont )
         IF oCol:nWidth < nWCol
            oCol:nWidth := nWCol
         ENDIF
      ELSE
         //oCol:cFooting := oCol:cHeading
         //oCol:cHeading := "Field:" + CRLF + oCol:cFooting
         oCol:nFAlign  := DT_CENTER
      ENDIF
      // делаем добавку -  :nCellMarginLR :=  1
      IF oCol:cFieldTyp == "C"
         oCol:cPicture := Nil
         oCol:nWidth := oCol:ToWidth( iif( oCol:nFieldLen > 50, 50, oCol:nFieldLen ) )
         oCol:nWidth := oCol:ToWidth( iif( oCol:nFieldLen < 10, oCol:nFieldLen + 1, oCol:nFieldLen ) )
      //ELSEIF oCol:cFieldTyp $ "N"
      //   n := 2 + 1
      //   oCol:nWidth   += oCol:ToWidth(n)
      ELSEIF oCol:cFieldTyp $ "D"
         oCol:cPicture := Nil
         IF LEN( SET( _SET_DATEFORMAT ) ) > 8
            oCol:nWidth := oCol:ToWidth(10+2)   // "01.01.2024"
         ELSE
            oCol:nWidth := oCol:ToWidth(10)   // "01.01.24"
         ENDIF
         oCol:nAlign   := DT_CENTER
      ELSEIF oCol:cFieldTyp $ "T=@"
         //oCol:cPicture := "@R 9999-99-99 99:99:99" // 23 символа
         //oCol:bDecode  := {|tval| iif( tval == hb_CToT(""), "", hb_TtoS(tval) ) }
         //oCol:bDecode:= {|tval| hb_TtoS(tval) }
         //oCol:nAlign   := DT_LEFT
         // лучше так
         oCol:cPicture := NIL
         IF nFSize > 14  ; oCol:nAlign   := DT_LEFT
         ELSE            ; oCol:nAlign   := DT_CENTER
         ENDIF
         oCol:nWidth   := oCol:ToWidth(24)
         //^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ - это не работает, если задан oCol:cPicture, он в приоритете
      ELSEIF oCol:cFieldTyp $ "^"
         oCol:bDecode  := {|tval| hb_NtoS(tval) }
         oCol:nAlign   := DT_CENTER
      ELSEIF oCol:cFieldTyp $ "L"
         oCol:nAlign   := DT_CENTER
      ELSEIF oCol:cFieldTyp == "M"
         oCol:cPicture := Nil
         oCol:nWidth   := oCol:ToWidth(40)
      ENDIF
      // выровним ширину колонки
      hFont := oCol:hFontHead                  // какой фонт в колонке шапки
      cHead := oCol:cHeading + "H"
      nWCol := GetTextWidth( Nil, cHead, hFont )
      IF oCol:nWidth < nWCol
         oCol:nWidth := nWCol
      ENDIF

   NEXT

RETURN NIL

///////////////////////////////////////////////////////////////////////////////
FUNCTION TestUseBase(nMode)
   LOCAL cPath, cDbf, lShared, cAls, cRdd, cCdPg, cFor, aUse1, aUse2

   lShared := .T.
   cPath := App.Cargo:cPathDbf
   cDbf  := cPath + "ListLeto.dbf"
   cAls  := "ListLeto"
   cRdd  := "DBFCDX"
   cCdPg := "RU1251"
   cFor  := ""
   IF nMode == 2
      cDbf := cPath + "Customer.dbf"
   ENDIF

   IF !File(cDbf)
      AlertStop("Not file !;" + cDbf)
      RETURN {}
   ENDIF

   USE ( cDbf )  ALIAS (cAls) CODEPAGE (cCdPg) NEW SHARED
   If OrdCount() < 1 .AND. nMode == 1
      INDEX ON &("LEVEL + UPPER(FNAME)") TAG ATR_NAME FOR !Deleted()
      INDEX ON &("FSIZE")                TAG SIZE
      INDEX ON &("FDT")                  TAG DATETIME
      cFor := "FCOMP == '*'"
      INDEX ON &("FCOMP")  TAG COMPARE FOR &cFor
      INDEX ON &("UPPER(FNAME)")         TAG FNAME  // сравнение
   EndIf
   aUse1 := { cAls, cDbf, lShared, cRdd, cCdPg }

   // -------- вторая база ----------
   cDbf  := cPath + "ListTemp.dbf"
   cAls  := "ListTemp"
   IF nMode == 2
      cDbf := cPath + "Customer2.dbf"
   ENDIF

   IF !File(cDbf)
      AlertStop("Not file !;" + cDbf)
      RETURN {}
   ENDIF

   USE ( cDbf )  ALIAS (cAls) CODEPAGE (cCdPg) NEW SHARED
   If OrdCount() < 1 .AND. nMode == 1
      INDEX ON &("UPPER(FNAME)") TAG ATR_NAME FOR !Deleted()
      INDEX ON &("FSIZE")                TAG SIZE
      INDEX ON &("FDT")                  TAG DATETIME
      cFor := "FCOMP == '*'"
      INDEX ON &("FCOMP")  TAG COMPARE FOR &cFor
      INDEX ON &("UPPER(FNAME)")         TAG FNAME  // сравнение
   EndIf
   aUse2 := { cAls, cDbf, lShared, cRdd, cCdPg }

RETURN { aUse1, aUse2 }

////////////////////////////////////////////////////////////////
FUNCTION ProcNL(nVal, cMsg)
   Default cMsg := ">>> "
   nVal := iif( Empty(nVal), 0, nVal ) + 1
   cMsg += ProcName(nVal) + "(" + hb_ntos( ProcLine(nVal) ) + ")"
   cMsg += " => " + ProcFile(nVal)
RETURN cMsg
