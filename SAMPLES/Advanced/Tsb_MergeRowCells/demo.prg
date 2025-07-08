/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2025 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * _TBrowse() Объединение ячеек строки в одну строку и показ в таблице (для массивов)
 * _TBrowse() Merge cells of a row into one row and displays in a table (for arrays)
*/
#define  _HMG_OUTLOG
#include "hmg.ch"
#include "tsbrowse.ch"

#define PROGRAM  "MiniGui - _TBrowse() Merge cells of a row into one row and displays in a table (for arrays)"
#define PROGVER  "Version 0.3 (23.03.2025)"
#define PROGINF  "Test"

FUNCTION Main()
   LOCAL nY, nX, nG := 20, aBClr := {231,178, 30} 
   LOCAL owc, oTbl, oTsb, aXdim, nWTsb, nHTsb, oCol
   LOCAL cVal  := MiniGuiVersion() + CRLF + Version() + CRLF + hb_Ccompiler()
   LOCAL nH := 800 //Sys.ClientHeight
   LOCAL nW := Sys.ClientWidth
   
   aXdim := LoadDim()

   SET FONT TO _GetSysFont(), App.Cargo:nFontSize

   IF nH > Sys.ClientHeight ; nH := Sys.ClientHeight
   ENDIF

   DEFINE WINDOW wMain CLIENTAREA nW, nH    TITLE PROGRAM   ;
          MAIN NOMAXIMIZE NOSIZE /*NOSHOW*/ BACKCOLOR aBClr ;
          ON INIT    _wPost( 0)                             ;
          ON RELEASE _wSend(90)
          This.Cargo := oHmgData() ; owc := This.Cargo

      nW := This.ClientWidth
      nH := This.ClientHeight

      owc:cForm  := This.Name
      owc:aBClr  := aBClr

      nY := 5
      DRAW ICON IN WINDOW wMain AT nY, nW-96-10 PICTURE "1MG" WIDTH 96 HEIGHT 96 COLOR aBClr

      @ nY, 5 LABEL Buff VALUE cVal WIDTH nW-96-nY*2 HEIGHT nH - nY*2 ;
        FONTCOLOR BLUE TRANSPARENT RIGHTALIGN

      nY := 5 + 96 + 5  ; nX := nG ; nWTsb := nW - nG * 2 ; nHTsb := nH - nY - nG
      IF nH == Sys.ClientHeight
         nHTsb := nH - nY - nG * 2  // GetMenuBarHeight() + GetTitleHeight()
      ENDIF
      /////////////////////// таблица ///////////////////////////////////////////////////////
      oTsb := TablePatam( owc:cForm, aXdim, "cTable", aBClr, nWTsb)
      ? _o2log(oTsb, 27, ProcNL() + "  oTsb => ", .T. ) // проверка
      // функция в библиотеке \MiniGUI\SOURCE\TsBrowse\h_controlmisc2.prg
      oTbl := _TBrowse( oTsb, aXdim, "cTable", nY, nX, nWTsb, nHTsb )
      //oTbl:HideColumns( { 2 } ,.t.)   // скрыть колонку "MT" - плохое решение
      //
      // блок кода для объединения колонок в одну колонку (2)
      oTbl:bTSDrawCell := {|ob,ocel,ocol|             // 
                           Local ldrw := .T., oo
                           IF !Empty(ocel:nDrawType) .or. ocol:cName == "oPhant"
                              Return ldrw // header-ы или посл.колонка -> выход
                           ENDIF
                           oo := ob:Cargo:oDraw_1        // объект параметров
                           // считаем общую длину объединенной колонки
                           IF oo:lSizes .and. ocol:cName $ oo:cNames
                              oo:nSizes += ocel:nSize
                              IF ocol:cName == oo:cLast 
                                 oo:lSizes := .F.
                              ENDIF
                           ENDIF
                           IF ocol:cName == oo:cDolg .and. val(ocel:uData) < 0
                              ocel:nClrFore := oo:nDolgFore
                           ENDIF
                           // применяем :oDraw_1
                           IF ob:aArray[ ob:nAt ][oo:nMet] == 1
                              IF ocol:cName == oo:cFirst
                                 ocel:nAlign := oo:nAlign
                                 ocel:nSize  := oo:nSizes
                                 IF ocel:nAlign != DT_CENTER .and. IsNumeric(ob:nCellMarginLR)
                                    ocel:uData := Space( ob:nCellMarginLR ) + ;
                                                  ATail(ob:aArray[ ob:nAt ])
                                 ELSE
                                    ocel:uData := ATail(ob:aArray[ ob:nAt ])
                                 ENDIF
                              ENDIF
                              IF IsNumeric(oo:nClrFore)
                                 ocel:nClrFore := oo:nClrFore
                              ENDIF
                              IF IsNumeric(oo:nClrBack)
                                 ocel:nClrBack := oo:nClrBack
                                 ocel:nClrTo   := oo:nClrBack
                              ENDIF
                              IF ob:nColumn(ocol:cName) > oo:nFirst
                                 ldrw := .F.
                              ENDIF
                           ELSEIF oo:cItog $ ob:aArray[ ob:nAt ][1] 
                              ocel:hFont := oo:hItog
                           ENDIF
                           //? ob:nAt, ldrw, ocel:nRow, ocel:nCell, ocel:nDrawType, ocol:cName, oo:nSizes, ocel:uData
                           Return ldrw
                           }
      // объект положим на окно
      This.Cargo:oTbl    := oTbl                         // oWnd:Cargo:oTbl
      This.Cargo:cTbl    := oTbl:cControlName            //"cTable"
      // здесь делаем донастройку таблицы                
      oTbl:Cargo:nModify := 0                            // счётчик изменений
      oTbl:nFreeze       := oTbl:nColumn("ARRAYNO")      // заморозить таблицу до этого столбца
      oTbl:lLockFreeze   := .T.                          // избегать прорисовки курсора на замороженных столбцах
      oTbl:nCell         := oTbl:nFreeze + 1             // передвинуть курсор на колонку номер
      oCol               := oTbl:GetColumn("ARRAYNO")  
      oCol:nClrBack      := RGB(240,240,240)             // изменение цвета виртуальной колонки
      oCol:hFont         := oCol:hFontFoot               // изменение фонта виртуальной колонки
      //oCol:hFont       := GetFontHandle(oTsb:aFont[3]) // или так
      //oTbl:DrawFooters()
      //oTbl:Refresh(.T., .T.) 
      //oTbl:Reset()

      ON KEY F1     OF wMain ACTION NIL
      ON KEY ESCAPE OF wMain ACTION _wPost(99,"wMain")

      WITH OBJECT This.Object
        :Event( 0, {|ow,ob| ob := ow:Cargo:oTbl, ob:SetFocus(), ob:DrawSelect(), ob:Show(), DoEvents() })

        :Event( 1, {|ow,ky,nI| _SetThisFormInfo(ow) , MsgDebug(nI,ky) ,;
                               _SetThisFormInfo() , ow:Setfocus("Buff"), DoEvents() })

        :Event( 2, {|ow,ky,nI| _SetThisFormInfo(ow) , MsgDebug(nI,ky) ,;
                               _SetThisFormInfo() , ow:Setfocus("Buff"), DoEvents() })

        :Event( 3, {|ow,ky,nI| _SetThisFormInfo(ow) , MsgDebug(nI,ky) ,;
                               _SetThisFormInfo() , ow:Setfocus("Buff"), DoEvents() })

        :Event(90, {|ow,ky| // ON Release windows
                            Local cm
                            ow:Hide()
                            DO EVENTS
                            cm := ProcNL()
                            ?  cm, "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                            ?  Repl(".", Len(cm)), "=> RELEASE WINDOW <=", ow:Name
                            ?? "... Program running time -", HMG_TimeMS( App.Cargo:tStart )
                            DO EVENTS
                            Return Nil
                            })

        :Event(99, {|ow| ow:Release() })
      END WITH

   END WINDOW

   ACTIVATE WINDOW wMain

RETURN NIL

////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION TablePatam(cForm,aXDim,cBrw,aBColor,nWTsb)
   LOCAL oTsb, nClr1, nClr2, a, nHFnt, aCSize, nWCol, nI, nAdd
   LOCAL hFont, cSay, nWSum, nW2, aHead, aPict, aName

   oTsb := oHmgData()
   oTsb:cBrw           := cBrw
   oTsb:cForm          := cForm      // <--- обязательно так !!!
   oTsb:cFormName      := cForm      // или так
   oTsb:lNoPicture     := .T.
   //                         cell     Head    Foot     SpecHider  SuperHider   Edit
   oTsb:aFont          := { "Normal", "Bold", "Italic", "SpecHdr" , "SuperHd", "TsbEdit" }
   IF IsArray(oTsb:aNumber) .AND. LEN(oTsb:aNumber) == 0
      oTsb:aNumber     := NIL
   ELSE
      oTsb:aNumber     := { 1, 30 }
   ENDIF

   oTsb:nCellMarginLR := 0          // отступ от линии ячейки при прижатии влево, вправо на кол-во пробелов
   oTsb:lSpecHd       := .F.        // НЕ поставить в таблице нумератор колонок
   oTsb:lSuperHd      := .T.        // поставить в таблице суперхидер
   oTsb:cSuperHd      := This.Title
   //oTsb:uSelector    := 20
   IF IsLogic(oTsb:lFooting) .AND. !oTsb:lFooting
      oTsb:lFooting    := .F.
      oTsb:aFoot       := .F.
   ELSE
      oTsb:lFooting    := .T.                            // поставить в таблице подвал
      oTsb:aFoot       := .T.                            // заполнить подвал
   ENDIF

   nHFnt               := App.Cargo:nFontSize * 1.8
   oTsb:nHeightCell    := 28                             // высота ячеек = высоте картинки чекбокса
   oTsb:nHeightHead    := nHFnt                          // высота шапки
   oTsb:nHeightFoot    := nHFnt                          // высота подвала
   IF !IsLogic(oTsb:lSpecHd)
      oTsb:lSpecHd     := .F.                            // НЕ поставить в таблице нумератор
   ENDIF
   IF oTsb:lSpecHd
      oTsb:nHeightSpecHd := App.Cargo:nFontSize          // высота нумератора
   ENDIF

   //oTsb:lSuperHd      :=             // поставить в таблице суперхидер
   //oTsb:cSuperHd      :=
   IF IsLogic(oTsb:lSuperHd) .AND. oTsb:lSuperHd
      oTsb:nHeightSuperHd := 24                          // высота суперхидера
   ENDIF
   nClr1 := HMG_RGB2n(aBColor)     // цвет фона шапка+подвал
   nClr2 := RGB( 48, 29,26)        // серо-черный фон
   oTsb:aSuperHdColor  := {CLR_YELLOW, { nClr1, nClr2 } }   // цвет: текст и фон суперхидера
   oTsb:aBrush         := {240,240,240}                     // цвет фона под таблицей 

   // цвета в таблицу
   a := {}
   // 1 , текста ячеек
   AAdd(a, { CLR_TEXT, CLR_BLACK } )                // 1 , текста ячеек
   // 2 , фона в ячейках таблицы
   //AAdd(a, { CLR_PANE, {|nr,nc,ob,nd| nr := CLR_BLACK, nc := CLR_WHITE, nd := CLR_HGRAY, ;
   //                      iif( (ob:cAlias)->(DELETED()), nr, iif( ob:nAt % 2 == 0, nc, nd ) ) } } )
   AAdd(a, { CLR_HEADF, CLR_WHITE                })  // 3 , текста шапки таблицы
   AAdd(a, { CLR_HEADB, { nClr2, nClr1 }         })  // 4 , фона шапки таблицы
   AAdd(a, { CLR_FOCUSB, {|a,b,c| a := b, If( c:nCell == b, -CLR_HRED, -CLR_BLUE ) } } ) // 6 , фона курсора
   AAdd(a, { CLR_EDITF, CLR_YELLOW               })  // 7 , текста редактируемого поля
   AAdd(a, { CLR_EDITB, CLR_HRED                 })  // 8 , фона редактируемого поля
   AAdd(a, { CLR_FOOTF, CLR_WHITE                })  // 9 , текста подвала таблицы
   AAdd(a, { CLR_FOOTB, { nClr1, nClr2 }         })  // 10, фона подвала таблицы
   AAdd(a, { CLR_SPCF , CLR_YELLOW               })  // 18, specheader text - нумератор
   AAdd(a, { CLR_SPCB , { nClr1, nClr2 }         })  // 19, specheader back - нумератор
   oTsb:aColorAdd := a
   oTsb:lZebra    := .T.
   //oTsb:aZebra  := { {230,230,230}, SILVER }    // серый
   oTsb:aZebra    := { {240,222,175}, {231,173,30} }

   //                1                 2              3                4                5                6               7               8               
   aHead := {"Calculation;year", "Receipt;(rub.)", "Receipt;date", "Type of;payment", "Accrued;(rub.)", "Accrued;date", "DEBT;(rub.)", "Tariff;(rub.)" }
   aPict := { "0000"           , "999 999.990"   , "99.99.990"   , "xxxx"           , ""              , "99.99.990"   , "999 999.990", "99999.990"     }
   aName := { "R_GOD"          , "P_SUM"         , "P_DAT"       , "V_OPL"          , "N_SUM"         , "N_DAT"       , "DOLG"       , "TARIF"         }
   oTsb:aName := aName    
   oTsb:aHead := aHead    
   // можно задать просто цифры в шапке таблицы
   IF ! IsArray(oTsb:aHead)
      a := aXDim[1]
      aHead  := {}
      FOR nI := 1 TO LEN(a)
         AADD( aHead, HB_ValToExp(nI) )
      NEXT
      oTsb:aHead := aHead
   ENDIF
   // расчёт ширины колонок 
   hFont  := GetFontHandle(oTsb:aFont[1])  // "Normal"
   nWSum  := 0
   aCSize := {}
   FOR nI := 1 TO LEN(aHead)
      nWCol := 0
      cSay := aHead[nI]
      IF ";" $ aHead[nI]
         cSay := SUBSTR(cSay, 1, AT(";",cSay) - 1) 
      ENDIF
      cSay += "H"
      nWCol := GetTextWidth( Nil, cSay, hFont )
      nW2   := GetTextWidth( Nil, aPict[nI], hFont )
      IF nW2 > nWCol
         nWCol := nW2
      ENDIF
      AADD( aCSize, nWCol )
      nWSum += nWCol
   NEXT
   nAdd := 0
   IF nWTsb > nWSum
      nAdd := nWTsb - nWSum - 60
   ENDIF
   // разницу по ширине таблицы добавляем в колонку 4
   aCSize[4] := aCSize[4] + nAdd
   // присвоим ширину колонок для передачи в _TBrowse()
   oTsb:aSize := aCSize
   ? "nWTsb=", nWTsb, "nWSum=", nWSum, "oTsb:aSize", HB_ValToExp(oTsb:aSize)

   oTsb:bInit  := {|ob| ob:Hide() }
   // блок кода для объединения колонок в одну колонку (1)
   oTsb:bAfter := {|ob|
                   Local oo, i
                   ob:Cargo:oDraw_1 := oHmgData()
                   oo := ob:Cargo:oDraw_1
                   oo:nSizes := 0
                   oo:lSizes := .T.         // надо считать общую длину
                   oo:cGod   := "R_GOD"
                   oo:cItog  := "TOTAL"
                   oo:hItog  := GetFontHandle("Bold")
                   oo:cFirst := "P_SUM"
                   oo:cLast  := "TARIF"
                   oo:nFirst := ob:nColumn(oo:cFirst)
                   oo:nLast  := ob:nColumn(oo:cLast )
                   oo:cNames := "," 
                   FOR i := oo:nFirst TO oo:nLast
                       oo:cNames += ob:aColumns[ i ]:cName + ","
                   NEXT
                   oo:cDolg     := "DOLG"
                   oo:nDolgFore := CLR_RED
                   oo:nClrFore  := CLR_YELLOW
                   oo:nClrBack  := CLR_RED
                   oo:nClrTo    := CLR_RED
                   oo:nTxt      := Len(ob:aArray[1])
                   oo:nMet      := oo:nTxt - 1
                   Return Nil
                   }

RETURN oTsb

///////////////////////////////////////////////////////////////////////////////
FUNCTION LoadDim()
   LOCAL a := array(20), aDim := {}, nI, nK := 0, aVal
   //LOCAL cT := "Длиная... длинная...  длинная... строка как в Экселе на всю таблицу ("
   LOCAL cT := "Long... long... long... string like in Excel for the whole table ("

   AEval(a, {|t,n| t := cT, a[n] := t + str(n,4) + ")" })
   //               1        2    3         4   5     6        7  8  9  10       (9 и 10 колонку не показываем)
   AADD( aDim, {"2024 year", 0, 0d00000000, "", 0, 0d00000000, 0, 0, 1, a[ ++nK ]}                               )
   AADD( aDim, {"", 0.00, 0d20240630, "     ", 68.00, 0d20240630, -68.00, 68.00, 0,""}                           )
   AADD( aDim, {"", 68.00, 0d20240710, "RIC files                      ", 0.00, 0d00000000, 0.00, 68.00, 0,""}   )
   AADD( aDim, {"", 0.00, 0d20240731, "     ", 68.00, 0d20240731, -68.00, 68.00, 0,""}                           )
   AADD( aDim, {"", 78.00, 0d20240809, "Sberbank files                 ", 0.00, 0d00000000, 0.00, 118.00, 0,""}  )
   AADD( aDim, {"", 0.00, 0d20240831, "     ", 118.00, 0d20240831, -108.00, 118.00, 0,""}                        )
   AADD( aDim, {"", 108.00, 0d20240910, "VTB24 files ....              ", 0.00, 0d00000000, 0.00, 118.00, 0,""}  ) 
   AADD( aDim, {"", 0.00, 0d20240930, "     ", 118.00, 0d20240930, -118.00, 118.00, 0,""}                        )
   AADD( aDim, {"", 118.00, 0d20241010, "RIC files                     ", 0.00, 0d00000000, 0.00, 118.00, 0,""}  ) 
   AADD( aDim, {"", 0.00, 0d20241031, "     ", 118.00, 0d20241031, -118.00, 118.00, 0,""}                        )
   AADD( aDim, {"   TOTAL", 372.00, "", "", 490.00, "", -118.00, "", 0,""}                                       )
   AADD( aDim, {"", "", "", "", "", "", "", "", 0,""}                                                            )
   AADD( aDim, {"2023 year", 0, 0d00000000, "", 0, 0d00000000, 0, 0, 0,a[ ++nK ]}                                )
   AADD( aDim, {"", 108.00, 0d20230910, "VTB24 files                   ", 0.00, 0d00000000, 0.00, 118.00, 0,""}  ) 
   AADD( aDim, {"", 0.00, 0d20230930, "     ", 118.00, 0d20130930, -118.00, 118.00, 0, ""}                       )
   AADD( aDim, {"", 118.00, 0d20231010, "RIC files                     ", 0.00, 0d00000000, 0.00, 118.00, 0,""}  ) 
   AADD( aDim, {"   TOTAL", 226.00, "", "", 118.00, "", -108.00, "", 0,""}                                       )
   AADD( aDim, {"", 0.00, 0d20241031, "     ", 118.00, 0d20241031, -118.00, 118.00, 0,""}                        )
   AADD( aDim, {"2022 year", 0, 0d00000000, "", 0, 0d00000000, 0, 0, 1,a[ ++nK ]}                                )
   AADD( aDim, {"", "", "", "NO PAYMENTS", "", "", "", "", 0,""}                                                 )
   AADD( aDim, {"", "", "", "", "", "", "", "", 0,""}                                                            )
   AADD( aDim, {"2021 year", 0, 0d00000000, "", 0, 0d00000000, 0, 0, 1,a[ ++nK ]}                                )
   AADD( aDim, {"", "", "", "NO PAYMENTS", "", "", "", "", 0,""}                                                 )
   AADD( aDim, {"", "", "", "", "", "", "", "", 0,""}                                                            )
   AADD( aDim, {"2020 year", 0, 0d00000000, "", 0, 0d00000000, 0, 0, 1,a[ ++nK ]}                                )
   AADD( aDim, {"", "No", "recno", "in DB-ABON2020", "", "", "", "", 0,""}                                       )
   AADD( aDim, {"", "", "", "", "", "", "", "", 0,""}                                                            )

   FOR nI := 1 TO LEN(aDim)
      aVal := aDim[nI]
      nK   := LEN(aVal)
      IF nK # 10
         ? nI, LEN(aVal), HB_ValToExp(aVal)
      ELSEIF !Empty(aVal[nK])
         aDim[nI][nK - 1] := 1
      ENDIF
   NEXT

RETURN aDim
///////////////////////////////////////////////////////////////////////////////
FUNCTION ProcNL(nVal, cMsg)
   Default cMsg := ">>> "
   nVal := iif( Empty(nVal), 0, nVal ) + 1
   cMsg += ProcName(nVal) + "(" + hb_ntos( ProcLine(nVal) ) + ")"
   cMsg += " => " + ProcFile(nVal)
RETURN cMsg

*----------------------------------------------------------------------------*
INIT PROCEDURE Sets_ENV()
*----------------------------------------------------------------------------*
   LOCAL o, cLog, cIni  := hb_FNameExtSet( App.ExeName, ".ini" )

   SET CODEPAGE TO RUSSIAN
   SET LANGUAGE TO RUSSIAN

   //rddSetDefault( "DBFCDX" )

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
   SET DATE FORMAT TO "DD.MM.YY"
   SET TOOLTIPSTYLE BALLOON
   //!!! такой порядок
   IF !HB_ISOBJECT( App.Cargo ) ; App.Cargo := oHmgData()
   ENDIF
   o := App.Cargo

   Set ShowRedAlert On        // увеличить фонт для окна "Program Error"

   // Проверка на запуск второй копии программы
   _HMG_MESSAGE[4] := "Attempting to run a second copy of the program:" + CRLF + ;
                      App.ExeName + CRLF + ;
                      "Refused to start." + CRLF + _HMG_MESSAGE[4]
   SET MULTIPLE QUIT WARNING  // окно маленькое
   SET WINDOW MAIN OFF

   o:tStart         := hb_DateTime()        // start time
   o:cLogFile       := ChangeFileExt( App.ExeName, '.log' )
   // для отладки - потом убрать
   cLog             := o:cLogFile
   //o:cLogFile       := cFilePath( cLog ) + "\"
   //o:cLogFile       += "_" + cFileNoPath( cLog )
   //
   o:tStart         := hb_DateTime()       // start time
   o:cIniFile       := cIni
   o:lLogDel        := .T.
   o:aDlgBColor     := {  5 , 191, 255 }     // Alert* BackColor
   o:aDlgFColor     := {  0 ,  0 ,  0  }     // Alert* FontColor
   o:aBClrMain      := {127,189,228}
   o:cDefAppIcon    := "1MG"
   o:lDebug         := .T.
   o:nMenuBmpHeight := 32
   o:aWinOpen       := {}
   o:cTitle         := PROGRAM + " ! " + PROGINF
   o:cVersion       := PROGVER
   o:cLang          := "EN"
   o:cAvtor         := "Copyright 2024 Verchenko Andrey + Sergej Kiselev"
   o:cEmail         := "<verchenkoag@gmail.com> Dmitrov, Moscow region / <bilance@bilance.lv>"
   o:cPrgInfo1      := "Many thanks for your help: Grigory Filatov <gfilatov@inbox.ru>"
   o:cPrgInfo2      := "Tips and tricks programmers from our forum http://clipper.borda.ru"
   o:cSiteDownload  := "Home page for download - http://www.hmgextended.com/"
   o:cPathTemp      := GetUserTempFolder() + "\"
   o:cPathDbf       := GetStartUpFolder() + "\DBF\"
   o:cPathStart     := GetStartUpFolder() + "\"
   //o:aDisplayMode := { System.DesktopWidth , System.DesktopHeight - GetTaskBarHeight() }
   o:aDisplayMode   := { Sys.ClientWidth , Sys.ClientHeight }
   // задание своих параметров, позволяет протестировать на другие разрешения экрана
   // setting your parameters, allows you to test for other screen resolutions
   //o:aDisplayMode   := { 1280 , 1280 }
   o:cDisplayMode   := HB_NtoS(o:aDisplayMode[1]) + "x" + HB_NtoS(o:aDisplayMode[2])
   o:cFontName      := "DejaVu Sans Mono"   // "Arial"
   o:cFontName2     := "Comic Sans MS"
   o:nFontSize      := 14
   o:cDlgFont       := "DejaVu Sans Mono"
   o:nDlgSize       := o:nFontSize + 2

   _SetGetLogFile( o:cLogFile )

   IF o:lLogDel ; hb_FileDelete( o:cLogFile )
   ENDIF

   IF o:lDebug ; SET LOGERROR ON
   ELSE        ; SET LOGERROR OFF
   ENDIF

   // Default font
   SET FONT TO o:cFontName , o:nFontSize
   // TsBrowse                                       bold italic
   _DefineFont("Normal"  , o:cFontName , o:nFontSize  , .F., .F. )
   _DefineFont("Bold"    , o:cFontName , o:nFontSize  , .T., .F. )
   _DefineFont("Italic"  , o:cFontName , o:nFontSize-2, .F., .T. )
   _DefineFont("ItalBold", o:cFontName , o:nFontSize-2, .T., .T. )
   _DefineFont("SpecHdr" , o:cFontName , o:nFontSize-4, .T., .T. )
   _DefineFont("SuperHd" , o:cFontName2, o:nFontSize+2, .F., .F. )
   _DefineFont("TsbEdit" , "Arial"     , o:nFontSize  , .F., .T. )
   // Menu* font
   _DefineFont("ComSanMS" , o:cFontName2 , o:nFontSize+2 , .F., .F. )         // фонт главного верхнего меню
   _DefineFont("MnNormal" , o:cFontName  , o:nFontSize+2 , .F., .F. )         // фонт главного верхнего меню
   _DefineFont("MenuBtn"  , o:cFontName  , o:nFontSize   , .T., .F. )         // фонт кнопок верхнего меню
   _DefineFont("WinBtn"   , o:cFontName  , o:nFontSize+2 , .F., .F. )         // фонт кнопок окон
   // Alert* font
   _DefineFont("DlgFont" , o:cDlgFont , o:nDlgSize   , .F., .F. )             // фонт окна Alert*
   // Alert* colors
   SET MSGALERT BACKCOLOR  TO o:aDlgBColor
   SET MSGALERT FONTCOLOR  TO o:aDlgFColor
   //
   SET DEFAULT ICON TO o:cDefAppIcon
   SET WINDOW MODAL PARENT HANDLE ON
   SET TOOLTIPSTYLE BALLOON
   SET NAVIGATION EXTENDED
   SET MENUSTYLE  EXTENDED
   SetMenuBitmapHeight( 32 )           // set menu icons size to 32x32

RETURN

