/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2024 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Просмотр массивов в окне через _TBrowse()
 * Viewing arrays in a window using _TBrowse()
*/
#define  _HMG_OUTLOG
#include "hmg.ch"
#include "tsbrowse.ch"

#define PROGRAM  "MG Test AlertTsb()"
#define PROGVER  "Version 0.63 (16.03.2025)"
#define PROGINF  "Viewing arrays in a window using _TBrowse()"

FUNCTION Main()
   LOCAL nH, nW, nY, aBClr, cWIco, cVal, owc
   LOCAL aParam := hb_aParams()

   nH    := 64 + GetMenuBarHeight() + GetTitleHeight() // высота окна главной формы
   aBClr := {42,174,239}
   cWIco := "1MG"
   cVal  := MiniGuiVersion() + CRLF + Version() + CRLF + hb_Ccompiler()

   SET FONT TO _GetSysFont(), App.Cargo:nFontSize

   DEFINE WINDOW wMain CLIENTAREA Sys.ClientWidth, nH   ;
      TITLE PROGRAM ICON cWIco                          ;
      MAIN NOMAXIMIZE NOSIZE BACKCOLOR aBClr          ;
      ON INIT _wPost(0)                                 ;
      ON RELEASE _wSend(90)

      nW  := This.ClientWidth
      nH  := This.ClientHeight

      This.Cargo := oHmgData() ; owc := This.Cargo
      owc:cForm  := This.Name
      owc:aBClr  := aBClr

      DEFINE MAIN MENU
         DEFINE POPUP 'AlertTSB(&1)' FONT "ComSanMS"
            ITEM 'One button on the form' DISABLED FONT "ComSanMS"
            SEPARATOR
            ITEM 'AlertTSB("INFO",64,...)'             ACTION _wPost(1,,1)  FONT "DlgFont"
            ITEM 'AlertTSB("STOP",64,...)'             ACTION _wPost(1,,2)  FONT "DlgFont"
            ITEM 'AlertTSB("EXCLAM",64,...)'           ACTION _wPost(1,,3)  FONT "DlgFont"
            ITEM 'AlertTSB("iMgLogfile128",96,...)'    ACTION _wPost(1,,4)  FONT "DlgFont"
            ITEM 'AlertTSB("iMgLogfile128",96,"text")' ACTION _wPost(1,,5)  FONT "DlgFont"
            ITEM 'Return = AlertTSB("EXCLAM",64,...)'  ACTION _wPost(1,,6)  FONT "DlgFont"
            SEPARATOR
            ITEM 'Exit'  ACTION _wPost(99) ICON "iExit32"   FONT "ComSanMS"
         END POPUP

         DEFINE POPUP 'AlertTSB(&2-3)' FONT "ComSanMS"
            ITEM 'Two buttons on the form' DISABLED FONT "ComSanMS"
            SEPARATOR
            ITEM 'AlertTSB("YesNo",96,...)'        ACTION _wPost(2,,1)  FONT "DlgFont"
            ITEM 'AlertTSB("RetryCancel",72,...)'  ACTION _wPost(2,,2)  FONT "DlgFont"
            ITEM 'AlertTSB("RetryCancel",72,...{"Save","Exit"})'  ACTION _wPost(2,,3)  FONT "DlgFont"
            ITEM 'AlertTSB("YesNoCancel",72,...)'  ACTION _wPost(2,,4)  FONT "DlgFont"
            ITEM 'AlertTSB("YesNoCancel",72,...,{"Save","Continue","Exit"})'  ACTION _wPost(2,,5)  FONT "DlgFont"
            ITEM 'Return = AlertTSB("YesNoCancel",64,...,{"1-Btn","2-Btn","3-Btn"})'  ACTION _wPost(2,,6)  FONT "DlgFont"
         END POPUP

         DEFINE POPUP '&Complex AlertTSB()' FONT "ComSanMS"
            ITEM 'Complex AlertTSB on the form' DISABLED FONT "ComSanMS"
            SEPARATOR
            ITEM 'AlertTSB("iMgLogfile128",96,...,{"Print","Save",..})'   ACTION Test3()  FONT "DlgFont"
            ITEM 'AlertTSB("iMgSave128",128,,"Config",{"Save","Cancel"})' ACTION Test4()  FONT "DlgFont"
            ITEM 'AlertTSB("iMgLogfile128",96,,"File CSV")' ACTION Test5()  FONT "DlgFont"
         END POPUP

      END MENU

      nY := 5
      DRAW ICON IN WINDOW wMain AT nY+5, nW-64 PICTURE "1MG" WIDTH 64 HEIGHT 64 COLOR aBClr

      @ nY, 5 LABEL Buff VALUE cVal WIDTH nW-64-nY*2 HEIGHT nH - nY*2 ;
        FONTCOLOR BLUE TRANSPARENT RIGHTALIGN

      ON KEY F1     OF wMain ACTION NIL
      ON KEY ESCAPE OF wMain ACTION _wPost(99,"wMain")

      WITH OBJECT This.Object
        :Event( 0, {|ow| DoEvents(), ow:Setfocus("Buff") })

        :Event( 1, {|ow,ky,nI| _SetThisFormInfo(ow) , Test_AlertTsb(nI,ky) ,;
                               _SetThisFormInfo() , ow:Setfocus("Buff"), DoEvents() })

        :Event( 2, {|ow,ky,nI| _SetThisFormInfo(ow) , Test_AlertTsb(nI,ky) ,;
                               _SetThisFormInfo() , ow:Setfocus("Buff"), DoEvents() })

        :Event( 3, {|ow,ky,nI| _SetThisFormInfo(ow) , Test_AlertTsb(nI,ky) ,;
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

///////////////////////////////////////////////////////////////////////////////
FUNCTION Test_AlertTsb(nI,ky)
   LOCAL cPath, cMask, cAttr, aDirs, aRet, n

   cPath := App.Cargo:cPathStart
   cMask := "*.*"
   cAttr := "D"
   aDirs := hb_Directory(cPath + cMask, cAttr)
   FOR n := 1 TO 3
      hb_ADel( aDirs, 1, .T. )
   NEXT
   SWITCH ky
   CASE 1
      IF nI == 1
      AlertTSB("INFO",64,aDirs)
      ELSEIF nI == 2
      AlertTSB("STOP",64,aDirs)
      ELSEIF nI == 3
      AlertTSB("EXCLAM",64,aDirs)
      ELSEIF nI == 4
      AlertTSB("iMgLogfile128",96,aDirs)
      ELSEIF nI == 5
      AlertTSB("iMgLogfile128",72,{"text"})
      ELSEIF nI == 6
      aRet := AlertTSB("EXCLAM",72,aDirs)
      MsgDebug("Return AlertTSB()=",aRet)
      ENDIF
      EXIT
   CASE 2
      IF nI == 1
      AlertTSB("YesNo",96,aDirs)
      ELSEIF nI == 2
      AlertTSB("RetryCancel",72,aDirs)
      ELSEIF nI == 3
      AlertTSB("RetryCancel",72,aDirs,"Your buttons",{"Save","Exit"})
      ELSEIF nI == 4
      AlertTSB("YesNoCancel",72,aDirs,"YesNoCancel")
      ELSEIF nI == 5
      AlertTSB("YesNoCancel",72,aDirs,"Your buttons",{"Save","Continue","Exit"})
      ELSEIF nI == 6
      aRet := AlertTSB("YesNoCancel",64,aDirs,"Your buttons",{"1-Button","2-Button","3-Button"})
      IF LEN(aRet) > 0
         MsgDebug("Return AlertTSB()=","Number of the pressed button=",aRet[1],;
                   "Name button=", aRet[2], "Corrected array=", aRet[3] )
      ELSE
         MsgDebug("Return AlertTSB()=",aRet)
      ENDIF
   ENDIF
   END

RETURN NIL

////////////////////////////////////////////////////////////////////////
FUNCTION Test3()
   LOCAL nI, cPath, cMask, cAttr, aDirs, aRet, aBtn, oWin, oTsb, bInit
   LOCAL aChk, a

   cPath := App.Cargo:cPathStart
   cMask := "*.*"
   cAttr := "D"
   aDirs := hb_Directory(cPath + cMask, cAttr)
   FOR nI := 1 TO 3
      hb_ADel(aDirs, 1, .T.)
   NEXT
   // добавим колонку в массив
   FOR EACH a IN aDirs
      hb_AIns(a, 1, .F., .T.)
   NEXT

   oWin := oHmgData()
   oWin:cHelp      := "Help line what to do, like SELECT the files you need" + CRLF + "second help line!"
   oWin:aHelpFClr  := YELLOW
   oWin:aHelpBClr  := MAROON

   // этот код выполняется уже в окне - alert_tsb.prg ПОСЛЕ END TBROWSE (в качестве примера)
   // this code is already executed in the window - alert_tsb.prg AFTER END TBROWSE (as an example)
   bInit := {|ow|
              Local ob := This.oBrw.Object
              Local op := ob:Cargo:oParam     // это oTsb
              // заменяем колонку CHECKBOX на свои картинки
              ob:aColumns[2]:aCheck   := {LoadImage("bMgCheckT24"), LoadImage("bMgCheckF24")}
              ob:aColumns[2]:lEdit    := .T.
              ob:aColumns[2]:nClrBack := CLR_YELLOW
             _LogFile(.T., "### bInit=", ProcNL(), ow:Name)
             _o2log(op,,"oTsb =", .T.)
             Return Nil
             }

   oTsb := oHmgData()
   oTsb:aHead    := {"*", "File","Size","DateTime","Time","Dir"}
   oTsb:lSpecHd  := .T.                            // поставить в таблице нумератор колонок
   oTsb:lSuperHd := .T.                            // поставить в таблице суперхидер
   oTsb:cSuperHd := "List of files to select"

   aBtn := {"Print","Print-2","Save","Continue","Exit"}
   aRet := AlertTSB("iMgLogfile128",96,aDirs,"Your buttons",aBtn,oWin,oTsb,bInit)
   DO EVENTS
   ? ProcNL(), aRet

   IF LEN(aRet) > 0
      a    := aRet[3]
      aChk := {}
      FOR nI := 1 TO LEN(a)
         IF a[nI,1]
            AADD(aChk, a[nI])
         ENDIF
      NEXT
      MsgDebug( "Return AlertTSB()=","Number of the pressed button=",aRet[1],;
                "Name button=", aRet[2], "Corrected array=", aRet[3] ,;
                "Cursor on a row in a table = ",  aRet[4]            ,;
                "Button event = " + HB_NtoS(aRet[5]) ,  REPL("~",60) ,;
                "Checkbox equals .T.", aChk )
   ELSE
      MsgDebug("Return AlertTSB()=",aRet)
   ENDIF

RETURN NIL

//////////////////////////////////////////////////////////////////////
FUNCTION Test4()
   LOCAL cIco, nIco, aDim, cTtl, aBtn, oWin, oTsb, bInitForm, aRet
   LOCAL oSec, oIni, cIni, nI, xVal, cKey, xRet, cMsg, aDog, cDT
   LOCAL lIni, cSec, aSize, aDim2, cTxt, cStr, aVal, nJ

   cIco := "iMgSave128"
   nIco := 128
   cSec := "Request"
   cTtl := "Input parameters: " + cSec
   aDog := {"I-one","II-two","III-three","IV-four","V-five","VI-six"}
   cDT  := hb_TtoC( hb_DateTime() ) + "   "

   aDim := {}
   AADD( aDim , { "Accepting applications" , "", "", "", "" , "" } )
   AADD( aDim , { "" , "Display completed applications for XX days:"                , 20             , "nDay" , "N"  , ""   } )
   AADD( aDim , { "" , "Disable/enable display of cancelled applications:"          , .F.            , "lZa1" , "L"  , ""   } )
   AADD( aDim , { "" , "Disable/enable display of other applications:"              , .F.            , "lZa2" , "L"  , ""   } )
   AADD( aDim , { "" , "Disable/enable display of applications - Key in the office:", .F.            , "lZa3" , "L"  , ""   } )
   AADD( aDim , { "" , "Application number template:"                               , "Z/02"+Space(5), "cZay" , "C"  , ""   } )
   AADD( aDim , { "Accepting contracts", "", "", "", "", "" } )
   AADD( aDim , { "" , "Displaying closed contracts for XXX days:"                  , 60             , "nDog" , "N"  , ""   } )
   AADD( aDim , { "" , "Disable/enable display of closed contracts:"                , .F.            , "lDog1", "L"  , ""   } )
   AADD( aDim , { "" , "Disable/enable display of archived contracts:"              , .F.            , "lDog1", "L"  , ""   } )
   AADD( aDim , { "" , "Contract validity date until (Option 1):"                   , DATE()         , "dDog1", "D"  , ""   } )
   AADD( aDim , { "" , "Contract validity date until (Option 2):"                   , DATE()         , "dDog2", "DMN", ""   } )
   AADD( aDim , { "" , "Date and time of contract control:"                         , cDT            , "cDTDg", "DT" , ""   } )
   AADD( aDim , { "" , "Example of displaying a menu selection with an array:"      , "(-)(-)"       , "cDog2", "ARR", HB_ValToExp(aDog) } )
   AADD( aDim , { "Display options", "", "", "", "", "" } )
   AADD( aDim , { "" , "Selection window - button font color:"                      , SPACE(5)       , "aBntFClr", "CLR", HB_ValToExp(ORANGE) } )
   AADD( aDim , { "" , "Selection window - button background color:"                , SPACE(5)       , "aBntBClr", "CLR", HB_ValToExp(YELLOW) } )
   AADD( aDim , { "" , "Selection window - button font:"                            , "Arial,15,.f.,", "cFont"   , "FNT", ""                  } )
   //              1                     2                                                 3                4        5      6

   // параметры массива будем сохранять в ини-файле
   cIni := LOWER( App.Cargo:cPathStart + ProcName() + ".ini" )
   lIni := FILE(cIni)
   // доступ к ини-файлу только в этом модуле
   oIni := TIniData():New(cIni, .T.):Read()
   Default oIni := oHmgData()
   Default oIni:INFO := oHmgData()
   Default oIni:INFO:Developed_in   := MiniGUIVersion()
   Default oIni:INFO:xBase_compiler := Version()
   Default oIni:INFO:C_compiler     := Hb_Compiler()
   Default oIni:INFO:Exe            := App.ExeName
   Default oIni:REQUEST := oHmgData()
   // если есть файл, то считаем записанное ранее
   IF lIni
      oSec := oIni:Get(cSec)
      FOR nI := 1 TO LEN(aDim)
         //xVal := aDim[nI,3]  // исх.значение в массиве
         cKey := ALLTRIM(aDim[nI,4])
         xRet := oSec:Get(cKey)
         IF !xRet == NIL
            IF IsString(xRet)
               xRet += SPACE(10)
            ENDIF
            IF aDim[nI,5] == "CLR"
               aDim[nI,6] := HB_ValToExp(xRet)
               aDim[nI,3] := SPACE(5)
            ELSE
               aDim[nI,3] := xRet
            ENDIF
         ENDIF
      NEXT
   ENDIF

   oWin := oHmgData()
   oWin:cHelp     := "Configure the settings for this selection and" + CRLF + "save the settings for future work!"
   oWin:aHelpFClr := RED
   oWin:aBtnFClr  := { WHITE  , WHITE  , WHITE   }    // цвет фонта кнопки
   oWin:aBtnBClr  := { LGREEN , ORANGE , MAROON  }    // цвет фона кнопки
   oWin:aBtnFClr2 := WHITE                            // инвертный цвет фонта кнопки (фокус на кнопке)
   oWin:aBtnBClr2 := BLUE                             // инвертный цвет фона кнопки  (фокус на кнопке)
   aBtn           := {"Save"  ,"Notepad"  , "Cancel"}

   // этот код выполняется уже в окне - alert_tsb.prg ПОСЛЕ END TBROWSE (в качестве примера)
   // this code is already executed in the window - alert_tsb.prg AFTER END TBROWSE (as an example)
   bInitForm := {|ow| //
                      Local ob := This.oBrw.Object
                      Local op := ob:Cargo:oParam     // это oTsb
                      ? "### bInitForm {|ow|...", ow:Name, ProcNL()
                      ?? op:cSuperHd, ob:nHeightSuper, ob:lDrawSuperHd
                      _o2log(op,,ProcNL() + "..... oTsb =", .T.)
                      Return Nil
                      }

   aDim2 := ACLONE(aDim)
   aSize := CalculatColumnWidths(aDim2)   // подсчёт ширины колонок

   oTsb := oHmgData()
   oTsb:aNumber     := {}                         // нет колонки нумерации
   oTsb:aHead       := {"", "", "Edit"}
   oTsb:aFoot       := {"", "", "Edit"}
   oTsb:nHeightHead := 0                          // высота шапки - убрать шапку таблицы
   oTsb:nHeightFoot := 0                          // высота подвала
   oTsb:lFooting    := .F.                        // НЕ ставить в таблице подвал
   oTsb:lNoPicture  := .T.
   oTsb:lSpecHd     := .F.                        // поставить в таблице нумератор колонок
   oTsb:lSuperHd    := .T.                        // поставить в таблице суперхидер
   oTsb:cSuperHd    := "Configuring the application entry - example"
   oTsb:aHideCol    := { 4, 5, 6 }                // скрыть колонки
   oTsb:aSize       := aSize                      // назначим ширину колонок для ТСБ

   // блоки кода для _TBrowse(...) - название переменных bInit,bBody,bEnd,bAfter менять нельзя
   // ob=oBrw, op=oTsb
   //oTsb:bInit  := {|ob,op| myTsbInit(ob,op)                   }  // настройки тсб
   //oTsb:bBody  := {|ob,op| myTsbKeyFX(ob,op), myTsbEdit(ob,op)}  // другие настройки тсб
   //oTsb:bAfter := {|ob,op| myTsbAfter(ob,op)                  }  // блок кода после END TBROWSE, чтобы не изменять oTsb:bEnd
   //oTsb:bEnd   := {|ob,op| myTsbEnd(ob,op)                    }  // блок кода после END TBROWSE
   oTsb:bBody := {|ob,op,oc| ob:nHeightHead  := 0   /* убрать шапку таблицы */     ,;
                             ob:lPickerMode := .F. , oc := ob:aColumns[3]          ,;
                             oc:lEdit  := .T. , oc:cPicture := Nil , oc:lCheckBox := .T. ,;
                             oc:nAlign := DT_CENTER , oc:nEditMove := 0 ,;  // перечитать ячейку
                             oc:aCheck := { LoadImage("bMgCheckT24"), LoadImage("bMgCheckF24") } ,;
                             _LogFile(.T., "### oTsb:bInit", ProcNL(), op:cSuperHd, ob:nHeightSuper, ob:lDrawSuperHd)  }

   oTsb:bInit := {|ob,op| //
                          //ob:HideColumns( op:aHideCol ,.t.)  // скрыть колонки - резерв
                          ob:nFreeze     := 2                  // Заморозить столбец
                          ob:lLockFreeze := .T.                // Избегать прорисовки курсора на замороженных столбцах
                          ob:nCell       := ob:nFreeze + 1
                          // цвет фона в ячейках таблицы  -> alert_tsb.prg
                          ob:Cargo:aZebra := op:aZebra
                          ob:aColumns[3]:nClrBack := { |nr,nc,ob| myAlertTsbColorBack(nr,nc,ob) }
                          // редактирование ячеек таблицы -> alert_tsb.prg
                          myAlertTsbEdit(ob,op)
                          // перечитать состояние скролинга
                          ob:ResetVScroll( .T. )
                          ob:oHScroll:SetRange( 0, 0 )
                          ob:Refresh()
                          Return Nil
                          }


   oTsb:bAfter := {|ob|
                    Local oc, nw := 0
                    ? ProcNL(), "@@@ TSB @@@ width =", ob:GetAllColsWidth()
                    FOR EACH oc IN ob:aColumns
                        ? hb_enumindex(oc), oc:lVisible, oc:nWidth
                        IF oc:lVisible ; nw += oc:nWidth
                        ENDIF
                    NEXT
                    ? "TSB nWidth =", nw  ; ?
                    Return Nil
                    }

   aRet := AlertTSB(cIco,nIco,aDim,cTtl,aBtn,oWin,oTsb,bInitForm)
   DO EVENTS
   ? ProcNL(), aRet

   IF LEN(aRet) > 0
      cMsg := "Return AlertTSB()=;"
      cMsg += "Number of the pressed button = " + HB_NtoS(aRet[1]) + ";"
      cMsg += "Name button = " + aRet[2] + ";"
      cMsg += "Table array = ob:aArray[" + HB_NtoS(LEN(aRet[3])) + "];"
      cMsg += "Cursor on a row in a table = " + HB_NtoS(aRet[4]) + ";"
      cMsg += "Button event = " + HB_NtoS(aRet[5]) + ";;"

      // запишем в ини-файл
      aDim := aRet[3]
      IF aRet[1] == 1  // aRet[2] == "Save"
         //oIni:Set(cSec, oHmgData())  // секцию добавили - не нужно, есть вверху
         oSec := oIni:Get(cSec)
         FOR nI := 1 TO LEN(aDim)
            xVal := aDim[nI,3]
            cKey := ALLTRIM(aDim[nI,4])
            IF LEN(cKey) > 0
               oSec:Set(cKey, xVal )
               IF aDim[nI,5] == "CLR"
                  xVal := aDim[nI,6]
                  oSec:Set(cKey, xVal )
               ELSE
                  oSec:Set(cKey, xVal )
               ENDIF
            ENDIF
         NEXT
         oSec:Set("Created", hb_TtoC( hb_DateTime()  ) )
         oIni:cCommentBegin := " Modify: " + hb_TtoC( hb_DateTime() )
         oIni:Write()  // НЕ UTF8, т.е. нет BOM на выходе
         //_o2log(oIni,,ProcNL() + "..... oIni =", .T.)
         cMsg += "Data from column 3 has been written to the file !;"
         cMsg += cIni
      ELSEIF aRet[1] == 2  // aRet[2] == "Notepad"
         cTxt := hb_FNameExtSet( cIni, ".txt" )
         cMsg += "Calling the Notepad editor with a text file !;"
         cMsg += cTxt
         cStr := ""
         FOR nI := 1 TO LEN(aDim)
            aVal := aDim[nI]
            FOR nJ := 1 TO LEN(aVal)
               xVal := aVal[nJ]
               IF VALTYPE(xVal) != "C"
                   xVal := cValToChar(xVal)
               ENDIF
               cStr += xVal + " ; "
            NEXT
            cStr += + CRLF
         NEXT
         HB_MemoWrit( cTxt, cStr )
         ShellExecute( 0, "open", "notepad.exe", cTxt,, 1 )
      ELSE
      ENDIF
      AlertInfo( cMsg, "Result", "iMgInfo128", 64, {RED} )
   ELSE
      MsgDebug("Return AlertTSB()=",aRet)
   ENDIF

RETURN NIL

///////////////////////////////////////////////////////////////////////////
FUNCTION Test5()
   LOCAL cIco, nIco, aDim, cTtl, cFile, cPath, aRet, cStr, cSprt, aDim2
   LOCAL aVal, cVal, aArray, nI, nJ, oTsb, aHead

   cPath := App.Cargo:cPathStart
   cFile := cPath + "layoffs.csv"
   cIco  := "iMgLogfile128"
   nIco  := 96
   cTtl  := "File CSV - " + cFile
   cSprt := ","

   IF FILE(cFile)
      cStr  := hb_MemoRead(cFile)
      aDim  := HB_ATokens(cStr,CRLF,.F.,.F.)

      aArray := {}
      FOR nI := 1 TO LEN(aDim)
         cStr := aDim[nI]
         IF nI == 1
            aDim2 := HB_ATokens(cStr,cSprt,.F.,.F.)
            aHead := {}
            FOR nJ := 1 TO LEN(aDim2)
               aDim2[nJ] := Upper(Left(aDim2[nJ],1)) + SubStr(aDim2[nJ],2)
               AADD( aHead, StrTran(aDim2[nJ], "_", " " ) )
            NEXT
            LOOP
         ENDIF
         cVal := SUBSTR(cStr,1,1)
         // Доп.обработка массива
         IF cVal == "#" .OR. cVal == "="
            LOOP
         ELSEIF LEN(ALLTRIM(cStr)) == 0
            LOOP
         ELSE
            // запись строки в массив
            aDim2 := HB_ATokens(cStr,cSprt,.F.,.F.)
            aVal  := {}
            FOR nJ := 1 TO LEN(aDim2)
if val(aDim2[5]) == 1
               AADD( aVal, aDim2[nJ] )
endif
            NEXT
      If Len(aVal) > 0
            AADD( aArray , aVal )
      EndIf
         ENDIF
      NEXT

      If Len(aArray) == 0
         aArray := { { "Error! ", "Could not create array for import !" } }
else
aSort(aArray, NIL, NIL, {| x, y | val(x[ 9 ]) > val(y[ 9 ]) })
      EndIf

      oTsb := oHmgData()
      oTsb:aHead   := aHead
      oTsb:aNumber := {}
   ELSE
      aArray := { { "Error! No file !", cFileNoPath(cFile), cFile } }
   ENDIF

   aRet := AlertTSB(cIco,nIco,aArray,cTtl,,,oTsb)

RETURN NIL

////////////////////////////////////////////////////////////////
FUNCTION CalculatColumnWidths(aDim)   // подсчёт ширины колонок
   LOCAL aSiz, v, a, i, n, aSize, nLen, kfc, aFnt

   aSiz := Array(Len(aDim[1])) ; aFill(aSiz, 0)
   FOR EACH a IN aDim
       FOR EACH v IN a
           i := hb_enumindex(v)
           // показ только 3 колонок
           IF i > 3 ; LOOP
           ENDIF
           IF !IsChar(v) ; v := cValToChar(v)
           ENDIF
           n := Len(trim(v)) + 2
           IF n < 10 ; n := 10
           ENDIF
           IF n > aSiz[ i ] ; aSiz[ i ] := n
           ENDIF
       NEXT
   NEXT

   aFnt := GetFontParam("Normal")
   IF UPPER("DejaVu") $ UPPER(aFnt[1])
      kfc := 0.98   // для DejaVu Sans Mono
   ELSE
      kfc := 0.7    // для Arial и др.фонты
   ENDIF

   aSize := {}
   FOR EACH n IN aSiz
      AAdd( aSize, int( GetFontWidth("Normal", n) * kfc ) )
   NEXT

   nLen := 0 ; AEval(aSize, {|nn| nLen += nn })
   ? ProcNL(), ">>> TSB >>> width =", aSiz, aSize, nLen
   FOR EACH v,n IN aSiz,aSize
       ? hb_enumindex(v), v, n
   NEXT

RETURN aSize

////////////////////////////////////////////////////////////////
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

   IF !HB_ISOBJECT( App.Cargo ) ; App.Cargo := oHmgData()
   ENDIF
   o := App.Cargo

   Set ShowRedAlert On        // for "Program Error" form

   // second copy checking
   _HMG_MESSAGE[4] := "Attempting to run a second copy of the program:" + CRLF + ;
                      App.ExeName + CRLF + ;
                      "Refused to start." + CRLF + _HMG_MESSAGE[4]
   SET MULTIPLE QUIT WARNING
   SET WINDOW MAIN OFF

   o:tStart         := hb_DateTime()        // start time
   o:cLogFile       := ChangeFileExt( App.ExeName, '.log' )
   cLog             := o:cLogFile
   o:cLogFile       := cFilePath( cLog ) + "\"
   o:cLogFile       += "_" + cFileNoPath( cLog )
   //
   o:tStart         := hb_DateTime()       // start time
   o:cIniFile       := cIni
   o:lLogDel        := .T.
   o:aDlgBColor     := {  5 , 191, 255 }     // Alert* BackColor
   o:aDlgFColor     := {  0 ,  0 ,  0  }     // Alert* FontColor
   o:aBClrMain      := {127,189,228}
   o:cDefAppIcon    := "1MG"
   // debug
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
   o:cFontName      := "Arial"             // "DejaVu Sans Mono"
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
   _DefineFont("TsbEdit" , "Tahoma"    , o:nFontSize  , .F., .T. )
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

