/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2023 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2023 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 * SAMPLES\Advanced\Tsb_Viewer
 *
 * Просмотр/правка Dbf файла. Опции/свойства по базе
 * View/edit Dbf file. Options/properties by base
*/

#define _HMG_OUTLOG

#include "minigui.ch"
#include "tsbrowse.ch"
#include "Dbinfo.ch"
////////////////////////////////////////////////////////////////////////////////////
// Показ окна с таблицей / Show table window
FUNCTION my_Standard3( cForm, nBtn, cAls, cWndMain )
   LOCAL cVal, nI, hW, bInitForm, oCnf, cSection, aPost, aThrd
   LOCAL cPathDbf := App.Cargo:cPathDbf  //GetStartUpFolder()
   LOCAL oWin, oUse, oMenu, oTsb, aEvent, oIndx, cMsg, aCurrLang
   DEFAULT nBtn     := 3
   DEFAULT cForm    := "w"+hb_ntos(nBtn)+"_Standart"
   DEFAULT cAls     := ""
   DEFAULT cWndMain := App.Cargo:cMainMenuProg    // имя окна главного меню программы Forma_Main

   aCurrLang := { hb_SetCodepage(), hb_CdpSelect(), Hb_LangSelect() }  // текущий язык программы

   SET CODEPAGE TO UKRAINIAN     // аналог hb_SetCodepage( "UA1251" )
   SET LANGUAGE TO UKRAINIAN     // аналог hb_CdpSelect( "UA1251" )
   // переделана функция _LogFile() для смены языка -> util_misc.prg

   cVal := nI := hW
   // это в лог не попадает, т.к. язык поменяли, ЕСЛИ оставить функцию _LogFile() без изменений
   ? ProcNL(), cForm, nBtn, cAls, cWndMain, "|" ,_IsWindowDefined(cForm), hb_CdpSelect()

   //cAls += nBtn  // тестовая проверка ошибки при другом языке

   aThrd := WaitThreadAvi( 'Create a table ...',,,2 )  // создаём окно ожидания с потоком
   // основной цикл вычислений/расчётов/созданий баз и т.д. - в качестве примера
   FOR nI := 1 TO 20
      wApi_Sleep( 70 )
      DO EVENTS
      cVal := hb_ntos( nI ) + "/" + "30"
      WaitThreadAviSay( aThrd, cVal )
   NEXT

   IF _IsWindowDefined( cWndMain )
      Domethod(cWndMain, "Minimize")
      DO EVENTS
   ENDIF
   SET FONT TO "DejaVu Sans Mono", 13   // новый фонт для таблицы

   oCnf     := App.Cargo:oCnf           // cnfg-файл из глобальной App.Cargo
   cSection := "ТАБЛИЦЯ_БД_АБОНЕНТИ"    // секция
   ? ProcNL(), "===== [" + cSection + "] ====="
   //_o2Log(oCnf, 20, "==> .T. cnfg: ", .T.)
   // Проверка секции
   IF !CnfgSection(oCnf, cSection)
      RETURN .F.
   ENDIF

   // считаем параметры из cnfg-файла
   oWin  := CnfgDataWin(oCnf, cSection)              // параметры окна
   oUse  := CnfgDataDbf(oCnf,cSection,cPathDbf,cAls) // одна или несколько баз
   oIndx := Tsb3DataIndex(oUse, cPathDbf)            // создаю индексы - можно тоже поместить в cfg-файл
   oMenu := CnfgDataMenu(oCnf, cSection)             // меню-кнопки окна и события по кнопкам
   oTsb  := CnfgDataTsb(oCnf, cSection, oUse, oWin)  // параметры ТСБ
   oWin:aCurrLang := aCurrLang                       // запомнить текущий язык программы, эта переменная
                                                     // передаётся и обрабатывается в TsbViewer.prg

   //_o2Log(oWin,  20, "==> .T. oWin: ", .T.)        // проверка в лог-файл
   //_o2Log(oUse,  20, "==> .T. oUse: ", .T.)        // проверка в лог-файл
   //_o2Log(oMenu, 20, "==> .T. oMenu: ", .T.)       // проверка в лог-файл
   //_o2Log(oTsb,  20, "==> .T. oTsb: ", .T.)        // проверка в лог-файл

   // код ниже можно тоже поместить в cfg-файл, для наглядности оставлено так
   aPost  := oMenu:aPost //{1,2,3,4,99}                                  // код события на кнопке меню
   aEvent := {}                                                          // события на окне, вызов функций
   AAdd( aEvent, { aPost[1], {|ow,ky,cn,ob| ob := This.oBrw.Object, my3Btn1(ow,ky,cn,ob)  } } )  // кнопка 1
   AAdd( aEvent, { aPost[2], {|ow,ky,cn,ob| ob := This.oBrw.Object, my3Btn1(ow,ky,cn,ob)  } } )  // кнопка 2
   AAdd( aEvent, { aPost[3], {|ow,ky,cn,ob| ob := This.oBrw.Object, my3Btn1(ow,ky,cn,ob)  } } )  // кнопка 3
   AAdd( aEvent, { aPost[4], {|ow,ky,cn,ob| ob := This.oBrw.Object, my3Btn1(ow,ky,cn,ob)  } } )  // кнопка 4
   AAdd( aEvent, { aPost[5], {|ow,ky,cn| SetProperty(ow:Name, cn, "Enabled", .T. ), ow:Release(), ky:=cn } } ) // выход
   //_o2Log(aEvent, 20, "==> .T. cnfg: ", .T.) // проверка в лог-файл

   ? ProcNL(), ALIAS(), oUse:cAlias, Used()
   IF ! oUse:lOpen
      RETURN NIL
   ENDIF
   ?? DBINFO( DBI_FULLPATH )

   // этот код выполняется уже в окне - TsbViewer.prg
   // this code is already executed in the window - TsbViewer.prg
   bInitForm  := {|ow,ob|
                  Local oc, cw, i, cv, ns := 0
                  cw := ow:Name
                  ? ProcNL()
                  ?? "===>>> bInitForm:", cw, ob:cControlName
                  oc := ob:GetColumn("ORDKEYNO")
                  oc:nAlign  := DT_RIGHT
                  oc:nFAlign := oc:nAlign
                  oc:nSAlign := oc:nAlign
                  oc:cSpcHeading += Space( ob:nCellMarginLR )
                  FOR EACH oc IN ob:aColumns
                     //? "      ...",hb_enumIndex(oc), oc:cName, oc:lEdit, oc:lCheckBox
                  NEXT
                  //
                  FOR i := 1 TO ob:nLen
                     ob:GotoRec(i)
                     ns += ob:GetValue("TAXRATE")
                  NEXT
                  ob:GoTop()
                  cv := "Amount by field [TAXRATE] = " + HB_NtoS(ns)
                  // "LblDown" - взять из oWin:aDown
                  ob:GotoRec(20)
                  cv += SPACE(10) + "Jump to record 20  /  oBrw:GotoRec(20) "
                  IF HB_IsArray(oWin:aDown)
                     IF LEN(oWin:aDown) >=6
                        cv += oWin:aDown[6]
                     ENDIF
                  ENDIF
                  IF HB_IsArray(ow:Cargo:aCurrLang)
                     cv += " [восстановим язык "
                     cv += ow:Cargo:aCurrLang[3]+"]"
                  ENDIF
                  IF GetControlIndex("LblDown", cw ) > 0
                     SetProperty(cw, "LblDown", "Value", cv)
                  ENDIF
                  Return Nil
                 }

   cMsg := "hb_SetCodepage()= " + hb_SetCodepage() + ";"
   cMsg += "hb_CdpSelect()  = " + hb_CdpSelect() + ";"
   cMsg += "hb_LangSelect() = " + hb_LangSelect() + ";"
   //cMsg += ";" + ProcNL()
   AlertInfo(cMsg)
   ? ProcNL(), cMsg

   // закрыть окно ожидания с потоком
   WaitThreadAviClose( aThrd )
   // окно с таблицей - показ окна зависит от описания в oWin
   TsbObjViewer(oWin, oUse, oIndx, oMenu, oTsb, aEvent, bInitForm)
   // исходники смотреть в \MiniGUI\SAMPLES\Advanced\Tsb_Viewer

   IF SELECT(oUse:cAlias) > 0
      (oUse:cAlias)->(dbCloseArea())  // закроем базу
   ENDIF
   ? ProcNL(), "--- END ------ Alias:", ALIAS()

   IF _IsWindowDefined( cWndMain )
      // поднять главное окно
      Domethod(cWndMain, "Restore")
   ENDIF
   DO EVENTS

   SET LANGUAGE TO RUSSIAN
   SET CODEPAGE TO RUSSIAN

RETURN Nil

///////////////////////////////////////////////////////////////////////////////
FUNCTION my3Btn1(ow,ky,cn,ob)

   MsgDebug(ow:Name,ky,cn,This.&(cn).caption)

   IF ky == 1         // кнопка 1
      myLoadTableCnf(ow)
   ELSEIF ky == 2     // кнопка 2
      myTable4menu(ow,ky,cn,ob)
   ELSEIF ky == 3     // кнопка 3
      myGetLang("Какой язык в окне ?","SAY")  // TsbViewMenu.prg
   ENDIF

   ow:Enabler(cn, .T.)
   ob:Setfocus()
   DO EVENTS

RETURN Nil

///////////////////////////////////////////////////////////////////////////////
FUNCTION CnfgDataWin(oIni, cSection)
   LOCAL oWin, aColor, aVal, nI

   oWin := oHmgData()
   oWin:cIcon      := GetIniData( oIni, cSection, "Win_cIcon"  , ""  )       ; ? "   Win_cIcon", oWin:cIcon
   oWin:lWait      := GetIniData( oIni, cSection, "Win_lWait"  , .T. ) // .T.-"WAIT", .F.="NOWAIT"
   oWin:lCenter    := GetIniData( oIni, cSection, "Win_lCenter", .F. )
   oWin:nPosY      := GetIniData( oIni, cSection, "Win_nPosY"  , 20, .t. )   ; ? "   Win_nPosY", oWin:nPosY
   oWin:nPosX      := GetIniData( oIni, cSection, "Win_nPosX"  , 20, .t. )   ; ? "   Win_nPosX", oWin:nPosX
   oWin:nPosW      := GetIniData( oIni, cSection, "Win_nPosW"  , -1  )
   oWin:nPosH      := GetIniData( oIni, cSection, "Win_nPosH"  , -1  )
   IF oWin:nPosW < 0
      oWin:nPosW   := App.Cargo:aDisplayMode[1]  //System.ClientWidth
   ENDIF
   IF oWin:nPosH < 0
      oWin:nPosH   := App.Cargo:aDisplayMode[2]  //System.ClientHeight
      oWin:nPosH   -= GetTaskBarHeight()         // высота Панели задач Desktop
   ENDIF

   oWin:aBackcolor := GetIniData( oIni, cSection, "Win_aBackcolor", RED       , .T. )
   oWin:cTitle     := GetIniData( oIni, cSection, "Win_cTitle"    , "no-Title", .T. )
   oWin:lTopmost   := GetIniData( oIni, cSection, "Win_lTopmost"  , .F.             ) // This.Topmost := lTopmost, если .T. то переключиться на другие окна будет нельзя
   oWin:bOnInit    := Nil      // по умолчанию берется в TsbViewer.prg
   oWin:bOnRelease := {||Nil}  // по умолчанию берется в TsbViewer.prg
   oWin:bIAClose   := {||Nil}  // по умолчанию берется в TsbViewer.prg
   oWin:aDown      := {}       // нет label внизу окна
   // есть label внизу окна {имя-label, высота, цвет фона, цвет текста, центровка, сам текст}
   aVal            := { "LblDown", GetTitleHeight()+1, WHITE, {42,97,181}, .T., "! здесь можно что-то написать ...." }
   oWin:aDown      := GetIniData( oIni, cSection, "Win_aDown", aVal )
   // замена функций в массиве oWin:aDown
   IF HB_IsArray(oWin:aDown)
      aVal := oWin:aDown
      FOR nI := 1 TO LEN(aVal)
         aVal[nI] := myStrFuncValue(aVal[nI])
      NEXT
      oWin:aDown := aVal
   ENDIF

   // смена цвета фона нижней строки [4] для Win10
   oWin:lClrDown4  := GetIniData( oIni, cSection, "Win_lClrDown4", .F. )
   IF oWin:lClrDown4
      aVal       := oWin:aDown
      aColor     := aVal[4]
      aVal[4]    := IIF( IsWin10OrLater(), Color10_ActiveCaption(), aColor )
      oWin:aDown := aVal
   ENDIF

RETURN oWin

///////////////////////////////////////////////////////////////////////////////
FUNCTION CnfgDataDbf(oIni, cSection, cPath, cAlsNew)
   LOCAL oUse, cMsg, oError, cOld, cAls, cCodePage, lShared, cUse, oSec
   DEFAULT cAlsNew := ""

   oUse := oHmgData()
   IF Empty( oSec := oIni:Get(cSection) )    // нет секции
      cMsg := "Нет секции [" + cSection + "] в Demo_timer.cnfg !"
      cMsg += ";;" + ProcNL() + ";" + ProcNL(1)
      AlertStop(cMsg)
      oUse:cFile  := "none-section.dbf"
      oUse:cError := AtRepl( ";", cMsg, CRLF )
      oUse:cPath  := cPath
      oUse:cAlias := ""
      RETURN oUse
   ENDIF

   oSec := oIni:Get(cSection)
   //_o2Log(oSec,  20, "==> .T. =========["+cSection+"]======== oSec: ", .T.)
   ? ProcNL(), "=========[" + cSection + "]========"
   oUse:cFile     := oSec:Get("Dbf_File"    , "none.dbf" )  ;  ? "   Dbf_File     =", oUse:cFile
   oUse:cAlias    := oSec:Get("Dbf_Alias"   , "TEMP"     )  ;  ? "   Dbf_Alias    =", oUse:cAlias
   oUse:cCodePage := oSec:Get("Dbf_CodePage", "RU866"    )  ;  ? "   Dbf_CodePage =", oUse:cCodePage
   oUse:lShared   := oSec:Get("Dbf_Shared"  , .F.        )  ;  ? "   Dbf_Shared   =", oUse:lShared
   IF LEN(cAlsNew) > 0
      oUse:cAlias := cAlsNew  // первостепенный алиас
   ELSE
      // алиас берем из Demo_timer.cfg
   ENDIF
   // или можно так
   //oUse:cFile     := GetIniData( oIni, cSection, "Dbf_File"    , "none.dbf" ,.T.)
   //oUse:cAlias    := GetIniData( oIni, cSection, "Dbf_Alias"   , "TEMP"     ,.T.)
   //oUse:cCodePage := GetIniData( oIni, cSection, "Dbf_CodePage", "RU866"    ,.T.)
   //oUse:lShared   := GetIniData( oIni, cSection, "Dbf_Shared"  , .T.        ,.T.)

   oUse:cError    := ""
   oUse:cPath     := cPath
   oUse:cFullPath := cPath + oUse:cFile
   cUse           := cPath + oUse:cFile
   cAls           := oUse:cAlias
   cCodePage      := oUse:cCodePage
   lShared        := oUse:lShared

   BEGIN SEQUENCE  WITH { |e|break( e ) }
      cOld := hb_cdpSelect(cCodePage)
      IF hb_cdpSelect() == cCodePage
         // есть такая кодовая страница
         // there is such a code page
      ENDIF
      hb_cdpSelect(cOld)
   RECOVER USING oError
      cMsg := "Code page error!;"
      cMsg += "No driver for CodePage: "
      cMsg += cCodePage + ";" + ProcNL()
      AlertStop( cMsg, "ERROR")
      oUse:cError := cMsg
      oUse:cAlias := ""
      oUse:lOpen  := .F.
      RETURN oUse
   END SEQUENCE

   IF hb_FileExists( cUse )

      BEGIN SEQUENCE  WITH { |e|break( e ) }

         IF lShared
            USE (cUse) ALIAS ( cAls ) CODEPAGE cCodePage SHARED NEW
         ELSE
            USE (cUse) ALIAS ( cAls ) CODEPAGE cCodePage EXCLUSIVE NEW
         ENDIF
         oUse:lOpen  := .T.

      RECOVER USING oError

         cMsg := "Error opening Database!;"
         cMsg += "The Database is occupied by another process;"
         cMsg += cUse + ";" + ProcNL()
         AlertStop( cMsg, "ERROR")
         oUse:cError := cMsg
         oUse:cAlias := ""
         oUse:lOpen  := .F.

      END SEQUENCE

   ELSE

      cMsg := 'File not found !;' + cUse + ";" + ProcNL()
      //AlertStop(cMsg, "ERROR")
      oUse:cAlias := ""
      oUse:lOpen  := .F.
      oUse:cError := cMsg

   ENDIF

RETURN oUse

///////////////////////////////////////////////////////////////////////////////
FUNCTION Tsb3DataIndex(oUse,cPathTemp)                    // создать индексы
   LOCAL oIndx, cIndex, cAls, cMsg, nI, cOn, cFor, cTag

   oIndx  := oHmgData()
   cIndex := ChangeFileExt( oUse:cFullPath, '.cdx' )
   cIndex := cPathTemp + cFileNoPath( cIndex )
   DeleteFile(cIndex)   // обязательно удалить
   cAls   := oUse:cAlias

   oIndx:cIndex    := cIndex
   oIndx:aFor      := { ""         , "!Deleted()"   , "Deleted()"    , "CUSTNO>0"    }
   oIndx:aTag      := { "PRINT"    , "NO_DEL"       , "DEL"          , "Except_zero" }
   oIndx:aIndxOn   := { "CUSTNO"   , "CUSTNO"       , "CUSTNO"       , "CUSTNO"      }
   oIndx:cError    := ""
   oIndx:nSetOrder := 0

   IF LEN(cAls) > 0  // если база открыта

      IF !hb_DirExists( cPathTemp )
         cMsg := "Couldn't create indexes !; There is no such path for files - "
         cMsg += cPathTemp + ";" + ProcNL()
         //AlertStop(cMsg, "ERROR")
         oIndx:cError  := cMsg
      ELSE
         dbSelectArea( cAls )
         FOR nI := 1 TO LEN(oIndx:aFor)
            cOn  := oIndx:aIndxOn[nI]
            cTag := oIndx:aTag[nI]
            cFor := oIndx:aFor[nI]
            IF LEN(cFor) == 0
               INDEX ON &cOn TAG (cTag) TO (cIndex) DESCENDING
            ELSE
               INDEX ON &cOn TAG (cTag) TO (cIndex) FOR &cFor DESCENDING
            ENDIF
         NEXT
         oIndx:nSetOrder := 1  // или так DbSetOrder(1)
      ENDIF

   ELSE
      cMsg := "Couldn't create indexes !; Database is not open!;"  + ProcNL()
      //AlertStop(cMsg, "ERROR")
      oIndx:cError  := cMsg
   ENDIF

RETURN oIndx

///////////////////////////////////////////////////////////////////////////////
FUNCTION CnfgDataMenu(oIni, cSection)             // меню-кнопки окна
   LOCAL oMenu, nWMenu, nKolvo, a5Fnt, cMsg, nI, aVal, cVal, xVal

   oMenu := oHmgData()
   oMenu:lDebug    := GetIniData( oIni, cSection, "Menu_lDebug"   , .T. )       // отладка, показ ошибок
   oMenu:nPosWin   := GetIniData( oIni, cSection, "Menu_nPosWin"  , 1   )       // 1-TopWindow, 2-BottomWindow, 3-LeftWindow, 4-RightWindow
   oMenu:nHAlign   := GetIniData( oIni, cSection, "Menu_nHAlign"  , 0   )       // горизонтальные кнопки: 0-LEFT, 1-CENTER, 2-RIGHT
   oMenu:nVAlign   := GetIniData( oIni, cSection, "Menu_nVAlign"  , 0   )       // вертикальные кнопки: 0-TOP , 1-CENTER, 2-BOTTOM
   oMenu:aCaption  := GetIniData( oIni, cSection, "Menu_aCaption" , {}  )       // кнопки меню
   oMenu:aBtnPost  := GetIniData( oIni, cSection, "Menu_aBtnPost" , {}  )       // _wPost(Х) - номер события на кнопке
   oMenu:aBColor   := GetIniData( oIni, cSection, "Menu_aBColor"  , {}  )       // кнопки цвет фона
   oMenu:lBtnIco   := GetIniData( oIni, cSection, "Menu_lBtnIco"  , .T. )       // F-кнопки без иконок
   oMenu:aIcon     := GetIniFor ( oIni, cSection, "Menu_Icon_"    , {}  , .F.)  // массив иконок
   oMenu:nIcoSize  := GetIniData( oIni, cSection, "Menu_nIcoSize" , 48  )       // уменьшить иконки до 48х48
   oMenu:lTextVert := GetIniData( oIni, cSection, "Menu_lTextVert", .F. )       //.T.-вертикальный текст для кнопок
   oMenu:lTextLeft := GetIniData( oIni, cSection, "Menu_lTextLeft", .T. )       //.F.-слева текст для кнопок
   a5Fnt           := { "Comic Sans MS", 15, .T., .F. , 17, "увеличение фонта кнопки" }
   oMenu:aFont     := GetIniData( oIni, cSection, "Menu_aFont"   , a5Fnt          )   // фонт на кнопках
   oMenu:aFClr     := GetIniData( oIni, cSection, "Menu_aFClr"   , {BLACK,YELLOW} )   // цвет фонта на кнопках
   oMenu:aHelp     := GetIniData( oIni, cSection, "Menu_aHelp"   , {}             )   // Tooltip кнопки
   oMenu:nIndent   := GetIniData( oIni, cSection, "Menu_nIndent" , 0              )   // отступ первой кнопки  - резерв
   oMenu:nHBtn     := GetIniData( oIni, cSection, "Menu_nHBtn"   , 54             )   // высота кнопки
   oMenu:nWBtn     := GetIniData( oIni, cSection, "Menu_nWBtn"   , 220            )   // ширина кнопки
   oMenu:nGaps     := GetIniData( oIni, cSection, "Menu_nGaps"   , 5              )   // отступ кнопки от края окна
   oMenu:nGapsBtn  := GetIniData( oIni, cSection, "Menu_nGapsBtn", 15             )   // между кнопками по ширине/высоте
   oMenu:aPost     := oMenu:aBtnPost // для событий по кнопкам верхнего меню AAdd( aEvent, { aPost[1]....

   // замена знака на кнопках
   aVal := oMenu:aCaption
   FOR nI := 1 TO LEN(aVal)
       aVal[nI] := AtRepl( "|", aVal[nI], ";" )
   NEXT

   // замена цвета на кнопках
   cVal := ""
   aVal := oMenu:aBColor
   FOR nI := 1 TO LEN(aVal)
      xVal := aVal[nI]
      IF HB_IsArray(xVal)
      ELSEIF HB_IsChar(xVal)
         IF AT("()", xVal ) > 0
            aVal[nI] := myStrFuncValue(aVal[nI])
         ENDIF
      ELSEIF HB_IsNumeric(xVal)
      ENDIF
      IF !HB_IsArray(aVal[nI])
         cVal += "oMenu:aBColor[" + HB_NtoS(nI) + "] = "
         cVal += cValToChar(xVal) +  ";"
      ENDIF
   NEXT
   oMenu:aBColor := aVal
   IF LEN(cVal) > 0
      cMsg := "ERROR !; Массив должен быть равен {XXX,XXX,XXX} !;"
      cMsg += cVal + ";;" + ProcNL() + ";" + ProcNL(1)
      AlertStop(cMsg)
   ENDIF

   // проверим массивы для кнопок
   IF LEN(oMenu:aCaption) # LEN(oMenu:aBtnPost) .AND. LEN(oMenu:aCaption) # LEN(oMenu:aBColor) .AND. ;
      LEN(oMenu:aCaption) # LEN(oMenu:aIcon)
      cMsg := "ERROR !; Массивы кнопок НЕ РАВНЫ !"
      cMsg += '(' + HB_NtoS(LEN(oMenu:aCaption)) + ') oMenu:aCaption - "Menu_aCaption" - кнопки текста меню;'
      cMsg += '(' + HB_NtoS(LEN(oMenu:aBtnPost)) + ') oMenu:aBtnPost - "Menu_aBtnPost" - номер события на кнопке;'
      cMsg += '(' + HB_NtoS(LEN(oMenu:aBColor )) + ') oMenu:aBColor  - "Menu_aBColor"  - кнопки цвета фона;'
      cMsg += '(' + HB_NtoS(LEN(oMenu:aIcon   )) + ') oMenu:aIcon    - "Menu_Icon_"    - массив иконок;'
      cMsg +=  ";" + ProcNL() + ";" + ProcNL(1)
      AlertStop(cMsg)
   ENDIF

   // проверим ширину всех кнопок
   nKolvo := LEN(oMenu:aCaption)
   nWMenu := oMenu:nGaps * 2 + oMenu:nWBtn * nKolvo + oMenu:nGapsBtn * nKolvo
   IF nWMenu > App.Cargo:aDisplayMode[1]  //System.ClientWidth
      oMenu:nWBtn := ( App.Cargo:aDisplayMode[1] - oMenu:nGaps*2 - oMenu:nGapsBtn * nKolvo ) / nKolvo
   ENDIF

   IF oMenu:nPosWin == 1 .OR. oMenu:nPosWin == 2
      // для 1-TopWindow, 2-BottomWindow
      oMenu:nHMenu   := oMenu:nHBtn + oMenu:nGaps * 2      // высота всего меню
   ELSE
      // для  3-LeftWindow, 4-RightWindow
      oMenu:nHMenu   := oMenu:nWBtn + oMenu:nGaps * 2      // ширина всего меню
   ENDIF

RETURN oMenu

///////////////////////////////////////////////////////////////////////////////
FUNCTION CnfgDataTsb(oIni, cSection, oUse, oWin)      // параметры ТСБ
   LOCAL aHead, aFSize, aFoot, aPict, aAlign, aName, aField, aFAlign, nAlgn
   LOCAL aDbf, nJ, nK, aEdit, cErr, cFld, cVal, cAls, cMsg, oTsb, cTmp, cTyp
   LOCAL aBColor, nBColor, nClr, nGrad, cRun, bClrDef, aClrDef

   oTsb   := CreateDateTsb(oUse,oUse:cCodePage,"Checkpoint (1) !",oWin)  // параметры ТСБ

   aBColor := oWin:aBackcolor
   nBColor := RGB( aBColor[1], aBColor[2], aBColor[3] )

   oTsb := oHmgData()
   oTsb:nWGaps       := GetBorderWidth()        // отступ по ширине формы
   oTsb:nHGaps       := GetBorderHeight()       // отступ по высоте формы
   oTsb:cError       := ""
   // настройки таблицы
   oTsb:lSelector    := GetIniData( oIni, cSection, "Tsb_lSelector"   , .T.        )  // F-убрать в таблице вирт.колонку SELECTOR
   oTsb:lColNumber   := GetIniData( oIni, cSection, "Tsb_lColNumber"  , .T.        )  // F-убрать в таблице вирт.колонку ORDKEYNO
   oTsb:aColNumber   := GetIniData( oIni, cSection, "Tsb_aColNumber"  , { 1, 60 }  )  // вирт.колонка с номерами - потом автоматом расчитывается ширина колонки
   oTsb:lSuperHead   := GetIniData( oIni, cSection, "Tsb_lSuperHead"  , .T.        )  // F-убрать в таблице суперхидер
   oTsb:lSpecHd      := GetIniData( oIni, cSection, "Tsb_lSpecHd"     , .T.        )  // F-убрать в таблице нумератор
   oTsb:cSupHd1Title := GetIniData( oIni, cSection, "Tsb_cSuperHead1" , oUse:cCodePage     )
   oTsb:cSupHd2Title := GetIniData( oIni, cSection, "Tsb_cSuperHead2" , "Checkpoint (1) !" )

   ? "//////////// цвета таблицы /////", ProcNL()
   oTsb:lShowZebra   := GetIniData( oIni, cSection, "Tsb_lShowZebra", .F. )  // показ чётная\нечётная строка
   ? SPACE(3)+"oTsb:lShowZebra =", oTsb:lShowZebra, "показ чётная\нечётная строка"

   bClrDef        := {|| myRGB('CLR_WHITE') }
   cRun           := GetIniData( oIni, cSection, "Tsb_nClr22Bck", bClrDef )  // цвет фона чётная\нечётная строка при Tsb_lShowZebra = .T.
   oTsb:nClr22Bck := EVal(cRun)
   ? SPACE(3)+"oTsb:nClr22Bck =", cRun, oTsb:nClr22Bck, HB_ValToExp(n2RGB(oTsb:nClr22Bck)), "цвет фона чётная\нечётная строка при Tsb_lShowZebra = .T."

   oTsb:aBrush    := GetIniData( oIni, cSection, "Tsb_aBrush"   , {60,60,60} ) // под таблицей - цвет фона окна
   ? SPACE(3)+"oTsb:aBrush =", HB_ValToExp( oTsb:aBrush )

   bClrDef        := {|| myRGB({128,128,128},.T.) }
   cRun           := GetIniData( oIni, cSection, "Tsb_nClrNoDbf", bClrDef ) // селектора/нумератора/вирт.колонки
   oTsb:nClrNoDbf := EVal(cRun) //&cRun
   ? SPACE(3)+"oTsb:nClrNoDbf =", cRun, oTsb:nClrNoDbf, HB_ValToExp(n2RGB(oTsb:nClrNoDbf))

   bClrDef         := {|| myRGB({255,0,0},.T.) }
   cRun            := GetIniData( oIni, cSection, "Tsb_nClrNoEdit", bClrDef ) // шапка/подвал колонок типа "+=^"
   oTsb:nClrNoEdit := EVal(cRun) //&cRun
   ? SPACE(3)+"oTsb:nClrNoEdit =", cRun, oTsb:nClrNoEdit, HB_ValToExp(n2RGB(oTsb:nClrNoEdit))

   bClrDef          := {|| myRGB({50,50,50},.T.) }
   cRun             := GetIniData( oIni, cSection, "Tsb_nClrBackDel", bClrDef ) // фона удалённых записей
   oTsb:nClrBackDel := EVal(cRun) //&cRun
   ? SPACE(3)+"oTsb:nClrBackDel =", cRun, oTsb:nClrBackDel, HB_ValToExp(n2RGB(oTsb:nClrBackDel))

   bClrDef          := {|| myRGB('CLR_GRAY') }
   cRun             := GetIniData( oIni, cSection, "Tsb_nClrForeDel", bClrDef ) // текст удалённых записей
   oTsb:nClrForeDel := EVal(cRun) //&cRun
   ? SPACE(3)+"oTsb:nClrForeDel =", cRun, oTsb:nClrForeDel, HB_ValToExp(n2RGB(oTsb:nClrForeDel))

   bClrDef         := {|| myRGB('CLR_BLUE') }
   cRun            := GetIniData( oIni, cSection, "Tsb_nClr1Fore", bClrDef )  // 1 , текст в ячейках таблицы
   oTsb:nClr1Fore  := EVal(cRun)
   ? SPACE(3)+"oTsb:nClr1Fore =", cRun, oTsb:nClr1Fore, HB_ValToExp(n2RGB(oTsb:nClr1Fore))

   bClrDef         := {|| myRGB('CLR_WHITE') }
   cRun            := GetIniData( oIni, cSection, "Tsb_nClr2Back", bClrDef )  // 2 , фон   в ячейках таблицы
   oTsb:nClr2Back  := EVal(cRun)
   ? SPACE(3)+"oTsb:nClr2Back =", cRun, oTsb:nClr2Back, HB_ValToExp(n2RGB(oTsb:nClr2Back))

   bClrDef         := {|| myRGB('CLR_YELLOW') }
   cRun            := GetIniData( oIni, cSection, "Tsb_nClr3Fore", bClrDef )  // 3 , текста шапки таблицы
   oTsb:nClr3Fore  := EVal(cRun)
   ? SPACE(3)+"oTsb:nClr3Fore =", cRun, oTsb:nClr3Fore, HB_ValToExp(n2RGB(oTsb:nClr3Fore))

   ? SPACE(3)+"oTsb:nClr4Back = { 1N, 2N } "
   nClr            := RGB(40,122,237)
   nGrad           := RGB(48,29,26)
   ?? "{",nClr,",",nGrad, "}"
   aClrDef         := { nClr , nGrad  }
   oTsb:nClr4Back  := GetIniData( oIni, cSection, "Tsb_nClr4Back", aClrDef )  // 4 , фона шапки таблицы
   ?? HB_ValToExp(oTsb:nClr4Back)

   bClrDef         := {|| myRGB('CLR_YELLOW') }
   cRun            := GetIniData( oIni, cSection, "Tsb_nClr9Fore", bClrDef )  // 9 , текста подвала таблицы
   oTsb:nClr9Fore  := EVal(cRun)
   ? SPACE(3)+"oTsb:nClr9Fore =", cRun, oTsb:nClr9Fore, HB_ValToExp(n2RGB(oTsb:nClr9Fore))

   ? SPACE(3)+"oTsb:nClr10Back = { 1N, 2N } "
   nClr            := RGB(40,122,237)
   nGrad           := RGB(48,29,26)
   ?? "{",nClr,",",nGrad, "}"
   aClrDef         := { nClr , nGrad  }
   oTsb:nClr10Back  := GetIniData( oIni, cSection, "Tsb_nClr10Back", aClrDef )  // 10, фона подвала таблицы
   ?? HB_ValToExp(oTsb:nClr10Back)

   bClrDef         := {|| myRGB('CLR_WHITE') }
   cRun            := GetIniData( oIni, cSection, "Tsb_nClr17Fore", bClrDef )   // 17, текста суперхидера
   oTsb:nClr17Fore := EVal(cRun)
   ? SPACE(3)+"oTsb:nClr17Fore =", cRun, oTsb:nClr17Fore, HB_ValToExp(n2RGB(oTsb:nClr17Fore))

   ? SPACE(3)+"oTsb:nClr16Back = { 1N, 2N } "
   nClr            := RGB(40,122,237)
   nGrad           := RGB(48,29,26)
   ?? "{",nClr,",",nGrad, "}"
   aClrDef         := { nClr , nGrad  }
   oTsb:nClr16Back  := GetIniData( oIni, cSection, "Tsb_nClr16Back", aClrDef )  // 16, фона суперхидера
   ?? HB_ValToExp(oTsb:nClr16Back)

   bClrDef          := {|| myRGB('CLR_RED') }
   cRun             := GetIniData( oIni, cSection, "Tsb_n1Clr17Fore", bClrDef )   // 1.17, текста суперхидера  колонка 1
   oTsb:n1Clr17Fore := EVal(cRun)
   ? SPACE(3)+"oTsb:n1Clr17Fore =", cRun, oTsb:n1Clr17Fore, HB_ValToExp(n2RGB(oTsb:n1Clr17Fore))

   ? SPACE(3)+"oTsb:n1Clr16Back = { 1N, 2N } "
   nClr             := RGB(247,172,8)
   nGrad            := RGB(86,211,83)
   ?? "{",nClr,",",nGrad, "}"
   aClrDef          := { nClr , nGrad  }
   oTsb:n1Clr16Back := GetIniData( oIni, cSection, "Tsb_n1Clr16Back", aClrDef )  // 1.16, фона суперхидера колонка 1
   ?? HB_ValToExp(oTsb:n1Clr16Back)

   ? "// ----- колонка 1-2 -------"
   ? SPACE(3)+"oTsb:n12Clr4Back = { 1N, 2N } TEST={" , CLR_GREEN, CLR_YELLOW, "} ,"
   nClr             := GetSysColor( COLOR_BTNFACE )     // селектора/нумератора/вирт.колонки
   nGrad            := GetSysColor( COLOR_BTNFACE )     // селектора/нумератора/вирт.колонки
   aClrDef          := { nClr , nGrad  }
   ?? HB_ValToExp(aClrDef)
   oTsb:n12Clr4Back := GetIniData( oIni, cSection, "Tsb_n12Clr4Back", aClrDef )  //  4 , фона шапки таблицы колонка 1-2
   ?? HB_ValToExp(oTsb:n12Clr4Back)

   ? SPACE(3)+"oTsb:n12Clr10Back = { 1N, 2N } "
   oTsb:n12Clr10Back := GetIniData( oIni, cSection, "Tsb_n12Clr10Back", aClrDef )  //  10, фона подвала таблицы колонка 1-2
   ?? HB_ValToExp(oTsb:n12Clr10Back)

   bClrDef          := {|| myRGB('CLR_RED') }
   cRun             := GetIniData( oIni, cSection, "Tsb_n12Clr3Fore", bClrDef )   // 3 , текста шапки таблицы колонка 1-2
   oTsb:n12Clr3Fore := EVal(cRun)
   ? SPACE(3)+"oTsb:n12Clr3Fore =", cRun, oTsb:n12Clr3Fore, HB_ValToExp(n2RGB(oTsb:n12Clr3Fore))

   cRun             := GetIniData( oIni, cSection, "Tsb_n12Clr9Fore", bClrDef )   // 9 , текста подвала таблицы колонка 1-2
   oTsb:n12Clr9Fore := EVal(cRun)
   ? SPACE(3)+"oTsb:n12Clr9Fore =", cRun, oTsb:n12Clr9Fore, HB_ValToExp(n2RGB(oTsb:n12Clr9Fore))

   // цвета курсора
   bClrDef          := {|| myRGB({5,5,5},.T.) * -1 }          // черная окантовка
   cRun             := GetIniData( oIni, cSection, "Tsb_nClrFocus1", bClrDef )
   oTsb:nClrFocus1  := EVal(cRun)
   ? SPACE(3)+"oTsb:nClrFocus1 =", cRun, oTsb:nClrFocus1, HB_ValToExp(n2RGB(oTsb:nClrFocus1))

   bClrDef          := {|| myRGB("CLR_HRED") * -1 }          // красная окантовка
   cRun             := GetIniData( oIni, cSection, "Tsb_nClrFocus2", bClrDef )
   oTsb:nClrFocus2  := EVal(cRun)
   ? SPACE(3)+"oTsb:nClrFocus2 =", cRun, oTsb:nClrFocus2, HB_ValToExp(n2RGB(oTsb:nClrFocus2))

   bClrDef          := {|| myRGB("CLR_SKYPE") * -1 }          // окантовка вне фокуса
   cRun             := GetIniData( oIni, cSection, "Tsb_nClrNoFocus1", bClrDef )
   oTsb:nClrNoFocus1:= EVal(cRun)
   ? SPACE(3)+"oTsb:nClrNoFocus1 =", cRun, oTsb:nClrNoFocus1, HB_ValToExp(n2RGB(oTsb:nClrNoFocus1))

   bClrDef          := {|| myRGB({128, 225, 225},.T.) * -1 }   // окантовка вне фокуса
   cRun             := GetIniData( oIni, cSection, "Tsb_nClrNoFocus2", bClrDef )
   oTsb:nClrNoFocus2:= EVal(cRun)
   ? SPACE(3)+"oTsb:nClrNoFocus2 =", cRun, oTsb:nClrNoFocus2, HB_ValToExp(n2RGB(oTsb:nClrNoFocus2))

   //oTsb:nClrSeleF   := GetSysColor( COLOR_WINDOWTEXT )
   bClrDef          := {|| myRGB("COLOR_WINDOWTEXT",.T.) }
   cRun             := GetIniData( oIni, cSection, "Tsb_nClrSeleF", bClrDef )   //  цвет текста ячейки таблицы вне фокуса
   oTsb:nClrSeleF   := EVal(cRun)
   ? SPACE(3)+"oTsb:nClrSeleF =", cRun, oTsb:nClrSeleF, HB_ValToExp(n2RGB(oTsb:nClrSeleF)), myRGB("COLOR_WINDOWTEXT",.T.)

   ? SPACE(3)+"oTsb:nClrSelectorHdBack = { 1N, 2N } TEST={" , CLR_GREEN, ",", CLR_GREEN, "} ,"
   nClr             := GetSysColor( COLOR_BTNFACE )     // селектора/нумератора/вирт.колонки
   nGrad            := GetSysColor( COLOR_BTNFACE )     // селектора/нумератора/вирт.колонки
   aClrDef          := { nClr , nGrad  }
   ?? HB_ValToExp(aClrDef)
   // цвет фона шапки/подвала таблицы колонка 1 - Selector
   oTsb:nClrSelectorHdBack := GetIniData( oIni, cSection, "Tsb_nClrSelectorHdBack", aClrDef )
   ?? HB_ValToExp(oTsb:nClrSelectorHdBack)
   //
   // работа с колонками  - цифры это кол-во знаков, а не пикселей
   //oTsb:aWidthCol  := { {"LOGPRN", -3}, {"CUSTNO", -5}, {"FAX", +2}, {"TAXRATE", -4} }
   oTsb:aWidthCol    := GetIniData( oIni, cSection, "Tsb_aWidthCol"  , {}  )  // добавить колонками  - цифры это кол-во знаков, а не пикселей

   cAls := oUse:cAlias
   cErr := ""
   /*AADD( aDbf, { "LOGPRN"     , "L",  1, 0, "Print;recno"         , .T. } )
   AADD( aDbf, { "CUSTNO"     , "N", 15, 0, "Company;number"      , .T. } )
   AADD( aDbf, { "COMPANY"    , "C", 30, 0, "Company"             , .T. } )
   AADD( aDbf, { "ADDR1"      , "C", 30, 0, "Adres-1"             , .T. } )
   AADD( aDbf, { "ADDR2"      , "C", 30, 0, "not-show"            , .T. } )
   AADD( aDbf, { "CITY"       , "C", 15, 0, "City"                , .T. } )
   AADD( aDbf, { "STATE"      , "C", 20, 0, "State"               , .T. } )
   AADD( aDbf, { "ZIP"        , "C", 10, 0, "Zip"                 , .T. } )
   AADD( aDbf, { "COUNTRY"    , "C", 20, 0, "Country"             , .T. } )
   AADD( aDbf, { "PHONE"      , "C", 15, 0, "Phone;company"       , .T. } )
   AADD( aDbf, { "FAX"        , "C", 15, 0, "Fax;company"         , .T. } )
   AADD( aDbf, { "TAXRATE"    , "N", 19, 4, "Taxrate"             , .T. } )
   AADD( aDbf, { "CONTACT"    , "C", 20, 0, "Contact"             , .T. } )
   AADD( aDbf, { "LASTINVOIC" , "C", 30, 0, "LAST INVOIC"         , .F. } )
   AADD( aDbf, { "LASTINVOIC" , "C", 30, 0, "not-show"            , .T. } ) */
   aDbf := GetIniFor( oIni, cSection, "Tsb_Column_" , {} )  // список полей таблицы

   nK      := LEN(aDbf)
   aHead   := {}  // список шапки колонок таблицы
   aFoot   := {}  // ширина колонок таблицы
   aPict   := {}  // список подвала колонок таблицы
   aName   := {}  // список PICTURE колонок таблицы
   aAlign  := {}  // список отбивки колонок таблицы
   aField  := {}  // список полей базы колонок таблицы
   aFSize  := {}  // список полей базы колонок таблицы
   aFAlign := {}  // список отбивки подвала колонок таблицы
   aEdit   := {}  // редактирование поля

   IF LEN(cAls) > 0  // если база открыта

      dbSelectArea( cAls )
      FOR nJ := 1 TO nK
         cFld := aDbf[nJ,1]
         cTyp := aDbf[nJ,2]
         cVal := aDbf[nJ,5]
         IF LOWER( cVal ) == "not-show"
            // пропуск
         ELSE
            IF FIELDNUM(cFld) == 0
               cVal := HB_ValToExp(aDbf[nJ])
               cVal := AtRepl( ";", cVal, "|" )
               cErr += HB_ValToExp(cVal) + ";"
            ELSE
               IF LEN(cVal) == 0
                  cTmp := cFld
               ELSE
                  cTmp := cVal
               ENDIF
               cTmp := AtRepl( "|", cTmp, ";" )
               AADD( aHead  , cTmp )
               AADD( aFoot  , "[ " + cFld + " ]" )
               AADD( aName  , cFld      )
               AADD( aField , cFld      )
               AADD( aFAlign, DT_CENTER )
               IF cTyp == 'C' .OR. cTyp == 'M'
                  nAlgn := DT_LEFT
               ELSEIF cTyp == 'N'
                  nAlgn := DT_RIGHT
               ELSE
                  nAlgn := DT_CENTER
               ENDIF
               AADD( aAlign , nAlgn )
               AADD( aEdit  , aDbf[nJ,6] )
            ENDIF
         ENDIF
      NEXT

      IF LEN(cErr) > 0
         cMsg := "No field in the database " + Alias() + " !;"
         cErr += ProcNL()
         //AlertStop( cMsg + cErr, "ERROR")
         oTsb:cError := cMsg + cErr
      ENDIF

      oTsb:aHead   := aHead
      oTsb:aFoot   := aFoot
      oTsb:aPict   := aPict
      oTsb:aName   := aName
      oTsb:aAlign  := aAlign
      oTsb:aField  := aField
      oTsb:aFSize  := aFSize
      oTsb:aFAlign := aFAlign
      oTsb:aEdit   := aEdit
      //oTsb:aEdit := .F.     // запрет правки всех ячеек, игнорирует aEdit

   ENDIF

RETURN oTsb


///////////////////////////////////////////////////////////////////////////////
FUNCTION myLoadTableCnf(oWnd)
   LOCAL cFile, cTxt, cFind, cSect

   ? ProcNL(), oWnd:Name

   cFile := ChangeFileExt( Application.ExeName, ".cfg" )
   cTxt  := HB_MemoRead(cFile)
   cFind := "[ТАБЛИЦЯ_БД_АБОНЕНТИ]"
   cSect := SUBSTR(cTxt, AT(cFind,UPPER(cTxt))-1 )
   cFind := ";////////////////"
   cSect := SUBSTR(cSect, 1, AT(cFind,cSect)-1 )
   cTxt  := AtRepl( ";", cSect, "|" )
   cFile := cFileNoPath(cFile)
   AlertInfo(cTxt, "Побудова ТАБЛИЦІ із файлу " + cFile)
   ? cTxt
   ? "-----------------"

RETURN NIL
