/*
 * MINIGUI - Harbour Win32 GUI library
 *
 * Copyright 2025 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Таблица журнала-событий-программы
 * Показ записей в таблице вида зебры по полю IDEVENT - сессия программы (экслюзив)
 * Program-event-log table
 * Display records in the zebra type table by the IDEVENT field - program session (exclusive)
*/
#define  _HMG_OUTLOG
#include "minigui.ch"
#include "tsbrowse.ch"
/////////////////////////////////////////////////////////////////////////
FUNCTION User2Tsb()
   LOCAL cAls

   SELECT User2Log
   cAls := ALIAS()
   OrdSetFocus("DATEIDEV")       // !!! ставим сразу здесь / we put it right here
   DbGotop()

   User2LogForm(cAls,.F.)        // см.ниже / see below

RETURN NIL

//////////////////////////////////////////////////////////////////////
FUNCTION User2LogForm(cAls,lModalWin)
   LOCAL oBrw, oTsb, nY, nX, nW, nH, nC, nLR, aXY, cForm
   LOCAL cFont, nFSize, aBColor, aTsbFont, nWTbl, nHTbl
   LOCAL cTitle, cIcon, c2Title, cSuperHd, cBrw, aClrTsb, owc
   LOCAL nHUp, nWBtn, nHIco, oTsbColumn, aIndexOpen, cTagCurr
   LOCAL bOnInit, bOnRele, bIClose, bOnGotFocus, nOrder
   DEFAULT lModalWin := .T.

   ? ProcNL(), cAls,lModalWin
   SET DATE FORMAT TO "DD.MM.YY"
   cForm    := "Form_2Log"
   nW       := App.Cargo:aDisplayMode[1] // Sys.ClientWidth
   nH       := App.Cargo:aDisplayMode[2] // Sys.ClientHeight
   nH       -= GetTaskBarHeight()        // высота Панели задач Desktop
   cIcon    := "iLogEvent48"
   aBColor  := {141,179,226}
   cFont    := "Arial"
   nFSize   := 12 //App.Cargo:nDefFontSize - 2  // 13

#ifdef KEY_ENG // for this project demo-en.hbp
   cTitle   := "Event log table"
   c2Title  := "Program event log"
   cSuperHd := "All log entries"
#else
   cTitle   := "Таблица журнала событий"
   c2Title  := "Журнал событий в программе"
   cSuperHd := "Все записи в журнале"
#endif

   aTsbFont := { "TsbNorm", "TsbBold", "TsbBold", "TsbSpecH", "TsbSuperH", "TsbEdit" }
   //            окно         aBrush      ---- зебра -----------        условия строк
   aClrTsb  := { aBColor , {179,207,242}, {116,172,242}, {214,230,249} , { {}, {} } }
   myTsbFont( .T., nFSize )                   // загрузить свои фонты для таблицы

   DbSelectArea(cAls)
   //Base_Tek()
   nOrder     := INDEXORD()
   cTagCurr   := (cAls)->( ordName(nOrder) )
   // записать в массив ВСЕ открытые индексные файлы БАЗЫ
   aIndexOpen := myIndexOpenSave()   //
   //MsgDebug(aIndexOpen,INDEXORD())
   //? ProcNL(), myGetIndexUse()

   SET FONT TO cFont, nFSize

   // определяем обработчики окна
   IF bOnInit == Nil ; bOnInit := {|| _wPost(0), iif(oBrw==Nil, Nil, oBrw:Setfocus()) }
   ENDIF
   IF bOnRele == Nil ; bOnRele := {|| _wPost(90)  }
   ENDIF
   IF bIClose == Nil ; bIClose := {|| Nil /*MG_YesNoQuit()*/ }
   ENDIF
   // возврат фокуса на форму
   IF bOnGotFocus == Nil ; bOnGotFocus := {|| App.Cargo:cFormGotFocus := This.Name, dbSelectArea(owc:cAlias) }
   ENDIF

   IF lModalWin
      DEFINE WINDOW &cForm At 0, 0 WIDTH nW HEIGHT nH        ;
         TITLE cTitle ICON cIcon                             ;
         MODAL NOSIZE                                        ;
         BACKCOLOR aBColor                                   ;
         ON INIT {|| _wPost(0) }
         //ON GOTFOCUS bOnGotFocus  // возврат фокуса на форму
         //ON RELEASE {|| myTsbFont() }  // выгрузить свои фонты для таблицы
   ELSE
      DEFINE WINDOW &cForm At 0, 0 WIDTH nW HEIGHT nH        ;
         TITLE cTitle ICON  cIcon                            ;
         WINDOWTYPE STANDARD NOMAXIMIZE NOSIZE TOPMOST       ;
         BACKCOLOR aBColor                                ;
         ON INIT {|| This.Topmost := .F. , _wPost(0)  }
         //ON GOTFOCUS bOnGotFocus                            // возврат фокуса на форму
         //ON RELEASE {|| myTsbFont() }
   ENDIF

      // установим обработчики окна
      //This.OnInit     := bOnInit
      This.OnRelease  := bOnRele
      This.OnInterActiveClose := bIClose
      //This.OnGotFocus := bOnGotFocus - так нельзя
      SetProperty( This.Name, "ONGOTFOCUS", bOnGotFocus )
      //_SetFormAction ( This.Name , bOnGotFocus , "ONGOTFOCUS" )   // или так
      //ThisWindow.OnGotFocus := bOnGotFocus                        // или так
      //
      This.Cargo := oHmgData() ; owc := This.Cargo
      owc:cAls := ALIAS()

      nW := This.ClientWidth           // ширина окна
      nH := This.ClientHeight          // высота окна

      // задать и вывести кнопки над таблицей
      nX    := nLR := 20
      nY    := nLR / 2
      aXY   := Draw_BtnEx_2Use( nY, nX, nLR/2, 0 )
      nHUp  := nY := aXY[1]
      nWBtn := aXY[2]
      nHIco := nHUp
      nC    := nW - nLR - nWBtn

      @ 0, 0 LABEL Buff VALUE "" WIDTH 5 HEIGHT 5 TRANSPARENT

      @ 0, nWBtn + nLR/2 LABEL Lbl_1 WIDTH nC HEIGHT nY VALUE c2Title ;
        FONTCOLOR NAVY FONT "Comic Sans MS" BOLD CENTERALIGN VCENTERALIGN TRANSPARENT
      myBigSizeLabel(ThisWindow.Name,"Lbl_1")

      //////////////////// таблица ///////////////////
      nX    := nLR
      nWTbl := nW - nLR*2
      nHTbl := nH - nY - nLR

      //@ nY, nX LABEL Lbl_Tsb WIDTH nWTbl HEIGHT nHTbl VALUE "ТАБЛИЦА" ;
      //  BACKCOLOR GRAY CENTERALIGN VCENTERALIGN
      //myBigSizeLabel(ThisWindow.Name,"Lbl_Tsb")
      //oTsb  := oBrw := NIL
      //
      oTsbColumn := ListColumnTsb(aTsbFont, nWTbl)   // список полей базы для таблицы и карточки
      cBrw  := "Tsb_" + cForm
      oTsb  := TsbPatam2Dbf( cForm, owc:cAls, cBrw, nWTbl, cSuperHd, aClrTsb, oTsbColumn, aTsbFont )
      // функция в библиотеке \MiniGUI\SOURCE\TsBrowse\h_controlmisc2.prg
      oBrw := _TBrowse( oTsb, owc:cAls, cBrw, nY, nX, nWTbl, nHTbl )
      //
      oBrw:uLastTag         := cTagCurr              // на всякий случай (без этого индекс слетает)
      oBrw:Cargo:nModify    := 0                     // счётчик изменений
      oBrw:Cargo:aIndexOpen := aIndexOpen            // ВСЕ открытые индексные файлы БАЗЫ
      oBrw:Cargo:cIndxTag   := cTagCurr              // текущий тэг индекса
      oBrw:Cargo:c2Title    := c2Title               // Журнал-событий-программы
      This.Cargo:oBrw       := oBrw                  // положить на форму
      //
      ON KEY ESCAPE OF &cForm ACTION {|  | iif( oBrw:IsEdit, oBrw:SetFocus() , _wPost(99) ) }  // выход по ESC
      ON KEY F1     OF &cForm ACTION {|ow| _wPost(11,ow,"_JHelp")  }
      //
      // Установка событий на это окно
      WITH OBJECT This.Object
         :Event( 0, {|  | iif(oBrw==Nil, Nil, oBrw:Setfocus()) } )

         // имя объекта + имя события  //   кнопки верхнего меню
         //            VVVV           ---   { "_JHelp", "_JCard"  , "_JFind" , "_JReport", "_JSetup"  , "_JExit" }
         :Event({11,"_JHelp"}, {|ow,ky,cn| //
                                           SET WINDOW THIS TO ow
                                           Darken2Open(ow:Handle)
                                           //MsgDebug(ow:Name, ky, cn)
                                           myHelpThis(ow:Name, cIcon, aBColor, ky, cn)
                                           Darken2Close(ow:Handle)
                                           SET WINDOW THIS TO
                                           ow:Enabler(cn, .T.)
                                           //This.&(cn).Enabled := .T.
                                           //ow:Setfocus('Buff')
                                           oBrw:Setfocus()
                                           Return Nil
                                           } )

         :Event({12,"_JCard"}, {|ow,ky,cn| // карточка
                                           Local nRec, obr := ow:Cargo:oBrw
                                           _LogFile(.T., "  -->> Button: ",cn, ow:Name, ky, obr:cAlias)
                                           nRec  := (obr:cAlias)->( RecNo() )
                                           If !IsString(cn)
                                              cn := "_JCard"   // всегда !!!
                                           Endif
                                           //obr:GoTop()
                                           SET WINDOW THIS TO ow
                                           Darken2Open(ow:Handle)
                                           //MsgDebug(ow:Name, ky, cn, This.&(cn).Caption)
                                           myViewCard(obr, ky, cn, cIcon)
                                           Darken2Close(ow:Handle)
                                           SET WINDOW THIS TO
                                           //This.&(cn).Enabled := .T.
                                           ow:Enabler(cn, .T.)
                                           //DbSelectArea(obr:cAlias)
                                           ? ProcNL(), obr:cAlias, "ALIAS()=",  ALIAS()
                                           //obr:Refresh()
                                           //obr:GoToRec( nRec )
                                           obr:Setfocus()
                                           Return Nil
                                           } )

         :Event({13,"_JFind"}, {|ow,ky,cn| // поиск
                                           Local obr := ow:Cargo:oBrw
                                           SET WINDOW THIS TO ow
                                           //MsgDebug(ow:Name, ky, cn, This.&(cn).Caption)
                                           Darken2Open(ow:Handle)
                                           myTsbFilter(obr, ky, cn)
                                           Darken2Close(ow:Handle)
                                           SET WINDOW THIS TO
                                           ow:Enabler(cn, .T.)
                                           //This.&(cn).Enabled := .T.
                                           //ow:Setfocus('Buff')
                                           obr:Setfocus()
                                           Return Nil
                                           } )

         :Event({14,"_JReport"}, {|ow,ky,cn| // печать - отчёты
                                             Local obr := ow:Cargo:oBrw
                                             SET WINDOW THIS TO ow
                                             //MsgDebug(ow:Name, ky, cn, This.&(cn).Caption)
                                             Darken2Open(ow:Handle)
                                             myTsbReport(obr, ow, ky, cn)  // -> user2report.prg
                                             Darken2Close(ow:Handle)
                                             SET WINDOW THIS TO
                                             ow:Enabler(cn, .T.)
                                             //This.&(cn).Enabled := .T.
                                             //ow:Setfocus('Buff')
                                             obr:Setfocus()
                                             Return Nil
                                             } )

         :Event({15,"_JSetup"}, {|ow,ky,cn| // настройки
                                            Local obr := ow:Cargo:oBrw
                                            SET WINDOW THIS TO ow
                                            //MsgDebug(ow:Name, ky, cn, This.&(cn).Caption)
                                            Darken2Open(ow:Handle)
                                            myTsbSetup(obr, ow, ky, cn)
                                            Darken2Close(ow:Handle)
                                            SET WINDOW THIS TO
                                            This.&(cn).Enabled := .T.
                                            //ow:Setfocus('Buff')
                                            obr:Setfocus()
                                            Return Nil
                                            } )

         :Event(19, {|ow| ow:Cargo:oBrw:DrawFooters() })  // при смене курсора таблицы (1) - перерисуем подвал

         :Event(90, {|ow,ky| // ON Release windows
                             Local cm
                             myTsbFont()     // выгрузить свои фонты для таблицы
                             cm := ProcNL()
                             ?  cm, "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                             ?  Repl(".", Len(cm)), "=> RELEASE WINDOW <=", ow:Name
                             DO EVENTS
                             Return Nil
                             })

         :Event({98,"_JExit"}, {|ow,ky,cn| // выход
                                          _LogFile(.T., "  -->> Button: ",cn, ow:Name, ky)
                                          //_SetThisFormInfo(ow)
                                          // запись
                                          //_SetThisFormInfo()
                                          _wSend(99,ow:Name)
                                          Return Nil
                                          } )

         :Event({99,"_JReleas"}, {|ow| ow:Release() } )

      END WITH

      _o2log(This.Object:oEvent, 27, ProcNL() + "  :oEvent => ", .T. ) // check in log
      _o2log(This.Object:oEvents, 27, ProcNL() + "  :oEvents => ", .T. ) // check in log

   END WINDOW

   ACTIVATE WINDOW &cForm

   ?  ProcNL(), "-->> End " + cForm

RETURN NIL

///////////////////////////////////////////////////////////////////////////////////////////
// список полей базы для таблицы и карточки / list of database fields for table and card
STATIC FUNCTION ListColumnTsb(aFonts, nWTbl)
   LOCAL nI, cFld, oCol, aFnt, nWCol, cText, nLen, cType, nPrc, nWItg, aDim := {}
   //          |              1         |     2       |   3    |      4           |    5          |   6
#ifdef KEY_ENG
   //          | column name            | base field  |field type| % display table|display table  | processing function.
   AADD( aDim, { "Event;Date"           , "DEVENT"    ,  "D"     ,     100        ,    1          , "" } )
   AADD( aDim, { "Event;Time"           , "TEVENT"    ,  "C"     ,     100        ,    1          , "" } )
   AADD( aDim, { "Computer"             , "COMPUTER"  ,  "C"     ,     100        ,    0          , "" } )
   AADD( aDim, { "User"                 , "LOGIN"     ,  "C"     ,     100        ,    0          , "" } )
   AADD( aDim, { "Operator;Code"        , "NUSER"     ,  "N"     ,     100        ,    0          , "" } )
   AADD( aDim, { "Operator"             , "USER"      ,  "C"     ,     70         ,    1          , "" } )
   AADD( aDim, { "Session;programs"     , "IDEVENT"   ,  "N"     ,     100        ,    1          , "" } )
   AADD( aDim, { "Event;code"           , "NEVENT"    ,  "N"     ,     100        ,    1          , "" } )
   AADD( aDim, { "Event name"           , "CEVENT"    ,  "C"     ,     90         ,    1          , "" } )
   AADD( aDim, { "Work time"            , "CTIME"     ,  "C"     ,     60         ,    1          , "" } )
   AADD( aDim, { "Note"                 , "REM"       ,  "C"     ,     0.2        ,    1          , "Scan2" } )
   AADD( aDim, { "ID;this;base"         , "ID"        ,  "N"     ,     100        ,    0          , "" } )
   AADD( aDim, { "DT_MODIFY"            , "DT_MODIFY" ,  "="     ,     100        ,    0          , "" } )
   AADD( aDim, { "DT_NEW"               , "DT_NEW"    ,  "@"     ,     100        ,    0          , "" } )
   AADD( aDim, { "DT_DEL"               , "DT_DEL"    ,  "@"     ,     100        ,    0          , "" } )
   AADD( aDim, { "DT_REST"              , "DT_REST"   ,  "@"     ,     100        ,    0          , "" } )
   AADD( aDim, { "EVENT"                , "EVENT"     ,  "@"     ,     100        ,    0          , "" } )
   //AADD( aDim, { "Version; bases"     , "D_20_09_20",  "C"     ,     100        ,    0          , "" } )
#else
   //          | наименование колонки   |   поле базы |Тип поля|%показа в таблице |показ в таблице|Функция обраб.
   AADD( aDim, { "Дата;события"         , "DEVENT"    ,  "D"   ,     100          ,     1         ,  ""      } )
   AADD( aDim, { "Время;события"        , "TEVENT"    ,  "C"   ,     100          ,     1         ,  ""      } )
   AADD( aDim, { "Компьютер"            , "COMPUTER"  ,  "C"   ,     100          ,     0         ,  ""      } )
   AADD( aDim, { "Пользователь"         , "LOGIN"     ,  "C"   ,     100          ,     0         ,  ""      } )
   AADD( aDim, { "Код;оператора"        , "NUSER"     ,  "N"   ,     100          ,     0         ,  ""      } )
   AADD( aDim, { "Оператор"             , "USER"      ,  "C"   ,     70           ,     1         ,  ""      } )
   AADD( aDim, { "Сессия;программы"     , "IDEVENT"   ,  "N"   ,     100          ,     1         ,  ""      } )
   AADD( aDim, { "Код;события"          , "NEVENT"    ,  "N"   ,     100          ,     1         ,  ""      } )
   AADD( aDim, { "Наименование события" , "CEVENT"    ,  "C"   ,     90           ,     1         ,  ""      } )
   AADD( aDim, { "Время работы"         , "CTIME"     ,  "C"   ,     60           ,     1         ,  ""      } )
   AADD( aDim, { "Примечание"           , "REM"       ,  "C"   ,     0.2          ,     1         ,  "Scan2" } )
   AADD( aDim, { "ID;этой;базы"         , "ID"        ,  "N"   ,     100          ,     0         ,  ""      } )
   AADD( aDim, { "DT_MODIFY"            , "DT_MODIFY" ,  "="   ,     100          ,     0         ,  ""      } )
   AADD( aDim, { "DT_NEW"               , "DT_NEW"    ,  "@"   ,     100          ,     0         ,  ""      } )
   AADD( aDim, { "DT_DEL"               , "DT_DEL"    ,  "@"   ,     100          ,     0         ,  ""      } )
   AADD( aDim, { "DT_REST"              , "DT_REST"   ,  "@"   ,     100          ,     0         ,  ""      } )
   AADD( aDim, { "EVENT"                , "EVENT"     ,  "@"   ,     100          ,     0         ,  ""      } )
   //AADD( aDim, { "Версия;базы"        , "D_20_09_20",  "C"   ,     100          ,     0         ,  ""      } )
#endif

   oCol := oHmgData()
   oCol:aDim := aDim

   // Назначение всех колонок таблицы / Assigning all columns of a table
   oCol:aHead  := {}      // название колонок в таблице / the name of the columns in the table
   oCol:aField := {}      // ОБЯЗАТЕЛЬНО для dbf !!!    / MANDATORY for dbf !!!
   oCol:aName  := {}      // имена колонок в таблице    / column names in the table
   oCol:aSize  := {}      // ширина колонок в таблице   / width of columns in table
   oCol:aPict  := {}      // формат полей, если нужно   / format fields if needed

   aFnt  := GetFontParam(GetFontHandle(aFonts[1]))      // Фонт ячеек / Cell font
   nWItg := 0

   FOR nI := 1 TO LEN(aDim)
      IF aDim[nI,5] == 1
         AADD( oCol:aHead, aDim[nI,1] )
         cFld := aDim[nI,2]
         nPrc := aDim[nI,4]
         AADD( oCol:aField, cFld )
         AADD( oCol:aName , cFld )
         nLen  := FIELDLEN ( FIELDNUM(cFld) )
         cType := FIELDTYPE( FIELDNUM(cFld) )
         IF cType $ "C"
            cText := REPL("x", nLen ) + "HH"  // отступ справа в колонках таблицы
         ELSEIF cType $ "M"                   // right indent in table columns
            cText := REPL("x", 50 )
         ELSEIF cType $ "=@T"
            cText := "H9999-99-99 99:99:99H"
         ELSEIF cType $ "+^"
            cText := REPL("9", nLen )
         ELSEIF cType == "D"
            cText := "H99.99.99H"
         ELSEIF cType == "N"
            cText := REPL("9", nLen )
         ELSEIF cType == "L"
            cText := REPL("9", nLen )
         ELSE
            cText := REPL("9", nLen )
         ENDIF
         nWCol := GetTxtWidth( cText, aFnt[2], aFnt[1] )
         IF nPrc < 100 .AND. nPrc > 1
            nWCol := INT(nWCol/100*nPrc)
         ELSEIF nPrc < 1
            nWCol := 200
         ENDIF
         AADD( oCol:aSize , nWCol )
         nWItg += nWCol
      ENDIF
   NEXT

   IF nWItg < nWTbl - GetHScrollBarHeight()
      nWCol := nWTbl - nWItg - GetHScrollBarHeight()
      oCol:aSize[LEN(oCol:aSize)] += nWCol
   ENDIF

   nWCol := 0
   FOR nI := 1 TO LEN(oCol:aSize)
      //? nI, oCol:aField[nI], oCol:aSize[nI]
      nWCol += oCol:aSize[nI]
   NEXT
   ? ProcNL(), "@@@ TSB @@@ width approximately =", nWCol, "nWTbl=", nWTbl
   ? "    oCol:aSize=", HB_ValToExp(oCol:aSize)

RETURN oCol

///////////////////////////////////////////////////////////////////
STATIC FUNCTION Draw_BtnEx_2Use( nY, nX, nGBtn, nW )
   LOCAL nHIco, lIco, nWBtn, nHBtn, nBtnLen, aFont2, oBtn, cText
   LOCAL nWText, nHBtn2, aYX, lRow := .T.  // кнопки по горизонтали
   DEFAULT nW := 0

   oBtn       := oHmgData()

#ifdef KEY_ENG // for this project demo-en.hbp
   oBtn:aCap  := { "Help", "Card", "F7 Search", "F5 Report", "Settings", "Exit" }
#else
   oBtn:aCap  := { "Помощь", "Карточка", "F7 Поиск", "F5 Отчеты", "Настройка", "Выход"  }
#endif

   nHIco      := myScreenIconSize(App.Cargo:aDisplayMode[2])  // высота иконки от экрана / icon height from screen
   nHIco      += 5
   lIco       := .T.  // растягивать размер иконки
   aFont2     := GetFontParam(GetFontHandle("FntBtnMain"))    // Фонт кнопок главной формы / Main form button font
   //               1          2            3           4             5            6
   oBtn:aObj  := { "_JHelp", "_JCard"  , "_JFind"  , "_JReport"   , "_JSetup"  , "_JExit"    } // метка события / event label
   oBtn:aClr  := { CLR_BLUE, CLR_FB    , CLR_SKYPE , {210,166,236}, CLR_GRAY   , {189,30,73} }
   oBtn:aPst  := { 11, 12, 13, 14, 15, 99 }  // _wPost(Х) - не использую / I don't use it
   nBtnLen    := LEN(oBtn:aCap)
   cText      := "Настройка00"
   nWText     := GetTxtWidth( cText, aFont2[2], aFont2[1] )
   IF nW == 0
      nWBtn   := nWText                                       // ширина кнопки / button width
   ELSE
      nWBtn   := ( nW - nGBtn * (nBtnLen+1) ) / nBtnLen       // ширина кнопки / button width
   ENDIF
   oBtn:aWBtn := { nWBtn, nWBtn, nWBtn, nWBtn, nWBtn, nWBtn }
   oBtn:lVert := .T.                                               // вертикальный текст на кнопке
   oBtn:aFnt  := { aFont2[1], aFont2[2], aFont2[3], oBtn:lVert }   // фонты для всех кнопок
   nHBtn2     := aFont2[2] * 4                                     // 2 строки текста на кнопках
   //nHBtn    := MAX(nHBtn,nHBtn2)                                 // скорректируем высоту кнопки
   //nHIco    := nHBtn - 10                                        // высота-ширина иконки на кнопке
   nHBtn      := nHIco + 25 + aFont2[2]*2
   oBtn:nHBtn := nHBtn                                             // ручное задание высоты кнопки
   oBtn:aIco  := { {"iUsers48x1"  , "iUsers48x2"   , lIco, nHIco } ,;
                   {"iCard48x1"   , "iCard48x2"    , lIco, nHIco } ,;
                   {"iFindTsb48x1", "iFindTsb48x2" , lIco, nHIco } ,;
                   {"iPrint48x1"  , "iPrint48x2"   , lIco, nHIco } ,;
                   {"iGear48x1"   , "iGear48x2"    , lIco, nHIco } ,;
                   {"iReturn48x1" , "iReturn48x2"  , lIco, nHIco }     }

   oBtn:aFntClr  := { BLACK, YELLOW }

   aYX := Draw_BtnEx( nY, nX, oBtn, nWBtn, nHBtn, nGBtn, lRow )  // -> util_button.prg

RETURN { aYX[1], aYX[2] }

////////////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION TsbPatam2Dbf(cForm, cAls, cBrw, nTsbW, cSuperHd, aColor, oColnm, aTsbFont )
   LOCAL oTsb, nHCell, nKChar, a, o
   DEFAULT nTsbW := 0

   IF IsObject(cForm)  ; oTsb := cForm      ; cForm      := NIL
   ELSE                ; oTsb := oHmgData() ; oTsb:cForm := cForm
   ENDIF
   cForm := oTsb:cForm ; Default cForm := oTsb:cFormName
                         Default cForm := _HMG_ThisFormName
   //
   oTsb:cForm          := cForm
   oTsb:cBrw           := cBrw
   oTsb:cAls           := cAls
   oTsb:lNoPicture     := .T.
   oTsb:lFooting       := .T.        // ставить в таблице подвал
   oTsb:aFoot          := .T.        // заполнить подвал
   oTsb:lNoPicture     := .T.
   oTsb:lSpecHd        := .T.        // поставить в таблице нумератор колонок
   oTsb:lSuperHd       := .T.        // поставить в таблице суперхидер
   oTsb:cSuperHd       := cSuperHd
   oTsb:aFont          := aTsbFont
   oTsb:uSelector      := 20                                            // использую
   nKChar              := 4                                             // кол-во знаков первой колонки
   oTsb:aNumber        := { 1, GetFontWidth(oTsb:aFont[4], nKChar)  }   // колонка нумерации и её ширина
   nHCell              := INT( GetFontHeight(oTsb:aFont[1])*1.35 )      // !!! только целые числа
   //nHCell            := IIF( nHCell < 32, 32, nHCell )
   oTsb:nHeightCell    := nHCell                            // высота ячеек
   oTsb:nHeightHead    := nHCell - 10                       // высота шапки
   oTsb:nHeightFoot    := nHCell - 10                       // высота подвала

   IF !IsLogic(oTsb:lSpecHd)
      oTsb:lSpecHd     := .F.                               // НЕ поставить в таблице нумератор
   ENDIF
   IF oTsb:lSpecHd
      oTsb:nHeightSpecHd := GetFontHeight(oTsb:aFont[4])    // высота нумератора
   ENDIF

   IF IsLogic(oTsb:lSuperHd) .AND. oTsb:lSuperHd
      oTsb:nHeightSuperHd := nHCell - 5                     // высота суперхидера
   ENDIF
   //
   Default aColor := array(5)
   IF IsArray(aColor) .and. Len(aColor) < 5 ; ASize(aColor, 5)
   ENDIF
   IF IsObject(aColor) ; oTsb:oClr := aColor     ; o := oTsb:oClr
   ELSE                ; oTsb:oClr := oHmgData() ; o := oTsb:oClr
   ENDIF
   //                1          2           3           4           5
   // aColor := { aClrForma, aClrBrush, aClrZebra1, aClrZebra2, aClrUslov }
   //
   o:aClrForma  := aColor[1] ; Default o:aClrForma  := {141,179,226}
   o:aClrBrush  := aColor[2] ; Default o:aClrBrush  := SILVER
   o:aClrZebra1 := aColor[3] ; Default o:aClrZebra1 := {116,172,242}
   o:aClrZebra2 := aColor[4] ; Default o:aClrZebra2 := {214,230,249}
   o:aClrUslov  := aColor[5] ; Default o:aClrUslov  := { {}, {} }
   //
   o:nClr1            := HMG_RGB2n(o:aClrForma)       // цвет фона шапка+подвал
   o:nClr2            := RGB( 48, 29,26)              // серо-черный фон
   o:aClr1            := { o:nClr1, o:nClr2 }
   o:aClr2            := { o:nClr2, o:nClr1 }
   o:aZebra           := { o:aClrZebra1, o:aClrZebra2 }
   oTsb:aBClrForm     := aColor[1]                    // цвет формы
   oTsb:lIdEvent      := .T.                          // закраска строк от "IDEVENT"
   oTsb:lZebra        := .T.                          //
   oTsb:aZebra        := o:aZebra                     //
   oTsb:aClrUsl       := o:aClrUsl                    // 7 условий для таблицы
   oTsb:aSuperHdColor := {CLR_YELLOW, o:aClr1}        // цвет: текст и фон суперхидера
   oTsb:aBrush        := o:aClrBrush                  // цвет фона под таблицей
   //oTsb:aIdEvent    := { CLR_HGRAY, CLR_YELLOW }    // цвета строк от "IDEVENT"
   oTsb:aIdEvent      := oTsb:aZebra                  // цвета строк от "IDEVENT"
   //
   // цвета в таблицу
   a := oTsb:aColorAdd ; Default a := {}
   AAdd(a, { CLR_TEXT , CLR_BLACK  })  // 1 , цвет текста ячеек
   // 2 , фона в ячейках таблицы
   //AAdd(a, { CLR_PANE, {|nr,nc,ob,nd| nr := CLR_BLACK, nc := CLR_WHITE, nd := CLR_HGRAY, ;
   //                      iif( (ob:cAlias)->(DELETED()), nr, iif( ob:nAt % 2 == 0, nc, nd ) ) } } )
   AAdd(a, { CLR_HEADF, CLR_WHITE  })  // 3 , текста шапки таблицы
   AAdd(a, { CLR_HEADB, o:aClr2    })  // 4 , фона шапки таблицы
                                       // 6 , фона курсора
   AAdd(a, { CLR_FOCUSB, {|a,b,c| a := b, iif( c:nCell == b, -CLR_HRED, -CLR_BLUE ) } } ) // 6
   AAdd(a, { CLR_EDITF, CLR_YELLOW })  // 7 , текста редактируемого поля
   AAdd(a, { CLR_EDITB, CLR_HRED   })  // 8 , фона редактируемого поля
   AAdd(a, { CLR_FOOTF, CLR_WHITE  })  // 9 , текста подвала таблицы
   AAdd(a, { CLR_FOOTB, o:aClr1    })  // 10, фона подвала таблицы
   AAdd(a, { CLR_SPCF , CLR_YELLOW })  // 18, specheader text - нумератор
   AAdd(a, { CLR_SPCB , o:aClr1    })  // 19, specheader back - нумератор
   //
   oTsb:aColorAdd     := a
   //
   // Назначение всех колонок таблицы
   oTsb:aHideCol := {} //{ 4, 5, 6, 7, 8}   // скрыть колонки, учитываем SELECTOR и ARRAYNO
   oTsb:aField   := oColnm:aField           // ОБЯЗАТЕЛЬНО для dbf !!!
   oTsb:aHead    := oColnm:aHead
   oTsb:aName    := oColnm:aName
   oTsb:aSize    := oColnm:aSize            // назначим ширину колонок для ТСБ
   oTsb:aPict    := oColnm:aPict            // формат полей, если нужно

   ThisCheckFieldDbf(oTsb:aField)           // проверка полей базы
   // Проверка колонок таблицы
   //  ? "oTsb:aField=", oTsb:aField ; ?v oTsb:aField
   //  ? "oTsb:aName= ", oTsb:aName  ; ?v oTsb:aName
   //  ? "oTsb:aPict=" , oTsb:aPict  ; ?v oTsb:aPict
   //  ? "oTsb:aHead= ", oTsb:aHead  ; ?v oTsb:aHead
   //  ? "oTsb:aFoot= ", oTsb:aFoot  ; ? IIF( IsArray(oTsb:aFoot), HB_ValToExp(oTsb:aFoot), oTsb:aFoot )
   //  ? "oTsb:aSize= ", oTsb:aSize  ; ?v oTsb:aSize
   //  ? "oTsb:aAlign=", oTsb:aAlign //; ?v oTsb:aAlign
   //
   // мои доп. данные по колонкам берем из ListColumnTsb()
   oTsb:aDimCard   := oColnm:aDim       // массив полей таблицы и карточки
   //oTsb:aColPrc  := oColnm:aColPrc     // тип обработки колонки
   //oTsb:aFunc1   := oColnm:aFunc1      // функция-1 :bPrevEdit для обработки колонки таблицы
   //oTsb:aFunc2   := oColnm:aFunc2      // функция-2 :bPostEdit для обработки колонки таблицы
   //oTsb:aBlock   := oColnm:aBlock      // кодовый блок на составные поля и функции
   //oTsb:aDecode  := oColnm:aDecode     // для колонки oCol:bDecode
   //oTsb:aCol     := oColnm:aCol        // массив колонок таблицы - сохраним ОБЯЗАТЕЛЬНО !!!
   //oTsb:aTable   := oColnm:aTable      // положить весь массив таблицы в cargo окна, на всякий случай
   //
   // блоки кода для _TBrowse(...) - название переменных bInit,bBody,bEnd,bAfter менять нельзя
   // ob == oBrw, op == oTsb, ob:Cargo:oParam == oTsb == op
   //oTsb:bInit  := {|ob,op| myTsbInit(ob,op)                   }  // настройки тсб
   //oTsb:bBody  := {|ob,op| myTsbKeyFX(ob,op), myTsbEdit(ob,op)}  // другие настройки тсб
   //oTsb:bAfter := {|ob,op| myTsbAfter(ob,op)                  }  // блок кода после END TBROWSE, чтобы не изменять oTsb:bEnd
   //oTsb:bEnd   := {|ob,op| myTsbEnd(ob,op) } // блок кода после END TBROWSE НЕ использовать
                                               // без необходимости, работает DEFAULT значение
                                               // !!! все делать в oTsb:bAfter !!!
   //
   oTsb:b_Init_Def := {|ob,op| // тут другие настройки тсб в :bInit
                   Local oTsb, oc
                   ob:nFreeze     := ob:nColumn("ORDKEYNO")       // Заморозить столбцы
                   //ob:nFreeze   := ob:nColumn("ADRESPRN")       // Заморозить столбцы
                   ob:lLockFreeze := .T.                          // Избегать прорисовки курсора на замороженных столбцах
                   ob:lNoKeyChar  := .F.                          // ввод в ячейки от букв, цифр
                   ob:nMemoHV     :=  1                           // показ одной строки мемо-поля
                   ob:nCell       := 3                            // передвинуть курсор
                   oTsb := op
                   oc := ATail(ob:aColumns)                       // последняя колонка
                   oc:nWidth -= 70                                // уменьшим ширину колонки
                   Return Nil
                   }
   // такой порядок работы блоков кода
   oTsb:bInit := {|ob,op| // настройки тсб
                   Local oTsb
                   ob:HideColumns( op:aHideCol ,.t.)              // скрыть колонки
                   ? "### oTsb:bInit", ProcNL(), HB_ValToExp(op:aHideCol)
                   //ob:nFreeze     := ob:nColumn("ORDKEYNO")       // Заморозить столбцы
                   //ob:lLockFreeze := .T.                          // Избегать прорисовки курсора на замороженных столбцах
                   //ob:lNoKeyChar  := .F.                          // ввод в ячейки от букв, цифр
                   //ob:nMemoHV     :=  1                           // показ одной строки мемо-поля
                   //ob:nCell       := 3                            // передвинуть курсор
                   // пример ^^^ общей, вынесенной настройки в :bInit
                   IF IsBlock(op:b_Init_Def) ; EVal(op:b_Init_Def, ob, op)
                   ENDIF
                   //
                   WITH OBJECT ob
                     oTsb := op
                     // мои доп. данные по колонкам
                     :Cargo:aDimCard  := oTsb:aDimCard    // массив полей таблицы и карточки
                     // запишем параметры условий для F4
                     // let's write down the parameters of the conditions for F4
                     :Cargo:cSayFltr   := "Все записи базы / All database records"
                     :Cargo:cTsbFilter := "All recno"
                     :Cargo:cSaySort   := "---"
                     :Cargo:cTsbSort   := "---"
                     :Cargo:aBClrForm  := oTsb:aBClrForm  // цвет формы
                     //:Cargo:aColPrc := oTsb:aColPrc     // тип обработки колонки
                     //:Cargo:aFunc1  := oTsb:aFunc1      // функция-1 :bPrevEdit для обработки колонки таблицы
                     //:Cargo:aFunc2  := oTsb:aFunc2      // функция-2 :bPostEdit для обработки колонки таблицы
                     //:Cargo:aTable  := oTsb:aTable      // положить весь массив таблицы в cargo окна, на всякий случай
                     //:Cargo:aBlock  := oTsb:aBlock      // кодовый блок на составные поля и функции
                     //:Cargo:aDecode := oTsb:aDecode     // для колонки oCol:bDecode
                     //:Cargo:lRecINS := .F.              // блокировка клавиши INS
                     //:Cargo:lRecDEL := .F.              // блокировка клавиши DEL
                     //:Cargo:nTable  := oTsb:nTable      // номер таблицы - обязательно !!!
                   END WITH
                   //Column_Init( ob, op )   // меняем поля на блок кода
                   //Color_Init( ob, op )    // задаём цвета в таблицу
                   Return Nil
                   }
   //
   oTsb:b_nClrBack   := {|clr,nat,ncol,obr|  // 2 , фона в ячейках таблицы
                         Local obc
                         IF pCount() < 4 ; obr := ncol ; ncol := nat
                         ENDIF
                         obc := obr:Cargo
                         clr := obc:aIdEvent[ obc:nIdPosBC ]
                         Return clr
                         }
   //
   oTsb:b_OnDrawLine := {|obr|   // |obr,row|
                         Local nIdE, nPos, lNo
                         Local obc := obr:Cargo
                         nIdE := obr:GetValue("IDEVENT")
                         nPos := obc:oIdEvent:Get(nIdE, 0)
                         IF ( lNo := nPos == 0 )
                            nPos := obc:nIdPosBC
                            nPos := iif( ++nPos > Len(obc:aIdEvent), 1, nPos )
                         ENDIF
                         obc:nIdPosBC := nPos
                         IF obc:nIdNew != obc:nIdEvent
                            obc:nIdEvent := nIdE
                         ENDIF
                         IF lNo ; obc:oIdEvent:Set(obc:nIdEvent, nPos)
                         ENDIF
                         Return Nil
                         }
   //
   oTsb:b_Body_Def   := {|ob,op| // другие настройки тсб
                         Local obc := ob:Cargo, nPos := 1
                         obc:nIdPosBC := nPos
                         obc:aIdEvent := op:aIdEvent
                         Default obc:aIdEvent := { CLR_HRED, CLR_WHITE }
                         obc:nIdEvent := ob:GetValue("IDEVENT")
                         obc:oIdEvent := oHmgData()
                         obc:oIdEvent:Set(obc:nIdEvent, nPos)
                         Return Nil
                         }

   oTsb:bBody := {|ob,op| // другие настройки тсб
                   Local nBClr := GetSysColor( COLOR_BTNFACE )   // цвет системный
                   Local hFont := GetFontParam(op:aFont[4])      // SpecHider
                   Local lZebra, oc, i := 0
                   hFont := ob:aColumns[1]:hFontSpcHd            // 4-special header font - или так
                   ob:lNoHScroll  := .T.   // нет показа горизонтального скролинга
                   ob:oHScroll    := NIL
                   // раскраска в зебру
                   lZebra := !Empty( op:lZebra )
                   Default op:lZebra := lZebra
                   IF lZebra
                      Default op:aZebra := { CLR_HGRAY, CLR_WHITE }
                   ENDIF
                   //
                   //ob:Cargo:nIdPosBC := 1
                   //ob:Cargo:aIdEvent := { CLR_HGRAY, CLR_WHITE }
                   //ob:Cargo:nIdEvent := ob:GetValue("IDEVENT")
                   // пример ^^^ общей, вынесенной настройки в :bBody
                   IF IsBlock(op:b_Body_Def) ; EVal(op:b_Body_Def, ob, op)
                   ENDIF
                   // замена первой колонки и спецхидера
                   FOR EACH oc IN ob:aColumns
                      oc:cSpcHeading := NIL
                      Default oc:Cargo := oHmgData()
                      IF oc:cName == "ARRAYNO" .OR. oc:cName == "ORDKEYNO"
                         oc:cSpcHeading := "#"
                         oc:nClrBack    := nBClr            // RGB(240,240,240) // изменение цвета фона виртуальной колонки
                         oc:nClrFore    := CLR_RED          // изменение цвета текста виртуальной колонки
                         oc:hFont       := hFont            // изменение фонта виртуальной колонки
                         oc:bDecode     := {|cv| Alltrim(cv) }
                         oc:nAlign      := DT_CENTER
                         oc:nFAlign     := DT_CENTER
                      ELSEIF oc:cName == "IDEVENT"  // только колонка "IDEVENT"
                         IF IsBlock(op:b_nClrBack)  // 2 , фона в ячейках таблицы
                            oc:nClrBack := op:b_nClrBack
                         ENDIF
                      ELSE
                         oc:cSpcHeading := hb_ntos( ++i )
                      ENDIF
                      IF lZebra .and. op:lIdEvent   // для всех колонок
                         IF IsBlock(op:b_nClrBack)  // 2 , фона в ячейках таблицы
                            oc:nClrBack := op:b_nClrBack
                         ENDIF
                      ENDIF
                   NEXT
                   //
                   IF IsBlock(op:b_OnDrawLine)
                      ob:bOnDrawLine := op:b_OnDrawLine
                   ENDIF
                   //
                   // поставим в подвал
                   // смена курсора таблицы / change table cursor
                   // cFooting := Eval( oColumn:cFooting, nCol, oBrw )
                   oc := ob:GetColumn("DEVENT")
                   oc:nFAlign  := DT_LEFT
                   oc:cFooting := {|nc,ob|
                                   Local na := ob:nAt, nl := ob:nLen
                                   nc := ""
                                   If ob:nLen > 0
                                      nc := hb_ntos(na)+ "/" + hb_ntos(nl)
                                      nc += Space(5) //+ " [!]"
                                   EndIf
                                   Return nc
                                   }
                   oc := ob:GetColumn("REM")
                   oc:nFAlign  := DT_LEFT
                   oc:cFooting := {|nc,ob|
                                   Local na := ob:nAt, nl := ob:nLen
                                   //DbSelectArea(ob:cAlias) - не нужно
                                   nc := "ORDER: ["
                                   If (ob:cAlias)->(OrdCount()) > 0
                                      nl := (ob:cAlias)->(INDEXORD())
                                      nc += hb_ntos(nl) + " | "
                                      nc += (ob:cAlias)->(OrdName(nl)) + "]"
                                   Else
                                      nc += "0]"
                                   EndIf
                                   na := nc
                                   Return nc
                                   }

                   ob:bChange := {|ob|  _wPost(19, ob:cParentWnd, ob) } // при смене курсора таблицы - см.окно
                   //
                   ? "### oTsb:bBody", ProcNL(), ob:nHeightSuper, ob:lDrawSuperHd, ;
                     "Head=",ob:nHeightHead, "Foot",ob:nHeightFoot
                   ? "### oTsb:bBody   op:cSuperHd=",op:cSuperHd //,"oc:cName=",oc:cName, oc:nFAlign
                   ?? "Als=", ob:cAlias
                   ? ProcNL(), "@@@ TSB @@@ width =", ob:GetAllColsWidth()
                   DO EVENTS
                   Return Nil
                   }

   // назначим клавиши в таблице
   oTsb:aUserKeys := { ;
        {VK_F2    , {|ob| _wPost(32, ob:cParentWnd, ob)          } }, ;  // инфо по базе
        {VK_F3    , {|ob| _wPost(33, ob:cParentWnd, ob)          } }, ;  // инфо по фонтам
        {VK_F4    , {|ob| _wPost(34, ob:cParentWnd, ob)          } }, ;  // инфо по индексам
        {VK_F5    , {|ob| _wPost(14, ob:cParentWnd, "_JReport")  } }, ;  // кнопка Отчёты
        {VK_F7    , {|ob| _wPost(13, ob:cParentWnd, "_JFind")    } }, ;  // кнопка Поиск
        {VK_F9    , {|ob| _wPost(15, ob:cParentWnd, "_JSetup")   } }, ;  // кнопка Настройка
        {VK_RETURN, {|ob|
                      Local oc := ob:aColumns[ ob:nCell ]
                      Local xval, lRet, cTxt, cLog, cMsg
                      IF oc:cName == "MARK"
                      ELSEIF oc:cName == "KZBID"
                         //oc:bPrevEdit := {|val, brw| ColumnEditPrev_Two( val, brw ) }
                         xval := ob:GetValue(ob:nCell)
                         lRet := EVal(oc:bPrevEdit, xval, ob )
                      ELSEIF oc:cName == "REM"
                         cTxt := ob:GetValue(ob:nCell)
                         IF LEN(cTxt) > 0
#ifdef KEY_ENG
   cMsg := "Do you want to open the editor with this cell value?;"
#else
   cMsg := "Вы хотите открыть редактор с этим значением ячейки ?"
#endif
                            IF AlertYesNo( cMsg, "Open file", .T., "iQuest64", 64, { LGREEN, RED }, .T. )
                               cLog := App.Cargo:cPathTemp + "\error-log.log"
                               HB_MemoWrit( cLog, cTxt )
                               DO EVENTS ; wApi_Sleep(100)
                               ShellExecute( , 'open', cLog, , , SW_SHOWNORMAL)
                            ENDIF
                         ENDIF
                      ELSE
                        _wPost(12, ob:cParentWnd )  // карточка
                      ENDIF
                      Return Nil
                      } } }

   // назначить события на окно
   oTsb:aEvents   := { ;
        {32, {|ow,ky,ob| myTsbListColumn( ob )  , ob:Setfocus(), ky:=ow:Name } }, ;   // инфо по списку колонок
        {33, {|ow,ky,ob| myTsbListFont( ob )    , ob:Setfocus(), ky:=ow:Name } }, ;   // инфо по фонтам таблицы
        {34, {|ow,ky,ob| myTsbUseDbf( ob )      , ob:Setfocus(), ky:=ow:Name } }  ;   // инфо по фильтру/сортировке таблицы
                     }
        //{50, {|ow,ky,ob| _wPost("_TsbRClick",ow) , ky:=ow:=ob                } }  ;   // правый клик мышки
        //НЕЛЬЗЯ !!! {40, {|ow,ky,ob| _wPost(12,ow)          , ob:Setfocus(), ky:=ow:Name } }  ;   // карточка

   // Двойной клик мышки на курсоре в таблице - уже есть
   //oTsb:bLDblClick := {|p1,p2,p3,ob| p1:=p2:=p3, ob:PostMsg( WM_KEYDOWN, VK_RETURN, 0 ) }
   oTsb:bLDblClick := .T.                       // Вот так !!!
   // Правый клик мышки на курсоре в таблице
   //oTsb:bRClicked  := {|p1,p2,p3,ob| _wPost(50, ob:cParentWnd, {p1,p2,p3,ob}) }
   // Левый клик мышки на курсоре в таблице
   //oTsb:bLClicked  := {|p1,p2,p3,ob| _wPost(XXX, ob:cParentWnd, {p1,p2,p3,ob}) }

RETURN oTsb

/////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbFont( lCreate, nFSDef )
   LOCAL aFont := {}, cFont
   DEFAULT nFSDef := _HMG_DefaultFontSize

   // создаем массив имен фонтов для режимов или убираем их
   AAdd( aFont, "TsbNorm"   )
   AAdd( aFont, "TsbBold"   )
   AAdd( aFont, "TsbSpecH"  )
   AAdd( aFont, "TsbSuperH" )
   AAdd( aFont, "TsbEdit"   )

   IF empty(lCreate)
      FOR EACH cFont IN aFont ; _ReleaseFont( cFont )
      NEXT
   ELSE
      DEFINE FONT TsbNorm   FONTNAME "DejaVu Sans Mono"   SIZE nFSDef
      //DEFINE FONT TsbNorm   FONTNAME "Arial"              SIZE nFSDef
      DEFINE FONT TsbBold   FONTNAME "Tahona"             SIZE nFSDef - 1 BOLD
      DEFINE FONT TsbSpecH  FONTNAME _HMG_DefaultFontName SIZE nFSDef - 3 BOLD
      DEFINE FONT TsbSuperH FONTNAME "Comic Sans MS"      SIZE nFSDef + 3 BOLD
      DEFINE FONT TsbEdit   FONTNAME "Tahona"             SIZE nFSDef
      //DEFINE FONT TsbEdit   FONTNAME "Arial"              SIZE nFSDef BOLD
   ENDIF

RETURN .T.

/////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myHelpThis(cForm,cIcon,aBClr)
   LOCAL aTmp, aFClr, cMsg, cTtl

#ifdef KEY_ENG // for this project demo-en.hbp
   cMsg := "Event log. Provides basic information about sessions.;"
   cMsg += "Records the start and end of each connection, specifies the user,;"
   cMsg += "who initiated it.;"
   cMsg += "The event log helps track activity and analyze the work of;"
   cMsg += "employees, receiving information about the date and time of each entry or exit;"
   cMsg += "from the program, the number of applications entered or the printing of debts, ;"
   cMsg += "debt receipts;"
   cTtl := "Event Log"
#else
   cMsg := "Журнал событий. Предоставляет базовую информацию о сеансах.;"
   cMsg += "Фиксирует начало и завершение каждого подключения, указывает пользователя,;"
   cMsg += "который его инициировал.;"
   cMsg += "Журнал событий помогает отслеживать активность и анализировать работу;"
   cMsg += "сотрудников, получая сведения о дате и времени каждого входа или выхода;"
   cMsg += "из программы, количества введеных заявок или печати задолженностей, ;"
   cMsg += "квитанций долга;"
   cTtl := "Журнал событий"
#endif

   aTmp  := cForm
   aFClr := NAVY
   aTmp  := _SetMsgAlertColors(aBClr,aFClr)  // новые цвета

   AlertInfo(cMsg, cTtl, cIcon, 64, {AQUA} )

   _SetMsgAlertColors(aTmp[1],aTmp[2])       // восстановить цвета

RETURN NIL

////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myViewCard(oBrw,ky,cBtn,cIcon)
   LOCAL aWin, aTsbCard, n2WColumn, aBColor
   LOCAL nCol, nRow, nWidth, nHeight

   ? ProcNL(), oBrw:cAlias, ky, cBtn, cIcon
   ky := cBtn
   //aTsbCard :=  ListColumnTsb()        // <-  возврат О, список полей базы для таблицы и карточки
   aTsbCard   :=  oBrw:Cargo:aDimCard    // список полей базы для таблицы и карточки
   n2WColumn  := oBrw:aColumns[1]:nWidth + oBrw:aColumns[2]:nWidth
   n2WColumn  += oBrw:nLeft + GetBorderWidth()
   nRow       := This.Row
   nCol       := This.Col
   nWidth     := This.Width // - GetBorderWidth()
   nHeight    := This.Height
   aBColor    := HMG_n2RGB( CLR_FB )

   aWin := { nRow, nCol + n2WColumn, nWidth - n2WColumn, nHeight, aBColor }

   User2LogCard(aWin,oBrw,aTsbCard,cIcon)

RETURN NIL

////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbFilter(oBrw)
   LOCAL hFont, aFont, cFontName, nFontSize, lFontBold, c2Title, cTag
   LOCAL aRet, cTitle, cKeyIndx, cFilter, cAls, aFilter, cTagNew
   LOCAL nLenMax, nJ, nWTsb, nMaxLine, aIndexOpen, cNameInd, cLang

#ifdef KEY_ENG // for this project demo-en.hbp
   cLang := "The entire event database"
#else
   cLang := "Вся база событий"
#endif

   cAls := oBrw:cAlias
   aRet := myUser2Filter()    // меню фильтра - { cStr, cFilter, cSort }
   DbSelectArea(cAls)
   ? ProcNL(), ALIAS(), INDEXORD(), OrdName(INDEXORD())
   ? "     aRet=", aRet, HB_ValToExp(aRet)
   IF LEN(aRet) > 0

      cTitle   := aRet[1]
      cTitle   := IIF( LEN(cTitle)==0,cLang,cTitle)
      cFilter  := aRet[2]
      cKeyIndx := aRet[3]
      cNameInd := aRet[4]
      aFilter  := { cTitle }
      // запишем параметры условий для F4
      // let's write down the parameters of the conditions for F4
      oBrw:Cargo:cSayFltr   := cTitle
      oBrw:Cargo:cTsbFilter := cFilter
      oBrw:Cargo:cSaySort   := cKeyIndx
      oBrw:Cargo:cTsbSort   := cNameInd

      //? "------------ Фильтр по базе! ------------"
      //? "LEN(cTitle)=", LEN(cTitle)
      //? cTitle
      //? cFilter
      //? cKeyIndx

      nWTsb  := oBrw:nWidth                   // ширина таблицы
      //nHSupH := oBrw:nHeightSuper           // высоту суперхидера МЕНЯТЬ НЕЛЬЗЯ !
                                              // здесь 3 строки в суперхидере
      hFont     := oBrw:aSuperHead[ 1, 7 ]    // 4-special header font
      aFont     := GetFontParam(hFont)
      cFontName := aFont[1]
      nFontSize := aFont[2]
      lFontBold := aFont[3]
      nLenMax   := GetMaxChar4FontWidth( "x", nWTsb, cFontName, nFontSize, lFontBold )
      nMaxLine  := MLCount( cTitle, nLenMax, 3, .F. )
      IF LEN(cTitle) > nLenMax
         c2Title := ""
         FOR nJ := 1 TO nMaxLine
            c2Title += MemoLine( cTitle, nLenMax, nJ, 3, .F. )
            c2Title += IIF( nJ == nMaxLine, "", CRLF)
         NEXT
      ELSE
         c2Title := cTitle
      ENDIF

      oBrw:aSuperHead[1,3] := c2Title             // поменяли СуперХидер / SuperHeader changed
      User2Filter(cAls, cKeyIndx, cFilter)
      DbSelectArea(cAls)                          // !!! ОБЯЗАТЕЛЬНО / NECESSARILY
      //? ProcNL(), "-----------", "OrdCount()=", OrdCount()
      //? myGetIndexUse()
      DbSetOrder(OrdCount())
      cTagNew := (cAls)->( OrdName(OrdCount()) )
      oBrw:uLastTag := cTagNew                    // без этого индекс на ТСБ слетает
                                                  // without this the index on the TSB will fall
      oBrw:Reset()
      oBrw:Display()
      oBrw:Refresh(.T.)

      /////////////// перезапуск окна с таблицей ////////////////////////
      //_wPost(99)  // закрытие окна таблицы
      //This.Release
      //INKEYGUI(1000)
      //User2Filter(cAls, cKeyIndx, cFilter)
      //User2LogForm(cAls,aFilter)
   ELSE

      // запишем параметры условий для F4
      // let's write down the parameters of the conditions for F4
      oBrw:Cargo:cSayFltr   := cLang
      oBrw:Cargo:cTsbFilter := "All recno"
      oBrw:Cargo:cSaySort   := "---"
      oBrw:Cargo:cTsbSort   := "---"

      // нажата кнопка Отмена / Cancel button pressed
      oBrw:aSuperHead[1,3] := cLang             // поменяли СуперХидер
      aIndexOpen  := oBrw:Cargo:aIndexOpen      // ВСЕ открытые индексные файлы БАЗЫ
      cTag        := oBrw:Cargo:cIndxTag        // текущий тэг индекса
      //? ProcNL(), "-----------", myGetIndexUse()
      IF OrdCount() > 0
         DBCLEARINDEX()
         myIndexOpenRestore(aIndexOpen, .F.)    // Восстановить открытые индексы
      ENDIF
      //MsgDebug(aIndexOpen)                    // !!! обнуляет ALIAS()
      //MsgInfo() + AlertInfo()                 // !!! обнуляет ALIAS()
      //? ProcNL(), "-----------", myGetIndexUse()
      oBrw:uLastTag := cTag                    // без этого индекс слетает
      OrdSetFocus(cTag)
      oBrw:Reset()
      oBrw:Display()
      oBrw:Refresh(.T.)

   ENDIF
   oBrw:SetFocus()
   DO EVENTS

RETURN NIL

////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myTsbSetup(oBrw)
   LOCAL cMsg

#ifdef KEY_ENG // for this project demo-en.hbp
   cMsg := This.Title + ";;"
   cMsg += "Table settings will be here!;"
   cMsg += oBrw:cAlias
#else
   cMsg := This.Title + ";;"
   cMsg += "Здесь будут настройки таблицы !;"
   cMsg += oBrw:cAlias
#endif

   AlertInfo( cMsg, , , , {RED})

RETURN NIL

//////////////////////////////////////////////////////////////////
STATIC FUNCTION ThisCheckFieldDbf(aNames)
   LOCAL cTmp, n, nE, cMsg, cAls, nCols

   cAls  := ALIAS()
   cTmp  := ""

   IF aNames == NIL
      cMsg := "Error! No list of DB fields: " + cAls + ";;"
      cMsg += "aNames == NIL"
      cMsg += ";;" + ProcNL() + ";" + ProcNL(1)
      AlertStop(cMsg,,,64,{RED})
      RETURN NIL
   ENDIF

   nCols := Len( aNames )

   FOR n := 1 TO nCols
      nE := ( cAls )->( FieldPos( aNames[ n ] ) )
      IF nE == 0
         cTmp += HB_NtoS(n) + ". " + aNames[ n ] + ";"
      ENDIF
   NEXT
   IF LEN(cTmp) > 0
      cMsg := "Error! No fields in DB: " + cAls + ";;" + cTmp
      cMsg += ";;" + ProcNL() + ";" + ProcNL(1)
      AlertStop(cMsg,,,64,{RED})
   ENDIF

RETURN NIL

///////////////////////////////////////////////////////////////////////////
// Как правильно написать функцию, чтобы колёсико прелодера не замирало ?
// How to write a function correctly so that the preloader wheel does not freeze?
FUNCTION myProgress()
DO EVENTS
RETURN .T.

//////////////////////////////////////////////////////////////////////
// условная индексация / conditional indexation
FUNCTION User2Filter(cAls, cKeyIndex, cToFilter )
   LOCAL cFileIndx

#ifdef KEY_ENG
   WaitWindow( "... Wait for the preparation to complete ...", .T. )
#else
   WaitThreadCreateIcon( "Поиск данных",  )
#endif

   DbSELECTArea(cAls)
   DBCLEARINDEX()
   cFileIndx := GetUserTempFolder() + "tmp_users2log.cdx"
   DeleteFile( cFileIndx )
   IF LEN(cToFilter) == 0
      INDEX ON &cKeyIndex TAG TEMP_00 TO (cFileIndx) EVAL myProgress()
   ELSE
      INDEX ON &cKeyIndex TAG TEMP_00 TO (cFileIndx)  EVAL myProgress() FOR &cToFilter
   ENDIF

#ifdef KEY_ENG
   WaitWindow()
#else
   WaitThreadCloseIcon()
#endif

RETURN .T.

//////////////////////////////////////////////////////////////////////
FUNCTION User2LogBase()
   LOCAL cFile, cAls, cCdp, cVia

   cFile := ".\users2log.dbf"
   cAls  := "User2Log"
   cCdp  := "RU866"
   cVia  := "DBFCDX"

   IF hb_FileExists( cFile )
      USE &(cFile) ALIAS (cAls) SHARED NEW CODEPAGE cCdp
   ELSE
      AlertStop( 'File Dbf not found !;' + cFile  + ";;" + ProcNL(),,,64, {RED} )
      RETURN .F.
   ENDIF

RETURN .T.

