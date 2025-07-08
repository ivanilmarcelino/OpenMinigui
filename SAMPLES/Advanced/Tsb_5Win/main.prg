/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2021 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2021 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 * Copyright 2013 Grigory Filatov <gfilatov@inbox.ru>
 *
 * Работа с окнами и одной карточкой
 * Контейнер на окне. Работа с таймером. Таймер по событию.
 * Working with windows and one card
 * Container on the window. Working with a timer. Event timer.
*/
//#define  _HMG_OUTLOG
#define  SHOW_TITLE   "Working with windows and one card"
#define  VERSION_PRJ  SPACE(10) + "Ver 1.1 - 05.12.23"

#include "minigui.ch"

STATIC nStatR := 50, nStatG := 50, nStatB := 50

MEMVAR oMain
////////////////////////////////////////////////////////////////////////////
FUNCTION Main()
   LOCAL cFont := 'Tahoma', nFontSize := 20
   LOCAL nY, nX, nW, nH, nG, nWBtn, nHBtn, nTimeSec := 0.2
   LOCAL hMainWin, cText, aBackColor := { 94, 59,185}

   PUBLIC oMain              // назначим переменную для объекта окна Main

   SetsEnv()    // loading the program environment -> SetsEnv_misc.prg
   //////////////////////////////////////////////////////////////////////
   IsVersionMiniGui()  // проверка номера версии МиниГуи
   /////////////////////////////////////////////////////////////////////
   LoadPublicValueThisProject()       // Функция загрузки моих данных
   /////////////////////////////////////////////////////////////////////
   myTsbFont( .T., ModeSizeFont() )   // создать фонты для таблиц

   nY := nX := 0 ; nW := System.ClientWidth ; nH := 100

   DEFINE WINDOW wMain AT nY, nX WIDTH nW HEIGHT nH   ;
       ICON       "1MAIN_ICO"                         ;
       TITLE      SHOW_TITLE + VERSION_PRJ            ;
       MAIN       NOMAXIMIZE NOSIZE                   ;
       FONT cFont SIZE nFontSize                      ;
       BACKCOLOR  aBackColor                          ;
       ON GOTFOCUS RefreshWin( ThisWindow.Handle )    ;      // перересовка объектов на форме
       ON INIT    {|| DoEvents(), _wSend(100) }       ;      // выполняется после инициализации окна
       ON RELEASE {|| myExit()      }                 ;      // выполняется перед разрушением окна
       ON INTERACTIVECLOSE {|lRet| lRet := myQuit(.F.) }     // НЕТ выхода, пока есть окна STANDARD

       oMainCargoInit()                 // см. ниже - создать объект (контейнер) для окон

       nW       := This.ClientWidth     // ширина внутри окна
       nH       := This.ClientHeight    // высота внутри окна
       hMainWin := ThisWindow.Handle    // хендл этого окна
       ? ProcNL()
       ? "The title of this window = [" + SHOW_TITLE + "]"
       ? "Handle of this window = " + HB_NtoS(hMainWin)
       ? "Фокус на форму по умолчанию = ", M->oMain:Cargo:aFormData[1]

       DRAW ICON IN WINDOW wMain AT 5, 5 PICTURE "2MAIN_64" WIDTH 64 HEIGHT 64 COLOR aBackColor
       nY := 10 ; nX := 5 + 64 + 5

       nWBtn := 50     // ширина кнопки
       nHBtn := 50     // высота кнопки
       nG    := 10     // расстояние между кнопками

       nX := Button_MainMenu(nY,nX,nG,nWBtn,nHBtn)  // положить меню кнопок на форму
       nX += nG

       // объекты для анимации букв / objects for animation of letters
       M->oMain:Cargo:aFlicker := { ThisWindow.Name, "Timer_1", "Label_1" }

       cText := SHOW_TITLE
       @ 0, nX LABEL Label_1 VALUE cText WIDTH nW - nX HEIGHT nH ;
         FONTCOLOR WHITE FONT 'Arial Black' BOLD TRANSPARENT CENTERALIGN VCENTERALIGN ;
         ON INIT {|| SetFontSizeTextMax(ThisWindow.Name, This.Name) ,;
                     M->oMain:Cargo:aFlickLbl := Label_Init(ThisWindow.Name, This.Name) }

       // События для объектов окна / Events for window objects
       MainEventsWindowObjects()

       // переведено на события, чтобы мышка в движении на буквах не замирала
       // translated to events so that the mouse does not freeze in movement on letters
       // flickering inscriptions from the top down
       //DEFINE TIMER Timer_1 INTERVAL 1000*nTimeSec ACTION ( This.Enabled := .F., _wPost(90) )
       // ------ лучше так для маленьких задержек (< 1 минуты) таймера ! -----
       DEFINE TIMER Timer_1 INTERVAL 1000*nTimeSec ACTION {|| SetProperty("wMain", "Timer_1", "Enabled", .F.) ,;
                                   Label_Left2Right(90, .F.), SetProperty("wMain", "Timer_1", "Enabled", .T.) }
       This.Timer_1.Enabled := .F.                        // отключить до On Init
       This.Timer_1.Visible := .F.  // блок кода без изменения среды This

       ON KEY F1     ACTION NIL
       ON KEY ESCAPE ACTION NIL

   END WINDOW

   //CENTER WINDOW wMain
   ACTIVATE WINDOW wMain

RETURN NIL

///////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION oMainCargoInit()
   LOCAL nI, cI

   M->oMain       := This.Object
   M->oMain:Cargo := oHmgData()

   WITH OBJECT M->oMain:Cargo
      :nStartWindow := 0                    // номер auto создаваемого окна
      :aFormData := {}
      FOR nI := 1 TO 5
          cI := "wTable_" + hb_ntos(nI)
          AAdd( :aFormData, cI )            // список окон
          :Set( cI, oHmgData() )            // по именам окна создадим контейнер для данных окна
      NEXT
      :lFormCard := .F.
      :cFormCard := ""
   END WITH

RETURN Nil

///////////////////////////////////////////////////////////////////////////////
// События для объектов окна / Events for window objects
FUNCTION MainEventsWindowObjects()

       WITH OBJECT This.Object
          :Event(  0, {|  | wApi_Sleep(200), AlertInfo("Запуск события 0") } )

          :Event(  1, {|  | This.Btn_01.SetFocus, Show_Table(1)  } )
          :Event(  2, {|  | This.Btn_02.SetFocus, Show_Table(2)  } )
          :Event(  3, {|  | This.Btn_03.SetFocus, Show_Table(3)  } )
          :Event(  4, {|  | This.Btn_04.SetFocus, Show_Table(4)  } )
          :Event(  5, {|  | This.Btn_05.SetFocus, Show_Table(5)  } )

          :Event(100, {|ow| // открыть сразу все таблицы (auto создания окон)
                           LOCAL om := This.Cargo
                           This.Topmost := .F.
                           om:nStartWindow += 1
                           IF om:nStartWindow > Len(om:aFormData)
                              om:nStartWindow := 0            // конец auto создания окон
                              ow:SetFocus("Btn_01")           // переключить кнопку
                              SwitchToWin( om:aFormData[1] )  // переключить на форму
                           ENDIF
                           IF om:nStartWindow > 0
                              DO EVENTS
                              _wPost(om:nStartWindow, ow:Name)
                           ENDIF
                           This.Timer_1.Enabled := .T.  // влючить таймер
                           RETURN NIL
                     } )

          :Event(101, {|ow| // установка фокуса на первое окно из списка окон, если оно есть
                           LOCAL nI := 0, om := This.Cargo, cForm
                           DO EVENTS
                           FOR EACH cForm IN om:aFormData
                              IF _IsWindowDefined( cForm )
                                 nI := hb_EnumIndex( cForm )
                                 EXIT
                              ENDIF
                           NEXT
                           DO EVENTS
                           IF nI > 0 ; _wPost(nI, ow:Name)
                           ELSE      ; ow:SetFocus("Btn_06")  // Quit
                           ENDIF
                           RETURN NIL
                      } )

          // включить анимацию букв, если вкл. DEFINE TIMER Timer_1 ... _wPost(90)
          :Event( 90, {|ow,ky| Label_Left2Right(ky,.F.), ky := ow            } )
          // разное
          :Event( 91, {|     | This.Minimize, wApi_Sleep(200), This.Restore  } )
          :Event( 92, {|     | This.Hide , wApi_Sleep(200), This.Show        } )
          :Event( 99, {|ow   | ow:Release()   /* выход/quit */               } )
       END WITH

RETURN NIL

/////////////////////////////////////////////////////////////////////////////////
// записать в массив названия,цвет,иконки и т.д.
FUNCTION Button_MainMenu(nY,nX,nG,nWBtn,nHBtn)
   LOCAL aBtnCap, aBtnIco, aBtnClr, aBtnPst, cCapt, aIco
   LOCAL cN, cFont, nFSize, lFBold, nJ, aColor, aGrOver, aGrFill
   LOCAL aFntClr, nwPost, aWBtn, nI

   // кнопки
   aBtnCap := { "1", "2", "3", "4", "5" , "Quit" }  // надписи иконок
   aBtnIco := {}                                    // нет иконок
   aBtnPst := { 1, 2, 3, 4, 5, 99 }                 // события на кнопке
   aBtnClr := ARRAY(6)                              // цвета на кнопке
   aWBtn   := ARRAY(6)                              // ширина кнопки
   FOR nI := 1 TO LEN(aBtnCap) - 1
      aBtnClr[nI] := myTableBackColor(nI)           // цвет формы -> ListTables.prg
      aWBtn[nI]   := nWBtn
   NEXT
   aBtnClr[6] := {189, 30, 73}
   aWBtn[6]   := nWBtn*2
   // фонты для кнопок
   aFntClr := { BLACK , YELLOW }
   cFont   := "Arial Black"
   nFSize  := 16
   lFBold  := .T.

   FOR nJ := 1 TO LEN(aBtnCap)
      cCapt   := StrTran( aBtnCap[nJ], ";" , CRLF )
      aColor  := aBtnClr[nJ]
      nwPost  := aBtnPst[nJ]
      cN      := 'Btn_' + StrZero(nJ, 2)
      nWBtn   := aWBtn[nJ]

      @ nY, nX BUTTONEX &cN WIDTH nWBtn HEIGHT nHBtn                                 ;
        CAPTION cCapt ICON NIL FLAT NOXPSTYLE HANDCURSOR NOTABSTOP                   ;
        FONT cFont SIZE nFSize BOLD FONTCOLOR aFntClr[1] BACKCOLOR aColor            ;
        ON MOUSEHOVER ( This.Fontcolor := aFntClr[2], This.Backcolor := This.Cargo:aBClr2 ) ;
        ON MOUSELEAVE ( This.Fontcolor := aFntClr[1], This.Backcolor := This.Cargo:aBClr1 ) ;
        ACTION  {|| _wPost(This.Cargo:nPost) } ;
        ON INIT {|o|                                     // т.к. задан This.Index параметр 2
                    This.Cargo := oHmgData()  // создать объект (контейнер) для этой кнопки
                    o := This.Cargo
                    // положим на кнопку нужные данные
                    o:nBtn    := nJ
                    o:nPost   := nwPost
                    o:cCapt   := cCapt
                    o:aIco    := aIco
                    o:aBClr1  := aColor
                    o:aBClr2  := BLACK
                    o:cObj    := This.Name
                    o:aGrFill := aGrFill
                    o:aGrOver := aGrOver
                    Return Nil
                 }          // ON INIT надо задавать только блоком кода
      nX += nWBtn + nG
   NEXT

RETURN nX

///////////////////////////////////////////////////////////////////////////////
FUNCTION myExit()
   LOCAL cFileLog := _SetGetLogFile()

   //ShellExecute(0,"Open",cFileLog,,,SW_SHOWNORMAL)
   // wApi_Sleep(200)
RETURN NIL

///////////////////////////////////////////////////////////////////////////////
FUNCTION myQuit(lQuit)
   LOCAL cMsg, nWin, lRet
   DEFAULT lQuit := .T.

   nWin := Len( HMG_GetForms("S") )
   cMsg := "Необходимо закрыть все другие окна программы !;;"
   cMsg += "All other program windows must be closed !"

   IF lQuit
      // НЕТ выхода, пока есть окна STANDARD
      lRet := IIF( nWin == 0, .T., ( MG_Exclam(cMsg, "Attention"), .F. ) )
   ELSE
      lRet := .T.  // всегда выход - это для отладки
   ENDIF

RETURN lRet

///////////////////////////////////////////////////////////////////////////////
// Здесь можно вводить свои переменные для своих проектов
FUNCTION LoadPublicValueThisProject()
   LOCAL cPath, cMsg, cFileLog := _SetGetLogFile()

   // назначение глобальных переменных - по новому ! Рекомендуемый вариант
   // пример для создания и использования объектов.  !!! после команды SET OOP ON
   App.Cargo := oHmgData()  // создать объект (контейнер) для глоб.переменной
   App.Cargo:cFileCfg      := ChangeFileExt( App.ExeName, '.cfg' )
   App.Cargo:cPathUserTemp := GetUserTempFolder()            // путь к временной папке для мусора
   App.Cargo:cPathTemp     := GetStartUpFolder() + "\TEMP\"  // путь к моей папке для мусора
   App.Cargo:cPathDbf      := GetStartUpFolder() + "\DBF\"   // путь к моим базам
   App.Cargo:nUser         := 101                            // код оператора в программе
   App.Cargo:cUser         := "User Admin"                   // имя оператора в программе
   App.Cargo:nUGrp         := 11                             // код группы оператора в программе

   cPath := App.Cargo:cPathTemp
   cMsg  := "I can not create a temporary folder for work !;;"
   cMsg  += cPath + ";;"
   cMsg  += "ERROR ! DOS(" + HB_NtoS(DosError()) + ");;"
   cMsg  += ProcNL(0)
   /*IF !ISDIRECTORY( cPath )
      CreateFolder( cPath )
      IF !ISDIRECTORY( cPath )
         MG_Stop( cMsg, "Creation error" )
         cMsg := AtRepl( ";", cMsg, CRLF )
         STRFILE( cMsg + CRLF, cFileLog, .T. )
      ENDIF
   ENDIF*/

RETURN NIL

////////////////////////////////////////////////////////////////////////////////
FUNCTION myTsbFont( lCreate, nFSDef )
   LOCAL aFont := {}, cFont
   DEFAULT nFSDef := _HMG_DefaultFontSize

   // создаем массив имен фонтов для режимов или убираем их
   AAdd( aFont, "TsbNorm"     )   // 1 - Cells
   AAdd( aFont, "TsbBold"     )   // 2 - Headers
   AAdd( aFont, "TsbBold"     )   // 3 = Footers
   AAdd( aFont, "TsbSpecH"    )   // 4 - SpecHeader
   AAdd( aFont, "TsbSuperH"   )   // 5 - SuperHeader
   AAdd( aFont, "TsbEdit"     )   // 6 - Edit
   AAdd( aFont, "TsbBoldMini" )   // мой фонт для Footers
   AAdd( aFont, "TsbOneCol"   )   // мой фонт для первой колонки

   IF pCount() > 0
      IF empty(lCreate)
         FOR EACH cFont IN aFont ; _ReleaseFont( cFont )
         NEXT
      ELSE
         DEFINE FONT TsbNorm   FONTNAME "DejaVu Sans Mono"   SIZE nFSDef
         DEFINE FONT TsbBold   FONTNAME "Comic Sans MS"      SIZE nFSDef + 2 BOLD
         DEFINE FONT TsbSpecH  FONTNAME "Tahoma"             SIZE nFSDef     BOLD
         DEFINE FONT TsbSuperH FONTNAME _HMG_DefaultFontName SIZE nFSDef     BOLD
         DEFINE FONT TsbEdit   FONTNAME "DejaVu Sans Mono"   SIZE nFSDef     BOLD
         DEFINE FONT TsbBoldMini FONTNAME "DejaVu Sans"      SIZE nFSDef - 4 BOLD
         DEFINE FONT TsbOneCol   FONTNAME "Arial Black"      SIZE nFSDef
      ENDIF
   ENDIF

RETURN aFont

//////////////////////////////////////////////////////////////////////////////////
* Copyright 2013 Grigory Filatov <gfilatov@inbox.ru>
//////////////////////////////////////////////////////////////////////////////////
FUNCTION Label_Init(cForm, cNameLbl, nDelta)
   LOCAL nRow, nCol, nWidth, nHeight, cVal, cFont, nFSize, lFBold, nWTxt, nX
   LOCAL aFColor, aBColor, nWSmbl, nWMax, aRet, i, nL, aObjLbl, aSymbol, cN, nX1
   DEFAULT nDelta := 0

   cVal    := GetProperty(cForm, cNameLbl, "Value"    )
   nFSize  := GetProperty(cForm, cNameLbl, "Fontsize" )
   cFont   := GetProperty(cForm, cNameLbl, "Fontname" )
   lFBold  := GetProperty(cForm, cNameLbl, "Fontbold" )
   aFColor := GetProperty(cForm, cNameLbl, "Fontcolor")
   aBColor := GetProperty(cForm, cNameLbl, "Backcolor")
   nRow    := GetProperty(cForm, cNameLbl, "Row"      )
   nCol    := GetProperty(cForm, cNameLbl, "Col"      )
   nWidth  := GetProperty(cForm, cNameLbl, "Width"    )
   nHeight := GetProperty(cForm, cNameLbl, "Height"   )
   SetProperty(cForm, cNameLbl, "Visible", .F. )

   // подсчёт начала установки букв
   nWTxt   := GetTxtWidth( cVal, nFSize, cFont, lFBold )
   nX1     := ( nWidth - nWTxt - nDelta*LEN(cVal) ) / 2

   nWSmbl  := nWMax := 0
   nL      := LEN( cVal )
   aSymbol := ARRAY(nL)
   aObjLbl := ARRAY(nL)
   FOR i := 1 TO nL
      aObjLbl[i] := cNameLbl + "_" + strzero(i,2)
      aSymbol[i] := SUBSTR( cVal, i, 1 )
      nWSmbl     := GetTxtWidth( aSymbol[i], nFSize, cFont, lFBold )
      nWMax      := MAX(nWMax, nWSmbl)
   NEXT
   nWSmbl := nWMax      // макс.ширина буквы

   nX := nCol + nX1     // начало позиции вывода букв
   FOR i := 1 TO nL

      cVal    := aSymbol[i]
      cN      := aObjLbl[i]
      nWSmbl  := GetTxtWidth( cVal, nFSize, cFont, lFBold )

      @ nRow, nX LABEL &cN VALUE '' WIDTH nWSmbl HEIGHT nHeight ;
        FONTCOLOR aFColor BACKCOLOR aBColor FONT cFont SIZE nFSize TRANSPARENT CENTERALIGN VCENTERALIGN

      This.&(cN).Value := cVal  // !!! заполняем значением контрол
      DO EVENTS

      SetProperty(cForm, aObjLbl[i], "Fontbold" , lFBold )
      DO EVENTS

      nX += nWSmbl + nDelta

   NEXT

   aRet := aObjLbl

Return aRet

//////////////////////////////////////////////////////////////////////////////////
FUNCTION Label_Left2Right(nEvent, lRandom)
   LOCAL cForm, aObjLbl, k, cTimer, cLabel, aFlicker, aFlickLbl
   LOCAL o := oMain:Cargo                   // считаем из объекта-контейнера
   //LOCAL o := ThisWindow.Cargo            // считаем из объекта-контейнера
   DEFAULT lRandom := .F.

   IF nEvent == 90                           // резерв
      aFlicker  := M->oMain:Cargo:aFlicker   // можно и так считать из объекта-контейнера
      aFlickLbl := M->oMain:Cargo:aFlickLbl
   ENDIF

   // считаем переменные из контейнера
   cForm   := o:aFlicker[1]
   cTimer  := o:aFlicker[2]
   cLabel  := o:aFlicker[3]
   aObjLbl := o:aFlickLbl

   IF lRandom
      nStatR := Random(255) ; nStatG := Random(255) ; nStatB := Random(255)
   ENDIF

   FOR k := 1 TO LEN(aObjLbl)
      nStatR += 50
      nStatG += 100
      nStatB += 150
      SetProperty ( cForm, aObjLbl[k], "FontColor" , { nStatR, nStatG, nStatB } ) ; DO EVENTS
      IF nStatR > 255
         nStatR := iif( lRandom, Random(255), 50 )
      ENDIF
      IF nStatG > 255
         nStatG := iif( lRandom, Random(255), 50 )
      ENDIF
      IF nStatB > 255
         nStatB := iif( lRandom, Random(255), 50 )
      ENDIF
   NEXT

Return NIL
