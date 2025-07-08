/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2021 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2021 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Универсальный модуль обработки нескольких таблиц
*/

//#define  _HMG_OUTLOG
#include "hmg.ch"

MEMVAR oMain
///////////////////////////////////////////////////////////////////////////////
FUNCTION Lang_Table( nTable )

   DO EVENTS

   IF Empty(nTable) .or. nTable == 1
      SET CODEPAGE TO ENGLISH       // аналог HB_CDPSELECT( "EN" )
      SET LANGUAGE TO ENGLISH       // аналог HB_LANGSELECT( "EN" )
   ELSEIF nTable == 2
      SET CODEPAGE TO UKRAINIAN     // аналог hb_SetCodepage( "UA1251" )
      SET LANGUAGE TO UKRAINIAN     // аналог hb_CdpSelect( "UA1251" )
   ELSEIF nTable == 3
      SET LANGUAGE TO BYELORUSSIAN
   ELSEIF nTable == 4
      SET CODEPAGE TO RUSSIAN
      SET LANGUAGE TO RUSSIAN
   ELSEIF nTable == 5
      SET CODEPAGE TO RUSSIAN
      SET LANGUAGE TO RUSSIAN
   ENDIF

   DO EVENTS

RETURN Nil

///////////////////////////////////////////////////////////////////////////////
FUNCTION Show_Table( nTable )
   LOCAL om := M->oMain:Cargo
   LOCAL cForm := om:aFormData[nTable]   // имя окна
   LOCAL cTabl := subs(cForm, 2)         // имя TBrowse
   LOCAL cIco, cTitle, c2Title, nW, nH, nY, nX, nG, nR, cAls
   LOCAL cFont, nFSize, hWnd, nHMain, nHBrd, oThis, aBackColor, aUse3Dim
   LOCAL aBtn4Dim, nWBtn, nHBtn, nKBtn, nHIco, nYBrw, nXBrw, nWBrw, nHBrw
   LOCAL oBrw   // для каждой таблицы будет свой oBrw

   ? "======", cForm, cTabl, ProcNL()
   IF !_IsWindowActive( cForm )

      Lang_Table( nTable )

      ?  "    LANG=" , hb_langSelect() + " -/- " + hb_langName()
      nHMain     := oMain:Height
      nW         := System.ClientWidth
      nHBrd      := GetBorderHeight()
      nH         := System.ClientHeight - nHMain - nHBrd
      nR         := nHMain + nHBrd
      cIco       := "2MAIN_ICO"
      cFont      := "Tahoma"
      nFSize     := ModeSizeFont()
      cTitle     := cForm + " (" + HB_NtoS(nTable) + ")"
      c2Title    := "Table: " + cTabl + " for form: " + cForm
      // данные по форме -> ListTables.prg
      aBackColor := myTableBackColor(nTable)          // цвет формы
      aBtn4Dim   := myTableButtonUp(nTable)           // массив кнопок верха формы
      // данные по таблице -> ListTables.prg
      aUse3Dim   := myTableUse(nTable)                // массив базы dbf/alias/codepage

      IF Empty( cAls := Use_Table(aUse3Dim) ) ; RETURN .F.
      ENDIF

      DEFINE WINDOW &cForm                           ;
         At nR, 0 WIDTH nW HEIGHT nH                 ;
         TITLE cTitle  ICON  cIco                    ;
         WINDOWTYPE STANDARD TOPMOST                 ;
         NOMAXIMIZE NOSIZE                           ;
         BACKCOLOR aBackColor                        ;
         ON GOTFOCUS Lang_Table( This.Cargo:nTable ) ;  // возврат фокуса на форму
         ON INIT     {|| DoEvents(), _wPost(1)   }   ;  // выполняется после инициализации окна
         ON RELEASE  {|| (cAls)->(dbCloseArea()) }      // выполняется перед разрушением окна

         oThis             := This.Object
         This.Cargo        := oHmgData()                // контейнер для ЭТОГО окна
         This.Cargo:nTable := nTable
         This.Cargo:cForm  := cForm

         nW    := This.ClientWidth
         nH    := This.ClientHeight
         hWnd  := ThisWindow.Handle
         nY    := nX := nG := 20
         nHIco := 64                                  // размер иконки
         nKBtn := LEN(aBtn4Dim[1])                    // кол-во кнопок
         nHBtn := nHIco + 10                          // высота кнопки
         nWBtn := ( nW - nG*(nKBtn+1) ) / nKBtn       // ширина кнопки

         @ 0, 10 LABEL Buff WIDTH nW HEIGHT 10 VALUE 'переключение фокуса'  INVISIBLE

         Button_UpMenu(nY,nX,nG,nHIco,aBtn4Dim,nWBtn,nHBtn)  // положить меню кнопок на форму

         nXBrw := nG
         nWBrw := nW - nXBrw * 2
         nYBrw := nG * 2 + nHBtn
         nHBrw := nH - nYBrw - nG

         // тестовый объект
         /*c2Title += " Alias: " + cAls
         @ nYBrw, nXBrw LABEL Label_Tsb WIDTH nWBrw HEIGHT nHBrw VALUE c2Title ;
           FONTCOLOR WHITE VCENTERALIGN CENTERALIGN TRANSPARENT BORDER
         // Функция заменит на максимальный размер фонта
         SetFontSizeText(ThisWindow.Name, "Label_Tsb") */

         // таблица -> form_tbrowse.prg
         oBrw := TBrowse_Create( nTable, cTabl, cForm, cAls, c2Title, nYBrw, nXBrw, nWBrw, nHBrw )

         oBrw:Cargo:nHMain := nR     // высота главного окна
         This.Cargo:oBrw   := oBrw   // запомнить таблицу

         // События для объектов окна / Events for window objects
         ThisEventsWindowObjects()

         ON KEY F1     ACTION NIL
         ON KEY ESCAPE ACTION NIL

      END WINDOW

      //CENTER   WINDOW &cForm
      ACTIVATE WINDOW &cForm ON INIT {|| This.Minimize, wApi_Sleep(50), ;
                                         This.Restore , DoEvents() }

      DO EVENTS ; _wPost( 101, oMain:Name )

   ELSE

      SwitchToWin( cForm )  // переключить на тек.форму

   ENDIF

RETURN NIL

///////////////////////////////////////////////////////////////////////////////
// События для объектов окна / Events for window objects
STATIC FUNCTION ThisEventsWindowObjects()

   // номера событий на кнопки берём из ListTables.prg
   // aBtnPst := { 10, 11, 12, 13, 14, 90 } // _wPost(Х) - номер события на кнопке
   // aBtnCap := { "Помощь", "Карточка", "Поиск", "Печать", "Настройки", "Выход" }

   WITH OBJECT This.Object

      :Event(  0, {|ow| AlertInfo(":Event(0)",ow:Name), DoEvents() })
      :Event(  1, {|ow| // при ON INIT этого окна
                        Local om := oMain:Cargo                          // MAIN окно
                        Local oBrw := ow:Cargo:oBrw
                        //? ProcNL(), "ow:Name=", ow:Name
                        This.Topmost := .F.
                        //ow:SetFocus("Buff")
                        oBrw:Setfocus()     // фокус на таблицу
                        DO EVENTS
                        IF om:nStartWindow > 0 ; _wPost(100, oMain:Name)  // MAIN окно
                        ENDIF
                        Return Nil
                  })

      :Event( 10, {|ow,ky,cp| TestButton( ow,ky,cp )     } )  // Помощь

      :Event( 11, {|| This.Enabled := .F., _wPost(111, , This.Name) } )  // Карточка
      :Event(111, {|ow,ky,cn| // Карточка - продолжение
                        Local oBrw := ow:Cargo:oBrw
                        Local hWnd := ow:Handle       //!!!
                        //MG_Debug(ow:Name,ky,cn)
                        ky := cn // убрать ошибку компилятора, т.к. ky нигде больше не используется
                        SET WINDOW THIS TO ow:Name    // save This среду окна ow
                        Darken2Open(hWnd)             // затенение на форме
                        Show_Card(oBrw)               // -> form_card.prg
                        ? "-> Show_Card(oBrw) end", ow:Name, hWnd
                        Darken2Close(hWnd)            // убрать затенение на форме
                        ? "-> Darken2Close(ky) end", ow:Name, hWnd
                        SET WINDOW THIS TO            // restore This среду окна ow
                        //This.&(cn).Enabled := .T.
                        SetProperty(ow:Name, cn, 'Enabled', .T.)
                        //ow:SetFocus('Buff')
                        oBrw:SetFocus()         // фокус на таблицу
                        Return Nil
                  })

      :Event( 12, {|ow,ky,cp| TestButton( ow,ky,cp )    } ) // Поиск
      :Event( 13, {|ow,ky,cp| TestButton( ow,ky,cp )    } ) // Печать
      :Event( 14, {|ow,ky,cp| TestButton( ow,ky,cp )    } ) // Настройки
      :Event( 50, {|ow,ky,cp| TestButton( ow,ky,cp )    } ) // Расчёт
      :Event( 51, {|ow,ky,cp| TestButton( ow,ky,cp )    } ) // Меню 1
      :Event( 52, {|ow,ky,cp| TestButton( ow,ky,cp )    } ) // Меню 2

      // событие 70 зарезервировано для контекстного меню таблицы -> form_tbrowse.prg
      //:Event( 70, {|ow,p1,p2| MG_Debug("ow:Name=",ow:Name,p1,p2), DoEvents() })
      // событие 80 зарезервировано для контекстного меню таблицы -> form_tbrowse.prg
      //:Event( 80, {|ow,p1,p2| MG_Debug("ow:Name=",ow:Name,p1,p2), DoEvents() })

      :Event( 90, {|  | _wPost(99, , This.Name)         } ) // выход из окна таблицы
      :Event( 99, {|ow| ow:Release()                    } ) // выход - продолжение
   END WITH

RETURN NIL

/////////////////////////////////////////////////////////////////////////////////
FUNCTION TestButton(ow,ky,cp)    // для тестирования нажатия кнопки
   LOCAL oWnd, hHandle, oCtl, cObj, oCargo, oBrw, a2Dim, nI, cTxt, cMsg
   LOCAL cForm := ThisWindow.Name

   DO EVENTS ; hb_gcAll() ; DO EVENTS        // запустить "сборщик мусора"

   ?  "--------- " + ProcNL() + "(", ow,ky,cp ,")"
   oWnd    := ThisWindow.Object  // объект окна
   oCtl    := This.Object        // объект контола кнопки
   hHandle := ThisWindow.Handle

   //------------------------------------------------------------------------
   ? 'Form   :', oWnd:Type, oWnd:Name, oWnd:Handle, oWnd:Index, ;
                 oWnd:IsWindow, oWnd:IsControl, ;
                 oWnd:Row , oWnd:Col , oWnd:Width , oWnd:Height, ;
                 oWnd:ClientWidth , oWnd:ClientHeight
   // методы окна
   // oWnd:Hide(), oWnd:Show(), oWnd:Restore(), oWnd:SetFocus(ControlName)
   // oWnd:SetSize(y,x,w,h), oWnd:GetObj(ControlName), oWnd:Release()

   ? 'Контрол:', oCtl:Type, oCtl:Name, oCtl:Handle, oCtl:Index, ;
                 oCtl:IsWindow, oCtl:IsControl, ;
                 oCtl:Row , oCtl:Col , oCtl:Width , oCtl:Height, ;
                 oCtl:ClientWidth , oCtl:ClientHeight
   // методы контрола
   // xVal := oCtl:Value
   // oCtl:Value := xVal
   // oCtl:Hide(), oCtl:Show(), oCtl:SetSize(y,x,w,h), oCtl:SetFocus()
   // oCtl:Refresh(), oCtl:Disable(), oCtl:Enable()
   // oGet := oCtl:Get - для GetBox
   // oBrw := oCtl:Tsb - для TsBrowse
   // oBrw:Refresh()
   // oBrw:... и т.л.
   //------------------------------------------------------------------------
   ? Repl("-",60)
   ?  " _HMG_ThisFormIndex  <окно index>               :=",  _HMG_ThisFormIndex
   ?  " _HMG_ThisEventType                             :=",  _HMG_ThisEventType
   IF _HMG_ThisType == 'W'
   ?  " _HMG_ThisType       'W' // 'W[indow]'  признак :=",  _HMG_ThisType
   ?  "    событие - _wPost(This.Cargo:nPost, This.Name)"
   ELSE
   ?  " _HMG_ThisType       'C' // 'C[ontrol]' признак :=",  _HMG_ThisType
   ?  "    событие - _wPost(This.Cargo:nPost, This.Index)"
   ENDIF
   ?  " _HMG_ThisIndex      <окно index>               :=",  _HMG_ThisIndex
   ?  " _HMG_ThisFormName   <окно name>                :=",  _HMG_ThisFormName
   ?  " _HMG_ThisControlName                           :=",  _HMG_ThisControlName
   ? Repl("-",60)

   cObj  := ow:Name
   ?  "cObj= (ow:Name)=",ow:Name
   ?  "oWnd:Name=", oWnd:Name

   oCargo  := This.Cargo       // берём данные с кнопки то что положили ранее

   // получить с ключём массив
   a2Dim := oCargo:GetAll(.F.)
   cTxt  := ""
   // это список ВСЕХ ключей и их значений
   FOR nI := 1 TO Len(a2Dim)
      ? "  oCargo:", nI, "Key =", a2Dim[nI][1], "Val =", a2Dim[nI][2]
      cTxt += "  oCargo:" + STR(nI,2) + ") Key = " + a2Dim[nI][1]
      cTxt += ", Val = " + cValToChar(a2Dim[nI][2])
      IF VALTYPE(a2Dim[nI][2]) == "A"
         ?? HB_ValToExp(a2Dim[nI][2])
         cTxt += HB_ValToExp(a2Dim[nI][2])
      ENDIF
      cTxt += CRLF
   NEXT
   ? Repl("-",60)

   SET WINDOW THIS TO oWnd:Name    // save This среду окна ow
       oBrw := oWnd:Cargo:oBrw                    // получить данные из объекта
       //oBrw := (ThisWindow.Object):Cargo:oBrw   // получить данные из объекта
   SET WINDOW THIS TO              // restore This среду окна ow
   ?  "oBrw = ",oBrw
   ??  oBrw:cAlias

   cMsg := ProcNL() + ";" + Repl("-",40) + ";"
   cMsg += "Was the language   = " + hb_cdpSelect() + ";"
   cMsg += "Current LangSelect = " + hb_langSelect() + ";"
   cMsg += "   " + hb_langName() + ";"
   cMsg += Repl("-",40) + ";"
   cMsg += "oWnd:Name="+oWnd:Name+", ow:Name=" + ow:Name + ";;"
   cMsg += cTxt + ";" + Repl("-",40) + ";"
#ifdef _HMG_OUTLOG
   cMsg += "ОБЪЕКТ oBrw - доступен ! oBrw:cAlias = " + oBrw:cAlias
   cMsg += ";значит доступны все переменные;"
   cMsg += "aHeader := oBrw:Cargo:aHeader // список названий полей в карточку;"
   cMsg += "aField  := oBrw:Cargo:aField  // список наименований колонок таблицы;"
   cMsg += "aEdit   := oBrw:Cargo:aEdit   // массив данных для редактирования колонок;"
   cMsg += "и все другие заданные в oBrw:Cargo:ХХХХХ !;"
   cMsg += Repl("-",40) + ";;"
   cMsg += "Для проверки доступа пользователя к тем или иным кнопкам:"
   cMsg += ";код оператора в программе        = " + HB_NtoS( App.Cargo:nUser )
   cMsg += ";имя оператора в программе        = " + App.Cargo:cUser
   cMsg += ";код группы оператора в программе = " + HB_NtoS( App.Cargo:nUGrp )
#endif
   MG_Info(cMsg)

   This.&(oCargo:cObj).Enabled := .T.
   //SetProperty(oWnd:Name, cObj, "Enabled", .T.) - можно и так

   DO EVENTS ; hb_gcAll() ; DO EVENTS ; wApi_Sleep()       //  запустить "сборщик мусора"

   oWnd:SetFocus('Buff')
   oBrw:Setfocus()   // вернуть фокус на таблицу

RETURN NIL
/////////////////////////////////////////////////////////////////////////////////
// записать в массив названия,цвет,иконки и т.д.
FUNCTION Button_UpMenu(nY,nX,nG,nIcoSize,aBtn4Dim,nWBtn,nHBtn)
   LOCAL cForm, aBtnCap, aBtnIco, aBtnClr, aBtnPst, cCapt, aIco
   LOCAL cN, cFont, nFSize, lFBold, nJ, aColor, aBtnGrd, aGrOver, aGrFill
   LOCAL aFntClr, nwPost, hIco, aBtnObj, nFSMin, nFH

   aBtnObj := {}
   cForm   := ThisWindow.Name
   // кнопки
   aBtnCap := aBtn4Dim[1]
   aBtnIco := aBtn4Dim[2]
   aBtnClr := aBtn4Dim[3]
   aBtnPst := aBtn4Dim[4]
   // фонты для кнопок
   aFntClr := { BLACK , OLIVE }
   cFont   := "Comic Sans MS"
   nFSize  := 16
   lFBold  := .T.
   nFSMin  := 72

   FOR nJ := 1 TO LEN(aBtnCap)
      cCapt   := StrTran( aBtnCap[nJ], ";" , CRLF )
      aIco    := aBtnIco[nJ]
      aColor  := aBtnClr[nJ]
      aBtnGrd := { HMG_RGB2n( aColor ), CLR_WHITE }  // градиент кнопки
      aGrOver := { { 0.5, aBtnGrd[2], aBtnGrd[1] }, { 0.5, aBtnGrd[1], aBtnGrd[2] } }
      aGrFill := { { 0.5, aBtnGrd[1], aBtnGrd[2] }, { 0.5, aBtnGrd[2], aBtnGrd[1] } }
      nwPost  := aBtnPst[nJ]
      hIco    := LoadIconByName( aIco[1], nIcoSize, nIcoSize )
      cN      := 'Btn_' + StrZero(nJ, 2)
      AADD(aBtnObj,cN)

      @ nY, nX BUTTONEX &cN WIDTH nWBtn HEIGHT nHBtn                            ;
        CAPTION cCapt ICON hIco                                                 ;
        FLAT NOXPSTYLE HANDCURSOR NOTABSTOP LEFTTEXT                            ;
        FONT cFont SIZE nFSize BOLD FONTCOLOR aFntClr[1]                        ;
        BACKCOLOR aGrOver GRADIENTFILL aGrFill                                  ;
        ON MOUSEHOVER ( This.Fontcolor := aFntClr[2]                                      ,;
                        This.Icon := LoadIconByName(This.Cargo:aIco[2],nIcoSize,nIcoSize) ,;
                        This.GradientFill := This.Cargo:aGrFill     )            ;
        ON MOUSELEAVE ( This.Fontcolor := aFntClr[1]                                      ,;
                        This.Icon := LoadIconByName(This.Cargo:aIco[1],nIcoSize,nIcoSize) ,;
                        This.GradientOver := This.Cargo:aGrOver     )            ;
        ACTION  {| | This.Enabled := .F., _wPost(This.Cargo:nPost, This.Index) } ; // в событии будет среда This для кнопки,
        ON INIT {|o|                                                               // т.к. задан This.Index параметр 2
                     This.Cargo := oKeyData()  // создать объект (контейнер) для этой кнопки
                     o := This.Cargo
                     // положим на кнопку нужные данные
                     o:nBtn    := nJ
                     o:nPost   := nwPost
                     o:cCapt   := cCapt
                     o:aIco    := aIco
                     o:aBClr   := aColor
                     o:cObj    := This.Name
                     o:aGrFill := aGrFill
                     o:aGrOver := aGrOver
                     Return Nil
                 }          // ON INIT надо задавать только блоком кода

      // при первом построении изменить размеры иконки
      This.&(cN).ImageWidth  := nIcoSize
      This.&(cN).ImageHeight := nIcoSize
      This.&(cN).Icon        := LoadIconByName( aIco[1], nIcoSize, nIcoSize )

      // максимальный размер фонта на кнопке + подсчет мин.значения
      SetFontSizeTextMax(cForm, cN, 8 + nIcoSize , 8 )
                        // 3-параметр: уменьшить на ширину картинки + 8 пикселей
                        // 4-параметр: уменьшить на высоту 8 пикселей
      nFH    := This.&(cN).Fontsize
      nFSMin := MIN(nFSMin,nFH)

      // сдвиг кнопки
      nX += nWBtn + nG
   NEXT

   // установить на кнопки минимальный ОДИНАКОВЫЙ размер фонта
   FOR nJ := 1 TO LEN(aBtnObj)
      cN  := aBtnObj[nJ]
      This.&(cN).Fontsize := nFSMin
   NEXT


RETURN NIL

///////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Use_Table( a3Dim, lMsg )
   LOCAL lUsed, cAls, cDbf, cCDP
   DEFAULT lMsg := .T.

   cDbf := App.Cargo:cPathDbf + a3Dim[1]
   cAls := a3Dim[2]
   cCDP := a3Dim[3]

   IF !FILE( cDbf )
      cAls := ""
      IF lMsg
         MG_Stop("Error ! No such file !;;"+cDbf, "File open error")
      ENDIF
   ELSE
      USE (cDbf) ALIAS (cAls) CODEPAGE (cCDP) SHARED  NEW
      lUsed := Used()
      cAls  := Alias()
      IF lMsg .and. ! lUsed
         cAls := ""
         MG_Stop("Error ! File not used !;;"+cDbf, "File open error")
      ENDIF
//      IF cAls == "STREET_UA"
//         Ukr_Check(cAls)
//         Go TOP
//      ENDIF
   ENDIF

RETURN cAls

