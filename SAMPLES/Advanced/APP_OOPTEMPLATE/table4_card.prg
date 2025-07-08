/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2023 Verchenko Andrey <verchenkoag@gmail.com>
 *
*/
#define _HMG_OUTLOG

#include "minigui.ch"
#include "metrocolor.ch"

//////////////////////////////////////////////////////////////////////
FUNCTION myTable4Card(oWnd, nKy, cObj, oBrw)
   LOCAL hWin, oWin, nH, nW, nG, nWBtn, nHBtn, cIco, cTitle
   LOCAL cFont, nFSize, cBFont, nBFSize, nHIco, nCol, nRow, cFrm
   LOCAL aBackColor, aBackClr2, aBtnFC, cTxt, aBtn, bAct, nX, nY
   LOCAL aHide, aForm, cFormCurr, cFormMain, lVsbl, nI, aXFont
   LOCAL cForm, a4Brw, aBackClr3, nHVirt, nGroup, aRet, nW2, nH2
   LOCAL nIcoSize, nHUp, nWTxt, nIniFSize
                       
   ? "-->> Start Form_Card", ProcNL(), oWnd:Name, nKy, cObj

   cIco       := "iDbInfo64x1"
   cTitle     := 'Карточка одной записи / Single entry card - MODAL'
   cFont      := App.Cargo:cDefFontName
   nFSize     := App.Cargo:nDefFontSize
   cBFont     := App.Cargo:cBtnFontName     // шрифт для кнопок
   nBFSize    := App.Cargo:nBtnFontSize + 2
   cFormMain  := App.Cargo:cWinMain         // имя окна MAIN формы
   aBtnFC     := BLACK                      // цвет инверт.фонта кнопок
   aBackColor := COLOR_AZURE3               // Цвет фона всей формы
   aBackClr2  := COLOR_BLUE_SKYPE /*COLOR_DARK_PURPLE*/    // Цвет фона вверху формы
   aBackClr3  := COLOR_AZURE3               // Цвет панели внутри формы
   nW         := App.Cargo:aDisplayMode[1]  //System.ClientWidth
   nH         := App.Cargo:aDisplayMode[2]  //System.ClientHeight
   nH         -= GetTaskBarHeight()         // высота Панели задач Desktop
   nG         := 10                         // отступ
   nY         := nX := 0
   nX         := nG*6
   cFormCurr  := ThisWindow.Name            // текущая форма
   cForm      := "Form_Card"                // новая форма
   aXFont     := FontCardLoad(cForm)        // загрузить фонты для карточки 
   a4Brw      := CardListField(oBrw)        // получить наименование и поля базы
   nIcoSize   := myScreenIconSize(App.Cargo:aDisplayMode[2])  // высота иконки от экрана
   nHBtn      := nIcoSize + 3*2             // высота кнопок вверху
   nHUp       := nHBtn + 5*2                // высота верха части окна 

   // скрыть все окна кроме текущего, пропуская скрытые окна
   aHide := {}
   aForm := HMG_GetForms()
   FOR nI := 1 TO Len(aForm)
      lVsbl := IsWindowVisible( GetFormHandle( aForm[nI] ) )
      IF aForm[nI] == cFormMain     ; LOOP
      ELSEIF aForm[nI] == cFormCurr ; LOOP
      ELSEIF !lVsbl                 ; LOOP
      ENDIF
      DoMethod(aForm[nI], "Hide")
      AADD( aHide, aForm[nI] )
   NEXT

   DEFINE WINDOW &cForm                                                ;
      At nY, nX WIDTH nW-nX HEIGHT nH                                  ;
      TITLE cTitle ICON cIco                                           ;
      MODAL NOSIZE                                                     ;
      BACKCOLOR aBackColor                                             ;
      FONT cFont SIZE nFSize                                           ;
      ON GOTFOCUS {|| App.Cargo:cFormGotFocus := This.Name }           ;
      ON INIT     {|| This.Topmost := .F., DoEvents(), _wPost(0) }     ;
      ON RELEASE  {|| AEval({91,92,93}, {|n| _wSend(n), DoEvents()}) } ; // executed before destroying the window

      nIniFSize  := IniGetCardFont(cForm)          // чтение параметров из Demo_timer.ini
      This.Cargo := oHmgData()
      This.Cargo:aFont  := aXFont                  // положить список фонтов для _wSend(91)
      This.Cargo:cForm  := cForm                   // имя этой формы - в качестве примера
      This.Cargo:aFor   := { 10 , 26 }             // присвоить список размеров высоты фонта
      This.Cargo:nSize  := nIniFSize               // чтение параметров из Demo_timer.ini
      This.Cargo:nSize0 := nIniFSize               // первоначальный размер фонта 

      nW   := This.ClientWidth
      nH   := This.ClientHeight
      oWin := This.Object
      cFrm := oWin:Name
      hWin := oWin:Handle

      @ 0, 0 LABEL Label_Buff WIDTH nW HEIGHT nHUp VALUE "" BACKCOLOR aBackClr2

      @ 1, nG LABEL Label_Title WIDTH nW HEIGHT nHUp-2 VALUE cTitle FONT cBFont SIZE nBFSize ;
        FONTCOLOR YELLOW BACKCOLOR aBackClr2  CENTERALIGN VCENTERALIGN   

      nY     += This.Label_Buff.Height + nG
      nX     := nG
      nW2    := nW - nX - nG
      nH2    := nH - nY - nG
      nGroup := 2
      // подсчёт виртуальной высоты панели, высота зависит от высоты шрифта и кол-ва строк вывода
      nHVirt := mySayCardDatos(.f., nGroup, oWin:Cargo, a4Brw, nY, nX, nG, nW2, nH2) 
      ? ProcNL(), nH2, nHVirt

      IF nHVirt <= nH2
         mySayCardDatos(.T., nGroup, oWin:Cargo, a4Brw, nY, nX, nG, nW2, nH2)
      ELSE
         // панель окна
         DEFINE WINDOW Win_2                   ;
            ROW nY COL nX WIDTH nW2 HEIGHT nH2 ;
            VIRTUAL HEIGHT nHVirt              ;
            BACKCOLOR aBackClr3                ;
            WINDOWTYPE PANEL                  
            //VIRTUAL WIDTH nW2   ;  // резерв

            nY  := nX := 0
            mySayCardDatos(.T., nGroup, oWin:Cargo, a4Brw, nY, nX, nG, nW2, nH2)

         END WINDOW
      ENDIF

      /////////////////////// Button ///////////////////////////////////////////
      //nWBtn := 220                         // ширина кнопок вверху
      nWTxt := GetTxtWidth( "0Config0", nBFSize, cBFont, .F. )  // ширина текста
      nHIco := nIcoSize                      // высота иконки на кнопках вверху
      nWBtn := nIcoSize + 5*2 + nWTxt        // ширина кнопок вверху
      ? ProcNL(), "nIcoSize=",nIcoSize,"nHBtn=", nHBtn,"nWBtn=", nWBtn
      nRow  := 5

      nCol  := nW - nWBtn*3 - nG*3 
      cTxt  := "Config" //+ CRLF + "this recno"
      aBtn  := { "Button_Config", cTxt, "iPiople64x1", "iPiople64x2", nHIco, aBtnFC, YELLOW, cBFont, nBFSize, .T. }
      bAct  := {|| /*MsgDebug(This.Cargo),*/ _wPost(80) }
      myDrawButtonGrad(nRow, nCol, nWBtn, nHBtn, aBtn, bAct, SILVER)

      nCol  := nW - nWBtn*2 - nG*2
      cTxt  := "Save" //+ CRLF + "this recno"
      aBtn  := { "Button_Save", cTxt, "iPiople64x1", "iPiople64x2", nHIco, aBtnFC, YELLOW, cBFont, nBFSize, .T. }
      bAct  := {|| /*MsgDebug(This.Cargo),*/ _wPost(90) }
      myDrawButtonGrad(nRow, nCol, nWBtn, nHBtn, aBtn, bAct, COLOR_GREEN_METRO)

      nCol := nW - nWBtn - nG
      cTxt := "Exit" //+ CRLF + "this card"
      aBtn := { "Button_Exit", cTxt, "iExit64x1", "iExit64x2", nHIco, aBtnFC, YELLOW, cBFont, nBFSize, .T. }
      bAct := {|| /*MsgDebug(This.Cargo),*/ _wPost(98) }
      myDrawButtonGrad(nRow, nCol, nWBtn, nHBtn, aBtn, bAct, COLOR_BRIGHT_RED)

      This.Label_Title.Width := This.Button_Config.Col - nG*2   // скорректируем ширину титула
      SetFontSizeText(cForm, "Label_Title")  // изменить размер фонта

      ON KEY ESCAPE OF &cForm ACTION _wPost(98)
      ON KEY F1     OF &cForm ACTION NIL

      WITH OBJECT This.Object
         :Event( 0, {|| InkeyGui(100), This.Label_Buff.Setfocus  } )

         :Event( 2, {|ow| ow:Setfocus('Label_Buff')              } )

         :Event(80, {|ow| // Button_Config
                          This.Button_Config.Enabled := .F.
                          // меню 
                          Card_Menu_Config(ow,"Button_Config")
                          // изменение параметров размеров фонта
                          IF ow:Cargo:nSize0 # ow:Cargo:nSize
                             AlertInfo("There have been changes in the card settings!;Restart the card !")
                             // запись параметров в Demo_timer.ini
                             IniSetCardFont(cForm,ow:Cargo:nSize)
                          ENDIF
                          This.Button_Config.Enabled := .T.
                          _wPost(2, ow)
                          RETURN NIL
                          } )

         :Event(90, {|ow| // Save
                          LOCAL nPost
                          This.Button_Save.Enabled := .F.
                          aRet := {} //Modal2(cFont, nFSize)
                          IF LEN(aRet) > 0
                             IF AlertYesNo("Вы хотите записать изменения в этой записи ?;" + ;
                                HB_ValToExp(aRet))
                                nPost := 99
                             ELSE
                                nPost := 2
                             ENDIF
                          ELSE
                             nPost := 99
                          ENDIF
                          _wPost(nPost, ow:Name)
                          RETURN NIL
                          } )

         :Event(91, {|ow,ky| // выгружаем/удаляем фонты карточки
                             ? "-->> :Event(91) _ReleaseFont:"
                             FOR EACH ky IN ow:Cargo:aFont
                                ?? ky + " ,"
                                _ReleaseFont(ky)
                             NEXT
                             Return Nil
                             } )
         :Event(92, {|ow| _LogFile(.T., "-->> :Event(92) ON RELEASE WINDOW: "+ow:Name ) } )
         :Event(93, {|ow| _LogFile(.T., "-->> :Event(93) ON RELEASE WINDOW: "+ow:Name ) } )

         :Event(98, {|ow| // Exit this menu
                          This.Button_Exit.Enabled := .F.
                          _LogFile(.T., ">>> :Event(98) Button - Exit")
                          aRet := {}
                          _wPost(99, ow:Name)
                          Return Nil
                          } )
         :Event(99, {|ow| ow:Release()  } )
      END WITH

   END WINDOW

   //CENTER WINDOW &cForm
   ACTIVATE WINDOW &cForm

   ? "-->> End " + cForm

   // восстановить скрытые окна / restore hidden windows
   FOR nI := 1 TO Len(aHide)
      IF _IsWindowDefined(aHide[nI])
         DoMethod(aHide[nI], "Show")
      ENDIF
   NEXT

   SwitchToWin( cFormCurr )     // переключить на тек.форму

   SET WINDOW THIS TO           // restore This среду окна

   ? "-->> Return Form_Card", "aRet=", aRet, HB_ValToExp(aRet)

RETURN aRet

//////////////////////////////////////////////////////////////////////
STATIC FUNCTION FontCardLoad(cForm)
   LOCAL cNam, aFnt, aSiz, aNam, cFnt, nSiz, cFont, nSize, aBld, aItl
   LOCAL nI, hFont, aFont, lBld, lItl

   cFont := App.Cargo:cDefFontName
   //nSize := App.Cargo:nDefFontSize

   // чтение параметров из Demo_timer.ini
   nSize := IniGetCardFont(cForm)

   cNam := "_"+cForm
   aFnt := {"Normal", "Bold" , "Italic", "ComSnMs"      , "SnapITC"  }
   aSiz := { nSize  ,  nSize , nSize   ,  nSize + 2     ,  nSize     }
   aNam := { cFont  ,  cFont , cFont   , "Comic Sans MS", "Snap ITC" }
   aBld := { .F.    ,  .T.   , .F.     , .F.            , .F.        }
   aItl := { .F.    ,  .F.   , .T.     , .F.            , .F.        }

   aFont := {}
   FOR nI := 1 TO LEN(aFnt)
      cFnt := aFnt[nI] + "_" + cForm
      cNam := aNam[nI]
      nSiz := aSiz[nI]
      lBld := aBld[nI]
      lItl := aItl[nI]
      //AADD( aFont, { cFnt, nSize, lBld, lItl } )
      AADD( aFont, cFnt )
      IF lItl
         DEFINE FONT &(cFnt) FONTNAME cNam SIZE nSiz ITALIC
      ELSE
         IF lBld
            DEFINE FONT &(cFnt) FONTNAME cNam SIZE nSiz BOLD
         ELSE
            DEFINE FONT &(cFnt) FONTNAME cNam SIZE nSiz 
         ENDIF
      ENDIF
      // или так
      hFont := GetFontHandle( cFnt )
      //IF hFont == 0
      //   _DefineFont( cFnt, cNam, nSiz, lBld, lItl, .F., .F.,, .F., )
      //ENDIF
   NEXT

   //AEval(aFnt, {|cn,ni| aFnt[ni] := cn+cNam })
   //FOR EACH cFnt, nSiz, cNam IN aFnt, aSiz, aNam
   //   DEFINE FONT &(cFnt) FONTNAME cNam SIZE nSiz
   //NEXT

   //или
   //DEFINE FONT &(aFnt[1]) FONTNAME cFont SIZE nSize
   //DEFINE FONT &(aFnt[2]) FONTNAME cFont SIZE nSize BOLD
   //DEFINE FONT &(aFnt[3]) FONTNAME cFont SIZE nSize ITALIC
   //DEFINE FONT &(aFnt[4]) FONTNAME "Comic Sans MS" SIZE nSize + 2
   //DEFINE FONT &(aFnt[5]) FONTNAME "Snap ITC" SIZE nSize 

RETURN aFont

//////////////////////////////////////////////////////////////////////
STATIC FUNCTION IniGetCardFont(cForm)
   LOCAL cSection, oIni, oSec, nSize

   nSize    := App.Cargo:nDefFontSize
   oIni     := App.Cargo:oIni   // считаем ini-файл из глобальной App.Cargo
   cSection := cForm + "/Настройки/Карточка"

   IF Empty( oSec := oIni:Get(cSection) )
      // Запись переменной в ини файл
      IniSetWrite(cSection,"Font_Size", nSize )
      IniSetWrite(cSection,"Font_Rem" , "высота фонта в карточке")
      IniWriteParam()   // Запись всего ини файла
   ENDIF

   nSize := GetIniData( oIni, cSection, "Font_Size", nSize )

RETURN nSize

//////////////////////////////////////////////////////////////////////
STATIC FUNCTION IniSetCardFont(cForm,nSize)
   LOCAL cSection, oIni, oSec

   oIni     := App.Cargo:oIni   // считаем ini-файл из глобальной App.Cargo
   cSection := cForm + "/Настройки/Карточка"

   IF Empty( oSec := oIni:Get(cSection) )
      // Запись переменной в ини файл
      IniSetWrite(cSection,"Font_Size", nSize )
      IniWriteParam()   // Запись всего ини файла
   ENDIF

RETURN NIL

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION CardListField(oBrw)   // получить наименование и поля базы
   LOCAL cMsg, a4Dim, oCol, cCol

   IF !hb_IsObject(oBrw)
      cMsg := "ERROR ! oBrw is not an object!;;"
      cMsg += ProcNL() + ";" + ProcNL(1)
      AlertStop(cMsg)
      RETURN {}
   ENDIF

   a4Dim := {}
   FOR EACH oCol IN oBrw:aColumns
      cCol := oCol:cName
      IF cCol == "ORDKEYNO" .OR. cCol == "SELECTOR"
      ELSE
         cMsg := oCol:cHeading
         cMsg := AtRepl( e"\r", cMsg, " " )
         AADD( a4Dim, { cMsg, cCol, oCol:cFieldTyp, oCol:cPicture } )
      ENDIF
   NEXT

   ? ProcNL(), "a4Dim=", a4Dim ;  ?v a4Dim

RETURN a4Dim

///////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION mySayCardDatos(lSay, nGroup, oCargo, a4Brw, nY, nX, nG, nW2, nH2)
   LOCAL nJ, nI, nW, nGRow, nGCol, aFont, nWLbl, nWTxt, cNgrp
   LOCAL cSay, cN, cN2, cGrp, cDbf, nWGbx, nLine, xVal, hFont, nHeight
   LOCAL cFont1, cFont2, cFont5, cForm, aHeader, aField, cPict
                                                                 
   ? "   ====[card]==== " + ProcNL()
   cForm   := oCargo:cForm
   aFont   := oCargo:aFont            // [5] cписок фонтов для карточки
   aHeader := {}
   aField  := {}
   cFont1  := aFont[2]
   cFont2  := aFont[1]
   cFont5  := aFont[5]
   hFont   := GetFontHandle( cFont1 )
   nHeight := GetTextHeight( 0, "A" , hFont )    // высота шрифта
   nGRow   := nG
   nGCol   := nG*2
   nWLbl   := 0
   nLine   := nHeight + 6

   FOR nI := 1 TO LEN(a4Brw)
      AADD( aHeader, a4Brw[nI,1] )
      AADD( aField , a4Brw[nI,2] )
   NEXT

   hFont := GetFontHandle( cFont5 )
   FOR nI := 1 TO LEN(aHeader)
      cSay  := aHeader[nI]
      //cSay  := AtRepl( ";", cSay, " " )
      cSay  := ALLTRIM( cSay )
      nWTxt := GetTextWidth( Nil, cSay, hFont )  // ширина текста
      //nWTxt := GetTxtWidth( cSay, nSize, cFont, lBold )    // получить Width текста
      nWLbl := MAX( nWLbl, nWTxt )
      aHeader[nI] := cSay
   NEXT

   nW    := nW2  //This.ClientWidth
   nH2   := 0    // не использую
   nWLbl += nG*2
   nWGbx := nW - nGCol - nWLbl - nGCol

   FOR nJ := 1 TO nGroup   // группа строки 

      cNgrp  := 'Lbl_Group_' + StrZero(nJ,2)
      cGrp   := StrZero(nJ,2)
      cSay   := "Group (" + HB_NtoS(nJ) + ") - example of a group header "
      cSay   += "/ Группа (" + HB_NtoS(nJ) + ")"
      hFont  := GetFontHandle( cFont5 )
      nWTxt  := GetTextWidth( Nil, cSay, hFont ) + nG         // ширина текста

      IF lSay
         @ nY, nX LABEL &cNgrp WIDTH nWTxt HEIGHT nLine VALUE cSay ;
           FONT cFont5 FONTCOLOR RED VCENTERALIGN TRANSPARENT
      ENDIF
      nY += nLine + nGRow/2

      FOR nI := 1 TO LEN(aHeader)
         cSay := aHeader[nI]
         IF LOWER(cSay) == "not-show"     //  удаляемая колонка
            // пропуск
            LOOP
         ELSE

            IF lSay

               cN := 'Lbl_Card_' + cGrp + '_' + StrZero(nI,2)
               @ nY, nX + nG LABEL &cN WIDTH nWLbl HEIGHT nLine VALUE cSay + ":" ;
                 FONT cFont1 FONTCOLOR BLUE RIGHTALIGN VCENTERALIGN TRANSPARENT

               cN2   := 'GBox_Card_'  + cGrp + '_' + StrZero(nI,2)
               cDbf  := aField[nI]
               xVal  := FIELDGET( FIELDNUM( cDbf ) )
               cPict := IIF( a4Brw[nI,3] == "L", "L", a4Brw[nI,4] )

               @ nY, nX + nWLbl + nGCol/2 GETBOX &cN2 WIDTH nWGbx HEIGHT nLine VALUE xVal ;
                 PICTURE cPict FONT cFont2

               IF Valtype(xVal) == "N"
                  This.&(cN2).Alignment := "LEFT"
               ENDIF

            ENDIF  // lSay

            nY += nLine + nGRow/2

         ENDIF
      NEXT

   NEXT

   nHeight := INT( nY - (nLine + nGRow/2) * 2 )

   ? "   ====[end]==== " + ProcNL(), cForm, "nHeight=", nHeight

RETURN nHeight

///////////////////////////////////////////////////////////////////////////////
FUNCTION Card_Menu_Config(oWnd,cObj)
   LOCAL lMenuStyle, nMenuBitmap, hForm, cForm, nY, nX, nI, cN
   LOCAL aMnFont, hFont1, hFont2, hFont5

   cForm       := oWnd:Name
   hForm       := oWnd:Handle
   aMnFont     := oWnd:Cargo:aFont                // список фонтов загруженных ранее
   //aMnFont := {"Normal", "Bold" , "Italic", "ComSnMs", "SnapITC"  }
   hFont1      := GetFontHandle( aMnFont[1] )
   hFont2      := GetFontHandle( aMnFont[2] )
   hFont5      := GetFontHandle( aMnFont[5] )
   // координаты кнопки
   nY := GetProperty(cForm,cObj,"Row") + GetProperty(cForm,cObj,"Height")
   nY += GetProperty(cForm, "Row") + GetTitleHeight() + 2  
   nX := GetProperty(cForm,cObj,"Col")
   nX += GetProperty(cForm, "Col") + GetBorderWidth() - 4         
   // remember CONTEXT MENU values
   lMenuStyle  := IsExtendedMenuStyleActive()     // menu style EXTENDED/STANDARD
   nMenuBitmap := GetMenuBitmapHeight()           // bmp height in context menu

   // set a new style for the context menu
   SET MENUSTYLE EXTENDED     // switch menu style to advanced
   SetMenuBitmapHeight( 32 )  // set image size 32x32

   //SetThemes(2)  // theme "Office 2000 theme" в ContextMenu
   //SetThemes(3)  // theme "Dark theme" в ContextMenu
   DEFINE CONTEXT MENU CONTROL &cObj OF &cForm
       MENUITEM "Config for Card"  DISABLED  FONT hFont5
       SEPARATOR                          
       MENUITEM "menu - reserve 1" ACTION _wPost( Val(This.Name), cForm, This.Name ) NAME &( "2021" ) FONT hFont1 ICON "i_Menu32x1"
       MENUITEM "menu - reserve 2" ACTION _wPost( Val(This.Name), cForm, This.Name ) NAME &( "2021" ) FONT hFont1 ICON "i_Menu32x1"
       MENUITEM "menu - reserve 3" ACTION _wPost( Val(This.Name), cForm, {3}       ) NAME &( "2023" ) FONT hFont1 ICON "i_Menu32x1"
       //MENUITEM cMenuFont          ACTION _wPost( Val(This.Name), cForm, This.Name ) NAME &( "2024" ) FONT hFont2 ICON "i_Menu32x2"
       Popup "Font size in card [" + HB_NtoS(oWnd:Cargo:nSize) + "]" NAME "SetSizeFont" FONT hFont2 // Level 2
          FOR nI := oWnd:Cargo:aFor[1] TO oWnd:Cargo:aFor[2]
             cN := "SetSizeFont" + "_" + StrZero(nI,5)
             IF nI == oWnd:Cargo:nSize
                MENUITEM HB_NtoS(nI) NAME &(cN) ACTION myMenuFont2(This.Name) FONT hFont1 ;
                CHECKED CHECKMARK "Tick32"   
             ELSE
                MENUITEM HB_NtoS(nI) NAME &(cN) ACTION myMenuFont2(This.Name) FONT hFont1 ;
                CHECKMARK "Tick32"                   
             ENDIF
          NEXT
       End Popup

   END MENU

   //пример задания MENUITEM через событие
   (This.Object):Event(2021, {|ow,ky,cItm| MsgDebug(ow:Name, ky, "Press menu Item - Name="+cItm) })
   (This.Object):Event(2023, {|ow,ky,ap  | MsgDebug(ow:Name,ky,ap) } )  
   //(This.Object):Event(2025, {|| myMenuFont2(This.Name) } )  

   _ShowContextMenu(cForm, nY, nX, .F. ) ; InkeyGui(20)  // menu runs through the queue

   IF _IsWindowDefined( cForm )
      DEFINE CONTEXT MENU OF &cForm    // deleting menu after exiting
      END MENU
   ENDIF

   // restore the CONTEXT MENU values
   SetMenuBitmapHeight(nMenuBitmap)  // bmp height in context menu - return as it was
   _NewMenuStyle( lMenuStyle )       // menu style EXTENDED/STANDARD - return as it was

RETURN NIL

///////////////////////////////////////////////////////////////////
FUNCTION myMenuFont2(cItemName)
   LOCAL cForm, cItem, nI, aFor, nSize, cMenu, cSize,cItm,nItm
   LOCAL oWnd := ThisWindow.Object
   DEFAULT cItemName := This.Name

   cForm := oWnd:Name
   nSize := oWnd:Cargo:nSize
   aFor  := oWnd:Cargo:aFor 
   cSize := HB_NtoS(nSize)
   cItm  := This.Caption
   nItm  := Val( SUBSTR(cItemName, RAt("_", cItemName) + 1) )

   FOR nI := aFor[1] TO aFor[2]
      cItem := "SetSizeFont" + "_" + StrZero(nI, 5)
      SetProperty( cForm, cItem, "Checked" , .F. )     
      IF nI == nItm
         SetProperty( cForm, cItem, "Checked"   , .T.      )
         oWnd:Cargo:nSize := nItm
         cSize := HB_NtoS(nItm)
      ENDIF
   NEXT 

   cItem := _GetMenuItemCaption( "SetSizeFont" , cForm )
   cMenu := Left( cItem, At( "[", cItem ) - 1 ) + "[" + cSize + "]"
   _SetMenuItemCaption( "SetSizeFont" , cForm , cMenu )
   
RETURN NIL
