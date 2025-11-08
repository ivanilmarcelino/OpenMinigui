/*
 * MINIGUI - Harbour Win32 GUI library
 *
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com>
 * Copyright 2025 Sergej Kiselev <bilance@bilance.lv>
 *
*/
#define  _HMG_OUTLOG
#include "minigui.ch"
////////////////////////////////////////////////////////////////////
// command line parameters: *.arr *.txt *.csv
FUNCTION CommandLine( aParam )
   LOCAL aFile, cFile, cName, aFil, cPath, nCnt, i
   LOCAL cDsk := App.Cargo:cPathCsv

   //aParam := { cDsk + "Абоненты.txt" , cDsk + "demo-array.arr" }
   //aParam := { ".\CSV\E011102_64187584_7521_3.txt", "D:Абоненты.txt" }
   //aParam := { ".\CSV\*.csv" }
   //aParam := { ".\CSV\*.arr" }
   //aParam := { ".\CSV\*.y*" }
   //aParam := { ".\CSV\demo-array.arr" }
   ? ProcNL(), aParam, hb_valtoexp(aParam)
   IF LEN(aParam) == 0
      RETURN {}
   ENDIF
   aFile := aParam
   nCnt  := 0

   IF Len(aFile) == 1
      cFile := aFile[1]
      IF "*" $ cFile
         cPath := hb_FNameDir(cFile)
         cName := hb_FNameNameExt(cFile)
         aFil  := Directory( cFile )
         aFile := {}
         FOR i := 1 TO LEN(aFil)
            AADD( aFile, cPath + aFil[i,1] )
         NEXT
      ENDIF
   ELSEIF Len(aFile) > 1
      FOR EACH cFile IN aFile
         IF "*" $ cFile
            nCnt++
         ENDIF
      NEXT
      IF nCnt > 1
         ? ProcNL(), "There is no such processing !"
         ?? HB_ValToExp(aFile)
         aFile := {}
      ENDIF
   ENDIF
   ? "aFile =", aFile ; ?v aFile ; ?

RETURN aFile

//////////////////////////////////////////////////////////////////////////
FUNCTION Table_List(oWnd, ky, cBtn)
   LOCAL nY, nX, cForm, hFont1, hFont2, aMenu, aList, aVal, cTxt, nI
   LOCAL aForm, cMenu, cLang, bAction, cName, lChk, lDis, hFont, cImg
   LOCAL nChoice, nHndl, cLng1, cLng2, nHMain, nCol, nRow, nW, nH, oW
   LOCAL aMove

   ? ProcNL(), oWnd:ClassName, ky, cBtn

   cForm  := oWnd:Name
   nHMain := oWnd:Cargo:nHMain
   aForm  := HMG_GetForms()
   hFont1 := GetFontHandle( "ComSanMS" )
   hFont2 := GetFontHandle( "DlgFont"  )
   // координаты вывода окна / window output coordinates
   nY     := GetProperty(cForm, "Row") + GetTitleHeight()
   nY     += GetProperty(cForm, cBtn, "Row") + GetProperty(cForm, cBtn, "Height")
   nX     := GetProperty(cForm, "Col") + GetBorderWidth()
   nX     += GetProperty(cForm, cBtn, "Col") - 4
   cLang  := IIF( App.Cargo:lRu, "Таблица: " , "Table: " )
   aMenu  := {}
   aList  := {}
   aMove  := {}
   cTxt   := ""

   FOR nI := 1 TO LEN(aForm)
      cForm := UPPER(aForm[nI])
      cMenu := GetProperty( cForm, "Title" )
      cMenu := ALLTRIM( SUBSTR( cMenu, AT("-",cMenu) + 1 ) )
      aVal  := { cForm, _HMG_aFormType[nI], _HMG_aFormHandles[nI] ,;
                 _HMG_aFormDeleted[nI], IsWindowVisible( GetFormHandle( cForm ) ) ,;
                 GetProperty( cForm, "Title" ), cLang + cMenu }
      cTxt  += SPACE(5) + HB_NtoS(nI) + ") "
      cTxt  += ' Form: ' + cForm + ', Type: "'+_HMG_aFormType[nI]+'" '
      cTxt  += ', Handle: '+HB_NtoS(_HMG_aFormHandles[nI])
      cTxt  += ', Deleted: ' + cValToChar( _HMG_aFormDeleted[nI] )
      cTxt  += ', Visible: ' + cValToChar( IsWindowVisible( GetFormHandle( cForm ) ) )
      cTxt  += ', Title: ' + GetProperty( cForm, "Title" ) + CRLF
      AADD( aList, aVal )
   NEXT
   aList := ASORT( aList,,, { | x, y | x[ 7 ] < y[ 7 ] } )
   ? cTxt
   FOR nI := 1 TO LEN(aList)
      IF aList[nI,2] # "A"
         AADD( aMenu, { "iWin48x2", aList[nI,7], .F. } )
         AADD( aMove, aList[nI,1] )
      ENDIF
   NEXT

   cLng1 := IIF( App.Cargo:lRu, "Расположить окна по горизонтали" , "Arrange windows horizontally" )
   cLng2 := IIF( App.Cargo:lRu, "Расположить окна лесенкой"       , "Arrange the windows in a ladder" )

   SET MENUSTYLE EXTENDED
   SetMenuBitmapHeight( 32 )

   nChoice := 0
   DEFINE CONTEXT MENU OF &cForm
      MENUITEM cLng1 NAME SetWin1 ACTION {|| nChoice := 2001 } ICON "iWin48x1" FONT hFont2
      MENUITEM cLng2 NAME SetWin2 ACTION {|| nChoice := 2002 } ICON "iWin48x1" FONT hFont2
      SEPARATOR

      FOR nI := 1 TO LEN(aMenu)
         cName   := StrZero(nI, 10)
         cImg    := aMenu[nI,1]
         cMenu   := aMenu[nI,2]
         bAction := {|| nChoice := Val( This.Name ) }
         lChk    := .F.
         lDis    := aMenu[nI,3] //.F. - DISABLED
         hFont   := IIF( lDis, hFont2, hFont1 )
         _DefineMenuItem( cMenu, bAction, cName, , lChk, lDis, , hFont , , .F., .F. , cImg, .F. )
      NEXT

   END MENU

   _ShowContextMenu(cForm, nY, nX, .f. ) // ПОКАЗ ВЫПАДАЕЩЕГО МЕНЮ / SHOW DROP-DOWN MENU

   DO EVENTS

   DEFINE CONTEXT MENU OF &cForm         // delete menu after exiting
   END MENU

   DO EVENTS

   IF nChoice == 0
   ELSEIF nChoice > 0 .AND. nChoice < 2000
      cForm := aList[nChoice,1]
      nHndl := aList[nChoice,3]
      If _IsWindowDefined( cForm )
         DO EVENTS
         If IsIconic( GetFormHandle(cForm) )
            _Restore( GetFormHandle(cForm) )
         EndIf
         DoMethod( cForm, "SetFocus" )
      EndIf
   ELSEIF nChoice == 2001

       FOR nI := 1 TO LEN(aMove)
          cForm := aMove[nI]
          oW   := _WindowObj(cForm)
          _Restore( GetFormHandle(cForm) )
          nRow := oW:Row
          nCol := oW:Col
          nW   := oW:Width
          nH   := oW:Height
? nI, cForm, nRow, nCol, "nHMain + (nI-1)*40", nHMain + (nI-1)*40
          MoveWindow( GetFormHandle( cForm ) , nCol , nHMain + (nI-1)*40, nW , nH , .t. )
          //GetWindowRect( GetFormHandle( Form ), actpos )
          //MoveWindow( GetFormHandle( Form ) , col , row , width , height , .t. )
       NEXT

   ELSEIF nChoice == 2002

       FOR nI := 1 TO LEN(aMove)
          cForm := aMove[nI]
          oW   := _WindowObj(cForm)
          _Restore( GetFormHandle(cForm) )
          nRow := oW:Row
          nCol := oW:Col
          nW   := oW:Width
          nH   := oW:Height
          MoveWindow( GetFormHandle( cForm ) , nCol + (nI-1)*40, nRow + nHMain + (nI-1)*40, nW , nH , .t. )
          //GetWindowRect( GetFormHandle( Form ), actpos )
          //MoveWindow( GetFormHandle( Form ) , col , row , width , height , .t. )
       NEXT

   ELSE
      MsgDebug(nChoice )
   ENDIF

   DO EVENTS

RETURN nChoice

//////////////////////////////////////////////////////////////////////
FUNCTION Table_Config(oWnd, ky, cBtn, cCapt)
   LOCAL nY, nX, cForm, hFont1, hFont2, nChoice, aPar, cLang, cLng2
   LOCAL oIni, cLng3, cLng4, cLng5, nLang, cLng6, cLng7, cLng8

   ? ProcNL(), oWnd:ClassName, ky, cBtn, cCapt

   oIni   := App.Cargo:oIni  // поместим объект ини-файла в переменную
   cForm  := oWnd:Name
   hFont1 := GetFontHandle( "ComSanMS" )
   hFont2 := GetFontHandle( "DlgFont"  )
   // координаты вывода окна / window output coordinates
   nY     := GetProperty(cForm, "Row") + GetTitleHeight()
   nY     += GetProperty(cForm, cBtn, "Row") + GetProperty(cForm, cBtn, "Height")
   nX     := GetProperty(cForm, "Col") + GetBorderWidth()
   nX     += GetProperty(cForm, cBtn, "Col") - 4
   cLang  := IIF( App.Cargo:lRu, "Автозамена " , "AutoCorrect " )
   cLng2  := IIF( App.Cargo:lRu, " на " , " on " )
   cLng3  := IIF( App.Cargo:lRu, "Язык программы" , "Program language" )
   cLng4  := IIF( App.Cargo:lRu, 'Русский'    , 'Russian' )
   cLng5  := IIF( App.Cargo:lRu, 'Английский' , 'English' )
   cLng6  := IIF( App.Cargo:lRu, "Ширина колонок таблиц"  , "Table Column Width" )
   cLng7  := IIF( App.Cargo:lRu, 'Включить расчёт строк текста' , 'Enable text line calculation'  )
   cLng8  := IIF( App.Cargo:lRu, 'Отключить расчёт строк текста', 'Disable text line calculation' )

   SET MENUSTYLE EXTENDED
   SetMenuBitmapHeight( 32 )

   aPar    := {}
   nChoice := 0
   DEFINE CONTEXT MENU OF &cForm
      //MENUITEM cCapt    DISABLED   ICON "iArrowC48x2" FONT hFont1
      MENUITEM cLang + "<|>" + cLng2 + "<;>" NAME SetZam1 ACTION {|| nChoice := 1 } ICON "iArrowC48x2" FONT hFont2
      SEPARATOR
      MENUITEM cLang + "OEM" + cLng2 + "ANSI"  NAME SetZam2 ACTION {|| nChoice := 2 } ICON "iArrowC48x2" FONT hFont2
      SEPARATOR
      MENUITEM cLang + "UTF8" + cLng2 + "ANSI" NAME SetZam3 ACTION {|| nChoice := 3 } ICON "iArrowC48x2" FONT hFont2
      SEPARATOR                                         //      vvv - нельзя здесь использовать ICON
      Popup cLng3 NAME "40_Lng"    FONT "DlgFont" IMAGE "bLang32"
         ITEM cLng4  NAME "40_Lng1" ACTION {|| nChoice := 4 , aPar := {1,"RU",This.Caption} } ICON "iFlag_Ru32" FONT "DlgFont"
         ITEM cLng5  NAME "40_Lng2" ACTION {|| nChoice := 4 , aPar := {2,"EN",This.Caption} } ICON "iFlag_En32" FONT "DlgFont"
         //ITEM 'Belorussian'     NAME "40_Lng3" ACTION _wPost("SetLang",, {3,"BE",This.Caption}) FONT "DlgFont" ICON "i_Bel32"
         //ITEM 'Ukrainian'       NAME "40_Lng4" ACTION _wPost("SetLang",, {4,"UK",This.Caption}) FONT "DlgFont" ICON "i_Uk32"
         //ITEM 'Kazakh'          NAME "40_Lng5" ACTION _wPost("SetLang",, {5,"KZ",This.Caption}) FONT "DlgFont" ICON "i_Kaz32"
      End Popup
      App.Cargo:aIcoLang := {"iFlag_Ru32","iFlag_En32","iFlag_Bel32","iFlag_Uk32","iFlag_Kaz32"}
      App.Cargo:aNumLang := {"01-Russian","02-English","03-Belorussian","04-Ukrainian","05-Kazakh" }

      SEPARATOR                                         //      vvv - нельзя здесь использовать ICON
      Popup cLng6 NAME "50_ColWidth"    FONT "DlgFont" IMAGE "bSize32"
         ITEM cLng7  NAME "50_ColWidth" ACTION {|| nChoice := 5 , aPar := {.T.,"ON" ,This.Caption} } ICON "iWin48x1" FONT "DlgFont"
         ITEM cLng8  NAME "50_ColWidth" ACTION {|| nChoice := 5 , aPar := {.F.,"OFF",This.Caption} } ICON "iWin48x2" FONT "DlgFont"
      End Popup

   END MENU

   _ShowContextMenu(cForm, nY, nX, .f. ) // ПОКАЗ ВЫПАДАЕЩЕГО МЕНЮ / SHOW DROP-DOWN MENU

   DO EVENTS

   DEFINE CONTEXT MENU OF &cForm         // delete menu after exiting
   END MENU

   DO EVENTS

   IF nChoice == 0
   ELSEIF nChoice == 1
   ELSEIF nChoice == 2
   ELSEIF nChoice == 3
   ELSEIF nChoice == 4
      nLang := aPar[1]
      cLang := aPar[2]
      // запишем новые значения в ини-файл
      App.Cargo:oIni:MAIN:nLang    := nLang
      App.Cargo:oIni:MAIN:cLang    := cLang
      App.Cargo:oIni:MAIN:aIcoLang := App.Cargo:aIcoLang
      App.Cargo:oIni:MAIN:aMnmLang := App.Cargo:aNumLang
      oIni:Write()  // NOT UTF8, i.e. no BOM in the output
      _wSend("ReStart",oWnd)
   ELSEIF nChoice == 5
      // ширина колонок полная по тексту - отключить/включить
      App.Cargo:lColumnWidthFull := aPar[1]
      // запишем новые значения в ини-файл
      App.Cargo:oIni:MAIN:lColumnWidthFull := aPar[1]
      oIni:Write()  // NOT UTF8, i.e. no BOM in the output
   ENDIF

   DO EVENTS

RETURN nChoice

//////////////////////////////////////////////////////////////////////
FUNCTION HelpThis()
   LOCAL cMsg, cVer, o := App.Cargo

   cVer := HB_NtoS(MiniGuiVersionNumba())
   cMsg := App.Cargo:cTitle    + ";"
   cMsg += App.Cargo:cVersion  + ";;"
   cMsg += IIF( o:lRu, o:cInfo1Ru , o:cInfo1En ) + ";"
   cMsg += IIF( o:lRu, o:cInfo2Ru , o:cInfo2En ) + ";"
   cMsg += IIF( o:lRu, o:cInfo3Ru , o:cInfo3En ) + ";;"
   cMsg += App.Cargo:cAvtor1   + ";"
   cMsg += App.Cargo:cAvtor2   + ";;"
   cMsg += App.Cargo:cPrgInfo1 + ";"
   cMsg += App.Cargo:cPrgInfo2 + ";"
   cMsg += App.Cargo:cSiteDownload + ";;"
   cMsg += "Operating System: " + Os() + ";"
   cMsg += "Developed in : " +  MiniGUIVersion()
   cMsg += "|" + cVer + ";"
   cMsg += "xBase Compiler: " + Version() + ";"
   cMsg += "C Compiler: " + Hb_Ccompiler() + ";;"
   cMsg += PadC( "This program is Freeware!", 70 ) + ";"
   cMsg += PadC( "Copying is allowed!", 70 )  + ";;"
   cMsg += REPL(";;",2)

RETURN cMsg

//////////////////////////////////////////////////////////////////////
// Новые цвета и иконки в функции Alert*()
// New colors and icons in the Alert*() function
FUNCTION oAlert()
   LOCAL o := App.Cargo

   o:cIcoStop64  := "iMgStop128"
   o:cIcoInfo64  := "iMgInfo128"
   o:cIcoExclm64 := "iMgExclam128"
   o:cIcoYesNo64 := "iMgQuest128"
   o:nIcoSize    := 64
   // замена на кнопке "Ok" на "Продолжить"
   // replacing the "Ok" button with "Continue"
   DEFAULT o:lRu := .F.
   o:aBtnCaptOk  := IIF( o:lRu, {"&Продолжить"},  {"&Continue"} )

RETURN NIL

///////////////////////////////////////////////////////////////////////////////
FUNCTION MiniGuiVersionNumba()
   LOCAL cRegEx, cVer, aVal, nVer := 0,  cVal := MiniGuiVersion()
   // Harbour MiniGUI Extended Edition 21.10.3 (32-bit) ANSI
   // Harbour MiniGUI Extended Edition 15.12
   cRegEx := "\d+\.\d+[\d.]*"
   aVal   := HB_RegEx(cRegEx, cVal)
   IF LEN(aVal) > 0
      cVal := aVal[1]
      cVer := CHARREM( '.', cVal )
      cVer := PADR(cVer,6,'0')
      nVer := VAL( cVer )
   ENDIF

RETURN nVer



