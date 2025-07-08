/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * ‘орма экспорта в файл / Export to file form
*/

#define _HMG_OUTLOG
#include "minigui.ch"
//////////////////////////////////////////////////////////////////////////////////
FUNCTION Form_ExportFile(oWnd, cTitle, cMsg, cFileExp, cIco3, lSayCdPg)
   LOCAL cIcon, cFont, nFontSize, aBackColor, aBColorTxt, nI
   LOCAL nY, nX, nW, nH, nG, nWTxt, nHTxt, nHBtn, nWBtn, nYBtn, nXBtn
   LOCAL nX2, ao, owc, oDlu, nWTxt2, lOpen, cPath, aBtnBClr
   LOCAL cBtnCapt, aBtnFont, aBtnFClr, nFBtnSize, cFontTxt
   LOCAL nWBtn2, nWBtnDir, nLR, cVal, ahIco, cForm, aIcon
   LOCAL cFontBtn, nHIco, cTxt, aBtn, bAct, cFile, cCdPg
   DEFAULT lSayCdPg := .T.

   oWnd:Cargo:aFileExport := {}                   // вернуть парметры экспорта - вариант 3
   ao         := (App.Cargo)                    
   cFont      := ao:cFontName                     // DejaVu Sans Mono
   nFontSize  := ao:nFontSize                   
   cFontBtn   := ao:cFontName2                    // Comic Sans MS
   nFBtnSize  := ao:nDlgSize                      // ao:nFontSize + 2
   aBtnFont   := { cFontBtn, nFBtnSize, .T. }     // фонт дл€ кнопок
   cFontTxt   := "Tahoma"
   aBackColor := ao:aDlgBColor                    // цвет окна формы
   aBColorTxt := {255,255,240}
   cForm      := "Form_Export"
   cIcon      := ao:cDefAppIcon
   aIcon      := { cIco3, "iArrow64", "iSqlite64" }
   aBtnFClr   := { BLACK, YELLOW }
   aBtnBClr   := {133,6,63}
   nWBtn2     := 0
   // эти данныЄ нужно вернуть через Cargo окна
   cFile      := cFileNoPath(cFileExp)
   cPath      := cFilePath(cFileExp) 
   cCdPg      := "RU1251" 
   lOpen      := .T. 

   // возвращает объект с данными размеров от размера фонта от dlu в pixel
   oDlu := oDlu4Font( nFontSize ) ; nG := oDlu:Top  // можно так
   // высота экрана Desktop
   nH   := GetDesktopHeight()
   IF nH <= 600                       ;  nG := 10
   ELSEIF nH >= 768 .AND. nH <= 864   ;  nG := 15
   ELSEIF nH > 864  .AND. nH <= 1080  ;  nG := 20
   ELSE                               ;  nG := 25
   ENDIF

   nX    := nLR := nG                      // отступ слева и право
   nY    := nG                             // отступ сверху и снизу
   nW    := 810                            // размеры окна
   nH    := 560                            // размеры окна
   nHTxt := nFontSize*2 + nFontSize/2      // ширина GET'ов
   nX2   := 0

   DEFINE WINDOW &cForm AT nY, nX WIDTH nW HEIGHT nH ;
      ICON cIcon TITLE cTitle BACKCOLOR aBackColor   ;
      MODAL NOSIZE                                   ;
      FONT cFont SIZE nFontSize                      ;
      ON INIT    _wPost( 0)                          ;
      ON RELEASE _wSend(98)

      This.Cargo := oHmgData() ; owc := This.Cargo  // дл€ окна создаем объект без переменных (условно пустой)
      owc:aBColor  := This.BackColor   // цвет окна
      owc:ahIcoDel := {}               // дл€ удалени€ хендлов иконок с формы
      owc:oWnd     := oWnd             // запомнить ¬≈—№ объект Cargo -> родительского окна  - вариант 3
      owc:cPath    := cPath               
      owc:cFile    := cFile
      owc:lOpen    := lOpen
      owc:cCdPg    := cCdPg
      owc:lSayCdPg := lSayCdPg

      nW    := This.ClientWidth
      nH    := This.ClientHeight
      nWTxt := nW - nLR * 2

      @ 0, 0 LABEL Label_0 WIDTH nG HEIGHT nG VALUE '' INVISIBLE

      FOR nI := 1 TO LEN(aIcon)
          owc:hIcon := LoadIconByName( aIcon[nI], 64, 64 )
          AADD( owc:ahIcoDel , owc:hIcon )
          owc:nXIco := nW-64*nI
          DRAW ICON IN WINDOW &cForm AT 0, owc:nXIco HICON owc:hIcon WIDTH 64 HEIGHT 64 COLOR WHITE
      NEXT

      @ 0, 0 LABEL Label_1 WIDTH owc:nXIco HEIGHT 64 VALUE '' BACKCOLOR WHITE

      @ 20, nG LABEL Label_2 VALUE cMsg SIZE nFontSize + 5 AUTOSIZE FONTCOLOR aBtnBClr BACKCOLOR WHITE

      nY := This.Label_1.Height + nG/2

      cVal := IIF( ao:cLang == "RU", " »м€ файла дл€ экспорта ", " File name for export " )
      @ nY, nX FRAME Frame_1  CAPTION cVal ;
        WIDTH nWTxt HEIGHT nHTxt*2+5 BACKCOLOR aBackColor //OPAQUE
      nY += nG + nG/2

      nWBtnDir := nHTxt
      @ nY, nX + nLR TEXTBOX Tb_File VALUE cFile WIDTH nWTxt - nLR*2 HEIGHT nHTxt ;
        FONT cFontTxt FONTCOLOR BLACK BACKCOLOR aBColorTxt ;
        ON CHANGE {|ow| cFile := This.Tb_File.Value ,;
                        ow := ThisWindow.Object , ow:Cargo:cFile := cFile }
      //myMaxFont(ThisWindow.Name,"Tb_File")  // уменьшить размер
      nY := This.Frame_1.Row + This.Frame_1.Height + nG

      cVal := IIF( ao:cLang == "RU", " ѕуть дл€ экспорта файла ", " Path to export file " )
      @ nY, nX FRAME Frame_2  CAPTION cVal ;
        WIDTH nWTxt HEIGHT nHTxt*2+5 BACKCOLOR aBackColor //OPAQUE
      nY += nG + nG/2

      nWBtnDir := nHTxt
      @ nY, nX + nLR TEXTBOX Tb_Path VALUE cPath WIDTH nWTxt - nWBtnDir - nLR*2 HEIGHT nHTxt ;
        FONT cFontTxt FONTCOLOR BLACK BACKCOLOR aBColorTxt ;
        ON CHANGE {|ow| cFile := This.Tb_Path.Value ,;
                        ow := ThisWindow.Object , ow:Cargo:cPath := cPath }
      //myMaxFont(ThisWindow.Name,"Tb_Path")  // уменьшить размер

      /////////////////////// Button ////////////////////////////
      cVal  := IIF( ao:cLang == "RU", "”кажите путь дл€ экспорта", "Specify the path for export" )
      nHIco := This.Tb_Path.Height - 2*2
      cTxt  := ""
      aBtn  := { "Button_Dir", cTxt, "iFFile32x1", "iFFile32x2", nHIco, aBtnFClr, aBtnFont, cVal }
      nYBtn := nY
      nXBtn := nX + nLR + This.Tb_Path.Width + 5
      nHBtn := nHIco + 2*2
      nWBtn := nWBtnDir
      bAct  := {|| /*MsgDebug(This.Cargo),*/ _wPost(1)  }  // событие на форме
      ahIco := my2DrawButtonGrad(nYBtn, nXBtn, nWBtn, nHBtn, aBtn, bAct, GRAY)
      // присвоим €вно кнопку здесь
      This.Button_Dir.Action := bAct
      IF LEN(ahIco) > 0 // дл€ удалени€ хендлов иконок с формы
         AADD( owc:ahIcoDel , ahIco[1] )
         AADD( owc:ahIcoDel , ahIco[2] )
      ENDIF

      nY := This.Frame_2.Row + This.Frame_2.Height + nG

      cVal := IIF( App.Cargo:cLang == "RU", " —оздание файла в кодировке ", " Creating a file in encoding " )
      @ nY, nX FRAME Frame_4 CAPTION cVal WIDTH nWTxt HEIGHT nHTxt*2+5 BACKCOLOR aBackColor //OPAQUE

      nY += nG + nG/5 
      nX := nLR*2

      /////////////////////// Button ////////////////////////////
      nWTxt2   := (nWTxt - nLR*2 - nG) / 2
      cBtnCapt := IIF(ao:cLang == "RU", " одировка: ", "Charset: " ) + cCdPg
      cVal     := IIF( ao:cLang == "RU", "¬ыберите кодировку Ѕƒ", "Select DB encoding" )
      nYBtn    := nY
      nXBtn    := nX 
      nWBtn    := nWTxt2
      nHBtn    := This.Tb_Path.Height + nG/4
      nHIco    := 32
      aBtn     := { "Button_CdPg", cBtnCapt, "iLang32", "iLang32", nHIco, aBtnFClr, aBtnFont, cVal }
      bAct     := {|| /*MsgDebug(This.Cargo),*/ _wPost(3,cForm) }   // событие на форме
      ahIco    := my2DrawButtonGrad(nYBtn, nXBtn, nWBtn, nHBtn, aBtn, bAct, GRAY)
      // присвоим €вно кнопку здесь
      This.Button_CdPg.Action := bAct
      IF LEN(ahIco) > 0 // дл€ удалени€ хендлов иконок с формы
         AADD( owc:ahIcoDel , ahIco[1] )
         AADD( owc:ahIcoDel , ahIco[2] )
      ENDIF

      nY := This.Frame_4.Row + This.Frame_4.Height + nG

      cVal  := IIF( ao:cLang == "RU", "«апрос на открытие файла после экспорта",;
                      "Prompt to open file after export" )
      nWTxt := GetTxtWidth( cVal, nFontSize + 3, cFont ) + 90

      @ nY, nX CHECKLABEL Chkl_1 WIDTH nWTxt HEIGHT 34    ;
        VALUE cVal LEFTCHECK SIZE nFontSize + 3           ;
        IMAGE { 'CheckT32', 'CheckF32' }                  ;
        ON MOUSEHOVER Rc_Cursor( "MINIGUI_FINGER" )       ;
        BACKCOLOR aBackColor                              ;
        ON INIT {|  | This.Checked := lOpen }             ;
        ONCLICK {|ow| This.Checked := ! This.Checked,; 
                      lOpen := This.Checked ,;
                      ow := ThisWindow.Object , ow:Cargo:lOpen := lOpen }

      nY += 32 + nG*2

      /////////////////////// Button ////////////////////////////
      cBtnCapt := IIF( ao:cLang == "RU", "ѕомощь", "Help" )
      nWBtn    := 300
      nHBtn    := 64 
      nYBtn    := nY
      nHIco    := nHBtn - nG/2

      cTxt  := IIF( ao:cLang == "RU", "Ёкспорт в файл", "Export to file" )
      nXBtn := nW - nWBtn*2 - nG*2
      aBtn  := { "Button_Open", cTxt, "iConn48x1", "iConn48x2", nHIco, aBtnFClr, aBtnFont, "" }
      bAct  := {|| This.Button_Open.Enabled := .F. ,  _wPost(90,cForm) }   // событие на форме
      ahIco := my2DrawButtonGrad(nYBtn, nXBtn, nWBtn, nHBtn, aBtn, bAct, aBtnBClr )
      // присвоим €вно кнопку здесь
      This.Button_Open.Action := bAct
      IF LEN(ahIco) > 0 // дл€ удалени€ хендлов иконок с формы
         AADD( owc:ahIcoDel , ahIco[1] )
         AADD( owc:ahIcoDel , ahIco[2] )
      ENDIF

      cTxt  := IIF( ao:cLang == "RU", "ќтмена", "Cancel" )
      nXBtn := nW - nWBtn - nG
      aBtn  := { "Button_Exit", cTxt, "iReturn64x1", "iReturn64x2", nHIco, aBtnFClr, aBtnFont, "" }
      //bAct:= {|| This.Button_Exit.Enabled := .F.,  _wPost(101,cForm) }   // не работает - вылет
      bAct  := {|| oWnd:Cargo:aFileExport := {} , M->aPubFileExport := {} ,  _wPost(99,cForm) }   // событие на форме
      ahIco := my2DrawButtonGrad(nYBtn, nXBtn, nWBtn, nHBtn, aBtn, bAct, BLACK )
      // присвоим €вно кнопку здесь
      This.Button_Exit.Action := bAct
      IF LEN(ahIco) > 0 // дл€ удалени€ хендлов иконок с формы
         AADD( owc:ahIcoDel , ahIco[1] )
         AADD( owc:ahIcoDel , ahIco[2] )
      ENDIF

      nY += nHBtn + nG

      // уменьшим внешнюю высоту окна
      ThisWindow.Height := nY + GetTitleHeight() + GetBorderHeight()

      //_o2log(owc, 15, ProcNL()+" -------------- ѕараметры объекта : => owc", .T.)

      // ”становка событий на это окно программы
      Sets_Event2ThisWindow()

      ON KEY ESCAPE OF &cForm ACTION _wPost(99)

   END WINDOW

   CENTER   WINDOW &cForm
   ACTIVATE WINDOW &cForm

   ? "@@@@@ =>", oWnd, valtype(oWnd), oWnd:ClassName, oWnd:Name
   ?? oWnd:Cargo
   ?? oWnd:Cargo:aFileExport

RETURN oWnd:Cargo:aFileExport // вернуть - вариант 3
//RETURN aFileExport          // вернуть - вариант 4
//RETURN M->aPubFileExport    // вернуть - вариант 1 

//////////////////////////////////////////////////////////////////////////////////////////////////////
// ”становка событий на это окно программы
STATIC FUNCTION Sets_Event2ThisWindow()

   WITH OBJECT This.Object
     :Event( 0, {|ow| // запуск после построени€ окна
                       This.Topmost := .F.
                       ? ProcNL(),">>> Start window: "+ow:Name
                       If !ow:Cargo:lSayCdPg
                          This.Frame_4.Enabled     := .F.
                          This.Button_CdPg.Enabled := .F.
                       Endif
                       ow:Setfocus('Label_0')
                       DO EVENTS
                       Return Nil
                       })

     :Event( 1, {|ow| // кнопка - ”кажите путь к экспорту файла
                      Local ao    := App.Cargo
                      Local cForm := ow:Name
                      Local cTtl  := "¬ыберите папку дл€ экспорта"
                      Local cTtl2 := "Select a folder to export to"
                      Local cDir, cPth := ow:Cargo:cPath
                      Local cObj  := "Tb_Path"
                      Local cObj2 := "Button_Dir"
                      cTtl := IIF(ao:cLang == "RU", cTtl, cTtl2 )
                      This.&(cObj2).Enabled := .F.
                      SET WINDOW THIS TO ow:Name        // ќЅя«ј“≈Ћ№Ќќ !!!
                      cDir := GetFolder( cTtl, cPth )
                      SET WINDOW THIS TO
                      IF LEN(cDir) > 0
                         DO EVENTS
                         SetProperty(cForm,cObj,"Value", cDir)
                         ow:Cargo:cPath := cDir        // исправили путь файла
                         ? ProcNL(), "cDir=", cDir ; ? "    ########", cObj, "["+ow:Cargo:cPath+"]"
                         //myMaxFont(cForm,cObj)  // максимальный размер фонта
                      ENDIF
                      //SET WINDOW THIS TO ow:Name        // ќЅя«ј“≈Ћ№Ќќ !!!
                      //MsgDebug(":Event(1)","ow:Cargo:cPath=",ow:Cargo:cPath)
                      //SET WINDOW THIS TO
                      This.&(cObj2).Enabled := .T.
                      ow:Setfocus('Label_0')
                      DO EVENTS
                      Return Nil
                      } )

     :Event( 3, {|ow| // кнопка - выбор кодовой страницы
                      Local aR
                      SET WINDOW THIS TO ow:Name        // ќЅя«ј“≈Ћ№Ќќ !!!
                      aR := myCodePage()           
                      SET WINDOW THIS TO
                      If LEN(aR) > 0
                         ow:Cargo:cCdPg           := aR[2]       // исправили
                         This.Button_CdPg.Caption := aR[3]
                         This.Button_CdPg.Icon    := aR[4]
                      Endif
                      ow:SetFocus('Label_0')
                      Return Nil
                      } )

     :Event(90, {|ow| // Ёкспорт в файл и закрыть окно
                      Local owc := ow:Cargo
                      Local ao  := App.Cargo
                      Local opc := owc:oWnd:Cargo    // объект Cargo -> родительского окна  - вариант 3
                      // Local opc := ow:Cargo:oWnd:Cargo
                      Local cTtl  := "ќЎ»Ѕ ј !; Ќе выбран файл дл€ экспорта !"
                      Local cTtl2 := "ERROR !; No file selected for export !"
                      Local aR
                      cTtl := IIF(ao:cLang == "RU", cTtl, cTtl2 )
                      If LEN(owc:cFile) > 0
                         aR := { owc:cPath + "\" + owc:cFile, owc:cCdPg, owc:lOpen } // исправили
                         opc:aFileExport   := aR           // вернуть - вариант 3, через Cargo родительского окна
                         M->aPubFileExport := aR           // вернуть - вариант 1
                         // App.Cargo:aFileExport      := aR - вариант 2 возврата базы и параметров
                         DO EVENTS
                         ? ProcNL(), HB_ValToExp(opc:aRet)
                         ? "       M->aPubFileExport=" + HB_ValToExp(M->aPubFileExport)+"|"
                         //SET WINDOW THIS TO ow:Name        // ќЅя«ј“≈Ћ№Ќќ !!!
                         //MsgDebug(":Event(90)","ow:Cargo:cPath=",ow:Cargo:cPath,"|",;
                         //            opc:aFileExport,M->aPubFileExport)
                         //SET WINDOW THIS TO
                         _wPost(99,ow:Name)
                      Else
                         SET WINDOW THIS TO ow:Name     // ќЅя«ј“≈Ћ№Ќќ !!!
                         AlertStop( cTtl, , "ZZZ_B_STOP64", 64, {RED} )
                         SET WINDOW THIS TO
                         ow:SetFocus('Label_0')
                         opc:aFileExport := {}          // вернуть - вариант 3, через Cargo родительского окна
                      Endif
                      Return Nil
                      } )

     :Event(101,{|ow| // Cancel
                      Local owc := ow:Cargo
                      Local opc := owc:oWnd:Cargo    // объект Cargo -> родительского окна  - вариант 3
                      opc:aFileExport := {}          // вернуть - вариант 3, через Cargo родительского окна
                      ? "------101----------", ow:Name, opc:aFileExport
                      _o2log(ow:Cargo:oWnd:Cargo , 15, ProcNL()+" ------- ѕараметры объекта : => ow:Cargo:oWnd:Cargo", .T.)
                      _wPost(99,ow:Name)
                      DO EVENTS
                      Return Nil
                      } )

     :Event(98, {|ow| // ON Release
                      Local ah := ow:Cargo:ahIcoDel
                      ?  ProcNL()
                      ?? ">>> Exit button pressed! Window: "+ow:Name
                      ? Repl(".", 10),"Delete handle icon - ow:Cargo:ahIcoDel="
                      ?? ah, HB_ValToExp(ah)
                      IF IsArray(ah)
                         AEval(ah, {|h| DestroyIcon(h) })  // удалить хендлы иконок
                      ENDIF
                      DO EVENTS
                      Return Nil
                      } )

     :Event(99, {|ow| ow:Release() } )
   END WITH

RETURN NIL

/////////////////////////////////////////////////////////////////////////////////////////////
FUNCTION myCodePage()
   LOCAL aDim, nBmpSize, nFSize, nChoice, nPos, lExit, aRet, cForm, aFntExt, cTypeRes, cMsg

   cForm := ThisWindow.Name
   IF App.Cargo:cLang == "RU"
      cMsg := " одова€ страница: "
   ELSE
      cMsg := "Code page: "
   ENDIF
   aDim := {}
   AADD( aDim, { "iFlag_Ru32"    , cMsg + "RU1251" , .F. , "", "RU1251"  , 1 } )
   AADD( aDim, { "iFlag_Ru32"    , cMsg + "RU866"  , .F. , "", "RU866"   , 2 } )
   AADD( aDim, { "iFlag_Uk32"    , cMsg + "UA866"  , .F. , "", "UA866"   , 3 } )
   AADD( aDim, { "iFlag_Uk32"    , cMsg + "UA1251" , .F. , "", "UA1251"  , 4 } )
   AADD( aDim, { "iFlag_Uk32"    , cMsg + "UA1125" , .F. , "", "UA1125"  , 5 } )
   AADD( aDim, { "iUtf32"        , cMsg + "UTF8"   , .F. , "", "UTF8"    , 6 } )
   AADD( aDim, { "iUtf32"        , cMsg + "UTF8EX" , .F. , "", "UTF8EX"  , 7 } )
   AADD( aDim, { "iUtf32"        , cMsg + "UTF16LE", .F. , "", "UTF16LE" , 8 } )
   AADD( aDim, {                                                             } )
   AADD( aDim, { "iLang32"       , cMsg + "–езерв" , .F. , "", "NONE"    ,99 } )

   aRet     := {}
   nPos     := 3
   cTypeRes := "ICO"  // "BMP"
   nBmpSize := 32
   nFSize   := App.Cargo:nFontSize 
   aFntExt  := { "DejaVu Sans Mono", "Comic Sans MS" }
   lExit    := .F.
   nChoice  := DynamicContextMenuExtend( cForm, aDim, nPos, nBmpSize, nFSize, lExit, aFntExt, cTypeRes )
   IF nChoice > 0
      aRet := { aDim[nChoice,6], aDim[nChoice,5], aDim[nChoice,2], aDim[nChoice,1] }
   ENDIF
   DO EVENTS

RETURN aRet

