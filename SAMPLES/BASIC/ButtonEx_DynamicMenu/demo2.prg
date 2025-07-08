/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2019 Grigory Filatov <gfilatov@inbox.ru>
 * Copyright 2019 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2019 Verchenko Andrey <verchenkoag@gmail.com>
 *
 * Динамическое меню. Изменение размеров фонта и картинок на кнопках
 * Dynamic menu Resize font and button images
 * События на окне: сообщения с This.Name  - создается среда This для окна
 * События на окне: сообщения с This.index - создается среда This для контрола
 * Events on the window: messages with This.Name - this environment is created for the window
*/

#define  SHOW_TITLE  "Dynamic menu Resize font and button images"

#include "hmg.ch"

REQUEST MyTest
///////////////////////////////////////////////////////////////////////////////
FUNCTION Main()
   LOCAL cIcon, cFont, nFontSize, nFSBtn, nUnchangPart, nFSize, cText, lBold
   LOCAL aBtn, aFC1Btn, aFC2Btn, nI, nK, nY, nX, nW, nH, nG, cN, aN, nS, cHlbl
   LOCAL nBtnH, nBtnW, aGrOver, aGrFill, aGrOverX, aGrFillX, nWidth, nHeight
   LOCAL aTmp, nImgSize

   SetsEnv()  // loading the program environment -> demo_misc.prg

   // Button Colors
   aGrOver  := { { 0.5, CLR_BLACK, CLR_VK     }, { 0.5, CLR_VK    , CLR_BLACK } }
   aGrFill  := { { 0.5, CLR_VK   , CLR_WHITE  }, { 0.5, CLR_WHITE , CLR_VK    } }
   aGrOverX := { { 0.5, CLR_RED  , CLR_HRED   }, { 0.5, CLR_HRED  , CLR_RED   } }
   aGrFillX := { { 0.5, CLR_HRED , CLR_WHITE  }, { 0.5, CLR_WHITE , CLR_HRED  } }

   aBtn := {} // 1.NameObj     2.Text                3.ExtFuncRun or CodeBlock                     4.Image.5         6.Colors.7          8.HotKey     9.nFSize
   AADD( aBtn, { "Btn_01", "1. Selection menu (pg)", {|p1,p2,p3,p4| MyTest(1,p1,p2,p3,p4)} , "Santa1" , "Santa2" , aGrOver , aGrFill , { VK_F1, 49 }, 0 } )
   AADD( aBtn, { "Btn_02", "2. Selection menu (pg)", {|           | MyTest(2)            } , "Folder1", "Folder2", aGrOver , aGrFill , { VK_F2, 50 }, 0 } )
   AADD( aBtn, { "Btn_03", "3. Selection menu (pg)", "MyTest(3)"                           , "HP1"    , "HP2"    , aGrOver , aGrFill , { VK_F3, 51 }, 0 } )
   AADD( aBtn, { "Btn_04", "4. Selection menu (pg)", "MyTest(4)"                           , "HMG1"   , "HMG2"   , aGrOver , aGrFill , { VK_F4, 52 }, 0 } )
   AADD( aBtn, { "Btn_05", "Exit programm"         , "MyExit(99)"                          , "Exit1"  , "Exit2"  , aGrOverX, aGrFillX,   VK_ESCAPE  , 0 } )

   // поставим две строки в меню / put two lines in the menu
   aBtn[4,2] := "4. Selection menu (pg) line-1" + CRLF + "extra line-2 (pg)"

   nY := nX := nG := 20
   nW        := System.ClientWidth * 0.4  // ширина 40% от ширины экрана компьютера
   nH        := System.ClientHeight       // высота 100% от высоты экрана компьютера
   cIcon     := "1MAIN_ICO"
   cFont     := "Microsoft Sans Serif" //'Tahoma'
   nFontSize := 20
   aFC1Btn   := BLACK
   aFC2Btn   := YELLOW
   nFSBtn    := 28
   nUnchangPart := 200  // неизменяемая часть вверху окна

   DEFINE WINDOW wMain AT 0, 0 WIDTH nW HEIGHT nH ;
       MINWIDTH 600 MINHEIGHT 500    ;
       ICON       cIcon              ;
       TITLE      SHOW_TITLE         ;
       BACKCOLOR  SILVER             ;
       MAIN NOMAXIMIZE               ;
       FONT cFont SIZE nFontSize     ;
       ON INIT  {|| _wPost(0) }      ; // можно назначить здесь запуск по клавише
       ON SIZE ResizeForm(nUnchangPart,nG)

       nW := This.ClientWidth    // ширина внутри окна
       nH := This.ClientHeight   // высота внутри окна

       cHlbl := 50 // высота Label_0
       @ nY, 0 LABEL Label_0 WIDTH nW HEIGHT cHlbl VALUE SHOW_TITLE ;
         BOLD FONTCOLOR BLACK TRANSPARENT CENTERALIGN VCENTERALIGN

       cText   := wMain.Label_0.Value       // получить текст объекта
       lBold   := wMain.Label_0.FontBold
       nS      := GetFontSize4Text( cText, cFont, , lBold, This.Label_0.ClientWidth  - 2, ;
                                                           This.Label_0.ClientHeight - 2 )    // -> demo_misc.prg
       wMain.Label_0.Fontsize := nS         // изменить размер фонта

       @ 50 + nG, nG BUTTONEX Btn_Font CAPTION cFont                       ;
         WIDTH 220 HEIGHT 50 SIZE 12 FONTCOLOR BLACK NOXPSTYLE HANDCURSOR  ;
         ACTION  {|| ChangeFont("Label_0"), This.Label_0.SetFocus }

       @ This.Btn_Font.Row + This.Btn_Font.Height + nG/2, nG BUTTONEX Btn_F10 CAPTION "F10"                        ;
         WIDTH 220 HEIGHT 50 SIZE 12 BOLD FONTCOLOR BLACK NOXPSTYLE HANDCURSOR  ;
         ACTION  {|| _wSend(10), This.Label_0.SetFocus }

       @ 50 + nG, 260 LABEL Label_1 WIDTH nW - 260 HEIGHT nUnchangPart - 50 - nG ;
         VALUE "Font size:"+CRLF+"Image height:"+CRLF+"Button height:"  ;
         SIZE 12 FONTCOLOR BLUE TRANSPARENT

         This.Label_0.Cargo := aBtn  // поместим массив МЕНЮ в Cargo Label_0

         nBtnW := nW - nG * 2
         nBtnH := ( nH - nUnchangPart - nG*(Len(aBtn)) ) / Len(aBtn)
         nY    := nUnchangPart

         // Расчет фонтов кнопок  START ------------------------------
         nWidth  := nBtnW - nBtnH - 10*2    // примерная ширина кнопки внутри
         nHeight :=         nBtnH - 10      // примерная высота кнопки внутри

         FOR nI := 1 TO Len(aBtn)
            // просчитать и записать в массив меню максимальный размер фонта
            aBtn[ nI ][9] := GetFontSize4Text( aBtn[ nI ][2], cFont, , lBold, nWidth, nHeight )
         NEXT

         nS := 0                                        // для строк БЕЗ CRLF
         FOR nI := 1 TO Len(aBtn)
             IF ! CRLF $ aBtn[ nI ][2]
                IF Empty(nS) ; nS := aBtn[ nI ][9]
                ENDIF
                aBtn[ nI ][9] := Min( nS, aBtn[ nI ][9] )
             ENDIF
         NEXT

         nS := 0                                        // для строк  C  CRLF
         FOR nI := 1 TO Len(aBtn)
             IF CRLF $ aBtn[ nI ][2]
                IF Empty(nS) ; nS := aBtn[ nI ][9]
                ENDIF
                aBtn[ nI ][9] := Min( nS, aBtn[ nI ][9] )
             ENDIF
         NEXT
         // Расчет фонтов кнопок  STOP -----------------------------

         FOR nI := 1 TO Len(aBtn)

           cN := aBtn[nI][1]
           nImgSize := INT(nBtnH - 5*2)  // размер картинки
           @ nY, nX BUTTONEX &cN WIDTH nBtnW HEIGHT nBtnH                         ;
             CAPTION aBtn[nI][2] PICTURE aBtn[nI][4] IMAGESIZE nImgSize, nImgSize ;
             BOLD SIZE aBtn[nI][9]                                                ; 
             FLAT NOXPSTYLE HANDCURSOR NOTABSTOP                                  ;
             BACKCOLOR aBtn[nI][6] GRADIENTFILL aBtn[nI][7]                       ;
             ON MOUSEHOVER ( This.Fontcolor := aFC2Btn, This.GradientFill := This.Cargo[2][7] ,;
                             This.Picture   := This.Cargo[2][5] ) ;
             ON MOUSELEAVE ( This.Fontcolor := aFC1Btn, This.GradientOver := This.Cargo[2][6] ,;
                             This.Picture   := This.Cargo[2][4] ) ;
             ACTION         _wPost( 1, This.Index ) ;
             ON INIT    { || This.Cargo := { nI, aBtn[ nI ] } }

             aN := aBtn[ nI ][8]              // HotKey array

             IF ! Empty( aN )                 // Define HotKey
                IF HB_ISARRAY( aN )
                      FOR nK := 1 TO Len( aN )
                   _DefineHotKey( This.Name , 0 , aN[ nK ] , hb_MacroBlock( "_wPost(2, , '"+cN+"')" ) )
                      NEXT
                ELSE
                   _DefineHotKey( This.Name , 0 , aN       , hb_MacroBlock( "_wPost(2, , '"+cN+"')" ) )
                ENDIF
             ENDIF

             nY += nBtnH + nG   // Row следующей кнопки

         NEXT

         ON KEY F10 ACTION _wPost(10)

         WITH OBJECT This.Object
            :Event( 0, {|        | InfoFormObj()                             } )

            :Event( 1, {|obt,ky  | ky := HMG_SetMousePos(This.&(obt:Name).Handle), InkeyGui(200), This.&(obt:Name).Setfocus, ;
                                         HMG_MouseSet(ky), MySelectThis()    } )
            :Event( 2, {|ow,ky,cn| This.&(cn).Setfocus, _PushKey( VK_SPACE ) } )

            :Event(10, {|        | Alert2Dim(aBtn) /* ==> demo_misc.prg */   } )
         END WITH

   END WINDOW

   //CENTER   WINDOW wMain
   ACTIVATE WINDOW wMain ON INIT bInitAction( aBtn )

RETURN NIL

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION bInitAction( aBtn )
   LOCAL nI, cN, nW, cF, nF, lB

   This.Minimize ; This.Restore ; DO EVENTS

   nW := This.ClientWidth

   FOR nI := 1 TO Len( aBtn )
       cN := aBtn[ nI ][1]
       This.&(cN).Width := nW - This.&(cN).Col * 2
   NEXT

   This.Label_0.Width := nW
   cN := This.Label_0.Value       // получить текст объекта
   lB := This.Label_0.FontBold
   cF := This.Label_0.FontName
   nF := This.Label_0.FontSize

   nI := GetFontSize4Text( cN, cF, , lB, This.Label_0.ClientWidth  - 2, ;
                                         This.Label_0.ClientHeight - 2 )    // -> demo_misc.prg
   This.Label_0.Fontsize := nI   // изменить размер фонта

   This.Label_1.Width := nW - This.Label_1.Col - 10

   This.Label_0.SetFocus
   DO EVENTS

RETURN Nil

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION InfoFormObj()
   LOCAL nI, aBtn, cN, nBtnH, nImgH, cText, cFont, nFSize, aFSize := {}
   LOCAL lBold, nWlbl, nHlbl, nS

   aBtn  := This.Label_0.Cargo

   FOR nI := 1 TO  Len(aBtn)
      cN := aBtn[nI,1]
      nBtnH := This.&(cN).Height
      nImgH := This.&(cN).ImageHeight
      AADD( aFSize, This.&(cN).FontSize )
   NEXT

   cText := "Font size: " + HB_NtoS(nFSize) + " " + HB_ValToExp(aFSize)  + CRLF
   cText += "Image height:    " + HB_NtoS(nImgH) + CRLF
   cText += "Button height: " + HB_NtoS(nBtnH)
   This.Label_1.Value := cText

   nFSize  := This.Label_1.Fontsize    // размер фонта
   cFont   := This.Label_1.Fontname    // имя фонта
   lBold   := wMain.Label_1.FontBold
   nFSize  := wMain.Label_1.Fontsize
   nHlbl   := wMain.Label_1.Height
   nWlbl   := wMain.Label_1.Width
   nS      := GetFontSize4Text( cText, cFont, , lBold, nWlbl, nHlbl )    // -> demo_misc.prg
   wMain.Label_1.Fontsize := nS         // изменить размер фонта

RETURN NIL

////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION ResizeForm(nUnchangPart,nG)
   LOCAL nI, nY, nW, nH, cN, nBtnW, nBtnH, aBtn, cFont, nFSize, cHlbl
   LOCAL lBold, cText, nWidth, nHeight, nImgSize, nSFnew

   nW    := This.ClientWidth    // новая ширина внутри окна
   nH    := This.ClientHeight   // новая высота внутри окна

   aBtn  := This.Label_0.Cargo     // массив меню формы
   cFont := This.Btn_Font.Caption  // имя фонта по всей форме

   nBtnW := nW - nG * 2
   nBtnH := ( nH - nUnchangPart - nG*(Len(aBtn)) ) / Len(aBtn)
   nY    := nUnchangPart

   FOR nI := 1 TO  Len(aBtn)
      cN := aBtn[nI,1]
      This.&(cN).Row    := nY
      This.&(cN).Width  := nBtnW
      This.&(cN).Height := nBtnH
      nY += nBtnH + nG   // Row следующей кнопки
      // получить размер иконки кнопки в зависимости от высоты
      nImgSize := INT(nBtnH - 5*2)  // размер картинки
      //  IMAGESIZE nImgSize, nImgSize ;
      This.&(cN).ImageWidth := nImgSize
      This.&(cN).ImageHeight := nImgSize

      This.&(cN).Picture := aBtn[nI,4]

      // получить размер фонта кнопки в зависимости
      // от текста наименования и высоты и ширины кнопки
      cText   := This.&(cN).Caption
      lBold   := This.&(cN).FontBold
      nFSize  := This.&(cN).Fontsize
      nWidth  := nBtnW - 10*2 - nImgSize       // примерная ширина кнопки внутри
      nHeight := nBtnH - 10                    // примерная высота кнопки внутри
      nSFnew  := GetFontSize4Text( cText, cFont, , lBold, nWidth, nHeight )    // -> demo_misc.prg
      This.&(cN).Fontname := cFont
      This.&(cN).Fontsize := nSFnew     // новый размер фонта на кнопке
   NEXT

   cHlbl  := This.Label_0.Height      // получить высоту объекта
   cText  := This.Label_0.Value       // получить текст объекта
   lBold  := This.Label_0.FontBold
   nFSize := This.Label_0.Fontsize
   // получить размер фонта для текста по ширине и высоте --> demo_misc.prg
   nSFnew := GetFontSize4Text( cText, cFont, , lBold, nW, cHlbl )
   This.Label_0.Width    := nW        // изменить ширину
   This.Label_0.Fontname := cFont     // изменить имя фонта
   This.Label_0.Fontsize := nSFnew    // изменить размер фонта

   This.Label_1.Width  := nW - This.Label_1.Col      // изменить ширину Label_1
   InfoFormObj()  // перепоказ информации об объектах

RETURN NIL

////////////////////////////////////////////////////////////////////////////
// все параметры внутри функции (нет повтора текстов в блоках кода)
// all parameters inside the function (there is no repetition of texts in blocks of code)
STATIC FUNCTION MySelectThis()
   LOCAL oWnd := ThisWindow.Object     // объект окна
   LOCAl cWnd := oWnd:Name             // имя окна
   LOCAL nWnd := oWnd:Index            // индекс окна
   LOCAL cTit := oWnd:Title            // заголовок окна
   LOCAL nBtn := This.Cargo[1]         // номер нажатой кнопки
   LOCAL aSel := This.Cargo[2]         // данные из aBtn[nI] выбранной кнопки
   LOCAL aBtn := This.Label_0.Cargo    // массив исходный по кнопкам
   LOCAL cTxt := aBtn[ nBtn ][2]       // текст нажатой кнопки
   LOCAL cRun := aBtn[ nBtn ][3], bRun // запуск внешней функции
   LOCAL cFrm := ThisWindow.Name       // имя окна др. способ
   LOCAL cMsg := ""
   LOCAL cRet := ""

   // тестовый вывод значений / test output values
   AEval({cFrm, cWnd, nWnd, cTit, nBtn, cTxt}, {|x| cMsg += cValToChar(x) + CRLF })

   IF     Empty( cRun )

   ELSEIF HB_ISCHAR( cRun ) .and. AT("exit",LOWER(cRun)) > 0
      oWnd:Release()

   ELSE

      HMG_Alert( Left(cMsg, Len(cMsg)-2) , , 'SELECT' )
      IF HB_ISBLOCK( cRun )
         bRun := cRun
      ELSE
         bRun := hb_MacroBlock( cRun )
      ENDIF
      Eval( bRun, oWnd, nBtn, aSel, aBtn )

   ENDIF

   oWnd:SetFocus('Label_0')  // This.Label_0.SetFocus

RETURN nBtn

////////////////////////////////////////////////////////////////////////////
// Function: вызвать стандартное меню шрифт для Windows
STATIC FUNCTION ChangeFont(cObjLbl)
   LOCAL cForm := ThisWindow.Name, cObj := This.Name
   Local nI, aBtn, cN, aF, cFont, cObjB

   aBtn := This.Label_0.Cargo
   cObjB := aBtn[1,1]  // имя объекта первой кнопки

   aF := { GetProperty(cForm, cObjB, "FontName"), GetProperty(cForm, cObjB, "FontSize") ,;
           GetProperty(cForm, cObjB, "FontBold"), GetProperty(cForm, cObjB, "FontItalic")  }
   //aF := GetFont ( 'Arial' , 12 , .f. , .t. , {0,0,0} , .f. , .f. , 0 )
   aF := GetFont( aF[1] , aF[2], aF[3], aF[4] , {0,0,0} , .f. , .f. , 0 )
   If empty ( aF[1] )
      MsgDebug("Attention ! The font is not defined! There is no such FONT NAME !",aF)
   Else
      cFont := aF[1]
      SetProperty(cForm, cObj, "Caption", cFont )

      // перересовка объектов на форме
      // wMain.&(cObjLbl).Fontname := cFont
      SetProperty(cForm, cObjLbl, "FontName", cFont )

      For nI := 1 TO  Len(aBtn)
         cN := aBtn[nI,1]
         This.&(cN).Fontname := cFont
      Next

   EndIf

RETURN NIL

//////////////////////////////////////////////////////
FUNCTION MyTest( nPar, oWnd, nBtn, aSel, aBtn )
   MsgDebug( nPar, pCount(), Valtype(oWnd), Valtype(nBtn), Valtype(aSel), Valtype(aBtn) ) 
RETURN Nil
