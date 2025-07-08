/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2024 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 * Copyright 2024 Grigory Filatov <gfilatov@inbox.ru>
 *
 * Тестирование ToolBar / Testing TOOLBAR
 */
#define _HMG_OUTLOG

#include "hmg.ch"
#include "tsbrowse.ch"

#define SHOW_TITLE  "Testing TOOLBAREX"

FUNCTION Main()
   LOCAL cForm := "wMain" , aBColor, aTsbBClr
   LOCAL cTitle, nY, nX, nH, nW, nG, nHTsb, owc, o 

   nY := nX := 0  ;  nG  := 10  // отсупы по краям формы
   nW := Sys.ClientWidth
   nH := Sys.ClientHeight 
   aBColor  := {184, 107, 228}
   aTsbBClr := { {216, 191, 216}, {156,169,253}, {178,249,235} }
   cTitle   := App.Cargo:cTitle

   DEFINE WINDOW &cForm AT nY, nX WIDTH nW HEIGHT nH TITLE cTitle ;
      MINWIDTH 600 MINHEIGHT 600                                  ; // блокировка уменьшения размеров окна
      MAIN TOPMOST                                                ;            
      BACKCOLOR aBColor                                           ;
      ON MAXIMIZE  ( ResizeForm( ThisWindow.Object ) )            ;
      ON SIZE      ( ResizeForm( ThisWindow.Object ) )            ;
      ON INIT    _wPost( 0)                                       ;
      ON RELEASE _wSend(90)

      This.Cargo := oHmgData() ; owc := This.Cargo  // для окна создаем объект без переменных (условно пустой)
      owc:aBColor  := This.BackColor   // цвет окна
      owc:aTsbBClr := aTsbBClr         // цвет окна таблицы
      owc:cForm    := cForm            // имя окна
      owc:nG       := nG               // отступы по краям формы
      owc:nHTBar   := 0                // высота ToolBar

      myToolBar(owc)   
      nY    := owc:nHTBar + nG
      nX    := nG
      nW    := This.ClientWidth
      nH    := This.ClientHeight
      nHTsb := ( nH - nG*3 - nY ) / 3

      @ nY, nX LABEL Label_1Tsb VALUE REPL(owc:cTsbTtl1 + CRLF,5) ;
        WIDTH nW-nG*2 HEIGHT nHTsb BACKCOLOR aTsbBClr[1] BORDER 
      owc:cObjSize1 := "Label_1Tsb"         // запомним объект для изменения размеров
      nY += nHTsb + nG 

      @ nY, nX LABEL Label_2Tsb VALUE REPL(owc:cTsbTtl2 + CRLF,5) ;
        WIDTH nW-nG*2 HEIGHT nHTsb BACKCOLOR aTsbBClr[2] BORDER 
      owc:cObjSize2 := "Label_2Tsb"         // запомним объект для изменения размеров
      nY += nHTsb + nG

      @ nY, nX LABEL Label_3Tsb VALUE REPL(owc:cTsbTtl3 + CRLF,5) ;
        WIDTH nW-nG*2 HEIGHT nHTsb BACKCOLOR aTsbBClr[3] BORDER 
      owc:cObjSize3 := "Label_3Tsb"         // запомним объект для изменения размеров
      nY += nHTsb + nG

      ON KEY F1 ACTION NIL

      o := This.Object
      o:Event( 0, {|ow,ky| // после построения окна
                           This.Topmost := .F.
                           ? "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                           WindowsCoordinat(ow)     // восстановить координаты окна
                           ow:SetFocus(ow:Cargo:cObjSize1)
                           DO EVENTS
                           Return Nil
                           })
         
     o:Event({ 8,"_TBar"  }, {|ow,ky,cn|     // для др. кнопок ToolBar
                  Local av, la := .F.  
                  If IsArray(cn)
                     av := cn
                     cn := av[1]
                     la := .T.
                  Endif
                  //MsgDebug("(1)", ow:Name,ky,cn,"=?",av)
                  ky := subs(cn, 2, 1) 
                  If val(ky) == 2 .or. val(ky) == 3   // 2-ой символ число 
                     ky := cn                         
                     cn := left(cn, 1) + subs(cn, 3)
                  Endif
                  If la  ; ky := av
                  Endif
                  //MsgDebug("(2)", cn, ow:Name, ky)
                  _wPost(cn, ow, ky)
                  Return Nil
                  })
     o:Event({ 9,"_Help"  }, {|ow,ky,cn| 
                  Local cCap, oCar, cTyp, cImg
                  This.&(cn).Enabled := .F.
                  cCap := This.&(cn).Caption
                  cImg := This.&(cn).Picture
                  cTyp := This.&(cn).Type
                  oCar := This.&(cn).Cargo
                  SET WINDOW THIS TO ow
                  MsgDebug("_Help |", ow:Name,ky,cn,"|", cTyp, cCap, oCar, cImg) 
                  SET WINDOW THIS TO 
                  This.&(cn).Enabled := .T.
                  Return Nil
                  })
     o:Event({10,"_Find"  }, {|ow,ky,cn|
                  Local cCap, oCar, cTyp
                  This.&(cn).Enabled := .F.
                  cCap := This.&(cn).Caption
                  cTyp := This.&(cn).Type
                  oCar := This.&(cn).Cargo
                  SET WINDOW THIS TO ow
                  MsgDebug("_Find |", ow:Name,ky,cn,"|", cTyp, cCap, oCar) 
                  SET WINDOW THIS TO 
                  This.&(cn).Enabled := .T.
                  Return Nil
                  })
     o:Event({11,"_RecIns"}, {|ow,ky,cn|
                  Local cCap, oCar, cTyp
                  This.&(cn).Enabled := .F.
                  cCap := This.&(cn).Caption
                  cTyp := This.&(cn).Type
                  oCar := This.&(cn).Cargo
                  SET WINDOW THIS TO ow
                  MsgDebug("_RecIns |", ow:Name,ky,cn,"|", cTyp, cCap, oCar) 
                  SET WINDOW THIS TO 
                  This.&(cn).Enabled := .T.
                  Return Nil
                  })
     o:Event({12,"_RecDel"}, {|ow,ky,cn|
                  Local cCap, oCar, cTyp
                  This.&(cn).Enabled := .F.
                  cCap := This.&(cn).Caption
                  cTyp := This.&(cn).Type
                  oCar := This.&(cn).Cargo
                  SET WINDOW THIS TO ow
                  MsgDebug("_RecDel |", ow:Name,ky,cn,"|", cTyp, cCap, oCar) 
                  SET WINDOW THIS TO 
                  This.&(cn).Enabled := .T.
                  Return Nil
                  })
     o:Event({70,"_Print" }, {|ow,ky,av|
                  Local cCap, oCar, cTyp, cn
                  cn := av[1]
                  This.&(cn).Enabled := .F.
                  cCap := This.&(cn).Caption
                  cTyp := This.&(cn).Type
                  oCar := This.&(cn).Cargo
                  SET WINDOW THIS TO ow
                  MsgDebug("_Print |", ow:Name,ky,av,"|", cn, cTyp, cCap, "Cargo:", oCar, "PRINT N=",av[3]) 
                  SET WINDOW THIS TO 
                  This.&(cn).Enabled := .T.
                  Return Nil
                  })
     o:Event({89,"_Exit"  }, {|ow,ky,cn|
                  Local cCap, cTyp
                  This.&(cn).Enabled := .F.
                  cCap := This.&(cn).Caption
                  cTyp := This.&(cn).Type
                  _LogFile(.T., ">>> Exit button pressed! Window: " + ;
                                ow:Name+":" + cn + " " + cTyp + " " + cCap)
                  ky := cn
                  _wSend(99, ow)
                  Return Nil
                  })

      o:Event(90, {|ow,ky| // Release
                           Local aWin, oIni
                           ow:Hide()
                           ? "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                           ?  Repl(".", 10), "=> RELEASE WINDOW <=", ow:Name
                           ?? "... Program running time -", HMG_TimeMS( App.Cargo:tStart )
                           // сохранить размеры окна
                           aWin := { ow:Row, ow:Col, ow:Width, ow:Height }
                           App.Cargo:oIni:MAIN:aWindow := aWin
                           oIni := App.Cargo:oIni
                           Save_Ini2File( oIni )
                           Return Nil
                           })
      o:Event(99, {|ow| ow:Release()   })

      DEFINE TIMER t_1 INTERVAL 150 ACTION resize_splitbox(owc)

   END WINDOW

     CENTER WINDOW &cForm
   ACTIVATE WINDOW &cForm

RETURN NIL

*----------------------------------------------------------------------------*
PROCEDURE resize_splitbox(owc)
*----------------------------------------------------------------------------*
   //local oWnd := _WindowObj( GetActiveWindow() )
   //local owc  := oWnd:Cargo

   IF owc:hSpl # 0
      SET WINDOW THIS TO owc:cForm

      if GetWindowHeight(owc:hSpl) <> owc:nHTBar  // для корректировки высоты SPLITBOX
         owc:nHTBar := GetWindowHeight(owc:hSpl)  // для корректировки высота ToolBar
         ResizeForm()
      endif

      SET WINDOW THIS TO
   ENDIF

RETURN

*----------------------------------------------------------------------------*
INIT PROCEDURE Sets_ENV()
*----------------------------------------------------------------------------*
   LOCAL o, cIni  := hb_FNameExtSet( App.ExeName, ".ini" )

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

   SET WINDOW MAIN OFF

   IF !HB_ISOBJECT( App.Cargo ) ; App.Cargo := oHmgData()
   ENDIF
   o := App.Cargo
   o:tStart         := hb_DateTime()   // start time
   o:cTitle         := SHOW_TITLE + SPACE(10) + cFileNoPath(App.Exename)
   o:cPathTemp      := GetUserTempFolder() + "\"
   o:cPathDbf       := GetStartUpFolder() + "\DBF\"
   o:cPathRes       := GetStartUpFolder() + "\RES\"
   o:cPathStart     := GetStartUpFolder() + "\"
   o:cFontName      := "DejaVu Sans Mono" //"Arial"
   o:nFontSize      := 12
   o:cLogFile       := "_msg.log"
   o:lLogDel        := .T.
   o:cDlgFont       := "DejaVu Sans Mono"
   o:nDlgSize       := o:nFontSize + 2
   o:aDlgBColor     := { 141, 179, 226 }     // Alert* BackColor
   o:aDlgFColor     := {  0 ,  0 ,  0  }     // Alert* FontColor
   o:cDefAppIcon    := "1MG"
   o:lDebug         := .T.
   o:nMenuBmpHeight := 32
   o:aDisplayMode   := { Sys.ClientWidth , Sys.ClientHeight }
   // задание своих параметров, позволяет протестировать на другие разрешения экрана
   // setting your parameters, allows you to test for other screen resolutions
   //o:aDisplayMode   := { 1280 , 1280 }   // задать свои координаты
   o:cDisplayMode   := HB_NtoS(o:aDisplayMode[1]) + "x" + HB_NtoS(o:aDisplayMode[2])
   o:cIni           := cIni

   _SetGetLogFile( o:cLogFile )

   IF o:lLogDel ; hb_FileDelete( o:cLogFile )
   ENDIF

   // Default font
   SET FONT TO o:cFontName , o:nFontSize
   // TsBrowse                                       bold italic
   _DefineFont("Normal"  , o:cFontName, o:nFontSize  , .F., .F. )
   _DefineFont("Bold"    , o:cFontName, o:nFontSize  , .T., .F. )
   _DefineFont("Italic"  , o:cFontName, o:nFontSize-2, .F., .T. )
   _DefineFont("ItalBold", "Comic Sans MS", o:nFontSize-1, .T., .T. )
   _DefineFont("TsbEdit" , "Arial"        , o:nFontSize-1, .F., .T. )
   // Alert* font
   _DefineFont("DlgFont" , o:cDlgFont , o:nDlgSize   , .F., .F. )
   // Alert* colors
   SET MSGALERT BACKCOLOR  TO o:aDlgBColor
   SET MSGALERT FONTCOLOR  TO o:aDlgFColor
   //
   SET DEFAULT ICON TO o:cDefAppIcon
   SET WINDOW MODAL PARENT HANDLE ON
   SET TOOLTIPSTYLE BALLOON
   SET NAVIGATION EXTENDED
   SET MENUSTYLE  EXTENDED
   Set ShowRedAlert On        // увеличить фонт для окна "Program Error"
   SetMenuBitmapHeight( o:nMenuBmpHeight )

   // Проверка на запуск второй копии программы
   _HMG_MESSAGE[4] := "Trying to launch a second copy of the program:" + CRLF + ;
                      App.ExeName + CRLF + ;
                      "Launch denied !" + CRLF + _HMG_MESSAGE[4]
   SET MULTIPLE QUIT WARNING  // окно маленькое

   ? PadC( " Program start - " + HB_TTOC( hb_DateTime() ) + " ", 80, "-" )
   ? " Screen resolution:", HB_NtoS(GetDesktopWidth())+" x "+HB_NtoS(GetDesktopHeight())
   ? "Free Open Software:", Version()
   ? "     Free Compiler:", hb_Ccompiler()
   ? "  Free Gui library:", MiniGuiVersion()

   o:cIniFile := cIni
   o:lIni     := hb_FileExists(cIni)
   // доступ к ини-файлу везде в программе - App.Cargo:oIni
   o:oIni := TIniData():New(cIni, .T.):Read()

   Default o:oIni:INFO := oHmgData()
   Default o:oIni:INFO:Developed_in   := MiniGUIVersion()
   Default o:oIni:INFO:xBase_compiler := Version()
   Default o:oIni:INFO:C_compiler     := Hb_Compiler()
   Default o:oIni:INFO:Programm       := SHOW_TITLE

   Default o:oIni:MAIN := oHmgData()
   Default o:oIni:MAIN:aBClrMain    := {215, 166, 0}
   Default o:oIni:MAIN:MainFont     := { o:cFontName2 , o:nFontSize+2 , .F., .F. } 
   Default o:oIni:MAIN:aWindow      := {0, 0, 0, 0}

   Default o:oIni:PATH := oHmgData()
   Default o:oIni:PATH:PathTemp  := App.Cargo:cPathTemp 
   Default o:oIni:PATH:PathDbf   := App.Cargo:cPathDbf  
   Default o:oIni:PATH:PathRes   := App.Cargo:cPathRes  
   Default o:oIni:PATH:PathStart := App.Cargo:cPathStart

   Save_Ini2File( o )

RETURN

///////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION ResizeForm( oWnd )
   Local nG, owc, nW, nH, cObj1, cObj2, cObj3, nHTB, nHTsb
   DEFAULT oWnd := _WindowObj( GetActiveWindow() )

   IF !ISOBJECT(oWnd)
      AlertStop("Not an oWnd object !")
      RETURN NIL
   ENDIF

   nW    := This.ClientWidth
   nH    := This.ClientHeight
   owc   := oWnd:Cargo
   cObj1 := owc:cObjSize1      // объект-1 для изменения размеров
   cObj2 := owc:cObjSize2      // объект-2 для изменения размеров
   cObj3 := owc:cObjSize3      // объект-3 для изменения размеров
   nG    := owc:nG             // отступы по краям формы
   nHTB  := owc:nHTBar         // высота ToolBar

   IF owc:hSpl # 0
      nHTB := GetWindowHeight(owc:hSpl)  // для корректировки высоты SPLITBOX
   ENDIF
   nHTsb := ( nH - nG*4 - nHTB ) / 3

   This.&(cObj1).Row    := nHTB + nG 
   This.&(cObj1).Width  := nW - nG * 2
   This.&(cObj1).Height := nHTsb 

   This.&(cObj2).Row    := nHTB + nHTsb + nG*2
   This.&(cObj2).Width  := nW - nG * 2
   This.&(cObj2).Height := nHTsb

   This.&(cObj3).Row    := nHTB + nHTsb * 2 + nG*3 
   This.&(cObj3).Width  := nW - nG * 2
   This.&(cObj3).Height := nHTsb 

   oWnd:SetFocus(cObj1)

   DO EVENTS

RETURN NIL

///////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myToolBar(oWC)
   LOCAL nW, nH, hFont, aFont, cFont, nFSize, lBold, nHImg, aImg, hSpl
   LOCAL hIco, hBmp, aFrmt, cFile, cPath, aTip, nWtxt, nWCap, i
   LOCAL aImg1, aImg2, aImg3, aObj, aCap, cForm, cCap, o

   cForm  := oWC:cForm                   // имя окна
   hFont  := GetFontHandle('ItalBold')
   aFont  := GetFontParam(hFont)
   cFont  := aFont[1]
   nFSize := aFont[2]
   lBold  := aFont[3]
   nHImg  := 55   // задаём размер картинки на кнопке

   owc:cTsbTtl1 := "Menu 32x32:"
   owc:cTsbTtl2 := "Menu 48x48:"
   owc:cTsbTtl3 := "Menu "+HB_NtoS(nHImg)+"x"+HB_NtoS(nHImg)+":"

   // преобразуем ICO -> XXX так как нет в TOOLBAR поддержи .ico
   cPath  := App.Cargo:cPathTemp              // путь, куда выгружаются картинки
   aFrmt  := { "BMP", "PNG", "GIF", "JPG" }
   aImg   := { "iHelp48" , "iFind48", "iInsert48", "iDelete48", "iPrint48", "iExit48" }
   aImg1  := ARRAY(LEN(aImg))
   aImg2  := ARRAY(LEN(aImg))
   aImg3  := ARRAY(LEN(aImg))

   FOR i := 1 TO LEN(aImg)
      hIco  := LoadIconByName( aImg[i], 32, 32 )
      hBmp  := BmpFromIcon( hIco )          // вернет хендл bmp
      cFile := cPath + aImg[i] + "x32.bmp"
      HMG_SaveImage( hBmp, cFile, "bmp" )
      aImg1[i] := cFile
      DestroyIcon(hIco)
      DeleteObject(hBmp)
      DO EVENTS

      hIco  := LoadIconByName( aImg[i], 48, 48 )
      hBmp  := BmpFromIcon( hIco )          // вернет хендл bmp
      cFile := cPath + aImg[i] + "x48.png"
      HMG_SaveImage( hBmp, cFile, "png" )
      aImg2[i] := cFile
      DestroyIcon(hIco)
      DeleteObject(hBmp)
      DO EVENTS

      hIco  := LoadIconByName( aImg[i], nHImg, nHImg )
      hBmp  := BmpFromIcon( hIco )          // вернет хендл bmp
      cFile := cPath + aImg[i] + "x55.png"
      HMG_SaveImage( hBmp, cFile, "png" )
      aImg3[i] := cFile
      DestroyIcon(hIco)
      DeleteObject(hBmp)
      DO EVENTS

   NEXT

   aObj   := { "_Help" , "_Find"     , "_RecIns"   , "_RecDel"      , "_Print"  , "_Exit" }
   aCap   := { "Help"  , "Documents" , "Add recno" , "Delete recno" , "Print"   , "Exit"  }
   aTip   := { "Help" , ;
                "F7 - Documents: search, work with records" , ;
                "Ins - add recno", ;
                "Del - delete recno", ;
                "F5 - Print", ;
                "Exit the program" }

   nWtxt  := nW := nH := 0
   // расчёт по тексту
   FOR i := 1 TO LEN(aCap)
      cCap := aCap[ i ]
      //nWCap := GetTxtWidth(cMenu, nFSize, cFont, lBold )
      nWCap := GetTextWidth( NIL, cCap, hFont )
      nWTxt := MAX(nWTxt,nWCap)
   NEXT
   nWTxt := IIF(nWTxt < nHImg, nHImg, nWTxt )   // nHImg-высота bmp
   nW    := nWTxt + 5                           // ширина кнопки
   nH    := nHImg + 5 + nFSize + 5              // высота кнопки  

   DEFINE SPLITBOX HANDLE hSpl
   DEFINE TOOLBAREX ToolBar_1 CAPTION owc:cTsbTtl1 BUTTONSIZE nW - 6, nH FLAT FONT cFont SIZE nFSize BOLD 

      // обычная кнопка
      BUTTON _Help   CAPTION aCap[1] PICTURE aImg1[1] TOOLTIP aTip[1] ACTION _wPost(This.Name,, This.Name ) SEPARATOR
      This._Help.Cargo := oHmgData() ; o := This._Help.Cargo
      o:cObjTB := "ToolBar_1"        ; o:cImage := aImg1[1]   // пример

      BUTTON _Find   CAPTION aCap[2] PICTURE aImg1[2] TOOLTIP aTip[2] ACTION _wPost(This.Name,, This.Name ) SEPARATOR 
      This._Find.Cargo := oHmgData() ; o := This._Find.Cargo
      o:cObjTB := "ToolBar_1"        ; o:cImage := aImg1[2]   // пример

      BUTTON _RecIns CAPTION aCap[3] PICTURE aImg1[3] TOOLTIP aTip[3] ACTION _wPost(This.Name,, This.Name ) SEPARATOR 
      This._RecIns.Cargo := oHmgData() ; o := This._RecIns.Cargo
      o:cObjTB := "ToolBar_1"          ; o:cImage := aImg1[3]   // пример

      BUTTON _RecDel CAPTION aCap[4] PICTURE aImg1[4] TOOLTIP aTip[4] ACTION _wPost(This.Name,, This.Name ) SEPARATOR 
      This._RecDel.Cargo := oHmgData() ; o := This._RecDel.Cargo
      o:cObjTB := "ToolBar_1"          ; o:cImage := aImg1[4]   // пример

      // кнопка с меню
      BUTTON _Print  CAPTION aCap[5] PICTURE aImg1[5] TOOLTIP aTip[5] ACTION _wPost(This.Name,, {"_Print",This.Name,0} ) SEPARATOR DROPDOWN
      This._Print.Cargo := oHmgData() ; o := This._Print.Cargo
      o:cObjTB := "ToolBar_1" ; o:cObj := "_Print"  ; o:cImage := aImg1[5]   // пример
         DEFINE DROPDOWN MENU BUTTON _Print
            ITEM 'Print-1 :' ACTION Nil  DISABLED  DEFAULT  ICON 'iPrint48'
            SEPARATOR
            ITEM 'Form - 1' NAME PRN_001 ACTION _wPost("_Print", , {"_Print",This.Name,1}) ICON 'iPrint48'
            ITEM 'Form - 2' NAME PRN_002 ACTION _wPost("_Print", , {"_Print",This.Name,2}) ICON 'iPrint48'
         END MENU
      // обычная кнопка
      BUTTON _Exit   CAPTION aCap[6] PICTURE aImg1[6] TOOLTIP aTip[6] ACTION _wPost(This.Name,, This.Name ) 
      This._Exit.Cargo := oHmgData() ; o := This._Exit.Cargo

   END TOOLBAR

   DEFINE TOOLBAREX ToolBar_2 CAPTION owc:cTsbTtl2  BUTTONSIZE nW, nH FLAT FONT cFont SIZE nFSize BOLD 

      // обычная кнопка
      BUTTON _2Help   CAPTION aCap[1] PICTURE aImg2[1] TOOLTIP aTip[1] ACTION _wPost("_TBar",, This.Name ) SEPARATOR AUTOSIZE
      BUTTON _2Find   CAPTION aCap[2] PICTURE aImg2[2] TOOLTIP aTip[2] ACTION _wPost("_TBar",, This.Name ) SEPARATOR AUTOSIZE
      BUTTON _2RecIns CAPTION aCap[3] PICTURE aImg2[3] TOOLTIP aTip[3] ACTION _wPost("_TBar",, This.Name ) SEPARATOR AUTOSIZE
      BUTTON _2RecDel CAPTION aCap[4] PICTURE aImg2[4] TOOLTIP aTip[4] ACTION _wPost("_TBar",, This.Name ) SEPARATOR AUTOSIZE
      // кнопка с меню
      BUTTON _2Print  CAPTION aCap[5] PICTURE aImg2[5] TOOLTIP aTip[5] ACTION _wPost("_TBar",, {"_2Print",This.Name,0} ) SEPARATOR AUTOSIZE DROPDOWN
         DEFINE DROPDOWN MENU BUTTON _2Print
            ITEM 'Print-2 :' ACTION Nil  DISABLED  DEFAULT  ICON 'iPrint48'
            SEPARATOR
            ITEM 'Form - 1' NAME PRN_001 ACTION _wPost("_TBar", , {"_2Print",This.Name,1}) ICON 'iPrint48'
            ITEM 'Form - 2' NAME PRN_002 ACTION _wPost("_TBar", , {"_2Print",This.Name,2}) ICON 'iPrint48'
         END MENU
      // обычная кнопка
      BUTTON _2Exit   CAPTION aCap[6] PICTURE aImg2[6] TOOLTIP aTip[6] ACTION _wPost("_TBar",, This.Name ) AUTOSIZE

   END TOOLBAR

   DEFINE TOOLBAREX ToolBar_3 CAPTION owc:cTsbTtl3 BUTTONSIZE nW, nH FLAT FONT cFont SIZE nFSize BOLD 

      // обычная кнопка
      BUTTON _3Help   CAPTION aCap[1] PICTURE aImg3[1] TOOLTIP aTip[1] ACTION _wPost("_TBar",, This.Name ) SEPARATOR AUTOSIZE
      BUTTON _3Find   CAPTION aCap[2] PICTURE aImg3[2] TOOLTIP aTip[2] ACTION _wPost("_TBar",, This.Name ) SEPARATOR AUTOSIZE
      BUTTON _3RecIns CAPTION aCap[3] PICTURE aImg3[3] TOOLTIP aTip[3] ACTION _wPost("_TBar",, This.Name ) SEPARATOR AUTOSIZE
      BUTTON _3RecDel CAPTION aCap[4] PICTURE aImg3[4] TOOLTIP aTip[4] ACTION _wPost("_TBar",, This.Name ) SEPARATOR AUTOSIZE
      // кнопка с меню
      BUTTON _3Print  CAPTION aCap[5] PICTURE aImg3[5] TOOLTIP aTip[5] ACTION _wPost("_TBar",, {"_3Print",This.Name,0} ) SEPARATOR AUTOSIZE DROPDOWN
         DEFINE DROPDOWN MENU BUTTON _3Print
            ITEM 'Print-3 :' ACTION Nil  DISABLED  DEFAULT  ICON 'iPrint48'
            SEPARATOR
            ITEM 'Form - 1' NAME PRN_001 ACTION _wPost("_TBar", , {"_3Print",This.Name,1}) ICON 'iPrint48'
            ITEM 'Form - 2' NAME PRN_002 ACTION _wPost("_TBar", , {"_3Print",This.Name,2}) ICON 'iPrint48'
         END MENU
      // обычная кнопка
      BUTTON _3Exit   CAPTION aCap[6] PICTURE aImg3[6] TOOLTIP aTip[6] ACTION _wPost("_TBar",, This.Name ) AUTOSIZE

   END TOOLBAR
   END SPLITBOX

   owc:hSpl   := hSpl                   // хендл объекта SPLITBOX
   owc:nHTBar := GetWindowHeight(hSpl)  // для корректировки высота ToolBar

RETURN NIL

///////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION WindowsCoordinat(ow)
   // считать параметры из ини-файла
   Local aWin := App.Cargo:oIni:MAIN:aWindow

   IF IsArray(aWin)
      IF aWin[1] < 0 .OR. aWin[2] < 0
         // это скрытие окна
      ELSEIF aWin[3] <= 0 .OR. aWin[4] <= 0
         // это сбой координат окна
      ELSE
         ow:Row    := aWin[1]
         ow:Col    := aWin[2]
         ow:Width  := aWin[3]
         ow:Height := aWin[4]
      ENDIF
      // проверка на размер тек.экрана
      IF aWin[3] > App.Cargo:aDisplayMode[1]
         ow:Width  := aWin[3] := App.Cargo:aDisplayMode[1]
      ENDIF
      IF aWin[4] > App.Cargo:aDisplayMode[2]
         ow:Height := aWin[4] := App.Cargo:aDisplayMode[2]
      ENDIF
   ENDIF

RETURN NIL

///////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Save_Ini2File( oIni )  // запись в ини-файл

   oIni:cCommentBegin  := " Modify: " + hb_TtoC( hb_DateTime() )
   oIni:Write()  // НЕ UTF8, т.е. нет BOM на выходе

RETURN NIL

*-----------------------------------------------------------------------------*
#pragma BEGINDUMP

#include "hbapi.h"
#include "windows.h"
#include <shellapi.h>

extern HBITMAP Icon2Bmp( HICON hIcon );

HB_FUNC( BMPFROMICON )
{
HICON hIcon = ( HICON ) hb_parnl( 1 );

hb_retnl( ( LONG ) Icon2Bmp( hIcon ) );
}

#pragma ENDDUMP
