/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2024 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * “естирование ToolBar / Testing TOOLBAR
 */
#define _HMG_OUTLOG

#include "hmg.ch"
#include "tsbrowse.ch"

#define SHOW_TITLE  "Testing TOOLBAREX"

FUNCTION Main()
   LOCAL cForm := "wMain" , aBColor, aTsbBClr
   LOCAL cTitle, nY, nX, nH, nW, nG, owc, o 

   nY := nX := 0  ;  nG  := 10  // отсупы по кра€м формы
   nW := Sys.ClientWidth
   nH := Sys.ClientHeight
   aBColor  := {184, 107, 228}
   aTsbBClr := {216, 191, 216}
   cTitle   := App.Cargo:cTitle

   DEFINE WINDOW &cForm AT nY, nX WIDTH nW HEIGHT nH TITLE cTitle ;
      MINWIDTH 600 MINHEIGHT 600                                  ; // блокировка уменьшени€ размеров окна
      MAIN TOPMOST                                                ;            
      BACKCOLOR aBColor                                           ;
      ON MAXIMIZE ( ResizeForm( ThisWindow.Object ) )             ;
      ON SIZE     ( ResizeForm( ThisWindow.Object ) )             ;
      ON INIT    _wPost( 0)                                       ;
      ON RELEASE _wSend(90)

      This.Cargo := oHmgData() ; owc := This.Cargo  // дл€ окна создаем объект без переменных (условно пустой)
      owc:aBColor  := This.BackColor   // цвет окна
      owc:aTsbBClr := aTsbBClr         // цвет окна таблицы
      owc:cForm    := cForm            // им€ окна
      owc:nG       := nG               // отступы по кра€м формы
      owc:nHTBar   := 0                // высота ToolBar

      myToolBar(owc)   
      nY  := owc:nHTBar + nG
      nX  := 0
      nW  := This.ClientWidth
      nH  := This.ClientHeight

      @ nY+nG, nX+nG LABEL Label_Tsb VALUE "" WIDTH nW-nG*2 HEIGHT nH-nG*2-nY BACKCOLOR aTsbBClr BORDER 
      owc:cObjSize1 := "Label_Tsb"         // запомним объект дл€ изменени€ размеров

      ON KEY F1     ACTION NIL

      o := This.Object
      o:Event( 0, {|ow,ky| // после построени€ окна
                           This.Topmost := .F.
                           ? "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                           WindowsCoordinat(ow)     // восстановить координаты окна
                           ow:SetFocus("Label_Tsb")
                           DO EVENTS
                           Return Nil
                           })

      o:Event({ 9,"_Help"  }, {|ow,ky,cn| This.&(cn).Enabled := .F. , _SetThisFormInfo(ow) ,;  // ќЅя«ј“≈Ћ№Ќќ
                                          MsgDebug(ow:Name,ky,cn,"|",This.&(cn).caption,This.&(cn).Picture) ,;
                                          _SetThisFormInfo() , This.&(cn).Enabled := .T.   } )

      o:Event({10,"_Find"  }, {|ow,ky,cn| This.&(cn).Enabled := .F. , _SetThisFormInfo(ow) ,;  // ќЅя«ј“≈Ћ№Ќќ
                                          MsgDebug(ow:Name,ky,cn,"|",This.&(cn).caption,This.&(cn).Picture) ,;
                                          _SetThisFormInfo() , This.&(cn).Enabled := .T.   } )

      o:Event({11,"_RecIns"}, {|ow,ky,cn| This.&(cn).Enabled := .F. , _SetThisFormInfo(ow) ,;  // ќЅя«ј“≈Ћ№Ќќ
                                          MsgDebug(ow:Name,ky,cn,"|",This.&(cn).caption,This.&(cn).Picture) ,;
                                          _SetThisFormInfo() , This.&(cn).Enabled := .T.   } )

      o:Event({12,"_RecDel"}, {|ow,ky,cn| This.&(cn).Enabled := .F. , _SetThisFormInfo(ow) ,;  // ќЅя«ј“≈Ћ№Ќќ
                                          MsgDebug(ow:Name,ky,cn,"|",This.&(cn).caption,This.&(cn).Picture) ,;
                                          _SetThisFormInfo() , This.&(cn).Enabled := .T.   } )

      o:Event({70,"_Print" }, {|ow,ky,av| // кнопка печать
                                          Local cn    := av[1]
                                          Local cMenu := av[2]
                                          Local nForm := av[3]
                                          SET WINDOW THIS TO ow   // ќЅя«ј“≈Ћ№Ќќ
                                          This.&(cn).Enabled := .F. 
                                          MsgDebug(ow:Name,ky,cn,"|",This.&(cn).caption,This.&(cn).Picture,;
                                                     "(2) cMenu=",cMenu,"(3) nForm=",nForm)
                                          SET WINDOW THIS TO 
                                          This.&(cn).Enabled := .T. 
                                          Return Nil
                                          } )

      o:Event({89,"_Exit"  }, {|ow| _LogFile(.T., ">>> Exit button pressed! Window: "+ow:Name), _wSend(99) } )

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

   END WINDOW

     CENTER WINDOW &cForm
   ACTIVATE WINDOW &cForm

RETURN NIL

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
   // задание своих параметров, позвол€ет протестировать на другие разрешени€ экрана
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
   Set ShowRedAlert On        // увеличить фонт дл€ окна "Program Error"
   SetMenuBitmapHeight( o:nMenuBmpHeight )

   // ѕроверка на запуск второй копии программы
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
   Local nG, owc, nW, nH, cObj1, nHTB
   DEFAULT oWnd := _WindowObj( GetActiveWindow() )

   IF !ISOBJECT(oWnd)
      AlertStop("Not an oWnd object !")
      RETURN NIL
   ENDIF

   nW    := This.ClientWidth
   nH    := This.ClientHeight
   owc   := oWnd:Cargo
   cObj1 := owc:cObjSize1      // объект дл€ изменени€ размеров
   nG    := owc:nG             // отступы по кра€м формы
   nHTB  := owc:nHTBar         // высота ToolBar

   IF owc:hSpl # 0
      nHTB := GetWindowHeight(owc:hSpl)  // дл€ корректировки высоты SPLITBOX
      This.&(cObj1).Row := nHTB + nG 
   ENDIF

   This.&(cObj1).Width  := nW - nG * 2
   This.&(cObj1).Height := nH - nHTB - nG * 2

   oWnd:SetFocus(cObj1)

   DO EVENTS

RETURN NIL

///////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION myToolBar(oWC)
   LOCAL nW, nH, hFont, aFont, cFont, nFSize, lBold, nHImg, aImg, hSpl
   LOCAL aImg1, aObj, aCap, cMenu, hIco, hBmp, aFrmt, cFile, cPath, cForm
   LOCAL cCap, aTip, nWtxt, nWCap, cObj, i

   cForm  := oWC:cForm                   // им€ окна
   hFont  := GetFontHandle('ItalBold')
   aFont  := GetFontParam(hFont)
   cFont  := aFont[1]
   nFSize := aFont[2]
   lBold  := aFont[3]
   nHImg  := 48 // 32,55  - задаЄм размер картинки на кнопке

   // преобразуем ICO -> XXX так как нет в TOOLBAR поддержи .ico
   cPath  := App.Cargo:cPathTemp              // путь, куда выгружаютс€ картинки
   aFrmt  := { "BMP", "PNG", "GIF", "JPG" }
   aImg   := { "iHelp48" , "iFind48", "iInsert48", "iDelete48", "iPrint48", "iExit48" }
   aImg1  := ARRAY(LEN(aImg))
   FOR i := 1 TO LEN(aImg)
      hIco  := LoadIconByName( aImg[i], nHImg, nHImg )
      hBmp  := BmpFromIcon( hIco )          // вернет хендл bmp
      cFile := cPath + aImg[i] + "-" + HB_NtoS(nHImg) + ".bmp"
      HMG_SaveImage( hBmp, cFile, "bmp" )
      aImg1[i] := cFile
      DeleteObject( hBmp )
      DestroyIcon(hIco)
      DO EVENTS
   NEXT

   aCap   := { "Help"  , "Documents" , "Add recno" , "Delete recno" , "Print"  , "Exit"  }
   aObj   := { "_Help" , "_Find"     , "_RecIns"   , "_RecDel"      , "_Print" , "_Exit" }
   aTip   := { "Help" , ;
                "F7 - Documents: search, work with records" , ;
                "Ins - add recno", ;
                "Del - delete recno", ;
                "F5 - Print", ;
                "Exit the program" }
   // расчЄт по тексту
   nWtxt  := nW := nH := 0
   FOR i := 1 TO LEN(aCap)
      cCap := aCap[ i ]
      //nWCap := GetTxtWidth(cMenu, nFSize, cFont, lBold )
      nWCap := GetTextWidth( NIL, cCap, hFont )
      nWTxt := MAX(nWTxt,nWCap)
   NEXT
   nWTxt := IIF(nWTxt < nHImg, nHImg, nWTxt )   // nHImg-высота bmp
   nW    := nWTxt + 15                          // ширина кнопки
   nH    := nHImg + 5 + nFSize + 5              // высота кнопки  
   cMenu := "Menu "+HB_NtoS(nHImg)+"x"+HB_NtoS(nHImg)+":"

   DEFINE SPLITBOX HANDLE hSpl

      DEFINE TOOLBAREX ToolBar_1 CAPTION cMenu BUTTONSIZE nW, nH FLAT ;
         FONT cFont SIZE nFSize BOLD 
         FOR i := 1 TO LEN(aCap) - 1

             cObj := aObj[i]

             IF i == 5  //  cObj == "_Print"
                // кнопка с меню
                BUTTON &cObj CAPTION aCap[i] PICTURE aImg1[i] TOOLTIP aTip[i]   ;
                ACTION _wPost(This.Name, , {"_Print", This.Name, 0} ) SEPARATOR DROPDOWN AUTOSIZE
                DEFINE DROPDOWN MENU BUTTON _Print
                   ITEM 'Print form :' ACTION Nil  DISABLED  DEFAULT  ICON 'iPrint48'
                   SEPARATOR
                   ITEM 'Form - 1' NAME PRN_001 ACTION _wPost("_Print", , {"_Print",This.Name,1}) ICON 'iPrint48'
                   ITEM 'Form - 2' NAME PRN_002 ACTION _wPost("_Print", , {"_Print",This.Name,2}) ICON 'iPrint48'
                END MENU
             ELSE
                // обычна€ кнопка
                BUTTON &cObj CAPTION aCap[i] PICTURE aImg1[i] TOOLTIP aTip[i]   ;
                ACTION _wPost(This.Name, ,This.Name) SEPARATOR //AUTOSIZE
             ENDIF

         NEXT
      END TOOLBAR

      DEFINE TOOLBAREX ToolBar_2 CAPTION "" BUTTONSIZE nW, nH FLAT ;
         FONT cFont SIZE nFSize BOLD 
             i    := LEN(aCap)
             cObj := aObj[i]
             BUTTON &cObj CAPTION aCap[i] PICTURE aImg1[i] TOOLTIP aTip[i]   ;
             ACTION _wPost(This.Name, ,This.Name) AUTOSIZE
      END TOOLBAR

      nH := This.ToolBar_1.Height 

   END SPLITBOX

   owc:hSpl   := hSpl                   // хендл объекта SPLITBOX
   owc:nHTBar := GetWindowHeight(hSpl)  // дл€ корректировки высота ToolBar

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
   oIni:Write()  // Ќ≈ UTF8, т.е. нет BOM на выходе

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

