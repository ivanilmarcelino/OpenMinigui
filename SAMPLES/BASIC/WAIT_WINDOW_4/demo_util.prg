/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
*/

#define _HMG_OUTLOG
#include "minigui.ch"
#include "i_winuser.ch"

#xtranslate MiniGuiVersionChar()  => MG_Version( .F. )
#xtranslate MiniGuiVersionNumba() => MG_Version( .T. )

*----------------------------------------------------------------------------*
FUNCTION MG_Version( lNum, nLen )
*----------------------------------------------------------------------------*
   LOCAL cVer, nVer, cTmp

   IF lNum == NIL ; RETURN MiniGuiVersion()
   ENDIF

   Default nLen := 6

   FOR EACH cTmp IN hb_ATokens( MiniGuiVersion(), " " )
       IF Val( cTmp ) > 0
          cVer := cTmp
          nVer := StrTran( cVer, ".", "" ) + Replicate("0", nLen)
          nVer := Val( Left( nVer, nLen ) )
          EXIT
       ENDIF
   NEXT

RETURN iif( Empty(lNum), cVer, nVer )

*----------------------------------------------------------------------------*
FUNCTION ProcNL(nVal, cMsg)
*----------------------------------------------------------------------------*
   Default cMsg := ">>> "//"Call from: "

   nVal := iif( Empty(nVal), 0, nVal ) + 1
   cMsg += ProcName(nVal) + "(" + hb_ntos( ProcLine(nVal) ) + ")"
   cMsg += " => " + ProcFile(nVal)

RETURN cMsg

*----------------------------------------------------------------------------*
FUNCTION SetsEnv(nMode, cFont, nSize, cFDlg, nFDlg, cLog, cIconDef, aBClrDlg)
*----------------------------------------------------------------------------*
   LOCAL o
   Default nMode := 0

   IF nMode == 0                  // базовая настройка
      Default cFont    := "Arial", nSize := 12
      Default cFDlg    := "DejaVu Sans Mono", nFDlg := nSize + 2
      Default cLog     := "_msg.log"
      Default cIconDef := "1MG"
      Default aBClrDlg := { 141, 179, 226 }

      SET DATE     TO GERMAN
      SET DECIMALS TO 4
      SET EPOCH    TO 2000
      SET EXACT    ON
      SET SOFTSEEK ON
      SET CENTURY  ON
      SET AUTOPEN  OFF
      SET DELETED  OFF

      SET OOP ON

      SET FONT TO cFont, nSize
      // Alert
      DEFINE FONT DlgFont FONTNAME cFDlg SIZE nFDlg
      // TBrowse
      DEFINE FONT Normal  FONTNAME cFont           SIZE nSize
      DEFINE FONT Bold    FONTNAME cFont           SIZE nSize   BOLD
      DEFINE FONT Italic  FONTNAME cFont           SIZE nSize-2 ITALIC
      // other
      DEFINE FONT Ital    FONTNAME cFont           SIZE nSize   ITALIC
      DEFINE FONT ComSnMs FONTNAME "Comic Sans MS" SIZE nSize

      SET DEFAULT  ICON       TO cIconDef
      SET MSGALERT BACKCOLOR  TO aBClrDlg
      SET WINDOW MODAL PARENT HANDLE ON
      SET NAVIGATION EXTENDED
      SET MENUSTYLE  EXTENDED              // switch menu style to advanced
      SetMenuBitmapHeight( 32 )            // set menu icons size to 32x32

      App.Cargo := oHmgData() ; o := App.Cargo

      o:cIconDef      := cIconDef
      o:cDefFontName  := cFont
      o:nDefFontSize  := nSize
      o:cDlgFontName  := cFDlg
      o:nDlgFontSize  := nFDlg
      o:cFDlg         := cFDlg
      o:nFDlg         := nFDlg
      o:cLog          := cLog
      o:cFormGotFocus := ""

      _SetGetLogFile( cLog )

   ELSEIF nMode == 1              // доп. настройка 1
      o := App.Cargo

   ELSEIF nMode == 2              // доп. настройка 2
      o := App.Cargo

   ENDIF

RETURN o

////////////////////////////////////////////////////////////////////////
FUNCTION oGetForms(lChar)
   LOCAL aForms := {}, cRet := "", ow, aw, lVsbl, nI, lDel
   Default lChar := .T.

   FOR EACH ow IN HMG_GetForms( , .T. )
      nI    := hb_enumIndex()
      lVsbl := IsWindowVisible( GetFormHandle( ow:Name ) )
      lDel  := _HMG_aFormDeleted[nI]
      AAdd(aForms, {nI, ow:Type, ow:Name, lVsbl, lDel, ow:Handle, ow:Title} )
   NEXT
   IF lChar
      FOR EACH aw IN aForms
          cRet += hb_valtoexp(aw) + CRLF
      NEXT
      RETURN cRet
   ENDIF

RETURN aForms

/////////////////////////////////////////////////////////////////////////////
FUNCTION PromptGetForms()
   LOCAL nH, nW, nG, nWBtn, nHBtn, cIco, cTitle, nY, nX, nK, nL, cTxt, nI
   LOCAL cFont, nFSize, cBFont, nBFSize, aBackColor, aBtn, cForm, cObj
   LOCAL aFntClr, aBtnBClr, aList, o

   ? "-->>", ProcNL(), ThisWindow.Name

   cIco       := "2MG_64"
   cTitle     := 'Список окон этой программы / '
   cTitle     += 'List of windows of this program'
   cFont      := App.Cargo:cFDlg
   nFSize     := App.Cargo:nFDlg
   cBFont     := App.Cargo:cFDlg                // шрифт для кнопок
   nBFSize    := App.Cargo:nFDlg + 2
   aBackColor := GREY                           // цвет фона всей формы
   aFntClr    := { WHITE, YELLOW      }         // цвет фонта кнопок
   aBtnBClr   := { {94,59,185}, BLACK }         // цвет фона кнопок
   nW         := System.DesktopWidth
   nH         := System.DesktopHeight - GetTaskBarHeight()   // высота Панели задач Desktop
   nG         := 20                             // отступ
   cForm      := "Form_ListWin"
   aBtn       := oGetForms(.F.)

   nWBtn := nHBtn := nL := 0
   nK    := Len(aBtn)
   aList := {}
   FOR nI := 1 TO nK
      nWBtn := MAX(LEN(aBtn[nI,7]), nWBtn)
      nHBtn := MAX(LEN(aBtn[nI,3]), nHBtn)
   NEXT
   nWBtn := IIF( nWBtn > 70, 70, nWBtn)
   // можно выкинуть лишние строки из aBtn с lDel == .T. и lVsbl := .F., например
   FOR nI := 1 TO nK
      //AAdd(aForms, {nI, ow:Type, ow:Name, lVsbl, lDel, ow:Handle, ow:Title} )
      cTxt := STR(nI,2) + ") " + PADR(aBtn[nI,3], nHBtn + 1)
      cTxt += ", " + PADR(aBtn[nI,7], nWBtn + 1)
      cTxt += ", [" + aBtn[nI,2] + "] "
      cTxt += CVALTOCHAR(aBtn[nI,4]) + " "
      cTxt += CVALTOCHAR(aBtn[nI,5]) + " "
      cTxt += HB_NtoS(aBtn[nI,6]) + " "
      nL   := MAX(Len(cTxt),nL)
      AADD( aList, cTxt )
      ? nI, nL, Len(cTxt)
   NEXT
   // добавим кнопку
   AADD( aList, PADC("EXIT",nL) )
   AADD(aBtn, {Len(aBtn) + 1, "", "", .F., .F., 0, ""})

   cTxt  := REPL("a",nL)
   nWBtn := GetTxtWidth( cTxt, nBFSize+2, cBFont, .F. ) + nG  // получить Width текста
   nW    := nWBtn + nG*2 + 10 + GetBorderWidth()*2            // ширина окна

   DEFINE WINDOW &cForm At nY, nX WIDTH nW HEIGHT nH                   ;
      TITLE cTitle ICON cIco                                           ;
      MODAL NOSIZE                                                     ;
      BACKCOLOR aBackColor                                             ;
      FONT cFont SIZE nFSize                                           ;
      ON GOTFOCUS {|| App.Cargo:cFormGotFocus := This.Name }           ;
      ON INIT     {|| This.Topmost := .F., DoEvents(), _wPost(0) }     ;
      ON RELEASE  {|| AEval({91,92,93}, {|n| _wSend(n), DoEvents()}) } ; // executed before destroying the window

      This.Cargo := oHmgData()
      nW    := This.ClientWidth
      nH    := This.ClientHeight
      nY    := nX := nG
      nHBtn := 40
      nWBtn := nW - nG * 2

      @ 0,0 Label Buff Value "" AUTOSIZE

      nK := Len(aList)
      FOR nI := 1 TO nK

         cObj := "Btn_" + StrZero(nI, 2)
         cTxt := aList[ nI ]

         @ nY, nX BUTTONEX &cObj WIDTH nWBtn HEIGHT nHBtn CAPTION cTxt  ;
           ICON NIL FLAT NOXPSTYLE HANDCURSOR NOTABSTOP                 ;
           FONTCOLOR aFntClr[1] BACKCOLOR aBtnBClr[1]                   ;
           FONT cBFont SIZE nBFSize                                     ;
           ACTION  {|| This.Enabled := .F., _wPost(This.Cargo:nPost, This.Index) }       ;
           ON MOUSEHOVER ( This.Fontcolor := This.Cargo:aFClr[2], This.FontSize := This.Cargo:nFSize+2, This.Backcolor := This.Cargo:aBClr[2] ) ;
           ON MOUSELEAVE ( This.Fontcolor := This.Cargo:aFClr[1], This.FontSize := This.Cargo:nFSize  , This.Backcolor := This.Cargo:aBClr[1] ) ;
           ON INIT {||
                    This.Cargo := oHmgData()
                    This.Cargo:cObj := This.Name
                    This.Cargo:oCtl := This.Object
                    Return Nil
                    }

         o := This.&(cObj).Cargo  // берем адрес объекта для быстроты доступа
         o:nBtn   := nI
         o:aBtn   := aBtn[ nI ]
         o:nPost  := 1
         o:aBClr  := aBtnBClr
         o:aFClr  := aFntClr
         o:nFSize := nBFSize
         o:cFont := (App.Cargo):cBtnFontName
         o:oWnd  := This.Object

         IF nI == nK
            o:aBClr := { MAROON , BLACK }
            This.&(cObj).Caption   := "Exit from this window"
            This.&(cObj).Backcolor := This.&(cObj).Cargo:aBClr[1]
            This.&(cObj).Action    := {|| _wPost(99) }
            This.&(cObj).FontName  := cBFont
            This.&(cObj).FontBold  := .T.
            This.&(cObj).FontSize  := nBFSize
         ENDIF
         nY += nHBtn + nG/2
      NEXT

      ThisWindow.Height  := nY + nG + nG/2 + GetBorderHeight()   // установим внешнюю высоту окна

      WITH OBJECT This.Object
         :Event( 0, {|    |
                     This.Topmost := .F.
                     Return Nil
                     })
         :Event( 1, {|obtn|
                     Local ow := obtn:Window
                     Local a  := obtn:Cargo:aBtn
                     Local cForm := a[3]
                     Local lVsbl := a[4]
                     Local lDel  := a[5]
                     //  1     2        3       4      5       6         7
                     // {nI, ow:Type, ow:Name, lVsbl, lDel, ow:Handle, ow:Title}
                     cForm := a[3]
                     IF _IsWindowDefined(cForm) .and. lVsbl .and. !lDel
                        DoMethod(cForm, "Minimize") ; DO EVENTS
                        DoMethod(cForm, "Restore" ) ; DO EVENTS
                        IF _IsControlDefined("Buff", cForm)
                           DoMethod(cForm, "Buff", "SetFocus" )
                        ENDIF
                        DO EVENTS
                     ENDIF
                     ow:Enabler(obtn:Name, .T.)
                     _wPost(99)
                     Return Nil
                     })
         :Event(99, {|ow  | ow:Release })
      END WITH

   END WINDOW

   CENTER WINDOW &cForm
   ACTIVATE WINDOW &cForm

RETURN NIL

///////////////////////////////////////////////////////////////////////////////////////
FUNCTION MsgAbout(hIcon,nIcoSize)
   LOCAL cMsg, bOnInit, aBtnClr, aBack_Alert
   LOCAL c := _HMG_MouseCol
   LOCAL r := _HMG_MouseRow
   DEFAULT hIcon := nIcoSize := 0

   SET MSGALERT BACKCOLOR TO { 159, 191, 236 } STOREIN aBack_Alert
   SET MSGALERT FONTCOLOR TO BLUE

   cMsg := REPL(".",70) + ";"
   cMsg += App.Cargo:cTitleRu + ";" + App.Cargo:cTitle + ";"
   cMsg += App.Cargo:cVersion + ";;"
   cMsg += App.Cargo:cAvtor    + ";" + App.Cargo:cEmail + ";;"
   cMsg += App.Cargo:cPrgInfo1 + ";"
   cMsg += App.Cargo:cPrgInfo2 + ";;"
   cMsg += "Special BIG THANK YOU - Sergej Kiselev ! <bilance@bilance.lv>;;"
   cMsg += "Operating System: " + Os() + ";"
   cMsg += "Developed in : " +  MiniGUIVersion() + ";"
   cMsg += "xBase Compiler: " + Version() + ";"
   cMsg += "C Compiler: " + Hb_Ccompiler() + ";;"
   cMsg += PadC( "This program is Freeware!", 70 ) + ";"
   cMsg += PadC( "Copying is allowed!", 70 )  + ";"
   cMsg += REPL(".",70) + ";"
   cMsg += REPL( ";", 6 )

   aBtnClr := { RED }
   bOnInit := {|| // свои параметры окна
                  Local ow := ThisWindow.Object
                  Local y, x, h, y1, x1
                  h  := This.Btn_01.Handle
                  y  := This.Btn_01.Row + 60
                  x  := This.Btn_01.Col + 30
                  y1 := GetWindowRow(ow:Handle)
                  x1 := GetWindowCol(ow:Handle)
                  //? ProcNL(), This.Name, ow:Name, "Btn_01.Handle: h, y, x:", h, y, x
                  //? ow:Name, ow:Handle, "Row win:", y1, "Col win:", x1
                  //This.Btn_01.Action      := {|| DoEvents(), _wPost(0, This.Index) }
                  This.Btn_01.OnGotFocus  := {|| DrawRR( RED ) }
                  This.Btn_01.OnLostFocus := {|| DrawRR( .F. ) }
                  This.Btn_01.Fontcolor   := YELLOW
                  This.Btn_01.SetFocus
                  DoEvents()
                  //SetCursorPos( x + x1, y + y1 )
                  //HMG_SetMousePos( ow:Handle, y, x )
                  //_PushKey( VK_SPACE )
                  //This.Btn_01.SetFocus
                  //This.Say_01.SetFocus
                  // или так
                  y := GetWindowHeight(h) * 0.5
                  x := GetWindowWidth (h) * 0.5
                  HMG_SetMousePos( h, y, x )
                  Return Nil
               }

   // ------------ alerts.prg ---------
   //AlertInfo( Message, Title, Icon, nSize, aColors, lTopMost, bInit, lNoSound )

   IF MiniGuiVersionNumba() < 231201
      hIcon := 0
   ENDIF

   IF hIcon == 0
      AlertInfo( cMsg, "About", "2MG_64", 64, aBtnClr, .T. , bOnInit, .F. )
   ELSE
      // Это только с версии 23.12.2 и выше
      AlertInfo( cMsg, "About", hIcon, nIcoSize, aBtnClr, .T. , bOnInit, .F. )
   ENDIF

   SetCursorPos( c, r )  // вернём мышку на место

   SET MSGALERT BACKCOLOR TO aBack_Alert[1]
   SET MSGALERT FONTCOLOR TO aBack_Alert[2]

RETURN NIL

////////////////////////////////////////////////////////////////////
FUNCTION DrawRR( focus, nPen, t, l, b, r, cWindowName, nCurve )
   LOCAL aColor

   DEFAULT t := This.Row, l := This.Col, b := This.Height, r := This.Width
   DEFAULT focus := .F., cWindowName := ThisWindow.Name, nCurve := 7
   DEFAULT nPen  := 3

   IF ISARRAY( focus ) ; aColor := focus
   ELSE                ; aColor := iif( focus, { 0, 120, 215 }, { 100, 100, 100 } )
   ENDIF

   DRAW ROUNDRECTANGLE IN WINDOW (cWindowName)  ;
        AT t - 2, l - 2 TO t + b + 2, l + r + 2 ;
        ROUNDWIDTH  nCurve ROUNDHEIGHT nCurve   ;
        PENCOLOR    aColor PENWIDTH    nPen

RETURN NIL

////////////////////////////////////////////////////////////////////
FUNCTION HMG_SetMousePos( nHandle, y1, x1 )
   LOCAL c := _HMG_MouseCol
   LOCAL r := _HMG_MouseRow
   Local y := GetWindowRow(nHandle)
   Local x := GetWindowCol(nHandle)
   Default y1 := 1, x1 := 1

   SetCursorPos( x + x1, y + y1 )

RETURN {c,r}

///////////////////////////////////////////////////////////////////////////////
// получить Width текста
FUNCTION GetTxtWidth( cText, nFontSize, cFontName, lBold )
   LOCAL hFont, nWidth
   DEFAULT cText     := REPL('A', 2)        ,  ;
           cFontName := _HMG_DefaultFontName,  ;   // из MiniGUI.Init()
           nFontSize := _HMG_DefaultFontSize,  ;   // из MiniGUI.Init()
           lBold     := .F.

   IF Valtype(cText) == 'N'
      cText := repl('A', cText)
   ENDIF

   hFont  := InitFont(cFontName, nFontSize, lBold)
   nWidth := GetTextWidth(0, cText, hFont)         // ширина текста
   DeleteObject (hFont)

RETURN nWidth

//////////////////////////////////////////////////////////////////////////////////
// Переключение объекта на несколько линий
FUNCTION SetStyleML( hControl )
   ChangeStyle( hControl, BS_MULTILINE )
   RETURN .t.

/////////////////////////////////////////////////
FUNCTION SwitchToWin( cForm )

   IF _IsWindowDefined( cForm )
      IF IsIconic( GetFormHandle( cForm ) )
         _Restore( GetFormHandle( cForm ) )
      ELSE
         DoMethod( cForm, "SetFocus" )
      ENDIF
   ENDIF

RETURN NIL

