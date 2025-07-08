/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2019-2024 Grigory Filatov <gfilatov@gmail.com>
 * Copyright 2024 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
*/
#include "minigui.ch"
#include "i_winuser.ch"
/////////////////////////////////////////////////////////////////////////////////
FUNCTION AlertDebug()
   LOCAL cFont, nFontSize, nI, nY, nX, nW, nH, nG, nL, nHMemo, cTitle, cForm
   LOCAL aBtn, nBtnH, nBtnW, aBtnBClr, cMsg, aParams, xVal, aMsg, cN, aDim
   LOCAL aFontColor, aBackColor, lWin, cDbgIni, lDbgIni, oDbgIni, nWTxt
   LOCAL hWndIsx, cFrmIsx

   aBackColor := { 230, 186, 186 }
   aFontColor := BLACK
   aBtnBClr   := { {183,109,225}, {217,67,67} }
   aDim       := GetFontParam("DlgFont")
   cFont      := "DejaVu Sans Mono"
   nFontSize  := 14
   nFontSize  := IIF( LEN(aDim) > 0, aDim[2], nFontSize )
   cForm      := "MG_Form_Debug_" + HB_NtoS( _GetId() )
   aMsg       := { ProcNL(1) , ProcNL(2) , ProcNL(3) }
   cMsg       := MiniGUIVersion() + CRLF
   cDbgIni    := GetUserTempFolder() + "\MG_Form_Debug.ini"
   aParams    := hb_aParams()

   // запомнить предыдущее окно, если оно есть
   hWndIsx := 0
   cFrmIsx := _HMG_ThisFormName
   IF !Empty(cFrmIsx) .and. _IsWindowDefined(cFrmIsx)
      hWndIsx := GetFormHandle( cFrmIsx )
      SET WINDOW THIS TO cFrmIsx
   ENDIF

   FOR nI := 1 TO LEN(aParams)
      cMsg += HB_NtoS(nI) + ") "
      xVal := aParams[nI]
      cMsg += HMG_Val2Txt( xVal ) + CRLF
   NEXT
   cMsg += CRLF + REPL("~.",40) + CRLF

   IF Hb_LangSelect() == "ru.RU1251"
      aBtn   := { '&Окна программы', '&Выход' }
      cTitle := "Отладка"
   ELSE
      aBtn   := { '&Program windows', '&Exit' }
      cTitle := "Debugging"
   ENDIF

   IF Empty( _HMG_MainHandle )  // если нет MAIN окна
      MsgDebug("NO MAIN window !"+ProcNL()+";"+ProcNL(1))
      SET WINDOW MAIN OFF
   ENDIF

   lWin := .T.
   nY   := nX := 0
   IF HB_ISOBJECT(App.Cargo)
      IF IsNumeric(App.Cargo:WinDebug_nY)
         nY   := App.Cargo:WinDebug_nY
         lWin := .F.
      ENDIF
      IF IsNumeric(App.Cargo:WinDebug_nX)
         nX := App.Cargo:WinDebug_nX
         lWin := .F.
      ENDIF
   ENDIF

   // если есть ини-файл, то координаты берем с него
   lDbgIni := hb_FileExists(cDbgIni)
   oDbgIni := TIniData():New(cDbgIni, .T.):Read()

   Default oDbgIni:INFO := oHmgData()
   Default oDbgIni:INFO:Developed_in   := MiniGUIVersion()
   Default oDbgIni:INFO:xBase_compiler := Version()
   Default oDbgIni:INFO:C_compiler     := Hb_Compiler()

   Default oDbgIni:MAIN := oHmgData()
   Default oDbgIni:MAIN:cExe := App.Exename
   Default oDbgIni:MAIN:aWin := {0, 0, 0, 0}

   IF lDbgIni // если файла есть
      nY   := oDbgIni:MAIN:aWin[1]
      nX   := oDbgIni:MAIN:aWin[2]
      lWin := .F.
   ENDIF

   // расчёт ширины на самую широкую строку
   nWTxt := 0
   aDim  := HB_ATokens(cMsg, CRLF)
   FOR nI := 1 TO LEN(aDim)
      nL    := GetTxtWidth( aDim[nI], nFontSize, cFont, .F. )  // получить Width текста
      nWTxt := MAX(nL,nWTxt)
   NEXT

   nG    := 20
   nWTxt += nG*2 + GetScrollBarSize() + nG  // добавка к ширине строки
   nW    := IIF( nWTxt > System.DesktopWidth, System.DesktopWidth, nWTxt )
   lWin  := IIF( nWTxt > System.DesktopWidth, .T., .F. )
   nH    := System.DesktopHeight * 0.6
   nBtnH := 55  // высота кнопки

   DEFINE WINDOW &cForm AT nY, nX WIDTH nW HEIGHT nH   ;
      TITLE cTitle                                     ;
      WINDOWTYPE STANDARD NOSYSMENU NOSIZE             ;
      FONT cFont SIZE nFontSize                        ;
      BACKCOLOR aBackColor                             ;
      ON INIT     {|| _wPost( 0) }                     ;
      ON RELEASE  {|| _wSend(90) }                     ;
      ON MOUSECLICK MoveActiveWindow()                 ;
      ON MAXIMIZE {|| _wPost(10) }  // {|| This.Restore, DoEvents() } - можно и так

      This.Cargo := oHmgData()
      This.Cargo:oDbgIni := oDbgIni
      This.Cargo:lWin    := lWin
      This.Cargo:hWndIsx := hWndIsx

      nW := This.ClientWidth
      nH := This.ClientHeight
      nY := nG/2
      nX := nG

      //DRAW ICON IN WINDOW &cForm AT nY, nX PICTURE "iDebug64" WIDTH 64 HEIGHT 64 COLOR aBackColor
      DRAW ICON IN WINDOW &cForm AT nY, nX PICTURE "1MG" WIDTH 64 HEIGHT 64 COLOR aBackColor

      nY := 2
      nX += 64 + nG
      FOR nI := 1 TO Len(aMsg)
         cN := 'Lbl_' + StrZero(nI, 2)
         @ nY, nX LABEL &cN PARENT &cForm WIDTH nW - nX - nG HEIGHT nFontSize*2 ;
           VALUE aMsg[nI] FONTCOLOR MAROON TRANSPARENT VCENTERALIGN
         nY += nFontSize + 5
      NEXT
      nY += nG + 2

      nHMemo  := nH - nY - ( nBtnH + nG * 2 )
      @ nY, nG EDITBOX Edit_Memo PARENT &cForm WIDTH nW - nG*2 HEIGHT nHMemo ;
        VALUE cMsg READONLY NOHSCROLL SIZE nFontSize - 2 ;
        BACKCOLOR aBackColor FONTCOLOR aFontColor

      nY    := nH - nBtnH - nG
      nBtnW := 290
      nX    := nW - ( nBtnW*2 + nG*2 )
      @ nY, nX BUTTONEX Btn_Log PARENT Form_Err           ;
        WIDTH nBtnW HEIGHT nBtnH CAPTION aBtn[1] ICON Nil ;
        NOHOTLIGHT NOXPSTYLE HANDCURSOR  SIZE nFontSize+2 ;
        FONTCOLOR WHITE  BACKCOLOR aBtnBClr[1]            ;
        ON MOUSEHOVER ( This.Backcolor := BLACK      , This.Fontcolor := YELLOW ) ;
        ON MOUSELEAVE ( This.Backcolor := aBtnBClr[1], This.Fontcolor := WHITE  ) ;
        ACTION {|| _wPost(5, ,This.Name) }
      nX += nBtnW + nG

      @ nY, nX BUTTONEX Btn_Exit PARENT Form_Err          ;
        WIDTH nBtnW HEIGHT nBtnH CAPTION aBtn[2] ICON Nil ;
        NOHOTLIGHT NOXPSTYLE HANDCURSOR SIZE nFontSize+2  ;
        FONTCOLOR WHITE  BACKCOLOR aBtnBClr[2]            ;
        ON MOUSEHOVER ( This.Backcolor := BLACK      , This.Fontcolor := YELLOW ) ;
        ON MOUSELEAVE ( This.Backcolor := aBtnBClr[2], This.Fontcolor := WHITE  ) ;
        ACTION {|| _wPost(99) }

      (This.Object):Event( 0, {|ow|
                                This.Topmost := .T.
                                ow:Cargo:nY := ow:Row
                                ow:Cargo:nX := ow:Col
                                ow:Cargo:nW := ow:Width
                                ow:Cargo:nH := ow:Height
                                ow:Setfocus("Edit_Memo")
                                DoEvents()
                                Return Nil
                                })
      (This.Object):Event( 5, {|ow,ky,cn| This.&(cn).Enabled := .F. , _SetThisFormInfo(ow),;
                                          ListGetForms(ky)          , _SetThisFormInfo(),;
                                          This.&(cn).Enabled := .T. , ow:Setfocus("Edit_Memo")  } )
      (This.Object):Event(10, {|ow| // ON MAXIMIZE - оставить размеры окна
                    Local o := ow:Cargo
                    ow:SetSize( o:nY, o:nX, o:nW, o:nH )
                    // или _SetWindowSizePos(ow:Name, o:nY, o:nX, o:nW, o:nH )
                    DoEvents()
                    Return Nil
                    })
      (This.Object):Event(90, {|ow| // ON RELEASE - сохранить координаты
                                    Local oDbgIni := ow:Cargo:oDbgIni
                                    If HB_ISOBJECT(App.Cargo)
                                       App.Cargo:WinDebug_nY := ow:Row
                                       App.Cargo:WinDebug_nX := ow:Col
                                    Endif
                                    // сохранить размеры окна
                                    IF !ow:Cargo:lWin
                                       oDbgIni:MAIN:cExe      := App.Exename
                                       oDbgIni:MAIN:aWin      := {ow:Row, ow:Col, ow:Width, ow:Height}
                                       oDbgIni:cCommentBegin  := " Modify: " + hb_TtoC( hb_DateTime() )
                                       oDbgIni:Write()  // НЕ UTF8, т.е. нет BOM на выходе
                                    ENDIF
                                    // вернуться на предыдущее окно
                                    If ow:Cargo:hWndIsx > 0
                                       SwitchToThisWindow(hWndIsx)
                                       SET WINDOW THIS TO
                                    Endif
                                    Return Nil
                                    })
      (This.Object):Event(99, {|ow| ow:Release() })

      ON KEY F1     ACTION NIL
      ON KEY ESCAPE ACTION _wPost(99)

   END WINDOW

   IF lWin
      CENTER WINDOW &cForm
   ENDIF
   ACTIVATE WINDOW &cForm ON INIT {|| This.Minimize, DoEvents(), This.Restore }

RETURN NIL

///////////////////////////////////////////////////////////////
#define HTCAPTION          2
//#define WM_NCLBUTTONDOWN   161
STATIC Procedure MoveActiveWindow( hWnd )
    DEFAULT hWnd := GetActiveWindow()
    PostMessage( hWnd, WM_NCLBUTTONDOWN, HTCAPTION, 0 )
    RC_CURSOR( "Grabbed32" )
Return

/////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION ListGetForms
   LOCAL nI, cForm, aFrm, cMsg

   aFrm := HMG_GetForms()
   cMsg := "Number of open windows: " + HB_NtoS(LEN(aFrm)) + ";;"

   FOR nI := 1 TO LEN(aFrm)
      cForm := UPPER(aFrm[nI])
      cMsg += HB_NtoS(nI) + ") "
      cMsg += ' Form: ' + cForm + ', Type: "'+_HMG_aFormType[nI]+'" '
      cMsg += ', Handle: '+HB_NtoS(_HMG_aFormHandles[nI])
      cMsg += ', Deleted: ' + cValToChar( _HMG_aFormDeleted[nI] )
      cMsg += ', Visible: ' + cValToChar( IsWindowVisible( GetFormHandle( cForm ) ) )
      cMsg += ', Title: ' + GetProperty( cForm, "Title" ) + CRLF
   NEXT
   cMsg += REPL(";",20)
   AlertInfo( cMsg, "List of windows in the program", "1MG", 64, {RED} )

RETURN NIL

/////////////////////////////////////////////////////////////////
FUNCTION HMG_Val2Txt( xVal )
LOCAL cTx, cTp:= ValType( xVal )

   IF     cTp=='C' ; cTx := AllTrim(xVal) ; cTx := iif(Empty(cTx), "'"+"'", cTx)
   ELSEIF cTp=='N' ; cTx := LTrim(Str(xVal))
   ELSEIF cTp=='L' ; cTx := iif(xVal, ".T.", ".F.")
   ELSEIF cTp=='D' ; cTx := DToS( xVal ) ;  cTx := Right(cTx,2)+'.'+subs(cTx,5,2)+'.'+Left(cTx,4)
   //ELSEIF cTp=='A'; cTx := "ARRAY["  + hb_NToS( Len(xVal) ) + "] = " + HB_ValToExp(xVal)
   ELSEIF cTp=='A' ; cTx := HMG_Arr2Txt(xVal)
   ELSEIF cTp=='U' ; cTx := 'NIL'
   ELSEIF cTp=='B' ; cTx := "'" + "B" + "'"
   ELSEIF cTp=='O' ; cTx := HMG_Obj2Txt(xVal)
   ENDIF

RETURN "[" + cTp + "] " + cTx //+ "'"

//////////////////////////////////////////////////////////////////////////////////
FUNCTION HMG_Obj2Txt(ob)
   LOCAL cStr, aName

   IF !hb_IsObject(ob)
      RETURN " This is not an Object !"
   ENDIF

   cStr := "'OBJECT' " + ob:ClassName + " "
   IF ob:ClassName $ "THMGDATA,TKEYDATA,TTHRDATA"                // контейнер-container
      cStr += " (container) ARRAY 2x["  + hb_NToS( Len( ob:GetAll() ) ) + "] " //+ CRLF
      cStr += _o2log(ob, 20, " => ", .T., .T.)
   ELSEIF ob:ClassName == "TSBROWSE"                             // таблица-table
      cStr += ob:cParentWnd + " " + ob:cControlName + " "
      cStr += ob:cAlias + " " + HB_NtoS( ob:nLen )
      aName := __objGetMethodList( ob )
   ELSEIF ob:ClassName $ "TCNLDATA,TGETDATA,TSTBDATA,TTSBDATA"  // контрол-control
      cStr += ob:Name + " (control)" + ob:Type + " " + ob:Index
      cStr += " " + HB_NtoS( ob:Handle )
   ELSEIF ob:ClassName $ "TINIDATA"                             //  ini file
      cStr += ob:cIni + " (ini-file)  lIni=" + cValToChar(ob:lIni)
      cStr += " lUtf8=" + cValToChar( ob:lUtf8 )
      cStr += _o2log(ob, 20, " => ", .T., .T.)
   ELSEIF ob:ClassName $ "TWNDDATA"                             //  окно формы - form window
      cStr += ob:Name + " (form window) "
      cStr += "ARRAY["  + hb_NToS( Len( ob:GETLISTTYPE() ) ) + "] "
      cStr += HB_ValToExp( ob:GETLISTTYPE() )
      cStr += " " + HB_NtoS( ob:CLIENTWIDTH ) + " " + HB_NtoS( ob:CLIENTHEIGHT)
   ELSE
      aName := __objGetMethodList( ob )
      cStr += HB_ValToExp(aName)
   ENDIF

RETURN cStr

//////////////////////////////////////////////////////////////////////////////////
FUNCTION HMG_Arr2Txt(a)
   LOCAL i, cStr := ""

   IF !hb_IsArray(a)
      RETURN " This is not an array !"
   ENDIF

   IF Len(a) > 0 .and. hb_IsArray(a[1])
      cStr += "{ " + CRLF
      FOR i := 1 TO LEN(a)
         cStr += SPACE(11) + STR(i) + "." + SPACE(3) 
         cStr += HB_ValToExp(a[i]) + IIF( i==LEN(a), "", "," ) + CRLF
      NEXT
      cStr += SPACE(17) + " }" + CRLF
   ELSE
      cStr += HB_ValToExp(a)
   ENDIF
   cStr := "ARRAY["  + hb_NToS( Len(a) ) + "] = " + cStr

RETURN cStr

////////////////////////////////////////////////////////////////
FUNCTION ProcNL(nVal, cMsg)
   Default cMsg := ">>> "
   nVal := iif( Empty(nVal), 0, nVal ) + 1
   cMsg += ProcName(nVal) + "(" + hb_ntos( ProcLine(nVal) ) + ")"
   cMsg += " => " + ProcFile(nVal)
RETURN cMsg

