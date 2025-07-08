/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2021 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2021 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * О программе / About the program
*/

#define _HMG_OUTLOG
#include "minigui.ch"
#include "i_winuser.ch"

#xtranslate MiniGuiVersionChar()  => MG_Version( .F. )
#xtranslate MiniGuiVersionNumba() => MG_Version( .T. )
///////////////////////////////////////////////////////////////////////////////////////
FUNCTION MsgAbout(hIcon,nIcoSize)
   LOCAL cMsg, bOnInit, aBtnClr //, aBack_Alert
   LOCAL c := _HMG_MouseCol
   LOCAL r := _HMG_MouseRow
   DEFAULT hIcon := nIcoSize := 0

   //SET MSGALERT BACKCOLOR TO { 159, 191, 236 } STOREIN aBack_Alert
   //SET MSGALERT FONTCOLOR TO BLUE

   cMsg := REPL(".",70) + ";"
   cMsg += App.Cargo:cTitle   + ";;"
   cMsg += App.Cargo:cVersion + ";;"
   cMsg += "SQLITE Version library: " + SQLiteVersion() + ";;"
   cMsg += App.Cargo:cAvtor    + ";" + App.Cargo:cEmail + ";;"
   cMsg += App.Cargo:cPrgInfo1 + ";"
   cMsg += App.Cargo:cPrgInfo2 + ";"
   cMsg += App.Cargo:cSiteDownload + ";;"
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
                  Local y, x, h, w, y1, x1, aObj, a
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
                  // добавить картинку на окно
                  // HmgClipper      PNG             res\Hmg_clipper.png
                  h := LoadImage("HmgClipper")
                  a := GetBitmapSize( h )
                  DeleteObject( h )
                  ? ProcNL(), "LoadImage:", a[1], a[2]
                  // вписать картинку на окно
                  aObj := HMG_GetFormControls(ow:Name) // все объекты
                  ? ProcNL(), ow:Name, "aObj=", aObj ; ?v aObj
                  y := This.Say_01.Row
                  x := This.Say_01.Col
                  w := This.Say_01.Width
                  h := This.Say_01.Height
                  ? y, x, w, h
                  @ y, x IMAGE Img_1 OF &(ow:Name) PICTURE "HmgClipper" ;
                    WIDTH w HEIGHT a[2] STRETCH WHITEBACKGROUND TRANSPARENT

                  This.Say_01.Row    := y + a[2] + 2
                  This.Say_01.Height := h - a[2] + 2

                  //SetCursorPos( x + x1, y + y1 )
                  //HMG_SetMousePos( ow:Handle, y, x )
                  //_PushKey( VK_SPACE )
                  //This.Btn_01.SetFocus
                  //This.Say_01.SetFocus
                  // или так
                  h := This.Btn_01.Handle
                  y := GetWindowHeight(h) * 0.5
                  x := GetWindowWidth (h) * 0.5
                  HMG_SetMousePos( h, y, x )
                  Return Nil
               }

   // ------------ alerts.prg ---------
   //AlertInfo( Message, Title, Icon, nSize, aColors, lTopMost, bInit, lNoSound )
   IF !IsNumeric(hIcon)
      hIcon := 0
   ENDIF

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

   //SET MSGALERT BACKCOLOR TO aBack_Alert[1]
   //SET MSGALERT FONTCOLOR TO aBack_Alert[2]

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
