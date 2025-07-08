/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2019 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2015-2020 Verchenko Andrey <verchenkoag@gmail.com>
 * Many thanks for your help - forum http://clipper.borda.ru
 *
*/

#include "hmg.ch"
#include "i_winuser.ch"
#include "Dbinfo.ch"

//////////////////////////////////////////////////////////////////////////////////////////////////////
FUNCTION my2BUTTON(y, x, w, h, cObj, cCapt, aBtnGrad, aBtnClr, aIcon, aFntClr, aFnt, nwPost, lBlock )
   LOCAL aGrOver, aGrFill, nSizeIcon, lSizeIcon, y1, x1, lTextVertical
   DEFAULT cCapt    := "" , aFntClr := {  BLACK, YELLOW }
   DEFAULT aFnt     := { "Tahoma", 12 , .T. , .F. } , aIcon := {"Icon1x1","Icon1x1",.F.,48}
   DEFAULT aBtnGrad := {} , aBtnClr := { BLUE, YELLOW }
   DEFAULT lBlock   := .F. // не блокировать кнопки

   IF LEN(aFnt) == 4
      lTextVertical := aFnt[4]  //  VERTICAL
   ELSE
      lTextVertical := .F.
   ENDIF

   IF LEN(aIcon) < 3  ;  lSizeIcon := .F.
   ELSE               ;  lSizeIcon := aIcon[3]
   ENDIF
   IF LEN(aIcon) < 4  ;  nSizeIcon := 32
   ELSE               ;  nSizeIcon := aIcon[4]
   ENDIF

   IF LEN(aBtnGrad) > 0

      aGrOver := { { 0.5, aBtnGrad[2], aBtnGrad[1] }, { 0.5, aBtnGrad[1], aBtnGrad[2] } }
      aGrFill := { { 0.5, aBtnGrad[1], aBtnGrad[2] }, { 0.5, aBtnGrad[2], aBtnGrad[1] } }

      IF lSizeIcon
         IF lTextVertical  //  VERTICAL
            @ y, x  BUTTONEX &cObj WIDTH w HEIGHT h CAPTION cCapt ICON aIcon[1]       ;
              NOXPSTYLE HANDCURSOR NOTABSTOP                                          ;
              BACKCOLOR aGrOver GRADIENTFILL aGrFill                                  ;
              FONT aFnt[1] SIZE aFnt[2]  BOLD  VERTICAL                               ;
              ON MOUSEHOVER ( This.GradientFill := aGrFill , This.Fontcolor := aFntClr[2]  ,;
                              This.Icon := LoadIconByName(aIcon[2], nSizeIcon, nSizeIcon) ) ;
              ON MOUSELEAVE ( This.GradientOver := aGrOver , This.Fontcolor := aFntClr[1]  ,;
                              This.Icon := LoadIconByName(aIcon[1], nSizeIcon, nSizeIcon) ) ;
              ON INIT   {|| This.Cargo := nwPost }                                    ;
              ACTION _wPost(This.Cargo, , This.Name)
         ELSE
            @ y, x  BUTTONEX &cObj WIDTH w HEIGHT h CAPTION cCapt ICON aIcon[1]       ;
              NOXPSTYLE HANDCURSOR NOTABSTOP                                          ;
              BACKCOLOR aGrOver GRADIENTFILL aGrFill                                  ;
              FONT aFnt[1] SIZE aFnt[2]  BOLD                                         ;
              ON MOUSEHOVER ( This.GradientFill := aGrFill , This.Fontcolor := aFntClr[2]  ,;
                              This.Icon := LoadIconByName(aIcon[2], nSizeIcon, nSizeIcon) ) ;
              ON MOUSELEAVE ( This.GradientOver := aGrOver , This.Fontcolor := aFntClr[1]  ,;
                              This.Icon := LoadIconByName(aIcon[1], nSizeIcon, nSizeIcon) ) ;
              ON INIT   {|| This.Cargo := nwPost }                                    ;
              ACTION _wPost(This.Cargo, , This.Name)
         ENDIF
         // при первом построении изменить размер иконки
         This.&(cObj).Icon := LoadIconByName( aIcon[1], nSizeIcon, nSizeIcon )
      ELSE
         IF lTextVertical  //  VERTICAL
            @ y, x  BUTTONEX &cObj WIDTH w HEIGHT h CAPTION cCapt ICON aIcon[1]        ;
              NOXPSTYLE HANDCURSOR NOTABSTOP                                          ;
              BACKCOLOR aGrOver GRADIENTFILL aGrFill                                  ;
              FONT aFnt[1] SIZE aFnt[2]  BOLD  VERTICAL                               ;
              ON MOUSEHOVER ( This.GradientFill := aGrFill , This.Icon := aIcon[2] ,;
                              This.Fontcolor := aFntClr[2]  )                         ;
              ON MOUSELEAVE ( This.GradientOver := aGrOver , This.Icon := aIcon[1] ,;
                              This.Fontcolor := aFntClr[1]  )                         ;
              ACTION _wPost(This.Cargo, , This.Name)                                  ;
              ON INIT   {|| This.Cargo := nwPost }
         ELSE
            @ y, x  BUTTONEX &cObj WIDTH w HEIGHT h CAPTION cCapt ICON aIcon[1]        ;
              NOXPSTYLE HANDCURSOR NOTABSTOP                                          ;
              BACKCOLOR aGrOver GRADIENTFILL aGrFill                                  ;
              FONT aFnt[1] SIZE aFnt[2]  BOLD                                         ;
              ON MOUSEHOVER ( This.GradientFill := aGrFill , This.Icon := aIcon[2] ,;
                              This.Fontcolor := aFntClr[2]  )                         ;
              ON MOUSELEAVE ( This.GradientOver := aGrOver , This.Icon := aIcon[1] ,;
                              This.Fontcolor := aFntClr[1]  )                         ;
              ACTION _wPost(This.Cargo, , This.Name)                                  ;
              ON INIT   {|| This.Cargo := nwPost }
         ENDIF
      ENDIF

   ELSE

      IF lSizeIcon
         IF lTextVertical  //  VERTICAL
            @ y, x  BUTTONEX &cObj WIDTH w HEIGHT h CAPTION cCapt ICON aIcon[1]        ;
              NOXPSTYLE HANDCURSOR NOTABSTOP BACKCOLOR aBtnClr[1]                      ;
              FONT aFnt[1] SIZE aFnt[2] BOLD VERTICAL                                  ;
              ON MOUSEHOVER ( This.Backcolor := aBtnClr[2] , This.Fontcolor := aFntClr[2] ,;
                              This.Icon := LoadIconByName(aIcon[2],nSizeIcon,nSizeIcon) );
              ON MOUSELEAVE ( This.Backcolor := aBtnClr[1] , This.Fontcolor := aFntClr[1] ,;
                              This.Icon := LoadIconByName(aIcon[1],nSizeIcon,nSizeIcon) );
              ON INIT   {|| This.Cargo := nwPost }                                    ;
              ACTION _wPost(This.Cargo, , This.Name)
         ELSE
            @ y, x  BUTTONEX &cObj WIDTH w HEIGHT h CAPTION cCapt ICON aIcon[1]        ;
              NOXPSTYLE HANDCURSOR NOTABSTOP BACKCOLOR aBtnClr[1]                     ;
              FONT aFnt[1] SIZE aFnt[2] BOLD                                          ;
              ON MOUSEHOVER ( This.Backcolor := aBtnClr[2] , This.Fontcolor := aFntClr[2] ,;
                              This.Icon := LoadIconByName(aIcon[2],nSizeIcon,nSizeIcon) );
              ON MOUSELEAVE ( This.Backcolor := aBtnClr[1] , This.Fontcolor := aFntClr[1] ,;
                              This.Icon := LoadIconByName(aIcon[1],nSizeIcon,nSizeIcon) );
              ON INIT   {|| This.Cargo := nwPost }                                    ;
              ACTION _wPost(This.Cargo, , This.Name)
         ENDIF
         // при первом построении изменить размер иконки
         This.&(cObj).Icon := LoadIconByName( aIcon[1], nSizeIcon, nSizeIcon )
      ELSE
         IF lTextVertical  //  VERTICAL
            @ y, x  BUTTONEX &cObj WIDTH w HEIGHT h CAPTION cCapt ICON aIcon[1]       ;
              NOXPSTYLE HANDCURSOR NOTABSTOP  BACKCOLOR aBtnClr[1]                    ;
              FONT aFnt[1] SIZE aFnt[2] BOLD  VERTICAL                                ;
              ON MOUSEHOVER ( This.Backcolor := aBtnClr[2] , This.Icon := aIcon[2] ,;
                              This.Fontcolor := aFntClr[2]  )                         ;
              ON MOUSELEAVE ( This.Backcolor := aBtnClr[1] , This.Icon := aIcon[1] ,;
                              This.Fontcolor := aFntClr[1]  )                         ;
              ON INIT   {|| This.Cargo := nwPost }                                    ;
              ACTION _wPost(This.Cargo, , This.Name)
         ELSE
            @ y, x  BUTTONEX &cObj WIDTH w HEIGHT h CAPTION cCapt ICON aIcon[1]        ;
              NOXPSTYLE HANDCURSOR NOTABSTOP  BACKCOLOR aBtnClr[1]                    ;
              FONT aFnt[1] SIZE aFnt[2] BOLD                                          ;
              ON MOUSEHOVER ( This.Backcolor := aBtnClr[2] , This.Icon := aIcon[2] ,;
                              This.Fontcolor := aFntClr[2]  )                         ;
              ON MOUSELEAVE ( This.Backcolor := aBtnClr[1] , This.Icon := aIcon[1] ,;
                              This.Fontcolor := aFntClr[1]  )                         ;
              ON INIT   {|| This.Cargo := nwPost }                                    ;
              ACTION _wPost(This.Cargo, , This.Name)
         ENDIF
      ENDIF
   ENDIF

   IF lBlock 
     This.&(cObj).Action := {|| This.Enabled := .F., DoEvents(), _wPost(This.Cargo, , This.Name) } 
   ENDIF 

   y1 := y + This.&(cObj).Height
   x1 := x + This.&(cObj).Width

RETURN { y1, x1 }

////////////////////////////////////////////////////////////////////
FUNCTION ToRGB(aDim)
   RETURN RGB(aDim[1],aDim[2],aDim[3])

///////////////////////////////////////////////////////////////////
FUNCTION ProcNL(nVal)
   DEFAULT nVal := 0
   RETURN "Called from: " + ProcName( nVal + 1 ) + "(" + hb_ntos( ProcLine( nVal + 1 ) ) + ") --> " + ProcFile( nVal + 1 )

////////////////////////////////////////////////////////////////////////////
// Затенение на форме / Darken the form
FUNCTION Darken2Open(hWinHandle)
   LOCAL aBClr := { BLACK , RED , { 61, 61, 61 }, YELLOW }  // Back Color array
   LOCAL aColor, nTransparencyLevel := 128                  // Transparency level

   aColor := aBClr[1]
   // Затенение на форме / Darken the form
   OverlayCreate(hWinHandle, aColor[1], aColor[2], aColor[3], nTransparencyLevel)

   DO EVENTS

RETURN NIL

/////////////////////////////7///////////////////////////////////////////////
// Затенение на форме / Darken the form
FUNCTION Darken2Close(hWinHandle)
   OverlayClose(hWinHandle)
   Do Events
   // перересовка объектов на форме
   SendMessage( hWinHandle, WM_PAINT, 0, 0 )
   Do Events
RETURN NIL

//////////////////////////////////////////////////////////////////////////
FUNCTION MG_Show(cMsg, cTitle, aBackColor, cBtnClr, cIcoRes, nIcoSize )
   LOCAL aClrs, nLine, nOldLine
   DEFAULT cIcoRes := "iSmile64", nIcoSize := 64
   DEFAULT cTitle  := "Attention", aBackColor :=  { 198, 217, 240 }
   DEFAULT cBtnClr := { 84,141,212 }

   nLine := NUMAT(";",cMsg)
   nLine += NUMAT(CRLF,cMsg)
   nLine += 2
   cMsg  += ";;;"

   nOldLine := HMG_Alert_MaxLines(nLine)
   aClrs  := _SetMsgAlertColors( aBackColor )
   SET MSGALERT FONTCOLOR TO BLACK

   AlertInfo( cMsg, cTitle, cIcoRes, nIcoSize, { cBtnClr } , , )

   HMG_Alert_MaxLines(nOldLine)

   SET MSGALERT BACKCOLOR TO aClrs

   RETURN NIL

///////////////////////////////////////////////////////////////////////////
FUNCTION MG_Info( cMsg, cTitle, cIcon, nSize, aColors )
   LOCAL aClrs, hParentWin := ThisWindow.Handle
   DEFAULT cTitle := "Alert", cIcon := "iAlert64"
   DEFAULT nSize := 64, aColors := {189,30,73}

   Darken2Open(hParentWin)           // Затенение на форме

   aClrs  := _SetMsgAlertColors()

   SET MSGALERT FONTCOLOR TO BLACK
   SET MSGALERT BACKCOLOR TO { 141, 179, 226 }

   AlertInfo( cMsg, cTitle, cIcon, nSize, {aColors} )

   SET MSGALERT BACKCOLOR TO aClrs

   Darken2Close(hParentWin)            // Затенение на форме

RETURN NIL

///////////////////////////////////////////////////////////////////////////
FUNCTION MG_Stop( cMsg, cTitle, cIcon, nSize, aColors )
   LOCAL aClrs, hParentWin := ThisWindow.Handle
   DEFAULT cTitle := "Stop!", cIcon := "iStop64"
   DEFAULT nSize := 64, aColors := {189,30,73}

   aClrs  := _SetMsgAlertColors()

   Darken2Open(hParentWin)           // Затенение на форме

   SET MSGALERT FONTCOLOR TO BLACK
   SET MSGALERT BACKCOLOR TO { 255, 178, 178 }

   AlertStop( cMsg, cTitle, cIcon, nSize, {aColors} )

   SET MSGALERT BACKCOLOR TO aClrs

   Darken2Close(hParentWin)            // Затенение на форме

RETURN NIL

//////////////////////////////////////////////////////////////////////////
FUNCTION MG_Exclam(cMsg, cTitle, cIcon, nSize, aColors)
   LOCAL aClrs, hParentWin := ThisWindow.Handle
   DEFAULT cTitle := "Внимание", cIcon := "iDebug64"
   DEFAULT nSize := 64, aColors := {189,30,73}

   aClrs  := _SetMsgAlertColors()

   Darken2Open(hParentWin)           // Затенение на форме

   SET MSGALERT FONTCOLOR TO BLACK
   SET MSGALERT BACKCOLOR TO { 238, 249, 142 }  // светло-жёлтый

   AlertExclamation( cMsg, cTitle, cIcon, nSize, {aColors} )

   SET MSGALERT BACKCOLOR TO aClrs

   Darken2Close(hParentWin)            // Затенение на форме

   RETURN NIL

///////////////////////////////////////////////////////////////////////////
Function MG_Debug()
   LOCAL aParams := hb_aParams()
   RETURN MsgDebug( aParams )

///////////////////////////////////////////////////////////////////////////
Function MG_YesNo(cMsg, cTitle, cIcoRes, nIcoSize, aWinColor, cParentWin, aBtnMsg, aBtnColor)
   LOCAL aClrs, hParentWin, nI, lRet := .F.
   DEFAULT aBtnColor := { LGREEN , {189,30,73} }
   DEFAULT aBtnMsg    := {"Co&ntinue", "&Cancel"}
   DEFAULT cParentWin := _HMG_ThisFormName
   DEFAULT cIcoRes    := "iSmile64", nIcoSize := 64
   DEFAULT cTitle     := "Please select"
   DEFAULT aWinColor  := { { 178, 162, 199 } , WHITE }

   aClrs  := _SetMsgAlertColors()

   IF ! empty(cParentWin) .and. _IsWindowDefined( cParentWin )
       hParentWin := GetFormHandle( cParentWin )
   ENDIF
   IF ! empty( hParentWin )
      hParentWin := GetFormHandle( cParentWin )
      Darken2Open(hParentWin)           // Затенение на форме
   ENDIF

   SET MSGALERT FONTCOLOR TO aWinColor[2]
   SET MSGALERT BACKCOLOR TO aWinColor[1]
   _HMG_ModalDialogReturn := 2
   nI := HMG_Alert( cMsg, aBtnMsg, cTitle, Nil, cIcoRes, nIcoSize, aBtnColor )
   IF nI == 1
      lRet := .T.
   ENDIF
   _HMG_ModalDialogReturn := 1

   SET MSGALERT BACKCOLOR TO aClrs

   IF ! empty( hParentWin )
      Darken2Close(hParentWin)            // Затенение на форме
   ENDIF

   RETURN lRet

////////////////////////////////////////////////////////////////////
FUNCTION HMG_SetMousePos( nHandle, y1, x1 )
   LOCAL c := _HMG_MouseCol
   LOCAL r := _HMG_MouseRow
   Local y := GetWindowRow(nHandle)
   Local x := GetWindowCol(nHandle)
   Default y1 := 1, x1 := 1

   SetCursorPos( x + x1, y + y1 )

RETURN {c,r}

////////////////////////////////////////////////////////////////////
FUNCTION HMG_MouseGet()
   LOCAL x := _HMG_MouseCol
   LOCAL y := _HMG_MouseRow
RETURN {x,y}

////////////////////////////////////////////////////////////////////
FUNCTION HMG_MouseSet(aXY)
   LOCAL aXYold := HMG_MouseGet()
   SetCursorPos( aXY[1], aXY[2] )
RETURN aXYold

//////////////////////////////////////////////////////////////////////////////
FUNCTION InfoDbase()
RETURN AlertInfo( Base_Current(), "Open databases" )

//////////////////////////////////////////////////////////////////////////////
FUNCTION Base_Current(cPar)
   LOCAL cMsg, nI, nSel, nOrder, cAlias, cIndx, aIndx := {}
   DEFAULT cPar := ""

   cAlias := ALIAS()
   nSel := SELECT(cAlias)
   IF nSel == 0
      cMsg := "No open BASE !" + CRLF
      RETURN cMsg
   ENDIF

   nOrder := INDEXORD()
   cMsg   := "Open Database - alias: " + cAlias + "   RddName: " + RddName() + CRLF
   cMsg   += "Path to the database - " + DBINFO(DBI_FULLPATH) + CRLF + CRLF
   cMsg   += "Open indexes: "

   IF nOrder == 0
      cMsg += " (no indexes) !" + CRLF
   ELSE
      cMsg += ' DBOI_ORDERCOUNT: ( ' + HB_NtoS(DBORDERINFO(DBOI_ORDERCOUNT)) + ' )' + CRLF + CRLF
      FOR nI := 1 TO 100
         cIndx := ALLTRIM( DBORDERINFO(DBOI_FULLPATH,,ORDNAME(nI)) )
         IF cIndx == ""
            EXIT
         ELSE
            DBSetOrder( nI )
            cMsg += HB_NtoS(nI) + ') - Index file: '  + CRLF + DBORDERINFO(DBOI_FULLPATH) + CRLF
            cMsg += '     Index Focus: ' + ORDSETFOCUS() + ",  DBSetOrder(" + HB_NtoS(nI)+ ")" + CRLF
            cMsg += '       Index key: "' + DBORDERINFO( DBOI_EXPRESSION ) + '"' + CRLF
            cMsg += '       FOR index: "' + OrdFor() + '"' + CRLF
            cMsg += '   DBOI_KEYCOUNT: ( ' + HB_NtoS(DBORDERINFO(DBOI_KEYCOUNT)) + ' )' + CRLF + CRLF
            AADD( aIndx, STR(nI,3) + "  OrdName: " + OrdName(nI) + "  OrdKey: " + OrdKey(nI) )
         ENDIF
      NEXT
      DBSetOrder( nOrder )
      cMsg += "Current index = "+HB_NtoS(nOrder)+" , Index Focus: " + ORDSETFOCUS()
   ENDIF
   cMsg += "          Number of records = " + HB_NtoS(ORDKEYCOUNT()) + CRLF

   RETURN cMsg

* ======================================================================
* При наличии файла добавить число версии в имя
FUNCTION GetFileNameMaskNum( cFile ) //FileNameMaskNum( cFile )
   LOCAL i := 0, cPth, cFil, cExt

   If ! hb_FileExists(cFile); RETURN cFile
   EndIf

   hb_FNameSplit(cFile, @cPth, @cFil, @cExt)

   WHILE ( hb_FileExists( hb_FNameMerge(cPth, cFil + '(' + hb_ntos(++i) + ')', cExt) ) )
   END

   RETURN hb_FNameMerge(cPth, cFil + '(' + hb_ntos(i) + ')', cExt)

* =========================================================================
* При наличии файла добавить число версии в имя файла без расширения файла
FUNCTION GetFileNameMaskNumNotExt( cFile )
   LOCAL i := 0, cPth, cFil, cExt

   If ! hb_FileExists(cFile); RETURN cFile
   EndIf

   hb_FNameSplit(cFile, @cPth, @cFil, @cExt)

   WHILE ( hb_FileExists( hb_FNameMerge(cPth, cFil + '(' + hb_ntos(++i) + ')', cExt) ) )
   END

   RETURN hb_FNameMerge(cPth, cFil + '(' + hb_ntos(i) + ')', cExt)

