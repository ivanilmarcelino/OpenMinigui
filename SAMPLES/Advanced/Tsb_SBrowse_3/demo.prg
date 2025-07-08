/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
*/
#define _HMG_OUTLOG

#include "hmg.ch"
#include "tsbrowse.ch"

REQUEST HB_CODEPAGE_RU1251, HB_CODEPAGE_RU866
REQUEST DBFCDX, DBFFPT
//REQUEST HB_MEMIO

MEMVAR oMain

/////////////////////////////////////////////////////////////////////////
FUNCTION Main( cWinType )
   LOCAL cFont := "Arial" // "DejaVu Sans Mono"
   LOCAL nSize := 12
   LOCAL cLog  := "_msg.log"
   LOCAL y, x, nY, nX, nW, nH, tTime

   //SET LANGUAGE TO RUSSIAN
   SET CODEPAGE TO RUSSIAN

   SET DATE  TO GERMAN
   SET EPOCH TO 2000

   SET NAVIGATION EXTENDED
   SET MULTIPLE OFF WARNING
   //////////
   SET OOP ON
   //////////
   RDDSETDEFAULT('DBFCDX')

   SET DELETED ON

   SET MSGALERT BACKCOLOR TO { 238, 249, 142 }  // for HMG_Alert()

   SET FONT TO cFont, nSize

   DEFINE FONT Normal FONTNAME cFont SIZE nSize
   DEFINE FONT Bold  FONTNAME cFont SIZE nSize  BOLD
   DEFINE FONT Italic FONTNAME cFont SIZE nSize-2 ITALIC

   DEFINE FONT DlgFont  FONTNAME "DejaVu Sans Mono"   SIZE 16  // for HMG_Alert()

   DEFAULT cWinType := "S"
           cWinType := upper(left(cWinType, 1))
           cWinType := iif( cWinType $ "CS", cWinType, "S" )

   _SetGetLogFile( cLog ) ; DeleteFile( cLog )

   PRIVATE oMain

   tTime := HB_DATETIME()
   ? REPL("=",20) + " Program start - " + HB_TTOC( HB_DATETIME() ) + " " + REPL("=",20)
   ? MiniGuiVersion()  ;  ?

   DEFINE WINDOW wMain AT 0,0 WIDTH 270 HEIGHT 150  ;
      TITLE 'SBrowse Viewer' ;
      MAIN NOMAXIMIZE NOSIZE ;
      BACKCOLOR  { 0,64,80 } ;
      ON RELEASE {|| _wSend(91, This.Name) } ;
      ON INIT  {|| DoEvents(), _wPost(0) } ;
      ON INTERACTIVECLOSE {|| _wSend(98, This.Name), This.Cargo:lCloseForm }

      This.Cargo := oHmgData() ; oMain := This.Object

      This.Cargo:lCloseForm := .T.
      This.Cargo:cWinType := cWinType
      This.Cargo:aMode := { "a_1", "a_2", "a_3" }
      This.Cargo:aSupHdFore := { CLR_YELLOW, CLR_YELLOW, CLR_YELLOW }
      This.Cargo:aSupHdBack := { CLR_CYAN  , CLR_BLUE  , CLR_HBLUE  }
      This.Cargo:aForm := { "", "", "" }
      This.Cargo:aRow := { -60,  65,  65 }
      This.Cargo:aCol := { 10, -250, 250 }

      nW := This.ClientWidth
      nH := This.ClientHeight

        nY := 20 ; y := nY
        nX := 20 ; x := nX + 40 + 20

      @ nY, nX BUTTON Btn_1 CAPTION "1" WIDTH 40 HEIGHT 20 ACTION _wPost(1, ,.F.)
        nY += 20 + 10
      @ nY, nX BUTTON Btn_2 CAPTION "2" WIDTH 40 HEIGHT 20 ACTION _wPost(2, ,.F.)
        nY += 20 + 10
      @ nY, nX BUTTON Btn_3 CAPTION "3" WIDTH 40 HEIGHT 20 ACTION _wPost(3, ,.F.)

      @ y, x BUTTON Btn_T CAPTION "Windows <S>" WIDTH 150 HEIGHT 20 ACTION _wPost(10, This.Index)
        This.Btn_T.Cargo := 1
        y += 20 + 10
      @ y, x BUTTON Btn_A CAPTION "Show all"  WIDTH 150 HEIGHT 20 ACTION _wPost(11)
        y += 20 + 10
      @ y, x BUTTON Btn_X CAPTION "Close all" WIDTH 150 HEIGHT 20 ACTION _wPost(12)

      WITH OBJECT This.Object
      :Event( 0, {|ow| DoEvents(), _wPost(1, ow:Name, .T.) } )
      :Event( 1, {|ow,ky,xv| ky := Test_Window_1(ow, !Empty(xv)) } )
      :Event( 2, {|ow,ky,xv| ky := Test_Window_2(ow, !Empty(xv)) } )
      :Event( 3, {|ow,ky,xv| ky := Test_Window_3(ow, !Empty(xv)) } )
      :Event( 4, {|ow,ky,xv| _LogFile(.T., "<<< END >>> Auto windowing ! Type =", ow:Cargo:cWinType, ow:Name,ky,xv ) } )
      :Event(10, {|  |
                  Local cCapt, nType, cType, aType := {"S", "C"}
                  IF Len( HMG_GetForms() ) < 2    // there is wMain window only
                     cCapt := This.Caption
                     nType := iif( This.Cargo == 1, 2, 1 )
                     cType := aType[ nType ]
                     This.Caption := left(cCapt, RAt("<", cCapt))+cType+">"
                     This.Cargo   := nType
                     ThisWindow.Cargo:cWinType := cType
                  ENDIF
                  Return Nil
                 } )
      :Event(11, {|  | iif( Len(HMG_GetForms()) < 2, _wPost(0), Nil ) } )
      :Event(12, {|  |
                  Local aForm, cType, aType := {"S", "C"}
                  Local cForm, nType := This.Btn_T.Cargo
                  cType := aType[ nType ]
                  aForm := HMG_GetForms(cType)
                  IF Len(aForm) > 0
                     FOR EACH cForm IN aForm
                         DoMethod(cForm, "Release")
                     NEXT
                  ENDIF
                  Return Nil
                 } )
      :Event(91, {|  | _LogFile(.T., ">>> STOP <<<  "+HMG_TimeMS(tTime)), _LogFile() } )
      :Event(98, {|  | This.Cargo:lCloseForm := Len(HMG_GetForms()) < 2 } )
      :Event(99, {|ow| ow:Release()  } )
      END WITH

   END WINDOW

   //CENTER WINDOW wMain
   ACTIVATE WINDOW wMain

RETURN Nil

/////////////////////////////////////////////////////////////////////
STATIC FUNCTION Get_Array(oWnd, nMode)
   LOCAL aDim := oWnd:Cargo:aMode
   LOCAL cDim := aDim[ nMode ]
   LOCAL cFil := ".\"+cDim+".txt"
   LOCAL aBuf := {}, cBuf := ""

   ? procname(), oWnd:Name, nMode, cFil, hb_FileExists( cFil )

   IF hb_FileExists( cFil )
      cBuf := hb_memoread(cFil)
   ENDIF

   IF !Empty(cBuf) .and. left(cBuf, 1) == "{" .and. right(cBuf, 1) == "}"
      aBuf := &( cBuf )
   ENDIF

   ?? aBuf

RETURN aBuf

/////////////////////////////////////////////////////////////////////
STATIC FUNCTION Test_Window_1(oWnd, lPostMsg)
   LOCAL cView, cWnd, oTsb := oHmgData(), nW, nH
   LOCAL nMode := 1
   DEFAULT oWnd := ThisWindow.Object, lPostMsg := .T.

   cWnd  := oWnd:Name
   cView := oMain:Cargo:aForm[ nMode ]

   IF _IsWindowDefined(cView)
      DoMethod(cView, "Minimize") ; DO EVENTS
      DoMethod(cView, "Restore" ) ; DO EVENTS
      DoMethod(cView, "oBrw", "SetFocus" )
      DO EVENTS
      RETURN Nil
   ENDIF

   oTsb:aArray := Get_Array(oWnd, nMode)

   ? ProcName(), "Select =", nMode, cWnd, oTsb:aArray

   oTsb:nMode  := nMode
   oTsb:nForm  := nMode
   oTsb:lPostMsg := lPostMsg
   oTsb:cWinType := oMain:Cargo:cWinType

   oTsb:cTitle := "Viewing a selection of duplicates personal accounts"
   oTsb:aHeader  := { "Delete", "RecNo", "Keys", "Field", "IdAb", "Full Name"}
   oTsb:aName  := { "DEL" , "REC"  , "KOD" , "NAME" , "IDAB", "FIO"  }
   oTsb:aAlign := {  1  ,  1 ,  1  ,   0 ,   1  ,  0 }

   oTsb:lNoHScroll   := .T.               // горизонт. скрола нет

   oTsb:cSupHdCapt   := "MY SUPER HEADER"+Str(nMode, 3)
   oTsb:nSupHdFore   := oWnd:Cargo:aSupHdFore[ nMode ]
   oTsb:nSupHdBack   := oWnd:Cargo:aSupHdBack[ nMode ]
   oTsb:nSupHdHeight := 28

   oTsb:bAfter  := {|obrw|                   // после END TSBROWSE  выполняется
       Local oTsb, cWnd, nWnd, cTmp, lMsg, nMode, cMain
       oTsb  := obrw:Cargo
       cWnd  := obrw:cParentWnd
       nWnd  := oTsb:nForm
       lMsg  := oTsb:lPostMsg
       nMode := oTsb:nMode
       cMain := oMain:Name
       oMain:Cargo:aForm[ nMode ] := cWnd
       // ...
       obrw:AdjColumns()
       obrw:Refresh(.T.)
       SetProperty( cWnd, "Topmost", .F. )

       IF lMsg
          cTmp  := 'my_OnInit_SBrowse(' + hb_ntos(nMode) + ', "'+cWnd+'")'
          SetProperty( cWnd, "ONINIT", hb_MacroBlock(cTmp) )
       ENDIF
       DO EVENTS
       Return Nil
       }

   nW := Sys.ClientWidth * 0.6
   nH := Sys.ClientHeight * 0.8

   SBrowse_Viewer( oTsb, nW, nH, , .T., {}, oTsb:cWinType )

RETURN Nil

/////////////////////////////////////////////////////////////////////
STATIC FUNCTION Test_Window_2(oWnd, lPostMsg)
   LOCAL cView, cWnd, oTsb := oHmgData(), nW, nH
   LOCAL m, nMode := 2
   DEFAULT oWnd := ThisWindow.Object, lPostMsg := .T.

   cWnd  := oWnd:Name
   cView := oMain:Cargo:aForm[ nMode ]

   IF _IsWindowDefined(cView)
      DoMethod(cView, "Minimize") ; DO EVENTS
      DoMethod(cView, "Restore" ) ; DO EVENTS
      DoMethod(cView, "oBrw", "SetFocus" )
      DO EVENTS
      RETURN Nil
   ENDIF

   oTsb:aArray := Get_Array(oWnd, nMode)

   ? ProcName(), "Select =", nMode, cWnd, oTsb:aArray

   oTsb:nMode  := nMode
   oTsb:nForm  := nMode
   oTsb:lPostMsg := lPostMsg
   oTsb:cWinType := oMain:Cargo:cWinType

   oTsb:cTitle := "Viewing a selection of duplicates personal accounts"
   oTsb:aHeader  := { "Delete", "RecNo", "Keys", "Field", "IdAb", "Full Name"}
   oTsb:aName  := { "DEL" , "REC"  , "KOD" , "NAME" , "IDAB", "FIO"  }
   oTsb:aAlign := {  1  ,  1 ,  1  ,   0 ,   1  ,  0 }

   oTsb:lNoHScroll := .T.               // горизонт. скрола нет
   oTsb:lCellBrw   := .T.               // режим cell курсора

   // cursor colors, цвета для курсора cell (при oTsb:lCellBrw := .T.):
   oTsb:nClr06_1 := -CLR_HRED
   oTsb:nClr06_2 := -RGB( 128, 225, 225 )
   oTsb:nClr12_1 := -CLR_BLUE
   oTsb:nClr12_2 := -RGB( 128, 225, 225 )

   oTsb:cSupHdCapt   := "MY SUPER HEADER"+Str(nMode, 3)
   oTsb:nSupHdFore   := oWnd:Cargo:aSupHdFore[ nMode ]
   oTsb:nSupHdBack   := oWnd:Cargo:aSupHdBack[ nMode ]
   oTsb:nSupHdHeight := 28

   // group highlighting, подсветка групп KOD (header "Keys"):
   oTsb:oKod := oHmgData()         // Keys unique
   FOR EACH m IN oTsb:aArray
       oTsb:oKod:Set(m[3], 0)
   NEXT
   AEval(oTsb:oKod:GetAll(), {|a,i| oTsb:oKod:Set(a[1], int(i % 2)) })

   oTsb:bKodCell := {|obrw,ocel,ocol|
       Local o := ocol:Cargo, nClr, nTo, cKod, nElm
       IF o:lColor
          nClr := ocel:nClrBack
          nTo  := ocel:nClrTo
          cKod := ocel:uValue
          nElm := o:oColor:Get(cKod, 0)
          ocel:nClrBack := iif( nElm > 0, o:nColor, nClr )
          ocel:nClrTo   := iif( nElm > 0, o:nColor, nTo  )
       ENDIF
       o := obrw
       Return Nil
       }

   oTsb:bBefore := {|obrw|                               // before END TSBROWSE
       Local oCol, oTsb
       oTsb := obrw:Cargo
       oCol := obrw:GetColumn("KOD")
       oCol:Cargo := oHmgData()
       oCol:Cargo:oColor := oTsb:oKod                    // keys unique
       oCol:Cargo:lColor := Len(obrw:aArray) > 1
       oCol:Cargo:nColor := CLR_HGRAY        // GetSysColor( COLOR_BTNFACE )
       oCol:bDrawCell    := oTsb:bKodCell
       Return Nil
       }

   oTsb:bAfter  := {|obrw|                   // after END TSBROWSE
       Local oTsb, cWnd, nWnd, cTmp, lMsg, nMode, cMain
       oTsb  := obrw:Cargo
       cWnd  := obrw:cParentWnd
       nWnd  := oTsb:nForm
       lMsg  := oTsb:lPostMsg
       nMode := oTsb:nMode
       cMain := oMain:Name
       oMain:Cargo:aForm[ nMode ] := cWnd
       // ...
       obrw:AdjColumns()
       obrw:Refresh(.T.)
       SetProperty( cWnd, "Topmost", .F. )

       IF lMsg
          cTmp  := 'my_OnInit_SBrowse(' + hb_ntos(nMode) + ', "'+cWnd+'")'
          SetProperty( cWnd, "ONINIT", hb_MacroBlock(cTmp) )
       ENDIF
       DO EVENTS
       Return Nil
       }

   nW := Sys.ClientWidth * 0.6
   nH := Sys.ClientHeight * 0.8

   SBrowse_Viewer( oTsb, nW, nH, , .T., {}, oTsb:cWinType )

RETURN Nil

/////////////////////////////////////////////////////////////////////
STATIC FUNCTION Test_Window_3(oWnd, lPostMsg)
   LOCAL cView, cWnd, oTsb := oHmgData(), nW, nH
   LOCAL m, nMode := 3
   DEFAULT oWnd := ThisWindow.Object, lPostMsg := .T.

   cWnd  := oWnd:Name
   cView := oMain:Cargo:aForm[ nMode ]

   IF _IsWindowDefined(cView)
      DoMethod(cView, "Minimize") ; DO EVENTS
      DoMethod(cView, "Restore" ) ; DO EVENTS
      DoMethod(cView, "oBrw", "SetFocus" )
      DO EVENTS
      RETURN Nil
   ENDIF

   oTsb:aArray := Get_Array(oWnd, nMode)

   ? ProcName(), "Select =", nMode, cWnd, oTsb:aArray

   oTsb:nMode  := nMode
   oTsb:nForm  := nMode
   oTsb:lPostMsg := lPostMsg
   oTsb:cWinType := oMain:Cargo:cWinType

   oTsb:cTitle := "Viewing a selection of duplicates personal accounts"
   oTsb:aHeader  := { "Delete", "RecNo", "Keys", "Field", "IdAb", "Full Name"}
   oTsb:aName  := { "DEL" , "REC"  , "KOD" , "NAME" , "IDAB", "FIO"  }
   oTsb:aAlign := {  1  ,  1 ,  1  ,   0 ,   1  ,  0 }

   oTsb:lNoHScroll   := .T.               // горизонт. скрола нет

   oTsb:cSupHdCapt   := "MY SUPER HEADER"+Str(nMode, 3)
   oTsb:nSupHdFore   := oWnd:Cargo:aSupHdFore[ nMode ]
   oTsb:nSupHdBack   := oWnd:Cargo:aSupHdBack[ nMode ]
   oTsb:nSupHdHeight := 28

   // group highlighting, подсветка групп KOD (header "Keys"):
   oTsb:oKod := oHmgData()         // Keys unique
   FOR EACH m IN oTsb:aArray
       oTsb:oKod:Set(m[3], 0)
   NEXT
   AEval(oTsb:oKod:GetAll(), {|a,i| oTsb:oKod:Set(a[1], 1 - int(i % 2)) })

   oTsb:bKodCell := {|obrw,ocel,ocol|
       Local o := ocol:Cargo, nClr, nTo, cKod, nElm
       IF o:lColor
          nClr := ocel:nClrBack
          nTo  := ocel:nClrTo
          cKod := ocel:uValue
          nElm := o:oColor:Get(cKod, 0)
          ocel:nClrBack := iif( nElm > 0, o:nColor, nClr )
          ocel:nClrTo   := iif( nElm > 0, o:nColor, nTo  )
       ENDIF
       o := obrw
       Return Nil
       }

   oTsb:bBefore := {|obrw|                               // before END TSBROWSE
       Local oCol, oTsb
       oTsb := obrw:Cargo
       oCol := obrw:GetColumn("KOD")
       oCol:Cargo := oHmgData()
       oCol:Cargo:oColor := oTsb:oKod                    // keys unique
       oCol:Cargo:lColor := Len(obrw:aArray) > 1
       oCol:Cargo:nColor := GetSysColor( COLOR_BTNFACE )
       oCol:bDrawCell    := oTsb:bKodCell
       Return Nil
       }

   oTsb:bAfter  := {|obrw|                                // after END TSBROWSE
       Local oTsb, cWnd, nWnd, cTmp, lMsg, nMode, cMain
       oTsb  := obrw:Cargo
       cWnd  := obrw:cParentWnd
       nWnd  := oTsb:nForm
       lMsg  := oTsb:lPostMsg
       nMode := oTsb:nMode
       cMain := oMain:Name
       oMain:Cargo:aForm[ nMode ] := cWnd
       // ...
       obrw:AdjColumns()
       obrw:Refresh(.T.)
       SetProperty( cWnd, "Topmost", .F. )

       IF lMsg
          cTmp  := 'my_OnInit_SBrowse(' + hb_ntos(nMode) + ', "'+cWnd+'")'
          SetProperty( cWnd, "ONINIT", hb_MacroBlock(cTmp) )
       ENDIF
       DO EVENTS
       Return Nil
       }

   nW := Sys.ClientWidth * 0.6
   nH := Sys.ClientHeight * 0.8

   SBrowse_Viewer( oTsb, nW, nH, , .T., {}, oTsb:cWinType )

RETURN Nil


FUNCTION my_OnInit_SBrowse(nMode, cWnd)
   LOCAL oBrw, oTsb, nWnd

   SET WINDOW THIS TO cWnd

   oBrw  := This.oBrw.Object
   oTsb  := oBrw:Cargo
   nWnd  := oTsb:nForm

   ? procname(), nMode, cWnd, nWnd, oTsb, oBrw:cParentWnd, oBrw:cControlName

   This.Row := This.Row + oMain:Cargo:aRow[ nMode ]
   This.Col := This.Col + oMain:Cargo:aCol[ nMode ]

   SET WINDOW THIS TO

   DO EVENTS ; _wPost(nMode + 1, oMain:Name, .T.)

RETURN Nil

/////////////////////////////////////////////////////////////////////
FUNCTION SBrowse_Viewer( uTsb, nW, nH, cTitul, lNumber, aFont, lModal, lCenter )
   LOCAL oTsb, aMax, aRec, xVal, cVal, nCol, nLen, nI
   LOCAL lAlias := .F., uAlias, cName, cType
   DEFAULT lNumber := .T., cTitul := "", lModal := .T., lCenter := .T.

   IF hb_IsChar(uTsb)
      uAlias := uTsb
      lAlias := .T.
   ELSEIF hb_IsObject(uTsb)
      oTsb := uTsb
      IF hb_IsChar(oTsb:cAlias)
         uAlias := oTsb:cAlias
         lAlias := .T.
      ENDIF
   ELSEIF hb_IsArray(uTsb)
      oTsb := oHmgData()
      oTsb:aArray := uTsb
   ENDIF
   DEFAULT oTsb := oHmgData()
   DEFAULT oTsb:aFont   := aFont
   DEFAULT oTsb:aFHand  := array(4) ; aFill(oTsb:aFHand, 0)
   DEFAULT oTsb:lModal  := lModal
   DEFAULT oTsb:lNumber := lNumber
   DEFAULT oTsb:lCenter := lCenter
   DEFAULT oTsb:lEmptyValToChar := .T.
   IF ! lAlias .and. Empty(oTsb:aArray)
      MsgStop("SBrowse_Viewer :"+CRLF+CRLF+"Alias or array not defined !", "ERROR")
      RETURN Nil
   ENDIF
   IF Empty(oTsb:aWidth) .and. Empty(oTsb:aWidthMaxLen)
      IF lAlias
         nLen := (uAlias)->( fCount() )
         oTsb:aHeader := array(nLen)
         oTsb:aName  := array(nLen)
         oTsb:aAlign  := array(nLen) ; aFill(oTsb:aAlign, 0)
         oTsb:aFAlign := array(nLen) ; aFill(oTsb:aAlign, 1)
         aMax := array(nLen) ; aFill(aMax, 0)
         aRec := array(nLen)
         FOR nI := 1 TO nLen
             cName := (uAlias)->( FieldName(nI) )
             oTsb:aHeader[ nI ] := cName
             oTsb:aName  [ nI ] := cName
             aMax[ nI ] := (uAlias)->( FieldLen(nI) ) + 2
             cType := (uAlias)->( FieldType(nI) )
             switch cType
             case "+"
                aMax[ nI ] := 10
                oTsb:aAlign[ nI ] := 2
                exit
             case "^"
                aMax[ nI ] := 10
                oTsb:aAlign[ nI ] := 2
                exit
             case "@"
             case "="
             case "T"
                aMax[ nI ] := 19
                oTsb:aAlign[ nI ] := 1
                exit
             case "D"
                oTsb:aAlign[ nI ] := 1
                exit
             case "L"
                oTsb:aAlign[ nI ] := 1
                exit
             case "N"
                oTsb:aAlign[ nI ] := 2
                exit
             end switch
             oTsb:aFAlign[ nI ] := oTsb:aAlign[ nI ]
         NEXT
      ELSE
         nLen := Len(oTsb:aArray[1])
         aMax := array(nLen) ; aFill(aMax, 0)
         FOR EACH aRec IN oTsb:aArray
             FOR EACH xVal IN aRec
                 cVal := cValToChar(xVal)
                 nCol := hb_EnumIndex(xVal)
                 aMax[nCol] := Max( aMax[nCol], Len(cVal)+2)
             NEXT
         NEXT
      ENDIF
      oTsb:aWidthMaxLen := array(nLen)
      FOR nI := 1 TO nLen
          oTsb:aWidthMaxLen[ nI ] := aMax[ nI ]
      NEXT
   ENDIF
   DEFAULT oTsb:cTitle := "SBrowse viewer" + cTitul
   DEFAULT oTsb:bSetUp := {|ob,um|
     Local oTsb := ob:Cargo, oCol, nCol, cNum, nLen
     Local hFnt := ob:hFont, cVal, nI, nTmp, nColNo
     Local nFnt := Len(oTsb:aFHand)
     IF Empty( um )                  // before END TBROWSE
        IF hb_IsArray( oTsb:aFont )
           IF Len(oTsb:aFont) < nFnt ; ASize(oTsb:aFont, nFnt)
           ENDIF
           DEFAULT oTsb:aFont[1] := "Normal"
           DEFAULT oTsb:aFont[2] := "Bold"
           DEFAULT oTsb:aFont[3] := "Bold"
           DEFAULT oTsb:aFont[4] := "Italic"
           FOR nI := 1 TO nFnt
               IF !Empty(oTsb:aFont[ nI ]) .and. hb_IsChar(oTsb:aFont[ nI ])
                  IF !Empty( nTmp := GetFontHandle(oTsb:aFont[ nI ]) )
                     oTsb:aFHand[ nI ] := nTmp
                  ENDIF
               ENDIF
           NEXT
           IF !Empty(oTsb:aFHand[1]) ; hFnt := oTsb:aFHand[1]
           ENDIF
        ENDIF
        ob:lNoHScroll := !Empty(oTsb:lNoHScroll)
        nLen := Len(ob:aColumns)
        IF Empty(oTsb:aWidth) .and. ! Empty(oTsb:aWidthMaxLen)
           oTsb:aWidth := array(nLen)
           FOR nI := 1 TO nLen
               nTmp := oTsb:aWidthMaxLen[ nI ]
               nTmp := iif( nTmp > 50, 50, nTmp )
               nTmp := Max( nTmp, 5 )
               cVal := Replicate("B", nTmp)
               oTsb:aWidth[ nI ] := GetTextWidth( , cVal, hFnt ) * 0.9
           NEXT
        ENDIF
        DEFAULT oTsb:lCellBrw  := .F.
        DEFAULT oTsb:aHeader  := array(nLen)
        DEFAULT oTsb:aFooter  := array(nLen)
        DEFAULT oTsb:aWidth    := array(nLen)
        DEFAULT oTsb:aName := array(nLen)
        DEFAULT oTsb:aAlign    := array(nLen)
        DEFAULT oTsb:aFAlign  := array(nLen)
        DEFAULT oTsb:aPicture  := array(nLen)
        DEFAULT oTsb:aPrevEdit := array(nLen)
        DEFAULT oTsb:aPostEdit := array(nLen)
        DEFAULT oTsb:aEdit := array(nLen)
        FOR EACH oCol IN ob:aColumns
            nCol := hb_EnumIndex(oCol)
            cNum := hb_ntos(nCol)
            IF !Empty(oTsb:aFHand[1]) ; oCol:hFont := oTsb:aFHand[1]
            ENDIF
            IF !Empty(oTsb:aFHand[2]) ; oCol:hFontHead := oTsb:aFHand[2]
            ENDIF
            IF !Empty(oTsb:aFHand[3]) ; oCol:hFontFoot  := oTsb:aFHand[3]
            ENDIF
            IF !Empty(oTsb:aFHand[4]) ; oCol:hFontSpcHd := oTsb:aFHand[4]
            ENDIF
            oCol:cName  := iif( Empty(oTsb:aName  [nCol]), "COL" +cNum  , oTsb:aName  [nCol] )
            oCol:cHeading  := iif( Empty(oTsb:aHeader[nCol]), "Col "+cNum  , oTsb:aHeader[nCol] )
            oCol:cFooting  := iif( Empty(oTsb:aFooter[nCol]), ""     , oTsb:aFooter[nCol] )
            oCol:nWidth  := iif( Empty(oTsb:aWidth [nCol]), oCol:nWidth  , oTsb:aWidth [nCol] )
            oCol:nAlign  := iif( Empty(oTsb:aAlign [nCol]), 0      , oTsb:aAlign [nCol] )
            oCol:nFAlign   := iif( Empty(oTsb:aFAlign[nCol]), 1            , oTsb:aFAlign[nCol] )
            oCol:cPicture  := oTsb:aPicture [nCol]
            oCol:bPostEdit := oTsb:aPostEdit[nCol]
            oCol:bPrevEdit := iif( Empty(oTsb:aPrevEdit[nCol]), {|| .F. } , oTsb:aPrevEdit[nCol] )
            nTmp := oCol:ToWidth(oCol:cHeading, , .T.) + oCol:ToWidth(2, , .T.)
            oCol:nWidth := Max( oCol:nWidth, nTmp )
            oCol:lEmptyValToChar := oTsb:lEmptyValToChar
        NEXT
        ob:nHeightCell += 5
        IF hb_IsNumeric(oTsb:nSupHdFore) .and. hb_IsNumeric(oTsb:nSupHdBack) .and. hb_IsNumeric(oTsb:nSupHdHeight)
           DEFAULT oTsb:cSupHdCapt := GetProperty(ob:cParentWnd, "Title")
           ADD SUPER HEADER TO ob FROM 1 TO ob:nColCount() TITLE oTsb:cSupHdCapt ;
               HEIGHT oTsb:nSupHdHeight COLOR oTsb:nSupHdFore,oTsb:nSupHdBack HORZ DT_CENTER
        ENDIF
        IF hb_IsBlock(oTsb:bBefore) ; Eval(oTsb:bBefore, ob, oTsb)
        ENDIF
     ELSE                                   // after END TBROWSE
        nColNo := ob:nColumn( iif( ob:lIsDbf, "ORDKEYNO", "ARRAYNO" ) )
        IF nColNo > 0
           oCol := ob:aColumns[ nColNo ]
           IF !Empty(oTsb:aFHand[1]) ; oCol:hFont := oTsb:aFHand[1]
           ENDIF
           IF !Empty(oTsb:aFHand[2]) ; oCol:hFontHead := oTsb:aFHand[2]
           ENDIF
           IF !Empty(oTsb:aFHand[3]) ; oCol:hFontFoot  := oTsb:aFHand[3]
           ENDIF
           IF !Empty(oTsb:aFHand[4]) ; oCol:hFontSpcHd := oTsb:aFHand[4]
           ENDIF
        ENDIF
        IF hb_IsBlock(oTsb:bAfter) ; Eval(oTsb:bAfter, ob, oTsb)
        ENDIF
        IF oTsb:lCellBrw .and. !Empty(oTsb:nClr06_1) .and. ;
                               !Empty(oTsb:nClr06_2) .and. ;
                               !Empty(oTsb:nClr12_1) .and. ;
                               !Empty(oTsb:nClr12_2)
     ob:SetColor( { 5}, { GetSysColor( COLOR_WINDOWTEXT ) } )                               // CLR_FOCUSF
     ob:SetColor( { 6}, { {|c,n,b| c := b:Cargo, iif( b:nCell == n, c:nClr06_1, c:nClr06_2 )} } ) // CLR_FOCUSB
     ob:SetColor( {11}, { GetSysColor( COLOR_WINDOWTEXT ) } )                               // CLR_SELEF
     ob:SetColor( {12}, { {|c,n,b| c := b:Cargo, iif( b:nCell == n, c:nClr12_1, c:nClr12_2 )} } ) // CLR_SELEB
        ENDIF
        IF nColNo > 0 .and. hb_IsNumeric(oTsb:nSupHdFore) .and. ;
                            hb_IsNumeric(oTsb:nSupHdBack) .and. ;
                            hb_IsNumeric(oTsb:nSupHdHeight)
           nCol := Len(ob:aColumns) ; nI := Len( ob:aSuperHead )
           IF nCol > nI ; ob:aSuperHead[ nI ][2] := nCol
           ENDIF
        ENDIF
        ob:SetNoHoles()
        ob:SetFocus()
     ENDIF
     Return oTsb:lCellBrw
     }

   DEFAULT oTsb:bAfter := {|ob|         // после END TSBROWSE  выполняется
     Local nW, nL
     nW := This.oBrw.ClientWidth
     nL := ob:GetAllColsWidth() - 1
     IF nL > nW
        ob:lAdjColumn  := .T.
        ob:lNoHScroll  := .F.
        ob:lMoreFields := ( ob:nColCount() > 30 )
     ELSE
        ob:AdjColumns()
     ENDIF
     ob:Refresh()
     Return Nil
     }

   uAlias := iif( hb_IsChar(uAlias), uAlias, oTsb:aArray )

   SBrowse( uAlias, {oTsb:cTitle, oTsb}, oTsb:bSetUp, , nW, nH, , oTsb:lModal, oTsb:lNumber, oTsb:lCenter )

RETURN Nil

