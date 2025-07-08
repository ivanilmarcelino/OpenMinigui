/*
 * MINIGUI - Harbour Win32 GUI library Demo
*/

#define _HMG_OUTLOG

#include "hmg.ch"
#include "tsbrowse.ch"
#include "Dbinfo.ch"
#include "hbthread.ch"

#define EM_SETREADONLY  207

REQUEST HB_CODEPAGE_UTF8, HB_CODEPAGE_RU866, HB_CODEPAGE_RU1251
REQUEST DBFNTX, DBFCDX, DBFFPT, HB_MEMIO
//REQUEST BMDBFNTX, BMDBFCDX, BMDBFNSX, BM_DBSEEKWILD

MEMVAR oMain

*-----------------------------------
PROCEDURE Main( FileName, ... )
*-----------------------------------
   LOCAL aParam := hb_aParams(), aPar
   LOCAL cFont  := "Arial"
   LOCAL nSize  := 12
   LOCAL aBackColor := { 89, 155, 227} // {130, 180, 234} // {166, 202, 240} // {209, 227, 248} // {215,166, 0}
   LOCAL oA, nY, nX, nW, nH, aItm, i, j, k
   LOCAL cCdp, cEdi, cMax, cCard, cExcl

   SET CODEPAGE TO RUSSIAN
   SET LANGUAGE TO RUSSIAN

   RddSetDefault( 'DBFCDX' )

   SET DECIMALS  TO 4
   SET EPOCH     TO 2000
   SET DATE  TO GERMAN
   SET CENTURY ON
   SET DELETED OFF
   SET AUTOPEN OFF

   SET EXACT     ON
   SET EXCLUSIVE ON
   SET SOFTSEEK  ON

   SET OOP ON

   SET TOOLTIP MAXWIDTH TO 512
   SET TOOLTIP BALLOON  ON
   SET NAVIGATION EXTENDED
   SET DEFAULT ICON TO "1MAIN_ICO"

   SET FONT TO cFont, nSize

   DEFINE FONT Normal    FONTNAME cFont SIZE nSize
   DEFINE FONT Bold     FONTNAME cFont SIZE nSize BOLD
   DEFINE FONT Italic    FONTNAME cFont SIZE nSize ITALIC
   DEFINE FONT Underline FONTNAME cFont SIZE nSize UNDERLINE

   IF ! Empty(FileName)
      IF left(FileName, 1) == "-"
         aPar := { "", FileName }
         FileName := ""
         FOR i := 2 TO Len( aParam )
             IF left(aParam[ i ], 1) == "-"
                AAdd( aPar, aParam[ i ] )
             ENDIF
         NEXT
         aParam := aPar
      ELSEIF ! "\" $ FileName ; FileName := GetCurrentFolder()+"\"+FileName
      ENDIF
      IF ( k := Len( aParam ) ) > 1
          FOR i := 2 TO k
              j := aParam[ i ]
              IF left(j, 1) == "-"
                 IF    left(j, 2) == "-l" .or. left(j, 2) == "-L"
                    j := upper(subs(j, 4))
                    IF ! Empty( j ) .and. j $ "RU866,RU1251"
                       cCdp := j
                    ENDIF
                 ELSEIF left(j, 2) == "-e" .or. left(j, 2) == "-E"
                    j := upper(subs(j, 4))
                    IF ! Empty( j ) ; cEdi := iif( "Y" $ j, "Yes", "No" )
                    ENDIF
                 ELSEIF left(j, 2) == "-m" .or. left(j, 2) == "-M"
                    j := upper(subs(j, 4))
                    IF ! Empty( j ) ; cMax := iif( "Y" $ j, "Yes", "No" )
                    ENDIF
                 ELSEIF left(j, 2) == "-c" .or. left(j, 2) == "-C"           // Card Enter
                    IF ! Empty( j ) ; cCard := iif( "Y" $ j, "Yes", "No" )
                    ENDIF
                 ELSEIF left(j, 2) == "-o" .or. left(j, 2) == "-O"           // Open Exclusive
                    IF ! Empty( j ) ; cExcl := iif( subs(j, 4, 1) $ "Ee", "Yes", "No" )
                    ENDIF
                 ENDIF
              ENDIF
          NEXT
      ENDIF
   ENDIF

   oA := App.Object
   oA:Cargo := oKeyData()
   oA:Cargo:aFonts := {"Normal", "Bold", "Bold", "Italic", "Bold"}
   oA:Cargo:cWaitText := "... W A I T ... "
                                 //  1  2  3  4   5    6  7  8  9  10   // items number
                                 // { 1.5, 2.4, 1.5, 1.34, 1.2, 1.2, 1.2, 0.6, 1.1, 0.7 } // copy value
   oA:Cargo:aStatusBarItemsLen   := { 2.1, 2.4, 1.5, 1.34, 0.9, 1.2, 1.2, 0.6, 1.1, 0.7 } // In units oDlu
   oA:Cargo:aStatusBarItemsWidth := AClone( oA:Cargo:aStatusBarItemsLen )                // width in pixel
   oA:Cargo:aBrush    := { 255, 255, 230 }
   oA:Cargo:aColorTsb := { { 6, {|c,n,b| c := n, iif( b:nCell == n, -CLR_BLUE, -RGB(128,225,225) ) } }, ;
                           {12, {|c,n,b| c := n, iif( b:nCell == n, -CLR_BLUE, -RGB(128,225,225) ) } }  ;
                         }
   oA:Cargo:bSetsTsb  := {|ob|
                           ob:Cargo       := oKeyData()
                           ob:nColOrder     :=  0
                           ob:lNoChangeOrd  := .T.
                           ob:nWheelLines :=  1
                           ob:lNoGrayBar   := .F.
                           ob:lNoLiteBar   := .F.
                           ob:lNoResetPos := .F.
                           ob:lNoPopUp    := .T.
                           ob:lNoHScroll   := .T.
                           ob:lPickerMode := .F.         // data normal
                           ob:nCellMarginLR :=  1
                           ob:nStatusItem :=  0
                           ob:lNoKeyChar   := .T.        // method :KeyChar disabled
                           ob:nFireKey    :=  0        // key to start edition, defaults to VK_F2
                           ob:lCheckBoxAllReturn := .T.   // Enter edition CheckBox column
                           Return Nil
                         }
   aItm := oA:Cargo:aStatusBarItemsWidth
   AEval( aItm, {|nk,nn| aItm[nn] := oA:W(nk) } )  // ~ width status items
   aItm[1] := 0
   AEval( aItm, {|nw| aItm[1] += nw } )             // ~ width statusbar - item 1
   oA:Cargo:nStatusBarItemsLenNo1   := aItm[1]    // widtn status items no item first
   oA:Cargo:aStatusBarItemsWidth[1] := 0

   nY := nX := nW := 0
   nH := System.ClientHeight * 0.9
   AEval( oA:Cargo:aStatusBarItemsLen  , {|nk| nW += oA:W(nk) } )                // ~ width window

   DEFINE WINDOW wMain AT nY,nX WIDTH nW HEIGHT nH MINWIDTH nW MINHEIGHT nH ;
      TITLE 'MiniGUI TsBrowse MDI Demo ' ;
      MAIN MDI TOPMOST ;
      BACKCOLOR  {227, 238, 251} ;
      ON INTERACTIVECLOSE Len(HMG_GetForms("Y")) == 0 .or. MsgYesNo( "End the program ?", "Confirmation") ;
      ON INIT  ( DoEvents(), This.Topmost := .F., _wSend(5), DoEvents(), ;         // Refresh StatusBar items
                   iif( !Empty( This.Cargo:cFile ), _wPost(1), _wPost(2) ) )

      IF ! empty(cMax) .and. "Y" $ cMax ; This.Maximize
      ENDIF

      PUBLIC oMain       := This.Object
      This.Cargo        := oKeyData()
      This.Cargo:cFile  := ""
      This.Cargo:nRdd    := 1
      This.Cargo:aRdd    := {"DBFCDX", "DBFNTX"}
      This.Cargo:aMem    := {"FPT"  , "DBT"  }
      This.Cargo:cRdd    := This.Cargo:aRdd[ This.Cargo:nRdd ]
      This.Cargo:cMem    := This.Cargo:aMem[ This.Cargo:nRdd ]
      This.Cargo:nLang  := 1
      This.Cargo:aLang  := {"RU1251", "RU866" }
      This.Cargo:cLang  := This.Cargo:aLang[ This.Cargo:nLang ]
      This.Cargo:aEdit  := {"Yes", "No "}
      This.Cargo:lEdit  := .T.
      This.Cargo:cEdit  := This.Cargo:aEdit[ 1 ]
      This.Cargo:aShared := {"Shared", "Exclusive"}
      This.Cargo:lShared := .T.
      This.Cargo:cShared := This.Cargo:aShared[ 1 ]
      This.Cargo:lCardEnter := .F.

      If ! empty( FileName ) .and. File( FileName )
         This.Cargo:cFile := FileName
      EndIf
      If ! empty( cCdp )
         This.Cargo:nLang  := ASCAN( This.Cargo:aLang, cCdp )
         This.Cargo:cLang  := This.Cargo:aLang[ This.Cargo:nLang ]
      EndIf
      If ! empty( cEdi )
         This.Cargo:lEdit  := "Y" $ cEdi
         This.Cargo:cEdit  := This.Cargo:aEdit[ iif( This.Cargo:lEdit, 1, 2 ) ]
      EndIf
      If ! empty( cCard )
         This.Cargo:lCardEnter := "Y" $ cCard
      EndIf
      If ! empty( cExcl )
         This.Cargo:lShared := ! "Y" $ cExcl
         This.Cargo:cShared := This.Cargo:aShared[ iif( This.Cargo:lShared, 1, 2 ) ]
      EndIf

      aItm := oA:Cargo:aStatusBarItemsWidth

      DEFINE STATUSBAR FONT "Normal"
         STATUSITEM "" WIDTH aItm[1]    ACTION  Nil                         // ~ oA:W2
         STATUSITEM "" WIDTH aItm[2]    ACTION  Nil      FONTCOLOR PURPLE
         This.Cargo:nStatusItemRecNo  := _HMG_StatusItemCount
         STATUSITEM "" WIDTH aItm[3]    ACTION  Nil      FONTCOLOR PURPLE
         This.Cargo:nStatusItemColNo  := _HMG_StatusItemCount
         STATUSITEM "" WIDTH aItm[4]    ACTION _wPost(7) FONTCOLOR BROWN  RIGHTALIGN ICON "Lists24" ;
                                              TOOLTIP "Windows  Ctrl+W"
         This.Cargo:nStatusItemWin   := _HMG_StatusItemCount
         STATUSITEM "" WIDTH aItm[5]    ACTION _wPost(2) FONTCOLOR BLACK  RIGHTALIGN ICON "Open24"  ;
                                              TOOLTIP "Open file  Ctrl+O"
         This.Cargo:nStatusItemOpen := _HMG_StatusItemCount
         STATUSITEM "" WIDTH aItm[6]    ACTION _wPost(6) FONTCOLOR BLUE RIGHTALIGN ICON "Ok24"  ;
                                              TOOLTIP "Open mode  Ctrl+M"
         This.Cargo:nStatusItemShared := _HMG_StatusItemCount
         STATUSITEM "" WIDTH aItm[7]    ACTION _wPost(3) FONTCOLOR LGREEN RIGHTALIGN ICON "Base24" ;
                                              TOOLTIP "Open RDD  Ctrl+R"
         This.Cargo:nStatusItemRdd   := _HMG_StatusItemCount
         STATUSITEM "" WIDTH aItm[8]    ACTION _wPost(3) FONTCOLOR MAROON
         This.Cargo:nStatusItemMem   := _HMG_StatusItemCount
         STATUSITEM "" WIDTH aItm[9]    ACTION _wPost(4) FONTCOLOR LGREEN RIGHTALIGN ICON "Lang24" ;
                                              TOOLTIP "Language  Ctrl+L"
         This.Cargo:nStatusItemLang := _HMG_StatusItemCount
         STATUSITEM "" WIDTH aItm[10]  ACTION _wPost(8) FONTCOLOR BLUE  RIGHTALIGN ICON "Edit16"   ;
                                              TOOLTIP "Edit mode  Ctrl+E"
         This.Cargo:nStatusItemEdit := _HMG_StatusItemCount
         This.Cargo:nStatusItemCount  := _HMG_StatusItemCount
      END STATUSBAR

      (This.Object):Event( 1, {||                          // Child MDI window create
                                 Local cFile := This.Cargo:cFile
                                 Local cRdd := This.Cargo:cRdd
                                 Local cCdp := This.Cargo:cLang
                                 Local lShared := This.Cargo:lShared
                                 MdiChildOpen( cFile, cRdd, cCdp, lShared )
                                 This.Cargo:cFile := ""
                                 _wSend(5)                 // Refresh StatusBar items
                                 Return Nil
                              } )
      (This.Object):Event( 2, {|ow|                    // Selected file dbf
                                 Local cCapt := 'Open table...'
                                 Local cDir  := GetCurrentFolder()
                                 Local cFile := GetFile( { {"File DBF (*.DBF)", "*.DBF"} }, cCapt, cDir, , )
                                 If !Empty(cFile)
                                    This.Cargo:cFile := cFile
                                    _wSend(5)              // Refresh StatusBar items
                                    DO EVENTS
                                    _wPost(1)              // Child MDI window create
                                 ElseIf Len(HMG_GetForms("Y")) > 0
                                    ow := do_obj( GetActiveMdiHandle() )
                                    If ISOBJECT(ow) ; _wSend(25, ow) // Refresh oBrw
                                    EndIf
                                 EndIf
                                 Return Nil
                              } )
      (This.Object):Event( 3, {||                          // Selected RDD for file dbf
                                 Local aRdd := This.Cargo:aRdd
                                 Local aMem := This.Cargo:aMem
                                 Local nRdd := This.Cargo:nRdd + 1
                                 This.Cargo:nRdd := iif( nRdd > Len(aRdd), 1, nRdd )
                                 This.Cargo:cRdd := This.Cargo:aRdd[ This.Cargo:nRdd ]
                                 This.Cargo:cMem := This.Cargo:aMem[ This.Cargo:nRdd ]
                                 RddSetDefault( This.Cargo:cRdd )
                                 _wSend(5)                 // Refresh StatusBar items
                                 Return Nil
                              } )
      (This.Object):Event( 4, {||                          // Selected language for file dbf
                                 Local aLng := This.Cargo:aLang
                                 Local nLng := This.Cargo:nLang + 1
                                 This.Cargo:nLang := iif( nLng > Len(aLng), 1, nLng )
                                 This.Cargo:cLang := This.Cargo:aLang[ This.Cargo:nLang ]
                                 _wSend(5)                 // Refresh StatusBar items
                                 Return Nil
                              } )
      (This.Object):Event( 5, {||                          // Refresh StatusBar items
                                 Local nIwin := This.Cargo:nStatusItemWin
                                 Local nIopn := This.Cargo:nStatusItemOpen
                                 Local nIshr := This.Cargo:nStatusItemShared
                                 Local nIrdd := This.Cargo:nStatusItemRdd
                                 Local nImem := This.Cargo:nStatusItemMem
                                 Local nIlng := This.Cargo:nStatusItemLang
                                 Local nIedi := This.Cargo:nStatusItemEdit
                                 Local cLng  := This.Cargo:cLang
                                 Local cShar := This.Cargo:cShared
                                 Local cWin  := hb_ntos(Len(HMG_GetForms("Y")))
                                 cLng  += space( iif( Len(cLng ) < 6, 3, 1 ) )
                                 cShar += space( iif( Len(cShar) > 6, 1, 4 ) )
                                 This.StatusBar.Item(nIlng) := cLng
                                 This.StatusBar.Item(nIrdd) := This.Cargo:cRdd+"  "
                                 This.StatusBar.Item(nImem) := This.Cargo:cMem
                                 This.StatusBar.Item(nIwin) := "Windows: "+cWin+iif(Len(cWin) > 1, "", " ")
                                 This.StatusBar.Item(nIopn) := "Open"+"  "
                                 This.StatusBar.Item(nIshr) := This.Cargo:cShared+" "
                                 This.StatusBar.Item(nIedi) := This.Cargo:cEdit+" "
                                 Return Nil
                              } )
      (This.Object):Event( 6, {||                                // Selected open mode for file dbf
                                 This.Cargo:lShared := ! This.Cargo:lShared
                                 This.Cargo:cShared := This.Cargo:aShared[ iif( This.Cargo:lShared, 1, 2 ) ]
                                 _wSend(5, oMain)        // Refresh StatusBar items
                                 Return Nil
                              } )
      (This.Object):Event( 7, {|ow| WindowMenu(ow)  } )         // Selected open windows
      (This.Object):Event( 8, {||                                // Selected Edit mode for file dbf
                                 This.Cargo:lEdit := ! This.Cargo:lEdit
                                 This.Cargo:cEdit := This.Cargo:aEdit[ iif( This.Cargo:lEdit, 1, 2 ) ]
                                 _wSend(5, oMain)        // Refresh StatusBar items
                                 Return Nil
                              } )
      (This.Object):Event( 99, {|ow| ow:Release })

      ON KEY CONTROL+O ACTION _wPost(2)     // Open file
      ON KEY CONTROL+R ACTION _wPost(3)     // RDD selected
      ON KEY CONTROL+L ACTION _wPost(4)     // Language selected
      ON KEY CONTROL+M ACTION _wPost(6)     // Mode open Shared\Exclusive
    //ON KEY CONTROL+W ACTION _wPost(7)     // Window selected
      ON KEY CONTROL+E ACTION _wPost(8)     // Mode Edit Yes\No
      ON KEY F1        ACTION  NIL

      This.Minimize ;  This.Restore ; DO EVENTS

   END WINDOW

   wMain.CENTER
   wMain.ACTIVATE

RETURN

*-----------------------------------------------------------------------------*
STATIC FUNCTION FileCard( cCard, aRec )
*-----------------------------------------------------------------------------*
   LOCAL aStru, nArea, nLen, nPos, nVal, a, i
   LOCAL cAls  := upper(cCard)
   LOCAL cFile := "mem:"+cAls

   IF Empty( aRec )
      IF ( nArea := Select( cAls ) ) > 0
         ( nArea )->( dbCloseArea() )
      ENDIF
      dbDrop(cFile, cFile, "DBFCDX")
      RETURN ""
   ENDIF

   dbDrop(cFile, cFile, "DBFCDX")

   aStru := { {"R_COL", "N",  5, 0}, ;  // 1
              {"R_NAM", "C",  30, 0}, ;  // 2
              {"R_MOD", "L",  1, 0}, ;  // 3
              {"R_VAL", "C", 100, 0}, ;  // 4
              {"R_EDI", "L",  1, 0}, ;  // 5
              {"R_FLD", "C",  10, 0}, ;  // 6
              {"R_TYP", "C",  1, 0}, ;  // 7
              {"R_LEN", "N",  5, 0}, ;  // 8
              {"R_DEC", "N",  1, 0}  ;  // 9
            }

   nPos := 8 ; nVal := 4 ; nLen := 0
   FOR EACH a IN aRec ; nLen := Max( nLen, a[ nPos ] )
   NEXT
   aStru[ nVal ][3] := nLen

   dbCreate( cFile, aStru, "DBFCDX", .T., cAls )

   FOR EACH a IN aRec
       APPEND BLANK
       FOR i := 1 TO Len( a ) ; FieldPut(i, a[ i ])
       NEXT
   NEXT

   GO TOP
   INDEX ON RecNo() TAG ID
   SET ORDER TO 1
   GO TOP

RETURN ALIAS()

*-----------------------------------------------------------------------------*
STATIC FUNCTION MdiChildEditBox()
*-----------------------------------------------------------------------------*
   LOCAL cForm := "wMemo"
   LOCAL oWnd  := ThisWindow.Object, oA := App.Object
   LOCAL cMode := This.ToolBar_1.Caption
   LOCAL cTit1 := oWnd:Title
   LOCAL cFile := oWnd:Cargo:cFile
   LOCAL lEdit := oWnd:Cargo:lEdit
   LOCAL oBrw  := This.oBrw.Object               // Card tsb
   LOCAL oEdit := oBrw:Cargo:oEditCol
   LOCAL oCol  := oBrw:GetColumn( oBrw:nCell )
   LOCAL oCol1 := oBrw:GetColumn( oBrw:nCell-2 )
   LOCAL oCol2 := oBrw:GetColumn( oBrw:nCell-3 )
   LOCAL cVal  := oBrw:GetValue( oCol  )
   LOCAL cVal1 := oBrw:GetValue( oCol1 )
   LOCAL cVal2 := oBrw:GetValue( oCol2 )
   LOCAL cPos  := oBrw:GetValue("col")
   LOCAL cCol  := oBrw:GetValue("NAM")
   LOCAL oTsb  := oWnd:Cargo:oTsb                 // Table tsb for card list
   LOCAL oWner := _WindowObj(oTsb:cParentWnd)
   LOCAL cCapt := oWnd:Cargo:cCapt+" "+"Edit column: "+cVal2+".  "+cVal1
   LOCAL cTit2 := oWner:Title
   LOCAL cTitl := cTit2+".  "+cTit1+"  "+"Edit column: "+cVal2+".  "+cVal1+" "
   LOCAL cBmpE := iif( oWnd:Cargo:lEdit, 'Edit24', 'Prev24' )
   LOCAL i, k, y, x, w, h, hSpl


   DEFINE WINDOW &cForm TITLE " "+cTitl MDICHILD ;
      ON INIT   ( _wSend(5, oMain), _wPost(1) ) ;
      ON RELEASE  Nil
      This.Maximize
      This.Cargo := oKeyData()
      This.Cargo:cTitle := trim(cTitl) + ". "
      This.Cargo:cCapt   := cCapt
      This.Cargo:cForm   := This.Name
      This.Cargo:oParent  := oWnd               // window for card list
      This.Cargo:lModify  := .F.              // memo modify
      This.Cargo:lEdit   := lEdit
      This.Cargo:oCard   := oBrw
      This.Cargo:oCardCol := oCol
      This.Cargo:oTablCol := oEdit
      This.Cargo:oTabl   := oTsb
      This.Cargo:lIsCrLf  := .F.
      This.Cargo:cType   := oEdit:cFieldTyp

      IF oEdit:cFieldTyp == "C"
         This.Cargo:lIsCrLf  := CRLF $ cVal
      ENDIF

      i := 24 + oA:W(.4)
      k := 24 + oA:H1 + 2
      DEFINE SPLITBOX HANDLE hSpl
      DEFINE TOOLBAR ToolBar_1 CAPTION " Mode : "+cMode FONT "Normal" BUTTONSIZE i,k  FLAT
         BUTTON btnMode  PICTURE cBmpE     CAPTION " "      ACTION  NIL           SEPARATOR
         BUTTON btnSave  PICTURE 'Save24'  CAPTION "Save"   ACTION  _wPost(3)     SEPARATOR ;
                         TOOLTIP iif( lEdit, "Save  F2, Ctrl+W ", Nil )
      END TOOLBAR
      DEFINE TOOLBAR ToolBar_2 CAPTION "" FONT "Normal"      BUTTONSIZE i,k FLAT
         BUTTON btnExit  PICTURE 'Return24' CAPTION "Exit" ACTION _wPost(99) TOOLTIP " "+"   Esc "
      END TOOLBAR
      END SPLITBOX

      This.btnSave.Enabled := .F.

      y := GetWindowHeight(hSpl)
      x := 1
      w := This.ClientWidth  - x * 2
      h := This.ClientHeight - x * 2 - y

      @ y, x EDITBOX Edit WIDTH w HEIGHT h VALUE cVal NOHSCROLL ON INIT {|| This.Cargo := This.Value }

      If This.Cargo:lEdit                                                         // Edit memo
         DEFINE TIMER TimerEd INTERVAL 200 ACTION _wPost(2)
         This.TimerEd.Enabled := .F.

         ON KEY F2        ACTION iif( ThisWindow.Cargo:lModify, _wPost(3), Nil ) // Save
         ON KEY CONTROL+W ACTION {||
                                  If ThisWindow.Cargo:lModify ; _wPost(3)         // Save
                                  EndIf
                                  Return Nil
                                 }
      Else                                                                        // ReadOnly
         SendMessage( This.Edit.Handle, EM_SETREADONLY, 1, 0 )
      EndIf

      (This.Object):Event( 1, {||                                                  // Enable TimerEd
                                  If This.Cargo:lEdit
                                     This.TimerEd.Enabled := .T.
                                  EndIf
                                  Return Nil
                              } )
      (This.Object):Event( 2, {||                                                  // Modify
                                  This.TimerEd.Enabled := .F.
                                  If ! This.Edit.Cargo == This.Edit.Value
                                     This.btnSave.Enabled := .T.
                                     This.Cargo:lModify  := .T.                   // memo is modify
                                     DO EVENTS
                                  EndIf
                                  If ! This.btnSave.Enabled
                                     This.TimerEd.Enabled := .T.
                                  EndIf
                                  Return Nil
                              } )
      (This.Object):Event( 3, {||                                                  // Save
                                  Local cVal := This.Edit.Value
                                  IF This.Cargo:lIsCrLf .and. CRLF $ cVal
                                     IF right(cVal, 2) == CRLF
                                        cVal := left(cVal, Len(cVal)-2)
                                     ENDIF
                                     cVal := StrTran(cVal, CRLF, " ")
                                  ENDIF
                                  DO EVENTS
                                  _wPost(99, , cVal)
                                  Return Nil
                              } )
      (This.Object):Event(99, {|ow,ob,txt|
                                  ob := ow:Cargo:oCard
                                  ow:Release()
                                  DO EVENTS
                                  _wSend(5, oMain)
                                  ActivateMdiChildWindow(ob:cParentWnd)
                                  DO EVENTS
                                  If txt != Nil
                                     _wPost(2, ob:cParentWnd, txt)
                                  EndIf
                                  Return Nil
                              } )

      ON KEY ESCAPE  ACTION _wPost(99)

   END WINDOW

RETURN Nil

*-----------------------------------------------------------------------------*
STATIC FUNCTION MdiChildCard()
*-----------------------------------------------------------------------------*
   LOCAL i, k, y, x, w, h, o
   LOCAL hSpl, xVal, cVal, cNam, cCol, nCol, xCol, lCol, lTyp
   LOCAL cForm := "wCard", oBrw, oCard, oCol, cFld
   LOCAL oWnd  := ThisWindow.Object, oA := App.Object
   LOCAL cCapt := oWnd:Cargo:cCapt
   LOCAL cMode := oWnd:Cargo:cMode
   LOCAL cFile := oWnd:Cargo:cFile
   LOCAL lEdit := oWnd:Cargo:lEdit
   LOCAL oTsb  := This.oBrw.Object
   LOCAL nTsbRec  := (oTsb:cAlias)->( RecNo() )
   LOCAL lTsbRec  := (oTsb:cAlias)->( Deleted() )
   LOCAL cTitl := hb_ntos(nTsbRec) + " - CARD "
   LOCAL nArea := Select()
   LOCAL cCard := oWnd:Name+"_"+hb_ntoc(nTsbRec)
   LOCAL aHead := { "Column", "Name"   , " "  , "Value" }
   LOCAL aSize := { oA:W(.9), oA:W(1.7), 20  , oA:W3  }
   LOCAL aName := { "col"  , "NAM"  , "MOD", "VAL" }
   LOCAL aJust := {  1   , 0    ,  1  ,  0    }
   LOCAL nMem  := 0
   LOCAL aVal  := {}
   LOCAL aCol  := {}

   oCard := oWnd:Cargo:oCardRec:Get( nTsbRec )

   IF ISOBJECT( oCard )
      ActivateMdiChildWindow( oCard:Name )
      o := oCard:GetObj("oBrw")
      IF ISOBJECT( o ) ; o:Tsb:SetFocus()
      ENDIF
      RETURN Nil
   ENDIF

   FOR nCol := 1 TO Len(oTsb:aColumns)
       oCol := oTsb:aColumns[ nCol ]
       If nCol == 1 .and. oTsb:lSelector ; LOOP
       ElseIf ! oCol:lVisible          ; LOOP
       ElseIf oCol:lBitMap             ; LOOP
       EndIf
       cCol := iif( oCol:cName == "ORDKEYNO", "Card", oCol:cName )
       xVal := oTsb:GetValue( nCol )
       xCol := xVal
       lTyp := oCol:cFieldTyp $ "CNDLM"
       nMem += iif( oCol:cFieldTyp == "M", 1, 0 )
       lCol := lEdit .and. lTyp .and. ! ","+cCol+"," $ ",Card,"
       xCol := iif( lTyp, xCol, cValToChar(xCol) )
       cNam := upper( cCol )
       cVal := iif( ISCHAR(xVal), xVal, cValToChar(xVal) )
       cFld := iif( oCol:cName == "ORDKEYNO", "", oCol:cField )
       AAdd(aVal, { hb_ntos(nCol),        ;  // 1
                    cNam,                 ;  // 2
                    .F.,                   ;  // 3 modify column
                    xCol,                 ;  // 4
                    oTsb:GetValue( nCol ), ;  // 5
                    iif( lCol, 1, 2 ),   ;  // 6
                    lCol,                 ;  // 7 lEdit column
                    cFld,                 ;  // 8
                    oCol:cFieldTyp,      ;  // 9
                    oCol:nFieldLen,      ;  // 10
                    oCol:nFieldDec,      ;  // 11
                    oCol                   ;  // 12
                  })
       AAdd(aCol, { nCol,                 ;  // 1
                    cNam,                 ;  // 2
                    .F.,                   ;  // 3
                    cVal,                 ;  // 4
                    lCol,                 ;  // 5
                    cFld,                 ;  // 6
                    oCol:cFieldTyp,      ;  // 7
                    oCol:nFieldLen,      ;  // 8
                    oCol:nFieldDec        ;  // 9
                  })
   NEXT

   //? procname(), cCard
   //? FileCard( cCard, aCol )
   //COPY all TO (cCard)
   //FileCard( cCard )
   //?
   //dbSelectArea( nArea )

   DEFINE WINDOW &cForm TITLE " "+cTitl MDICHILD ;
      ON INIT   ( _wSend(5, oMain), _wSend(5) )                      // Refresh StatusBar items
      This.Maximize
      This.Cargo        := oKeyData()
      This.Cargo:cTitle  := trim(cTitl) + ". "
      This.Cargo:cCapt  := This.Cargo:cTitle + cCapt
      This.Cargo:cForm  := This.Name
      This.Cargo:oParent := oWnd             // window for card list
      This.Cargo:oTsb    := oTsb             // table for card list
      This.Cargo:nTsbRec := nTsbRec          // Card Recno\ID
      This.Cargo:lModify := .F.               // Card modify
      This.Cargo:lEdit  := oWnd:Cargo:lEdit

      oWnd:Cargo:oCardRec:Set( nTsbRec, This.Object )

      i := 24 + oA:W(.4)
      k := 24 + oA:H1 + 2
      DEFINE SPLITBOX HANDLE hSpl
      DEFINE TOOLBAR ToolBar_1 CAPTION " Mode : "+cMode FONT "Normal" BUTTONSIZE i,k  FLAT
         BUTTON btnMode  PICTURE 'Dbf24'    CAPTION " "     ACTION  NIL           SEPARATOR
         BUTTON btnSave  PICTURE 'Save24'  CAPTION "Save"   ACTION  _wPost(1)     SEPARATOR TOOLTIP "Save  F2"
         BUTTON btnFirst PICTURE 'ToFirst24' CAPTION "First"  ACTION  _wPost(97, , 0) SEPARATOR
         BUTTON btnNext  PICTURE 'ToNext24'  CAPTION "Next" ACTION  _wPost(97, , 1) SEPARATOR
         BUTTON btnPrev  PICTURE 'ToPrev24'  CAPTION "Prev" ACTION  _wPost(97, , 2) SEPARATOR
         BUTTON btnLast  PICTURE 'ToLast24'  CAPTION "Last" ACTION  _wPost(97, , 3) SEPARATOR
      END TOOLBAR
      DEFINE TOOLBAR ToolBar_2 CAPTION "" FONT "Normal"      BUTTONSIZE i,k FLAT
         BUTTON btnExit  PICTURE 'Return24' CAPTION "Exit" ACTION _wPost(98) TOOLTIP " "+"   Esc "
      END TOOLBAR
      END SPLITBOX

      x := 2
      i := 1
      y := GetWindowHeight(hSpl) + i
      h := This.ClientHeight - y - i * 2 - 1
      w := This.ClientWidth  - x * 2
      This.Cargo:nSplitHeight := y
      This.btnSave.Enabled  := .F.

      DEFINE TBROWSE oBrw AT y,x WIDTH w HEIGHT h CELL ;
             ALIAS        aVal                         ;
             VALUE        1                            ;
             FONT       oA:Cargo:aFonts              ;
             BRUSH        oA:Cargo:aBrush            ;
             HEADERS    aHead                      ;
             COLSIZES     aSize                       ;
             JUSTIFY    aJust                      ;
             COLNAMES     aName                       ;
             COLORS      oA:Cargo:aColorTsb          ;
             FOOTER      .T.                         ;
             FIXED                                     ;
             LOADFIELDS GOTFOCUSSELECT             ;
             COLNUMBER   { 1, 60 }

             Eval( oA:Cargo:bSetsTsb, oBrw )

             :Cargo:lToolTipCol := oTsb:Cargo:lToolTipCol
             :Cargo:nMaxCharCol := oTsb:Cargo:nMaxCharCol
             :Cargo:nMaxMemoCnt := nMem
             :Cargo:nMaxCharCol := oTsb:Cargo:nMaxCharCol     // Max len char column
             :Cargo:nMaxLineMem := 15                        // Max line for memo edit
             :Cargo:lEdit      := oWnd:Cargo:lEdit
             :Cargo:oEditCol   := Nil                       // Last edit column

             :nHeightCell  := oA:H1 + oA:nGapsHeight
             :nHeightHead  := :nHeightCell
             :nHeightFoot  := :nHeightCell

             IF :Cargo:nMaxMemoCnt > 0 ; :nHeightCell := oA:H2
             ENDIF

             :aBitMaps := { LoadImage("Pen16"), LoadImage("Empty16") }

             oCol := :GetColumn("ORDKEYNO")
             oCol:nClrBack := oCol:nClrHeadBack

             oCol := :GetColumn("col")
             oCol:nClrBack := oCol:nClrHeadBack
             oCol:hFont  := oCol:hFontHead

             oCol := :GetColumn("NAM")
             oCol:nClrBack := oCol:nClrHeadBack
             oCol:hFont  := oCol:hFontHead
             oCol:uBmpCell := {|nc,ob| nc := ob:aArray[ ob:nAt ][6], ob:aBitMaps[ nc ] }

             oCol := :GetColumn("MOD")
             oCol:nClrBack  := oCol:nClrHeadBack
             oCol:nWidth      := :nHeightCell
             oCol:aCheck      := { LoadImage("Mod16"), Nil }
             oCol:nLineStyle  := LINES_HORZ
           //oCol:nHLineStyle := LINES_HORZ  // !!! эти строки дают ошибку прорисовки шапки и подвала:
           //oCol:nFLineStyle := LINES_HORZ  // !!! пропадает горизонтальная линия, примыкающая к строкам

             ADD SUPER HEADER TO oBrw FROM 1 TO 2 TITLE iif( lTsbRec, "Deleted", "" ) HEIGHT :nHeightCell + 2 ;
                       COLORS iif( lTsbRec, CLR_RED, CLR_BLUE ) HORZ DT_CENTER
             ADD SUPER HEADER TO oBrw FROM 3 TO :nColCount() TITLE " "+This.Cargo:cTitle+" "+cFile ;
                       COLORS iif( lTsbRec, CLR_RED, CLR_BLUE ) HORZ DT_LEFT

             AEval( :aColumns, {|oc,nc| iif( nc > 1, oc:cPicture := Nil, ) })  // сбросили Picture колонки
             ATail( :aColumns ):nEditMove := 0

             :nCell      := :nColumn("VAL")
             :nFreeze     := :nCell - 1
             :lLockFreeze := .T.
             :bChange     := {|ob| _wPost(5, ob:cParentWnd) }
             :aCheck    := { StockBmp( 6 ), StockBmp( 7 ) }
             :bTSDrawCell := {|ob,ocel,ocol|
                               If ocel:nDrawType == 0 .and. ocol:cName == "VAL"   // Line
                                  IF ISLOGICAL(ocel:uValue)
                                     ocel:uData := ""
                                     ocel:hBitMap := ob:aCheck[ iif( ocel:uValue, 1, 2 ) ]
                                  ENDIF
                               EndIf
                               Return Nil
                              }

             :UserKeys( VK_F2, {|ob|
                                    SET WINDOW THIS TO ob:cParentWnd
                                    If This.Cargo:lModify
                                       _wSend(1, ob:cParentWnd)
                                    EndIf
                                    SET WINDOW THIS TO
                                    Return .F.
                               } )

             oCol := :GetColumn("VAL")
             oCol:lEdit := .T.

             oCol:bPrevEdit := {|cv,ob,nc,oc|
                                Local xv,ct,nl,nd,ocol
                                Local lRet := oBrwArray(ob, "Edit") // ob:aArray[ob:nAt][7]  // lEdit
                                If ! lRet ; Return lRet            // No edit
                                EndIf
                                SET WINDOW THIS TO ob:cParentWnd
                                ct  := oBrwArray(ob, "Type") // ob:aArray[ob:nAt][ 9]        // Type
                                nl  := oBrwArray(ob, "Len" ) // ob:aArray[ob:nAt][10]        // Len
                                nd  := oBrwArray(ob, "Dec" ) // ob:aArray[ob:nAt][11]        // Dec
                                ocol := oBrwArray(ob, "oCol") // ob:aArray[ob:nAt][12]       // oCol
                                ob:Cargo:oEditCol := ocol              // Last edit column
                                oc:Cargo := oc:nEditWidth
                                If ct == "L"
                                   xv := cv
                                   xv := ! xv
                                   oBrwArray(ob, "Value", xv) //
                                   //ob:aArray[ob:nAt][3] := .T.      // modify column
                                   //ob:aArray[ob:nAt][5] := xv        // new value
                                   //ob:SetValue(oc, xv)
                                   ob:DrawSelect()
                                   This.Cargo:lModify := .T.
                                   This.btnSave.Enabled := .T.
                                   lRet := .F.
                                ElseIf ct == "C"
                                   oc:nEditWidth   := oc:nWidth - 4
                                   oc:cEditPicture := repl("X", nl)
                                ElseIf ct == "D"
                                   oc:nEditWidth   := oc:nWidth - 4
                                   oc:cEditPicture := "@D "
                                ElseIf ct == "N"
                                   oc:nEditWidth   := (App.Object):W2
                                   oc:cEditPicture := repl("9", nl)
                                   If nd > 0
                                      oc:cEditPicture += "."+repl("9", nd)
                                      oc:cEditPicture := right(oc:cEditPicture, nl)
                                   EndIf
                                EndIf
                                If ocol:cFieldTyp $ "CM"
                                   If ocol:cFieldTyp == "M" .or. ocol:nFieldLen > ob:Cargo:nMaxCharCol
                                      lRet := .F.
                                      DO EVENTS
                                      _wPost(96)                   // Edit memo
                                   EndIf
                                EndIf
                                SET WINDOW THIS TO
                                Return lRet
                               }
             oCol:bPostEdit := {|cv,ob|
                                Local nc,oc,xv,ct,nl,nd,lm,ocol
                                SET WINDOW THIS TO ob:cParentWnd
                                nc := ob:nCell
                                oc := ob:aColumns[nc]
                                xv := oBrwArray(ob, "Value2") // ob:aArray[ob:nAt][ 5]         // value real
                                ct := oBrwArray(ob, "Type"  ) // ob:aArray[ob:nAt][ 9]         // Type
                                nl := oBrwArray(ob, "Len"   ) // ob:aArray[ob:nAt][10]         // Len
                                nd := oBrwArray(ob, "Dec"   ) // ob:aArray[ob:nAt][11]         // Dec
                                ocol := oBrwArray(ob, "oCol") // ob:aArray[ob:nAt][12]         // oCol
                                oc:bDecode    := NIL
                                oc:nEditWidth := oc:Cargo
                                oc:lEditBox  := .F.
                                oc:lEditBoxROnly := .F.
                                oc:lEdit := ob:Cargo:lEdit
                                If ct $ "NCD"
                                   If ct == "N" ; cv := Val(Transform( cv, oc:cEditPicture ))
                                   EndIf
                                   oBrwArray(ob, "Value", cv) //
                                //  ob:aArray[ob:nAt][5] := cv
                                //  ob:SetValue(oc, cv)
                                //ElseIf ct $ "CD"
                                //  ob:aArray[ob:nAt][5] := cv
                                //  ob:SetValue(oc, cv)
                                EndIf
                                oc:cEditPicture := Nil
                                lm := oc:xOldEditValue != ob:GetValue(oc)
                                oBrwArray(ob, "Modify", lm) // ob:aArray[ob:nAt][3] := lm // modify column
                                ob:DrawSelect()
                                If lm
                                   This.Cargo:lModify := .T.
                                   This.btnSave.Enabled := .T.
                                EndIf
                                ob:Cargo:oEditCol := Nil               // Last edit column
                                SET WINDOW THIS TO
                                Return Nil
                               }
             IF :Cargo:lToolTipCol
                :lRowPosAtRec := .T.
                :ToolTipSet(7, 1024)                            // 7 sek., 1024 buffer
                :cToolTip := {|ob,x,y|
                               Local cRet := "", xVal, nOld, nNew
                               If ! ISNUMERIC(y) .or. ! ISNUMERIC(x) .or. ob:nLen == 0 ; Return cRet
                               ElseIf ! ob:nCell == x ; Return cRet
                               EndIf
                               If y > 0 // .and. x == ob:nCell .and. y == ob:nRowPos
                                  nNew := ob:aRowPosAtRec[ y ]
                                  If nNew > 0
                                     nOld := ob:nAt
                                     ob:nAt := nNew
                                     xVal := ob:GetValue( x )
                                     ob:nAt := nOld
                                     If ISCHAR(xVal)
                                        cRet := Trim(xVal)
                                        If ! CRLF $ cRet .and. Len(cRet) < ( ob:Cargo:nMaxCharCol + 10 )
                                           cRet := ""
                                        EndIf
                                     EndIf
                                  EndIf
                               EndIf
                               Return cRet
                             }
             ENDIF

             IF :nLen > :nRowCount()
                :ResetVScroll( .T. )
                :oHScroll:SetRange( 0, 0 )
             ENDIF

             :AdjColumns("VAL")

      END TBROWSE ON END {|ob| ob:SetNoHoles(), ob:SetFocus() }

      oBrw:aEditCellAdjust[ 1 ] := int( ( oBrw:nHeightCell - oA:H1 ) / 2 )   // Row
      oBrw:aEditCellAdjust[ 2 ] := 2                                           // Col
      oBrw:aEditCellAdjust[ 3 ] := -6                                         // Width
      oBrw:aEditCellAdjust[ 4 ] := oA:H1 - oBrw:nHeightCell                  // Height

      IF oTsb:nCell > oBrw:nRowCount()
         WHILE oBrw:nAt < oTsb:nCell ; oBrw:GoNext()
         END
      ELSE
         oBrw:GoPos( oTsb:nCell, oBrw:nCell )
      ENDIF

     (This.Object):Event( 1, {||                                             // Save
                                Local ob := This.oBrw.Object, ni, cn, xv, nn
                                Local ot := This.Cargo:oTsb
                                Local ca := ot:cAlias                // alias for table card list
                                Local no := (ca)->( RecNo() )         // current recno
                                Local nc := This.Cargo:nTsbRec       // Card recno
                                If This.Cargo:lModify
                                   If no != nc ; (ca)->( dbGoto(nc) )
                                   EndIf
                                   If (ca)->( RLock() )
                                      For ni := 1 To Len(ob:aArray)
                                          cn := ob:aArray[ ni ][2]            // cName
                                          xv := Nil
                                          nn := 0
                                          If ob:aArray[ ni ][3]               // modify column
                                             xv := ob:aArray[ ni ][4]         // xValue
                                             nn := (ca)->( FieldPos(cn) )
                                             If nn > 0 ; (ca)->( FieldPut(nn, xv) )
                                             EndIf
                                          EndIf
                                      Next
                                      (ca)->( dbCommit() )
                                      (ca)->( dbUnLock() )
                                   EndIf
                                   (ca)->( dbGoto(no) )
                                   This.Cargo:lModify := .F.
                                EndIf
                                DO EVENTS
                                _wPost(98)
                                Return Nil
                             } )
     (This.Object):Event( 2, {|ow,ky,cv|                                     // Modify value from memo
                                Local ob := This.oBrw.Object
                                ky := ow
                                oBrwArray(ob, "Value", cv)
                                This.Cargo:lModify := .T.
                                This.btnSave.Enabled := .T.
                                ob:SetFocus()
                                ob:DrawSelect()
                                _wSend(5, oMain)
                                Return Nil
                             } )
     (This.Object):Event( 5, {||                                            // Refresh StatusBar items
                                 Local nIrec := oMain:Cargo:nStatusItemRecNo
                                 Local nIcol := oMain:Cargo:nStatusItemColNo
                                 Local oBrw  := This.oBrw.Object, cOut
                                 cOut := iif( lTsbRec, "*", "" )+"RecNo: "+hb_ntos(nTsbRec)+"/"
                                 cOut += hb_ntos((oTsb:cAlias)->( LastRec() ))+" "
                                 cOut += iif( lTsbRec, "Deleted", "" )
                                 oMain:StatusBar:Say(cOut, nIrec)
                                 cOut := "Column: "+hb_ntos(oBrw:nAt)+"/"+hb_ntos(oBrw:nLen)
                                 oMain:StatusBar:Say(cOut, nIcol)
                                 Return Nil
                             } )
     (This.Object):Event(96, {|| MdiChildEditBox() } )                  // Edit memo
     (This.Object):Event(97, {|ow,ky,ngo|                             // Close window and Go new card
                               _wSend(98)
                               ky := ow
                               DO EVENTS
                               _wPost(28, oWnd, ngo)
                               Return Nil
                             } )
     (This.Object):Event(98, {|ow|                                     // Close window
                                Local oBrw  := This.oBrw.Object
                                Local oTsb, nRec, oCard
                                IF oBrw:IsEdit
                                   oBrw:PostMsg( WM_KEYDOWN, VK_ESCAPE )     // TsBrowse edit end
                                   oBrw:SetFocus()
                                ELSE
                                   nRec  := ow:Cargo:nTsbRec
                                   oCard := ow:Cargo:oParent
                                   oCard:Cargo:oCardRec:Del( nRec )
                                   _CloseActiveMdi()
                                   ActivateMdiChildWindow( oCard:Name )
                                   oTsb := oCard:GetObj("oBrw")
                                   IF ISOBJECT( oTsb ) ; oTsb:Tsb:SetFocus()
                                   ENDIF
                                   _wSend(5, oMain)                           // Refresh StatusBar items
                                ENDIF
                                DO EVENTS
                                Return Nil
                             } )
      (This.Object):Event(99, {|ow| ow:Release })

      ON KEY ESCAPE  ACTION _wSend(98)
      ON KEY CONTROL+W ACTION {||
                               Local oBrw := This.oBrw.Object
                               If oBrw:IsEdit                  // tsb field edit
                                  oBrw:aColumns[ oBrw:nCell ]:oEdit:Save()
                                  oBrw:SetFocus()
                               Else                          // Window selected
                                  _wPost(7, oMain)
                               EndIf
                               Return Nil
                              }
   END WINDOW

RETURN Nil

*-----------------------------------------------------------------------------*
STATIC FUNCTION oBrwArray( oBrw, xElm, xVal )
*-----------------------------------------------------------------------------*
   LOCAL xRet, nElm, oCol
   LOCAL aElm := {"Nr","Name","Modify","Value","Value2","Image","Edit","Field","Type","Len","Dec","oCol"}

   IF ISCHAR( xElm ) ; nElm := ASCAN( aElm, xElm )
   ELSE             ; nElm := xElm
   ENDIF

   IF xElm == "Name" .or. xElm == "Field"
      oCol := Atail( oBrw:aArray[ oBrw:nAt ] )
      xRet := iif( xElm == "Name", oCol:cName, oCol:cField )
   ELSE
      xRet := oBrw:aArray[ oBrw:nAt ][ nElm ]
   ENDIF

   IF pCount() > 2
      oBrw:aArray[ oBrw:nAt ][ nElm ] := xVal
      IF xElm == "Value2" .or. xElm == "Value"
         nElm += iif( xElm == "Value", 1, -1 )
         oBrw:aArray[ oBrw:nAt ][ nElm ] := xVal
         nElm := ASCAN( aElm, "Modify" )
         oBrw:aArray[ oBrw:nAt ][ nElm ] := .T.
      ENDIF
   ENDIF

RETURN xRet

*-----------------------------------------------------------------------------*
STATIC FUNCTION MdiChildOpen()
*-----------------------------------------------------------------------------*
   LOCAL cForm := "wChild", hSpl, cAls, oCar, cMode, cCapt, oBrw
   LOCAL oWnd  := ThisWindow.Object
   LOCAL cFile := oMain:Cargo:cFile
   LOCAL cRdd  := oMain:Cargo:cRdd
   LOCAL cCdp  := oMain:Cargo:cLang
   LOCAL lShared := oMain:Cargo:lShared
   LOCAL cShared := oMain:Cargo:cShared
   LOCAL cEdit := oMain:Cargo:cEdit
   LOCAL nCapt := 64        // max len FileName
   LOCAL i, k, y, x, w, h, o
   LOCAL nSel := Select()
   LOCAL lUse := .F.
   LOCAL nMsg := 2
   LOCAL c9 := chr(9)
   LOCAL oA := App.Object

   cMode := " [ "+cRdd+" , "+cCdp+" , "+cShared+" , "+"Edit - "+cEdit+" ]"
   cCapt := iif( Len(cFile) > nCapt, left(cFile, 3) + "..." + right(cFile, nCapt), cFile ) + cMode

   SELECT 0
   cAls  := "_A_"+hb_ntos(Select())+"_"

   BEGIN SEQUENCE WITH { |e|break(e) }
      DbUseArea( .F., cRdd, cFile, cAls, lShared, , cCdp )
      lUse := ! NetErr() .and. Used()
      nMsg := 0
   END SEQUENCE

   DEFINE WINDOW &cForm TITLE "" MDICHILD ;
      ON INIT   ( _wSend(1), DoEvents(), _wPost(nMsg) ) ;
      ON RELEASE  {|oc| oc := This.Cargo, ;
                        iif( oc:lUse, (oc:cAls)->( dbCloseArea() ), ), Select(oc:nOld) }
      This.Maximize
      This.Cargo := oKeyData()
      This.Cargo:cParent := oWnd:Name
      This.Cargo:cForm  := This.Name
      This.Cargo:cFile  := cFile
      This.Cargo:cRdd    := cRdd
      This.Cargo:cCdp    := cCdp
      This.Cargo:cAls    := cAls
      This.Cargo:nOld    := nSel
      This.Cargo:lUse    := lUse
      This.Cargo:cCapt  := cCapt
      This.Cargo:cMode  := cMode
      This.Cargo:lEdit  := oMain:Cargo:lEdit
      This.Cargo:lShared := oMain:Cargo:lShared
      This.Cargo:cFltr  := ""             // _CheckMenuItem
      This.Cargo:cFltu  := ""             // _UnCheckMenuItem
      This.Cargo:aStru  := iif( lUse, dbStruct(), {} )
      This.Cargo:FocusedControl := ""
      This.Cargo:lCardEnter := oMain:Cargo:lCardEnter   // Enter or DblClick -> Card view
      This.Cargo:oCardRec := oKeyData()
      oCar := This.Cargo

      oCar:aTB := {{ 'Dbf24'  , ""                                      }, ;  // 1 'Empty16'
                   { 'Plus24'  , " Add new record "         +"  F2, Ins " }, ;  // 2
                   { 'Minus24' , " Delete \ Restore record "+"  F3, Del " }, ;  // 3
                   { 'Tabl24'  , " View selected record "   +"  F4"      }, ;  // 4
                   { 'Refr24'  , " Refresh table "        +"  F5 "      }, ;  // 5
                   { 'Seek24'  , " Filter table  "        +"  F6 "      }, ;  // 6
                   { 'Search24', " Search by entry "       +"  F7 "     }, ;  // 7
                   { 'Sum24'  , " Amounts "               +"  F8 "      }, ;  // 8
                   { 'Excl24'  , " Export to Excel "       +"  F9 "     }, ;  // 9
                   { 'EXIT'    , " Close table "            +"  Esc "     }  ;  // 10
                  }

      i := 24 + oA:W(.45)
      k := 24 + oA:H1 + 2
      DEFINE SPLITBOX HANDLE hSpl
      DEFINE TOOLBAR ToolBar_1 CAPTION " Mode : "+cMode FONT "Normal" BUTTONSIZE i,k FLAT
         BUTTON btnMode  PICTURE oCar:aTB[1][1] CAPTION " "     ACTION  NIL          SEPARATOR DROPDOWN
            DEFINE DROPDOWN MENU BUTTON btnMode
               SEPARATOR
               ITEM 'SERVIS :'                    ACTION Nil        DISABLED  DEFAULT  IMAGE 'p_prop16'
               SEPARATOR
               ITEM 'Table structure'            ACTION Nil               NAME _101 IMAGE 'n01'
               ITEM 'Copy in table structure ...' ACTION Nil                 NAME _102 IMAGE 'n02'
               ITEM 'Copy in table columns ...' ACTION Nil                NAME _103 IMAGE 'n03'
               ITEM 'Append from ...'            ACTION Nil               NAME _104 IMAGE 'n04'
               ITEM 'Delete all records ...'    ACTION Nil                NAME _105 IMAGE 'n05'
               ITEM 'Restore all records ...'     ACTION Nil                 NAME _106 IMAGE 'n06'
               ITEM 'Replace all records ...'     ACTION Nil                 NAME _107 IMAGE 'n07'
            END MENU
            IF ThisWindow.Cargo:lEdit                                    // Card
         BUTTON btnAdd  PICTURE oCar:aTB[2][1] CAPTION "Add"     ACTION _wPost(22)   SEPARATOR TOOLTIP oCar:aTB[2][2]
         BUTTON btnDel  PICTURE oCar:aTB[3][1] CAPTION "Delete"  ACTION _wPost(23)   SEPARATOR TOOLTIP oCar:aTB[3][2]
            ENDIF
         BUTTON btnTabl  PICTURE oCar:aTB[4][1] CAPTION "Card"  ACTION _wPost(24)    SEPARATOR TOOLTIP oCar:aTB[4][2]

         BUTTON btnRefr  PICTURE oCar:aTB[5][1] CAPTION "Refresh" ACTION _wPost(25)  SEPARATOR TOOLTIP oCar:aTB[5][2]
         BUTTON btnFltr  PICTURE oCar:aTB[6][1] CAPTION "Filter"  ACTION _wPost(3, ,0) SEPARATOR TOOLTIP oCar:aTB[6][2] DROPDOWN
            DEFINE DROPDOWN MENU BUTTON btnFltr
               SEPARATOR
               ITEM 'FILTER :'                  ACTION Nil       DISABLED  DEFAULT  IMAGE 'Fltr16'
               SEPARATOR
               ITEM 'All records'+c9+'F6'     ACTION _wPost(3, ,{0, This.Name}) NAME _601 IMAGE 'n01'
               ITEM 'Not deleted records '      ACTION _wPost(3, ,{1, This.Name}) NAME _602 IMAGE 'n02'
               ITEM 'Deleted records '        ACTION _wPost(3, ,{2, This.Name}) NAME _603 IMAGE 'n03'
            END MENU
         BUTTON btnFind  PICTURE oCar:aTB[7][1] CAPTION "Find" ACTION _wPost(27)  SEPARATOR TOOLTIP oCar:aTB[7][2]
         BUTTON btnSum  PICTURE oCar:aTB[8][1] CAPTION "Sums" ACTION _wPost(4, ,1) SEPARATOR TOOLTIP oCar:aTB[8][2] DROPDOWN
            DEFINE DROPDOWN MENU BUTTON btnSum
               SEPARATOR
               ITEM 'AMOUNTS :'                ACTION Nil     DISABLED  DEFAULT  IMAGE 'Itog16'
               SEPARATOR
               ITEM 'Calculate amounts'+c9+'F8' ACTION _wPost(4, ,1) IMAGE 'n01'
               ITEM 'Reset amounts '            ACTION _wPost(4, ,0) IMAGE 'n02'
            END MENU
         BUTTON btnExcl  PICTURE oCar:aTB[9][1] CAPTION "Excel" ACTION _wPost(29)    SEPARATOR TOOLTIP oCar:aTB[9][2]

      END TOOLBAR

      DEFINE TOOLBAR ToolBar_2 CAPTION "" FONT "Normal"      BUTTONSIZE i,k FLAT
         BUTTON btnExit  PICTURE oCar:aTB[10,1] CAPTION "Exit" ACTION _wPost(98)             TOOLTIP oCar:aTB[10,2]
      END TOOLBAR
      END SPLITBOX

      This.Cargo:nSplitHeight := 0
      IF This.Cargo:lShared
         This._104.Enabled := .F.
         This._105.Enabled := .F.
         This._106.Enabled := .F.
         This._107.Enabled := .F.
      ENDIF

      IF lUse
         x := 2
         i := 1
         y := GetWindowHeight(hSpl) + i
         h := This.ClientHeight - y - i * 2 - 1
         w := This.ClientWidth  - x * 2
         This.Cargo:nSplitHeight := y

         DEFINE TBROWSE oBrw AT y,x WIDTH w HEIGHT h CELL          ;
                ALIAS        ALIAS()                               ;
                FONT       oA:Cargo:aFonts                       ;
                BRUSH        oA:Cargo:aBrush                     ;
                COLORS      oA:Cargo:aColorTsb                   ;
                ON GOTFOCUS  oCar:FocusedControl := "oBrw"        ;
                FOOTER      .T.                                  ;
                FIXED        COLSEMPTY  COLSEDIT oMain:Cargo:lEdit ;
                LOADFIELDS GOTFOCUSSELECT                      ;
                COLNUMBER   { 1, 60 }                            ;
                ENUMERATOR LOCK

                Eval( oA:Cargo:bSetsTsb, oBrw )

                :Cargo:nRecnoDraw  := 0
                :Cargo:nColnoDraw  := 0
                :Cargo:nClrDeleted := RGB(255, 160, 160)
                :Cargo:lToolTipCol := .F.
                :Cargo:nMaxMemoCnt := 0              // Max count field memo
                :Cargo:nMaxCharCol := 50            // Max len char column
                :Cargo:nMaxLineMem := 10            // Max line for memo edit
                :Cargo:lEdit  := oMain:Cargo:lEdit

                :nHeightCell  := (App.Object):H1 + 3
                :nHeightHead  := :nHeightCell
                :nHeightFoot  := :nHeightCell

                :SetDeleteMode( .T., .F., {|rec,obr| DelRecords(rec, obr) }, ;
                                          {|obr| obr:Cargo:nRecnoDraw := 0, obr:DrawSelect() } )

                FOR EACH o IN :aColumns
                    IF o:cName == "ORDKEYNO"
                       o:nClrBack := {|nr,nc,ob| iif( (ob:cAlias)->( Deleted() ), ob:Cargo:nClrDeleted, CLR_WHITE ) }
                       o:hFont    := o:hFontHead
                       If (:cAlias)->( LastRec() ) > 99999 ; o:nWidth  += GetFontWidth("Bold", 2)
                       EndIf
                    ELSE
                       IF :Cargo:lEdit .and. o:cFieldTyp $ "=@T^+"
                          o:bPrevEdit := {|xv,ob,nc|
                                           If nc == ob:nColCount()
                                           ElseIf ATail(ob:aDrawCols) == nc
                                              ob:GoRight()
                                           Else
                                              ob:nCell := nc + 1
                                              ob:DrawSelect()
                                           EndIf
                                           Return .F.
                                         }
                       ENDIF
                       IF    o:cFieldTyp == "D"
                          o:cPicture := "@D"
                       ELSEIF o:cFieldTyp == "N" .and. o:nFieldLen < 10
                          o:nWidth += GetFontWidth("Bold", 2)
                       ELSEIF o:cFieldTyp $ "CM"
                          IF o:cFieldTyp == "M" .or. o:nFieldLen > :Cargo:nMaxCharCol
                             o:lEditBox := .T.
                             IF o:cFieldTyp == "M"
                                :nMemoHE := :Cargo:nMaxLineMem
                                :Cargo:nMaxMemoCnt += 1
                             ELSE
                                o:nEditBoxWrap := :Cargo:nMaxCharCol
                             ENDIF
                             o:nWidth := o:ToWidth( :Cargo:nMaxCharCol )
                             :Cargo:lToolTipCol := .T.
                             IF ! :Cargo:lEdit
                                o:lEditBoxROnly := .T.
                                o:lEdit := .T.
                             ENDIF
                          ELSE
                             o:nWidth += GetFontWidth("Normal", 1)
                          ENDIF
                       ENDIF
                    ENDIF
                NEXT

                ATail( :aColumns ):nEditMove := 0

                IF :Cargo:nMaxMemoCnt > 0 ; :nHeightCell  := (App.Object):H2
                ENDIF

                ADD SUPER HEADER TO oBrw FROM 1 TO 1 TITLE "" HEIGHT :nHeightCell + 2
                ADD SUPER HEADER TO oBrw FROM 2 TO :nColCount() TITLE " "+cFile COLORS CLR_BLUE HORZ DT_LEFT

                IF :nLen > 0
                   :nRowPos := 1
                   :nCell := :nFreeze + 1
                   :ResetVScroll( .T. )
                   :oHScroll:SetRange( 0, 0 )
                ENDIF

                IF (:GetAllColsWidth() - 1) > ( This.oBrw.ClientWidth )
                   :lAdjColumn  := .T.
                   :lNoHScroll  := .F.
                   :lMoreFields := ( :nColCount() > 30 )
                ELSE
                   :AdjColumns()
                ENDIF

                :UserKeys( VK_INSERT, {|| .F. } )
                :UserKeys( VK_DELETE, {|| .F. } )

                   IF ThisWindow.Cargo:lEdit
                :UserKeys( VK_INSERT, {|ob| _wSend(22, ob:cParentWnd ), .F. } )
                :UserKeys( VK_F2  , {|ob| _wSend(22, ob:cParentWnd   ), .F. } )
                :UserKeys( VK_F3  , {|ob| _wSend(23, ob:cParentWnd   ), .F. } )
                   ENDIF
                :UserKeys( VK_F4  , {|ob| _wSend(24, ob:cParentWnd   ), .F. } )  // Card view
                :UserKeys( VK_F5  , {|ob| _wSend(25, ob:cParentWnd   ), .F. } )
                :UserKeys( VK_F6  , {|ob| _wPost( 3, ob:cParentWnd, 0), .F. } )
                :UserKeys( VK_F7  , {|ob| _wPost(27, ob:cParentWnd   ), .F. } )
                :UserKeys( VK_F8  , {|ob| _wPost( 4, ob:cParentWnd, 1), .F. } )

                   IF ! ThisWindow.Cargo:lEdit .or. This.Cargo:lCardEnter         // Card view
                :bLDblClick := {|p1,p2,p3,ob| p1:=p2:=p3:=Nil, ob:PostMsg( WM_KEYDOWN, VK_RETURN, 0 ) }
                :UserKeys( VK_RETURN, {|ob| _wPost(24, ob:cParentWnd), .F. } )
                   ENDIF

                :bOnDraw  := {|ob| _wPost(20, ob:cParentWnd, ob) }  // Say StatusBar items RecNo:... and Column: ...

                IF :Cargo:lToolTipCol                              // Tooltip
                   :lRowPosAtRec := .T.
                   :ToolTipSet(7, 1024)                             // 7 sek., 1024 buffer
                   :cToolTip := {|ob,x,y|
                                  Local cRet := "", xVal, nRec, nNew
                                  If ! ISNUMERIC(y) .or. ! ISNUMERIC(x) .or. ob:nLen == 0 ; Return cRet
                                  EndIf
                                  If y > 0   //.and. x == ob:nCell
                                     nNew := ob:aRowPosAtRec[ y ]
                                     If nNew > 0
                                        nRec := (ob:cAlias)->( RecNo() )
                                        (ob:cAlias)->( dbGoto( nNew ) )
                                        xVal := ob:GetValue( x )
                                        (ob:cAlias)->( dbGoto( nRec ) )
                                        If ISCHAR(xVal)
                                           cRet := Trim(xVal)
                                           If ! CRLF $ cRet .and. Len(cRet) < ob:Cargo:nMaxCharCol
                                              cRet := ""
                                           EndIf
                                        EndIf
                                     EndIf
                                  EndIf
                                  Return cRet
                                }
                ENDIF

                MdiChildOpenEvent( oBrw )

         END TBROWSE ON END {|ob| ob:SetNoHoles(), ob:SetFocus(), ob:Refresh() }

      ENDIF

      y := oBrw:nTop + iif( oBrw:nHeightSuper > oA:H1, int( (oBrw:nHeightSuper - oA:H1)/2 ), 0 ) + 1
      x := This.ClientWidth - oA:W(3.5)

      @ y,x GETBOX SyFind HEIGHT oA:H1 WIDTH oA:W1 VALUE space(4)+"Find :" FONT "Bold" INVISIBLE NOTABSTOP READONLY
        x += This.SyFind.Width
      @ y,x GETBOX MyFind HEIGHT oA:H1 WIDTH oA:W2 VALUE space(25) INVISIBLE NOTABSTOP ;
                   ON GOTFOCUS   oCar:FocusedControl := This.Name ;
                   ON CHANGE    oCar:cMyFindValue := This.Value  ;
                   ON LOSTFOCUS {||                                // Hide GetBox and UnCheck menu items
                                   Local wnd := ThisWindow.Name
                                   Local car := ThisWindow.Cargo
                                   Local fnd := car:cMyFindValue
                                   Local obr := This.oBrw.Object
                                   This.SyFind.Hide
                                   This.Hide
                                   obr:FilterFTS( Alltrim(fnd), .T., , , .T. )
                                   _wSend(6, wnd, iif( Empty(fnd), "", "FIND" )) // UnCheck menu items
                                   Return Nil
                                }
      This.Cargo:cMyFindValue := ""

      ON KEY ESCAPE   ACTION _wSend(98)
      ON KEY CONTROL+W ACTION {||
                               Local oBrw := This.oBrw.Object
                               If oBrw:IsEdit                  // tsb field edit
                                  oBrw:aColumns[ oBrw:nCell ]:oEdit:Save()
                                  oBrw:SetFocus()
                               Else                          // Window selected
                                  _wPost(7, oMain)
                               EndIf
                               Return Nil
                              }
   END WINDOW

RETURN Nil

*-----------------------------------------------------------------------------*
STATIC FUNCTION MdiChildOpenEvent( oBrw )              //
*-----------------------------------------------------------------------------*

      This.Cargo:oBrw := oBrw

      (This.Object):Event( 1, {|| This.Title := " "+hb_ntos(Len(HMG_GetForms("Y")))+" " } )     // Child window title
      (This.Object):Event( 2, {|ow|                                                            // File not used
                                MsgStop("File not used() !"+CRLF+ow:Cargo:cFile, "ERROR")
                                _wSend(99)
                                Return Nil
                              } )
      (This.Object):Event( 3, {|ow,ky,afl| FilterData(ow, ky, afl ) } )                        // Filter
      (This.Object):Event( 4, {|ow,ky,nfl| Amounts(ow, ky, nfl )  } )                           // Amounts
      (This.Object):Event( 5, {|ow,ky,txt|                                                       // WaitThread
                                If !empty(txt)
                                   CursorWait()
                                   WaitThreadCreateIcon( txt, " " )  // запуск со временем если Nil
                                Else
                                   WaitThreadCloseIcon()           // kill the window waiting
                                   CursorArrow()
                                EndIf
                                Return Nil
                              } )
      (This.Object):Event( 6, {|ow,ky,txt|                           // UnCheck menu items                            //
                                Local wnd := ow:Name
                                Local car := ow:Cargo
                                Local obr := This.oBrw.Object
                                This.btnMode.Caption := iif( Empty(txt), " ", txt )
                                If ! Empty(car:cFltu) .and. _IsControlDefined( car:cFltu, wnd )
                                   _UnCheckMenuItem ( car:cFltu, wnd )
                                EndIf
                                If ! Empty(car:cFltr) .and. _IsControlDefined( car:cFltr, wnd )
                                   _UnCheckMenuItem ( car:cFltr, wnd )
                                EndIf
                                car:cFltr := car:cFltu := ky := ""
                                obr:SetFocus()
                                DO EVENTS
                                Return Nil
                              } )
      (This.Object):Event( 20, {|ow,ky,ob| SayItemsRecCol(ow, ky, ob) } ) // Say StatusBar items RecNo:... and Column: ...
      (This.Object):Event( 21, {||                                      // Refresh StatusBar items RecNo:... and Column: ...
                                 Local ob := This.oBrw.Object
                                 ob:Cargo:nRecnoDraw := 0
                                 ob:Cargo:nColnoDraw := 0
                                 ob:DrawSelect()
                                 Return Nil
                               } )
      (This.Object):Event( 22, {||  AddRecords() } )
      (This.Object):Event( 23, {|| (This.oBrw.Object):PostMsg( WM_KEYDOWN, VK_DELETE, 0 )  } ) // Delete  button
      (This.Object):Event( 24, {||  MdiChildCard() } )
      (This.Object):Event( 25, {||                                                            // Refresh button
                                  Local ob := (This.oBrw.Object)
                                  Local nn := nLenRefresh( ob:cAlias )
                                  If nn != ob:nLen ; ob:nLen := nn
                                  EndIf
                                  ob:Refresh()
                                  DoEvents()
                                  _wSend(4, ,0)
                                  Return Nil
                               } )
      (This.Object):Event( 27, {||
                                  Local obr := This.oBrw.Object
                                  Local y, x, h, w
                                  SET WINDOW THIS TO obr
                                  This.SyFind.Show
                                  This.MyFind.Value := space(20)
                                  This.MyFind.Show
                                  This.MyFind.SetFocus
                                  y := This.SyFind.Row
                                  x := This.SyFind.Col
                                  h := This.MyFind.Height
                                  w := This.SyFind.Width + This.MyFind.Width
                                  DrawRR( BLUE, 3, y, x, h, w, , 2 )
                                  SET WINDOW THIS TO
                                  Return Nil
                               } )
      (This.Object):Event( 28, {|ow,ky,ngo|
                                  Local obr := This.oBrw.Object
                                  If     ngo == 0 ; obr:GoTop()
                                  ElseIf ngo == 1 ; obr:GoDown()
                                  ElseIf ngo == 2 ; obr:GoUp()
                                  ElseIf ngo == 3 ; obr:GoBottom()
                                  EndIf
                                  obr:PostMsg( WM_KEYDOWN, VK_F4, 0 )
                                  Return Nil
                               } )
      (This.Object):Event( 98, {|ow|
                                  Local oBrw, oTsb := ow:GetObj("oBrw")
                                  If ISOBJECT(oTsb)
                                     oBrw := oTsb:Tsb
                                     If ow:Cargo:FocusedControl == "MyFind"           // GetBox edit end
                                        oBrw:SetFocus()
                                        DO EVENTS
                                        RETURN Nil
                                     ElseIf oBrw:IsEdit
                                        oBrw:PostMsg( WM_KEYDOWN, VK_ESCAPE )       // TsBrowse edit end
                                        oBrw:SetFocus()
                                        DO EVENTS
                                        RETURN Nil
                                     EndIf                                            // Child release
                                  EndIf
                                  _CloseActiveMdi()
                                  DO EVENTS
                                  If Len(HMG_GetForms("Y")) == 0
                                     _wPost(99, oMain)
                                  EndIf
                                  RETURN Nil
                               } )
      (This.Object):Event( 99, {|ow| ow:Release })

RETURN Nil

*-----------------------------------------------------------------------------*
STATIC FUNCTION nLenRefresh( cAls )                       // Counter new :nLen for TBrowse
*-----------------------------------------------------------------------------*
   LOCAL cTxt := (App.Object):Cargo:cWaitText
   LOCAL nLen := 0
   LOCAL nRec := (cAls)->( RecNo() )

   oMain:StatusBar:Say(cTxt, 1)
   (cAls)->( dbGotop() )
   DO WHILE (cAls)->( !EOF() )
      nLen ++
      (cAls)->( dbSkip() )
   ENDDO
   (cAls)->( dbGoto( nRec ) )
   oMain:StatusBar:Say(" ", 1)

RETURN nLen

*-----------------------------------------------------------------------------*
STATIC FUNCTION SayItemsRecCol( ow, ky, ob )              // Say StatusBar items RecNo:... and Column: ...
*-----------------------------------------------------------------------------*
   Local nCol0 := ob:Cargo:nColnoDraw
   Local nCol1 := ob:nCell
   Local nRec0 := ob:Cargo:nRecnoDraw
   Local nRec1 := (ob:cAlias)->( RecNo() )
   Local cRec1,cRec2,cRec,lDel,cDel,cCol

   If nCol0 != nCol1
      cCol := "Column: "+hb_ntos(nCol1)+"/"+hb_ntos(ob:nColCount())
      ky := oMain:Cargo:nStatusItemColNo
      oMain:StatusBar:Say(cCol, ky)
      ob:Cargo:nColnoDraw := nCol1
   EndIf

   If nRec0 == nRec1 ; Return Nil
   EndIf

   cRec1 := cValToChar(nRec1)
   cRec2 := cValToChar((ob:cAlias)->( LastRec() ))
   lDel  := (ob:cAlias)->( Deleted() )
   cDel  := iif( (ob:cAlias)->( Deleted() ), "Deleted", "" )
   cRec  := iif( lDel, "*", " " )+"RecNo: "
   ow := oMain:Cargo
   ky := ow:nStatusItemRecNo
   oMain:StatusBar:Say(cRec+cRec1+"/"+cRec2+" "+cDel, ky)
   ob:Cargo:nRecnoDraw := nRec1

RETURN Nil

*-----------------------------------------------------------------------------*
STATIC FUNCTION DelRecords( rec, obr )                    // Delete  records
*-----------------------------------------------------------------------------*
   Local txt, del, ret := .F.
   Local flt := (obr:cAlias)->( dbFilter() )

   If ! empty(flt)
      If MsgYesNo("The filter is set !"+CRLF+CRLF+'Reset All records', "Confirmation")
         obr:FilterData("")
         obr:nCell := obr:nFreeze + 1
         obr:GotoRec(rec)
         _wSend(4, obr:cParentWnd, 0)
         _wSend(6, obr:cParentWnd, "")
      EndIf
      RETURN ret
   EndIf

   del := (obr:cAlias)->( Deleted() )
   txt := iif( del, "Restore", "Delete" )+" "+"record ?"
   ret := MsgYesNo(txt, "Confirmation")

RETURN ret

*-----------------------------------------------------------------------------*
STATIC FUNCTION AddRecords()                                // Add new records
*-----------------------------------------------------------------------------*
   Local ob := This.oBrw.Object
   Local ls := (ob:cAlias)->( dbInfo( DBI_SHARED ) )
   Local ld := (ob:cAlias)->( Deleted() )
   Local lc := .F., nb, orec
   Local ct := "Add new record ?"+CRLF+CRLF
   Local obr := ob
   Local flt := (obr:cAlias)->( dbFilter() )
   Local rec := (obr:cAlias)->( RecNo() )

   If ! empty(flt)
      If MsgYesNo("The filter is set !"+CRLF+CRLF+'Reset All records', "Confirmation")
         obr:FilterData("")
         obr:nCell := obr:nFreeze + 1
         obr:GotoRec(rec)
         _wSend(4, obr:cParentWnd, 0)
         _wSend(6, obr:cParentWnd, "")
      EndIf
      RETURN Nil
   EndIf

   If ls                                           // Shared
      ct += " to the end of the table"
      nb := iif( MsgYesNo(ct, "Confirmation"), 0, -1 )
   Else                                              // Exclusive
      ct += " Yes - before the current entry"+CRLF
      ct += " No - to the end of the table"
      nb := MsgYesNoCancel(ct, "Confirmation")
   EndIf

   If nb >= 0
      lc := MsgYesNo("Dublicate a current record ?", "Confirmation")
      If lc ; orec := (ob:cAlias)->( oRecGet() )
      EndIf
   EndIf

   If nb == 1
         (ob:cAlias)->( dbInsert() )
          If lc
             (ob:cAlias)->( oRecPut(orec) )
             If ld ; (ob:cAlias)->( dbDelete() )
             EndIf
          EndIf
          ob:nLen := ( ob:cAlias )->( Eval( ob:bLogicLen ) )
          ob:GoToRec(( ob:cAlias )->( RecNo() ))
   ElseIf nb == 0
      (ob:cAlias)->( dbAppend(.T.) )
      If lc
         (ob:cAlias)->( oRecPut(orec) )
         If ld ; (ob:cAlias)->( dbDelete() )
         EndIf
      EndIf
      (ob:cAlias)->( dbCommit() )
      (ob:cAlias)->( dbUnLock() )
       ob:nLen := ( ob:cAlias )->( Eval( ob:bLogicLen ) )
       ob:GoToRec(( ob:cAlias )->( RecNo() ), .T.)
   EndIf

   If nb >= 0
      ob:DrawFooters()
      ob:SetFocus()
      _wSend(4, ,0)                                               // Reset amounts
   EndIf

RETURN Nil

*-----------------------------------------------------------------------------*
STATIC FUNCTION FilterData( ow, ky, afl )                       // Filter
*-----------------------------------------------------------------------------*
   Local obr := This.oBrw.Object, nfl
   Local als := obr:cAlias
   Local cfl := (als)->( dbFilter() )

   If ISNUMERIC( afl )
      nfl := afl         // nomer filtra
      ky  := "_601"      // name menu items
   Else
      nfl := afl[1]      // nomer filtra
      ky  := afl[2]      // name menu items
   EndIf

   This.Cargo:cFltu := This.Cargo:cFltr
   This.Cargo:cFltr := ky
   This.btnMode.Caption := " "

   If ! empty( This.Cargo:cFltu ) ; _UnCheckMenuItem( This.Cargo:cFltu, This.Name )
   EndIf
   If ! empty( This.Cargo:cFltr ) ; _CheckMenuItem  ( This.Cargo:cFltr, This.Name )
   EndIf

   If Empty(nfl) .and. Empty(cfl)
      obr:SetFocus()
      Return Nil
   EndIf

   cfl := ""
   If   nfl == 1 ; cfl := "!Deleted()"
   ElseIf nfl == 2 ; cfl :=  "Deleted()"
   EndIf
   If nfl > 0 ; This.btnMode.Caption := "FILTER"
   EndIf

   obr:FilterData(cfl, , .T.)
   obr:nCell := obr:nFreeze + 1
   _wSend(4, ,0)                                                  // Reset amounts

RETURN Nil

*-----------------------------------------------------------------------------*
STATIC FUNCTION Amounts( ow, ky, nfl )                          // Amounts
*-----------------------------------------------------------------------------*
   Local txt := (App.Object):Cargo:cWaitText
   Local als, xv, oc, asm, old, obr, cnt

   obr := This.oBrw.Object

   If Empty(nfl)                                         // Reset amounts
      For ky := 2 TO Len(obr:aColumns)
          obr:aColumns[ky]:cFooting := ""
      Next
   Else                                                  // Calculate amounts
      _wSend(5, This.Name, "Calculate amounts ... ")     // WaitThreadCreateIcon
      oMain:StatusBar:Say(txt + hb_ntos(1), 1)

      als := obr:cAlias
      old := (als)->( RecNo() )
      asm := Array(Len(obr:aColumns))
      cnt := 0
      aFill(asm, 0)
      (als)->( dbGotop() )
      DO WHILE (als)->( !EOF() )
         cnt++
         If cnt % 10 == 0
            oMain:StatusBar:Say(txt + hb_ntos(cnt), 1)
            DO EVENTS
         EndIf
         For ky := 2 TO Len(obr:aColumns)
             oc := obr:aColumns[ky]
             xv := (als)->( FieldGet(FieldPos(oc:cField)) )
             If Valtype(xv) == "N" ; asm[ky] += xv
             Else              ; asm[ky] := ""
             EndIf
         Next
         (als)->( dbSkip(1) )
      ENDDO
      (als)->( dbGoto(old) )
      oMain:StatusBar:Say(txt + hb_ntos(cnt), 1)
      For ky := 2 TO Len(obr:aColumns)
          xv := iif( Valtype(asm[ky]) == "N", hb_ntos(asm[ky]), "" )
          obr:aColumns[ky]:cFooting := xv
      Next

      wApi_Sleep(500)
      _wSend(5, This.Name, "")                             // WaitThreadCloseIcon
      oMain:StatusBar:Say(" ", 1)
      DO EVENTS

   EndIf

   obr:DrawFooters()
   obr:SetFocus()

RETURN Nil

*-----------------------------------------------------------------------------*
FUNCTION _ShowFormContextMenu( cForm, nRow, nCol, lCenter )
*-----------------------------------------------------------------------------*
   LOCAL xContextMenuParentHandle := 0, hWnd, aRow

   DEFAULT nRow := -1, nCol := -1, lCenter := .F.

   If .Not. _IsWindowDefined(cForm)
      xContextMenuParentHandle := _HMG_xContextMenuParentHandle
   Else
      xContextMenuParentHandle := GetFormHandle(cForm )
   Endif

   If xContextMenuParentHandle == 0
      MsgMiniGuiError("Context Menu is not defined. Program terminated")
   EndIf

   lCenter := lCenter .or. ( nRow == 0 .or. nCol == 0 )
   hWnd  := GetFormHandle(cForm)

   If lCenter
      If nCol == 0
         nCol := int( GetWindowWidth (hWnd) / 2 )
      EndIf
      If nRow == 0
         nRow := int( GetWindowHeight(hWnd) / 2 )
      EndIf
   ElseIf nRow < 0 .or. nCol < 0
      aRow := GetCursorPos()
      nRow := aRow[1]
      nCol := aRow[2]
   EndIf

   TrackPopupMenu ( _HMG_xContextMenuHandle , nCol , nRow , xContextMenuParentHandle )

RETURN Nil

*-----------------------------------------------------------------------------*
STATIC FUNCTION WindowMenu( oWnd, aown, cTitl )
*-----------------------------------------------------------------------------*
   Local ncnt := oWnd:Cargo:nStatusItemCount
   Local cwnd := This.Name, cnew := ""
   Local nmnu := 0, ow, nn
   Local nItm := 0, cn, ci
   Local y, x, otsb, oChild
   DEFAULT cTitl := "SELECTED WINDOW :", aown := HMG_GetForms("Y", .T.)

   IF Empty( aown ) ; RETURN Nil
   ENDIF

   IF ISOBJECT( oChild := do_obj(GetActiveMdiHandle()) )
      IF "wMemo" $ oChild:Name ; RETURN NIl
      ENDIF
   ENDIF

   y := This.ClientHeight - This.StatusBar.Height * ( Len(aown) + 1 ) - (App.Object):GapsHeight
   y := iif( y < 0, 0, y )
   x := (App.Object):W1
   FOR nn := 1 TO nCnt
       x += oWnd:StatusBar:Width( nn )
       IF "Window" $ This.StatusBar.Item( nn ) ; EXIT
       ENDIF
   NEXT

   DEFINE CONTEXT MENU OF &cwnd
       SEPARATOR
       ITEM cTitl  DISABLED DEFAULT IMAGE "View16"
       SEPARATOR
       nn := 0
       FOR EACH ow IN aown
           nn++
           IF nn > 20 ; EXIT
           ENDIF
           ci := iif( ow:Name == cwnd, "* ", "  " ) + ow:Cargo:cCapt
           cn := StrZero(nn, 2)
           ITEM ci ACTION nItm := Val(This.Name) NAME &( "000"+cn ) IMAGE "n"+cn
       NEXT
       SEPARATOR
       ITEM "Exit" ACTION nItm := 0
   END MENU

   _ShowFormContextMenu(cwnd, y, x )

   DEFINE CONTEXT MENU OF &cwnd
   END MENU
   DO EVENTS

   IF nItm > 0
      cnew := aown[ nItm ]:Name
      IF cnew != cwnd
         ActivateMdiChildWindow( cnew )
         ow := _WindowObj(cnew)
         IF ISOBJECT(ow)
            otsb := ow:GetObj("oBrw")
            IF ISOBJECT(otsb) ; otsb:Tsb:SetFocus()
            ENDIF
         ENDIF
         DO EVENTS
      ENDIF
   ENDIF

RETURN nItm

*----------------------------------------------------------------------------*
FUNCTION DrawRR( focus, nPen, t, l, b, r, cWindowName, nCurve )
*----------------------------------------------------------------------------*
   LOCAL aColor

   DEFAULT t := This.Row, l := This.Col, b := This.Height, r := This.Width
   DEFAULT focus := .F., cWindowName := ThisWindow.Name, nCurve := 5
   DEFAULT nPen  := 3

   IF ISARRAY( focus ) ; aColor := focus
   ELSE             ; aColor := iif( focus, { 0, 120, 215 }, { 100, 100, 100 } )
   ENDIF

   DRAW ROUNDRECTANGLE IN WINDOW (cWindowName)  ;
        AT t - 2, l - 2 TO t + b + 2, l + r + 2 ;
        ROUNDWIDTH  nCurve ROUNDHEIGHT nCurve   ;
        PENCOLOR  aColor PENWIDTH   nPen

RETURN Nil

*----------------------------------------------------------------------------*
FUNCTION oRecGet()
*----------------------------------------------------------------------------*
   LOCAL oRec := oKeyData()

   AEval( Array( FCount() ), {| v, n| v:=nil, oRec:Set( trim(FieldName( n )), FieldGet( n ) ) } )

RETURN oRec

*----------------------------------------------------------------------------*
FUNCTION oRecPut( oRec )
*----------------------------------------------------------------------------*
   LOCAL nCnt := 0

   AEval( oRec:GetAll(.F.), {|a,n| n := FieldPos(a[1]), nCnt += n, ;
                              iif( n > 0, FieldPut( n, a[2] ), ) } )
RETURN nCnt > 0
