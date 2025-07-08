/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * (c) 2023 Sergej Kiselev <bilance@bilance.lv>
 *
*/
#define _HMG_OUTLOG

#include "hmg.ch"
#include "TSBrowse.ch"
#include "dbinfo.ch"

////////////////////////////////////////////////////////////////////////////////
// Показ окна с таблицей / Show table window
FUNCTION my_Standard( cForm, nBtn, cTitle, aBClr, nY, nX, nW, nH, cAls )
   LOCAL cDbf, cBrw, oBrw, oTsb := oHmgData()
   LOCAL nGaps  := 10, oCol, aBegin, cVal, nI, hW, cMsg
   DEFAULT cAls := "CUST_"+hb_ntos(nBtn)

   cDbf := App.Cargo:cPathDbf + "Customer"
   ? ProcNL(), cForm, _IsWindowDefined(cForm), cDbf
   IF _IsWindowDefined(cForm)
      hW := GetFormHandle(cForm)
      nI := GetFormNameByHandle( hW, @cVal )
      ? "*Form =", hW, nI, cVal
      hW := GetFocus()
      cVal := ""
      nI := -1
      IF hW <> 0
         nI := GetFormNameByHandle( hW, @cVal )
      ENDIF
      ? "*Focus=", hW, nI, cVal
      To_Focus(cForm)
      hW := GetFocus()
      cVal := ""
      nI := -1
      IF hW <> 0
         nI := GetFormNameByHandle( hW, @cVal )
      ENDIF
      ? "#Focus=", hW, nI, cVal

      IF select(cAls) > 0 ; dbSelectArea(cAls)
      ENDIF
      RETURN Nil
   ENDIF

   DEFAULT nY := 0, nX := 0, nW := 500, nH := 400
   DEFAULT aBClr  := { 93,114,148}
   DEFAULT cTitle := cForm + ". WINDOW STANDARD"
   aBegin := cVal := nI := NIL

   cMsg := "hb_SetCodepage()= " + hb_SetCodepage() + ";"
   cMsg += "hb_CdpSelect()  = " + hb_CdpSelect() + ";"
   cMsg += "hb_LangSelect() = " + hb_LangSelect() + ";"
   cMsg += ";" + ProcNL()
   //AlertInfo(cMsg)
   ? ProcNL(), cMsg

   my_WaitWindow('Create a table ...', 1, 7)

   USE ( cDbf ) ALIAS ( cAls ) NEW SHARED
   ? ProcNL(), cDbf, cAls, Used()

   DEFINE WINDOW &cForm AT nY,nX WIDTH nW HEIGHT nH ;
      TITLE "STANDARD: " + cTitle ;
      WINDOWTYPE STANDARD TOPMOST ;
      NOMAXIMIZE NOSIZE ;
      BACKCOLOR aBClr ;
      ON GOTFOCUS {|| App.Cargo:cFormGotFocus := This.Name } ;
      ON INIT  {|| This.Topmost := .F., DoEvents(), _wPost(0) } ;
      ON RELEASE {|| (cAls)->(dbCloseArea()) }
      This.Cargo := oHmgData()

      nY := nX := 0
      nY += nGaps
      nX += nGaps
      nW := This.ClientWidth  - nX * 2
      nH := This.ClientHeight - nY * 2

      cBrw := cAls
      oTsb:aNumber := { 1, 50 }
      oTsb:aEdit  := .F.
      oTsb:lSpecHd := .T.
      oTsb:aFoot  := .T.

      oBrw := _TBrowse( oTsb, cAls, cBrw, nY, nX, nW, nH )

      oBrw:aBitMaps    := { LoadImage("MG_TSB_RECDEL") }
      oCol             := oBrw:GetColumn("ORDKEYNO")
      oCol:nAlign      := DT_RIGHT
      oCol:nFAlign     := oCol:nAlign
      oCol:nSAlign     := oCol:nAlign
      oCol:cSpcHeading := "."+space( oBrw:nCellMarginLR )
      oCol:uBmpCell    := {|nc,ob| nc := (ob:cAlias)->(Deleted()), ;
                                      iif( nc, ob:aBitMaps[1], Nil ) }

      oBrw:bLDblClick := {|p1,p2,nf,ob| p1 := p2 := nf := Nil, ;
                           ob:PostMsg( WM_KEYDOWN, VK_RETURN, 0 ) }
      oBrw:UserKeys( VK_RETURN, {|ob| _wPost(VK_RETURN, ob:cParentWnd) } )
      oBrw:bGotFocus := {|ob| ob:Refresh(.T., .T.) }
      oBrw:SetFocus()

      This.Cargo:nBrw := nBtn
      This.Cargo:oBrw := oBrw
      This.Cargo:cBrw := cBrw
      This.Cargo:cAls := cAls

      ON KEY F1     ACTION HelpThisWindow()
      ON KEY ESCAPE ACTION iif( oBrw:IsEdit, oBrw:SetFocus(), _wPost(99) )

      (This.Object):Event( 0, {|  | NIL })
    //(This.Object):Event(VK_RETURN, {|ow| my_Child(ow) })
      (This.Object):Event(VK_RETURN, {|ow| my_Modal(ow) })
      (This.Object):Event(99, {|ow| ow:Release() })

   END WINDOW

   IF Empty( nY + nX ) ; CENTER WINDOW &cForm
   ENDIF

   ACTIVATE WINDOW &cForm

RETURN Nil

////////////////////////////////////////////////////////////////////////////////
FUNCTION my_Modal( oWnd, nNnn )
   LOCAL nBrw, oBrw, cBrw, cAls, cWnd
   LOCAL cMsg := "", cForm, cTitle, cNnn, nK1
   LOCAL cDbf := "Customer", oTsb := oHmgData()
   LOCAL nGaps := 10, nY, nX, nW, nH, cSupHd
   LOCAL o := oWnd:Cargo, y, x, w, h
   LOCAL cTypeWnd := oWnd:Type
   LOCAL oac := App.Cargo
   DEFAULT nNnn := 0

   IF nNnn > 0
      cNnn := "_"+hb_ntos(nNnn)
      nK1 := 0.92
      nK1 := 0.92
   ELSE
      cNnn := ""
      nK1 := 0.72
      nK1 := 0.72
   ENDIF

   cDbf  := oac:cPathDbf + cDbf
   cWnd  := oWnd:Name
   nBrw  := o:nBrw
   cBrw  := o:cBrw + "_" + hb_ntos(nBrw) + cNnn
   cAls  := o:cAls + "_" + hb_ntos(nBrw) + cNnn
   cForm :=  cWnd  + "_" + hb_ntos(nBrw) + cNnn
   nBrw  += nNnn
   cTitle := oWnd:Title + " Alias: "+cAls

   y := x := 0
   w := Sys.DesktopWidth  * nK1
   h := Sys.DesktopHeight * nK1

   USE ( cDbf ) ALIAS ( cAls ) NEW SHARED
   ? ProcNL(), cDbf, cAls, Used()
   cSupHd := Alias()+": "+dbInfo(DBI_FULLPATH)

   DEFINE WINDOW &cForm AT y,x WIDTH w HEIGHT h TITLE cTitle ;
      MODAL NOSIZE ;
      BACKCOLOR oWnd:BackColor ;
      ON GOTFOCUS {|| App.Cargo:cFormGotFocus := This.Name } ;
      ON INIT  {|| DoEvents(), _wPost(0) } ;
      ON RELEASE {|| (cAls)->(dbCloseArea()) }
      This.Cargo := oHmgData()

      nW := int( This.ClientWidth * 0.7 )
      nH := "Enter - Select, F2,Ins - Append, F3,Del - Delete\Restore, F4 - Edit"

      DEFINE STATUSBAR
         STATUSITEM ""
         STATUSITEM nH WIDTH nW FONTCOLOR BLUE
      END STATUSBAR

      nY := nX := 0
      nY += nGaps
      nX += nGaps
      nW := This.ClientWidth  - nX * 2
      nH := This.ClientHeight - nY * 2 - This.StatusBar.Height

      cBrw := cAls
      oTsb:aNumber  := { 1, 70 }
      oTsb:aEdit  := .T.
      oTsb:nFireKey := VK_F4
      oTsb:lSpecHd  := .T.
      oTsb:aFoot  := .T.
      oTsb:cSupHd := cSupHd
      oTsb:nSupHdBack := CLR_CYAN
      oTsb:nSupHdFore := CLR_YELLOW
      oTsb:bInit := {|ob,op|
           Local oc
           ob:nHeightSpecHd := GetFontHeight(op:aFont[4])
           ob:nHeightCell  += 4
           ob:nHeightHead  += 5
           ob:nHeightFoot  := ob:nHeightHead
           ob:aBitMaps := { LoadImage("MG_TSB_RECDEL") }
           oc := ob:GetColumn("ORDKEYNO")
           oc:hFont := oc:hFontHead
           oc:uBmpCell := {|nc,ob| nc := (ob:cAlias)->(Deleted()), ;
                                   iif( nc, ob:aBitMaps[1], Nil ) }
           ob:SetDeleteMode( .T., .T.)
           ob:bPostDel := {|ob| ob:DrawSelect() }
           ATail(ob:aColumns):nEditMove := 0
           Return Nil
           }
      oTsb:bBody := {|ob,op|
           Local n := ob:nColCount()
           Local nFore := op:nSupHdFore
           Local nBack := op:nSupHdBack
           ADD SUPER HEADER TO ob FROM 1 TO 1 COLOR nFore, nBack ;
                     TITLE " " HORZ DT_CENTER
           ADD SUPER HEADER TO ob FROM 2 TO n COLOR nFore, nBack ;
                     TITLE " " + op:cSupHd HORZ DT_CENTER
           ob:nHeightSuper := 28
           Return Nil
           }

      oBrw := _TBrowse( oTsb, cAls, cBrw, nY, nX, nW, nH )

      oBrw:bLDblClick := {|p1,p2,nf,ob| p1 := p2 := nf := Nil, ;
                           ob:PostMsg( WM_KEYDOWN, VK_RETURN, 0 ) }
      oBrw:UserKeys( VK_RETURN, {|ob| _wPost(VK_RETURN, ob:cParentWnd) } )
      oBrw:UserKeys( VK_F3  , {|ob| ob:PostMsg( WM_KEYDOWN, VK_DELETE, 0 ) } )
      oBrw:UserKeys( VK_INSERT, {|ob| _wPost(VK_INSERT, ob:cParentWnd, ob) } )
      oBrw:UserKeys( VK_F2  , {|ob| ob:PostMsg( WM_KEYDOWN, VK_INSERT, 0 ) } )

      oBrw:SetFocus()
      oBrw:bGotFocus := {|ob| ob:Refresh(.T., .T.) }

      This.Cargo:nBrw := nBrw
      This.Cargo:oBrw := oBrw
      This.Cargo:cBrw := cBrw
      This.Cargo:cAls := cAls

      ON KEY F1     ACTION HelpThisWindow()
      ON KEY ESCAPE ACTION iif( oBrw:IsEdit, oBrw:SetFocus(), _wPost(99) )

      (This.Object):Event( 0, {|  | NIL })
      (This.Object):Event(VK_INSERT, {|ow,ky,ob|
                    ? ProcNL(), ow:Name,ky,ob:cAlias
                    ky := AlertYesNo("Добавить запись ?")
                    IF ky
                       (ob:cAlias)->(dbAppend(.F.))
                       ob:nCell := ob:nFreeze + 1
                       ob:Reset(.T.)
                       DO EVENTS
                       ob:PostMsg( WM_KEYDOWN, VK_F4, 0 )
                    ENDIF
                    Return NIL
                    })
      IF nNnn > 0
         (This.Object):Event(VK_RETURN, {|ow| my_AlertInfo(ow) })
      ELSE
         (This.Object):Event(VK_RETURN, {|ow| my_Modal(ow, 2) })
      ENDIF
      (This.Object):Event(99, {|ow| ow:Release() })

   END WINDOW

     CENTER WINDOW &cForm
   ACTIVATE WINDOW &cForm
   IF cTypeWnd == "S"
      To_Focus(cWnd, cBrw)
   ENDIF

RETURN Nil

////////////////////////////////////////////////////////////////////////////////
FUNCTION my_Child( oWnd, nNnn )
   LOCAL nBrw, oBrw, cBrw, cAls, cWnd
   LOCAL cMsg := "", cForm, cTitle, cNnn, nK1
   LOCAL cDbf := "Customer", oTsb := oHmgData()
   LOCAL nGaps := 10, nY, nX, nW, nH, cSupHd
   LOCAL o := oWnd:Cargo, y, x, w, h
   LOCAL nHandle := _HMG_MainHandle
   LOCAL oac := App.Cargo
   DEFAULT nNnn := 0

   _HMG_MainHandle := oWnd:Handle

   IF nNnn > 0
      cNnn := "_"+hb_ntos(nNnn)
      nK1 := 0.92
      nK1 := 0.92
   ELSE
      cNnn := ""
      nK1 := 0.72
      nK1 := 0.72
   ENDIF

   cDbf  := oac:cPathDbf + cDbf
   cWnd  := oWnd:Name
   nBrw  := o:nBrw
   cBrw  := o:cBrw + "_" + hb_ntos(nBrw) + cNnn
   cAls  := o:cAls + "_" + hb_ntos(nBrw) + cNnn
   cForm :=  cWnd  + "_" + hb_ntos(nBrw) + cNnn
   nBrw  += nNnn
   cTitle := oWnd:Title + " Alias: "+cAls

   y := x := 0
   w := Sys.DesktopWidth  * nK1
   h := Sys.DesktopHeight * nK1

   USE ( cDbf ) ALIAS ( cAls ) NEW SHARED
   ? ProcNL(), cDbf, cAls, Used()
   cSupHd := Alias()+": "+dbInfo(DBI_FULLPATH)

   DEFINE WINDOW &cForm AT y,x WIDTH w HEIGHT h TITLE cTitle ;
      CHILD TOPMOST NOSIZE ;
      BACKCOLOR oWnd:BackColor ;
      ON GOTFOCUS {|| App.Cargo:cFormGotFocus := This.Name } ;
      ON INIT  {|| This.Topmost := .F., DoEvents(), _wPost(0) } ;
      ON RELEASE {|| (cAls)->(dbCloseArea()) }
      _HMG_MainHandle := nHandle
      This.Cargo := oHmgData()

      nW := int( This.ClientWidth * 0.7 )
      nH := "Enter - Select, F2,Ins - Append, F3,Del - Delete\Restore, F4 - Edit"

      DEFINE STATUSBAR
         STATUSITEM ""
         STATUSITEM nH WIDTH nW FONTCOLOR BLUE
      END STATUSBAR

      nY := nX := 0
      nY += nGaps
      nX += nGaps
      nW := This.ClientWidth  - nX * 2
      nH := This.ClientHeight - nY * 2 - This.StatusBar.Height

      cBrw := cAls
      oTsb:aNumber  := { 1, 70 }
      oTsb:aEdit  := .T.
      oTsb:nFireKey := VK_F4
      oTsb:lSpecHd  := .T.
      oTsb:aFoot  := .T.
      oTsb:cSupHd := cSupHd
      oTsb:nSupHdBack := CLR_CYAN
      oTsb:nSupHdFore := CLR_YELLOW
      oTsb:bInit := {|ob,op|
           Local oc
           ob:nHeightSpecHd := GetFontHeight(op:aFont[4])
           ob:nHeightCell  += 4
           ob:nHeightHead  += 5
           ob:nHeightFoot  := ob:nHeightHead
           ob:aBitMaps := { LoadImage("MG_TSB_RECDEL") }
           oc := ob:GetColumn("ORDKEYNO")
           oc:hFont := oc:hFontHead
           oc:uBmpCell := {|nc,ob| nc := (ob:cAlias)->(Deleted()), ;
                                   iif( nc, ob:aBitMaps[1], Nil ) }
           ob:SetDeleteMode( .T., .T.)
           ob:bPostDel := {|ob| ob:DrawSelect() }
           ATail(ob:aColumns):nEditMove := 0
           Return Nil
           }
      oTsb:bBody := {|ob,op|
           Local n := ob:nColCount()
           Local nFore := op:nSupHdFore
           Local nBack := op:nSupHdBack
           ADD SUPER HEADER TO ob FROM 1 TO 1 COLOR nFore, nBack ;
                     TITLE " " HORZ DT_CENTER
           ADD SUPER HEADER TO ob FROM 2 TO n COLOR nFore, nBack ;
                     TITLE " " + op:cSupHd HORZ DT_CENTER
           ob:nHeightSuper := 28
           Return Nil
           }

      oBrw := _TBrowse( oTsb, cAls, cBrw, nY, nX, nW, nH )

      oBrw:bLDblClick := {|p1,p2,nf,ob| p1 := p2 := nf := Nil, ;
                           ob:PostMsg( WM_KEYDOWN, VK_RETURN, 0 ) }
      oBrw:UserKeys( VK_RETURN, {|ob| _wPost(VK_RETURN, ob:cParentWnd) } )
      oBrw:UserKeys( VK_F3  , {|ob| ob:PostMsg( WM_KEYDOWN, VK_DELETE, 0 ) } )
      oBrw:UserKeys( VK_INSERT, {|ob| _wPost(VK_INSERT, ob:cParentWnd, ob) } )
      oBrw:UserKeys( VK_F2  , {|ob| ob:PostMsg( WM_KEYDOWN, VK_INSERT, 0 ) } )

      oBrw:SetFocus()
      oBrw:bGotFocus := {|ob| ob:Refresh(.T., .T.) }

      This.Cargo:nBrw := nBrw
      This.Cargo:oBrw := oBrw
      This.Cargo:cBrw := cBrw
      This.Cargo:cAls := cAls

      ON KEY F1     ACTION NIL
      ON KEY ESCAPE ACTION iif( oBrw:IsEdit, oBrw:SetFocus(), _wPost(99) )

      (This.Object):Event( 0, {|  | NIL })
      (This.Object):Event(VK_INSERT, {|ow,ky,ob|
                    ? ProcNL(), ow:Name,ky,ob:cAlias
                    ky := AlertYesNo("Добавить запись ?")
                    IF ky
                       (ob:cAlias)->(dbAppend(.F.))
                       ob:nCell := ob:nFreeze + 1
                       ob:Reset(.T.)
                       DO EVENTS
                       ob:PostMsg( WM_KEYDOWN, VK_F4, 0 )
                    ENDIF
                    Return NIL
                    })
      IF nNnn > 0
         (This.Object):Event(VK_RETURN, {|ow| my_AlertInfo(ow) })
      ELSE
         (This.Object):Event(VK_RETURN, {|ow| my_Modal(ow, 2) })
      ENDIF
      (This.Object):Event(99, {|ow| ow:Release() })

   END WINDOW

     CENTER WINDOW &cForm
   ACTIVATE WINDOW &cForm

   //To_Focus(cWnd, cBrw)

RETURN Nil

////////////////////////////////////////////////////////////////////////////////
FUNCTION my_AlertInfo( oWnd )
   LOCAL nBrw, oBrw, cBrw, cAls, cWnd
   LOCAL cMsg := "", cTitle
   LOCAL o := oWnd:Cargo

   cWnd  := oWnd:Name
   nBrw  := o:nBrw
   cBrw  := o:cBrw
   oBrw  := o:oBrw
   cAls  := o:cAls
   cTitle := oWnd:Title

   cMsg := hb_valtoexp({ nBrw, cBrw, cAls, oBrw:cControlName})

   AlertInfo(cTitle+CRLF+cMsg, "INFO - "+cWnd)

   To_Focus(cWnd, cBrw)

RETURN Nil
