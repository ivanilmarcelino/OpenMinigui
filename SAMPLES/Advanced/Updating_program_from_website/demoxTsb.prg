/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2025 Sergej Kiselev https://clipper.borda.ru/?32-sergkis
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Показ таблицы из массива объектом _TBrowse()
 * Displaying a table from an array using the _TBrowse() object
*/
#define  _HMG_OUTLOG
#include "hmg.ch"
#include "tsbrowse.ch"

//////////////////////////////////////////////////////////////////////////////////
// json -> array, xml -> array, csv -> array, ...
FUNCTION Converting2Array(cText,nType,cLink)
   LOCAL aXArr, nI, aDim, hItem, cFile, cTxt, a

   aXArr := {}
   IF nType == 1  // json -> array
      hb_jsonDecode(cText,@aDim)
      FOR nI := 1 TO LEN(aDim)
        hItem := aDim[nI]
        cFile := hItem["filename"]
        cTxt  := hItem["description"]
        cTxt  := SUBSTR(cTxt, AT(";", cTxt) + 1 )
        cTxt  := ALLTRIM(cTxt)
        a     := ARRAY(6)
        a[1]  := 0
        a[2]  := cTxt
        a[3]  := hItem["size"]
        a[4]  := hItem["downloads"]
        a[5]  := cLink + cFile
        IF     "7z"  $ cFile ; a[1] := 1
        ELSEIF "pdf" $ cFile ; a[1] := 2
        ELSEIF "doc" $ cFile ; a[1] := 3
        ELSEIF "zip" $ cFile ; a[1] := 4
        ELSEIF "rar" $ cFile ; a[1] := 5
        ELSE                 ; a[1] := 6
        ENDIF
        AADD( aXArr, a )
      NEXT
   ELSEIF nType == 2  // xml -> array
   ELSEIF nType == 3  // csv -> array
   ELSE
      MsgDebug("ERROR ! No nType processing=",nType)
   ENDIF

RETURN aXArr

//////////////////////////////////////////////////////////////////////////////////
FUNCTION ShowTsb(oWnd, cPath, cUrl, cText)
   LOCAL cForm, aBClr, cVal, cTmpFile, nY, nX, nG, oBrw, nWTsb, nHTsb, oTsb
   LOCAL aDelObj, cLink, aXArr

   cForm    := oWnd:Name
   aBClr    := oWnd:Cargo:aBClr
   cTmpFile := cPath + cFileNoPath(cUrl)
   cLink    := hb_FNameDir(cUrl) + "minigui/"
   // processing a file from a website
   aXArr    := Converting2Array(cText,1,cLink)  // 1 - json -> array

   ? ProcNL(), "aXArr=", aXArr ; ?v aXArr
   IF LEN(aXArr) == 0
      cVal := "ERROR! There is no update file list in file " + cFileNoPath(cTmpFile)
      cVal += " ! WRONG FILE STRUCTURE!"
      SetProperty( cForm, "Lbl_List", "Fontcolor", MAROON )
      SetProperty( cForm, "Lbl_List", "Value", cVal )
      RETURN NIL
   ENDIF

   nX      := nG := oWnd:Cargo:nG
   nY      := oWnd:Cargo:nYEnd
   nHTsb   := oWnd:ClientHeight - nY - nG
   nWTsb   := oWnd:Cargo:nW - nG*2
   aDelObj := {}
   oTsb    := TsbPatam( aXArr, "cTable", cLink )
   oBrw    := _TBrowse( oTsb, aXArr, "cTable", nY, nX, nWTsb, nHTsb )
   This.Cargo:oBrw := oBrw     // we put the object on the window
   AADD( aDelObj, "cTable" )

   oWnd:Cargo:aDelObj := aDelObj

RETURN NIL

//////////////////////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION TsbPatam( aXArr, cBrw, cSuperHd )
   LOCAL aWSize, oTsb, aBClr, nClr1, nClr2, nI, a, nKChar := 4

   oTsb := oHmgData()
   //                     cell      Head      foot     SpecHider  SuperHider   Edit
   oTsb:aFont       := { "Normal", "Normal", "Normal", "Normal" , "Normal", "Normal" }
   oTsb:aNumber     := { 1, GetFontWidth("Normal", nKChar + 1), DT_RIGHT, 6 }
   oTsb:cAls        := "ARRAY"
   oTsb:cBrw        := cBrw
   oTsb:lHide       := .F.
   oTsb:aEdit       := .T.        // edit cells
   oTsb:uSelector   := NIL        // do not put in the Selector table
   oTsb:lFooting    := .T.        // put a footer in the table
   oTsb:aFoot       := .T.        // fill the basement
   oTsb:lNoPicture  := .T.
   oTsb:lSpecHd     := .F.        // DO NOT put column numbers in the table
   oTsb:lSuperHd    := .T.        // put a superheader in the table
   oTsb:cSuperHd    := cSuperHd
   oTsb:nHeightCell := 36         // cell height - 32 icons + 2*2
   oTsb:nHeightHead := 30         // table header height
   oTsb:nHeightFoot := 30         // table footer height
   //
   IF oTsb:lSpecHd  ; oTsb:nHeightSpecHd := GetFontHeight(oTsb:aFont[4])  // height of the numerator
   ENDIF
   //
   IF oTsb:lSuperHd ; oTsb:nHeightSuperHd := INT(oTsb:nHeightCell * 1.1)  // superheader height
   ENDIF
   //
   oTsb:aBmpLoad := {"b1x7z", "b2xPdf", "b3xDoc", "b4xZip", "b5xRar", "b6xAll"}
   oTsb:aMsgLoad := {"7z"   , "Pdf"   , "Doc"   , "Zip"   , "Rar"   , "Other" }  // reserve
   oTsb:aHideCol := {}    //{ 4, 5, 6, 7, 8}   // hide columns, taking into account SELECTOR and ARRAYNO
   //
   // Assigning all table columns
   a := aXArr[1]
   oTsb:aName := {}
   FOR nI := 1 TO LEN(a)
      AADD( oTsb:aName, "COL_" + HB_ValToExp(nI) )
   NEXT
   oTsb:aHead  := {"-o-","Description","File size","Number of;downloads","Download link"}
   aWSize      := CalculatColumnWidths(aXArr,"Normal",oTsb:aHead)  // calculate column widths - show all columns
   oTsb:aSize  := aWSize                                           // let's assign the width of the columns for TSB
   //oTsb:aPict:= {}                                               // format fields if needed
   //
   // colors in the table
   aBClr := { 90,217,217}
   nClr1 := HMG_RGB2n(aBClr)                                   // background color of header + footer
   nClr2 := RGB( 48, 29,26)                                    // gray-black background
   oTsb:aSuperHdColor  := {CLR_YELLOW, { nClr1, nClr2 } }      // superheader text and background color
   oTsb:aBrush         := aBClr                                // background color under the table
   a := {}
   AAdd(a, { CLR_TEXT, CLR_BLACK } )                // 1 , cell text
   // 2 , фона в ячейках таблицы
   //AAdd(a, { CLR_PANE, {|nr,nc,ob,nd| nr := CLR_BLACK, nc := CLR_WHITE, nd := CLR_HGRAY, ;
   //                      iif( (ob:cAlias)->(DELETED()), nr, iif( ob:nAt % 2 == 0, nc, nd ) ) } } )
   AAdd(a, { CLR_HEADF, CLR_WHITE                })  // 3 , table header text
   AAdd(a, { CLR_HEADB, { nClr2, nClr1 }         })  // 4 , table header background
   AAdd(a, { CLR_FOCUSB, {|a,b,c| a := b, If( c:nCell == b, -CLR_HRED, -CLR_BLUE ) } } ) // 6 ,cursor background
   AAdd(a, { CLR_EDITF, CLR_YELLOW               })  // 7 , editable field text
   AAdd(a, { CLR_EDITB, CLR_HRED                 })  // 8 , background of the editable field
   AAdd(a, { CLR_FOOTF, CLR_WHITE                })  // 9 , table footer text
   AAdd(a, { CLR_FOOTB, { nClr1, nClr2 }         })  // 10, table footer background
   AAdd(a, { CLR_SPCF , CLR_YELLOW               })  // 18, specheader text - numerator
   AAdd(a, { CLR_SPCB , { nClr1, nClr2 }         })  // 19, specheader back - numerator
   oTsb:aColorAdd := a
   oTsb:lZebra    := .T.
   //oTsb:aZebra  := { {230,230,230}, SILVER }
   oTsb:aZebra    := { HMG_RGB2n({88,208,208}), HMG_RGB2n({146,244,244}) }
   //
   oTsb:bInit := {|ob,op| // TSB initialization
                   ob:HideColumns( op:aHideCol ,.t.)         // hide columns
                   ? "### oTsb:bInit", ProcNL(), HB_ValToExp(op:aHideCol)
                   ob:nCell  := 3                            // move the cursor
                   Do Events
                   Return Nil
                   }

   oTsb:bBody := {|ob,op| // other TSB settings
                   Local nBClr := GetSysColor( COLOR_BTNFACE )   // system color
                   Local hFont := GetFontParam(op:aFont[4])      // SpecHider
                   Local oc, i := 0, bBmpCell, cImg, hImg, nHImg := 32   // icon height
                   //
                   hFont := ob:aColumns[1]:hFontSpcHd            // 4-special header font - или так
                   // replacement of the first column and special header
                   FOR EACH oc IN ob:aColumns
                      oc:cSpcHeading := NIL
                      IF oc:cName == "ARRAYNO" .OR. oc:cName == "ORDKEYNO"
                         oc:cSpcHeading := "#"
                         oc:nClrBack    := nBClr            // change the background color of the virtual column
                         oc:nClrFore    := CLR_RED          // change the text color of a virtual column
                         oc:hFont       := hFont            // changing the virtual column font
                         //oc:bDecode   := {|cv| Alltrim(cv) }
                         oc:nAlign      := DT_CENTER
                         oc:nFAlign     := DT_CENTER
                      ELSE
                         oc:cSpcHeading := hb_ntos( ++i )
                      ENDIF
                   NEXT
                   // column (1) - change icon
                   ob:Cargo:cField := "COL_1"
                   nI := ob:nColumn("COL_1", .T.)
                   IF nI > 0
                      oc := ob:aColumns[nI]
                      oc:Cargo := oHmgData()            // Let's create a container for the column
                      oc:Cargo:cField := "COL_1"
                      oc:Cargo:aBmp   := op:aBmpLoad    // set above - oTsb:aBmpLoad
                      oc:Cargo:aMsg   := op:aMsgLoad    // set above - oTsb:aMsgLoad
                      oc:nClrBack     := CLR_WHITE
                      oc:lEdit        := .T.
                      oc:nEditMove    := 0              // reread the cell
                      oc:lBitMap      := .T.            // remove display of field values from a column
                      oc:aBitMaps     := {}
                      For i := 1 To Len(op:aBmpLoad)
                          cImg := op:aBmpLoad[i]
                          hImg := LoadImage(cImg,,nHImg,nHImg)
                          AAdd( oc:aBitMaps, hImg )
                      Next
                      //
                      bBmpCell := {|nc,ob| // displaying an image depending on the column
                                    Local ocol  := ob:aColumns[nc]
                                    Local ni    := 0
                                    Local nMax  := LEN(ocol:aBitMaps)
                                    Local nCode := ob:GetValue(ob:Cargo:cField)
                                    //? ProcName(), nCode, ocol:cName, ocol:Cargo:cField
                                    IF !IsNumeric(nCode)
                                       nCode := 0
                                    ENDIF
                                    IF nCode <= 0 .OR. nCode >= nMax
                                       ni := nMax
                                    ELSE
                                       ni := nCode
                                    ENDIF
                                    //?? ocol:aBitMaps[ni]
                                    Return ocol:aBitMaps[ni]
                                    }

                      oc:uBmpCell := bBmpCell  // block code for changing images
                      oc:nAlign   := nMakeLong( DT_CENTER, DT_CENTER )
                      oc:nHAlign  := DT_CENTER
                      //oc:bData  :=  {||Nil}
                      //oc:cData  := '{||Nil}'
                      // Edit column -> Reserve
                      //oc:bPrevEdit := {|val, brw| ColumnEditPrev_Two( val, brw ) }
                   ENDIF
                   //
                   // change table cursor
                   // cFooting := Eval( oColumn:cFooting, nCol, oBrw )
                   oc := ob:GetColumn("COL_2")
                   oc:nFAlign  := DT_LEFT
                   oc:cFooting := {|nc,ob|
                                   Local na := ob:nAt, nl := ob:nLen
                                   nc := ""
                                   If ob:nLen > 0
                                      nc := hb_ntos(na)+ "/" + hb_ntos(nl)
                                      nc += Space(5) //+ " [!]"
                                   EndIf
                                   Return nc
                                   }
                   // when changing the table cursor
                   ob:bChange := {|ob|  _wPost(19, ob:cParentWnd, ob) }
                   //
                   ? "### oTsb:bBody", ProcNL(), ob:nHeightSuper, ob:lDrawSuperHd, ;
                     "Head=",ob:nHeightHead, "Foot",ob:nHeightFoot
                   DO EVENTS
                   Return Nil
                   }

   // Double-click the mouse cursor in the table
   //oTsb:bLDblClick := {|p1,p2,p3,ob| p1:=p2:=p3, ob:PostMsg( WM_KEYDOWN, VK_RETURN, 0 ) }
   oTsb:bLDblClick := .T.                       // Вот так !!!
   // Right-click on the cursor in the table
   //oTsb:bRClicked  := {|p1,p2,p3,ob| _wPost(50, ob:cParentWnd, {p1,p2,p3,ob}) }
   // Left-click on the cursor in the table
   //oTsb:bLClicked  := {|p1,p2,p3,ob| _wPost(XXX, ob:cParentWnd, {p1,p2,p3,ob}) }

   // Let's assign keys in the table
   oTsb:aUserKeys := { ;
        {VK_F2    , {|ob| _wPost(32, ob:cParentWnd, ob)           } }, ;   // reserve
        {VK_F3    , {|ob| _wPost(33, ob:cParentWnd, ob)           } }, ;   // reserve
        {VK_RETURN, {|ob|
                      Local oc := ob:aColumns[ ob:nCell ]
                      Local xval, lRet
                      IF oc:cName == "MARK"                                // reserve
                      ELSEIF oc:cName == "KZBID"                           // reserve
                         //oc:bPrevEdit := {|val, brw| ColumnEditPrev_Two( val, brw ) }
                         xval := ob:GetValue(ob:nCell)
                         lRet := EVal(oc:bPrevEdit, xval, ob )
                      ELSE
                        _wPost(40, ob:cParentWnd, ob)
                      ENDIF
                      Return Nil
                      } } }

   // назначить события на окно
   oTsb:aEvents   := { ;
        {32, {|ow,ky,ob| MsgDebug( ob:cAlias ), ob:Setfocus(), ky:=ow:Name  } }, ;   // reserve
        {33, {|ow,ky,ob| MsgDebug( ob:cAlias ), ob:Setfocus(), ky:=ow:Name  } }, ;   // reserve
        {50, {|ow,ky,ob| _wPost("_TsbRClick",ow) , ky:=ow:=ob               } }  ;   // reserve - right mouse click
                     }
RETURN oTsb

/////////////////////////////////////////////////////////////////////
// calculating column widths
STATIC FUNCTION CalculatColumnWidths(aXDim,cFont,aHead)
   LOCAL aDim, v, a, i, hFont, nW, aWSize, aWHead, nLen, aStr

   aDim   := ACLONE(aXDim)
   hFont  := GetFontHandle(cFont)
   aWSize := Array(Len(aDim[1]))
   aWHead := Array(Len(aDim[1]))
   aFill(aWSize, 0)
   aFill(aWHead, 0)

   FOR EACH a IN aDim
      FOR EACH v IN a
         i := hb_enumindex(v)
         IF !IsChar(v) ; v := cValToChar(v)
         ENDIF
         v  += "HH"  // additive
         nW := GetTextWidth( Nil, v, hFont )
         aWSize[ i ] := MAX(nW,aWSize[ i ])
      NEXT
   NEXT

   // recalculation of table header width
   FOR EACH a IN aHead
      IF ";" $ a
         aStr := HB_ATokens(a, ";")
         nLen := 0
         FOR i := 1 TO LEN(aStr)
             nLen := MAX( nLen, LEN(aStr[i]) )
         NEXT
         v := REPL("H",nLen)  // добавка
         nW := GetTextWidth( Nil, v, hFont )
      ELSE
         nW := GetTextWidth( Nil, a, hFont )
      ENDIF
      i := hb_enumindex(a)
      aWHead[i] := nW
   NEXT
   // let's make the columns wider
   FOR i := 1 TO LEN(aWSize)
      IF aWHead[i] > aWSize[i]
         aWSize[i] := aWHead[i]
      ENDIF
   NEXT

RETURN aWSize

///////////////////////////////////////////////////////////////////////////////
FUNCTION ProcNL(nVal, cMsg)
   Default cMsg := ">>> "
   nVal := iif( Empty(nVal), 0, nVal ) + 1
   cMsg += ProcName(nVal) + "(" + hb_ntos( ProcLine(nVal) ) + ")"
   cMsg += " => " + ProcFile(nVal)
RETURN cMsg
