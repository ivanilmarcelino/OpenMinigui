/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2025 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * _TBrowse() - Разное редактирование полей базы, функции обработки нажатия мышки
 * _TBrowse() - Various editing of database fields, functions for handling mouse clicks
*/
#define  _HMG_OUTLOG

#include "hmg.ch"
#include "tsbrowse.ch"
#include "dbinfo.ch"

REQUEST DBFCDX

Function Main()
   LOCAL cForm := "wMain", oTsb1, oBrw1, oTsb2, oBrw2
   LOCAL cAls1, cAls2, nY, nX, nW, nH, cTabl := "CUSTOMER4"
   LOCAL cTitl := " Mouse (Right, Left) click events" 
         cTitl += "    - Version 0.2 (29.09.2025)" 
   //
   USE ( cTabl )  ALIAS ( "CUST1" )  NEW SHARED
   cAls1 := Alias()
   USE ( cTabl )  ALIAS ( "CUST2" )  NEW SHARED
   cAls2 := Alias()

   DEFINE WINDOW &cForm TITLE "Demo TBrowse. DEMO_E[dit]. " + cTitl   ;
          AT 0,0 WIDTH Sys.ClientWidth HEIGHT Sys.ClientHeight        ;
          MAIN NOSIZE TOPMOST                                         ;
          ON INIT {|| This.Topmost := .F., This.Cargo:lOnInit := .T., ;
                          AEval(This.Cargo:aBrw, {|ob| ob:Show() }) } ;
          ON RELEASE {|| 
             This.Hide
             AEval( This.Cargo:aBrw, {|ob| (ob:cAlias)->(dbCloseArea()) })
             ?
             ? "*** Total running time of the program:", TimeFromStart()
             ?
             Return Nil
             }
      
      This.Cargo := oHmgData()
      This.Cargo:lOnInit := .F.   // On Init не выполнен 

      nY := nX := 0
      nW := This.ClientWidth
      nH := Int( This.ClientHeight / 2 )

      oTsb1 := oTsb_Def():Clone()              // ! This is if you don't use App.Cargo:oTsb
      oTsb1:cBrw := "oBrw1" ; oTsb1:nBrw := 1  // number tsb
      oTsb_my1(oTsb1, cAls1)                   // Editing previously set oTsb1 parameters 
      oBrw1 := _TBrowse( oTsb1, cAls1, /*cBrw*/, nY, nX, nW, nH ) //; oBrw1:SetFocus()

      oTsb2 := oTsb_Def():Clone()              // ! This is if you don't use App.Cargo:oTsb
      oTsb2:cBrw := "oBrw2" ; oTsb2:nBrw := 2  // number tsb
      oTsb_my2(oTsb2, cAls2)                   // Editing previously set oTsb2 parameters 
      oBrw2 := _TBrowse( oTsb2, cAls2, /*cBrw*/, nH, nX, nW, nH ) 

      This.Cargo:aBrw := {oBrw1, oBrw2}
      This.Cargo:nBrw := 2                  // set focus to the number tsb

      This.Cargo:aBrw[ This.Cargo:nBrw ]:SetFocus() 
      //  This.Cargo:aBrw[1]:SetFocus()     // first element
      // ATail(This.Cargo:aBrw):SetFocus()  // last element

      ON KEY F1     ACTION NIL
      ON KEY TAB    ACTION {|| 
                            Local ab := ThisWindow.Cargo:aBrw
                            Local nb := ThisWindow.Cargo:nBrw + 1
                            nb := iif( nb > Len(ab), 1, nb )
                            ab[ nb ]:SetFocus()
                            Return Nil
                            }
      ON KEY ESCAPE ACTION {||
                            Local nb := ThisWindow.Cargo:nBrw, ob
                            ob := ThisWindow.Cargo:aBrw[ nb ]
                            IF ob:IsEdit ; ob:SetFocus()
                            ELSE         ; ThisWindow.Release
                            ENDIF
                            Return Nil
                            }
   END WINDOW

   ACTIVATE WINDOW &cForm

RETURN NIL


FUNCTION oTsb_my1(oTsb, cAls, cBrw) // Editing previously set oTsb parameters
   LOCAL i, k, a
   LOCAL cTtl := "Showing database fields - column deletion method" + SPACE(8)
   Default cBrw := oTsb:cBrw
   //
   //oTsb:aEdit := .F.           // mode Edit .T.\.F.
   //
   // ~calculating the column width based on the field length in the structure
   a := (cAls)->( dbStruct() ) ; k := Len( a )

   oTsb:aHead    := Array(k)
   oTsb:aField   := Array(k)
   oTsb:aSizeLen := Array(k)

   FOR i := 1 TO k
      oTsb:aHead   [ i ] := a[ i ][1]
      oTsb:aField  [ i ] := a[ i ][1]
      oTsb:aSizeLen[ i ] := iif( a[ i ][3] > 50, 50, a[ i ][3] )
      IF a[ i ][2] $ "+^"
         oTsb:aSizeLen[ i ] := 5
      ELSEIF a[ i ][2] $ "=T"
         oTsb:aSizeLen[ i ] := 20
      ELSEIF Len( oTsb:aField[ i ] ) > oTsb:aSizeLen[ i ] 
         oTsb:aSizeLen[ i ] := Len( oTsb:aField[ i ] ) 
      ENDIF
      oTsb:aSizeLen[ i ] += 2
      IF oTsb:aSizeLen[ i ] > 10
         oTsb:aSizeLen[ i ] := Round(oTsb:aSizeLen[ i ] * 0.75, 0)
      ENDIF
   NEXT
   //
   oTsb:aNumber  := { 1, App.Object:W(0.6), DT_RIGHT, "bDelRed24" } // 1 or 6 or 7
   //
   oTsb:cSuperHd := cTtl + EVal(oTsb:bSuperHdSet, {cBrw, cAls}) + "   EDIT: " + ;
                    iif( Empty(oTsb:aEdit), "NO", "YES" )
   //oTsb:aSuperHdColor := {CLR_HBLUE, CLR_YELLOW}        // new color
   oTsb:b_mClick_0 := {|ap|
           Local cMsg := "Test SuperHider !"
           Local nLine, lRight, lSuper, oBrw, nYpix, nXpix, nRow, nCol, cTxt
           nLine  := ap[1]
           lRight := ap[2]
           lSuper := ap[3]
           oBrw   := ap[4]
           nYpix  := ap[5]
           nXpix  := ap[6]
           nRow   := ap[7]
           nCol   := ap[8]
           cTxt   := ap[9]

           ? ; ? "oTsb:b_mClick_0 =", ap ; ?v ap ; ?

           MsgDebug( cMsg, nLine, lRight, lSuper,         ;
                     valtype(oBrw)+":"+oBrw:cControlName, ;
                     nYpix, nXpix, nRow, nCol, cTxt )
           Return Nil
           }
   //
   IF   IsLogical(oTsb:aEdit) .and. oTsb:aEdit == .F.
      oTsb:bLDblClick := NIL                              // no Edit mode
   ELSEIF IsArray(oTsb:aEdit)
      i := 0 ; AEval(oTsb:aEdit, {|led| i += iif( Empty(led), 0, 1 ) })
      IF i == 0 ; oTsb:bLDblClick := NIL                  // no Edit mode
      ENDIF
   ENDIF
   IF !Empty( oTsb:bLDblClick ) ; oTsb:bLClicked := NIL   // removed the block
   ENDIF
   // for this purpose we hide the database columns
   oTsb:aHideCol := {"ADDR1", "ADDR2", "EDITTIME", "FAX", "TAXRATE", "LASTINVOIC" } 

   // ATTENTION!!! It's better to delete columns instead of oTsb:aHideCol := {"ADDR1", ...}
   // (they don't exist physically in Tsb) oTsb:aDelCol := {"ADDR1", ...}
   // hide slows down column processing by analyzing oCol:lVisible everywhere and performing actions on each column.

RETURN oTsb


FUNCTION oTsb_my2(oTsb, cAls, cBrw) // Editing previously set oTsb parameters
   LOCAL a, i, k, nPos, nLen, cTyp
   LOCAL cTtl := "Showing database fields - field list method" + SPACE(8)
   Default cBrw := oTsb:cBrw
   //
   //oTsb:aEdit := .F.           // mode Edit .T.\.F.
   //
   // ~calculating the column width based on the field length in the structure
   oTsb:aField := {"ID","EDITTIME","LCHK","COMPANY","ADDR1","ADDR2","LPRN","PHONE","FAX"}
   oTsb:aHead  := oTsb:aField
   oTsb:aName  := oTsb:aField ; k := Len( oTsb:aField )
   //
   oTsb:aFoot  := array(k) ; AFill(oTsb:aFoot, "")
   AEval(oTsb:aFoot, {|x,n| x := "("+hb_ntos(n)+")", oTsb:aFoot[ n ] := x })
   oTsb:aEdit    := array(k) ; AFill(oTsb:aEdit, .T.)  // editing columns
   oTsb:aEdit[1] := .F.
   oTsb:aEdit[2] := .F.
   // for installation in columns: DT_LEFT, DT_CENTER, DT_RIGHT
   //oTsb:aAlign := array(k)  // or  "LEFT" , "CENTER" , "RIGHT"
   //
   oTsb:aSizeLen := Array(k)  ; AFILL(oTsb:aSizeLen,0)
   // ~calculating
   FOR i := 1 TO k
       nPos := (cAls)->( FieldPos ( oTsb:aField[ i ] ) )
       nLen := (cAls)->( FieldLen ( nPos ) )
       cTyp := (cAls)->( FieldType( nPos ) )
       oTsb:aSizeLen[ i ] := iif( nLen > 50, 50, nLen )
       IF cTyp $ "+^"
          oTsb:aSizeLen[ i ] := 5
       ELSEIF cTyp $ "=T"
          oTsb:aSizeLen[ i ] := 22
       ENDIF
       // if the column header is larger than the field length
       IF Len( oTsb:aHead[ i ] ) > oTsb:aSizeLen[ i ] 
          oTsb:aSizeLen[ i ] := Len( oTsb:aHead[ i ] ) 
       ENDIF
       oTsb:aSizeLen[ i ] += 2  // supplement due to nMarginLR
       IF oTsb:aSizeLen[ i ] > 10
          oTsb:aSizeLen[ i ] := Round(oTsb:aSizeLen[ i ] * 0.75, 0)
       ENDIF
   NEXT
   //
   oTsb:aNumber  := { 1, App.Object:W(0.6), DT_RIGHT, "bDelRed24" } // 1 or 6 or 7
   //
   oTsb:cSuperHd := cTtl + EVal(oTsb:bSuperHdSet, {cBrw, cAls}) + "   EDIT: " + ;
                    iif( Empty(oTsb:aEdit), "NO", "YES" )
   oTsb:aSuperHdColor := {CLR_YELLOW, { RGB(147,112,219), RGB(48,29,26) } }        // new color

   IF   IsLogical(oTsb:aEdit) .and. oTsb:aEdit == .F.
      oTsb:bLDblClick := NIL                              // no Edit mode
   ELSEIF IsArray(oTsb:aEdit)
      i := 0 ; AEval(oTsb:aEdit, {|led| i += iif( Empty(led), 0, 1 ) })
      IF i == 0 ; oTsb:bLDblClick := NIL                  // no Edit mode
      ENDIF
   ENDIF
   IF !Empty( oTsb:bLDblClick ) ; oTsb:bLClicked := NIL   // removed the block
   ENDIF

   // 2 , background in table cells - replace colors
   a := oTsb:aColorAdd
   AAdd( a, { CLR_PANE, {|nr,nc,ob| // change the color to your zebra
             Local nClr := ob:nClrPane
             Local aClr := {RGB(194,154,194), ;
                            RGB(207,196,232), ;
                            CLR_GRAY}
             nc := nr % 2
             nClr := aClr[ nc + 1 ]
             IF (ob:cAlias)->( Deleted() )
                nClr := aClr[3]
             ENDIF
             Return nClr
             } } )

   AAdd( a, { CLR_HEADB, { RGB(48,29,26), RGB(109,25,98) } } ) // 4 , table header background
   AAdd( a, { CLR_FOOTB, { RGB(109,25,98), RGB(48,29,26) } } ) // 10, table footer background
   AAdd( a, { CLR_SPCB , CLR_HGRAY } )                         // 19, specheader back - table numberer

RETURN oTsb


FUNCTION oTsb_Def(aBrush, nClr1, nClr2, cFreeze)
   LOCAL a, i, oTsb := oHmgData()
   Default nClr1   := RGB(247,172, 8)    // background color header+footer
   Default nClr2   := RGB( 48, 29,26)    // grey and black background
   Default aBrush  := {240,240,240}      // background color under table
   Default cFreeze := "ID"               // freeze columns up to and including ID
   //
   oTsb:lHide        := .T.
   oTsb:lZebra       := .T.
   oTsb:aEdit        := .T.
   oTsb:aFoot        := .T.
   oTsb:uSelector    := 20 // NIL
   IF Empty(oTsb:uSelector) ; i := 7
   ELSE                     ; i := 6
   ENDIF
   oTsb:aNumber       := { 1, App.Object:W(0.6), DT_CENTER, i } // 1 or 6 or 7
   oTsb:aNumber_nBClr := GetSysColor( COLOR_BTNFACE )           // system color
   oTsb:aNumber_nFClr := CLR_RED
   //
   oTsb:cFreeze       := cFreeze
   oTsb:aBrush        := aBrush
   // параметы вынесенные:
   // для oCol:lCheckBox == .T.
   oTsb:aCheckBox     := { "bMgCheckT20" , "bMgCheckF20" } // вынесли
   oTsb:nCellMarginLR := 1                                 // параметы
   oTsb:lNoKeyChar    := .F.                               // в 
   oTsb:lNoHScroll    := .T.                               // oTsb
   oTsb:nMemoHV       := 1                                 //
   //
   oTsb:bLDblClick    := .T.
   oTsb:lSpecHd       := .T.
   oTsb:cSpecHdChar   := "#"
   oTsb:lSuperHd      := .T.
   oTsb:aSuperHdColor := {CLR_YELLOW, { nClr1, nClr2 } } // color: superheader text and background
   oTsb:bSuperHdSet   := {|a,cMsg|
                          cMsg := a[1] + "." + a[2] + " -> " 
                          cMsg += Lower((a[2])->( dbInfo( DBI_FULLPATH ) ))
                          Return cMsg + " "
                          }
   //
   oTsb:nHeightHead  := App.Object:H(1.1)
   oTsb:nHeightCell  := App.Object:H(1.1)
   oTsb:nHeightFoot  := App.Object:H(1.1)
   oTsb:nHeightSuper := App.Object:H(1.2)
   //
   //
   // 1 - line: Cell. Left mouse click
   oTsb:bLClicked := {|nrp,ncp,nfl,ob| bMClicked(.F., 1, ob, nrp, ncp, nfl) }
   /*
   oTsb:bLClicked := {|nrp,ncp,nfl,ob| 
        Local nRow, nCol, cTxt := 'Cell. Left mouse click'
        nRow  := ob:GetTxtRow(nrp)       // table row cursor number      
        nCol  := Max(ob:nAtCol(ncp), 1 ) // cursor column number in table
        MsgDebug(ob:cControlName, cTxt, nRow, nCol)
        Return Nil
        }
   */
   // 1 - line: Cell. Right mouse click
   oTsb:bRClicked := {|nrp,ncp,nfl,ob| bMClicked(.T., 1, ob, nrp, ncp, nfl) }
   /*
   oTsb:bRClicked := {|nrp,ncp,nfl,ob| 
        Local nRow, nCol, cTxt := 'Cell. Right mouse click'
        nRow  := ob:GetTxtRow(nrp)       // table row cursor number      
        nCol  := Max(ob:nAtCol(ncp), 1 ) // cursor column number in table
        MsgDebug(ob:cControlName, cTxt, nRow, nCol)
        Return Nil
        }
   */
   // 0 - line: [Super] Header. Left mouse click
   oTsb:bHLClicked := {|nrp,ncp,nat,ob| bMClicked(.F., 0, ob, nrp, ncp, nat) }
   /*
   oTsb:bHLClicked := {|nrp,ncp,nat,ob| 
        Local nRow, nCol, cTxt := 'Header. Left mouse click'
        IF nrp < ob:nHeightSuper ; cTxt := "Super " + cTxt
        ENDIF
        nRow := ob:GetTxtRow(nrp)       // table row cursor number      
        nCol := Max(ob:nAtCol(ncp), 1 ) // cursor column number in table
        MsgDebug(ob:cControlName, cTxt, nRow, nCol)
        Return Nil
        }
   */
   // 0 - line: [Super] Header. Right mouse click
   oTsb:bHRClicked := {|nrp,ncp,nat,ob| bMClicked(.T., 0, ob, nrp, ncp, nat) }
   /*
   oTsb:bHRClicked := {|nrp,ncp,nat,ob|  
        Local nRow, nCol, cTxt := 'Header. Right mouse click'
        IF nrp < ob:nHeightSuper ; cTxt := "Super " + cTxt
        ENDIF
        nRow := ob:GetTxtRow(nrp)       // table row cursor number      
        nCol := Max(ob:nAtCol(ncp), 1 ) // cursor column number in table
        MsgDebug(ob:cControlName, cTxt, nRow, nCol)
        Return Nil
        }
   */
   // 3 - line: Special Header. Left mouse click
   oTsb:bSLClicked := {|nrp,ncp,nat,ob| bMClicked(.F., 3, ob, nrp, ncp, nat) }
   /*
   oTsb:bSLClicked := {|nrp,ncp,nat,ob| 
        Local nRow, nCol, cTxt := 'Special Header. Left mouse click'
        nRow := ob:GetTxtRow(nrp)       // table row cursor number      
        nCol := Max(ob:nAtCol(ncp), 1 ) // cursor column number in table
        MsgDebug(ob:cControlName, cTxt, nRow, nCol)
        Return Nil
        }
   */
   // 3 - line: Special Header. Right mouse click
   oTsb:bSRClicked := {|nrp,ncp,nat,ob| bMClicked(.T., 3, ob, nrp, ncp, nat) }
   /*
   oTsb:bSRClicked := {|nrp,ncp,nat,ob| 
        Local nRow, nCol, cTxt := 'Special Header. Right mouse click'
        nRow := ob:GetTxtRow(nrp)       // table row cursor number      
        nCol := Max(ob:nAtCol(ncp), 1 ) // cursor column number in table
        MsgDebug(ob:cControlName, cTxt, nRow, nCol)
        Return Nil
        }
   */
   // 2 - line: Footer. Left mouse click
   oTsb:bFLClicked := {|nrp,ncp,nat,ob| bMClicked(.F., 2, ob, nrp, ncp, nat) }  
   /*
   oTsb:bFLClicked := {|nrp,ncp,nat,ob|  
        Local nRow, nCol, cTxt := 'Footer. Left mouse click'
        nRow  := ob:GetTxtRow(nrp)       // table row cursor number      
        nCol  := Max(ob:nAtCol(ncp), 1 ) // cursor column number in table
        MsgDebug(ob:cControlName, cTxt, nRow, nCol)
        Return Nil
        }
   */
   // 2 - line: Footer. Right mouse click
   oTsb:bFRClicked := {|nrp,ncp,nat,ob| bMClicked(.T., 2, ob, nrp, ncp, nat) }
   /*
   oTsb:bFRClicked := {|nrp,ncp,nat,ob| 
        Local nRow, nCol, cTxt := 'Footer. Right mouse click'
        nRow := ob:GetTxtRow(nrp)       // table row cursor number      
        nCol := Max(ob:nAtCol(ncp), 1 ) // cursor column number in table
        MsgDebug(ob:cControlName, cTxt, nRow, nCol)
        Return Nil
        }
   */
   //
   oTsb:aColorAdd := {} ; a := oTsb:aColorAdd
   // setting colors in a table 
   AAdd(a, { CLR_TEXT, {|nr,nc,ob,nd| nr := CLR_HGRAY, nc := CLR_BLACK, nd := CLR_BLACK, ;
                         iif( (ob:cAlias)->(DELETED()), nr, iif( ob:nAt % 2 == 0, nc, nd ) ) } } )
   // 2 , background in table cells
   AAdd(a, { CLR_PANE, {|nr,nc,ob| // change the color to your zebra
                         Local nClr := ob:nClrPane
                         Local aClr := {RGB(238,201,120), ;
                                        RGB(247,233,204), ;
                                        CLR_BLACK}
                         nc := nr % 2
                         nClr := aClr[ nc + 1 ]
                         IF (ob:cAlias)->( Deleted() )
                            nClr := aClr[3]
                         ENDIF
                         Return nClr
                         } } )
   // 
   AAdd(a, { CLR_HEADF, CLR_WHITE  })         // 3 , table header text
   AAdd(a, { CLR_HEADB, { nClr2, nClr1 } })   // 4 , table header background
   AAdd(a, { CLR_FOCUSF, {|a,b,ob|            // 5 , focused text
                           Local nClr := CLR_BLACK 
                           a := b
                           //nClr := iif( ob:nCell == b, CLR_HRED, -CLR_BLUE ) 
                           IF (ob:cAlias)->( Deleted() )
                              nClr := CLR_WHITE
                           ENDIF
                           Return nClr
                           } } ) 

   AAdd(a, { CLR_FOCUSB, {|a,b,c| a := b, If( c:nCell == b, -CLR_HRED, -CLR_BLUE ) } } ) // 6 , cursor background
   AAdd(a, { CLR_EDITF, CLR_YELLOW       })  // 7 , editable field text
   AAdd(a, { CLR_EDITB, CLR_HRED         })  // 8 , editable field background
   AAdd(a, { CLR_FOOTF, CLR_WHITE        })  // 9 , table footer text
   AAdd(a, { CLR_FOOTB, { nClr1, nClr2 } })  // 10, table footer background
   AAdd(a, { CLR_SPCF , CLR_RED          })  // 18, specheader text - table numberer
   AAdd(a, { CLR_SPCB , CLR_YELLOW       })  // 19, specheader back - table numberer
   //
   oTsb:bGotFocus := {|ob|
          Local oo, owc
          IF !IsObject(ob) ; Return Nil
          ENDIF
          SET WINDOW THIS TO ob:cParentWnd
          owc := This.Cargo
          IF !Empty(owc:lOnInit)               // on init window completed
             owc:nBrw := ob:Cargo:nBrw
             FOR EACH oo IN owc:aBrw ; oo:Refresh() ; DO EVENTS
             NEXT
          ENDIF
          SET WINDOW THIS TO 
          Return Nil
          }
   // separate block for :aNumber
   oTsb:b_aNumber := {|ob,op,nCol,cCol|
          Local oc := ob:aColumns[ nCol ], hFont
          Local nBClr := ob:Cargo:oParam:aNumber_nBClr
          Local nFClr := ob:Cargo:oParam:aNumber_nFClr
          Default nBClr := GetSysColor( COLOR_BTNFACE ) // system color
          Default nFClr := CLR_RED
          If IsArray(op:aFont)
             hFont := GetFontHandle(op:aFont[4])        // SpecHider
          Else
             hFont := ob:aColumns[1]:hFontSpcHd         // 4-special header font 
          Endif
          oc:nClrBack := nBClr
          oc:nClrFore := nFClr
          oc:hFont    := hFont            
          oc:bDecode  := {|cv| Alltrim(cv) }
          cCol := op    // column name "ARRAYNO" or "ORDKEYNO"
          Return Nil
          }

   oTsb:b_Init_Def := {|ob,op| // TSB settings
          Local cID := op:cFreeze, nMemoHV := op:nMemoHV   // carried out 
          Local nMarginLR   := op:nCellMarginLR            // parameters
          Local lNoKeyChar  := op:lNoKeyChar               // to oTsb
          Default nMarginLR := 1, lNoKeyChar := .F., nMemoHV := 1
          IF IsNumeric(op:nBrw) ; ob:Cargo:nBrw := op:nBrw // number tsb
          ENDIF
          IF !Empty(cID)
             ob:nFreeze     := ob:nColumn(cID) // Freeze columns
             ob:lLockFreeze := .T.             // Avoid cursor rendering on frozen columns
          ENDIF
          ob:lNoKeyChar     := lNoKeyChar // input of letters and numbers into cells
          ob:nMemoHV        := nMemoHV    // displaying one line of the database memo field
          ob:nCellMarginLR  := nMarginLR  // indent from the cell line when pressing left or right by the number of spaces
          IF IsBlock(op:bInit_2)
             ? "### Additional settings connected :bInit_2"
             EVal(op:bInit_2, ob, op)
          ENDIF
          Return Nil
          }

   oTsb:b_Body_Def := {|ob,op| // other TSB settings
          Local oc, cv, aChk, i
          Local aCheck := op:aCheckBox                      // carried out
          Local lNoHScroll := op:lNoHScroll                 // parameters in oTsb
          Default aCheck := {"bMgCheckT20", "bMgCheckF20"}  
          Default lNoHScroll := .T.
          // один раз грузим handle bmp
          aChk := { LoadImage(aCheck[1]), LoadImage(aCheck[2]) }
          IF ( ob:lNoHScroll := lNoHScroll ) // no horizontal scrolling display
             ob:oHScroll := NIL
          ENDIF
          FOR EACH oc IN ob:aColumns
             i := hb_EnumIndex(oc)
             ? "***", i, ob:GetColumn(i):cName  // output to the log _MsgLog.txt
             IF oc:cName == "ARRAYNO" .OR. oc:cName == "ORDKEYNO" ; LOOP
             ENDIF
             IF oc:lCheckBox
                oc:lEdit     := .T.
                oc:cPicture  := Nil
                oc:nAlign    := DT_CENTER
                oc:nEditMove := 0
                oc:aCheck    := { aChk[1], aChk[2] }
             ENDIF
             If ob:lIsDbf .AND. !IsArray(op:aFoot)     // ONLY for Dbf, 
                cv := oc:cFieldTyp + '('               // and the names in the basement are not set
                cv += HB_NtoS(oc:nFieldLen) + ','
                cv += HB_NtoS(oc:nFieldDec) + ')'
                oc:cFooting := cv
             Endif
             oc:nFAlign := DT_CENTER
             // Column editing is prohibited
             If ob:lIsDbf .And. oc:cFieldTyp $ "+=^"  
                 oc:nClrFootFore := CLR_WHITE
                 oc:nClrFootBack := CLR_HRED
             Endif
          NEXT
          ? "***"
          Return Nil
          }

   oTsb:b_After_Def := {|ob| //,op|
          Local oc
          IF ob:lSelector
             ob:lClrSelectorHdBack := .F.
             // If you remove the line ob:lClrSelectorHdBack, then the line below
             //ob:nClrSelectorHdBack := CLR_HGRAY 
             oc := ob:aColumns[1]
             oc:nClrBack := {|clr,del,obr|
                              clr := CLR_HGRAY
                              IF ( del := (obr:cAlias)->( Deleted() ) )
                                 clr := CLR_GRAY
                              ENDIF
                              Return clr
                              }
          ENDIF
          Return Nil
          }

RETURN oTsb

FUNCTION bMClicked(lRight, nLine, oBrw, nYpix, nXpix, nAt)
   LOCAL oTsb := oBrw:Cargo:oParam, cBlk, bBlk, a
   LOCAL nRow, nCol, cTxt, cMsg := "^1. ^2 mouse click", nFlag, lSupr
   LOCAL aMsg := {"HEADER", "CELL", "FOOTER", "SPEC. HEADER"}
   //   nLine -> ^    0        1        2            3      value
   //
   SET WINDOW THIS TO oBrw:cParentWnd
   //
   This.Cargo:lOnInit := .F.              // lock Refresh
   //
   nLine += 1 ; cTxt := StrTran( cMsg, "^1", aMsg[ nLine ] )
   nFlag := iif( nLine == 2, nAt, 0 )
   lSupr := nLine == 1 .and. nYpix < oBrw:nHeightSuper

   IF lSupr ; cTxt := "SUPER " + cTxt
   ENDIF

   nRow := oBrw:GetTxtRow (nYpix)       // table row cursor number      
   nCol := Max(oBrw:nAtCol(nXpix), 1 )  // cursor column number in table
   
   cTxt := StrTran( cTxt, "^2", iif( lRight, "Right", "Left" ) )
   
   MsgDebug(oBrw:cControlName, cTxt, nRow, nCol, "RecNo=", (oBrw:cAlias)->(RecNo()))

   IF nLine == 2 .and. AlertYesNo("Enable correction ?")
      DO EVENTS ; oBrw:PostMsg( WM_KEYDOWN, VK_RETURN, 0 )
   ELSE
      // example of launching execution on click:
      // oTsb:b_mClick_0 := {|ap| ... }   // super    
      // oTsb:b_mClick_1 := {|ap| ... }   // header   
      // oTsb:b_mClick_2 := {|ap| ... }   // line     
      // oTsb:b_mClick_3 := {|ap| ... }   // footer   
      // oTsb:b_mClick_4 := {|ap| ... }   // spec.header
      cBlk := "b_mClick_" + hb_ntos( iif( lSupr, 0, nLine ) )
      bBlk := oTsb:Get(cBlk)
      IF IsBlock( bBlk )                  // find block code
         a := {nLine, lRight, lSupr, oBrw, nYpix, nXpix, nRow, nCol, cTxt}
         EVal( bBlk, a )
      ENDIF
   ENDIF

   This.Cargo:lOnInit := .T.              // unlock Refresh

   SET WINDOW THIS TO 

RETURN Nil

*----------------------------------------------------------------------------*
INIT PROCEDURE Sets_ENV()
*----------------------------------------------------------------------------*
   LOCAL cFont := "Arial", nSize := 11, lDebug := .F.
   LOCAL cLog  := hb_FNameDir (App.ExeName) + "_" + ;
                  hb_FNameName(App.ExeName) + ".log"

   rddSetDefault( "DBFCDX" )

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
   SET TOOLTIPSTYLE BALLOON

   IF !HB_ISOBJECT( App.Cargo ) ; App.Cargo := oHmgData()
   ENDIF

   Set ShowRedAlert On 

   SET MULTIPLE QUIT WARNING  
   SET WINDOW MAIN OFF

   SET WINDOW MODAL PARENT HANDLE ON
   SET TOOLTIPSTYLE BALLOON
   SET NAVIGATION EXTENDED
   SET MENUSTYLE  EXTENDED
   // Alert* colors
   SET MSGALERT BACKCOLOR  TO  { 247,172, 8 }
   SET MSGALERT FONTCOLOR  TO  { 0  ,  0, 0 }
   //
   _SetGetLogFile( cLog ) ; hb_FileDelete( cLog ) ; SET LOGERROR ON
   //
   IF     Sys.DesktopWidth >= 1920 ; nSize += 4 
   ELSEIF Sys.DesktopWidth >  1280 ; nSize += 2 
   ENDIF
   //
   SET FONT TO cFont, nSize

   _DefineFont("Normal"  , cFont, nSize  , .F., .F. )
   _DefineFont("Bold"    , cFont, nSize  , .T., .F. )
   _DefineFont("Italic"  , cFont, nSize-4, .F., .T. )
   // Alert* font
   _DefineFont("DlgFont" , cFont, nSize+2, .F., .F. )
   //
   _HMG_bOnErrorInit := {|cMsg,oErr,cTxt,cErr| my_ErrorExit(cMsg,oErr,cTxt,cErr) }
   _HMG_bOnErrorExit := {|| my_ErrorExit() }
   //
   ALTD(iif( lDebug, 1, 0 ))       // 1 - debug mode, 0 - no debug mode
   //
   ? REPL("=",20) + " Program start - " + HB_TTOC( hb_DateTime() ) + " " + REPL("=",20)
   ? MiniGuiVersion()  ; ? Version() ; ? hb_Ccompiler() 
   IF lDebug ; ? upper(hb_FNameName(App.ExeName)),"Debug mode is enabled."
   ENDIF
   ?

RETURN 

FUNCTION my_ErrorExit(cMsg,oErr,cTxt,cErr)

   IF pCount() > 0
      AlertStop(cErr)
      cTxt := oErr
   ENDIF

RETURN cMsg

