/*
 * MINIGUI - Harbour Win32 GUI library
 *
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com>
*/
#define  _HMG_OUTLOG
#include "hmg.ch"
///////////////////////////////////////////////////////////////////
STATIC FUNCTION MenuButtonsTopThisForm(aBColor)
   LOCAL hFont, aFont, nHBtn, oMenu
   DEFAULT aBColor := GRAY

   oMenu := oHmgData()
   oMenu:aImg := { {"iMg96x1"     ,"iMg96x2"      } ,;
                   {"iMg96x1"     ,"iMg96x2"      } ,;
                   {"iMg96x1"     ,"iMg96x2"      } ,;
                   {"iMg96x1"     ,"iMg96x2"      } ,;
                   {"iMg96x1"     ,"iMg96x2"      } ,;
                   {"iMg96x1"     ,"iMg96x2"      } ,;
                   {"iMg96x1"     ,"iMg96x2"      } ,;
                   {"iMg96x1"     ,"iMg96x2"      } ,;
                   {"iMg96x1"     ,"iMg96x2"      } ,;
                   {"iReturn48x1" ,"iReturn48x2"  }     }
   // ��� ������� + ��� �������
                  //  1          2          3             4              5            6             7           8            9           10
   oMenu:aMnEn  := { ""      , "Card" ,"F3;Refresh" ,"F4 Calculat", "F5 Print", "F6 Sorting", "F7 Search", "F8 Menu","Table;settings", "Exit"  }
   oMenu:aCapt  := oMenu:aMnEn
   oMenu:aObj   := { "_ZHelp","_ZCard","_ZF3Refresh","_ZF4Calc"   ,"_ZF5Print", "_ZF6Sort"  , "_ZF7Find" ,"_ZF8Menu","_ZF9Config"    , "_ZExit" }
   oMenu:a1BClr := { {4,135,109}, BLUE, GREY, GREY, {210, 71, 38}, {151,  0,160} ,{  9, 77,181}, {255,160, 66}, { 94, 59,185} , {189, 30, 73}  }

   oMenu:nHIco     := 48           // ������ ������ �� ������
   oMenu:nH1Ico    := 96           // ������ ������ ������ �� ������
   oMenu:nHG2      := 10           // ���������� ������ � ������ ������
   oMenu:aBtnFClr  := { BLACK, YELLOW, RED, BLACK     }         // ���� ����� ������ + 2/3/4-���� ���������
   //oMenu:aBtnBClr:= { {66,92,251} , WHITE, YELLOW, GRAY }     // ���� ���� ������ + 2/3/4-���� ���������
   oMenu:aBtnBClr  := { aBColor , WHITE, YELLOW, GRAY }         // ���� ���� ������ + ���� ���������

   hFont := GetFontHandle('FntBtnMain')                         // ���� ������ ������� �����
   aFont := GetFontParam(hFont)
   oMenu:aBtnFont  := { aFont[1], aFont[2], aFont[3] }          // ���� �� �������
   //oMenu:aBtnFont := { "Tahoma", aFont[2], .F. }              // ���� �� ������� - ����� � ��� ������
   //nHBtn         := oMenu:nHIco + oMenu:nHG2 + aFont[2] * 4   // 2 ������ ������ �� �������
   nHBtn           := oMenu:nHIco + oMenu:nHG2 + aFont[2] * 4   // 1 ������ ������ �� �������
   oMenu:lVert     := .T.        // ������������ ����� �� ������
   ? ProcNL(), "$$$$$$$$ 2 rows of buttons=", nHBtn

   oMenu:nX        := 0
   oMenu:nY        := 0
   //oMenu:lAutoSize := .T.      // T - �������������� ������ ������ � ������ ������ �� ������ ������
   oMenu:nWBtn     := 0          // ������ ������� ������ ������
   //oMenu:nHBtn   := 0          // ������ ������� ������ ������
   oMenu:nClientW  := 0          // ������ ����

   oMenu:lAutoSize := .F.        // F - ������ �������
   oMenu:nWBtn     := nHBtn      // ������ ������� ������ ������
   oMenu:nHBtn     := nHBtn      // ������ ������� ������ ������
   oMenu:lGradient := .T.        // �������� �� ������� - ��������

RETURN oMenu

///////////////////////////////////////////////////////////////////
FUNCTION Table_One(oWnd,ky,cn)
   LOCAL oMenu, cForm, cTtl, cIco, cAls, cBtnEnabled, cWnd
   LOCAL nTable, cSprHd, oColumn, oClr

   cWnd   := oWnd:Name
   oMenu  := MenuButtonsTopThisForm()
   cForm  := "FORM_Defect"
   cTtl   := "List of repair requests"
   cIco   := "iMg96x1"
   cAls   := "Defect"
   nTable := 1
   cSprHd := "Take data from ini-config"
   cBtnEnabled := ky := cn

   oColumn := Column_TSB(cWnd,cAls)   // ������ ������� �������
   oClr    := oHmgData()              // ��� ����� ��� �������
   oClr:aBClr  := {   6,211,170 }     // ���� ���� ���� �����
   oClr:aBrush := { 136,240,219 }     // ���� ���� ��� ��������
   oClr:lZebra := .T.                                    // ��� ���.\����. �������� zebra
   //oClr:aZebra := { {230,230,230}, SILVER }            // ����� ����
   oClr:aZebra := { oClr:aBClr, {187,244,233} }

   FormTable12(nTable, cForm, cTtl, cIco, cAls, cBtnEnabled, cSprHd, oClr, oMenu, oColumn)

RETURN NIL

///////////////////////////////////////////////////////////////////////////////////////////
STATIC FUNCTION Column_TSB(cWnd,cAls)   // ������ ������� �������
   LOCAL aCol, a, i, j, cFld, cType, bBlock, lEdit, c8Col, aSprav, aDcd
   LOCAL aKey, cMsg, cErr, lFind, nWCol, oCol, aMaster
   LOCAL owc := _WindowCargo(cWnd)     // cargo ������������ �����

   IF owc == NIL
      owc := This.Cargo    // Cargo �������� ����
   ENDIF
   DbSelectArea(cAls)      // ����������� ����� !!!

   aCol    := {}
   aSprav  := {"Operat","KOperat","Operat","Operators","Name",""}
   aMaster := {"Master","KMaster","Master", 2,"KFIRMA==1"}
   // �������� ! � ���� ������ ���� ���� PUSTO ������� �� ������������, ��� ����� ��� :LoadFields()
   //             1    2    3            4                     5               6              7       8            9              10             11
   //        ����-��|Edit| ��� | �������� �������         | ���� ��/�����.| ������ ����  | ������ |���� ���� | *F �������1   |  **F �������2 | ���.������
   AADD( aCol, {  1 , "W", "L" , "(�)"                    , "MARK"        , REPL("X",5)  , nil    , nil      , nil          , nil            , nil   } )
   AADD( aCol, {  2 , "W","BMP", "*�"                     , "KZBID"       , "XXX"        , nil    , nil      , "myImage()"  , nil            , nil   } )
   AADD( aCol, {  3 , "W", "N" , "Number;bid"             , "NNza"        , REPL("9",12) , nil    , "bKey03" , nil          , nil            , nil   } )
   AADD( aCol, {  4 , "W", "D" , "Date;bid"               , "Dateza"      , "00990990990", nil    , nil      , nil          , "CheckDate()"  , nil   } ) // �������� ����� �����
   AADD( aCol, {  5 , "W", "N" , "Time;bid"               , "TimeZa"      , REPL("9",8)  , nil    , "bKey05" , nil          , nil            , nil   } )
   AADD( aCol, {  6 , "W", "J" , "Address bid"            , "ADRESPRN"    , REPL("x",61) , nil    , nil      , "myAdres()"  , nil            , nil   } )
   AADD( aCol, {  7 , "R", "C" , "Malfunction on bid"     , "PUSTO"       , REPL("A",62) , nil    , "bKey11" , nil          , nil            , nil   } )
   AADD( aCol, {  8 , "W", "S" , "Master on bid"          , "KMaster"     , REPL("A",25) , nil    , "bKey12" , aMaster      , nil            , "iMaster48" } )
   AADD( aCol, {  9 , "W", "S" , "Entered by"             , "KOPERAT0"    , REPL("A",25) , nil    , "bKey18" , aSprav       , nil            , "iUser32"   } )
   AADD( aCol, {  10, "R", "S" , "Who ruled?"             , "KOPERAT"     , REPL("A",25) , nil    , "bKey20" , aSprav       , nil            , "iUser32"   } )
   AADD( aCol, {  11, "R", "C" , "Date/time;of edit"      , "PUSTO"       , REPL("A",22) , nil    , "bKey21" , nil          , nil            , nil   } )
   AADD( aCol, {  12, "R", "+" , "ID recno"               , "IDZ"         , REPL("9",9)  , nil    , nil      , nil          , nil            , nil   } )
   AADD( aCol, {  13, "R", "=" , "TS recno"               , "TSZ"         , REPL("9",23) , nil    , nil      , nil          , nil            , nil   } )
   /*
   AADD( aCol, {  3 , "W", "N" , "Number;bid"           , "NNza"        , REPL("9",12) , nil    , "bKey03" , nil          , nil            , nil   } )
   AADD( aCol, {  4 , "W", "D" , "����;bid"            , "Dateza"      , "00990990990", nil    , nil      , nil          , "CheckDate()"  , nil   } ) // �������� ����� �����
   AADD( aCol, {  5 , "W", "N" , "�����;bid"           , "TimeZa"      , REPL("9",8)  , nil    , "bKey05" , nil          , nil            , nil   } )
   AADD( aCol, {  6 , "W", "J" , "����� ������"           , "ADRESPRN"    , REPL("x",61) , nil    , nil      , "myAdres()"  , nil            , nil   } )
   AADD( aCol, {  7 , "R", "C" , "������������� �� ������", "PUSTO"       , REPL("A",62) , nil    , "bKey11" , nil          , nil            , nil   } )
   AADD( aCol, {  8 , "W", "S" , "������ �� ������"       , "KMaster"     , REPL("A",25) , nil    , "bKey12" , aMaster      , nil            , "iMaster48" } )
   AADD( aCol, {  9 , "W", "S" , "��� �������"            , "KOPERAT0"    , REPL("A",25) , nil    , "bKey18" , aSprav       , nil            , "iUser32"   } )
   AADD( aCol, {  10, "R", "S" , "��� ������"             , "KOPERAT"     , REPL("A",25) , nil    , "bKey20" , aSprav       , nil            , "iUser32"   } )
   AADD( aCol, {  11, "R", "C" , "����/����� ������"      , "PUSTO"       , REPL("A",22) , nil    , "bKey21" , nil          , nil            , nil   } )
   */
   // ��� ������ ����� ��� ������ � �������
   aKey := {}
   ? "#############", ProcNL(), ALIAS()
   AADD( aKey, { "bKey01", {|| Nil  }                                             } )
   AADD( aKey, { "bKey03", {|| TRANSFORM(FIELD->NNza  , "@R 99999/99") }          } )
   AADD( aKey, { "bKey05", {|| TRANSFORM(FIELD->TimeZa, "@R 99:99")  }            } )
   AADD( aKey, { "bKey11", {|| REPL("*-?-",10)                                  } } )  // ���� - �������� �� �������
   AADD( aKey, { "bKey12", {|| SAY_SEL_DIM(FIELD->KMaster ,"Master", "Master")  } } )  //
   AADD( aKey, { "bKey18", {|| SAY_SEL_DIM(FIELD->KOPERAT0,"Operat", "Operat")  } } )  // ��� �������
   AADD( aKey, { "bKey20", {|| SAY_SEL_DIM(FIELD->KOPERAT,"Operat", "Operat")   } } )  // ��� ������
   AADD( aKey, { "bKey21", {|| myFldTime()                                      } } )  // ����/����� ������
   cErr := ""

   // ��� ������� ����� ���������� ��������� 30 �������� �� ������ � ������� ?
   // How to make it show the last 30 characters from a line in a column?
   // oCol:bDecode := {|ca| ca := trim(ca), iif( Len(ca) > 30, "..."+right(ca, 30), ca ) }
   aDcd := ARRAY(LEN(aCol))
   AFILL(aDcd, "" )
   i := 6           // "Bid Address"
   aDcd[i] := {|ca| ca := trim(ca), iif( Len(ca) > 47, ".."+right(ca, 47), ca ) }

   // ���������� ������� �������
   oCol := oHmgData()

   oCol:cAls      := ALIAS()
   //                    cell     Head    Foot     SpecHider  SuperHider   Edit
   oCol:aFont     := { "Normal", "Bold", "Italic", "SpecHdr" , "SuperHd", "TsbEdit" }
   oCol:aHead     := {}
   oCol:aField    := {}
   oCol:aPict     := {}
   oCol:aSize     := {}  // !!! :aSize - ������ ������, ���������� � _TBrowse(...)
   oCol:aName     := {}
   oCol:aFoot     := {}  // .T.
   oCol:aEdit     := {}  // .T.    // ������������� ��� �������
   oCol:aColPrc   := {}
   oCol:aFunc1    := {}
   oCol:aFunc2    := {}
   oCol:aBlock    := {}
   oCol:aDecode   := {}   // ��� ������� oCol:bDecode
   oCol:aTable    := aCol
   oCol:aSupHd    := { "1" + CRLF + "2" + CRLF + "3" }

   FOR EACH a IN aCol
      i     := hb_EnumIndex(a)
      cType := a[3]
      cFld  := a[5]
      AAdd(oCol:aHead , StrTran(a[4], ";", CRLF)  )

      nWCol := GetFontWidth(oCol:aFont[1], LEN(a[6])) * 0.85  // "Normal"  [6]=������ ����
      IF cType == "BMP" .OR. cType == "L"
         nWCol := GetFontHeight(oCol:aFont[1])*1.35
         nWCol := IIF( nWCol < 32, 32, nWCol )
      ENDIF
      AAdd(oCol:aSize , nWCol  )  // !!! :aSize - ������ ������, ���������� � _TBrowse(...)
      AAdd(oCol:aName , a[5]   ) // "NAME_"+HB_NtoS(i) )
      AAdd(oCol:aFoot , a[5]   ) // "NAME_"+HB_NtoS(i) )
      //AAdd(oCol:aPict , a[7] )
      c8Col := a[8]   // �������� �� ���� - ���������
      IF !IsString(c8Col)
         c8Col := ''
      ENDIF
      // ����� ����� ��� ����� ���� � ������� aKey[?]
      bBlock := ''
      IF LEN(c8Col) > 0
         lFind := .F.
         FOR j := 1 TO LEN(aKey)
            IF UPPER(c8Col) == UPPER(aKey[j,1])
               bBlock := aKey[j,2]
               lFind := .T.
               EXIT
            ENDIF
         NEXT
         IF !lFind
            cErr += ";��� �����: " + c8Col + ";"
         ENDIF
      ENDIF
      // ������� ���� ����
      AAdd(oCol:aBlock, bBlock )
      // ������� ���� ����
      AAdd(oCol:aField, upper(cFld) )

      lEdit := IIF( a[2] == "W", .T., .F. )  // ������ � �������: "R" ������, "W" ������
      AAdd(oCol:aEdit, lEdit  )

      // ��� ���������� ���������
      AAdd(oCol:aColPrc , cType    )  // ��� ��������� �������: "BMP", "K", "S", "C", "N", "D"
      AAdd(oCol:aFunc1  , a[9]     )  // �������-1 :bPrevEdit ��� ��������� ������� �������
      AAdd(oCol:aFunc2  , a[10]    )  // �������-2 :bPostEdit ��� ��������� ������� �������
      AAdd(oCol:aDecode , aDcd[i]  )  // ��� ������� oCol:bDecode
   NEXT
   //
   IF LEN(cErr) > 0
      IF App.Cargo:cLang == "RU"
      cMsg := "������ !;"
      cMsg += "����� ����� ��� ����� ���� � ������� aKey[?];;"
      ELSE
      cMsg := "ERROR!;"
      cMsg += "Searching for a key for a code block in the array aKey[?];;"
      ENDIF
      cMsg += ProcNL()
      AlertStop(cMsg + cErr, ProcNL() )
   ENDIF

   oCol:aCol := aCol    // ������ ������� ������� - ��������
   ? ProcNL(), "oCol:aCol=", oCol:aCol ; ?v oCol:aCol
   ? "#############"

RETURN oCol

///////////////////////////////////////////////////////////////////
// ��������� ������� �� ��� ����
FUNCTION Sets_Event2Zaivka()

   WITH OBJECT This.Object
      :Event( 0, {|ow| // ON INIT - ������ ��� ������������� ����
                      This.Topmost := .F.
                      ow:Cargo:oBrw:Setfocus()
                      //ow:Setfocus('Buff')
                      Return Nil
                      } )

      :Event( 2, {|ow| ow:Setfocus('Buff')          })  // ������

      :Event( 19, {|ow| ow:Cargo:oBrw:DrawFooters() })  // ��� ����� ������� ������� - ���������� ������
                                                        // when changing the table cursor - redraw the footer

      // object name + event name:
      // { "_ZHelp", "_ZCard","_ZF3Refresh","_ZF4Calc" ,"_ZF5Print","_ZF7Find" ,"_ZF6Sort",;
      // "_ZF8Menu","_ZF9Config","_ZExit" }

      // �������� ������� � �������, ��. ���� / assign keys in the table, see below
      // oTsb:aUserKeys := { VK_F2, VK_F3, VK_F4, VK_RETURN
      //             _wPost(  32  ,  33  , 34   ,
      :Event({20,"_ZHelp"}, {|ow,ky,cn| // � ���������
                              This.&(cn).Enabled := .F.
                              _SetThisFormInfo(ow)
                              AlertInfo(App.Cargo:cAbout, , "iMg96x1", 64, {ORANGE})
                              _SetThisFormInfo()
                              This.&(cn).Enabled := .T.
                              DO EVENTS
                              ky := cn
                              Return Nil
                              })

      :Event({21,"_ZCard"}, {|ow| // �������� �� ������
                              _wSend(40,, ow:Cargo:oBrw )
                              DO EVENTS
                              Return Nil
                              })

      :Event(40, {|ow,ky,ob| // �������� �� �������
                             Local cn := "_ZCard"
                              This.&(cn).Enabled := .F.
                             _SetThisFormInfo(ow)
                             // ���������� ������� � ���������
                             ob:nCLR_Gray  := HMG_RGB2n({184,196,55})
                             ob:nCLR_HGray := HMG_RGB2n({184,196,55})
                             ob:nCLR_Lines := CLR_YELLOW
                             ob:Enabled(.F.)
                             Card_for_table(ow,ky,ob,"iMaster48","�ard for table-1",{97,209,187})
                             //MsgDebug(ow:Name,ky,ob:cAlias )
                             _SetThisFormInfo()
                             ob:Enabled(.T.)
                             This.&(cn).Enabled := .T.
                             DO EVENTS
                             ky := cn
                             Return Nil
                             })

      :Event({50,"_TsbRClick"}, {|ow,ky,xc| // Right click on the cursor in the table
                                  Local cm, ob, p1, p2, p3, nAt
                                  DO EVENTS
                                  cm := ProcNL()
                                  ?  cm, "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                                  ?? "������ ���� ����� �� ������� � �������"
                                  ?  Repl(".", 10), ow:Name, "{p1,p2,p3,ob}="
                                  p1 := xc[1] ; p2 := xc[2] ; p3 := nAt := xc[3] ; ob := xc[4]
                                  ?? "{", p1, p2, p3, ob, "}", ob:cAlias
                                  // p1, p2 - ���������� �����, p3=nAt, ob = ������ �������
                                  // p1, p2 - click coordinates, p3=nAt, ob = table object
                                  nAt := ob:nAt
                                  ? "   ����� [nAt]=", nAt //, ":aArray[nAt]=", ob:aArray[nAt]
                                  //?v ob:aArray[nAt]
                                  //? "------------------"
                                  _SetThisFormInfo(ow)
                                  //Tsb_RClickContexMenu(ob, ob:aArray[nAt], nAt)  // -> tsb_EditWindows.prg
                                  MsgDebug(nAt, (ob:cAlias)->( RecNo() ) )
                                  _SetThisFormInfo()
                                  DO EVENTS
                                  Return Nil
                                  })

      :Event(90, {|ow| // ON RELEASE  {|| AEval({91,92,93}
                       Local n
                       FOR EACH n IN { 91,92,93 }
                          _wSend(n, ow)
                          DoEvents()
                       NEXT
                       Return Nil
                       })

      :Event(91, {|ow,ky| // ON RELEASE
                         Local ah := ow:Cargo:ahIcoDel
                         Local cFormCurr   := ow:Cargo:cFormCurr        // ������������ ����
                         Local cBtnEnabled := ow:Cargo:cBtnEnabled      // �������������� ������
                         _LogFile(.T., "  -->> :Event(",ky,") ON RELEASE WINDOW: "+ow:Name )
                         ? SPACE(4) + ">>> Delete handle icon - ow:Cargo:ahIcoDel="
                         ?? ah, HB_ValToExp(ah)
                         IF IsArray(ah)
                            AEval(ah, {|h| DestroyIcon(h) })  // ������� ������ ������
                         Endif
                         // ������� ������� ����
                         Domethod(cFormCurr, "Restore" )
                         SetProperty(cFormCurr, "Btn_Exit", "Enabled" , .T. )
                         SetProperty(cFormCurr, cBtnEnabled, "Enabled" , .T. )
                         Return Nil
                         } )

     :Event(92, {|ow,ky| _LogFile(.T., "  -->> :Event(",ky,") ON RELEASE WINDOW: "+ow:Name ) } )
     :Event(93, {|ow,ky| _LogFile(.T., "  -->> :Event(",ky,") ON RELEASE WINDOW: "+ow:Name ) } )


     :Event(97, {|ow| _LogFile(.T., "  -->> :Event(97) Exit: "+ow:Name ), _wPost(99, ow:Name) } )

     :Event({98,"_ZExit"}, {|ow,ky,cn| // �������� ����
                              Local cwn := ow:Name
                              ? ProcNL(), ":Event(98)=", cwn, ky, cn
                              IF _IsWindowDefined(cwn)
                                 SetProperty(cwn, "OnInterActiveClose", {|| NIL })
                                 DoMethod(cwn, "Release")
                                 DO EVENTS
                              ENDIF
                              Return Nil
                              } )

     // �������� ���� ��� ����������
     :Event(99, {|ow| ow:Release()  } )

   END WITH

RETURN NIL

////////////////////////////////////////////////////////////
FUNCTION myFldTime()   // ����/����� ������
   LOCAL cAls := ALIAS(), cRet := DTOC((cAls)->DATEVVOD)+' '
   LOCAL cTime := HB_NToS( (cAls)->TIMEVVOD )

   cTime := PADL(cTime,4,"0")
   cTime := SUBSTR(cTime,1,2) + ":" + SUBSTR(cTime,3)
   cRet  += cTime

RETURN cRet

/////////////////////////////////////////////////////////////////////////////
FUNCTION LoadDimZaivCard(nPage)
   LOCAL aDim := {}, nI, nK, aVal, aName, aHead, cErr, cMsg, oTipVid, oTipVd
   LOCAL oVipZa, oSrkZa, oProper, aSpr, aFld, aSFld, cSay

   App.Cargo:cTsbVersion := "11.04.2025"  // ��� ��������� ������� ����� ������� - �������� !!!
                                          // when changing the array of table rows - CHANGE !!!
   //                1            2               3                  4                     5                6                  7                  8                   9           10           11             12              13           14           15
   aName := { "RNAME"        , "REDIT"      , "F_NN"           ,  "F_PROCES"          , "F_BASE"    , "F_READ"          , "F_WRITE"         , "F_WINDOWS"      , "F_ACCESS"  , "V_CALC"    , "V_SPR"    ,  "V_AADD"      , "F_CALC"  , "F_SPR"     ,  "F_AADD"    }
   aHead := { "������������" , "��������"   , "�������;������" , "�������;���������"  , "����;����" , "�������;������"  , "�������;������"  , "�������;����"   , "Write;Read", "����� CALC", "�����;SPR", "�����;���.���", "��� CALC", "������;SPR", "���.������" }
   aHead := { "Name"         , "Value"      , "Order;show"     , "Processing;function", "Base;field", "Reading;function", "Writing;function", "Window;function", "Write;Read", "Show;CALC" , "Show;SPR" , "Show;add.data", "For CALC", "SPR;data"  , "Add.data"   }
   // (1)  - ������� ������
   // (2)  - ������� ������, ������ ��������
   // ----- ����� ������� ������ ---------------
   // (3)  - ������� ������ ����� � �������
   // (4)  - ��� ��������� ����� �������
   // (5)  - ���� ���� � dbf
   // (6)  - ������� ������ ���� (4) � ������ � ������� (2)
   // (7)  - ������� ������ � ���� (4) �� ������� (2) ��� (8)-��� NIL
   // (8)  - ������� ��� ���� �������������� ���������� ��� ���� CALC,SPR_A,SPR_J,SPR_S
   // (9)  - ������ �������������� ����� Write/Read
   // (10) - �������������� � "C" ������� (12)
   // (11) - �������������� � "C" ������� (13)
   // (12) - �������������� � "C" ������� (14)
   // (13) - �������� ������������� ���� {} ��� ���� CALC,SPR_A,SPR_J,SPR_S �� (3), � ��������� ������� NIL
   // (14) - ���.������ ��� ���� (3): SPR_A,CALC,SPR_J,SPR_S,CALC
   // (15) - ���.������ ������
   //
   // (1) - display column
   // (2) - display column, value editing
   // -------- hide further columns ---------------------
   // (3) - the order of displaying rows in the table
   // (4)  - table cell processing type
   // (5) - base field in dbf
   // (6) - function reading field (4) and writing to column (2)
   // (7) - function writing to field (4) from column (2) or (8) - where NIL
   // (8) - function for variable editing window for type CALC,SPR_A,SPR_J,SPR_S
   // (9) - Write/Read cell editing access
   // (10)- conversion to "C" of column (12)
   // (11) - conversion to "C" of column (13)
   // (12) - conversion to "C" of column (14)
   // (13) - value of corrected field {} for type CALC,SPR_A,SPR_J,SPR_S from (3), in other cases NIL
   // (14) - additional data for type (3): SPR_A,CALC,SPR_J,SPR_S,CALC
   // (15) - additional data different
 IF nPage == 1
   //                      1                         2            3     4        5               6                 7              8                9   10  11   12  13  14   15
   AADD( aDim, {"Variable checks"                  , ""        ,  1, "LINE1" , ""         , ""               , ""              , ""             , "R", "", "", "", nil, {} , "" } )
   AADD( aDim, {"   Type C field test"             , SPACE(20) ,  2, "C"     , "FIO"      , ""               , ""              , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"   Type L field test"             , .F.       ,  3, "L"     , "lPrint"   , ""               , ""              , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"   Type M (memo) field test"      , SPACE(90) ,  4, "M"     , "MREM"     , ""               , ""              , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"   Test of field type N"          , 0         ,  5, "N"     , "NNZa"     , ""               , ""              , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"   Test of field type D"          , CTOD("")  ,  6, "D"     , "DateDog"  , ""               , ""              , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"   Test of field type D calendar" , CTOD("")  ,  7, "DMN"   , "DatePlan" , ""               , ""              , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"   Test of field of type T"       , ""        ,  8, "DT"    , "IMZ"      , ""               , ""              , ""             , "W", "", "", "", nil, {} , "" } )

 ELSEIF nPage == 2

   AADD( aDim, {"Checks of composite variables"    , ""        ,  9, "LINE2" , ""         , ""               , ""              , ""             , "R", "", "", "", nil, {} , "" } )
   AADD( aDim, {"   Subscriber's apartment N + C"  , SPACE(20) , 10, "A"     , {"NKvar","CKvar"}, ""         , ""              , ""             , "W", "", "", "", nil, {"@Z 99999","xxxxx", "/" } , {"Number:","Letter:"} } )
   AADD( aDim, {"   Phone (home/mobile/work)"      , SPACE(50) , 11, "A"     , {"TelFIO","TelFIO3","TelFIO2"}, ""    , ""      , ""             , "W", "", "", "", nil, {"@R (999) 999-99-99","@R (999) 999-99-99","@R (999) 999-99-99", " , "} , "" } )
   AADD( aDim, {"   Select from array - type SPR_A", SPACE(20) , 12, "SPR_A" , "KDispJil" , "GetDim2Str()"   , ""              , ""             , "W", "", "", "", nil, {"Tenant","Dispatch","Rostelecom","ODS (obshch.disp.service)","City 77"}, "iVOTelefon48" } ) // 15-icon
   AADD( aDim, {"   Select from array in this line", SPACE(20) , 13, "SPR_A" , "KOnline"  , "GetDim2Str()"   , ""              , ""             , "W", "", "", "", nil, {"Portal" ,"other", "https://www.hmgextended.com/" } , "iUser32"} ) // 15-icon

   // ����������� ��� ���������� ����������� "CALC", ��� ������ �� ����
   oTipVd := oHmgData()
   oTipVd:aType := { "S"       , "S"        }
   oTipVd:aName := { "Bid type", "Bid kind" }
   oTipVd:aTFor := { {"tipza","Ktipza","tipza",2,""} , {"Works","KWorks","Works",2,""} }
   oTipVd:aFld  := {"Ktipza","KWorks"}     // ���� ������ � ����
   oTipVd:aBClr := GREEN   // ���� ������
   oTipVd:aFClr := BLUE                                                                     // V--- ���-�� = oTipVid:aFld                    //  9   10  11   12  13       14        15
   AADD( aDim, {"   Bid: type / kind"              , SPACE(20) , 13, "CALC"  , ""         , "Za_TipVid2()"   , "SetDim2Wrt()"  , "myWinCalc()"  , "W", "", "", "", nil, oTipVd:aFld, oTipVd } )

   // ����������� ��� ���������� ����������� "CALC", ��� ������ �� ����
   oTipVid := oHmgData()
   oTipVid:aType := { "S"               , "S"         , "D"                   , "N"             , "C"       , "A"                , "M"          }
   oTipVid:aName := { "Application type", "Condition" , "Application deadline", "Payment amount", "Comment" , "Print application", "Memo-field" }
   oTipVid:aTFor := { {"tipza","Ktipza","tipza",2} , {"Works","KWorks","Works",2} , {"D"}, {"N"}, {"C"} , {"Master","Manager","Warehouse"}, {"M"} }
   oTipVid:aFld  := {"Ktipza","KWorks","DatePere","SumPlata","Dispetch","KAdres1" ,"mAktVip"}   // ���� ������ � ����
   oTipVid:aIcon := {""      ,""      ,""        ,""        ,""        ,"iPrint32", ""      }   // ������ � ����������� ����
   oTipVid:aBClr := GREEN   // ���� ������
   oTipVid:aFClr := BLUE                                                                    // V--- ���-�� = oTipVid:aFld                //  8    9  10  11   12    13         14     15
   AADD( aDim, {"   Application Type/Miscellaneous" , SPACE(20) , 14, "CALC"  , ""         , "Za_TipVid()"    , "SetDim2Wrt()"  , "myWinCalc()"  , "W", "", "", "", nil, oTipVid:aFld, oTipVid } )
   // ---------------------
   // ����������� ��� ���������� ����������� "CALC", ��� ������ �� ����
   oSrkZa := oHmgData()
   oSrkZa:aName := {"Type of urgency", "Deadline" }
   oSrkZa:aType := {"S","D"}
   oSrkZa:aTFor := { {"srokza","Ksrokza","srokza",2} , {"D"} }
   oSrkZa:aCod1 := { 1, 2, 3, 4, 7, 5, 6, 10, 20, 30 }
   oSrkZa:aVal1 := { "1-day", "2-days", "3-days", "4-days", "7-days", "out of turn (4 hours)", "current day (until 17:30)", "repair (10-days)", "repair (20-days)", "end of current month" }
   oSrkZa:aXArr := { {oSrkZa:aCod1,oSrkZa:aVal1} , {}  }        // ��� �������� �� ����� Dbf + ����
   oSrkZa:aBClr := BLUE      // ���� ������
   oSrkZa:aFClr := MAROON    // PINK  PURPLE
   oSrkZa:aFld  := {"Ksrokza","DateSrok" }  // ���� ������ � ����                                                                         //     9  10  11   12  13        14        15
   AADD( aDim, {"   (*) Request urgency type"       , SPACE(20) , 15, "CALC"  , ""         , "Za_Srokza()"  , "SetDim2Wrt()" , "myWinCalc2()" , "W", "", "", "", nil, oSrkZa:aFld , oSrkZa } )
   // ---------------------
   AADD( aDim, {"   Request overdue"                , SPACE(20) , 16, "CALC"  , ""         , "Za_ProSrok()" , ""             , ""             , "R", "", "", "", nil, {} , nil } )
   AADD( aDim, {"   (*) Electronics Master (dbf)"   , SPACE(20) , 17, "SPR_S" , "KMaster"  , "Spr_1Dbf()"   , ""             , "Spr_2Dbf()"   , "W", "", "", "", nil, {"Master","KMaster","Master",2,"KFIRMA==1"}, "iMaster48" } ) // 15-icon
   AADD( aDim, {"   (*) Gate Master (dbf)"          , SPACE(20) , 18, "SPR_S" , "KMaster0" , "Spr_1Dbf()"   , ""             , "Spr_2Dbf()"   , "W", "", "", "", nil, {"Master","KMaster","Master",2,"KFIRMA==2"}, "iMaster48" } ) // 15-icon

   oProper := oHmgData()
   oProper:aType := { "A"          , "A"            , "A"                , "A"         , "A"               , "A"             }
   oProper:aName := { "Type of repair", "Type of installation", "Disconnecting the handset", "Technical inspection", "Pre-dial", "Print request" }
   oProper:aTFor := { {"entrance","apartment"} , {"regular", "at the company's expense"} , {"disable","enable"} , {" yes","no"} , {" yes","no"} , {" yes","no"} }
   oProper:aFld  := {"KZ_PKV","KTipUst","KodTruba","KTexOsm","KZvonok","KPrint"}                  // ���� ������ � ����
   oProper:aIcon := {"iMaster48","iVOTelefon48","iUser32","iMg_Ok48x2","iMg_Ok48x1","iPrint32"}   // ������ � ����������� ����
   oProper:aBClr := GREEN   // ���� ������
   oProper:aFClr := BLUE                                                                                                                     //   9   10  11  12  13         14        15
   AADD( aDim, {"   Application properties"         , SPACE(20) , 20, "CALC"  , ""         ,"Zaiv_PropDbf()", "SetDim2Wrt()"  , "myWinCalc()"  , "W", "", "", "", nil, oProper:aFld, oProper } )
   // ---------------------------------------------------------------
   oVipZa := oHmgData()
   oVipZa:aName := {"Type of execution", "Date of execution", "Time of execution", "Submit to site"}
   oVipZa:aType := {"S","D","N","N" }
   oVipZa:aPict := { Repl("x", 22), "@D", "@Z 99:99", "@Z 999" }
   oVipZa:aTFor := { {"VipZa","KVipZa","VipZa",2,""} , {}, {}, {} }
   oVipZa:aBClr := BLUE      // ���� ������
   oVipZa:aFClr := MAROON    // PINK  PURPLE
   oVipZa:aFld  := {"Ksrokza", "DATEVip", "TimeVip", "INET" }  // ���� ������ � ���� - "INET" ���� ��� �������� �� ����
   AADD( aDim, {"   (*) Request execution type"     , SPACE(20) , 21, "CALC"  , ""         , "Za_VipZa()"     , "SetDim2Wrt()"  , "myWinCalc3()" , "W", "", "", "", nil, oVipZa:aFld , oVipZa  } )

 ELSEIF nPage == 3
   AADD( aDim, {"Example of a database card:"       , ""        , 22, "LINE1" , ""         , ""             , ""             , ""             , "R", "", "", "", nil, {} , "" } )
   AADD( aDim, {"1.1.The lock is faulty"            , .F.       , 24, "L"     , "Nele6"    , "GetNum2Log()" , "SetLog2Num()" , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"  2.Adjusting the door closer"     , .F.       , 25, "L"     , "Nele7"    , "GetNum2Log()" , "SetLog2Num()" , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"  3.Magnet does not hold the door" , .F.       , 26, "L"     , "Nele10"   , "GetNum2Log()" , "SetLog2Num()" , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"2.1.Panel - E2"                    , .F.       , 27, "L"     , "Nele1"    , "GetNum2Log()" , "SetLog2Num()" , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"  2.Panel - Error"                 , .F.       , 28, "L"     , "Nele2"    , "GetNum2Log()" , "SetLog2Num()" , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"  3.Panel - Other"                 , .F.       , 29, "L"     , "Nele9"    , "GetNum2Log()" , "SetLog2Num()" , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"3.1.Intercom: not working"         , .F.       , 30, "L"     , "Nele8"    , "GetNum2Log()" , "SetLog2Num()" , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"  2.Intercom: other"               , SPACE(32) , 31, "C"     , "NeleRem3" , ""             , ""             , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"4.1.Keys: do not open the entrance", .F.       , 32, "L"     , "Nele3"    , "GetNum2Log()" , "SetLog2Num()" , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"  2.Keys: recoding"                , .F.       , 33, "L"     , "Nele5"    , "GetNum2Log()" , "SetLog2Num()" , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"  3.Keys: order (pieces)"          , SPACE(10) , 34, "C"     , "cNele4"   , ""             , ""             , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"5.Codes do not open the entrance"  , .F.       , 35, "L"     , "Nele4"    , "GetNum2Log()" , "SetLog2Num()" , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"6.1.Subscriber device: not working", .F.       , 36, "L"     , "NeleAbo3" , "GetNum2Log()" , "SetLog2Num()" , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"  2.Subscriber device: entrance"   , SPACE(23) , 37, "C"     , "NeleAbo2" , ""             , ""             , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"  3.Subscriber device: apartment"  , SPACE(23) , 38, "C"     , "NeleAbo1" , ""             , ""             , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"7.Additional tube installation:"   , SPACE(33) , 40, "C"     , "NeleDou1" , ""             , ""             , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"8.Create code for apartment"       , SPACE(4)  , 41, "C"     , "cNele11"  , ""             , ""             , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"9.Cost"                            , SPACE(22) , 42, "C"     , "NeleDou3" , ""             , ""             , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"10.1.Other"                        , SPACE(36) , 43, "C"     , "NeleRem1" , ""             , ""             , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"   2.Other"                        , SPACE(36) , 44, "C"     , "NeleRem2" , ""             , ""             , ""             , "W", "", "", "", nil, {} , "" } )
   AADD( aDim, {"   3.Other"                        , SPACE(36) , 45, "C"     , "NeleRem4" , ""             , ""             , ""             , "W", "", "", "", nil, {} , "" } )

 ELSEIF nPage == 4
   // ����� �� �����������
   aSpr := {"Sprav","KNNEISPR","NNEISPR",2,"KVIEW==1"}
   aFld := {"KDfc1","KDfc2","KDfc3","KDfc4","KDfc5","KDfc6","KDfc7","KDfc8","KDfc9","KDfc10" }  // ���� ������ � ����
   AADD( aDim, {"Bid: number, date, time"           , ""        , 23, ""      , ""         , "GetNumZa()"   , ""             , ""             , "R", "", "", "", nil, {} , "" } )
   AADD( aDim, {"Table-4: List of faults"           , ""        , 46, "LINE1" , ""         , ""             , ""             , ""             , "R", "", "", "", nil, {}  , ""   } )
   AADD( aDim, {"Troubleshooting guide"             , SPACE(60) , 47, "CALC"  , "aDefect"  , "ZaivSay()"    , "ZaWrtDfc()"   , "ZaListNeis()" , "W", "", "", "", nil, aFld, ""   } )
   AADD( aDim, {""                                  , SPACE(90) , 48, "K"     , "KDfc1"    , ""             , ""             , ""             , "R", "", "", "", nil, aSpr, ""   } )
   AADD( aDim, {""                                  , SPACE(90) , 49, "K"     , "KDfc2"    , ""             , ""             , ""             , "R", "", "", "", nil, aSpr, ""   } )
   AADD( aDim, {""                                  , SPACE(90) , 50, "K"     , "KDfc3"    , ""             , ""             , ""             , "R", "", "", "", nil, aSpr, ""   } )
   AADD( aDim, {""                                  , SPACE(90) , 51, "K"     , "KDfc4"    , ""             , ""             , ""             , "R", "", "", "", nil, aSpr, ""   } )
   AADD( aDim, {""                                  , SPACE(90) , 52, "K"     , "KDfc5"    , ""             , ""             , ""             , "R", "", "", "", nil, aSpr, ""   } )
   AADD( aDim, {""                                  , SPACE(90) , 53, "K"     , "KDfc6"    , ""             , ""             , ""             , "R", "", "", "", nil, aSpr, ""   } )
   AADD( aDim, {""                                  , SPACE(90) , 54, "K"     , "KDfc7"    , ""             , ""             , ""             , "R", "", "", "", nil, aSpr, ""   } )
   AADD( aDim, {""                                  , SPACE(90) , 55, "K"     , "KDfc8"    , ""             , ""             , ""             , "R", "", "", "", nil, aSpr, ""   } )
   AADD( aDim, {""                                  , SPACE(90) , 56, "K"     , "KDfc9"    , ""             , ""             , ""             , "R", "", "", "", nil, aSpr, ""   } )
   AADD( aDim, {""                                  , SPACE(90) , 57, "K"     , "KDfc10"   , ""             , ""             , ""             , "R", "", "", "", nil, aSpr, ""   } )
   AADD( aDim, {"Malfunctions note"                 , SPACE(90) , 58, "M"     , "MDefect"  , ""             , ""             , ""             , "W", "", "", "", nil, {}  , ""   } )

 ELSEIF nPage == 5
   AADD( aDim, {"Bid: number, date, time"           , ""        ,  1, ""      , ""         , "GetNumZa()"   , ""             , ""             , "R", "", "", "", nil, {} , "" } )
   AADD( aDim, {"Table-5: Equipment list"           , ""        ,  2, "LINE2" , ""         , ""             , ""             , ""             , "R", "", "", "", nil, {}  , ""   } )
   // ---------------------
   // "MKob4or"  - [2,35,277,2.00;2,31,244,1.00;1,2,141,1.00;]  // ���� ������ � ���� ����� ������������ �� ������ ���������
   aSFld := { {"SumVsego","SumWObor","SumMaster"} , { 0,0,0} }                                                                              //   9   10  11  12  13    14    15

   AADD( aDim, {"(*) Works completed/Equipment"     , SPACE(60) ,  3, "CALC"  , "MOb4orud" , "ZA_MOb4Err()" , "Set_ZAMOb4()" , "Tovar_HMG()"  , "W", "", "", "", nil, aSFld, ""   } )
   AADD( aDim, {""                                  , ""        ,  4, ""      , ""         , "myMemo['MOb4orud',99,2]", ""   , ""             , "R", "", "", "", nil, {}   , ""   } )
   AADD( aDim, {""                                  , ""        ,  5, ""      , ""         , "myMemo['MOb4orud',99,3]", ""   , ""             , "R", "", "", "", nil, {}   , ""   } )

   AADD( aDim, {""                                  , ""        ,  6, ""      , ""         , ""               , ""           , ""             , "R", "", "", "", nil, {} , ""  } )
   AADD( aDim, {""                                  , ""        ,  7, ""      , ""         , ""               , ""           , ""             , "R", "", "", "", nil, {} , ""  } )
   // ---------------------
#ifdef KEY_ENG // for this project demo1-en.hbp
   cSay := "  ** List apartments (checkerboard)"
#else
   cSay := "  ** ������ ������� (��������)"
#endif
   aFld := {"NAKTVIP","MAKTVIP"}  // {"C","M"} ���� ������ � ���� / database entry fields
   AADD( aDim, {cSay                                , SPACE(20) , 66, "CALC"  , "MAKTVIP"  , "Get_Fld64()"  , "Set_Fld64()"  , "Win_Fld64()"  , "W", "", "", "", nil, aFld, "Run_Line64()" } )

 ELSEIF nPage == 6
   AADD( aDim, {"Bid: number, date, time"           , ""        ,  1, ""      , ""         , "GetNumZa()"   , ""             , ""             , "R", "", "", "", nil, {} , "" } )
   AADD( aDim, {"Table-6: reserve"                  , ""        ,  2, "LINE2" , ""         , ""             , ""             , ""             , "R", "", "", "", nil, {}  , ""             } )
   AADD( aDim, {"  reserve"                         , ""        ,  3, ""      , ""         , ""               , ""           , ""             , "R", "", "", "", nil, {} , ""  } )
   AADD( aDim, {"  reserve"                         , ""        ,  4, ""      , ""         , ""               , ""           , ""             , "R", "", "", "", nil, {} , ""  } )
   AADD( aDim, {"  reserve"                         , ""        ,  5, ""      , ""         , ""               , ""           , ""             , "R", "", "", "", nil, {} , ""  } )
   AADD( aDim, {"  reserve"                         , ""        ,  6, ""      , ""         , ""               , ""           , ""             , "R", "", "", "", nil, {} , ""  } )

 ELSEIF nPage == 7
   AADD( aDim, { "Table-????:"                     , ""        ,  1, "LINE1" , ""         , ""               , ""              , ""             , "R", "", "", "", nil, {} , ""  } )
 ENDIF

   cErr := ""
   FOR nI := 1 TO LEN(aDim)
      aVal := aDim[nI]
      nK   := LEN(aVal)
      IF nK # 15
         cErr += HB_NtoS(nI) + ") LEN()="
         cErr += HB_NtoS(LEN(aVal))
         cErr += " aDim=" + HB_ValToExp(aVal)
         cErr += CRLF
      ENDIF
   NEXT
   IF LEN(cErr) > 0
      cMsg := "������ ��� �������� ������� !" + CRLF
      cMsg += "Error creating array!" + CRLF
      cMsg += cErr + CRLF + CRLF
      cMsg += ProcNL() + CRLF + ProcNL(1)
      AlertStop(cMsg, "ERROR!", , 64, {RED})
      ? cMsg
   ENDIF
   // �������� ��� ������ ������� � 14 ��������� � ������ �� �����
   FOR nI := 1 TO LEN(aDim)
       IF Len(aDim[nI]) < 11 ; ASize(aDim[nI], 11)
       ENDIF
       IF Len(aDim[nI]) < 12 ; ASize(aDim[nI], 12)
       ENDIF
       IF Len(aDim[nI]) < 13 ; ASize(aDim[nI], 13)
       ENDIF
       IF Len(aDim[nI]) < 14 ; ASize(aDim[nI], 14)
       ENDIF
       IF Len(aDim[nI]) < 15 ; ASize(aDim[nI], 15)
       ENDIF
   NEXT

RETURN { aDim, aName, aHead }


