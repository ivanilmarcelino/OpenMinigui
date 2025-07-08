/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2019 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2019 Verchenko Andrey <verchenkoag@gmail.com>
 *
 * События на окне: сообщения с This.Name - создается среда This для контрола
 * События на окне: сообщения с This.index - создается среда This для контрола
 * Events on the window: messages with This.Name - this environment is created for control
*/

#include "hmg.ch"

SET PROCEDURE TO demo_misc

#define  SHOW_TITLE  "Events on the form: CheckLabel"

STATIC nStaticRadioGr
////////////////////////////////////////////////////////////////////
FUNCTION Main()
   LOCAL cFont := 'Tahoma', nFontSize := 20
   LOCAL nI, nY, nX, nW, nH, nG, cN, nCol2, nWrg, nWGbx
   LOCAL aBackColor, nBtnH, nBtnW, n2Line, cObj, cObj2, cText
   LOCAL aFind := {}, aRadioGr := {}, aPict := {}
   LOCAL aRet := {}, aRadioGrNm := {}
   LOCAL cFileIni := ChangeFileExt( Application.ExeName, ".save" )

   SetsEnv()  // loading the program environment -> demo_misc.prg

   nStaticRadioGr := 1
   aBackColor     := SILVER    // Цвет фона всей формы

   // table from CheckLabel
   For nI := 1 to 6
      AAdd( aFind    , "value_"+HB_NtoS(nI)+SPACE(10) )
      AAdd( aPict    , REPL('x',20)                   )
      AAdd( aRadioGr , nI                             )
   Next

   // restore variable values from file
   IniLoadFile(cFileIni, @aFind, @nStaticRadioGr )

   nY := nX := nG := 20; nW := 750; nH := 700

   DEFINE WINDOW wMain AT nY, nX WIDTH nW HEIGHT nH ;
       ICON       "1MAIN_ICO"        ;
       TITLE      SHOW_TITLE         ;
       BACKCOLOR  aBackColor         ;
       MAIN       NOMAXIMIZE NOSIZE  ;
       FONT cFont SIZE nFontSize     ;
       ON INTERACTIVECLOSE (This.Object):Action

       @ nY, 0 LABEL Label_0 WIDTH This.ClientWidth HEIGHT 36   ;
               VALUE "CheckLabel to RadioGroup" BOLD SIZE nFontSize + 4 ;
               FONTCOLOR BLACK TRANSPARENT CENTERALIGN VCENTERALIGN

             cObj := 'No lock _wPost(), _wSend() message'
             nY := 70
             nW := GetTxtWidth( cObj, nFontSize, cFont ) + 50

             @ nY, 140 CHECKBOX Chk_Lock            ;
               CAPTION cObj                         ;
               VALUE   (ThisWindow.Object):Action   ;
               WIDTH nW HEIGHT nFontSize*2          ;
               FONTCOLOR NAVY TRANSPARENT           ;
               ON CHANGE ( (ThisWindow.Object):Action := This.Value, ;
                            MsgDebug("No lock=",(ThisWindow.Object):Action) )

         n2Line := nFontSize*1.4 // между строками
         nWrg   := GetTxtWidth( "Search by 9 column:", nFontSize, cFont ) 
         nY     += 80
         nX     := 70

         FOR nI := 1 TO Len(aFind)

            // -------------- CHECKLABEL to RADIOGROUP ---------------
            cObj  := "RG_" + HB_NtoS(nI)
            AAdd(aRadioGrNm, cObj)
            cText := "Search by " + HB_NtoS(nI) + " column:"

            // 50 ширина картинки
            // 50 is a picture width
            @ nY, nX CHECKLABEL &cObj WIDTH nWrg + 50 HEIGHT 40          ;
              VALUE cText LEFTCHECK                                      ;
              IMAGE { 'CheckT32', 'CheckF32' }                           ;
              FONTCOLOR BLUE BACKCOLOR aBackColor                        ;
              ON MOUSEHOVER Rc_Cursor( "MINIGUI_FINGER" )                ;
              ACTION  {|| _wSend(11, This.Index) }                       ;
              ON INIT {|| This.Cargo   := nI, This.Checked := nI == nStaticRadioGr }

            nCol2 := nX + nWrg + 50 + 10
            cObj2 := "GB_" + HB_NtoS(nI)
            cText := REPL("A",LEN(aPict[nI]))
            nWGbx := GetTxtWidth( cText, nFontSize, cFont )

            @ nY, nCol2 GETBOX &cObj2 VALUE aFind[nI] WIDTH nWGbx HEIGHT nFontSize*2  ;
              PICTURE aPict[nI]                                                       ;
              ON CHANGE {|| _wSend(10, This.Index), This.Value := aFind[This.Cargo] } ;
              ON INIT   {|| This.Cargo := nI, This.Value := aFind[nI] }

            nY += nFontSize*2 + n2Line

         NEXT

        // назначаем на checkbox событие
        // сообщения с This.Name - создается среда This для контрола
        (This.Object):Event(10, {|| aFind[ This.Cargo ] := This.Value   })
        (This.Object):Event(11, {|| ToRadioGroup(aRadioGrNm)            })

       ///////////////////////// button ////////////////////////////
         nX := nG
         nBtnH := 62
         nBtnW := ( This.ClientWidth - nG * 3 ) / 2
         nY := This.ClientHeight - nG - nBtnH

         nI := 98
         cN := 'Btn_' + hb_ntos(nI)
       @ nY, nX BUTTONEX &cN WIDTH nBtnW HEIGHT nBtnH CAPTION 'Save' ICON "iResume48x1" ;
         NOHOTLIGHT NOXPSTYLE HANDCURSOR    ;
         FONTCOLOR WHITE  BACKCOLOR LGREEN  ;
         ON MOUSEHOVER ( This.Backcolor := BLACK , This.Fontcolor := YELLOW, This.Icon := "iResume48x2" ) ;
         ON MOUSELEAVE ( This.Backcolor := LGREEN, This.Fontcolor := WHITE , This.Icon := "iResume48x1") ;
         ACTION _wPost(This.Cargo)     ON INIT {|| This.Cargo := nI }
        // назначаем на этот объект событие nI := 98
        (This.Object):Event(nI, {|ow| aRet := {nStaticRadioGr, aFind[nStaticRadioGr] } ,;
                                      MsgDebug(aRet)                                   ,;
                                      IniSaveFile(cFileIni,aFind,nStaticRadioGr)       ,;
                                      ow:Release() })

         nI := 99
         cN := 'Btn_' + hb_ntos(nI)
         nX := nG + nBtnW + nG
       @ nY, nX BUTTONEX &cN WIDTH nBtnW HEIGHT nBtnH CAPTION 'Cancel' ICON "iCancel48x1" ;
         NOHOTLIGHT NOXPSTYLE HANDCURSOR    ;
         FONTCOLOR WHITE  BACKCOLOR RED     ;
         ON MOUSEHOVER ( This.Backcolor := BLACK, This.Fontcolor := YELLOW, This.Icon := "iCancel48x2" ) ;
         ON MOUSELEAVE ( This.Backcolor := RED  , This.Fontcolor := WHITE , This.Icon := "iCancel48x1") ;
         ACTION _wPost(This.Cargo)     ON INIT {|| This.Cargo := nI }
        // назначаем на этот объект событие nI := 99
        (This.Object):Event(nI, {|ow| aRet := {}, MsgDebug(aRet) , ow:Release() })

   END WINDOW

   CENTER   WINDOW wMain
   ACTIVATE WINDOW wMain ON INIT {|| This.Minimize, This.Restore, ;
                                     This.Label_0.SetFocus }

RETURN aRet

////////////////////////////////////////////////////////////////////
STATIC FUNCTION ToRadioGroup( aRadioGrName )
  LOCAL i

  nStaticRadioGr := This.Cargo

  FOR i := 1 TO Len( aRadioGrName )
      This.&(aRadioGrName[ i ]).Checked := ( i == nStaticRadioGr )
  NEXT

RETURN Nil

////////////////////////////////////////////////////////////////////
// restore variable values from file
Function IniLoadFile(cFileIni, aDim, nChoice)
   LOCAL cStr, aRet

   IF !FILE(cFileIni)
      IniSaveFile(cFileIni,aDim, nChoice)
   ENDIF

   cStr    := hb_MemoRead(cFileIni)
   aRet    := &cStr
   aDim    := aRet[1]
   nChoice := aRet[2]

Return Nil

////////////////////////////////////////////////////////////////////
// save the values of variables to a file
Static Function IniSaveFile(cFileIni, aDim, nChoice)
   LOCAL aSave := { aDim, nChoice }

   HB_MemoWrit( cFileIni, HB_ValToExp(aSave) )

Return Nil
