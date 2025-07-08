/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
*/

#include "hmg.ch"
#include "tsbrowse.ch"

REQUEST DBFCDX


FUNCTION Main()
   LOCAL cFont := "Arial"
   LOCAL nSize := 12
   LOCAL cForm := "wMain"
   LOCAL cIni  := "CONFIG"
   LOCAL cBuf, oIni, oCom
   LOCAL nY, nX, nH, nW, cLog, hSpl

   SET CODEPAGE TO RUSSIAN
   SET LANGUAGE TO RUSSIAN

   RddSetDefault("DBFCDX")

   SET OOP ON

   SET EPOCH   TO 2000
   SET DATE  TO GERMAN
   SET CENTURY ON
   SET AUTOPEN OFF
   SET DELETED OFF

   SET NAVIGATION EXTENDED
   SET DEFAULT ICON TO "1MAIN_ICO"

   cBuf := RCDataToMem( cIni )  // reading ini file from resources to buffer
   oIni := TIniData():New( , .T., , , cBuf ):Read()
   oCom := oIni:COM

   oIni:Read( ".\demo_new.ini" )  // reading additional settings from ini file on the disk

   App.Cargo := oHmgData()

   App.Cargo:cIni := cIni
   App.Cargo:oIni := oIni

   Default oCom:FontName := cFont, ;
           oCom:FontSize := nSize, ;
           oCom:LogName  := ".\Msg.log"

   cFont := oCom:FontName
   nSize := oCom:FontSize
   cLog  := oCom:LogName

   SET LOGFILE TO ( cLog ) ; fErase( cLog )

   SET FONT TO cFont, nSize
   // фонт по default для oTsb1, oBrw1
   DEFINE FONT Normal FONTNAME cFont SIZE nSize
   DEFINE FONT Bold  FONTNAME cFont SIZE nSize BOLD
   DEFINE FONT Italic FONTNAME cFont SIZE nSize - 2 ITALIC

   nW := 800
   nH := 120
   nY := 0
   nX := Int( ( System.ClientWidth - nW ) / 2 )

   DEFINE WINDOW wMain AT nY, nX WIDTH nW HEIGHT nH ;
      TITLE "MiniGUI TsBrowse Demo2. Ini value => _Tbrowse()" ;
      MAIN  NOMAXIMIZE  NOSIZE ;
      ON RELEASE  dbCloseAll()

      DEFINE STATUSBAR BOLD
         STATUSITEM ''                    ACTION Nil
         STATUSITEM '' WIDTH Int(nW * 0.2)  ACTION Nil
         STATUSITEM '' WIDTH Int(nW / 3  )  ACTION Nil
      END STATUSBAR

      DEFINE SPLITBOX HANDLE hSpl
      DEFINE TOOLBAR ToolBar_1 CAPTION ""          BUTTONSIZE 72,32 FLAT
         BUTTON 01  CAPTION 'Base 1'  PICTURE 'n1' ACTION _wPost(1)  SEPARATOR
         BUTTON 02  CAPTION 'Base 2'  PICTURE 'n2' ACTION _wPost(2)  SEPARATOR
      END TOOLBAR
      DEFINE TOOLBAR ToolBar_2 CAPTION ""          BUTTONSIZE 42,32 FLAT
         BUTTON 99  CAPTION 'Exit'  PICTURE 'exit' ACTION _wPost(99)
      END TOOLBAR
      END SPLITBOX

      This.Height := This.StatusBar.Height + GetWindowHeight(hSpl) + ;
                     GetTitleBarHeight()  + GetBorderHeight()

      WITH OBJECT This.Object                       // ---- Window events
      :Event( 01, {|ow| Base_View(ow, "Brw_1", "CUST1") } )
      :Event( 02, {|ow| Base_View(ow, "Brw_2", "CUST2") } )
      :Event( 99, {|ow| ow:Release() } )
      END WITH                                      // ---- Window events

   END WINDOW

   wMain.Activate

RETURN Nil

FUNCTION Base_View( oWnd, cBrw1, cAlias1, cDbf1 )
   LOCAL oIni  := App.Cargo:oIni
   LOCAL cForm := "w"+cBrw1
   LOCAL oBrw1, nY, nX, nH, nW, oTsb1, aTmp
   Default cDbf1 := "CUSTOMER"

   SET WINDOW THIS TO oWnd:Name

   USE ( cDbf1 )  ALIAS ( cAlias1 )  NEW SHARED

   DEFINE WINDOW &cForm TITLE "Demo ini => TBrowse. [ "+cForm+" ]" MODAL NOSIZE ;
          ON INIT  NIL ;
          ON RELEASE ( (This.Cargo:oBrw:cAlias)->( dbCloseArea() ) )
          This.Maximize
          This.Cargo := oHmgData()

      oTsb1 := oIni:&(cBrw1)

      oTsb1:oText := oIni:&(cBrw1+"_Text")

      oTsb1:aUserKeys := {}
      FOR EACH aTmp IN oIni:&(cBrw1+"_Keys"):GetAll()
          AADD( oTsb1:aUserKeys, {Val(aTmp[1]), aTmp[2]} )
      NEXT

      oTsb_Init( oTsb1 )

      oBrw1 := _TBrowse( oTsb1, cAlias1, cBrw1, nY, nX, nW, nH )

      oBrw1:SetFocus() ; DO EVENTS

      This.Cargo:cBrw := cBrw1
      This.Cargo:oBrw := oBrw1

      ON KEY ESCAPE ACTION iif( oBrw1:IsEdit, oBrw1:SetFocus(), ThisWindow.Release )
      ON KEY F1 ACTION NIL

      FOR EACH aTmp IN oIni:&(cBrw1+"_Event"):GetAll()
          (This.Object):Event( Val(aTmp[1]), aTmp[2] )
      NEXT

   END WINDOW

   //CENTER WINDOW &cForm
   ACTIVATE WINDOW &cForm

   SET WINDOW THIS TO

RETURN NIL

FUNCTION oTsb_Init( oTsb )

   oTsb:bBody := {|ob,op|
        Local oCol, aTxt, nTxt, lEditNo, nTmp, aTmp
        lEditNo := HB_ISARRAY(op:aEditNo) .and. Len(op:aEditNo) > 0
        FOR EACH oCol IN ob:aColumns
            oCol:hFont := GetFontHandle( op:aFont[1] )
            IF oCol:cFieldTyp == "C"
               oCol:cPicture := Nil
               oCol:nWidth := oCol:ToWidth( iif( oCol:nFieldLen > 50, 50, oCol:nFieldLen ), 0.82 )
            ELSEIF oCol:cFieldTyp == "M"
               oCol:cPicture := Nil
               oCol:nWidth := oCol:ToWidth(40)
            ELSEIF oCol:cFieldTyp == "D"
               oCol:cPicture := Nil
               oCol:nWidth := oCol:ToWidth(10)
            ELSEIF oCol:cFieldTyp $ "@=T"
               oCol:cPicture := Nil
               oCol:nWidth := oCol:ToWidth(24)
            ENDIF
            IF lEditNo .and. AScan(op:aEditNo, oCol:cName) > 0
               oCol:lEdit := .F.
            ELSEIF oCol:cFieldTyp $ "+=^"
               oCol:lEdit := .F.
            ENDIF
        NEXT
        nTmp := 1
        nTxt := 0
        FOR EACH aTxt IN op:oText:GetAll()
            IF ob:nColumn(aTxt[1], .T.) == 0 ; LOOP
            ENDIF
            oCol := ob:GetColumn(aTxt[1])
            IF "\" $ aTxt[2]
               nTxt++
               aTmp := hb_ATokens(aTxt[2], "\")
               nTmp := Max( nTmp, Len(aTmp))
               oCol:cHeading := StrTran(aTxt[2], "\", CRLF)
            ELSE
               oCol:cHeading := aTxt[2]
            ENDIF
        NEXT
        IF nTxt > 0
           ob:nHeightHead := GetFontHeight( op:aFont[2] ) * nTmp
        ENDIF
        Return Nil
       }

RETURN oTsb
