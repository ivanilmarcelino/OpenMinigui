/*
 * MINIGUI - Harbour Win32 GUI library Demo
*/

#define _HMG_OUTLOG

#include "hmg.ch"
#include "i_winuser.ch"

#define APP_TITLE   'HMG GetBox Demo. '

*-----------------------------
Function Main( FontSize, FontName )
*-----------------------------
   LOCAL a, h, cFontName, nFontSize
   LOCAL cBrwFont, nBrwFont, nStbSize
   LOCAL Width_Keybrd, oGet, oGet2

   If Empty(FontSize)              // Default

      FontSize := Nil
      FontName := Nil

   ElseIf ! Empty(FontSize) .and. ',' $ FontSize

      a := hb_ATokens(FontSize, ',')
      ASize(a, 2)
      FontSize := a[1]
      FontName := iif( Empty(a[2]), Nil, a[2] )

   EndIf

   FontName    := hb_defaultvalue(FontName, 'MS Sans Serif')
   FontSize    := hb_defaultvalue(FontSize, '10')

   cFontName    := FontName
   nFontSize    := val(FontSize   )
   nBrwFont     := nFontSize - 1
   cBrwFont     := cFontName
   nStbSize     := nFontSize
   Width_Keybrd := 90

   If nFontSize > 12
      nStbSize := 12
   ElseIf nFontSize == 12
      nStbSize -= 1
   Else
      Width_Keybrd := NIL
      nStbSize     := NIL
   EndIf

   SET CENTURY ON
   SET DATE ANSI
   SET ShowDetailError ON
   SET DELETED ON
   SET BROWSESYNC ON

   SET OOP ON

   SET FONT TO cFontName, nFontSize

   DEFINE FONT Normal    FONTNAME _HMG_DefaultFontName SIZE _HMG_DefaultFontSize
   //DEFINE FONT Bold      FONTNAME _HMG_DefaultFontName SIZE _HMG_DefaultFontSize BOLD
   //DEFINE FONT Italic    FONTNAME _HMG_DefaultFontName SIZE _HMG_DefaultFontSize ITALIC
   //DEFINE FONT Underline FONTNAME _HMG_DefaultFontName SIZE _HMG_DefaultFontSize UNDERLINE

   DEFINE FONT font_0  FONTNAME cFontName SIZE nFontSize
   DEFINE FONT font_1  FONTNAME cBrwFont  SIZE nBrwFont
   DEFINE FONT DlgFont FONTNAME "Tahoma"  SIZE nFontSize

   // font for functions HMG_Alert() и Alert...()
   DEFINE FONT DlgFont FONTNAME "DejaVu Sans Mono" SIZE 13

   OPEN_TABLE()

   WITH OBJECT App.Object

   IF cFontName == "Arial Black"  // спец. правка width для фонта
      a := iif( nFontSize < 15, 20, iif( nFontSize < 20, 30, 40 ) )
      :nPixWidth    += a
      :nPixWidthDT  += a
      :nPixWidthDT1 += a
      :nPixWidthDT2 += a
   ENDIF

   :O:BColorGet    := {{255,255,255},{255,255,200},{200,255,255}}
   :O:FColorGet    := {{  0,  0,  0},{255,255,200},{0  ,0  ,255}}
   :O:BClrGetFocus := :O:BColorGet[3]           // {200,255,255}
   :O:FClrGetFocus := :O:FColorGet[3]           // BLUE
   :O:FColor1      := BLACK
   :O:FColor2      := RED

   SET GETBOX FOCUS BACKCOLOR TO :O:BClrGetFocus
   SET GETBOX FOCUS FONTCOLOR TO :O:FClrGetFocus

   :Event( 1, {|| HMG_Alert("MessageBox Info", , "Information", ICON_INFORMATION) } )
   :Event( 2, {|oa,ky,np,cp| ShellExecute( , 'open', App.ExeName, cp, , np ), ;
                             ReleaseAllWindows() } )
   :Event( 3, {|oa,ky,np| TONE( np ) } )
   :Event( 8, {|oa,ky,ni,ap| MsgBox(ap[1] + ' = ' + cValToChar(ap[2]) + CRLF + ;
                             'ControlIndex = ' + cValToChar(ni) + CRLF + ;
                             'FormName = ' + ap[3], 'DisplayValue' ) } )
   :Event( 9, {|oa,ky,ni,ap| MsgBox(ap[1] + ' = ' + cValToChar(ap[2]) + CRLF + ;
                             'ControlIndex = ' + cValToChar(ni) + CRLF + ;
                             'FormName = ' + ap[3], 'VALUE' ) } )
   :Event(10, {|oa,ky,ni,ap| iif( ni == 0, _DisableControl(ap[1], ap[2]),    ;
                                           _EnableControl (ap[1], ap[2]) ) } )

   :Event( 7, {|oa,ky,np,xp| _LogFile(.T., oa, ky, np, xp, oa:ClassName) } )

   END WITH

   DEFINE WINDOW Form_1 AT 0,0 WIDTH 600 HEIGHT 500 ;
      TITLE APP_TITLE + _HMG_DefaultFontName+" "+ ;
                        hb_ntos( _HMG_DefaultFontSize )+" ( "+ ;
                        hb_ntos(App.Object:nScaleWidth)+", "+ ;
                        hb_ntos(App.Object:nScaleHeight)+" % )" ;
      MAIN ;
      ON INIT ( _Restore( This.Handle ), DoEvents() ) ;
      ON INTERACTIVECLOSE iif( This.Button_3.Enabled, ;
              ( ButtonPress('Button_3', 'Browse_1'), .F. ), .T. )

      Main_Menu()

      WITH OBJECT App.Object
//------------------------------------------ for SET OOP ON
//    WITH OBJECT This.Object
//    :O:BColorGet   := :AO:BColorGet   // (App.Object):O:BColorGet
//    :O:FColorGet   := :AO:FColorGet   // (App.Object):O:FColorGet
//    :O:FColor1     := :AO:FColor1     // (App.Object):O:FColor1
//    :O:FColor2     := :AO:FColor2     // (App.Object):O:FColor2
//------------------------------------------

      :O:nDefLen     := :W(1.5)    // default width
      :O:nBrwLen     := :W(3.8)
      :O:nBrwSayLen  := :W(0.6)
      :O:nBoolLen    := :W(0.25)    // width logic. getbox browse

      DEFINE STATUSBAR  SIZE nStbSize
          STATUSITEM ""
          STATUSITEM "" WIDTH :W(.6)
          KEYBOARD      WIDTH  Width_Keybrd
      END STATUSBAR

      :GetGaps({ , , , })

      :T += :GapsHeight
      :L += :GapsWidth

      :O:nTop2  := :T
      :O:nLeft2 := :L + :O:nDefLen + :GapsWidth * 3

      _HMG_InplaceParentHandle := ThisWindow.Handle

      /*  
      DEFINE GETBOX Text_1 // Alternate Syntax - option 1
        ROW  ( :Y := :T )
        COL  ( :X := :L )
        WIDTH  :W(1.5)
        HEIGHT :H1
        VALUE DATE()
        PICTURE '@K'
        TOOLTIP "Date Value: Must be greater or equal to "+DTOC(DATE())
        VALID {|| Compare(this.value)}
        VALIDMESSAGE "Must be greater or equal to "+DTOC(DATE())
        MESSAGE "Date Value"
        ON INIT {|og| :Y += This.Height + :GapsHeight,  ;
                                          This.Alignment := 'CENTER', ;
                                          Set_KeyEvent(og) }
      END GETBOX */

      DEFINE GETBOX Text_1    // Alternate Syntax - option 2
        ROW  ( :Y := :T )
        COL  ( :X := :L )
        WIDTH  :W(1.5)
        HEIGHT :H1
        VALUE DATE()
        PICTURE '@K'
        TOOLTIP "Date Value: Must be greater or equal to "+DTOC(DATE())
        MESSAGE "Date Value"
        ON INIT {|og| :Y += This.Height + :GapsHeight,  ;
                                          This.Alignment := 'CENTER', ;
                                          Set_KeyEvent(og) }
      END GETBOX OBJECT oGet

      oGet:PostBlock := {|| Compare(This.value, This.Name) }

      /*  
      @ :Y, :X GETBOX Text_2 ;      // option 1
               WIDTH  :O:nDefLen ;
               HEIGHT :H1 ;
               VALUE 57639 ;
               ACTION MsgInfo( "Button Action");
               TOOLTIP {"Numeric input. RANGE -100,200000 PICTURE @Z 999,999.99","Button ToolTip"};
               BUTTONWIDTH :H1 ;
               PICTURE '@Z 999,999.99';
               RANGE -100,200000;
               BOLD;
               MESSAGE "Numeric input";
               VALIDMESSAGE "Value between -100 and 200000 "  ;
               ON INIT {|og| :Y += This.Height + :GapsHeight, ;
                                   Set_KeyEvent(og) } */

      @ :Y, :X GETBOX Text_2 OBJ oGet2 ;     // option 2
               WIDTH  :O:nDefLen ;
               HEIGHT :H1 ;
               VALUE 57639 ;
               ACTION MsgInfo( "Button Action");
               TOOLTIP {"Numeric input. RANGE -100,200000 PICTURE @Z 999,999.99","Button ToolTip"};
               BUTTONWIDTH :H1 ;
               PICTURE '@Z 999,999.99';
               BOLD;
               MESSAGE "Numeric input";
               ON INIT {|og| :Y += This.Height + :GapsHeight, ;
                                   Set_KeyEvent(og) }

      oGet2:PostBlock := {|| CompareNumeric(This.value, This.Name) }  // option 2

        h := Max(:H1, 24)
      @ :Y, :X GETBOX Text_3 WIDTH :O:nDefLen HEIGHT h ;
               VALUE 10 ;
               ACTION  _wPost(3, , {This.Name, +1}) ;
               ACTION2 _wPost(3, , {This.Name, -1}) ;
               IMAGE {"add.bmp","remove.bmp"} ;
               BUTTONWIDTH h;
               PICTURE "999999"; // VALID {|| This.Value >= This.Cargo[1] .and. This.Value <= This.Cargo[2] };
               ON INIT {|| :Y += This.Height + :GapsHeight * 2 }

      (This.Object):Event(3, {|ow,ky,ag|
                               Local cn := ag[1]
                               Local ns := ag[2]
                               Local nv := This.&(cn).Value
                               Local nMin := 0
                               Local nMax := 100
                               ky := nv + ns
                               If ky >= nMin .and. ky <= nMax
                                  This.&(cn).Value := ky
                               EndIf
                               ky := ow:Name
                               Return Nil
                             })

      @ :Y, :X GETBOX Text_4 WIDTH :O:nBoolLen HEIGHT :H1 ;
               VALUE .T.;
               TOOLTIP "Logical Input VALID {|| (This.Value == .t.)}";
               PICTURE "Y";
               VALID {|| (This.Value == .T.)};
               VALIDMESSAGE "Only True is allowed here !!!";
               MESSAGE "Logical Input" ;
               ON INIT {|| :Y += This.Height + :GapsHeight * 2, ;
                                 This.Alignment := 'CENTER' }

      @ :Y, :X GETBOX Text_2a WIDTH :O:nDefLen HEIGHT :H1 ;
               VALUE 234123.10 ;
               TOOLTIP "Numeric input PICTURE @ECX) $**,***.**" ;
               PICTURE '@ECX) $**,***.**' ;
               ON INIT {|| :Y += This.Height + :GapsHeight * 2, ;
                           :O:nTop2 := :Y + :GapsHeight }

      @ :Y, :X GETBOX Text_2b WIDTH :O:nDefLen HEIGHT :H1 ;
               VALUE "Kowalski";
               PICTURE "@K !!!!!!!!!!";
               ON CHANGE (App.Object):Send(This.Cargo, 300) ; //{|| TONE(300)};
               ON INIT {|| This.Cargo := 3, ;                   // :Event
                           :Y += This.Height + :GapsHeight * 2 }

      DEFINE GETBOX Text_2c    // Alternate Syntax
             ROW    :Y
             COL    :X
             WIDTH  :O:nDefLen
             HEIGHT :H1
             VALUE "MyPass"
             PICTURE "@K !!!!!!!!!"
             VALID {|| ( len(alltrim(This.Value)) >= 4)}
             TOOLTIP "Character input PASSWORD clause is set"
             VALIDMESSAGE "Password must contains minimum 4 characters"
             MESSAGE "Enter password (min 4 char.) "
             PASSWORD .T.
             ON INIT {|| :Y += This.Height + :GapsHeight * 3 }
       END GETBOX

       @ :T - :GapsHeight, :L - :GapsWidth FRAME Frame_1 Caption "" ;
               WIDTH :X + :O:nDefLen HEIGHT :Y - :GapsHeight * 3

       @ :Y, :X BUTTONEX OButton_4 WIDTH :O:nDefLen HEIGHT :H3 ;
                CAPTION "Test"+CRLF+"&Info"  ;
                PICTURE "info.bmp" ;
                FLAT ;
                FONTCOLOR BLUE BOLD  ;
                BACKCOLOR WHITE ;
                TOOLTIP "horizontal Bitmap BUTTONEX 4" ;
                ACTION ( (App.Object):Send(This.Cargo[1], 800), ; // TONE(800)
                         (App.Object):Post(This.Cargo[2]) ) ;
                ON INIT {|| This.Cargo := { 3, 1 }, ;              // Events
                            :Y += This.Height + :GapsHeight * 2 }

        :Y := This.Text_1.Row
        :X := :O:nLeft2
      @ :Y, :X BROWSE Browse_1 ;
               WIDTH    :O:nBrwLen ;
               HEIGHT ( :O:nTop2 - :T + :GapsHeight * .5 ) ;
               WORKAREA TEST ;
               BACKCOLOR {255,255,200} ;
               HEADERS {"Date","Numeric","Character","Logical"};
               WIDTHS { :O:nBrwLen*0.2,:O:nBrwLen*0.2,:O:nBrwLen*0.5,:O:nBrwLen*0.1 };
               FIELDS { 'Test->Datev' , 'Test->Numeric' , 'Test->Character' , 'Test->Logical'} ;
               JUSTIFY {BROWSE_JTFY_LEFT,BROWSE_JTFY_RIGHT, BROWSE_JTFY_LEFT,BROWSE_JTFY_CENTER} ;
               FONT 'font_1' ;
               Value 1;
               LOCK;
               TOOLTIP "Double Click to edit";
               ON DBLCLICK {|| UnlockData( ) } ;
               ON CHANGE   {|| RefreshData() }

        :Y += This.Browse_1.Height + :GapsHeight * 2
      @ :Y, :X LABEL Label_1a VALUE "Date" WIDTH :O:nBrwSayLen HEIGHT :H1 BOLD ;
               ON INIT {|| :X += This.Width + :GapsWidth * 2 }
      @ :Y, :X GETBOX Text_5 WIDTH :D1 HEIGHT :H1 ;
        TOOLTIP "Text_5. DoubleClick => Edit" ;
        BACKCOLOR {{255,255,255},{255,255,200},{200,255,255}} ;
        PICTURE '@KD';
        FIELD test->Datev ;
        ON LOSTFOCUS LostFocus2Get() ;
        READONLY ;
        ON INIT {|og| :Y += This.Height + :GapsHeight, ;
                            Set_KeyEvent(og) }
        :X := :O:nLeft2
      @ :Y, :X LABEL Label_1b VALUE "Num." WIDTH :O:nBrwSayLen HEIGHT :H1 BOLD ;
               ON INIT {|| :X += This.Width + :GapsWidth * 2 }
      DEFINE GETBOX Text_6 // Alternate Syntax
          ROW    :Y
          COL    :X
          WIDTH  :W1
          HEIGHT :H1
          FIELD test->Numeric
          ON LOSTFOCUS LostFocus2Get()
          WHEN {|| This.Value > 99}
          TOOLTIP "Numeric field. WHEN {|| This.Value > 99}"
          BACKCOLOR :O:BColorGet
          READONLY .T.
          PICTURE "@KB 999999"
          ON INIT {|og| :Y += This.Height + :GapsHeight, ;
                                      Set_KeyEvent(og) }
      END GETBOX

        :X := :O:nLeft2
      @ :Y, :X LABEL Label_1c VALUE "Char." WIDTH :O:nBrwSayLen HEIGHT :H1 BOLD ;
               ON INIT {|| :X += This.Width + :GapsWidth * 2 }
      @ :Y, :X GETBOX Text_7  WIDTH :O:nDefLen HEIGHT :H1 ;
        BACKCOLOR  :O:BColorGet ;
        TOOLTIP "Characters field. DoubleClick => Edit" ;
        VALIDMESSAGE "Can not be empty!. VALID {|| (!EMPTY(This.Value))} . PICTURE @K !XXXXXXXXXXXXXXXX ";
        VALID {|| (!EMPTY(This.Value))} ;
        FIELD test->Character  ;
        ON LOSTFOCUS LostFocus2Get() ;
        PICTURE "@K !XXXXXXXXXXXXXXXX" ;
        READONLY ;
        ON INIT {|og| :Y += This.Height + :GapsHeight, ;
                            Set_KeyEvent(og) }
        :X := :O:nLeft2
      @ :Y, :X LABEL Label_1d VALUE "Logic." WIDTH :O:nBrwSayLen HEIGHT :H1 BOLD ;
               ON INIT {|| :X += This.Width + :GapsWidth * 2 }
      @ :Y, :X GETBOX Text_8 WIDTH :O:nBoolLen HEIGHT :H1 ;
        BACKCOLOR  :O:BColorGet ;
        FONTCOLOR  :O:FColor2   ;
        BOLD;
        TOOLTIP "Logical field. DoubleClick => Edit" ;
        FIELD test->Logical;
        ON LOSTFOCUS LostFocus2Get() ;
        READONLY ;
        ON INIT {|og| :Y += This.Height +  :GapsHeight, ;
                            This.Alignment := 'CENTER', ;
                            Set_KeyEvent(og) }
        :O:nTop2 := :Y

        :X := This.Text_5.Col + :O:nDefLen + :GapsWidth * 2
        :Y := This.Text_5.Row
      @ :Y, :X BUTTONEX Button_1 WIDTH :W(1.4) HEIGHT :H1 CAPTION "Save"   ;
               FONTCOLOR {200,0,0} BOLD ACTION saveDateNow()
        :Y := This.Text_6.Row
      @ :Y, :X BUTTONEX Button_2 WIDTH :W(1.4) HEIGHT :H1 CAPTION "Edit"   ;
               FONTCOLOR {200,0,0} BOLD ACTION UnlockData()
        :Y := This.Text_7.Row
      @ :Y, :X BUTTONEX Button_3 WIDTH :W(1.4) HEIGHT :H1 CAPTION "Cancel" ;
               FONTCOLOR {200,0,0} BOLD ACTION CancelData()

      @ :T - :GapsHeight, :O:nLeft2 - :GapsWidth FRAME Frame_2 Caption "" ;
              WIDTH :O:nBrwLen + :GapsWidth * 2 HEIGHT :O:nTop2 - :GapsHeight

        :O:nMaxX := :O:nLeft2 + :O:nBrwLen + :GapsWidth
        :O:nMaxY := :O:nTop2  + :GapsHeight

        This.Width  := :O:nMaxX + :R + :GapsWidth + GetBorderWidth()
        This.Height := :O:nMaxY + :B + This.StatusBar.Height + ;
                    GetBorderHeight() + GetTitleHeight() + GetMenuBarHeight()

      END WITH

      EnabledBtn(.F.)
      This.Browse_1.ColumnsAutoFitH
      RefreshData()

   END WINDOW

   Form_1.Center()
   Form_1.Activate()

Return NIL

*-----------------------------
STATIC FUNC _HMG_Value()
*-----------------------------
   LOCAL s := ''

   s += '_HMG_ThisFormIndex   = ' + cValToChar( _HMG_ThisFormIndex   ) + CRLF
   s += '_HMG_ThisEventType   = ' + cValToChar( _HMG_ThisEventType   ) + CRLF
   s += '_HMG_ThisType        = ' + cValToChar( _HMG_ThisType        ) + CRLF
   s += '_HMG_ThisIndex       = ' + cValToChar( _HMG_ThisIndex       ) + CRLF
   s += '_HMG_ThisFormName    = ' + cValToChar( _HMG_ThisFormName    ) + CRLF
   s += '_HMG_ThisControlName = ' + cValToChar( _HMG_ThisControlName ) + CRLF

RETURN s

*-----------------------------
STAT FUNC Set_KeyEvent( oGet )
*-----------------------------

   If     This.Name == 'Text_1'

      oGet:SetKeyEvent( VK_F5, {|o| MsgBox( 'VK_F5 : ' + cValToChar( o:VarGet() ), This.Name ) } )
      oGet:SetKeyEvent( , {|o| MsgBox( 'LDblClick : ' + cValToChar( o:VarGet() ), This.Name ) } )

   ElseIf This.Name == 'Text_2'

      oGet:SetKeyEvent( VK_F5, {|o| MsgBox( 'VK_F5 : ' + cValToChar( o:VarGet() ), This.Name ) } )
      oGet:SetKeyEvent(  , {|o| MsgBox( 'LDblClick : ' + cValToChar( o:VarGet() ), This.Name ) } )

   ElseIf This.Name $  'Text_5,Text_6,Text_7,Text_8'

      oGet:SetKeyEvent(  , {|| DoublClick2Get() } )

   EndIf

RETURN Nil

*-----------------------------
STATIC FUNC LostFocus2Get()
*-----------------------------
   LOCAL cCtl := This.FocusedControl
   LOCAL cGet := This.Name

   If     This.ReadOnly
   ElseIf cCtl == 'Browse_1'
      ButtonPress('Button_3')
   ElseIf ! cCtl $ 'Text_5,Text_6,Text_7,Text_8,Button_1,Button_3'
      This.&(cGet).SetFocus
   EndIf

RETURN Nil

*-----------------------------
FUNC ButtonPress( cName, cFocus )
*-----------------------------

//   (ThisWindow.Object):Send( This.&(cName).Cargo  )
   This.&(cName).SetFocus
   _PushKey( VK_SPACE )
   DO EVENTS

   If !Empty(cFocus)
      This.&(cFocus).SetFocus
   EndIf

RETURN Nil

*-----------------------------
STATIC FUNC DoublClick2Get()
*-----------------------------
   LOCAL cGet := This.FocusedControl

   If This.ReadOnly
      ButtonPress('Button_2', cGet)
   EndIf

RETURN Nil

*-----------------------------
STAT PROC RefreshData()
*-----------------------------

  SetProperty( "Form_1", "StatusBar", "Item", 2, hb_ntos(RecNo()) )

  Form_1.Text_5.FontColor := (App.Object):O:FColor1
  Form_1.Text_6.FontColor := (App.Object):O:FColor1
  Form_1.Text_7.FontColor := (App.Object):O:FColor1
  Form_1.Text_8.FontColor := (App.Object):O:FColor2

  Form_1.Text_5.Refresh
  Form_1.Text_6.Refresh
  Form_1.Text_7.Refresh
  Form_1.Text_8.Refresh
  DO EVENTS

Return

*-----------------------------
STAT PROC ReadOnlyData( lROnly )
*-----------------------------

  Form_1.Text_5.Readonly := lROnly
  Form_1.Text_6.Readonly := lROnly
  Form_1.Text_7.Readonly := lROnly
  Form_1.Text_8.Readonly := lROnly
  DO EVENTS

Return

*-----------------------------
STAT PROC EnabledBtn( lEnable )
*-----------------------------

  Form_1.Button_1.Enabled :=   lEnable
  Form_1.Button_2.Enabled := ! lEnable
  Form_1.Button_3.Enabled :=   lEnable
  DO EVENTS

Return

*-----------------------------
Procedure UnlockData()
*-----------------------------

   IF !RLOCK()
      MsgStop("Record occupied by another user")
      return
   endif

  RefreshData()
  ReadOnlyData(.F.)
  EnabledBtn(.T.)

  Form_1.Text_5.SetFocus

Return

*-----------------------------
Procedure saveDateNow()
*-----------------------------
  IF RLOCK()
     Form_1.Text_5.Save
     Form_1.Text_6.Save
     Form_1.Text_7.Save
     Form_1.Text_8.Save
     UNLOCK
  else
     RETURN
  endif

  RefreshData()
  ReadOnlyData(.T.)
  EnabledBtn(.F.)

  Form_1.Browse_1.Refresh
  Form_1.Browse_1.SetFocus

return

*-----------------------------
Function CancelData()
*-----------------------------

  RefreshData()
  ReadOnlyData(.T.)
  EnabledBtn(.F.)

  Form_1.Browse_1.SetFocus

   UNLOCK

return NIL

*-----------------------------
Function OPEN_TABLE()
*-----------------------------
Local i

   If !FILE("test.dbf")

      DBTESTCREATE("test")

      USE TEST NEW EXCLUSIVE

      FOR i=1 to 10
         APPEND BLANK
         test->Datev := date()+i
         test->Numeric := i*10
         test->Character := "Character "+ltrim(str(i))
         test->Logical := ( int(i/2) == i/2 )
      next i

      USE

   ENDIF

   USE TEST NEW SHARED

Return NIL

*-----------------------------
Function DBTESTCREATE(ufile)
*-----------------------------
  Local aDbf := {}

  AADD (aDbf,{"Datev"      , "D",  8,0})
  AADD (aDbf,{"Numeric"    , "N",  5,0})
  AADD (aDbf,{"Character"  , "C",  20,0})
  AADD (aDbf,{"Logical"    , "L",  1,0})
  dbcreate( ufile, aDbf, 'DBFNTX' )

Return NIL

*-----------------------------
Function Compare(dDate, cName)
*-----------------------------
   Local lRet := .F.

   if !empty(dDate) .and. year(dDate) == year(date())
      lRet := .T.
   else
      Define timer timer_1 ;
		of form_1 ;
		interval 50 ;
		action ( C_Center( FindWindowEx( ,,, "Error -> " + cName ), form_1.timer_1.release ) ) once

      AlertStop( "Must be greater or equal to "+hb_ntos(year(DATE())), "Error -> " + cName )
   endif
return lRet

*-----------------------------
Function CompareNumeric(nVal, cName)
*-----------------------------
   Local lRet := .F.

   if nVal > - 100.01 .and. nVal < 200000.01
      lRet := .T.
   else
      Define timer timer_2 ;
		of form_1 ;
		interval 50 ;
		action ( C_Center( FindWindowEx( ,,, "Error -> " + cName ) ), form_1.timer_2.release ) once

      AlertStop( "Value between -100 or 200000 !", "Error -> " + cName )
   endif
return lRet

*-----------------------------
Function _Trans(xval)
*-----------------------------
   Local RetVal

   if valtype(xVAL)=="C"
      RetVal := xval
   elseif valtype(xVal)=="D"
      RetVal := DTOC(xVal)
   elseif valtype(xVal)=="N"
      RetVal := alltrim(str(xVal))
   elseif valtype(xVal)=="L"
      RetVal := if(xVal,"True","False")
   else
      RetVal := "Unknown"
   endif

return RetVal

*-----------------------------
STATIC FUNC Main_Menu()
*-----------------------------
   LOCAL cFnt := "MS Sans Serif"
   LOCAL cFn1 := "Arial"
   LOCAL cFn2 := "Tahoma"
   LOCAL cFn3 := "DejaVu Sans Mono"
   LOCAL cFn4 := "Arial Black"
   LOCAL AO := App.Object

   DEFINE MAIN MENU

      POPUP 'Set font parameters'
      POPUP 'Font '+cFnt
      ITEM "Set "+cFnt+",  9 " ACTION {|cp| cp := '" 9,'+cFnt+'"', AO:Send(2, 2, cp) }
      ITEM "Set "+cFnt+", 10 " ACTION {|cp| cp := '"10,'+cFnt+'"', AO:Send(2, 2, cp) }
      ITEM "Set "+cFnt+", 11 " ACTION {|cp| cp := '"11,'+cFnt+'"', AO:Send(2, 2, cp) }
      ITEM "Set "+cFnt+", 12 " ACTION {|cp| cp := '"12,'+cFnt+'"', AO:Send(2, 2, cp) }
      ITEM "Set "+cFnt+", 14 " ACTION {|cp| cp := '"14,'+cFnt+'"', AO:Send(2, 2, cp) }
      ITEM "Set "+cFnt+", 16 " ACTION {|cp| cp := '"16,'+cFnt+'"', AO:Send(2, 2, cp) }
      ITEM "Set "+cFnt+", 18 " ACTION {|cp| cp := '"18,'+cFnt+'"', AO:Send(2, 2, cp) }
      ITEM "Set "+cFnt+", 20 " ACTION {|cp| cp := '"20,'+cFnt+'"', AO:Send(2, 2, cp) }
      ITEM "Set "+cFnt+", 22 " ACTION {|cp| cp := '"22,'+cFnt+'"', AO:Send(2, 2, cp) }
      ITEM "Set "+cFnt+", 24 " ACTION {|cp| cp := '"24,'+cFnt+'"', AO:Send(2, 2, cp) }
      END POPUP
      POPUP 'Font '+cFn1
      ITEM "Set "+cFn1+",  9 " ACTION {|cp| cp := " 9,"+cFn1, AO:Send(2, 2, cp) }
      ITEM "Set "+cFn1+", 10 " ACTION {|cp| cp := "10,"+cFn1, AO:Send(2, 2, cp) }
      ITEM "Set "+cFn1+", 11 " ACTION {|cp| cp := "11,"+cFn1, AO:Send(2, 2, cp) }
      ITEM "Set "+cFn1+", 12 " ACTION {|cp| cp := "12,"+cFn1, AO:Send(2, 2, cp) }
      ITEM "Set "+cFn1+", 14 " ACTION {|cp| cp := "14,"+cFn1, AO:Send(2, 2, cp) }
      ITEM "Set "+cFn1+", 16 " ACTION {|cp| cp := "16,"+cFn1, AO:Send(2, 2, cp) }
      ITEM "Set "+cFn1+", 18 " ACTION {|cp| cp := "18,"+cFn1, AO:Send(2, 2, cp) }
      ITEM "Set "+cFn1+", 20 " ACTION {|cp| cp := "20,"+cFn1, AO:Send(2, 2, cp) }
      ITEM "Set "+cFn1+", 22 " ACTION {|cp| cp := "22,"+cFn1, AO:Send(2, 2, cp) }
      ITEM "Set "+cFn1+", 24 " ACTION {|cp| cp := "24,"+cFn1, AO:Send(2, 2, cp) }
      END POPUP
      POPUP 'Font '+cFn2
      ITEM "Set "+cFn2+",  9 " ACTION {|cp| cp := " 9,"+cFn2, AO:Send(2, 2, cp) }
      ITEM "Set "+cFn2+", 10 " ACTION {|cp| cp := "10,"+cFn2, AO:Send(2, 2, cp) }
      ITEM "Set "+cFn2+", 11 " ACTION {|cp| cp := "11,"+cFn2, AO:Send(2, 2, cp) }
      ITEM "Set "+cFn2+", 12 " ACTION {|cp| cp := "12,"+cFn2, AO:Send(2, 2, cp) }
      ITEM "Set "+cFn2+", 14 " ACTION {|cp| cp := "14,"+cFn2, AO:Send(2, 2, cp) }
      ITEM "Set "+cFn2+", 16 " ACTION {|cp| cp := "16,"+cFn2, AO:Send(2, 2, cp) }
      ITEM "Set "+cFn2+", 18 " ACTION {|cp| cp := "18,"+cFn2, AO:Send(2, 2, cp) }
      ITEM "Set "+cFn2+", 20 " ACTION {|cp| cp := "20,"+cFn2, AO:Send(2, 2, cp) }
      ITEM "Set "+cFn2+", 22 " ACTION {|cp| cp := "22,"+cFn2, AO:Send(2, 2, cp) }
      ITEM "Set "+cFn2+", 24 " ACTION {|cp| cp := "24,"+cFn2, AO:Send(2, 2, cp) }
      END POPUP
      POPUP 'Font '+cFn3
      ITEM "Set "+cFn3+",  9 " ACTION {|cp| cp := '" 9,'+cFn3+'"', AO:Send(2, 2, cp) }
      ITEM "Set "+cFn3+", 10 " ACTION {|cp| cp := '"10,'+cFn3+'"', AO:Send(2, 2, cp) }
      ITEM "Set "+cFn3+", 11 " ACTION {|cp| cp := '"11,'+cFn3+'"', AO:Send(2, 2, cp) }
      ITEM "Set "+cFn3+", 12 " ACTION {|cp| cp := '"12,'+cFn3+'"', AO:Send(2, 2, cp) }
      ITEM "Set "+cFn3+", 14 " ACTION {|cp| cp := '"14,'+cFn3+'"', AO:Send(2, 2, cp) }
      ITEM "Set "+cFn3+", 16 " ACTION {|cp| cp := '"16,'+cFn3+'"', AO:Send(2, 2, cp) }
      ITEM "Set "+cFn3+", 18 " ACTION {|cp| cp := '"18,'+cFn3+'"', AO:Send(2, 2, cp) }
      ITEM "Set "+cFn3+", 20 " ACTION {|cp| cp := '"20,'+cFn3+'"', AO:Send(2, 2, cp) }
      ITEM "Set "+cFn3+", 22 " ACTION {|cp| cp := '"22,'+cFn3+'"', AO:Send(2, 2, cp) }
      ITEM "Set "+cFn3+", 24 " ACTION {|cp| cp := '"24,'+cFn3+'"', AO:Send(2, 2, cp) }
      END POPUP
      POPUP 'Font '+cFn4
      ITEM "Set "+cFn4+",  9 " ACTION {|cp| cp := '" 9,'+cFn4+'"', AO:Send(2, 2, cp) }
      ITEM "Set "+cFn4+", 10 " ACTION {|cp| cp := '"10,'+cFn4+'"', AO:Send(2, 2, cp) }
      ITEM "Set "+cFn4+", 11 " ACTION {|cp| cp := '"11,'+cFn4+'"', AO:Send(2, 2, cp) }
      ITEM "Set "+cFn4+", 12 " ACTION {|cp| cp := '"12,'+cFn4+'"', AO:Send(2, 2, cp) }
      ITEM "Set "+cFn4+", 14 " ACTION {|cp| cp := '"14,'+cFn4+'"', AO:Send(2, 2, cp) }
      ITEM "Set "+cFn4+", 16 " ACTION {|cp| cp := '"16,'+cFn4+'"', AO:Send(2, 2, cp) }
      ITEM "Set "+cFn4+", 18 " ACTION {|cp| cp := '"18,'+cFn4+'"', AO:Send(2, 2, cp) }
      ITEM "Set "+cFn4+", 20 " ACTION {|cp| cp := '"20,'+cFn4+'"', AO:Send(2, 2, cp) }
      ITEM "Set "+cFn4+", 22 " ACTION {|cp| cp := '"22,'+cFn4+'"', AO:Send(2, 2, cp) }
      ITEM "Set "+cFn4+", 24 " ACTION {|cp| cp := '"24,'+cFn4+'"', AO:Send(2, 2, cp) }
      END POPUP
      SEPARATOR
      ITEM "Set font parameters: Default" ACTION {|cp| cp := [""], AO:Send(2, 2, cp) }
      END POPUP

      POPUP '&Get Value'
      ITEM "Get Text_1  Value" ACTION (App.Object):Post(9, This.Text_1.Index , {'Text_1' , This.Text_1.Value , ThisWindow.Name}) MESSAGE "Vale and ValueType"
      ITEM "Get Text_2  Value" ACTION (App.Object):Post(9, This.Text_2.Index , {'Text_2' , This.Text_2.Value , ThisWindow.Name}) MESSAGE "Vale and ValueType"
      ITEM "Get Text_3  Value" ACTION (App.Object):Post(9, This.Text_3.Index , {'Text_3' , This.Text_3.Value , ThisWindow.Name}) MESSAGE "Vale and ValueType"
      ITEM "Get Text_4  Value" ACTION (App.Object):Post(9, This.Text_4.Index , {'Text_4' , This.Text_4.Value , ThisWindow.Name}) MESSAGE "Vale and ValueType"
      ITEM "Get Text_2a Value" ACTION (App.Object):Post(9, This.Text_2a.Index, {'Text_2a', This.Text_2a.Value, ThisWindow.Name}) MESSAGE "Vale and ValueType"
      ITEM "Get Text_2b Value" ACTION (App.Object):Post(9, This.Text_2b.Index, {'Text_2b', This.Text_2b.Value, ThisWindow.Name}) MESSAGE "Vale and ValueType"
      ITEM "Get Text_2c Value" ACTION (App.Object):Post(9, This.Text_2c.Index, {'Text_2c', This.Text_2c.Value, ThisWindow.Name}) MESSAGE "Vale and ValueType"
      END POPUP

      POPUP 'Get &DisplayValue'
      ITEM "Get Text_1 DisplayValue"  ACTION (App.Object):Post(8, This.Text_1.Index , {'Text_1' , This.Text_1.DisplayValue , ThisWindow.Name})
      ITEM "Get Text_2 DisplayValue"  ACTION (App.Object):Post(8, This.Text_2.Index , {'Text_2' , This.Text_2.DisplayValue , ThisWindow.Name})
      ITEM "Get Text_3 DisplayValue"  ACTION (App.Object):Post(8, This.Text_3.Index , {'Text_3' , This.Text_3.DisplayValue , ThisWindow.Name})
      ITEM "Get Text_4 DisplayValue"  ACTION (App.Object):Post(8, This.Text_4.Index , {'Text_4' , This.Text_4.DisplayValue , ThisWindow.Name})
      ITEM "Get Text_2a DisplayValue" ACTION (App.Object):Post(8, This.Text_2a.Index, {'Text_2a', This.Text_2a.DisplayValue, ThisWindow.Name})
      ITEM "Get Text_2b DisplayValue" ACTION (App.Object):Post(8, This.Text_2b.Index, {'Text_2b', This.Text_2b.DisplayValue, ThisWindow.Name})
      ITEM "Get Text_2c DisplayValue" ACTION (App.Object):Post(8, This.Text_2c.Index, {'Text_2c', This.Text_2c.DisplayValue, ThisWindow.Name})
      END POPUP

      POPUP 'Disable/Enable'
      ITEM "Enable Text_1"   ACTION (App.Object):Post(10, 1, {'Text_1' , ThisWindow.Name})
      ITEM "Enable Text_2"   ACTION (App.Object):Post(10, 1, {'Text_2' , ThisWindow.Name})
      ITEM "Enable Text_3"   ACTION (App.Object):Post(10, 1, {'Text_3' , ThisWindow.Name})
      ITEM "Enable Text_4"   ACTION (App.Object):Post(10, 1, {'Text_4' , ThisWindow.Name})
      ITEM "Enable Text_2a"  ACTION (App.Object):Post(10, 1, {'Text_2a', ThisWindow.Name})
      ITEM "Enable Text_2b"  ACTION (App.Object):Post(10, 1, {'Text_2b', ThisWindow.Name})
      ITEM "Enable Text_2c"  ACTION (App.Object):Post(10, 1, {'Text_2c', ThisWindow.Name})

      SEPARATOR
      ITEM "Disable Text_1"  ACTION (App.Object):Post(10, 0, {'Text_1' , ThisWindow.Name})
      ITEM "Disable Text_2"  ACTION (App.Object):Post(10, 0, {'Text_2' , ThisWindow.Name})
      ITEM "Disable Text_3"  ACTION (App.Object):Post(10, 0, {'Text_3' , ThisWindow.Name})
      ITEM "Disable Text_4"  ACTION (App.Object):Post(10, 0, {'Text_4' , ThisWindow.Name})
      ITEM "Disable Text_2a" ACTION (App.Object):Post(10, 0, {'Text_2a', ThisWindow.Name})
      ITEM "Disable Text_2b" ACTION (App.Object):Post(10, 0, {'Text_2b', ThisWindow.Name})
      ITEM "Disable Text_2c" ACTION (App.Object):Post(10, 0, {'Text_2c', ThisWindow.Name})
      END POPUP

   END MENU

RETURN Nil
