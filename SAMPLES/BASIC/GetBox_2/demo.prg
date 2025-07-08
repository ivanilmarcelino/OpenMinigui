/*
 * MINIGUI - Harbour Win32 GUI library Demo
*/

#include "demo.ch"

REQUEST DBFCDX

SET PROCEDURE TO demo_misc

*-----------------------------
Function MAIN( FontSize, ScaleWidth, ScaleHeight, FontName )
*-----------------------------
   LOCAL a, nScaleWidth, nScaleHeight, cFontName, nFontSize
   LOCAL cBrwFont, nBrwFont, nStbSize
   LOCAL Width_Keybrd

   If Empty(FontSize)              // Default

      FontSize    := Nil
      ScaleWidth  := Nil
      ScaleHeight := Nil
      FontName    := Nil

   ElseIf ! Empty(FontSize) .and. ',' $ FontSize

      a := hb_ATokens(FontSize, ','); ASize(a, 4)
     
      AEval(a, {|cv,nn| a[nn] := iif( empty(cv), '', alltrim(cv) ) })

      FontSize    := a[1]
      ScaleWidth  := a[2]
      ScaleHeight := a[3]
      FontName    := iif( empty(a[4]), Nil, a[4] )

   EndIf

   FontName    := hb_defaultvalue(FontName   , 'MS Sans Serif')
   FontSize    := hb_defaultvalue(FontSize   ,  '10')
   ScaleWidth  := hb_defaultvalue(ScaleWidth , '100')
   ScaleHeight := hb_defaultvalue(ScaleHeight, '100')

   cFontName    := FontName
   nFontSize    := val(FontSize   )
   nScaleWidth  := val(ScaleWidth )
   nScaleHeight := val(ScaleHeight)
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
   SET DATE GERMAN
   SET ShowDetailError ON
   SET DELETED ON
   SET BROWSESYNC ON
   SET CENTERWINDOW RELATIVE PARENT
   SET OOP ON

   App.Object := { nScaleWidth, nScaleHeight }
   
   DEFINE FONT font_0  FONTNAME cFontName SIZE nFontSize DEFAULT
   DEFINE FONT font_1  FONTNAME cBrwFont  SIZE nBrwFont
   DEFINE FONT DlgFont FONTNAME "Tahoma"  SIZE nFontSize
                                                  // ---- Application events
   WITH OBJECT App.Object
   :O:BColorGet    := {{255,255,255},{255,255,200},{200,255,255}}
   :O:FColorGet    := {{  0,  0,  0},{255,255,200},{0  ,0  ,255}}
   :O:BClrGetFocus := :O:BColorGet[3]           // {200,255,255}
   :O:FClrGetFocus := :O:FColorGet[3]           // BLUE         
   :O:FColor1      := BLACK
   :O:FColor2      := RED

   :Event( 1, {|| HMG_Alert( "MessageBox Info", , "Information", ICON_INFORMATION ) } )

   :Event( 2, {|oa,ky,np,cp| ShellExecute( , 'open', App.ExeName, cp, , np ), ;
                             ReleaseAllWindows() } )

   :Event( 3, {|oa,ky,np,xp| _LogFile(.T., oa, ky, np, xp, oa:ClassName) } )

   SET GETBOX FOCUS BACKCOLOR TO :O:BClrGetFocus
   SET GETBOX FOCUS FONTCOLOR TO :O:FClrGetFocus

   END WITH

   OPEN_TABLE()

   DEFINE WINDOW Form_1 AT 0,0 WIDTH 480 HEIGHT 410 ;
      TITLE 'HMG GetBox Demo. Dlu2Pixel.' ;
      MAIN ;
      ON INIT ( _Restore( This.Handle ), DoEvents() ) ;
      ON INTERACTIVECLOSE iif( This.Button_3.Enabled, ;
              ( (This.Object):Post(This.Button_3.Cargo), .F. ), .T. )
      
      Main_Menu()

      WITH OBJECT This.Object
      :O:BColorGet   := :AO:BColorGet   // (App.Object):O:BColorGet
      :O:FColorGet   := :AO:FColorGet   // (App.Object):O:FColorGet
      :O:FColor1     := :AO:FColor1     // (App.Object):O:FColor1
      :O:FColor2     := :AO:FColor2     // (App.Object):O:FColor2

      :O:nDefLen     := :W(1.5)    // default width
      :O:nBrwLen     := :W(3.8)
      :O:nBrwSayLen  := :W(0.6)
      :O:nBoolLen    := :W(0.3)    // width logic. getbox browse

      DEFINE STATUSBAR  SIZE nStbSize
          STATUSITEM ""            // WIDTH :O:nDefLen
          STATUSITEM "" WIDTH :W(.6)
          KEYBOARD      WIDTH  Width_Keybrd
      END STATUSBAR

      :O:aFrm    := array(4)       // frame1
      :O:aFr2    := array(4)       // frame2

      :O:aFrm[1] := :Top
      :O:aFrm[2] := :Left
      :O:aFrm[3] := :GapsHeight + :O:nDefLen + :GapsHeight
      
      :O:nLeft2  := :Left + :O:aFrm[3] + :GapsWidth * 2
      :O:aFr2[1] := :Top
      :O:aFr2[2] := :O:nLeft2 - :GapsWidth
      
      :O:nTop2   := :Top
       
      :Top  += :GapsHeight
      :Left += :GapsWidth

      :Y := :Top 
      :X := :Left
      DEF GET Text_1 ROWS ;
          VALUE DATE() ; 
          TOOLTIP "Date Value: Must be greater or equal to "+DTOC(DATE()) ;
          VALID {|| Compare(this.value)} ;
          VALIDMESSAGE "Must be greater or equal to "+DTOC(DATE()) ;
          MESSAGE "Date Value" ;
          CENTERALIGN ;
          GOTFOCUSSELECT ;
          ON DBLCLICK          (ThisWindow.Object):Post( 6, This.Text_1.Index)    ;
          ON KEY {{ VK_F5, {|| (ThisWindow.Object):Post( 7, This.Text_1.Index) } }, ;
                  { VK_F6, {|| (ThisWindow.Object):Post(14, This.Text_1.Index) } }}
//          BACKCOLOR   :O:BColorGet   ; 
//          FONTCOLOR   :O:FColorGet   ; 
          
      DEF GET Text_2 ROWS ;
          VALUE 57639 ;
          ACTION MsgInfo( "Button Action") ;
          TOOLTIP {"Numeric input. RANGE -100,200000 PICTURE @Z 99,999.99","Button ToolTip"} ;
          PICTURE '@Z 999,999,999.99' ;
          RANGE -100,200000 ;
          BOLD ;
          MESSAGE "Numeric input" ;
          VALIDMESSAGE "Value between -100 and 200000 " ;
          GOTFOCUSSELECT ;
          ON DBLCLICK         (ThisWindow.Object):Post(6, This.Text_2.Index)   ;
          ON KEY { VK_F5, {|| (ThisWindow.Object):Post(7, This.Text_2.Index) } }
//          BACKCOLOR   :O:BColorGet   ; 
//          FONTCOLOR   :O:FColorGet   ; 

      DEF GET Text_3 ROWS ;
          VALUE "Jacek";
          ACTION  MsgInfo( "Button Action");
          ACTION2 MsgInfo( "Button2 Action");
          IMAGE {"folder.bmp","info.bmp"}; 
          PICTURE "@K !xxxxxxxxxxx";
          TOOLTIP {"Character Input. VALID {|| ( len(alltrim(This.Value)) >= 2)} PICTURE @K !xxxxxxxxxxx ","Button ToolTip","Button 2 ToolTip"};
          VALID {|| ( len(alltrim(This.Value)) >= 2)};
          VALIDMESSAGE "Minimum 2 characters" ;
          MESSAGE "Character Input" //;
//          BACKCOLOR   :O:BColorGet   ; 
//          FONTCOLOR   :O:FColorGet     

      DEF GET Text_4 WIDTH :O:nBoolLen GAPS {0, 2.0, , } ROWS ;
          VALUE .t.;
          TOOLTIP "Logical Input VALID {|| (This.Value == .t.)}";
          PICTURE "Y";
          VALID {|| (This.Value == .t.)};
          VALIDMESSAGE "Only True is allowed here !!!";
          MESSAGE "Logical Input";
          CENTERALIGN //;
//          BACKCOLOR   :O:BColorGet   ; 
//          FONTCOLOR   :O:FColorGet     

      DEF GET Text_2a GAPS {0, 2.0, , } ROWS ;
          VALUE 234123.10 ;
          TOOLTIP "Numeric input PICTURE @ECX) $**,***.**" ;
          PICTURE '@ECX) $**,***.**' ;
          GOTFOCUSSELECT //;
//          BACKCOLOR   :O:BColorGet   ; 
//          FONTCOLOR   :O:FColorGet     

      :O:nTop2 := :Y
      DEF GET Text_2b GAPS {0, 2.0, , 2.0} ROWS ;
          VALUE "Kowalski";
          PICTURE "@K !!!!!!!!!!";
          ON CHANGE (ThisWindow.Object):Post(This.Cargo, , 300) ; //{|| TONE(300)};
          ON INIT This.Cargo := 13        // :Event
//          BACKCOLOR   :O:BColorGet   ; 
//          FONTCOLOR   :O:FColorGet   ;

      :Y += :GapsHeight
      :X := :Left
      DEFINE GETBOX Text_2c    // Alternate Syntax
          ROW    :Y
          COL    :X
          WIDTH  :W()
          HEIGHT :H() 
          VALUE "MyPass"
          PICTURE "@K !!!!!!!!!"
          BACKCOLOR   :O:BColorGet 
          FONTCOLOR   :O:FColorGet 
          VALID {|| ( len(alltrim(This.Value)) >= 4)}
          TOOLTIP "Character input PASSWORD clause is set"
          VALIDMESSAGE "Password must contains minimum 4 characters"
          MESSAGE "Enter password (min 4 char.) "
          PASSWORD .T.
      END GETBOX
      :Y += This.Text_2c.Height + :GapsHeight
      
      :O:aFrm[4] := :Y - :O:aFrm[1]

      @ :O:aFrm[1], :O:aFrm[2] FRAME Frame_1 Caption "" WIDTH :O:aFrm[3] HEIGHT :O:aFrm[4]

      :X := :Left
      DEF BTNEX OButton_4 GAPS {0, , , 2.0} ROWS HEIGHT :H1 * 2 ;
        CAPTION "Test"+CRLF+"&Info"  ;
        PICTURE "info.bmp" ;
        FLAT ;
        FONTCOLOR BLUE BOLD  ;
        BACKCOLOR WHITE ;
        TOOLTIP "horizontal Bitmap BUTTONEX 4" ;
        ACTION ( (ThisWindow.Object):Post(This.Cargo[1], , 800), ; // TONE(800)
                        (App.Object):Post(This.Cargo[2]) ) ; 
        ON INIT This.Cargo := { 13, 1 }  // Events ThisWindow and Application

      :Y += :Bottom
  
      :O:nMaxY := :Y
      :O:nMaxX := :O:nLeft2 + :O:nBrwLen 

      :Y := :Top
      :X := :O:nLeft2

      @ :Y, :X BROWSE Browse_1 ;
               WIDTH :O:nBrwLen HEIGHT ( :O:nTop2 - :Top + :GapsHeight * .5 ) ;
               WORKAREA TEST ;
               BACKCOLOR {255,255,200} ;
               HEADERS {"Date","Numeric","Character","Logical"};
               WIDTHS {70,60,120,40};
               FIELDS { 'Test->Datev' , 'Test->Numeric' , 'Test->Character' , 'Test->Logical'} ;
               JUSTIFY {BROWSE_JTFY_LEFT,BROWSE_JTFY_RIGHT, BROWSE_JTFY_LEFT,BROWSE_JTFY_CENTER} ;
               FONT 'font_1' ;  // "MS Sans serif" SIZE nBrwFont ; // 09 ;
               Value 1;
               LOCK;
               TOOLTIP "Double Click to edit";
               ON DBLCLICK  (ThisWindow.Object):Post(2)  ;
               ON CHANGE  ( (ThisWindow.Object):Post(4), ;
                            (ThisWindow.Object):Post(5) )

      :O:aFr2[3] := This.Browse_1.Width + :GapsWidth * 2
               
      :Y := This.Text_2b.Row + :GapsHeight
      DEF SAY Label_1a COLS WIDTH :O:nBrwSayLen VALUE "Date"   BOLD 
      DEF GET Text_5   ROWS WIDTH :D ;
          FIELD test->Datev ;
          ON LOSTFOCUS LostFocus2Get() ;
          ON DBLCLICK  (ThisWindow.Object):Post(12, This.Text_5.Index) ; //DublClick2Get() ;
          TOOLTIP "Text_5. DublClick --> Edit" ;
          BACKCOLOR   :O:BColorGet   ; 
          PICTURE '@D';
          GOTFOCUSSELECT ;
          READONLY
          
      :X := :O:nLeft2
      DEF SAY Label_1b COLS WIDTH :O:nBrwSayLen VALUE "Num."   BOLD 
      DEF GET Text_6   ROWS WIDTH :W1 ;
          FIELD test->Numeric ;
          ON LOSTFOCUS LostFocus2Get() ;
          ON DBLCLICK (ThisWindow.Object):Post(12, This.Text_6.Index) ; // DublClick2Get() ;
          TOOLTIP "Numeric field. VALID {|| (!EMPTY(This.Value).AND.This.Value<=99999)} . WHEN {|| This.Value > 99}" ;
          BACKCOLOR   :O:BColorGet   ;
          PICTURE "@KB 999999";
          VALID {|| (!EMPTY(This.Value).AND.This.Value<=99999)} ;
          WHEN {|| This.Value > 99} ;
          GOTFOCUSSELECT ;
          READONLY 
          
      :X := :O:nLeft2
      DEF SAY Label_1c      COLS WIDTH :O:nBrwSayLen VALUE "Char."  BOLD 
      DEF GET Text_7   ROWS COLS ;
          FIELD test->Character  ;
          ON LOSTFOCUS LostFocus2Get() ;
          ON DBLCLICK (ThisWindow.Object):Post(12, This.Text_7.Index) ; // DublClick2Get() ;
          TOOLTIP "Characters field.  DublClick --> Edit" ;
          VALIDMESSAGE "Can not be empty!. VALID {|| (!EMPTY(This.Value))} . PICTURE @K !XXXXXXXXXXXXXXXX ";
          VALID {|| (!EMPTY(This.Value))} ;
          PICTURE "@K !XXXXXXXXXXXXXXXX";
          BACKCOLOR   :O:BColorGet   ;
          GOTFOCUSSELECT ;
          READONLY 

      :O:nLeft3 := :X + :GapsWidth 

      :X := :O:nLeft2
      DEF SAY Label_1d COLS WIDTH :O:nBrwSayLen VALUE "Logic." BOLD 
      DEF GET Text_8   ROWS WIDTH :O:nBoolLen ;
          FIELD test->Logical;
          ON LOSTFOCUS LostFocus2Get() ;
          ON DBLCLICK (ThisWindow.Object):Post(12, This.Text_8.Index) ; //DublClick2Get() ;
          BACKCOLOR   :O:BColorGet   ; 
          FONTCOLOR   :O:FColor2     ;
          BOLD;
          TOOLTIP "Logical field. DublClick --> Edit" ;
          CENTERALIGN ;
          READONLY 
          
      :O:aFr2[4] := :Y - :O:aFr2[1]

      :Y := This.Text_5.Row
      :X := :O:nLeft3
      DEF BTNEX Button_1   ROWS WIDTH 1.4 CAPTION "Save"   FONTCOLOR {200,0,0} BOLD  ;
                ACTION  (ThisWindow.Object):Post(This.Cargo)   ;
                ON INIT This.Cargo := 1                 // saveDateNow()
      DEF BTNEX Button_2   ROWS WIDTH 1.4 CAPTION "Edit"   FONTCOLOR {200,0,0} BOLD  ;
                ACTION  (ThisWindow.Object):Post(This.Cargo)   ;
                ON INIT This.Cargo := 2                 // UnlockData()
      DEF BTNEX Button_3   ROWS WIDTH 1.4 CAPTION "Cancel" FONTCOLOR {200,0,0} BOLD  ;
                ACTION  (ThisWindow.Object):Post(This.Cargo)   ;
                ON INIT This.Cargo := 3                 // CancelData()
                
      @ :O:aFr2[1], :O:aFr2[2] FRAME Frame_2 Caption "" WIDTH :O:aFr2[3] HEIGHT :O:aFr2[4]
      
      :Width  := :O:nMaxX + :Right + :GapsWidth + GetBorderWidth()
      :Height := MAX( :Y, :O:nMaxY ) + :StatusBar:Height + ;
                 GetBorderHeight() + GetTitleHeight() + GetMenuBarHeight()

                                             // ---- Window events
      :Event( 1, {|ow| SaveDateNow() } )             // Button_1
      :Event( 2, {|ow| UnlockData()  } )             // Button_2
      :Event( 3, {|ow| CancelData()  } )             // Button_3
      :Event( 4, {|ow| ow:StatusBar:Say(hb_ntos(TEST->( RecNo() )), 2) } )
      :Event( 5, {|ow| RefreshData()  } )
      :Event( 6, {|  | MsgBox('LDblClick :' + CRLF + _HMG_Value() + ;
              This.Name + ' = ' + cValToChar( This.Value ) + CRLF + ;
              'Focused   :' + This.FocusedControl, ThisWindow.Name ) } )
      :Event( 7, {|  | MsgBox('VK_F5 : '    + CRLF + _HMG_Value() + ;
              This.Name + ' = ' + cValToChar( This.Value ) + CRLF + ;
              'Focused   :' + This.FocusedControl, ThisWindow.Name ) } )
      :Event( 8, {|  | MsgBox(This.Name + ' = ' + cValToChar( This.DisplayValue ), ThisWindow.Name ) } )
      :Event( 9, {|  | MsgBox(This.Name + ' = ' + cValToChar( This.Value ) + CRLF + ;
                       "Valtype: " + ValType( This.Value ), ThisWindow.Name ) } )
      :Event(10, {|  | This.Enabled := .F., This.Browse_1.SetFocus } )
      :Event(11, {|  | This.Enabled := .T., This.SetFocus } )
      :Event(12, {|  | DublClick2Get() } )
      :Event(13, {|ow,ky,np| TONE( np ) } )
      :Event(14, {|oc| HMG_Alert('VK_F6 : '  + CRLF + _HMG_Value() + ;
         This.Name   + ' = ' + cValToChar( This.Value ) + CRLF + ;
         'Focused: ' + This.FocusedControl + CRLF + ;
         'Window : ' + oc:Window:Name, , "Information", ICON_INFORMATION) } )
      
      END WITH

      EnabledBtn(.F.)
  
      This.Browse_1.ColumnsAutoFitH
     (This.Object):Post(4)
     (This.Object):Post(5)
      
   END WINDOW

   Form_1.Center    //   Form_1.Activate

   ACTIVATE WINDOW Form_1 // INIT _logfile(.T., This.Name, _HMG_Value())

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
   s += ThisWindow.Name + CRLF

RETURN s
      
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

   (ThisWindow.Object):Send( This.&(cName).Cargo  )

   If !Empty(cFocus)
      This.&(cFocus).SetFocus
   EndIf

RETURN Nil

*-----------------------------
STATIC FUNC DublClick2Get()
*-----------------------------
   LOCAL cGet := This.FocusedControl
   LOCAL xVal := This.&(cGet).Value

   If This.ReadOnly
      ButtonPress('Button_2', cGet)
   EndIf
   
RETURN Nil

*-----------------------------
STAT PROC UnlockData()
*-----------------------------
   IF !RLOCK()
      MsgStop("Record occupied by another user")
      return
   endif

  RefreshData()
  ReadOnlyData(.F.)
  EnabledBtn(.T.)

  This.Text_5.SetFocus

Return

*-----------------------------
STAT PROC RefreshData()
*-----------------------------

  This.Text_5.FontColor := (ThisWindow.Object):O:FColor1
  This.Text_6.FontColor := (ThisWindow.Object):O:FColor1
  This.Text_7.FontColor := (ThisWindow.Object):O:FColor1
  This.Text_8.FontColor := (ThisWindow.Object):O:FColor2

  This.Text_5.Refresh
  This.Text_6.Refresh
  This.Text_7.Refresh
  This.Text_8.Refresh
  DO EVENTS

Return

*-----------------------------
STAT PROC ReadOnlyData( lROnly )
*-----------------------------

  This.Text_5.Readonly := lROnly
  This.Text_6.Readonly := lROnly
  This.Text_7.Readonly := lROnly
  This.Text_8.Readonly := lROnly
  DO EVENTS

Return

*-----------------------------
STAT PROC EnabledBtn( lEnable )
*-----------------------------

  This.Button_1.Enabled :=   lEnable
  This.Button_2.Enabled := ! lEnable
  This.Button_3.Enabled :=   lEnable
  DO EVENTS

Return

*-----------------------------
STAT PROC saveDateNow()
*-----------------------------

  IF RLOCK()
     This.Text_5.Save
     This.Text_6.Save
     This.Text_7.Save
     This.Text_8.Save
     UNLOCK
  else
     RETURN
  endif

  RefreshData()
  ReadOnlyData(.T.)
  EnabledBtn(.F.)

  This.Browse_1.Refresh
  This.Browse_1.SetFocus

return

*-----------------------------
Stat Function CancelData()
*-----------------------------

  RefreshData()
  ReadOnlyData(.T.)
  EnabledBtn(.F.)

  This.Browse_1.SetFocus

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
Stat Function DBTESTCREATE(ufile)
*-----------------------------
Local aDbf := {}

  AADD (aDbf,{"Datev"      , "D",  8,0})
  AADD (aDbf,{"Numeric"    , "N",  5,0})
  AADD (aDbf,{"Character"  , "C",  20,0})
  AADD (aDbf,{"Logical"    , "L",  1,0})
  dbcreate( ufile, aDbf, 'DBFNTX' )
  aDbf := {}

Return NIL

*-----------------------------
Function Compare(dDate)
*-----------------------------
   if empty(dDate) .or. dDate < date()
      return .f.
   endif
return .t.

*-----------------------------
Function _Trans(xval)
*-----------------------------
   Local RetVal:=""

   if VALTYPE(xVAL)=="C"
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

   DEFINE MAIN MENU

      POPUP 'Set Parameters'
      ITEM "Set parameters: 12 110 100" ACTION {|cp| cp := ["12,110,100"], ;
                                                (App.Object):Send(2, 2, cp) }
      ITEM "Set parameters: 14 120 110" ACTION {|cp| cp := ["14,120,110"], ;
                                                (App.Object):Send(2, 2, cp) }
      ITEM "Set parameters: 16 140 120" ACTION {|cp| cp := ["16,140,120"], ;
                                                (App.Object):Send(2, 2, cp) }
      ITEM "Set parameters: 18 150 130" ACTION {|cp| cp := ["18,150,130"], ;
                                                (App.Object):Send(2, 2, cp) }
      ITEM "Set parameters: 20 170 140" ACTION {|cp| cp := ["20,170,140"], ;
                                                (App.Object):Send(2, 2, cp) }
      ITEM "Set parameters: 22 180 150" ACTION {|cp| cp := ["22,180,150"], ;
                                                (App.Object):Send(2, 2, cp) }
      SEPARATOR
      ITEM "Set parameters: 12 110 100 Arial" ACTION {|cp| cp := ["12,110,100, Arial"], ;
                                                       (App.Object):Send(2, 2, cp) }
      ITEM "Set parameters: 14 120 110 Arial" ACTION {|cp| cp := ["14,120,110, Arial"], ;
                                                       (App.Object):Send(2, 2, cp) }
      ITEM "Set parameters: 16 140 120 Arial" ACTION {|cp| cp := ["16,140,120, Arial"], ;
                                                       (App.Object):Send(2, 2, cp) }
      ITEM "Set parameters: 18 150 130 Arial" ACTION {|cp| cp := ["18,150,130, Arial"], ;
                                                       (App.Object):Send(2, 2, cp) }
      ITEM "Set parameters: 20 170 140 Arial" ACTION {|cp| cp := ["20,170,140, Arial"], ;
                                                       (App.Object):Send(2, 2, cp) }
      ITEM "Set parameters: 22 180 150 Arial" ACTION {|cp| cp := ["22,180,150, Arial"], ;
                                                       (App.Object):Send(2, 2, cp) }
      SEPARATOR
      ITEM "Set parameters: Default" ACTION {|cp| cp := [""], (App.Object):Send(2, 2, cp) }
      END POPUP

      POPUP '&Get Value'
      ITEM "Get Text_1  Value" ACTION (ThisWindow.Object):Post(9, This.Text_1.Index)  MESSAGE "Vale and ValueType"
      ITEM "Get Text_2  Value" ACTION (ThisWindow.Object):Post(9, This.Text_2.Index)  MESSAGE "Vale and ValueType"
      ITEM "Get Text_3  Value" ACTION (ThisWindow.Object):Post(9, This.Text_3.Index)  MESSAGE "Vale and ValueType"
      ITEM "Get Text_4  Value" ACTION (ThisWindow.Object):Post(9, This.Text_4.Index)  MESSAGE "Vale and ValueType"
      ITEM "Get Text_2a Value" ACTION (ThisWindow.Object):Post(9, This.Text_2a.Index) MESSAGE "Vale and ValueType"
      ITEM "Get Text_2b Value" ACTION (ThisWindow.Object):Post(9, This.Text_2b.Index) MESSAGE "Vale and ValueType"
      ITEM "Get Text_2c Value" ACTION (ThisWindow.Object):Post(9, This.Text_2c.Index) MESSAGE "Vale and ValueType"
      END POPUP

      POPUP 'Get &DisplayValue'
      ITEM "Get Text_1 DisplayValue"  ACTION (ThisWindow.Object):Post(8, This.Text_1.Index)
      ITEM "Get Text_2 DisplayValue"  ACTION (ThisWindow.Object):Post(8, This.Text_2.Index)
      ITEM "Get Text_3 DisplayValue"  ACTION (ThisWindow.Object):Post(8, This.Text_3.Index)
      ITEM "Get Text_4 DisplayValue"  ACTION (ThisWindow.Object):Post(8, This.Text_4.Index)
      ITEM "Get Text_2a DisplayValue" ACTION (ThisWindow.Object):Post(8, This.Text_2a.Index)
      ITEM "Get Text_2b DisplayValue" ACTION (ThisWindow.Object):Post(8, This.Text_2b.Index)
      ITEM "Get Text_2c DisplayValue" ACTION (ThisWindow.Object):Post(8, This.Text_2c.Index)
      END POPUP

      POPUP 'Disable/Enable'
      ITEM "Enable Text_1"   ACTION (ThisWindow.Object):Post(11, This.Text_1.Index) 
      ITEM "Enable Text_2"   ACTION (ThisWindow.Object):Post(11, This.Text_2.Index) 
      ITEM "Enable Text_3"   ACTION (ThisWindow.Object):Post(11, This.Text_3.Index) 
      ITEM "Enable Text_4"   ACTION (ThisWindow.Object):Post(11, This.Text_4.Index) 
      ITEM "Enable Text_2a"  ACTION (ThisWindow.Object):Post(11, This.Text_2a.Index)
      ITEM "Enable Text_2b"  ACTION (ThisWindow.Object):Post(11, This.Text_2b.Index)
      ITEM "Enable Text_2c"  ACTION (ThisWindow.Object):Post(11, This.Text_2c.Index)

      SEPARATOR
      ITEM "Disable Text_1"  ACTION (ThisWindow.Object):Post(10, This.Text_1.Index)
      ITEM "Disable Text_2"  ACTION (ThisWindow.Object):Post(10, This.Text_2.Index) 
      ITEM "Disable Text_3"  ACTION (ThisWindow.Object):Post(10, This.Text_3.Index) 
      ITEM "Disable Text_4"  ACTION (ThisWindow.Object):Post(10, This.Text_4.Index) 
      ITEM "Disable Text_2a" ACTION (ThisWindow.Object):Post(10, This.Text_2a.Index)
      ITEM "Disable Text_2b" ACTION (ThisWindow.Object):Post(10, This.Text_2b.Index)
      ITEM "Disable Text_2c" ACTION (ThisWindow.Object):Post(10, This.Text_2c.Index)
      END POPUP

   END MENU

RETURN Nil
