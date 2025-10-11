/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2002-06 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
 *
 * (c) 2006-2025 MiniGUI Team
 * 2025 Add Zoom Save / MEM  and Restore By Pierpaolo Martinello
 */

#ifndef __XHARBOUR__
#include "mgextern.ch"
#endif
#include "minigui.ch"
#include "fileio.ch"

#define APP_TITLE "Main Demo"
#define APP_ABOUT "Free GUI Library For Harbour"
#define IDI_MAIN 1001
#define MsgInfo( c ) MsgInfo( c, , , .f. )

Static lchng := .F.

FUNCTION MAIN()
   LOCAL cIniFile := GetStartupFolder() + '\demo.ini'
   Public undoVar, aRct
   SET DATE BRITISH
   SET TOOLTIPSTYLE BALLOON
   SET AUTOZOOMING ON

   If !FILE( cInifile )
      SetIniValue( cIniFile )
   Endif

   m->aRct := GetIniValue(cIniFile)

   DEFINE WINDOW Form_1 ;
      ROW M->aRct[1,1] ;
      COL M->aRct[1,2] ;
      WIDTH 650 HEIGHT 415 + iif( IsVistaOrLater(), 8, 0 ) ;
      TITLE 'Harbour MiniGUI Demo - ' + if( IsExe64(), "64", "32" ) + " bits, ComCtl32.dll version " + GetComCtl32Ver() ;
      ICON IDI_MAIN ;
      Main ;
      NOSHOW ;
      ON INIT ( Autoset ( This.name ) );
      ON RELEASE  ONQUIT(cIniFile) ;
      ON MINIMIZE Minimize_Click() ;
      NOTIFYTOOLTIP "MiniGUI Main Demo" ;
      ON NOTIFYDBLCLICK Notify_Click()

   DEFINE STATUSBAR
      STATUSITEM ' HMG Power Ready!'
   END STATUSBAR

   ON KEY ALT + A ACTION MsgInfo( "Pierpaolo Martinello's 2025 Demo.")

   DEFINE MAIN MENU
   POPUP '&File'

      ITEM 'Get active controls' ACTION Listactivenames( m->undovar )
      ITEM 'Undo posiztion and Size'  ACTION AutoSet( "Form_1" )
      ITEM 'Save Coordinate and size' ACTION SetIniValue( cIniFile, {thisWindow.row,thisWindow.col,thisWindow.width,thisWindow.height } )
      SEPARATOR
      ITEM 'Exit' ACTION Form_1.RELEASE
   END POPUP

   POPUP 'H&elp'
      ITEM 'About' ACTION AlertInfo ( MiniGuiVersion ( 1 ) + ";" + APP_ABOUT, APP_TITLE )
      ITEM 'Versions' ACTION AlertInfo ( "GUI Library       : " + App.Cargo:Version + ";" + ;
         "xBase Compiler: " + Version() + ";" + ;
         "C Compiler       : " + hb_Ccompiler(), 'Versions' )
   END POPUP

   END MENU

   DEFINE NOTIFY MENU
      ITEM '&Restore' ACTION Notify_CLick()
      SEPARATOR
      ITEM 'E&xit' ACTION Form_1.RELEASE
   END MENU

   @ 200, 140 CHECKBUTTON CheckButton_1 ;
      CAPTION 'CheckButton' ;
      VALUE .T. ;
      TOOLTIP 'MiniPrint / HbPrinter switcher'

   @ 200, 247 BUTTON ImageButton_1 ;
      PICTURE 'button.bmp' ;
      ACTION PRINTPIE() ;
      WIDTH 27 HEIGHT 27 TOOLTIP 'Print Preview' ;

   @ 230, 247 BUTTONEX CtrlName_1 ;
      ICON 'igear' ;
      ACTION Listactivenames( m->undovar ) ;
      WIDTH 62 HEIGHT 57 BACKCOLOR RED TOOLTIP 'Get active name controls'  NOXPSTYLE  ;

      @ 200, 282 CHECKBUTTON CheckButton_2 ;
      PICTURE 'open.bmp' WIDTH 27 HEIGHT 27 ;
      VALUE .F. ;
      TOOLTIP 'Graphical CheckButton'

   DEFINE TAB Tab_1 ;
      AT 5, 180 ;
      WIDTH 440 ;
      HEIGHT 180 ;
      VALUE 1 ;
      TOOLTIP 'Tab Control'

   PAGE '&Grid'

   @ 30, 10 GRID Grid_1 ;
      WIDTH 420 ;
      HEIGHT 140 ;
      HEADERS { '', 'Last Name', 'First Name' } ;
      WIDTHS { 0, 220, 220 } ;
      ITEMS { { 0, 'Simpson', 'Homer' }, { 1, 'Mulder', 'Fox' } } VALUE 1 ;
      TOOLTIP 'Grid Control' ;
      ON HEADCLICK { {|| MsgInfo( 'Header 1 Clicked !' ) }, {|| MsgInfo( 'Header 2 Clicked !' ) } } ;
      IMAGE { "br_no", "br_ok" } ;
      ON DBLCLICK MsgInfo ( 'Double Click event', 'Grid' ) ;
      ON RCLICK MsgInfo ( 'Right Click event', 'Grid' )

   END PAGE

   PAGE '&Misc.'

   @ 45, 80 FRAME TabFrame_1 WIDTH 130 HEIGHT 110

   @ 55, 90 LABEL Label_99 ;
      VALUE '&This is a Label !!!' ;
      WIDTH 115 HEIGHT 27

   @ 80, 90 CHECKBOX Check_1 ;
      CAPTION 'Check 1' ;
      VALUE .T. ;
      TOOLTIP 'CheckBox' ;
      ON CHANGE PLAYOK()

   @ 115, 85 SLIDER Slider_1 ;
      RANGE 1, 10 ;
      VALUE 5 ;
      TOOLTIP 'Slider' ;
      ON CHANGE PLAYOK()

   @ 45, 240 FRAME TabFrame_2 WIDTH 125 HEIGHT 110

   @ 50, 260 RADIOGROUP Radio_1 ;
      OPTIONS { 'One', 'Two', 'Three', 'Four' } ;
      VALUE 1 ;
      WIDTH 100 ;
      TOOLTIP 'RadioGroup' ON CHANGE PLAYOK() AUTOSIZE

   END PAGE

   PAGE '&EditBox'

   @ 30, 10 EDITBOX Edit_1 ;
      WIDTH 410 ;
      HEIGHT 140 ;
      VALUE 'EditBox!!' ;
      TOOLTIP 'EditBox' ;
      MAXLENGTH 255

   END PAGE

   PAGE '&ProgressBar'

   @ 80, 120 PROGRESSBAR Progress_1 RANGE 0, 65535

   @ 80, 250 BUTTON Btn_Prg OF FOrm_1 ;
      CAPTION '<- !!!' ;
      ACTION Animate_CLick() ;
      WIDTH 50 ;
      HEIGHT 28 ;
      TOOLTIP 'Animate Progressbar'

   END PAGE

   END TAB

   @ 10, 15 DATEPICKER Date_1 ;
      VALUE CToD( '  / /  ' ) ;
      TOOLTIP 'DatePicker Control' ;

   @ 45, 15 BTNTEXTBOX Text_3 ;
      WIDTH 100 ;
      VALUE '' ;
      ACTION Form_1.Text_3.VALUE := GetFolder( 'Select Folder:' ) ;
      PICTURE "open.bmp" ;
      BUTTONWIDTH 22 ;
      TOOLTIP { 'Button TextBox', 'Select Folder' }

   @ 80, 15 SPINNER Spinner_1 ;
      RANGE 0, 10 ;
      VALUE 5 ;
      WIDTH 100 ;
      TOOLTIP 'Range 0,10'

   @ 230, 140 BUTTONEX Button_7 ;
      CAPTION 'GetValue' ;
      ACTION GetValue_Click()

   @ 260, 140 BUTTONEX Button_8 ;
      CAPTION 'SetValue' ;
      ACTION SetValue_Click()

    @ 200, 527 IMAGE Image_1 ;
      PICTURE 'Demo.PNG' ;
      WIDTH 87 ;
      HEIGHT 87

   @ 115, 15 COMBOBOX Combo_1 ;
      ITEMS { 'One', 'Two', 'Three' } ;
      VALUE 2 ;
      TOOLTIP 'ComboBox'

   @ 201, 317 LISTBOX List_1 ;
      WIDTH 90 ;
      HEIGHT 86 ;
      ITEMS { 'Andres', 'Analia', 'Item 3', 'Item 4', 'Item 5', 'Item 6' } ;
      VALUE 2 ;
      TOOLTIP 'ListBox' ;
      ON DBLCLICK MsgInfo( 'Double Click!', 'ListBox' )

   @ 200, 15 TEXTBOX Text_Pass ;
      VALUE 'Secret' ;
      PASSWORD ;
      TOOLTIP 'Password TextBox' ;
      MAXLENGTH 16 ;
      UPPERCASE

   @ 231, 15 TEXTBOX Text_1 ;
      WIDTH 50 ;
      VALUE 'Hi!!!' ;
      TOOLTIP 'TextBox' ;
      MAXLENGTH 16 ;
      LOWERCASE ;
      ON ENTER MsgInfo( 'Enter pressed' )

   @ 231 , 65 TEXTBOX MaskedText ;
      WIDTH 70 ;
      VALUE 1234.12 ;
      TOOLTIP "TextBox With Numeric And InputMask Clauses" ;
      NUMERIC ;
      INPUTMASK '9999.99' ;
      ON CHANGE PlayOk() ;
      ON ENTER MsgInfo( 'Enter pressed' ) ;
      RIGHTALIGN

   @ 262, 15 TEXTBOX Text_2 ;
      VALUE 123 ;
      NUMERIC ;
      TOOLTIP 'Numeric TextBox' ;
      MAXLENGTH 16 RIGHTALIGN

   @ 160, 15 LABEL Label_2 ;
      VALUE 'Timer Test:'

   @ 160, 90 LABEL Label_3 TRANSPARENT

    DEFINE TIMER Timer_1 ;
      INTERVAL 1000 ;
      ACTION Form_1.Label_3.VALUE := Time()

   @ 200, 415 BUTTONEX Button_17 ;
          CAPTION 'Restore'+CRLF+CRLF+'original' ;
	      ACTION  ( AutoSet( "Form_1" , .T. ), UndoVar() ) ;
	 	  HEIGHT 88 ;
          BACKCOLOR AQUA ;
          TOOLTIP 'Restore position , size and Controls values!';
          NOXPSTYLE

   @ 305, 15 LABEL Label_OR Value "Undo Row / Col / Width / Height from previous sizing or Ini" ;
         Action AutoSet( "Form_1" ) ;
         OnMouseHover CursorHand() ;
         WIDTH 390 FONT "ARIAL" size 9 BACKCOLOR YELLOW BORDER Vcenteralign

   @ 305,415 LABEL Label_Save Value  Space (4) + "Save coordinate and size" ;
         Action  SetIniValue( cIniFile, {thisWindow.row,thisWindow.col,thisWindow.width,thisWindow.height } ) ;
         OnMouseHover CursorHand() ;
         WIDTH 200 FONT "ARIAL" size 9 backcolor GREEN FONTCOLOR BLUE BORDER Vcenteralign

	END WINDOW

   SET TOOLTIP BACKCOLOR TO WHITE OF Form_1

   SET TOOLTIP TEXTCOLOR TO RED OF Form_1

   ADD TOOLTIPICON INFO WITH MESSAGE "Information" OF Form_1

   Form_1.Image_1.ToolTip := 'Image Control'
   Form_1.Radio_1.ToolTip := 'RadioGroup Control'
   Form_1.Label_99.ToolTip := 'Label Control'
   Form_1.Spinner_1.ToolTip := GetProperty( 'Form_1', 'Spinner_1', 'tooltip' ) + ' with default step 1'

   ON KEY ESCAPE OF FORM_1 ACTION Form_1.Release

   Form_1.minWidth   := 650
   Form_1.minheight  := 415 + iif( IsVistaOrLater(), 8, 0 ) ;

   ACTIVATE WINDOW Form_1 ;
            ON INIT ( _HMG_aFormNotifyIconName[ GetFormIndex( "Form_1" ) ] := IDI_MAIN, This.Center() , m->UndoVar := Mem_Read() )

RETURN NIL
/*
*/
*-----------------------------------------------------------------------------*
PROCEDURE ONQUIT (cIniFile )
*-----------------------------------------------------------------------------*
if lchng
   if msgYesNo ("Do you want to save the coordinates and size?","Question:",.T.)
      SetIniValue( cIniFile, {thisWindow.row,thisWindow.col,thisWindow.width,thisWindow.height } )
   Endif
Endif
Return
/*
*/
*-----------------------------------------------------------------------------*
PROCEDURE AutoSet( cFname, lRestore, aSize )
*-----------------------------------------------------------------------------*
  Local hfrm , aret, aSz
   DEFAULT cFname to this.name, lRestore to .F., aSize To { 650, 415 + iif( IsVistaOrLater(), 8, 0 ) }
   hfrm := Getproperty( cFname,"handle")
   aRet := GetIniValue( GetStartupFolder() + '\demo.ini' )

   If valtype(aret) == "L"
      MsgStop( chr(9)+"Autoset Alert"+CRLF+"Wrong ini parameters","Error!")
      Return
   Endif

   If aret[1,4] = 0
      Return
   Endif

   If lRestore
      aSz := aSize
      Setproperty (cFname, "width",  aRet[1,3] )
      Setproperty (cFname, "Height", aRet[1,4] )
      aRet[1,3]:= asz[1]
      aRet[1,4]:= Asz[2]
   Endif

   If aRet[1,1] <= 0 .or. lRestore
      domethod( cFname,"center")
   Else
      Setproperty (cFname, "Row", aRet[1,1] )
      Setproperty (cFname, "Col", aRet[1,2] )
   Endif

   _AutoAdjust( hFrm )

   Setproperty (cFname, "width",  aRet[1,3] )
   Setproperty (cFname, "Height", aRet[1,4] )

   _AutoAdjust( hFrm )

   lchng := .T.
   DoMethod(cFname,"show")

Return
/*
*/
*-----------------------------------------------------------------------------*
PROCEDURE Animate_CLick
*-----------------------------------------------------------------------------*
   LOCAL i

   FOR i = 0 TO 65535 STEP 25
      Form_1.Progress_1.VALUE := i
   NEXT i

RETURN
/*
*/
*-----------------------------------------------------------------------------*
FUNCTION GetComCtl32Ver
*-----------------------------------------------------------------------------*
   LOCAL nVer := GETCOMCTL32DLLVER()
   LOCAL nMajor := HiWord( nVer )
   LOCAL nMinor := LoWord( nVer )

RETURN hb_ntos( nMajor ) + "." + hb_ntos( nMinor )
/*
*/
*-----------------------------------------------------------------------------*
PROCEDURE Notify_CLick
*-----------------------------------------------------------------------------*
   LOCAL FormHandle := GetFormHandle( "Form_1" )

   Restore_CLick()
   SetForegroundWindow( FormHandle )
   ShowNotifyIcon( FormHandle, .F., NIL, NIL )

RETURN
/*
*/
*-----------------------------------------------------------------------------*
PROCEDURE Minimize_CLick
*-----------------------------------------------------------------------------*
   LOCAL i := GetFormIndex( "Form_1" )

   If _HMG_aFormMiscData1[ i ][ 1 ] == NIL
      _HMG_aFormMiscData1[ i ][ 1 ] := LoadTrayIcon( GetInstance(), _HMG_aFormNotifyIconName[ i ] )
   ENDIF

   ShowNotifyIcon( _HMG_aFormhandles[ i ], .T., ;
      _HMG_aFormMiscData1[ i ][ 1 ], _HMG_aFormNotifyIconToolTip[ i ] )
   Form_1.Hide

RETURN
/*
*/
*-----------------------------------------------------------------------------*
PROCEDURE Restore_CLick
*-----------------------------------------------------------------------------*

   Form_1.RESTORE

RETURN
/*
*/
*-----------------------------------------------------------------------------*
PROCEDURE GetValue_CLick
*-----------------------------------------------------------------------------*
   LOCAL s

   s :=     "Grid:                " + Str ( Form_1.Grid_1.Value ) + Chr( 13 ) + Chr( 10 )
   s := s + "TextBox:             " + Form_1.Text_1.VALUE +Chr( 13 ) + Chr( 10 )
   s := s + "EditBox:             " + Form_1.Edit_1.VALUE +Chr( 13 ) + Chr( 10 )
   s := s + "RadioGroup:          " + Str ( Form_1.Radio_1.Value ) + Chr( 13 ) + Chr( 10 )
   s := s + "Tab:                 " + Str ( Form_1.Tab_1.Value ) + Chr( 13 ) + Chr( 10 )
   s := s + "ListBox:             " + Str ( Form_1.List_1.Value ) + Chr( 13 ) + Chr( 10 )
   s := s + "ComboBox:            " + Str ( Form_1.Combo_1.Value ) + Chr( 13 ) + Chr( 10 )
   s := s + "CheckBox:          " + iif ( Form_1.Check_1.VALUE, ".T.", ".F." ) + Chr( 13 ) + Chr( 10 )
   s := s + "Numeric TextBox:     " + Str ( Form_1.Text_2.Value ) + Chr( 13 ) + Chr( 10 )
   s := s + "Password TextBox:    " + Form_1.Text_Pass.VALUE +Chr( 13 ) + Chr( 10 )
   s := s + "Slider:       " + Str ( Form_1.Slider_1.Value ) + Chr( 13 ) + Chr( 10 )
   s := s + "Spinner:             " + Str ( Form_1.Spinner_1.Value ) + Chr( 13 ) + Chr( 10 )
   s := s + "TextBox (InputMask): " + Str ( Form_1.MaskedText.Value ) + Chr( 13 ) + Chr( 10 )
   s := s + "DatePicker:          " + DToC( Form_1.Date_1.Value )

   MsgInfo ( s, "Get Control Values" )

RETURN
/*
*/
*-----------------------------------------------------------------------------*
PROCEDURE SetValue_CLick
*-----------------------------------------------------------------------------*

   Form_1.Grid_1.VALUE := 2
   Form_1.Text_1.VALUE := "New Text value"
   Form_1.Edit_1.VALUE := "New Edit Value"
   Form_1.Radio_1.VALUE := 4
   Form_1.Tab_1.VALUE := 2
   Form_1.Check_1.VALUE := .T.
   Form_1.List_1.VALUE := 1
   Form_1.Combo_1.VALUE := 1
   Form_1.Date_1.VALUE := CToD( "02/02/2002" )
   Form_1.Text_2.VALUE := 999
   Form_1.Timer_1.VALUE := 500
   Form_1.MaskedText.VALUE := 12.12
   Form_1.Spinner_1.VALUE := 6

RETURN
/*
*/
*------------------------------------------------------------------------------*
FUNCTION PRINTPIE
*------------------------------------------------------------------------------*
   LOCAL cDisk := "C:", aPos := GetCursorPos()
   LOCAL nFree := Round( hb_DiskSpace( cDisk, HB_DISK_FREE ) / 1073741824, 2 )
   LOCAL nTotal := Round( hb_DiskSpace( cDisk, HB_DISK_TOTAL ) / 1073741824, 2 ) - .01
   LOCAL nUsed := nTotal - nFree

   SET FONT TO _GetSysFont(), 8

   If IsWindowDefined ( Form_2 )
      DoMethod("Form_2","RELEASE")
      DoEvents()
   ENDIF

   DEFINE WINDOW Form_2 ;
         AT aPos[ 1 ], aPos[ 2 ] WIDTH 240 HEIGHT 240 ;
         TITLE cDisk ;
         CHILD ;
         NOCAPTION ;
         NOSHOW

   END WINDOW

   ACTIVATE WINDOW Form_2 NOWAIT

   If Form_1.CheckButton_1.VALUE == .T.

      PRINT GRAPH IN WINDOW Form_2 AT 10, 20 ;
         TO 190, 200 ;
         TITLE "Drive " + cDisk + " (Total (GB) - " + LTrim( Str( nTotal ) ) + ")" ;
         TYPE PIE ;
         SERIES { nUsed, nFree } ;
         DEPTH 10 ;
         SERIENAMES { "Used (GB)", "Free (GB)" } ;
         COLORS { { 0, 0, 255 }, { 255, 0, 255 } } ;
         3DVIEW SHOWXVALUES SHOWLEGENDS

   ELSE

      PRINT GRAPH IN WINDOW Form_2 AT 10, 20 ;
         TO 190, 200 ;
         TITLE "Drive " + cDisk + " (Total (GB) - " + LTrim( Str( nTotal ) ) + ")" ;
         TYPE PIE ;
         SERIES { nUsed, nFree } ;
         DEPTH 10 ;
         SERIENAMES { "Used (GB)", "Free (GB)" } ;
         COLORS { { 0, 0, 255 }, { 255, 0, 255 } } ;
         3DVIEW SHOWXVALUES SHOWLEGENDS ;
         LIBRARY HBPRINT

   ENDIF

   SET FONT TO _GetSysFont(), GetDefaultFontSize()

RETURN NIL
/*
*/
*-----------------------------------------------------------------------------*
PROCEDURE SetIniValue( cIni, aVal )
*-----------------------------------------------------------------------------*
   DEFAULT aVal to {-1,100,650,415 + iif( IsVistaOrLater(), 8, 0 ) }

   BEGIN INI FILE cIni
      SET BEGIN COMMENT TO "it's a top line."
      SET SECTION 'Form_1' ENTRY 'Row' TO aVal[1]
      SET SECTION 'Form_1' ENTRY 'Col' TO aVal[2]
      SET SECTION 'Form_1' ENTRY 'Height' TO aVal[3]
      SET SECTION 'Form_1' ENTRY 'Width' TO aVal[4]
      SET END COMMENT TO Time() + " it's a bottom line."
      SET BEGIN COMMENT TO Time() + " it's a first line."
   END INI
   lchng := .F.
RETURN
/*
*/
*-----------------------------------------------------------------------------*
FUNCTION GetIniValue( cIni )
*-----------------------------------------------------------------------------*
   LOCAL R, C, H , W

   BEGIN INI FILE (cIni)
      GET R SECTION 'Form_1' ENTRY 'Row' DEFAULT 100
      GET C SECTION 'Form_1' ENTRY 'Col' DEFAULT 100
      GET H SECTION 'Form_1' ENTRY 'Height' DEFAULT 640
      GET W SECTION 'Form_1' ENTRY 'Width' DEFAULT 480
   END INI

RETURN { { R, C, H , W } }
/*
*/
*-----------------------------------------------------------------------------*
Function ListActiveNames( )
*-----------------------------------------------------------------------------*
Local cList :='', aUserType

      // This array is not the law but only a sample for the names of the most active checks
       aUserType := {"TEXTBOX","BTNTEXTBOX","BTNNUMTEXTBOX","EDITBOX","MONTHCALENDAR","SPINNER";
        , "SLIDER","RADIOGROUP","LISTBOX","DATEPICKER","TIMEPICKER","RICHEDIT","RICHEDITBOXEX","IPADDRESS";
        , "COMBOBOX","CHECKBOX","CHECKBUTTON","GETBOX","CHECKLABEL","PROGRESSBAR" }

      Mem_Read( aUserType , "Form_1", .T. )

Return cList
/*
*/
*--------------------------------------------------------*
Function Mem_Read( aCtrl, cSWin, lRpt )
*--------------------------------------------------------*
    LOCAL hRtv := {=>}, alCTrl
    Default cSWin TO ThisWindow.Name, lRpt to .F.

    Default aCtrl TO {"TEXTBOX","BTNTEXTBOX","BTNNUMTEXTBOX","EDITBOX","MONTHCALENDAR","SPINNER";
        , "SLIDER","RADIOGROUP","LISTBOX","DATEPICKER","TIMEPICKER","RICHEDIT","RICHEDITBOXEX","IPADDRESS";
        , "COMBOBOX","CHECKBOX","CHECKBUTTON","GETBOX","CHECKLABEL","PROGRESSBAR" }

    HB_hCaseMatch( hRtv,.F.) // deactivates the capital letters/tiny

    alCTrl := HMG_GetFormControls ( csWin , aCtrl )

    // I build the array of the names of the fields preceded by the name of the window that manages them
    aEval ( alCtrl , {|x| hRtv[ cSWin+"."+x ] :=  GetProperty( cSwin, x , Check_Extra(x , cSwin)  ) } )

    If lRpt
        IF Len (hrtv) > 0
            MEM_SAY (hRtv)
        Else
            msgstop("No active controls Found.")
        Endif
    Endif

    Return hRtv
/*
*/
*--------------------------------------------------------*
Function Check_Extra ( cKey , cSwin )
*--------------------------------------------------------*
lOCAL cRtv := "VALUE"
DO CASE
   CASE GETCONTROLTYPE(CkEY,CSwIN) == "CHECKLABEL"
        cRtv := "CHECKED"
   CASE GETCONTROLTYPE(CkEY,CSwIN) == "RICHEDIT"
        cRtv := "RICHVALUE"
ENDCASE

RETURN CrTV
/*
*/
*--------------------------------------------------------*
Procedure Undovar( lTotale ,cSwin )
*--------------------------------------------------------*
    Default cSWin TO ThisWindow.Name, lTotale TO .f.
    Mem_Write( iif (lTotale,NIL,m->undoVar) ,cSwin )
Return
/*
*/
*--------------------------------------------------------*
PROCEDURE Mem_Write( cKey , cSwin )
*--------------------------------------------------------*
    LOCAL aChiavi, cChiave, xValore , cWin , cCtrl
    Default cSWin TO ThisWindow.Name, cKey to m->undoVar

    if ValType ( cKey ) == "H"
        aChiavi := HB_Hkeys( cKey )
    ElseIf ValType ( cKey ) == "C"
        aChiavi := { cKey }
    Endif
    // Let's process write
    FOR EACH cChiave IN aChiavi
        // You specified the form
        if At ( "." ,cChiave ) > 0
            cWin    := substr(cChiave,1, at(".",cChiave)-1 )
            cCtrl   := substr(cChiave,at(".",cChiave)+1 )
            xValore := m->undovar [ cChiave ]
        Else
            cWin    := cSwin
            cCtrl   := cChiave
            if ! empty( cChiave )
                xValore := cKey [ cSwin+"."+cChiave ]
            Endif
        Endif
        // Check that the control and the correct form was called
        if GetControlIndex( cCtrl, cWin ) > 0
           SetProperty( cWin,cCtrl, Check_Extra( cCtrl , cWin), xValore )
        Else
            if Len(cCtrl) > 0
                Msgstop("Wrong control name: "+cCtrl,"Mem_WRITE in "+cSWin )
            Endif
        Endif
    NEXT
RETURN
/*
*/
*--------------------------------------------------------*
PROCEDURE MEM_Say( hMyHash , lFld )
*--------------------------------------------------------*
    LOCAL aChiavi, cChiave, xValore , cWin ,cRpt := ""
    Default hMyHash to hash(), lFld to .F.

    // We get all the keys
    aChiavi := HB_Hkeys( hMyHash )

    // Let's process and create the report
    FOR EACH cChiave IN aChiavi
        cWin := substr(cChiave,1, at(".",cChiave)-1 )
        xValore := hMyHash[ cChiave ]
        crpt += PadR(substr(cChiave,at(".",cChiave)+1),13)+chr(9)+ ": = "+ltrim(hb_valtoExp(xValore)) +CRLF
    NEXT
    IF !empty(cRpt)
        MsgInfo( cRpt ,;
            iif ( lfld ,"Value fields ", "Active inputs " ) ;
            + "in the window "+ cWin )
    Endif
    RETURN
/*
*/