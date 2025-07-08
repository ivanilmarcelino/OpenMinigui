/*
 * MINIGUI - Harbour Win32 GUI library Demo
 */

#include "minigui.ch"

ANNOUNCE RDDSYS

*----------------------------------------------------------------------------*
FUNCTION Main(...)
*----------------------------------------------------------------------------*
   LOCAL cForm
   LOCAL oForm := oHmgData()

   oForm:FormName  := "wMain"
   oForm:Title     := 'MiniGUI Demo App'
   oForm:BackColor := SILVER
   oForm:Main      := .T.
   oForm:Width     := 640
   oForm:Height    := 480

   App.Cargo:Form_Main := oForm

   Create_IniApp( oForm )

   cForm := oForm:FormName

   DEFINE WINDOW &cForm WIDTH oForm:Width HEIGHT oForm:Height ;
          TITLE oForm:Title ;
          MAIN TOPMOST ;
          BACKCOLOR oForm:BackColor ;
          ON INIT _wPost(0)

      (This.Object):Event( 0, {|| 
            Local h  := This.Handle, t, a
            Local o  := This.Cargo
            Local oc := App.Cargo, oi
            WaitWindow("Processing ...", .T.)
            ? procname(), h 
            _o2log(o, , "=> Cargo", .T.) ; ?
            _o2log(oc,, "=> App.Cargo", .T.) ; ?
            t := oc:tDateTimeStart
            ? "oc:tDateTimeStart =", t, hb_valtoexp(t)
            ?
            oi := TIniData():New(oc:COMMON:cIniFile, .T.):Read()
            ? oi:cIni
            ? repl("=", len(oi:cIni))
            _o2log(oi, , "=> oIni", .T.) 
            ?
            ? repl("~", len(oi:cIni))
            FOR EACH a IN oi:GetAll()
                ? hb_enumindex(a), a[1], a[2]
                IF HB_ISOBJECT( a[2] )
                   _o2log(a[2]:GetAll(),, "  >>> ["+a[1]+"]", .T.) 
                ELSE
                   _o2log(a[2], , "  =>", .T.) 
                ENDIF
            NEXT
            ?
            WaitWindow()
            Return Nil
            })
      (This.Object):Event(99, {|ow| ow:Release() })

   END WINDOW

     CENTER WINDOW &cForm
   ACTIVATE WINDOW &cForm

RETURN NIL

FUNCTION Create_IniApp(oForm)
   LOCAL a, oi, oc := App.Cargo

   // create ini file from App.Cargo values
   oi := TIniData():New(oc:COMMON:cIniFile, .T.)
   oi:nMinKey := 15           // minimum key length
   FOR EACH a IN oc:GetAll()
       IF HB_ISOBJECT( a[2] )
          oi:Set(a[1], a[2])
       ELSE
          Default oi:MAIN := oHmgData()
          oi:MAIN:Set(a[1], a[2])
       ENDIF
   NEXT
   oi:MAIN:oForm := oForm

RETURN oi:Write()

*----------------------------------------------------------------------------*
INIT PROCEDURE Sets_ENV()
*----------------------------------------------------------------------------*
   LOCAL o, cFont, nSize

   SET DATE     TO GERMAN
   SET DECIMALS TO 4
   SET EPOCH    TO 2000
   SET EXACT    ON
   SET SOFTSEEK ON
   SET CENTURY  ON
   SET AUTOPEN  OFF
   SET DELETED  OFF

   SET MULTIPLE QUIT
   SET WINDOW MAIN OFF

   SET OOP ON

   hb_FileDelete("_MsgLog.txt") // delete file before appointment _msg.log

   App.Cargo := oHmgData()
   App.Cargo:tDateTimeStart := hb_DateTime()
   App.Cargo:cFormGotFocus  := ""

   App.Cargo:COMMON := oHmgData() ; o := App.Cargo:COMMON  // [COMMON] section

   o:cIniFile       := StrTran(lower(App.ExeName), ".exe", ".ini")
   o:cLogFile       := "_msg.log"
   o:lLogFileDel    := .T.
   o:lDebug         := .T.
   o:lWithDraw      := .T.
   o:cDefAppIcon    := "MAIN"
   o:cDefFontName   := "Arial"
   o:nDefFontSize   := 12
   o:cDlgFontName   := "DejaVu Sans Mono"
   o:nDlgFontSize   := 14                
   o:aDlgBackColor  := { 141, 179, 226 }        // Alert* BackColor
   o:aDlgFontColor  := {  0 ,  0 ,  0  }        // Alert* FontColor
   o:aGetBackColor  := { 141, 179, 226 }        // GetBox BackColor
   o:aGetFontColor  := {  0 ,  0 ,  0  }        // GetBox FontColor
   o:lCloseYesNo    := .F.
   o:nMenuBmpHeight := 32

   _SetGetLogFile( o:cLogFile )

   IF o:lLogFileDel ; hb_FileDelete( o:cLogFile )
   ENDIF

   IF o:lDebug ; SET LOGERROR ON
   ELSE        ; SET LOGERROR OFF
   ENDIF
   // Setting ...
   cFont := o:cDefFontName ; nSize := o:nDefFontSize
   // Default font 
   SET FONT TO cFont , nSize
   //
   SET DEFAULT ICON TO o:cDefAppIcon
   SET WINDOW MODAL PARENT HANDLE ON
   SET TOOLTIPSTYLE BALLOON
   SET NAVIGATION EXTENDED
   SET MENUSTYLE  EXTENDED     
   SetMenuBitmapHeight( o:nMenuBmpHeight )  

   App.Cargo:FONTS := oHmgData() ; o := App.Cargo:FONTS
   // Default font
   cFont := App.Cargo:COMMON:cDlgFontName
   nSize := App.Cargo:COMMON:nDlgFontSize
   // TsBrowse                     bold italic
   o:Normal   := {cFont, nSize    , .F., .F.}
   o:Bold     := {cFont, nSize    , .T., .F.}
   o:Italic   := {cFont, nSize - 2, .F., .F.}
   o:ItalBold := {cFont, nSize - 2, .T., .T.}
   // other 
   o:Ital     := {cFont, nSize    , .F., .T.}
   o:BoldItal := {cFont, nSize    , .T., .T.}
   // Alert* font
   cFont := App.Cargo:COMMON:cDlgFontName
   nSize := App.Cargo:COMMON:nDlgFontSize
   o:DlgFont  := {cFont, nSize, .F., .F.}

   AEval(o:GetAll(), {|a| _DefineFont( a[1], a[2][1], a[2][2], a[2][3], a[2][4]) })
   // Alert* colors
   SET MSGALERT BACKCOLOR  TO App.Cargo:COMMON:aDlgBackColor
   SET MSGALERT FONTCOLOR  TO App.Cargo:COMMON:aDlgFontColor

RETURN
