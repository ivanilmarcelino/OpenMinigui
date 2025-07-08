/*
 * MINIGUI - Harbour Win32 GUI library Demo
 */

#include "hmg.ch"
#include "tsbrowse.ch"

REQUEST DBFCDX, HB_MEMIO  // Request necessary database and memory IO support

#define SHOW_TITLE "Testing DEMO zip to mem:"

//----------------------------------------------------------------------------//
// Main function: initializes the GUI and processes the ZIP file
//----------------------------------------------------------------------------//
FUNCTION Main(...)
   LOCAL cZipFile, bUpdate, lWithPath, cPassword, cPathOut, acFiles, bProgress
   LOCAL aDbf := {}, aNam := {}   // Arrays to store extracted file names
   LOCAL nI, cI, cN, nY, nX, nW, nH, hSpl

   // Define ZIP file path
   cZipFile := App.Cargo:PthZip + "test.zip"

   cPathOut  := "mem:"       // Extract to memory
   lWithPath := cPathOut != "mem:"  // Check if we need to maintain paths

   // Callback function to update file lists when extracting
   bUpdate   := {|cZip, nPos, cFil| AAdd(aDbf, cFil), nPos := cZip, ;
                                    AAdd(aNam, hb_FNameName(cFil) ) }  

   // Callback to log extraction progress
   bProgress := {|nRead, nSize, cFil| _logfile(.T., nRead, nSize, cFil) }

   WaitWindow( "... Wait for the preparation to complete ...", .T. )

   // Extract ZIP contents into memory
   hb_UnzipFile( cZipFile, bUpdate, lWithPath, cPassword, cPathOut, acFiles, bProgress )
   
   WaitWindow()

   // Window dimensions
   nY := nX := 0
   nW := Sys.ClientWidth 
   nH := 120

   // Define Main Window
   DEFINE WINDOW wMain AT nY, nX WIDTH nW HEIGHT nH TITLE SHOW_TITLE ;
          MAIN  NOMAXIMIZE  NOSIZE ;
          ON INIT    _wPost( 0) ;  // Post event 0 on initialization
          ON RELEASE _wSend(90)    // Post event 90 on release

      // Store extracted DBF filenames
      This.Cargo := oHmgData()
      This.Cargo:aDbf := aDbf
      This.Cargo:aNam := aNam
      This.Cargo:nDbf := 0

      // Define Status Bar
      DEFINE STATUSBAR BOLD
         STATUSITEM ''
         STATUSITEM '' WIDTH Int(nW * 0.3)
         STATUSITEM '' WIDTH Int(nW * 0.3)
      END STATUSBAR

      DEFINE SPLITBOX HANDLE hSpl  // GUI split box for layout

      // Define Toolbar with extracted file buttons
      DEFINE TOOLBAR ToolBar_1 CAPTION "MEM:" BUTTONSIZE 100,32 FLAT
         FOR nI := 1 TO Len(aNam)
             IF nI > 9 ; EXIT  // Limit to first 9 files
             ENDIF
             cN := StrZero(nI, 2)   // Format index with leading zero
             cI := "n" + hb_ntos(nI)
             BUTTON &cN CAPTION aNam[nI] PICTURE cI SEPARATOR  ;
                         ACTION ( This.Enabled := .F., _wPost(1,, This.Name) )
         NEXT
      END TOOLBAR

      // Define Exit Button Toolbar
      DEFINE TOOLBAR ToolBar_2 CAPTION "" BUTTONSIZE 42,32 FLAT
         BUTTON 99 CAPTION 'Exit' PICTURE 'exit' ;
                   ACTION ( This.Enabled := .F., _wPost(Val(This.Name),, This.Name) )
      END TOOLBAR

      END SPLITBOX

      // Window Events Handling
      WITH OBJECT This.Object
       :Event( 0, {|| NIL })   // Placeholder event

       // Event triggered when a file button is clicked
       :Event( 1, {|ow,ky,cn| 
                   LOCAL cdb, cnm, nn := Val(cn), cFnt := "Normal"
                   LOCAL i, n := 0, w, h
                   cdb := ow:Cargo:aDbf[nn]  // Get file path
                   cnm := ow:Cargo:aNam[nn]  // Get file name
                   USE ( cdb ) ALIAS ( cnm ) NEW   // Open DBF file
                   FOR i := 1 TO fCount() ; n += FieldLen( i )
                   NEXT
                   w := int( GetFontWidth(cFnt, n + 20) * 0.8 )
                   h := LastRec()
                   h := iif( h < 10, 10, h ) * ( GetFontHeight(cFnt) + 2 )
                   h := iif( h > Sys.ClientHeight, Sys.ClientHeight, h )
                   SBrowse( cnm, cdb, , , w, h, , .T., .T. )  // Display in GUI
                   USE
                   ky := cn
                   This.&(cn).Enabled := .T.
                   Return Nil
                   })

       // Event triggered when window is closed
       :Event(90, {|ow| 
                   LOCAL cdb
                   ow:Hide()
                   FOR EACH cdb IN ow:Cargo:aDbf ; hb_vfErase(cdb) // Delete extracted files
                   NEXT
                   Return Nil
                   })

       :Event(99, {|ow| ow:Release() })  // Exit application
      END WITH

   END WINDOW

   ACTIVATE WINDOW wMain

RETURN NIL

//----------------------------------------------------------------------------//
// Initialize Application Environment
//----------------------------------------------------------------------------//
INIT PROCEDURE Sets_ENV()
   LOCAL o, cIni  := hb_FNameExtSet( App.ExeName, ".ini" )
   LOCAL a := hb_AParams()

   SET EPOCH     TO 2000
   SET DATE      TO GERMAN
   SET CENTURY   ON
   SET DELETED   OFF
   SET AUTOPEN   ON
   SET EXACT     ON
   SET EXCLUSIVE ON
   SET SOFTSEEK  ON
   SET OOP ON
   SET DATE FORMAT TO "DD.MM.YYYY"

   rddSetDefault( "DBFCDX" )  // Default RDD for DBF files

   IF !HB_ISOBJECT( App.Cargo ) ; App.Cargo := oHmgData()
   ENDIF

   o := App.Cargo
   o:PthZip := hb_DirBase() + hb_ps() 
   o:ExtDbf := ".dbf"
   o:ExtInd := ".cdx"
   o:ExtMem := ".fpt"
   o:tStart := hb_DateTime()   // Log start time

   // Application font settings
   o:cFontName := "Arial"
   o:nFontSize := 12
   o:cLogFile  := "trace.log"
   o:lLogDel   := .T.
   o:cDlgFont  := "DejaVu Sans Mono"
   o:nDlgSize  := o:nFontSize + 2

   // Alert window colors
   o:aDlgBColor := { 141, 179, 226 }  // Background color
   o:aDlgFColor := {  0 ,  0 ,  0  }  // Font color

   o:cDefAppIcon := "iMG48"
   o:lDebug := .T.

   o:nMenuBmpHeight := 32
   o:aDisplayMode   := { Sys.ClientWidth , Sys.ClientHeight }
   o:cDisplayMode   := hb_ntos(o:aDisplayMode[1]) + "x" + hb_ntos(o:aDisplayMode[2])

   o:cIni := cIni

   IF hb_FileExists( cIni ) 
      o:oIni := TIniData():New(cIni, .T.):Read()
   ENDIF

   Default o:oIni := oHmgData()

   _SetGetLogFile( o:cLogFile )

   IF o:lLogDel 
      hb_FileDelete( o:cLogFile )
   ENDIF

   SET FONT TO o:cFontName , o:nFontSize
   // TsBrowse                                            bold italic
   _DefineFont("Normal"  , o:cFontName    , o:nFontSize  , .F., .F. )
   _DefineFont("Bold"    , o:cFontName    , o:nFontSize  , .T., .F. )
   _DefineFont("Italic"  , o:cFontName    , o:nFontSize-2, .F., .T. )
   _DefineFont("ItalBold", "Comic Sans MS", o:nFontSize-1, .T., .T. )
   // Alert* font
   _DefineFont("DlgFont" , o:cDlgFont     , o:nDlgSize   , .F., .F. )
   _DefineFont("DlgFont2", "Courier New"  , o:nDlgSize   , .F., .F. )
   // Alert* colors
   SET MSGALERT BACKCOLOR  TO o:aDlgBColor
   SET MSGALERT FONTCOLOR  TO o:aDlgFColor
   //
   SET DEFAULT ICON TO o:cDefAppIcon
   SET WINDOW MAIN  OFF
   SET WINDOW MODAL PARENT HANDLE ON
   SET TOOLTIPSTYLE BALLOON
   SET NAVIGATION EXTENDED
   SET MENUSTYLE  EXTENDED

   Set ShowRedAlert On        // increase font of window "Program Error"
   SetMenuBitmapHeight( o:nMenuBmpHeight )

RETURN

