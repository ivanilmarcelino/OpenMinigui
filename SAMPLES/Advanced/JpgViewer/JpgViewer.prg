/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2002-2007 Roberto Lopez <harbourminigui@gmail.com>
 *
 * Copyright 2003-2022 Grigory Filatov <gfilatov@inbox.ru>
 */

/*
  edited by Alexey L. Gustow <gustow33 at mail.ru> (see "gugu") :

   1. wrong JPEG width/height (if EXIF exists) - see JpgSize()
   2. add button "Zoom Fill" and function ZoomFill(),
      add hotkey GRAY MULTIPLY for this operation
*/

ANNOUNCE RDDSYS

#include "minigui.ch"
#include "winprint.ch"

#xcommand ON KEY SPACE [ OF <parent> ] ACTION <action> ;
      = > ;
      _DefineHotKey ( < "parent" >, 0, VK_SPACE, < { action } > )

#xcommand ON KEY GRAYPLUS [ OF <parent> ] ACTION <action> ;
      = > ;
      _DefineHotKey ( < "parent" >, 0, VK_ADD, < { action } > )

#xcommand ON KEY GRAYMINUS [ OF <parent> ] ACTION <action> ;
      = > ;
      _DefineHotKey ( < "parent" >, 0, VK_SUBTRACT, < { action } > )

// gugu
#xcommand ON KEY GRAYMULTIPLY [ OF <parent> ] ACTION <action> ;
      = > ;
      _DefineHotKey ( < "parent" >, 0, VK_MULTIPLY, < { action } > )
//

#define PROGRAM 'JpgViewer'
#define VERSION ' version 1.2.3'
#define COPYRIGHT ' Grigory Filatov, 2003-2022'

#define MsgAlert( c ) MsgEXCLAMATION( c, "Error" )

STATIC nWidth, nHeight, cFile := "", nKoef := 1, aFiles := {}, nItem := 1, nLoadTime

MEMVAR nScrWidth, nScrHeight

*--------------------------------------------------------*
FUNCTION Main( fname )
*--------------------------------------------------------*
   LOCAL nLen, aRect := GetDesktopArea()
   PRIVATE nScrWidth := aRect[ 3 ] - aRect[ 1 ], nScrHeight := aRect[ 4 ] - aRect[ 2 ]

   DEFAULT fname := "JpgViewer.jpg"

   aFiles := Directory( "*.JPG" )
   AEval( Directory( "*.JPEG" ), {| e | AAdd( aFiles, e ) } )
   nLen := Len( aFiles )
   nItem := AScan( aFiles, {| e | Upper( e[ 1 ] ) = Upper( fname ) } )

   SET CENTURY ON
   SET DATE GERMAN

   DEFINE WINDOW Form_Main ;
         WIDTH 640 HEIGHT 480 ;
         TITLE PROGRAM ;
         ICON 'IMAGE' ;
         MAIN ;
         ON INIT FileOpen( fname, .T. ) ;
         FONT 'MS Sans Serif' ;
         SIZE 9

      DEFINE MAIN MENU

         DEFINE POPUP "&File"
            MENUITEM "&Open" + Chr( 9 ) + "   Ctrl+O" ACTION FileOpen()
            SEPARATOR
            MENUITEM "&Print" + Chr( 9 ) + "   Ctrl+P" ACTION FilePrint()
            SEPARATOR
            MENUITEM "E&xit" + Chr( 9 ) + "   Esc" ACTION ReleaseAllWindows()
         END POPUP

         DEFINE POPUP "&View"
            MENUITEM "&Information" + Chr( 9 ) + "   Ctrl+I" ACTION PictInfo()
            SEPARATOR
            MENUITEM "&Zoom In" + Chr( 9 ) + "   Gray +" ACTION Zoom( 1 )
            MENUITEM "Zoom &Out" + Chr( 9 ) + "   Gray -" ACTION Zoom( -1 )

            // gugu
            MENUITEM "Zoom &Fill" + Chr( 9 ) + "   Gray *" ACTION ZoomFill()
            //

            MENUITEM "O&riginal size" + Chr( 9 ) + "   Ctrl+H" ACTION Zoom( 0 )
         END POPUP

         DEFINE POPUP "&Help"
            MENUITEM "&About" ACTION MsgAbout()
         END POPUP
      END MENU

      // gugu
      @ 6, 235 TEXTBOX Text_1 HEIGHT 20 WIDTH 80 VALUE iif( Empty( nLen ), "", hb_ntos( nItem ) + "/" + hb_ntos( nLen ) )
      //

      DEFINE SPLITBOX

         DEFINE TOOLBAR ToolBar_1 BUTTONSIZE 20, 20 FLAT

            BUTTON Button_1 ;
               PICTURE 'Open' ;
               TOOLTIP 'Open' ;
               ACTION FileOpen()

            BUTTON Button_2 ;
               PICTURE 'Print' ;
               TOOLTIP 'Print' ;
               ACTION FilePrint() SEPARATOR

            BUTTON Button_3 ;
               PICTURE 'Info' ;
               TOOLTIP 'Info' ;
               ACTION PictInfo()

            BUTTON Button_4 ;
               PICTURE 'Zout' ;
               TOOLTIP 'Zoom out' ;
               ACTION Zoom( 1 )

            BUTTON Button_5 ;
               PICTURE 'Zin' ;
               TOOLTIP 'Zoom in' ;
               ACTION Zoom( -1 )

            BUTTON Button_6 ;
               PICTURE 'ZFill' ;
               TOOLTIP 'Zoom Fill' ;
               ACTION ZoomFill()

            BUTTON Button_7 ;
               PICTURE 'Left' ;
               TOOLTIP 'Previous' ;
               ACTION Next( -1 )

            BUTTON Button_8 ;
               PICTURE 'Right' ;
               TOOLTIP 'Next' ;
               ACTION Next( 1 )

         END TOOLBAR

      END SPLITBOX

      Form_Main.Button_2.Enabled := ! Empty( nLen )
      Form_Main.Button_3.Enabled := ! Empty( nLen )
      Form_Main.Button_4.Enabled := ! Empty( nLen )
      Form_Main.Button_5.Enabled := ! Empty( nLen )
      Form_Main.Button_6.Enabled := ! Empty( nLen )
      Form_Main.Button_7.Enabled := ( nLen > 1 )
      Form_Main.Button_8.Enabled := ( nLen > 1 )

      DEFINE STATUSBAR
         STATUSITEM "No file loaded" WIDTH 80
         STATUSITEM "" WIDTH 52
         STATUSITEM "" WIDTH 52 ACTION Zoom( 0 )
         STATUSITEM "" WIDTH 80
         STATUSITEM "" WIDTH 124
         STATUSITEM "" WIDTH 200
      END STATUSBAR

      ON KEY ESCAPE ACTION ReleaseAllWindows()

      ON KEY F12 ACTION Form_Main.MINIMIZE

      ON KEY CONTROL + O ACTION FileOpen()

      ON KEY CONTROL + P ACTION FilePrint()

      ON KEY GRAYPLUS ACTION Zoom( 1 )

      ON KEY GRAYMINUS ACTION Zoom( -1 )

      // gugu
      ON KEY GRAYMULTIPLY ACTION ZoomFill()
      //

      ON KEY CONTROL + I ACTION PictInfo()

      ON KEY CONTROL + H ACTION Zoom( 0 )

      ON KEY LEFT ACTION Next( -1 )

      ON KEY RIGHT ACTION Next( 1 )

      ON KEY UP ACTION ( nItem := 1, Next( 0 ) )

      ON KEY DOWN ACTION ( nItem := Len( aFiles ), Next( 0 ) )

      ON KEY BACK ACTION Next( -1 )

      ON KEY SPACE ACTION Next( 1 )

   END WINDOW

   CENTER WINDOW Form_Main

   ACTIVATE WINDOW Form_Main

RETURN NIL

*--------------------------------------------------------*
STATIC FUNCTION FileOpen( fname, lInit )
*--------------------------------------------------------*
   LOCAL cPath := "\" + CurDir() + IF( Empty( CurDir() ), "", "\" ), ;
      aSize, nLen := Len( aFiles ), nFormWidth, nFormHeight, nStart

   DEFAULT fname := "", lInit := .F.

   IF Empty( fname )
      fname := GetFile( { { "Image files (*.jpg, *.jpeg)", "*.jpg;*.jpeg" } }, "Open", cPath )
   ELSE
      fname := IF( At( '"', fname ) > 0, StrTran( fname, '"', '' ), fname )
   ENDIF

   IF ! Empty( fname ) .AND. File( fname )
      cFile := fname
      IF ! lInit
         aFiles := Directory( "*.JPG" )
         AEval( Directory( "*.JPEG" ), {| e | AAdd( aFiles, e ) } )
         nLen := Len( aFiles )
         nItem := AScan( aFiles, {| e | Upper( e[ 1 ] ) = Upper( cFileNoPath( cFile ) ) } )
      ENDIF
      nKoef := 1

      aSize := JpgSize( cFile )
      nWidth := IF( aSize[ 1 ] > 10000, nScrWidth, aSize[ 1 ] )
      nHeight := IF( aSize[ 2 ] > 10000, nScrHeight, aSize[ 2 ] )

      DO WHILE .T.
         nFormWidth := Round( nWidth * nKoef, 0 ) + GetBorderWidth() + 4
         nFormHeight := Round( nHeight * nKoef, 0 ) + GetTitleHeight() + GetBorderHeight() + GetMenuBarHeight() + GetToolBarHeight() + Form_Main.Statusbar.HEIGHT + 2
         IF ( nFormWidth <= nScrWidth .AND. nFormHeight <= nScrHeight ) .OR. nKoef < 0.16
            EXIT
         ENDIF
         nKoef -= iif( nWidth > 2000, 0.05, 0.02 )
      ENDDO

      Form_Main.WIDTH := IF( nFormWidth < 400, 400, nFormWidth )
      Form_Main.HEIGHT := IF( nFormHeight < 100, 100, nFormHeight )

      IF ! Form_Main.Button_2.Enabled .OR. ! lInit
         Form_Main.Button_2.Enabled := .T.
         Form_Main.Button_3.Enabled := .T.
         Form_Main.Button_4.Enabled := .T.
         Form_Main.Button_5.Enabled := .T.
         Form_Main.Button_6.Enabled := .T.
         Form_Main.Button_7.Enabled := ( nLen > 1 )
         Form_Main.Button_8.Enabled := ( nLen > 1 )
      ENDIF
      Form_Main.Text_1.VALUE := LTrim( Str( nItem ) ) + "/" + LTrim( Str( nLen ) )

      Form_Main.TITLE := cFileNoPath( fname ) + ' - ' + PROGRAM + ;
         IF( nKoef # 1, " (Zoom: " + LTrim( Str( Round( nKoef * nWidth, 0 ) ) ) + " x " + LTrim( Str( Round( nKoef * nHeight, 0 ) ) ) + ")", "" )
      Form_Main.StatusBar.Item( 1 ) := LTrim( Str( nWidth ) ) + " x " + LTrim( Str( nHeight ) )
      Form_Main.StatusBar.Item( 2 ) := LTrim( Str( nItem ) ) + "/" + LTrim( Str( nLen ) )
      Form_Main.StatusBar.Item( 3 ) := Str( nKoef * 100, 4 ) + " %"
      Form_Main.StatusBar.Item( 4 ) := LTrim( Str( aFiles[ nItem ][ 2 ] / 1024 ) ) + " KB"
      Form_Main.StatusBar.Item( 5 ) := DToC( aFiles[ nItem ][ 3 ] ) + " / " + aFiles[ nItem ][ 4 ]

      IF IsControlDefined( Image_1, Form_Main )
         Form_Main.Image_1.RELEASE
      ENDIF

      nStart := Seconds()

      @ 35, 0 IMAGE Image_1 OF Form_Main ;
         PICTURE cFile ;
         ON CLICK Next( 1 ) ;
         WIDTH nKoef * nWidth ;
         HEIGHT nKoef * nHeight

      nLoadTime := Seconds() - nStart

      Form_Main.Image_1.SetFocus
      Form_Main.Center()

   ENDIF

RETURN NIL

*--------------------------------------------------------*
STATIC FUNCTION Zoom( nOp )
*--------------------------------------------------------*
   LOCAL nPictWidth, nPictHeight

   IF Form_Main.StatusBar.Item( 1 ) == "No file loaded"
      RETURN NIL
   ENDIF

   IF Empty( nOp ) .AND. nKoef == 1
      RETURN NIL
   ENDIF

   IF Int( nKoef * 10 ) # Round( nKoef, 1 ) * 10
      nKoef := Int( nKoef * 10 ) / 10
   ENDIF

   IF nOp < 0 .AND. nKoef > 0.11
      nKoef -= 0.1
   ELSEIF nOp > 0 .AND. nKoef < 9.9
      nKoef += 0.1
   ELSE
      nKoef := 1
   ENDIF

   nPictWidth := Round( nWidth * nKoef, 0 )
   nPictHeight := Round( nHeight * nKoef, 0 )

   Form_Main.WIDTH := nPictWidth + GetBorderWidth() + 4
   Form_Main.HEIGHT := nPictHeight + GetTitleHeight() + GetBorderHeight() + GetMenuBarHeight() + GetToolBarHeight() + Form_Main.Statusbar.HEIGHT + 2

   IF Form_Main.Width < 400
      Form_Main.WIDTH := 400
   ENDIF
   IF Form_Main.Height < 100
      Form_Main.HEIGHT := 100
   ENDIF

   Form_Main.TITLE := cFileNoPath( cFile ) + ' - ' + PROGRAM + ;
      IF( nKoef # 1, " (Zoom: " + hb_ntos( nPictWidth ) + " x " + hb_ntos( nPictHeight ) + ")", "" )
   Form_Main.StatusBar.Item( 3 ) := Str( nKoef * 100, 4 ) + " %"

   IF IsControlDefined( Image_1, Form_Main )
      Form_Main.Image_1.RELEASE
   ENDIF

   @ 35, 0 IMAGE Image_1 OF Form_Main ;
      PICTURE cFile ;
      ON CLICK Next( 1 ) ;
      WIDTH nKoef * nWidth ;
      HEIGHT nKoef * nHeight

   Form_Main.Image_1.SetFocus
   Form_Main.Center()

RETURN NIL

*--------------------------------------------------------*
STATIC FUNCTION ZoomFill() // gugu (Gustow)
*--------------------------------------------------------*
   LOCAL nPictWidth, nPictHeight

   LOCAL nFormWidth, nFormHeight

   IF Form_Main.StatusBar.Item( 1 ) == "No file loaded"
      RETURN NIL
   ENDIF

   // from FileOpen()
   nKoef := 1
   DO WHILE .T.
      nFormWidth := Round( nWidth * nKoef, 0 ) + GetBorderWidth() + 4
      nFormHeight := Round( nHeight * nKoef, 0 ) + GetTitleHeight() + GetBorderHeight() + GetMenuBarHeight() + GetToolBarHeight() + Form_Main.Statusbar.HEIGHT + 2
      IF ( nFormWidth <= nScrWidth .AND. nFormHeight <= nScrHeight ) .OR. nKoef < 0.16
         EXIT
      ENDIF
      nKoef -= 0.02
   ENDDO
   //

   // from Zoom()
   nPictWidth := Round( nWidth * nKoef, 0 )
   nPictHeight := Round( nHeight * nKoef, 0 )

   Form_Main.WIDTH := nPictWidth + GetBorderWidth() + 4
   Form_Main.HEIGHT := nPictHeight + GetTitleHeight() + GetBorderHeight() + GetMenuBarHeight() + GetToolBarHeight() + Form_Main.Statusbar.HEIGHT + 2

   IF Form_Main.Width < 400
      Form_Main.WIDTH := 400
   ENDIF
   IF Form_Main.Height < 100
      Form_Main.HEIGHT := 100
   ENDIF

   Form_Main.TITLE := cFileNoPath( cFile ) + ' - ' + PROGRAM + ;
      IF( nKoef # 1, " (Zoom: " + hb_ntos( nPictWidth ) + " x " + hb_ntos( nPictHeight ) + ")", "" )
   Form_Main.StatusBar.Item( 3 ) := Str( nKoef * 100, 4 ) + " %"

   IF IsControlDefined( Image_1, Form_Main )
      Form_Main.Image_1.RELEASE
   ENDIF

   @ 35, 0 IMAGE Image_1 OF Form_Main ;
      PICTURE cFile ;
      ON CLICK Next( 1 ) ;
      WIDTH nKoef * nWidth ;
      HEIGHT nKoef * nHeight

   Form_Main.Image_1.SetFocus
   Form_Main.Center()

RETURN NIL

*--------------------------------------------------------*
STATIC FUNCTION PictInfo()
*--------------------------------------------------------*
   LOCAL aLabel := {}, cLabel, aText := {}, cText, n

   IF ! Empty( cFile )

      AAdd( aLabel, "File name:" )
      AAdd( aLabel, "Folder:" )
      AAdd( aLabel, "Compression:" )
      AAdd( aLabel, "Original size:" )
      AAdd( aLabel, "Disk size:" )
      AAdd( aLabel, "Current folder index:" )
      AAdd( aLabel, "File date/time:" )
      AAdd( aLabel, "Loaded in:" )

      AAdd( aText, cFileNoPath( cFile ) )
      AAdd( aText, CurDrive() + ":\" + CurDir() + IF( Empty( CurDir() ), "", "\" ) )
      AAdd( aText, "JPEG" )
      AAdd( aText, LTrim( Str( nWidth ) ) + " x " + LTrim( Str( nHeight ) ) + " Pixels" )
      AAdd( aText, LTrim( Str( aFiles[ nItem ][ 2 ] / 1024 ) ) + " KB (" + LTrim( Str( aFiles[ nItem ][ 2 ] ) ) + " Bytes)" )
      AAdd( aText, LTrim( Str( nItem ) ) + " / " + LTrim( Str( Len(aFiles ) ) ) )
      AAdd( aText, DToC( aFiles[ nItem ][ 3 ] ) + " / " + aFiles[ nItem ][ 4 ] )
      AAdd( aText, LTrim( Str( Int(nLoadTime * 1000 ) ) ) + " milliseconds" )

      DEFINE WINDOW Form_Info AT 0, 0 ;
            WIDTH 360 HEIGHT 300 ;
            TITLE PROGRAM + " - Image properties" ;
            MODAL ;
            NOSIZE ;
            NOSYSMENU ;
            ON INIT Form_Info.Button_OK.SetFocus ;
            FONT 'MS Sans Serif' ;
            SIZE 9

         @ 8, 8 FRAME Frame_1 ;
            WIDTH 338 ;
            HEIGHT 212

         FOR n := 1 TO Len( aLabel )
            cLabel := "Label_" + LTrim( Str( n ) )
            @ (n - 1 ) * 24 + 23, 20 LABEL &cLabel ;
               VALUE aLabel[ n ] ;
               WIDTH 100 HEIGHT 20
         NEXT

         FOR n := 1 TO Len( aText )
            cText := "Text_" + LTrim( Str( n ) )
            @ (n - 1 ) * 24 + 21, 134 TEXTBOX &cText ;
               VALUE aText[ n ] ;
               HEIGHT 20 ;
               WIDTH 200 ;
               READONLY
         NEXT

         @ Form_Info.HEIGHT - 68, Form_Info.Width / 2 - 50 BUTTON Button_OK ;
            CAPTION "OK" ;
            ACTION Form_Info.RELEASE ;
            WIDTH 96 HEIGHT 32 - IF( IsThemed(), 6, 0 ) ;
            DEFAULT

         ON KEY ESCAPE ACTION ThisWindow.Release()

      END WINDOW

      CENTER WINDOW Form_Info

      ACTIVATE WINDOW Form_Info

   ENDIF

RETURN NIL

*--------------------------------------------------------*
STATIC FUNCTION Next( nOp )
*--------------------------------------------------------*
   LOCAL nLen := Len( aFiles )

   IF nLen > 1
      IF nOp < 0
         nItem--
         nItem := IF( nItem < 1, nLen, nItem )
      ELSEIF nOp > 0
         nItem++
         nItem := IF( nItem > nLen, 1, nItem )
      ENDIF

      FileOpen( aFiles[ nItem ][ 1 ], .T. )
   ELSE
      Form_Main.Text_1.VALUE := hb_ntos( nItem ) + "/" + hb_ntos( nLen )
   ENDIF

RETURN NIL

*--------------------------------------------------------*
STATIC FUNCTION FilePrint()
*--------------------------------------------------------*
   LOCAL nScale := 1 / 3.937, nX, nY, nH, nW

   IF ! Empty( cFile ) .AND. File( cFile )

      INIT PRINTSYS

      SELECT BY DIALOG

      IF HBPRNERROR != 0
         RETURN NIL
      ENDIF

      SET UNITS MM // Sets @... units to milimeters
      SET PAPERSIZE DMPAPER_A4 // Sets paper size to A4

      IF nHeight >= nWidth
         SET ORIENTATION PORTRAIT // Sets paper orientation to portrait
         nH := 250
         nW := 170
      ELSE
         SET ORIENTATION LANDSCAPE // Sets paper orientation to landscape
         nH := 170
         nW := 250
      ENDIF

      SET BIN DMBIN_FIRST // Use first bin
      SET QUALITY DMRES_HIGH // Sets print quality to high
      SET COLORMODE DMCOLOR_COLOR // Set print color mode to color
      SET PREVIEW ON // Enables print preview
      SET PREVIEW RECT 0, 0, nScrHeight, nScrWidth
      START DOC NAME Left( PROGRAM, 9 )

      START PAGE

      DO WHILE .T.

         nX := Round( nHeight * nScale, 0 )
         nY := Round( nWidth * nScale, 0 )

         IF ( nX <= nH .AND. nY <= nW ) .OR. nScale < 0.15
            EXIT
         ENDIF

         nScale -= 0.05

      ENDDO

      @ 15, 20 PICTURE cFile SIZE nX, nY

   END PAGE

   END DOC

   RELEASE PRINTSYS

   ENDIF

RETURN NIL

*--------------------------------------------------------*
STATIC FUNCTION GetToolBarHeight()
*--------------------------------------------------------*
   LOCAL h := Form_Main.ToolBar_1.Handle

RETURN ( LoWord( GetSizeToolBar( h ) ) + 2 * GetBorderHeight() )

*--------------------------------------------------------*
STATIC FUNCTION MsgAbout()
*--------------------------------------------------------*

RETURN MsgInfo( PadC( PROGRAM + VERSION, 38 ) + CRLF + ;
      "Copyright " + Chr( 169 ) + COPYRIGHT + CRLF + CRLF + ;
      hb_Compiler() + CRLF + Version() + CRLF + ;
      Left( MiniGuiVersion(), 38 ) + CRLF + CRLF + ;
      PadC( "This program is Freeware!", 38 ), "About", , .F. )

*--------------------------------------------------------*
FUNCTION JpgSize( cJPGfile )
*--------------------------------------------------------*

RETURN hb_GetImageSize( cJPGfile )
