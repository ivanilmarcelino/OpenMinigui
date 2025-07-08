#include "hmg.ch"
*----------------------------------------------*
* © Gripau - Soft » Mustafa López «            *
*----------------------------------------------*
MEMVAR cFolder
MEMVAR oPlayer, p_GetFile, xeck0, xeck1
*----------------------------------------------*
FIELD nome IN arq
*----------------------------------------------*
REQUEST DBFCDX
*----------------------------------------------*
FUNCTION MAIN
*----------------------------------------------*
   rddSetDefault( "DBFCDX" )
   *----------------------------------------------*
   SET CENTURY ON
   SET DATE FRENCH
   SET DELETE ON
   *----------------------------------------------*
   SET NAVIGATION EXTENDED
   SET CODEPAGE TO UNICODE
   SET BROWSESYNC ON
   SET TOOLTIPSTYLE BALLOON
   *----------------------------------------------*
   PUBLIC cFolder := GetStartUpFolder()
   *----------------------------------------------*
   PUBLIC oPlayer, p_GetFile, xeck0, xeck1
   *----------------------------------------------*

   IF ! File( "arq.dbf" )
      NoEstaDBF()
   ENDIF

   IF ! File( 'xeckyn.mem' )
      xeck0 := .T.
      xeck1 := .F.
      SAVE TO xeckyn.mem ALL LIKE xeck*
   ELSE
      RESTORE FROM xeckyn.mem ADDITIVE
   ENDIF

   DEFINE WINDOW Form_1 ;
         AT 0, 0 ;
         WIDTH 995 ;
         HEIGHT 650 ;
         TITLE Space( 3 ) + 'File Selection m3u - 2021' ;
         ICON 'Logo.ico' ;
         MAIN NOSIZE NOMAXIMIZE ;
         ON INIT ( oPlayer := Form_1.Test.XObject, Play_Movie_1() )

      ON KEY ESCAPE ACTION Form_1.RELEASE

      DEFINE ACTIVEX Test
         ROW 030
         COL 028
         WIDTH 800
         HEIGHT 550
         PROGID "WMPlayer.OCX.7"
      END ACTIVEX

      DEFINE BUTTON Button_01
         ROW 030
         COL 846
         WIDTH 123
         HEIGHT 032
         CAPTION "Open File"
         ACTION f_importar()
      END BUTTON

      DEFINE BUTTON Button_02
         ROW 064
         COL 846
         WIDTH 123
         HEIGHT 032
         CAPTION "Play"
         ACTION ( oPlayer:controls:play() )
      END BUTTON

      DEFINE BUTTON Button_03
         ROW 098
         COL 846
         WIDTH 123
         HEIGHT 032
         CAPTION "Pause"
         ACTION ( oPlayer:controls:pause() )
      END BUTTON

      DEFINE BUTTON Button_04
         ROW 132
         COL 846
         WIDTH 123
         HEIGHT 032
         CAPTION "Stop"
         ACTION ( oPlayer:controls:Stop() )
      END BUTTON

      DEFINE BUTTON Button_05
         ROW 166
         COL 846
         WIDTH 123
         HEIGHT 032
         CAPTION "Without Controls"
         ACTION ( oPlayer:uiMode := "none" )
      END BUTTON

      DEFINE BUTTON Button_06
         ROW 200
         COL 846
         WIDTH 123
         HEIGHT 032
         CAPTION "With Controls"
         ACTION ( oPlayer:uiMode := "full" )
      END BUTTON

      DEFINE BUTTON Button_07
         ROW 234
         COL 846
         WIDTH 123
         HEIGHT 032
         CAPTION "Previous"
         ACTION ( oPlayer:controls:Previous() )
      END BUTTON

      DEFINE BUTTON Button_08
         ROW 268
         COL 846
         WIDTH 123
         HEIGHT 032
         CAPTION "Next"
         ACTION ( oPlayer:controls:Next() )
      END BUTTON

      DEFINE BUTTON Button_09
         ROW 302
         COL 846
         WIDTH 123
         HEIGHT 032
         CAPTION "FullScreen"
         ACTION ( iif( oPlayer:PlayState == 3, oPlayer:Fullscreen := .T., NIL ) )
      END BUTTON

      DEFINE BUTTON Button_10
         ROW 336
         COL 846
         WIDTH 039
         HEIGHT 035
         CAPTION "V-0"
         ACTION ( Form_1.Slider_1.VALUE := 0 )
      END BUTTON

      DEFINE BUTTON Button_11
         ROW 336
         COL 888
         WIDTH 039
         HEIGHT 035
         CAPTION "V-50"
         ACTION ( Form_1.Slider_1.VALUE := 5 )
      END BUTTON

      DEFINE BUTTON Button_12
         ROW 336
         COL 931
         WIDTH 039
         HEIGHT 035
         CAPTION "V-100"
         ACTION ( Form_1.Slider_1.VALUE := 10 )
      END BUTTON

      @ 379, 949 TEXTBOX LabelSli VALUE " 5" WIDTH 22 HEIGHT 22 MAXLENGTH 5 READONLY

      @ 379, 840 SLIDER Slider_1 ;
         RANGE 0, 10 ;
         WIDTH 100 ;
         HEIGHT 35 ;
         VALUE 5 ;
         ON CHANGE {|| Slider1_Change() }

      DEFINE CHECKBOX checkbox_0
         ROW 420
         COL 846
         WIDTH 120
         HEIGHT 20
         VALUE xeck0
         CAPTION Space( 5 ) + "AutoStart"
      END CHECKBOX

      DEFINE CHECKBOX checkbox_1
         ROW 440
         COL 846
         WIDTH 120
         HEIGHT 20
         VALUE xeck1
         CAPTION Space( 5 ) + "Mute"
      END CHECKBOX

      DEFINE BUTTON Button_13
         ROW 462
         COL 846
         WIDTH 123
         HEIGHT 32
         CAPTION "Save Config"
         ACTION ReverseCheck()
      END BUTTON

      DRAW ROUNDRECTANGLE IN WINDOW Form_1 ;
         AT 417, 840 TO 500, 975 ;
         ROUNDWIDTH 5 ;
         ROUNDHEIGHT 5 ;
         PENCOLOR BLACK

      DEFINE BUTTON Button_14
         ROW 508
         COL 846
         WIDTH 123
         HEIGHT 32
         CAPTION "Zap Base"
         ACTION Deletery()
      END BUTTON

      DEFINE BUTTON Button_15
         ROW 548
         COL 846
         WIDTH 123
         HEIGHT 32
         CAPTION "Exit"
         ACTION Form_1.RELEASE
      END BUTTON


   END WINDOW

   CENTER WINDOW Form_1
   ACTIVATE WINDOW Form_1

RETURN NIL

*----------------------------------------------*
FUNCTION Slider1_Change()
*----------------------------------------------*

   LOCAL nValue := Form_1.Slider_1.VALUE

   oPlayer:Settings:volume := nValue * 10

   Form_1.LabelSli.VALUE := Str( nValue, 2 )

RETURN NIL

*----------------------------------------------*
PROCEDURE Deletery()
*----------------------------------------------*

   DirChange( cFolder )

   dbCloseAll()

   IF MsgYesNo( "Are you sure ?", "Delete DataBase !!!" ) == .T.

      USE ARQ NEW
      ZAP

      oPlayer:controls:Stop()
      FileDelete( "Container.m3u" )

   ENDIF

RETURN

*----------------------------------------------*
PROCEDURE ReverseCheck()
*----------------------------------------------*
   LOCAL bChec00, bChec01

   bChec00 := iif( Form_1.checkbox_0.VALUE, "True", "False" )

   IF bChec00 = "True"
      xeck0 := .T.
   ELSE
      xeck0 := .F.
   ENDIF

   bChec01 := iif( Form_1.checkbox_1.VALUE, "True", "False" )

   IF bChec01 = "True"
      xeck1 := .T.
   ELSE
      xeck1 := .F.
   ENDIF

   SAVE TO xeckyn.mem ALL LIKE xeck*

   RESTORE FROM xeckyn.mem ADDITIVE

   Play_Movie_1()

RETURN

*----------------------------------------------*
PROCEDURE f_importar()
*----------------------------------------------*
   LOCAL varios, arq_cas, File_cas, i, n_for

   CLOSE DATABASES

   varios := .T. // selecionar varios arquivos

   p_GetFile := iif( Empty( p_GetFile ), GetMyDocumentsFolder(), p_GetFile )

   arq_cas := Getfile ( { ;
      { 'Files *.mp3', '*.mp3' }, ;
      { 'Files *.avi', '*.avi' }, ;
      { 'Files *.mp4', '*.mp4' }, ;
      { 'Files *.jpg', '*.jpg' }, ;
      { 'Files *.png', '*.png' }, ;
      { 'Files *.bmp', '*.bmp' }, ;
      { 'Files *.gif', '*.gif' }, ;
      { 'All Files', '*.*' } }, ;
      'Open File(s)', p_GetFile, varios, .T. )

   IF Len( arq_cas ) == 0
      RETURN
   ENDIF

   USE ARQ NEW

   FOR n_for := 1 TO Len( arq_cas )
      i := n_for + 1

      IF n_for == Len( arq_cas ) // esta consistencia foi feita pq o ultimo arquivo
         i := 1                  // é sempre o primeiro
      ENDIF

      File_cas := arq_cas[ i ]

      APPEND BLANK

      REPL NOME WITH File_cas

   NEXT

   // -----------------------------------  Create "Container.m3u"  -----------------------------------

   SET PRINTER ON
   SET CONSOLE OFF
   SET PRINTER TO "Container.m3u"

   @ PRow(), PCol()

   ??'#EXTM3U By © Gripau - Soft' + Space( 1 ) + hb_UChar( 187 ) + ' Mustafa López' + Space( 1 ) + hb_UChar( 171 ) // <=  Copyright
   ? '#EXTINF:-1, Selected Files' + Space( 1 ) + hb_UChar( 187 ) + Space( 1 ) + DToC( Date() ) + Space( 1 ) + Time()

   GO TOP
   DO WHILE .NOT. Eof()
      ? AllTrim( NOME )
      SKIP
   ENDDO
   ?
   // ------------------------------------------------------------------------------------------------

   SET CONSOLE ON
   SET PRINTER OFF

   CLOSE DATABASES
   Play_Movie_1()

RETURN

*----------------------------------------------*
FUNCTION Play_Movie_1()
*----------------------------------------------*

   dbCloseAll()

   oPlayer:Settings:mute := xeck1
   oPlayer:Settings:autoStart := xeck0

   IF ! File( "Container.m3u" )
      // MsgStop("File Missing ->'Container.m3u","Attention !!!")
      f_importar()
   ELSE
      Form_1.Test.VALUE := .T.
      oPlayer:url := "Container.m3u"
   ENDIF

RETURN NIL

*----------------------------------------------*
FUNCTION NoEstaDBF()
*----------------------------------------------*

   LOCAL aStru1 := { { "nome", "C", 250, 0 } }

   dbCreate( "arq.dbf", aStru1, "DBFCDX", .T. )

   CLOSE DATABASES

RETURN NIL

*----------------------------------------------*
* EOF
*----------------------------------------------*
