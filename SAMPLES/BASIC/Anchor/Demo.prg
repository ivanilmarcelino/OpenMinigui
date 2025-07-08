/*******************************************************************************
    Filename        : Demo.prg
    URL             : \MiniGui\SAMPLES\BASIC\Anchor\Demo.prg

    Created         : 08 December 2021 (18:26:25)
    Created by      : Pierpaolo Martinello

    Last Updated    : 11 December 2021 (18:52:57)
    Updated by      : Pierpaolo
*******************************************************************************/
/*
*/
#include "minigui.ch"

#define MONITOR_HANDLE   1
#define MONITOR_RECT     2

FUNCTION Main()

   LOAD WINDOW oWindow1
   oWindow1.Center
   ACTIVATE WINDOW oWindow1

RETURN NIL
/*
*/
*-----------------------------------------------------------------------------*
PROCEDURE SEE_OPTIONS ( arg1 )
*-----------------------------------------------------------------------------*
   LOCAL n, cItem
   IF arg1
      DRAW LINE IN WINDOW oWindow1 AT 80, 70 TO 80, 520 PENWIDTH 5 // Top horizontal
      DRAW LINE IN WINDOW oWindow1 AT 80, 70 TO 470, 70 PENWIDTH 5 // Left vertical
      DRAW LINE IN WINDOW oWindow1 AT 80, 520 TO 470, 520 PENWIDTH 5 // Right Vertical
      DRAW LINE IN WINDOW oWindow1 AT 470, 70 TO 470, 520 PENWIDTH 5 // Bottom horizontal
   ELSE
      erasewindow( "oWindow1" )
      oWindow1.statusbar.item( 1 ) := ""
      oWindow1.redraw
   ENDIF

   FOR n = 0 TO 21
      cItem := 'Label_' + hb_ntos( n )
      oWindow1.&( citem ).visible := arg1
   NEXT

RETURN
/*
*/
*-----------------------------------------------------------------------------*
FUNCTION scegli( opt, TITLE, note, def, PosRel )
*-----------------------------------------------------------------------------*
   LOCAL r := 0, WName := thiswindow.NAME
   LOCAL ANCHOR := { "UL", "EUC", "UR", "EUR", "ECR", "EDR", "DR", "EDC", "DL", "EDL", "ECL" ;
      , "EUL", "ICUL", "IUC", "ICUR", "ICR", "ICDR", "IDC", "ICDL", "ICL", "FC" }

   DEFAULT TITLE TO "Print selection.", opt to { "This tab", "All" }
   DEFAULT PosRel TO "", note TO "", def TO 0
   note := Space( 10 ) + note

   IF Empty( posrel )

      DEFINE WINDOW COMBOCHOICE ;
            AT 0, 0 ;
            WIDTH 400 ;
            HEIGHT 150 ;
            TITLE 'Demo anchor' ;
            MODAL

         @ 40, 10 COMBOBOX Combo_1 ;
            WIDTH 210 ;
            ITEMS { "Upper Left", "Upper Centered", "Upper Right", "External Upper Right", "External Centered Right", "External Down Right", "Down Right" ;
            , "External Down Centered", "Down Left", "External Down Left", "External Centered Left", "External Upper Left", "Internal Corner Upper Left" ;
            , "Internal Upper Centered", "Internal Corner Upper Right", "Internal Centered Right", "Internal Corner Down Rigth", "Internal Down Centered" ;
            , "Internal Corner Down Left", "Internal Centered Left", "Form Centered" } ;
            VALUE 1 ;
            ITEMHEIGHT 21

         DEFINE BUTTON Control_1
            ROW 30
            COL 240
            WIDTH 130
            HEIGHT 50
            CAPTION 'Conferma'
            ACTION ( POSREL := ANCHOR[ COMBOCHOICE.COMBO_1.VALUE ], THISWINDOW.RELEASE )
         END BUTTON

      END WINDOW
      CENTER WINDOW COMBOCHOICE
      ACTIVATE WINDOW COMBOCHOICE

      IF Empty( posrel )
         msgInfo( "The next window will be Monitor 1 centered." )
      ENDIF

   ENDIF
   LOAD WINDOW Scegli
   Scegli.RadioGroup_1.Setfocus
   ACTIVATE WINDOW Scegli

RETURN r
/*
*/
*-----------------------------------------------------------------------------*
PROCEDURE load_Scegli_base( TITLE, def, note, opt )
*-----------------------------------------------------------------------------*
   LOCAL nWidthCli := Max( 286, GetTextWidth( Getdc ( this.handle ), note, GetFontHandle( GetDefaultFontname() ) ) )
   LOCAL s_hg := Len ( opt ) * 25 + 125

   ON KEY RETURN OF SCEGLI ACTION ( SCEGLI.BUTTON_1.SETFOCUS, _PUSHKEY( VK_SPACE ) )

   ON KEY ESCAPE OF SCEGLI ACTION SCEGLI.RELEASE

   Scegli.TITLE := TITLE

   Scegli.HEIGHT := s_hg
   Scegli.WIDTH := nWidthCli + iif( _HMG_IsXPorLater, 2, 1 ) * GetBorderWidth()
   Scegli.Button_1.ROW := s_hg - 100
   Scegli.Button_2.ROW := s_hg - 100
   Scegli.Button_2.COL := Scegli.WIDTH - 125

   _setitem( "statusbar", "Scegli", 1, note )

   Scegli.Radiogroup_1.RELEASE

   DEFINE RADIOGROUP "RadioGroup_1"
      Parent Scegli
      COL 21
      HEIGHT 59
      OPTIONS opt
      ROW 11
      SPACING 25
      TRANSPARENT TRUE
      WIDTH 230
   END RADIOGROUP

   Scegli.RadioGroup_1.VALUE := def

RETURN
/*
*/
*-----------------------------------------------------------------------------*
PROCEDURE PosRel ( wName, prel, parent, offset )
*-----------------------------------------------------------------------------*
   LOCAL oCol, orow, wh, ww, ph, pW, mbh, sbh, frow, bw, m, rr, bh
   LOCAL DskTW := 0, DskTH, nDesk
   LOCAL aCli := GetNonClient()
   LOCAL aMonitors := EnumDisplayMonitors()

   // aCli GetNonClient return Values
   // 1 - BorderWidth, 2 - ScrollWidth, 3 - ScrollHeight, 4 - bCaptionWidth, 5 - CaptionHeight, 6 - MenuWidth, 7 - MenuHeight

   DEFAULT wName TO ThisWindow.NAME, parent TO "SCEGLI", offset TO 0
   prel := Upper( prel )

   FOR EACH m IN aMonitors
      DskTW := Max ( DskTW, m[ 2, "right" ] )
   NEXT

   oCol := GetProperty ( wName, "COL" )
   orow := GetProperty ( wName, "ROW" )
   sbh := GetStatusBarHeight( wName )
   wh := GetProperty ( wName, "HEIGHT" ) - iif ( sbh = 0, 23, 0 ) // statusbar = 23 pixel
   wW := GetProperty ( wName, "WIDTH" )
   ph := GetProperty ( Parent, "HEIGHT" )
   pW := GetProperty ( Parent, "WIDTH" )
   mbh := GetSystemMetrics( SM_CYMENU )
   bW := GetborderWidth ()
   bh := GetborderHeight ()
   rr := MonitorFromPoint( Abs( ocol ), Abs( orow ) ) // use abs to prevent crashes with maximized window

   // I identify the current monitor
   AEval( aMonitors, {| x, y | if( x[ 1 ] == rr, ( ndesk := y, m := x[ 2 ] ), '' ) } )

   // identify the Heigth of current monitor
   DskTH := m[ "bottom" ]

   DO CASE

   CASE Prel = "EUR" // ESTERNAL UPPER RIGHT *
      IF ( ocol + wW + pw ) > DskTw
         offset -= ( oCol + wW + acli[ 1 ] + bW - acli[ 2 ] - dsktw )
      ELSE
         offset += pW - acli[ 1 ]
      ENDIF
      Prel := "ICUR"

   CASE Prel = "ECR" // ESTERNAL CENTER RIGHT  *
      offset += ( wW / 2 ) + ( PW / 2 ) - 2 * acli[ 1 ]
      Prel := "FC"

   CASE Prel = "EDR" // ESTERNAL DOWN RIGHT  *
      offset += pW - acli[ 1 ]
      Prel := "ICDR"

   CASE Prel = "EUL" // ESTERNAL UPPER LEFT
      IF ocol < pw
         offset := -ocol - acli[ 1 ]
      ELSE
         offset -= pW - acli[ 1 ]
      ENDIF
      Prel := "ICUL"

   CASE Prel = "ECL" // ESTERNAL CENTER LEFT
      IF ocol < pw
         offset := -ocol - acli[ 1 ]
      ELSE
         offset -= pW - acli[ 1 ]
      ENDIF

   CASE Prel = "EDL" // ESTERNAL DOWN LEFT
      IF ocol < pw
         offset := -ocol - acli[ 1 ]
      ELSE
         offset -= pW - acli[ 1 ]
      ENDIF
      Prel := "ICDL"

   ENDCASE

   DO CASE

   CASE Prel = "UL" // UPPER lEFT          *
      frow := ( orow - ph + bh - acli[ 1 ] )
      IF frow + offset < 0
         Frow := offset := 0
      ENDIF
      SetProperty ( Parent, "ROW", Frow + offset )
      SetProperty ( Parent, "COL", oCol + acli[ 1 ] )

   CASE Prel = "IUC" // INTERNAL UPPER CENTER        *
      IF Wh >= DskTH - acli[ 7 ]
         SetProperty ( Parent, "ROW", orow + acli[ 1 ] )
      ELSE
         SetProperty ( Parent, "ROW", orow )
      ENDIF
      SetProperty ( Parent, "COL", oCol + ( wW / 2 ) - ( PW / 2 ) )

   CASE Prel = "UR" // UPPER RIGHT         *
      frow := ( orow - ph + bh - acli[ 1 ] )
      IF frow + offset < 0
         Frow := offset := 0
      ENDIF
      SetProperty ( Parent, "ROW", Frow + offset )
      SetProperty ( Parent, "COL", oCol + wW + acli[ 1 ] + bW - pW - acli[ 2 ] + offset )

   CASE Prel = "EUC" // EXTERNAL UPPER RIGHT         *
      frow := ( orow - ph + bh - acli[ 1 ] )
      IF frow + offset < 0
         Frow := offset := 0
      ENDIF
      SetProperty ( Parent, "ROW", Frow + offset )
      SetProperty ( Parent, "COL", oCol + ( wW / 2 ) - ( PW / 2 ) )

   CASE Prel = "DL" // DOWN lEFT           *
      frow := ( orow + wh + acli[ 7 ] + 2 * bH ) - ( mbh + sbh )
      IF frow + offset + ph > DskTH
         offset := 0
         Frow := DskTH - ph - sbh
      ENDIF
      SetProperty ( Parent, "ROW", Frow + offset )
      SetProperty ( Parent, "COL", oCol + acli[ 1 ] )

   CASE Prel = "IDC" // DOWN CENTER         *
      frow := ( orow + wh + 2 * acli[ 7 ] ) - ( ph + mbh + sbh )
      SetProperty ( Parent, "ROW", Frow )
      SetProperty ( Parent, "COL", oCol + ( wW / 2 ) - ( PW / 2 ) )

   CASE Prel = "DR" // DOWN RIGHT          *
      frow := ( orow + wh + acli[ 7 ] + 2 * bH ) - ( mbh + sbh )
      IF frow + offset + ph > DskTH
         offset := 0
         Frow := DskTH - ph - sbh
      ENDIF
      SetProperty ( Parent, "ROW", Frow + offset )
      SetProperty ( Parent, "COL", oCol + wW + acli[ 1 ] + bW - pW - acli[ 2 ] )

   CASE Prel = "ICUL" // INTERNAL CORNER UPPER LEFT  *
      IF Wh >= DskTH - acli[ 7 ]
         SetProperty ( Parent, "ROW", orow + acli[ 1 ] )
      ELSE
         SetProperty ( Parent, "ROW", orow )
      ENDIF
      SetProperty ( Parent, "COL", oCol + acli[ 1 ] + offset )

   CASE Prel = "ICUR" // INTERNAL CORNER UPPER RIGHT *
      IF Wh >= DskTH - acli[ 7 ]
         SetProperty ( Parent, "ROW", orow + acli[ 1 ] )
      ELSE
         SetProperty ( Parent, "ROW", orow )
      ENDIF
      SetProperty ( Parent, "COL", oCol + wW + acli[ 1 ] + bW - pW - acli[ 2 ] + offset )

   CASE Prel = "ICDL" // INTERNAL CORNER DOWN  LEFT  *
      frow := ( orow + wh + 2 * acli[ 7 ] ) - ( ph + mbh + sbh )
      SetProperty ( Parent, "ROW", Frow )
      SetProperty ( Parent, "COL", oCol + acli[ 1 ] + offset )

   CASE Prel = "EDC" // EXTERNAL DOWN CENTER
      frow := ( orow + wh + acli[ 7 ] + 2 * bH ) - ( mbh + sbh )
      IF frow + offset + ph > DskTH
         offset := 0
         Frow := DskTH - ph - sbh
      ENDIF
      SetProperty ( Parent, "ROW", Frow + offset )
      SetProperty ( Parent, "COL", oCol + ( wW / 2 ) - ( PW / 2 ) )

   CASE Prel = "ECL" // EXTERNAL CENTER LEFT
      frow := ( orow + ( wh / 2 ) - ( ph / 2 ) )
      SetProperty ( Parent, "ROW", frow )
      SetProperty ( Parent, "COL", oCol + acli[ 1 ] + offset )

   CASE Prel = "ICDR" // INTERNAL CORNER DOWN  RIGHT *
      frow := ( orow + wh + 2 * acli[ 7 ] ) - ( ph + mbh + sbh )
      SetProperty ( Parent, "ROW", Frow )
      SetProperty ( Parent, "COL", oCol + wW + acli[ 1 ] + bW - pW - acli[ 2 ] + offset )

   CASE Prel = "ICR" // INTERNAL CENTER LEFT
      frow := ( orow + ( wh / 2 ) - ( ph / 2 ) )
      SetProperty ( Parent, "ROW", frow )
      SetProperty ( Parent, "COL", oCol + wW + acli[ 1 ] + bW - pW - acli[ 2 ] + offset )

   CASE Prel = "ICL" // INTERNAL CENTER RIGHT
      frow := ( orow + ( wh / 2 ) - ( ph / 2 ) )
      SetProperty ( Parent, "ROW", frow )
      SetProperty ( Parent, "COL", oCol + acli[ 1 ] )

   CASE Prel = "FC" // WINDOW CENTERED    *
      frow := ( orow + ( wh / 2 ) - ( ph / 2 ) )
      SetProperty ( Parent, "ROW", frow )
      SetProperty ( Parent, "COL", oCol + ( wW / 2 ) - ( PW / 2 ) + offset )

   OTHERWISE
      DoMethod ( Parent, "center" ) // Defautl Monitor 1 Centered
   ENDCASE

RETURN
/*
*/
*-----------------------------------------------------------------------------*
FUNCTION GetStatusBarHeight( cForm ) // Height StatusBar
*-----------------------------------------------------------------------------*
   LOCAL rtv := 0
   DEFAULT cForm := ThisWindow.NAME
   IF GetControlIndex ( 'STATUSBAR', cForm ) > 0
      Rtv += GetWindowHeight( GetControlHandle( 'STATUSBAR', cForm ) )
   ENDIF

RETURN Rtv
/*
*/
*-----------------------------------------------------------------------------*
PROCEDURE anchor( Arg1, msg )
*-----------------------------------------------------------------------------*
   LOCAL n, cItem, rtv := 0
   LOCAL Anchor := { { "UL", "Upper Left" } ;
      , { "EUC", "External Upper Centered" } ;
      , { "UR", "Upper Right" } ;
      , { "EUL", "External Upper Left" } ;
      , { "ICUL", "Internal Corner Upper Left" } ;
      , { "IUC", "Internal Upper Center" } ;
      , { "EUR", "External Upper Right" } ;
      , { "ECL", "External Center Left" } ;
      , { "EDL", "External Down Left" } ;
      , { "DL", "Down Left" } ;
      , { "EDC", "External Center Down" } ;
      , { "DR", "Down Right" } ;
      , { "EDR", "External Upper Left" } ;
      , { "ECR", "External Upper Left" } ;
      , { "ICUR", "Internal Corner Upper Right" } ;
      , { "ICDR", "Internal Corner Down Right" } ;
      , { "IDC", "Internal Corner Down" } ;
      , { "FC", "Form Centered" } ;
      , { "ICR", "Internal Center Right" } ;
      , { "ICL", "Internal Center Left" } ;
      , { "ICDL", "Internal Corner Down Left" } }

   DEFAULT Msg TO .F.

   FOR n = 1 TO 21
      cItem := 'Label_' + hb_ntos( n )
      IF cItem == arg1
         rtv := n
         IF Msg
            oWindow1.statusbar.item( 1 ) := Space ( 20 ) + "For " + anchor[ n, 2 ] + " -> Use the parameter: " + anchor[ n, 1 ]
            RETURN
         ENDIF
      ENDIF
   NEXT

   Scegli (,,,, iif ( rtv > 0, ANCHOR[ rtv, 1 ], "" ) )

RETURN
