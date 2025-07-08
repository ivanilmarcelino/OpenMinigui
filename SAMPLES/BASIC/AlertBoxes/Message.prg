/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * (c) 2019-2023 Grigory Filatov <gfilatov@gmail.com>
*/

#include "minigui.ch"
#include "i_winuser.ch"

FUNCTION Main()

   SET DEFAULT ICON TO "alert.ico"

   DEFINE WINDOW Form_1 ;
         AT 0, 0 ;
         WIDTH 640 HEIGHT 480 ;
         TITLE 'MiniGUI Alert Boxes Demo (Based Upon a Contribution Of Grigory Filatov)' ;
         ICON 'DEMO.ICO' ;
         MAIN ;
         FONT 'Arial' SIZE 10

      DEFINE STATUSBAR
         STATUSITEM '[x] Harbour Power Ready!'
      END STATUSBAR

      DEFINE MAIN MENU

         POPUP 'AlertBoxes'

            ITEM 'Message Information' ACTION AlertInfo( "MessageBox Test" )

            ITEM 'Message Stop' ACTION AlertStop( "MessageBox Test" )

            ITEM 'Message Error' ACTION AlertExclamation( "MessageBox Test" )

            ITEM 'Message Box with 2 Buttons: Yes and No' ACTION MsgInfo( AlertYesNo( "MessageBox Test", "AlertYesNo" ) )

            ITEM 'Message Box with 2 Buttons: Yes and No reverted' ACTION MsgInfo( AlertYesNo( "MessageBox Test", "AlertYesNo", .T. ) )

            ITEM 'Message Box with 2 Buttons: Ok and Cancel' ACTION MsgInfo( AlertOKCancel( "MessageBox Test", "AlertOKCancel" ) )

            ITEM 'Message Box with 2 Buttons: Retry and Cancel' ACTION MsgInfo( AlertRetryCancel( "MessageBox Test", "AlertRetryCancel" ) )

            ITEM 'Message Box with 3 Buttons: Yes, No and Cancel' ACTION MsgInfo( AlertYesNoCancel( "MessageBox Test", "AlertYesNoCancel" ) )

            ITEM 'Message Box with 3 Buttons: Abort, Retry and Ignore' ACTION MsgBoxDemo()

            ITEM 'OwnerDraw Message Box with 3 Buttons' ACTION OwnerDrawDemo()

            SEPARATOR

            ITEM '&Exit' ACTION Form_1.RELEASE

         END POPUP

         POPUP 'AlertRetryCancel'

            ITEM 'Message Box with 2 Buttons: Retry and Cancel and Exclamation Icon' ;
               ACTION AlertRetryCancel( "This have Exclamation Icon", "With icons...", , "alert.ico" )

            ITEM 'Message Box with 2 Buttons: Retry and Cancel and Focus on Cancel' ;
               ACTION AlertRetryCancel( "This have Focus on CANCEL button", "With focus...", 2 )

            ITEM 'Message Box with 2 Buttons: Retry and Cancel and TopMost Dialog' ;
               ACTION AlertRetryCancel( "This is a TOPMOST DIALOG", "TopMost...", , , , , .T. )

         END POPUP

         POPUP '&Help'

            ITEM '&About' ACTION AlertInfo ( "MiniGUI Alert Boxes demo", , "demo.ico" )

         END POPUP

      END MENU

   END WINDOW

   CENTER WINDOW Form_1

   ACTIVATE WINDOW Form_1

RETURN NIL


FUNCTION MsgBoxDemo()

   LOCAL nRet

   _HMG_ModalDialogReturn := 0

   WHILE ( nRet := HMG_Alert( "Please choose IGNORE option", { "Abort", "Retry", "Ignore" }, "Please, choose..." ) ) == 2
      _HMG_ModalDialogReturn := 0
   ENDDO

   IF nRet == 1

      Tone( 600, 2 )

      Form_1.RELEASE

   ENDIF

RETURN NIL


FUNCTION OwnerDrawDemo()

   LOCAL aBack

   DEFINE FONT DlgFont FONTNAME "DejaVu Sans Mono" SIZE 12

   // Set Message Alert colors and store previous colors in aBack array
   SET MSGALERT FONTCOLOR TO BLACK STOREIN aBack
   SET MSGALERT BACKCOLOR TO { 248, 209, 211 }

   _HMG_ModalDialogReturn := 0

   WHILE HMG_Alert( "Please choose IGNORE option", { "Abort", "Retry", "Ignore" }, "Please, choose...", , ;
         "alert.ico", /*nIcoSize*/, /*aBtnColor*/, {|| my_bInitAlert( 2 ) } ) == 2
      _HMG_ModalDialogReturn := 0
   ENDDO

   IF _HMG_ModalDialogReturn == 1

      Tone( 600, 2 )

      Form_1.RELEASE

   ENDIF

   // Restore Message Alert colors from aBack array
   SET MSGALERT BACKCOLOR TO aBack[ 1 ]
   SET MSGALERT FONTCOLOR TO aBack[ 2 ]

   RELEASE FONT DlgFont

RETURN NIL


FUNCTION Alert_BtnEx( xBtn, aBtnColor, aGrFill, aFontColor )

   LOCAL y, x, w, h, n, c, b, f, cBtn, nBtn, lRet := .F.
   LOCAL cWnd := ThisWindow.NAME

   DEFAULT aFontColor := nRGB2Arr( GetSysColor( COLOR_BTNTEXT ) )

   IF HB_ISNUMERIC( xBtn ) ; nBtn := xBtn ; cBtn := "Btn_" + StrZero( nBtn, 2 )
   ELSEIF HB_ISCHAR( xBtn ) ; nBtn := Val( Right( xBtn, 2 ) ) ; cBtn := xBtn
   ENDIF

   IF ! _IsControlDefined( cBtn, cWnd ) ; RETURN lRet
   ENDIF

   IF GetControlType( cBtn, cWnd ) == "BUTTON"
      y := This.( cBtn ).Row
      x := This.( cBtn ).Col
      w := This.( cBtn ).Width
      h := This.( cBtn ).Height
      n := This.( cBtn ).Cargo ; DEFAULT n := nBtn
      c := This.( cBtn ).Caption
      f := This.( cBtn ).Fontname
      b := This.( cBtn ).Event.Action

      _ReleaseControl( cBtn, cWnd ) ; DO EVENTS

      @ y, x BUTTONEX ( cBtn ) OF ( cWnd ) CAPTION c WIDTH w HEIGHT h ;
         FONT f FONTCOLOR aFontColor BACKCOLOR aBtnColor ;
         GRADIENTFILL aGrFill ;
         NOXPSTYLE HANDCURSOR

      This.( cBtn ).OnGotFocus := {|| SetProperty( cWnd, cBtn, "BackColor", aBtnColor ) }
      This.( cBtn ).Cargo := n
      This.( cBtn ).Event.Action := b
      lRet := .T.

   ENDIF

RETURN lRet


FUNCTION my_bInitAlert( nPos, aFontColor )

   LOCAL cBtn, aGrOver, aGrFill, nI
   LOCAL aBtn := HMG_GetFormControls( ThisWindow.NAME, "BUTTON" )
   LOCAL aGrO := Array( 5 )
   LOCAL aGrF := Array( 5 )
   LOCAL nDef := 1
   LOCAL aBtnGrd := { HMG_RGB2n( GRAY ), CLR_WHITE }

   DEFAULT nPos := 0

   AFill( aGrO, NIL ) ; AFill( aGrF, NIL )

   // Blue color gradient at nPos parameter = 1
   aGrO[ 1 ] := { { 0.5, CLR_WHITE, CLR_HBLUE }, { 0.5, CLR_HBLUE, CLR_WHITE } }
   aGrF[ 1 ] := { { 0.5, CLR_HBLUE, CLR_WHITE }, { 0.5, CLR_WHITE, CLR_HBLUE } }

   // Gray color gradient at nPos parameter = 2
   aGrO[ 2 ] := { { 0.5, aBtnGrd[ 2 ], aBtnGrd[ 1 ] }, { 0.5, aBtnGrd[ 1 ], aBtnGrd[ 2 ] } }
   aGrF[ 2 ] := { { 0.5, aBtnGrd[ 1 ], aBtnGrd[ 2 ] }, { 0.5, aBtnGrd[ 2 ], aBtnGrd[ 1 ] } }

   // Red color gradient at nPos parameter = 3
   aGrO[ 3 ] := { { 0.5, CLR_RED, CLR_HRED }, { 0.5, CLR_HRED, CLR_RED } }
   aGrF[ 3 ] := { { 0.5, CLR_HRED, CLR_WHITE }, { 0.5, CLR_WHITE, CLR_HRED } }

   ASize( aBtn, Len( aGrO ) )

   FOR nI := 1 TO Len( aBtn )
      cBtn := aBtn[ nI ]
      IF Empty( cBtn ) ; LOOP
      ENDIF
      IF nPos > 0 .AND. nPos <= Len( aBtn )
         aGrOver := aGrO[ nPos ]
         aGrFill := aGrF[ nPos ]
      ELSE  // nPos == 0
         aGrOver := aGrO[ nI ]
         aGrFill := aGrF[ nI ]
      ENDIF
      DEFAULT aGrOver := aGrO[ nDef ]
      DEFAULT aGrFill := aGrF[ nDef ]
      Alert_BtnEx( cBtn, aGrOver, aGrFill, aFontColor )
   NEXT

RETURN NIL
