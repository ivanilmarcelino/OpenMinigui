/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Revised on March 3, 2022 by Pierpaolo Martinello
 */

#include "minigui.ch"
#include "i_winuser.ch"

MEMVAR src4

FUNCTION MAIN

   PRIVATE src4 := { 'Ipsum', .F., '', .F., .F., 1 }

   DEFINE WINDOW Form_1 ;
         AT 0, 0 ;
         WIDTH 640 HEIGHT 380 ;
         TITLE 'Harbour MiniGUI Demo' ;
         ICON 'demo.ico' ;
         MAIN ;
         ON INIT LoadGeneric() ;
         FONT 'Arial' SIZE 10 ;
         NOMAXIMIZE ;
         NOMINIMIZE ;
         NOSIZE

      DEFINE STATUSBAR
         STATUSITEM ' HMG Power Ready! - Ctrl + F for search.'
         STATUSITEM ' HMG Power Ready!'
         STATUSITEM ' About ' ACTION MsgAbout()
      END STATUSBAR

      ON KEY ESCAPE ACTION ThisWindow.RELEASE
      ON KEY CONTROL + F ACTION ( src4[ 6 ] := 1, TxtSearch() )

      DEFINE EDITBOX Edit_1
         COL 10
         ROW 30
         WIDTH 600
         HEIGHT 140
         VALUE ''
         TOOLTIP 'EditBox'
         ON CHANGE ShowRowCol()
         NOHSCROLLBAR .T.
      END EDITBOX

      DEFINE LABEL Label_1
         COL 10
         ROW 200
         AUTOSIZE .T.
         HEIGHT 140
         VALUE 'You can use the mouse for select the searched text and press CTRL + F to search it again.'
      END LABEL

      DEFINE BUTTON B
         ROW 240
         COL 10
         CAPTION 'Set CaretPos'
         ACTION ( Form_1.Edit_1.CaretPos := Val( InputBox( 'Set Caret Position', '' ) ), Form_1.Edit_1.SetFocus )
      END BUTTON

      DEFINE BUTTON Search
         ROW 240
         COL 120
         CAPTION 'Search Text'
         ACTION ( src4[ 6 ] := 1, TxtSearch() )
      END BUTTON

      DEFINE BUTTON UP
         ROW 240
         COL 230
         WIDTH 30
         CAPTION '5'
         FONTNAME "Webdings"
         FONTSIZE 28
         FONTBOLD .T.
         ACTION ( src4[ 6 ] := 2, TxtSearch( src4 ) )
         Visible .F.
      END BUTTON

      DEFINE BUTTON DOWN
         ROW 240
         COL 270
         WIDTH 30
         CAPTION '6'
         FONTNAME "Webdings"
         FONTSIZE 28
         FONTBOLD .T.
         ACTION ( src4[ 6 ] := 1, TxtSearch( src4 ) )
         Visible .F.
      END BUTTON

      DEFINE TIMER Timer_1 INTERVAL 100 ACTION ShowRowCol()

   END WINDOW

   Form_1.Center()

   Form_1.Activate()

RETURN NIL
/*
*/
*-----------------------------------------------------------------------------*
PROCEDURE ShowRowCol
*-----------------------------------------------------------------------------*
   LOCAL s, c, i, e, q

   s := Form_1.Edit_1.VALUE
   c := Form_1.Edit_1.CaretPos
   e := 0
   q := 0

   FOR i := 1 TO c
      IF SubStr ( s, i, 1 ) == Chr( 13 )
         e++
         q := 0
      ELSE
         q++
      ENDIF
   NEXT i

   Form_1.StatusBar.Item( 2 ) := 'Row: ' + hb_ntos( e + 1 ) + ' Col: ' + hb_ntos( q )

RETURN
/*
*/
*-----------------------------------------------------------------------------*
PROCEDURE LoadGeneric()
*-----------------------------------------------------------------------------*
   LOCAL text := MemoRead( hb_DirBase() + "Generic.txt" )
   LOCAL ntextLen := Len ( text )
   Form_1.Edit_1.VALUE := text
   Form_1.Edit_1.caretpos := nTextLen
   Form_1.Edit_1.setfocus

RETURN
/*
*/
*-----------------------------------------------------------------------------*
PROCEDURE TxtSearch( argSrc )
*-----------------------------------------------------------------------------*
   LOCAL cEditStr, nLN, A, ndir, up, dn, nfind, pf, cReplAll, np
   LOCAL aLabels, aInitValues, aFormats, aResults
   LOCAL cText := AllTrim( Form_1.Edit_1.value )
   LOCAL hWndEdit := GetControlHandle( "Edit_1", "Form_1" )
   LOCAL pnt := Form_1.Edit_1.caretpos
   LOCAL aFindIt := {}
   LOCAL ws := AllTrim( GetSelect( hWndEdit ) )
   LOCAL cRepl := ''
   LOCAL ww := .F.

   IF ISNIL ( Argsrc )
      aLabels := { "Find:", ;
         "Whole words", ;
         "Replace with", ;
         "Replace all", ;
         "Case sensitive", ;
         "Direction" }

      aInitValues := AClone( src4 )

      IF ! Empty( ws )
         aInitValues[ 1 ] := Ws
      ENDIF

      aFormats := { 30, ;
         .F., ;
         30, ;
         .F., ;
         .F., ;
         { "Down", "Up" } }

      aResults := InputWindow( "Text Search", aLabels, aInitValues, aFormats,,, .T., { "Find", "Cancel" },, 200 )

      IF aResults[ 1 ] != NIL
         src4[ 1 ] := cEditStr := aResults[ 1 ]
         src4[ 2 ] := ww := aResults[ 2 ]
         src4[ 3 ] := cRepl := aResults[ 3 ]
         src4[ 4 ] := cReplall := aResults[ 4 ]
         src4[ 5 ] := aResults[ 5 ]
         src4[ 6 ] := ndir := aResults[ 6 ]
      ELSE
         cEditStr := ""
      ENDIF
   ELSE
      cEditStr := ArgSrc[ 1 ]
      ww := ArgSrc[ 2 ]
      cRepl := ArgSrc[ 3 ]
      cReplAll := ArgSrc[ 4 ]
      ndir := ArgSrc[ 6 ]
   ENDIF

   cEditStr := IF ( src4[ 5 ], cEditStr, Upper( cEditStr ) )
   nLN := Len( cEditStr )
   IF ww
      cEditStr := " " + AllTrim( cEditStr ) + " "
   ENDIF

   nfind := numat( cEditStr, IF( src4[ 5 ], cText, Upper( ctext ) ) )

   FOR A = 1 TO nfind
      AAdd( aFindIt, atnum( cEditStr, IF( src4[ 5 ], cText, Upper(ctext ) ), a, 0 ) )
   NEXT

   IF nFind < 1 .OR. nlN = 0
      Form_1.StatusBar.Item( 1 ) := ' HMG Power Ready! - Ctrl + F for search.'
      Form_1.UP.Hide
      Form_1.DOWN.Hide
      Form_1.Search.CAPTION := "Search Text"
      IF Len ( cEditStr ) > 0
         MsgStop( ' Unable to find "' + src4[ 1 ] + '" !', 'I am Sorry but...' )
      ENDIF
      Form_1.Edit_1.setfocus
      RETURN
   ENDIF

   // lower limit
   up := nFind
   WHILE pnt - nlN < afindit[ up ]
      Up--
      IF Up = 0
         Up := 1
         EXIT
      ENDIF
   END

   // Upper limit
   Dn := 1
   WHILE pnt >= afindit[ Dn ]
      Dn++
      IF dn > nfind
         Dn := nfind
         EXIT
      ENDIF
   END

   pf := IF ( ndir = 1, dn, up )

   IF nlN > 1
      Form_1.UP.show
      Form_1.DOWN.show
      Form_1.Search.CAPTION := "Search Text"
   ELSE
      Form_1.Search.CAPTION := "New Search"
   ENDIF

   // Up Search control
   IF pnt <= aFindIt[ 1 ] + nLn - 1 .AND. ndir = 2
      IF MsgyesNo( PadC( 'Unable to find "' + src4[ 1 ] + '"', 50 ) ;
            +if( nFind > 0, CRLF + CRLF + "But this word is present inside the text !" ;
            +CRLF + CRLF + " Do you want reverse search from End ?", " !" ) ;
            , 'I am Sorry but from here is ...' )
         Form_1.Edit_1.caretpos := Len( Form_1.Edit_1.value )
         src4[ 5 ] := .T.
         TxtSearch( src4 )
      ELSE
         G_Msg()
      ENDIF
      Form_1.Edit_1.setfocus
      RETURN
   ENDIF

   // Down Search control
   IF pnt >= ATail( aFindIt ) .AND. ndir = 1
      IF MsgyesNo( PadC( 'Unable to find "' + src4[ 1 ] + '"', 50 ) ;
            +if( nFind > 0, CRLF + CRLF + "But this word is present inside the text !" ;
            +CRLF + CRLF + " Do you want search from beginning ?", " !" ) ;
            , 'I am Sorry but from here is ...' )
         Form_1.Edit_1.caretpos := 0
         src4[ 5 ] := .F.
         TxtSearch( src4 )
      ELSE
         G_Msg()
      ENDIF
      Form_1.Edit_1.setfocus
      RETURN
   ENDIF

   Form_1.StatusBar.Item( 1 ) := '  "' + cEditStr + '" is Found ' + hb_ntos( pf ) + " of " + hb_ntos( nFind ) + ' Times'

   IF cReplall .AND. Len( afindit ) > 0
      ASort( aFindit,,, {| x, y | x > y } )
      FOR np = 1 TO nfind
         SendMessage( hWndEdit, EM_SETSEL, ( aFindIt[ np ] + if( ww, 0, -1 ) ), ( aFindIt[ np ] + nLN + if( ww, 0, -1 ) ) )
         SendMessageString( hWndEdit, EM_REPLACESEL, , cRepl )
      NEXT
      Form_1.StatusBar.Item( 1 ) := '  "' + src4[ 1 ] + "' was replaced " + hb_ntos( nFind ) + ' Times'
      src4 := { '', .F., '', .F., .F., 1 }
      g_msg()
   ELSE
      SendMessage( hWndEdit, EM_SETSEL, ( aFindIt[ pf ] + if( ww, 0, -1 ) ), ( aFindIt[ pf ] + nLN + if( ww, 0, -1 ) ) )

      IF Len( cRepl ) > 0
         switch MsgyesNoCancel( "Do you want replace next " + src4[ 1 ] + " with " + src4[ 3 ] + " ?" )
         CASE -1
            src4[ 3 ] := ''
            EXIT

         CASE 0
            EXIT

         CASE 1
            SendMessageString( hWndEdit, EM_REPLACESEL, , cRepl )
            SendMessage( hWndEdit, EM_SETSEL, ( aFindIt[ pf ] + if( ww, 0, -1 ) ), ( aFindIt[ pf ] + Len( crepl ) + if( ww, 0, -1 ) ) )

         EndSwitch
      ENDIF
   ENDIF

   Form_1.Edit_1.setfocus

   SendMessage( hWndEdit, EM_SCROLLCARET, 1, 0 )

RETURN
/*
*/
*-----------------------------------------------------------------------------*
PROCEDURE G_Msg () // only whole words
*-----------------------------------------------------------------------------*

   Form_1.Search.CAPTION := "New Search"
   Form_1.UP.hide
   Form_1.DOWN.hide
   Form_1.StatusBar.Item( 1 ) := ' HMG Power Ready! - Ctrl + F for search.'

RETURN
/*
*/
*-----------------------------------------------------------------------------*
FUNCTION MsgAbout
*-----------------------------------------------------------------------------*

RETURN MsgInfo( "EditBox with Text Search - Freeware" + CRLF + CRLF + ;
      "Revised in 2022 by Pierpaolo Martinello" + CRLF + CRLF + ;
      hb_Compiler() + CRLF + ;
      Version() + CRLF + ;
      SubStr( MiniGuiVersion(), 1, 38 ), "About" )
/*
*/
*-----------------------------------------------------------------------------*
STATIC FUNCTION GetSelect( argh )
*-----------------------------------------------------------------------------*
   LOCAL cText

   DEFAULT argh TO GetControlHandle( 'Edit_1', 'Form_1' )

   DoMethod( 'Form_1', 'Edit_1', "Setfocus" )

   ClearClipboard( Application.Handle )

   SendMessage( argh, WM_CUT, 0, 0 )

   cText := AllTrim( RetrieveTextFromClipboard() )

   SendMessage( argh, WM_PASTE, 0, 0 )

   ClearClipboard( Application.Handle )

RETURN cText
