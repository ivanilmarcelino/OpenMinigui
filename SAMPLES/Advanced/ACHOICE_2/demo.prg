/*
   Here is my implementation of ACHOICE in HMG using grid control.
   The only problem is that it nearly look likes the ACHOICE of the old days.

   Caveats:
   1. Grid control height and width must be larger than the height of the window, frmAchoice in this regard so that the scroll bars is not visible.
   2. IF you want the horizontal and/or vertical scroll bar, then adjust the dimension calculation.

   Dhanny del Pilar <dhaine_adp@hotmail.com>

   24/August/2024 To maintain compatibility of use I made the following changes:
   1) Enhanced with the ability to manage multiple columns
   2) when sorting columns it now returns the correct value,
   3) the dimensions are now automatic but not exaggerated!

   Pierpaolo Martinello pier_dot_martinello[at]alice.it
*/

#include "minigui.ch"
#include "i_winuser.ch"

*----------------------------------------------------------------------------*
PROCEDURE Main
*----------------------------------------------------------------------------*
   SET    DEFAULT ICON TO "ThumNails"
   SET CENTERWINDOW RELATIVE PARENT

   DEFINE WINDOW Win1 ;
         AT 0, 0 WIDTH 330 HEIGHT 400 ;
         TITLE "HMG Enhanced AChoice Test" ;
         ICON "ThumNails" ;
         MAIN ;
         NOMAXIMIZE NOSIZE ;
         ON RELEASE On_Release()

      DEFINE MAINMENU
         DEFINE POPUP "&File"
            ITEM "Test ACHOICE" ACTION Test()
            ITEM "Test SORT Achoice" ACTION Test(, .T., .T. )
            ITEM "Test SORT Achoice With Pipe Separator" ACTION Test(, .T., .T. )
            ITEM "Test SORT Achoice Start on element 3 " ACTION Test( 3, .T. )
            ITEM "Test SORT Achoice NO Header " ACTION Test( 0, .T. )
            SEPARATOR
            ITEM "Exit" ACTION ThisWindow.Release
         END POPUP
      END MENU

      ON KEY ESCAPE ACTION ThisWindow.Release

   END WINDOW

   IF INSTALL_READ_KEYBOARD() == .F.
      MsgInfo ( "ERROR al instalar READ_KEYBOARD" )
   ENDIF

   Win1.Center
   Win1.Activate

RETURN
/*
*/
PROCEDURE On_Release

   IF UNINSTALL_READ_KEYBOARD() == .F.
      MsgInfo ( "ERROR al desinstalar READ_KEYBOARD" )
   ENDIF

RETURN
/*
*/
*----------------------------------------------------------------------------*
FUNCTION Test( argStart, argSort, ArgPipe ) // test stub module
*----------------------------------------------------------------------------*
   LOCAL aChoices_ := { ;
      { "Date", "Is the time ", }, ;
      { "0 Date Before", "The world distruction ", "s" }, ;
      { "0 Date After", "The resurrection", "F" }, ;
      { "Month to Date", "", "A" }, ;
      { "Year to Date", "", "Q" }, ;
      { "aDate Range", "Z", "Super long string for extreme testing" } ;
      }

   LOCAL nChoice, argHeader := { "GQQ", "This header is long for a thousand whales", "GK" }

   DEFAULT argStart TO 1, argSort TO .F.
   IF argStart = 0
      argHeader := ""
      argStart := Max( 1, argStart )
   ENDIF
   nChoice := hmg_Achoice( NIL, aChoices_, argHeader,,, argSort, argStart, argPipe )

   IF nChoice > 0
      MSGINFO( aChoices_[ nChoice, 1 ], "Achoice return " + hb_ntos( nChoice ) )
   ELSE
      MsgExclamation( "No user choice !", "Achoice return 0" )
   ENDIF

RETURN NIL

/*
*/
*----------------------------------------------------------------------------*
FUNCTION hmg_Achoice( cTitle, aSelection_, cHead, cFont, nFontSize, lSort, nStart, lPipe )
*----------------------------------------------------------------------------*
   LOCAL nRetVal := 0

   LOCAL nWidth := 0, aHeading := {}
   LOCAL nHeight, aWdt

   LOCAL nAs, AS, nl, n, aTmp

   LOCAL aSelect
   LOCAL nhl := 0

   DEFAULT cTitle TO "Make a choice"
   DEFAULT aSelection_ TO {}
   DEFAULT cHead TO { "Options" }
   DEFAULT cFont TO _HMG_DefaultFontName
   DEFAULT nFontSize TO _HMG_DefaultFontSize + 2
   DEFAULT lSort TO .F., nStart TO 1, lPipe TO .F.

   aSelect := AClone ( aSelection_ ) // Necessary for don't touch the array origin
   nAs := Len( aSelect )

   // --> terminate and return 0 IF there are no selections specified
   IF nAs < 1
      RETURN nRetVal
   ENDIF

   IF ValType( cHead ) # "A"
      AAdd( aHeading, cHead )
   ELSE
      aHeading := cHead
   ENDIF

   FOR n = Len( aHeading ) TO 9
      AAdd( aHeading, "" )
   NEXT

   // calculation necessary to establish whether to display the Header
   AEval( aheading, {| x | nhl += if ( x = NIL, 0, Len( x ) ) } )

   awdt := Array( 10 ) ; AFill( awdt, 0 )

   // --> check IF the array is needed to be sorted out
   IF lSort
      aSelect := ASort( aSelect,,, {| x, y | Lower( x[ 1 ] ) < Lower( y[ 1 ] ) } )
   ENDIF

   // Make a correct Items entry for Grid and calculate the length
   FOR EACH AS IN aSelect
      nl := Len( as )
      aTmp := AClone( as )

      FOR n = 1 TO 10 - nl
         IF aTmp[ n ] == NIL // Clean NIL Values
            aTmp[ n ] := ""
         ENDIF
         AAdd( aTmp, "" )
      NEXT

      // process maxlen row array
      AEval( atmp, {| x, y | awdt[ y ] := Max( Max( awdt[ y ], Len( x ) ), Len( aheading[ y ] ) ) } )

      IF Lpipe
         FOR n = 2 TO nl // add a pipe separator
            IF Left( aTmp[ n ], 2 ) != "|"
               aTmp[ n ] = "| " + aTmp[ n ]
            ENDIF
         NEXT
      ENDIF

      aSelect[ HB_EnumIndex() ] := AClone( aTmp )
      // process maxlen row array

   NEXT
   // transform character lengths into widths
   AEval( awdt, {| x, y | aWdt[ y ] := GetTxtWidth( x + if( x > 0, 1, 0 ), nFontSize, cFont ) / 2 } )

   AEval( awdt, {| x | nWidth += x } )
   nWidth += GetBorderWidth()

   // calculate the character height for the dialog font
   nHeight := ( Len( aSelect ) + if( nhl < 1, 2, 4 ) ) * GetTxtHeight()

   IF nHeight >= GetdesktopHeight()
      Msgstop( "Too many lines to display ( " + hb_ntos( Len( aSelect ) ) + " )" + CRLF ;
         + "    I can only visualize some " + hb_ntos( Int( ( GetdesktopHeight() - 100 ) / GetTxtHeight() ) ), "Error !" )
      RETURN nRetVal
   ENDIF

   IF nWidth >= GetdesktopWidth()
      Msgstop( "Columns with text that is too wide." + CRLF + Space( 8 ) + "Please reduce them.", "Error !" )
      RETURN nRetVal
   ENDIF

   nHeight += GetTitleHeight() + GetBorderHeight() / 2

   DEFINE WINDOW frmAchoice ;
         CLIENTAREA nWidth, nHeight + 20 ;
         TITLE cTitle ;
         MODAL NOSIZE NOSYSMENU ;
         ON MOUSECLICK ThisWindow.Release ;
         ON INIT ( PAUSE_READ_VK( .T. ), GetKeyb( aSelect ), PAUSE_READ_VK( .F. ) )

      ON KEY ESCAPE ACTION ThisWindow.Release
      ON KEY RETURN ACTION ( nRetVal := frmAchoice.grdChoice.VALUE, ThisWindow.Release )

      DEFINE GRID grdChoice
         ROW 0
         COL 0
         WIDTH frmAchoice.WIDTH
         HEIGHT frmAchoice.HEIGHT
         HEADERS aHeading
         WIDTHS awdt
         ITEMS aSelect
         VALUE nStart
         FONTNAME cFont
         FONTSIZE nFontSize
         ON CHANGE ( nRetVal := This.CellRowIndex, GetKeyb( aSelect ) )
         ON DBLCLICK ( nRetVal := This.CellRowIndex, ThisWindow.Release )
         NOLINES Lpipe
         JUSTIFY { GRID_JTFY_LEFT }
         SHOWHEADERS nhl > 0
      END GRID

      DEFINE STATUSBAR FONT "Arial Black" SIZE 12
         STATUSITEM "" FONTCOLOR { 255, 0, 0 } CENTERALIGN
      END STATUSBAR

   END WINDOW

   CENTER WINDOW frmAchoice
   ACTIVATE WINDOW frmAchoice

   IF nRetVal > 0
      nRetVal := AScan( aSelection_, {| aVal | aVal[ 1 ] == aSelect[ nRetVal, 1 ] } )
   ENDIF

RETURN nRetVal
/*
*/
*----------------------------------------------------------------------------*
STATIC PROCEDURE GetKeyb ( aText )
*----------------------------------------------------------------------------*
   Local cKey := GET_LAST_VK_NAME()

   if len (cKey) < 2 .and. len (cKey) > 0
      frmAchoice.StatusBar.item( 1 ) := "You have press [" + cKey + "] Found : " +atext[ frmAchoice.grdchoice.VALUE, 1 ]
   Else
      frmAchoice.StatusBar.item( 1 ) := "You have select >"+ atext[ frmAchoice.grdchoice.VALUE, 1 ]+"<"
   Endif

RETURN
/*
*/
*----------------------------------------------------------------------------*
STATIC FUNCTION GetTxtWidth( cText, nFontSize, cFontName ) // get the width of the text
*----------------------------------------------------------------------------*
   LOCAL hFont, nWidth

   LOCAL cChr := 'W'

   IF ValType( cText ) == 'N'
      cText := Replicate( cChr, cText )
   ENDIF

   DEFAULT cText := Replicate( cChr, 2 ), ;
      cFontName := _HMG_DefaultFontName, ;
      nFontSize := _HMG_DefaultFontSize

   hFont := InitFont( cFontName, nFontSize + 2 )
   nWidth := GetTextWidth( 0, cText, hFont )

   DeleteObject( hFont )

RETURN nWidth
/*
*/
*----------------------------------------------------------------------------*
STATIC FUNCTION GetTxtHeight( cText, nFontSize, cFontName, lBold )
*----------------------------------------------------------------------------*
   LOCAL hFont, nHeight
   DEFAULT cText := "B", ;
      cFontName := _HMG_DefaultFontName, ;
      nFontSize := _HMG_DefaultFontSize, ;
      lBold := .F.

   hFont := InitFont( cFontName, nFontSize, lBold )
   nHeight := GetTextHeight( 0, cText, hFont )
   DeleteObject( hFont )

RETURN nHeight

#pragma begindump

#include <windows.h>
#include "hbapi.h"

HB_BOOL flag_hhk = FALSE;
HB_BOOL PAUSE_hhk = FALSE;
HHOOK hhk = NULL;
HB_LONG VK_PRESIONADO = 0;
HB_LONG VK_lParam = 0;


LRESULT CALLBACK KeyboardProc(int nCode, WPARAM wParam, LPARAM lParam)
{
    if (nCode < 0)
        return CallNextHookEx(hhk, nCode, wParam, lParam);

    if (PAUSE_hhk == FALSE)
    {   VK_PRESIONADO = (long) wParam;
        VK_lParam = (LONG) lParam;
    }
    else
    {   VK_PRESIONADO = 0;
        VK_lParam = 0;
    }

    return CallNextHookEx(hhk, nCode, wParam, lParam);
}

HB_FUNC (GET_LAST_VK_NAME)
{
   CHAR cadena [128];

   if (flag_hhk == TRUE)
      {  GetKeyNameText (VK_lParam, (LPTSTR) &cadena, 128);
         hb_retc (cadena);
      }
   else
      hb_retc ("");
}

HB_FUNC (PAUSE_READ_VK)
{
   if (hb_pcount () == 1 && hb_parinfo (1) == HB_IT_LOGICAL)
   {   if (hb_parl (1) == TRUE)
       {   VK_PRESIONADO = 0;
           VK_lParam = 0;
       }
       PAUSE_hhk = hb_parl (1);
   }
}

HB_FUNC (INSTALL_READ_KEYBOARD)
{
   if (flag_hhk == FALSE)
   {    hhk = SetWindowsHookEx (WH_KEYBOARD, KeyboardProc, (HINSTANCE) NULL, GetCurrentThreadId());

        if (hhk == NULL)
            hb_retl (FALSE);
        else
        {   flag_hhk = TRUE;
            hb_retl (TRUE);
        }
   }
   else
      hb_retl (TRUE);
}

HB_FUNC (UNINSTALL_READ_KEYBOARD)
{
   if (flag_hhk == TRUE)
   {   if (UnhookWindowsHookEx (hhk) == TRUE)
       {   flag_hhk = FALSE;
           hb_retl (TRUE);
       }
       else
           hb_retl (FALSE);
   }
   else
      hb_retl (TRUE);
}

#pragma enddump
