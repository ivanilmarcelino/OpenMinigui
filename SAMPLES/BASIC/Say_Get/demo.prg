#xcommand DEFINE LBLTEXTBOX <name> ;
      [ OF <cParent> ] ;
      ROW <nRow> COL <nCol> ;
      [ WIDTH <nW> ] ;
      CAPTION <cCaption> ;
      [ VALUE <cValue> ] ;
=> ;
      CreateTextboxWithLabel( <(name)>, <(cParent)>, <nRow>, <nCol>, <cCaption>, <nW>, <cValue> )

#xcommand END LBLTEXTBOX =>

#include "minigui.ch"

FUNCTION Main()

   LOCAL nWidth := 400 + GetBorderWidth() - iif( IsSeven(), 2, 0 )
   LOCAL nHeight := 170 + GetTitleHeight() + GetBorderHeight() - iif( IsSeven(), 2, 0 )

   IF ! _IsControlDefined( "DlgFont", "Main" )
      DEFINE FONT DlgFont FONTNAME "Segoe UI" SIZE 10
   ENDIF

   SET WINDOW MAIN OFF
   SET NAVIGATION EXTENDED

   DEFINE WINDOW MainForm ;
      WIDTH nWidth HEIGHT nHeight ;
      TITLE "Labeled TextBox Demo" ;
      MODAL ;
      NOSIZE ;
      FONT "Segoe UI" SIZE 10

      DEFINE LBLTEXTBOX Text_1 ;
         ROW 10 ;
         COL 135 ;
         WIDTH 250 ;
         CAPTION "Enter your name:" ;
         VALUE "User Name"
      END LBLTEXTBOX

      DEFINE LBLTEXTBOX Text_2 ;
         ROW 40 ;
         COL 135 ;
         WIDTH 250 ;
         CAPTION "Enter your address:" ;
         VALUE "User Address"
      END LBLTEXTBOX

      DEFINE LBLTEXTBOX Text_3 ;
         ROW 70 ;
         COL 135 ;
         WIDTH 250 ;
         CAPTION "Enter your city:" ;
         VALUE "User City"
      END LBLTEXTBOX

      DEFINE LBLTEXTBOX Text_4 ;
         ROW 100 ;
         COL 135 ;
         WIDTH 250 ;
         CAPTION "" ;
         VALUE "textbox without 'caption'"
      END LBLTEXTBOX

      DEFINE BUTTON Button_1
         ROW nHeight - GetTitleHeight() - GetBorderHeight() - iif( IsSeven(), 2, 0 ) - 35
         COL nWidth - GetBorderWidth() - iif( IsSeven(), 2, 0 ) - 80
         WIDTH 70
         CAPTION "Close"
         ACTION ThisWindow.RELEASE
      END BUTTON

   END WINDOW

   MainForm.Center()
   MainForm.Activate()

RETURN NIL


STATIC FUNCTION CreateTextboxWithLabel( textboxname, cParent, nR, nC, cCaption, nW, cValue )

   LOCAL hWnd := ThisWindow.Handle
   LOCAL hDC := GetDC( hWnd )
   LOCAL hDlgFont := GetFontHandle( "DlgFont" )
   LOCAL nLabelLen := GetTextWidth( hDC, cCaption, hDlgFont )

   ReleaseDC( hWnd, hDC )

   hb_default( @cParent, This.Name )
   hb_default( @nW, 120 )
   hb_default( @cValue, "" )

   DEFINE LABEL NUL
      PARENT cParent
      ROW nR
      COL nC - nLabelLen - GetBorderWidth()
      VALUE cCaption
      HEIGHT 24
      AUTOSIZE .T.
      VCENTERALIGN .T.
   END LABEL

   DEFINE TEXTBOX ( textboxname )
      PARENT cParent
      ROW nR
      COL nC
      WIDTH nW
      HEIGHT 24
      VALUE cValue
      ONGOTFOCUS SetProperty( ThisWindow.NAME, textboxname, "BackColor", YELLOW )
      ONLOSTFOCUS SetProperty( ThisWindow.NAME, textboxname, "BackColor", WHITE )
   END TEXTBOX

RETURN NIL
