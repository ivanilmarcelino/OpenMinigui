/*
 *  HMG Hyperlink Demo
 *  Copyright 2005 Roberto Lopez <mail.box.hmg@gmail.com>
 *  http://www.hmgforum.com//
 *
 *  Revised by Pablo César
 *
 *  Now all URL it's accepting any URL and CodeBlocks is allowed
 *  Changing display color when accessed it
 *  GetProperty for address was fixed
 *
 */

#include "hmg.ch"

MEMVAR nColor
MEMVAR aColorNow

FUNCTION Main()

   LOCAL aItems := {}
   LOCAL aImages := { 'C1', 'C2', 'C3', 'C4', 'C5', 'C6', 'C7', 'C8' }

   PRIVATE nColor := 1
   PRIVATE aColorNow := { ;
      { { 000, 000, 255 }, { 102, 000, 153 } }, ; // Conventional
      { { 000, 000, 102 }, { 153, 153, 204 } }, ; // Blue
      { { 000, 102, 102 }, { 153, 255, 255 } }, ; // Cyan
      { { 080, 080, 080 }, { 153, 153, 153 } }, ; // Gray
      { { 000, 153, 000 }, { 000, 051, 000 } }, ; // Green
      { { 204, 000, 204 }, { 102, 051, 102 } }, ; // Margenta
      { { 204, 000, 000 }, { 153, 051, 051 } }, ; // Red
      { { 255, 255, 000 }, { 153, 153, 000 } } }  // Yellow

   AAdd ( aItems, "Conventional" )
   AAdd ( aItems, "Blue        " )
   AAdd ( aItems, "Cyan        " )
   AAdd ( aItems, "Gray        " )
   AAdd ( aItems, "Green       " )
   AAdd ( aItems, "Margenta    " )
   AAdd ( aItems, "Red         " )
   AAdd ( aItems, "Yellow      " )

   DEFINE WINDOW Main_form ;
         AT 0, 0 ;
         WIDTH 646 ;
         HEIGHT 400 ;
         TITLE 'HyperLinks and Labels' ;
         MAIN ;
         TOPMOST NOSIZE NOMAXIMIZE

      @ 010, 015 FRAME Frame_1 CAPTION "HYPERLINKs" WIDTH 300 HEIGHT 280

      @ 040, 060 HYPERLINK H01 VALUE "UPU - Universal Post Union" ADDRESS 'https://www.upu.int/en/Members-Centre' WIDTH 250 HANDCURSOR
      @ 070, 060 HYPERLINK H02 VALUE "EMail by default engine" ADDRESS 'mailto:someone@example.com' WIDTH 250 HANDCURSOR
      @ 100, 060 HYPERLINK H03 VALUE "Wikipedia (https)" ADDRESS 'https://en.wikipedia.org/wiki/Main_Page' WIDTH 250 HANDCURSOR
      @ 130, 060 HYPERLINK H04 VALUE "www.hmgforum.com" ADDRESS 'http://www.hmgforum.com' WIDTH 250 HANDCURSOR
      @ 160, 060 HYPERLINK H05 VALUE "Wrong forum address" ADDRESS 'http://www.hmgforum1.com' WIDTH 250 HANDCURSOR
      @ 190, 060 HYPERLINK H06 VALUE "ErrorLog file (common error HMG file)" ADDRESS 'ErrorLog.Htm' WIDTH 250 HANDCURSOR
      @ 220, 060 HYPERLINK H07 VALUE "Word file" ADDRESS 'TEST.docx' WIDTH 250 HANDCURSOR

      @ 102, 020 IMAGE I03 PICTURE "NEW" ACTION MsgNew( "https" ) TRANSPARENT TOOLTIP "Click on to know more about"
      @ 132, 020 IMAGE I04 PICTURE "NEW" ACTION MsgNew( "www" ) TRANSPARENT TOOLTIP "Click on to know more about"
      @ 162, 020 IMAGE I05 PICTURE "NEW" ACTION MsgNew( "wrong" ) TRANSPARENT TOOLTIP "Click on to know more about"
      @ 192, 020 IMAGE I06 PICTURE "NEW" ACTION MsgNew( "ErrorLog" ) TRANSPARENT TOOLTIP "Click on to know more about"
      @ 222, 020 IMAGE I07 PICTURE "NEW" ACTION MsgNew( "Word file" ) TRANSPARENT TOOLTIP "Click on to know more about"

      @ 010, 325 FRAME Frame_2 CAPTION "LABELs" WIDTH 300 HEIGHT 280

      @ 040, 360 LABEL H08 VALUE "UPU - Universal Post Union" TOOLTIP 'https://www.upu.int/en/Members-Centre' WIDTH 250 UNDERLINE FONTCOLOR aColorNow[ nColor, 1 ]
      @ 070, 360 LABEL H09 VALUE "EMail by default engine" TOOLTIP 'mailto:someone@example.com' WIDTH 250 UNDERLINE FONTCOLOR aColorNow[ nColor, 1 ]
      @ 100, 360 LABEL H10 VALUE "Wikipedia (https)" TOOLTIP 'https://en.wikipedia.org/wiki/Main_Page' WIDTH 250 UNDERLINE FONTCOLOR aColorNow[ nColor, 1 ]
      @ 130, 360 LABEL H11 VALUE "www.hmgforum.com (www)" TOOLTIP 'www.hmgforum.com' WIDTH 250 UNDERLINE FONTCOLOR aColorNow[ nColor, 1 ]
      @ 160, 360 LABEL H12 VALUE "Wrong forum address" TOOLTIP 'www.hmgforum1.com' WIDTH 250 UNDERLINE FONTCOLOR aColorNow[ nColor, 1 ]
      @ 190, 360 LABEL H13 VALUE "ErrorLog file (common error HMG file)" TOOLTIP 'ErrorLog.Htm' WIDTH 250 UNDERLINE FONTCOLOR aColorNow[ nColor, 1 ]
      @ 220, 360 LABEL H14 VALUE "Word file" TOOLTIP 'TEST.docx' WIDTH 250 UNDERLINE FONTCOLOR aColorNow[ nColor, 1 ]

      @ 250, 360 LABEL H15 VALUE "CodeBlock simulating HYPERLINK" ACTION ( MsgInfo( 'Hiperlink test message' ), ChangeColor( This.NAME, .F. ) ) WIDTH 250 UNDERLINE FONTCOLOR aColorNow[ nColor, 1 ]

      @ 294, 240 FRAME Frame_3 CAPTION "Links color setup" WIDTH 160 HEIGHT 064

      @ 320, 260 COMBOBOXEX Combo_1 ITEMS aItems VALUE 1 WIDTH 120 HEIGHT 180 ON CHANGE ( nColor := This.Value, ChangeAll() ) IMAGE aImages

      DEFINE BUTTON Button_1
         ROW 310
         COL 90
         WIDTH 90
         HEIGHT 28
         ACTION MsgDebug( GetProperty( "Main_form", "H01", "ADDRESS" ) )
         CAPTION "Get address"
         FONTNAME "Arial"
         FONTSIZE 9
      END BUTTON

      DEFINE BUTTON Button_2
         ROW 310
         COL 460
         WIDTH 90
         HEIGHT 28
         ACTION MsgDebug( GetProperty( "Main_form", "H08", "ADDRESS" ) )
         CAPTION "Get Address"
         FONTNAME "Arial"
         FONTSIZE 9
      END BUTTON

   END WINDOW
   CENTER WINDOW Main_form

   AddAllHandCursor()
   AddAllSetAddress()

   ACTIVATE WINDOW Main_form

RETURN NIL


FUNCTION MsgNew( cOption )

   LOCAL cMsg := ""

   DO CASE
   CASE cOption == "https"
      cMsg += "Before only http (Hypertext Transfer Protocol) was accepting as valid link address." + CRLF
      cMsg += "Now we have the need and opportunity to access https (Hyper Text Transfer Protocol Secure)." + CRLF
   CASE cOption == "www"
      cMsg += "Now it also accepts www that is very common address." + CRLF
   CASE cOption == "wrong"
      cMsg += "Even if the address is wrong or there is no server, the web browser displays a corresponding message." + CRLF
   CASE cOption == "ErrorLog"
      cMsg += "You can make view of ErrorLog because its has HTML format." + CRLF
   CASE cOption == "Word file"
      cMsg += "Also any text or any file can be displaying or actioned by the explorer association file." + CRLF
   ENDCASE

   MsgInfo( cMsg, "New for " + cOption )

RETURN NIL


PROCEDURE AddAllHandCursor()

   LOCAL i, cCtrl

   FOR i = 8 TO 15
      cCtrl := "H" + StrZero( i, 2, 0 )
      InitHyperLinkCursor( cCtrl, "Main_form" )
   NEXT

RETURN


PROCEDURE InitHyperLinkCursor( cCtrl, Form )

   LOCAL nHandle := GetControlHANDLE( cCtrl, Form )

   SetWindowCursor( nHandle, IDC_HAND )

RETURN


PROCEDURE AddAllSetAddress()

   LOCAL i, cCtrl
   LOCAL cURL

   FOR i = 8 TO 14
      cCtrl := "H" + StrZero( i, 2, 0 )
      cURL := _GetToolTip( cCtrl, "Main_form" )
      MySetAddressControlProcedure( cCtrl, cURL )
   NEXT

RETURN


PROCEDURE MySetAddressControlProcedure( cCtrl, URL )

   LOCAL bCode, nIndex := GetControlIndex( cCtrl, "Main_form" )

   IF ( "@" $ URL )
      bCode := {|| ShellExecute( 0, "open", "rundll32.exe", "url.dll,FileProtocolHandler mailto:" + URL, , 1 ), ChangeColor( This.Name, .F. ) }
   ELSE
      bCode := {|| ShellExecute( 0, "open", "rundll32.exe", "url.dll,FileProtocolHandler " + URL, , 1 ), ChangeColor( This.Name, .F. ) }
   ENDIF

   _HMG_aControlProcedures[ nIndex ] := bCode
   _HMG_aControlType[ nIndex ] := "HYPERLINK"
   _HMG_aControlValue[ nIndex ] := URL

RETURN


PROCEDURE ChangeColor( cCtrlName, lOn )

   SetProperty( "Main_form", cCtrlName, "FONTCOLOR", aColorNow[ nColor, iif( lOn, 1, 2 ) ] )

RETURN


PROCEDURE ChangeAll()

   LOCAL i, cCtrl

   FOR i = 1 TO 15
      cCtrl := "H" + StrZero( i, 2, 0 )
      ChangeColor( cCtrl, .T. )
   NEXT

RETURN
