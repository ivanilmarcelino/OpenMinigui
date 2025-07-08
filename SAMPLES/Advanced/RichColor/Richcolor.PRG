/*
Versão Designer : 3.2412.0.0 RELEASE SPECIAL PALLADIUM (Pd) Year V 241218 2249
==============================================================================================================
https://github.com/ivanilmarcelino/designer by IVANIL MARCELINO <ivanil.marcelino@yahoo.com.br>
--------------------------------------------------------------------------------------------------------------
Projeto : Richcolor
--------------------------------------------------------------------------------------------------------------
*/

#include <hmg.ch>
/*
  Main function to initialize the application
*/
FUNCTION Main()

   LOCAL oColor

   // Load the main application window from a template named "Richcolor"
   LOAD WINDOW Richcolor AS MAIN

   // Initialize a new TColorRTF object for rich text formatting
   oColor := TColorRTF():New( "Main", "oRichEditEX1" )

   // Set the title of the main window
   Main.Title := "Colored RichEditEX"

   // Attach an event handler for text change events in the RichEdit control
   Main.oRichEditEX1.OnChange := {|| oColor:Refresh() }

   // Set the background color of the RichEdit control to black
   Main.oRichEditEX1.BackGroundColor := { 0, 0, 0 }
   Main.oRichEditEX1.value := "FUNCTION "

   // Select all text in the RichEdit control, set font color to white, and unselect text
   Main.oRichEditEX1.SelectAll()
   Main.oRichEditEX1.FontColor := { 255, 255, 255 }
   Main.oRichEditEX1.FontBackColor := { 0, 0, 0 }
   Main.oRichEditEX1.UnSelectAll()
   Main.oRichEditEX1.CaretPos := Main.oRichEditEX1.GetTextLength

   // Set the initialization event handler for the main window
   Main.OnInit := {|| OnInit( oColor ) }

   // Center the main window on the screen and activate it
   Main.Center()
   Main.Activate()

RETURN NIL
/*
  Initialization function for the application
*/
STATIC FUNCTION OnInit( oColor )

   // Set properties for function keywords
   WITH OBJECT oColor:oFunction
      :FontSize := 12
      :FontName := "Calibri"
      :Fontcolor := BLUE
      :Lista := { "function", "return", "if", "endif" }
   END WITH
   // Set properties for reserved keywords
   WITH OBJECT oColor:oReserved
      :FontSize := 12
      :FontName := "Arial"
      :Fontcolor := RED
      :Fontbold := TRUE
      :Lista := { "While", "Enddo", "for", "next" }
   END WITH
   IF ! Empty( oColor:text )
      oColor:Refresh()
   ENDIF

RETURN NIL
/*
  Class definition for TColorRTF
*/
FUNCTION TColorRTF()

   STATIC s_oClass
   LOCAL oClass
   IF s_oClass == NIL .AND. __clsLockDef( @s_oClass )
      oClass := HBClass():New( "TColorRTF" ) // Create a new class

      // Add data members to the clas
      oClass:AddData( "handle", 0 )
      oClass:AddData( "form", NIL )
      oClass:AddData( "control", NIL )
      oClass:AddData( "oFunction", Nil )
      oClass:AddData( "oReserved", Nil )
      oClass:AddData( "oNumber", Nil )
      oClass:AddData( "oSymbol", Nil )
      oClass:AddData( "oPadrao", Nil )
      oClass:AddData( "nStart", 0 )
      oClass:AddData( "nStop", 0 )
      oClass:AddDAta( "nPos", 0 )
      oClass:AddDAta( "nLen", 0 )
      oClass:AddDAta( "Member", "0123456789abcdefghijklmnopqrstuvxwyz" )
      oClass:AddDAta( "range", "" )
      oClass:AddDAta( "Text", "" )
      oClass:AddDAta( "lPaste", .F. )
      oClass:AddData( "bCorPad", Nil )
      oClass:AddData( "bCorNew", Nil )

      // Add methods to the class
      oClass:AddMethod( "New", @New() )
      oClass:AddMethod( "SetRange", @setRange() )
      oClass:AddMethod( "refresh", @refresh() )

      oClass:Create()
      __clsUnlockDef( @s_oClass, oClass )
   ENDIF

RETURN s_oClass:Instance()
/*
  Constructor for TColorRTF
*/
DECLARE WINDOW &( ::Form )
DECLARE WINDOW &( cForm )
STATIC FUNCTION New( cForm, cControl )

   LOCAL Self := QSelf() // Get the current object instance

   // Initialize object properties
   ::Form := cForm
   ::control := cControl
   ::Handle := &( ::form ).&( ::control ).handle
   ::oFunction := RTFCORPROP( cForm, cControl )
   ::oReserved := RTFCORPROP( cForm, cControl )
   ::oSymbol := RTFCORPROP( cForm, cControl )
   ::oNumber := RTFCORPROP( cForm, cControl )
   ::oPadrao := RTFCORPROP( cForm, cControl )

   // Define formatting for numeric and symbol properties
   ::oNumber:Lista := { "0", "1", "2", "3", "4", "5", "6", "7", "8", "9" }
   ::oNumber:Unico := TRUE
   ::oSymbol:Unico := TRUE

   // Define default and custom color properties
   ::bCorPad := {|| &( ::form ).&( ::control ).fontcolor := ::oPadrao:Fontcolor, ;
      &( ::form ).&( ::control ).fontbold := ::oPadrao:fontbold, ;
      &( ::form ).&( ::control ).fontitalic := ::oPadrao:fontItalic, ;
      &( ::form ).&( ::control ).fontsize := ::oPadrao:fontsize }

   ::bCorNew := {| o | &( ::form ).&( ::control ).fontcolor := o:Fontcolor, ;
      &( ::form ).&( ::control ).fontbold := o:fontbold, ;
      &( ::form ).&( ::control ).fontitalic := o:fontItalic, ;
      &( ::form ).&( ::control ).fontsize := o:fontsize }

RETURN Self
/*
*/
STATIC FUNCTION SetRange( nStart, nStop )

   LOCAL Self := QSelf()
   ::nStart := iif( ! Empty( nStart ), nStart, ::nStart )
   ::nStop := iif( ! Empty( nStop ), nStop, ::nStop )
   RichEditBox_SetSelRange ( ::Handle, { ::nStart, ::nStop } )

RETURN Self
/*
  Class definition for RTFCORPROP
*/
STATIC FUNCTION RTFCORPROP( cForm, cControl )

   STATIC s_oClass
   LOCAL oClass
   IF s_oClass == NIL .AND. __clsLockDef( @s_oClass )
      oClass := HBClass():New( "RTFCOR" )
      oClass:AddData( "cor", BLUE )
      oClass:AddData( "unico", FALSE )
      oClass:AddData( "fontbold", &( cform ).&( cControl ).fontbold )
      oClass:AddData( "backcolor", &( cform ).&( cControl ).backcolor )
      oClass:AddData( "fontcolor", &( cform ).&( cControl ).backcolor )
      oClass:AddData( "fontname", &( cform ).&( cControl ).fontname )
      oClass:AddData( "fontsize", &( cform ).&( cControl ).fontsize )
      oClass:AddData( "fontitalic", &( cform ).&( cControl ).fontitalic )

      oClass:AddData( "lista", {} )
      oClass:Create()
      __clsUnlockDef( @s_oClass, oClass )
   ENDIF

RETURN s_oClass:Instance()
/*
*/
STATIC FUNCTION refresh()

   LOCAL Self := QSelf()
   STATIC lFarol := .F.
   IF ! lFarol
      lFarol := TRUE
      ::nPos := &( ::form ).&( ::control ).CaretPos
      LOCKWINDOWUPDATE ( ::handle )
      Setcor1( self )
      LOCKWINDOWUPDATE ( 0 )
      &( ::form ).&( ::control ).CaretPos := ::nPos
      lFarol := FALSE
   ENDIF

RETURN Self
/*
*/
STATIC FUNCTION setcor1( self )

   LOCAL c, lFound, aListAll := { ::oFunction, ::oReserved, ::oSymbol, ::oNumber }, o
   ::Text := IFCHAR( &( ::form ).&( ::control ).value, &( ::form ).&( ::control ).value, "" )
   ::nLen := Len( ::Text )
   ::nStart := Max( ::nPos - 1, 0 )
   ::nStop := ::nPos + 1
   IF Empty( ::text )
      ::lPaste := TRUE
      Eval( ::bCorPad )
      RETURN NIL
   ENDIF
   // IF ::lPaste
   // ::lpaste:=FALSE
   // waitwindow("loading...")
   // setcor2(self)
   // waitwindow()
   // Return Nil
   // endif
   WHILE ::nStart > 0 .AND. hb_AtI( SubStr( ::Text, ::nStart, 1 ), ::Member ) > 0
      ::nStart--
   ENDDO
   WHILE ::nStop <= ::nLen .AND. hb_AtI( SubStr( ::Text, ::nStop, 1 ), ::Member ) > 0
      ::nStop++
   ENDDO
   ::Range := SubStr( ::Text, ::nStart + 1, ::nStop - ( ::nStart + 1 ) )
   ::Range := StrTran( StrTran( ::Range, Chr( 13 ), " " ), Chr( 10 ), " " )
   lFound := FALSE
   ::SetRange()
   FOR EACH o IN aListAll
      FOR EACH c IN o:Lista
         IF Len( c ) > Len( ::Range )
            LOOP
         ENDIF
         IF o:Unico .AND. SubStr( ::Text, ::nPos - 1, 1 ) == C
            ::SetRange( ::nPos - 1, ::nPos - 1 )
            Eval( ::bCorNew, o )
            lFound := TRUE
            EXIT
         ELSEIF hb_WildMatchI( AllTrim( ::Range ), c )
            Eval( ::bCorNew, o )
            lFound := TRUE
         ENDIF
      NEXT
      IF lFound
         EXIT
      ENDIF
   NEXT
   IF ! lFound
      Eval( ::bCorPad )
   ENDIF

RETURN Self
