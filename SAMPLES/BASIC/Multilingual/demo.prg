/*
 * MiniGUI Multilingual Interface Demo
 * (c) 2007 Grigory Filatov
*/

#include "minigui.ch"

#define ID_TITLE 1
#define ID_COMBO 2
#define ID_BUTTON 3
#define ID_LBL1  4
#define ID_LBL2  5
#define ID_DESCRIPT 6
#define ID_RESULT 7

MEMVAR StrDefs
*------------------------------------------------------------------------------*
FUNCTION Main
*------------------------------------------------------------------------------*
   PUBLIC StrDefs := Array( 10 )

   AFill( StrDefs, "" )

   SET NAVIGATION EXTENDED

   LOAD WINDOW DEMO AS Main
   CENTER WINDOW Main
   ACTIVATE WINDOW Main

RETURN NIL

*------------------------------------------------------------------------------*
FUNCTION SetFormCaptions()
*------------------------------------------------------------------------------*
   LOCAL aArr := {}

   AAdd( aArr, 'Main' )      // object
   AAdd( aArr, ID_TITLE )    // identifier
   AAdd( aArr, 'Label_1' )   // object
   AAdd( aArr, ID_COMBO )    // identifier
   AAdd( aArr, 'Button_1' )  // object
   AAdd( aArr, ID_BUTTON )   // identifier
   AAdd( aArr, 'Label_2' )   // object
   AAdd( aArr, ID_LBL1 )     // identifier
   AAdd( aArr, 'Label_3' )   // object
   AAdd( aArr, ID_LBL2 )     // identifier
   AAdd( aArr, 'Label_5' )   // object
   AAdd( aArr, ID_DESCRIPT ) // identifier
   AAdd( aArr, 'Label_4' )   // object
   AAdd( aArr, ID_RESULT )   // identifier

   SetCaptions( aArr )

RETURN NIL

*------------------------------------------------------------------------------*
FUNCTION Form_Load()
*------------------------------------------------------------------------------*
   LOCAL n := 1, s

   Main.Combo_1.DeleteAllItems
   WHILE .T.
      s := GetLangName( n ) // get the language name by number
      // If invalid # of language — STOP:
      IF s == ""
         EXIT
      ENDIF
      Main.Combo_1.AddItem( s ) // Add language in list
      n++ // increment
   END WHILE

   // Select language with number 1 in list:
   Main.Combo_1.VALUE := 1
   // Initialising of Multilingual Interface for selected language:
   InitLangInterface ( Main.Combo_1.Value )

   SetFormCaptions() // make inscriptions

   CalculateResult()

RETURN NIL

*------------------------------------------------------------------------------*
FUNCTION GetLangName( iLangNumber )
*------------------------------------------------------------------------------*
   LOCAL cFileName := GetStartupFolder() + "\langdefs.dat"
   LOCAL sReadStr, i, LangName := ""
   LOCAL oFile

   IF File( cFileName )
      oFile := TFileRead():New( cFileName )
      oFile:Open()
      IF oFile:Error()
         MsgStop( oFile:ErrorMsg( "FileRead: " ), "Error" )
      ELSE
         sReadStr := oFile:ReadLine()
         oFile:Close()
         FOR i = 1 TO NumToken( sReadStr, "/" )
            IF i == iLangNumber
               LangName := Token( sReadStr, "/", i )
               EXIT
            ENDIF
         NEXT
      ENDIF
   ENDIF

Return( LangName )

*------------------------------------------------------------------------------*
FUNCTION InitLangInterface( iLangNumber )
*------------------------------------------------------------------------------*
   LOCAL cFileName := GetStartupFolder() + "\langdefs.dat"
   LOCAL sReadStr, iID, f
   LOCAL oFile

   IF File( cFileName )
      oFile := TFileRead():New( cFileName )
      oFile:Open()
      IF oFile:Error()
         MsgStop( oFile:ErrorMsg( "FileRead: " ), "Error" )
      ELSE
         // pass over first string with
         // list of available languages
         sReadStr := oFile:ReadLine()

         WHILE oFile:MoreToRead() // while not end of file

            sReadStr := oFile:ReadLine()
            If ! Empty( sReadStr ) .AND. At( Chr( 26 ), sReadStr ) = 0

               iID := Val( sReadStr ) // read out identifier
               // seek for needed string after identifier:
               FOR f = 1 TO iLangNumber // for f from 1 to number of selected language
                  sReadStr := oFile:ReadLine()
               NEXT
               StrDefs[ iID ] := sReadStr // put the string into array

               // pass over strings while is empty string:
               WHILE ! Empty( sReadStr )
                  IF At( Chr( 26 ), sReadStr ) > 0 // if end of file, then
                     oFile:Close() // close file
                     RETURN NIL // exit
                  ENDIF
                  sReadStr := oFile:ReadLine()
               END WHILE

            ENDIF

         END WHILE

         oFile:Close()
      ENDIF
   ENDIF

RETURN NIL

*------------------------------------------------------------------------------*
FUNCTION SetCaptions( aArray )
*------------------------------------------------------------------------------*
   LOCAL i, t, n

   // Index steps across one: 1, 3, 5... — thus get the object's list.
   // So, Array[i] — object, Array[i + 1] — identifier.
   FOR i = 1 TO Len( aArray ) STEP 2
      IF i < 2
         Main.TITLE := GetStr( aArray[ i + 1 ] )
      ELSE
         n := aArray[ i ]
         t := GetControlType ( n, 'Main' )
         IF t == 'LABEL'
            SetProperty( 'Main', n, 'Value', GetStr( aArray[ i + 1 ] ) )
         ELSEIF t == 'BUTTON'
            SetProperty( 'Main', n, 'Caption', GetStr( aArray[ i + 1 ] ) )
         ENDIF
      ENDIF

   NEXT i

RETURN NIL

*------------------------------------------------------------------------------*
FUNCTION GetStr( iStrID )
*------------------------------------------------------------------------------*

RETURN StrTran( StrDefs[ iStrID ], "/R\", Chr( 13 ) )

*------------------------------------------------------------------------------*
FUNCTION CalculateResult()
*------------------------------------------------------------------------------*
   LOCAL v1 := Main.Text_1.VALUE
   LOCAL v2 := Main.Text_2.VALUE

   Main.Label_4.VALUE := GetStr( ID_RESULT ) + ": " + LTrim( Transform( v1 + v2, "999,999,999.99" ) )

RETURN NIL

*------------------------------------------------------------------------------*
FUNCTION InterfaceLanguage_Click() // Select language from list
*------------------------------------------------------------------------------*
   // Initialising of Multilingual Interface for selected language:
   InitLangInterface ( Main.Combo_1.Value )

   SetFormCaptions() // make inscriptions

   CalculateResult()

RETURN NIL

#ifdef __XHARBOUR__
#include <fileread.prg>

#endif
