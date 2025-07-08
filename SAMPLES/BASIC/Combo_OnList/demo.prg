/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2002-05 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
 *
 * Combo OnList events demo
 * (C) 2005 Martin Waller <wallerm@freenet.de>
 *
 * Main procedure name changed - Kevin Carmody - 2007.03.28
*/

#include "minigui.ch"

PROCEDURE Main

   LOCAL nRow, nColumn, abKeyBlocks, aResults

   nRow := 0
   nColumn := 0
   abKeyBlocks := Array( 0 )
   aResults := Array( 0 )

   DEFINE WINDOW Demo ;
         AT 0, 0 ;
         WIDTH 0 ;
         HEIGHT 0 ;
         MAIN ;
         TITLE "Combo OnList events demo - RETURN: Search and ESC: Close" ;
         NOSIZE ;
         NOSYSMENU ;
         FONT "Verdana" ;
         SIZE 10

      ON KEY ESCAPE ACTION {|| ThisWindow.Release() }
      ON KEY RETURN ACTION {|| MsgBox( "Searching..." ) }

      nRow := Demo.ROW + 15
      nColumn := Demo.COL + 15

      @ nROW, nColumn LABEL DemoLabel1 ;
         VALUE "Search for:" ;
         AUTOSIZE ;
         FONT "VERDANA" ;
         SIZE 10

      @ nROW - 2, ;
         GetProperty( "Demo", "DemoLabel1", "COL" ) + ;
         GetProperty( "Demo", "DemoLabel1", "WIDTH" ) + 55 ;
         COMBOBOX DemoCombo ;
         ITEMS { "Number", "Name" } ;
         VALUE 1 ;
         WIDTH 210 ;
         HEIGHT 90 ;
         FONT "VERDANA" ;
         SIZE 10 ;
         ON LISTDISPLAY {|| abKeyBlocks := _KeysOff(), ;
         MSGBOX( "Now ESC closes the combolist." + Chr( 10 ) + Chr( 13 ) + ;
            "Not the window! -" + Chr( 10 ) + Chr( 13 ) + ;
            "RETURN chooses the combolist item" + Chr( 10 ) + Chr( 13 ) + ;
            "and doesn't start searching!" ) } ;
         ON LISTCLOSE {|| _KeysOn( abKeyBlocks ), ;
            MSGBOX( "Now ESC again closes the window." + Chr( 10 ) + Chr( 13 ) + ;
            "RETURN again starts searching!" ) }

      @ GETPROPERTY( "Demo", "DemoLabel1", "ROW" ) + ;
         GetProperty( "Demo", "DemoLabel1", "HEIGHT" ) + 10, ;
         nColumn LABEL DemoLabel2 ;
         VALUE "Searchargument  :" ;
         AUTOSIZE ;
         FONT "VERDANA" ;
         SIZE 10

      @ GETPROPERTY( "Demo", "DemoLabel2", "ROW" ) - 2, ;
         GetProperty( "Demo", "DemoLabel2", "COL" ) + ;
         GetProperty( "Demo", "DemoLabel2", "WIDTH" ) + 5 ;
         TEXTBOX DemoText1 ;
         VALUE "" ;
         WIDTH 210 ;
         HEIGHT GetProperty( "Demo", "DemoLabel2", "HEIGHT" ) ;
         MAXLENGTH 25 ;
         FONT "VERDANA" ;
         SIZE 10

      @ GETPROPERTY( "Demo", "DemoCombo", "ROW" ), ;
         GetProperty( "Demo", "DemoCombo", "COL" ) + ;
         GetProperty( "Demo", "DemoCombo", "WIDTH" ) + 60 ;
         BUTTON DemoButton1 ;
         CAPTION "&Search" ;
         ON CLICK {|| MSGBOX( "Searching..." ) } ;
         FONT "VERDANA" ;
         SIZE 10

      @ GETPROPERTY( "Demo", "DemoButton1", "ROW" ) + ;
         GetProperty( "Demo", "DemoButton1", "HEIGHT" ) + 10, ;
         GetProperty( "Demo", "DemoButton1", "COL" ) ;
         BUTTON DemoButton2 ;
         CAPTION "&Cancel" ;
         ON CLICK {|| DoMethod( "Demo", "RELEASE" ) } ;
         FONT "VERDANA" ;
         SIZE 10

      SetProperty( "Demo", "WIDTH", ;
         GetProperty( "Demo", "DemoButton1", "COL" ) + ;
         GetProperty( "Demo", "DemoButton1", "WIDTH" ) + 15 )

      SetProperty( "Demo", "HEIGHT", ;
         GetProperty( "Demo", "DemoButton2", "ROW" ) + ;
         GetProperty( "Demo", "DemoButton2", "HEIGHT" ) + 35 )

   END WINDOW

   Demo.Center()
   Demo.Activate()

RETURN

/*-------------------------------------------------------------------------------------------*/

STATIC FUNCTION _KeysOff

   LOCAL bKeyBlock, abRetVal := Array( 0 )

   STORE KEY ESCAPE OF Demo TO bKeyBlock
   AAdd( abRetVal, bKeyBlock )
   RELEASE KEY ESCAPE OF Demo

   STORE KEY RETURN OF Demo TO bKeyBlock
   AAdd( abRetVal, bKeyBlock )
   RELEASE KEY RETURN OF Demo

RETURN( abRetVal )

/*-------------------------------------------------------------------------------------------*/

STATIC PROCEDURE _KeysOn( abKeyBlocks )

   _DefineHotKey( "Demo", 0, 27, abKeyBlocks[ 1 ] )
   _DefineHotKey( "Demo", 0, 13, abKeyBlocks[ 2 ] )

RETURN
