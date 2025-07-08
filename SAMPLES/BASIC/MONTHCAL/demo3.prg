/*
 * HMG - Harbour Win32 GUI library Demo
 *
 * Copyright 2002 Roberto Lopez <mail.box.hmg@gmail.com>
 * http://www.hmgforum.com//
*/

#include "hmg.ch"

FUNCTION MAIN

   DEFINE WINDOW Form_1 ;
         CLIENTAREA 620, 460 ;
         TITLE "Month Calendar Control Demo" ;
         ICON "DEMO.ICO" ;
         MAIN ;
         FONT "Arial" SIZE 9

      // month calendar 1

      @ 10, 10 MONTHCALENDAR Month_1 ;
         VALUE Date() ;
         TOOLTIP "Month Calendar Control NoToday" ;
         NOTODAY ;
         INVISIBLE ;
         ON CHANGE MsgInfo ( "Month_1 Change", "Info" )

      @ 10, 370 BUTTON Button_11 ;
         CAPTION "SHOW" ;
         ACTION Form_1.Month_1.Show

      @ 50, 370 BUTTON Button_12 ;
         CAPTION "HIDE" ;
         ACTION Form_1.Month_1.Hide

      @ 90, 370 BUTTON Button_13 ;
         CAPTION "IS VISIBLE ?" ;
         ACTION MsgInfo ( if ( Form_1.Month_1.Visible, "TRUE", "FALSE" ), "Info" )

      @ 10, 490 BUTTON Button_14 ;
         CAPTION "SET DATE" ;
         ACTION Form_1.Month_1.VALUE := Date()

      @ 50, 490 BUTTON Button_15 ;
         CAPTION "GET DATE" ;
         ACTION MsgInfo ( GetDate ( Form_1.Month_1.Value ), "Info" )

      // month calendar 2

      @ 210, 10 MONTHCALENDAR Month_2 ;
         VALUE D"2001/01/01" ;
         FONT "Arial" SIZE 12 ;
         TOOLTIP "Month Calendar Control NoTodayCircle WeekNumbers NoTabStop BoldDays" ;
         NOTODAYCIRCLE ;
         WEEKNUMBERS ;
         NOTABSTOP ;
         ON CHANGE MsgInfo( "Month_2 Change", "Info" )

      @ 210, 370 BUTTON Button_21 ;
         CAPTION "SHOW" ;
         ACTION Form_1.Month_2.Show

      @ 250, 370 BUTTON Button_22 ;
         CAPTION "HIDE" ;
         ACTION Form_1.Month_2.Hide

      @ 290, 370 BUTTON Button_23 ;
         CAPTION "IS VISIBLE ?" ;
         ACTION MsgInfo ( if ( Form_1.Month_2.Visible, "TRUE", "FALSE" ), "Info" )

      @ 210, 490 BUTTON Button_24 ;
         CAPTION "SET DATE" ;
         ACTION Form_1.Month_2.VALUE := D"2001/01/01"

      @ 250, 490 BUTTON Button_25 ;
         CAPTION "GET DATE" ;
         ACTION MsgInfo ( GetDate ( Form_1.Month_2.Value ), "Info" )

      @ 290, 490 BUTTON Button_26 ;
         CAPTION "SET COLOR" ;
         ACTION Form_1.Month_2.BACKCOLOR := LIME

   END WINDOW

   BoldDays( BOM( Form_1.Month_2.Value ), EOM( Form_1.Month_2.Value ) )

   SetMonthCalView( Form_1.Month_2.Handle, 0 )

   CENTER WINDOW Form_1

   ACTIVATE WINDOW Form_1

RETURN NIL


STATIC FUNCTION GetDate ( dDate )

   LOCAL nDay := Day( dDate )
   LOCAL nMonth := Month( dDate )
   LOCAL nYear := Year( dDate )
   LOCAL cRet := ""

   cRet += "Day: " + StrZero( nDay, 2 )
   cRet += Space( 2 )
   cRet += "Month: " + StrZero( nMonth, 2 )
   cRet += Space( 2 )
   cRet += "Year: " + StrZero( nYear, 4 )

RETURN cRet


STATIC FUNCTION BoldDays ( dStart, dEnd )

   LOCAL aBold := {}
   LOCAL dTest := dStart

   WHILE dTest <= dEnd
      IF Day( dTest ) == 1 .OR. Day( dTest ) == 15 .OR. Day( dTest ) == 29
         AddMonthCalBoldDay( 'Month_2', 'Form_1', dTest )
      ENDIF
      dTest++
   END WHILE

RETURN aBold
