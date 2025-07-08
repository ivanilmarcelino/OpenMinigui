/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2021 Петр  http://clipper.borda.ru/?32-netp
 *
 * Пример работы Дата + Время / Работа с объектом DateTime
 * Example of work Date + Time / Working with DateTime Object
*/
ANNOUNCE RDDSYS

#include "minigui.ch"

FUNCTION Main()

   LOCAL nFSize := 16, cFName := "Arial"
   LOCAL nWDate := 360, nRow := 30, nCol := 20, nG := 20
   LOCAL dDateTime, dDate, nCol2, nHSay := nFSize * 3

   DEFINE WINDOW Form_1 ;
         AT 0, 0 ;
         WIDTH 530 ;
         HEIGHT 300 ;
         TITLE "MiniGUI DateTime Demo" ;
         MAIN ;
         FONT cFName SIZE nFSize

      dDateTime := hb_DateTime()
      dDate := hb_TToD( dDateTime )

      @ nROW, nCol DATEPICKER Date_1 /*VALUE dDate*/ WIDTH nWDate HEIGHT nHSay ;
         SHOWNONE UPDOWN DATEFORMAT "dd MMMM yyyy' | 'HH:mm:ss"
      // important
      Form_1.Date_1.VALUE := dDateTime

      nCol2 := nCol + Form_1.Date_1.WIDTH + 10
      @ nROW, nCol2 BUTTONEX Btn_11 WIDTH nHSay HEIGHT nHSay CAPTION '' ;
         PICTURE "MINIGUI_EDIT_CANCEL" NOHOTLIGHT NOXPSTYLE HANDCURSOR ;
         ACTION ( dtp_ChangeTimePart( Form_1.Date_1.Handle, 0, 0, 0 ) )

      nCol2 += Form_1.Btn_11.WIDTH + 10
      @ nROW, nCol2 BUTTONEX Btn_12 WIDTH nHSay HEIGHT nHSay CAPTION '' ;
         PICTURE "MINIGUI_EDIT_OK" NOHOTLIGHT NOXPSTYLE HANDCURSOR ;
         ACTION ( dtp_ChangeTimePart( Form_1.Date_1.Handle, 23, 59, 59 ) )

      nRow += Form_1.Date_1.HEIGHT + nG

      @ nROW, nCol DATEPICKER Date_2 VALUE ( dDate += 2 ) WIDTH nWDate HEIGHT nHSay ;
         SHOWNONE UPDOWN DATEFORMAT "dd MMMM yyyy' | 'HH:mm:ss"
      // important
      Form_1.Date_2.VALUE := { Year( dDate ), Month( dDate ), Day( dDate ), 23, 59, 59 }

      nCol2 := nCol + Form_1.Date_2.WIDTH + 10
      @ nROW, nCol2 BUTTONEX Btn_21 WIDTH nHSay HEIGHT nHSay CAPTION '' ;
         PICTURE "MINIGUI_EDIT_CANCEL" NOHOTLIGHT NOXPSTYLE HANDCURSOR ;
         ACTION ( dtp_ChangeTimePart( Form_1.Date_2.Handle, 0, 0, 0 ) )

      nCol2 += Form_1.Btn_21.WIDTH + 10
      @ nROW, nCol2 BUTTONEX Btn_22 WIDTH nHSay HEIGHT nHSay CAPTION '' ;
         PICTURE "MINIGUI_EDIT_OK" NOHOTLIGHT NOXPSTYLE HANDCURSOR ;
         ACTION ( dtp_ChangeTimePart( Form_1.Date_2.Handle, 23, 59, 59 ) )

      nRow += Form_1.Date_2.HEIGHT + nG

      @ nROW, nCol BUTTON Button_1 CAPTION "Get a FILTER condition" ;
         WIDTH nWDate HEIGHT 35 ;
         ACTION MsgInfo( mySearchString() )

      nRow += Form_1.Button_1.HEIGHT + 5

      @ nROW, nCol BUTTON Button_2 CAPTION "UnCheck DATEPICKUPs (Set to Null)" ;
         WIDTH nWDate HEIGHT 35 ;
         ACTION ( SetDatePickNull( Form_1.Date_1.Handle ), SetDatePickNull( Form_1.Date_2.Handle ) )

   END WINDOW

   CENTER WINDOW Form_1
   ACTIVATE WINDOW Form_1

RETURN NIL


FUNCTION mySearchString()

   LOCAL tDateTime1, tDateTime2
   LOCAL cCondition

   IF ( Form_1.Date_1.Checked .AND. Form_1.Date_2.Checked )
      tDateTime1 := Form_1.Date_1.VALUE
      tDateTime2 := Form_1.Date_2.VALUE
      cCondition := hb_StrFormat( 'SKLAD->TSZ >= t"%s" .AND. SKLAD->TSZ <= t"%s"', ;
         hb_TToC( tDateTime1, "YYYY-MM-DD", "hh:mm:ss.fff" ), ;
         hb_TToC( tDateTime2, "YYYY-MM-DD", "hh:mm:ss.fff" ) )
   ELSEIF Form_1.Date_1.Checked
      tDateTime1 := Form_1.Date_1.VALUE
      cCondition := 'SKLAD->TSZ >= t"' + hb_TToC( tDateTime1, "YYYY-MM-DD", "hh:mm:ss.fff" ) + '"'
   ELSEIF Form_1.Date_2.Checked
      tDateTime2 := Form_1.Date_2.VALUE
      cCondition := 'SKLAD->TSZ <= t"' + hb_TToC( tDateTime2, "YYYY-MM-DD", "hh:mm:ss.fff" ) + '"'
   ELSE
      cCondition := "No condition"
   ENDIF

RETURN cCondition


FUNCTION dtp_ChangeTimePart( nHandle, nHour, nMinute, nSecond )

   LOCAL tDateTime, dDate

   hb_defaultValue( @nHour, 0 )
   hb_defaultValue( @nMinute, 0 )
   hb_defaultValue( @nSecond, 0 )

   IF dtp_IsChecked( nHandle )
      tDateTime := dtp_GetDatetime( nHandle )
      dDate := hb_TToD( tDateTime )
   ELSE
      dDate := Date()
   ENDIF

   tDateTime := hb_DateTime( Year( dDate ), Month( dDate ), Day( dDate ), nHour, nMinute, nSecond )

RETURN dtp_SetDatetime( nHandle, tDateTime )
