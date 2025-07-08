/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2021 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2021 Verchenko Andrey <verchenkoag@gmail.com>
 *
 * Пример работы Дата + Время / Работа с объектом DateTime
 * Example of work Date + Time / Working with DateTime Object
*/
ANNOUNCE RDDSYS

#include "minigui.ch"

FUNCTION Main()

   LOCAL nFSize := 16, cFName := "Arial"
   LOCAL nWDate, nWTime, x, y, nG, x2, cTime, cSay, nHObj

   SET OOP ON

   SET CENTURY ON

   SET NAVIGATION EXTENDED

   nWDate := 290
   nWTime := 160
   nG := 20
   cTime := Space( 6 )
   nHObj := nFSize * 2

   DEFINE WINDOW Form_1 ;
         AT 0, 0 ;
         WIDTH 600 ;
         HEIGHT 490 ;
         TITLE "MiniGUI Date + Time Demo" ;
         MAIN ;
         FONT cFName SIZE nFSize

      y := 20
      x := nG

      cSay := "Period from:"
      @ y, x LABEL Label_1 VALUE cSay WIDTH nWDate HEIGHT nHObj

      y += Form_1.Label_1.HEIGHT

      // дата период начало
      @ y, x DATEPICKER Date_1 VALUE Date() WIDTH nWDate HEIGHT nHObj ;
         DATEFORMAT "dd'.'MMMM' 'yyyy" SHOWNONE

      x2 := Form_1.Date_1.COL + Form_1.Date_1.WIDTH + nG

      // время период начало
      @ y, x2 GETBOX Time_1 VALUE cTime WIDTH nWTime HEIGHT nHObj ;
         PICTURE "@R 99:99:99" VALID {| og | bValid( og ) } BUTTONWIDTH nHObj ;
         ON GOTFOCUS {|| SendMessage( This.Handle, 177 /*EM_SETSEL*/, 0, Len( This.Value ) ) } ;
      ON INIT {|| _SetAlign ( This.NAME, ThisWindow.NAME, "CENTER" ) } ;
         ACTION ( This.VALUE := Space( 6 ) ) IMAGE { "MINIGUI_EDIT_CANCEL", NIL }

      y += Form_1.Date_1.HEIGHT + nG * 2

      cSay := "Period to:"
      @ y, x LABEL Label_2 VALUE cSay WIDTH nWDate HEIGHT nHObj

      y += Form_1.Label_2.HEIGHT

      // дата период конец
      @ y, x DATEPICKER Date_2 VALUE Date() + 2 WIDTH nWDate HEIGHT nHObj ;
         DATEFORMAT "dd'.'MMMM' 'yyyy" SHOWNONE

      // время период конец
      @ y, x2 GETBOX Time_2 VALUE cTime WIDTH nWTime HEIGHT nHObj ;
         PICTURE "@R 99:99:99" VALID {| og | bValid( og ) } BUTTONWIDTH nHObj ;
         ON GOTFOCUS {|| SendMessage( This.Handle, 177 /*EM_SETSEL*/, 0, Len( This.Value ) ) } ;
      ON INIT {|| _SetAlign ( This.NAME, ThisWindow.NAME, "CENTER" ) } ;
         IMAGE { "MINIGUI_EDIT_OK", "MINIGUI_EDIT_CANCEL" } ;
         ACTION ( Form_1.Time_2.VALUE := "235959" ) ;
         ACTION2 ( Form_1.Time_2.VALUE := cTime )

      y += Form_1.Date_2.HEIGHT + nG

      @ y, x BUTTON Button_1 CAPTION "Get a FILTER condition" ;
         WIDTH nWDate + nWTime + nG HEIGHT 35 ;
         ACTION ( Form_1.Label_Search.VALUE := mySearchString() )

      y += Form_1.Button_1.HEIGHT + nG * 2

      @ y, x LABEL Label_Search VALUE "Search line:" WIDTH Form_1.WIDTH - 40 ;
         HEIGHT 80 TOOLTIP "Search line" FONTCOLOR RED

   END WINDOW

   CENTER WINDOW Form_1
   ACTIVATE WINDOW Form_1

RETURN NIL

// ----------------------------------------------------------------------------------- *
FUNCTION mySearchString()
// ----------------------------------------------------------------------------------- *
   LOCAL dDate1, dDate2, cDate1, cDate2, cTime1, cTime2, cRet, cStr

   dDate1 := Form_1.Date_1.VALUE
   dDate2 := Form_1.Date_2.VALUE
   cDate1 := hb_DToC( dDate1, 'YYYY-MM-DD' )
   cDate2 := hb_DToC( dDate2, 'YYYY-MM-DD' )
   cTime1 := Form_1.Time_1.VALUE
   cTime2 := Form_1.Time_2.VALUE
   IF ! Empty( cTime1 )
      cTime1 := Left( Trim( This.Time_1.Value ) + repl( "0", 6 ), 6 )
      cTime1 := Transform( cTime1, "@R 99:99:99" )
   ENDIF
   IF ! Empty( cTime2 )
      cTime2 := Left( Trim( This.Time_2.Value ) + repl( "0", 6 ), 6 )
      cTime2 := Transform( cTime2, "@R 99:99:99" )
   ENDIF
   cRet := "!DELETED() .AND. "

   // поиск по полю TSZ типа "T@="
   // = ModTime 8 Last modified date & time of this record
   // @ DayTime 8 Date & Time
   // T Time 4 or 8 Only time (if width is 4 ) or Date & Time (if width is 8 )

   IF ( Empty( dDate1 ) .AND. Empty( dDate2 ) )
      cRet := 'No search data! Exit!'

   ELSEIF ( ! Empty( dDate1 ) .AND. Empty( dDate2 ) )
      IF Empty( cTime1 )
         cRet += 'SKLAD->TSZ >= 0d' + cDate1
      ELSE
         cRet += 'SKLAD->TSZ >= t"' + cDate1 + ' ' + cTime1 + '"'
      ENDIF

   ELSEIF ( Empty( dDate1 ) .AND. ! Empty( dDate2 ) )
      IF Empty( cTime2 )
         cRet += 'SKLAD->TSZ <= 0d' + cDate2
      ELSE
         cRet += 'SKLAD->TSZ <= t"' + cDate2 + ' ' + cTime2 + '"'
      ENDIF

   ELSE
      IF ( Empty( cTime1 ) .OR. Empty( cTime2 ) )
         cStr := '(SKLAD->TSZ >= 0d%s .AND. SKLAD->TSZ <= 0d%s)'
         cRet += hb_StrFormat( cStr, hb_DToC( dDate1, 'YYYY-MM-DD' ), hb_DToC( dDate2, 'YYYY-MM-DD' ) )
      ELSE
         cStr := '(SKLAD->TSZ >= t"%s %s" .AND. SKLAD->TSZ <= t"%s %s")'
         cRet += hb_StrFormat( cStr, hb_DToC( dDate1, 'YYYY-MM-DD' ), cTime1, hb_DToC( dDate2, 'YYYY-MM-DD' ), cTime2 )
      ENDIF

   ENDIF

RETURN cRet

// ----------------------------------------------------------------------------------- *
STATIC FUNCTION bValid( oGet )  // проверка правильности времени в GetBox
// ----------------------------------------------------------------------------------- *
   LOCAL lRet, lVl1, lVl2, lVl3, nVal
   LOCAL cVal := Left( Trim( oGet:VarGet() ) + repl( "0", 6 ), 6 )
   LOCAL hGet := oGet:Control
   LOCAL hWnd := ThisWindow.Handle

   lVl1 := lVl2 := lVl3 := .F.

   nVal := Val( Left( cVal, 2 ) )
   IF nVal >= 0 .AND. nVal < 24 ; lVl1 := .T.
   ENDIF
   nVal := Val( subs( cVal, 3, 2 ) )
   IF nVal >= 0 .AND. nVal < 60 ; lVl2 := .T.
   ENDIF
   nVal := Val( subs( cVal, 5, 2 ) )
   IF nVal >= 0 .AND. nVal < 60 ; lVl3 := .T.
   ENDIF

   lRet := lVl1 .AND. lVl2 .AND. lVl3

   IF ! lRet
      // есть команды\ф-ии управления временем Tooltip, если надо исп. ShowGetValid можно применить
      // т.е. сохранить старое, поставить новое и потом после ShowGetValid (InkeyGui) восстановить
      SetFocus( hGet )
      ShowGetValid( hGet, This.NAME + ": Please enter a right time value ! ", 'ERROR ' + ThisWindow.NAME, 'E' )
      InkeyGui( 5 * 1000 )
      SetFocus( hWnd )

      // --- это можно использовать без ShowGetValid() ------
      oGet:VarPut( Space( 6 ) )
      oGet:Refresh()
      SetFocus( hGet )
      lRet := .T.
   ENDIF

RETURN lRet

#pragma BEGINDUMP

#define _WIN32_WINNT 0x0600

#include <windows.h>

#include "hbapi.h"
#include "hbapicdp.h"

#include <commctrl.h>

#if ( defined( __BORLANDC__ ) && __BORLANDC__ < 0x582 )
typedef struct _tagEDITBALLOONTIP
{
   DWORD   cbStruct;
   LPCWSTR pszTitle;
   LPCWSTR pszText;
   INT     ttiIcon; // From TTI_*
} EDITBALLOONTIP, *PEDITBALLOONTIP;

#define EM_SHOWBALLOONTIP   (ECM_FIRST + 3)     // Show a balloon tip associated to the edit control
#define Edit_ShowBalloonTip(hwnd, peditballoontip)  (BOOL)SNDMSG((hwnd), EM_SHOWBALLOONTIP, 0, (LPARAM)(peditballoontip))
#define EM_HIDEBALLOONTIP   (ECM_FIRST + 4)     // Hide any balloon tip associated with the edit control
#define Edit_HideBalloonTip(hwnd)  (BOOL)SNDMSG((hwnd), EM_HIDEBALLOONTIP, 0, 0)

#define ECM_FIRST               0x1500      // Edit control messages

#endif  // (__BORLANDC__ < 0x582)

// ToolTip Icons (Set with TTM_SETTITLE)
#define TTI_NONE                0
#define TTI_INFO                1
#define TTI_WARNING             2
#define TTI_ERROR               3
#if (_WIN32_WINNT >= 0x0600)
  #define TTI_INFO_LARGE        4
  #define TTI_WARNING_LARGE     5
  #define TTI_ERROR_LARGE       6

#endif  // (_WIN32_WINNT >= 0x0600)

/*
   ShowGetValid( hWnd, cText [ , cTitul ]   [ , cTypeIcon ] )
*/

#if ( HB_VER_MAJOR == 3 )
  #define _hb_cdpGetU16( cdp, fCtrl, ch)  hb_cdpGetU16(cdp, ch )
  #define _hb_cdpGetChar(cdp, fCtrl, ch)  hb_cdpGetChar(cdp, ch)

#else
  #define _hb_cdpGetU16( cdp, fCtrl, ch)  hb_cdpGetU16(cdp, fCtrl, ch )
  #define _hb_cdpGetChar(cdp, fCtrl, ch)  hb_cdpGetChar(cdp, fCtrl, ch)

#endif

HB_FUNC( SHOWGETVALID )
{
   int i, k;
   const char *tp, *s;
   WCHAR Text[512];
   WCHAR Title[512];

   EDITBALLOONTIP bl;

   PHB_CODEPAGE  s_cdpHost = hb_vmCDP();

   HWND hWnd = ( HWND ) hb_parnl(1);

   if( ! IsWindow( hWnd ) )
      return;

   bl.cbStruct = sizeof( EDITBALLOONTIP );
   bl.pszTitle = NULL;
   bl.pszText  = NULL;
   bl.ttiIcon  = TTI_NONE;

   if( HB_ISCHAR( 2 ) ){

       ZeroMemory( Text,  sizeof(Text) );

       k = hb_parclen(2);
       s = (const char *) hb_parc(2);
       for(i=0;i<k;i++) Text[i] = _hb_cdpGetU16( s_cdpHost, TRUE, s[i] );
       bl.pszText  = Text;
   }

   if( HB_ISCHAR( 3 ) ){

       ZeroMemory( Title,  sizeof(Title) );

       k = hb_parclen(3);
       s = (const char *) hb_parc(3);
       for(i=0;i<k;i++) Title[i] = _hb_cdpGetU16( s_cdpHost, TRUE, s[i] );
       bl.pszTitle  = Title;
   }

   tp = ( const char * ) hb_parc(4);

   switch( *tp ){
       case 'E' :  bl.ttiIcon  = TTI_ERROR_LARGE;   break;
       case 'e' :  bl.ttiIcon  = TTI_ERROR;         break;

       case 'I' :  bl.ttiIcon  = TTI_INFO_LARGE;    break;
       case 'i' :  bl.ttiIcon  = TTI_INFO;          break;

       case 'W' :  bl.ttiIcon  = TTI_WARNING_LARGE; break;
       case 'w' :  bl.ttiIcon  = TTI_WARNING;       break;

   }

   Edit_ShowBalloonTip( hWnd, &bl );

}

#pragma ENDDUMP
