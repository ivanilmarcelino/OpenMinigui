/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Demo was contributed to HMG forum by Edward 16/Oct/2021
 *
 * Adapted for MiniGUI Extended Edition by Grigory Filatov
 */

#include "hmg.ch"

#define SM_CXFRAME 32
#define SM_CYFRAME 33

FUNCTION MAIN

   LOCAL nXSizeFrame := GetSystemMetrics( SM_CXFRAME )
   LOCAL nYSizeFrame := GetSystemMetrics( SM_CYFRAME )

   PUBLIC nMaximizedWindowCol := 0 - nXSizeFrame
   PUBLIC nMaximizedWindowRow := 0 - nYSizeFrame
   PUBLIC nMaximizedWindowWidth := Sys.ClientWidth + nXSizeFrame * 2
   PUBLIC nMaximizedWindowHeight := Sys.ClientHeight + nYSizeFrame * 2

   DEFINE WINDOW Form_1 ;
         AT nMaximizedWindowRow, nMaximizedWindowCol ;
         WIDTH nMaximizedWindowWidth ;
         HEIGHT nMaximizedWindowHeight ;
         TITLE 'Window 1' ;
         MAIN

      ON KEY F2 ACTION MsgInfo( "F2 key pressed" )

      @ 2, 10 LABEL Label_1 ;
         WIDTH 100 HEIGHT 20 ;
         VALUE 'Press F1' ;
         ACTION HMG_PressKey ( VK_F1 )

      @ 2, 120 LABEL Label_2 ;
         WIDTH 100 HEIGHT 20 ;
         VALUE 'Press F2' ;
         ACTION HMG_PressKey ( VK_F2 )

      @ 30, 10 BUTTON Button_1 ;
         CAPTION 'Window 2' ;
         ACTION Window2() ;
         TOOLTIP 'WINDOW 2'

      @ 70, 10 BUTTON Button_2 ;
         CAPTION 'Release' ;
         ACTION ThisWindow.RELEASE
   END WINDOW

   CREATE EVENT PROCNAME F1_KEY_TRAP()

   MAXIMIZE WINDOW Form_1
   ACTIVATE WINDOW Form_1

RETURN NIL

PROCEDURE WINDOW2()

   IF isWindowDefined( Form_2 )
      DoMethod( 'Form_2', 'SetFocus' )
      DoMethod( 'Form_2', 'Restore' )
      RETURN
   ENDIF

   DEFINE WINDOW Form_2 ;
         AT nMaximizedWindowRow, nMaximizedWindowCol ;
         WIDTH nMaximizedWindowWidth ;
         HEIGHT nMaximizedWindowHeight ;
         TITLE 'Window 2'

      @ 2, 10 LABEL Label_1 ;
         WIDTH 100 HEIGHT 20 ;
         VALUE 'Press F1' ;
         ACTION HMG_PressKey ( VK_F1 )

      @ 70, 10 BUTTON Button_2 ;
         CAPTION 'Release' ;
         ACTION ThisWindow.RELEASE
   END WINDOW

   ACTIVATE WINDOW Form_2

RETURN
***********************************************************
FUNCTION F1_KEY_TRAP( nHWnd, nMsg )

   LOCAL cFormName

   #define WM_ACTIVATE    6

   IF nMsg == WM_ACTIVATE

      cFormName := GetFormNameByIndex( GetFormIndexByHandle( nHWnd ) )

      ON KEY F1 OF &( cFormName ) ACTION MsgInfo( "FormName: " + ThisWindow.NAME + CRLF + ;
         "Form Title: " + ThisWindow.TITLE + CRLF + ;
         "width :=" + AllTrim( hb_ValToStr( ThisWindow.Width ) ) + CRLF + ;
         "height:=" + AllTrim( hb_ValToStr( ThisWindow.Height ) ) + CRLF + ;
         "row   :=" + AllTrim( hb_ValToStr( ThisWindow.Row ) ) + CRLF + ;
         "col   :=" + AllTrim( hb_ValToStr( ThisWindow.Col ) ) )

   ENDIF

RETURN NIL
