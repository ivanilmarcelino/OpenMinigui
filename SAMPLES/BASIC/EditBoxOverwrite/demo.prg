/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2013 Dr. Claudio Soto <srvet@adinet.com.uy>
 *
 * Adapted for MiniGUI Extended Edition by Grigory Filatov
*/

#include "SET_COMPILE_HMG_UNICODE.ch"

#include "hmg.ch"
#include "i_winuser.ch"

FUNCTION Main()

   SET FONT TO "Arial", 10

   DEFINE WINDOW Form_1 ;
         AT 0, 0 ;
         WIDTH 500 HEIGHT 300 ;
         TITLE 'EditBox Create Event Demo' ;
         MAIN ;
         ON RELEASE EventRemove()

      @ 30, 10 EDITBOX Edit_1 ;
         WIDTH 400 ;
         HEIGHT 200 ;
         VALUE 'EditBox: Overwrite (press INSERT key)'

      DEFINE TIMER Timer_1 INTERVAL 100 ACTION OnkeyOfEditBox()
   END WINDOW

   CREATE EVENT PROCNAME OnkeyOfEditBox()

   Form_1.Center()
   Form_1.Activate()

RETURN NIL


FUNCTION OnkeyOfEditBox( hWnd, nMsg, wParam /*, lParam*/ )

   LOCAL nIndex
   LOCAL nEnd, cChar
   LOCAL nWidth, nHeight
   LOCAL lOverwrite

   DEFAULT nMsg := 0

   hWnd := GetFocus()
   nIndex := GetControlIndexByHandle ( hWnd )
   IF nIndex > 0 .AND. GetControlTypeByIndex ( nIndex ) == "EDIT"
      Form_1.Timer_1.Enabled := .F.

      lOverwrite := ! IsInsertActive()

      nEnd := HiWord( SendMessage( hWnd, EM_GETSEL, 0, 0 ) )
      cChar := HMG_EditControlGetChar( hWnd, nEnd )

      HMG_GetAverageFontSize( hWnd, @nWidth, @nHeight )

#ifdef UNICODE
      nWidth := HMG_GetCharWidth( hWnd, cChar )

#else
      nWidth := Max( nWidth, .75 * HMG_GetCharWidth( hWnd, cChar ) )

#endif
      IF lOverwrite
         CreateCaret( hWnd, 0, Int( nWidth ), nHeight )
      ELSE
         #define SM_CXBORDER 5
         CreateCaret( hWnd, 0, GetSystemMetrics( SM_CXBORDER ), nHeight )
      ENDIF

      ShowCaret( hWnd )

      IF nMsg == WM_CHAR
         IF lOverwrite .AND. wParam <> VK_RETURN .AND. wParam <> VK_BACK
            SendMessage( hWnd, EM_SETSEL, nEnd, nEnd + 1 )
         ENDIF
      ENDIF

      Form_1.Timer_1.Enabled := .T.
   ENDIF

RETURN NIL


FUNCTION HMG_GetAverageFontSize( hWnd, nWidth, nHeight )

   LOCAL hDC, aMetr

   hDC := GetDC( hWnd )
   aMetr := GetTextMetric( hDC )
   ReleaseDC( hWnd, hDC )
   nHeight := aMetr[ 1 ]
   nWidth := aMetr[ 2 ]

RETURN NIL
