/*
 * MiniGUI Custom Caption Window Demo
 */

#include "minigui.ch"
#include "i_winuser.ch"

#define CLR_W7GRAY		RGB( 201, 201, 201 )
#define CLR_MSRED		RGB( 232,  17,  35 )
#define CLR_MSGRAY		RGB( 229, 229, 229 )

#define COLOR_BTNFACE       15

#define HTCAPTION           2
#define WM_NCLBUTTONDOWN    161

PROCEDURE Main()

   LOCAL aButtons[ 3 ]

   LOCAL bResize := {|| EraseWindow( This.Name ), CreateLine(), ;
      This.Title_1.WIDTH := ThisWindow.ClientWidth - 185, This.(aButtons[ 1 ]).COL := ThisWindow.ClientWidth - 46, ;
      This.(aButtons[ 2 ]).COL := ThisWindow.ClientWidth - 92, This.(aButtons[ 3 ]).COL := ThisWindow.ClientWidth - 138 }

   SET EVENTS FUNCTION TO App_OnEvents

   DEFINE WINDOW Form_1 ;
         AT 0, 0 ;
         WIDTH 500 ;
         MINWIDTH 310 ;
         HEIGHT 300 ;
         MINHEIGHT 100 ;
         MAIN ;
         ON SIZE Eval( bResize ) ;
         ON MAXIMIZE Eval( bResize ) ;
         FONT _GetSysFont() SIZE 9 ;
         ON MOUSECLICK MoveActiveWindow()

      DRAW ICON IN WINDOW Form_1 AT 4, 8 PICTURE "demo.ico" WIDTH 16 HEIGHT 16

      DEFINE LABEL Title_1
         ROW 1
         COL 29
         WIDTH ThisWindow.ClientWidth - 172
         HEIGHT 23
         VALUE 'Application title'
         ALIGNMENT LEFT //CENTER
         VCENTERALIGN .T.
      END LABEL

      DEFINE IMAGE NUL
         ROW 1
         COL ThisWindow.ClientWidth - 46
         WIDTH 45
         HEIGHT 29
         PICTURE 'closew.png'
         TRANSPARENT .T.
         STRETCH .T.
         ON MOUSEHOVER This.BackgroundColor := CLR_MSRED
         ON MOUSELEAVE This.BackgroundColor := GetSysColor( COLOR_BTNFACE )
         ACTION ThisWindow.RELEASE
      END IMAGE
      aButtons[ 1 ] := ATail( HMG_GetFormControls( this.Name, "IMAGE" ) )

      DEFINE IMAGE NUL
         ROW 1
         COL ThisWindow.ClientWidth - 92
         WIDTH 45
         HEIGHT 29
         PICTURE 'max.png'
         STRETCH .T.
         TRANSPARENT .T.
         ON MOUSEHOVER This.BackgroundColor := iif( isseven(), CLR_W7GRAY, CLR_MSGRAY )
         ON MOUSELEAVE This.BackgroundColor := GetSysColor( COLOR_BTNFACE )
         ACTION iif( IsZoomed( ThisWindow.Handle ), ThisWindow.RESTORE, ThisWindow.Maximize )
      END IMAGE
      aButtons[ 2 ] := ATail( HMG_GetFormControls( this.Name, "IMAGE" ) )

      DEFINE IMAGE NUL
         ROW 1
         COL ThisWindow.ClientWidth - 138
         WIDTH 45
         HEIGHT 29
         PICTURE 'min.png'
         STRETCH .T.
         TRANSPARENT .T.
         ON MOUSEHOVER This.BackgroundColor := iif( isseven(), CLR_W7GRAY, CLR_MSGRAY )
         ON MOUSELEAVE This.BackgroundColor := GetSysColor( COLOR_BTNFACE )
         ACTION iif( IsIconic( ThisWindow.Handle ), ThisWindow.Restore, ThisWindow.Minimize )
      END IMAGE
      aButtons[ 3 ] := ATail( HMG_GetFormControls( this.Name, "IMAGE" ) )

   END WINDOW

   CreateLine()

   Form_1.TitleBar := .F.

   CENTER WINDOW Form_1

   ACTIVATE WINDOW Form_1

RETURN


FUNCTION CreateLine()

   LOCAL nClientWidth := Form_1.ClientWidth

   DRAW ICON IN WINDOW Form_1 AT 4, 8 PICTURE "demo.ico" WIDTH 16 HEIGHT 16

   DRAW LINE IN WINDOW Form_1 ;
      AT 30, 1 TO 30, nClientWidth ;
      PENCOLOR iif( IsWinXPorLater(), { 172, 168, 153 }, GRAY )

   DRAW LINE IN WINDOW Form_1 ;
      AT 31, 1 TO 31, nClientWidth ;
      PENCOLOR WHITE

RETURN NIL


PROCEDURE MoveActiveWindow( hWnd )

   DEFAULT hWnd := GetActiveWindow()

   PostMessage( hWnd, WM_NCLBUTTONDOWN, HTCAPTION, 0 )

   RC_CURSOR( "MINIGUI_FINGER" )

RETURN

/*----------------------------------------------------------------------*/

FUNCTION App_OnEvents( hWnd, nMsg, wParam, lParam )

   LOCAL nResult
   LOCAL ControlCount, i, k, x

   SWITCH nMsg

   CASE WM_SIZE

      ControlCount := Len ( _HMG_aControlHandles )

      i := AScan ( _HMG_aFormHandles, hWnd )

      IF i > 0

            IF ( k := _HMG_aFormReBarHandle [i] ) > 0

               SizeRebar ( k )
               RebarHeight ( k )
               RedrawWindow ( k )

            ENDIF

            FOR x := 1 TO ControlCount

               IF _HMG_aControlParentHandles [x] == hWnd

                  IF _HMG_aControlType [x] == "MESSAGEBAR"

                     MoveWindow( _HMG_aControlHandles [x] , 0 , 0 , 0 , 0 , .T. )
                     RefreshItemBar ( _HMG_aControlHandles [x] , _GetStatusItemWidth( hWnd, 1 ) )

                     IF ( k := GetControlIndex( 'ProgressMessage', GetParentFormName( x ) ) ) != 0
                        RefreshProgressItem ( _HMG_aControlMiscData1 [k, 1], _HMG_aControlHandles [k], _HMG_aControlMiscData1 [k, 2] )
                     ENDIF
                     EXIT

                  ENDIF

               ENDIF

            NEXT x

            IF _HMG_MainActive == .T.

               IF wParam == SIZE_MAXIMIZED

                  _DoWindowEventProcedure ( _HMG_aFormMaximizeProcedure [i], i )

                  IF _HMG_AutoAdjust .AND. _HMG_MainClientMDIHandle == 0
                     _Autoadjust( hWnd )
                  ENDIF

               ELSEIF wParam == SIZE_MINIMIZED

                  _DoWindowEventProcedure ( _HMG_aFormMinimizeProcedure [i], i )

               ELSEIF wParam == SIZE_RESTORED .AND. !IsWindowSized( hWnd )

                  _DoWindowEventProcedure ( _HMG_aFormRestoreProcedure [i], i )

               ELSE

                  _DoWindowEventProcedure ( _HMG_aFormSizeProcedure [i], i )

                  IF _HMG_AutoAdjust .AND. _HMG_MainClientMDIHandle == 0
                     _Autoadjust( hWnd )
                  ENDIF

               ENDIF

            ENDIF

      ENDIF

      FOR i := 1 TO ControlCount

         IF _HMG_aControlParentHandles [i] == hWnd

            IF _HMG_aControlType [i] == "TOOLBAR"
               SendMessage ( _HMG_aControlHandles [i], TB_AUTOSIZE, 0, 0 )
            ENDIF

         ENDIF

      NEXT i

      nResult := 0
      exit

   OTHERWISE
      nResult := Events( hWnd, nMsg, wParam, lParam )

   END SWITCH

RETURN nResult
