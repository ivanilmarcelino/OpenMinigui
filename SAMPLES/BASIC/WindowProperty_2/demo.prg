/*
 * HMG Window Demo
 * (c) 2010 Roberto Lopez <mail.box.hmg@gmail.com>
 */

#include "hmg.ch"
#include "i_winuser.ch"

DECLARE WINDOW oSample1

/*----------------------------------------------------------------------*/

FUNCTION Main

   SET EVENTS FUNCTION TO App_OnEvents

   DEFINE WINDOW oWindow1;
      Width  550;
      Height 400;
      Title  'HMG Window Demo';
      MAIN;
      On Init       ( oWindow1.Center(), MsgInfo( "Init Event fired!" ) );
      On Release    MsgInfo( "Release event fired!" );
      On MouseClick MsgInfo( "Mouse Click Event Fired!" );
      On MouseMove  oWindow1.StatusBar.Item(3) := "Your Mouse Moved!";
      On Size       WindowResized();
      On Minimize   MsgInfo( 'Minimize Event' );
      On Maximize   ( WindowResized(), MsgInfo( 'Maximize Event' ) );
      On Paint      oWindow1.StatusBar.Item(4) := 'Window Repainted';
      Font 'Arial' Size 12

      DEFINE MAIN MENU
         DEFINE POPUP "Window Properties"
            POPUP "Row"
               MenuItem "Get Value" ACTION MsgInfo( oWindow1.Row )
               MenuItem "Set Value" ACTION oWindow1.Row := 100
            END POPUP
            POPUP "Col"
               MenuItem "Get Value" ACTION MsgInfo( oWindow1.Col )
               MenuItem "Set Value" ACTION oWindow1.Col := 100
            END POPUP
            POPUP "Width"
               MenuItem "Get Value" ACTION MsgInfo( oWindow1.Width )
               MenuItem "Set Value" ACTION oWindow1.Width := 600
            END POPUP
            POPUP "Height"
               MenuItem "Get Value" ACTION MsgInfo( oWindow1.Height )
               MenuItem "Set Value" ACTION oWindow1.Height := 500
            END POPUP
            POPUP "Title"
               MenuItem "Get Value" ACTION MsgInfo( oWindow1.Title )
               MenuItem "Set Value" ACTION oWindow1.Title := 'New Title'
            END POPUP
            POPUP "Visible"
               MenuItem "Get Value" ACTION IF( iswindowdefined(oSample1), MsgInfo( oSample1.Visible ), )
               MenuItem "Set Value" ACTION IF( iswindowdefined(oSample1), IF( oSample1.Visible, oSample1.Visible := .F., oSample1.Visible := .T.), )
            END POPUP
            POPUP "Cursor"
               MenuItem "Get Value" ACTION MsgInfo( oWindow1.Cursor)
               MenuItem "Set Value" ACTION ( oWindow1.Cursor := GetCurrentFolder() + '\finger.cur', SetWindowCursor( oWindow1.oButton1.Handle, oWindow1.Cursor ) )
            END POPUP
            POPUP "OnRelease"
               MenuItem "Get Value" ACTION Eval( oWindow1.OnRelease )
               MenuItem "Set Value" ACTION oWindow1.OnRelease := { || MsgInfo( "New Release Event Fired!" ) }
            END POPUP
            POPUP "MinButton"
               MenuItem "Get Value" ACTION MsgInfo( oWindow1.MinButton )
               MenuItem "Set Value" ACTION IF( oWindow1.MinButton, oWindow1.MinButton := .F., oWindow1.MinButton := .T. )
            END POPUP
            POPUP "MaxButton"
               MenuItem "Get Value" ACTION MsgInfo( oWindow1.MaxButton )
               MenuItem "Set Value" ACTION IF( oWindow1.MaxButton, oWindow1.MaxButton := .F., oWindow1.MaxButton := .T. )
            END POPUP
            POPUP "Sizable"
               MenuItem "Get Value" ACTION MsgInfo( oWindow1.Sizable )
               MenuItem "Set Value" ACTION IF( oWindow1.Sizable, oWindow1.Sizable := .F., oWindow1.Sizable := .T. )
            END POPUP
            POPUP "SysMenu"
               MenuItem "Get Value" ACTION MsgInfo( oWindow1.SysMenu )
               MenuItem "Set Value" ACTION IF( oWindow1.SysMenu, oWindow1.SysMenu := .F., oWindow1.SysMenu := .T. )
            END POPUP
            POPUP "TitleBar"
               MenuItem "Get Value" ACTION MsgInfo( oWindow1.TitleBar )
               MenuItem "Set Value" ACTION IF( oWindow1.TitleBar, oWindow1.TitleBar := .F., oWindow1.TitleBar := .T. )
            END POPUP
            POPUP "TopMost"
               MenuItem "Get Value" ACTION MsgInfo( oWindow1.TopMost )
               MenuItem "Set Value" ACTION IF( oWindow1.TopMost, oWindow1.TopMost := .F., oWindow1.TopMost := .T. )
            END POPUP
            POPUP "HelpButton"
               MenuItem "Get Value" ACTION MsgInfo( oWindow1.HelpButton )
               MenuItem "Set Value" ACTION IF( oWindow1.HelpButton, oWindow1.HelpButton := .F., oWindow1.HelpButton := .T. )
            END POPUP
            POPUP "BackColor"
               MenuItem "Get Value" ACTION MsgDebug( oWindow1.BackColor )
               MenuItem "Set Value" ACTION oWindow1.BackColor := {255,255,0}
            END POPUP
            POPUP "FocusedControl"
               MenuItem "Get Value" ACTION MsgInfo( ThisWindow.FocusedControl )
               MenuItem "Set Value" ACTION oWindow1.oButton5.SetFocus
            END POPUP
            POPUP "FocusedWindow"
               MenuItem "Get Value" ACTION ThisWindowName()
            END POPUP
         END POPUP

         DEFINE POPUP "Methods"
            MenuItem "Show"     ACTION IF( iswindowdefined(oSample1), oSample1.show(), )
            MenuItem "Hide"     ACTION IF( iswindowdefined(oSample1), oSample1.hide(), )
            MenuItem "Minimize" ACTION IF( iswindowdefined(oSample1), oSample1.minimize(), )
            MenuItem "Maximize" ACTION IF( iswindowdefined(oSample1), oSample1.maximize(), )
            MenuItem "Restore"  ACTION IF( iswindowdefined(oSample1), oSample1.restore(), )
            MenuItem "SetFocus" ACTION IF( iswindowdefined(oSample1), oSample1.setfocus(), )
            MenuItem "Release"  ACTION IF( iswindowdefined(oSample1), oSample1.release(), )
         END POPUP

         DEFINE POPUP "Functions"
            MenuItem "GetCurrentFolder()" ACTION MsgInfo( GetCurrentFolder() )
         END POPUP

      END MENU

      DEFINE BUTTON oButton1
         Row     40
         Col     10
         Width   100
         Caption "Click here"
         OnClick CreateNewWindow()
      END BUTTON

      DEFINE BUTTON oButton2
         Row     70
         Col     10
         Width   140
         Caption "Draw Something"
         OnClick drawnewrect()
      END BUTTON

      DEFINE BUTTON oButton3
         Row     100
         Col     10
         Width   160
         Caption "Release Shift+F8 Key"
         OnClick Releasekey()
      END BUTTON

      DEFINE BUTTON oButton4
         Row     100
         Col     170
         Width   120
         Caption "Store F3 Key"
         OnClick StoreKey()
      END BUTTON

      DEFINE TEXTBOX oText1
         Row     130
         Col     10
         Width   160
         Height  28
         Value   'Sample TextBox'
      END TEXTBOX

      DEFINE BUTTON oButton5
         Row     130
         Col     170
         Width   120
         Caption "This"
         OnClick oWindow1.oText1.Value := ThisWindow.Name
      END BUTTON


      DEFINE LABEL oLabel1
         Row     160
         Col     10
         Width   200
         Height  80
         Value   "Press F2 TO Find out of the focused control"+ CRLF +"F4 FOR implementation OF thiswindow property"
         Border  .T.
         Transparent .T.
      END LABEL

      DEFINE STATUSBAR
         StatusItem ' '
         StatusItem ' ' width 120
         StatusItem ' ' width 160
         StatusItem ' ' width 160
      END STATUSBAR

      On Key F2        ACTION MsgInfo( ThisWindow.FocusedControl )
      On Key Control+X ACTION MsgInfo( 'Control + X pressed' )
      On Key Shift+F8  ACTION MsgInfo( 'Shift+F8 Pressed' )
      On Key ESCAPE    ACTION oWindow1.Release()

   END WINDOW

   On Key F3            OF oWindow1 ACTION MsgInfo( 'F3 Pressed' )
   On Key F4            OF oWindow1 ACTION ThisWindowName()
   On Key Control+A     OF oWindow1 ACTION MsgInfo( 'Control + A pressed' )
   On Key Control+Prior OF oWindow1 ACTION MsgInfo( 'Control + Prior pressed' )
   On Key Alt+F3        OF oWindow1 ACTION MsgInfo( 'Alt + F3 pressed' )

   ACTIVATE WINDOW oWindow1

   RETURN NIL

/*----------------------------------------------------------------------*/

STATIC FUNCTION CreateNewWindow

   DEFINE WINDOW oSample1;
      Width  300;
      Height 300;
      Title  'Sample Window';
      CHILD;
      On Init      oSample1.center();
      On Release   MsgInfo( "Window Release event" );
      On MouseDrag MsgInfo( "MouseDrag Event" );
      On GotFocus  oSample1.StatusBar.Item(1) := "Window Got Focus!";
      On LostFocus oSample1.StatusBar.Item(1) := "Window Lost Focus!"

      DEFINE LABEL oLabel1
         Row     160
         Col     10
         Width   290
         Value   "Press F10 to show the all active windows"
      END LABEL

      DEFINE STATUSBAR
         StatusItem ''
      END STATUSBAR

   END WINDOW

   On Key F4 OF oSample1 ACTION ThisWindowName()
   On Key F10 OF oSample1 ACTION MsgDebug( HMG_GetForms() )

   ACTIVATE WINDOW oSample1

   RETURN NIL

/*----------------------------------------------------------------------*/

STATIC FUNCTION WindowResized

   oWindow1.StatusBar.Item(1) := "Width : "+hb_ntos( oWindow1.Width)
   oWindow1.StatusBar.Item(2) := "Height : "+hb_ntos( oWindow1.Height)
   oWindow1.StatusBar.Item(4) := ""

   RETURN NIL

/*----------------------------------------------------------------------*/

STATIC FUNCTION drawnewrect

   draw rectangle in window oWindow1 At 70, 350 TO 120,500 pencolor {255,0,0} penwidth 2 fillcolor {255,255,255}

   RETURN NIL

/*----------------------------------------------------------------------*/

STATIC FUNCTION Releasekey

   Release Key SHIFT+F8 OF oWindow1

   RETURN NIL

/*----------------------------------------------------------------------*/

STATIC FUNCTION StoreKey

   LOCAL bAction := NIL

   Store Key F3 OF oWindow1 TO bAction
   Eval(bAction)

   RETURN NIL

/*----------------------------------------------------------------------*/

STATIC FUNCTION ThisWindowName

   MsgInfo( ThisWindow.Name )

   RETURN NIL

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
