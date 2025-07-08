/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2002 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
*/

#include "minigui.ch"

Function Main

   Local hSplitWnd

   SET EVENTS FUNCTION TO App_OnEvents

   DEFINE WINDOW Form_1 ;
      AT 0,0 ;
      WIDTH 640 HEIGHT 480 ;
      TITLE 'MiniGUI SplitBox Demo' ;
      MAIN ;
      ON PAINT SplitBox_Resize (hSplitWnd)

      DEFINE STATUSBAR
         STATUSITEM 'HMG Power Ready - Resize SplitBox And Enjoy !' 
      END STATUSBAR

      DEFINE MAIN MENU 
         POPUP '&Help'
            ITEM 'About'      ACTION MsgInfo ("MiniGUI SplitBox Demo","A COOL Feature ;)") 
         END POPUP
      END MENU

      DEFINE SPLITBOX HANDLE hSplitWnd

         DEFINE TOOLBAR ToolBar_1 BUTTONSIZE 95,30 FLAT RIGHTTEXT CAPTION 'ToolBar 1'

		BUTTON Button_1 ;
			CAPTION '&Undo' ;
			PICTURE 'button4.bmp' ;
			ACTION MsgInfo('UnDo Click!') 

		BUTTON Button_2 ;
			CAPTION '&Save' ;
			PICTURE 'button5.bmp' ;
			ACTION MsgInfo('Save Click!') 
	
		BUTTON Button_3 ;
			CAPTION '&Close' ;
			PICTURE 'button6.bmp' ;
			ACTION MsgInfo('Close Click!') 

		BUTTON Button_10 ;
			CAPTION '&Login' ;
			PICTURE 'button14.bmp' ;
			ACTION MsgInfo('Login Click!') 

         END TOOLBAR

         DEFINE WINDOW SplitChild_1 ; 
            WIDTH 200 ;
            HEIGHT 200 ;
            VIRTUAL WIDTH 800 ;
            VIRTUAL HEIGHT 800 ;
            SPLITCHILD NOCAPTION

            DEFINE LABEL Label_1
               ROW   55
               COL   30
               VALUE 'Label !!!' 
               WIDTH 100 
               HEIGHT 27 
            END LABEL

            DEFINE CHECKBOX Check_1
               ROW   80
               COL   30
               CAPTION 'Check 1' 
               VALUE .T. 
               TOOLTIP 'CheckBox' 
            END CHECKBOX
         
            DEFINE SLIDER Slider_1
               ROW 115
               COL 30
               RANGEMIN 1
               RANGEMAX 10 
               VALUE 5 
               TOOLTIP 'Slider' 
            END SLIDER
            
            DEFINE FRAME Frame_1
               ROW   45
               COL   170
               WIDTH 85
               HEIGHT 110
            END FRAME

            DEFINE RADIOGROUP Radio_1
               ROW   50
               COL   180
               OPTIONS { 'One' , 'Two' , 'Three', 'Four' } 
               VALUE 1 
               WIDTH 70 
               TOOLTIP 'RadioGroup' 
            END RADIOGROUP

         END WINDOW 

      END SPLITBOX

   END WINDOW

   CENTER WINDOW Form_1

   ACTIVATE WINDOW Form_1

Return Nil

*---------------------------------------------*
PROCEDURE SplitBox_Resize (SplitBoxHandle)
*---------------------------------------------*
LOCAL Height    := Form_1.ClientHeight - Form_1.Statusbar.Height
LOCAL DifHeight := Height - REBAR_GETHEIGHT (SplitBoxHandle)
LOCAL aRect     := REBAR_GETBANDINFO (SplitBoxHandle, 1)  // SplitChild_1
LOCAL NewHeight := aRect [2] + DifHeight

   REBAR_SETMINCHILDSIZE (SplitBoxHandle, 1, NewHeight)   // SplitChild_1

RETURN

#include "i_winuser.ch"
*---------------------------------------------*
FUNCTION App_OnEvents( hWnd, nMsg, wParam, lParam )
*---------------------------------------------*
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
