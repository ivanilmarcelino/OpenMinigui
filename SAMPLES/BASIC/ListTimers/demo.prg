#include "minigui.ch"

STATIC nTickCount := 0 // Counter to show timer is working

/*
 * FUNCTION Main()
 *
 * Initializes the main application window and defines its controls.
 *
 * Purpose:
 *   This is the entry point of the application. It creates the main window,
 *   defines the buttons, labels, and edit box, and sets up their initial
 *   properties and event handlers. The window is then centered and activated.
 *   This function sets up the user interface for testing timer functionality.
 *
 * Notes:
 *   The window definition includes buttons to start, stop, and list timers,
 *   a label to display the tick count, and an edit box to display timer information.
 */
FUNCTION Main()

   DEFINE WINDOW Form_1 ;
         AT 0, 0 ;
         WIDTH 420 HEIGHT 340 ;
         TITLE "HMG_LISTTIMERS() Function Test" ;
         MAIN ;
         ON RELEASE OnReleaseResources()

      DEFINE STATUSBAR
         STATUSITEM "Ready"
      END STATUSBAR

      @ 20, 20 BUTTON btnStart ;
         CAPTION "Start Timer" ;
         WIDTH 100 HEIGHT 28 ;
         ACTION StartTimer()

      @ 20, 140 BUTTON btnStop ;
         CAPTION "Stop Timer" ;
         WIDTH 100 HEIGHT 28 ;
         ACTION StopTimer()

      @ 60, 20 BUTTON btnList ;
         CAPTION "List Timers" ;
         WIDTH 100 HEIGHT 28 ;
         ACTION ListTimers()

      @ 60, 140 BUTTON btnReset ;
         CAPTION "Reset Count" ;
         WIDTH 100 HEIGHT 28 ;
         ACTION ResetCounter()

      @ 100, 20 LABEL lblCounter ;
         VALUE "Tick Count: 0" ;
         WIDTH 300 HEIGHT 24

      @ 140, 20 EDITBOX edtOutput ;
         WIDTH 370 HEIGHT 140 ;
         NOHSCROLL ;
         READONLY

   END WINDOW

   CENTER WINDOW Form_1
   ACTIVATE WINDOW Form_1

RETURN NIL

/*
 * FUNCTION OnReleaseResources()
 *
 * Releases the timer control when the form is closed.
 *
 * Purpose:
 *   This function is called when the main form is closed (ON RELEASE event).
 *   It checks if the Timer_1 control is defined on the form. If it is, it releases
 *   the timer's resources to prevent memory leaks or other issues.
 *
 * Notes:
 *   Releasing controls when they are no longer needed is good practice to ensure
 *   efficient resource management.
 */
FUNCTION OnReleaseResources
   IF IsControlDefined( Timer_1, Form_1 )
      Form_1.Timer_1.RELEASE
   ENDIF

RETURN NIL

/*
 * PROCEDURE OnTimer()
 *
 * Updates the tick count label on the main form.
 *
 * Purpose:
 *   This procedure is called by the timer at a regular interval (defined in StartTimer()).
 *   It increments the global tick counter (nTickCount) and updates the value of the
 *   lblCounter label on the main form to display the current tick count. This provides
 *   a visual indication that the timer is running.
 *
 * Notes:
 *   The nTickCount variable is a global variable that is incremented each time the timer fires.
 */
PROCEDURE OnTimer()
   nTickCount++
   Form_1.lblCounter.VALUE := "Tick Count: " + LTrim( Str( nTickCount ) )

RETURN

/*
 * PROCEDURE StartTimer()
 *
 * Starts or creates a timer that calls the OnTimer() procedure.
 *
 * Purpose:
 *   This procedure is called when the "Start Timer" button is clicked. It checks if a timer
 *   named "Timer_1" already exists on the form. If it does, it enables the timer. If it
 *   doesn't exist, it creates a new timer with an interval of 1000 milliseconds (1 second)
 *   and sets its action to call the OnTimer() procedure. The status bar is updated to indicate
 *   that the timer has been started.
 *
 * Notes:
 *   The IsControlDefined() function is used to check if the timer control already exists.
 */
PROCEDURE StartTimer()

   IF IsControlDefined( Timer_1, Form_1 )
      IF Form_1.Timer_1.Enabled
         Form_1.StatusBar.Item( 1 ) := "Timer already running"
         RETURN
      ELSE
         Form_1.Timer_1.Enabled := .T.
      ENDIF
   ELSE
      DEFINE TIMER Timer_1
         PARENT Form_1
         INTERVAL 1000
         ACTION OnTimer()
      END TIMER
   ENDIF

   Form_1.StatusBar.Item( 1 ) := "Timer started"

RETURN

/*
 * PROCEDURE StopTimer()
 *
 * Stops the timer if it is running.
 *
 * Purpose:
 *   This procedure is called when the "Stop Timer" button is clicked. It disables the timer
 *   by setting its Enabled property to .F.. The status bar is updated to indicate whether
 *   the timer was stopped or if there was no timer running.
 *
 * Notes:
 *   The Enabled property of the timer control determines whether the timer is running or not.
 */
PROCEDURE StopTimer()

   IF IsControlDefined( Timer_1, Form_1 ) .AND. Form_1.Timer_1.Enabled
      Form_1.Timer_1.Enabled := .F.
      Form_1.StatusBar.Item( 1 ) := "Timer stopped"
   ELSE
      Form_1.StatusBar.Item( 1 ) := "No timer to stop"
   ENDIF

RETURN

/*
 * FUNCTION ListTimers()
 *
 * Displays a list of currently active timers in the edit box on the main form.
 *
 * Purpose:
 *   This function is called when the "List Timers" button is clicked. It retrieves a list of
 *   currently active timers using the HMG_LISTTIMERS() function. If there are no active timers,
 *   it displays a message indicating that. Otherwise, it iterates through the list of timers
 *   and formats the timer information (window handle, timer ID, and interval) into a string.
 *   This string is then displayed in the edtOutput edit box on the main form. The status bar
 *   is updated to show the number of timers listed.
 *
 * Notes:
 *   The HMG_LISTTIMERS() function returns an array of active timers. Each element in the array
 *   is itself an array containing the timer's window handle, timer ID, and interval.
 */
FUNCTION ListTimers()

   LOCAL aTimers := HMG_LISTTIMERS()
   LOCAL cOutput := ""
   LOCAL i

   IF Len( aTimers ) == 0
      cOutput := "No active timers."
   ELSE
      FOR i := 1 TO Len( aTimers )
         cOutput += "Timer #" + LTrim( Str( i ) ) + CRLF
         cOutput += "  hWnd     : " + LTrim( Str( aTimers[ i ][ 1 ] ) ) + CRLF
         cOutput += "  Timer ID : " + LTrim( Str( aTimers[ i ][ 2 ] ) ) + CRLF
         cOutput += "  Interval : " + LTrim( Str( aTimers[ i ][ 3 ] ) ) + " ms" + CRLF + CRLF
      NEXT
   ENDIF

   Form_1.edtOutput.VALUE := cOutput
   Form_1.StatusBar.Item( 1 ) := "Listed " + LTrim( Str( Len( aTimers ) ) ) + " timer(s)"

RETURN NIL

/*
 * PROCEDURE ResetCounter()
 *
 * Resets the tick counter to zero.
 *
 * Purpose:
 *   This procedure is called when the "Reset Count" button is clicked. It resets the global
 *   tick counter (nTickCount) to zero and updates the lblCounter label on the main form to
 *   reflect the reset value. The status bar is also updated to indicate that the counter
 *   has been reset.
 *
 * Notes:
 *   The nTickCount variable is a global variable that is incremented by the timer.
 */
PROCEDURE ResetCounter()
   nTickCount := 0
   Form_1.lblCounter.VALUE := "Tick Count: 0"
   Form_1.StatusBar.Item( 1 ) := "Counter reset"

RETURN
