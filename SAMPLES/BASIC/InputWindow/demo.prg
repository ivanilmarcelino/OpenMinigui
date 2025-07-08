/*
 * MINIGUI - Harbour Win32 GUI library Demo
 */

#include "minigui.ch" 

FUNCTION Main()

   DEFINE WINDOW Frm1 ;
      WIDTH 300 HEIGHT 300 ;
      TITLE "InputWindow Function" ;
      MAIN

      @  20, 20 LABEL lbl_0 VALUE "Concept:"
      @  50, 20 LABEL lbl_1 VALUE "Motive:"
      @  83, 20 LABEL lbl_2 VALUE "Date:"
      @ 113, 20 LABEL lbl_3 VALUE "Amount:"
      @ 143, 20 LABEL lbl_4 VALUE "Register:"
      @ 173, 20 LABEL lbl_5 VALUE "Type:"

      @  20, 120 LABEL lbl_Concept ;
         VALUE "ADVANCE ON" ;
         AUTOSIZE
      @  50, 120 LABEL lbl_Motive ;
         VALUE "BILL PAYMENT N° 1122" ;
         AUTOSIZE
      @  80, 120 DATEPICKER dtp_Date ;
         WIDTH 110 ;
         NOTABSTOP
      @ 110, 120 TEXTBOX txt_Amount ;
         VALUE 1234 ;
         HEIGHT 24 ;
         WIDTH 110 ;
         READONLY ;
         NUMERIC ;
         INPUTMASK "999,999.99" FORMAT 'E' ;
         NOTABSTOP
      @ 140, 120 CHECKBOX chk_Register ;
         CAPTION "" ;
         WIDTH 110 ;
         HEIGHT 24 ;
         VALUE .F. ;
         NOTABSTOP
      @ 170, 120 COMBOBOX cmb_Type ;
         ITEMS { "Collect", "Payment" } ;
         WIDTH 110 ;
         HEIGHT 60 ;
         VALUE 1 ;
         NOTABSTOP

      @ 220, 90 BUTTON btn_Capture ;
         CAPTION "Capture" ;
         ACTION Capture() ;
         WIDTH 120 ;
         HEIGHT 28

      Frm1.dtp_Date.Enabled     := .f.
      Frm1.chk_Register.Enabled := .f.
      Frm1.cmb_Type.Enabled     := .f.

      ON KEY ESCAPE ACTION Frm1.Release
   END WINDOW

   CENTER WINDOW Frm1
   ACTIVATE WINDOW Frm1

RETURN NIL


FUNCTION Capture()

/*
 * InputWindow function parameters:
 *
 * ( cTitle, ;
 *   aLabels, ;
 *   aValues, ;
 *   aFormats, ;
 *   nRow, ;
 *   nCol, ;
 *   lCenterWindow, ;
 *   aButOKCancelCaptions, ;
 *   nLabelWidth, ;
 *   nControlWidth )
 *
 * cTitle
 *    String.
 *    Window's title.
 * aLabels
 *    Array of strings.
 *    Labels to show.
 *    The number of items determines the number of capture fields.
 * aInitValues
 *    Array.
 *    Initial values for the capture fields.
 *    The type of each item determines the type of control to use for the
 *    capture. Valid types are logical, date, numeric, string and memo.
 *    The corresponding controls are checkbox, datepicker, combobox or
 *    editbox (see next parameter), textbox or editbox (see next parameter),
 *    and editbox.
 * aFormats
 *    Array.
 *    Aditional data for the capture. They depend on the type of the items in
 *    aInitValues (see previous parameter):
 *    Logical and Memo: NIL.
 *    Date: "SHOWNONE" to allow blank dates, NIL otherwise.
 *    Numeric: array or string. If it's an array, a combobox control is used
 *       for capture and the array content is used as items in the combobox.
 *       If it's a string (or memo) a numeric textbox is used for capture.
 *       If the string contains a decimal point, it's content is used as mask;
 *       if not, the string length is used as the maximum length to capture.
 *    String: numeric. If the value is less than or equal to 32 a textbox is
 *       used for the capture, if is greater an editbox control is used.
 *       The value is used as the maximun length to capture.
 * nRow y nCol
 *   Numeric.
 *   Window's position.
 *   A non numeric value means the window will be centered in the screen.
 * aButOKCancelCaptions
 *   An array with two items of character type.
 *   Text for the window's buttons.
 *   Defaults to {'Ok','Cancel'} or their respective translations according
 *   to the SET LANGUAGE specified.
 * nLabelWidth
 *   Numeric.
 *   Width of the labels. Defaults to 90.
 * nControlWidth
 *   Numeric.
 *   Width of the controls. Defaults to 140.
 */

   cTitle      := "Enter Data"
   aLabels     := { "Concept:", ;
                    "Motive:", ;
                    "Date:", ;
                    "Amount:", ;
                    "Register:", ;
                    "Type:" }
   aInitValues := { Frm1.lbl_Concept.Value, ;
                    Frm1.lbl_Motive.Value, ;
                    Frm1.dtp_Date.Value, ;
                    Frm1.txt_Amount.Value, ;
                    Frm1.chk_Register.Value, ;
                    Frm1.cmb_Type.Value }
   aFormats    := { 40, ;
                    20, ;
                    NIL, ;
                    "999,999.99", ;
                    NIL, ;
                    { "Collect", "Payment" } }

   aResults    := InputWindow( cTitle, aLabels, aInitValues, aFormats )

   IF aResults[1] != NIL
      Frm1.lbl_Concept.Value  := aResults[1]
      Frm1.lbl_Motive.Value   := aResults[2]
      Frm1.dtp_Date.Value     := aResults[3]
      Frm1.txt_Amount.Value   := aResults[4]
      Frm1.chk_Register.Value := aResults[5]
      Frm1.cmb_Type.Value     := aResults[6]
   ENDIF

RETURN NIL
