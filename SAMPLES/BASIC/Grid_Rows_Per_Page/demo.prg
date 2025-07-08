/*
 * HMG - Harbour Win32 GUI library Demo
 *
 * Copyright 2014 Dr. Claudio Soto <srvet@adinet.com.uy>
 */

#include "hmg.ch"

FUNCTION Main()

   LOCAL aRows := { ;
      { 113.12, Date() + 1, 1, 1, .T. }, ;
      { 123.12, Date() + 2, 2, 2, .F. }, ;
      { 133.12, Date() + 3, 3, 3, .T. }, ;
      { 143.12, Date() + 4, 1, 4, .F. }, ;
      { 153.12, Date() + 5, 2, 5, .T. }, ;
      { 163.12, Date() + 6, 3, 6, .F. }, ;
      { 173.12, Date() + 7, 1, 7, .T. }, ;
      { 183.12, Date() + 8, 2, 8, .F. }, ;
      { 193.12, Date() + 9, 3, 9, .T. }, ;
      { 203.12, Date() + 10, 1, 10, .F. }, ;
      { 113.12, Date() + 11, 2, 11, .T. }, ;
      { 123.12, Date() + 12, 3, 12, .F. }, ;
      { 133.12, Date() + 13, 1, 13, .T. }, ;
      { 143.12, Date() + 14, 2, 14, .F. }, ;
      { 153.12, Date() + 15, 3, 15, .T. }, ;
      { 163.12, Date() + 16, 1, 16, .F. }, ;
      { 173.12, Date() + 17, 2, 17, .T. }, ;
      { 183.12, Date() + 18, 3, 18, .F. }, ;
      { 193.12, Date() + 19, 1, 19, .T. }, ;
      { 203.12, Date() + 20, 2, 20, .F. }, ;
      { 113.12, Date() + 21, 2, 21, .T. }, ;
      { 123.12, Date() + 22, 3, 22, .F. }, ;
      { 133.12, Date() + 23, 1, 23, .T. }, ;
      { 143.12, Date() + 24, 2, 24, .F. }, ;
      { 153.12, Date() + 25, 3, 25, .T. }, ;
      { 163.12, Date() + 26, 1, 26, .F. }, ;
      { 173.12, Date() + 27, 2, 27, .T. }, ;
      { 183.12, Date() + 28, 3, 28, .F. }, ;
      { 193.12, Date() + 29, 1, 29, .T. }, ;
      { 203.12, Date() + 30, 2, 30, .F. }, ;
      { 113.12, Date() + 31, 2, 31, .T. }, ;
      { 123.12, Date() + 32, 3, 32, .F. }, ;
      { 133.12, Date() + 33, 1, 33, .T. }, ;
      { 143.12, Date() + 34, 2, 34, .F. }, ;
      { 153.12, Date() + 35, 3, 35, .T. }, ;
      { 163.12, Date() + 36, 1, 36, .F. }, ;
      { 173.12, Date() + 37, 2, 37, .T. }, ;
      { 183.12, Date() + 38, 3, 38, .F. }, ;
      { 193.12, Date() + 39, 1, 39, .T. }, ;
      { 203.12, Date() + 40, 2, 40, .F. } }

   DEFINE WINDOW Form_1 ;
         AT 0, 0 ;
         WIDTH Min( 1000, GetDesktopWidth() ) ;
         HEIGHT Min( 1000, GetDesktopHeight() - GetTaskBarHeight() ) ;
         TITLE 'Resize Grid Test' ;
         MAIN

      DEFINE MAIN MENU
         DEFINE POPUP 'File'
            MENUITEM 'Get Width' ACTION MsgInfo ( 'Width:  ' + hb_ntos ( Form_1.Grid_1.Width ) )
            MENUITEM 'Get Height' ACTION MsgInfo ( 'Height:  ' + hb_ntos ( Form_1.Grid_1.Height ) )
            MENUITEM 'Get Item Count' ACTION MsgInfo ( 'Item Count:  ' + hb_ntos ( Form_1.Grid_1.ItemCount ) )
            MENUITEM 'Get Rows Per Page' ACTION MsgInfo ( 'Rows Per Page:  ' + hb_ntos ( Form_1.Grid_1.RowsPerPage ) )
            MENUITEM 'Get Column Count' ACTION MsgInfo ( 'Column Count:  ' + hb_ntos ( Form_1.Grid_1.ColumnCount ) )
            SEPARATOR
            MENUITEM 'Set Width' ACTION ( Form_1.Grid_1.WIDTH := Val( InputBox( 'Enter new Width', , hb_ntos ( Form_1.Grid_1.Width ) ) ), AutoFit( 1 ) )
            MENUITEM 'Set Height' ACTION ( Form_1.Grid_1.HEIGHT := Val( InputBox( 'Enter new Height', , hb_ntos ( Form_1.Grid_1.Height ) ) ), AutoFit( 2 ) )
            MENUITEM 'AutoFit' ACTION ( Form_1.AutoFit.Checked := ! Form_1.AutoFit.Checked ) NAME AutoFit CHECKED
            SEPARATOR
            MENUITEM 'Exit' ACTION Form_1.Release()
         END POPUP
      END MENU

      @ 10, 10 GRID Grid_1 ;
         WIDTH 620 ;
         HEIGHT 330 ;
         HEADERS { 'Column 1', 'Column 2', 'Column 3', 'Column 4', 'Column 5' } ;
         WIDTHS { 140, 140, 140, 140, 140 } ;
         ITEMS aRows ;
         EDIT ;
         COLUMNCONTROLS { ;
         { 'TEXTBOX', 'NUMERIC', '$ 999,999.99' }, ;
         { 'DATEPICKER', 'DROPDOWN' }, ;
         { 'COMBOBOX', { 'One', 'Two', 'Three' } }, ;
         { 'SPINNER', 1, 40 }, ;
         { 'CHECKBOX', 'Yes', 'No' } ;
         }

   END WINDOW

   CENTER WINDOW Form_1

   ACTIVATE WINDOW Form_1

RETURN NIL


PROCEDURE AutoFit( n )

   LOCAL nW, nH

   IF Form_1.AutoFit.Checked

      hmg_SysWait( .01 )

      ListView_CalculateSize( Form_1.Grid_1.Handle, Min( Form_1.Grid_1.ItemCount, Form_1.Grid_1.RowsPerPage ), @nW, @nH )

      IF n == 1
         SetProperty( 'Form_1', 'Grid_1', 'Width', nW )
      ELSE
         SetProperty( 'Form_1', 'Grid_1', 'Height', nH )
      ENDIF

   ENDIF

RETURN
