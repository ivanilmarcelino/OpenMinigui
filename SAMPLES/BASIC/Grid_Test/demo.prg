/*
 HMG Grid Demo
 (c) 2010 Roberto Lopez
 (c) 2011-2016 Grigory Filatov
*/

#include "minigui.ch"
#include "i_winuser.ch"

STATIC showheader := .T.
STATIC bColor
STATIC fColor

FUNCTION Main

   LOCAL aRows[20 ][7 ]

   bColor := {|| iif ( This.CellRowIndex / 2 == Int( This.CellRowIndex / 2 ), { 222, 222, 222 }, { 255, 255, 255 } ) }
   fColor := {|| iif ( This.CellRowIndex / 2 == Int( This.CellRowIndex / 2 ), { 255, 0, 0 }, { 0, 0, 255 } ) }

   SET DATE german
   SET CENTURY ON

   aRows[ 1] := { 'Simpson', 1500.00, CToD( '23/05/1989' ), .T., Date(), 12, 1 }
   aRows[ 2] := { 'Mulder', 2000.00, CToD( '12/06/1989' ), .F., Date(), 15, 2 }
   aRows[ 3] := { 'Smart', 340, Date(), .T., Date(), 13, 3 }
   aRows[ 4] := { 'Grillo', 323.60, Date(), .F., Date(), 1, 4 }
   aRows[ 5] := { 'Kirk', 120, Date(), .T., Date(), 2, 5 }
   aRows[ 6] := { 'Barriga', 0, Date(), .F., Date(), 45, 6 }
   aRows[ 7] := { 'Flanders', 0, Date(), .T., Date(), 35, 7 }
   aRows[ 8] := { 'Smith', 128, Date(), .F., Date(), 45, 8 }
   aRows[ 9] := { 'Pedemonti', 12.5, Date(), .T., Date(), 34, 9 }
   aRows[10] := { 'Gomez', -4, Date(), .F., Date(), 57, 10 }
   aRows[11] := { 'Simpson', 1500.00, CToD( '23/05/1989' ), .T., Date(), 12, 1 }
   aRows[12] := { 'Mulder', 2000.00, CToD( '12/06/1989' ), .F., Date(), 15, 2 }
   aRows[13] := { 'Smart', 340, Date(), .T., Date(), 13, 3 }
   aRows[14] := { 'Grillo', 323.60, Date(), .F., Date(), 1, 4 }
   aRows[15] := { 'Kirk', 120, Date(), .T., Date(), 2, 5 }
   aRows[16] := { 'Barriga', 0, Date(), .F., Date(), 45, 6 }
   aRows[17] := { 'Flanders', 0, Date(), .T., Date(), 35, 7 }
   aRows[18] := { 'Smith', 128, Date(), .F., Date(), 45, 8 }
   aRows[19] := { 'Pedemonti', 12.5, Date(), .T., Date(), 34, 9 }
   aRows[20] := { 'Gomez', -4, Date(), .F., Date(), 57, 10 }


   Define Window oWindow    ;
      At 10, 10    ;
      Width 650    ;
      Height 400    ;
      Title 'HMG Grid Demo'   ;
      Main

   Define Main MENU

   Define Popup "&Properties"
   MenuItem "Get Item (2,3)"                  action MsgInfo( oWindow.oGrid.Cell( 2, 3 ) )
   MenuItem "Set Item (2,2)"                  action oWindow.oGrid.Cell( 2, 2 ) := 1250.5
   MenuItem "Get Item (2,7)"                  action MsgInfo( oWindow.oGrid.Cell( 2, 7 ) )
   MenuItem "Set Item (2,7)"                  action oWindow.oGrid.Cell( 2, 7 ) := 8
   MenuItem "Get ItemCount"                   action MsgInfo( oWindow.oGrid.ItemCount )
   Separator
   MenuItem "Get Item (4)"                    action ShowItems( 4 )
   MenuItem "Set Item (4)"                    action AEval( aRows[9 ], {| x, i| oWindow.oGrid.Cell( 4, i ) := x } )
   MenuItem "Get Header (3)"                  action MsgInfo( oWindow.oGrid.Header( 3 ) )
   MenuItem "Set Header (3)"                  action oWindow.oGrid.Header( 3 ) := "New Header"
   Separator
   MenuItem "Show/Hide Headers"               action ( showheader := !showheader, LoadGrid( {}, isGridMultiSelect( 'oGrid', 'oWindow' ), isgridcelled( 'oGrid', 'oWindow' ), isgrideditable( 'oGrid', 'oWindow' ) ) )
   MenuItem "Toggle Grid Lines"               action setgridlines( 'oGrid', 'oWindow', !isgridlines( 'oGrid', 'oWindow' ) )
   MenuItem "Toggle MultiSelect"              action LoadGrid( {}, !isGridMultiSelect( 'oGrid', 'oWindow' ), isgridcelled( 'oGrid', 'oWindow' ), isgrideditable( 'oGrid', 'oWindow' ) )
   MenuItem "Toggle CellNavigation"           action LoadGrid( {}, isGridMultiSelect( 'oGrid', 'oWindow' ), !isgridcelled( 'oGrid', 'oWindow' ), isgrideditable( 'oGrid', 'oWindow' ) )
   MenuItem "Toggle AllowEdit"                action LoadGrid( {}, isGridMultiSelect( 'oGrid', 'oWindow' ), isgridcelled( 'oGrid', 'oWindow' ), !isgrideditable( 'oGrid', 'oWindow' ) )
   Separator
   MenuItem "Get HeaderDragDrop"              action msgdebug(oWindow.oGrid.HeaderDragDrop)
   MenuItem "Get InfoTip"                     action msgdebug(oWindow.oGrid.InfoTip)
   MenuItem "Set HeaderDragDrop Off"          action oWindow.oGrid.HeaderDragDrop := .f.
   MenuItem "Set InfoTip Off"                 action oWindow.oGrid.InfoTip := .f.
   Separator
   MenuItem "Get Value"                       action ShowGridValue()
   MenuItem "Set Value"                       action SetGridValue()
   Separator
   MenuItem "Get All Items List"              action ShowAllItems()
   End PopUp

   Define PopUp "&Events"
   MenuItem "Change OnChange Event"           action oWindow.oGrid.onChange := {|| MsgInfo( "OnChange event now changed!" ) }
   MenuItem "Change OnDblClick Event"         action iif( isgrideditable( 'oGrid', 'oWindow' ), nil, oWindow.oGrid.onDblClick := {|| MsgInfo( "OnDblClick event now changed!" ) } )
   End PopUp

   Define PopUp "&Methods"
   MenuItem "AddItem()"                       action AddNewRow()
   MenuItem "DeleteItem(3)"                   action oWindow.oGrid.DeleteItem( 3 )
   MenuItem "DeleteAllItems()"                action oWindow.oGrid.DeleteAllItems()
   Separator
   MenuItem "AddColumn(2)"                    action AddNewColumn( 'oGrid', 'oWindow', 2 )
   MenuItem "DeleteColumn(2)"                 action DeleteColumn( 'oGrid', 'oWindow', 2 )
   Separator
   MenuItem "AddColumn(8)"                    action AddNewColumn( 'oGrid', 'oWindow', 8 )
   End PopUp

   End Menu

   End Window

   _HMG_GridSelectedCellBackColor := { 122, 163, 204 }
   _HMG_GridSelectedRowBackColor := { 193, 224, 255 }

   LoadGrid( aRows, .F., .T., .T. )

   oWindow.Center()
   oWindow.Activate()

RETURN NIL


FUNCTION LoadGrid( aRows, lmultiselect, lcelled, leditable )

   LOCAL aItems, i

   default lcelled := .T.

   IF iscontroldefined( oGrid, oWindow )
      aItems := {}
      IF oWindow.oGrid.ItemCount > 0
         FOR i := 1 TO oWindow.oGrid.ItemCount
            AAdd( aItems, oWindow.oGrid.Item( i ) )
         NEXT i
      ENDIF
      aRows := aItems
      oWindow.oGrid.release
      DO events
      IF lmultiselect
         lcelled := .F.
      ENDIF
   ENDIF

   Define Grid oGrid
      Row  20
      Col  10
      Width  615
      Height  300
      Parent  oWindow
      Widths  { 150, 60, 70, 40, 90, 40, 100 }
      Headers  { 'Column 1', 'Column 2', 'Column 3', 'Column 4', 'Column 5', 'Column 6', 'Column 7' }
      Items  aRows
      Value  IF ( lmultiselect, { 1 }, if ( lcelled, { 1, 1 }, 1 ) )
      AllowEdit leditable
      CellNavigation lcelled
      MultiSelect lmultiselect
      Justify  { 0, 1, 0, 0, 0, 1, 0 }
      ColumnControls { { 'TEXTBOX', 'CHARACTER' }, ;
                       { 'TEXTBOX', 'NUMERIC', '9,999.99' }, ;
                       { 'TEXTBOX', 'DATE' }, ;
                       { 'CHECKBOX', 'Yes', 'No' }, ;
                       { "DATEPICKER", "UPDOWN" }, { "SPINNER", 1, 1000 }, ;
                       { "COMBOBOX", { "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December" } };
                     }
      ColumnWhen { {|| .F. }, ;
                   {|| .T. }, ;
                   {|| .T. }, ;
                   {|| .T. }, ;
                   {|| .T. }, ;
                   {|| .T. }, ;
                   {|| .T. };
                 }
      ColumnValid { {|| .T. }, ;
                    {|| .T. }, ;
                    {|| .T. }, ;
                    {|| .T. }, ;
                    {|| .T. }, ;
                    {|| .T. }, ;
                    {|| MsgYesNo( 'Is this valid ?', 'Confirm' ) };
                  }
      OnDblClick MsgInfo( "Double Click event!" )
      DynamicBackColor { bColor, bColor, bColor, bColor, bColor, bColor, bColor }
      DynamicForeColor { fColor, fColor, fColor, fColor, fColor, fColor, fColor }
      HeaderImages { 'help.bmp', 'help.bmp', 'help.bmp', 'help.bmp', 'help.bmp', 'help.bmp', 'help.bmp' }
      OnHeadClick { {|| MsgInfo( "Header1 Clicked!" ) }, {|| MsgInfo( "Header2 Clicked!" ) }, {|| MsgInfo( "Header3 Clicked!" ) }, {|| MsgInfo( "Header4 Clicked!" ) }, {|| MsgInfo( "Header5 Clicked!" ) }, {|| MsgInfo( "Header6 Clicked!" ) }, {|| MsgInfo( "Header7 Clicked!" ) } }
      ShowHeaders showheader
   End Grid

   IF showheader
      oWindow.oGrid.ColumnsAutoFitH
   ENDIF
   oWindow.oGrid.Setfocus

RETURN NIL


FUNCTION ShowItems( nItem )

   LOCAL cStr := '', i
   LOCAL aLine := oWindow.oGrid.Item( nItem )

   FOR i := 1 TO Len( aLine )
      cStr += ' ' + cValToChar( aLine[ i ] )
   NEXT i
   MsgInfo( cStr )

RETURN NIL


FUNCTION ShowAllItems

   LOCAL i, j
   LOCAL cStr, aLine, aStr := {}

   FOR i := 1 TO oWindow.oGrid.ItemCount
      cStr := ''
      aLine := oWindow.oGrid.Item( i )
      FOR j := 1 TO Len( aLine )
         cStr += ' ' + cValToChar( aLine[ j ] )
      NEXT j
      AAdd( aStr, cStr )
   NEXT i
   MsgDebug( aStr )

RETURN NIL


FUNCTION ShowGridValue

   LOCAL cStr := '', i
   LOCAL aValue := oWindow.oGrid.Value

   IF isGridMultiSelect( 'oGrid', 'oWindow' )
      cStr := "Selected Lines are : ("
      FOR i := 1 TO Len( aValue )
         cStr += AllTrim( Str( aValue[ i ] ) )
         IF i < Len( aValue )
            cStr += ","
         ENDIF
      NEXT i
      cStr += ")"
   ELSE
      IF isgridcelled( 'oGrid', 'oWindow' )
         cStr := "Value is : (" + AllTrim( Str( aValue[ 1 ] ) ) + "," + AllTrim( Str( aValue[ 2 ] ) ) + ")"
      ELSE
         cStr := "Value is :" + Str( oWindow.oGrid.Value )
      ENDIF
   ENDIF
   MsgInfo( cStr )

RETURN NIL


FUNCTION SetGridValue

   IF isGridMultiSelect( 'oGrid', 'oWindow' )
      oWindow.oGrid.Value := { 1, 3, 5, 7 }
   ELSE
      IF isgridcelled( 'oGrid', 'oWindow' )
         oWindow.oGrid.Value := { 1, 2 }
      ELSE
         oWindow.oGrid.Value := 5
      ENDIF
   ENDIF

RETURN NIL


FUNCTION SetLastValue

   IF isGridMultiSelect( 'oGrid', 'oWindow' )
      oWindow.oGrid.Value := { ( oWindow.oGrid.ItemCount ) }
   ELSE
      IF isgridcelled( 'oGrid', 'oWindow' )
         oWindow.oGrid.Value := { ( oWindow.oGrid.ItemCount ), 1 }
      ELSE
         oWindow.oGrid.Value := ( oWindow.oGrid.ItemCount )
      ENDIF
   ENDIF

RETURN NIL


FUNCTION AddNewRow

   LOCAL i := getcontrolindex( "oGrid", "oWindow" )
   LOCAL adbc, n, aEditcontrols
   LOCAL bColor2 := {|val, rowindex| if( Empty( val[ 1 ] ), { 193, 224, 255 }, ;
      if( RowIndex / 2 == Int( RowIndex / 2 ), { 222, 222, 222 }, { 255, 255, 255 } ) ) }
   LOCAL aValue := {}

   adbc := _HMG_aControlMiscData1[ i ][ 12 ]
   AEval( adbc, {| val, nColumn| adbc[ nColumn ] := bColor2, val := nil } )
   _HMG_aControlMiscData1[ i ][ 12 ] := adbc
   aEditcontrols := _HMG_aControlMiscData1[ i ][ 13 ]
   FOR n := 1 TO Len( aEditcontrols )
      AAdd( aValue, CtrlToData( aEditcontrols[ n ] ) )
   NEXT n

   oWindow.oGrid.AddItem( aValue )
   SetLastValue()

RETURN NIL


STATIC FUNCTION CtrlToData( aValue )

   DO CASE
   CASE 'TEXTBOX' $ aValue[ 1 ]
      IF 'CHARACTER' $ aValue[ 2 ]
         RETURN ""
      ELSEIF 'NUMERIC' $ aValue[ 2 ]
         RETURN 0
      ELSEIF 'DATE' $ aValue[ 2 ]
         RETURN CToD( "" )
      ENDIF
   CASE 'DATE' $ aValue[ 1 ]
      RETURN CToD( "" )
   CASE 'CHECKBOX' $ aValue[ 1 ]
      RETURN .F.
   CASE 'SPINNER' $ aValue[ 1 ] .OR. 'COMBOBOX' $ aValue[ 1 ]
      RETURN 0
   ENDCASE

RETURN ""


FUNCTION isgridmultiselect( control, form )

RETURN ( "MULTI" $ getcontroltype( control, form ) )


FUNCTION isgrideditable( control, form )

RETURN getproperty( form, control, "EditAble" )


FUNCTION isgridcelled( control, form )

RETURN getproperty( form, control, "CellNavigation" )


FUNCTION isgridlines( control, form )

   LOCAL i := getcontrolindex( control, form )

RETURN _HMG_aControlMiscData1[ i ][ 7 ]


FUNCTION setgridlines( control, form, nogrid )

   LOCAL i := getcontrolindex( control, form )
   LOCAL ControlHandle := getcontrolhandle( control, form )

   SendMessage( ControlHandle, LVM_SETEXTENDEDLISTVIEWSTYLE, 0, iif( nogrid, 0, 1 ) + LVS_EX_FULLROWSELECT )
   _HMG_aControlMiscData1[ i ][ 7 ] := nogrid

RETURN NIL


FUNCTION AddNewColumn( control, form, nColumn )

   LOCAL i := getcontrolindex( control, form ), aRow, Value
   LOCAL aItems := {}, n, adbc, adfc, aEditcontrols
   LOCAL bColor2 := {|| { 193, 224, 255 } }

   IF GetProperty( form, control, "ItemCount" ) > 0
      Value := GetProperty( form, control, "Value" )
      FOR n := 1 TO GetProperty( form, control, "ItemCount" )
         AAdd( aItems, GetProperty( form, control, "Item", n ) )
      NEXT n
      adfc := _HMG_aControlMiscData1[ i ][ 11 ]
      AIns( adfc, nColumn, fColor, .T. )
      _HMG_aControlMiscData1[ i ][ 11 ] := adfc
      adbc := _HMG_aControlMiscData1[i ][ 12 ]
      AIns( adbc, nColumn, bColor2, .T. )
      _HMG_aControlMiscData1[ i ][ 12 ] := adbc
      aEditcontrols := _HMG_aControlMiscData1[ i ][13 ]
      AIns( aEditcontrols, nColumn, { 'TEXTBOX', 'CHARACTER' }, .T. )
      _HMG_aControlMiscData1[ i ][ 13 ] := aEditcontrols
   ENDIF

   _AddGridColumn( control, form, nColumn, 'New Column', 100, 1 )

   IF Len( aItems ) > 0
      SetProperty( form, control, "Value", 0 )
      Domethod( form, control, "DisableUpdate" )
      FOR i := 1 TO Len( aItems )
         aRow := aItems[ i ]
         AIns( aRow, nColumn, "", .T. )
         Domethod( form, control, "AddItem", aRow )
      NEXT i
      Domethod( form, control, "EnableUpdate" )
      SetProperty( form, control, "Value", Value )
   ENDIF

RETURN NIL


FUNCTION DeleteColumn( control, form, nColumn )

   LOCAL i := getcontrolindex( control, form ), aRow, Value
   LOCAL aItems := {}, n, aEditcontrols, adbc

   IF GetProperty( form, control, "ItemCount" ) > 0
      Value := GetProperty( form, control, "Value" )
      FOR n := 1 TO GetProperty( form, control, "ItemCount" )
         AAdd( aItems, GetProperty( form, control, "Item", n ) )
      NEXT n
      adbc := _HMG_aControlMiscData1[ i ][ 12 ]
      ADel( adbc, nColumn, .T. )
      _HMG_aControlMiscData1[ i ][ 12 ] := adbc
      aEditcontrols := _HMG_aControlMiscData1[ i ][ 13 ]
      ADel( aEditcontrols, nColumn, .T. )
      _HMG_aControlMiscData1[ i ][ 13 ] := aEditcontrols
   ENDIF

   Domethod( form, control, "DeleteColumn", nColumn )

   IF Len( aItems ) > 0
      SetProperty( form, control, "Value", 0 )
      Domethod( form, control, "DisableUpdate" )
      FOR i := 1 TO Len( aItems )
         aRow := aItems[ i ]
         ADel( aRow, nColumn, .T. )
         Domethod( form, control, "AddItem", aRow )
      NEXT i
      Domethod( form, control, "EnableUpdate" )
      SetProperty( form, control, "Value", Value )
   ENDIF

RETURN NIL
