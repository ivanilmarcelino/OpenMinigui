/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Sample was contributed to HMG forum by KDJ
 *
 * Adapted for MiniGUI Extended Edition by Grigory Filatov
 * Add Picture Columns  by Jan Szczepanik  2020.05.04
*/

#include 'minigui.ch'

// -----------------------------------------------------------------------------

FUNCTION Main()

   LOCAL lInit := .F.
   LOCAL cTitle := 'Browse PICTURE {NIL,NIL,NIL,NIL}'
   LOCAL aAction := {}, i

   SET FONT TO 'MS Shell Dlg', 8
   SET DATE TO GERMAN
   SET CENTURY ON

   OpenDB()

   DEFINE WINDOW MainWA ;
         AT 60, 60 ;
         MAIN ;
         WIDTH 460 ;
         HEIGHT 330 ;
         TITLE cTitle ;
         NOMAXIMIZE NOSIZE

      @ 10, 10 BROWSE CompGR ;
         WIDTH 440 ;
         HEIGHT 180 ;
         HEADERS { 'Name', 'Code', 'Price', 'Date' } ;
         WIDTHS { 120, 100, 115, 80 } ;
         WORKAREA COMP ;
         FIELDS { 'Name', 'Code', 'Price', 'Date' } ;
         ON GOTFOCUS iif( lInit, , ( HMG_SetOrder( 1, .F. ), lInit := .T. ) ) ;
         JUSTIFY { BROWSE_JTFY_LEFT, BROWSE_JTFY_CENTER, BROWSE_JTFY_RIGHT, BROWSE_JTFY_CENTER } ;
         COLUMNSORT { /*.T., .F., .T.*/ }
      // ok PICTURE     {}
      // ok PICTURE     {  , , , }
      // ok PICTURE     { NIL ,NIL , NIL, NIL }
      // ok PICTURE     { NIL ,"@R X.X.X" , "@Z 999,999.99", "@D"  }
      // ok PICTURE     {  ,"@R X.X.X" , "@Z 999,999.99",  }

      AAdd( aAction, [ _BrowseHome( "CompGR", "MainWA" ) ] )
      AAdd( aAction, [ _BrowseUp( "CompGR", "MainWA" ) ] )
      AAdd( aAction, [ _BrowseDown( "CompGR", "MainWA" ) ] )
      AAdd( aAction, [ _BrowseEnd( "CompGR", "MainWA" ) ] )

      FOR i := 1 TO Len( aAction )
         aAction[ i ] := aAction[ i ] + [, DoMethod("MainWA","CompGR","SetFocus")]
      NEXT

      CreateToolBar( 200, 10, { "db_top1", "db_back1", "db_next1", "db_end1" }, ;
         { "hbprint_top", "hbprint_back", "hbprint_next", "hbprint_end" }, 75, 25, 10, ;
         { aAction[ 1 ], aAction[ 2 ], aAction[ 3 ], aAction[ 4 ] }, ;
         { "First", "Previous", "Next", "End" } )

      DEFINE BUTTON ExitBU
         ROW 250
         COL 140
         WIDTH 80
         HEIGHT 25
         CAPTION 'E&xit'
         ACTION MainWA.Release()
      END BUTTON

      DEFINE MAINMENU
         DEFINE POPUP 'Set order'
            MENUITEM 'Name - ascending' ACTION SetOrder( 1, .F. )
            MENUITEM 'Name - descending' ACTION SetOrder( 1, .T. )
            SEPARATOR
            MENUITEM 'Code - ascending' ACTION SetOrder( 2, .F. )
            MENUITEM 'Code - descending' ACTION SetOrder( 2, .T. )
            SEPARATOR
            MENUITEM 'Price - ascending' ACTION SetOrder( 3, .F. )
            MENUITEM 'Price - descending' ACTION SetOrder( 3, .T. )
         END POPUP
      END MENU

   END WINDOW

   MainWA.CompGR.Value := RecNo()

   // MainWA.Center
   MainWA.Activate()

RETURN NIL

// -----------------------------------------------------------------------------

FUNCTION CreateToolBar( nRow, nCol, aButtonName, aButtonPicture, nWidth, nHeight, nGap, aButtonAction, aToolTip )

   LOCAL cButtonName, cButtonPicture, i, cAction, cToolTip
   LOCAL cParentName := ThisWindow.Name

   FOR i := 1 TO Len( aButtonName )

      cButtonName := aButtonName[ i ]
      cButtonPicture := aButtonPicture[ i ]
      cToolTip := aToolTip[ i ]

      @ nRow, nCol BUTTONEX ( cButtonName ) CAPTION "" ;
         PICTURE cButtonPicture WIDTH nWidth HEIGHT nHeight TOOLTIP cToolTip

      IF aButtonAction <> NIL .AND. aButtonAction[ i ] <> NIL
         cAction := aButtonAction[ i ]
         SetProperty( cParentName, cButtonName, "Action", hb_macroBlock( cAction ) )
      ELSE
         SetProperty( cParentName, cButtonName, "Action", hb_macroBlock( "AlertInfo( '" + cButtonName + " pressed' )" ) )
      ENDIF

      nCol += GetWindowWidth( GetControlHandle( cButtonName, cParentName ) ) + nGap

   NEXT

RETURN NIL

// -----------------------------------------------------------------------------

FUNCTION SetOrder( nColumn, lDescend )

   LOCAL nOrder := ordNumber( ordSetFocus() )
   LOCAL nRecord := MainWA.CompGR.Value

   ListView_SetSortHeader( MainWA.CompGR.Handle, nOrder, 0, IsAppXPThemed() )

   IF ValType( lDescend ) != 'L'
      lDescend := iif( nOrder == nColumn, ! ordDescend( nOrder ), .F. )
   ENDIF

   nOrder := nColumn

   ListView_SetSortHeader( MainWA.CompGR.Handle, nColumn, iif( lDescend, -1, 1 ), IsAppXPThemed() )

   ordSetFocus( nOrder )
   ordDescend( nOrder, NIL, lDescend )

   MainWA.CompGR.Value := nRecord
   MainWA.CompGR.Refresh

RETURN NIL

// -----------------------------------------------------------------------------

FUNCTION OpenDB()

   LOCAL cDbf := 'comp1.dbf'
   LOCAL n := 1

   IF File( cDbf )
      dbUseArea( NIL, NIL, cDbf, "Comp", .T. )
   ELSE
      dbCreate( cDbf, { { 'Name', 'C', 30, 0 }, { 'Code', 'C', 3, 0 }, { 'Price', 'N', 10, 2 }, { 'Date', 'D', 8, 0 } } ) // app waga
      dbUseArea( NIL, NIL, cDbf, "Comp", .T. )

      dbAppend()
      Comp->Name := 'Main board'
      Comp->Code := '002'
      Comp->Price := 1120.34
      Comp->Date := Date() - n++
      dbAppend()
      Comp->Name := 'Processor'
      Comp->Code := '004'
      Comp->Price := 0
      Comp->Date := Date() - n++
      dbAppend()
      Comp->Name := 'RAM'
      Comp->Code := '006'
      Comp->Price := 2204.58
      Comp->Date := Date() - n++
      dbAppend()
      Comp->Name := 'HDD'
      Comp->Code := '008'
      Comp->Price := 142.71
      Comp->Date := Date() - n++
      dbAppend()
      Comp->Name := 'SSD'
      Comp->Code := '010'
      Comp->Price := 316.94
      Comp->Date := Date() - n++
      dbAppend()
      Comp->Name := 'Graphics card'
      Comp->Code := '012'
      Comp->Price := 143.48
      Comp->Date := Date() - n++
      dbAppend()
      Comp->Name := 'Power supply'
      Comp->Code := '014'
      Comp->Price := 3054.29
      Comp->Date := Date() - n++
      dbAppend()
      Comp->Name := 'PC case'
      Comp->Code := '013'
      Comp->Price := 72.85
      Comp->Date := Date() - n++
      dbAppend()
      Comp->Name := 'Pendrive'
      Comp->Code := '011'
      Comp->Price := 12.78
      Comp->Date := Date() - n++
      dbAppend()
      Comp->Name := 'Monitor'
      Comp->Code := '009'
      Comp->Price := 315.61
      Comp->Date := Date() - n++
      dbAppend()
      Comp->Name := 'Keyboard'
      Comp->Code := '007'
      Comp->Price := 16.92
      Comp->Date := Date() - n++
      dbAppend()
      Comp->Name := 'Mouse'
      Comp->Code := '005'
      Comp->Price := 9.84
      Comp->Date := Date() - n++
      dbAppend()
      Comp->Name := 'Modem'
      Comp->Code := '003'
      Comp->Price := 31.45
      Comp->Date := Date() - n++
      dbAppend()
      Comp->Name := 'Speakers'
      Comp->Code := '001'
      Comp->Price := 43.59
      Comp->Date := Date() - n++
   ENDIF

RETURN NIL
