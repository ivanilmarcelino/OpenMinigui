/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Program : Test01.prg
 * Purpose : TData class test showing multiple browses of same data file
 *           in MDI-child windows.
 * Note    : Each press of the button will open a new browse of the same file.
*/

#include "minigui.ch"

STATIC nChild := 0


FUNCTION Main()

   SET FONT TO "Arial", 10

   DEFINE WINDOW Form_1 ;
         CLIENTAREA 640, 480 ;
         TITLE 'Browse MDI demo' ;
         MAIN ;
         MDI

      DEFINE MAIN MENU

         POPUP 'File'
            ITEM 'Open' ACTION Customer()
            ITEM 'Close' ACTION CloseMdi()
            SEPARATOR
            ITEM 'Exit' ACTION Form_1.RELEASE
         END POPUP

         POPUP 'Child Windows'
            POPUP '&Tiled'
               ITEM '&Horizontal ' ACTION WinChildTile( .F. )
               ITEM '&Vertical ' ACTION WinChildTile( .T. )
            END POPUP
            ITEM '&Cascade' ACTION WinChildCascade()
            ITEM 'Arrange &Icons' ACTION WinChildIcons()
            SEPARATOR
            ITEM 'Close &All' ACTION WinChildCloseAll()
            ITEM '&Restore All' ACTION WinChildRestoreAll()
         END POPUP

      END MENU

      DEFINE IMAGELIST ImageList_1 ;
         BUTTONSIZE 24, 24 ;
         IMAGE { 'tb_24.bmp' } ;
         IMAGECOUNT 19

      DEFINE SPLITBOX

         DEFINE TOOLBAREX ToolBar_1 BUTTONSIZE 28, 28 IMAGELIST 'ImageList_1' FLAT

         BUTTON Btn_Open ;
            PICTUREINDEX 0 ;
            TOOLTIP 'Browse customer' ;
            ACTION Customer()

         BUTTON Btn_Close ;
            PICTUREINDEX 5 ;
            TOOLTIP 'Close Active Child Window' ;
            ACTION CloseMdi()

         END TOOLBAR

      END SPLITBOX

      Form_1.Btn_Close.Enabled := .F.

   END WINDOW

   CENTER WINDOW Form_1

   ACTIVATE WINDOW Form_1

RETURN NIL


PROCEDURE CloseMdi()

   CLOSE ACTIVE MDICHILD
   Form_1.Btn_Close.Enabled := nChild > 0

RETURN


// You can call this as many times as you want
// and you'll get a new browse window each time.
FUNCTION Customer()

   LOCAL a_fields, a_width, a_headers, i

   LOCAL oCust := tdata():new(, "cust" )

   IF oCust:use()

      if ! File( "cust2.ntx" )
         oCust:createIndex( "cust2",, "upper(company)" )
         oCust:closeIndex()
      ENDIF
      oCust:addIndex( "cust2" )
      oCust:goTop()

      a_fields := {}
      FOR i := 1 TO oCust:lastRec()
         AAdd( a_fields, { oCust:company, oCust:address1, oCust:city } )
         oCust:skip()
      NEXT
      a_headers := { "Customer", "Address", "City" }
      a_width := { 200, 250, 120 }

      nChild++

      DEFINE WINDOW ChildMdi ;
            TITLE "Child " + hb_ntos( nChild ) + ": " + oCust:cAlias ;
            MDICHILD ;
            ON INIT ( ResizeEdit() ) ;
            ON RELEASE ( oCust:close(), ReleaseMdiChild() ) ;
            ON SIZE ResizeEdit() ;
            ON MAXIMIZE ResizeEdit() ;
            ON MINIMIZE ResizeEdit() ;
            ON MOUSECLICK SetEditFocus()

         @ 0, 0 GRID BrwMdi ;
            WIDTH 200 ;
            HEIGHT 200 ;
            HEADERS a_headers ;
            WIDTHS a_width ;
            ITEMS a_fields

      END WINDOW

      Form_1.Btn_Close.Enabled := .T.

   ENDIF

RETURN NIL


PROCEDURE ResizeEdit()

   LOCAL ChildHandle, ChildName
   LOCAL i

   ChildHandle := GetActiveMdiHandle()
   i := AScan ( _HMG_aFormHandles, ChildHandle )
   IF i > 0
      ChildName := _HMG_aFormNames[ i ]
      ResizeChildEdit( ChildName )
   ENDIF

RETURN


PROCEDURE ResizeChildEdit( ChildName )

   LOCAL hwndCln, actpos := { 0, 0, 0, 0 }
   LOCAL i, w, h

   i := AScan ( _HMG_aFormNames, ChildName )
   IF i > 0
      hwndCln := _HMG_aFormHandles[ i ]
      GetClientRect( hwndCln, actpos )
      w := actpos[ 3 ] - actpos[ 1 ]
      h := actpos[ 4 ] - actpos[ 2 ]
      _SetControlHeight( "BrwMdi", ChildName, h )
      _SetControlWidth( "BrwMdi", ChildName, w )
   ENDIF

RETURN


PROCEDURE SetEditFocus()

   LOCAL ChildHandle, ChildName
   LOCAL i

   ChildHandle := GetActiveMdiHandle()
   i := AScan ( _HMG_aFormHandles, ChildHandle )
   IF i > 0
      ChildName := _HMG_aFormNames[ i ]
      _SetFocus ( "BrwMdi", ChildName )
      DoMethod ( ChildName, "BrwMdi", "Refresh" )
   ENDIF

RETURN


PROCEDURE ReleaseMdiChild()
   nChild--
   Form_1.Btn_Close.Enabled := nChild > 0

RETURN


PROCEDURE ResizeAllChildEdit()

   LOCAL nfrm, ChildName, n

   nFrm := Len( _HMG_aFormHandles )
   FOR n := 1 TO nFrm
      IF _HMG_aFormType[ n ] == 'Y'
         ChildName := _HMG_aFormNames[ n ]
         ResizeChildEdit( ChildName )
      ENDIF
   NEXT

RETURN


PROCEDURE WinChildTile( lVert )
   IF lVert
      TILE MDICHILDS VERTICAL
   ELSE
      TILE MDICHILDS HORIZONTAL
   ENDIF
   ResizeAllChildEdit()

RETURN


PROCEDURE WinChildCascade()
   CASCADE MDICHILDS
   ResizeAllChildEdit()

RETURN


PROCEDURE WinChildIcons()
   ARRANGE MDICHILD ICONS

RETURN


PROCEDURE WinChildCloseAll()
   CLOSE MDICHILDS ALL

RETURN


PROCEDURE WinChildRestoreAll()
   RESTORE MDICHILDS ALL

RETURN
