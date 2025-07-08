/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Program : Test04.prg
 * Purpose : Test Customer class
 * Notes   : This shows how to build add, edit, delete and browse right
             into the customer object. The browse assumes it will be in a
             MDI child window.

             You can open a customer browse just by attaching the following
             to a button:

                TCustomer():new():browse()

             Add, edit, delete, and browse are all user interface methods.
*/

#include "minigui.ch"
#include "hbclass.ch"

STATIC hSplitHandle

// --- For testing only
FUNCTION Main()

   LOCAL oDB

   FIELD custno, company

   oDB := tdata():new(, "cust",, .F. )
   IF oDB:use()
      oDB:createIndex( "cust",, "custno",,, .F. )
      oDB:createIndex( "cust2",, "company",,, .F. )
      oDB:goTop()
      WHILE oDB:recNo() <= oDB:lastRec()
         oDB:recall()
         oDB:skip()
      END
   ENDIF
   oDB:close()

   SET EPOCH TO 1980
   SET DELETED ON

   SET FONT TO _GetSysFont(), 12

   DEFINE WINDOW Form_1 ;
         CLIENTAREA 640, 480 ;
         TITLE 'Test TCustomer class' ;
         MAIN ;
         MDI ;
         ON MAXIMIZE ResizeSplitedTB( hSplitHandle ) ;
         ON SIZE ResizeSplitedTB( hSplitHandle )

      DEFINE IMAGELIST ImageList_1 ;
         OF Form_1 ;
         BUTTONSIZE 24, 24 ;
         IMAGE { 'tb_24.bmp' } ;
         IMAGECOUNT 19

      DEFINE SPLITBOX

         DEFINE TOOLBAREX ToolBar_1 BUTTONSIZE 32, 32 IMAGELIST 'ImageList_1' FLAT

            BUTTON Btn_Open ;
               PICTUREINDEX 0 ;
               TOOLTIP 'Browse customer' ;
               ACTION TCustomer():new():Browse()

            BUTTON Btn_Exit ;
               PICTUREINDEX 5 ;
               TOOLTIP 'Exit' ;
               ACTION ReleaseAllWindows()
         END TOOLBAR

      END SPLITBOX

   END WINDOW

   CENTER WINDOW Form_1

   ACTIVATE WINDOW Form_1

RETURN NIL


PROCEDURE ResizeSplitedTB( hSpl )

   IF ISNUMERIC( hSpl )
      _SetWindowSizePos ( hSpl, , , This.Width )
   ENDIF

RETURN
// --- End test


// --- Customer class
CLASS TCustomer FROM TData

   METHOD new
   METHOD browse
   METHOD add
   METHOD edit
   MESSAGE delete METHOD _delete

   HIDDEN:
   DATA lAdd AS LOGICAL

ENDCLASS


METHOD new()

   ::super:new(, "cust" )

   IF ::use()
      ::SetArea( Select() )
      ::addIndex( "cust" )
      ::addIndex( "cust2" )
      ::setOrder( 1 )
      ::gotop()
   ENDIF

   ::lAdd := .F.

RETURN self


METHOD browse()

   LOCAL a_fields, a_width, a_headers, i

   ::setOrder( 2 )
   ::gotop()

   a_fields := {}
   a_width := {}
   FOR i := 1 TO ::FCount()
      AAdd( a_fields, ::FieldName( i ) )
      AAdd( a_width, iif( ::FieldType( i ) == 'C', 205, 100 ) )
   NEXT
   a_headers := a_fields

   Form_1.Btn_Open.Enabled := .F.

   SET BROWSESYNC ON

   DEFINE WINDOW ChildMdi ;
         TITLE "Customer" ;
         MDICHILD ;
         ON INIT ResizeEdit() ;
         ON RELEASE ::end() ;
         ON SIZE ResizeEdit() ;
         ON MAXIMIZE ResizeEdit() ;
         ON MINIMIZE ResizeEdit()

      WinChildMaximize()

      DEFINE IMAGELIST ImageList_1 ;
         BUTTONSIZE 24, 24 ;
         IMAGE { 'tb_24.bmp' } ;
         IMAGECOUNT 19

      DEFINE SPLITBOX HANDLE hSplitHandle

         DEFINE TOOLBAREX ToolBar_1 BUTTONSIZE 28, 28 IMAGELIST 'ImageList_1' FLAT

            BUTTON Btn_New ;
               PICTUREINDEX 0 ;
               TOOLTIP 'Add' ;
               ACTION ( ::add(), SetEditFocus( ::RecNo() ) )

            BUTTON Btn_New ;
               PICTUREINDEX 17 ;
               TOOLTIP 'Edit' ;
               ACTION ( ::edit(), SetEditFocus( ::RecNo() ) )

            BUTTON Btn_Delete ;
               PICTUREINDEX 5 ;
               TOOLTIP 'Delete' ;
               ACTION ( ::delete(), ::skip( -1 ), iif( ::Bof(),, ::skip() ), SetEditFocus( ::RecNo() ) )

         END TOOLBAR

      END SPLITBOX

      @ 40, 0 BROWSE BrwMdi ;
         WIDTH 200 ;
         HEIGHT 200 ;
         HEADERS a_headers ;
         WIDTHS a_width ;
         FIELDS a_fields ;
         WORKAREA &( ::cAlias ) ;
         ON DBLCLICK ( ::edit(), SetEditFocus( ::RecNo() ) )

   END WINDOW

RETURN self


METHOD add()

   ::lAdd := .T.
   ::blank() // start with a blank record
   ::edit()
   ::load() // in case they cancel
   ::lAdd := .F.

RETURN self


METHOD edit()

   LOCAL mNumLine := 0

   iif( ::lAdd, , ::load() )

   DEFINE WINDOW Edit AT 0, 0 WIDTH 360 HEIGHT 240 ;
         TITLE iif( ::lAdd, "Untitled", ::company ) ;
         MODAL ;
         FONT 'Tahoma' SIZE 9

      @ mNumLine += 15, 10 LABEL LABEL_1 VALUE 'Custno' WIDTH 80 HEIGHT 20 RIGHTALIGN FONTCOLOR NAVY VCENTERALIGN
      @ mNumLine += 0, 100 TEXTBOX TEXTBOX_1 VALUE ::aBuffer[ 1 ] WIDTH 100 HEIGHT 20 ON LOSTFOCUS ::aBuffer[ 1 ] := Edit.TEXTBOX_1.VALUE FONTCOLOR BLUE BACKCOLOR SILVER
      @ mNumLine += 25, 10 LABEL LABEL_2 VALUE 'Company' WIDTH 80 HEIGHT 20 RIGHTALIGN FONTCOLOR NAVY VCENTERALIGN
      @ mNumLine += 0, 100 TEXTBOX TEXTBOX_2 VALUE ::aBuffer[ 2 ] WIDTH 200 HEIGHT 20 ON LOSTFOCUS ::aBuffer[ 2 ] := Edit.TEXTBOX_2.VALUE FONTCOLOR BLUE BACKCOLOR SILVER
      @ mNumLine += 25, 10 LABEL LABEL_3 VALUE 'Address1' WIDTH 80 HEIGHT 20 RIGHTALIGN FONTCOLOR NAVY VCENTERALIGN
      @ mNumLine += 0, 100 TEXTBOX TEXTBOX_3 VALUE ::aBuffer[ 3 ] WIDTH 200 HEIGHT 20 ON LOSTFOCUS ::aBuffer[ 3 ] := Edit.TEXTBOX_3.VALUE FONTCOLOR BLUE BACKCOLOR SILVER
      @ mNumLine += 25, 10 LABEL LABEL_4 VALUE 'City' WIDTH 80 HEIGHT 20 RIGHTALIGN FONTCOLOR NAVY VCENTERALIGN
      @ mNumLine += 0, 100 TEXTBOX TEXTBOX_4 VALUE ::aBuffer[ 4 ] WIDTH 160 HEIGHT 20 ON LOSTFOCUS ::aBuffer[ 4 ] := Edit.TEXTBOX_4.VALUE FONTCOLOR BLUE BACKCOLOR SILVER
      @ mNumLine += 25, 10 LABEL LABEL_5 VALUE 'State' WIDTH 80 HEIGHT 20 RIGHTALIGN FONTCOLOR NAVY VCENTERALIGN
      @ mNumLine += 0, 100 TEXTBOX TEXTBOX_5 VALUE ::aBuffer[ 5 ] WIDTH 60 HEIGHT 20 ON LOSTFOCUS ::aBuffer[ 5 ] := Edit.TEXTBOX_5.VALUE FONTCOLOR BLUE BACKCOLOR SILVER
      @ mNumLine += 25, 10 LABEL LABEL_6 VALUE 'Zip' WIDTH 80 HEIGHT 20 RIGHTALIGN FONTCOLOR NAVY VCENTERALIGN
      @ mNumLine += 0, 100 TEXTBOX TEXTBOX_6 VALUE ::aBuffer[ 6 ] WIDTH 120 HEIGHT 20 ON LOSTFOCUS ::aBuffer[ 6 ] := Edit.TEXTBOX_6.VALUE FONTCOLOR BLUE BACKCOLOR SILVER
      @ mNumLine += 30, 170 BUTTONEX BUTTONEX_1 CAPTION 'OK' WIDTH 80 HEIGHT 24 ACTION ( iif( ::lAdd, ::append(), ), ::save(), ThisWindow.Release() )
      @ mNumLine += 0, 250 BUTTONEX BUTTONEX_2 CAPTION 'Cancel' WIDTH 80 HEIGHT 24 ACTION ThisWindow.Release()

      ON KEY ESCAPE ACTION ThisWindow.Release()
   END WINDOW
   Edit.Center()
   Edit.Activate()

RETURN self


METHOD _delete()

   IF msgYesNo( "Delete this record?", "Confirmation" )
      ::super:delete()
   ENDIF

RETURN self


#define WM_MDIMAXIMIZE 0x0225

FUNCTION WinChildMaximize()

   LOCAL ChildHandle

   ChildHandle := GetActiveMdiHandle()
   IF AScan ( _HMG_aFormHandles, ChildHandle ) > 0
      SendMessage( _HMG_MainClientMDIHandle, WM_MDIMAXIMIZE, ChildHandle, 0 )
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
      h := actpos[ 4 ] - actpos[ 2 ] - 40
      _SetControlHeight( "BrwMdi", ChildName, h )
      _SetControlWidth( "BrwMdi", ChildName, w )
   ENDIF

RETURN


PROCEDURE SetEditFocus( value )

   LOCAL ChildHandle, ChildName
   LOCAL i

   ChildHandle := GetActiveMdiHandle()
   i := AScan ( _HMG_aFormHandles, ChildHandle )
   IF i > 0
      ChildName := _HMG_aFormNames[ i ]
      _SetValue ( "BrwMdi", ChildName, value )
      _SetFocus ( "BrwMdi", ChildName )
      DoMethod ( ChildName, "BrwMdi", "Refresh" )
   ENDIF

RETURN
