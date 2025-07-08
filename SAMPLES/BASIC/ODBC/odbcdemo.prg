 /*
 * MiniGUI ODBC Demo
 * Based upon code from:
 *  ODBCDEMO - ODBC Access Class Demonstration
 *  Felipe G. Coury <fcoury@flexsys-ci.com>
 * MiniGUI Version:
 *  Roberto Lopez
 *
 * Updated for HMG Extended Edition by MiniGUI Team
 */

#include "minigui.ch"

#xcommand WITH <oObject> DO => Self := <oObject>
#xcommand ENDWITH           => Self := NIL

MEMVAR Self

PROCEDURE Main

   LOCAL cConStr := "DBQ=" + GetStartupFolder() + "\bd1.mdb;" + ;
      "Driver={Microsoft Access Driver (*.mdb)}"
   LOCAL oo := TODBC():New( cConStr )
   LOCAL cFont := "Arial", nSize := 12

   SET FONT TO cFont, nSize
   // TsBrowse                          bold italic
   _DefineFont( "Normal", cFont, nSize, .F., .F. )
   _DefineFont( "Bold", cFont, nSize, .T., .F. )
   _DefineFont( "Italic", cFont, nSize - 2, .F., .T. )

   DEFINE WINDOW Form_1 AT 0, 0 WIDTH 400 HEIGHT 400 ;
         TITLE 'MiniGUI ODBC Demo' ;
         MAIN TOPMOST ;
         ON INIT {|| This.Topmost := .F. } ;
         ON RELEASE {|| This.Hide, oo:Destroy() }

      DEFINE MAIN MENU
         DEFINE POPUP 'File'
            MENUITEM 'Test' ACTION Test ( oo )
            MENUITEM '_TBrowse() Test' ACTION Test2( oo, "table1" )
            SEPARATOR
            MENUITEM 'Exit' ACTION Form_1.Release
         END POPUP
      END MENU

   END WINDOW

   ACTIVATE WINDOW Form_1

RETURN

FUNCTION TEST2( oo, cTbl )

   LOCAL cForm := "Form_2", oBrw, oTsb := oHmgData()
   LOCAL y := ThisWindow.ROW, ;
      x := ThisWindow.COL, ;
      w := ThisWindow.WIDTH, ;
      h := ThisWindow.HEIGHT, ;
      t := ThisWindow.TITLE + " _TBrowse(...)"

   oo:SetSQL( "SELECT * FROM " + cTbl )

   IF ! oo:Open()
      MsgStop( "SELECT * FROM " + cTbl + " not open !" )
      RETURN NIL
   ENDIF

   oTsb:uSelector := 20
   oTsb:aNumber := { 1, 30 }
   oTsb:aFoot := .T.
   oTsb:lZebra := .T.
   oTsb:cSuperHd := "Table name: " + cTbl
   oTsb:lSuperHd := .T.
   oTsb:aSuperHdColor := { CLR_YELLOW, CLR_BLUE }

   DEFINE WINDOW &cForm AT y, x WIDTH w HEIGHT h TITLE t ;
      CHILD
      This.Cargo := oHmgData()

      y := x := 0
      w := This.ClientWidth
      h := This.ClientHeight

      oBrw := _TBrowse( oTsb, oo, , y, x, w, h )

      oBrw:SetFocus()

   END WINDOW

   ACTIVATE WINDOW &cForm

   oo:Close()

RETURN NIL

FUNCTION TEST( dsFunctions )

   WITH dsFunctions DO

   ::SetSQL( "SELECT * FROM table1" )
   IF ::Open()

      // Put data in fields array
      ::LoadData( ::nRecNo )

      MsgInfo( ::FieldByName( "field1" ):Value )

      ::Skip()

      MsgInfo ( ::FieldByName( "field1" ):Value )

      ::GoTo( 1 )

      MsgInfo ( ::FieldByName( "field1" ):Value )

      ::Prior()

      MsgInfo ( ::FieldByName( "field1" ):Value )

      ::First()

      MsgInfo ( ::FieldByName( "field1" ):Value )

      ::Last()

      MsgInfo ( ::FieldByName( "field1" ):Value )

      ::Close()

   ENDIF

   ENDWITH

RETURN NIL
