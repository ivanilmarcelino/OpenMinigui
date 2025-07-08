/*
 * MINIGUI - Harbour Win32 GUI library Demo
*/

#include "minigui.ch"
#include "tsbrowse.ch"

// -----------------------------------
FUNCTION Main()
// -----------------------------------
   LOCAL cFont := "Tahoma", nSize := 10
   LOCAL cAls := "TEST", cDbf
   LOCAL cFil := "test2.dbf"

   SET OOP ON

   SET AUTOADJUST ON NOBUTTONS

   SET FONT TO cFont, nSize

   DEFINE FONT Normal FONTNAME cFont SIZE nSize
   DEFINE FONT BOLD FONTNAME cFont SIZE nSize BOLD
   DEFINE FONT Italic FONTNAME cFont SIZE nSize ITALIC

   SET DEFAULT ICON TO GetStartupFolder() + "\demo.ico"

   cDbf := hb_DirBase() + cFil

   if ! File( cDbf ) ; CreateTable()
   ENDIF

   USE ( cDbf ) Alias ( cAls ) NEW

   DEFINE WINDOW sample AT 0, 0 WIDTH 640 HEIGHT 480 ;
         TITLE "Open Table via SBrowse" ;
         MAIN NOSHOW ;
         ON INIT _wPost( 0 ) ;
         ON RELEASE _wSend( 98 )
      This.Cargo := oHmgData()

      This.Cargo:cDbf := cDbf
      This.Cargo:cAls := cAls
      This.Cargo:aStru := ( cAls )->( dbStruct() )

      WITH OBJECT This.OBJECT
         :Event( 0, {| ow |
         LOCAL aData
         aData := my_Sbrowse( ow )
         _wSend( 1, ow, aData )
         _wSend( 99, ow )
         RETURN NIL
         } )
         :Event( 1, {| ow, ky, aArr |
         ky := ow:Cargo:cAls
         IF Select( ky ) > 0
            ( ky )->( dbZap() )
            ( ky )->( HMG_ArrayToDBF( aArr ) )
            ( ky )->( dbGoTop() )
         ENDIF
         RETURN NIL
         } )
         :Event( 98, {| ow | ( ow:Cargo:cAls )->( dbCloseArea() ) } )
         :Event( 99, {| ow | ow:Release() } )
      END WITH

   END WINDOW

   sample.Center()
   sample.Activate()

RETURN NIL

// -----------------------------------
FUNCTION my_SBrowse( ow )
// -----------------------------------
   LOCAL o := ow:Cargo
   LOCAL bSetup := {| obrw, laft | SetMyBrowser( obrw, laft ) }
   LOCAL cTitle := "Array Browse: Right Click For Record View"
   LOCAL cAls := o:cAls
   LOCAL aData := ( cAls )->( HMG_DbfToArray() )

   SBrowse( aData, cTitle, bSetup,, 950, 430,,, .T. )

RETURN aData

// -----------------------------------
FUNCTION CreateTable
// -----------------------------------

   dbCreate( "Test2", { { "CODE", "C", 3, 0 }, { "NAME", "C", 50, 0 }, { "RESIDENTS", "N", 11, 0 }, { "NOTES", "M", 10, 0 } },, .T. )

   dbAppend()
   REPLACE CODE WITH 'LTU', NAME WITH 'Lithuania', RESIDENTS WITH 3369600
   dbAppend()
   REPLACE CODE WITH 'USA', NAME WITH 'United States of America', RESIDENTS WITH 305397000
   dbAppend()
   REPLACE CODE WITH 'POR', NAME WITH 'Portugal', RESIDENTS WITH 10617600
   dbAppend()
   REPLACE CODE WITH 'POL', NAME WITH 'Poland', RESIDENTS WITH 38115967
   dbAppend()
   REPLACE CODE WITH 'AUS', NAME WITH 'Australia', RESIDENTS WITH 21446187
   dbAppend()
   REPLACE CODE WITH 'FRA', NAME WITH 'France', RESIDENTS WITH 64473140
   dbAppend()
   REPLACE CODE WITH 'RUS', NAME WITH 'Russia', RESIDENTS WITH 141900000
   USE

RETURN NIL

// -----------------------------------
FUNCTION SetMyBrowser( oBrw, lAft )
// -----------------------------------
   LOCAL cTitle, oCol, nCol
   LOCAL hFont := GetFontHandle( "Bold" )
   LOCAL ow := _WindowObj( oBrw:cParentWnd )
   LOCAL om := _WindowObj( _HMG_MainHandle )
   LOCAL o := om:Cargo // main cargo

   IF Empty( lAft )

      SetProperty( ow:Name, "MinWidth", 950 )
      SetProperty( ow:Name, "MinHeight", 430 )

      ON KEY CONTROL + W OF &( ow:Name ) ;
         ACTION {||
            LOCAL oBrw := This.oBrw.OBJECT
            IF oBrw:IsEdit
               oBrw:aColumns[ oBrw:nCell ]:oEdit:Save()
               oBrw:SetFocus()
            ENDIF
            RETURN NIL
         }

      WITH OBJECT oBrw
         :nHeightCell += 5
         :nHeightHead += 12
         :nClrFocuFore := CLR_BLACK
         :nClrFocuBack := COLOR_GRID

         :hFontSupHd := hFont
         :nHeightSuper := oBrw:nHeightHead
      END WITH

      cTitle := "Code"
      ADD SUPER HEADER TO oBrw FROM 1 TO 2 TITLE " Array"
      ADD SUPER HEADER TO oBrw FROM 3 TO oBrw:nColCount() TITLE cTitle

      FOR EACH oCol IN oBrw:aColumns
         nCol := hb_enumindex( oCol )
         oCol:hFontHead := hFont
         oCol:cName := o:aStru[ nCol, 1 ]
         oCol:cDataType := o:aStru[ nCol, 2 ]
         oCol:cField := oCol:cName
         oCol:nFieldLen := o:aStru[ nCol, 3 ]
         oCol:nFieldDec := o:aStru[ nCol, 4 ]
         oCol:cHeading := oCol:cName
         IF oCol:cName == "NOTES"
            oCol:nWidth += 200
         ELSEIF oCol:cName == "NAME"
            oCol:nWidth += 70
         ENDIF
      NEXT

   ELSE

      ow:Enabler( "Btn_1", .F. )
      ow:Enabler( "Btn_2", .F. )
      oCol := oBrw:GetColumn( "ARRAYNO" )
      oCol:hFont := GetFontHandle( "Italic" )
      oCol:hFontFoot := hFont
      oCol:cDataType := ValType( oBrw:GetValue( oCol ) )
      oCol := ATail( oBrw:aColumns )
      ATail( oBrw:aSuperHead )[ 2 ] := oBrw:nColumn( oCol:cName )
      IF oBrw:nLen > oBrw:nRowCount()
         oBrw:ResetVScroll( .T. )
      ENDIF
      oBrw:AdjColumns()
      oBrw:SetNoHoles()
      oBrw:SetFocus()

   ENDIF

RETURN .T. // editable browse (return .F. is readonly)
