/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2021 Sergej Kiselev <bilance@bilance.lv>
*/

#include "minigui.ch"
#include "tsbrowse.ch"
#include "dbinfo.ch"

STATIC cParent

// -----------------------------------
FUNCTION Main()
// -----------------------------------
   LOCAL cTitle := "Test Browse: Right Click For Record View", ;
      bSetup := {| oBrw, lAft | SetMyBrowser( oBrw, lAft ) }, ;
      cFont := "Tahoma", nSize := 10

   CreateTable()

   USE ( hb_dirBase() + "test.dbf" ) NEW

   SET AUTOADJUST ON NOBUTTONS

   SET FONT TO cFont, nSize

   DEFINE FONT Normal FONTNAME cFont SIZE nSize
   DEFINE FONT Bold FONTNAME cFont SIZE nSize BOLD

   SET DEFAULT ICON TO GetStartupFolder() + "\demo.ico"

   DEFINE WINDOW sample AT 0, 0 WIDTH 640 HEIGHT 480 ;
         TITLE "Open Table via SBrowse" ;
         MAIN NOSHOW ;
         ON INIT ( This.Timer_1.Enabled := .T., SBrowse( "Test", cTitle, bSetup,, 950, 430,,, .T. ) ) ;
         ON RELEASE ( dbCloseArea( "Test" ), hb_dbDrop( "Test" ) )

      DEFINE TIMER Timer_1 INTERVAL 1000 ACTION iif( Empty( CountChildWindows() ), ThisWindow.Release(), )

      This.Timer_1.Enabled := .F.

      DEFINE TIMER Timer_2 INTERVAL 250 ;
         ACTION iif( _IsControlDefined( "Btn_1", cParent ), ;
         ( SetProperty( cParent, "Btn_1", "Enabled", .F. ), ;
         SetProperty( cParent, "Btn_2", "Enabled", .F. ) ), ) ONCE

   END WINDOW

   sample.Center()
   sample.Activate()

RETURN NIL

// -----------------------------------
FUNCTION CreateTable
// -----------------------------------

   dbCreate( "Test", { { "CODE", "C", 3, 0 }, { "NAME", "C", 50, 0 }, { "RESIDENTS", "N", 11, 0 }, { "NOTES", "M", 10, 0 } },, .T. )

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
   LOCAL cFormName := oBrw:cParentWnd, cTitle, oCol, ;
      hFont := GetFontHandle( "Bold" )

   IF Empty( lAft )

      SetProperty( cFormName, "MinWidth", 950 )
      SetProperty( cFormName, "MinHeight", 430 )

      cParent := cFormName

      WITH OBJECT oBrw
         :nHeightCell += 5
         :nHeightHead += 12
         :nClrFocuFore := CLR_BLACK
         :nClrFocuBack := COLOR_GRID

         :hFontSupHd := hFont
         :nHeightSuper := oBrw:nHeightHead
      END WITH

      cTitle := " [" + oBrw:cAlias + "] " + dbInfo( DBI_FULLPATH )

      ADD SUPER HEADER TO oBrw FROM 1 TO 2 TITLE " " + rddName()
      ADD SUPER HEADER TO oBrw FROM 3 TO oBrw:nColCount() TITLE cTitle

      FOR EACH oCol IN oBrw:aColumns
         oCol:hFontHead := hFont
      NEXT

   ELSE

      oBrw:GetColumn( "ORDKEYNO" ):hFontFoot := hFont
      oCol := ATail( oBrw:aColumns )
      ATail( oBrw:aSuperHead )[ 2 ] := oBrw:nColumn( oCol:cName )
      IF oBrw:nLen > oBrw:nRowCount()
         oBrw:ResetVScroll( .T. )
      ENDIF
      oBrw:SetNoHoles()
      oBrw:SetFocus()

   ENDIF

RETURN .T. // editable browse (return .F. is readonly)

// -----------------------------------
FUNCTION CountChildWindows
// -----------------------------------
   LOCAL i, nFormCount := Len ( _HMG_aFormHandles ), nCnt := 0

   FOR i := 1 TO nFormCount
      IF _HMG_aFormType[ i ] <> "A"
         IF _IsWindowDefined ( _HMG_aFormNames[ i ] )
            nCnt++
         ENDIF
      ENDIF
   NEXT

RETURN nCnt
