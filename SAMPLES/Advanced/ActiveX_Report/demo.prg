/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2016 Grigory Filatov <gfilatov@inbox.ru>
 */

#include "minigui.ch"
#include "Selector.ch"

*------------------------------------------------------------------------------*
PROCEDURE Main()
/*
 *  Main procedure of the application.
 *  This procedure defines the main window of the application, sets its properties (size, title),
 *  and defines the main menu. It also registers the ActiveX control when the window is initialized
 *  and unregisters it when the window is released.
 */
   DEFINE WINDOW Form_1 ;
         AT 0, 0 ;
         WIDTH 334 ;
         HEIGHT 276 ;
         TITLE 'Catchysoft Report Test' ;
         MAIN ;
         ON INIT RegActiveX() ;
         ON RELEASE UnRegActiveX()

      DEFINE MAIN MENU

         DEFINE POPUP "Test"
            MENUITEM 'Report Test' ACTION REPORT()
            MENUITEM 'Report Test 2' ACTION REPORT2()
            SEPARATOR
            ITEM 'Exit' ACTION Form_1.Release()
         END POPUP

      END MENU

   END WINDOW

   Form_1.Center()
   Form_1.Activate()

RETURN

*------------------------------------------------------------------------------*
STATIC PROCEDURE REPORT()
/*
 *  Generates and displays a simple report using the CatchysoftReport ActiveX control.
 *  This procedure creates an instance of the ActiveX control, initializes the report,
 *  defines the columns, adds data rows, sets a summary, and displays the report in print preview.
 */
*------------------------------------------------------------------------------*
   LOCAL i, SUM
   LOCAL oreport := CreateObject( "CatchysoftReport.Report" )

   oreport:InitReport()

   oreport:ColumnName := "Column 1"
   oreport:ColumnName := "Column 2"
   oreport:ColumnName := "Column 3"
   oreport:ColumnWidth := 20
   oreport:ColumnWidth := 20
   oreport:ColumnWidth := 60

   SUM := 0
   FOR i := 1 TO 100
      oreport:FieldText := hb_ntos( i )
      oreport:FieldText := "test"
      oreport:FieldText := "10"
      SUM += 10
   NEXT

   oreport:Summary := ""
   oreport:Summary := "Sum"
   oreport:Summary := hb_ntos( Sum )

   oreport:ReportName := "Simple Report"
   oreport:PrintPreview()

   oreport := NIL

RETURN

*------------------------------------------------------------------------------*
STATIC PROCEDURE REPORT2()
/*
 *  Generates and displays a more complex report using the CatchysoftReport ActiveX control.
 *  This procedure reads data from a database table ("country"), groups it by continent,
 *  calculates totals for area and population for each continent, and displays the report.
 *  It also calculates and displays the overall totals for area and population.
 */
*------------------------------------------------------------------------------*
   LOCAL i, total1, total2, aContinent := {}
   LOCAL oreport := CreateObject( "CatchysoftReport.Report" )

   MEMVAR SelectList
   FIELD NAME, Capital, Area, Population, Continent

   USE ( hb_DirBase() + "country" ) EXCLUSIVE
   IF ! Used()
      MsgStop( "Can not connect to the table!", "Error" )
      RETURN
   ENDIF

   INDEX ON Continent TO temp2 UNIQUE

   dbEval( {|| AAdd( aContinent, Continent ) } )

   INDEX ON PadR( CONTINENT, 20 ) + PadR( NAME, 28 ) TO temp1

   oreport:InitReport()

   // column header
   oreport:ColumnName := "Name"
   oreport:ColumnName := "Capital"
   oreport:ColumnName := "Area"
   oreport:ColumnName := "Population"
   // column width (in percents)
   oreport:ColumnWidth := 30
   oreport:ColumnWidth := 30
   oreport:ColumnWidth := 20
   oreport:ColumnWidth := 20

   FOR i := 1 TO Len( aContinent )
      total1 := 0
      total2 := 0
      // output of Continent name
      oreport:FieldText := ""
      oreport:FieldText := Upper( aContinent[ i ] )
      oreport:FieldText := ""
      oreport:FieldText := ""

      oreport:FieldText := ""
      oreport:FieldText := Repl( "-", 30 )
      oreport:FieldText := ""
      oreport:FieldText := ""

      // select of data for a current continent
      @ SELECT NAME, Capital, Area, Population FOR Continent = aContinent[ i ] FROM Country ALIAS temp

      GO TOP
      DO WHILE ! Eof()
         oreport:FieldText := temp->NAME
         oreport:FieldText := temp->Capital
         oreport:FieldText := Transform( temp->Area, '999 999 999' )
         oreport:FieldText := Transform( temp->Population, '99 999 999 999' )
         total1 += temp->Area
         total2 += temp->Population
         SKIP
      ENDDO

      // output of totals
      oreport:FieldText := Repl( "-", 30 )
      oreport:FieldText := Repl( "-", 30 )
      oreport:FieldText := Repl( "-", 20 )
      oreport:FieldText := Repl( "-", 20 )

      oreport:FieldText := ""
      oreport:FieldText := ""
      oreport:FieldText := Transform( total1, '999 999 999' )
      oreport:FieldText := Transform( total2, '99 999 999 999' )

      dbCloseArea( "TEMP" ) // close a temporary table
   NEXT

   dbDrop( "TEMP" ) // erase a temporary table

   total1 := 0
   total2 := 0
   Country->( dbEval( {|| total1 += Area, total2 += Population } ) )
   Country->( dbCloseArea() )
   hb_FileDelete( "temp?.ntx" )

   // output of summary
   oreport:Summary := "Total:"
   oreport:Summary := ""
   oreport:Summary := Transform( total1, '999 999 999' )
   oreport:Summary := Transform( total2, '99 999 999 999' )

   // report header (limitation is 13 symbols)
   oreport:ReportName := "Countries Sum"
   oreport:PrintPreview()

   oreport := NIL

RETURN

*------------------------------------------------------------------------------*
PROCEDURE RegActiveX()
/*
 *  Registers the CatchysoftReport ActiveX control.
 *  This procedure checks if the CatchysoftReport.dll file exists in the startup folder.
 *  If the file exists, it executes the regsvr32 command to register the ActiveX control.
 *  This allows the application to use the ActiveX control. The /s parameter makes the registration silent.
 */
*------------------------------------------------------------------------------*

   IF File( GetStartUpFolder() + '\CatchysoftReport.dll' )
      EXECUTE FILE "regsvr32" PARAMETERS "/s CatchysoftReport.dll" HIDE
   ENDIF

RETURN

*------------------------------------------------------------------------------*
PROCEDURE UnRegActiveX()
/*
 *  Unregisters the CatchysoftReport ActiveX control.
 *  This procedure checks if the CatchysoftReport.dll file exists in the startup folder.
 *  If the file exists, it executes the regsvr32 command with the /u parameter to unregister the ActiveX control.
 *  This is typically done when the application is closed or uninstalled. The /s parameter makes the unregistration silent.
 */
*------------------------------------------------------------------------------*

   IF File( GetStartUpFolder() + '\CatchysoftReport.dll' )
      EXECUTE FILE "regsvr32" PARAMETERS "/u /s CatchysoftReport.dll" HIDE
   ENDIF

RETURN
