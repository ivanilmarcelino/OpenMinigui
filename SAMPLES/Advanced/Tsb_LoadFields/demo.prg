#include "minigui.ch"
#include "tsbrowse.ch"
#include "Dbstruct.ch"

REQUEST DBFCDX

/*
 * FUNCTION Main()
 *
 * Entry point of the application.
 * Initializes the environment, ensures the DBF file exists,
 * opens it, and displays its contents in a TBrowse control
 * within a GUI window.
 *
 * Purpose:
 *   - Verifies the presence of the required DBF file (creates it if missing).
 *   - Sets global environment and database options.
 *   - Launches a GUI with a data grid powered by TBrowse.
 */

FUNCTION Main()

   LOCAL cDbf := GetStartupFolder() + "\Test.dbf"
   LOCAL cAlias

   // Set global date and deletion settings
   SET CENTURY ON
   SET DELETED ON

   // Set the default Replaceable Database Driver (RDD)
   rddSetDefault( "DBFCDX" )

   // Create DBF file if it does not exist
   IF ! File( cDbf )
      CreateTable()
   ENDIF

   // Extracts alias from filename (without extension)
   cAlias := cFileNoExt( cDbf )

   // Open DBF file with specified alias
   USE ( cDbf ) Alias ( cAlias ) SHARED NEW
   Test->( ordSetFocus( 1 ) )

   // Define default font used in the GUI
   DEFINE FONT FontNorm FONTNAME _HMG_DefaultFontName SIZE _HMG_DefaultFontSize

   // Define main window of the application
   DEFINE WINDOW Form_1 ;
         AT 0, 0 ;
         WIDTH 650 ;
         HEIGHT 470 ;
         TITLE "TsBrowse Sample: Order" ;
         MAIN ;
         NOMAXIMIZE ;
         NOSIZE

      // Add a TBrowse control to the main window
      CreateBrowse( "oBrw_1", "Form_1", ;
         2, 2, ;
         Form_1.WIDTH - 10, ;
         Form_1.HEIGHT - GetTitleHeight() - ;
         iif( IsThemed(), 1, 2 ) * GetBorderHeight() - 2, ;
         cAlias )

   END WINDOW

   // Center and display the main window
   CENTER WINDOW Form_1
   ACTIVATE WINDOW Form_1

RETURN NIL

/*
 * FUNCTION CreateBrowse(cBrw, cParent, nRow, nCol, nWidth, nHeight, cAlias)
 *
 * Dynamically defines and configures a TBrowse control.
 *
 * Parameters:
 *   cBrw     [Character] : Name of the browse control.
 *   cParent  [Character] : Name of the parent window.
 *   nRow     [Numeric]   : Top position (Y).
 *   nCol     [Numeric]   : Left position (X).
 *   nWidth   [Numeric]   : Width of the browse.
 *   nHeight  [Numeric]   : Height of the browse.
 *   cAlias   [Character] : Alias of the DBF to bind to.
 *
 * Returns:
 *   Object reference to the created TBrowse control.
 *
 * Purpose:
 *   Builds a reusable, styled TBrowse grid linked to a database.
 *
 * Notes:
 *   - Sets up field bindings, fonts, and rich color themes.
 *   - Enables editing on most fields except those explicitly excluded.
 */

FUNCTION CreateBrowse( cBrw, cParent, nRow, nCol, nWidth, nHeight, cAlias )

   LOCAL oBrw, i

   // Define the TBrowse control
   DEFINE TBROWSE &cBrw ;
         AT nRow, nCol ;
         ALIAS cAlias ;
         OF &cParent ;
         WIDTH nWidth ;
         HEIGHT nHeight ;
         COLORS { CLR_BLACK, CLR_BLUE }

      :SetAppendMode( .F. )
      :SetDeleteMode( .F. )
      :lNoHScroll := .T.
      :lCellBrw := .T.
      :nSelWidth := 16
      :hFont := GetFontHandle( "FontNorm" )

   END TBROWSE

   // Load field definitions and obtain object reference
   LoadFields( cBrw, cParent )
   oBrw := TBrw_Obj( cBrw, cParent )

   // Set visual and interactive properties
   WITH OBJECT oBrw
      :nHeightCell -= 1
      :nHeightHead += 16
      :nWheelLines := 1
      :lNoChangeOrd := .T.

      :hBrush := CreateSolidBrush( 230, 240, 255 )

      // Define visual styles for various parts of the browse
      :SetColor( { 16 }, { RGB(  43, 149, 168 ) } )                                 // SuperHeader background
      :SetColor( {  3 }, { RGB( 255, 255, 255 ) } )                                 // Header font color
      :SetColor( {  4 }, { {|| { RGB( 43, 149, 168 ), RGB( 0, 54, 94 ) } } } )      // Header background
      :SetColor( { 17 }, { RGB( 255, 255, 255 ) } )                                 // SuperHeader font color
      :SetColor( {  6 }, { {|| { RGB( 255, 255, 74 ), RGB( 240, 240, 0 ) } } } )    // Cursor background
      :SetColor( { 12 }, { {|| { RGB( 128, 128, 128 ), RGB( 250, 250, 250 ) } } } ) // Inactive cursor background
      :SetColor( {  2 }, { {|| RGB( 230, 240, 255 ) } } )                           // Grid background
      :SetColor( {  1 }, { {|| RGB( 0, 0, 0 ) } } )                                 // Grid text color
      :SetColor( {  5 }, { {|| RGB( 0, 0, 255 ) } } )                               // Cursor text color
      :SetColor( { 11 }, { {|| RGB( 0, 0, 0 ) } } )                                 // Inactive cursor text color

      :nClrLine := COLOR_GRID
      :ResetVScroll()

      // Configure specific columns
      :aColumns[ 1 ]:cHeading := "Code"
      :SetColSize( 1, 100 )
      :aColumns[ 1 ]:nAlign := DT_RIGHT
      :aColumns[ 1 ]:nEditMove := DT_DONT_MOVE

      :aColumns[ 5 ]:cHeading := "Birthday"
      :SetColSize( 5, 120 )

      // Enable editing for most columns
      FOR i := 1 TO Len( :aColumns ) - 1
         :aColumns[ i ]:lEdit := .T.
      NEXT

      // Hide column 6 (Bio field)
      :HideColumns( 6, .T. )
   END OBJECT

RETURN oBrw

/*
 * STATIC FUNCTION TBrw_Obj(cBrw, cParent)
 *
 * Retrieves a reference to the TBrowse object from control ID.
 *
 * Parameters:
 *   cBrw     [Character] : Control name of the TBrowse.
 *   cParent  [Character] : Name of the parent window.
 *
 * Returns:
 *   Object reference to the TBrowse control, or NIL if not found.
 *
 * Purpose:
 *   Provides a safe way to access a TBrowse object after it's defined.
 */

STATIC FUNCTION TBrw_Obj( cBrw, cParent )

   LOCAL oBrw := NIL
   LOCAL i

   IF ( i := GetControlIndex( cBrw, cParent ) ) > 0
      oBrw := _HMG_aControlIds[ i ]
   ENDIF

RETURN oBrw

/*
 * PROCEDURE CreateTable()
 *
 * Creates and populates a sample DBF file for demonstration purposes.
 *
 * Purpose:
 *   - Ensures a data source exists for the TBrowse control.
 *   - Demonstrates grid functionality with mock data.
 *
 * Notes:
 *   - Defines 6 fields: Code, First, Last, Married, Birth, and Bio.
 *   - Populates the DBF with 100 rows of test data.
 */

PROCEDURE CreateTable()

   LOCAL aDbf[ 6 ][ 4 ]
   LOCAL i

   // Define DBF structure
   aDbf[ 1 ][ DBS_NAME ] := "Code"
   aDbf[ 1 ][ DBS_TYPE ] := "Numeric"
   aDbf[ 1 ][ DBS_LEN ] := 10
   aDbf[ 1 ][ DBS_DEC ] := 0

   aDbf[ 2 ][ DBS_NAME ] := "First"
   aDbf[ 2 ][ DBS_TYPE ] := "Character"
   aDbf[ 2 ][ DBS_LEN ] := 25
   aDbf[ 2 ][ DBS_DEC ] := 0

   aDbf[ 3 ][ DBS_NAME ] := "Last"
   aDbf[ 3 ][ DBS_TYPE ] := "Character"
   aDbf[ 3 ][ DBS_LEN ] := 25
   aDbf[ 3 ][ DBS_DEC ] := 0

   aDbf[ 4 ][ DBS_NAME ] := "Married"
   aDbf[ 4 ][ DBS_TYPE ] := "Logical"
   aDbf[ 4 ][ DBS_LEN ] := 1
   aDbf[ 4 ][ DBS_DEC ] := 0

   aDbf[ 5 ][ DBS_NAME ] := "Birth"
   aDbf[ 5 ][ DBS_TYPE ] := "Date"
   aDbf[ 5 ][ DBS_LEN ] := 8
   aDbf[ 5 ][ DBS_DEC ] := 0

   aDbf[ 6 ][ DBS_NAME ] := "Bio"
   aDbf[ 6 ][ DBS_TYPE ] := "Memo"
   aDbf[ 6 ][ DBS_LEN ] := 10
   aDbf[ 6 ][ DBS_DEC ] := 0

   // Create and open the DBF
   dbCreate( "Test", aDbf, "DBFCDX" )
   USE Test VIA "DBFCDX"

   // Fill with demo records
   FOR i := 1 TO 100
      APPEND BLANK
      REPLACE Code WITH i
      REPLACE First WITH "First Name " + LTrim( Str( i ) )
      REPLACE Last WITH "Last Name " + LTrim( Str( i ) )
      REPLACE Married WITH ( i % 2 == 0 )
      REPLACE Birth WITH Date() - Max( 10000, RANDOM( 20000 ) ) + RANDOM( LastRec() )
   NEXT

   // Create index on the Code field
   INDEX ON Test->Code TAG Code
   USE

RETURN
