//  test
//  E:\Appl\Harbour_HMG\Report_Class_HMG\
//  Anand K Gupta  Sun, 19 Mar 2017

#include "minigui.ch"
#include "ReportClass.ch"
#include "Version.ch"

/*
	Please note that I have used codes from different samples of HMG and MiniGUI
	for my ease of using them. There is no preference of one over other.
	User are free to change the codes to use different logic, say for printing
	to PDF, HTM etc. files.

	Only request is to make the same available for all to use and make this more
	better and useful.

*/

//*****************************************************************************
FUNCTION Main()

	DEFINE WINDOW Win_1 ;
		AT 0,0 ;
		WIDTH 400 ;
		HEIGHT 400 ;
		TITLE 'Report Class Test' ;
		MAIN

		DEFINE MAIN MENU
			POPUP 'File'
				MENUITEM '&TEXT Report' ACTION TestReport("TEXT")
				MENUITEM '&PRINT Preview Fixed Font' ACTION TestReport("HBPRINT")
				MENUITEM 'P&RINT Preview Propotionate Font' ACTION TestReport("HBPRINT",.t.)
				MENUITEM 'P&DF File' ACTION TestReport("PDF")
				MENUITEM '&HTML File' ACTION TestReport("HTM")
				MENUITEM 'HTML &Absolute Position File' ACTION TestReport("HTM",,.t.)
				MENUITEM '&Excel File' ACTION TestReport("XLS")
				SEPARATOR
				MENUITEM 'E&xit' ACTION ThisWindow.Release
			END POPUP
		END MENU

		ON KEY ESCAPE ACTION ThisWindow.Release

		@ 20, 60 Label lbl_1 value "Report Class" width 300 FONT "Arial" SIZE 16
		@ 60, 60 Label lbl_2 value "Created by Jon Credit for Clipper" WIDTH 300 FONT "Arial" SIZE 10
		@ 80, 60 Label lbl_3 value "Changed by Anand K Gupta for HMG" WIDTH 300 FONT "Arial" SIZE 10
		@ 100, 60 Label lbl_4 value "Output to PDF, XLS, HTM and Print Preview, also Text" WIDTH 300 HEIGHT 40 FONT "Arial" SIZE 10
		@ 140, 60 Label lbl_5 value "Version "+APP_VER+" Dated "+SDMY(APP_DATE) WIDTH 300 FONT "Arial" SIZE 10

	END WINDOW

	CENTER WINDOW Win_1
	ACTIVATE WINDOW Win_1

RETURN NIL

//*****************************************************************************
function TestReport(cDevice, lProportionate, lAbsolutePosition )
local oReport,oCol1,oCol2,oCol3,oCol7,oCOl6,oCol4,oCol5,aStruc
priv HBPRN,cOutputFile,oExcel,oXls

Default lProportionate to .f.
Default lAbsolutePosition to .f.

	aStruc := { ;
	         { "NAME",    "C", 30, 0 }  , ;
	         { "ADDRESS", "C", 30, 0 }  , ;
	         { "CITY",    "C", 30,  0 } , ;
	         { "STATE",   "C", 2,  0 }    ;
	         }

	dbcreate( "testrep1.dbf", aStruc )

	aStruc := { ;
	         { "NAME",    "C", 30, 0 } , ;
	         { "DATE",    "D", 8,  0 } , ;
	         { "AMOUNT",  "N", 8,  2 } , ;
	         { "DESCRIP", "C", 60,  0 }  ;
	         }

	dbcreate( "testrep2.dbf", aStruc )

	use testrep1 exclusive alias MASTER
	use testrep2 exclusive alias DETAIL NEW
	index on field->name to testrep2

	MASTER -> ( dbAppend() )
	MASTER -> NAME     := "JOE SOMEONE"
	MASTER -> ADDRESS  := "123 any street"
	MASTER -> CITY     :=  "Some large name city!!!"
	MASTER -> STATE    := "az"

	MASTER -> ( dbAppend() )
	MASTER -> NAME     := "JANE SOMEONE"
	MASTER -> ADDRESS  := "123 notice it's capitalized"
	MASTER -> CITY     :=  "some city"
	MASTER -> STATE    :=  "Ay"

	MASTER -> ( dbAppend() )
	MASTER -> NAME     := "Jon Credit"
	MASTER -> ADDRESS  := "50 B Paisley Lane"
	MASTER -> CITY     :=  "Columbia"
	MASTER -> STATE    :=  "SC"

	DETAIL -> ( dbAppend() )
	DETAIL -> NAME := "JOE SOMEONE"
	DETAIL -> DATE := CTOD( "01/01/94")
	DETAIL -> AMOUNT := 100.00
	DETAIL -> DESCRIP := "This is the first purchase for Joe Someone"

	DETAIL -> ( dbAppend() )
	DETAIL -> NAME := "JOE SOMEONE"
	DETAIL -> DATE := CTOD( "02/01/94")
	DETAIL -> AMOUNT := 200.00
	DETAIL -> DESCRIP := "This is the second purchase for Joe Someone"

	DETAIL -> ( dbAppend() )
	DETAIL -> NAME := "JOE SOMEONE"
	DETAIL -> DATE := CTOD( "03/03/94")
	DETAIL -> AMOUNT := 330.00
	DETAIL -> DESCRIP := "This is the third purchase for Joe Someone"

	DETAIL -> ( dbAppend() )
	DETAIL -> NAME := "JANE SOMEONE"
	DETAIL -> DATE := CTOD( "01/01/92")
	DETAIL -> AMOUNT := 500.00
	DETAIL -> DESCRIP := "This is the first purchase for Jane Someone"

	DETAIL -> ( dbAppend() )
	DETAIL -> NAME := "JANE SOMEONE"
	DETAIL -> DATE := CTOD( "02/01/92")
	DETAIL -> AMOUNT := 700.00
	DETAIL -> DESCRIP := "This is the second purchase for Jane Someone"

	DETAIL -> ( dbAppend() )
	DETAIL -> NAME := "Jon Credit"
	DETAIL -> DATE := date()
	DETAIL -> AMOUNT := 30
	DETAIL -> DESCRIP := "Just Thirty Dollars to register this report class!!"

	// repeat data to have more than one page of report
	sele detail
	copy to temp1
	appe from temp1
	appe from temp1
	appe from temp1
	appe from temp1
	go top

	gDevice(cDevice, lProportionate, lAbsolutePosition)

	oCol1 := repColumn():new("THIS IS;THE NAME", fieldwblock("NAME", select( "MASTER" ) ), .T. , 13    , NIL )
	oCol2 := repColumn():new("THIS IS;THE;ADDRESS", fieldwblock("ADDRESS", select( "MASTER" ) ), .T. , 10    , "@!" )
	oCol3 := repColumn():new("THIS;IS;THE;CITY", fieldwblock("CITY", select( "MASTER" ) ), .T.  , 10   , "@!" )
	oCol4 := repColumn():new("STATE", fieldwblock("STATE", select( "MASTER" ) ), NIL , 5    , "@!" )
	oCol5 := repColumn():new("DATE OF;PURCHASE", fieldwblock("DATE", select( "DETAIL" ) ), NIL , 8  ,NIL  )
	oCol6 := repColumn():new("THIS IS ;A;CENTERED;TITLE!;;AMOUNT", fieldwblock("AMOUNT", select( "DETAIL" ) ), NIL , 9  , '$99999.99' )
	oCol7 := repColumn():new("DESCRIPTION", fieldwblock("DESCRIP", select( "DETAIL" ) ), .T.  , 15  , NIL )

	oCol1:cColumnTrim := "L"                      // LTRIM()
	oCol2:cColumnTrim := "R"                      // RTRIM()
	oCol3:cColumnTrim := "R"                      // RTRIM()
	oCol7:cColumnTrim := "R"                      // RTRIM()
	oCOl6:cJustify    := "C"                      // CENTER THE TITLE FOR COL6
	oCOl7:cJustify    := "C"                      // CENTER THE TITLE FOR COL6

	oReport := report():new( {|oRepOBj| MyHeader( oRepObj ) },  {|oRepObj| MyFooter( oRepObj ) }, NIL )

	oReport:addColumn( oCol1 )
	oReport:addColumn( oCol2 )
	oReport:addColumn( oCol3 )
	oReport:addColumn( oCol4 )
	oReport:addColumn( oCol5 )
	oReport:addColumn( oCol6 )
	oReport:addColumn( oCol7 )

	oReport:lUndTitles := .T.
	oReport:cWorkArea := "MASTER"

	// Lets create a child column process while the name is the same
	// we will not display the NAME. ADDRESS, or LONG DESCRIPTION while
	// in the child process!!

	oCol1:lChild := .T.
	oCol1:bToDo := {|| DETAIL -> ( dbseek( MASTER -> NAME ) )  }
	oCol1:bWhile :=  {|| DETAIL -> NAME == MASTER -> NAME }
	oCol1:aToBlank := { 1, 2, 3, 4 }  // NAME, ADDRESS, CITY, STATE
	oCol1:cChildAlias := "DETAIL"

	//   NOTE: if you are not doing a child relationship then you need to tell
	//         oCOl5 how to find its data....
	//         however not all of the details records will be displayed!!!
	//         oCol5:bFind := {|| DETAIL -> ( dbseek( MASTER -> NAME ) ) }

	/*
	Notice that we are currently at EOF() yet the report object will gotop()
	by default!!
	*/

	oReport:exec()
	CLOSE ALL

	ferase( "testrep1.dbf" )
	ferase( "testrep2.dbf" )
	ferase( "testrep2.ntx" )
	ferase( "temp1.dbf" )

return (NIL)

// This function is the default value for most of the code blocks
// within the report class!!
FUNCTION Nothing()
RETURN (NIL)

function MyHeader( oRepObj )

	oRepObj:nRow := 3
	@ oRepObj:nRow, 30 SAY "THIS IS A TEST REPORT"
	oRepObj:nRow ++
	@ oRepObj:nRow, 30 SAY "Date "+dtoc(date())+" Time "+time()

//   oRepObj:nRow ++
//   n := len(oRepObj:aColInfo)
//   @ oRepObj:nRow, oRepObj:nStartCol SAY Replicate("=", oRepObj:aColInfo[n]:nCol + (n * oRepObj:nColSpace))

	oRepObj:nRow := 6

	if gDevice() == "HTM"
		gHtmTableCloseOpen()
	endif

return (NIL)

function MyFooter( oRepObj )

	if gDevice() == "HTM"
		gHtmTableCloseOpen()
	endif

	oRepObj:nRow := oRepObj:nRow + 1
	@ oRepObj:nRow, 25 SAY "THIS IS THE TEST REPORT FOOTER!!!!"

return (NIL)


/* You can also process a child relationship within a single database file by
   using a getset function.  Set the field name that is to be repeated in the child
   bToDo block ... bToDo := {|| GetSetNameVar( FIELD_NAME ) }
   assign bWhile to ... {|| FIELD_NAME == GetSetNameVar() }.
   This will process as a child relationship until the
*/

// Standard GetSet function
function GetSetNameVar( cVar )
local cRetVar
static cName := ""

cRetVar := cName
if !( cVar == NIL )
  cName := cVar
endif

return ( cRetVar )


/***
*
*  SDmy( <dDate> ) --> cDate
*
*  Convert a date to string formatted as "dd mmm yyyy".
*
*  Parameter:
*	  dDate - Date value to convert
*
*  Returns: The date value in european date format
*
*/
FUNCTION SDmy( dDate )
   LOCAL cYear,cDate

   if empty(dDate)
	cDate := ''
   else
	cYear := STR( YEAR( dDate ))
	cDate := LTRIM( STR( DAY( dDate ))) + " " + left( CMONTH( dDate ) ,3) + cYear
   endif

RETURN cDate
