/*

 BadaSystem
 Program       : generates_data
 Modulo        : generates data
 Compilador    : MINIGUI - Harbour Win32 GUI
 Link          : BCC 32 bit
 Autor         : Marcos Jarrín
 email         : marvijarrin@gmail.com
 website       : badasystem.tech
 Date          : 01/04/2024
 Update        : 19/04/2024
 Rev           : 1.0

*/

#include "minigui.ch"
#include "dbStruct.ch"

#define LIMITED        1000000

REQUEST DBFCDX

PROCEDURE Main()

   LOCAL nRow, nCol

   SET DATE FORMAT "dd-mm-yyyy"
   rddSetDefault( "DBFCDX" )

   DEFINE WINDOW main_1 ;
         AT 0, 0 ;
         WIDTH 520 ;
         HEIGHT 300 ;
         TITLE "Data Generator" ;
         ICON "APPICON" ;
         MAIN ;
         ON RELEASE CloseAll() ;


      DEFINE STATUSBAR
         STATUSITEM ""
         PROGRESSITEM WIDTH 180 RANGE 0 , 100
         STATUSITEM "    " ACTION Process() TOOLTIP "Start ProgressBar"
         DATE
         STATUSITEM "  BadaSystem  "
      END STATUSBAR

      nRow := 0
      nCol := 20
      @ nROW + 15, nCol + 100 LABEL cAmount VALUE "Quantity" WIDTH 300 HEIGHT 500 SIZE 15
      @ nROW + 15, nCol + 200 TEXTBOX nAmount VALUE 100000 WIDTH 100 HEIGHT 30 SIZE 15 NUMERIC INPUTMASK "9,999,999"


      @ nROW + 60, nCol + 10 CHECKLABEL lNameCheck ;
         HEIGHT 46 ;
         VALUE 'Name Full  ' ;
         FONT 'Arial' SIZE 12 ;
         IMAGE { 'MINIGUI_SWITCH_ON', 'MINIGUI_SWITCH_OFF' } ;
         CHECKED ;
         VCENTERALIGN ;
         AUTOSIZE ;
         ON MOUSEHOVER Rc_Cursor( "MINIGUI_FINGER" )

      @ nROW + 60, nCol + 300 CHECKLABEL lCountryCheck ;
         HEIGHT 46 ;
         VALUE 'Country' ;
         FONT 'Arial' SIZE 12 ;
         IMAGE { 'MINIGUI_SWITCH_ON', 'MINIGUI_SWITCH_OFF' } ;
         CHECKED ;
         VCENTERALIGN ;
         AUTOSIZE ;
         ON MOUSEHOVER Rc_Cursor( "MINIGUI_FINGER" )

      @ nROW + 100, nCol + 10 CHECKLABEL lEmailCheck ;
         HEIGHT 46 ;
         VALUE 'Email        ' ;
         FONT 'Arial' SIZE 12 ;
         IMAGE { 'MINIGUI_SWITCH_ON', 'MINIGUI_SWITCH_OFF' } ;
         CHECKED ;
         VCENTERALIGN ;
         AUTOSIZE ;
         ON MOUSEHOVER Rc_Cursor( "MINIGUI_FINGER" )

      @ nROW + 100, nCol + 300 CHECKLABEL lSalaryCheck ;
         HEIGHT 46 ;
         VALUE 'Salaty  ' ;
         FONT 'Arial' SIZE 12 ;
         IMAGE { 'MINIGUI_SWITCH_ON', 'MINIGUI_SWITCH_OFF' } ;
         CHECKED ;
         VCENTERALIGN ;
         AUTOSIZE ;
         ON MOUSEHOVER Rc_Cursor( "MINIGUI_FINGER" )

      @ nROW + 150, nCol + 10 CHECKLABEL lDateCheck ;
         HEIGHT 46 ;
         VALUE 'Date         ' ;
         FONT 'Arial' SIZE 12 ;
         IMAGE { 'MINIGUI_SWITCH_ON', 'MINIGUI_SWITCH_OFF' } ;
         CHECKED ;
         VCENTERALIGN ;
         AUTOSIZE ;
         ON MOUSEHOVER Rc_Cursor( "MINIGUI_FINGER" )

      SetProperty( 'main_1', 'lNameCheck', "Enabled", .F. )

      @ nROW + 200, nCol + 175 BUTTON ACEPTAR CAPTION '&Process' ACTION Process()

   END WINDOW

   CENTER WINDOW main_1
   ACTIVATE WINDOW main_1

RETURN


/*

  Process to generate information


*/
PROCEDURE Process()

   LOCAL nNumber1, nNumber2, nNumber3, nNumber4, nNumber5, nNumber6
   LOCAL nX
   LOCAL nValue
   FIELD id, NAME, middlename, lastname0, lastname1 IN data
   LOCAL dat01, dat02, dat03, dat04, dat05
   LOCAL cEmail := ""
   LOCAL aEmail1 := { "@hotmail", "@outlook", "@outlook", "@yandex", "@gmail", "@yahoo", "@yahoo", "@proton", "@latinmail", "@ibm", "@intel", "@atary", "@mac", "@verizon", "@icloud" }
   LOCAL aEmail2 := { ".com", ".net", ".org", ".es", ".tv", ".co", ".ec", ".br", ".us", ".ru", ".pol", ".ch", ".ar", ".zn", ".uk" }
   LOCAL nEmaila1 := 1
   LOCAL nEmaila2 := 1
   LOCAL nCantidad
   LOCAL nEmalS := 1
   LOCAL lCountry, lEmail, lSalary, lDate
   LOCAL nDivi

   //
   nCantidad := main_1.nAmount.VALUE
   IF nCantidad == 0
      nCantidad := 1000
   ENDIF

   IF nCantidad > LIMITED
      MsgBox( "Limit exceeded" )
      RETURN
   ENDIF

   // Data entered
   lCountry := main_1.lCountryCheck.checked
   lEmail := main_1.lEmailCheck.checked
   lSalary := main_1.lSalaryCheck.checked
   lDate := main_1.lDateCheck.checked


   // Create the table where the data is generated
   createinter()

   // Using data tables of world names and countries
   USE data NEW READONLY
   USE country NEW READONLY

   nDivi := nCantidad / 100
   FOR nX := 1 TO nCantidad

      nNumber1 := Round( hb_Random( 125 ), 0 )
      nNumber2 := Round( hb_Random( 125 ), 0 )
      nNumber3 := Round( hb_Random( 125 ), 0 )
      nNumber4 := Round( hb_Random( 125 ), 0 )

      IF lCountry
         nNumber5 := Round( hb_Random( 213 ), 0 )
         if(nNumber5 == 0,nNumber5++,nNumber5)

         country->( dbGoto( nNumber5 ) )
         dat05 := " " + AllTrim( country->countrynom )
      ENDIF

      IF lSalary
         nNumber6 := Round( hb_Random( 99999 ), 0 )
         if(nNumber6 == 0,nNumber6++,nNumber6)
      ENDIF

      if(nNumber1 == 0,nNumber1++,nNumber1)
      if(nNumber2 == 0,nNumber2++,nNumber2)
      if(nNumber3 == 0,nNumber3++,nNumber3)
      if(nNumber4 == 0,nNumber4++,nNumber4)

      IF lEmail
         nEmaila1 := Round( hb_Random( 14 ), 0 )
         nEmaila2 := Round( hb_Random( 14 ), 0 )

         if(nEmaila1 == 0,nEmaila1++,nEmaila1)
         if(nEmaila2 == 0,nEmaila2++,nEmaila2)
      ENDIF

      data->( dbGoto( nNumber1 ) )
      dat01 := AllTrim( name )

      data->( dbGoto( nNumber2 ) )
      dat02 := " " + AllTrim( middlename )

      data->( dbGoto( nNumber3 ) )
      dat03 := " " + AllTrim( lastname0 )

      data->( dbGoto( nNumber4 ) )
      dat04 := " " + AllTrim( lastname0 )


      IF lEmail
         IF nEmalS == 1
            cEmail := Lower( dat01 ) + AllTrim( Str( Round( hb_Random(9999 ), 0 ) ) ) + aEmail1[ nEmaila1 ] + aEmail2[ nEmaila2 ]
            nEmalS++
         ELSE
            cEmail := LTrim( Lower( dat02 ) ) + AllTrim( Str( Round( hb_Random(9999 ), 0 ) ) ) + aEmail1[ nEmaila1 ] + aEmail2[ nEmaila2 ]
            nEmalS := 1
         ENDIF
      ENDIF


      data_end->( dbAppend() )
      data_end->namefull := dat01 + dat02 + dat03 + dat04

      IF lCountry
         data_end->country := dat05
      ENDIF

      IF lSalary
         data_end->salary := nNumber6
      ENDIF

      IF lEmail
         data_end->email := cEmail
      ENDIF

      IF lDate
         data_end->dates := CreateDate()
      ENDIF


      nValue := Int( nX / nDivi )

      SET STATUSBAR ProgressItem OF main_1 Position TO nValue

      IF nX % 500 == 0
         main_1.StatusBar.Item( 3 ) := Str( nValue, 7 ) + "      "
         InKeyGUI(20)
      ENDIF

   NEXT

   SELECT data_end
   COPY TO datos_end_no_duplicates
   COPY TO data_end.csv DELIMITED

   CloseAll()

   MSGBOX( "Finished process" )

RETURN

/*

  Create dbf table for generated data


*/
PROCEDURE createinter()

   LOCAL aEstructure := {}
   LOCAL lCountry, lEmail, lSalary, lDate
   FIELD namefull, country, email, salary, dates IN data_end

   lCountry := main_1.lCountryCheck.checked
   lEmail := main_1.lEmailCheck.checked
   lSalary := main_1.lSalaryCheck.checked
   lDate := main_1.lDateCheck.checked

   // Eliminar las tablas
   FErase( "data_end.dbf" )
   FErase( "data_end.cdx" )
   FErase( "datos_end_sin_duplicados.dbf" )
   FErase( "data_end.csv" )


   // Create data name, lastname etc
   AAdd( aEstructure, { "namefull", "C", 50, 0 } )

   IF lCountry
      AAdd( aEstructure, { "country", "C", 30, 0 } )
   ENDIF

   IF lEmail
      AAdd( aEstructure, { "email", "C", 25, 0 } )
   ENDIF

   IF lSalary
      AAdd( aEstructure, { "salary", "N", 6, 0 } )
   ENDIF

   IF lDate
      AAdd( aEstructure, { "dates", "D", 8, 0 } )
   ENDIF

   dbCreate( "data_end", aEstructure )
   USE data_end NEW
   INDEX ON namefull TAG namefull UNIQUE

RETURN



/*

  Close all open tables


*/
PROCEDURE CloseAll()

   dbCloseAll()

RETURN



/*

   Generates dates randomly from 1950 plus 70 years maximum.

*/
FUNCTION CreateDate()

   LOCAL nDay, nMounth, nYear, dDate

   nDay := Round( hb_Random( 27 ), 0 )
   nMounth := Round( hb_Random( 11 ), 0 )
   nYear := Round( hb_Random( 70 ), 0 )

   if(nDay    == 0,nDay++   ,nDay)
   if(nMounth == 0,nMounth++,nMounth)
   if(nYear   == 0,nYear++  ,nYear)

   dDate := CToD( Str( nday ) + "-" + Str( nMounth ) + "-" + Str( 1950 + nYear ) )

RETURN dDate
