#include "hmg.ch"

// Define the fields for the customer database
FIELD ID, NAME, EMAIL, PHONE, ADDRESS, BIRTHDAY, AGE

FUNCTION Main()
   /*
    *  Main function of the application.
    *  This function initializes the application, defines the main window,
    *  and handles user interactions.
    */

   LOCAL n, bBackColor
   LOCAL cSearch := "" // Local variable to store the search string

   SET NAVIGATION EXTENDED // Enables extended navigation features in HMG
   SET DATE FORMAT "yyyy-mm-dd" // Sets the date format to year-month-day

   CreateDBF() // Calls the CreateDBF() function to create the database file if it doesn't exist

   // Open DBF
   IF ! File( "customers.dbf" ) // Checks if the database file "customers.dbf" exists
      MsgStop( "Database file customers.dbf not found!", "Error" ) // Displays an error message if the file is not found
      RETURN NIL // Exits the Main() function if the database file is not found
   ENDIF

   USE customers NEW VIA "DBFNTX" // Opens the "customers.dbf" database file in exclusive mode using the DBFNTX driver

   SET DEFAULT ICON TO "APPICON"

   DEFINE WINDOW MainWindow ;
         AT 0, 0 ;
         WIDTH 785 HEIGHT 600 ;
         TITLE "Customer Database Manager" ;
         MAIN

      @ 10, 10 LABEL lblTitle VALUE "Customers" WIDTH 300 FONT "Arial" SIZE 16

      @ 40, 10 GRID Grid_1 ;
         AUTOSIZEWIDTH 750 ;
         AUTOSIZEHEIGHT 20 ;
         HEADERS { "ID", "Name", "Email", "Phone", "Address", "Birthday", "Age" } ;
         WIDTHS { 50, 140, 120, 110, 200, 80, 45 } ;
         ITEMS {} ;
         VALUE { 1, 1 } ;
         ON DBLCLICK EditCustomer() ;
         CELLNAVIGATION ;
         PAINTDOUBLEBUFFER

      // Set color theme
      bBackColor := {| x, CellRowIndex | HB_SYMBOL_UNUSED( x ), iif( CellRowIndex / 2 == Int( CellRowIndex / 2 ), { 220, 220, 220 }, WHITE ) }
      FOR n = 1 TO 7
         MainWindow.Grid_1.HeaderDynamicForeColor( n ) := {|| BLACK }
         MainWindow.Grid_1.HeaderDynamicBackColor( n ) := {|| { 240, 240, 240 } }
         MainWindow.Grid_1.ColumnDynamicBackColor( n ) := bBackColor
      NEXT
      _HMG_GridSelectedCellForeColor := YELLOW
      _HMG_GridSelectedCellBackColor := { 0, 0, 128 }
      _HMG_GridSelectedRowBackColor := { 0, 120, 220 }
      _HMG_GridSelectedRowForeColor := WHITE

      @ 460, 10 BUTTON btnAdd CAPTION "Add" WIDTH 100 HEIGHT 28 ACTION AddCustomer()
      @ 460, 120 BUTTON btnEdit CAPTION "Edit" WIDTH 100 HEIGHT 28 ACTION EditCustomer()
      @ 460, 230 BUTTON btnDelete CAPTION "Delete" WIDTH 100 HEIGHT 28 ACTION DeleteCustomer()
      @ 460, 340 BUTTON btnRefresh CAPTION "Refresh" WIDTH 100 HEIGHT 28 ACTION RefreshGrid()

      @ 510, 10 LABEL lblSearch VALUE "Search Name:" WIDTH 100 CENTERALIGN VCENTERALIGN
      @ 510, 120 TEXTBOX txtSearch ;
         WIDTH 200 ;
         VALUE "" ;
         ON CHANGE {|| cSearch := THIS.VALUE, SearchCustomer( cSearch ) }

   END WINDOW

   RefreshGrid() // Calls the RefreshGrid() function to populate the grid with data from the database

   MainWindow.CENTER // Centers the main window on the screen
   MainWindow.ACTIVATE // Activates the main window, making it visible and responsive

RETURN NIL

// ------------------------

PROCEDURE RefreshGrid()
   /*
    *  Refreshes the data displayed in the grid control.
    *  This procedure reads all records from the "customers" database
    *  and populates the grid with the data.
    */

   LOCAL aRows := {} // Local array to store the data for each row in the grid

   GO TOP // Moves the record pointer to the first record in the database
   DO WHILE ! Eof() // Loops through all records in the database until the end-of-file (EOF) is reached
      AAdd( aRows, { ; // Adds a new row to the aRows array with the data from the current record
      Str( ID ), ; // Converts the ID field (numeric) to a string
      NAME, ; // The Name field
      EMAIL, ; // The Email field
      PHONE, ; // The Phone field
      ADDRESS, ; // The Address field
      DToC( BIRTHDAY ), ; // Converts the Birthday field (date) to a character string in the current date format
      Str( AGE ) ; // Converts the Age field (numeric) to a string
      } )
      SKIP // Moves the record pointer to the next record in the database
   ENDDO

   MainWindow.Grid_1.SetArray( aRows ) // Sets the data in the grid control to the data stored in the aRows array

RETURN

// ------------------------

PROCEDURE AddCustomer()
   /*
    *  Opens a modal window to add a new customer to the database.
    *  This procedure defines a new window with textboxes for entering
    *  customer information and a button to save the new customer.
    */

   DEFINE WINDOW WinAdd ;
         AT 100, 100 WIDTH 400 HEIGHT 400 ;
         MODAL ;
         TITLE "Add New Customer"

      @ 10, 10 LABEL lbl1 VALUE "Name:" WIDTH 80
      @ 30, 10 TEXTBOX txtName WIDTH 300

      @ 60, 10 LABEL lbl2 VALUE "Email:" WIDTH 80
      @ 80, 10 TEXTBOX txtEmail WIDTH 300

      @ 110, 10 LABEL lbl3 VALUE "Phone:" WIDTH 80
      @ 130, 10 TEXTBOX txtPhone WIDTH 300

      @ 160, 10 LABEL lbl4 VALUE "Address:" WIDTH 80
      @ 180, 10 TEXTBOX txtAddress WIDTH 300

      @ 210, 10 LABEL lbl5 VALUE "Birthday:" WIDTH 80
      @ 230, 10 DATEPICKER dpkBirthday WIDTH 200

      @ 260, 10 LABEL lbl6 VALUE "Age:" WIDTH 80
      @ 280, 10 TEXTBOX txtAge WIDTH 50

      @ 320, 10 BUTTON btnSave CAPTION "Save" WIDTH 100 ACTION {|| SaveNewCustomer(), WinAdd.Release }

   END WINDOW

   WinAdd.CENTER // Centers the "Add New Customer" window on the screen
   WinAdd.ACTIVATE // Activates the "Add New Customer" window, making it visible and responsive

RETURN

PROCEDURE SaveNewCustomer()
   /*
    *  Saves the new customer data entered in the "Add New Customer" window
    *  to the database. This procedure appends a new blank record to the
    *  database, populates it with the data from the textboxes in the
    *  "Add New Customer" window, and then refreshes the grid.
    */

   LOCAL nMaxID := 0 // Local variable to store the maximum ID value in the database

   GO TOP // Moves the record pointer to the first record in the database
   DO WHILE ! Eof() // Loops through all records in the database until the end-of-file (EOF) is reached
      IF ID > nMaxID // Checks if the current record's ID is greater than the current maximum ID
         nMaxID := ID // Updates the maximum ID if the current record's ID is greater
      ENDIF
      SKIP // Moves the record pointer to the next record in the database
   ENDDO

   APPEND BLANK // Appends a new blank record to the end of the database
   REPLACE ID WITH ( nMaxID + 1 ) // Replaces the ID field in the new record with the next available ID (maximum ID + 1)
   REPLACE NAME WITH WinAdd.txtName.VALUE // Replaces the Name field with the value from the txtName textbox in the "Add New Customer" window
   REPLACE EMAIL WITH WinAdd.txtEmail.VALUE // Replaces the Email field with the value from the txtEmail textbox in the "Add New Customer" window
   REPLACE PHONE WITH WinAdd.txtPhone.VALUE // Replaces the Phone field with the value from the txtPhone textbox in the "Add New Customer" window
   REPLACE ADDRESS WITH WinAdd.txtAddress.VALUE // Replaces the Address field with the value from the txtAddress textbox in the "Add New Customer" window
   REPLACE BIRTHDAY WITH WinAdd.dpkBirthday.VALUE // Replaces the Birthday field with the value from the dpkBirthday datepicker in the "Add New Customer" window
   REPLACE AGE WITH Val( WinAdd.txtAge.Value ) // Replaces the Age field with the value from the txtAge textbox in the "Add New Customer" window (converted to a numeric value)

   RefreshGrid() // Calls the RefreshGrid() function to update the grid with the new data

   MainWindow.Grid_1.VALUE := { LastRec(), 1 } // Sets the grid's selected cell to the last record added

RETURN

// ------------------------

PROCEDURE EditCustomer()
   /*
    *  Opens a modal window to edit an existing customer in the database.
    *  This procedure retrieves the selected record from the grid,
    *  populates the textboxes in the "Edit Customer" window with the
    *  data from the selected record, and then displays the window.
    */

   LOCAL nSelected := MainWindow.Grid_1.VALUE[ 1 ] // Local variable to store the row number of the selected record in the grid
   IF nSelected == 0 // Checks if a record is selected in the grid
      MsgStop( "Please select a record first!", "Warning" ) // Displays a warning message if no record is selected
      RETURN // Exits the EditCustomer() function if no record is selected
   ENDIF

   GOTO nSelected // Moves the record pointer to the selected record in the database

   DEFINE WINDOW WinEdit ;
         AT 100, 100 WIDTH 400 HEIGHT 400 ;
         MODAL ;
         TITLE "Edit Customer"

      @ 10, 10 LABEL lbl1 VALUE "Name:" WIDTH 80
      @ 30, 10 TEXTBOX txtName WIDTH 300 VALUE NAME

      @ 60, 10 LABEL lbl2 VALUE "Email:" WIDTH 80
      @ 80, 10 TEXTBOX txtEmail WIDTH 300 VALUE EMAIL

      @ 110, 10 LABEL lbl3 VALUE "Phone:" WIDTH 80
      @ 130, 10 TEXTBOX txtPhone WIDTH 300 VALUE PHONE

      @ 160, 10 LABEL lbl4 VALUE "Address:" WIDTH 80
      @ 180, 10 TEXTBOX txtAddress WIDTH 300 VALUE ADDRESS

      @ 210, 10 LABEL lbl5 VALUE "Birthday:" WIDTH 80
      @ 230, 10 DATEPICKER dpkBirthday WIDTH 200 VALUE BIRTHDAY

      @ 260, 10 LABEL lbl6 VALUE "Age:" WIDTH 80
      @ 280, 10 TEXTBOX txtAge WIDTH 50 VALUE Str( AGE )

      @ 320, 10 BUTTON btnUpdate CAPTION "Update" WIDTH 100 ACTION {|| UpdateCustomer(), WinEdit.Release }

   END WINDOW

   WinEdit.CENTER // Centers the "Edit Customer" window on the screen
   WinEdit.ACTIVATE // Activates the "Edit Customer" window, making it visible and responsive

RETURN

PROCEDURE UpdateCustomer()
   /*
    *  Updates the customer data in the database with the data entered in
    *  the "Edit Customer" window. This procedure replaces the fields in the
    *  current record (the record that was being edited) with the values
    *  from the textboxes in the "Edit Customer" window, and then refreshes
    *  the grid.
    */
   REPLACE NAME WITH WinEdit.txtName.VALUE // Replaces the Name field with the value from the txtName textbox in the "Edit Customer" window
   REPLACE EMAIL WITH WinEdit.txtEmail.VALUE // Replaces the Email field with the value from the txtEmail textbox in the "Edit Customer" window
   REPLACE PHONE WITH WinEdit.txtPhone.VALUE // Replaces the Phone field with the value from the txtPhone textbox in the "Edit Customer" window
   REPLACE ADDRESS WITH WinEdit.txtAddress.VALUE // Replaces the Address field with the value from the txtAddress textbox in the "Edit Customer" window
   REPLACE BIRTHDAY WITH WinEdit.dpkBirthday.VALUE // Replaces the Birthday field with the value from the dpkBirthday datepicker in the "Edit Customer" window
   REPLACE AGE WITH Val( WinEdit.txtAge.Value ) // Replaces the Age field with the value from the txtAge textbox in the "Edit Customer" window (converted to a numeric value)

   RefreshGrid() // Calls the RefreshGrid() function to update the grid with the changes

RETURN

// ------------------------

PROCEDURE DeleteCustomer()
   /*
    *  Deletes the selected customer from the database.
    *  This procedure retrieves the selected record from the grid,
    *  prompts the user for confirmation, and then deletes the record
    *  if the user confirms.
    */

   LOCAL nSelected := MainWindow.Grid_1.VALUE[ 1 ] // Local variable to store the row number of the selected record in the grid
   IF nSelected == 0 // Checks if a record is selected in the grid
      MsgStop( "Please select a record first!", "Warning" ) // Displays a warning message if no record is selected
      RETURN // Exits the DeleteCustomer() function if no record is selected
   ENDIF

   IF MsgYesNo( "Are you sure you want to delete this customer?", "Confirm" ) // Prompts the user for confirmation before deleting the record
      GOTO nSelected // Moves the record pointer to the selected record in the database
      DELETE // Marks the current record for deletion
      PACK // Physically removes all records marked for deletion from the database
      RefreshGrid() // Calls the RefreshGrid() function to update the grid after deleting the record
   ENDIF

RETURN

// ------------------------

PROCEDURE SearchCustomer( cText )
   /*
    *  Searches for customers in the database whose name contains the
    *  specified search text. This procedure iterates through all records
    *  in the database and adds the records that match the search criteria
    *  to a new array, which is then used to update the grid.
    *
    *  Parameters:
    *      cText - The search text to look for in the customer names.
    */

   LOCAL aFound := {} // Local array to store the records that match the search criteria

   GO TOP // Moves the record pointer to the first record in the database
   DO WHILE ! Eof() // Loops through all records in the database until the end-of-file (EOF) is reached
      IF cText == "" .OR. At( Upper( cText ), Upper( NAME ) ) > 0 // Checks if the search text is empty or if the customer's name contains the search text (case-insensitive)
         AAdd( aFound, { ; // Adds the current record to the aFound array if it matches the search criteria
         Str( ID ), ; // Converts the ID field (numeric) to a string
         NAME, ; // The Name field
         EMAIL, ; // The Email field
         PHONE, ; // The Phone field
         ADDRESS, ; // The Address field
         DToC( BIRTHDAY ), ; // Converts the Birthday field (date) to a character string in the current date format
         Str( AGE ) ; // Converts the Age field (numeric) to a string
         } )
      ENDIF
      SKIP // Moves the record pointer to the next record in the database
   ENDDO

   MainWindow.Grid_1.SetArray( aFound ) // Sets the data in the grid control to the data stored in the aFound array (the search results)

RETURN

// ------------------------

PROCEDURE CreateDBF()
    /*
     *  Creates the "customers.dbf" database file if it doesn't exist.
     *  This procedure defines the structure of the database file (fields
     *  and their data types) and populates it with some example data.
     */

   // Define the structure of the DBF file
   LOCAL aFields := { ;
      { "ID", "N", 5, 0 }, ;
      { "Name", "C", 50, 0 }, ;
      { "Email", "C", 50, 0 }, ;
      { "Phone", "C", 20, 0 }, ;
      { "Address", "C", 80, 0 }, ;
      { "Birthday", "D", 8, 0 }, ;
      { "Age", "N", 3, 0 } ;
      }

   // Example data for 20 records
   LOCAL aNames := { "John Doe", "Jane Smith", "Alice Johnson", "Bob Brown", "Charlie Davis", ;
      "Eva Wilson", "Frank Miller", "Grace Garcia", "Henry Martinez", "Ivy Robinson", ;
      "Jack Thompson", "Karen Clark", "Leo Lewis", "Mia Lee", "Nathan Young", ;
      "Olivia Hall", "Paul Allen", "Quinn Hernandez", "Rachel King", "Sam Lopez" }

   LOCAL aEmails := { "john.doe@example.com", "jane.smith@example.com", "alice.johnson@example.com", ;
      "bob.brown@example.com", "charlie.davis@example.com", "eva.wilson@example.com", ;
      "frank.miller@example.com", "grace.garcia@example.com", "henry.martinez@example.com", ;
      "ivy.robinson@example.com", "jack.thompson@example.com", "karen.clark@example.com", ;
      "leo.lewis@example.com", "mia.lee@example.com", "nathan.young@example.com", ;
      "olivia.hall@example.com", "paul.allen@example.com", "quinn.hernandez@example.com", ;
      "rachel.king@example.com", "sam.lopez@example.com" }

   LOCAL aPhones := { "123-456-7890", "234-567-8901", "345-678-9012", "456-789-0123", "567-890-1234", ;
      "678-901-2345", "789-012-3456", "890-123-4567", "901-234-5678", "012-345-6789", ;
      "123-456-7890", "234-567-8901", "345-678-9012", "456-789-0123", "567-890-1234", ;
      "678-901-2345", "789-012-3456", "890-123-4567", "901-234-5678", "012-345-6789" }

   LOCAL aAddresses := { "123 Main St", "456 Elm St", "789 Oak St", "101 Pine St", "202 Cedar St", ;
      "303 Maple St", "404 Birch St", "505 Walnut St", "606 Chestnut St", "707 Spruce St", ;
      "808 Ash St", "909 Alder St", "1010 Beech St", "1111 Dogwood St", "1212 Magnolia St", ;
      "1313 Willow St", "1414 Sycamore St", "1515 Locust St", "1616 Hickory St", "1717 Juniper St" }

   LOCAL aBirthdays := { "1990-01-01", "1985-02-15", "1992-03-20", "1988-04-25", "1995-05-30", ;
      "1980-06-10", "1993-07-18", "1987-08-22", "1991-09-28", "1983-10-05", ;
      "1994-11-12", "1989-12-19", "1990-01-24", "1986-02-28", "1993-03-14", ;
      "1984-04-30", "1991-05-07", "1982-06-15", "1995-07-21", "1988-08-29" }

   LOCAL aAges := Array( 20 )

   LOCAL h := { => }, i

   FOR i := 1 TO Len( aFields )
      h[ aFields[ i ][ 1 ] ] := i
   NEXT

   // Calculate the age
   FOR i := 1 TO Len( aAges )
      aAges[ i ] := Age( Date(), CToD( aBirthdays[ i ] ) )
   NEXT

   // Create the DBF file
   dbCreate( "customers", aFields, "DBFNTX" )

   USE customers NEW VIA "DBFNTX"

   // Populate the DBF file with 20 example records
   FOR i := 1 TO Len( aNames )
      dbAppend()
      FieldPut( h[ "ID" ], i )
      FieldPut( h[ "Name" ], aNames[ i ] )
      FieldPut( h[ "Email" ], aEmails[ i ] )
      FieldPut( h[ "Phone" ], aPhones[ i ] )
      FieldPut( h[ "Address" ], aAddresses[ i ] )
      FieldPut( h[ "Birthday" ], CToD( aBirthdays[ i ] ) )
      FieldPut( h[ "Age" ], aAges[ i ] )
   NEXT

   // Close the DBF file
   CLOSE DATABASES

RETURN

STATIC FUNCTION Age( dWhen, dBirth )
/*
 *  Calculates the age based on the current date and the date of birth.
 *
 *  Parameters:
 *      dWhen   - The current date.
 *      dBirth  - The date of birth.
 *
 *  Return:
 *      The age as a numeric value.
 */
RETURN Year( dWhen ) - Year( dBirth ) - iif( Right( DToS( dBirth ), 4 ) > Right( DToS( dWhen ), 4 ), 1, 0 )
