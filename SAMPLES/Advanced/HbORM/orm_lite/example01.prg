/**
 * BadaSystem
 * Program       : HbORM - Harbour Object-Relational Mapping
 * Module        : HbORM Test Program
 * Compiler      : MINIGUI - Harbour Win32 GUI
 * Compiler-C    : BCC 32 bit
 * Author        : Marcos Jarrin
 * Email         : marvijarrin@gmail.com
 * Date          : 22/07/2025
 * Update        : 02/08/2025
 * Rev           : 0.1
 * Description   : Comprehensive test of HbORM, HbORMRelation and HbORMValidator classes
 */

/**
 * Main procedure for testing HbORM functionality
 * procedure Main
 * description Tests all major features of the HbORM system including:
 *              - Basic CRUD operations
 *              - Table relations (1:N)
 *              - Data validation
 *              - Query building
 */
PROCEDURE Main()
   LOCAL oCustomers, oProducts, oOrders, oOrderDetails
   LOCAL oCustProdRelation, oOrderRelation
   LOCAL oValidator
   LOCAL oQuery
   LOCAL hData, aResults, nCount
   LOCAL lSuccess
   LOCAL cError
   LOCAL aRelations
   LOCAL i

   REQUEST DBFCDX

   // Initialize error logging
   SET DATE FORMAT "yyyy-mm-dd"
   SET CENTURY ON
   SET DELETED ON
   RddSetDefault( "DBFCDX" )
   SET EXCLUSIVE ON

   ? "=== Starting HbORM Test Program ==="
   ?

   // Create test tables if they don't exist
   ? "Creating test tables if needed..."
   CreateTestTables()
   GenerateTestData()
   ?

   // Test 1: Basic ORM operations
   ? "=== TEST 1: Basic ORM Operations ==="

   // Create ORM instances
   oCustomers    := HbORM():New("customers", "cust","data\")
   oProducts     := HbORM():New("products", "prod","data\")
   oOrders       := HbORM():New("orders", "ord","data\")
   oOrderDetails := HbORM():New("order_details", "ord_det","data\")

   // Open tables
   IF !oCustomers:Open() .OR. !oProducts:Open() .OR. !oOrders:Open() .OR. !oOrderDetails:Open()
      ? "Error opening tables:", oCustomers:GetError(), oProducts:GetError(), oOrders:GetError(), oOrderDetails:GetError()
      QUIT
   ENDIF

   // Test insert operations
   ? "Inserting test data..."

   // Insert customers
   lSuccess := oCustomers:Insert({;
      "CUST_ID" => 51, ;
      "NAME" => "John Doe", ;
      "EMAIL" => "john@example.com", ;
      "PHONE" => "555-0101", ;
      "ADDRESS" => "123 Main St", ;
      "REG_DATE" => Date(), ;
      "STATUS" => "A" ;
   })

   lSuccess := lSuccess .AND. oCustomers:Insert({;
      "CUST_ID" => 52, ;
      "NAME" => "Zane Smith", ;
      "EMAIL" => "jane@example.com", ;
      "PHONE" => "555-0102", ;
      "ADDRESS" => "456 Oak Ave", ;
      "REG_DATE" => Date() - 1, ;
      "STATUS" => "A" ;
   })

   lSuccess := lSuccess .AND. oCustomers:Insert({;
      "CUST_ID" => 53, ;
      "NAME" => "Bob Johnson", ;
      "EMAIL" => "bob@example.com", ;
      "PHONE" => "555-0103", ;
      "ADDRESS" => "789 Pine Rd", ;
      "REG_DATE" => Date() - 2, ;
      "STATUS" => "I" ;
   })

   // Insert products
   lSuccess := lSuccess .AND. oProducts:Insert({;
      "PROD_ID" => 51, ;
      "NAME" => "Laptop", ;
      "DESCRIPTIO" => "High performance laptop", ;
      "PRICE" => 999.99, ;
      "STOCK" => 50, ;
      "CATEGORY" => "Electronics" ;
   })

   lSuccess := lSuccess .AND. oProducts:Insert({;
      "PROD_ID" => 52, ;
      "NAME" => "Smartphone", ;
      "DESCRIPTIO" => "Latest model smartphone", ;
      "PRICE" => 699.99, ;
      "STOCK" => 100, ;
      "CATEGORY" => "Electronics" ;
   })

   lSuccess := lSuccess .AND. oProducts:Insert({;
      "PROD_ID" => 53, ;
      "NAME" => "Desk Chair", ;
      "DESCRIPTIO" => "Ergonomic office chair", ;
      "PRICE" => 199.99, ;
      "STOCK" => 30, ;
      "CATEGORY" => "Furniture" ;
   })

   // Insert orders
   lSuccess := lSuccess .AND. oOrders:Insert({;
      "ORDER_ID" => 130, ;
      "CUST_ID" => 1, ;
      "ORDER_DATE" => Date(), ;
      "TOTAL" => 1299.98, ;
      "STATUS" => "P" ;
   })

   lSuccess := lSuccess .AND. oOrders:Insert({;
      "ORDER_ID" => 131, ;
      "CUST_ID" => 2, ;
      "ORDER_DATE" => Date() - 1, ;
      "TOTAL" => 199.99, ;
      "STATUS" => "S" ;
   })

   // Insert order details
   lSuccess := lSuccess .AND. oOrderDetails:Insert({;
      "DETAIL_ID" => 130, ;
      "ORDER_ID" => 1, ;
      "PROD_ID" => 1, ;
      "QUANTITY" => 1, ;
      "UNIT_PRICE" => 999.99, ;
      "SUBTOTAL" => 999.99 ;
   })

   lSuccess := lSuccess .AND. oOrderDetails:Insert({;
      "DETAIL_ID" => 130, ;
      "ORDER_ID" => 1, ;
      "PROD_ID" => 2, ;
      "QUANTITY" => 1, ;
      "UNIT_PRICE" => 299.99, ;
      "SUBTOTAL" => 299.99 ;
   })

   lSuccess := lSuccess .AND. oOrderDetails:Insert({;
      "DETAIL_ID" => 131, ;
      "ORDER_ID" => 2, ;
      "PROD_ID" => 3, ;
      "QUANTITY" => 1, ;
      "UNIT_PRICE" => 199.99, ;
      "SUBTOTAL" => 199.99 ;
   })

   IF !lSuccess
      ? "Error inserting test data"
      QUIT
   ENDIF

   ? "   "
   ?

   // Test record navigation
   ? "Testing record navigation..."
   oCustomers:GoTop()
   ? "First customer:", oCustomers:GetValue("NAME")

   oCustomers:Skip()
   ? "Second customer:", oCustomers:GetValue("NAME")

   oCustomers:GoBottom()
   ? "Last customer:", oCustomers:GetValue("NAME")

   oCustomers:GoTo(1)
   ? "Customer at position 1:", oCustomers:GetValue("NAME")
   ?

   // Test GetRow method
   ? "Testing GetRow method..."
   hData := oCustomers:GetRow()
   ? "Current customer data:"
   HB_HGetAll(hData, .T.)
   ?

   // Test SetValue method
   ? "Testing SetValue method..."
   IF oCustomers:SetValue("PHONE", "555-0111")
      ? "Phone updated successfully"
   ELSE
      ? "Error updating phone:", oCustomers:GetError()
   ENDIF
   ?

   // Test Find/Seek methods
   ? "Testing Find/Seek methods..."
   IF oProducts:Find(2)
      ? "Product found:", oProducts:GetValue("NAME")
   ELSE
      ? "Product not found"
   ENDIF

   IF oProducts:Seek(3)
      ? "Product found (seek):", oProducts:GetValue("NAME")
   ELSE
      ? "Product not found"
   ENDIF
   ?

   // Test Delete method
   ? "Testing Delete method..."
   oProducts:GoBottom()
   ? "Deleting product:", oProducts:GetValue("NAME")
   IF oProducts:Delete()
      ? "Product marked as deleted"
      oProducts:Close()
      oProducts:Open(.F.)
      IF oProducts:Pack()
         ? "Table packed successfully"
      ELSE
         ? "Error packing table:", oProducts:GetError()
      ENDIF
      oProducts:Close()
      oProducts:Open(.T.)

   ELSE
      ? "Error deleting product:", oProducts:GetError()
   ENDIF
   ?

   // Test RecCount method
   ? "Testing RecCount method..."
   ? "Number of customers:", oCustomers:RecCount()
   ?

   INKEY(0)
   // Test 2: ORM Relations
   ? "=== TEST 2: ORM Relations ==="

   // Create relations
   oCustProdRelation := HbORMRelation():New(oCustomers, oProducts,     "1:N", "CUST_ID",  "PROD_ID")
   oOrderRelation    := HbORMRelation():New(oOrders,    oOrderDetails, "1:N", "ORDER_ID", "ORDER_ID")

   // Test GetRelated method
   ? "Testing GetRelated method..."
   oCustomers:Find(1)
   aRelations := oCustProdRelation:GetRelated()
   ? "Products related to customer 1:", Len(aRelations)

   oOrders:Find(1)
   aRelations := oOrderRelation:GetRelated()
   ? "Details for order 1:", Len(aRelations)
   ?

   // Test AddRelated method
   ? "Testing AddRelated method..."
   oCustomers:Find(2)
   lSuccess := oCustProdRelation:AddRelated(NIL, {;
      "PROD_ID" => 56, ;
      "NAME" => "Monitor", ;
      "DESCRIPTIO" => "27-inch 4K monitor", ;
      "PRICE" => 399.99, ;
      "STOCK" => 20, ;
      "CATEGORY" => "Electronics" ;
   })

   IF lSuccess
      ? "Related product added successfully"
   ELSE
      ? "Error adding related product"
   ENDIF
   ?

   // Test RemoveRelated method
   ? "Testing RemoveRelated method..."
   oOrders:Find(1)
   lSuccess := oOrderRelation:RemoveRelated(NIL, 2)

   IF lSuccess
      ? "Order detail removed successfully"
   ELSE
      ? "Error removing order detail"
   ENDIF
   ?

   // Test GetParentByChild method
   ? "Testing GetParentByChild method..."
   hData := oOrderRelation:GetParentByChild(3)
   IF hData != NIL
      ? "Parent order for detail 3:", hData["ORDER_ID"]
   ELSE
      ? "Parent order not found"
   ENDIF
   ?

   // Test 3: ORM Validator
   ? "=== TEST 3: ORM Validator ==="

   oValidator := HbORMValidator():New()

   // Add validation rules
   oValidator:AddRule("NAME", "required", NIL, "Name is required")
   oValidator:AddRule("EMAIL", "email", NIL, "Invalid email format")
   oValidator:AddRule("PHONE", "regex", "^[0-9-]+$", "Phone can only contain numbers and dashes")
   oValidator:AddRule("REG_DATE", "date", NIL, "Invalid date")
   oValidator:AddRule("STATUS", "length", 1, "Status must be 1 character")

   // Test validation with valid data
   ? "Testing validation with valid data..."
   hData := {;
      "NAME" => "Test Customer", ;
      "EMAIL" => "test@example.com", ;
      "PHONE" => "123-4567", ;
      "REG_DATE" => Date(), ;
      "STATUS" => "A" ;
   }

   IF oValidator:Validate(hData)
      ? "Validation passed"
   ELSE
      ? "Validation failed:"
      AEval(oValidator:GetErrors(), {|aError| QOut(aError[1] + ": " + aError[2])})
   ENDIF
   ?

   // Test validation with invalid data
   ? "Testing validation with invalid data..."
   hData := {;
      "NAME" => "", ;
      "EMAIL" => "invalid", ;
      "PHONE" => "abc", ;
      "REG_DATE" => "not a date", ;
      "STATUS" => "Active" ;
   }

   IF oValidator:Validate(hData)
      ? "Validation passed (unexpected)"
   ELSE
      ? "Validation failed as expected:"
      AEval(oValidator:GetErrors(), {|aError| QOut(aError[1] + ": " + aError[2])})
   ENDIF
   ?
   INKEY(0)
   // Test 4: ORM Query Builder
   ? "=== TEST 4: ORM Query Builder ==="

   oQuery := HbORMQuery():New(oCustomers)

   // Build a complex query
   oQuery:Select({"CUST_ID", "NAME", "EMAIL","REG_DATE"}) ;
      :Where("STATUS", "=", "A") ;
      :WhereBetween("REG_DATE", Date() - 250, Date()+150) ;
      :OrderBy("NAME") ;
      :Limit(10)

   ? "Generated SQL:", oQuery:ToSQL()
   ?

   // Execute the query
   ? "Executing query..."
   aResults := oQuery:Get()

   ? "Query results (" + AllTrim(Str(Len(aResults))) + " records):"
   FOR i := 1 TO Len(aResults)
      ? "Record", i, ":", alltrim( aResults[i]["NAME"]), " < " + alltrim(aResults[i]["EMAIL"]) + " > " + DTOC(aResults[i]["REG_DATE"])
   NEXT
   ?

   // Test Count method
   nCount := oQuery:Count()
   ? "Number of active customers:", nCount
   ?

   // Test First method
   hData := oQuery:First()
   IF hData != NIL
      ? "First active customer:", hData["NAME"]
   ELSE
      ? "No active customers found"
   ENDIF
   ?

   // Clean up
   ? "Cleaning up..."
   oCustomers:Close()
   oProducts:Close()
   oOrders:Close()
   oOrderDetails:Close()

   ? "=== Test completed successfully ==="
   Inkey( 0 )

RETURN

/**
 * Creates test tables if they don't exist
 * @procedure CreateTestTables
 * @description Initializes the database structure by creating required tables
 *              with appropriate fields and indexes if they don't already exist
 */
STATIC PROCEDURE CreateTestTables()

   LOCAL oCustomers, oProducts, oOrders, oOrderDetails
   LOCAL aCustStruct, aProdStruct, aOrderStruct, aDetailStruct
   LOCAL lTablesExist := .T.

   // Define table structures
   aCustStruct := {;
      {"CUST_ID",  "N",  10, 0}, ;
      {"NAME",     "C",  50, 0}, ;
      {"EMAIL",    "C", 100, 0}, ;
      {"PHONE",    "C",  20, 0}, ;
      {"ADDRESS",  "C", 200, 0}, ;
      {"REG_DATE", "D",   8, 0}, ;
      {"STATUS",   "C",   1, 0}  ;
   }

   aProdStruct := {;
      {"PROD_ID",     "N",  10, 0}, ;
      {"NAME",        "C",  50, 0}, ;
      {"DESCRIPTION", "C", 200, 0}, ;
      {"PRICE",       "N",  12, 2}, ;
      {"STOCK",       "N",   6, 0}, ;
      {"CATEGORY",    "C",  30, 0}  ;
   }

   aOrderStruct := {;
      {"ORDER_ID",   "N", 10, 0}, ;
      {"CUST_ID",    "N", 10, 0}, ;
      {"ORDER_DATE", "D",  8, 0}, ;
      {"TOTAL",      "N", 12, 2}, ;
      {"STATUS",     "C",  1, 0}  ;
   }

   aDetailStruct := {;
      {"DETAIL_ID", "N", 10, 0}, ;
      {"ORDER_ID",  "N", 10, 0}, ;
      {"PROD_ID",   "N", 10, 0}, ;
      {"QUANTITY",  "N",  6, 0}, ;
      {"UNIT_PRICE","N", 12, 2}, ;
      {"SUBTOTAL",  "N", 12, 2}  ;
   }

   // Check if tables exist
   oCustomers    := HbORM():New("customers",,"data\")
   oProducts     := HbORM():New("products",,"data\")
   oOrders       := HbORM():New("orders",,"data\")
   oOrderDetails := HbORM():New("order_details",,"data\")

   IF !oCustomers:Exists() .OR. !oProducts:Exists() .OR. !oOrders:Exists() .OR. !oOrderDetails:Exists()
      lTablesExist := .F.
   ENDIF

   // Create tables if they don't exist
   IF !lTablesExist
      ? "Creating test tables..."

      IF oCustomers:Create(aCustStruct)
         oCustomers:AddIndex("CUST_ID", "CUST_ID", , .T.)
         ? "Customers table created"
      ELSE
         ? "Error creating customers table:", oCustomers:GetError()
      ENDIF

      IF oProducts:Create(aProdStruct)
         oProducts:AddIndex("PROD_ID", "PROD_ID", , .T.)
         oProducts:AddIndex("CATEGORY", "CATEGORY+NAME")
         ? "Products table created"
      ELSE
         ? "Error creating products table:", oProducts:GetError()
      ENDIF

      IF oOrders:Create(aOrderStruct)
         oOrders:AddIndex("ORDER_ID", "ORDER_ID", , .T.)
         oOrders:AddIndex("CUST_ID", "CUST_ID")
         ? "Orders table created"
      ELSE
         ? "Error creating orders table:", oOrders:GetError()
      ENDIF

      IF oOrderDetails:Create(aDetailStruct)
         oOrderDetails:AddIndex("DETAIL_ID", "DETAIL_ID", , .T.)
         oOrderDetails:AddIndex("ORDER_ID", "ORDER_ID")
         oOrderDetails:AddIndex("PROD_ID", "PROD_ID")
         ? "Order details table created"
      ELSE
         ? "Error creating order details table:", oOrderDetails:GetError()
      ENDIF
   ELSE
      ? "Test tables already exist"
   ENDIF

   // Close tables if they were opened
   IF oCustomers:lOpen
      oCustomers:Close()
   ENDIF

   IF oProducts:lOpen
      oProducts:Close()
   ENDIF

   IF oOrders:lOpen
      oOrders:Close()
   ENDIF

   IF oOrderDetails:lOpen
      oOrderDetails:Close()
   ENDIF

   Inkey( 0 )

RETURN

/**
 * Displays all key-value pairs from a hash
 * @function HB_HGetAll
 * @param hHash The hash to display
 * @param lShowTypes Logical to show data types (.T. or .F.)
 * @return cOutput Formatted string with hash contents
 * @description Utility function to display the contents of a hash array,
 *              optionally showing the data type of each value
 */
FUNCTION HB_HGetAll(hHash, lShowTypes)
   LOCAL xKey, xValue
   LOCAL cType
   LOCAL cOutput := ""

   // Default parameter
   IIF( lShowTypes == NIL, .F., lShowTypes  )

   IF ValType(hHash) != "H"
      ? "Error: Parameter is not a hash"
      RETURN ""
   ENDIF

   IF Empty(hHash)
      ? "Hash is empty"
      RETURN ""
   ENDIF

   ? "Hash contents:"
   ? Replicate("-", 60)

   FOR EACH xKey IN hb_HKeys(hHash)
      xValue := hHash[xKey]

      IF lShowTypes
         cType := ValType(xValue)
         DO CASE
            CASE cType == "C"
               cOutput := "' " + Alltrim(xValue) + " ' (Character)"
            CASE cType == "N"
               cOutput := AllTrim(Str(xValue)) + " (Numeric)"
            CASE cType == "D"
               cOutput := DToC(xValue) + " (Date)"
            CASE cType == "L"
               cOutput := IIf(xValue, ".T.", ".F.") + " (Logical)"
            CASE cType == "A"
               cOutput := "Array(" + AllTrim(Str(Len(xValue))) + ")"
            CASE cType == "H"
               cOutput := "Hash(" + AllTrim(Str(Len(hb_HKeys(xValue)))) + ")"
            CASE cType == "B"
               cOutput := "{||...} (Code block)"
            CASE xValue == NIL
               cOutput := "NIL"
            OTHERWISE
               cOutput := "Unknown type"
         ENDCASE
      ELSE
         DO CASE
            CASE ValType(xValue) == "C"
               cOutput := "'" + xValue + "'"
            CASE ValType(xValue) == "N"
               cOutput := AllTrim(Str(xValue))
            CASE ValType(xValue) == "D"
               cOutput := DToC(xValue)
            CASE ValType(xValue) == "L"
               cOutput := IIf(xValue, ".T.", ".F.")
            CASE ValType(xValue) == "A"
               cOutput := "Array(" + AllTrim(Str(Len(xValue))) + ")"
            CASE ValType(xValue) == "H"
               cOutput := "Hash(" + AllTrim(Str(Len(hb_HKeys(xValue)))) + ")"
            CASE ValType(xValue) == "B"
               cOutput := "{||...}"
            CASE xValue == NIL
               cOutput := "NIL"
            OTHERWISE
               cOutput := "Unknown"
         ENDCASE
      ENDIF

      ? PadR(xKey, 20), "=>", cOutput
   NEXT

   ? Replicate("-", 60)

RETURN ""
