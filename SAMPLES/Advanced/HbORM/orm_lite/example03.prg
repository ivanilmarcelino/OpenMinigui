/*

 BadaSystem
 Program       : HbORM - Harbour Object-Relational Mapping
 Module        : Test suite for HbORM relationship management
 Compiler      : MINIGUI - Harbour Win32 GUI
 Compiler-C    : BCC 32 bit
 Author        : Marcos Jarrin
 Email         : marvijarrin@gmail.com
 Date          : 10/07/2025
 Update        : 08/08/2025
 Rev           : 0.1
 Description: Functions to extend HbORM

*/

#include "hbclass.ch"
#include "minigui.ch"
#include "fileio.ch"

REQUEST DBFCDX

/**
 * Test suite for HbORM relationship management
 * class HbORMTest
 * description Provides comprehensive testing for 1:1, 1:N, and N:M relationships
 */
CREATE CLASS HbORMTest
   EXPORTED:
      VAR cLogFile   AS STRING   // Path to the log file
      VAR nStartTime AS NUMERIC  // Start time for performance metrics
      VAR aResults   AS ARRAY    // Array of test results
      
      METHOD New() CONSTRUCTOR
      METHOD RunAllTests()
      METHOD TestOneToOne()
      METHOD TestOneToMany()
      METHOD TestManyToMany()
      METHOD LogMessage(cMessage, lError)
      METHOD SaveResults()

   PROTECTED:
      METHOD CreateTables()
      METHOD PopulateData()
      METHOD ValidateResults(cTestName, lResult, cExpected, cActual)
ENDCLASS

/**
 * Constructor method
 * method New
 * returns {HbORMTest} New test instance
 */
METHOD New() CLASS HbORMTest
   ::cLogFile := "test_orm.log"
   ::aResults := {}
   ::nStartTime := 0
   
   // Initialize log file
   IF FILE(::cLogFile)
      FERASE(::cLogFile)
   ENDIF
   
   RETURN Self

/**
 * Runs all relationship tests
 * method RunAllTests
 * returns {Boolean} True if all tests pass, False otherwise
 */
METHOD RunAllTests() CLASS HbORMTest
   LOCAL lAllPassed := .T.
   
   ::LogMessage("Starting HbORM Test Suite", .F.)
   ::CreateTables()
   ::PopulateData()


   lAllPassed := lAllPassed .AND. ::TestOneToOne()
   lAllPassed := lAllPassed .AND. ::TestOneToMany()
   lAllPassed := .T.
   lAllPassed := lAllPassed .AND. ::TestManyToMany()
   
   ::SaveResults()
   ::LogMessage("Test Suite Completed. All tests passed: " + IIF(lAllPassed, "Yes", "No"), .F.)
   
   RETURN lAllPassed

/**
 * Creates test tables for all relationship types
 * method CreateTables
 * private
 */
METHOD CreateTables() CLASS HbORMTest
   LOCAL oORM
   LOCAL aStructure
   LOCAL cPath := "data"
   
   ::LogMessage("Creating test tables...", .F.)
   
    /**
     * Creates the data directory if it does not exist.
     */
    IF !isdir( cPath)
         dirmake( cPath )
    ENDIF

   // 1:1 - User and Profile
   //oORM := HbORM():New("user","user","data\")
   oORM := HbORM():New("user")
   aStructure := { ;
      {"id", "C", 10, 0}, ;
      {"name", "C", 50, 0} ;
   }
   oORM:Create(aStructure)
   oORM:AddIndex("ID", "id", , .T.)
   oORM:Close()
   
   oORM := HbORM():New("profile","profile","data\")
   aStructure := { ;
      {"id", "C", 10, 0}, ;
      {"user_id", "C", 10, 0}, ;
      {"bio", "M", 10, 0} ;
   }
   oORM:Create(aStructure)
   oORM:AddIndex("ID", "id", , .T.)
   oORM:AddIndex("USER_ID", "user_id")
   oORM:Close()
   
   // 1:N - Customer and Orders
   oORM := HbORM():New("customer","customer","data\")
   aStructure := { ;
      {"id", "C", 10, 0}, ;
      {"name", "C", 50, 0}, ;
      {"active", "L", 1, 0} ;
   }
   oORM:Create(aStructure)
   oORM:AddIndex("ID", "id", , .T.)
   oORM:Close()
   
   oORM := HbORM():New("order","order","data\")
   aStructure := { ;
      {"id", "C", 10, 0}, ;
      {"CUSTOMERID", "C", 10, 0}, ;
      {"order_date", "D", 8, 0}, ;
      {"amount", "N", 10, 2} ;
   }
   oORM:Create(aStructure)
   oORM:AddIndex("ID", "id", , .T.)
   oORM:AddIndex("CUSTOMERID", "CUSTOMERID")
   oORM:Close()
   
   // N:M - Product and Tag
   oORM := HbORM():New("product","product","data\")
   aStructure := { ;
      {"id", "C", 10, 0}, ;
      {"name", "C", 50, 0}, ;
      {"price", "N", 10, 2} ;
   }
   oORM:Create(aStructure)
   oORM:AddIndex("ID", "id", , .T.)
   oORM:Close()
   
   oORM := HbORM():New("tag","tag","data\")
   aStructure := { ;
      {"id", "C", 10, 0}, ;
      {"name", "C", 50, 0} ;
   }
   oORM:Create(aStructure)
   oORM:AddIndex("ID", "id", , .T.)
   oORM:Close()
   
   oORM := HbORM():New("product_tag","product_tag","data\")
   aStructure := { ;
      {"product_id", "C", 10, 0}, ;
      {"tag_id", "C", 10, 0} ;
   }
   oORM:Create(aStructure)
   oORM:AddIndex("PRODUCT_TAG", "product_id+tag_id", , .T.)
   oORM:Close()
   
   ::LogMessage("Tables created successfully", .F.)
   
   RETURN NIL

/**
 * Populates test data for all tables
 * method PopulateData
 * private
 */
METHOD PopulateData() CLASS HbORMTest
   LOCAL oUser, oProfile, oCustomer, oOrder, oProduct, oTag, oProductTag
   LOCAL i
   LOCAL hData := {=>}
   
   ::LogMessage("Populating test data...", .F.)
   
   oUser       := HbORM():New("user","user","data\")
   oProfile    := HbORM():New("profile","profile","data\")
   oCustomer   := HbORM():New("customer","customer","data\")
   oOrder      := HbORM():New("order","order","data\")
   oProduct    := HbORM():New("product","product","data\")
   oTag        := HbORM():New("tag","tag","data\")
   oProductTag := HbORM():New("product_tag","product_tag","data\")

   oUser:Open()
   oProfile:Open()
   oCustomer:Open()
   oOrder:Open()
   oProduct:Open()
   oTag:Open()
   oProductTag:Open()

   // Populate Users and Profiles (1:1)
   FOR i := 1 TO 1000
      hData := { ;
         "id" => StrZero(i, 10), ;
         "name" => "User " + StrZero(i, 4) ;
      }
      oUser:Insert(hData)

      hData := { ;
         "id" => StrZero(i, 10), ;
         "user_id" => StrZero(i, 10), ;
         "bio" => "Bio for user " + StrZero(i, 4) ;
      }
      oProfile:Insert(hData)
   NEXT
   
   // Populate Customers and Orders (1:N)
   FOR i := 1 TO 1000
      hData := { ;
         "id" => StrZero(i, 10), ;
         "name" => "Customer " + StrZero(i, 4), ;
         "active" => (i % 2 == 0) ;
      }
      oCustomer:Insert(hData)
      
      // Each customer has 2 orders
      hData := { ;
         "id" => StrZero(i * 2 - 1, 10), ;
         "CUSTOMERID" => StrZero(i, 10), ;
         "order_date" => Date() - i, ;
         "amount" => i * 100.50 ;
      }
      oOrder:Insert(hData)
      
      hData := { ;
         "id" => StrZero(i * 2, 10), ;
         "CUSTOMERID" => StrZero(i, 10), ;
         "order_date" => Date() - i + 1, ;
         "amount" => i * 200.75 ;
      }
      oOrder:Insert(hData)
   NEXT
   
   // Populate Products, Tags, and Product-Tag (N:M)
   FOR i := 1 TO 1000
      hData := { ;
         "id" => StrZero(i, 10), ;
         "name" => "Product " + StrZero(i, 4), ;
         "price" => i * 10.99 ;
      }
      oProduct:Insert(hData)
      
      hData := { ;
         "id" => StrZero(i, 10), ;
         "name" => "Tag " + StrZero(i, 4) ;
      }
      oTag:Insert(hData)
      
      // Create 2 relationships per product
      hData := { ;
         "product_id" => StrZero(i, 10), ;
         "tag_id" => StrZero(i, 10) ;
      }
      oProductTag:Insert(hData)
      
      hData := { ;
         "product_id" => StrZero(i, 10), ;
         "tag_id" => StrZero(Min(i + 1, 1000), 10) ;
      }
      oProductTag:Insert(hData)
   NEXT
   
   oUser:Close()
   oProfile:Close()
   oCustomer:Close()
   oOrder:Close()
   oProduct:Close()
   oTag:Close()
   oProductTag:Close()
   
   ::LogMessage("Data population completed", .F.)
   
   RETURN NIL

/**
 * Tests 1:1 relationship (User-Profile)
 * method TestOneToOne
 * returns {Boolean} True if all tests pass, False otherwise
 */
METHOD TestOneToOne() CLASS HbORMTest
   LOCAL lPassed     := .T.
   LOCAL oUser       := HbORM():New("user","user","data\")
   LOCAL oProfile    := HbORM():New("profile","profile","data\")
   LOCAL oRelation   := HbORMRelation():New(oUser, oProfile, "1:1", "id", "user_id")
   LOCAL oValidator  := HbORMValidator():New()
   LOCAL hData, aRelated, hResult
   
   oUser:Open()
   oProfile:Open()

   ::LogMessage("Testing 1:1 relationship (User-Profile)...", .F.)
   
   // Test 1: Try inserting profile without user
   hData := {"id" => StrZero(9999, 10), "user_id" => "INVALID", "bio" => "Invalid bio"}
   oValidator:AddRule("user_id", "custom", { |x| oUser:Find(x) }, "Invalid user reference")
   lPassed := lPassed .AND. !oValidator:Validate(hData)
   ::ValidateResults("1:1 Invalid Profile Insert", !oValidator:Validate(hData), "Validation failed", IIF(oValidator:HasErrors(), oValidator:GetErrors()[1][2], "No errors"))
   
   // Test 2: Valid profile insert
   hData := {"id" => StrZero(1001, 10), "user_id" => StrZero(1, 10), "bio" => "New bio"}
   oValidator:ClearErrors()
   oValidator:AddRule("user_id", "custom", { |x| oUser:Find(x) }, "Invalid user reference")
   lPassed := lPassed .AND. oValidator:Validate(hData) .AND. oProfile:Insert(hData)
   ::ValidateResults("1:1 Valid Profile Insert", oProfile:Insert(hData), "Success", IIF(oProfile:GetError() == "", "Success", oProfile:GetError()))
   
   // Test 3: Get related profile
   aRelated := oRelation:GetRelated(StrZero(1, 10))
   lPassed := lPassed .AND. Len(aRelated) == 1
   ::ValidateResults("1:1 Get Related", Len(aRelated) == 1, "1 record", AllTrim(Str(Len(aRelated))))
   
   // Test 4: Update related profile
   hData := {"bio" => "Updated bio"}
   oProfile:Find(StrZero(1, 10))
   lPassed := lPassed .AND. oProfile:Update(hData)
   ::ValidateResults("1:1 Update Profile", oProfile:Update(hData), "Success", IIF(oProfile:GetError() == "", "Success", oProfile:GetError()))
   
   oUser:Close()
   oProfile:Close()
   
   ::LogMessage("1:1 Tests Completed. Passed: " + IIF(lPassed, "Yes", "No"), .F.)
   RETURN lPassed

/**
 * Tests 1:N relationship (Customer-Order)
 * method TestOneToMany
 * returns {Boolean} True if all tests pass, False otherwise
 */
METHOD TestOneToMany() CLASS HbORMTest
   LOCAL lPassed   := .T.
   LOCAL oCustomer := HbORM():New("customer","customer","data\")
   LOCAL oOrder    := HbORM():New("order","order","data\")
   LOCAL oRelation := HbORMRelation():New(oCustomer, oOrder, "1:N", "id", "CUSTOMERID")
   LOCAL oQuery    := HbORMQuery():New(oOrder)
   LOCAL hData, aRelated, nCount

   oCustomer:Open()
   oOrder:Open()
   
   ::LogMessage("Testing 1:N relationship (Customer-Order)...", .F.)
   

   // Test 1: Add new order
   hData := {"id" => StrZero(2001, 10), "CUSTOMERID" => StrZero(2, 10), "order_date" => Date(), "amount" => 999.99}
   lPassed := lPassed .AND. oOrder:Insert(hData)
   ::ValidateResults("1:N Insert Order", oOrder:Insert(hData), "Success", IIF(oOrder:GetError() == "", "Success", oOrder:GetError()))
   
   // Test 2: Query orders for customer
   nCount := oQuery:Where("CUSTOMERID", "=", StrZero(2, 10)):Count()
   lPassed := lPassed .AND. nCount >= 2
   ::ValidateResults("1:N Query Orders", nCount >= 2, ">= 2 records", AllTrim(Str(nCount)))
   
   oCustomer:Close()
   oOrder:Close()
   
   ::LogMessage("1:N Tests Completed. Passed: " + IIF(lPassed, "Yes", "No"), .F.)
   RETURN lPassed

/**
 * Tests N:M relationship (Product-Tag)
 * method TestManyToMany
 * returns {Boolean} True if all tests pass, False otherwise
 */
METHOD TestManyToMany() CLASS HbORMTest
   LOCAL lPassed     := .T.
   LOCAL oProduct    := HbORM():New("product","product","data\")
   LOCAL oTag        := HbORM():New("tag","tag","data\")
   LOCAL oProductTag := HbORM():New("product_tag","product_tag","data\")
   LOCAL oRelation   := HbORMRelation():New(oProduct, oTag, "N:M", "id", "id", "product_tag", "product_id", "tag_id")
   LOCAL oQuery      := HbORMQuery():New(oProduct)
   LOCAL hData, aRelated, nCount

   oProduct:Open()
   oTag:Open()
   oProductTag:Open()
   
   ::LogMessage("Testing N:M relationship (Product-Tag)...", .F.)
   
   // Test 1: Add new relationship
   hData := {"id" => StrZero(1001, 10), "name" => "New Tag"}
   oTag:Insert(hData)
   lPassed := lPassed .AND. oRelation:AddRelated(StrZero(1, 10), {"id" => StrZero(1001, 10), "name" => "New Tag"})
   ::ValidateResults("N:M Add Relationship", oRelation:AddRelated(StrZero(1, 10), {"id" => StrZero(1001, 10), "name" => "New Tag"}), "Success", IIF(oRelation:oChild:GetError() == "", "Success", oRelation:oChild:GetError()))
   
   ::LogMessage("N:M Tests Completed. Passed: " + IIF(lPassed, "Yes", "No"), .F.)
   RETURN lPassed

/**
 * Logs a message to the log file
 * method LogMessage
 * param {String} cMessage - Message to log
 * param {Boolean} lError - Whether this is an error message
 */
METHOD LogMessage(cMessage, lError) CLASS HbORMTest
   LOCAL cLine := DToC(Date()) + " " + Time() + " [" + IIF(lError, "ERROR", "INFO") + "] " + ;
                 cMessage + " [" + __FILE__ + ":" + AllTrim(Str(__LINE__)) + "]" + hb_eol()
   LOCAL nHandle
   LOCAL cDataOld  := ""
   local cNameFile := ::cLogFile
   LOCAL cPathFull := ""
   local nByte

    cPathFull := CurDrive()+":\"+CurDir()+"\"
   IF FILE(cPathFull + ::cLogFile)
      nHandle := FOpen( ::cLogFile, FO_READWRITE)
   ELSE
      nHandle := FCREATE(::cLogFile, FC_NORMAL)
   ENDIF
   cDataOld := memoread(cPathFull+ ::cLogFile)

   cDataOld += cLine
   FWRITE(nHandle, cDataOld )
   FCLOSE(nHandle)
   ? cLine
   
   RETURN NIL

/**
 * Validates test results and stores them
 * method ValidateResults
 * param {String} cTestName - Name of the test
 * param {Boolean} lResult - Test result
 * param {String} cExpected - Expected outcome
 * param {String} cActual - Actual outcome
 */
METHOD ValidateResults(cTestName, lResult, cExpected, cActual) CLASS HbORMTest
   LOCAL cStatus := IIF(lResult, "OK", "FAIL")
   AAdd(::aResults, {cTestName, cStatus, cExpected, cActual})
   ::LogMessage("Test: " + cTestName + " - " + cStatus + " (Expected: " + cExpected + ", Actual: " + cActual + ")", !lResult)
   RETURN NIL

/**
 * Saves test results to a report file
 * method SaveResults
 */
METHOD SaveResults() CLASS HbORMTest
   LOCAL nHandle := FCREATE("test_orm_report.txt", FC_NORMAL)
   LOCAL aResult, nTotal := 0, nPassed := 0, nTotalTime := 0
   
   FWRITE(nHandle, "HbORM Test Report" + hb_eol())
   FWRITE(nHandle, "=================" + hb_eol() + hb_eol())
   FWRITE(nHandle, "Test Name                    | Status | Expected            | Actual" + hb_eol())
   FWRITE(nHandle, "-----------------------------|--------|---------------------|----------------" + hb_eol())
   
   FOR EACH aResult IN ::aResults
      FWRITE(nHandle, PadR(aResult[1], 28) + " | " + PadC(aResult[2], 6) + " | " + ;
             PadR(aResult[3], 19) + " | " + aResult[4] + hb_eol())
      nTotal++
      IF aResult[2] == "OK"
         nPassed++
      ENDIF
   NEXT
   
   FWRITE(nHandle, hb_eol() + "Summary:" + hb_eol())
   FWRITE(nHandle, "Total Tests: " + AllTrim(Str(nTotal)) + hb_eol())
   FWRITE(nHandle, "Passed: " + AllTrim(Str(nPassed)) + hb_eol())
   FWRITE(nHandle, "Failed: " + AllTrim(Str(nTotal - nPassed)) + hb_eol())
   FWRITE(nHandle, hb_eol() + "Optimization Recommendations:" + hb_eol())
   FWRITE(nHandle, "- Use SET RELATION TO for simple 1:1 and 1:N relationships to improve performance" + hb_eol())
   FWRITE(nHandle, "- Consider caching frequently accessed N:M relationships" + hb_eol())
   FWRITE(nHandle, "- Implement connection pooling for high-concurrency environments" + hb_eol())
   
   FCLOSE(nHandle)
   ::LogMessage("Test report saved to test_orm_report.txt", .F.)
   
   RETURN NIL

/**
 * Main function to run the test suite
 */
FUNCTION Main()
   LOCAL oTest := HbORMTest():New()
   RddSetDefault( "DBFCDX" )
   oTest:RunAllTests()
   RETURN NIL
