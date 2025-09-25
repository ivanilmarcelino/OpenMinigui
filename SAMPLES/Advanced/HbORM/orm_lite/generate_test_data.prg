/*

 BadaSystem
 Program       : HbORM - Harbour Object-Relational Mapping
 Module        : Generates test data for HbORM tables
 Compiler      : MINIGUI - Harbour Win32 GUI
 Compiler-C    : BCC 32 bit
 Author        : Marcos Jarrin
 Email         : marvijarrin@gmail.com
 Date          : 22/07/2025
 Update        : 02/08/2025
 Rev           : 0.1
 Description: Procedure to generate test data for HbORM test tables
              Generates 50+ records across all tables

*/

/**
 * Generates test data for HbORM tables
 * procedure GenerateTestData
 * description Creates realistic test data for customers, products, orders and order details
 * example
 * GenerateTestData() // Generates test data for all tables
 */
PROCEDURE GenerateTestData()

   LOCAL oCustomers, oProducts, oOrders, oOrderDetails
   LOCAL nCustId, nProdId, nOrderId, nDetailId
   LOCAL aFirstNames, aLastNames, aProductNames, aCategories
   LOCAL aStreetNames, aCities, aStates, aEmailDomains
   LOCAL i, j, nCustomerCount, nProductCount, nOrderCount, nMaxOrders
   LOCAL nOrderDetailCount, nMaxDetailsPerOrder
   LOCAL nTotal, nUnitPrice, nQuantity, nSubtotal
   LOCAL dOrderDate, hOrder, aOrderDetails
   LOCAL cPhone, cEmail, cAddress, cName
   LOCAL lSuccess := .T.

   // Open tables in exclusive mode
   oCustomers    := HbORM():New("customers", "cust","data\")
   oProducts     := HbORM():New("products", "prod","data\")
   oOrders       := HbORM():New("orders", "ord","data\")
   oOrderDetails := HbORM():New("order_details", "ord_det","data\")

   IF !oCustomers:Open(.F.) .OR. !oProducts:Open(.F.) .OR. !oOrders:Open(.F.) .OR. !oOrderDetails:Open(.F.)
      ? "Error opening tables:", oCustomers:GetError(), oProducts:GetError(), oOrders:GetError(), oOrderDetails:GetError()
      RETURN
   ENDIF

   // Clear existing data
   ? "Clearing existing data..."
   oCustomers:Zap()
   oProducts:Zap()
   oOrders:Zap()
   oOrderDetails:Zap()

   /**
    * Data pools for random generation
    * var {Array} aFirstNames - Common first names
    * var {Array} aLastNames - Common last names
    * var {Array} aProductNames - Product names
    * var {Array} aCategories - Product categories
    * var {Array} aStreetNames - Street names
    * var {Array} aCities - City names
    * var {Array} aStates - US state abbreviations
    * var {Array} aEmailDomains - Common email domains
    */
   aFirstNames := {;
      "John", "Robert", "Michael", "William", "David", "Richard", "Joseph", "Thomas", "Charles", "Christopher",;
      "Daniel", "Matthew", "Anthony", "Donald", "Mark", "Paul", "Steven", "Andrew", "Kenneth", "Joshua",;
      "Mary", "Patricia", "Jennifer", "Linda", "Elizabeth", "Barbara", "Susan", "Jessica", "Sarah", "Karen",;
      "Nancy", "Lisa", "Margaret", "Betty", "Sandra", "Ashley", "Dorothy", "Kimberly", "Emily", "Donna" ;
   }

   aLastNames := {;
      "Smith", "Johnson", "Williams", "Brown", "Jones", "Miller", "Davis", "Garcia", "Rodriguez", "Wilson",;
      "Martinez", "Anderson", "Taylor", "Thomas", "Hernandez", "Moore", "Martin", "Jackson", "Thompson", "White",;
      "Lopez", "Lee", "Gonzalez", "Harris", "Clark", "Lewis", "Robinson", "Walker", "Perez", "Hall",;
      "Young", "Allen", "Sanchez", "Wright", "King", "Scott", "Green", "Baker", "Adams", "Nelson" ;
   }

   aProductNames := {;
      "Laptop", "Smartphone", "Tablet", "Monitor", "Keyboard", "Mouse", "Printer", "Scanner", "Router", "External HDD",;
      "SSD Drive", "USB Flash", "Headphones", "Speakers", "Webcam", "Microphone", "Projector", "Smartwatch", "Fitness Tracker",;
      "Gaming Console", "VR Headset", "Drone", "Camera", "Action Camera", "E-Reader", "Smart Speaker", "Wireless Earbuds",;
      "Bluetooth Speaker", "Power Bank", "Charger", "Cables", "Adapter", "Dock Station", "Memory Card", "Surge Protector",;
      "Desk Lamp", "Office Chair", "Desk", "Filing Cabinet", "Bookshelf", "Whiteboard", "Notebook", "Pen Set", "Stapler",;
      "Paper Shredder", "Calculator", "Calendar", "Desk Organizer", "Monitor Stand", "Foot Rest" ;
   }

   aCategories := {;
      "Electronics", "Computers", "Office", "Furniture", "Accessories", "Home", "Gadgets", "Appliances", "Tools", "Stationery" ;
   }

   aStreetNames := {;
      "Main", "Oak", "Pine", "Maple", "Cedar", "Elm", "View", "Washington", "Lake", "Hill",;
      "Park", "Highland", "Church", "Willow", "Sunset", "Ridge", "Meadow", "Spring", "Forest", "River" ;
   }

   aCities := {;
      "New York", "Los Angeles", "Chicago", "Houston", "Phoenix", "Philadelphia", "San Antonio", "San Diego", "Dallas", "San Jose",;
      "Austin", "Jacksonville", "San Francisco", "Columbus", "Indianapolis", "Fort Worth", "Charlotte", "Seattle", "Denver", "Washington" ;
   }

   aStates := {;
      "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",;
      "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",;
      "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",;
      "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",;
      "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY" ;
   }

   aEmailDomains := {;
      "gmail.com", "yahoo.com", "hotmail.com", "outlook.com", "aol.com", "icloud.com", "protonmail.com", "mail.com", "zoho.com", "yandex.com" ;
   }

   ? "Generating test data..."

   /**
    * Generate customers (50 records)
    */
   nCustomerCount := 50
   ? "Generating", nCustomerCount, "customers..."

   FOR nCustId := 1 TO nCustomerCount
      // Generate realistic customer data
      cName := aFirstNames[HB_RandomInt(1, Len(aFirstNames))] + " " + aLastNames[HB_RandomInt(1, Len(aLastNames))]
      cEmail := Lower(SubStr(aFirstNames[HB_RandomInt(1, Len(aFirstNames))], 1, 1) + Lower(aLastNames[HB_RandomInt(1, Len(aLastNames))]) + ;
                Str(HB_RandomInt(1, 99), 2) + "" + aEmailDomains[HB_RandomInt(1, Len(aEmailDomains))] )
      cPhone := Str(HB_RandomInt(200, 999), 3) + "-" + Str(HB_RandomInt(100, 999), 3) + "-" + Str(HB_RandomInt(1000, 9999), 4)
      cAddress := Str(HB_RandomInt(1, 9999), 4) + " " + aStreetNames[HB_RandomInt(1, Len(aStreetNames))] + " " + ;
                  IIf(HB_RandomInt(1, 4) > 1, "St.", IIf(HB_RandomInt(1, 2) == 1, "Ave.", "Rd."))

      lSuccess := lSuccess .AND. oCustomers:Insert({;
         "CUST_ID" => nCustId, ;
         "NAME" => cName, ;
         "EMAIL" => cEmail, ;
         "PHONE" => cPhone, ;
         "ADDRESS" => cAddress + ", " + aCities[HB_RandomInt(1, Len(aCities))] + ", " + aStates[HB_RandomInt(1, Len(aStates))] + " " + Str(HB_RandomInt(10000, 99999), 5), ;
         "REG_DATE" => Date() - HB_RandomInt(0, 365), ;
         "STATUS" => IIf(HB_RandomInt(1, 10) > 1, "A", "I") ;
      })

      IF !lSuccess
         ? "Error inserting customer", nCustId, ":", oCustomers:GetError()
         EXIT
      ENDIF
   NEXT

   IF !lSuccess
      ? "Aborting due to errors"
      RETURN
   ENDIF

   /**
    * Generate products (50 records)
    */
   nProductCount := 50
   ? "Generating", nProductCount, "products..."

   FOR nProdId := 1 TO nProductCount
      lSuccess := lSuccess .AND. oProducts:Insert({;
         "PROD_ID" => nProdId, ;
         "NAME" => aProductNames[HB_RandomInt(1, Len(aProductNames))], ;
         "DESCRIPTIO" => "High quality " + Lower(aProductNames[HB_RandomInt(1, Len(aProductNames))]) + " for all your needs", ;
         "PRICE" => HB_RandomInt(10, 2000) + (HB_RandomInt(0, 99) / 100), ;
         "STOCK" => HB_RandomInt(0, 500), ;
         "CATEGORY" => aCategories[HB_RandomInt(1, Len(aCategories))] ;
      })

      IF !lSuccess
         ? "Error inserting product", nProdId, ":", oProducts:GetError()
         EXIT
      ENDIF
   NEXT

   IF !lSuccess
      ? "Aborting due to errors"
      RETURN
   ENDIF

   /**
    * Generate orders (approximately 1-5 per customer)
    */
   nOrderCount := 0
   nOrderId := 0
   ? "Generating orders..."

   FOR nCustId := 1 TO nCustomerCount
      // Skip some customers (about 10% won't have orders)
      IF HB_RandomInt(1, 10) == 1
         LOOP
      ENDIF

      // Each customer gets 1-5 orders
      nMaxOrders := HB_RandomInt(1, 5)

      FOR i := 1 TO nMaxOrders
         nOrderId++
         nOrderCount++

         dOrderDate := Date() - HB_RandomInt(0, 365)

         // Create order header
         lSuccess := lSuccess .AND. oOrders:Insert({;
            "ORDER_ID" => nOrderId, ;
            "CUST_ID" => nCustId, ;
            "ORDER_DATE" => dOrderDate, ;
            "TOTAL" => 0, ; // Will be calculated after details are added
            "STATUS" => SubStr("PSC", HB_RandomInt(1, 3), 1) ; // P=Pending, S=Shipped, C=Canceled
         })

         IF !lSuccess
            ? "Error inserting order", nOrderId, ":", oOrders:GetError()
            EXIT
         ENDIF

         // Generate order details (1-5 items per order)
         nOrderDetailCount := 0
         nTotal := 0
         nMaxDetailsPerOrder := HB_RandomInt(1, 5)
         aOrderDetails := {}

         FOR j := 1 TO nMaxDetailsPerOrder
            nDetailId := (nOrderId * 100) + j
            nProdId := HB_RandomInt(1, nProductCount)
            nQuantity := HB_RandomInt(1, 5)

            // Get product price
            IF oProducts:Find(nProdId)
               nUnitPrice := oProducts:GetValue("PRICE")
               nSubtotal := nUnitPrice * nQuantity
               nTotal += nSubtotal

               // Add to order details
               lSuccess := lSuccess .AND. oOrderDetails:Insert({;
                  "DETAIL_ID" => nDetailId, ;
                  "ORDER_ID" => nOrderId, ;
                  "PROD_ID" => nProdId, ;
                  "QUANTITY" => nQuantity, ;
                  "UNIT_PRICE" => nUnitPrice, ;
                  "SUBTOTAL" => nSubtotal ;
               })

               IF !lSuccess
                  ? "Error inserting order detail", nDetailId, ":", oOrderDetails:GetError()
                  EXIT
               ENDIF
            ELSE
               ? "Product", nProdId, "not found for order detail"
               lSuccess := .F.
               EXIT
            ENDIF
         NEXT

         // Update order total
         IF lSuccess .AND. oOrders:Find(nOrderId)
            lSuccess := lSuccess .AND. oOrders:SetValue("TOTAL", nTotal)

            IF !lSuccess
               ? "Error updating order total for order", nOrderId, ":", oOrders:GetError()
               EXIT
            ENDIF
         ELSE
            ? "Order", nOrderId, "not found for total update"
            lSuccess := .F.
            EXIT
         ENDIF

         IF !lSuccess
            EXIT
         ENDIF
      NEXT

      IF !lSuccess
         EXIT
      ENDIF
   NEXT

   IF !lSuccess
      ? "Aborting due to errors"
      RETURN
   ENDIF

   ? "Data generation completed successfully:"
   ? "  Customers:", oCustomers:RecCount()
   ? "  Products:", oProducts:RecCount()
   ? "  Orders:", oOrders:RecCount()
   ? "  Order Details:", oOrderDetails:RecCount()

   // Close tables
   oCustomers:Close()
   oProducts:Close()
   oOrders:Close()
   oOrderDetails:Close()

RETURN
