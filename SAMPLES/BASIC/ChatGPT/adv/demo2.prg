#include "minigui.ch"

function Main()

   SET DATE FORMAT "yyyy-mm-dd"

   DEFINE WINDOW WinMain ;
      AT 0,0 ;
      WIDTH 800 ;
      HEIGHT 600 ;
      TITLE "Inventory Management System" ;
      MAIN ;
      ON INIT OnInit()

   @ 20,20 TAB TabMain ;
      WIDTH 750 ;
      HEIGHT 500

      // Products Tab
      PAGE 'Products'

      @ 40,20 GRID GrdProducts ;
         WIDTH 700 ;
         HEIGHT 400 ;
         HEADERS { "Product ID", "Product Name", "Quantity", "Price" } ;
         WIDTHS { 100, 250, 100, 100 } ;
         ITEMS {{ 1, "Product A", 50, 10.00 }, { 2, "Product B", 30, 15.00 }} ;
         JUSTIFY { 1, 0, 1, 1 }

      @ 460,620 BUTTON BtnAddProduct CAPTION "Add" ACTION AddProduct("GrdProducts")
      @ 460,520 BUTTON BtnDeleteProduct CAPTION "Delete" ACTION DeleteProduct("GrdProducts")

      END PAGE

      // Suppliers Tab
      PAGE 'Suppliers'

      @ 40,20 GRID GrdSuppliers ;
         WIDTH 700 ;
         HEIGHT 400 ;
         HEADERS { "Supplier ID", "Supplier Name", "Contact" } ;
         WIDTHS { 100, 250, 150 } ;
         ITEMS {{ 1, "Supplier A", "123-456-7890" }, { 2, "Supplier B", "098-765-4321" }} ;
         JUSTIFY { 1, 0, 0 }

      @ 460,620 BUTTON BtnAddSupplier CAPTION "Add" ACTION AddSupplier("GrdSuppliers")
      @ 460,520 BUTTON BtnDeleteSupplier CAPTION "Delete" ACTION DeleteSupplier("GrdSuppliers")

      END PAGE

      // Orders Tab
      PAGE 'Orders'

      @ 40,20 GRID GrdOrders ;
         WIDTH 700 ;
         HEIGHT 400 ;
         HEADERS { "Order ID", "Product ID", "Quantity", "Date" } ;
         WIDTHS { 100, 100, 100, 150 } ;
         ITEMS {{ 1, 1, 10, "2024-07-20" }, { 2, 2, 5, "2024-07-21" }} ;
         JUSTIFY { 1, 1, 1, 0 }

      @ 460,620 BUTTON BtnAddOrder CAPTION "Add" ACTION AddOrder("GrdOrders")
      @ 460,520 BUTTON BtnDeleteOrder CAPTION "Delete" ACTION DeleteOrder("GrdOrders")

      END PAGE

   END TAB

   // Progress Bar
   @ 10,320 PROGRESSBAR PrgLoading ;
      WIDTH 450 ;
      HEIGHT 25 ;
      RANGE 0,100 ;
      VALUE 0

   END WINDOW

   ACTIVATE WINDOW WinMain

return nil

// ============================================================================
// Function: OnInit()
// Purpose: Initializes the window and simulates loading data using a progress bar.
// ============================================================================
function OnInit()
   // Simulate data loading
   LoadData("PrgLoading")
return nil

// ============================================================================
// Function: LoadData(PrgLoading)
// Purpose: Simulates data loading with a progress bar.
// ============================================================================
function LoadData(PrgLoading)
   LOCAL i
   FOR i := 10 TO 100 STEP 10
      WinMain.(PrgLoading).Value := i
      Sleep(1) // Simulate loading
   NEXT
   WinMain.(PrgLoading).Hide()
return nil

// ============================================================================
// Procedure: Sleep(n)
// Purpose: Pauses execution for a given number of seconds.
// ============================================================================
PROCEDURE Sleep( n )
   n += Seconds()
   WHILE Seconds() < n
      DO EVENTS
   ENDDO
RETURN

// ============================================================================
// Function: AddProduct(GrdProducts)
// Purpose: Adds a product to the Products grid.
// ============================================================================
function AddProduct(GrdProducts)
   LOCAL aNewProduct, nNewID

   // Simulate input dialog for product details
   nNewID := WinMain.(GrdProducts).ItemCount + 1
   aNewProduct := { nNewID, InputBox("Enter product name:"), Val(InputBox("Enter quantity:")), Val(InputBox("Enter price:")) }

   // Check if user provided all inputs
   IF ! Empty(aNewProduct[2]) .AND. aNewProduct[3] > 0 .AND. aNewProduct[4] > 0
      aNewProduct[4] := uCharToVal(Str(aNewProduct[4], 10, 2), "N")
      WinMain.(GrdProducts).AddItem(aNewProduct)
      WinMain.(GrdProducts).Value := WinMain.(GrdProducts).ItemCount
      WinMain.(GrdProducts).Refresh()
   ELSE
      MsgInfo("Invalid input!")
   ENDIF
return nil

// ============================================================================
// Function: DeleteProduct(GrdProducts)
// Purpose: Deletes a selected product from the Products grid.
// ============================================================================
function DeleteProduct(GrdProducts)
   LOCAL nRow := WinMain.(GrdProducts).Value
   IF nRow > 0
      WinMain.(GrdProducts).DeleteItem(nRow)
      WinMain.(GrdProducts).Refresh()
   ELSE
      MsgInfo("No row selected!")
   ENDIF
return nil

// ============================================================================
// Function: AddSupplier(GrdSuppliers)
// Purpose: Adds a supplier to the Suppliers grid.
// ============================================================================
function AddSupplier(GrdSuppliers)
   LOCAL aNewSupplier, nNewID

   // Simulate input dialog for supplier details
   nNewID := WinMain.(GrdSuppliers).ItemCount + 1
   aNewSupplier := { nNewID, InputBox("Enter supplier name:"), InputBox("Enter contact:") }

   // Check if user provided all inputs
   IF ! Empty(aNewSupplier[2]) .AND. ! Empty(aNewSupplier[3])
      WinMain.(GrdSuppliers).AddItem(aNewSupplier)
      WinMain.(GrdSuppliers).Value := WinMain.(GrdSuppliers).ItemCount
      WinMain.(GrdSuppliers).Refresh()
   ELSE
      MsgInfo("Invalid input!")
   ENDIF
return nil

// ============================================================================
// Function: DeleteSupplier(GrdSuppliers)
// Purpose: Deletes a selected supplier from the Suppliers grid.
// ============================================================================
function DeleteSupplier(GrdSuppliers)
   LOCAL nRow := WinMain.(GrdSuppliers).Value
   IF nRow > 0
      WinMain.(GrdSuppliers).DeleteItem(nRow)
      WinMain.(GrdSuppliers).Refresh()
   ELSE
      MsgInfo("No row selected!")
   ENDIF
return nil

// ============================================================================
// Function: AddOrder(GrdOrders)
// Purpose: Adds an order to the Orders grid.
// ============================================================================
function AddOrder(GrdOrders)
   LOCAL aNewOrder, nNewID, nProductID

   // Simulate input dialog for order details
   nNewID := WinMain.(GrdOrders).ItemCount + 1
   nProductID := Val(InputBox("Enter product ID:"))
   aNewOrder := { nNewID, nProductID, Val(InputBox("Enter quantity:")), Date() }

   // Check if user provided all inputs
   IF nProductID > 0 .AND. aNewOrder[3] > 0
      WinMain.(GrdOrders).AddItem(aNewOrder)
      WinMain.(GrdOrders).Value := WinMain.(GrdOrders).ItemCount
      WinMain.(GrdOrders).Refresh()
   ELSE
      MsgInfo("Invalid input!")
   ENDIF
return nil

// ============================================================================
// Function: DeleteOrder(GrdOrders)
// Purpose: Deletes a selected order from the Orders grid.
// ============================================================================
function DeleteOrder(GrdOrders)
   LOCAL nRow := WinMain.(GrdOrders).Value
   IF nRow > 0
      WinMain.(GrdOrders).DeleteItem(nRow)
      WinMain.(GrdOrders).Refresh()
   ELSE
      MsgInfo("No row selected!")
   ENDIF
return nil
