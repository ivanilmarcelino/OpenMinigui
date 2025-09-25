/**
 * BadaSystem
 * Program       : HbORM - Harbour Object-Relational Mapping
 * Module        : HbORM Test Program
 * Compiler      : MINIGUI - Harbour Win32 GUI
 * Compiler-C    : BCC 32 bit
 * Author        : Marcos Jarrin
 * Email         : marvijarrin@gmail.com
 * Date          : 25/07/2025
 * Update        : 02/08/2025
 * Rev           : 0.1
 * Description   : Advanced example of using the ORM for Harbour
 */

#include "hbclass.ch"

REQUEST DBFCDX

/**
 * Main procedure demonstrating advanced HbORM features
 * procedure Main
 * description Demonstrates advanced ORM operations including:
 *              - Data validation
 *              - Complex queries
 *              - Relationship queries
 *              - Transactions (commented example)
 *              - Advanced search conditions
 */
PROCEDURE Main()
   LOCAL oClientes, oFacturas, oProductos, oDetalles
   LOCAL oValidator, oQuery
   LOCAL hCliente, hFactura, aFacturas
   LOCAL lValid
   LOCAL hDetalle

   RddSetDefault( "DBFCDX" )

   // Configure environment
   SET EXACT ON
   SET DELETED ON

   // Create table instances
   oClientes  := HbORM():New("clientes", "CLI", "data/")
   oFacturas  := HbORM():New("facturas", "FAC", "data/")
   oProductos := HbORM():New("productos", "PRO", "data/")
   oDetalles  := HbORM():New("detalles", "DET", "data/")

   // Create data directory if it doesn't exist
   IF !isdir("data")
        dirmake("data")
   ENDIF

   // Ensure tables exist
   EnsureTablesExist(oClientes, oFacturas, oProductos, oDetalles)

   // Open tables
   oClientes:Open()
   oFacturas:Open()
   oProductos:Open()
   oDetalles:Open()

   // Example 1: Data validation
   ? "\nExample 1: Data validation\n"

   // Create validator for clients
   oValidator := HbORMValidator():New()
   oValidator:AddRule("NOMBRE", "required", NIL, "Name is required")
   oValidator:AddRule("NOMBRE", "type", "C", "Name must be text")
   oValidator:AddRule("NOMBRE", "min", 3, "Name must be at least 3 characters")
   oValidator:AddRule("EMAIL", "email", NIL, "Invalid email")

   // Valid data
   hCliente := { ;
      "CLI_ID" => 3, ;
      "NOMBRE" => "Carlos Rodríguez", ;
      "DIRECCION" => "Plaza Mayor 789", ;
      "TELEFONO" => "555-9012", ;
      "EMAIL" => "carlos@example.com" ;
   }

   lValid := oValidator:Validate(hCliente)

   ? "Valid data validation:", IIf(lValid, "Success", "Error")

   IF !lValid
      ? "Errors:"
      AEval(oValidator:GetErrors(), {|aError| qout(" -", aError[1] + ":", aError[2]) })
   ENDIF

   // Invalid data
   hCliente := { ;
      "CLI_ID" => 4, ;
      "NOMBRE" => "AB", ;  // Too short
      "DIRECCION" => "Plaza Mayor 789", ;
      "TELEFONO" => "555-9012", ;
      "EMAIL" => "invalid-email" ;  // Invalid email
   }

   lValid := oValidator:Validate(hCliente)

   ? "\nInvalid data validation:", IIf(lValid, "Success", "Error")

   IF !lValid
      ? "Errors:"
      AEval(oValidator:GetErrors(), {|aError| qout(" -", aError[1] + ":", aError[2]) })
   ENDIF

   // Example 2: Advanced queries
   ? "\nExample 2: Advanced queries\n"

   // Create invoice query
   oQuery := HbORMQuery():New(oFacturas)

   // Select specific fields
   oQuery:Select({"FAC_ID", "CLI_ID", "FECHA", "TOTAL"})

   // Add conditions
   oQuery:Where("TOTAL", ">", 50)
   oQuery:Where("PAGADA", "=", .T.)

   // Order by date descending
   oQuery:OrderBy("FECHA", .T.)

   // Limit to 5 results
   oQuery:Limit(5)

   // Show equivalent SQL (for debugging)
   ? "Equivalent SQL:"
   ? oQuery:ToSQL()

   // Execute query
   ? "\nResults:"
   aFacturas := oQuery:Get()

   FOR EACH hFactura IN aFacturas
      ? "Invoice ID:", hFactura["FAC_ID"], "Date:", hFactura["FECHA"], "Total:", hFactura["TOTAL"]
   NEXT

   // Example 3: Relationship queries
   ? "\nExample 3: Relationship queries\n"

   // Find client with ID 1
   IF oClientes:Find(1)
      hCliente := oClientes:GetRow()
      ? "Client:", hCliente["NOMBRE"]

      // Create query for client invoices
      oQuery := HbORMQuery():New(oFacturas)
      oQuery:Where("CLI_ID", "=", hCliente["CLI_ID"])
      oQuery:OrderBy("FECHA", .T.)

      aFacturas := oQuery:Get()

      ? "Client invoices:"
      FOR EACH hFactura IN aFacturas
         ? "  Invoice ID:", hFactura["FAC_ID"], "Date:", hFactura["FECHA"], "Total:", hFactura["TOTAL"]

         // Create query for invoice details
         oQuery := HbORMQuery():New(oDetalles)
         oQuery:Where("FAC_ID", "=", hFactura["FAC_ID"])

         aDetalles := oQuery:Get()

         ? "  Details:"
         FOR EACH hDetalle IN aDetalles
            // Find product
            oProductos:Find(hDetalle["PRO_ID"])
            hProducto := oProductos:GetRow()

            ? "    Product:", hProducto["NOMBRE"], "Quantity:", hDetalle["CANTIDAD"], "Subtotal:", hDetalle["SUBTOTAL"]
         NEXT
      NEXT
   ENDIF

   inkey(0)
   // Example 4: Advanced search with multiple conditions
   ? "\nExample 4: Advanced search with multiple conditions\n"

   oQuery := HbORMQuery():New(oFacturas)
   oQuery:Where("TOTAL", ">", 50)
   oQuery:Where("FECHA", ">", Date() - 30)  // Invoices from last 30 days
   oQuery:OrWhere("PAGADA", "=", .F.)      // Or unpaid invoices

   ? "Equivalent SQL:"
   ? oQuery:ToSQL()

   ? "\nResults:"
   aFacturas := oQuery:Get()

   FOR EACH hFactura IN aFacturas
      ? "Invoice ID:", hFactura["FAC_ID"], "Date:", hFactura["FECHA"], "Total:", hFactura["TOTAL"], "Paid:", IIf(hFactura["PAGADA"], "Yes", "No")
   NEXT

   // Close tables
   oClientes:Close()
   oFacturas:Close()
   oProductos:Close()
   oDetalles:Close()

   ? "\nAdvanced example completed"
   inkey(0)

   RETURN

/**
 * Ensures required tables exist
 * @procedure EnsureTablesExist
 * @param oClientes Clients ORM instance
 * @param oFacturas Invoices ORM instance
 * @param oProductos Products ORM instance
 * @param oDetalles Invoice details ORM instance
 * @description Creates all required tables with their structure if they don't exist
 */
PROCEDURE EnsureTablesExist(oClientes, oFacturas, oProductos, oDetalles)
   // Create tables if they don't exist
   IF !oClientes:Exists()
      ? "Creating clients table..."
      oClientes:Create({ ;
         {"CLI_ID",    "N",  10, 0}, ;
         {"NOMBRE",    "C",  50, 0}, ;
         {"DIRECCION", "C", 100, 0}, ;
         {"TELEFONO",  "C",  15, 0}, ;
         {"EMAIL",     "C",  50, 0}  ;
      })
      oClientes:AddIndex("CLI_ID", "CLI_ID")
   ENDIF

   IF !oFacturas:Exists()
      ? "Creating invoices table..."
      oFacturas:Create({ ;
         {"FAC_ID",    "N", 10, 0}, ;
         {"CLI_ID",    "N", 10, 0}, ;
         {"FECHA",     "D", 8, 0}, ;
         {"TOTAL",     "N", 12, 2}, ;
         {"PAGADA",    "L", 1, 0} ;
      })
      oFacturas:AddIndex("FAC_ID", "FAC_ID")
      oFacturas:AddIndex("CLI_ID", "CLI_ID")
   ENDIF

   IF !oProductos:Exists()
      ? "Creating products table..."
      oProductos:Create({ ;
         {"PRO_ID",    "N", 10, 0}, ;
         {"CODIGO",    "C", 20, 0}, ;
         {"NOMBRE",    "C", 50, 0}, ;
         {"PRECIO",    "N", 12, 2}, ;
         {"STOCK",     "N", 10, 0} ;
      })
      oProductos:AddIndex("PRO_ID", "PRO_ID")
      oProductos:AddIndex("CODIGO", "CODIGO")
   ENDIF

   IF !oDetalles:Exists()
      ? "Creating invoice details table..."
      oDetalles:Create({ ;
         {"DET_ID",    "N", 10, 0}, ;
         {"FAC_ID",    "N", 10, 0}, ;
         {"PRO_ID",    "N", 10, 0}, ;
         {"CANTIDAD",  "N", 10, 0}, ;
         {"PRECIO",    "N", 12, 2}, ;
         {"SUBTOTAL",  "N", 12, 2} ;
      })
      oDetalles:AddIndex("DET_ID", "DET_ID")
      oDetalles:AddIndex("FAC_ID", "FAC_ID")
      oDetalles:AddIndex("PRO_ID", "PRO_ID")
   ENDIF

   // Insert sample data if tables are empty
   InsertSampleData(oClientes, oFacturas, oProductos, oDetalles)

   RETURN

/**
 * Inserts sample data into tables
 * @procedure InsertSampleData
 * @param oClientes Clients ORM instance
 * @param oFacturas Invoices ORM instance
 * @param oProductos Products ORM instance
 * @param oDetalles Invoice details ORM instance
 * @description Populates tables with sample data if they are empty
 */
PROCEDURE InsertSampleData(oClientes, oFacturas, oProductos, oDetalles)
   // Open tables
   oClientes:Open()
   oFacturas:Open()
   oProductos:Open()
   oDetalles:Open()

   // Insert sample data if tables are empty
   IF oClientes:RecCount() == 0
      ? "Inserting sample data..."

      // Insert clients
      oClientes:Insert({ ;
         "CLI_ID" => 1, ;
         "NOMBRE" => "Juan Pérez", ;
         "DIRECCION" => "Calle Principal 123", ;
         "TELEFONO" => "555-1234", ;
         "EMAIL" => "juan@example.com" ;
      })

      oClientes:Insert({ ;
         "CLI_ID" => 2, ;
         "NOMBRE" => "María López", ;
         "DIRECCION" => "Avenida Central 456", ;
         "TELEFONO" => "555-5678", ;
         "EMAIL" => "maria@example.com" ;
      })

      // Insert products
      oProductos:Insert({ ;
         "PRO_ID" => 1, ;
         "CODIGO" => "P001", ;
         "NOMBRE" => "Teclado", ;
         "PRECIO" => 25.50, ;
         "STOCK" => 100 ;
      })

      oProductos:Insert({ ;
         "PRO_ID" => 2, ;
         "CODIGO" => "P002", ;
         "NOMBRE" => "Mouse", ;
         "PRECIO" => 15.75, ;
         "STOCK" => 150 ;
      })

      oProductos:Insert({ ;
         "PRO_ID" => 3, ;
         "CODIGO" => "P003", ;
         "NOMBRE" => "Monitor", ;
         "PRECIO" => 199.99, ;
         "STOCK" => 50 ;
      })

      // Insert invoice for Juan Pérez
      oFacturas:Insert({ ;
         "FAC_ID" => 1, ;
         "CLI_ID" => 1, ;
         "FECHA" => Date(), ;
         "TOTAL" => 266.99, ;
         "PAGADA" => .T. ;
      })

      // Insert invoice details
      oDetalles:Insert({ ;
         "DET_ID" => 1, ;
         "FAC_ID" => 1, ;
         "PRO_ID" => 1, ;
         "CANTIDAD" => 1, ;
         "PRECIO" => 25.50, ;
         "SUBTOTAL" => 25.50 ;
      })

      oDetalles:Insert({ ;
         "DET_ID" => 2, ;
         "FAC_ID" => 1, ;
         "PRO_ID" => 2, ;
         "CANTIDAD" => 1, ;
         "PRECIO" => 15.75, ;
         "SUBTOTAL" => 15.75 ;
      })

      oDetalles:Insert({ ;
         "DET_ID" => 3, ;
         "FAC_ID" => 1, ;
         "PRO_ID" => 3, ;
         "CANTIDAD" => 1, ;
         "PRECIO" => 199.99, ;
         "SUBTOTAL" => 199.99 ;
      })

      // Insert invoice for María López
      oFacturas:Insert({ ;
         "FAC_ID" => 2, ;
         "CLI_ID" => 2, ;
         "FECHA" => Date() - 5, ;
         "TOTAL" => 41.25, ;
         "PAGADA" => .F. ;
      })

      // Insert invoice details
      oDetalles:Insert({ ;
         "DET_ID" => 4, ;
         "FAC_ID" => 2, ;
         "PRO_ID" => 1, ;
         "CANTIDAD" => 1, ;
         "PRECIO" => 25.50, ;
         "SUBTOTAL" => 25.50 ;
      })

      oDetalles:Insert({ ;
         "DET_ID" => 5, ;
         "FAC_ID" => 2, ;
         "PRO_ID" => 2, ;
         "CANTIDAD" => 1, ;
         "PRECIO" => 15.75, ;
         "SUBTOTAL" => 15.75 ;
      })

      // Insert another invoice for Juan Pérez
      oFacturas:Insert({ ;
         "FAC_ID" => 3, ;
         "CLI_ID" => 1, ;
         "FECHA" => Date() - 10, ;
         "TOTAL" => 51.00, ;
         "PAGADA" => .T. ;
      })

      // Insert invoice details
      oDetalles:Insert({ ;
         "DET_ID" => 6, ;
         "FAC_ID" => 3, ;
         "PRO_ID" => 1, ;
         "CANTIDAD" => 2, ;
         "PRECIO" => 25.50, ;
         "SUBTOTAL" => 51.00 ;
      })
   ENDIF

   // Close tables
   oClientes:Close()
   oFacturas:Close()
   oProductos:Close()
   oDetalles:Close()

   RETURN
