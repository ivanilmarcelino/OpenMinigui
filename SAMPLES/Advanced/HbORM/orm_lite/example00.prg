/**
 * BadaSystem
 * Program       : HbORM - Harbour Object-Relational Mapping
 * Module        : HbORM Test Program
 * Compiler      : MINIGUI - Harbour Win32 GUI
 * Compiler-C    : BCC 32 bit
 * Author        : Marcos Jarrin
 * Email         : marvijarrin@gmail.com
 * Date          : 20/07/2025
 * Update        : 02/08/2025
 * Rev           : 0.1
 * Description   : Example of using the ORM for Harbour
 */

#include "hbclass.ch"

REQUEST DBFCDX
REQUEST HB_GT_WIN_DEFAULT

/**
 * Main procedure demonstrating HbORM functionality
 * procedure Main
 * description Demonstrates basic ORM operations including:
 *              - Table creation and management
 *              - Data insertion
 *              - Relationship handling
 *              - Data querying and updates
 */
PROCEDURE Main()
   LOCAL oClientes, oFacturas, oProductos, oDetalles
   LOCAL oRelFacturas, oRelDetalles
   LOCAL aFacturas, aDetalles
   LOCAL hCliente, hFactura, hProducto, hDetalle
   LOCAL nI
   LOCAL nFacId, nDetId
   LOCAL oClientes01,oClientes02,oClientes03

   // Configure environment
   SET EXACT ON
   SET DELETED ON
   RddSetDefault( "DBFCDX" )

   // Create table instances
   oClientes  := HbORM():New("clientes", "CLI", "data\")
   oFacturas  := HbORM():New("facturas", "FAC", "data\")
   oProductos := HbORM():New("productos", "PRO", "data\")
   oDetalles  := HbORM():New("detalles", "DET", "data\")

   // Create data directory if it doesn't exist
   IF !isdir("data")
        dirmake("data")
   ENDIF

   // Create tables if they don't exist
   IF !oClientes:Exists()
      ? "Creating clients table..."
      oClientes:Create({ ;
         {"CLI_ID",    "N",  10, 0}, ;
         {"NOMBRE",    "C",  50, 0}, ;
         {"DIRECCION", "C", 100, 0}, ;
         {"TELEFONO",  "C",  15, 0}, ;
         {"EMAIL",     "C",  50, 0} })
      oClientes:AddIndex("CLI_ID", "CLI_ID")
   ENDIF

   IF !oFacturas:Exists()
      ? "Creating invoices table..."
      oFacturas:Create({ ;
         {"FAC_ID",    "N", 10, 0}, ;
         {"CLI_ID",    "N", 10, 0}, ;
         {"FECHA",     "D",  8, 0}, ;
         {"TOTAL",     "N", 12, 2}, ;
         {"PAGADA",    "L",  1, 0}  ;
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
         {"STOCK",     "N", 10, 0}  ;
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
         {"SUBTOTAL",  "N", 12, 2}  ;
      })
      oDetalles:AddIndex("DET_ID", "DET_ID")
      oDetalles:AddIndex("FAC_ID", "FAC_ID")
      oDetalles:AddIndex("PRO_ID", "PRO_ID")
   ENDIF

   // Open tables
   oClientes:Close()
   oFacturas:Close()
   oProductos:Close()
   oDetalles:Close()

   // Open tables
   oClientes:Open()
   oFacturas:Open()
   oProductos:Open()
   oDetalles:Open()

   // Create table relationships
   oRelFacturas := HbORMRelation():New(oClientes, oFacturas, "1:N", "CLI_ID", "CLI_ID")
   oRelDetalles := HbORMRelation():New(oFacturas, oDetalles, "1:N", "FAC_ID", "FAC_ID")

   // Insert sample data if tables are empty
   IF oClientes:RecCount() == 0
      ? "Inserting sample data..."

      // Insert clients
      oClientes:Insert({ ;
         "CLI_ID"    => 1, ;
         "NOMBRE"    => "Juan Perez", ;
         "DIRECCION" => "Calle Principal 123", ;
         "TELEFONO"  => "555-1234", ;
         "EMAIL"     => "juanexample.com" ;
      })

      oClientes:Insert({ ;
         "CLI_ID"    => 2, ;
         "NOMBRE"    => "Maria Lopez", ;
         "DIRECCION" => "Avenida Central 456", ;
         "TELEFONO"  => "555-5678", ;
         "EMAIL"     => "mariaexample.com" ;
      })

      // Insert products
      oProductos:Insert({ ;
         "PRO_ID" => 1, ;
         "CODIGO" => "P001", ;
         "NOMBRE" => "Teclado", ;
         "PRECIO" => 25.50, ;
         "STOCK"  => 100 ;
      })

      oProductos:Insert({ ;
         "PRO_ID" => 2, ;
         "CODIGO" => "P002", ;
         "NOMBRE" => "Mouse", ;
         "PRECIO" => 15.75, ;
         "STOCK"  => 150 ;
      })

      oProductos:Insert({ ;
         "PRO_ID" => 3, ;
         "CODIGO" => "P003", ;
         "NOMBRE" => "Monitor", ;
         "PRECIO" => 199.99, ;
         "STOCK"  => 50 ;
      })

      // Insert invoice for Juan Pérez
      oFacturas:Insert({ ;
         "FAC_ID" => 1, ;
         "CLI_ID" => 1, ;
         "FECHA"  => Date(), ;
         "TOTAL"  => 266.99, ;
         "PAGADA" => .T. ;
      })

      // Insert invoice details
      oDetalles:Insert({ ;
         "DET_ID"   => 1, ;
         "FAC_ID"   => 1, ;
         "PRO_ID"   => 1, ;
         "CANTIDAD" => 1, ;
         "PRECIO"   => 25.50, ;
         "SUBTOTAL" => 25.50 ;
      })

      oDetalles:Insert({ ;
         "DET_ID"   => 2, ;
         "FAC_ID"   => 1, ;
         "PRO_ID"   => 2, ;
         "CANTIDAD" => 1, ;
         "PRECIO"   => 15.75, ;
         "SUBTOTAL" => 15.75 ;
      })

      oDetalles:Insert({ ;
         "DET_ID"   => 3, ;
         "FAC_ID"   => 1, ;
         "PRO_ID"   => 3, ;
         "CANTIDAD" => 1, ;
         "PRECIO"   => 199.99, ;
         "SUBTOTAL" => 199.99 ;
      })

      // Insert invoice for María López
      oFacturas:Insert({ ;
         "FAC_ID" => 2, ;
         "CLI_ID" => 2, ;
         "FECHA"  => Date() - 5, ;
         "TOTAL"  => 41.25, ;
         "PAGADA" => .F. ;
      })

      // Insert invoice details
      oDetalles:Insert({ ;
         "DET_ID"   => 4, ;
         "FAC_ID"   => 2, ;
         "PRO_ID"   => 1, ;
         "CANTIDAD" => 1, ;
         "PRECIO"   => 25.50, ;
         "SUBTOTAL" => 25.50 ;
      })

      oDetalles:Insert({ ;
         "DET_ID"   => 5, ;
         "FAC_ID"   => 2, ;
         "PRO_ID"   => 2, ;
         "CANTIDAD" => 1, ;
         "PRECIO"   => 15.75, ;
         "SUBTOTAL" => 15.75 ;
      })
   ENDIF


   // ORM usage example
   ? "ORM usage example:"
   //The system enables concurrent openings of the same table without conflicts by automatically
   //assigning a unique alias to each instance.

   oClientes01 := HbORM():New("clientes")
   oClientes02 := HbORM():New("clientes")
   oClientes03 := HbORM():New("clientes")
   ?
   ? "The system enables concurrent openings of the same table without conflicts"
   ? "by automatically assigning a unique alias to each instance."
   oClientes01:Open()
   ? "Alias: " + oClientes01:cAlias
   ? oClientes01:GetValue("NOMBRE")

   oClientes02:Open()
   ? "Alias: " + oClientes02:cAlias
   ? oClientes02:GetValue("NOMBRE")

   oClientes03:Open()
   ? "Alias: " + oClientes03:cAlias
   ? oClientes03:GetValue("NOMBRE")

   oClientes01:Close()
   oClientes02:Close()
   oClientes03:Close()
   Inkey(0)
   ?

   oRelFacturas := HbORMRelation():New(oClientes, oFacturas, "1:N", "CLI_ID", "CLI_ID")
   // Find client by ID
   ? "Searching for client with ID 1..."
   IF oClientes:Find(1)
      hCliente := oClientes:GetRow()
      ? "Client found:", hCliente["NOMBRE"]

      // Get client invoices using relationship
      ? "Client invoices:"
      aFacturas := oRelFacturas:GetRelated(hCliente["CLI_ID"])

      FOR EACH hFactura IN aFacturas
         ? "  Invoice ID:", hFactura["FAC_ID"], "Date:", hFactura["FECHA"], "Total:", hFactura["TOTAL"]

         // Get invoice details using relationship
         ? "  Invoice details:"
         aDetalles := oRelDetalles:GetRelated(hFactura["FAC_ID"])

         FOR EACH hDetalle IN aDetalles
            // Find product
            oProductos:Find(hDetalle["PRO_ID"],"PRO_ID")
            hProducto := oProductos:GetRow()

            ? "    Product:", hProducto["NOMBRE"], "Quantity:", hDetalle["CANTIDAD"], "Subtotal:", hDetalle["SUBTOTAL"]
         NEXT
      NEXT
   ELSE
      ? "Client not found"
   ENDIF

   // Update example
   ? "Updating data..."
   IF oClientes:Find(1)
      ? "Updating client phone..."
      oClientes:SetValue("TELEFONO", "555-9876")
      ? "Phone updated to:", oClientes:GetValue("TELEFONO")
   ENDIF

    oFacturas:GoBottom()
    nFacId := oFacturas:GetValue("Fac_ID")
   // Insert example
   ? "Inserting new invoice..."
   oFacturas:Insert({ ;
      "FAC_ID" => ++nFacId, ;
      "CLI_ID" => 1, ;
      "FECHA"  => Date(), ;
      "TOTAL"  => 51.00, ;
      "PAGADA" => .F. ;
   })

   oDetalles:SetOrder("DET_ID")
   oDetalles:GoBottom()
   nDetId := oDetalles:GetValue("DET_ID")
   oDetalles:Insert({ ;
      "DET_ID"   => ++nDetId, ;
      "FAC_ID"   => nFacId, ;
      "PRO_ID"   => 1, ;
      "CANTIDAD" => 2, ;
      "PRECIO"   => 25.50, ;
      "SUBTOTAL" => 51.00 ;
   })

   ? "New invoice inserted with ID 3"


   // Conditional search example
   ? "Searching for paid invoices..."
   oFacturas:GoTop()

   DO WHILE !oFacturas:Eof()
      IF oFacturas:GetValue("PAGADA")
         ? "  Invoice ID:", oFacturas:GetValue("FAC_ID"), "is paid"
      ENDIF

      oFacturas:Skip()
   ENDDO

   // Close tables
   oClientes:Close()
   oFacturas:Close()
   oProductos:Close()
   oDetalles:Close()

   ? "Example completed"
   Inkey(0)

   RETURN
