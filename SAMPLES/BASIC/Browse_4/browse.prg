/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2002 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
*/

#include "minigui.ch"

#define DSU_FILE "BROWSE.SET"

SET PROCEDURE TO DSU_UDFS.prg

FUNCTION Main()

   SET BROWSESYNC ON

   DEFINE WINDOW Form_1 ;
         AT 0, 0 ;
         WIDTH 800 HEIGHT 480 ;
         TITLE 'MiniGUI Browse Demo' ;
         MAIN NOMAXIMIZE ;
         ON INIT ( IF( File( DSU_FILE ), DSU_Rest( DSU_FILE ), OpenTables() ), SetItems() ) ;
         ON RELEASE CloseTables()

      @ 10, 10 LABEL Label_1 VALUE 'Pedidos'

      @ 10, 400 LABEL Label_2 VALUE 'Items'

      @ 40, 10 BROWSE Pedidos ;
         WIDTH 380 ;
         HEIGHT 370 ;
         HEADERS { 'Pedido', 'Cliente', 'Endereco', 'Cidade' } ;
         WIDTHS { 100, 250, 250, 150 } ;
         WORKAREA Pedidos ;
         FIELDS { 'Pedidos->Pedido', 'Clientes->Nome', 'Clientes->Endereco', 'Clientes->Cidade' } ;
         ON CHANGE UpdateItems() ;
         EDIT INPLACE ;
         READONLY { .T., .F., .F., .F. } LOCK

      DEFINE BROWSE Items
         ROW 40
         COL 400
         WIDTH 380
         HEIGHT 370
         HEADERS { 'Pedido', 'Produto', 'Quant', 'Valor', 'Sum' }
         WIDTHS { 99, 120, 60, 80, 90 }
         WORKAREA ITEMS
         FIELDS { 'Items->Pedido', 'Items->Produto', 'Items->Quant', 'Items->Valor', 'Quant*Valor' }
         JUSTIFY { BROWSE_JTFY_LEFT, BROWSE_JTFY_LEFT, BROWSE_JTFY_RIGHT, BROWSE_JTFY_RIGHT, BROWSE_JTFY_RIGHT }
         ALLOWEDIT .T.
         INPLACEEDIT .T.
         READONLY { .T., .F., .F., .F., .T. }
         LOCK .T.
         PICTURE { NIL, NIL, "99999", "999999.99", "999999.99" }
      END BROWSE

   END WINDOW

   CENTER WINDOW Form_1

   ACTIVATE WINDOW Form_1

RETURN NIL

PROCEDURE OpenTables()

   USE Clientes Shared New
   INDEX ON FIELD->Codigo TO Clientes

   USE Produtos Shared New
   INDEX ON FIELD->Produto TO Produtos

   USE Items Shared New
   INDEX ON FIELD->Pedido TO ITEMS

   USE Pedidos Shared New
   SET RELATION TO FIELD->Cliente INTO Clientes

   DSU_Save( DSU_FILE )

RETURN

PROCEDURE CloseTables()

   Close DataBases

RETURN

PROCEDURE SetItems()

   LOCAL aProduto := {}

   SELECT Produtos
   dbEval( {|| AAdd( aProduto, { FIELD->Nome, FIELD->Produto } ) } )

   SELECT Pedidos
   Form_1.Pedidos.Value := RecNo()

   UpdateItems()

   Form_1.Items.InputItems := { NIL, aProduto, NIL, NIL, Nil }

   Form_1.Items.DisplayItems := { NIL, aProduto, NIL, NIL, Nil }

RETURN

PROCEDURE UpdateItems()

   LOCAL nArea := Select()

   SELECT Items
   SET SCOPE TO Pedidos->Pedido
   GO TOP
   Form_1.Items.Value := RecNo()
   Select( nArea )

RETURN
