#include <hmg.ch>

*-----------*
Function Main
*-----------*
   Local aMainItems
   Local aMainHeaders
   Local aMainJustify
   Local aMainWidths

   /*
    * Init RDD DBFCDX
   */

   REQUEST DBFCDX
   RDDSETDEFAULT("DBFCDX")

   /*
    * Set global
   */

   SET MULTIPLE OFF WARNING      
   SET NAVIGATION EXTENDED
   SET DELETED ON
   SET DATE FRENCH
   SET CENTURY ON

   Use NOMES New
   INDEX ON FIELD->NOME TAG Nome TO NOMES.CDX
   Close NOMES

   aMainItems   := { {"",""} }
   aMainHeaders := { 'Codigo' , 'Nombre' }
   aMainJustify := {1,0}
   aMainWidths  := {105,390}
  
   Load Window Main
   Main.Center
   Main.Activate

Return Nil

*---------------------*
Static Procedure Buscar
*---------------------*
   Local cBusca := Upper( AllTrim(Main.Text_1.Value) )
   Local cAlias

   Use NOMES New
   cAlias := Alias()
   (cAlias)->(DbSetOrder(1))
   (cAlias)->(DbGoTop())

   DELETE ITEM ALL FROM Grid_1 OF Main

   If ! Empty(cBusca)

      IF Left(cBusca, 1) == "*" .OR. "*" $ cBusca .OR. "?" $ cBusca
      ELSE
         cBusca := "*" + cBusca + "*"
      ENDIF

      IF (cAlias)->(OrdWildSeek( cBusca ))  // 1st time call

         ADD ITEM { (cAlias)->CODIGO, (cAlias)->NOME } TO Grid_1 OF Main

         Do While (cAlias)->(OrdWildSeek( cBusca, .T. ))  // repeated call with 2nd param .T.
            ADD ITEM { (cAlias)->CODIGO, (cAlias)->NOME } TO Grid_1 OF Main
         EndDo 

      ENDIF

   Endif

   Close NOMES

   Main.Grid_1.Value := Main.Grid_1.ItemCount
   Main.Grid_1.Setfocus

Return
