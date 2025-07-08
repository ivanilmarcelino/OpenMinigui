#include "minigui.ch"

function Main()
   LOCAL aData := { ;
      { "John Doe", 28, "Engineer" }, ;
      { "Jane Smith", 34, "Manager" }, ;
      { "Sam Johnson", 25, "Intern" } }

   DEFINE WINDOW WinMain ;
      AT 0,0 ;
      WIDTH 600 ;
      HEIGHT 400 ;
      TITLE "Dynamic Grid Update - Add, Edit, Delete" ;
      MAIN 

   @ 20,20 GRID GrdEmployees ;
      WIDTH 550 ;
      HEIGHT 300 ;
      HEADERS { "Name", "Age", "Occupation" } ;
      WIDTHS { 200, 50, 150 } ;
      ITEMS aData ;
      JUSTIFY { 0, 1, 0 }

   @ 330,150 BUTTON BtnAdd ;
      CAPTION "Add Row" ;
      ACTION AddRow("GrdEmployees", aData)

   @ 330,250 BUTTON BtnEdit ;
      CAPTION "Edit Row" ;
      ACTION EditRow("GrdEmployees", aData)

   @ 330,350 BUTTON BtnDelete ;
      CAPTION "Delete Row" ;
      ACTION DeleteRow("GrdEmployees", aData)

   END WINDOW

   CENTER WINDOW WinMain
   ACTIVATE WINDOW WinMain

return nil

function AddRow(oGrid, aData)
   LOCAL cName := InputBox("Enter name:")
   LOCAL nAge := VAL(InputBox("Enter age:"))
   LOCAL cOccupation := InputBox("Enter occupation:")

   AADD(aData, { cName, nAge, cOccupation })
   WinMain.(oGrid).SetArray(aData)
   WinMain.(oGrid).Refresh()

   MsgInfo("Row added successfully!")
return nil

function EditRow(oGrid, aData)
   LOCAL aRow
   LOCAL nRow := WinMain.(oGrid).Value
   IF nRow <= 0
      MsgInfo("Please select a row to edit.")
      RETURN NIL
   ENDIF

   aRow := aData[nRow]

   aRow[1] := InputBox("Edit name:", ,aRow[1])
   aRow[2] := VAL(InputBox("Edit age:", ,LTRIM(STR(aRow[2]))))
   aRow[3] := InputBox("Edit occupation:", ,aRow[3])

   WinMain.(oGrid).Item(nRow) := aRow
   WinMain.(oGrid).Refresh()

   MsgInfo("Row edited successfully!")
return nil

function DeleteRow(oGrid, aData)
   LOCAL nRow := WinMain.(oGrid).Value
   IF nRow <= 0
      MsgInfo("Please select a row to delete.")
      RETURN NIL
   ENDIF

   hb_ADEL(aData, nRow, .T.)  // Adjust array to remove NIL
   WinMain.(oGrid).SetArray(aData)
   WinMain.(oGrid).Refresh()

   MsgInfo("Row deleted successfully!")
return nil
