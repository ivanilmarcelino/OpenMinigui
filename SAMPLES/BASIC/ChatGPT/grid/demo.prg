#include "minigui.ch"

function Main()
   LOCAL bBackColor, n
   LOCAL aData := { ;
        { "John Doe", 28, "Engineer", "New York" }, ;
        { "Jane Smith", 34, "Manager", "Los Angeles" }, ;
        { "Sam Johnson", 25, "Intern", "Chicago" }, ;
        { "Alice Brown", 42, "Director", "Miami" }, ;
        { "Bob White", 30, "Developer", "Seattle" } }

   DEFINE WINDOW WinMain ;
      AT 0,0 ;
      WIDTH 700 ;
      HEIGHT 400 ;
      TITLE "Grid with Color Themes" ;
      MAIN 

   @ 20,20 GRID GrdEmployees ;
      WIDTH 650 ;
      HEIGHT 300 ;
      HEADERS { "Name", "Age", "Occupation", "Location" } ;
      WIDTHS { 200, 50, 150, 150 } ;
      JUSTIFY { GRID_JTFY_LEFT, GRID_JTFY_RIGHT, GRID_JTFY_CENTER, GRID_JTFY_CENTER } ;
      ITEMS aData ;
      CELLNAVIGATION ;
      ON CHANGE GridCellChange(aData)

   WinMain.GrdEmployees.Value := 1

   // Set color theme
   bBackColor := {|x, CellRowIndex| iif(CellRowIndex / 2 == Int( CellRowIndex / 2 ), { 220, 220, 220 }, WHITE ) }
   FOR n=1 TO 4
      WinMain.GrdEmployees.HeaderDynamicForeColor(n) := {|| BLACK }
      WinMain.GrdEmployees.HeaderDynamicBackColor(n) := {|| { 240, 240, 240 } }
      WinMain.GrdEmployees.ColumnDynamicBackColor(n) := bBackColor
   NEXT
   _HMG_GridSelectedCellForeColor := YELLOW
   _HMG_GridSelectedCellBackColor := { 0, 0, 128 }
   _HMG_GridSelectedRowBackColor  := { 0, 120, 220 }
   _HMG_GridSelectedRowForeColor  := WHITE

   @ 330,20 BUTTON BtnShowData ;
      CAPTION "Show Data" ;
      ACTION ShowData(@aData)

   END WINDOW

   CENTER WINDOW WinMain
   ACTIVATE WINDOW WinMain

return nil

procedure GridCellChange(aData)
   LOCAL oGrid := This.Name // "GrdEmployees"
   LOCAL nRow := WinMain.(oGrid).Value[1]
   LOCAL nCol := WinMain.(oGrid).Value[2]
   LOCAL cValue

   IF nRow > 0 .AND. nCol > 0
      cValue := WinMain.(oGrid).Cell(nRow,nCol)
      aData[nRow][nCol] := iif(nCol == 2, VAL(cValue), cValue)
      MsgInfo("Data Updated! Row: " + LTRIM(STR(nRow)) + ", Column: " + LTRIM(STR(nCol)) + ", Cell Value: " + cValue)
   ENDIF
return

procedure ShowData(aData)
   LOCAL aRow, cData := ""

   FOR EACH aRow IN aData
      cData += aRow[1] + " | " + STR(aRow[2]) + " | " + aRow[3] + " | " + aRow[4] + CRLF
   NEXT

   MsgInfo("Current Data:" + CRLF + cData)
return
