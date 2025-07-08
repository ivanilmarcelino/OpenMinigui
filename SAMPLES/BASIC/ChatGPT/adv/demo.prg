#include "minigui.ch"

function Main()
   LOCAL aEmployees := { ;
      { "John Doe", 28, "Engineer" }, ;
      { "Jane Smith", 34, "Manager" }, ;
      { "Sam Johnson", 25, "Intern" } }

   DEFINE WINDOW WinMain ;
      AT 0,0 ;
      WIDTH 500 ;
      HEIGHT 400 ;
      TITLE "Employee Management System" ;
      MAIN 

   @ 20,20 GRID GrdEmployees ;
      WIDTH 450 ;
      HEIGHT 200 ;
      HEADERS { "Name", "Age", "Occupation" } ;
      WIDTHS { 200, 50, 150 } ;
      ITEMS aEmployees ;
      JUSTIFY { 0, 1, 0 }

   @ 240,180 BUTTON BtnAdd CAPTION "Add Employee" ACTION AddEmployee(aEmployees)
   @ 280,180 BUTTON BtnDelete CAPTION "Delete Employee" ACTION DeleteEmployee(aEmployees, "GrdEmployees")

   END WINDOW

   CENTER WINDOW WinMain
   ACTIVATE WINDOW WinMain

return nil

function AddEmployee(aEmployees)
   DEFINE WINDOW WinAdd ;
      AT 100,100 ;
      WIDTH 400 ;
      HEIGHT 300 ;
      TITLE "Add New Employee"

   @ 20,20 LABEL LblName VALUE "Name:"
   @ 20,120 TEXTBOX TxtName VALUE ""

   @ 60,20 LABEL LblAge VALUE "Age:"
   @ 60,120 TEXTBOX TxtAge VALUE "" NUMERIC

   @ 100,20 LABEL LblOccupation VALUE "Occupation:"
   @ 100,120 TEXTBOX TxtOccupation VALUE ""

   @ 150,150 BUTTON BtnSave CAPTION "Save" ACTION (SaveEmployee(aEmployees, WinAdd.TxtName.Value, WinAdd.TxtAge.Value, WinAdd.TxtOccupation.Value), WinAdd.Release)

   END WINDOW

   CENTER WINDOW WinAdd
   ACTIVATE WINDOW WinAdd

return nil

function SaveEmployee(aEmployees, cName, nAge, cOccupation)
   AADD(aEmployees, { cName, nAge, cOccupation })
   WinMain.GrdEmployees.SetArray(aEmployees)
   WinMain.GrdEmployees.Value := LEN(aEmployees)
   WinMain.GrdEmployees.Refresh()
return nil

function DeleteEmployee(aEmployees, GrdEmployees)
   LOCAL nRow := WinMain.(GrdEmployees).Value
   IF nRow > 0
      hb_ADEL(aEmployees, nRow, .T.)
      WinMain.(GrdEmployees).SetArray(aEmployees)
      WinMain.(GrdEmployees).Refresh()
   ELSE
      MsgInfo("No row selected!")
   ENDIF
return nil
