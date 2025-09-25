/*

 BadaSystem
 Program       : LibrarySystemMain
 Module        : EmployeeManager Class
 Compiler      : MINIGUI - Harbour Win32 GUI
 Compiler-C    : BCC 32 bit
 Author        : Marcos Jarrín
 Email         : marvijarrin@gmail.com
 Date          : 15/07/2025
 Update        : 07/08/2025
 Rev           : 0.1

*/

#include "hbclass.ch"
#include "minigui.ch"

// ----------------------------------------------------------------------------
// EmployeeManager Class
// ----------------------------------------------------------------------------
CLASS EmployeeManager
    DATA oEmployees AS OBJECT

    METHOD New() CONSTRUCTOR
    METHOD showWindow()
    METHOD scanEmployee( CtxtEmpID )
ENDCLASS

METHOD New() CLASS EmployeeManager
    ::oEmployees  := HbORM():New("employeesn", "employees","data\")
RETURN Self

/**
 * Shows employee management window
 * @return void
 */
METHOD showWindow() CLASS EmployeeManager

    DEFINE WINDOW EmployeeForm ;
        AT 0,0 WIDTH 415 HEIGHT 250 ;
        TITLE "Employee Management" ;
        CHILD

        ON KEY ESCAPE ACTION EmployeeForm.Release

        @ 20, 20 LABEL lblEmpID VALUE "Employee ID:" WIDTH 100 HEIGHT 20
        @ 20,120 TEXTBOX txtEmpID WIDTH 150 HEIGHT 20 MAXLENGTH 10 UPPER PLACEHOLDER "COD001"

        @ 50, 20 LABEL lblName VALUE "Name:" WIDTH 100 HEIGHT 20
        @ 50,120 TEXTBOX txtName WIDTH 150 HEIGHT 20 MAXLENGTH 50 PLACEHOLDER "John Fitzgerald"

        @ 80, 20 LABEL lblPosition VALUE "Position:" WIDTH 100 HEIGHT 20
        @ 80,120 TEXTBOX txtPosition WIDTH 150 HEIGHT 20 MAXLENGTH 20 PLACEHOLDER "LIBRARIAN"

        @110, 20 LABEL lblPassword VALUE "Password:" WIDTH 100 HEIGHT 20
        @110,120 TEXTBOX txtPassword WIDTH 150 HEIGHT 20 PASSWORD MAXLENGTH 20

        DEFINE BUTTON btnAdd
            ROW 150
            COL 20
            WIDTH 80
            HEIGHT 30
            CAPTION "Add"
            ACTION {|| Employee():New(EmployeeForm.txtEmpID.Value , EmployeeForm.txtName.Value, "", "LIBRARIAN", EmployeeForm.txtPassword.Value, EmployeeForm.txtEmpID.Value, EmployeeForm.txtPosition.Value):add(), ;
                       EmployeeForm.txtEmpID.Value := "", EmployeeForm.txtName.Value := "", EmployeeForm.txtPosition.Value := "", EmployeeForm.txtPassword.Value := "" }
        END BUTTON

        DEFINE BUTTON btnUpdate
            ROW 150
            COL 110
            WIDTH 80
            HEIGHT 30
            CAPTION "Update"
            ACTION {|| Employee():New(EmployeeForm.txtEmpID.Value, EmployeeForm.txtName.Value, "", "LIBRARIAN", EmployeeForm.txtPassword.Value, EmployeeForm.txtEmpID.Value, EmployeeForm.txtPosition.Value):update() , ;
                       EmployeeForm.txtEmpID.Value := "", EmployeeForm.txtName.Value := "", EmployeeForm.txtPosition.Value := "", EmployeeForm.txtPassword.Value := "" }
        END BUTTON

        DEFINE BUTTON btnDelete
            ROW 150
            COL 200
            WIDTH 80
            HEIGHT 30
            CAPTION "Delete"
            ACTION {|| Employee():New(EmployeeForm.txtEmpID.Value, "", "", "", "", EmployeeForm.txtEmpID.Value, ""):delete() ,;
                       EmployeeForm.txtEmpID.Value := "", EmployeeForm.txtName.Value := "", EmployeeForm.txtPosition.Value := "", EmployeeForm.txtPassword.Value := "" }
        END BUTTON

        DEFINE BUTTON btnScan
            ROW 150
            COL 290
            WIDTH 90
            HEIGHT 30
            CAPTION "Scan Employee"
            ACTION {|| ::scanEmployee( EmployeeForm.txtEmpID.Value ) }
        END BUTTON

    END WINDOW

    CENTER WINDOW EmployeeForm
    ACTIVATE WINDOW EmployeeForm

RETURN NIL


METHOD scanEmployee( CtxtEmpID )

    LOCAL oError

    TRY
       ::oEmployees:Open()
           IF ::oEmployees:Seek( EmployeeForm.txtEmpID.value  )
              EmployeeForm.txtName.value     := ::oEmployees:GetValue("NAME")
              EmployeeForm.txtPosition.value := ::oEmployees:GetValue("POSITION")
           ELSE
              EmployeeForm.txtEmpID.value    := ""
              EmployeeForm.txtName.value     := ""
              EmployeeForm.txtPosition.value := ""
              MSGBOX("Employee does not exist")
           ENDIF
        ::oEmployees:Close()
    CATCH  oError
        MsgStop("Database verification failed: " + oError:description)
    END

RETURN NIL
