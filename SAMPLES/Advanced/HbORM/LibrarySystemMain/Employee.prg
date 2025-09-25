/*

 BadaSystem
 Program       : LibrarySystemMain
 Module        : Employee Class
 Compiler      : MINIGUI - Harbour Win32 GUI
 Compiler-C    : BCC 32 bit
 Author        : Marcos Jarrín
 Email         : marvijarrin@gmail.com
 Date          : 15/07/2025
 Update        : 04/08/2025
 Rev           : 0.1

*/

#include "hbclass.ch"
#include "minigui.ch"

// ----------------------------------------------------------------------------
// Employee Class
// ----------------------------------------------------------------------------
CLASS Employee FROM User
    DATA EMPID      AS STRING
    DATA POSITION   AS STRING
    DATA oEmployees AS OBJECT
    DATA oUsers     AS OBJECT
    DATA oBooks     AS OBJECT
    DATA oLoans     AS OBJECT

    METHOD New(cID, cName, cEmail, cRole, cPassword, cEmpID, cPosition) CONSTRUCTOR
    METHOD processLoan(oLoan)
    METHOD generateReports()
    METHOD Add()
    METHOD Update()
    METHOD Delete()
ENDCLASS

METHOD New(cID, cName, cEmail, cRole, cPassword, cEmpID, cPosition) CLASS Employee
    ::Super:New(cID, cName, cEmail, cRole, cPassword)
    ::EMPID      := cEmpID
    ::POSITION   := cPosition
    ::oEmployees := HbORM():New("employeesn", "employees","data\")
    ::oUsers     := HbORM():New("users",      "users",    "data\")
    ::oBooks     := HbORM():New("books",      "books",    "data\")
    ::oLoans     := HbORM():New("loans",      "loans",    "data\")
RETURN Self

//
METHOD Add() CLASS Employee

    LOCAL oError

    TRY
        ::oEmployees:Open()
            IF !::oEmployees:Seek(::Super:USERID)
                ::oUsers:Open()
                    IF !::oUsers:Seek(::Super:USERID)
                        ::oEmployees:Insert({;
                           "EMPID"    =>  ::EMPID      ,;
                           "NAME"     =>  ::Super:NAME ,;
                           "POSITION" =>  ::POSITION    ;
                            })
                        ::oUsers:Close()
                        ::Super:Add()
                        MSGBOX("New Employee and New User")
                    ELSE
                        ::oUsers:Close()
                        MSGBOX("User already exists")
                    ENDIF
            ELSE
                MSGBOX("Employee already exists")
            ENDIF
        ::oEmployees:Close()
    CATCH oError
        MsgStop("Database verification failed: " + oError:description )
    END

RETURN NIL

METHOD Update() CLASS Employee

    LOCAL oError

    TRY
        ::oEmployees:Open()
            IF ::oEmployees:Seek( ::Super:USERID )
                ::oUsers:Open()
                IF ::oUsers:Seek(::Super:USERID)
                    ::oEmployees:Update({;
                        "NAME"      =>  ::Super:NAME ,;
                        "POSITION"  =>  ::POSITION    ;
                        })
                    ::oUsers:Close()
                    ::Super:Update()
                    MSGBOX("Update employees")
                ELSE
                    ::oUsers:Close()
                    MSGBOX("User not exists")
                ENDIF
            ELSE
                MSGBOX("Employee not exists")
            ENDIF
        ::oEmployees:Close()
    CATCH  oError
        MsgStop("Database verification failed: " + oError:description)
    END

RETURN NIL

/**
 * Deletes Employee from database
 * return Logical Success status
 */
METHOD delete() CLASS Employee

    LOCAL lReturn := .T.
    LOCAL oError

    TRY
        ::oEmployees:Open(.F.)
        IF ::oEmployees:Seek( ::Super:USERID )
            ::oEmployees:Delete()
            ::oEmployees:Close()
            ::Super:delete()
            lReturn := .T.
            MSGBOX("Employee and user deleted")
        ELSE
            ::oEmployees:Close()
            lReturn := .F.
            MSGBOX("Employee not found")
        ENDIF
    CATCH oError
        MsgStop("Database verification failed: " + oError:description)
    END

RETURN lReturn


/**
 * Processes a loan transaction
 * param oLoan Loan Transaction object
 * return Logical Success status
 */
METHOD processLoan(oLoan) CLASS Employee

    LOCAL lReturn
    LOCAL oError

    TRY
        ::oBooks:Open()
        IF ::oBooks:Seek(oLoan:ISBN) .AND. ALLTRIM( ::oBooks:GetValue("STATUS")) == "AVAILABLE"
            ::oBooks:SetValue( "STATUS", "CHECKED_OUT" )
            ::oLoans:Open()
            ::oLoans:Insert({;
                    "LOANID"     => oLoan:LOANID   , ;
                    "USERID"     => oLoan:USERID   , ;
                    "ISBN"       => oLoan:ISBN     , ;
                    "CHECKOUT"   => oLoan:CHECKOUT , ;
                    "DUEDATE"    => oLoan:DUEDATE  , ;
                    "RETURNDATE" => oLoan:RETURNDATE ;
                })
            ::oLoans:Close()
            MSGBOX("Borrowed book")
            lReturn := .T.
        ELSE
            MSGBOX("Book not available")
            lReturn := .F.
        ENDIF
        ::oBooks:Close()
    CATCH oError
        MsgStop("Database verification failed: " + oError:description)
    END

RETURN lReturn

/**
 * Generates various system reports
 * @return void
 */
METHOD generateReports() CLASS Employee
    ReportManager():New():showWindow(::ROLE)
RETURN NIL
