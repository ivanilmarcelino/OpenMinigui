/*

 BadaSystem
 Program       : LibrarySystemMain
 Module        : Main Program
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

REQUEST DBFCDX

// ----------------------------------------------------------------------------
// Main Program
// ----------------------------------------------------------------------------
FUNCTION Main()

    LOCAL oApp

    SET DATE FORMAT "YYYY-MM-DD"
    SET DELETE ON
    SET EXCLUSIVE ON
    SET NAVIGATION EXTENDED

    RddSetDefault( "DBFCDX" )

    oApp := LibrarySystem():New()
    oApp:startApplication()

RETURN NIL

// ----------------------------------------------------------------------------
// Utility Function
// ----------------------------------------------------------------------------
FUNCTION GenerateUniqueID()

    LOCAL cID
    LOCAL oError
    LOCAL oUsers
    LOCAL oEmployees
    LOCAL oLoans

    oUsers      := HbORM():New("users",     "users",    "data\")
    oEmployees  := HbORM():New("employees", "employees","data\")
    oLoans      := HbORM():New("loans",     "loans",    "data\")

    TRY
        DO WHILE .T.
            cID := StrZero(hb_RandInt(1000000, 9999999), 7)

            oUsers:Open()
                IF !oUsers:Seek( cId )
                    oEmployees:Open()
                    IF !oEmployees:Seek(cID)
                        oLoans:Open()
                        IF !oLoans:Seek(cID)
                            oUsers:Close()
                            oEmployees:Close()
                            oLoans:Close()
                            EXIT
                        ENDIF
                    ENDIF
                ENDIF
            oUsers:Close()
            oEmployees:Close()
            oLoans:Close()
        ENDDO
    CATCH oError
        MsgStop("Database verification failed: " + oError:description)
    END

RETURN cID

//
//Test procedure for implementing new options
//
PROCEDURE nothing()
RETURN
