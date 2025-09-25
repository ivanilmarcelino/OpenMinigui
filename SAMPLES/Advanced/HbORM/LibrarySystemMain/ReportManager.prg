/*

 BadaSystem
 Program       : LibrarySystemMain
 Module        : ReportManager Class
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
// ReportManager Class
// ----------------------------------------------------------------------------
CLASS ReportManager
    DATA oLoans AS OBJECT
    DATA oBooks AS OBJECT

    METHOD New() CONSTRUCTOR
    METHOD showWindow(cRole)
    METHOD showCurrentLoans()
    METHOD showOverdueBooks()
    METHOD showInventoryStatus()
    METHOD showUserActivity()
    METHOD displayReport(cTitle, aHeaders, aData)
ENDCLASS

METHOD New() CLASS ReportManager
    ::oLoans := HbORM():New("loans","loans","data\")
    ::oBooks := HbORM():New("books","books","data\")
RETURN Self

/**
 * Shows report selection window
 * param cRole String User role for access control
 * return void
 */
METHOD showWindow(cRole) CLASS ReportManager
   IF cRole $ "ADMIN,LIBRARIAN"

       DEFINE WINDOW ReportForm ;
            AT 0,0 WIDTH 200 HEIGHT 250 ;
            TITLE "Report Selection" ;
            CHILD

            ON KEY ESCAPE ACTION ReportForm.Release

            DEFINE BUTTON btnLoans
                ROW 20
                COL 20
                WIDTH 150
                HEIGHT 30
                CAPTION "Current Loans"
                ACTION {|| ::showCurrentLoans() }
            END BUTTON

            DEFINE BUTTON btnOverdue
                ROW 60
                COL 20
                WIDTH 150
                HEIGHT 30
                CAPTION "Overdue Books"
                ACTION {|| ::showOverdueBooks() }
            END BUTTON

            DEFINE BUTTON btnInventory
                ROW 100
                COL 20
                WIDTH 150
                HEIGHT 30
                CAPTION "Inventory Status"
                ACTION {|| ::showInventoryStatus() }
            END BUTTON

            DEFINE BUTTON btnUserActivity
                ROW 140
                COL 20
                WIDTH 150
                HEIGHT 30
                CAPTION "User Activity"
                ACTION {|| ::showUserActivity() }
            END BUTTON

        END WINDOW

        CENTER WINDOW ReportForm
        ACTIVATE WINDOW ReportForm
    ELSE
        MsgInfo("Access restricted to Librarians and Admins")
    ENDIF
RETURN NIL

/**
 * Displays current loans report
 * return void
 */
METHOD showCurrentLoans() CLASS ReportManager

    LOCAL aData := {}
    LOCAL oError

    TRY
        altd()
        ::oLoans:Open()
        ::oLoans:SetOrder("RETURNDATE")
        ::oLoans:Gotop()
        DO WHILE Empty(::oLoans:GetValue("RETURNDATE" )) .AND. !::oLoans:Eof()
            AAdd(aData, { ::oLoans:GetValue("LOANID") , ::oLoans:GetValue("USERID") , ::oLoans:GetValue("ISBN"), DToC( ::oLoans:GetValue("CHECKOUT") ), DToC( ::oLoans:GetValue("DUEDATE") )})
            ::oLoans:SKIP()
        ENDDO
        ::oLoans:Close()
        ::displayReport("Current Loans", {"Loan ID", "User ID", "ISBN", "Checkout", "Due Date"}, aData)
    CATCH oError
        MsgStop("Database verification failed: " + oError:description +" - " +oError:filename + " - " +oError:operation )
    END

RETURN NIL

/**
 * Displays overdue books report
 * return void
 */
METHOD showOverdueBooks() CLASS ReportManager

    LOCAL aData  := {}
    LOCAL dToday := Date()
    LOCAL oError
    altd()
    TRY
        ::oLoans:Open()
        ::oLoans:SetOrder("DUEDATE")
        ::oLoans:Gotop()
        DO WHILE !Empty(::oLoans:GetValue("DUEDATE") ) .AND. dToday > ::oLoans:GetValue("DUEDATE")  .AND. Empty( ::oLoans:GetValue("RETURNDATE") )
             AAdd(aData, { ::oLoans:GetValue("LOANID") , ::oLoans:GetValue("USERID") , ::oLoans:GetValue("ISBN") , DToC( ::oLoans:GetValue("DUEDATE") ), Str((dToday - ::oLoans:GetValue("DUEDATE") ) * ConfigManager():New("library.ini"):getConfig("LATE_FEE", 1))})
            ::oLoans:SKIP()
        ENDDO
        ::oLoans:Close()
        ::displayReport("Overdue Books", {"Loan ID", "User ID", "ISBN", "Due Date", "Late Fee"}, aData)
    CATCH oError
        MsgStop("Database verification failed: " + oError:description)
    END

RETURN NIL

/**
 * Displays inventory status report
 * return void
 */
METHOD showInventoryStatus() CLASS ReportManager

    LOCAL aData := {}
    LOCAL oError

    TRY
        ::oBooks:Open()
        DO WHILE  !::oBooks:Eof()
            AAdd(aData, { ::oBooks:GetValue("ISBN") , ::oBooks:GetValue("TITLE") , ::oBooks:GetValue("AUTHOR") , ::oBooks:GetValue("STATUS") })
            ::oBooks:SKIP()
        ENDDO
        ::oBooks:Close()
        ::displayReport("Inventory Status", {"ISBN", "Title", "Author", "Status"}, aData)
    CATCH oError
        MsgStop("Database verification failed: " + oError:description)
    END

RETURN NIL

/**
 * Displays user activity report
 * return void
 */
METHOD showUserActivity() CLASS ReportManager

    LOCAL aData := {}
    LOCAL oError

    TRY
        ::oLoans:Open()
        DO WHILE !::oLoans:Eof()
            AAdd(aData, { ::oLoans:GetValue("USERID") , ::oLoans:GetValue("ISBN") , DToC( ::oLoans:GetValue("CHECKOUT") ), DToC( ::oLoans:GetValue("DUEDATE")), iif(Empty( ::oLoans:GetValue("RETURNDATE") ), "", DToC( ::oLoans:GetValue("RETURNDATE")))})
            ::oLoans:SKIP()
        ENDDO
        ::oLoans:Close()
        ::displayReport("User Activity", {"User ID", "ISBN", "Checkout", "Due Date", "Return Date"}, aData)
    CATCH
        MsgStop("Database verification failed: " + oError:description)
    END

RETURN NIL

METHOD displayReport(cTitle, aHeaders, aData)

    DEFINE WINDOW ReportDisplay ;
        AT 0,0 WIDTH 800 HEIGHT 600 ;
        TITLE cTitle ;
        MODAL

        DEFINE GRID grdReport
            ROW 20
            COL 20
            WIDTH 760
            HEIGHT 540
            HEADERS aHeaders
            WIDTHS {100, 100, 100, 100, 100}
            ITEMS aData
        END GRID

    END WINDOW

    ACTIVATE WINDOW ReportDisplay

RETURN NIL
