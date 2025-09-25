/*

 BadaSystem
 Program       : LibrarySystemMain
 Module        : Loan Class
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
// Loan Class
// ----------------------------------------------------------------------------
CLASS Loan
    DATA LOANID       AS STRING
    DATA USERID       AS STRING
    DATA ISBN         AS STRING
    DATA CHECKOUT     AS DATE
    DATA DUEDATE      AS DATE
    DATA RETURNDATE   AS DATE
    DATA oLoans       AS OBJECT
    DATA oBooks       AS OBJECT

    METHOD New(cLoanID, cUserID, cISBN, dCheckout, dDue, dReturn) CONSTRUCTOR
    METHOD processReturn()
ENDCLASS

METHOD New(cLoanID, cUserID, cISBN, dCheckout, dDue, dReturn) CLASS Loan
    ::LOANID     := cLoanID
    ::USERID     := cUserID
    ::ISBN       := cISBN
    ::CHECKOUT   := dCheckout
    ::DUEDATE    := dDue
    ::RETURNDATE := dReturn
    ::oLoans     := HbORM():New("loans", "loans", "data\")
    ::oBooks     := HbORM():New("books", "books", "data\")
RETURN Self

/**
 * Processes book return
 * return Logical Success status
 */
METHOD processReturn() CLASS Loan

    LOCAL lReturn
    LOCAL oError

    TRY
        ::oLoans:Open()
        IF ::oLoans:Seek( ::LOANID ) .AND. Empty( ::oLoans:GetValue("RETURNDATE") )
             ::oLoans:SetValue("RETURNDATE", Date())
             ::oBooks:Open()
            IF ::oBooks:Seek(::ISBN)
                ::oBooks:SetValue("STATUS","AVAILABLE")
            ENDIF
            ::oBooks:Close()
            lReturn := .T.
        ELSE
            lReturn := .F.
        ENDIF
        ::oLoans:Close()
    CATCH
        MsgStop("Database verification failed class loans: " + oError:description)
    END

RETURN lReturn
