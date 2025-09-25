/*

 BadaSystem
 Program       : LibrarySystemMain
 Module        : LoanManager Class
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
// LoanManager Class
// ----------------------------------------------------------------------------
CLASS LoanManager
    DATA oLoans AS OBJECT
    DATA oBooks AS OBJECT

    METHOD New() CONSTRUCTOR
    METHOD showWindow(cRole)
    METHOD processReturn(cUserID, cISBN)
ENDCLASS

METHOD New() CLASS LoanManager
      ::oLoans := HbORM():New("loans", "loans", "data\")
      ::oBooks := HbORM():New("books", "books", "data\")
RETURN Self

/**
 * Shows loan management window
 * param cRole String User role for access control
 * return void
 */
METHOD showWindow(cRole) CLASS LoanManager

    DEFINE WINDOW LoanForm ;
        AT 0,0 WIDTH 300 HEIGHT 180 ;
        TITLE "Loan Management" ;
        CHILD ;
        ON RELEASE dbCloseAll()

        ON KEY ESCAPE ACTION LoanForm.Release

        @ 20, 20 LABEL lblUserID VALUE "User ID:" WIDTH 100 HEIGHT 20
        @ 20,120 TEXTBOX txtUserID WIDTH 150 HEIGHT 20 MAXLENGTH 10 UPPER PLACEHOLDER "COD001"

        @ 50, 20 LABEL lblISBN VALUE "ISBN:" WIDTH 100 HEIGHT 20
        @ 50,120 TEXTBOX txtISBN WIDTH 150 HEIGHT 20 INPUTMASK "999-9-99-999999-9" PLACEHOLDER "978-3-16-148410-0"

        DEFINE BUTTON btnCheckout
            ROW 80
            COL 20
            WIDTH 80
            HEIGHT 30
            CAPTION "Checkout"
            ACTION  {|| iif(cRole $ "ADMIN,LIBRARIAN", User():New(LoanForm.txtUserID.Value, "", "", "", ""):checkOutBook(LoanForm.txtISBN.Value), MsgInfo("Access restricted")),;
              LoanForm.txtUserID.value := "" , LoanForm.txtISBN.value := "" }
        END BUTTON

        DEFINE BUTTON btnReturn
            ROW 80
            COL 110
            WIDTH 80
            HEIGHT 30
            CAPTION "Return"
            ACTION {|| iif(cRole $ "ADMIN,LIBRARIAN", ::processReturn(LoanForm.txtUserID.Value, LoanForm.txtISBN.Value), MsgInfo("Access restricted")) ,;
                   LoanForm.txtUserID.value := "" , LoanForm.txtISBN.value := "" }
        END BUTTON

    END WINDOW

    CENTER WINDOW LoanForm
    ACTIVATE WINDOW LoanForm

RETURN NIL

/**
 * Processes book return by user ID and ISBN
 * param cUserID String User ID
 * param cISBN String Book ISBN
 * return Logical Success status
 */
METHOD processReturn(cUserID, cISBN) CLASS LoanManager

    LOCAL oLoan
    LOCAL lReturn
    LOCAL oError

    TRY
        AltD()
        ::oLoans:Open()
        IF ::oLoans:Seek(cUserID,"USERID" ,.T.) .AND. ALLTRIM(::oLoans:GetValue("ISBN")) == ALLTRIM(cISBN) .AND. Empty( ::oLoans:GetValue( "RETURNDATE") )
            ::oLoans:SetValue( "RETURNDATE", Date() )
            ::oBooks:Open()
                IF ::oBooks:Seek( alltrim( LoanForm.txtISBN.value )  )
                    ::oBooks:SetValue("STATUS","AVAILABLE" )
                ENDIF
            ::oBooks:Close()
            MSGBOX("Succeed")
            lReturn := .T.
        ELSE
            ::oLoans:Close()
            lReturn := .F.
            MSGBOX("Not Ready")
        ENDIF

    CATCH oError
        MsgStop("Database verification failed loans: " + oError:description)
    END

RETURN lReturn
