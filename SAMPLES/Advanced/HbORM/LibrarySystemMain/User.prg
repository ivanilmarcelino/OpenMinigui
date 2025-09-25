/*

 BadaSystem
 Program       : LibrarySystemMain
 Module        : User Class
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
// User Class
// ----------------------------------------------------------------------------
CLASS User
    DATA USERID    AS STRING
    DATA NAME      AS STRING
    DATA EMAIL     AS STRING
    DATA ROLE      AS STRING
    DATA PASSWORD  AS STRING
    DATA oUsers    AS OBJECT
    DATA oBooks    AS OBJECT
    DATA oLoans    AS OBJECT

    METHOD New(cID, cName, cEmail, cRole, cPassword) CONSTRUCTOR
    METHOD checkOutBook(cISBN)
    METHOD viewLoanHistory()
    METHOD add()
    METHOD update()
    METHOD delete()
ENDCLASS

METHOD New(cID, cName, cEmail, cRole, cPassword) CLASS User
    ::USERID   := cID
    ::NAME     := cName
    ::EMAIL    := cEmail
    ::ROLE     := cRole
    ::PASSWORD := cPassword
    ::oUsers   := HbORM():New("users", "users", "data\")
    ::oBooks   := HbORM():New("books", "books", "data\")
    ::oLoans   := HbORM():New("loans", "loans", "data\")
RETURN Self

/**
 * Checks out a book for this user
 * param cISBN String Book ISBN
 * return Logical Success status
 */
METHOD checkOutBook(cISBN) CLASS User

    LOCAL oLoan := Loan():New(GenerateUniqueID(), ::USERID, cISBN, Date(), Date() + ConfigManager():New("library.ini"):getConfig("LOAN_DAYS", 14), Date() )
    LOCAL lReturn
    LOCAL oError

    TRY
        AltD()
        ::oBooks:Open()
        IF ::oBooks:Seek( cISBN ) .AND. ALLTRIM( ::oBooks:GetValue("STATUS") ) == "AVAILABLE"
            ::oBooks:SetValue("STATUS","CHECKED_OUT")
            ::oLoans:Open()
            ::oLoans:Insert({;
                    "LOANID"   => oLoan:LOANID  , ;
                    "USERID"   => oLoan:USERID  , ;
                    "ISBN"     => oLoan:ISBN    , ;
                    "CHECKOUT" => oLoan:CHECKOUT, ;
                    "DUEDATE"  => oLoan:DUEDATE ;
                })
            ::oLoans:Close()
            ::oBooks:Close()
            lReturn := .T.
            MSGBOX("Borrowed Book")
        ELSE
            ::oBooks:Close()
            lReturn := .F.
            MSGBOX("Book Not Available")
        ENDIF
    CATCH oError
        MsgStop("Database verification failed: " + oError:description)
    END

RETURN lReturn

/**
 * Retrieves loan history
 * return Array of Loan objects
 */
METHOD viewLoanHistory() CLASS User

    LOCAL aHistory := {}
    LOCAL oError

    TRY
        ::oLoans:Open()
        ::oLoans:Seek(::USERID,"USERID")
        DO WHILE !::oLoans:EOF() .AND. ::oLoans:GetValue("USERID")  == ::USERID
            AAdd(aHistory, Loan():New( ::oLoans:GetValue("LOANID")  , ::oLoans:GetValue("USERID") , ::oLoans:GetValue("ISBN") , ::oLoans:GetValue("CHECKOUT") , ::oLoans:GetValue("DUEDATE") , ::oLoans:GetValue("RETURNDATE") ))
            ::oLoans:Skip()
        ENDDO
        ::oLoans:Close()
    CATCH oError
        MsgStop("Database verification failed: " + oError:description)
    END

RETURN aHistory

/**
 * Adds a new user to the database
 * return Logical Success status
 */
METHOD add() CLASS User

    LOCAL lReturn
    LOCAL oError

    TRY
        ::oUsers:Open()
        IF !::oUsers:Seek(::USERID )
            ::oUsers:Insert({;
                    "USERID"   => ::USERID  ,;
                    "NAME"     => ::NAME    ,;
                    "EMAIL"    => ::EMAIL   ,;
                    "ROLE"     => ::ROLE    ,;
                    "PASSWORD" => ::PASSWORD ;
                })
            ::oUsers:Close()
            lReturn := .T.
            MSGBOX("User added")
        ELSE
            ::oUsers:Close()
            lReturn := .F.
            MSGBOX("User not added")
        ENDIF
    CATCH oError
        MsgStop("Database verification failed: " + oError:description)
    END

RETURN lReturn

/**
 * Updates user information
 * return Logical Success status
 */
METHOD update() CLASS User

    LOCAL lReturn := .T.
    LOCAL oError

    TRY
        ::oUsers:Open()
        IF ::oUsers:Seek(::USERID)
            ::oUsers:Update({;
                    "NAME"     => ::NAME    ,;
                    "EMAIL"    => ::EMAIL   ,;
                    "ROLE"     => ::ROLE    ,;
                    "PASSWORD" => ::PASSWORD ;
                })
            ::oUsers:Close()
            lReturn := .T.
            MSGBOX("User updated")
        ELSE
            ::oUsers:Close()
            MSGBOX("User not updated")
            lReturn := .F.
        ENDIF
    CATCH oError
        MsgStop("Database verification failed: " + oError:description)
    END

RETURN lReturn

/**
 * Deletes user from database
 * return Logical Success status
 */
METHOD delete() CLASS User

    LOCAL lReturn := .T.
    LOCAL oError

    TRY
        ::oUsers:Open(.F.)
        IF ::oUsers:Seek( ::USERID )
            ::oLoans:Open()
            IF !::oLoans:Seek(::USERID,"USERID") .AND. Empty( ::oLoans:GetValue("RETURNDATE")  )
                ::oUsers:Delete()
                ::oUsers:Close()
                lReturn := .T.
                MSGBOX("User Deleted")
            ELSE
                MSGBOX("User cannot be deleted, has books on loan")
            ENDIF
            ::oLoans:Close()
            ::oUsers:Close()
        ELSE
            ::oUsers:Close()
            MSGBOX("User not found")
            lReturn := .F.
        ENDIF
    CATCH oError
        MsgStop("Database verification failed: " + oError:description)
    END

RETURN lReturn
