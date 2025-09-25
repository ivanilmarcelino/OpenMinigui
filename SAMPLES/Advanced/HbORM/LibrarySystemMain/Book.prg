/*

 BadaSystem
 Program       : LibrarySystemMain
 Module        : Book Class
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
// Book Class
// ----------------------------------------------------------------------------
CLASS Book
    DATA ISBN      AS STRING
    DATA TITLE     AS STRING
    DATA AUTHOR    AS STRING
    DATA STATUS    AS STRING
    DATA oBooks    AS OBJECT

    METHOD New(cISBN, cTitle, cAuthor) CONSTRUCTOR
    METHOD checkAvailability()
    METHOD calculateLateFees()
    METHOD add()
    METHOD update()
    METHOD delete()
ENDCLASS

METHOD New(cISBN, cTitle, cAuthor) CLASS Book
    ::ISBN   := cISBN
    ::TITLE  := cTitle
    ::AUTHOR := cAuthor
    ::STATUS := "AVAILABLE"
    ::oBooks := HbORM():New("books", "books", "data\")
RETURN Self

/**
 * Checks if book is available
 * return Logical Availability status
 */
METHOD checkAvailability() CLASS Book

    LOCAL lAvailable := .F.
    LOCAL oError

    TRY
        ::oBooks:Open()
        IF ::oBooks:Seek(::ISBN)
            ::STATUS   := ::oBooks:GetValue( "STATUS" )
            lAvailable := ::oBooks:GetValue( "STATUS" )  == "AVAILABLE"
        ENDIF
        ::oBooks:Close()
    CATCH oError
        MsgStop("Database verification failed: " + oError:description)
    END

RETURN lAvailable

/**
 * Calculates late fees for this book
 * return Numeric Fee amount
 */
METHOD calculateLateFees() CLASS Book

    LOCAL nFee   := 0
    LOCAL dToday := Date()
    LOCAL oError
    LOCAL oLoans

    oLoans := HbORM():New("loans", "loans", "data\")

    TRY
        oLoans:Open()
        IF oLoans:Seek(::ISBN) .AND. Empty( ::oLoans:GetValue("RETURNDATE") ) .AND. dToday > ::oLoans:GetValue( "DUEDATE")
            nFee := ConfigManager():New("library.ini"):getConfig("LATE_FEE", 1) * (dToday - ::oLoans:GetValue( "DUEDATE") )
        ENDIF
    CATCH oError
        MsgStop("Database verification failed: " + oError:description)
    END

RETURN nFee

/**
 * Adds a new book to the database
 * return Logical Success status
 */
METHOD add() CLASS Book

    LOCAL lReturn
    LOCAL oError

    TRY
        ::oBooks:Open()
        IF !::oBooks:Seek( ::ISBN )
            ::oBooks:Insert({;
                    "ISBN"   => ::ISBN,  ;
                    "TITLE"  => ::TITLE, ;
                    "AUTHOR" => ::AUTHOR,;
                    "STATUS" => ::STATUS ;
                })
            ::oBooks:Close()
            lReturn := .T.
            MSGBOX("Book added")
        ELSE
            MSGBOX("Book already exists, not added")
            ::oBooks:Close()
            lReturn := .F.
        ENDIF
    CATCH
        MsgStop("Database verification failed: " + oError:description)
    END

RETURN lReturn

/**
 * Updates book information
 * return Logical Success status
 */
METHOD update() CLASS Book

    LOCAL lReturn
    LOCAL oError

    TRY
        ::oBooks:Open()
        IF ::oBooks:Seek( ::ISBN )
            ::oBooks:Update({;
                    "TITLE"  => ::TITLE, ;
                    "AUTHOR" => ::AUTHOR,;
                    "STATUS" => ::STATUS ;
                })
            ::oBooks:Close()
            MSGBOX("Book Update")
            lReturn := .T.
        ELSE
            lReturn := .F.
            ::oBooks:Close()
            MSGBOX("Book is not available to Update")
        ENDIF
    CATCH
        MsgStop("Database verification failed: " + oError:description)
    END

RETURN lReturn

/**
 * Deletes book from database
 * return Logical Success status
 */
METHOD delete() CLASS Book
    
    LOCAL lReturn
    LOCAL oError

    TRY
        ::oBooks:Open(.F.)
        IF ::oBooks:Seek(::ISBN  ) .AND. ALLTRIM( ::oBooks:GetValue( "STATUS") ) == "AVAILABLE"
            ::oBooks:Delete()
            ::oBooks:Close()
            MSGBOX("Book deleted successfully")
            lReturn := .T.
        ELSE
            MSGBOX("Book is not available to delete")
            lReturn := .F.
            ::oBooks:Close()
        ENDIF
    CATCH
        MsgStop("Database verification failed: " + oError:description)
    END
    
RETURN lReturn
