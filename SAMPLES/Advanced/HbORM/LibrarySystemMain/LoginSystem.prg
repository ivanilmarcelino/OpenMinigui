/*

 BadaSystem
 Program       : LibrarySystemMain
 Module        : LoginSystem Class
 Compiler      : MINIGUI - Harbour Win32 GUI
 Compiler-C    : BCC 32 bit
 Author        : Marcos Jarrín
 Email         : marvijarrin@gmail.com
 Date          : 15/07/2025
 Update        : 03/08/2025
 Rev           : 0.1

*/

#include "hbclass.ch"
#include "minigui.ch"

// ----------------------------------------------------------------------------
// LoginSystem Class
// ----------------------------------------------------------------------------
CLASS LoginSystem
    DATA cCurrentUserID AS STRING
    DATA cCurrentRole   AS STRING
    DATA oUsers         AS OBJECT

    METHOD New() CONSTRUCTOR
    METHOD initialize()
    METHOD login(cUserID, cPassword)
    METHOD logout()
ENDCLASS

METHOD New() CLASS LoginSystem
    ::cCurrentUserID := "admin"
    ::cCurrentRole   := "ADMIN"
    ::oUsers         := HbORM():New("users", "users", "data\")
RETURN Self

/**
 * Initializes login system
 * return void
 */
METHOD initialize() CLASS LoginSystem

    LOCAL oError

    TRY
        ::oUsers:Open()
        IF ::oUsers:Eof()
            ::oUsers:Insert({;
                 "USERID"   => "admin", ;
                 "NAME"     => "Administrator", ;
                 "EMAIL"    => "admin@library.com", ;
                 "ROLE"     => "ADMIN", ;
                 "PASSWORD" => "admin"  ;
                })
        ENDIF
        ::oUsers:Close()
     CATCH oError
         MsgStop("Database verification failed: " + oError:description + oError:filename )
     END

RETURN NIL

/**
 * Performs login authentication
 * param cUserID String User ID
 * param cPassword String Password
 * return Logical Success status
 */
METHOD login(cUserID, cPassword) CLASS LoginSystem

    LOCAL lSuccess := .F.
    LOCAL oError

    TRY
        ::oUsers:Open()
        IF ::oUsers:Seek(cUserID) .AND. users->PASSWORD == cPassword
            ::cCurrentUserID := cUserID
            ::cCurrentRole   := users->ROLE
            lSuccess         := .T.
        ELSE
            MsgStop("Invalid user ID or password")
        ENDIF
        ::oUsers:Close()
    CATCH oError
        MsgStop("Database verification failed: " + oError:description)
    END

RETURN lSuccess

/**
 * Performs logout
 * return void
 */
METHOD logout() CLASS LoginSystem
    ::cCurrentUserID := ""
    ::cCurrentRole   := ""
RETURN NIL
