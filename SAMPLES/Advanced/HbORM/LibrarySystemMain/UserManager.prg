/*

 BadaSystem
 Program       : LibrarySystemMain
 Module        : UserManager Class
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
// UserManager Class
// ----------------------------------------------------------------------------
CLASS UserManager
    METHOD New() CONSTRUCTOR
    METHOD showWindow(cRole)
    METHOD scanUser(cUserID)
    METHOD ValidateEmail()  PROTECTED
ENDCLASS

METHOD New() CLASS UserManager
RETURN Self

/**
 * Shows user management window
 * param cRole String User role for access control
 * return void
 */
METHOD showWindow(cRole) CLASS UserManager

    IF cRole $ "ADMIN,LIBRARIAN"
        DEFINE WINDOW UserForm ;
            AT 0,0 WIDTH 400 HEIGHT 250 ;
            TITLE "User Management" ;
            CHILD

            ON KEY ESCAPE ACTION UserForm.Release

            @ 20, 20 LABEL lblUserID VALUE "User ID:" WIDTH 100 HEIGHT 20
            @ 20,120 TEXTBOX txtUserID WIDTH 150 HEIGHT 20 PLACEHOLDER "COD001" UPPER MAXLENGTH 10

            @ 50, 20 LABEL lblName VALUE "Name:" WIDTH 100 HEIGHT 20
            @ 50,120 TEXTBOX txtName WIDTH 250 HEIGHT 20 PLACEHOLDER "Will Smith" MAXLENGTH 50

            @ 80, 20 LABEL lblEmail VALUE "Email:" WIDTH 100 HEIGHT 20
            @ 80,120 TEXTBOX txtEmail WIDTH 250 HEIGHT 20 PLACEHOLDER "willsmith@gmail.com" LOWER MAXLENGTH 50 ON ENTER ::ValidateEmail()

            @110, 20 LABEL lblPassword VALUE "Password:" WIDTH 100 HEIGHT 20
            @110,120 TEXTBOX txtPassword WIDTH 170 HEIGHT 20 PASSWORD MAXLENGTH 20

            DEFINE BUTTON btnAdd
                ROW 150
                COL 20
                WIDTH 80
                HEIGHT 30
                CAPTION "Add"
                ACTION {|| User():New(UserForm.txtUserID.Value, UserForm.txtName.Value, UserForm.txtEmail.Value, "LIBRARIAN", UserForm.txtPassword.Value):add(), ;
                         UserForm.txtUserID.Value := "", UserForm.txtName.Value := "", UserForm.txtEmail.Value := "", UserForm.txtPassword.Value := "" }
            END BUTTON

            DEFINE BUTTON btnUpdate
                ROW 150
                COL 110
                WIDTH 80
                HEIGHT 30
                CAPTION "Update"
                ACTION {|| User():New(UserForm.txtUserID.Value, UserForm.txtName.Value, UserForm.txtEmail.Value, "LIBRARIAN", UserForm.txtPassword.Value):update(), ;
                         UserForm.txtUserID.Value := "", UserForm.txtName.Value := "", UserForm.txtEmail.Value := "", UserForm.txtPassword.Value := "" }
            END BUTTON

            DEFINE BUTTON btnDelete
                ROW 150
                COL 200
                WIDTH 80
                HEIGHT 30
                CAPTION "Delete"
                ACTION {|| User():New(UserForm.txtUserID.Value, "", "", "", ""):delete(), UserForm.txtUserID.Value := "",;
                           UserForm.txtUserID.Value := "", UserForm.txtName.Value := "", UserForm.txtEmail.Value := "", UserForm.txtPassword.Value := "" }
            END BUTTON

            DEFINE BUTTON btnScan
                ROW 150
                COL 290
                WIDTH 80
                HEIGHT 30
                CAPTION "Scan User"
                ACTION {|| ::scanUser( UserForm.txtUserID.Value ), UserForm.txtUserID.Value := ""  }
            END BUTTON

        END WINDOW

        CENTER WINDOW UserForm
        ACTIVATE WINDOW UserForm
    ELSE
        MsgInfo("Access restricted to Librarians and Admins")
    ENDIF

RETURN NIL

METHOD scanUser(cUserID) CLASS UserManager


    USE users INDEX users
        IF dbSeek( cUserID )
            UserForm.txtUserID.Value   := users->USERID
            UserForm.txtName.Value     := users->NAME
            UserForm.txtEmail.Value    := users->EMAIL
            UserForm.txtPassword.Value := users->PASSWORD
            USE
            RETURN  User():New(users->USERID, users->NAME, users->EMAIL, "LIBRARIAN", users->PASSWORD)
        ELSE
            MsgInfo("User not found")
            UserForm.txtUserID.Value   := ""
            UserForm.txtName.Value     := ""
            UserForm.txtEmail.Value    := ""
            UserForm.txtPassword.Value := ""
        ENDIF
    USE

RETURN NIL

METHOD ValidateEmail() CLASS UserManager

   LOCAL cRegEx  := '^[^@]+@[^@]+\.[a-zA-Z]{2,}$'
   LOCAL cEmail  := ""
   LOCAL lReturn := .T.

   cEmail := AllTrim( UserForm.txtEmail.value )
   if ! hb_regexMatch( cRegEx, cEmail )
      msginfo( "Incorrect Email" )
      UserForm.txtEmail.SetFocus
      lReturn := .F.
   ELSE
     lReturn := .T.
     UserForm.txtPassword.SetFocus
   ENDIF

RETURN lReturn
