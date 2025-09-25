/*

 BadaSystem
 Program       : LibrarySystemMain
 Module        : LibrarySystem Class
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
// LibrarySystem Class
// ----------------------------------------------------------------------------
CLASS LibrarySystem
    DATA config     AS OBJECT   // ConfigManager instance
    DATA auth       AS OBJECT   // LoginSystem instance
    DATA oUsers     AS OBJECT
    DATA oEmployees AS OBJECT
    DATA oBooks     AS OBJECT
    DATA oLoans     AS OBJECT
    DATA mainWindow AS OBJECT   // Main MDI window

    METHOD New() CONSTRUCTOR
    METHOD startApplication()
    METHOD CreateTables()

ENDCLASS

METHOD New() CLASS LibrarySystem
    ::config := ConfigManager():New("library.ini")
    ::auth   := LoginSystem():New()
RETURN Self

/**
 * Starts the library application
 * return void
 */
METHOD startApplication() CLASS LibrarySystem

    LOCAL oError

    TRY
        ::CreateTables()
        ::config:WriteIni( hash("LOAN_DAYS", 14,"LATE_FEE", 1) )
        ::config:loadConfig()
        ::auth:initialize()

        DEFINE WINDOW MainForm ;
            AT 0,0 ;
            WIDTH 560 ;
            HEIGHT 400 ;
            TITLE "Library Management System" ;
            MAIN ;
            NOMAXIMIZE ;
            NOSIZE

            DEFINE IMAGE img_wallpaper
                ROW 045
                COL 000
                HEIGHT 1400 
                WIDTH  560 
                PICTURE "library"
                ADJUSTIMAGE .T.
            END IMAGE
            
            DEFINE SPLITBOX
                DEFINE TOOLBAR tlFile BUTTONSIZE 35, 35 FLAT

                BUTTON btBook ;
                    TOOLTIP 'Book' ;
                    PICTURE "book"   ;
                    ACTION {|| BookManager():New():showWindow(::auth:cCurrentRole) }

                BUTTON btUsers ;
                    TOOLTIP 'User'  ;
                    PICTURE 'user' ;
                    ACTION {|| UserManager():New():showWindow(::auth:cCurrentRole) }

                BUTTON btEmployees ;
                    TOOLTIP 'Employees' ;
                    PICTURE 'employees' ;
                    ACTION {|| iif(::auth:cCurrentRole == "ADMIN", EmployeeManager():New():showWindow(), MsgInfo("Admin access required")) }

                BUTTON btLoans ;
                    TOOLTIP 'Loans'  ;
                    PICTURE 'loan' ;
                    ACTION {|| LoanManager():New():showWindow(::auth:cCurrentRole) }

                BUTTON btReports ;
                    TOOLTIP 'Reports'  ;
                    PICTURE 'reports' ;
                    ACTION {|| ReportManager():New():showWindow(::auth:cCurrentRole) }

                BUTTON btExit ;
                    TOOLTIP 'Exit'  ;
                    PICTURE 'exit' ;
                    ACTION  {|| ::auth:logout() , MainForm.release }
  
                END TOOLBAR

            END SPLITBOX

            DEFINE MAIN MENU
                POPUP "&File"
                    MENUITEM "&Books" ;
                        ACTION {|| BookManager():New():showWindow(::auth:cCurrentRole) }
                    MENUITEM "&Users" ;
                        ACTION {|| UserManager():New():showWindow(::auth:cCurrentRole) }
                    MENUITEM "&Employees" ;
                        ACTION {|| iif(::auth:cCurrentRole == "ADMIN", EmployeeManager():New():showWindow(), MsgInfo("Admin access required")) }
                    MENUITEM "&Loans" ;
                        ACTION {|| LoanManager():New():showWindow(::auth:cCurrentRole) }
                    MENUITEM "&Reports" ;
                        ACTION {|| ReportManager():New():showWindow(::auth:cCurrentRole) }
                    MENUITEM "&Exit" ;
                         ACTION {|| ::auth:logout() , MainForm.release  }
                END POPUP

                POPUP "&Exit"
                    MENUITEM "&Exit" ;
                        ACTION  {|| ::auth:logout(), MainForm.release  }
                END POPUP

            END MENU

        END WINDOW

        CENTER WINDOW MainForm
        ACTIVATE WINDOW MainForm
    CATCH oError
        MsgStop("Application failed to start: " + oError:description)
    END

RETURN NIL

/*
Create all necessary database tables for system operation.
*/
METHOD CreateTables()

    LOCAL oError
    LOCAL lSuccess := .T.
    LOCAL cPath   := "data"

    /**
     * Creates the data directory if it does not exist.
     */
    IF !isdir( cPath)
         dirmake( cPath )
    ENDIF

    // Create instances of the tables
    TRY
        ::oUsers      := HbORM():New("users",      "users",    "data\")
        ::oEmployees  := HbORM():New("employeesn", "employees","data\")
        ::oBooks      := HbORM():New("books",      "books",    "data\")
        ::oLoans      := HbORM():New("loans",      "loans",    "data\")

       /**
        * Creates the Users table if it does not exist.
        */
       IF !::oUsers:Exists()
          ::oUsers:Create({ ;
            {"USERID",   "C", 10, 0}, ;
            {"NAME",     "C", 50, 0}, ;
            {"EMAIL",    "C", 50, 0}, ;
            {"ROLE",     "C", 10, 0}, ;
            {"PASSWORD", "C", 20, 0}  ;
            })
          ::oUsers:AddIndex("USERID", "USERID")
       ENDIF

       /**
        * Creates the Employees table if it does not exist.
        */
       IF !::oEmployees:Exists()
          ::oEmployees:Create({ ;
                {"EMPID",    "C", 10, 0}, ;
                {"NAME",     "C", 50, 0}, ;
                {"POSITION", "C", 20, 0}  ;
                })
           ::oEmployees:AddIndex("EMPID", "EMPID")
       ENDIF

       /**
        * Creates the Books table if it does not exist.
        */
       IF !::oBooks:Exists()
          ::oBooks:Create({ ;
                {"ISBN",   "C",  17, 0}, ;
                {"TITLE",  "C", 100, 0}, ;
                {"AUTHOR", "C",  50, 0}, ;
                {"STATUS", "C",  11, 0}  ;
                })
           ::oBooks:AddIndex("ISBN", "ISBN")
       ENDIF

       /**
        * Creates the Loans table if it does not exist.
        */
       IF !::oLoans:Exists()
          ::oLoans:Create({ ;
                {"LOANID",     "C", 10, 0}, ;
                {"USERID",     "C", 10, 0}, ;
                {"ISBN",       "C", 17, 0}, ;
                {"CHECKOUT",   "D",  8, 0}, ;
                {"DUEDATE",    "D",  8, 0}, ;
                {"RETURNDATE", "D",  8, 0} ;
                })
           ::oLoans:AddIndex("LOANID", "LOANID")
           ::oLoans:AddIndex("USERID", "USERID")
           ::oLoans:AddIndex("ISBN"  , "ISBN")
           ::oLoans:AddIndex("RETURNDATE","RETURNDATE")
           ::oLoans:AddIndex("DUEDATE","DUEDATE")
       ENDIF

        ::oUsers:Close()
        ::oEmployees:Close()
        ::oBooks:Close()
        ::oLoans:Close()

     CATCH oError
         MsgStop("Database verification failed: " + oError:description)
         lSuccess := .F.
     END

RETURN lSuccess
