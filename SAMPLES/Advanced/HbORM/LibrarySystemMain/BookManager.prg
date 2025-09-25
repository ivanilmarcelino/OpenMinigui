/*

 BadaSystem
 Program       : LibrarySystemMain
 Module        : BookManager Class
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
// BookManager Class
// ----------------------------------------------------------------------------
CLASS BookManager
    DATA oBooks AS OBJECT

    METHOD New() CONSTRUCTOR
    METHOD showWindow(cRole)
    METHOD scanIsbn(cIsbn)
ENDCLASS

METHOD New() CLASS BookManager
    ::oBooks      := HbORM():New("books", "books", "data\")
RETURN Self

/**
 * Shows book management window
 * param cRole String User role for access control
 * return void
 */
METHOD showWindow(cRole) CLASS BookManager

    IF cRole $ "ADMIN,LIBRARIAN"
        DEFINE WINDOW BookForm ;
            AT 0,0 ;
            WIDTH 430 ;
            HEIGHT 230 ;
            TITLE "Book Management" ;
            MODAL
        
            ON KEY ESCAPE ACTION BookForm.Release

            @20, 20 LABEL   lblISBN VALUE "ISBN:" WIDTH 100 HEIGHT 20
            @20,120 TEXTBOX txtISBN WIDTH 130 HEIGHT 20  INPUTMASK "999-9-99-999999-9" PLACEHOLDER "978-3-16-148410-0"

            @50, 20 LABEL   lblTitle VALUE "Title:" WIDTH 100 HEIGHT 20
            @50,120 TEXTBOX txtTitle WIDTH 280 HEIGHT 20 MAXLENGTH 100 PLACEHOLDER "OOP Harbor Programming"

            @80, 20 LABEL   lblAuthor VALUE "Author:" WIDTH 100 HEIGHT 20
            @80,120 TEXTBOX txtAuthor WIDTH 280 HEIGHT 20 MAXLENGTH 50 PLACEHOLDER "Acme"

            DEFINE BUTTON btnAdd
                ROW 120
                COL  20
                WIDTH  80
                HEIGHT 30
                CAPTION "Add"
                ACTION {|| Book():New(BookForm.txtISBN.Value, BookForm.txtTitle.Value, BookForm.txtAuthor.Value):add(), BookForm.txtISBN.Value := "", BookForm.txtTitle.Value := "", BookForm.txtAuthor.Value := "" }
            END BUTTON

            DEFINE BUTTON btnUpdate
                ROW 120
                COL 110
                WIDTH 80
                HEIGHT 30
                CAPTION "Update"
                ACTION {|| Book():New(BookForm.txtISBN.Value, BookForm.txtTitle.Value, BookForm.txtAuthor.Value):update(), BookForm.txtISBN.Value := "", BookForm.txtTitle.Value := "", BookForm.txtAuthor.Value := ""  }
            END BUTTON

            DEFINE BUTTON btnDelete
                ROW 120
                COL 200
                WIDTH 80
                HEIGHT 30
                CAPTION "Delete"
                ACTION {|| Book():New(BookForm.txtISBN.Value, "", ""):delete(), BookForm.txtISBN.Value := "" , BookForm.txtTitle.Value := "", BookForm.txtAuthor.Value := ""   }
            END BUTTON

            DEFINE BUTTON btnScan
                ROW 120
                COL 290
                WIDTH  80
                HEIGHT 30
                CAPTION "Scan ISBN"
                ACTION {|| ::scanIsbn(BookForm.txtISBN.Value ) }
            END BUTTON

        END WINDOW
        
        CENTER WINDOW BookForm
        ACTIVATE WINDOW BookForm
    ELSE
        MsgInfo("Access restricted to Librarians and Admins")
    ENDIF

RETURN NIL

/**
 * Simulates barcode scanning
 * param cIsbn String input
 * return Book Object or NIL
 */
METHOD scanIsbn(cIsbn) CLASS BookManager

    ::oBooks:Open()
        IF ::oBooks:Seek( cIsbn )
            BookForm.txtISBN.Value   := ::oBooks:GetValue( "ISBN" )
            BookForm.txtTitle.Value  := ::oBooks:GetValue( "TITLE" )
            BookForm.txtAuthor.Value := ::oBooks:GetValue( "AUTHOR" )
            ::oBooks:Close()
        ELSE
            MsgInfo("Book not found")
            BookForm.txtISBN.Value   := ""
            BookForm.txtTitle.Value  := ""
            BookForm.txtAuthor.Value := ""
            ::oBooks:Close()
        ENDIF

RETURN NIL
