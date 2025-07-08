/*
   Test program for Simple Error Handling sample - 2 ( SErrHnd2.prg )
*/

#include <hmg.ch>

MEMVAR bOldErrHandler
MEMVAR lMaintMode

MEMVAR aMemVars
MEMVAR cShortSt
MEMVAR cLongStr
MEMVAR cVeryLongStr
MEMVAR cHeader
MEMVAR xNILValu
MEMVAR nNumer1
MEMVAR nNumWDec
MEMVAR cDigitStr
MEMVAR dDateOfDay
MEMVAR cTooLongVarName

PROCEDURE Main()

   LOCAL fhnd

   // Save the old (standard) error handle block and then assign the new (local) error handler

   PUBLIC bOldErrHandler := ERRORBLOCK( { | oError | SmpErrHandler02( oError ) } )
   
   PUBLIC lMaintMode := .T.  // Maintenance Mode

   // This variables are for testing
   
   aMemVars   := { 1, 2, 3 }            
   cShortSt   := 'a short string'
   cLongStr   := REPLICATE( 'a long string ', 10 )
   cVeryLongStr  := REPLICATE( 'a very long string (more than 256 byte)', 10 ) // Its name too long than 10 chracter
   cHeader    := "Memory  Variable List "
   xNILValu   := NIL
   nNumer1    := 1
   nNumWDec   := 2.123
   cDigitStr  := "12345"
   dDateOfDay := DATE()
   cTooLongVarName := "cTooLongVarName"
         
   SET CENT ON
   SET DATE GERM
   
   /* Create corrupt index file */
   fhnd := FCreate( "corrupted.cdx" )
   FWrite( fhnd, Chr( 0 ) )
   FClose( fhnd )

   DEFINE WINDOW frmTstSErHnd2 ;
      WIDTH 678 HEIGHT 581 ;
      TITLE "A simple error handling sample - 2 ";  
      ON INIT OpenTables() ;
      MAIN
      
      ON KEY ESCAPE OF frmTstSErHnd2 ACTION frmTstSErHnd2.release
      
      DEFINE MAIN MENU OF frmTstSErHnd2
         POPUP "File"
            POPUP "Activate Error"
               ITEM "&Variable Not Found" ACTION ActivateError( 1 )
               ITEM "&File Not Found"     ACTION ActivateError( 2 )
               ITEM "&Corrupted Index"    ACTION ActivateError( 3 )
               ITEM "&Array Bound"        ACTION ActivateError( 4 )
               ITEM "&Type Mismatch"      ACTION ActivateError( 5 )
               ITEM "&Missing .dbt"       ACTION ActivateError( 6 )
            END POPUP
            SEPARATOR
            ITEM "Open Tables"        ACTION OpenTables()
            ITEM "Close Tables"       ACTION DBCLOSEALL()            
            SEPARATOR
            ITEM "E&xit" ACTION frmTstSErHnd2.release
         END POPUP
         POPUP "?"
            ITEM "About" ACTION MsgBox( "Simple Error Handler - 2", "About" )
         END POPUP
      END MENU
      
   END WINDOW // frmTstSErHnd2

   frmTstSErHnd2.Center
   frmTstSErHnd2.Activate

RETURN // TstErrH2.Main()

*-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.

PROCEDURE ActivateError(;                   // Activate an artificial error 
                         nErrorNo )                  

   LOCAL Numer1
   LOCAL Numer2
   LOCAL Numer3
                         
   DO CASE 
      CASE nErrorNo = 1    // Variable not found error      
         // Referencing an undefined variable for producing RTE 
         MsgBox( xVaribleUnExist ) 
         *
         * This two messages never will be seen.
         *
         * Because "Variable doesn't exists" is a unrecoverable error.
         *
         MsgBox( "That's all !", "End of program" )
      CASE nErrorNo = 2    // Open Error -2 ( File Not Found )
         SELECT TEST
         SET INDEX TO NAME  // NAME.NTX file not found
      CASE nErrorNo = 3     // Open Error -2 ( File Not Found )
         SELECT TEST
         SET INDEX TO CORRUPTED  // CORRUPTED.NTX file not a valid index file
      CASE nErrorNo = 4     // Array Bound Violation
         MsgBox( aMemVars[ 4 ] )  // aMemVars has tree elements
      CASE nErrorNo = 5     // Type mismatch (argument) error
         Numer1 := 5
         Numer2 := "12"
         Numer3 := Numer1 + Numer2 
      CASE nErrorNo = 6     // Attemp to open a .dbf missing .dbt
         USE TEST3
   END CASE 
   
RETURN // ActivateError() 

*-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.
 
PROCEDURE OpenTables() 
 
  USE TEST ALIAS TEST INDEX TEST
  GO 5
  USE TEST2 ALIAS TEST2 INDEX TEST2 NEW
  GO 12
  
RETURN // OpenTables()      
  
*-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.

