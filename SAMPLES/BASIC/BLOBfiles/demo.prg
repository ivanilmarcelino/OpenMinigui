/*
 * MINIGUI - Harbour Win32 GUI library Demo
 */

#define _HMG_OUTLOG

#include "minigui.ch"
#include "blob.ch"

FUNCTION Main

   LOCAL aStruct := { {"CODE", "N", 3, 0}, {"IMAGE", "M", 10, 0} }
   LOCAL cInput  := GetStartupFolder() + "\Input.ico"
   LOCAL cOutput := GetTempFolder() + "\Output.ico"

   REQUEST DBFCDX
   RDDSETDEFAULT( "DBFCDX")

   DBCREATE( "IMAGES", aStruct )

   USE IMAGES NEW
   APPEND BLANK
   REPLACE code with 1

   // Import
   IF ! BLOBIMPORT( FIELDPOS( "IMAGE" ), cInput )
      ? "Error importing !!!"
      RETURN NIL
   ENDIF

   // Export
   FERASE( cOutput )
   IF ! BLOBEXPORT( FIELDPOS( "IMAGE" ), cOutput, BLOB_EXPORT_OVERWRITE )
      ? "Error exporting !!!"
   ENDIF

   // Show
   DEFINE WINDOW Form_1 ;
      WIDTH 588 ;
      HEIGHT 480 ;
      TITLE 'Show image from BLOB file' ;
      MAIN ;
      ON RELEASE Clean( cOutput )

      @ 10, 10 IMAGE Img_1 ;
         PICTURE "demo.ico"  ;
         WIDTH 32 ;
         HEIGHT 32 ;
         STRETCH

      @ 80, 10 BUTTON Btn_1 ;
         CAPTION "Change image" ;
         ACTION Form_1.Img_1.Picture := cOutput
 
      ON KEY ESCAPE ACTION ThisWindow.Release()
   END WINDOW

   Form_1.Center()
   Form_1.Activate()

RETURN NIL


PROCEDURE Clean( cOutput )

   FErase( cOutput )

   CLOSE DATABASES
   FErase( "IMAGES.DBF" )
   FErase( "IMAGES.FPT" )

RETURN
