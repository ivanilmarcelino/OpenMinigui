/*
 * MiniGUI Demonstration of icons from the library shell32.dll
 *
 * Copyright 2013-2022 Grigory Filatov <gfilatov@gmail.com>
*/

#include "minigui.ch"

PROCEDURE Main()

   LOCAL cIconDll := System.SystemFolder + "\shell32.dll"
   // LOCAL cIconSave := System.TempFolder + '\temp.ico'
   LOCAL cIconSave := GetStartUpFolder() + '\temp.ico'

   SET MULTIPLE OFF WARNING

   // icon with number 90 to write along the path
   IF ! SaveIcon( cIconSave, cIconDll, 90 )
      MsgInfo( "Icon is NOT saved!", "Error" )
   ENDIF

   SET DEFAULT ICON TO cIconSave

   DEFINE WINDOW Form_1 ;
         AT 50, 50 ;
         WIDTH 660 HEIGHT 450 ;
         TITLE "This is icon #90 from the file - " + cIconDll ;
         MAIN ;
         BACKCOLOR ORANGE ;
         ON RELEASE FErase( cIconSave )

      DRAW SYSICON IN WINDOW Form_1 ;
         AT ( This.ClientHeight - 32 ) / 2, ( This.ClientWidth - 32 ) / 2 ;
         FROM cIconDll ID 90 COLOR CLR_ORANGE

      ON KEY ESCAPE ACTION ThisWindow.Release()

   END WINDOW

   Form_1.Activate()

RETURN


FUNCTION SaveIcon( cIconName, cIconSrc, nItem )

   LOCAL aIcons := {}, lRet, hIcon

   AAdd( aIcons, ExtractIconEx( cIconSrc, nItem )[1] )
   AAdd( aIcons, ExtractIconEx( cIconSrc, nItem, 16, 16 )[1] )

   lRet := C_SaveHIconToFile( cIconName, aIcons )

   FOR EACH hIcon IN aIcons
      DestroyIcon( hIcon )
   NEXT

RETURN lRet
