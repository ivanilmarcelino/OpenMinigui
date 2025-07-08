/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
 */

#include "minigui.ch"

PROCEDURE MAIN

   DEFINE WINDOW Form_1 ;
         WIDTH 350 ;
         HEIGHT 300 ;
         TITLE 'Registry Test' ;
         MAIN

      DEFINE MAIN MENU

         DEFINE POPUP "Test"
            MENUITEM 'Read Registry' ACTION ReadRegistryTest()
            MENUITEM 'Write Registry' ACTION WriteRegistryTest()
            SEPARATOR
            ITEM 'Exit' ACTION Form_1.Release
         END POPUP

      END MENU

   END WINDOW

   Form_1.Center
   Form_1.Activate

RETURN


PROCEDURE ReadRegistryTest()

   AlertInfo ( GetRegistryValue( HKEY_CURRENT_USER, "Control Panel\Desktop", "Wallpaper" ), ;
      "HKEY_CURRENT_USER\Control Panel\Desktop\Wallpaper" )

RETURN


PROCEDURE WriteRegistryTest()

   LOCAL hKey := HKEY_CURRENT_USER
   LOCAL cKey := "Control Panel\Desktop"
   LOCAL cVar := "Wallpaper"
   LOCAL cValue

   IF AlertYesNo ( 'This will change HKEY_CURRENT_USER\Control Panel\Desktop\Wallpaper.;Are you sure?', "Please, confirm" )

      cValue := InputBox ( '', 'New Value:', GetRegistryValue( hKey, cKey, cVar ) )

      IF .NOT. Empty ( cValue )
         IF .NOT. SetRegistryValue( hKey, cKey, cVar, cValue )
            MsgAlert( 'Write Registry is failure!', 'Error' )
         ELSE
            SetWallPaper( cValue )
         ENDIF
      ENDIF

   ENDIF

RETURN

/*
 * Parameter for SystemParametersInfo()
 */
#define SPI_SETDESKWALLPAPER       20

/*
 * Flags
 */
#define SPIF_UPDATEINIFILE    0x0001
#define SPIF_SENDWININICHANGE 0x0002
#define SPIF_SENDCHANGE       SPIF_SENDWININICHANGE

FUNCTION SetWallPaper( cBitmap )

   IF ! SystemParametersInfo( SPI_SETDESKWALLPAPER, 0, @cBitmap, SPIF_SENDCHANGE )
      MsgAlert( 'Set WallPaper is failure!', 'Error' )
   ENDIF

RETURN NIL
