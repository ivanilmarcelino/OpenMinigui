/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 */

#include "minigui.ch"

SET PROCEDURE TO REGCLASS

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
            ITEM 'Exit' ACTION Form_1.Release()
         END POPUP

      END MENU


   END WINDOW

   Form_1.Center()
   Form_1.Activate()

RETURN


PROCEDURE ReadRegistryTest()

   LOCAL cKey := "HKEY_CURRENT_USER\Control Panel\Desktop"
   LOCAL cVar := "Wallpaper"

   LOCAL oReg := XbpReg():NEW( cKey )
   LOCAL cWallPaper := oReg:GetValue( cVar )

   AlertInfo ( cWallPaper, cKey + "\" + cVar )

RETURN

PROCEDURE WriteRegistryTest()

   LOCAL cKey := "HKEY_CURRENT_USER\Control Panel\Desktop"
   LOCAL cVar := "Wallpaper"

   LOCAL oReg := XbpReg():NEW( cKey )
   LOCAL cWallPaper := oReg:GetValue( cVar )
   LOCAL cValue

   IF AlertYesNo ( 'This will change HKEY_CURRENT_USER\Control Panel\Desktop\Wallpaper.;Are you sure?', "Please, confirm" )

      cValue := InputBox ( '', 'New Value:', cWallPaper )

      IF .NOT. Empty ( cValue )
         IF .NOT. ( oReg:SetValue( cVar, cValue ) == 0 )
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
