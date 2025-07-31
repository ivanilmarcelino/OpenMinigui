/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2002 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
 *
 * Copyright 2005-2015 Grigory Filatov <gfilatov@inbox.ru>
 */

ANNOUNCE RDDSYS

#include "minigui.ch"
#include "fileio.ch"

/*
 * PROCEDURE Main
 *
 * Creates and displays a main window containing a pie chart that visualizes disk space usage.
 *
 * Purpose:
 *   This is the main entry point of the application. It performs the following steps:
 *     1. Determines the current drive.
 *     2. Defines the main window with a title showing the drive letter and an icon.
 *     3. Sets the window to be non-maximizable and non-resizable.
 *     4. Attaches the ShowPie() procedure to the ON INIT event of the window, so the pie chart is drawn when the window is initialized.
 *     5. Attaches an action to the ESCAPE key to close the window.
 *     6. Centers the window on the screen.
 *     7. Activates the window, making it visible.
 */
PROCEDURE Main

   LOCAL cDisk := CurDrive() + ":\"

   SET DECIMALS TO
   SET FONT TO "Arial", 10

   DEFINE WINDOW Win_1 ;
      AT 0, 0 WIDTH 220 HEIGHT 240 ;
      TITLE cDisk ;
      ICON "HARD" ;
      MAIN NOMAXIMIZE NOSIZE ;
      ON INIT ShowPie( cDisk )

      ON KEY ESCAPE ACTION ThisWindow.Release()

   END WINDOW

   Win_1.Center()
   Win_1.Activate()

RETURN


/*
 * PROCEDURE ShowPie( cDisk )
 *
 * Creates and displays a pie chart within the specified window, visualizing the used and free space of a given disk drive.
 *
 * Input parameters:
 *   cDisk: A character string representing the disk drive (e.g., "C:\").
 *
 * Return Value: None
 *
 * Purpose:
 *   This procedure calculates the used and free space of a disk drive and displays it as a pie chart.
 *   It performs the following steps:
 *     1. Calculates the free space in megabytes using hb_DiskSpace() with the HB_DISK_FREE flag.
 *     2. Calculates the total space in megabytes using hb_DiskSpace() with the HB_DISK_TOTAL flag.
 *     3. Calculates the used space by subtracting the free space from the total space.
 *     4. Determines if the used space is greater than the free space.
 *     5. Draws a pie chart using the DRAW GRAPH command, showing the used and free space.
 *     6. Adds a title, series names, colors, and 3D view to the chart.
 *     7. Displays a legend for the chart.
 *     8. Draws text indicating the percentage of free space.
 */
PROCEDURE ShowPie( cDisk )

   LOCAL iFree := hb_DiskSpace( cDisk, HB_DISK_FREE ) / ( 1024 * 1024 )
   LOCAL iTotal := hb_DiskSpace( cDisk, HB_DISK_TOTAL ) / ( 1024 * 1024 )
   LOCAL iUsed := iTotal - iFree
   LOCAL lFlag := ( iUsed > iFree )

   DRAW GRAPH IN WINDOW Win_1 ;
      AT 10, 10 ;
      TO 200, 200 ;
      TITLE "" ;
      TYPE PIE ;
      SERIES { iUsed, iFree } ;
      DEPTH 10 ;
      SERIENAMES { "Espacio utilizado", "Espacio libre" } ;
      COLORS { BLUE, FUCHSIA } ;
      3DVIEW ;
      SHOWLEGENDS

   DRAW TEXT IN WINDOW Win_1 ;
      AT iif( lFlag, 60, 90 ), 84 ;
      VALUE "Libre " + hb_ntos( iFree / iTotal * 100 ) + "%" ;
      FONT "MS Sans Serif" SIZE 9 ;
      FONTCOLOR iif( lFlag, WHITE, BLACK ) ;
      TRANSPARENT

RETURN
