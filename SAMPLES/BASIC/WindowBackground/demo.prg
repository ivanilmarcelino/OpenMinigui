/*
 * Author: P.Chornyj <myorg63@mail.ru>
 *
 * Make Your Project Look Great!
*/

#include "minigui.ch"

#define COLOR_WINDOW	5

PROCEDURE Main()

   DEFINE WINDOW Form_Main ;
      CLIENTAREA 640, 480 ;
      TITLE 'How to set a Window background' ;
      BKBRUSH iif(IsThemed(), nRGB2Arr( GetSysColor( COLOR_WINDOW ) ), ) ;
      MAIN

      DEFINE MAIN MENU

         DEFINE POPUP "&Create BKBrush"

            POPUP "&SOLID"
               ITEM '&RED' ACTION SetBKBrush( 1 )
               ITEM '&GREEN' ACTION SetBKBrush( 2 )
               ITEM '&BLUE' ACTION SetBKBrush( 3 )
               ITEM 'LightGoldenrod&3' ACTION SetBKBrush( 13 )
            END POPUP

            POPUP "&HATCHED"
               ITEM '&VERTICAL' ACTION SetBKBrush( 4 )
               ITEM '&HORIZONTAL' ACTION SetBKBrush( 5 )
               ITEM '&FDIAGONAL' ACTION SetBKBrush( 6 )
               ITEM '&BDIAGONAL' ACTION SetBKBrush( 7 )
               ITEM '&CROSS' ACTION SetBKBrush( 8 )
               ITEM '&DIAGCROS' ACTION SetBKBrush( 9 )
            END POPUP

            POPUP "&PATTERN"
               ITEM '&HEARTS' ACTION SetBKBrush( 10 )
               ITEM '&WALL' ACTION SetBKBrush( 11 )
               ITEM 'S&TEEL' ACTION SetBKBrush( 12 )
               ITEM '&SMILES' ACTION SetBKBrush( 14 )
            END POPUP

            SEPARATOR

            MENUITEM "&Exit" ACTION Form_Main.Release

         END POPUP

      END MENU

      ThisWindow.Height := ( ThisWindow.Height ) + GetMenuBarHeight()

   END WINDOW

   CENTER WINDOW Form_Main

   ACTIVATE WINDOW Form_Main

RETURN


STATIC PROCEDURE SetBKBrush( nBrushStyle )

   LOCAL hBrush
   LOCAL aPict := { 'hearts.png', "WALL", "WALL2", "smiles.gif" }
   LOCAL cFormName := 'Form_Main'

   Form_Main.DisableUpdate

   SWITCH nBrushStyle
   CASE  1 ; ADD BKBRUSH hBrush SOLID COLOR RED   TO FORM (cFormName) 
      EXIT

   CASE  2 ; ADD BKBRUSH hBrush SOLID COLOR GREEN TO (cFormName)
      EXIT

   CASE  3 ; ADD BKBRUSH hBrush SOLID COLOR BLUE TO (cFormName)
      EXIT

   CASE 13 ; ADD BKBRUSH hBrush SOLID COLOR { 205, 190, 112 } TO (cFormName)
      EXIT

   CASE  4 ; ADD BKBRUSH hBrush HATCHED HATCHSTYLE HS_VERTICAL COLOR { 0, 200, 0 } TO (cFormName)
      EXIT

   CASE  5 ; ADD BKBRUSH hBrush HATCHED HS_HORIZONTAL TO (cFormName)
      EXIT

   CASE  6 ; ADD BKBRUSH hBrush HATCHED HS_FDIAGONAL TO (cFormName)
      EXIT

   CASE  7 ; ADD BKBRUSH hBrush HATCHED HS_BDIAGONAL TO (cFormName)
      EXIT

   CASE  8 ; ADD BKBRUSH hBrush HATCHED HS_CROSS TO (cFormName)
      EXIT

   CASE  9 ; ADD BKBRUSH hBrush HATCHED HS_DIAGCROSS TO (cFormName)
      EXIT

   CASE 10
   CASE 11
   CASE 12
   CASE 14
             DEFINE BKGBRUSH hBrush PATTERN PICTURE (aPict[ nBrushStyle - iif(nBrushStyle < 13, 9, 10) ]) IN (cFormName)
      EXIT

   END SWITCH

   Form_Main.EnableUpdate

RETURN
