/*
 * HMG - Harbour Win32 GUI library Demo
 *
 * Copyright 2014-2021 Grigory Filatov <gfilatov@inbox.ru>
*/

#include "minigui.ch"

Function Main

        DEFINE WINDOW Win_1 ;
               AT 0,0 ;
               WIDTH 400 HEIGHT 380 ;
               TITLE 'ReadOnly Rating Test' ;
               ICON 'star.ico' ;
               MAIN ;
               FONT "Arial" SIZE 14 ;
               BACKCOLOR WHITE
 
               DEFINE MAINMENU 
                        DEFINE POPUP "File"
				MENUITEM "Exit" ONCLICK ThisWindow.Release
                        END POPUP
                END MENU

		@ 20, 20 LABEL LABEL_0 VALUE '5 Star Rating Scale' WIDTH 360 FONT "Arial" SIZE 16 CENTERALIGN BOLD TRANSPARENT

		@ 70, 40 LABEL LABEL_1 VALUE 'Loved It' BOLD TRANSPARENT

		@ 70, 180 RATING Rate_1 ;
			WIDTH 21 ;
			HEIGHT 21 ;
			STARS 5 ;
			FROM RESOURCE ;
			SPACING 15 ;
			TOOLTIP '5 stars' ;
			RATE 5 READONLY

		@ 120, 40 LABEL LABEL_2 VALUE 'Liked It' BOLD TRANSPARENT

		@ 120, 180 RATING Rate_2 ;
			WIDTH 21 ;
			HEIGHT 21 ;
			STARS 5 ;
			FROM RESOURCE ;
			SPACING 15 ;
			TOOLTIP '4 stars' ;
			RATE 4 READONLY

		@ 170, 40 LABEL LABEL_3 VALUE 'It was ok' BOLD TRANSPARENT

		@ 170, 180 RATING Rate_3 ;
			WIDTH 21 ;
			HEIGHT 21 ;
			STARS 5 ;
			FROM RESOURCE ;
			SPACING 15 ;
			TOOLTIP '3 stars' ;
			RATE 3 READONLY

		@ 220, 40 LABEL LABEL_4 VALUE 'Disliked It' BOLD TRANSPARENT

		@ 220, 180 RATING Rate_4 ;
			WIDTH 21 ;
			HEIGHT 21 ;
			STARS 5 ;
			FROM RESOURCE ;
			SPACING 15 ;
			TOOLTIP '2 stars' ;
			RATE 2 READONLY

		@ 270, 40 LABEL LABEL_5 VALUE 'Hated It' BOLD TRANSPARENT

		@ 270, 180 RATING Rate_5 ;
			WIDTH 21 ;
			HEIGHT 21 ;
			STARS 5 ;
			FROM RESOURCE ;
			SPACING 15 ;
			TOOLTIP '1 star' ;
			RATE 1 READONLY

        END WINDOW

        Win_1.Center
        ACTIVATE WINDOW Win_1

Return Nil

