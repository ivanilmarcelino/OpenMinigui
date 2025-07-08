/*
 * MiniGUI HeaderImage Property Test
 * (c) 2008 Roberto Lopez
 *
 * March 24, 2020 Revisited by Pierpaolo Martinello
*/

#include "minigui.ch"

#define _GRID_SETBKIMAGE_NONE_   0
#define _GRID_SETBKIMAGE_NORMAL_ 1
#define _GRID_SETBKIMAGE_FILL_   2

********************************************************************************
Function Main
********************************************************************************

    Set( _SET_DEBUG, .T. )

    DEFINE WINDOW Form_1 ;
        WIDTH 550 ;
        HEIGHT 400 ;
        TITLE 'Hello World!' ;
        Icon 'AASIC' ;
        MAIN ;
        ON INIT OnInit()

        DEFINE MAIN MENU
            POPUP 'Image / Skin'
               MENUITEM "MiniGuiLogo"  ACTION GRID_SetBkImage ("Grid_1", "Form_1", _GRID_SETBKIMAGE_NORMAL_, "MiniguiLogo.bmp", 99,  0)
               MENUITEM "Skin Cream"   ACTION GRID_SetBkImage ("Grid_1", "Form_1", _GRID_SETBKIMAGE_FILL_  , "Cream.bmp",       30, 50)
               MENUITEM "Skin Aqua "   ACTION GRID_SetBkImage ("Grid_1", "Form_1", _GRID_SETBKIMAGE_FILL_  , "Aqua.bmp",         0,  0)
               MENUITEM "No Image "    ACTION GRID_SetBkImage ("Grid_1", "Form_1", _GRID_SETBKIMAGE_NONE_  , "",                 0,  0)
            END POPUP

        END MENU

        DEFINE GRID Grid_1
            ROW        10
            COL        10
            WIDTH     500
            HEIGHT    330
            HEADERS   {'Last Name','First Name','Phone'}
            WIDTHS    {140,140,110}
            ITEMS     {}
        END GRID

    END WINDOW

    Form_1.Center()
    Form_1.Activate()

Return Nil
/*
*/
********************************************************************************
Procedure OnInit
********************************************************************************

    Form_1.Grid_1.SetArray( LoadItems() )
    Form_1.Grid_1.Value := 1

    GRID_SetBkImage ("Grid_1", NIL, _GRID_SETBKIMAGE_NORMAL_, "MiniguiLogo.bmp", 99, 0)

Return
/*
*/
********************************************************************************
Function LoadItems()
********************************************************************************
Local aRows [20] [3]

    aRows [1]  := {'Simpson','Homer','555-5555'}
    aRows [2]  := {'Mulder','Fox','324-6432'}
    aRows [3]  := {'Smart','Max','432-5892'}
    aRows [4]  := {'Grillo','Pepe','894-2332'}
    aRows [5]  := {'Kirk','James','346-9873'}
    aRows [6]  := {'Barriga','Carlos','394-9654'}
    aRows [7]  := {'Flanders','Ned','435-3211'}
    aRows [8]  := {'Smith','John','123-1234'}
    aRows [9]  := {'Pedemonti','Flavio','000-0000'}
    aRows [10] := {'Gomez','Juan','583-4832'}
    aRows [11] := {'Fernandez','Raul','321-4332'}
    aRows [12] := {'Borges','Javier','326-9430'}
    aRows [13] := {'Alvarez','Alberto','543-7898'}
    aRows [14] := {'Gonzalez','Ambo','437-8473'}
    aRows [15] := {'Batistuta','Gol','485-2843'}
    aRows [16] := {'Vinazzi','Amigo','394-5983'}
    aRows [17] := {'Pedemonti','Flavio','534-7984'}
    aRows [18] := {'Samarbide','Armando','854-7873'}
    aRows [19] := {'Pradon','Alejandra','???-????'}
    aRows [20] := {'Reyes','Monica','432-5836'}

Return aRows
/*
*/
********************************************************************************
PROCEDURE GRID_SetBkImage (cControlName, cParentForm, nAction, cBMPFileName, yOffset, xOffset)
********************************************************************************
   LOCAL hBmp := LoadBitmap (cBMPFileName)

   IF Valtype (cParentForm) == "U"
      cParentForm := ThisWindow.Name
   ENDIF

   // LISTVIEW_SETBKIMAGE is Low-level function in C (see the end of this file)
   LISTVIEW_SETBKIMAGE (GetControlHandle (cControlName, cParentForm), hBmp, xOffset, yOffset, nAction)
   // clear the bitmap resource
   DeleteObject (hBmp)

RETURN
/*
*/
********************************************************************************
* Low-level Function in C
********************************************************************************
#pragma begindump

#define _WIN32_IE      0x0500
#define _WIN32_WINNT   0x501

#include <windows.h>
#include <commctrl.h>
#include "hbapi.h"

//******************************************************************************
// LISTVIEW_SETBKIMAGE (ControlHandle, hBitmap, yOffset, xOffset, nFlag)
HB_FUNC ( LISTVIEW_SETBKIMAGE )
//******************************************************************************
{
   LVBKIMAGE plBackImage;
   ULONG flag = hb_parnl(5);

   plBackImage.ulFlags = LVBKIF_SOURCE_NONE;
   if (flag == 1)
       plBackImage.ulFlags = LVBKIF_SOURCE_HBITMAP | LVBKIF_STYLE_NORMAL;

   if (flag == 2)
       plBackImage.ulFlags = LVBKIF_SOURCE_HBITMAP | LVBKIF_STYLE_TILE;

   plBackImage.hbm = (HBITMAP) hb_parnl(2);
   plBackImage.pszImage = NULL;
   plBackImage.cchImageMax = 0;
   plBackImage.yOffsetPercent = hb_parni(3);
   plBackImage.xOffsetPercent = hb_parni(4);

   ListView_SetBkImage ((HWND) hb_parnl(1), &plBackImage);
}

#pragma enddump
