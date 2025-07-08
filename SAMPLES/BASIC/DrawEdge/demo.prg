#include <hmg.ch>

FUNCTION Main()

   LOAD WINDOW MAIN
   MAIN.Center
   MAIN.Maximize
   MAIN.Activate

RETURN NIL

FUNCTION Screen1()
   LOAD WINDOW Screen_1
   Screen_1.Center
   SetProperty( "Screen_1", "Width", GetProperty( "Screen_1", "Width" ) + If( _HMG_IsXP, 0, 100 ) )
   SetProperty( "Screen_1", "MonthCal_2", "Col", GetProperty( "Screen_1", "MonthCal_2", "Col" ) + If( _HMG_IsXP, 0, 20 ) )
   SetProperty( "Screen_1", "MonthCal_3", "Col", GetProperty( "Screen_1", "MonthCal_3", "Col" ) + If( _HMG_IsXP, 0, 40 ) )
   SetProperty( "Screen_1", "MonthCal_4", "Col", GetProperty( "Screen_1", "MonthCal_4", "Col" ) + If( _HMG_IsXP, 0, 60 ) )
   Screen_1.Activate

RETURN NIL

FUNCTION Set_Screen1()

   LOCAL hDC := GetDC( ThisWindow.Handle )

   DrawEdge( hDC, GetControlCoords( "Screen_1", "MonthCal_1", 2 ), EDGE_BUMP, BF_RECT )
   DrawEdge( hDC, GetControlCoords( "Screen_1", "MonthCal_2", 2 ), EDGE_ETCHED, BF_RECT )
   DrawEdge( hDC, GetControlCoords( "Screen_1", "MonthCal_3", 2 ), EDGE_RAISED, BF_RECT )
   DrawEdge( hDC, GetControlCoords( "Screen_1", "MonthCal_4", 2 ), EDGE_SUNKEN, BF_RECT )

   ReleaseDC( ThisWindow.Handle, hDC )

RETURN NIL

FUNCTION Screen2()

   LOAD WINDOW Screen_2
   Screen_2.Center
   Screen_2.Activate

RETURN NIL

FUNCTION Set_Screen2()

   LOCAL hDC := GetDC( ThisWindow.Handle )

   DrawEdge( hDC, GetControlCoords( "Screen_2", "Image_1", 2 ), EDGE_BUMP, BF_RECT )
   DrawEdge( hDC, GetControlCoords( "Screen_2", "Image_2", 2 ), EDGE_ETCHED, BF_RECT )
   DrawEdge( hDC, GetControlCoords( "Screen_2", "Image_3", 2 ), EDGE_RAISED, BF_RECT )
   DrawEdge( hDC, GetControlCoords( "Screen_2", "Image_4", 2 ), EDGE_SUNKEN, BF_RECT )

   ReleaseDC( ThisWindow.Handle, hDC )

RETURN NIL

FUNCTION Screen3()

   LOAD WINDOW Screen_3
   Screen_3.Center
   Screen_3.Activate

RETURN NIL

FUNCTION Set_Screen3()

   LOCAL hDC := GetDC( ThisWindow.Handle )

   DrawEdge( hDC, GetControlCoords( "Screen_3", "Grid_1", 2 ), EDGE_BUMP, BF_RECT )
   DrawEdge( hDC, GetControlCoords( "Screen_3", "Grid_2", 2 ), EDGE_ETCHED, BF_RECT )
   DrawEdge( hDC, GetControlCoords( "Screen_3", "Grid_3", 2 ), EDGE_RAISED, BF_RECT )
   DrawEdge( hDC, GetControlCoords( "Screen_3", "Grid_4", 2 ), EDGE_SUNKEN, BF_RECT )

   ReleaseDC( ThisWindow.Handle, hDC )

RETURN NIL

FUNCTION Screen4()

   LOAD WINDOW Screen_4
   Screen_4.Center
   Screen_4.Activate

RETURN NIL

FUNCTION Set_Screen4()

   LOCAL hDC := GetDC( ThisWindow.Handle )

   DrawEdge( hDC, GetControlCoords( "Screen_4", "Edit_1", 2 ), EDGE_BUMP, BF_RECT )
   DrawEdge( hDC, GetControlCoords( "Screen_4", "Edit_2", 2 ), EDGE_ETCHED, BF_RECT )
   DrawEdge( hDC, GetControlCoords( "Screen_4", "Edit_3", 2 ), EDGE_RAISED, BF_RECT )
   DrawEdge( hDC, GetControlCoords( "Screen_4", "Edit_4", 2 ), EDGE_SUNKEN, BF_RECT )

   ReleaseDC( ThisWindow.Handle, hDC )

RETURN NIL

FUNCTION Screen5()

   LOAD WINDOW Screen_5
   Screen_5.Center
   Screen_5.Activate

RETURN NIL

FUNCTION Set_Screen5()

   LOCAL hDC := GetDC( ThisWindow.Handle )

   DrawEdge( hDC, GetControlCoords( "Screen_5", "List_1", 2 ), EDGE_BUMP, BF_RECT )
   DrawEdge( hDC, GetControlCoords( "Screen_5", "List_2", 2 ), EDGE_ETCHED, BF_RECT )
   DrawEdge( hDC, GetControlCoords( "Screen_5", "List_3", 2 ), EDGE_RAISED, BF_RECT )
   DrawEdge( hDC, GetControlCoords( "Screen_5", "List_4", 2 ), EDGE_SUNKEN, BF_RECT )

   ReleaseDC( ThisWindow.Handle, hDC )

RETURN NIL

FUNCTION Screen6()

   LOAD WINDOW Screen_6
   Screen_6.Center
   Screen_6.Activate

RETURN NIL

FUNCTION Set_Screen6()

   LOCAL hDC := GetDC( ThisWindow.Handle )
   LOCAL aCheck1, aCheck2, aCheck3, aCheck4
   LOCAL aCombo1, aCombo2, aCombo3, aCombo4

   DrawEdge( hDC, GetControlCoords( "Screen_6", "Button_1", 2 ), EDGE_BUMP, BF_RECT )
   DrawEdge( hDC, GetControlCoords( "Screen_6", "Button_2", 2 ), EDGE_ETCHED, BF_RECT )
   DrawEdge( hDC, GetControlCoords( "Screen_6", "Button_3", 2 ), EDGE_RAISED, BF_RECT )
   DrawEdge( hDC, GetControlCoords( "Screen_6", "Button_4", 2 ), EDGE_SUNKEN, BF_RECT )

   aCheck1 := GetControlCoords( "Screen_6", "Check_1", 2 )
   aCheck1[ 1 ] := aCheck1[ 1 ] + 7
   aCheck1[ 3 ] := aCheck1[ 3 ] - 8
   aCheck1[ 4 ] := aCheck1[ 4 ] - 87
   aCheck2 := GetControlCoords( "Screen_6", "Check_2", 2 )
   aCheck2[ 1 ] := aCheck2[ 1 ] + 7
   aCheck2[ 3 ] := aCheck2[ 3 ] - 8
   aCheck2[ 4 ] := aCheck2[ 4 ] - 87
   aCheck3 := GetControlCoords( "Screen_6", "Check_3", 2 )
   aCheck3[ 1 ] := aCheck3[ 1 ] + 7
   aCheck3[ 3 ] := aCheck3[ 3 ] - 8
   aCheck3[ 4 ] := aCheck3[ 4 ] - 87
   aCheck4 := GetControlCoords( "Screen_6", "Check_4", 2 )
   aCheck4[ 1 ] := aCheck4[ 1 ] + 7
   aCheck4[ 3 ] := aCheck4[ 3 ] - 8
   aCheck4[ 4 ] := aCheck4[ 4 ] - 87
   DrawEdge( hDC, aCheck1, EDGE_BUMP, BF_RECT )
   DrawEdge( hDC, aCheck2, EDGE_ETCHED, BF_RECT )
   DrawEdge( hDC, aCheck3, EDGE_RAISED, BF_RECT )
   DrawEdge( hDC, aCheck4, EDGE_SUNKEN, BF_RECT )

   DrawEdge( hDC, GetControlCoords( "Screen_6", "Spinner_1", 2 ), EDGE_BUMP, BF_RECT )
   DrawEdge( hDC, GetControlCoords( "Screen_6", "Spinner_2", 2 ), EDGE_ETCHED, BF_RECT )
   DrawEdge( hDC, GetControlCoords( "Screen_6", "Spinner_3", 2 ), EDGE_RAISED, BF_RECT )
   DrawEdge( hDC, GetControlCoords( "Screen_6", "Spinner_4", 2 ), EDGE_SUNKEN, BF_RECT )

   DrawEdge( hDC, GetControlCoords( "Screen_6", "Text_1", 2 ), EDGE_BUMP, BF_RECT )
   DrawEdge( hDC, GetControlCoords( "Screen_6", "Text_2", 2 ), EDGE_ETCHED, BF_RECT )
   DrawEdge( hDC, GetControlCoords( "Screen_6", "Text_3", 2 ), EDGE_RAISED, BF_RECT )
   DrawEdge( hDC, GetControlCoords( "Screen_6", "Text_4", 2 ), EDGE_SUNKEN, BF_RECT )

   DrawEdge( hDC, GetControlCoords( "Screen_6", "Label_1", 2 ), EDGE_BUMP, BF_RECT )
   DrawEdge( hDC, GetControlCoords( "Screen_6", "Label_2", 2 ), EDGE_ETCHED, BF_RECT )
   DrawEdge( hDC, GetControlCoords( "Screen_6", "Label_3", 2 ), EDGE_RAISED, BF_RECT )
   DrawEdge( hDC, GetControlCoords( "Screen_6", "Label_4", 2 ), EDGE_SUNKEN, BF_RECT )

   aCombo1 := GetControlCoords( "Screen_6", "Combo_1", 2 )
   aCombo1[ 3 ] := aCombo1[ 3 ] - 77
   aCombo2 := GetControlCoords( "Screen_6", "Combo_2", 2 )
   aCombo2[ 3 ] := aCombo2[ 3 ] - 77
   aCombo3 := GetControlCoords( "Screen_6", "Combo_3", 2 )
   aCombo3[ 3 ] := aCombo3[ 3 ] - 77
   aCombo4 := GetControlCoords( "Screen_6", "Combo_4", 2 )
   aCombo4[ 3 ] := aCombo4[ 3 ] - 77
   DrawEdge( hDC, aCombo1, EDGE_BUMP, BF_RECT )
   DrawEdge( hDC, aCombo2, EDGE_ETCHED, BF_RECT )
   DrawEdge( hDC, aCombo3, EDGE_RAISED, BF_RECT )
   DrawEdge( hDC, aCombo4, EDGE_SUNKEN, BF_RECT )

   ReleaseDC( ThisWindow.Handle, hDC )

RETURN NIL

FUNCTION Screen7()

   LOAD WINDOW Screen_7
   Screen_7.Center
   Screen_7.Activate

RETURN NIL

FUNCTION Set_Screen7()

   LOCAL hDC := GetDC( ThisWindow.Handle )
   LOCAL aFrame1, aFrame2, aFrame3, aFrame4

   aFrame1 := GetControlCoords( "Screen_7", "Frame_1", 2 )
   aFrame1[ 1 ] := aFrame1[ 1 ] + 2
   aFrame2 := GetControlCoords( "Screen_7", "Frame_2", 2 )
   aFrame2[ 1 ] := aFrame2[ 1 ] + 2
   aFrame3 := GetControlCoords( "Screen_7", "Frame_3", 2 )
   aFrame3[ 1 ] := aFrame3[ 1 ] + 2
   aFrame4 := GetControlCoords( "Screen_7", "Frame_4", 2 )
   aFrame4[ 1 ] := aFrame4[ 1 ] + 2

   DrawEdge( hDC, aFrame1, EDGE_BUMP, BF_RECT )
   DrawEdge( hDC, aFrame2, EDGE_ETCHED, BF_RECT )
   DrawEdge( hDC, aFrame3, EDGE_RAISED, BF_RECT )
   DrawEdge( hDC, aFrame4, EDGE_SUNKEN, BF_RECT )

   ReleaseDC( ThisWindow.Handle, hDC )

RETURN NIL

FUNCTION GetControlCoords( cForm, cComp, nAdd )

   LOCAL aRect := { 0, 0, 0, 0 }

   hb_default( @nAdd, 0 )

   aRect[ 1 ] := GetProperty( cForm, cComp, "ROW" ) - nAdd
   aRect[ 2 ] := GetProperty( cForm, cComp, "COL" ) - nAdd
   aRect[ 3 ] := GetProperty( cForm, cComp, "HEIGHT" ) + aRect[ 1 ] + ( nAdd * 2 )
   aRect[ 4 ] := GetProperty( cForm, cComp, "WIDTH" ) + aRect[ 2 ] + ( nAdd * 2 )

RETURN aRect

FUNCTION About()
RETURN MsgInfo( "DrawEdge Demo version 1.1 - Freeware" + CRLF + CRLF + ;
      "Author: Pablo Cesar Arrascaeta" + CRLF + CRLF + ;
      hb_Compiler() + CRLF + ;
      Version() + CRLF + ;
      SubStr( MiniGuiVersion(), 1, 38 ), "About" )

FUNCTION DrawEdge( hDC, aRect, nEff, nBorder )
RETURN BT_DrawEdge( hDC, aRect[ 1 ], aRect[ 2 ], aRect[ 4 ], aRect[ 3 ], nEff, nBorder )
