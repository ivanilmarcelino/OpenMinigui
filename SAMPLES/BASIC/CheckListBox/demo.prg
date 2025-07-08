#include "minigui.ch"

#xcommand ON KEY SPACE [ OF <parent> ] ACTION <action> ;
      = > ;
      _DefineHotKey ( < "parent" >, 0, VK_SPACE, < { action } > )


FUNCTION Main()

   LOCAL aItems := { "Item 1", "Item 2", "Item 3", "Item 4", "Item 5" }

   DEFINE WINDOW Form_1 AT 97, 62 WIDTH 402 HEIGHT 449 ;
         TITLE "Checked ListBox - By Janusz Pora" ;
         MAIN ;
         NOMAXIMIZE NOSIZE

      @ 10, 10 CHECKLISTBOX ListBox_1 ;
         WIDTH 150 HEIGHT 160 ;
         ITEMS aItems ;
         VALUE 2 ;
         FONTCOLOR { 0, 255, 64 } ;
         BACKCOLOR { 255, 0, 128 } ;
         CHECKBOXITEM { 3, 5 } ;
         ON GOTFOCUS cmlb_setclr( this.Name, { 0 , 255 , 64 }, { 255 , 0 , 128 } ) ;
         ON LOSTFOCUS cmlb_setclr( this.Name, { 0 , 255 , 64 }, { 255 , 0 , 128 } ) ;
         ON DBLCLICK clb_Check() ;
         ITEMHEIGHT 19 ;
         FONT 'Arial' SIZE 9

      @ 10, 200 CHECKLISTBOX ListBox_2 ;
         WIDTH 150 HEIGHT 160 ;
         ITEMS aItems ;
         VALUE { 2 } ;
         BACKCOLOR { 0, 255, 64 } ;
         FONTCOLOR { 255, 0, 128 } ;
         CHECKBOXITEM { 4, 5 } ;
         ON GOTFOCUS cmlb_setclr( this.Name, { 255 , 0 , 128 }, { 0 , 255 , 64 } ) ;
         ON LOSTFOCUS cmlb_setclr( this.Name, { 255 , 0 , 128 }, { 0 , 255 , 64 } ) ;
         ON DBLCLICK cmlb_Check() ;
         MULTISELECT ;
         ITEMHEIGHT 19 ;
         FONT 'Arial' SIZE 9

      @ 200, 10 BUTTON bt1 CAPTION 'Add' ACTION clb_add()
      @ 230, 10 BUTTON bt2 CAPTION 'Del' ACTION clb_del()
      @ 260, 10 BUTTON bt3 CAPTION 'Del All' ACTION clb_delete_all()
      @ 290, 10 BUTTON bt4 CAPTION 'Modify' ACTION clb_modify()
      @ 320, 10 BUTTON bt5 CAPTION 'Check' ACTION clb_Check()
      @ 350, 10 BUTTON bt6 CAPTION 'Check #4' ACTION clb_Check( 4 )

      @ 200, 200 BUTTON btm1 CAPTION 'Add' ACTION cmlb_add()
      @ 230, 200 BUTTON btm2 CAPTION 'Del' ACTION cmlb_del()
      @ 260, 200 BUTTON btm3 CAPTION 'Del All' ACTION cmlb_delete_all()
      @ 290, 200 BUTTON btm4 CAPTION 'Modify' ACTION cmlb_modify()
      @ 320, 200 BUTTON btm5 CAPTION 'Check' ACTION cmlb_Check()
      @ 350, 200 BUTTON btm6 CAPTION 'Check #4' ACTION cmlb_Check( 4 )

      ON KEY space ACTION OnPressSpacebar()

   END WINDOW

   Form_1.CENTER ; Form_1.ACTIVATE

RETURN NIL

*.....................................................*

PROC clb_add

   LOCAL nn := form_1.ListBox_1.ItemCount + 1
   form_1.ListBox_1.Setfocus
   form_1.ListBox_1.AddItem( 'ITEM_' + AllTrim( Str( nn ) ) )
   form_1.ListBox_1.VALUE := nn

RETURN

*.....................................................*

PROC clb_del

   LOCAL n1
   LOCAL nn := form_1.ListBox_1.VALUE
   form_1.ListBox_1.Setfocus
   form_1.ListBox_1.DeleteItem( nn )
   n1 := form_1.ListBox_1.ItemCount
   IF nn <= n1
      form_1.ListBox_1.VALUE := nn
   ELSE
      form_1.ListBox_1.VALUE := n1
   ENDIF

RETURN

*.....................................................*

PROC clb_delete_all

   form_1.ListBox_1.DeleteAllItems
   form_1.ListBox_1.VALUE := 1

RETURN

*.....................................................*

PROC clb_modify

   LOCAL nn := form_1.ListBox_1.VALUE
   form_1.ListBox_1.Setfocus
   IF nn > 0
      form_1.ListBox_1.item( nn ) := 'New ' + AllTrim( Str( nn ) )
   ENDIF

RETURN

*.....................................................*

FUNCTION clb_Check( nn )

   LOCAL lCheck
   DEFAULT nn := form_1.ListBox_1.VALUE
   form_1.ListBox_1.Setfocus
   IF nn > 0
      lCheck := clb_getCheck( nn )
      SetProperty( 'form_1', 'ListBox_1', "CHECKBOXITEM", nn, ! lCheck )
   ENDIF

RETURN NIL

*.....................................................*

FUNCTION clb_getCheck( nn )

   LOCAL lCheck
   lCheck := GetProperty( 'form_1', 'ListBox_1', "CHECKBOXITEM", nn )

RETURN lCheck

*.....................................................*

PROC OnPressSpacebar()

   IF GetProperty( 'form_1', "FOCUSEDCONTROL" ) == "ListBox_1"
      clb_Check()
   ELSE
      cmlb_Check()
   ENDIF

RETURN

*.....................................................*

PROC cmlb_add

   LOCAL nn := form_1.ListBox_2.ItemCount + 1
   form_1.ListBox_2.Setfocus
   form_1.ListBox_2.AddItem( 'ITEM_' + AllTrim( Str( nn ) ) )
   form_1.ListBox_2.VALUE := { nn }

RETURN

*.....................................................*

PROC cmlb_del

   LOCAL n1, i
   LOCAL nn := form_1.ListBox_2.VALUE
   form_1.ListBox_2.Setfocus
   IF Len ( nn ) > 0
      FOR i := Len( nn ) TO 1 STEP -1
         form_1.ListBox_2.DeleteItem( nn[ i ] )
      NEXT
      n1 := form_1.ListBox_2.ItemCount
      IF nn[ 1 ] <= n1
         form_1.ListBox_2.VALUE := { nn[ 1 ] }
      ELSE
         form_1.ListBox_2.VALUE := { n1 }
      ENDIF
   ENDIF

RETURN

*.....................................................*

PROC cmlb_delete_all

   form_1.ListBox_2.Setfocus
   form_1.ListBox_2.DeleteAllItems
   form_1.ListBox_2.VALUE := 1

RETURN

*.....................................................*

PROC cmlb_modify

   LOCAL i, nn := form_1.ListBox_2.VALUE
   form_1.ListBox_2.Setfocus
   FOR i := 1 TO Len( nn )
      form_1.ListBox_2.item( nn[ i ] ) := 'New ' + AllTrim( Str( nn[ i ] ) )
   NEXT

RETURN

*.....................................................*

FUNCTION cmlb_Check( n )

   LOCAL lCheck, i
   LOCAL nn := form_1.ListBox_2.VALUE
   DEFAULT n := 0
   form_1.ListBox_2.Setfocus
   IF n == 0
      FOR i := 1 TO Len( nn )
         lCheck := cmlb_getCheck( nn[ i ] )
         SetProperty( 'form_1', 'ListBox_2', "CHECKBOXITEM", nn[ i ], ! lCheck )
      NEXT
   ELSE
      lCheck := cmlb_getCheck( n )
      SetProperty( 'form_1', 'ListBox_2', "CHECKBOXITEM", n, ! lCheck )
   ENDIF

RETURN NIL

*.....................................................*

FUNCTION cmlb_getCheck( nn )

   LOCAL lCheck
   lCheck := GetProperty( 'form_1', 'ListBox_2', "CHECKBOXITEM", nn )

RETURN lCheck

*.....................................................*

FUNCTION cmlb_setclr( nn, clr1, clr2 )

   SetProperty( 'form_1', nn, "FONTCOLOR", clr1 )
   SetProperty( 'form_1', nn, "BACKCOLOR", clr2 )

RETURN NIL
