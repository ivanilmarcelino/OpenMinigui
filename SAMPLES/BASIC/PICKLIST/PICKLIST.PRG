*+--------------------------------------------------------------------
*+
*+ Source Module => PICKLIST.PRG
*+
*+    Copyright(C) 1983-2020 by Auge & Ohr
*+
*+    Functions: Procedure PickList()
*+               Static Procedure FillLeftBox()
*+               Static Procedure SelectLeftAll()
*+               Static Procedure DelItemLeft()
*+               Static Procedure SelectLeftThis()
*+               Static Procedure SelectRightAll()
*+               Static Procedure DelItemRight()
*+               Static Procedure SelectRightThis()
*+               Static Procedure GetRightResult()
*+
*+    Reformatted by Click! 2.05.30 on May-26-2020 at 11:27 pm
*+
*+--------------------------------------------------------------------

#include "hmg.ch"
#include "Dbstruct.ch"

STATIC aResult := {}

PROCEDURE MAIN(cDBF)

   LOCAL aStructure

   DEFAULT cDBF := "test.dbf"

   IF !FILE(cDBF)
      MsgInfo("need DBF")
   ELSE
      USE (cDBF)
      aStructure := DbStruct()
      PickList(aStructure)
   ENDIF

RETURN


FUNCTION PickList( aIn )

   DEFINE WINDOW PickList ;
         AT 0, 0   ;
         WIDTH 550 ;
         HEIGHT 350 ;
         TITLE "Picklist" ;
         MAIN ;
         NOMAXIMIZE ;
         NOMINIMIZE ;
         NOSIZE ;
         ON INIT FillLeftBox( aIn )

      DEFINE LISTBOX List_Left
         ROW 20
         COL 10
         WIDTH 200
         HEIGHT 260
         ITEMS {}
         VALUE 0
         FONTNAME "Arial"
         FONTSIZE 9
         TABSTOP .T.
         VISIBLE .T.
         MULTISELECT .T.
         DRAGITEMS .F.
      END LISTBOX

      DEFINE LISTBOX List_Right
         ROW 20
         COL 320
         WIDTH 200
         HEIGHT 260
         ITEMS {}
         VALUE 0
         FONTNAME "Arial"
         FONTSIZE 9
         TABSTOP .T.
         VISIBLE .T.
         MULTISELECT .T.
         DRAGITEMS .T.
      END LISTBOX

      DEFINE BUTTON Button_LeftThis
         ROW 20
         COL 230
         WIDTH 60
         HEIGHT 28
         ACTION SelectLeftThis()
         CAPTION ">"
         FONTNAME "Arial"
         FONTSIZE 9
         TABSTOP .T.
         VISIBLE .T.
      END BUTTON

      DEFINE BUTTON Button_LeftAll
         ROW 60
         COL 230
         WIDTH 60
         HEIGHT 28
         ACTION SelectLeftAll( aIn )
         CAPTION ">>"
         FONTNAME "Arial"
         FONTSIZE 9
         TABSTOP .T.
         VISIBLE .T.
      END BUTTON

      DEFINE BUTTON Button_RightAll
         ROW 100
         COL 230
         WIDTH 60
         HEIGHT 28
         ACTION SelectRightAll( aIn )
         CAPTION "<<"
         FONTNAME "Arial"
         FONTSIZE 9
         TABSTOP .T.
         VISIBLE .T.
      END BUTTON

      DEFINE BUTTON Button_RightThis
         ROW 140
         COL 230
         WIDTH 60
         HEIGHT 28
         ACTION SelectRightThis()
         CAPTION "<"
         FONTNAME "Arial"
         FONTSIZE 9
         TABSTOP .T.
         VISIBLE .T.
      END BUTTON

      DEFINE BUTTON Button_Cancel
         ROW 180
         COL 230
         WIDTH 60
         HEIGHT 28
         ACTION PickList.Release
         CAPTION "&Cancel"
         FONTNAME "Arial"
         FONTSIZE 9
         TABSTOP .T.
         VISIBLE .T.
      END BUTTON

      DEFINE BUTTON Button_OK
         ROW 220
         COL 230
         WIDTH 64
         HEIGHT 64
         ACTION GetRightResult( aIn )
         CAPTION "&OK"
         FONTNAME "Arial"
         FONTSIZE 9
         TABSTOP .T.
         VISIBLE .T.
      END BUTTON

   END WINDOW

   ON KEY ESCAPE OF PickList ACTION PickList.Release

   CENTER WINDOW PickList
   ACTIVATE WINDOW PickList

RETURN ACLONE(aResult)

*+--------------------------------------------------------------------
*+
*+    Static Procedure FillLeftBox()
*+
*+    Called from ( picklist.prg )   1 - procedure picklist()
*+
*+--------------------------------------------------------------------
*+
STATIC PROCEDURE FillLeftBox( aIn )

LOCAL ii, iMax

   iMax := LEN( aIn )
   FOR ii := 1 TO iMax
      PickList.List_Left.AddItem( aIn[ ii ] [ DBS_NAME ] )
   NEXT

RETURN

*+--------------------------------------------------------------------
*+
*+    Static Procedure SelectLeftAll()
*+
*+    Called from ( picklist.prg )   1 - procedure picklist()
*+
*+--------------------------------------------------------------------
*+
STATIC PROCEDURE SelectLeftAll( aIn )

LOCAL ii, iMax

   PickList.List_Right.DeleteAllItems

   iMax := LEN( aIn )
   FOR ii := 1 TO iMax
      PickList.List_Right.AddItem( aIn[ ii ] [ DBS_NAME ] )
      DelItemLeft( aIn[ ii ] [ DBS_NAME ] )
   NEXT
   DO EVENTS

RETURN

*+--------------------------------------------------------------------
*+
*+    Static Procedure DelItemLeft()
*+
*+    Called from ( picklist.prg )   1 - static procedure selectleftall()
*+
*+--------------------------------------------------------------------
*+
STATIC PROCEDURE DelItemLeft( cName )

LOCAL ii, iMax := PickList.List_Left.ItemCount
LOCAL cItem

   FOR ii := 1 TO iMax
      cItem := PickList.List_Left.Item( ii )
      IF cItem = cName
         PickList.List_Left.DeleteItem( ii )
         EXIT
      ENDIF
   NEXT
   DO EVENTS

RETURN

*+--------------------------------------------------------------------
*+
*+    Static Procedure SelectLeftThis()
*+
*+    Called from ( picklist.prg )   1 - procedure picklist()
*+
*+--------------------------------------------------------------------
*+
STATIC PROCEDURE SelectLeftThis()

LOCAL ii, iMax
LOCAL cItem
LOCAL aSelect := PickList.List_Left.Value

   iMax := LEN( aSelect )
   FOR ii := 1 TO iMax
      cItem := PickList.List_Left.Item( aSelect[ ii ] )
      PickList.List_Right.AddItem(cItem)
   NEXT

   FOR ii := iMax TO 1 STEP - 1
      PickList.List_Left.DeleteItem( aSelect[ ii ] )
   NEXT
   DO EVENTS

RETURN

*+--------------------------------------------------------------------
*+
*+    Static Procedure SelectRightAll()
*+
*+    Called from ( picklist.prg )   1 - procedure picklist()
*+
*+--------------------------------------------------------------------
*+
STATIC PROCEDURE SelectRightAll( aIn )

LOCAL ii, iMax

   PickList.List_Left.DeleteAllItems

   iMax := LEN( aIn )
   FOR ii := 1 TO iMax
      PickList.List_Left.AddItem( aIn[ ii ] [ DBS_NAME ] )
      DelItemRight( aIn[ ii ] [ DBS_NAME ] )
   NEXT
   DO EVENTS

RETURN

*+--------------------------------------------------------------------
*+
*+    Static Procedure DelItemRight()
*+
*+    Called from ( picklist.prg )   1 - static procedure selectrightall()
*+
*+--------------------------------------------------------------------
*+
STATIC PROCEDURE DelItemRight( cName )

LOCAL ii, iMax := PickList.List_Right.ItemCount
LOCAL cItem

   FOR ii := 1 TO iMax
      cItem := PickList.List_Right.Item( ii )
      IF cItem = cName
         PickList.List_Right.DeleteItem( ii )
         EXIT
      ENDIF
   NEXT
   DO EVENTS

RETURN

*+--------------------------------------------------------------------
*+
*+    Static Procedure SelectRightThis()
*+
*+    Called from ( picklist.prg )   1 - procedure picklist()
*+
*+--------------------------------------------------------------------
*+
STATIC PROCEDURE SelectRightThis()

LOCAL ii, iMax
LOCAL cItem
LOCAL aSelect := PickList.List_Right.Value

   iMax := LEN( aSelect )
   FOR ii := 1 TO iMax
      cItem := PickList.List_Right.Item( aSelect[ ii ] )
      PickList.List_Left.AddItem( cItem )
   NEXT

   FOR ii := iMax TO 1 STEP - 1
      PickList.List_Right.DeleteItem( aSelect[ ii ] )
   NEXT
   DO EVENTS

RETURN

*+--------------------------------------------------------------------
*+
*+    Static Procedure GetRightResult()
*+
*+    Called from ( picklist.prg )   1 - procedure picklist()
*+
*+--------------------------------------------------------------------
*+
STATIC PROCEDURE GetRightResult( aIn )

LOCAL ii, iMax, nPosi
LOCAL cItem

   aResult := {}                                 // here Result

   iMax := PickList.List_Right.ItemCount

   FOR ii := 1 TO iMax
      cItem := PickList.List_Right.Item( ii )
      nPosi := ASCAN( aIn, { | e | e[ DBS_NAME ] = cItem } )
      IF nPosi > 0
         AADD( aResult, aIn[ nPosi ] )
      ENDIF
   NEXT

   MsgDebug( aResult )

   PickList.Release

RETURN

*+ EOF: PICKLIST.PRG
