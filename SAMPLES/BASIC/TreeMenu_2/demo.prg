
// by Andrés González López, September 2014

#include "hmg.ch"

FUNCTION Main()

   DEFINE WINDOW Form_1 ;
         WIDTH 800 ;
         HEIGHT 600 ;
         NOSIZE ;
         NOMAXIMIZE ;
         ON GOTFOCUS Form_1.Tree_1.SetFocus ;
         MAIN

      DEFINE TREE Tree_1 ;
            AT 10, 10 ;
            WIDTH 250 ;
            HEIGHT 550 ;
            VALUE 1 ;
            FONT "Calibri" SIZE 12 ;
            ON CHANGE OnChangeTree ( This.Value ) ;
            NODEIMAGES { "folder.bmp", "folder_page.bmp" } ;
            ITEMIMAGES { "Page.bmp", "page_next.bmp" } ;
            NOROOTBUTTON

         NODE 'MY APP       [F9]'

            NODE 'BOOK      [F1]'
               TREEITEM 'Science'
               TREEITEM 'Literature'
            END NODE

            NODE 'MUSIC     [F2]'
               TREEITEM 'Rock'
               TREEITEM 'Classic'
            END NODE

            NODE 'VIDEOS    [F3]'
               TREEITEM 'Documentary'
               TREEITEM 'Series'
               TREEITEM 'Sports'
            END NODE

            NODE 'EMAIL      [F4]'
               TREEITEM 'Send'
               TREEITEM 'Inbox'
               TREEITEM 'Outbox'
               TREEITEM 'Drafts'
               TREEITEM 'Spam'
               TREEITEM 'Sent Items'
               NODE 'OTHER'
                  TREEITEM 'Backup'
                  TREEITEM 'Calendar'
               END NODE
            END NODE

            NODE 'ABOUT...  [F5]'
            END NODE

         END NODE
      END TREE

   END WINDOW

   OnChangeTree ( 1 )
   Form_1.Tree_1.Expand ( 1 )

   ON KEY F1 OF Form_1 ACTION OnKeyFx ( 2 )
   ON KEY F2 OF Form_1 ACTION OnKeyFx ( 5 )
   ON KEY F3 OF Form_1 ACTION OnKeyFx ( 8 )
   ON KEY F4 OF Form_1 ACTION OnKeyFx ( 12 )
   ON KEY F5 OF Form_1 ACTION OnKeyFx ( 22 )

   ON KEY F9 OF Form_1 ACTION ( Form_1.Tree_1.VALUE := 1, OnChangeTree ( 1 ), ;
      IF ( Form_1.Tree_1.IsExpand ( Form_1.Tree_1.Value ) == .T., ;
      Form_1.Tree_1.Collapse ( Form_1.Tree_1.VALUE, .T. ), ; // Collapse All
   Form_1.Tree_1.Expand ( Form_1.Tree_1.VALUE, .T. ) ) ) // Expand All

   CENTER WINDOW Form_1
   ACTIVATE WINDOW Form_1

RETURN NIL


FUNCTION OnKeyFx ( nItem )

   OnChangeTree( nItem )

   Form_1.Tree_1.VALUE := nItem
   IF Form_1.Tree_1.IsExpand( Form_1.Tree_1.Value )
      Form_1.Tree_1.Collapse( Form_1.Tree_1.Value )
   ELSE
      Form_1.Tree_1.Expand( Form_1.Tree_1.Value )
   ENDIF

RETURN NIL


FUNCTION OnChangeTree ( nItem )

   DO CASE
   CASE nItem = 1
   CASE nItem = 2
   CASE nItem = 3
   // CASE nOption 4 to 22  ....
   ENDCASE

   Form_1.TITLE := "#" + hb_ntos( nItem ) + " --> " + hb_ValToExp( TreeItemGetPathName( "Tree_1", "Form_1", nItem ) )

RETURN NIL


FUNCTION TreeItemGetPathValue ( ControlName, ParentForm, nItem )

   LOCAL ParentItem, aPathValues
   LOCAL nControlHandle := GetControlHandle ( ControlName, ParentForm )
   LOCAL ItemHandle := TreeItemGetHandle ( ControlName, ParentForm, nItem )

   IF ItemHandle <> 0
      aPathValues := { nItem }
      ParentItem := TreeView_GetParent ( nControlHandle, ItemHandle )
      WHILE ( ParentItem <> 0 )
         AAdd ( aPathValues, NIL )
         AIns ( aPathValues, 1 )
         aPathValues[ 1 ] := TreeItemGetValueByItemHandle ( ControlName, ParentForm, ParentItem )
         ParentItem := TreeView_GetParent ( nControlHandle, ParentItem )
      END
   ENDIF

RETURN aPathValues


FUNCTION TreeItemGetPathName ( ControlName, ParentForm, nItem )

   LOCAL aPathName
   LOCAL aPathValues := TreeItemGetPathValue ( ControlName, ParentForm, nItem )

   IF ValType ( aPathValues ) == "A"
      aPathName := TreeItemGetItemText ( ControlName, ParentForm, aPathValues )
   ENDIF

RETURN aPathName


FUNCTION TreeItemGetItemText ( ControlName, ParentForm, aItem )

   LOCAL k, cText, aItemsText

   IF ValType ( aItem ) == "A"
      aItemsText := {}
      FOR k = 1 TO HMG_LEN ( aItem )
         cText := GetProperty ( ParentForm, ControlName, "Item", aItem[ k ] )
         AAdd ( aItemsText, cText )
      NEXT
   ENDIF

RETURN aItemsText


FUNCTION TreeItemGetValueByItemHandle ( ControlName, ParentForm, ItemHandle )

   LOCAL nPos, nID
   LOCAL nControlHandle := GetControlHandle ( ControlName, ParentForm )
   LOCAL i

   IF ( i := GetControlIndex ( ControlName, ParentForm ) ) > 0 .AND. ItemHandle <> 0
      IF _HMG_aControlInputMask[ i ] == .F.
         nPos := AScan ( _HMG_aControlPageMap[ i ], ItemHandle )
         RETURN nPos
      ELSE
         nID := TREEITEM_GETID ( nControlHandle, ItemHandle )
         RETURN nID
      ENDIF
   ENDIF

RETURN NIL
