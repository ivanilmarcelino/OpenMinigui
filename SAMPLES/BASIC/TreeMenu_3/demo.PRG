/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Demo was contributed to HMG forum by Edward 11/Apr/2021
 *
 * Adapted for MiniGUI Extended Edition by Grigory Filatov
 */

#include "hmg.ch"

FUNCTION Main()

   DEFINE WINDOW Form_1 ;
         AT 0, 0 ;
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
            FONT "Calibri" SIZE 11 ;
            ON CHANGE OnChangeTree ( This.Value ) ;
            NODEIMAGES { "folder.gif", "folder_chk.gif" } ;
            ITEMIMAGES { "page.gif", "page_chk.gif" } ;
            NOROOTBUTTON

         DEFINE NODE 'MY APP '

            DEFINE NODE 'BOOK'
               TREEITEM 'Science'
               TREEITEM 'Literature'
            END NODE

            DEFINE NODE 'MUSIC'
               TREEITEM 'Rock'
               TREEITEM 'Classic'
            END NODE

            DEFINE NODE 'VIDEOS'
               TREEITEM 'Documentary'
               TREEITEM 'Series'
               TREEITEM 'Sports'
            END NODE

            DEFINE NODE 'EMAIL'
               TREEITEM 'Send'
               TREEITEM 'Inbox'
               TREEITEM 'Outbox'
               TREEITEM 'Drafts'
               TREEITEM 'Spam'
               TREEITEM 'Sent Items'
               DEFINE NODE 'OTHER'
                  TREEITEM 'Backup'
                  TREEITEM 'Calendar'
               END NODE
            END NODE

            DEFINE NODE 'ABOUT'
            END NODE

         END NODE

      END TREE

      Form_1.TITLE := Form_1.Tree_1.Item ( 1 )
      Form_1.Tree_1.Expand ( 1 )
      Form_1.Tree_1.FONTSIZE := 12

      @ 10, 400 BUTTON Button_1 ;
         CAPTION 'Delete Item' ;
         ACTION Form_1.Tree_1.DeleteItem( Form_1.Tree_1.Value ) ;
         WIDTH 150

      @ 40, 400 BUTTON Button_2 ;
         CAPTION 'Delete All Items' ;
         ACTION Form_1.Tree_1.DeleteAllItems ;
         WIDTH 150

      @ 70, 400 BUTTON Button_3 ;
         CAPTION 'Get Item Count' ;
         ACTION MsgInfo ( Form_1.Tree_1.ItemCount ) ;
         WIDTH 150

      @ 100, 400 BUTTON Button_4 ;
         CAPTION 'DeleteAll / Add Test' ;
         ACTION AddItemTest() ;
         WIDTH 150

      @ 130, 400 BUTTON Button_5 ;
         CAPTION 'Set Value' ;
         ACTION Form_1.Tree_1.VALUE := 1 ;
         WIDTH 150

      @ 160, 400 BUTTON Button_6 ;
         CAPTION 'Get Item' ;
         ACTION MsgInfo ( Form_1.Tree_1.Item ( Form_1.Tree_1.Value ) ) ;
         WIDTH 150

      @ 190, 400 BUTTON Button_7 ;
         CAPTION 'Set Item' ;
         ACTION Form_1.Tree_1.Item( Form_1.Tree_1.Value ) := 'New Item text' ;
         WIDTH 150

      @ 225, 400 BUTTON Button_8 ;
         CAPTION 'Save Tree Stru' ;
         ACTION SaveTreeStru( 'Form_1', 'Tree_1' ) ;
         WIDTH 150

      @ 255, 400 BUTTON Button_9 ;
         CAPTION 'Load Tree Stru' ;
         ACTION LoadTreeStru( 'Form_1', 'Tree_1' ) ;
         WIDTH 150

      @ 290, 400 BUTTON Button_A ;
         CAPTION 'Save Tree Stru as DBF' ;
         ACTION SaveTreeStruDBF( 'Form_1', 'Tree_1' ) ;
         WIDTH 150

      @ 320, 400 BUTTON Button_B ;
         CAPTION 'Load Tree Stru from DBF' ;
         ACTION LoadTreeStruDBF( 'Form_1', 'Tree_1' ) ;
         WIDTH 150

   END WINDOW

   CENTER WINDOW Form_1
   ACTIVATE WINDOW Form_1

RETURN NIL


FUNCTION OnChangeTree ( nItem )

   IF nItem > 0 .AND. nItem <= Form_1.Tree_1.ItemCount
      Form_1.TITLE := "#" + hb_ntos ( nItem ) + " --> " + Form_1.Tree_1.Item ( nItem )
   ELSE
      Form_1.TITLE := "MY APP"
   ENDIF

RETURN NIL


PROCEDURE AddItemTest()

   Form_1.Tree_1.DeleteAllItems

   Form_1.Tree_1.AddItem( 'New Root Item 1', 0 )

   Form_1.Tree_1.AddItem( 'New Item 1.1', 1 )
   Form_1.Tree_1.AddItem( 'New Item 1.2', 1 )
   Form_1.Tree_1.AddItem( 'New Item 1.3', 1 )

   Form_1.Tree_1.AddItem( 'New Item 1.4', 1 )
   Form_1.Tree_1.AddItem( 'New Item 1.4.1', 5 )

   Form_1.Tree_1.NodeFlag ( 5 ) := .T.
   Form_1.Tree_1.Image( 5 ) := { "folder.gif", "folder_chk.gif" }

   Form_1.Tree_1.AddItem( 'New Root Item 2', 0 )

   Form_1.Tree_1.AddItem( 'New Item 2.1', 7 )
   Form_1.Tree_1.AddItem( 'New Item 2.2', 7 )
   Form_1.Tree_1.AddItem( 'New Item 2.3', 7 )

   Form_1.Tree_1.SetFocus

RETURN


FUNCTION _GetTreeStru( cFormName, cTreeName )

   LOCAL aTreeItem := {}, hTree := { => }, hTreeItem
   LOCAL nParent, nItem

   IF _IsControlDefined ( cTreeName, cFormName ) .AND. GetControlType ( cTreeName, cFormName ) == "TREE"

      FOR nItem := 1 TO GetProperty ( cFormName, cTreeName, "ItemCount" )
         hTreeItem := { => }
         hTreeItem[ "id" ] := nItem
         hTreeItem[ "item" ] := GetProperty ( cFormName, cTreeName, "Item", nItem )
         nParent := GetProperty ( cFormName, cTreeName, "ParentValue", nItem )
         IF nParent = NIL
            nParent := 0
         ENDIF
         hTreeItem[ "parent" ] := nParent
         hTreeItem[ "nodeFlag" ] := GetProperty ( cFormName, cTreeName, "NodeFlag", nItem )
         AAdd ( aTreeItem, hTreeItem )
      NEXT nItem
      hTree[ "treeStru" ] := aTreeItem
   ENDIF

RETURN hTree


PROCEDURE SaveTreeStru ( cFormName, cTreeName )

   LOCAL hTreeStru := _GetTreeStru( cFormName, cTreeName )
   LOCAL cFileName := Putfile ( { { 'json Files', '*.json' } }, 'Save Tree Structure', hb_DirSepDel ( hb_cwd() ), .T.,,, .T. )

   IF ! Empty( cFileName )
      StrFile ( hb_jsonEncode( hTreeStru, .T. ), cFileName )
      MsgInfo ( "The tree structure was successfully saved as " + cFileName )
   ENDIF

RETURN


PROCEDURE LoadTreeStru ( cFormName, cTreeName )

   LOCAL hTreeStru, aStru, lValid := .T., nItem := 1
   LOCAL cFName

   IF .NOT. ( _IsControlDefined ( cTreeName, cFormName ) .AND. GetControlType ( cTreeName, cFormName ) == "TREE" )
      RETURN
   ENDIF

   cFName := Getfile ( { { 'json Files', '*.json' } }, 'Load Tree Structure', hb_DirSepDel ( hb_cwd() ), .F., .T. )

   IF ! Empty( cFName ) .AND. File ( cFName )

      hTreeStru := hb_jsonDecode( FileStr ( cFName ) )
      IF hTreeStru = NIL
         MsgInfo ( "The selected file is not in json format." )
         RETURN
      ENDIF
      aStru := hb_HGetDef ( hTreeStru, "treeStru" )

      IF ValType ( aStru ) <> "A"
         MsgInfo ( "The selected file has the wrong structure." )
         RETURN
      ENDIF

      DO WHILE nItem <= Len( aStru )
         IF ValType ( hb_HGetDef ( aStru[ nItem ], "id" ) ) <> "N"
            lValid := .F.
         ENDIF
         IF ValType ( hb_HGetDef ( aStru[ nItem ], "item" ) ) <> "C"
            lValid := .F.
         ENDIF
         IF ValType ( hb_HGetDef ( aStru[ nItem ], "parent" ) ) <> "N"
            lValid := .F.
         ENDIF
         IF ValType ( hb_HGetDef ( aStru[ nItem ], "nodeFlag" ) ) <> "L"
            lValid := .F.
         ENDIF
         IF ! lValid
            MsgInfo ( "The selected file has the wrong structure." )
            RETURN
         ENDIF
         DO Events
         nItem++
      ENDDO

      DoMethod( cFormName, cTreeName, "DeleteAllItems" )

      nItem := 1

      DO WHILE nItem <= Len( aStru )
         DoMethod ( cFormName, cTreeName, "AddItem", hb_HGetDef ( aStru[ nItem ], "item" ), hb_HGetDef ( aStru[ nItem ], "parent" ) )
         SetProperty( cFormName, cTreeName, "NodeFlag", nItem, hb_HGetDef ( aStru[ nItem ], "nodeFlag" ) )
         IF hb_HGetDef ( aStru[ nItem ], "nodeFlag" )
            SetProperty( cFormName, cTreeName, "Image", nItem, { "folder.gif", "folder_chk.gif" } )
         ENDIF
         DO Events
         nItem++
      ENDDO

      DoMethod ( cFormName, cTreeName, "ReDraw" )
      Form_1.Tree_1.Expand ( 1 )

      MsgInfo ( "The tree structure was loaded successfully." )
   ENDIF

RETURN


FUNCTION _GetTreeStruDBF( cFormName, cTreeName )

   LOCAL aTreeItem, aTree := {}
   LOCAL nParent, nItem

   IF _IsControlDefined ( cTreeName, cFormName ) .AND. GetControlType ( cTreeName, cFormName ) == "TREE"

      FOR nItem := 1 TO GetProperty ( cFormName, cTreeName, "ItemCount" )
         aTreeItem := Array( 4 )
         aTreeItem[ 1 ] := nItem
         aTreeItem[ 2 ] := GetProperty ( cFormName, cTreeName, "Item", nItem )
         nParent := GetProperty ( cFormName, cTreeName, "ParentValue", nItem )
         IF nParent = NIL
            nParent := 0
         ENDIF
         aTreeItem[ 3 ] := nParent
         aTreeItem[ 4 ] := GetProperty ( cFormName, cTreeName, "NodeFlag", nItem )
         AAdd( aTree, aTreeItem )
      NEXT nItem
   ENDIF

RETURN aTree


PROCEDURE SaveTreeStruDBF ( cFormName, cTreeName )

   LOCAL aTreeStru := _GetTreeStruDBF( cFormName, cTreeName )
   LOCAL cFileName := Putfile ( { { 'DBF Files', '*.dbf' } }, 'Save Tree Structure', hb_DirSepDel ( hb_cwd() ), .T.,,, .T. )

   IF ! Empty( cFileName )

      dbCreate( cFileName, { { "ID", "N", 5, 0 }, { "ITEM", "C", 50, 0 }, ;
         { "PARENT", "N", 5, 0 }, { "NODEFLAG", "L", 1, 0 } }, "DBFNTX", .T., "DATA" )

      HMG_ArrayToDBF( aTreeStru )
      CLOSE DATA

      MsgInfo ( "The tree structure was successfully saved as " + cFileName )
   ENDIF

RETURN


PROCEDURE LoadTreeStruDBF ( cFormName, cTreeName )

   LOCAL aStru, lValid := .T., nItem := 1
   LOCAL cFName

   IF .NOT. ( _IsControlDefined ( cTreeName, cFormName ) .AND. GetControlType ( cTreeName, cFormName ) == "TREE" )
      RETURN
   ENDIF

   cFName := Getfile ( { { 'DBF Files', '*.dbf' } }, 'Load Tree Structure', hb_DirSepDel ( hb_cwd() ), .F., .T. )

   IF ! Empty( cFName ) .AND. File ( cFName )

      USE ( cFName ) ALIAS DATA NEW
      IF !Used()
         MsgInfo ( "The selected file is not in DBF format." )
         RETURN
      ENDIF

      aStru := HMG_DbfToArray()
      CLOSE DATA
      IF ValType ( aStru ) <> "A"
         MsgInfo ( "The selected file has the wrong structure." )
         RETURN
      ENDIF

      DO WHILE nItem <= Len( aStru[ 1 ] )
         IF ValType ( aStru[ nItem ][ 1 ] ) <> "N"
            lValid := .F.
         ENDIF
         IF ValType ( aStru[ nItem ][ 2 ] ) <> "C"
            lValid := .F.
         ENDIF
         IF ValType ( aStru[ nItem ][ 3 ] ) <> "N"
            lValid := .F.
         ENDIF
         IF ValType ( aStru[ nItem ][ 4 ] ) <> "L"
            lValid := .F.
         ENDIF
         IF ! lValid
            MsgInfo ( "The selected file has the wrong structure." )
            RETURN
         ENDIF
         DO Events
         nItem++
      ENDDO

      DoMethod( cFormName, cTreeName, "DeleteAllItems" )

      nItem := 1

      DO WHILE nItem <= Len( aStru )
         DoMethod ( cFormName, cTreeName, "AddItem", AllTrim( aStru[ nItem ][ 2 ] ), aStru[ nItem ][ 3 ] )
         SetProperty( cFormName, cTreeName, "NodeFlag", nItem, aStru[ nItem ][ 4 ] )
         IF aStru[ nItem ][ 4 ] == .T.
            SetProperty( cFormName, cTreeName, "Image", nItem, { "folder.gif", "folder_chk.gif" } )
         ENDIF
         DO Events
         nItem++
      ENDDO

      DoMethod ( cFormName, cTreeName, "ReDraw" )
      Form_1.Tree_1.Expand ( 1 )

      MsgInfo ( "The tree structure was loaded successfully." )
   ENDIF

RETURN
