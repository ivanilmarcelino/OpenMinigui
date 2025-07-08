/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2002 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
*/

#include "hmg.ch"

MEMVAR cFile

*-----------------------------------------------------------------------------*
FUNCTION Main
*-----------------------------------------------------------------------------*
   PRIVATE cFile := "test.rtf"

   SET AUTOADJUST ON

   OpenTable()

   DEFINE WINDOW Form_1 ;
      AT 0, 0 ;
      WIDTH 640 HEIGHT 480 ;
      TITLE 'Harbour MiniGUI RichEdit Demo - Based Upon a Contribution by Janusz Pora' ;
      MAIN ;
      ON INIT ReadField() ;
      ON RELEASE CloseTable()

   ON KEY CONTROL + N ACTION New_File()
   ON KEY CONTROL + O ACTION Open_File()
   ON KEY CONTROL + S ACTION Save_File( 0 )

   DEFINE MAIN MENU
      POPUP '&File'
         ITEM '&New' + Chr( 9 ) + 'Ctrl+N'   ACTION New_File()
         ITEM '&Open' + Chr( 9 ) + 'Ctrl+O'  ACTION Open_File()
         ITEM '&Save' + Chr( 9 ) + 'Ctrl+S'  ACTION Save_File( 0 )
         ITEM 'Save &as ...'                 ACTION Save_File( 1 )
         SEPARATOR
         ITEM 'E&xit'  ACTION Form_1.Release
      END POPUP
   END MENU

   @ 20, 10 LABEL Label_1 ;
      VALUE "This is Richedit control with FILE clause set" ;
      AUTOSIZE ;
      FONT "Arial" SIZE 10 BOLD

   @ 45, 10 RICHEDITBOX Edit_1 ;
      WIDTH 610 ;
      HEIGHT 140 ;
      FILE cFile ;
      VALUE '' ;
      TOOLTIP 'RichEditBox';
      ON CHANGE Form_1.Btn_2.Enabled := .T. ;
      NOHSCROLL

   @ 210, 10 LABEL Label_2 ;
      VALUE "This is Richedit control with FIELD clause set" ;
      AUTOSIZE ;
      FONT "Arial" SIZE 10 BOLD

   @ 235,10  BROWSE Browse_1;
      WIDTH 300 ;
      HEIGHT 140 ;
      HEADERS { 'Code', 'First Name', 'Biography' } ;
      WIDTHS { 50, 120, 110 } ;
      WORKAREA Test ;
      FIELDS { 'Test->Code', 'Test->First', 'Test->Bio' } ;
      VALUE 1;
      ON CHANGE ChangeTest()

   @ 235, 315 RICHEDITBOX Edit_2 ;
      WIDTH 295 ;
      HEIGHT 140 ;
      FIELD test->bio ;
      VALUE '' ;
      TOOLTIP 'RichEditBox';
      ON CHANGE Form_1.Btn_3.Enabled := .T.

   @ 390, 20 BUTTON Btn_1 ;
      CAPTION 'Open File' ;
      ACTION Open_File();
      WIDTH 80 ;
      HEIGHT 24 ;
      TOOLTIP 'Load Rich Text File'

   @ 390, 120 BUTTON Btn_2 ;
      CAPTION 'Save File' ;
      ACTION Save_File( 0 );
      WIDTH 80 ;
      HEIGHT 24 ;
      TOOLTIP 'Save Rich Text File'

   @ 390, 320 BUTTON Btn_3 ;
      CAPTION 'Save Field' ;
      ACTION SaveField();
      WIDTH 80 ;
      HEIGHT 24 ;
      TOOLTIP 'Save data from RichEditBox'

   @ 390, 420 BUTTON Btn_4 ;
      CAPTION 'Exit' ;
      ACTION Form_1.Release;
      WIDTH 80 ;
      HEIGHT 24 ;
      TOOLTIP 'Exit from program'

   DEFINE CONTEXT MENU CONTROLS Edit_1, Edit_2
      MENUITEM "&Undo" ACTION mnuEdit_Click( "UNDO" ) NAME mnuEditUndo
      SEPARATOR
      MENUITEM "Cu&t"  ACTION mnuEdit_Click( "CUT" ) NAME mnuEditCut
      MENUITEM "&Copy" ACTION mnuEdit_Click( "COPY" ) NAME mnuEditCopy
      MENUITEM "&Paste" ACTION mnuEdit_Click( "PASTE" ) NAME mnuEditPaste
      MENUITEM "&Delete" ACTION mnuEdit_Click( "DEL" ) NAME mnuEditDelete
      SEPARATOR
      MENUITEM "Select &All" ACTION mnuEdit_Click( "SELALL" ) NAME mnuEditSelAll
   END MENU

   DEFINE TIMER mnuEditContext INTERVAL 100 ACTION EnableEditMenuItems()

   END WINDOW

   Form_1.Edit_1.AutoFont := .T.
   Form_1.Edit_2.AutoFont := .F.

   Form_1.Btn_2.Enabled := .F.
   Form_1.Btn_3.Enabled := .F.

   Form_1.Center

   Form_1.Activate

RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION New_File()
*-----------------------------------------------------------------------------*
   LOCAL  cBuffer := ''

   _Setvalue ( 'Edit_1', 'Form_1', cBuffer )
   Form_1.Btn_2.Enabled := .F.

RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION Read_file( c_File )
*-----------------------------------------------------------------------------*
   LOCAL typ, cRichValue := ""

   IF ! File( c_File )
      MSGSTOP( "File I/O error, cannot proceed" )
      RETURN NIL
   ENDIF

   IF Upper( AllTrim( SubStr( c_File, RAt( '.', c_File ) + 1 ) ) ) == 'RTF'
      typ := 2
   ELSE
      typ := 1
   ENDIF

   Form_1.Edit_1.RichValue := cRichValue

   _DataRichEditBoxOpen ( 'Edit_1', 'Form_1', c_File, typ )

   Form_1.Btn_2.Enabled := .F.

RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION Open_File()
*-----------------------------------------------------------------------------*
   cFile := GetFile( { { 'Rich text File', '*.rtf' }, { 'Text File', '*.txt' } }, 'Get File' )

   IF !Empty( cFile )
      Read_file( cFile )
   ENDIF

RETURN NIL

*-----------------------------------------------------------------------------*
FUNCTION Save_File( met )
*-----------------------------------------------------------------------------*
   LOCAL c_File := cFile

   IF met == 1
      c_File := PutFile ( { { 'Rich text File', '*.rtf' }, { 'Text File', '*.txt' } }, 'Get File' )
   ENDIF

   IF !Empty( c_File )

      IF Upper( AllTrim( SubStr( c_File, RAt( '.', c_File ) + 1 ) ) ) == 'RTF'
         MemoWrit( c_File, Form_1.Edit_1.RichValue )
      ELSE
         MemoWrit( c_File, Form_1.Edit_1.Value )
      ENDIF

      Form_1.Btn_2.Enabled := .F.

   ENDIF

RETURN NIL

FUNCTION OpenTable()
   USE test

RETURN NIL

FUNCTION CloseTable()
   USE

RETURN NIL

FUNCTION ReadField()

   DoMethod ( 'Form_1', 'Edit_2', 'Refresh' )
   Form_1.Btn_3.Enabled := .F.

RETURN NIL

FUNCTION ChangeTest()

   LOCAL rec
   rec := Form_1.Browse_1.Value
   dbGoto( rec )
   ReadField()

RETURN NIL

FUNCTION SaveField()

   DoMethod ( 'Form_1', 'Edit_2', 'Save' )
   Form_1.Btn_3.Enabled := .F.

RETURN NIL

/*
   RichEdit with default Windows Context Popup Menu

   Standard popup items are:
   Undo - backs out all changes in the undo buffer.
   Cut - copies the selected text to the Clipboard in CF_TEXT format and then deletes the selection.
   Copy - copies the selected text in the edit control to the Clipboard in CF_TEXT format.
   Paste - pastes the contents of the Clipboard into edit control, replacing the current selection.
   Delete - removes the selected text from the edit control.
   Select All - selects all text in the edit control.
*/

FUNCTION EnableEditMenuItems()

   LOCAL cParent, hEdit, aSelRange, nStart, nEnd, TxtLen

   IF ! Empty( _HMG_xControlsContextMenuID )

      cParent := GetParentFormName( _HMG_xControlsContextMenuID )
      hEdit := GetControlHandleByIndex( _HMG_xControlsContextMenuID )

      // Current selection range:
      aSelRange := RichEditBox_GetSelRange( hEdit )
      nStart := aSelRange[ 1 ]
      nEnd := aSelRange[ 2 ]

      // Undo:
      SetProperty( cParent, "mnuEditUndo", "Enabled", RichEditBox_CanUndo( hEdit ) )

      // Cut, Copy & Delete: enable if a selection, disable if no selection
      SetProperty( cParent, "mnuEditCut", "Enabled", ( nStart < nEnd ) )
      SetProperty( cParent, "mnuEditCopy", "Enabled", ( nStart < nEnd ) )
      SetProperty( cParent, "mnuEditDelete", "Enabled", ( nStart < nEnd ) )

      // Paste: enable of clipboard text, disable if not
      SetProperty( cParent, "mnuEditPaste", "Enabled", RichEditBox_CanPaste( hEdit ) )

      // Select All: disable if everything's already selected, enable otherwise.
      TxtLen := RichEditBox_GetTextLength( hEdit )
      SetProperty( cParent, "mnuEditSelAll", "Enabled", .NOT. ( nStart == 0 .AND. nEnd == TxtLen + 1 ) )

   ENDIF

RETURN NIL

FUNCTION mnuEdit_Click( cAction )

   LOCAL hEdit

   hEdit := GetControlHandleByIndex( _HMG_xControlsContextMenuID )

   DO CASE
   CASE cAction == "UNDO"
      RichEditBox_ChangeUndo( hEdit ) // Ctrl+Z
   CASE cAction == "CUT"
      RichEditBox_SelCut( hEdit )     // Ctrl+X
   CASE cAction == "COPY"
      RichEditBox_SelCopy( hEdit )    // Ctrl+C
   CASE cAction == "PASTE"
      RichEditBox_SelPaste( hEdit )   // Ctrl+V
   CASE cAction == "DEL"
      RichEditBox_SelClear( hEdit )   // Del
   CASE cAction == "SELALL"
      RichEditBox_SelectAll( hEdit )  // Ctrl+A
   END CASE

RETURN NIL
