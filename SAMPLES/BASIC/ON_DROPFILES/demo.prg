/*
 * MINIGUI - Harbour Win32 GUI library Demo
 * 28/07/2019  Contributed by
 *             Grigory Filatov <gfilatov@inbox.ru>
 *             Pierpaolo Martinello <assistenza[at]pcmodula.it>
 */

#include "MiniGUI.ch"

*-------------------------------------------------------------------*
FUNCTION Main()
*-------------------------------------------------------------------*

   DEFINE WINDOW Form_1 AT 50, 50 WIDTH 515 HEIGHT 580 ;
         TITLE "DROP HERE" ;
         NOSIZE ;
         NOMAXIMIZE ;
         NOMINIMIZE ;
         MAIN

      ON KEY ESCAPE ACTION ThisWindow.Release()

      @ 0, 8 FRAME Frame_1 ;
         OF Form_1 ;
         CAPTION "Frame_1" ;
         WIDTH 485 ;
         HEIGHT 340 ;
         BOLD

      @ 20, 15 GRID GRID_1 WIDTH 470 HEIGHT 150 ;
         HEADERS { "FilePath", "FileSize (bytes)" } ;
         WIDTHS { 350, 116 } ;
         JUSTIFY { GRID_JTFY_LEFT, GRID_JTFY_RIGHT } ;
         ITEMS {}

      @ 180, 15 LISTBOX LISTBOX_1 WIDTH 470 HEIGHT 150 ITEMS {}

      @ 350, 8 LABEL Label_1 ;
         OF Form_1 ;
         VALUE "Drag one image here !!!" ;
         WIDTH 485 ;
         HEIGHT 180 ;
         BORDER ;
         CLIENTEDGE ;
         TRANSPARENT ;
         CENTERALIGN ;
         VCENTERALIGN

      @ 355, 15 IMAGE Image_1 ;
         OF Form_1 ;
         PICTURE getstartupFolder() + "\NoPicture.png" ;
         WIDTH 470 ;
         HEIGHT 170 ;
         STRETCH

   END WINDOW

   ACTIVATE WINDOW Form_1 ;
      ON INIT This.OnDropFiles := {| aFiles | ResolveDrop( "Form_1", HMG_GetFormControls( "Form_1" ), aFiles, { "Frame_1" } ) }

RETURN NIL

*-------------------------------------------------------------------*
FUNCTION ResolveDrop( cForm, aCtrl, aFiles, acExcluded )
*-------------------------------------------------------------------*
   LOCAL mx, my, ni, tx, ty, bx, by, ct
   LOCAL aRect := { 0, 0, 0, 0 }
   LOCAL aCtlPos := {}
   LOCAL cTarget, cFilePath, cFileSize

   DEFAULT acExcluded TO { "" }
   AEval( acExcluded, {| x, y | acExcluded[ y ] := Upper( acExcluded[ y ] ), HB_SYMBOL_UNUSED( x ) } )

   my := GetCursorRow() /* Mouse y position on desktop */
   mx := GetCursorCol() /* Mouse x position on desktop */

   FOR ni = 1 TO Len( aCtrl )
      GetWindowRect( GetControlHandle( aCtrl[ ni ], cForm ), aRect )
      AAdd( aCtlPos, { Upper( aCtrl[ ni ] ), aRect[ 1 ], aRect[ 2 ], aRect[ 3 ], aRect[ 4 ] } )
   NEXT ni

   cTarget := ""
   ni := 0
   DO WHILE ni < Len( aCtlPos ) .AND. Len( cTarget ) == 0
      ni++
      tx := aCtlPos[ ni, 2 ] /* Top-Left Corner x */
      ty := aCtlPos[ ni, 3 ] /* Top-Left Corner y */
      bx := aCtlPos[ ni, 4 ] /* Right-Bottom Corner x */
      by := aCtlPos[ ni, 5 ] /* Right-Bottom Corner y */

      IF mx >= tx .AND. mx <= bx .AND. my >= ty .AND. my <= by .AND. ( AScan( acExcluded, actlpos[ ni, 1 ] ) < 1 )
         cTarget := aCtlPos[ ni, 1 ]
      ENDIF
   ENDDO

   IF Len( cTarget ) > 0
      ct := GetControlType( cTarget, cForm )

      DO CASE
      CASE ct == "GRID" .OR. ct == "MULTIGRID"
         FOR ni = 1 TO Len( aFiles )
            cFilePath := aFiles[ ni ]
            cFileSize := TRANS( FileSize( cFilePath ), "999,999,999,999,999" )
            AddNewItem( cForm, cTarget, { cFilePath, cFileSize }, .F. )
         NEXT ni
      CASE ct == "LIST"
         FOR ni = 1 TO Len( aFiles )
            AddNewItem( cForm, cTarget, aFiles[ ni ], .T. )
         NEXT ni
      CASE ct == "LABEL" .OR. ct = "IMAGE"
         FOR ni = 1 TO Len( aFiles )
            Form_1.Image_1.Release
            DO EVENTS
            @ 355, 15 IMAGE Image_1 ;
               OF Form_1 ;
               PICTURE aFiles[ ni ] ;
               WIDTH 470 ;
               HEIGHT 170 ;
               STRETCH ;
               WHITEBACKGROUND ;
               TRANSPARENT

            InkeyGUI( 3000 )
         NEXT
      ENDCASE
   ENDIF

RETURN NIL

*-------------------------------------------------------------------*
STATIC FUNCTION AddNewItem( cForm, cControl, xValue, lList )
*-------------------------------------------------------------------*
   LOCAL lExist, nItemCount, ni

   lExist := .F.
   nItemCount := GetProperty( cForm, cControl, "ItemCount" )
   ni := 0

   DO WHILE ni < nItemCount .AND. lExist = .F.
      ni += 1
      IF lList
         lExist := ( GetProperty( cForm, cControl, "Item", ni ) == xValue )
      ELSE
         lExist := ( GetProperty( cForm, cControl, "Cell", ni, 1 ) == xValue[ 1 ] )
      ENDIF
   ENDDO

   IF ! lExist
      DoMethod( cForm, cControl, "AddItem", xValue )
   ENDIF

RETURN NIL
