*+--------------------------------------------------------------------
*+
*+ Source Module => \FILEICO\FILEICON.PRG
*+
*+    Copyright(C) 1983-2020 by Auge & Ohr
*+
*+    Functions: Procedure MAIN()
*+               Static Procedure DirImage()
*+               Static Procedure Doit()
*+               Static Function UpPath()
*+               Static Procedure FillWithDir()
*+               Static Procedure DoReSizeMain()
*+               Static Procedure SortDirTop()
*+
*+    Reformatted by Click! 2.05.30 on Aug-26-2020 at  5:45 am
*+
*+--------------------------------------------------------------------

#include "hmg.ch"
#include "directry.ch"

#xtranslate IsWin10()  => hb_osIsWin10()

STATIC cWorkPath := "C:\"

*+--------------------------------------------------------------------
*+
*+    Procedure MAIN()
*+
*+--------------------------------------------------------------------
*+
PROCEDURE Main

   LOCAL aPos := { 0, 0 }
   LOCAL aSize := { 800, 600 }
   LOCAL aDir

   SET CENTURY ON

   aDir := Directory( cWorkPath + "*.*", "DHS" )
   SortDirTop( aDir )

   DEFINE WINDOW Win_1 ;
         AT aPos[ 1 ], aPos[ 2 ] ;
         WIDTH aSize[ 1 ] HEIGHT aSize[ 2 ] ;
         TITLE 'Test Directory List with System Icons' ;
         MAIN ;
         ON MAXIMIZE DoReSizeMain( ThisWindow.Name ) ;
         ON SIZE DoReSizeMain( ThisWindow.Name )

      DEFINE GRID Grid_1
         PARENT Win_1
         ROW 10
         COL 10
         WIDTH aSize[ 1 ] - 40
         HEIGHT aSize[ 2 ] - 60
         ITEMS aDir
         VALUE NIL
         WIDTHS { 450, 80, 78, 64, 60 }
         HEADERS { 'Name', 'Size', 'Date', 'Time', 'Attr' }
         FONTNAME "Arial"
         FONTSIZE 10
         ITEMCOUNT Len( aDir )
         SHOWHEADERS .T.
         CELLNAVIGATION .F.
         JUSTIFY { GRID_JTFY_LEFT, GRID_JTFY_RIGHT, GRID_JTFY_RIGHT, GRID_JTFY_LEFT, GRID_JTFY_LEFT }
         ONDBLCLICK Doit( ThisWindow.NAME, This.Name )
      END GRID
   END WINDOW

   // get System-Icon Imagelist
   ListView_SetImageList( Win_1.Grid_1.HANDLE, BT_ImageListGetSystemIcon() )
   // get Image Index for each file
   DirImage( cWorkPath, aDir, "Win_1", "Grid_1" )

   CENTER WINDOW Win_1
   ACTIVATE WINDOW Win_1

RETURN

*+--------------------------------------------------------------------
*+
*+    Static Procedure DirImage()
*+
*+    Called from ( fileicon.prg )   1 - procedure main()
*+                                   1 - static procedure fillwithdir()
*+
*+--------------------------------------------------------------------
*+
STATIC PROCEDURE DirImage( cPath, aDir, cForm, cObj )

   LOCAL i, iMax := Len( aDir )
   LOCAL nIndex, cFile

   FOR i := 1 TO iMax
      cFile := cPath + aDir[ i ][ F_NAME ]

      IF "D" $ aDir[ i ][ F_ATTR ]
         nIndex := iif( IsWin10(), 3, 1 )
      ELSE
         nIndex := GETICOINDEX( cFile )
      ENDIF

      ListView_SetItemImageIndex( GetControlHandle( cObj, cForm ), i, 1, nIndex )
   NEXT

RETURN

*+--------------------------------------------------------------------
*+
*+    Static Procedure Doit()
*+
*+    Called from ( fileicon.prg )   1 - procedure main()
*+
*+--------------------------------------------------------------------
*+
STATIC PROCEDURE Doit( cForm, cObj )

   LOCAL cPath, cText, cAttr, nItem
   LOCAL cNewPath

   nItem := GetProperty( cForm, cObj, "Value" )
   cText := GetProperty( cForm, cObj, "CELL", nItem, F_NAME )
   cAttr := GetProperty( cForm, cObj, "CELL", nItem, F_ATTR )
   cPath := cWorkPath

   IF cText == "." .AND. Len( cText ) == 1

   ELSEIF cText == ".." .AND. "D" $ cAttr
      cNewPath := UpPath( cPath )
      FillWithDir( cForm, cObj, cNewPath )
   ELSE
      IF "D" $ cAttr
         cNewPath := cPath + cText + "\"
         FillWithDir( cForm, cObj, cNewPath )
      ELSE
         ShellExecute( 0, "open", cPath + cText,,, SW_SHOW )
      ENDIF
   ENDIF

   InvalidateRect( GetControlHandle( cObj, cForm ), 0 )

RETURN

*+--------------------------------------------------------------------
*+
*+    Static Function UpPath()
*+
*+    Called from ( fileicon.prg )   1 - static procedure doit()
*+
*+--------------------------------------------------------------------
*+
STATIC FUNCTION UpPath( cFull )

   LOCAL cRet := ""
   LOCAL nPosi
   LOCAL nStart

   IF ( nStart := RAt( "\", cFull ) ) > 0
      cRet := SubStr( cFull, 1, nStart - 1 )
      nPosi := RAt( "\", cRet )
      cRet := SubStr( cRet, 1, nPosi )
   ENDIF

RETURN cRet

*+--------------------------------------------------------------------
*+
*+    Static Procedure FillWithDir()
*+
*+    Called from ( fileicon.prg )   2 - static procedure doit()
*+
*+--------------------------------------------------------------------
*+
STATIC PROCEDURE FillWithDir( cForm, cObj, cNewPath )

   LOCAL aNewDir, i, iMax, xLast

   cNewPath := hb_DirSepAdd( cNewPath )
   aNewDir := Directory( cNewPath + "*.*", "DHS" )

   IF Empty( aNewDir )
      RETURN
   ENDIF

   DoMethod( cForm, cObj, "DisableUpdate" )
   DoMethod( cForm, cObj, "DeleteAllItems" )

   SortDirTop( aNewDir )
   iMax := Len( aNewDir )
   xLast := ""
   FOR i := 1 TO iMax
      IF xLast <> aNewDir[ i ][ F_NAME ]
         xLast := aNewDir[ i ][ F_NAME ]
         DoMethod( cForm, cObj, "addItem", aNewDir[ i ] )
      ENDIF
   NEXT
   DirImage( cNewPath, aNewDir, cForm, cObj )

   DoMethod( cForm, cObj, "Refresh" )
   DoMethod( cForm, cObj, "EnableUpdate" )
   DoMethod( cForm, cObj, "SetFocus" )

   cWorkPath := cNewPath

   DO EVENTS

RETURN

*+--------------------------------------------------------------------
*+
*+    Static Procedure DoReSizeMain()
*+
*+    Called from ( fileicon.prg )   2 - procedure main()
*+
*+--------------------------------------------------------------------
*+
STATIC PROCEDURE DoReSizeMain( fMain )

   LOCAL mainWidth := GetProperty( fMain, 'Width' )
   LOCAL mainHeight := GetProperty( fMain, 'Height' )

   SetProperty( fMain, "Grid_1", "Width", mainWidth - 40 )
   SetProperty( fMain, "Grid_1", "Height", mainHeight - 60 )

RETURN

*+--------------------------------------------------------------------
*+
*+    Static Procedure SortDirTop()
*+
*+    Called from ( fileicon.prg )   1 - procedure main()
*+                                   1 - static procedure fillwithdir()
*+
*+--------------------------------------------------------------------
*+
STATIC PROCEDURE SortDirTop( aDirOut )

   LOCAL nStartAt := 1

   // "D"irectry On Top
   ASort( aDirOut, , , {| x | "D" $ x[ F_ATTR ] } )

   AEval( aDirOut, {| x, i | nStartAt := iif( "D" $ x[ F_ATTR ], i, nStartAt ) } )
   ASort( aDirOut, 1, nStartAt, {| x, y | Lower( x[ F_NAME ] ) < Lower( y[ F_NAME ] ) } )

   ASort( aDirOut, nStartAt + 1, , {| x, y | x[ F_NAME ] < y[ F_NAME ] } )

RETURN

*
* EOF
*

*+ EOF: FILEICON.PRG
