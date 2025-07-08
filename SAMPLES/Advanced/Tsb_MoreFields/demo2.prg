#include "hmg.ch"
#include "TSBrowse.ch"

REQUEST DBFCDX

function Main()

   local cDbf     := "customer.dbf"
   local cNew     := "cust400f.dbf"
   local i,j,n,c,obrw, aCol, aData, aRow, aCols, aNew := {}

   if File( cNew )
      USE ( cNew ) NEW ALIAS C400 VIA "DBFCDX"
   else
      USE ( cDbf ) SHARED
      aCols    := DBSTRUCT( cDbf )
      AAdd( aNew, aCols[ 1 ] )

      for i := 1 to 40
         c  := StrZero( i, 2, 0 )
         for n := 2 to 11
            aCol  := AClone( aCols[ n ] )
            aCol[ 1 ] += c
            AAdd( aNew, aCol )
         next
      next

      aData := hmg_DbfToArray( nil, nil, { || RecNo() <= 100 } )
      CLOSE DATA

      DBCREATE( cNew, aNew, "DBFCDX", .T., "C400" )
      for each aRow in aData
         DBAPPEND()
         n  := 2
         for i := 1 to 40
            for j := 2 to 11
               FieldPut( n, aRow[ j ] )
               n++
            next
         next
      next
      GO TOP
   endif

   SET DELETE ON

   DEFINE WINDOW win_1 AT 0, 0 WIDTH 1004 HEIGHT 541 ;
      MAIN TITLE Alias() + ": " + hb_ntos( FCount() ) + " Fields" NOMAXIMIZE NOSIZE

      DEFINE TOOLBAR ToolBar_1 BUTTONSIZE 8,36 IMAGESIZE 24,24 FLAT BORDER

      BUTTON TOP_1 ;
         PICTURE "res\go_first.bmp" ;
         TOOLTIP "Top" ;
         ACTION ( oBrw:GoTop(), oBrw:SetFocus() )

      BUTTON PREV_1 ;
         PICTURE "res\go_prev.bmp" ;
         TOOLTIP "Up" ;
         ACTION ( oBrw:GoUp(), oBrw:SetFocus() )

      BUTTON DOWN_1 ;
         PICTURE "res\go_next.bmp" ;
         TOOLTIP "Down" ;
         ACTION ( oBrw:GoDown(), oBrw:SetFocus() )

      BUTTON BOTTOM_1 ;
         PICTURE "res\go_last.bmp" ;
         TOOLTIP "Bottom" ;
         ACTION ( oBrw:GoBottom(), oBrw:SetFocus() ) SEPARATOR

      BUTTON NEW_1 ;
         PICTURE "res\frm_new.bmp" ;
         TOOLTIP "Add" ;
          ACTION ( ( oBrw:cAlias )->( dbAppend() ), oBrw:GoToRec( ( oBrw:cAlias )->( RecNo() ), .T. ), oBrw:SetFocus() )

      BUTTON EDIT_1 ;
         PICTURE "res\frm_edit.bmp" ;
         TOOLTIP "Edit" ;
         ACTION ( oBrw:PostMsg( WM_KEYDOWN, VK_F10, 0 ), oBrw:SetFocus() )

      BUTTON DELETE_1 ;
         PICTURE "res\frm_delete.bmp" ;
         TOOLTIP "Delete" ;
         ACTION ( iif( MsgYesNo( "Delete Record ?", , .T. ), oBrw:DeleteRow(), NIL ), oBrw:SetFocus() ) SEPARATOR

      BUTTON PRINT_1 ;
         PICTURE "res\frm_print.bmp" ;
         TOOLTIP "Report" ;
         ACTION PrintData( oBrw ) SEPARATOR

      BUTTON EXIT_1 ;
         PICTURE "res\frm_exit.bmp" ;
         TOOLTIP "Exit" ;
         ACTION Win_1.Release

      END TOOLBAR

      DEFINE TBROWSE obrw AT 56, 20 ;
         CELLED SELECTOR "res\pointer.bmp" ;
         COLORS CLR_BLACK, CLR_WHITE, CLR_BLACK, { RGB( 231, 242, 255 ), GetSysColor( COLOR_GRADIENTINACTIVECAPTION ) } ;
         ALIAS Alias() ;
         WIDTH win_1.Width - 40 - GetBorderWidth()/2 HEIGHT 420 ;
         FONT "Arial" ;
         SIZE 9 ;
         ON INIT {|ob| TsbCreate( ob, .T. ) }

      END TBROWSE ON END {|ob| TsbCreate( ob, .F. ) }

   END WINDOW

   CENTER WINDOW win_1
   ACTIVATE WINDOW win_1

return nil

*----------------------------------------
STATIC PROCEDURE TsbCreate( obrw, lInit )
*----------------------------------------
   local aCols, aCol, cCust, n, a, i, c
   local aFields, aNew := {}

   IF lInit

      cCust := "customer"
      aCols := hmg_DBfSTRUCT( cCust )
      AAdd( aNew, aCols[ 1 ] )

      for i := 1 to 40
         c  := StrZero( i, 2, 0 )
         for n := 2 to 11
            aCol  := AClone( aCols[ n ] )
            aCol[ 1 ] += c
            AAdd( aNew, aCol )
         next
      next

      // initial columns
      aFields := {}
      for n := 1 to Len( aNew )
         a  := aNew[ n ][ 1 ]
         AAdd( aFields, a )
      next

      LoadFields( "oBrw", "win_1", .T., aFields )

      with object oBrw
         :nHeightCell += 5
         :nHeightHead := oBrw:nHeightCell

         :SetColor( { 5 }, { CLR_WHITE } )
         :SetColor( { 6 }, { RGB( 0, 0, 128 ) } )

         :aColumns[ 1 ]:lEdit := .F.

         :SetAppendMode( .F. )
         :SetDeleteMode( .T., .F. )

         :lNoResetPos  := .T.
         :lNoMoveCols  := .T.
         :lNoKeyChar   := .T.
         :lNoChangeOrd := .T.
         :nFireKey := VK_F10         // default Edit key

         :lMoreFields := .T.
      end object

   ELSE

      obrw:SetNoHoles()
      obrw:SetFocus()

   ENDIF

RETURN

*---------------------------------
STATIC PROCEDURE PrintData( oBrw )
*---------------------------------
   LOCAL aStruct, cCust, n, nLen, a
   LOCAL PrevRec
   LOCAL aHdr := {}
   LOCAL aLen := {}
   LOCAL aHdr1
   LOCAL aTot
   LOCAL aFmt

   cCust    := "customer"
   aStruct  := hmg_DBfSTRUCT( cCust )
   ASize( aStruct, Len( aStruct ) - 1 )
   nLen  := Len( aStruct )
   for n := 2 to nLen
      a  := AClone( aStruct[ n ] )
      AAdd( aHdr, a[1] + '01' )
      AAdd( aLen, a[3] )
   next

   aHdr1 := Array( Len( aHdr ) )
   aTot  := Array( Len( aHdr ) )
   aFmt  := Array( Len( aHdr ) )
   AFill( aHdr1, '' )
   AFill( aTot, .F. )
   AFill( aFmt, '' )

   PrevRec := ( oBrw:cAlias )->( RecNo() )
   ( oBrw:cAlias )->( dbGoTop() )

   DO REPORT ;
      TITLE Upper( cCust ) + ' Database List' ;
      HEADERS  aHdr1, aHdr            ;
      FIELDS   aHdr                   ;
      WIDTHS   aLen                   ;
      TOTALS   aTot                   ;
      NFORMATS aFmt                   ;
      WORKAREA &( oBrw:cAlias )       ;
      LMARGIN  3                      ;
      TMARGIN  3                      ;
      PAPERSIZE DMPAPER_A4            ;
      PREVIEW

   ( oBrw:cAlias )->( dbGoto( PrevRec ) )

RETURN
