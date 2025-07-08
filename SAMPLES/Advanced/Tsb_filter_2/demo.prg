#include "hmg.ch"
#include "TSBrowse.ch"

REQUEST DBFCDX

*----------------------------------------
FUNCTION Main()
*----------------------------------------
   LOCAL obrw
   LOCAL aFilters := { { .F., "AGE>40" }, { .F., "STATE='NY'" }, { .F., "MARRIED" } }
   LOCAL aCheck[ 3 ], n
   LOCAL nRow := 40
   LOCAL nCol := 40

   USE CUSTOMER NEW SHARED VIA "DBFCDX"

   DEFINE WINDOW win_1 ;
         AT 0, 0 ;
         WIDTH 600 HEIGHT 400 ;
         MAIN ;
         TITLE Alias() + ": Dynamic Filter Test" ;
         ICON "lupa.ico" ;
         FONT "Arial" ;
         SIZE 10 ;
         NOMAXIMIZE NOSIZE

      FOR n := 1 TO Len( aFilters )

         aCheck[ n ] := CreateChk( nRow, nCol, thiswindow.Name, aFilters, n )
         this.( aCheck[ n ] ).Cargo := n
         this.( aCheck[ n ] ).OnChange := {|| n := this.Cargo, aFilters[ n, 1 ] := ! aFilters[ n, 1 ], ;
            SetProperty( 'win_1', "Label_1", "Value", "FILTER : " + ResetFilter( obrw, aFilters ) ) }

         nCol += 150

      NEXT

      @ 80, 40 LABEL Label_1 VALUE "FILTER : " WIDTH 400 HEIGHT 25 CENTERALIGN

      DEFINE TBROWSE obrw AT 115, 40 ;
         CELLED SELECTOR "pointer.bmp" ;
         COLORS CLR_BLACK, CLR_WHITE, CLR_BLACK, { RGB( 230, 240, 255 ), GetSysColor( COLOR_GRADIENTINACTIVECAPTION ) } ;
         ALIAS Alias() ;
         WIDTH win_1.Width - 70 - GetBorderWidth() HEIGHT 220 ;
         ON INIT {| ob | TsbCreate( ob, .T. ) }

      END TBROWSE ON END {| ob | TsbCreate( ob, .F. ) }

   END WINDOW

   ON KEY ESCAPE OF win_1 ACTION win_1.Release()

   CENTER WINDOW win_1
   ACTIVATE WINDOW win_1

RETURN NIL

*----------------------------------------
STATIC PROCEDURE TsbCreate( obrw, lInit )
*----------------------------------------
   LOCAL aFields, oCol

   IF lInit

      // initial columns
      aFields := { "ID", "CITY", "STATE", "MARRIED", "AGE" }

      LoadFields( "oBrw", "win_1", .F., aFields )

      AEval( oBrw:aColumns, {|oCol| oCol:lFixLite := .T., oCol:cHeading := oCol:cName } )

      WITH OBJECT oBrw
         :nHeightCell += 4
         :nHeightHead := oBrw:nHeightCell - 2

         :SetColor( { 5 }, { { |nAt,nNr,oBr| nAt := Nil, ;
                      If( oBr:nCell == nNr, RGB( 250, 230, 250 ), CLR_BLACK ) } } )
         :SetColor( { 6 }, { { |nAt,nNr,oBr| nAt := Nil, ;
                      If( oBr:nCell == nNr, RGB( 0, 0, 128 ), RGB( 230, 230, 230 ) ) } } )

         :SetAppendMode( .F. )
         :SetDeleteMode( .T., .F. )

         :lNoResetPos := .T.
         :lNoMoveCols := .T.
         :lNoKeyChar := .T.
         :lNoChangeOrd := .T.
         :lNoHScroll := .T.
      END OBJECT

   ELSE

      obrw:SetNoHoles()
      obrw:SetFocus()

   ENDIF

RETURN

*----------------------------------------
STATIC FUNCTION CreateChk( nRow, nCol, oDlg, aFilters, nIndex )
*----------------------------------------

   @ nRow, nCol CheckBox NUL PARENT ( oDlg ) ;
      CAPTION aFilters[ nIndex, 2 ] VALUE aFilters[ nIndex, 1 ] ;
      WIDTH 150 HEIGHT 21

RETURN HMG_GetFormControls( oDlg, "CHECKBOX" )[ nIndex ]

*----------------------------------------
STATIC FUNCTION ResetFilter( obrw, aFilters )
*----------------------------------------
   LOCAL cFilter
   LOCAL af := {}

   AEval( aFilters, {| a | iif( a[ 1 ], AAdd( af, a[ 2 ] ), NIL ) } )

   cFilter := LB_Array2String( af, " .AND. " )

   IF Empty( cFilter )
      oBrw:FilterData()
   ELSE
      oBrw:FilterData( cFilter, , .T. )
   ENDIF

RETURN cFilter
