#include "minigui.ch"
#include "TSBrowse.ch"


PROCEDURE MAIN ( Employee, FieldMax )

   LOCAL aDatos, aArray, aHead, aSize, aFoot, aPict, aAlign, aName, aField
   LOCAL oBrw, nY, nX, nW, nH, lArray

   lArray := Empty( Employee )

   SET DECIMALS TO 4
   SET DATE     TO GERMAN
   SET EPOCH    TO 2000
   SET CENTURY  ON
   SET EXACT    ON

   SET FONT TO 'Arial', 11

   DEFINE FONT Norm FONTNAME _HMG_DefaultFontName SIZE _HMG_DefaultFontSize
   DEFINE FONT Bold FONTNAME _HMG_DefaultFontName SIZE _HMG_DefaultFontSize BOLD

   DEFINE WINDOW test ;
      TITLE "SetArray For Report Demo" ;
      MAIN ;
      ON RELEASE {|| iif( ISCHAR(aArray), (aArray)->(dbCloseArea()), ) } ;
      NOMAXIMIZE NOSIZE

      DEFINE STATUSBAR
         STATUSITEM "0"                 // WIDTH 0 FONTCOLOR BLACK
         STATUSITEM "Item 1" WIDTH 230  // FONTCOLOR BLACK
         STATUSITEM "Item 2" WIDTH 230  // FONTCOLOR BLACK
         STATUSITEM "Item 3" WIDTH 230  // FONTCOLOR BLACK
      END STATUSBAR

      nY := 1 + iif( IsVistaOrLater(), GetBorderWidth ()/2, 0 )
      nX := 1 + iif( IsVistaOrLater(), GetBorderHeight()/2, 0 )
      nW := test.WIDTH  - 2 * GetBorderWidth()
      nH := test.HEIGHT - 2 * GetBorderHeight() -    ;
            GetTitleHeight() - test.StatusBar.Height

      aDatos   := CreateDatos( lArray, FieldMax )

      aArray := aDatos[ 1 ]
      aHead  := aDatos[ 2 ]
      aSize  := aDatos[ 3 ]
      aFoot  := aDatos[ 4 ]
      aPict  := aDatos[ 5 ]
      aAlign := aDatos[ 6 ]
      aName  := aDatos[ 7 ]
      aField := aDatos[ 8 ]

      IF ISCHAR( aArray ) ; dbSelectArea( aArray )
      ENDIF

      // aFoot := .F.

      DEFINE TBROWSE oBrw ;
             AT nY, nX ALIAS aArray WIDTH nW HEIGHT nH CELL ;
             FONT       { "Norm", "Bold", "Bold" }          ;
             BRUSH      { 255, 255, 240 }                   ;
             HEADERS    aHead                               ;
             COLSIZES   aSize                               ;
             PICTURE    aPict                               ;
             JUSTIFY    aAlign                              ;
             COLUMNS    aField                              ;
             COLNAMES   aName                               ;
             COLNUMBER  { 1, 50 }                           ;
             FOOTERS    aFoot                               ;
             FIXED      COLSEMPTY                           ;
             LOADFIELDS                                     ;
             ENUMERATOR EDIT GOTFOCUSSELECT

                IF :lIsDbf
             :GetColumn("FLD10"):nWidth += 20
                ENDIF
             mySetTsb  ( oBrw )
             myColorTsb( oBrw )
             mySet2Tsb ( oBrw )

      END TBROWSE ON END {|ob| ob:SetNoHoles(), ob:SetFocus() }

   END WINDOW

   DoMethod( "test", "Activate" )

RETURN

FUNCTION mySetTsb( oBrw )
   WITH OBJECT oBrw
      :nColOrder     := 0
      :lNoChangeOrd  := .T.
      :nWheelLines   := 1
      :lNoGrayBar    := .F.
      :lNoLiteBar    := .F.
      :lNoResetPos   := .F.
      :lNoHScroll    := .T.
      :lNoPopUp      := .T.
      :nHeightCell   += 3
      :nCellMarginLR := 1
/*
      :nCellMarginLR := { |ncol,obr,ocol,nalign,nout|
                          ncol := 0
                          If nout == 0 .or. nout == 2
                             If nalign == DT_LEFT
                                ncol := 2
                             Else
                                ncol := 1
                             EndIf
                          EndIf
                          obr := ocol
                          Return ncol
                        } */
   END WITH
RETURN Nil

FUNCTION mySet2Tsb( oBrw )
   LOCAL nLen, cBrw, nTsb

   WITH OBJECT oBrw
      cBrw := :cControlName
      nTsb := This.&(cBrw).ClientWidth
      nLen := :GetAllColsWidth() - 1
      IF nLen > nTsb
         :lAdjColumn  := .T.
         :lNoHScroll  := .F.
         :lMoreFields := ( :nColCount() > 45 )
      ELSE
         :AdjColumns()
      ENDIF
   END WITH

RETURN Nil

FUNCTION myColorTsb( oBrw )
   WITH OBJECT oBrw
      :nClrLine              := RGB(180,180,180) // COLOR_GRID
      :SetColor( { 11 }, { { || RGB(0,0,0) } } )
      :SetColor( {  2 }, { { || RGB(255,255,240) } } )
      :SetColor( {  5 }, { { || RGB(0,0,0) } } )
      :SetColor( {  6 }, { { |a,b,c| iif( c:nCell == b, -CLR_HRED, -RGB(128,225,225) ) } } )
      :SetColor( { 12 }, { { |a,b,c| iif( c:nCell == b, -CLR_HRED, -RGB(128,225,225) ) } } )
   END WITH
RETURN Nil

* ======================================================================

STATIC FUNCTION CreateDatos( lArray, nField )

   LOCAL i, k := 100
   LOCAL aDatos, aHead, aSize, aFoot, aPict, aAlign, aName, aField

   DEFAULT lArray := .T.

   IF lArray

      aDatos := Array( k )
      FOR i := 1 TO k
         aDatos[ i ] := {      ;                         //
            i, ;                                         // 1
            ntoc( i ) + "_123", ;                        // 2
            Date() + i, ;                                // 3
            PadR( "Test line - " + ntoc( i ), 20 ), ;    // 4
            Round( ( 10000 -i ) * i / 3, 2 ), ;          // 5
            100.00 * i, ;                                // 6
            0.12, ;                                      // 7
            Round( 100.00 * i * 0.12, 2 ), ;             // 8
            Round( 1234567.00 / i, 3 ), ;                // 9
            PadR( "Line " + StrZero( i, 5 ), 20 ), ;     // 10
            Date(), ;                                    // 11
            Time(), ;                                    // 12
            i % 2 == 0 }                                 // 13
      NEXT

      aHead  := AClone( aDatos[ 1 ] )
      // AEval(aHead, {|x,n| aHead[ n ] := "Head_" + hb_ntos(n) })
      AEval( aHead, {| x, n| aHead[ n ] := "Head" + hb_ntos( n ) + ;
         iif( n % 2 == 0, CRLF + "SubHead" + hb_ntos( n ), "" ) } )

      aFoot  := Array( Len( aDatos[ 1 ] ) )
      AEval( aFoot, {| x, n| aFoot[ n ] := n } )
      // aFoot  := .T.                        // подножие есть с пустыми значениями

      aPict     := Array( Len( aDatos[ 1 ] ) )   // можно не задавать, формируются
      aPict[ 10 ] := "99999999999.999"           // автоматом для C,N по мах значению

      aSize     := Array( Len( aDatos[ 1 ] ) )   // можно не задавать, формируются
      aSize[ 10 ] := aPict[ 10 ]                 // автоматом по мах значению в колонке

      aAlign    := Array( Len( aDatos[ 1 ] ) )   // тип поля C   - DT_LEFT
      aAlign[ 2 ] := DT_CENTER                   // D,L - DT_CENTER
                                                 // N   - DT_RIGHT

      aName     := Array( Len( aDatos[ 1 ] ) )
      AEval( aName, {| x, n| aName[ n ] := "MyName_" + hb_ntos( n ) } )

   ELSEIF hb_FileExists( "EMPLOYEE.DBF" )

      USE EMPLOYEE ALIAS TEST NEW
      k := fCount()
      IF ! empty(nField)
         i := val(nField)
         IF i > 2 .and. i < k ; k := i
         ENDIF
      ENDIF
      aDatos := ALIAS()
      aHead  := array(k)
      aFoot  := array(k)
      aName  := array(k)
      aAlign := array(k)
      aField := array(k)

      FOR i := 1 TO k
          aHead[ i ]  := "Head" + hb_ntos( i ) + ;
                      iif( i % 2 == 0, CRLF + "SubHead" + hb_ntos( i ), "" )
          aFoot [ i ] := hb_ntos( i )
          aName [ i ] := 'FLD'+hb_ntos( i )
          aField[ i ] := FieldName( i )
          aAlign[ i ] := DT_LEFT
          switch FieldType( i )
             case 'N' ; aAlign[ i ] := DT_RIGHT  ; exit
             case 'D' ; aAlign[ i ] := DT_CENTER ; exit
             case 'L' ; aAlign[ i ] := DT_CENTER ; exit
          end switch
      NEXT
      aSize := Array( k )   // можно не задавать, формируются автоматом
                             // по мах значению в колонке
      aPict := Array( k )   // можно не задавать, формируются автоматом

   ELSE

      MsgStop('File not found ! ' + "EMPLOYEE.DBF", "ERROR")
      QUIT

   ENDIF

RETURN { aDatos, aHead, aSize, aFoot, aPict, aAlign, aName, aField }
