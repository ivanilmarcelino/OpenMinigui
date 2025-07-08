 #include "minigui.ch" 
 #include "TSBrowse.ch" 
  
  
 PROCEDURE MAIN 
  
    LOCAL oBrw, aDatos, aArray, aHead, aSize, aFoot, aPict, aAlign, aName 
    LOCAL nY, nX, nW, nH 
  
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
      
       aDatos   := CreateDatos() 
      
       aArray   := aDatos[ 1 ] 
       aHead    := aDatos[ 2 ] 
       aSize    := aDatos[ 3 ] 
       aFoot    := aDatos[ 4 ] 
       aPict    := aDatos[ 5 ] 
       aAlign   := aDatos[ 6 ] 
       aName    := aDatos[ 7 ] 
      
       DEFINE TBROWSE oBrw ; 
              AT nY, nX ALIAS aArray WIDTH nW HEIGHT nH CELL ; 
              FONT       { "Norm", "Bold", "Bold" }          ; 
              BRUSH      { 255, 255, 240 }                   ; 
              HEADERS    aHead                               ; 
              COLSIZES   aSize                               ; 
              PICTURE    aPict                               ; 
              JUSTIFY    aAlign                              ; 
              COLNAMES   aName                               ; 
              COLNUMBER  { 1, 40 }                           ; 
              FOOTERS    aFoot                               ; 
              FIXED ADJUST COLEMPTY                          ; 
              ENUMERATOR   EDIT                              ; 
              GOTFOCUSSELECT 
      
              mySetTsb( oBrw ) 
              myColorTsb( oBrw ) 
      
       END TBROWSE 
      
       oBrw:SetNoHoles() 
  
    END WINDOW 
  
    DoMethod( "test", "Activate" ) 
  
 RETURN 
  
 FUNCTION mySetTsb( oBrw ) 
    WITH OBJECT oBrw 
       :nColOrder    := 0 
       :lNoChangeOrd := .T. 
       :nWheelLines  := 1 
       :lNoGrayBar   := .F. 
       :lNoLiteBar   := .F. 
       :lNoResetPos  := .F. 
       :lNoHScroll   := .T. 
       :lNoPopUp     := .T. 
    END WITH 
 RETURN Nil 
  
 FUNCTION myColorTsb( oBrw ) 
    WITH OBJECT oBrw 
       :nClrLine              := RGB(180,180,180) // COLOR_GRID 
       :SetColor( { 11 }, { { || RGB(0,0,0) } } ) 
       :SetColor( {  2 }, { { || RGB(255,255,240) } } ) 
       :SetColor( {  5 }, { { || RGB(0,0,0) } } ) 
       :SetColor( {  6 }, { { |a,b,c| iif( c:nCell == b,  -CLR_HRED        , -RGB(128,225,225) ) } } ) 
       :SetColor( { 12 }, { { |a,b,c| iif( c:nCell == b,  -RGB(128,225,225), -RGB(128,225,225) ) } } ) 
    END WITH 
 RETURN Nil 
  
 * ====================================================================== 
  
 STATIC FUNCTION CreateDatos() 
  
    LOCAL i, k := 1000, aDatos, aHead, aSize, aFoot, aPict, aAlign, aName 
  
    aDatos := Array( k ) 
    FOR i := 1 TO k 
       aDatos[ i ] := { " ", ;                         // 1 
          i, ;                                         // 2 
          ntoc( i ) + "_123", ;                        // 3 
          Date() + i, ;                                // 4 
          PadR( "Test line - " + ntoc( i ), 20 ), ;    // 5 
          Round( ( 10000 -i ) * i / 3, 2 ), ;          // 6 
          100.00 * i, ;                                // 7 
          0.12, ;                                      // 8 
          Round( 100.00 * i * 0.12, 2 ), ;             // 9 
          Round( 1234567.00 / i, 3 ), ;                // 10 
          PadR( "Line " + StrZero( i, 5 ), 20 ), ;     // 11 
          Date(), ;                                    // 12 
          Time(), ;                                    // 13 
          i % 2 == 0 }                                 // 14 
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
  
 RETURN { aDatos, aHead, aSize, aFoot, aPict, aAlign, aName } 
 