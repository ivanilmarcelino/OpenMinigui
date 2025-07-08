/*

  MINIGUI - Harbour Win32 GUI library Demo/Sample
 
  Copyright 2002-08 Roberto Lopez <harbourminigui@gmail.com>
 
  MsgExtended is Freevare HMG Message function. 
    
  Test program (MsgExtTest.prg) also demonstrate using
  some handy HMG functions and commands.
  
  All bug reports and suggestions are welcome.
    
  Developed under Harbour Compiler and 
  MINIGUI - Harbour Win32 GUI library (HMG);
  compiled and linked by MinGW. 
  
  Thanks to "Le Roy" Roberto Lopez.
 
  All modules in the MsgExtended.prg and MExTest.prg 
  copyrighted : 2008 Bicahi Esgici <esgici@gmail.com>
 
  History :
  
     2008.08 : First Release
*/

#include <minigui.ch>

#define NTrim( n ) ( LTRIM( STR( n ) ) )

#define Emp2Nil( x ) IF( ISCHAR( x ) .AND. EMPTY( ALLTRIM( x ) ), , x )
#define c1Tab CHR(9)

#translate ISNIL(  <xVal> ) => ( <xVal> == NIL )
#translate ISARRY( <xVal> ) => ( VALTYPE( <xVal> ) == "A" )
#translate ISCHAR( <xVal> ) => ( VALTYPE( <xVal> ) == "C" )
#translate ISNUMB( <xVal> ) => ( VALTYPE( <xVal> ) == "N" )

#xcommand DEFAULT <v1> TO <x1> [, <vn> TO <xn> ]                        ;
          =>                                                            ;
          IF <v1> == NIL ; <v1> := <x1> ; END                           ;
          [; IF <vn> == NIL ; <vn> := <xn> ; END ]
          

/*

   f.MsgExtended() : Message Extended 
   
   Author  : Bicahi Esgici

   Syntax  : MsgExtended( [ <acMsgLines> ],;                          
                          [ <xTitle> ],;
                          [ <acOptions> ],;
                          [ <anBackColor> ],;
                          [ <cFontNameM> ],;
                          [ <nFontSizeM> ],;
                          [ <anFontColorM> ],;
                          [ <cFontNameO> ],;
                          [ <nFontSizeO> ],;
                          [ <xPosition ] )       => <nResult>


   Parameters :

             All parameters are optional.

             <acMsgLines>   : Message Line(s); Char for single, array for multiple lines; default is ""
             <xTitle>       : Tittle for Message box; char; default is ""
             <acOptions>    : Options; Char for single, array for multiple options; default is ""
             <anBackColor>  : Back Color; num array; default is windows default
             <cFontNameM>   : Font Name for message line(s); char; default is windows default
             <nFontSizeM>   : Font Size for message line(s); numeric; default is windows default
             <anFontColorM> : Font Color message line(s);  num array; default is windows default
             <cFontNameO>   : Font Name for option(s); default is windows default
             <nFontSizeO>   : Font Size for option(s); default is windows default
             <aPosition>    : Position relative to the window / form

                              NIL/EMPTY : will be defaulted to : { 0, 0 }

                              Numeric : will be defaulted to : { <this number>, 0 }

                              Array :  may be included 1 to 3 elements :

                                  1° : relativity base: 1: Desktop, 0: parent window/form;  default : 0

                                  2° : If element count is 2, Position code:

                                          0 : Center
                                          1: Up left
                                          2: Up cent
                                          3: Up right
                                          4: Center left
                                          5: Center right
                                          6: Down left
                                          7: Down center
                                          8: Down right

                                          default : 0 ( Center )

                                        else, If element count is 3, this number will be treated as row number
                                              for upper left corner of message box AND

                                  3° Column number for upper left corner of message box

             All parameters may be given as strings (character type). In this case arrays elements may 
             be separated by CRLF or double semicolon (';;') for strings arrays and by comma (',') for
             numeric arrays. Expressions must be specified in their exact form.
             
             
   Return : <nResult> : Number of option selected;
                        When message box closed without selection ( Esc / Alt-F4 ), zero.

   History :
             2008.07 : First Release

*/

FUNC MsgExtended( ;                          // Message Extended
               acMsgLines,;  // Message Line(s)
               xTitle,;      // Tittle for Message box
               xOptions,;    // Options
               aBackColor,;  // Back Color
               cFontNameM,;  // Msg. Font Name
               nFontSizeM,;  // Msg. Font Size
               aFontColorM,; // Msg. Font Color
               cFontNameO,;  // Opt. Font Name
               nFontSizeO,;  // Opt. Font Size
               xPosition  )  // Position relative to the window / form

   LOCA aMsgLines := {},;   // Message Lines
        aOptions  := {},;   // Options
        nRVal     :=  0,;
        nLinNo    :=  0,;
        c1Line    := '',;
        nOpts     :=  0,;
        c1Optn    := '',;
        nMxLnLn   :=  0,;  // Max Line Len
        nOptnCo   :=  0,;  // Option Count
        cLblNam   := '',;  // Label Name
        cbtnNam   := '',;  // Button Name
        nButRow   :=  0,;
        nLnLnPx   :=  0    // Message Line Length in pixel

   LOCA nRelBase  :=  0,;
        nMsBxRow  :=  0,;
        nMsBxCol  :=  0,;
        nPosCode  :=  0,;
        cPosCode  := '',;
        nBaseRow  :=  0,;
        nBaseCol  :=  0,;
        nBasHeig  :=  0,;
        nBasWidt  :=  0
        
   LOCA nMLineCo  :=  0,;         // Messages line count
        nMBxHeig  :=  0,;
        nMBxWidt  :=  0,;
        nMxBtnLn  :=  0,;         // Max Button length (in pixel )
        nBtn1Len  :=  0,;
        nBtnTotL  :=  5
        
   LOCA nMFontSiz :=  0,;
        nOFontSiz :=  0,;
        lVertOpts := .F.        

   * * * * * * * * * * * * * * * * * * * * * * * * *
   *
   *  Defaulting parameters      
   *
   * * * * * * * * * * * * * * * * * * * * * * * * *

   acMsgLines  := Emp2Nil( acMsgLines )
   xTitle      := Emp2Nil( xTitle )    
   xOptions    := Emp2Nil( xOptions )   
   aBackColor  := Emp2Nil( aBackColor ) 
   cFontNameM  := Emp2Nil( cFontNameM )
   nFontSizeM  := Emp2Nil( nFontSizeM )
   aFontColorM := Emp2Nil( aFontColorM )
   cFontNameO  := Emp2Nil( cFontNameO )
   nFontSizeO  := Emp2Nil( nFontSizeO )
   xPosition   := Emp2Nil( xPosition ) 

   DEFAULT acMsgLines  TO  {""},;
           xTitle      TO  Any2Strg( xTitle ),;
           xOptions    TO  {" Ok "},;              // aBackColor  TO  { 201, 215, 228 },;  // 
           xPosition   TO  { 0, 0 }

   IF ISCHAR( aBackColor )
      aBackColor := ParsArStr( aBackColor , { CRLF, ',' } ) 
      IF LEN( aBackColor ) # 3
        aBackColor := NIL
      ELSE   
        AEVAL( aBackColor, { | c1, i1 | aBackColor[ i1 ] := VAL( c1 ) } )
      ENDIF   
   ENDIF
   
   IF ISCHAR( aFontColorM )
      aFontColorM := ParsArStr( aFontColorM , { CRLF, ',' } ) 
      IF LEN( aFontColorM ) # 3
         aFontColorM := NIL
      ELSE   
         AEVAL( aFontColorM, { | c1, i1 | aFontColorM[ i1 ] := VAL( c1 ) } )
      ENDIF   
   ENDIF
   
   IF ISCHAR( nFontSizeM )
      nFontSizeM := VAL( nFontSizeM )
   ENDIF
      
   IF ISCHAR( nFontSizeO )
      nFontSizeO := VAL( nFontSizeO )
   ENDIF

   IF ISNIL( cFontNameM ) .AND. !ISNIL( nFontSizeM )
      cFontNameM := "Arial"
   ENDIF
      
   IF ISNIL( cFontNameO ) .AND. !ISNIL( nFontSizeO )
      cFontNameO := "Arial"
   ENDIF
   
   nMFontSiz := MAX( MIN( IF( ISNIL( nFontSizeM ), 9, nFontSizeM ), 32 ), 8 )      
   nOFontSiz := MAX( MIN( IF( ISNIL( nFontSizeO ), 9, nFontSizeO ), 32 ), 8 )
   
   IF !ISNIL( nFontSizeM )     
      nFontSizeM := nMFontSiz 
   ENDIF   
   
   IF !ISNIL( nFontSizeO )     
      nFontSizeO := nOFontSiz 
   ENDIF   
   
   aMsgLines := ParsArStr( acMsgLines, { CRLF, ";;" } )                  // Message lines
   
   nMLineCo := LEN( aMsgLines )

   AEVAL( aMsgLines, { | c1 | nMxLnLn := MAX( nMxLnLn, LEN( c1 ) ) } )   // Max Line Legth of Messages lines

   nLnLnPx :=  nMxLnLn * nMFontSiz * .8                                  // Messages line length by pixel
   
   * * * * * * * * * * * * * * * * * * * * * * * * *
   *
   * Buttons ( Options )
   *
   * * * * * * * * * * * * * * * * * * * * * * * * *
   
   aOptions := ParsArStr( xOptions, { CRLF, ";;" } )                // Options

   nOptnCo :=  LEN( aOptions )                                      // Options (buttons) Count
   
   lVertOpts := ( EMPTY( aMsgLines ) .AND. nOptnCo > 1 )
   
   AEVAL( aOptions,  { | c1 |  nBtn1Len := nOFontSiz * LEN( c1 ) * .8 + 10,;
                               nMxBtnLn := MAX( nMxBtnLn, nBtn1Len ),; 
                               nBtnTotL += nBtn1Len } )
                               

   
   * * * * * * * * * * * * * * * * * * * * * * * * *
   *
   * Form metrics
   *
   * * * * * * * * * * * * * * * * * * * * * * * * *
      
   IF lVertOpts 
      nMBxHeig := 40 + ( nOptnCo * 4 * nOFontSiz ) 
      nMBxWidt := MAX( nMxBtnLn, 123 ) + 100 // !!! 123 : Min Window WIDTH !!!
   ELSE   
      nMBxHeig := 40 + ( nMLineCo * 4 * nMFontSiz ) + ( nOFontSiz * 6 )
      nMBxWidt := MAX( MAX( nBtnTotL, nLnLnPx ), 123 ) + 10 // !!! 123 : Min Window WIDTH !!!
   ENDIF lVertOpts 
   
   * * * * * * * * * * * * * * * * * * * * * * * * *
   *
   * Position of Message Box
   *
   * * * * * * * * * * * * * * * * * * * * * * * * *

   IF EMPTY( xPosition )
      xPosition := { 0, 0 }
   ENDIF
   
   IF ISCHAR( xPosition )
      xPosition := ParsArStr( xPosition, { CRLF, ',' } ) 
   ENDIF
   
   IF ISARRY( xPosition )
      AEVAL( xPosition, { | x1, i1 | xPosition[ i1 ] := IF( ISCHAR( x1 ), VAL( x1 ), 0 ) } )
   ELSE
      IF ISNUMB( xPosition )
         xPosition := { xPosition, 0 }
      ELSE   
         xPosition := { 0, 0 }
      ENDIF   
   ENDIF

   nRelBase := xPosition[ 1 ]
   
   IF nRelBase < 1     // Parent ( Caller of f.MsgExtended() form/window
      nBaseRow :=  ThisWindow.Row
      nBaseCol :=  ThisWindow.Col
      nBasHeig :=  ThisWindow.Height 
      nBasWidt :=  ThisWindow.Width
   ELSE                // Desktop 
      nBasHeig :=  GetDesktopHeight() 
      nBasWidt :=  GetDesktopWidth() 
   ENDIF nRelBase < 1      
   

   IF LEN( xPosition ) > 2
      nMsBxRow := nBaseRow + xPosition[ 2 ]
      nMsBxCol := nBaseCol + xPosition[ 3 ]
   ELSE
   
      nPosCode := xPosition[ 2 ]
      cPosCode := NTrim( nPosCode )
      
      IF cPosCode $ "123"             // Up line
         nMsBxRow :=  nBaseRow + 50
      ELSEIF cPosCode $ "678"         // Down lin
         nMsBxRow :=  nBasHeig - nMBxHeig - 10 + nBaseRow 
      ELSE                            // Center Line
         nMsBxRow :=  ( nBasHeig - nMBxHeig ) / 2 + nBaseRow 
      ENDIF

      IF cPosCode $ "146"             // Left Column
         nMsBxCol :=  nBaseCol + 10
      ELSEIF cPosCode $ "358"         // Rigth Column
         nMsBxCol :=  nBaseCol + nBasWidt / 3 * 2
      ELSE                            // Center Column
         nMsBxCol :=  ( nBasWidt - nMBxWidt ) / 2 + nBaseCol
      ENDIF

   ENDIF LEN( xPosition ) > 2

   DEFINE WINDOW frmMsgExtended ;
      AT     nMsBxRow, nMsBxCol ;
      WIDTH  nMBxWidt ;
      HEIGHT nMBxHeig ;
      TITLE  xTitle ;
      MODAL  ; //      NOSIZE ;
      NOSYSMENU ;
      BACKCOLOR aBackColor  // { 201, 215, 228 } { 24, 240, 223 }
      
      ON KEY ESCAPE ACTION frmMsgExtended.Release
      
      FOR nLinNo := 1 TO nMLineCo
         cLblNam := 'lbl_' + STRZERO( nLinNo, 2 )
         c1Line  := aMsgLines[ nLinNo ]
         DEFINE LABEL &cLblNam
            ROW       (nLinNo * 3 - 1 ) * nMFontSiz
            COL       0
            VALUE     c1Line
            WIDTH     nMBxWidt
            HEIGHT    nMFontSiz * 2
            FONTNAME  cFontNameM
            FONTSIZE  nFontSizeM
            BACKCOLOR aBackColor
            FONTCOLOR aFontColorM
            CENTERALIGN  .T.
         END LABEL
         
      NEXT nLinNo
   
      IF lVertOpts 
         nButRow  := nOFontSiz * 2
      ELSE
         nButRow := nMBxHeig - 40 - nOFontSiz * 3
      ENDIF lVertOpts 
      
      nBtnSpac := INT( ( nMBxWidt - nBtnTotL ) / ( nOptnCo + 1 ) )
      n1BtnCol := nBtnSpac
      
      FOR nOpts := 1 TO nOptnCo
      
         c1Optn   := aOptions[ nOpts ]
         cbtnNam  := 'btn_'  + STRZERO( nOpts, 2 )
         nBtnWidt := INT( nOFontSiz * ( LEN( c1Optn ) ) * .8 + 10 )         
         
         IF lVertOpts 
            n1BtnCol := ( nMBxWidt - nBtnWidt ) / 2
         ENDIF lVertOpts 
         
         DEFINE BUTTON &cbtnNam
            ROW         nButRow
            COL         n1BtnCol 
            CAPTION     c1Optn
            ACTION      { || nRVal := VAL( RIGHT( this.name, 2 ) ),  frmMsgExtended.Release }
            WIDTH       nBtnWidt        
            HEIGHT      nOFontSiz * 2 + 4
            FONTNAME    cFontNameO
            FONTSIZE    nFontSizeO
            CENTERALIGN .T.
         END BUTTON
         
         IF lVertOpts
            nButRow += nOFontSiz * 3
         ELSE          
            n1BtnCol += nBtnWidt + nBtnSpac
         ENDIF lVertOpts 
      
      NEXT nOpts

   END WINDOW // frmMsgExtended

   ACTIVATE WINDOW frmMsgExtended
   
   
RETU nRVal // MsgExtended()


*-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.


/*

    f.Any2Strg() : Covert any type data to string

    Syntax       : Any2Strg( <xAny> ) -> <cString>

    Argument     : <xAny> : A value in any data type

    Return       : <cString> : String equivalent of <xAny>

    History      :

                   7.2006 : First Release

*/

FUNC Any2Strg( xAny )

   LOCA cRVal  := '???',;
        nType  :=  0,;
        aCases := { { "A", { | x | "{...}" } },;
                    { "B", { | x | "{||}" } },;
                    { "C", { | x | x }},;
                    { "M", { | x | x   } },;
                    { "D", { | x | DTOC( x ) } },;
                    { "L", { | x | IF( x,"True","False") } },;
                    { "N", { | x | NTrim( x )  } },;
                    { "O", { | x | ":Object:" } },;
                    { "U", { | x | "" } } }

   IF (nType := ASCAN( aCases, { | a1 | VALTYPE( xAny ) == a1[ 1 ] } ) ) > 0
      cRVal := EVAL( aCases[ nType, 2 ], xAny )
   ENDIF
   
RETU cRVal // Any2Strg()

*-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._
/*

   f.ParsArStr() is a sub-function of f.MsgExtended().
   
   Author  : Bicahi Esgici
   
   Purpose : Pars Lines of an array
   
   Syntax  : ParsArStr( <aArray>, <aDelim> ) => <aParsed>
   
   Parameters : <aArray> : Array to parse 
                <aDelim> : Delimiter(s)
                
   Return : <aParsed> : Parsed verison of <aArray>
   
   History :
   
       2008.08 : First Release
   
*/


FUNC ParsArStr(;                                    // Pars Lines of an array
                aArray, aDelim )

   LOCA nDelm  :=  0,;
        cDelm  := '',;
        nLiNo  :=  0,;
        c1Lin  := '',;
        aTemp  := {},;
        a1Arr  := {},;
        nPosNo :=  0,;
        aRVal  := {}

   DEFAULT aDelim TO { CRLF }
   
   IF !ISARRY( aArray )
      aArray := { Any2Strg( aArray ) }
   ENDIF   
   
   IF !ISARRY( aDelim )
      aDelim := { Any2Strg( aDelim ) }
   ENDIF   

   FOR nDelm := 1 TO LEN( aDelim )
      cDelm := aDelim[ nDelm ]
      FOR nLiNo := 1 TO LEN( aArray )
         c1Lin  := aArray[ nLiNo ]
         a1Arr  := {}
         IF ISCHAR( c1Lin ) .AND. !EMPTY( c1Lin )
            WHILE ( nPosNo := AT( cDelm, c1Lin ) ) > 0
               AADD( a1Arr, LEFT( c1Lin, nPosNo - 1 ) )
               c1Lin := SUBS( c1Lin, nPosNo + LEN( cDelm ) )
            ENDDO
            IF !EMPTY( c1Lin  )
               AADD( a1Arr, c1Lin )
            ENDIF
         ENDIF ISCHAR( c1Lin ) .AND. !EMPTY( c1Lin )
         AEVAL( a1Arr, { | c1 | AADD( aTemp, c1 ) } )
      NEXT nLinNo
      aRVal := ACLONE( aTemp )
      aTemp := {}
   NEXT nDelm

RETU aRVal // ParsArStr()

*-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.



