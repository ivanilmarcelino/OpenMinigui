#include <hmg.ch>

#define NTrim( n ) ( LTRIM( STR( n ) ) )

#define Emp2Nil( x ) IF( ISCHAR( x ) .AND. EMPTY( ALLTRIM( x ) ), , x )

#translate ISNIL(  <xVal> ) => ( <xVal> == NIL )
#translate ISARRY( <xVal> ) => ( VALTYPE( <xVal> ) == "A" )
#translate ISCHAR( <xVal> ) => ( VALTYPE( <xVal> ) == "C" )
#translate ISNUMB( <xVal> ) => ( VALTYPE( <xVal> ) == "N" )

#xcommand DEFAULT <v1> TO <x1> [, <vn> TO <xn> ]                        ;
          =>                                                            ;
          IF <v1> == NIL ; <v1> := <x1> ; END                           ;
          [; IF <vn> == NIL ; <vn> := <xn> ; END ]
          


*-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.

FUNCTION HL_StripRZero( ;                 // Clean zero ended string
                        cZEString )   // zero ended string
                        
   LOCAL cRetVal := cZEString

   IF CHR(0) $ cRetVal
      cRetVal := LEFT( cRetVal, AT( CHR( 0 ), cRetVal ) - 1 )
   ENDIF   
   
RETURN cRetVal // HL_StripRZero(

*-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.

FUNCTION HL__STRG2HEX( c1 )                    // Convert string to hex
RETURN ( HL__STRGEVAL( c1, '',, { | c1, i1, cTrg | cTrg += NTOC( ASC( c1 ), 16, 2, "0" ) } ) )

*-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._

FUNCTION HL__ISINCCTRL( ;                       // Is this string include any control character ( ASC < 32 ) ?
                       cString )
   LOCAL lRVal := .F.,;
         nCPos := 0
         
   IF HB_ISCHAR( cString )
      FOR nCPos := 1 TO LEN( cString )
         IF ASC( SUBSTR( cString, nCPos, 1 ) ) < 32
            lRVal := .T.
            EXIT
         ENDIF    
      NEXT nCPos 
   ENDIF
   
RETURN lRVal // HL__ISINCCTRL()

*.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._
                       
FUNCTION HL__STRGEVAL(;                              // String Evaluation (AEVAL'ýn Katar'casý) (eski adý SEVAL idi)
                     cSource,;  // Source (String)
                     xRVal ,;   // Return Value ( any type )
                     nStep ,;
                     bBlock )   // Code Block to evaluate

   LOCAL nSInd  :=  0,;
        cSubS := ''        // sub string 

   DEFAULT nStep TO 1
   
   FOR nSInd := 1 TO LEN( cSource ) STEP nStep
      cSubS := SUBSTR( cSource, nSInd, nStep )
      EVAL( bBlock, cSubS, nSInd, @xRVal )
   NEXT nSInd

RETURN xRVal // HL__STRGEVAL()

*-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.

FUNCTION HL_Any2Str( ;                       // Convert any type data to String.
                  xAny )

   LOCAL cRVal  := '???',;
         nType  :=  0,;
         aCases := { { "A", { | x | "{...}" } },;                
                     { "B", { | x | "{||}" } },;                
                     { "C", { | x | IF( LEN( x ) < 3 .AND. HL__ISINCCTRL( x ), "0x" + HL__STRG2HEX( x ), x ) }},;
                     { "H", { | x | "<Hash>" } },;
                     { "M", { | x | x   } },;                   
                     { "D", { | x | DTOC( x ) } },;             
                     { "L", { | x | IF( x,".T.",".F.") } },;    
                     { "N", { | x | LTRIM( TRAN( x, IF( INT( x ) # x,;
                                                    "999,999,999,999,999,999.999",;
                                                    IF( LEN( LTRIM( STR( x ) ) ) > 4,;                                                      
                                                    "999,999,999,999,999,999,999",;
                                                    "9999" ) ) ) ) } },;
                     { "O", { | | ":Object:" } },;
                     { "U", { | | "<NIL>" } } }
                    
   IF (nType := ASCAN( aCases, { | a1 | VALTYPE( xAny ) == a1[ 1 ] } ) ) > 0
      cRVal := EVAL( aCases[ nType, 2 ], xAny )
   ENDIF    
                   
RETURN cRVal // HL_Any2Str()

*-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.

FUNCTION HL_OsVers()

   LOCAL aWinVers := WindowsVersion() 
   
   LOCAL cRetVal := TRIM( aWinVers[ 1 ] ) + ", " + aWinVers[ 3 ]
   
RETURN cRetVal // HL_OsVers()  

*-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.

FUNCTION HL_CallSequence( nBegLevel )

   LOCA nLevel  := nBegLevel,;
        cRetVal := ''

   WHILE !(PROCNAME( nLevel ) == "")
      cRetVal += IF( nLevel > nBegLevel, SPACE( 23 ), '' ) + ;
                 PROCNAME( nLevel ) + ;
                 " (" + HL_Any2Str( PROCLINE( nLevel ) ) + ")" + CRLF
      nLevel++                    
   ENDDO

RETURN cRetVal // HL_CallSequence()

*-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.

/*

   f.HL_ParsArStr() is a sub-function of f.HL_MsgExt().
   
   Author  : Bicahi Esgici
   
   Purpose : Pars Lines of an array
   
   Syntax  : HL_ParsArStr( <aArray>, <aDelim> ) => <aParsed>
   
   Parameters : <aArray> : Array to parse 
                <aDelim> : Delimiter(s)
                
   Return : <aParsed> : Parsed verison of <aArray>
   
   History :
   
       2008.08 : First Release
   
*/


FUNC HL_ParsArStr(;                                    // Pars Lines of an array
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
      aArray := { HL_Any2Str( aArray ) }
   ENDIF   
   
   IF !ISARRY( aDelim )
      aDelim := { HL_Any2Str( aDelim ) }
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

RETU aRVal // HL_ParsArStr()

*-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.

FUNCTION HL_MemVarList(;                     // Build an array with variables recorded as a .mem file
                     cMemFNam )
                     
   LOCAL aRetVal  := {},;
         cMFilStr := '',;
         c1VHeadr := '',;
         c1VName  := '',;
         c1VType  := '',;
         c1VData  := '',;
         x1VValu  := '',;
         n1VLeng  := 0,;
         n1VDeci  := 0,;
         n1VWidt  := 0,;
         nFilPntr := 1,;
         nMFStrLn := 0,;
         nActLen  := 0   // Actual Length ( regarding value )
   
   cMFilStr := MEMOREAD( cMemFNam )
   
   nMFStrLn := LEN ( cMFilStr )
   
   WHILE nFilPntr < nMFStrLn 
    
      c1VHeadr := SUBSTR( cMFilStr, nFilPntr, 20 )
      c1VName  := HL_StripRZero( LEFT( c1VHeadr, 11 ) )
      c1VType  := CHR( ASC(SUBSTR( c1VHeadr, 12, 1 ) ) - 128 )      
      n1VDeci  := ASC( SUBSTR( c1VHeadr, 18, 1 ) )
      
      nFilPntr += 32      
      
      DO CASE 
         CASE c1VType = "L"
              n1VWidt := 1
              c1VData := SUBSTR( cMFilStr, nFilPntr, n1VWidt )
              x1VValu := ASC( c1VData ) > 0
              nActLen := 1
         CASE c1VType = "D"
              n1VWidt := 8
              c1VData := SUBSTR( cMFilStr, nFilPntr, n1VWidt )
              x1VValu := ( CTOF( c1VData ) - 2415021 ) + CTOD( "01/01/1900" ) 
              nActLen := 8
         CASE c1VType = "N"
              n1VWidt := 8
              c1VData := SUBSTR( cMFilStr, nFilPntr, n1VWidt )
              x1VValu := HL_Any2Str( CTOF( c1VData ) )
              nActLen := LEN( x1VValu )              
         OTHERWISE // "C"  
              n1VLeng  := ASC( SUBSTR( c1VHeadr, 17, 1 ) )
              n1VWidt  := n1VDeci * 256 + n1VLeng - 1
              c1VData := SUBSTR( cMFilStr, nFilPntr, n1VWidt )                        
              x1VValu := HL_StripRZero( c1VData )             
              nActLen := LEN( x1VValu )                            
      ENDCASE
      
      AADD( aRetVal, { c1VName,;    // Variable name
                       c1VType,;    // Variable type 
                       nActLen,;    // Actual lenght of variable
                       n1VDeci,;    // Decimal digit count
                       x1VValu } )  // Recorded value of variable
      
      nFilPntr += n1VWidt + IF( c1VType = "C", 1, 0 )
      
   ENDDO   
   
RETURN aRetVal // HL_MemVarList()

*-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.