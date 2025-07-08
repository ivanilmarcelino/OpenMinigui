/*******************************************************************************
    Filename        : demo2.prg

    Created         : 09 July 2018 (23:46:12)
    Created by      : Pierpaolo Martinello

    Last Updated    : 16 July 2018 (22:44:42)
    Updated by      : Pierpaolo

    Comments        : With this program you can easily manage your reports without having to recompile
*******************************************************************************/

#include "minigui.ch"
#include "hbhrb.ch"
#include "i_rptgen.ch"

#define I_RDATA  "_HMG_SYSDATA[450]"

SET PROCEDURE TO h_rptgen

MEMVAR Drv, Htitle, i, aRPaper, aRpt
MEMVAR aRows

PROCEDURE Main

   PRIVATE aRPaper := { ;
        "PRINTER_PAPER_LETTER"      ;
      , "PRINTER_PAPER_LETTERSMALL" ;
      , "PRINTER_PAPER_TABLOID"     ;
      , "PRINTER_PAPER_LEDGER"      ;
      , "PRINTER_PAPER_LEGAL"       ;
      , "PRINTER_PAPER_STATEMENT"   ;
      , "PRINTER_PAPER_EXECUTIVE"   ;
      , "PRINTER_PAPER_A3"          ;
      , "PRINTER_PAPER_A4"          ;
      , "PRINTER_PAPER_A4SMALL"     ;
      , "PRINTER_PAPER_A5"          ;
      , "PRINTER_PAPER_B4"          ;
      , "PRINTER_PAPER_B5"          ;
      , "PRINTER_PAPER_FOLIO"       ;
      , "PRINTER_PAPER_QUARTO"      ;
      , "PRINTER_PAPER_10X14"       ;
      , "PRINTER_PAPER_11X17"       ;
      , "PRINTER_PAPER_NOTE" }

   PUBLIC Drv := "M", Htitle := ''

   AAdd( _HMG_RPTDATA, "" )
   AAdd( _HMG_RPTDATA, "" )
   AAdd( _HMG_RPTDATA, "" )

   PUBLIC i
   PUBLIC aRows[ 20 ][ 3 ]
   aRows[ 1 ]    := { 'Simpson', 'Homer', '555-5555' }
   aRows[ 2 ]    := { 'Mulder', 'Fox', '324-6432' }
   aRows[ 3 ]    := { 'Smart', 'Max', '432-5892' }
   aRows[ 4 ]    := { 'Grillo', 'Pepe', '894-2332' }
   aRows[ 5 ]    := { 'Kirk', 'James', '346-9873' }
   aRows[ 6 ]    := { 'Barriga', 'Carlos', '394-9654' }
   aRows[ 7 ]    := { 'Flanders', 'Ned', '435-3211' }
   aRows[ 8 ]    := { 'Smith', 'John', '123-1234' }
   aRows[ 9 ]    := { 'Pedemonti', 'Flavio', '000-0000' }
   aRows[10 ]    := { 'Gomez', 'Juan', '583-4832' }
   aRows[11 ]    := { 'Fernandez', 'Raul', '321-4332' }
   aRows[12 ]    := { 'Borges', 'Javier', '326-9430' }
   aRows[13 ]    := { 'Alvarez', 'Alberto', '543-7898' }
   aRows[14 ]    := { 'Gonzalez', 'Ambo', '437-8473' }
   aRows[15 ]    := { 'Batistuta', 'Gol', '485-2843' }
   aRows[16 ]    := { 'Vinazzi', 'Amigo', '394-5983' }
   aRows[17 ]    := { 'Pedemonti', 'Flavio', '534-7984' }
   aRows[18 ]    := { 'Samarbide', 'Armando', '854-7873' }
   aRows[19 ]    := { 'Pradon', 'Alejandra', '???-????' }
   aRows[20 ]    := { 'Reyes', 'Monica', '432-5836' }

   SET CENTURY ON
   SET DATE FRENCH

   DEFINE WINDOW Win_1 ;
      ROW 0 ;
      COL 0 ;
      WIDTH 600 ;
      HEIGHT 400 ;
      TITLE 'Report Generator: I can print in multimode!' ;
      MAIN

   DEFINE MAIN MENU
      POPUP 'File'
         ITEM '&1 MiniPrint Test'   ACTION Test( "M" )
         ITEM '&2 HbPrinter Test'   ACTION Test( "H" )
         ITEM '&3 Pdf Output Test'  ACTION Test( "P" )
         ITEM '&4 Html Output Test' ACTION Test( "T" )
         ITEM '&5 Rtf Output Test'  ACTION Test( "R" ) NAME RTF // Work in progress...
         SEPARATOR
         ITEM '&6 Array Report by Miniprint'    ACTION Test( "M", 2 )
         ITEM '&7 Array Report by Hbprinter'    ACTION Test( "H", 2 )
         SEPARATOR
         ITEM '&Exit'               ACTION Win_1.Release
      END POPUP
   END MENU

   ON KEY ESCAPE ACTION Win_1.Release
   Win_1.Rtf.Enabled := .F.

   END WINDOW

   Win_1.Center

   Win_1.Activate

   RELEASE aRPaper

RETURN

PROCEDURE Test( arg1, itest )

   LOCAL aPrI, aRpt := {}, kw, rFile
   LOCAL cRfile, aSrc
   DEFAULT itest TO 1

   USE Test

   drv := arg1
   I := 0
   IF itest = 1 .OR. itest = 3
      // get a report File
      cRfile := Getfile ( { { 'All Report Files', '*.Rmg' } }, 'Open Report File', GetCurrentFolder(), .F., .T. )

      IF Empty( CrFile )
         RETURN
      ENDIF

      IF Upper( cFilenoext( cRFile ) ) = "DEMO8"
         msgstop( "Denied action!" + CRLF + CRLF + "With this file use menu option 6 or 7." ) // Need array
         RETURN
      ENDIF

      aPrI := hb_ATokens( MemoRead( cRfile ), CRLF, .T., .F. )   // Read the file
      AEval ( aPrI, {| x, y| ( aPrI[ y ] := StrTran( x, Chr( 11 ), "" ), aPrI[ y ] := AllTrim( x ) ) } ) // Remove tabulation and space
      AEval ( aPrI, {| x, y|  if ( x == "BEGIN GROUP" .OR. x == "END GROUP", aPrI[ y ] := CharRem( " ", x ), "" ) } ) // A little adjust is needed for correct command interpretations
      AEval ( aPri, {| x| AAdd( aRpt, { x, "" } ) } )  // Copy the commands into a bidimensional array

      aSrc := { ;     // a List of available commands
           { "DEFINE REPORT",13,[_DefineReport],"D" } ;
         , { "BEGIN LAYOUT",12,"_BeginLayout","F" } ;
         , { "END LAYOUT",10,"_EndLayout","F" } ;
         , { "BEGIN HEADER",12,"_BeginHeader","F" } ;
         , { "END HEADER",10,"_EndHeader","F" } ;
         , { "BANDHEIGHT",10,"_BandHeight",, "F" } ;
         , { "BEGIN LINE",10,"_BeginLine","F" } ;
         , { "END LINE", 8,"_EndLine","F" } ;
         , { "BEGIN TEXT",10,"_BeginText","F" } ;
         , { "END TEXT", 8,"_EndText","F" } ;
         , { "ROW", 3,I_RDATA + "[331] := ","D" } ;
         , { "COL", 3,I_RDATA + "[332] := ","D" } ;
         , { "WIDTH", 5,I_RDATA + "[320] := ","D" } ;
         , { "HEIGHT", 6,I_RDATA + "[321] := ","D" } ;
         , { "FROMROW", 7,I_RDATA + "[110] := ","D" } ;
         , { "FROMCOL", 7,I_RDATA + "[111] := ","D" } ;
         , { "TOROW", 5,I_RDATA + "[112] := ","D" } ;
         , { "TOCOL", 5,I_RDATA + "[113] := ","D" } ;
         , { "EXPRESSION",10,I_RDATA + "[116] := ","D" } ;
         , { "PAPERSIZE", 9,I_RDATA + "[156] := ","D" } ;
         , { "ORIENTATION",11,I_RDATA + "[155] := ","D" } ;
         , { "PENWIDTH", 8,I_RDATA + "[114] := ","D" } ;
         , { "PENCOLOR", 8,I_RDATA + "[115] := ","D" } ;
         , { "FONTNAME", 8,I_RDATA + "[322] := ","D" } ;
         , { "FONTSIZE", 8,I_RDATA + "[323] := ","D" } ;
         , { "FONTBOLD", 8,I_RDATA + "[312] := ","D" } ;
         , { "STRETCH", 7,I_RDATA + "[311] := ","D" } ;
         , { "ITERATOR", 8,I_RDATA + "[164] := {||", "D" } ;
         , { "STOPPER", 7,I_RDATA + "[165] := {||", "D" } ;
         , { "BEGIN PICTURE",13,"_BeginImage","F" } ;
         , { "END PICTURE",11,"_EndImage","F" } ;
         , { "BEGIN RECTANGLE",15,"_BeginRectangle","F" } ;
         , { "END RECTANGLE",13,"_EndRectangle","F" } ;
         , { "BEGIN DETAIL",12,"_BeginDetail","F" } ;
         , { "END DETAIL",10,"_EndDetail","F" } ;
         , { "BEGIN DATA",10,"_BeginData","F" } ;
         , { "END DATA", 8,"_EndData","F" } ;
         , { "BEGIN FOOTER",12,"_BeginFooter","F" } ;
         , { "END FOOTER",10,"_EndFooter","F" } ;
         , { "BEGIN SUMMARY",13,"_BeginSummary","F" } ;
         , { "END SUMMARY",11,"_EndSummary","F" } ;
         , { "BEGIN GROUPFOOTER",17,"_BeginGroupFooter","F" } ;
         , { "BEGIN GROUPHEADER",17,"_BeginGroupHeader","F" } ;
         , { "END GROUPHEADER",15,"_EndGroupHeader","F" } ;
         , { "BEGINGROUP",10,"_BeginGroup","F" } ;
         , { "END GROUPFOOTER",15,"_EndGroupFooter","F" } ;
         , { "ENDGROUP", 8,"_EndGroup","F" } ;
         , { "BEGIN DATA",10,"_BeginData","F" } ;
         , { "END DATA", 8,"_EndData","F" } ;
         , { "END REPORT",10,"_EndReport","F" } ;
         , { "VALUE", 5,I_RDATA + "[334] := ","D" } ;
         , { "GROUPEXPRESSION",15,I_RDATA + "[125] := ","D" } ;
         }

      rFile := cFilenoext( cRFile )

      FOR kw = 1 TO Len( aRpt )
         AEval( aSrc, {| x| if( Left( aRpt[ Kw, 1 ], X[ 2 ] ) == x[ 1 ], Sep_Value( aRpt[ Kw, 1 ], x, rfile ), '' ) } )
      NEXT

   ELSE
      LOAD REPORT DEMO8
   ENDIF

   DO CASE

   CASE drv = "P"
      Htitle := 'Report Header using Pdf driver'
      IF File( "demo2.pdf" )
         FErase( "demo2.pdf" )
      ENDIF
      ExecuteReport( rfile, .F., .F., 'demo2.pdf' )
      IF File( "demo2.pdf" )
         EXECUTE FILE "demo2.pdf"
      ENDIF

   CASE drv = "T"
      Htitle := 'Report Header using Html driver'
      IF File( "demo2.html" )
         FErase( "demo2.html" )
      ENDIF
      ExecuteReport( rfile, .F., .F., 'demo2.Html' )
      IF File( "demo2.Html" )
         EXECUTE FILE "demo2.html"
      ENDIF

   OTHERWISE
      Htitle := 'Report Header using ' + if( drv = "M", "Miniprint", "Hbprinter" ) + " driver"
      IF itest = 1 .OR. itest = 3
         ExecuteReport( rfile, .T., .T. ) // EXECUTE REPORT RFILE PREVIEW SELECTPRINTER
      ELSE
         i := 1    ;    ExecuteReport( 'demo8', .T., .T. )
      ENDIF

   ENDCASE

   USE

RETURN
/*
*/
*-----------------------------------------------------------------------------*
STATIC FUNCTION ChangeFunc ( arg )
*-----------------------------------------------------------------------------*
   LOCAL argu, npSta
   argu := Upper( arg )
   DO CASE
   CASE At( "ISWINNT", argu ) > 0
      npSta := At( "ISWINNT", argu ) - 1
      arg   := SubStr( arg, 1, npSta ) + [( hb_GetEnv( "OS" ) == "Windows_NT" )] + SubStr( arg, At( ",", arg ) )

   CASE At( "_PAGENO", argu ) > 0
      npSta := At( "_PAGENO", argu ) - 1
      arg   := SubStr( arg, 1, npSta ) + "_HMG_SYSDATA[450] [117]" + SubStr( arg, npSta + 8 )
   ENDCASE

RETURN arg
/*
*/
*-----------------------------------------------------------------------------*
PROCEDURE Sep_value( arg, opt, rFile )
*-----------------------------------------------------------------------------*
   LOCAL act

   DO CASE
   CASE opt[ 1 ] = "PAPERSIZE"
      act := Trim( Str( AScan( aRpaper, AllTrim( SubStr( arg, opt[ 2 ] + 1 ) ) ) ) )

   CASE opt[ 1 ] = "ORIENTATION"
      act := if ( Right( AllTrim( SubStr( arg, opt[ 2 ] + 1 ) ), 8 ) == "PORTRAIT", "1", "2" )

   CASE opt[ 1 ] = "DEFINE REPORT"
      act :=  rFile

   OTHERWISE
      act := AllTrim( SubStr( arg, opt[ 2 ] + 1 ) )
   ENDCASE
   act := AllTrim( act )

   Traduci( Opt[ 1 ], Act )

RETURN
/*
*/
*-----------------------------------------------------------------------------*
PROCEDURE Traduci( arg1, argg ) // this function execute the commands
*-----------------------------------------------------------------------------*
   LOCAL arg2, blse := {| x| if( Val( x ) > 0, .T., if( x = ".T." .OR. x = "ON", .T., .F. ) ) }
   LOCAL nchr, nct := 0, aApex := { ["],['] }, nAp
   DEFAULT argg TO ""

   arg2 := Val( argg )
   argg := ChangeFunc( argg )
   nAp := { AScan( aApex, Left( argg, 1 ) ), AScan( aApex, Right( argg, 1 ) ) }

   DO CASE
   CASE arg1 = "DEFINE REPORT"          ; _DefineReport( argg )

   CASE arg1 = "BANDHEIGHT"             ; _BandHeight( arg2 )
   CASE arg1 = "ROW"                    ; _HMG_SYSDATA[ 331 ] := arg2
   CASE arg1 = "COL"                    ; _HMG_SYSDATA[ 332 ] := Eval( {|| &( argg ) } )
   CASE arg1 = "WIDTH"                  ; _HMG_SYSDATA[ 320 ] := arg2
   CASE arg1 = "HEIGHT"                 ; _HMG_SYSDATA[ 321 ] := arg2
   CASE arg1 = "FONTNAME"               ; _HMG_SYSDATA[ 322 ] := argg
   CASE arg1 = "FONTSIZE"               ; _HMG_SYSDATA[ 323 ] := arg2
   CASE arg1 = "FONTBOLD"               ; _HMG_SYSDATA[ 312 ] := Eval( blse, argg )
   CASE arg1 = "STRETCH"                ; _HMG_SYSDATA[ 311 ] := Eval( blse, argg )
      // Layout ......................................................................
   CASE arg1 = "BEGIN LAYOUT"           ; _BeginLayout()
   CASE arg1 = "ORIENTATION"            ; _HMG_SYSDATA[ 450 ][155 ] := arg2
   CASE arg1 = "PAPERSIZE"              ; _HMG_SYSDATA[ 450 ][156 ] := arg2
   CASE arg1 = "PAPERWIDTH"             ; _HMG_SYSDATA[ 450 ][118 ] := arg2
   CASE arg1 = "PAPERLENGTH"            ; _HMG_SYSDATA[ 450 ][119 ] := arg2
   CASE arg1 = "END LAYOUT"             ; _EndLayout()
      // Header ......................................................................
   CASE arg1 = "BEGIN HEADER"           ; _BeginHeader()
   CASE arg1 = "END HEADER"             ; _EndHeader()
      // Detail ......................................................................
   CASE arg1 = "BEGIN DETAIL"           ; _BeginDetail()
   CASE arg1 = "END DETAIL"             ; _EndDetail()
      // Data ........................................................................
   CASE arg1 = "BEGIN DATA"             ; _BeginData()
   CASE arg1 = "END DATA"               ; _EndData()
      // Footer ......................................................................
   CASE arg1 = "BEGIN FOOTER"           ; _BeginFooter()
   CASE arg1 = "END FOOTER"             ; _EndFooter()
      // Text ........................................................................
   CASE arg1 = "BEGIN TEXT"             ; _BeginText()
   CASE arg1 = "END TEXT"               ; _EndText()
   CASE arg1 = "END GROUP"              ; _EndGroup()
      // Group Header ................................................................
   CASE arg1 = "BEGIN GROUPHEADER"      ; _BeginGroupHeader()
   CASE arg1 = "END GROUPHEADER"        ; _EndGroupHeader()
      // Group Footer ................................................................
   CASE arg1 = "BEGIN GROUPFOOTER"      ; _BeginGroupFooter()
   CASE arg1 = "END GROUPFOOTER"        ; _EndGroupFooter()
      // Rectangle ...................................................................
   CASE arg1 = "BEGIN RECTANGLE"        ; _BeginRectangle()
   CASE arg1 = "END RECTANGLE"          ; _EndRectangle()
      // Line ........................................................................
   CASE arg1 = "BEGIN LINE"             ; _BeginLine()
   CASE arg1 = "END LINE"               ;  _EndLine()
   CASE arg1 = "PENCOLOR"               ; _HMG_SYSDATA[ 450 ][ 115 ] := COLOR ( argg )
      // Image .......................................................................
   CASE arg1 = "BEGIN PICTURE"          ; _BeginImage()
   CASE arg1 = "END PICTURE"            ; _EndImage()
   CASE arg1 = "VALUE"                  ; _HMG_SYSDATA[ 334 ] := if ( nAp[ 1 ] > 0, RemAll( argg, aApex[ nAp[ 1 ] ] ), argg )
   CASE arg1 = "FROMROW"                ; _HMG_SYSDATA[ 450 ][ 110 ] := arg2
   CASE arg1 = "FROMCOL"                ; _HMG_SYSDATA[ 450 ][ 111 ] := arg2
   CASE arg1 = "TOROW"                  ; _HMG_SYSDATA[ 450 ][ 112 ] := arg2
   CASE arg1 = "TOCOL"                  ; _HMG_SYSDATA[ 450 ][ 113 ] := arg2
   CASE arg1 = "PENWIDTH"               ; _HMG_SYSDATA[ 450 ][ 114 ] := arg2
      // Layout ......................................................................
   CASE arg1 = "BEGIN SUMMARY"          ; _BeginSummary()
   CASE arg1 = "END SUMMARY"            ; _EndSummary()
      // Group .......................................................................
   CASE arg1 = "BEGINGROUP"             ;  _BeginGroup()
      // case arg1 = "GROUPEXPRESSION"        ; _HMG_SYSDATA[450] [ 125 ] :=  argg
      // Skip Expression .............................................................
   CASE arg1 = "ITERATOR"               ; _HMG_SYSDATA[ 450 ][164 ] := {|| &argg }
   CASE arg1 = "STOPPER"                ; _HMG_SYSDATA[ 450 ][165 ] := {|| &argg }
      // End of Report ...............................................................
   CASE arg1 = "END REPORT"             ; _EndReport()

   CASE arg1 = "EXPRESSION" .OR. arg1 = "GROUPEXPRESSION"
      nchr := { chrCount( ["],argg), chrCount(['],argg), chrCount("]",argg), chrCount("[",argg), chrCount([{],argg), chrCount([}],argg), chrCount([(],argg), chrCount([)],argg) }
           //                 1                   2                   3                   4                   5                   6                   7                  8
      AEval( nChr, {| x| nct += x } )

      DO CASE
      CASE nct = 0
         _HMG_SYSDATA[ 450 ][ if ( arg1 = "GROUPEXPRESSION", 125, 116 ) ] :=  if ( "->" $ argg,argg, "'" + argg + "'" )

      OTHERWISE
         nAp := { AScan( aApex, Left( argg, 1 ) ), AScan( aApex, Right( argg, 1 ) ) }

         IF ( nAp[ 1 ] = nAp[ 2 ] ) .AND. nAp[ 1 ] > 0
            argg :=  remall( argg, aApex[ nAp[ 1 ] ] )
            _HMG_SYSDATA[ 450 ][ if ( arg1 = "GROUPEXPRESSION", 125, 116 ) ] :=  "[" + argg + "]"
         ELSE
            _HMG_SYSDATA[ 450 ][ if ( arg1 = "GROUPEXPRESSION", 125, 116 ) ] :=  argg
         ENDIF

      ENDCASE


   ENDCASE

RETURN
/*
*/
*-----------------------------------------------------------------------------*
STATIC FUNCTION Color( GR, GR1, GR2 )
*-----------------------------------------------------------------------------*
   LOCAL DATO
   IF PCount () = 1 .AND. ValType( GR ) == "C"
      IF "," $ GR
         gr :=  StrTran( gr, "{", '' )
         gr :=  StrTran( gr, "}", '' )
         tokeninit( GR, "," )
         Dato := { Val( tokENNEXT( GR ) ), Val( tokENNEXT( GR ) ), Val( tokENNEXT( GR ) ) }
      ENDIF
   ELSEIF PCount() = 1 .AND. ValType( GR ) == "A"
      DATO := rgb( GR[ 1 ], GR[ 2 ], GR[ 3 ] )
   ELSEIF PCount() = 3
      DATO := rgb( GR, GR1, GR2 )
   ENDIF

RETURN DATO
/*
*/
#pragma BEGINDUMP

#include "hbapi.h"

HB_FUNC( CHRCOUNT )
{
   if( HB_ISCHAR( 1 ) && HB_ISCHAR( 2 ) )
   {
      const char * s1  = hb_parc( 1 );
      const char * s2  = hb_parc( 2 );
      HB_ISIZ      len = hb_parclen( 2 );
      HB_ISIZ      count, pos2;

      /* loop through s2 matching passed character (s1) with
         each character of s1 */
      for( count = 0, pos2 = 1; pos2 <= len; s2++, pos2++ )
         if( *s1 == *s2 )  /* character matches s1 */
            count++;       /* increment counter */

      hb_retns( count );
   }
   else
      hb_retns( -1 );
}

#pragma ENDDUMP
