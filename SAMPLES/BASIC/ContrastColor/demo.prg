/*
   Defining a contrasting text color for a form

   Adapted for MiniGUI by Grigory Filatov
*/

#include "minigui.ch"

FUNCTION Main()

   LOCAL aDlg[ 30 ], oFont, nClrText, nClrBack, n
   LOCAL nTop := -100
   LOCAL nLeft := 1200

   SET WINDOW MAIN OFF

   DEFINE FONT oFont FONTNAME "VERDANA" SIZE 16 DEFAULT

   FOR n := 1 TO 30

      nClrBack := hb_RandomInt( 0, CLR_WHITE )
      nClrText := ContrastClr( nClrBack )

      aDlg[ n ] := HMG_GetUniqueName( "Form_" )

      DEFINE WINDOW ( aDlg[ n ] ) ;
         AT -200, -200 ;
         CLIENTAREA 150, 100 ;
         TITLE aDlg[ n ] ;
         NOMINIMIZE NOMAXIMIZE ;
         BACKCOLOR nClrBack ;
         ON INIT ( SetProperty( this.NAME, "Row", nTop ), SetProperty( this.NAME, "Col", nLeft ) )

      @ 40, 20 LABEL l1 VALUE "ANY TEXT" AUTOSIZE OF aDlg[ n ] CENTERALIGN VCENTERALIGN ;
         FONTCOLOR nClrText BACKCOLOR nClrBack

      IF nLeft >= 1100
         nTop  += 140
         nLeft := 100
      ELSE
         nLeft += 200
      ENDIF

      END WINDOW

      IF n < 30
         ACTIVATE WINDOW ( aDlg[ n ] ) NOWAIT
      ELSE
         ON KEY ESCAPE OF ( aDlg[ 30 ] ) ACTION DoMethod( aDlg[ 30 ], "Release" )
      ENDIF

   NEXT

   ACTIVATE WINDOW ( aDlg[ 30 ] )

RETURN NIL


FUNCTION ContrastClr( nClr )

   LOCAL nLuma

   IF HB_ISNUMERIC( nClr ) .AND. nClr >= 0 .AND. nClr <= 0x00ffffff
      nLuma := ( 0.299 * GetRed( nClr ) + 0.587 * GetGreen( nClr ) + 0.114 * GetBlue( nClr ) )
   ELSE
      RETURN CLR_WHITE
   ENDIF

RETURN iif( nLuma < 128, CLR_WHITE, CLR_BLACK )
