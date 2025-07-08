// ------------------------------------------------------------------------
// --> CALENDARI()
// ------------------------------------------------------------------------
// --> VERSION 1.0 Adaptación a HARBOUR
// --> (c) by SARGANTANA SOFT -
// --> email : SargantanaSoft@GMAIL.COM
// --> 2 de Noviembre de 2018
// ------------------------------------------------------------------------
// Esta función la he realizado al darme cuenta que la función MONTHCALENDAR cambia
// de apariencia dependiendo de la versión de WINDOW que estemos utilizando y teniendo
// que ajustar nuestros ".PRG" porque al Billy Puertas le de la gana.
// ------------------------------------------------------------------------


#include "hmg.ch"

// ---------------------------------------------------------
#include "Calendari.ch"

SET PROCEDURE TO Calendari
// ---------------------------------------------------------

// -------------
PROCEDURE Main()
// -------------

   LOCAL dFecha := Date()

   SET CENTURY ON
   SET DATE FORMAT TO 'dd/mm/yyyy'

   SET NAVIGATION EXTENDED
   SET LANGUAGE TO SPANISH
   SET CODEPAGE TO SPANISH

   DEFINE WINDOW Form_1 ;
         AT 0, 0 ;
         WIDTH 645 ;
         HEIGHT 490 ;
         ICON "Calen" ;
         BACKCOLOR { 132, 195, 248 } ;
         NOMAXIMIZE NOSIZE ;
         TITLE "Calendari Idus Martiae" ;
         MAIN

      ON KEY ESCAPE ACTION ThisWindow.RELEASE

      // ---------------------------------------------------------------------------------
      // ---------------------------------------------------------------------------------

      @ 90, 75 CALENDARI PARENT Form_1 ;
         VALUE @dFecha ;
         ONCHANGE Ver_imagen( dFecha ) ;
         VERDIA ;
         COLORNOMES GREEN
      // ETC., ETC
      // ----------------------------------------------------------------------------------
      // ----------------------------------------------------------------------------------

      @ 395, 110 BUTTON EXIT ;
         CAPTION Space( 5 ) - "&Exit" ;
         PICTURE "Resource\exit.bmp" ;
         WIDTH 96 ;
         HEIGHT 34 ;
         ACTION Form_1.RELEASE ;
         TOOLTIP 'Exit' ;
         FONT "ARIAL" SIZE 09 BOLD ITALIC ;
         LEFT

      // ---------------------------------- Foto Estaciones -------------------------------

      DRAW BOX ;
         IN WINDOW Form_1 ;
         AT 055,306 ;
         TO 55+162, 306*2+10

      @ 060, 312 IMAGE Image_1 ;
         PICTURE NIL ;
         WIDTH 306 HEIGHT 152
      // ----------------------------------------------------------------------------------
      // ----------------------------------------------------------------------------------

   END WINDOW

   VER_IMAGEN( dFecha ) // se incorpora para que salga la imagen de la estación del més

   SETWINDOWCURSOR ( Form_1.dhoy.Handle, "Resource\Hand_Cursor.cur" )
   SETWINDOWCURSOR ( Form_1.EXIT.Handle, "Resource\link-select.cur" )

   CENTER WINDOW Form_1
   ACTIVATE WINDOW Form_1

RETURN

// ---------------------------
PROCEDURE VER_IMAGEN( dFecha )
// ---------------------------
   LOCAL Imagen

   DEFAULT dFecha := Date()

   DO CASE
   CASE Month( dFecha ) == 12
      imagen := "Resource\Merry Christmas.jpg"
   CASE Month( dFecha ) = 1 .OR. Month( dFecha ) < 4
      imagen := "Resource\Invierno.jpg"
   CASE Month( dFecha ) > 3 .AND. Month( dFecha ) < 7
      imagen := "Resource\Primavera.jpg "
   CASE Month( dFecha ) > 6 .AND. Month( dFecha ) < 10
      imagen := "Resource\Verano.jpg"
   CASE Month( dFecha ) > 9 .AND. Month( dFecha ) < 13
      imagen := "Resource\Otonyo.jpg"
   ENDCASE

   Form_1.Image_1.PICTURE := Imagen

RETURN
