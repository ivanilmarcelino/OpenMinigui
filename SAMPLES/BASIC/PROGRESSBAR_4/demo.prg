/*______________________________________________________________________________________________________*/

/* GENERIC FUNCTION  BY AndyGlezL       Mon Sep  3, 2018 10:23 pm */
/* GENERIC FUNCTION  BY AndyGlezL       Fri Feb 22, 2019 16:15 pm */
/* GENERIC FUNCTION  BY AndyGlezL       Wed Mzo  6, 2019 13:29 pm */
/*______________________________________________________________________________________________________*/

#include "hmg.ch"

STATIC i1


FUNCTION Main()

   IF ! hb_osIsWin10()
      MsgStop( 'This Program Runs In Win 10 Only!' )
      QUIT
   ENDIF

   DEFINE WINDOW Form_1 ;
      WIDTH 640 HEIGHT 320 ;
      TITLE "FUNCION GENERICA" ;
      MAIN ;
      NOSIZE NOMAXIMIZE ;
      BACKCOLOR { 216, 191, 216 }

      @ 010, 010 BUTTON BT_1 OF Form_1 CAPTION "Proc. 1" WIDTH 100 HEIGHT 25 ;
         ACTION MiWinSiNo( "REORGANIZA:", WHITE, CRLF + ;
            "Este Procedimiento Reorganiza" + CRLF + ;
            "todos los archivos y se tomará" + CRLF + ;
            "su tiempo." + CRLF + ;
            " " + CRLF + ;
            "Desea Continuar ?" + CRLF, ;
            {|| REORGANIZA() } )

      @ 040, 010 BUTTON BT_2 OF Form_1 CAPTION "Proc. 2" WIDTH 100 HEIGHT 25 ;
         ACTION MiWinSiNo( "REPORTE:", WHITE, CRLF + ;
            "Se Generar  el Reporte en un" + CRLF + ;
            "  archivo con formato Word. " + CRLF + ;
            " " + CRLF + ;
            " " + CRLF + ;
            "Desea Continuar ?" + CRLF, ;
            {|| COTIZA_WORD() } )

      @ 070, 010 BUTTON BT_3 OF Form_1 CAPTION "Proc. 3" WIDTH 100 HEIGHT 25 ;
         ACTION MiWinSiNo( "RESPALDA:", WHITE, CRLF + ;
            "Este Proceso Respaldará TODAS" + CRLF + ;
            "sus Bases de Datos." + CRLF + ;
            " " + CRLF + ;
            " " + CRLF + ;
            "Desea Continuar ?" + CRLF, ;
            {|| RESPALDA() } )

   END WINDOW

   ON KEY ESCAPE OF Form_1 ACTION Form_1.RELEASE

   CENTER WINDOW Form_1
   ACTIVATE WINDOW Form_1

RETURN NIL
/*______________________________________________________________________________________________________*/

/*  FUNCION GENERICA  */
/*______________________________________________________________________________________________________*/

FUNCTION MiWinSiNo( cTit, aClr1, cTex, bBlock )

   LOCAL oPagina

   DEFINE WINDOW sino ;
      WIDTH 400 HEIGHT 150 ;
      MODAL ;
      NOSIZE NOCAPTION NOSYSMENU ;
      BACKCOLOR aClr1

      *****************************************************************************************************
      DEFINE WINDOW ActX AT 050, 320 WIDTH 200 HEIGHT 200 PANEL
         _DefineActivex( 'PagWeb', "ActX", -13, -10, 200, 200, "shell.explorer" )
         oPagina := GetProperty( "ActX", 'PagWeb', 'XObject' )
         oPagina:Navigate( GetStartUpFolder() + "\gif-load6.gif" )
      END WINDOW
      SET WINDOW ActX TRANSPARENT TO COLOR WHITE
      ActX.PagWeb.Hide
      *****************************************************************************************************

      @ 000, 000 IMAGE Image_1 PICTURE 'boton3.jpg' WIDTH 400 HEIGHT 150 STRETCH

      @ 130, 010 PROGRESSBAR PBAR_1 OF sino WIDTH 380 HEIGHT 10 SMOOTH
      sino.PBAR_1.Hide

      @ 015, 015 LABEL LB_Tit VALUE cTit CENTERALIGN VCENTERALIGN WIDTH 280 HEIGHT 25 FONT 'Verdana' SIZE 9 FONTCOLOR aClr1 BOLD TRANSPARENT
      @ 040, 010 LABEL LB_SiNo VALUE cTex CENTERALIGN WIDTH 280 HEIGHT 105 FONT 'Verdana' SIZE 9 FONTCOLOR aClr1 BOLD TRANSPARENT

      @ 040, 300 BUTTON BTN_Si CAPTION "&SI" ACTION WinSiNoAct( bBlock ) WIDTH 80 HEIGHT 25 FONT "Verdana" SIZE 9
      @ 090, 300 BUTTON BTN_No CAPTION "&NO" ACTION sino.RELEASE WIDTH 80 HEIGHT 25 FONT "Verdana" SIZE 9

   END WINDOW

   CENTER WINDOW sino
   ACTIVATE WINDOW sino

RETURN NIL
/*______________________________________________________________________________________________________*/

FUNCTION WinSiNoAct( bBlock )
   sino.PBAR_1.Show
   SET PROGRESSBAR PBAR_1 OF sino ENABLE MARQUEE UPDATED 30

   sino.BTN_Si.Hide
   sino.BTN_No.Hide

   sino.LB_SiNo.FontSize := 14
   sino.LB_SiNo.FontColor := YELLOW
   sino.LB_SiNo.VALUE := "E s p e r e," + CRLF + " P r o c e s a n d o . . ."
   IF hb_osIsWin10()
      ActX.Show
      ActX.PagWeb.Show
   ENDIF

   Eval( bBlock )

   ActX.PagWeb.Hide
   ActX.Hide
   sino.LB_SiNo.VALUE := "P r o c e s o" + CRLF + "T e r m i n a d o !"
   sino.BTN_No.CAPTION := "&Ok"
   sino.BTN_No.Show
   sino.BTN_No.SetFocus

   BT_ClientAreaInvalidateAll( "SiNo", .F. )
   PlayAsterisk()

   SET PROGRESSBAR PBAR_1 OF sino DISABLE MARQUEE
   sino.PBAR_1.Hide

RETURN( Nil )
/*______________________________________________________________________________________________________*/

PROCEDURE REORGANIZA()
   // PROCESO DE REORGANIZAR
   FOR i1 = 1 TO 10000000
      DO EVENTS
   NEXT

RETURN
/*______________________________________________________________________________________________________*/

PROCEDURE COTIZA_WORD()
   // PROCESO GENERA WORD
   FOR i1 = 1 TO 20000000
      DO EVENTS
   NEXT

RETURN
/*______________________________________________________________________________________________________*/

PROCEDURE RESPALDA()
   // PROCESO DE RESPALDAR
   FOR i1 = 1 TO 40000000
      DO EVENTS
   NEXT

RETURN
