#include <minigui.ch>
#define CrLf       chr(13)+chr(10)
#define cAcercaDe  "Ejemplo de definición de menú"+CrLf+"en tiempo de ejecución"+CrLf+CrLf+cVersion+CrLf+CrLf+chr(174)+" Abril 2006, Roberto Sánchez"+CrLf+ Chr( 174 ) + " June 2024, Marcos Jarrin" +CrLf+MiniGUIVersion()+CrLf+Version()
#define cFaltaMenu "Falta la tabla MenuDBF.DBF","Falta Archivo"
#define cVersion   "Versión 00.10.00"

STATIC aListItens := {}

FUNCTION Main()

   LOCAL cm_tipo
   LOCAL cm_caption
   LOCAL cm_action
   LOCAL cm_name
   LOCAL cm_image
   LOCAL cm_checked
   LOCAL cm_Disabled
   LOCAL cm_Message
   LOCAL wret := .F.
   LOCAL cFile := "Menu.DBF"
   LOCAL c1, c_dbf, f
   LOCAL cm_Idioma := 2
   LOCAL lescolha := .T.

   LOCAL x // added code

   IF ! lescolha
      // se remover estas duas linhas vai ocorrer o erro ao clicar no menu current folder e get folder
      msginfo( getcurrentfolder() )
      msginfo( getfolder() )
   ENDIF

   SET EXCLUSIVE OFF
   IF File( cfile )
      C1 := RAt( ".", CFILE )
      C_DBF := Left( CFILE, C1 - 1 )

      USE &C_DBF NEW
      // INDEX ON descend(FIELD->LINHA) TO &C_DBF                                  // Erased line

      x := 1 // added code
      DO while ! Eof()
         AAdd( aListItens, { "", "", "", "", "", "", "", "", "", "", .F., .F. } )
         aListItens[ x++ ] := { ( Alias() )->TIPO, ;  // 1
         ( Alias() )->CAPTIONP, ; // 2
         ( Alias() )->CAPTIONE, ; // 3
         ( Alias() )->CAPTIONI, ; // 4
         ( Alias() )->NOME, ;     // 5
         ( Alias() )->ACTION, ;   // 6
         ( Alias() )->IMAGE, ;    // 7
         ( Alias() )->MESSAGEP, ; // 8
         ( Alias() )->MESSAGEE, ; // 9
         ( Alias() )->MESSAGEI, ; // 10
         ( Alias() )->CHECKED, ;  // 11
         ( Alias() )->DISABLED }  // 12
         SKIP
      ENDDO
      USE

      DEFINE WINDOW WinMain AT 0, 0 WIDTH 800 HEIGHT 600 TITLE "Menu DBF en tiempo de ejecución" MAIN

         DEFINE MAIN MENU
            FOR f := 1 TO Len( aListItens )
               cm_tipo := aListItens[ f ][ 1 ]
               cm_caption := AllTrim( aListItens[ f ][ 2 + cm_Idioma ] )
               cm_name := AllTrim( aListItens[ f ][ 5 ] )
               cm_action := iif( EMPT( AllTrim( aListItens[ f ][ 6 ] ) ), NIL, AllTrim( aListItens[ f ][ 6 ] ) )
               cm_image := iif( EMPT( AllTrim( aListItens[ f ][ 7 ] ) ), NIL, AllTrim( aListItens[ f ][ 7 ] ) )
               cm_Message := iif( EMPT( AllTrim( aListItens[ f ][ 8 + cm_Idioma ] ) ), NIL, AllTrim( aListItens[ f ][ 8 + cm_Idioma ] ) )
               cm_checked := aListItens[ f ][ 11 ]
               cm_Disabled := aListItens[ f ][ 12 ]

               IF cm_tipo = "DEFINE POPUP"
                  DEFINE POPUP cm_caption NAME cm_name
                  ELSEIF cm_tipo = "MENUITEM"
                     IF ! cm_checked
                        IF cm_Disabled
                           IF cm_action = NIL
                              MENUITEM cm_caption ACTION NIL NAME cm_name IMAGE cm_image DISABLED MESSAGE cm_Message
                           ELSE
                              MENUITEM cm_caption ACTION &cm_action NAME cm_name IMAGE cm_image DISABLED MESSAGE cm_Message
                           ENDIF
                        ELSE

                           IF cm_action = NIL
                              MENUITEM cm_caption ACTION NIL NAME cm_name IMAGE cm_image MESSAGE cm_Message
                           ELSE
                              MENUITEM cm_caption ACTION &cm_action NAME cm_name IMAGE cm_image MESSAGE cm_Message
                           ENDIF
                        ENDIF

                     ELSE
                        IF cm_Disabled
                           IF cm_action = NIL
                              MENUITEM cm_caption ACTION NIL NAME cm_name IMAGE cm_image CHECKED DISABLED MESSAGE cm_Message
                           ELSE
                              MENUITEM cm_caption ACTION &cm_action NAME cm_name IMAGE cm_image CHECKED DISABLED MESSAGE cm_Message
                           ENDIF
                        ELSE
                           IF cm_action = NIL
                              MENUITEM cm_caption ACTION NIL NAME cm_name IMAGE cm_image CHECKED MESSAGE cm_Message
                           ELSE
                              MENUITEM cm_caption ACTION &cm_action NAME cm_name IMAGE cm_image CHECKED MESSAGE cm_Message
                           ENDIF
                        ENDIF
                     ENDIF
                  ELSEIF cm_tipo = "SEPARATOR"
                     SEPARATOR
                  ELSEIF cm_tipo = "END POPUP"
                  END POPUP
               ENDIF

            NEXT

         END MENU

         DEFINE STATUSBAR
            STATUSITEM "" DEFAULT
            CLOCK WIDTH 85
            DATE
         END STATUSBAR

      END WINDOW

      CENTER WINDOW WinMain
      ACTIVATE WINDOW WinMain
   ELSE
      MsgStop( cFaltaMenu )
   ENDIF

RETURN NIL

// ------------------------------------------------------------
// Ejemplo de definición de menú partiendo de una tabla
// (r) 2006, Roberto Sánchez
// ------------------------------------------------------------
FUNCTION Salir()

   RELEASE WINDOW ALL

RETURN NIL

// ------------------------------------------------------------
// Ejemplo de definición de menú partiendo de una tabla
// (r) 2006, Roberto Sánchez
// ------------------------------------------------------------
FUNCTION AcercaDe()

   MsgInfo( cAcercaDe )

RETURN NIL
