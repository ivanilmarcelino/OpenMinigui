/*

 Program       : MenuDBF
 Modulo        : menudbf.prg
 Compilador    : MINIGUI - Harbour Win32 GUI
 Link          : BCC 32 bit
 Author        : André Luiz Neto Cobas
 Update        : Roberto Sánchez
 Update        : Marcos Jarrín
 email         : marvijarrin@gmail.com
 website       : badasystem.tech
 Date          : Abril 2006
 Update        : 11/06/2024
 Rev           : 1.0

*/
#include "minigui.ch"
#define CrLf       chr(13)+chr(10)
#define cAcercaDe   "Editor de Menus"+CrLf+"A partir do exemplo de Roberto Sánchez"+CrLf+;
      Chr( 174 ) + " Abril 2006, André Luiz Neto Cobas " + CrLf + CrLf + ;
      "----------------------------------------------------------" + CrLf + CrLf + ;
      "Ejemplo de definición de menú" + CrLf + "en tiempo de ejecución" + CrLf + CrLf + cVersion + CrLf + Chr( 174 ) + " Abril 2006, Roberto Sánchez" + CrLf + Chr( 174 ) + " June 2024, Marcos Jarrin" +  CrLf + MiniGUIVersion() + CrLf + Version()
#define cVersion   "Versión 00.10.00"

/*
ShellExecute(0,"open","rundll32.exe","url.dll,FileProtocolHandler"+"mailto:andrecobas@yahoo.com.br?cc=&bcc="+"&subject=Editor%20de%20Menus:"+"&body=How%20are%20you%2C%20André%3F", , 1)

ShellExecute(0, "open", "rundll32.exe","url.dll,FileProtocolHandler " + "mailto:andrecobas@yahoo.com.br?cc=&bcc=" + "&subject=Editor%20de%20Menus:" + "&body=Como%20vai%20você%2C%20André%3F", , 1)
*/
STATIC Mitem      := 0
STATIC qesp       := 0
STATIC MyMenu     := "0"
STATIC MenuPai    := "0"
STATIC MnuLn      := {}
STATIC ITMAT      := 0
STATIC Fpos       := 0
STATIC TAG        := 0
STATIC aListItens := {}
STATIC ATIVADO    := .F.
//Static QTS := 0 Unused variable

// ------------------------------------------------------------------------------------
FUNCTION Main()

   SET NAVIGATION EXTENDED
   SET DELETE ON
   SET EXCLUSIVE OFF

   AAdd( aListItens, { "-", "", "", "", "", "", "", "", "", "", "", "", "", .F., .F. } )

   DEFINE WINDOW FORM1 ;
         AT 82, 157 ;
         WIDTH 800 ;
         HEIGHT 610 ;
         TITLE "Editor de Menus " + Chr( 174 ) + " June 2024, André Luiz Neto Cobas - Marcos Jarrin" ;
         MAIN ;
         NOMAXIMIZE ;
         NOSIZE ;
         ON INIT formActivate() ;
         ON RELEASE NIL

      @ 10,  10 LABEL Lbltxt WIDTH 770 HEIGHT 20 BACKCOLOR { 255, 255, 206 } FONTCOLOR NIL BORDER

      @ 34,  10 FRAME Frame_1 WIDTH 120 HEIGHT 175 CAPTION ""
      @ 34, 138 FRAME Frame_2 WIDTH 122 HEIGHT 115 CAPTION ""
      @ 210, 10 FRAME Frame_3 WIDTH 253 HEIGHT 350 CAPTION ""

      @ 150, 138 FRAME Frame_4 WIDTH 122 HEIGHT 60 CAPTION ""

      @ 227, 20 LABEL Lbl_P WIDTH 70 HEIGHT 24 VALUE "Caption Port.:" TRANSPARENT FONTCOLOR { 0, 0, 255 } AUTOSIZE
      @ 257, 20 LABEL Lbl_E WIDTH 70 HEIGHT 24 VALUE "Caption Esp.:"  TRANSPARENT FONTCOLOR { 0, 0, 255 } AUTOSIZE
      @ 287, 20 LABEL Lbl_I WIDTH 70 HEIGHT 24 VALUE "Caption Ing.:"  TRANSPARENT FONTCOLOR { 0, 0, 255 } AUTOSIZE
      // -----------------------------------------------------------------------------
      @ 317, 20 LABEL LblAction WIDTH 70 HEIGHT 24 VALUE "Action:" TRANSPARENT FONTCOLOR { 0, 0, 255 } AUTOSIZE
      @ 347, 20 LABEL LblName   WIDTH 70 HEIGHT 24 VALUE "Name:"   TRANSPARENT FONTCOLOR { 0, 0, 255 } AUTOSIZE
      @ 377, 20 LABEL LblImage  WIDTH 70 HEIGHT 24 VALUE "Image:"  TRANSPARENT FONTCOLOR { 0, 0, 255 } AUTOSIZE
      // -----------------------------------------------------------------------------
      @ 407, 20 LABEL LbMsg_P WIDTH 70 HEIGHT 20 VALUE "Message Port.:" TRANSPARENT FONTCOLOR { 0, 0, 255 } AUTOSIZE
      @ 437, 20 LABEL LbMsg_E WIDTH 70 HEIGHT 20 VALUE "Message Esp.:"  TRANSPARENT FONTCOLOR { 0, 0, 255 } AUTOSIZE
      @ 467, 20 LABEL LbMsg_I WIDTH 70 HEIGHT 20 VALUE "Message Ing.:"  TRANSPARENT FONTCOLOR { 0, 0, 255 } AUTOSIZE

      // -----------------------------------------------------------------------------
      @ 50, 20 BUTTONEX BtModifica WIDTH 100 HEIGHT 30 CAPTION "Modificar Linha" ;
         ACTION Bt_Inc_Click( 3 ) FLAT fontcolor { 0, 0, 255 }

      @ 80, 20 BUTTONEX BtLoad WIDTH 100 HEIGHT 30 CAPTION "Carregar Menu" ;
         ACTION BtLoad_Click() FLAT fontcolor { 0, 0, 255 }

      @ 110, 20 BUTTONEX BtSalvar WIDTH 100 HEIGHT 30 CAPTION "Salvar Menu" ;
         ACTION BtSalvar_Click() FLAT fontcolor { 0, 0, 255 }

      @ 140, 20 BUTTONEX BtApagar WIDTH 100 HEIGHT 30 CAPTION "Apagar Tudo" ;
         ACTION BtApagarTudo_Click( .T. ) FLAT fontcolor { 255, 0, 0 }

      @ 170, 20 BUTTONEX BtRemover WIDTH 100 HEIGHT 30 CAPTION "Remover" ;
         ACTION BtRemover_Click() FLAT fontcolor { 255, 100, 0 }

      // -----------------------------------------------------------------------------
      @ 50, 150 BUTTONEX BtPopUp WIDTH 100 HEIGHT 30 CAPTION "&POPUP" ;
         ACTION Bt_Inc_Click( 0 ) FLAT fontcolor { 0, 0, 255 }

      @ 80, 150 BUTTONEX BtMenuItem WIDTH 100 HEIGHT 30 CAPTION "MENU&ITEM" ;
         ACTION Bt_Inc_Click( 1 ) FLAT fontcolor { 0, 0, 255 }

      @ 110, 150 BUTTONEX BtSeparator WIDTH 99 HEIGHT 30 CAPTION "&SEPARATOR" ;
         ACTION Bt_Inc_Click( 2 ) FLAT fontcolor { 0, 0, 255 }

      @ 170, 150 BUTTONEX BtTestar WIDTH 99 HEIGHT 30 CAPTION "&Testar" ;
         ACTION Bt_Testar_Click() FLAT fontcolor { 0, 0, 255 }

      // -----------------------------------------------------------------------------
      @ 225, 105 TEXTBOX TxtCaptionP WIDTH 150 HEIGHT 24 BACKCOLOR { { 255, 255, 255 }, { 255, 255, 255 }, { 255, 210, 0 } } ;
         FONTCOLOR { { 0, 0, 0 }, { 0, 0, 0 }, { 0, 0, 255 } } VALUE Space( 50 )

      @ 255, 105 TEXTBOX TxtCaptionE WIDTH 150 HEIGHT 24 BACKCOLOR { { 255, 255, 255 }, { 255, 255, 255 }, {255, 210, 0 } } ;
         FONTCOLOR { { 0, 0, 0 }, { 0, 0, 0 }, { 0, 0, 255 } } VALUE Space( 50 )
      @ 285, 105 TEXTBOX TxtCaptionI WIDTH 150 HEIGHT 24 BACKCOLOR { { 255, 255, 255 }, { 255, 255, 255 }, {255, 210, 0 } } ;
         FONTCOLOR { { 0, 0, 0 }, { 0, 0, 0 }, { 0, 0, 255 } } VALUE Space( 50 )

      // -----------------------------------------------------------------------------
      @ 315, 105 TEXTBOX TxtAction WIDTH 150 HEIGHT 24 BACKCOLOR { { 255, 255, 255 }, { 255, 255, 255 }, {255, 210, 0 } } ;
         FONTCOLOR { { 0, 0, 0 }, { 0, 0, 0 }, { 0, 0, 255 } } VALUE Space( 200 )

      @ 345, 105 TEXTBOX TxtNome WIDTH 150 HEIGHT 24 BACKCOLOR { { 255, 255, 255 }, { 255, 255, 255 }, {255, 210, 0 } } ;
         FONTCOLOR { { 0, 0, 0 }, { 0, 0, 0 }, { 0, 0, 255 } } VALUE Space( 50 ) // VALID {|| ValidaNome(this.value)}

      @ 375, 105 TEXTBOX TxtImage WIDTH 150 HEIGHT 24 BACKCOLOR { { 255, 255, 255 }, { 255, 255, 255 }, {255, 210, 0 } } ;
         FONTCOLOR { { 0, 0, 0 }, { 0, 0, 0 }, { 0, 0, 255 } } VALUE Space( 100 )

      @ 405, 105 TEXTBOX TxtMessageP WIDTH 150 HEIGHT 24 BACKCOLOR { { 255, 255, 255 }, { 255, 255, 255 }, {255, 210, 0 } } ;
         FONTCOLOR { { 0, 0, 0 }, { 0, 0, 0 }, { 0, 0, 255 } } VALUE Space( 100 )

      @ 435, 105 TEXTBOX TxtMessageE WIDTH 150 HEIGHT 24 BACKCOLOR { { 255, 255, 255 }, { 255, 255, 255 }, {255, 210, 0 } } ;
         FONTCOLOR { { 0, 0, 0 }, { 0, 0, 0 }, { 0, 0, 255 } } VALUE Space( 100 )

      @ 465, 105 TEXTBOX TxtMessageI WIDTH 150 HEIGHT 24 BACKCOLOR { { 255, 255, 255 }, { 255, 255, 255 }, {255, 210, 0 } } ;
         FONTCOLOR { { 0, 0, 0 }, { 0, 0, 0 }, { 0, 0, 255 } } VALUE Space( 100 )
      // -----------------------------------------------------------------------------------------------------
      @ 490, 20 CHECKBOX Chk_Checked CAPTION "Checked:" WIDTH 100 HEIGHT 28 VALUE .F. FONTCOLOR { 0, 0, 255 } TRANSPARENT LEFTJUSTIFY
      @ 490, 155 CHECKBOX Chk_Disabled CAPTION "Disabled" WIDTH 100 HEIGHT 28 VALUE .F. FONTCOLOR { 0, 0, 255 } TRANSPARENT LEFTJUSTIFY

      @ 520, 20 BUTTONEX BtOk WIDTH 110 HEIGHT 30 CAPTION "&Ok" ;
         ACTION BtOk_Click() FLAT fontcolor { 0, 0, 255 }

      @ 520, 139 BUTTONEX BtCancel WIDTH 110 HEIGHT 30 CAPTION "&Cancelar" ;
         ACTION Bt_Cancel_Click() FLAT fontcolor { 0, 0, 255 }


      DEFINE GRID Grid1
         ROW 41
         COL 271
         WIDTH 509 // 869
         HEIGHT 520
         VALUE 1
         ITEMS aListItens
         HEADERS { "LINHA DO MENU", ;
            "ID", ;
            "MENU PAI", ;
            "TIPO", ;
            "CAPTION_P", ;
            "CAPTION_E", ;
            "CAPTION_I", ;
            "NOME", ;
            "ACTION", ;
            "IMAGE", ;
            "MESSAGE_P", ;
            "MESSAGE_E", ;
            "MESSAGE_I", ;
            "CHECKED", ;
            "DISABLED" }
         WIDTHS { 449, 30, 30, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 30, 30 }
         ONCHANGE ( iif( ATIVADO = .T., ( E_D_Frame3( .F. ), ChecaList() ), NIL ) )
         NOLINES .T.
         FONTNAME "courier new"
         FONTSIZE 10
      END GRID
   END WINDOW
   ACTIVATE WINDOW Form1

RETURN NIL

// ------------------------------------------------------------------------------------
FUNCTION ValidaNome( cNome )

   LOCAL lRet := .T., f

   cNome := Upper( AllTrim( cNome ) )
   if ! Empt( cNome )
      FOR f := 1 TO Len( aListItens )
         IF Upper( AllTrim( aListItens[ f ][ 8 ] ) ) == cNome
            lret := .T.
            IF aListItens[ f ][ 4 ] = "DEFINE POPUP"
               MsgInfo( "Já Existe um POPUP com este nome", "Atenção" )
            ELSE
               MsgInfo( "Já Existe um MENUITEM com este nome", "Atenção" )
            ENDIF
         ENDIF
      NEXT
   ENDIF

RETURN lret

// ------------------------------------------------------------------------------------
PROCEDURE formActivate()

   //QTS := 100 Unused variable
   Inicia()

RETURN

// ------------------------------------------------------------------------------------
PROCEDURE Inicia()

   MyMenu := Str( 0 )
   MenuPai := Str( 0 )
   BtApagarTudo_Click( .F. )

   form1.TxtCaptionP.VALUE := "Menu PopUp P" + AllTrim( Str( Val( MyMenu ) + 1 ) )
   form1.TxtCaptionE.VALUE := "Menu PopUp E" + AllTrim( Str( Val( MyMenu ) + 1 ) )
   form1.TxtCaptionI.VALUE := "Menu PopUp I" + AllTrim( Str( Val( MyMenu ) + 1 ) )
   form1.TxtNome.VALUE := "MNU01"
   ATIVADO := .T.
   InserePopUp()
   form1.Grid1.VALUE := 1
   E_D_Frame3( .F. )

RETURN

// ------------------------------------------------------------------------------------
PROCEDURE LimpaInFo()

   form1.TxtCaptionP.VALUE  := Space( 50 )
   form1.TxtCaptionE.VALUE  := Space( 50 )
   form1.TxtCaptionI.VALUE  := Space( 50 )
   form1.TxtAction.VALUE    := Space( 200 )
   form1.TxtNome.VALUE      := Space( 50 )
   form1.TxtImage.VALUE     := Space( 100 )
   form1.TxtMessageP.VALUE  := Space( 100 )
   form1.TxtMessageE.VALUE  := Space( 100 )
   form1.TxtMessageI.VALUE  := Space( 100 )
   form1.Chk_Checked.VALUE  := .F.
   form1.Chk_Disabled.VALUE := .F.

RETURN

// ------------------------------------------------------------------------------------
PROCEDURE PegaInFo( I )
   form1.TxtCaptionP.VALUE  := iif ( ! Empt( aListItens[ I ][ 5 ] ), aListItens[ I ][ 5 ], "" ) // "CAPTION_P"
   form1.TxtCaptionE.VALUE  := iif ( ! Empt( aListItens[ I ][ 6 ] ), aListItens[ I ][ 6 ], "" ) // "CAPTION_E"
   form1.TxtCaptionI.VALUE  := iif ( ! Empt( aListItens[ I ][ 7 ] ), aListItens[ I ][ 7 ], "" ) // "CAPTION_I"
   form1.TxtNome.VALUE      := iif ( ! Empt( aListItens[ I ][ 8 ] ), aListItens[ I ][ 8 ], "" ) // "NOME"
   form1.TxtAction.VALUE    := iif ( ! Empt( aListItens[ I ][ 9 ] ), aListItens[ I ][ 9 ], "" ) // "ACTION"
   form1.TxtImage.VALUE     := iif ( ! Empt( aListItens[ I ][ 10 ] ), aListItens[ I ][ 10 ], "" ) // "IMAGE"
   form1.TxtMessageP.VALUE  := iif ( ! Empt( aListItens[ I ][ 11 ] ), aListItens[ I ][ 11 ], "" ) // "MESSAGE_P"
   form1.TxtMessageE.VALUE  := iif ( ! Empt( aListItens[ I ][ 12 ] ), aListItens[ I ][ 12 ], "" ) // "MESSAGE_E"
   form1.TxtMessageI.VALUE  := iif ( ! Empt( aListItens[ I ][ 13 ] ), aListItens[ I ][ 13 ], "" ) // "MESSAGE_I"
   form1.Chk_Checked.VALUE  := iif ( ! Empt( aListItens[ I ][ 14 ] ), aListItens[ I ][ 14 ], .F. ) // "CHECKED"
   form1.Chk_Disabled.VALUE := iif ( ! Empt( aListItens[ I ][ 15 ] ), aListItens[ I ][ 15 ], .F. ) // "DISABLED"

   ITMAT := I

RETURN
// ------------------------------------------------------------------------------------
PROCEDURE E_D_Frame3( lvalor )
   form1.TxtCaptionP.Enabled  := lvalor
   form1.TxtCaptionE.Enabled  := lvalor
   form1.TxtCaptionI.Enabled  := lvalor
   form1.TxtAction.Enabled    := lvalor
   form1.TxtNome.Enabled      := lvalor
   form1.TxtImage.Enabled     := lvalor
   form1.TxtMessageP.Enabled  := lvalor
   form1.TxtMessageE.Enabled  := lvalor
   form1.TxtMessageI.Enabled  := lvalor
   form1.Chk_Checked.Enabled  := lvalor
   form1.Chk_Disabled.Enabled := lvalor
   form1.BtOK.enabled         := lvalor
   form1.BtCancel.enabled     := lvalor

RETURN

// ------------------------------------------------------------------------------------
PROCEDURE E_D_Frame1( lvalor )
   form1.BtModifica.Enabled := lvalor
   form1.BtLoad.Enabled     := lvalor
   form1.BtSalvar.Enabled   := lvalor
   form1.BtApagar.Enabled   := lvalor
   form1.BtRemover.Enabled  := lvalor

RETURN
// ------------------------------------------------------------------------------------
FUNCTION PegaTabulacao( wpos )
LOCAL Wdif := 0, ln, wtmp

   IF empty( wpos )
      wpos := 0
   ENDIF

   IF wpos > 0
      //Update
      //ln := form1.Grid1.CELL((wpos-1),1)
      ln := form1.Grid1.CELL( ( wpos ), 1 )
      wtmp := LTrim( ln )
      wdif := Len( ln ) - Len( wtmp )

      IF Left( wtmp, 12 ) = "DEFINE POPUP"
         wdif := wdif + 4
      END IF

   END IF

RETURN wdif

// ------------------------------------------------------------------------------------
PROCEDURE E_D_Frame2( valor )

   Form1.Btpopup.Enabled     := valor
   Form1.BtMenuitem.Enabled  := valor
   Form1.BtSeparator.Enabled := valor

RETURN

// ------------------------------------------------------------------------------------
PROCEDURE Bt_Inc_Click( nIndex )

   LOCAL Nm := ""
   LOCAL tx := ""
   LOCAL wp := 0
   LOCAL ln

   E_D_Frame1( .F. )
   E_D_Frame2( .F. )
   E_D_Frame3( .F. )
   form1.BtOK.enabled     := .T.
   form1.BtCancel.enabled := .T.

   IF nIndex # 3
      limpaInfo()
   END IF

   DO CASE
      CASE nIndex = 0
         form1.TxtCaptionP.enabled := .T.
         form1.TxtCaptionE.enabled := .T.
         form1.TxtCaptionI.enabled := .T.
         form1.TxtNome.enabled     := .T.

         form1.TxtCaptionP.VALUE := "Menu PopUp P" + AllTrim( Str( Val( MyMenu ) + 1 ) )
         form1.TxtCaptionE.VALUE := "Menu PopUp E" + AllTrim( Str( Val( MyMenu ) + 1 ) )
         form1.TxtCaptionI.VALUE := "Menu PopUp I" + AllTrim( Str( Val( MyMenu ) + 1 ) )
         form1.TxtCaptionP.SetFocus()
         TAG := nIndex
      CASE nIndex = 1
         E_D_Frame3( .T. )
         form1.TxtCaptionP.VALUE := "Menu Item P" + AllTrim( Str( Val( MyMenu ) + 1 ) )
         form1.TxtCaptionE.VALUE := "Menu Item E" + AllTrim( Str( Val( MyMenu ) + 1 ) )
         form1.TxtCaptionI.VALUE := "Menu Item I" + AllTrim( Str( Val( MyMenu ) + 1 ) )
         form1.TxtCaptionP.SetFocus()
         TAG := nIndex
      CASE nIndex = 2
         E_D_Frame1( .T. )
         E_D_Frame2( .T. )
         E_D_Frame3( .F. )
         MenuPai := PegaMenuPai()
         InsereMENUITEM ( "-" )
         ChecaList()

      CASE nIndex = 3
         ln := AllTrim( form1.Grid1.cell( form1.Grid1.VALUE, 1 ) )

         IF Upper( Left( Ln, 12 ) ) = "DEFINE POPUP"
            form1.TxtCaptionP.Enabled := .T.
            form1.TxtCaptionE.Enabled := .T.
            form1.TxtCaptionI.Enabled := .T.
            form1.TxtNome.Enabled := .T.
         ELSE
            E_D_Frame3( .T. )
         END IF

         form1.TxtCaptionP.SetFocus()
         TAG := nIndex
   ENDCASE

RETURN

// ------------------------------------------------------------------------------------
FUNCTION PegaMenuPai()

   LOCAL f, ln
   LOCAL wret := "0"
   LOCAL wp := form1.Grid1.VALUE
   LOCAL qEnd := 1

   //Update
   //FOR f := wp - 1 TO 0 STEP -1
   FOR f := wp - 1 TO 1 STEP -1
      ln := LTrim( form1.Grid1.CELL( f, 1 ) )

      IF Upper( Left( ln, 12 ) ) = "DEFINE POPUP"
         qEnd := qEnd - 1
      ENDIF

      IF Upper( Left( ln, 3 ) ) = "END"
         qEnd := qEnd + 1
      ENDIF

      IF qEnd = 0
         wret := form1.Grid1.cell( f, 2 )
         RETURN wret
      END IF

   NEXT

RETURN wret

// ------------------------------------------------------------------------------------
PROCEDURE ChecaList()

   LOCAL it
   LOCAL wp := form1.Grid1.VALUE
   LOCAL txt := form1.Grid1.cell( wp, 1 )

   IF FORM1.Grid1.ITEMCOUNT > 0
      it := form1.Grid1.cell( form1.Grid1.VALUE, 4 )

      IF Left( AllTrim( txt ), 3 ) = "END" .OR. txt = "-"
         form1.BtRemover.Enabled  := .F.
         form1.BtModifica.Enabled := .F.
         LIMPAINFO()
      ELSE
         form1.BtRemover.Enabled := .T.

         IF ( "SEPAR" $ it )
            form1.BtModifica.Enabled := .F.
            LIMPAINFO()
         ELSE
            form1.BtModifica.Enabled := .T.
         END IF

      END IF

      IF Left( txt, 1 ) # " " .AND. Upper( Left( txt, 1 ) ) # "E"
         form1.Btseparator.Enabled := .F.
         form1.BtMenuItem.Enabled  := .F.
      ELSE
         form1.Btseparator.Enabled := .T.
         form1.BtMenuItem.Enabled  := .T.
      END IF

      IF it = "DEFINE POPUP" .OR. it = "MENUITEM"
         PEGAINFO ( form1.Grid1.value )
      END IF

      form1.Lbltxt.VALUE := AllTrim( form1.Grid1.cell( form1.Grid1.VALUE, 1 ) )
   ENDIF

RETURN

// ------------------------------------------------------------------------------------
FUNCTION PegaPos( wpos )

   LOCAL wret := -1
   LOCAL ID := 0, ltmp

   LOCAL ln := form1.Grid1.cell( wpos, 1 )
   IF ln = "-"
      MenuPai := Str( 0 )
      RETURN wret
   END IF

   ltmp := LTrim( ln )
   IF Left( ltmp, 3 ) = "END"
      RETURN wret
   ENDIF

   wret := Val( form1.Grid1.cell( wpos, 2 ) )

RETURN wret

// ------------------------------------------------------------------------------------
procedure ModificaMENUITEM()

   LOCAL tipo
   LOCAL TXT

   fPos := form1.Grid1.VALUE // linha sendo modificada
   tipo := aListItens[ fPos ][ 4 ]
   TXT := Space( PegaTabulacao( fPos ) ) + LinhaMenu( form1.TxtCaptionP.VALUE, ;
      form1.TxtCaptionE.VALUE, ;
      form1.TxtCaptionI.VALUE, ;
      form1.TxtAction.VALUE, ;
      form1.TxtNome.VALUE, ;
      form1.TxtImage.VALUE, ;
      form1.TxtMessageP.VALUE, ;
      form1.TxtMessageE.VALUE, ;
      form1.TxtMessageI.VALUE, ;
      form1.Chk_Checked.VALUE, ;
      form1.Chk_Disabled.VALUE, ;
      tipo )
   aListItens[ fpos ] := { TXT, ;
      aListItens[ fPos ][ 2 ], ;
      aListItens[ fPos ][ 3 ], ;
      tipo, ;
      Form1.TxtCaptionP.VALUE, ;
      Form1.TxtCaptionE.VALUE, ;
      Form1.TxtCaptionI.VALUE, ;
      form1.TxtNome.VALUE, ;
      form1.TxtAction.VALUE, ;
      form1.TxtImage.VALUE, ;
      form1.TxtMessageP.VALUE, ;
      form1.TxtMessageE.VALUE, ;
      form1.TxtMessageI.VALUE, ;
      form1.Chk_Checked.VALUE, ;
      form1.Chk_Disabled.Value }
   AtualList()
   form1.Grid1.VALUE := fpos

RETURN

// ------------------------------------------------------------------------------------
procedure InsereMenuItem( txtsep )

   LOCAL tipo
   LOCAL TXT

   MyMenu := Str( Val( MyMenu ) + 1 )
   MenuPai := PegaMenuPai()

   IF Form1.Grid1.cell( Form1.Grid1.ItemCount, 1 ) = "-"
      fpos := Form1.Grid1.VALUE
   END IF

   IF fpos < 1
      fpos := 1
   ENDIF

   IF TXTSEP = "-"
      Tipo := "SEPARATOR"
   ELSE
      Tipo := "MENUITEM"
   END IF

   TXT := Space( PegaTabulacao( fPos ) ) + LinhaMenu( form1.TxtCaptionP.VALUE, ;
      form1.TxtCaptionE.VALUE, ;
      form1.TxtCaptionI.VALUE, ;
      form1.TxtAction.VALUE, ;
      form1.TxtNome.VALUE, ;
      form1.TxtImage.VALUE, ;
      form1.TxtMessageP.VALUE, ;
      form1.TxtMessageE.VALUE, ;
      form1.TxtMessageI.VALUE, ;
      form1.Chk_Checked.VALUE, ;
      form1.Chk_Disabled.VALUE, ;
      tipo )

   AAdd( aListItens, { "-", "", "", "", "", "", "", "", "", "", "", "", "", .F., .F. } )
   AIns( aListItens, fpos )

   aListItens[ fpos ] := { TXT, ;
      AllTrim( MyMenu ), ;
      AllTrim( MenuPai ), ;
      AllTrim( Tipo ), ;
      Form1.TxtCaptionP.VALUE, ;
      Form1.TxtCaptionE.VALUE, ;
      Form1.TxtCaptionI.VALUE, ;
      form1.TxtNome.VALUE, ;
      form1.TxtAction.VALUE, ;
      form1.TxtImage.VALUE, ;
      form1.TxtMessageP.VALUE, ;
      form1.TxtMessageE.VALUE, ;
      form1.TxtMessageI.VALUE, ;
      form1.Chk_Checked.VALUE, ;
      form1.Chk_Disabled.Value }
   Mitem++
   AtualList()
   form1.Grid1.VALUE := fpos

RETURN
// ------------------------------------------------------------------------------------
PROCEDURE InserePopUp()

   LOCAL cTipo := ""
   LOCAL Texto, nome, tx

   MyMenu := Str( Val( MyMenu ) + 1 )
   MenuPai := iif ( form1.Grid1.ItemCOunt > 0, PegaMenuPai(), Str( 0 ) )

   fpos := Form1.Grid1.VALUE

   IF form1.Grid1.CELL( ( Form1.Grid1.VALUE -1 ), 1 ) = "-"
      fpos := Form1.Grid1.VALUE
   END IF

   Texto := Chr( 34 ) + form1.TxtCaptionP.VALUE +Chr( 34 )
   nome := AllTrim( form1.TxtNome.value )

   If ! Empty( nome )
      Texto := Texto + " NAME " + nome
   ENDIF

   IF fpos < 1
      fpos := 1
   ENDIF

   qesp := PegaTabulacao( fpos )

   tx := Space( qesp ) + "DEFINE POPUP " + Texto
   AAdd( aListItens, { "-", "", "", "", "", "", "", "", "", "", "", "", "", .F., .F. } )
   AIns( aListItens, fpos )
   aListItens[ fpos ] := { tx, ;
      AllTrim( MyMenu ), ;
      AllTrim( MenuPai ), ;
      "DEFINE POPUP", ;
      Form1.TxtCaptionP.VALUE, ;
      Form1.TxtCaptionE.VALUE, ;
      Form1.TxtCaptionI.VALUE, ;
      form1.TxtNome.VALUE, ;
      "", "", "", "", "", .F., .F. }

   tx := Space( qesp ) + "END POPUP "
   AAdd( aListItens, { "-", "", "", "", "", "", "", "", "", "", "", "", "", .F., .F. } )
   AIns( aListItens, fpos + 1 )
   aListItens[ fpos + 1 ] := { tx, ;
      AllTrim( MyMenu ), ;
      AllTrim( MenuPai ), ;
      "END POPUP", "", "", "", "", "", "", "", "", "", .F., .F. }

   AtualList()
   form1.Grid1.VALUE := fpos

RETURN

// ------------------------------------------------------------------------------------
PROCEDURE Bt_Cancel_Click()

   E_D_Frame3( .F. )
   E_D_Frame2( .T. )
   E_D_Frame1( .T. )

   form1.Grid1.Enabled := .T.
   ChecaList()

RETURN
// ------------------------------------------------------------------------------------
PROCEDURE AtualList()

   LOCAL f

   Form1.Grid1.deleteAllItems()

   FOR f := 1 TO Len( aListItens )
      form1.Grid1.additem( { aListItens[ f ][ 1 ], ;
         aListItens[ f ][ 2 ], ;
         aListItens[ f ][ 3 ], ;
         aListItens[ f ][ 4 ], ;
         aListItens[ f ][ 5 ], ;
         aListItens[ f ][ 6 ], ;
         aListItens[ f ][ 7 ], ;
         aListItens[ f ][ 8 ], ;
         aListItens[ f ][ 9 ], ;
         aListItens[ f ][ 10 ], ;
         aListItens[ f ][ 11 ], ;
         aListItens[ f ][ 12 ], ;
         aListItens[ f ][ 13 ], ;
         aListItens[ f ][ 14 ], ;
         aListItens[ f ][ 15 ] } )
   NEXT

RETURN

// ------------------------------------------------------------------------------------
PROCEDURE BtOk_Click()

   LOCAL tx := ""

   SelecionaTipo()
   E_D_FRAME2( .T. )
   E_D_FRAME3( .F. )
   E_D_FRAME1( .T. )
   FORM1.Grid1.Enabled := .T.
   ChecaList()

RETURN
// ------------------------------------------------------------------------------------
PROCEDURE SelecionaTipo()

   DO CASE
      CASE Tag = 0
         InserePopUp()
      CASE Tag = 1
         InsereMENUITEM()
      CASE Tag = 2
         InsereMENUITEM( '-' )
      CASE Tag = 3
         ModificaMENUITEM()
   END CASE

RETURN

// ------------------------------------------------------------------------------------
FUNCTION LinhaMenu( TxtCaptionP, TxtCaptionE, TxtCaptionI, TxtAction, TxtNome, TxtImage, TxtMessageP, TxtMessageE, TxtMessageI, Chk_Checked, Chk_Disabled, tipo )

   LOCAL TXT := ""
   LOCAL ctxtAction := ""
   LOCAL ctxtName := ""
   LOCAL ctxtImage := ""
   LOCAL ctxtMsg := ""
   LOCAL ctxtChecked := ""
   LOCAL ctxtDisabled := ""
   LOCAL ctipo := ""

   If ! Empty( AllTrim( TxtNome ) )
      ctxtName := " NAME " + AllTrim( TxtNome )
   END IF

   IF tipo = "MENUITEM"

      IF Empty( AllTrim( TxtAction ) )
         ctxtAction := " ACTION Nil"
      ELSE
         ctxtAction := " ACTION " + AllTrim( TxtAction )
      END IF

      If ! Empty( AllTrim( TxtImage ) )
         ctxtImage := " IMAGE " + Chr( 34 ) + AllTrim( TxtImage ) + Chr( 34 )
      END IF

      ctxtMsg := AllTrim( TxtMessageP )
      IF Empty( ctxtMsg )
         ctxtMsg := AllTrim( TxtMessageE )
      ENDIF

      IF Empty( ctxtMsg )
         ctxtMsg := AllTrim( TxtMessageI )
      ENDIF

      If ! Empty( AllTrim( ctxtMsg ) )
         ctxtMsg := " MESSAGE " + Chr( 34 ) + AllTrim( ctxtMsg ) + Chr( 34 )
      END IF

      IF Chk_Checked = .T.
         ctxtChecked := " CHECKED"
      ENDIF

      IF Chk_Disabled = .T.
         ctxtDisabled := " DISABLED"
      END IF
   ENDIF

   IF tipo = "MENUITEM" .OR. TIPO = "DEFINE POPUP"
      TXT := AllTrim( tipo ) + " " + Chr( 34 ) + AllTrim( TxtCaptionP ) + Chr( 34 )
   ELSE
      TXT := AllTrim( tipo )
   ENDIF

   TXT += ctxtAction + ctxtName + ctxtImage + ctxtChecked + ctxtDisabled + ctxtMsg

RETURN Txt

// ------------------------------------------------------------------------------------
PROCEDURE BtApagarTudo_Click( lpergunta )

   LOCAL lescolha := .F.

   IF lpergunta
      lescolha := msgyesno( "Tem Certeza Que Deseja Apagar Tudo?", "Atenção", .F. )

      IF lescolha = .F.
         RETURN
      ENDIF

   ENDIF

   MyMenu  := Str( 0 )
   MenuPai := Str( 0 )
   mItem   := 1
   Apagatudo()
   AtualList()
   FORM1.Grid1.VALUE := 1
   FORM1.Grid1.SETFOCUS()

RETURN

// ------------------------------------------------------------------------------------
PROCEDURE ApagaTudo()
   Form1.Grid1.deleteAllItems()
   ASize( aListItens, 0 )
   AAdd( aListItens, { "-", "", "", "", "", "", "", "", "", "", "", "", "", .F., .F. } )

RETURN

// ------------------------------------------------------------------------------------
PROCEDURE BtLoad_Click()
local cSpace := ""

   LOCAL cFile, c1, c_dbf
   LOCAL txt

   cFile := GetFile ( { { "Arquivos DBF", "*.DBF" } }, "Abrir Arquivo", GetCurrentFolder(), .F., .F. )

   If ! Empt( AllTrim( cFile ) )
      C1 := RAt( ".", CFILE )
      C_DBF := Left( CFILE, C1 - 1 )

       C1:=RAT("\",C_DBF)
       C_NTAB:= RIGHT(C_DBF,LEN(C_DBF)-C1)

      Mitem := 1
      fpos := 1
      qesp := 0

      USE &C_DBF NEW
      // INDEX ON descend(FIELD->LINHA) TO &C_DBF                         Erased line

      ApagaTudo()

      DO while ! Eof()
         AAdd( aListItens, { "-", "", "", "", "", "", "", "", "", "", "", "", "", .F., .F. } )
         txt := ""
         MyMenu := ( Alias() )->ID
         MenuPai := ( Alias() )->MNUPAI

         IF ( Alias() )->TIPO = "DEFINE POPUP"
            qesp := qesp - 4
         ENDIF

         //Update
         //Place 5 spaces at the beginning of the MENUITEM option
         IF ( Alias() )->TIPO = "MENUITEM"
            qesp := 5
         ELSE
            qesp := qesp - 4
         ENDIF

         IF qesp > 0
            txt := Space( qesp ) + txt
         ENDIF

         if( alltrim(( Alias() )->TIPO) == "MENUITEM",cSpace := space(5),cSpace := "")
         TxT += LinhaMenu( ( Alias() )->CAPTIONP, ;
            ( Alias() )->CAPTIONE, ;
            ( Alias() )->CAPTIONI, ;
            ( Alias() )->ACTION, ;
            ( Alias() )->NOME, ;
            ( Alias() )->IMAGE, ;
            ( Alias() )->MESSAGEP, ;
            ( Alias() )->MESSAGEE, ;
            ( Alias() )->MESSAGEI, ;
            ( Alias() )->CHECKED, ;
            ( Alias() )->DISABLED, ;
            ( Alias() )->TIPO )


         aListItens[ fpos++ ] := { TXT, ;
            AllTrim( MyMenu ), ;
            AllTrim( MenuPai ), ;
            ( Alias() )->TIPO, ;
            ( Alias() )->CAPTIONP, ;
            ( Alias() )->CAPTIONE, ;
            ( Alias() )->CAPTIONI, ;
            ( Alias() )->NOME, ;
            ( Alias() )->ACTION, ;
            ( Alias() )->IMAGE, ;
            ( Alias() )->MESSAGEP, ;
            ( Alias() )->MESSAGEE, ;
            ( Alias() )->MESSAGEI, ;
            ( Alias() )->CHECKED, ;
            ( Alias() )->DISABLED }

         IF ( Alias() )->TIPO = "END POPUP"
            qesp += 4
         ENDIF

         Mitem++
         SKIP
      ENDDO

      USE
      AtualList()
      FORM1.Grid1.VALUE := 1
      FORM1.Grid1.SETFOCUS()
   ENDIF

RETURN

// ------------------------------------------------------------------------------------
PROCEDURE BtSalvar_Click()

   LOCAL cFile, x, c1, f
   LOCAL AaRQ := {}
   LOCAL C_DBF := "", C_NTAB := ""

   cFile := Putfile ( { { "Arquivos DBF", "*.DBF" } }, ;
      'Salvar Arquivo como...', GetCurrentFolder(), .F., "MENU.DBF" )

   CFILE := Upper( CFILE )

   If ! Empty( cFile )
      IF File( cfile )
         x := msgyesno( "Tem certeza que quer substituir este arquivo?", "Arquivo já existe", .F. )

         IF x = .F.
            RETURN
         ELSE
            IF FErase( cfile ) = -1
               msginfo( "Arquivo Está Sendo Usado.", "Erro ao Apagar o Arquivo" )
               RETURN
            ENDIF
         ENDIF
      ENDIF

      IF Right( cfile, 3 ) = "DBF"
         C1 := RAt( ".", CFILE )
         C_DBF := Left( CFILE, C1 - 1 )

         C1 := RAt( "\", C_DBF )
         C_NTAB := Right( C_DBF, Len( C_DBF ) - C1 )

         AAdd( aArq, { 'LINHA',   'C',   6, 0 } )
         AAdd( aArq, { 'ID',      'C',   6, 0 } )
         AAdd( aArq, { 'MNUPAI',  'C',   6, 0 } )
         AAdd( aArq, { 'TIPO',    'C',  12, 0 } ) // POPUP MENUITEM  SEPARATOR
         AAdd( aArq, { 'CAPTIONP','C',  50, 0 } )
         AAdd( aArq, { 'CAPTIONE','C',  50, 0 } )
         AAdd( aArq, { 'CAPTIONI','C',  50, 0 } )
         AAdd( aArq, { 'NOME',    'C',  50, 0 } )
         AAdd( aArq, { 'ACTION',  'C', 200, 0 } )
         AAdd( aArq, { 'IMAGE',   'C', 100, 0 } )
         AAdd( aArq, { 'MESSAGEP','C', 100, 0 } )
         AAdd( aArq, { 'MESSAGEE','C', 100, 0 } )
         AAdd( aArq, { 'MESSAGEI','C', 100, 0 } )
         AAdd( aArq, { 'CHECKED', 'L',   1, 0 } )
         AAdd( aArq, { 'DISABLED','L',   1, 0 } )
         dbCreate ( CFILE, aArq )

         USE &C_DBF NEW
         INDEX ON FIELD->LINHA TO &C_DBF

         FOR F := 1 TO Len( aListItens ) - 1
            APPEND BLANK
            REPL LINHA    WITH StrZero( F, 6 )
            REPL ID       WITH aListItens[ f ][ 2 ]
            REPL MNUPAI   WITH aListItens[ f ][ 3 ]
            REPL TIPO     WITH aListItens[ f ][ 4 ]
            REPL CAPTIONP WITH aListItens[ f ][ 5 ]
            REPL CAPTIONE WITH aListItens[ f ][ 6 ]
            REPL CAPTIONI WITH aListItens[ f ][ 7 ]
            REPL NOME     WITH aListItens[ f ][ 8 ]
            REPL ACTION   WITH aListItens[ f ][ 9 ]
            REPL IMAGE    WITH aListItens[ f ][ 10 ]
            REPL MESSAGEP WITH aListItens[ f ][ 11 ]
            REPL MESSAGEE WITH aListItens[ f ][ 12 ]
            REPL MESSAGEI WITH aListItens[ f ][ 13 ]
            REPL CHECKED  WITH aListItens[ f ][ 14 ]
            REPL DISABLED WITH aListItens[ f ][ 15 ]
            COMMIT
         NEXT
         USE
         MSGINFO( "GRAVAÇÃO COMPLETA", "SALVAR COMO" )
      ENDIF
   ENDIF

RETURN

// ------------------------------------------------------------------------------------
PROCEDURE BtRemover_Click()

   LOCAL fpos := form1.Grid1.VALUE
   LOCAL ID   := aListItens[ fpos ][ 2 ]
   LOCAL Tipo := aListItens[ fpos ][ 4 ]
   LOCAL fim  := .F., ItemID

   ApagaUmItem( fpos )
   IF Tipo = "DEFINE POPUP"
      DO WHILE fim = .F.
         ItemID := aListItens[ fpos ][ 2 ] // Id desta linha
         ApagaUmItem( fpos )
         IF ItemID = ID
            fim := .T.
            // EXIT
         ENDIF
      ENDDO
   ENDIF
   AtualList()
   FORM1.Grid1.VALUE := fpos
   FORM1.Grid1.SETFOCUS()

RETURN
// ------------------------------------------------------------------------------------
procedure ApagaUmItem( fPos )

   LOCAL f

   FOR f := fpos + 1 TO Len( aListItens )
         aListItens[ f - 1 ] := { aListItens[ f ][ 1 ], aListItens[ f ][ 2 ], aListItens[ f ][ 3 ], ;
         aListItens[ f ][ 4 ], aListItens[ f ][ 5 ], aListItens[ f ][ 6 ], ;
         aListItens[ f ][ 7 ], aListItens[ f ][ 8 ], aListItens[ f ][ 9 ], ;
         aListItens[ f ][ 10 ], aListItens[ f ][ 11 ], aListItens[ f ][ 12 ], ;
         aListItens[ f ][ 13 ], aListItens[ f ][ 14 ], aListItens[ f ][ 15 ] }
   NEXT
   ASize( aListItens, Len( aListItens ) - 1 )

RETURN

// ------------------------------------------------------------------------------------
PROCEDURE Bt_Testar_Click()

   LOCAL nIdioma := QualIdioma()

   IF nIdioma = 0
      RETURN
   ELSE
      nIdioma -= 1
   ENDIF

   DEFINE WINDOW WinModal AT 0, 0 ;
         WIDTH 500 HEIGHT 300 ;
         TITLE "Menu DBF en tempo de execução" ;
         MODAL
      MontaMenu( nIdioma )
      DEFINE STATUSBAR
         STATUSITEM "" DEFAULT // area where the messages of the menu are shown
         CLOCK WIDTH 85
         DATE
      END STATUSBAR

   END WINDOW

   CENTER WINDOW WinModal
   ACTIVATE WINDOW WinModal

RETURN

// ------------------------------------------------------------------------------------
procedure MontaMenu( nIdioma )

   LOCAL f
   LOCAL cm_tipo
   LOCAL cm_caption
   LOCAL cm_name
   LOCAL cm_action
   LOCAL cm_image
   LOCAL cm_Message
   LOCAL cm_checked
   LOCAL cm_Disabled


   DEFINE MAIN MENU
      FOR f := 1 TO Len( aListItens ) - 1
         cm_tipo := aListItens[ f ][ 4 ]
         cm_caption := AllTrim( aListItens[ f ][ 5 + nIdioma ] )
         cm_name := AllTrim( aListItens[ f ][ 8 ] )
         cm_action := iif( EMPT( AllTrim( aListItens[ f ][ 9 ] ) ), NIL, aListItens[ f ][ 9 ] )
         cm_image := iif( EMPT( AllTrim( aListItens[ f ][ 10 ] ) ), NIL, AllTrim( aListItens[ f ][ 10 ] ) )
         cm_Message := iif( EMPT( AllTrim( aListItens[ f ][ 11 ] ) ), NIL, AllTrim( aListItens[ f ][ 11 + nIdioma ] ) )
         cm_checked := aListItens[ f ][ 14 ]
         cm_Disabled := aListItens[ f ][ 15 ]

         IF cm_tipo = "DEFINE POPUP"
            DEFINE POPUP cm_caption NAME cm_name
            ELSEIF cm_tipo = "MENUITEM"
               If ! cm_checked
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
      // Use
   END MENU

RETURN

// ------------------------------------------------------------------------------------
FUNCTION QualIdioma()

   LOCAL lRet := 0

   DEFINE WINDOW FrmIdioma AT 134, 235 WIDTH 269 HEIGHT 169 TITLE "Escolha o Idioma" MODAL NOSIZE NOSYSMENU
      DEFINE FRAME Frame_1
         ROW 10
         COL 10
         WIDTH 240
         HEIGHT 120
         FONTNAME "Arial"
         FONTSIZE 9
         FONTBOLD .F.
         FONTITALIC .F.
         FONTUNDERLINE .F.
         FONTSTRIKEOUT .F.
         CAPTION "Idioma"
      END FRAME

      DEFINE RADIOGROUP RgIdioma
         ROW 30
         COL 20
         WIDTH 90
         HEIGHT 75
         OPTIONS { 'Português', 'Espanhol', 'Inglês' }
         VALUE 1
         FONTNAME "Arial"
         FONTSIZE 9
         TOOLTIP ""
         FONTBOLD .F.
         FONTITALIC .F.
         FONTUNDERLINE .F.
         FONTSTRIKEOUT .F.
         SPACING 25
         BACKCOLOR NIL
         FONTCOLOR NIL
      END RADIOGROUP


      DEFINE BUTTON BtOk
         ROW 30
         COL 140
         WIDTH 100
         HEIGHT 28
         CAPTION "Ok"
         ACTION ( lret := frmIdioma.RgIdioma.VALUE, frmIdioma.release )
         FONTNAME "Arial"
         FONTSIZE 9
         TOOLTIP ""
         FONTBOLD .F.
         FONTITALIC .F.
         FONTUNDERLINE .F.
         FONTSTRIKEOUT .F.
      END BUTTON

      DEFINE BUTTON BtCancel
         ROW 70
         COL 140
         WIDTH 100
         HEIGHT 28
         CAPTION "Cancelar"
         ACTION frmIdioma.RELEASE
         FONTNAME "Arial"
         FONTSIZE 9
         FONTBOLD .F.
         FONTITALIC .F.
         FONTUNDERLINE .F.
         FONTSTRIKEOUT .F.
      END BUTTON

   END WINDOW
   frmIdioma.CENTER
   frmIdioma.ACTIVATE

RETURN lret

// ------------------------------------------------------------
// Ejemplo de definición de menú partiendo de una tabla
// (r) 2006, Roberto Sánchez
// ------------------------------------------------------------
FUNCTION Salir()

   RELEASE WINDOW WinModal

RETURN NIL

// ------------------------------------------------------------
// Ejemplo de definición de menú partiendo de una tabla
// (r) 2006, Roberto Sánchez
//Function that is not used
// ------------------------------------------------------------
FUNCTION AcercaDe()

   MsgInfo( cAcercaDe )

RETURN NIL
