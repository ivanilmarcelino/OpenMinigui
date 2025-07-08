#include "minigui.ch"

MEMVAR cFrmName
MEMVAR cCmpName
MEMVAR lCaps, lNum, lAbc
MEMVAR cLetras
MEMVAR cNumeros
MEMVAR cEspeciais
MEMVAR aCamposNomes
MEMVAR lEspaco, cNmr

FUNCTION Main

   DEFINE WINDOW Form_Main ;
      AT 0, 0 ;
      WIDTH 640 HEIGHT 480 ;
      MAIN ;
      TITLE 'Exemplo de Teclado Virtual' ;
      ON INIT {|| Tela_Login() }

      DEFINE MAIN MENU

        POPUP 'Exemplo de Cadastro'

          ITEM 'Agenda' ACTION Cadastro()
          SEPARATOR
          ITEM 'Salir' ACTION Form_Main.Release()

        END POPUP

      END MENU

   END WINDOW

   CENTER WINDOW Form_Main

   ACTIVATE WINDOW Form_Main

RETURN Nil

//*********************************************************
FUNC Tela_Login()

   DEFINE window login ;
     WIDTH 351 HEIGHT 190 ;
     TITLE "Tela de Login" ;
     MODAL ;
     BACKCOLOR { 236, 233, 216 } ;
     ON INIT ShowTeclado()

     DEFINE IMAGE Image_1
            ROW    15
            COL    15
            WIDTH  72
            HEIGHT 87
            PICTURE "Lock"
     END IMAGE  

     DEFINE IMAGE Image_2
            ROW    116
            COL    15
            WIDTH  34
            HEIGHT 31
            PICTURE "Keyb"
            ACTION {|| ShowTeclado() }
     END IMAGE  

     DEFINE LABEL Label_1
            ROW    10
            COL    110
            WIDTH  120
            HEIGHT 15
            VALUE "Nome do Usuário"
            TRANSPARENT .T.
     END LABEL  

     DEFINE TEXTBOX oUsuario
            ROW    30
            COL    110
            WIDTH  200
            HEIGHT 20
            TOOLTIP "Informe o seu Nome do Usuário"
            ONENTER {|| login.oSenha.SetFocus, login.oSenha.CaretPos:=0 }
            MAXLENGTH 20
     END TEXTBOX 

     DEFINE LABEL Label_2
            ROW    60
            COL    110
            WIDTH  120
            HEIGHT 15
            VALUE "Senha de Acesso"
            TRANSPARENT .T.
     END LABEL  

     DEFINE TEXTBOX oSenha
            ROW    80
            COL    110
            WIDTH  100
            HEIGHT 20
            TOOLTIP "Informe a sua senha de acesso"
            ONENTER Ver_senha()
            UPPERCASE .T.
            PASSWORD .T.
     END TEXTBOX 

    DEFINE BUTTON Button_1
           ROW    120
           COL    100
           WIDTH  100
           HEIGHT 28
           ACTION Ver_senha()
           CAPTION "Entrar"
     END BUTTON  

    DEFINE BUTTON Button_2
           ROW    120
           COL    220
           WIDTH  100
           HEIGHT 28
           ACTION Sair_senha()
           CAPTION "Cancela"
     END BUTTON  

  END WINDOW

  CENTER WINDOW login

  ACTIVATE WINDOW login

RETURN Nil

//**********************************************
PROCEDURE Ver_senha()

   LOCAL cNome := login.oUsuario.Value
   LOCAL cSenha := login.oSenha.Value

   MsgInfo( "Nome: " + cNome + CRLF + "Senha: " + cSenha )

RETURN

//**********************************************
PROCEDURE Sair_senha()
   login.release

RETURN

//*************************************************************
FUNCTION ShowTeclado()

   LOCAL nRow := 5, nCol := 5, i_key, cButton, cLtr
   LOCAL nHeight_ := GetDesktopHeight() - 180, nWidth_ := GetDesktopWidth() - 365
   LOCAL n_tel_Row := Min( nHeight_, ( thiswindow.Row + thiswindow.Height ) + 5 ), n_tel_Col := Min( nWidth_, thiswindow.Col + ( thiswindow.Width/2 ) )

   IF ! IsWindowDefined ( Teclado_window )
      PRIVATE cFrmName := thiswindow.name
      PRIVATE cCmpName := this.focusedcontrol
      PRIVATE lCaps := .T. , lNum := .F. , lAbc := .T.
      PRIVATE cLetras := "QWERTYUIOPASDFGHJKLZXCVBNM"
      PRIVATE cNumeros := "1234567890@#$%&*()!\,.:;/?"
      PRIVATE cEspeciais := "~|-+=÷[]{}^_<>ºªçÇ§´`£½¼«»"
      PRIVATE aCamposNomes := _PegarConteudoCampos( cFrmName )
      PRIVATE lEspaco := .F. , cNmr
      SetProperty( cFrmName, cCmpName, "BackColor", { 255, 255, 192 } )

      DEFINE WINDOW Teclado_window ;
         AT  n_tel_row, n_tel_Col ;
         WIDTH  356 ;
         HEIGHT  146 ;
         TITLE  '' ; 
         MODAL  ;
         NOSIZE         ;
         NOSYSMENU ;
         NOCAPTION ;
         BACKCOLOR { 192, 192, 192 } ;
         ON MOUSEMOVE CursorSizeAll() ;
         ON MOUSECLICK MoveActiveWindow()
      
      DEFINE WINDOW Teclado_panel ;
         ROW 7 ;
         COL 7 ;
         WIDTH  Teclado_window.Width - 17 ;
         HEIGHT Teclado_window.Height - 17 ;
         WINDOWTYPE PANEL

      FOR i_key = 1 TO 26
         cNmr := "'" + AllTrim( Str( i_key,2,0 ) ) + "'"
         cButton := "Tecla_" + AllTrim( Str( i_key,2,0 ) )
         cLtr := "'" + SUBS( cLetras, i_key, 1 ) + "'"
         @ nRow, nCol BUTTONEX &cButton WIDTH 28 HEIGHT 28 ;
            ACTION {|| Tecla_Press( &cNmr ) } ;
            CAPTION &cLtr FONT 'Arial' BOLD
         nCol += 30
         IF i_key == 10
            nRow += 30
            nCol := 5
         ENDIF
         IF i_key == 19
            nRow += 30
            nCol := 65
         ENDIF
         IF i_key == 26
            nRow := 5
            nCol := 5
         ENDIF
      NEXT

      @ nRow, nCol + 300 BUTTONEX Tecla_Close WIDTH 28 HEIGHT 28 ;
         ACTION {|| Close_Press() } ;
         FONT 'Arial' BOLD PICTURE "Close"

      @ nRow + 30, nCol + 270 BUTTONEX Tecla_Enter WIDTH 58 HEIGHT 58 ;
         ACTION {|| Enter_Press() } ;
         FONT 'Arial' BOLD PICTURE "Enter"

      @ nRow + 60, nCol BUTTONEX Tecla_Caps WIDTH 58 HEIGHT 28 ;
         ACTION {|| Caps_Press() } ;
         CAPTION "Caps" FONT 'Arial' BOLD

      @ nRow + 90, nCol BUTTONEX Tecla_Num WIDTH 58 HEIGHT 28 ;
         ACTION {|| Num_Press() } ;
         CAPTION "123?" FONT 'Arial' BOLD

      @ nRow + 90, nCol + 60 BUTTONEX Tecla_Espaco WIDTH 148 HEIGHT 28 ;
         ACTION {|| Espaco_Press() } ;
         CAPTION "Espaço" FONT 'Arial' BOLD

      @ nRow + 90, nCol + 210 BUTTONEX Tecla_Back WIDTH 58 HEIGHT 28 ;
         ACTION {|| Back_Press() } ;
         CAPTION "Back" FONT 'Arial' BOLD

      @ nRow + 90, nCol + 270 BUTTONEX Tecla_Limpar WIDTH 58 HEIGHT 28 ;
         ACTION {|| Limpar_Press() } ;
         CAPTION "Limpar" FONT 'Arial' BOLD

      END WINDOW

      END WINDOW

      ACTIVATE WINDOW Teclado_window

   ENDIF

RETURN Nil


//*************************************************************
#define HTCAPTION          2
#define WM_NCLBUTTONDOWN   161

PROCEDURE MoveActiveWindow( hWnd )
   DEFAULT hWnd := GetActiveWindow()

   PostMessage( hWnd, WM_NCLBUTTONDOWN, HTCAPTION, 0 )

   CursorSizeAll()

RETURN

//*************************************************************
FUNC Tecla_Press( nTecla_ )

   LOCAL cCmpValue := GetProperty( cFrmName, cCmpName, "Value" )
   LOCAL nCmpPos  := GetProperty( cFrmName, cCmpName, "CaretPos" )

   IF lEspaco
      cCmpValue += " "
      nCmpPos += 1
      lEspaco := .F.
   ENDIF
   IF lAbc
      cCmpValue := Stuff( cCmpValue, nCmpPos + 1, 0, SUBS( IF(lCaps,cLetras,Lower(cLetras ) ),Val(nTecla_ ),1 ) )
   ELSE
      IF lNum
         cCmpValue := Stuff( cCmpValue, nCmpPos + 1, 0, SUBS( cNumeros,Val(nTecla_ ),1 ) )
      ELSE
         cCmpValue := Stuff( cCmpValue, nCmpPos + 1, 0, SUBS( cEspeciais,Val(nTecla_ ),1 ) )
      ENDIF
   ENDIF
   SetProperty( cFrmName, cCmpName, "Value", cCmpValue )
   SetProperty( cFrmName, cCmpName, "CaretPos", nCmpPos + 1 )

RETURN .T.

//*************************************************************
FUNC Close_Press()
   IF IsWindowDefined ( Teclado_window )
      SetProperty( cFrmName, cCmpName, "BackColor", { 255, 255, 255 } )
      Teclado_window.Release
      SETFOCUS &( cCmpName ) OF &( cFrmName )
   ENDIF

RETURN .T.

//*************************************************************
FUNC Enter_Press()

   LOCAL lSai := .F. , nCmp_, CtrlName

   FOR nCmp_ := 1 TO Len( aCamposNomes )
      CtrlName := aCamposNomes[nCmp_]
      IF lSai
         EXIT
      ELSE
         IF CtrlName == cCmpName
            lSai := .T.
         ENDIF
      ENDIF
   NEXT
   SetProperty( cFrmName, cCmpName, "BackColor", { 255, 255, 255 } )
   DoMethod( cFrmName, CtrlName, "SetFocus" )
   cCmpName := CtrlName
   SetProperty( cFrmName, cCmpName, "BackColor", { 255, 255, 192 } )

RETURN .T.

//*************************************************************
FUNC Caps_Press()

   LOCAL i_key, cCmp_, cLtr

   IF lAbc
      lCaps := !lCaps
      FOR i_key = 1 TO 26
         cCmp_ := "Tecla_" + AllTrim( Str( i_key,2,0 ) )
         cLtr := SUBS( cLetras, i_key, 1 )
         SetProperty( "Teclado_panel", cCmp_, "Caption", IF( lCaps,cLtr,Lower(cLtr ) ) )
      NEXT
      SetProperty( "Teclado_panel", "Tecla_Caps", "Caption", "Caps" )
   ELSE
      lNum := !lNum
      FOR i_key = 1 TO 26
         cCmp_ := "Tecla_" + AllTrim( Str( i_key,2,0 ) )
         cLtr := SUBS( IF( lNum,cNumeros,cEspeciais ), i_key, 1 )
         IF cLtr == '&'
            cLtr := '&&'
         ENDIF
         SetProperty( "Teclado_panel", cCmp_, "Caption", cLtr )
      NEXT
      SetProperty( "Teclado_panel", "Tecla_Caps", "Caption", IF( lNum,"+=<","123?" ) )
   ENDIF

RETURN .T.

//*************************************************************
FUNC Num_Press()
   IF lAbc
      lAbc := .F.
      lNum := .F.
      Caps_Press()
      SetProperty( "Teclado_panel", "Tecla_Num", "Caption", "ABC" )
   ELSE
      lAbc := .T.
      lCaps := !lCaps
      Caps_Press()
      SetProperty( "Teclado_panel", "Tecla_Num", "Caption", "123?" )
   ENDIF

RETURN .T.

//*************************************************************
FUNC Espaco_Press()

   LOCAL cCmpValue := GetProperty( cFrmName, cCmpName, "Value" )
   LOCAL nCmpPos  := GetProperty( cFrmName, cCmpName, "CaretPos" )

   IF nCmpPos < Len( cCmpValue )
      cCmpValue := Stuff( cCmpValue, nCmpPos + 1, 0, " " )
      SetProperty( cFrmName, cCmpName, "Value", cCmpValue )
      SetProperty( cFrmName, cCmpName, "CaretPos", nCmpPos + 1 )
      lEspaco := .F.
   ELSE
      lEspaco := .T.
   ENDIF

RETURN .T.

//*************************************************************
FUNC Back_Press()

   LOCAL cCmpValue := GetProperty( cFrmName, cCmpName, "Value" )
   LOCAL nCmpPos  := GetProperty( cFrmName, cCmpName, "CaretPos" )

   cCmpValue := Stuff( cCmpValue, nCmpPos, 1, "" )
   SetProperty( cFrmName, cCmpName, "Value", cCmpValue )
   SetProperty( cFrmName, cCmpName, "CaretPos", nCmpPos - 1 )

RETURN .T.

//*************************************************************
FUNC Limpar_Press()
   SetProperty( cFrmName, cCmpName, "Value", "" )
   SetProperty( cFrmName, cCmpName, "CaretPos", 0 )

RETURN .T.

//*************************************************************
FUNCTION _PegarConteudoCampos( cFormName )

   LOCAL nFormHandle , i , nControlCount , aRetVal := {} , x

   nFormHandle := GetFormHandle ( cFormName )
   nControlCount := Len ( _HMG_aControlHandles )
   FOR i := 1 TO nControlCount
      IF ( _HMG_aControlType[i] $ [NUMTEXT-TEXT-MASKEDTEXT-CHARMASKTEXT-GETBOX] )
         IF _HMG_aControlParentHandles[i] == nFormHandle
            IF ValType( _HMG_aControlHandles[i] ) == 'N'
               IF ! Empty( _HMG_aControlNames[i] )
                  IF AScan( aRetVal, _HMG_aControlNames[i] ) == 0
                     AAdd( aRetVal, _HMG_aControlNames[i] )
                  ENDIF
               ENDIF
            ELSEIF ValType( _HMG_aControlHandles [i] ) == 'A'
               FOR x := 1 TO Len ( _HMG_aControlHandles[i] )
                  IF !Empty( _HMG_aControlNames[i] )
                     IF AScan( aRetVal, _HMG_aControlNames[i] ) == 0
                        AAdd( aRetVal, _HMG_aControlNames [i] )
                     ENDIF
                  ENDIF
               NEXT x
            ENDIF
         ENDIF
      ENDIF
   NEXT i

RETURN aRetVal

//******************************************************************
FUNCTION Cadastro()

   LOCAL cCodigo := ""
   LOCAL cNome := " "
   LOCAL cEndereco := " "
   LOCAL cBairro := " "
   LOCAL cCep := " "
   LOCAL cCidade := " "
   LOCAL cEstado := " "
   LOCAL cFone1 := " "
   LOCAL cFone2 := " "
   LOCAL cEmail := " "
	
   DEFINE WINDOW Form_2   ;
      AT 0, 0 ;
      WIDTH 490 ;
      HEIGHT 300 ;
      TITLE "Agenda de Contatos" ; 
      MODAL ;                
      NOSIZE ;
      BACKCOLOR { 236, 233, 216 } ;
      ON INIT ShowTeclado()
	
	             @ 15,10 LABEL Label_Codigo	;
		          VALUE 'Código'	;
		          AUTOSIZE		;
		          TRANSPARENT		;
                          FONT 'Arial' SIZE 09 

	             @ 45,10 LABEL Label_Nome	;
		            VALUE 'Nome'	;
		            AUTOSIZE		;
		            TRANSPARENT		;
	                    FONT 'Arial' SIZE 09

	             @ 75,10 LABEL Label_Endereco	;
		            VALUE 'Endereço'	;
		            AUTOSIZE		;
		            TRANSPARENT		;
                            FONT 'Arial' SIZE 09

	             @105,10 LABEL Label_Bairro	;
		            VALUE 'Bairro'	;
		            AUTOSIZE		;
		            TRANSPARENT		;
	                    FONT 'Arial' SIZE 09

	             @105,360 LABEL Label_Cep	;
		            VALUE 'Cep'		;
		            AUTOSIZE		;
		            TRANSPARENT		;
	                    FONT 'Arial' SIZE 09

	             @135,10 LABEL Label_Cidade	;
		            VALUE 'Cidade'	;
		            AUTOSIZE		;
		            TRANSPARENT		;
	                    FONT 'Arial' SIZE 09

	             @135,345 LABEL Label_Estado;
		            VALUE 'Estado'	;
		            AUTOSIZE		;
		            TRANSPARENT		;
	                    FONT 'Arial' SIZE 09

	             @165,10 LABEL Label_Fone1	;
		            VALUE 'Fone 1'	;
		            AUTOSIZE		;
		            TRANSPARENT		;
	                    FONT 'Arial' SIZE 09

	             @165,346 LABEL Label_Fone2	;
		            VALUE 'Fone 2'	;
		            AUTOSIZE		;
		            TRANSPARENT		;
	                    FONT 'Arial' SIZE 09

	             @195,10 LABEL Label_Email	;
		            VALUE 'e-mail'	;
		            AUTOSIZE		;
		            TRANSPARENT		;
	                    FONT 'Arial' SIZE 09

	             @ 13,70 TEXTBOX T_Codigo	;
		             WIDTH 40		;
                             VALUE cCodigo	;
		             TOOLTIP 'Código do Contato' ;
                             INPUTMASK '9999'

		     @ 43,70 TEXTBOX T_Nome	;	
		              OF Form_2		;
		              WIDTH 400		;
		              VALUE cNome	;
		              TOOLTIP 'Nome do Contato'	;
		              MAXLENGTH 40		;
		              UPPERCASE
 
                     @ 73,70 TEXTBOX T_Endereco	;
		              OF Form_2		;
	                      WIDTH 400		;
	                      VALUE cEndereco	;
  	                      TOOLTIP 'Endereço do Contato';
	                      MAXLENGTH 40	;
	                      UPPERCASE

                     @103,70 TEXTBOX T_Bairro	;	
		              OF Form_2		; 
		              WIDTH 250		;
		              VALUE cBairro	;
		              TOOLTIP 'Bairro do Contato'	;
		              MAXLENGTH 25	;
		              UPPERCASE

                    @103,390 TEXTBOX T_Cep  	;
                              OF Form_2		;	
		              WIDTH 80		;
		              VALUE cCep	;
		              TOOLTIP 'Cep do Contato'	;
		              MAXLENGTH 08	;
		              UPPERCASE 

                    @133,70 TEXTBOX T_Cidade	;
		              OF Form_2		;
		              WIDTH 250		;
		              VALUE cCidade	;
		              TOOLTIP 'Bairro do Contato'	;
		              MAXLENGTH 25	;
		              UPPERCASE

                    @133,390 TEXTBOX T_Estado	;
		              OF Form_2		;
		              WIDTH 30		;
		              VALUE cEstado	;
		              TOOLTIP 'Estado do Contato';
		              MAXLENGTH 02	;
		              UPPERCASE

                    @163,70 TEXTBOX T_Fone1	;
	                      OF Form_2		;
		              WIDTH 110		;
		              VALUE cFone1	;
		              TOOLTIP 'Telefone do Contato' ;
		              MAXLENGTH 10 

                    @163,390 TEXTBOX T_Fone2	;
		              OF Form_2		;
		              WIDTH 80		;
		              VALUE cFone2	;
		              TOOLTIP 'Telefone do Contato';
		              MAXLENGTH 10

                    @193,70 TEXTBOX T_Email	;
	                      OF Form_2		;
		              WIDTH 400		;
		              VALUE cEmail	;
		              TOOLTIP 'E-mail do Contato'	;
		              MAXLENGTH 40	;
		              LOWERCASE

	            @ 232,70 BUTTON Btn_Salvar Of Form_2	;
		              CAPTION '&Salvar'			;
			      WIDTH 120 HEIGHT 27		;
		              FONT "Arial" SIZE 09		;
		              ACTION MsgInfo("Salvar")		;
                              TOOLTIP "Salvar Registro"		;
			      FLAT       

	            @ 232,210 BUTTON Btn_Excluir Of Form_2	;
		              CAPTION '&Deletar'		;
			      WIDTH 120 HEIGHT 27		;
		              FONT "Arial" SIZE 09		;
		              ACTION MsgInfo("Deletar")		;
                              TOOLTIP "Excluir Registro"	;
			      FLAT

	            @ 232,346 BUTTON Btn_Cancelar Of Form_2	;
		              CAPTION '&Cancelar'		;
			      WIDTH 120 HEIGHT 27		;
		              FONT "Arial" SIZE 09		;
		              ACTION thiswindow.release()	;
                              TOOLTIP "Cancelar Operação"	;
			      FLAT

                   DEFINE IMAGE Image_2
                      ROW    230
                      COL    20
                      WIDTH  34
                      HEIGHT 31
                      PICTURE "Keyb"
                      ACTION {|| ShowTeclado() }
                   END IMAGE  

   END WINDOW

   CENTER WINDOW   Form_2
   ACTIVATE WINDOW Form_2

RETURN Nil
