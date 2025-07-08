//----------------------------------------------------------------------------------------------------------------------------//
// Application..: Ejemplo Clase TMP3Info - Devuelve información contenida en los Header y Frames de un MP3                    //
// FILE NAME....: TestMP3Info.prg                                                                                             //
// Author...... : Víctor Daniel Cuatecatl León                            San Cristóbal de las Casas,  Chiapas - Mexico.      //
// Date Created : 27 Julio 2019                                                                                               //
// Date Modified: 18 Agosto 2019                                                                                              //
// Copyright....: cuatecatl82       Soldisoft Software                                                                        //
// Email........: danyleon82@hotmail.com                                                                                      //
//----------------------------------------------------------------------------------------------------------------------------//

//----------------------------------------------------------------------------------------------------------------------------//
/* Adapted for MiniGUI Extended Edition by Grigory Filatov */

#include "minigui.ch"
#include "tsbrowse.ch"

#Define  LFN2SFN _GetShortPathName

SET PROCEDURE TO TMP3Info

STATIC oId3v23
STATIC nPos:= 0


//------------------------------------------------------------------------------------------------------------------------//
FUNCTION Main()

   LOCAL aLabels, aInitValues, aFormats

   aLabels	:= { 'File:',;
                     'Song Title:',;
                     'Performer(s):',;
                     'Album:',;
                     'Year:',;
                     'Track:',;
                     'Genre:',;
                     'Comment:',;
                     'Artist(s):',;
                     'Compositor:',;
                     'Disk:',;
                     'ID3 Version:',;
                     'Duration:',;
                     'Version:',;
                     'Layer:',;
                     'Compresion:',;
                     'Sampling:',;
                     'Channels:',;
                     'Padding:',;
                     'Copyright:',;
                     'CRC:',;
                     'Private:',;
                     'Original:',;
                     'Emphasis:',;
                     'Ext. JStr:',;
                     'Pos.Headr:';
                   }

   aInitValues	:= AFILL(ARRAY(LEN(aLabels)), "")

   aFormats	:= { 60, 60, 60, 60, 40, 40, 60, 60, 60, 60, 60, 40,;
                     40, 40, 60, 60, 60, 60, 60, 60, 60, 60, 40, 40, 40, 40, 40, 40, 40, 40 }

   MyInputWindow( "MP3 Info Class Test", aLabels, aInitValues, aFormats )

RETURN Nil


//------------------------------------------------------------------------------------------------------------------------//
Function MyInputWindow( Title, aLabels, aValues, aFormats )

   LOCAL i, l, ControlRow, LN, CN
   LOCAL aBmp:= ARRAY(5), nCol:= 0, N

	l := 12

	DEFINE WINDOW _InputWindow ;
		AT 0,0 ;
		WIDTH 610 ;
		HEIGHT (14*30) + 115 ;
		TITLE Title ;
		ICON "ICO00" ;
		MAIN ;
		NOSIZE ;
		FONT _GetSysFont() ; 
		SIZE 9 ;
		ON RELEASE iif(VALTYPE(oId3v23) == "O", FERASE( GetTempFolder() + "\mp3image" + oId3v23:cMimePic ), NIL)

		ControlRow:= 10

		For i:= 1 to l

			LN:= 'Label_' + hb_ntos(i)
			CN:= 'Control_' + hb_ntos(i)

			@ ControlRow, 10 LABEL &LN VALUE aLabels[i] WIDTH 85 HEIGHT 21 RIGHTALIGN VCENTERALIGN

			do case

			case ValType ( aValues [i] ) == 'C'

				If ValType ( aFormats [i] ) == 'N'
					If  i == 1
						@ ControlRow, 100 TEXTBOX &CN ;
						VALUE aValues[i] WIDTH 290 HEIGHT 21 ; 
						MAXLENGTH aFormats[i] ;
						ON ENTER MsgInfo(This.Value)
						ControlRow+= 30
					Else
						@ ControlRow, 100 TEXTBOX &CN ;
						VALUE aValues[i] WIDTH 290 HEIGHT 21 ;
						MAXLENGTH aFormats[i] ;
						ON ENTER SetCtrl()
						ControlRow+= 30
					EndIf
				EndIf

			endcase

		Next i

		@ ControlRow,20 SLIDER Slider_1 ;
		RANGE 0,100 ;
		VALUE nPos ;
		WIDTH 380 HEIGHT 21 ;
		NOTICKS

		DEFINE IMAGE Image_1
			PARENT            _InputWindow
			ROW               ControlRow + 28
			COL               100
			WIDTH             100
			HEIGHT            100
			PICTURE           "NOIMAGE"
			STRETCH           .T.
		END IMAGE

		@ ControlRow + 60, 400 LABEL Stars VALUE "Classification:" WIDTH 85 HEIGHT 21 RIGHTALIGN VCENTERALIGN

                FOR N:= 1 TO 5
                   aBmp[N] := "Star_" + hb_ntos(N)
                   @ControlRow + 60, 490 + nCol IMAGE &(aBmp[N]) PICTURE "" WIDTH 16 HEIGHT 16 TRANSPARENT
                   nCol+= 16
                NEXT

		@ ControlRow + 50, 240 BUTTON Play ;
		OF _InputWindow ;
		CAPTION '&Play MP3' ;
		ACTION NIL ;
		WIDTH 70 ;
		HEIGHT 24

		@ ControlRow + 50, 320 BUTTON Stop ;
		OF _InputWindow ;
		CAPTION '&Stop MP3' ;
		ACTION NIL ;
		WIDTH 70 ;
		HEIGHT 24

		ControlRow:= 10

		For i:= l + 1 to LEN(aLabels)

			LN:= 'Label_' + hb_ntos(i)
			CN:= 'Control_' + hb_ntos(i)

			@ ControlRow, 400 LABEL &LN VALUE aLabels[i] WIDTH 85 HEIGHT 21 RIGHTALIGN VCENTERALIGN

			do case

			case ValType ( aValues [i] ) == 'C'

				If ValType ( aFormats [i] ) == 'N'
					If  i == LEN(aLabels)
						@ ControlRow, 490 TEXTBOX &CN ;
						VALUE aValues[i] WIDTH 100 HEIGHT 21 ; 
						MAXLENGTH aFormats[i] ;
						ON ENTER _InputWindow.Button_3.Action
						ControlRow+= 25
					Else
						@ ControlRow, 490 TEXTBOX &CN ;
						VALUE aValues[i] WIDTH 100 HEIGHT 21 ;
						MAXLENGTH aFormats[i] ;
						ON ENTER SetCtrl()
						ControlRow+= 30
					EndIf
				EndIf

			endcase

		Next i

		@ ControlRow + 50, 240 BUTTON Button_1 ;
		OF _InputWindow ;
		CAPTION '&Load MP3' ;
		ACTION CargarMP31() ;
		WIDTH 70 ;
		HEIGHT 24

		@ ControlRow + 50, 320 BUTTON Button_2 ;
		OF _InputWindow ;
		CAPTION '&Info MP3' ;
		ACTION MasDatos(oId3v23) ;
		WIDTH 70 ;
		HEIGHT 24

		@ ControlRow + 50, 400 BUTTON Button_3 ;
		OF _InputWindow ;
		CAPTION '&About' ;
		ACTION MSGAbout(oId3v23) ;
		WIDTH 70 ;
		HEIGHT 24

		ON KEY ESCAPE ACTION ThisWindow.Release()

	END WINDOW

	_InputWindow.OnInit:= {|| _InputWindow.Button_1.Setfocus(), MITIMER(), DEACTIVATE TIMER Timer_1 OF _InputWindow}

        _InputWindow.Stars.Hide
	_InputWindow.Slider_1.BackColor := { 200, 200, 200 }
	_InputWindow.Slider_1.Enabled := .F.
	_InputWindow.Play.Enabled := .F.
	_InputWindow.Stop.Enabled := .F.
	_InputWindow.Button_2.Enabled := .F.
	_InputWindow.Button_3.Enabled := .F.

	CENTER WINDOW _InputWindow

	ACTIVATE WINDOW _InputWindow

Return Nil


//------------------------------------------------------------------------------------------------------------------------//
STATIC FUNCTION MITIMER()

	DEFINE TIMER Timer_1
		PARENT _InputWindow
		INTERVAL 1000
		ACTION (_InputWindow.Slider_1.Value:= ++nPos, IF(nPos == _InputWindow.Slider_1.RangeMax,;
                       (_InputWindow.Slider_1.Value:= nPos, HMG_SysWait(),;    // redraw slider Max value
                        DEACTIVATE TIMER Timer_1 OF _InputWindow, _InputWindow.Stop.Action), SysRefresh()))
	END TIMER

RETURN Nil



//------------------------------------------------------------------------------------------------------------------------//
STATIC FUNCTION CargarMP31()

   LOCAL aBmp:= ARRAY(5), nStars, N
   LOCAL cFileMP3:= GetFile({{"Archivo de Sonido en MP3   (*.mp3)", "*.mp3"}}, "Abrir Archivo de Mp3", GetCurrentFolder())

   _InputWindow.Button_1.Setfocus()

   IF !EMPTY(cFileMP3).AND. LOWER(cFileExt(cFileMP3)) == "mp3"

        _InputWindow.Stars.Hide
        FOR N:= 1 TO 5
            aBmp[N] := "Star_" + hb_ntos(N)
            _InputWindow.&(aBmp[N]).Hide
        NEXT   

	_InputWindow.Play.Enabled := .T.

        _InputWindow.Play.Action:= {|| ACTIVATE TIMER Timer_1 OF _InputWindow, _InputWindow.Slider_1.Value := 0,;
                            _InputWindow.Button_1.Enabled := .F., _InputWindow.Play.Enabled := .F., _InputWindow.Stop.Enabled := .T.,;
                            MCISENDSTR( "OPEN " + LFN2SFN(cFileMP3) + " TYPE MPEGVIDEO ALIAS MP3DEMO",, App.Handle ),;
                            MCISENDSTR( "PLAY MP3DEMO",0, App.Handle ), _InputWindow.Stop.Setfocus()}
                
        _InputWindow.Stop.Action:= {|| DEACTIVATE TIMER Timer_1 OF _InputWindow, nPos:= 0, _InputWindow.Slider_1.Value := nPos,;
                            _InputWindow.Button_1.Enabled := .T., _InputWindow.Stop.Enabled := .F., _InputWindow.Play.Enabled := .T.,;
                            MCISENDSTR("STOP MP3DEMO", 0, App.Handle),;
                            MCISENDSTR("CLOSE MP3DEMO",0, App.Handle), _InputWindow.Button_1.Setfocus()}

        oId3v23:= TMP3INFO():NEW()     
        oId3v23:Cargar(cFileMP3)

	IF oId3v23:lCargado

		oId3v23:lGuarPic:= .T.
		oId3v23:Cover()

		nStars:= oId3v23:CLASIFICACION()
        
		IF nStars >= 1
                   _InputWindow.Stars.Show
                   FOR N:= 1 TO nStars
                       aBmp[N] := "Star_" + hb_ntos(N)
                       _InputWindow.&(aBmp[N]).Picture:= "STARS"
                       _InputWindow.&(aBmp[N]).Show
                   NEXT   
		ENDIF

		_InputWindow.Slider_1.Enabled := .T.
        	_InputWindow.Slider_1.RangeMin:= 0
	        _InputWindow.Slider_1.RangeMax:= oId3v23:SEGUNDOS_ACTVX()

		_InputWindow.Button_2.Enabled := .T.
		_InputWindow.Button_3.Enabled := .T.
        
		_InputWindow.Control_1.Value := cFileMP3

		_InputWindow.Control_2.Value := oId3v23:TITULO()

		_InputWindow.Control_3.Value := oId3v23:INTERPRETE()

		_InputWindow.Control_4.Value := oId3v23:ALBUM()

		_InputWindow.Control_5.Value := oId3v23:FECHA()

		_InputWindow.Control_6.Value := cValToChar(oId3v23:PISTA())

		_InputWindow.Control_7.Value := oId3v23:GENERO()

		_InputWindow.Control_8.Value := oId3v23:COMENTARIO()

		_InputWindow.Control_9.Value := oId3v23:ARTISTA()

		_InputWindow.Control_10.Value := oId3v23:COMPOSITOR()

		_InputWindow.Control_11.Value := oId3v23:DISCO()

		_InputWindow.Control_12.Value := oId3v23:cID3Vers()

		_InputWindow.Control_13.Value := oId3v23:DURACION_ACTVX()

		_InputWindow.Control_14.Value := oId3v23:MPEGVERSION()

		_InputWindow.Control_15.Value := oId3v23:LAYER()

		_InputWindow.Control_16.Value := oId3v23:BYTERATE() + " Kb/s"

		_InputWindow.Control_17.Value := oId3v23:SAMPLING() + " Hz"

		_InputWindow.Control_18.Value := oId3v23:CANALES()

		_InputWindow.Control_19.Value := oId3v23:PADDING()

		_InputWindow.Control_20.Value := cValToChar(oId3v23:COPYRIGHT())

		_InputWindow.Control_21.Value := cValToChar(oId3v23:CRCPROTECTED())

		_InputWindow.Control_22.Value := cValToChar(oId3v23:PRIVATEBIT())

		_InputWindow.Control_23.Value := cValToChar(oId3v23:ORIGINAL())

		_InputWindow.Control_24.Value := cValToChar(oId3v23:EMPHASIS())

		_InputWindow.Control_25.Value := oId3v23:MODEXTENSION()

		_InputWindow.Control_26.Value := ALLTRIM( TRANSFORM(oId3v23:nBytsID3, "999,999,999") )

		IF oId3v23:lHayImag == .T.

			_InputWindow.Image_1.OnDblClick := {|| oId3v23:GUARDARCOVER(.T.)}
			_InputWindow.Image_1.Picture := GetTempFolder() + "\mp3image" + oId3v23:cMimePic

		ELSE

			_InputWindow.Image_1.OnDblClick := {|| NIL}
			_InputWindow.Image_1.Picture := "NOIMAGE"

		ENDIF

	ENDIF

   ENDIF

RETURN Nil


//------------------------------------------------------------------------------------------------------------------------//
STATIC FUNCTION MasDatos(oId3)

   IF !EMPTY(oId3) .AND. VALTYPE(oId3) == "O"
      _InputWindow.Button_2.Setfocus()
      MSGInfo( "Information: " + CRLF + oId3:INFO_ACTVX() + CRLF + CRLF+;
	"File Name: " + oId3:NOMARCH_ACTVX() + CRLF+;
	"File Size: " + oId3:TAMANO_ACTVX() + CRLF+;
	"File Type: " + oId3:TIPO_ACTVX() + CRLF+;
	"Modified: " + oId3:MODIFICADO_ACTVX() + CRLF+;
	"Created: " + oId3:CREADO_ACTVX() + CRLF+;
	"Last Access: " + oId3:ULTIMOACCESO_ACTVX() + CRLF+;
	"Attribute(s): " + oId3:ATTRIBUTO_ACTVX() + CRLF + CRLF+;
	"Performer(s): " + oId3:INTRPRT_ACTVX() + CRLF+;
	"Album: " + oId3:ALBUM_ACTVX() + CRLF+;
	"Year: " + oId3:FECHA_ACTVX() + CRLF+;
	"Genre: " + oId3:GENERO_ACTVX() + CRLF+;
	"Artist(s): " + oId3:ARTISTA_ACTVX() + CRLF+;
	"Song Title: " + oId3:TITULO_ACTVX() + CRLF+;
	"Comment: " + oId3:COMNTARIO_ACTVX() + CRLF+;
	"Track: " + oId3:PISTA_ACTVX() + CRLF+;
	"Duration: " + oId3:DURACION_ACTVX() + CRLF+;
	"Seconds: " + STR( oId3:SEGUNDOS_ACTVX() ) + CRLF+;
	"Compression:" + STRTRAN(oId3:COMPRESION_ACTVX(), "?", " "), oId3:ClassName() )
  ENDIF
 
RETURN Nil


//------------------------------------------------------------------------------------------------------------------------//
STATIC FUNCTION MSGAbout(oId3)

   IF !EMPTY(oId3) .AND. VALTYPE(oId3) == "O"
      _InputWindow.Button_3.Setfocus()
      oId3:BIOGRAFIA()
  ENDIF
   
RETURN Nil


//------------------------------------------------------------------------------------------------------------------------//
FUNCTION cFileExt( cPathMask )

   LOCAL cExt := AllTrim( cFileNoPath( cPathMask ) )
   LOCAL n    := RAt( ".", cExt )

RETURN AllTrim( If( n > 0 .AND. Len( cExt ) > n, Right( cExt, Len( cExt ) - n ), "" ) )


//------------------------------------------------------------------------------------------------------------------------//
FUNCTION SetCtrl()
   LOCAL ControlName := This.Name, cControl
   LOCAL nControl := Val( SubStr( ControlName, At("_", ControlName) + 1 ) )

   cControl := 'Control_' + hb_ntos( nControl + 1 )

RETURN DoMethod( '_InputWindow', cControl, 'SetFocus' )


#pragma BEGINDUMP

#define NO_LEAN_AND_MEAN

#include <mgdefs.h>

HB_FUNC ( MCISENDSTR )

{
   const char * bBuffer[ 255 ];

   hb_retnl( ( LONG ) mciSendString( ( LPSTR ) hb_parc( 1 ), ( LPSTR ) bBuffer, 254, ( HWND ) hb_parnl( 3 ) ) );

   hb_storc( ( const char * ) bBuffer, 2 );
}

#pragma ENDDUMP
