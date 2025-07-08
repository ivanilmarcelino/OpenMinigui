//----------------------------------------------------------------------------------------------------------------------------//
// Application..: Clase TMP3Info - Devuelve información contenida en los Header y Frames de un MP3                            //
// File Name....: TMP3Info.prg                                                                                                //
// Author...... : Víctor Daniel Cuatecatl León                            San Cristóbal de las Casas,  Chiapas - Mexico.      //
// Date Created : 27 Julio 2019                                                                                               //
// Date Modified: 18 Agosto 2019                                                                                              //
// Copyright....: cuatecatl82       Soldisoft Software                                                                        //
// Email........: danyleon82@hotmail.com                                                                                      //
//----------------------------------------------------------------------------------------------------------------------------//

/* This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation;  either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but  WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE. See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this software; see the file COPYING.  If not, write to
   the Free Software Foundation,  Inc., 59 Temple  Place, Suite 330,
   Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
*/


//----------------------------------------------------------------------------------------------------------------------------//
/* Adapted for MiniGUI Extended Edition by Grigory Filatov */


#include "minigui.ch"
#include "tsbrowse.ch"
#Include "hbclass.ch"

#Define  LFN2SFN _GetShortPathName

#Define  TRESBYTES   "494433"                //       Los Primeros 3 Bytes de un MP3 con TAGID3 son siempre 0x49 0x44 0x33
#Define  ULTIBYTES   128                     //       Los Ultimos 128 Bytes son del TAG 
#Define  MP3CLASSE   "Clase ID3v123"         //       Titulo para los Mensajes de Aviso



//----------------------------------------------------------------------------//
CLASS TMP3INFO

   DATA   cMp3File  AS CHARACTER             /*       Nombre del Archivo MP3 a Analizar                                  */
   
   DATA   nHndlMp3  AS NUMERIC    HIDDEN     /*       Handle de Control de Apertura del MP3 y Núcleo de la Clase         */
   DATA   lCargado  AS LOGICAL    READONLY   /*       Si se ha cargado con exito un MP3                                  */
   DATA   oObjtMP3  AS OBJECT     HIDDEN     /*       Objeto Contedenedor de más Detalles del MP3 Obtenida de ActiveX    */
   DATA   nBinrMP3  AS CHARACTER  READONLY   /*       Cadena Binaria de 32 Bits (4 Bytes 0x00 * 8) del Header Frame MP3  */
   
   DATA   nBytsID3  AS NUMERIC    READONLY   /* \     Numero de Bytes Totales que componen el TAG Id3v2.X                */
   DATA   cBytsID3  AS CHARACTER  READONLY   /* >|    Caracteres Binarios del TAG                                        */
   DATA   cFileCdn  AS CHARACTER  HIDDEN     /* >|    Caracteres ASCII del TAG                                           */
   DATA   cID3Vers  AS CHARACTER  READONLY   /* /     Version del TAG encontrado puede ser 2.3.0  o 2.4.0                */

   DATA   cMimePic  AS CHARACTER  READONLY   /* \     Formato de Imagen dentro del MP3                                   */
   DATA   nByteIma  AS NUMERIC    READONLY   /* >|    Numero de Bytes solo de la Imagen CoverArt                         */
   DATA   bByteIma  AS CHARACTER  READONLY   /* /     Contenido en Binario de la Imagen                                  */

   DATA   cTipoCvr  AS CHARACTER  READONLY   /* \     Tipo de Imagen del CoverArt (Frontal, Trasera, CD)                 */
   DATA   cDescCvr  AS CHARACTER  READONLY   /* \     Descripción de la Imagen CoverArt (Solo ID3v2.4)                   */

   DATA   lGuarPic  AS LOGICAL               /*       Para Controlar si se Guarda o no la Imaágen extraida del MP3       */
   DATA   lHayImag  AS LOGICAL    READONLY   /*       Para Saber si se encontro un CoverArt dentro del MP3               */


   METHOD NEW() CONSTRUCTOR

   METHOD CARGAR(cMp3File)
   METHOD ERROR(nError)
   METHOD LEER_ARCHIVO()

   METHOD LEER_ID3()   
   METHOD BYTE2ASCII(nPost, nLong)

   METHOD ID3v_220_230(cHdrSz)
   METHOD ID3v10_11()

   METHOD TITULO()
   METHOD INTERPRETE()
   METHOD ALBUM()
   METHOD FECHA()
   METHOD PISTA()
   METHOD GENERO()
   METHOD COMENTARIO()
   METHOD ARTISTA()
   METHOD COMPOSITOR()
   METHOD DISCO()

   METHOD COVER()
   METHOD GUARDARCOVER(lRes)
   METHOD CALCULAR(aByts)
   METHOD DETALLESMP3()

   METHOD BIOGRAFIA()

   /*     Datos Obtenidos desde Los Bytes del Header    */

   METHOD MPEGVERSION()
   METHOD LAYER()
   METHOD CRCPROTECTED()
   METHOD BYTERATE()
   METHOD SAMPLING()
   METHOD PADDING()
   METHOD PRIVATEBIT()
   METHOD CANALES()
   METHOD MODEXTENSION()
   METHOD COPYRIGHT()
   METHOD ORIGINAL()
   METHOD EMPHASIS()
   METHOD CLASIFICACION()

   /*     Datos Obtenidos desde ActiveX     */

   METHOD INFO_ACTVX()          INLINE  ::oObjtMP3:GetDetailsOf( ::oObjtMP3:Parsename( cFileName(::cMp3File) ),  -1 )
   METHOD NOMARCH_ACTVX()       INLINE  ::oObjtMP3:GetDetailsOf( ::oObjtMP3:Parsename( cFileName(::cMp3File) ),   0 )
   METHOD TAMANO_ACTVX()        INLINE  ::oObjtMP3:GetDetailsOf( ::oObjtMP3:Parsename( cFileName(::cMp3File) ),   1 )
   METHOD TIPO_ACTVX()          INLINE  ::oObjtMP3:GetDetailsOf( ::oObjtMP3:Parsename( cFileName(::cMp3File) ),   2 )
   METHOD MODIFICADO_ACTVX()    INLINE  ::oObjtMP3:GetDetailsOf( ::oObjtMP3:Parsename( cFileName(::cMp3File) ),   3 )
   METHOD CREADO_ACTVX()        INLINE  ::oObjtMP3:GetDetailsOf( ::oObjtMP3:Parsename( cFileName(::cMp3File) ),   4 )
   METHOD ULTIMOACCESO_ACTVX()  INLINE  ::oObjtMP3:GetDetailsOf( ::oObjtMP3:Parsename( cFileName(::cMp3File) ),   5 )
   METHOD ATTRIBUTO_ACTVX()     INLINE  ::oObjtMP3:GetDetailsOf( ::oObjtMP3:Parsename( cFileName(::cMp3File) ),   6 )

   METHOD INTRPRT_ACTVX()       INLINE  ::oObjtMP3:GetDetailsOf( ::oObjtMP3:Parsename( cFileName(::cMp3File) ),  13 )
   METHOD ALBUM_ACTVX()         INLINE  ::oObjtMP3:GetDetailsOf( ::oObjtMP3:Parsename( cFileName(::cMp3File) ),  14 )
   METHOD FECHA_ACTVX()         INLINE  ::oObjtMP3:GetDetailsOf( ::oObjtMP3:Parsename( cFileName(::cMp3File) ),  15 )
   METHOD GENERO_ACTVX()        INLINE  ::oObjtMP3:GetDetailsOf( ::oObjtMP3:Parsename( cFileName(::cMp3File) ),  16 )
   METHOD ARTISTA_ACTVX()       INLINE  ::oObjtMP3:GetDetailsOf( ::oObjtMP3:Parsename( cFileName(::cMp3File) ),  20 )
   METHOD TITULO_ACTVX()        INLINE  ::oObjtMP3:GetDetailsOf( ::oObjtMP3:Parsename( cFileName(::cMp3File) ),  21 )

   METHOD COMNTARIO_ACTVX()     INLINE  ::oObjtMP3:GetDetailsOf( ::oObjtMP3:Parsename( cFileName(::cMp3File) ),  24 )
   METHOD PISTA_ACTVX()         INLINE  ::oObjtMP3:GetDetailsOf( ::oObjtMP3:Parsename( cFileName(::cMp3File) ),  26 )
   METHOD DURACION_ACTVX()      INLINE  ::oObjtMP3:GetDetailsOf( ::oObjtMP3:Parsename( cFileName(::cMp3File) ),  27 )
   METHOD SEGUNDOS_ACTVX()      INLINE    Secs( ::DURACION_ACTVX() )
   METHOD COMPRESION_ACTVX()    INLINE  ::oObjtMP3:GetDetailsOf( ::oObjtMP3:Parsename( cFileName(::cMp3File) ),  28 )


ENDCLASS




//----------------------------------------------------------------------------//
METHOD NEW() CLASS TMP3INFO

   ::cMp3File:= ""
   ::cID3Vers:= ""
   ::cFileCdn:= ""
   ::lCargado:= .F.
   ::nHndlMp3:= -1
   ::nBytsID3:= 0
   ::cBytsID3:= ""
   ::cMimePic:= ""
   ::lGuarPic:= .T.
   ::lHayImag:= .F.
   ::nByteIma:= 0
   ::bByteIma:= ""
   ::oObjtMP3:= Nil
   ::nBinrMP3:= ""
   ::cTipoCvr:= ""
   ::cDescCvr:= ""

RETURN Self




//----------------------------------------------------------------------------//
METHOD CARGAR(cMp3File) CLASS TMP3INFO

	LOCAL cPathMP3:= LFN2SFN(cMp3File)
	LOCAL nError

       IF FILE(cPathMP3)

          ::cFileCdn:= MEMOREAD(cPathMP3)
          ::nHndlMp3:= FOPEN( cPathMP3, 0 )
          SysRefresh()

          nError:= FERROR()

          ::ERROR(nError)

          IF !Empty( ::cFileCdn ) .AND. ::nHndlMp3 <> -1 .AND. nError == 0    //  Si no se han producido errores de Lectura del Archivo

             ::lCargado:= .T.
             ::cMp3File:= cPathMP3

             ::LEER_ARCHIVO()
             ::DETALLESMP3()

             FCLOSE(::nHndlMp3)
            
          ENDIF

       ENDIF

RETURN Nil




//----------------------------------------------------------------------------//
METHOD LEER_ARCHIVO() CLASS TMP3INFO

	LOCAL N, cStrBin, cMP3Hdr:= ""
	LOCAL cEsMp3ID3:= SPACE(03)
	LOCAL nID3Versn:= SPACE(01)
	LOCAL nSubVersn:= SPACE(01)
	LOCAL cHedrSize:= SPACE(04)
	LOCAL cFrameHdr:= SPACE(04)

	LOCAL cVerId3, cSubVer

           FSEEK(::nHndlMp3,0,0)            //         0x49 0x44 0x33
           FREAD(::nHndlMp3,@cEsMp3ID3,3)   //$01-$03  "I    D    3"      ->   Etiqueta
           FSEEK(::nHndlMp3,3,0)
           FREAD(::nHndlMp3,@nID3Versn,1)   //$04         "2 o 3"         ->   Version
           FSEEK(::nHndlMp3,4,0)
           FREAD(::nHndlMp3,@nSubVersn,1)   //$05           "0"           ->   SubVersion
           FSEEK(::nHndlMp3,6,0)
           FREAD(::nHndlMp3,@cHedrSize,4)   //$07-$10  0x00 00 00 00      ->   Tamaño Total del TAG ID3

        IF StrToHex(@cEsMp3ID3) == TRESBYTES                      // Es un Mp3 con ID3 Ver 2.x pues esta al inicio del Archivo
           cVerId3:= ASC(nID3Versn)
           cSubVer:= ASC(nSubVersn)

           ::cID3Vers:= "2."+ cValtoChar(cVerId3) +"."+ cValtoChar(cSubVer) // Buscamos la Version ya sea ID3v2.0  2.3  o 2.4
           ::ID3v_220_230(@cHedrSize)

        ELSE
           ::ID3v10_11()                                        // Buscamos si existe un Id3v1.0 o Id3v1.1
        ENDIF   

        FSEEK(::nHndlMp3,::nBytsID3,0)
        FREAD(::nHndlMp3,@cFrameHdr,4)

       FOR N:= 1 TO 4                                                //  Los 4 Bytes del Tamaño del Header Frame del MP3
           cStrBin:= SUBSTR(@cFrameHdr,N,1)
           cMP3Hdr:= cMP3Hdr + STRZERO(  VAL( DecToBin( HexToNum( StrToHex(cStrBin) ) ) ) , 8 )
       NEXT

       ::nBinrMP3:= cMP3Hdr

RETURN Nil
            
              
           
           
//----------------------------------------------------------------------------//           
METHOD ID3v_220_230(cHdrSz)  CLASS TMP3INFO

	LOCAL nBytesMP3, nEntSeg
	LOCAL bBit1,bBit2,bBit3,bBit4
	LOCAL cBytesMP3:= StrToHex(cHdrSz)               // En busqueda del Entero Seguro dentro de los 4 bytes

        bBit1:= STRZERO( VAL( DecToBin( HexToNum( SUBSTR(cBytesMP3,1,2) ) ) ), 7 )  // (*7) Revisar la Formula en Biografia.
        bBit2:= STRZERO( VAL( DecToBin( HexToNum( SUBSTR(cBytesMP3,3,2) ) ) ), 7 )  // Me llevó una semana investigar y probar
        bBit3:= STRZERO( VAL( DecToBin( HexToNum( SUBSTR(cBytesMP3,5,2) ) ) ), 7 )  // sin éxito, hasta que por fin la encontre
        bBit4:= STRZERO( VAL( DecToBin( HexToNum( SUBSTR(cBytesMP3,7,2) ) ) ), 7 )  // el dia de mi Cumple, ¡¡ Fue un Excelente AutoRegalo!!

        nEntSeg:= bBit1 + bBit2 + bBit3 + bBit4       // Tamaño Total del ID3v2.3.0 dentro del MP3

        nBytesMP3:= BinToDec( nEntSeg ) + 10 // Más los Primeros 10 Bytes ( $01 al $10 ) ID3vsfSIZE

        ::nBytsID3:= nBytesMP3

        ::cBytsID3:= ::LEER_ID3()

RETURN Nil




//----------------------------------------------------------------------------//           
METHOD ID3v10_11()  CLASS TMP3INFO

   LOCAL nPosFrm0:= RAT("TAG", ::cFileCdn)
   LOCAL N, cVal

   IF nPosFrm0 > 0

      ::cID3Vers:= "1.0"                                   // Es una version 1 con SubVersion 0 ( 1.0 )  ID3v1.0

      FOR N:= 1 TO ULTIBYTES
         cVal:= SUBSTR(::cFileCdn, (nPosFrm0 + N + 2), 1)    // + 2 Por "TAG" 
         ::cBytsID3:= ::cBytsID3 + cVal
      NEXT  

   ENDIF

RETURN Nil




//----------------------------------------------------------------------------//
METHOD LEER_ID3() CLASS TMP3INFO

   LOCAL cBuffer:= SPACE(::nBytsID3)

   FSEEK(::nHndlMp3,0,0)                       // Pasamos el Puntero al Inicio del Archivo
   FREAD(::nHndlMp3,@cBuffer,::nBytsID3)       // Leemos todos los Bytes indicados por ::nBytsID3

RETURN cBuffer




//----------------------------------------------------------------------------//
METHOD BYTE2ASCII(nPost, nLong) CLASS TMP3INFO

   LOCAL N, cBit, nChr, cBuffer:= ""

   FOR N:= 1 TO nLong
      cBit:= SUBSTR(::cFileCdn,nPost,1)
      nChr:= HexToNum( StrToHex(cBit) )

      IF nChr >= 32 .AND. nChr <= 175       // Que sea Carácter en ASCII 
         cBuffer:= cBuffer + cBit
      ENDIF

      nPost:= nPost + 1
   NEXT

RETURN cBuffer




//----------------------------------------------------------------------------//
METHOD CALCULAR(aByts)  CLASS TMP3INFO

LOCAL aBtImg1:= StrToHex( aByts )

RETURN HexToNum( aBtImg1 )




//----------------------------------------------------------------------------//
METHOD INTERPRETE() CLASS TMP3INFO

LOCAL cInfoFrm, nPosFrm1, nPosFrm2
LOCAL nPosFrm0, bSizeFrm, nSizeFrm

IF ::cID3Vers == "2.3.0" .OR. ::cID3Vers == "2.4.0"

   nPosFrm0:= AT("TPE1", ::cBytsID3)

   IF nPosFrm0 > 0
      nPosFrm1:= nPosFrm0 + 4                      // + 4 ( 4 por "TPE1" )   
      bSizeFrm:= SUBSTR(::cBytsID3, nPosFrm1, 4)   // El Tamaño del Frame esta en estos 4 Bytes, hay que decoficicar
      nSizeFrm:= ::CALCULAR(bSizeFrm)
      nPosFrm2:= nPosFrm1 + 6                      // 4 por "TPE1" + 2 Por los Bytes de Flags, pueden haber mas de 2, hay que analizar
      cInfoFrm:= ::BYTE2ASCII(nPosFrm2, nSizeFrm)  // Se Extrae la Cadena Completa
   ENDIF

ELSEIF ::cID3Vers == "2.2.0"

       nPosFrm0:= AT("TP1", ::cBytsID3)

   IF nPosFrm0 > 0
      nPosFrm1:= nPosFrm0 + 3                      // + 3 ( 3 por "TP1" )   
      bSizeFrm:= SUBSTR(::cBytsID3, nPosFrm1, 3)   // El Tamaño del Frame esta en estos 3  Bytes, hay que decoficicar
      nSizeFrm:= ::CALCULAR(bSizeFrm)
      nPosFrm2:= nPosFrm1 + 3                      // 3 por "TP1" + 2 Por los Bytes de Flags, pueden haber mas de 2, hay que analizar
      cInfoFrm:= ::BYTE2ASCII(nPosFrm2, nSizeFrm)  // Se Extrae la Cadena Completa
   ENDIF

ELSEIF ::cID3Vers == "1.0" .OR. ::cID3Vers == "1.1"

       cInfoFrm:= SUBSTR(::cBytsID3, 31, 30)

ENDIF

RETURN cInfoFrm




//----------------------------------------------------------------------------//
METHOD TITULO() CLASS TMP3INFO

LOCAL cInfoFrm, nPosFrm1, nPosFrm2
LOCAL nPosFrm0, bSizeFrm, nSizeFrm

IF ::cID3Vers == "2.3.0" .OR. ::cID3Vers == "2.4.0"

    nPosFrm0:= AT("TIT2", ::cBytsID3)

   IF nPosFrm0 > 0
      nPosFrm1:= nPosFrm0 + 4                      // + 4 ( 4 por "TIT2" )   
      bSizeFrm:= SUBSTR(::cBytsID3, nPosFrm1, 4)   // El Tamaño del Frame esta en estos 4 Bytes, hay que decoficicar
      nSizeFrm:= ::CALCULAR(bSizeFrm)
      nPosFrm2:= nPosFrm1 + 6                      // 4 por "TIT2" + 2 Por los Bytes de Flags, pueden haber mas de 2, hay que analizar
      cInfoFrm:= ::BYTE2ASCII(nPosFrm2, nSizeFrm)  // Se Extrae la Cadena Completa
   ENDIF

ELSEIF ::cID3Vers == "2.2.0"

       nPosFrm0:= AT("TT2", ::cBytsID3)

   IF nPosFrm0 > 0
      nPosFrm1:= nPosFrm0 + 3                      // + 3 ( 3 por "TT2" )   
      bSizeFrm:= SUBSTR(::cBytsID3, nPosFrm1, 3)   // El Tamaño del Frame esta en estos 3  Bytes, hay que decoficicar
      nSizeFrm:= ::CALCULAR(bSizeFrm)
      nPosFrm2:= nPosFrm1 + 3                      // 3 por "TT2" + 2 Por los Bytes de Flags, pueden haber mas de 2, hay que analizar
      cInfoFrm:= ::BYTE2ASCII(nPosFrm2, nSizeFrm)  // Se Extrae la Cadena Completa
   ENDIF

ELSEIF ::cID3Vers == "1.0" .OR. ::cID3Vers == "1.1"

       cInfoFrm:= SUBSTR(::cBytsID3, 01, 30)

ENDIF

RETURN cInfoFrm




//----------------------------------------------------------------------------//
METHOD ALBUM() CLASS TMP3INFO

LOCAL cInfoFrm, nPosFrm1, nPosFrm2
LOCAL nPosFrm0, bSizeFrm, nSizeFrm

IF ::cID3Vers == "2.3.0" .OR. ::cID3Vers == "2.4.0"

    nPosFrm0:= AT("TALB", ::cBytsID3)

   IF nPosFrm0 > 0
      nPosFrm1:= nPosFrm0 + 4                      // + 4 ( 4 por "TALB" )   
      bSizeFrm:= SUBSTR(::cBytsID3, nPosFrm1, 4)   // El Tamaño del Frame esta en estos 4 Bytes, hay que decoficicar
      nSizeFrm:= ::CALCULAR(bSizeFrm)
      nPosFrm2:= nPosFrm1 + 6                      // 4 por "TALB" + 2 Por los Bytes de Flags, pueden haber mas de 2, hay que analizar
      cInfoFrm:= ::BYTE2ASCII(nPosFrm2, nSizeFrm)  // Se Extrae la Cadena Completa
   ENDIF

ELSEIF ::cID3Vers == "2.2.0"
  
       nPosFrm0:= AT("TAL", ::cBytsID3)

   IF nPosFrm0 > 0
      nPosFrm1:= nPosFrm0 + 3                      // + 3 ( 3 por "TAL" )   
      bSizeFrm:= SUBSTR(::cBytsID3, nPosFrm1, 3)   // El Tamaño del Frame esta en estos 3  Bytes, hay que decoficicar
      nSizeFrm:= ::CALCULAR(bSizeFrm)
      nPosFrm2:= nPosFrm1 + 3                      // 3 por "TAL" + 2 Por los Bytes de Flags, pueden haber mas de 2, hay que analizar
      cInfoFrm:= ::BYTE2ASCII(nPosFrm2, nSizeFrm)  // Se Extrae la Cadena Completa
   ENDIF

ELSEIF ::cID3Vers == "1.0" .OR. ::cID3Vers == "1.1"

       cInfoFrm:= SUBSTR(::cBytsID3, 61, 30)

ENDIF

RETURN cInfoFrm




//----------------------------------------------------------------------------//
METHOD FECHA() CLASS TMP3INFO

LOCAL cInfoFrm, nPosFrm1, nPosFrm2
LOCAL nPosFrm0, bSizeFrm, nSizeFrm

IF ::cID3Vers == "2.3.0" .OR. ::cID3Vers == "2.4.0"

    nPosFrm0:= AT("TYER", ::cBytsID3)

   IF nPosFrm0 > 0
      nPosFrm1:= nPosFrm0 + 4                      // + 4 ( 4 por "TYER" )   
      bSizeFrm:= SUBSTR(::cBytsID3, nPosFrm1, 4)   // El Tamaño del Frame esta en estos 4 Bytes, hay que decoficicar
      nSizeFrm:= ::CALCULAR(bSizeFrm)
      nPosFrm2:= nPosFrm1 + 6                      // 4 por "TYER" + 2 Por los Bytes de Flags, pueden haber mas de 2, hay que analizar
      cInfoFrm:= ::BYTE2ASCII(nPosFrm2, nSizeFrm)  // Se Extrae la Cadena Completa
   ENDIF

ELSEIF ::cID3Vers == "2.2.0"

       nPosFrm0:= AT("TYE", ::cBytsID3)

   IF nPosFrm0 > 0
      nPosFrm1:= nPosFrm0 + 3                      // + 3 ( 3 por "TYE" )   
      bSizeFrm:= SUBSTR(::cBytsID3, nPosFrm1, 3)   // El Tamaño del Frame esta en estos 3  Bytes, hay que decoficicar
      nSizeFrm:= ::CALCULAR(bSizeFrm)
      nPosFrm2:= nPosFrm1 + 3                      // 3 por "TYE" + 2 Por los Bytes de Flags, pueden haber mas de 2, hay que analizar
      cInfoFrm:= ::BYTE2ASCII(nPosFrm2, nSizeFrm)  // Se Extrae la Cadena Completa
   ENDIF

ELSEIF ::cID3Vers == "1.0" .OR. ::cID3Vers == "1.1"

       cInfoFrm:= SUBSTR(::cBytsID3, 91, 4)

ENDIF

RETURN cInfoFrm



//----------------------------------------------------------------------------//
METHOD PISTA() CLASS TMP3INFO

LOCAL cInfoFrm, nPosFrm1, nPosFrm2
LOCAL nPosFrm0, bSizeFrm, nSizeFrm

IF ::cID3Vers == "2.3.0" .OR. ::cID3Vers == "2.4.0"

    nPosFrm0:= AT("TRCK", ::cBytsID3)
    
   IF nPosFrm0 > 0
      nPosFrm1:= nPosFrm0 + 4                      // + 4 ( 4 por "TRCK" )   
      bSizeFrm:= SUBSTR(::cBytsID3, nPosFrm1, 4)   // El Tamaño del Frame esta en estos 4 Bytes, hay que decoficicar
      nSizeFrm:= ::CALCULAR(bSizeFrm)
      nPosFrm2:= nPosFrm1 + 6                      // 4 por "TRCK" + 2 Por los Bytes de Flags, pueden haber mas de 2, hay que analizar
      cInfoFrm:= ::BYTE2ASCII(nPosFrm2, nSizeFrm)  // Se Extrae la Cadena Completa
ENDIF

ELSEIF ::cID3Vers == "2.2.0"
     
       nPosFrm0:= AT("TRK", ::cBytsID3)
 
   IF nPosFrm0 > 0
      nPosFrm1:= nPosFrm0 + 3                      // + 3 ( 3 por "TRK" )   
      bSizeFrm:= SUBSTR(::cBytsID3, nPosFrm1, 3)   // El Tamaño del Frame esta en estos 3  Bytes, hay que decoficicar
      nSizeFrm:= ::CALCULAR(bSizeFrm)
      nPosFrm2:= nPosFrm1 + 3                      // 3 por "TRK" + 2 Por los Bytes de Flags, pueden haber mas de 2, hay que analizar
      cInfoFrm:= ::BYTE2ASCII(nPosFrm2, nSizeFrm)  // Se Extrae la Cadena Completa
   ENDIF
     
ELSEIF ::cID3Vers == "1.0"

  cInfoFrm:= ASC(SUBSTR(::cBytsID3, 124, 1))

 ENDIF
 
RETURN cInfoFrm


//----------------------------------------------------------------------------//
METHOD GENERO() CLASS TMP3INFO

LOCAL cInfoFrm, nPosFrm1, nPosFrm2
LOCAL nPosFrm0, bSizeFrm, nSizeFrm
LOCAL aGenero:= {"Blues", "Classic Rock", "Country", "Dance", "Disco", "Funk", "Grunge", "Hip-Hop", "Jazz", "Metal",;
                "New Age", "Oldies", "Pop", "R&B", "Rap", "Reggae", "Rock", "Techno", "Industrial",;
                "Alternative", "Ska", "Death Metal", "Pranks", "Soundtrack", "Euro-Techno", "Ambient", "Trip Hop",;
                "Vocal", "Jazz+Funk", "Fusion", "Trance", "Classical", "Instrumental", "Acid", "House", "Game",;
                "Sound Clip", "Gospel", "Noise", "Alt. Rock", "Bass", "Soul", "Punk", "Space", "Meditative",;
                "Instrumental Pop", "Instrumental Rock", "Ethnic", "Gothic", "Darkwave", "Techno-Industrial",;
                "Electronic", "Pop-Folk", "Eurodance", "Dream", "Southern Rock", "Comedy", "Cult", "Gangsta Rap",;
                "Top 40", "Christian Rap", "Pop/Punk", "Jungle", "Native American", "Cabaret", "New Wave", "Phychedelic",;
                "Rave", "Showtunes", "Trailer", "Lo-Fi", "Tribal", "Acid Punk", "Acid Jazz", "Polka", "Retro", "Musical",;
                "Rock & Roll", "Hard Rock", "Folk", "Folk/Rock", "National Folk", "Swing", "Fast-Fusion", "Bebob",;
                "Latin", "Revival", "Celtic", "Blue Grass", "Avantegarde", "Gothic Rock", "Progressive Rock",;
                "Psychedelic Rock", "Symphonic Rock", "Slow Rock", "Big Band", "Chorus", "Easy Listening", "Acoustic",;
                "Humour", "Speech", "Chanson", "Opera", "Chamber Music", "Sonata", "Symphony", "Booty Bass", "Primus",;
                "Porn Groove", "Satire", "Slow Jam", "Club", "Tango", "Samba", "Folklore", "Ballad", "power Ballad",;
                "Rhythmic Soul", "Freestyle", "Duet", "Punk Rock", "Drum Solo", "A Capella", "Euro-House", "Dance Hall",;
                "Goa", "Drum & Bass", "Club-House", "Hardcore", "Terror", "indie", "Brit Pop", "Negerpunk", "Polsk Punk",;
                "Beat", "Christian Gangsta Rap", "Heavy Metal", "Black Metal", "Crossover", "Comteporary Christian",;
                "Christian Rock", "Merengue", "Salsa", "Trash Metal", "Anime", "JPop", "Synth Pop"}

IF ::cID3Vers == "2.3.0" .OR. ::cID3Vers == "2.4.0"

   nPosFrm0:= AT("TCON", ::cBytsID3)

   IF nPosFrm0 > 0
      nPosFrm1:= nPosFrm0 + 4                      // + 4 ( 4 por "TCON" )   
      bSizeFrm:= SUBSTR(::cBytsID3, nPosFrm1, 4)   // El Tamaño del Frame esta en estos 4 Bytes, hay que decoficicar
      nSizeFrm:= ::CALCULAR(bSizeFrm)
      nPosFrm2:= nPosFrm1 + 6                      // 4 por "TCON" + 2 Por los Bytes de Flags, pueden haber mas de 2, hay que analizar
      cInfoFrm:= ::BYTE2ASCII(nPosFrm2, nSizeFrm)  // Se Extrae la Cadena Completa
   ENDIF

ELSEIF ::cID3Vers == "2.2.0"

   nPosFrm0:= AT("TCO", ::cBytsID3)

   IF nPosFrm0 > 0
      nPosFrm1:= nPosFrm0 + 3                      // + 3 ( 3 por "TCO" )   
      bSizeFrm:= SUBSTR(::cBytsID3, nPosFrm1, 3)   // El Tamaño del Frame esta en estos 3  Bytes, hay que decoficicar
      nSizeFrm:= ::CALCULAR(bSizeFrm)
      nPosFrm2:= nPosFrm1 + 3                      // 3 por "TCO" + 2 Por los Bytes de Flags, pueden haber mas de 2, hay que analizar
      cInfoFrm:= ::BYTE2ASCII(nPosFrm2, nSizeFrm)  // Se Extrae la Cadena Completa
   ENDIF

ELSEIF ::cID3Vers == "1.0" .OR. ::cID3Vers == "1.1"

   nPosFrm0:= ASC(SUBSTR(::cBytsID3, 125, 1))

   IF nPosFrm0 > 0 .AND. nPosFrm0 <= LEN(aGenero)
      cInfoFrm:= aGenero[nPosFrm0]
   ENDIF

ENDIF

RETURN cInfoFrm


//----------------------------------------------------------------------------//
METHOD COMENTARIO() CLASS TMP3INFO

LOCAL cInfoFrm, nPosFrm1, nPosFrm2
LOCAL nPosFrm0, bSizeFrm, nSizeFrm

IF ::cID3Vers == "2.3.0" .OR. ::cID3Vers == "2.4.0"
    
    nPosFrm0:= AT("COMM", ::cBytsID3)
   
   IF nPosFrm0 > 0
      nPosFrm1:= nPosFrm0 + 4                      // + 4 ( 4 por "COMM" )   
      bSizeFrm:= SUBSTR(::cBytsID3, nPosFrm1, 4)   // El Tamaño del Frame esta en estos 4 Bytes, hay que decoficicar
      nSizeFrm:= ::CALCULAR(bSizeFrm) - 4          // 1 para flag y 3 por el idioma (eng)
      nPosFrm2:= nPosFrm1 + 10                     // 4 por "COMM" + 3 Por los Bytes de Flags y 3 para idioma (eng)
      cInfoFrm:= ::BYTE2ASCII(nPosFrm2, nSizeFrm)  // Se Extrae la Cadena Completa
   ENDIF

ELSEIF ::cID3Vers == "2.2.0"

       nPosFrm0:= AT("COM", ::cBytsID3)

   IF nPosFrm0 > 0
      nPosFrm1:= nPosFrm0 + 3                      // + 3 ( 3 por "COM" )   
      bSizeFrm:= SUBSTR(::cBytsID3, nPosFrm1, 3)   // El Tamaño del Frame esta en estos 3  Bytes, hay que decoficicar
      nSizeFrm:= ::CALCULAR(bSizeFrm)
      nPosFrm2:= nPosFrm1 + 7                      // 3 por "COM" + 3 Por los Bytes de Flags y 3 para idioma (eng)
      cInfoFrm:= ::BYTE2ASCII(nPosFrm2, nSizeFrm)  // Se Extrae la Cadena Completa
   ENDIF

ELSEIF ::cID3Vers == "1.0" .OR. ::cID3Vers == "1.1"

       cInfoFrm:= SUBSTR(::cBytsID3, 95, 30)

ENDIF

RETURN cInfoFrm




//----------------------------------------------------------------------------//
METHOD ARTISTA() CLASS TMP3INFO

LOCAL cInfoFrm, nPosFrm1, nPosFrm2
LOCAL nPosFrm0, bSizeFrm, nSizeFrm

IF ::cID3Vers == "2.3.0" .OR. ::cID3Vers == "2.4.0"
    
    nPosFrm0:= AT("TPE2", ::cBytsID3)
    
   IF nPosFrm0 > 0
      nPosFrm1:= nPosFrm0 + 4                      // + 4 ( 4 por "TPE2" )   
      bSizeFrm:= SUBSTR(::cBytsID3, nPosFrm1, 4)   // El Tamaño del Frame esta en estos 4 Bytes, hay que decoficicar
      nSizeFrm:= ::CALCULAR(bSizeFrm)
      nPosFrm2:= nPosFrm1 + 6                      // 4 por "TPE2" + 2 Por los Bytes de Flags, pueden haber mas de 2, hay que analizar
      cInfoFrm:= ::BYTE2ASCII(nPosFrm2, nSizeFrm)  // Se Extrae la Cadena Completa
   ENDIF

ELSEIF ::cID3Vers == "2.2.0"
       
       nPosFrm0:= AT("TP3", ::cBytsID3)
 
   IF nPosFrm0 > 0
      nPosFrm1:= nPosFrm0 + 3                      // + 3 ( 3 por "TP3" )   
      bSizeFrm:= SUBSTR(::cBytsID3, nPosFrm1, 3)   // El Tamaño del Frame esta en estos 3  Bytes, hay que decoficicar
      nSizeFrm:= ::CALCULAR(bSizeFrm)
      nPosFrm2:= nPosFrm1 + 3                      // 3 por "TP3" + 2 Por los Bytes de Flags, pueden haber mas de 2, hay que analizar
      cInfoFrm:= ::BYTE2ASCII(nPosFrm2, nSizeFrm)  // Se Extrae la Cadena Completa
   ENDIF

ENDIF

RETURN cInfoFrm




//----------------------------------------------------------------------------//
METHOD COMPOSITOR() CLASS TMP3INFO

LOCAL cInfoFrm, nPosFrm1, nPosFrm2
LOCAL nPosFrm0, bSizeFrm, nSizeFrm

IF ::cID3Vers == "2.3.0" .OR. ::cID3Vers == "2.4.0"

nPosFrm0:= AT("TCOM", ::cBytsID3)

   IF nPosFrm0 > 0
      nPosFrm1:= nPosFrm0 + 4                      // + 4 ( 4 por "TCOM" )   
      bSizeFrm:= SUBSTR(::cBytsID3, nPosFrm1, 4)   // El Tamaño del Frame esta en estos 4 Bytes, hay que decoficicar
      nSizeFrm:= ::CALCULAR(bSizeFrm)
      nPosFrm2:= nPosFrm1 + 6                      // 4 por "TCOM" + 2 Por los Bytes de Flags, pueden haber mas de 2, hay que analizar
      cInfoFrm:= ::BYTE2ASCII(nPosFrm2, nSizeFrm)  // Se Extrae la Cadena Completa
ENDIF

ELSEIF ::cID3Vers == "2.2.0"

nPosFrm0:= AT("TCM", ::cBytsID3)
 
   IF nPosFrm0 > 0
      nPosFrm1:= nPosFrm0 + 3                      // + 3 ( 3 por "TCM" )   
      bSizeFrm:= SUBSTR(::cBytsID3, nPosFrm1, 3)   // El Tamaño del Frame esta en estos 3  Bytes, hay que decoficicar
      nSizeFrm:= ::CALCULAR(bSizeFrm)
      nPosFrm2:= nPosFrm1 + 3                      // 3 por "TCM" + 2 Por los Bytes de Flags, pueden haber mas de 2, hay que analizar
      cInfoFrm:= ::BYTE2ASCII(nPosFrm2, nSizeFrm)  // Se Extrae la Cadena Completa
   ENDIF

ENDIF
   
RETURN cInfoFrm




//----------------------------------------------------------------------------//
METHOD DISCO() CLASS TMP3INFO

LOCAL cInfoFrm, nPosFrm1, nPosFrm2
LOCAL nPosFrm0, bSizeFrm, nSizeFrm

IF ::cID3Vers == "2.3.0" .OR. ::cID3Vers == "2.4.0"

   nPosFrm0:= AT("TPOS", ::cBytsID3)

   IF nPosFrm0 > 0
      nPosFrm1:= nPosFrm0 + 4                      // + 4 ( 4 por "TPOS" )   
      bSizeFrm:= SUBSTR(::cBytsID3, nPosFrm1, 4)   // El Tamaño del Frame esta en estos 4 Bytes, hay que decoficicar
      nSizeFrm:= ::CALCULAR(bSizeFrm)
      nPosFrm2:= nPosFrm1 + 6                      // 4 por "TPOS" + 2 Por los Bytes de Flags, pueden haber mas de 2, hay que analizar
      cInfoFrm:= ::BYTE2ASCII(nPosFrm2, nSizeFrm)  // Se Extrae la Cadena Completa
   ENDIF

ELSEIF ::cID3Vers == "2.2.0"

   nPosFrm0:= AT("TCI", ::cBytsID3)

   IF nPosFrm0 > 0
      nPosFrm1:= nPosFrm0 + 3                      // + 3 ( 3 por "TCI" )   
      bSizeFrm:= SUBSTR(::cBytsID3, nPosFrm1, 3)   // El Tamaño del Frame esta en estos 3  Bytes, hay que decoficicar
      nSizeFrm:= ::CALCULAR(bSizeFrm)
      nPosFrm2:= nPosFrm1 + 3                      // 3 por "TCI" + 2 Por los Bytes de Flags, pueden haber mas de 2, hay que analizar
      cInfoFrm:= ::BYTE2ASCII(nPosFrm2, nSizeFrm)  // Se Extrae la Cadena Completa
   ENDIF

ENDIF

RETURN cInfoFrm




//----------------------------------------------------------------------------//
METHOD CLASIFICACION() CLASS TMP3INFO

LOCAL nPos, bSize, nVal, bClass, nClass, nPCls
LOCAL cInfoFrm:= 0

IF ::cID3Vers == "2.3.0" .OR. ::cID3Vers == "2.4.0"

  nPos:= AT("POPM", ::cBytsID3)

  nPos:= nPos + 4
  bSize:= SUBSTR(::cBytsID3, nPos, 4)      // El Tamaño del Frame para Clasificación esta en estos 4 Bytes, hay que decoficicar

  nVal:= ::CALCULAR(bSize)               

  nPCls:= nPos + 4 + nVal + 1

  bClass:= StrToHex(SUBSTR(::cBytsID3, nPCls, 1))
  nClass:= HexToNum(bClass)

      IF nClass >=   1 .AND. nClass <=  63   // => 1
         cInfoFrm:= 1
  ELSEIF nClass >=  64 .AND. nClass <= 127   // => 2
         cInfoFrm:= 2
  ELSEIF nClass >= 128 .AND. nClass <= 195   // => 3
         cInfoFrm:= 3
  ELSEIF nClass >= 196 .AND. nClass <= 254   // => 4
         cInfoFrm:= 4
  ELSEIF nClass == 255                       // => 5
         cInfoFrm:= 5
   ENDIF
  
ENDIF

RETURN cInfoFrm




//----------------------------------------------------------------------------//
METHOD COVER() CLASS TMP3INFO

LOCAL nPosAPIC0, nPosAPIC2, nPosAPIC3, cDatIm1, cDatIm2, cDatIm3, nBusqd1, nBusqd2
LOCAL nPosAPIC1, cMime, bSize, nFlags, nDesc

IF ::cID3Vers == "2.3.0" .OR. ::cID3Vers == "2.4.0"

  nPosAPIC0:= AT("APIC", ::cBytsID3)

     IF nPosAPIC0 > 0
        nPosAPIC1:= nPosAPIC0 + 4                      // + 4 ( 4 por "APIC" )
        nPosAPIC2:= AT("image/",::cBytsID3) + 6        // + 6 ( por "image/" ) 
        
            bSize:= SUBSTR(::cBytsID3, nPosAPIC1, 4)   // El Tamaño del Frame esta en estos 4 Bytes, hay que decoficicar
       ::nByteIma:= ::CALCULAR(bSize) - 14             // Se extrae el numero de Bytes de la Imagen, pero se restan 14 Bytes por MIME de Imagen (0image/jpeg030)
            cMime:= SUBSTR(::cBytsID3, nPosAPIC2, 3)   // Se extrae el Tipo Mime de la Imagen
            
 
               cDatIm1:= SUBSTR(::cBytsID3, nPosAPIC0 + 20, 100)    // Aprox 100 de Longitud. Sirve para Buscar la Descripcion y Tipo de Imagen
               nBusqd1:= AT(CHR(0), cDatIm1) + 1
/*  => */      cDatIm2:= StrToHex( SUBSTR(cDatIm1, nBusqd1, 1) )    // Para encontrar el tipo de Imagen.
               cDatIm3:= SUBSTR(cDatIm1,(nBusqd1+1))
               nBusqd2:= AT(CHR(0), cDatIm3)         
/*  => */   ::cDescCvr:= SUBSTR(cDatIm3,1,(nBusqd2 - 1))
/*  => */        nDesc:= LEN(::cDescCvr)

        
                  IF cDatIm2 == "00"
                     ::cTipoCvr:= "Other"
              ELSEIF cDatIm2 == "01"
                     ::cTipoCvr:= "32x32 Pixels 'File Icon' (PNG Only)"
              ELSEIF cDatIm2 == "02"
                     ::cTipoCvr:= "Other File Icon"
              ELSEIF cDatIm2 == "03"
                     ::cTipoCvr:= "Cover (Front)"
              ELSEIF cDatIm2 == "04"
                     ::cTipoCvr:= "Cover (Back)"
              ELSEIF cDatIm2 == "05"
                     ::cTipoCvr:= "Leaflet Page"
              ELSEIF cDatIm2 == "06"
                     ::cTipoCvr:= "Media (e.g. Label Side of CD)"
              ELSEIF cDatIm2 == "07"
                     ::cTipoCvr:= "Lead Artist/Lead Performer/Soloist"
              ELSEIF cDatIm2 == "08"
                     ::cTipoCvr:= "Artist/Performer"
              ELSEIF cDatIm2 == "09"
                     ::cTipoCvr:= "Conductor"
              ELSEIF cDatIm2 == "0A"
                     ::cTipoCvr:= "Band/Orchestra"
              ELSEIF cDatIm2 == "0B"
                     ::cTipoCvr:= "Composer"
              ELSEIF cDatIm2 == "0C"
                     ::cTipoCvr:= "Lyricist/Text Writer"
              ELSEIF cDatIm2 == "0D"
                     ::cTipoCvr:= "Recording Location"
              ELSEIF cDatIm2 == "0E"
                     ::cTipoCvr:= "During recording"
              ELSEIF cDatIm2 == "0F"
                     ::cTipoCvr:= "During performance"
              ELSEIF cDatIm2 == "10"
                     ::cTipoCvr:= "Movie/Video Screen Capture"
              ELSEIF cDatIm2 == "11"
                     ::cTipoCvr:= "A bright coloured fish"
              ELSEIF cDatIm2 == "12"
                     ::cTipoCvr:= "Illustration"
              ELSEIF cDatIm2 == "13"
                     ::cTipoCvr:= "Band/Artist Logotype"       
              ELSEIF cDatIm2 == "14"
                     ::cTipoCvr:= "Publisher/Studio Logotype"                                                                       
               ENDIF
            
            
             IF LOWER(cMime) == "jpe"
                nFlags:= 7 + nDesc                  // Bytes de Flags en jpg (No se Usan)
                ::cMimePic:= ".jpg"
         ELSEIF LOWER(cMime) == "jpg"
                nFlags:= 6 + nDesc                  // Bytes de Flags en png (No se Usan)
                ::cMimePic:= ".jpg"
         ELSEIF LOWER(cMime) == "png"
                nFlags:= 6 + nDesc                  // Bytes de Flags en png (No se Usan)
                ::cMimePic:= ".png"
         ELSEIF LOWER(cMime) == "bmp"
                nFlags:= 6  + nDesc                 // Bytes de Flags en bmp (No se Usan)
                ::cMimePic:= ".bmp"        
          ENDIF
         
         ::lHayImag:= .T.
          nPosAPIC3:= nPosAPIC2 + nFlags                        // Apartir de Este Byte se crea la Imagen
        
            IF cMime <> "jpeg"
               ::nByteIma:= ::nByteIma + 1                      // Si noes JPG agragar un byte que se recorta en el mime
         ENDIF
         
         ::bByteIma:= SUBSTR(::cBytsID3,nPosAPIC3,::nByteIma)   // Se Lee los Bytes de la Imagen y se pasan a Memoria
         
         IF ::lGuarPic == .T.
            ::GUARDARCOVER()                                   // Se Guarda la Imagen en Disco 
         ENDIF

     ENDIF

ELSEIF ::cID3Vers == "2.2.0"

      nPosAPIC0:= AT("PIC", ::cBytsID3)

     IF nPosAPIC0 > 0
        nPosAPIC1:= nPosAPIC0 + 3                             // + 3 ( 3 por "PIC" )
        nPosAPIC2:= nPosAPIC1 + 4                             // 1 Posicion Actual + 3 ( por Size "0x00 0x00 0x00" ) 
     
            bSize:= SUBSTR(::cBytsID3, nPosAPIC1, 3)          // El Tamaño del Frame esta en estos 3 Bytes, hay que decodificar         
       ::nByteIma:= ::CALCULAR(bSize) - 6                     // Se extrae el numero de Bytes de la Imagen restando los 6 bytes del Mime
            cMime:= SUBSTR(::cBytsID3, nPosAPIC2, 3)          // Se extrae el Tipo Mime de la Imagen

/*  => */ cDatIm2:= StrToHex( SUBSTR(::cBytsID3, (nPosAPIC2 + 3), 1) )    // Para encontrar el tipo de Imagen.

           nFlags:= 5                                         // Bytes de Flags de Mime de la imagen
       ::cMimePic:= "."+LOWER(cMime)
       ::lHayImag:= .T.
        nPosAPIC3:= nPosAPIC2 + nFlags                        // Apartir de Este Byte se crea la Imagen        
       ::bByteIma:= SUBSTR(::cBytsID3,nPosAPIC3,::nByteIma)   // Se Lee los Bytes de la Imagen y se pasan a Memoria
         
                  IF cDatIm2 == "00"
                     ::cTipoCvr:= "Other"
              ELSEIF cDatIm2 == "01"
                     ::cTipoCvr:= "32x32 Pixels 'File Icon' (PNG Only)"
              ELSEIF cDatIm2 == "02"
                     ::cTipoCvr:= "Other File Icon"
              ELSEIF cDatIm2 == "03"
                     ::cTipoCvr:= "Cover (Front)"
              ELSEIF cDatIm2 == "04"
                     ::cTipoCvr:= "Cover (Back)"
              ELSEIF cDatIm2 == "05"
                     ::cTipoCvr:= "Leaflet Page"
              ELSEIF cDatIm2 == "06"
                     ::cTipoCvr:= "Media (e.g. Label Side of CD)"
              ELSEIF cDatIm2 == "07"
                     ::cTipoCvr:= "Lead Artist/Lead Performer/Soloist"
              ELSEIF cDatIm2 == "08"
                     ::cTipoCvr:= "Artist/Performer"
              ELSEIF cDatIm2 == "09"
                     ::cTipoCvr:= "Conductor"
              ELSEIF cDatIm2 == "0A"
                     ::cTipoCvr:= "Band/Orchestra"
              ELSEIF cDatIm2 == "0B"
                     ::cTipoCvr:= "Composer"
              ELSEIF cDatIm2 == "0C"
                     ::cTipoCvr:= "Lyricist/Text Writer"
              ELSEIF cDatIm2 == "0D"
                     ::cTipoCvr:= "Recording Location"
              ELSEIF cDatIm2 == "0E"
                     ::cTipoCvr:= "During recording"
              ELSEIF cDatIm2 == "0F"
                     ::cTipoCvr:= "During performance"
              ELSEIF cDatIm2 == "10"
                     ::cTipoCvr:= "Movie/Video Screen Capture"
              ELSEIF cDatIm2 == "11"
                     ::cTipoCvr:= "A bright coloured fish"
              ELSEIF cDatIm2 == "12"
                     ::cTipoCvr:= "Illustration"
              ELSEIF cDatIm2 == "13"
                     ::cTipoCvr:= "Band/Artist Logotype"       
              ELSEIF cDatIm2 == "14"
                     ::cTipoCvr:= "Publisher/Studio Logotype"                                                                       
               ENDIF

         IF ::lGuarPic == .T.
            ::GUARDARCOVER()                                  // Se Guarda la Imagen en Disco 
         ENDIF

     ENDIF

ENDIF

RETURN Nil




//----------------------------------------------------------------------------//
METHOD GUARDARCOVER(lRes)  CLASS TMP3INFO

LOCAL nHandlSav, cPathIm
LOCAL cNombreIm:= "mp3image"

  DEFAULT lRes:= .F.

  IF lRes
     cNombreIm:= PutFile({ {"Imágen sin Extención (*.*)", "*.*"} }, "Guardar Imagen", GetCurrentFolder())
  ENDIF   
  IF LEN(cNombreIm) >= 4
     cPathIm:= cFilePath(cNombreIm)
     cNombreIm:= cFileNoExt(cNombreIm)
     nHandlSav:= FCREATE( iif(lRes, cPathIm, GetTempFolder()) + "\" + cNombreIm + ::cMimePic )
     SysRefresh()
     HMG_SysWait()
     FWRITE(nHandlSav, ::bByteIma)
     SysRefresh()
     HMG_SysWait()
     FCLOSE(nHandlSav)
     SysRefresh()
     HMG_SysWait()
  ENDIF   

RETURN Nil




//----------------------------------------------------------------------------//
METHOD MPEGVERSION() CLASS TMP3INFO

LOCAL cBits:= SUBSTR(::nBinrMP3,12,2)
LOCAL cValor:= ""

      IF cBits == "00"
         cValor:= "MPEG Version 2.5"
  ELSEIF cBits == "01"
         cValor:= "Reservado"
  ELSEIF cBits == "10"
         cValor:= "MPEG Version 2"
  ELSEIF cBits == "11"
         cValor:= "MPEG Version 1"       
   ENDIF

RETURN cValor




//----------------------------------------------------------------------------//
METHOD LAYER() CLASS TMP3INFO

LOCAL cBits:= SUBSTR(::nBinrMP3,14,2)
LOCAL cValor:= ""

      IF cBits == "00"
         cValor:= "Reservado"
  ELSEIF cBits == "01"
         cValor:= "Layer III"
  ELSEIF cBits == "10"
         cValor:= "Layer II"
  ELSEIF cBits == "11"
         cValor:= "Layer I"       
   ENDIF

RETURN cValor




//----------------------------------------------------------------------------//
METHOD CRCPROTECTED() CLASS TMP3INFO

LOCAL cBits:= SUBSTR(::nBinrMP3,16,1)

RETURN IF( cBits == "0", (.T.),(.F.))




//----------------------------------------------------------------------------//
METHOD BYTERATE() CLASS TMP3INFO

LOCAL cVers:= SUBSTR(::nBinrMP3,12,2)
LOCAL cLayr:= SUBSTR(::nBinrMP3,14,2)
LOCAL cBits:= SUBSTR(::nBinrMP3,17,4)
LOCAL cValor:= ""

           IF cBits == "0000"

           IF cVers == "11" .AND. cLayr == "11"     //  Version 1 y  Layer I
              cValor:= "Free"
       ELSEIF cVers == "11" .AND. cLayr == "10"     //  Version 1 y  Layer II
              cValor:= "Free"
       ELSEIF cVers == "11" .AND. cLayr == "01"     //  Version 1 y  Layer III
              cValor:= "Free"
       ELSEIF cVers == "10" .AND. cLayr == "11"     //  Version 2 y  Layer I
              cValor:= "Free"
       ELSEIF cVers == "10" .AND. cLayr == "10"     //  Version 2 y  Layer II
              cValor:= "Free"
       ELSEIF cVers == "10" .AND. cLayr == "01"     //  Version 2 y  Layer III
              cValor:= "Free"
        ENDIF

       ELSEIF cBits == "0001"

           IF cVers == "11" .AND. cLayr == "11"     //  Version 1 y  Layer I
              cValor:= "32"
       ELSEIF cVers == "11" .AND. cLayr == "10"     //  Version 1 y  Layer II
              cValor:= "32"
       ELSEIF cVers == "11" .AND. cLayr == "01"     //  Version 1 y  Layer III
              cValor:= "32"
       ELSEIF cVers == "10" .AND. cLayr == "11"     //  Version 2 y  Layer I
              cValor:= "32"
       ELSEIF cVers == "10" .AND. cLayr == "10"     //  Version 2 y  Layer II
              cValor:= "8"
       ELSEIF cVers == "10" .AND. cLayr == "01"     //  Version 2 y  Layer III
              cValor:= "8"
        ENDIF

       ELSEIF cBits == "0010"

           IF cVers == "11" .AND. cLayr == "11"     //  Version 1 y  Layer I
              cValor:= "64"
       ELSEIF cVers == "11" .AND. cLayr == "10"     //  Version 1 y  Layer II
              cValor:= "48"
       ELSEIF cVers == "11" .AND. cLayr == "01"     //  Version 1 y  Layer III
              cValor:= "40"
       ELSEIF cVers == "10" .AND. cLayr == "11"     //  Version 2 y  Layer I
              cValor:= "48"
       ELSEIF cVers == "10" .AND. cLayr == "10"     //  Version 2 y  Layer II
              cValor:= "16"
       ELSEIF cVers == "10" .AND. cLayr == "01"     //  Version 2 y  Layer III
              cValor:= "16"
        ENDIF

       ELSEIF cBits == "0011"

           IF cVers == "11" .AND. cLayr == "11"     //  Version 1 y  Layer I
              cValor:= "96"
       ELSEIF cVers == "11" .AND. cLayr == "10"     //  Version 1 y  Layer II
              cValor:= "56"
       ELSEIF cVers == "11" .AND. cLayr == "01"     //  Version 1 y  Layer III
              cValor:= "48"
       ELSEIF cVers == "10" .AND. cLayr == "11"     //  Version 2 y  Layer I
              cValor:= "56"
       ELSEIF cVers == "10" .AND. cLayr == "10"     //  Version 2 y  Layer II
              cValor:= "24"
       ELSEIF cVers == "10" .AND. cLayr == "01"     //  Version 2 y  Layer III
              cValor:= "24"
        ENDIF

       ELSEIF cBits == "0100"

           IF cVers == "11" .AND. cLayr == "11"     //  Version 1 y  Layer I
              cValor:= "128"
       ELSEIF cVers == "11" .AND. cLayr == "10"     //  Version 1 y  Layer II
              cValor:= "64"
       ELSEIF cVers == "11" .AND. cLayr == "01"     //  Version 1 y  Layer III
              cValor:= "56"
       ELSEIF cVers == "10" .AND. cLayr == "11"     //  Version 2 y  Layer I
              cValor:= "64"
       ELSEIF cVers == "10" .AND. cLayr == "10"     //  Version 2 y  Layer II
              cValor:= "32"
       ELSEIF cVers == "10" .AND. cLayr == "01"     //  Version 2 y  Layer III
              cValor:= "32"
        ENDIF

       ELSEIF cBits == "0101"

           IF cVers == "11" .AND. cLayr == "11"     //  Version 1 y  Layer I
              cValor:= "160"
       ELSEIF cVers == "11" .AND. cLayr == "10"     //  Version 1 y  Layer II
              cValor:= "80"
       ELSEIF cVers == "11" .AND. cLayr == "01"     //  Version 1 y  Layer III
              cValor:= "64"
       ELSEIF cVers == "10" .AND. cLayr == "11"     //  Version 2 y  Layer I
              cValor:= "80"
       ELSEIF cVers == "10" .AND. cLayr == "10"     //  Version 2 y  Layer II
              cValor:= "40"
       ELSEIF cVers == "10" .AND. cLayr == "01"     //  Version 2 y  Layer III
              cValor:= "40"
        ENDIF

       ELSEIF cBits == "0110"

           IF cVers == "11" .AND. cLayr == "11"     //  Version 1 y  Layer I
              cValor:= "192"
       ELSEIF cVers == "11" .AND. cLayr == "10"     //  Version 1 y  Layer II
              cValor:= "96"
       ELSEIF cVers == "11" .AND. cLayr == "01"     //  Version 1 y  Layer III
              cValor:= "80"
       ELSEIF cVers == "10" .AND. cLayr == "11"     //  Version 2 y  Layer I
              cValor:= "96"
       ELSEIF cVers == "10" .AND. cLayr == "10"     //  Version 2 y  Layer II
              cValor:= "48"
       ELSEIF cVers == "10" .AND. cLayr == "01"     //  Version 2 y  Layer III
              cValor:= "48"
        ENDIF

       ELSEIF cBits == "0111"

           IF cVers == "11" .AND. cLayr == "11"     //  Version 1 y  Layer I
              cValor:= "224"
       ELSEIF cVers == "11" .AND. cLayr == "10"     //  Version 1 y  Layer II
              cValor:= "112"
       ELSEIF cVers == "11" .AND. cLayr == "01"     //  Version 1 y  Layer III
              cValor:= "96"
       ELSEIF cVers == "10" .AND. cLayr == "11"     //  Version 2 y  Layer I
              cValor:= "112"
       ELSEIF cVers == "10" .AND. cLayr == "10"     //  Version 2 y  Layer II
              cValor:= "56"
       ELSEIF cVers == "10" .AND. cLayr == "01"     //  Version 2 y  Layer III
              cValor:= "56"
        ENDIF

       ELSEIF cBits == "1000"

           IF cVers == "11" .AND. cLayr == "11"     //  Version 1 y  Layer I
              cValor:= "256"
       ELSEIF cVers == "11" .AND. cLayr == "10"     //  Version 1 y  Layer II
              cValor:= "128"
       ELSEIF cVers == "11" .AND. cLayr == "01"     //  Version 1 y  Layer III
              cValor:= "112"
       ELSEIF cVers == "10" .AND. cLayr == "11"     //  Version 2 y  Layer I
              cValor:= "128"
       ELSEIF cVers == "10" .AND. cLayr == "10"     //  Version 2 y  Layer II
              cValor:= "64"
       ELSEIF cVers == "10" .AND. cLayr == "01"     //  Version 2 y  Layer III
              cValor:= "64"
        ENDIF

       ELSEIF cBits == "1001"

           IF cVers == "11" .AND. cLayr == "11"     //  Version 1 y  Layer I
              cValor:= "288"
       ELSEIF cVers == "11" .AND. cLayr == "10"     //  Version 1 y  Layer II
              cValor:= "160"
       ELSEIF cVers == "11" .AND. cLayr == "01"     //  Version 1 y  Layer III
              cValor:= "128"
       ELSEIF cVers == "10" .AND. cLayr == "11"     //  Version 2 y  Layer I
              cValor:= "144"
       ELSEIF cVers == "10" .AND. cLayr == "10"     //  Version 2 y  Layer II
              cValor:= "80"
       ELSEIF cVers == "10" .AND. cLayr == "01"     //  Version 2 y  Layer III
              cValor:= "80"
        ENDIF

       ELSEIF cBits == "1010"

           IF cVers == "11" .AND. cLayr == "11"     //  Version 1 y  Layer I
              cValor:= "320"
       ELSEIF cVers == "11" .AND. cLayr == "10"     //  Version 1 y  Layer II
              cValor:= "192"
       ELSEIF cVers == "11" .AND. cLayr == "01"     //  Version 1 y  Layer III
              cValor:= "160"
       ELSEIF cVers == "10" .AND. cLayr == "11"     //  Version 2 y  Layer I
              cValor:= "160"
       ELSEIF cVers == "10" .AND. cLayr == "10"     //  Version 2 y  Layer II
              cValor:= "96"
       ELSEIF cVers == "10" .AND. cLayr == "01"     //  Version 2 y  Layer III
              cValor:= "96"
        ENDIF

       ELSEIF cBits == "1011"

           IF cVers == "11" .AND. cLayr == "11"     //  Version 1 y  Layer I
              cValor:= "352"
       ELSEIF cVers == "11" .AND. cLayr == "10"     //  Version 1 y  Layer II
              cValor:= "224"
       ELSEIF cVers == "11" .AND. cLayr == "01"     //  Version 1 y  Layer III
              cValor:= "192"
       ELSEIF cVers == "10" .AND. cLayr == "11"     //  Version 2 y  Layer I
              cValor:= "176"
       ELSEIF cVers == "10" .AND. cLayr == "10"     //  Version 2 y  Layer II
              cValor:= "112"
       ELSEIF cVers == "10" .AND. cLayr == "01"     //  Version 2 y  Layer III
              cValor:= "112"
        ENDIF

       ELSEIF cBits == "1100"
       
           IF cVers == "11" .AND. cLayr == "11"     //  Version 1 y  Layer I
              cValor:= "384"
       ELSEIF cVers == "11" .AND. cLayr == "10"     //  Version 1 y  Layer II
              cValor:= "256"
       ELSEIF cVers == "11" .AND. cLayr == "01"     //  Version 1 y  Layer III
              cValor:= "224"
       ELSEIF cVers == "10" .AND. cLayr == "11"     //  Version 2 y  Layer I
              cValor:= "192"
       ELSEIF cVers == "10" .AND. cLayr == "10"     //  Version 2 y  Layer II
              cValor:= "128"
       ELSEIF cVers == "10" .AND. cLayr == "01"     //  Version 2 y  Layer III
              cValor:= "128"
        ENDIF

       ELSEIF cBits == "1101"

           IF cVers == "11" .AND. cLayr == "11"     //  Version 1 y  Layer I
              cValor:= "416"
       ELSEIF cVers == "11" .AND. cLayr == "10"     //  Version 1 y  Layer II
              cValor:= "320"
       ELSEIF cVers == "11" .AND. cLayr == "01"     //  Version 1 y  Layer III
              cValor:= "256"
       ELSEIF cVers == "10" .AND. cLayr == "11"     //  Version 2 y  Layer I
              cValor:= "224"
       ELSEIF cVers == "10" .AND. cLayr == "10"     //  Version 2 y  Layer II
              cValor:= "144"
       ELSEIF cVers == "10" .AND. cLayr == "01"     //  Version 2 y  Layer III
              cValor:= "144"
        ENDIF

       ELSEIF cBits == "1110"

           IF cVers == "11" .AND. cLayr == "11"     //  Version 1 y  Layer I
              cValor:= "448"
       ELSEIF cVers == "11" .AND. cLayr == "10"     //  Version 1 y  Layer II
              cValor:= "384"
       ELSEIF cVers == "11" .AND. cLayr == "01"     //  Version 1 y  Layer III
              cValor:= "320"
       ELSEIF cVers == "10" .AND. cLayr == "11"     //  Version 2 y  Layer I
              cValor:= "256"
       ELSEIF cVers == "10" .AND. cLayr == "10"     //  Version 2 y  Layer II
              cValor:= "160"
       ELSEIF cVers == "10" .AND. cLayr == "01"     //  Version 2 y  Layer III
              cValor:= "160"
        ENDIF

       ELSEIF cBits == "1111"

           IF cVers == "11" .AND. cLayr == "11"     //  Version 1 y  Layer I
              cValor:= "Bad"
       ELSEIF cVers == "11" .AND. cLayr == "10"     //  Version 1 y  Layer II
              cValor:= "Bad"
       ELSEIF cVers == "11" .AND. cLayr == "01"     //  Version 1 y  Layer III
              cValor:= "Bad"
       ELSEIF cVers == "10" .AND. cLayr == "11"     //  Version 2 y  Layer I
              cValor:= "Bad"
       ELSEIF cVers == "10" .AND. cLayr == "10"     //  Version 2 y  Layer II
              cValor:= "Bad"
       ELSEIF cVers == "10" .AND. cLayr == "01"     //  Version 2 y  Layer III
              cValor:= "Bad"
        ENDIF

        ENDIF

RETURN cValor




//----------------------------------------------------------------------------//
METHOD SAMPLING() CLASS TMP3INFO

LOCAL cVers:= SUBSTR(::nBinrMP3,12,2)
LOCAL cBits:= SUBSTR(::nBinrMP3,21,2)
LOCAL cValor:= ""

           IF cBits == "00"

              IF cVers == "11"   //   MPEG Version 1
                 cValor:= "44100"
          ELSEIF cVers == "10"   //   MPEG Version 2
                 cValor:= "22050"
          ELSEIF cVers == "00"   //   MPEG Version 2.5
                 cValor:= "11025"
           ENDIF

       ELSEIF cBits == "01"

              IF cVers == "11"   //   MPEG Version 1
                 cValor:= "48000"
          ELSEIF cVers == "10"   //   MPEG Version 2
                 cValor:= "24000"
          ELSEIF cVers == "00"   //   MPEG Version 2.5
                 cValor:= "12000"
           ENDIF

       ELSEIF cBits == "10"

              IF cVers == "11"   //   MPEG Version 1
                 cValor:= "32000"
          ELSEIF cVers == "10"   //   MPEG Version 2
                 cValor:= "16000"
          ELSEIF cVers == "00"   //   MPEG Version 2.5
                 cValor:= "8000"
           ENDIF

       ELSEIF cBits == "11"

              IF cVers == "11"   //   MPEG Version 1
                 cValor:= "Reservado"
          ELSEIF cVers == "10"   //   MPEG Version 2
                 cValor:= "Reservado"
          ELSEIF cVers == "00"   //   MPEG Version 2.5
                 cValor:= "Reservado"
           ENDIF

        ENDIF

RETURN cValor




//----------------------------------------------------------------------------//
METHOD PADDING() CLASS TMP3INFO
RETURN SUBSTR(::nBinrMP3,23,1) + " Byte"




//----------------------------------------------------------------------------//
METHOD PRIVATEBIT() CLASS TMP3INFO
RETURN IF (SUBSTR(::nBinrMP3,24,1) == "0", (.F.),(.T.))




//----------------------------------------------------------------------------//
METHOD CANALES() CLASS TMP3INFO

LOCAL cBits:= SUBSTR(::nBinrMP3,25,2)
LOCAL cValor:= ""

         IF cBits == "00"
            cValor:= "Stereo"
     ELSEIF cBits == "01"
            cValor:= "Joint Stereo"
     ELSEIF cBits == "10"
            cValor:= "Dual Channel"
     ELSEIF cBits == "11"
            cValor:= "Mono"
      ENDIF

RETURN cValor




//----------------------------------------------------------------------------//
METHOD MODEXTENSION() CLASS TMP3INFO
RETURN SUBSTR(::nBinrMP3,27,2)




//----------------------------------------------------------------------------//
METHOD COPYRIGHT() CLASS TMP3INFO

LOCAL cBits:= SUBSTR(::nBinrMP3,29,1)

RETURN IF( cBits == "0",(.F.),(.T.))




//----------------------------------------------------------------------------//
METHOD ORIGINAL() CLASS TMP3INFO

LOCAL cBits:= SUBSTR(::nBinrMP3,30,1)

RETURN IF( cBits == "0",(.F.),(.T.))




//----------------------------------------------------------------------------//
METHOD EMPHASIS() CLASS TMP3INFO

LOCAL cBits:= SUBSTR(::nBinrMP3,31,2)
LOCAL cValor:= ""

         IF cBits == "00"
            cValor:= "None"
     ELSEIF cBits == "01"
            cValor:= "50/15 ms"
     ELSEIF cBits == "10"
            cValor:= "Reservado"
     ELSEIF cBits == "11"
            cValor:= "CCIT J.17"
      ENDIF

RETURN cValor




//------------------------------------------------------------------------------------------------------------------------//
METHOD DETALLESMP3() CLASS TMP3INFO

LOCAL cDirF:= LFN2SFN(cFilePath(::cMp3File))
LOCAL oActX:= CreateObject( "Shell.Application" )

::oObjtMP3:= oActX:NameSpace(cDirF)

RETURN Nil




//----------------------------------------------------------------------------//
METHOD BIOGRAFIA() CLASS TMP3INFO

LOCAL cBio:= "Clase Creada por: cuatecatl82  Víctor Daniel Cuatecatl León"+CRLF+;
             "Soluciones y Diseño de Software Empresarial    SOLDISOFT Software"+CRLF+CRLF+;
             "http://id3.org/"+ CRLF +;
             "https://phoxis.org/2010/05/08/what-are-id3-tags-all-about/"+ CRLF +;  //  <- (*7) La Formula esta aqui.
             "https://es.wikipedia.org/wiki/Bit_m%C3%A1s_significativo"+ CRLF +;
             "http://id3lib.sourceforge.net/id3/id3v2com-00.html"+ CRLF +;
             "http://xworkforall.blogspot.com/2016/10/vamos-programar-16.html"

MSGInfo(cBio,"Biografia de la Clase ID3V2.3.0", MP3CLASSE)

RETURN Nil




//----------------------------------------------------------------------------//
METHOD ERROR(nError) CLASS TMP3INFO

DO CASE
   CASE nError = 0
     // MSGStop("Satisfactorio", MP3CLASSE)

   CASE nError = 2
     MSGStop("Fichero no encontrado", MP3CLASSE)

   CASE nError = 3
     MSGStop("Vía no encontrada", MP3CLASSE)

   CASE nError = 4                 
     MSGStop("Demasiados ficheros abiertos", MP3CLASSE)

   CASE nError = 5                
     MSGStop("Acceso denegado", MP3CLASSE)

   CASE nError = 6                 
     MSGStop("Manejador no válido", MP3CLASSE)

   CASE nError = 8                 
     MSGStop("Memoria insuficiente", MP3CLASSE)

   CASE nError = 15               
     MSGStop("Unidad especificada no válida", )

   CASE nError = 19                
     MSGStop("Ha intentado escribir en un disco protegido contra escritura", MP3CLASSE)

   CASE nError = 21
     MSGStop("Unidad no preparada", MP3CLASSE)

   CASE nError = 23                
     MSGStop("Error CRC de datos", MP3CLASSE)

   CASE nError = 29
     MSGStop("Fallo de escritura", MP3CLASSE)

   CASE nError = 30                
     MSGStop("Fallo de lectura", MP3CLASSE)

   CASE nError = 32 
     MSGStop("Violación de compartición", MP3CLASSE)

   CASE nError = 33
     MSGStop("Violación de bloqueo", MP3CLASSE)
ENDCASE

RETURN Nil