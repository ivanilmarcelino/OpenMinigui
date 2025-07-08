/*
 * Implementación del comando EDIT EXTENDED para la librería HMG.
 * (c) Cristóbal Mollá [cemese@terra.es]
*/


// Ficheros de definiciones.---------------------------------------------------
#include "hmg.ch"
#include "dbstruct.ch"

// Declaración de definiciones.  Generales.------------------------------------
#define ABM_CRLF                HB_OsNewLine()

// Estructura de la etiquetas.
#define ABM_LBL_LEN          6     // changed 190713
#define ABM_LBL_NAME            1
#define ABM_LBL_ROW             2
#define ABM_LBL_COL             3
#define ABM_LBL_WIDTH           4
#define ABM_LBL_HEIGHT          5
#define ABM_LBL_CC              6  // aded 190713

// Estructura de los controles de edición.
#define ABM_CON_LEN          8     // changed 190713
#define ABM_CON_NAME            1
#define ABM_CON_ROW             2
#define ABM_CON_COL             3
#define ABM_CON_WIDTH           4
#define ABM_CON_HEIGHT          5
#define ABM_CON_DES             6
#define ABM_CON_TYPE            7
#define ABM_CON_CC             8   // aded 190713

// Tipos de controles de edición.
#define ABM_TEXTBOXC            1
#define ABM_TEXTBOXN            2
#define ABM_DATEPICKER          3
#define ABM_CHECKBOX            4
#define ABM_EDITBOX             5
#define ABM_COMBOBOX            6  // added 170214


// Declaración de variables globales.------------------------------------------
STATIC _cArea /*as character*/ // Nombre del area de la bdd.
STATIC _aEstructura /*as array*/ // Estructura de la bdd.
STATIC _aNombreCampo /*as array*/ // Nombre desciptivo de los campos de la bdd.
STATIC _aEtiqueta /*as array*/ // Datos de las etiquetas.
STATIC _aControl /*as array*/ // Datos de los controles.
STATIC _aEditConfig /*as array*/ // Estructura de la bdd. + Labels + Controls  //add 20151217
STATIC aAvisoCampo

/****************************************************************************************
 *  Aplicación: Comando EDIT para HMG
 *       Autor: Cristóbal Mollá [cemese@terra.es]
 *     Función: ABM3generar()
 * Descripción: Función
 *  Parámetros: cArea           Nombre del area de la bdd. Por defecto se toma el area
 *    Devuelve: NIL
****************************************************************************************/
FUNCTION ABM3generar()

   // Declaración de variables locales.-----------------------------------
   LOCAL i AS NUMERIC // Indice de iteración.
   LOCAL nFila AS NUMERIC // Fila de creación del control de edición.
   LOCAL nColumna AS NUMERIC // Columna de creación del control de edición.
   LOCAL acAreas := {}
   LOCAL nEstructura

   // Control de parámetros.----------------------------------------------
   // Area de la base de datos.
   FOR i = 1 TO 255
      If ! Empty( Alias( i ) )
         AAdd( acAreas, Prop( Alias( i ) ) )
      ENDIF
   NEXT

   _cArea := acAreas[ MsgExtended( "Please, choose Test1   ", "Open Areas", acAreas ) ]
   _aEstructura := ( _cArea )->( dbStruct() )
   nEstructura := Len( _aEstructura )

   // Nombres de los campos.
   _aNombreCampo := {}
   FOR i := 1 TO HMG_LEN( _aEstructura )
      AAdd( _aNombreCampo, HMG_UPPER( hb_ULeft( _aEstructura[ i, DBS_NAME ], 1 ) ) + ;
         HMG_LOWER( hb_USubStr( _aEstructura[ i, DBS_NAME ], 2 ) ) )
   NEXT

   // Texto de aviso en la barra de estado de la ventana de edición de registro.
   aAvisoCampo := {}
   FOR i := 1 TO nEstructura
      DO CASE
      CASE _aEstructura[ i, DBS_TYPE ] == "C"
         AAdd( aAvisoCampo, _HMG_aLangUser[ 2 ] )
      CASE _aEstructura[ i, DBS_TYPE ] == "N"
         AAdd( aAvisoCampo, _HMG_aLangUser[ 3 ] )
      CASE _aEstructura[ i, DBS_TYPE ] == "D"
         AAdd( aAvisoCampo, _HMG_aLangUser[ 4 ] )
      CASE _aEstructura[ i, DBS_TYPE ] == "L"
         AAdd( aAvisoCampo, _HMG_aLangUser[ 5 ] )
      CASE _aEstructura[ i, DBS_TYPE ] == "M"
         AAdd( aAvisoCampo, _HMG_aLangUser[ 6 ] )
      OTHERWISE
         AAdd( aAvisoCampo, _HMG_aLangUser[ 7 ] )
      ENDCASE
   NEXT

   // ----------------------------------------------------------------------------*
   // Datos de las etiquetas y los controles de la ventana de edición. -> Hecho 190713
   _aEtiquetaV2 := Array( HMG_LEN( _aEstructura ), ABM_LBL_LEN ) ;  _aEtiquetaV3 := Array( HMG_LEN( _aEstructura ), ABM_LBL_LEN )
   _aEtiquetaH2 := Array( HMG_LEN( _aEstructura ), ABM_LBL_LEN ) ;  _aEtiquetaH3 := Array( HMG_LEN( _aEstructura ), ABM_LBL_LEN )

   _aControlV2 := Array( HMG_LEN( _aEstructura ), ABM_CON_LEN ) ;  _aControlV3 := Array( HMG_LEN( _aEstructura ), ABM_CON_LEN )
   _aControlH2 := Array( HMG_LEN( _aEstructura ), ABM_CON_LEN ) ;  _aControlH3 := Array( HMG_LEN( _aEstructura ), ABM_CON_LEN )

   // ----------------------------------------------------------------------------*
   // Decidir en que columna queda c/ control (1, 2 ó 3)
   FOR i := 1 TO HMG_LEN( _aEstructura )
      _aEtiquetaH2[ i, ABM_LBL_CC ] := iif( i / 2 = Int( i / 2 ), 2, 1 )
      _aControlH2[ i, ABM_CON_CC ] := iif( i / 2 = Int( i / 2 ), 2, 1 )
      _aEtiquetaV2[ i, ABM_LBL_CC ] := iif( i <= ( 1 + HMG_LEN( _aEstructura ) / 2 ), 1, 2 )
      _aControlV2[ i, ABM_CON_CC ] := iif( i <= ( 1 + HMG_LEN( _aEstructura ) / 2 ), 1, 2 )

      IF i / 3 = Int( i / 3 )
         _aEtiquetaH3[ i, ABM_LBL_CC ] := 3
         _aControlH3[ i, ABM_CON_CC ] := 3
      ELSEIF ( i + 1 ) / 3 = Int( ( i + 1 ) / 3 )
         _aEtiquetaH3[ i, ABM_LBL_CC ] := 2
         _aControlH3[ i, ABM_CON_CC ] := 2
      ELSE
         _aEtiquetaH3[ i, ABM_LBL_CC ] := 1
         _aControlH3[ i, ABM_CON_CC ] := 1
      ENDIF

      IF i <= 1 + ( HMG_LEN( _aEstructura ) / 3 )
         _aEtiquetaV3[ i, ABM_LBL_CC ] := 1
         _aControlV3[ i, ABM_CON_CC ] := 1
      ELSEIF i <= 1 + ( 2 * HMG_LEN( _aEstructura ) / 3 )
         _aEtiquetaV3[ i, ABM_LBL_CC ] := 2
         _aControlV3[ i, ABM_CON_CC ] := 2
      ELSE
         _aEtiquetaV3[ i, ABM_LBL_CC ] := 3
         _aControlV3[ i, ABM_CON_CC ] := 3
      ENDIF
   NEXT

   // marcar las filas y las columnas en los controles y etiquetas
   nFila := 20 // valores iniciales
   nColumna := 20 // se pueden cambiar
   // --------------------------------------------------*
   // Horizontal
   // 2 columnas
   DefinePantalla( _aEtiquetaH2, _aControlH2, "H", 2, nFila, nColumna )
   GuardaArray ( "_aEtiquetaH2", "_aControlH2", "2h" )
   // 3 columnas
   DefinePantalla( _aEtiquetaH3, _aControlH3, "H", 3, nFila, nColumna )
   GuardaArray ( "_aEtiquetaH3", "_aControlH3", "3h" )

   // Vertical
   // 2 columnas
   DefinePantalla( _aEtiquetaV2, _aControlV2, "V", 2, nFila, nColumna )
   GuardaArray ( "_aEtiquetaV2", "_aControlV2", "2v" )
   // 3 columnas
   DefinePantalla( _aEtiquetaV3, _aControlV3, "V", 3, nFila, nColumna )
   GuardaArray ( "_aEtiquetaV3", "_aControlV3", "3v" )

RETURN // ABM3generar

// --------------------------------------------------

FUNCTION DefinePantalla( _aEtiqueta, _aControl, cHV, nCanCol, nFilaIni, nColumnaIni ) // Define posicion Etiquetas y Controles
   nColumna := nColumnaIni
   FOR nCol := 1 TO nCanCol // cicla c/columna     //for each Column
      nFila := nFilaIni
      nAnchoControl := 0
      nAnchoEtiqueta := 0
      // Busca la etiqueta más ancha
      FOR i := 1 TO HMG_LEN( _aEstructura ) // cicla cada etiqueta    //for each Label
         IF _aEtiqueta[ i, ABM_LBL_CC ] = nCol
            nAnchoEtiqueta := iif( nAnchoEtiqueta > ( HMG_LEN( _aNombreCampo[ i ] ) * 9 ), ;
               nAnchoEtiqueta, HMG_LEN( _aNombreCampo[ i ] ) * 9 )
         ENDIF
      NEXT

      FOR i := 1 TO HMG_LEN( _aEstructura ) // cicla cada etiqueta (y control)     //for each Field
         // Define las etiquetas
         IF _aEtiqueta[ i, ABM_LBL_CC ] = nCol
            _aEtiqueta[ i, ABM_LBL_NAME ] := "ABM3Etiqueta" + AllTrim( Str( i, 4, 0 ) )
            _aEtiqueta[ i, ABM_LBL_ROW ] := nFila
            _aEtiqueta[ i, ABM_LBL_COL ] := nColumna
            _aEtiqueta[ i, ABM_LBL_WIDTH ] := HMG_LEN( _aNombreCampo[ i ] ) * 9
            _aEtiqueta[ i, ABM_LBL_HEIGHT ] := 25
         ENDIF
         // Define los controles
         IF _aControl[ i, ABM_CON_CC ] = nCol
            DO CASE
            CASE _aEstructura[ i, DBS_TYPE ] == "C"
               _aControl[ i, ABM_CON_NAME ] := "ABM3Control" + AllTrim( Str( i, 4, 0 ) )
               _aControl[ i, ABM_CON_ROW ] := nFila
               _aControl[ i, ABM_CON_COL ] := nColumna + nAnchoEtiqueta + 0
               IF ( _aEstructura[ i, DBS_LEN ] * 9 ) < 50
                  _aControl[ i, ABM_CON_WIDTH ] := 50
               ELSEIF ( _aEstructura[ i, DBS_LEN ] * 9 ) > 270 // added
                  _aControl[ i, ABM_CON_WIDTH ] := 270
               ELSE
                  _aControl[ i, ABM_CON_WIDTH ] := _aEstructura[ i, DBS_LEN ] * 9
               ENDIF
               _aControl[ i, ABM_CON_HEIGHT ] := 25
               _aControl[ i, ABM_CON_DES ] := aAvisoCampo[ i ]
               _aControl[ i, ABM_CON_TYPE ] := ABM_TEXTBOXC
               nFila += 35
            CASE _aEstructura[ i, DBS_TYPE ] == "D"
               _aControl[ i, ABM_CON_NAME ] := "ABM3Control" + AllTrim( Str( i, 4, 0 ) )
               _aControl[ i, ABM_CON_ROW ] := nFila
               _aControl[ i, ABM_CON_COL ] := nColumna + nAnchoEtiqueta + 0
               _aControl[ i, ABM_CON_WIDTH ] := _aEstructura[ i, DBS_LEN ] * 9
               _aControl[ i, ABM_CON_HEIGHT ] := 25
               _aControl[ i, ABM_CON_DES ] := aAvisoCampo[ i ]
               _aControl[ i, ABM_CON_TYPE ] := ABM_DATEPICKER
               nFila += 35
            CASE _aEstructura[ i, DBS_TYPE ] == "N"
               _aControl[ i, ABM_CON_NAME ] := "ABM3Control" + AllTrim( Str( i, 4, 0 ) )
               _aControl[ i, ABM_CON_ROW ] := nFila
               _aControl[ i, ABM_CON_COL ] := nColumna + nAnchoEtiqueta + 0
               _aControl[ i, ABM_CON_WIDTH ] := iif( ( _aEstructura[ i, DBS_LEN ] * 9 ) < 50, 50, _aEstructura[ i, DBS_LEN ] * 10 )
               _aControl[ i, ABM_CON_HEIGHT ] := 25
               _aControl[ i, ABM_CON_DES ] := aAvisoCampo[ i ]
               _aControl[ i, ABM_CON_TYPE ] := ABM_TEXTBOXN
               nFila += 35
            CASE _aEstructura[ i, DBS_TYPE ] == "L"
               _aControl[ i, ABM_CON_NAME ] := "ABM3Control" + AllTrim( Str( i, 4, 0 ) )
               _aControl[ i, ABM_CON_ROW ] := nFila
               _aControl[ i, ABM_CON_COL ] := nColumna + nAnchoEtiqueta + 0
               _aControl[ i, ABM_CON_WIDTH ] := 25
               _aControl[ i, ABM_CON_HEIGHT ] := 25
               _aControl[ i, ABM_CON_DES ] := aAvisoCampo[ i ]
               _aControl[ i, ABM_CON_TYPE ] := ABM_CHECKBOX
               nFila += 35
            CASE _aEstructura[ i, DBS_TYPE ] == "M"
               _aControl[ i, ABM_CON_NAME ] := "ABM3Control" + AllTrim( Str( i, 4, 0 ) )
               _aControl[ i, ABM_CON_ROW ] := nFila
               _aControl[ i, ABM_CON_COL ] := nColumna + nAnchoEtiqueta + 0
               _aControl[ i, ABM_CON_WIDTH ] := 300
               _aControl[ i, ABM_CON_HEIGHT ] := 70
               _aControl[ i, ABM_CON_DES ] := aAvisoCampo[ i ]
               _aControl[ i, ABM_CON_TYPE ] := ABM_EDITBOX
               nFila += 80
            ENDCASE
            nAnchoControl := iif( nAnchoControl > _aControl[ i, ABM_CON_WIDTH ], nAnchoControl, _aControl[ i, ABM_CON_WIDTH ] )
         ENDIF // solamente si el control (Etiqueta) esta en esta columna.
      NEXT // HMG_LEN (cada campo )     //for each field
      nColumna = nColumna + nAnchoEtiqueta + nAnchoControl + 70
   NEXT // nCol   cada columna     //for each colum

RETURN // DefinePantalla()


// -------------Save arrays----------------------------*
STATIC FUNCTION GuardaArray ( cEtiqueta, cControl, cExt2 )
   _aEditConfig := { _aEstructura, &cEtiqueta, &cControl }
   nErrorCode := 0
   ft_SaveArr( _aEditConfig, ( _cArea ) + "." + cExt2 + ".arr", nErrorCode ) // libhbnf.a
   IF nErrorCode != 0
      MsgInfo ( "Error writing array" )
      RETURN
   ELSE
      _aEditConfig := {} // don't need it any more
   ENDIF

RETURN // GuardaArray()


// -------------Proper----------------------------*
FUNCTION PROP( cInString )

   LOCAL nIter, cOutString, cThisChar, lCapNext
   lCapNext := .T.
   cOutString := ''
   // - loop for length of string
   FOR nIter = 1 TO Len( cInString )
      cThisChar := SUBST( cInString, nIter, 1 )
      // - if its not alpha,cap the next alpha character
      IF ! Upper( cThisChar ) $ "ABCDEFGHIJKLMNOPQRSTUVWXYZ'"
         lCapNext := .T.
      ELSE
         // - capitalise it or lower() it accordingly
         IF lCapNext
            cThisChar := Upper( cThisChar )
            lCapNext := .F.
         ELSE
            cThisChar := Lower( cThisChar )
         ENDIF
      ENDIF
      // - add it to the cOutString
      cOutString += cThisChar
   NEXT

RETURN cOutString
// : EOF: S_PROPER.PRG  (de superlib)
