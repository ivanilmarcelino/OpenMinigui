/*
 * Implementación del comando EDIT EXTENDED para la librería MiniGUI.
 * (c) Cristóbal Mollá [cemese@terra.es]
 *
 * - Descripción -
 * ===============
 *      EDIT EXTENDED, es un comando que permite realizar el mantenimiento de una bdd. En principio
 *      está diseñado para administrar bases de datos que usen los divers DBFNTX y DBFCDX,
 *      presentando otras bases de datos con diferentes drivers resultados inesperados.
 *
 * - Sintáxis -
 * ============
 *      Todos los parámetros del comando EDIT son opcionales.
 *
 *      EDIT EXTENDED                           ;
 *       [ WORKAREA cWorkArea ]                 ;
 *       [ TITLE cTitle ]                       ;
 *       [ FIELDNAMES acFieldNames ]            ;
 *       [ FIELDMESSAGES acFieldMessages ]      ;
 *       [ FIELDENABLED alFieldEnabled ]        ;
 *       [ TABLEVIEW alTableView ]              ;
 *       [ OPTIONS aOptions ]                   ;
 *       [ ON SAVE bSave ]                      ;
 *       [ ON FIND bFind ]                      ;
 *       [ ON PRINT bPrint ]
 *
 *      Si no se pasa ningún parámetro, el comando EDIT toma como bdd de trabajo la del
 *      area de trabajo actual.
 *
 *      [cWorkArea]             Cadena de texto con el nombre del alias de la base de datos
 *                              a editar. Por defecto el alias de la base de datos activa.
 *
 *      [cTitle]                Cadena de texto con el título de la ventana de visualización
 *                              de registros. Por defecto se toma el alias de la base de datos
 *                              activa.
 *
 *      [acFieldNames]          Matriz de cadenas de texto con el nombre descriptivo de los
 *                              campos de la base de datos. Tiene que tener el mismo número
 *                              de elementos que campos la bdd. Por defecto se toma el nombre
 *                              de los campos de la estructura de la bdd.
 *
 *      [acFieldMessages]       Matriz de cadenas de texto con el texto que aparaecerá en la
 *                              barra de estado cuando se este añadiento o editando un registro.
 *                              Tiene que tener el mismo numero de elementos que campos la bdd.
 *                              Por defecto se rellena con valores internos.
 *
 *      [alFieldEnabled]        Matriz de valores lógicos que indica si el campo referenciado
 *                              por la matriz esta activo durante la edición de registro. Tiene
 *                              que tener el mismo numero de elementos que campos la bdd. Por
 *                              defecto toma todos los valores como verdaderos ( .t. ).
 *
 *      [alTableView]           Matriz de valores lógicos que indica si el campo referenciado
 *                              por la matriz es visible en la tabla. Tiene que tener el mismo
 *                              numero de elementos que campos la bdd. Por defecto toma todos
 *                              los valores como verdaderos ( .t. ).
 *
 *                              Array of logical values that it indicates if the referenced field
 *                              by the array is visible in the table. It must have the same one
 *                              number of elements as fields count of edited dbf.
 *                              By default it takes all the values as true (t.).
 *
 *      [aOptions]              Matriz de 2 niveles. el primero de tipo texto es la descripción
 *                              de la opción. El segundo de tipo bloque de código es la opción
 *                              a ejecutar cuando se selecciona. Si no se pasa esta variable,
 *                              se desactiva la lista desplegable de opciones.
 *
 *                              Array of 2 elements subarrays. First element( character type)
 *                              it is the description of the option. The second (codeblock type)
 *                              will evaluate when it is selected. If this variable does not go,
 *                              the drop-down list of options is deactivated.
 *                              By default this array is empty.
 *
 *      [bSave]                 Bloque de codigo con el formato {|aValores, lNuevo| Accion } que
 *                              se ejecutará al pulsar la tecla de guardar registro. Se pasan
 *                              las variables aValores con el contenido del registro a guardar y
 *                              lNuevo que indica si se está añadiendo (.t.) o editando (.f.).
 *                              Esta variable ha de devolver .t. para salir del modo de edición.
 *                              Por defecto se graba con el código de la función.
 *
 *      [bFind]                 Bloque de codigo a ejecutar cuando se pulsa la tecla de busqueda.
 *                              Por defecto se usa el código de la función.
 *
 *      [bPrint]                Bloque de código a ejecutar cuando se pulsa la tecla de listado.
 *                              Por defecto se usa el codigo de la función.
 *
 *
 * - Historial -
 * =============
 *      Mar 03  - Definición de la función.
 *              - Pruebas.
 *              - Soporte para lenguaje en inglés.
 *              - Corregido bug al borrar en bdds con CDX.
 *              - Mejora del control de parámetros.
 *              - Mejorada la función de de busqueda.
 *              - Soprte para multilenguaje.
 *              - Versión 1.0 lista.
 *      Abr 03  - Corregido bug en la función de busqueda (Nombre del botón).
 *              - Añadido soporte para idioma Ruso (Grigory Filiatov).
 *              - Añadido soporte para idioma Catalán (Por corregir).
 *              - Añadido soporte para idioma Portugués (Clovis Nogueira Jr).
 *              - Añadido soporte para idioma Polaco (Janusz Poura).
 *              - Añadido soporte para idioma Francés (C. Jouniauxdiv).
 *      May 03  - Añadido soporte para idioma Italiano (Lupano Piero).
 *              - Añadido soporte para idioma Alemán (Janusz Poura).
 *              - Cambio del formato de llamada al comando.
 *              - La creación de ventanas se realiza en función del alto y ancho
 *                de la pantalla.
 *              - Se elimina la restricción de tamaño en los nombre de etiquetas.
 *              - Se elimina la restricción de numero de campos del area de la bdd.
 *              - Se elimina la restricción de que los campos tipo MEMO tienen que ir
 *                al final de la base de datos.
 *              - Se añade la opción de visualizar comentarios en la barra de estado.
 *              - Se añade opción de control de visualización de campos en el browse.
 *              - Se modifica el parámetro nombre del area de la bdd que pasa a ser
 *                opcional.
 *              - Se añade la opción de saltar al siguiente foco mediante la pulsación
 *                de la tecla ENTER (Solo a controles tipo TEXTBOX).
 *              - Se añade la opción de cambio del indice activo.
 *              - Mejora de la rutina de busqueda.
 *              - Mejora en la ventana de definición de listados.
 *              - Pequeños cambios en el formato del listado.
 *              - Actualización del soporte multilenguaje.
 *      Jun 03  - Pruebas de la versión 1.5
 *              - Se implementan las nuevas opciones de la librería de Ryszard Rylko
 *              - Se implementa el filtrado de la base de datos.
 *      Ago 03  - Se corrige bug en establecimiento de filtro.
 *              - Actualizado soporte para italiano (Arcangelo Molinaro).
 *              - Actualizado soporte multilenguage.
 *              - Actualizado el soporte para filtrado.
 *      Sep 03  - Idioma Vasco listo. Gracias a Gerardo Fernández.
 *              - Idioma Italaino listo. Gracias a Arcangelo Molinaro.
 *              - Idioma Francés listo. Gracias a Chris Jouniauxdiv.
 *              - Idioma Polaco listo. Gracias a Jacek Kubica.
 *      Oct 03  - Solucionado problema con las clausulas ON FIND y ON PRINT, ahora
 *                ya tienen el efecto deseado. Gracias a Grigory Filiatov.
 *              - Se cambia la referencia a _ExtendedNavigation por _HMG_ExtendedNavigation
 *                para adecuarse a la sintaxis de la construción 76.
 *              - Idioma Alemán listo. Gracias a Andreas Wiltfang.
 *      Nov 03  - Problema con dbs en set exclusive. Gracias a cas_minigui.
 *              - Problema con tablas con pocos campos. Gracias a cas_minigui.
 *              - Cambio en demo para ajustarse a nueva sintaxis RDD Harbour (DBFFPT).
 *      Dic 03  - Ajuste de la longitud del control para fecha. Gracias a Laszlo Henning.
 *      Ene 04  - Problema de bloqueo con SET DELETED ON. Gracias a Grigory Filiatov y Roberto L¢pez.
 *
 *
 * - Limitaciones -
 * ================
 *      - No se pueden realizar busquedas por campos lógico o memo.
 *      - No se pueden realizar busquedas en indices con claves compuestas, la busqueda
 *        se realiza por el primer campo de la clave compuesta.
 *
 *
 * - Por hacer -
 * =============
 *      - Implementar busqueda del siguiente registro.
 *
 */

// Ficheros de definiciones.---------------------------------------------------
#include "minigui.ch"
#include "dbstruct.ch"
#include "winprint.ch"

// Declaración de definiciones.------------------------------------------------
// Generales.
#define ABM_CRLF                HB_OsNewLine()

// Estructura de la etiquetas.
#define ABM_LBL_LEN             5
#define ABM_LBL_NAME            1
#define ABM_LBL_ROW             2
#define ABM_LBL_COL             3
#define ABM_LBL_WIDTH           4
#define ABM_LBL_HEIGHT          5

// Estructura de los controles de edición.
#define ABM_CON_LEN             7
#define ABM_CON_NAME            1
#define ABM_CON_ROW             2
#define ABM_CON_COL             3
#define ABM_CON_WIDTH           4
#define ABM_CON_HEIGHT          5
#define ABM_CON_DES             6
#define ABM_CON_TYPE            7

// Tipos de controles de edición.
#define ABM_TEXTBOXC            1
#define ABM_TEXTBOXN            2
#define ABM_DATEPICKER          3
#define ABM_CHECKBOX            4
#define ABM_EDITBOX             5
#define ABM_COMBOBOX            6   // added 20170214

// Estructura de las opciones de usuario.
#define ABM_OPC_TEXTO           1
#define ABM_OPC_BLOQUE          2

// Tipo de acción al definir las columnas del listado.
#define ABM_LIS_ADD             1
#define ABM_LIS_DEL             2

// Tipo de acción al definir los registros del listado.
#define ABM_LIS_SET1            1
#define ABM_LIS_SET2            2


#define IS_SQLRDD ( Select() > 0 .AND. ( RddName()=="SQLRDD" .OR. RddName()=="SQLEX" ) )
#xtranslate Alltrim( Str( <i> ) ) => hb_NtoS( <i> )


// Declaración de variables globales.------------------------------------------
STATIC _cArea /*as character*/ // Nombre del area de la bdd.
STATIC _aEstructura /*as array*/ // Estructura de la bdd.
STATIC _aIndice /*as array*/ // Nombre de los indices de la bdd.
STATIC _aIndiceCampo /*as array*/ // Número del campo indice.
STATIC _nIndiceActivo /*as array*/ // Indice activo.
STATIC _aNombreCampo /*as array*/ // Nombre desciptivo de los campos de la bdd.
STATIC _aEditable /*as array*/ // Indicador de si son editables.
STATIC _cTitulo /*as character*/ // Título de la ventana.
STATIC _nAltoPantalla /*as numeric*/ // Alto de la pantalla.
STATIC _nAnchoPantalla /*as numeric*/ // Ancho de la pantalla.
STATIC _aEtiqueta /*as array*/ // Datos de las etiquetas.
STATIC _aControl /*as array*/ // Datos de los controles.
STATIC _aCampoTabla /*as array*/ // Nombre de los campos para la tabla.
STATIC _aAnchoTabla /*as array*/ // Anchos de los campos para la tabla.
STATIC _aCabeceraTabla /*as array*/ // Texto de las columnas de la tabla.
STATIC _aAlineadoTabla /*as array*/ // Alineación de las columnas de la tabla.
STATIC _aVisibleEnTabla /*as array*/ // Campos visibles en la tabla.
STATIC _nControlActivo /*as numeric*/ // Control con foco.
STATIC _aOpciones /*as array*/ // Opciones del usuario.
STATIC _bGuardar /*as codeblock*/ // Acción para guardar registro.
STATIC _bBuscar /*as codeblock*/ // Acción para buscar registro.
STATIC _bImprimir /*as codeblock*/ // Acción para imprimir listado.
STATIC _lFiltro /*as logical*/ // Indicativo de filtro activo.
STATIC _cFiltro /*as character*/ // Condición de filtro.

STATIC _cArrDescrip /*as character*/ // Name of the array descript file  *.arr
STATIC _aEditConfig /*as array*/ // Estructura de la bdd. + Labels + Controls  //add 20151217
STATIC aCombos
STATIC lArrDescrip
STATIC cTable
STATIC cField
MEMVAR cArrDescrip

/****************************************************************************************
 *  Aplicación: Comando EDIT para MiniGUI
 *       Autor: Cristóbal Mollá [cemese@terra.es]
 *     Función: ABM3()
 * Descripción: Función inicial. Comprueba los parámetros pasados, crea la estructura
 *              para las etiquetas y controles de edición y crea la ventana de visualización
 *              de registro.
 *  Parámetros: cArea           Nombre del area de la bdd. Por defecto se toma el area
 *                              actual.
 *              cTitulo         Título de la ventana de edición. Por defecto se toma el
 *                              nombre de la base de datos actual.
 *              aNombreCampo    Matriz de valores carácter con los nombres descriptivos de
 *                              los campos de la bdd.
 *              aAvisoCampo     Matriz con los textos que se presentarán en la barra de
 *                              estado al editar o añadir un registro.
 *              aEditable       Matriz de valóre lógicos que indica si el campo referenciado
 *                              esta activo en la ventana de edición de registro.
 *              aVisibleEnTabla Matriz de valores lógicos que indica la visibilidad de los
 *                              campos del browse de la ventana de edición.
 *              aOpciones       Matriz con los valores de las opciones de usuario.
 *              bGuardar        Bloque de código para la acción de guardar registro.
 *              bBuscar         Bloque de código para la acción de buscar registro.
 *              bImprimir       Bloque de código para la acción imprimir.
 *    Devuelve: NIL
****************************************************************************************/
FUNCTION ABM3( cArea, cTitulo, aNombreCampo, ;
      aAvisoCampo, aEditable, aVisibleEnTabla, ;
      aOpciones, bGuardar, bBuscar, ;
      bImprimir, cArrDescriptor, aComboBox )

   // ------- Declaración de variables locales.-----------------------------------
   LOCAL i, j /*as numeric*/ // Indice de iteración.
   LOCAL k /*as numeric*/ // Indice de iteración.
   LOCAL nArea AS NUMERIC // Numero del area de la bdd.
   LOCAL nRegistro AS NUMERIC // Número de regisrto de la bdd.
   LOCAL nEstructura /*as numeric*/ // Ancho del estructura de la bdd.
   LOCAL lSalida /*as logical*/ // Control de bucle.
   LOCAL nVeces /*as numeric*/ // Indice de iteración.
   LOCAL cIndice AS character // Nombre del indice.
   LOCAL cIndiceActivo AS character // Nombre del indice activo.
   LOCAL cClave AS character // Clave del indice.
   LOCAL nInicio /*as numeric*/ // Inicio de la cadena de busqueda.
   LOCAL nAnchoCampo /*as numeric*/ // Ancho del campo actual.
   LOCAL nAnchoEtiqueta /*as numeric*/ // Ancho máximo de las etiquetas.
   LOCAL nFila /*as numeric*/ // Fila de creación del control de edición.
   LOCAL nColumna /*as numeric*/ // Columna de creación del control de edición.
   LOCAL aTextoOp /*as numeric*/ // Texto de las opciones de usuario.
   LOCAL _BakExtendedNavigation // Estado de SET NAVIAGTION.
   LOCAL _BackDeleted // Estado de SET DELETED.
   LOCAL cFiltroAnt AS character // Condición del filtro anterior.
   LOCAL nErrorCode

   PUBLIC cArrDescrip := cArrDescriptor

   // ------- Gusrdar estado actual de SET DELETED y activarlo
   _BackDeleted := Set( _SET_DELETED )
   SET DELETED OFF

   // ------- Inicialización del soporte multilenguaje.---------------------------
   InitMessages()

   // ------- Desactivación de SET NAVIGATION.------------------------------------
   _BakExtendedNavigation := _HMG_ExtendedNavigation
   _HMG_ExtendedNavigation := .F.

   // ------- Control de parámetros.----------------------------------------------
   // Area de la base de datos.
   IF ValType( cArea ) != "C" .OR. Empty( cArea )
      _cArea := Alias()
      IF _cArea == ""
         AlertExclamation( _HMG_aLangUser[ 1 ], "EDIT EXTENDED" )
         RETURN NIL
      ENDIF
   ELSE
      _cArea := cArea
   ENDIF
   _aEstructura := ( _cArea )->( dbStruct() )
   nEstructura := Len( _aEstructura )

   lArrDescrip := .F. ;  nErrorCode := 0
   IF ( ValType( cArrDescrip ) = "C" ) .AND. File( cArrDescrip ) // There is a Description file
      _aEditConfig := ft_RestArr( cArrDescrip, @nErrorCode )
      IF nErrorCode = 0
         lArrDescrip := .T. // Read it ok
      ELSE
         MsgInfo ( "Error restoring array: " + cArrDescrip ) // Error reading. Continue in compatibility mode
      ENDIF
   ENDIF

   aCombos := aComboBox
   if ! Empty( aCombos ) .AND. ( ValType( aCombos ) = "A" ) .AND. ( Len( aCombos ) = Len( _aEstructura ) )
      FOR i := 1 TO Len( aCombos )
         IF ValType( aCombos[ i ] ) = "C"
            aCombos[ i ] := AllTrim( aCombos[ i ] ) // seria mejor eliminar todos los espacios?
            cTable := BeforAtNum( "-", aCombos[ i ], 1 )
            cField := AfterAtNum ( ">", aCombos[ i ], 1 )
            IF &cTable->( FieldPos( cField ) ) = 0
               MsgBox( "Field does not exist in:  " + cTable, cField )
               aCombos[ i ] := NIL
            ENDIF
         ELSEIF ValType( aCombos[ i ] ) = "A" // es ListBox
            FOR j := 1 TO Len( aCombos[ i ] )
               IF ValType( aCombos[ i, j ] ) != "C" .OR. Empty( aCombos[ i, j ] )
                  aCombos[ i, j ] := NIL
               ENDIF
            NEXT
         ELSE
            aCombos[ i ] := NIL
         ENDIF
      NEXT
   ELSE
      aCombos := Array( Len( _aEstructura ) )
   ENDIF

   // Título de la ventana.
   IF Empty( cTitulo ) .OR. ValType( cTitulo ) != "C"
      _cTitulo := _cArea
   ELSE
      _cTitulo := cTitulo
   ENDIF

   // Nombres de los campos.
   lSalida := .T.
   IF ValType( aNombreCampo ) != "A"
      lSalida := .F.
   ELSE
      IF Len( aNombreCampo ) != nEstructura
         lSalida := .F.
      ELSE
         FOR i := 1 TO Len( aNombreCampo )
            IF ValType( aNombreCampo[ i ] ) != "C"
               lSalida := .F.
               EXIT
            ENDIF
         NEXT
      ENDIF
   ENDIF
   IF lSalida
      _aNombreCampo := aNombreCampo
   ELSE
      _aNombreCampo := {}
      FOR i := 1 TO nEstructura
         AAdd( _aNombreCampo, Upper( Left( _aEstructura[ i, DBS_NAME ], 1 ) ) + ;
            Lower( SubStr( _aEstructura[ i, DBS_NAME ], 2 ) ) )
      NEXT
   ENDIF

   // Texto de aviso en la barra de estado de la ventana de edición de registro.
   lSalida := .T.
   IF ValType( aAvisoCampo ) != "A"
      lSalida := .F.
   ELSE
      IF Len( aAvisoCampo ) != nEstructura
         lSalida := .F.
      ELSE
         FOR i := 1 TO Len( aAvisoCampo )
            IF ValType( aAvisoCampo[ i ] ) != "C"
               lSalida := .F.
               EXIT
            ENDIF
         NEXT
      ENDIF
   ENDIF
   if ! lSalida
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
   ENDIF

   // Campos visibles en la tabla de la ventana de visualización de registros.
   lSalida := .T.
   IF ValType( aVisibleEnTabla ) != "A"
      lSalida := .F.
   ELSE
      IF Len( aVisibleEnTabla ) != nEstructura
         lSalida := .F.
      ELSE
         FOR i := 1 TO Len( aVisibleEnTabla )
            IF ValType( aVisibleEnTabla[ i ] ) != "L"
               lSalida := .F.
               EXIT
            ENDIF
         NEXT
      ENDIF
   ENDIF
   IF lSalida
      _aVisibleEnTabla := aVisibleEnTabla
   ELSE
      _aVisibleEnTabla := {}
      FOR i := 1 TO nEstructura
         AAdd( _aVisibleEnTabla, .T. )
      NEXT
   ENDIF

   // Estado de los campos en la ventana de edición de registro.
   lSalida := .T.
   IF ValType( aEditable ) != "A"
      lSalida := .F.
   ELSE
      IF Len( aEditable ) != nEstructura
         lSalida := .F.
      ELSE
         FOR i := 1 TO Len( aEditable )
            IF ValType( aEditable[ i ] ) != "L"
               lSalida := .F.
               EXIT
            ENDIF
         NEXT
      ENDIF
   ENDIF
   IF lSalida
      _aEditable := aEditable
   ELSE
      _aEditable := {}
      FOR i := 1 TO nEstructura
         AAdd( _aEditable, .T. )
      NEXT
   ENDIF

   // Opciones del usuario.
   lSalida := .T.

   IF ValType( aOpciones ) != "A"
      lSalida := .F.
   ELSEIF Len( aOpciones ) < 1
      lSalida := .F.
   ELSEIF Len( aOpciones[ 1 ] ) != 2
      lSalida := .F.
   ELSE
      FOR i := 1 TO Len( aOpciones )
         IF ValType( aOpciones[ i, ABM_OPC_TEXTO ] ) != "C" .OR. ;
               ValType( aOpciones[ i, ABM_OPC_BLOQUE ] ) != "B"
            lSalida := .F.
            EXIT
         ENDIF
      NEXT
   ENDIF

   IF lSalida
      _aOpciones := aOpciones
   ELSE
      _aOpciones := {}
   ENDIF

   // Acción al guardar.
   IF ValType( bGuardar ) == "B"
      _bGuardar := bGuardar
   ELSE
      _bGuardar := NIL
   ENDIF

   // Acción al buscar.
   IF ValType( bBuscar ) == "B"
      _bBuscar := bBuscar
   ELSE
      _bBuscar := NIL
   ENDIF

   // Acción al imprimir.
   IF ValType( bImprimir ) == "B"
      _bImprimir := bImprimir
   ELSE
      _bImprimir := NIL
   ENDIF

   // ------- Selección del area de la bdd.---------------------------------------
   nRegistro := ( _cArea )->( RecNo() )
   nArea := Select()
   cIndiceActivo := ( _cArea )->( ordSetFocus() )
   cFiltroAnt := ( _cArea )->( dbFilter() )
   dbSelectArea( _cArea )
   ( _cArea )->( dbGoTop() )

   // ------- Inicialización de variables.----------------------------------------
   // Filtro.
   _cFiltro := cFiltroAnt
   _lFiltro := !( Empty( _cFiltro ) )

   // Indices de la base de datos.
   lSalida := .T.
   k := 1
   _aIndice := {}
   _aIndiceCampo := {}
   nVeces := 1
   AAdd( _aIndice, _HMG_aLangLabel[ 1 ] )
   AAdd( _aIndiceCampo, 0 )
   DO WHILE lSalida
      IF Empty( ( _cArea )->( ordName( k ) ) )
         lSalida := .F.
      ELSE
         cIndice := Upper( ( _cArea )->( ordName( k ) ) )
         AAdd( _aIndice, cIndice )
         cClave := Upper( ( _cArea )->( ordKey( k ) ) )
         FOR i := 1 TO nEstructura
            IF nVeces <= 1
               nInicio := At( _aEstructura[ i, DBS_NAME ], cClave )
               IF nInicio != 0
                  AAdd( _aIndiceCampo, i )
                  nVeces++
               ENDIF
            ENDIF
         NEXT
      ENDIF
      k++
      nVeces := 1
   ENDDO

   // Numero de indice.
   IF Empty( cIndiceActivo )
      _nIndiceActivo := 1
   ELSE
      _nIndiceActivo := AScan( _aIndice, Upper( ( _cArea )->( ordSetFocus() ) ) )
   ENDIF

   // Tamaño de la pantalla.
   _nAltoPantalla := GETDESKTOPREALHEIGHT()
   _nAnchoPantalla := GETDESKTOPREALWIDTH()

   IF lArrDescrip // There is a Description file
      _aEtiqueta := _aEditConfig[ 2 ] // use it
      _aControl := _aEditConfig[ 3 ]
      _aEditConfig := {}
   ELSE // calculate controls in standard way (1 col)
      // Datos de las etiquetas y los controles de la ventana de edición.
      _aEtiqueta := Array( nEstructura, ABM_LBL_LEN )
      _aControl := Array( nEstructura, ABM_CON_LEN )
      nFila := 10
      nColumna := 10
      nAnchoEtiqueta := 0
      FOR i := 1 TO Len( _aNombreCampo )
         nAnchoEtiqueta := iif( nAnchoEtiqueta > ( Len( _aNombreCampo[ i ] ) * 9 ), ;
            nAnchoEtiqueta, ;
            Len( _aNombreCampo[ i ] ) * 9 )
      NEXT
      FOR i := 1 TO nEstructura
         _aEtiqueta[ i, ABM_LBL_NAME ] := "ABM3Etiqueta" + AllTrim( Str( i ) )
         _aEtiqueta[ i, ABM_LBL_ROW ] := nFila
         _aEtiqueta[ i, ABM_LBL_COL ] := nColumna
         _aEtiqueta[ i, ABM_LBL_WIDTH ] := Len( _aNombreCampo[ i ] ) * 9
         _aEtiqueta[ i, ABM_LBL_HEIGHT ] := 25
         switch Left( _aEstructura[ i, DBS_TYPE ], 1 )
         CASE "C"
            _aControl[ i, ABM_CON_NAME ] := "ABM3Control" + AllTrim( Str( i ) )
            _aControl[ i, ABM_CON_ROW ] := nFila
            _aControl[ i, ABM_CON_COL ] := nColumna + nAnchoEtiqueta + 20
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
            EXIT
         CASE "D"
            _aControl[ i, ABM_CON_NAME ] := "ABM3Control" + AllTrim( Str( i ) )
            _aControl[ i, ABM_CON_ROW ] := nFila
            _aControl[ i, ABM_CON_COL ] := nColumna + nAnchoEtiqueta + 20
            _aControl[ i, ABM_CON_WIDTH ] := _aEstructura[ i, DBS_LEN ] * 10
            _aControl[ i, ABM_CON_HEIGHT ] := 25
            _aControl[ i, ABM_CON_DES ] := aAvisoCampo[ i ]
            _aControl[ i, ABM_CON_TYPE ] := ABM_DATEPICKER
            EXIT
         CASE "N"
            _aControl[ i, ABM_CON_NAME ] := "ABM3Control" + AllTrim( Str( i ) )
            _aControl[ i, ABM_CON_ROW ] := nFila
            _aControl[ i, ABM_CON_COL ] := nColumna + nAnchoEtiqueta + 20
            _aControl[ i, ABM_CON_WIDTH ] := iif( ( _aEstructura[ i, DBS_LEN ] * 9 ) < 50, 50, _aEstructura[ i, DBS_LEN ] * 10 )
            _aControl[ i, ABM_CON_HEIGHT ] := 25
            _aControl[ i, ABM_CON_DES ] := aAvisoCampo[ i ]
            _aControl[ i, ABM_CON_TYPE ] := ABM_TEXTBOXN
            EXIT
         CASE "L"
            _aControl[ i, ABM_CON_NAME ] := "ABM3Control" + AllTrim( Str( i ) )
            _aControl[ i, ABM_CON_ROW ] := nFila
            _aControl[ i, ABM_CON_COL ] := nColumna + nAnchoEtiqueta + 20
            _aControl[ i, ABM_CON_WIDTH ] := 25
            _aControl[ i, ABM_CON_HEIGHT ] := 25
            _aControl[ i, ABM_CON_DES ] := aAvisoCampo[ i ]
            _aControl[ i, ABM_CON_TYPE ] := ABM_CHECKBOX
            EXIT
         CASE "M"
            _aControl[ i, ABM_CON_NAME ] := "ABM3Control" + AllTrim( Str( i ) )
            _aControl[ i, ABM_CON_ROW ] := nFila
            _aControl[ i, ABM_CON_COL ] := nColumna + nAnchoEtiqueta + 20
            _aControl[ i, ABM_CON_WIDTH ] := 300
            _aControl[ i, ABM_CON_HEIGHT ] := 70
            _aControl[ i, ABM_CON_DES ] := aAvisoCampo[ i ]
            _aControl[ i, ABM_CON_TYPE ] := ABM_EDITBOX
            nFila += 45
         endswitch
         IF _aEstructura[ i, DBS_TYPE ] $ "CDNLM"
            nFila += 35
         ENDIF
      NEXT
   ENDIF // calcular los controles

   // Datos de la tabla de la ventana de visualización.

   _aCampoTabla := { "if(deleted(), 'X', ' ')" }
   _aAnchoTabla := { 20 }
   _aCabeceraTabla := { "X" }
   _aAlineadoTabla := { BROWSE_JTFY_LEFT }

   FOR i := 1 TO nEstructura
      IF _aVisibleEnTabla[ i ]
         AAdd( _aCampoTabla, _cArea + "->" + _aEstructura[ i, DBS_NAME ] )
         nAnchoCampo := iif( ( _aEstructura[ i, DBS_LEN ] * 10 ) < 50, 50, ;
            _aEstructura[ i, DBS_LEN ] * 10 )
         nAnchoEtiqueta := Len( _aNombreCampo[ i ] ) * 10
         AAdd( _aAnchoTabla, iif( nAnchoEtiqueta > nAnchoCampo, ;
            nAnchoEtiqueta, ;
            nAnchoCampo ) )
         AAdd( _aCabeceraTabla, _aNombreCampo[ i ] )

         IF _aEstructura[ i, DBS_TYPE ] == "L"
            AAdd( _aAlineadoTabla, BROWSE_JTFY_CENTER )
         ELSEIF _aEstructura[ i, DBS_TYPE ] == "D"
            AAdd( _aAlineadoTabla, BROWSE_JTFY_CENTER )
         ELSEIF _aEstructura[ i, DBS_TYPE ] == "N"
            AAdd( _aAlineadoTabla, BROWSE_JTFY_RIGHT )
         ELSE
            AAdd( _aAlineadoTabla, BROWSE_JTFY_LEFT )
         ENDIF
      ENDIF
   NEXT

   IF AScan( _HMG_aFormType, 'A' ) == 0
      _HMG_MainWindowFirst := .F.
   ENDIF

   // ------- Definición de la ventana de visualización.--------------------------
   DEFINE WINDOW wndABM3Edit ;
         AT 60, 30 ;
         WIDTH _nAnchoPantalla - 60 ;
         HEIGHT _nAltoPantalla - 140 ;
         TITLE _cTitulo ;
         MODAL ;
         NOSIZE ;
         NOSYSMENU ;
         ON INIT {|| ABM3Redibuja() } ;
         ON RELEASE {|| ABM3salir( nRegistro, cIndiceActivo, cFiltroAnt, nArea ) } ;
         FONT _GetSysFont() SIZE 9

      // Define la barra de estado de la ventana de visualización.
      DEFINE STATUSBAR FONT _GetSysFont() SIZE 9
         STATUSITEM _HMG_aLangLabel[ 19 ] // 1
         STATUSITEM _HMG_aLangLabel[ 20 ] WIDTH 100 raised // 2
         STATUSITEM _HMG_aLangLabel[ 2 ] + ': ' WIDTH 200 raised // 3
      END STATUSBAR

      // Define la barra de botones de la ventana de visualización.
      DEFINE TOOLBAR tbEdit buttonsize 100, 32 FLAT righttext BORDER
         BUTTON tbbCerrar CAPTION _HMG_aLangButton[ 1 ] ;
            PICTURE "MINIGUI_EDIT_CLOSE" ;
            ACTION wndABM3Edit.RELEASE
         BUTTON tbbNuevo CAPTION _HMG_aLangButton[ 2 ] ;
            PICTURE "MINIGUI_EDIT_NEW" ;
            ACTION {|| ABM3Editar( .T. ) }
         BUTTON tbbEditar CAPTION _HMG_aLangButton[ 3 ] ;
            PICTURE "MINIGUI_EDIT_EDIT" ;
            ACTION {|| ABM3Editar( .F. ) }
         BUTTON tbbBorrar CAPTION _HMG_aLangButton[ 4 ] ;
            PICTURE "MINIGUI_EDIT_DELETE" ;
            ACTION {|| ABM3Borrar() }
         BUTTON tbbRecover CAPTION _HMG_aLangButton[ 12 ] ;
            PICTURE "MINIGUI_EDIT_UNDO" ;
            ACTION {|| ABM3Recover() }
         BUTTON tbbBuscar CAPTION _HMG_aLangButton[ 5 ] ;
            PICTURE "MINIGUI_EDIT_FIND" ;
            ACTION {|| ABM3Buscar() }
         BUTTON tbbListado CAPTION _HMG_aLangButton[ 6 ] ;
            PICTURE "MINIGUI_EDIT_PRINT" ;
            ACTION {|| ABM3Imprimir() }
      END TOOLBAR

   END WINDOW

   // ------- Creación de los controles de la ventana de visualización.-----------
   @ 50, 10 FRAME frmEditOpciones ;
      OF wndABM3Edit ;
      CAPTION "" ;
      WIDTH wndABM3Edit.WIDTH - 25 ;
      HEIGHT 60
   @ 112, 10 FRAME frmEditTabla ;
      OF wndABM3Edit ;
      CAPTION "" ;
      WIDTH wndABM3Edit.WIDTH - 25 ;
      HEIGHT wndABM3Edit.HEIGHT - 165
   @ 60, 20 LABEL lblIndice ;
      OF wndABM3Edit ;
      VALUE _HMG_aLangUser[ 26 ] ;
      AUTOSIZE ;
      FONT _GetSysFont() SIZE 9
   @ 75, 20 COMBOBOX cbIndices ;
      OF wndABM3Edit ;
      ITEMS _aIndice ;
      VALUE _nIndiceActivo ;
      WIDTH 150 ;
      FONT _GetSysFont() SIZE 9 ;
      ON CHANGE {|| ABM3CambiarOrden() }
   nColumna := wndABM3Edit.WIDTH - 175
   aTextoOp := {}
   FOR i := 1 TO Len( _aOpciones )
      AAdd( aTextoOp, _aOpciones[ i, ABM_OPC_TEXTO ] )
   NEXT
   @ 60, nColumna LABEL lblOpciones ;
      OF wndABM3Edit ;
      VALUE _HMG_aLangLabel[ 5 ] ;
      AUTOSIZE ;
      FONT _GetSysFont() SIZE 9
   @ 75, nColumna COMBOBOX cbOpciones ;
      OF wndABM3Edit ;
      ITEMS aTextoOp ;
      VALUE 1 ;
      WIDTH 150 ;
      FONT _GetSysFont() SIZE 9 ;
      ON CHANGE {|| ABM3EjecutaOpcion() }
   @ 65, ( wndABM3Edit.Width / 2 ) - 110 BUTTON btnFiltro1 ;
      OF wndABM3Edit ;
      CAPTION _HMG_aLangButton[ 10 ] ;
      ACTION {|| ABM3ActivarFiltro() } ;
      WIDTH 100 ;
      HEIGHT 32 ;
      FONT _GetSysFont() SIZE 9
   @ 65, ( wndABM3Edit.Width / 2 ) + 5 BUTTON btnFiltro2 ;
      OF wndABM3Edit ;
      CAPTION _HMG_aLangButton[ 11 ] ;
      ACTION {|| ABM3DesactivarFiltro() } ;
      WIDTH 100 ;
      HEIGHT 32 ;
      FONT _GetSysFont() SIZE 9
   @ 132, 20 BROWSE brwABM3Edit ;
      OF wndABM3Edit ;
      WIDTH wndABM3Edit.WIDTH - 45 ;
      HEIGHT wndABM3Edit.HEIGHT - 195 ;
      HEADERS _aCabeceraTabla ;
      WIDTHS _aAnchoTabla ;
      WORKAREA &_cArea ;
      FIELDS _aCampoTabla ;
      VALUE ( _cArea )->( RecNo() ) ;
      FONT _GetSysFont() SIZE 9 ;
      ON CHANGE {|| ( _cArea )->( dbGoto( wndABM3Edit.brwABM3Edit.Value ) ), ;
      ABM3Redibuja( .F. ) } ;
      ON DBLCLICK {|| iif( wndABM3Edit.tbbEditar.Enabled, ABM3Editar( .F. ), ) } ;
      JUSTIFY _aAlineadoTabla paintdoublebuffer

   // Comprueba el estado de las opciones de usuario.
   IF Len( _aOpciones ) == 0
      wndABM3Edit.cbOpciones.Enabled := .F.
   ENDIF

   // ------- Activación de la ventana de visualización.--------------------------
   ACTIVATE WINDOW wndABM3Edit

   // ------- Restauración de SET NAVIGATION.-------------------------------------
   _HMG_ExtendedNavigation := _BakExtendedNavigation

   // ------- Restaurar SET DELETED a su valor inicial
   Set( _SET_DELETED, _BackDeleted )

RETURN NIL


/****************************************************************************************
 *  Aplicación: Comando EDIT para MiniGUI
 *       Autor: Cristóbal Mollá [cemese@terra.es]
 *     Función: ABM3salir()
 * Descripción: Cierra la ventana de visualización de registros y sale.
 *  Parámetros: Ninguno.
 *    Devuelve: NIL
****************************************************************************************/
STATIC FUNCTION ABM3salir( nRegistro, cIndiceActivo, cFiltroAnt, nArea )

   // ------- Restaura el area de la bdd inicial.---------------------------------
   ( _cArea )->( dbGoto( nRegistro ) )
   IF ! Empty( cIndiceActivo )
      ( _cArea )->( ordSetFocus( cIndiceActivo ) )
   ENDIF
   IF Empty( cFiltroAnt )
      ( _cArea )->( dbClearFilter() )
   ELSEIF ! IS_SQLRDD
      ( _cArea )->( dbSetFilter( &( "{||" + cFiltroAnt + "}" ), cFiltroAnt ) )
   ENDIF
   dbSelectArea( nArea )

RETURN NIL


/****************************************************************************************
 *  Aplicación: Comando EDIT para MiniGUI
 *       Autor: Cristóbal Mollá [cemese@terra.es]
 *     Función: ABM3Redibuja()
 * Descripción: Actualización de la ventana de visualización de registros.
 *  Parámetros: Ninguno
 *    Devuelve: NIL
****************************************************************************************/
STATIC FUNCTION ABM3Redibuja( lTabla )

   // ------- Declaración de variables locales.-----------------------------------
   LOCAL lDeleted /*as logical*/

   // ------- Control de parámetros.----------------------------------------------
   IF ValType( lTabla ) != "L"
      lTabla := .F.
   ENDIF

   // ------- Refresco de la barra de botones.------------------------------------
   IF ( _cArea )->( RecCount() ) == 0
      wndABM3Edit.tbbEditar.Enabled := .F.
      wndABM3Edit.tbbBorrar.Enabled := .F.
      wndABM3Edit.tbbBuscar.Enabled := .F.
      wndABM3Edit.tbbListado.Enabled := .F.
   ELSE
      lDeleted := Deleted()
      wndABM3Edit.tbbEditar.Enabled := ! lDeleted
      wndABM3Edit.tbbBorrar.Enabled := ! lDeleted
      wndABM3Edit.tbbBuscar.Enabled := .T.
      wndABM3Edit.tbbRecover.Enabled := lDeleted
      wndABM3Edit.tbbListado.Enabled := .T.
   ENDIF

   // ------- Refresco de la barra de estado.-------------------------------------
   wndABM3Edit.StatusBar.Item( 1 ) := _HMG_aLangLabel[ 19 ] + _cFiltro
   wndABM3Edit.StatusBar.Item( 2 ) := _HMG_aLangLabel[ 20 ] + iif( _lFiltro, _HMG_aLangUser[ 29 ], _HMG_aLangUser[ 30 ] )
   wndABM3Edit.StatusBar.Item( 3 ) := _HMG_aLangLabel[ 2 ] + ': ' + ;
      AllTrim( Str( ( _cArea )->( RecNo() ) ) ) + "/" + ;
      AllTrim( Str( ( _cArea )->( RecCount() ) ) )

   // ------- Refresca el browse si se indica.
   IF lTabla
      wndABM3Edit.brwABM3Edit.VALUE := ( _cArea )->( RecNo() )
      wndABM3Edit.brwABM3Edit.Refresh
   ENDIF

   // ------- Coloca el foco en el browse.
   wndABM3Edit.brwABM3Edit.SetFocus

RETURN NIL


/****************************************************************************************
 *  Aplicación: Comando EDIT para MiniGUI
 *       Autor: Cristóbal Mollá [cemese@terra.es]
 *     Función: ABM3CambiarOrden()
 * Descripción: Cambia el orden activo.
 *  Parámetros: Ninguno.
 *    Devuelve: NIL
****************************************************************************************/
STATIC FUNCTION ABM3CambiarOrden()

   // ------- Declaración de variables locales.-----------------------------------
   LOCAL nIndice /*as numeric*/ // Número del indice.

   // ------- Inicializa las variables.-------------------------------------------
   nIndice := wndABM3Edit.cbIndices.VALUE -1

   // ------- Cambia el orden del area de trabajo.--------------------------------
   ( _cArea )->( ordSetFocus( nIndice ) )
   IF IS_SQLRDD
      ( _cArea )->( dbGoTop() )
   ENDIF
   _nIndiceActivo := ++nIndice
   ABM3Redibuja( .T. )

RETURN NIL


/****************************************************************************************
 *  Aplicación: Comando EDIT para MiniGUI
 *       Autor: Cristóbal Mollá [cemese@terra.es]
 *     Función: ABM3EjecutaOpcion()
 * Descripción: Ejecuta las opciones del usuario.
 *  Parámetros: Ninguno
 *    Devuelve: NIL
****************************************************************************************/
STATIC FUNCTION ABM3EjecutaOpcion()

   // ------- Declaración de variables locales.-----------------------------------
   LOCAL nItem /*as numeric*/ // Numero del item seleccionado.
   LOCAL bBloque AS codebloc // Bloque de codigo a ejecutar.

   // ------- Inicialización de variables.----------------------------------------
   nItem := wndABM3Edit.cbOpciones.VALUE
   bBloque := _aOpciones[ nItem, ABM_OPC_BLOQUE ]

   // ------- Ejecuta la opción.--------------------------------------------------
   Eval( bBloque )

   // ------- Refresca el browse.
   ABM3Redibuja( .T. )

RETURN NIL


/****************************************************************************************
 *  Aplicación: Comando EDIT para MiniGUI
 *       Autor: Cristóbal Mollá [cemese@terra.es]
 *     Función: ABM3Editar( lNuevo )
 * Descripción: Creación de la ventana de edición de registro.
 *  Parámetros: lNuevo          Valor lógico que indica si se está añadiendo un registro
 *                              o editando el actual.
 *    Devuelve: NIL
****************************************************************************************/
STATIC FUNCTION ABM3Editar( lNuevo )

   // ------- Declaración de variables locales.-----------------------------------
   LOCAL i AS NUMERIC // Indice de iteración.
   LOCAL nRegistro AS NUMERIC // Número de regisrto de la bdd.
   LOCAL nAnchoEtiqueta /*as numeric*/ // Ancho máximo de las etiquetas.
   LOCAL nAltoControl /*as numeric*/ // Alto total de los controles de edición.
   LOCAL nAncho /*as numeric*/ // Ancho de la ventana de edición .
   LOCAL nAlto /*as numeric*/ // Alto de la ventana de edición.
   LOCAL nAnchoTope /*as numeric*/ // Ancho máximo de la ventana de edición.
   LOCAL nAltoTope /*as numeric*/ // Alto máximo de la ventana de edición.
   LOCAL nAnchoSplit /*as numeric*/ // Ancho de la ventana Split.
   LOCAL nAltoSplit /*as numeric*/ // Alto de la ventana Split.
   LOCAL cTitulo AS character // Título de la ventana.
   LOCAL cMascara AS character // Máscara de edición de los controles numéricos.
   LOCAL nAnchoControl /*as numeric*/
   LOCAL Wider
   LOCAL aMedidas
   LOCAL cFuncName
   LOCAL aItems
   LOCAL cThisOne
   LOCAL cReferTable
   LOCAL cValor
   LOCAL nItem
   LOCAL abKeyBlocks

   // ------- Control de parámetros.----------------------------------------------
   IF ValType( lNuevo ) != "L"
      lNuevo := .T.
   ENDIF

   // ------- Incialización de variables.-----------------------------------------
   nAnchoEtiqueta := 0
   nAnchoControl := 0
   nAltoControl := 0
   IF lArrDescrip
      Wider := .F.
      aMedidas := Array ( 12 )
      AFill ( aMedidas, 0 )
      FOR i := 1 TO Len( _aEstructura )
         IF _aControl[ i, 8 ] = 1 // 8=ABM_CON_CC  (column)
            // increm alto etiq 1
            aMedidas[ 1 ] += _aEtiqueta[ i, ABM_LBL_HEIGHT ] + 10
            // increm alto contrs 1
            aMedidas[ 2 ] += _aControl[ i, ABM_CON_HEIGHT ] + 10
            // elija ancho etiq mayor col 1
            aMedidas[ 3 ] := iif( aMedidas[ 3 ] > _aEtiqueta[ i, ABM_LBL_WIDTH ], ;
               aMedidas[ 3 ], _aEtiqueta[ i, ABM_LBL_WIDTH ] )
            // elija ancho contrs mayor col 1
            aMedidas[ 4 ] := iif( aMedidas[ 4 ] > _aControl[ i, ABM_CON_WIDTH ], ;
               aMedidas[ 4 ], _aControl[ i, ABM_CON_WIDTH ] )
         ELSEIF _aControl[ i, 8 ] = 2 // 8=ABM_CON_CC
            // increm alto etiq 2
            aMedidas[ 5 ] += _aEtiqueta[ i, ABM_LBL_HEIGHT ] + 10
            // increm alto contrs 2
            aMedidas[ 6 ] += _aControl[ i, ABM_CON_HEIGHT ] + 10
            // elija ancho etiq mayor col 2
            aMedidas[ 7 ] := iif( aMedidas[ 7 ] > _aEtiqueta[ i, ABM_LBL_WIDTH ], ;
               aMedidas[ 7 ], _aEtiqueta[ i, ABM_LBL_WIDTH ] )
            // elija ancho contrs mayor col 2
            aMedidas[ 8 ] := iif( aMedidas[ 8 ] > _aControl[ i, ABM_CON_WIDTH ], ;
               aMedidas[ 8 ], _aControl[ i, ABM_CON_WIDTH ] )
         ELSEIF _aControl[ i, 8 ] = 3
            Wider := .T.
            // increm alto etiq 3
            aMedidas[ 9 ] += _aEtiqueta[ i, ABM_LBL_HEIGHT ] + 10
            // increm alto contrs 3
            aMedidas[ 10 ] += _aControl[ i, ABM_CON_HEIGHT ] + 10
            // elija ancho etiq mayor col 3
            aMedidas[ 11 ] := iif( aMedidas[ 11 ] > _aEtiqueta[ i, ABM_LBL_WIDTH ], ;
               aMedidas[ 11 ], _aEtiqueta[ i, ABM_LBL_WIDTH ] )
            // elija ancho contrs mayor col 3
            aMedidas[ 12 ] := iif( aMedidas[ 12 ] > _aControl[ i, ABM_CON_WIDTH ], ;
               aMedidas[ 12 ], _aControl[ i, ABM_CON_WIDTH ] )
         ENDIF
      NEXT
      aMedidas[ 4 ] := iif( aMedidas[ 4 ] > 300, 300, aMedidas[ 4 ] )
      aMedidas[ 8 ] := iif( aMedidas[ 8 ] > 300, 300, aMedidas[ 8 ] )
      // Buscar el mayor alto-maximo   (etiq y cntr) -->  alto maximo
      nAltoControl := iif( aMedidas[ 1 ] > aMedidas[ 2 ], aMedidas[ 1 ], aMedidas[ 2 ] )
      nAltoControl := iif( aMedidas[ 5 ] > nAltoControl, aMedidas[ 5 ], nAltoControl )
      nAltoControl := iif( aMedidas[ 6 ] > nAltoControl, aMedidas[ 6 ], nAltoControl )
      nAltoControl := iif( aMedidas[ 9 ] > nAltoControl, aMedidas[ 9 ], nAltoControl )
      nAltoControl := iif( aMedidas[ 10 ] > nAltoControl, aMedidas[ 10 ], nAltoControl )
      nAltoControl += 10
      // Sumar todos los ancho-maximos (etiq y cntr)
      nAnchoEtiqueta := aMedidas[ 3 ] + aMedidas[ 7 ] + aMedidas[ 11 ]
      nAnchoControl := aMedidas[ 4 ] + aMedidas[ 8 ] + aMedidas[ 12 ]
      nAnchoControl += iif( wider, 160, 70 )
   ELSE // legacy
      FOR i := 1 TO Len( _aEtiqueta )
         nAnchoEtiqueta := iif( nAnchoEtiqueta > _aEtiqueta[ i, ABM_LBL_WIDTH ], ;
            nAnchoEtiqueta, ;
            _aEtiqueta[ i, ABM_LBL_WIDTH ] )
         nAnchoControl := iif( nAnchoControl > _aControl[ i, ABM_CON_WIDTH ], ;
            nAnchoControl, ;
            _aControl[ i, ABM_CON_WIDTH ] )
         nAltoControl += _aControl[ i, ABM_CON_HEIGHT ] + 10
      NEXT
   ENDIF
   nAltoSplit := 10 + nAltoControl + 25
   nAnchoSplit := 10 + nAnchoEtiqueta + 10 + nAnchoControl + 25
   nAlto := 80 + nAltoSplit + 15 + iif( _HMG_IsThemed, 10, 0 )
   nAltoTope := _nAltoPantalla - 130
   nAncho := 15 + nAnchoSplit + 15
   nAncho := iif( nAncho < 300, 300, nAncho )
   nAnchoTope := _nAnchoPantalla - 60
   cTitulo := iif( lNuevo, _HMG_aLangLabel[ 6 ], _HMG_aLangLabel[ 7 ] )

   // ------- Define la ventana de edición de registro.---------------------------
   DEFINE WINDOW wndABM3EditNuevo ;
         AT 70, 40 ;
         WIDTH iif( nAncho > nAnchoTope, nAnchoTope, nAncho ) ;
         HEIGHT iif( nAlto > nAltoTope, nAltoTope, nAlto ) ;
         TITLE cTitulo ;
         MODAL ;
         NOSIZE ;
         NOSYSMENU ;
         FONT _GetSysFont() SIZE 9

      // Define la barra de estado de la ventana de edición de registro.
      DEFINE STATUSBAR FONT _GetSysFont() SIZE 9
         STATUSITEM ""
      END STATUSBAR

      DEFINE SPLITBOX

         // Define la barra de botones de la ventana de edición de registro.
         DEFINE TOOLBAR tbEditNuevo buttonsize 100, 32 FLAT righttext
            BUTTON tbbCancelar CAPTION _HMG_aLangButton[ 7 ] ;
               PICTURE "MINIGUI_EDIT_CANCEL" ;
               ACTION wndABM3EditNuevo.RELEASE
            BUTTON tbbAceptar CAPTION _HMG_aLangButton[ 8 ] ;
               PICTURE "MINIGUI_EDIT_OK" ;
               ACTION ABM3EditarGuardar( lNuevo )
            BUTTON tbbCopiar CAPTION _HMG_aLangButton[ 9 ] ;
               PICTURE "MINIGUI_EDIT_COPY" ;
               ACTION ABM3EditarCopiar()
         END TOOLBAR

         // Define la ventana donde van contenidos los controles de edición.
         DEFINE WINDOW wndABM3EditNuevoSplit ;
               WIDTH iif( nAncho > nAnchoTope, ;
               nAnchoTope - 10, ;
               nAnchoSplit - 1 ) ;
               HEIGHT iif( nAlto > nAltoTope, ;
               nAltoTope - 95, ;
               nAltoSplit - 1 ) ;
               VIRTUAL WIDTH nAnchoSplit ;
               VIRTUAL HEIGHT nAltoSplit ;
               SPLITCHILD ;
               NOCAPTION ;
               FONT _GetSysFont() SIZE 9 ;
               FOCUSED
         END WINDOW
      END SPLITBOX
   END WINDOW

   // ------- Define las etiquetas de los controles.------------------------------
   FOR i := 1 TO Len( _aEtiqueta )

      @ _aEtiqueta[ i, ABM_LBL_ROW ], _aEtiqueta[ i, ABM_LBL_COL ] ;
         LABEL ( _aEtiqueta[ i, ABM_LBL_NAME ] ) ;
         OF wndABM3EditNuevoSplit ;
         VALUE _aNombreCampo[ i ] ;
         WIDTH _aEtiqueta[ i, ABM_LBL_WIDTH ] ;
         HEIGHT _aEtiqueta[ i, ABM_LBL_HEIGHT ] ;
         VCENTERALIGN ;
         FONT _GetSysFont() SIZE 9
   NEXT

   // ------- Define los controles de edición.------------------------------------
   FOR i := 1 TO Len( _aControl )
      DO CASE
      CASE _aControl[ i, ABM_CON_TYPE ] == ABM_TEXTBOXC .OR. _aControl[ i, ABM_CON_TYPE ] == ABM_COMBOBOX
         cFuncName := "LB_" + _cArea + "_" + _aEstructura[ i, DBS_NAME ]
         IF ValType( aCombos[ i ] ) = "C" // it is ComboBox
            _aControl[ i, ABM_CON_TYPE ] := ABM_COMBOBOX
            @ _aControl[ i, ABM_CON_ROW ], _aControl[ i, ABM_CON_COL ] ;
               ComboBox ( _aControl[ i, ABM_CON_NAME ] ) ;
               OF wndABM3EditNuevoSplit ;
               ITEMSOURCE &( aCombos[ i ] ) ;
               VALUESOURCE &( aCombos[ i ] ) ;
               HEIGHT _aControl[ i, ABM_CON_HEIGHT ] * 10 ;
               WIDTH _aControl[ i, ABM_CON_WIDTH ] ;
               FONT "arial" SIZE 9 ;
               ON DROPDOWN abKeyBlocks := _KeysOff() ;
               ON CLOSEUP _KeysOn( abKeyBlocks ) ;
               ON gotfocus ABM3ConFoco() ;
               ON lostfocus ABM3SinFoco() ;
               ON ENTER ABM3AlEntrar()
         ELSEIF ValType( aCombos[ i ] ) = "A" // it is ListBox
            aItems := aCombos[ i ]
            _aControl[ i, ABM_CON_TYPE ] := ABM_COMBOBOX
            @ _aControl[ i, ABM_CON_ROW ], _aControl[ i, ABM_CON_COL ] ;
               ComboBox ( _aControl[ i, ABM_CON_NAME ] ) ;
               OF wndABM3EditNuevoSplit ;
               ITEMS aItems ;
               VALUE 1 ;
               HEIGHT _aControl[ i, ABM_CON_HEIGHT ] * 10 ;
               WIDTH _aControl[ i, ABM_CON_WIDTH ] ;
               FONT "arial" SIZE 9 ;
               ON DROPDOWN abKeyBlocks := _KeysOff() ;
               ON CLOSEUP _KeysOn( abKeyBlocks ) ;
               ON gotfocus ABM3ConFoco() ;
               ON lostfocus ABM3SinFoco() ;
               ON ENTER ABM3AlEntrar()

         ELSEIF hb_IsFunction( cFuncName ) // it is Function
            aItems := &( cFuncName + "()" )
            _aControl[ i, ABM_CON_TYPE ] := ABM_COMBOBOX
            @ _aControl[ i, ABM_CON_ROW ], _aControl[ i, ABM_CON_COL ] ;
               ComboBox ( _aControl[ i, ABM_CON_NAME ] ) ;
               OF wndABM3EditNuevoSplit ;
               ITEMS aItems ;
               VALUE 1 ;
               HEIGHT _aControl[ i, ABM_CON_HEIGHT ] * 10 ;
               WIDTH _aControl[ i, ABM_CON_WIDTH ] ;
               FONT "arial" SIZE 9 ;
               ON DROPDOWN abKeyBlocks := _KeysOff() ;
               ON CLOSEUP _KeysOn( abKeyBlocks ) ;
               ON gotfocus ABM3ConFoco() ;
               ON lostfocus ABM3SinFoco() ;
               ON ENTER ABM3AlEntrar()
         ELSE // It is a Normal TextBox
            @ _aControl[ i, ABM_CON_ROW ], _aControl[ i, ABM_CON_COL ] ;
               textbox ( _aControl[ i, ABM_CON_NAME ] ) ;
               OF wndABM3EditNuevoSplit ;
               VALUE "" ;
               HEIGHT _aControl[ i, ABM_CON_HEIGHT ] ;
               WIDTH _aControl[ i, ABM_CON_WIDTH ] ;
               FONT "Arial" SIZE 9 ;
               MAXLENGTH _aEstructura[ i, DBS_LEN ] ;
               ON GOTFOCUS ABM3ConFoco() ;
               ON LOSTFOCUS ABM3SinFoco() ;
               ON ENTER ABM3AlEntrar()
         ENDIF
      CASE _aControl[ i, ABM_CON_TYPE ] == ABM_DATEPICKER
         @ _aControl[ i, ABM_CON_ROW ], _aControl[ i, ABM_CON_COL ] ;
            DATEPICKER ( _aControl[ i, ABM_CON_NAME ] ) ;
            OF wndABM3EditNuevoSplit ;
            HEIGHT _aControl[ i, ABM_CON_HEIGHT ] ;
            WIDTH _aControl[ i, ABM_CON_WIDTH ] + 25 ;
            FONT "Arial" SIZE 9 ;
            SHOWNONE ;
            ON GOTFOCUS ABM3ConFoco() ;
            ON LOSTFOCUS ABM3SinFoco()
      CASE _aControl[ i, ABM_CON_TYPE ] == ABM_TEXTBOXN
         IF _aEstructura[ i, DBS_DEC ] == 0
            @ _aControl[ i, ABM_CON_ROW ], _aControl[ i, ABM_CON_COL ] ;
               TEXTBOX ( _aControl[ i, ABM_CON_NAME ] ) ;
               OF wndABM3EditNuevoSplit ;
               VALUE "" ;
               HEIGHT _aControl[ i, ABM_CON_HEIGHT ] ;
               WIDTH _aControl[ i, ABM_CON_WIDTH ] ;
               NUMERIC ;
               FONT "Arial" SIZE 9 ;
               MAXLENGTH _aEstructura[ i, DBS_LEN ] ;
               ON GOTFOCUS ABM3ConFoco( i ) ;
               ON LOSTFOCUS ABM3SinFoco( i ) ;
               ON ENTER ABM3AlEntrar()
         ELSE
            cMascara := Replicate( "9", _aEstructura[ i, DBS_LEN ] - ( _aEstructura[ i, DBS_DEC ] + 1 ) )
            cMascara += "."
            cMascara += Replicate( "9", _aEstructura[ i, DBS_DEC ] )
            @ _aControl[ i, ABM_CON_ROW ], _aControl[ i, ABM_CON_COL ] ;
               TEXTBOX ( _aControl[ i, ABM_CON_NAME ] ) ;
               OF wndABM3EditNuevoSplit ;
               VALUE "" ;
               HEIGHT _aControl[ i, ABM_CON_HEIGHT ] ;
               WIDTH _aControl[ i, ABM_CON_WIDTH ] ;
               NUMERIC ;
               INPUTMASK cMascara ;
               ON GOTFOCUS ABM3ConFoco() ;
               ON LOSTFOCUS ABM3SinFoco() ;
               ON ENTER ABM3AlEntrar()
         ENDIF
      CASE _aControl[ i, ABM_CON_TYPE ] == ABM_CHECKBOX
         @ _aControl[ i, ABM_CON_ROW ], _aControl[ i, ABM_CON_COL ] ;
            CheckBox ( _aControl[ i, ABM_CON_NAME ] ) ;
            OF wndABM3EditNuevoSplit ;
            CAPTION "" ;
            HEIGHT _aControl[ i, ABM_CON_HEIGHT ] ;
            WIDTH _aControl[ i, ABM_CON_WIDTH ] ;
            VALUE .F. ;
            ON GOTFOCUS ABM3ConFoco() ;
            ON LOSTFOCUS ABM3SinFoco()
      CASE _aControl[ i, ABM_CON_TYPE ] == ABM_EDITBOX
         @ _aControl[ i, ABM_CON_ROW ], _aControl[ i, ABM_CON_COL ] ;
            EDITBOX ( _aControl[ i, ABM_CON_NAME ] ) ;
            OF wndABM3EditNuevoSplit ;
            WIDTH _aControl[ i, ABM_CON_WIDTH ] ;
            HEIGHT _aControl[ i, ABM_CON_HEIGHT ] ;
            VALUE "" ;
            FONT "Arial" SIZE 9 ;
            ON GOTFOCUS ABM3ConFoco() ;
            ON LOSTFOCUS ABM3SinFoco()
      ENDCASE
   NEXT

   // ------- Actualiza los controles si se está editando.------------------------
   if ! lNuevo
      FOR i := 1 TO Len( _aControl )
         IF ABM_COMBOBOX = _aControl[ i, ABM_CON_TYPE ] .AND. ValType( aCombos[ i ] ) = "C" // It is a ComboBox * (ValueSource)
            nRegistro := ( cTable )->( RecNo() ) // ItemSource => Table related
            ( cTable )->( dbGoTop() )
            DO WHILE ( cTable )->( ! Eof() )
               cThisOne := Upper( AllTrim( ( _cArea )->( FieldGet(i ) ) ) ) // this table
               cReferTable := Upper( AllTrim( ( cTable )->&cField ) ) // related table
               IF cReferTable = cThisOne
                  SetProperty( "wndABM3EditNuevoSplit", _aControl[ i, ABM_CON_NAME ], "Value", ( cTable )->( RecNo() ) )
                  EXIT // This is the saved value
               ENDIF
               ( cTable )->( dbSkip() )
            END
            ( cTable )->( dbGoto( nRegistro ) )
         ELSEIF ABM_COMBOBOX = _aControl[ i, ABM_CON_TYPE ] .AND. ValType( aCombos[ i ] ) = "A" // It is a ListBox * (Item => aParamet)
            aItems := aCombos[ i ] ;  cValor := RTrim( ( _cArea )->( FieldGet( i ) ) )
            nItem := AScan( aItems, cValor ) // read the options list (for the user to select)
            SetProperty( "wndABM3EditNuevoSplit", _aControl[ i, ABM_CON_NAME ], "Value", nItem )

         ELSEIF ABM_COMBOBOX = _aControl[ i, ABM_CON_TYPE ] // It is a ListBox * (Item => function list
            cFuncName := "LB_" + _cArea + "_" + _aEstructura[ i, DBS_NAME ] + "()"
            aItems := &( cFuncName ) ;  cValor := RTrim( ( _cArea )->( FieldGet( i ) ) )
            nItem := AScan( aItems, cValor ) // read the options list (for the user to select)
            SetProperty( "wndABM3EditNuevoSplit", _aControl[ i, ABM_CON_NAME ], "Value", nItem )
         ELSE // It is a normal TextBox
            SetProperty( "wndABM3EditNuevoSplit", _aControl[ i, ABM_CON_NAME ], "Value", ( _cArea )->( FieldGet( i ) ) )
         ENDIF
      NEXT
   ENDIF

   // ------- Establece el estado inicial de los controles.-----------------------
   FOR i := 1 TO Len( _aControl )
      SetProperty( "wndABM3EditNuevoSplit", _aControl[ i, ABM_CON_NAME ], "Enabled", _aEditable[ i ] )
   NEXT

   // ------- Establece el estado del botón de copia.-----------------------------
   if ! lNuevo
      wndABM3EditNuevo.tbbCopiar.Enabled := .F.
   ENDIF

   // ------- Activa la ventana de edición de registro.---------------------------
   ON KEY ESCAPE ;
      OF wndABM3EditNuevoSplit ;
      ACTION wndABM3EditNuevo.tbbCancelar.ONCLICK

   ON KEY RETURN ;
      OF wndABM3EditNuevoSplit ;
      ACTION wndABM3EditNuevo.tbbAceptar.ONCLICK

   CENTER WINDOW wndABM3EditNuevo // added 20160209
   ACTIVATE WINDOW wndABM3EditNuevo

RETURN NIL


/****************************************************************************************
 *  Aplicación: Comando EDIT para MiniGUI
 *       Autor: Cristóbal Mollá [cemese@terra.es]
 *     Función: ABM3ConFoco()
 * Descripción: Actualiza las etiquetas de los controles y presenta los mensajes en la
 *              barra de estado de la ventana de edición de registro al obtener un
 *              control de edición el foco.
 *  Parámetros: Ninguno
 *    Devuelve: NIL
****************************************************************************************/
STATIC FUNCTION ABM3ConFoco()

   // ------- Declaración de variables locales.-----------------------------------
   LOCAL i /*as numeric*/ // Indice de iteración.
   LOCAL cControl AS character // Nombre del control activo.
   LOCAL acControl /*as array*/ // Matriz con los nombre de los controles.

   // ------- Inicialización de variables.----------------------------------------
   cControl := This.NAME
   acControl := {}
   FOR i := 1 TO Len( _aControl )
      AAdd( acControl, _aControl[ i, ABM_CON_NAME ] )
   NEXT
   _nControlActivo := AScan( acControl, cControl )

   // ------- Pone la etiqueta en negrita.----------------------------------------
   SetProperty( "wndABM3EditNuevoSplit", _aEtiqueta[ _nControlActivo, ABM_LBL_NAME ], "FontBold", .T. )

   // ------- Presenta el mensaje en la barra de estado.--------------------------
   wndABM3EditNuevo.StatusBar.Item( 1 ) := _aControl[ _nControlActivo, ABM_CON_DES ]

RETURN NIL


/****************************************************************************************
 *  Aplicación: Comando EDIT para MiniGUI
 *       Autor: Cristóbal Mollá [cemese@terra.es]
 *     Función: ABM3SinFoco()
 * Descripción: Restaura el estado de las etiquetas y de la barra de estado de la ventana
 *              de edición de registros al dejar un control de edición sin foco.
 *  Parámetros: Ninguno
 *    Devuelve: NIL
****************************************************************************************/
STATIC FUNCTION ABM3SinFoco()

   // ------- Restaura el estado de la etiqueta.----------------------------------
   SetProperty( "wndABM3EditNuevoSplit", _aEtiqueta[ _nControlActivo, ABM_LBL_NAME ], "FontBold", .F. )

   // ------- Restaura el texto de la barra de estado.----------------------------
   wndABM3EditNuevo.StatusBar.Item( 1 ) := ""

RETURN NIL


/****************************************************************************************
 *  Aplicación: Comando EDIT para MiniGUI
 *       Autor: Cristóbal Mollá [cemese@terra.es]
 *     Función: ABM3AlEntrar()
 * Descripción: Cambia al siguiente control de edición tipo TEXTBOX al pulsar la tecla
 *              ENTER.
 *  Parámetros: Ninguno
 *    Devuelve: NIL
****************************************************************************************/
STATIC FUNCTION ABM3AlEntrar()

   // ------- Declaración de variables locales.-----------------------------------
   LOCAL lSalida /*as logical*/ // Tipo de salida.
   LOCAL nTipo /*as numeric*/ // Tipo del control.

   // ------- Inicializa las variables.-------------------------------------------
   lSalida := .T.

   // ------- Restaura el estado de la etiqueta.----------------------------------
   SetProperty( "wndABM3EditNuevoSplit", _aEtiqueta[ _nControlActivo, ABM_LBL_NAME ], "FontBold", .F. )

   // ------- Activa el siguiente control editable con evento ON ENTER.-----------
   DO WHILE lSalida
      _nControlActivo++
      IF _nControlActivo > Len( _aControl )
         _nControlActivo := 1
      ENDIF
      nTipo := _aControl[ _nControlActivo, ABM_CON_TYPE ]
      IF nTipo == ABM_TEXTBOXC .OR. nTipo == ABM_TEXTBOXN
         IF _aEditable[ _nControlActivo ]
            lSalida := .F.
         ENDIF
      ENDIF
   ENDDO

   DoMethod( "wndABM3EditNuevoSplit", _aControl[ _nControlActivo, ABM_CON_NAME ], "SetFocus" )

RETURN NIL


/****************************************************************************************
 *  Aplicación: Comando EDIT para MiniGUI
 *       Autor: Cristóbal Mollá [cemese@terra.es]
 *     Función: ABM3EditarGuardar( lNuevo )
 * Descripción: Añade o guarda el registro en la bdd.
 *  Parámetros: lNuevo          Valor lógico que indica si se está añadiendo un registro
 *                              o editando el actual.
 *    Devuelve: NIL
****************************************************************************************/
STATIC FUNCTION ABM3EditarGuardar( lNuevo )

   // ------- Declaración de variables locales.-----------------------------------
   LOCAL i /*as numeric*/ // Indice de iteración.
   LOCAL xValor // Valor a guardar.
   LOCAL lResultado /*as logical*/ // Resultado del bloque del usuario.
   LOCAL aValores /*as array*/ // Valores del registro.
   LOCAL cFuncName
   LOCAL aItems

   // ------- Guarda el registro.-------------------------------------------------
   IF _bGuardar == NIL

      // No hay bloque de código del usuario.
      IF lNuevo
         ( _cArea )->( dbAppend() )
      ENDIF

      IF ( _cArea )->( RLock() )

         FOR i := 1 TO Len( _aEstructura )
            xValor := GetProperty( "wndABM3EditNuevoSplit", _aControl[ i, ABM_CON_NAME ], "Value" )
            IF ABM_COMBOBOX == _aControl[ i, ABM_CON_TYPE ]
               IF ValType( aCombos[ i ] ) = "C" // it is a ComboBox
                  // xValor := wndABM3EditNuevoSplit.&(_HMG_cMacroTemp).Value
               ELSEIF ValType( aCombos[ i ] ) = "A" // it is a ListBox Param
                  aItems := aCombos[ i ] // select the value to be written
                  xValor := iif( 0 = xValor, "", aItems[ xValor ] ) // (after it was selected)
               ELSE // it is a ListBox (function)
                  cFuncName := "LB_" + _cArea + "_" + _aEstructura[ i, DBS_NAME ] + "()"
                  aItems := &( cFuncName ) // select the value to be written
                  xValor := iif( 0 = xValor, "", aItems[ xValor ] ) // (after it was selected)
               ENDIF
            ENDIF
            ( _cArea )->( FieldPut( i, xValor ) )
         NEXT

         ( _cArea )->( dbUnlock() )

         // Refresca la ventana de visualización.
         wndABM3EditNuevo.RELEASE
         ABM3Redibuja( .T. )

      ELSE
         AlertStop ( _HMG_aLangUser[ 41 ], _cTitulo )
      ENDIF
   ELSE

      // Hay bloque de código del usuario.
      aValores := {}
      FOR i := 1 TO Len( _aControl )
         xValor := GetProperty( "wndABM3EditNuevoSplit", _aControl[ i, ABM_CON_NAME ], "Value" )
         AAdd( aValores, xValor )
      NEXT
      lResultado := Eval( _bGuardar, aValores, lNuevo )
      IF ValType( lResultado ) != "L"
         lResultado := .T.
      ENDIF

      // Refresca la ventana de visualización.
      IF lResultado
         wndABM3EditNuevo.RELEASE
         ABM3Redibuja( .T. )
      ENDIF
   ENDIF

RETURN NIL


/****************************************************************************************
 *  Aplicación: Comando EDIT para MiniGUI
 *       Autor: Cristóbal Mollá [cemese@terra.es]
 *     Función: ABM3Seleccionar()
 * Descripción: Presenta una ventana para la selección de un registro.
 *  Parámetros: Ninguno
 *    Devuelve: [nReg]          Numero de registro seleccionado, o cero si no se ha
 *                              seleccionado ninguno.
****************************************************************************************/
STATIC FUNCTION ABM3Seleccionar()

   // ------- Declaración de variables locales.-----------------------------------
   LOCAL lSalida AS logical // Control de bucle.
   LOCAL nReg AS NUMERIC // Valores del registro
   LOCAL nRegistro /*as numeric*/ // Número de registro.

   // ------- Inicialización de variables.----------------------------------------
   lSalida := .F.
   nReg := 0
   nRegistro := ( _cArea )->( RecNo() )

   // ------- Se situa en el primer registro.-------------------------------------
   ( _cArea )->( dbGoTop() )

   // ------- Creación de la ventana de selección de registro.--------------------
   DEFINE WINDOW wndSeleccionar ;
         AT 0, 0 ;
         WIDTH 500 ;
         HEIGHT 300 ;
         TITLE _HMG_aLangLabel[ 8 ] ;
         MODAL ;
         NOSIZE ;
         NOSYSMENU ;
         FONT _GetSysFont() SIZE 9

      // Define la barra de botones de la ventana de selección.
      DEFINE TOOLBAR tbSeleccionar buttonsize 100, 32 FLAT righttext BORDER
         BUTTON tbbCancelarSel CAPTION _HMG_aLangButton[ 7 ] ;
            PICTURE "MINIGUI_EDIT_CANCEL" ;
            ACTION {|| lSalida := .F., ;
            nReg := 0, ;
            wndSeleccionar.Release }
         BUTTON tbbAceptarSel CAPTION _HMG_aLangButton[ 8 ] ;
            PICTURE "MINIGUI_EDIT_OK" ;
            ACTION {|| lSalida := .T., ;
            nReg := wndSeleccionar.brwSeleccionar.VALUE, ;
            wndSeleccionar.Release }
      END TOOLBAR

      // Define la barra de estado de la ventana de selección.
      DEFINE STATUSBAR FONT _GetSysFont() SIZE 9
         STATUSITEM _HMG_aLangUser[ 7 ]
      END STATUSBAR

      // Define la tabla de la ventana de selección.
      @ 55, 20 BROWSE brwSeleccionar ;
         WIDTH 460 ;
         HEIGHT 190 ;
         HEADERS _aCabeceraTabla ;
         WIDTHS _aAnchoTabla ;
         WORKAREA &_cArea ;
         FIELDS _aCampoTabla ;
         VALUE ( _cArea )->( RecNo() ) ;
         FONT "Arial" SIZE 9 ;
         ON DBLCLICK {|| lSalida := .T., ;
         nReg := wndSeleccionar.brwSeleccionar.VALUE, ;
         wndSeleccionar.Release } ;
         JUSTIFY _aAlineadoTabla paintdoublebuffer

   END WINDOW

   // ------- Activa la ventana de selección de registro.-------------------------
   CENTER WINDOW wndSeleccionar
   ACTIVATE WINDOW wndSeleccionar

   // ------- Restuara el puntero de registro.------------------------------------
   ( _cArea )->( dbGoto( nRegistro ) )

RETURN ( nReg )


/****************************************************************************************
 *  Aplicación: Comando EDIT para MiniGUI
 *       Autor: Cristóbal Mollá [cemese@terra.es]
 *     Función: ABM3EditarCopiar()
 * Descripción: Copia el registro seleccionado en los controles de edición del nuevo
 *              registro.
 *  Parámetros: Ninguno
 *    Devuelve: NIL
****************************************************************************************/
STATIC FUNCTION ABM3EditarCopiar()

   // ------- Declaración de variables locales.-----------------------------------
   LOCAL i /*as numeric*/ // Indice de iteración.
   LOCAL nRegistro /*as numeric*/ // Puntero de registro.
   LOCAL nReg /*as numeric*/ // Numero de registro.

   // ------- Obtiene el registro a copiar.---------------------------------------
   nReg := ABM3Seleccionar()

   // ------- Actualiza los controles de edición.---------------------------------
   IF nReg != 0
      nRegistro := ( _cArea )->( RecNo() )
      ( _cArea )->( dbGoto( nReg ) )
      FOR i := 1 TO Len( _aControl )
         IF _aEditable[ i ]
            SetProperty( "wndABM3EditNuevoSplit", _aControl[ i, ABM_CON_NAME ], "Value", ( _cArea )->( FieldGet( i ) ) )
         ENDIF
      NEXT
      ( _cArea )->( dbGoto( nRegistro ) )
   ENDIF

RETURN NIL


/****************************************************************************************
 *  Aplicación: Comando EDIT para MiniGUI
 *       Autor: Cristóbal Mollá [cemese@terra.es]
 *     Función: ABM3Borrar()
 * Descripción: Borra el registro activo.
 *  Parámetros: Ninguno
 *    Devuelve: NIL
****************************************************************************************/
STATIC FUNCTION ABM3Borrar()

   // ------- Borra el registro si se acepta.-------------------------------------
   IF AlertOKCancel( _HMG_aLangUser[ 8 ], _HMG_aLangLabel[ 16 ] )
      IF ( _cArea )->( RLock() )
         ( _cArea )->( dbDelete() )
         ( _cArea )->( dbCommit() )
         ( _cArea )->( dbUnlock() )
         IF Set( _SET_DELETED )
            ( _cArea )->( dbSkip() )
            IF ( _cArea )->( Eof() )
               ( _cArea )->( dbGoBottom() )
            ENDIF
         ENDIF
         ABM3Redibuja( .T. )
      ELSE
         AlertStop( _HMG_aLangUser[ 41 ], _cTitulo )
      ENDIF
   ENDIF

RETURN NIL


/****************************************************************************************
 *  Aplicación: Comando EDIT para MiniGUI
 *       Autor: Cristóbal Mollá [cemese@terra.es]
 *     Función: ABM3Recover()
 * Descripción: Restaurar el registro activo.
 *  Parámetros: Ninguno
 *    Devuelve: NIL
****************************************************************************************/
STATIC FUNCTION ABM3Recover()

   // ------- Restaurar el registro si se acepta.-----------------------------------
   IF AlertOKCancel( _HMG_aLangUser[ 42 ], StrTran( _HMG_aLangButton[ 12 ], "&", "" ) )
      IF ( _cArea )->( RLock() )
         ( _cArea )->( dbRecall() )
         ( _cArea )->( dbCommit() )
         ( _cArea )->( dbUnlock() )
         ABM3Redibuja( .T. )
      ELSE
         AlertStop( _HMG_aLangUser[ 41 ], _cTitulo )
      ENDIF
   ENDIF

RETURN NIL


/****************************************************************************************
 *  Aplicación: Comando EDIT para MiniGUI
 *       Autor: Cristóbal Mollá [cemese@terra.es]
 *     Función: ABM3Buscar()
 * Descripción: Busca un registro por la clave del indice activo.
 *  Parámetros: Ninguno
 *    Devuelve: NIL
****************************************************************************************/
STATIC FUNCTION ABM3Buscar()

   // ------- Declaración de variables locales.-----------------------------------
   LOCAL nControl /*as numeric*/ // Numero del control.
   LOCAL lSalida AS logical // Tipo de salida de la ventana.
   LOCAL xValor // Valor de busqueda.
   LOCAL cMascara AS character // Mascara de edición del control.
   LOCAL lResultado /*as logical*/ // Resultado de la busqueda.
   LOCAL nRegistro /*as numeric*/ // Numero de registro.

   // ------- Inicialización de variables.----------------------------------------
   nControl := _aIndiceCampo[ _nIndiceActivo ]

   // ------- Comprueba si se ha pasado una acción del usuario.--------------------
   IF _bBuscar != NIL
      Eval( _bBuscar )
      ABM3Redibuja( .T. )
      RETURN NIL
   ENDIF

   // ------- Comprueba si hay un indice activo.----------------------------------
   IF _nIndiceActivo == 1
      AlertExclamation( _HMG_aLangUser[ 9 ], _cTitulo )
      RETURN NIL
   ENDIF

   // ------- Comprueba que el campo indice no es del tipo memo o logico.---------
   IF _aEstructura[ nControl, DBS_TYPE ] == "L" .OR. _aEstructura[ nControl, DBS_TYPE ] == "M"
      AlertExclamation( _HMG_aLangUser[ 10 ], _cTitulo )
      RETURN NIL
   ENDIF

   // ------- Crea la ventana de busqueda.----------------------------------------
   DEFINE WINDOW wndABMBuscar ;
         AT 0, 0 ;
         WIDTH 500 ;
         HEIGHT 170 ;
         TITLE _HMG_aLangLabel[ 9 ] ;
         MODAL ;
         NOSIZE ;
         NOSYSMENU ;
         FONT _GetSysFont() SIZE 9

      // Define la barra de botones de la ventana de busqueda.
      DEFINE TOOLBAR tbBuscar buttonsize 100, 32 FLAT righttext BORDER
         BUTTON tbbCancelarBus CAPTION _HMG_aLangButton[ 7 ] ;
            PICTURE "MINIGUI_EDIT_CANCEL" ;
            ACTION {|| lSalida := .F., ;
            xValor := wndABMBuscar .conBuscar.VALUE, ;
            wndABMBuscar.Release }
         BUTTON tbbAceptarBus CAPTION _HMG_aLangButton[ 8 ] ;
            PICTURE "MINIGUI_EDIT_OK" ;
            ACTION {|| lSalida := .T., ;
            xValor := wndABMBuscar .conBuscar.VALUE, ;
            wndABMBuscar.Release }
      END TOOLBAR

      // Define la barra de estado de la ventana de busqueda.
      DEFINE STATUSBAR FONT _GetSysFont() SIZE 9
         STATUSITEM ""
      END STATUSBAR
   END WINDOW

   // ------- Crea los controles de la ventana de busqueda.-----------------------
   // Frame.
   @ 45, 10 FRAME frmBuscar ;
      OF wndABMBuscar ;
      CAPTION "" ;
      WIDTH wndABMBuscar.WIDTH - 25 ;
      HEIGHT wndABMBuscar.HEIGHT - 100

   // Etiqueta.
   @ 60, 20 LABEL lblBuscar ;
      OF wndABMBuscar ;
      VALUE _aNombreCampo[ nControl ] ;
      WIDTH _aEtiqueta[ nControl, ABM_LBL_WIDTH ] ;
      HEIGHT _aEtiqueta[ nControl, ABM_LBL_HEIGHT ] ;
      FONT _GetSysFont() SIZE 9

   // Tipo de dato a buscar.
   DO CASE

      // Carácter.
   CASE _aControl[ nControl, ABM_CON_TYPE ] == ABM_TEXTBOXC
      @ 75, 20 TEXTBOX conBuscar ;
         OF wndABMBuscar ;
         VALUE "" ;
         HEIGHT _aControl[ nControl, ABM_CON_HEIGHT ] ;
         WIDTH _aControl[ nControl, ABM_CON_WIDTH ] ;
         FONT "Arial" SIZE 9 ;
         MAXLENGTH _aEstructura[ nControl, DBS_LEN ]

      // Fecha.
   CASE _aControl[ nControl, ABM_CON_TYPE ] == ABM_DATEPICKER
      @ 75, 20 DATEPICKER conBuscar ;
         OF wndABMBuscar ;
         VALUE Date() ;
         HEIGHT _aControl[ nControl, ABM_CON_HEIGHT ] ;
         WIDTH _aControl[ nControl, ABM_CON_WIDTH ] + 25 ;
         FONT "Arial" SIZE 9

      // Numerico.
   CASE _aControl[ nControl, ABM_CON_TYPE ] == ABM_TEXTBOXN
      IF _aEstructura[ nControl, DBS_DEC ] == 0

         // Sin decimales.
         @ 75, 20 TEXTBOX conBuscar ;
            OF wndABMBuscar ;
            VALUE "" ;
            HEIGHT _aControl[ nControl, ABM_CON_HEIGHT ] ;
            WIDTH _aControl[ nControl, ABM_CON_WIDTH ] ;
            NUMERIC ;
            FONT "Arial" SIZE 9 ;
            MAXLENGTH _aEstructura[ nControl, DBS_LEN ]
      ELSE

         // Con decimales.
         cMascara := Replicate( "9", _aEstructura[ nControl, DBS_LEN ] - ( _aEstructura[ nControl, DBS_DEC ] + 1 ) )
         cMascara += "."
         cMascara += Replicate( "9", _aEstructura[ nControl, DBS_DEC ] )
         @ 75, 20 TEXTBOX conBuscar ;
            OF wndABMBuscar ;
            VALUE "" ;
            HEIGHT _aControl[ nControl, ABM_CON_HEIGHT ] ;
            WIDTH _aControl[ nControl, ABM_CON_WIDTH ] ;
            NUMERIC ;
            INPUTMASK cMascara
      ENDIF
   ENDCASE

   // ------- Actualiza la barra de estado.---------------------------------------
   wndABMBuscar.StatusBar.Item( 1 ) := _aControl[ nControl, ABM_CON_DES ]

   // ------- Comprueba el tamaño del control de edición del dato a buscar.-------
   IF wndABMBuscar .conBuscar. Width > wndABM3Edit.WIDTH - 45
      wndABMBuscar .conBuscar. WIDTH := wndABM3Edit.WIDTH - 45
   ENDIF

   // ------- Activa la ventana de busqueda.--------------------------------------
   ON KEY ESCAPE ;
      OF wndABMBuscar ;
      ACTION wndABMBuscar.tbbCancelarBus.ONCLICK

   ON KEY RETURN ;
      OF wndABMBuscar ;
      ACTION wndABMBuscar.tbbAceptarBus.ONCLICK

   CENTER WINDOW wndABMBuscar
   ACTIVATE WINDOW wndABMBuscar

   // ------- Busca el registro.--------------------------------------------------
   IF lSalida
      nRegistro := ( _cArea )->( RecNo() )
      lResultado := ( _cArea )->( dbSeek( xValor ) )
      if ! lResultado
         AlertExclamation( _HMG_aLangUser[ 11 ], _cTitulo )
         ( _cArea )->( dbGoto( nRegistro ) )
      ELSE
         ABM3Redibuja( .T. )
      ENDIF
   ENDIF

RETURN NIL


/****************************************************************************************
 *  Aplicación: Comando EDIT para MiniGUI
 *       Autor: Cristóbal Mollá [cemese@terra.es]
 *     Función: ABM3ActivarFiltro()
 * Descripción: Filtra la base de datos.
 *  Parámetros: Ninguno
 *    Devuelve: NIL
****************************************************************************************/
STATIC FUNCTION ABM3ActivarFiltro()

   // ------- Declaración de variables locales.-----------------------------------
   LOCAL aCompara /*as array*/ // Comparaciones.
   LOCAL aCampos /*as array*/ // Nombre de los campos.

   // ------- Comprueba que no hay ningun filtro activo.--------------------------
   IF _cFiltro != ""
      AlertInfo( _HMG_aLangUser[ 34 ], '' )
   ENDIF

   // ------- Inicialización de variables.----------------------------------------
   aCampos := _aNombreCampo
   aCompara := { _HMG_aLangLabel[ 27 ], ;
      _HMG_aLangLabel[ 28 ], ;
      _HMG_aLangLabel[ 29 ], ;
      _HMG_aLangLabel[ 30 ], ;
      _HMG_aLangLabel[ 31 ], ;
      _HMG_aLangLabel[ 32 ] }


   // ------- Crea la ventana de filtrado.----------------------------------------
   DEFINE WINDOW wndABM3Filtro ;
         AT 0, 0 ;
         WIDTH 400 ;
         HEIGHT 325 ;
         TITLE _HMG_aLangLabel[ 21 ] ;
         MODAL ;
         NOSIZE ;
         NOSYSMENU ;
         ON INIT {|| ABM3ControlFiltro() } ;
         FONT _GetSysFont() SIZE 9

      // Define la barra de botones de la ventana de filtrado.
      DEFINE TOOLBAR tbBuscar buttonsize 100, 32 FLAT righttext BORDER
         BUTTON tbbCancelarFil CAPTION _HMG_aLangButton[ 7 ] ;
            PICTURE "MINIGUI_EDIT_CANCEL" ;
            ACTION {|| wndABM3Filtro.RELEASE, ;
            ABM3Redibuja( .F. ) }
         BUTTON tbbAceptarFil CAPTION _HMG_aLangButton[ 8 ] ;
            PICTURE "MINIGUI_EDIT_OK" ;
            ACTION {|| ABM3EstableceFiltro() }
      END TOOLBAR

      // Define la barra de estado de la ventana de filtrado.
      DEFINE STATUSBAR FONT _GetSysFont() SIZE 9
         STATUSITEM ""
      END STATUSBAR
   END WINDOW

   // ------- Controles de la ventana de filtrado.
   // Frame.
   @ 45, 10 FRAME frmFiltro ;
      OF wndABM3Filtro ;
      CAPTION "" ;
      WIDTH wndABM3Filtro.WIDTH - 25 ;
      HEIGHT wndABM3Filtro.HEIGHT - 100
   @ 65, 20 LABEL lblCampos ;
      OF wndABM3Filtro ;
      VALUE _HMG_aLangLabel[ 22 ] ;
      WIDTH 140 ;
      HEIGHT 25 ;
      FONT _GetSysFont() SIZE 9
   @ 65, 220 LABEL lblCompara ;
      OF wndABM3Filtro ;
      VALUE _HMG_aLangLabel[ 23 ] ;
      WIDTH 140 ;
      HEIGHT 25 ;
      FONT _GetSysFont() SIZE 9
   @ 200, 20 LABEL lblValor ;
      OF wndABM3Filtro ;
      VALUE _HMG_aLangLabel[ 24 ] ;
      WIDTH 140 ;
      HEIGHT 25 ;
      FONT _GetSysFont() SIZE 9
   @ 85, 20 LISTBOX lbxCampos ;
      OF wndABM3Filtro ;
      WIDTH 140 ;
      HEIGHT 100 ;
      ITEMS aCampos ;
      VALUE 1 ;
      FONT "Arial" SIZE 9 ;
      ON CHANGE {|| ABM3ControlFiltro() } ;
      ON GOTFOCUS wndABM3Filtro.StatusBar.Item( 1 ) := _HMG_aLangLabel[ 25 ] ;
      ON LOSTFOCUS wndABM3Filtro.StatusBar.Item( 1 ) := ""
   @ 85, 220 LISTBOX lbxCompara ;
      OF wndABM3Filtro ;
      WIDTH 140 ;
      HEIGHT 100 ;
      ITEMS aCompara ;
      VALUE 1 ;
      FONT "Arial" SIZE 9 ;
      ON GOTFOCUS wndABM3Filtro.StatusBar.Item( 1 ) := _HMG_aLangLabel[ 26 ] ;
      ON LOSTFOCUS wndABM3Filtro.StatusBar.Item( 1 ) := ""
   @ 220, 20 TEXTBOX conValor ;
      OF wndABM3Filtro ;
      VALUE "" ;
      HEIGHT 25 ;
      WIDTH 160 ;
      FONT "Arial" SIZE 9 ;
      MAXLENGTH 16

   // ------- Activa la ventana.
   CENTER WINDOW wndABM3Filtro
   ACTIVATE WINDOW wndABM3Filtro

RETURN NIL


/****************************************************************************************
 *  Aplicación: Comando EDIT para MiniGUI
 *       Autor: Cristóbal Mollá [cemese@terra.es]
 *     Función: ABM3ControlFiltro()
 * Descripción: Comprueba que el filtro se puede aplicar.
 *  Parámetros: Ninguno
 *    Devuelve: NIL
****************************************************************************************/
STATIC FUNCTION ABM3ControlFiltro()

   // ------- Declaración de variables locales.-----------------------------------
   LOCAL nControl /*as numeric*/
   LOCAL cMascara AS character
   LOCAL cMensaje AS character

   // ------- Inicializa las variables.
   nControl := wndABM3Filtro.lbxCampos.VALUE

   // ------- Comprueba que se puede crear el control.----------------------------
   IF _aEstructura[ nControl, DBS_TYPE ] == "M"
      AlertExclamation( _HMG_aLangUser[ 35 ], _cTitulo )
      RETURN NIL
   ENDIF
   IF nControl == 0
      AlertExclamation( _HMG_aLangUser[ 36 ], _cTitulo )
      RETURN NIL
   ENDIF

   // ------- Crea el nuevo control.----------------------------------------------
   wndABM3Filtro.conValor.RELEASE
   cMensaje := _aControl[ nControl, ABM_CON_DES ]
   DO CASE

      // Carácter.
   CASE _aControl[ nControl, ABM_CON_TYPE ] == ABM_TEXTBOXC
      @ 226, 20 TEXTBOX conValor ;
         OF wndABM3Filtro ;
         VALUE "" ;
         HEIGHT _aControl[ nControl, ABM_CON_HEIGHT ] ;
         WIDTH _aControl[ nControl, ABM_CON_WIDTH ] ;
         FONT "Arial" SIZE 9 ;
         MAXLENGTH _aEstructura[ nControl, DBS_LEN ] ;
         ON GOTFOCUS wndABM3Filtro.StatusBar.Item( 1 ) := ;
         cMensaje ;
         ON LOSTFOCUS wndABM3Filtro.StatusBar.Item( 1 ) := ""

      // Fecha.
   CASE _aControl[ nControl, ABM_CON_TYPE ] == ABM_DATEPICKER
      @ 226, 20 DATEPICKER conValor ;
         OF wndABM3Filtro ;
         VALUE Date() ;
         HEIGHT _aControl[ nControl, ABM_CON_HEIGHT ] ;
         WIDTH _aControl[ nControl, ABM_CON_WIDTH ] + 25 ;
         FONT "Arial" SIZE 9 ;
         ON GOTFOCUS wndABM3Filtro.StatusBar.Item( 1 ) := ;
         cMensaje ;
         ON LOSTFOCUS wndABM3Filtro.StatusBar.Item( 1 ) := ""

      // Numerico.
   CASE _aControl[ nControl, ABM_CON_TYPE ] == ABM_TEXTBOXN
      IF _aEstructura[ nControl, DBS_DEC ] == 0

         // Sin decimales.
         @ 226, 20 TEXTBOX conValor ;
            OF wndABM3Filtro ;
            VALUE "" ;
            HEIGHT _aControl[ nControl, ABM_CON_HEIGHT ] ;
            WIDTH _aControl[ nControl, ABM_CON_WIDTH ] ;
            NUMERIC ;
            FONT "Arial" SIZE 9 ;
            MAXLENGTH _aEstructura[ nControl, DBS_LEN ] ;
            ON GOTFOCUS wndABM3Filtro.StatusBar.Item( 1 ) := ;
            cMensaje ;
            ON LOSTFOCUS wndABM3Filtro.StatusBar.Item( 1 ) := ""

      ELSE

         // Con decimales.
         cMascara := Replicate( "9", _aEstructura[ nControl, DBS_LEN ] - ( _aEstructura[ nControl, DBS_DEC ] + 1 ) )
         cMascara += "."
         cMascara += Replicate( "9", _aEstructura[ nControl, DBS_DEC ] )
         @ 226, 20 TEXTBOX conValor ;
            OF wndABM3Filtro ;
            VALUE "" ;
            HEIGHT _aControl[ nControl, ABM_CON_HEIGHT ] ;
            WIDTH _aControl[ nControl, ABM_CON_WIDTH ] ;
            NUMERIC ;
            INPUTMASK cMascara ;
            ON GOTFOCUS wndABM3Filtro.StatusBar.Item( 1 ) := ;
            cMensaje ;
            ON LOSTFOCUS wndABM3Filtro.StatusBar.Item( 1 ) := ""
      ENDIF

      // Logico
   CASE _aControl[ nControl, ABM_CON_TYPE ] == ABM_CHECKBOX
      @ 226, 20 CHECKBOX conValor ;
         OF wndABM3Filtro ;
         CAPTION "" ;
         HEIGHT _aControl[ nControl, ABM_CON_HEIGHT ] ;
         WIDTH _aControl[ nControl, ABM_CON_WIDTH ] ;
         VALUE .F. ;
         ON GOTFOCUS wndABM3Filtro.StatusBar.Item( 1 ) := ;
         cMensaje ;
         ON LOSTFOCUS wndABM3Filtro.StatusBar.Item( 1 ) := ""

   ENDCASE

   // ------- Actualiza el valor de la etiqueta.----------------------------------
   wndABM3Filtro .lblValor.VALUE := _aNombreCampo[ nControl ]

   // ------- Comprueba el tamaño del control de edición del dato a buscar.-------
   IF wndABM3Filtro.conValor.Width > wndABM3Filtro.WIDTH - 45
      wndABM3Filtro.conValor.WIDTH := wndABM3Filtro.WIDTH - 45
   ENDIF

RETURN NIL


/****************************************************************************************
 *  Aplicación: Comando EDIT para MiniGUI
 *       Autor: Cristóbal Mollá [cemese@terra.es]
 *     Función: ABM3EstableceFiltro()
 * Descripción: Establece el filtro seleccionado.
 *  Parámetros: Ninguno.
 *    Devuelve: NIL
****************************************************************************************/
STATIC FUNCTION ABM3EstableceFiltro()

   // ------- Declaración de variables locales.-----------------------------------
   LOCAL aOperador /*as array*/
   LOCAL nCampo /*as numeric*/
   LOCAL nCompara /*as numeric*/
   LOCAL cValor AS character
   LOCAL bd

   // ------- Inicialización de variables.----------------------------------------
   nCompara := wndABM3Filtro.lbxCompara.VALUE
   nCampo := wndABM3Filtro.lbxCampos.VALUE
   cValor := hb_ValToStr( wndABM3Filtro.conValor.Value )
   aOperador := { "=", "<>", ">", "<", ">=", "<=" }

   // ------- Comprueba que se puede filtrar.-------------------------------------
   IF nCompara == 0
      AlertExclamation( _HMG_aLangUser[ 37 ], _cTitulo )
      RETURN NIL
   ENDIF
   IF nCampo == 0
      AlertExclamation( _HMG_aLangUser[ 36 ], _cTitulo )
      RETURN NIL
   ENDIF
   IF cValor == ""
      AlertExclamation( _HMG_aLangUser[ 38 ], _cTitulo )
      RETURN NIL
   ENDIF
   IF _aEstructura[ nCampo, DBS_TYPE ] == "M"
      AlertExclamation( _HMG_aLangUser[ 35 ], _cTitulo )
      RETURN NIL
   ENDIF

   // ------- Establece el filtro.------------------------------------------------
   DO CASE
   CASE _aEstructura[ nCampo, DBS_TYPE ] == "C"
      _cFiltro := "Upper(" + _cArea + "->" + ;
         _aEstructura[ nCampo, DBS_NAME ] + ")" + ;
         aOperador[ nCompara ]
      _cFiltro += "'" + Upper( AllTrim( cValor ) ) + "'"

   CASE _aEstructura[ nCampo, DBS_TYPE ] == "N"
      _cFiltro := _cArea + "->" + ;
         _aEstructura[ nCampo, DBS_NAME ] + ;
         aOperador[ nCompara ]
      _cFiltro += AllTrim( cValor )

   CASE _aEstructura[ nCampo, DBS_TYPE ] == "D"
      IF IS_SQLRDD
         bd := Set ( _SET_DATEFORMAT, "yyyy-mm-dd" )
      ENDIF
      _cFiltro := _cArea + "->" + ;
         _aEstructura[ nCampo, DBS_NAME ] + ;
         aOperador[ nCompara ]
      _cFiltro += "CToD(" + "'" + cValor + "')"

   CASE _aEstructura[ nCampo, DBS_TYPE ] == "L"
      _cFiltro := _cArea + "->" + ;
         _aEstructura[ nCampo, DBS_NAME ] + ;
         aOperador[ nCompara ]
      _cFiltro += cValor
   ENDCASE
   ( _cArea )->( dbSetFilter( {|| &_cFiltro }, _cFiltro ) )
   _lFiltro := .T.
   IF IS_SQLRDD
      SET ( _SET_DATEFORMAT, bd )
   ENDIF
   wndABM3Filtro.RELEASE
   ABM3Redibuja( .T. )

RETURN NIL


/****************************************************************************************
 *  Aplicación: Comando EDIT para MiniGUI
 *       Autor: Cristóbal Mollá [cemese@terra.es]
 *     Función: ABM3DesactivarFiltro()
 * Descripción: Desactiva el filtro si procede.
 *  Parámetros: Ninguno
 *    Devuelve: NIL
****************************************************************************************/
STATIC FUNCTION ABM3DesactivarFiltro()

   // ------- Desactiva el filtro si procede.
   if ! _lFiltro
      AlertExclamation( _HMG_aLangUser[ 39 ], _cTitulo )
      ABM3Redibuja( .F. )
      RETURN NIL
   ENDIF
   IF AlertYesNo( _HMG_aLangUser[ 40 ], _cTitulo )
      ( _cArea )->( dbClearFilter( NIL ) )
      _lFiltro := .F.
      _cFiltro := ""
      ABM3Redibuja( .T. )
   ENDIF

RETURN NIL


/****************************************************************************************
 *  Aplicación: Comando EDIT para MiniGUI
 *       Autor: Cristóbal Mollá [cemese@terra.es]
 *     Función: ABM3Imprimir()
 * Descripción: Presenta la ventana de recogida de datos para la definición del listado.
 *  Parámetros: Ninguno
 *    Devuelve: NIL
****************************************************************************************/
STATIC FUNCTION ABM3Imprimir()

   // ------- Declaración de variables locales.-----------------------------------
   LOCAL aCampoBase /*as array*/ // Campos de la bdd.
   LOCAL aCampoListado /*as array*/ // Campos del listado.
   LOCAL nRegistro /*as numeric*/ // Numero de registro actual.
   LOCAL nCampo /*as numeric*/ // Numero de campo.
   LOCAL cRegistro1 AS character // Valor del registro inicial.
   LOCAL cRegistro2 AS character // Valor del registro final.
   LOCAL aImpresoras AS array // Impresoras disponibles.

   // ------- Comprueba si se ha pasado la clausula ON PRINT.---------------------
   IF _bImprimir != NIL
      Eval( _bImprimir )
      ABM3Redibuja( .T. )
      RETURN NIL
   ENDIF

   // ------- Obtiene las impresoras disponibles.---------------------------------
   aImpresoras := {}
   INIT PRINTSYS
   GET PRINTERS TO aImpresoras
   RELEASE PRINTSYS

   // ------- Comprueba que hay un indice activo.---------------------------------
   IF _nIndiceActivo == 1
      AlertExclamation( _HMG_aLangUser[ 9 ], _cTitulo )
      RETURN NIL
   ENDIF

   // ------- Inicialización de variables.----------------------------------------
   aCampoListado := {}
   aCampoBase := _aNombreCampo
   SET DELETED ON
   nRegistro := ( _cArea )->( RecNo() )
   IF ( _cArea )->( Deleted() )
      ( _cArea )->( dbSkip() )
   ENDIF

   // Registro inicial y final.
   nCampo := _aIndiceCampo[ _nIndiceActivo ]
   ( _cArea )->( dbGoTop() )
   cRegistro1 := hb_ValToStr( ( _cArea )->( FieldGet( nCampo ) ) )
   ( _cArea )->( dbGoBottom() )
   cRegistro2 := hb_ValToStr( ( _cArea )->( FieldGet( nCampo ) ) )
   ( _cArea )->( dbGoto( nRegistro ) )

   // ------- Definición de la ventana de formato de listado.---------------------
   DEFINE WINDOW wndABM3Listado ;
         AT 0, 0 ;
         WIDTH 390 ;
         HEIGHT 365 ;
         TITLE _HMG_aLangLabel[ 10 ] ;
         ICON "MINIGUI_EDIT_PRINT" ;
         MODAL ;
         NOSIZE ;
         NOSYSMENU ;
         FONT _GetSysFont() SIZE 9

      // Define la barra de botones de la ventana de formato de listado.
      DEFINE TOOLBAR tbListado buttonsize 100, 32 FLAT righttext BORDER
         BUTTON tbbCancelarLis CAPTION _HMG_aLangButton[ 7 ] ;
            PICTURE "MINIGUI_EDIT_CANCEL" ;
            ACTION wndABM3Listado.RELEASE
         BUTTON tbbAceptarLis CAPTION _HMG_aLangButton[ 8 ] ;
            PICTURE "MINIGUI_EDIT_OK" ;
            ACTION ABM3Listado( aImpresoras )

      END TOOLBAR

      // Define la barra de estado de la ventana de formato de listado.
      DEFINE STATUSBAR FONT _GetSysFont() SIZE 9
         STATUSITEM ""
      END STATUSBAR
   END WINDOW

   // ------- Define los controles de edición de la ventana de formato de listado.-
   // Frame.
   @ 45, 10 FRAME frmListado ;
      OF wndABM3Listado ;
      CAPTION "" ;
      WIDTH wndABM3Listado.WIDTH - 25 ;
      HEIGHT wndABM3Listado.HEIGHT - 100

   // Label
   @ 65, 20 LABEL lblCampoBase ;
      OF wndABM3Listado ;
      VALUE _HMG_aLangLabel[ 11 ] ;
      WIDTH 140 ;
      HEIGHT 25 ;
      FONT _GetSysFont() SIZE 9
   @ 65, 220 LABEL lblCampoListado ;
      OF wndABM3Listado ;
      VALUE _HMG_aLangLabel[ 12 ] ;
      WIDTH 140 ;
      HEIGHT 25 ;
      FONT _GetSysFont() SIZE 9
   @ 200, 20 LABEL lblImpresoras ;
      OF wndABM3Listado ;
      VALUE _HMG_aLangLabel[ 13 ] ;
      WIDTH 140 ;
      HEIGHT 25 ;
      FONT _GetSysFont() SIZE 9
   @ 200, 170 LABEL lblInicial ;
      OF wndABM3Listado ;
      VALUE _HMG_aLangLabel[ 14 ] ;
      WIDTH 160 ;
      HEIGHT 25 ;
      FONT _GetSysFont() SIZE 9
   @ 255, 170 LABEL lblFinal ;
      OF wndABM3Listado ;
      VALUE _HMG_aLangLabel[ 15 ] ;
      WIDTH 160 ;
      HEIGHT 25 ;
      FONT _GetSysFont() SIZE 9

   // Listbox.
   @ 85, 20 LISTBOX lbxCampoBase ;
      OF wndABM3Listado ;
      WIDTH 140 ;
      HEIGHT 100 ;
      ITEMS aCampoBase ;
      VALUE 1 ;
      FONT "Arial" SIZE 9 ;
      ON GOTFOCUS wndABM3Listado.StatusBar.Item( 1 ) := _HMG_aLangUser[ 12 ] ;
      ON LOSTFOCUS wndABM3Listado.StatusBar.Item( 1 ) := ""
   @ 85, 220 LISTBOX lbxCampoListado ;
      OF wndABM3Listado ;
      WIDTH 140 ;
      HEIGHT 100 ;
      ITEMS aCampoListado ;
      VALUE 1 ;
      FONT "Arial" SIZE 9 ;
      ON gotFocus wndABM3Listado.StatusBar.Item( 1 ) := _HMG_aLangUser[ 13 ] ;
      ON LOSTFOCUS wndABM3Listado.StatusBar.Item( 1 ) := ""

   // ComboBox.
   @ 220, 20 COMBOBOX cbxImpresoras ;
      OF wndABM3Listado ;
      ITEMS aImpresoras ;
      VALUE 1 ;
      WIDTH 140 ;
      FONT "Arial" SIZE 9 ;
      ON GOTFOCUS wndABM3Listado.StatusBar.Item( 1 ) := _HMG_aLangUser[ 14 ] ;
      ON LOSTFOCUS wndABM3Listado.StatusBar.Item( 1 ) := ""

   // PicButton.
   @ 90, 170 BUTTON btnMas ;
      OF wndABM3Listado ;
      PICTURE "MINIGUI_EDIT_ADD" ;
      ACTION ABM3DefinirColumnas( ABM_LIS_ADD ) ;
      WIDTH 40 ;
      HEIGHT 40 ;
      ON GOTFOCUS wndABM3Listado.StatusBar.Item( 1 ) := _HMG_aLangUser[ 15 ] ;
      ON LOSTFOCUS wndABM3Listado.StatusBar.Item( 1 ) := ""
   @ 140, 170 BUTTON btnMenos ;
      OF wndABM3Listado ;
      PICTURE "MINIGUI_EDIT_DEL" ;
      ACTION ABM3DefinirColumnas( ABM_LIS_DEL ) ;
      WIDTH 40 ;
      HEIGHT 40 ;
      ON GOTFOCUS wndABM3Listado.StatusBar.Item( 1 ) := _HMG_aLangUser[ 16 ] ;
      ON LOSTFOCUS wndABM3Listado.StatusBar.Item( 1 ) := ""
   @ 220, 170 BUTTON btnSet1 ;
      OF wndABM3Listado ;
      PICTURE "MINIGUI_EDIT_SET" ;
      ACTION ABM3DefinirRegistro( ABM_LIS_SET1 ) ;
      WIDTH 25 ;
      HEIGHT 25 ;
      ON GOTFOCUS wndABM3Listado.StatusBar.Item( 1 ) := _HMG_aLangUser[ 17 ] ;
      ON LOSTFOCUS wndABM3Listado.StatusBar.Item( 1 ) := ""
   @ 275, 170 BUTTON btnSet2 ;
      OF wndABM3Listado ;
      PICTURE "MINIGUI_EDIT_SET" ;
      ACTION ABM3DefinirRegistro( ABM_LIS_SET2 ) ;
      WIDTH 25 ;
      HEIGHT 25 ;
      ON GOTFOCUS wndABM3Listado.StatusBar.Item( 1 ) := _HMG_aLangUser[ 18 ] ;
      ON LOSTFOCUS wndABM3Listado.StatusBar.Item( 1 ) := ""

   // CheckBox.
   @ 255, 20 CHECKBOX chkVistas ;
      OF wndABM3Listado ;
      CAPTION _HMG_aLangLabel[ 18 ] ;
      WIDTH 140 ;
      HEIGHT 25 ;
      VALUE .T. ;
      FONT _GetSysFont() SIZE 9
   @ 275, 20 CHECKBOX chkPrevio ;
      OF wndABM3Listado ;
      CAPTION _HMG_aLangLabel[ 17 ] ;
      WIDTH 140 ;
      HEIGHT 25 ;
      VALUE .T. ;
      FONT _GetSysFont() SIZE 9

   // Editbox.
   @ 220, 196 TEXTBOX txtRegistro1 ;
      OF wndABM3Listado ;
      VALUE cRegistro1 ;
      HEIGHT 25 ;
      WIDTH 160 ;
      FONT "Arial" SIZE 9 ;
      MAXLENGTH 16
   @ 275, 196 TEXTBOX txtRegistro2 ;
      OF wndABM3Listado ;
      VALUE cRegistro2 ;
      HEIGHT 25 ;
      WIDTH 160 ;
      FONT "Arial" SIZE 9 ;
      MAXLENGTH 16

   // ------- Estado de los controles.--------------------------------------------
   wndABM3Listado .txtRegistro1.Enabled := .F.
   wndABM3Listado .txtRegistro2.Enabled := .F.

   // ------- Comrprueba que la selección de registros es posible.----------------
   nCampo := _aIndiceCampo[ _nIndiceActivo ]
   IF _aEstructura[ nCampo, DBS_TYPE ] == "L" .OR. _aEstructura[ nCampo, DBS_TYPE ] == "M"
      wndABM3Listado .btnSet1.Enabled := .F.
      wndABM3Listado .btnSet2.Enabled := .F.
   ENDIF

   // ------- Activación de la ventana de formato de listado.---------------------
   CENTER WINDOW wndABM3Listado
   ACTIVATE WINDOW wndABM3Listado

   // ------- Restaura.-----------------------------------------------------------
   SET DELETED OFF
   ( _cArea )->( dbGoto( nRegistro ) )
   ABM3Redibuja( .F. )

RETURN NIL


/****************************************************************************************
 *  Aplicación: Comando EDIT para MiniGUI
 *       Autor: Cristóbal Mollá [cemese@terra.es]
 *     Función: ABM3DefinirRegistro( nAccion )
 * Descripción:
 *  Parámetros: [nAccion]       Numerico. Indica el tipo de accion realizado.
 *    Devuelve: NIL
****************************************************************************************/
STATIC FUNCTION ABM3DefinirRegistro( nAccion )

   // ------- Declaración de variables locales.-----------------------------------
   LOCAL nRegistro AS character // Puntero de registros.
   LOCAL nReg AS character // Registro seleccionado.
   LOCAL cValor AS character // Valor del registro seleccionado.
   LOCAL nCampo /*as numeric*/ // Numero del campo indice.

   // ------- Inicializa las variables.-------------------------------------------
   nRegistro := ( _cArea )->( RecNo() )

   // ------- Selecciona el registro.---------------------------------------------
   nReg := ABM3Seleccionar()
   IF nReg == 0
      ( _cArea )->( dbGoto( nRegistro ) )
      RETURN NIL
   ELSE
      ( _cArea )->( dbGoto( nReg ) )
      nCampo := _aIndiceCampo[ _nIndiceActivo ]
      cValor := hb_ValToStr( ( _cArea )->( FieldGet( nCampo ) ) )
   ENDIF

   // ------- Actualiza según la acción.------------------------------------------
   DO CASE
   CASE nAccion == ABM_LIS_SET1
      wndABM3Listado .txtRegistro1.VALUE := cValor
   CASE nAccion == ABM_LIS_SET2
      wndABM3Listado .txtRegistro2.VALUE := cValor
   ENDCASE

   // ------- Restaura el registro.
   ( _cArea )->( dbGoto( nRegistro ) )

RETURN NIL


/****************************************************************************************
 *  Aplicación: Comando EDIT para MiniGUI
 *       Autor: Cristóbal Mollá [cemese@terra.es]
 *     Función: ABM3DefinirColumnas( nAccion )
 * Descripción: Controla el contenido de las listas al pulsar los botones de añadir y
 *              eliminar campos del listado.
 *  Parámetros: [nAccion]       Numerico. Indica el tipo de accion realizado.
 *    Devuelve: NIL
****************************************************************************************/
STATIC FUNCTION ABM3DefinirColumnas( nAccion )

   // ------- Declaración de variables locales.-----------------------------------
   LOCAL aCampoBase /*as array*/ // Campos de la bbd.
   LOCAL aCampoListado /*as array*/ // Campos del listado.
   LOCAL i /*as numeric*/ // Indice de iteración.
   LOCAL nItem /*as numeric*/ // Numero del item seleccionado.
   LOCAL cValor AS character

   // ------- Inicialización de variables.----------------------------------------
   aCampoBase := {}
   aCampoListado := {}
   FOR i := 1 TO wndABM3Listado.lbxCampoBase.ItemCount
      AAdd( aCampoBase, wndABM3Listado.lbxCampoBase.Item( i ) )
   NEXT
   FOR i := 1 TO wndABM3Listado.lbxCampoListado.ItemCount
      AAdd( aCampoListado, wndABM3Listado.lbxCampoListado.Item( i ) )
   NEXT

   // ------- Ejecuta según la acción.--------------------------------------------
   DO CASE
   CASE nAccion == ABM_LIS_ADD

      // Obtiene la columna a añadir.
      nItem := wndABM3Listado.lbxCampoBase.VALUE
      cValor := wndABM3Listado.lbxCampoBase.Item( nItem )

      // Actualiza los datos de los campos de la base.
      IF Len( aCampoBase ) == 0
         AlertExclamation( _HMG_aLangUser[ 23 ], _cTitulo )
         RETURN NIL
      ELSE
         wndABM3Listado.lbxCampoBase.DeleteAllItems
         FOR i := 1 TO Len( aCampoBase )
            IF i != nItem
               wndABM3Listado.lbxCampoBase.AddItem( aCampoBase[ i ] )
            ENDIF
         NEXT
         wndABM3Listado.lbxCampoBase.VALUE := iif( nItem > 1, nItem - 1, 1 )
      ENDIF

      // Actualiza los datos de los campos del listado.
      IF Empty( cValor )
         AlertExclamation( _HMG_aLangUser[ 23 ], _cTitulo )
         RETURN NIL
      ELSE
         wndABM3Listado.lbxCampoListado.AddItem( cValor )
         wndABM3Listado.lbxCampoListado.VALUE := ;
            wndABM3Listado.lbxCampoListado.ItemCount
      ENDIF
   CASE nAccion == ABM_LIS_DEL

      // Obtiene la columna a quitar.
      nItem := wndABM3Listado.lbxCampoListado.VALUE
      cValor := wndABM3Listado.lbxCampoListado.Item( nItem )

      // Actualiza los datos de los campos del listado.
      IF Len( aCampoListado ) == 0
         AlertExclamation( _HMG_aLangUser[ 23 ], _cTitulo )
         RETURN NIL
      ELSE
         wndABM3Listado.lbxCampoListado.DeleteAllItems
         FOR i := 1 TO Len( aCampoListado )
            IF i != nItem
               wndABM3Listado.lbxCampoListado.AddItem( aCampoListado[ i ] )
            ENDIF
         NEXT
         wndABM3Listado.lbxCampoListado.VALUE := ;
            wndABM3Listado.lbxCampoListado.ItemCount
      ENDIF

      // Actualiza los datos de los campos de la base.
      IF Empty( cValor )
         AlertExclamation( _HMG_aLangUser[ 23 ], _cTitulo )
         RETURN NIL
      ELSE
         wndABM3Listado.lbxCampoBase.DeleteAllItems
         FOR i := 1 TO Len( _aNombreCampo )
            IF AScan( aCampoBase, _aNombreCampo[ i ] ) != 0
               wndABM3Listado.lbxCampoBase.AddItem( _aNombreCampo[ i ] )
            ENDIF
            IF _aNombreCampo[ i ] == cValor
               wndABM3Listado.lbxCampoBase.AddItem( _aNombreCampo[ i ] )
            ENDIF
         NEXT
         wndABM3Listado.lbxCampoBase.VALUE := 1
      ENDIF
   ENDCASE

RETURN NIL


/****************************************************************************************
 *  Aplicación: Comando EDIT para MiniGUI
 *       Autor: Cristóbal Mollá [cemese@terra.es]
 *     Función: ABM3Listado()
 * Descripción: Imprime la selecciona realizada por ABM3Imprimir()
 *  Parámetros: Ninguno
 *    Devuelve: NIL
****************************************************************************************/
STATIC FUNCTION ABM3Listado( aImpresoras )

   // ------- Declaración de variables locales.-----------------------------------
   LOCAL i /*as numeric*/ // Indice de iteración.
   LOCAL cCampo /*as character*/ // Nombre del campo indice.
   LOCAL aCampo /*as array*/ // Nombres de los campos.
   LOCAL nCampo /*as numeric*/ // Numero del campo actual.
   LOCAL nPosicion /*as numeric*/ // Posición del campo.
   LOCAL aNumeroCampo /*as array*/ // Numeros de los campos.
   LOCAL aAncho /*as array*/ // Anchos de las columnas.
   LOCAL nAncho /*as numeric*/ // Ancho de las columna actual.
   LOCAL lPrevio /*as logical*/ // Previsualizar.
   LOCAL lVistas /*as logical*/ // Vistas en miniatura.
   LOCAL nImpresora /*as numeric*/ // Numero de la impresora.
   LOCAL cImpresora AS character // Nombre de la impresora.
   LOCAL lOrientacion /*as logical*/ // Orientación de la página.
   LOCAL lSalida /*as logical*/ // Control de bucle.
   LOCAL lCabecera /*as logical*/ // ¿Imprimir cabecera?.
   LOCAL nFila /*as numeric*/ // Numero de la fila.
   LOCAL nColumna /*as numeric*/ // Numero de la columna.
   LOCAL nPagina /*as numeric*/ // Numero de página.
   LOCAL nPaginas /*as numeric*/ // Páginas totales.
   LOCAL cPie AS character // Texto del pie de página.
   LOCAL nPrimero /*as numeric*/ // Numero del primer registro a imprimir.
   LOCAL nUltimo /*as numeric*/ // Numero del ultimo registro a imprimir.
   LOCAL nTotales /*as numeric*/ // Registros totales a imprimir.
   LOCAL nRegistro /*as numeric*/ // Numero del registro actual.
   LOCAL cRegistro1 AS character // Valor del registro inicial.
   LOCAL cRegistro2 AS character // Valor del registro final.
   LOCAL xRegistro1 // Valor de comparación.
   LOCAL xRegistro2 // Valor de comparación.

   // ------- Inicialización de variables.----------------------------------------
   // Previsualizar.
   lPrevio := wndABM3Listado .chkPrevio.VALUE
   lVistas := wndABM3Listado .chkVistas.VALUE

   // Nombre de la impresora.
   nImpresora := wndABM3Listado.cbxImpresoras.VALUE
   IF nImpresora == 0
      AlertExclamation( _HMG_aLangUser[ 32 ], '' )
   ELSE
      cImpresora := aImpresoras[ nImpresora ]
   ENDIF

   // Nombre del campo.
   aCampo := {}
   FOR i := 1 TO wndABM3Listado.lbxCampoListado.ItemCount
      cCampo := wndABM3Listado.lbxCampoListado.Item( i )
      AAdd( aCampo, cCampo )
   NEXT
   IF Len( aCampo ) == 0
      AlertExclamation( _HMG_aLangUser[ 23 ], _cTitulo )
      RETURN NIL
   ENDIF

   // Número del campo.
   aNumeroCampo := {}
   FOR i := 1 TO Len( aCampo )
      nPosicion := AScan( _aNombreCampo, aCampo[ i ] )
      AAdd( aNumeroCampo, nPosicion )
   NEXT

   // ------- Obtiene el ancho de impresión.--------------------------------------
   aAncho := {}
   FOR i := 1 TO Len( aNumeroCampo )
      nCampo := aNumeroCampo[ i ]
      DO CASE
      CASE _aEstructura[ nCampo, DBS_TYPE ] == "D"
         nAncho := 9
      CASE _aEstructura[ nCampo, DBS_TYPE ] == "M"
         nAncho := 20
      OTHERWISE
         nAncho := _aEstructura[ nCampo, DBS_LEN ]
      ENDCASE
      nAncho := iif( Len( _aNombreCampo[ nCampo ] ) > nAncho, ;
         Len( _aNombreCampo[ nCampo ] ), ;
         nAncho )
      AAdd( aAncho, 2 + nAncho )
   NEXT

   // ------- Comprueba el ancho de impresión.------------------------------------
   nAncho := 0
   FOR i := 1 TO Len( aAncho )
      nAncho += aAncho[ i ]
   NEXT
   IF nAncho > 164
      AlertExclamation( _HMG_aLangUser[ 24 ], _cTitulo )
      RETURN NIL
   ELSE
      lOrientacion := ( nAncho > 109 ) // Horizontal / Vertical
   ENDIF

   // ------- Valores de inicio y fin de listado.---------------------------------
   nRegistro := ( _cArea )->( RecNo() )
   cRegistro1 := wndABM3Listado .txtRegistro1.VALUE
   cRegistro2 := wndABM3Listado .txtRegistro2.VALUE
   DO CASE
   CASE _aEstructura[ _aIndiceCampo[ _nIndiceActivo ], DBS_TYPE ] == "C"
      xRegistro1 := cRegistro1
      xRegistro2 := cRegistro2
   CASE _aEstructura[ _aIndiceCampo[ _nIndiceActivo ], DBS_TYPE ] == "N"
      xRegistro1 := Val( cRegistro1 )
      xRegistro2 := Val( cRegistro2 )
   CASE _aEstructura[ _aIndiceCampo[ _nIndiceActivo ], DBS_TYPE ] == "D"
      IF IS_SQLRDD
         xRegistro1 := hb_CToD( cRegistro1, "yyyy-mm-dd" )
         xRegistro2 := hb_CToD( cRegistro2, "yyyy-mm-dd" )
      ELSE
         xRegistro1 := CToD( cRegistro1 )
         xRegistro2 := CToD( cRegistro2 )
      ENDIF
   CASE _aEstructura[ _aIndiceCampo[ _nIndiceActivo ], DBS_TYPE ] == "L"
      xRegistro1 := ( cRegistro1 == ".t." )
      xRegistro2 := ( cRegistro2 == ".t." )
   ENDCASE
   ( _cArea )->( dbSeek( xRegistro2 ) )
   nUltimo := ( _cArea )->( RecNo() )
   ( _cArea )->( dbSeek( xRegistro1 ) )
   nPrimero := ( _cArea )->( RecNo() )

   // ------- Obtiene el número de páginas.---------------------------------------
   nTotales := 1
   ( _cArea )->( dbEval( {|| nTotales++ },, {|| !( RecNo() == nUltimo ) .AND. ! Eof() },,, .T. ) )
   ( _cArea )->( dbGoto( nPrimero ) )
   IF lOrientacion
      IF Mod( nTotales, 33 ) == 0
         nPaginas := Int( nTotales / 33 )
      ELSE
         nPaginas := Int( nTotales / 33 ) + 1
      ENDIF
   ELSE
      IF Mod( nTotales, 55 ) == 0
         nPaginas := Int( nTotales / 55 )
      ELSE
         nPaginas := Int( nTotales / 55 ) + 1
      ENDIF
   ENDIF

   // ------- Inicializa el listado.----------------------------------------------
   INIT PRINTSYS

   // Opciones de la impresión.
   IF lPrevio
      SELECT PRINTER cImpresora PREVIEW
   ELSE
      SELECT PRINTER cImpresora
   ENDIF
   IF lVistas
      ENABLE THUMBNAILS
   ENDIF

   // Control de errores.
   IF HBPRNERROR > 0
      AlertExclamation( _HMG_aLangUser[ 25 ], _cTitulo )
      RETURN NIL
   ENDIF

   // Definición de las fuentes del listado.
   DEFINE FONT "a8" NAME "Arial" SIZE 8
   DEFINE FONT "a9" NAME "Arial" SIZE 9
   DEFINE FONT "a9n" NAME "Arial" SIZE 9 BOLD
   DEFINE FONT "a10" NAME "Arial" SIZE 10
   DEFINE FONT "a12n" NAME "Arial" SIZE 12 BOLD

   // Definición de los tipos de linea.
   DEFINE PEN "l0" STYLE PS_SOLID WIDTH 0.0 COLOR 0x000000
   DEFINE PEN "l1" STYLE PS_SOLID WIDTH 0.1 COLOR 0x000000
   DEFINE PEN "l2" STYLE PS_SOLID WIDTH 0.2 COLOR 0x000000

   // Definición de los patrones de relleno.
   DEFINE BRUSH "s0" STYLE BS_NULL
   DEFINE BRUSH "s1" STYLE BS_SOLID COLOR RGB( 220, 220, 220 )

   // Inicio del listado.
   lCabecera := .T.
   lSalida := .T.
   nFila := iif( Empty( _cFiltro ), 12, 13 )
   nPagina := 1
   START DOC
   SET UNITS ROWCOL
   IF lOrientacion
      SET PAGE ORIENTATION DMORIENT_LANDSCAPE ;
         PAPERSIZE DMPAPER_A4 FONT "a10"
   ELSE
      SET PAGE ORIENTATION DMORIENT_PORTRAIT ;
         PAPERSIZE DMPAPER_A4 FONT "a10"
   ENDIF
   START PAGE
   DO WHILE lSalida

      // Cabecera el listado.
      IF lCabecera
         SET TEXT ALIGN RIGHT
         SELECT pen "l2"
         @ 5, HBPRNMAXCOL - 5 SAY _cTitulo FONT "a12n" TO PRINT
         @ 6, 10, 6, HBPRNMAXCOL - 5 line
         SELECT pen "l0"
         @ 7, 29 SAY _HMG_aLangUser[ 26 ] FONT "a9n" TO PRINT
         @ 8, 29 SAY _HMG_aLangUser[ 27 ] FONT "a9n" TO PRINT
         @ 9, 29 SAY _HMG_aLangUser[ 28 ] FONT "a9n" TO PRINT
         if ! Empty( _cFiltro )
            @ 10, 29 SAY _HMG_aLangUser[ 33 ] FONT "a9n" TO PRINT
         ENDIF
         SET TEXT ALIGN LEFT
         @ 7, 31 say ( _cArea )->( ordName() ) FONT "a9" TO PRINT
         @ 8, 31 SAY cRegistro1 FONT "a9" TO PRINT
         @ 9, 31 SAY cRegistro2 FONT "a9" TO PRINT
         if ! Empty( _cFiltro )
            @ 10, 31 SAY _cFiltro FONT "a9" TO PRINT
         ENDIF
         nColumna := 10
         SELECT pen "l1"
         SELECT brush "s1"
         FOR i := 1 TO Len( aCampo )
            @ nFila - .9, nColumna, nFila, nColumna + aAncho[ i ] fillrect
            @ nFila - 1, nColumna + 1 SAY aCampo[ i ] FONT "a9n" TO PRINT
            nColumna += aAncho[ i ]
         NEXT
         SELECT pen "l0"
         SELECT brush "s0"
         lCabecera := .F.
      ENDIF

      // Registros.
      nColumna := 10
      FOR i := 1 TO Len( aNumeroCampo )
         nCampo := aNumeroCampo[ i ]
         DO CASE
         CASE _aEstructura[ nCampo, DBS_TYPE ] == "N"
            SET TEXT ALIGN RIGHT
            @ nFila, nColumna + aAncho[ i ] say ( _cArea )->( FieldGet( aNumeroCampo[ i ] ) ) FONT "a8" TO PRINT
         CASE _aEstructura[ nCampo, DBS_TYPE ] == "L"
            SET TEXT ALIGN LEFT
            @ nFila, nColumna + 1 SAY iif( ( _cArea )->( FieldGet( aNumeroCampo[ i ] ) ), _HMG_aLangUser[ 29 ], _HMG_aLangUser[ 30 ] ) FONT "a8" TO PRINT
         CASE _aEstructura[ nCampo, DBS_TYPE ] == "M"
            SET TEXT ALIGN LEFT
            @ nFila, nColumna + 1 SAY SubStr( ( _cArea )->( FieldGet( aNumeroCampo[ i ] ) ), 1, 20 ) FONT "a8" TO PRINT
         OTHERWISE
            SET TEXT ALIGN LEFT
            @ nFila, nColumna + 1 say ( _cArea )->( FieldGet( aNumeroCampo[ i ] ) ) FONT "a8" TO PRINT
         ENDCASE
         nColumna += aAncho[ i ]
      NEXT
      nFila++

      // Comprueba el final del registro.
      IF ( _cArea )->( RecNo() ) == nUltimo .OR. ( _cArea )->( Eof() )
         lSalida := .F.
      ENDIF
      ( _cArea )->( dbSkip( 1 ) )

      // Pie.
      IF lOrientacion
         IF nFila > 44
            SET TEXT ALIGN LEFT
            SELECT pen "l2"
            @ 46, 10, 46, HBPRNMAXCOL - 5 line
            cPie := hb_ValToStr( Date() ) + " " + Time()
            @ 46, 10 SAY cPie FONT "a9n" TO PRINT
            SET TEXT ALIGN RIGHT
            cPie := _HMG_aABMLangLabel[ 22 ] + ;
               AllTrim( Str( nPagina ) ) + ;
               "/" + ;
               AllTrim( Str( nPaginas ) )
            @ 46, HBPRNMAXCOL - 5 SAY cPie FONT "a9n" TO PRINT
            nPagina++
            nFila := iif( Empty( _cFiltro ), 12, 13 )
            lCabecera := .T.
            END PAGE
            START PAGE
         ENDIF
      ELSE
         IF nFila > 66
            SET TEXT ALIGN LEFT
            SELECT pen "l2"
            @ 68, 10, 68, HBPRNMAXCOL - 5 line
            cPie := hb_ValToStr( Date() ) + " " + Time()
            @ 68, 10 SAY cPie FONT "a9n" TO PRINT
            SET TEXT ALIGN RIGHT
            cPie := _HMG_aABMLangLabel[ 22 ] + ;
               AllTrim( Str( nPagina ) ) + ;
               "/" + ;
               AllTrim( Str( nPaginas ) )
            @ 68, HBPRNMAXCOL - 5 SAY cPie FONT "a9n" TO PRINT
            nFila := iif( Empty( _cFiltro ), 12, 13 )
            nPagina++
            lCabecera := .T.
            END PAGE
            START PAGE
         ENDIF
      ENDIF
   ENDDO

   // Comprueba que se imprime el pie de la ultima hoja.----------
   IF nPagina == nPaginas
      IF lOrientacion
         SET TEXT ALIGN LEFT
         SELECT pen "l2"
         @ 46, 10, 46, HBPRNMAXCOL - 5 line
         cPie := hb_ValToStr( Date() ) + " " + Time()
         @ 46, 10 SAY cPie FONT "a9n" TO PRINT
         SET TEXT ALIGN RIGHT
         cPie := _HMG_aABMLangLabel[ 22 ] + ;
            AllTrim( Str( nPagina ) ) + ;
            "/" + ;
            AllTrim( Str( nPaginas ) )
         @ 46, HBPRNMAXCOL - 5 SAY cPie FONT "a9n" TO PRINT
      ELSE
         SET TEXT ALIGN LEFT
         SELECT pen "l2"
         @ 68, 10, 68, HBPRNMAXCOL - 5 line
         cPie := hb_ValToStr( Date() ) + " " + Time()
         @ 68, 10 SAY cPie FONT "a9n" TO PRINT
         SET TEXT ALIGN RIGHT
         cPie := _HMG_aABMLangLabel[ 22 ] + ;
            AllTrim( Str( nPagina ) ) + ;
            "/" + ;
            AllTrim( Str( nPaginas ) )
         @ 68, HBPRNMAXCOL - 5 SAY cPie FONT "a9n" TO PRINT
      ENDIF

      END PAGE
   ENDIF
   END DOC

   RELEASE PRINTSYS

   // ------- Cierra la ventana.--------------------------------------------------
   ( _cArea )->( dbGoto( nRegistro ) )
   wndABM3Listado.RELEASE

RETURN NIL

/*-------------------------------------------------------------------------------------------*/

STATIC FUNCTION _KeysOff

   LOCAL bKeyBlock, abRetVal := Array( 0 )

   STORE KEY ESCAPE OF wndABM3EditNuevoSplit TO bKeyBlock
   AAdd( abRetVal, bKeyBlock )
   RELEASE KEY ESCAPE OF wndABM3EditNuevoSplit

   STORE KEY RETURN OF wndABM3EditNuevoSplit TO bKeyBlock
   AAdd( abRetVal, bKeyBlock )
   RELEASE KEY RETURN OF wndABM3EditNuevoSplit

RETURN( abRetVal )

/*-------------------------------------------------------------------------------------------*/

STATIC PROCEDURE _KeysOn( abKeyBlocks )

   _DefineHotKey( "wndABM3EditNuevoSplit", 0, 27, abKeyBlocks[ 1 ] )
   _DefineHotKey( "wndABM3EditNuevoSplit", 0, 13, abKeyBlocks[ 2 ] )

RETURN
