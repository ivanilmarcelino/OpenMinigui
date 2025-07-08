/*

 BadaSystem
 Program       : dic_structure
 Modulo        : Create a data dictionary system for maintenance
                 to the boards in the course of a project
 Compilador    : MINIGUI - Harbour Win32 GUI
 Link          : BCC 32 bit
 Autor         : Marcos Jarrín
 email         : marvijarrin@gmail.com
 website       : badasystem.org
 Date          : 17/04/2021
 Update        : 16/06/2022

*/

REQUEST DBFCDX

#include "minigui.ch"
#include "rddsys.ch"
#include "dic_structure.ch"

//
//
//
PROCEDURE Main()

   LOCAL nFila, nCol

   //
   // Identify the tables of the dataset and create if they do not exist
   //
   nFila := 40
   nCol := 60

   // Use RDD DBFCDX
   rddSetDefault( "DBFCDX" )

   SET DEFAULT ICON TO GetStartupFolder() + "\Main.ico"
   DEFINE WINDOW Win_1 ;
         AT 0, 0 ;
         WIDTH 600 ;
         HEIGHT 200 ;
         TITLE 'Test Dataset' ;
         MAIN ;
         FONT 'ARIAL' SIZE 9

      @ nFila, nCol BUTTON GENERAR ;
         CAPTION '&Generate Structure' WIDTH 130 ;
         TOOLTIP 'Genera Estructura Inicial y Archivos' ;
         ACTION ( generateStructure(), fillData(), MakeDataset(), llenarDatostablasCreadas() )

      @ nFila, nCol + 170 BUTTON EJECUTARESTRUC ;
         CAPTION '&Modify Structure' WIDTH 130 ;
         TOOLTIP 'Modify structure or add new structure' ;
         ACTION IdeDicStructure()


      @ nFila, nCol + 340 BUTTON CREARESTRUC ;
         CAPTION '&Create New Structure' WIDTH 130 ;
         TOOLTIP 'Checks and Modifies Structure but does not create new files' ;
         ACTION MakeStructure()


      @ nFila + 70, nCol + 190 BUTTON EXIT ;
         CAPTION '&Exit' ;
         ACTION Win_1.Release()
   END WINDOW

   CENTER WINDOW Win_1

   ACTIVATE WINDOW Win_1

RETURN


//
// Create the necessary tables to manage the system, verify if it exists,
// if they do not exist, create them
//
PROCEDURE generateStructure()

   LOCAL aStructure01 := {}
   LOCAL aStructure02 := {}
   LOCAL aStructure03 := {}
   LOCAL aStructure04 := {}

   FIELD codataset, namedatset, servertype, password, pathbase, pathdefaul, pathtemp, vesion, datedatset, timedatset, statusdase, commendase IN dataset
   FIELD codtable, codatset, nametable, ALIAS, INDEX, rdd, datetable, timetable, statustab, commentab IN tables
   FIELD codstruc, coddataset, codtables, namestruc, tipo, SIZE, DECIMAL, indexstruc, statustruc, commentstu IN STRUC
   FIELD codindex, codataseti, codtablei, nameindex, TAG, tipoindex, expresion, statusind, comentind IN INDEX

   //
   // Dataset dataset.dbf   aStructure01
   //
   aStructure01 := { { "codataset", "N", 4, 0 }, ;   // Key primary
   { "namedatset", "C", 50, 0 }, ;
      { "servertype", "C", 25, 0 }, ;
      { "password", "C", 500, 0 }, ;
      { "pathbase", "C", 255, 0 }, ;
      { "pathdefaul", "C", 255, 0 }, ;
      { "pathtemp", "C", 255, 0 }, ;
      { "vesion", "C", 10, 0 }, ;
      { "datedatset", "D", 8, 0 }, ;
      { "timedatset", "C", 8, 0 }, ;
      { "statusdase", "L", 1, 0 }, ;
      { "commendase", "M", 10, 0 } }

   if ! File( "dataset.dbf" )
      dbCreate( "dataset", aStructure01, , NEWAREA, "dataset" )
      INDEX ON codataset TAG codataset
      INDEX ON namedatset TAG namedatset
      SET ORDER TO TAG namedatset
   ENDIF

   //
   // Table that make up the dataset. tables.dbf   aStructure02
   //
   aStructure02 := { { "codtable", "N", 4, 0 }, ;   // primary key
   { "codatset", "N", 4, 0 }, ;   // foreign key
   { "nametable", "C", 50, 0 }, ;
      { "alias", "C", 50, 0 }, ;
      { "index", "L", 1, 0 }, ; // si la tala tiene archivos indices
   { "rdd", "C", 25, 0 }, ;
      { "datetable", "D", 8, 0 }, ;
      { "timetable", "C", 8, 0 }, ;
      { "statustab", "L", 1, 0 }, ;
      { "commentab", "M", 10, 0 } }

   if ! File( "tables.dbf" )
      dbCreate( "tables", aStructure02, , NEWAREA, "tables" )
      INDEX ON codtable TAG codtabla
      INDEX ON codatset TAG codatset
      INDEX ON nametable TAG nametable
      SET ORDER TO TAG codatset
   ENDIF


   //
   // structure of struc.dbf      aStructure03
   //
   aStructure03 := { { "codstruc", "N", 4, 0 }, ;        // primary key
      { "coddataset", "N", 4, 0 }, ;
      { "codtables", "N", 4, 0 }, ; // foreign key
   { "namestruc", "C", 25, 0 }, ;
      { "tipo", "C", 1, 0 }, ;
      { "size", "N", 4, 0 }, ;
      { "decimal", "N", 2, 0 }, ;
      { "indexstruc", "L", 1, 0 }, ;
      { "statustruc", "L", 1, 0 }, ;
      { "commentstu", "M", 10, 0 } }

   if ! File( "struc.dbf" )
      dbCreate( "struc", aStructure03, , NEWAREA, "struc" )
      INDEX ON codstruc TAG codstruc
      INDEX ON codtables TAG codtables
      INDEX ON namestruc TAG namestruc
      INDEX ON AllTrim( Str( coddataset ) ) + AllTrim( Str( codtables ) ) TAG coddataset
      SET ORDER TO TAG coddataset

   ENDIF

   //
   // structure of index.dbf   aStructure04
   //
   aStructure04 := { { "codindex", "N", 4, 0 }, ;          // primary key
      { "codataseti", "N", 4, 0 }, ;
      { "codtablei", "N", 4, 0 }, ; // foreign key
      { "nameindex", "C", 25, 0 }, ;
      { "tag", "C", 50, 0 }, ;
      { "tipoindex", "C", 10, 0 }, ;
      { "expresion", "C", 50, 0 }, ;
      { "statusind", "L", 1, 0 }, ;
      { "comentind", "M", 10, 0 } }

   if ! File( "index.dbf" )
      dbCreate( "index", aStructure04, , NEWAREA, "index" )
      INDEX ON codindex TAG codindex
      INDEX ON codtablei TAG codtablei
      INDEX ON nameindex TAG nameindex
      INDEX ON AllTrim( Str( codataseti ) ) + AllTrim( Str( codtablei ) ) TAG coddatasei
      SET ORDER TO TAG coddatasei
   ENDIF

   dbCloseAll()

RETURN

//
// Identifies the dataset tables and creates if they do not exist.
//
PROCEDURE MakeDataset() // cDataSet)

   LOCAL lExist := FALSE
   LOCAL lTablasDefi := FALSE

   LOCAL nCodDataset := 0
   LOCAL nCodTable := 0
   // local cNombreDataset
   // local cValor
   LOCAL cDataSet

   // dataset
   // tables
   // struc
   // index

   DEFINE WINDOW elidatset ;
         AT 0, 0 ;
         WIDTH 240 ;
         HEIGHT 120 ;
         TITLE "Elegir EL Dataset" ;
         CHILD ;
         NOMAXIMIZE ;
         NOSIZE ;
         ON INIT OpenDataset()


      @ 10, 15 COMBOBOX cNombreDataset ;
         ITEMSOURCE dataset->namedatset ;
         VALUE 1 ;
         WIDTH 200 HEIGHT 100 ;
         FONT "Arial" SIZE 9 ;
         TOOLTIP "Dataset"

      @ 45, 75 BUTTON EXIT ;
         CAPTION '&Ejecutar' ;
         ACTION ( cDataSet := elidatset.cNombreDataset.DisplayValue, elidatset.release )

   END WINDOW
   CENTER WINDOW elidatset
   ACTIVATE WINDOW elidatset

   if ! Empty( cDataSet )
      cDataSet := AllTrim( cDataSet )
   ELSE
      dbCloseAll()
      RETURN
   ENDIF

   dataset->( dbCloseArea() )


   // Dataset
   dbUseArea( NEWAREA, "DBFCDX", "dataset", "dataset", SHARED )
   SET ORDER TO TAG namedatset

   // Tables
   dbUseArea( NEWAREA, , "tables", "tables", SHARED )
   SET ORDER TO TAG codatset

   // structure
   dbUseArea( NEWAREA, , "struc", "struc", SHARED, NOREADONLY )
   SET ORDER TO TAG coddataset

   // index
   dbUseArea( NEWAREA, , "index", "index", SHARED )
   SET ORDER TO TAG coddatasei


   IF dataset->( dbSeek( cDataSet ) )
      lExist := TRUE
      nCodDataset := dataset->codataset
   ELSE
      msgbox( "Dataset no existe" )
      lExist := FALSE
   ENDIF


   // Search if there are defined tables of the dataset
   IF lExist

      IF tables->( dbSeek( nCodDataset ) )
         lTablasDefi := TRUE

         nCodTable := tables->codtable
         //
         // Create all tables defined in the dataset
         //
         WHILE nCodDataset == tables->codatset .AND. tables->( ! Eof() )

            // Check if the structure table has information from the dataset table
            IF STRUC->( dbSeek( AllTrim( Str(tables->codatset ) ) + AllTrim( Str(tables->codtable ) ) ) )
               MakeTable( nCodTable, AllTrim( tables->nametable ) )
            ELSE
               msgbox( "Table does not have its structure defined" )
            ENDIF
            tables->( dbSkip() )
            nCodTable := tables->codtable
         END
      ELSE
         msgbox( "It has no tables defined" )
         lTablasDefi := FALSE
      ENDIF

   ENDIF

   dbCloseAll()

RETURN

//
//
//
PROCEDURE OpenDataset()

   // Dataset
   dbUseArea( NEWAREA, "DBFCDX", "dataset", "dataset", SHARED )
   SET ORDER TO TAG namedatset

RETURN


//
// Create the table with the defined structure
// structure of struc.dbf
//
PROCEDURE MakeTable( nCodTable, cNameTable )

   FIELD codstruc, codtable, namestruc, tipo, SIZE, DECIMAL, INDEX, rdd, vesion, STATUS, comment IN STRUC
   LOCAL aStructure := {}

   //
   // Check if the table exists, if it doesn't exist create it
   //
   IF File( cNameTable + ".dbf" )
      if ! File( cNameTable + ".cdx" )
         // Crea indices de la tabla
         CreaIndices( tables->codatset, nCodTable, cNameTable )
      ENDIF
   ELSE

      WHILE nCodTable == STRUC->codtables .AND. STRUC->( ! Eof() )
         AAdd( aStructure, { AllTrim( STRUC->namestruc ), Upper( AllTrim( STRUC->tipo ) ), STRUC->SIZE, STRUC->decimal } )
         STRUC->( dbSkip() )
      END

      dbCreate( cNameTable, aStructure )

      // Create table indexes
      CreaIndices( tables->codatset, nCodTable, cNameTable )
   END

RETURN

//
// Create table indexes with the defined structure
//
//
PROCEDURE CreaIndices( nCodDataset, nCodTable, cNameTable )

   FIELD codindex, codtablei, nameindex, TAG, tipoindex, expresion, vesionind, statusind, comentind IN INDEX
   LOCAL cNombre, cTag


   // Check if the table has defined indexes to create them next to the table
   IF INDEX->( dbSeek( AllTrim( Str(nCodDataset ) ) + AllTrim( Str(nCodTable ) ) ) )
      if ! File( cNameTable + ".cdx" )
         dbUseArea( NEWAREA, , cNameTable )
         WHILE INDEX->codtablei == nCodTable .AND. INDEX->( ! Eof() )
            cNombre := AllTrim( INDEX->nameindex )
            cTag := AllTrim( INDEX->tag )

            // index on &cExpresion tag &cExpresion
            INDEX ON &(cNombre) TAG &(cTag)
            INDEX->( dbSkip() )
         END
      ENDIF
   ELSE
      msgbox( "The indices of the table are not defined" )
   ENDIF

RETURN



//
//
//
PROCEDURE MakeStructure()

   LOCAL nCodigoTables, cNameTable
   LOCAL aStructure := {}


   if ! File( "dataset.dbf" )
      msgbox( "The structures do not exist execute the first option" )
      RETURN
   ENDIF

   // tables
   dbUseArea( NEWAREA, "DBFCDX", "tables", "tables", SHARED )
   SET ORDER TO TAG nametable

   // struc
   dbUseArea( NEWAREA, "DBFCDX", "struc", "struc", SHARED )
   SET ORDER TO TAG coddataset // codtables

   // index
   dbUseArea( NEWAREA, , "index", "index", SHARED )
   SET ORDER TO TAG coddatasei

   cNameTable := AllTrim( tables->nametable )
   IF tables->( dbSeek( cNameTable ) )
      nCodigoTables := tables->codtable
      cNameTable := AllTrim( tables->nametable )

      IF STRUC->( dbSeek( AllTrim( Str(tables->codatset ) ) + AllTrim( Str(tables->codtable ) ) ) )
         WHILE tables->( ! Eof() )

            // Check if the files exist
            IF File( cNameTable + ".dbf" )

               WHILE nCodigoTables == STRUC->codtables
                  AAdd( aStructure, { AllTrim( STRUC->namestruc ), AllTrim( STRUC->tipo ), STRUC->SIZE, STRUC->decimal } )
                  STRUC->( dbSkip() )
               END

               badaupdate( cNameTable + ".dbf", "xyz123.dbf", aStructure )
               FErase( cNameTable + ".bak" )
               FRename( cNameTable + ".dbf", cNameTable + ".bak" )
               FRename( "xyz123.dbf", cNameTable + ".dbf" )

               // Crea indices de la tabla
               FErase( cNameTable + ".cdx" )
               CreaIndices( tables->codatset, nCodigoTables, cNameTable )

               tables->( dbSkip() )
               nCodigoTables := tables->codtable
               cNameTable := AllTrim( tables->nametable )
               STRUC->( dbSeek( AllTrim( Str(tables->codatset ) ) + AllTrim( Str(tables->codtable ) ) ) )
               aStructure := {}
            ELSE
               msgbox( "File does not exist, this module only modifies. It does not create new files" + cNameTable + ".dbf" )
               tables->( dbSkip() )
            ENDIF
         END
      ELSE
         msgbox( "There is no Structure for the Table" )
      ENDIF

   ELSE
      msgbox( "Table does not exist" )
   ENDIF

   dbCloseAll()

   msgbox( "Success" )

RETURN
