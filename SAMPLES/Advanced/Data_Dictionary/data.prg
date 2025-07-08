/*
 BadaSystem
 Programa      : dic_dbstructure
 Modulo        : Fill the structures with data and the tables with data.
 Compilador    : MINIGUI - Harbour Win32 GUI
 Link          : BCC 32 bit
 Autor         : Marcos Jarrín
 email         : marvijarrin@gmail.com
 website       : badasystem.org
 Date          : 03/06/2022

*/
#include "dic_structure.ch"

PROCEDURE fillData()

   // Data of the structures of the tables to create
   // Dataset
   dbUseArea( NEWAREA, "DBFCDX", "dataset", "dataset", EXCLUSIVE )
   SET ORDER TO TAG NAME
   IF Eof()
      // 1 dataset
      dataset->( dbAppend() )
      dataset->codataset := 1
      dataset->namedatset := "acme"
      dataset->servertype := "NONE"
      dataset->vesion := "1.0.0"
      dataset->datedatset := Date()
      dataset->timedatset := Time()
      dataset->statusdase := TRUE

      // 2 dataset
      dataset->( dbAppend() )
      dataset->codataset := 2
      dataset->namedatset := "tools"
      dataset->servertype := "NONE"
      dataset->vesion := "1.0.0"
      dataset->datedatset := Date()
      dataset->timedatset := Time()
      dataset->statusdase := TRUE

   ENDIF

   // Tables
   dbUseArea( NEWAREA, , "tables", "tables", EXCLUSIVE )
   SET ORDER TO TAG codatset

   // Client
   IF Eof()

      // client acme
      tables->( dbAppend() )
      tables->codtable := 1
      tables->codatset := 1
      tables->nametable := "cliente"
      tables->ALIAS := "cliente"
      tables->INDEX := TRUE
      tables->rdd := "DBFCDX"
      tables->datetable := Date()
      tables->timetable := Time()
      tables->statustab := TRUE


      // supplier acme
      tables->( dbAppend() )
      tables->codtable := 2
      tables->codatset := 1
      tables->nametable := "proveedor"
      tables->ALIAS := "proveedor"
      tables->INDEX := TRUE
      tables->rdd := "DBFCDX"
      tables->datetable := Date()
      tables->timetable := Time()
      tables->statustab := TRUE


      // product acme
      tables->( dbAppend() )
      tables->codtable := 3
      tables->codatset := 1
      tables->nametable := "producto"
      tables->ALIAS := "producto"
      tables->INDEX := TRUE
      tables->rdd := "DBFCDX"
      tables->datetable := Date()
      tables->timetable := Time()
      tables->statustab := TRUE

      // ///////////////////////////////////////////////////////
      // hardware tools
      tables->( dbAppend() )
      tables->codtable := 3
      tables->codatset := 2
      tables->nametable := "hardware"
      tables->ALIAS := "hardware"
      tables->INDEX := TRUE
      tables->rdd := "DBFCDX"
      tables->datetable := Date()
      tables->timetable := Time()
      tables->statustab := TRUE

   ENDIF

   // structure
   dbUseArea( NEWAREA, , "struc", "struc", EXCLUSIVE, NOREADONLY )
   SET ORDER TO TAG codtables

   // Client
   IF Eof()
      // client acme
      STRUC->( dbAppend() )
      STRUC->codstruc := 1
      STRUC->coddataset := 1
      STRUC->codtables := 1
      STRUC->namestruc := "codigo_cli"
      STRUC->tipo := "N"
      STRUC->SIZE := 10
      STRUC->DECIMAL := 0
      STRUC->indexstruc := TRUE
      STRUC->statustruc := TRUE

      // client acme
      STRUC->( dbAppend() )
      STRUC->codstruc := 2
      STRUC->coddataset := 1
      STRUC->codtables := 1
      STRUC->namestruc := "nombre"
      STRUC->tipo := "C"
      STRUC->SIZE := 40
      STRUC->DECIMAL := 0
      STRUC->indexstruc := TRUE
      STRUC->statustruc := TRUE

      // client acme
      STRUC->( dbAppend() )
      STRUC->codstruc := 3
      STRUC->coddataset := 1
      STRUC->codtables := 1
      STRUC->namestruc := "cedula"
      STRUC->tipo := "C"
      STRUC->SIZE := 13
      STRUC->DECIMAL := 0
      STRUC->indexstruc := TRUE
      STRUC->statustruc := TRUE

      // client acme
      STRUC->( dbAppend() )
      STRUC->codstruc := 4
      STRUC->coddataset := 1
      STRUC->codtables := 1
      STRUC->namestruc := "fecha_nac"
      STRUC->tipo := "D"
      STRUC->SIZE := 8
      STRUC->DECIMAL := 0
      STRUC->indexstruc := TRUE
      STRUC->statustruc := TRUE

      // client acme
      STRUC->( dbAppend() )
      STRUC->codstruc := 5
      STRUC->coddataset := 1
      STRUC->codtables := 1
      STRUC->namestruc := "credito"
      STRUC->tipo := "N"
      STRUC->SIZE := 10
      STRUC->DECIMAL := 2
      STRUC->indexstruc := TRUE
      STRUC->statustruc := TRUE

      // supplier acme
      STRUC->( dbAppend() )
      STRUC->codstruc := 6
      STRUC->coddataset := 1
      STRUC->codtables := 2
      STRUC->namestruc := "codigo_prov"
      STRUC->tipo := "N"
      STRUC->SIZE := 10
      STRUC->DECIMAL := 0
      STRUC->indexstruc := TRUE
      STRUC->statustruc := TRUE

      // supplier acme
      STRUC->( dbAppend() )
      STRUC->codstruc := 7
      STRUC->coddataset := 1
      STRUC->codtables := 2
      STRUC->namestruc := "nombre"
      STRUC->tipo := "C"
      STRUC->SIZE := 40
      STRUC->DECIMAL := 0
      STRUC->indexstruc := TRUE
      STRUC->statustruc := TRUE

      // supplier acme
      STRUC->( dbAppend() )
      STRUC->codstruc := 8
      STRUC->coddataset := 1
      STRUC->codtables := 2
      STRUC->namestruc := "cedula"
      STRUC->tipo := "C"
      STRUC->SIZE := 13
      STRUC->DECIMAL := 0
      STRUC->indexstruc := TRUE
      STRUC->statustruc := TRUE

      // supplier acme
      STRUC->( dbAppend() )
      STRUC->codstruc := 9
      STRUC->coddataset := 1
      STRUC->codtables := 2
      STRUC->namestruc := "fecha_nac"
      STRUC->tipo := "D"
      STRUC->SIZE := 8
      STRUC->DECIMAL := 0
      STRUC->indexstruc := TRUE
      STRUC->statustruc := TRUE

      // supplier acme
      STRUC->( dbAppend() )
      STRUC->codstruc := 10
      STRUC->coddataset := 1
      STRUC->codtables := 2
      STRUC->namestruc := "credito"
      STRUC->tipo := "N"
      STRUC->SIZE := 10
      STRUC->DECIMAL := 2
      STRUC->indexstruc := TRUE
      STRUC->statustruc := TRUE


      // product acme
      STRUC->( dbAppend() )
      STRUC->codstruc := 11
      STRUC->coddataset := 1
      STRUC->codtables := 3
      STRUC->namestruc := "codigo_pro"
      STRUC->tipo := "N"
      STRUC->SIZE := 10
      STRUC->DECIMAL := 0
      STRUC->indexstruc := TRUE
      STRUC->statustruc := TRUE

      // product acme
      STRUC->( dbAppend() )
      STRUC->codstruc := 12
      STRUC->coddataset := 1
      STRUC->codtables := 3
      STRUC->namestruc := "producto"
      STRUC->tipo := "C"
      STRUC->SIZE := 50
      STRUC->DECIMAL := 0
      STRUC->indexstruc := TRUE
      STRUC->statustruc := TRUE

      // product acme
      STRUC->( dbAppend() )
      STRUC->codstruc := 13
      STRUC->coddataset := 1
      STRUC->codtables := 3
      STRUC->namestruc := "cantidad"
      STRUC->tipo := "N"
      STRUC->SIZE := 10
      STRUC->DECIMAL := 0
      STRUC->indexstruc := TRUE
      STRUC->statustruc := TRUE

      // product acme
      STRUC->( dbAppend() )
      STRUC->codstruc := 14
      STRUC->coddataset := 1
      STRUC->codtables := 3
      STRUC->namestruc := "costo"
      STRUC->tipo := "N"
      STRUC->SIZE := 12
      STRUC->DECIMAL := 2
      STRUC->indexstruc := TRUE
      STRUC->statustruc := TRUE

      // product acme
      STRUC->( dbAppend() )
      STRUC->codstruc := 15
      STRUC->coddataset := 1
      STRUC->codtables := 3
      STRUC->namestruc := "precio"
      STRUC->tipo := "N"
      STRUC->SIZE := 12
      STRUC->DECIMAL := 2
      STRUC->indexstruc := TRUE
      STRUC->statustruc := TRUE

      // //////////////////////////////////////////////////
      // hardware tools
      STRUC->( dbAppend() )
      STRUC->codstruc := 16
      STRUC->coddataset := 2
      STRUC->codtables := 3
      STRUC->namestruc := "codigo"
      STRUC->tipo := "N"
      STRUC->SIZE := 4
      STRUC->DECIMAL := 0
      STRUC->indexstruc := TRUE
      STRUC->statustruc := TRUE

   ENDIF

   // index
   dbUseArea( NEWAREA, , "index", "index", EXCLUSIVE )
   SET ORDER TO TAG codtablei

   IF Eof()
      // client acme
      INDEX->( dbAppend() )
      INDEX->codindex := 1
      INDEX->codataseti := 1
      INDEX->codtablei := 1
      INDEX->nameindex := "codigo_cli"
      INDEX->TAG := "codigo_cli"
      INDEX->statusind := TRUE

      // client acme
      INDEX->( dbAppend() )
      INDEX->codindex := 2
      INDEX->codataseti := 1
      INDEX->codtablei := 1
      INDEX->nameindex := "nombre"
      INDEX->TAG := "nombre"
      INDEX->statusind := TRUE


      // supplier acme
      INDEX->( dbAppend() )
      INDEX->codindex := 3
      INDEX->codataseti := 1
      INDEX->codtablei := 2
      INDEX->nameindex := "codigo_pro"
      INDEX->TAG := "codigo_pro"
      INDEX->statusind := TRUE

      // supplier acme
      INDEX->( dbAppend() )
      INDEX->codindex := 4
      INDEX->codataseti := 1
      INDEX->codtablei := 2
      INDEX->nameindex := "nombre"
      INDEX->TAG := "nombre"
      INDEX->statusind := TRUE


      // product acme
      INDEX->( dbAppend() )
      INDEX->codindex := 5
      INDEX->codataseti := 1
      INDEX->codtablei := 3
      INDEX->nameindex := "codigo_pro"
      INDEX->TAG := "codigo_pro"
      INDEX->statusind := TRUE

      // product acme
      INDEX->( dbAppend() )
      INDEX->codindex := 6
      INDEX->codataseti := 1
      INDEX->codtablei := 3
      INDEX->nameindex := "producto"
      INDEX->TAG := "producto"
      INDEX->statusind := TRUE

      // product acme
      INDEX->( dbAppend() )
      INDEX->codindex := 7
      INDEX->codataseti := 1
      INDEX->codtablei := 3
      INDEX->nameindex := "cantidad"
      INDEX->TAG := "cantidad"
      INDEX->statusind := TRUE

      // //////////////////////////////////////////////////////
      // hardware tools
      INDEX->( dbAppend() )
      INDEX->codindex := 8
      INDEX->codataseti := 2
      INDEX->codtablei := 3
      INDEX->nameindex := "codigo"
      INDEX->TAG := "codigo"
      INDEX->statusind := TRUE

   ENDIF

   dbCloseAll()

RETURN



// //////////////////
//
// Fill the three created tables with data
// Client
// supplier
// product
//
// ///////////////////
PROCEDURE llenarDatostablasCreadas()


   // client
   IF File( "cliente.dbf" )
      dbUseArea( NEWAREA, , "cliente" )
      SET ORDER TO TAG codigo_cli

      IF cliente->( Eof() )
         cliente->( dbAppend() )
         cliente->codigo_cli := 1
         cliente->nombre := "CONSUMIDOR FINAL"
         cliente->cedula := "1111111111111"
         cliente->fecha_nac := CToD( "1/10/2020" )
         cliente->credito := 12

         cliente->( dbAppend() )
         cliente->codigo_cli := 2
         cliente->nombre := "SR. JESUS LOOR"
         cliente->cedula := "2222222222222"
         cliente->fecha_nac := CToD( "1/10/2020" )
         cliente->credito := 14

         cliente->( dbAppend() )
         cliente->codigo_cli := 3
         cliente->nombre := "SR.SERGIO DOMINGO VELASQUEZ"
         cliente->cedula := "3333333333333"
         cliente->fecha_nac := CToD( "1/10/2020" )
         cliente->credito := 20

         cliente->( dbAppend() )
         cliente->codigo_cli := 4
         cliente->nombre := "SR. JOSE CEDEÑO"
         cliente->cedula := "4444444444444"
         cliente->fecha_nac := CToD( "1/10/2020" )
         cliente->credito := 36

         cliente->( dbAppend() )
         cliente->codigo_cli := 5
         cliente->nombre := "SR. IGNACIO SORNOZA"
         cliente->cedula := "4444444444444"
         cliente->fecha_nac := CToD( "1/10/2020" )
         cliente->credito := 725

         cliente->( dbAppend() )
         cliente->codigo_cli := 6
         cliente->nombre := "ING PATRICIO MORA"
         cliente->cedula := "5555555555555"
         cliente->fecha_nac := CToD( "1/10/2020" )
         cliente->credito := 745

         cliente->( dbAppend() )
         cliente->codigo_cli := 7
         cliente->nombre := "SR. ROBERT PONCE"
         cliente->cedula := "6666666666666"
         cliente->fecha_nac := CToD( "1/10/2020" )
         cliente->credito := 753

         cliente->( dbAppend() )
         cliente->codigo_cli := 8
         cliente->nombre := "SR. PEDRO VILLAPRADO"
         cliente->cedula := "7777777777777"
         cliente->fecha_nac := CToD( "1/10/2020" )
         cliente->credito := 951

         cliente->( dbAppend() )
         cliente->codigo_cli := 9
         cliente->nombre := "SR. ANTONIO GUILLEN"
         cliente->cedula := "8888888888888"
         cliente->fecha_nac := CToD( "1/10/2020" )
         cliente->credito := 856

         cliente->( dbAppend() )
         cliente->codigo_cli := 10
         cliente->nombre := "SR ERLINDO INTRIAGO"
         cliente->cedula := "9999999999999"
         cliente->fecha_nac := CToD( "1/10/2020" )
         cliente->credito := 485
      ENDIF

   ENDIF
   // supplier
   IF File( "proveedor.dbf" )
      dbUseArea( NEWAREA, , "proveedor" )
      SET ORDER TO TAG codigo_pro

      IF proveedor->( Eof() )
         proveedor->( dbAppend() )
         proveedor->codigo_pro := 1
         proveedor->nombre := "REPUESTOS QUIROZ"
         proveedor->cedula := "990917051001"
         proveedor->fecha_nac := CToD( "1/1/2020" )
         proveedor->credito := 145

         proveedor->( dbAppend() )
         proveedor->codigo_pro := 2
         proveedor->nombre := "SOLO DIESEL"
         proveedor->cedula := "454542121447"
         proveedor->fecha_nac := CToD( "1/1/2020" )
         proveedor->credito := 145

         proveedor->( dbAppend() )
         proveedor->codigo_pro := 3
         proveedor->nombre := "VICTOR HIANG"
         proveedor->cedula := "132154787888"
         proveedor->fecha_nac := CToD( "1/1/2020" )
         proveedor->credito := 145

         proveedor->( dbAppend() )
         proveedor->codigo_pro := 4
         proveedor->nombre := "INEXTROSA"
         proveedor->cedula := "813214667551"
         proveedor->fecha_nac := CToD( "1/1/2020" )
         proveedor->credito := 145

         proveedor->( dbAppend() )
         proveedor->codigo_pro := 5
         proveedor->nombre := "MOTRANSA"
         proveedor->cedula := "456878788881"
         proveedor->fecha_nac := CToD( "1/1/2020" )
         proveedor->credito := 145

         proveedor->( dbAppend() )
         proveedor->codigo_pro := 6
         proveedor->nombre := "LA CASA DE LA TOBERA"
         proveedor->cedula := "787564545411"
         proveedor->fecha_nac := CToD( "1/1/2020" )
         proveedor->credito := 145

         proveedor->( dbAppend() )
         proveedor->codigo_pro := 7
         proveedor->nombre := "KIMSA"
         proveedor->cedula := "125454545451"
         proveedor->fecha_nac := CToD( "1/1/2020" )
         proveedor->credito := 145

         proveedor->( dbAppend() )
         proveedor->codigo_pro := 8
         proveedor->nombre := "VICTOR H. ARIAS"
         proveedor->cedula := "945132154551"
         proveedor->fecha_nac := CToD( "1/1/2020" )
         proveedor->credito := 145

         proveedor->( dbAppend() )
         proveedor->codigo_pro := 9
         proveedor->nombre := "TRACTOPARTES"
         proveedor->cedula := "121441551001"
         proveedor->fecha_nac := CToD( "1/1/2020" )
         proveedor->credito := 145

         proveedor->( dbAppend() )
         proveedor->codigo_pro := 10
         proveedor->nombre := "MANSUERA"
         proveedor->cedula := "778777454544"
         proveedor->fecha_nac := CToD( "1/1/2020" )
         proveedor->credito := 145
      ENDIF
   ENDIF

   // product
   IF File( "producto.dbf" )
      dbUseArea( NEWAREA, , "producto" )
      SET ORDER TO TAG codigo_pro

      IF producto->( Eof() )
         producto->( dbAppend() )
         producto->codigo_pro := 1
         producto->producto := "RESORTE DïRETORNO BOMBA INY.CK20-TK20 N"
         producto->cantidad := 146
         producto->costo := 488
         producto->precio := 654

         producto->( dbAppend() )
         producto->codigo_pro := 2
         producto->producto := "RULIMAN EXT. DE RUEDA DELANT CWA51/TK/CK20."
         producto->cantidad := 185
         producto->costo := 488
         producto->precio := 888

         producto->( dbAppend() )
         producto->codigo_pro := 3
         producto->producto := "RULIMAN RUEDA POSTERIOR CMB86/HINO FF1JP"
         producto->cantidad := 147
         producto->costo := 457
         producto->precio := 758

         producto->( dbAppend() )
         producto->codigo_pro := 4
         producto->producto := "ESPARRAGOS TK20/CK20/CWA51/CWA52/CWA53N"
         producto->cantidad := 146
         producto->costo := 466
         producto->precio := 658

         producto->( dbAppend() )
         producto->codigo_pro := 5
         producto->producto := "TUERCA CORONA N"
         producto->cantidad := 545
         producto->costo := 458
         producto->precio := 655

         producto->( dbAppend() )
         producto->codigo_pro := 6
         producto->producto := "TUERCA ABRAZ.DELANTERA N"
         producto->cantidad := 445
         producto->costo := 448
         producto->precio := 654

         producto->( dbAppend() )
         producto->codigo_pro := 7
         producto->producto := "TUERCA PAQ. POSTERIOR CK20 N"
         producto->cantidad := 143
         producto->costo := 438
         producto->precio := 358

         producto->( dbAppend() )
         producto->codigo_pro := 8
         producto->producto := "TUERCA DïESPAR.DïRUEDACWA51-52/TK20/CK20 N"
         producto->cantidad := 125
         producto->costo := 452
         producto->precio := 258

         producto->( dbAppend() )
         producto->codigo_pro := 9
         producto->producto := "ANILLOS DE BRONCE N"
         producto->cantidad := 115
         producto->costo := 411
         producto->precio := 628

         producto->( dbAppend() )
         producto->codigo_pro := 10
         producto->producto := "EMPAQUE N"
         producto->cantidad := 145
         producto->costo := 458
         producto->precio := 658

         producto->( dbAppend() )
         producto->codigo_pro := 11
         producto->producto := "PERNO DE CA¥ERIA COMBUSTIBLE PD6 N"
         producto->cantidad := 115
         producto->costo := 438
         producto->precio := 648

         producto->( dbAppend() )
         producto->codigo_pro := 12
         producto->producto := "PERNO DE CA¥ERIA N"
         producto->cantidad := 135
         producto->costo := 453
         producto->precio := 638

         producto->( dbAppend() )
         producto->codigo_pro := 13
         producto->producto := "PERNO VALVULA N"
         producto->cantidad := 135
         producto->costo := 438
         producto->precio := 338

         producto->( dbAppend() )
         producto->codigo_pro := 14
         producto->producto := "PERNO CONECTOR COMPRESOR CWA51 N"
         producto->cantidad := 245
         producto->costo := 348
         producto->precio := 458

         producto->( dbAppend() )
         producto->codigo_pro := 15
         producto->producto := "BANDA VENTILADOR N  PD6"
         producto->cantidad := 115
         producto->costo := 451
         producto->precio := 618

         producto->( dbAppend() )
         producto->codigo_pro := 16
         producto->producto := "PERNO BASE POST. N"
         producto->cantidad := 125
         producto->costo := 438
         producto->precio := 653

         producto->( dbAppend() )
         producto->codigo_pro := 17
         producto->producto := "EMPAQT.COMPLETA FD35 N"
         producto->cantidad := 155
         producto->costo := 468
         producto->precio := 656

         producto->( dbAppend() )
         producto->codigo_pro := 18
         producto->producto := "O-RING ENFR. ACEITE PEQ. RD8 N"
         producto->cantidad := 145
         producto->costo := 444
         producto->precio := 644

         producto->( dbAppend() )
         producto->codigo_pro := 19
         producto->producto := "TUBO ACEITE CABEZOTE FINO FD6 N"
         producto->cantidad := 135
         producto->costo := 428
         producto->precio := 118


         producto->( dbAppend() )
         producto->codigo_pro := 20
         producto->producto := "BUJIAS INCANDECENTES FD35 N"
         producto->cantidad := 754
         producto->costo := 456
         producto->precio := 852
      ENDIF
   ENDIF

   dbCloseAll()

   msgbox( "Success" )

RETURN
