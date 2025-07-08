Data  Dictionary 
Create a data dictionary for harbour

dataset
tables
struc
index

THIS PROGRAM IS DISTRIBUTED "AS IS".  NO WARRANTY OF ANY KIND IS
EXPRESSED   OR   IMPLIED.   YOU  USE  IT   AT   YOUR  OWN  RISK.
THE  AUTHOR  WILL  NOT  BE  LIABLE  FOR ANY SPECIAL, INCIDENTAL,
CONSEQUENTIAL, INDIRECT OR  SIMILAR DAMAGES  DUE TO LOSS OF DATA
OR ANY OTHER REASON.


Programa para manejar las tablas necesarias para realizar un sistema.
El programa tiene la siguiente estructura, con las siguientes tablas.

  //
  // Dataset dataset.dbf   aStructure01
  //
                     {"codataset", "N",  4,0},;   //Key primary
                     {"namedatset","C", 50,0},;
                     {"servertype","C", 25,0},;
                     {"password",  "C",500,0},;
                     {"pathbase",  "C",255,0},;
                     {"pathdefaul","C",255,0},;
                     {"pathtemp",  "C",255,0},;
                     {"vesion",    "C", 10,0},;
                     {"datedatset","D",  8,0},;
                     {"timedatset","C",  8,0},;
                     {"statusdase","L",  1,0},;
                     {"commendase","M", 10,0} }

   //
   // Table that make up the dataset. tables.dbf   aStructure02
   //
                     {"codtable",  "N",  4,0},;   //primary key
                     {"codatset",  "N",  4,0},;   //foreign key
                     {"nametable", "C", 50,0},;
                     {"alias",     "C", 50,0},;
                     {"index",     "L",  1,0},;   
                     {"rdd",       "C", 25,0},;
                     {"datetable", "D",  8,0},;
                     {"timetable", "C",  8,0},;
                     {"statustab", "L",  1,0},;
                     {"commentab", "M", 10,0} }

   //
   // structure of struc.dbf      aStructure03
   //
                  {"codstruc",  "N",  4,0},;        //primary key
                     {"coddataset","N",  4,0},;
                     {"codtables", "N",  4,0},;        //foreign key
                     {"namestruc", "C", 25,0},;
                     {"tipo",      "C",  1,0},;
                     {"size",      "N",  4,0},;
                     {"decimal",   "N",  2,0},;
                     {"indexstruc","L",  1,0},;
                     {"statustruc","L",  1,0},;
                     {"commentstu","M", 10,0} }
   //
   // structure of index.dbf   aStructure04
   //
                     {"codindex",  "N",  4,0},;          //primary key
                     {"codataseti","N",  4,0},;
                     {"codtablei", "N",  4,0},;          //foreign key
                     {"nameindex", "C", 25,0},;
                     {"tag",       "C", 50,0},;
                     {"tipoindex", "C", 10,0},;
                     {"expresion", "C", 50,0},;
                     {"statusind", "L",  1,0},;
                     {"comentind", "M", 10,0} }



dataset
Almacena los datosset que maneja el proyecto-. Cada dataset este compuesto por un conjunto de tablas.

Tables
Conjunto de tablas por cada dataset

Struc
Estructura de cada tabla

Index
Índices de las tablas

La primera vez que se ejecuta el programa crea las estructuras necesarias para que el sistema funcione, las tablas:

Dataset
 Tables
 Struc
 index.

Para esto se presiona el primer Botón

Estas tablas contendrán las tablas que se crearán en el proyecto que se está trabajando.
También llena las tablas con un ejemplo para que se vea como funciona, crea y llena de datos las siguientes tablas:

Cliente
Proveedor
Producto

hardware


El segundo botón muestra las estructuras de las tablas que el sistema creará.

El tercer botón sirve para modificar las estructuras según los cambios realizados en la segunda opción.
Esta opción no crea nuevas tablas solo modifica su estructura, aumenta campos, elimina campo o cambia de tipo sin perder datos.

El programa tiene las rutinas separadas para que puedan implementarse en cualquier proyecto.

El concepto de dataset y las opciones de crear y modificar las estructuras de las tablas es personal, tomé algunas ideas y pautas de la librería:

Autor: Vern Six de FrontLine Software
Esta empresa libero el código fuente de toda su librería.

Las tablas tienen campos que no se utilizan es esta versión del programa, están pensadas para posteriores versiones.

En lo personal me parece que estas opciones debería estar en una RDD de Harbour, como DBFCDX o LetoDB.

Marcos Jarrin Pita
marvijarrin@gmail.com
badasystem.org
