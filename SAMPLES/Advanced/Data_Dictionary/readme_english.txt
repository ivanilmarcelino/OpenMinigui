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


Program to handle the necessary tables to make a system.
The program has the following structure, with the following tables.

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
Stores the data handled by the project. Each dataset is made up of a set of tables

Tables
Set of table for each dataset

Struc
Structure of each table

Index
Table indexes

The first time the program is executed, it creates the necessary structures for the system to work, the tables:

Dataset
 Tables
 Struc
 index.

For this the first Button is pressed

These tables will contain the tables that will be created in the project you are working on.
It also fills the tables with an example so you can see how it works, create and fill the following tables with data:

Cliente
Proveedor
Producto

hardware

The second button shows the structures of the tables that the system will create.

The third button is used to modify the structures according to the changes made in the second option.
This option does not create new tables, it only modifies its structure, increases fields, deletes fields or changes its type without losing data.

The program has the separate routines so that they can be implemented in any project.

The concept of dataset and the options to create and modify table structures is personal, take some ideas and guidelines from the library:

Author: Vern Six of FrontLine Software
This company released the source code of its entire library.

The tables have fields that are not used in this version of the program, they are designed for later versions.

Personally, it seems to me that these options should be in a Harbor RDD, such as DBFCDX or LetoDB.


Marcos Jarrin Pita
marvijarrin@gmail.com
badasystem.org

