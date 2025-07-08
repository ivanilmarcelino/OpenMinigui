/*

 BadaSystem
 Programa      : dic_dbstructure
 Modulo        : IDE to change dataset data
 Compilador    : MINIGUI - Harbour Win32 GUI
 Link          : BCC 32 bit
 Autor         : Marcos Jarrín
 email         : marvijarrin@gmail.com
 website       : badasystem.org
 Date          : 20/05/2022
 Update        : 04/06/2022

*/

#include "minigui.ch"

//
//
//

PROCEDURE IdeDicStructure()

   LOCAL bColor := {|| if ( Deleted(), RGB( 255, 0, 0 ), RGB( 255, 255, 255 ) ) }
   LOCAL nRow0, nCol0
   LOCAL nRow1, nRow2, nCol1

   IF ! File( "dataset.dbf" )
      msgbox( "The structures do not exist execute the first option" )
      RETURN
   ENDIF

   //
   nRow0 := 10
   ncol0 := 10

   //
   nRow1 := 405
   nRow2 := 440
   nCol1 := 10

   SET BROWSESYNC ON

   DEFINE WINDOW IdeModify ;
         AT 0, 0 ;
         WIDTH 1350 HEIGHT 600 ;
         TITLE 'DataSet' ;
         CHILD NOMAXIMIZE ;
         ON INIT ( OpenTables(), UpdateStart() ) ;
         ON RELEASE CloseTables()

      @ nRow0, nCol1 LABEL Label_1 VALUE 'DataSet'
      @ nRow0, nCol1 + 290 LABEL Label_2 VALUE 'Tables'
      @ nRow0, nCol1 + 690 LABEL Label_3 VALUE 'Structure'
      @ nRow0, nCol1 + 1140 LABEL Label_4 VALUE 'Index'

      DEFINE STATUSBAR
         STATUSITEM 'DataSet'
      END STATUSBAR


      @ 40, 10 BROWSE dataset ;
         WIDTH 210 ;
         HEIGHT 350 ;
         HEADERS { 'Codigo', 'Dataset' } ;
         WIDTHS { 60, 130 } ;
         WORKAREA dataset ;
         FIELDS { 'dataset->codataset', 'dataset->namedatset' } ;
         ON CHANGE UpdateDataset() ;
         DYNAMICBACKCOLOR { bColor, bColor } ;
         EDIT INPLACE ;
         READONLY { .T., .F. } LOCK

      DEFINE BROWSE tables
         ROW 40
         COL 240
         WIDTH 270
         HEIGHT 350
         HEADERS { 'Codaset', "CodTable", "Table" }
         WIDTHS { 60, 65, 180, 100 }
         WORKAREA tables
         FIELDS { 'tables->codatset', 'tables->codtable', 'tables->nametable' }
         ON CHANGE UpdateTables()
         JUSTIFY { BROWSE_JTFY_LEFT, BROWSE_JTFY_LEFT, BROWSE_JTFY_LEFT }
         ALLOWEDIT .T.
         INPLACEEDIT .T.
         READONLY { .T., .T., .F. }
         LOCK .T.
         PICTURE { NIL, NIL, NIL }
         DYNAMICBACKCOLOR { bColor, bColor, bColor }
      END BROWSE


      @ 40, 510 BROWSE STRUC ;
         WIDTH 475 ;
         HEIGHT 350 ;
         HEADERS { 'Codigo', 'Nombre', "Tipo", 'Size', "Decimal" } ;
         WIDTHS { 60, 185, 65, 80, 60 } ;
         WORKAREA STRUC ;
         FIELDS { 'struc->codtables', 'struc->namestruc', 'struc->tipo', 'struc->size', 'struc->decimal' } ;
         ON CHANGE UpdateIndex() ;
         EDIT INPLACE ;
         DYNAMICBACKCOLOR { bColor, bColor, bColor, bColor, bColor } ;
         READONLY { .T., .F., .F., .F., .F. } LOCK

      DEFINE BROWSE INDEX
         ROW 40
         COL 1000
         WIDTH 325
         HEIGHT 350
         HEADERS { 'Codigo', 'Index', 'Tag' }
         WIDTHS { 60, 110, 200 }
         WORKAREA INDEX
         FIELDS { 'index->codtablei', 'index->nameindex', 'index->tag' }
         JUSTIFY { BROWSE_JTFY_LEFT, BROWSE_JTFY_LEFT }
         ALLOWEDIT .T.
         INPLACEEDIT .T.
         READONLY { .T., .F. }
         DYNAMICBACKCOLOR { bColor, bColor, bColor }
         LOCK .T.
         PICTURE { NIL, NIL }
      END BROWSE


      // first button
      @ nRow1, nCol1 BUTTON Button_1 ;
         CAPTION 'Append record' ;
         WIDTH 140 ;
         ACTION Append_record( 1 ) ;
         TOOLTIP 'Append a new record'
      @ nRow2, nCol1 BUTTON Button_2 ;
         CAPTION 'Delete/Undelete' ;
         WIDTH 140 ;
         ACTION Delete_record( 1 ) ;
         TOOLTIP 'Delete / Recall the current record'

      // second button
      @ nRow1, nCol1 + 290 BUTTON Button_3 ;
         CAPTION 'Append record' ;
         WIDTH 140 ;
         ACTION Append_record( 2 ) ;
         TOOLTIP 'Append a new record'
      @ nRow2, nCol1 + 290 BUTTON Button_4 ;
         CAPTION 'Delete/Undelete' ;
         WIDTH 140 ;
         ACTION Delete_record( 2 ) ;
         TOOLTIP 'Delete / Recall the current record'


      // third button
      @ nRow1, nCol1 + 670 BUTTON Button_5 ;
         CAPTION 'Append record' ;
         WIDTH 140 ;
         ACTION Append_record( 3 ) ;
         TOOLTIP 'Append a new record'
      @ nRow2, nCol1 + 670 BUTTON Button_6 ;
         CAPTION 'Delete/Undelete' ;
         WIDTH 140 ;
         ACTION Delete_record( 3 ) ;
         TOOLTIP 'Delete / Recall the current record'

      // fourth button
      @ nRow1, nCol1 + 1090 BUTTON Button_7 ;
         CAPTION 'Append record' ;
         WIDTH 140 ;
         ACTION Append_record( 4 ) ;
         TOOLTIP 'Append a new record'
      @ nRow2, nCol1 + 1090 BUTTON Button_8 ;
         CAPTION 'Delete/Undelete' ;
         WIDTH 140 ;
         ACTION Delete_record( 4 ) ;
         TOOLTIP 'Delete / Recall the current record'

      // Pack
      @ 500, 450 BUTTON Button_9 ;
         CAPTION 'Pack' ;
         WIDTH 140 ;
         ACTION PackRecord() ;
         TOOLTIP 'Pack Table'

      // exit
      @ 500, 650 BUTTON Button_10 ;
         CAPTION 'Exit' ;
         WIDTH 140 ;
         ACTION IdeModify.Release() ;
         TOOLTIP 'Exit'

   END WINDOW

   CENTER WINDOW IdeModify
   ACTIVATE WINDOW IdeModify

RETURN

PROCEDURE OpenTables()

   LOCAL aTipo := {}

   // 1
   USE dataset Shared NEW
   SET ORDER TO TAG codataset

   // 2
   USE tables SHARED NEW
   SET ORDER TO TAG codatset

   // 3
   USE STRUC SHARED NEW
   SET ORDER TO TAG coddataset

   AAdd ( aTipo, { 'Character', 'C' } )
   AAdd ( aTipo, { 'Numeric', 'N' } )
   AAdd ( aTipo, { 'Logical', 'L' } )
   AAdd ( aTipo, { 'Date', 'D' } )
   AAdd ( aTipo, { 'Memo', 'M' } )


   IdeModify.struc.InputItems := { NIL, NIL, aTipo, NIL, Nil }
   IdeModify.struc.DisplayItems := { NIL, NIL, aTipo, NIL, Nil }


   // 4
   USE INDEX SHARED NEW
   SET ORDER TO TAG coddatasei

RETURN

//
//
//
PROCEDURE CloseTables()

   dbCloseAll()

RETURN

//
//
//
PROCEDURE OpenTablesExclusive()

   // 1
   USE dataset EXCLUSIVE NEW
   SET ORDER TO TAG codataset

   // 2
   USE tables EXCLUSIVE NEW
   SET ORDER TO TAG codatset

   // 3
   USE STRUC EXCLUSIVE NEW
   SET ORDER TO TAG coddataset

   // 4
   USE INDEX EXCLUSIVE NEW
   SET ORDER TO TAG codtablei

RETURN


//
//
//
PROCEDURE UpdateDataset()

   LOCAL nArea := Select()

   SELECT tables
   SET SCOPE TO dataset->codataset
   tables->( dbGoTop() )
   IdeModify.Tables.VALUE := RecNo()
   Select( nArea )

RETURN

//
//
//
PROCEDURE UpdateTables()

   LOCAL nArea := Select()

   SELECT STRUC
   SET SCOPE TO AllTrim( Str( tables->codatset ) ) + AllTrim( Str( tables->codtable ) )
   STRUC->( dbGoTop() )
   IdeModify.Struc.VALUE := RecNo()
   Select( nArea )

RETURN


//
//
//
PROCEDURE UpdateIndex()

   LOCAL nArea := Select()

   SELECT INDEX
   SET SCOPE TO AllTrim( Str( tables->codatset ) ) + AllTrim( Str( tables->codtable ) )
   INDEX->( dbGoTop() )
   IdeModify.index.VALUE := RecNo()
   Select( nArea )

RETURN


//
//
//
PROCEDURE UpdateStart()

   LOCAL nArea := Select()

   SELECT tables
   SET SCOPE TO dataset->codataset
   tables->( dbGoTop() )
   IdeModify.Tables.VALUE := RecNo()

   SELECT STRUC
   SET SCOPE TO AllTrim( Str( tables->codatset ) ) + AllTrim( Str( tables->codtable ) )
   STRUC->( dbGoTop() )
   IdeModify.Struc.VALUE := RecNo()

   SELECT INDEX
   SET SCOPE TO STRUC->codtables
   INDEX->( dbGoTop() )
   IdeModify.index.VALUE := RecNo()

   Select( nArea )

RETURN


//
//
//
PROCEDURE Append_Record( nArea )

   LOCAL i := GetControlIndex ( "dataset", "IdeModify" )
   LOCAL n, m, l
   LOCAL cAreaOld

   // test
   DO CASE
   CASE nArea == 1
      dataset->( dbGoBottom() )
      n := dataset->codataset

      dataset->( dbAppend() )
      dataset->codataset := n + 1
      dataset->servertype := "NONE"
      dataset->vesion := "1.0.0"
      dataset->datedatset := Date()
      dataset->timedatset := Time()
      dataset->statusdase := TRUE

      IdeModify.dataset.VALUE := dataset->( RecNo() )

      IdeModify.dataset.SetFocus


   CASE nArea == 2

      //
      cAreaOld := Alias( Select() )
      dbSelectArea( "tables" )
      SET ORDER TO TAG codtabla
      tables->( dbGoBottom() )
      n := tables->codtable
      SET ORDER TO TAG codatset
      dbSelectArea( cAreaOld )


      tables->( dbGoBottom() )
      m = dataset->codataset

      tables->( dbAppend() )
      tables->codtable := n + 1
      tables->codatset := m
      tables->INDEX := TRUE
      tables->rdd := "DBFCDX"
      tables->datetable := Date()
      tables->timetable := Time()
      tables->statustab := TRUE


      IdeModify.tables.VALUE := tables->( RecNo() )

      IdeModify.tables.SetFocus


   CASE nArea == 3

      //
      cAreaOld := Alias( Select() )
      dbSelectArea( "struc" )
      SET ORDER TO TAG codstruc
      STRUC->( dbGoBottom() )
      n := STRUC->codstruc
      SET ORDER TO TAG coddataset
      dbSelectArea( cAreaOld )


      STRUC->( dbGoBottom() )
      m := tables->codtable
      l := tables->codatset


      STRUC->( dbAppend() )
      STRUC->codstruc := n + 1
      STRUC->codtables := m
      STRUC->coddataset := l
      STRUC->indexstruc := TRUE
      STRUC->statustruc := TRUE

      IdeModify.struc.VALUE := STRUC->( RecNo() )

      IdeModify.struc.SetFocus

   OTHERWISE
      SET ORDER TO TAG codindex
      INDEX->( dbGoBottom() )
      n := INDEX->codindex
      SET ORDER TO TAG coddatasei


      INDEX->( dbGoBottom() )
      m := tables->codtable
      l := tables->codatset

      INDEX->( dbAppend() )
      INDEX->codindex := n + 1
      INDEX->codtablei := m
      INDEX->codataseti := l
      INDEX->statusind := TRUE

      IdeModify.index.VALUE := INDEX->( RecNo() )

      IdeModify.index.SetFocus

   ENDCASE

RETURN


//
//
//
PROCEDURE Delete_record( nArea )

   DO CASE
   CASE nArea == 1
      dataset->( dbRLock() )
      iif( dataset->( Deleted() ), dataset->( dbRecall() ), dataset->( dbDelete() ) )
      dataset->( dbUnlock() )

      IdeModify.dataset.Refresh
      IdeModify.dataset.SetFocus

   CASE nArea == 2
      tables->( dbRLock() )
      iif( tables->( Deleted() ), tables->( dbRecall() ), tables->( dbDelete() ) )
      tables->( dbUnlock() )

      IdeModify.tables.Refresh
      IdeModify.tables.SetFocus


   CASE nArea == 3
      STRUC->( dbRLock() )
      iif( STRUC->( Deleted() ), STRUC->( dbRecall() ), STRUC->( dbDelete() ) )
      STRUC->( dbUnlock() )

      IdeModify.struc.Refresh
      IdeModify.struc.SetFocus

   OTHERWISE
      INDEX->( dbRLock() )
      iif( INDEX->( Deleted() ), INDEX->( dbRecall() ), INDEX->( dbDelete() ) )
      INDEX->( dbUnlock() )

      IdeModify.index.Refresh
      IdeModify.index.SetFocus


   ENDCASE

RETURN

//
//
//
PROCEDURE PackRecord()

   dbCloseAll()
   OpenTablesExclusive()

   SELECT dataset
   PACK

   SELECT tables
   PACK


   SELECT STRUC
   PACK


   SELECT INDEX
   PACK

   dbCloseAll()
   OpenTables()
   dataset->( dbGoTop() )
   UpdateStart()

   IdeModify.dataset.Refresh
   IdeModify.tables.Refresh
   IdeModify.struc.Refresh
   IdeModify.index.Refresh

   IdeModify.dataset.SetFocus

RETURN
