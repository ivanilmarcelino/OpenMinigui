#include <hmg.ch>

// -----------
FUNCTION MAIN
// -----------
   LOCAL aMainHeaders
   LOCAL aMainFields
   LOCAL aMainJustify
   LOCAL aMainWidths

   /*
    * Init RDD DBFCDX
   */

   REQUEST DBFCDX
   rddSetDefault( "DBFCDX" )

   /*
    * Set global
   */

   SET MULTIPLE OFF WARNING
   SET NAVIGATION EXTENDED
   SET DELETED ON

   USE NOMES NEW
   INDEX ON FIELD->NOME TAG Nome TO NOMES.CDX
   IF ! File( "QUERY.DBF" )
      COPY TO QUERY
   ENDIF
   CLOSE NOMES
   USE QUERY NEW
   ZAP

   aMainHeaders := { 'Codigo', 'Nombre' }
   aMainFields := { 'Codigo', 'Nome' }
   aMainJustify := { 1, 0 }
   aMainWidths := { 105, 390 }

   LOAD WINDOW MAIN
   MAIN.Center()
   MAIN.Activate()

RETURN NIL

// ---------------------
STATIC PROCEDURE Buscar
// ---------------------
   LOCAL cBusca := Upper( AllTrim( MAIN.Text_1.Value ) )
   LOCAL cAlias

   SELECT QUERY
   ZAP

   USE NOMES NEW
   cAlias := Alias()
   ( cAlias )->( dbSetOrder( 1 ) )
   ( cAlias )->( dbGoTop() )

   IF ! Empty( cBusca )

      IF !( Left( cBusca, 1 ) == "*" .OR. "*" $ cBusca .OR. "?" $ cBusca )
         cBusca := "*" + cBusca + "*"
      ENDIF

      IF ( cAlias )->( ordWildSeek( cBusca ) ) // first time call

         DbfCopyRec( "QUERY", .T. ) // append record with second param .T.

         DO WHILE ( cAlias )->( ordWildSeek( cBusca, .T. ) ) // repeated call with second param .T.
            DbfCopyRec( "QUERY", .T. )
         ENDDO

      ENDIF

   ENDIF

   CLOSE NOMES

   SELECT QUERY
   dbGoTop()
   MAIN.Grid_1.Value := QUERY->( RecNo() )
   MAIN.Grid_1.Refresh()
   MAIN.Grid_1.Setfocus()

RETURN
