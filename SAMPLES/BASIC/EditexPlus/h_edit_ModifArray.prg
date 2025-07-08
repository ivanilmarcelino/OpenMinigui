/*
Based on LSanyArray de: Bicahi Esgici < esgici <at> gmail >
*/

#include <hmg.ch>

PROCEDURE ModificarArray()
   nErrorCode := 0
   // _aEditConfig := ft_RestArr( Alias()+".array", @nErrorCode )
   _aEditConfig := ft_RestArr( cArrDescrip, @nErrorCode )
   IF nErrorCode != 0
      MsgInfo ( "Error restoring array" )
      RETURN
   ELSE
      _aEstructura := _aEditConfig[ 1 ]
      _aEtiqueta := _aEditConfig[ 2 ]
      _aControl := _aEditConfig[ 3 ]
      _aEditConfig := {} // don't need it any more
   ENDIF

   DEFINE WINDOW frmAdjust ;
         AT 0, 0 ;
         WIDTH 1100 ;
         HEIGHT 602 ;
         TITLE '    Cambiar manualmente la posicion de las etiquetas...                                     y de los controles' ;
         MODAL ;
         ON INIT Array2Grid( ThisWindow.NAME, 'grdLabels', 'grdCntrls' ) ;
         ON RELEASE Grid2File( ThisWindow.NAME, 'grdLabels', 'grdCntrls' ) ;

         ON KEY ESCAPE ACTION ThisWindow.RELEASE

      DEFINE GRID grdLabels
         ROW 10
         COL 10
         WIDTH 390
         HEIGHT 544
         HEADERS { 'Name', 'Row', 'Colum', 'With', 'Height', 'CN' }
         WIDTHS { 150, 50, 50, 50, 50, 35 }
         COLUMNCONTROLS { { 'TEXTBOX', 'CHARACTER' }, ;
            { 'TEXTBOX', 'NUMERIC', '999' }, ;
            { 'TEXTBOX', 'NUMERIC', '999' }, ;
            { 'TEXTBOX', 'NUMERIC', '999' }, ;
            { 'TEXTBOX', 'NUMERIC', '999' }, ;
            { 'TEXTBOX', 'NUMERIC', '9' }, }
         CELLNAVIGATION .T.
         ALLOWEDIT .T.
      END GRID // grdLabels

      DEFINE GRID grdCntrls
         ROW 10
         COL 400
         WIDTH 673
         HEIGHT 544
         HEADERS { 'Name', 'Row', 'Colum', 'With', 'Height', 'Instructions', 'Type', 'CN' }
         WIDTHS { 150, 50, 50, 50, 50, 220, 50, 45 }
         COLUMNCONTROLS { { 'TEXTBOX', 'CHARACTER' }, ;
            { 'TEXTBOX', 'NUMERIC', '999' }, ;
            { 'TEXTBOX', 'NUMERIC', '999' }, ;
            { 'TEXTBOX', 'NUMERIC', '999' }, ;
            { 'TEXTBOX', 'NUMERIC', '999' }, ;
            { 'TEXTBOX', 'CHARACTER' }, ;
            { 'TEXTBOX', 'NUMERIC', '999' }, ;
            { 'TEXTBOX', 'NUMERIC', '9' }, }
         CELLNAVIGATION .T.
         ALLOWEDIT .T.
      END GRID // grdCntrls

   END WINDOW
   CENTER WINDOW frmAdjust
   ACTIVATE WINDOW frmAdjust

RETURN // ModificarArray()


PROCEDURE Array2Grid( cFormName, cGridName1, cGridName2 )
   // Load a two dim array to a GRID

   DoMethod( cFormName, cGridName1, "DeleteAllItems" )
   AEval( _aEtiqueta, {| a1 | DoMethod( cFormName, cGridName1, "AddItem", a1 ) } )

   DoMethod( cFormName, cGridName2, "DeleteAllItems" )
   AEval( _aControl, {| a1 | DoMethod( cFormName, cGridName2, "AddItem", a1 ) } )

RETURN // Array2Grid()


PROCEDURE Grid2File( cFormName, cGridName1, cGridName2 )
   _aEtiqueta := Grid2Array( cFormName, cGridName1 )
   _aControl := Grid2Array( cFormName, cGridName2 )

   _aEditConfig := { _aEstructura, _aEtiqueta, _aControl }
   ft_SaveArr( _aEditConfig, cArrDescrip, nErrorCode )
   _aEditConfig := {} // don't need it any more

RETURN // Grid2File()


FUNCTION Grid2Array( cFormName, cGridName )

   LOCAL nGrdItmCo := GetProperty( cFormName, cGridName, "ItemCount" ), ;
      n1GrdRow // 1 row of grid

   LOCAL aRetVal := {}

   FOR n1GrdRow := 1 TO nGrdItmCo
      AAdd( aRetVal, _GetItem( cGridName, cFormName, n1GrdRow ) )
   NEXT nGrdRowNo

RETURN aRetVal // Grid2Array()
