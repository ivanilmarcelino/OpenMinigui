#include "MiniGui.ch"
#include "TSBrowse.ch"

//#define CLR_HBROWN  nRGB( 205, 192, 176 )

Function SBrwTest()

   Local cTitle := "Customer List", ;
         bSetup := { |oBrw| SetMyBrowser( oBrw ) }

   DbSelectArea( "Employee" )

   SBrowse( "Employee", cTitle, bSetup )

Return Nil

//----------------------------------------------------------------------------//

Function SetMyBrowser( oBrw )

       oBrw:nHeightCell += 5
       oBrw:nHeightHead += 5
       oBrw:nClrFocuFore := CLR_BLACK
       oBrw:nClrFocuBack := COLOR_GRID

Return .T.

//----------------------------------------------------------------------------//

Function TBrwTest()

   Local oBrw, ;
         cForm := HMG_GetUniqueName( "wMain" )

   DbSelectArea( "Employee" )

   DEFINE WINDOW (cForm) TITLE "Customer List" CHILD NOSIZE

       oBrw := _TBrowse()

       AEval( oBrw:aColumns, {| oCol | oCol:lEdit := .T. } )

       ON KEY ESCAPE ACTION ( iif( oBrw:IsEdit, oBrw:SetFocus(), ThisWindow.Release ) )

   END WINDOW

   CENTER WINDOW (cForm)
   ACTIVATE WINDOW (cForm)

Return Nil
