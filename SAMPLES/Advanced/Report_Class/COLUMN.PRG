/* Column class for report object.

    Created by Jon Credit
    All rights reserved
    I created this class in search of a way to pay more attention to
    the data that I am reporting on, vs how the data will be outputted on
    paper.  This report class is the result of many long hours of design,
    coding and testing to get it to its current state.

    This class is distributed as is with no expressed or implied
    warranties.  If you find the Report Class to be useful, a donation of
    $30.00 U.S. Dollars would be appreciated.  Any registered users will
    automatically be sent any bug fixes or upgrades to the class(es).

    I am also currently working on a front end to the Report Class to allow
    creation of reports from a txt file using a format similar to a windows
    ini file.  This utility will support most if not all of the methods and
    ivars in the class including goTop(), bWhile, bFind, cInitString,
    cResetString, and will also allow data to be from multiple database files
    with a bFind block evaled before printing the data.
    This utility will be sent to the first 30 people who register the report
    class at no charge, with complete source code.

    I hope you find this class to be as useful as I have.

    If you find any of the concepts dealt with in this report class to
    be of benifit please contribute $30.00 to the author so that he can
    justify all the long hours he puts in to his wife!!

    Thanks and Enjoy....

    Jon Credit
    50 B. Paisley Lane
    Columbia, S.C. 29210
    CIS 71371.1675


*/





#include "hbclass.ch"

#define CRLF    chr(13) + chr(10)

#xtranslate ifnil(<param>,<value>) =>  IIF( <param> == NIL, ;
                                        <value> , <param> )

#xtranslate @ <r>,<c> SAY <s> => gDevPos( <r>,<c> ) ; gDevOut( <s> )


create class RepColumn

export:

   var lChild
   VAR lWrap        TYPE Logical
   VAR lShowTotal   TYPE Logical
   VAR lMoreToPrint TYPE Logical

   var bToDo
   var bWhile
   VAR bBlock       TYPE Block
   VAR bFind        TYPE Block

   var aToBlank

   var cJustify
   var cChildAlias
   var cColumnTrim
   VAR cTitle       TYPE Character
   VAR cPicture     TYPE Character

   VAR nCol         TYPE Integer
   VAR nWidth       TYPE Integer
   VAR nTotal       TYPE Numeric


   var cDelimeter                             // {Indent}
   var cNoLineDelimeter

   method Init
   method show
   method delimit
   method childProcess
   method ShowTotal

   protected:

   var lOrigWrap
   var nPRow        TYPE Integer
   var cParentAlias
   var aOldToBlank
   var cToPrint


endclass


METHOD Init( cTitle, bBlock, lWrap, nWidth, cPicture )
   local uVar

   ::cTitle       := IfNil( cTitle, "" )
   ::bBlock       := IfNil( bBlock, {|| "" } )
   ::cPicture     := IfNil( cPicture, "")
   ::lWrap        := IfNil( lWrap, .F. )
   ::bFind        := {|| Nothing() }
   ::lMoreToPrint := .F.
   ::nCol         := 0
   ::nPRow        := 1
   ::lShowTotal   := .F.
   ::nTotal       := 0

   uVar := EVAL( bBlock )
   IF !( nWidth == NIL )
      ::nWidth := nWidth
   ELSE
      // Calculate the widths
      IF VALTYPE( uVar ) == "C"
         ::nWidth := MAX( LEN( ::cTitle ), LEN( uVar ) )
      ELSEIF VALTYPE( uVar ) == "D"
         ::nWidth := MAX( LEN( ::cTitle ), 8 )
      ELSEIF VALTYPE( uVar ) == "N"
         ::nWidth := MAX( LEN( ::cTitle ), LEN( STR( uVar ) ) )
      ELSEIF VALTYPE( uVar ) == "L"
         ::nWidth := MAX( LEN( ::cTitle ), 3 )
      ENDIF
   ENDIF

   ::cJustify := "L"
   ::lChild := .F.
   ::bWhile :={|| ( ::cParentAlias ) -> ( indexkey(0) ) == ( ::cChildAlias ) -> ( indexkey(0) ) }
   ::aToBlank := {}
   ::aOldToBlank := {}
   ::lOrigWrap := .T.
   ::cColumnTrim := "R"

   ::cDelimeter := ""
   ::cNoLineDelimeter := "~~"

return(SELF)


method show( oBj )

   if !::lMoreToPrint
      ::cToPrint := eval( ::bBlock )
      if ( valtype( ::cToPrint ) == "C" )
         do case
            case ::cColumnTrim == "R"
               ::cToPrint := rtrim( ::cToPrint )
            case ::cColumnTrim == "L"
               ::cToPrint := ltrim( ::cToPrint )
            case ::cColumnTrim == "A"
               ::cToPrint := alltrim( ::cToPrint )
          endcase

         if ::delimit( ::cToPrint )
            ::lOrigWrap := ::lWrap
            ::lWrap := .T.
         endif
      endif
   endif
   gDevPos( oBj:nRow, ::nCol )
   IF ::lWrap
       gDevOutPict( MEMOLINE( ::cToPrint, ::nWidth, ::nPRow++, ,.T.  ), ::cPicture )
       IF MLCOUNT( ::cToPrint, ::nWidth, , .T. ) >= ::nPRow
          ::lMoreToPrint := .T.
          oBj:lWrapping := .T.
       ELSE                      // IF IT MAKES IT HERE AND WRAP HAS
                                 // NOT BEEN CHANGED THEN IT WAS
                                 // TRUE TO BEGIN WITH!!!
          ::lWrap := ::lOrigWrap // SO lOrigWrap DEFAULTS TO TRUE!!!!!
          ::lMoreToPrint := .F.
          ::nPRow := 1
       ENDIF
   ELSE
       gDevOutPict( ::cToPrint, ::cPicture )
   ENDIF

RETURN(SELF)


method delimit( cString )
   local nAt

   while ( nAt := at( ::cDelimeter, ::cToPrint ) ) > 0
      ::cToPrint := substr( ::cToPrint, 1, nAt - 1 ) + CRLF + replicate("Ä", ::nWidth ) + substr( ::cToPrint, nAt + len( ::cDelimeter ) )
   end

   while ( nAt := at( ::cNoLineDelimeter, ::cToPrint ) ) > 0
      ::cToPrint := substr( ::cToPrint, 1, nAt - 1 ) + CRLF + substr( ::cToPrint, nAt + len( ::cNoLineDelimeter ) )
   end

   // Same thing here as in the report class ...
   // problems with the strtran function!

   // ::cToPrint := strtran( ::cToPrint, ::cDelimeter, CRLF+ replicate("Ä", ::nWidth ) )
   // ::cToPrint := strtran( ::cToPrint, ::cNoLineDelimeter, CRLF )

return( ::cToPrint == cString )


method childProcess( oBj )
   begin sequence
      if !oBj:lChild
         ::cParentAlias := alias()
         if eval( ::bToDo )
            if ::cChildAlias == NIL
               ::cChildAlias := ::cParentAlias()
            endif
            dbselectarea( ::cChildAlias )
            oBj:lChild := .T.
            ::aOldToBlank := acopy( oBJ:aToBlank, asize( ::aOldToBlank, len( oBj:aToBlank ) ) )
            oBj:aToBlank := acopy( ::aToBlank, asize( oBj:aToBlank, len( ::aToBlank ) ) )
         else                                    // Never changed areas.. reset Parent Alias
            break
         endif
      endif
      // Check if next record is a child of the parent database
      // if it is a child then stay in child alias and let report object
      // skip through the child alias!!!
      ( ::cChildAlias ) -> ( dbskip() )
      if !eval( ::bWhile )
         dbselectarea( ::cParentAlias )
         oBj:lChild := .F.
         acopy( ::aOldToBlank, asize( oBj:aToBlank, len( ::aOldToBlank ) ) )
         ::aOldToBlank := {}
      endif
      ( ::cChildAlias ) -> ( dbskip(-1) )
   end sequence
return(self)

method ShowTotal(oBj)

   if ::lShowTotal
       gDevPos( oBj:nRow, ::nCol )
       gDevOutPict( ::nTotal, ::cPicture )
   endif

return(self)




