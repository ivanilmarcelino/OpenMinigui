/* Report class for printing reports to file or printer.

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
#include "ReportClass.ch"

create class Report
export:
   var bInitPrinter
   var bResetPrinter
   var bTitleFont
   var bNormalFont
   var bRepHeader
   var bRepFooter
   VAR bHeader        TYPE Block
   VAR bFooter        TYPE Block
   VAR bPageBreak     TYPE Block
   VAR bSkipBlock     TYPE Block
   VAR bGoTopBlock    TYPE Block
   VAr bGoBottomBlock TYPE Block
   VAR bFor           TYPE Block
   VAR bWhile         TYPE Block

   var lGrid
   var lChild
   var lUndTitles
   var lForm
   var lCenterReport
   var lChildOnBreak
   var lWrapping
   VAR lShowTitles    TYPE Logical
   VAR lShowingTitle

   var nStartCol
   var nMaxTitleLen
   var nLastRow
   VAR nPageNo        TYPE Integer
   VAR nRow           TYPE Integer
   VAR nColSpace      TYPE Integer
   VAR nSpace         TYPE Integer
   var nWidth         type Integer

   var cWorkArea
   var cBoxString
   var cTitleUnd
   VAR cInitString    TYPE Character
   VAR cResetString   TYPE Character

   var aToBlank
   var aBlank
   VAR aColInfo       TYPE Array    READONLY

   var oBar


   method Init
   method Exec
   method showTitles
   method showCols
   method printIt
   method setTitles
   method incRow
   method gridTop
   method gridBottom
   method gridSide
   method showRepHeader
   method showRepFooter

   METHOD addColumn
   METHOD delColumn
   METHOD goTop
   METHOD goBottom
   METHOD skip


PROTECTED:

   VAR nColumns TYPE Integer
   VAR bGoNext      TYPE Block
   VAR bGoPrev      TYPE Block


   METHOD showHeader
   METHOD showFooter
   METHOD tbPrev
   METHOD tbNext
   METHOD setCols
endclass


method Init( bHeader, bFooter, bPageBreak )


   ::bHeader        := IFNIL(bHeader, {|| Nothing() } )
   ::nRow     := 0
   ::bFooter  := IFNIL(bFooter, {|| Nothing() })
   ::bPageBreak := ifnil(bPageBreak, {|| ::nRow > ::nLastRow })
   ::cInitString := ""
   ::cResetString := ""
   ::aColInfo := {}
   ::nColumns := 0
   ::bSkipBlock := {|nToSkip|  ::Skip( nToSkip ) }
   ::bGoTopBlock := {|| DBGOTOP() }
   ::bGoBottomBlock := {|| DBGOBOTTOM() }
   ::bFor := {|| .T. }
   ::bWhile := {|| .NOT. EOF() }
   ::bGoNext := {|| ::TBNext() }
   ::bGoPrev := {|| ::TBPrev() }
   ::nPageNo := 1
   ::nColSpace  := 1
   ::lShowTitles := .T.
   ::oBar           := {|| .T. }

   ::bInitPrinter := {|| Nothing() }
   ::bResetPrinter := {|| Nothing() }
   ::nStartCol := 0
   ::cWorkArea := alias()
   ::bTitleFont := {|| Nothing() }
   ::bNormalFont := {|| Nothing() }
   ::lUndTitles := .F.
   ::cBoxString := "ÚÄÂ¿³ÙÁÀ"
   ::nMaxTitleLen := 0
   ::lChild := .F.
   ::aToBlank := {}
   ::cTitleUnd := "="
   ::aBlank := {}
   ::lForm := .F.
   ::lGrid := .F.
   ::nLastRow := 57
   ::lCenterReport := .F.
   ::lChildOnBreak := .F.
   ::lWrapping := .F.
   ::lShowingTitle := .F.
   ::bRepHeader := {|| nothing() }
   ::bRepFooter := {|| nothing() }
   ::nWidth         := 80
return(SELF)

method Exec()
   LOCAL x, nRepWidth := 0
   local cOldArea := alias()
   local nNewStartCol
   dbselectarea( ::cWorkArea )
   ::goTop()
   ::nRow := 0
   if ::lForm
      ::lGrid := .T.
   endif
   if ::lGrid
      if ::nStartCol == 0
         ::nStartCol := 1
      endif
      ::nColSpace := 1
   endif

   if ::lCenterReport
      for x := 1 to ::nColumns
        nRepWidth += ::aColInfo[x]:nWidth + ::nColSpace
      next
      // If nStartCol < 0 screen file output is all messed up!!!!
      nNewStartCol := int( ( ::nWidth - nRepWidth ) / 2 )
      ::nStartCol := iif(nNewStartCol >= 0, nNewStartCol , ::nStartCol )
   endif

 IF gInitPrinter(self)
   eval( ::bInitPrinter, self )
   //~@ 0,0 SAY ::cInitString
   ::showRepHeader()
   ::SetCols()
   ::showHeader()
   IF ::lShowTitles
      ::setTitles()
      ::showTitles()
   ENDIF
   WHILE EVAL(::bWhile, SELF )
      IF EVAL( ::bPageBreak, SELF )
         if ::lForm
            while ::nRow < ::nLastRow
               ::gridTop()
               for x := 1 to ::nColumns
                  ::gridSide(x)
               next
               ::gridSide(x)
               ::nRow++
               ::gridBottom()
            end
         endif
         if ::lChild
            ::lChildOnBreak := .T.
         endif
         ::showFooter()
         ::nPageNo++
         geject()
         ::showHeader()
         IIF(::lShowTitles, ::showTitles(), NIL )
      ENDIF
      ::printIt()
      ::lChildOnBreak := .F.
      IF ::skip(1) == 0                          // No skip occurred
         EXIT
      ENDIF
   END
   if ::lForm
      while ::nRow < ::nLastRow
         ::gridTop()
         for x := 1 to ::nColumns
            ::gridSide(x)
         next
         ::gridSide(x)
         ::nRow++
         ::gridBottom()
      end
   endif
   ::showFooter()
   ::showRepFooter()
   eval( ::bResetPrinter, self )
   //~@ 0,0 SAY ::cResetString
   gResetPrinter()
 ELSE
	MSGSTOP ('Init Printer Error!')
 ENDIF

   dbselectarea( cOldArea )
RETURN(SELF)

METHOD addColumn( oCol )
   AADD( ::aColInfo, oCol )
   ::nColumns++
RETURN(SELF)

METHOD delColumn( nCol )
   ADEL( ::aColInfo, nCol )
   ASIZE( ::aColInfo, --::nColumns )
RETURN(SELF)

METHOD showHeader()
   gshowHeader()
   EVAL( ::bHeader, SELF )
RETURN(SELF)

METHOD showFooter()
   EVAL( ::bFooter, SELF )
   gshowFooter()
RETURN(SELF)

METHOD printIt()
   LOCAL x

   acopy( ::aToBlank, asize( ::aBlank, len( ::aToBlank ) ) )
   ::gridTop()
   ::ShowCols()
   WHILE ::lWrapping
      ::lWrapping := .F.
      ::nRow++
      FOR x := 1 TO ::nColumns
         ::gridSide(x)
         IF ::aColInfo[x]:lMoreToPrint
            ::aColInfo[x]:show( self )
         ENDIF
      NEXT
      ::gridSide(x)
   END
   ::nRow++
   ::gridBottom()
   acopy( ::aToBlank, asize( ::aBlank, len( ::aToBlank ) ) )

RETURN(SELF)

METHOD ShowCols()
   LOCAL x
   FOR x := 1 TO ::nColumns
      ::gridSide(x)
      if ::aColInfo[x]:lChild
         ::aColInfo[x]:childProcess( self )
      endif
      EVAL(::aColInfo[x]:bFind)
      if ::lChildOnBreak .OR. ( ascan( ::aBlank, x ) == 0 )
         ::aColInfo[x]:show( self )
      endif
   NEXT
   ::gridSide(x)
RETURN(SELF)


METHOD setCols()
   LOCAL x, nPrevCol := ::nStartCol, nPrevWidth := 0
   FOR x := 1 TO ::nColumns
      ::aColInfo[x]:nCol := nPrevCol + nPrevWidth
      nPrevWidth := ::aColInfo[x]:nWidth + ::nColSpace
      nPrevCol := ::aColInfo[x]:nCol
   NEXT
RETURN(SELF)


method setTitles()
   local x, cTitle := "", nAt

   for x := 1 to ::nColumns
      cTitle := ""
      while ( nAt := at( ";", ::aColInfo[x]:cTitle ) ) > 0
         cTitle += left( ::aColInfo[x]:cTitle, nAt-1) + CRLF
         ::aColInfo[x]:cTitle := substr( ::aColInfo[x]:cTitle, nAt+1 )
      end
      cTitle += ::aColInfo[x]:cTitle
      ::aColInfo[x]:cTitle := cTitle
      ::nMaxTitleLen := max( ::nMaxTitleLen, mlcount( ::aColInfo[x]:cTitle ) )

      // This is commented out because of problems I was having with
      // Clipper strtran.  The problems were consistent!!
      // Feel free to uncomment these lines to replace the above mess.

      // ::aColInfo[x]:cTitle := strtran( ::aColInfo[x]:cTitle, ";", CRLF )
      // ::nMaxTitleLen := max( ::nMaxTitleLen, mlcount( ::aColInfo[x]:cTitle ) )

   next
return(self)

method showTitles()
   local nLen, nCnt, x
   local cTitle
   nLen := ::nMaxTitleLen

   ::lShowingTitle := .T.
   if ::lGrid
      ::gridTop()
   endif
   eval( ::bTitleFont,self )
   for x := 1 to ::nMaxTitleLen
      for nCnt := 1 to ::nColumns
         ::gridSide( nCnt )
         cTitle := ::aColInfo[nCnt]:cTitle
         if mlcount( cTitle ) >= nLen - x + 1
            gDevPos( ::nRow, ::aColInfo[nCnt]:nCol )
            if ::aColInfo[nCnt]:cJustify == "C"
               gDevOut( padc( alltrim(memoline( cTitle, , x + mlcount( cTitle ) - nLen )), ::aColInfo[nCnt]:nWidth ) )
            elseif ::aColInfo[nCnt]:cJustify == "R"
               gDevOut( padl( alltrim(memoline( cTitle, , x + mlcount( cTitle ) - nLen )),::aColInfo[nCnt]:nWidth ) )
            else
               gDevOut( alltrim( memoline( cTitle, , x + mlcount( cTitle ) - nLen ) ) )
            endif
         endif
      next
      ::gridSide( nCnt )
      eval( ::bNormalFont, self )
      ::incRow()
      eval( ::bTitleFont, self )
   next
   eval( ::bNormalFont,self )
   if ::lGrid
      ::gridBottom()
   else
      if ::lUndTitles
         FOR x := 1 TO ::nColumns
            gDevPos( ::nRow, ::aColInfo[x]:nCol )
            gDevOut( replicate( ::cTitleUnd, ::aColInfo[x]:nWidth ) )
         NEXT
         ::incRow()
      endif
   endif
   ::lShowingTitle := .F.
return(self)


// This method is for possible future use
// I thought I would need to use it for the grid but it
// turned out not to be necessary.
method incRow()
   ::nRow++
return(self)


method gridTop()
   local x

   if ::lGrid
      gDevPos( ::nRow, ::nStartCol - 1 )
      if ::lShowingTitles
         gDevOut( substr( ::cBoxString, 1, 1) )
      elseif ( ascan( ::aToBlank, 1 ) > 0 ) .and. !::lChildOnBreak
         gDevOut( substr( ::cBoxString, 5, 1) )
      else
         gDevOut( substr( ::cBoxString, 1, 1) )
      endif
      for x := 1 to ::nColumns - 1
         if ::lShowingTitles
            gDevOut( replicate( substr( ::cBoxString, 2, 1 ), ::aColInfo[x]:nWidth  ) )
            gDevOut( substr( ::cBoxString, 3, 1 ) )
         elseif ( ascan( ::aToBlank, x ) > 0 ) .and. !::lChildOnBreak
            gDevOut( replicate( " ", ::aColInfo[x]:nWidth ) )
            if ( ascan( ::aToBlank, x+1 ) > 0 )
               gDevOut( substr( ::cBoxString, 5, 1 ) )
            else
               gDevOut( substr( ::cBoxString, 1, 1 ) )
            endif
         else
            gDevOut( replicate( substr( ::cBoxString, 2, 1 ), ::aColInfo[x]:nWidth  ) )
            if ( ascan( ::aToBlank, x+1 ) > 0 ) .and. !::lChildOnBreak
               gDevOut( substr( ::cBoxString, 4, 1 ) )
            else
               gDevOut( substr( ::cBoxString, 3, 1 ) )
            endif
         endif
      next
      gDevOut( replicate( substr( ::cBoxString, 2, 1 ), atail(::aColInfo):nWidth ) )
      gDevOut( substr( ::cBoxString, 4, 1 ) )
      ::nRow++
   endif
return(self)


method gridSide(x)
   if ::lGrid
      if x > ::nColumns
         gDevPos( ::nRow, ::aColInfo[::nColumns]:nCol + ::aColInfo[::nColumns]:nWidth )
         gDevOut( substr( ::cBoxString, 5, 1 ) )
      else
         gDevPos( ::nRow, ::aColInfo[x]:nCol - 1)
         gDevOut( substr( ::cBoxString, 5, 1 ) )
      endif
   endif
return(self)


method gridBottom()
   local x

   if ::lGrid
      gDevPos( ::nRow, ::nStartCol - 1 )
      if ::lShowingTitles
         gDevOut( substr( ::cBoxString, 8, 1) )
      elseif ( ascan( ::aToBlank, 1 ) > 0 ) .and. !( ::nRow >= ::nlastRow ) // .and. !::lChildOnBreak .and. !( ::nRow >= ::nlastRow )
         gDevOut( substr( ::cBoxString, 5, 1) )
      else
         gDevOut( substr( ::cBoxString, 8, 1) )
      endif
      for x := 1 to ::nColumns - 1
         if ::lShowingTitles
            gDevOut( replicate( substr( ::cBoxString, 2, 1 ), ::aColInfo[x]:nWidth ) )
            gDevOut( substr( ::cBoxString, 7, 1 ) )
         elseif ( ascan( ::aToBlank, x ) > 0 ) .and. !( ::nRow >= ::nlastRow ) // .and. !::lChildOnBreak .and. !( ::nRow >= ::nlastRow )
            gDevOut( replicate( " ", ::aColInfo[x]:nWidth ) )
            if ( ascan( ::aToBlank, x+1 ) > 0 )
               gDevOut( substr( ::cBoxString, 5, 1 ) )
            else
               gDevOut( substr( ::cBoxString, 8, 1 ) )
            endif
         else
            gDevOut( replicate( substr( ::cBoxString, 2, 1 ), ::aColInfo[x]:nWidth ) )
            if ( ascan( ::aToBlank, x+1 ) > 0 ) .and. !::lChildOnBreak .and. !( ::nRow >= ::nlastRow )
               gDevOut( substr( ::cBoxString, 6, 1 ) )
            else
               gDevOut( substr( ::cBoxString, 7, 1 ) )
            endif
         endif
      next
      gDevOut( replicate( substr( ::cBoxString, 2, 1 ), atail(::aColInfo):nWidth ) )
      gDevOut( substr( ::cBoxString, 6, 1 ) )
   endif
return(self)

method showRepHeader()
//   gshowRepHeader()
   eval( ::bRepHeader, self )
return (Self)

method showRepFooter()
   eval( ::bRepFooter, self )
   //gshowRepFooter()
return (Self)

METHOD goTop()

   EVAL( ::bGoTopBlock )

   WHILE !EOF() .AND. EVAL( ::bWhile, SELF  ) .AND. !EVAL( ::bFor, SELF )
      DBSKIP()
   END

   IF EOF() .OR. !EVAL( ::bWhile, SELF )
      DBGOTO(0)
   ENDIF
RETURN(NIL)


METHOD goBottom()

   EVAL( ::bGoBottomBlock )

   WHILE !BOF() .AND. EVAL( ::bWhile, SELF ) .AND. !EVAL( ::bFor, SELF )
      DBSKIP(-1)
   END

   IF BOF() .OR. !EVAL( ::bWhile, SELF )
      DBGOTO(0)
   ENDIF
RETURN(NIL)


METHOD skip(n)
   LOCAL nSkipped := 0

   IF n == 0
      DBSKIP(0)
   ELSEIF n > 0
      WHILE nSkipped != n .AND. EVAL( ::bGoNext )
         nSkipped++
      END
   ELSE
      WHILE nSkipped != n .AND. EVAL( ::bGoPrev )
         nSkipped--
      END
   ENDIF

RETURN(nSkipped)



METHOD TBNext()
   LOCAL nOldRecno := RECNO()
   LOCAL lMoved := .T.

   IF EOF()
      lMoved := .F.
   ELSE
      DBSKIP()
      eval(::oBar)
      WHILE !EVAL( ::bFor, SELF ) .AND. EVAL( ::bWhile, SELF ) .AND. !EOF()
         DBSKIP()
         eval(::oBar)
      END
      IF !EVAL(::bWhile, SELF ) .OR. EOF()
         lMoved := .F.
         DBGOTO( noldRecno )
      ENDIF
   ENDIF
RETURN(lMoved)

METHOD TBPrev()
   LOCAL nOldRecno := RECNO()
   LOCAL lMoved := .T.

   IF EOF()
      EVAL( ::bGoLast )
   ELSE
      DBSKIP(-1)
      eval(::oBar)
   ENDIF

   WHILE !EVAL( ::bFor, SELF ) .AND. EVAL( ::bWhile, SELF ) .AND. !BOF()
      DBSKIP(-1)
      eval(::oBar)
   END
   IF !EVAL(::bWhile, SELF ) .OR. BOF()
      DBGOTO(nOldRecno)
      lMoved := .F.
   ENDIF

RETURN(lMoved)
