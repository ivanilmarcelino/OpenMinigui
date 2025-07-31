/*----------------------------------------------------------------------------
 MINIGUI - Harbour Win32 GUI library source code

 Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
 http://harbourminigui.googlepages.com/

 This program is free software; you can redistribute it and/or modify it under
 the terms of the GNU General Public License as published by the Free Software
 Foundation; either version 2 of the License, or (at your option) any later
 version.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

 You should have received a copy of the GNU General Public License along with
 this software; see the file COPYING. If not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA (or
 visit the web site http://www.gnu.org/).

 As a special exception, you have permission for additional uses of the text
 contained in this release of Harbour Minigui.

 The exception is that, if you link the Harbour Minigui library with other
 files to produce an executable, this does not by itself cause the resulting
 executable to be covered by the GNU General Public License.
 Your use of that executable is in no way restricted on account of linking the
 Harbour-Minigui library code into it.

 Parts of this project are based upon:

 "Harbour GUI framework for Win32"
  Copyright 2001 Alexander S.Kresin <alex@kresin.ru>
  Copyright 2001 Antonio Linares <alinares@fivetech.com>
 www - https://harbour.github.io/

 "Harbour Project"
 Copyright 1999-2025, https://harbour.github.io/

 "WHAT32"
 Copyright 2002 AJ Wos <andrwos@aust1.net>

 "HWGUI"
   Copyright 2001-2021 Alexander S.Kresin <alex@kresin.ru>

---------------------------------------------------------------------------*/

#ifdef __XHARBOUR__
   #define _BT_
#endif

#include "hmg.ch"

#ifdef _HMG_COMPAT_

#include "fileio.ch"
#xtranslate Alltrim( Str( <i> ) ) => hb_NtoS( <i> )

/*
 * PROCEDURE _DefineReport( cName )
 *
 * Defines a report structure in memory, initializing its properties.
 *
 * Parameters:
 *   cName (STRING): The name of the report to define. This name will be used as a public variable.
 *
 * Return Value:
 *   None. This procedure creates a public variable with the name provided in cName, which is an array containing report data.
 *
 * Purpose:
 *   This procedure is used to create a report definition in memory before defining its layout, header, detail, footer, and other sections.
 *   It initializes various elements of the _HMG_RPTDATA array, which is used internally by HMG to store report properties.
 *   The report name is stored in _HMG_RPTDATA[162] and used later when the report is finalized with _EndReport.
 *   The public variable created will hold the final report definition.
 *
 * Notes:
 *   The procedure uses a global array _HMG_RPTDATA to store report properties.
 *   The report definition is not complete until _EndReport is called.
 *   If cName is "_TEMPLATE_", the procedure uses the existing report name stored in _HMG_RPTDATA[162].
 */
PROCEDURE _DefineReport ( cName )

   _HMG_RPTDATA[ 118 ] := 0
   _HMG_RPTDATA[ 119 ] := 0

   _HMG_RPTDATA[ 120 ] := 0

   _HMG_RPTDATA[ 121 ] := {}
   _HMG_RPTDATA[ 122 ] := {}

   _HMG_RPTDATA[ 123 ] := 0
   _HMG_RPTDATA[ 124 ] := 0

   _HMG_RPTDATA[ 155 ] := 0
   _HMG_RPTDATA[ 156 ] := 0

   _HMG_RPTDATA[ 157 ] := {}
   _HMG_RPTDATA[ 158 ] := {}
   _HMG_RPTDATA[ 159 ] := {}
   _HMG_RPTDATA[ 160 ] := {}
   _HMG_RPTDATA[ 126 ] := {}
   _HMG_RPTDATA[ 127 ] := 0
   _HMG_RPTDATA[ 161 ] := 'MAIN'

   _HMG_RPTDATA[ 164 ] := Nil
   _HMG_RPTDATA[ 165 ] := Nil

   IF cName <> '_TEMPLATE_'

      _HMG_RPTDATA[ 162 ] := cName

   ELSE

      cName := _HMG_RPTDATA[ 162 ]

   ENDIF

   Public &cName := {}

RETURN

/*
 * PROCEDURE _EndReport()
 *
 * Finalizes the report definition and assigns it to a public variable.
 *
 * Parameters:
 *   None.
 *
 * Return Value:
 *   None. This procedure assigns the report definition to a public variable.
 *
 * Purpose:
 *   This procedure is called after defining all sections of a report (layout, header, detail, footer, etc.).
 *   It gathers all the data stored in the _HMG_RPTDATA array and creates a structured array representing the complete report definition.
 *   This array is then assigned to a public variable with the name previously defined in _DefineReport.
 *   This allows the report to be easily accessed and executed later using ExecuteReport.
 *
 * Notes:
 *   The procedure relies on the _HMG_RPTDATA array being populated with the correct report properties.
 *   The public variable created will hold the final report definition.
 */
PROCEDURE _EndReport

   LOCAL cReportName
   LOCAL aMiscdata

   aMiscData := {}

   AAdd ( aMiscData, _HMG_RPTDATA[ 120 ] ) // nGroupCount
   AAdd ( aMiscData, _HMG_RPTDATA[ 152 ] ) // nHeadeHeight
   AAdd ( aMiscData, _HMG_RPTDATA[ 153 ] ) // nDetailHeight
   AAdd ( aMiscData, _HMG_RPTDATA[ 154 ] ) // nFooterHeight
   AAdd ( aMiscData, _HMG_RPTDATA[ 127 ] ) // nSummaryHeight
   AAdd ( aMiscData, _HMG_RPTDATA[ 124 ] ) // nGroupHeaderHeight
   AAdd ( aMiscData, _HMG_RPTDATA[ 123 ] ) // nGroupFooterHeight
   AAdd ( aMiscData, _HMG_RPTDATA[ 125 ] ) // xGroupExpression
   AAdd ( aMiscData, _HMG_RPTDATA[ 164 ] ) // xSkipProcedure
   AAdd ( aMiscData, _HMG_RPTDATA[ 165 ] ) // xEOF

   cReportName := _HMG_RPTDATA[ 162 ]

   &cReportName := { _HMG_RPTDATA[ 159 ], _HMG_RPTDATA[ 160 ], _HMG_RPTDATA[ 158 ], _HMG_RPTDATA[ 157 ], _HMG_RPTDATA[ 126 ], _HMG_RPTDATA[ 121 ], _HMG_RPTDATA[ 122 ], aMiscData }

RETURN

/*
 * PROCEDURE _BeginLayout()
 *
 * Marks the beginning of the layout definition section of a report.
 *
 * Purpose:
 *   This procedure sets the internal state of the report definition process to indicate that the following code defines the report's layout.
 *   The layout typically includes settings such as paper size, orientation, and margins.
 *   It sets the _HMG_RPTDATA[161] flag to 'LAYOUT', which is used by other procedures to determine where to store the defined properties.
 *
 * Notes:
 *   This procedure must be called before defining any layout properties.
 *   It is paired with the _EndLayout procedure.
 */
PROCEDURE _BeginLayout

   _HMG_RPTDATA[ 161 ] := 'LAYOUT'

RETURN

/*
 * PROCEDURE _EndLayout()
 *
 * Marks the end of the layout definition section of a report.
 *
 * Purpose:
 *   This procedure is called after defining all layout properties of a report.
 *   It gathers the layout information stored in various _HMG_RPTDATA elements and adds them to the _HMG_RPTDATA[159] array.
 *   This array will eventually be part of the complete report definition.
 *
 * Notes:
 *   This procedure must be called after defining all layout properties.
 *   It is paired with the _BeginLayout procedure.
 */
PROCEDURE _EndLayout

   AAdd ( _HMG_RPTDATA[ 159 ], _HMG_RPTDATA[ 155 ] )
   AAdd ( _HMG_RPTDATA[ 159 ], _HMG_RPTDATA[ 156 ] )
   AAdd ( _HMG_RPTDATA[ 159 ], _HMG_RPTDATA[ 118 ] )
   AAdd ( _HMG_RPTDATA[ 159 ], _HMG_RPTDATA[ 119 ] )

RETURN

/*
 * PROCEDURE _BeginHeader()
 *
 * Marks the beginning of the header definition section of a report.
 *
 * Purpose:
 *   This procedure sets the internal state of the report definition process to indicate that the following code defines the report's header.
 *   The header typically includes elements that appear at the top of each page, such as titles, logos, and page numbers.
 *   It sets the _HMG_RPTDATA[161] flag to 'HEADER' and initializes the _HMG_RPTDATA[160] array, which will store the header elements.
 *
 * Notes:
 *   This procedure must be called before defining any header elements.
 *   It is paired with the _EndHeader procedure.
 */
PROCEDURE _BeginHeader

   _HMG_RPTDATA[ 161 ] := 'HEADER'

   _HMG_RPTDATA[ 160 ] := {}

RETURN

/*
 * PROCEDURE _EndHeader()
 *
 * Marks the end of the header definition section of a report.
 *
 * Purpose:
 *   This procedure is called after defining all header elements of a report.
 *   Currently, it doesn't perform any specific actions but serves as a marker for the end of the header definition.
 *   The header elements are already stored in the _HMG_RPTDATA[160] array.
 *
 * Notes:
 *   This procedure must be called after defining all header elements.
 *   It is paired with the _BeginHeader procedure.
 */
PROCEDURE _EndHeader

RETURN


/*
 * PROCEDURE _BeginDetail()
 *
 * Marks the beginning of the detail definition section of a report.
 *
 * Purpose:
 *   This procedure sets the internal state of the report definition process to indicate that the following code defines the report's detail section.
 *   The detail section typically includes the main data rows of the report.
 *   It sets the _HMG_RPTDATA[161] flag to 'DETAIL' and initializes the _HMG_RPTDATA[158] array, which will store the detail elements.
 *
 * Notes:
 *   This procedure must be called before defining any detail elements.
 *   It is paired with the _EndDetail procedure.
 */
PROCEDURE _BeginDetail

   _HMG_RPTDATA[ 161 ] := 'DETAIL'

   _HMG_RPTDATA[ 158 ] := {}

RETURN

/*
 * PROCEDURE _EndDetail()
 *
 * Marks the end of the detail definition section of a report.
 *
 * Purpose:
 *   This procedure is called after defining all detail elements of a report.
 *   Currently, it doesn't perform any specific actions but serves as a marker for the end of the detail definition.
 *   The detail elements are already stored in the _HMG_RPTDATA[158] array.
 *
 * Notes:
 *   This procedure must be called after defining all detail elements.
 *   It is paired with the _BeginDetail procedure.
 */
PROCEDURE _EndDetail

RETURN

/*
 * PROCEDURE _BeginFooter()
 *
 * Marks the beginning of the footer definition section of a report.
 *
 * Purpose:
 *   This procedure sets the internal state of the report definition process to indicate that the following code defines the report's footer.
 *   The footer typically includes elements that appear at the bottom of each page, such as page numbers, dates, and copyright notices.
 *   It sets the _HMG_RPTDATA[161] flag to 'FOOTER' and initializes the _HMG_RPTDATA[157] array, which will store the footer elements.
 *
 * Notes:
 *   This procedure must be called before defining any footer elements.
 *   It is paired with the _EndFooter procedure.
 */
PROCEDURE _BeginFooter

   _HMG_RPTDATA[ 161 ] := 'FOOTER'

   _HMG_RPTDATA[ 157 ] := {}

RETURN

/*
 * PROCEDURE _EndFooter()
 *
 * Marks the end of the footer definition section of a report.
 *
 * Purpose:
 *   This procedure is called after defining all footer elements of a report.
 *   Currently, it doesn't perform any specific actions but serves as a marker for the end of the footer definition.
 *   The footer elements are already stored in the _HMG_RPTDATA[157] array.
 *
 * Notes:
 *   This procedure must be called after defining all footer elements.
 *   It is paired with the _BeginFooter procedure.
 */
PROCEDURE _EndFooter

RETURN

/*
 * PROCEDURE _BeginSummary()
 *
 * Marks the beginning of the summary definition section of a report.
 *
 * Purpose:
 *   This procedure sets the internal state of the report definition process to indicate that the following code defines the report's summary section.
 *   The summary section typically includes totals, averages, and other calculated values that appear at the end of the report.
 *   It sets the _HMG_RPTDATA[161] flag to 'SUMMARY'.
 *
 * Notes:
 *   This procedure must be called before defining any summary elements.
 *   It is paired with the _EndSummary procedure.
 */
PROCEDURE _BeginSummary

   _HMG_RPTDATA[ 161 ] := 'SUMMARY'

RETURN

/*
 * PROCEDURE _EndSummary()
 *
 * Marks the end of the summary definition section of a report.
 *
 * Purpose:
 *   This procedure is called after defining all summary elements of a report.
 *   Currently, it doesn't perform any specific actions but serves as a marker for the end of the summary definition.
 *
 * Notes:
 *   This procedure must be called after defining all summary elements.
 *   It is paired with the _BeginSummary procedure.
 */
PROCEDURE _EndSummary


RETURN


/*
 * PROCEDURE _BeginText()
 *
 * Initializes the properties for a text object within a report band.
 *
 * Purpose:
 *   This procedure is called before defining the properties of a text object that will be placed in a report band (header, detail, footer, etc.).
 *   It resets the global variables that store the text object's properties, such as text content, position, size, font, and alignment.
 *   These properties will be set by subsequent calls to other procedures and then used by _EndText to create the text object.
 *
 * Notes:
 *   This procedure must be called before setting any text object properties.
 *   It is paired with the _EndText procedure.
 */
PROCEDURE _BeginText

   _HMG_RPTDATA[ 116 ] := ''    // Text
   _HMG_ActiveControlRow := 0   // Row
   _HMG_ActiveControlCol := 0   // Col
   _HMG_ActiveControlWidth := 0   // Width
   _HMG_ActiveControlHeight := 0   // Height
   _HMG_ActiveControlFont := 'Arial'  // FontName
   _HMG_ActiveControlSize := 9   // FontSize
   _HMG_ActiveControlFontBold := .F.  // FontBold
   _HMG_ActiveControlFontItalic := .F.  // FontItalic
   _HMG_ActiveControlFontUnderLine := .F.  // FontUnderLine
   _HMG_ActiveControlFontStrikeOut := .F.  // FontStrikeout
   _HMG_ActiveControlFontColor := { 0, 0, 0 } // FontColor
   _HMG_ActiveControlRightAlign := .F.  // Alignment
   _HMG_ActiveControlCenterAlign := .F.  // Alignment

RETURN

/*
 * PROCEDURE _EndText()
 *
 * Creates a text object and adds it to the appropriate report band.
 *
 * Purpose:
 *   This procedure is called after defining all properties of a text object.
 *   It creates an array containing the text object's properties and adds it to the array corresponding to the current report band (header, detail, footer, etc.).
 *   The current report band is determined by the value of _HMG_RPTDATA[161].
 *   This allows the text object to be printed in the correct location when the report is executed.
 *
 * Notes:
 *   This procedure must be called after setting all text object properties.
 *   It is paired with the _BeginText procedure.
 */
PROCEDURE _EndText

   LOCAL aText

   aText := {      ;
      'TEXT', ;
      _HMG_RPTDATA[ 116 ], ;
      _HMG_ActiveControlRow, ;
      _HMG_ActiveControlCol, ;
      _HMG_ActiveControlWidth, ;
      _HMG_ActiveControlHeight, ;
      _HMG_ActiveControlFont, ;
      _HMG_ActiveControlSize, ;
      _HMG_ActiveControlFontBold, ;
      _HMG_ActiveControlFontItalic, ;
      _HMG_ActiveControlFontUnderLine, ;
      _HMG_ActiveControlFontStrikeOut, ;
      _HMG_ActiveControlFontColor, ;
      _HMG_ActiveControlRightAlign, ;
      _HMG_ActiveControlCenterAlign   ;
      }

   IF _HMG_RPTDATA[ 161 ] == 'HEADER'

      AAdd (  _HMG_RPTDATA[ 160 ], aText )

   ELSEIF _HMG_RPTDATA[ 161 ] == 'DETAIL'

      AAdd ( _HMG_RPTDATA[ 158 ], aText )

   ELSEIF _HMG_RPTDATA[ 161 ] == 'FOOTER'

      AAdd ( _HMG_RPTDATA[ 157 ], aText )

   ELSEIF _HMG_RPTDATA[ 161 ] == 'SUMMARY'

      AAdd ( _HMG_RPTDATA[ 126 ], aText )

   ELSEIF _HMG_RPTDATA[ 161 ] == 'GROUPHEADER'

      AAdd ( _HMG_RPTDATA[ 121 ], aText )

   ELSEIF _HMG_RPTDATA[ 161 ] == 'GROUPFOOTER'

      AAdd ( _HMG_RPTDATA[ 122 ], aText )

   ENDIF

RETURN

/*
 * PROCEDURE _BandHeight( nValue )
 *
 * Sets the height of the current report band.
 *
 * Parameters:
 *   nValue (NUMERIC): The height of the band in millimeters.
 *
 * Return Value:
 *   None.
 *
 * Purpose:
 *   This procedure sets the height of the currently active report band (header, detail, footer, summary, group header, or group footer).
 *   The current report band is determined by the value of _HMG_RPTDATA[161].
 *   The band height is used to calculate the layout of the report and to determine when to start a new page.
 *
 * Notes:
 *   This procedure must be called after calling the corresponding _Begin... procedure and before calling the corresponding _End... procedure for the band.
 */
PROCEDURE _BandHeight ( nValue )

   IF _HMG_RPTDATA[ 161 ] == 'HEADER'

      _HMG_RPTDATA[ 152 ] := nValue

   ELSEIF _HMG_RPTDATA[ 161 ] == 'DETAIL'

      _HMG_RPTDATA[ 153 ] := nValue

   ELSEIF _HMG_RPTDATA[ 161 ] == 'FOOTER'

      _HMG_RPTDATA[ 154 ] := nValue

   ELSEIF _HMG_RPTDATA[ 161 ] == 'SUMMARY'

      _HMG_RPTDATA[ 127 ] := nValue

   ELSEIF _HMG_RPTDATA[ 161 ] == 'GROUPHEADER'

      _HMG_RPTDATA[ 124 ] := nValue

   ELSEIF _HMG_RPTDATA[ 161 ] == 'GROUPFOOTER'

      _HMG_RPTDATA[ 123 ] := nValue

   ENDIF

RETURN

/*
 * PROCEDURE ExecuteReport( cReportName, lPreview, lSelect, cOutputFileName )
 *
 * Executes a report definition, printing it to a printer, displaying a preview, or saving it to a file (PDF or HTML).
 *
 * Parameters:
 *   cReportName (STRING): The name of the report to execute. This should match the name used when defining the report with _DefineReport.
 *   lPreview (LOGICAL):  .T. to display a print preview, .F. to print directly to the printer.
 *   lSelect (LOGICAL):   .T. to show a printer selection dialog, .F. to use the default printer.
 *   cOutputFileName (STRING): Optional. If specified, the report will be saved to this file.  If the extension is ".PDF", it will be saved as a PDF. If the extension is ".HTML", it will be saved as HTML.
 *
 * Return Value:
 *   None.
 *
 * Purpose:
 *   This procedure is the main entry point for executing a report definition. It performs the following steps:
 *     1. Retrieves the report definition from memory using the report name.
 *     2. Determines the print parameters (printer, paper size, orientation) based on the report definition and the input parameters.
 *     3. Selects the printer (if necessary).
 *     4. Starts the print job (or PDF/HTML creation).
 *     5. Iterates through the data, printing each detail row.
 *     6. Handles group headers and footers (if defined).
 *     7. Prints the summary (if defined).
 *     8. Prints the footer.
 *     9. Ends the print job (or PDF/HTML creation).
 *     10. Restores the original record pointer position.
 *     11. Releases the report definition from memory.
 *
 * Notes:
 *   The procedure relies on the report definition being correctly defined using the _DefineReport, _BeginLayout, _BeginHeader, _BeginDetail, _BeginFooter, and _EndReport procedures.
 *   The procedure uses the _HMG_RPTDATA array to store report properties and temporary data.
 *   The procedure supports printing to a printer, displaying a print preview, and saving to a PDF or HTML file.
 *   Only one group level is allowed.
 *   For PDF output, only JPG images are supported.
 *   For PDF output, only horizontal and vertical lines are supported.
 */
PROCEDURE ExecuteReport ( cReportName, lPreview, lSelect, cOutputFileName )

   LOCAL aLayout
   LOCAL aHeader
   LOCAL aDetail
   LOCAL aFooter
   LOCAL aSummary
   LOCAL aTemp
   LOCAL cPrinter
   LOCAL nPaperWidth
   LOCAL nPaperHeight
   LOCAL nOrientation
   LOCAL nPaperSize
   LOCAL nHeadeHeight
   LOCAL nDetailHeight
   LOCAL nFooterHeight
   LOCAL nCurrentOffset
   LOCAL nPreviousRecNo
   LOCAL nSummaryHeight
   LOCAL aGroupHeader
   LOCAL aGroupFooter
   LOCAL nGroupHeaderHeight
   LOCAL nGroupFooterHeight
   LOCAL xGroupExpression
   LOCAL nGroupCount
   LOCAL xPreviousGroupExpression
   LOCAL lGroupStarted
   LOCAL lSuccess
   LOCAL aMiscData
   LOCAL xTemp
   LOCAL aPaper[ 18 ][ 2 ]
   LOCAL cPdfPaperSize := ''
   LOCAL cPdfOrientation := ''
   LOCAL nOutfile
   LOCAL xSkipProcedure
   LOCAL xEOF
   LOCAL lTempEof

   IF _HMG_RPTDATA[ 120 ] > 1
      MsgMiniGUIError( 'Only One Group Level Allowed' )
   ENDIF

   _HMG_RPTDATA[ 149 ] := ''
   _HMG_RPTDATA[ 150 ] := .F.
   _HMG_RPTDATA[ 163 ] := .F.

   IF ValType ( cOutputFileName ) == 'C'

      IF AllTrim ( Upper ( Right ( cOutputFileName, 4 ) ) ) == '.PDF'

         _HMG_RPTDATA[ 150 ] := .T.

      ELSEIF AllTrim ( Upper ( Right ( cOutputFileName, 5 ) ) ) == '.HTML'

         _HMG_RPTDATA[ 163 ] := .T.

      ENDIF

   ENDIF

   IF _HMG_RPTDATA[ 163 ] == .T.

      _HMG_RPTDATA[ 149 ] += '<html>' + Chr( 13 ) + Chr( 10 )

      _HMG_RPTDATA[ 149 ] += '<style>' + Chr( 13 ) + Chr( 10 )
      _HMG_RPTDATA[ 149 ] += 'div {position:absolute}' + Chr( 13 ) + Chr( 10 )
      _HMG_RPTDATA[ 149 ] += '.line { }' + Chr( 13 ) + Chr( 10 )
      _HMG_RPTDATA[ 149 ] += '</style>' + Chr( 13 ) + Chr( 10 )

      _HMG_RPTDATA[ 149 ] += '<body>' + Chr( 13 ) + Chr( 10 )

   ENDIF

   IF _HMG_RPTDATA[ 150 ] == .T.
      PdfInit()
      pdfOpen( cOutputFileName, 200, .T. )
   ENDIF

   IF xSkipProcedure == NIL

      /*
       * If not workarea open, cancel report execution.
       * This check ensures that the report is only executed if a database work area is currently open.
       * If no work area is open, the function returns immediately, preventing potential errors.
       */
      IF Select() == 0
         RETURN
      ENDIF

      nPreviousRecNo := RecNo()

   ENDIF

   /* 
    * Determine Print Parameters
    * 
    * This section retrieves the report definition and extracts the layout, header, detail, footer, summary,
    * group header, group footer, and miscellaneous data arrays. These arrays contain the information needed
    * to format and print the report.
    */

   aTemp := __mvGet ( cReportName )

   aLayout  := aTemp[ 1 ]
   aHeader  := aTemp[ 2 ]
   aDetail  := aTemp[ 3 ]
   aFooter  := aTemp[ 4 ]
   aSummary := aTemp[ 5 ]
   aGroupHeader := aTemp[ 6 ]
   aGroupFooter := aTemp[ 7 ]
   aMiscData := aTemp[ 8 ]

   nGroupCount := aMiscData[ 1 ]
   nHeadeHeight := aMiscData[ 2 ]
   nDetailHeight := aMiscData[ 3 ]
   nFooterHeight := aMiscData[ 4 ]
   nSummaryHeight := aMiscData[ 5 ]
   nGroupHeaderHeight := aMiscData[ 6 ]
   nGroupFooterHeight := aMiscData[ 7 ]
   xTemp := aMiscData[ 8 ]
   xSkipProcedure := aMiscData[ 9 ]
   xEOF  := aMiscData[ 10 ]

   nOrientation := aLayout[ 1 ]
   nPaperSize   := aLayout[ 2 ]
   nPaperWidth  := aLayout[ 3 ]
   nPaperHeight := aLayout[ 4 ]

   IF ValType ( lPreview ) <> 'L'
      lPreview := .F.
   ENDIF

   IF ValType ( lSelect ) <> 'L'
      lSelect := .F.
   ENDIF

   IF _HMG_RPTDATA[ 150 ] == .F. .AND. _HMG_RPTDATA[ 163 ] == .F.

      IF lSelect == .T.
         cPrinter := GetPrinter()
      ELSE
         cPrinter := GetDefaultPrinter()
      ENDIF

      IF Empty ( cPrinter )
         RETURN
      ENDIF

   ENDIF

   /* 
    * Select Printer
    * 
    * This section selects the printer to be used for printing the report.
    * If lSelect is .T., a printer selection dialog is displayed. Otherwise, the default printer is used.
    * The printer selection is skipped if the report is being generated as a PDF or HTML file.
    */

   IF _HMG_RPTDATA[ 150 ] == .F. .AND. _HMG_RPTDATA[ 163 ] == .F.

      IF lPreview == .T.

         IF nPaperSize == PRINTER_PAPER_USER

            SELECT PRINTER cPrinter ;
               TO lSuccess ;
               ORIENTATION nOrientation ;
               PAPERSIZE nPaperSize ;
               PAPERWIDTH nPaperWidth ;
               PAPERLENGTH nPaperHeight ;
               PREVIEW

         ELSE

            SELECT PRINTER cPrinter ;
               TO lSuccess ;
               ORIENTATION nOrientation ;
               PAPERSIZE nPaperSize ;
               PREVIEW

         ENDIF

      ELSE

         IF nPaperSize == PRINTER_PAPER_USER

            SELECT PRINTER cPrinter ;
               TO lSuccess ;
               ORIENTATION nOrientation ;
               PAPERSIZE nPaperSize ;
               PAPERWIDTH nPaperWidth ;
               PAPERLENGTH nPaperHeight

         ELSE

            SELECT PRINTER cPrinter ;
               TO lSuccess ;
               ORIENTATION nOrientation ;
               PAPERSIZE nPaperSize ;

         ENDIF

      ENDIF

      IF ! lSuccess
         MsgMiniGuiError ( "Report: Can't Init Printer." )
      ENDIF

   ENDIF

   /* 
    * Determine Paper Dimensions in mm.
    * 
    * This section determines the paper dimensions in millimeters based on the selected paper size and orientation.
    * It uses a lookup table (aPaper) to retrieve the dimensions for standard paper sizes.
    * If the paper size or orientation is not supported, an error message is displayed.
    */

   IF npaperSize >= 1 .AND. nPaperSize <= 18

      aPaper[ PRINTER_PAPER_LETTER      ] := { 215.9, 279.4 }
      aPaper[ PRINTER_PAPER_LETTERSMALL ] := { 215.9, 279.4 }
      aPaper[ PRINTER_PAPER_TABLOID     ] := { 279.4, 431.8 }
      aPaper[ PRINTER_PAPER_LEDGER      ] := { 431.8, 279.4 }
      aPaper[ PRINTER_PAPER_LEGAL       ] := { 215.9, 355.6 }
      aPaper[ PRINTER_PAPER_STATEMENT   ] := { 139.7, 215.9 }
      aPaper[ PRINTER_PAPER_EXECUTIVE   ] := { 184.15, 266.7 }
      aPaper[ PRINTER_PAPER_A3          ] := { 297, 420 }
      aPaper[ PRINTER_PAPER_A4          ] := { 210, 297 }
      aPaper[ PRINTER_PAPER_A4SMALL     ] := { 210, 297 }
      aPaper[ PRINTER_PAPER_A5          ] := { 148, 210 }
      aPaper[ PRINTER_PAPER_B4          ] := { 250, 354 }
      aPaper[ PRINTER_PAPER_B5          ] := { 182, 257 }
      aPaper[ PRINTER_PAPER_FOLIO       ] := { 215.9, 330.2 }
      aPaper[ PRINTER_PAPER_QUARTO      ] := { 215, 275 }
      aPaper[ PRINTER_PAPER_10X14       ] := { 254, 355.6 }
      aPaper[ PRINTER_PAPER_11X17       ] := { 279.4, 431.8 }
      aPaper[ PRINTER_PAPER_NOTE        ] := { 215.9, 279.4 }

      IF  nOrientation == PRINTER_ORIENT_PORTRAIT

         npaperHeight := aPaper[ nPaperSize ][ 2 ]

      ELSEIF nOrientation == PRINTER_ORIENT_LANDSCAPE

         npaperHeight := aPaper[ nPaperSize ][ 1 ]

      ELSE

         MsgMiniGUIError ( 'Report: Orientation Not Supported.' )

      ENDIF

   ELSE

      MsgMiniGUIError ( 'Report: Paper Size Not Supported.' )

   ENDIF


   IF _HMG_RPTDATA[ 150 ] == .T.

      /*
       * PDF Paper Size
       * This section maps the HMG paper size constants to PDF paper size names.
       * This is necessary because the PDF library uses different names for the same paper sizes.
       * If the paper size is not supported, an error message is displayed.
       */

      IF nPaperSize == PRINTER_PAPER_LETTER

         cPdfPaperSize := "LETTER"

      ELSEIF nPaperSize == PRINTER_PAPER_LEGAL

         cPdfPaperSize := "LEGAL"

      ELSEIF nPaperSize == PRINTER_PAPER_A4

         cPdfPaperSize := "A4"

      ELSEIF nPaperSize == PRINTER_PAPER_TABLOID

         cPdfPaperSize := "LEDGER"

      ELSEIF nPaperSize == PRINTER_PAPER_EXECUTIVE

         cPdfPaperSize := "EXECUTIVE"

      ELSEIF nPaperSize == PRINTER_PAPER_A3

         cPdfPaperSize := "A3"

      ELSEIF nPaperSize == PRINTER_PAPER_ENV_10

         cPdfPaperSize := "COM10"

      ELSEIF nPaperSize == PRINTER_PAPER_B4

         cPdfPaperSize := "JIS B4"

      ELSEIF nPaperSize == PRINTER_PAPER_B5

         cPdfPaperSize := "B5"

      ELSEIF nPaperSize == PRINTER_PAPER_P32K

         cPdfPaperSize := "JPOST"

      ELSEIF nPaperSize == PRINTER_PAPER_ENV_C5

         cPdfPaperSize := "C5"

      ELSEIF nPaperSize == PRINTER_PAPER_ENV_DL

         cPdfPaperSize := "DL"

      ELSEIF nPaperSize == PRINTER_PAPER_ENV_B5

         cPdfPaperSize := "B5"

      ELSEIF nPaperSize == PRINTER_PAPER_ENV_MONARCH

         cPdfPaperSize := "MONARCH"

      ELSE

         MsgMiniGUIError ( "Report: PDF Paper Size Not Supported." )

      ENDIF

      /*
       * PDF Orientation
       * This section maps the HMG orientation constants to PDF orientation names.
       * This is necessary because the PDF library uses different names for the same orientation.
       * If the orientation is not supported, an error message is displayed.
       */

      IF  nOrientation == PRINTER_ORIENT_PORTRAIT

         cPdfOrientation := 'P'

      ELSEIF nOrientation == PRINTER_ORIENT_LANDSCAPE

         cPdfOrientation := 'L'

      ELSE

         MsgMiniGUIError ( 'Report: Orientation Not Supported.' )

      ENDIF

   ENDIF

   /* *
    * Print Document
    * *
    * This section contains the main loop that iterates through the data and prints the report.
    * It handles group headers and footers, the detail section, and the summary section.
    * It also handles page breaks and prints the header and footer on each page.
    */

   IF nGroupCount > 0

      xGroupExpression := &( xTemp )

   ENDIF

   _HMG_RPTDATA[ 117 ] := 1

   IF _HMG_RPTDATA[ 150 ] == .F. .AND. _HMG_RPTDATA[ 163 ] == .F.

      START PRINTDOC

   ENDIF

   IF xSkipProcedure == NIL
      GO TOP
   ENDIF

   xPreviousGroupExpression := ''
   lGroupStarted := .F.

   IF xSkipProcedure == NIL
      lTempEof := Eof()
   ELSE
      lTempEof := Eval( xEof )
   ENDIF

   DO WHILE .NOT. lTempEof

      IF _HMG_RPTDATA[ 163 ] == .F.

         IF _HMG_RPTDATA[ 150 ] == .T.

            pdfNewPage( cPdfPaperSize, cPdfOrientation, 6 )

         ELSE

            START PRINTPAGE

         ENDIF

         nCurrentOffset := 0

         _ProcessBand ( aHeader, nCurrentOffset )

         nCurrentOffset := nHeadeHeight

         DO WHILE .T.

            IF nGroupCount > 0

               IF ( ValType ( xPreviousGroupExpression ) != ValType ( xGroupExpression ) ) .OR. ( xPreviousGroupExpression <> xGroupExpression )

                  IF lGroupStarted

                     _ProcessBand ( aGroupFooter, nCurrentOffset )
                     nCurrentOffset += nGroupFooterHeight

                  ENDIF

                  _ProcessBand ( aGroupHeader, nCurrentOffset )
                  nCurrentOffset += nGroupHeaderHeight

                  xPreviousGroupExpression := xGroupExpression

                  lGroupStarted := .T.

               ENDIF

            ENDIF

            _ProcessBand ( aDetail, nCurrentOffset )

            nCurrentOffset += nDetailHeight

            IF xSkipProcedure == NIL
               SKIP
               lTempEof := Eof()
            ELSE
               Eval( xSkipProcedure )
               lTempEof := Eval( xEof )
            ENDIF

            IF lTempEof

               /*
                * If group footer defined, PRINT it.
                * This section checks if a group footer is defined and prints it if it is.
                * It also handles the case where the group footer does not fit on the current page,
                * in which case it prints the page footer, starts a new page, and prints the header first.
                */

               IF nGroupFooterHeight > 0

                  /*
                   * If group footer don't fit in the current page, print page footer,
                   * start a NEW page and PRINT header first
                   */

                  IF nCurrentOffset + nGroupFooterHeight > nPaperHeight - nFooterHeight

                     nCurrentOffset := nPaperHeight - nFooterHeight
                     _ProcessBand ( aFooter, nCurrentOffset )

                     IF _HMG_RPTDATA[ 150 ] == .F.

                        END PRINTPAGE
                        START PRINTPAGE

                     ELSE

                        pdfNewPage( cPdfPaperSize, cPdfOrientation, 6 )

                     ENDIF

                     _HMG_RPTDATA[ 117 ]++

                     nCurrentOffset := 0
                     _ProcessBand ( aHeader, nCurrentOffset )
                     nCurrentOffset := nHeadeHeight

                  ENDIF

                  _ProcessBand ( aGroupFooter, nCurrentOffset )
                  nCurrentOffset += nGroupFooterHeight

               ENDIF

               /*
                * If Summary defined, PRINT it.
                * This section checks if a summary is defined and prints it if it is.
                * It also handles the case where the summary does not fit on the current page,
                * in which case it prints the page footer, starts a new page, and prints the header first.
                */

               IF Len ( aSummary ) > 0

                  /*
                   * If summary don't fit in the current page, print footer,
                   * start a NEW page and PRINT header first
                   */

                  IF nCurrentOffset + nSummaryHeight > nPaperHeight - nFooterHeight

                     nCurrentOffset := nPaperHeight - nFooterHeight
                     _ProcessBand ( aFooter, nCurrentOffset )

                     IF _HMG_RPTDATA[ 150 ] == .F.

                        END PRINTPAGE
                        START PRINTPAGE

                     ELSE

                        pdfNewPage( cPdfPaperSize, cPdfOrientation, 6 )

                     ENDIF

                     _HMG_RPTDATA[ 117 ]++

                     nCurrentOffset := 0
                     _ProcessBand ( aHeader, nCurrentOffset )
                     nCurrentOffset := nHeadeHeight

                  ENDIF

                  _ProcessBand ( aSummary, nCurrentOffset )

                  EXIT

               ENDIF

               EXIT

            ENDIF

            IF nGroupCount > 0

               xGroupExpression := &( xTemp )

            ENDIF

            IF nCurrentOffset + nDetailHeight > nPaperHeight - nFooterHeight

               EXIT

            ENDIF

         ENDDO

         nCurrentOffset := nPaperHeight - nFooterHeight

         _ProcessBand ( aFooter, nCurrentOffset )

         IF _HMG_RPTDATA[ 150 ] == .F.

            END PRINTPAGE

         ENDIF

         _HMG_RPTDATA[ 117 ]++

      ELSE

         nCurrentOffset := 0

         _ProcessBand ( aHeader, nCurrentOffset )

         nCurrentOffset := nHeadeHeight

         DO WHILE .T.

            IF nGroupCount > 0

               IF xPreviousGroupExpression <> xGroupExpression

                  IF lGroupStarted

                     _ProcessBand ( aGroupFooter, nCurrentOffset )
                     nCurrentOffset += nGroupFooterHeight

                  ENDIF

                  _ProcessBand ( aGroupHeader, nCurrentOffset )
                  nCurrentOffset += nGroupHeaderHeight

                  xPreviousGroupExpression := xGroupExpression

                  lGroupStarted := .T.

               ENDIF

            ENDIF

            _ProcessBand ( aDetail, nCurrentOffset )

            nCurrentOffset += nDetailHeight

            IF xSkipProcedure == NIL
               SKIP
               lTempEof := Eof()
            ELSE
               Eval( xSkipProcedure )
               lTempEof := Eval( xEof )
            ENDIF

            IF lTempEof

               /*
                * If group footer defined, PRINT it.
                * This section checks if a group footer is defined and prints it if it is.
                */

               IF nGroupFooterHeight > 0

                  _ProcessBand ( aGroupFooter, nCurrentOffset )
                  nCurrentOffset += nGroupFooterHeight

               ENDIF

               /*
                * If Summary defined, PRINT it.
                * This section checks if a summary is defined and prints it if it is.
                */

               IF Len ( aSummary ) > 0
                  _ProcessBand ( aSummary, nCurrentOffset )
                  nCurrentOffset += nSummaryHeight
               ENDIF

               EXIT

            ENDIF

            IF nGroupCount > 0
               xGroupExpression := &( xTemp )
            ENDIF

         ENDDO

         _ProcessBand ( aFooter, nCurrentOffset )

      ENDIF

   ENDDO

   IF _HMG_RPTDATA[ 150 ] == .F. .AND. _HMG_RPTDATA[ 163 ] == .F.

      END PRINTDOC

   ELSEIF _HMG_RPTDATA[ 150 ] == .T.

      pdfClose()

   ELSEIF _HMG_RPTDATA[ 163 ] == .T.

      _HMG_RPTDATA[ 149 ] += '</body>' + Chr( 13 ) + Chr( 10 )
      _HMG_RPTDATA[ 149 ] += '</html>' + Chr( 13 ) + Chr( 10 )

      nOutfile := FCreate( cOutputFileName, FC_NORMAL )

      FWrite( nOutfile, _HMG_RPTDATA[ 149 ], Len( _HMG_RPTDATA[ 149 ] ) )

      FClose( nOutfile )

   ENDIF

   IF xSkipProcedure == NIL
      GO nPreviousRecNo
   ENDIF

   IF __mvExist ( cReportName )
#ifndef _PUBLIC_RELEASE_
      __mvPut ( cReportName , 0 )
#else
      __mvXRelease ( cReportName )
#endif
   ENDIF

RETURN

/*
 * STATIC PROCEDURE _ProcessBand( aBand, nOffset )
 *
 * Processes a report band (header, detail, footer, summary, group header, or group footer).
 *
 * Parameters:
 *   aBand (ARRAY): An array containing the objects to be printed in the band. Each element of the array is an array representing a report object (text, image, line, rectangle).
 *   nOffset (NUMERIC): The vertical offset (in millimeters) from the top of the page where the band should be printed.
 *
 * Return Value:
 *   None.
 *
 * Purpose:
 *   This procedure iterates through the objects in a report band and calls the _PrintObject procedure to print each object.
 *   It is used to print the different sections of the report (header, detail, footer, summary, group header, and group footer).
 *
 * Notes:
 *   The procedure assumes that the aBand array is correctly formatted and contains valid report objects.
 */
STATIC PROCEDURE _ProcessBand ( aBand, nOffset )
   LOCAL i

   FOR i := 1 TO Len ( aBand )

      _PrintObject ( aBand[i ], nOffset )

   NEXT i

RETURN

/*
 * STATIC PROCEDURE _PrintObject( aObject, nOffset )
 *
 * Prints a single report object (text, image, line, or rectangle).
 *
 * Parameters:
 *   aObject (ARRAY): An array representing the report object to be printed. The first element of the array indicates the object type ('TEXT', 'IMAGE', 'LINE', or 'RECTANGLE'). The remaining elements contain the object's properties (e.g., text value, coordinates, font, color).
 *   nOffset (NUMERIC): The vertical offset (in millimeters) from the top of the page where the object should be printed.
 *
 * Return Value:
 *   None.
 *
 * Purpose:
 *   This procedure determines the type of report object and calls the appropriate printing procedure (_PrintText, _PrintImage, _PrintLine, or _PrintRectangle) to print the object.
 *   It acts as a dispatcher, routing the printing request to the correct handler based on the object type.
 *
 * Notes:
 *   The procedure assumes that the aObject array is correctly formatted and contains valid object properties.
 */
STATIC PROCEDURE _PrintObject ( aObject, nOffset )

   IF aObject[ 1 ] == 'TEXT'

      _PrintText( aObject, nOffset )

   ELSEIF aObject[ 1 ] == 'IMAGE'

      _PrintImage( aObject, nOffset )

   ELSEIF aObject[ 1 ] == 'LINE'

      _PrintLine( aObject, nOffset )

   ELSEIF aObject[ 1 ] == 'RECTANGLE'

      _PrintRectangle( aObject, nOffset )

   ENDIF

RETURN

/*
 * STATIC PROCEDURE _PrintText( aObject, nOffset )
 *
 * Prints a text object on the report.
 *
 * Parameters:
 *   aObject (ARRAY): An array containing the text object's properties:
 *     - aObject[2] (STRING): The text value to be printed.
 *     - aObject[3] (NUMERIC): The row coordinate (in millimeters) of the text.
 *     - aObject[4] (NUMERIC): The column coordinate (in millimeters) of the text.
 *     - aObject[5] (NUMERIC): The width (in millimeters) of the text area.
 *     - aObject[6] (NUMERIC): The height (in millimeters) of the text area.
 *     - aObject[7] (STRING): The font name.
 *     - aObject[8] (NUMERIC): The font size.
 *     - aObject[9] (LOGICAL): .T. if the font is bold, .F. otherwise.
 *     - aObject[10] (LOGICAL): .T. if the font is italic, .F. otherwise.
 *     - aObject[11] (LOGICAL): .T. if the font is underlined, .F. otherwise.
 *     - aObject[12] (LOGICAL): .T. if the font is strikeout, .F. otherwise.
 *     - aObject[13] (ARRAY): An array containing the RGB color values for the font (e.g., {255, 0, 0} for red).
 *     - aObject[14] (LOGICAL): .T. for right alignment, .F. otherwise.
 *     - aObject[15] (LOGICAL): .T. for center alignment, .F. otherwise.
 *   nOffset (NUMERIC): The vertical offset (in millimeters) from the top of the page.
 *
 * Return Value:
 *   None.
 *
 * Purpose:
 *   This procedure prints a text object on the report, taking into account the object's properties (font, size, color, alignment, etc.).
 *   It uses different printing functions depending on whether the report is being printed to a printer, saved as a PDF file, or saved as an HTML file.
 *
 * Notes:
 *   The procedure uses the _HMG_PRINTER_H_MULTILINE_PRINT function to print text to a printer.
 *   For PDF output, it uses the pdfAtSay function.
 *   For HTML output, it generates HTML code to display the text.
 */
STATIC PROCEDURE _PrintText( aObject, nOffset )
   LOCAL cValue  := aObject[ 2 ]
   LOCAL nRow  := aObject[ 3 ]
   LOCAL nCol  := aObject[ 4 ]
   LOCAL nWidth  := aObject[ 5 ]
   LOCAL nHeight  := aObject[ 6 ]
   LOCAL cFontname  := aObject[ 7 ]
   LOCAL nFontSize  := aObject[ 8 ]
   LOCAL lFontBold  := aObject[ 9 ]
   LOCAL lFontItalic := aObject[ 10 ]
   LOCAL lFontUnderLine := aObject[ 11 ]
   LOCAL lFOntStrikeout := aObject[ 12 ]
   LOCAL aFontColor := aObject[ 13 ]
   LOCAL lAlignment_1  := aObject[ 14 ]
   LOCAL lAlignment_2  := aObject[ 15 ]
   LOCAL cAlignment := ''
   LOCAL nFontStyle := 0
   LOCAL nTextRowFix := 5
   LOCAL cHtmlAlignment

   cValue := &cValue // Evaluate the text value, allowing for expressions

   IF _HMG_RPTDATA[ 150 ] == .F. .AND. _HMG_RPTDATA[ 163 ] == .F. // Printing to printer

      IF lAlignment_1 == .F. .AND.  lAlignment_2 == .T. // Center

         cAlignment := 'CENTER'

      ELSEIF lAlignment_1 == .T. .AND.  lAlignment_2 == .F. // Right

         cAlignment := 'RIGHT'

      ELSEIF lAlignment_1 == .F. .AND.  lAlignment_2 == .F. // Left

         cAlignment := ''

      ENDIF

      _HMG_PRINTER_H_MULTILINE_PRINT ( _HMG_PRINTER_HDC, nRow  + nOffset, nCol, nRow + nHeight  + nOffset, nCol + nWidth, cFontName, nFontSize, aFontColor[ 1 ], aFontColor[ 2 ], aFontColor[ 3 ], cValue, lFontBold, lFontItalic, lFontUnderline, lFontStrikeout, .T., .T., .T., cAlignment )

   ELSEIF _HMG_RPTDATA[ 163 ] == .T. // Saving as HTML

      IF ValType ( cValue ) == "N"

         cValue := AllTrim( Str( cValue ) )

      ELSEIF ValType ( cValue ) == "D"

         cValue := DToC ( cValue )

      ELSEIF ValType ( cValue ) == "L"

         cValue := if ( cValue == .T., _HMG_RPTDATA[ 371 ][ 24 ], _HMG_RPTDATA[ 371 ][ 25 ] )

      ENDIF

      IF lAlignment_1 == .F. .AND.  lAlignment_2 == .T. // Center

         cHtmlAlignment := 'center'

      ELSEIF lAlignment_1 == .T. .AND.  lAlignment_2 == .F. // Right

         cHtmlAlignment := 'right'

      ELSEIF lAlignment_1 == .F. .AND.  lAlignment_2 == .F. // Left

         cHtmlAlignment := 'left'

      ENDIF

      _HMG_RPTDATA[ 149 ] += '<div style=position:absolute;left:' + AllTrim( Str( nCol ) ) +  'mm;top:' +  AllTrim( Str( nRow + nOffset ) ) + 'mm;width:' +  AllTrim( Str( nWidth ) ) + 'mm;font-size:' + AllTrim( Str( nFontSize ) ) + 'pt;font-family:"' +  cFontname + '";text-align:' + cHtmlAlignment + ';font-weight:' + iif( lFontBold, 'bold', 'normal' ) + ';font-style:' + iif( lFontItalic, 'italic', 'normal' ) + ';text-decoration:' + iif( lFontUnderLine, 'underline', 'none' ) + ';color:rgb(' + AllTrim( Str( aFontColor[ 1 ] ) ) + ',' + AllTrim( Str( aFontColor[ 2 ] ) ) + ',' +  AllTrim( Str( aFontColor[ 3 ] ) ) + ');>' + cValue + '</div>' + Chr( 13 ) + Chr( 10 )

   ELSEIF _HMG_RPTDATA[ 150 ] == .T. // Saving as PDF

      IF ValType ( cValue ) == "N"

         cValue := AllTrim( Str( cValue ) )

      ELSEIF ValType ( cValue ) == "D"

         cValue := DToC ( cValue )

      ELSEIF ValType ( cValue ) == "L"

         cValue := if ( cValue == .T., _HMG_RPTDATA[ 371 ][ 24 ], _HMG_RPTDATA[ 371 ][ 25 ] )

      ENDIF

      IF lFontBold == .F. .AND. lFontItalic == .F.

         nFontStyle := 0

      ELSEIF lFontBold == .T. .AND. lFontItalic == .F.

         nFontStyle := 1

      ELSEIF lFontBold == .F. .AND. lFontItalic == .T.

         nFontStyle := 2

      ELSEIF lFontBold == .T. .AND. lFontItalic == .T.

         nFontStyle := 3

      ENDIF

      pdfSetFont( cFontname, nFontStyle, nFontSize )

      IF lAlignment_1 == .F. .AND.  lAlignment_2 == .T. // Center

         IF lFontUnderLine

            pdfAtSay ( cValue + Chr( 254 ), nRow + nOffset + nTextRowFix, nCol + ( nWidth - ( pdfTextWidth( cValue ) * 25.4 ) ) / 2, 'M' )

         ELSE

            pdfAtSay ( Chr( 253 ) + Chr( aFontColor[ 1 ] ) + Chr( aFontColor[ 2 ] ) + Chr( aFontColor[ 3 ] ) + cValue, nRow + nOffset + nTextRowFix, nCol + ( nWidth - ( pdfTextWidth( cValue ) * 25.4 ) ) / 2, 'M' )

         ENDIF

      ELSEIF lAlignment_1 == .T. .AND.  lAlignment_2 == .F. // Right

         IF lFontUnderLine

            pdfAtSay ( cValue + Chr( 254 ), nRow + nOffset + nTextRowFix, nCol + nWidth - pdfTextWidth( cValue ) * 25.4, 'M' )

         ELSE

            pdfAtSay ( Chr( 253 ) + Chr( aFontColor[ 1 ] ) + Chr( aFontColor[ 2 ] ) + Chr( aFontColor[ 3 ] ) + cValue, nRow + nOffset + nTextRowFix, nCol + nWidth - pdfTextWidth( cValue ) * 25.4, 'M' )

         ENDIF

      ELSEIF lAlignment_1 == .F. .AND.  lAlignment_2 == .F. // Left

         IF lFontUnderLine

            pdfAtSay ( cValue + Chr( 254 ), nRow + nOffset + nTextRowFix, nCol, 'M' )

         ELSE

            pdfAtSay ( Chr( 253 ) + Chr( aFontColor[ 1 ] ) + Chr( aFontColor[ 2 ] ) + Chr( aFontColor[ 3 ] ) + cValue, nRow + nOffset + nTextRowFix, nCol, 'M' )

         ENDIF

      ENDIF

   ENDIF

RETURN

/*
 * STATIC PROCEDURE _PrintImage( aObject, nOffset )
 *
 * Prints an image object on the report.
 *
 * Parameters:
 *   aObject (ARRAY): An array containing the image object's properties:
 *     - aObject[2] (STRING): The path to the image file.
 *     - aObject[3] (NUMERIC): The row coordinate (in millimeters) of the image.
 *     - aObject[4] (NUMERIC): The column coordinate (in millimeters) of the image.
 *     - aObject[5] (NUMERIC): The width (in millimeters) of the image.
 *     - aObject[6] (NUMERIC): The height (in millimeters) of the image.
 *   nOffset (NUMERIC): The vertical offset (in millimeters) from the top of the page.
 *
 * Return Value:
 *   None.
 *
 * Purpose:
 *   This procedure prints an image on the report, handling different output formats (printer, PDF, HTML).
 *   It uses different functions depending on the output format.
 *
 * Notes:
 *   For printer output, it uses the _HMG_PRINTER_H_IMAGE function.
 *   For PDF output, it uses the pdfImage function, but only supports JPG images.
 *   For HTML output, it generates HTML code to display the image.
 *   The _HMG_RPTDATA array contains report-specific data and settings.
 */
STATIC PROCEDURE _PrintImage( aObject, nOffset )
   LOCAL cValue  := aObject[ 2 ]
   LOCAL nRow  := aObject[ 3 ]
   LOCAL nCol  := aObject[ 4 ]
   LOCAL nWidth  := aObject[ 5 ]
   LOCAL nHeight  := aObject[ 6 ]

   IF _HMG_RPTDATA[ 150 ] == .F. .AND. _HMG_RPTDATA[ 163 ] == .F. // Printing to printer

      _HMG_PRINTER_H_IMAGE ( _HMG_PRINTER_HDC, cValue, nRow + nOffset, nCol, nHeight, nWidth, .T. )

   ELSEIF _HMG_RPTDATA[ 150 ] == .T. // Saving as PDF

      IF Upper ( Right( cValue, 4 ) ) == '.JPG'

         pdfImage ( cValue, nRow + nOffset, nCol, "M", nHeight, nWidth )

      ELSE

         MsgMiniGuiError ( "Report: Only JPG images allowed." )

      ENDIF

   ELSEIF _HMG_RPTDATA[ 163 ] == .T. // Saving as HTML

      _HMG_RPTDATA[ 149 ] += '<div style=position:absolute;left:' + AllTrim( Str( nCol ) ) + 'mm;top:' + AllTrim( Str( nRow + nOffset ) )  + 'mm;> <img src="' + cValue + '" ' + 'width=' + AllTrim( Str( nWidth * 3.85 ) ) + 'mm height=' + AllTrim( Str( nHeight * 3.85 ) ) + 'mm/> </div>' + Chr( 13 ) + Chr( 10 )

   ENDIF

RETURN

/*
 * STATIC PROCEDURE _PrintLine( aObject, nOffset )
 *
 * Prints a line object on the report.
 *
 * Parameters:
 *   aObject (ARRAY): An array containing the line object's properties:
 *     - aObject[2] (NUMERIC): The row coordinate (in millimeters) of the starting point.
 *     - aObject[3] (NUMERIC): The column coordinate (in millimeters) of the starting point.
 *     - aObject[4] (NUMERIC): The row coordinate (in millimeters) of the ending point.
 *     - aObject[5] (NUMERIC): The column coordinate (in millimeters) of the ending point.
 *     - aObject[6] (NUMERIC): The width (in millimeters) of the line (pen width).
 *     - aObject[7] (ARRAY): An array containing the RGB color values for the line (e.g., {255, 0, 0} for red).
 *   nOffset (NUMERIC): The vertical offset (in millimeters) from the top of the page.
 *
 * Return Value:
 *   None.
 *
 * Purpose:
 *   This procedure prints a line on the report, handling different output formats (printer, PDF, HTML).
 *   It uses different functions depending on the output format.
 *
 * Notes:
 *   For printer output, it uses the _HMG_PRINTER_H_LINE function.
 *   For PDF output, it uses the pdfBox function to simulate a line, but only supports horizontal and vertical lines.
 *   For HTML output, it generates HTML code to display the line.
 *   The _HMG_RPTDATA array contains report-specific data and settings.
 */
STATIC PROCEDURE _PrintLine( aObject, nOffset )
   LOCAL nFromRow  := aObject[ 2 ]
   LOCAL nFromCol  := aObject[ 3 ]
   LOCAL nToRow  := aObject[ 4 ]
   LOCAL nToCol  := aObject[ 5 ]
   LOCAL nPenWidth  := aObject[ 6 ]
   LOCAL aPenColor  := aObject[ 7 ]

   IF _HMG_RPTDATA[ 150 ] == .F. .AND. _HMG_RPTDATA[ 163 ] == .F. // Printing to printer

      _HMG_PRINTER_H_LINE ( _HMG_PRINTER_HDC, nFromRow + nOffset, nFromCol, nToRow  + nOffset, nToCol, nPenWidth, aPenColor[ 1 ], aPenColor[ 2 ], aPenColor[ 3 ], .T., .T. )

   ELSEIF _HMG_RPTDATA[ 150 ] == .T. // Saving as PDF

      IF nFromRow <> nToRow .AND. nFromCol <> nToCol
         MsgMiniGuiError ( 'Report: Only horizontal and vertical lines are supported with PDF output.' )
      ENDIF

      pdfBox ( nFromRow + nOffset, nFromCol, nToRow + nOffset + nPenWidth, nToCol, 0, 1, "M", Chr( 253 ) + Chr( aPenColor[ 1 ] ) + Chr( aPenColor[ 2 ] ) + Chr( aPenColor[ 3 ] ) )

   ELSEIF _HMG_RPTDATA[ 163 ] == .T. // Saving as HTML

      _HMG_RPTDATA[ 149 ] += '<div style="left:' + AllTrim( Str( nFromCol ) ) + 'mm;top:' +  AllTrim( Str( nFromRow + nOffset ) ) +  'mm;width:' +  AllTrim( Str( nToCol - nFromCol ) ) +  'mm;height:0mm;BORDER-STYLE:SOLID;BORDER-COLOR:' + 'rgb(' + AllTrim( Str( aPenColor[ 1 ] ) ) + ',' + AllTrim( Str( aPenColor[ 2 ] ) ) + ',' +  AllTrim( Str( aPenColor[ 3 ] ) ) + ')' + ';BORDER-WIDTH:' + AllTrim( Str( nPenWidth ) ) + 'mm;BACKGROUND-COLOR:#FFFFFF;"><span class="line"></span></DIV>' + Chr( 13 ) + Chr( 10 )

   ENDIF

RETURN

/*
 * STATIC PROCEDURE _PrintRectangle( aObject, nOffset )
 *
 * Prints a rectangle object on the report.
 *
 * Parameters:
 *   aObject (ARRAY): An array containing the rectangle object's properties:
 *     - aObject[2] (NUMERIC): The row coordinate (in millimeters) of the top-left corner.
 *     - aObject[3] (NUMERIC): The column coordinate (in millimeters) of the top-left corner.
 *     - aObject[4] (NUMERIC): The row coordinate (in millimeters) of the bottom-right corner.
 *     - aObject[5] (NUMERIC): The column coordinate (in millimeters) of the bottom-right corner.
 *     - aObject[6] (NUMERIC): The width (in millimeters) of the rectangle's border (pen width).
 *     - aObject[7] (ARRAY): An array containing the RGB color values for the rectangle's border (e.g., {255, 0, 0} for red).
 *   nOffset (NUMERIC): The vertical offset (in millimeters) from the top of the page.
 *
 * Return Value:
 *   None.
 *
 * Purpose:
 *   This procedure prints a rectangle on the report, handling different output formats (printer, PDF, HTML).
 *   It uses different functions depending on the output format.
 *
 * Notes:
 *   For printer output, it uses the _HMG_PRINTER_H_RECTANGLE function.
 *   For PDF output, it uses the pdfBox function to draw four lines to simulate a rectangle.
 *   For HTML output, it generates HTML code to display the rectangle.
 *   The _HMG_RPTDATA array contains report-specific data and settings.
 */
STATIC PROCEDURE _PrintRectangle( aObject, nOffset )
   LOCAL nFromRow  := aObject[ 2 ]
   LOCAL nFromCol  := aObject[ 3 ]
   LOCAL nToRow  := aObject[ 4 ]
   LOCAL nToCol  := aObject[ 5 ]
   LOCAL nPenWidth  := aObject[ 6 ]
   LOCAL aPenColor  := aObject[ 7 ]


   IF _HMG_RPTDATA[ 150 ] == .F. .AND. _HMG_RPTDATA[ 163 ] == .F. // Printing to printer

      _HMG_PRINTER_H_RECTANGLE ( _HMG_PRINTER_HDC, nFromRow + nOffset, nFromCol, nToRow  + nOffset, nToCol, nPenWidth, aPenColor[ 1 ], aPenColor[ 2 ], aPenColor[ 3 ], .T., .T. )

   ELSEIF _HMG_RPTDATA[ 150 ] == .T. // Saving as PDF

      pdfBox( nFromRow + nOffset, nFromCol, nFromRow + nOffset + nPenWidth, nToCol, 0, 1, "M", Chr( 253 ) + Chr( aPenColor[ 1 ] ) + Chr( aPenColor[ 2 ] ) + Chr( aPenColor[ 3 ] ) )
      pdfBox( nToRow + nOffset, nFromCol, nToRow + nOffset + nPenWidth, nToCol, 0, 1, "M", Chr( 253 ) + Chr( aPenColor[ 1 ] ) + Chr( aPenColor[ 2 ] ) + Chr( aPenColor[ 3 ] ) )
      pdfBox( nFromRow + nOffset, nFromCol, nToRow + nOffset, nFromCol + nPenWidth, 0, 1, "M", Chr( 253 ) + Chr( aPenColor[ 1 ] ) + Chr( aPenColor[ 2 ] ) + Chr( aPenColor[ 3 ] ) )
      pdfBox( nFromRow + nOffset, nToCol, nToRow + nOffset, nToCol + nPenWidth, 0, 1, "M", Chr( 253 ) + Chr( aPenColor[ 1 ] ) + Chr( aPenColor[ 2 ] ) + Chr( aPenColor[ 3 ] ) )

   ELSEIF _HMG_RPTDATA[ 163 ] == .T. // Saving as HTML

      _HMG_RPTDATA[ 149 ] += '<div style="left:' + AllTrim( Str( nFromCol ) ) + 'mm;top:' +  AllTrim( Str( nFromRow + nOffset ) ) +  'mm;width:' +  AllTrim( Str( nToCol - nFromCol ) ) +  'mm;height:' + AllTrim( Str( nToRow - nFromRow ) ) + 'mm;BORDER-STYLE:SOLID;BORDER-COLOR:' + 'rgb(' + AllTrim( Str( aPenColor[ 1 ] ) ) + ',' + AllTrim( Str( aPenColor[ 2 ] ) ) + ',' +  AllTrim( Str( aPenColor[ 3 ] ) ) + ')' + ';BORDER-WIDTH:' + AllTrim( Str( nPenWidth ) ) + 'mm;BACKGROUND-COLOR:#FFFFFF;"><span class="line"></span></DIV>' + Chr( 13 ) + Chr( 10 )

   ENDIF

RETURN

/*
 * PROCEDURE _BeginLine()
 *
 * Initializes the properties for a line object before defining its attributes.
 *
 * Purpose:
 *   This procedure is called at the beginning of a "LINE" block in the report definition.
 *   It resets the relevant properties in the _HMG_RPTDATA array to their default values,
 *   preparing the system to store the line's attributes (coordinates, pen width, color).
 *   This ensures that any previous line definitions do not interfere with the current one.
 */
PROCEDURE _BeginLine

   _HMG_RPTDATA[ 110 ] := 0  // FromRow
   _HMG_RPTDATA[ 111 ] := 0  // FromCol
   _HMG_RPTDATA[ 112 ] := 0  // ToRow
   _HMG_RPTDATA[ 113 ] := 0  // ToCol
   _HMG_RPTDATA[ 114 ] := 1  // PenWidth
   _HMG_RPTDATA[ 115 ] := { 0, 0, 0 } // PenColor

RETURN

/*
 * PROCEDURE _EndLine()
 *
 * Finalizes the definition of a line object and adds it to the appropriate section of the report.
 *
 * Purpose:
 *   This procedure is called at the end of a "LINE" block in the report definition.
 *   It retrieves the line's properties (coordinates, pen width, color) from the _HMG_RPTDATA array,
 *   creates an array representing the line object, and adds this array to the appropriate section
 *   of the report (header, detail, footer, summary, group header, or group footer) based on the
 *   current report section being defined (_HMG_RPTDATA[161]).
 */
PROCEDURE _EndLine

   LOCAL aLine

   aLine := {     ;
      'LINE', ;
      _HMG_RPTDATA[ 110 ], ;
      _HMG_RPTDATA[ 111 ], ;
      _HMG_RPTDATA[ 112 ], ;
      _HMG_RPTDATA[ 113 ], ;
      _HMG_RPTDATA[ 114 ], ;
      _HMG_RPTDATA[ 115 ]   ;
      }

   IF _HMG_RPTDATA[ 161 ] == 'HEADER'

      AAdd (  _HMG_RPTDATA[ 160 ], aLine )

   ELSEIF _HMG_RPTDATA[ 161 ] == 'DETAIL'

      AAdd ( _HMG_RPTDATA[ 158 ], aLine )

   ELSEIF _HMG_RPTDATA[ 161 ] == 'FOOTER'

      AAdd ( _HMG_RPTDATA[ 157 ], aLine )

   ELSEIF _HMG_RPTDATA[ 161 ] == 'SUMMARY'

      AAdd ( _HMG_RPTDATA[ 126 ], aLine )

   ELSEIF _HMG_RPTDATA[ 161 ] == 'GROUPHEADER'

      AAdd ( _HMG_RPTDATA[ 121 ], aLine )

   ELSEIF _HMG_RPTDATA[ 161 ] == 'GROUPFOOTER'

      AAdd ( _HMG_RPTDATA[ 122 ], aLine )

   ENDIF

RETURN

/*
 * PROCEDURE _BeginImage()
 *
 * Initializes the properties for an image object before defining its attributes.
 *
 * Purpose:
 *   This procedure is called at the beginning of an "IMAGE" block in the report definition.
 *   It resets the relevant properties (value, row, column, width, height, stretch) to their default values,
 *   preparing the system to store the image's attributes. This ensures that any previous image
 *   definitions do not interfere with the current one.  It uses global variables (_HMG_ActiveControlValue, etc.)
 *   to store the image properties during the definition process.
 */
PROCEDURE _BeginImage

   _HMG_ActiveControlValue := ''   // Value
   _HMG_ActiveControlRow := 0   // Row
   _HMG_ActiveControlCol := 0   // Col
   _HMG_ActiveControlWidth := 0   // Width
   _HMG_ActiveControlHeight := 0   // Height
   _HMG_ActiveControlStretch := .F.  // Stretch

RETURN

/*
 * PROCEDURE _EndImage()
 *
 * Finalizes the definition of an image object and adds it to the appropriate section of the report.
 *
 * Purpose:
 *   This procedure is called at the end of an "IMAGE" block in the report definition.
 *   It retrieves the image's properties (value, row, column, width, height, stretch) from the global variables,
 *   creates an array representing the image object, and adds this array to the appropriate section
 *   of the report (header, detail, footer, summary, group header, or group footer) based on the
 *   current report section being defined (_HMG_RPTDATA[161]).
 */
PROCEDURE _EndImage

   LOCAL aImage

   aImage := {      ;
      'IMAGE', ;
      _HMG_ActiveControlValue, ;
      _HMG_ActiveControlRow, ;
      _HMG_ActiveControlCol, ;
      _HMG_ActiveControlWidth, ;
      _HMG_ActiveControlHeight, ;
      _HMG_ActiveControlStretch   ;
      }

   IF _HMG_RPTDATA[ 161 ] == 'HEADER'

      AAdd (  _HMG_RPTDATA[ 160 ], aImage )

   ELSEIF _HMG_RPTDATA[ 161 ] == 'DETAIL'

      AAdd ( _HMG_RPTDATA[ 158 ], aImage )

   ELSEIF _HMG_RPTDATA[ 161 ] == 'FOOTER'

      AAdd ( _HMG_RPTDATA[ 157 ], aImage )

   ELSEIF _HMG_RPTDATA[ 161 ] == 'SUMMARY'

      AAdd ( _HMG_RPTDATA[ 126 ], aImage )

   ELSEIF _HMG_RPTDATA[ 161 ] == 'GROUPHEADER'

      AAdd ( _HMG_RPTDATA[ 121 ], aImage )

   ELSEIF _HMG_RPTDATA[ 161 ] == 'GROUPFOOTER'

      AAdd ( _HMG_RPTDATA[ 122 ], aImage )

   ENDIF

RETURN

/*
 * PROCEDURE _BeginRectangle()
 *
 * Initializes the properties for a rectangle object before defining its attributes.
 *
 * Purpose:
 *   This procedure is called at the beginning of a "RECTANGLE" block in the report definition.
 *   It resets the relevant properties in the _HMG_RPTDATA array to their default values,
 *   preparing the system to store the rectangle's attributes (coordinates, pen width, color).
 *   This ensures that any previous rectangle definitions do not interfere with the current one.
 */
PROCEDURE _BeginRectangle

   _HMG_RPTDATA[ 110 ] := 0  // FromRow
   _HMG_RPTDATA[ 111 ] := 0  // FromCol
   _HMG_RPTDATA[ 112 ] := 0  // ToRow
   _HMG_RPTDATA[ 113 ] := 0  // ToCol
   _HMG_RPTDATA[ 114 ] := 1  // PenWidth
   _HMG_RPTDATA[ 115 ] := { 0, 0, 0 } // PenColor

RETURN

/*
 * PROCEDURE _EndRectangle()
 *
 * Finalizes the definition of a rectangle object and adds it to the appropriate section of the report.
 *
 * Purpose:
 *   This procedure is called at the end of a "RECTANGLE" block in the report definition.
 *   It retrieves the rectangle's properties (coordinates, pen width, color) from the _HMG_RPTDATA array,
 *   creates an array representing the rectangle object, and adds this array to the appropriate section
 *   of the report (header, detail, footer, summary, group header, or group footer) based on the
 *   current report section being defined (_HMG_RPTDATA[161]).
 */
PROCEDURE _EndRectangle

   LOCAL aRectangle

   aRectangle := {     ;
      'RECTANGLE', ;
      _HMG_RPTDATA[ 110 ], ;
      _HMG_RPTDATA[ 111 ], ;
      _HMG_RPTDATA[ 112 ], ;
      _HMG_RPTDATA[ 113 ], ;
      _HMG_RPTDATA[ 114 ], ;
      _HMG_RPTDATA[ 115 ]   ;
      }

   IF _HMG_RPTDATA[ 161 ] == 'HEADER'

      AAdd (  _HMG_RPTDATA[ 160 ], aRectangle )

   ELSEIF _HMG_RPTDATA[ 161 ] == 'DETAIL'

      AAdd ( _HMG_RPTDATA[ 158 ], aRectangle )

   ELSEIF _HMG_RPTDATA[ 161 ] == 'FOOTER'

      AAdd ( _HMG_RPTDATA[ 157 ], aRectangle )

   ELSEIF _HMG_RPTDATA[ 161 ] == 'SUMMARY'

      AAdd ( _HMG_RPTDATA[ 126 ], aRectangle )

   ELSEIF _HMG_RPTDATA[ 161 ] == 'GROUPHEADER'

      AAdd ( _HMG_RPTDATA[ 121 ], aRectangle )

   ELSEIF _HMG_RPTDATA[ 161 ] == 'GROUPFOOTER'

      AAdd ( _HMG_RPTDATA[ 122 ], aRectangle )

   ENDIF

RETURN

/*
 * PROCEDURE _BeginGroup()
 *
 * Marks the beginning of a group definition in the report.
 *
 * Purpose:
 *   This procedure is called at the beginning of a "GROUP" block in the report definition.
 *   It sets the _HMG_RPTDATA[161] flag to 'GROUP', indicating that the following elements belong to a group.
 *   It also increments the group counter (_HMG_RPTDATA[120]), which is used to track the number of groups in the report.
 */
PROCEDURE _BeginGroup()

   _HMG_RPTDATA[ 161 ] := 'GROUP'

   _HMG_RPTDATA[ 120 ]++

RETURN

/*
 * PROCEDURE _EndGroup()
 *
 * Marks the end of a group definition within a report.
 *
 * Purpose:
 *   This procedure is called when the report engine encounters the end of a "GROUP" block in the report definition.
 *   It essentially resets any group-specific flags or counters that were set by _BeginGroup().  In this specific implementation, it does nothing.
 *   It's important for maintaining the correct state of the report engine as it processes the report definition.
 *
 * Notes:
 *   While this procedure currently does nothing, it's included for completeness and to provide a placeholder for future functionality related to group ending.
 *   It's good practice to have corresponding begin and end procedures for structured blocks like groups.
 */
PROCEDURE _EndGroup()

RETURN

/*
 * PROCEDURE _BeginGroupHeader()
 *
 * Marks the beginning of a group header section within a report.
 *
 * Purpose:
 *   This procedure is called when the report engine encounters the beginning of a "GROUPHEADER" block in the report definition.
 *   It sets the _HMG_RPTDATA[161] element to 'GROUPHEADER', indicating that subsequent report elements belong to the group header section.
 *   This allows the report engine to apply specific formatting or processing rules to the group header.
 *
 * Notes:
 *   The group header is typically used to display information about the group, such as the group name or summary statistics.
 */
PROCEDURE _BeginGroupHeader()

   _HMG_RPTDATA[ 161 ] := 'GROUPHEADER'

RETURN

/*
 * PROCEDURE _EndGroupHeader()
 *
 * Marks the end of a group header section within a report.
 *
 * Purpose:
 *   This procedure is called when the report engine encounters the end of a "GROUPHEADER" block in the report definition.
 *   It essentially resets any group header-specific flags or counters that were set by _BeginGroupHeader(). In this specific implementation, it does nothing.
 *   It's important for maintaining the correct state of the report engine as it processes the report definition.
 *
 * Notes:
 *   While this procedure currently does nothing, it's included for completeness and to provide a placeholder for future functionality related to group header ending.
 */
PROCEDURE _EndGroupHeader()

RETURN

/*
 * PROCEDURE _BeginGroupFooter()
 *
 * Marks the beginning of a group footer section within a report.
 *
 * Purpose:
 *   This procedure is called when the report engine encounters the beginning of a "GROUPFOOTER" block in the report definition.
 *   It sets the _HMG_RPTDATA[161] element to 'GROUPFOOTER', indicating that subsequent report elements belong to the group footer section.
 *   This allows the report engine to apply specific formatting or processing rules to the group footer.
 *
 * Notes:
 *   The group footer is typically used to display summary information about the group, such as totals or averages.
 */
PROCEDURE _BeginGroupFooter()

   _HMG_RPTDATA[ 161 ] := 'GROUPFOOTER'

RETURN

/*
 * PROCEDURE _EndGroupFooter()
 *
 * Marks the end of a group footer section within a report.
 *
 * Purpose:
 *   This procedure is called when the report engine encounters the end of a "GROUPFOOTER" block in the report definition.
 *   It essentially resets any group footer-specific flags or counters that were set by _BeginGroupFooter(). In this specific implementation, it does nothing.
 *   It's important for maintaining the correct state of the report engine as it processes the report definition.
 *
 * Notes:
 *   While this procedure currently does nothing, it's included for completeness and to provide a placeholder for future functionality related to group footer ending.
 */
PROCEDURE _EndGroupFooter()

RETURN

/*
 * FUNCTION _dbSum( cField )
 *
 * Calculates the sum of a numeric field in the currently selected database work area.
 *
 * Parameters:
 *   cField (STRING): The name of the numeric field to sum.
 *
 * Return Value:
 *   NUMERIC: The sum of the specified field in the current database. Returns 0 if the field is not numeric.
 *
 * Purpose:
 *   This function provides a convenient way to calculate the sum of a field in a database.
 *   It dynamically constructs a SUM command using the provided field name and stores the result in a local variable.
 *   This is useful for calculating totals and subtotals in reports or other data analysis tasks.
 *
 * Notes:
 *   The function uses the &() operator for macro substitution, which allows it to dynamically construct the SUM command.
 *   The function assumes that the specified field exists in the currently selected database work area.
 *   If the field is not numeric, the function returns 0.
 */
FUNCTION _dbSum( cField )
   LOCAL nVar

   IF Type ( cField ) == 'N'
      SUM &( cField ) TO nVar
      RETURN nVar
   ENDIF

RETURN 0

/*
 * PROCEDURE _BeginData()
 *
 * Marks the beginning of the data section within a report.
 *
 * Purpose:
 *   This procedure is called when the report engine encounters the beginning of the data section in the report definition.
 *   In this specific implementation, it does nothing. It might be used in the future to perform initialization tasks before processing the data records.
 *   It's included for completeness and to provide a placeholder for future functionality.
 */
PROCEDURE _BeginData()

RETURN

/*
 * PROCEDURE _EndData()
 *
 * Marks the end of the data section within a report.
 *
 * Purpose:
 *   This procedure is called when the report engine encounters the end of the data section in the report definition.
 *   In this specific implementation, it does nothing. It might be used in the future to perform cleanup tasks after processing the data records.
 *   It's included for completeness and to provide a placeholder for future functionality.
 */
PROCEDURE _EndData()

RETURN

#endif
