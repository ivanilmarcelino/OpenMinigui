
#include "MiniGUI.ch"


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//
// PrintRawFile (cPrinter, cFileName, nPaper, nOrient, nFSize)
//
// Purpose:  Print any basic text file
//
// Params:  <cPrinter>   Printer name
// -> If NIL then default printer is assumed
// -> If "" then selection dialog will appear
// <cFileName>   Document to be printed
// <nPaper>   Paper size
// -> Default to DMPAPER_LETTER
// <nOrient>   Paper orientation
// -> Default to DMORIENT_PORTRAIT
// <nFSize>   Font size
// -> Best result with 8-12 font size
// -> Default size to 9 pts
//
// Notes:  Font is fixed to "Courier New"
// Will skip pages if longer than maximum rows allowed
// Will wrap lines if longer than maximun columns allowed
// Will calculate maximum of characters to print according
// to nPaper, nOrient and FSize
//
// Returns:  Nothing
//
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

FUNCTION PrintRawFile ( cPrinter, cFileName, nPaper, nOrient, nFSize )

   LOCAL cMessage := ""
   LOCAL lNewPage
   LOCAL nCurLine
   LOCAL cLine
   LOCAL cSubLine
   LOCAL nTokenPos
   LOCAL oText
   LOCAL oPrnt

   nPaper := iif( nPaper == NIL, 1, nPaper ) // Default to LETTER
   nOrient := iif( nOrient == NIL, 1, nOrient ) // Default to PORTRAIT
   nFSize := iif( nFSize == NIL, 9, nFSize ) // Default to 9

   DO CASE
   CASE cPrinter == NIL
      cPrinter := GetDefaultPrinter()
   CASE Empty( cFileName )
      cMessage := "File to print not specified !"
   CASE hb_FNameName( cFileName ) == ""
      cMessage := "File name to print is missing !"
   CASE hb_FNameExt( cFileName ) == ""
      cMessage := "File extention to print is missing !"
   case ! File( cFileName )
      cMessage := "Could not find file «" + cFileName + "» !"
   ENDCASE
   if ! Empty( cMessage )
      MsgStop( cMessage, "Error" )
      RETURN ( NIL )
   ENDIF

   oText := TFileRead():New( cFileName )
   oText:Open()
   DO CASE
   case ! oText:IsOpen()
      cMessage := "Cannot open file «" + cFileName + "» !"
   CASE oText:Error()
      cMessage := oText:ErrorMsg( "FileRead: " )
   ENDCASE
   if ! Empty( cMessage )
      MsgStop( cMessage, "Error" )
      oText:Close()
      RETURN ( NIL )
   ENDIF

   oPrnt := HBPrinter():New()
   oPrnt:SelectPrinter( cPrinter )
   IF Empty( oPrnt:PrinterName )
      RETURN ( NIL )
   ENDIF

   oPrnt:SetDevMode( 0x00000002, nPaper ) // DM_PAPERSIZE
   IF nOrient == 2
      oPrnt:SetDevMode( 0x00000001, 2 ) // DM_ORIENTATION, DMORIENT_LANDSCAPE
   ELSE
      oPrnt:SetDevMode( 0x00000001, 1 ) // DM_ORIENTATION, DMORIENT_PORTRAIT
   ENDIF
   oPrnt:DefineFont( "f0", "Courier New", nFSize )
   oPrnt:SelectFont( "f0" )
   oPrnt:MaxRow := ( oPrnt:MaxRow - 1 )
   oPrnt:MaxCol := Int( ( oPrnt:MaxCol - 1 ) * ( 10 / nFSize ) )

   lNewPage := .T.
   oPrnt:StartDoc( cFileName )
   DO WHILE oText:MoreToRead()
      IF lNewPage
         oPrnt:StartPage()
         nCurLine := 1
         lNewPage := .F.
      ENDIF

      cLine := oText:ReadLine()
      DO WHILE Len( cLine ) > oPrnt:MaxCol
         cSubLine := Left( cLine, oPrnt:MaxCol )
         nTokenPos := AtToken( cSubLine )
         IF nTokenPos > 1
            cSubLine := Left( cSubLine, nTokenPos - 1 )
         ENDIF
         cLine := SubStr( cLine, Len( cSubLine ) + 1 )
         oPrnt:Say( nCurLine, 1, cSubLine, "f0" )
         ++nCurLine
      ENDDO
      oPrnt:Say( nCurLine, 1, cLine, "f0" )
      ++nCurLine

      IF nCurLine >= oPrnt:MaxRow
         oPrnt:EndPage()
         lNewPage := .T.
      ENDIF
   ENDDO

   oPrnt:EndPage()
   oPrnt:EndDoc()
   oPrnt:End()

   oText:Close()

RETURN ( NIL )
