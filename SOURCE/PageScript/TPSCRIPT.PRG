// ----------------------------------------------------------------------------
// Copyright (c) 2000-2022 Pagescript32.com.  ALL RIGHTS RESERVED.
// ----------------------------------------------------------------------------
// This software is provided "AS IS" without warranty of any kind. The entire
// risk as to the quality and performance of this software is with the purchaser.
// If this software proves defective or inadequate, purchaser assumes the entire
// cost of servicing or repair.

/*-----------------------------------------------------------------------------
Coordinate system used by PageScript 32 (defaults to APS_TOPLEFT)

            nLeft
            |
 --- nTop --+--------------+
            |              |
            |              |
            |              |
            +--------------+-- nBottom
                           |
                           nRight

For example, to draw this rectangle, you would call PSFrameEx() with the following
coordinates : PSFrameEx(nTop, nLeft, nBottom, nRight, ...)

if you change the coordinate system to APS_LEFTTOP then you would use the following
coordinates : PSFrameEx(nLeft, nTop, nRight, nBottom, ...)
------------------------------------------------------------------------------*/
#include "PScript.ch"
#include "FileIO.ch"

#ifdef __XHARBOUR__
// 02.06.2005 - IBTC - changed for xHarbour Builder:
#xtranslate Method <Classname>:<x> => Method <x>
#define DLL_STDCALL NIL

#include "hbclass.ch"

#else

#ifdef __HARBOUR__
# xtranslate Method <Classname>:<x> => Method <x>
# include "hbclass.ch"
# include "Dll.ch"
#else
# include "Xbp.ch"
# include "Dll.ch"
#endif

#endif

// Windows constants for GetDeviceCaps WinApi call (Not complete)
#define HORZSIZE             4         // Horizontal size in millimeters
#define VERTSIZE             6         // Vertical size in millimeters
#define HORZRES              8         // Horizontal width in pixels
#define VERTRES             10         // Vertical height in pixels
#define BITSPIXEL           12         // Number of bits per pixel
#define NUMFONTS            22         // Number of fonts the device has
#define NUMCOLORS           24         // Number of colors the device supports
#define ASPECTX             40         // Length of the X leg
#define ASPECTY             42         // Length of the Y leg
#define LOGPIXELSX          88         // Logical pixelsinch in X
#define LOGPIXELSY          90         // Logical pixelsinch in Y
#define PHYSICALWIDTH      110         // Physical Width in device units
#define PHYSICALHEIGHT     111         // Physical Height in device units
#define PHYSICALOFFSETX    112         // Physical Printable Area x margin
#define PHYSICALOFFSETY    113         // Physical Printable Area y margin
#define SCALINGFACTORX     114         // Scaling factor x
#define SCALINGFACTORY     115         // Scaling factor y

#define pfBUFFERLEN       1024         // Print file buffer length

#define dtRAW                1         // Document Type RAW
#define dtEMULATION          2         // Document Type EMULATION

STATIC oPScript // Holds a generic TPageScript object
STATIC slInitialized := .F. // True when PageScript is initialized

/*-----------------------------------------------------------------------------
Class .....: #TPageScript(...)
Description: PageScript for (x)Harbour and xBase++ wrapper class
By ........: Stephan St-Denis
Date ......: March 2005
-----------------------------------------------------------------------------*/
CLASS TPageScript

   PROTECTED:

   VAR hDll // Handle to PScript.dll
   VAR aPrinters // Array of available printers
   VAR bWaterMark // Codeblock for Watermark function
   VAR nWaterMark // Indicates if the WaterMark is to be printed in foreground or background
   VAR lFromDialog // Indicates if printer selected from Dialog
   VAR lClipper // Indicates if calls to TextOut and TextBox should be compatible with PageScript for Clipper
   VAR aDocInfo // Print job info array

   METHOD Buffer2String // Internal function. Returns a string from a buffer

   EXPORTED:

   // Numerics
   VAR nError // Error condition. 0 = OK
   VAR nUnit // Current UNIT of measurment

   METHOD Init // Class initialization

   METHOD Abort // Abort the current document
   METHOD BarCode // Print a barcode in either 3 of 9 or 128B
   METHOD QRCode // Print QRCode
   METHOD begindoc // Starts a new WINDOWS print job
   METHOD BeginDocEx // Starts a new WINDOWS print job, no parameters
   METHOD BeginEmuDoc // Starts a new EMULATION print job
   METHOD BeginRawDoc // Starts a new RAW print job
   METHOD Bitmap // Draw a bitmap on the document. .BMP, .JPG and .GIF files are supported.
   METHOD Ellipse // Draw an ellipse
   METHOD EllipseEx // Draw an ellipse
   METHOD EndDoc // Ends a WINDOWS print job
   METHOD EndEmuDoc // Ends an EMULATION print job
   METHOD EndRawDoc // Ends a RAW print job
   METHOD FindPrinter // Search for a printer name and returns its position within the array
   METHOD Frame // Draw a frame
   METHOD FrameEx // Draw a frame

   // Gets
   METHOD GetAsciiToAnsi // Retreive the current conversion value
   METHOD GetBorderColor // Gets the current border color
   METHOD GetBorderThickness // Gets the border thickness
   METHOD GetCoorSystem // Gets the actual coordinate system
   METHOD GetCopies // Gets the number of copies
   METHOD GetCPI // Gets the current CPI
   METHOD GetDecimalSep // Gets the current Ascii to Ansi flag
   METHOD GetDefaultPrinter // Gets the current printer number
   METHOD GetDuplex // Gets the current Duplex mode
   METHOD GetFillColor // Gets the current fill color
   METHOD GetFillPattern // Gets the current fill pattern
   METHOD GetFonts // Returns an array with all available font names
   METHOD GetFontAngle // Gets the current font angle
   METHOD GetFontCount // Gets the count of available fonts
   METHOD GetFontName // Gets the current font name
   METHOD GetFontNames // Returns the name of a font from its index position
   METHOD GetFontJustify // Gets the current text justification
   METHOD GetFontStyle // Gets the current font style
   METHOD GetFontSize // Gets the current font size
   METHOD GetFontFColor // Gets the current font foreground color (text color)
   METHOD GetFontBColor // Gets the current font background color (bounding box color)
   METHOD GetJustify // Gets the current text justification
   METHOD GetLPI // Gets the current LPI
   METHOD GetMaxHeight // Gets the maximum height of the page in the selected unit
   METHOD GetMaxWidth // Gets the maximum width of the page in the selected unit
   METHOD GetOrientation // Gets the current document's orientation
   METHOD GetPageSize // Gets the printer paper size
   METHOD GetPaperBin // Gets the current printer paper bin

   METHOD GetPaperBinCount // Gets the paper bin's count
   METHOD GetPaperBinNames // Gets the name of any of the available paper bin
   METHOD GetPaperBinNumbers // Gets the number of any of the available paper bin

   METHOD GetPaperCount // Gets the count of paper formats available for the selected printer
   METHOD GetPaperNames // Gets the name of any of the paper formats, based on its index position
   METHOD GetPaperNumbers // Gets the number of any of the paper formats, based on its index position

   METHOD GetPrinter // Returns the number of the currently selected printer
   METHOD GetPrinters // Returns the list of available printers for this computer
   METHOD GetPrinterCaps // Retreive capabilities of the selected printer
   METHOD GetPrinterCapsEx // Retreive one of the capabilities of the selected printer
   METHOD GetPrinterCount // Retreive the number of available printers
   METHOD GetPrinterHandle // Returns handle of the selected printer for direct Win API calls
   METHOD GetPrinterNames // Returns the name of the selected printer
   METHOD GetTextHeight // Returns the text height in the selected unit
   METHOD GetTextWidth // Returns the text width in the selected unit
   METHOD GetTitle // Gets the document's title
   METHOD GetUnit // Gets the current unit
   METHOD GetUseDIB // Returns the DIB flag
   METHOD GetVersion // Returns the .DLL version number
   METHOD GetXerox // Returns the Xerox flag

   METHOD IsPreviewVisible // Returns .t. is print preview window is visible, otherwise, returns .f.

   METHOD Line // Draw a line
   METHOD LineEx // Draw a line
   METHOD NewPage // Signals the end of page, page eject
   METHOD PrintDialog // Shows the printer dialog and returns if user clicked OK or Cancel
   METHOD PrintEmuFile // Prints an EMULATION file
   METHOD PrintRawFile // Prints a RAW file

   // Sets
   METHOD SetAsciiToAnsi // Set the conversion from Ascii to Ansi for text strings.
   METHOD SetBorder // Set the attributes used to draw lines and borders
   METHOD SetBorderColor // Set the border color
   METHOD SetBorderThickness // Set the border thickness
   METHOD SetClipperComp // Set Clipper compatible calls for TextOut() and TextBox()
   METHOD SetCoorSystem // Set the coordinate system (xbase like: Top/Left or Windows like: Left/Top)
   METHOD SetCPI // Set the number of CPI (APS_TEXT only)
   METHOD SetCopies // Set the number of copies
   METHOD SetDecimalSep // Set the decimal separator character
   METHOD SetDevice // Set the device to use (see PSCRIPT.CH for device contstant definitions)
   METHOD SetDirectPrint // Set the direct print flag (used for very special cases)
   METHOD SetDuplex // Set the Duplex mode
   METHOD SetFileName // Set the file name for a PDF file document
   METHOD SetFill // Set the color and/or pattern used to fill text and shapes, like boxes and ellipes
   METHOD SetFillColor // Set the color used to fill text and shapes, like boxes and ellipes
   METHOD SetFillPattern // Set the Pattern used to fill text and shapes, like boxes and ellipes
   METHOD SetFontAttributes // Set the font attributes used by each call to PSTextOut()
   METHOD SetFontAngle // Set the font attributes used by each call to PSTextOut()
   METHOD SetFontBColor // Set the font attributes used by each call to PSTextOut()
   METHOD SetFontFColor // Set the font attributes used by each call to PSTextOut()
   METHOD SetFontName // Set the font attributes used by each call to PSTextOut()
   METHOD SetFontSize // Set the font attributes used by each call to PSTextOut()
   METHOD SetFontStyle // Set the font attributes used by each call to PSTextOut()
   METHOD SetJustify // Set justification for TextOut
   METHOD SetLPI // Set the number of LPI
   METHOD SetOrientation // Set the paper orientation
   METHOD SetPageSize // Set the page size to a user defined ou predifined size
   METHOD SetPaperBin // Set the paper bin

   METHOD SetPDFCharSet // Set the PDF text charset // not implemented yet
   METHOD SetPDFOwnerPassword // Set the encryption password for PDF documents
   METHOD SetPDFEncoding // Set the PDF encoding to be used
   METHOD SetPDFVersion // Set the PDF versionnumber to be used
   METHOD SetPDFEmbeddedFonts // Set the PDF embedding type
   METHOD ShowPDF // Show PDF after creation in default PDF reader

   METHOD SetPWState // Set the print preview window state
   METHOD SetPWPosition // Set the print preview window position
   METHOD SetPWSize // Set the print preview window size
   METHOD SetPWBounds // Set the print preview window bounds (position and size at the same time)
   METHOD SetPWZoomLevel // Set the print preview window zoom level
   METHOD SetPWColors // Set the print preview window colors

   METHOD SetPrinter // Set the er number
   METHOD SetRowCol // Set the number of rows and columns when printing using the APS_CLIP unit system
   METHOD SetTitle // Set the title of the document
   METHOD SetUnit // Set the unit used to calculate dimensions and placement
   METHOD SetUseDIB // Set the use of compatible bitmaps printing using DIB
   METHOD SetXerox // Set Xerox compatible mode for Xerox WorkCenter and other faulty printers

   METHOD TextBox // Prints a string in a box at position X1,Y1, X2,Y2 using parameters
   METHOD TextBoxEx // Prints a string in a box at position X1,Y1, X2,Y2 using parameters
   METHOD TextOut // Prints a string at position X,Y using parameters
   METHOD TextOutEx // Prints a string at position X,Y using parameters
   METHOD WaterMark // Sets/Returns the current WaterMark

   METHOD UnicodeChar // Prints a Unicode character at position X,Y using parameters
   METHOD UnicodeCharEx
   METHOD SignPDF // Digitally sign PDF
   METHOD PrintPDF // Prints PDF file
   METHOD PDF2Text // Export PDF to txt file
   METHOD ActiveHyperLinks // activate hyperlinks URL mailto
   METHOD SetImageQuality // default 50% sets image quality of PDF images

ENDCLASS

/*-----------------------------------------------------------------------------
Method .....: #Init()
Description : Class constructor
-----------------------------------------------------------------------------*/
METHOD TPageScript:Init()

   ::bWaterMark := NIL // Codeblock for Watermark function
   ::nWaterMark := AWM_NONE // Indicates if the WaterMark is to be printed foreground or background
   ::lFromDialog := .F. // Indicates if printer selected from printer dialog
   ::lClipper := .F. // Indicates if calls to TextOut and TextBox should be compatible with PageScript for Clipper
   ::nUnit := APS_MILL // Default unit is MILLIMETERS
   ::aPrinters := {} // List of available printers
   ::nError := PSE_NOERROR // Indicates an error condition, such as no printer available
   ::aDocInfo := { 0, "", "", APS_PORTRAIT, 1, APS_COURIER } // Print job info array

   ::hDll := DllLoad( "PScript.dll" ) // Handle of the .DLL

   IF Empty( ::hDll )
      ::nError := PSE_DLLNOTLOADED // DLL not loaded error
   ELSE
      ::nError := DllCall( ::hDll, DLL_STDCALL, "PSInit" )

      // No error, set the coordinate system to TOP/LEFT (Default)
      IF ::nError == PSE_NOERROR
         DllCall( ::hDll, DLL_STDCALL, "PSSetCoorSystem", APS_TOPLEFT )
      ENDIF
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #Abort() -> Self
Description : Abort the current document
-----------------------------------------------------------------------------*/
METHOD TPageScript:Abort()

   DllCall( ::hDll, DLL_STDCALL, "PSAbort" )
   ::lFromDialog := .F.

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetImageQuality( nPerc ) -> NIL
Description : Set PDF image quality
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetImageQuality( nPerc )

   IF nPerc > 0
      DllCall( ::hDll, DLL_STDCALL, "PSSetImageQuality", nPerc )
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
Method .....: #PDF2Text( cFileName, cOutPutFileName ) -> NIL
Description : Output PDF to text file
-----------------------------------------------------------------------------*/
METHOD TPageScript:PDF2Text( cFileName, cOutPutFileName )

   DllCall( ::hDll, DLL_STDCALL, "PSExport2Txt", cFileName, cOutputFilename )

RETURN NIL

/*-----------------------------------------------------------------------------
Method .....: #ActiveHyperlinks( lActive ) -> NIL
Description : Activate hyperlinks
-----------------------------------------------------------------------------*/
METHOD TPagescript:ActiveHyperlinks( lActive )

   lActive := iif( lActive == NIL, .F., lActive )

   DllCall( ::hDll, DLL_STDCALL, "PSSetHyperlinks", lActive )

RETURN NIL

/*-----------------------------------------------------------------------------
Method .....: #PrintPDF(<c>, <l> ) -> nError
Description : Print a PDFFile /  Copies will be set with PSSetCopies
            : lShowdialog .T. will display printer dialog
            : PSInit() needs to be called before calling this function
Return......: 0 = OK
              1 = File not found
              2 = File not loaded
              3 = Exception
             -1 = Function call problem
------------------------------------------------------------------------------*/
METHOD TPageScript:PrintPDF( cFilename, lShowDialog )

   lShowDialog := iif( lShowDialog == NIL, .F., lShowDialog )

RETURN DllCall( ::hDll, DLL_STDCALL, "PSPrintPDF", cFilename, lShowDialog )

/*-----------------------------------------------------------------------------
Method .....: #QRCode(<n>, <n>, <c>, [<n>]) -> NIL
Description : Print a QRcode
Parameter nVersion : can be any value between 1 and 40 ( Symbolsize 21*21 modules to 177 * 177 )
                     increasing in steps of 4 modules per side.
Parameter nSize    : Size in pixels 100 * 100 by default
Parameter nModuleWidth : size in pixels of the dots in the QRCode
-----------------------------------------------------------------------------*/
METHOD TPageScript:QRCode( nTop, nLeft, cCode, nVersion, nSize, nModuleWidth )

   nVersion := iif( nVersion == NIL, 1, Int( nVersion ) )
   nSize := iif( nSize == NIL, 100, Int( nSize ) )
   nModuleWidth := iif( nModuleWidth == NIL, 4, Int( nModuleWidth ) )

   DllCall( ::hDll, DLL_STDCALL, "PSQRCode", PSFtoI( nTop ), PSFtoI( nLeft ), cCode, nVersion, nSize, nModuleWidth )

RETURN NIL

/*-----------------------------------------------------------------------------
Method .....: #BarCode(<n>, <n>, <c>, [<n>, <n>, <l>, <n>]) -> NIL
Description : Print a barcode with choice of Code 39 and Code 128
-----------------------------------------------------------------------------*/
METHOD TPageScript:BarCode( nTop, nLeft, cCode, nHeight, nThick, lPrintText, nType, lVertical, lUnitMM )

   // Default values
   nHeight := iif( nHeight == NIL, 36, Int( nHeight ) )
   nThick := iif( nThick == NIL, 1, nThick )
   lPrintText := iif( lPrintText == NIL, .T., lPrintText )
   nType := iif( nType == NIL, APS_BC128, Int( nType ) )
   lVertical := iif( lVertical == NIL, .F., lVertical )
   lUnitMM := iif( lUnitMM == NIL, .F., lUnitMM ) // if .T. measurement is in MM

   IF nType == APS_BC39
      cCode := Upper( cCode )
   ENDIF

   DllCall( ::hDll, DLL_STDCALL, "PSBarCode", PSFtoI( nTop ), PSFtoI( nLeft ), cCode, nHeight, PSFtoI( nThick ), lPrintText, nType, lVertical, lUnitMM )

RETURN NIL

/*-----------------------------------------------------------------------------
Method .....: #BeginDoc(<n>, <c>, <n>, <n>, <n>, <n>, <n>) -> lError
Description : Start a new print job using the passed parameters
-----------------------------------------------------------------------------*/
METHOD TPageScript:BeginDoc( nPrinter, cTitle, nOrientation, nCopies, nPageSize, nDuplex, nPaperBin )

   ::SetTitle( cTitle )

   if ! ::lFromDialog
      ::SetPrinter( nPrinter )
      ::SetPageSize( nPageSize )
      ::SetPaperBin( nPaperBin )
      ::SetDuplex( nDuplex )
      ::SetOrientation( nOrientation )
      ::SetCopies( nCopies )
   ENDIF

   DllCall( ::hDll, DLL_STDCALL, "PSBeginDoc" )

   // Put a WaterMark in the BackGround
   IF ::nWaterMark = AWM_BACKGROUND .AND. ValType( ::bWaterMark ) == "B"
      Eval( ::bWaterMark, Self )
   ENDIF

RETURN ::nError

/*-----------------------------------------------------------------------------
Method .....: #BeginDocEx() -> lError
Description : Start a new print job using the defaults values
-----------------------------------------------------------------------------*/
METHOD TPageScript:BeginDocEx()

   DllCall( ::hDll, DLL_STDCALL, "PSBeginDoc" )

   // Put a WaterMark in the BackGround
   IF ::nWaterMark = AWM_BACKGROUND .AND. ValType( ::bWaterMark ) == "B"
      Eval( ::bWaterMark, Self )
   ENDIF

RETURN ::nError

/*-----------------------------------------------------------------------------
Method .....: #BeginEmuDoc(<n>, <c>) -> lError
Description : Start a new EMULATION print job using the passed parameters
-----------------------------------------------------------------------------*/
METHOD TPageScript:BeginEmuDoc( nPrinter, cTitle, nOrientation, nCopies, cFont )

   nOrientation := iif( nOrientation == NIL, APS_PORTRAIT, nOrientation )
   nPrinter := iif( nPrinter == NIL, 0, nPrinter )
   cFont := iif( cFont == NIL, APS_COURIER, cFont )

   ::aDocInfo[ 1 ] := nPrinter
   ::aDocInfo[ 2 ] := cTitle
   ::aDocInfo[ 3 ] := StrTran( Time(), ":", "-" ) + ".PRN"
   ::aDocInfo[ 4 ] := nOrientation
   ::aDocInfo[ 5 ] := nCopies
   ::aDocInfo[ 6 ] := cFont

   SET PRINTER ON
   SET CONSOLE OFF
   Set( _SET_DEVICE, "PRINTER" )
   Set( _SET_PRINTFILE, ::aDocInfo[ 3 ], .F. )

RETURN ::nError

/*-----------------------------------------------------------------------------
Method .....: #BeginRawDoc(<n>, <c>) -> lError
Description : Start a new RAW print job using the passed parameters
-----------------------------------------------------------------------------*/
METHOD TPageScript:BeginRawDoc( nPrinter, cTitle )

   ::aDocInfo[ 1 ] := nPrinter
   ::aDocInfo[ 2 ] := cTitle
   ::aDocInfo[ 3 ] := StrTran( Time(), ":", "-" ) + ".PRN"

   SET PRINTER ON
   SET CONSOLE OFF
   Set( _SET_DEVICE, "PRINTER" )
   Set( _SET_PRINTFILE, ::aDocInfo[ 3 ], .F. )

RETURN ::nError

/*-----------------------------------------------------------------------------
Method .....: #PSBitmap(<n>, <n>, <n>, <n>, <x>, [<n>]) -> Self
Description : Draw a bitmap on the document.
-----------------------------------------------------------------------------*/
METHOD TPageScript:Bitmap( nTop, nLeft, nBottom, nRight, cBitmap, nTransColor, lKeepRatio )

   IF File( cBitmap )
      nBottom := iif( nBottom == NIL, 0, nBottom )
      nRight := iif( nRight == NIL, 0, nRight )
      nTransColor := iif( nTransColor == NIL, APS_NONE, Int( nTransColor ) )
      lKeepRatio := iif( lKeepRatio == NIL, .F., lKeepRatio )

      DllCall( ::hDll, DLL_STDCALL, "PSBitmap", PSFtoI( nTop ), PSFtoI( nLeft ), PSFtoI( nBottom ), PSFtoI( nRight ), cBitmap, nTransColor, iif( lKeepRatio, 1, 0 ) )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #Ellipse(<n>, <n>, <n>, <n>, [<n>, <n>, <n>, <n>]) -> Self
Description : Draw an ellipse
-----------------------------------------------------------------------------*/
METHOD TPageScript:Ellipse( nTop, nLeft, nBottom, nRight, nThick, nBorderColor, nFillColor, nPattern )

   // Sets default values
   nThick := iif( nThick == NIL, APS_DEFAULT, nThick )
   nBorderColor := iif( nBorderColor == NIL, APS_DEFAULT, Int( nBorderColor ) )
   nFillColor := iif( nFillColor == NIL, APS_DEFAULT, Int( nFillColor ) )
   nPattern := iif( nPattern == NIL, APS_DEFAULT, Int( nPattern ) )

   DllCall( ::hDll, DLL_STDCALL, "PSEllipseEx", PSFtoI( nTop ), PSFtoI( nLeft ), PSFtoI( nBottom ), PSFtoI( nRight ), PSFtoI( nThick ), nBorderColor, nFillColor, nPattern )

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #EllipseEx(<n>, <n>, <n>, <n>) -> Self
Description : Draw an ellipse
-----------------------------------------------------------------------------*/
METHOD TPageScript:EllipseEx( nTop, nLeft, nBottom, nRight )

   DllCall( ::hDll, DLL_STDCALL, "PSEllipse", PSFtoI( nTop ), PSFtoI( nLeft ), PSFtoI( nBottom ), PSFtoI( nRight ) )

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #EndDoc() -> Self
Description : Signal the end of the document (stop spooling)
-----------------------------------------------------------------------------*/
METHOD TPageScript:EndDoc()

   // Put a WaterMark in the ForeGround
   IF ::nWaterMark = AWM_FOREGROUND .AND. ValType( ::bWaterMark ) == "B"
      Eval( ::bWaterMark, Self )
   ENDIF

   DllCall( ::hDll, DLL_STDCALL, "PSEndDoc" )
   ::lFromDialog := .F.

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #EndRawDoc() -> Self
Description : Ends a RAW print job
-----------------------------------------------------------------------------*/
METHOD TPageScript:EndEmuDoc()

   SET CONSOLE ON
   SET PRINTER OFF
   SET PRINTER TO
   Set( _SET_DEVICE, "SCREEN" )
   SetPRC( 0, 0 )

   ::PrintEmuFile( ::aDocInfo[ 3 ], .T., ::aDocInfo[ 1 ], ::aDocInfo[ 2 ], ::aDocInfo[ 4 ], ::aDocInfo[ 5 ], ::aDocInfo[ 6 ] )

   ::aDocInfo := { 0, "", "", APS_PORTRAIT, 1, APS_COURIER }

RETURN NIL

/*-----------------------------------------------------------------------------
Method .....: #EndRawDoc() -> Self
Description : Ends a RAW print job
-----------------------------------------------------------------------------*/
METHOD TPageScript:EndRawDoc()

   SET CONSOLE ON
   SET PRINTER OFF
   SET PRINTER TO
   Set( _SET_DEVICE, "SCREEN" )
   SetPRC( 0, 0 )

   ::PrintRawFile( ::aDocInfo[ 3 ], .T., ::aDocInfo[ 1 ], ::aDocInfo[ 2 ] )

   ::aDocInfo := { 0, "", "", APS_PORTRAIT, 1, APS_COURIER }

RETURN NIL

/*-----------------------------------------------------------------------------
Method .....: #FindPrinter(<c>) -> nPrinter
Description : Finds a printer by name and returns its position in the array
-----------------------------------------------------------------------------*/
METHOD TPageScript:FindPrinter( cPrinter )

   LOCAL nPos := -1
   LOCAL aPrinters := ::GetPrinters()

   // Finds a printer position in the array
   IF ValType( cPrinter ) == "C" .AND. Len( cPrinter ) > 0
      nPos := AScan( aPrinters, {| x | AllTrim( Upper( x ) ) == AllTrim( Upper( cPrinter ) ) }, 1, Len( aPrinters ) )
   ENDIF

RETURN nPos

/*-----------------------------------------------------------------------------
Method .....: #Frame(<n>, <n>, <n>, <n>, [<n>, <n>, <n>, <n>, <n>]) -> Self
Description : Draw a frame
-----------------------------------------------------------------------------*/
METHOD TPageScript:Frame( nTop, nLeft, nBottom, nRight, nThick, nBorderColor, nFillColor, nPattern, nRadius )

   // Sets default values
   nThick := iif( nThick == NIL, APS_DEFAULT, nThick )
   nBorderColor := iif( nBorderColor == NIL, APS_DEFAULT, Int( nBorderColor ) )
   nFillColor := iif( nFillColor == NIL, APS_DEFAULT, Int( nFillColor ) )
   nPattern := iif( nPattern == NIL, APS_DEFAULT, Int( nPattern ) )
   nRadius := iif( nRadius == NIL, 0, Int( nRadius ) )

   DllCall( ::hDll, DLL_STDCALL, "PSFrameEx", PSFtoI( nTop ), PSFtoI( nLeft ), PSFtoI( nBottom ), PSFtoI( nRight ), PSFtoI( nThick ), nBorderColor, nFillColor, nPattern, nRadius )

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #FrameEx(<n>, <n>, <n>, <n>) -> Self
Description : Draw a frame
-----------------------------------------------------------------------------*/
METHOD TPageScript:FrameEx( nTop, nLeft, nBottom, nRight, nRadius )

   nRadius := iif( nRadius == NIL, 0, Int( nRadius ) )

   DllCall( ::hDll, DLL_STDCALL, "PSFrame", PSFtoI( nTop ), PSFtoI( nLeft ), PSFtoI( nBottom ), PSFtoI( nRight ), nRadius )

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #GetAsciiToAnsi() -> lValue
Description : Returns the current setting
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetAsciiToAnsi()

   LOCAL nValue := DllCall( ::hDll, DLL_STDCALL, "PSGetAsciiToAnsi" )

RETURN ( nValue == 1 )

/*-----------------------------------------------------------------------------
Method .....: #GetBorderColor() -> nValue
Description : Returns the current setting
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetBorderColor()

   LOCAL nValue := DllCall( ::hDll, DLL_STDCALL, "PSGetBorderColor" )

RETURN nValue

/*-----------------------------------------------------------------------------
Method .....: #GetBorderThickness() -> nValue
Description : Returns the current setting
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetBorderThickness()

   LOCAL nValue := DllCall( ::hDll, DLL_STDCALL, "PSGetBorderThickness" )

RETURN nValue

/*-----------------------------------------------------------------------------
Method .....: #GetCoorSystem() -> nValue
Description : Returns the current setting
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetCoorSystem()

   LOCAL nValue := DllCall( ::hDll, DLL_STDCALL, "PSGetCoorSystem" )

RETURN nValue

/*-----------------------------------------------------------------------------
Method .....: #GetCopies() -> nValue
Description : Returns the current setting
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetCopies()

   LOCAL nValue := DllCall( ::hDll, DLL_STDCALL, "PSGetCopies" )

RETURN nValue

/*-----------------------------------------------------------------------------
Method .....: #GetCPI() -> nValue
Description : Returns the current setting
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetCPI()

   LOCAL nValue := DllCall( ::hDll, DLL_STDCALL, "PSGetCPI" )

RETURN nValue

/*-----------------------------------------------------------------------------
Method .....: #GetDecimalSep() -> cValue
Description : Returns the current setting
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetDecimalSep()

   LOCAL nValue := DllCall( ::hDll, DLL_STDCALL, "PSGetDecimalSep" )

RETURN Chr( nValue )

/*-----------------------------------------------------------------------------
Method .....: #GetDefaultPrinter() -> nDefaultPrinter
Description : Returns the default Windows printer
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetDefaultPrinter()

   LOCAL nDefaultPrinter := DllCall( ::hDll, DLL_STDCALL, "PSGetDefaultPrinter" )

RETURN nDefaultPrinter + 1

/*-----------------------------------------------------------------------------
Method .....: #GetDuplex() -> nValue
Description : Returns the current setting
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetDuplex()

   LOCAL nValue := DllCall( ::hDll, DLL_STDCALL, "PSGetDuplex" )

RETURN nValue

/*-----------------------------------------------------------------------------
Method .....: #GetFillColor() -> nValue
Description : Returns the current setting
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetFillColor()

   LOCAL nValue := DllCall( ::hDll, DLL_STDCALL, "PSGetFillColor" )

RETURN nValue

/*-----------------------------------------------------------------------------
Method .....: #GetFillPattern() -> nValue
Description : Returns the current setting
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetFillPattern()

   LOCAL nValue := DllCall( ::hDll, DLL_STDCALL, "PSGetFillPattern" )

RETURN nValue

/*-----------------------------------------------------------------------------
Method .....: #GetFonts() -> aFontList
Description : Retreive the printer fonts for the currently selected printer
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetFonts()

   LOCAL aFonts := {}
   LOCAL cFont := ""
   LOCAL nCount := 0
   LOCAL nLoop := 0

   nCount := DllCall( ::hDll, DLL_STDCALL, "PSGetFontCount" )

   IF nCount > 0
      FOR nLoop := 0 TO nCount - 1
         cFont := Space( 128 )
         DllCall( ::hDll, DLL_STDCALL, "PSGetFontNames", nLoop, @cFont )
         AAdd( aFonts, ::Buffer2String( cFont ) )
      NEXT
   ENDIF

RETURN aFonts

/*-----------------------------------------------------------------------------
Method .....: #GetFontAngle() -> nValue
Description : Returns the current font/text angle
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetFontAngle()

   LOCAL nValue := DllCall( ::hDll, DLL_STDCALL, "PSGetFontAngle" )

RETURN nValue

/*-----------------------------------------------------------------------------
Method .....: #GetFontCount() -> nValue
Description : Returns the current font count
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetFontCount()

   LOCAL nValue := DllCall( ::hDll, DLL_STDCALL, "PSGetFontCount" )

RETURN nValue

/*-----------------------------------------------------------------------------
Method .....: #GetFontJustify() -> nValue
Description : Returns the current setting
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetFontJustify()

   LOCAL nValue := DllCall( ::hDll, DLL_STDCALL, "PSGetJustify" )

RETURN nValue

/*-----------------------------------------------------------------------------
Method .....: #GetFontName() -> cValue
Description : Returns the current setting
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetFontName()

   LOCAL cFont := Space( 128 )

   DllCall( ::hDll, DLL_STDCALL, "PSGetFontName", @cFont )

RETURN ::Buffer2String( cFont )

/*-----------------------------------------------------------------------------
Method .....: #GetFontNames() -> aFontList
Description : Retreive the name of the font from its index position
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetFontNames( nFontIndex )

   LOCAL cFontName := ""
   LOCAL nCount

   nCount := DllCall( ::hDll, DLL_STDCALL, "PSGetFontCount" )

   IF nFontIndex >= 1 .AND. nFontIndex <= nCount
      cFontName := Space( 128 )
      DllCall( ::hDll, DLL_STDCALL, "PSGetFontNames", nFontIndex - 1, @cFontName )
   ENDIF

RETURN ::Buffer2String( cFontName )

/*-----------------------------------------------------------------------------
Method .....: #GetFontStyle() -> nValue
Description : Returns the current setting
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetFontStyle()

   LOCAL nValue := DllCall( ::hDll, DLL_STDCALL, "PSGetFontStyle" )

RETURN nValue

/*-----------------------------------------------------------------------------
Method .....: #GetFontSize() -> nValue
Description : Returns the current setting
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetFontSize()

   LOCAL nValue := DllCall( ::hDll, DLL_STDCALL, "PSGetFontSize" )

RETURN PSItoF( nValue )

/*-----------------------------------------------------------------------------
Method .....: #GetFontBColor() -> nValue
Description : Returns the current setting
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetFontBColor()

   LOCAL nValue := DllCall( ::hDll, DLL_STDCALL, "PSGetFontBColor" )

RETURN nValue

/*-----------------------------------------------------------------------------
Method .....: #GetFontFColor() -> nValue
Description : Returns the current setting
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetFontFColor()

   LOCAL nValue := DllCall( ::hDll, DLL_STDCALL, "PSGetFontFColor" )

RETURN nValue

/*-----------------------------------------------------------------------------
Method .....: #GetJustify() -> nValue
Description : Returns the current setting
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetJustify()

   LOCAL nValue := DllCall( ::hDll, DLL_STDCALL, "PSGetJustify" )

RETURN nValue

/*-----------------------------------------------------------------------------
Method .....: #GetLPI() -> nValue
Description : Returns the current setting
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetLPI()

   LOCAL nValue := DllCall( ::hDll, DLL_STDCALL, "PSGetLPI" )

RETURN nValue

/*-----------------------------------------------------------------------------
Method .....: #GetMaxHeight() -> nValue
Description : Returns the max height in current unit
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetMaxHeight()

   LOCAL nValue := DllCall( ::hDll, DLL_STDCALL, "PSGetMaxHeight" )

RETURN PSItoF( nValue )

/*-----------------------------------------------------------------------------
Method .....: #GetMaxWidth() -> nValue
Description : Returns the max width in current unit
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetMaxWidth()

   LOCAL nValue := DllCall( ::hDll, DLL_STDCALL, "PSGetMaxWidth" )

RETURN PSItoF( nValue )

/*-----------------------------------------------------------------------------
Method .....: #GetOrientation() -> nValue
Description : Returns the current setting
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetOrientation()

   LOCAL nValue := DllCall( ::hDll, DLL_STDCALL, "PSGetOrientation" )

RETURN nValue

/*-----------------------------------------------------------------------------
Method .....: #GetPageSize() -> nValue
Description : Retreive the printer paper size
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetPageSize()

   LOCAL nValue := DllCall( ::hDll, DLL_STDCALL, "PSGetPageSize" )

RETURN nValue

/*-----------------------------------------------------------------------------
Method .....: #GetPaperBin() -> nValue
Description : Retreive the currently selected printer paper bin
Note .....: Returns -1 if called while printing
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetPaperBin()

   LOCAL nBin := DllCall( ::hDll, DLL_STDCALL, "PSGetPaperBin" )

RETURN nBin

/*-----------------------------------------------------------------------------
Method .....: #GetPaperBinCount() -> nValue
Description : Retreive the number of paper bins installed
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetPaperBinCount()

   LOCAL nCount := DllCall( ::hDll, DLL_STDCALL, "PSGetPaperBinCount" )

RETURN nCount

/*-----------------------------------------------------------------------------
Method .....: #GetPaperBinNames() -> cValue
Description : Retrieve the name of any of the available paper bin
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetPaperBinNames( nIndex )

   LOCAL cName := ""
   LOCAL nCount := 0

   nCount := DllCall( ::hDll, DLL_STDCALL, "PSGetPaperBinCount" )

   IF nIndex >= 1 .AND. nIndex <= nCount
      cName := Space( 24 )
      DllCall( ::hDll, DLL_STDCALL, "PSGetPaperBinNames", nIndex - 1, @cName )
   ENDIF

RETURN ::Buffer2String( cName )

/*-----------------------------------------------------------------------------
Method .....: #GetPaperBinNumbers() -> nValue
Description : Retrieve the number of any of the available paper bin
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetPaperBinNumbers( nIndex )

   LOCAL nNumber := -1
   LOCAL nCount := 0

   nCount := DllCall( ::hDll, DLL_STDCALL, "PSGetPaperBinCount" )

   IF nIndex >= 1 .AND. nIndex <= nCount
      nNumber := DllCall( ::hDll, DLL_STDCALL, "PSGetPaperBinNumbers", nIndex - 1 )
   ENDIF

RETURN nNumber

/*-----------------------------------------------------------------------------
Method .....: #GetPaperCount() -> nValue
Description : Retreive the number of paper installed
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetPaperCount()

   LOCAL nCount := DllCall( ::hDll, DLL_STDCALL, "PSGetPaperCount" )

RETURN nCount

/*-----------------------------------------------------------------------------
Method .....: #GetPaperNames() -> nValue
Description : Retreive the name of a paper type based on its index position
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetPaperNames( nIndex )

   LOCAL cName := ""
   LOCAL nCount := 0

   nCount := DllCall( ::hDll, DLL_STDCALL, "PSGetPaperCount" )

   IF nIndex >= 1 .AND. nIndex <= nCount
      cName := Space( 64 )
      DllCall( ::hDll, DLL_STDCALL, "PSGetPaperNames", nIndex - 1, @cName )
   ENDIF

RETURN ::Buffer2String( cName )

/*-----------------------------------------------------------------------------
Method .....: #GetPaperNumbers() -> nValue
Description : Retreive the number of the paper type pointed by nIndex
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetPaperNumbers( nIndex )

   LOCAL nNumber := -1
   LOCAL nCount := 0

   nCount := DllCall( ::hDll, DLL_STDCALL, "PSGetPaperCount" )

   IF nIndex >= 1 .AND. nIndex <= nCount
      nNumber := DllCall( ::hDll, DLL_STDCALL, "PSGetPaperNumbers", nIndex - 1 )
   ENDIF

RETURN nNumber

/*-----------------------------------------------------------------------------
Method .....: #GetPrinter() -> nValue
Description : Returns the current setting
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetPrinter()

   LOCAL nValue := DllCall( ::hDll, DLL_STDCALL, "PSGetPrinter" )

RETURN nValue + 1

/*-----------------------------------------------------------------------------
Method .....: #GetPrinterCount() -> nValue
Description : Returns the current setting
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetPrinterCount()

   LOCAL nValue := DllCall( ::hDll, DLL_STDCALL, "PSGetPrinterCount" )

RETURN nValue

/*-----------------------------------------------------------------------------
Method .....: #GetPrinters() -> aClone(saPrinters)
Description : Returns the list of available printers for this computer
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetPrinters()

   LOCAL aPrinters := {}
   LOCAL cPrinter := ""
   LOCAL nCount := 0
   LOCAL nLoop := 0

   nCount := DllCall( ::hDll, DLL_STDCALL, "PSGetPrinterCount" )

   FOR nLoop := 0 TO nCount - 1
      cPrinter := Space( 256 )
      DllCall( ::hDll, DLL_STDCALL, "PSGetPrinterNames", nLoop, @cPrinter )
      AAdd( aPrinters, ::Buffer2String( cPrinter ) )
   NEXT

RETURN AClone( aPrinters )

/*-----------------------------------------------------------------------------
Method .....: #GetPrinterCaps() -> aCaps
Description : Retreive the printer capabilities of the selected printer
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetPrinterCaps()

   LOCAL cDllTemplate
   LOCAL nAreaWidth
   LOCAL nAreaHeight
   LOCAL nTopMargin
   LOCAL nLeftMargin
   LOCAL nPaperWidth
   LOCAL nPaperHeight
   // Local nMAreaWidth
   // Local nMAreaHeight
   LOCAL nHPixelsInch
   LOCAL nVPixelsInch
   LOCAL nBitDepth

   cDllTemplate := DllPrepareCall( ::hDll, DLL_STDCALL, "PSGetPrinterCaps" )

   nAreaWidth := DllExecuteCall( cDllTemplate, HORZRES ) // Printable Horz area (Width)    Pixels
   nAreaHeight := DllExecuteCall( cDllTemplate, VERTRES ) // Printable Vert area (Height)   Pixels
   nTopMargin := DllExecuteCall( cDllTemplate, PHYSICALOFFSETY ) // Top margin                     Pixels
   nLeftMargin := DllExecuteCall( cDllTemplate, PHYSICALOFFSETX ) // Left margin                    Pixels
   nPaperWidth := DllExecuteCall( cDllTemplate, PHYSICALWIDTH ) // Total paper width              Pixels
   nPaperHeight := DllExecuteCall( cDllTemplate, PHYSICALHEIGHT ) // Total paper height             Pixels
   // nMAreaWidth  := DllExecuteCall(cDllTemplate, HORZSIZE) // Printable Horz area            mm
   // nMAreaHeight := DllExecuteCall(cDllTemplate, VERTSIZE) // Printable Vert area            mm
   nHPixelsInch := DllExecuteCall( cDllTemplate, LOGPIXELSX ) // Number of Horz pixels/Inch     Pixels
   nVPixelsInch := DllExecuteCall( cDllTemplate, LOGPIXELSY ) // Number of Vert pixels/Inch     Pixels
   nBitDepth := DllExecuteCall( cDllTemplate, NUMCOLORS ) // Bit depth (number of bits per pixel)

RETURN { nPaperWidth, ;
      nPaperHeight, ;
      nAreaWidth, ;
      nAreaHeight, ;
      nTopMargin, ;
      nLeftMargin, ;
      nHPixelsInch, ;
      nVPixelsInch, ;
      nBitDepth }

/*-----------------------------------------------------------------------------
Method .....: #GetPrinterCapsEx(<n>) -> nCap
Description : Retreive one of the printer capabilities of the selected printer
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetPrinterCapsEx( nCap )

   LOCAL nValue := DllCall( ::hDll, DLL_STDCALL, "PSGetPrinterCaps", nCap )

RETURN nValue

/*-----------------------------------------------------------------------------
Method .....: #GetPrinterHandle() -> nValue
Description : Returns the current setting
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetPrinterHandle()

   LOCAL nValue := DllCall( ::hDll, DLL_STDCALL, "PSGetPrinterHandle" )

RETURN nValue

/*-----------------------------------------------------------------------------
Method .....: #GetPrinterNames(<n>) -> cValue
Description : Returns the name of the selected printer
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetPrinterNames( nPrinter )

   LOCAL cPrinter := Space( 256 )

   DllCall( ::hDll, DLL_STDCALL, "PSGetPrinterNames", nPrinter - 1, @cPrinter )

RETURN ::Buffer2String( cPrinter )

/*-----------------------------------------------------------------------------
Method .....: #GetTitle() -> cValue
Description : Returns the current setting
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetTitle()

   LOCAL cTitle := Space( 256 )

   DllCall( ::hDll, DLL_STDCALL, "PSGetTitle", @cTitle )

RETURN ::Buffer2String( cTitle )

/*-----------------------------------------------------------------------------
Method .....: #Get
Description :
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetTextHeight( cText )

   LOCAL nValue := DllCall( ::hDll, DLL_STDCALL, "PSGetTextHeight", cText )

RETURN PSItoF( nValue )

/*-----------------------------------------------------------------------------
Method .....: #Get
Description :
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetTextWidth( cText )

   LOCAL nValue := DllCall( ::hDll, DLL_STDCALL, "PSGetTextWidth", cText )

RETURN PSItoF( nValue )

/*-----------------------------------------------------------------------------
Method .....: #GetUnit() -> nValue
Description : Returns the current setting
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetUnit()

   LOCAL nValue := DllCall( ::hDll, DLL_STDCALL, "PSGetUnit" )

RETURN nValue

/*-----------------------------------------------------------------------------
Method .....: #GetUseDIB() -> lValue
Description : Returns the current setting
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetUseDIB()

   LOCAL lValue := ( DllCall( ::hDll, DLL_STDCALL, "PSGetUseDIB" ) == 1 )

RETURN lValue

/*-----------------------------------------------------------------------------
Method .....: #Version() -> cVersionNumber
Description : Returns the DLL version number
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetVersion()

   LOCAL cVersion := Space( 24 )

   DllCall( ::hDll, DLL_STDCALL, "PSGetVersion", @cVersion )

RETURN ::Buffer2String( cVersion )

/*-----------------------------------------------------------------------------
Method .....: #GetXerox() -> lValue
Description : Returns the current setting
-----------------------------------------------------------------------------*/
METHOD TPageScript:GetXerox()

   LOCAL lValue := ( DllCall( ::hDll, DLL_STDCALL, "PSGetXerox" ) == 1 )

RETURN lValue

/*-----------------------------------------------------------------------------
Method .....: #IsPreviewVisible() -> lValue
Description : Returns .t. is print preview window is visible, otherwise, returns .f.
-----------------------------------------------------------------------------*/
METHOD TPageScript:IsPreviewVisible()

   LOCAL lValue := ( DllCall( ::hDll, DLL_STDCALL, "PSIsPreviewVisible" ) == 1 )

RETURN lValue

/*-----------------------------------------------------------------------------
Method .....: #Line(<n>, <n>, <n>, <n>, [<n>, <n>]) -> NIL
Description : Draw a line
-----------------------------------------------------------------------------*/
METHOD TPageScript:Line( nTop, nLeft, nBottom, nRight, nThick, nBorderColor )

   nThick := iif( nThick == NIL, APS_DEFAULT, nThick )
   nBorderColor := iif( nBorderColor == NIL, APS_DEFAULT, Int( nBorderColor ) )

   DllCall( ::hDll, DLL_STDCALL, "PSLineEx", PSFtoI( nTop ), PSFtoI( nLeft ), PSFtoI( nBottom ), PSFtoI( nRight ), PSFtoI( nThick ), nBorderColor )

RETURN NIL

/*-----------------------------------------------------------------------------
Method .....: #LineEx(<n>, <n>, <n>, <n>) -> NIL
Description : Draw a line
-----------------------------------------------------------------------------*/
METHOD TPageScript:LineEx( nTop, nLeft, nBottom, nRight )

   DllCall( ::hDll, DLL_STDCALL, "PSLine", PSFtoI( nTop ), PSFtoI( nLeft ), PSFtoI( nBottom ), PSFtoI( nRight ) )

RETURN NIL

/*-----------------------------------------------------------------------------
Method .....: #NewPage() -> NIL
Description : Signal the end of page, page eject
-----------------------------------------------------------------------------*/
METHOD TPageScript:NewPage()

   // Put a WaterMark in the ForeGround
   IF ::nWaterMark = AWM_FOREGROUND .AND. ValType( ::bWaterMark ) == "B"
      Eval( ::bWaterMark, Self )
   ENDIF

   DllCall( ::hDll, DLL_STDCALL, "PSNewPage" )

   // Put a WaterMark in the BackGround
   IF ::nWaterMark = AWM_BACKGROUND .AND. ValType( ::bWaterMark ) == "B"
      Eval( ::bWaterMark, Self )
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
Method .....: #PrintDialog() -> lPrint
Description : Shows the printer dialog and returns if the user clicked OK
-----------------------------------------------------------------------------*/
METHOD TPageScript:PrintDialog()

   LOCAL nValue

   ::lFromDialog := .F.

   nValue := DllCall( ::hDll, DLL_STDCALL, "PSPrintDialog" )

   IF nValue == 1
      ::lFromDialog := .T.
   ENDIF

RETURN ( nValue == 1 ) // 1 == Print, 0 == Error or don't print

/*-----------------------------------------------------------------------------
Function ...: PSPrintEmuFile(<c>, [<l>, <n>, <c>, <n>, <n>, <c>]) -> lSuccess
Description : Print the file pointed by cFile and optionnaly deletes it.
Author .....: Stephan St-Denis
Date .......: September 2002
Notes ......: You're free to enhance this function. If you do so, please let
              us know : tech@abeelabs.com
-----------------------------------------------------------------------------*/
METHOD TPageScript:PrintEmuFile( cFileName, lDelete, nPrinter, cTitle, nOrientation, nCopies, cFont )

   LOCAL lSuccess := .T. // Return value
   LOCAL nStyle := APS_PLAIN // Start with plain font style
   LOCAL nRow := 0 // Print head line
   LOCAL nCol := 0 // Print head column
   LOCAL lBold := .F. // Bold flag
   LOCAL lItalic := .F. // Italic flag
   LOCAL lUnder := .F. // Underline flag
   LOCAL lStrike := .F. // Strikeout flag
   LOCAL cPrintLine // Line to print
   LOCAL cBuffer // File buffer
   LOCAL nLoop // For / Next loop
   LOCAL nCtrl // Control character in ASCII
   LOCAL nChar // Character read
   LOCAL nPos // Pos of CRLF, EJECT or ESCAPE in buffer (in any)
   LOCAL nHandle // File handle
   LOCAL nBytes // Number of bytes read from file
   LOCAL nSaveUnit // Unit in use before in order to restore it at the end

   lDelete := iif( lDelete == NIL, .T., lDelete )
   nOrientation := iif( nOrientation == NIL, APS_PORTRAIT, nOrientation )
   nPrinter := iif( nPrinter == NIL, 0, nPrinter )
   cFont := iif( cFont == NIL, APS_COURIER, cFont )
   lSuccess := ( nHandle := FOpen( cFileName ) ) <> -1

   IF lSuccess
      ::BeginDoc( nPrinter, cTitle, nOrientation, nCopies ) // Start document
      nSaveUnit := ::GetUnit()
      ::SetUnit( APS_TEXT ) // Set to millimeters. We'll handle the position by ourself
      ::SetFontAttributes( cFont, APS_PLAIN, 12 ) // Set default font

      WHILE .T.
         cBuffer := Space( pfBUFFERLEN ) // File buffer
         nBytes := FRead( nHandle, @cBuffer, pfBUFFERLEN ) // Read a chunk

         IF nBytes == 0 // If Eof
            EXIT // Exit
         ENDIF

         nPos := At( Chr( 13 ) + Chr( 10 ), cBuffer ) // Finds CR (if any)
         nLoop := 1
         cPrintLine := "" // Text to print

         IF nPos = 0
            nPos := nBytes
         ELSE
            nPos++
         ENDIF

         WHILE .T.
            nChar := Asc( SubStr( cBuffer, nLoop, 1 ) )

            DO CASE
            CASE nChar == 27 // ESCAPE code

               IF nLoop < nPos // Advances to next char
                  nLoop++
                  nCtrl := Asc( SubStr( cBuffer, nLoop, 1 ) ) // Read the control character

                  IF Len( cPrintLine ) != 0 // If there's still something to print...  // Changed by STD - 2008-02-25 - 1.3.5.0
                     ::TextOut( nRow, nCol, cPrintLine, , , , , nStyle ) // Print the left part of the string, before the ESCAPE code
                     nCol += Len( cPrintLine )
                     cPrintLine := ""
                  ENDIF

                  DO CASE
                  CASE nCtrl == 66 .AND. ! lBold // B - Set   Bold
                     lBold := .T.
                     nStyle += APS_BOLD

                  CASE nCtrl == 98 .AND. lBold // b - Reset Bold
                     lBold := .F.
                     nStyle -= APS_BOLD

                  CASE nCtrl == 73 .AND. ! lItalic // I - Set   Italic
                     lItalic := .T.
                     nStyle += APS_ITALIC

                  CASE nCtrl == 105 .AND. lItalic // i - Reset Italic
                     lItalic := .F.
                     nStyle -= APS_ITALIC

                  CASE nCtrl == 85 .AND. ! lUnder // U - Set   Underline
                     lUnder := .T.
                     nStyle += APS_UNDERLINE

                  CASE nCtrl == 117 .AND. lUnder // u - Reset Underline
                     lUnder := .F.
                     nStyle -= APS_UNDERLINE

                  CASE nCtrl == 83 .AND. ! lStrike // S - Set   Strikeout
                     lStrike := .T.
                     nStyle += APS_STRIKEOUT

                  CASE nCtrl == 115 .AND. lStrike // s - Reset Strikeout
                     lStrike := .F.
                     nStyle -= APS_STRIKEOUT

                  CASE nCtrl == 7 // 8 is already taken         8 CPI
                     ::SetCPI( 8 )

                  CASE nCtrl == 10 // 10 CPI
                     ::SetCPI( 10 )

                  CASE nCtrl == 12 // 12 CPI
                     ::SetCPI( 12 )

                  CASE nCtrl == 15 // 15 CPI
                     ::SetCPI( 15 )

                  CASE nCtrl == 17 // 17 CPI (16.66)
                     ::SetCPI( 17 )

                  CASE nCtrl == 18 // 18 CPI
                     ::SetCPI( 18 )

                  CASE nCtrl == 20 // 20 CPI
                     ::SetCPI( 20 )

                  CASE nCtrl == 6 // 6 LPI
                     ::SetLPI( 6 )

                  CASE nCtrl == 8 // 8 LPI
                     ::SetLPI( 8 )

                  ENDCASE

                  nLoop++
               ENDIF

            CASE nChar == 12 // EJECT code
               nLoop++

               IF Len( cPrintLine ) != 0 // If there's still something to print...  // Changed by STD - 2008-02-25 - 1.3.5.0
                  ::TextOut( nRow, nCol, cPrintLine, , , , , nStyle ) // Print the rest
                  nCol += Len( cPrintLine )
                  cPrintLine := ""
               ENDIF

               nRow := 0 // Reset row to 0
               nCol := 0 // Reset column to 0
               ::NewPage() // Page eject

            CASE nChar == 13 // CR/LF code
               nLoop++

               IF nLoop <= nPos
                  IF Asc( SubStr( cBuffer, nLoop, 1 ) ) == 10
                     nLoop++

                     IF Len( cPrintLine ) != 0 // If there's still something to print...  // Changed by STD - 2008-02-25 - 1.3.5.0
                        ::TextOut( nRow, nCol, cPrintLine, , , , , nStyle )
                        cPrintLine := ""
                     ENDIF

                     nRow++ // For each CRLF, skip line
                     nCol := 0 // Reset to column 0

                     FSeek( nHandle, -( nBytes - nLoop + 1 ), FS_RELATIVE ) // Position file pointer after EJECT or CRLF
                  ELSE
                     nCol := 0 // Reset to column 0
                  ENDIF
               ENDIF

            OTHERWISE
               cPrintLine := cPrintLine + SubStr( cBuffer, nLoop, 1 )
               nLoop++

            ENDCASE

            IF nLoop > nPos
               EXIT
            ENDIF
         ENDDO
      ENDDO

      IF Len( cPrintLine ) != 0 // If there's still something to print...  // Changed by STD - 2008-02-25 - 1.3.5.0
         ::TextOut( nRow, nCol, cPrintLine, , , , , nStyle )
         cPrintLine := ""
      ENDIF

      ::SetUnit( nSaveUnit ) // Set to whatever unit was in use before
      ::EndDoc() // End the document

      FClose( nHandle )

   ENDIF

   IF lDelete
      FErase( cFileName )
   ENDIF

   SetPRC( 0, 0 )

RETURN lSuccess

/*-----------------------------------------------------------------------------
Function ...: PrintRawFile(<c>, [<l>, <n>, <c>]) -> lSuccess
Description : Print the file pointed by cFile and optionnaly deletes it.
              No processing on file. The file is sent "as is" to the printer.
Author .....: Stephan St-Denis
Date .......: September 2002
-----------------------------------------------------------------------------*/
METHOD TPageScript:PrintRawFile( cFileName, lDelete, nPrinter, cTitle )

   LOCAL lSuccess // Return value
   LOCAL nHandle // File handle
   LOCAL cBuffer // File buffer
   LOCAL nBytes // Number of bytes read from file

   lDelete := iif( lDelete == NIL, .T., lDelete )
   nPrinter := iif( nPrinter == NIL, 0, nPrinter )
   cTitle := iif( cTitle == NIL, "Untitled", cTitle )
   lSuccess := File( cFileName )

   lSuccess := ( nHandle := FOpen( cFileName ) ) <> -1

   IF lSuccess
      ::SetPrinter( nPrinter )
      ::SetTitle( cTitle )

      DllCall( ::hDll, DLL_STDCALL, "PSBeginRawDoc" )

      WHILE .T.
         cBuffer := Space( pfBUFFERLEN ) // File buffer
         nBytes := FRead( nHandle, @cBuffer, pfBUFFERLEN ) // Read a chunk

         IF nBytes == 0 // If Eof
            EXIT // Exit
         ENDIF

         DllCall( ::hDll, DLL_STDCALL, "PSPrintRawData", cBuffer, nBytes )
      ENDDO

      FClose( nHandle )

      IF lDelete
         FErase( cFileName )
      ENDIF

      DllCall( ::hDLL, DLL_STDCALL, "PSEndRawDoc" )
   ENDIF

RETURN lSuccess

/*-----------------------------------------------------------------------------
Method .....: #SetAsciiToAnsi(<l>) -> Self
Description : Set the conversion from Ascii to Ansi for text strings.
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetAsciiToAnsi( lValue )

   IF ValType( lValue ) == "L"
      DllCall( ::hDll, DLL_STDCALL, "PSSetAsciiToAnsi", iif( lValue, 1, 0 ) )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetBorder([<n>, <n>]) -> Self
Description : Sets the attributes used to draw lines and borders
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetBorder( nThickness, nBorderColor )

   IF ValType( nThickness ) == "N"
      DllCall( ::hDll, DLL_STDCALL, "PSSetBorderThickness", PSFtoI( nThickness ) )
   ENDIF

   IF ValType( nBorderColor ) == "N"
      DllCall( ::hDll, DLL_STDCALL, "PSSetBorderColor", nBorderColor )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetBorderColor(<n>) -> Self
Description : Sets the border color
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetBorderColor( nBorderColor )

   IF ValType( nBorderColor ) == "N"
      DllCall( ::hDll, DLL_STDCALL, "PSSetBorderColor", nBorderColor )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetBorderThickness(<n>) -> Self
Description : Sets the border thickness used to draw lines and borders
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetBorderThickness( nThickness )

   IF ValType( nThickness ) == "N"
      DllCall( ::hDll, DLL_STDCALL, "PSSetBorderThickness", PSFtoI( nThickness ) )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetClipperComp(<l>) -> Self
Description : Set Clipper compatible calls for TextOut() and TextBox()
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetClipperComp( lClipper )

   IF ValType( lClipper ) == "L"
      ::lClipper := lClipper
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetCoorSystem(<n>) -> Self
Description : Sets the coordinate system
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetCoorSystem( nCoor )

   IF ValType( nCoor ) == "N"
      DllCall( ::hDll, DLL_STDCALL, "PSSetCoorSystem", nCoor )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetCopies([<n>]) -> Self
Description : Sets the number of copies
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetCopies( nCopies )

   IF ValType( nCopies ) == "N"
      DllCall( ::hDll, DLL_STDCALL, "PSSetCopies", nCopies )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetCPI(<n>) -> Self
Description : Sets the number of characters per inch (Text mode only)
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetCPI( nCPI )

   IF ValType( nCPI ) == "N"
      DllCall( ::hDll, DLL_STDCALL, "PSSetCPI", nCPI )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetDevice(<n>) -> Self
Description : Sets the device to use for the next print job
Choices ....: DEV_PRINTER - The output will be sent to a printer
              DEV_PREVIEW - The output will be sent to the print preview dialog
              DEV_PDFFILE - The output will create a PDF file (must set filename)
              DEV_EMFFILE - Print to a serie of EMF files
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetDevice( nDevice )

   IF ValType( nDevice ) == "N"
      DllCall( ::hDll, DLL_STDCALL, "PSSetDevice", nDevice )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetDirectPrint(<c>) -> Self
Description : Set the direct print flag (used for very special cases)
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetDirectPrint( lDirect )

   IF ValType( lDirect ) == "L"
      DllCall( ::hDll, DLL_STDCALL, "PSSetDirectPrint", iif( lDirect, 1, 0 ) )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetDecimalSep(<c>) -> Self
Description : Sets the decimal separator character
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetDecimalSep( cSep )

   IF ValType( cSep ) == "C" .AND. Len( cSep ) > 0
      DllCall( ::hDll, DLL_STDCALL, "PSSetDecimalSep", Asc( cSep ) )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetDuplex([<n>]) -> Self
Description : Sets the duplex mode
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetDuplex( nDuplex )

   IF ValType( nDuplex ) == "N"
      DllCall( ::hDll, DLL_STDCALL, "PSSetDuplex", nDuplex )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetFileName(<c>) -> Self
Description : Sets the file name of a PDF file
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetFileName( cFileName )

   IF ValType( cFileName ) == "C" .AND. ! Empty( cFileName )
      DllCall( ::hDll, DLL_STDCALL, "PSSetFileName", cFileName )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetFill(<n>, [<n>]) -> Self
Description : Sets the color used to fill shapes, like boxes and ellipes
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetFill( nFillColor, nFillPattern )

   IF ValType( nFillColor ) == "N"
      DllCall( ::hDll, DLL_STDCALL, "PSSetFillColor", nFillColor )
   ENDIF

   IF ValType( nFillPattern ) == "N"
      DllCall( ::hDll, DLL_STDCALL, "PSSetFillPattern", nFillPattern )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetFillColor(<n>) -> Self
Description : Sets the color used to fill shapes, like boxes and ellipes
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetFillColor( nFillColor )

   IF ValType( nFillColor ) == "N"
      DllCall( ::hDll, DLL_STDCALL, "PSSetFillColor", nFillColor )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetFillPattern(<n>) -> Self
Description : Sets the pattern used to fill shapes, like boxes and ellipes
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetFillPattern( nFillPattern )

   IF ValType( nFillPattern ) == "N"
      DllCall( ::hDll, DLL_STDCALL, "PSSetFillPattern", nFillPattern )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetFont([<n>, <n>, <n>, <n>, <n>]) -> Self
Description : Sets the font attributes used by each call to PSTextOut() and others
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetFontAttributes( cFont, nStyle, nSize, nTFColor, nTBColor, nAngle )

   IF ValType( cFont ) == "C"
      DllCall( ::hDll, DLL_STDCALL, "PSSetFontName", cFont )
   ENDIF

   IF ValType( nStyle ) == "N"
      DllCall( ::hDll, DLL_STDCALL, "PSSetFontStyle", nStyle )
   ENDIF

   IF ValType( nSize ) == "N"
      DllCall( ::hDll, DLL_STDCALL, "PSSetFontSize", PSFtoI( nSize ) )
   ENDIF

   IF ValType( nTFColor ) == "N"
      DllCall( ::hDll, DLL_STDCALL, "PSSetFontFColor", nTFColor )
   ENDIF

   IF ValType( nTBColor ) == "N"
      DllCall( ::hDll, DLL_STDCALL, "PSSetFontBColor", nTBColor )
   ENDIF

   IF ValType( nAngle ) == "N"
      DllCall( ::hDll, DLL_STDCALL, "PSSetFontAngle", nAngle )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetFont() -> Self
Description : Sets the font
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetFontAngle( nAngle )

   IF ValType( nAngle ) == "N"
      DllCall( ::hDll, DLL_STDCALL, "PSSetFontAngle", nAngle )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetFont() -> Self
Description : Sets the font
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetFontBColor( nTBColor )

   IF ValType( nTBColor ) == "N"
      DllCall( ::hDll, DLL_STDCALL, "PSSetFontBColor", nTBColor )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetFont() -> Self
Description : Sets the font
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetFontFColor( nTFColor )

   IF ValType( nTFColor ) == "N"
      DllCall( ::hDll, DLL_STDCALL, "PSSetFontFColor", nTFColor )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetFont() -> Self
Description : Sets the font
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetFontName( cFont )

   IF ValType( cFont ) == "C"
      DllCall( ::hDll, DLL_STDCALL, "PSSetFontName", cFont )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetFont() -> Self
Description : Sets the font
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetFontSize( nSize )

   IF ValType( nSize ) == "N"
      DllCall( ::hDll, DLL_STDCALL, "PSSetFontSize", PSFtoI( nSize ) )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetFont() -> Self
Description : Sets the font
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetFontStyle( nStyle )

   IF ValType( nStyle ) == "N"
      DllCall( ::hDll, DLL_STDCALL, "PSSetFontStyle", nStyle )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetJustify(<n>) -> Self
Description : Sets justification for TextOut
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetJustify( nJustify )

   IF ValType( nJustify ) == "N"
      DllCall( ::hDll, DLL_STDCALL, "PSSetJustify", nJustify )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetLPI(<n>) -> Self
Description : Sets the number of lines per inch (Text mode only)
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetLPI( nLPI )

   IF ValType( nLPI ) == "N"
      DllCall( ::hDll, DLL_STDCALL, "PSSetLPI", nLPI )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetOrientation(<n>) -> Self
Description : Sets the paper orientation
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetOrientation( nOrientation )

   IF ValType( nOrientation ) == "N" .AND. ( nOrientation == APS_PORTRAIT .OR. nOrientation == APS_LANDSCAPE )
      DllCall( ::hDll, DLL_STDCALL, "PSSetOrientation", nOrientation )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetPaperBin(<n>) -> Self
Description : Sets the paper bin
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetPaperBin( nPaperBin )

   IF ValType( nPaperBin ) == "N"
      DllCall( ::hDll, DLL_STDCALL, "PSSetPaperBin", nPaperBin )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetPDFCharSet(<n>) -> Self
Description : Set the PDF charset to be use
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetPDFCharSet( nCharSet )

   IF ValType( nCharSet ) == "N"
      DllCall( ::hDll, DLL_STDCALL, "PSSetPDFCharSet", nCharSet )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetPDFOwnerPassword(<c>) -> Self
Description : Set the encryption password for PDF documents
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetPDFOwnerPassword( cPassword )

   IF ValType( cPassword ) == "C" .AND. ! Empty( cPassword )
      DllCall( ::hDll, DLL_STDCALL, "PSSetPDFOwnerPassword", cPassword )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetPDFEncoding(<n>) -> Self
Description : Set the internal encoding for PDF documents, default = 0;
            : 0 -> WinAnsiEncoding  ( Regular encoding for Latin-text Type 1
     and TrueType fonts on Windows OS.)
     : 1 -> StandardEncoding (Adobe Latin-text encoding for standard and
     other Type 1 fonts in case of non-symbolic fonts) and the
                   built-in encoding in case of symbolic fonts.)
            : 2 -> PDFDocEncoding (Rarely used non-Unicode encoding with a
     single byte for each character.)
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetPDFEncoding( nEncoding )

   IF ValType( nEncoding ) == "N"
      DllCall( ::hDll, DLL_STDCALL, "PSSetPDFEncoding", nEncoding )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetPDFVersion(<n>) -> Self
Description : Sets the PDF version to be used for creation of PDF
            : 0 - PDFVersion 1.4
            : 1 - PDFVersion 1.5
            : 2 - PDFVersion 1.6
            : 3 - PDfVersion 1.7
-----------------------------------------------------------------------------*/
METHOD TPagescript:SetPDFVersion( nVersion )

   IF ValType( nVersion ) == "N"
      DllCall( ::hDll, DLL_STDCALL, "PSSetPDFVersion", nVersion )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #ShowPDF(<l>) -> Self
Description : Show PDF in default reader after creation
-----------------------------------------------------------------------------*/
METHOD TPageScript:ShowPDF( lShow )

   IF ValType( lShow ) == "L"
      DllCall( ::hDll, DLL_STDCALL, "PSShowPDF", iif( lShow, 1, 0 ) )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetPDFEmbeddedPDFFonts(<n>) -> Self
Description : Set usage of embedded fonts
            : 0 -> None
            : 1 -> Full
            : 2 -> Subset
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetPDFEmbeddedFonts( nEmbedded )

   IF ValType( nEmbedded ) == "N"
      DllCall( ::hDll, DLL_STDCALL, "PSSetPDFEmbeddedFonts", nEmbedded )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetPageSize(<n>) -> Self
Description : Sets the page size to a predifined paper size
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetPageSize( nPageSize )

   IF ValType( nPageSize ) == "N" // .and. (nPageSize >= DMPAPER_FIRST .and. nPageSize <= DMPAPER_LAST)
      DllCall( ::hDll, DLL_STDCALL, "PSSetPageSize", nPageSize )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetPrinter(<n>) -> Self
Description : Sets the printer number to which we want to print
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetPrinter( nPrinter )

   IF ValType( nPrinter ) == "N"
      DllCall( ::hDll, DLL_STDCALL, "PSSetPrinter", nPrinter - 1 )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetPWState(<n>) -> Self
Description : Set the print preview window state
Version ....: 2.1.0.0
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetPWState( nState )

   IF ( ValType( nState ) == "N" ) .AND. ( nState >= PWS_MINIMIZED ) .AND. ( nState <= PWS_AUTO )
      DllCall( ::hDll, DLL_STDCALL, "PSSetPWState", nState )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetPWPosition(<n>, <n>) -> Self
Description : Set the print preview window position
Version ....: 2.1.0.0
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetPWPosition( nLeft, nTop )

   IF ( ValType( nLeft ) == "N" ) .AND. ( ValType( nTop ) == "N" )
      DllCall( ::hDll, DLL_STDCALL, "PSSetPWPosition", nLeft, nTop )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetPWSize(<n>, <n>) -> Self
Description : Set the print preview window size (-1, -1 = Auto size)
Version ....: 2.1.0.0
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetPWSize( nWidth, nHeight )

   IF ( ValType( nWidth ) == "N" ) .AND. ( ValType( nHeight ) == "N" )
      DllCall( ::hDll, DLL_STDCALL, "PSSetPWSize", nWidth, nHeight )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetPWBounds(<n>, <n>, <n>, <n>) -> Self
Description : Set the print preview window bounds (position and size at the same time)
Version ....: 2.1.0.0
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetPWBounds( nLeft, nTop, nWidth, nHeight )

   IF ( ValType( nLeft ) == "N" ) .AND. ( ValType( nTop ) == "N" ) .AND. ( ValType( nWidth ) == "N" ) .AND. ( ValType( nHeight ) == "N" )
      DllCall( ::hDll, DLL_STDCALL, "PSSetPWBounds", nLeft, nTop, nWidth, nHeight )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetPWColors(<n>, <n>, <n>, <n>) -> Self
Description : Set the print preview window colors
Version ....: 3.0.0.0
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetPWColors( nBackground, nPaper, nShadow, nToolbar )

   IF ( ValType( nBackground ) == "N" ) .AND. ( ValType( nPaper ) == "N" ) .AND. ( ValType( nShadow ) == "N" ) .AND. ( ValType( nToolbar ) == "N" )
      DllCall( ::hDll, DLL_STDCALL, "PSSetPWColors", nBackground, nPaper, nShadow, nToolbar )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetPWZoomLevel(<n>) -> Self
Description : Set the print preview window zoom level
Version ....: 2.1.0.0
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetPWZoomLevel( nZoomLevel )

   IF ( ValType( nZoomLevel ) == "N" )
      DllCall( ::hDll, DLL_STDCALL, "PSSetPWZoomLevel", nZoomLevel )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetRowCol(<n>, <n>) -> Self
Description : Sets the number of rows and columns in the way PageScript for Clipper does
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetRowCol( nRow, nCol )

   IF ValType( nRow ) == "N" .AND. ValType( nCol ) == "N"
      DllCall( ::hDll, DLL_STDCALL, "PSSetRowCol", nRow, nCol )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetTitle(<c>) -> Self
Description : Sets the title of the document
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetTitle( cTitle )

   IF ValType( cTitle ) == "C" .AND. ! Empty( cTitle )
      DllCall( ::hDll, DLL_STDCALL, "PSSetTitle", cTitle )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetUnit(<n>) -> Self
Description : Sets the unit used to calculate dimensions and placement
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetUnit( nUnit )

   IF ValType( nUnit ) == "N"
      ::nUnit := nUnit
      DllCall( ::hDll, DLL_STDCALL, "PSSetUnit", nUnit )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetUseDIB(<l>) -> Self
Description : Sets the use of DIB (Device Independant Bitmap) when printing bitmaps
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetUseDIB( lUseDIB )

   IF ValType( lUseDIB ) == "L"
      DllCall( ::hDll, DLL_STDCALL, "PSSetUseDIB", iif( lUseDIB, 1, 0 ) )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #SetXerox(<l>) -> Self
Description : Sets the use of Xerox compatible printing mode (Slower, but
            compatible with all Xerox printers)
-----------------------------------------------------------------------------*/
METHOD TPageScript:SetXerox( lXerox )

   IF ValType( lXerox ) == "L"
      DllCall( ::hDll, DLL_STDCALL, "PSSetXerox", iif( lXerox, 1, 0 ) )
   ENDIF

RETURN Self

/*-----------------------------------------------------------------------------
Method .....: #PSTextBox(<n>, <n>, <n>, <n>, <x>, [<n>, <c>, <n>, <n>, <n>, <n>,
                       <n>, <n>]) -> NIL
Description : Prints a string in a box at position X1,Y1, X2,Y2 using parameters
-----------------------------------------------------------------------------*/
METHOD TPageScript:TextBox( nTop, nLeft, nBottom, nRight, cText, nJustify, cFont, nSize, ;
      nStyle, nFColor, nBColor, nThick )

   LOCAL Temp

   IF ( ValType( cText ) == "C" .AND. Len( cText ) < 65001 ) // Must be a string and Length of less than 65001 bytes
      cFont := iif( cFont == NIL, "", cFont )
      nJustify := iif( nJustify == NIL, APS_DEFAULT, Int( nJustify ) )
      nStyle := iif( nStyle == NIL, APS_DEFAULT, Int( nStyle ) )
      nSize := iif( nSize == NIL, APS_DEFAULT, nSize )
      nFColor := iif( nFColor == NIL, APS_DEFAULT, Int( nFColor ) )
      nBColor := iif( nBColor == NIL, APS_DEFAULT, Int( nBColor ) )
      nThick := iif( nThick == NIL, APS_DEFAULT, nThick )

      // Clipper compatible call
      IF ::lClipper
         Temp := nSize
         nSize := nStyle
         nStyle := Temp
      ENDIF

      DllCall( ::hDll, DLL_STDCALL, "PSTextBoxEx", PSFtoI( nTop ), PSFtoI( nLeft ), PSFtoI( nBottom ), PSFtoI( nRight ), cText, nJustify, cFont, PSFtoI( nSize ), nStyle, nFColor, nBColor, PSFtoI( nThick ) )
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
Method .....: #PSTextBoxEx(<n>, <n>, <n>, <n>, <x>) -> NIL
Description : Prints a string in a box at position X1,Y1, X2,Y2 using parameters
-----------------------------------------------------------------------------*/
METHOD TPageScript:TextBoxEx( nTop, nLeft, nBottom, nRight, cText )

   IF ( ValType( cText ) == "C" .AND. Len( cText ) < 65001 .AND. !( cText == "" ) ) // Must be a non empty string and Length of less than 65001 bytes
      DllCall( ::hDll, DLL_STDCALL, "PSTextBox", PSFtoI( nTop ), PSFtoI( nLeft ), PSFtoI( nBottom ), PSFtoI( nRight ), cText )
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
Method .....: #TextOut(<n>, <n>, <x>, [<c>, <n>, <n>, <n>, <n>, <n>, <n>, <n>]) -> NIL
Description : Prints a string at position X,Y using parameters

PSTEXTOUT(YY1, XX2 + XXO, ' {continued)', pic, just, font, PTS3, APS_PLAIN)
-----------------------------------------------------------------------------*/
METHOD TPageScript:TextOut( nTop, nLeft, xValue, cPicture, nJustify, cFont, nSize, ;
      nStyle, nTFColor, nTBColor, nAngle )

   LOCAL cValType
   LOCAL Temp

   cValType := ValType( xValue )
   nTop := iif( nTop == NIL, APS_DEFAULT, nTop )
   nLeft := iif( nLeft == NIL, APS_DEFAULT, nLeft )
   cPicture := iif( cPicture == NIL, "", cPicture )
   nJustify := iif( nJustify == NIL, APS_DEFAULT, Int( nJustify ) )
   cFont := iif( cFont == NIL, "", cFont )
   nStyle := iif( nStyle == NIL, APS_DEFAULT, Int( nStyle ) )
   nSize := iif( nSize == NIL, APS_DEFAULT, nSize )
   nTFColor := iif( nTFColor == NIL, APS_DEFAULT, Int( nTFColor ) )
   nTBColor := iif( nTBColor == NIL, APS_DEFAULT, Int( nTBColor ) )
   nAngle := iif( nAngle == NIL, APS_DEFAULT, Int( nAngle ) )

   IF Empty( cPicture )
      DO CASE
      CASE cValType == "N"
         cPicture := "@N"

      CASE cValType == "D"
         cPicture := "@D"

      CASE cValType == "L"
         cPicture := "@Y"

      OTHERWISE
         cPicture := "@X"

      ENDCASE
   ENDIF

   xValue := Transform( xValue, cPicture )

   IF nAngle <> APS_DEFAULT .AND. ( nAngle < 0 .OR. nAngle > 360 )
      nAngle := 0
   ENDIF

   // Clipper compatible call
   IF ::lClipper
      Temp := nSize
      nSize := nStyle
      nStyle := Temp
   ENDIF

   if ! xValue == ""
      DllCall( ::hDll, DLL_STDCALL, "PSTextOutEx", PSFtoI( nTop ), PSFtoI( nLeft ), xValue, nJustify, cFont, PSFtoI( nSize ), nStyle, nTFColor, nTBColor, nAngle )
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
Method .....: #TextOutEx(<n>, <n>, <x>, [<c>]) -> NIL
Description : Prints a string at position X,Y using parameters
-----------------------------------------------------------------------------*/
METHOD TPageScript:TextOutEx( nTop, nLeft, xValue, cPicture )

   LOCAL cValType := ValType( xValue )

   nTop := iif( nTop == NIL, APS_DEFAULT, nTop )
   nLeft := iif( nLeft == NIL, APS_DEFAULT, nLeft )
   cPicture := iif( cPicture == NIL, "", cPicture )

   IF Empty( cPicture )
      DO CASE
      CASE cValType == "N"
         cPicture := "@N"

      CASE cValType == "D"
         cPicture := "@D"

      CASE cValType == "L"
         cPicture := "@Y"

      OTHERWISE
         cPicture := "@X"

      ENDCASE
   ENDIF

   xValue := Transform( xValue, cPicture )

   if ! xValue == ""
      DllCall( ::hDll, DLL_STDCALL, "PSTextOut", PSFtoI( nTop ), PSFtoI( nLeft ), xValue )
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
Method .....: #UnicodeCharEx(<n>, <n>, <n>, [<n>, <c>, <n>, <n>, <n>, <n>, <n>]) -> NIL
Description : Prints a Unicode Character at position X,Y using parameters
-----------------------------------------------------------------------------*/
METHOD TPageScript:UnicodeCharEx( nTop, nLeft, nUnicodeHex, nJustify, cFont, nSize, ;
      nStyle, nTFColor, nTBColor, nAngle )

   LOCAL Temp

   nUnicodeHex := iif( nUnicodeHex == NIL, 0, nUnicodeHex )

   nTop := iif( nTop == NIL, APS_DEFAULT, nTop )
   nLeft := iif( nLeft == NIL, APS_DEFAULT, nLeft )
   nJustify := iif( nJustify == NIL, APS_DEFAULT, Int( nJustify ) )
   cFont := iif( cFont == NIL, "", cFont )
   nStyle := iif( nStyle == NIL, APS_DEFAULT, Int( nStyle ) )
   nSize := iif( nSize == NIL, APS_DEFAULT, nSize )
   nTFColor := iif( nTFColor == NIL, APS_DEFAULT, Int( nTFColor ) )
   nTBColor := iif( nTBColor == NIL, APS_DEFAULT, Int( nTBColor ) )
   nAngle := iif( nAngle == NIL, APS_DEFAULT, Int( nAngle ) )

   IF nAngle <> APS_DEFAULT .AND. ( nAngle < 0 .OR. nAngle > 360 )
      nAngle := 0
   ENDIF

   // Clipper compatible call
   IF ::lClipper
      Temp := nSize
      nSize := nStyle
      nStyle := Temp
   ENDIF

   DllCall( ::hDll, DLL_STDCALL, "PSUnicodeCharEx", PSFtoI( nTop ), PSFtoI( nLeft ), nUnicodeHex, nJustify, cFont, PSFtoI( nSize ), nStyle, nTFColor, nTBColor, nAngle )

RETURN NIL

/*-----------------------------------------------------------------------------
Method .....: #UnicodeChar(<n>, <n>, <n> ) -> NIL
Description : Prints a string at position X,Y using parameters
-----------------------------------------------------------------------------*/
METHOD TPageScript:UnicodeChar( nTop, nLeft, nUnicodeHex )

   nUnicodeHex := iif( nUnicodeHex == NIL, 0, nUnicodeHex )

   nTop := iif( nTop == NIL, APS_DEFAULT, nTop )
   nLeft := iif( nLeft == NIL, APS_DEFAULT, nLeft )

   DllCall( ::hDll, DLL_STDCALL, "PSUnicodeChar", PSFtoI( nTop ), PSFtoI( nLeft ), nUnicodeHex )

RETURN NIL

/*-----------------------------------------------------------------------------
Method .....: #WaterMark([<b>]) -> Previous WaterMark
Description : Sets/Returns the current WaterMark
Note .......: This method is not implemented in PSCRIPT.DLL
-----------------------------------------------------------------------------*/
METHOD TPageScript:WaterMark( bWaterMark, nWaterMark )

   LOCAL bOldWaterMark := ::bWaterMark
   LOCAL nOldWaterMark := ::nWaterMark

   nWaterMark := iif( nWaterMark == NIL, ::nWaterMark, nWaterMark )

   IF ValType( bWaterMark ) $ "BU"
      ::bWaterMark := bWaterMark
      ::nWaterMark := nWaterMark
   ENDIF

RETURN { bOldWaterMark, nOldWaterMark }

/*-----------------------------------------------------------------------------
Method .....: #SignPDF(<c>, <c>, <c>,<c>,<c>,<c>,<n>,<n>,<n>,<n>,<n> ) -> NIL
Description : Digitally sign PDF file
Parameters..: cFilename --> inputfile
            : cOutName  --> outputfile signed
            : cCertFilename --> pfx file
            : cCertPassword --> pfx password
            : cLocation,cReason --> printed info on PDF
            : nPage --> pagenumber of PDF file where sign is printed if vissible
            : nLeft,nTop,nBottom,nRight in pixels display box if these values are 0 its invisible.
-----------------------------------------------------------------------------*/

METHOD TPagescript:SignPDF( cFilename, cOutName, cCertFilename, cCertPassword, cLocation, cReason, nPage, nLeft, nTop, nBottom, nRight )

   LOCAL nRet := -1

   IF ValType( cCertFilename ) == "C" .AND. ValType( cCertPassword ) == "C"

      // Sets default values
      nLeft := iif( nLeft == NIL, 0, nLeft )
      nTop := iif( nTop == NIL, 0, nTop )
      nBottom := iif( nBottom == NIL, 0, nBottom )
      nRight := iif( nRight == NIL, 0, nRight )
      cLocation := iif( cLocation == NIL, '', cLocation )
      cReason := iif( cReason == NIL, '', cReason )

      nRet := DllCall( ::hDll, DLL_STDCALL, "PSSignPDF", cFilename, cOutname, cCertFilename, cCertPassword, cLocation, cReason, nPage, PSFtoI( nLeft ), PSFtoI( nTop ), PSFtoI( nRight ), PSFtoI( nBottom ) )

   ENDIF

RETURN nRet

/*-----------------------------------------------------------------------------


                            HELPER FUNCTIONS START HERE


-----------------------------------------------------------------------------*/

/*-----------------------------------------------------------------------------
Method .....: #Buffer2String()
Description : Internal function. Returns a string from a buffer
-----------------------------------------------------------------------------*/
METHOD TPageScript:Buffer2String( cBuffer )

   LOCAL cString

   cString := AllTrim( cBuffer )

RETURN Left( cString, Len( cString ) - 1 )

/*-----------------------------------------------------------------------------
Function ...: #PSFtoI(AValue) -> Value
Description : Converts a Double to an Integer for PSInt compatibility
-----------------------------------------------------------------------------*/
FUNCTION PSFtoI( AValue )

RETURN Int( AValue * 10000 )

/*-----------------------------------------------------------------------------
Function ...: #PSItoF(AValue) -> Value
Description : Converts a PSInt to a Double for PSInt compatibility
-----------------------------------------------------------------------------*/
FUNCTION PSItoF( AValue )

RETURN Round( AValue / 10000, 4 )

/*-----------------------------------------------------------------------------


                            FUNCTIONS START HERE


-----------------------------------------------------------------------------*/

/*-----------------------------------------------------------------------------
Function ...: PSInit() -> nErrorCode
Description : Initialization of the printer driver
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSInit()

   // If not Initialized, then do it !!!
   if ! slInitialized
      oPScript := TPageScript():New()
      slInitialized := ( oPScript:nError == PSE_NOERROR )
   ENDIF

RETURN oPScript:nError

/*-----------------------------------------------------------------------------
Function ...: PSAbort() -> NIL
Description : Abort the current document
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSAbort()

   IF slInitialized
      oPScript:Abort()
   ENDIF

RETURN NIL

FUNCTION PSUnicodeChar( nTop, nLeft, nUnicodeHex )
   IF slInitialized
      oPScript:UnicodeChar( nTop, nLeft, nUnicodeHex )
   ENDIF

RETURN NIL

FUNCTION PSUnicodeCharEx( nTop, nLeft, nUnicodeHex, nJustify, cFont, nSize, nStyle, nTFColor, nTBColor, nAngle )
   IF slInitialized
      oPScript:UnicodeCharEx( nTop, nLeft, nUnicodeHex, nJustify, cFont, nSize, nStyle, nTFColor, nTBColor, nAngle )
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
Function ...: PSAsciiToAnsi([<l>]) -> lOldValue
Description : Set/Get the conversion from Ascii to Ansi for text strings.
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSAsciiToAnsi( lValue )

   LOCAL lOldValue := .F.

   IF slInitialized
      lOldValue := oPScript:GetAsciiToAnsi()
      oPScript:SetAsciiToAnsi( lValue )
   ENDIF

RETURN lOldValue

/*-----------------------------------------------------------------------------
Function ...: PSBarCodeEx(<n>, <n>, <c>, [<n>, <n>, <n>, <l>]) -> NIL
Description : Print a barcode with validation
-----------------------------------------------------------------------------*/
FUNCTION PSBarCodeEx( nTop, nLeft, cCode, nHeight, nThick, nType, lValidate )

   IF slInitialized
      oPScript:BarCodeEx( nTop, nLeft, cCode, nHeight, nThick, nType, lValidate )
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
Function ...: PSQRCode(<n>, <n>, <c>, [<n>,<n>]) -> NIL
Description : Print a QRCode
Author .....: R.Visscher
Date .......: June 2017
-----------------------------------------------------------------------------*/
FUNCTION PSQRCode( nTop, nLeft, cCode, nVersion, nSize, nModuleWidth )

   IF slInitialized
      oPScript:QRCode( nTop, nLeft, cCode, nVersion, nSize, nModuleWidth )
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
Function ...: PSBarCode(<n>, <n>, <c>, [<n>, <n>, <l>, <n>, <l>]) -> NIL
Description : Print a barcode with choice of Code 39 and Code 128
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSBarCode( nTop, nLeft, cCode, nHeight, nThick, lPrintText, nType, lVertical, lUnitMM )

   IF slInitialized
      oPScript:BarCode( nTop, nLeft, cCode, nHeight, nThick, lPrintText, nType, lVertical, lUnitMM )
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
Function ...: PSBeginDoc(<n>, <c>, <n>, <n>) -> nError
Description : Starts a new print job with parameters
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSBeginDoc( nPrinter, cTitle, nOrientation, nCopies, nPageSize, nDuplex, nPaperBin )

   LOCAL nError := PSE_NOTINITIALIZED

   IF slInitialized
      nError := oPScript:BeginDoc( nPrinter, cTitle, nOrientation, nCopies, nPageSize, nDuplex, nPaperBin )
   ENDIF

RETURN nError

/*-----------------------------------------------------------------------------
Function ...: PSBeginDocEx() -> nError
Description : Starts a new print job with defaults parameters
Author .....: Stephan St-Denis
Date .......: July 2006
-----------------------------------------------------------------------------*/
FUNCTION PSBeginDocEx()

   LOCAL nError := PSE_NOTINITIALIZED

   IF slInitialized
      nError := oPScript:BeginDocEx()
   ENDIF

RETURN nError

/*-----------------------------------------------------------------------------
Function ...: PSBeginEmuDoc(<n>, <c>, <n>, <n>, <n>, <c>) -> lError
Description :
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSBeginEmuDoc( nPrinter, cTitle, nOrientation, nCopies, cFont )

   LOCAL nError := PSE_NOTINITIALIZED

   IF slInitialized
      nError := oPScript:BeginEmuDoc( nPrinter, cTitle, nOrientation, nCopies, cFont )
   ENDIF

RETURN nError

/*-----------------------------------------------------------------------------
Function ...: PSBeginRawDoc(<n>, <c>, <n>, <n>) -> nError
Description : Starts the printer driver with some parameters
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSBeginRawDoc( nPrinter, cTitle )

   LOCAL nError := PSE_NOTINITIALIZED

   IF slInitialized
      nError := oPScript:BeginRawDoc( nPrinter, cTitle )
   ENDIF

RETURN nError

/*-----------------------------------------------------------------------------
Function ...: PSBitmap(<n>, <n>, <n>, <n>, <x>, [<n>]) -> NIL
Description : Draw a bitmap on the document.
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSBitmap( nTop, nLeft, nBottom, nRight, cBitmap, nTransColor, lKeepRatio )

   IF slInitialized
      oPScript:Bitmap( nTop, nLeft, nBottom, nRight, cBitmap, nTransColor, lKeepRatio )
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
Function ...: PSEllipse(<n>, <n>, <n>, <n>, [<n>, <n>, <n>, <n>]) -> NIL
Description : Draw an ellipse
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSEllipse( nTop, nLeft, nBottom, nRight, nThick, nBorderColor, nFillColor, nPattern )

   IF slInitialized
      oPScript:Ellipse( nTop, nLeft, nBottom, nRight, nThick, nBorderColor, nFillColor, nPattern )
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
Function ...: PSEllipseEx(<n>, <n>, <n>, <n>) -> NIL
Description : Draw an ellipse
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSEllipseEx( nTop, nLeft, nBottom, nRight )

   IF slInitialized
      oPScript:EllipseEx( nTop, nLeft, nBottom, nRight )
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
Function ...: PSEndDoc() -> NIL
Description : Signal the end of the document
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSEndDoc()

   IF slInitialized
      oPScript:EndDoc()
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
Function ...: PSEndEmuDoc() -> NIL
Description : Signal the end of the document
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSEndEmuDoc()

   IF slInitialized
      oPScript:EndEmuDoc()
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
Function ...: PSEndRawDoc() -> NIL
Description : Signal the end of the document
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSEndRawDoc()

   IF slInitialized
      oPScript:EndRawDoc()
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
Function ...: PSFindPrinter(<c>) -> nPos
Description : Search for a printer name and returns its position within the array
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSFindPrinter( cPrinter )

   LOCAL nPos := -1

   IF slInitialized
      nPos := oPScript:FindPrinter( cPrinter )
   ENDIF

RETURN nPos

/*-----------------------------------------------------------------------------
Function ...: PSFrame(<n>, <n>, <n>, <n>, [<n>, <n>, <n>, <n>, <n>]) -> NIL
Description : Draw a frame
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSFrame( nTop, nLeft, nBottom, nRight, nThick, nBorderColor, nFillColor, nPattern, nRadius )

   IF slInitialized
      oPScript:Frame( nTop, nLeft, nBottom, nRight, nThick, nBorderColor, nFillColor, nPattern, nRadius )
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
Function ...: PSFrameEx(<n>, <n>, <n>, <n>) -> NIL
Description : Draw a frame
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSFrameEx( nTop, nLeft, nBottom, nRight, nRadius )

   IF slInitialized
      oPScript:FrameEx( nTop, nLeft, nBottom, nRight, nRadius )
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
Function ...: PSGetAsciiToAnsi() -> lValue
Description : Retreive the translation setting
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSGetAsciiToAnsi()

   LOCAL lValue := .F.

   IF slInitialized
      lValue := oPScript:GetAsciiToAnsi()
   ENDIF

RETURN lValue

/*-----------------------------------------------------------------------------
Function ...: PSGetBorderColor() -> nValue
Description : Returns the current border color
Author .....: Stephan St-Denis
Date .......: April 2005
-----------------------------------------------------------------------------*/
FUNCTION PSGetBorderColor()

   LOCAL nValue := -1

   IF slInitialized
      nValue := oPScript:GetBorderColor()
   ENDIF

RETURN nValue

/*-----------------------------------------------------------------------------
Function ...: PSGetBorderThickness() -> nValue
Description : Returns the current border thickness
Author .....: Stephan St-Denis
Date .......: April 2005
-----------------------------------------------------------------------------*/
FUNCTION PSGetBorderThickness()

   LOCAL nValue := -1

   IF slInitialized
      nValue := oPScript:GetBorderThickness()
   ENDIF

RETURN nValue

/*-----------------------------------------------------------------------------
Function ...: PSGetCaps() -> aCaps
Description : Retreive the printer capabilities of the selected printer
Author .....: Stephan St-Denis
Date .......: March 2005
Note .......: Compatibility
-----------------------------------------------------------------------------*/
FUNCTION PSGetCaps()

RETURN PSGetPrinterCaps()

/*-----------------------------------------------------------------------------
Function ...: PSGetCoorSystem() -> nValue
Description : Returns the current coordinate system
Author .....: Stephan St-Denis
Date .......: April 2006
-----------------------------------------------------------------------------*/
FUNCTION PSGetCoorSystem()

   LOCAL nValue := -1

   IF slInitialized
      nValue := oPScript:GetCoorSystem()
   ENDIF

RETURN nValue

/*-----------------------------------------------------------------------------
Function ...: PSGetCopies() -> nValue
Description : Returns the number of copies set for the current print job
Author .....: Stephan St-Denis
Date .......: April 2005
-----------------------------------------------------------------------------*/
FUNCTION PSGetCopies()

   LOCAL nValue := -1

   IF slInitialized
      nValue := oPScript:GetCopies()
   ENDIF

RETURN nValue

/*-----------------------------------------------------------------------------
Function ...: PSGetCPI() -> nValue
Description : Returns the number of character per inch in use (text mode only)
Author .....: Stephan St-Denis
Date .......: April 2005
-----------------------------------------------------------------------------*/
FUNCTION PSGetCPI()

   LOCAL nValue := -1

   IF slInitialized
      nValue := oPScript:GetCPI()
   ENDIF

RETURN nValue

/*-----------------------------------------------------------------------------
Function ...: PSGetDecimalSep() -> cValue
Description : Returns the decimal separator in use
Author .....: Stephan St-Denis
Date .......: April 2005
-----------------------------------------------------------------------------*/
FUNCTION PSGetDecimalSep()

   LOCAL cValue := "?"

   IF slInitialized
      cValue := oPScript:GetDecimalSep()
   ENDIF

RETURN cValue

/*-----------------------------------------------------------------------------
Function ...: PSGetDefPrinter() -> nValue
Description : Returns the default Windows printer
Author .....: Stephan St-Denis
Date .......: March 2005
Note .......: Compatibility
-----------------------------------------------------------------------------*/
FUNCTION PSGetDefPrinter()

RETURN PSGetDefaultPrinter()

/*-----------------------------------------------------------------------------
Function ...: PSGetDefaultPrinter() -> nValue
Description : Returns the default Windows printer
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSGetDefaultPrinter()

   LOCAL nValue := -1

   IF slInitialized
      nValue := oPScript:GetDefaultPrinter()
   ENDIF

RETURN nValue

/*-----------------------------------------------------------------------------
Function ...: PSGetDuplex() -> nValue
Description : Returns the current Duplex mode
Author .....: Stephan St-Denis
Date .......: April 2005
-----------------------------------------------------------------------------*/
FUNCTION PSGetDuplex()

   LOCAL nValue := -1

   IF slInitialized
      nValue := oPScript:GetDuplex()
   ENDIF

RETURN nValue

/*-----------------------------------------------------------------------------
Function ...: PSGetFillColor() -> nValue
Description : Returns the current fill color
Author .....: Stephan St-Denis
Date .......: April 2005
-----------------------------------------------------------------------------*/
FUNCTION PSGetFillColor()

   LOCAL nValue := -1

   IF slInitialized
      nValue := oPScript:GetFillColor()
   ENDIF

RETURN nValue

/*-----------------------------------------------------------------------------
Function ...: PSGetFillPattern() -> nValue
Description : Returns the current fill pattern
Author .....: Stephan St-Denis
Date .......: April 2005
-----------------------------------------------------------------------------*/
FUNCTION PSGetFillPattern()

   LOCAL nValue := -1

   IF slInitialized
      nValue := oPScript:GetFillPattern()
   ENDIF

RETURN nValue

/*-----------------------------------------------------------------------------
Function ...: PSGetFontAngle() -> nValue
Description : Returns the current font angle
Author .....: Stephan St-Denis
Date .......: April 2006
-----------------------------------------------------------------------------*/
FUNCTION PSGetFontAngle()

   LOCAL nValue := -1

   IF slInitialized
      nValue := oPScript:GetFontAngle()
   ENDIF

RETURN nValue

/*-----------------------------------------------------------------------------
Function ...: PSGetFontCount() -> nValue
Description : Returns the current font count
Author .....: Stephan St-Denis
Date .......: April 2006
-----------------------------------------------------------------------------*/
FUNCTION PSGetFontCount()

   LOCAL nValue := 6
/*
   IF slInitialized
      nValue := oPScript:GetFontCount()
   ENDIF
*/
RETURN nValue

/*-----------------------------------------------------------------------------
Function ...: PSGetFonts() -> aFonts
Description : Retreive the printer fonts for the selected printer
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSGetFonts()

   LOCAL aFonts := { "Arial", "Courier New", "System", "Times New Roman", "Verdana", "WingDings" }
/*
   IF slInitialized
      aFonts := oPScript:GetFonts()
   ENDIF
*/
RETURN AClone( aFonts )

/*-----------------------------------------------------------------------------
Function ...: PSGetFontName() -> cValue
Description : Returns the current font name
Author .....: Stephan St-Denis
Date .......: April 2005
-----------------------------------------------------------------------------*/
FUNCTION PSGetFontName()

   LOCAL cValue := ""

   IF slInitialized
      cValue := oPScript:GetFontName()
   ENDIF

RETURN cValue

/*-----------------------------------------------------------------------------
Function ...: PSGetFontNames(<n>) -> cValue
Description : Returns the font name from its index position
Author .....: Stephan St-Denis
Date .......: April 2006
-----------------------------------------------------------------------------*/
FUNCTION PSGetFontNames( nFontIndex )

   LOCAL cValue := ""

   IF slInitialized
      cValue := oPScript:GetFontNames( nFontIndex )
   ENDIF

RETURN cValue

/*-----------------------------------------------------------------------------
Function ...: PSGetFontStyle() -> nValue
Description : Returns the current font style
Author .....: Stephan St-Denis
Date .......: April 2005
-----------------------------------------------------------------------------*/
FUNCTION PSGetFontStyle()

   LOCAL nValue := -1

   IF slInitialized
      nValue := oPScript:GetFontStyle()
   ENDIF

RETURN nValue

/*-----------------------------------------------------------------------------
Function ...: PSGetFontSize() -> nValue
Description : Returns the current font size
Author .....: Stephan St-Denis
Date .......: April 2005
-----------------------------------------------------------------------------*/
FUNCTION PSGetFontSize()

   LOCAL nValue := -1

   IF slInitialized
      nValue := oPScript:GetFontSize()
   ENDIF

RETURN nValue

/*-----------------------------------------------------------------------------
Function ...: PSGetFontBColor() -> nValue
Description : Returns the current Font background color
Author .....: Stephan St-Denis
Date .......: April 2005
-----------------------------------------------------------------------------*/
FUNCTION PSGetFontBColor()

   LOCAL nValue := -1

   IF slInitialized
      nValue := oPScript:GetFontBColor()
   ENDIF

RETURN nValue

/*-----------------------------------------------------------------------------
Function ...: PSGetFontFColor() -> nValue
Description : Returns the current font Foreground color
Author .....: Stephan St-Denis
Date .......: April 2005
-----------------------------------------------------------------------------*/
FUNCTION PSGetFontFColor()

   LOCAL nValue := -1

   IF slInitialized
      nValue := oPScript:GetFontFColor()
   ENDIF

RETURN nValue

/*-----------------------------------------------------------------------------
Function ...: PSGetJustify() -> nValue
Description : Returns the current text justification
Author .....: Stephan St-Denis
Date .......: April 2005
-----------------------------------------------------------------------------*/
FUNCTION PSGetJustify()

   LOCAL nValue := -1

   IF slInitialized
      nValue := oPScript:GetJustify()
   ENDIF

RETURN nValue

/*-----------------------------------------------------------------------------
Function ...: PSGetFontJustify() -> nValue
Description : Returns the current text justification
Author .....: Stephan St-Denis
Date .......: April 2005
-----------------------------------------------------------------------------*/
FUNCTION PSGetFontJustify()

   LOCAL nValue := -1

   IF slInitialized
      nValue := oPScript:GetFontJustify()
   ENDIF

RETURN nValue

/*-----------------------------------------------------------------------------
Function ...: PSGetLPI() -> nValue
Description : Returns the current number of lines per inch (text mode only)
Author .....: Stephan St-Denis
Date .......: April 2005
-----------------------------------------------------------------------------*/
FUNCTION PSGetLPI()

   LOCAL nValue := -1

   IF slInitialized
      nValue := oPScript:GetLPI()
   ENDIF

RETURN nValue

/*-----------------------------------------------------------------------------
Function ...: PSGetMaxHeight() -> nValue
Description :
Author .....: Stephan St-Denis
Date .......: May 2005
-----------------------------------------------------------------------------*/
FUNCTION PSGetMaxHeight()

   LOCAL nValue := -1

   IF slInitialized
      nValue := oPScript:GetMaxHeight()
   ENDIF

RETURN nValue

/*-----------------------------------------------------------------------------
Function ...: PSGetMaxWidth() -> nValue
Description :
Author .....: Stephan St-Denis
Date .......: May 2005
-----------------------------------------------------------------------------*/
FUNCTION PSGetMaxWidth()

   LOCAL nValue := -1

   IF slInitialized
      nValue := oPScript:GetMaxWidth()
   ENDIF

RETURN nValue

/*-----------------------------------------------------------------------------
Function ...: PSGetOrientation() -> nValue
Description : Returns the current page orientation (Portrait or Landscape)
Author .....: Stephan St-Denis
Date .......: April 2005
-----------------------------------------------------------------------------*/
FUNCTION PSGetOrientation()

   LOCAL nValue := -1

   IF slInitialized
      nValue := oPScript:GetOrientation()
   ENDIF

RETURN nValue

/*-----------------------------------------------------------------------------
Function ...: PSGetPageSize() -> nValue
Description : Retreive the printer paper size
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSGetPageSize()

   LOCAL nValue := -1

   IF slInitialized
      nValue := oPScript:GetPageSize()
   ENDIF

RETURN nValue

/*-----------------------------------------------------------------------------
Function ...: PSGetPaperBin() -> nValue
Description : Returns the current paper bin
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSGetPaperBin()

   LOCAL nValue := -1

   IF slInitialized
      nValue := oPScript:GetPaperBin()
   ENDIF

RETURN nValue

/*-----------------------------------------------------------------------------
Function ...: PSGetPaperBins() -> aBinArray
Description : Returns a two dimensions array with bins/trays numbers/names
Author .....: Stephan St-Denis
Date .......: May 2006
-----------------------------------------------------------------------------*/
FUNCTION PSGetPaperBins()

   LOCAL aBins := {}
   LOCAL nBinCount := 0
   LOCAL nLoop

   IF slInitialized
      nBinCount := PSGetPaperBinCount()

      FOR nLoop := 1 TO nBinCount
         AAdd( aBins, { PSGetPaperBinNumbers( nLoop ), PSGetPaperBinNames( nLoop ) } )
      NEXT nLoop
   ENDIF

RETURN AClone( aBins )

/*-----------------------------------------------------------------------------
Function ...: PSGetPaperBinCount() -> nValue
Description : Retreive the number of paper bins installed
Author .....: Stephan St-Denis
Date .......: March 2006
-----------------------------------------------------------------------------*/
FUNCTION PSGetPaperBinCount()

   LOCAL nValue := -1

   IF slInitialized
      nValue := oPScript:GetPaperBinCount()
   ENDIF

RETURN nValue

/*-----------------------------------------------------------------------------
Function ...: PSGetPaperBinNames() -> cValue
Description : Retrieve the name of any of the available paper bin
Author .....: Stephan St-Denis
Date .......: March 2006
-----------------------------------------------------------------------------*/
FUNCTION PSGetPaperBinNames( nIndex )

   LOCAL cValue := ""

   IF slInitialized
      cValue := oPScript:GetPaperBinNames( nIndex )
   ENDIF

RETURN cValue

/*-----------------------------------------------------------------------------
Function ...: PSGetPaperBinNumbers() -> nValue
Description : Retrieve the number of any of the available paper bin
Author .....: Stephan St-Denis
Date .......: March 2006
-----------------------------------------------------------------------------*/
FUNCTION PSGetPaperBinNumbers( nIndex )

   LOCAL nValue := -1

   IF slInitialized
      nValue := oPScript:GetPaperBinNumbers( nIndex )
   ENDIF

RETURN nValue

/*-----------------------------------------------------------------------------
Function ...: PSGetPaperCount() -> nValue
Description : Retreive the number of paper installed
Author .....: Stephan St-Denis
Date .......: March 2006
-----------------------------------------------------------------------------*/
FUNCTION PSGetPaperCount()

   LOCAL nValue := -1

   IF slInitialized
      nValue := oPScript:GetPaperCount()
   ENDIF

RETURN nValue

/*-----------------------------------------------------------------------------
Function ...: PSGetPaperNames(<nIndex>) -> cValue
Description : Retreive the name of a paper type based on its index position
Author .....: Stephan St-Denis
Date .......: March 2006
-----------------------------------------------------------------------------*/
FUNCTION PSGetPaperNames( nIndex )

   LOCAL cValue := ""

   IF slInitialized
      cValue := oPScript:GetPaperNames( nIndex )
   ENDIF

RETURN cValue

/*-----------------------------------------------------------------------------
Function ...: PSGetPaperNumbers() -> nValue
Description : Retreive the number of the paper type pointed by nIndex
Author .....: Stephan St-Denis
Date .......: March 2006
-----------------------------------------------------------------------------*/
FUNCTION PSGetPaperNumbers( nIndex )

   LOCAL nValue := -1

   IF slInitialized
      nValue := oPScript:GetPaperNumbers( nIndex )
   ENDIF

RETURN nValue

/*-----------------------------------------------------------------------------
Function ...: PSGetPrinter() -> nValue
Description : Returns the currently selected printer
Author .....: Stephan St-Denis
Date .......: April 2005
-----------------------------------------------------------------------------*/
FUNCTION PSGetPrinter()

   LOCAL nValue := -1

   IF slInitialized
      nValue := oPScript:GetPrinter()
   ENDIF

RETURN nValue

/*-----------------------------------------------------------------------------
Function ...: PSGetPrinterCount() -> nValue
Description : Returns the number of installed printer drivers
Author .....: Stephan St-Denis
Date .......: April 2005
-----------------------------------------------------------------------------*/
FUNCTION PSGetPrinterCount()

   LOCAL nValue := -1

   IF slInitialized
      nValue := oPScript:GetPrinterCount()
   ENDIF

RETURN nValue

/*-----------------------------------------------------------------------------
Function ...: PSGetPrinterNames(nIndex) -> cValue
Description : Returns the name of the printer pointed by nIndex
Author .....: Stephan St-Denis
Date .......: April 2005
-----------------------------------------------------------------------------*/
FUNCTION PSGetPrinterNames( nIndex )

   LOCAL cValue := ""

   IF slInitialized
      cValue := oPScript:GetPrinterNames( nIndex )
   ENDIF

RETURN cValue

/*-----------------------------------------------------------------------------
Function ...: PSGetPrinters() -> aClone(saPrinters)
Description : Returns the list of available printers for this computer
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSGetPrinters()

   LOCAL aPrinters := {}

   IF slInitialized
      aPrinters := oPScript:GetPrinters()
   ENDIF

RETURN aPrinters

/*-----------------------------------------------------------------------------
Function ...: PSGetPrinterCaps() -> aCaps
Description : Retreive the printer capabilities of the selected printer
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSGetPrinterCaps()

   LOCAL aCaps := { -1, -1, -1, -1, -1, -1, -1, -1, -1 }

   IF slInitialized
      aCaps := oPScript:GetPrinterCaps()
   ENDIF

RETURN aCaps

/*-----------------------------------------------------------------------------
Function ...: PSGetPrinterCapsEx(nCap)
Description : Retreive the printer capabilities of the selected printer
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSGetPrinterCapsEx( nCap )

   LOCAL nValue := -1

   IF slInitialized
      nValue := oPScript:GetPrinterCapsEx( nCap )
   ENDIF

RETURN nValue

/*-----------------------------------------------------------------------------
Function ...: PSGetPrinterHandle() -> nValue
Description : Returns the printer handle (for direct API calls)
Author .....: Stephan St-Denis
Date .......: April 2005
-----------------------------------------------------------------------------*/
FUNCTION PSGetPrinterHandle()

   LOCAL nValue := -1

   IF slInitialized
      nValue := oPScript:GetPrinterHandle()
   ENDIF

RETURN nValue

/*-----------------------------------------------------------------------------
Function ...: PSGetPrinterName() -> cValue
Description : Returns the name of the printer number passed in parameter
Author .....: Stephan St-Denis
Date .......: April 2005
-----------------------------------------------------------------------------*/
FUNCTION PSGetPrinterName( nPrinter )

   LOCAL cValue := ""

   IF slInitialized
      cValue := oPScript:GetPrinterName( nPrinter )
   ENDIF

RETURN cValue

/*-----------------------------------------------------------------------------
Function ...: PSGetTextHeight() -> nValue
Description : Returns the height of the text in the current unit
Author .....: Stephan St-Denis
Date .......: April 2005
-----------------------------------------------------------------------------*/
FUNCTION PSGetTextHeight( cText )

   LOCAL nValue := 0

   IF slInitialized
      nValue := oPScript:GetTextHeight( cText )
   ENDIF

RETURN nValue

/*-----------------------------------------------------------------------------
Function ...: PSGetTextWidth() -> nValue
Description : Returns the width of the text in the current unit
Author .....: Stephan St-Denis
Date .......: April 2005
-----------------------------------------------------------------------------*/
FUNCTION PSGetTextWidth( cText )

   LOCAL nValue := 0

   IF slInitialized
      nValue := oPScript:GetTextWidth( cText )
   ENDIF

RETURN nValue

/*-----------------------------------------------------------------------------
Function ...: PSGetTitle() -> cValue
Description : Returns the title of the print job
Author .....: Stephan St-Denis
Date .......: April 2005
-----------------------------------------------------------------------------*/
FUNCTION PSGetTitle()

   LOCAL cValue := ""

   IF slInitialized
      cValue := oPScript:GetTitle()
   ENDIF

RETURN cValue

/*-----------------------------------------------------------------------------
Function ...: PSGetUnit() -> nValue
Description : Returns the unit in use
Author .....: Stephan St-Denis
Date .......: April 2005
-----------------------------------------------------------------------------*/
FUNCTION PSGetUnit()

   LOCAL nValue := -1

   IF slInitialized
      nValue := oPScript:GetUnit()
   ENDIF

RETURN nValue

/*-----------------------------------------------------------------------------
Function ...: PSGetUseDIB() -> lValue
Description : Returns the current setting for DIB (Device Independant Bitmap)
Author .....: Stephan St-Denis
Date .......: April 2005
-----------------------------------------------------------------------------*/
FUNCTION PSGetUseDIB()

   LOCAL lValue := .T.

   IF slInitialized
      lValue := oPScript:GetUseDIB()
   ENDIF

RETURN lValue

/*-----------------------------------------------------------------------------
Function ...: PSGetVersion() -> cValue
Description : Returns the current PageScript version number as a character string
Author .....: Stephan St-Denis
Date .......: April 2006
-----------------------------------------------------------------------------*/
FUNCTION PSGetVersion()

RETURN oPScript:GetVersion()

/*-----------------------------------------------------------------------------
Function ...: PSGetXerox() -> lValue
Description : Returns the current setting for Xerox compatibility
Author .....: Stephan St-Denis
Date .......: April 2005
-----------------------------------------------------------------------------*/
FUNCTION PSGetXerox()

   LOCAL lValue := .F.

   IF slInitialized
      lValue := oPScript:GetXerox()
   ENDIF

RETURN lValue

/*-----------------------------------------------------------------------------
Function ...: PSIsPreviewVisible() -> lValue
Description : Returns .t. if Print Preview Window is shown
Author .....: Stephan St-Denis
Date .......: May 2012
-----------------------------------------------------------------------------*/
FUNCTION PSIsPreviewVisible()

   LOCAL lValue := .F.

   IF slInitialized
      lValue := oPScript:IsPreviewVisible()
   ENDIF

RETURN lValue

/*-----------------------------------------------------------------------------
Function ...: PSLine(<n>, <n>, <n>, <n>, [<n>, <n>]) -> NIL
Description : Draw a line
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSLine( nTop, nLeft, nBottom, nRight, nThick, nBorderColor )

   IF slInitialized
      oPScript:Line( nTop, nLeft, nBottom, nRight, nThick, nBorderColor )
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
Function ...: PSLineEx(<n>, <n>, <n>, <n>) -> NIL
Description : Draw a line
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSLineEx( nTop, nLeft, nBottom, nRight )

   IF slInitialized
      oPScript:LineEx( nTop, nLeft, nBottom, nRight )
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
Function ...: PSNewPage() -> NIL
Description : Signal the end of page, page eject
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSNewPage()

   IF slInitialized
      oPScript:NewPage()
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
Function ...: PSPrintDialog() -> lPrint
Description : Shows the printer dialog and returns if may print or not
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSPrintDialog()

   LOCAL lPrint := .F.

   IF slInitialized
      lPrint := oPScript:PrintDialog()
   ENDIF

RETURN lPrint

/*-----------------------------------------------------------------------------
Function ...: PSPrintFile(<c>, [<l>, <n>, <c>, <n>, <n>, <c>]) -> lSuccess
Description : Prints the file pointed by cFile and optionnaly deletes it.
              EMULATION mode service.
Author .....: Stephan St-Denis
Date .......: September 2002
Note .......: Compatibility
-----------------------------------------------------------------------------*/
FUNCTION PSPrintFile( cFileName, lDelete, nPrinter, cTitle, nOrientation, nCopies, cFont )

   LOCAL lSuccess := .F.

   IF slInitialized
      lSuccess := oPScript:PrintEmuFile( cFileName, lDelete, nPrinter, cTitle, nOrientation, nCopies, cFont )
   ENDIF

RETURN lSuccess

/*-----------------------------------------------------------------------------
Function ...: PSPrintEmuFile(<c>, [<l>, <n>, <c>, <n>, <n>, <c>]) -> lSuccess
Description : Prints the file pointed by cFile and optionnaly deletes it.
              EMULATION mode service.
Author .....: Stephan St-Denis
Date .......: September 2002
-----------------------------------------------------------------------------*/
FUNCTION PSPrintEmuFile( cFileName, lDelete, nPrinter, cTitle, nOrientation, nCopies, cFont )

   LOCAL lSuccess := .F.

   IF slInitialized
      lSuccess := oPScript:PrintEmuFile( cFileName, lDelete, nPrinter, cTitle, nOrientation, nCopies, cFont )
   ENDIF

RETURN lSuccess

/*-----------------------------------------------------------------------------
Function ...: PSPrintRawFile(<c>, [<l>, <n>, <c>]) -> lSuccess
Description : Prints the file pointed by cFile and optionnaly deletes it.
              No processing on file. The file is sent "as is" to the printer.
Author .....: Stephan St-Denis
Date .......: September 2002
-----------------------------------------------------------------------------*/
FUNCTION PSPrintRawFile( cFileName, lDelete, nPrinter, cTitle )

   LOCAL lSuccess := .F.

   IF slInitialized
      lSuccess := oPScript:PrintRawFile( cFileName, lDelete, nPrinter, cTitle )
   ENDIF

RETURN lSuccess

/*-----------------------------------------------------------------------------
Function ...: PSSetAsciiToAnsi([<l>]) -> NIL
Description : Set the conversion from Ascii to Ansi for text strings.
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSSetAsciiToAnsi( lValue )

   LOCAL OldValue := oPScript:GetAsciiToAnsi()

   IF slInitialized
      oPScript:SetAsciiToAnsi( lValue )
   ENDIF

RETURN OldValue

/*-----------------------------------------------------------------------------
Function ...: PSSetBin(<n>) -> NIL
Description : Sets the paper bin
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSSetBin( nPaperBin )

   LOCAL OldValue := oPScript:GetPaperBin()

   PSSetPaperBin( nPaperBin )

RETURN OldValue

/*-----------------------------------------------------------------------------
Function ...: PSSetBorder([<n>, <n>]) -> NIL
Description : Sets the attributes used to draw lines and borders
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSSetBorder( nThickness, nBorderColor )

   LOCAL OldValue := { oPScript:GetBorderThickness(), oPScript:GetBorderColor() }

   IF slInitialized
      oPScript:SetBorder( nThickness, nBorderColor )
   ENDIF

RETURN OldValue

/*-----------------------------------------------------------------------------
Function ...: PSSetBorderColor(<n>) -> NIL
Description : Sets the border color used to draw lines and borders
Author .....: Stephan St-Denis
Date .......: March 2006
-----------------------------------------------------------------------------*/
FUNCTION PSSetBorderColor( nBorderColor )

   LOCAL OldValue := oPScript:GetBorderColor()

   IF slInitialized
      oPScript:SetBorderColor( nBorderColor )
   ENDIF

RETURN OldValue

/*-----------------------------------------------------------------------------
Function ...: PSSetBorderThickness(<n>) -> NIL
Description : Sets the border thickness used to draw lines and borders
Author .....: Stephan St-Denis
Date .......: March 2006
-----------------------------------------------------------------------------*/
FUNCTION PSSetBorderThickness( nThickness )

   LOCAL OldValue := oPScript:GetBorderThickness()

   IF slInitialized
      oPScript:SetBorderThickness( nThickness )
   ENDIF

RETURN OldValue

/*-----------------------------------------------------------------------------
Method .....: #PSSetClipperComp(<l>) -> NIL
Description : Set Clipper compatible calls for TextOut() and TextBox()
-----------------------------------------------------------------------------*/
FUNCTION PSSetClipperComp( lClipper )

   oPScript:SetClipperComp( lClipper )

RETURN NIL

/*-----------------------------------------------------------------------------
Function ...: PSSetCoorSystem(<n>) -> NIL
Description : Sets the Coordinate system
Author .....: Stephan St-Denis
Date .......: March 2006
-----------------------------------------------------------------------------*/
FUNCTION PSSetCoorSystem( nCoor )

   LOCAL OldValue := oPScript:GetCoorSystem()

   IF slInitialized
      oPScript:SetCoorSystem( nCoor )
   ENDIF

RETURN OldValue

/*-----------------------------------------------------------------------------
Function ...: PSSetCopies([<n>]) -> NIL
Description : Sets the number of copies
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSSetCopies( nCopies )

   LOCAL OldValue := oPScript:GetCopies()

   IF slInitialized
      oPScript:SetCopies( nCopies )
   ENDIF

RETURN OldValue

/*-----------------------------------------------------------------------------
Function ...: PSSetCPI([<n>]) -> NIL
Description : Sets the number of characters per inch (Text mode only)
Author .....: Stephan St-Denis
Date .......: April 2005
-----------------------------------------------------------------------------*/
FUNCTION PSSetCPI( nCPI )

   LOCAL OldValue := oPScript:GetCPI()

   IF slInitialized
      oPScript:SetCPI( nCPI )
   ENDIF

RETURN OldValue

/*-----------------------------------------------------------------------------
Function ...: PSSetDecimalSep([<c>]) -> cOldSeparator
Description : Sets the decimal separator character
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSSetDecimalSep( cSep )

   LOCAL OldValue := oPScript:GetDecimalSep()

   IF slInitialized
      oPScript:SetDecimalSep( cSep )
   ENDIF

RETURN OldValue

/*-----------------------------------------------------------------------------
Function ...: PSSetDevice([<n>]) -> NIL
Description : Sets the device for the next print job
Author .....: Stephan St-Denis
Date .......: January 2007
-----------------------------------------------------------------------------*/
FUNCTION PSSetDevice( nDevice )

   IF slInitialized
      oPScript:SetDevice( nDevice )
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
Function ...: PSSetDirectPrint(<l>) -> NIL
Description : Sets the direct print mode (use for very special cases)
Author .....: Stephan St-Denis
Date .......: March 2010
-----------------------------------------------------------------------------*/
FUNCTION PSSetDirectPrint( lDirect )

   IF slInitialized
      oPScript:SetDirectPrint( lDirect )
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
Function ...: PSSetDuplex([<n>]) -> NIL
Description : Sets the Duplex mode
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSSetDuplex( nDuplex )

   LOCAL OldValue := oPScript:GetDuplex()

   IF slInitialized
      oPScript:SetDuplex( nDuplex )
   ENDIF

RETURN OldValue

/*-----------------------------------------------------------------------------
Function ...: PSSetFileName([<c>]) -> NIL
Description : Sets the file name of a PDF file document
Author .....: Stephan St-Denis
Date .......: January 2007
-----------------------------------------------------------------------------*/
FUNCTION PSSetFileName( cFileName )

   IF slInitialized
      oPScript:SetFileName( cFileName )
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
Function ...: PSSetFill(<n>, [<n>]) -> NIL
Description : Sets the color used to fill shapes, like boxes and ellipes
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSSetFill( nFillColor, nFillPattern )

   LOCAL OldValue := { oPScript:GetFillColor(), oPScript:GetFillPattern() }

   IF slInitialized
      oPScript:SetFill( nFillColor, nFillPattern )
   ENDIF

RETURN OldValue

/*-----------------------------------------------------------------------------
Function ...: PSSetFillColor(<n>) -> NIL
Description : Sets the color used to fill shapes, like boxes and ellipes
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSSetFillColor( nFillColor )

   LOCAL OldValue := oPScript:GetFillColor()

   IF slInitialized
      oPScript:SetFillColor( nFillColor )
   ENDIF

RETURN OldValue

/*-----------------------------------------------------------------------------
Function ...: PSSetFillPattern(<n>) -> NIL
Description : Sets the pattern used to fill shapes, like boxes and ellipes
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSSetFillPattern( nFillPattern )

   LOCAL OldValue := oPScript:GetFillPattern()

   IF slInitialized
      oPScript:SetFillPattern( nFillPattern )
   ENDIF

RETURN OldValue

/*-----------------------------------------------------------------------------
Function ...: PSSetFont([<n>, <n>, <n>, <n>, <n>]) -> NIL
Description : Sets the font attributes used by each call to PSTextOut()
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSSetFont( cFont, nStyle, nSize, nTFColor, nTBColor, nAngle )

   LOCAL OldValue := PSSetFontAttributes( cFont, nStyle, nSize, nTFColor, nTBColor, nAngle )

RETURN OldValue

/*-----------------------------------------------------------------------------
Function ...: PSSetFontAttributes([<n>, <n>, <n>, <n>, <n>]) -> NIL
Description : Sets the font attributes used by each call to PSTextOut()
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSSetFontAttributes( cFont, nStyle, nSize, nTFColor, nTBColor, nAngle )

   LOCAL OldValue := { oPScript:GetFontName(), oPScript:GetFontStyle(), oPScript:GetFontSize(), ;
      oPScript:GetFontFColor(), oPScript:GetFontBColor(), oPScript:GetFontAngle() }

   IF slInitialized
      oPScript:SetFontAttributes( cFont, nStyle, nSize, nTFColor, nTBColor, nAngle )
   ENDIF

RETURN OldValue

/*-----------------------------------------------------------------------------
Function ...: PSSetFontAngle(<n>) -> NIL
Description : Sets the font angle
Author .....: Stephan St-Denis
Date .......: March 2006
-----------------------------------------------------------------------------*/
FUNCTION PSSetFontAngle( nValue )

   LOCAL OldValue := oPScript:GetFontAngle()

   IF slInitialized
      oPScript:SetFontAngle( nValue )
   ENDIF

RETURN OldValue

/*-----------------------------------------------------------------------------
Function ...: PSSetFontBColor(<n>) -> NIL
Description : Sets the text background color
Author .....: Stephan St-Denis
Date .......: March 2006
-----------------------------------------------------------------------------*/
FUNCTION PSSetFontBColor( nValue )

   LOCAL OldValue := oPScript:GetFontBColor()

   IF slInitialized
      oPScript:SetFontBColor( nValue )
   ENDIF

RETURN OldValue

/*-----------------------------------------------------------------------------
Function ...: PSSetFontFColor(<n>) -> NIL
Description : Sets the text foreground color
Author .....: Stephan St-Denis
Date .......: March 2006
-----------------------------------------------------------------------------*/
FUNCTION PSSetFontFColor( nValue )

   LOCAL OldValue := oPScript:GetFontFColor()

   IF slInitialized
      oPScript:SetFontFColor( nValue )
   ENDIF

RETURN OldValue

/*-----------------------------------------------------------------------------
Function ...: PSSetFontName(<c>) -> NIL
Description : Sets the text foreground color
Author .....: Stephan St-Denis
Date .......: March 2006
-----------------------------------------------------------------------------*/
FUNCTION PSSetFontName( cValue )

   LOCAL OldValue := oPScript:GetFontName()

   IF slInitialized
      oPScript:SetFontName( cValue )
   ENDIF

RETURN OldValue

/*-----------------------------------------------------------------------------
Function ...: PSSetFontSize(<n>) -> NIL
Description : Sets the text size in points
Author .....: Stephan St-Denis
Date .......: March 2006
-----------------------------------------------------------------------------*/
FUNCTION PSSetFontSize( nValue )

   LOCAL OldValue := oPScript:GetFontSize()

   IF slInitialized
      oPScript:SetFontSize( nValue )
   ENDIF

RETURN OldValue

/*-----------------------------------------------------------------------------
Function ...: PSSetFontStyle(<n>) -> NIL
Description : Sets the font style
Author .....: Stephan St-Denis
Date .......: March 2006
-----------------------------------------------------------------------------*/
FUNCTION PSSetFontStyle( nValue )

   LOCAL OldValue := oPScript:GetFontStyle()

   IF slInitialized
      oPScript:SetFontStyle( nValue )
   ENDIF

RETURN OldValue

/*-----------------------------------------------------------------------------
Function ...: PSSetJustify(<n>) -> NIL
Description : Sets justification for TextOut
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSSetJustify( nJustify )

   LOCAL OldValue := oPScript:GetJustify()

   IF slInitialized
      oPScript:SetJustify( nJustify )
   ENDIF

RETURN OldValue

/*-----------------------------------------------------------------------------
Function ...: PSSetLPI([<n>]) -> NIL
Description : Sets The number of Lines per inch (Text mode only)
Author .....: Stephan St-Denis
Date .......: April 2005
-----------------------------------------------------------------------------*/
FUNCTION PSSetLPI( nLPI )

   LOCAL OldValue := oPScript:GetLPI()

   IF slInitialized
      oPScript:SetLPI( nLPI )
   ENDIF

RETURN OldValue

/*-----------------------------------------------------------------------------
Function ...: PSSetOrientation([<n>]) -> NIL
Description : Sets the paper orientation
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSSetOrientation( nOrientation )

   LOCAL OldValue := oPScript:GetOrientation()

   IF slInitialized
      oPScript:SetOrientation( nOrientation )
   ENDIF

RETURN OldValue

/*-----------------------------------------------------------------------------
Function ...: PSSetPaperBin(<n>) -> NIL
Description : Sets the paper bin
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSSetPaperBin( nPaperBin )

   LOCAL OldValue := oPScript:GetPaperBin()

   IF slInitialized
      oPScript:SetPaperBin( nPaperBin )
   ENDIF

RETURN OldValue

/*-----------------------------------------------------------------------------
Function ...: PSSetPageSize(<n>) -> NIL
Description : Sets the page size to a predifined size
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSSetPageSize( nPageSize )

   LOCAL OldValue := oPScript:GetPageSize()

   IF slInitialized
      oPScript:SetPageSize( nPageSize )
   ENDIF

RETURN OldValue

/*-----------------------------------------------------------------------------
Function ...: PSSetPDFCharSet(<N>) -> NIL
Description : Set the character set to be use when printing text in PDF docs
Author .....: Stephan St-Denis
Date .......: October 2011
-----------------------------------------------------------------------------*/
FUNCTION PSSetPDFCharSet( nCharSet )

   IF slInitialized
      oPScript:SetPDFCharSet( nCharSet )
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
Function ...: PSSetPDFOwnerPassword(<C>) -> NIL
Description : Set the encryption password for PDF documents
Author .....: Stephan St-Denis
Date .......: April 2011
-----------------------------------------------------------------------------*/
FUNCTION PSSetPDFOwnerPassword( cPassword )

   IF slInitialized
      oPScript:SetPDFOwnerPassword( cPassword )
   ENDIF

RETURN NIL


/*-----------------------------------------------------------------------------
Function ...: PSSetPDFEncoding(<N>) -> NIL
Description : Set the internal encoding for PDF documents
Author .....: R.Visscher
Date .......: July 2017
-----------------------------------------------------------------------------*/
FUNCTION PSSetPDFEncoding( nEncoding )

   IF slInitialized
      oPScript:SetPDFEncoding( nEncoding )
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
Function ...: PSSetPDFVersion(<N>) -> NIL
Description : Set the version number for PDF documents
Author .....: R.Visscher
Date .......: July 2017
-----------------------------------------------------------------------------*/
FUNCTION PSSetPDFVersion( nVersion )

   IF slInitialized
      oPScript:SetPDFVersion( nVersion )
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
Function ...: PSShowPDF(<L>) -> NIL
Description : Show PDF after creation in default PDF reader
Author .....: R.Visscher
Date .......: July 2017
-----------------------------------------------------------------------------*/
FUNCTION PSShowPDF( lShow )

   IF slInitialized
      oPScript:ShowPDF( lShow )
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
Function ...: PSSetPDFEmbeddedFonts(<N>) -> NIL
Description : Set the embedding for PDF documents ( none/sub/full )
Author .....: R.Visscher
Date .......: July 2017
-----------------------------------------------------------------------------*/
FUNCTION PSSetPDFEmbeddedFonts( nEmbedded )

   IF slInitialized
      oPScript:SetPDFEmbeddedFonts( nEmbedded )
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
Function ...: PSSetPrinter([<n>]) -> NIL
Description : Sets the printer
Author .....: Stephan St-Denis
Date .......: April 2005
-----------------------------------------------------------------------------*/
FUNCTION PSSetPrinter( nPrinter )

   LOCAL OldValue := oPScript:GetPrinter()

   IF slInitialized
      oPScript:SetPrinter( nPrinter )
   ENDIF

RETURN OldValue

/*-----------------------------------------------------------------------------
Function ...: #PSSetPWState(<n>) -> NIL
Description : Set the print preview window state
Version ....: 2.1.0.0
-----------------------------------------------------------------------------*/
FUNCTION PSSetPWState( nState )

   IF slInitialized
      oPScript:SetPWState( nState )
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
Function ...: PSSetPWPosition(<n>, <n>) -> NIL
Description : Set the print preview window position
Version ....: 2.1.0.0
-----------------------------------------------------------------------------*/
FUNCTION PSSetPWPosition( nLeft, nTop )

   IF slInitialized
      oPScript:SetPWPosition( nLeft, nTop )
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
Function ...: #PSSetPWSize(<n>, <n>) -> NIL
Description : Set the print preview window size (-1, -1 = Auto size)
Version ....: 2.1.0.0
-----------------------------------------------------------------------------*/
FUNCTION PSSetPWSize( nWidth, nHeight )

   IF slInitialized
      oPScript:SetPWSize( nWidth, nHeight )
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
Function ...: #PSSetPWBounds(<n>, <n>, <n>, <n>) -> NIL
Description : Set the print preview window bounds (position and size at the same time)
Version ....: 2.1.0.0
-----------------------------------------------------------------------------*/
FUNCTION PSSetPWBounds( nLeft, nTop, nWidth, nHeight )

   IF slInitialized
      oPScript:SetPWBounds( nLeft, nTop, nWidth, nHeight )
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
Function ...: #PSSetPWZoomLevel(<n>) -> NIL
Description : Set the print preview window zoom level
Version ....: 2.1.0.0
-----------------------------------------------------------------------------*/
FUNCTION PSSetPWZoomLevel( nZoomLevel )

   IF slInitialized
      oPScript:SetPWZoomLevel( nZoomLevel )
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
Function ...: #PSSetPWColors(<n>, <n>, <n>, <n>)) -> NIL
Description : Set the print preview window zoom level
Version ....: 2.1.0.0
-----------------------------------------------------------------------------*/
/*-----------------------------------------------------------------------------
Method .....: #SetPWColors(<n>, <n>, <n>, <n>) -> Self
Description : Set the print preview window colors
Version ....: 3.0.0.0
-----------------------------------------------------------------------------*/
FUNCTION PSSetPWColors( nBackground, nPaper, nShadow, nToolbar )

   IF slInitialized
      oPScript:SetPWColors( nBackground, nPaper, nShadow, nToolbar )
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
Function ...: PSSetRowCol([<n>, <n>]) -> {60, 80}
Description : Sets the number of virtual Rows and Columns in APS_TEXT unit.
Author .....: Stephan St-Denis
Date .......: April 2005
-----------------------------------------------------------------------------*/
FUNCTION PSSetRowCol( nRow, nCol )

   LOCAL nCPI := 10
   LOCAL nLPI := 6

   IF slInitialized
      IF oPScript:nUnit == APS_CLIP
         oPScript:SetRowCol( nRow, nCol )
      ELSE
         DO CASE
         CASE nCol >= 1 .AND. nCol <= 64
            nCPI := 8

         CASE nCol >= 65 .AND. nCol <= 80
            nCPI := 10

         CASE nCol >= 81 .AND. nCol <= 96
            nCPI := 12

         CASE nCol >= 97 .AND. nCol <= 120
            nCPI := 15

         CASE nCol >= 121 .AND. nCol <= 136
            nCPI := 17

         CASE nCol >= 137 .AND. nCol <= 144
            nCPI := 18

         CASE nCol >= 145
            nCPI := 20

         ENDCASE

         DO CASE
         CASE nRow >= 1 .AND. nRow <= 66
            nLPI := 6

         CASE nRow > 66
            nLPI := 8

         ENDCASE

         PSSetCPI( nCPI )
         PSSetLPI( nLPI )
      ENDIF
   ENDIF

RETURN { 60, 80 }

/*-----------------------------------------------------------------------------
Function ...: PSSetTitle([<c>]) -> cOldValue
Description : Sets the document title
Author .....: Stephan St-Denis
Date .......: April 2005
-----------------------------------------------------------------------------*/
FUNCTION PSSetTitle( cTitle )

   LOCAL OldValue := oPScript:GetTitle()

   IF slInitialized
      oPScript:SetTitle( cTitle )
   ENDIF

RETURN OldValue

/*-----------------------------------------------------------------------------
Function ...: PSSetUnit(<n>) -> nOldValue
Description : Sets the unit used to calculate dimensions and placement
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSSetUnit( nUnit )

   LOCAL OldValue := oPScript:GetUnit()

   IF slInitialized
      oPScript:SetUnit( nUnit )
   ENDIF

RETURN OldValue

/*-----------------------------------------------------------------------------
Function ...: PSSetUseDIB(<l>) -> lOldValue
Description : Sets the use of DIB for printing Bitmaps
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSSetUseDIB( lUseDIB )

   LOCAL OldValue := oPScript:GetUseDIB()

   IF slInitialized
      oPScript:SetUseDIB( lUseDIB )
   ENDIF

RETURN OldValue

/*-----------------------------------------------------------------------------
Function ...: PSSetXerox(<l>) -> lOldValue
Description : Sets Xerox compatible mode for Xerox WorkCenter and other
              faulty printers
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSSetXerox( lXerox )

   LOCAL OldValue := oPScript:GetXerox()

   IF slInitialized
      oPScript:SetXerox( lXerox )
   ENDIF

RETURN OldValue

/*-----------------------------------------------------------------------------
Function ...: PSTextBox(<n>, <n>, <n>, <n>, <x>, [<n>, <c>, <n>, <n>, <n>, <n>,
                        <n>, <n>]) -> NIL
Description : Prints a string in a box at position X1,Y1, X2,Y2 using parameters
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSTextBox( nTop, nLeft, nBottom, nRight, cText, nJustify, cFont, nSize, ;
      nStyle, nFColor, nBColor, nThick )

   IF slInitialized
      oPScript:TextBox( nTop, nLeft, nBottom, nRight, cText, nJustify, cFont, nSize, ;
         nStyle, nFColor, nBColor, nThick )
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
Function ...: PSTextBoxEx(<n>, <n>, <n>, <n>, <x>) -> NIL
Description : Prints a string in a box at position X1,Y1, X2,Y2 using parameters
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSTextBoxEx( nTop, nLeft, nBottom, nRight, cText )

   IF slInitialized
      oPScript:TextBoxEx( nTop, nLeft, nBottom, nRight, cText )
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
Function ...: PSTextOut(<n>, <n>, <x>, [<c>, <n>, <n>, <n>, <n>, <n>, <n>, <n>]) -> NIL
Description : Prints a string at position X,Y using parameters
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSTextOut( nTop, nLeft, xValue, cPicture, nJustify, cFont, nSize, nStyle, ;
      nTFColor, nTBColor, nAngle )

   IF slInitialized
      oPScript:TextOut( nTop, nLeft, xValue, cPicture, nJustify, cFont, nSize, ;
         nStyle, nTFColor, nTBColor, nAngle )
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
Function ...: PSTextOutEx(<n>, <n>, <x>, [<c>]) -> NIL
Description : Prints a string at position X,Y using parameters
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSTextOutEx( nTop, nLeft, xValue, cPicture )

   IF slInitialized
      oPScript:TextOutEx( nTop, nLeft, xValue, cPicture )
   ENDIF

RETURN NIL

PROCEDURE PSPrintTotalPages( nTop, nLeft )
   IF slInitialized
      oPScript:PrintTotalPages( nTop, nLeft )
   ENDIF

RETURN
/*-----------------------------------------------------------------------------
Function ...: PSVersion() -> cVersionNumber
Description : Returns the version number
Author .....: Stephan St-Denis
Date .......: March 2005
-----------------------------------------------------------------------------*/
FUNCTION PSVersion()

RETURN oPScript:GetVersion()


/*-----------------------------------------------------------------------------
Function ...: PSPrintPDF -> nError
Description : Print PDF file to printer
Author .....: R.Visscher
Date .......: Jan 2022
-----------------------------------------------------------------------------*/
FUNCTION PSPrintPDF( cFilename, lShowDialog )

   LOCAL nRet := -1
   IF slInitialized
      nRet := oPScript:PrintPDF( cFilename, lShowDialog )
   ENDIF

RETURN nRet

/*-----------------------------------------------------------------------------
Function ...: PSSignPDF -> nError
Description : Digitally sign PDF file
Author .....: R.Visscher
Date .......: Jan 2022
-----------------------------------------------------------------------------*/
FUNCTION PSSignPDF( cFilename, cFilenameOut, cCertFilename, cCertPassword, cLocation, cReason, nPage, nLeft, nTop, nRight, nBottom )

   LOCAL nRet := -1
   IF slInitialized
      nRet := oPScript:SignPDF( cFilename, cFilenameOut, cCertFilename, cCertPassword, cLocation, cReason, nPage, nLeft, nTop, nRight, nBottom )
   ENDIF

RETURN nRet

/*-----------------------------------------------------------------------------
Function ...: PSSetImageQuality
Description : Sets image quality of PDF images default is 50
Author .....: R.Visscher
Date .......: Jan 2022
-----------------------------------------------------------------------------*/
PROCEDURE PSSetImageQuality( nPerc )
   IF slInitialized
      oPScript:SetImageQuality( nPerc )
   ENDIF

RETURN
/*-----------------------------------------------------------------------------
Function ...: PSSetHyperlinks
Description : Enable/disable hyperlinks in PDF file ( http(s)/mail )
Author .....: R.Visscher
Date .......: Jan 2022
-----------------------------------------------------------------------------*/
PROCEDURE PSSetHyperlinks( lActive )
   IF slInitialized
      oPScript:ActiveHyperlinks( lActive )
   ENDIF

RETURN

/*-----------------------------------------------------------------------------
Function ...: PSExport2Txt
Description : Export txt from PDF to file
Author .....: R.Visscher
Date .......: Jan 2022
-----------------------------------------------------------------------------*/
PROCEDURE PSExport2Txt( cFileName, cOutPutFile )
   IF slInitialized
      oPScript:PDF2Text( cFileName, cOutPutFile )
   ENDIF

RETURN

/*-----------------------------------------------------------------------------
Function ...: PSWaterMark([<b>]) -> Previous WaterMark
Description : Sets/Returns the current WaterMark
Author .....: Stephan St-Denis
Date .......: March 2005
Note .......: This funtion is not implemented in PSCRIPT.DLL
-----------------------------------------------------------------------------*/
FUNCTION PSWaterMark( bWaterMark, lWaterMark )

   LOCAL aOldValues := { {|| NIL }, .F. }
   LOCAL nWM := AWM_NONE

   IF slInitialized
      IF ValType( lWaterMark ) = "L"
         IF lWaterMark
            nWM := AWM_FOREGROUND
         ELSE
            nWM := AWM_BACKGROUND
         ENDIF
      ENDIF

      aOldValues := oPScript:WaterMark( bWaterMark, nWM )
   ENDIF

RETURN aOldValues

/*-----------------------------------------------------------------------------
Unsupported / Obsolete functions
-----------------------------------------------------------------------------*/

/*-----------------------------------------------------------------------------
Function ...: PSPrnChanged()
Note .......: This funtion is not implemented in PSCRIPT.DLL
-----------------------------------------------------------------------------*/
FUNCTION PSPrnChanged()

RETURN .F.

/*-----------------------------------------------------------------------------
Function ...: PSRefreshPrinters()
Note .......: This funtion is not implemented in PSCRIPT.DLL
-----------------------------------------------------------------------------*/
FUNCTION PSRefreshPrinters()

RETURN NIL

/*-----------------------------------------------------------------------------
Function ...: PSSendMail()
Note .......: This funtion is not implemented in PSCRIPT.DLL
-----------------------------------------------------------------------------*/
FUNCTION PSSendMail()

RETURN { .F., "" }

/*-----------------------------------------------------------------------------
Function ...: PSSetTimeSlice()
Note .......: This funtion is not implemented in PSCRIPT.DLL
-----------------------------------------------------------------------------*/
FUNCTION PSSetTimeSlice()

RETURN {|| NIL }

/*-----------------------------------------------------------------------------
Function ...: PSSShowIcon()
Note .......: This funtion is not implemented in PSCRIPT.DLL
-----------------------------------------------------------------------------*/
FUNCTION PSShowIcon()

RETURN NIL

/*-----------------------------------------------------------------------------
Function ...: PSShutDown()
Note .......: This funtion is not implemented in PSCRIPT.DLL
-----------------------------------------------------------------------------*/
FUNCTION PSShutDown()

RETURN NIL
