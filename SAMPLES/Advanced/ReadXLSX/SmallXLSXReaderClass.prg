* :encoding=UTF-8:    ąćęłńóśżźĄĆĘŁŃÓŚŻŹ

// https://www.brendanlong.com/the-minimum-viable-xlsx-reader.html 

*****************************************************
*         SmallXLSXReader class - begin             * 
*****************************************************
/*
 A "small" class that reads xlsx files
 
 
 
 oXLSX := SmallXLSXReader():Open( cXlsxFileOpen, bBlockUnZip, bBlockParser, lAttributes )        //Opening an xlsx file
 
 where: 
    cXlsxFileOpen - xlsx filename with path
    bBlockUnZip   - optional code block executed during internal extraction of xml files. The block arguments are:
             { | cFileNameInXlsx, nFileNoInXlsx, nFileCountInXlsx | }
    bBlockParser  - optional block of code executed when reading sharedStrings.xml (shared texts). The block arguments are:
             { | nItemNoProcessed, nNumberOfItems | }
    lAttributes   - optional parameter ( .T. / .F. ) indicating whether to include cell attributes (.T.) or only values (.F.) as a result of reading data
    
 note: when opening an xlsx file, it is decompressed to strings (does not create temporary files).
         First, a file is opened with relations describing under which files individual sheets are stored
         Then the file with the list of sheets contained in the workbook is opened, 
         the name of the xml file is assigned to the names of the sheets (from the relation read above).
         The next file is data formatting styles, based on them, the type of variable (date / time) is determined 
         at the stage of data reading.
         The last file to be opened are strings of texts shared between the sheets (contains the texts contained in the sheets). 
         This can be quite a large file, so I added an optional block of code that is executed during its processing,
         so that I can show the progressbar if necessary
 
 aSheetList := oXLSX:WorkSheetList()             //returns an array with the names of the sheets contained in the cXlsxFileOpen workbook

 aSheetData := oXLSX:WorkSheet( cSheetName, bBlockUnZip, bBlockParser )        //returns an array with data read from the sheet 
 
 where: 
    cSheetName    - sheet name in the workbook
    bBlockUnZip   - optional code block executed during internal extraction of xml files. The block arguments are:
             { | cFileNameInXlsx, nFileNoInXlsx, nFileCountInXlsx | }
    bBlockParser  - an optional block of code executed when reading a sheet file (sheet{n}.xml). The block arguments are:
             { | nItemNoProcessed, nNumberOfItems | }
             
 note: the returned array has the maximum size, taking into account the initial blank rows of the spreadsheet, 
       e.g. data is in cells B3:E9 -> returned array will be of size Array ( 9, 5 ) where 
       rows 1,2 and column 1 (A) will be Nil 

 oXLSX:Close()  - release the variables used in the class to store data from the xlsx file
 
*/

#include "hmg.ch"
#INCLUDE "hbclass.ch"

CREATE CLASS SmallXLSXReader
VAR cXlsxFile  
DATA aRels PROTECTED  
DATA aSheets PROTECTED 
DATA aNumStyle PROTECTED
DATA aSheetData PROTECTED
DATA aSharedStrings PROTECTED
VAR lAttr PROTECTED

METHOD Open( cXlsxFileOpen, bBlockUnZip, bBlockParser, lAttributes )
METHOD WorkSheet( cName, bBlockUnZip, bBlockParser )
METHOD WorkSheetList()
METHOD Close()
METHOD Parser_sheet ( e, n, bBlockParser ) PROTECTED
METHOD ParserExcelCell ( cXmlCell ) PROTECTED
END CLASS

************************************
METHOD Open( cXlsxFileOpen, bBlockUnZip, bBlockParser ,lAttributes ) CLASS SmallXLSXReader
Local cXmlFile, cXmlStyle, aRecords, xf, Xe, nf, f, cRels, cSheetPath, crId, nUniqueCount
::cXlsxFile := cXlsxFileOpen
::aRels := {}  
::aSheets := {}
::aNumStyle := {}
::aSharedStrings := {}
::aSheetData := {}
Default lAttributes := .F. 
::lAttr := lAttributes 

//~//rels IDs
cXmlFile := "xl\_rels\workbook.xml.rels"
cRels := UnZipFileToStr ( ::cXlsxFile , , cXmlFile, bBlockUnZip )
IF Empty ( cRels )
    RETURN self
ENDIF
AEval ( hb_ATokens( hb_StrExtract ( cRels, '<Relationships ', '</Relationships>' ) , '/>' ), { | x | IF( .Not. Empty( x ), AAdd ( ::aRels, { hb_StrExtract ( x, '<Relationship Id="rId', '"' ), hb_StrExtract ( x, ' Target="', '"' ) } ), Nil ) } )

//~// workbook
cXmlFile := "xl\workbook.xml"
AEval ( hb_ATokens( hb_StrExtract ( UnZipFileToStr ( ::cXlsxFile , , cXmlFile, bBlockUnZip ), '<sheets>', '</sheets>' ) , '/>' ), { | x | IF( .Not. Empty( x ), ( cSheetPath := "", crId := hb_StrExtract ( x, ' r:id="rId', '"' ), AEval ( ::aRels, { | r | IF ( r [ 1 ] == crId, cSheetPath := r [ 2 ], Nil ) }), AAdd ( ::aSheets, { hb_StrExtract ( x, '<sheet name="', '"' ), cSheetPath } ) ), Nil ) } )
::aRels := Nil

//~// Number formatting styles
cXmlFile := "xl\styles.xml"
cXmlStyle := UnZipFileToStr ( ::cXlsxFile , , cXmlFile, bBlockUnZip )

AEval ( hb_ATokens( hb_StrExtract ( cXmlStyle, '<cellXfs count=', '</cellXfs>'), '/>'), { | x | ( xf := hb_StrExtract ( x , '<xf numFmtId="', '"' ), IF ( .Not. Empty ( xf ), AAdd (::aNumStyle, IF( xf > "0", { xf, isExcelDateTime ( xf ) }, { "", .F.} ) ), Nil ) ) } )
AEval ( hb_ATokens( hb_StrExtract ( cXmlStyle, '<numFmts count=', '</numFmts>'), '/>'), { | x | ( Xe := x, nf := hb_StrExtract ( x , '<numFmt numFmtId="', '"' ), IF ( .Not. Empty ( nf ), AEval ( ::aNumStyle , { | n, f | IF ( n [ 1 ] == nf, ( ::aNumStyle [ f ] [ 1 ] := hb_StrExtract ( Xe , 'formatCode="', '"'), ::aNumStyle [ f ] [ 2 ] := isExcelDateTime ( ::aNumStyle [ f ] [ 1 ] ) ), Nil ) } ), Nil ) ) } )

//~// Shared strings
cXmlFile := "xl\sharedStrings.xml"
aRecords := hb_ATokens( UnZipFileToStr ( ::cXlsxFile , , cXmlFile, bBlockUnZip ), '</si>')
nUniqueCount := Val ( hb_StrExtract ( aRecords [ 1 ], 'uniqueCount="', '">' ))
IF nUniqueCount = 0
    nUniqueCount := Len ( aRecords ) - 1
ENDIF
::aSharedStrings := Array ( nUniqueCount )
AEval ( ::aSharedStrings, { | e, n | ( ::aSharedStrings [ n ] := Decode_si_t ( aRecords [ n ] ), IF (n % 10000 = 0 , ( IF ( Valtype ( bBlockParser ) = 'B', Eval ( bBlockParser , n , Len ( ::aSharedStrings ) ), Nil ), doEvents() ) , Nil)) }  )
aRecords := Nil

Release Memory
hb_ReleaseCPU()

Return Self

**************************************************************************
METHOD WorkSheetList() CLASS SmallXLSXReader
Local aList := {}
IF Empty ( ::aSheets )
    Return {}
ENDIF
AEval ( ::aSheets, { | x | AAdd ( aList, x [ 1 ] ) } )
Return aList

**************************************************************************
METHOD WorkSheet( cName, bBlockUnZip, bBlockParser ) CLASS SmallXLSXReader
Local nSheet, cXmlFile, aRecords, adr, nRow, nCol

IF Empty ( ::aSheets )
    Return ::aSheetData
ENDIF

nSheet :=  AScan ( ::aSheets , { | x | x [ 1 ] == cName } )

IF nSheet = 0
    MsgStop ( cName + " sheet not found", "SmallXLSXReader:WorkSheet" )
    Return ::aSheetData
ENDIF

//~// Sheet - rows
cXmlFile := "xl\" + ::aSheets [ nSheet ] [ 2 ]
aRecords := hb_ATokens( UnZipFileToStr ( ::cXlsxFile , , cXmlFile, bBlockUnZip ), '</row>')

Release Memory
hb_ReleaseCPU()

//read datasheet size
adr := hb_ATokens( hb_StrExtract ( aRecords [ 1 ], '<dimension ref="', '"/>' ), ":" )
WORKSHEET_RC( adr[ Len ( adr ) ], @nRow, @nCol )
::aSheetData := Array ( nRow, nCol )

AEval ( aRecords, { | e, n | ::Parser_sheet ( e, n, bBlockParser ) } )

aRecords := Nil

Release Memory
hb_ReleaseCPU()

Return ::aSheetData

*******************************************

METHOD Close() CLASS SmallXLSXReader
Release ::aRels 
Release ::aSheets
Release ::aNumStyle
Release ::aSharedStrings
Release ::aSheetData
Return Nil

*********************************************************************************
METHOD Parser_sheet ( e, n, bBlockParser ) CLASS SmallXLSXReader
Local nRow, nCol, hCell
        
IF .Not. '<row r="'$e
    Return Self
ENDIF

//~// Row number
nRow := Val ( hb_StrExtract ( e, '<row r="', '" spans="' ) )

//~// Cells
AEval ( hb_ATokens( e, '<c ' ), { | x, i | if ( i > 1, ( hCell := ::ParserExcelCell ( x ), ;
                                                           WORKSHEET_RC( hCell ["col"], @nRow, @nCol ), ;
                                                           IF ( nRow > Len ( ::aSheetData ), ( ASize( ::aSheetData, nRow ), AEval ( ::aSheetData, { | x, i | IF (x = Nil, ::aSheetData [ i ] := { x }, Nil ) } ) ), Nil ), ;
                                                           IF ( nCol > Len ( ::aSheetData [ nRow ] ), ( ASize( ::aSheetData [ nRow ], nCol ), AEval ( ::aSheetData, { | x, i | IF ( Len ( ::aSheetData [ i ] ) < nCol, ASize ( ::aSheetData [ i ], nCol ), Nil ) } ) ) , Nil ), ;
                                                           ::aSheetData [ nRow ] [ nCol ] := IF ( ::lAttr, hCell, hCell ["value"] ) ) , Nil ) } )

IF n % 10000 = 0 //every 10.000 do events, possibly show the progressbar
    Release Memory
    hb_ReleaseCPU()
    IF Valtype ( bBlockParser ) = 'B'
        Eval ( bBlockParser , n , Len ( ::aSheetData ) )     
    EndIF
    Do Events
ENDIF

RETURN Self
*********************************************************************************
METHOD ParserExcelCell ( cXmlCell ) CLASS SmallXLSXReader
//~// Cell
Local nStyle := Val ( hb_StrExtract ( cXmlCell, 's="', '"' ) )
Local hCell := { "col"      => hb_StrExtract ( cXmlCell, 'r="', '"' ),;
                 "style"    => ::aNumStyle [ nStyle + 1 ] [ 1 ],;
                 "DateTime" => ::aNumStyle [ nStyle + 1 ] [ 2 ],;
                 "type"     => hb_StrExtract ( cXmlCell, 't="', '"' ),;
                 "value"    => hb_StrExtract ( hb_StrExtract ( cXmlCell, '<v', '/v>' ), '>', '<' ),;
                 "formula"  => hb_StrExtract ( hb_StrExtract ( cXmlCell, '<f', '/f>' ), '>', '<' ) }
Local nValue

IF hCell ["type"] == "inlineStr" /* string that doesn't use the shared string table */
    IF '</si>'$cXmlCell
        hCell ["value"] := Decode_si_t ( hb_StrExtract ( cXmlCell, '<si', '</si>' ) )
    ENDIF
ElSEIF .Not. Empty ( hCell ["value"] )

    IF  hCell ["type"] == "str" /* ? The string value returned by the function ? */ .Or. ;
        hCell ["type"] == "inlineStr" /* string that doesn't use the shared string table */ .Or. ;
        hCell ["type"] == "e" /* cell error */
        
            //values of these types do not require conversion
    ELSE

        nValue := Val ( hCell ["value"] )
        
        IF "E" $ hCell ["value"]        //exponential notation
            nValue := &(StrTran ( hCell ["value"], "E", "*10^" ))
        ENDIF
        
        DO CASE
        
            //CASE hCell ["type"] == "n" .Or. hCell ["type"] == ""        //numeric values
            //    hCell ["value"] := nValue
            
            CASE hCell ["type"] == "b"        //boolean values
                hCell ["value"] := ( nValue == 1 )
                
            CASE hCell ["type"] == "d"        //date values in ISO8601
                hCell ["value"] := hb_CtoT( hCell ["value"], "YYYY-MM-DD", "HH:MM:SS.FFF") 
        
            CASE hCell ["type"] == "s"        //values from the sharedStrings array
                hCell ["value"] := ::aSharedStrings [ nValue + 1 ]
                
            //CASE .Not.Empty( hCell ["style"]) .And. isExcelDateTime ( hCell ["style"] )
            CASE hCell ["DateTime"]
                 
                IF    "." $ hCell ["value"]    //time is in
                    IF nValue >= 1        //date and time
                        hCell ["value"] :=  hb_DtoT ( hb_Date( 1900, 01, 01 ) + Int ( nValue ) - 2 ) + hb_SecToT ( (24 * 60 * 60) * ( nValue - Int ( nValue ) ) )
                    ELSE                //time only
                        hCell ["value"] := hb_SecToT ( (24 * 60 * 60) * nValue )
                    ENDIF
                
                ELSE    //date only
                
                    IF nValue = 0
                        hCell ["value"] := ""        //empty date
                    ELSE
                        hCell ["value"] := hb_Date( 1900, 01, 01 ) + nValue - 2
                    ENDIF
                ENDIF
                
            OTHER
                hCell ["value"] := nValue
        
        END CASE
        
    ENDIF
    
ENDIF
    
RETURN hCell
******************************************************************************************
Static Function isExcelDateTime ( cStyle )
// https://learn.microsoft.com/en-us/dotnet/api/documentformat.openxml.spreadsheet.numberingformat?view=openxml-2.8.1

Return Ascan ( {"yy", "m", "d", "h", "s", "AM/PM", "A/P"}, { | x | x $ cStyle } ) > 0 .Or. ;
       AScan ( {"14", "15", "16", "17", "18", "19", "20", "21", "22", "45", "46", "47" } /* predefined styles */ , { | x | x == cStyle } ) > 0
******************************************************************************************
Static Function UnZipFileToStr ( cZipFileName , cPassword, cFileToUnZip, bBlock )
Local i := 0 , hUnzip , nErr, cFile, dDate, cTime, nSize, nCompSize, cComment, nFilesInZip, lCrypted 
Local cBuf := "", cZipFileName_1
IF .Not. File ( cZipFileName )
    RETURN cBuf
ENDIF

IF hb_StrIsUTF8 ( cZipFileName )        //hb_unzip can't handle UTF 8, let's try to convert to 1252
    cZipFileName_1 := hb_utf8ToStr( cZipFileName, "PLWIN" )
ELSE
    cZipFileName_1 := cZipFileName
ENDIF

hUnzip := hb_unzipOpen( cZipFileName_1 )
    
IF Empty( hUnzip  )
    MsgStop ( "Error trying to extract the file " + cZipFileName, "UnZipFileToStr" )
    Return cBuf
EndIf

hb_unzipGlobalInfo( hUnzip, @nFilesInZip, @cComment )

nErr := hb_unzipFileFirst( hUnzip )

DO WHILE nErr == 0

    hb_unzipFileInfo( hUnzip, @cFile, @dDate, @cTime, , , , @nSize, @nCompSize, @lCrypted, @cComment )

    i++
    IF Valtype (bBlock) = 'B'
        Eval ( bBlock , cFile , i, nFilesInZip )     
    EndIF
    
    IF StrTran ( cFileToUnZip, "/", "\" ) == StrTran ( cFile, "/", "\" )
        IF hb_unzipFileOpen( hUnzip, cPassword ) == 0
            cBuf := Space ( nSize )
            hb_unzipFileRead( hUnzip, @cBuf, nSize )
            hb_unzipFileClose( hUnzip )
        ELSE
            MsgStop ( "Error trying to open file " + cFileToUnZip + " in archive " + cZipFileName, "UnZipFileToStr" )
        ENDIF

        hb_unzipClose( hUnzip )
        
        RETURN cBuf
    
    ENDIF

    nErr := hb_unzipFileNext( hUnzip )

ENDDO
hb_unzipClose( hUnzip )
Return cBuf
*******************************************************************
Static Function Decode_si_t ( cSi )
Local cT := ""
AEval ( hb_ATokens( cSi, '</t>'), { | x | IF ( '<t '$x .OR. '<t>'$x, cT := cT + IF ( .Not. Empty ( cT ), CRLF, '' ) + hb_StrExtract ( hb_StrExtract ( x + '</t>', '<t', '</t>', .T. ) + '</t>', '>', '</t>', .T. ) , Nil ) } )
RETURN DecodeHTMLEntities ( cT )
************************************************************************
Static Function DecodeHTMLEntities ( cString )
Local aEntities := { { '&nbsp;'  , ' ' }, ;
                     { '&lt;'    , '<' }, ;
                     { '&gt;'    , '>' }, ;
                     { '&quot;'  , '"' }, ;
                     { '&apos;'  , "'" }, ;
                     { '&brvbar;', "|" }, ;
                     { '&sect;'  , "§" }, ;
                     { '&amp;'   , '&' } }
Local cEntinie, nChar

DO WHILE !Empty ( cEntinie := hb_StrExtract ( cString, '&#', ';' ) )
    IF cEntinie = 'x'
        nChar := &('0' + cEntinie)        //zapis hex
    ELSE
        nChar := Val ( cEntinie )
    ENDIF
    cString :=  StrTran ( cString, '&#' + cEntinie + ';', Chr( nChar ) )
ENDDO
AEVAL( aEntities, { | x | IF ( x [ 1 ]$cString, cString :=  StrTran ( cString, x [ 1 ], x [ 2 ] ), Nil ) } )
Return cString

*****************************************************************************

Static Function hb_StrExtract ( cString, cFromTag, cToTag, lReverse )
Default lReverse := .F.
Return IF( lReverse, hb_TokenGet( hb_TokenGet( cString, 1, cToTag ), 2, cFromTag ), hb_TokenGet( hb_TokenGet( cString, 2, cFromTag ), 1, cToTag ) )

*****************************************************************************

#pragma BEGINDUMP
#include "hbapi.h"
#include <ctype.h>

HB_FUNC( WORKSHEET_RC )
{ 
  const char *cellAddr = hb_parc(1);
  int ii=0, jj, colVal=0;

  while(cellAddr[ii++] >= 'A') {};
  ii--;
  for(jj=0;jj<ii;jj++) colVal = 26*colVal + toupper(cellAddr[jj]) -'A' + 1;

  hb_storni( atoi(cellAddr+ii), 2 );
  hb_storni( colVal, 3 );
}    

#pragma ENDDUMP

****************************************************************
*****************************************************
*         SmallXLSXReader class - end               * 
*****************************************************
