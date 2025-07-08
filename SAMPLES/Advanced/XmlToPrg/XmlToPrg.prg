/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Demo was contributed to HMG forum by Edward 30/Dec/2021
 *
 * Adapted for MiniGUI Extended Edition by Grigory Filatov
 */

#define RT_RCDATA  10
#define cTab       Chr (9)

Memvar aXmlStru

/* Below are special comments that allow you to identify the source code snippets and use them to self-generate a new .prg. Please do not modify or delete them. */ 
** Code snippet #1 **
#require "hbmxml.hbc"

#include "hmg.ch"
#include "hbmxml.ch"

Function Main
** Code snippet #1 - End **

DEFINE WINDOW Form_1 ;
	AT 0,0 ;
	WIDTH 1100 ;
	HEIGHT 660 ;
	TITLE 'XML To Prg Generator' ;
	MAIN
	
	DEFINE MAIN MENU
		DEFINE POPUP 'Xml File'
			MENUITEM 'Generate Do_Case.prg from the XML file structure.' ACTION Make_DO_CASE()
			MENUITEM 'Generate almost ready to use .prg from the XML file structure.' ACTION Make_DO_CASE( 1 )
			MENUITEM 'Convert XML to an Array - example' ACTION xml2Array()
		END POPUP
	END MENU
	  
	@ 10,10 RICHEDITBOXEX Rich_edit_1 ;
            WIDTH 1050 ;
            HEIGHT 580 ;
            VALUE "" ;
            FONT "Consolas" ;
            SIZE 9 
            
     Form_1.Rich_edit_1.AutoURLDetect := .F.

END WINDOW

ACTIVATE WINDOW Form_1

Return Nil

******************************************************
Function Xml2Array ()
Local cXmlString, cXmlFile, aXML := {}, i, cPath, cKey, xValue, hAttrib

Form_1.Rich_edit_1.Value := ""

cXmlFile := GetFile( {{'XML File', '*.xml'}}, 'Select XML', hb_CWD(), .F. , .T. ) 

IF EMPTY( cXmlFile )
	RETURN Nil
ENDIF

cXmlString := FileStr( cXmlFile )                                                           

hb_XMLParser( cXmlString, .T., { | aRowXML | AAdd ( aXml, aRowXML) } )

FOR i := 1 TO Len ( aXML )
	cPath   := aXML [ i ] [ 1 ]
	cKey    := aXML [ i ] [ 2 ]
	xValue  := aXML [ i ] [ 3 ]
	hAttrib := aXML [ i ] [ 4 ]
	
	IF i == 1
		Form_1.Rich_edit_1.FontBold := .T.
		Form_1.Rich_edit_1.AddText( Form_1.Rich_edit_1.GetTextLength ) := cTab + "--- First ---" + CRLF 
		Form_1.Rich_edit_1.FontBold := .F.
	ENDIF
	
	Form_1.Rich_edit_1.AddText( Form_1.Rich_edit_1.GetTextLength ) := 'Key Path:' + cTab + '"' + cPath + '"' + CRLF + CRLF + ;
		'Key:' + cTab + cTab + '"' + cKey + '"' + CRLF + CRLF + ;
		'Value:' + cTab + cTab + Left ( hb_ValToStr( xValue ), 100 ) /* first 100 chars for test only */ + CRLF + CRLF
	IF Len( hAttrib ) > 0 
		Form_1.Rich_edit_1.AddText( Form_1.Rich_edit_1.GetTextLength ) := 'Attribute(s):' + cTab +hb_JsonEncode ( hAttrib ) + CRLF
	ENDIF
	
	Form_1.Rich_edit_1.FontBold := .T.
	Form_1.Rich_edit_1.AddText( Form_1.Rich_edit_1.GetTextLength ) := cTab + IF( i < Len ( aXML ), "--- Next ---", "--- Last ---" ) + CRLF 
	Form_1.Rich_edit_1.FontBold := .F.
	
NEXT i
Return Nil

*********************************************************
Function Make_DO_CASE( nMode )
Local cXmlString, cXmlFile
Local cPrgFile, cBody := "", cSourcePrg := RCDataToMem ( 'SourcePrg', RT_RCDATA )
Default nMode := 0

Form_1.Rich_edit_1.Value := ""

** Code snippet #2 **
cXmlFile := GetFile( {{'XML File', '*.xml'}}, 'Select XML', hb_CWD(), .F. , .T. ) 

IF EMPTY( cXmlFile )
	RETURN Nil
ENDIF
** Code snippet #2 - End **

IF nMode = 0 //Do Case only
	cPrgFile := "Do_Case.prg"
ELSE         //nMode = 1 - Almost ready to use .prg
	cPrgFile := hb_FNameName ( cXmlFile ) + ".prg"
ENDIF

IF nMode == 1
	cBody += hb_StrExtract ( cSourcePrg, "** " + "Code snippet #1 **" /* I write it this way to avoid finding myself */, "** " + "Code snippet #1 - End **" /* I write it this way to avoid finding myself */ ) + CRLF + ;
		"Local cXmlFile" + CRLF + ; 
		hb_StrExtract ( cSourcePrg, "** " + "Code snippet #2 **" /* I write it this way to avoid finding myself */, "** " + "Code snippet #2 - End **" /* I write it this way to avoid finding myself */) + CRLF + ;
		"hb_XMLParser( hb_MemoRead ( cXmlFile ), .T., { | aRowXML | Parser_" +  + hb_FNameName ( cXmlFile ) + " ( aRowXML ) } )" + CRLF + CRLF + ;
		"Return Nil" + CRLF + ;
		"*******************************************************"  + CRLF +;
		"Function Parser_" + hb_FNameName ( cXmlFile ) + " ( aXmlOneRow ) " + CRLF + ;
		hb_StrExtract ( cSourcePrg, "** " + "Code snippet #3 **" /* I write it this way to avoid finding myself */, "** " + "Code snippet #3 - End **" /* I write it this way to avoid finding myself */) + CRLF 
ENDIF

cBody +=  "****** DO CASE for the " + hb_FNameNameExt ( cXmlFile ) + " file structure ********" + CRLF + ;
		"DO CASE" + CRLF

STRFILE (cBody, cPrgFile, .F. )

Private aXmlStru := {}

cXmlString := FileStr( cXmlFile )                                                           

hb_XMLParser( cXmlString, .T., { | aRowXML | MakeItInRunTime ( aRowXML, cPrgFile ) } )

cBody := "END CASE" + CRLF 

IF nMode = 1 
	cBody += "Return" + CRLF + ;
	 		hb_StrExtract ( cSourcePrg, "** " + "Code snippet #4 **" /* I write it this way to avoid finding myself */, "** " + "Code snippet #4 - End **" /* I write it this way to avoid finding myself */)
ENDIF
	 
STRFILE ( cBody , cPrgFile, .T. )

Form_1.Rich_edit_1.Value := hb_MemoRead ( cPrgFile )

Return Nil

**********************************************************
Function MakeItInRunTime ( aXmlOneRow, cFile )
Local Attrib_Key, cPath, cKey, xValue, hAttrib
** Code snippet #3 **Local cPath, cKey, xValue, hAttrib, xAttribValue 

cPath   := aXmlOneRow [ 1 ]
cKey    := aXmlOneRow [ 2 ]
xValue  := aXmlOneRow [ 3 ]
hAttrib := aXmlOneRow [ 4 ]
** Code snippet #3 - End **

IF hb_AScan ( aXmlStru, cPath + "/" + cKey, , , .T. ) = 0	//structure path not generated, add ones
	AAdd( aXmlStru, cPath + "/" + cKey)
	STRFILE ( cTab + 'CASE cKey == "' + cKey + '" .AND. cPath == "' + cPath + '"' + CRLF + cTab + cTab + CRLF , cFile , .T. )
	IF LEN( hAttrib ) > 0
		FOR EACH Attrib_Key IN hb_HKeys ( hAttrib )
			STRFILE ( cTab + cTab + 'IF hb_HHasKey( hAttrib, "' + Attrib_Key + '" )' + CRLF + ;
					cTab + cTab + cTab + 'xAttribValue := hAttrib ["' + Attrib_Key + '"] ' + CRLF + cTab + cTab + cTab + CRLF + ; 
					cTab + cTab + 'ENDIF' + CRLF + CRLF , cFile, .T. )			
		NEXT 
	ENDIF
ENDIF
	
Return Nil


** Code snippet #4 **
*****************************************************************************

STATIC FUNCTION hb_XMLParser( cXML, lOmitHeader, xFunction )
LOCAL pXML

Default lOmitHeader := .T.

pXML := mxmlLoadString( NIL, hb_utf8ToStr( cXML ), MXML_OPAQUE_CALLBACK )
IF !Empty( pXML )
	IF lOmitHeader
		hb_XMLParser_getnodes( mxmlGetFirstChild( pXML ), xFunction )
	ELSE
		hb_XMLParser_getnodes( pXML, xFunction )
	ENDIF
ENDIF

mxmlDelete( pXML )

RETURN Nil

*****************************************************************************

STATIC FUNCTION hb_XMLParser_getnodes( pNode, xFunction )
LOCAL cKey, pChild, xResult, xValue, cPrevKey, hAttrib, pParrentNode, cPath

DO WHILE !Empty( pNode )

	IF mxmlGetType( pNode ) == MXML_ELEMENT
		
		cKey   := mxmlGetElement( pNode )
		pChild := mxmlGetFirstChild( pNode )
		
		IF hb_mxmlGetAttrsCount ( pNode ) > 0 .AND. !Empty( pChild ) .AND. ( mxmlGetOpaque( pNode ) = CRLF .OR. Empty ( mxmlGetOpaque( pNode ) ) )	//Support nodes with child node and attribute(s) and no value
			
			cPath := ""
			pParrentNode := mxmlGetParent( pNode )
				
			IF Left ( cKey, 8 ) == "![CDATA["
				cKey         := mxmlGetElement( mxmlGetParent( pNode ) )
				pParrentNode := mxmlGetParent( mxmlGetParent( pNode ) )
			ENDIF
				
			DO WHILE !EMPTY ( pParrentNode )
				cPrevkey := mxmlGetElement( pParrentNode )
				IF Left ( cPrevkey, 4 ) <> "?xml"
					cPath := cPrevkey + IF( !Empty( cPath ), "/", "") + cPath 
				ENDIF
				pParrentNode := mxmlGetParent( pParrentNode )
			ENDDO
			hAttrib := hb_mxmlGetAttrs( pNode )
			IF HB_ISEVALITEM( xFunction )
				Eval( xFunction, { cPath, cKey, Nil, hAttrib } )
			ENDIF
		
		ENDIF
		
		xValue := hb_XMLParser_getnodes( pChild, xFunction )

		IF xValue == NIL
			xValue := mxmlGetOpaque( pNode )
			
			IF !(xValue == CRLF)
				cPath        := ""
				pParrentNode := mxmlGetParent( pNode )
				
				IF Left ( cKey, 8 ) == "![CDATA["
					xValue       := hb_StrShrink ( mxmlGetCDATA( pNode ), 2 )
					cKey         := mxmlGetElement( mxmlGetParent( pNode ) )
					pParrentNode := mxmlGetParent( mxmlGetParent( pNode ) )
				ENDIF
				
				DO WHILE !EMPTY ( pParrentNode )
					cPrevkey := mxmlGetElement( pParrentNode )
					IF Left ( cPrevkey, 4 ) <> "?xml"
						cPath := cPrevkey + IF( !Empty( cPath ), "/", "") + cPath 
					ENDIF
					pParrentNode := mxmlGetParent( pParrentNode )
				ENDDO
				hAttrib := hb_mxmlGetAttrs( pNode )
				IF HB_ISEVALITEM( xFunction )
					Eval( xFunction, { cPath, cKey, xValue, hAttrib } )
				ENDIF
				xResult := { cPath, cKey, xValue, hAttrib } 
			ENDIF
		ENDIF
		
	ENDIF
	pNode := mxmlGetNextSibling( pNode )
ENDDO
RETURN xResult
****************************************************************
** Code snippet #4 - End **

****************************************************************
Static Function hb_StrExtract ( cString, cFrom, cTo )
Local nFromPos, nToPos
Default cFrom := "", cTo := ""
nFromPos := hb_utf8At ( Upper( cFrom ), Upper ( cString ) ) + hb_utf8Len( cFrom )
nToPos   := hb_utf8RAt( Upper( cTo )  , Upper ( cString ) ) - nFromPos
Return hb_utf8Substr( cString, nFromPos, nToPos )
