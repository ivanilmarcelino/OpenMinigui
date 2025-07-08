/*
 * $Id: hxmldoc.prg 2868 2020-08-19 11:23:28Z alkresin $
 *
 * Harbour XML Library
 * HXmlDoc class
 *
 * Copyright 2003 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
*/

#include "hbclass.ch"
#include "fileio.ch"
#include "i_xml.ch"

#define XML_ERROR_FILEOPEN     11

STATIC cNewLine := e"\r\n"

/*
 *  CLASS DEFINITION
 *  HXMLNode
 */

CLASS HXMLNode

   CLASS VAR nLastErr SHARED
   DATA title
   DATA type
   DATA aItems INIT {}
   DATA aAttr INIT {}
   DATA cargo

   METHOD New( cTitle, type, aAttr, cValue )
   METHOD Add( xItem )
   METHOD GetAttribute( cName, cType, xDefault )
   METHOD SetAttribute( cName, cValue )
   METHOD DelAttribute( cName )
   METHOD Save( handle, level )
   METHOD Find( cTitle,nStart,block )

ENDCLASS

METHOD New( cTitle, type, aAttr, cValue ) CLASS HXMLNode

   IF cTitle != NIL ; ::title := cTitle ; ENDIF
   IF aAttr != NIL ; ::aAttr := aAttr ; ENDIF
   ::type := iif( type != NIL, type, HBXML_TYPE_TAG )
   IF cValue != NIL
      ::Add( cValue )
   ENDIF

RETURN Self

METHOD Add( xItem ) CLASS HXMLNode

   AAdd( ::aItems, xItem )

RETURN xItem

METHOD GetAttribute( cName, cType, xDefault ) CLASS HXMLNode

   LOCAL i := AScan( ::aAttr, {| a | a[ 1 ] == cName } )

   IF i != 0
      IF cType == NIL .OR. cType == "C"
         RETURN ::aAttr[ i, 2 ]
      ELSEIF cType == "N"
         RETURN Val( ::aAttr[ i, 2 ] )
      ELSEIF cType == "L"
         RETURN ( Lower( ::aAttr[ i, 2 ] ) $ ".t.;on;yes;true" )
      ENDIF
   ENDIF

RETURN xDefault

METHOD SetAttribute( cName, cValue ) CLASS HXMLNode

   LOCAL i := AScan( ::aAttr, {| a | a[ 1 ] == cName } )

   IF i == 0
      AAdd( ::aAttr, { cName, cValue } )
   ELSE
      ::aAttr[ i, 2 ] := cValue
   ENDIF

RETURN .T.

METHOD DelAttribute( cName ) CLASS HXMLNode

   LOCAL i := AScan( ::aAttr, {| a | a[ 1 ] == cName } )

   IF i != 0
      ADel( ::aAttr, i )
      ASize( ::aAttr, Len( ::aAttr ) - 1 )
   ENDIF

RETURN .T.

METHOD Save( handle, level ) CLASS HXMLNode

   LOCAL i, s := Space( level * 2 ) + '<', lNewLine

   IF ! __mvExist( "HXML_NEWLINE" )
      __mvPrivate( "HXML_NEWLINE" )
      __mvPut( "HXML_NEWLINE", .T. )
   ENDIF
   lNewLine := m->hxml_newline
   IF ::type == HBXML_TYPE_COMMENT
      s += '!--'
   ELSEIF ::type == HBXML_TYPE_CDATA
      s += '![CDATA['
   ELSEIF ::type == HBXML_TYPE_PI
      s += '?' + ::title
   ELSE
      s += ::title
   ENDIF
   IF ::type == HBXML_TYPE_TAG .OR. ::type == HBXML_TYPE_SINGLE
      FOR i := 1 TO Len( ::aAttr )
         // s += ' ' + ::aAttr[i,1] + '="' + HBXML_PreSave(::aAttr[i,2]) + '"'
         s += ' ' + ::aAttr[ i, 1 ] + '="' + ::aAttr[ i, 2 ] + '"'
      NEXT
   ENDIF
   IF ::type == HBXML_TYPE_PI
      s += '?>' + cNewLine
      m->hxml_newline := .T.
   ELSEIF ::type == HBXML_TYPE_SINGLE
      s += '/>' + cNewLine
      m->hxml_newline := .T.
   ELSEIF ::type == HBXML_TYPE_TAG
      s += '>'
      IF Empty( ::aItems ) .OR. ( Len( ::aItems ) == 1 .AND. ;
            ValType( ::aItems[ 1 ] ) == "C" .AND. Len( ::aItems[ 1 ] ) + Len( s ) < 80 )
         lNewLine := m->hxml_newline := .F.
      ELSE
         s += cNewLine
         lNewLine := m->hxml_newline := .T.
      ENDIF
   ENDIF
   IF handle >= 0
      FWrite( handle, s )
   ENDIF

   FOR i := 1 TO Len( ::aItems )
      IF ValType( ::aItems[ i ] ) == "C"
         IF handle >= 0
            IF ::type == HBXML_TYPE_CDATA .OR. ::type == HBXML_TYPE_COMMENT
               FWrite( handle, ::aItems[ i ] )
            ELSE
               FWrite( handle, HBXML_PreSave( ::aItems[ i ] ) )
            ENDIF
            IF lNewLine .AND. Right( ::aItems[ i ], 1 ) != Chr( 10 )
               FWrite( handle, cNewLine )
            ENDIF
         ELSE
            IF ::type == HBXML_TYPE_CDATA .OR. ::type == HBXML_TYPE_COMMENT
               s += ::aItems[ i ]
            ELSE
               s += HBXML_PreSave( ::aItems[ i ] )
            ENDIF
            IF lNewLine .AND. Right( s, 1 ) != Chr( 10 )
               s += cNewLine
            ENDIF
         ENDIF
         m->hxml_newline := .F.
      ELSE
         s += ::aItems[ i ]:Save( handle, level + 1 )
      ENDIF
   NEXT
   m->hxml_newline := .T.
   IF handle >= 0
      IF ::type == HBXML_TYPE_TAG
         FWrite( handle, iif( lNewLine, Space( level * 2 ), "" ) + '</' + ::title +'>' + cNewLine )
      ELSEIF ::type == HBXML_TYPE_CDATA
         FWrite( handle, ']]>' + cNewLine )
      ELSEIF ::type == HBXML_TYPE_COMMENT
         FWrite( handle, '-->' + cNewLine )
      ENDIF
   ELSE
      IF ::type == HBXML_TYPE_TAG
         s += iif( lNewLine, Space( level * 2 ), "" ) + '</' + ::title +'>' + cNewLine
      ELSEIF ::type == HBXML_TYPE_CDATA
         s += ']]>' + cNewLine
      ELSEIF ::type == HBXML_TYPE_COMMENT
         s += '-->' + cNewLine
      ENDIF
      RETURN s
   ENDIF

RETURN ""

METHOD Find( cTitle, nStart, block ) CLASS HXMLNode

   LOCAL i

   IF nStart == NIL
      nStart := 1
   ENDIF
   DO WHILE .T.
      i := AScan( ::aItems, {| a | ValType( a ) != "C" .AND. a:title == cTitle }, nStart )
      IF i == 0
         EXIT
      ELSE
         nStart := i
         IF block == NIL .OR. Eval( block, ::aItems[ i ] )
            RETURN ::aItems[ i ]
         ELSE
            nStart++
         ENDIF
      ENDIF
   ENDDO

RETURN NIL


/*
 *  CLASS DEFINITION
 *  HXMLDoc
 */

CLASS HXMLDoc INHERIT HXMLNode

   METHOD New( encoding )
   METHOD Read( fname, buffer )
   METHOD ReadString( buffer ) INLINE ::Read( , buffer )
   METHOD Save( fname, lNoHeader )
   METHOD Save2String() INLINE ::Save()

ENDCLASS

METHOD New( encoding ) CLASS HXMLDoc

   IF encoding != NIL
      AAdd( ::aAttr, { "version", "1.0" } )
      AAdd( ::aAttr, { "encoding", encoding } )
   ENDIF

RETURN Self

METHOD Read( fname, buffer ) CLASS HXMLDoc

   LOCAL han

   IF fname != NIL
      han := FOpen( fname, FO_READ )
      ::nLastErr := 0
      IF han != -1
         ::nLastErr := hbxml_GetDoc( Self, han )
         FClose( han )
      ELSE
         ::nLastErr := XML_ERROR_FILEOPEN
      ENDIF
   ELSEIF buffer != NIL
      ::nLastErr := hbxml_GetDoc( Self, buffer )
   ELSE
      RETURN NIL
   ENDIF

RETURN iif( ::nLastErr == 0, Self, Nil )

METHOD Save( fname, lNoHeader ) CLASS HXMLDoc

   LOCAL handle := -2
   LOCAL cEncod, i, s

   IF fname != NIL
      handle := FCreate( fname )
   ENDIF
   IF handle != -1
      IF lNoHeader == NIL .OR. ! lNoHeader
         IF ( cEncod := ::GetAttribute( "encoding" ) ) == NIL
            cEncod := "UTF-8"
         ENDIF
         s := '<?xml version="1.0" encoding="' + cEncod + '"?>' + cNewLine
         IF fname != NIL
            FWrite( handle, s )
         ENDIF
      ELSE
         s := ""
      ENDIF
      FOR i := 1 TO Len( ::aItems )
         s += ::aItems[ i ]:Save( handle, 0 )
      NEXT
      IF fname != NIL
         FClose( handle )
      ELSE
         RETURN s
      ENDIF
   ENDIF

RETURN .T.
* ======================= EOF of hxmldoc.prg ========================
