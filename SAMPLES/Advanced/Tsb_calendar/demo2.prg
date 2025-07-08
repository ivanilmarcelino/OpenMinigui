/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Author: Igor Nazarov
 *
 * Revised by Grigory Filatov <gfilatov@inbox.ru>
*/

#include "minigui.ch"
#include "tsbrowse.ch"

FUNCTION Main()

   LOCAL i, j, n, oBrw, hFont
   LOCAL dFirst := FindSunday( Date() )
   LOCAL aDay := {}

   SET TOOLTIPSTYLE BALLOON

   DEFINE FONT Font_1 FONTNAME "Arial" SIZE 10
   hFont := GetFontHandle( "Font_1" )

   SET DATE TO GERMAN

   FT_DATECNFG( , 2 )

   FOR i := 1 TO 7
      AAdd( aDay, CDoW( ++dFirst ) )
   NEXT

   dFirst := Date()

   DEFINE WINDOW Win_1 ;
      At 0, 0 ;
      WIDTH 600 ;
      HEIGHT 520 ;
      TITLE 'TsBrowse Calendar - ' + Upper( CMonth( dFirst ) ) + Str( Year( dFirst ) ) ;
      ICON 'calendar.ico' ;
      MAIN ;
      NOMAXIMIZE ;
      NOSIZE ;
      ON GOTFOCUS oBrw:SetFocus()

      ON KEY ESCAPE ACTION ThisWindow.Release

   END WINDOW

   DEFINE TBrowse oBrw ;
      AT     GetProperty( "Win_1", 'Row' ) + 20, GetProperty( "Win_1", 'Col' ) + 15 ;
      OF     Win_1 ;
      WIDTH  GetProperty( "Win_1", 'Width' ) - 38 ;
      HEIGHT GetProperty( "Win_1", 'Height' ) - 68 ;
      FONT   "Arial" ;
      SIZE   18 BOLD ;
      GRID

   END TBROWSE

   oBrw:SetArray( Array( 6, 7 ), TRUE )

   // add an owner data to TSBrowse class
   __objAddData( oBrw, 'aMark' )
   __objAddData( oBrw, 'aDate' )

   oBrw:aMark := Array( 6, 7 )
   oBrw:aDate := Array( 6, 7 )

   dFirst := FT_ACCTWEEK( BOM( dFirst ) )[ 2 ]

   n := 0
   FOR i := 1 TO 6
      FOR j := 1 TO 7
         oBrw:aDate[ i ][ j ] := dFirst + n
         IF Day( oBrw:aDate[ i ][ j ] ) == Day( Date() ) .AND. Month( oBrw:aDate[ i ][ j ] ) == Month( Date() )
            oBrw:aMark[ i ][ j ] := TRUE
            oBrw:lInitGoTop := .F.
            oBrw:GoPos( i, j )  // select Today cell
         ELSE
            oBrw:aMark[ i ][ j ] := FALSE
         ENDIF
         n++
      NEXT
   NEXT

   oBrw:nHeightCell  := 70
   oBrw:nHeightHead  := 30
   oBrw:lNoHScroll   := .T.
   oBrw:nFreeze      := 7
   oBrw:lNoMoveCols  := .T.
   oBrw:lLockFreeze  := FALSE
   oBrw:lNoChangeOrd := TRUE
   oBrw:nFireKey     := VK_SPACE
   oBrw:cToolTip     := {|oBr, nCol, nRow| if( nRow > 0, ( dFirst := oBr:aDate[ nRow ][ nCol ], ;
                        CDoW( dFirst ) + ", " + CMonth( dFirst ) + " " + PadL( Day( dFirst ), 2, " " ) ), ) }

   oBrw:SetColor( { CLR_HEADF }, { {|| RGB( 240, 255, 255 ) } },  )

   oBrw:SetColor( { CLR_HEADB }, { {|| RGB( 130, 130, 65 )  } },  )

   oBrw:SetColor( { CLR_FOCUSB }, { {|| -RGB( 130, 130, 65 ) } },  )

   oBrw:SetColor( { CLR_PANE }, { {|| IF( IsMark( 1 ),  RGB( 255, 255, 0 ),  RGB( 205, 205, 155 ) ) } }, 1 )
   oBrw:SetColor( { CLR_PANE }, { {|| IF( IsMark( 2 ),  RGB( 255, 255, 0 ),  RGB( 205, 205, 155 ) ) } }, 2 )
   oBrw:SetColor( { CLR_PANE }, { {|| IF( IsMark( 3 ),  RGB( 255, 255, 0 ),  RGB( 205, 205, 155 ) ) } }, 3 )
   oBrw:SetColor( { CLR_PANE }, { {|| IF( IsMark( 4 ),  RGB( 255, 255, 0 ),  RGB( 205, 205, 155 ) ) } }, 4 )
   oBrw:SetColor( { CLR_PANE }, { {|| IF( IsMark( 5 ),  RGB( 255, 255, 0 ),  RGB( 205, 205, 155 ) ) } }, 5 )
   oBrw:SetColor( { CLR_PANE }, { {|| IF( IsMark( 6 ),  RGB( 255, 255, 0 ),  RGB( 205, 205, 155 ) ) } }, 6 )
   oBrw:SetColor( { CLR_PANE }, { {|| IF( IsMark( 7 ),  RGB( 255, 255, 0 ),  RGB( 205, 205, 155 ) ) } }, 7 )

   oBrw:SetColor( { CLR_TEXT }, { {|| IF( Month( oBrw:aDate[ oBrw:nAt ][ 1 ] ) == Month( Date() ),  RGB( 0, 0, 0 ),  RGB( 128, 128, 128 ) ) } }, 1 )
   oBrw:SetColor( { CLR_TEXT }, { {|| IF( Month( oBrw:aDate[ oBrw:nAt ][ 2 ] ) == Month( Date() ),  RGB( 0, 0, 0 ),  RGB( 128, 128, 128 ) ) } }, 2 )
   oBrw:SetColor( { CLR_TEXT }, { {|| IF( Month( oBrw:aDate[ oBrw:nAt ][ 3 ] ) == Month( Date() ),  RGB( 0, 0, 0 ),  RGB( 128, 128, 128 ) ) } }, 3 )
   oBrw:SetColor( { CLR_TEXT }, { {|| IF( Month( oBrw:aDate[ oBrw:nAt ][ 4 ] ) == Month( Date() ),  RGB( 0, 0, 0 ),  RGB( 128, 128, 128 ) ) } }, 4 )
   oBrw:SetColor( { CLR_TEXT }, { {|| IF( Month( oBrw:aDate[ oBrw:nAt ][ 5 ] ) == Month( Date() ),  RGB( 0, 0, 0 ),  RGB( 128, 128, 128 ) ) } }, 5 )
   oBrw:SetColor( { CLR_TEXT }, { {|| IF( Month( oBrw:aDate[ oBrw:nAt ][ 6 ] ) == Month( Date() ),  RGB( 0, 0, 0 ),  RGB( 128, 128, 128 ) ) } }, 6 )
   oBrw:SetColor( { CLR_TEXT }, { {|| IF( Month( oBrw:aDate[ oBrw:nAt ][ 7 ] ) == Month( Date() ),  RGB( 0, 0, 0 ),  RGB( 128, 128, 128 ) ) } }, 7 )

   oBrw:SetColor( { CLR_FOCUSF }, { {|| IF( Month( oBrw:aDate[ oBrw:nAt ][ 1 ] ) == Month( Date() ),  RGB( 0, 0, 0 ),  RGB( 128, 128, 128 ) ) } }, 1 )
   oBrw:SetColor( { CLR_FOCUSF }, { {|| IF( Month( oBrw:aDate[ oBrw:nAt ][ 2 ] ) == Month( Date() ),  RGB( 0, 0, 0 ),  RGB( 128, 128, 128 ) ) } }, 2 )
   oBrw:SetColor( { CLR_FOCUSF }, { {|| IF( Month( oBrw:aDate[ oBrw:nAt ][ 3 ] ) == Month( Date() ),  RGB( 0, 0, 0 ),  RGB( 128, 128, 128 ) ) } }, 3 )
   oBrw:SetColor( { CLR_FOCUSF }, { {|| IF( Month( oBrw:aDate[ oBrw:nAt ][ 4 ] ) == Month( Date() ),  RGB( 0, 0, 0 ),  RGB( 128, 128, 128 ) ) } }, 4 )
   oBrw:SetColor( { CLR_FOCUSF }, { {|| IF( Month( oBrw:aDate[ oBrw:nAt ][ 5 ] ) == Month( Date() ),  RGB( 0, 0, 0 ),  RGB( 128, 128, 128 ) ) } }, 5 )
   oBrw:SetColor( { CLR_FOCUSF }, { {|| IF( Month( oBrw:aDate[ oBrw:nAt ][ 6 ] ) == Month( Date() ),  RGB( 0, 0, 0 ),  RGB( 128, 128, 128 ) ) } }, 6 )
   oBrw:SetColor( { CLR_FOCUSF }, { {|| IF( Month( oBrw:aDate[ oBrw:nAt ][ 7 ] ) == Month( Date() ),  RGB( 0, 0, 0 ),  RGB( 128, 128, 128 ) ) } }, 7 )

   oBrw:SetColor( { CLR_LINE }, { {|| RGB( 150, 150, 75 )  } },   )

   FOR i := 1 TO 7
      oBrw:aColumns[ i ]:cHeading := aDay[ i ]
      oBrw:aColumns[ i ]:hFontHead := hFont
      oBrw:SetColSize( i, 80 )
      oBrw:aColumns[ i ]:bData := hb_macroBlock( "GetDate(" + NTOC( i ) + ")" )
      oBrw:aColumns[ i ]:nAlign := DT_CENTER
      oBrw:aColumns[ i ]:lEdit := TRUE
      oBrw:aColumns[ i ]:bPrevEdit := {|| Mark(), .F. }
   NEXT

   CENTER WINDOW Win_1
   ACTIVATE WINDOW Win_1

RETURN NIL


FUNCTION GetDate( nCol )

   LOCAL oBrw := GetTBrowseObject( "oBrw", "Win_1" ) 

RETURN Day( oBrw:aDate[ oBrw:nAt ][ nCol ] )


STATIC FUNCTION Mark()

   LOCAL oBrw := GetTBrowseObject( "oBrw", "Win_1" ) 

   IF Month( oBrw:aDate[ oBrw:nAt ][ oBrw:nCell ] ) == Month( Date() )

      oBrw:aMark[ oBrw:nAt ][ oBrw:nCell ] := .NOT. oBrw:aMark[ oBrw:nAt ][ oBrw:nCell ]
      oBrw:DrawSelect()

   ENDIF

RETURN NIL


STATIC FUNCTION IsMark( n )

   LOCAL oBrw := GetTBrowseObject( "oBrw", "Win_1" ) 

RETURN oBrw:aMark[ oBrw:nAt ][ n ]


STATIC FUNCTION GetTBrowseObject( cTbrw, cForm )

   LOCAL oBrw, i

   DEFAULT cForm := _HMG_ThisFormName

   IF ( i := GetControlIndex( cTBrw, cForm ) ) > 0
      oBrw := _HMG_aControlIds [ i ]
   ENDIF

RETURN oBrw


STATIC FUNCTION FindSunday( dDate )

   LOCAL Sunday := CTOD( "" )

   IF Upper( CDoW( dDate ) ) == "SUNDAY"
      Sunday := dDate
   ELSE
      DO WHILE .t.
         ++dDate
         IF Upper( CDoW( dDate ) ) == "SUNDAY"
            Sunday := dDate
            EXIT
         ENDIF
      ENDDO
   ENDIF

RETURN Sunday
