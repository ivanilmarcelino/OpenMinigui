/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2018 Igor Nazarov
 *
*/

#include "demo.ch"

#require "hbxlsxml"

// ======================================================================
FUNCTION XmlSetDefault( oBrw )

   LOCAL nAlign
   LOCAL cType
   LOCAL n
   LOCAL oCol

   FOR n := 1 TO Len( oBrw:aColumns )

      oCol := oBrw:aColumns[ n ]

      __objAddData ( oCol, 'XML_ColWidth' )
      oCol:XML_ColWidth := oBrw:aColumns[ n ]:nWidth / 1.3

      __objAddData ( oCol, 'XML_ColFontName' )
      IF n == 1
         oCol:XML_ColFontName := GetFontParam( GetFontHandle( "Font_7" ) )[ 1 ]
      ELSE
         oCol:XML_ColFontName := GetFontParam( GetFontHandle( "Font_6" ) )[ 1 ]
      END

      __objAddData ( oCol, 'XML_ColFontSize' )
      oCol:XML_ColFontSize := 16

      __objAddData ( oCol, 'XML_ColFontBold' )
      oCol:XML_ColFontBold := .F.

      __objAddData ( oCol, 'XML_HdrFontName' )
      oCol:XML_HdrFontName := GetFontParam( oCol:hFontHead )[ 1 ]

      __objAddData ( oCol, 'XML_HdrFontSize' )
      oCol:XML_HdrFontSize := GetFontParam( oCol:hFontHead )[ 2 ]

      __objAddData ( oCol, 'XML_HdrFontBold' )
      oCol:XML_HdrFontBold := .T.

      __objAddData ( oCol, 'XML_FootFontName' )
      oCol:XML_HdrFontName := GetFontParam( oCol:hFontHead )[ 1 ]

      __objAddData ( oCol, 'XML_FootFontSize' )
      oCol:XML_HdrFontSize := GetFontParam( oCol:hFontHead )[ 2 ]

      __objAddData ( oCol, 'XML_FootFontBold' )
      oCol:XML_HdrFontBold := .T.

      __objAddData ( oCol, 'XML_AlignV' )
      oCol:XML_AlignV := 'Center'

      __objAddData ( oCol, 'XML_AlignH' )
      nAlign := oCol:nAlign
      SWITCH nAlign
      CASE DT_LEFT
         oCol:XML_AlignH := "Left"
         EXIT
      CASE DT_CENTER
         oCol:XML_AlignH := "Center"
         EXIT
      CASE DT_RIGHT
         oCol:XML_AlignH := "Right"
         EXIT
      END SWITCH

      __objAddData ( oCol, 'XML_Format' )
      cType := ValType( Eval( oCol:bData ) )
      SWITCH cType
      CASE 'D'
         oCol:XML_Format := "m/d/yyyy"
         EXIT
      CASE 'N'
         oCol:XML_Format := "#,##0.00"
         EXIT

      END SWITCH

   END

RETURN NIL

// ======================================================================
FUNCTION XmlResetDefault( oBrw )

   LOCAL n, oCol

   FOR n := 1 TO Len( oBrw:aColumns )
      oCol := oBrw:aColumns[ n ]
      __objDelData ( oCol, 'XML_ColWidth' )
      __objDelData ( oCol, 'XML_ColFontName' )
      __objDelData ( oCol, 'XML_ColFontSize' )
      __objDelData ( oCol, 'XML_ColFontBold' )

      __objDelData ( oCol, 'XML_SHdrFontName' )
      __objDelData ( oCol, 'XML_SHdrFontSize' )
      __objDelData ( oCol, 'XML_SHdrFontBold' )

      __objDelData ( oCol, 'XML_HdrFontName' )
      __objDelData ( oCol, 'XML_HdrFontSize' )
      __objDelData ( oCol, 'XML_HdrFontBold' )

      __objDelData ( oCol, 'XML_FootFontName' )
      __objDelData ( oCol, 'XML_FootFontSize' )
      __objDelData ( oCol, 'XML_FootFontBold' )

      __objDelData ( oCol, 'XML_AlignV' )
      __objDelData ( oCol, 'XML_AlignH' )
      __objDelData ( oCol, 'XML_Format' )
   END

RETURN NIL

// ======================================================================
FUNCTION Brw2Xml( oBrw, cFile, lActivate, hProgress, aTitle )

   LOCAL oXml, oSheet, oStyle
   LOCAL nLen, nLine
   LOCAL i, j
   LOCAL nRow
   LOCAL nCol := 0
   LOCAL nAlign
   LOCAL cAlign := ''
   LOCAL cType
   LOCAL uData
   LOCAL hFont
   LOCAL nSkip
   LOCAL nRec := iif( oBrw:lIsDbf, ( oBrw:cAlias )->( RecNo() ), 0 )
   LOCAL nOldRow := oBrw:nLogicPos()
   LOCAL nOldCol := oBrw:nCell
   LOCAL lError := .F.
   LOCAL aColors := { { 0, 0, "" } }
   LOCAL nColor

   STATIC nCount := 0
   nCount++

   IF lActivate == NIL
      lActivate := .T.
   END

   IF cFile == NIL
      cFile := "Book.xml"
   END

   hProgress := NIL // не используется

   // Проверяем наличие файла для экспорта и возмоджность записи в него
   WHILE lError
      lError := .F.
      IF File( cFile ) // Есть такой
         i := FOpen( cFile, 16 )
         IF i > 0 // Файл не занят
            FClose( i )
            lError := .F.
         ELSE // Файл занят
            lError := .T.
         END
         IF lError
            cFile := hb_FNameName( cFile ) + '(' + hb_ntoc( nCount++ ) + ')' + ".XML" // Переименовываем
         END
      END
   END

   // Создаем объект XML
   oXml := ExcelWriterXML():New( cFile )
   oXml:setOverwriteFile( .T. )
   // oXml:setCodePage( "RU1251" )

   // Определяем Лист
   oSheet := oXml:addSheet( "Sheet1" )

   // Определяем ширины колонок из бровса
   FOR i := 1 TO Len( oBrw:aColumns )
      oSheet:columnWidth( i, oBrw:aColumns[ i ]:XML_ColWidth )
   NEXT

   // Определяем стиль названия отчета
   oStyle := oXml:addStyle( "Title" )
   oStyle:alignHorizontal ( "Center" )
   oStyle:alignVertical ( "Center" )
   oStyle:SetfontName( GetFontParam( aTitle[ 2 ] )[ 1 ] )
   oStyle:SetfontSize( GetFontParam( aTitle[ 2 ] )[ 2 ] )
   IF GetFontParam( aTitle[ 2 ] )[ 3 ]
      oStyle:setFontBold()
   END

   // Определяем суперхидер
   FOR i := 1 TO Len( oBrw:aSuperHead )
      oStyle := oXml:addStyle( "SH" + hb_ntoc( i ) )
      nAlign := oBrw:aSuperHead[ i ][ 12 ]
      SWITCH nAlign
      CASE DT_LEFT
         cAlign := "Left"
         EXIT
      CASE DT_CENTER
         cAlign := "Center"
         EXIT
      CASE DT_RIGHT
         cAlign := "Right"
         EXIT
      END SWITCH

      oStyle:alignHorizontal( cAlign )

      nAlign := oBrw:aSuperHead[ i ][ 13 ]
      SWITCH nAlign
      CASE DT_LEFT
         cAlign := "Left"
         EXIT
      CASE DT_CENTER
         cAlign := "Center"
         EXIT
      CASE DT_RIGHT
         cAlign := "Right"
         EXIT
      END SWITCH

      oStyle:alignVertical( cAlign )
      oStyle:SetfontName( GetFontParam( oBrw:aSuperHead[ i ][ 7 ] )[ 1 ] )
      oStyle:SetfontSize( GetFontParam( oBrw:aSuperHead[ i ][ 7 ] )[ 2 ] )
      oStyle:bgColor( HMG_ClrToHTML( CLR_HGRAY ) ) // only Excel 2003
      IF GetFontParam( oBrw:aSuperHead[ i ][ 7 ] )[ 3 ]
         oStyle:setFontBold()
      END
      oStyle:Border( "All", 2, "Automatic", "Continuous" )
      oStyle:alignWraptext()
   END

   FOR i := 1 TO Len( oBrw:aColumns )

      // Определяем стили шапки колонок
      oStyle := oXml:addStyle( "H" + hb_ntoc( i ) )
      oStyle:Border( "All", 2, "Automatic", "Continuous" )
      oStyle:alignHorizontal( "Center" )
      oStyle:alignVertical( "Center" )
      oStyle:bgColor( HMG_ClrToHTML( CLR_HGRAY ) ) // only Excel 2003
      oStyle:alignWraptext()
      oStyle:SetfontName( oBrw:aColumns[ i ]:XML_HdrFontName )
      oStyle:SetfontSize( oBrw:aColumns[ i ]:XML_HdrFontSize )
      IF oBrw:aColumns[ i ]:XML_HdrFontBold
         oStyle:setFontBold()
      END

      // Определяем стили подвалов
      oStyle := oXml:addStyle( "F" + hb_ntoc( i ) )
      oStyle:Border( "All", 2, "Automatic", "Continuous" )
      oStyle:alignHorizontal( "Center" )
      oStyle:alignVertical( "Center" )
      oStyle:bgColor( HMG_ClrToHTML( CLR_HGRAY ) ) // only Excel 2003

      oStyle:alignWraptext()
      oStyle:SetfontName( oBrw:aColumns[ i ]:XML_FootFontName )
      oStyle:SetfontSize( oBrw:aColumns[ i ]:XML_FootFontSize )
      IF oBrw:aColumns[ i ]:XML_HdrFontBold
         oStyle:setFontBold()
      END

   END

   // Определяем используемые стили ячеек прогоном бровса
   Eval( oBrw:bGoTop )
   nLen := oBrw:nLen
   nLine := 1

   WHILE nLine <= nLen
      FOR nCol := 1 TO Len( oBrw:aColumns )
         nColor := oBrw:aColumns[ nCol ]:nClrBack
         IF HB_ISBLOCK( nColor )
            nColor := Eval( nColor, oBrw:nAt, nCol, oBrw )
         END
         // в aColors храним массивы ( строка, столбец. стиль )
         AAdd( aColors, { nLine, nCol, "S" + HMG_ClrToHTML( nColor ) } )

         nAlign := oBrw:aColumns[ nCol ]:nAlign
         SWITCH nAlign
         CASE DT_CENTER
            cAlign := "Center"
            EXIT
         CASE DT_LEFT
            cAlign := "Left"
            EXIT
         CASE DT_RIGHT
            cAlign := "Right"
            EXIT
         END SWITCH

         oStyle := oXml:addStyle( "S" + HMG_ClrToHTML( nColor ) )
         oStyle:Border( "All", 1, "Automatic", "Continuous" )
         oStyle:alignHorizontal( cAlign )
         oStyle:alignVertical( "Center" )
         // шрифт
         hFont := oBrw:aColumns[ nCol ]:hFont
         IF HB_ISBLOCK( hFont )
            hFont := Eval( hFont, oBrw:nAt, nCol, oBrw )
         END
         oStyle:SetfontName( GetFontParam( hFont )[ 1 ] )
         oStyle:SetfontSize( GetFontParam( hFont )[ 2 ] )
         IF GetFontParam( hFont )[ 3 ]
            oStyle:setFontBold()
         END
         oStyle:bgColor( HMG_ClrToHTML( nColor ) )
         oStyle:alignWraptext()

      END
      nLine++
      nSkip := oBrw:Skip( 1 )
      SysRefresh()
      IF nSkip == 0
         EXIT
      ENDIF
   END

   // Пишем название отчета
   nRow := 1
   uData := { aTitle[ 1 ], aTitle[ 1 ] }
   FOR i := 1 TO Len( uData )
      oSheet:writeString( nRow + i, 1, uData[ i ], "Title" )
      oSheet:cellMerge ( nRow + i, 1, Len( oBrw:aColumns ) - 1, 0 )
      nRow++
   NEXT

   nRow++
   // Пишем Суперхидер
   FOR i := 1 TO Len( oBrw:aSuperHead )
      uData := AtRepl( Chr( 13 ) + Chr( 10 ), oBrw:aSuperHead[ i ][ 3 ], "&#10;" )
      oSheet:writeString( nRow, oBrw:aSuperHead[ i ][ 1 ], uData, "SH" + hb_ntoc( i ) )
      oSheet:cellHeight( nRow, 1, oBrw:nHeightSuper )
      oSheet:cellMerge( nRow, oBrw:aSuperHead[ i ][ 1 ], oBrw:aSuperHead[ i ][ 2 ] - oBrw:aSuperHead[ i ][ 1 ], 0 )
   END
   // Пишем шапку бровса
   nRow++
   FOR i := 1 TO Len( oBrw:aColumns )
      uData := AtRepl( Chr( 13 ) + Chr( 10 ), oBrw:aColumns[ i ]:cHeading, "&#10;" )
      oSheet:writeString( nRow, i, uData, "H" + hb_ntoc( i ) )
   END

   oSheet:cellHeight( nRow, 1, oBrw:nHeightHead )

   // Пишем таблицу
   Eval( oBrw:bGoTop )

   nRow++
   nLen := oBrw:nLen
   nLine := 1

   WHILE nLine <= nLen
      oSheet:cellHeight( nRow, 1, oBrw:nHeightCell / 1.3 )

      FOR nCol := 1 TO Len( oBrw:aColumns )
         uData := Eval( oBrw:aColumns[ nCol ]:bData )
         cType := ValType( uData )
         j := AScan( aColors, {| e | e[ 1 ] == nLine .AND. e[ 2 ] == nCol } )

         SWITCH cType
         CASE "N"
            oSheet:writeNumber( nRow, nCol, uData, aColors[ j ][ 3 ] )
            EXIT
         CASE "C"
            uData := AtRepl( Chr( 13 ) + Chr( 10 ), uData, "&#10;" )
            oSheet:writeString( nRow, nCol, uData, aColors[ j ][ 3 ] )
            EXIT
         CASE "D"
            oSheet:writeDateTime( nRow, nCol, DToC( uData ), aColors[ j ][ 3 ] )
            EXIT
         CASE "L"
            oSheet:writeString( nRow, nCol, iif( uData, '.T.', '.F.' ), aColors[ j ][ 3 ] )
            EXIT
         CASE "U"
            oSheet:writeString( nRow, nCol, '', aColors[ j ][ 3 ] )
            EXIT
         CASE "T"
            oSheet:writeString( nRow, nCol, hb_TToC( uData ), aColors[ j ][ 3 ] )
            EXIT
         END SWITCH

      NEXT

      oBrw:Skip( 1 )
      nLine++
      nRow++
      SysRefresh()
   END

   // Пишем подвал бровса
   FOR i := 1 TO Len( oBrw:aColumns )
      uData := AtRepl( Chr( 13 ) + Chr( 10 ), oBrw:aColumns[ i ]:cFooting, "&#10;" )
      oSheet:writeString( nRow, i, uData, "H" + hb_ntoc( i ) )
   END

   oSheet:cellHeight( nRow, 1, oBrw:nHeightFoot )

   IF oBrw:lIsDbf
      ( oBrw:cAlias )->( dbGoto( nRec ) )
   ELSE
      oBrw:GoPos( nOldRow, nOldCol )
   END

   oXml:writeData( cFile )

   IF lActivate
      hb_MemoWrit( '_e_.cmd', '@Start Excel .\' + cFile + CRLF )
      RUN '_e_.cmd'
      InkeyGui( 1000 )
      FErase( '_e_.cmd' )
      // ShellExecute( 0, "Open", cFile,,, 3 )
   ENDIF

   oBrw:Display()

RETURN NIL
