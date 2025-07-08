/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Разное для Tsbrowse / Miscellaneous for Tsbrowse
*/

#include "minigui.ch"
///////////////////////////////////////////////////////////////////////////////
FUNCTION myTsbListColumn( oBrw )
   LOCAL oCol, nCol, cCol, cSize, cFld, cMsg, cTitle, aStruct

   aStruct := oBrw:Cargo:aStruct  // {cFld, cTyp, nLen, nDec} - структура базы для экспорта
   IF App.Cargo:cLang == "RU"
      cTitle := 'Инфо по списку колонок'
   ELSE
      cTitle := 'Info on the list of columns'
   ENDIF
   cMsg   := "Table alias: " + oBrw:cAlias + ";;"
   FOR nCol := 1 TO Len(oBrw:aColumns)
      oCol  := oBrw:aColumns[ nCol ]
      cCol  := oCol:cName
      cFld  := oCol:cField
      cSize := HB_NtoS( INT(oBrw:GetColSizes()[nCol]) )
      cMsg  += HB_NtoS(nCol) + ") " + cCol + " = " + cSize
      cMsg  += ' ( "'+ cFld + '", "'  + oCol:cFieldTyp + '" '
      //cMsg  += HB_NtoS(oCol:nFieldLen)                  // нет такой переменной в RECORDSET
      //cMsg  += ',' + HB_NtoS(oCol:nFieldDec) + ' ) ;'   // нет такой переменной в RECORDSET
      IF nCol > 1
         cMsg  += HB_NtoS(aStruct[nCol-1,3]) + ','
         cMsg  += HB_NtoS(aStruct[nCol-1,4]) + ' ) ;'
      ELSE
         cMsg  += ' ) ;'
      ENDIF
   NEXT
   cMsg += ";"
   FOR nCol := 1 TO Len(oBrw:aColumns)
      oCol  := oBrw:aColumns[ nCol ]
      cCol  := oCol:cName
      cFld  := cValToChar( oCol:cPicture )
      cMsg  += HB_NtoS(nCol) + ") " + cCol + " = "
      cMsg  += ' "'+ cFld + '"  ;'
   NEXT
   cMsg += REPL("; ",20)

   AlertInfo(cMsg , cTitle, , , {RED})

RETURN Nil

///////////////////////////////////////////////////////////////////////////////
FUNCTION myTsbListFont( oBrw )
   LOCAL cMsg, cTitle, cMsg1, cMsg2, cFnt, hFnt, aFnt, aFonts, n

   IF App.Cargo:cLang == "RU"
      cTitle := 'Инфо по фонтам'
      cMsg1  := 'Фонты в таблице'
      cMsg2  := 'Все фонты в программе'
   ELSE
      cTitle := 'Info on fonts'
      cMsg1  := 'Fonts in table'
      cMsg2  := 'All fonts in the program'
   ENDIF
   cMsg   := ";" + cMsg1 + ";;"
   cMsg   += "     1-Cell: "+hb_valtoexp(GetFontParam(oBrw:hFont)) + ";"
   cMsg   += "     2-Head: "+hb_valtoexp(GetFontParam(oBrw:hFontHead )) + ";"
   cMsg   += "     3-Foot: "+hb_valtoexp(GetFontParam(oBrw:hFontFoot )) + ";"
   cMsg   += "    4-SpcHd: "+hb_valtoexp(GetFontParam(oBrw:hFontSpcHd)) + ";"
   cMsg   += "  5-SuperHd: "+hb_valtoexp(GetFontParam(oBrw:hFontSupHdGet(1))) + ";;"
   cMsg   += "     6-Edit: "+hb_valtoexp(GetFontParam(oBrw:hFontEdit )) + ";"

   cMsg   += ";" + cMsg2 + ";;"
   aFonts := {}
   FOR n := 1 TO Len( _HMG_aControlNames )
      IF _HMG_aControlType[ n ] == "FONT"
         AAdd( aFonts, { _HMG_aControlNames[ n ], _HMG_aControlHandles[ n ] } )
      ENDIF
   NEXT

   FOR EACH aFnt IN aFonts
       cFnt := aFnt[1]
       hFnt := aFnt[2]
       cMsg += "  " + hb_NtoS(hb_enumindex(aFnt)) + ". " + ;       /*+ cFnt + " : "*/
               hb_valtoexp( GetFontParam( hFnt ) ) + CRLF
   NEXT
   cMsg += REPL("; ",10)

   AlertInfo(cMsg , cTitle, , , {RED})

RETURN Nil

