/*
 * MINIGUI - Harbour Win32 GUI library Demo
 * Copyright 2021 Verchenko Andrey <verchenkoag@gmail.com>
 *
 * ѕример меню справочника дл€ таблицы
 * Example of a reference menu for a table
*/

#include "minigui.ch"


//////////////////////////////////////////////////////////////////////////////
FUNCTION myListConstantsImages()
   LOCAL a2Dim := {}
// ------- список констант дл€ показа картинок в “—Ѕ
// смотреть в C:\borland\BCC55\Include\wingdi.h
//#define SRCCOPY             (DWORD)0x00CC0020 /* dest = source                   */
//#define SRCPAINT            (DWORD)0x00EE0086 /* dest = source OR dest           */
//#define SRCAND              (DWORD)0x008800C6 /* dest = source AND dest          */
//#define SRCINVERT           (DWORD)0x00660046 /* dest = source XOR dest          */
//#define SRCERASE            (DWORD)0x00440328 /* dest = source AND (NOT dest )   */
//#define NOTSRCCOPY          (DWORD)0x00330008 /* dest = (NOT source)             */
//#define NOTSRCERASE         (DWORD)0x001100A6 /* dest = (NOT src) AND (NOT dest) */
//#define MERGECOPY           (DWORD)0x00C000CA /* dest = (source AND pattern)     */
//#define MERGEPAINT          (DWORD)0x00BB0226 /* dest = (NOT source) OR dest     */
//#define PATCOPY             (DWORD)0x00F00021 /* dest = pattern                  */
//#define PATPAINT            (DWORD)0x00FB0A09 /* dest = DPSnoo                   */
//#define PATINVERT           (DWORD)0x005A0049 /* dest = pattern XOR dest         */
//#define DSTINVERT           (DWORD)0x00550009 /* dest = (NOT dest)               */
//#define BLACKNESS           (DWORD)0x00000042 /* dest = BLACK                    */
//#define WHITENESS           (DWORD)0x00FF0062 /* dest = WHITE                    */
  AADD( a2Dim, { "SRCCOPY    " , 0x00CC0020 } )
  AADD( a2Dim, { "SRCPAINT   " , 0x00EE0086 } )
  AADD( a2Dim, { "SRCAND     " , 0x008800C6 } )
  AADD( a2Dim, { "SRCINVERT  " , 0x00660046 } )
  AADD( a2Dim, { "SRCERASE   " , 0x00440328 } )
  AADD( a2Dim, { "NOTSRCCOPY " , 0x00330008 } )
  AADD( a2Dim, { "NOTSRCERASE" , 0x001100A6 } )
  AADD( a2Dim, { "MERGECOPY  " , 0x00C000CA } )
  AADD( a2Dim, { "MERGEPAINT " , 0x00BB0226 } )
  AADD( a2Dim, { "PATCOPY    " , 0x00F00021 } )
  AADD( a2Dim, { "PATPAINT   " , 0x00FB0A09 } )
  AADD( a2Dim, { "PATINVERT  " , 0x005A0049 } )
  AADD( a2Dim, { "DSTINVERT  " , 0x00550009 } )
  AADD( a2Dim, { "BLACKNESS  " , 0x00000042 } )
  AADD( a2Dim, { "WHITENESS  " , 0x00FF0062 } )

RETURN a2Dim

//////////////////////////////////////////////////////////////////////////////
FUNCTION myMenuListCountry(oBrw)
   LOCAL o, nTable, cAls, nRow, cRet, nCell, cTyp, oCol, cCol, nRec, nOrd
   LOCAL lFind, cVal, cForm, nI, aDim, oCell, nY, nX, nW, nH

   aDim   := {}
   o      := oBrw:Cargo       // получить данные из объекта
   nTable := o:nTable         // номер таблицы
   cAls   := oBrw:cAlias
   nRow   := oBrw:nAt         // номер строки в таблице
   nCell  := oBrw:nCell       // номер €чейки/колонки в таблице
   oCol   := oBrw:aColumns[ nCell ]
   cTyp   := oCol:cFieldTyp
   cCol   := oCol:cName
   cForm  := oBrw:cParentWnd
   oCell  := oBrw:GetCellInfo(oBrw:nRowPos)
   nY     := oCell:nRow + oBrw:nHeightHead + 4
   nX     := oCell:nCol
   nW     := oCell:nWidth
   nH     := oCell:nHeight

   SELECT(cAls)
   nRec := RecNo()
   nOrd := IndexOrd()
   OrdSetFocus(0)
   dbGotop()
   AADD( aDim, FIELDGET( FIELDNUM(cCol) ) )
   DO WHILE !EOF()
      cVal  := FIELDGET( FIELDNUM(cCol) )
      IF LEN(ALLTRIM(cVal)) > 0
         lFind := .F.
         FOR nI := 1 TO LEN(aDim)
            IF cVal == aDim[nI]
               lFind := .T.
               EXIT
            ENDIF
         NEXT
         IF !lFind
            AADD( aDim, cVal )
         ENDIF
      ENDIF
      SKIP
      DO EVENTS
   ENDDO
   OrdSetFocus(nOrd)
   dbGoto(nRec)

   aDim := ASORT(aDim)
   cRet := my2ContexMenu( aDim, cForm, { nY, nX } )

RETURN cRet

//////////////////////////////////////////////////////////////////////////
FUNCTION my2ContexMenu( aDim, cForm, aYX )
   LOCAL aList, nChoice, xRet, nBmpSize, nFSize, aFntExt, nI
   LOCAL nPos := 3, lExit := .F.
   DEFAULT aYX := {}

   // ------ nPos = -----------------------------
   // 1 - Extend Dynamic Context Menu at Cursor
   // 2 - Extend Dynamic Context Menu at Position
   // 3 - Extend Dynamic Context Menu at Row Col

   aList := {}
   FOR nI := 1 TO LEN(aDim)                      // .T.-нет выбора  .F.-есть выбор
       AADD( aList, { "Dbg32", " " + ALLTRIM(aDim[nI]) , .F., "MsgDebug", "Str" , nI } )
   NEXT

   //SetThemes(2)  // тема "Office 2000 theme" в ContextMenu
   SetThemes(3)    // тема "Dark theme" в ContextMenu

   nBmpSize := 20
   nFSize   := ModeSizeFont() + 6
   aFntExt  := { "DejaVu Sans Mono", "Comic Sans MS" }
   // aYX - координаты по €чейке таблицы
   nChoice  := DynamicContextMenuExtend(cForm,aList,nPos,nBmpSize,nFSize,lExit,aFntExt,aYX)
   xRet     := ""

   IF nChoice > 0
      xRet := ALLTRIM( aList[nChoice,2] )
   ENDIF

RETURN xRet
