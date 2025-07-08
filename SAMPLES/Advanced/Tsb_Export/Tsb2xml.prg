/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2018 Igor Nazarov
 *
*/
#include "hmg.ch"
#include "tsbrowse.ch"

#define PBM_SETPOS       1026

#require "hbxlsxml"

* ======================================================================
FUNCTION XmlSetDefault( oBrw )
   local nAlign := 0
   local cType  := ''
   local n := 0
   local oCol

   for n := 1 TO Len(oBrw:aColumns)

       oCol := oBrw:aColumns[n]

       __objAddData  (oCol, 'XML_ColWidth'     )         
       oCol:XML_ColWidth   := oBrw:aColumns[n]:nWidth / 1.3

       __objAddData  (oCol, 'XML_ColFontName'  )
       if n == 1
        oCol:XML_ColFontName := GetFontParam(GetFontHandle( "Font_7" ))[1]
     else
        oCol:XML_ColFontName := GetFontParam(GetFontHandle( "Font_6" ))[1]
     end

       __objAddData  (oCol, 'XML_ColFontSize'  )
       oCol:XML_ColFontSize := 16

       __objAddData  (oCol, 'XML_ColFontBold'  )
       oCol:XML_ColFontBold := .F.

       __objAddData  (oCol, 'XML_HdrFontName'  )
       oCol:XML_HdrFontName := GetFontParam( oCol:hFontHead )[1]

       __objAddData  (oCol, 'XML_HdrFontSize'  )
       oCol:XML_HdrFontSize := GetFontParam( oCol:hFontHead )[2]

       __objAddData  (oCol, 'XML_HdrFontBold'  )
       oCol:XML_HdrFontBold := .T.

       __objAddData  (oCol, 'XML_FootFontName'  )
       oCol:XML_FootFontName := GetFontParam( oCol:hFontHead )[1]

       __objAddData  (oCol, 'XML_FootFontSize'  )
       oCol:XML_FootFontSize := GetFontParam( oCol:hFontHead )[2]

       __objAddData  (oCol, 'XML_FootFontBold'  )
       oCol:XML_FootFontBold := .T.

       __objAddData  (oCol, 'XML_AlignV'       )
       oCol:XML_AlignV := 'Center'

       __objAddData  (oCol, 'XML_AlignH'       )
       nAlign := oCol:nAlign
       switch nAlign
          case DT_LEFT
            oCol:XML_AlignH := "Left"
            Exit
         case DT_CENTER
            oCol:XML_AlignH := "Center"
            Exit
         Case DT_RIGHT
            oCol:XML_AlignH := "Right"
            Exit
      End switch

       __objAddData  (oCol, 'XML_Format'       )
       cType := Valtype(Eval(oCol:bData))
       switch cType
          case 'D'
            oCol:XML_Format := "m/d/yyyy"
            Exit
          case 'N'
            oCol:XML_Format := "#,##0.00"
            Exit

      End switch

   end

   Return nil

* ======================================================================
FUNCTION XmlResetDefault( oBrw )
   local n , oCol

   for n := 1 TO Len(oBrw:aColumns)
       oCol := oBrw:aColumns[n]
       __objDelData  (oCol, 'XML_ColWidth'     )
       __objDelData  (oCol, 'XML_ColFontName'  )
       __objDelData  (oCol, 'XML_ColFontSize'  )
       __objDelData  (oCol, 'XML_ColFontBold'  )

       __objDelData  (oCol, 'XML_SHdrFontName'  )
       __objDelData  (oCol, 'XML_SHdrFontSize'  )
       __objDelData  (oCol, 'XML_SHdrFontBold'  )

       __objDelData  (oCol, 'XML_HdrFontName'  )
       __objDelData  (oCol, 'XML_HdrFontSize'  )
       __objDelData  (oCol, 'XML_HdrFontBold'  )

       __objDelData  (oCol, 'XML_FootFontName'  )
       __objDelData  (oCol, 'XML_FootFontSize'  )
       __objDelData  (oCol, 'XML_FootFontBold'  )

       __objDelData  (oCol, 'XML_AlignV'       )
       __objDelData  (oCol, 'XML_AlignH'       )
       __objDelData  (oCol, 'XML_Format'       )
   end

   Return nil

* ======================================================================
FUNCTION Brw2Xml( oBrw, cFile, lActivate, hProgress, aTitle, aColSel ) 
   LOCAL oXml, oSheet, oStyle, uData
   LOCAL nLen, nLine, cTitle, i
   LOCAL nRow  :=  0
   LOCAL nCol  :=  0
   LOCAL cStr  := ""
   LOCAL nAlign := 0
   LOCAL nSkip  := 0
   LOCAL cAlign := ''
   LOCAL cType  := ''
   LOCAL aFont  := {} , aFontTemp := {}
   LOCAL nRec := iif( oBrw:lIsDbf, ( oBrw:cAlias )->( RecNo() ), 0 )
   LOCAL nOldRow := oBrw:nLogicPos()
   LOCAL nOldCol := oBrw:nCell
   LOCAL lError   := .F.
   LOCAL nColHead 
   LOCAL lTsbSuperHd, lTsbHeading, lTsbFooting
   LOCAL nColDbf , aCol

   DEFAULT cFile := "Book.xml", lActivate := .T., hProgress := nil
   DEFAULT aTitle := {"", nil} , aColSel := nil

   // проверка суперхидер таблицы
   lTsbSuperHd := oBrw:lDrawSuperHd
   IF lTsbSuperHd
      lTsbSuperHd := ( AScan( oBrw:aSuperHead, {|a| !Empty(a[3]) } ) > 0 )
      // если суперхидер таблицы задан пустым, то нет вывода суперхидера таблицы
      // пустой суперхидер задаётся в demo2.prg строка 232 - :aSuperhead[ 1, 3 ] := '' 
   ENDIF

   // проверка шапки таблицы
   lTsbHeading := oBrw:lDrawHeaders
   If lTsbHeading    
      lTsbHeading := ( AScan( oBrw:aColumns, { |o| !Empty( o:cHeading ) } ) > 0 )
      // если шапка таблицы задана пустые колонки, то нет вывода шапки таблицы
      // пустая шапка задаётся в demo2.prg строка 266 - oCol:cHeading := '' или NIL
   Endif

   // проверка подвала таблицы
   lTsbFooting := oBrw:lDrawFooters
   If lTsbFooting    
      lTsbFooting := ( AScan( oBrw:aColumns, { |o| !Empty( o:cFooting ) } ) > 0 )
      // если подвал таблицы задан пустые колонки, то нет вывода подвала таблицы
      // пустой подвал задаётся в demo2.prg строка 269 - oCol:cFooting := '' или NIL
   Endif

   // Проверяем наличие файла для экспорта и возмоджость записи в него
   While lError
    lError := .F.
    if File( cFile )                        // Есть такой
        i := Fopen( cFile , 16 )
        if i >  0     // Файл не занят
          Fclose(i)
          lError := .F.
        else                                   // Файл занят
          lError := .T.
        end
        if lError
           cFile := GetFileNameMaskNum(cFile)  // получить новое имя файла
        end
     end
   End

   // Создаем объект XML
   oXml := ExcelWriterXML():New( cFile )
   oXml:setOverwriteFile( .T. )
   oXml:setCodePage( "RU1251" )

   // Определяем Лист
   oSheet := oXml:addSheet( "Sheet1" )

   //Определяем колонки  
   if aColSel= Nil .or. Len(aColSel) = 0
     aColSel := CalcAcolselForTbl( oBrw,aColSel)
   Endif

   nColDbf :=Len(aColSel)

   // Определяем ширины колонок из бровса
   nColHead := 0
   FOR EACH i IN aColSel
      oSheet:columnWidth(  ++nColHead,  oBrw:aColumns[i]:XML_ColWidth )
   Next

   // Определяем стиль названия отчета
   oStyle := oXml:addStyle( "Title" )
   oStyle:alignHorizontal( "Left" )
   oStyle:alignVertical( "Center" )
   oStyle:SetfontName( 'Arial' )
   oStyle:SetfontSize( 17 )
   oStyle:setFontBold()

   IF lTsbSuperHd
   // Определяем суперхидер
    For i := 1 To len( oBrw:aSuperHead )
     oStyle := oXml:addStyle( "SH" + hb_ntoc(i) )
          nAlign := oBrw:aSuperHead[i][12]
          switch nAlign
             case DT_LEFT
               cAlign := "Left"
               Exit
            case DT_CENTER
               cAlign := "Center"
               Exit
            Case DT_RIGHT
               cAlign := "Right"
               Exit
         End switch

     oStyle:alignHorizontal( cAlign )

          nAlign := oBrw:aSuperHead[i][13]
          switch nAlign
             case DT_LEFT
               cAlign := "Left"
               Exit
            case DT_CENTER
               cAlign := "Center"
               Exit
            Case DT_RIGHT
               cAlign := "Right"
               Exit
         End switch

     oStyle:alignVertical( cAlign )  
     oStyle:bgColor( HMG_ClrToHTML(oBrw:nClrHeadBack) ) 
     oStyle:SetfontName( GetFontParam(oBrw:aSuperHead[i][7])[1] )
     oStyle:SetfontSize( GetFontParam(oBrw:aSuperHead[i][7])[2] )
     if  GetFontParam(oBrw:aSuperHead[i][7])[3]
         oStyle:setFontBold()
     end
     oStyle:Border( "All", 2, "Automatic",  "Continuous" )
     oStyle:alignWraptext()
   end
  Endif


   //Определяем стили шапки колонок
    nColHead := 0
    FOR EACH i IN aColSel
       nColHead ++
       If lTsbHeading    
       //Определяем стили шапки таблицы
          oStyle := oXml:addStyle( "H" + hb_ntoc(nColHead) )
          oStyle:Border( "All", 2, "Automatic",  "Continuous" )
          oStyle:alignHorizontal( "Center" )
          oStyle:alignVertical( "Center" )
          oStyle:bgColor( HMG_ClrToHTML(oBrw:nClrHeadBack) ) 
          oStyle:alignWraptext()
          oStyle:SetfontName( oBrw:aColumns[i]:XML_HdrFontName  )
          oStyle:SetfontSize( oBrw:aColumns[i]:XML_HdrFontSize )
          if oBrw:aColumns[i]:XML_HdrFontBold
           oStyle:setFontBold()
          end
          oStyle:alignWraptext()
       Endif
       //Определяем стили колонок
       oStyle := oXml:addStyle( "S" + hb_ntoc(nColHead) )
       oStyle:Border( "All", 1, "Automatic",  "Continuous" )
       oStyle:alignHorizontal( oBrw:aColumns[i]:XML_AlignH  )
       oStyle:alignVertical( oBrw:aColumns[i]:XML_AlignV  )
       oStyle:SetfontName( oBrw:aColumns[i]:XML_ColFontName )
       oStyle:SetfontSize( oBrw:aColumns[i]:XML_ColFontSize )
       if oBrw:aColumns[i]:XML_ColFontBold
     oStyle:setFontBold()
       end
       oStyle:alignWraptext()

       if oBrw:aColumns[i]:XML_Format <> NIL
         oStyle:setNumberFormat( oBrw:aColumns[i]:XML_Format )
       end

       If lTsbFooting    
       //Определяем стили колонок подвала таблицы
          oStyle := oXml:addStyle( "F" + hb_ntoc(nColHead) )
          oStyle:Border( "All", 2, "Automatic",  "Continuous" )
          oStyle:alignHorizontal( "Center" )
          oStyle:alignVertical( "Center" )
          oStyle:bgColor( HMG_ClrToHTML(oBrw:nClrFootBack) ) 
          oStyle:alignWraptext()
          oStyle:SetfontName( oBrw:aColumns[i]:XML_FootFontName  )
          oStyle:SetfontSize( oBrw:aColumns[i]:XML_FootFontSize )
          if oBrw:aColumns[i]:XML_FootFontBold
            oStyle:setFontBold()
          end
          oStyle:alignWraptext()
       Endif
   end

   nRow := 1
   // Пишем название отчета
   cTitle:= aTitle[1]
   cTitle := AllTrim( cTitle )
   If !Empty(cTitle) 
      oSheet:writeString( nRow, 1, cTitle, "Title" )
      nRow++
   Endif

   nColHead := 0
   IF lTsbSuperHd
   // Пишем суперхидер
      nRow ++
      FOR EACH aCol IN oBrw:aSuperHead
         nColHead ++
         uData := If( ValType( aCol[3] ) == "B", Eval( aCol[3] ), aCol[3] )
         oSheet:writeString( nRow,  MaxNumFromArr(aColSel,aCol[1]), uData , "SH" + hb_ntoc(nColHead))
         oSheet:cellMerge(    nRow, MaxNumFromArr(aColSel,aCol[1]), MinNumFromArr(aColSel,aCol[2]) - MaxNumFromArr(aColSel,aCol[1]), 0 )
      NEXT
   Endif
  
   If lTsbHeading    
   // Пишем шапку бровса
     nRow ++
     nColHead := 0
     FOR EACH i IN aColSel
       uData := AtRepl( Chr(13)+Chr(10), oBrw:aColumns[i]:cHeading, "&#10;" )
       nColHead ++
       oSheet:writeString( nRow,  nColHead, uData , "H" + hb_ntoc(nColHead) )
     End
   Endif

   oSheet:cellHeight( nRow, 1, oBrw:nHeightHead )

  // Пишем таблицу
   Eval( oBrw:bGoTop )

   nRow ++
   nLen   := oBrw:nLen
   nLine  := 1

   While  nLine <= nLen
      oSheet:cellHeight( nRow, 1, oBrw:nHeightCell / 1.3 )

      nColHead := 0
      FOR EACH nCol IN aColSel

         nColHead ++
         uData  := Eval( oBrw:aColumns[ nCol ]:bData )
         cType := ValType( uData )

         switch cType
            Case "N"
               oSheet:writeNumber( nRow, nColHead, uData, 'S' + hb_ntoc(nColHead) )
               Exit
            Case "C"
          uData := AtRepl( Chr(13)+Chr(10), uData, "&#10;" )
               oSheet:writeString( nRow, nColHead, uData, 'S' + hb_ntoc(nColHead))
               Exit
            Case "D"
                oSheet:writeDateTime( nRow, nColHead, Dtoc(uData), 'S' + hb_ntoc(nColHead) )
                Exit
            Case "L"
                oSheet:writeString( nRow, nColHead, IIF(uData, '.T.' , '.F.'), 'S' + hb_ntoc(nColHead) )
                Exit
            Case "U"
               oSheet:writeString( nRow, nColHead, '', 'S' + hb_ntoc(nColHead))
               Exit
            Case "T"
               oSheet:writeString( nRow, nColHead, HB_TToC( uData), 'S' + hb_ntoc(nColHead))
               Exit

         End Switch

      Next

      oBrw:Skip(1)
      nLine++
      nRow++
      SysRefresh()
   End

   // Пишем подвал бровса
   If lTsbFooting    
     nColHead := 0

     FOR EACH i IN aColSel
       uData := AtRepl( Chr(13)+Chr(10), oBrw:aColumns[i]:cFooting, "&#10;" )
       nColHead ++
       oSheet:writeString( nRow,  nColHead , uData , "H" + hb_ntoc(nColHead) )
     End
   Endif
   oSheet:cellHeight( nRow, 1, oBrw:nHeightFoot )

   if oBrw:lIsDbf
      ( oBrw:cAlias )->( DbGoTo( nRec ) )
   else
      oBrw:GoPos(nOldRow, nOldCol)
   end

   oXml:writeData( cFile )

   If lActivate
      ShellExecute( 0, "Open", cFile,,, 3 )
   EndIf

   RETURN NIL

* ======================================================================
FUNCTION Brw2XmlColor( oBrw, cFile, lActivate, hProgress, aTitle, aColSel ) 
   LOCAL oXml, oSheet, oStyle, uData
   LOCAL nLen, nLine, i, j, hFont
   LOCAL nRow :=  0, nCol := 0, cStr := "", nAlign := 0
   LOCAL cAlign  := '', cType  := '', nSkip  := 0, lError   := .F.
   LOCAL nRec := iif( oBrw:lIsDbf, ( oBrw:cAlias )->( RecNo() ), 0 )
   LOCAL nOldRow := oBrw:nLogicPos()
   LOCAL nOldCol := oBrw:nCell
   LOCAL aColors := {{0,0,""}}
   LOCAL nTotal, nEvery, nColor := 0
   LOCAL nColHead 
   LOCAL lTsbSuperHd, lTsbHeading, lTsbFooting
   LOCAL nColDbf , aCol

   DEFAULT cFile := "Book.xml", lActivate := .T., hProgress := nil
   DEFAULT aTitle := {"", nil} , aColSel := nil

   // проверка суперхидер таблицы
   lTsbSuperHd := oBrw:lDrawSuperHd
   IF lTsbSuperHd
      lTsbSuperHd := ( AScan( oBrw:aSuperHead, {|a| !Empty(a[3]) } ) > 0 )
   ENDIF

   // проверка шапки таблицы
   lTsbHeading := oBrw:lDrawHeaders
   If lTsbHeading    
      lTsbHeading := ( AScan( oBrw:aColumns, { |o| !Empty( o:cHeading ) } ) > 0 )
   Endif

   // проверка подвала таблицы
   lTsbFooting := oBrw:lDrawFooters
   If lTsbFooting    
      lTsbFooting := ( AScan( oBrw:aColumns, { |o| !Empty( o:cFooting ) } ) > 0 )
   Endif

   If hProgress != Nil
      nTotal := oBrw:nLen 
      SetProgressBarRange ( hProgress , 1 , nTotal )
      SendMessage(hProgress, PBM_SETPOS, 0, 0)
      nEvery := Max( 1, Int( nTotal * .02 ) ) // refresh hProgress every 2 %
   EndIf

   // Проверяем наличие файла для экспорта и возможность записи в него
   While lError
      lError := .F.
      if File( cFile )                        // Есть такой
         i := Fopen( cFile , 16 )
         if i >  0     // Файл не занят
           Fclose(i)
           lError := .F.
         else                                   // Файл занят
           lError := .T.
         end
         if lError
            cFile := GetFileNameMaskNum(cFile)  // получить новое имя файла 
         end
     end
   End

   // Создаем объект XML
   oXml := ExcelWriterXML():New( cFile )
   oXml:setOverwriteFile( .T. )
   oXml:setCodePage( "RU1251" )

   // Определяем Лист
   oSheet := oXml:addSheet( "Sheet1" )

   //Определяем колонки  
   if aColSel= Nil .or. Len(aColSel) = 0
     aColSel := CalcAcolselForTbl( oBrw,aColSel)
   Endif

   nColDbf :=Len(aColSel)

   // Определяем ширины колонок из бровса
   nColHead := 0
   FOR EACH i IN aColSel
      oSheet:columnWidth(  ++nColHead,  oBrw:aColumns[i]:XML_ColWidth )
   Next

   // Определяем стиль названия отчета
   oStyle := oXml:addStyle( "Title" )
   oStyle:alignHorizontal( "Left" )
   oStyle:alignVertical( "Center" )
   oStyle:SetfontName(   GetFontParam(aTitle[2])[1]  )
   oStyle:SetfontSize(   GetFontParam(aTitle[2])[2] )
   if  GetFontParam(aTitle[2])[3]
       oStyle:setFontBold()
   end

   IF lTsbSuperHd
   // Определяем суперхидер
    For i := 1 To len( oBrw:aSuperHead )
     oStyle := oXml:addStyle( "SH" + hb_ntoc(i) )
          nAlign := oBrw:aSuperHead[i][12]
          switch nAlign
             case DT_LEFT
               cAlign := "Left"
               Exit
            case DT_CENTER
               cAlign := "Center"
               Exit
            Case DT_RIGHT
               cAlign := "Right"
               Exit
         End switch

     oStyle:alignHorizontal( cAlign )

          nAlign := oBrw:aSuperHead[i][13]
          switch nAlign
             case DT_LEFT
               cAlign := "Left"
               Exit
            case DT_CENTER
               cAlign := "Center"
               Exit
            Case DT_RIGHT
               cAlign := "Right"
               Exit
         End switch

     oStyle:alignVertical( cAlign )
     oStyle:SetfontName( GetFontParam(oBrw:aSuperHead[i][7])[1] )
     oStyle:SetfontSize( GetFontParam(oBrw:aSuperHead[i][7])[2] )
     oStyle:bgColor( HMG_ClrToHTML(CLR_HGRAY) )
     if  GetFontParam(oBrw:aSuperHead[i][7])[3]
         oStyle:setFontBold()
     end
     oStyle:Border( "All", 2, "Automatic",  "Continuous" )
     oStyle:alignWraptext()
   end
  Endif

    nColHead := 0
    FOR EACH i IN aColSel
       nColHead ++

       If lTsbHeading    
       //Определяем стили шапки таблицы
          oStyle := oXml:addStyle( "H" + hb_ntoc(nColHead) )
          oStyle:Border( "All", 2, "Automatic",  "Continuous" )
          oStyle:alignHorizontal( "Center" )
          oStyle:alignVertical( "Center" )
          oStyle:bgColor( HMG_ClrToHTML(CLR_HGRAY) )            // only Excel 2003
          oStyle:alignWraptext()
          oStyle:SetfontName( oBrw:aColumns[i]:XML_HdrFontName  )
          oStyle:SetfontSize( oBrw:aColumns[i]:XML_HdrFontSize )
          if oBrw:aColumns[i]:XML_HdrFontBold
        oStyle:setFontBold()
          endif
       endif
       // Определяем стили колонок
       /*oStyle := oXml:addStyle( "S" + hb_ntoc(i) )
       oStyle:Border( "All", 1, "Automatic",  "Continuous" )
       oStyle:alignHorizontal( oBrw:aColumns[i]:XML_AlignH  )
       oStyle:alignVertical( oBrw:aColumns[i]:XML_AlignV  )
       oStyle:SetfontName( oBrw:aColumns[i]:XML_ColFontName )
       oStyle:SetfontSize( oBrw:aColumns[i]:XML_ColFontSize )
       //oStyle:bgColor( HMG_ClrToHTML( CLR_BLUE) ) 
       if oBrw:aColumns[i]:XML_ColFontBold
     oStyle:setFontBold()
       end
       oStyle:alignWraptext()

       if oBrw:aColumns[i]:XML_Format <> NIL
     oStyle:setNumberFormat( oBrw:aColumns[i]:XML_Format )
       end*/

       // Определяем стили подвалов
       If lTsbFooting    
          oStyle := oXml:addStyle( "F" + hb_ntoc(nColHead) )
          oStyle:Border( "All", 2, "Automatic",  "Continuous" )
          oStyle:alignHorizontal( "Center" )
          oStyle:alignVertical( "Center" )
          oStyle:bgColor( HMG_ClrToHTML(CLR_HGRAY) )

          oStyle:alignWraptext()
          oStyle:SetfontName( oBrw:aColumns[i]:XML_FootFontName  )
          oStyle:SetfontSize( oBrw:aColumns[i]:XML_FootFontSize )
          if oBrw:aColumns[i]:XML_HdrFontBold
           oStyle:setFontBold()
          end
        Endif
   NEXT

   // Определяем используемые стили ячеек прогоном бровса
   Eval( oBrw:bGoTop )
   nLen   := oBrw:nLen
   nLine  := 1

   While  nLine <= nLen
       nColHead := 0
       FOR EACH nCol IN aColSel
          nColHead ++

          nColor := myColorN( oBrw:aColumns[nCol]:nClrBack, oBrw, nCol, oBrw:nAt ) 
          // в aColors храним массивы ( строка, столбец. стиль )
          Aadd( aColors, {nLine, nColHead, "S" + HMG_ClrToHTML(nColor)} )

          nAlign := oBrw:aColumns[nCol]:nAlign
          switch nAlign
            Case  DT_CENTER
                 cAlign := "Center"
                 Exit
            Case  DT_LEFT
                 cAlign := "Left"
                 Exit
            Case  DT_RIGHT
                 cAlign := "Right"
                 Exit
          end Switch

          oStyle := oXml:addStyle( "S" + HMG_ClrToHTML(nColor) )
          oStyle:Border( "All", 1, "Automatic",  "Continuous" )
          oStyle:alignHorizontal( cAlign )
          oStyle:alignVertical( "Center" )
          //шрифт
          hFont := oBrw:aColumns[nCol]:hFont
          If hb_isBlock( hFont ) 
             hFont := Eval(hFont, oBrw:nAt, nCol, oBrw )
          end
          oStyle:SetfontName(   GetFontParam(hFont)[1]  )
          oStyle:SetfontSize(   GetFontParam(hFont)[2] )
          if  GetFontParam(hFont)[3]
                 oStyle:setFontBold()
          end
          oStyle:bgColor( HMG_ClrToHTML(nColor) ) 
          oStyle:alignWraptext()

      NEXT

      If hProgress != Nil
         If nLine % nEvery == 0
            SendMessage(hProgress, PBM_SETPOS,nLine,0)
         EndIf
      EndIf

      nLine++
      nSkip := oBrw:Skip(1)
      SysRefresh()
      IF nSkip ==0
         EXIT
      ENDIF
   End

   // Пишем название отчета
   nRow := 1
   oSheet:writeString( nRow, 1, aTitle[1]  , "Title" )
   //oSheet:cellMerge(    nRow, 1, 4, 0 )

   nRow++
   nRow++
   IF lTsbSuperHd
   // Пишем Суперхидер

   IF lTsbSuperHd
      nRow ++
      nColHead := 0
      FOR EACH aCol IN oBrw:aSuperHead
         nColHead ++
         uData := If( ValType( aCol[3] ) == "B", Eval( aCol[3] ), aCol[3] )
         oSheet:writeString( nRow,  MaxNumFromArr(aColSel,aCol[1]), uData , "SH" + hb_ntoc(nColHead))
         oSheet:cellHeight( nRow, 1, oBrw:nHeightSuper )
         oSheet:cellMerge(    nRow, MaxNumFromArr(aColSel,aCol[1]), MinNumFromArr(aColSel,aCol[2]) - MaxNumFromArr(aColSel,aCol[1]), 0 )
      NEXT
      nRow ++
  Endif
  If lTsbHeading    

   // Пишем шапку бровса
   nColHead := 0
   FOR EACH i IN aColSel
     nColHead ++
     uData := AtRepl( Chr(13)+Chr(10), oBrw:aColumns[i]:cHeading, "&#10;" )
     oSheet:writeString( nRow,  nColHead, uData , "H" + hb_ntoc(nColHead) )
   End
   oSheet:cellHeight( nRow, 1, oBrw:nHeightHead )
   nRow ++
 Endif

   If hProgress != Nil
      SendMessage( hProgress, PBM_SETPOS, 0, 0 )
   EndIf

   // Пишем таблицу
   Eval( oBrw:bGoTop )

   nLen   := oBrw:nLen
   nLine  := 1

   While  nLine <= nLen
      oSheet:cellHeight( nRow, 1, oBrw:nHeightCell / 1.3 )
      
      nColHead := 0
      FOR EACH nCol IN aColSel
         nColHead ++
         uData  := Eval( oBrw:aColumns[ nCol ]:bData )
         cType := ValType( uData )
         j := Ascan( aColors, {|e| e[1] == nLine .and. e[2] == nColHead })

         switch cType
            Case "N"
               oSheet:writeNumber( nRow, nColHead, uData, aColors[j][3] )
               Exit
            Case "C"
          uData := AtRepl( Chr(13)+Chr(10), uData, "&#10;" )
               oSheet:writeString( nRow, nColHead, uData, aColors[j][3] )
               Exit
            Case "D"
                oSheet:writeDateTime( nRow, nColHead, Dtoc(uData), aColors[j][3] )
                Exit
            Case "L"
                oSheet:writeString( nRow, nColHead, IIF(uData, '.T.' , '.F.'), aColors[j][3] )
                Exit
            Case "U"
               oSheet:writeString( nRow, nColHead, '', aColors[j][3] )
               Exit
            Case "T"
               oSheet:writeString( nRow, nColHead, HB_TToC( uData), aColors[j][3] )
               Exit

         End Switch

      Next

      If hProgress != Nil
         If nLine % nEvery == 0
            SendMessage(hProgress, PBM_SETPOS,nLine,0)
         EndIf
      EndIf

      oBrw:Skip(1)
      nLine++
      nRow++
      SysRefresh()
   End

   If lTsbFooting    
   // Пишем подвал бровса
    nColHead := 0
    FOR EACH i IN aColSel
     nColHead ++
     uData := AtRepl( Chr(13)+Chr(10), oBrw:aColumns[i]:cFooting, "&#10;" )
     oSheet:writeString( nRow,  nColHead, uData , "H" + hb_ntoc(nColHead) )
    NEXT
   endif

   oSheet:cellHeight( nRow, 1, oBrw:nHeightFoot )
   Endif

   if oBrw:lIsDbf
      ( oBrw:cAlias )->( DbGoTo( nRec ) )
   else
      oBrw:GoPos(nOldRow, nOldCol)
   end

   oXml:writeData( cFile )

   If lActivate
      WaitWindow( 'Loading the report in EXCEL ...', .T. ) 

      hb_memowrit('_e_.cmd', '@Start Excel ' + cFile + CRLF)
      RUN '_e_.cmd'
      InkeyGui(1000)
      fErase('_e_.cmd')

      // можно и так, если назначен Excel для открытия *.xml
      // ShellExecute( 0, "Open", cFile,,, 3 )  
      INKEYGUI(800)
      WaitWindow()            // close the wait window
   EndIf

   If hProgress != Nil
      SendMessage( hProgress, PBM_SETPOS, 0, 0 )
   EndIf

RETURN NIL

* =======================================================================================
// Массив выводимых колонок
STATIC FUNCTION CalcAcolselForTbl( oBrw,aColSel)
   LOCAL lIsNotVisible :=.f., Arab, InCol, nCol
   InCol := If(oBrw:lSelector,2,1)
   If aColSel != Nil .and. Len( aColSel) >0
       aRab := aColSel
   Else  
         Arab:={}
         For nCol := InCol TO Len( oBrw:aColumns )
            if oBrw:aColumns[nCol]:lVisible
               Aadd(Arab,nCol)
            Endif
         Next
   Endif
   RETURN aRab 

* =======================================================================================
// Взять номер из массива равным или меньше заданного 
STATIC FUNCTION MinNumFromArr(aColSel,nIn)
   LOCAL nRet := Ascan(aColSel,nIn), nI
   if nRet == 0
     For nI := 1 to Len(aColSel)
        nRet := aColSel[nI]
        if nRet> nIn .and. nI>1
          nRet := aColSel[nI-1]
        endif
     Next
     nRet:=Ascan(aColSel,nRet)
   Endif
   RETURN nRet

* =======================================================================================
// Взять номер из массива равным или больше заданного 
STATIC FUNCTION MaxNumFromArr(aColSel,nIn)
   LOCAL nRet := Ascan(aColSel,nIn), nI
   if nRet == 0
     For nI := 1 to Len(aColSel)
        nRet := aColSel[nI]
        if nRet > nIn 
          Exit
        endif
     Next
     nRet:=Ascan(aColSel,nRet)
   Endif
   RETURN nRet

* =======================================================================================
STATIC FUNCTION myColorN( nColor, oBrw, nCol, nAt )
   If Valtype( nColor ) == "B"
      If empty(nAt) 
         nColor := Eval( nColor, nCol, oBrw )
      Else
         nColor := Eval( nColor, nAt, nCol, oBrw )
      EndIf
   EndIf  

   If Valtype( nColor ) == "A"
      nColor := nColor[1]
   EndIf  

RETURN nColor
