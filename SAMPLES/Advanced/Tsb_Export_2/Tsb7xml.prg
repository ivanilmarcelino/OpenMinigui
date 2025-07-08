/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2018 Igor Nazarov
 * Copyright 2020 Sidorov Aleksandr <aksidorov@mail.ru>
 *
 * Экспорт таблицы в файл xml
 * Export spreadsheets to file xml
*/
#include "hmg.ch"
#include "tsbrowse.ch"

#define PBM_SETPOS           1026   // Устанавливает текущую позицию для индикатора выполнения и перерисовывает полосу, чтобы отразить новую позицию
#define FACTORHIGH           1      // 1.3 - уменьшаем высоту колонок
#define FACTORWIDTH          1      // 1.3 - уменьшаем ширину колонок
#define TYPE_EXCEL_FORMULA  '#'     // мой тип ФОРМУЛА для экселя

#require "hbxlsxml"

* =================================================================================
STATIC FUNCTION RunOpenFileXML(cFile)

      cFile := HB_ANSItoOEM(cFile) // OEM кодировка для русских букв
      // можно так, если принудительно нужно запустить Excel для открытия *.xml
      hb_memowrit('_run_.cmd', '@Start Excel "' + cFile + '"' + CRLF)
      ShellExecute( 0, "Open", '_run_.cmd',,, SW_HIDE )
      InkeyGui(1000)
      fErase('_run_.cmd')

      // можно и так, если назначена другая программа для открытия *.xml
      // cFile - должно быть в ANSI кодировке для русских букв
      //ShellExecute( 0, "Open", cFile,,, 3 )
RETURN NIL
* =================================================================================
FUNCTION Brw7Xml( aTsb, aXmlParam, aXmlTitle, aXmlFoot, hProgress)
   LOCAL oXml, oSheet, oStyle, uData, nColDbf, nTotal, aCol, aClr
   LOCAL nLen, nLine, cTitle, i, lTsbSuperHd, lTsbHeading, lTsbFooting
   LOCAL nRow, nCol, nAlign, nSkip, rPicture, nPoint, nColSH1, nColSH2
   LOCAL cStr, cAlign, cType, nColHead, aFont, aFontTemp
   Local cFile, lActivate, lSave, aFonf, cMsg
   DEFAULT hProgress := nil

   ////////////// структура отчёта ///////////////
   // титул xls-файла, если есть
   // таблица
   // подвал xls-файла, если есть

   CursorWait()
   IF Hb_LangSelect() == "ru.RU1251" ; cMsg := 'Создаю отчёт в XML-формате'
   ELSE                              ; cMsg := 'Create report in XML format'
   ENDIF
   WaitThreadCreateIcon( cMsg, nil )   // запуск со временем

   lTsbSuperHd := lTsbHeading := lTsbFooting := .f.
   nRow := nCol := nAlign := nSkip := 0
   cStr := cAlign := cType := ""
   aFont     := {}
   aFontTemp := {}
   cFile     := aXmlParam[1]  // НОВОЕ ИМЯ ФАЙЛА ВСЕГДА уникально и приходит в эту функцию
   lActivate := aXmlParam[2]
   lSave     := aXmlParam[3]
   aFonf     := aXmlParam[4]

   // проверка суперхидер таблицы
   If Len(aTsb[1])>0
      lTsbSuperHd := .t.
   ENDIF

   // проверка шапки таблицы
   If Len(aTsb[2])>0
      lTsbHeading := .t.
   Endif

   // проверка подвала таблицы
   If Len(aTsb[5])>0
      lTsbFooting := .t.
   Endif

   // Создаем объект XML
   oXml := ExcelWriterXML():New( cFile )
   oXml:setOverwriteFile( .T. )
   oXml:setCodePage( "RU1251" )

   // Определяем Лист
   oSheet := oXml:addSheet( "Sheet1" )

   nTotal  := Len(aTsb[4]) //количество строк таблицы
   nColDbf := Len(aTsb[4,1])//количество колонок

   // Определяем ширины колонок из бровса
   FOR nColHead:= 1 to nColDbf //Len(aTsb[4,1])
      oSheet:columnWidth(  nColHead, aXmlParam[5, nColHead ]/FACTORWIDTH )
   Next

   // Определяем стиль названия отчета
   If !Empty(aXmlTitle)
      For nRow := 1 TO Len(aXmlTitle)
         If Len(aXmlTitle[nRow]) >0
            oStyle := oXml:addStyle( "Title"+ hb_ntoc(nRow) )
            If aXmlTitle[nRow,6] != Nil
              oStyle:alignHorizontal( TbsXmlAlign( aXmlTitle[nRow,6] ) )
            Else
              oStyle:alignHorizontal("Center" )
            Endif
            oStyle:alignVertical( "Center" )
            aFont := aXmlTitle[nRow,4]
            oStyle:SetfontName( aFont[1])
            oStyle:SetfontSize( aFont[2])
            if aFont[ 3 ]
              oStyle:setFontBold()
            end
            If Len(aXmlTitle[nRow])>4.and.aXmlTitle[nRow,5] != Nil
              aClr  := aXmlTitle[nRow,5]
              oStyle:bgColor( HMG_ClrToHTML(RGB(aClr[2,1],aClr[2,2],aClr[2,3])),, HMG_ClrToHTML(RGB(aClr[1,1],aClr[1,2],aClr[1,3])) )
            EndIf
         EndIf
       Next
   Endif

   // Определяем стиль подвала
   If !Empty(aXmlFoot)
      For nRow := 1 TO Len(aXmlFoot)
         If Len(aXmlFoot[nRow]) >0
            oStyle := oXml:addStyle( "Foot"+ hb_ntoc(nRow) )
            If aXmlFoot[nRow,6] != Nil
              oStyle:alignHorizontal( TbsXmlAlign( aXmlFoot[nRow,6] ) )
            Else
              oStyle:alignHorizontal("Center" )
            Endif
            oStyle:alignVertical( "Center" )
            aFont := aXmlFoot[nRow,4]
            oStyle:SetfontName( aFont[1])
            oStyle:SetfontSize( aFont[2])
            if aFont[ 3 ]
              oStyle:setFontBold()
            end
            If Len(aXmlFoot[nRow])>4.and.aXmlFoot[nRow,5] != Nil
              aClr  := aXmlFoot[nRow,5]
              oStyle:bgColor( HMG_ClrToHTML(RGB(aClr[2,1],aClr[2,2],aClr[2,3])),, HMG_ClrToHTML(RGB(aClr[1,1],aClr[1,2],aClr[1,3])) )
            EndIf
         EndIf
       Next
   Endif

   // Определяем суперхидер
   IF lTsbSuperHd
      For i := 1 To len( aTsb[1])
         oStyle := oXml:addStyle( "SH" + hb_ntoc(i) )
         nAlign := aTsb[1,i,7]
         Switch nAlign
            case DT_LEFT    ;  cAlign := "Left"   ;  Exit
            case DT_CENTER  ;  cAlign := "Center" ;  Exit
            case DT_RIGHT   ;  cAlign := "Right"  ;  Exit
         End switch

         oStyle:alignHorizontal( cAlign )
         nAlign := aTsb[1,i,8]
         Switch nAlign
            case DT_LEFT    ;  cAlign := "Left"   ;  Exit
            case DT_CENTER  ;  cAlign := "Center" ;  Exit
            case DT_RIGHT   ;  cAlign := "Right"  ;  Exit
         End switch

         oStyle:alignVertical( cAlign )
         aFont := GetFontParam( aTsb[1,i,3])
         oStyle:SetfontName( aFont[ 1 ] )
         oStyle:SetfontSize( aFont[ 2 ] )
         if aFont[ 3 ]
            oStyle:setFontBold()
         endif
         oStyle:Border( "All", 2, "Automatic",  "Continuous" )
         oStyle:alignWraptext()
      Next
   Endif

   // Определяем стили шапки колонок
    FOR nColHead:= 1 to nColDbf //Len(aTsb[4,1])
       // Определяем стили шапки таблицы
       If lTsbHeading
          oStyle := oXml:addStyle( "H" + hb_ntoc(nColHead) )
          oStyle:Border( "All", 2, "Automatic",  "Continuous" )
          oStyle:alignHorizontal( "Center" )
          oStyle:alignVertical( "Center" )
          oStyle:alignWraptext()
          aFont := GetFontParam( aTsb[2,nColHead,3])
          oStyle:SetfontName( aFont[ 1 ] )
          oStyle:SetfontSize( aFont[ 2 ] )
          if aFont[ 3 ]//oBrw:aColumns[i]:XML_HdrFontBold
             oStyle:setFontBold()
          endif
          oStyle:alignWraptext()
       Endif
       // Определяем стили нумератора
       If lTsbHeading
          oStyle := oXml:addStyle( "N" + hb_ntoc(nColHead) )
          oStyle:Border( "All", 2, "Automatic",  "Continuous" )
          oStyle:alignHorizontal( "Center" )
          oStyle:alignVertical( "Center" )
          oStyle:alignWraptext()
          aFont := GetFontParam( aTsb[3,nColHead,3])
          oStyle:SetfontName( aFont[ 1 ] )
          oStyle:SetfontSize( aFont[ 2 ] )
          if aFont[ 3 ]//oBrw:aColumns[i]:XML_HdrFontBold
             oStyle:setFontBold()
          endif
          oStyle:alignWraptext()
       Endif

       //Определяем стили колонок
       oStyle := oXml:addStyle( "S" + hb_ntoc(nColHead) )
       oStyle:Border( "All", 1, "Automatic",  "Continuous" )
       oStyle:alignHorizontal( aTsb[4,1,nColHead,8] )
       oStyle:alignVertical( 'Center')
       aFont := GetFontParam(aTsb[4,1,nColHead,3])
       oStyle:SetfontName( aFont[ 1 ] )
       oStyle:SetfontSize( aFont[ 2 ] )
       if aFont[ 3 ]
          oStyle:setFontBold()
       endif
       oStyle:alignWraptext()
       cType    := aTsb[4,1,nColHead,5]
       rPicture := aTsb[4,1,nColHead,6]
       Do case
          Case cType=="D"
             oStyle:setNumberFormat( "@")
          case cType == 'N'
           if !empty(rPicture)  
            nPoint   := AT('.', rPicture )
             if nPoint == 0
                rPicture :='#0'
             else
                rPicture := Repl("#",nPoint-2) + '0.' + Repl("0",Len(rPicture)-nPoint)
             endif
             oStyle:setNumberFormat( rPicture )
           endif
          Otherwise
             oStyle:setNumberFormat( "@")
       endcase

       If lTsbFooting
       //Определяем стили колонок подвала таблицы
          oStyle := oXml:addStyle( "F" + hb_ntoc(nColHead) )
          oStyle:Border( "All", 2, "Automatic",  "Continuous" )
          oStyle:alignHorizontal( "Center" )
          oStyle:alignVertical( "Center" )
          oStyle:alignWraptext()
          aFont := GetFontParam( aTsb[5,nColHead,3])
          oStyle:SetfontName( aFont[ 1 ] )
          oStyle:SetfontSize( aFont[ 2 ] )
          if aFont[ 3 ]
             oStyle:setFontBold()
          endif
          oStyle:alignWraptext()
       Endif
   end

   nRow := 1
   // Пишем название отчета
   If !Empty(aXmlTitle)
      For i := 1 TO Len(aXmlTitle)
         if Len(aXmlTitle[i]) >0
            cTitle := aXmlTitle[i,3]
            cTitle := AllTrim( cTitle )
            nCol := if (Empty(aXmlTitle[i,2]),nColDbf,aXmlTitle[i,2])
            oSheet:writeString( nRow, aXmlTitle[i,1], cTitle, "Title"+ hb_ntoc(i) )
            oSheet:cellMerge(    nRow, aXmlTitle[i,1], nCol - aXmlTitle[i,1], 0 )
         EndIf
       ++nRow
       Next
       ++nRow
    Endif

   nCol := 0
   // Пишем суперхидер
   IF lTsbSuperHd
      nRow ++
      FOR EACH aCol IN aTsb[1]
         nCol ++
         uData := if(Empty(aCol[4]),' ',aCol[4])
         nColSh1 := if(aCol[5]>0, aCol[5], nColSh2+1)
         // Если  с -1 не последняя и следующая нормальная, то берем до следующей
         if aCol[6]>0.and.nCol<Len(aTsb[1])
            if aTsb[1,nCol+1,5]>0
                nColSh2 := aTsb[1,nCol,5]-1
            endif
         endif
         nColSh2 := if(aCol[6]>0, aCol[6], if(nCol==Len(aTsb[1]), nColDbf, nColSh1))
         oSheet:writeString( nRow,  nColSh1, uData , "SH" + hb_ntoc(nCol))
         oSheet:cellMerge(    nRow, nColSh1, nColSh2 - nColSh1, 0 )
      NEXT
   Endif

   // Пишем шапку бровса
   If lTsbHeading
      nRow ++
      nColHead := 0
      FOR EACH i IN aTsb[2]
        nColHead ++
        uData := AtRepl( Chr(13)+Chr(10), aTsb[2,nColHead,4], "&#10;" )
        oSheet:writeString( nRow,  nColHead, uData , "H" + hb_ntoc(nColHead) )
      NEXT
   Endif

   // Пишем шапку нумератор
   If Len(aTsb[3])>0
      nRow ++
      nColHead := 0
      FOR EACH i IN aTsb[3]
        nColHead ++
        uData := AtRepl( Chr(13)+Chr(10), aTsb[3,nColHead,4], "&#10;" )
        oSheet:writeString( nRow,  nColHead, uData , "H" + hb_ntoc(nColHead) )
      NEXT
   Endif

   // Пишем таблицу
   nRow ++
   nLen   := nTotal
   nLine  := 1

   While  nLine <= nLen
      oSheet:cellHeight( nRow, 1, aXmlParam[6]/FACTORHIGH )

      FOR nColHead := 1 to nColDbf
         uData    := aTsb[4,nLine,nColHead,4]
         cType    := aTsb[4,nLine,nColHead,5]
         rPicture := aTsb[4,nLine,nColHead,6]

         do Case
            Case (cType=='@'.or.cType=='D').and.Empty(uData)
               uData := ''
            Case ValType( uData )=="D"
               uData := hb_dtoc( uData , "dd.mm.yyyy")
            Case cType == 'L'
               cType :='C'
            Case rPicture != Nil .and. uData != Nil .and. cType !='N'
              uData := Transform( uData, rPicture )
         endCase
         uData := If( ValType( uData )=="N", uData , ;
         If( ValType( uData )=="L", If( uData ,".T." ,".F." ), cValToChar( uData ) ) )
         Do case
            Case (cType==TYPE_EXCEL_FORMULA)
               oSheet:writeFormula( '', nRow, nColHead, uData, 'S' + hb_ntoc(nColHead))
            Case cType = "N"
               oSheet:writeNumber( nRow, nColHead, uData, 'S' + hb_ntoc(nColHead) )
            Case cType = "C".or.cType=='@'.or.cType=='D'.or.cType=='L'
               uData := AtRepl( Chr(13)+Chr(10), uData, "&#10;" )
               oSheet:writeString( nRow, nColHead, uData, 'S' + hb_ntoc(nColHead))
            Case cType = "U"
               oSheet:writeString( nRow, nColHead, '', 'S' + hb_ntoc(nColHead))
            Case cType = "T"
               oSheet:writeString( nRow, nColHead, HB_TToC( uData), 'S' + hb_ntoc(nColHead))
            Otherwise
               uData := AtRepl( Chr(13)+Chr(10), uData, "&#10;" )
               oSheet:writeString( nRow, nColHead, uData, 'S' + hb_ntoc(nColHead))
         End Case
         DO EVENTS
      Next
      nLine++
      nRow++
   End
   // Пишем подвал бровса
   If lTsbFooting
      FOR nColHead:= 1 to Len(aTsb[5])
        uData := AtRepl( Chr(13)+Chr(10), aTsb[5,nColHead,4], "&#10;" )
        oSheet:writeString( nRow,  nColHead , uData , "F" + hb_ntoc(nColHead) )
      End
   Endif

   nRow++
   If !Empty(aXmlFoot)
      For i := 1 TO Len(aXmlFoot)
         if Len(aXmlFoot[i]) >0
            cTitle := aXmlFoot[i,3]
            cTitle := AllTrim( cTitle )
            nCol := if (Empty(aXmlFoot[i,2]),nColDbf,if(aXmlFoot[i,2]<0,aXmlFoot[i,1],aXmlFoot[i,2]))
            oSheet:writeString( nRow, aXmlFoot[i,1], cTitle, "Foot"+ hb_ntoc(i) )
            oSheet:cellMerge(    nRow, aXmlFoot[i,1], nCol - aXmlFoot[i,1], 0 )
         EndIf
       ++nRow
       Next
       ++nRow
    Endif


   oXml:writeData( cFile )

   WaitThreadCloseIcon()  // kill the window waiting
   CursorArrow()

   If lActivate
      WaitWindow( 'Load the XML file into the program ...', .T. )
      RunOpenFileXML(cFile)   // запуск программы
      INKEYGUI(800)
      WaitWindow()            // close the wait window
   EndIf

   RETURN NIL

* =================================================================================
FUNCTION Brw7XmlColor( aTsb, aXmlParam, aXmlTitle, aXmlFoot, hProgress )
   LOCAL oXml, oSheet, oStyle, uData, nColDbf, nTotal, nEvery, aCol, aClr
   LOCAL nLine, cTitle, i, j, lTsbSuperHd, lTsbHeading, lTsbFooting
   LOCAL nRow, nCol, nAlign, nSkip, rPicture, nColSH1, nColSH2,cMsg
   LOCAL cStr, cAlign, cType, nColHead, aFont, aFontTemp, aColors
   Local cFile, lActivate, lSave, aFonf, nColor, nDigcoldbf, nDigTotal
   DEFAULT hProgress := nil

   CursorWait()
   IF Hb_LangSelect() == "ru.RU1251" ; cMsg := 'Создаю отчёт в XML-формате'
   ELSE                              ; cMsg := 'Create report in XML format'
   ENDIF
   WaitThreadCreateIcon( cMsg, nil )   // запуск со временем

   ////////////// структура отчёта ///////////////
   // титул xls-файла, если есть
   // таблица
   // подвал xls-файла, если есть

   lTsbSuperHd := lTsbHeading := lTsbFooting := .f.
   nRow := nCol := nAlign := nSkip := nColor := 0
   cStr := cAlign := cType := ""
   aColors   := {{0,0,""}}
   aFont     := {}
   aFontTemp := {}
   cFile     := aXmlParam[1]  // НОВОЕ ИМЯ ФАЙЛА ВСЕГДА уникально и приходит в эту функцию
   lActivate := aXmlParam[2]
   lSave     := aXmlParam[3]
   aFonf     := aXmlParam[4]
   nTotal    := Len(aTsb[4])      // количество строк таблицы
   nColDbf   := Len(aTsb[4,1])    // количество колонок

   // проверка суперхидер таблицы
   If Len(aTsb[1])>0
      lTsbSuperHd := .t.
   ENDIF

   // проверка шапки таблицы
   If Len(aTsb[2])>0
      lTsbHeading := .t.
   Endif

   // проверка подвала таблицы
   If Len(aTsb[5])>0
      lTsbFooting := .t.
   Endif

   If hProgress != Nil
      SetProgressBarRange ( hProgress , 1 , nTotal )
      SendMessage(hProgress, PBM_SETPOS, 0, 0)
      nEvery := Max( 1, Int( nTotal * .02 ) ) // refresh hProgress every 2 %
   EndIf

   // Создаем объект XML
   oXml := ExcelWriterXML():New( cFile )
   oXml:setOverwriteFile( .T. )
   oXml:setCodePage( "RU1251" )

   // Определяем Лист
   oSheet := oXml:addSheet( "Sheet1" )

   // Определяем ширины колонок из бровса
   nColHead := 0
   FOR nColHead:= 1 to nColDbf
      oSheet:columnWidth(  nColHead, aXmlParam[5, nColHead ]/FACTORWIDTH )
   Next
   // Определяем стиль названия отчета
   If !Empty(aXmlTitle)
      For nRow := 1 TO Len(aXmlTitle)
         If Len(aXmlTitle[nRow]) >0
            oStyle := oXml:addStyle( "Title"+ hb_ntoc(nRow) )
            If aXmlTitle[nRow,6] != Nil
              oStyle:alignHorizontal( TbsXmlAlign( aXmlTitle[nRow,6] ) )
            Else
              oStyle:alignHorizontal("Center" )
            Endif
            oStyle:alignVertical( "Center" )
            aFont := aXmlTitle[nRow,4]
            oStyle:SetfontName( aFont[1])
            oStyle:SetfontSize( aFont[2])
            if aFont[ 3 ]
              oStyle:setFontBold()
            end
            If Len(aXmlTitle[nRow])>4.and.aXmlTitle[nRow,5] != Nil
              aClr  := aXmlTitle[nRow,5]
              oStyle:bgColor( HMG_ClrToHTML(RGB(aClr[2,1],aClr[2,2],aClr[2,3])),, HMG_ClrToHTML(RGB(aClr[1,1],aClr[1,2],aClr[1,3])) )
            EndIf
         EndIf
       Next
   Endif

   // Определяем стиль подвала
   If !Empty(aXmlFoot)
      For nRow := 1 TO Len(aXmlFoot)
         If Len(aXmlFoot[nRow]) >0
            oStyle := oXml:addStyle( "Foot"+ hb_ntoc(nRow) )
            If aXmlFoot[nRow,6] != Nil
              oStyle:alignHorizontal( TbsXmlAlign( aXmlFoot[nRow,6] ) )
            Else
              oStyle:alignHorizontal("Center" )
            Endif
            oStyle:alignVertical( "Center" )
            aFont := aXmlFoot[nRow,4]
            oStyle:SetfontName( aFont[1])
            oStyle:SetfontSize( aFont[2])
            if aFont[ 3 ]
              oStyle:setFontBold()
            end
            If Len(aXmlFoot[nRow])>4.and.aXmlFoot[nRow,5] != Nil
              aClr  := aXmlFoot[nRow,5]
              oStyle:bgColor( HMG_ClrToHTML(RGB(aClr[2,1],aClr[2,2],aClr[2,3])),, HMG_ClrToHTML(RGB(aClr[1,1],aClr[1,2],aClr[1,3])) )
            EndIf
         EndIf
       Next
   Endif

   // Определяем суперхидер
   IF lTsbSuperHd
      For i := 1 To len( aTsb[1])
         oStyle := oXml:addStyle( "SH" + hb_ntoc(i) )
         nAlign := aTsb[1,i,7]
         switch nAlign
           case DT_LEFT    ;  cAlign := "Left"   ;  Exit
           case DT_CENTER  ;  cAlign := "Center" ;  Exit
           case DT_RIGHT   ;  cAlign := "Right"  ;  Exit
         End switch

         oStyle:alignHorizontal( cAlign )
         nAlign := aTsb[1,i,8]
         switch nAlign
           case DT_LEFT    ;  cAlign := "Left"   ;  Exit
           case DT_CENTER  ;  cAlign := "Center" ;  Exit
           case DT_RIGHT   ;  cAlign := "Right"  ;  Exit
         End switch

         oStyle:alignVertical( cAlign )

         oStyle:bgColor( HMG_ClrToHTML(aTsb[1,i,2]),, HMG_ClrToHTML(aTsb[1,i,1]) )
         aFont := GetFontParam( aTsb[1,i,3])
         oStyle:SetfontName( aFont[ 1 ] )
         oStyle:SetfontSize( aFont[ 2 ] )
         if aFont[ 3 ]
            oStyle:setFontBold()
         end
         oStyle:Border( "All", 2, "Automatic",  "Continuous" )
         oStyle:alignWraptext()
      end
   Endif

   FOR nColHead:= 1 to nColDbf

       // Определяем стили шапки таблицы
       If lTsbHeading
          oStyle := oXml:addStyle( "H" + hb_ntoc(nColHead) )
          oStyle:Border( "All", 2, "Automatic",  "Continuous" )
          oStyle:alignHorizontal( "Center" )
          oStyle:alignVertical( "Center" )
          oStyle:bgColor( HMG_ClrToHTML(aTsb[2,nColHead,2]),, HMG_ClrToHTML(aTsb[2,nColHead,1]) )
          oStyle:alignWraptext()
          aFont := GetFontParam( aTsb[2,nColHead,3])
          oStyle:SetfontName( aFont[ 1 ] )
          oStyle:SetfontSize( aFont[ 2 ] )
          if aFont[ 3 ]//oBrw:aColumns[i]:XML_HdrFontBold
             oStyle:setFontBold()
          end
       endif

       // Определяем стили нумератора
       If len(aTsb[3])>0
          oStyle := oXml:addStyle( "N" + hb_ntoc(nColHead) )
          oStyle:Border( "All", 2, "Automatic",  "Continuous" )
          oStyle:alignHorizontal( "Center" )
          oStyle:alignVertical( "Center" )
          oStyle:bgColor( HMG_ClrToHTML(aTsb[3,nColHead,2]),, HMG_ClrToHTML(aTsb[3,nColHead,1]) )
          oStyle:alignWraptext()
          aFont := GetFontParam( aTsb[3,nColHead,3])
          oStyle:SetfontName( aFont[ 1 ] )
          oStyle:SetfontSize( aFont[ 2 ] )
          if aFont[ 3 ]//oBrw:aColumns[i]:XML_HdrFontBold
             oStyle:setFontBold()
          end
          oStyle:alignWraptext()
       Endif

       // Определяем стили подвалов
       If lTsbFooting
          oStyle := oXml:addStyle( "F" + hb_ntoc(nColHead) )
          oStyle:Border( "All", 2, "Automatic",  "Continuous" )
          oStyle:alignHorizontal( "Center" )
          oStyle:alignVertical( "Center" )
          oStyle:bgColor( HMG_ClrToHTML(aTsb[5,nColHead,2]),, HMG_ClrToHTML(aTsb[5,nColHead,1]) )
          oStyle:alignWraptext()
          aFont := GetFontParam( aTsb[5,nColHead,3])
          oStyle:SetfontName( aFont[ 1 ] )
          oStyle:SetfontSize( aFont[ 2 ] )
          if aFont[ 3 ]
             oStyle:setFontBold()
          endif
       Endif
      DO EVENTS
   NEXT

   // Определяем используемые стили ячеек прогоном бровса
   nLine  := 1

   Ndigcoldbf := Ndigit(nColDbf)    //количество цифр в колонках
   NdigTotal  := Ndigit(nTotal)    //количество цифр в строках таблицы

   While  nLine <= nTotal
      FOR nColHead := 1 to nColDbf
         ncolor := strzero(nLine,NdigTotal)+strzero(nColHead,Ndigcoldbf)
         Aadd( aColors, {nLine, nColHead, "S" + ncolor} )

         oStyle := oXml:addStyle( "S" + nColor )
         oStyle:Border( "All", 1, "Automatic",  "Continuous" )
         oStyle:alignHorizontal( aTsb[4,1,nColHead,8] )
         oStyle:alignVertical( "Center" )
         //шрифт
         aFont := GetFontParam(aTsb[4,1,nColHead,3])
         oStyle:SetfontName( aFont[ 1 ] )
         oStyle:SetfontSize( aFont[ 2 ] )
         if aFont[ 3 ]
           oStyle:setFontBold()
         end
         oStyle:bgColor( HMG_ClrToHTML(aTsb[4,nLine,nColHead,2]),, HMG_ClrToHTML(aTsb[4,nLine,nColHead,1]) )

         oStyle:alignWraptext()
         DO EVENTS
      NEXT

      If hProgress != Nil
         If nLine % nEvery == 0
            SendMessage(hProgress, PBM_SETPOS,nLine,0)
         EndIf
      EndIf
      nLine++
   End

   // Пишем название отчета
   nRow :=1

   If !Empty(aXmlTitle)
      For i := 1 TO Len(aXmlTitle)
         if Len(aXmlTitle[i]) >0
            cTitle := aXmlTitle[i,3]
            cTitle := AllTrim( cTitle )
            nCol := if (Empty(aXmlTitle[i,2]),nColDbf,aXmlTitle[i,2])
            oSheet:writeString( nRow, aXmlTitle[i,1], cTitle, "Title"+ hb_ntoc(i) )
            oSheet:cellMerge(    nRow, aXmlTitle[i,1], nCol - aXmlTitle[i,1], 0 )
         EndIf
       ++nRow
       Next
       ++nRow
    Endif

   // Пишем Суперхидер
   nCol := 0
   IF lTsbSuperHd
      nRow ++
      FOR EACH aCol IN aTsb[1]
         nCol ++
         uData := if(Empty(aCol[4]),' ',aCol[4])
         nColSh1 := if(aCol[5]>0, aCol[5], nColSh2+1)
         // Если  с -1 не последняя и следующая нормальная, то берем до следующей
         if aCol[6]>0.and.nCol<Len(aTsb[1])
            if aTsb[1,nCol+1,5]>0
                nColSh2 := aTsb[1,nCol,5]-1
            endif
         endif
         nColSh2 := if(aCol[6]>0, aCol[6], if(nCol==Len(aTsb[1]), nColDbf, nColSh1))
         oSheet:writeString( nRow,  nColSh1, uData , "SH" + hb_ntoc(nCol))
         oSheet:cellMerge(    nRow, nColSh1, nColSh2 - nColSh1, 0 )
      NEXT
   Endif

   // Пишем шапку бровса
   If lTsbHeading
     nRow ++
     nColHead := 0
     FOR EACH i IN aTsb[2]
       nColHead ++
       uData := AtRepl( Chr(13)+Chr(10), aTsb[2,nColHead,4], "&#10;" )
       oSheet:writeString( nRow,  nColHead, uData , "H" + hb_ntoc(nColHead) )
     End
   Endif

   // Пишем шапку нумератор
   If Len(aTsb[3])>0
     nRow ++
     nColHead := 0
     FOR EACH i IN aTsb[3]
       nColHead ++
       uData := AtRepl( Chr(13)+Chr(10), aTsb[3,nColHead,4], "&#10;" )
       oSheet:writeString( nRow,  nColHead, uData , "H" + hb_ntoc(nColHead) )
     End
   Endif

   If hProgress != Nil
      SendMessage( hProgress, PBM_SETPOS, 0, 0 )
   EndIf

   // Пишем таблицу
   nLine  := 1
   nRow ++
   While  nLine <= nTotal
     oSheet:cellHeight( nRow, 1, aXmlParam[6] / FACTORHIGH )

      FOR nColHead := 1 to nColDbf
         uData    := aTsb[4,nLine,nColHead,4]
         cType    := aTsb[4,nLine,nColHead,5]
         rPicture := aTsb[4,nLine,nColHead,6]
         j := Ascan( aColors, {|e| e[1] == nLine .and. e[2] == nColHead })
         do Case
            Case (cType=='@'.or.cType=='D').and.Empty(uData)
               uData := ''
            Case ValType( uData )=="D"
               uData := hb_dtoc( uData , "dd.mm.yyyy")
            Case cType == 'L'
               cType :='C'
            Case rPicture != Nil .and. uData != Nil .and. cType !='N'
              uData := Transform( uData, rPicture )
         endCase
         uData := If( ValType( uData )=="N", uData , ;
         If( ValType( uData )=="L", If( uData ,".T." ,".F." ), cValToChar( uData ) ) )
         Do case
            Case (cType==TYPE_EXCEL_FORMULA)
               oSheet:writeFormula( '', nRow, nColHead, uData, aColors[j][3])
            Case cType = "N"
               oSheet:writeNumber( nRow, nColHead, uData, aColors[j][3] )
            Case cType = "C".or.cType=='@'.or.cType=='D'.or.cType=='L'
               uData := AtRepl( Chr(13)+Chr(10), uData, "&#10;" )
               oSheet:writeString( nRow, nColHead, uData, aColors[j][3])
            Case cType = "U"
               oSheet:writeString( nRow, nColHead, '', aColors[j][3])
            Case cType = "T"
               oSheet:writeString( nRow, nColHead, HB_TToC( uData), aColors[j][3])
            Otherwise
               uData := AtRepl( Chr(13)+Chr(10), uData, "&#10;" )
               oSheet:writeString( nRow, nColHead, uData, aColors[j][3])
         End Case
         DO EVENTS
      Next

      If hProgress != Nil
         If nLine % nEvery == 0
            SendMessage(hProgress, PBM_SETPOS,nLine,0)
         EndIf
      EndIf
      nLine++
      nRow++
   end

   // Пишем подвал бровса
   If lTsbFooting
      FOR nColHead:= 1 to Len(aTsb[5])
        uData := AtRepl( Chr(13)+Chr(10), aTsb[5,nColHead,4], "&#10;" )
        oSheet:writeString( nRow,  nColHead , uData , "F" + hb_ntoc(nColHead) )
      Next
   Endif

   If !Empty(aXmlFoot)
      For i := 1 TO Len(aXmlFoot)
         if Len(aXmlFoot[i]) >0
            cTitle := aXmlFoot[i,3]
            cTitle := AllTrim( cTitle )
            nCol := if (Empty(aXmlFoot[i,2]),nColDbf,if(aXmlFoot[i,2]<0,aXmlFoot[i,1],aXmlFoot[i,2]))
            oSheet:writeString( nRow, aXmlFoot[i,1], cTitle, "Foot"+ hb_ntoc(i) )
            oSheet:cellMerge(    nRow, aXmlFoot[i,1], nCol - aXmlFoot[i,1], 0 )
         EndIf
       ++nRow
       Next
       ++nRow
   Endif

   oXml:writeData( cFile )

   WaitThreadCloseIcon()  // kill the window waiting
   CursorArrow()

   If lActivate
      WaitWindow( 'Load the XML file into the program ...', .T. )
      RunOpenFileXML(cFile)   // запуск программы
      INKEYGUI(800)
      WaitWindow()            // close the wait window
   EndIf

   If hProgress != Nil
      SendMessage( hProgress, PBM_SETPOS, 0, 0 )
   EndIf

RETURN NIL

* =================================================================================
STATIC FUNCTION TbsXmlAlign(nAlign)
   LOCAL nRet := 0

   IF nAlign == DT_LEFT
      nRet := "Left"
   ELSEIF nAlign == DT_RIGHT
      nRet := "Right"
   ELSE
      nRet := "Center"
   ENDIF

   RETURN nRet

* =================================================================================
STATIC FUNCTION Ndigit(x)
   LOCAL nRet := 0
   Do case
      Case x<10
           nRet := 1
      Case x<100
           nRet := 2
      Case x<1000
           nRet := 3
      Case x<10000
           nRet := 4
      Case x<100000
           nRet := 5
      Case x<1000000
           nRet := 6
      Case x<10000000
           nRet := 7
      Case x<100000000
           nRet := 8
      Otherwise
           nRet :=10
   endcase
   RETURN nRet
