/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2020 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 * Copyright 2020 Sidorov Aleksandr <aksidorov@mail.ru>  Dmitrov, Moscow region
 *
 * Экспорт таблицы Word в файл doc через OLE
 * Export Word table to doc file via OLE
*/
#define _HMG_OUTLOG

#include "minigui.ch"
#include "tsbrowse.ch"
#include "word.ch"

#define wdWord8TableBehavior 0
#define wdWord9TableBehavior 1
#define wdAutoFitFixed       0
#define wdAutoFitContent     1
#define wdLineStyleSingle    1
#define WordSeparatorBox    '^'
#define MinPxToPnt          50    // минимальный размер колонки Word

#define PBM_SETPOS          1026  // Устанавливает текущую позицию для индикатора выполнения и перерисовывает полосу, чтобы отразить новую позицию
#define TYPE_EXCEL_FORMULA  '#'   // мой тип ФОРМУЛА для экселя
* =====================================================================================
FUNCTION Brw7DocOle( aTsb, aDocParam, aDocTitle, aDocFoot, aImage, hProgress, bExtern )
   Local oWord, oText, oTbl, oActive, cText, aRepl, oRange
   Local cMsg, nRowDbf, nColDbf, cVal, cTitle, aFont
   Local nTotal, nLine  := 1, nCount := 0, nLenHead := 0
   Local nmerge := 1, flag_new_OutWrd:=.f. , aColWidth:={}, rColWidth
   Local nRow, nCol, uData, nEvery, nColHead, rType, rPicture, aFColor
   Local findObject, aClr, nWidthTsb, nLeftRightMargin, nPxLRM
   Local oColumn, nWidth, nWidthWordTsb, nPxToPnt, oPar, aCol
   Local nAddPxToPnt:=0, arrnPxToPnt:={}, nColSh1, nColSh2
   Local lSeparator := .f., tSeparator, lTsbFooting :=.f.
   Local cFile, lActivate, lSave, aFonf
   Default hProgress := nil

   ////////////// структура отчёта ///////////////
   // титул Doc-файла, если есть
   // таблица
   // подвал Doc-файла, если есть

   cFile     := aDocParam[1]
   lActivate := aDocParam[2]
   lSave     := aDocParam[3]
   aFonf     := aDocParam[4]

   CursorWait()
   IF Hb_LangSelect() == "ru.RU1251" ; cMsg := 'Загружаю отчёт в'
   ELSE                              ; cMsg := 'Upload report to'
   ENDIF
   WaitThreadCreateIcon( cMsg, 'WORD OLE ...' )   // запуск без времени

   // Используем Ole из HBWIN.lib
   IF ( oWord := win_oleCreateObject( "Word.Application" ) ) == NIL
      cMsg := REPLICATE( "-._.", 16 ) + ";;"
      IF Hb_LangSelect() == "ru.RU1251"
         cMsg += "MS Word не доступен !;;   Ошибка"
         cVal := "Ошибка!"
      ELSE
         cMsg += "MS Word is not available !;;   Error"
         cVal := "Error!"
      ENDIF
      WaitThreadCloseIcon()  // kill the window waiting
      CursorArrow()

      cMsg += " [ " + win_oleErrorText() + " ];;"
      cMsg += REPLICATE( "-._.", 16 ) + ";;"
      cMsg := AtRepl( ";", cMsg, CRLF )
      MsgStop( cMsg , cVal )
      RETURN Nil
   ENDIF

   oWord:Visible := .F.                         // если открыты другие документы в Word-е
                                                // .T. показать Word на экране
   oWord:DisplayAlerts := .F.                   // убрать предупреждения Word
   oWord:Options:CheckSpellingAsYouType := .F.  // Отключить автопроверку текста документа

   oActive:=oWord:Documents:Add()

   If ! Empty( aImage )
     oPar := oActive:Paragraphs:Add()
     oActive:Shapes:AddPicture(aImage[1],.f.,.t.,,, PixelToPointX(aImage[2]),PixelToPointY(aImage[3]))
   Endif

   nTotal := Len(aTsb[4]) //количество строк таблицы
   nColDbf := Len(aTsb[4,1])//количество колонок

   If hProgress != Nil
      SendMessage(hProgress, PBM_SETPOS, nCount, 0)
   EndIf


   If hProgress != Nil
      SetProgressBarRange ( hProgress , 1 , nTotal )
      SendMessage(hProgress, PBM_SETPOS, 0, 0)
      nEvery := Max( 1, Int( nTotal * .02 ) ) // refresh hProgress every 2 %
   EndIf

   // Нужно определиться с форматом листа для DOC.
   // Единица измерения для Word равна пунктам (points)
   // так как размер таблицы представлены в пикселях, то будем считать далее в пикселах
      nWidthTsb := aDocParam[5] //oBrw:GetAllColsWidth()    // ширина всех колонок таблицы (пикселы)

   // Формат бумаги печати
   // Источник: http://biznessystem.ru/kakoj-razmer-v-pikselyah-imeet-list-formata-a4/
   // А4 = 2480 x 3508 px при dpi=300  // А4 = 1240 x 1754 px при dpi=150
   // А3 = 3508 x 4961 px при dpi=300  // А3 = 1754 x 2480 px при dpi=150

   // Размер печати таблицы на листе = отступ слева + отступ справа
   nLeftRightMargin := 29     // пунктам (points) (примерно 1 см) - отступ слева
   nPxLRM := 38.8 * 2         // пикселей - отступ слева + отступ справа
   // ------- Установка параметров страницы (листа) -------
   If nWidthTsb + nPxLRM >= 1754
      //? "  ==> The size of the paper to print the table is larger than A4"
      // Word имеет ограничение в установке размеров - 55,87 см по любой из сторон листа.
      // 55.87 сантиметров равно 1 583.717 пунктов
      // Высоту листа возьмем как у A4 (210х297 мм) == 297
      // 297 миллиметров равно 841.889862 пункта
      oActive:PageSetup:PageWidth = 1583
      oActive:PageSetup:PageHeight = 841
      // альбомная ориентация
      oActive:PageSetup:Orientation := wdOrientLandscape
      // книжная ориентация
      // oActive:PageSetup:Orientation := wdOrientPortrait
   Else
      //? "  ==> The size of the paper to print the table is A4 "
      oActive:PageSetup:PaperSize := wdPaperA4
      If nWidthTsb + nPxLRM < 1240
         // книжная ориентация
         oActive:PageSetup:Orientation := wdOrientPortrait
      Else
         // альбомная ориентация
         oActive:PageSetup:Orientation := wdOrientLandscape
      Endif
   Endif

   // поля страницы (отступ слева и справа)
   oActive:PageSetup:LeftMargin  := nLeftRightMargin //~1 см
   oActive:PageSetup:RightMargin := nLeftRightMargin //~1 см
   // поля страницы (отступ сверху и внизу)
   oActive:PageSetup:TopMargin    := nLeftRightMargin //~1 см
   oActive:PageSetup:BottomMargin := nLeftRightMargin //~1 см

  // -------- Заголовок таблицы ------------------
   If ! Empty( aDocTitle )
      For nRow := 1 TO Len(aDocTitle)
            oPar := oActive:Paragraphs:Add()
            // oText := oPar:Range
            oPar := oActive:Paragraphs:Add()
            oText := oPar:Range
            If Len(aDocTitle[nRow])>3.and.aDocTitle[nRow,4] != Nil
              aFont := aDocTitle[nRow,4]
              oText:Font:Name = aFont[1]
              oText:Font:Size = aFont[2]
              oText:Font:Bold = aFont[3]
            Endif
            If Len(aDocTitle[nRow])>4.and.aDocTitle[nRow,5] != Nil
              aClr  := aDocTitle[nRow,5]
              oText:Font:Color = RGB(aClr[1,1],aClr[1,2],aClr[1,3])
              oText:Font:Shading:BackgroundPatternColor := RGB(aClr[2,1],aClr[2,2],aClr[2,3])
            EndIf
            if Len(aDocTitle[nRow]) >0
               cTitle := aDocTitle[nRow,3]
               cTitle := AllTrim( cTitle )
            Else
               cTitle := ""
            Endif
            oText:Text := cTitle
            If Len(aDocTitle[nRow])>5.and.aDocTitle[nRow,6] != Nil
              oText:ParagraphFormat:Alignment := TbsDocAlign( aDocTitle[nRow,6])
            Else
              oText:ParagraphFormat:Alignment := TbsDocAlign( DT_CENTER )
            Endif
       Next
   EndIf
   // ------- создание таблицы ---------------
   oRange := oActive:Paragraphs:Add()

   nWidth := oActive:PageSetup:PageWidth
   nWidthWordTsb := oWord:PixelsToPoints( nWidthTsb, 0 )

   cText := ""

   oText:ParagraphFormat:Alignment = wdAlignParagraphCenter
   aRepl := {}
   // Суперхидер
   If Len(aTsb[1])>0
      For nCol := 1 To nColDbf
         cText += "" + WordSeparatorBox
      Next
      ++ nLenHead
   EndIf

   nColHead := 0
   // Хидер
   If  Len(aTsb[2])>0
      For nCol := 1 To nColDbf
         cText += "" + WordSeparatorBox
      Next
      ++ nLenHead
   Endif
   // Нумератор
   If  Len(aTsb[3])>0
      For nCol := 1 To nColDbf
         cText += "" + WordSeparatorBox
      Next
      ++ nLenHead
   Endif
   // Данные
   FOR nRow:= 1 to nTotal //Len(aTsb[4])
      FOR nColHead:= 1 to nColDbf //Len(aTsb[4,1])
         uData    := aTsb[4,nRow,nColHead,4]
         rType    := aTsb[4,nRow,nColHead,5]
         rPicture := aTsb[4,nRow,nColHead,6]
         Do Case
            Case (rType=='@'.or.rType=='D').and.Empty(uData)
               uData := ''
            Case ValType( uData )=="D"
               uData := hb_dtoc( uData , "dd.mm.yyyy")
            Case rType == 'L'
               rType :='C'
            Case rPicture != Nil .and. uData != Nil .and. rType !='N'
              uData := Transform( uData, rPicture )
         Endcase
         If ValType( uData ) == "C" .and. At( CRLF, uData ) > 0
            uData := StrTran( uData, CRLF, "&&" )
            If AScan( aRepl, nColHead ) == 0
               AAdd( aRepl, nColHead )
            EndIf
         EndIf
         uData := If( uData == NIL, "", Transform( uData, rPicture ) )
         uData  :=  If( ValType( uData )=="D", DtoC( uData ), If( ValType( uData )=="N", Str( uData ) , ;
                    If( ValType( uData )=="L", If( uData ,".T." ,".F." ), cValToChar( uData ) ) ) )
         // Разделитель между ячейками кроме первой (lSeparator  == .f.)
         tSeparator := if(lSeparator,WordSeparatorBox,( lSeparator :=.t.,""))

         cText += tSeparator + Trim( uData )

         If hProgress != Nil
            If nCount % nEvery == 0
               SendMessage(hProgress, PBM_SETPOS, nCount, 0)
            EndIf
            nCount ++
         EndIf
      Next

   Next

   // Футер - итоги
   If Len(aTsb[5])>0
      lTsbFooting :=.t.
      For nCol := 1 To nColDbf
         tSeparator := if(lSeparator,WordSeparatorBox,( lSeparator :=.t.,""))
         cText += tSeparator + ""
      Next
   EndIf

   oPar := oActive:Paragraphs:Add()
   oPar:Range:Text:= cText
   // aFont := aDocParam[4]
   // oPar:Range:Font:Name := aFont[ 1 ]
   // oPar:Range:Font:Size := aFont[ 2 ]
   // if len(aFont)>2
   //   oRange:Font:Bold := aFont[ 3 ]
   // Endif

   // кол-во строк в таблице + шапка + подвал таблицы
   nRowDbf := nTotal  + nLenHead+iif(Len(aTsb[5])>0,1,0)
   // Создаем и заполняем таблицу
   oPar:Range:ConvertToTable(WordSeparatorBox,nRowDbf,nColDbf)
   oTbl := oActive:Tables[1]
   // меняем ширину колонок пропорционально ширине Tsbrowse
   oColumn := oTbl:Columns
   oRange:=oActive:Range(oTbl:Cell(1,1):Range:Start, oTbl:Cell(nRowDbf, nColDbf):Range:End)
   aFont := aDocParam[4]
   oRange:Font:Name := aFont[ 1 ]
   oRange:Font:Size := aFont[ 2 ]
   if len(aFont)>2
     oRange:Font:Bold := aFont[ 3 ]
   Endif

   FOR nColHead:= 1 to nColDbf
      nPxToPnt := oWord:PixelsToPoints(aDocParam[6, nColHead ], 0 )
      if nPxToPnt<MinPxToPnt                 // минимальная ширина столбца
         nAddPxToPnt += MinPxToPnt-nPxToPnt  // нужно добавить
         nPxToPnt:=MinPxToPnt
      endif
      aadd(arrnPxToPnt,nPxToPnt)             // скорректированные размеры в point
   NEXT
   FOR nColHead:= 1 to nColDbf
      rColWidth := (nWidth - 2 * nLeftRightMargin) / (nWidthWordTsb+nAddPxToPnt) * arrnPxToPnt[nColHead]
      AADD(aColWidth,{nColHead, rColWidth, rColWidth - oColumn[nColHead]:Width})
   NEXT
   aColWidth:=ASORT(aColWidth,,,{|x,y|x[3]<y[3]})
   FOR EACH nCol IN aColWidth
      oColumn[ ncol[1] ]:Width := nCol[2]
   Next

   nLine :=1
   // Заполням суперхидер в уже созданную в таблицу и мержим
   If Len(aTsb[1])>0
      For nCol := 1 To nColDbf
         cText += "" + WordSeparatorBox
      Next
      nmerge := 1
      //---------------
      nCol :=0
      nColSh2 :=0
      FOR EACH aCol IN aTsb[1]
         nCol++
         nColSh1 := if(aCol[5]>0, aCol[5], nColSh2+1)
         // Если  с -1 не последняя и следующая нормальная, то берем до следующей
         if aCol[6]>0.and.nCol<Len(aTsb[1])
            if aTsb[1,nCol+1,5]>0
                nColSh2 := aTsb[1,nCol,5]-1
            endif
         endif
          nColSh2 := if(aCol[6]>0, aCol[6], if(nCol==Len(aTsb[1]), nColDbf, nColSh1))

         uData := if(Empty(aCol[4]),' ',aCol[4])
         oRange:=oActive:Range(oTbl:Cell( nLine, nmerge):Range:Start, oTbl:Cell( nLine, nmerge+nColSh2 - nColSh1):Range:End)
         oRange:Cells:Merge()
         oTbl:Cell(nLine, nmerge ):Range:Text := uData
         nmerge++
      NEXT
      ++nLine
   Endif

   If Len(aTsb[2])>0
      FOR nColHead:= 1 to nColDbf

         uData := aTsb[2,nColHead,4]

         If ValType( uData ) != "C"
            Loop
         EndIf

         uData := StrTran( uData, CRLF, Chr( 10 ) )
         nColHead ++
         oTbl:Cell( nLine, nColHead ):Range:ParagraphFormat:Alignment:= wdAlignParagraphCenter
         oTbl:Cell( nLine, nColHead ):Range:Text := uData

         If hProgress != Nil

            If nCount % nEvery == 0
               SendMessage(hProgress, PBM_SETPOS,nCount,0)
            EndIf

            nCount ++
         EndIf
      Next
      ++nLine
   Endif

   If Len(aTsb[3])>0
      FOR nColHead:= 1 to nColDbf

         uData := aTsb[3,nColHead,4]

         If ValType( uData ) != "C"
            Loop
         EndIf

         uData := StrTran( uData, CRLF, Chr( 10 ) )
         oTbl:Cell( nLine, nColHead ):Range:ParagraphFormat:Alignment:= wdAlignParagraphCenter
         oTbl:Cell( nLine, nColHead ):Range:Text := uData

         If hProgress != Nil

            If nCount % nEvery == 0
               SendMessage(hProgress, PBM_SETPOS,nCount,0)
            EndIf

            nCount ++
         EndIf
      Next
   Endif
   nLine := nRowDbf+1
   If Len(aTsb[5])>0
      FOR nColHead:= 1 to nColDbf

         uData := aTsb[5,nColHead,4]
         uData := cValTochar( uData )
         uData := StrTran( uData, CRLF, Chr( 10 ) )
         oTbl:Cell( nLine, nColHead):Range:Text := uData
      Next
   EndIf
   oTbl:Borders:OutsideLineStyle := wdLineStyleSingle
   oTbl:Borders:OutsideLineWidth := wdLineWidth100pt
   oTbl:Borders:InsideLineStyle  := wdLineStyleSingle

   // обработка строк содержащие знак "&&" - многострочные строки таблицы
   If ! Empty( aRepl )
      For nCol := 1 To Len( aRepl )
        // колонка aRepl[nCol] таблицы, в которой нужно сделать замену
        // Начало колонки nLenHead+1
        // Конец колонки nRowDbf, или (nRowDbf -1), если есть Footing,
        // то есть_ nRowDbf-iif(lTsbFooting,1,0)
        // Получаем колонку с координатами( nLenHead+1, aRepl[nCol])- (nRowDbf-iif(lTsbFooting,1,0), aRepl[nCol])
        oRange:=oActive:Range( ;
             oTbl:Cell( nLenHead+1, aRepl[nCol]):Range:Start,;
             oTbl:Cell( nRowDbf-iif(lTsbFooting,1,0), aRepl[nCol]):Range:End;
                            )
        oRange:Select()
        findObject := oRange:Find
        MSWordFind_Replace(findObject, "&&", "^l")
      Next
   EndIf

   If ! Empty( aDocFoot )
      For nRow := 1 TO Len(aDocFoot)
            oPar := oActive:Paragraphs:Add()
            oText := oPar:Range
            if Len(aDocFoot[nRow]) >0
               cTitle := aDocFoot[nRow,3]
               cTitle := AllTrim( cTitle )
            Else
               cTitle := ""
            Endif
            oPar := oActive:Paragraphs:Add()
            oText := oPar:Range
            oText:Text := cTitle + CRLF

            If Len(aDocFoot[nRow])>5.and.aDocFoot[nRow,6] != Nil
              oText:ParagraphFormat:Alignment := TbsDocAlign( aDocFoot[nRow,6] )
            Else
              oText:ParagraphFormat:Alignment := TbsDocAlign( DT_CENTER )
            Endif

            If Len(aDocFoot[nRow])>3.and.aDocFoot[nRow,4] != Nil
              aFont := aDocFoot[nRow,4]
              oText:Font:Name = aFont[1]
              oText:Font:Size = aFont[2]
              oText:Font:Bold = aFont[3]
            Endif

            If Len(aDocFoot[nRow])>4.and.aDocFoot[nRow,5] != Nil
              aClr  := aDocFoot[nRow,5]
              oText:Font:Color = RGB(aClr[1,1],aClr[1,2],aClr[1,3])
              oText:Font:Shading:BackgroundPatternColor := RGB(aClr[2,1],aClr[2,2],aClr[2,3])
            EndIf
       Next
   EndIf


   If hProgress != Nil
      SendMessage( hProgress, PBM_SETPOS, nCount, 0 )
   EndIf

   // Доп.надпись под таблицей

   cVal := CRLF + "End table ! - Version (" + oWord:Version + ") " + WordVersion( VAL( oWord:Version ) )
   cVal += "  Path - " + WordPath() + CRLF

   aFColor := RED
   oPar := oActive:Paragraphs:Add()
   oPar:Range:Font:Color := RGB(aFColor[1],aFColor[2],aFColor[3])
   oPar:Range:Font:Name  := "Times New Roman"
   oPar:Range:Font:Size  := 16
   oPar:Range:Font:Bold  := .T.
   oPar:Range:Text:= cVal


   If bExtern != Nil
      Eval( bExtern, aTsb, oTbl, oActive)
   EndIf

   If ! Empty( cFile ) .and. lSave
      oActive:SaveAs( cFile, wdFormatDocument )
   EndIf

   If hProgress != Nil
      SendMessage( hProgress, PBM_SETPOS, 0, 0 )
   EndIf

   WaitThreadCloseIcon()  // kill the window waiting
   CursorArrow()

   If lActivate
      oWord:Visible := .T.               // показать Word на экране
      SetWordWindowToForeground(oWord)   // окно Word на передний план
   Else
      oWord:Quit()                       // закрыть Word
   EndIf

RETURN NIL

//////////////////////////////////////////////////////////////////////
PROCEDURE MSWordFind4Replace(oFind, cFind, cReplace)

   With object oFind
   :ClearFormatting()

   :Execute(cFind,0,0,0,0,0,1,1,0,cReplace,wdReplaceAll)

   :ClearFormatting()
   END

RETURN

//////////////////////////////////////////////////////////////////////
STATIC FUNCTION MSWordFind_Replace(oFind, cFind, cReplace)

   With object oFind
   :ClearFormatting()

   :Execute(cFind,0,0,0,0,0,1,1,0,cReplace,wdReplaceAll)

   :ClearFormatting()
   END

RETURN NIL

//////////////////////////////////////////////////////////////////////
// окно Word на передний план
STATIC FUNCTION SetWordWindowToForeground(oWord)
   LOCAL hWnd, nVer, cCaption, cTitle

   //  поиск ХЕНДЛА открытого окна документа
   hWnd := 0
   nVer := VAL( oWord:Version ) // Версия Word
   IF nVer > 14  // Word 2010
      hWnd := oWord:ActiveDocument:ActiveWindow:Hwnd
   ELSE
      //hWnd:=oWord:hwnd - так делать нельзя !
      cCaption := oWord:Windows[1]:Caption
      cTitle := cCaption + " - MICROSOFT WORD"
      hWnd := FindWindowEx(,,, cTitle )
      IF hWnd == 0
         cTitle := cCaption + " [Режим ограниченной функциональности] - MICROSOFT WORD"
         hWnd := FindWindowEx(,,, cTitle )
      ENDIF
   ENDIF

   IF hWnd > 0
      ShowWindow( hWnd, 6 )      // MINIMIZE windows
      ShowWindow( hWnd, 3 )      // MAXIMIZE windows
      BringWindowToTop( hWnd )   // A window on the foreground
   ENDIF

   RETURN NIL

* =======================================================================================
// Отбивка строки из TSB в DOC
STATIC FUNCTION TbsDocAlign(nAlign)
   LOCAL nRet := 0

   IF nAlign == DT_LEFT
      nRet := wdAlignParagraphLeft
   ELSEIF nAlign == DT_RIGHT
      nRet := wdAlignParagraphRight
   ELSE
      nRet := wdAlignParagraphCenter  // DT_CENTER
   ENDIF

   RETURN nRet

* =======================================================================================
FUNCTION WordOle7Extern( hProgress, aTsb, oTbl, oActive)

   LOCAL nLine, nCol, nRow, nBColor, nFColor
   LOCAL nCount, nTotal, nEvery, aFont, aCol, nmerge, lendTabl
   LOCAL oldnFColor, aRCnFColor[4], oldaFont[3]
   LOCAL oldnBColor, aRCnBColor[4], aRCaFont[4]
   LOCAL nColDbf, nColHead, nMaxCol
   Local lTsbFontTable  := .t.              // менять фонты таблицы
   Local lTsbFontHeader := .f.              // менять фонты заголовка и подвала

   nTotal := Len(aTsb[4]) //количество строк таблицы
   nColDbf := Len(aTsb[4,1])//количество колонок

   nLine := 1
   // выводим цвета фона и текста суперхидера таблицы
   If Len( aTsb[1] )>0

      nmerge := 1

      FOR EACH aCol IN aTsb[1]

         nFColor := myColorFirst  (aCol[1])
         nBColor := myColorFirst  (aCol[2])

         oTbl:Cell(nLine, nmerge):Range:Font:Color    := nFColor  // Цвет шрифта шапки
         oTbl:Cell(nLine, nmerge):Range:Shading:BackgroundPatternColor := nBColor

         If lTsbFontHeader

           aFont := GetFontParam( aCol[3])
           oTbl:Cell(nLine, nmerge):Range:Font:Name := aFont[ 1 ]
           oTbl:Cell(nLine, nmerge):Range:Font:Size := aFont[ 2 ]
           oTbl:Cell(nLine, nmerge):Range:Font:Bold := aFont[ 3 ]
         Endif
         nmerge++

      Next
      nLine++
   EndIf

   // выводим цвета фона и текста шапки таблицы
   If Len( aTsb[2] )>0

      nCol :=0
      FOR nColHead:= 1 to Len(aTsb[2])
          nFColor := myColorFirst( aTsb[2,nColHead,1] )
          nBColor := myColorFirst( aTsb[2,nColHead,2] )

          oTbl:Cell(nLine, nColHead):Range:Font:Color    := nFColor  // Цвет шрифта шапки
          oTbl:Cell(nLine, nColHead):Range:Shading:BackgroundPatternColor := nBColor // Цвет фона шапки
          If lTsbFontHeader
            aFont := GetFontParam( aTsb[2,nColHead,3])
            oTbl:Cell(nLine, nColHead):Range:Font:Name := aFont[ 1 ]
            oTbl:Cell(nLine, nColHead):Range:Font:Size := aFont[ 2 ]
            oTbl:Cell(nLine, nColHead):Range:Font:Bold := aFont[ 3 ]
          Endif
      Next
      nLine++
   Endif
   // выводим цвета фона и текста шапки нумератора
   If Len( aTsb[3] )>0

      nCol :=0
      FOR nColHead:= 1 to Len(aTsb[3])
          nFColor := myColorFirst( aTsb[3,nColHead,1] )
          nBColor := myColorFirst( aTsb[3,nColHead,2] )

          oTbl:Cell(nLine, nColHead):Range:Font:Color    := nFColor  // Цвет шрифта шапки
          oTbl:Cell(nLine, nColHead):Range:Shading:BackgroundPatternColor := nBColor // Цвет фона шапки
          If lTsbFontHeader
            aFont := GetFontParam( aTsb[3,nColHead,3])
            oTbl:Cell(nLine, nColHead):Range:Font:Name := aFont[ 1 ]
            oTbl:Cell(nLine, nColHead):Range:Font:Size := aFont[ 2 ]
            oTbl:Cell(nLine, nColHead):Range:Font:Bold := aFont[ 3 ]
          Endif
      Next
      nLine++
   Endif

   If hProgress != Nil
      nTotal := nTotal
      SetProgressBarRange ( hProgress , 1 , nTotal )
      SendMessage(hProgress, PBM_SETPOS, 0, 0)
      nEvery := Max( 1, Int( nTotal * 0.05 ) ) // refresh hProgress every 5 %
   EndIf

   nCount := 0
   oldnFColor := Nil
   oldnBColor := Nil
   aFill(oldaFont,Nil)
   lendTabl := .f.

   // выводим цвета фона и текста ячеек всех колонок таблицы

   nMaxCol := nColDbf
   FOR nRow:= 1 to nTotal //Len(aTsb[4])
      FOR nColHead:= 1 to nColDbf //Len(aTsb[4,1])

          If nRow == nTotal .and. nColHead == nColDbf
             lEndTabl :=.t. //флаг последней ячейки таблицы
          Endif

          nFColor := myColorFirst(aTsb[4,nRow,nColHead,1])

          if (!oldnFColor == nFColor)
             //при изменении цвета либо по концу таблицы раскрашиваем область
             if !oldnFColor==Nil
                ChangeRangeFontColor( oTbl,oActive,oldnFColor, aRCnFColor, nMaxCol )
             Endif
             oldnFColor:=nFColor
             aRCnFColor[1] :=  nLine; aRCnFColor[2] :=  nColHead
          Endif
          aRCnFColor[3] :=  nLine; aRCnFColor[4] :=  nColHead
          If lendTabl
                ChangeRangeFontColor( oTbl, oActive,oldnFColor, aRCnFColor, nMaxCol )
          Endif


          nBColor := myColorFirst(aTsb[4,nRow,nColHead,2])
          // Фон шрифта
          if (!oldnBColor == nBColor)
             // при изменении цвета либо по концу таблицы раскрашиваем область
             if !oldnBColor==Nil
                ChangeRangeBackgroundPatternColor( oTbl,oActive,oldnBColor, aRCnBColor, nMaxCol )
             Endif
             oldnBColor:=nBColor
             aRCnBColor[1] :=  nLine; aRCnBColor[2] :=  nColHead
          Endif
          aRCnBColor[3] :=  nLine; aRCnBColor[4] :=  nColHead
          If lEndTabl
                ChangeRangeBackgroundPatternColor( oTbl,oActive,oldnBColor, aRCnBColor, nMaxCol )
          Endif

           // Фонт шрифта
          If lTsbFontTable
            aFont := GetFontParam(aTsb[4,nRow,nColHead,3])
            if (!(oldaFont[1] == aFont[1].and.oldaFont[2] == aFont[2].and.oldaFont[3] == aFont[3])).or.lEndTabl
               // при изменении цвета либо по концу меняем шрифты области
               if !oldaFont[1] == Nil
                  ChangeRangeFont( oTbl,oActive,oldaFont, aRCaFont, nMaxCol )
               Endif
               oldaFont[1] := aFont[1]; oldaFont[2] := aFont[2]; oldaFont[3] := aFont[3]
               aRCaFont[1] :=  nLine; aRCaFont[2] :=  nColHead
             Endif
             aRCaFont[3] :=  nLine; aRCaFont[4] :=  nColHead
             if lEndTabl
              if !oldaFont[1] == Nil
                  ChangeRangeFont( oTbl,oActive,oldaFont, aRCaFont, nMaxCol )
               Endif
            Endif
          Endif
      Next

      If hProgress != Nil

         If nCount % nEvery == 0
            SendMessage(hProgress, PBM_SETPOS,nCount,0)
         EndIf

         nCount ++
      EndIf

      ++nLine
   Next

   // выводим цвета фона и текста подвала таблицы
   If Len(aTsb[5])>0
      FOR nColHead:= 1 to Len(aTsb[5])
         nFColor := myColorFirst(aTsb[5,nColHead,1])
         nBColor := myColorFirst(aTsb[5,nColHead,2])

          oTbl:Cell(nLine, nColHead):Range:Font:Color    := nFColor  // Цвет шрифта шапки
          oTbl:Cell(nLine, nColHead):Range:Shading:BackgroundPatternColor := nBColor // Цвет фона шапки

          If lTsbFontTable
             aFont := GetFontParam( aTsb[5,nColHead,3])

            oTbl:Cell(nLine, nColHead):Range:Font:Name := aFont[ 1 ]
            oTbl:Cell(nLine, nColHead):Range:Font:Size := aFont[ 2 ]
            oTbl:Cell(nLine, nColHead):Range:Font:Bold := aFont[ 3 ]
          Endif
      Next

      nLine++
   Endif

   RETURN Nil

* =======================================================================================
STATIC FUNCTION myColorFirst(nColor)
   If Valtype( nColor ) == "A"
      nColor := nColor[1]
   EndIf
Return nColor

* =======================================================================================
* Функция проверки версии WinWord
STATIC FUNCTION WordVersion(nVer)
   LOCAL aDim[20]
   DEFAULT nVer := 1

   AFILL(aDim,"???")
   aDim[01] := "No Word on this computer!"
   aDim[09] := "Word 2000"
   aDim[10] := "Word XP"
   aDim[11] := "Word 2003"
   aDim[12] := "Word 2007"
   aDim[14] := "Word 2010"
   aDim[15] := "Word 2013"
   aDim[16] := "Word 2016"
   aDim[17] := "Word 2019"
   aDim[18] := "Word New!"

   RETURN aDim[nVer]

* =======================================================================================
STATIC FUNCTION WordPath()
 LOCAL cPath := NIL
 cPath := win_regRead( "HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\App Paths\Winword.exe\Path" )
 IF cPath == NIL
    cPath := ""
 ENDIF
Return cPath

/////////////////////////////////////////////////////////////////
STATIC Function PixelToPointX(iPixels)
    Local lngDPI, rPixelToPoint
    lngDPI = GetDPIX()
    rPixelToPoint = (iPixels / lngDPI) * 72
Return rPixelToPoint

/////////////////////////////////////////////////////////////////
STATIC Function PixelToPointY(iPixels)
    Local lngDPI, rPixelToPoint
    lngDPI = GetDPIY()
    rPixelToPoint = (iPixels / lngDPI) * 72
Return rPixelToPoint

* =======================================================================================
STATIC FUNCTION ChangeRangeBackgroundPatternColor( oTbl,oActive,oldnBColor, aRCnBColor, nMaxCol )
LOCAL oRange, nDif := aRCnBColor[3] - aRCnBColor[1]

   Do case
      case nDif == 0
         oRange:=oActive:Range(oTbl:Cell( aRCnBColor[1], aRCnBColor[2]):Range:Start, oTbl:Cell( aRCnBColor[3], aRCnBColor[4]):Range:End)
         oRange:Shading:BackgroundPatternColor :=oldnBColor  // Фон шрифта

      case ndif =1
         oRange:=oActive:Range(oTbl:Cell( aRCnBColor[1], aRCnBColor[2]):Range:Start, oTbl:Cell( aRCnBColor[1], nMaxCol):Range:End)
         oRange:Shading:BackgroundPatternColor :=oldnBColor  // Фон шрифта

         oRange:=oActive:Range(oTbl:Cell( aRCnBColor[3], 1):Range:Start, oTbl:Cell( aRCnBColor[3], aRCnBColor[4]):Range:End)
         oRange:Shading:BackgroundPatternColor :=oldnBColor  // Фон шрифта
      Otherwise
         oRange:=oActive:Range(oTbl:Cell( aRCnBColor[1], aRCnBColor[2]):Range:Start, oTbl:Cell( aRCnBColor[1], nMaxCol):Range:End)
         oRange:Shading:BackgroundPatternColor :=oldnBColor  // Фон шрифта

         oRange:=oActive:Range(oTbl:Cell( aRCnBColor[1]+1, 1):Range:Start, oTbl:Cell( aRCnBColor[3]-1, nMaxCol):Range:End)
         oRange:Shading:BackgroundPatternColor :=oldnBColor  // Фон шрифта

         oRange:=oActive:Range(oTbl:Cell( aRCnBColor[3], 1):Range:Start, oTbl:Cell( aRCnBColor[3], aRCnBColor[4]):Range:End)
         oRange:Shading:BackgroundPatternColor :=oldnBColor  // Фон шрифта
   Endcase

RETURN Nil

* =======================================================================================
STATIC FUNCTION ChangeRangeFontColor( oTbl,oActive,oldnFColor, aRCnFColor, nMaxCol )
LOCAL oRange, nDif := aRCnFColor[3] - aRCnFColor[1]

   Do case
      case nDif == 0
           oRange := oActive:Range(oTbl:Cell( aRCnFColor[1], aRCnFColor[2]):Range:Start, oTbl:Cell( aRCnFColor[3], aRCnFColor[4]):Range:End)
           oRange:Font:Color := oldnFColor  // Цвет шрифта
      case ndif =1
           oRange := oActive:Range(oTbl:Cell( aRCnFColor[1], aRCnFColor[2]):Range:Start, oTbl:Cell( aRCnFColor[3], nMaxCol):Range:End)
           oRange:Font:Color := oldnFColor  // Цвет шрифта

           oRange := oActive:Range(oTbl:Cell( aRCnFColor[3], 1):Range:Start, oTbl:Cell( aRCnFColor[3], aRCnFColor[4]):Range:End)
           oRange:Font:Color := oldnFColor  // Цвет шрифта
      Otherwise
           oRange := oActive:Range(oTbl:Cell( aRCnFColor[1], aRCnFColor[2]):Range:Start, oTbl:Cell( aRCnFColor[1], nMaxCol):Range:End)
           oRange:Font:Color := oldnFColor  // Цвет шрифта

           oRange := oActive:Range(oTbl:Cell( aRCnFColor[1]+1, 1):Range:Start, oTbl:Cell( aRCnFColor[3]-1, nMaxCol):Range:End)
           oRange:Font:Color := oldnFColor  // Цвет шрифта

           oRange := oActive:Range(oTbl:Cell( aRCnFColor[3], 1):Range:Start, oTbl:Cell( aRCnFColor[3], aRCnFColor[4]):Range:End)
           oRange:Font:Color := oldnFColor  // Цвет шрифта
   Endcase

RETURN Nil

* =======================================================================================
STATIC FUNCTION ChangeRangeFont( oTbl,oActive,oldaFont, aRCaFont, nMaxCol )
LOCAL oRange, nDif := aRCaFont[3] - aRCaFont[1]
   Do case
      case nDif == 0
         oRange:=oActive:Range(oTbl:Cell( aRCaFont[1], aRCaFont[2]):Range:Start, oTbl:Cell( aRCaFont[3], aRCaFont[4]):Range:End)
         oRange:Font:Name := oldaFont[ 1 ]
         oRange:Font:Size := oldaFont[ 2 ]
         oRange:Font:Bold := oldaFont[ 3 ]
      case ndif =1
         oRange:=oActive:Range(oTbl:Cell( aRCaFont[1], aRCaFont[2]):Range:Start, oTbl:Cell( aRCaFont[3], nMaxCol):Range:End)
         oRange:Font:Name := oldaFont[ 1 ]
         oRange:Font:Size := oldaFont[ 2 ]
         oRange:Font:Bold := oldaFont[ 3 ]

         oRange:=oActive:Range(oTbl:Cell( aRCaFont[3], 1):Range:Start, oTbl:Cell( aRCaFont[3], aRCaFont[4]):Range:End)
         oRange:Font:Name := oldaFont[ 1 ]
         oRange:Font:Size := oldaFont[ 2 ]
         oRange:Font:Bold := oldaFont[ 3 ]
      Otherwise
         oRange:=oActive:Range(oTbl:Cell( aRCaFont[1], aRCaFont[2]):Range:Start, oTbl:Cell( aRCaFont[3], nMaxCol):Range:End)
         oRange:Font:Name := oldaFont[ 1 ]
         oRange:Font:Size := oldaFont[ 2 ]
         oRange:Font:Bold := oldaFont[ 3 ]

         oRange:=oActive:Range(oTbl:Cell( aRCaFont[1]+1, 1):Range:Start, oTbl:Cell( aRCaFont[3]-1, nMaxCol):Range:End)
         oRange:Font:Name := oldaFont[ 1 ]
         oRange:Font:Size := oldaFont[ 2 ]
         oRange:Font:Bold := oldaFont[ 3 ]

         oRange:=oActive:Range(oTbl:Cell( aRCaFont[3], 1):Range:Start, oTbl:Cell( aRCaFont[3], aRCaFont[4]):Range:End)
         oRange:Font:Name := oldaFont[ 1 ]
         oRange:Font:Size := oldaFont[ 2 ]
         oRange:Font:Bold := oldaFont[ 3 ]
   Endcase

RETURN Nil
