/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2018 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 * Copyright 2018 Sidorov Aleksandr <aksidorov@mail.ru>  Dmitrov, Moscow region
 *
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

#define BUFFER_CLIPBOARD    1000000  // при появлении переносов строк в таблице
                                     // можно увеличить размер 
#define PBM_SETPOS          1026

* =====================================================================================
// Внимание ! Выгружать больше 32767 строк в WinWord НЕЛЬЗЯ ! Ограничение WinWord.
// Attention ! Upload more than 32767 rows in WinWord is NOT possible ! WinWord Restriction.
FUNCTION Brw2Doc(oBrw, cFile, lActivate, hProgress, aTitle, hFont, lSave, bExtern )
   Local oWord, oText, oRange, oTbl, oActive, oMarks, cText, aRepl
   Local cMsg, nRowDbf, nColDbf, cVal, cTitle, nStart, aFont
   Local nTotal, nLine := 1, nCount := 0, nLenHead := 0
   Local nMerge := 1, flag_new_OutWrd := .f.
   Local nRow, nCol, uData, nEvery, nColHead, nVar
   Local nRecNo := ( oBrw:cAlias )->( RecNo() ), nAt := oBrw:nAt
   Local nOldRow := oBrw:nLogicPos(), nOldCol := oBrw:nCell
   Local findObject, aClr, nWidthTsb, nLeftRightMargin, nPxLRM
   Local oColumn, nWidth, nWidthWordTsb, nPxToPnt
   Local lTsbSuperHd, lTsbHeading, lTsbFooting
   Local lBrSelector := oBrw:lSelector
   Local aColWidth := {}, rColWidth
  
   Default cFile := "", lActivate := .T., hProgress := nil
   Default aTitle := {"",0}, hFont := 10, lSave := .F. , bExtern := nil

   CursorWait()
   WaitWindow( 'Loading the report in WORD ...', .T. )   // open the wait window

   // Используем Ole из HBWIN.lib
   Try
      oWord := CreateObject( "Word.Application" )
   Catch
      cMsg := REPLICATE( "-._.", 16 ) + ";;"
      IF Hb_LangSelect() == "ru.RU1251"
         cMsg += SPACE( 5 ) + "На этом компьютере MS Word не установлен !;;"
         cMsg += SPACE( 5 ) + " Код ошибки [" + win_oleErrorText() + "];;"
         cVal := "Ошибка!"
      ELSE
         cMsg += SPACE( 5 ) + "On this computer MS Word is not installed !;;"
         cMsg += SPACE( 5 ) + " Error code [" + win_oleErrorText() + "];;"
         cMsg += SPACE( 5 ) + " Error code [" + Ole2TxtError() + "];;"
         cVal := "Error!"
      ENDIF

      WaitWindow()    // close the wait window
      CursorArrow()

      cMsg += REPLICATE( "-._.", 16 ) + ";;"
      cMsg := AtRepl( ";", cMsg, CRLF )
      MsgStop( cMsg , cVal )
      Return .F.
   End Try

   oActive:= oWord:Documents:Add()
   oMarks := oActive:BookMarks
   oText  := oWord:Selection()

   oWord:Visible := .F.                         // если открыты другие документы в Word-е
   oWord:DisplayAlerts := .F.                   // убрать предупреждения Word
   oWord:Options:CheckSpellingAsYouType := .F.  // Отключить автопроверку текста документа 

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

   If oBrw:lSelector // если есть селектор в таблице
      oBrw:aClipBoard := { ColClone( oBrw:aColumns[ 1 ], oBrw ), 1, "" }
      oBrw:DelColumn( 1 )
      oBrw:lSelector := .F.
   EndIf

   If hProgress != Nil
      SendMessage(hProgress, PBM_SETPOS, nCount, 0)
   EndIf

   oBrw:lNoPaint := .F.

   If hProgress != Nil
      nTotal := oBrw:nLen  
      SetProgressBarRange ( hProgress , 1 , nTotal )
      SendMessage(hProgress, PBM_SETPOS, 0, 0)
      nEvery := Max( 1, Int( nTotal * .02 ) ) // refresh hProgress every 2 %
   EndIf

   // Нужно определиться с форматом листа для DOC.
   // Единица измерения для Word равна пунктам (points)
   // так как размер таблицы представлены в пикселях, то будем считать далее в пикселах 
   nWidthTsb := oBrw:GetAllColsWidth()    // ширина всех колонок таблицы (пикселы)
   ?  "  ------ nWidthTsb=",nWidthTsb,"(px, пикселы)"

   // Формат бумаги печати
   // Источник: http://biznessystem.ru/kakoj-razmer-v-pikselyah-imeet-list-formata-a4/
   // А4 = 2480 x 3508 px при dpi=300  // А4 = 1240 x 1754 px при dpi=150
   // А3 = 3508 x 4961 px при dpi=300  // А3 = 1754 x 2480 px при dpi=150

   // Размер печати таблицы на листе = отступ слева + отступ справа 
   nLeftRightMargin := 29     // пунктам (points) (примерно 1 см) - отступ слева
   nPxLRM := 38.8 * 2         // пикселей - отступ слева + отступ справа
   
   // ------- Установка параметров страницы (листа) ------- 
   If nWidthTsb + nPxLRM >= 1754
      ? "  ==> The size of the paper to print the table is larger than A4"
      // Word имеет ограничение в установке размеров - 55,87 см по любой из сторон листа. 
      // 55.87 сантиметров равно 1 583.717 пунктов
      // Высоту листа возьмем как у A4 (210х297 мм) == 297
      // 297 миллиметров равно 841.889862 пункта 
      oActive:PageSetup:PageWidth = 1583
      oActive:PageSetup:PageHeight = 841
      // книжная ориентация
      oActive:PageSetup:Orientation := wdOrientPortrait
   Else
      ? "  ==> The size of the paper to print the table is A4 "
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

   // ------- Отключить автопроверку текста документа -------
   oWord:Options:CheckSpellingAsYouType := .F.

   // -------- Заголовок таблицы ------------------
   cTitle:= aTitle[1]
   cTitle := AllTrim( cTitle )
   if !Empty(cTitle) 
     oText:HomeKey(wdStory)         // в начало текста
     aFont := GetFontParam( aTitle[2] )
     oText := oWord:Selection()
     oText:Text := cTitle + CRLF
     oText:InsertAfter(CRLF) 
     oText:Font:Name = aFont[1]
     oText:Font:Size = aFont[2]
     oText:Font:Bold = aFont[3]
     aClr := BLACK
     oText:Font:Color = RGB(aClr[1],aClr[2],aClr[3])
     oText:ParagraphFormat:Alignment = wdAlignParagraphRight
     oText:ParagraphFormat:Alignment = wdAlignParagraphCenter
     oText:HomeKey(wdStory)        // в начало текста
   endif

   if ! Empty( oBrw:aSuperHead )
     nLenHead++
   endif

   nLenHead++ // шапка таблицы
   if AScan( oBrw:aColumns, { |o| o:cFooting != Nil  } ) > 0
     nLenHead++
   endif
   nRowDbf := oBrw:nLen - oBrw:nAt + nLenHead + 1  // кол-во строк в таблице + шапка + подвал таблицы
   nColDbf := Len( oBrw:aColumns )                 // кол-во столбцов в таблице

   // ------- создание таблицы ---------------
   oRange = oActive:Range(len(cTitle)+2)

   ////////// таблица с автоподстройкой по ширине странице (вариант 1) //////////////
   //oTbl:= oActive:Tables:Add(oRange,nRowDbf,nColDbf,wdWord9TableBehavior,wdAutoFitContent)

   ///////// таблица без автоподстройки по ширине странице (вариант 2)  ////////////
   oTbl:= oActive:Tables:Add(oRange,nRowDbf,nColDbf,wdWord8TableBehavior,wdAutoFitFixed)
   oTbl:Borders:OutsideLineStyle := wdLineStyleSingle 
   oTbl:Borders:OutsideLineWidth := wdLineWidth100pt 
   oTbl:Borders:InsideLineStyle := wdLineStyleSingle 

   nWidth := oActive:PageSetup:PageWidth  
   nWidthWordTsb := oWord:PixelsToPoints( nWidthTsb, 0 )

   // меняем ширину колонок пропорционально ширине Tsbrowse 
   oColumn := oTbl:Columns 
   For nCol := 1 To Len( oBrw:aColumns )
      nPxToPnt := oWord:PixelsToPoints( oBrw:aColumns[ nCol ]:nWidth, 0 )
      rColWidth := (nWidth - 2 * nLeftRightMargin) / nWidthWordTsb * nPxToPnt
      AADD(aColWidth,{nCol, rColWidth, rColWidth - oColumn[nCol]:Width})
   Next
   aColWidth:=ASORT(aColWidth,,,{|x,y|x[3]<y[3]})
   FOR EACH nCol IN aColWidth
      oColumn[ ncol[1] ]:Width := nCol[2]
   Next
   ///////// таблица без автоподстройки по ширине странице (вариант 2)  ////////////

   ( oBrw:cAlias )->( Eval( oBrw:bGoTop ) )  // на первую запись в таблице

   cText := ""

   oText:ParagraphFormat:Alignment = wdAlignParagraphCenter
   aRepl := {}

   For nRow := 1 To oBrw:nLen

      If nRow == 1

         nStart := nLine

         If lTsbSuperHd

            For nCol := 1 To Len( oBrw:aSuperHead )
               nVar := If( oBrw:lSelector, 1, 0 )
               uData := If( ValType( oBrw:aSuperhead[ nCol, 3 ] ) == "B", Eval( oBrw:aSuperhead[ nCol, 3 ] ), ;
                                     oBrw:aSuperhead[ nCol, 3 ] )
               oRange:=oActive:Range(oTbl:Cell( nLine, nmerge):Range:Start, oTbl:Cell( nLine, nmerge+oBrw:aSuperHead[ nCol, 2 ] - oBrw:aSuperHead[ nCol, 1 ]):Range:End)
               oRange:Cells:Merge()
               oTbl:Cell(nLine, nmerge):Range:ParagraphFormat:Alignment:= wdAlignParagraphCenter
               oTbl:Cell(nLine, nmerge ):Range:Text := uData
               nmerge++
            Next

            nStart := nLine ++
         EndIf

         nColHead := 0

         If lTsbHeading
            For nCol := 1 To Len( oBrw:aColumns )

               uData := If( ValType( oBrw:aColumns[ nCol ]:cHeading ) == "B", Eval( oBrw:aColumns[ nCol ]:cHeading ), ;
                                  oBrw:aColumns[ nCol ]:cHeading )

               If ValType( uData ) != "C"
                  Loop
               EndIf

               uData := StrTran( uData, CRLF, Chr( 10 ) )
               nColHead ++
               oTbl:Cell(nLine, nColHead ):Range:ParagraphFormat:Alignment:= wdAlignParagraphCenter
               oTbl:Cell( nLine, nColHead ):Range:Text := uData

               If hProgress != Nil

                  If nCount % nEvery == 0
                     SendMessage(hProgress, PBM_SETPOS,nCount,0)
                  EndIf

                  nCount ++
               EndIf
            Next

            nStart := ++ nLine
         Endif

         DO EVENTS

      EndIf


      For nCol := 1 To Len( oBrw:aColumns )

         uData := Eval( oBrw:aColumns[ nCol ]:bData )
         If ValType( uData ) == "C" .and. At( CRLF, uData ) > 0
            uData := StrTran( uData, CRLF, "&&" )
            If AScan( aRepl, nCol ) == 0
               AAdd( aRepl, nCol )
            EndIf
         EndIf
         uData := If( uData == NIL, "", Transform( uData, oBrw:aColumns[ nCol ]:cPicture ) )
         uData  :=  If( ValType( uData )=="D", DtoC( uData ), If( ValType( uData )=="N", Str( uData ) , ;
                    If( ValType( uData )=="L", If( uData ,".T." ,".F." ), cValToChar( uData ) ) ) )

         cText += Trim( uData ) + Chr( 9 )

         If hProgress != Nil

            If nCount % nEvery == 0
               SendMessage(hProgress, PBM_SETPOS, nCount, 0)
            EndIf

            nCount ++
         EndIf
      Next

      oBrw:Skip( 1 )

      IF ( Len( cText ) < BUFFER_CLIPBOARD ) .and. !( nRow == oBrw:nLen )
        cText += CRLF
      ELSE
	flag_new_OutWrd:=.t.
      ENDIF
      ++nLine

      //
      // Every 20k set text into Word , using Clipboard , very easy and faster.
      //

      IF flag_new_OutWrd
        CopyToClipboard(cText)
        InkeyGui(100)
        oRange:=oActive:Range(oTbl:Cell( nStart, 1):Range:Start, oTbl:Cell( nLine-1, nColDbf):Range:End)
        oRange:Select()
        oRange:Paste()
        InkeyGui(100)
        cText := ""
        nStart := nLine 
        flag_new_OutWrd:=.f.
      EndIf

      DO EVENTS

   Next
   If lTsbFooting
      For nCol := 1 To Len( oBrw:aColumns )

         uData := If( ValType( oBrw:aColumns[ nCol ]:cFooting ) == "B", Eval( oBrw:aColumns[ nCol ]:cFooting ), ;
                      oBrw:aColumns[ nCol ]:cFooting )
         uData := cValTochar( uData )
         uData := StrTran( uData, CRLF, Chr( 10 ) )
         oTbl:Cell( nLine, nCol):Range:Text := uData
      Next
   EndIf

   // обработка строк содержащие знак "&&" - многострочные строки таблицы
   If ! Empty( aRepl )
      For nCol := 1 To Len( aRepl )
        oRange:=oActive:Range(oTbl:Cell( nLenHead+1, nCol):Range:Start, oTbl:Cell( nRowDbf-1, nColDbf):Range:End)
        oRange:Select()
        findObject := oRange:Find
        MSWordFind_Replace(findObject, "&&", "^l") 
        DO EVENTS
      Next           
   EndIf

   CLEARCLIPBOARD()

   ( oBrw:cAlias )->( Eval( oBrw:bGoTop ) )  // на первую запись в таблице

   If hProgress != Nil
      SendMessage( hProgress, PBM_SETPOS, nCount, 0 )
   EndIf

   If bExtern != Nil
      Eval( bExtern, oTbl, oBrw, oWord, oActive )
   EndIf

   oBrw:Reset()

   If oBrw:lIsDbf
      ( oBrw:cAlias )->( DbGoTo( nRecNo ) )
      oBrw:GoPos(nOldRow, nOldCol)
   EndIf
   oBrw:nAt := nAt

   If lBrSelector // если есть селектор в таблице
      oBrw:lSelector := .T.  
      oBrw:InsColumn( oBrw:aClipBoard[ 2 ], oBrw:aClipBoard[ 1 ] ) 
      oBrw:lNoPaint  := .F.
   EndIf

   If ! Empty( cFile ) .and. lSave
      oActive:SaveAs( cFile, wdFormatDocument )
   EndIf

   WaitWindow()           // close the wait window
   CursorArrow()

   If hProgress != Nil
      SendMessage( hProgress, PBM_SETPOS, 0, 0 )
   EndIf

   If lActivate
      oText:HomeKey(wdStory)           // в начало текста
      oWord:Visible := .T.             // показать Word на экране
      SetWordWindowToForeground(oWord) // окно Word на передний план
   Else
      oWord:Quit()                     // закрыть Word
   EndIf

RETURN NIL

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
FUNCTION SetWordWindowToForeground(oWord)
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
FUNCTION WordOleExtern( hProgress, lTsbFont, oTbl, oBrw, oWord, oActive )
   Local lTsbSuperHd, lTsbHeading, lTsbFooting, oRange
   LOCAL nLine, nCol, nRow, aFColor, nBColor, nFColor, oPar, lendTabl
   LOCAL nCount, nTotal, nEvery, aFont, oCol, hFont, nmerge 
   LOCAL oldnFColor, aRCnFColor[4], oldaFont[3]
   LOCAL oldnBColor, aRCnBColor[4], aRCaFont[4]

   // nLine := 1  // титул таблицы 
   // nLine := 2  // пустая строка 
   // nLine := 3  // суперхидер таблицы, если есть 
   // nLine := 4  // шапка таблицы, если есть 
   // nLine := 5  // ячейки таблицы, первая ячейка (если есть суперхидер и шапка таблицы)
   // nLine := nLine + oBrw:nLen // подвал таблицы, если есть 

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

   // Цвет шрифта титула таблицы (пример смены цвета)
   aFColor := BLUE
   nLine := 1  
   oTbl:Cell(nLine, 1):Range:Font:Color := RGB(aFColor[1],aFColor[2],aFColor[3])

   nLine :=1 
   // выводим цвета фона и текста суперхидера таблицы
   If lTsbSuperHd

      nmerge :=1

      For nCol := 1 To Len( oBrw:aSuperHead )

         nFColor := myColorN     ( oBrw:aSuperhead[ nCol, 4 ], oBrw, nCol ) // oBrw:nClrSpcHdFore
         nBColor := myColorN     ( oBrw:aSuperhead[ nCol, 5 ], oBrw, nCol ) // oBrw:nClrSpcHdBack
         aFont   := GetFontParam( oBrw:aSuperHead[ nCol, 7 ] )  // шрифт суперхидера

         oTbl:Cell(nLine, nmerge):Range:Font:Color    := nFColor  // Цвет шрифта шапки
         oTbl:Cell(nLine, nmerge):Range:Shading:BackgroundPatternColor := nBColor

         If lTsbFont 
           oTbl:Cell(nLine, nmerge):Range:Font:Name := aFont[ 1 ]
           oTbl:Cell(nLine, nmerge):Range:Font:Size := aFont[ 2 ]
           oTbl:Cell(nLine, nmerge):Range:Font:Bold := aFont[ 3 ]
         Endif
         nmerge++

      Next
      nLine++
   EndIf

   // выводим цвета фона и текста шапки таблицы
   If lTsbHeading    

      For nCol := 1 TO Len( oBrw:aColumns )
          oCol    := oBrw:aColumns[ nCol ]
          nFColor := myColorN( oCol:nClrHeadFore, oBrw, nCol ) 
          nBColor := myColorN( oCol:nClrHeadBack, oBrw, nCol ) 

          oTbl:Cell(nLine, nCol):Range:Font:Color    := nFColor  // Цвет шрифта шапки
          oTbl:Cell(nLine, nCol):Range:Shading:BackgroundPatternColor := nBColor // Цвет фона шапки
          If lTsbFont 
            hFont := oCol:hFontHead              // шрифт шапки таблицы
            aFont := myFontParam( hFont, oBrw, nCol, 0 )
            oTbl:Cell(nLine, nCol):Range:Font:Name := aFont[ 1 ]
            oTbl:Cell(nLine, nCol):Range:Font:Size := aFont[ 2 ]
            oTbl:Cell(nLine, nCol):Range:Font:Bold := aFont[ 3 ]
          Endif
      Next
      nLine++
   Endif

   If hProgress != Nil
      nTotal := oBrw:nLen 
      SetProgressBarRange ( hProgress , 1 , nTotal )
      SendMessage(hProgress, PBM_SETPOS, 0, 0)
      nEvery := Max( 1, Int( nTotal * 0.05 ) ) // refresh hProgress every 5 %
   EndIf

   Eval( oBrw:bGoTop )  // переход на начало таблицы
   nCount := 0
   oldnFColor := Nil
   oldnBColor := Nil
   aFill(oldaFont,Nil)
   lendTabl := .f.

   // выводим цвета фона и текста ячеек всех колонок таблицы
   For nRow := 1 TO oBrw:nLen

      For nCol := 1 TO Len( oBrw:aColumns )
         If nRow == oBrw:nLen.and.nCol == Len( oBrw:aColumns )
            lendTabl :=.t. //флаг последней ячейки таблицы 
         Endif

        oCol    := oBrw:aColumns[ nCol ] 
        nFColor := myColorN( oCol:nClrFore, oBrw, nCol, oBrw:nAt ) 
          if (!oldnFColor == nFColor) 
             //при изменении цвета либо по концу таблицы раскрашиваем область
             if !oldnFColor==Nil
                oRange:=oActive:Range(oTbl:Cell( aRCnFColor[1], aRCnFColor[2]):Range:Start, oTbl:Cell( aRCnFColor[3], aRCnFColor[4]):Range:End)
                oRange:Font:Color    := oldnFColor  // Цвет шрифта
             Endif
             oldnFColor:=nFColor
             aRCnFColor[1] :=  nLine; aRCnFColor[2] :=  nCol
          Endif
          aRCnFColor[3] :=  nLine; aRCnFColor[4] :=  nCol
          If lendTabl
                oRange:=oActive:Range(oTbl:Cell( aRCnFColor[1], aRCnFColor[2]):Range:Start, oTbl:Cell( aRCnFColor[3], aRCnFColor[4]):Range:End)
                oRange:Font:Color    := oldnFColor  // Цвет шрифта
          Endif


          nBColor := myColorN( oCol:nClrBack, oBrw, nCol, oBrw:nAt ) 
          // Фон шрифта
          if (!oldnBColor == nBColor)
             // при изменении цвета либо по концу таблицы раскрашиваем область
             if !oldnBColor==Nil
                oRange:=oActive:Range(oTbl:Cell( aRCnBColor[1], aRCnBColor[2]):Range:Start, oTbl:Cell( aRCnBColor[3], aRCnBColor[4]):Range:End)
                oRange:Shading:BackgroundPatternColor :=oldnBColor  // Фон шрифта
             Endif
             oldnBColor:=nBColor
             aRCnBColor[1] :=  nLine; aRCnBColor[2] :=  nCol
          Endif
          aRCnBColor[3] :=  nLine; aRCnBColor[4] :=  nCol
          If lEndTabl
                oRange:=oActive:Range(oTbl:Cell( aRCnBColor[1], aRCnBColor[2]):Range:Start, oTbl:Cell( aRCnBColor[3], aRCnBColor[4]):Range:End)
                oRange:Shading:BackgroundPatternColor :=oldnBColor  // Фон шрифта
          Endif

           // Фонт шрифта
          If lTsbFont 
            aFont := myFontParam( oCol:hFont, oBrw, nCol, oBrw:nAt )

            if (!(oldaFont[1] == aFont[1].and.oldaFont[2] == aFont[2].and.oldaFont[3] == aFont[3])).or.lEndTabl
               // при изменении цвета либо по концу меняем шрифты области
               if !oldaFont[1] == Nil
                  oRange:=oActive:Range(oTbl:Cell( aRCaFont[1], aRCaFont[2]):Range:Start, oTbl:Cell( aRCaFont[3], aRCaFont[4]):Range:End)
                  oRange:Font:Name := oldaFont[ 1 ]
                  oRange:Font:Size := oldaFont[ 2 ]
                  oRange:Font:Bold := oldaFont[ 3 ]
               Endif
               oldaFont[1] := aFont[1]; oldaFont[2] := aFont[2]; oldaFont[3] := aFont[3]
               aRCaFont[1] :=  nLine; aRCaFont[2] :=  nCol
             Endif
             aRCaFont[3] :=  nLine; aRCaFont[4] :=  nCol
             if lEndTabl
              if !oldaFont[1] == Nil
                  oRange:=oActive:Range(oTbl:Cell( aRCaFont[1], aRCaFont[2]):Range:Start, oTbl:Cell( aRCaFont[3], aRCaFont[4]):Range:End)
                  oRange:Font:Name := oldaFont[ 1 ]
                  oRange:Font:Size := oldaFont[ 2 ]
                  oRange:Font:Bold := oldaFont[ 3 ]
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
      oBrw:Skip(1)
   Next
   
   // выводим цвета фона и текста подвала таблицы
   If lTsbFooting

      For nCol := 1 TO Len( oBrw:aColumns )
          oCol    := oBrw:aColumns[ nCol ] 
          nFColor := myColorN( oCol:nClrFootFore, oBrw, nCol, oBrw:nAt ) 
          nBColor := myColorN( oCol:nClrFootBack, oBrw, nCol, oBrw:nAt ) 

          oTbl:Cell(nLine, nCol):Range:Font:Color    := nFColor  // Цвет шрифта шапки
          oTbl:Cell(nLine, nCol):Range:Shading:BackgroundPatternColor := nBColor // Цвет фона шапки
   
          If lTsbFont 
             aFont := myFontParam( oCol:hFontFoot, oBrw, nCol, 0 )
   
            oTbl:Cell(nLine, nCol):Range:Font:Name := aFont[ 1 ]
            oTbl:Cell(nLine, nCol):Range:Font:Size := aFont[ 2 ]
            oTbl:Cell(nLine, nCol):Range:Font:Bold := aFont[ 3 ]
          Endif
      Next

      nLine++                          
   Endif

   // Доп.надпись под таблицей
   aFColor := RED
   oPar := oActive:Paragraphs:Add()
   oPar:Range:Font:Color := RGB(aFColor[1],aFColor[2],aFColor[3])   
   oPar:Range:Font:Name  := "Times New Roman"   
   oPar:Range:Font:Size  := 14   
   oPar:Range:Font:Bold  := .T.   
   oPar:Range:Text:= CRLF + "End table ! - Version " + WordVersion(VAL(oWord:Version)) + CRLF

   RETURN Nil

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

* =======================================================================================
STATIC FUNCTION myFontParam( hFont, oBrw, nCol, nAt )
   LOCAL aFont, oCol := oBrw:aColumns[ nCol ] 
   DEFAULT nAt := 0
   // шрифт ячеек таблицы
   hFont := If( hFont == Nil, oBrw:hFont, hFont )  
   hFont := If( ValType( hFont ) == "B", Eval( hFont, nAt, nCol, oBrw ), hFont )  

   If empty(hFont)
      aFont    := array(3) 
      aFont[1] := _HMG_DefaultFontName
      aFont[2] := _HMG_DefaultFontSize
      aFont[3] := .F.
   Else
      aFont := GetFontParam( hFont )
   EndIf

RETURN aFont

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
