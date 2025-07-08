#include "minigui.ch"
#include "TSBrowse.ch"

REQUEST DBFCDX

MEMVAR oBrw

FUNCTION Main()
   LOCAL aStr := {}
   LOCAL cDbf := "Rates"
   LOCAL cAlias
   LOCAL oWnd, nY, nX, nW, nH

   SET OOP     ON
   SET CENTURY ON
   SET DATE FORMAT 'DD.MM.YYYY'
   SET CODEPAGE TO RUSSIAN

   rddSetDefault( 'DBFCDX' )

   SET FONT TO "MS Sans Serif" , 9

   DEFINE FONT Normal    FONTNAME _HMG_DefaultFontName SIZE _HMG_DefaultFontSize
   DEFINE FONT Header    FONTNAME "Arial"              SIZE _HMG_DefaultFontSize BOLD
   DEFINE FONT Footer    FONTNAME "Arial"              SIZE _HMG_DefaultFontSize BOLD

   IF !hb_FileExists( cDbf + ".dbf"  )
      AAdd( aStr, { 'DATE',	 'D',  8, 0 } )
      AAdd( aStr, { 'CHARCODE',  'C',  3, 0 } )
      AAdd( aStr, { 'NUMCODE',   'C',  3, 2 } )
      AAdd( aStr, { 'NAME',      'C', 50, 0 } )
      AAdd( aStr, { 'NOMINAL',   'N',  5, 0 } )
      AAdd( aStr, { 'VALUE',     'N', 12, 4 } )

      dbCreate( cDbf + ".dbf", aStr )

   ENDIF

   USE ( cDbf + ".dbf" ) ALIAS "RATES" EXCL NEW

   IF hb_FileExists( cDbf + ".cdx" )
       SET INDEX TO ( cDbf + ".cdx")
   ELSE
       INDEX ON DTOS(FIELD->DATE)+FIELD->CHARCODE TAG "DATE" TO ( cDbf + ".cdx")
   END

   cAlias := ALIAS()

   (cAlias)->( OrdSetFocus("DATE") )

   DEFINE WINDOW Form_0 ;
      At 0, 0 ;
      WIDTH  710 ;
      HEIGHT 700 ;
      TITLE "Exchange rate of the Central Bank of the Russian Federation" ;
      MAIN ;
      NOMAXIMIZE NOSIZE ;
      ON RELEASE dbCloseArea( cAlias )

      oWnd := ThisWindow.Object

      DEFINE STATUSBAR BOLD
         STATUSITEM ""
         STATUSITEM "" WIDTH 300 FONTCOLOR BLUE
      END STATUSBAR

      ON KEY ESCAPE ACTION ThisWindow.Release

      DEFINE LABEL Label_1
         ROW    7
         COL    5
         WIDTH    oWnd:ClientWidth - 110
         HEIGHT    18
         FONTNAME 'Arial Narrow'
         FONTSIZE   12
         FONTBOLD   TRUE
         FONTITALIC TRUE
         FONTCOLOR  {0,0,0}
         VALUE    " "
      END LABEL

      This.Label_1.Cargo := "Exchange rate of the Central Bank of the Russian Federation on: "

      DEFINE DATEPICKER Date_1
         ROW    2
         COL    oWnd:ClientWidth - 105
         WIDTH  100
         VALUE  Date()
         SHOWNONE   .F.
         FONTNAME   'Arial'
         FONTSIZE   9
         FONTBOLD   FALSE
         FONTITALIC FALSE
         ON CHANGE  {|| SetDate( This.Value ) }
         TABSTOP .F.
      END DATEPICKER

      This.Date_1.Cargo := Date()

      nY := This.Date_1.Row + This.Date_1.Height + 5
      nX := 1
      nW := oWnd:ClientWidth  - nX * 2
      nH := oWnd:ClientHeight - nY - oWnd:StatusBar:Height

      DEFINE TBROWSE oBrw ;
             AT nY, nX ALIAS cAlias WIDTH nW HEIGHT nH ;
             GRID ;
             FONT       { "Normal", "Header", "Footer" } ;
             COLORS     { CLR_BLACK, CLR_BLUE } ;
             BRUSH      { 255, 255, 240 } ;
             HEADERS    { "Char;Code", "Num;Code", "Name"             , "Nominal", "Value"   } ;
             COLSIZES   { 40         , 40        , 250                , 50       , 50        } ;
             PICTURE    {            ,           , "@R  "+Repl('X',50),          ,           } ;
             JUSTIFY    { DT_CENTER  , DT_CENTER , DT_LEFT            , DT_CENTER, DT_CENTER } ;
             COLUMNS    { "CHARCODE" , "NUMCODE" , "NAME"             , "NOMINAL", "VALUE"   } ;
             COLNAMES   { "CHAR"     , "NUM"     , "NAME"             , "NOM"    , "VAL"     } ;
             COLNUMBER  { 1, 30 }                                                              ;
             FOOTERS    .T.                                                                    ;
             LOADFIELDS ;
             FIXED ADJUST

             mySetTsb( oBrw )
             myColorTsb( oBrw )
      END TBROWSE

      oBrw:SetNoHoles()
      SetDate( Date() )

   END WINDOW

   Form_0.Center
   Form_0.Activate

RETURN NIL


FUNCTION mySetTsb( oBrw )
   WITH OBJECT oBrw
      :nColOrder    := 0
      :lNoChangeOrd := .T.
      :nWheelLines  := 1
      :lNoGrayBar   := .F.
      :lNoLiteBar   := .F.
      :lNoResetPos  := .F.
      :lNoHScroll   := .T.
      :lNoPopUp     := .T.
   END WITH
RETURN Nil


FUNCTION myColorTsb( oBrw )
   WITH OBJECT oBrw
      :nClrLine              := RGB(180,180,180) // COLOR_GRID
      :SetColor( { 11 }, { { || RGB(0,0,0) } } )
      :SetColor( {  2 }, { { || RGB(255,255,240) } } )
      :SetColor( {  5 }, { { || RGB(0,0,0) } } )
      :SetColor( {  6 }, { { |a,b,c| iif( c:nCell == b,  -CLR_HRED        , -RGB(128,225,225) ) } } )
      :SetColor( { 12 }, { { |a,b,c| iif( c:nCell == b,  -RGB(128,225,225), -RGB(128,225,225) ) } } )
   END WITH
RETURN Nil


FUNCTION SetDate( dDate )
   LOCAL aArray
   LOCAL cDate := DtoS(dDate)
   LOCAL lRet  := .F.

    IF dDate <= Date()
       Form_0.StatusBar.Item(2) := "... W A I T ..."
       This.Date_1.Cargo        := dDate
       DO EVENTS
       Rates->(OrdScope( 0, NIL ))
       Rates->(OrdScope( 1, NIL ))

       IF ! Rates->(dbSeek(cDate))
          aArray := GetCBR( dDate )
          IF ( lRet := ! Empty(aArray) )
             AEval( aArray, {|e| Rates->(DBAppend()),;
                                 Rates->Date     := dDate,;
                                 Rates->CHARCODE := e[1],;
                                 Rates->NUMCODE  := e[2],;
                                 Rates->NOMINAL  := Val(e[3]),;
                                 Rates->NAME     := e[4],;
                                 Rates->VALUE    := Val(e[5]) ;
                            } )
          ENDIF
       ENDIF

       Rates->(OrdScope( 0, cDate ))
       Rates->(OrdScope( 1, cDate ))
       Rates->(dbGoTop())
    ENDIF

    Form_0.StatusBar.Item(2) := ' '
    This.Label_1.Value := This.Label_1.Cargo + DtoC( This.Date_1.Cargo )

    DO EVENTS
    oBrw:Reset()
    oBrw:SetFocus()
    DO EVENTS

RETURN lRet


FUNCTION GetCBR( dDate )
 LOCAL oHttp, cHtml
 LOCAL aArray
 LOCAL cDate := StrZero( Day( dDate ), 2, 0 ) + "/" + StrZero( Month( dDate ), 2, 0 ) + "/" + StrZero( Year( dDate ), 4, 0 )

    Form_0.StatusBar.Item(1) := " "

    oHttp := TIpClientHttp():New( "http://www.cbr.ru/scripts/XML_daily.asp?date_req=" + cDate )
    IF ! oHttp:open()
       Form_0.StatusBar.Item(2) := "Connection error: " +  oHttp:lastErrorMessage()
       RETURN {}
    ENDIF

    Form_0.StatusBar.Item(2) := "Connection established"

    cHtml := oHttp:readAll()
    oHttp:close()

    aArray := FindCBR( cHtml )

    IF ! Empty( aArray )
       Form_0.StatusBar.Item(1) := "Data successfully received"
    END		

RETURN aArray


FUNCTION FindCBR( cHtml )
 LOCAL oDoc, oVal, oIterator, oCurrent
 LOCAL cNumCode, cCharCode ,cNominal, cName, cValue
 LOCAL aArray := {}

    oDoc := TXMLDocument():New( cHtml, 8 )

    IF oDoc:nError != 0
       Form_0.StatusBar.Item(1) :=  "xml parsing error " + hb_ntos( oDoc:nError )
       RETURN {}
    ENDIF

    oVal := oDoc:findfirst( "Valute" )
    IF oVal == NIL
       Form_0.StatusBar.Item(1) := "xml parsing error " + "Key not found"
       RETURN {}
    ENDIF

    DO WHILE .T.

       oIterator := TXMLIterator():New( oVal )

       DO WHILE .T.
          oCurrent := oIterator:Next()
          IF oCurrent == NIL
             EXIT
          ELSE

             switch oCurrent:cName
                case "CharCode"
                   cCharCode := oCurrent:cData
                   exit
                case "NumCode"
                   cNumCode := oCurrent:cData
                   exit
                case "Nominal"
                   cNominal := oCurrent:cData
                   exit
                case "Name"
                   cName := oCurrent:cData
                   exit
                case "Value"
                   cValue := CharRepl( ",", oCurrent:cData, "." )
                   exit
             end switch

          ENDIF

       ENDDO

       AAdd( aArray, { cCharCode, cNumCode, cNominal, cName, cValue } )

       oVal := oDoc:findnext()

       IF oVal == NIL ; EXIT
       ENDIF

    ENDDO

RETURN aArray
