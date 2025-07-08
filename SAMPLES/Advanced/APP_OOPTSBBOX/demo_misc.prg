//

#include "demo.ch"

FIELD CUSTNO, COUNTRYNO, KOD

*----------------------------------------------------------------------------*
FUNC RecGet()
*----------------------------------------------------------------------------*
   LOCAL oRec := oKeyData()

   AEval( Array( FCount() ), {| v, n|  oRec:Set( FieldName( n ), FieldGet( n ) ) } )

RETURN oRec

*----------------------------------------------------------------------------*
FUNC InitBaseCols()
*----------------------------------------------------------------------------*
   LOCAL oCol

   // Сolumn objects create
   Dbf2Cols( 'Customer', 'Cust', '-TAXRATE,LASTINVOIC'    )  // Exclude fields
   Dbf2Cols( 'Country', 'Land', 'COUNTRYNO,KOD,NAME,ISES' )  // Include fields
   // Dbf2Cols('Country' , 'Land'                           )  // All fields

   // Сolumn objects set properties
   sCols( Cust.CUSTNO, lEmptyValToChar, .F.                     )
   sCols( Cust.CUSTNO, cHeading, 'Company' + CRLF + 'ID' )
   sCols( Cust.CUSTNO, nAlign, DT_CENTER               )
   sCols( Cust.CUSTNO, bData, {|| hb_ntos( CUSTNO ) }   )
   sCols( Cust.COMPANY, cHeading, 'Name'                  )
   sCols( Cust.COUNTRY, cHeading, 'Country'               )
   sCols( Cust.CITY, cHeading, 'City'                  )

   sCols( Land.COUNTRYNO, lEmptyValToChar, .F.                      )
   sCols( Land.COUNTRYNO, cHeading, 'Country' + CRLF + 'ID'  )
   sCols( Land.KOD, cHeading, 'Code'                   )
   sCols( Land.KOD, nWidth, TxtWidth( 4 )              )
   sCols( Land.KOD, nAlign, DT_CENTER                )
   sCols( Land.NAME, cHeading, 'Name'                   )
   sCols( Land.NAME, nWidth, TxtWidth( 35 )             )
   sCols( Land.ISES, cHeading, 'Enter' + CRLF + 'into EU' )
   sCols( Land.ISES, nWidth, TxtWidth( 'into EU' )        )

   // Для проверки, потом можно в комментарии положить или удалить
   oCol        := gCols( Land.ISES )
   oCol:cAlias := 'CUST'

   sCols( Cust.ISES, oCol )

   // AEval(gCols()     , {|ao,no,oc| oc:=ao[2], _LogFile(.T.,no,ao[1],oc:cAlias,oc:cName,oc:cField,oc:cData,oc:cHeading) })
   // AEval(gCols(Land.), {|ao,no,oc| oc:=ao[2], _LogFile(.T.,no,ao[1],oc:cAlias,oc:cName,oc:cField,oc:cData,oc:cHeading) })
   // AEval(gCols(Cust.), {|ao,no,oc| oc:=ao[2], _LogFile(.T.,no,ao[1],oc:cAlias,oc:cName,oc:cField,oc:cData,oc:cHeading) })

   ? '------------------------ after Add column',
   AEval( gCols( Cust. ), {| ao, no, oc| oc := ao[ 2 ], _LogFile( .T., no, ao[ 1 ], oc:cAlias, oc:cName, oc:cField, oc:cData, oc:cHeading ) } )

   dCols( Cust.ISES )

   ? '------------------------ after Del column',
   AEval( gCols( Cust. ), {| ao, no, oc| oc := ao[ 2 ], _LogFile( .T., no, ao[ 1 ], oc:cAlias, oc:cName, oc:cField, oc:cData, oc:cHeading ) } )

RETURN NIL

*----------------------------------------------------------------------------*
FUNC Tsb_Create( cName, nY, nX, nW, nH, aCols, aColors, hFontHead, hFontFoot )
*----------------------------------------------------------------------------*
   LOCAL oBrw
   DEFAULT hFontFoot := hFontHead

   IF Empty( aColors ) .OR. ! HB_ISARRAY( aColors )
      aColors := {}
      AAdd( aColors, { CLR_FOCUSB, {|a, b, c| If( c:nCell == b,   ;
         { RGB( 66, 255, 236 ), RGB( 209, 227, 248 ) }, ;
         { RGB( 220, 220, 220 ), RGB( 220, 220, 220 ) } ) } } )
   ENDIF

   DEFINE TBROWSE &cName OBJ oBrw AT nY, nX ALIAS Alias() WIDTH nW HEIGHT nH CELL ;
      COLORS   aColors

   IF ! Empty( hFontHead )
      :hFontHead := hFontHead
   ENDIF
   IF ! Empty( hFontFoot )
      :hFontFoot := hFontFoot
   ENDIF

   AEval( aCols, {| oc| :AddColumn( oc ) } )

   :lNoGrayBar   := .T.
   :lNoLiteBar   := .F.
   :nWheelLines  := 1
   :nClrLine     := COLOR_GRID
   :nHeightCell  += 5
   :nHeightHead  := :nHeightCell + 2
   :nHeightFoot  := :nHeightCell + 2
   :lDrawFooters := .T.
   :lFooting     := .T.
   :lNoVScroll   := .F.
   :lNoHScroll   := GetWindowWidth( :hWnd ) > ( :GetAllColsWidth() + GetVSCrollBarWidth() )
   :lNoKeyChar   := .T.
   // :lNoMoveCols := .T.
   :lNoResetPos := .F.
   :lPickerMode := .F.              // usual date format
   :nLineStyle  := LINES_ALL        // LINES_NONE LINES_ALL LINES_VERT LINES_HORZ LINES_3D LINES_DOTTED

   :AdjColumns()
   :ResetVScroll( .T. )
   :oHScroll:SetRange( 0, 0 )

   END TBROWSE

RETURN oBrw

*----------------------------------------------------------------------------*
FUNC MyUse( cDbf, cAls, lShared, cRdd )
*----------------------------------------------------------------------------*
   LOCAL lRet := .F.
   DEFAULT lShared := .T.

   SELECT 0
   IF     Empty ( cAls )    ; cAls := '_XYZ_' + hb_ntos( Select() )
   ELSEIF Select( cAls ) > 0; cAls += '_'    + hb_ntos( Select() )
   ENDIF

   BEGIN SEQUENCE WITH {|e| break( e ) }
      dbUseArea( .F., cRdd, cDbf, cAls, lShared )
      lRet := Used()
   END SEQUENCE

   IF lRet
      IF ordCount() > 0
         ordSetFocus( 1 )
      ENDIF
      dbGoTop()
   ENDIF

RETURN lRet

*----------------------------------------------------------------------------*
FUNC wPost( nEvent, nIndex, xParam )
*----------------------------------------------------------------------------*
   LOCAL oWnd

   IF HB_ISOBJECT( nIndex )

      IF nIndex:ClassName == 'TSBROWSE'
         oWnd := _WindowObj( nIndex:cParentWnd )
      ELSE
         oWnd := nIndex
      ENDIF

      oWnd:SetProp( nEvent, xParam )
      oWnd:PostMsg( nEvent )

   ELSE

      DEFAULT nEvent := Val( This.Name )

      IF nEvent > 0
         oWnd := ThisWindow.Object
         oWnd:SetProp( nEvent, xParam )
         oWnd:PostMsg( nEvent, nIndex )
      ENDIF

   ENDIF

RETURN NIL

*----------------------------------------------------------------------------*
FUNC wSend( nEvent, nIndex, xParam )
*----------------------------------------------------------------------------*
   LOCAL oWnd

   IF HB_ISOBJECT( nIndex )

      IF nIndex:ClassName == 'TSBROWSE'
         oWnd := _WindowObj( nIndex:cParentWnd )
      ELSE
         oWnd := nIndex
      ENDIF

      oWnd:SetProp( nEvent, xParam )
      oWnd:SendMsg( nEvent )

   ELSE

      DEFAULT nEvent := Val( This.Name )

      IF nEvent > 0
         oWnd := ThisWindow.Object
         oWnd:SetProp( nEvent, xParam )
         oWnd:SendMsg( nEvent, nIndex )
      ENDIF

   ENDIF

RETURN NIL

*----------------------------------------------------------------------------*
FUNC CreateIndex()
*----------------------------------------------------------------------------*
   LOCAL cDbf := 'Customer'
   LOCAL cAls := 'CUST'

   IF ! hb_FileExists( cDbf + '.cdx' )
      USE ( cDbf )  ALIAS &cAls   NEW
      INDEX ON CUSTNO TAG CUSTNO
      USE
   ENDIF

   cDbf := 'Country'
   cAls := 'LAND'

   IF ! hb_FileExists( cDbf + '.cdx' )
      USE ( cDbf )  ALIAS &cAls   NEW
      INDEX ON COUNTRYNO TAG COUNTRYNO
      INDEX ON KOD       TAG KOD
      USE
   ENDIF

RETURN NIL

*----------------------------------------------------------------------------*
FUNC Dbf2Cols( cDbf, cAls, cField, cVarName )
*----------------------------------------------------------------------------*
   LOCAL nOldArea := Select()
   LOCAL lVarName := ! Empty( cVarName ), lInclude
   LOCAL oCol, aStru, aFld, cKey, lExclude := .F.
   LOCAL cFld, cTyp, nLen, nDec, cPic, nAli, nAlh, nAlf, nWdt

   DEFAULT cAls := Left( cDbf, 4 ), cVarName := BASE_COLUMNS, cField := ""

   _CrtCols( cVarName )

   IF ! MyUse( cDbf, cAls )
      dbSelectArea( nOldArea )
      MsgStop( 'Table not used !' + CRLF + cDbf, 'ERROR' )
      RETURN .F.
   ENDIF

   aStru := dbStruct()
   dbCloseArea()
   dbSelectArea( nOldArea )

   IF ! Empty( cField )
      IF Left( cField, 1 ) $ '+-'
         lExclude := Left( cField, 1 ) == '-'
         cField   := Subs( cField, 2 )
      ENDIF
      cField := StrTran( cField, ' ', '' )
      cField := ',' + Upper( cField ) + ','
   ENDIF

   FOR EACH aFld IN aStru
      cFld := aFld[ 1 ]                   // FieldName

      IF Empty( cField ); lInclude := .T.
      ELSE
         IF lExclude  ; lInclude := ! ',' + cFld + ',' $ cField
         Else         ; lInclude :=   ',' + cFld + ',' $ cField
         ENDIF
      ENDIF

      IF ! lInclude; LOOP               // FieldName skip
      ENDIF

      cTyp := aFld[ 2 ]
      nLen := aFld[ 3 ]
      nDec := aFld[ 4 ]
      cKey := iif( lVarName, '', cAls + '.' ) + cFld
      nAli := 0
      nAlh := 1
      nAlf := 1
      cPic := Nil
      nWdt := 60

      IF cTyp $ '@='; cTyp := 'T'     // as TimeStamp
      ENDIF

      IF     cTyp == 'C'
         cPic := Replicate( 'X', nLen )
         nWdt := TxtWidth( cPic )
      ELSEIF cTyp == 'M'
         nLen := 30
         cPic := Replicate( 'X', nLen )
         nWdt := TxtWidth( cPic )
      ELSEIF cTyp == 'D'
         nAli := 1
         nWdt := TxtWidth( 10 )
      ELSEIF cTyp == 'N'
         nAli := 2
         nAlf := 2
         IF nDec > 0
            cPic := Replicate( '9', nLen - nDec ) + '.' + Replicate( '9', nDec )
         ELSE
            cPic := Replicate( '9', nLen )
         ENDIF
         nWdt := TxtWidth( cPic )
      ELSEIF cTyp == '+'
         nAli := 1
         nAlf := 1
         nLen := 10
         cPic := Replicate( '9', nLen )
         nWdt := TxtWidth( cPic )
      ELSEIF cTyp == '^'
         nAli := 1
         nAlf := 1
         nLen := 19
         cPic := Replicate( '9', nLen )
         nWdt := TxtWidth( cPic )
      ELSEIF cTyp == 'L'
         nAli := 1
      ELSEIF cTyp == 'T'
         nAli := 1
         nAlf := 1
         nLen := 17
         cPic := Replicate( 'X', nLen )
         nWdt := TxtWidth( cPic )
      ENDIF

      DEFINE COLUMN oCol DATA      cFld                              ;
         HEADER    cFld                              ;
         FOOTER    ' '                               ;
         ALIGN     nAli, nAlh, nAlf                  ;
         WIDTH     nWdt                              ;
         PICTURE   cPic                              ;
         MOVE      0                                 ;
         DBLCURSOR                                   ;
         NAME      &cFld      ALIAS &( Upper( cAls ) )
      IF cTyp == 'L'
         oCol:lCheckBox         := .T.
      ENDIF
      oCol:lEmptyValToChar   := .T.
      oCol:lOnGotFocusSelect := .T.
      IF cTyp == 'T'
         oCol:cData             := 'hb_macroblock( hb_TSToStr("' + oCol:cField + '", .T. ))'
         oCol:bData             :=  hb_macroBlock( hb_TSToStr(   oCol:cField, .T. ) )
      ELSE
         oCol:cData             := 'hb_macroblock("' + oCol:cField + '")'
         oCol:bData             :=  hb_macroBlock(   oCol:cField )
      ENDIF

      __mvGet( cVarName ):Set( cKey, oCol )
   NEXT

RETURN .T.

*----------------------------------------------------------------------------*
FUNC _CrtCols( cVarName )
*----------------------------------------------------------------------------*
   LOCAL oCol, oVar, lVar

   IF ! Empty( cVarName ) .AND. HB_ISCHAR( cVarName )

      IF ( lVar := __mvExist( cVarName ) )
         oVar := __mvGet( cVarName )
         IF HB_ISOBJECT( oVar )
            RETURN oVar
         ENDIF
      ENDIF

      IF ! lVar
         __mvPublic( cVarName )
      ENDIF

      __mvPut( cVarName, oKeyData() )

      oVar := __mvGet( cVarName )

   ELSE                 // using: PRIVATE o_Cols := _CrtCols()

      oVar := oKeyData()

   ENDIF

   IF ! HB_ISOBJECT( oVar:Get( 'OrdKeyNo', NIL ) )

      DEFINE COLUMN oCol  DATA      'hb_ntos(ORDKEYNO())'             ;
         HEADER    '#'                               ;
         FOOTER    ' '                               ;
         ALIGN     1, 1, 1                           ;
         WIDTH     TxtWidth( 6 )                       ;
         PICTURE   '999999'                          ;
         MOVE      0                                 ;
         DBLCURSOR                                   ;
         NAME      ORDKEYNO           ALIAS ____
      oCol:cFooting        := {|nc, ob| nc := ob:nLen, iif( Empty( nc ), '', hb_ntos( nc ) ) }
      oCol:lEmptyValToChar := .T.
      oCol:cData           := 'hb_macroblock("' + oCol:cField + '")'
      oCol:bData           :=  hb_macroBlock(   oCol:cField )

      oVar:Set( 'OrdKeyNo', oCol )

   ENDIF

RETURN oVar

*----------------------------------------------------------------------------*
FUNC _DelCols( cKey, cVarName )
*----------------------------------------------------------------------------*
   LOCAL oVar
   DEFAULT cVarName := BASE_COLUMNS

   IF !__mvExist( cVarName );                       RETURN .F.
   ENDIF

   IF ! HB_ISOBJECT( oVar := __mvGet( cVarName ) ); RETURN .F.
   ENDIF

   oVar:Del( cKey )

RETURN .T.

*----------------------------------------------------------------------------*
FUNC _SetCols( cKey, cName, xVal, cVarName )
*----------------------------------------------------------------------------*
   LOCAL oVar, oCol
   DEFAULT cVarName := BASE_COLUMNS

   IF !__mvExist( cVarName );                       RETURN .F.
   ENDIF

   IF ! HB_ISOBJECT( oVar := __mvGet( cVarName ) ); RETURN .F.
   ENDIF

   IF PCount() < 3
      IF PCount() == 2 .AND. HB_ISOBJECT( cName )
         oVar:Set( cKey, cName )
      ENDIF
      RETURN .F.
   ENDIF

   IF ! HB_ISOBJECT( oCol := oVar:Get( cKey ) );  RETURN .F.
   ENDIF

   oCol:SetProperty( cName, xVal )

RETURN .T.

*----------------------------------------------------------------------------*
FUNC _GetCols( cKey, cVarName )
*----------------------------------------------------------------------------*
   LOCAL oVar, aCol := {}
   DEFAULT cVarName := BASE_COLUMNS

   IF !__mvExist( cVarName );                       RETURN NIL
   ENDIF

   IF ! HB_ISOBJECT( oVar := __mvGet( cVarName ) ); RETURN NIL
   ENDIF

   IF PCount() > 0

      IF Right( cKey, 1 ) == '*'; cKey := Left( cKey, At( '.', cKey ) )
      ENDIF

      IF Right( cKey, 1 ) == '.'
         AEval( oVar:GetAll(), {| ac| iif( cKey $ ac[ 1 ], AAdd( aCol, AClone( ac ) ), Nil ) } )
         RETURN aCol
      ENDIF

      RETURN oVar:Get( cKey ):Clone()

   ENDIF

RETURN oVar:GetAll()

*----------------------------------------------------------------------------*
FUNC SetsEnv()
*----------------------------------------------------------------------------*

   SET CODEPAGE TO RUSSIAN
   SET LANGUAGE TO RUSSIAN

   rddSetDefault( "DBFCDX" )

   SET CENTURY      ON
   SET DATE         GERMAN
   SET DELETED      ON
   SET EXCLUSIVE    ON
   SET EPOCH TO     2000
   SET AUTOPEN      ON
   SET EXACT        ON
   SET SOFTSEEK     ON

   SET NAVIGATION   EXTENDED
   SET FONT         TO "Arial", 11
   SET DEFAULT ICON TO "hmg_ico"

   // --------------------------------
   SET OOP ON
   // --------------------------------

   DEFINE FONT FontNorm FONTNAME App.FontName SIZE App.FontSize
   DEFINE FONT FontBold FONTNAME App.FontName SIZE App.FontSize BOLD

RETURN NIL

*----------------------------------------------------------------------------*
FUNC TxtWidth( cText, cFontName, nFontSize, cChr ) // get the width of the text
*----------------------------------------------------------------------------*
   LOCAL hFont, nWidth
   LOCAL lFont  := ! HB_ISNUMERIC( cFontName )
   DEFAULT cChr := 'A'

   IF ValType( cText ) == 'N'
      cText := Replicate( cChr, cText )
   ENDIF

   DEFAULT cText := Replicate( cChr, 2 ), ;
      cFontName := App.FontName,         ;
      nFontSize := App.FontSize

   IF lFont; hFont := InitFont( cFontName, nFontSize )
   Else    ; hFont := cFontName
   ENDIF

   nWidth := GetTextWidth( 0, cText + cChr, hFont )

   IF lFont; DeleteObject( hFont )
   ENDIF

RETURN nWidth

*----------------------------------------------------------------------------*
FUNC TxtHeight( cFontName, nFontSize, nDelta ) // get the height of the text
*----------------------------------------------------------------------------*
   LOCAL hFont, cText := "B", nHeight
   LOCAL lFont := ! HB_ISNUMERIC( cFontName )
   DEFAULT cFontName := App.FontName, ;
      nFontSize := App.FontSize, ;
      nDelta    := 1                 // as TsBrowse :nHeightCell default

   IF lFont; hFont := InitFont( cFontName, nFontSize )
   Else    ; hFont := cFontName
   ENDIF

   nHeight := GetTextHeight( 0, cText, hFont )

   IF lFont; DeleteObject( hFont )
   ENDIF

RETURN ( nHeight + nDelta )
