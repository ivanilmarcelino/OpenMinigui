/*
 * MINIGUI - Harbour Win32 GUI library Demo 
 *
 * Copyright 2018 Sergej Kiselev
 *
*/

#include "demo.ch"
#include "hbclass.ch"

FUNC oTsb2Xml2Xls( cXml, oBrw, aTitle, aBefore, aAfter, aForma, cSheet )
   LOCAL oXml 

   oXml := Tsb2Xml2Xls():New(cXml, oBrw, aTitle, aBefore, aAfter, aForma, cSheet )

   oXml:StyleForma  ()   
   oXml:StyleTitle  ()   
   oXml:StyleBefore ()   
   oXml:StyleAfter  ()   

   oXml:StyleTable  ()

   oXml:WriteForma  ()   
   oXml:WriteTitle  ()   
   oXml:WriteBefore ()   
   oXml:WriteSuperHd()  
   oXml:WriteHeader ()
   oXml:WriteColumns()
   oXml:WriteFooter ()   
   oXml:WriteAfter  ()   
   
RETURN oXml

FUNC oColorN2H( cPref, nDef, cDef )
RETURN ColorN2H():New( cPref ):Def( nDef, cDef )

CLASS ColorN2H

  VAR oNH               INIT oKeyData()
  VAR cPref             INIT 'C'
  VAR nLenN             INIT 0
  VAR nDef              INIT CLR_SILVER
  VAR cDef              INIT HMG_ClrToHTML( CLR_SILVER )

  METHOD New( cPref ) INLINE ( ::cPref := hb_defaultValue(cPref, ::cPref), Self )
  METHOD Def( nDef, cDef )
  METHOD Name( nKey )
  METHOD Set ( nClr, hClr ) INLINE ::oNH:Set( nClr, hClr )
  METHOD Get ( nKey, xDef ) INLINE ( nKey := hb_defaultValue( nKey, ::nDef), ;
                                     xDef := hb_defaultValue( xDef, ::cDef), ;
                                                   ::oNH:Get( nKey,   xDef ) )
ENDCLASS

METHOD Name( nKey ) CLASS ColorN2H
   LOCAL cName := ::cPref

   If HB_ISARRAY( nKey )
      cName += strzero(::oNH:Pos( nKey[1] ), ::nLenN ) 
      If len(nKey) > 1 .and. nKey[2] != Nil
         cName += strzero(::oNH:Pos( nKey[2] ), ::nLenN ) 
      EndIf
   Else
      cName += strzero(::oNH:Pos( nKey )   , ::nLenN )
   EndIf

RETURN cName

METHOD Def( nDef, cDef ) CLASS ColorN2H
   LOCAL aClr

   If nDef != Nil .and. HB_ISNUMERIC(nDef)
      ::nDef := nDef
   EndIf

   If cDef != Nil .and. HB_ISCHAR(cDef)
      ::cDef := cDef
   EndIf

   aClr := {                            ; 
             CLR_BLACK                , ; 
             CLR_MAROON               , ; 
             CLR_DARKRED              , ; 
             CLR_RED                  , ; 
             CLR_ORANGERED            , ; 
             CLR_DARKGREEN            , ; 
             CLR_GREEN                , ; 
             CLR_OLIVE                , ; 
             CLR_DARKORANGE           , ; 
             CLR_ORANGE               , ; 
             CLR_GOLD                 , ; 
             CLR_LAWNGREEN            , ; 
             CLR_LIME                 , ; 
             CLR_CHARTREUSE           , ; 
             CLR_DARKGOLDENROD        , ; 
             CLR_SADDLEBROWN          , ; 
             CLR_CHOCOLATE            , ; 
             CLR_GOLDENROD            , ; 
             CLR_FIREBRICK            , ; 
             CLR_FORESTGREEN          , ; 
             CLR_OLIVEDRAB            , ; 
             CLR_BROWN                , ; 
             CLR_SIENNA               , ; 
             CLR_DARKOLIVEGREEN       , ; 
             CLR_GREENYELLOW          , ; 
             CLR_LIMEGREEN            , ; 
             CLR_YELLOWGREEN          , ; 
             CLR_CRIMSON              , ; 
             CLR_PERU                 , ; 
             CLR_TOMATO               , ; 
             CLR_DARKSLATEGRAY        , ; 
             CLR_CORAL                , ; 
             CLR_SEAGREEN             , ; 
             CLR_YELLOW               , ; 
             CLR_SANDYBROWN           , ; 
             CLR_DIMGRAY              , ; 
             CLR_DARKKHAKI            , ; 
             CLR_MIDNIGHTBLUE         , ; 
             CLR_MEDIUMSEAGREEN       , ; 
             CLR_SALMON               , ; 
             CLR_DARKSALMON           , ; 
             CLR_LIGHTSALMON          , ; 
             CLR_SPRINGGREEN          , ; 
             CLR_NAVY                 , ; 
             CLR_PURPLE               , ; 
             CLR_TEAL                 , ; 
             CLR_GRAY                 , ; 
             CLR_LIGHTCORAL           , ; 
             CLR_INDIGO               , ; 
             CLR_MEDIUMVIOLETRED      , ; 
             CLR_BURLYWOOD            , ; 
             CLR_DARKBLUE             , ; 
             CLR_DARKMAGENTA          , ; 
             CLR_DARKSLATEBLUE        , ; 
             CLR_DARKCYAN             , ; 
             CLR_TAN                  , ; 
             CLR_KHAKI                , ; 
             CLR_ROSYBROWN            , ; 
             CLR_DARKSEAGREEN         , ; 
             CLR_SLATEGRAY            , ; 
             CLR_LIGHTGREEN           , ; 
             CLR_DEEPPINK             , ; 
             CLR_PALEVIOLETRED        , ; 
             CLR_PALEGREEN            , ; 
             CLR_LIGHTSLATEGRAY       , ; 
             CLR_MEDIUMSPRINGGREEN    , ; 
             CLR_CADETBLUE            , ; 
             CLR_DARKGRAY             , ; 
             CLR_LIGHTSEAGREEN        , ; 
             CLR_MEDIUMAQUAMARINE     , ; 
             CLR_PALEGOLDENROD        , ; 
             CLR_NAVAJOWHITE          , ; 
             CLR_WHEAT                , ; 
             CLR_HOTPINK              , ; 
             CLR_STEELBLUE            , ; 
             CLR_MOCCASIN             , ; 
             CLR_PEACHPUFF            , ; 
             CLR_SILVER               , ; 
             CLR_LIGHTPINK            , ; 
             CLR_BISQUE               , ; 
             CLR_PINK                 , ; 
             CLR_DARKORCHID           , ; 
             CLR_MEDIUMTURQUOISE      , ; 
             CLR_MEDIUMBLUE           , ; 
             CLR_SLATEBLUE            , ; 
             CLR_BLANCHEDALMOND       , ; 
             CLR_LEMONCHIFFON         , ; 
             CLR_TURQUOISE            , ; 
             CLR_DARKTURQUOISE        , ; 
             CLR_LIGHTGOLDENRODYELLOW , ; 
             CLR_DARKVIOLET           , ; 
             CLR_MEDIUMORCHID         , ; 
             CLR_LIGHTGRAY            , ; 
             CLR_AQUAMARINE           , ; 
             CLR_PAPAYAWHIP           , ; 
             CLR_ORCHID               , ; 
             CLR_ANTIQUEWHITE         , ; 
             CLR_THISTLE              , ; 
             CLR_MEDIUMPURPLE         , ; 
             CLR_GAINSBORO            , ; 
             CLR_BEIGE                , ; 
             CLR_CORNSILK             , ; 
             CLR_PLUM                 , ; 
             CLR_LIGHTSTEELBLUE       , ; 
             CLR_LIGHTYELLOW          , ; 
             CLR_ROYALBLUE            , ; 
             CLR_MISTYROSE            , ; 
             CLR_BLUEVIOLET           , ; 
             CLR_LIGHTBLUE            , ; 
             CLR_POWDERBLUE           , ; 
             CLR_LINEN                , ; 
             CLR_OLDLACE              , ; 
             CLR_SKYBLUE              , ; 
             CLR_CORNFLOWERBLUE       , ; 
             CLR_MEDIUMSLATEBLUE      , ; 
             CLR_VIOLET               , ; 
             CLR_PALETURQUOISE        , ; 
             CLR_SEASHELL             , ; 
             CLR_FLORALWHITE          , ; 
             CLR_HONEYDEW             , ; 
             CLR_IVORY                , ; 
             CLR_LAVENDERBLUSH        , ; 
             CLR_WHITESMOKE           , ; 
             CLR_LIGHTSKYBLUE         , ; 
             CLR_LAVENDER             , ; 
             CLR_SNOW                 , ; 
             CLR_MINTCREAM            , ; 
             CLR_BLUE                 , ; 
             CLR_FUCHSIA              , ; 
             CLR_DODGERBLUE           , ; 
             CLR_DEEPSKYBLUE          , ; 
             CLR_ALICEBLUE            , ; 
             CLR_GHOSTWHITE           , ; 
             CLR_CYAN                 , ; 
             CLR_LIGHTCYAN            , ; 
             CLR_AZURE                , ; 
             CLR_WHITE                  ; 
           }

   AEval( aClr, {|nclr| ::oNH:Set( nclr, HMG_ClrToHTML( nclr ) ) } )

   ::nLenN := Len( hb_ntos( ::oNH:Len ) )

RETURN Self

CLASS Tsb2Xml2Xls

  VAR nRow         INIT 1
  VAR nCol         INIT 1
  VAR cFile        INIT "Book.xml"
  VAR oBrw
  VAR oXml 
  VAR cSheet       INIT "Sheet1"
  VAR oSheet
  VAR aForma
  VAR hFormaFont
  VAR nFormaAlign
  VAR nFormaFore 
  VAR nFormaBack 
  VAR aTitle
  VAR hTitleFont
  VAR nTitleAlign
  VAR nTitleFore 
  VAR nTitleBack 
  VAR aBefore
  VAR hBeforeFont
  VAR nBeforeAlign
  VAR nBeforeFore 
  VAR nBeforeBack 
  VAR aAfter 
  VAR hAfterFont
  VAR nAfterAlign
  VAR nAfterFore 
  VAR nAfterBack 
  VAR aLogicalText INIT { "Yes", "No" }
  VAR cDateFormat  INIT "dd.mm.yyyy"
  VAR lActivate    INIT .T.
  VAR nOldRec      INIT 0
  VAR nOldRow      INIT 0
  VAR nOldCol      INIT 0

  VAR oStyles
  VAR oClrN2H      INIT oColorN2H()
  VAR oAlign       INIT oKeyData()
  VAR oBorder      INIT oKeyData()
  VAR oFont        INIT oKeyData()
  VAR oFore        INIT oColorN2H('FC')
  VAR oBack        INIT oColorN2H('BC')
  VAR oData        INIT oKeyData()
  VAR oName        INIT oKeyData()
  VAR oStyl        INIT oKeyData()
  VAR oData        INIT oKeyData()

  VAR nClrSuperHd                    // INIT CLR_HGRAY
  VAR nClrHead     INIT CLR_HGRAY
  VAR nClrFoot     INIT CLR_HGRAY

  VAR cShellExec 
  // cShellExec := "C:\Program Files\OpenOffice 4\program\scalc.exe"
  // cShellExec := "C:\Program Files (x86)\OpenOffice 4\program\scalc.exe"   

  METHOD New( cFile, oBrw, aTitle, aBefore, aAfter, Forma, cSheet )

  METHOD cAlign( nAlign )  INLINE ::oAlign:Get( hb_defaultValue(nAlign, 0) )

  METHOD StyleOut( cName, hFont, nAlign, cFrm, nFore, nBack, nBord )
  METHOD StyleTable()
  METHOD StyleCreate( cName )

  METHOD StyleForma  () 
  METHOD StyleTitle  () 
  METHOD StyleBefore () 
  METHOD StyleAfter  () 
  METHOD StyleSet( cKey, xVal, cPic, nAlign, nBord, hFont, nFore, nBack )

  METHOD WriteForma  () 
  METHOD WriteTitle  () 
  METHOD WriteBefore () 
  METHOD WriteSuperHd() 
  METHOD WriteHeader () 
  METHOD WriteColumns()
  METHOD WriteFooter ()
  METHOD WriteAfter  () 
  METHOD WriteValue( nRow, nCol, xVal, cPic, cNam, nMerge )

  METHOD WriteData()

ENDCLASS
  
METHOD New( cFile, oBrw, aTitle, aBefor, aAfter, aForma, cSheet ) CLASS Tsb2Xml2Xls
   LOCAL i, oCol, nCell
   DEFAULT cFile  := ::cFile, ;
           cSheet := ::cSheet

   ::cFile   := cFile
   ::cSheet  := cSheet
   ::oBrw    := oBrw
   ::oXml    := ExcelWriterXML():New( cFile )
   ::oXml:setOverwriteFile( .T. )
   ::oSheet  := ::oXml:addSheet( cSheet )
   ::oStyles := oKeyData()

   ::oAlign:Set(DT_LEFT  , 'Left'  )
   ::oAlign:Set(DT_CENTER, 'Center')
   ::oAlign:Set(DT_RIGHT , 'Right' )

   AEval(array(5), {|xb,nb| ::oBorder:Set(nb-1, 'B' + hb_ntos(::oBorder:Len)) })

   ::nOldRec := iif( oBrw:lIsDbf, ( oBrw:cAlias )->( RecNo() ), oBrw:nAt )
   ::nOldRow := oBrw:nLogicPos()
   ::nOldCol := oBrw:nCell
   
   If ! empty( aForma )
      If ! HB_ISARRAY( aForma ); aForma := { { aForma } }
      EndIf

      i := Len( aForma )

      ::aForma := aForma[1]
      
      If     i == 2
         ::hFormaFont  := aForma[2]
      ElseIf i == 3
         ::hFormaFont  := aForma[2]
         ::nFormaAlign := aForma[3]
      ElseIf i == 4
         ::hFormaFont  := aForma[2]
         ::nFormaAlign := aForma[3]
         ::nFormaFore  := aForma[4]
      ElseIf i == 5
         ::hFormaFont  := aForma[2]
         ::nFormaAlign := aForma[3]
         ::nFormaFore  := aForma[4]
         ::nFormaBack  := aForma[5]
      EndIf
   EndIf
   
   If ! empty( aTitle )
      If ! HB_ISARRAY( aTitle ); aTitle := { { aTitle } }
      EndIf

      i := Len( aTitle )

      ::aTitle := aTitle[1]
      
      If     i == 2
         ::hTitleFont  := aTitle[2]
      ElseIf i == 3
         ::hTitleFont  := aTitle[2]
         ::nTitleAlign := aTitle[3]
      ElseIf i == 4
         ::hTitleFont  := aTitle[2]
         ::nTitleAlign := aTitle[3]
         ::nTitleFore  := aTitle[4]
      ElseIf i == 5
         ::hTitleFont  := aTitle[2]
         ::nTitleAlign := aTitle[3]
         ::nTitleFore  := aTitle[4]
         ::nTitleBack  := aTitle[5]
      EndIf
   EndIf
   
   If ! empty( aBefor )
      If ! HB_ISARRAY( aBefor ); aBefor := { { aBefor } }
      EndIf

      i := Len( aBefor )
      ::aBefore := aBefor[1]
      If     i == 2
         ::hBeforeFont  := aBefor[2]
      ElseIf i == 3
         ::hBeforeFont  := aBefor[2]
         ::nBeforeAlign := aBefor[3]
      ElseIf i == 4
         ::hBeforeFont  := aBefor[2]
         ::nBeforeAlign := aBefor[3]
         ::nBeforeFore  := aBefor[4]
      ElseIf i == 5
         ::hBeforeFont  := aBefor[2]
         ::nBeforeAlign := aBefor[3]
         ::nBeforeFore  := aBefor[4]
         ::nBeforeBack  := aBefor[5]
      EndIf
   EndIf
   
   If ! empty( aAfter )
      If ! HB_ISARRAY( aAfter ); aAfter := { { aAfter } }
      EndIf

      i := Len( aAfter )
      ::aAfter := aAfter[1]
      If     i == 2
         ::hAfterFont  := aAfter[2]
      ElseIf i == 3
         ::hAfterFont  := aAfter[2]
         ::nAfterAlign := aAfter[3]
      ElseIf i == 4
         ::hAfterFont  := aAfter[2]
         ::nAfterAlign := aAfter[3]
         ::nAfterFore  := aAfter[4]
      ElseIf i == 5
         ::hAfterFont  := aAfter[2]
         ::nAfterAlign := aAfter[3]
         ::nAfterFore  := aAfter[4]
         ::nAfterBack  := aAfter[5]
      EndIf
   EndIf

   WITH OBJECT ::oBrw
   nCell := 0
   FOR i := 1 TO Len( :aColumns )
       oCol := :aColumns[ i ]
       If i == 1 .and. :lSelector; LOOP
       ElseIf ! oCol:lVisible    ; LOOP
       ElseIf oCol:lBitMap       ; LOOP
       EndIf
       If ::hTitleFont == Nil
          ::hTitleFont := :hFontHeadGet( oCol, i )
       EndIf
       nCell++
       ::oSheet:columnWidth( nCell,  oCol:nWidth / 1.3 )
   NEXT
   END WITH

   DEFAULT ::hFormaFont   := oBrw:hFont, ;
           ::nFormaAlign  := DT_CENTER,  ;
           ::hTitleFont   := oBrw:hFont, ;
           ::nTitleAlign  := DT_CENTER,  ;
           ::hBeforeFont  := oBrw:hFont, ;
           ::nBeforeAlign := DT_LEFT,    ;
           ::hAfterFont   := oBrw:hFont, ;
           ::nAfterAlign  := DT_LEFT      

RETURN Self

METHOD StyleOut( cName, hFont, nAlign, cFrm, nFore, nBack, nBord ) CLASS Tsb2Xml2Xls
   LOCAL aFont  := GetFontParam( hFont )
   LOCAL oStyle := ::oXml:addStyle( cName )
  
   oStyle:alignHorizontal( ::cAlign( nAlign    ) )
   oStyle:alignVertical  ( ::cAlign( DT_CENTER ) )
   oStyle:SetfontName( aFont[1] )
   oStyle:SetfontSize( aFont[2] )
      If aFont[3]
   oStyle:setFontBold()         
      EndIf                                     
      If aFont[4]
   oStyle:setFontItalic()       
      EndIf                                     
      If aFont[5]
   oStyle:setFontUnderline()    
      EndIf                                     
      If aFont[6]
   oStyle:setFontStrikethrough()
      EndIf
      If nFore != Nil
   oStyle:setFontColor( ::oFore:Get( nFore ) )
      EndIf
      If nBack != Nil
   oStyle:bgColor     ( ::oBack:Get( nBack ) )  
      EndIf
      If nBord != Nil .and. nBord >= 0 .and. nBord <= ::oBorder:Len
   oStyle:Border( "All", nBord, "Automatic",  "Continuous" )
      EndIf
      If ! empty( cFrm )
   oStyle:setNumberFormat( cFrm )
      EndIf
   oStyle:alignWraptext()

   ::oStyles:Set(cName, oStyle)

RETURN Nil

METHOD StyleForma() CLASS Tsb2Xml2Xls
   LOCAL cName, hFont, nAlign, cFrm, nFore, nBack, nBord
   
   If empty( ::aForma ); RETURN Nil
   EndIf

   cName  := "Forma"
   hFont  := ::hFormaFont
   nAlign := ::nFormaAlign

   ::StyleOut( cName, hFont, nAlign, cFrm, nFore, nBack, nBord )
  
RETURN Nil

METHOD StyleTitle() CLASS Tsb2Xml2Xls
   LOCAL cName, hFont, nAlign, cFrm, nFore, nBack, nBord

   If empty( ::aTitle ); RETURN Nil
   EndIf

   cName  := "Title"
   hFont  := ::hTitleFont
   nAlign := ::nTitleAlign

   ::StyleOut( cName, hFont, nAlign, cFrm, nFore, nBack, nBord )
  
RETURN Nil

METHOD StyleBefore() CLASS Tsb2Xml2Xls
   LOCAL cName, hFont, nAlign, cFrm, nFore, nBack, nBord

   If empty( ::aBefore ); RETURN Nil
   EndIf               

   cName  := "Before"
   hFont  := ::hBeforeFont
   nAlign := ::nBeforeAlign

   ::StyleOut( cName, hFont, nAlign, cFrm, nFore, nBack, nBord )
   
RETURN Nil

METHOD StyleAfter() CLASS Tsb2Xml2Xls
   LOCAL cName, hFont, nAlign, cFrm, nFore, nBack, nBord

   If empty( ::aAfter ); RETURN Nil
   EndIf

   cName  := "After"
   hFont  := ::hAfterFont
   nAlign := ::nAfterAlign

   ::StyleOut( cName, hFont, nAlign, cFrm, nFore, nBack, nBord )

RETURN Nil

METHOD WriteForma  ()  CLASS Tsb2Xml2Xls
   LOCAL aForma := ::aForma, i
   LOCAL nRow  := ::nRow, nLen
   LOCAL cName := "Forma"

   If HB_ISCHAR( aForma ); aForma := { aForma }
   EndIf

   If ! HB_ISARRAY( aForma ) .or. empty( ::oStyles:Get(cName) )
      RETURN Nil
   EndIf

   nLen := Len( ::oBrw:aColumns )

   FOR i := 1 TO Len( aForma )
       ::WriteValue(nRow + i - 1, 1, aForma[ i ], , cName, nLen - 1) 
       ::nRow++
   NEXT

RETURN Nil

METHOD WriteTitle  ()  CLASS Tsb2Xml2Xls
   LOCAL aTitle := ::aTitle, i
   LOCAL nRow  := ::nRow, nLen
   LOCAL cName := "Title"

   If HB_ISCHAR( aTitle ); aTitle := { aTitle }
   EndIf

   If ! HB_ISARRAY( aTitle ) .or. empty( ::oStyles:Get(cName) )
      RETURN Nil
   EndIf

   nLen := Len( ::oBrw:aColumns )

   FOR i := 1 TO Len( aTitle )
       ::WriteValue(nRow + i - 1, 1, aTitle[ i ], , cName, nLen - 1) 
       ::nRow++
   NEXT

RETURN Nil

METHOD WriteBefore ()  CLASS Tsb2Xml2Xls
   LOCAL aBefor := ::aBefore, i
   LOCAL nRow  := ::nRow, nLen
   LOCAL cName := "Before"

   If HB_ISCHAR( aBefor ); aBefor := { aBefor }
   EndIf

   If ! HB_ISARRAY( aBefor ) .or. empty( ::oStyles:Get(cName) )
      RETURN Nil
   EndIf

   nLen := Len( ::oBrw:aColumns )

   FOR i := 1 TO Len( aBefor )
       ::WriteValue(nRow + i - 1, 1, aBefor[ i ], , cName, nLen - 1) 
       ::nRow++
   NEXT

RETURN Nil

METHOD WriteAfter  ()  CLASS Tsb2Xml2Xls
   LOCAL aAfter := ::aAfter, i
   LOCAL nRow  := ::nRow, nLen
   LOCAL cName := "After"

   If HB_ISCHAR( aAfter ); aAfter := { aAfter }
   EndIf

   If ! HB_ISARRAY( aAfter ) .or. empty( ::oStyles:Get(cName) )
      RETURN Nil
   EndIf

   nLen := Len( ::oBrw:aColumns )

   FOR i := 1 TO Len( aAfter )
       ::WriteValue(nRow + i - 1, 1, aAfter[ i ], , cName, nLen - 1) 
       ::nRow++
   NEXT

RETURN Nil

METHOD WriteSuperHd()  CLASS Tsb2Xml2Xls
   LOCAL a, b, c, i, k, n, t, s
   LOCAL nRow  := ::nRow
   LOCAL cPref := "SH"

   WITH OBJECT ::oBrw
   FOR i := 1 TO Len( :aSuperHead )
       s := ::oStyl:Get('S' + '.' + hb_ntos(i))
       If empty(s); LOOP
       EndIf
       t := :cTextSupHdGet( i )
       n := :aSuperHead[ i ][1]
       k := :aSuperHead[ i ][2]
       ::WriteValue( nRow, n, t, , s, k - n )
   NEXT 
   ::oSheet:cellHeight ( nRow, 1, :nHeightSuper )
   ::nRow++
   END WITH

RETURN Nil

METHOD WriteHeader ()  CLASS Tsb2Xml2Xls
   LOCAL nRow  := ::nRow, a, c, o, i, t, s
   LOCAL cPref := "H", nCell

   WITH OBJECT ::oBrw
   nCell := 0
   FOR i := 1 TO Len( :aColumns )
       o := :aColumns[ i ]
       s := ::oStyl:Get('H' + '.' + hb_ntos(i))
       If empty( s ); LOOP
       EndIf
       nCell++
       t := :GetValProp( o:cHeading, "", i )
       ::WriteValue( nRow, nCell, t, , s )
   NEXT 
   ::oSheet:cellHeight( nRow, 1, :nHeightHead )
   ::nRow++
   END WITH

RETURN Nil

METHOD WriteColumns()  CLASS Tsb2Xml2Xls
   LOCAL nRow := ::nRow
   LOCAL cPic, nLine, nLen, xVal, cNam, nCol, oCol, nCell

   WITH OBJECT ::oBrw
   Eval( :bGoTop )
   nLen  := :nLen
   nLine := 1
   DO WHILE  nLine <= nLen
      ::oSheet:cellHeight( nRow, 1, :nHeightCell / 1.3 )
      nCell := 0
      FOR nCol := 1 TO Len( :aColumns )
          oCol := :aColumns[ nCol ]
          cNam := ::oStyl:Get(hb_ntos(nLine) + '.' + hb_ntos(nCol))
          If empty( cNam ); LOOP
          EndIf
          nCell++
          xVal := :bDataEval( oCol, , nCol )
          cPic := :cPictureGet( oCol, nCol )
          ::WriteValue( nRow, nCell, xVal, cPic, cNam )
      NEXT
      :Skip(1)
      nLine++
      nRow++
      SysRefresh()
   ENDDO
   ::nRow := nRow
   END WITH
   
RETURN Nil

METHOD WriteFooter ()  CLASS Tsb2Xml2Xls
   LOCAL a, c, o, i, t, s
   LOCAL nRow  := ::nRow
   LOCAL cPref := "F", nCell 

   WITH OBJECT ::oBrw
   nCell := 0
   FOR i := 1 TO Len( :aColumns )
       o := :aColumns[ i ]
       s := ::oStyl:Get('F' + '.' + hb_ntos(i))
       If empty( s ); LOOP
       EndIf
       nCell++
       t := :GetValProp( o:cFooting, "", i )
       ::WriteValue( nRow, nCell, t, , s )
   NEXT 
   ::oSheet:cellHeight( nRow, 1, :nHeightFoot )
   ::nRow++
   END WITH
   
RETURN Nil

METHOD WriteValue( nRow, nCol, xVal, cPic, cNameStyle, nMerge )  CLASS Tsb2Xml2Xls
   LOCAL cTyp := ValType( xVal ), cName, aVal
   
   If   ! empty( ::oStyles:Get(cNameStyle) )
      cName := cNameStyle
   ElseIf empty( aVal := ::oName:Get( cNameStyle ) )
      cName := ::oStyl:Get( cNameStyle )
   Else
      cName := cNameStyle
   EndIf

   If     cTyp == 'N'
      If ! empty(xVal) .and. ! empty(cPic)
         xVal := val( Transform( xVal, cPic ) )
      EndIf
      ::oSheet:writeNumber  ( nRow, nCol, xVal, cName )
   ElseIf cTyp == 'C'
      If ! empty(xVal) .and. ! empty(cPic)
         xVal := Transform( xVal, cPic )
      EndIf
      xVal := StrTran( xVal, CRLF, "&#10;" )
      ::oSheet:writeString  ( nRow, nCol, xVal, cName )
   ElseIf cTyp == 'D'
      xVal := hb_DtoC(xVal, ::cDateFormat)
      ::oSheet:writeDateTime( nRow, nCol, xVal, cName )
   ElseIf cTyp == 'L'
      xVal := ::aLogicalText [ iif( xVal, 1, 2 ) ]
      ::oSheet:writeString  ( nRow, nCol, xVal, cName )
   ElseIf cTyp == 'U'
      xVal := ''
      ::oSheet:writeString  ( nRow, nCol, xVal, cName )
   ElseIf cTyp == 'T'
      xVal := HB_TToC( xVal )
      ::oSheet:writeString  ( nRow, nCol, xVal, cName )
   EndIf

   If HB_ISNUMERIC( nMerge )
      ::oSheet:cellMerge( nRow, nCol, nMerge, 0 )
   EndIf
   
RETURN Nil

METHOD WriteData()  CLASS Tsb2Xml2Xls
   LOCAL cFile := ::cFile, cPth, cFil, cExt, n := 0
   LOCAL cExec := ::cShellExec

   If empty(cExec); cExec := 'Excel'
   EndIf

   hb_FNameSplit(cFile, @cPth, @cFil, @cExt)

   If hb_FileExists(hb_FNameMerge(cPth, cFil, cExt))
      DO WHILE hb_FileExists(hb_FNameMerge(cPth, cFil+" ("+hb_ntos( ++n )+")", cExt))
      ENDDO
      cFile := hb_FNameMerge(cPth, cFil+" ("+hb_ntos( n )+")", cExt)
   EndIf

   WITH OBJECT ::oBrw
   ::cFile := cFile
   :GotoRec(::nOldRec)
   :GoPos( ::nOldRow, ::nOldCol )
   ::oXml:writeData( cFile )
   If ::lActivate
      hb_memowrit('_e_.cmd', '@Start '+cExec+' ".\'+cFile+'"'+CRLF) // Temp line
      RUN '_e_.cmd'                                                 // Temp line
      InkeyGui(1000)                                                // Temp line
      fErase('_e_.cmd')                                             // Temp line
//      ShellExecute( 0, "Open", cFile,,, 3 )
   EndIf
   :Display()
   END WITH
   
RETURN Nil

METHOD StyleTable()  CLASS Tsb2Xml2Xls
   LOCAL nLine, cLine, nSkip, nAlign, i, xVal
   LOCAL nCol, oCol, nCnt, cCol, cTyp, cDec, cPic, cFrm
   LOCAL oXml  := ::oXml, cAlg, hFont
   LOCAL oFont := ::oFont, cFont, cBor, nBor
   LOCAL oFore := ::oFore, cFore, nFore
   LOCAl oBack := ::oBack, cBack, nBack
   LOCAL oName := ::oName, cName
   
   WITH OBJECT ::oBrw
   // selected fonts. SuperHead
   FOR nCol := 1 TO Len( :aSuperHead )
       hFont := :hFontSupHdGet( nCol )
       oFont:Set(hFont, hFont)
   NEXT
   // selected fonts. Header footer
   FOR nCol := 1  To Len( :aColumns )
       oCol := :aColumns[ nCol ]
       If nCol == 1 .and. :lSelector; LOOP
       ElseIf ! oCol:lVisible       ; LOOP
       ElseIf oCol:lBitMap          ; LOOP
       EndIf
       hFont := :hFontHeadGet( oCol, nCol )
       oFont:Set(hFont, hFont)
       hFont := :hFontFootGet( oCol, nCol )
       oFont:Set(hFont, hFont)
   NEXT
   // selected fonts. All cell
   Eval( :bGoTop )
   nCnt  := :nLen
   nLine := 1 
   DO WHILE nLine <= nCnt
       cLine := hb_ntos(nLine)
       FOR nCol := 1  To Len( :aColumns )
           oCol := :aColumns[ nCol ]
           If nCol == 1 .and. :lSelector; LOOP
           ElseIf ! oCol:lVisible       ; LOOP
           ElseIf oCol:lBitMap          ; LOOP
           EndIf
           hFont := :hFontGet( oCol, nCol )
           oFont:Set(hFont, hFont)
       NEXT
       nSkip := :Skip(1)
       nLine++
       SysRefresh()
       IF nSkip == 0
          EXIT
       ENDIF
   ENDDO
   // create virtual style
   cTyp := 'C'
   cDec := '0'
   cFrm := ''
   nBor := 2
   cBor := 'B' + hb_ntos(nBor)
   // SuperHead
   FOR nCol := 1 TO Len( :aSuperHead )
       cCol   := hb_ntos(nCol)
       hFont  := :hFontSupHdGet( nCol )
       cFont  := 'FN' + hb_ntos(oFont:Pos(hFont))
       nFore  := :nForeSupHdGet( nCol )
       cFore  := oFore:Name(nFore)
       nBack  := :nBackSupHdGet( nCol, 2 )
       cBack  := oBack:Name(nBack)
       nAlign := :nAlignSupHdGet( nCol )
       cAlg   := 'A' + hb_ntos(nAlign)
       cName  := cFont + cFore + cBack + cAlg + cBor + cTyp + cDec
       ::oStyl:Set('S' + '.' + cCol, cName)
       oName:Set(cName, { hFont, nFore, nBack, nAlign, nBor, cTyp, cFrm })
   NEXT
   // header and footer
   FOR nCol := 1  To Len( :aColumns )
       oCol := :aColumns[ nCol ]
       If nCol == 1 .and. :lSelector; LOOP
       ElseIf ! oCol:lVisible       ; LOOP
       ElseIf oCol:lBitMap          ; LOOP
       EndIf
       cCol  := hb_ntos(nCol)
       // header
       hFont  := :hFontHeadGet( oCol, nCol )
       cFont  := 'FN' + hb_ntos(oFont:Pos(hFont))
       nFore  := :nColorGet( oCol:nClrHeadFore, nCol )
       cFore  := oFore:Name(nFore)
       nBack  := :nColorGet( oCol:nClrHeadBack, nCol, Nil, .F. )
       cBack  := oBack:Name(nBack)
       nAlign := :nAlignGet( oCol:nHAlign, nCol, DT_CENTER )
       cAlg   := 'A' + hb_ntos(nAlign)
       cName  := cFont + cFore + cBack + cAlg + cBor + cTyp + cDec
       ::oStyl:Set('H' + '.' + cCol, cName)
       oName:Set(cName, { hFont, nFore, nBack, nAlign, nBor, cTyp, cFrm })
       // footer
       hFont  := :hFontFootGet( oCol, nCol )
       nFore  := :nColorGet( oCol:nClrFootFore, nCol )
       cFore  := oFore:Name(nFore)
       nBack  := :nColorGet( oCol:nClrFootBack, nCol, Nil, .F. )
       cBack  := oBack:Name(nBack)
       nAlign := :nAlignGet( oCol:nFAlign, nCol, DT_CENTER )
       cAlg   := 'A' + hb_ntos(nAlign)
       cName  := cFont + cFore + cBack + cAlg + cBor + cTyp + cDec
       ::oStyl:Set('F' + '.' + hb_ntos(nCol), cName)
       oName:Set(cName, { hFont, nFore, nBack, nAlign, nBor, cTyp, cFrm })
   NEXT
   // all cell
   Eval( :bGoTop )
   nCnt  := :nLen
   nLine := 1 
   DO WHILE nLine <= nCnt
       cLine := hb_ntos(nLine)
       FOR nCol := 1  To Len( :aColumns )
           oCol := :aColumns[ nCol ]
           If nCol == 1 .and. :lSelector; LOOP
           ElseIf ! oCol:lVisible       ; LOOP
           ElseIf oCol:lBitMap          ; LOOP
           EndIf
           cCol  := hb_ntos(nCol)
           hFont := :hFontGet( oCol, nCol )
           cFont := 'FN' + hb_ntos(oFont:Pos(hFont))
           nFore := :nColorGet( oCol:nClrFore, nCol, :nAt )
           cFore := oFore:Name(nFore)
           nBack := :nColorGet( oCol:nClrBack, nCol, :nAt, .F.)
           cBack := oBack:Name(nBack)
           nAlign := :nAlignGet( oCol:nAlign , nCol, DT_LEFT )
           xVal := :bDataEval( oCol, , nCol )
           cTyp := ValType( xVal )
           cDec := '0'
           cFrm := ''
           nBor := 1
           cBor := 'B' + hb_ntos(nBor)
           cAlg := 'A' + hb_ntos(nAlign)
           If cTyp == 'N'
              cPic := :cPictureGet( oCol, nCol )
              If empty(cPic); cPic := hb_ntos(xVal)
              EndIf
              cFrm := "#,##0"
              If ( i := RAt('.', cPic) ) > 0
                 i := Len(cPic) - i
                 cFrm += "." + Replicate('0', i)
                 cDec := hb_ntos( i )
              EndIf
           ElseIf cTyp == 'D'
              cFrm := ::cDateFormat
           EndIf
           cName := cFont + cFore + cBack + cAlg + cBor + cTyp + cDec
           ::oStyl:Set(cLine + '.' + cCol, cName)
           oName:Set(cName, { hFont, nFore, nBack, nAlign, nBor, cTyp, cFrm })
       NEXT
       nSkip := :Skip(1)
       nLine++
       SysRefresh()
       IF nSkip == 0
          EXIT
       ENDIF
   ENDDO

   :GotoRec(::nOldRec)
   :GoPos( ::nOldRow, ::nOldCol )
   :Display()
   
   END WITH
   
   ::StyleCreate()
  
RETURN Nil

METHOD StyleCreate( cKey )  CLASS Tsb2Xml2Xls
   LOCAL oStyle, aName, cName, aNam
   LOCAL aFont, hFont, nFore, nBack, nAlign, nBord, cTyp, cFrm, a

   If empty( cKey )
      aNam  := ::oName:GetAll( .F. )
   Else
      cName := ::oStyl:Get( cKey  )
      aName := ::oName:Get( cName )
      aNam  := { { cName, aName } }
   EndIf

   FOR EACH a IN aNam

       cName  := a[1]
       aName  := a[2]

       hFont  := aName[1]
       nFore  := aName[2]
       nBack  := aName[3]
       nAlign := aName[4]
       nBord  := aName[5]
       cTyp   := aName[6]
       cFrm   := aName[7]
       aFont  := GetFontParam( hFont )
  
       oStyle := ::oXml:addStyle( cName )
  
       oStyle:alignHorizontal( ::cAlign( nAlign    ) )
       oStyle:alignVertical  ( ::cAlign( DT_CENTER ) )
       oStyle:SetfontName( aFont[1] )
       oStyle:SetfontSize( aFont[2] )
          If aFont[3]
       oStyle:setFontBold()         
          EndIf                                     
          If aFont[4]
       oStyle:setFontItalic()       
          EndIf                                     
          If aFont[5]
       oStyle:setFontUnderline()    
          EndIf                                     
          If aFont[6]
       oStyle:setFontStrikethrough()
          EndIf
       oStyle:setFontColor( ::oFore:Get( nFore ) )
       oStyle:bgColor     ( ::oBack:Get( nBack ) )  
          If nBord >= 0 .and. nBord <= ::oBorder:Len
       oStyle:Border( "All", nBord, "Automatic",  "Continuous" )
          EndIf
          If ! empty( cFrm )
       oStyle:setNumberFormat( cFrm )
          EndIf
       oStyle:alignWraptext()

   NEXT

RETURN oStyle

METHOD StyleSet( cKey, xVal, cPic, nAlign, nBord, hFont, nFore, nBack ) CLASS Tsb2Xml2Xls
   LOCAL cName, nFont, cFont, cFore, cBack, cBord, cAlign, i
   LOCAL cTyp, cFrm, cDec
   DEFAULT hFont  := ::oBrw:hFont,    ;
           nFore  := ::oBrw:nClrText, ;
           nBack  := ::oBrw:nClrPane, ;
           nAlign := DT_LEFT,         ;
           nBord  :=  -1

   If empty(::oFont:Pos( hFont ))
      ::oFont:Set( hFont, hFont )
   EndIf

   nFont  := ::oFont:Pos( hFont )
   cFont  := 'FN' + hb_ntos( nFont )
   cFore  := ::oFore:Name( nFore )
   cBack  := ::oBack:Name( nBack )
   cAlign := 'A' + hb_ntos( nAlign )
   cBord  := 'B' + hb_ntos( nBord )
   cTyp   := ValType( xVal )
   cFrm   := ''
   cDec   := '0'

   If cTyp == 'N'
      If empty(cPic); cPic := hb_ntos(xVal)
      EndIf
      cFrm := "#,##0"
      If ( i := RAt('.', cPic) ) > 0
         i := Len(cPic) - i
         cFrm += "." + Replicate('0', i)
         cDec := hb_ntos( i )
      EndIf
   ElseIf cTyp == 'D'
      cFrm := ::cDateFormat
   EndIf

   cName := cFont + cFore + cBack + cAlign + cBord + cTyp + cDec

   ::oStyl:Set( cKey , cName )
   ::oName:Set( cName, { hFont, nFore, nBack, nAlign, nBord, cTyp, cFrm } )
   
RETURN cName
