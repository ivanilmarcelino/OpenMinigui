#include "hmg.ch"

// ---------------------------------------------------------
// ---------------------------------------------------------
// --> FUNCION GENERICA PARA MOSTRAR UN CALENDARIO OPERATIVO
// ---------------------------------------------------------
// --> Calendari(Ventana,nFil,ncol,dFecha,lDia,cColorNoMes,cColorMes,cColorDia,cColorDom,cColorInv,cColorFondo,xFuncion)
// --> PARAMETROS
// --> Ventana   -> Window propietaria
// --> nFil, nCol -> Fila y Columna donde mostraremos el calendario
// --> PARAMETROS OPCIONALES
// --> dFecha   -> Opcional DATE()
// --> lDia    -> Opcional .T. se mostrará dia .f. no se mostrara
// --> cColorNoMes -> Opcional, color del mes pasado o mes siguiente.
// --> cColorMes  -> Opcional, color del mes de la fecha
// --> cColorDia  -> Opcional, color del día
// --> cColorDom  -> Opcional, color del Domingo
// --> cColorInv  -> Opcional, color Inverso
// --> cColorFondo -> Opcional, Color Fondo
// --> xFuncion  -> Opcional, Si queremos que ejecute algo

// ----------------------------------------------------------------------------
// 02 DE NOVIEMBRE DEL 2018 - AÑADIMOS ZEROS A UN NUMERO TRASFORMADO EN CADENA
// ----------------------------------------------------------------------------
// --> Transforma un numero en una cadena y rellena con ceros
// --> cStrZero( <nNum>, [<nLen>] )
// --> Parametros  <nNum> es el número a transformar en cadena.
// -->     <nLen> longitud máxima
// --> Return  Número a cadena relleno de ceros
// ----------------------------------------------------------------------------
// 05 DE NOVIEMBRE DEL 2018 - SE INCLUYE EL DEFINE PARA PODER INCLUIRLO EN ".CH"
// ----------------------------------------------------------------------------

FUNCTION Calendari( Ventana, nFil, ncol, dFecha, lDia, cColorNoMes, cColorMes, cColorDia, cColorDom, cColorInv, cColorFondo, xFuncion )

   LOCAL nWeek, nDay, cTB, bFunc
   LOCAL aDia := { "Lu", "Ma", "Mi", "Ju", "Vi", "Sa", "Do" }

   DEFAULT cColorFondo := { 204, 204, 224 }
   DEFAULT nFil := nCol := 1
   DEFAULT dFecha := Date()
   DEFAULT cColorNomes := GRAY
   DEFAULT cColorMes := BLUE
   DEFAULT ccolorDia := BLACK
   DEFAULT cColorDom := RED
   DEFAULT cColorInv := COLOR_SkyBlue
   DEFAULT lDia := .F.
   DEFAULT xFuncion := ""

   IF ! Empty( xFuncion )
      bFunc := xFuncion
   ENDIF

   DRAW BOX ;
      IN WINDOW &Ventana ;
      AT nFil-8,47 ;
      TO nFil-18+222 , 47+6+214

   @ nFil-7,48 LABEL Label_Fondo VALUE "" WIDTH 4+214 HEIGHT 222-12 BACKCOLOR cColorFondo

   // -----> Fondo cabezera color Verde Intenso

   @ 085, 050 LABEL Label_000 VALUE "" WIDTH 215 HEIGHT 029 BACKCOLOR { 036, 067, 009 }
   // ---------------------------------------------------------------------------------

   @ nFil, ncol LABEL LBTras VALUE "<<" WIDTH 30 CENTERALIGN BOLD FONTCOLOR { 235, 241, 053 } BACKCOLOR { 036, 067, 009 } ;
      ACTION ( DoMethod( Ventana, "HOY", "SETFOCUS" ), dFecha := Restames( dFecha ), Carga_cal( Ventana, dFEcha, cColorNoMes, cColorMes, cColorDia, cColorDom, cColorInv, cColorFondo ), Proxima_Fecha( Ventana, dFecha, lDia ), ;
      IF( ! Empty( xFuncion ), Eval( bFunc ), nil ) )

   @ nFIL, ncol + 50 LABEL Fecha1 VALUE dFecha WIDTH 70 FONTCOLOR { 235, 241, 053 } BACKCOLOR { 036, 067, 009 } CENTERALIGN

   @ nFil, ncol + 140 LABEL LBAdel VALUE ">>" WIDTH 30 CENTERALIGN BOLD FONTCOLOR { 235, 241, 053 } BACKCOLOR { 036, 067, 009 } ;
      ACTION ( DoMethod( Ventana, "HOY", "SETFOCUS" ), dFecha := Sumames( dFecha ), Carga_cal( Ventana, dFEcha, cColorNoMes, cColorMes, cColorDia, cColorDom, cColorInv, cColorFondo ), Proxima_Fecha( Ventana, dFecha, lDia ), ;
      IF( ! Empty( xFuncion ), Eval( bFunc ), nil ) )

   nFil += 60
   FOR nWeek = 1 TO 6
      FOR nDay = 1 TO 7
         // --> Pongo los nombres de los dias de la semana
         IF nWeek = 1
            cTb = "cTbd_" + cStrZero( nWeek, 1 ) + cStrZero( nDay, 1 )
            @ nFil - 30, nCol + ( 25 * ( nDay - 1 ) ) LABEL &cTb CENTERALIGN VALUE aDia[ nDay ] WIDTH 20 FONTCOLOR BLUE BACKCOLOR cColorFondo
         ENDIF
         // --> Por los dias del mes
         cTb = "cTb_" + cStrZero( nWeek, 1 ) + cStrZero( nDay, 1 )
         @ nFil + ( 20 * ( nWeek - 1 ) ), nCol + ( 25 * ( nDay - 1 ) ) LABEL &cTb CENTERALIGN VALUE " " WIDTH 20 HEIGHT 16 FONTCOLOR BLUE BACKCOLOR cColorFondo ;
            ACTION ( Cambia_dia( Ventana, This.VALUE, @dFecha, lDia, cColorNoMes, cColorMes, cColorDia, cColorDom, cColorInv, cColorFondo ), Proxima_Fecha( Ventana, dFecha, lDia ), DoMethod( Ventana, "HOY", "SETFOCUS" ), ;
            IF( ! Empty( xFuncion ), Eval( bFunc ), nil ) )
      NEXT
   NEXT

   @ nFil + ( 20 * ( nWeek - 1 ) ), ncol + 50 LABEL dhoy VALUE "  HOY  " WIDTH 70 HEIGHT 16 FONTCOLOR { 235, 241, 053 } BACKCOLOR { 036, 067, 009 } CENTERALIGN ;
      ACTION ( DoMethod( Ventana, "HOY", "SETFOCUS" ), dFecha := Date(), Carga_cal( Ventana, dFEcha, cColorNoMes, cColorMes, cColorDia, cColorDom, cColorInv, cColorFondo ), Proxima_Fecha( Ventana, dFecha, lDia ), ;
      IF( ! Empty( xFuncion ), Eval( bFunc ), nil ) )

   @ nFil + 170, 14 BUTTON ANTERIOR ;
      CAPTION "&Previous" ;
      PICTURE "Resource\btn_02.bmp" ;
      WIDTH 96 ;
      HEIGHT 34 ;
      ACTION ( dFecha := Restames( dFecha ), Carga_cal( Ventana, dFEcha, cColorNoMes, cColorMes, cColorDia, cColorDom, cColorInv, cColorFondo ), Proxima_Fecha( Ventana, dFecha, lDia ), IF( ! Empty( xFuncion ), Eval( bFunc ), nil ) ) ;
      TOOLTIP 'Previous' ;
      FONT "ARIAL" SIZE 09 BOLD ITALIC ;
      LEFT

   @ nFil + 170, 110 BUTTON HOY ;
      CAPTION "&Today" ;
      PICTURE "Resource\Today.bmp" ;
      WIDTH 96 ;
      HEIGHT 34 ;
      ACTION ( dFecha := Date(), Carga_cal( Ventana, dFEcha, cColorNoMes, cColorMes, cColorDia, cColorDom, cColorInv, cColorFondo ), Proxima_Fecha( Ventana, dFecha, lDia ), IF( ! Empty( xFuncion ), Eval( bFunc ), nil ) ) ;
      TOOLTIP 'Today' ;
      FONT "ARIAL" SIZE 09 BOLD ITALIC ;
      LEFT

   @ nFil + 170, 206 BUTTON POSTERIOR ;
      CAPTION "&Next" ;
      PICTURE "Resource\btn_03.bmp" ;
      WIDTH 96 ;
      HEIGHT 34 ;
      ACTION ( dFecha := Sumames( dFecha ), Carga_cal( Ventana, dFEcha, cColorNoMes, cColorMes, cColorDia, cColorDom, cColorInv, cColorFondo ), Proxima_Fecha( Ventana, dFecha, lDia ), IF( ! Empty( xFuncion ), Eval( bFunc ), nil ) ) ;
      TOOLTIP 'Next' ;
      FONT "ARIAL" SIZE 09 BOLD ITALIC ;
      LEFT

   IF lDia
      // --> Ahora ponemos la fecha, si es que toca.
      DEFINE IMAGE Fondo_10w
         ROW 245 ; COL 386 ; WIDTH 156 ; HEIGHT 163
         PICTURE 'resource\Calen00x.jpg'
         STRETCH .T.
      END IMAGE

   // ----------  Day ---------------------------------------------------------
   @ 282, 415 LABEL Label_02qA VALUE "" ;
      FONT "Arial" SIZE 65 BOLD ITALIC FONTCOLOR { 000, 000, 066 } ;
      AUTOSIZE
   // ----------  Year --------------------------------------------------------
   @ 270, 494 LABEL Label_01zA VALUE "" ;
      FONT "Arial" SIZE 09 BOLD ITALIC FONTCOLOR { 000, 000, 066 } ;
      AUTOSIZE
   // --------- Month ---------------------------------------------------------
   @ 270, 404 LABEL Label_12zA VALUE "" ;
      FONT "Arial" SIZE 09 BOLD ITALIC FONTCOLOR { 000, 000, 066 } ;
      AUTOSIZE
   // --------- Day Date ------------------------------------------------------
   @ 374, 435 LABEL Label_13zA VALUE "" ;
      FONT "Arial" SIZE 12 BOLD ITALIC FONTCOLOR { 000, 000, 066 } ;
      AUTOSIZE

   ENDIF

   CARGA_CAL( Ventana, dFecha, cColorNoMes, cColorMes, cColorDia, cColorDom, cColorInv, cColorFondo )

   Proxima_Fecha( Ventana, dFecha, lDia )

RETURN dFecha


STATIC PROCEDURE CARGA_CAL( Ventana, dFecha, cColorNoMes, cColorMes, cColorDia, cColorDom, cColorInv, cColorFondo )

   LOCAL cTB
   LOCAL dBoM, dStart
   LOCAL nWeek, nDay

   DEFAULT cColorFondo := { 204, 204, 224 }
   DEFAULT dFecha := Date()
   DEFAULT cColorNomes := GRAY
   DEFAULT cColorMes := BLUE
   DEFAULT ccolorDia := BLACK
   DEFAULT cColorDom := RED
   DEFAULT cColorInv := COLOR_SkyBlue

   dBoM = dFecha - Day( dFecha ) + 1
   dStart = If( DoW( dBoM ) != 1, dBoM - DoW( dBoM ) + 2, dBoM - 6 )

   SETPROPERTY( Ventana, "Fecha1", "Value", dFecha )

   FOR nWeek = 1 TO 6
      FOR nDay = 1 TO 7
         cTb = "cTb_" + cStrZero( nWeek, 1 ) + cStrZero( nDay, 1 )
         SETPROPERTY( Ventana, cTb, "Value", cStrZero( Day( dStart ), 2 ) )
         SETPROPERTY( Ventana, cTb, "BACKCOLOR", cColorFondo )
         SETPROPERTY( Ventana, cTb, "FONTCOLOR", IF( Month( dStart ) == Month( dFecha ), If( dStart == dFecha, cColorDia, cColorMes ), cColorNomes ) )
         IF nDay == 7 .AND. Month( dStart ) == Month( dFecha ) .AND. dStart != dFecha // --> DOMINGOOOOL
            SETPROPERTY( Ventana, ctb, "FONTCOLOR", cColorDom )
         ENDIF
         IF dStart == dFecha
            SETPROPERTY( Ventana, cTb, "BACKCOLOR", cColorInv )
         ENDIF
         dStart++
      NEXT
   NEXT

RETURN

// ------------------------------------------------------------------------------------

STATIC PROCEDURE Cambia_DIA( Ventana, cDia, dFecha, lDia, cColorNoMes, cColorMes, cColorDia, cColorDom, cColorInv, cColorFondo )

   LOCAL nDif, nMes
   LOCAL i := GetLastActiveControlIndex ()
   LOCAL aColor

   nMes := Month( dFecha )

   // --> Compruebo si he tocado el mes anterior o el siguiente
   aColor := GETPROPERTY( Ventana, _HMG_aControlNames[ i ], "FONTCOLOR" )

   IF aColor[ 1 ] = cColorNoMes[ 1 ] .AND. aColor[ 2 ] = cColorNomes[ 2 ] .AND. aColor[ 3 ] = cColorNomes[ 3 ]
      IF Val( cDia ) > 20
         dFecha := RestaMes( dFecha )
      ELSE
         WHILE Day( dFecha ) != Val( cDia ) .OR. Month( dFecha ) = nMes
            dFecha++
         ENDDO
      ENDIF
   ENDIF
   // --> Voy a poner el dia que corresponde
   IF Val( cDia ) > Day( dFecha )
      nDif := Val( cDia ) - Day( dFecha )
      dFecha := dFecha + nDif
   ELSE
      nDif := Day( dFecha ) - Val( cDia )
      dFecha := dFecha - nDif
   ENDIF

   Carga_cal( Ventana, @dFEcha, cColorNoMes, cColorMes, cColorDia, cColorDom, cColorInv, cColorFondo )

RETURN

// ----------------------------------------------------------------------------

STATIC FUNCTION SumaMes( dFecha )

   LOCAL dTemp := dFecha
   LOCAL nMonth := Month( dFecha )

   WHILE Month( dTemp++ ) == nMonth
   ENDDO

RETURN --dTemp + Day( dFecha ) - 1

// ----------------------------------------------------------------------------

STATIC FUNCTION RestaMes( dFecha )

   LOCAL nDay := Day( dFecha )

   dFecha -= Day( dFecha )
   dFecha -= Day( dFecha )

RETURN dFecha + nDay

// ----------------------------------------------------------------------------
// --> Transforma un numero en una cadena y rellena con ceros
// --> cStrZero( <nNum>, [<nLen>] )
// --> Parameteros  <nNum> es el número a transformar en cadena.
// -->     <nLen> longitud máxima
// --> Return  Número a cadena relleno de ceros
FUNCTION cStrZero( nNum, nLen )

   LOCAL cSal, nSigno, nDe

   IF nNum < 0
      nSigno := -1
      nNum := nNum * ( -1 )
   ELSE
      nSigno := 1
   ENDIF

   nDe = At( ".", Str( nNum ) )
   IF nDe != 0
      nDe := Len( Str( nNum ) ) - nDe
   ENDIF

   IF nLen == NIL
      cSal := StrTran( Str( nNum ), " ", "0" )
   ELSE
      cSal := StrTran( Str( nNum, nLen, nDe ), " ", "0" )
   ENDIF
   IF nSigno = -1
      cSal := "-" + SubStr( cSal, 2 )
   ENDIF

RETURN cSal

// ---------------------------------------------
PROCEDURE Proxima_Fecha( Ventana, dFecha, lDia )
// ---------------------------------------------
   LOCAL ProximaDate, ProvaxDiaZ, DprovaxDiaZ
   LOCAL ProxiNomDia, ProvaxDiaX, DprovaxDiaX
   LOCAL ProximoMes, ProvaxDiaA, DprovaxDiaA
   LOCAL Atox, Dtox

   IF lDia
      ProximaDate := dFecha // <--- DATE()
      ProxiNomDia := hb_OEMToANSI( CDoW( ProximaDate ) )
      ProximoMes := CMonth( ProximaDate )

      ProvaxDiaZ := SubStr( DToC( ProximaDate ), 1, 2 )
      ProvaxDiaX := SubStr( DToC( ProximaDate ), 4, 2 )
      ProvaxDiaA := SubStr( DToC( ProximaDate ), 7, 4 )

      Atox := ProvaxDiaZ + ProvaxDiaX + ProvaxDiaA

      DprovaxDiaZ := SubStr( DToC( Date() ), 1, 2 )
      DprovaxDiaX := SubStr( DToC( Date() ), 4, 2 )
      DprovaxDiaA := SubStr( DToC( Date() ), 7, 4 )

      Dtox := DprovaxDiaZ + DprovaxDiaX + DprovaxDiaA

      SETPROPERTY( Ventana, "Fondo_10w", "PICTURE", IF( Atox == Dtox, 'Resource\Calen01x.jpg', 'resource\Calen00x.jpg' ) )

      SETPROPERTY( Ventana, "Label_01zA", "BACKCOLOR", IF( Atox == Dtox, { 253, 099, 000 }, { 255, 255, 255 } ) )
      SETPROPERTY( Ventana, "Label_02qA", "BACKCOLOR", IF( Atox == Dtox, { 253, 099, 000 }, { 255, 255, 255 } ) )
      SETPROPERTY( Ventana, "Label_12zA", "BACKCOLOR", IF( Atox == Dtox, { 253, 099, 000 }, { 255, 255, 255 } ) )
      SETPROPERTY( Ventana, "Label_13zA", "BACKCOLOR", IF( Atox == Dtox, { 253, 099, 000 }, { 255, 255, 255 } ) )

      SETPROPERTY( Ventana, "Label_01zA", "VALUE", SubStr( DToC( ProximaDate ), 7, 4 ) )
      SETPROPERTY( Ventana, "Label_02qA", "VALUE", SubStr( DToC( ProximaDate ), 1, 2 ) )
      SETPROPERTY( Ventana, "Label_12zA", "VALUE", ProximoMes )
      SETPROPERTY( Ventana, "Label_13zA", "VALUE", ProxiNomDia )

   ENDIF

RETURN
