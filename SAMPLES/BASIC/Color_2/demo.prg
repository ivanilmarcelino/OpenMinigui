/*
 * Name...: CAS
 * E-mail.: cas.soft@gmail.com
 * Country: Brazil
 *
 * dd/mm/yy
 * Date...: 03/Jun/2004  14:30
 * Modify.: 04/Jun/2004  17:25
 * .......: Test color
 */

#include "MiniGUI.ch"

STATIC a_curr, n_curr := 1

FUNCTION Main()

   LOCAL n, x_var, v_val, f_action

   MEMVAR a_cor
   PRIVATE a_cor := {}
   a_curr := Array ( 16 )

   SET NAVIGATION EXTENDED  // or STANDARD

   AAdd( a_cor, { 0, 0, 0 } )
   AAdd( a_cor, { 128, 128, 128 } )
   AAdd( a_cor, { 255, 255, 255 } )

   AAdd( a_cor, { 0, 0, 255 } )
   AAdd( a_cor, { 0, 255, 0 } )
   AAdd( a_cor, { 255, 0, 0 } )

   AAdd( a_cor, { 0, 0, 128 } )
   AAdd( a_cor, { 0, 128, 0 } )
   AAdd( a_cor, { 128, 0, 0 } )

   AAdd( a_cor, { 0, 255, 255 } )
   AAdd( a_cor, { 255, 255, 0 } )
   AAdd( a_cor, { 255, 0, 255 } )

   AAdd( a_cor, { 0, 128, 128 } )
   AAdd( a_cor, { 128, 128, 0 } )
   AAdd( a_cor, { 128, 0, 128 } )


   DEFINE WINDOW Form_1 AT 0, 0 WIDTH 410 HEIGHT 350 MAIN ;
         TITLE "Color RGB  -  by CAS <cas.soft@gmail.com>" ;
         NOMAXIMIZE NOSIZE ;
         ON INIT ( f_cor( a_cor[ 3 ] ), f_cor2( a_cor[ 3 ] ) )

      ON KEY ESCAPE ACTION ThisWindow.Release
      ON KEY ALT + X ACTION ThisWindow.Release
      ON KEY CONTROL + X ACTION ThisWindow.Release


      @ 14, 5 SPINNER Spn_1 RANGE 0, 255 VALUE 000 ;
         ON LOSTFOCUS f_cor() ;
         ON CHANGE f_cor()

      @ 46, 5 SPINNER Spn_2 RANGE 0, 255 VALUE 255 ;
         ON LOSTFOCUS f_cor() ;
         ON CHANGE f_cor()

      @ 77, 5 SPINNER Spn_3 RANGE 0, 255 VALUE 000 ;
         ON LOSTFOCUS f_cor() ;
         ON CHANGE f_cor()

      @ 130, 5 SLIDER Sld_1 RANGE 0, 255 VALUE 000 ;
         WIDTH 250 HEIGHT 30 NOTICKS ;
         ON SCROLL f_cor2() ;
         ON CHANGE f_cor2()

      @ 170, 5 SLIDER Sld_2 RANGE 0, 255 VALUE 255 ;
         WIDTH 250 HEIGHT 30 NOTICKS ;
         ON SCROLL f_cor2() ;
         ON CHANGE f_cor2()

      @ 210, 5 SLIDER Sld_3 RANGE 0, 255 VALUE 000 ;
         WIDTH 250 HEIGHT 30 NOTICKS ;
         ON SCROLL f_cor2() ;
         ON CHANGE f_cor2()

      @ 14, 134 LABEL Label_1 VALUE "Click Here" ;
         WIDTH 120 HEIGHT 86 ;
         BORDER ACTION f_GetColor()

      FOR n = 1 TO Len( a_cor )

         x_var := 'lb_c' + hb_ntos( n )

         v_val := 'Color {' + ;
            StrZero( a_cor[ n, 1 ], 3 ) + ',' + ;
            StrZero( a_cor[ n, 2 ], 3 ) + ',' + ;
            StrZero( a_cor[ n, 3 ], 3 ) + '}'

         f_action := 'f_cor( a_cor[' + hb_ntos( n ) + '] )'

         @ 20 * n - 6, 270 LABEL &x_var ;
            VALUE v_val ;
            BACKCOLOR a_cor[ n ] BORDER ;
            ACTION &f_action ;
            HEIGHT 18

         SetProperty ( 'form_1', x_var, 'FONTCOLOR', HMG_n2RGB( ContrastClr( HMG_RGB2n( a_cor[ n ] ) ) ) )

      NEXT

   END WINDOW

   Form_1.Center
   Form_1.Activate

RETURN NIL

*______________________________________________________________________________________*

PROCEDURE f_cor( m_cor )

   LOCAL m_form, c1, c2, c3

   m_form := thiswindow.NAME

   IF PCount() = 1
      SetProperty ( m_form, 'spn_1', 'value', m_cor[ 1 ] )
      SetProperty ( m_form, 'spn_2', 'value', m_cor[ 2 ] )
      SetProperty ( m_form, 'spn_3', 'value', m_cor[ 3 ] )
      f_cor()
      RETURN
   ENDIF

   c1 := GetProperty ( m_form, 'spn_1', 'value' )
   c2 := GetProperty ( m_form, 'spn_2', 'value' )
   c3 := GetProperty ( m_form, 'spn_3', 'value' )

   SetProperty ( m_form, 'sld_1', 'value', c1 )
   SetProperty ( m_form, 'sld_2', 'value', c2 )
   SetProperty ( m_form, 'sld_3', 'value', c3 )

   SetProperty ( m_form, 'label_1', 'FONTCOLOR', HMG_n2RGB( ContrastClr( HMG_RGB2n( { c1, c2, c3 } ) ) ) )
   SetProperty ( m_form, 'label_1', 'BACKCOLOR', { c1, c2, c3 } )

RETURN

*______________________________________________________________________________________*

PROCEDURE f_cor2( m_cor )

   LOCAL m_form, c1, c2, c3

   m_form := thiswindow.NAME

   IF PCount() = 1
      SetProperty ( m_form, 'sld_1', 'value', m_cor[ 1 ] )
      SetProperty ( m_form, 'sld_2', 'value', m_cor[ 2 ] )
      SetProperty ( m_form, 'sld_3', 'value', m_cor[ 3 ] )
      RETURN
   ENDIF

   c1 := GetProperty ( m_form, 'sld_1', 'value' )
   c2 := GetProperty ( m_form, 'sld_2', 'value' )
   c3 := GetProperty ( m_form, 'sld_3', 'value' )

   SetProperty ( m_form, 'spn_1', 'VALUE', c1 )
   SetProperty ( m_form, 'spn_2', 'VALUE', c2 )
   SetProperty ( m_form, 'spn_3', 'VALUE', c3 )

   SetProperty ( m_form, 'label_1', 'FONTCOLOR', HMG_n2RGB( ContrastClr( HMG_RGB2n( { c1, c2, c3 } ) ) ) )
   SetProperty ( m_form, 'label_1', 'BACKCOLOR', { c1, c2, c3 } )

RETURN

*______________________________________________________________________________________*

PROCEDURE f_GetColor

   LOCAL m_form, cor_atual, m_cor

   m_form := thiswindow.NAME

   cor_atual := GetProperty ( m_form, 'label_1', 'backcolor' )
   a_curr[ n_curr++ ] := cor_atual

   m_cor := GetColor( cor_atual, a_curr, .t. )

   IF ValType( m_cor[ 1 ] ) # 'N'
      RETURN
   ENDIF

   SetProperty ( m_form, 'label_1', 'backcolor', m_cor )

   SetProperty ( m_form, 'spn_1', 'value', m_cor[ 1 ] )
   SetProperty ( m_form, 'spn_2', 'value', m_cor[ 2 ] )
   SetProperty ( m_form, 'spn_3', 'value', m_cor[ 3 ] )

   f_cor()

RETURN

*______________________________________________________________________________________*

FUNCTION ContrastClr( nClr )

   LOCAL nLuma

   IF HB_ISNUMERIC( nClr ) .and. nClr >= 0 .and. nClr <= 0x00ffffff
      nLuma := ( 0.299 * GetRed( nClr ) + 0.587 * GetGreen( nClr ) + 0.114 * GetBlue( nClr ) )
   ELSE
      RETURN CLR_WHITE
   ENDIF

RETURN iif( nLuma < 128, CLR_WHITE, CLR_BLACK )
