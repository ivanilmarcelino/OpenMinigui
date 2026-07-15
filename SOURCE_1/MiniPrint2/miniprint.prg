/*----------------------------------------------------------------------------
 MINIGUI - Harbour Win32 GUI library source code

 Copyright 2002-2016 Roberto Lopez <mail.box.hmg@gmail.com>
 http://sites.google.com/site/hmgweb/

 This program is free software; you can redistribute it and/or modify it under
 the terms of the GNU General Public License as published by the Free Software
 Foundation; either version 2 of the License, or (at your option) any later
 version.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

 You should have received a copy of the GNU General Public License along with
 this software; see the file COPYING. If not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA (or
 visit the web site http://www.gnu.org/).

 As a special exception, you have permission for additional uses of the text
 contained in this release of Harbour Minigui.

 The exception is that, if you link the Harbour Minigui library with other
 files to produce an executable, this does not by itself cause the resulting
 executable to be covered by the GNU General Public License.
 Your use of that executable is in no way restricted on account of linking the
 Harbour-Minigui library code into it.

 Parts of this project are based upon:

   "Harbour GUI framework for Win32"
    Copyright 2001 Alexander S.Kresin <alex@belacy.ru>
    Copyright 2001 Antonio Linares <alinares@fivetech.com>
   www - https://harbour.github.io/

   "Harbour Project"
        Copyright 1999-2023, https://harbour.github.io/

 Parts of this module are based upon:

   "HBPRINT"
   Copyright 2002 Richard Rylko <rrylko@poczta.onet.pl>
   http://rrylko.republika.pl

   "HBPRINTER"
   Copyright 2002 Richard Rylko <rrylko@poczta.onet.pl>
   http://rrylko.republika.pl

---------------------------------------------------------------------------*/

///////////////////////////////////////////////////////////////////////////////
// HARBOUR LEVEL PRINT ROUTINES
///////////////////////////////////////////////////////////////////////////////

#include "hmg.ch"
#include "hp_images.ch"
#include "i_mail.ch" 

#define SB_HORZ      0
#define SB_VERT      1

#xtranslate Alltrim( Str( <i> ) )  =>  hb_ntos( <i> )

DECLARE WINDOW _HMG_PRINTER_PPNAV

*------------------------------------------------------------------------------*
PROCEDURE _HMG_PRINTER_SHOWPREVIEW
*------------------------------------------------------------------------------*
   LOCAL ModalHandle
   LOCAL Tmp
   LOCAL i
   LOCAL tHeight
   LOCAL tFactor
   LOCAL tvHeight
   LOCAL icb
   
   IF _SetGetGlobal( "IsVistaThemed" ) == NIL
      STATIC IsVistaThemed AS GLOBAL VALUE ( IsVistaOrLater() .AND. IsAppXPThemed() )
   ENDIF
   
   _hmg_printer_BasePageName := GetTempFolder() + hb_ps() + _hmg_printer_timestamp + "_hmg_print_preview_"
   _hmg_printer_CurrentPageNumber := 1
   _hmg_printer_Dx := 0
   _hmg_printer_Dy := 0
   _hmg_printer_Dz := 0
   _hmg_printer_scrollstep := 10
   _hmg_printer_zoomclick_xoffset := 0
   _hmg_printer_thumbupdate := .T.
   _hmg_printer_PrevPageNumber := 0
   _hmg_printer_collate := PRINTER_COLLATE_FALSE
   
   IF _HMG_IsModalActive == .T.
      
      ModalHandle := _hmg_activemodalhandle
      
      _HMG_IsModalActive := .F.
      _hmg_activemodalhandle := 0
      
      DisableWindow ( ModalHandle )
      
   ELSE
      
      ModalHandle := 0

   ENDIF
   
   IF _hmg_printer_hdc_bak == 0
      RETURN
   ENDIF
   
   IF _IsWindowDefined ( "_HMG_PRINTER_SHOWPREVIEW" )
      RETURN
   ENDIF
   
   icb := _HMG_InteractiveClose
   
   SET INTERACTIVECLOSE ON
   
   _hmg_printer_SizeFactor := GetDeskTopHeight() / _HMG_PRINTER_GETPAGEHEIGHT( _hmg_printer_hdc_bak ) * 0.63
   
   IF _HMG_PRINTER_GETPAGEHEIGHT( _hmg_printer_hdc_bak ) > 370
      _HMG_PRINTER_DELTA_ZOOM := - 250
   ELSE
      _HMG_PRINTER_DELTA_ZOOM := 0
   ENDIF
   
   DEFINE WINDOW _HMG_PRINTER_Wait At 0, 0 WIDTH 310 HEIGHT 85 TITLE '' CHILD NOSHOW NOCAPTION
      DEFINE LABEL label_1
         Row 30
         Col 5
         WIDTH 300
         HEIGHT 30
         VALUE _hmg_printer_usermessages [ 29 ]
         centeralign .T.
      END LABEL
   END WINDOW
   
   _HMG_PRINTER_Wait.Center
   
   DEFINE WINDOW _HMG_PRINTER_PPNAV ;
          At 0, 0 ;
          WIDTH GetDeskTopWidth() - 103 - iif ( _SetGetGlobal( "IsVistaThemed" ) , 25 , 0 ) ;
          HEIGHT GetDeskTopHeight() - 66 - iif ( _SetGetGlobal( "IsVistaThemed" ) , 25 , 0 ) ;
          TITLE _hmg_printer_usermessages [ 01 ] + ' [' + AllTrim( Str( _hmg_printer_CurrentPageNumber ) ) + '/' + ;
                                                       AllTrim( Str( _hmg_printer_PageCount ) ) + ']' ;
          CHILD ;
          NOMINIMIZE ;
          NOMAXIMIZE ;
          NOSYSMENU ;
          BACKCOLOR GRAY ;
          ON GOTFOCUS Iif( IsWin8OrLater(), _HMG_PRINTER_PREVIEWRefresh(), ) ;
          ON PAINT _HMG_PRINTER_PREVIEWRefresh() ;
          ON SIZE  _HMG_PRINTER_PREVIEWRefresh() ;
          ON RELEASE ( _DelGlobal( "IsVistaThemed" ) , _DelGlobal( 'aCoords' ) , _DelGlobal( "_HMG_PRINTER_lFlag" ) )
      
      DEFINE SPLITBOX
         
         DEFINE TOOLBAR ToolBar_1 CAPTION _hmg_printer_usermessages [ 02 ] BUTTONSIZE 25, 25 FLAT
            
            BUTTON b2 PICTURE IMG_BACK TOOLTIP _hmg_printer_usermessages [ 04 ] ACTION ( _hmg_printer_CurrentPageNumber -- , _HMG_PRINTER_PREVIEWRefresh() )
            
            BUTTON b3 PICTURE IMG_NEXT TOOLTIP _hmg_printer_usermessages [ 05 ] ACTION ( _hmg_printer_CurrentPageNumber ++ , _HMG_PRINTER_PREVIEWRefresh() ) SEPARATOR
            
            BUTTON b1 PICTURE IMG_TOP TOOLTIP _hmg_printer_usermessages [ 03 ] ACTION ( _hmg_printer_CurrentPageNumber := 1 , _HMG_PRINTER_PREVIEWRefresh() )
            
            BUTTON b4 PICTURE IMG_END TOOLTIP _hmg_printer_usermessages [ 06 ] ACTION ( _hmg_printer_CurrentPageNumber := _hmg_printer_PageCount, _HMG_PRINTER_PREVIEWRefresh() ) SEPARATOR
            
            BUTTON GoToPage PICTURE IMG_GOPAGE TOOLTIP _hmg_printer_usermessages [ 07 ] + ' [Ctrl+G]' ACTION _HMG_PRINTER_GO_TO_PAGE() SEPARATOR
            
            BUTTON thumbswitch PICTURE IMG_THUMBNAIL TOOLTIP _hmg_printer_usermessages [ 28 ] + ' [Ctrl+T]' ACTION _HMG_PRINTER_ProcessTHUMBNAILS() CHECK SEPARATOR
            
            BUTTON b5 PICTURE IMG_ZOOM TOOLTIP _hmg_printer_usermessages [ 08 ] + ' [*]' ACTION _HMG_PRINTER_Zoom() CHECK SEPARATOR
            
            BUTTON b12 PICTURE IMG_PRINT TOOLTIP _hmg_printer_usermessages [ 09 ] + ' [Ctrl+P]' ACTION _HMG_PRINTER_PrintPages()
            
            BUTTON b7 PICTURE IMG_SAVE TOOLTIP _hmg_printer_usermessages [ 27 ] + ' [Ctrl+S]' ACTION _hmg_printer_save_pdf_pages() SEPARATOR
            
            BUTTON b8 PICTURE IMG_MAIL TOOLTIP _hmg_printer_usermessages [ 30 ] + ' [Ctrl+E]' ACTION _hmg_printer_send_mail() SEPARATOR
            
            BUTTON b6 PICTURE IMG_CLOSE TOOLTIP _hmg_printer_usermessages [ 26 ] + ' [Ctrl+C]' ACTION _HMG_PRINTER_PreviewClose()
            
         END TOOLBAR
         
         DEFINE WINDOW _HMG_PRINTER_SHOWPREVIEW ;
                WIDTH GetDeskTopWidth() - 103 - iif ( _SetGetGlobal( "IsVistaThemed" ) , 25 , 0 ) ;
                HEIGHT GetDeskTopHeight() - 140  - iif ( _SetGetGlobal( "IsVistaThemed" ) , 25 , 0 ) ;
                VIRTUAL WIDTH ( GetDeskTopWidth() - 103 ) * 2 ;
                VIRTUAL HEIGHT ( GetDeskTopHeight() - 140 ) * 2 ;
                SPLITCHILD NOCAPTION ;
                ON SCROLLUP   _HMG_PRINTER_ScrolluP() ;
                ON SCROLLDOWN   _HMG_PRINTER_ScrollDown() ;
                ON SCROLLLEFT   _HMG_PRINTER_ScrollLeft() ;
                ON SCROLLRIGHT   _HMG_PRINTER_ScrollRight() ;
                ON HSCROLLBOX   _HMG_PRINTER_hScrollBoxProcess() ;
                ON VSCROLLBOX   _HMG_PRINTER_vScrollBoxProcess()
            
            _HMG_PRINTER_SetKeys( '_HMG_PRINTER_SHOWPREVIEW' )
            
         END WINDOW
         
         CREATE EVENT PROCNAME _HMG_PRINTER_SpltChldMouseClick()
         
      END SPLITBOX
      
      _HMG_PRINTER_SetKeys( '_HMG_PRINTER_PPNAV' )
      
   END WINDOW
   
   DEFINE WINDOW _HMG_PRINTER_PRINTPAGES ;
          At 0, 0 ;
          WIDTH 430 ;
          HEIGHT 170 + GetTitleHeight() ;
          TITLE _hmg_printer_usermessages [ 9 ] ;
          CHILD NOSHOW ;
          NOSIZE NOSYSMENU
      
      ON KEY ESCAPE ACTION ( HideWindow ( GetFormHandle ( "_HMG_PRINTER_PRINTPAGES" ) ) , EnableWindow ( GetFormHandle ( "_HMG_PRINTER_SHOWPREVIEW" ) ) , EnableWindow ( GetFormHandle ( "_HMG_PRINTER_SHOWTHUMBNAILS" ) ) , EnableWindow ( GetFormHandle ( "_HMG_PRINTER_PPNAV" ) ) , _HMG_PRINTER_SHOWPREVIEW.setfocus )
      ON KEY RETURN ACTION _HMG_PRINTER_PrintPagesDo()
      
      DEFINE FRAME Frame_1
         Row 5
         Col 10
         WIDTH 275
         HEIGHT 150
         FONTNAME 'Arial'
         FONTSIZE 9
         CAPTION _hmg_printer_usermessages [ 15 ]
      END FRAME
      
      DEFINE RADIOGROUP Radio_1
         Row 25
         Col 20
         FONTNAME 'Arial'
         FONTSIZE 9
         VALUE 1
         OPTIONS { _hmg_printer_usermessages [ 16 ] , _hmg_printer_usermessages [ 17 ] }
         ONCHANGE Iif ( This.value == 1 , ( _HMG_PRINTER_PRINTPAGES.Label_1.Enabled := .F. , _HMG_PRINTER_PRINTPAGES.Label_2.Enabled := .F. , _HMG_PRINTER_PRINTPAGES.Spinner_1.Enabled := .F. , _HMG_PRINTER_PRINTPAGES.Spinner_2.Enabled := .F. , _HMG_PRINTER_PRINTPAGES.Combo_1.Enabled := .F.  , _HMG_PRINTER_PRINTPAGES.Label_4.Enabled := .F. ) , ( _HMG_PRINTER_PRINTPAGES.Label_1.Enabled := .T. , _HMG_PRINTER_PRINTPAGES.Label_2.Enabled := .T. , _HMG_PRINTER_PRINTPAGES.Spinner_1.Enabled := .T. , _HMG_PRINTER_PRINTPAGES.Spinner_2.Enabled := .T. , _HMG_PRINTER_PRINTPAGES.Combo_1.Enabled := .T.  , _HMG_PRINTER_PRINTPAGES.Label_4.Enabled := .T. , _HMG_PRINTER_PRINTPAGES.Spinner_1.SetFocus ) )
      END RADIOGROUP
      
      DEFINE LABEL Label_1
         Row 84
         Col 50
         WIDTH 50
         HEIGHT 25
         FONTNAME 'Arial'
         FONTSIZE 8
         VALUE _hmg_printer_usermessages [ 18 ]
      END LABEL
      
      DEFINE SPINNER Spinner_1
         Row 81
         Col 110
         WIDTH 50
         FONTNAME 'Arial'
         FONTSIZE 9
         VALUE 1
         RangeMin 1
         RangeMax _hmg_printer_PageCount
      END SPINNER
      
      DEFINE LABEL Label_2
         Row 84
         Col 175
         WIDTH 35
         HEIGHT 25
         FONTNAME 'Arial'
         FONTSIZE 8
         VALUE _hmg_printer_usermessages [ 19 ]
      END LABEL
      
      DEFINE SPINNER Spinner_2
         Row 81
         Col 205
         WIDTH 50
         FONTNAME 'Arial'
         FONTSIZE 9
         VALUE _hmg_printer_PageCount
         RangeMin 1
         RangeMax _hmg_printer_PageCount
      END SPINNER
      
      DEFINE LABEL Label_4
         Row 115
         Col 50
         WIDTH 55
         HEIGHT 25
         FONTNAME 'Arial'
         FONTSIZE 8
         VALUE _hmg_printer_usermessages [ 09 ]
      END LABEL
      
      DEFINE COMBOBOX Combo_1
         Row 113
         Col 110
         WIDTH 145
         FONTNAME 'Arial'
         FONTSIZE 9
         VALUE 1
         ITEMS { _hmg_printer_usermessages [ 21 ] , _hmg_printer_usermessages [ 22 ] , _hmg_printer_usermessages [ 23 ] }
      END COMBOBOX
      
      DEFINE BUTTON Ok
         Row 10
         Col 300
         WIDTH 105
         HEIGHT 25
         FONTNAME 'Arial'
         FONTSIZE 9
         CAPTION _hmg_printer_usermessages [ 11 ]
         ACTION _HMG_PRINTER_PrintPagesDo()
      END BUTTON
      
      DEFINE BUTTON Cancel
         Row 40
         Col 300
         WIDTH 105
         HEIGHT 25
         FONTNAME 'Arial'
         FONTSIZE 9
         CAPTION _hmg_printer_usermessages [ 12 ]
         ACTION ( EnableWindow ( GetFormHandle ( "_HMG_PRINTER_SHOWPREVIEW" ) ) , EnableWindow ( GetFormHandle ( "_HMG_PRINTER_SHOWTHUMBNAILS" ) ) , EnableWindow ( GetFormHandle ( "_HMG_PRINTER_PPNAV" ) ) , HideWindow ( GetFormHandle ( "_HMG_PRINTER_PRINTPAGES" ) ) , _HMG_PRINTER_SHOWPREVIEW.setfocus )
      END BUTTON
      
      DEFINE LABEL Label_3
         Row 103
         Col 295
         WIDTH 55
         HEIGHT 25
         FONTNAME 'Arial'
         FONTSIZE 8
         VALUE _hmg_printer_usermessages [ 20 ]
      END LABEL
      
      DEFINE SPINNER Spinner_3
         Row 100
         Col 355
         WIDTH 50
         FONTNAME 'Arial'
         FONTSIZE 9
         VALUE _hmg_printer_copies
         RangeMin 1
         RangeMax 999
         ONCHANGE Iif ( IsControlDefined ( CheckBox_1, _HMG_PRINTER_PRINTPAGES ) , Iif ( This.Value > 1 , SetProperty( '_HMG_PRINTER_PRINTPAGES' , 'CheckBox_1', 'Enabled', .T. ) , SetProperty( '_HMG_PRINTER_PRINTPAGES', 'CheckBox_1', 'Enabled', .F. ) ) , NIL )
      END SPINNER
      
      DEFINE CHECKBOX CheckBox_1
         Row 132
         Col 295
         WIDTH 120
         FONTNAME 'Arial'
         FONTSIZE 8
         VALUE Iif ( _hmg_printer_collate == 1 , .T. , .F. )
         CAPTION _hmg_printer_usermessages [ 14 ]
      END CHECKBOX
      
   END WINDOW
   
   CENTER WINDOW _HMG_PRINTER_PRINTPAGES
   
   DEFINE WINDOW _HMG_PRINTER_GO_TO_PAGE ;
          At 0, 0 ;
          WIDTH 195 ;
          HEIGHT 90 + GetTitleHeight() ;
          TITLE _hmg_printer_usermessages [ 07 ] ;
          CHILD NOSHOW ;
          NOSIZE NOSYSMENU
      
      ON KEY ESCAPE ACTION ( HideWindow( GetFormHandle ( "_HMG_PRINTER_GO_TO_PAGE" ) ) , EnableWindow ( GetFormHandle ( "_HMG_PRINTER_SHOWPREVIEW" ) )  , EnableWindow ( GetFormHandle ( "_HMG_PRINTER_SHOWTHUMBNAILS" ) ) , EnableWindow ( GetFormHandle ( "_HMG_PRINTER_PPNAV" ) ) , _HMG_PRINTER_SHOWPREVIEW.setfocus  )
      ON KEY RETURN ACTION ( _hmg_printer_CurrentPageNumber := _HMG_PRINTER_GO_TO_PAGE.Spinner_1.Value , HideWindow( GetFormHandle ( "_HMG_PRINTER_GO_TO_PAGE" ) ) , EnableWindow ( GetFormHandle ( "_HMG_PRINTER_SHOWPREVIEW" ) ) , EnableWindow ( GetFormHandle ( "_HMG_PRINTER_SHOWTHUMBNAILS" ) ) , EnableWindow ( GetFormHandle ( "_HMG_PRINTER_PPNAV" ) ) , _HMG_PRINTER_PREVIEWRefresh() , _HMG_PRINTER_SHOWPREVIEW.setfocus  )
      
      DEFINE LABEL Label_1
         Row 13
         Col 10
         WIDTH 94
         HEIGHT 25
         FONTNAME 'Arial'
         FONTSIZE 9
         VALUE _hmg_printer_usermessages [ 10 ] + ':'
      END LABEL
      
      DEFINE SPINNER Spinner_1
         Row 10
         Col 105
         WIDTH 75
         FONTNAME 'Arial'
         FONTSIZE 9
         VALUE _hmg_printer_CurrentPageNumber
         RangeMin 1
         RangeMax _hmg_printer_PageCount
      END SPINNER
      
      DEFINE BUTTON Ok
         Row 48
         Col 10
         WIDTH 80
         HEIGHT 25
         FONTNAME 'Arial'
         FONTSIZE 9
         CAPTION _hmg_printer_usermessages [ 11 ]
         ACTION ( _hmg_printer_CurrentPageNumber := _HMG_PRINTER_GO_TO_PAGE.Spinner_1.Value , HideWindow( GetFormHandle ( "_HMG_PRINTER_GO_TO_PAGE" ) ) , EnableWindow ( GetFormHandle ( "_HMG_PRINTER_SHOWPREVIEW" ) ) , EnableWindow ( GetFormHandle ( "_HMG_PRINTER_SHOWTHUMBNAILS" ) ) , EnableWindow ( GetFormHandle ( "_HMG_PRINTER_PPNAV" ) ) , _HMG_PRINTER_PREVIEWRefresh() , _HMG_PRINTER_SHOWPREVIEW.setfocus  )
      END BUTTON
      
      DEFINE BUTTON Cancel
         Row 48
         Col 100
         WIDTH 80
         HEIGHT 25
         FONTNAME 'Arial'
         FONTSIZE 9
         CAPTION _hmg_printer_usermessages [ 12 ]
         ACTION ( HideWindow( GetFormHandle ( "_HMG_PRINTER_GO_TO_PAGE" ) ) , EnableWindow ( GetFormHandle ( "_HMG_PRINTER_SHOWPREVIEW" ) )  , EnableWindow ( GetFormHandle ( "_HMG_PRINTER_SHOWTHUMBNAILS" ) ) , EnableWindow ( GetFormHandle ( "_HMG_PRINTER_PPNAV" ) ) , _HMG_PRINTER_SHOWPREVIEW.setfocus  )
      END BUTTON
      
   END WINDOW
   
   CENTER WINDOW _HMG_PRINTER_GO_TO_PAGE
   
   IF _HMG_PRINTER_GETPAGEHEIGHT( _hmg_printer_hdc_bak ) > _HMG_PRINTER_GETPAGEWIDTH( _hmg_printer_hdc_bak )
      tFactor := 0.44
   ELSE
      tFactor := 0.26
   ENDIF
   
   tHeight := _HMG_PRINTER_GETPAGEHEIGHT( _hmg_printer_hdc_bak ) * tFactor
   
   tHeight := Int ( tHeight )
   
   tvHeight := ( _hmg_printer_PageCount * ( tHeight + 10 ) ) + GetHScrollbarHeight() + GetTitleHeight() + ( GetBorderHeight() * 2 ) + 7
   
   IF tvHeight <= GetDeskTopHeight() - 66
      _hmg_printer_thumbscroll := .F.
      tvHeight := GetDeskTopHeight() - 65
   ELSE
      _hmg_printer_thumbscroll := .T.
   ENDIF
   
   DEFINE WINDOW _HMG_PRINTER_SHOWTHUMBNAILS ;
          At 0, 5 ;
          WIDTH 130 ;
          HEIGHT GetDeskTopHeight() - 66 - iif ( _SetGetGlobal( "IsVistaThemed" ) , 25 , 0 ) ;
          VIRTUAL WIDTH 131 ;
          VIRTUAL HEIGHT tvHeight ;
          TITLE _hmg_printer_usermessages [ 28 ] ;
          CHILD ;
          NOSIZE ;
          NOMINIMIZE ;
          NOMAXIMIZE ;
          NOSYSMENU ;
          NOSHOW ;
          BACKCOLOR GRAY
      
      _HMG_PRINTER_SetKeys( '_HMG_PRINTER_SHOWTHUMBNAILS' )
      
   END WINDOW
   
   IF _hmg_printer_thumbscroll == .F.
      _HMG_PRINTER_PREVIEW_DISABLESCROLLBARS ( GetFormHandle( '_HMG_PRINTER_SHOWTHUMBNAILS' ) )
   ENDIF
   
   SetScrollRange ( GetFormHandle( '_HMG_PRINTER_SHOWPREVIEW' ) , SB_VERT , 0 , 100 , .T. )
   SetScrollRange ( GetFormHandle( '_HMG_PRINTER_SHOWPREVIEW' ) , SB_HORZ , 0 , 100 , .T. )
   
   SetScrollPos ( GetFormHandle( '_HMG_PRINTER_SHOWPREVIEW' ) , SB_VERT , 50 , .T. )
   SetScrollPos ( GetFormHandle( '_HMG_PRINTER_SHOWPREVIEW' ) , SB_HORZ , 50 , .T. )
   
   _HMG_PRINTER_PREVIEW_DISABLESCROLLBARS ( GetFormHandle( '_HMG_PRINTER_SHOWPREVIEW' ) )
   
   _HMG_PRINTER_PREVIEW_DISABLEHSCROLLBAR ( GetFormHandle( '_HMG_PRINTER_SHOWTHUMBNAILS' ) )

   CENTER WINDOW _HMG_PRINTER_PPNAV
   
   Tmp := _HMG_PRINTER_PPNAV.ROW
   
   _HMG_PRINTER_SHOWTHUMBNAILS.ROW := Tmp
   
   ACTIVATE WINDOW _HMG_PRINTER_PRINTPAGES , _HMG_PRINTER_GO_TO_PAGE , _HMG_PRINTER_SHOWTHUMBNAILS , _HMG_PRINTER_Wait , _HMG_PRINTER_PPNAV
   
   EventRemove()

   _hmg_printer_hdc := _hmg_printer_hdc_bak
   
   IF ModalHandle != 0
      
      FOR i := 1 TO Len ( _HMG_aFormHandles )
         IF _HMG_aFormDeleted [ i ] == .F.
            IF _HMG_aFormType [ i ] != 'X'
               IF _HMG_aFormHandles [ i ] != ModalHandle
                  DisableWindow ( _HMG_aFormHandles [ i ] )
               ENDIF
            ENDIF
         ENDIF
      NEXT i
      
      EnableWindow ( ModalHandle )
      
      FOR i := 1 TO Len ( _HMG_aFormHandles )
         IF _HMG_aFormDeleted [ i ] == .F.
            IF _HMG_aFormType [ i ] == 'P' .AND. _HMG_aFormParentHandle [ i ] == ModalHandle  // Panel window into Modal window
               EnableWindow ( _HMG_aFormHandles [ i ] )
            ENDIF
         ENDIF
      NEXT i
      
      SetFocus ( ModalHandle )
      
      _HMG_IsModalActive := .T.
      _hmg_activemodalhandle := ModalHandle
      
   ENDIF
   
   _HMG_InteractiveClose := icb
   
   RETURN
*------------------------------------------------------------------------------*
STATIC PROCEDURE _HMG_PRINTER_SetKeys( parent )
*------------------------------------------------------------------------------*
   
   ON KEY HOME        OF &parent ACTION ( _hmg_printer_CurrentPageNumber := 1 , _HMG_PRINTER_PREVIEWRefresh()  )
   ON KEY PRIOR       OF &parent ACTION ( _hmg_printer_CurrentPageNumber -- , _HMG_PRINTER_PREVIEWRefresh()  )
   ON KEY NEXT        OF &parent ACTION ( _hmg_printer_CurrentPageNumber ++ , _HMG_PRINTER_PREVIEWRefresh()  )
   ON KEY END         OF &parent ACTION ( _hmg_printer_CurrentPageNumber := _hmg_printer_PageCount, _HMG_PRINTER_PREVIEWRefresh()  )
   ON KEY CONTROL + P OF &parent ACTION _HMG_PRINTER_Printpages()
   ON KEY CONTROL + G OF &parent ACTION _HMG_PRINTER_GO_TO_PAGE()
   ON KEY ESCAPE      OF &parent ACTION _HMG_PRINTER_PreviewClose()
   ON KEY MULTIPLY    OF &parent ACTION ( _HMG_PRINTER_PPNAV.b5.value := ! _HMG_PRINTER_PPNAV.b5.value , _HMG_PRINTER_MouseZoom() )
   ON KEY CONTROL + C OF &parent ACTION _HMG_PRINTER_PreviewClose()
   ON KEY ALT + F4    OF &parent ACTION _HMG_PRINTER_PreviewClose()
   ON KEY CONTROL + S OF &parent ACTION _hmg_printer_save_pdf_pages()
   ON KEY CONTROL + E OF &parent ACTION _hmg_printer_send_mail()
   ON KEY CONTROL + T OF &parent ACTION _hmg_printer_ThumbnailToggle()
   
   RETURN
*------------------------------------------------------------------------------*
STATIC PROCEDURE CreateThumbNails
*------------------------------------------------------------------------------*
   LOCAL tFactor
   LOCAL tWidth
   LOCAL tHeight
   LOCAL ttHandle
   LOCAL i
   LOCAL cMacroTemp
   LOCAL cAction
   
   IF _IsControlDefined ( 'Image1' , '_HMG_PRINTER_SHOWTHUMBNAILS' )
      RETURN
   ENDIF
   
   ShowWindow ( GetFormHandle ( "_HMG_PRINTER_Wait" ) )
   
   IF _HMG_PRINTER_GETPAGEHEIGHT( _hmg_printer_hdc_bak ) > _HMG_PRINTER_GETPAGEWIDTH( _hmg_printer_hdc_bak )
      tFactor := 0.44
   ELSE
      tFactor := 0.30
   ENDIF
   
   tWidth   := _HMG_PRINTER_GETPAGEWIDTH( _hmg_printer_hdc_bak ) * tFactor
   tHeight := _HMG_PRINTER_GETPAGEHEIGHT( _hmg_printer_hdc_bak ) * tFactor
   
   tHeight := Int ( tHeight )
   
   ttHandle := GetFormToolTipHandle ( '_HMG_PRINTER_SHOWTHUMBNAILS' )
   
   FOR i := 1 TO _hmg_printer_PageCount
      
      cMacroTemp := 'Image' + AllTrim( Str( i ) )
      
      cAction := "_HMG_MINIPRINT [4] := " + AllTrim( Str( i ) ) + ", _HMG_MINIPRINT [11] := .F., _HMG_PRINTER_PREVIEWRefresh(), _HMG_MINIPRINT [11] := .T."
      
      _DefineEmfFile( ;
                      cMacroTemp, ;
                      '_HMG_PRINTER_SHOWTHUMBNAILS', ;
                      10, ;
                      ( i * ( tHeight + 10 ) ) - tHeight, ;
                      _hmg_printer_BasePageName + StrZero( i, 4 ) + ".emf", ;
                      tWidth, ;
                      tHeight, ;
                      { || &cAction }, ;
                      NIL, ;
                      .F., ;
                      .F., ;
                      .T. ;
                    )
      
      SetToolTip ( GetControlHandle ( cMacroTemp, '_HMG_PRINTER_SHOWTHUMBNAILS' ), _hmg_printer_usermessages [ 01 ] + ' ' + AllTrim( Str( i ) ) + ' [Click]', ttHandle )
      
   NEXT i
   
   HideWindow ( GetFormHandle ( "_HMG_PRINTER_Wait" ) )
   
   RETURN
*------------------------------------------------------------------------------*
STATIC PROCEDURE _hmg_printer_ThumbnailToggle()
*------------------------------------------------------------------------------*
   
   _HMG_PRINTER_PPNAV.thumbswitch.Value := ! _HMG_PRINTER_PPNAV.thumbswitch.Value
   
   _HMG_PRINTER_ProcessTHUMBNAILS()
   
   RETURN
*------------------------------------------------------------------------------*
PROCEDURE _HMG_PRINTER_ProcessTHUMBNAILS()
*------------------------------------------------------------------------------*
   
   IF _HMG_PRINTER_PPNAV.thumbswitch.Value == .T.
      
      CreateThumbNails()
      
      _hmg_printer_zoomclick_xoffset := 90
      
      _hmg_printer_SizeFactor := GetDeskTopHeight() / _HMG_PRINTER_GETPAGEHEIGHT( _hmg_printer_hdc_bak ) * 0.58
      
      _HMG_PRINTER_PPNAV.Width := GetDeskTopWidth() - 148 - iif ( _SetGetGlobal( "IsVistaThemed" ) , 30 , 0 )
      
      _HMG_PRINTER_PPNAV.Col := 138 + iif ( _SetGetGlobal( "IsVistaThemed" ) , 20 , 0 )
      
      _HMG_PRINTER_PREVIEWRefresh()
      
      ShowWindow ( GetFormHandle ( "_HMG_PRINTER_SHOWTHUMBNAILS" ) )
      
   ELSE
      
      _hmg_printer_zoomclick_xoffset := 0
      
      _hmg_printer_SizeFactor := GetDeskTopHeight() / _HMG_PRINTER_GETPAGEHEIGHT( _hmg_printer_hdc_bak ) * 0.63
      
      _HMG_PRINTER_PPNAV.Width := GetDeskTopWidth() - 103
      
      _HMG_PRINTER_PPNAV.Col := 51
      
      HideWindow ( GetFormHandle ( "_HMG_PRINTER_SHOWTHUMBNAILS" ) )
      
      _HMG_PRINTER_SHOWPREVIEW.SetFocus
      
      _HMG_PRINTER_PREVIEWRefresh()
      
   ENDIF
   
   RETURN
*------------------------------------------------------------------------------*
PROCEDURE _hmg_printer_save_pdf_pages( cDestFile, lOpen, cTitle )  /* Added by Pete D., February 2022 */
*------------------------------------------------------------------------------*
   LOCAL cSrcFolder, aEmfs, aPages

   IF Empty( cDestFile )
      cDestFile := PutFile( { { 'PDF Files', '*.pdf' } }, , GetCurrentFolder(), .T., _hmg_printer_JobName )
      IF Empty( cDestFile )
         RETURN
      ENDIF
   ENDIF

   IF ISNIL( lOpen )
      _HMG_PRINTER_PPNAV.b7.Enabled := .F.
      HMG_SysWait()
   ENDIF

   cSrcFolder := hb_DirSepAdd( GetTempFolder() )
   aEmfs := hb_vfDirectory( cSrcFolder + _hmg_printer_timestamp  + "_hmg_print_preview_*.Emf" )

   aPages := {}
   AEval( aEmfs, {|e| AAdd( aPages, cSrcFolder + e[ 1 ] ) } )

   cDestfile := hb_FNameExtSetDef( cDestfile, ".pdf" )
   _CreatePdf( ASort( aPages ), cDestfile, lOpen, cTitle )

   _HMG_PRINTER_PPNAV.b7.Enabled := .T.
   _HMG_PRINTER_SHOWPREVIEW.SetFocus

   RETURN
*------------------------------------------------------------------------------*
PROCEDURE _hmg_printer_send_mail()  /* Added by GF, February 2022 */
*------------------------------------------------------------------------------*
   LOCAL oMail, cName, cFile, cText
   LOCAL cSMTP, nPort, cUserName, cFrom, cPassWord, cTo, cTo2, cCopy
   LOCAL lReceipt, nPriority
   LOCAL cIniFile := GetStartupFolder() + hb_ps() + 'mail.cfg'
   LOCAL aCustomer := {}

   IF File( cIniFile )
      _HMG_PRINTER_PPNAV.b8.Enabled := .F.
      HMG_SysWait()
      cName := _hmg_printer_JobName
      cFile := hb_cwd() + StrTran( cName, ' ', '_' ) + '_' + ;
               Left( hb_TToS( hb_DateTime() ), 14 ) + ".pdf"

      _hmg_printer_save_pdf_pages( cFile, .f., cName )

      IF ! File( cFile )
         MsgInfo( "PDF not saved to send Email" )
      ELSE
         cSMTP     := ""
         nPort     := 0
         cUserName := GetUserName()
         cFrom     := ""
         cPassWord := ""
         cTo       := ""
         cTo2      := ""
         cCopy     := ""
         cText     := ""
         lReceipt  := .F.
         nPriority := 1

         BEGIN INI FILE (cIniFile)
            GET cSMTP     SECTION 'Mail' ENTRY 'Server' DEFAULT cSMTP
            GET nPort     SECTION 'Mail' ENTRY 'Port' DEFAULT nPort
            GET cUserName SECTION 'Mail' ENTRY 'UserName' DEFAULT cUserName
            GET cFrom     SECTION 'Mail' ENTRY 'From' DEFAULT cFrom
            GET cPassWord SECTION 'Mail' ENTRY 'PassWord' DEFAULT cPassWord
            GET cTo       SECTION 'Mail' ENTRY 'Recipient1' DEFAULT cTo
            GET cTo2      SECTION 'Mail' ENTRY 'Recipient2' DEFAULT cTo2
            GET cCopy     SECTION 'Mail' ENTRY 'Copy' DEFAULT cCopy
            GET cText     SECTION 'Mail' ENTRY 'TextBody' DEFAULT cText
            GET lReceipt  SECTION 'Mail' ENTRY 'Receipt' DEFAULT lReceipt
            GET nPriority SECTION 'Mail' ENTRY 'Priority' DEFAULT nPriority
         END INI

         IF ! Empty( cTo )
            aAdd( aCustomer, { cTo, NIL } )
         ENDIF
         IF ! Empty( cTo2 )
            aAdd( aCustomer, { cTo2, NIL } )
         ENDIF

         DEFINE MAIL oMail ;
            SUBJECT cName ;
            TEXT cText ;
            FILES cFile

         WITH OBJECT oMail
            :cServer     := cSMTP
         IF ! Empty( nPort )
            :nPort       := nPort
         ENDIF
            :cUser       := cFrom
            :cPass       := cPassWord
            :aOrigin     := { cUserName, cFrom }
            :aRecipients := aCustomer 
         IF ! Empty( cCopy )
            :cCopy       := cCopy
         ENDIF
            :lReceipt    := lReceipt
            :nPriority   := nPriority
         END WITH

         ACTIVATE MAIL oMail

         IF oMail:lSuccess ; MsgInfo( "Mail sent" ) ; ENDIF
      ENDIF
#ifndef __DEBUG__
      IF File( cFile )
         hb_vfErase( cFile )
      ENDIF
#endif
      _HMG_PRINTER_PPNAV.b8.Enabled := .T.
   ENDIF

   _HMG_PRINTER_SHOWPREVIEW.SetFocus

   RETURN
*------------------------------------------------------------------------------*
PROCEDURE _HMG_PRINTER_GO_TO_PAGE
*------------------------------------------------------------------------------*
   
   DisableWindow ( GetFormHandle ( "_HMG_PRINTER_PPNAV" ) )
   
   DisableWindow ( GetFormHandle ( "_HMG_PRINTER_SHOWTHUMBNAILS" ) )
   
   DisableWindow ( GetFormHandle ( "_HMG_PRINTER_SHOWPREVIEW" ) )
   
   ShowWindow ( GetFormHandle ( "_HMG_PRINTER_GO_TO_PAGE" ) )
   
   RETURN
*------------------------------------------------------------------------------*
STATIC PROCEDURE _HMG_PRINTER_hScrollBoxProcess()
*------------------------------------------------------------------------------*
   LOCAL Sp
   
   Sp := GetScrollPos ( GetFormHandle( '_HMG_PRINTER_SHOWPREVIEW' ) , SB_HORZ )
   
   _hmg_printer_Dx := -( Sp - 50 ) * 10
   
   _HMG_PRINTER_PREVIEWRefresh()
   
   RETURN
*------------------------------------------------------------------------------*
STATIC PROCEDURE _HMG_PRINTER_vScrollBoxProcess()
*------------------------------------------------------------------------------*
   LOCAL Sp
   
   Sp := GetScrollPos ( GetFormHandle( '_HMG_PRINTER_SHOWPREVIEW' ) , SB_VERT )
   
   _hmg_printer_Dy := -( Sp - 50 ) * 10
   
   _HMG_PRINTER_PREVIEWRefresh()
   
   RETURN
*------------------------------------------------------------------------------*
PROCEDURE _HMG_PRINTER_PreviewClose()
*------------------------------------------------------------------------------*
   
   _HMG_PRINTER_CleanPreview()
   
   _HMG_PRINTER_WAIT.Release
   _HMG_PRINTER_SHOWTHUMBNAILS.Release
   _HMG_PRINTER_GO_TO_PAGE.Release
   _HMG_PRINTER_PRINTPAGES.Release
   _HMG_PRINTER_PPNAV.Release
   
   RETURN
*------------------------------------------------------------------------------*
STATIC FUNCTION _HMG_PRINTER_SpltChldMouseCursor()   // Claudio Soto, April 2014
*------------------------------------------------------------------------------*
   LOCAL hWnd, aPos, IsPoint, aCoords := _SetGetGlobal( 'aCoords' )
   
   IF _SetGetGlobal( "_HMG_PRINTER_lFlag" ) == NIL
      STATIC _HMG_PRINTER_lFlag AS GLOBAL VALUE .F.
   ENDIF
   
   IF _IsWindowDefined ( "_HMG_PRINTER_SHOWPREVIEW" ) .AND. ValType( aCoords ) == 'A'
      hWnd := GetFormHandle( "_HMG_PRINTER_SHOWPREVIEW" )
      aPos := GetCursorPos( hWnd )
      
      IsPoint := PtInRect( aPos, aCoords )
      
      IF IsPoint == .T. .AND. _SetGetGlobal( "_HMG_PRINTER_lFlag" ) == .F.
         ASSIGN GLOBAL _HMG_PRINTER_lFlag := .T.
         SetWindowCursor( hWnd, IMG_CURSOR )
      ELSEIF IsPoint == .F. .AND. _SetGetGlobal( "_HMG_PRINTER_lFlag" ) == .T.
         ASSIGN GLOBAL _HMG_PRINTER_lFlag := .F.
         SetWindowCursor( hWnd, IDC_ARROW )
      ENDIF
   ENDIF
   
   RETURN _SetGetGlobal( "_HMG_PRINTER_lFlag" )
*------------------------------------------------------------------------------*
FUNCTION _HMG_PRINTER_SpltChldMouseClick( hWnd, nMsg, wParam, lParam )   // Pablo Cesar and Claudio Soto, April 2014
*------------------------------------------------------------------------------*
   LOCAL RetVal := NIL
   LOCAL Flag := _HMG_PRINTER_SpltChldMouseCursor()
   
   HB_SYMBOL_UNUSED( lParam )
   
   #define WM_SETCURSOR 32
   IF nMsg == WM_SETCURSOR
      IF wParam == GetControlHandle( "TOOLBAR_1", "_HMG_PRINTER_PPNAV" )
         RetVal := 0
         DoMethod( "_HMG_PRINTER_PPNAV", "SetFocus" )   // SetFocus for display ToolTip of the ToolBar define into SPLITBOX
      ENDIF
   ENDIF

   #define WM_LBUTTONDOWN 513
   IF nMsg == WM_LBUTTONDOWN .AND. Flag == .T.
      IF hWnd == GetFormHandle( "_HMG_PRINTER_SHOWPREVIEW" )  // Click in show page to print
         RetVal := 0
         _HMG_PRINTER_PPNAV.b5.Value := ! _HMG_PRINTER_PPNAV.b5.Value
         _HMG_PRINTER_MouseZoom()
      ENDIF
   ENDIF

RETURN RetVal
*------------------------------------------------------------------------------*
STATIC PROCEDURE _HMG_PRINTER_CleanPreview
*------------------------------------------------------------------------------*
   LOCAL t := GetTempFolder() + hb_ps()
   
   AEval( Directory( t + _hmg_printer_timestamp + "_hmg_print_preview_*.Emf" ), ;
      { | File | FErase( t + File[ 1 ] ) } )
   
   RETURN
*------------------------------------------------------------------------------*
PROCEDURE _HMG_PRINTER_PREVIEWRefresh
*------------------------------------------------------------------------------*
   LOCAL hwnd
   LOCAL nRow
   LOCAL nScrollMax
   
   IF _IsControlDefined ( 'Image' + AllTrim( Str( _hmg_printer_CurrentPageNumber ) ) , '_HMG_PRINTER_SHOWTHUMBNAILS' ) .AND. _HMG_PRINTER_THUMBUPDATE == .T. .AND. _hmg_printer_thumbscroll == .T.
      
      IF _hmg_printer_PrevPageNumber != _hmg_printer_CurrentPageNumber
         
         _hmg_printer_PrevPageNumber := _hmg_printer_CurrentPageNumber
         hwnd := GetFormHandle( '_HMG_PRINTER_SHOWTHUMBNAILS' )
         nRow := GetProperty ( '_HMG_PRINTER_SHOWTHUMBNAILS' , 'Image' + AllTrim( Str( _hmg_printer_CurrentPageNumber ) ) , 'Row' )
         nScrollMax := GetScrollRangeMax ( hwnd , SB_VERT )
         
         IF _hmg_printer_PageCount == _hmg_printer_CurrentPageNumber
            
            IF GetScrollPos( hwnd, SB_VERT ) != nScrollMax
               _HMG_SETVSCROLLVALUE ( hwnd , nScrollMax )
            ENDIF
            
         ELSEIF _hmg_printer_CurrentPageNumber == 1
            
            IF GetScrollPos( hwnd, SB_VERT ) != 0
               _HMG_SETVSCROLLVALUE ( hwnd , 0 )
            ENDIF
            
         ELSE
            
            IF ( nRow - 9 ) < nScrollMax
               _HMG_SETVSCROLLVALUE ( hwnd , nRow - 9 )
            ELSE
               IF GetScrollPos( hwnd, SB_VERT ) != nScrollMax
                  _HMG_SETVSCROLLVALUE ( hwnd , nScrollMax )
               ENDIF
            ENDIF
            
         ENDIF
         
      ENDIF
      
   ENDIF
   
   IF _hmg_printer_CurrentPageNumber < 1
      _hmg_printer_CurrentPageNumber := 1
      PlayBeep()
      RETURN
   ENDIF
   
   IF _hmg_printer_CurrentPageNumber > _hmg_printer_PageCount
      _hmg_printer_CurrentPageNumber := _hmg_printer_PageCount
      PlayBeep()
      RETURN
   ENDIF
   
   InvalidateRect ( GetFormHandle ( '_HMG_PRINTER_SHOWPREVIEW' ) , 0 )
   
   STATIC aCoords AS GLOBAL VALUE ;
   _HMG_PRINTER_SHOWPAGE ( _hmg_printer_BasePageName + StrZero( _hmg_printer_CurrentPageNumber, 4 ) + ".emf" , GetFormHandle ( '_HMG_PRINTER_SHOWPREVIEW' ) , _hmg_printer_hdc_bak , _hmg_printer_SizeFactor * 10000 , _hmg_printer_Dz , _hmg_printer_Dx , _hmg_printer_Dy )
   
   _HMG_PRINTER_PPNAV.TITLE := _hmg_printer_usermessages [ 01 ] + ' [' + AllTrim( Str( _hmg_printer_CurrentPageNumber ) ) + '/' + AllTrim( Str( _hmg_printer_PageCount ) ) + ']'
   
   RETURN
*------------------------------------------------------------------------------*
PROCEDURE _HMG_PRINTER_PrintPages
*------------------------------------------------------------------------------*
   
   DIsableWindow ( GetFormHandle ( "_HMG_PRINTER_PPNAV" ) )
   DIsableWindow ( GetFormHandle ( "_HMG_PRINTER_SHOWTHUMBNAILS" ) )
   DIsableWindow ( GetFormHandle ( "_HMG_PRINTER_SHOWPREVIEW" ) )
   
   _HMG_PRINTER_PRINTPAGES.Radio_1.Value := 1
   
   _HMG_PRINTER_PRINTPAGES.Label_1.Enabled := .F.
   _HMG_PRINTER_PRINTPAGES.Label_2.Enabled := .F.
   _HMG_PRINTER_PRINTPAGES.Label_4.Enabled := .F.
   _HMG_PRINTER_PRINTPAGES.Spinner_1.Enabled := .F.
   _HMG_PRINTER_PRINTPAGES.Spinner_2.Enabled := .F.
   _HMG_PRINTER_PRINTPAGES.Combo_1.Enabled := .F.
   _HMG_PRINTER_PRINTPAGES.CheckBox_1.Enabled := .F.
   
   IF   _hmg_printer_usercopies == .T. ;
      .OR. ;
      _hmg_printer_usercollate == .T.
      
      _HMG_PRINTER_PRINTPAGES.Spinner_3.Enabled := .F.
      
   ENDIF
   
   ShowWindow ( GetFormHandle ( "_HMG_PRINTER_PRINTPAGES" ) )
   
   RETURN
*------------------------------------------------------------------------------*
PROCEDURE _HMG_PRINTER_PrintPagesDo
*------------------------------------------------------------------------------*
   LOCAL i
   LOCAL PageFrom
   LOCAL PageTo
   LOCAL p
   LOCAL OddOnly := .F.
   LOCAL EvenOnly := .F.
   
   IF _HMG_PRINTER_PrintPages.Radio_1.Value == 1
      
      PageFrom := 1
      PageTo    := _hmg_printer_PageCount
      
   ELSEIF _HMG_PRINTER_PrintPages.Radio_1.Value == 2
      
      PageFrom := _HMG_PRINTER_PrintPages.Spinner_1.Value
      PageTo    := _HMG_PRINTER_PrintPages.Spinner_2.Value
      
      IF _HMG_PRINTER_PrintPages.Combo_1.Value == 2
         OddOnly := .T.
      ELSEIF _HMG_PRINTER_PrintPages.Combo_1.Value == 3
         EvenOnly := .T.
      ENDIF
      
   ENDIF
   
   _hmg_printer_JobId := _HMG_PRINTER_StartDoc ( _hmg_printer_hdc_bak, _hmg_printer_JobName )
   
   IF ! Empty ( _hmg_printer_JobData )
      IF __mvExist( _hmg_printer_JobData )
         __mvPut( _hmg_printer_JobData , OpenPrinterGetJobData() )
      ELSE
         MsgMiniGuiError ( "START PRINTDOC STOREJOBDATA: " + _hmg_printer_JobData + " must be declared as Public or Private." )
      ENDIF
   ENDIF
   
   IF _HMG_PRINTER_PrintPages.Spinner_3.Value == 1 // Copies
      
      FOR i := PageFrom TO PageTo
         
         IF OddOnly == .T.
            IF i / 2 != Int ( i / 2 )
               _HMG_PRINTER_PRINTPAGE ( _hmg_printer_hdc_bak , _hmg_printer_BasePageName + StrZero( i, 4 ) + ".emf" )
            ENDIF
         ELSEIF EvenOnly == .T.
            IF i / 2 == Int ( i / 2 )
               _HMG_PRINTER_PRINTPAGE ( _hmg_printer_hdc_bak , _hmg_printer_BasePageName + StrZero( i, 4 ) + ".emf" )
            ENDIF
         ELSE
            _HMG_PRINTER_PRINTPAGE ( _hmg_printer_hdc_bak , _hmg_printer_BasePageName + StrZero( i, 4 ) + ".emf" )
         ENDIF
         
      NEXT i
      
   ELSE
      
      IF _HMG_PRINTER_PrintPages.CheckBox_1.Value == .F.
         
         FOR p := 1 TO _HMG_PRINTER_PrintPages.Spinner_3.Value
            
            FOR i := PageFrom TO PageTo
               
               IF OddOnly == .T.
                  IF i / 2 != Int ( i / 2 )
                     _HMG_PRINTER_PRINTPAGE ( _hmg_printer_hdc_bak , _hmg_printer_BasePageName + StrZero( i, 4 ) + ".emf" )
                  ENDIF
               ELSEIF EvenOnly == .T.
                  IF i / 2 == Int ( i / 2 )
                     _HMG_PRINTER_PRINTPAGE ( _hmg_printer_hdc_bak , _hmg_printer_BasePageName + StrZero( i, 4 ) + ".emf" )
                  ENDIF
               ELSE
                  _HMG_PRINTER_PRINTPAGE ( _hmg_printer_hdc_bak , _hmg_printer_BasePageName + StrZero( i, 4 ) + ".emf" )
               ENDIF
               
            NEXT i
            
         NEXT p

      ELSE
         
         FOR i := PageFrom TO PageTo
            
            FOR p := 1 TO _HMG_PRINTER_PrintPages.Spinner_3.Value
               
               IF OddOnly == .T.
                  IF i / 2 != Int ( i / 2 )
                     _HMG_PRINTER_PRINTPAGE ( _hmg_printer_hdc_bak , _hmg_printer_BasePageName + StrZero( i, 4 ) + ".emf" )
                  ENDIF
               ELSEIF EvenOnly == .T.
                  IF i / 2 == Int ( i / 2 )
                     _HMG_PRINTER_PRINTPAGE ( _hmg_printer_hdc_bak , _hmg_printer_BasePageName + StrZero( i, 4 ) + ".emf" )
                  ENDIF
               ELSE
                  _HMG_PRINTER_PRINTPAGE ( _hmg_printer_hdc_bak , _hmg_printer_BasePageName + StrZero( i, 4 ) + ".emf" )
               ENDIF
               
            NEXT p
            
         NEXT i
         
      ENDIF

   ENDIF
   
   _HMG_PRINTER_ENDDOC ( _hmg_printer_hdc_bak )
   
   EnableWindow ( GetFormHandle ( "_HMG_PRINTER_SHOWPREVIEW" ) )
   EnableWindow ( GetFormHandle ( "_HMG_PRINTER_SHOWTHUMBNAILS" ) )
   EnableWindow ( GetFormHandle ( "_HMG_PRINTER_PPNAV" ) )
   
   HideWindow ( GetFormHandle ( "_HMG_PRINTER_PRINTPAGES" ) )
   
   _HMG_PRINTER_SHOWPREVIEW.setfocus
   
   RETURN
*------------------------------------------------------------------------------*
STATIC PROCEDURE _HMG_PRINTER_ScrollLeft
*------------------------------------------------------------------------------*
   _hmg_printer_Dx := _hmg_printer_Dx + _hmg_printer_scrollstep
   IF _hmg_printer_Dx >= 500
      _hmg_printer_Dx := 500
      PlayBeep()
   ENDIF
   _HMG_PRINTER_PREVIEWRefresh()
   RETURN
*------------------------------------------------------------------------------*
STATIC PROCEDURE _HMG_PRINTER_ScrollRight
*------------------------------------------------------------------------------*
   _hmg_printer_Dx := _hmg_printer_Dx - _hmg_printer_scrollstep
   IF _hmg_printer_Dx <= - 500
      _hmg_printer_Dx := - 500
      PlayBeep()
   ENDIF
   _HMG_PRINTER_PREVIEWRefresh()
   RETURN
*------------------------------------------------------------------------------*
STATIC PROCEDURE _HMG_PRINTER_ScrollUp
*------------------------------------------------------------------------------*
   _hmg_printer_Dy := _hmg_printer_Dy + _hmg_printer_scrollstep
   IF _hmg_printer_Dy >= 500
      _hmg_printer_Dy := 500
      PlayBeep()
   ENDIF
   _HMG_PRINTER_PREVIEWRefresh()
   RETURN
*------------------------------------------------------------------------------*
STATIC PROCEDURE _HMG_PRINTER_ScrollDown
*------------------------------------------------------------------------------*
   _hmg_printer_Dy := _hmg_printer_Dy - _hmg_printer_scrollstep
   IF _hmg_printer_Dy <= - 500
      _hmg_printer_Dy := - 500
      PlayBeep()
   ENDIF
   
   _HMG_PRINTER_PREVIEWRefresh()
   RETURN
*------------------------------------------------------------------------------*
FUNCTION GetPrinter()
*------------------------------------------------------------------------------*
   LOCAL RetVal      := ''
   LOCAL Printers      := ASort ( aPrinters() )
   LOCAL cDefaultPrinter   := GetDefaultPrinter()
   LOCAL i
   LOCAL nInitPosition   := 0
   
   FOR i := 1 TO Len ( Printers )
      
      IF Printers [ i ] == cDefaultPrinter
         nInitPosition := i
         EXIT
      ENDIF
      
   NEXT i
   
   IF Type ( '_HMG_MINIPRINT[22]' ) == "U"
      _hmg_printer_InitUserMessages()
   ENDIF
   
   DEFINE WINDOW _HMG_PRINTER_GETPRINTER ;
          At 0, 0 ;
          WIDTH 345 ;
          HEIGHT GetTitleHeight() + 100 ;
          TITLE _hmg_printer_usermessages [ 13 ] ;
          MODAL ;
          NOSIZE

      @ 15, 10 COMBOBOX Combo_1 ITEMS Printers VALUE nInitPosition WIDTH 320
      
      @ 53 , 65  BUTTON Ok CAPTION _hmg_printer_usermessages [ 11 ] ACTION ( RetVal := Printers [ GetProperty ( '_HMG_PRINTER_GETPRINTER', 'Combo_1', 'Value' ) ] , DoMethod( '_HMG_PRINTER_GETPRINTER', 'Release' ) )
      
      @ 53 , 175 BUTTON Cancel CAPTION _hmg_printer_usermessages [ 12 ] ACTION ( RetVal := '' , DoMethod( '_HMG_PRINTER_GETPRINTER', 'Release' ) )
      
      ON KEY ESCAPE ACTION _HMG_PRINTER_GETPRINTER.Cancel.OnClick ()
   END WINDOW
   
   CENTER WINDOW _HMG_PRINTER_GETPRINTER
   
   ACTIVATE WINDOW _HMG_PRINTER_GETPRINTER
   
   RETURN ( RetVal )
   
#define TA_CENTER   6
#define TA_LEFT      0
#define TA_RIGHT   2

*------------------------------------------------------------------------------*
PROCEDURE _HMG_PRINTER_H_PRINT ( nHdc , nRow , nCol , cFontName , nFontSize , nColor1 , nColor2 , nColor3 , cText , lbold , litalic , lunderline , lstrikeout , lcolor , lfont , lsize , cAlign , lAngle , nAngle )
*------------------------------------------------------------------------------*
   LOCAL lAlignChanged := .F.
   
   DEFAULT lAngle TO .F., ;
   nAngle TO 0
   
   IF ValType ( cText ) == "N"
      cText := AllTrim( Str( cText ) )
   ELSEIF ValType ( cText ) == "D"
      cText := DToC ( cText )
   ELSEIF ValType ( cText ) == "L"
      cText := Iif ( cText == .T. , _hmg_printer_usermessages [ 24 ] , _hmg_printer_usermessages [ 25 ] )
   ELSEIF ValType ( cText ) == "A"
      RETURN
   ELSEIF ValType ( cText ) == "B"
      RETURN
   ELSEIF ValType ( cText ) == "O"
      RETURN
   ELSEIF ValType ( cText ) == "U"
      RETURN
   ENDIF
   
   nRow := Int ( nRow * 10000 / 254 )
   nCol := Int ( nCol * 10000 / 254 )
   
   IF ValType ( cAlign ) = 'C'
      IF Upper ( cAlign ) = 'CENTER'
         SetTextAlign ( nHdc , TA_CENTER )
         lAlignChanged := .T.
      ELSEIF Upper ( cAlign ) = 'RIGHT'
         SetTextAlign ( nHdc , TA_RIGHT )
         lAlignChanged := .T.
      ENDIF
   ENDIF
   
   IF lAngle
      nAngle *= 10
   ENDIF
   
   _HMG_PRINTER_C_PRINT ( nHdc , nRow , nCol , cFontName , nFontSize , nColor1 , nColor2 , nColor3 , cText , lbold , litalic , lunderline , lstrikeout , lcolor , lfont , lsize , lAngle , nAngle )
   
   IF lAlignChanged
      SetTextAlign ( nHdc , TA_LEFT )
   ENDIF

   RETURN
   
*------------------------------------------------------------------------------*
PROCEDURE _HMG_PRINTER_H_MULTILINE_PRINT ( nHdc , nRow , nCol , nToRow , nToCol , cFontName , nFontSize , nColor1 , nColor2 , nColor3 , cText , lbold , litalic , lunderline , lstrikeout , lcolor , lfont , lsize , cAlign )
*------------------------------------------------------------------------------*
   LOCAL nAlign := TA_LEFT
   
   IF ValType ( cText ) == "N"
      cText := AllTrim( Str( cText ) )
   ELSEIF ValType ( cText ) == "D"
      cText := DToC ( cText )
   ELSEIF ValType ( cText ) == "L"
      cText := Iif ( cText == .T. , _hmg_printer_usermessages [ 24 ] , _hmg_printer_usermessages [ 25 ] )
   ELSEIF ValType ( cText ) == "A"
      RETURN
   ELSEIF ValType ( cText ) == "B"
      RETURN
   ELSEIF ValType ( cText ) == "O"
      RETURN
   ELSEIF ValType ( cText ) == "U"
      RETURN
   ENDIF
   
   nRow := Int ( nRow * 10000 / 254 )
   nCol := Int ( nCol * 10000 / 254 )
   nToRow := Int ( nToRow * 10000 / 254 )
   nToCol := Int ( nToCol * 10000 / 254 )
   
   IF ValType ( cAlign ) = 'C'
      IF Upper ( cAlign ) = 'CENTER'
         nAlign := TA_CENTER
      ELSEIF Upper ( cAlign ) = 'RIGHT'
         nAlign := TA_RIGHT
      ENDIF
   ENDIF
   
   _HMG_PRINTER_C_MULTILINE_PRINT ( nHdc , nRow , nCol , cFontName , nFontSize , nColor1 , nColor2 , nColor3 , cText , lbold , litalic , lunderline , lstrikeout , lcolor , lfont , lsize , nToRow , nToCol , nAlign )
   
   RETURN
*------------------------------------------------------------------------------*
PROCEDURE _HMG_PRINTER_H_IMAGE ( nHdc , cImage , nRow , nCol , nHeight , nWidth , lStretch , lTransparent )
*------------------------------------------------------------------------------*

   nRow   := Int ( nRow * 10000 / 254 )
   nCol   := Int ( nCol * 10000 / 254 )
   nWidth   := Int ( nWidth * 10000 / 254 )
   nHeight   := Int ( nHeight * 10000 / 254 )
   
   _HMG_PRINTER_C_IMAGE ( nHdc , cImage , nRow , nCol , nHeight , nWidth , lStretch , lTransparent )
   
   RETURN
*------------------------------------------------------------------------------*
PROCEDURE _HMG_PRINTER_H_LINE ( nHdc , nRow , nCol , nToRow , nToCol , nWidth , nColor1 , nColor2 , nColor3 , lwidth , lcolor , nstyle )
*------------------------------------------------------------------------------*
   
   nRow   := Int ( nRow * 10000 / 254 )
   nCol   := Int ( nCol * 10000 / 254 )
   nToRow   := Int ( nToRow * 10000 / 254 )
   nToCol   := Int ( nToCol * 10000 / 254 )
   
   DEFAULT nstyle TO 0
   
   IF ValType ( nWidth ) != 'U'
      nWidth   := Int ( nWidth * 10000 / 254 )
   ELSEIF nstyle > 0
      IF nstyle == 1
         nWidth := 1
      ELSEIF nstyle == 2
         nWidth   := 3
      ELSEIF nstyle == 3
         nWidth := 10
      ELSEIF nstyle == 4
         nWidth := 12
      ENDIF
      lwidth   := .T.
   ENDIF
   
   _HMG_PRINTER_C_LINE ( nHdc , nRow , nCol , nToRow , nToCol , nWidth , nColor1 , nColor2 , nColor3 , lwidth , lcolor , nstyle )
   
   RETURN
*------------------------------------------------------------------------------*
PROCEDURE _HMG_PRINTER_H_RECTANGLE ( nHdc , nRow , nCol , nToRow , nToCol , nWidth , nColor1 , nColor2 , nColor3 , lwidth , lcolor , lfilled , lnoborder )
*------------------------------------------------------------------------------*
   
   nRow   := Int ( nRow * 10000 / 254 )
   nCol   := Int ( nCol * 10000 / 254 )
   nToRow   := Int ( nToRow * 10000 / 254 )
   nToCol   := Int ( nToCol * 10000 / 254 )
   
   IF ValType ( nWidth ) != 'U'
      nWidth   := Int ( nWidth * 10000 / 254 )
   ENDIF
   
   _HMG_PRINTER_C_RECTANGLE ( nHdc , nRow , nCol , nToRow , nToCol , nWidth , nColor1 , nColor2 , nColor3 , lwidth , lcolor, lfilled, lnoborder )
   
   RETURN
*------------------------------------------------------------------------------*
PROCEDURE _HMG_PRINTER_H_ROUNDRECTANGLE ( nHdc , nRow , nCol , nToRow , nToCol , nWidth , nColor1 , nColor2 , nColor3 , lwidth , lcolor, lfilled )
*------------------------------------------------------------------------------*

   nRow   := Int ( nRow * 10000 / 254 )
   nCol   := Int ( nCol * 10000 / 254 )
   nToRow   := Int ( nToRow * 10000 / 254 )
   nToCol   := Int ( nToCol * 10000 / 254 )

   IF ValType ( nWidth ) != 'U'
      nWidth   := Int ( nWidth * 10000 / 254 )
   ENDIF

   _HMG_PRINTER_C_ROUNDRECTANGLE ( nHdc , nRow , nCol , nToRow , nToCol , nWidth , nColor1 , nColor2 , nColor3 , lwidth , lcolor, lfilled )

   RETURN

   
*------------------------------------------------------------------------------*
FUNCTION GETPRINTABLEAREAWIDTH()
*------------------------------------------------------------------------------*

   RETURN _HMG_PRINTER_GETPRINTERWIDTH ( _hmg_printer_hdc )
   
*------------------------------------------------------------------------------*
FUNCTION GETPRINTABLEAREAHEIGHT()
*------------------------------------------------------------------------------*
   
   RETURN _HMG_PRINTER_GETPRINTERHEIGHT ( _hmg_printer_hdc )
   
*------------------------------------------------------------------------------*
FUNCTION GETPRINTABLEAREAHORIZONTALOFFSET()
*------------------------------------------------------------------------------*

   IF Type ( '_hmg_miniprint[19]' ) == 'U'
      RETURN 0
   ENDIF
   
   RETURN ( _HMG_PRINTER_GETPRINTABLEAREAPHYSICALOFFSETX ( _hmg_printer_hdc ) / _HMG_PRINTER_GETPRINTABLEAREALOGPIXELSX ( _hmg_printer_hdc ) * 25.4 )
   
*------------------------------------------------------------------------------*
FUNCTION GETPRINTABLEAREAVERTICALOFFSET()
*------------------------------------------------------------------------------*

   IF Type ( '_hmg_miniprint[19]' ) == 'U'
      RETURN 0
   ENDIF
   
   RETURN ( _HMG_PRINTER_GETPRINTABLEAREAPHYSICALOFFSETY ( _hmg_printer_hdc ) / _HMG_PRINTER_GETPRINTABLEAREALOGPIXELSY ( _hmg_printer_hdc ) * 25.4 )
   
*------------------------------------------------------------------------------*
PROCEDURE _HMG_PRINTER_MouseZoom
*------------------------------------------------------------------------------*
   LOCAL WIDTH := GetDeskTopWidth()
   LOCAL HEIGHT := GetDeskTopHeight()
   LOCAL Q := 0
   LOCAL DeltaHeight := 35 + GetTitleHeight() + GetBorderHeight() + 10
   
   IF _hmg_printer_Dz == 1000 + _HMG_PRINTER_DELTA_ZOOM
      
      _hmg_printer_Dz := 0
      _hmg_printer_Dx := 0
      _hmg_printer_Dy := 0
      
      SetScrollPos ( GetFormHandle( '_HMG_PRINTER_SHOWPREVIEW' ) , SB_VERT , 50 , .T. )
      SetScrollPos ( GetFormHandle( '_HMG_PRINTER_SHOWPREVIEW' ) , SB_HORZ , 50 , .T. )
      
      _HMG_PRINTER_PREVIEW_DISABLESCROLLBARS ( GetFormHandle( '_HMG_PRINTER_SHOWPREVIEW' ) )
      
   ELSE
      
      * Calculate Quadrant
      
      IF   _HMG_MouseCol <= ( WIDTH / 2 ) - _hmg_printer_zoomclick_xoffset ;
         .AND. ;
         _HMG_MouseRow <= ( HEIGHT / 2 )   - DeltaHeight

         Q := 1
         
      ELSEIF   _HMG_MouseCol > ( WIDTH / 2 ) - _hmg_printer_zoomclick_xoffset ;
         .AND. ;
         _HMG_MouseRow <= ( HEIGHT / 2 )   - DeltaHeight
         
         Q := 2
         
      ELSEIF   _HMG_MouseCol <= ( WIDTH / 2 ) - _hmg_printer_zoomclick_xoffset ;
         .AND. ;
         _HMG_MouseRow > ( HEIGHT / 2 ) - DeltaHeight
         
         Q := 3
         
      ELSEIF   _HMG_MouseCol > ( WIDTH / 2 ) - _hmg_printer_zoomclick_xoffset ;
         .AND. ;
         _HMG_MouseRow > ( HEIGHT / 2 ) - DeltaHeight
         
         Q := 4

      ENDIF
      
      IF   _HMG_PRINTER_GETPAGEHEIGHT( _hmg_printer_hdc_bak ) ;
         > ;
         _HMG_PRINTER_GETPAGEWIDTH( _hmg_printer_hdc_bak )
         
         * Portrait

         IF Q == 1
            _hmg_printer_Dz := 1000 + _HMG_PRINTER_DELTA_ZOOM
            _hmg_printer_Dx := 100
            _hmg_printer_Dy := 400
            SetScrollPos ( GetFormHandle( '_HMG_PRINTER_SHOWPREVIEW' ) , SB_VERT , 10 , .T. )
            SetScrollPos ( GetFormHandle( '_HMG_PRINTER_SHOWPREVIEW' ) , SB_HORZ , 40 , .T. )
         ELSEIF Q == 2
            _hmg_printer_Dz := 1000 + _HMG_PRINTER_DELTA_ZOOM
            _hmg_printer_Dx := - 100
            _hmg_printer_Dy := 400
            SetScrollPos ( GetFormHandle( '_HMG_PRINTER_SHOWPREVIEW' ) , SB_VERT , 10 , .T. )
            SetScrollPos ( GetFormHandle( '_HMG_PRINTER_SHOWPREVIEW' ) , SB_HORZ , 60 , .T. )
         ELSEIF Q == 3
            _hmg_printer_Dz := 1000 + _HMG_PRINTER_DELTA_ZOOM
            _hmg_printer_Dx := 100
            _hmg_printer_Dy := - 400
            SetScrollPos ( GetFormHandle( '_HMG_PRINTER_SHOWPREVIEW' ) , SB_VERT , 90 , .T. )
            SetScrollPos ( GetFormHandle( '_HMG_PRINTER_SHOWPREVIEW' ) , SB_HORZ , 40 , .T. )
         ELSEIF Q == 4
            _hmg_printer_Dz := 1000 + _HMG_PRINTER_DELTA_ZOOM
            _hmg_printer_Dx := - 100
            _hmg_printer_Dy := - 400
            SetScrollPos ( GetFormHandle( '_HMG_PRINTER_SHOWPREVIEW' ) , SB_VERT , 90 , .T. )
            SetScrollPos ( GetFormHandle( '_HMG_PRINTER_SHOWPREVIEW' ) , SB_HORZ , 60 , .T. )
         ENDIF
         
      ELSE
         
         * Landscape
         
         IF Q == 1
            _hmg_printer_Dz := 1000 + _HMG_PRINTER_DELTA_ZOOM
            _hmg_printer_Dx := 500
            _hmg_printer_Dy := 300
            SetScrollPos ( GetFormHandle( '_HMG_PRINTER_SHOWPREVIEW' ) , SB_VERT , 20 , .T. )
            SetScrollPos ( GetFormHandle( '_HMG_PRINTER_SHOWPREVIEW' ) , SB_HORZ , 1 , .T. )
         ELSEIF Q == 2
            _hmg_printer_Dz := 1000 + _HMG_PRINTER_DELTA_ZOOM
            _hmg_printer_Dx := - 500
            _hmg_printer_Dy := 300
            SetScrollPos ( GetFormHandle( '_HMG_PRINTER_SHOWPREVIEW' ) , SB_VERT , 20 , .T. )
            SetScrollPos ( GetFormHandle( '_HMG_PRINTER_SHOWPREVIEW' ) , SB_HORZ , 99 , .T. )
         ELSEIF Q == 3
            _hmg_printer_Dz := 1000 + _HMG_PRINTER_DELTA_ZOOM
            _hmg_printer_Dx := 500
            _hmg_printer_Dy := - 300
            SetScrollPos ( GetFormHandle( '_HMG_PRINTER_SHOWPREVIEW' ) , SB_VERT , 80 , .T. )
            SetScrollPos ( GetFormHandle( '_HMG_PRINTER_SHOWPREVIEW' ) , SB_HORZ , 1 , .T. )
         ELSEIF Q == 4
            _hmg_printer_Dz := 1000 + _HMG_PRINTER_DELTA_ZOOM
            _hmg_printer_Dx := - 500
            _hmg_printer_Dy := - 300
            SetScrollPos ( GetFormHandle( '_HMG_PRINTER_SHOWPREVIEW' ) , SB_VERT , 80 , .T. )
            SetScrollPos ( GetFormHandle( '_HMG_PRINTER_SHOWPREVIEW' ) , SB_HORZ , 99 , .T. )
         ENDIF
         
      ENDIF
      
      _HMG_PRINTER_PREVIEW_ENABLESCROLLBARS ( GetFormHandle( '_HMG_PRINTER_SHOWPREVIEW' ) )
      
   ENDIF
   
   _HMG_PRINTER_PREVIEWRefresh()
   
   RETURN

*------------------------------------------------------------------------------*
PROCEDURE _HMG_PRINTER_Zoom
*------------------------------------------------------------------------------*

   IF _hmg_printer_Dz == 1000 + _HMG_PRINTER_DELTA_ZOOM
      
      _hmg_printer_Dz := 0
      _hmg_printer_Dx := 0
      _hmg_printer_Dy := 0
      
      SetScrollPos ( GetFormHandle( '_HMG_PRINTER_SHOWPREVIEW' ) , SB_VERT , 50 , .T. )
      SetScrollPos ( GetFormHandle( '_HMG_PRINTER_SHOWPREVIEW' ) , SB_HORZ , 50 , .T. )
      
      _HMG_PRINTER_PREVIEW_DISABLESCROLLBARS ( GetFormHandle( '_HMG_PRINTER_SHOWPREVIEW' ) )
      
   ELSE
      
      IF   _HMG_PRINTER_GETPAGEHEIGHT( _hmg_printer_hdc_bak ) ;
         > ;
         _HMG_PRINTER_GETPAGEWIDTH( _hmg_printer_hdc_bak )
         
         _hmg_printer_Dz := 1000 + _HMG_PRINTER_DELTA_ZOOM
         _hmg_printer_Dx := 100
         _hmg_printer_Dy := 400
         SetScrollPos ( GetFormHandle( '_HMG_PRINTER_SHOWPREVIEW' ) , SB_VERT , 10 , .T. )
         SetScrollPos ( GetFormHandle( '_HMG_PRINTER_SHOWPREVIEW' ) , SB_HORZ , 40 , .T. )
         
      ELSE
         
         _hmg_printer_Dz := 1000 + _HMG_PRINTER_DELTA_ZOOM
         _hmg_printer_Dx := 500
         _hmg_printer_Dy := 300
         SetScrollPos ( GetFormHandle( '_HMG_PRINTER_SHOWPREVIEW' ) , SB_VERT , 20 , .T. )
         SetScrollPos ( GetFormHandle( '_HMG_PRINTER_SHOWPREVIEW' ) , SB_HORZ , 1 , .T. )

      ENDIF
      
      _HMG_PRINTER_PREVIEW_ENABLESCROLLBARS ( GetFormHandle( '_HMG_PRINTER_SHOWPREVIEW' ) )

   ENDIF
   
   _HMG_PRINTER_PREVIEWRefresh()
   
   RETURN

*------------------------------------------------------------------------------*
FUNCTION _hmg_printer_setjobname( cName )
*------------------------------------------------------------------------------*
   RETURN hb_defaultValue( cName, 'HMGPrintSys' )
   
*------------------------------------------------------------------------------*
FUNCTION HMG_PrintGetJobInfo ( aJobData )   // by Dr. Claudio Soto, August 2015
*------------------------------------------------------------------------------*
   IF ValType( aJobData ) == "U"
      aJobData := OpenPrinterGetJobData()
   ENDIF
   
   RETURN _HMG_PrintGetJobInfo( aJobData [ 2 ], aJobData [ 1 ] ) // --> aJobInfo
   
*------------------------------------------------------------------------------*
FUNCTION HMG_PrinterGetStatus ( cPrinterName )
*------------------------------------------------------------------------------*
   IF ValType( cPrinterName ) == "U"
      cPrinterName := _hmg_printer_name
   ENDIF

   RETURN _HMG_PrinterGetStatus( cPrinterName ) // --> nStatus
   
*-----------------------------------------------------------------------------*
FUNCTION _DefineEmfFile ( ControlName, ParentFormName, x, y, FileName, w, h, ;
         ProcedureName, HELPID, invisible, STRETCH, WhiteBackground, TRANSPARENT )
*-----------------------------------------------------------------------------*
   LOCAL ParentFormHandle , mVar , ACTION := .F. , k
   LOCAL ControlHandle
   
   IF ValType( ProcedureName ) == "U"
      ProcedureName := ""
   ELSE
      ACTION := .T.
   ENDIF
   
   DEFAULT STRETCH TO FALSE, WhiteBackground TO FALSE, TRANSPARENT TO FALSE
   
   mVar := '_' + ParentFormName + '_' + ControlName
   k := _GetControlFree()
   
   ParentFormHandle := GetFormHandle ( ParentFormName )

   ControlHandle := InitEmfFile ( ParentFormHandle, 0, x, y, invisible, ACTION )
   
#ifdef _NAMES_LIST_
   _SetNameList( mVar , k )
#else
   PUBLIC &mVar. := k
#endif
   
   _HMG_aControlType  [ k ] :=  "IMAGE"
   _HMG_aControlNames [ k ] :=  ControlName
   _HMG_aControlHandles [ k ] :=  ControlHandle
   _HMG_aControlParentHandles  [ k ] :=  ParentFormHandle
   _HMG_aControlIds  [ k ] :=  0
   _HMG_aControlProcedures [ k ] :=  ProcedureName
   _HMG_aControlPageMap   [ k ] :=  { }
   _HMG_aControlValue  [ k ] :=  Iif ( STRETCH, 1, 0 )
   _HMG_aControlInputMask  [ k ] :=  Iif ( TRANSPARENT, 1, 0 )
   _HMG_aControllostFocusProcedure  [ k ] :=  ""
   _HMG_aControlGotFocusProcedure  [ k ] :=  ""
   _HMG_aControlChangeProcedure  [ k ] :=  ""
   _HMG_aControlDeleted  [ k ] :=  .F.
   _HMG_aControlBkColor  [ k ] :=  NIL
   _HMG_aControlFontColor  [ k ] :=  NIL
   _HMG_aControlDblClick  [ k ] :=  ""
   _HMG_aControlHeadClick  [ k ] :=  { }
   _HMG_aControlRow  [ k ] :=  y
   _HMG_aControlCol  [ k ] :=  x
   _HMG_aControlWidth  [ k ] :=  w
   _HMG_aControlHeight  [ k ] :=  h
   _HMG_aControlSpacing  [ k ] :=  Iif ( WhiteBackground, 1, 0 )
   _HMG_aControlContainerRow  [ k ] :=  - 1
   _HMG_aControlContainerCol  [ k ] :=  - 1
   _HMG_aControlPicture  [ k ] :=  FileName
   _HMG_aControlContainerHandle [ k ] :=  0
   _HMG_aControlFontName  [ k ] :=  ''
   _HMG_aControlFontSize  [ k ] :=  0
   _HMG_aControlFontAttributes  [ k ] :=  { .F. , .F. , .F. , .F. }
   _HMG_aControlToolTip   [ k ] :=  ''
   _HMG_aControlRangeMin  [ k ] :=  0
   _HMG_aControlRangeMax  [ k ] :=  0
   _HMG_aControlCaption  [ k ] :=  ''
   _HMG_aControlVisible  [ k ] :=  Iif( invisible, .F. , .T. )
   _HMG_aControlHelpId  [ k ] :=  HELPID
   _HMG_aControlFontHandle  [ k ] :=   0
   _HMG_aControlBrushHandle [ k ] := C_SetEmfFile ( ControlHandle , FileName , w , h , _HMG_aControlValue [ k ] , _HMG_aControlSpacing [ k ] )
   _HMG_aControlEnabled  [ k ] :=  .T.
   _HMG_aControlMiscData1 [ k ] := 0
   _HMG_aControlMiscData2 [ k ] := ''

   RETURN NIL

///////////////////////////////////////////////////////////////////////////////
// LOW LEVEL C PRINT ROUTINES
///////////////////////////////////////////////////////////////////////////////
#pragma BEGINDUMP

#ifndef CINTERFACE
  #define CINTERFACE
#endif

#define NO_LEAN_AND_MEAN

#include <mgdefs.h>
#include "hbapiitm.h"

#include <olectl.h>

#ifndef WC_STATIC
  #define WC_STATIC   TEXT( "Static" )
#endif

static DWORD charset = DEFAULT_CHARSET;

#ifdef UNICODE
  LPWSTR AnsiToWide( LPCSTR );
  LPSTR  WideToAnsi( LPWSTR );
#endif
HINSTANCE GetInstance( void );

#ifdef __cplusplus
  extern "C" {
#endif
extern HBITMAP HMG_LoadImage( char * FileName );
#ifdef __cplusplus
  }
#endif


HB_FUNC( _HMG_SETCHARSET )
{
   charset = hmg_par_DWORD( 1 );
}

HB_FUNC( _HMG_PRINTER_ABORTDOC )
{
   HDC hdcPrint = hmg_par_raw_HDC( 1 );

   AbortDoc( hdcPrint );
}

HB_FUNC( _HMG_PRINTER_STARTDOC )
{

   DOCINFO docInfo;

#ifndef UNICODE
   LPTSTR lpText = ( LPTSTR ) hb_parc( 2 );
#else
   LPWSTR lpText = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif
   HDC hdcPrint = hmg_par_raw_HDC( 1 );

   if( hdcPrint != 0 )
   {
      ZeroMemory( &docInfo, sizeof( docInfo ) );
      docInfo.cbSize      = sizeof( docInfo );
      docInfo.lpszDocName = lpText;

      hb_retni( StartDoc( hdcPrint, &docInfo ) );
   }
}

HB_FUNC( _HMG_PRINTER_STARTPAGE )
{

   HDC hdcPrint = hmg_par_raw_HDC( 1 );

   if( hdcPrint != 0 )
      StartPage( hdcPrint );

}

HB_FUNC( _HMG_PRINTER_C_PRINT )
{

   // 1:  Hdc
   // 2:  y
   // 3:  x
   // 4:  FontName
   // 5:  FontSize
   // 6:  R Color
   // 7:  G Color
   // 8:  B Color
   // 9:  Text
   // 10: Bold
   // 11: Italic
   // 12: Underline
   // 13: StrikeOut
   // 14: Color Flag
   // 15: FontName Flag
   // 16: FontSize Flag
   // 17: Angle Flag
   // 18: Angle

   HGDIOBJ hgdiobj;

   TCHAR FontName[ 32 ];
   int  FontSize;

#ifdef UNICODE
   LPWSTR pFontName, pText;
#endif

   DWORD fdwItalic;
   DWORD fdwUnderline;
   DWORD fdwStrikeOut;

   int fnWeight;
   int r;
   int g;
   int b;

   int x = hb_parni( 3 );
   int y = hb_parni( 2 );

   HFONT hfont;

   HDC hdcPrint = hmg_par_raw_HDC( 1 );

   int FontHeight;
   int FontAngle;

   if( hdcPrint != 0 )
   {

      // Bold

      if( hb_parl( 10 ) )
         fnWeight = FW_BOLD;
      else
         fnWeight = FW_NORMAL;

      // Italic

      if( hb_parl( 11 ) )
         fdwItalic = TRUE;
      else
         fdwItalic = FALSE;

      // UnderLine

      if( hb_parl( 12 ) )
         fdwUnderline = TRUE;
      else
         fdwUnderline = FALSE;

      // StrikeOut

      if( hb_parl( 13 ) )
         fdwStrikeOut = TRUE;
      else
         fdwStrikeOut = FALSE;

      // Color

      if( hb_parl( 14 ) )
      {
         r = hb_parni( 6 );
         g = hb_parni( 7 );
         b = hb_parni( 8 );
      }
      else
      {
         r = 0;
         g = 0;
         b = 0;
      }

      // Fontname

      if( hb_parl( 15 ) )
      {
#ifndef UNICODE
         lstrcpy( FontName, hb_parc( 4 ) );
#else
         pFontName = AnsiToWide( hb_parc( 4 ) );
         lstrcpy( FontName, pFontName );
         hb_xfree( pFontName );
#endif
      }
      else
         lstrcpy( FontName, TEXT( "Arial" ) );

      // FontSize

      if( hb_parl( 16 ) )
         FontSize = hb_parni( 5 );
      else
         FontSize = 10;

      // Angle

      if( hb_parl( 17 ) )
         FontAngle = hb_parni( 18 );
      else
         FontAngle = 0;

      FontHeight = -MulDiv( FontSize, GetDeviceCaps( hdcPrint, LOGPIXELSY ), 72 );

      hfont = CreateFont
              (
         FontHeight,
         0,
         FontAngle,
         FontAngle,
         fnWeight,
         fdwItalic,
         fdwUnderline,
         fdwStrikeOut,
         charset,
         OUT_TT_PRECIS,
         CLIP_DEFAULT_PRECIS,
         DEFAULT_QUALITY,
         FF_DONTCARE,
         FontName
              );

      hgdiobj = SelectObject( hdcPrint, hfont );

      SetTextColor( hdcPrint, RGB( r, g, b ) );
      SetBkMode( hdcPrint, TRANSPARENT );

#ifndef UNICODE
      TextOut( hdcPrint,
               ( x * GetDeviceCaps( hdcPrint, LOGPIXELSX ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETX ),
               ( y * GetDeviceCaps( hdcPrint, LOGPIXELSY ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETY ),
               hb_parc( 9 ),
               strlen( hb_parc( 9 ) ) );
#else
      pText = AnsiToWide( hb_parc( 9 ) );
      TextOut( hdcPrint,
               ( x * GetDeviceCaps( hdcPrint, LOGPIXELSX ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETX ),
               ( y * GetDeviceCaps( hdcPrint, LOGPIXELSY ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETY ),
               pText,
               lstrlen( pText ) );
      hb_xfree( pText );
#endif

      SelectObject( hdcPrint, hgdiobj );

      DeleteObject( hfont );

   }

}

HB_FUNC( _HMG_PRINTER_C_MULTILINE_PRINT )
{

   // 1:  Hdc
   // 2:  y
   // 3:  x
   // 4:  FontName
   // 5:  FontSize
   // 6:  R Color
   // 7:  G Color
   // 8:  B Color
   // 9:  Text
   // 10: Bold
   // 11: Italic
   // 12: Underline
   // 13: StrikeOut
   // 14: Color Flag
   // 15: FontName Flag
   // 16: FontSize Flag
   // 17: ToRow
   // 18: ToCol
   // 19: Alignment

   UINT uFormat = 0;

   HGDIOBJ hgdiobj;

   TCHAR FontName[ 32 ];
   int  FontSize;

#ifdef UNICODE
   LPWSTR pFontName, pText;
#endif

   DWORD fdwItalic;
   DWORD fdwUnderline;
   DWORD fdwStrikeOut;

   RECT rect;

   int fnWeight;
   int r;
   int g;
   int b;

   int x   = hb_parni( 3 );
   int y   = hb_parni( 2 );
   int toy = hb_parni( 17 );
   int tox = hb_parni( 18 );

   HFONT hfont;

   HDC hdcPrint = hmg_par_raw_HDC( 1 );

   int FontHeight;

   if( hdcPrint != 0 )
   {

      // Bold

      if( hb_parl( 10 ) )
         fnWeight = FW_BOLD;
      else
         fnWeight = FW_NORMAL;

      // Italic

      if( hb_parl( 11 ) )
         fdwItalic = TRUE;
      else
         fdwItalic = FALSE;

      // UnderLine

      if( hb_parl( 12 ) )
         fdwUnderline = TRUE;
      else
         fdwUnderline = FALSE;

      // StrikeOut

      if( hb_parl( 13 ) )
         fdwStrikeOut = TRUE;
      else
         fdwStrikeOut = FALSE;

      // Color

      if( hb_parl( 14 ) )
      {
         r = hb_parni( 6 );
         g = hb_parni( 7 );
         b = hb_parni( 8 );
      }
      else
      {
         r = 0;
         g = 0;
         b = 0;
      }

      // Fontname

      if( hb_parl( 15 ) )
      {
#ifndef UNICODE
         lstrcpy( FontName, hb_parc( 4 ) );
#else
         pFontName = AnsiToWide( hb_parc( 4 ) );
         lstrcpy( FontName, pFontName );
         hb_xfree( pFontName );
#endif
      }
      else
         lstrcpy( FontName, TEXT( "Arial" ) );

      // FontSize

      if( hb_parl( 16 ) )
         FontSize = hb_parni( 5 );
      else
         FontSize = 10;

      FontHeight = -MulDiv( FontSize, GetDeviceCaps( hdcPrint, LOGPIXELSY ), 72 );

      hfont = CreateFont
              (
         FontHeight,
         0,
         0,
         0,
         fnWeight,
         fdwItalic,
         fdwUnderline,
         fdwStrikeOut,
         charset,
         OUT_TT_PRECIS,
         CLIP_DEFAULT_PRECIS,
         DEFAULT_QUALITY,
         FF_DONTCARE,
         FontName
              );

      if( hb_parni( 19 ) == 0 )
         uFormat = DT_END_ELLIPSIS | DT_NOPREFIX | DT_WORDBREAK | DT_LEFT;
      else if( hb_parni( 19 ) == 2 )
         uFormat = DT_END_ELLIPSIS | DT_NOPREFIX | DT_WORDBREAK | DT_RIGHT;
      else if( hb_parni( 19 ) == 6 )
         uFormat = DT_END_ELLIPSIS | DT_NOPREFIX | DT_WORDBREAK | DT_CENTER;

      hgdiobj = SelectObject( hdcPrint, hfont );

      SetTextColor( hdcPrint, RGB( r, g, b ) );
      SetBkMode( hdcPrint, TRANSPARENT );

      rect.left   = ( x * GetDeviceCaps( hdcPrint, LOGPIXELSX ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETX );
      rect.top    = ( y * GetDeviceCaps( hdcPrint, LOGPIXELSY ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETY );
      rect.right  = ( tox * GetDeviceCaps( hdcPrint, LOGPIXELSX ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETX );
      rect.bottom = ( toy * GetDeviceCaps( hdcPrint, LOGPIXELSY ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETY );

#ifndef UNICODE
      DrawText( hdcPrint,
                hb_parc( 9 ),
                strlen( hb_parc( 9 ) ),
                &rect,
                uFormat
                );
#else
      pText = AnsiToWide( hb_parc( 9 ) );
      DrawText( hdcPrint,
                pText,
                lstrlen( pText ),
                &rect,
                uFormat
                );
      hb_xfree( pText );
#endif

      SelectObject( hdcPrint, hgdiobj );

      DeleteObject( hfont );

   }

}

HB_FUNC( _HMG_PRINTER_ENDPAGE )
{
   HDC hdcPrint = hmg_par_raw_HDC( 1 );

   if( hdcPrint != 0 )
      EndPage( hdcPrint );
}

HB_FUNC( _HMG_PRINTER_ENDDOC )
{
   HDC hdcPrint = hmg_par_raw_HDC( 1 );

   if( hdcPrint != 0 )
      EndDoc( hdcPrint );
}

HB_FUNC( _HMG_PRINTER_DELETEDC )
{
   HDC hdcPrint = hmg_par_raw_HDC( 1 );

   DeleteDC( hdcPrint );
}

HB_FUNC( _HMG_PRINTER_PRINTDIALOG )
{

#ifdef UNICODE
   LPSTR pStr;
#endif
   PRINTDLG pd;

   LPDEVMODE pDevMode;

   pd.lStructSize         = sizeof( PRINTDLG );
   pd.hDevMode            = ( HANDLE ) NULL;
   pd.hDevNames           = ( HANDLE ) NULL;
   pd.Flags               = PD_RETURNDC | PD_PRINTSETUP;
   pd.hwndOwner           = NULL;
   pd.hDC                 = NULL;
   pd.nFromPage           = 1;
   pd.nToPage             = 0xFFFF;
   pd.nMinPage            = 1;
   pd.nMaxPage            = 0xFFFF;
   pd.nCopies             = 1;
   pd.hInstance           = ( HINSTANCE ) NULL;
   pd.lCustData           = 0L;
   pd.lpfnPrintHook       = ( LPPRINTHOOKPROC ) NULL;
   pd.lpfnSetupHook       = ( LPSETUPHOOKPROC ) NULL;
   pd.lpPrintTemplateName = NULL;
   pd.lpSetupTemplateName = NULL;
   pd.hPrintTemplate      = ( HANDLE ) NULL;
   pd.hSetupTemplate      = ( HANDLE ) NULL;

   if( PrintDlg( &pd ) )
   {
      pDevMode = ( LPDEVMODE ) GlobalLock( pd.hDevMode );

      hb_reta( 4 );
      hmg_storvnl_HANDLE( pd.hDC, -1, 1 );
#ifndef UNICODE
      HB_STORC( ( const char * ) pDevMode->dmDeviceName, -1, 2 );
#else
      pStr = WideToAnsi( pDevMode->dmDeviceName );
      HB_STORC( pStr, -1, 2 );
      hb_xfree( pStr );
#endif
      HB_STORNI( pDevMode->dmCopies > 1 ? pDevMode->dmCopies : pd.nCopies, -1, 3 );
      HB_STORNI( pDevMode->dmCollate, -1, 4 );

      GlobalUnlock( pd.hDevMode );
   }
   else
   {
      hb_reta( 4 );
      HB_STORVNL( 0, -1, 1 );
#ifndef UNICODE
      HB_STORC( "", -1, 2 );
#else
      pStr = WideToAnsi( TEXT( "" ) );
      HB_STORC( pStr, -1, 2 );
      hb_xfree( pStr );
#endif
      HB_STORNI( 0, -1, 3 );
      HB_STORNI( 0, -1, 4 );
   }

}

HB_FUNC( APRINTERS )
{

   OSVERSIONINFO osvi;

   HGLOBAL cBuffer;
   HGLOBAL pBuffer;

   DWORD dwSize     = 0;
   DWORD dwPrinters = 0;
   DWORD i;

#ifdef UNICODE
   LPSTR pStr;
#endif

   PRINTER_INFO_4 * pInfo4 = NULL;
   PRINTER_INFO_5 * pInfo  = NULL;

   osvi.dwOSVersionInfoSize = sizeof( OSVERSIONINFO );

   GetVersionEx( &osvi );

   if( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT )
      EnumPrinters( PRINTER_ENUM_LOCAL | PRINTER_ENUM_CONNECTIONS, NULL, 4, NULL, 0, &dwSize, &dwPrinters );
   else
      EnumPrinters( PRINTER_ENUM_LOCAL, NULL, 5, NULL, 0, &dwSize, &dwPrinters );

   pBuffer = ( char * ) GlobalAlloc( GPTR, dwSize );

   if( pBuffer == NULL )
   {
      hb_reta( 0 );
      GlobalFree( pBuffer );
      return;
   }

   if( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT )
      EnumPrinters( PRINTER_ENUM_LOCAL | PRINTER_ENUM_CONNECTIONS, NULL, 4, ( LPBYTE ) pBuffer, dwSize, &dwSize, &dwPrinters );
   else
      EnumPrinters( PRINTER_ENUM_LOCAL, NULL, 5, ( LPBYTE ) pBuffer, dwSize, &dwSize, &dwPrinters );

   if( dwPrinters == 0 )
   {
      hb_reta( 0 );
      GlobalFree( pBuffer );
      return;
   }

   if( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT )
      pInfo4 = ( PRINTER_INFO_4 * ) pBuffer;
   else
      pInfo = ( PRINTER_INFO_5 * ) pBuffer;

   hb_reta( dwPrinters );

   if( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT )
      for( i = 0; i < dwPrinters; i++, pInfo4++ )
      {
         cBuffer = GlobalAlloc( GPTR, 256 );
         lstrcat( cBuffer, pInfo4->pPrinterName );
#ifndef UNICODE
         HB_STORC( ( const char * ) cBuffer, -1, i + 1 );
#else
         pStr = WideToAnsi( cBuffer );
         HB_STORC( pStr, -1, i + 1 );
         hb_xfree( pStr );
#endif
         GlobalFree( cBuffer );
      }
   else
      for( i = 0; i < dwPrinters; i++, pInfo++ )
      {
         cBuffer = GlobalAlloc( GPTR, 256 );
         lstrcat( cBuffer, pInfo->pPrinterName );
#ifndef UNICODE
         HB_STORC( ( const char * ) cBuffer, -1, i + 1 );
#else
         pStr = WideToAnsi( cBuffer );
         HB_STORC( pStr, -1, i + 1 );
         hb_xfree( pStr );
#endif
         GlobalFree( cBuffer );
      }

   GlobalFree( pBuffer );

}

HB_FUNC( _HMG_PRINTER_C_RECTANGLE )
{

   // 1: hDC
   // 2: y
   // 3: x
   // 4: toy
   // 5: tox
   // 6: width
   // 7: R Color
   // 8: G Color
   // 9: B Color
   // 10: lWidth
   // 11: lColor
   // 12: lFilled

   int r;
   int g;
   int b;

   int x = hb_parni( 3 );
   int y = hb_parni( 2 );

   int tox = hb_parni( 5 );
   int toy = hb_parni( 4 );

   int width;

   HDC     hdcPrint = hmg_par_raw_HDC( 1 );
   HGDIOBJ hgdiobj;
   HBRUSH  hbrush = NULL;
   HPEN    hpen   = NULL;
   RECT    rect;

   if( hdcPrint != 0 )
   {

      // Width

      if( hb_parl( 10 ) )
         width = hb_parni( 6 );
      else
         width = 1 * 10000 / 254;

      // Color

      if( hb_parl( 11 ) )
      {
         r = hb_parni( 7 );
         g = hb_parni( 8 );
         b = hb_parni( 9 );
      }
      else
      {
         r = 0;
         g = 0;
         b = 0;
      }

      // Filled

      if( hb_parl( 12 ) )
      {
         hbrush  = CreateSolidBrush( ( COLORREF ) RGB( r, g, b ) );
         hgdiobj = SelectObject( hdcPrint, hbrush );
      }
      else
      {
         hpen    = CreatePen( PS_SOLID, ( width * GetDeviceCaps( hdcPrint, LOGPIXELSX ) / 1000 ), ( COLORREF ) RGB( r, g, b ) );
         hgdiobj = SelectObject( hdcPrint, hpen );
      }

      // Border  ( contributed by Alen Uzelac 08.06.2011 )

      rect.left   = ( x * GetDeviceCaps( hdcPrint, LOGPIXELSX ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETX );
      rect.top    = ( y * GetDeviceCaps( hdcPrint, LOGPIXELSY ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETY );
      rect.right  = ( tox * GetDeviceCaps( hdcPrint, LOGPIXELSX ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETX );
      rect.bottom = ( toy * GetDeviceCaps( hdcPrint, LOGPIXELSY ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETY );

      if( hb_parl( 12 ) && hb_parl( 13 ) )
         FillRect( hdcPrint, &rect, ( HBRUSH ) hbrush );
      else
         Rectangle( hdcPrint, rect.left, rect.top, rect.right, rect.bottom );

      SelectObject( hdcPrint, ( HGDIOBJ ) hgdiobj );

      if( hb_parl( 12 ) )
         DeleteObject( hbrush );
      else
         DeleteObject( hpen );

   }

}

HB_FUNC( _HMG_PRINTER_C_ROUNDRECTANGLE )
{

   // 1: hDC
   // 2: y
   // 3: x
   // 4: toy
   // 5: tox
   // 6: width
   // 7: R Color
   // 8: G Color
   // 9: B Color
   // 10: lWidth
   // 11: lColor
   // 12: lFilled

   int r;
   int g;
   int b;

   int x = hb_parni( 3 );
   int y = hb_parni( 2 );

   int tox = hb_parni( 5 );
   int toy = hb_parni( 4 );

   int width;

   int w, h, p;

   HDC     hdcPrint = hmg_par_raw_HDC( 1 );
   HGDIOBJ hgdiobj;
   HBRUSH  hbrush = NULL;
   HPEN    hpen   = NULL;

   if( hdcPrint != 0 )
   {

      // Width

      if( hb_parl( 10 ) )
         width = hb_parni( 6 );
      else
         width = 1 * 10000 / 254;

      // Color

      if( hb_parl( 11 ) )
      {
         r = hb_parni( 7 );
         g = hb_parni( 8 );
         b = hb_parni( 9 );
      }
      else
      {
         r = 0;
         g = 0;
         b = 0;
      }

      // Filled

      if( hb_parl( 12 ) )
      {
         hbrush  = CreateSolidBrush( ( COLORREF ) RGB( r, g, b ) );
         hgdiobj = SelectObject( ( HDC ) hdcPrint, hbrush );
      }
      else
      {
         hpen    = CreatePen( PS_SOLID, ( width * GetDeviceCaps( hdcPrint, LOGPIXELSX ) / 1000 ), ( COLORREF ) RGB( r, g, b ) );
         hgdiobj = SelectObject( ( HDC ) hdcPrint, hpen );
      }

      w = ( tox * GetDeviceCaps( hdcPrint, LOGPIXELSX ) / 1000 ) - ( x * GetDeviceCaps( hdcPrint, LOGPIXELSX ) / 1000 );
      h = ( toy * GetDeviceCaps( hdcPrint, LOGPIXELSY ) / 1000 ) - ( y * GetDeviceCaps( hdcPrint, LOGPIXELSY ) / 1000 );
      p = ( w + h ) / 2;
      p = p / 10;

      RoundRect( ( HDC ) hdcPrint,
                 ( x * GetDeviceCaps( hdcPrint, LOGPIXELSX ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETX ),
                 ( y * GetDeviceCaps( hdcPrint, LOGPIXELSY ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETY ),
                 ( tox * GetDeviceCaps( hdcPrint, LOGPIXELSX ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETX ),
                 ( toy * GetDeviceCaps( hdcPrint, LOGPIXELSY ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETY ),
                 p,
                 p
                 );

      SelectObject( hdcPrint, ( HGDIOBJ ) hgdiobj );

      if( hb_parl( 12 ) )
         DeleteObject( hbrush );
      else
         DeleteObject( hpen );

   }

}

HB_FUNC( _HMG_PRINTER_C_LINE )
{

   // 1: hDC
   // 2: y
   // 3: x
   // 4: toy
   // 5: tox
   // 6: width
   // 7: R Color
   // 8: G Color
   // 9: B Color
   // 10: lWidth
   // 11: lColor
   // 12: nStyle

   int r;
   int g;
   int b;

   int x = hb_parni( 3 );
   int y = hb_parni( 2 );

   int tox = hb_parni( 5 );
   int toy = hb_parni( 4 );

   int width;
   int Style;

   HDC     hdcPrint = hmg_par_raw_HDC( 1 );
   HGDIOBJ hgdiobj;
   HPEN    hpen;

   if( hdcPrint != 0 )
   {

      // Width

      if( hb_parl( 10 ) )
         width = hb_parni( 6 );
      else
         width = 1 * 10000 / 254;

      // Color

      if( hb_parl( 11 ) )
      {
         r = hb_parni( 7 );
         g = hb_parni( 8 );
         b = hb_parni( 9 );
      }
      else
      {
         r = 0;
         g = 0;
         b = 0;
      }

      switch( hb_parni( 12 ) )
      {
         case 1:
            Style = PS_DOT;
            break;
         case 2:
            Style = PS_DASH;
            break;
         case 3:
            Style = PS_DASHDOT;
            break;
         case 4:
            Style = PS_DASHDOTDOT;
            break;
         default:
            Style = PS_SOLID;
      }

      hpen = CreatePen( Style, ( width * GetDeviceCaps( hdcPrint, LOGPIXELSX ) / 1000 ), ( COLORREF ) RGB( r, g, b ) );

      hgdiobj = SelectObject( hdcPrint, hpen );

      MoveToEx( hdcPrint,
                ( x * GetDeviceCaps( hdcPrint, LOGPIXELSX ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETX ),
                ( y * GetDeviceCaps( hdcPrint, LOGPIXELSY ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETY ),
                NULL
                );

      LineTo( hdcPrint,
              ( tox * GetDeviceCaps( hdcPrint, LOGPIXELSX ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETX ),
              ( toy * GetDeviceCaps( hdcPrint, LOGPIXELSY ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETY )
              );

      SelectObject( hdcPrint, ( HGDIOBJ ) hgdiobj );

      DeleteObject( hpen );

   }

}

HB_FUNC( _HMG_PRINTER_SETPRINTERPROPERTIES )
{
   HANDLE hPrinter = NULL;
   DWORD  dwNeeded = 0;
   PRINTER_INFO_2 * pi2;
   DEVMODE *        pDevMode = NULL;
   BOOL bFlag;
   LONG lFlag;

   HDC hdcPrint;

#ifdef UNICODE
   LPWSTR pPrinterName, pDeviceName, pwszDevice;
   LPSTR  pStr;
#endif

   int fields = 0;

#ifndef UNICODE
   bFlag = OpenPrinter( ( LPSTR ) hb_parc( 1 ), &hPrinter, NULL );
#else
   pPrinterName = AnsiToWide( hb_parc( 1 ) );
   bFlag        = OpenPrinter( pPrinterName, &hPrinter, NULL );
   hb_xfree( pPrinterName );
#endif

   if( ! bFlag || ( hPrinter == NULL ) )
   {
#ifdef _ERRORMSG_
      MessageBox( 0, TEXT( "Printer Configuration Failed! (001)" ), TEXT( "Error!" ), MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL );
#endif
      hb_reta( 4 );
      HB_STORVNL( 0, -1, 1 );
      HB_STORC( "", -1, 2 );
      HB_STORNI( 0, -1, 3 );
      HB_STORNI( 0, -1, 4 );

      return;
   }

   SetLastError( 0 );

   bFlag = GetPrinter( hPrinter, 2, 0, 0, &dwNeeded );

   if( ( ! bFlag ) && ( ( GetLastError() != ERROR_INSUFFICIENT_BUFFER ) || ( dwNeeded == 0 ) ) )
   {
      ClosePrinter( hPrinter );
#ifdef _ERRORMSG_
      MessageBox( 0, TEXT( "Printer Configuration Failed! (002)" ), TEXT( "Error!" ), MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL );
#endif
      hb_reta( 4 );
      HB_STORVNL( 0, -1, 1 );
      HB_STORC( "", -1, 2 );
      HB_STORNI( 0, -1, 3 );
      HB_STORNI( 0, -1, 4 );

      return;
   }

   pi2 = ( PRINTER_INFO_2 * ) GlobalAlloc( GPTR, dwNeeded );

   if( pi2 == NULL )
   {
      ClosePrinter( hPrinter );
#ifdef _ERRORMSG_
      MessageBox( 0, TEXT( "Printer Configuration Failed! (003)" ), TEXT( "Error!" ), MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL );
#endif
      hb_reta( 4 );
      HB_STORVNL( 0, -1, 1 );
      HB_STORC( "", -1, 2 );
      HB_STORNI( 0, -1, 3 );
      HB_STORNI( 0, -1, 4 );

      return;
   }

   bFlag = GetPrinter( hPrinter, 2, ( LPBYTE ) pi2, dwNeeded, &dwNeeded );

   if( ! bFlag )
   {
      GlobalFree( pi2 );
      ClosePrinter( hPrinter );
#ifdef _ERRORMSG_
      MessageBox( 0, TEXT( "Printer Configuration Failed! (004)" ), TEXT( "Error!" ), MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL );
#endif
      hb_reta( 4 );
      HB_STORVNL( 0, -1, 1 );
      HB_STORC( "", -1, 2 );
      HB_STORNI( 0, -1, 3 );
      HB_STORNI( 0, -1, 4 );

      return;
   }

   if( pi2->pDevMode == NULL )
   {
#ifndef UNICODE
      dwNeeded = DocumentProperties( NULL, hPrinter, ( LPSTR ) hb_parc( 1 ), NULL, NULL, 0 );
#else
      pDeviceName = AnsiToWide( hb_parc( 1 ) );
      dwNeeded    = DocumentProperties( NULL, hPrinter, pDeviceName, NULL, NULL, 0 );
      hb_xfree( pDeviceName );
#endif
      if( dwNeeded > 0 )
         pDevMode = ( DEVMODE * ) GlobalAlloc( GPTR, dwNeeded );
      else
      {
         GlobalFree( pi2 );
         ClosePrinter( hPrinter );
#ifdef _ERRORMSG_
         MessageBox( 0, TEXT( "Printer Configuration Failed! (005)" ), TEXT( "Error!" ), MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL );
#endif
         hb_reta( 4 );
         HB_STORVNL( 0, -1, 1 );
         HB_STORC( "", -1, 2 );
         HB_STORNI( 0, -1, 3 );
         HB_STORNI( 0, -1, 4 );

         return;
      }

      if( pDevMode == NULL )
      {
         GlobalFree( pi2 );
         ClosePrinter( hPrinter );
#ifdef _ERRORMSG_
         MessageBox( 0, TEXT( "Printer Configuration Failed! (006)" ), TEXT( "Error! (006)" ), MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL );
#endif
         hb_reta( 4 );
         HB_STORVNL( 0, -1, 1 );
         HB_STORC( "", -1, 2 );
         HB_STORNI( 0, -1, 3 );
         HB_STORNI( 0, -1, 4 );

         return;
      }

#ifndef UNICODE
      lFlag = DocumentProperties( NULL, hPrinter, ( LPSTR ) hb_parc( 1 ), pDevMode, NULL, DM_OUT_BUFFER );
#else
      pDeviceName = AnsiToWide( hb_parc( 1 ) );
      lFlag       = DocumentProperties( NULL, hPrinter, pDeviceName, pDevMode, NULL, DM_OUT_BUFFER );
      hb_xfree( pDeviceName );
#endif
      if( lFlag != IDOK || pDevMode == NULL )
      {
         GlobalFree( pDevMode );
         GlobalFree( pi2 );
         ClosePrinter( hPrinter );
#ifdef _ERRORMSG_
         MessageBox( 0, TEXT( "Printer Configuration Failed! (007)" ), TEXT( "Error!" ), MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL );
#endif
         hb_reta( 4 );
         HB_STORVNL( 0, -1, 1 );
         HB_STORC( "", -1, 2 );
         HB_STORNI( 0, -1, 3 );
         HB_STORNI( 0, -1, 4 );

         return;
      }

      pi2->pDevMode = pDevMode;
   }

   ///////////////////////////////////////////////////////////////////////
   // Specify Fields
   //////////////////////////////////////////////////////////////////////
   // Orientation
   if( hb_parni( 2 ) != -999 )
      fields = fields | DM_ORIENTATION;

   // PaperSize
   if( hb_parni( 3 ) != -999 )
      fields = fields | DM_PAPERSIZE;

   // PaperLength
   if( hb_parni( 4 ) != -999 )
      fields = fields | DM_PAPERLENGTH;

   // PaperWidth
   if( hb_parni( 5 ) != -999 )
      fields = fields | DM_PAPERWIDTH;

   // Copies
   if( hb_parni( 6 ) != -999 )
      fields = fields | DM_COPIES;

   // Default Source
   if( hb_parni( 7 ) != -999 )
      fields = fields | DM_DEFAULTSOURCE;

   // Print Quality
   if( hb_parni( 8 ) != -999 )
      fields = fields | DM_PRINTQUALITY;

   // Print Color
   if( hb_parni( 9 ) != -999 )
      fields = fields | DM_COLOR;

   // Print Duplex Mode
   if( hb_parni( 10 ) != -999 )
      fields = fields | DM_DUPLEX;

   // Print Collate
   if( hb_parni( 11 ) != -999 )
      fields = fields | DM_COLLATE;

   pi2->pDevMode->dmFields = fields;

   ///////////////////////////////////////////////////////////////////////
   // Load Fields
   //////////////////////////////////////////////////////////////////////
   // Orientation
   if( hb_parni( 2 ) != -999 )
   {
      if( ! ( pi2->pDevMode->dmFields & DM_ORIENTATION ) )
      {
#ifdef _ERRORMSG_
         MessageBox( 0, TEXT( "Printer Configuration Failed: ORIENTATION Property Not Supported By Selected Printer" ), TEXT( "Error!" ),
                     MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL );
#endif
         hb_reta( 4 );
         HB_STORVNL( 0, -1, 1 );
         HB_STORC( "", -1, 2 );
         HB_STORNI( 0, -1, 3 );
         HB_STORNI( 0, -1, 4 );

         return;
      }

      pi2->pDevMode->dmOrientation = hmg_par_short( 2 );
   }

   // PaperSize
   if( hb_parni( 3 ) != -999 )
   {
      if( ! ( pi2->pDevMode->dmFields & DM_PAPERSIZE ) )
      {
#ifdef _ERRORMSG_
         MessageBox( 0, TEXT( "Printer Configuration Failed: PAPERSIZE Property Not Supported By Selected Printer" ), TEXT( "Error!" ),
                     MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL );
#endif
         hb_reta( 4 );
         HB_STORVNL( 0, -1, 1 );
         HB_STORC( "", -1, 2 );
         HB_STORNI( 0, -1, 3 );
         HB_STORNI( 0, -1, 4 );

         return;
      }

      pi2->pDevMode->dmPaperSize = hmg_par_short( 3 );
   }

   // PaperLength
   if( hb_parni( 4 ) != -999 )
   {
      if( ! ( pi2->pDevMode->dmFields & DM_PAPERLENGTH ) )
      {
#ifdef _ERRORMSG_
         MessageBox( 0, TEXT( "Printer Configuration Failed: PAPERLENGTH Property Not Supported By Selected Printer" ), TEXT( "Error!" ),
                     MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL );
#endif
         hb_reta( 4 );
         HB_STORVNL( 0, -1, 1 );
         HB_STORC( "", -1, 2 );
         HB_STORNI( 0, -1, 3 );
         HB_STORNI( 0, -1, 4 );

         return;
      }

      pi2->pDevMode->dmPaperLength = ( short ) ( hb_parni( 4 ) * 10 );
   }

   // PaperWidth
   if( hb_parni( 5 ) != -999 )
   {
      if( ! ( pi2->pDevMode->dmFields & DM_PAPERWIDTH ) )
      {
#ifdef _ERRORMSG_
         MessageBox( 0, TEXT( "Printer Configuration Failed: PAPERWIDTH Property Not Supported By Selected Printer" ), TEXT( "Error!" ),
                     MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL );
#endif
         hb_reta( 4 );
         HB_STORVNL( 0, -1, 1 );
         HB_STORC( "", -1, 2 );
         HB_STORNI( 0, -1, 3 );
         HB_STORNI( 0, -1, 4 );

         return;
      }

      pi2->pDevMode->dmPaperWidth = ( short ) ( hb_parni( 5 ) * 10 );
   }

   // Copies
   if( hb_parni( 6 ) != -999 )
   {
      if( ! ( pi2->pDevMode->dmFields & DM_COPIES ) )
      {
#ifdef _ERRORMSG_
         MessageBox( 0, TEXT( "Printer Configuration Failed: COPIES Property Not Supported By Selected Printer" ), TEXT( "Error!" ), MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL );
#endif
         hb_reta( 4 );
         HB_STORVNL( 0, -1, 1 );
         HB_STORC( "", -1, 2 );
         HB_STORNI( 0, -1, 3 );
         HB_STORNI( 0, -1, 4 );

         return;
      }

      pi2->pDevMode->dmCopies = hmg_par_short( 6 );
   }

   // Default Source
   if( hb_parni( 7 ) != -999 )
   {
      if( ! ( pi2->pDevMode->dmFields & DM_DEFAULTSOURCE ) )
      {
#ifdef _ERRORMSG_
         MessageBox( 0, TEXT( "Printer Configuration Failed: DEFAULTSOURCE Property Not Supported By Selected Printer" ), TEXT( "Error!" ),
                     MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL );
#endif
         hb_reta( 4 );
         HB_STORVNL( 0, -1, 1 );
         HB_STORC( "", -1, 2 );
         HB_STORNI( 0, -1, 3 );
         HB_STORNI( 0, -1, 4 );

         return;
      }

      pi2->pDevMode->dmDefaultSource = hmg_par_short( 7 );
   }

   // Print Quality
   if( hb_parni( 8 ) != -999 )
   {
      if( ! ( pi2->pDevMode->dmFields & DM_PRINTQUALITY ) )
      {
#ifdef _ERRORMSG_
         MessageBox( 0, TEXT( "Printer Configuration Failed: QUALITY Property Not Supported By Selected Printer" ), TEXT( "Error!" ),
                     MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL );
#endif
         hb_reta( 4 );
         HB_STORVNL( 0, -1, 1 );
         HB_STORC( "", -1, 2 );
         HB_STORNI( 0, -1, 3 );
         HB_STORNI( 0, -1, 4 );

         return;
      }

      pi2->pDevMode->dmPrintQuality = hmg_par_short( 8 );
   }

   // Print Color
   if( hb_parni( 9 ) != -999 )
   {
      if( ! ( pi2->pDevMode->dmFields & DM_COLOR ) )
      {
#ifdef _ERRORMSG_
         MessageBox( 0, TEXT( "Printer Configuration Failed: COLOR Property Not Supported By Selected Printer" ), TEXT( "Error!" ), MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL );
#endif
         hb_reta( 4 );
         HB_STORVNL( 0, -1, 1 );
         HB_STORC( "", -1, 2 );
         HB_STORNI( 0, -1, 3 );
         HB_STORNI( 0, -1, 4 );

         return;
      }

      pi2->pDevMode->dmColor = hmg_par_short( 9 );
   }

   // Print Duplex
   if( hb_parni( 10 ) != -999 )
   {
      if( ! ( pi2->pDevMode->dmFields & DM_DUPLEX ) )
      {
#ifdef _ERRORMSG_
         MessageBox( 0, TEXT( "Printer Configuration Failed: DUPLEX Property Not Supported By Selected Printer" ), TEXT( "Error!" ), MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL );
#endif
         hb_reta( 4 );
         HB_STORVNL( 0, -1, 1 );
         HB_STORC( "", -1, 2 );
         HB_STORNI( 0, -1, 3 );
         HB_STORNI( 0, -1, 4 );

         return;
      }

      pi2->pDevMode->dmDuplex = hmg_par_short( 10 );
   }

   // Print Collate
   if( hb_parni( 11 ) != -999 )
   {
      if( ! ( pi2->pDevMode->dmFields & DM_COLLATE ) )
      {
#ifdef _ERRORMSG_
         MessageBox( 0, TEXT( "Printer Configuration Failed: COLLATE Property Not Supported By Selected Printer" ), TEXT( "Error!" ),
                     MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL );
#endif
         hb_reta( 4 );
         HB_STORVNL( 0, -1, 1 );
         HB_STORC( "", -1, 2 );
         HB_STORNI( 0, -1, 3 );
         HB_STORNI( 0, -1, 4 );

         return;
      }

      pi2->pDevMode->dmCollate = hmg_par_short( 11 );
   }

   //////////////////////////////////////////////////////////////////////

   pi2->pSecurityDescriptor = NULL;

#ifndef UNICODE
   lFlag = DocumentProperties( NULL, hPrinter, ( LPSTR ) hb_parc( 1 ), pi2->pDevMode, pi2->pDevMode, DM_IN_BUFFER | DM_OUT_BUFFER );
#else
   pDeviceName = AnsiToWide( hb_parc( 1 ) );
   lFlag       = DocumentProperties( NULL, hPrinter, pDeviceName, pi2->pDevMode, pi2->pDevMode, DM_IN_BUFFER | DM_OUT_BUFFER );
   hb_xfree( pDeviceName );
#endif
   if( lFlag != IDOK )
   {
      GlobalFree( pi2 );
      ClosePrinter( hPrinter );
      if( pDevMode )
         GlobalFree( pDevMode );
#ifdef _ERRORMSG_
      MessageBox( 0, TEXT( "Printer Configuration Failed! (008)" ), TEXT( "Error!" ), MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL );
#endif
      hb_reta( 4 );
      HB_STORVNL( 0, -1, 1 );
      HB_STORC( "", -1, 2 );
      HB_STORNI( 0, -1, 3 );
      HB_STORNI( 0, -1, 4 );

      return;
   }

#ifdef UNICODE
   pwszDevice = AnsiToWide( ( char * ) hb_parc( 1 ) );
   hdcPrint   = CreateDC( NULL, pwszDevice, NULL, pi2->pDevMode );
#else
   hdcPrint = CreateDC( NULL, hb_parc( 1 ), NULL, pi2->pDevMode );
#endif

   if( hdcPrint != NULL )
   {
      hb_reta( 4 );
      HB_STORVNL( ( LONG_PTR ) hdcPrint, -1, 1 );
#ifndef UNICODE
      HB_STORC( hb_parc( 1 ), -1, 2 );
#else
      pStr = WideToAnsi( pwszDevice );
      HB_STORC( hb_parc( 1 ), -1, 2 );
      hb_xfree( pStr );
#endif
      HB_STORNI( ( INT ) pi2->pDevMode->dmCopies, -1, 3 );
      HB_STORNI( ( INT ) pi2->pDevMode->dmCollate, -1, 4 );
   }
   else
   {
      hb_reta( 4 );
      HB_STORVNL( 0, -1, 1 );
      HB_STORC( "", -1, 2 );
      HB_STORNI( 0, -1, 3 );
      HB_STORNI( 0, -1, 4 );
   }

#ifdef UNICODE
   hb_xfree( pwszDevice );
#endif

   if( pi2 )
      GlobalFree( pi2 );

   if( hPrinter )
      ClosePrinter( hPrinter );

   if( pDevMode )
      GlobalFree( pDevMode );

}

#if ! defined( __XHARBOUR__ ) && ! ( defined( __MINGW32__ ) || defined( __POCC__ ) )

HB_FUNC( GETDEFAULTPRINTER )
{

   OSVERSIONINFO    osvi;
   LPPRINTER_INFO_5 PrinterInfo;
   DWORD Needed, Returned;
   DWORD BufferSize = 254;

   TCHAR DefaultPrinter[ 254 ];

#ifdef UNICODE
   LPSTR pStr;
#endif

   osvi.dwOSVersionInfoSize = sizeof( OSVERSIONINFO );

   GetVersionEx( &osvi );

   if( osvi.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS )
   {

      EnumPrinters( PRINTER_ENUM_DEFAULT, NULL, 5, NULL, 0, &Needed, &Returned );
      PrinterInfo = ( LPPRINTER_INFO_5 ) LocalAlloc( LPTR, Needed );
      EnumPrinters( PRINTER_ENUM_DEFAULT, NULL, 5, ( LPBYTE ) PrinterInfo, Needed, &Needed, &Returned );
      lstrcpy( DefaultPrinter, PrinterInfo->pPrinterName );
      LocalFree( PrinterInfo );

   }
   else if( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT )
   {

      GetProfileString( TEXT( "windows" ), TEXT( "device" ), TEXT( "" ), DefaultPrinter, BufferSize );
      _tcstok( DefaultPrinter, TEXT( "," ) );

   }

#ifndef UNICODE
   hb_retc( DefaultPrinter );
#else
   pStr = WideToAnsi( DefaultPrinter );
   hb_retc( pStr );
   hb_xfree( pStr );
#endif

}

#endif

HB_FUNC( _HMG_PRINTER_STARTPAGE_PREVIEW )
{

#ifndef UNICODE
   LPSTR FileName = ( LPSTR ) hb_parc( 2 );
#else
   LPWSTR FileName = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif
   HDC  tmpDC;
   RECT emfrect;

   SetRect( &emfrect, 0, 0, GetDeviceCaps( hmg_par_raw_HDC( 1 ), HORZSIZE ) * 100, GetDeviceCaps( hmg_par_raw_HDC( 1 ), VERTSIZE ) * 100 );

   tmpDC = CreateEnhMetaFile( hmg_par_raw_HDC( 1 ), FileName, &emfrect, TEXT( "" ) );

   hmg_ret_raw_HDC( tmpDC );

}

HB_FUNC( _HMG_PRINTER_ENDPAGE_PREVIEW )
{
   DeleteEnhMetaFile( CloseEnhMetaFile( hmg_par_raw_HDC( 1 ) ) );
}

HB_FUNC( _HMG_PRINTER_SHOWPAGE )
{

   HENHMETAFILE hemf;

#ifndef UNICODE
   LPSTR FileName = ( LPSTR ) hb_parc( 1 );
#else
   LPWSTR FileName = AnsiToWide( ( char * ) hb_parc( 1 ) );
#endif
   HWND        hWnd       = hmg_par_raw_HWND( 2 );
   HDC         hDCPrinter = hmg_par_raw_HDC( 3 );
   RECT         rct;
   RECT         aux;
   int          zw;
   int          zh;
   int          ClientWidth;
   int          ClientHeight;
   int          xOffset;
   int          yOffset;
   PAINTSTRUCT  ps;
   HDC          hDC = BeginPaint( hWnd, &ps );

   hemf = GetEnhMetaFile( FileName );

   GetClientRect( hWnd, &rct );

   ClientWidth  = rct.right - rct.left;
   ClientHeight = rct.bottom - rct.top;

   zw = hb_parni( 5 ) * GetDeviceCaps( hDCPrinter, HORZSIZE ) / 750;
   zh = hb_parni( 5 ) * GetDeviceCaps( hDCPrinter, VERTSIZE ) / 750;

   xOffset = ( ClientWidth - ( GetDeviceCaps( hDCPrinter, HORZSIZE ) * hb_parni( 4 ) / 10000 ) ) / 2;
   yOffset = ( ClientHeight - ( GetDeviceCaps( hDCPrinter, VERTSIZE ) * hb_parni( 4 ) / 10000 ) ) / 2;

   SetRect( &rct,
            xOffset + hb_parni( 6 ) - zw,
            yOffset + hb_parni( 7 ) - zh,
            xOffset + ( GetDeviceCaps( hDCPrinter, HORZSIZE ) * hb_parni( 4 ) / 10000 ) + hb_parni( 6 ) + zw,
            yOffset + ( GetDeviceCaps( hDCPrinter, VERTSIZE ) * hb_parni( 4 ) / 10000 ) + hb_parni( 7 ) + zh
            );

   FillRect( hDC, &rct, ( HBRUSH ) RGB( 255, 255, 255 ) );

   PlayEnhMetaFile( hDC, hemf, &rct );

   // Remove prints outside printable area

   // Right
   aux.top    = 0;
   aux.left   = rct.right;
   aux.right  = ClientWidth;
   aux.bottom = ClientHeight;
   FillRect( hDC, &aux, ( HBRUSH ) GetStockObject( GRAY_BRUSH ) );

   // Bottom
   aux.top    = rct.bottom;
   aux.left   = 0;
   aux.right  = ClientWidth;
   aux.bottom = ClientHeight;
   FillRect( hDC, &aux, ( HBRUSH ) GetStockObject( GRAY_BRUSH ) );

   // Top
   aux.top    = 0;
   aux.left   = 0;
   aux.right  = ClientWidth;
   aux.bottom = yOffset + hb_parni( 7 ) - zh;
   FillRect( hDC, &aux, ( HBRUSH ) GetStockObject( GRAY_BRUSH ) );

   // Left
   aux.top    = 0;
   aux.left   = 0;
   aux.right  = xOffset + hb_parni( 6 ) - zw;
   aux.bottom = ClientHeight;
   FillRect( hDC, &aux, ( HBRUSH ) GetStockObject( GRAY_BRUSH ) );

   // Clean up

   DeleteEnhMetaFile( hemf );

   EndPaint( hWnd, &ps );

   hb_reta (4);
   HB_STORVNL ((LONG) rct.left,   -1, 1); 
   HB_STORVNL ((LONG) rct.top,    -1, 2); 
   HB_STORVNL ((LONG) rct.right,  -1, 3); 
   HB_STORVNL ((LONG) rct.bottom, -1, 4); 
}

HB_FUNC( _HMG_PRINTER_GETPAGEWIDTH )
{
   hb_retni( GetDeviceCaps( hmg_par_raw_HDC( 1 ), HORZSIZE ) );
}

HB_FUNC( _HMG_PRINTER_GETPAGEHEIGHT )
{
   hb_retni( GetDeviceCaps( hmg_par_raw_HDC( 1 ), VERTSIZE ) );
}

HB_FUNC( _HMG_PRINTER_PRINTPAGE )
{

#ifndef UNICODE
   LPSTR FileName = ( LPSTR ) hb_parc( 2 );
#else
   LPWSTR FileName = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif
   HENHMETAFILE hemf;

   RECT rect;

   hemf = GetEnhMetaFile( FileName );

   SetRect( &rect, 0, 0, GetDeviceCaps( hmg_par_raw_HDC( 1 ), HORZRES ), GetDeviceCaps( hmg_par_raw_HDC( 1 ), VERTRES ) );

   StartPage( hmg_par_raw_HDC( 1 ) );

   PlayEnhMetaFile( hmg_par_raw_HDC( 1 ), ( HENHMETAFILE ) hemf, &rect );

   EndPage( hmg_par_raw_HDC( 1 ) );

   DeleteEnhMetaFile( hemf );

}

HB_FUNC( _HMG_PRINTER_PREVIEW_ENABLESCROLLBARS )
{
   EnableScrollBar( hmg_par_raw_HWND( 1 ), SB_BOTH, ESB_ENABLE_BOTH  );
}

HB_FUNC( _HMG_PRINTER_PREVIEW_DISABLESCROLLBARS )
{
   EnableScrollBar( hmg_par_raw_HWND( 1 ), SB_BOTH, ESB_DISABLE_BOTH );
}

HB_FUNC( _HMG_PRINTER_PREVIEW_DISABLEHSCROLLBAR )
{
   EnableScrollBar( hmg_par_raw_HWND( 1 ), SB_HORZ, ESB_DISABLE_BOTH );
}

HB_FUNC( _HMG_PRINTER_GETPRINTERWIDTH )
{
   HDC hdc = hmg_par_raw_HDC( 1 );

   hb_retnl( GetDeviceCaps( hdc, HORZSIZE ) );
}

HB_FUNC( _HMG_PRINTER_GETPRINTERHEIGHT )
{
   HDC hdc = hmg_par_raw_HDC( 1 );

   hb_retnl( GetDeviceCaps( hdc, VERTSIZE ) );
}

HB_FUNC( _HMG_PRINTER_GETPRINTABLEAREAPHYSICALOFFSETX )
{
   HDC hdc = hmg_par_raw_HDC( 1 );

   hb_retnl( GetDeviceCaps( hdc, PHYSICALOFFSETX ) );
}

HB_FUNC( _HMG_PRINTER_GETPRINTABLEAREALOGPIXELSX )
{
   HDC hdc = hmg_par_raw_HDC( 1 );

   hb_retnl( GetDeviceCaps( hdc, LOGPIXELSX ) );
}

HB_FUNC( _HMG_PRINTER_GETPRINTABLEAREAPHYSICALOFFSETY )
{
   HDC hdc = hmg_par_raw_HDC( 1 );

   hb_retnl( GetDeviceCaps( hdc, PHYSICALOFFSETY ) );
}

HB_FUNC( _HMG_PRINTER_GETPRINTABLEAREALOGPIXELSY )
{
   HDC hdc = hmg_par_raw_HDC( 1 );

   hb_retnl( GetDeviceCaps( hdc, LOGPIXELSY ) );
}

HB_FUNC( _HMG_PRINTER_C_IMAGE )
{
   // 1: hDC
   // 2: Image File
   // 3: Row
   // 4: Col
   // 5: Height
   // 6: Width
   // 7: Stretch
   // 8: Transparent

   HDC hdcPrint = hmg_par_raw_HDC( 1 );

#ifndef UNICODE
   LPSTR FileName = ( LPSTR ) hb_parc( 2 );
#else
   LPWSTR FileName = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif
   BOOL    bBmpImage = TRUE;
   HBITMAP hBitmap;
   HRGN    hRgn;
   HDC     memDC;
   INT     nWidth, nHeight;
   POINT   Point;
   BITMAP  Bmp;
   int     r   = hb_parni( 3 ); // Row
   int     c   = hb_parni( 4 ); // Col
   int     odr = hb_parni( 5 ); // Height
   int     odc = hb_parni( 6 ); // Width
   int     dr;
   int     dc;

   if( hdcPrint != NULL )
   {
      c  = ( c * GetDeviceCaps( hdcPrint, LOGPIXELSX ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETX );
      r  = ( r * GetDeviceCaps( hdcPrint, LOGPIXELSY ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETY );
      dc = ( odc * GetDeviceCaps( hdcPrint, LOGPIXELSX ) / 1000 );
      dr = ( odr * GetDeviceCaps( hdcPrint, LOGPIXELSY ) / 1000 );

      hBitmap = ( HBITMAP ) LoadImage( GetInstance(), FileName, IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION );

      if( hBitmap == NULL )
         hBitmap = ( HBITMAP ) LoadImage( NULL, FileName, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | LR_CREATEDIBSECTION );

      if( hBitmap == NULL )
      {
         bBmpImage = FALSE;
         hBitmap   = HMG_LoadImage( ( char * ) hb_parc( 2 ) );
      }
      if( hBitmap == NULL )
         return;

      GetObject( hBitmap, sizeof( BITMAP ), &Bmp );
      nWidth  = Bmp.bmWidth;
      nHeight = Bmp.bmHeight;

      if( ! hb_parl( 7 ) ) // Scale
      {
         if( odr * nHeight / nWidth <= odr )
            dr = odc * GetDeviceCaps( hdcPrint, LOGPIXELSY ) / 1000 * nHeight / nWidth;
         else
            dc = odr * GetDeviceCaps( hdcPrint, LOGPIXELSX ) / 1000 * nWidth / nHeight;
      }

      GetViewportOrgEx( hdcPrint, &Point );

      hRgn = CreateRectRgn( c + Point.x,
                            r + Point.y,
                            c + dc + Point.x - 1,
                            r + dr + Point.y - 1 );

      SelectClipRgn( hdcPrint, hRgn );

      if( ! bBmpImage )
      {
         if( hb_parl( 7 ) )             // Stretch
            SetStretchBltMode( hdcPrint, COLORONCOLOR );
         else
         {
            GetBrushOrgEx( hdcPrint, &Point );
            SetStretchBltMode( hdcPrint, HALFTONE );
            SetBrushOrgEx( hdcPrint, Point.x, Point.y, NULL );
         }
      }

      memDC = CreateCompatibleDC( hdcPrint );
      SelectObject( memDC, hBitmap );

      if( hb_parl( 8 ) && ! bBmpImage ) // Transparent
         TransparentBlt( hdcPrint, c, r, dc, dr, memDC, 0, 0, nWidth, nHeight, GetPixel( memDC, 0, 0 ) );
      else
         StretchBlt( hdcPrint, c, r, dc, dr, memDC, 0, 0, nWidth, nHeight, SRCCOPY );

      SelectClipRgn( hdcPrint, NULL );

      DeleteObject( hBitmap );
      DeleteDC( memDC );
   }
}

//  GetJobInfo ( cPrinterName, nJobID ) --> { nJobID, cPrinterName, cMachineName, cUserName, cDocument, cDataType, cStatus, nStatus
//                                            nPriorityLevel, nPositionPrintQueue, nTotalPages, nPagesPrinted, cLocalDate, cLocalTime }
HB_FUNC( _HMG_PRINTGETJOBINFO )
{

#ifndef UNICODE
   LPSTR cPrinterName = ( LPSTR ) hb_parc( 1 );
#else
   LPWSTR cPrinterName = AnsiToWide( ( char * ) hb_parc( 1 ) );
   LPSTR  pStr;
#endif
   DWORD      nJobID   = hmg_par_DWORD( 2 );
   HANDLE     hPrinter     = NULL;
   TCHAR      cDateTime[ 256 ];
   SYSTEMTIME LocalSystemTime;

   if( OpenPrinter( cPrinterName, &hPrinter, NULL ) )
   {
      DWORD        nBytesNeeded = 0;
      DWORD        nBytesUsed   = 0;
      JOB_INFO_1 * Job_Info_1;

      GetJob( hPrinter, nJobID, 1, NULL, 0, &nBytesNeeded );

      if( nBytesNeeded > 0 )
      {
         Job_Info_1 = ( JOB_INFO_1 * ) hb_xgrab( nBytesNeeded );
         ZeroMemory( Job_Info_1, nBytesNeeded );

         if( GetJob( hPrinter, nJobID, 1, ( LPBYTE ) Job_Info_1, nBytesNeeded, &nBytesUsed ) )
         {
            hb_reta( 14 );
            HB_STORNI( ( INT ) Job_Info_1->JobId, -1, 1 );
#ifndef UNICODE
            HB_STORC(      Job_Info_1->pPrinterName, -1, 2 );
            HB_STORC(      Job_Info_1->pMachineName, -1, 3 );
            HB_STORC(      Job_Info_1->pUserName, -1, 4 );
            HB_STORC(      Job_Info_1->pDocument, -1, 5 );
            HB_STORC(      Job_Info_1->pDatatype, -1, 6 );
            HB_STORC(      Job_Info_1->pStatus, -1, 7 );
#else
            pStr = WideToAnsi( Job_Info_1->pPrinterName );
            HB_STORC(      pStr, -1, 2 );
            hb_xfree( pStr );
            pStr = WideToAnsi( Job_Info_1->pMachineName );
            HB_STORC(      pStr, -1, 3 );
            hb_xfree( pStr );
            pStr = WideToAnsi( Job_Info_1->pUserName );
            HB_STORC(      pStr, -1, 4 );
            hb_xfree( pStr );
            pStr = WideToAnsi( Job_Info_1->pDocument );
            HB_STORC(      pStr, -1, 5 );
            hb_xfree( pStr );
            pStr = WideToAnsi( Job_Info_1->pDatatype );
            HB_STORC(      pStr, -1, 6 );
            hb_xfree( pStr );
            pStr = WideToAnsi( Job_Info_1->pStatus );
            HB_STORC(      pStr, -1, 7 );
            hb_xfree( pStr );
#endif
            HB_STORNI( ( INT ) Job_Info_1->Status, -1, 8 );
            HB_STORNI( ( INT ) Job_Info_1->Priority, -1, 9 );
            HB_STORNI( ( INT ) Job_Info_1->Position, -1, 10 );
            HB_STORNI( ( INT ) Job_Info_1->TotalPages, -1, 11 );
            HB_STORNI( ( INT ) Job_Info_1->PagesPrinted, -1, 12 );

            SystemTimeToTzSpecificLocalTime( NULL, &Job_Info_1->Submitted, &LocalSystemTime );

            wsprintf( cDateTime, TEXT( "%02d/%02d/%02d" ), LocalSystemTime.wYear, LocalSystemTime.wMonth, LocalSystemTime.wDay );
#ifndef UNICODE
            HB_STORC( cDateTime, -1, 13 );
#else
            pStr = WideToAnsi( cDateTime );
            HB_STORC( pStr, -1, 13 );
            hb_xfree( pStr );
#endif

            wsprintf( cDateTime, TEXT( "%02d:%02d:%02d" ), LocalSystemTime.wHour, LocalSystemTime.wMinute, LocalSystemTime.wSecond );
#ifndef UNICODE
            HB_STORC( cDateTime, -1, 14 );
#else
            pStr = WideToAnsi( cDateTime );
            HB_STORC( pStr, -1, 14 );
            hb_xfree( pStr );
#endif
         }
         else
            hb_reta( 0 );

         if( Job_Info_1 )
            hb_xfree( ( void * ) Job_Info_1 );
      }
      else
         hb_reta( 0 );

      ClosePrinter( hPrinter );
   }
   else
      hb_reta( 0 );
}

HB_FUNC( _HMG_PRINTERGETSTATUS )
{

#ifndef UNICODE
   LPSTR cPrinterName = ( LPSTR ) hb_parc( 1 );
#else
   LPWSTR cPrinterName = AnsiToWide( ( char * ) hb_parc( 1 ) );
#endif
   HANDLE hPrinter     = NULL;
   DWORD  nBytesNeeded = 0;
   DWORD  nBytesUsed   = 0;
   PRINTER_INFO_6 * Printer_Info_6;

   if( OpenPrinter( cPrinterName, &hPrinter, NULL ) )
   {
      GetPrinter( hPrinter, 6, NULL, 0, &nBytesNeeded );
      if( nBytesNeeded > 0 )
      {
         Printer_Info_6 = ( PRINTER_INFO_6 * ) hb_xgrab( nBytesNeeded );
         ZeroMemory( Printer_Info_6, nBytesNeeded );

         if( GetPrinter( hPrinter, 6, ( LPBYTE ) Printer_Info_6, nBytesNeeded, &nBytesUsed ) )
            hb_retnl( Printer_Info_6->dwStatus );
         else
            hb_retnl( PRINTER_STATUS_NOT_AVAILABLE );

         if( Printer_Info_6 )
            hb_xfree( ( void * ) Printer_Info_6 );
      }
      else
         hb_retnl( PRINTER_STATUS_NOT_AVAILABLE );

      ClosePrinter( hPrinter );
   }
   else
      hb_retnl( PRINTER_STATUS_NOT_AVAILABLE );
}

HB_FUNC( GETTEXTALIGN )
{
   hb_retni( GetTextAlign( hmg_par_raw_HDC( 1 ) ) );
}

HB_FUNC( SETTEXTALIGN )
{
   hb_retni( SetTextAlign( hmg_par_raw_HDC( 1 ), hmg_par_UINT( 2 ) ) );
}

static HBITMAP loademffile( TCHAR * filename, int width, int height, HWND handle, int scalestrech, int whitebackground );

HB_FUNC( INITEMFFILE )
{

   DWORD Style = WS_CHILD | SS_BITMAP;

   if( ! hb_parl( 5 ) )
      Style |= WS_VISIBLE;

   if( hb_parl( 6 ) )
      Style |= SS_NOTIFY;

   hmg_ret_raw_HWND( CreateWindowEx( 0, WC_STATIC, NULL, Style, hb_parni( 3 ), hb_parni( 4 ), 0, 0, hmg_par_raw_HWND( 1 ), hmg_par_raw_HMENU( 2 ), GetInstance(), NULL ) );

}

HB_FUNC( C_SETEMFFILE )
{

#ifndef UNICODE
   TCHAR * cFileName = ( TCHAR * ) hb_parc( 2 );
#else
   TCHAR * cFileName = ( TCHAR * ) hb_osStrU16Encode( hb_parc( 2 ) );
#endif
   HBITMAP hBitmap;

   if( hb_parclen( 2 ) == 0 )
      hmg_ret_raw_HANDLE( NULL );

   hBitmap = loademffile( cFileName, hb_parni( 3 ), hb_parni( 4 ), hmg_par_raw_HWND( 1 ), hb_parni( 5 ), hb_parni( 6 ) );

   if( hBitmap != NULL )
      SendMessage( hmg_par_raw_HWND( 1 ), ( UINT ) STM_SETIMAGE, ( WPARAM ) IMAGE_BITMAP, ( LPARAM ) hBitmap );

   hmg_ret_raw_HANDLE( hBitmap );

}

static BOOL read_image( TCHAR * filename, DWORD * nFileSize, HGLOBAL * hMem )
{
   HANDLE hFile;
   LPVOID lpDest;
   DWORD  dwFileSize;
   DWORD  dwBytesRead = 0;
   BOOL   bRead;

   // open the file
   hFile = CreateFile( filename, GENERIC_READ, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL );
   if( hFile == INVALID_HANDLE_VALUE )
      return FALSE;
   // we will read the whole file in global memory, find the size first
   dwFileSize = GetFileSize( hFile, NULL );
   // allocate memory to read the whole file
   if( dwFileSize == INVALID_FILE_SIZE || ( *hMem = GlobalAlloc( GHND, dwFileSize ) ) == NULL )
   {
      CloseHandle( hFile );
      return FALSE;
   }
   *nFileSize = dwFileSize;
   // lock memory for image
   lpDest = GlobalLock( *hMem );
   if( lpDest == NULL )
   {
      GlobalFree( *hMem );
      CloseHandle( hFile );
      return FALSE;
   }
   // read file and store in global memory
   bRead = ReadFile( hFile, lpDest, dwFileSize, &dwBytesRead, NULL );

   GlobalUnlock( *hMem );
   CloseHandle( hFile );

   if( ! bRead )
   {
      GlobalFree( *hMem );
      return FALSE;
   }
   return TRUE;
}

static void calc_rect( HWND handle, int width, int height, int scalestrech, LONG lWidth, LONG lHeight, RECT * rect, RECT * rect2 )
{
   if( width == 0 && height == 0 )
      GetClientRect( handle, rect );
   else
      SetRect( rect, 0, 0, width, height );

   SetRect( rect2, 0, 0, rect->right, rect->bottom );

   if( scalestrech == 0 )
   {
      if( ( int ) lWidth * rect->bottom / lHeight <= rect->right )
         rect->right = ( int ) lWidth * rect->bottom / lHeight;
      else
         rect->bottom = ( int ) lHeight * rect->right / lWidth;
   }

   rect->left = ( int ) ( width - rect->right ) / 2;
   rect->top  = ( int ) ( height - rect->bottom ) / 2;
}

static HBITMAP loademffile( TCHAR * filename, int width, int height, HWND handle, int scalestrech, int whitebackground )
{
   IStream *  iStream;
   IPicture * iPicture = NULL;
   HGLOBAL    hMem     = ( HGLOBAL ) NULL;
   HRESULT    hr;
   DWORD      nFileSize = 0;
   RECT       rect, rect2;
   HBITMAP    bitmap;
   LONG       lWidth, lHeight;
   HDC        imgDC = GetDC( handle );
   HDC        tmpDC;

   if( read_image( filename, &nFileSize, &hMem ) == FALSE )
   {
      ReleaseDC( handle, imgDC );
      return NULL;
   }
   // don't delete memory on object's release
   hr = CreateStreamOnHGlobal( hMem, FALSE, &iStream );
   if( hr != S_OK || iStream == NULL )
   {
      GlobalFree( hMem );
      ReleaseDC( handle, imgDC );
      return NULL;
   }
   // Load from stream
#if defined( __cplusplus )
   hr = OleLoadPicture( iStream, nFileSize, ( nFileSize == 0 ), IID_IPicture, ( LPVOID * ) &iPicture );
#else
   hr = OleLoadPicture( iStream, nFileSize, ( nFileSize == 0 ), &IID_IPicture, ( LPVOID * ) &iPicture );
   iStream->lpVtbl->Release( iStream );
#endif
   if( hr != S_OK || iPicture == NULL )
   {
      GlobalFree( hMem );
      ReleaseDC( handle, imgDC );
      return NULL;
   }

   iPicture->lpVtbl->get_Width( iPicture, &lWidth );
   iPicture->lpVtbl->get_Height( iPicture, &lHeight );

   calc_rect( handle, width, height, scalestrech, lWidth, lHeight, &rect, &rect2 );

   tmpDC  = CreateCompatibleDC( imgDC );
   bitmap = CreateCompatibleBitmap( imgDC, width, height );
   SelectObject( tmpDC, bitmap );

   if( whitebackground == 1 )
      FillRect( tmpDC, &rect2, ( HBRUSH ) GetStockObject( WHITE_BRUSH ) );
   else
      FillRect( tmpDC, &rect2, ( HBRUSH ) GetSysColorBrush( COLOR_BTNFACE ) );

   // Render to device context
   iPicture->lpVtbl->Render( iPicture, tmpDC, rect.left, rect.top, rect.right, rect.bottom, 0, lHeight, lWidth, -lHeight, NULL );
   iPicture->lpVtbl->Release( iPicture );
   GlobalFree( hMem );

   DeleteDC( tmpDC );
   ReleaseDC( handle, imgDC );

   return bitmap;
}

HB_FUNC( ISWIN8ORLATER )
{
   OSVERSIONINFO osvi;

   osvi.dwOSVersionInfoSize = sizeof( OSVERSIONINFO );
   GetVersionEx( ( OSVERSIONINFO * ) &osvi );

   hb_retl( osvi.dwMajorVersion >= 6 && osvi.dwMinorVersion > 1 );
}

#pragma ENDDUMP
