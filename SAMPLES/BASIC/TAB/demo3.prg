/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2021 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2021 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
*/

ANNOUNCE RDDSYS

#include "hmg.ch"

Function Main

   LOCAL o, oApp, nW, nH, nWm, nHm, nWg, nHg

   App.Cargo := oKeyData() ; o := App.Cargo

   o:cFontName := "Arial"
   o:nFontSize := 11

   o:cTabFontName := "Comic Sans MS"
   o:nTabFontSize := 14
   o:nGaps        := 20

   // --------------------------------
   SET OOP ON    // necessarily
   // --------------------------------

   SET FONT TO o:cFontName, o:nFontSize

   oApp := App.Object

   nWm := oApp:Left
   nHm := oApp:Top
   nWg := oApp:GapsWidth
   nHg := oApp:GapsHeight

   nW  := oApp:W5 * 2
   nH  := oApp:H5 * 5

   IF nW > System.ClientWidth  ; nW := System.ClientWidth
   ENDIF
   IF nH > System.ClientHeight ; nH := System.ClientHeight
   ENDIF

   o:nGaps := nWm

   TabFontSet() // set the font for TAB

   DEFINE WINDOW Form_1 AT 0,0 CLIENTAREA nW, nH ;
      TITLE 'Tab Control OOP Demo' ;
      MAIN ;
      ON SIZE SizeTest()
      
      MenuMainUpForm()

      _CheckMenuItem( "I"+hb_ntos(App.Cargo:nTabFontSize), This.Name )

      (This.Object):Event(10, {|ow| // Change Font Size TAB
                    Local nSize := App.Cargo:nTabFontSize
                    Local nSold := Form_1.tab_1.Fontsize
                    TabFontSet()
                    // change the checkbox in the menu 
                    _UnCheckMenuItem( "I"+hb_ntos(nSold), This.Name )
                    _CheckMenuItem  ( "I"+hb_ntos(nSize), This.Name )
                    Form_1.tab_1.Fontsize := nSize
                    DO EVENTS
                    SizeTest()
                    DO EVENTS
                    Return Nil
                    })

      SetTab_1()  // building a TAB object

   END WINDOW

   Form_1.Center

   Form_1.Activate

Return Nil


Function TabFontSet()
   Local cFont := App.Cargo:cTabFontName
   Local nSize := App.Cargo:nTabFontSize

   DEFINE FONT FontTab  FONTNAME cFont SIZE nSize

Return Nil


Procedure SizeTest()
   Local o := App.Cargo
   Local nHBkm   := GetBookmarkHeight()
   Local nTabRow := Form_1.Tab_1.Row
   Local nG      := o:nGaps       // padding top, left, right, bottom

   DO EVENTS
   Form_1.Tab_1.Width    := Form_1.ClientWidth  - nG * 2
   Form_1.Tab_1.Height   := Form_1.ClientHeight - nG * 2

   IF _HMG_ActiveTabBottom 
      // Style - Bottom pages
      DO EVENTS
      Form_1.Label_1.Row    := nTabRow  
      Form_1.Label_1.Height := Form_1.Tab_1.Height - nG * 2 - nHBkm 
      Form_1.Label_1.Width  := Form_1.Tab_1.Width - nG * 2 

      Form_1.Frame_2.Row    := nTabRow - 5 // только для объекта Frame
      Form_1.Frame_2.Height := Form_1.Tab_1.Height - nG - nHBkm - nTabRow
      Form_1.Frame_2.Width  := Form_1.Tab_1.Width  - nG * 2

      Form_1.Frame_3.Row    := nTabRow  - 5 // только для объекта Frame
      Form_1.Frame_3.Height := Form_1.Tab_1.Height - nG - nHBkm - nTabRow
      Form_1.Frame_3.Width  := Form_1.Tab_1.Width  - nG * 2
   ELSE
      // Style - Top pages
      DO EVENTS
      Form_1.Label_1.Row    := Form_1.Tab_1.Row + nHBkm
      Form_1.Label_1.Height := Form_1.Tab_1.Height - nG - nHBkm - nTabRow
      Form_1.Label_1.Width  := Form_1.Tab_1.Width  - nG * 2

      Form_1.Frame_2.Row    := Form_1.Tab_1.Row + nHBkm 
      Form_1.Frame_2.Height := Form_1.Tab_1.Height - nG - nHBkm - nTabRow
      Form_1.Frame_2.Width  := Form_1.Tab_1.Width  - nG * 2

      Form_1.Frame_3.Row    := Form_1.Tab_1.Row + nHBkm 
      Form_1.Frame_3.Height := Form_1.Tab_1.Height - nG - nHBkm - nTabRow
      Form_1.Frame_3.Width  := Form_1.Tab_1.Width  - nG * 2
   ENDIF

Return


#define COLOR_BTNFACE 15

Procedure SetTab_1( lBottomStyle )
   Local o := App.Cargo, y, x
   Local oApp := App.Object
   Local nColor := GetSysColor( COLOR_BTNFACE )
   Local aColor := {GetRed( nColor ), GetGreen( nColor ), GetBlue( nColor )}
   Local nTabRow, nTabCol, nTabWidth, nTabHeight, nHBkm
   Local cFontTab  := o:cTabFontName
   Local nFSizeTab := o:nTabFontSize
   Local nG        := o:nGaps        // padding top, left, right, bottom

   Default lBottomStyle := .f. 

   IF IsControlDefined(Tab_1, Form_1)
      Form_1.Tab_1.Release
   ENDIF

   nTabRow    := nTabCol := nG
   nTabWidth  := Form_1.ClientWidth  - nG * 2
   nTabHeight := Form_1.ClientHeight - nG * 2

   DEFINE TAB Tab_1       ;
      OF Form_1           ;
      AT nTabRow, nTabCol ;
      WIDTH nTabWidth     ;
      HEIGHT nTabHeight   ;
      VALUE 1             ;
      HOTTRACK            ;
      FONT cFontTab SIZE nFSizeTab BOLD ;
      ON CHANGE MsgInfo( 'Page is changed!' )

      nHBkm := my2GetBookmarkHeight(nFSizeTab)

      _HMG_ActiveTabBottom := lBottomStyle

      PAGE 'Page &1 ' IMAGE 'Exit.Bmp' TOOLTIP 'TabPage 1'

         @ nTabRow + nHBkm, nTabCol LABEL Label_1 VALUE "Label for Page 1" ;
           WIDTH nTabWidth - nG*2 HEIGHT nTabHeight - nHBkm - nG - nTabRow BACKCOLOR WHITE 

         y := 150 ; x := 100
         @ y, x BUTTON Button_11 CAPTION "Test 1a" WIDTH oApp:W1 HEIGHT oApp:H2 ; 
                ACTION MsgInfo('Press! '+This.Caption) NOTABSTOP
         x += This.Button_11.Width + oApp:GapsWidth
         @ y, x BUTTON Button_12 CAPTION "Test 1b" WIDTH oApp:W1 HEIGHT oApp:H2 ; 
                ACTION MsgInfo('Press! '+This.Caption) NOTABSTOP

      END PAGE

      PAGE 'Page &2 ' IMAGE 'Info.Bmp' TOOLTIP 'TabPage 2'

         @ nTabRow + nHBkm - 5, nTabCol FRAME Frame_2 CAPTION "Frame for Page 2" ;
           WIDTH nTabWidth - nG*2 HEIGHT nTabHeight - nHBkm - nG - nTabRow TRANSPARENT

         y := 150 ; x := 100
         @ y, x BUTTON Button_21 CAPTION "Test 2a" WIDTH oApp:W2 HEIGHT oApp:H2 ;
                ACTION MsgInfo('Press! '+This.Caption) NOTABSTOP
         y += This.Button_21.Height + oApp:GapsHeight
         @ y, x BUTTON Button_22 CAPTION "Test 2b" WIDTH oApp:W2 HEIGHT oApp:H2 ; 
                ACTION MsgInfo('Press! '+This.Caption) NOTABSTOP

      END PAGE

      PAGE 'Page &3 ' IMAGE 'Check.Bmp'

         @ nTabRow + nHBkm - 5, nTabCol FRAME Frame_3 CAPTION "Frame for Page 3" ;
           WIDTH nTabWidth - nG*2 HEIGHT nTabHeight - nHBkm - nG - nTabRow TRANSPARENT

         y := 150 ; x := 100
         @ y, x BUTTON Button_31 CAPTION "Test 3a" WIDTH oApp:W3 HEIGHT oApp:H3 ; 
                ACTION MsgInfo('Press! '+This.Caption) NOTABSTOP
         y += This.Button_31.Height + oApp:GapsHeight
         @ y, x BUTTON Button_32 CAPTION "Test 3b" WIDTH oApp:W3 HEIGHT oApp:H3 ; 
                ACTION MsgInfo('Press! '+This.Caption) NOTABSTOP

      END PAGE

   END TAB

   Form_1.Tab_1.BACKCOLOR := aColor
   Form_1.Tab_1.HTFORECOLOR := BLUE
   Form_1.Tab_1.HTINACTIVECOLOR := GRAY

Return


STATIC FUNCTION MenuMainUpForm()

      DEFINE MAIN MENU

         DEFINE POPUP 'Style'
            MENUITEM 'Top pages' ACTION SetTab_1()
            MENUITEM 'Bottom pages' ACTION ( SetTab_1( .T. ) , DoEvents(), SizeTest() )
            SEPARATOR 
            MENUITEM 'Exit' ACTION ThisWindow.Release
         END POPUP

         DEFINE POPUP 'Tests'
            MENUITEM 'Change Page' ACTION Form_1.tab_1.Value := 2
            MENUITEM 'Get Page Count' ACTION MsgInfo(Str(Form_1.tab_1.ItemCount))
            SEPARATOR
            MENUITEM 'Add Page' ACTION Form_1.Tab_1.AddPage ( 2 , '&New Page' , 'Info.Bmp' , 'New Page' )
            MENUITEM 'Delete Page' ACTION Form_1.tab_1.DeletePage ( 2 )
            SEPARATOR
            MENUITEM 'Change Image' ACTION Form_1.Tab_1.Image( 1 ) := 'Info.Bmp'
            MENUITEM 'Replace Image' ACTION Form_1.Tab_1.Image( 1 ) := 'Exit.Bmp'
            SEPARATOR
            MENUITEM 'Change Caption' ACTION Form_1.Tab_1.Caption( 1 ) := 'Caption'
            MENUITEM 'Replace Caption' ACTION Form_1.Tab_1.Caption( 1 ) := 'Page &1'
            SEPARATOR
            MENUITEM 'Change Tooltip of Page 3' ACTION Form_1.Tab_1.Tooltip( 3 ) := 'TabPage 3'
            MENUITEM 'Get Tooltip of Page 3' ACTION MsgInfo( GetProperty ( 'Form_1', 'Tab_1', 'Tooltip', 3 ) )
            SEPARATOR
            MENUITEM 'Get Row' ACTION MsgInfo(Str(Form_1.tab_1.Row))
            MENUITEM 'Get Col' ACTION MsgInfo(Str(Form_1.tab_1.Col))
            MENUITEM 'Get Width' ACTION MsgInfo(Str(Form_1.tab_1.Width))
            MENUITEM 'Get Height' ACTION MsgInfo(Str(Form_1.tab_1.Height))
            SEPARATOR
            MENUITEM 'Set Row' ACTION Form_1.tab_1.Row := Val( InputBox('',''))
            MENUITEM 'Set Col' ACTION Form_1.tab_1.Col:= Val( InputBox('',''))
            MENUITEM 'Set Width' ACTION Form_1.tab_1.Width:= Val( InputBox('',''))
            MENUITEM 'Set Height' ACTION Form_1.tab_1.Height:= Val( InputBox('',''))
            SEPARATOR
            * Optional Syntax (Refer button as tab child )
            MENUITEM 'Get Button Caption' ACTION MsgInfo ( Form_1.Tab_1(1).Button_11.Caption ) 
            MENUITEM 'Set Button Caption' ACTION Form_1.Tab_1(1).Button_11.Caption := 'New'
         END POPUP

         DEFINE POPUP 'Tests-2'
            POPUP 'Change Font Size Tab '                                        NAME I00 
               MENUITEM '10' ACTION ( App.Cargo:nTabFontSize := 10, _wPost(10) ) NAME I10  
               MENUITEM '12' ACTION ( App.Cargo:nTabFontSize := 12, _wPost(10) ) NAME I12  
               MENUITEM '14' ACTION ( App.Cargo:nTabFontSize := 14, _wPost(10) ) NAME I14  
               MENUITEM '16' ACTION ( App.Cargo:nTabFontSize := 16, _wPost(10) ) NAME I16  
               MENUITEM '18' ACTION ( App.Cargo:nTabFontSize := 18, _wPost(10) ) NAME I18  
               MENUITEM '20' ACTION ( App.Cargo:nTabFontSize := 20, _wPost(10) ) NAME I20  
               MENUITEM '22' ACTION ( App.Cargo:nTabFontSize := 22, _wPost(10) ) NAME I22  
               MENUITEM '24' ACTION ( App.Cargo:nTabFontSize := 24, _wPost(10) ) NAME I24  
               MENUITEM '28' ACTION ( App.Cargo:nTabFontSize := 28, _wPost(10) ) NAME I28  
            END POPUP                 
            SEPARATOR
            MENUITEM 'Change Color font Tab - RED'   ACTION ( Form_1.Tab_1.HTFORECOLOR := RED  , SetTabCaption ( Form_1.tab_1.handle , Form_1.tab_1.Value , Form_1.Tab_1.Caption( Form_1.tab_1.Value ) ) )
            MENUITEM 'Change Color font Tab - GREEN' ACTION ( Form_1.Tab_1.HTFORECOLOR := GREEN, SetTabCaption ( Form_1.tab_1.handle , Form_1.tab_1.Value , Form_1.Tab_1.Caption( Form_1.tab_1.Value ) ) )
            SEPARATOR
            MENUITEM 'Get Bookmark height'         ACTION MsgDebug("Get Bookmark height =",GetBookmarkHeight())
            MENUITEM 'Approximate height Bookmark' ACTION MsgDebug("Approximate height Bookmark =",my2GetBookmarkHeight(App.Cargo:nTabFontSize))
            MENUITEM 'Bookmark height from array'  ACTION MsgDebug("Bookmark height from array =",my2GetDimBookmarkHeight(App.Cargo:nTabFontSize))
         END POPUP
      END MENU

RETURN Nil

Function GetBookmarkHeight()
   Local idx := Form_1.Tab_1.Index
Return _HMG_aControlMiscData1 [idx] [1]


// will return the height of the TAB tab from the font height
Static Function my2GetBookmarkHeight()
Return GetFontHeight("FontTab") + 10


// will return the height of the TAB tab from the font height
// Bookmark height from array 5-38px font height
Static Function my2GetDimBookmarkHeight(nFsize) 
   Local aRanges, i, aRange, nHRet := -1

   aRanges := { {5,20},{6,20},{7,22},{8,25},{9,25},{10,28},{11,30},;
                {12,32},{13,34},{14,36},{15,38},{16,39},{17,41},{18,44},;
                {19,44},{20,47},{21,48},{22,49},{23,53},{24,54},{25,54},;
                {26,58},{27,59},{28,60},{29,64},{30,64},{31,67},{32,69},;
                {33,69},{34,72},{35,74},{36,76},{37,77},{38,80} }

   For i := 1 TO LEN(aRanges)
      aRange := aRanges[i]
      If nFsize <= aRange[1]
         nHRet = aRange[2]
         Exit
      Endif
   Next
   If nHRet == -1
      nHRet := aRanges[LEN(aRanges)][2]
   Endif
Return nHRet
