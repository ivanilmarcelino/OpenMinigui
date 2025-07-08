/*                                                         
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2018 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Show the different fonts depending on a screen resolution
 */

ANNOUNCE RDDSYS

#define _HMG_OUTLOG

#include "minigui.ch"
#include "i_winuser.ch"

#define  SHOW_TITLE  "Show the different fonts depending on a screen resolution"

STATIC aStatObj
///////////////////////////////////////////////////////////////////////////////
FUNCTION MAIN()
   LOCAL nMaxWidth, nMaxHeight, nBarHeight, cText, nTxtHeight, nTxtWidth
   LOCAL cFontName1 := _HMG_DefaultFontName
   LOCAL cFontName2 := "Tahoma"
   LOCAL cFontName3 := "Times New Roman"
   LOCAL cFontName4 := "Courier New"
   LOCAL cFontName5 := "Arial"
   LOCAL cFontName6 := "Comic Sans MS" 
   LOCAL cFontName7 := "Arial Black"
   LOCAL aFont  := {cFontName1,cFontName2,cFontName3,cFontName4,cFontName5,cFontName6,cFontName7}
   LOCAL aColor := {BLACK,BLUE,LGREEN,RED,MAROON,PURPLE,YELLOW}
   LOCAL cMode  := HB_NtoS(GetDesktopWidth())+"x"+HB_NtoS(GetDesktopHeight())
   LOCAL nI, nJ, cObj, cLabel, nLabelW, cFont, cObj1, cObj2, nRow, aFColor
   LOCAL nWintWidth, nFontSize, nAutoMGFS, nKoeffMode

   SET LANGUAGE TO RUSSIAN  
   SET CODEPAGE TO RUSSIAN  
   SET DECIMALS TO 4
   SET DATE     TO GERMAN
   SET EPOCH    TO 2000
   SET CENTURY  ON
   SET EXACT    ON

   _HMG_MESSAGE[4] := "Attempt to start the second copy of the program:" + CRLF + ; 
                      App.ExeName + CRLF + ; 
                      "Startup denied." + CRLF + _HMG_MESSAGE[4]    

   SET MULTIPLE OFF WARNING 

   DEFINE FONT Font_1  FONTNAME cFontName1 SIZE nFontSize 
   DEFINE FONT Font_2  FONTNAME cFontName2 SIZE nFontSize 
   DEFINE FONT Font_3  FONTNAME cFontName3 SIZE nFontSize 
   DEFINE FONT Font_4  FONTNAME cFontName4 SIZE nFontSize 
   DEFINE FONT Font_5  FONTNAME cFontName5 SIZE nFontSize 
   DEFINE FONT Font_6  FONTNAME cFontName6 SIZE nFontSize BOLD
   DEFINE FONT Font_7  FONTNAME cFontName6 SIZE nFontSize 

   aStatObj   := {}   // put all the objects that need to be redrawn on the form
   nMaxWidth  := GetDesktopWidth()  
   nMaxHeight := GetDesktopHeight() - GetTaskBarHeight() - 10
   nKoeffMode := IIF( GetDesktopHeight() <= 768, 50, 80 )  

   // automatically set the font size depending on the screen resolution
   nFontSize  := ModeSizeFont()        // -> util_fonts.prg
   // MiniGUI font size
   nAutoMGFS  := _HMG_DefaultFontSize  // from MiniGUI.Init()

   SET FONT     TO cFontName1, nFontSize

   DEFINE WINDOW test                          ;
      AT 0,0 WIDTH nMaxWidth HEIGHT nMaxHeight ;
      MINWIDTH 600 MINHEIGHT 400               ; // restriction of window coordinate changes
      TITLE SHOW_TITLE                         ;
      ICON "1hmg_ico"                          ;
      MAIN                                     ;
      BACKCOLOR SILVER                         ;
      ON SIZE     {|| ResizeFormTest()  }      ; // executed when window coordinates change
      ON MAXIMIZE {|| ResizeFormTest()  }      ; // executed when window coordinates change
      ON INIT     {|| InitFormTest()    }      ; // executed after window initialization
      ON RELEASE  {|| ReleaseFormTest() }      ; // executed before the window is destroyed
      ON INTERACTIVECLOSE {|| MsgInfo("Close [x]") }  // close the window by [x]

      // do reassignment, now it's the internal dimensions of the window
      nMaxWidth  := This.ClientWidth   
      nMaxHeight := This.ClientHeight

      DEFINE STATUSBAR FONT 'MS Sans Serif' SIZE 8
         STATUSITEM "Item 1"    ACTION MsgInfo('Click! 1')
         STATUSITEM "Item 2"    WIDTH 100 ACTION MsgInfo('Click! 2') 
         STATUSITEM 'Hello !'   WIDTH 100 ICON 'smile_ico'
         CLOCK 
         DATE 
         KEYBOARD
      END STATUSBAR

      // object at the bottom of the window
      nBarHeight := 36
      nRow := nMaxHeight - nBarHeight - 10 - GetStatusBarHeight() // consider the height of the StstusBar
      @ nRow, 30 PROGRESSBAR Bar_1 WIDTH nMaxWidth - 30*2 ;
        HEIGHT nBarHeight RANGE 0,100  VALUE 100   

      // first line of fonts horizontally
      cText := REPL("1234567890",20)
      nTxtHeight := GetTxtHeight(,nFontSize,cFontName1) + 2
      @ 0, 0 LABEL Label_1 WIDTH nMaxWidth HEIGHT nTxtHeight VALUE cText ;
         FONTCOLOR BLACK  TRANSPARENT 
      AADD( aStatObj , "Label_1" ) // to redraw the object on the form

      cText := "1234567890"
      nTxtHeight := GetTxtHeight(,nFontSize,cFontName1)      // determine the height of the font
      nTxtWidth  := GetTxtWidth(cText,nFontSize,cFontName1)  // determine the width of the text
      FOR nI := 1 TO 20
         cObj    := "Label_2i" + HB_NtoS(nI)
	 cLabel  := "|" + HB_NtoS(nI) + "0"
         nLabelW := GetTxtWidth(cLabel,nFontSize,cFontName1)
         @ nTxtHeight-5, nI * nTxtWidth-8 LABEL &cObj WIDTH nLabelW HEIGHT nTxtHeight ;
           VALUE cLabel FONTCOLOR BLACK TRANSPARENT 
         AADD( aStatObj , cObj ) // to redraw the object on the form
      NEXT

      cLabel := "AutoMiniGuiFont: " + cFontName1 + "   Size: " + HB_NtoS(nFontSize) 
      cLabel += "   Display: " + cMode + "   LargeFont: " + IIF(Large2Fonts(),"True","False")
      cLabel += "   AutoMiniGuiFontSize: " + HB_NtoS(nAutoMGFS)
      @ nTxtHeight*2-5, nTxtWidth-8 LABEL Label_2 WIDTH nMaxWidth HEIGHT nTxtHeight+6 ;
          VALUE cLabel FONTCOLOR BLACK TRANSPARENT VCENTERALIGN
      AADD( aStatObj , "Label_2" ) // to redraw the object on the form

      nRow  := GetProperty( ThisWindow.Name, "Label_2", "ROW" ) + nTxtHeight + 10

      // first line of fonts vertically
      FOR nI := 1 TO 40
         cObj    := "Label_v" + HB_NtoS(nI)
	 cLabel  := HB_NtoS(nI+1)
         nLabelW := GetTxtWidth(cLabel,nFontSize,cFontName1)
         @ nTxtHeight * nI - 5 , 2 LABEL &cObj WIDTH nLabelW HEIGHT nTxtHeight ;
           VALUE cLabel FONTCOLOR BLACK TRANSPARENT 
         AADD( aStatObj , cObj ) // to redraw the object on the form
      NEXT

      // other lines of fonts horizontally
      nRow -= nKoeffMode // for the first display of the font line 2
      FOR nJ := 2 TO LEN(aFont) - IIF( GetDesktopHeight() <= 768, 1, 0 )

         nRow       += nKoeffMode 
         cText      := REPL("1234567890",20)
         cFont      := aFont[ nJ ]
         aFColor    := aColor[ nJ ]
         cObj1      := "Lbl_F"+ HB_NtoS(nJ) 
         nTxtHeight := GetTxtHeight(,nFontSize,cFont) + 2
         @ nRow, 0 LABEL &cObj1 WIDTH nMaxWidth HEIGHT nTxtHeight VALUE cText ;
            FONT cFont SIZE nFontSize FONTCOLOR aFColor TRANSPARENT 
         AADD( aStatObj , cObj1 ) // to redraw the object on the form

         cText := "1234567890"
         nTxtHeight := GetTxtHeight(,nFontSize,cFont)      // determine the height of the font
         nTxtWidth  := GetTxtWidth(cText,nFontSize,cFont)  // determine the width of the text
         FOR nI := 1 TO 20
            cObj    := "Lbl_" + HB_NtoS(nJ) + HB_NtoS(nI)
            cLabel  := "|" + HB_NtoS(nI) + "0"
            nLabelW := GetTxtWidth(cLabel,nFontSize,cFont)
            @ nRow+nTxtHeight-5, nI * nTxtWidth-8 LABEL &cObj WIDTH nLabelW HEIGHT nTxtHeight ;
              VALUE cLabel FONT cFont SIZE nFontSize FONTCOLOR aFColor TRANSPARENT 
            AADD( aStatObj , cObj ) // to redraw the object on the form
         NEXT

         cLabel := "Font: " + cFont + "   Size: " + HB_NtoS(nFontSize) 
         cLabel += "   Display: " + cMode + "   LargeFont: " + IIF(Large2Fonts(),"True","False")
         cLabel += "   AutoMiniGuiFontSize: " + HB_NtoS(nAutoMGFS)
         cObj2  := "Lbl_N"+ HB_NtoS(nJ) 
         @ nRow+nTxtHeight*2-5, nTxtWidth-8 LABEL &cObj2 WIDTH nMaxWidth HEIGHT nTxtHeight+6 ;
             VALUE cLabel FONT cFont SIZE nFontSize FONTCOLOR aFColor TRANSPARENT VCENTERALIGN
         AADD( aStatObj , cObj2 ) // to redraw the object on the form

      NEXT

      // calculate the font size for 100 characters of the line, depending on the width of the screen
      // calculating the font size for 100 characters of the string depending on the screen width
      //       width of the inner window - indent left and right
      nWintWidth := nMaxWidth - 10*2
      cText  := REPL("1234567890",10)   // output - given string

      // The function will return the maximum font size for the given string
      // at the specified width -> util_fonts.prg
      nFontSize  := FontSizeMaxAutoFit( cText, cFontName2, nWintWidth )

      nTxtHeight := GetTxtHeight(,nFontSize,cFontName2)      // determine the height of the font
      nTxtWidth  := GetTxtWidth(cText,nFontSize,cFontName2)  // determine the width of the text

      cObj2 := aStatObj[ LEN(aStatObj) ] // name of the last object
      nRow  := GetProperty( ThisWindow.Name, cObj2, "ROW" ) + 40
      
      @ nRow, 0 LABEL Label_3 WIDTH nMaxWidth HEIGHT nTxtHeight VALUE cText ;
        FONT cFontName2 SIZE nFontSize FONTCOLOR BLACK TRANSPARENT 

      cLabel := "Auto-fit font height for 100 character string - " 
      cLabel += "Font: " + cFontName2 + "  Size: " + HB_NtoS(nFontSize) 

      @ nRow + nTxtHeight, 20 LABEL Label_4 WIDTH nTxtWidth HEIGHT nTxtHeight VALUE cLabel ;
        FONT cFontName2 SIZE nFontSize FONTCOLOR BLACK TRANSPARENT 

      AADD( aStatObj , "Label_3" ) // to redraw the object on the form
      AADD( aStatObj , "Label_4" ) // to redraw the object on the form

      nRow  := GetProperty( ThisWindow.Name, "Label_4", "ROW" ) + 50
      nRow  := nRow - IIF( GetDesktopHeight() <= 864, 25, 0 )

      @ nRow, 100 BUTTONEX BUTTON_Plus WIDTH 40 HEIGHT 40                                     ;
         CAPTION "+" ICON NIL FONTCOLOR BLACK                                            ;
         FONT "Arial Black" SIZE 18 BOLD FLAT NOXPSTYLE HANDCURSOR NOTABSTOP             ;
         BACKCOLOR { { 0.5, CLR_GRAY, CLR_SKYPE }    , { 0.5, CLR_SKYPE , CLR_GRAY } }   ;
         GRADIENTFILL { { 0.5, CLR_SKYPE, CLR_WHITE }, { 0.5, CLR_WHITE , CLR_SKYPE } }  ; 
         ON MOUSEHOVER ( This.Backcolor := BLACK , This.Fontcolor := WHITE  )            ; 
         ON MOUSELEAVE ( This.Backcolor := ORANGE, This.Fontcolor := BLACK  )            ; 
         ACTION {|| MySizeGetBox(+1,"GetBox_1","Label_Size"), test.Label_4.Setfocus }

      @ nRow, 150 BUTTONEX BUTTON_Minus WIDTH 40 HEIGHT 40                                     ;
         CAPTION "-" ICON NIL FONTCOLOR BLACK                                           ;
         FONT "Arial Black" SIZE 18 BOLD FLAT NOXPSTYLE HANDCURSOR NOTABSTOP            ;
         BACKCOLOR  { { 0.5, CLR_GRAY, CLR_GREEN    }, { 0.5, CLR_GREEN, CLR_GRAY   } } ;
         GRADIENTFILL { { 0.5, CLR_GREEN, CLR_WHITE }, { 0.5, CLR_WHITE , CLR_GREEN } } ; 
         ON MOUSEHOVER ( This.Backcolor := BLACK , This.Fontcolor := WHITE  )           ; 
         ON MOUSELEAVE ( This.Backcolor := ORANGE, This.Fontcolor := BLACK  )           ; 
         ACTION {|| MySizeGetBox(-1,"GetBox_1","Label_Size"), test.Label_4.Setfocus }

      cText  := DTOC(DATE()) + "0"                  // counting characters of a given string
      nTxtHeight := GetTxtHeight(,nFontSize,cFontName2)      // determine the height of the font
      nTxtWidth  := GetTxtWidth(cText,nFontSize,cFontName2)  // determine the width of the text

      @ nRow, 200 LABEL Label_Size WIDTH nTxtWidth HEIGHT nFontSize*2 VALUE ":" + HB_NtoS(nFontSize) ;
        FONT cFontName2 SIZE nFontSize BOLD FONTCOLOR BLACK TRANSPARENT 

      @ nRow, 270 GETBOX GetBox_1 HEIGHT nTxtHeight WIDTH nTxtWidth ;
        VALUE DATE() PICTURE '@K'                                 ;
        MESSAGE "Date Value - sample message"                     ;
        BACKCOLOR { WHITE, {255,255,200},{200,255,255}}           ;
        FONTCOLOR { BLUE , {255,255,200}, BLUE        }           ;
        FONT cFontName2 SIZE nFontSize

      @ nRow, 820 BUTTONEX BUTTON_Dir WIDTH 54 HEIGHT 54                                ;
         CAPTION CHR(49) ICON NIL FONTCOLOR BLACK                                       ;
         FONT "Wingdings" SIZE 24 BOLD FLAT NOXPSTYLE HANDCURSOR NOTABSTOP              ;
         BACKCOLOR  { { 0.5, CLR_GRAY, CLR_BROWN    }, { 0.5, CLR_BROWN, CLR_GRAY  } }  ;
         GRADIENTFILL { { 0.5, CLR_BROWN, CLR_WHITE }, { 0.5, CLR_WHITE, CLR_BROWN } }  ; 
         ON MOUSEHOVER ( This.Backcolor := BLACK , This.Fontcolor := WHITE  )           ; 
         ON MOUSELEAVE ( This.Backcolor := ORANGE, This.Fontcolor := BLACK  )           ; 
         ACTION {|| MyTableFontWingdings(), test.Label_4.Setfocus }


      ON KEY ESCAPE ACTION ThisWindow.Release   // close the window by ESC

   END WINDOW

   //CENTER   WINDOW test
   ACTIVATE WINDOW test
   
RETURN NIL

//////////////////////////////////////////////////////////////////
//This function initializes the form, or another function,
//where you can transfer the management of the program the main window
// This function initializes the form, or calls another
// functions where you can transfer program control from the main window
STATIC FUNCTION InitFormTest()

   ? "Start - " + cFileNoPath( Application.ExeName ) 
   ? "  "+ OS()
   ?? "  "+ MiniGuiVersion()

RETURN NIL

//////////////////////////////////////////////////////////////////
// In this function, we perform all actions before closing the form
// In this function, we perform all actions before closing the form
STATIC FUNCTION ReleaseFormTest()

   ? "Closing the program - " + DTOC(DATE()) + " " + TIME()
   ? "..."

RETURN NIL

///////////////////////////////////////////////////////////////////////////
// executed when window coordinates change
FUNCTION ResizeFormTest()
   LOCAL cForm := ThisWindow.Name , lExit := .T. 
   LOCAL nI, cObj, nRow, nWBar, nHBar, nFWidth, nFHeight, cText
   LOCAL nFontSize, cFontName, nWinWidth, cVal

   // window dimensions after changing it
   nFWidth  := This.ClientWidth   
   nFHeight := This.ClientHeight   

   cObj  := "Bar_1"                               // object name
   nHBar := GetProperty( cForm, cObj, "HEIGHT" )  // object height
   nRow  := nFHeight - nHBar - 10                 // position of the object in the Y coordinate
   nRow  := nRow  - GetStatusBarHeight()          // consider the height of StstusBar
   nWBar := nFWidth - 30*2                        // object width

   // change the location of the object Bar_1
   SetProperty( cForm, cObj, "ROW"   , nRow  )
   SetProperty( cForm, cObj, "WIDTH" , nWBar )
   SetProperty( cForm, cObj, "HEIGHT", nHBar )   
   Domethod( cForm, cObj , "Setfocus" )

   // redraw Label_3 and Label_4 objects
   // width of the inner window - indent left and right
   nWinWidth := nFWidth - 10*2
   cText     := GetProperty( cForm, "Label_3", "VALUE" )
   cFontName := GetProperty( cForm, "Label_3", "FONTNAME" )

   // The function will return the maximum font size for the given string
   // at the specified width -> util_fonts.prg
   nFontSize  := FontSizeMaxAutoFit( cText, cFontName, nWinWidth )
   SetProperty( cForm, "Label_3", "FONTSIZE" , nFontSize  )

   cVal := GetProperty( cForm, "Label_4", "VALUE" ) 
   cVal := SUBSTR( cVal, 1 , RAT(":", cVal) + 1 ) + HB_NtoS(nFontSize)
   SetProperty( cForm, "Label_4", "FONTSIZE" , nFontSize  )
   SetProperty( cForm, "Label_4", "VALUE"    , cVal       )

   // redraw objects from aStatObj
   FOR nI := 1 TO LEN(aStatObj)
      cObj := aStatObj[nI]
      Domethod( cForm, cObj , "ReDraw" )
   NEXT

RETURN NIL

///////////////////////////////////////////////////////////////////////////
// executed when the button is clicked
FUNCTION MySizeGetBox(nSign,cGetBox,cLabel)  
   LOCAL cForm := ThisWindow.Name 
   LOCAL cText, nTxtHeight, nTxtWidth, nFontSize, cFontName, cVal

   cFontName := GetProperty( cForm, cGetBox, "FONTNAME" )
   nFontSize := GetProperty( cForm, cGetBox, "FONTSIZE" )
   IF nSign == -1
      nFontSize--
      nFontSize := IIF( nFontSize < 6, 6, nFontSize )
      cVal := "Minus button pressed"    
   ELSE
      nFontSize++
      nFontSize := IIF( nFontSize > 72, 72, nFontSize )
      cVal := "The plus button is pressed"
   ENDIF
   //test.StatusBar.Item(1) := cVal
   SetProperty ( cForm, "StatusBar" , "Item" , 1 , cVal )

   cText  := DTOC(DATE()) + "0"                   // counting characters of a given string
   nTxtHeight := GetTxtHeight(,nFontSize,cFontName)      // determine the height of the font
   nTxtWidth  := GetTxtWidth(cText,nFontSize,cFontName)  // determine the width of the text
   nTxtWidth  := nTxtWidth + IIF( GetDesktopHeight() <= 960, 5 , 0 ) // correction factor

   // redraw cLabel and cGetBox objects
   cVal := ":" + HB_NtoS(nFontSize)
   SetProperty( cForm, cLabel, "VALUE"    , cVal        )

   SetProperty( cForm, cGetBox, "FONTSIZE" , nFontSize  )
   SetProperty( cForm, cGetBox, "WIDTH"    , nTxtWidth  )
   SetProperty( cForm, cGetBox, "HEIGHT"   , nTxtHeight )   

RETURN NIL

///////////////////////////////////////////////////////////////////////////
// font table Wingdings
FUNCTION MyTableFontWingdings()
   LOCAL hWnd, cForm := ThisWindow.Name 
   LOCAL nMaxWidth, nMaxHeight, cTitle, nTxtHeight, nTxtWidth
   LOCAL nJ, nI, nCol, nRow, cObj, cFontName, nFontSize, cFont
   LOCAL nVirtHeight, nLineAll, nLineLetters, nLetters
   LOCAL aFont := {"Wingdings", "Wingdings 2", "Wingdings 3" }

   cFontName := GetProperty( cForm, "GetBox_1", "FONTNAME" )
   nFontSize := GetProperty( cForm, "GetBox_1", "FONTSIZE" )

   nMaxWidth  := GetDesktopWidth() * 0.8
   nMaxHeight := GetDesktopHeight() * 0.8
    
   cFont := IIF( nFontSize < 16, aFont[1], aFont[3] )
   nTxtWidth    := GetTxtWidth(,nFontSize, cFont )    // determine the width of the font letter
   nLetters     := nMaxWidth / nTxtWidth              // number of letters across the window
   nLineLetters := 256 / nLetters                     // number of lines of output characters
   nLineAll     := nLineLetters * 3 + 3 *2            // total number of lines
   nTxtHeight   := GetTxtHeight(,nFontSize, cFont )   // determine the height of the font
   nVirtHeight  := nTxtHeight * nLineAll              // form scrolling height
   nVirtHeight  := IIF( nVirtHeight <= nMaxHeight, nMaxHeight + 10, nVirtHeight ) 

   DEFINE WINDOW Win_Font                      ;
      AT 0,0 WIDTH nMaxWidth HEIGHT nMaxHeight ;
      VIRTUAL HEIGHT nVirtHeight               ;
      TITLE "Fonts table !"                    ;
      ICON "1hmg_ico"                          ;
      MODAL                                    ;
      NOSIZE                                   ;
      BACKCOLOR SILVER                         ;
      FONT cFontName SIZE nFontSize            ;

      // do reassignment, now it's the internal dimensions of the window
      nMaxWidth  := This.ClientWidth   
      nMaxHeight := This.ClientHeight

      nRow := 0
      nCol := 0
      FOR nJ := 1 TO LEN(aFont)

         nCol := 0
         cFont  := aFont[ nJ ]
         cTitle := "Font table " + cFont + " - size " + HB_NtoS(nFontSize)
         nTxtHeight := GetTxtHeight(,nFontSize,cFontName)      // determine the height of the font
         cObj := "Label_Title" + HB_NtoS(nJ)
         nRow := nRow + nTxtHeight*2

         @ nRow - nTxtHeight, nCol LABEL &cObj WIDTH nMaxWidth HEIGHT nTxtHeight ;
           VALUE cTitle FONTCOLOR WHITE TRANSPARENT CENTERALIGN 

         nTxtHeight := GetTxtHeight(,nFontSize,cFont)      // determine the height of the font
         nTxtWidth  := GetTxtWidth(,nFontSize,cFont)       // determine the width of the text

         FOR nI := 0 TO 256

            cObj := "Label_" + HB_NtoS(nJ) + HB_NtoS(nI)
            @ nRow, nCol LABEL &cObj WIDTH nTxtWidth HEIGHT nTxtHeight ;
              TOOLTIP " CHR(" + HB_NtoS(nI) + ") "                     ;
              FONT cFont SIZE nFontSize                                ;
              VALUE CHR(nI) FONTCOLOR BLACK TRANSPARENT BORDER

            nCol := nCol + nTxtWidth
            IF nCol > nMaxWidth - nTxtWidth
               nRow += nTxtHeight
               nCol := 0
            ENDIF

            IF nI % 18 == 0
               DO EVENTS
            ENDIF

         NEXT
      NEXT

      hWnd := GetFormHandle(ThisWindow.Name) 
      ON KEY PRIOR ACTION SendMessage( hWnd, WM_VSCROLL, SB_PAGEUP, 0 )
      ON KEY NEXT ACTION SendMessage( hWnd, WM_VSCROLL, SB_PAGEDOWN, 0 )
      ON KEY UP ACTION SendMessage( hWnd, WM_VSCROLL, SB_LINEUP, 0 )
      ON KEY DOWN ACTION SendMessage( hWnd, WM_VSCROLL, SB_LINEDOWN, 0 )
	
      ON KEY ESCAPE ACTION ThisWindow.Release   // close the window by ESC

   END WINDOW

   CENTER   WINDOW Win_Font
   ACTIVATE WINDOW Win_Font
   
RETURN NIL

///////////////////////////////////////////////////////////////////////////////
FUNCTION GetStatusBarHeight( cForm ) // Height StatusBar
   DEFAULT cForm := ThisWindow.Name
RETURN GetWindowHeight(GetControlHandle('STATUSBAR', cForm))

