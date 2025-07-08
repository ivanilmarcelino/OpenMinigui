/*
* MiniGUI Menu Demo
*/

#include "hmg.ch"

Procedure Main

   DEFINE WINDOW Win_1 ;
      AT 0,0 ;
      WIDTH 600 ;
      HEIGHT 600 ;
      TITLE 'Dynamic Context Menu Demo 2' ;
      Icon 'Demo.ico' ;
      MAIN

      DEFINE MAIN MENU
         POPUP "&Options-1"

            MENUITEM  'Dynamic Context Menu at Cursor '    ACTION DynamicContextMenu(1)   Image 'Check.Bmp'
            MENUITEM  'Dynamic Context Menu at Position '  ACTION DynamicContextMenu(2) Image 'Check.Bmp'

         END POPUP

         POPUP "&Options-2"

            MENUITEM  '2-Extend Dynamic Context Menu at Cursor '   ACTION DynamicContextMenuExtend(1) Image 'Check.Bmp'
            MENUITEM  '2-Extend Dynamic Context Menu at Position'  ACTION DynamicContextMenuExtend(2) Image 'Check.Bmp'

         END POPUP

         POPUP "&Options-3"

            MENUITEM  '3-Extend Dynamic Context Menu at Cursor '   ACTION Test_ContexMenu(1) Image 'Check.Bmp'
            MENUITEM  '3-Extend Dynamic Context Menu at Position'  ACTION Test_ContexMenu(2) Image 'Check.Bmp'
            MENUITEM  '3-Extend Dynamic Context Menu at Row Col '  ACTION Test_ContexMenu({App.Row+70, App.Col+50}) Image 'Check.Bmp'

         END POPUP

      END MENU

   END WINDOW

   CENTER WINDOW Win_1

   ACTIVATE WINDOW Win_1

Return


Procedure MenuProc()

   If This.Name == '01'
      MsgInfo ('Action 01')
   ElseIf This.Name == '02'
      MsgInfo ('Action 02')
   ElseIf This.Name == '03'
      MsgInfo ('Action 03')
   EndIf

RETURN


FUNCTION DynamicContextMenu(typ)
   LOCAL N
   LOCAL m_char

   DEFINE CONTEXT MENU OF Win_1

      FOR N = 1 TO 3
         m_char := strzero(n,2)
         MENUITEM  'Context Item ' + m_char ACTION MenuProc() NAME &m_char
      NEXT

   END MENU

   DO CASE
      CASE typ == 1
         SHOW CONTEXTMENU PARENT Win_1

      CASE typ == 2
         SetCursorPos( GetDesktopWidth()/2, GetDesktopHeight()/2 - 20 )
         SHOW CONTEXTMENU OF Win_1 AT GetDesktopHeight()/2 - 50, GetDesktopWidth()/2 - 80
   ENDCASE

   RELEASE CONTEXT MENU OF Win_1

RETURN Nil


FUNCTION DynamicContextMenuExtend(nVal)
   LOCAL aFlags := { "FLAG_RU.bmp", "FLAG_UK.bmp", "FLAG_Bel.bmp", "FLAG_Kaz.bmp" }
   LOCAL Font1, Font2, nY, nX, lMenuStyle, nMenuBitmap
   LOCAL nLang, cForm := ThisWindow.Name

   lMenuStyle  := IsExtendedMenuStyleActive()     // menu style EXTENDED/STANDARD
   nMenuBitmap := GetMenuBitmapHeight()           // bmp height in context menu

   IF ! _IsControlDefined ( "Font_1" , "Main" )
      DEFINE FONT Font_1  FONTNAME "Times New Roman" SIZE 16
      DEFINE FONT Font_2  FONTNAME "Comic Sans MS"   SIZE 16 BOLD
   ENDIF

   Font1 := GetFontHandle( "Font_1" )
   Font2 := GetFontHandle( "Font_2" )

   // set a new style for the context menu
   SET MENUSTYLE EXTENDED     // switch menu style to advanced
   SetMenuBitmapHeight( 32 )  // set image size 32x32

   nLang := -2   // initial value, selection outside the menu

   DEFINE CONTEXT MENU OF &cForm
       MENUITEM  "Russian"       ACTION  {|| nLang := 1 }  FONT Font2 IMAGE aFlags[1]
       MENUITEM  "Ukrainian"     ACTION  {|| nLang := 2 }  FONT Font2 IMAGE aFlags[2]
       MENUITEM  "Byelorussian"  ACTION  {|| nLang := 3 }  FONT Font2 IMAGE aFlags[3]
       MENUITEM  "Kazakh"        ACTION  {|| nLang := 4 }  FONT Font2 IMAGE aFlags[4]
       SEPARATOR
       MENUITEM  "Exit"          ACTION  {|| nLang := -1 } FONT Font1 IMAGE "exit.bmp"
   END MENU

   DO CASE
      CASE nVal == 1
         //SHOW CONTEXTMENU PARENT Win_1
         _ShowContextMenu( cForm )

      CASE nVal == 2
         //SetCursorPos( GetDesktopWidth()/2, GetDesktopHeight()/2 - 20 )
         //SHOW CONTEXTMENU OF Win_1 AT GetDesktopHeight()/2 - 50, GetDesktopWidth()/2 - 80
         nY := GetDesktopHeight()/2 - 50
         nX := GetDesktopWidth()/2 - 80
         _ShowContextMenu( cForm, nY, nX )

   ENDCASE

   InkeyGui(10)  // menu runs through the queue

   // font removal
   RELEASE FONT Font_1
   RELEASE FONT Font_2

   DEFINE CONTEXT MENU OF &cForm  // deleting menu after exiting 
   END MENU

   // bmp height in context menu - return as it was
   SetMenuBitmapHeight(nMenuBitmap)

   // menu style EXTENDED/STANDARD - return as it was
   IF lMenuStyle
      SET MENUSTYLE EXTENDED
   ELSE
      SET MENUSTYLE STANDARD
   ENDIF

   MsgDebug( nLang, IIF( nLang > 0, aFlags[nLang], "" ) )

RETURN Nil


FUNCTION Test_ContexMenu( nVal, lExit, cForm )
   LOCAL aDim := {}, nChoice, xRet, nBmpSize := 48, nFSize := 18

   DEFAULT nVal := 1, cForm := ThisWindow.Name, lExit := .T.

   AADD( aDim, {"FLAG_RU.bmp"  , "Test menu - Russian     ", "MsgDebug", "String1" , 1 } )
   AADD( aDim, {                                                                       } )
   AADD( aDim, {"FLAG_UK.bmp"  , "Test menu - Ukrainian   ", "MsgDebug", "String2" , 2 } )
   AADD( aDim, {"SEPARATOR"    , "SEPARATOR               ", ""        , ""        ,   } )
   AADD( aDim, {"FLAG_Bel.bmp" , "Test menu - Byelorussian", "MsgDebug", "String3" , 3 } )
   AADD( aDim, {               ,                           ,           ,           ,   } )
   AADD( aDim, {"FLAG_Kaz.bmp" , "Test menu - Kazakh      ", "MsgDebug", "String4" , 4 } )

   nChoice := DynamicContextMenuExtend3( cForm, aDim, nVal, nBmpSize, nFSize, lExit )

   IF nChoice > 0
      xRet := EVal( hb_MacroBlock( aDim[nChoice,3] + "(" + HB_ValToExp(aDim[nChoice]) + ")" ), nChoice, aDim[nChoice] )
   ENDIF

RETURN xRet

FUNCTION DynamicContextMenuExtend3( cForm, aDim, nVal, nBmpSize, nFSize, lExit )
   LOCAL Font1, Font2, nY, nX, nI, lMenuStyle, nMenuBitmap
   LOCAL nChoice, aMenu, cMenu, bAction, cName, cImg, lChk, lDis
   LOCAL hForm :=GetFormHandle( cForm )
   LOCAL nH, nW, nS := nBmpSize

   IF HB_ISARRAY( nVal )
      nY   := nVal[1]
      nX   := nVal[2]
      nVal := 2
   ENDIF

   lMenuStyle  := IsExtendedMenuStyleActive()     // menu style EXTENDED/STANDARD
   nMenuBitmap := GetMenuBitmapHeight()           // bmp height in context menu

   IF ! _IsControlDefined ( "Font_1dcm" , "Main" )
      DEFINE FONT Font_1dcm  FONTNAME "Times New Roman" SIZE nFSize
      DEFINE FONT Font_2dcm  FONTNAME "Comic Sans MS"   SIZE nFSize BOLD
   ENDIF

   Font1 := GetFontHandle( "Font_1dcm" )
   Font2 := GetFontHandle( "Font_2dcm" )

   // set a new style for the context menu
   SET MENUSTYLE EXTENDED     // switch menu style to advanced
   SetMenuBitmapHeight( nS )  // set image size 48x48

   DEFINE CONTEXT MENU OF &cForm

      nI := nChoice := nW := nH := 0
      FOR EACH aMenu IN aDim

         nI++
         IF Empty(aMenu) .or. aMenu[1] == NIL .or. Empty(aMenu[2]) .or. "SEPARATOR" $ aMenu[2]
            nH += 4
            SEPARATOR
            LOOP
         ENDIF

         cName   := StrZero(nI, 10)
         cImg    := aMenu[1]
         cMenu   := aMenu[2]
         bAction := {|| nChoice := Val( This.Name ) }
         lChk    := .F.
         lDis    := .F.

         _DefineMenuItem( cMenu, bAction, cName, cImg, lChk, lDis, , Font2 , , .F., .F. )

         nW := Max( nW, GetFontWidth("Font_2dcm", Len( cMenu ) + 5) )   // Width  menu
         nH += nS + 4                                                   // Height menu

      NEXT

      IF !Empty(lExit)
         nH += 4
         SEPARATOR
         MENUITEM  "Exit"  ACTION {|| nChoice := 0 } FONT Font1 IMAGE "exit.bmp"
         nH += nS + 4
      ENDIF

      nW += nS + 4   // Width menu

   END MENU

   DO CASE
      CASE nVal == 1

      CASE nVal == 2
         If Empty(nY)      // center row
            nY := GetWindowRow( hForm ) + int( ( GetWindowHeight( hForm ) - nH - ;
                  GetTitleHeight() - GetMenuBarHeight() - GetBorderHeight() ) / 2 )
         EndIf
         If Empty(nX)      // center col
            nX := GetWindowCol( hForm ) + int( ( GetWindowWidth( hForm ) - nW ) / 2 ) + ;
                                                 GetBorderWidth()
         EndIf

   ENDCASE

   _ShowContextMenu( cForm, nY, nX ) ; InkeyGui(10)  // menu runs through the queue

   RELEASE FONT Font_1dcm           // font removal
   RELEASE FONT Font_2dcm           // font removal

   DEFINE CONTEXT MENU OF &cForm    // deleting menu after exiting 
   END MENU

   SetMenuBitmapHeight(nMenuBitmap) // bmp height in context menu   - return as it was

   _NewMenuStyle( lMenuStyle )      // menu style EXTENDED/STANDARD - return as it was

RETURN nChoice
