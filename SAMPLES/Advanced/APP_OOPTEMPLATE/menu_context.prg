/*
 * MINIGUI - Harbour Win32 GUI library
 *
 * (c) 2019-2023 Verchenko Andrey <verchenkoag@gmail.com>
 * (c) 2019 Sergej Kiselev <bilance@bilance.lv>
 * Edit 23.09.20 / 04.11.21
*/

#include "minigui.ch"

///////////////////////////////////////////////////////////////////////////////////
FUNCTION Test_ContexMenu( nPos, lExit, cForm )
   LOCAL aDim, nChoice, xRet, cFunc, nBmpSize, nFSize, aFntExt, cBmpExit, aYX
   DEFAULT nPos := 2, cForm := ThisWindow.Name, lExit := .T.
   // 1 - Extend Dynamic Context Menu at Cursor
   // 2 - Extend Dynamic Context Menu at Position
   // 3 - Extend Dynamic Context Menu at Row Col

   aDim := {}                                // .T.-нет выбора  .F.-есть выбор
   AADD( aDim, { "bNone32" , "Нет выбора"        , .T.   , "MsgDebug", "Str1" , 1 } )
   AADD( aDim, {                                                                  } )
   AADD( aDim, { "bEye32"  , "Показать в окне"   , .F.   , "MsgDebug", "Str3" , 3 } )
   AADD( aDim, {                                                                  } )
   AADD( aDim, { "bClone32", "Копировать в буфер", .F.   , "MsgDebug", "Str4" , 4 } )
   AADD( aDim, { "bPaste32", "Вставить из буфера", .F.   , "MsgDebug", "Str5" , 5 } )

   SetThemes(2)    // тема "Office 2000 theme" в ContextMenu
   //SetThemes(3)  // тема "Dark theme" в ContextMenu

   nBmpSize := 48
   nFSize   := 18
   aFntExt  := { "DejaVu Sans Mono", "Comic Sans MS" }
   cBmpExit := "bRetrn32Blck"
   aYX      := {}  // свои координаты показа окна
   nChoice  := DynamicContextMenuExtend( cForm, aDim, nPos, nBmpSize, nFSize, lExit, aFntExt, aYX, cBmpExit )

   IF nChoice > 0
      cFunc  := aDim[nChoice,3] + "(" + HB_ValToExp(aDim[nChoice]) + ")"
      IF MyIsFunNoRun(cFunc)
         xRet := EVal( hb_MacroBlock( cFunc ), nChoice, aDim[nChoice] )
      ELSE
         xRet := NIL
      ENDIF
   ENDIF

RETURN xRet

///////////////////////////////////////////////////////////////////////////////////////////////////////
FUNCTION DynamicContextMenuExtend(cForm, aDim, nPos, nBmpSize, nFSize, lExit, aFontExt, aYX, cRes2)
   LOCAL Font1, Font2, nY, nX, nI, lMenuStyle, nMenuBitmap, cRes, hFont
   LOCAL nChoice, aMenu, cMenu, bAction, cName, cImg, lChk, lDis
   LOCAL hForm := GetFormHandle( cForm )
   LOCAL nH, nW, nS := nBmpSize
   DEFAULT aFontExt := {} , aYX := {} , cRes2 := ""

   IF HB_ISARRAY( nPos )
      nY   := nPos[1]
      nX   := nPos[2]
      nPos := 2
   ENDIF

   nChoice := -1
   IF aDim == NIL
      MsgDebug("Error ! Нет массива aDim !", ProcNL(1) )
      RETURN nChoice
   ENDIF

   IF LEN(aDim) == 0
      MsgDebug("Error ! Массив aDim == 0 !", ProcNL(1) )
      RETURN nChoice
   ENDIF

   lMenuStyle  := IsExtendedMenuStyleActive()     // menu style EXTENDED/STANDARD
   nMenuBitmap := GetMenuBitmapHeight()           // bmp height in context menu

   IF LEN(aFontExt) > 0
      DEFINE FONT Font_1dcm  FONTNAME aFontExt[1] SIZE nFSize
      DEFINE FONT Font_2dcm  FONTNAME aFontExt[2] SIZE nFSize BOLD
   ELSE
      //DEFINE FONT Font_1dcm  FONTNAME "Times New Roman" SIZE nFSize
      DEFINE FONT Font_1dcm  FONTNAME "DejaVu Sans Mono" SIZE nFSize
      DEFINE FONT Font_2dcm  FONTNAME "Comic Sans MS"    SIZE nFSize BOLD
   ENDIF

   Font1 := GetFontHandle( "Font_1dcm" )
   Font2 := GetFontHandle( "Font_2dcm" )

                              // set a new style for the context menu
   SET MENUSTYLE EXTENDED     // switch menu style to advanced
   SetMenuBitmapHeight( nS )  // set image size 48x48

   DEFINE CONTEXT MENU OF &cForm

      nI := nW := nH := 0
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
         lDis    := aMenu[3] //.F.
                                                              //     DISABLED
         // _DefineMenuItem ( "Help1" , , "HELP0" , "No_image1" , .F. , .T. ,, Font1,, .F., .F., , .F. )
         // _DefineMenuItem ( "Help2" , , "HELP1" , "No_image2" , .F. , .F. ,, Font1,, .F., .F., , .F. )
         hFont   := IIF( lDis, Font2, Font1 )

         //_DefineMenuItem ( "&View" , {|| View(1))} , , , .F. , .F. ,, hFont,, .F., .F. , "iFolder32", .F. )
         //_DefineMenuItem ( "&View" , {|| Nil     } , , "Tick32" , .F. , .F. ,, hFont,, .F., .F., , .F. )
         _DefineMenuItem( cMenu, bAction, cName, cImg, lChk, lDis, , hFont , , .F., .F. )

         nW := Max( nW, GetFontWidth("Font_2dcm", Len( cMenu ) + 5) )   // Width  menu
         nH += nS + 4                                                   // Height menu

      NEXT

      IF !Empty(lExit)
         IF nS == 32
            cRes := "bExit32"
         ELSEIF nS == 48
            cRes := "bExit48"
         ELSE
            cRes := "bExit64"
         ENDIF
         IF LEN(cRes2) > 0
            cRes := cRes2
         ENDIF
         nH += 4
         SEPARATOR
         MENUITEM  "Выход"  ACTION {|| nChoice := 0 } FONT Font1 IMAGE cRes
         nH += nS + 4
      ENDIF

      nW += nS + 4   // Width menu

   END MENU

   DO CASE
      CASE nPos == 1

      CASE nPos == 2
         If Empty(nY)      // center row
            nY := GetWindowRow( hForm ) + int( ( GetWindowHeight( hForm ) - nH - ;
                  GetTitleHeight() - GetMenuBarHeight() - GetBorderHeight() ) / 2 )
         EndIf
         If Empty(nX)      // center col
            nX := GetWindowCol( hForm ) + int( ( GetWindowWidth( hForm ) - nW ) / 2 ) + ;
                                                 GetBorderWidth()
         EndIf

      CASE nPos == 3
         If Len(aYX) > 0
            nY := aYX[1]
            nX := aYX[2]
         EndIf

   ENDCASE

   _ShowContextMenu(cForm, nY, nX, .F. ) ; InkeyGui(10)  // menu runs through the queue

   RELEASE FONT Font_1dcm           // font removal
   RELEASE FONT Font_2dcm           // font removal

? ProcNL(), "nPos=",nPos, nY, nX
   IF _IsWindowDefined( cForm )
      DEFINE CONTEXT MENU OF &cForm    // deleting menu after exiting
      END MENU
   ENDIF

   SetMenuBitmapHeight(nMenuBitmap) // bmp height in context menu   - return as it was

   _NewMenuStyle( lMenuStyle )      // menu style EXTENDED/STANDARD - return as it was

RETURN nChoice

//////////////////////////////////////////////////////////////////////////////////////
FUNCTION MyIsFunNoRun(cStr)
   LOCAL cMsg, cRun, lValue

   IF AT("(",cStr) > 0
      cRun := SUBSTR(cStr,1,AT("(",cStr)-1)
   ELSE
      cRun := cStr
   ENDIF

   IF !hb_IsFunction( cRun )
      cMsg := 'Функции: '+cRun+ "  нет в EXE-файле !;;"
      cMsg += 'Запуск: ' + cStr + ";;"
      cMsg += ProcNL( 0 ) + ";"
      cMsg += ProcNL( 1 ) + ";"
      cMsg += ProcNL( 2 ) + ";;"
      AlertStop( cMsg, "Ошибка запуска !" )
      lValue := .F.
   ELSE
      lValue := .T.
   ENDIF

   RETURN lValue

//////////////////////////////////////////////////////////////////////////////////////
/*  MiniGUI\SAMPLES\Advanced\MenuEx
*/
PROCEDURE SetThemes( type )
LOCAL aColors := GetMenuColors()

   DEFAULT type TO 0

   SWITCH type
   CASE 0
      aColors[ MNUCLR_MENUBARBACKGROUND1 ]  := GetSysColor( 15 )
      aColors[ MNUCLR_MENUBARBACKGROUND2 ]  := GetSysColor( 15 )
      aColors[ MNUCLR_MENUBARTEXT ]         := RGB(   0,   0,   0 )
      aColors[ MNUCLR_MENUBARSELECTEDTEXT ] := RGB(   0,   0,   0 )
      aColors[ MNUCLR_MENUBARGRAYEDTEXT ]   := RGB( 192, 192, 192 )
      aColors[ MNUCLR_MENUBARSELECTEDITEM1 ]:= RGB( 255, 252, 248 )
      aColors[ MNUCLR_MENUBARSELECTEDITEM2 ]:= RGB( 136, 133, 116 )

      aColors[ MNUCLR_MENUITEMTEXT ]        := RGB(   0,   0,   0 )
      aColors[ MNUCLR_MENUITEMSELECTEDTEXT ]:= RGB(   0,   0,   0 )
      aColors[ MNUCLR_MENUITEMGRAYEDTEXT ]  := RGB( 192, 192, 192 )

      aColors[ MNUCLR_MENUITEMBACKGROUND1 ] := RGB( 255, 255, 255 )
      aColors[ MNUCLR_MENUITEMBACKGROUND2 ] := RGB( 255, 255, 255 )

      aColors[ MNUCLR_MENUITEMSELECTEDBACKGROUND1 ] := RGB( 182, 189, 210 )
      aColors[ MNUCLR_MENUITEMSELECTEDBACKGROUND2 ] := RGB( 182, 189, 210 )
      aColors[ MNUCLR_MENUITEMGRAYEDBACKGROUND1 ]   := RGB( 255, 255, 255 )
      aColors[ MNUCLR_MENUITEMGRAYEDBACKGROUND2 ]   := RGB( 255, 255, 255 )

      aColors[ MNUCLR_IMAGEBACKGROUND1 ] := RGB( 246, 245, 244 )
      aColors[ MNUCLR_IMAGEBACKGROUND2 ] := RGB( 207, 210, 200 )

      aColors[ MNUCLR_SEPARATOR1 ] := RGB( 168, 169, 163 )
      aColors[ MNUCLR_SEPARATOR2 ] := RGB( 255, 255, 255 )

      aColors[ MNUCLR_SELECTEDITEMBORDER1 ] := RGB(  10, 36, 106 )
      aColors[ MNUCLR_SELECTEDITEMBORDER2 ] := RGB(  10, 36, 106 )
      aColors[ MNUCLR_SELECTEDITEMBORDER3 ] := RGB(  10, 36, 106 )
      aColors[ MNUCLR_SELECTEDITEMBORDER4 ] := RGB(  10, 36, 106 )

      SET MENUCURSOR FULL

      SET MENUSEPARATOR SINGLE RIGHTALIGN

      SET MENUITEM BORDER 3DSTYLE

      EXIT

   CASE 1
      aColors[ MNUCLR_MENUBARBACKGROUND1 ]  := GetSysColor( 15 )
      aColors[ MNUCLR_MENUBARBACKGROUND2 ]  := GetSysColor( 15 )
      aColors[ MNUCLR_MENUBARTEXT ]         := GetSysColor(  7 )
      aColors[ MNUCLR_MENUBARSELECTEDTEXT ] := GetSysColor( 14 )
      aColors[ MNUCLR_MENUBARGRAYEDTEXT ]   := GetSysColor( 17 )
      aColors[ MNUCLR_MENUBARSELECTEDITEM1 ]:= GetSysColor( 13 )
      aColors[ MNUCLR_MENUBARSELECTEDITEM2 ]:= GetSysColor( 13 )

      aColors[ MNUCLR_MENUITEMTEXT ]        := GetSysColor(  7 )
      aColors[ MNUCLR_MENUITEMSELECTEDTEXT ]:= GetSysColor( 14 )
      aColors[ MNUCLR_MENUITEMGRAYEDTEXT ]  := GetSysColor( 17 )

      aColors[ MNUCLR_MENUITEMBACKGROUND1 ] := IF( _HMG_IsXP, GetSysColor( 4 ), RGB( 255, 255, 255 ) )
      aColors[ MNUCLR_MENUITEMBACKGROUND2 ] := IF( _HMG_IsXP, GetSysColor( 4 ), RGB( 255, 255, 255 ) )

      aColors[ MNUCLR_MENUITEMSELECTEDBACKGROUND1 ] := GetSysColor( 13 )
      aColors[ MNUCLR_MENUITEMSELECTEDBACKGROUND2 ] := GetSysColor( 13 )
      aColors[ MNUCLR_MENUITEMGRAYEDBACKGROUND1 ]   := IF( _HMG_IsXP, GetSysColor( 4 ), RGB( 255, 255, 255 ) )
      aColors[ MNUCLR_MENUITEMGRAYEDBACKGROUND2 ]   := IF( _HMG_IsXP, GetSysColor( 4 ), RGB( 255, 255, 255 ) )

      aColors[ MNUCLR_IMAGEBACKGROUND1 ] := GetSysColor( 15 )
      aColors[ MNUCLR_IMAGEBACKGROUND2 ] := GetSysColor( 15 )

      aColors[ MNUCLR_SEPARATOR1 ] := GetSysColor( 17 )
      aColors[ MNUCLR_SEPARATOR2 ] := GetSysColor( 14 )

      aColors[ MNUCLR_SELECTEDITEMBORDER1 ] := GetSysColor( 13 )
      aColors[ MNUCLR_SELECTEDITEMBORDER2 ] := GetSysColor( 13 )
      aColors[ MNUCLR_SELECTEDITEMBORDER3 ] := GetSysColor( 17 )
      aColors[ MNUCLR_SELECTEDITEMBORDER4 ] := GetSysColor( 14 )

      SET MENUCURSOR FULL

      SET MENUSEPARATOR DOUBLE RIGHTALIGN

      SET MENUITEM BORDER FLAT

      EXIT

   CASE 2
      aColors[ MNUCLR_MENUBARBACKGROUND1 ]  := GetSysColor( 15 )
      aColors[ MNUCLR_MENUBARBACKGROUND2 ]  := GetSysColor( 15 )
      aColors[ MNUCLR_MENUBARTEXT ]         := RGB(   0,   0,   0 )
      aColors[ MNUCLR_MENUBARSELECTEDTEXT ] := RGB(   0,   0,   0 )
      aColors[ MNUCLR_MENUBARGRAYEDTEXT ]   := RGB( 128, 128, 128 )
      aColors[ MNUCLR_MENUBARSELECTEDITEM1 ]:= GetSysColor(15)
      aColors[ MNUCLR_MENUBARSELECTEDITEM2 ]:= GetSysColor(15)

      aColors[ MNUCLR_MENUITEMTEXT ]        := RGB(   0,   0,   0 )
      aColors[ MNUCLR_MENUITEMSELECTEDTEXT ]:= RGB( 255, 255, 255 )
      aColors[ MNUCLR_MENUITEMGRAYEDTEXT ]  := RGB( 128, 128, 128 )

      aColors[ MNUCLR_MENUITEMBACKGROUND1 ] := RGB( 212, 208, 200 )
      aColors[ MNUCLR_MENUITEMBACKGROUND2 ] := RGB( 212, 208, 200 )

      aColors[ MNUCLR_MENUITEMSELECTEDBACKGROUND1 ] := RGB(  10,  36, 106 )
      aColors[ MNUCLR_MENUITEMSELECTEDBACKGROUND2 ] := RGB(  10,  36, 106 )
      aColors[ MNUCLR_MENUITEMGRAYEDBACKGROUND1 ]   := RGB( 212, 208, 200 )
      aColors[ MNUCLR_MENUITEMGRAYEDBACKGROUND2 ]   := RGB( 212, 208, 200 )

      aColors[ MNUCLR_IMAGEBACKGROUND1 ] := RGB( 212, 208, 200 )
      aColors[ MNUCLR_IMAGEBACKGROUND2 ] := RGB( 212, 208, 200 )

      aColors[ MNUCLR_SEPARATOR1 ] := RGB( 128, 128, 128 )
      aColors[ MNUCLR_SEPARATOR2 ] := RGB( 255, 255, 255 )

      aColors[ MNUCLR_SELECTEDITEMBORDER1 ] := RGB(  10,  36, 106 )
      aColors[ MNUCLR_SELECTEDITEMBORDER2 ] := RGB( 128, 128, 128 )
      aColors[ MNUCLR_SELECTEDITEMBORDER3 ] := RGB(  10,  36, 106 )
      aColors[ MNUCLR_SELECTEDITEMBORDER4 ] := RGB( 255, 255, 255 )

      SET MENUCURSOR SHORT
      SET MENUSEPARATOR DOUBLE LEFTALIGN
      SET MENUITEM BORDER 3D

      EXIT

   CASE 3
      aColors[ MNUCLR_MENUBARBACKGROUND1 ]  := RGB( 40, 40, 40 )
      aColors[ MNUCLR_MENUBARBACKGROUND2 ]  := RGB( 40, 40, 40 )
      aColors[ MNUCLR_MENUBARTEXT ]         := RGB( 215, 215, 215 )
      aColors[ MNUCLR_MENUBARSELECTEDTEXT ] := RGB( 255, 255, 255 )
      aColors[ MNUCLR_MENUBARGRAYEDTEXT ]   := RGB( 120, 120, 120 )
      aColors[ MNUCLR_MENUBARSELECTEDITEM1 ]:= RGB( 90, 90, 90 )
      aColors[ MNUCLR_MENUBARSELECTEDITEM2 ]:= RGB( 90, 90, 90 )

      aColors[ MNUCLR_MENUITEMTEXT ]        := RGB( 215, 215, 215 )
      aColors[ MNUCLR_MENUITEMSELECTEDTEXT ]:= RGB( 255, 255, 255 )
      aColors[ MNUCLR_MENUITEMGRAYEDTEXT ]  := RGB( 120, 120, 120 )

      aColors[ MNUCLR_MENUITEMBACKGROUND1 ] := RGB( 40, 40, 40 )
      aColors[ MNUCLR_MENUITEMBACKGROUND2 ] := RGB( 40, 40, 40 )

      aColors[ MNUCLR_MENUITEMSELECTEDBACKGROUND1 ] := RGB( 90, 90, 90 )
      aColors[ MNUCLR_MENUITEMSELECTEDBACKGROUND2 ] := RGB( 90, 90, 90 )
      aColors[ MNUCLR_MENUITEMGRAYEDBACKGROUND1 ]   := RGB( 40, 40, 40 )
      aColors[ MNUCLR_MENUITEMGRAYEDBACKGROUND2 ]   := RGB( 40, 40, 40 )

      aColors[ MNUCLR_IMAGEBACKGROUND1 ] := RGB( 40, 40, 40 )
      aColors[ MNUCLR_IMAGEBACKGROUND2 ] := RGB( 40, 40, 40 )

      aColors[ MNUCLR_SEPARATOR1 ] := RGB( 75, 75, 75 )
      aColors[ MNUCLR_SEPARATOR2 ] := RGB( 90, 90, 90 )

      aColors[ MNUCLR_SELECTEDITEMBORDER1 ] := RGB( 75, 75, 75 )
      aColors[ MNUCLR_SELECTEDITEMBORDER2 ] := RGB( 120, 120, 120 )
      aColors[ MNUCLR_SELECTEDITEMBORDER3 ] := RGB( 75, 75, 75 )
      aColors[ MNUCLR_SELECTEDITEMBORDER4 ] := RGB( 215, 215, 215 )

      SET MENUCURSOR FULL

      SET MENUSEPARATOR SINGLE RIGHTALIGN

      SET MENUITEM BORDER FLAT

   END

   SetMenuColors( aColors )

   SetColorMenu( aColors[ MNUCLR_MENUBARBACKGROUND1 ] )

RETURN

/*
*/
STATIC PROCEDURE SetColorMenu( nColor, lSubMenu )

LOCAL aColor := { GetRed( nColor ),;
      GetGreen( nColor ),;
      GetBlue( nColor ) }

   _ColorMenu( App.Handle, aColor, lSubMenu )

RETURN
