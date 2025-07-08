/*
 * MINIGUI - Harbour Win32 GUI library
 *
 * (c) 2019 Sergej Kiselev <bilance@bilance.lv>
 * (c) 2019 Verchenko Andrey <verchenkoag@gmail.com>
 * Edit - 06.07.24
*/

#include "minigui.ch"

/////////////////////////////////////////////////////////////////////////////
FUNCTION Test_ContexMenu( nPos, lExit, cForm )
   LOCAL aDim, nChoice, xRet, cFunc, nBmpSize, nFSize, aFontExt, cTypeRes
   DEFAULT nPos := 2, cForm := ThisWindow.Name, lExit := .T.
   // 1 - Extend Dynamic Context Menu at Cursor
   // 2 - Extend Dynamic Context Menu at Position
   // 3 - Extend Dynamic Context Menu at Row Col
   // {nY,nX} - Position y/x

   aDim := {}                                                // .T.
   AADD( aDim, {"FLAG_RU.bmp/ico"  , "Test menu - Russian     ", .F. , "MsgDebug", "Stroka1" , 1 } )
   AADD( aDim, {                                                                                 } )
   AADD( aDim, {"FLAG_UK.bmp/ico"  , "Test menu - Ukrainian   ", .F. , "MsgDebug", "Stroka2" , 2 } )
   AADD( aDim, {"SEPARATOR"        , "SEPARATOR               ",     , ""        , ""        ,   } )
   AADD( aDim, {"FLAG_Bel.bmp/ico" , "Test menu - Byelorussian", .F. , "MsgDebug", "Stroka3" , 3 } )
   AADD( aDim, {                   ,                           ,     ,           ,           ,   } )
   AADD( aDim, {"FLAG_Kaz.bmp/ico" , "Test menu - Kazakh      ", .F. , "MsgDebug", "Stroka4" , 4 } )

   cTypeRes := "ICO" // "BMP"
   nBmpSize := 48
   nFSize   := _HMG_DefaultFontSize + 10
   aFontExt := { "DejaVu Sans Mono", "Comic Sans MS" }
   nChoice  := DynamicContextMenuExtend( cForm, aDim, nPos, nBmpSize, nFSize, lExit, aFontExt, cTypeRes )

   IF nChoice > 0
      cFunc  := aDim[nChoice,3] + "(" + HB_ValToExp(aDim[nChoice]) + ")"
      IF MyIsFunNoRun(cFunc)
         xRet := EVal( hb_MacroBlock( cFunc ), nChoice, aDim[nChoice] )
      ELSE
         xRet := NIL
      ENDIF
   ENDIF

RETURN xRet

///////////////////////////////////////////////////////////////////////////////////////////////////
FUNCTION DynamicContextMenuExtend( cForm, aDim, nPos, nBmpSize, nFSize, lExit, aFontExt, cType )
   LOCAL Font1, Font2, nY, nX, nI, lMenuStyle, nMenuBitmap, cRes, hFont
   LOCAL nChoice, aMenu, cMenu, bAction, cName, cImg, lChk, lDis, lIcon
   LOCAL lFontNew, hForm := GetFormHandle( cForm )
   LOCAL nH, nW, nS := nBmpSize
   DEFAULT aFontExt := {}, cType := "ICON"

   IF HB_ISARRAY( nPos )
      nY   := nPos[1]
      nX   := nPos[2]
      nPos := 2
   ENDIF

   nChoice := -1
   IF aDim == NIL
      MsgDebug("Error ! No array aDim !", PROCNL(1) )
      RETURN nChoice
   ENDIF

   IF LEN(aDim) == 0
      MsgDebug("Error ! Array aDim == 0 !", PROCNL(1) )
      RETURN nChoice
   ENDIF

   lMenuStyle  := IsExtendedMenuStyleActive()     // menu style EXTENDED/STANDARD
   nMenuBitmap := GetMenuBitmapHeight()           // bmp height in context menu

   lFontNew := .T.
   IF LEN(aFontExt) > 0
      IF IsString(aFontExt[1])
         DEFINE FONT Font_1dcm  FONTNAME aFontExt[1] SIZE nFSize
         DEFINE FONT Font_2dcm  FONTNAME aFontExt[2] SIZE nFSize BOLD
      ELSE
         lFontNew := .F.
         Font1    := aFontExt[1]
         Font2    := aFontExt[2]
      ENDIF
   ELSE
      DEFINE FONT Font_1dcm  FONTNAME "Times New Roman" SIZE nFSize
      DEFINE FONT Font_2dcm  FONTNAME "Comic Sans MS"   SIZE nFSize BOLD
   ENDIF

   IF lFontNew
      Font1 := GetFontHandle( "Font_1dcm" )
      Font2 := GetFontHandle( "Font_2dcm" )
   ENDIF
   lIcon := IIF( "ICO" $ UPPER(cType), .T., .F. )

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

         //_DefineMenuItem( cMenu, bAction, cName, cImg, lChk, lDis, , hFont , , .F., .F. )
         IF lIcon
            _DefineMenuItem( cMenu, bAction, cName,     , lChk, lDis, , hFont , , .F., .F. , cImg, .F. )
         ELSE
            _DefineMenuItem( cMenu, bAction, cName, cImg, lChk, lDis, , hFont , , .F., .F. )
         ENDIF

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

   ENDCASE

   _ShowContextMenu(cForm, nY, nX, .F. ) ; InkeyGui(10)  // menu runs through the queue

   IF lFontNew
      RELEASE FONT Font_1dcm           // font removal
      RELEASE FONT Font_2dcm           // font removal
   ENDIF

   DEFINE CONTEXT MENU OF &cForm    // deleting menu after exiting
   END MENU

   SetMenuBitmapHeight(nMenuBitmap) // bmp height in context menu   - return as it was

   _NewMenuStyle( lMenuStyle )      // menu style EXTENDED/STANDARD - return as it was

RETURN nChoice

//////////////////////////////////////////////////////////////////////////////////////
FUNCTION MyIsFunNoRun(cStr)
   LOCAL cMsg, cRun, lValue, cTtl

   IF AT("(",cStr) > 0
      cRun := SUBSTR(cStr,1,AT("(",cStr)-1)
   ELSE
      cRun := cStr
   ENDIF

   IF App.Cargo:cLang == "RU"
      cTtl := "Ошибка запуска !"
      cMsg := 'Функции: '+ cRun + "() нет в EXE-файле !;;"
   ELSE
      cTtl := "Startup error !"
      cMsg := 'Functions: ' + cRun + "() not in EXE file!;;"
   ENDIF

   IF !hb_IsFunction( cRun )
      cMsg += ProcNL( 0 ) + ";" + ProcNL( 1 ) + ";"
      AlertStop( cMsg, cTtl, "ZZZ_B_STOP64", 64, {RED}, .T. , , .F.  )
      lValue := .F.
   ELSE
      lValue := .T.
   ENDIF

RETURN lValue

////////////////////////////////////////////////////////////////
FUNCTION myContextMenu(aMenu, nY2, nX2, cType)
   LOCAL Font1, Font2, Font3, cForm, nY, nX
   LOCAL oWnd, nI, nChoice, lIcon
   LOCAL cMenu, bAction, cName, cImg, lChk, lDis

   oWnd  := _WindowObj( GetActiveWindow() )  // окно в фокусе
   cForm := oWnd:Name
   Font1 := GetFontHandle( "ComSanMS" )
   Font2 := GetFontHandle( "Bold"     )
   Font3 := GetFontHandle( "ItalBold" )
   lIcon := IIF( "ICO" $ UPPER(cType), .T., .F. )

   // координаты вывода окна
   nY    := GetProperty(cForm, "Row") + GetTitleHeight()
   nY    += nY2 + 5
   nX    := GetProperty(cForm, "Col") + GetBorderWidth()
   nX    += nX2

   SET MENUSTYLE EXTENDED     // переключить стиль меню на расширенный
   SetMenuBitmapHeight( 32 )  // установить размер иконок 32х32

   nChoice := -2              // обязательно, первоначальное значение
   DEFINE CONTEXT MENU OF &cForm

       FOR nI := 1 TO LEN(aMenu)
          cMenu := aMenu[nI,2]
          IF LEN(cMenu) == 0
             SEPARATOR
          ELSE
             cImg    := IIF( LEN(aMenu[nI,1])==0, Nil, aMenu[nI,1] )
             cName   := StrZero(nI, 10)
             bAction := {|| nChoice := Val( This.Name ) }
             lChk    := .F.
             lDis    := .F.
             IF lIcon
                _DefineMenuItem( cMenu, bAction, cName,     , lChk, lDis, , Font1 , , .F., .F. , cImg, .F. )
             ELSE
                _DefineMenuItem( cMenu, bAction, cName, cImg, lChk, lDis, , Font1 , , .F., .F. )
             ENDIF
          ENDIF
       NEXT
       /*
       SEPARATOR
       nI      := LEN(aBmp)
       cMenu   := aItem[nI]
       cBmp    := aBmp[nI]
       cName   := StrZero(nI, 10)
       bAction := {|| nChoice := Val( This.Name ) }
       lChk    := .F.
       lDis    := .F.
       _DefineMenuItem( cMenu, bAction, cName, cBmp, lChk, lDis, , Font1 , , .F., .F. )

       SEPARATOR
       MENUITEM  "Exit"           ACTION  {|| nChoice := -1 } FONT Font3
       */
   END MENU

   _ShowContextMenu(cForm, nY, nX, .f. ) // ПОКАЗ ВЫПАДАЕЩЕГО МЕНЮ

   InkeyGui(10)  // menu работает через очередь !

   DEFINE CONTEXT MENU OF &cForm         // delete menu after exiting
   END MENU

   DO EVENTS

RETURN nChoice

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

   //SetProperty( "Form_1", "BackColor", aColors[ MNUCLR_MENUBARBACKGROUND1 ] )
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


#pragma BEGINDUMP

#include <mgdefs.h>
#include "commctrl.h"

HB_FUNC ( _COLORMENU )
{
   HMENU      iMenu;
   MENUINFO   iMenuInfo ;

   INT   nRed   = HB_PARNI(2,1);
   INT   nGreen = HB_PARNI(2,2);
   INT   nBlue  = HB_PARNI(2,3);
   BOOL  lSubMenu = HB_ISLOG(3) ? hb_parl(3) : FALSE;

   iMenu              = GetMenu( (HWND) HB_PARNL(1) );
   GetMenuInfo(iMenu, &iMenuInfo);
   iMenuInfo.cbSize   = sizeof(MENUINFO);
   if (lSubMenu)
   {
      iMenuInfo.fMask    = MIM_BACKGROUND | MIM_APPLYTOSUBMENUS;
   }
   else
   {
      iMenuInfo.fMask    = MIM_BACKGROUND;
   }

   iMenuInfo.hbrBack = CreateSolidBrush( RGB(nRed, nGreen, nBlue) );

   SetMenuInfo(iMenu, &iMenuInfo);

   DrawMenuBar((HWND) hb_parnl(1));
}

#pragma ENDDUMP
