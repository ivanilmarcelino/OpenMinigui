/*
 * Author: P.Chornyj <myorg63@mail.ru>
 *
 * A Quick & Easy guide to Microsoft Windows Icon Size
 * https://www.creativefreedom.co.uk/icon-designers-blog/windows-7-icon-sizes/
 */

ANNOUNCE RDDSYS

#include "minigui.ch"

///////////////////////////////////////////////////////////////////////////////
procedure main()

   local cIcon := 'myicons.dll', hIconFromDll, hIcon, nIcon := 0
   local aInfo, w, h

   if ! Empty( ExtractIconEx( cIcon, -1 ) )
      if IsVistaOrLater()
         hIconFromDll := ExtractIconEx( cIcon, nIcon, 256, 256 )[1]
      elseif IsWinXPorLater()
         hIconFromDll := ExtractIconEx( cIcon, nIcon, 128, 128 )[1]
      endif

      if ! Empty( hIconFromDll )
         hIcon := CopyIcon( hIconFromDll )
         DestroyIcon( hIconFromDll )
      endif

   endif

   if Empty( hIcon )
      quit
   endif

   aInfo := GetIconSize( hIcon )
   w := aInfo[ 1 ]
   h := aInfo[ 2 ]

   define window Form_Main ;
      clientarea w, h + GetMenuBarHeight() ;
      title 'Icon from Dll' ;
      main ;
      nomaximize nosize ;
      on release ;
      ( ;
         DestroyIcon( hIcon ) ;
      )

      define main menu
         define popup "&File" 
            menuitem "&Save multipages icon" action MsgInfo( ;
                iif( SaveIcon( "myicon.ico", cIcon, nIcon, { 256, 128, 48, 32, 24, 16 } ), ;
                "Success!", "Failure!" ), "Result" )
            separator
            menuitem "E&xit" action ThisWindow.Release
         end popup
      end menu
   end window

   draw icon in window Form_Main at 0, 0 hicon hIcon width w height h

   on key Escape of Form_Main action ThisWindow.Release

   Form_Main.Center()
   Form_Main.Activate()

return

///////////////////////////////////////////////////////////////////////////////
Function SaveIcon( cIconName, cIcon, nIcon, aSizes )

   local aIcons := {}, i
   local lRet

   For i = 1 To Len( aSizes )
         AAdd( aIcons, ExtractIconEx( cIcon, nIcon, aSizes[i], aSizes[i] )[1] )
         Do Events
   Next

   lRet := C_SaveHIconToFile( cIconName, aIcons )

   For i = 1 To Len( aIcons )
         DestroyIcon( aIcons[i] )
   Next

Return lRet
