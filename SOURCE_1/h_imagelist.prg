/*----------------------------------------------------------------------------
MINIGUI - Harbour Win32 GUI library source code

Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
http://harbourminigui.googlepages.com/

IMAGELIST control source code
(C)2005 Janusz Pora <januszpora@onet.eu>

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
   Copyright 2001 Alexander S.Kresin <alex@kresin.ru>
   Copyright 2001 Antonio Linares <alinares@fivetech.com>
   www - https://harbour.github.io/

   "Harbour Project"
   Copyright 1999-2026, https://harbour.github.io/

   "WHAT32"
   Copyright 2002 AJ Wos <andrwos@aust1.net>

   "HWGUI"
   Copyright 2001-2021 Alexander S.Kresin <alex@kresin.ru>

---------------------------------------------------------------------------*/

#include "minigui.ch"

#define MAX_IMAGE 10

/*--------------------------------------------------------------------*
   Helper: RGB normalization
*--------------------------------------------------------------------*/
STATIC FUNCTION _NormRGB( aColor )
   IF IsArrayRGB( aColor )
      RETURN RGB( aColor[1], aColor[2], aColor[3] )
   ENDIF
RETURN aColor

/*--------------------------------------------------------------------*
   Define ImageList control
*--------------------------------------------------------------------*/
FUNCTION _DefineImageList ( ControlName , ParentForm , w , h , aImage , ;
                            aImageMask , aColor , ImageCount , mask )

   LOCAL i, k, id, mVar, kCtrl
   LOCAL controlHandles, maskImage, colorVal, posImage

   hb_default( @w, 24 )
   hb_default( @h, 24 )
   __defaultNIL( @aImage, {} )
   __defaultNIL( @aImageMask, {} )
   __defaultNIL( @aColor, { 0, 0, 0 } )
   hb_default( @ImageCount, 0 )
   hb_default( @mask, .F. )

   IF _HMG_BeginWindowActive
      ParentForm := _HMG_ActiveFormName
   ENDIF

   IF !_IsWindowDefined( ParentForm )
      MsgMiniGuiError( "Window: " + ParentForm + " is not defined." )
   ENDIF

   IF _IsControlDefined( ControlName, ParentForm )
      MsgMiniGuiError( "Control: " + ControlName + " Of " + ParentForm + " Already defined." )
   ENDIF

   mVar := "_" + ParentForm + "_" + ControlName

   k := Len( aImage )
   IF ImageCount == 0
      ImageCount := IFEMPTY( k, MAX_IMAGE, k )
   ENDIF

   id := _GetId()
   controlHandles := InitImageList( w, h, mask, ImageCount )

   kCtrl := _GetControlFree()

#ifdef _NAMES_LIST_
   _SetNameList( mVar, kCtrl )
#else
   PUBLIC &mVar. := kCtrl
#endif

   /*---------------- Control registration (core metadata) ----------------*/
   _HMG_aControlType[kCtrl]              := "IMAGELIST"
   _HMG_aControlNames[kCtrl]             := ControlName
   _HMG_aControlHandles[kCtrl]           := controlHandles
   _HMG_aControlParenthandles[kCtrl]     := GetFormHandle( ParentForm )
   _HMG_aControlIds[kCtrl]               := id
   _HMG_aControlProcedures[kCtrl]        := ""
   _HMG_aControlPageMap[kCtrl]           := {}
   _HMG_aControlValue[kCtrl]             := ImageCount
   _HMG_aControlInputMask[kCtrl]         := ""
   _HMG_aControllostFocusProcedure[kCtrl]:= ""
   _HMG_aControlGotFocusProcedure[kCtrl] := ""
   _HMG_aControlChangeProcedure[kCtrl]   := ""
   _HMG_aControlDeleted[kCtrl]           := .F.
   _HMG_aControlBkColor[kCtrl]           := Nil
   _HMG_aControlFontColor[kCtrl]         := Nil
   _HMG_aControlDblClick[kCtrl]          := ""
   _HMG_aControlHeadClick[kCtrl]         := {}
   _HMG_aControlRow[kCtrl]               := 0
   _HMG_aControlCol[kCtrl]               := 0
   _HMG_aControlWidth[kCtrl]             := w
   _HMG_aControlHeight[kCtrl]            := h
   _HMG_aControlSpacing[kCtrl]           := 0
   _HMG_aControlContainerRow[kCtrl]      := IIF( _HMG_FrameLevel > 0, _HMG_ActiveFrameRow[_HMG_FrameLevel], -1 )
   _HMG_aControlContainerCol[kCtrl]      := IIF( _HMG_FrameLevel > 0, _HMG_ActiveFrameCol[_HMG_FrameLevel], -1 )
   _HMG_aControlPicture[kCtrl]           := ""
   _HMG_aControlContainerHandle[kCtrl]   := 0
   _HMG_aControlFontName[kCtrl]          := ""
   _HMG_aControlFontSize[kCtrl]          := 0
   _HMG_aControlFontAttributes[kCtrl]    := { .F., .F., .F., .F. }
   _HMG_aControlToolTip[kCtrl]           := ""
   _HMG_aControlRangeMin[kCtrl]          := 0
   _HMG_aControlRangeMax[kCtrl]          := 0
   _HMG_aControlCaption[kCtrl]           := ""
   _HMG_aControlVisible[kCtrl]           := .T.
   _HMG_aControlHelpId[kCtrl]            := 0
   _HMG_aControlFontHandle[kCtrl]        := 0
   _HMG_aControlBrushHandle[kCtrl]       := 0
   _HMG_aControlEnabled[kCtrl]           := .T.
   _HMG_aControlMiscData1[kCtrl]         := 0
   _HMG_aControlMiscData2[kCtrl]         := ""

   IF _HMG_lOOPEnabled
      Eval( _HMG_bOnControlInit, kCtrl, mVar )
   ENDIF

   /*---------------- Image population ----------------*/
   FOR i := 1 TO Len( aImage )

      IF mask

         IF Len( aImageMask ) > 0
            maskImage := IIF( i <= Len( aImageMask ), aImageMask[i], "" )
            posImage := IL_Add( controlHandles, aImage[i], maskImage, w, h, ImageCount )
         ELSE
            colorVal := _NormRGB( aColor )
            posImage := IL_AddMasked( controlHandles, aImage[i], colorVal, w, h, ImageCount )
         ENDIF

      ELSE
         posImage := IL_Add( controlHandles, aImage[i], "", w, h, ImageCount )
      ENDIF

      IF posImage == -1
         MsgMiniGuiError( "Image: " + aImage[i] + " is not added. Check image size." )
      ENDIF

   NEXT

RETURN Nil


/*--------------------------------------------------------------------*
   Add image (normal)
*--------------------------------------------------------------------*/
FUNCTION _AddImageToImageList ( ControlName, ParentControl, Image, MaskImage )

   LOCAL w, h, c

   w := _GetControlWidth( ControlName, ParentControl )
   h := _GetControlHeight( ControlName, ParentControl )
   c := GetControlHandle( ControlName, ParentControl )

RETURN IL_Add( c, Image, hb_defaultValue( MaskImage, "" ), w, h )


/*--------------------------------------------------------------------*
   Add masked image
*--------------------------------------------------------------------*/
FUNCTION _AddImageMaskedToImageList ( ControlName, ParentControl, Image, aColor )

   LOCAL w, h, c

   w := _GetControlWidth( ControlName, ParentControl )
   h := _GetControlHeight( ControlName, ParentControl )
   c := GetControlHandle( ControlName, ParentControl )

RETURN IL_AddMasked( c, Image, _NormRGB( aColor ), w, h )


/*--------------------------------------------------------------------*
   Set background color
*--------------------------------------------------------------------*/
FUNCTION _ImageListSetBkColor ( ControlName, ParentControl, aColor )

   LOCAL c

   c := GetControlHandle( ControlName, ParentControl )

RETURN IL_SetBkColor( c, _NormRGB( aColor ) )


/*--------------------------------------------------------------------*
   Erase image region
*--------------------------------------------------------------------*/
FUNCTION _EraseImage ( ControlName, ParentControl, ix, iy )

   LOCAL w, h

   w := _GetControlWidth( ControlName, ParentControl )
   h := _GetControlHeight( ControlName, ParentControl )

RETURN IL_EraseImage( GetFormHandle( ParentControl ), ix, iy, w, h )


/*--------------------------------------------------------------------*
   Begin drag operation
*--------------------------------------------------------------------*/
FUNCTION _BeginDragImage ( ControlName, ParentControl, imageindex, ix, iy )

   LOCAL c, h

   c := GetControlHandle( ControlName, ParentControl )
   h := GetFormHandle( ParentControl )

   _HMG_ActiveDragImageHandle := h
   IL_BeginDrag( h, c, imageindex, ix, iy )

RETURN Nil