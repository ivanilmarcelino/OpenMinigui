/*----------------------------------------------------------------------------
 MINIGUI - Harbour Win32 GUI library source code

 Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
 http://harbourminigui.googlepages.com/

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

 Parts of this code is contributed and used here under permission of his author:
 (C)2005 Janusz Pora <januszpora@onet.eu>
---------------------------------------------------------------------------*/

#include "minigui.ch"

#define NOFOUND        -1
#define NOMRUS          0

/* asMRU indexes */
#define cFileIni        asMRU[1]
#define cSectionIni     asMRU[2]
#define MRUParentForm   asMRU[3]
#define MRUCount        asMRU[4]
#define cMRU_Id         asMRU[5]
#define maxMRU_Files    asMRU[6]

/* aMRU_File indexes */
#define MRU_CAPTION     1
#define MRU_FILENAME    2
#define MRU_MENU_ID     3
#define MRU_ACTION      4
#define MRU_INDEX       5

STATIC asMRU := { Nil, Nil, Nil, Nil, Nil, Nil }
STATIC aMRU_File

*-----------------------------------------------------------------------------*
FUNCTION AddMRUItem( cNewItem, cAction )
*-----------------------------------------------------------------------------*
   LOCAL nDuplicate

   nDuplicate := CheckForDuplicateMRU( cNewItem )

   IF nDuplicate <> NOFOUND
      ReorderMRUList( nDuplicate )
   ENDIF

   IF nDuplicate <> 1 .OR. MRUCount == 0
      AddMenuElement( cNewItem, cAction )
   ENDIF

RETURN Nil

*-----------------------------------------------------------------------------*
STATIC FUNCTION CheckForDuplicateMRU( cNewItem )
*-----------------------------------------------------------------------------*
   LOCAL nDuplicate := NOFOUND
   LOCAL cCompare

   IF Empty( cNewItem )
      RETURN nDuplicate
   ENDIF

   cCompare := Upper( cNewItem )

   nDuplicate := AScan( ;
      aMRU_File, ;
      {|aItem| Upper( aItem[MRU_FILENAME] ) == cCompare } )

   IF nDuplicate == 0
      nDuplicate := NOFOUND
   ENDIF

RETURN nDuplicate

*-----------------------------------------------------------------------------*
STATIC FUNCTION BuildMRUCaption( cFileName )
*-----------------------------------------------------------------------------*
   LOCAL cCaption

   cCaption := iif( ;
      Len( cFileName ) < 40, ;
      cFileName, ;
      SubStr( cFileName, 1, 3 ) + "..." + ;
      SubStr( cFileName, Len( cFileName ) - 34 ) )

RETURN cCaption

*-----------------------------------------------------------------------------*
STATIC FUNCTION BuildMRUAction( cFileName, cAction )
*-----------------------------------------------------------------------------*
   LOCAL bAction
   LOCAL cProcName

   IF Empty( cAction )
      RETURN {|| Nil }
   ENDIF

   cProcName := Left( cAction, At( "(", cAction ) )

   bAction := &( ;
      '{|| ' + cProcName + ' "' + cFileName + '" ) }' )

RETURN bAction

*-----------------------------------------------------------------------------*
STATIC FUNCTION RefreshMRUMenuCaptions()
*-----------------------------------------------------------------------------*
   LOCAL n
   LOCAL cMenuId
   LOCAL cCaption

   FOR n := 1 TO Len( aMRU_File )

      cMenuId := aMRU_File[n, MRU_MENU_ID]

      cCaption := ;
         "&" + hb_ntos( n ) + " " + ;
         aMRU_File[n, MRU_CAPTION]

      _ModifyMenuItem( ;
         cMenuId, ;
         MRUParentForm, ;
         cCaption, ;
         aMRU_File[n, MRU_ACTION] )

   NEXT

RETURN Nil

*-----------------------------------------------------------------------------*
FUNCTION AddMenuElement( cNewItem, cAction )
*-----------------------------------------------------------------------------*
   LOCAL bAction
   LOCAL cCaption
   LOCAL cMenuId
   LOCAL cNewMenuId
   LOCAL n
   LOCAL nFreeIndex
   LOCAL nPos

   cCaption := BuildMRUCaption( cNewItem )
   bAction  := BuildMRUAction( cNewItem, cAction )

   IF MRUCount == 0

      cMenuId := cMRU_Id

      _ModifyMenuItem( ;
         cMenuId, ;
         MRUParentForm, ;
         "&1 " + cCaption, ;
         bAction )

      AAdd( aMRU_File, ;
         { cCaption, cNewItem, cMenuId, bAction, 1 } )

   ELSE

      nFreeIndex := 1

      FOR n := 1 TO Len( aMRU_File ) + 1

         nPos := AScan( ;
            aMRU_File, ;
            {|aItem| aItem[MRU_INDEX] == n } )

         IF nPos == 0
            nFreeIndex := n
            EXIT
         ENDIF

      NEXT

      cNewMenuId := cMRU_Id + "_" + hb_ntos( nFreeIndex )
      cMenuId    := aMRU_File[1, MRU_MENU_ID]

      _InsertMenuItem( ;
         cMenuId, ;
         MRUParentForm, ;
         "&1 " + cCaption, ;
         bAction, ;
         cNewMenuId )

      AIns( ;
         aMRU_File, ;
         1, ;
         { ;
            cCaption, ;
            cNewItem, ;
            cNewMenuId, ;
            bAction, ;
            nFreeIndex ;
         }, ;
         .T. )

      RefreshMRUMenuCaptions()

      IF Len( aMRU_File ) > maxMRU_Files

         cMenuId := aMRU_File[Len( aMRU_File ), MRU_MENU_ID]

         ASize( aMRU_File, maxMRU_Files )

         _RemoveMenuItem( cMenuId, MRUParentForm )

      ENDIF

   ENDIF

   MRUCount++

RETURN Nil

*-----------------------------------------------------------------------------*
STATIC FUNCTION ReorderMRUList( nDuplicateLocation )
*-----------------------------------------------------------------------------*
   LOCAL cMenuId

   IF nDuplicateLocation <= 1
      RETURN Nil
   ENDIF

   cMenuId := aMRU_File[nDuplicateLocation, MRU_MENU_ID]

   _RemoveMenuItem( cMenuId, MRUParentForm )

   hb_ADel( aMRU_File, nDuplicateLocation, .T. )

RETURN Nil

*-----------------------------------------------------------------------------*
FUNCTION SaveMRUFileList()
*-----------------------------------------------------------------------------*
   LOCAL cFile
   LOCAL n

   BEGIN INI FILE cFileIni

      FOR n := 1 TO maxMRU_Files

         cFile := iif( ;
            n <= Len( aMRU_File ), ;
            aMRU_File[n, MRU_FILENAME], ;
            "" )

         SET SECTION cSectionIni ;
            ENTRY hb_ntos( n ) ;
            TO cFile

      NEXT

   END INI

RETURN Nil

*-----------------------------------------------------------------------------*
FUNCTION _DefineMruItem( ;
   cCaption, ;
   cIniFile, ;
   cSection, ;
   nMaxItems, ;
   cAction, ;
   cName )
*-----------------------------------------------------------------------------*
   LOCAL aTmp := {}
   LOCAL cValue := ""
   LOCAL lExist := .F.
   LOCAL n

   DEFAULT ;
      cCaption  := " (Empty) ", ;
      nMaxItems := 10, ;
      cName     := "MRU", ;
      cIniFile  := "mru.ini", ;
      cSection  := "MRU"

   cFileIni      := cIniFile
   cSectionIni   := cSection
   MRUParentForm := _HMG_xMainMenuParentName
   MRUCount      := 0
   aMRU_File     := {}
   cMRU_Id       := cName
   maxMRU_Files  := nMaxItems

   BEGIN INI FILENAME cIniFile

      FOR n := 1 TO nMaxItems

         GET cValue ;
            SECTION cSection ;
            ENTRY hb_ntos( n ) ;
            DEFAULT ""

         IF Empty( cValue )
            EXIT
         ENDIF

         lExist := .T.

         AAdd( aTmp, cValue )

         IF n == 1
            MENUITEM cCaption NAME &cName
         ENDIF

      NEXT

   END INI

   IF lExist

      IF Empty( cAction )
         cAction := Nil
      ENDIF

      FOR EACH n IN aTmp DESCEND
         AddMRUItem( n, cAction )
      NEXT

   ELSE

      MENUITEM cCaption NAME &cName DISABLED

   ENDIF

RETURN Nil

*-----------------------------------------------------------------------------*
FUNCTION ClearMRUList()
*-----------------------------------------------------------------------------*
   LOCAL cMenuId
   LOCAL n

   FOR n := Len( aMRU_File ) TO 1 STEP -1

      cMenuId := aMRU_File[n, MRU_MENU_ID]

      IF n > 1

         _RemoveMenuItem( cMenuId, MRUParentForm )

      ELSE

         _ModifyMenuItem( ;
            cMenuId, ;
            MRUParentForm, ;
            " (Empty) ", ;
            {|| Nil } )

         SetProperty( ;
            MRUParentForm, ;
            cMenuId, ;
            "Enabled", ;
            .F. )

         cMRU_Id   := cMenuId
         aMRU_File := {}
         MRUCount  := 0

      ENDIF

   NEXT

RETURN Nil
