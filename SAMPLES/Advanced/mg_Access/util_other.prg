/*
 * MINIGUI - Harbour Win32 GUI library Demo
 * 07.07.24
 */
#define _HMG_OUTLOG

#include "hmg.ch"
#include "dbinfo.ch"

#xtranslate MiniGuiVersionChar()  => Substr( MiniGuiVersion(), At(".", MiniGuiVersion()) - 2, 8 )
#xtranslate MiniGuiVersionNumba() => Int( Val( MiniGuiVersionChar() ) * 10000 + Val( Right(MiniGuiVersionChar(), 2) ) )
////////////////////////////////////////////////////////////////////////
FUNCTION MGVersChar()
   RETURN MiniGuiVersionChar()

////////////////////////////////////////////////////////////////////////
FUNCTION MGVersNumba()
   RETURN MiniGuiVersionNumba()

////////////////////////////////////////////////////////////////
FUNCTION ProcNL(nVal, cMsg)
   Default cMsg := ">>> "
   nVal := iif( Empty(nVal), 0, nVal ) + 1
   cMsg += ProcName(nVal) + "(" + hb_ntos( ProcLine(nVal) ) + ")"
   cMsg += " => " + ProcFile(nVal)
RETURN cMsg

//////////////////////////////////////////////////////////////////
FUNCTION _Font2oDlu( cFontName )
   LOCAL i, f := _HMG_DefaultFontName
   LOCAL o, n := _HMG_DefaultFontSize

   IF !Empty( cFontName )
      IF ( i := GetControlIndex( cFontName, "Main" ) ) > 0
         f := _HMG_aControlFontName[ i ]
         n := _HMG_aControlFontSize[ i ]
      ENDIF
   ENDIF

   o := oDlu4Font( n )

   IF f == "Arial Black"
      i := iif( n < 15, 20, iif( n < 20, 30, 40 ) )
      o:nPixWidth    += i
      o:nPixWidthDT  += i
      o:nPixWidthDT1 += i
      o:nPixWidthDT2 += i
   ENDIF

RETURN o

* ======================================================================
* При наличии файла добавить число версии в имя
FUNCTION GetFileNameMaskNum( cFile ) //FileNameMaskNum( cFile )
   LOCAL i := 0, cPth, cFil, cExt

   If ! hb_FileExists(cFile); RETURN cFile
   EndIf

   hb_FNameSplit(cFile, @cPth, @cFil, @cExt)

   WHILE ( hb_FileExists( hb_FNameMerge(cPth, cFil + '(' + hb_ntos(++i) + ')', cExt) ) )
   END

   RETURN hb_FNameMerge(cPth, cFil + '(' + hb_ntos(i) + ')', cExt)

* =========================================================================
* При наличии файла добавить число версии в имя файла без расширения файла
FUNCTION GetFileNameMaskNumNotExt( cFile )
   LOCAL i := 0, cPth, cFil, cExt

   If ! hb_FileExists(cFile); RETURN cFile
   EndIf

   hb_FNameSplit(cFile, @cPth, @cFil, @cExt)

   WHILE ( hb_FileExists( hb_FNameMerge(cPth, cFil + '(' + hb_ntos(++i) + ')', cExt) ) )
   END

   RETURN hb_FNameMerge(cPth, cFil + '(' + hb_ntos(i) + ')', cExt)

*-----------------------------------------------------------------------------*
// Показ иконок ассоциации файлов / Icon associated with the file type

#pragma BEGINDUMP

#include "hbapi.h"
#include "windows.h"
#include <shellapi.h>

extern HBITMAP Icon2Bmp( HICON hIcon );

HB_FUNC( EXTRACTASSICON )
{
HICON hIcon;
LONG hInstance = hb_parnl( 1 );
char * lpIconPath = ( char * ) hb_parc( 2 );
WORD lpiIcon = hb_parnl( 3 );

hIcon = ExtractAssociatedIcon(
( HINSTANCE ) hInstance,
lpIconPath,
&lpiIcon );

hb_stornl( lpiIcon, 3 );
hb_retnl( ( LONG ) hIcon );

}

HB_FUNC( NEXTRACTASSICON )
{
HICON hIcon;
LONG hInstance = hb_parnl( 1 );
char * lpIconPath = ( char * ) hb_parc( 2 );
WORD lpiIcon = hb_parnl( 3 );

hIcon = ExtractAssociatedIcon(
( HINSTANCE ) hInstance,
lpIconPath,
&lpiIcon );

hb_retnl( ( LONG ) Icon2Bmp( hIcon ) );
DestroyIcon(hIcon);
}

HB_FUNC( ICON_EXEREAD )
{
SHFILEINFO sfi;

ZeroMemory(&sfi, sizeof(SHFILEINFO));

/*SHGetFileInfo(hb_parc(1), 0, &sfi, sizeof(SHFILEINFO), SHGFI_ICON | SHGFI_SHELLICONSIZE | SHGFI_USEFILEATTRIBUTES ); */
SHGetFileInfo(hb_parc(1), 0, &sfi, sizeof(SHFILEINFO), SHGFI_ICON | SHGFI_USEFILEATTRIBUTES );

hb_retnl( ( LONG ) sfi.hIcon );

}

HB_FUNC( BMPFROMICON )
{
HICON hIcon = ( HICON ) hb_parnl( 1 );

hb_retnl( ( LONG ) Icon2Bmp( hIcon ) );
}

#pragma ENDDUMP
