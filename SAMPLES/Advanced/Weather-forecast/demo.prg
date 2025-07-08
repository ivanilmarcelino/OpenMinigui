#include <minigui.ch>

*---------------------------------------------------*
FUNCTION MAIN
*---------------------------------------------------*
   LOAD WINDOW MainFrm
   CENTER WINDOW MainFrm
   ACTIVATE WINDOW MainFrm

RETURN( NIL )

*---------------------------------------------------*
PROCEDURE ScanUrl( cCity )
*---------------------------------------------------*
   LOCAL cUrl := "http://wttr.in/"
   LOCAL cHtml
   LOCAL cHtmlFile := 'weather.html'
   LOCAL cLocalUrl := "file:///" + GetCurrentFolder() + "/" + cHtmlFile
   LOCAL cStringToLookForA := '<span class="ef250">.-.    </span></span> <span class="ef220">'
   LOCAL nPosition
   LOCAL oActiveX

   URLDownloadToFile( cUrl + cCity, cHtmlFile )
   cHtml := hb_MemoRead( cHtmlFile )
   nPosition := At( AllTrim( cStringToLookForA ), cHtml )
   MainFrm.Label_2.VALUE := 'Low Temp: ' + SubStr( cHtml, nPosition + Len( cStringToLookForA ), 2 )
   MainFrm.Label_3.VALUE := 'High Temp: ' + SubStr( cHtml, nPosition + Len( cStringToLookForA ) + 31, 2 )

   oActiveX := GetProperty( 'MainFrm', 'Activex_1', 'XObject' )
   oActiveX:Silent := 1
   oActiveX:Navigate( cLocalUrl )

RETURN

#pragma BEGINDUMP

#include <mgdefs.h>
#include <urlmon.h>

// https://msdn.microsoft.com/en-us/library/ms775123(v=vs.85).aspx

HB_FUNC( URLDOWNLOADTOFILE )  // URLDownloadToFile(cURL, cFile)
{
   HRESULT hr = URLDownloadToFile( NULL, hb_parc( 1 ), hb_parc( 2 ), 0, NULL );

   hb_retl( hr == S_OK );
}

#pragma ENDDUMP
