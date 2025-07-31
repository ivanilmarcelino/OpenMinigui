/*-------------------------------------------------------------------------
   MINIGUI - Harbour Win32 GUI library source code

   Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
   http://harbourminigui.googlepages.com/

   This  program is free software; you can redistribute it and/or modify it
   under  the  terms  of the GNU General Public License as published by the
   Free  Software  Foundation; either version 2 of the License, or (at your
   option) any later version.

   This  program  is  distributed  in  the hope that it will be useful, but
   WITHOUT   ANY   WARRANTY;   without   even   the   implied  warranty  of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
   Public License for more details.

   You  should have received a copy of the GNU General Public License along
   with  this  software;  see  the  file COPYING. If not, write to the Free
   Software  Foundation,  Inc.,  59  Temple  Place,  Suite  330, Boston, MA
   02111-1307 USA (or visit the web site http://www.gnu.org/).

   As  a  special exception, you have permission for additional uses of the
   text contained in this release of Harbour Minigui.

   The  exception  is  that,  if  you link the Harbour Minigui library with
   other  files to produce an executable, this does not by itself cause the
   resulting  executable  to  be covered by the GNU General Public License.
   Your  use  of  that  executable  is  in  no way restricted on account of
   linking the Harbour-Minigui library code into it.

   Parts of this project are based upon:

    "Harbour GUI framework for Win32"
    Copyright 2001 Alexander S.Kresin <alex@kresin.ru>
    Copyright 2001 Antonio Linares <alinares@fivetech.com>
    www - https://harbour.github.io/

    "Harbour Project"
    Copyright 1999-2025, https://harbour.github.io/

    "WHAT32"
    Copyright 2002 AJ Wos <andrwos@aust1.net>

    "HWGUI"
    Copyright 2001-2021 Alexander S.Kresin <alex@kresin.ru>

-------------------------------------------------------------------------*/

#include <mgdefs.h>        // Include Minigui definitions and function prototypes
#include "hbapierr.h"      // Include Harbour error handling

#ifndef __XHARBOUR__
#include "hbwinuni.h"      // Include Harbour-specific Windows Unicode support
#else
// Define HB_WCHAR as a wide-character type for compatibility with xHarbour
typedef wchar_t HB_WCHAR;
#endif

// Define EM_GETCUEBANNER if not defined in the system headers (for retrieving cue banner text)
#if (!defined(EM_GETCUEBANNER))
#if (!defined(ECM_FIRST))
#define ECM_FIRST 0x1500                // Base constant for edit control messages
#endif
#define EM_GETCUEBANNER (ECM_FIRST + 2) // Define EM_GETCUEBANNER as a message ID for retrieving cue banner text
#endif

/*
 * FUNCTION GETCUEBANNERTEXT()
 *
 * Retrieves the cue banner text (placeholder text) from an edit control.
 *
 * Parameters:
 *   Parameter 1: hwnd (HWND) - The handle of the edit control window.
 *
 * Returns:
 *   (STRING) - The cue banner text as a string. Returns NULL string if the cue banner is not set or if an error occurs.
 *
 * Purpose:
 *   This function allows you to retrieve the cue banner text that is displayed in an edit control when it is empty.
 *   The cue banner provides a hint to the user about what kind of input is expected in the edit control.
 *   This is useful for providing a better user experience by guiding the user on how to use the application.
 *
 * Notes:
 *   The function allocates memory to store the cue banner text. This memory is freed before the function returns.
 *   The function uses the EM_GETCUEBANNER message to retrieve the cue banner text.
 *   If an invalid window handle is passed, an error is raised.
 */
HB_FUNC( GETCUEBANNERTEXT )
{
   HWND hwnd = hmg_par_raw_HWND( 1 ); // Get the window handle from the function parameters

   if( IsWindow( hwnd ) ) // Check if the provided handle is a valid window
   {
      // Allocate memory for a buffer to store the cue banner text as a wide string
      HB_WCHAR *lpWCStr = ( HB_WCHAR * ) hb_xgrab( 256 * sizeof( HB_WCHAR ) );

      // Retrieve the cue banner text using the EM_GETCUEBANNER message, limited to 256 characters
      if( SendMessage( hwnd, EM_GETCUEBANNER, ( WPARAM ) ( LPWSTR ) lpWCStr, ( LPARAM ) 256 ) )
      {
#ifdef __XHARBOUR__
         // Convert the wide string to a multibyte string for xHarbour and return it
         hb_retc( ( const char * ) hb_wctomb( lpWCStr ) );
#else
         // Return the Unicode string directly for Harbour, specifying the encoding
         hb_retstrlen_u16( HB_CDP_ENDIAN_NATIVE, lpWCStr, 256 );
#endif
      }
      else
      {
         hb_retc_null(); // Return NULL if cue banner retrieval fails
      }

      hb_xfree( lpWCStr ); // Free the allocated memory for the buffer
   }
   else
   {
      // Error handling: Invalid window handle provided
      hb_errRT_BASE_SubstR( EG_ARG, 0, "Invalid window handle.", HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
   }
}

/*
 * FUNCTION SENDMESSAGESTRINGW()
 *
 * Sends a Windows message to a specified window, including a Unicode string as a parameter.
 *
 * Parameters:
 *   Parameter 1: hwnd (HWND) - The handle of the window to which the message will be sent.
 *   Parameter 2: message (UINT) - The message identifier (e.g., WM_SETTEXT).
 *   Parameter 3: boolean flag (LOGICAL) - A boolean flag (TRUE or FALSE) to be passed as the WPARAM of the message.
 *   Parameter 4: string (STRING) - The string to be sent as the LPARAM of the message. This string will be converted to Unicode.
 *
 * Returns:
 *   None (VOID). The function does not return a value directly. The effect of the function depends on the message being sent.
 *
 * Purpose:
 *   This function provides a way to send Windows messages that require a Unicode string as a parameter.
 *   It handles the conversion of the Harbour string to a wide-character (Unicode) string before sending the message.
 *   This is essential for interacting with Windows controls that expect Unicode input, especially in internationalized applications.
 *
 * Notes:
 *   The function allocates memory to store the Unicode string. This memory is freed after the message is sent.
 *   If an invalid window handle is passed, an error is raised.
 *   If the string parameter is empty, a NULL pointer is passed as the LPARAM.
 */
HB_FUNC( SENDMESSAGESTRINGW )
{
   HWND hwnd = hmg_par_raw_HWND( 1 ); // Get the window handle from function parameters

   if( IsWindow( hwnd ) ) // Check if the provided handle is a valid window
   {
      // Convert the provided multibyte string to a wide string (Unicode) if it is not empty
      HB_WCHAR *lpWCStr = ( HB_WCHAR * ) ( hb_parclen( 4 ) == 0 ? NULL : hb_mbtowc( hb_parc( 4 ) ) );

      // Send the Unicode message with the specified parameters to the window
      SendMessage( hwnd, hmg_par_UINT( 2 ), ( WPARAM ) hb_parl( 3 ), ( LPARAM ) ( LPCWSTR ) lpWCStr );

      // Free the allocated memory for the wide string buffer if it was created
      if( lpWCStr != NULL )
      {
         hb_xfree( lpWCStr );
      }
   }
   else
   {
      // Error handling: Invalid window handle provided
      hb_errRT_BASE_SubstR( EG_ARG, 0, "Invalid window handle.", HB_ERR_FUNCNAME, 1, hb_paramError( 1 ) );
   }
}
