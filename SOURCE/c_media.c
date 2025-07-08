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
    Copyright 1999-2025, https://harbour.github.io/

    "WHAT32"
    Copyright 2002 AJ Wos <andrwos@aust1.net>

    "HWGUI"
    Copyright 2001-2021 Alexander S.Kresin <alex@kresin.ru>

   ---------------------------------------------------------------------------*/

// Include necessary headers for multimedia functions and controls
#include <mgdefs.h>                   // Custom definitions for the application (e.g., Harbour-specific macros)
#include <commctrl.h>                 // Common controls library for Windows GUI applications
#include <mmsystem.h>                 // Multimedia API for playing sounds and controlling multimedia devices

// Borland C++ compiler-specific warnings and configurations
#if defined( __BORLANDC__ ) && !defined( _WIN64 )
#pragma warn -use                     // Disable warnings about unused variables for 32-bit Borland C++
#endif

#if defined( __BORLANDC__ ) && defined( _WIN64 )
#ifndef UNICODE

// Define _MCIWndCreate as external for Borland C++ 64-bit compilation (ANSI version)
extern HWND _MCIWndCreate( HWND, HINSTANCE, DWORD, LPCSTR );
#else

// Define _MCIWndCreate as external for Borland C++ 64-bit compilation (Unicode version)
extern HWND _MCIWndCreate( HWND, HINSTANCE, DWORD, LPCWSTR );
#endif
#endif /* __BORLANDC__ & _WIN64 */

#include <vfw.h>                      // Video for Windows, used for handling video files and MCI (Media Control Interface)

// Function prototype to convert ANSI strings to Wide (Unicode) strings
#ifdef UNICODE
LPWSTR      AnsiToWide( LPCSTR );
#endif

// Function to retrieve resources (used when playing sounds from resource files)
HINSTANCE   GetResources( void );

// Harbour function to play a system beep sound
HB_FUNC( MESSAGEBEEP )
{
   hb_retl( MessageBeep( hb_parni( 1 ) ) );
}

// Harbour function to play a WAV sound file or resource
HB_FUNC( C_PLAYWAVE )
{
   DWORD    Style = SND_ASYNC;         // Default style for asynchronous play
   HMODULE  hmod = NULL;

#ifndef UNICODE
   LPCSTR   pszSound = hb_parc( 1 );   // Retrieve sound file name as ANSI string
#else
   LPCWSTR  pszSound = AnsiToWide( ( char * ) hb_parc( 1 ) );  // Convert to Wide string for Unicode
#endif

   // Check for resource flag and set appropriate style and module handle
   if( hb_parl( 2 ) )
   {
      Style |= SND_RESOURCE;
      hmod = ( HMODULE ) GetResources();
   }
   else
   {
      Style |= SND_FILENAME;
   }

   // Set additional flags based on function parameters
   if( hb_parl( 3 ) )
   {
      Style |= SND_SYNC;
   }  // Play synchronously

   if( hb_parl( 4 ) )
   {
      Style |= SND_NOSTOP;
   }  // Do not stop other sounds

   if( hb_parl( 5 ) )
   {
      Style |= SND_LOOP;
   }  // Loop the sound

   if( hb_parl( 6 ) )
   {
      Style |= SND_NODEFAULT;
   }  // No default sound if file/resource not found

   // Play the sound and return success/failure
   hb_retl( PlaySound( pszSound, hmod, Style ) );

#ifdef UNICODE
   hb_xfree( ( TCHAR * ) pszSound );   // Free converted Wide string memory
#endif
}

// Harbour function to stop any currently playing wave sounds
HB_FUNC( STOPWAVE )
{
   hb_retl( PlaySound( NULL, ( HMODULE ) GetResources(), SND_PURGE ) );
}

// Harbour function to initialize a media player window for playback
HB_FUNC( INITPLAYER )
{
   HWND     hwnd;

#ifndef UNICODE
   LPCSTR   szFile = hb_parc( 2 );     // File name in ANSI format
#else
   LPCWSTR  szFile = AnsiToWide( ( char * ) hb_parc( 2 ) ); // Convert to Wide string for Unicode
#endif
   DWORD    Style = WS_VISIBLE | WS_CHILD | WS_BORDER;      // Default window styles

   // Configure additional MCI (Media Control Interface) window styles based on parameters
   if( hb_parl( 7 ) )
   {
      Style |= MCIWNDF_NOAUTOSIZEWINDOW;
   }

   if( hb_parl( 8 ) )
   {
      Style |= MCIWNDF_NOAUTOSIZEMOVIE;
   }

   if( hb_parl( 9 ) )
   {
      Style |= MCIWNDF_NOERRORDLG;
   }

   if( hb_parl( 10 ) )
   {
      Style |= MCIWNDF_NOMENU;
   }

   if( hb_parl( 11 ) )
   {
      Style |= MCIWNDF_NOOPEN;
   }

   if( hb_parl( 12 ) )
   {
      Style |= MCIWNDF_NOPLAYBAR;
   }

   if( hb_parl( 13 ) )
   {
      Style |= MCIWNDF_SHOWALL;
   }

   if( hb_parl( 14 ) )
   {
      Style |= MCIWNDF_SHOWMODE;
   }

   if( hb_parl( 15 ) )
   {
      Style |= MCIWNDF_SHOWNAME;
   }

   if( hb_parl( 16 ) )
   {
      Style |= MCIWNDF_SHOWPOS;
   }

#if ( defined( __BORLANDC__ ) && defined( _WIN64 ) )
   hwnd = _MCIWndCreate( hmg_par_raw_HWND( 1 ), NULL, Style, szFile );
#else
   hwnd = MCIWndCreate( hmg_par_raw_HWND( 1 ), NULL, Style, szFile );
#endif
#ifdef UNICODE
   hb_xfree( ( TCHAR * ) szFile );        // Free converted Wide string memory
#endif
   if( hwnd == NULL )
   {
      MessageBox( 0, TEXT( "Player Creation Failed!" ), TEXT( "Error!" ), MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL );
      return;
   }

   // Set the player window's position and size based on parameters
   MoveWindow( hwnd, hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), TRUE );
   hmg_ret_raw_HWND( hwnd );
}

// Harbour function to perform various MCI (Media Control Interface) actions
HB_FUNC( MCIFUNC )
{
   HWND  mcihand = hmg_par_raw_HWND( 1 ); // Retrieve the MCI window handle
   int   func = hb_parni( 2 );            // Function code to execute

   // Execute the requested MCI action based on the function code
   switch( func )
   {
      case 1:
         hb_retnl( MCIWndPlay( mcihand ) );
         break;

      case 2:
         hb_retnl( MCIWndStop( mcihand ) );
         break;

      case 3:
         hb_retnl( MCIWndPause( mcihand ) );
         break;

      case 4:
         hb_retnl( MCIWndClose( mcihand ) );
         break;

      case 5:
         MCIWndDestroy( mcihand );
         hb_retnl( 0 );
         break;

      case 6:
         hb_retnl( MCIWndEject( mcihand ) );
         break;

      case 7:
         hb_retnl( MCIWndEnd( mcihand ) );
         break;

      case 8:
         hb_retnl( MCIWndHome( mcihand ) );
         break;

      case 9:
         hb_retnl( MCIWndOpen( mcihand, hb_parc( 3 ), ( UINT ) 0 ) );
         break;

      case 10:
         hb_retnl( MCIWndOpenDialog( mcihand ) );
         break;

      case 11:
         hb_retnl( MCIWndPlayReverse( mcihand ) );
         break;

      case 12:
         hb_retnl( MCIWndResume( mcihand ) );
         break;

      case 13:
         MCIWndSetRepeat( mcihand, hb_parl( 3 ) );
         hb_retnl( 0 );
         break;

      case 14:
         hb_retnl( MCIWndSetSpeed( mcihand, hb_parni( 3 ) ) );
         break;

      case 15:
         hb_retnl( MCIWndSetVolume( mcihand, hb_parni( 3 ) ) );
         break;

      case 16:
         MCIWndSetZoom( mcihand, hb_parni( 3 ) );
         hb_retnl( 0 );
         break;

      case 17:
         hb_retnl( MCIWndGetLength( mcihand ) );
         break;

      case 18:
         hb_retnl( MCIWndGetPosition( mcihand ) );
         break;

      case 19:
         hb_retnl( MCIWndGetVolume( mcihand ) );
         break;

      case 20:
         hb_retnl( MCIWndSeek( mcihand, hb_parni( 3 ) ) );
         break;

      default:
         hb_retnl( 0 );                   // Return 0 if function code is invalid
   }
}

// Harbour function to initialize an animation window
HB_FUNC( INITANIMATE )
{
   HWND  hwnd;
   DWORD Style = WS_CHILD;                // Default style for child window

   // Add styles based on parameters
   if( hb_parl( 9 ) )
   {
      Style |= WS_BORDER;
   }

   if( !hb_parl( 10 ) )
   {
      Style |= WS_VISIBLE;
   }

   if( hb_parl( 6 ) )
   {
      Style |= ACS_AUTOPLAY;
   }

   if( hb_parl( 7 ) )
   {
      Style |= ACS_CENTER;
   }

   if( hb_parl( 8 ) )
   {
      Style |= ACS_TRANSPARENT;
   }

   hwnd = Animate_Create( hmg_par_raw_HWND( 1 ), NULL, Style, GetResources() );  // Create the animation window
   if( hwnd == NULL )
   {
      MessageBox( 0, TEXT( "AnimateBox Creation Failed!" ), TEXT( "Error!" ), MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL );
      return;
   }

   // Set the animation window's position and size
   MoveWindow( hwnd, hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), TRUE );
   hmg_ret_raw_HWND( hwnd );
}

// Harbour function to open an animation resource
HB_FUNC( OPENANIMATE )
{
#ifndef UNICODE
   LPCSTR   szName = hb_parc( 2 );  // Animation file/resource name in ANSI
#else
   LPCWSTR  szName = AnsiToWide( ( char * ) hb_parc( 2 ) ); // Convert to Wide string for Unicode
#endif
   Animate_Open( hmg_par_raw_HWND( 1 ), szName );           // Open the animation
#ifdef UNICODE
   hb_xfree( ( TCHAR * ) szName );                    // Free converted Wide string memory
#endif
}

// Harbour function to play the opened animation
HB_FUNC( PLAYANIMATE )
{
   Animate_Play( hmg_par_raw_HWND( 1 ), 0, -1, 1 );   // Play from beginning to end, once
}

// Harbour function to seek to a specific frame in the animation
HB_FUNC( SEEKANIMATE )
{
   Animate_Seek( hmg_par_raw_HWND( 1 ), hb_parni( 2 ) ); // Seek to the specified frame
}

// Harbour function to stop the animation playback
HB_FUNC( STOPANIMATE )
{
   Animate_Stop( hmg_par_raw_HWND( 1 ) );
}

// Harbour function to close the animation and release resources
HB_FUNC( CLOSEANIMATE )
{
   Animate_Close( hmg_par_raw_HWND( 1 ) );
}
