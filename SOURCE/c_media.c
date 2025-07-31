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
#include <mgdefs.h>                    // Custom definitions for the application (e.g., Harbour-specific macros)
#include <commctrl.h>                  // Common controls library for Windows GUI applications
#include <mmsystem.h>                  // Multimedia API for playing sounds and controlling multimedia devices

// Borland C++ compiler-specific warnings and configurations
#if defined( __BORLANDC__ ) && !defined( _WIN64 )
#pragma warn -use                      // Disable warnings about unused variables for 32-bit Borland C++
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

#include <vfw.h>                       // Video for Windows, used for handling video files and MCI (Media Control Interface)

// Function prototype to convert ANSI strings to Wide (Unicode) strings
#ifdef UNICODE
LPWSTR      AnsiToWide( LPCSTR );
#endif

// Function to retrieve resources (used when playing sounds from resource files)
HINSTANCE   GetResources( void );

/*
 * FUNCTION HB_FUNC( MESSAGEBEEP )
 *
 * Plays a system beep sound based on the specified type.
 *
 * Parameters:
 *   hb_parni(1) : An integer representing the type of beep sound to play.
 *                 This value corresponds to the flags defined in the Windows API
 *                 for MessageBeep (e.g., MB_OK, MB_ICONERROR).
 *
 * Returns:
 *   A logical value indicating whether the function call was successful.
 *   Returns .T. if the function call was successful, .F. otherwise.
 *
 * Purpose:
 *   This function provides a way to play standard system beep sounds within a
 *   Harbour application. It allows developers to provide auditory feedback to
 *   the user for various events, such as errors, warnings, or confirmations.
 *   Example Usage: MESSAGEBEEP(MB_ICONEXCLAMATION) would play the standard
 *   exclamation beep sound.
 *
 * Notes:
 *   The specific beep sound played depends on the system's configuration.
 */
HB_FUNC( MESSAGEBEEP )
{
   hb_retl( MessageBeep( hb_parni( 1 ) ) );
}

/*
 * FUNCTION HB_FUNC( C_PLAYWAVE )
 *
 * Plays a WAV sound file or a WAV resource.
 *
 * Parameters:
 *   hb_parc(1) : A character string representing the path to the WAV file or the name of the WAV resource.
 *   hb_parl(2) : A logical value indicating whether the sound is a resource. .T. if it's a resource, .F. if it's a file.
 *   hb_parl(3) : A logical value indicating whether the sound should be played synchronously. .T. for synchronous, .F. for asynchronous.
 *   hb_parl(4) : A logical value indicating whether to prevent stopping other sounds. .T. to prevent stopping, .F. otherwise.
 *   hb_parl(5) : A logical value indicating whether the sound should loop. .T. to loop, .F. otherwise.
 *   hb_parl(6) : A logical value indicating whether to suppress the default sound if the file/resource is not found. .T. to suppress, .F. otherwise.
 *
 * Returns:
 *   A logical value indicating whether the sound was played successfully.
 *   Returns .T. if the sound was played successfully, .F. otherwise.
 *
 * Purpose:
 *   This function allows Harbour applications to play WAV sound files or resources.
 *   It provides options for asynchronous or synchronous playback, looping, and
 *   resource handling. This is useful for adding sound effects or background
 *   music to an application.
 *   Example Usage: C_PLAYWAVE("mysound.wav", .F., .T., .F., .F., .F.) would play
 *   the "mysound.wav" file synchronously.
 *
 * Notes:
 *   The function uses the Windows PlaySound API.  Ensure the specified WAV file
 *   exists and is in a valid format.  For resources, ensure the resource is
 *   correctly defined in the application's resource file.
 */
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
   hb_xfree( ( TCHAR * ) pszSound );            // Free converted Wide string memory
#endif
}

/*
 * FUNCTION HB_FUNC( STOPWAVE )
 *
 * Stops any currently playing wave sounds.
 *
 * Parameters:
 *   None
 *
 * Returns:
 *   A logical value indicating whether the function call was successful.
 *   Returns .T. if the function call was successful, .F. otherwise.
 *
 * Purpose:
 *   This function provides a way to stop any currently playing WAV sounds
 *   initiated by the C_PLAYWAVE function. This is useful for stopping
 *   background music or sound effects when they are no longer needed.
 *
 * Notes:
 *   The function uses the Windows PlaySound API with the SND_PURGE flag to
 *   stop all sounds associated with the application's instance.
 */
HB_FUNC( STOPWAVE )
{
   hb_retl( PlaySound( NULL, ( HMODULE ) GetResources(), SND_PURGE ) );
}

/*
 * FUNCTION HB_FUNC( INITPLAYER )
 *
 * Initializes a media player window for playback of multimedia files.
 *
 * Parameters:
 *   hmg_par_raw_HWND(1) : The HWND of the parent window for the media player.
 *   hb_parc(2) : A character string representing the path to the media file to be played.
 *   hb_parni(3) : An integer representing the X coordinate of the media player window.
 *   hb_parni(4) : An integer representing the Y coordinate of the media player window.
 *   hb_parni(5) : An integer representing the width of the media player window.
 *   hb_parni(6) : An integer representing the height of the media player window.
 *   hb_parl(7) : A logical value indicating whether to disable automatic window sizing. .T. to disable, .F. otherwise.
 *   hb_parl(8) : A logical value indicating whether to disable automatic movie sizing. .T. to disable, .F. otherwise.
 *   hb_parl(9) : A logical value indicating whether to disable error dialogs. .T. to disable, .F. otherwise.
 *   hb_parl(10) : A logical value indicating whether to hide the menu. .T. to hide, .F. otherwise.
 *   hb_parl(11) : A logical value indicating whether to disable the open option. .T. to disable, .F. otherwise.
 *   hb_parl(12) : A logical value indicating whether to hide the playbar. .T. to hide, .F. otherwise.
 *   hb_parl(13) : A logical value indicating whether to show all controls. .T. to show, .F. otherwise.
 *   hb_parl(14) : A logical value indicating whether to show the mode. .T. to show, .F. otherwise.
 *   hb_parl(15) : A logical value indicating whether to show the file name. .T. to show, .F. otherwise.
 *   hb_parl(16) : A logical value indicating whether to show the current position. .T. to show, .F. otherwise.
 *
 * Returns:
 *   The HWND of the created media player window. Returns NULL if the window creation fails.
 *
 * Purpose:
 *   This function creates and initializes a media player window using the MCIWndCreate
 *   function. It allows developers to embed a media player control within their
 *   Harbour applications. The function takes various parameters to customize the
 *   appearance and behavior of the media player.
 *   Example Usage: INITPLAYER(hWnd, "mymovie.avi", 10, 10, 320, 240, .F., .F., .F., .F., .F., .F., .F., .F., .F., .F.)
 *   would create a media player window playing "mymovie.avi" at position (10, 10)
 *   with a size of 320x240 pixels.
 *
 * Notes:
 *   The function uses the MCIWndCreate API, which requires the Video for Windows
 *   (VFW) library to be installed. The function handles Unicode conversions for
 *   file names when compiling in Unicode mode.  Error handling is limited to a
 *   message box if the window creation fails.
 */
HB_FUNC( INITPLAYER )
{
   HWND     hwnd;

#ifndef UNICODE
   LPCSTR   szFile = hb_parc( 2 );  // File name in ANSI format
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
   hb_xfree( ( TCHAR * ) szFile );                    // Free converted Wide string memory
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

/*
 * FUNCTION HB_FUNC( MCIFUNC )
 *
 * Performs various MCI (Media Control Interface) actions on a media player window.
 *
 * Parameters:
 *   hmg_par_raw_HWND(1) : The HWND of the media player window.
 *   hb_parni(2) : An integer representing the function code to execute.
 *                 1: Play, 2: Stop, 3: Pause, 4: Close, 5: Destroy, 6: Eject, 7: End, 8: Home,
 *                 9: Open, 10: Open Dialog, 11: Play Reverse, 12: Resume, 13: Set Repeat,
 *                 14: Set Speed, 15: Set Volume, 16: Set Zoom, 17: Get Length, 18: Get Position,
 *                 19: Get Volume, 20: Seek.
 *   hb_parc(3) : (Optional) A character string representing the file name to open (used with function code 9).
 *   hb_parl(3) : (Optional) A logical value indicating whether to repeat the playback (used with function code 13).
 *   hb_parni(3) : (Optional) An integer representing the speed, volume, seek position, or zoom level (used with function codes 14, 15, 16, and 20).
 *
 * Returns:
 *   A numeric value indicating the result of the MCI action. The specific meaning
 *   of the return value depends on the function code executed.  Returns 0 for
 *   invalid function codes or when the action doesn't return a specific value.
 *
 * Purpose:
 *   This function provides a unified interface for performing various MCI actions
 *   on a media player window. It allows developers to control the playback,
 *   position, and other properties of the media player.
 *   Example Usage: MCIFUNC(hWnd, 1) would play the media in the player window.
 *   MCIFUNC(hWnd, 20, 1000) would seek to position 1000 in the media.
 *
 * Notes:
 *   The function uses the MCIWnd API to perform the requested actions. The
 *   specific parameters required depend on the function code.  Error handling is
 *   minimal; it's the developer's responsibility to ensure the correct parameters
 *   are passed for each function code.
 */
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

/*
 * FUNCTION HB_FUNC( INITANIMATE )
 *
 * Initializes an animation control window.
 *
 * Parameters:
 *   hmg_par_raw_HWND(1) : The HWND of the parent window for the animation control.
 *   hb_parni(2) : An integer representing the X coordinate of the animation control window.
 *   hb_parni(3) : An integer representing the Y coordinate of the animation control window.
 *   hb_parni(4) : An integer representing the width of the animation control window.
 *   hb_parni(5) : An integer representing the height of the animation control window.
 *   hb_parl(6) : A logical value indicating whether to enable automatic playback. .T. to enable, .F. otherwise.
 *   hb_parl(7) : A logical value indicating whether to center the animation within the control. .T. to center, .F. otherwise.
 *   hb_parl(8) : A logical value indicating whether to make the background transparent. .T. to make transparent, .F. otherwise.
 *   hb_parl(9) : A logical value indicating whether to add a border to the control. .T. to add border, .F. otherwise.
 *   hb_parl(10) : A logical value indicating whether the control is visible. .T. for visible, .F. otherwise.
 *
 * Returns:
 *   The HWND of the created animation control window. Returns NULL if the window creation fails.
 *
 * Purpose:
 *   This function creates and initializes an animation control window using the
 *   Animate_Create function. It allows developers to embed an animation control
 *   within their Harbour applications. The function takes various parameters to
 *   customize the appearance and behavior of the animation control.
 *   Example Usage: INITANIMATE(hWnd, 10, 10, 100, 100, .T., .T., .T., .T., .T.)
 *   would create an animation control window at position (10, 10) with a size of
 *   100x100 pixels, with autoplay, centering, transparency, and a border enabled.
 *
 * Notes:
 *   The function uses the Animate_Create API, which requires the common controls
 *   library to be initialized. The function retrieves resources using the
 *   GetResources function. Error handling is limited to a message box if the
 *   window creation fails.
 */
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

/*
 * FUNCTION HB_FUNC( OPENANIMATE )
 *
 * Opens an animation resource or file for playback in an animation control.
 *
 * Parameters:
 *   hmg_par_raw_HWND(1) : The HWND of the animation control window.
 *   hb_parc(2) : A character string representing the name of the animation resource or the path to the animation file (AVI).
 *
 * Returns:
 *   None
 *
 * Purpose:
 *   This function opens an animation resource or file for playback in an existing
 *   animation control. It allows developers to load animations from resources or
 *   external files.
 *   Example Usage: OPENANIMATE(hWnd, "myanimation") would open the animation
 *   resource named "myanimation".
 *
 * Notes:
 *   The function uses the Animate_Open API. The animation file should be in AVI format.
 *   The function handles Unicode conversions for file names when compiling in Unicode mode.
 */
HB_FUNC( OPENANIMATE )
{
#ifndef UNICODE
   LPCSTR   szName = hb_parc( 2 );  // Animation file/resource name in ANSI
#else
   LPCWSTR  szName = AnsiToWide( ( char * ) hb_parc( 2 ) ); // Convert to Wide string for Unicode
#endif
   Animate_Open( hmg_par_raw_HWND( 1 ), szName );           // Open the animation
#ifdef UNICODE
   hb_xfree( ( TCHAR * ) szName );  // Free converted Wide string memory
#endif
}

/*
 * FUNCTION HB_FUNC( PLAYANIMATE )
 *
 * Plays the animation in an animation control from the beginning to the end, once.
 *
 * Parameters:
 *   hmg_par_raw_HWND(1) : The HWND of the animation control window.
 *
 * Returns:
 *   None
 *
 * Purpose:
 *   This function plays the animation in an animation control from the beginning
 *   to the end, one time.
 *   Example Usage: PLAYANIMATE(hWnd) would play the animation in the control.
 *
 * Notes:
 *   The function uses the Animate_Play API. The parameters 0, -1, and 1 specify
 *   the starting frame, ending frame (play to the end), and number of repetitions
 *   (play once), respectively.
 */
HB_FUNC( PLAYANIMATE )
{
   Animate_Play( hmg_par_raw_HWND( 1 ), 0, -1, 1 );      // Play from beginning to end, once
}

/*
 * FUNCTION HB_FUNC( SEEKANIMATE )
 *
 * Seeks to a specific frame in the animation within an animation control.
 *
 * Parameters:
 *   hmg_par_raw_HWND(1) : The HWND of the animation control window.
 *   hb_parni(2) : An integer representing the frame number to seek to.
 *
 * Returns:
 *   None
 *
 * Purpose:
 *   This function allows developers to seek to a specific frame in the animation
 *   within an animation control. This is useful for implementing interactive
 *   animation controls where the user can navigate through the animation frame by frame.
 *   Example Usage: SEEKANIMATE(hWnd, 10) would seek to frame 10 in the animation.
 *
 * Notes:
 *   The function uses the Animate_Seek API. The frame number is zero-based.
 */
HB_FUNC( SEEKANIMATE )
{
   Animate_Seek( hmg_par_raw_HWND( 1 ), hb_parni( 2 ) ); // Seek to the specified frame
}

/*
 * FUNCTION HB_FUNC( STOPANIMATE )
 *
 * Stops the animation playback in an animation control.
 *
 * Parameters:
 *   hmg_par_raw_HWND(1) : The HWND of the animation control window.
 *
 * Returns:
 *   None
 *
 * Purpose:
 *   This function stops the animation playback in an animation control.
 *   Example Usage: STOPANIMATE(hWnd) would stop the animation in the control.
 *
 * Notes:
 *   The function uses the Animate_Stop API.
 */
HB_FUNC( STOPANIMATE )
{
   Animate_Stop( hmg_par_raw_HWND( 1 ) );
}

/*
 * FUNCTION HB_FUNC( CLOSEANIMATE )
 *
 * Closes the animation and releases resources associated with an animation control.
 *
 * Parameters:
 *   hmg_par_raw_HWND(1) : The HWND of the animation control window.
 *
 * Returns:
 *   None
 *
 * Purpose:
 *   This function closes the animation and releases resources associated with an
 *   animation control. It's important to call this function when the animation
 *   control is no longer needed to prevent memory leaks.
 *   Example Usage: CLOSEANIMATE(hWnd) would close the animation and release resources.
 *
 * Notes:
 *   The function uses the Animate_Close API.
 */
HB_FUNC( CLOSEANIMATE )
{
   Animate_Close( hmg_par_raw_HWND( 1 ) );
}
