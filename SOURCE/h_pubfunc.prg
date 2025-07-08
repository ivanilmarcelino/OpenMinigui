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
 Copyright 1999-2023, https://harbour.github.io/

 "WHAT32"
 Copyright 2002 AJ Wos <andrwos@aust1.net>

 "HWGUI"
   Copyright 2001-2021 Alexander S.Kresin <alex@kresin.ru>

 ---------------------------------------------------------------------------*/

#include "minigui.ch"

#if !defined(__XHARBOUR__)
// Include all Harbour code pages for extended character set support.
#include "hbextcdp.ch"

#endif

// ============================================================================

// Hides a window specified by its handle.
// hWnd: Handle to the window to be hidden.
FUNCTION HideWindow(hWnd)
RETURN ;
      ShowWindow(hWnd, SW_HIDE)

// Maximizes a window specified by its handle.
// hWnd: Handle to the window to be maximized.
FUNCTION _Maximize(hWnd)
RETURN ;
      ShowWindow(hWnd, SW_MAXIMIZE)

// Maximizes a window identified by its form name.
// FormName: Name of the form whose window needs to be maximized.
FUNCTION _MaximizeWindow(FormName)
RETURN ;
      _Maximize(GetFormHandle(FormName))

// Minimizes a window specified by its handle.
// hWnd: Handle to the window to be minimized.
FUNCTION _Minimize(hWnd)
RETURN ;
      ShowWindow(hWnd, SW_MINIMIZE)

// Minimizes a window identified by its form name.
// FormName: Name of the form whose window needs to be minimized.
FUNCTION _MinimizeWindow(FormName)
RETURN ;
      _Minimize(GetFormHandle(FormName))

// Restores a window specified by its handle to its normal state.
// hWnd: Handle to the window to be restored.
FUNCTION _Restore(hWnd)
RETURN ;
      ShowWindow(hWnd, SW_RESTORE)

// Restores a window identified by its form name to its normal state.
// FormName: Name of the form whose window needs to be restored.
FUNCTION _RestoreWindow(FormName)
RETURN ;
      _Restore(GetFormHandle(FormName))

// ============================================================================

// Retrieves the path of a special folder based on its CSIDL value.
// nCSIDL: Integer identifier for the special folder (e.g., Desktop, My Documents).
FUNCTION GetSpecialFolder(nCSIDL)
RETURN ;
      C_GetSpecialFolder(nCSIDL)

// Retrieves the Windows folder path.
FUNCTION GetWindowsFolder()
RETURN ;
      GetWindowsDir()

// Retrieves the System folder path.
FUNCTION GetSystemFolder()
RETURN ;
      GetSystemDir()

// Retrieves the system's temporary folder path.
FUNCTION GetTempFolder()
RETURN ;
      cFilePath(GetTempDir())

// Retrieves the "Temp" subdirectory within the Windows folder.
FUNCTION GetWindowsTempFolder()
RETURN ;
      GetWindowsDir() + "\Temp"

// Retrieves the "My Documents" folder path for the current user.
FUNCTION GetMyDocumentsFolder()
RETURN ;
      GetSpecialFolder(CSIDL_PERSONAL)

// Retrieves the Desktop folder path for the current user.
FUNCTION GetDesktopFolder()
RETURN ;
      GetSpecialFolder(CSIDL_DESKTOPDIRECTORY)

// Retrieves the Application Data folder path for the current user.
FUNCTION GetApplicationDataFolder()
RETURN ;
      GetSpecialFolder(CSIDL_APPDATA)

// Retrieves the Local Application Data folder path for the current user.
FUNCTION GetAppLocalDataFolder()
RETURN ;
      GetSpecialFolder(CSIDL_LOCAL_APPDATA)

// Retrieves the User Profile folder path for the current user.
FUNCTION GetUserProfileFolder()
RETURN ;
      GetSpecialFolder(CSIDL_PROFILE)

// Retrieves the temporary folder path specific to the user.
// Uses a different approach depending on the Windows version.
FUNCTION GetUserTempFolder()
RETURN ;
      iif(IsVistaOrLater(), GetAppLocalDataFolder() + "\Temp", cFilePath(GetTempDir()))

// Retrieves the "Program Files" folder path.
FUNCTION GetProgramFilesFolder()
RETURN ;
      GetSpecialFolder(CSIDL_PROGRAM_FILES)

// Retrieves the startup folder path for the current executable.
FUNCTION GetStartUpFolder()
RETURN ;
      cFilePath(GetExeFilename())

// Retrieves the full path of the current program's executable file.
FUNCTION GetProgramFilename()
RETURN ;
      GetExeFilename()

// Retrieves the full path of the module's executable file.
// The x or ... parameter is unused but included for compatibility.
#if defined(__XHARBOUR__)
FUNCTION GetModuleFilename(x)
HB_SYMBOL_UNUSED(x)
#else
FUNCTION GetModuleFilename(...)
#endif
RETURN ;
      GetExeFilename()

// ============================================================================

// Makes a control visible based on its window handle.
// hWnd: Handle to the control.
FUNCTION CShowControl(hWnd)
RETURN ;
      ShowWindow(hWnd)

// Checks if a window is a tab stop (receives focus via Tab key).
// hWnd: Handle to the window to check.
FUNCTION IsTabStop(hWnd)
RETURN ;
      IsWindowHasStyle(hWnd, 0x00010000)

// Sets or removes the tab stop style for a window.
// hWnd: Handle to the window.
// ltab: Boolean indicating whether to set (true) or remove (false) the tab stop.
FUNCTION SetTabStop(hWnd, ltab)
RETURN ;
      SetWindowStyle(hWnd, 0x00010000, ltab)

// Checks if a window can be resized (has a sizing border).
// hWnd: Handle to the window to check.
FUNCTION IsWindowSized(hWnd)
RETURN ;
      IsWindowHasStyle(hWnd, 0x00040000)

// Sets the background brush for a window.
// hWnd: Handle to the window.
// hBrush: Handle to the brush to set as the background.
FUNCTION SetWindowBackground(hWnd, hBrush)
RETURN ;
      SetWindowBrush(hWnd, hBrush)

// ============================================================================

// Retrieves the key state for a virtual key.
// VKey: Virtual key code to check the state for.
FUNCTION _GetKeyState(VKey)
RETURN ;
      CheckBit(GetKeyState(VKey), 32768)

// Retrieves the state of the Escape key.
FUNCTION GetEscapeState()
RETURN ;
      GetKeyState(VK_ESCAPE)

// Retrieves the state of the Alt key.
FUNCTION GetAltState()
RETURN ;
      GetKeyState(VK_MENU)

// Checks if the Alt key is currently active (pressed).
FUNCTION IsAltActive()
RETURN ;
      CheckBit(GetAsyncKeyState(VK_MENU), 32768)

// Checks if the Insert key is currently active (pressed).
FUNCTION IsInsertActive()
RETURN ;
      (GetKeyState(VK_INSERT) > 0)

// Checks if Caps Lock is currently active.
FUNCTION IsCapsLockActive()
RETURN ;
      (GetKeyState(VK_CAPITAL) > 0)

// Checks if Num Lock is currently active.
FUNCTION IsNumLockActive()
RETURN ;
      (GetKeyState(VK_NUMLOCK) > 0)

// Checks if Scroll Lock is currently active.
FUNCTION IsScrollLockActive()
RETURN ;
      (GetKeyState(VK_SCROLL) > 0)

// ============================================================================
// OS Detection Functions

// Checks if the operating system is Windows NT.
FUNCTION IsWinNT()
RETURN os_IsWinNT()

// Checks if the operating system is Windows XP or later.
FUNCTION IsWinXPorLater()
RETURN os_IsWinXP_Or_Later()

// Checks if the operating system is Windows Vista.
FUNCTION IsVista()
RETURN os_IsWinVista()

// Checks if the operating system is Windows Vista or later.
FUNCTION IsVistaOrLater()
RETURN os_IsWinVista_Or_Later()

// Checks if the operating system is Windows 7.
FUNCTION IsSeven()
RETURN os_IsWin7()

#if ! defined(__XHARBOUR__)
// Checks if the operating system is 64-bit (only for Harbour builds).
FUNCTION IsWin64()
RETURN hb_osIs64bit()

#else

// Checks if the operating system is Windows 11.
FUNCTION hb_osIsWin11()
RETURN '11' $ WinVersion()[1]

#endif

// Checks if the operating system is Windows 10 or later (includes Windows 11).
FUNCTION IsWin10OrLater()
RETURN (hb_osIsWin10() .OR. hb_osIsWin11())

// ============================================================================
// Sound and Beep Functions

// Plays the default beep sound.
FUNCTION PlayBeep()
RETURN ;
      MessageBeep(0xFFFFFFFF)

// Plays the asterisk sound.
FUNCTION PlayAsterisk()
RETURN ;
      MessageBeep(64)

// Plays the exclamation sound.
FUNCTION PlayExclamation()
RETURN ;
      MessageBeep(48)

// Plays the hand (error) sound.
FUNCTION PlayHand()
RETURN ;
      MessageBeep(16)

// Plays the question sound.
FUNCTION PlayQuestion()
RETURN ;
      MessageBeep(32)

// Plays the "OK" sound.
FUNCTION PlayOk()
RETURN ;
      MessageBeep(0)

// ============================================================================
// Wave and Media Player Control Functions

// Plays a wave sound from a resource.
// wave: The identifier of the wave sound resource.
FUNCTION PlayWaveFromResource(wave)
RETURN ;
      C_PlayWave(wave, .T., .F., .F., .F., .F.)

// Starts playback in a media player control.
// ControlName: Name of the media player control.
// ParentFormName: Name of the parent form containing the control.
FUNCTION _PlayPlayer(ControlName, ParentFormName)
RETURN ;
      mcifunc(GetControlHandle(ControlName, ParentFormName), 1)

// Stops playback in a media player control.
FUNCTION _StopPlayer(ControlName, ParentFormName)
RETURN ;
      mcifunc(GetControlHandle(ControlName, ParentFormName), 2)

// Pauses playback in a media player control.
FUNCTION _PausePlayer(ControlName, ParentFormName)
RETURN ;
      mcifunc(GetControlHandle(ControlName, ParentFormName), 3)

// Closes a media player control.
FUNCTION _ClosePlayer(ControlName, ParentFormName)
RETURN ;
      mcifunc(GetControlHandle(ControlName, ParentFormName), 4)

// Destroys a media player control.
// Frees associated resources.
FUNCTION _DestroyPlayer(ControlName, ParentFormName)
RETURN ;
      mcifunc(GetControlHandle(ControlName, ParentFormName), 5)

// Ejects media from a media player control (if supported).
FUNCTION _EjectPlayer(ControlName, ParentFormName)
RETURN ;
      mcifunc(GetControlHandle(ControlName, ParentFormName), 6)

// Sets the playback position to the end of the media.
FUNCTION _SetPlayerPositionEnd(ControlName, ParentFormName)
RETURN ;
      mcifunc(GetControlHandle(ControlName, ParentFormName), 7)

// Sets the playback position to the beginning of the media.
FUNCTION _SetPlayerPositionHome(ControlName, ParentFormName)
RETURN ;
      mcifunc(GetControlHandle(ControlName, ParentFormName), 8)

// Opens a media file in a media player control.
// file: The path to the media file to open.
FUNCTION _OpenPlayer(ControlName, ParentFormName, file)
RETURN ;
      mcifunc(GetControlHandle(ControlName, ParentFormName), 9, file)

// Displays an Open File dialog for selecting a media file to open.
FUNCTION _OpenPlayerDialog(ControlName, ParentFormName)
RETURN ;
      mcifunc(GetControlHandle(ControlName, ParentFormName), 10)

// Plays media in reverse in a media player control.
FUNCTION _PlayPlayerReverse(ControlName, ParentFormName)
RETURN ;
      mcifunc(GetControlHandle(ControlName, ParentFormName), 11)

// Resumes playback in a media player control.
FUNCTION _ResumePlayer(ControlName, ParentFormName)
RETURN ;
      mcifunc(GetControlHandle(ControlName, ParentFormName), 12)

// Enables repeat mode in a media player control.
FUNCTION _SetPlayerRepeatOn(ControlName, ParentFormName)
RETURN ;
      mcifunc(GetControlHandle(ControlName, ParentFormName), 13, .T.)

// Disables repeat mode in a media player control.
FUNCTION _SetPlayerRepeatOff(ControlName, ParentFormName)
RETURN ;
      mcifunc(GetControlHandle(ControlName, ParentFormName), 13, .F.)

// Sets the playback speed of a media player control.
// speed: The desired playback speed.
FUNCTION _SetPlayerSpeed(ControlName, ParentFormName, speed)
RETURN ;
      mcifunc(GetControlHandle(ControlName, ParentFormName), 14, speed)

// Sets the volume of a media player control.
// volume: The desired volume level.
FUNCTION _SetPlayerVolume(ControlName, ParentFormName, volume)
RETURN ;
      mcifunc(GetControlHandle(ControlName, ParentFormName), 15, volume)

// Sets the zoom level of a media player control.
// zoom: The desired zoom level.
FUNCTION _SetPlayerZoom(ControlName, ParentFormName, zoom)
RETURN ;
      mcifunc(GetControlHandle(ControlName, ParentFormName), 16, zoom)

// Retrieves the total length of the media in a media player control.
FUNCTION _GetPlayerLength(ControlName, ParentFormName)
RETURN ;
      mcifunc(GetControlHandle(ControlName, ParentFormName), 17)

// Retrieves the current playback position in a media player control.
FUNCTION _GetPlayerPosition(ControlName, ParentFormName)
RETURN ;
      mcifunc(GetControlHandle(ControlName, ParentFormName), 18)

// Retrieves the current volume level of a media player control.
FUNCTION _GetPlayerVolume(ControlName, ParentFormName)
RETURN ;
      mcifunc(GetControlHandle(ControlName, ParentFormName), 19)

// Sets the playback position in a media player control.
// pos: The desired playback position.
FUNCTION _SetPlayerPosition(ControlName, ParentFormName, pos)
RETURN ;
      mcifunc(GetControlHandle(ControlName, ParentFormName), 20, pos)

// ============================================================================
// Animation Box Control Functions

// Opens an animation file in an animation box control.
FUNCTION _OpenAnimateBox(ControlName, ParentFormName, FileName)
RETURN ;
      openanimate(GetControlHandle(ControlName, ParentFormName), FileName)

// Starts playback in an animation box control.
FUNCTION _PlayAnimateBox(ControlName, ParentFormName)
RETURN ;
      playanimate(GetControlHandle(ControlName, ParentFormName))

// Seeks to a specific frame in an animation box control.
// Frame: The frame number to seek to.
FUNCTION _SeekAnimateBox(ControlName, ParentFormName, Frame)
RETURN ;
      seekanimate(GetControlHandle(ControlName, ParentFormName), Frame)

// Stops playback in an animation box control.
FUNCTION _StopAnimateBox(ControlName, ParentFormName)
RETURN ;
      stopanimate(GetControlHandle(ControlName, ParentFormName))

// Closes an animation box control.
FUNCTION _CloseAnimateBox(ControlName, ParentFormName)
RETURN ;
      closeanimate(GetControlHandle(ControlName, ParentFormName))

// Destroys an animation box control.
// Frees associated resources.
FUNCTION _DestroyAnimateBox(ControlName, ParentFormName)
RETURN ;
      destroywindow(GetControlHandle(ControlName, ParentFormName))

// ============================================================================

// ============================================================================

/**
 * Retrieves the append permission for a Browse control.
 * @param ControlName Name of the control.
 * @param ParentFormName Name of the parent form.
 * @return Current state of the append permission.
 */
FUNCTION _GetBrowseAllowAppend ( ControlName, ParentFormName )
RETURN ;
      _SetGetBrowseProperty ( ControlName, ParentFormName, 2 )

/**
 * Retrieves the edit permission for a Browse control.
 * @param ControlName Name of the control.
 * @param ParentFormName Name of the parent form.
 * @return Current state of the edit permission.
 */
FUNCTION _GetBrowseAllowEdit ( ControlName, ParentFormName )
RETURN ;
      _SetGetBrowseProperty ( ControlName, ParentFormName, 6 )

/**
 * Retrieves the delete permission for a Browse control.
 * @param ControlName Name of the control.
 * @param ParentFormName Name of the parent form.
 * @return Current state of the delete permission.
 */
FUNCTION _GetBrowseAllowDelete ( ControlName, ParentFormName )
RETURN ;
      _SetGetBrowseProperty ( ControlName, ParentFormName, 12 )

/**
 * Retrieves the input items for a Browse control.
 * @param ControlName Name of the control.
 * @param ParentFormName Name of the parent form.
 * @return List of input items.
 */
FUNCTION _GetBrowseInputItems ( ControlName, ParentFormName )
RETURN ;
      _SetGetBrowseProperty ( ControlName, ParentFormName, 13 )

/**
 * Retrieves the display items for a Browse control.
 * @param ControlName Name of the control.
 * @param ParentFormName Name of the parent form.
 * @return List of display items.
 */
FUNCTION _GetBrowseDisplayItems ( ControlName, ParentFormName )
RETURN ;
      _SetGetBrowseProperty ( ControlName, ParentFormName, 14 )

/**
 * Sets the append permission for a Browse control.
 * @param ControlName Name of the control.
 * @param ParentFormName Name of the parent form.
 * @param Value New state for append permission.
 */
FUNCTION _SetBrowseAllowAppend ( ControlName, ParentFormName, Value )
RETURN ;
      _SetGetBrowseProperty ( ControlName, ParentFormName, 2, Value )

/**
 * Sets the edit permission for a Browse control.
 * @param ControlName Name of the control.
 * @param ParentFormName Name of the parent form.
 * @param Value New state for edit permission.
 */
FUNCTION _SetBrowseAllowEdit ( ControlName, ParentFormName, Value )
RETURN ;
      _SetGetBrowseProperty ( ControlName, ParentFormName, 6, Value )

/**
 * Sets the delete permission for a Browse control.
 * @param ControlName Name of the control.
 * @param ParentFormName Name of the parent form.
 * @param Value New state for delete permission.
 */
FUNCTION _SetBrowseAllowDelete ( ControlName, ParentFormName, Value )
RETURN ;
      _SetGetBrowseProperty ( ControlName, ParentFormName, 12, Value )

/**
 * Sets the input items for a Browse control.
 * @param ControlName Name of the control.
 * @param ParentFormName Name of the parent form.
 * @param Value New list of input items.
 */
FUNCTION _SetBrowseInputItems ( ControlName, ParentFormName, Value )
RETURN ;
      _SetGetBrowseProperty ( ControlName, ParentFormName, 13, Value )

/**
 * Sets the display items for a Browse control.
 * @param ControlName Name of the control.
 * @param ParentFormName Name of the parent form.
 * @param Value New list of display items.
 */
FUNCTION _SetBrowseDisplayItems ( ControlName, ParentFormName, Value )
RETURN ;
      _SetGetBrowseProperty ( ControlName, ParentFormName, 14, Value )

// ============================================================================

/**
 * Retrieves the name of a hotkey control.
 * @param ControlName Name of the control.
 * @param FormName Name of the parent form.
 * @return Hotkey name as a string.
 */
FUNCTION GetHotKeyName ( ControlName, FormName )
RETURN ;
      _GetHotKeyName ( ControlName, FormName )

/**
 * Retrieves the value associated with a hotkey control.
 * @param ControlName Name of the control.
 * @param FormName Name of the parent form.
 * @return Hotkey value.
 */
FUNCTION GetHotKeyValue ( ControlName, FormName )
RETURN ;
      _GetHotKeyValue ( ControlName, FormName )

/**
 * Helper function to retrieve the value of a hotkey control.
 * @param ControlName Name of the control.
 * @param FormName Name of the parent form.
 * @return Hotkey value.
 */
FUNCTION _GetHotKeyValue( ControlName, FormName )
RETURN ;
      C_GetHotKeyValue( GetControlHandle( ControlName, FormName ) )

// ============================================================================

/**
 * Checks if the error log feature is active.
 * @return Status of the error log feature.
 */
FUNCTION IsErrorLogActive ()
RETURN ;
      _HMG_CreateErrorLog

/**
 * Retrieves the error log file name.
 * @return Error log file name.
 */
FUNCTION _GetErrorlogFile ()
RETURN ;
      _HMG_ErrorLogFile

// ============================================================================

/**
 * Retrieves the NotifyIcon name for a given form.
 * @param FormName Name of the form.
 * @return NotifyIcon name.
 */
FUNCTION _GetNotifyIconName ( FormName )
RETURN ;
      _HMG_aFormNotifyIconName[ GetFormIndex( FormName ) ]

/**
 * Retrieves the NotifyIcon tooltip for a given form.
 * @param FormName Name of the form.
 * @return NotifyIcon tooltip.
 */
FUNCTION _GetNotifyIconTooltip ( FormName )
RETURN ;
      _HMG_aFormNotifyIconTooltip[ GetFormIndex( FormName ) ]

/**
 * Retrieves the read-only state of a radio group control.
 * @param ControlName Name of the control.
 * @param FormName Name of the parent form.
 * @return Read-only state of the control.
 */
FUNCTION _GetRadioGroupReadOnly ( ControlName, FormName )
RETURN ;
      GetControlPageMap ( ControlName, FormName )

/**
 * Retrieves the memory address of a control's data.
 * @param ControlName Name of the control.
 * @param FormName Name of the parent form.
 * @return Address of the control's data.
 */
FUNCTION _GetAddress ( ControlName, FormName )
RETURN ;
      _HMG_aControlValue[ GetControlIndex ( ControlName, FormName ) ]

// ============================================================================

/**
 * Sets the cursor resource to the specified cursor file.
 * @param cCursor Name of the cursor resource.
 * @return The result of setting the cursor.
 */
FUNCTION RC_CURSOR ( cCursor )
RETURN ;
      SetResCursor ( LoadCursor( GetInstance(), cCursor ) )

/**
 * Retrieves the row position of the cursor.
 * @return Current row position of the cursor.
 */
FUNCTION GetCursorRow ()
RETURN ;
      GetCursorPos() [ 1 ]

/**
 * Retrieves the column position of the cursor.
 * @return Current column position of the cursor.
 */
FUNCTION GetCursorCol ()
RETURN ;
      GetCursorPos() [ 2 ]

/**
 * Splits a string into an array using a specified separator.
 * @param cData Input string to split.
 * @param Sep Separator character or string. Defaults to a tab if not provided.
 * @return Array of split strings.
 */
FUNCTION LB_String2Array( cData, Sep )
RETURN ;
      hb_ATokens( cData, iif( HB_ISSTRING( Sep ), Sep, Chr( 9 ) ) )

// ============================================================================

/**
 * Destroys the image list associated with a control.
 * @param ControlName Name of the control.
 * @param ParentFormName Name of the parent form.
 */
FUNCTION _DestroyImageList ( ControlName, ParentFormName )
   ImageList_Destroy ( GetControlHandle( ControlName, ParentFormName ) )
   _ReleaseControl ( ControlName, ParentFormName )
RETURN NIL

/**
 * Destroys an image list handle.
 * @param h Handle to the image list.
 * @return Result of the destroy operation.
 */
FUNCTION IL_DESTROY ( h )
RETURN ;
      ImageList_Destroy ( h )

/**
 * Initiates drag operation for an image at the specified position.
 * @param ix X-coordinate of the drag start.
 * @param iy Y-coordinate of the drag start.
 * @return Result of the drag operation.
 */
FUNCTION _DragEnterImage ( ix, iy )
RETURN ;
      IL_DragEnter ( _HMG_ActiveDragImageHandle, ix, iy )

/**
 * Moves the currently dragged image to a new position.
 * @param ix New X-coordinate for the image.
 * @param iy New Y-coordinate for the image.
 * @return Result of the move operation.
 */
FUNCTION _MoveImage ( ix, iy )
RETURN ;
      IL_DragMove ( ix, iy )

/**
 * Ends the current image drag operation.
 * @return Result of ending the drag operation.
 */
FUNCTION _EndDragImage ()
RETURN ;
      IL_EndDrag ( _HMG_ActiveDragImageHandle )

/**
 * Retrieves the number of images in an image list associated with a control.
 * @param ControlName Name of the control.
 * @param ParentFormName Name of the parent form.
 * @return Number of images in the list.
 */
FUNCTION _GetImageCount ( ControlName, ParentFormName )
RETURN ;
      IL_GetImageCount ( GetControlHandle( ControlName, ParentFormName ) )

/**
 * Removes an image from the image list at a specified index.
 * @param ControlName Name of the control.
 * @param ParentFormName Name of the parent form.
 * @param ImageIndex Index of the image to remove.
 * @return Result of the removal operation.
 */
FUNCTION _RemoveImageFromImageList ( ControlName, ParentFormName, ImageIndex )
RETURN ;
      IL_Remove( GetControlHandle( ControlName, ParentFormName ), ImageIndex )

/**
 * Draws an image from the image list at the specified coordinates.
 * @param ControlName Name of the control.
 * @param ParentFormName Name of the parent form.
 * @param ImageIndex Index of the image to draw.
 * @param cx X-coordinate for drawing the image.
 * @param cy Y-coordinate for drawing the image.
 * @return Result of the drawing operation.
 */
FUNCTION _DrawImageFromImageList ( ControlName, ParentFormName, ImageIndex, cx, cy )
RETURN ;
      IL_Draw( GetFormHandle ( ParentFormName ), GetControlHandle( ControlName, ParentFormName ), ImageIndex, cx, cy )

// ============================================================================

/**
 * Adds a child control to a Pager control.
 * @param ControlName Name of the child control.
 * @param ParentFormName Name of the parent form.
 * @return Result of the addition.
 */
FUNCTION _AddChildToPager ( ControlName, ParentFormName )
RETURN ;
      AddToPager ( _HMG_ActivePagerForm, GetControlHandle( ControlName, ParentFormName ) )

/**
 * Enables or disables mouse forwarding for a Pager control.
 * @param ControlName Name of the control.
 * @param ParentFormName Name of the parent form.
 * @param lEnable Boolean flag to enable or disable mouse forwarding.
 * @return Result of the operation.
 */
FUNCTION _Pager_ForwardMouse ( ControlName, ParentFormName, lEnable )
RETURN ;
      PagerForwardMouse ( GetControlHandle( ControlName, ParentFormName ), IFLOGICAL( lEnable, lEnable, .F. ) )

/**
 * Retrieves the button size for a Pager control.
 * @param ControlName Name of the control.
 * @param ParentFormName Name of the parent form.
 * @return Size of the Pager buttons.
 */
FUNCTION _Pager_GetButtonSize ( ControlName, ParentFormName )
RETURN ;
      PagerGetButtonSize ( GetControlHandle( ControlName, ParentFormName ) )

/**
 * Sets the button size for a Pager control.
 * @param ControlName Name of the control.
 * @param ParentFormName Name of the parent form.
 * @param nSize New size for the Pager buttons.
 * @return Result of the operation.
 */
FUNCTION _Pager_SetButtonSize ( ControlName, ParentFormName, nSize )
RETURN ;
      PagerSetButtonSize ( GetControlHandle( ControlName, ParentFormName ), nSize )

/**
 * Retrieves the border size for a Pager control.
 * @param ControlName Name of the control.
 * @param ParentFormName Name of the parent form.
 * @return Size of the Pager border.
 */
FUNCTION _Pager_GetBorder ( ControlName, ParentFormName )
RETURN ;
      PagerGetBorder ( GetControlHandle( ControlName, ParentFormName ) )

/**
 * Sets the border size for a Pager control.
 * @param ControlName Name of the control.
 * @param ParentFormName Name of the parent form.
 * @param nSize New border size for the Pager.
 * @return Result of the operation.
 */
FUNCTION _Pager_SetBorder ( ControlName, ParentFormName, nSize )
RETURN ;
      PagerSetBorder ( GetControlHandle( ControlName, ParentFormName ), nSize )

/**
 * Retrieves the current position of a Pager control.
 * @param ControlName Name of the control.
 * @param ParentFormName Name of the parent form.
 * @return Current position of the Pager.
 */
FUNCTION _Pager_GetPos ( ControlName, ParentFormName )
RETURN ;
      PagerGetPos ( GetControlHandle( ControlName, ParentFormName ) )

/**
 * Sets the current position of a Pager control.
 * @param ControlName Name of the control.
 * @param ParentFormName Name of the parent form.
 * @param nPos New position for the Pager.
 * @return Result of the operation.
 */
FUNCTION _Pager_SetPos ( ControlName, ParentFormName, nPos )
RETURN ;
      PagerSetPos ( GetControlHandle( ControlName, ParentFormName ), nPos )

// ============================================================================

/**
 * Sets the column position of a control.
 * @param ControlName Name of the control.
 * @param ParentForm Name of the parent form.
 * @param Value New column position.
 */
FUNCTION _SetControlCol ( ControlName, ParentForm, Value )
RETURN ;
      _SetControlSizePos ( ControlName, ParentForm, _GetControlRow ( ControlName, ParentForm ), VALUE, _GetControlWidth ( ControlName, ParentForm ), _GetControlHeight ( ControlName, ParentForm ) )

// ============================================================================

/**
 * Sets the row position of a control.
 * @param ControlName Name of the control.
 * @param ParentForm Name of the parent form.
 * @param Value New row position.
 */
FUNCTION _SetControlRow ( ControlName, ParentForm, Value )
RETURN ;
      _SetControlSizePos ( ControlName, ParentForm, VALUE, _GetControlCol ( ControlName, ParentForm ), _GetControlWidth ( ControlName, ParentForm ), _GetControlHeight ( ControlName, ParentForm ) )

/**
 * Sets the width of a control.
 * @param ControlName Name of the control.
 * @param ParentForm Name of the parent form.
 * @param Value New width for the control.
 */
FUNCTION _SetControlWidth ( ControlName, ParentForm, Value )
RETURN ;
      _SetControlSizePos ( ControlName, ParentForm, _GetControlRow ( ControlName, ParentForm ), _GetControlCol ( ControlName, ParentForm ), VALUE, _GetControlHeight ( ControlName, ParentForm ) )

/**
 * Sets the height of a control.
 * @param ControlName Name of the control.
 * @param ParentForm Name of the parent form.
 * @param Value New height for the control.
 */
FUNCTION _SetControlHeight ( ControlName, ParentForm, Value )
RETURN ;
      _SetControlSizePos ( ControlName, ParentForm, _GetControlRow ( ControlName, ParentForm ), _GetControlCol ( ControlName, ParentForm ), _GetControlWidth ( ControlName, ParentForm ), Value )

// ============================================================================

/**
 * Gets the width of the desktop screen.
 * @return Width of the desktop in pixels.
 */
FUNCTION GetDesktopWidth ()
RETURN ;
      GetSystemMetrics( SM_CXSCREEN )

/**
 * Gets the height of the desktop screen.
 * @return Height of the desktop in pixels.
 */
FUNCTION GetDesktopHeight ()
RETURN ;
      GetSystemMetrics( SM_CYSCREEN )

/**
 * Gets the width of a vertical scrollbar.
 * @return Width of the vertical scrollbar in pixels.
 */
FUNCTION GetVScrollBarWidth ()
RETURN ;
      GetSystemMetrics( SM_CXVSCROLL )

/**
 * Gets the height of a horizontal scrollbar.
 * @return Height of the horizontal scrollbar in pixels.
 */
FUNCTION GetHScrollBarHeight ()
RETURN ;
      GetSystemMetrics( SM_CYHSCROLL )

/**
 * Gets the height of a window's title bar.
 * @return Height of the title bar in pixels.
 */
FUNCTION GetTitleHeight ()
RETURN ;
      GetSystemMetrics( SM_CYCAPTION )

/**
 * Gets the height of a window border.
 * @return Height of the border in pixels.
 */
FUNCTION GetBorderHeight ()
RETURN ;
      GetSystemMetrics( SM_CYSIZEFRAME )

/**
 * Gets the width of a window border.
 * @return Width of the border in pixels.
 */
FUNCTION GetBorderWidth ()
RETURN ;
      GetSystemMetrics( SM_CXSIZEFRAME )

/**
 * Gets the height of a 3D edge in the system's UI.
 * @return Height of the 3D edge in pixels.
 */
FUNCTION Get3DEdgeHeight ()
RETURN ;
      GetSystemMetrics( SM_CYEDGE )

/**
 * Gets the width of a 3D edge in the system's UI.
 * @return Width of the 3D edge in pixels.
 */
FUNCTION Get3DEdgeWidth ()
RETURN ;
      GetSystemMetrics( SM_CXEDGE )

/**
 * Gets the height of the menu bar.
 * @return Height of the menu bar in pixels.
 */
FUNCTION GetMenuBarHeight ()
RETURN ;
      GetSystemMetrics( SM_CYMENU )

// ============================================================================

/**
 * Retrieves the border size of a window.
 * @return Array containing details of the non-client area. First element is the border size.
 */
FUNCTION GetWindowBorderSize ()
RETURN GetNonClient() [ 1 ]

/**
 * Retrieves the size of the scrollbar area.
 * @return Array containing details of the non-client area. Third element is the scrollbar size.
 */
FUNCTION GetScrollBarSize ()
RETURN GetNonClient() [ 3 ]

/**
 * Retrieves the width of the title bar.
 * @return Array containing details of the non-client area. Fourth element is the title bar width.
 */
FUNCTION GetTitleBarWidth ()
RETURN GetNonClient() [ 4 ]

/**
 * Retrieves the height of the title bar.
 * @return Array containing details of the non-client area. Fifth element is the title bar height.
 */
FUNCTION GetTitleBarHeight ()
RETURN GetNonClient() [ 5 ]

/**
 * Retrieves the size of the menu bar.
 * @return Array containing details of the non-client area. Seventh element is the menu bar size.
 */
FUNCTION GetMenuBarSize ()
RETURN GetNonClient() [ 7 ]

// ============================================================================

/**
 * Sends a message with a wide string parameter to a specified window handle.
 * @param h Handle of the target window.
 * @param n Message ID.
 * @param wp Additional message-specific information (Word).
 * @param lp Wide string parameter (Long).
 * @return Result of the message processing.
 */
FUNCTION SendMessageWideString( h, n, wp, lp )
RETURN ;
      SendMessageStringW( h, n, wp, lp )

/**
 * Processes all pending Windows messages in the application queue.
 * @return Result of the operation.
 */
FUNCTION ProcessMessages()
RETURN DoEvents()

/**
 * Performs a bitwise AND operation.
 * @param x1 First operand.
 * @param x2 Second operand.
 * @return Result of the bitwise AND operation.
 */
FUNCTION And( x1, x2 )
RETURN hb_bitAnd( x1, x2 )

#if defined( __XHARBOUR__ )
/**
 * Performs a bitwise OR operation with up to 7 operands.
 * @param p1-p7 Operands for the OR operation.
 * @return Result of the bitwise OR operation.
 */
FUNCTION nOr( p1, p2, p3, p4, p5, p6, p7 )
RETURN hb_bitOr( p1, p2, p3, p4, p5, p6, p7 )

#else

/**
 * Performs a bitwise OR operation with any number of operands.
 * @param ... Variable number of operands.
 * @return Result of the bitwise OR operation.
 */
FUNCTION nOr( ... )
RETURN hb_bitOr( ... )

#endif

/**
 * Generates a random number within a specified range.
 * @param nMax Maximum value for the random number (inclusive).
 * @return Random integer between 0 and nMax.
 */
FUNCTION Random( nMax )
RETURN hb_RandomInt( IFNUMERIC( nMax, nMax, 65535 ) )

/**
 * Placeholder dummy function.
 * @return Always returns NIL.
 */
FUNCTION _dummy()
RETURN iif( .T.,, )

/**
 * Calculates the width of a string when rendered in a specific font.
 * @param FontName Name of the font.
 * @param nLen Length of the string.
 * @return Width of the string in pixels.
 */
FUNCTION GetFontWidth( FontName, nLen )
RETURN ;
      GetFontParam( GetFontHandle( FontName ) )[ 8 ] * nLen

/**
 * Retrieves the height of a specified font.
 * @param FontName Name of the font.
 * @return Height of the font in pixels.
 */
FUNCTION GetFontHeight( FontName )
RETURN ;
      GetFontParam( GetFontHandle( FontName ) )[ 9 ]

/**
 * Retrieves the name of a font from its handle.
 * @param hFont Handle of the font.
 * @return Name of the font.
 */
FUNCTION GetFontNameByHandle( hFont )
RETURN ;
      GetFontParam( hFont )[ 10 ]

/**
 * Converts RGB values into a single numerical value.
 * @param p1 Red value or an array containing RGB values.
 * @param p2 Green value (optional).
 * @param p3 Blue value (optional).
 * @return Numerical RGB representation.
 */
FUNCTION HMG_RGB2n( p1, p2, p3 )
RETURN IFNUMERIC( p1, RGB( p1, p2, p3 ), IFARRAY( p1, RGB( p1[ 1 ], p1[ 2 ], p1[ 3 ] ), p1 ) )

/**
 * Converts a numerical RGB value into its component RGB values.
 * @param x Numerical RGB value.
 * @return Array containing Red, Green, and Blue components.
 */
FUNCTION HMG_n2RGB( x )
RETURN { GetRed( x ), GetGreen( x ), GetBlue( x ) }

/**
 * Deletes a global variable.
 * @param cVarName Name of the variable to delete.
 * @return Result of the deletion.
 */
FUNCTION _DelGlobal( cVarName )
RETURN _SetGetGlobal( cVarName, , .T. )

#if ! defined( __XHARBOUR__ )

/**
 * Sets a name list with specified values.
 * @param x List identifier, used as a key to associate with the list.
 * @param v Values to set for the name list (optional). If omitted, no change is made.
 * @return Result of the operation, typically the updated or current name list.
 */
FUNCTION _SetNameList( x, v )
RETURN _SetGetNamesList( x, v )

/**
 * Retrieves the name list for a specified identifier.
 * @param x List identifier whose associated name list is to be retrieved.
 * @return The name list associated with the given identifier.
 */
FUNCTION _GetNameList( x )
RETURN _SetGetNamesList( x )

/**
 * Deletes a name list for a specified identifier.
 * @param x List identifier whose associated name list is to be deleted.
 * @return Result of the deletion operation.
 */
FUNCTION _DelNameList( x )
RETURN _SetGetNamesList( x, , .T. )

/**
 * Calculates the time difference in milliseconds.
 * @param dTS1 Start datetime value.
 * @param dTS2 End datetime value (optional, defaults to the current date and time).
 * @return A string representation of the time difference in milliseconds.
 */
FUNCTION HMG_TimeMS( dTS1, dTS2 )
RETURN LTrim( hb_TSToStr( ( hb_StrToTS( "" ) + ( hb_defaultValue( dTS2, hb_DateTime() ) - dTS1 ) ), .T. ) )

#endif

/**
 * Pauses the execution of the program for a specified duration.
 * @param nSeconds Number of seconds to wait (optional, defaults to 0.105 seconds).
 * @return Result of the sleep operation.
 */
FUNCTION HMG_SysWait( nSeconds )
RETURN hb_idleSleep( hb_defaultValue( nSeconds, 0.105 ) )

/**
 * Creates a new instance of the THmgData object.
 * @param lUpper Boolean indicating whether to use uppercase keys for the data object (default is .T.).
 * @return A new instance of THmgData.
 */
FUNCTION oHmgData( lUpper )
RETURN THmgData():New( hb_defaultValue( lUpper, .T. ) )
