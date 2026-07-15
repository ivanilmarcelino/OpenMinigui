
/////////////////////////////////////////////
// Application pseudo-properties
/////////////////////////////////////////////

#translate <p:Application,App>.ExeName         => GetExeFileName()
#translate <p:Application,App>.Handle          => _HMG_MainHandle
#translate <p:Application,App>.FormName        => _HMG_aFormNames \[ Ascan ( _HMG_aFormHandles, <p>.Handle ) ]
#translate <p:Application,App>.Col             => GetWindowCol ( <p>.Handle )
#translate <p:Application,App>.Col := <arg>    => MoveWindow ( <p>.Handle , <arg> , <p>.Row , <p>.Width , <p>.Height , .t. )
#translate <p:Application,App>.Row             => GetWindowRow ( <p>.Handle )
#translate <p:Application,App>.Row := <arg>    => MoveWindow ( <p>.Handle , <p>.Col , <arg> , <p>.Width , <p>.Height , .t. )
#translate <p:Application,App>.Width           => GetWindowWidth ( <p>.Handle )
#translate <p:Application,App>.Width := <arg>  => MoveWindow ( <p>.Handle , <p>.Col , <p>.Row , <arg> , <p>.Height , .t. )
#translate <p:Application,App>.Height          => GetWindowHeight ( <p>.Handle )
#translate <p:Application,App>.Height := <arg> => MoveWindow ( <p>.Handle , <p>.Col , <p>.Row , <p>.Width , <arg> , .t. )
#translate <p:Application,App>.ClientWidth     => _GetClientRect ( <p>.Handle ) \[ 3 ]
#translate <p:Application,App>.ClientHeight    => _GetClientRect ( <p>.Handle ) \[ 4 ]
#translate <p:Application,App>.Title           => GetWindowText ( <p>.Handle )
#translate <p:Application,App>.Title := <arg>  => SetWindowText ( <p>.Handle, <arg> )
#translate <p:Application,App>.Icon            => _HMG_DefaultIconName
#translate <p:Application,App>.Icon.Handle     => LoadTrayIcon( GetResources(), _HMG_DefaultIconName, 32, 32 )
#translate <p:Application,App>.Cursor := <arg> => SetWindowCursor ( <p>.Handle, <arg> )
#translate <p:Application,App>.FontName        => _HMG_DefaultFontName
#translate <p:Application,App>.FontSize        => _HMG_DefaultFontSize
#translate <p:Application,App>.BackColor       => _HMG_aFormBkColor \[ Ascan ( _HMG_aFormHandles, <p>.Handle ) ]
#translate <p:Application,App>.BackColor := <arg> => _SetWindowBackColor ( <p>.Handle, <arg> )
#translate <p:Application,App>.Topmost         => GetProperty ( <p>.FormName, 'Topmost' )
#translate <p:Application,App>.Topmost := <arg> => SetProperty ( <p>.FormName, 'Topmost', <arg> )
#translate <p:Application,App>.HelpButton      => GetProperty ( <p>.FormName, 'HelpButton' )
#translate <p:Application,App>.HelpButton := <arg> => SetProperty ( <p>.FormName, 'HelpButton', <arg> )
#translate <p:Application,App>.WindowStyle     => GetWindowStyle ( <p>.Handle )
#translate <p:Application,App>.WindowStyle := <arg> => SetWindowStyle ( <p>.Handle, <arg>, .T. )
#translate <p:Application,App>.Object          => oDlu2Pixel()
#translate <p:Application,App>.Object := <arg> => oDlu2Pixel ( <arg> )

/////////////////////////////////////////////
// Application Cargo support
/////////////////////////////////////////////

#translate <p:Application,App,Sys>.Cargo       => _HMG_MainCargo
#xtranslate _GetAppCargo ()                    => _HMG_MainCargo

/////////////////////////////////////////////
// System pseudo-properties
/////////////////////////////////////////////

#translate <p:System,Sys>.Clipboard          => RetrieveTextFromClipboard()
#translate <p:System,Sys>.Clipboard := <arg> => CopyToClipboard ( <arg> )
#translate <p:System,Sys>.DesktopWidth       => GetDesktopWidth ()
#translate <p:System,Sys>.DesktopHeight      => GetDesktopHeight()
#translate <p:System,Sys>.ClientRow          => GetDesktopArea() \[2]
#translate <p:System,Sys>.ClientCol          => GetDesktopArea() \[1]
#translate <p:System,Sys>.ClientWidth        => GetDesktopRealWidth ()
#translate <p:System,Sys>.ClientHeight       => GetDesktopRealHeight()
#translate <p:System,Sys>.DefaultPrinter     => GetDefaultPrinter()

#translate <p:System,Sys>.DesktopFolder      => GetDesktopFolder()
#translate <p:System,Sys>.MyDocumentsFolder  => GetMyDocumentsFolder()
#translate <p:System,Sys>.ProgramFilesFolder => GetProgramFilesFolder()
#translate <p:System,Sys>.SystemFolder       => GetSystemFolder()
#translate <p:System,Sys>.TempFolder         => GetTempFolder()
#translate <p:System,Sys>.UserTempFolder     => GetUserTempFolder()
#translate <p:System,Sys>.WindowsFolder      => GetWindowsFolder()

#translate <p:System,Sys>.OkSound            => PlayOk()
#translate <p:System,Sys>.HandSound          => PlayHand()
#translate <p:System,Sys>.QuestionSound      => PlayQuestion()
#translate <p:System,Sys>.ExclamationSound   => PlayExclamation()
#translate <p:System,Sys>.AsteriskSound      => PlayAsterisk()
#translate <p:System,Sys>.BeepSound          => PlayBeep()
