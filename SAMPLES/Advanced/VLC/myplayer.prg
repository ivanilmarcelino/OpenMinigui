/*
 * VideoLan VLC ActiveX demo
 *
 * Author: Carlos Vargas <cvargaz[at]donboscocorp.com>
 */

#include "minigui.ch"
#include "hbclass.ch"

/*---------------------------------------------------------------------------------------------*/

#define VLC_INPUT_STATE_IDLE      0
#define VLC_INPUT_STATE_OPENING   1
#define VLC_INPUT_STATE_BUFFERING 2
#define VLC_INPUT_STATE_PLAYING   3
#define VLC_INPUT_STATE_PAUSED    4
#define VLC_INPUT_STATE_STOPPING  5
#define VLC_INPUT_STATE_ENDED     6
#define VLC_INPUT_STATE_ERROR     7

#define E_MediaPlayerNothingSpecial      200
#define E_MediaPlayerOpening             201
#define E_MediaPlayerBuffering           202
#define E_MediaPlayerPlaying             203
#define E_MediaPlayerPaused              204
#define E_MediaPlayerForward             205
#define E_MediaPlayerBackward            206
#define E_MediaPlayerEncounteredError    207
#define E_MediaPlayerEndReached          208
#define E_MediaPlayerStopped             209
#define E_MediaPlayerStopAsyncDone       222
#define E_MediaPlayerTimeChanged         210
#define E_MediaPlayerPositionChanged     211
#define E_MediaPlayerSeekableChanged     212
#define E_MediaPlayerPausableChanged     213
#define E_MediaPlayerMediaChanged        214
#define E_MediaPlayerTitleChanged        215
#define E_MediaPlayerLengthChanged       216
#define E_MediaPlayerChapterChanged      217
#define E_MediaPlayerVout                218
#define E_MediaPlayerMuted               219
#define E_MediaPlayerUnmuted             220
#define E_MediaPlayerAudioVolume         221

/*---------------------------------------------------------------------------------------------*/

STATIC oVlc
STATIC lFirstRun := .T.

/*---------------------------------------------------------------------------------------------*/

PROCEDURE Main()

   SET GLOBAL HOTKEYS ON

   DEFINE WINDOW Win1 ;
      AT 0, 0 WIDTH 800 HEIGHT 600 ;
      TITLE "MyPlayer" ;
      MAIN ;
      ICON "MYPLAYER" ;
      NOMAXIMIZE ;
      ON INIT MP_Init()

      DEFINE ACTIVEX Activex1
         ROW 10
         COL 10
         WIDTH 600
         HEIGHT 480
         PROGID "VideoLAN.VLCPlugin.2"
         EVENTMAP { { E_MediaPlayerTimeChanged, "VLC_OnTimeChanged" }, ;
                    { E_MediaPlayerPositionChanged, "VLC_OnPositionChanged" } }
      END ACTIVEX

      @ 010, 010 LABEL Logo1 VALUE "" WIDTH 600 HEIGHT 480 BACKCOLOR BLACK
      @ 230, 300 IMAGE Logo2 PICTURE "VLC" WIDTH 32 HEIGHT 32

      @ 010, 620 BUTTON BtnLoad CAPTION "Open File" ACTION MOpenFile() WIDTH 80

      @ 040, 620 BUTTON BtnPlay CAPTION "Play" ACTION oVlc:Play() WIDTH 80
      @ 070, 620 BUTTON BtnPaus CAPTION "Pause" ACTION iif( oVlc:IsPlaying(), ;
         oVlc:Pause(), iif( oVlc:State() == VLC_INPUT_STATE_PAUSED, oVlc:Play(), ) ) WIDTH 80
      @ 100, 620 BUTTON BtnStop CAPTION "Stop" ACTION oVlc:Stop() WIDTH 80

      @ 150, 620 BUTTON BtnVolI CAPTION "Vol +" ACTION oVlc:VolumenPlus() WIDTH 80
      @ 180, 620 BUTTON BtnVolD CAPTION "Vol -" ACTION oVlc:VolumenMinus() WIDTH 80

      @ 220, 616 SLIDER Slider1 RANGE 1, 20 VALUE 10 WIDTH 92 HEIGHT 020 BOTH NOTICKS NOTABSTOP ;
         ON CHANGE {|| oVlc:Volume( this.Value * 5 ), iif( oVlc:IsPlaying(), , VLC_OnTimeChanged() ) } ;
         ON SCROLL {|| oVlc:Volume( this.Value * 5 ), iif( oVlc:IsPlaying(), , VLC_OnTimeChanged() ) }

      @ 250, 620 CHECKBUTTON BtnMute CAPTION "Mute" WIDTH 80 ON CHANGE toggleMute( .T. )
      @ 280, 620 CHECKBUTTON BtnUnMt CAPTION "UnMute" VALUE .T. WIDTH 80 ON CHANGE toggleMute( .F. )

      @ 495, 010 LABEL Info1 VALUE "" WIDTH 600 HEIGHT 038 FONTCOLOR BLUE BACKCOLOR PINK ;
         CENTERALIGN VCENTERALIGN BORDER SIZE 12
      @ 535, 010 LABEL Info2 VALUE "" WIDTH 600 HEIGHT 020 FONTCOLOR BLUE BACKCOLOR PINK CENTERALIGN BORDER

      ON KEY F11 ACTION oVlc:FullScreen()

   END WINDOW

   CENTER WINDOW Win1

   ACTIVATE WINDOW Win1

RETURN

/*---------------------------------------------------------------------------------------------*/

PROCEDURE MP_Init()

   LOCAL oActiveX

   oActiveX := GetControlId( "Activex1", "Win1" )
   oActiveX:Refresh()

   // create VLC object
   oVlc := TVlc():New( Win1.Activex1.XObject )
   Win1.Title := This.Title + " is based upon VLC " + oVlc:GetVersion()
   oVlc:bOnSetFile := {|| VLC_OnSelFile() }
   oVlc:bOnVolumePlus := {|| VLC_OnVolumeChanged() }
   oVlc:bOnVolumeMinus := {|| VLC_OnVolumeChanged() }
   oVlc:bOnMuted := {|| VLC_OnMuteChanged( .F. ) }
   oVlc:bOnUnMuted := {|| VLC_OnMuteChanged( .T. ) }
   oVlc:StepVolume( 5 )
   oVlc:Stop()

RETURN

/*---------------------------------------------------------------------------------------------*/

PROCEDURE VLC_OnSelFile()

   IF Win1.BtnMute.Value == .T.
      Win1.BtnUnMt.Value := ! Win1.BtnUnMt.Value
      oVlc:Mute( .F. )
   ENDIF
   Win1.Info1.Value := oVlc:cFileName

RETURN

/*---------------------------------------------------------------------------------------------*/

PROCEDURE VLC_OnVolumeChanged()

   Win1.Slider1.Value := oVlc:Volume() / 5
   if ! oVlc:IsPlaying()
      VLC_OnTimeChanged()
   ENDIF

RETURN

/*---------------------------------------------------------------------------------------------*/

PROCEDURE VLC_OnTimeChanged()

   Win1.Info2.Value := "Length: " + oVlc:GetLengthStr() + ", nPos: " + oVlc:GetTimeStr() + ", Volume: " + StrZero( oVlc:Volume(), 3 )

RETURN

/*---------------------------------------------------------------------------------------------*/

PROCEDURE VLC_OnPositionChanged()

   IF oVlc:GetTimeStr() == "00:00:30"
      oVlc:LogoOff()
      oVlc:MarqueeOff()
   ENDIF

RETURN

/*---------------------------------------------------------------------------------------------*/

PROCEDURE VLC_OnMuteChanged( lMute )

   Win1.BtnVolI.Enabled := lMute
   Win1.BtnVolD.Enabled := lMute
   Win1.Slider1.Enabled := lMute

RETURN

/*---------------------------------------------------------------------------------------------*/

PROCEDURE MOpenFile()

   LOCAL cFile, nIndex := 1

   IF oVlc:IsPlaying() .OR. oVlc:State() == VLC_INPUT_STATE_PAUSED
      RETURN
   ENDIF

   cFile := Getfile ( { { 'Video', '*.avi;*.flv;*.mp4;*.mpg;*.mpeg4;*.mkv' }, { 'Music', '*.mp3;*.wav;*.ogg' } }, 'Open Media', , , , @nIndex )
   IF ! Empty( cFile )
      IF lFirstRun
         Win1.Logo1.Hide()
         Win1.Logo2.Hide()
         lFirstRun := .F.
      ENDIF
      oVlc:SetFile( cFile )
      IF nIndex == 1
         oVlc:Logo( GetStartupFolder() + "\Video-48.png", "top-right" )
         oVlc:Marquee( "vlc " + SubStr( oVlc:GetVersion(), 1, 6 ), "bottom-right", , , 0x00FF00 )
      ELSE
         oVlc:LogoOff()
         oVlc:MarqueeOff()
      ENDIF
      Win1.BtnPlay.SetFocus()
   ENDIF

RETURN

/*---------------------------------------------------------------------------------------------*/

PROCEDURE toggleMute( lMute )

   oVlc:Mute( lMute )
   IF lMute
      Win1.BtnUnMt.Value := ! Win1.BtnUnMt.Value
   ELSE
      Win1.BtnMute.Value := ! Win1.BtnMute.Value
   ENDIF

RETURN

/*---------------------------------------------------------------------------------------------*/

CLASS TVlc

   HIDDEN:
   DATA oControl, oPlayList, oInput, oVideo, oAudio
   DATA nStepVolume INIT 5
   DATA nVolume INIT 50

   EXPORTED:
   DATA cFileName
   DATA bOnSetFile
   DATA bOnPlay, bOnPause, bOnStop, bOnEnd
   DATA bOnVolumePlus, bOnVolumeMinus, bOnVolumeChange
   DATA bOnMuted, bOnUnMuted
   DATA bOnFullScreen

   METHOD New( oControl ) CONSTRUCTOR
   METHOD GetVersion() INLINE ::oControl:getVersionInfo()
   METHOD IsPlaying() INLINE ::oPlaylist:IsPlaying()
   METHOD Pause() INLINE iif( ::IsPlaying(), ::oPlaylist:pause(), NIL )
   METHOD Stop() INLINE iif( ::IsPlaying(), ( ::oPlaylist:stop(), VLC_OnTimeChanged() ), NIL )
   METHOD Play()
   METHOD Mute( lSet ) INLINE iif( HB_ISLOGICAL( lSet ), ( ::oAudio:mute := lSet, Eval( iif( lSet, ::bOnMuted, ::bOnUnMuted ) ) ), NIL )
   METHOD State() INLINE ::oInput:state()
   METHOD HasVout() INLINE ::oInput:hasVout()
   METHOD GetLength() INLINE ::oInput:length()
   METHOD GetLengthStr() INLINE ::MiliSec2Time( ::oInput:length() )
   METHOD GetTime() INLINE ::oInput:time()
   METHOD GetTimeStr() INLINE ::MiliSec2Time( ::oInput:time() )
   METHOD Position( nPosition )
   METHOD FullScreen() INLINE ::oVideo:toggleFullscreen()
   METHOD Volume( nVolume )
   METHOD VolumenPlus()
   METHOD VolumenMinus()
   METHOD StepVolume( nStep ) INLINE iif( HB_ISNUMERIC( nStep ) .AND. ( nStep > 1 .AND. nStep < 10 ), ( ::nStepVolume := nStep ), NIL )
   METHOD SetFile( cFileName )
   METHOD Time2MiliSec( cTime )
   METHOD MiliSec2Time( nMiliSec )
   METHOD Marquee( cText, cPos, nTimeout, nSize, nColor )
   METHOD MarqueeOff() INLINE ::oVideo:marquee:disable()
   METHOD Logo( cPng, cPos )
   METHOD LogoOff() INLINE ::oVideo:logo:disable()

ENDCLASS

/*---------------------------------------------------------------------------------------------*/

METHOD New( oControl ) CLASS TVlc

   IF HB_ISOBJECT( oControl )

      ::oControl := oControl

      TRY
         ::oPlaylist := oControl:playlist
      CATCH
         MsgStop( 'This Program Required Installed ' + hb_ntos( hb_Version( HB_VERSION_BITWIDTH ) ) + '-bit AXVLC.DLL!', 'Stop' )
         QUIT
      END

      ::oInput := oControl:input
      ::oVideo := oControl:video
      ::oAudio := oControl:audio

      ::oControl:Toolbar := FALSE

      ::Volume( ::nVolume )

      ::bOnSetFile := NIL
      ::bOnPlay := NIL
      ::bOnPause := NIL
      ::bOnStop := NIL
      ::bOnEnd := NIL
      ::bOnMuted := {|| NIL }
      ::bOnUnMuted := {|| NIL }
      ::bOnVolumeChange := NIL
      ::bOnVolumePlus := NIL
      ::bOnVolumeMinus := NIL
      ::bOnFullScreen := NIL

   ENDIF

RETURN Self

/*---------------------------------------------------------------------------------------------*/

METHOD SetFile( cFileName ) CLASS TVlc

   LOCAL lRet := FALSE

   ::cFileName := RTrim( cFileName )

   IF ! Empty( ::cFileName )
      IF At( " ", ::cFileName ) > 0
         ::cFileName := _GetShortPathName( ::cFileName )
      ENDIF
      ::oPlaylist:items:clear()
      ::oPlaylist:add( "File:///" + ::cFileName )
      ::oInput:position := 0
      IF HB_ISBLOCK( ::bOnSetFile )
         Eval( ::bOnSetFile, Self, cFileName )
      ENDIF
      lRet := TRUE
   ENDIF

RETURN lRet

/*---------------------------------------------------------------------------------------------*/

METHOD Play() CLASS TVlc

   IF ! ::IsPlaying() .AND. ::oPlaylist:items:count() > 0
      IF ::State() == VLC_INPUT_STATE_PAUSED
         ::oPlaylist:play()
      ELSE
         ::oPlaylist:playitem( 0 )
      ENDIF
   ENDIF

RETURN NIL


/*---------------------------------------------------------------------------------------------*/

METHOD Volume( nVolume ) CLASS TVlc

   IF HB_ISNIL( nVolume )
      RETURN ::oControl:volume
   ELSE
      IF nVolume > 0 .AND. nVolume <= 100
         ::nVolume := nVolume
         ::oControl:volume := ::nVolume
      ENDIF
   ENDIF

RETURN NIL

/*---------------------------------------------------------------------------------------------*/

METHOD Position( nPosition ) CLASS TVlc

   IF HB_ISNIL( nPosition )
      RETURN ::oControl:input:position
   ELSE
      IF nPosition >= 0 .AND. nPosition <= 1
         ::oControl:input:position := nPosition
      ENDIF
   ENDIF

RETURN NIL

/*---------------------------------------------------------------------------------------------*/

METHOD VolumenPlus() CLASS TVlc

   IF ::nVolume < 100
      ::nVolume := ::nVolume + ::nStepVolume
      IF ::nVolume > 100
         ::nVolume := 100
      ENDIF
      ::Volume( ::nVolume )
      IF HB_ISBLOCK( ::bOnVolumePlus )
         Eval( ::bOnVolumePlus, Self )
      ENDIF
   ENDIF

RETURN NIL

/*---------------------------------------------------------------------------------------------*/

METHOD VolumenMinus() CLASS TVlc

   IF ::nVolume > 0
      ::nVolume := ::nVolume - ::nStepVolume
      IF ::nVolume < 1
         ::nVolume := 1
      ENDIF
      ::Volume( ::nVolume )
      IF HB_ISBLOCK( ::bOnVolumeMinus )
         Eval( ::bOnVolumeMinus, Self )
      ENDIF
   ENDIF

RETURN NIL

/*---------------------------------------------------------------------------------------------*/

METHOD Marquee( cText, cPos, nTimeout, nSize, nColor ) CLASS TVlc

   LOCAL nAt := 1, acPos := { "center", "left", "right", "top", "top-left", "top-right", "bottom", "bottom-left", "bottom-right" }

   IF HB_ISSTRING( cText )
      ::oVideo:marquee:text := cText
      IF HB_ISSTRING( cPos )
         nAt := AScan( acPos, {| i | i == cPos } )
      ENDIF
      IF HB_ISNUMERIC( nTimeout )
         ::oVideo:marquee:timeout := nTimeout
      ENDIF
      IF HB_ISNUMERIC( nSize )
         ::oVideo:marquee:size := nSize
      ENDIF
      IF HB_ISNUMERIC( nColor )
         ::oVideo:marquee:color := nColor
      ENDIF
      ::oVideo:marquee:position := acPos[ nAt ]
      ::oVideo:marquee:enable()
   ENDIF

RETURN NIL

/*---------------------------------------------------------------------------------------------*/

METHOD Logo( cPng, cPos ) CLASS TVlc

   LOCAL nAt := 1, acPos := { "center", "left", "right", "top", "top-left", "top-right", "bottom", "bottom-left", "bottom-right" }

   IF HB_ISSTRING( cPng )
      ::oVideo:logo:file( cPng )
      IF HB_ISSTRING( cPos )
         nAt := AScan( acPos, {| i | i == cPos } )
      ENDIF
      ::oVideo:logo:position := acPos[ nAt ]
      ::oVideo:logo:enable()
   ENDIF

RETURN NIL

/*---------------------------------------------------------------------------------------------*/

METHOD Time2MiliSec( cTime ) CLASS TVlc

   LOCAL nMiliSec := 0
   LOCAL nH, nM, nS, nT

   IF ! Empty( cTime )
      nH = Val( SubStr( cTime, 1, 2 ) )
      nM = Val( SubStr( cTime, 4, 2 ) )
      nS = Val( SubStr( cTime, 7, 2 ) )
      nT = ( nH * 3600 ) + ( nM * 60 ) + nS
      nMiliSec := nT * 1000
   ENDIF

RETURN nMiliSec

/*---------------------------------------------------------------------------------------------*/

METHOD MiliSec2Time( nMiliSec ) CLASS TVlc

   LOCAL cTime := "00:00:00"
   LOCAL nHora, nMinuto, nSegundo

   IF nMiliSec > 0
      nMiliSec := Int( nMiliSec / 1000 )
      nHora := Int( nMiliSec / 3600 )
      nMiliSec := nMiliSec - ( nHora * 3600 )
      nMinuto := Int( nMiliSec / 60 )
      nSegundo := nMiliSec - ( nMinuto * 60 )
      cTime := StrZero( nHora, 2 ) + ":" + StrZero( nMinuto, 2 ) + ":" + StrZero( nSegundo, 2 )
   ENDIF

RETURN cTime

/*---------------------------------------------------------------------------------------------*/
// EOF
/*---------------------------------------------------------------------------------------------*/
