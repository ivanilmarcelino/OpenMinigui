#include "hmg.ch"

// SpVoice Flags
#define  SVSFDefault            0
#define  SVSFlagsAsync          1  // SVSFlagsAsync Specifies that the Speak call should be asynchronous.
                                   // That is, it will return immediately after the speak request is queued.

STATIC oNarrator, cDescription, cNameVoice, cGender, cAge, cLanguage, cVendor

FUNCTION Main()

   LOCAL aImage[ 2 ]

   aImage[ 1 ] := 'Female.bmp' // 0
   aImage[ 2 ] := 'Male.bmp'   // 1
   oNarrator := CreateObject( "Sapi.SPVoice", "WithEvents" )

   LOAD WINDOW MAIN
   CENTER WINDOW MAIN
   ACTIVATE WINDOW MAIN

RETURN NIL

FUNCTION GetVoices()

   LOCAL voices := oNarrator:GetVoices() // The GetVoices method returns a selection of available to the voice.
   LOCAL Voice, nIndex, nImageIndex

   FOR nIndex := 0 TO voices:Count - 1
      Voice := voices:Item( nIndex )
      cDescription := Voice:GetDescription
      cNameVoice := Voice:GetAttribute( "Name" )
      cGender := Voice:GetAttribute( "Gender" )
      cAge := Voice:GetAttribute( "Age" )
      cLanguage := Voice:GetAttribute( "Language" )
      cVendor := Voice:GetAttribute( "Vendor" )
      DoMethod ( "Main", "List_1", 'AddItem', cDescription )
      nImageIndex := iif( cGender == "Female", 0, 1 )
      DoMethod ( "Main", "Grid_1", 'AddItem', { nImageIndex, cNameVoice, cGender, cAge, cLanguage, cVendor } )
      DoMethod ( "Main", "List_1", 'SetFocus' )
   NEXT

RETURN NIL

FUNCTION ChangeVoice() // cambiar voz desde un doble click sobre la lista de narradores

   LOCAL nGrid

   nGrid := MAIN.List_1.VALUE // Select narrator (double click)
   cDescription := MAIN.List_1.Item( nGrid )
   oNarrator:voice := oNarrator:GetVoices():Item( nGrid - 1 )
   oNarrator:Speak( cDescription, SVSFlagsAsync )

RETURN NIL

FUNCTION SetVol()

   oNarrator:Volume := MAIN.Slider_1.VALUE
   oNarrator:Speak( "volume is " + hb_ntos( MAIN.Slider_1.Value ) + "%", SVSFlagsAsync )

RETURN NIL
