/*
  MINIGUI - Harbour Win32 GUI library Demo

  Author: Siri Rathinagiri <srgiri@dataone.in>

  This version of Circle Magic has the following features:

  - Double buffering as suggested by Claudio
  - Animation ON/OFF
  - Stereo 3D images ON/OFF & Flexible Distance
  - Output Images of flexible size (up to 4000 x 4000 pixels) (made use of Panel window)
  - Saving the image in bmp format
  - Importing and exporting parameters in .ini files (To create again the same design)
  - Random design creation
  - Progress bar
  - Stop intermediately

  Revised by Grigory Filatov <gfilatov@inbox.ru>
*/

#include <hmg.ch>

memvar lCancel, hBitmap

Function Main
   public lCancel := .f.
   public hBitmap := 0

   set font to 'Arial', 9

   define window cm;
      width 800 height 600;
      main title 'Circle Magic';
      on paint    ProcOnPaint();
      on release  BT_BitmapRelease (hBitmap);
      on init drawrandom()

      define button draw
         row 10
         col 10
         caption 'Draw'
         width 50
         action Draw()
      end button
      define button save
         row 40
         col 10
         caption 'Save'
         width 50
         action savebitmap()
      end button
      define button random
         row 70
         col 10
         caption 'Random'
         width 50
         action drawrandom()
      end button
      define label innerrlabel
         row 15
         col 70
         width 90
         value 'Inner Planet R'
      end label
      define spinner innerr
         row 10
         col 160
         width 60
         rangemin 5
         rangemax 1800
         increment 5
         value 80
      end spinner
      define label outerrlabel
         row 15
         col 230
         width 90
         value 'Outer Planet R'
      end label
      define spinner outerr
         row 10
         col 320
         width 60
         rangemin 5
         rangemax 2000
         increment 5
         value 110
      end spinner
      define label backcolorlabel
         row 15
         col 390
         width 90
         value 'Back Color'
      end label   
      define label backcolor
         row 15
         col 480
         width 50
         height 20
         backcolor { 0, 0, 0 }
         onclick getbackcolor()
      end label
      define checkbox animationspeed
         row 15
         col 540
         width 100
         caption 'Animation'
         value .t.
      end checkbox
      define spinner anispeed
         row 10
         col 640
         width 80
         rangemin 1000
         rangemax 100000
         value 50000
         increment 5000
      end spinner
      define label innerspeed
         row 45
         col 70
         width 90
         value 'Inner Speed'
      end label
      define spinner speed
         row 40
         col 160
         width 60
         rangemin 1
         rangemax 50
         value 20
         increment 1
      end spinner
      define label outerspeed
         row 45
         col 230
         width 90
         value 'Outer Speed'
      end label
      define spinner ospeed
         row 40
         col 320
         width 60
         rangemin 1
         rangemax 50
         value 3
         increment 1
      end spinner
      define label revolutionslabel
         row 45
         col 390
         width 90
         value 'Revolutions'
      end label
      define spinner revolutions
         row 40
         col 480
         width 60
         rangemin 1
         rangemax 50
         value 1
         increment 1
      end spinner
      define checkbox stereo
         row 40
         col 550
         width 70
         caption 'Stereo'
         value .f.
         ON CHANGE BT_ClientAreaInvalidateAll ("cm")
      end checkbox
      define spinner base
         row 40
         col 640
         width 80
         rangemin 2
         rangemax 50
         value 20
         increment 2
      end spinner
      define label imagesizelabel
         row 75
         col 70
         width 90
         value 'Image Height'
      end label
      define spinner imageheight
         row 70
         col 160
         width 60
         rangemin 400
         rangemax 3000
         increment 5
         value 400
      end spinner
      define label pencolorlabel
         row 75
         col 230
         width 30
         value 'Pen '
      end label   
      define spinner penwidth
         row 70
         col 265
         width 55
         value 1
         rangemin 1
         rangemax 5
         increment 1
      end spinner
      define label pencolor
         row 70
         col 320
         width 60
         height 20
         backcolor { 255, 0, 0 }         
         onclick getpencolor()
      end label 
      define checkbox randomcolor
         row 70
         col 390
         width 90
         caption 'Use Random'
         value .t.
      end checkbox         
      define checkbox singlecolor
         row 70
         col 490
         width 90
         caption 'Single Color'
         value .t.
      end checkbox  
      define progressbar progress
         row 70
         col 640
         width 80
         smooth .t.
      end progressbar         
      define button open
         row 10
         col 730
         caption 'Import'
         width 50
         action importparameters()
      end button
      define button parameters
         row 40
         col 730
         caption 'Export'
         width 50
         action exportparameters()
      end button
      define button stop
         row 70
         col 730
         caption 'Stop'
         width 50
         action ( lCancel := .t. )
      end button

      define window image at 100, 10 width cm.width - 30 height cm.height - 180 virtual width 4000 virtual height 4000 panel on paint redraw()
      end window      

   end window
   cm.progress.value := 0
   cm.maximize
   cm.activate
Return nil


function drawstereo
   LOCAL hDC, BTstruct
   LOCAL hDC2, BTstruct2
   local nSmallRadius := cm.innerr.value
   local nBigRadius := cm.outerr.value
   local nBigRevolutions := cm.revolutions.value
   local nInnerSpeed := cm.speed.value
   local nOuterSpeed := cm.ospeed.value
   local nImageHeight := cm.imageheight.value
   local nCenterCol := nImageHeight + 25
   local nCenterRow := int( nImageHeight / 2 )
   local nSmallRow := 0
   local nSmallCol := 0
   local nBigRow := 0
   local nBigCol := 0
   local nDotRadius := 1
   local nSmallDots := 360
   local nBigDots := round( nSmallDots * nBigRadius / nSmallRadius, 0 )
   local aLSmall := {}
   local aLBig := {}
   local aRSmall := {}
   local aRBig := {}
   local nRadians := 0.0174532925
   local nSmallCount := 0
   local nBigCount := 0
   local nMax := int( ( nImageHeight - 20 ) / 2 )
   local nAniSpeed := cm.anispeed.value
   local nStereoBase := cm.base.value
   local nR := random( 255 )
   local nG := random( 255 )
   local nB := random( 255 )
   local lAnimation := cm.animationspeed.value
   local lSingleColor := cm.singlecolor.value
   local nPenWidth := cm.penwidth.value
   local nLSmallCenterCol := nCenterCol - ( ( nImageHeight ) / 2 ) - ( nStereoBase / 2 ) - 25
   local nRSmallCenterCol := nCenterCol + ( ( nImageHeight ) / 2 ) + ( nStereoBase / 2 ) + 25
   local nLBigCenterCol := nCenterCol - ( ( nImageHeight ) / 2 ) + ( nStereoBase / 2 ) - 25
   local nRBigCenterCol := nCenterCol + ( ( nImageHeight ) / 2 ) - ( nStereoBase / 2 ) + 25
   local i, j, nLSmallCol, nRSmallCol, nLBigCol, nRBigCol, aRGB, nCount

   if nSmallRadius < 10
      nSmallRadius := 10
      cm.innerr.value := nSmallRadius
   endif   
   if nBigRadius > nMax
      nBigRadius := nMax
      cm.outerr.value := nBigRadius
   endif
   if nSmallRadius > nBigRadius + 10
      nSmallRadius := nBigRadius - 10
      cm.innerr.value := nSmallRadius
   endif
   if nBigRadius <= nSmallRadius + 10
      nBigRadius := nSmallRadius + 10
      cm.outerr.value := nBigRadius
   endif
   BT_ClientAreaInvalidateAll ("image")
   hBitmap := BT_BitmapCreateNew ( ( nImageHeight * 2 ) + 50, nImageHeight, cm.backcolor.backcolor)
   hDC  := BT_CreateDC ( "image", BT_HDC_ALLCLIENTAREA, @BTstruct )
   hDC2 := BT_CreateDC ( hBitmap, BT_HDC_BITMAP, @BTstruct2 )
   BT_DrawFillRectangle (hDC, 0, 0, getproperty( 'image', 'WIDTH' ) * 2, getproperty( 'image', 'HEIGHT' ), cm.backcolor.backcolor, cm.backcolor.backcolor, 1)
   for i := 1 to nSmallDots
      nSmallRow := nCenterRow + nSmallRadius * Cos( i / ( nSmallDots / 360 ) * nRadians )
      nSmallCol := nCenterCol + nSmallRadius * Sin( i / ( nSmallDots / 360 ) * nRadians )
      nLSmallCol := nLSmallCenterCol + nSmallRadius * Sin( i / ( nSmallDots / 360 ) * nRadians )
      nRSmallCol := nRSmallCenterCol + nSmallRadius * Sin( i / ( nSmallDots / 360 ) * nRadians )
      aadd( aLSmall, { nSmallRow, nLSmallCol } )
      aadd( aRSmall, { nSmallRow, nRSmallCol } )
      BT_DrawEllipse ( hDC, nSmallRow, nLSmallCol, nDotRadius, nDotRadius, { 0, 0, 0 }, nPenWidth )
      BT_DrawEllipse ( hDC, nSmallRow, nRSmallCol, nDotRadius, nDotRadius, { 0, 0, 0 }, nPenWidth )
      BT_DrawEllipse ( hDC2, nSmallRow, nLSmallCol, nDotRadius, nDotRadius, { 0, 0, 0 }, nPenWidth )
      BT_DrawEllipse ( hDC2, nSmallRow, nRSmallCol, nDotRadius, nDotRadius, { 0, 0, 0 }, nPenWidth )
   next i   
   for i := 1 to nBigDots
      nBigCol := nCenterCol + nBigRadius * Sin( i / ( nBigDots / 360 ) * nRadians )
      nBigRow := nCenterRow + nBigRadius * Cos( i / ( nBigDots / 360 ) * nRadians )
      nLBigCol := nLBigCenterCol + nBigRadius * Sin( i / ( nBigDots / 360 ) * nRadians )
      nRBigCol := nRBigCenterCol + nBigRadius * Sin( i / ( nBigDots / 360 ) * nRadians )
      aadd( aLBig, { nBigRow, nLBigCol } )
      aadd( aRBig, { nBigRow, nRBigCol } )
      BT_DrawEllipse ( hDC, nBigRow, nLBigCol, nDotRadius, nDotRadius, { 0, 0, 0 }, nPenWidth )
      BT_DrawEllipse ( hDC, nBigRow, nRBigCol, nDotRadius, nDotRadius, { 0, 0, 0 }, nPenWidth )
      BT_DrawEllipse ( hDC2, nBigRow, nLBigCol, nDotRadius, nDotRadius, { 0, 0, 0 }, nPenWidth )
      BT_DrawEllipse ( hDC2, nBigRow, nRBigCol, nDotRadius, nDotRadius, { 0, 0, 0 }, nPenWidth )
   next i
   nSmallCount := 1
   nBigCount := 1
   if cm.singlecolor.value
      if cm.randomcolor.value
         nR := random( 255 )
         nG := random( 255 )
         nB := random( 255 )
      else
         aRGB := cm.pencolor.backcolor
         nR := aRGB[ 1 ]     
         nG := aRGB[ 2 ]
         nB := aRGB[ 3 ]
      endif
   endif
   nCount := int( nBigDots * nBigRevolutions / nOuterSpeed )
   for i := 1 to  nCount
      cm.progress.value := ( i / nCount ) * 100
      nBigCount := nBigCount + nOuterSpeed
      nSmallCount := nSmallCount + nInnerSpeed
      if nSmallCount > nSmallDots
         nSmallCount := nSmallCount - nSmallDots
      endif
      if nBigCount > nBigDots
         nBigCount := nBigCount - nBigDots
      endif
      if .not. lSingleColor
         nR := random( 255 )
         nG := random( 255 )
         nB := random( 255 )
      endif
      nSmallRow := aLSmall[ nSmallCount, 1 ]
      nLSmallCol := aLSmall[ nSmallCount, 2 ]
      nRSmallCol := aRSmall[ nSmallCount, 2 ]
      nBigRow := aLBig[ nBigCount, 1 ]
      nLBigCol := aLBig[ nBigCount, 2 ]
      nRBigCol := aRBig[ nBigCount, 2 ]
      BT_DrawLine ( hDC, nSmallRow, nLSmallCol, nBigRow, nLBigCol, { nR, nG, nB }, nPenWidth )
      BT_DrawLine ( hDC, nSmallRow, nRSmallCol, nBigRow, nRBigCol, { nR, nG, nB }, nPenWidth )
      BT_DrawLine ( hDC2, nSmallRow, nLSmallCol, nBigRow, nLBigCol, { nR, nG, nB }, nPenWidth )
      BT_DrawLine ( hDC2, nSmallRow, nRSmallCol, nBigRow, nRBigCol, { nR, nG, nB }, nPenWidth )
      if lAnimation
         for j := 1 to nAniSpeed
         next j
      endif   
      Do Events
      if lCancel
         lCancel := .f.
         exit
      endif
   next i
   BT_DeleteDC (BTstruct)
   BT_DeleteDC (BTstruct2)
return nil


PROCEDURE ProcOnPaint
   LOCAL  hDC, BTstruct

   setproperty( 'image', 'WIDTH', cm.width - 30 )
   if cm.height > 180
      setproperty( 'image', 'HEIGHT', cm.height - 180 )
   endif   
   hDC := BT_CreateDC ( "image", BT_HDC_INVALIDCLIENTAREA, @BTstruct )
   BT_DrawFillRectangle ( hDC, 0, 0, getproperty( 'image', 'WIDTH' ), getproperty( 'image', 'HEIGHT' ), cm.backcolor.backcolor, cm.backcolor.backcolor, 1)
   BT_DrawBitmapTransparent (hDC, 0, 0, NIL, NIL, BT_COPY, hBitmap, NIL)
   BT_DeleteDC (BTstruct)
RETURN


FUNCTION GetBackColor
   local aRGB := cm.backcolor.backcolor
   local aNewRGB := aclone( aRGB )

   aNewRGB := getcolor( aRGB )   
   if aNewRGB[ 1 ] <> Nil
      cm.backcolor.backcolor := aNewRGB
   endif
   BT_ClientAreaInvalidateAll ("cm")
RETURN NIL


FUNCTION Draw
   LOCAL hDC, BTstruct
   LOCAL hDC2, BTstruct2
   local nSmallRadius := cm.innerr.value
   local nBigRadius := cm.outerr.value
   local nBigRevolutions := cm.revolutions.value
   local nInnerSpeed := cm.speed.value
   local nOuterSpeed := cm.ospeed.value
   local nImageHeight := cm.imageheight.value
   local nCenterCol := int( nImageHeight / 2 )
   local nCenterRow := int( nImageHeight / 2 )
   local nSmallRow := 0
   local nSmallCol := 0
   local nBigRow := 0
   local nBigCol := 0
   local nDotRadius := 1
   local nSmallDots := 360
   local nBigDots := round( nSmallDots * nBigRadius / nSmallRadius, 0 )
   local aSmall := {}
   local aBig := {}
   local nRadians := 0.0174532925
   local nSmallCount := 0
   local nBigCount := 0
   local nMax := int( ( nImageHeight - 20 ) / 2 )
   local nAniSpeed := cm.anispeed.value
   local nR := 255
   local nG := 255
   local nB := 255
   local lAnimation := cm.animationspeed.value
   local lSingleColor := cm.singlecolor.value
   local nPenWidth := cm.penwidth.value
   local i, j, nCount, aRGB

   if cm.stereo.value
      drawstereo()
      return nil
   endif
   if nSmallRadius < 10
      nSmallRadius := 10
      cm.innerr.value := nSmallRadius
   endif   
   if nBigRadius > nMax
      nBigRadius := nMax
      cm.outerr.value := nBigRadius
   endif
   if nSmallRadius > nBigRadius + 10
      nSmallRadius := nBigRadius - 10
      cm.innerr.value := nSmallRadius
   endif
   if nBigRadius <= nSmallRadius + 10
      nBigRadius := nSmallRadius + 10
      cm.outerr.value := nBigRadius
   endif
   IF hBitmap <> 0
      BT_BitmapRelease (hBitmap)
   ENDIF
   BT_ClientAreaInvalidateAll ("image")
   hBitmap := BT_BitmapCreateNew ( nImageHeight, nImageHeight, cm.backcolor.backcolor)
   hDC  := BT_CreateDC ( "image", BT_HDC_ALLCLIENTAREA, @BTstruct )
   hDC2 := BT_CreateDC ( hBitmap, BT_HDC_BITMAP, @BTstruct2 )
   BT_DrawFillRectangle (hDC, 0, 0, getproperty( 'image', 'WIDTH' ), getproperty( 'image', 'HEIGHT' ), cm.backcolor.backcolor, cm.backcolor.backcolor, 1)
   nSmallDots := 360
   nBigDots := round( nSmallDots * nBigRadius / nSmallRadius, 0 )
   for i := 1 to nSmallDots
      nSmallRow := nCenterRow + nSmallRadius * Cos( i / ( nSmallDots / 360 ) * nRadians )
      nSmallCol := nCenterCol + nSmallRadius * Sin( i / ( nSmallDots / 360 ) * nRadians )
      aadd( aSmall, { nSmallRow, nSmallCol } )
      BT_DrawEllipse ( hDC, nSmallRow, nSmallCol, nDotRadius, nDotRadius, { 0, 0, 0 }, nPenWidth )
      BT_DrawEllipse ( hDC2, nSmallRow, nSmallCol, nDotRadius, nDotRadius, { 0, 0, 0 }, nPenWidth )
   next i   
   for i := 1 to nBigDots
      nBigCol := nCenterCol + nBigRadius * Sin( i / ( nBigDots / 360 ) * nRadians )
      nBigRow := nCenterRow + nBigRadius * Cos( i / ( nBigDots / 360 ) * nRadians )
      aadd( aBig, { nBigRow, nBigCol } )
      BT_DrawEllipse ( hDC, nBigRow, nBigCol, nDotRadius, nDotRadius, { 0, 0, 0 }, nPenWidth )
      BT_DrawEllipse ( hDC2, nBigRow, nBigCol, nDotRadius, nDotRadius, { 0, 0, 0 }, nPenWidth )
   next i
   nSmallCount := 1
   nBigCount := 1
   nCount := int( nBigDots * nBigRevolutions / nOuterSpeed )
   if cm.singlecolor.value
      if cm.randomcolor.value
         nR := random( 255 )
         nG := random( 255 )
         nB := random( 255 )
      else
         aRGB := cm.pencolor.backcolor
         nR := aRGB[ 1 ]     
         nG := aRGB[ 2 ]
         nB := aRGB[ 3 ]
      endif
   endif   
   for i := 1 to  nCount
      cm.progress.value := i/nCount * 100
      nBigCount := nBigCount + nOuterSpeed
      nSmallCount := nSmallCount + nInnerSpeed
      if nSmallCount > nSmallDots
         nSmallCount := nSmallCount - nSmallDots
      endif
      if nBigCount > nBigDots
         nBigCount := nBigCount - nBigDots
      endif
      if .not. lSingleColor
         nR := random( 255 )
         nG := random( 255 )
         nB := random( 255 )
      endif
      nSmallRow := aSmall[ nSmallCount, 1 ]
      nSmallCol := aSmall[ nSmallCount, 2 ]
      nBigRow := aBig[ nBigCount, 1 ]
      nBigCol := aBig[ nBigCount, 2 ]
      BT_DrawLine ( hDC, nSmallRow, nSmallCol, nBigRow, nBigCol, { nR, nG, nB }, nPenWidth )
      BT_DrawLine ( hDC2, nSmallRow, nSmallCol, nBigRow, nBigCol, { nR, nG, nB }, nPenWidth )
      if lAnimation
         for j := 1 to nAniSpeed
         next j
      endif   
      Do Events
      if lCancel
         lCancel := .f.
         exit
      endif
   next i
   cm.progress.value := 100
   BT_DeleteDC (BTstruct)
   BT_DeleteDC (BTstruct2)
RETURN NIL

function getpencolor
   local aRGB := cm.pencolor.backcolor
   local aNewRGB := aclone( aRGB )

   aNewRGB := getcolor( aRGB )   
   if aNewRGB[ 1 ] <> Nil
      cm.pencolor.backcolor := aNewRGB
   endif
return nil


function redraw
   LOCAL  hDC, BTstruct

   hDC := BT_CreateDC ( "image", BT_HDC_INVALIDCLIENTAREA, @BTstruct )
   BT_DrawFillRectangle ( hDC, 0, 0, getproperty( 'image', 'WIDTH' ), getproperty( 'image', 'HEIGHT' ), cm.backcolor.backcolor, cm.backcolor.backcolor, 1)
   BT_DrawBitmapTransparent (hDC, 0, 0, NIL, NIL, BT_COPY, hBitmap, NIL)
   BT_DeleteDC (BTstruct)
return nil   

function savebitmap
   local cFileName := ''

   cFileName := putfile( { {'Bitmap Files','*.bmp'} } , 'Save Image' )
   if len( cFileName ) == 0
      return nil
   endif
   if file( cFileName ) 
      if .not. msgyesno( cFileName + ' - This file already exists! Are you sure to overwrite?' )
         return nil
      endif
   endif
   BT_BitmapSaveFile ( hBitmap, cFileName )
return nil

function drawrandom
   local nDesktopHeight := getdesktopheight() 
   local nDesktopWidth := getdesktopwidth()
   local nMaxHeight := min( nDesktopHeight, nDesktopWidth ) 
   local nInnerR := 0
   local nOuterR := 0
   local nInnerSpeed := 0
   local nOuterSpeed := 0
   local nRevolutions := 0

   cm.imageheight.value := nMaxHeight
   nMaxHeight := nMaxHeight / 2
   nInnerR := random( nMaxHeight / 2 )
   nOuterR := nInnerR + random( nMaxHeight - 50 - nInnerR )
   nInnerSpeed := random( 100 )
   nOuterSpeed := random( 100 )
   nRevolutions := random( 5 )
   cm.innerr.value := nInnerR
   cm.outerr.value := nOuterR
   cm.speed.value := nInnerSpeed
   cm.ospeed.value := nOuterSpeed
   cm.revolutions.value := nRevolutions
   draw()   
return nil

function importparameters
   local cFileName := ''
   local aPenColor := {}
   local aBackColor := {}
   local nInnerR := 0
   local nInnerSpeed := 0
   local nOuterR := 0
   local nOuterSpeed := 0
   local lStereo := .f.
   local lAnimation := .f.
   local lSingleColor := .f.
   local lRandomColor := .f.
   local nStereoBase := 0
   local nPenWidth := 0
   local nAniSpeed := 0
   local nRevolutions := 0
   local nImageHeight := 0

   cFileName := alltrim( getfile( { {'CircleMagic Parameters Files','*.ini'} } , 'Import CircleMagic Parameters' ) )
   if len( cFileName ) == 0
      return nil
   endif
   if .not. file( cFileName ) 
      msgstop( cFileName + " - File doesn't exist!" )
      return nil
   endif
   begin ini FILENAME cFileName
      GET nInnerR      SECTION 'PARAMETERS' ENTRY 'InnerR'
      GET nOuterR      SECTION 'PARAMETERS' ENTRY 'OuterR'
      GET nInnerSpeed   SECTION 'PARAMETERS' ENTRY 'InnerSpeed'
      GET nOuterSpeed   SECTION 'PARAMETERS' ENTRY 'OuterSpeed'
      GET nRevolutions  SECTION 'PARAMETERS' ENTRY 'Revolutions'
      GET aPenColor    SECTION 'PARAMETERS' ENTRY 'PenColor'
      GET aBackColor   SECTION 'PARAMETERS' ENTRY 'BackColor'
      GET lStereo    SECTION 'PARAMETERS' ENTRY 'Stereo'
      GET nStereoBase  SECTION 'PARAMETERS' ENTRY 'StereoZ'
      GET lSingleColor  SECTION 'PARAMETERS' ENTRY 'SingleColor'
      GET lRandomColor SECTION 'PARAMETERS' ENTRY 'RandomColor'
      GET nPenWidth    SECTION 'PARAMETERS' ENTRY 'PenWidth'
      GET lAnimation   SECTION 'PARAMETERS' ENTRY 'Animation'
      GET nAniSpeed   SECTION 'PARAMETERS' ENTRY 'AnimationSpeed'
      GET nImageHeight   SECTION 'PARAMETERS' ENTRY 'ImageHeight'
   end ini
   cm.innerr.value := nInnerR
   cm.outerr.value := nOuterR
   cm.speed.value := nInnerSpeed
   cm.ospeed.value := nOuterSpeed
   cm.revolutions.value := nRevolutions
   cm.pencolor.backcolor := aPenColor
   cm.backcolor.backcolor := aBackColor
   cm.stereo.value := lStereo
   cm.base.value := nStereoBase
   cm.singlecolor.value := lSingleColor
   cm.randomcolor.value := lRandomColor
   cm.animationspeed.value := lAnimation
   cm.anispeed.value := nAniSpeed
   cm.penwidth.value := nPenWidth
   cm.imageheight.value := nImageHeight
   draw()
return nil


function exportparameters
   local cFileName := ''
   local aPenColor := cm.pencolor.backcolor
   local aBackColor := cm.backcolor.backcolor

   cFileName := alltrim( putfile( { {'CircleMagic Parameters Files','*.ini'} } , 'Save CircleMagic Parameters' ) )
   if len( cFileName ) == 0
      return nil
   endif
   if upper( substr( cFileName, len( cFileName ) - 3, 4 ) ) <> '.INI'
      cFileName := cFileName + '.INI'
   endif   
   if file( cFileName ) 
      if .not. msgyesno( cFileName + ' - This file already exists! Are you sure to overwrite?' )
         return nil
      endif
   endif
   begin ini FILENAME cFileName
      SET SECTION 'PARAMETERS' ENTRY 'InnerR' TO cm.innerr.value
      SET SECTION 'PARAMETERS' ENTRY 'OuterR' TO cm.outerr.value
      SET SECTION 'PARAMETERS' ENTRY 'InnerSpeed' TO cm.speed.value
      SET SECTION 'PARAMETERS' ENTRY 'OuterSpeed' TO cm.ospeed.value
      SET SECTION 'PARAMETERS' ENTRY 'Revolutions' TO cm.revolutions.value
      SET SECTION 'PARAMETERS' ENTRY 'PenColor' TO aPenColor
      SET SECTION 'PARAMETERS' ENTRY 'BackColor' TO aBackColor
      SET SECTION 'PARAMETERS' ENTRY 'Stereo' TO cm.stereo.value
      SET SECTION 'PARAMETERS' ENTRY 'StereoZ' TO cm.base.value
      SET SECTION 'PARAMETERS' ENTRY 'SingleColor' TO cm.singlecolor.value
      SET SECTION 'PARAMETERS' ENTRY 'RandomColor' TO cm.randomcolor.value
      SET SECTION 'PARAMETERS' ENTRY 'PenWidth' TO cm.penwidth.value
      SET SECTION 'PARAMETERS' ENTRY 'Animation' TO cm.animationspeed.value
      SET SECTION 'PARAMETERS' ENTRY 'AnimationSpeed' TO cm.anispeed.value
      SET SECTION 'PARAMETERS' ENTRY 'ImageHeight' TO cm.imageheight.value
   end ini
return nil
