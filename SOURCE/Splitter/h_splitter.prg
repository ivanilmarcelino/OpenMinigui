// Author: Kamil Kalus
// Email: kamilkalus0[at]gmail.com

#include "i_winuser.ch"
#include "minigui.ch"
#include "hbclass.ch"

#xtranslate _RGB(<array>) => HMG_RGB2n(<array>)

#define ROUND_VALUE   20

INIT PROCEDURE InitGradientFunc()

   __InitGradientFunc()

RETURN

EXIT PROCEDURE ExitGradientFunc()

   __ExitGradientFunc()

RETURN


CREATE CLASS Splitter

   EXPORTED:
   VAR aLeft                   // left/top controls
   VAR aRight                  // right/bottom controls
   VAR lVertical               // is splitter vertical/horizontal
   VAR nLeftLimit, nRightLimit // limit values that the splitter cannot be moved past, relative to divided controls
   VAR lShowArrow
   VAR lUseHover
   VAR lUseGradient
   VAR lRounded
   VAR color
   VAR bcolor                  // splitter background colours, defaults to the color of splitter
   VAR colorHover
   VAR bColorHover
   VAR gradient
   VAR gradientHover

   // PRIVATE VARIABLES, shoudn't be changed by hand
   VAR nPosDblClick            // position to return after dbclick
   VAR nFrom, nTo              // range of the splitter calculated based on controls x/y coordinates + corresponding limits
   VAR nLeftSize, nRightSize   // width of left and right splitted windows (or height of top and bottom) - used when scaling the controls when splitter is moved
   VAR nLeftEdge, nRightEdge   // left/rightmost x position of divided controls (or top/bottommost y position)
   VAR lRolled                 // is splitter rolled to the right/bottom (after double click)

   METHOD New( parentHandle, aLeft, aRight, lVertical, limits, lShowArrow, lUseHover, lUseGradient, color, bcolor, colorHover, bColorHover, gradient, gradientHover, lRounded )

   HIDDEN:
   VAR parentHandle

ENDCLASS

METHOD New( parentHandle, aLeft, aRight, lVertical, limits, lShowArrow, lUseHover, lUseGradient, color, bcolor, colorHover, bColorHover, gradient, gradientHover, lRounded ) CLASS Splitter

   ::parentHandle := parentHandle
   ::aLeft := iif( aLeft == NIL, {}, aLeft )
   ::aRight := iif( aRight == NIL, {}, aRight )
   ::lVertical := lVertical
   ::lShowArrow := lShowArrow
   ::lUseHover := lUseHover
   ::lUseGradient := lUseGradient
   ::lRounded := lRounded
   ::nFrom := NIL
   ::nTo := NIL
   ::nPosDblClick := NIL
   ::nLeftLimit := limits[ 1 ]
   ::nRightLimit := limits[ 2 ]
   ::nLeftSize := NIL
   ::nRightSize := NIL
   ::nLeftEdge := NIL
   ::nRightEdge := NIL
   ::color := color
   ::bcolor := bcolor
   ::colorHover := colorHover
   ::bColorHover := bColorHover
   ::gradient := gradient
   ::gradientHover := gradientHover
   ::lRolled := .F.

RETURN Self


*-----------------------------------------------------------------------------*
FUNCTION _DefineSplitter ( ControlName, ParentFormName, x, y, ;
      w, h, vertical, horizontal, aLeft, aRight, limits, lHideArrow, lNoHover, lUseGradient, nId, color, bcolor, ;
      colorHover, bcolorHover, gLeft, gMiddle, gRight, gLeftHover, gMiddleHover, gRightHover, lRounded, invisible )
*-----------------------------------------------------------------------------*

   LOCAL ParentFormHandle
   LOCAL ControlHandle := NIL
   LOCAL lDialogInMemory
   LOCAL mVar
   LOCAL k

   LOCAL splitter
   LOCAL dimensions

   // Initial values if not provided
   hb_default( @x, -1 )
   hb_default( @y, -1 )
   hb_default( @w, 0 )
   hb_default( @h, 0 )

   hb_default( @vertical, .F. )
   hb_default( @horizontal, .F. )
   hb_default( @lHideArrow, .F. )
   hb_default( @lNoHover, .F. )
   hb_default( @lUseGradient, .F. )
   hb_default( @lRounded, .F. )
   hb_default( @invisible, .F. )

   hb_default( @aLeft, {} )
   hb_default( @aRight, {} )
   hb_default( @limits, { 0, 0 } )

   hb_default( @color, { 128, 128, 128 } )
   IF color[ 1 ] != 128 .OR. color[ 2 ] != 128 .OR. color[ 3 ] != 128
      hb_default( @bCOLOR, { color[ 1 ], color[ 2 ], color[ 3 ] } )
   ELSE
      hb_default( @bCOLOR, { 242, 242, 242 } )
   ENDIF
   hb_default( @colorHover, { 0, 0, 0 } )
   hb_default( @bColorHover, { bcolor[ 1 ], bcolor[ 2 ], bcolor[ 3 ] } )

   hb_default( @gLeft, { 199, 204, 209 } )
   hb_default( @gMiddle, { 242, 242, 242 } )
   hb_default( @gRight, { gLeft[ 1 ], gLeft[ 2 ], gLeft[ 3 ] } )

   hb_default( @gLeftHover, { 100, 100, 100 } )
   hb_default( @gMiddleHover, { 242, 242, 242 } )
   hb_default( @gRightHover, { gLeftHover[ 1 ], gLeftHover[ 2 ], gLeftHover[ 3 ] } )

   // Error if both vertical and horizontal have the same value
   IF vertical == horizontal
      MsgMiniGuiError( "Splitter must be defined as either vertical or horizontal!" )
   ENDIF

   IF ( w = 0 .OR. h = 0 .OR. x = -1 .OR. y = -1 )
      dimensions := CalculateSizePos( aLeft, aRight, iif( vertical, .T., .F. ), x, y, w, h) // add x, y, w, h
      x := dimensions[ 1 ]
      y := dimensions[ 2 ]
      w := dimensions[ 3 ]
      h := dimensions[ 4 ]
   ENDIF

   IF _HMG_BeginWindowActive .OR. _HMG_BeginDialogActive
      ParentFormName := iif( _HMG_BeginDialogActive, _HMG_ActiveDialogName, _HMG_ActiveFormName )
   ENDIF

   IF _HMG_FrameLevel > 0 .AND. ! _HMG_ParentWindowActive
      x := x + _HMG_ActiveFrameCol[ _HMG_FrameLevel ]
      y := y + _HMG_ActiveFrameRow[ _HMG_FrameLevel ]
      ParentFormName := _HMG_ActiveFrameParentFormName[ _HMG_FrameLevel ]
   ENDIF
   lDialogInMemory := _HMG_DialogInMemory

   IF .NOT. _IsWindowDefined ( ParentFormName ) .AND. .NOT. lDialogInMemory
      MsgMiniGuiError( "Window: " + IFNIL( ParentFormName, "Parent", ParentFormName ) + " is not defined." )
   ENDIF

   IF _IsControlDefined ( ControlName, ParentFormName ) .AND. .NOT. lDialogInMemory
      MsgMiniGuiError ( "Control: " + ControlName + " Of " + ParentFormName + " Already defined." )
   ENDIF

   mVar := '_' + ParentFormName + '_' + ControlName
   k := _GetControlFree()

   ParentFormHandle := GetFormHandle ( ParentFormName )
   ControlHandle := InitSplitter ( ParentFormHandle, ControlName, 0, x, y, w, h )

   IF _HMG_BeginTabActive
      AAdd ( _HMG_ActiveTabCurrentPageMap, ControlHandle )
   ENDIF

#ifdef _NAMES_LIST_
   _SetNameList( mVar, k )
#else
   PUBLIC &mVar. := k
#endif

   splitter = Splitter():New( ParentFormHandle, aLeft, aRight, iif( vertical, .T., .F. ), limits, ! lHideArrow, ! lNoHover, lUseGradient, color, bcolor, ;
                              colorHover, bColorHover, { gLeft, gMiddle, gRight }, { gLeftHover, gMiddleHover, gRightHover }, lRounded )

   _HMG_aControlType[ k ] := "SPLITTER"
   _HMG_aControlNames[ k ] := ControlName
   _HMG_aControlHandles[ k ] := ControlHandle
   _HMG_aControlParenthandles[ k ] := ParentFormHandle
   _HMG_aControlIds[ k ] := nId
   // _HMG_aControlProcedures [k] :=
   _HMG_aControlPageMap[ k ] := {}
   _HMG_aControlValue[ k ] := NIL
   // _HMG_aControlInputMask [k] := iif ( ISCHARACTER( key ), key, "" )
   // _HMG_aControllostFocusProcedure [k] := lostfocus
   // _HMG_aControlGotFocusProcedure [k] :=gotfocus
   // _HMG_aControlChangeProcedure [k] := ""
   _HMG_aControlDeleted[ k ] := FALSE
   _HMG_aControlBkColor[ k ] := NIL
   _HMG_aControlFontColor[ k ] := NIL
   _HMG_aControlDblClick[ k ] := ""
   _HMG_aControlHeadClick[ k ] := {}
   _HMG_aControlRow[ k ] := y
   _HMG_aControlCol[ k ] := x
   _HMG_aControlWidth[ k ] := w
   _HMG_aControlHeight[ k ] := h
   _HMG_aControlSpacing[ k ] := 0
   _HMG_aControlContainerRow[ k ] := iif ( _HMG_FrameLevel > 0, _HMG_ActiveFrameRow[ _HMG_FrameLevel ], -1 )
   _HMG_aControlContainerCol[ k ] := iif ( _HMG_FrameLevel > 0, _HMG_ActiveFrameCol[ _HMG_FrameLevel ], -1 )
   _HMG_aControlPicture[ k ] := ""
   _HMG_aControlContainerHandle[ k ] := 0
   // _HMG_aControlFontName [k] := fontname
   // _HMG_aControlFontSize [k] := fontsize
   // _HMG_aControlFontAttributes [k] := { bold, italic, underline, strikeout }
   // _HMG_aControlToolTip [k] := tooltip
   _HMG_aControlRangeMin[ k ] := 0
   _HMG_aControlRangeMax[ k ] := 0
   // _HMG_aControlCaption [k] := Caption
   _HMG_aControlVisible [k] := iif( invisible, FALSE, TRUE )
   // _HMG_aControlHelpId [k] := NIL
   // _HMG_aControlFontHandle [k] := FontHandle
   _HMG_aControlBrushHandle[ k ] := 0
   _HMG_aControlEnabled[ k ] := .T.


   // For the splitter control this will hold a reference to an instance of Splitter containing its data
   _HMG_aControlMiscData1[ k ] := splitter

   CalculateLimits( ControlHandle )
   splitter:nPosDblClick := splitter:nTo


   IF _HMG_lOOPEnabled
      Eval ( _HMG_bOnControlInit, k, mVar )
   ENDIF

RETURN splitter


*-----------------------------------------------------------------------------*
FUNCTION SPLITTEREVENTS( hWnd, nMsg, x, y )
*-----------------------------------------------------------------------------*
   LOCAL i
   LOCAL splitter

   IF ( i := AScan( _HMG_aControlHandles, hWnd ) ) > 0 .AND. _HMG_aControlType[ i ] == "SPLITTER"
      splitter := _HMG_aControlMiscData1[ i ]
      SWITCH nMsg
      CASE WM_LBUTTONDOWN

         CalculateLimits( hWnd )
         EXIT

      CASE WM_SETFOCUS
         IF ( splitter:lVertical )
            SetWindowCursor( hWnd, IDC_SIZEWE )
         ELSE
            SetWindowCursor( hWnd, IDC_SIZENS )
         END

         EXIT

      CASE WM_MOUSEMOVE

         // Check if splitter is within a specified limit:
         IF ( splitter:lVertical )
            IF ( x <= splitter:nFrom )
               x := splitter:nFrom
            ELSEIF ( x >= splitter:nTo )
               x := splitter:nTo
            END


            MoveWindow( hWnd, x, _HMG_aControlRow[ i ], _HMG_aControlWidth[ i ], _HMG_aControlHeight[ i ], .T. )

         ELSE
            IF ( y <= splitter:nFrom )
               y := splitter:nFrom
            ELSEIF ( y >= splitter:nTo )
               y := splitter:nTo
            END

            MoveWindow( hWnd, _HMG_aControlCol[ i ], y, _HMG_aControlWidth[ i ], _HMG_aControlHeight[ i ], .T. )
         END

         EXIT

      CASE WM_LBUTTONUP
         IF ( splitter:lVertical )

            // Change x value to splitter limit if it exceeds it
            IF ( x <= splitter:nFrom )
               x := splitter:nFrom
            ELSEIF ( x >= splitter:nTo )
               x := splitter:nTo
               splitter:lRolled := .T.
            ELSE
               splitter:lRolled := .F.
            END

            AdjustControls( hwnd, splitter:nLeftSize, x - splitter:nLeftEdge, x, .T. ) // Adjust left  controls
            AdjustControls( hwnd, splitter:nRightSize, splitter:nRightEdge - x - _HMG_aControlWidth[ i ], x, .F. ) // Adjust right controls

            // splitter:x := x
            _HMG_aControlCol[ i ] := x

            MoveWindow( hWnd, x, _HMG_aControlRow[ i ], _HMG_aControlWidth[ i ], _HMG_aControlHeight[ i ], .T. )

            // WM_LBUTTONUP always executes before WM_LBUTTONDBLCLK so it will always set nPosDblClick to a correct value
            IF ( x < splitter:nTo )
               splitter:nPosDblClick := splitter:nTo
            END
         ELSE
            IF ( y <= splitter:nFrom )
               y := splitter:nFrom
            ELSEIF ( y >= splitter:nTo )
               y := splitter:nTo
               splitter:lRolled := .T.
            ELSE
               splitter:lRolled := .F.
            END

            AdjustControls( hwnd, splitter:nLeftSize, y - splitter:nLeftEdge, y, .T. ) // Adjust top     controls
            AdjustControls( hwnd, splitter:nRightSize, splitter:nRightEdge - y - _HMG_aControlHeight[ i ], y, .F. ) // Adjust bottom  controls

            // splitter:y := y
            _HMG_aControlRow[ i ] := y

            MoveWindow( hWnd, _HMG_aControlCol[ i ], y, _HMG_aControlWidth[ i ], _HMG_aControlHeight[ i ], .T. )
            IF ( y < splitter:nTo )
               splitter:nPosDblClick := splitter:nTo
            END
         END

         EXIT

      CASE WM_LBUTTONDBLCLK
         CalculateLimits( hWnd )

         // VERTICAL
         IF ( splitter:lVertical )

            IF ( splitter:nPosDblClick >= splitter:nTo )
               splitter:nPosDblClick := x
               x := splitter:nTo
               splitter:lRolled := .T.
            ELSE
               x := iif( splitter:nPosDblClick < splitter:nFrom, splitter:nFrom, splitter:nPosDblClick )
               splitter:nPosDblClick := splitter:nTo
               splitter:lRolled := .F.
            END

            AdjustControls( hwnd, splitter:nLeftSize, x - splitter:nLeftEdge, x, .T. ) // Adjust left  controls
            AdjustControls( hwnd, splitter:nRightSize, splitter:nRightEdge - x - _HMG_aControlWidth[ i ], x, .F. ) // Adjust right controls


            // splitter:x := x
            _HMG_aControlCol[ i ] := x

            MoveWindow( hWnd, x, _HMG_aControlRow[ i ], _HMG_aControlWidth[ i ], _HMG_aControlHeight[ i ], .T. )

            // HORIZONTAL
         ELSE
            IF ( splitter:nPosDblClick >= splitter:nTo )
               splitter:nPosDblClick := y
               y := splitter:nTo
               splitter:lRolled := .T.
            ELSE
               y := iif( splitter:nPosDblClick < splitter:nFrom, splitter:nFrom, splitter:nPosDblClick )
               splitter:nPosDblClick := splitter:nTo
               splitter:lRolled := .F.
            END

            AdjustControls( hwnd, splitter:nLeftSize, y - splitter:nLeftEdge, y, .T. ) // Adjust top     controls
            AdjustControls( hwnd, splitter:nRightSize, splitter:nRightEdge - y - _HMG_aControlHeight[ i ], y, .F. ) // Adjust bottom  controls

            // splitter:y := y
            _HMG_aControlRow[ i ] := y

            MoveWindow( hWnd, _HMG_aControlCol[ i ], y, _HMG_aControlWidth[ i ], _HMG_aControlHeight[ i ], .T. )

         END

         EXIT


      CASE WM_MOUSEHOVER
         IF splitter:lUseHover
            DrawSplitter( hWnd, splitter, .T. )
         ELSE
            DrawSplitter( hWnd, splitter )
         END

         EXIT


      CASE WM_MOUSELEAVE
         DrawSplitter( hWnd, splitter )
         EXIT


      CASE WM_PAINT
         DrawSplitter( hWnd, splitter )

      END
   END

RETURN 0


*-----------------------------------------------------------------------------*
FUNCTION DrawSplitter( hWnd, splitter, lHighlightOnHover )
*-----------------------------------------------------------------------------*
   LOCAL colorBrush, bColorBrush
   LOCAL colorHoverBrush, bColorHoverBrush
   LOCAL gradientBrush
   LOCAL width, height
   LOCAL blank
   LOCAL middleH
   LOCAL middleW
   LOCAL rect
   LOCAL i

   hb_default( @lHighlightOnHover, .F. )

   i := AScan( _HMG_aControlHandles, hWnd )

   rect := { 0, 0, 0, 0 }
   GetClientRect( hWnd, @rect )

   colorBrush := CreateSolidBrush( splitter:color[ 1 ], splitter:color[ 2 ], splitter:color[ 3 ] )
   bColorBrush := CreateSolidBrush( splitter:bcolor[ 1 ], splitter:bcolor[ 2 ], splitter:bcolor[ 3 ] )
   colorHoverBrush := CreateSolidBrush( splitter:colorHover[ 1 ], splitter:colorHover[ 2 ], splitter:colorHover[ 3 ] )
   bColorHoverBrush := CreateSolidBrush( splitter:bColorHover[ 1 ], splitter:bColorHover[ 2 ], splitter:bColorHover[ 3 ] )

   IF splitter:lVertical
      blank := Floor( _HMG_aControlWidth[ i ] / 4 )

      IF splitter:lUseGradient
         IF lHighlightOnHover
            gradientBrush := CreateThreeColorGradientBrush( hWnd, rect[ 3 ] - rect[ 1 ], rect[ 4 ] - rect[ 2 ], _RGB( splitter:gradientHover[ 1 ] ), _RGB( splitter:gradientHover[ 2 ] ), _RGB( splitter:gradientHover[ 3 ] ), .F. )
         ELSE
            gradientBrush := CreateThreeColorGradientBrush( hWnd, rect[ 3 ] - rect[ 1 ], rect[ 4 ] - rect[ 2 ], _RGB( splitter:gradient[ 1 ] ), _RGB( splitter:gradient[ 2 ] ), _RGB( splitter:gradient[ 3 ] ), .F. )
         END
         IF splitter:lRounded
            FillRgn( hWnd, rect[ 1 ], rect[ 2 ], rect[ 3 ], rect[ 4 ], ROUND_VALUE, ROUND_VALUE, gradientBrush )
         ELSE
            FillRect( hWnd, rect[ 1 ], rect[ 2 ], rect[ 3 ], rect[ 4 ], gradientBrush )
         END
         DeleteObject( gradientBrush )
      ELSE
         IF lHighlightOnHover
            IF splitter:lRounded
               FillRgn( hWnd, rect[ 1 ], rect[ 2 ], rect[ 3 ], rect[ 4 ], ROUND_VALUE, ROUND_VALUE, bColorHoverBrush )
            ELSE
               FillRect( hWnd, rect[ 1 ], rect[ 2 ], rect[ 3 ], rect[ 4 ], bColorHoverBrush )
            END
            
            FillRect( hWnd, rect[ 1 ] + blank, rect[ 2 ] + 2, rect[ 3 ] - blank, rect[ 4 ] - 2, colorHoverBrush )
         ELSE
            IF splitter:lRounded
               FillRgn( hWnd, rect[ 1 ], rect[ 2 ], rect[ 3 ], rect[ 4 ], ROUND_VALUE, ROUND_VALUE, bColorBrush )
            ELSE
               FillRect( hWnd, rect[ 1 ], rect[ 2 ], rect[ 3 ], rect[ 4 ], bColorBrush )
            END
            
            FillRect( hWnd, rect[ 1 ] + blank, rect[ 2 ] + 2, rect[ 3 ] - blank, rect[ 4 ] - 2, colorBrush )
         END
      END
   ELSE
      blank := Floor( _HMG_aControlHeight[ i ] / 4 )

      IF splitter:lUseGradient
         IF lHighlightOnHover
            gradientBrush := CreateThreeColorGradientBrush( hWnd, rect[ 3 ] - rect[ 1 ], rect[ 4 ] - rect[ 2 ], _RGB( splitter:gradientHover[ 1 ] ), _RGB( splitter:gradientHover[ 2 ] ), _RGB( splitter:gradientHover[ 3 ] ), .T. )
         ELSE
            gradientBrush := CreateThreeColorGradientBrush( hWnd, rect[ 3 ] - rect[ 1 ], rect[ 4 ] - rect[ 2 ], _RGB( splitter:gradient[ 1 ] ), _RGB( splitter:gradient[ 2 ] ), _RGB( splitter:gradient[ 3 ] ), .T. )
         END
         IF splitter:lRounded
            FillRgn( hWnd, rect[ 1 ], rect[ 2 ], rect[ 3 ], rect[ 4 ], ROUND_VALUE, ROUND_VALUE, gradientBrush )
         ELSE
            FillRect( hWnd, rect[ 1 ], rect[ 2 ], rect[ 3 ], rect[ 4 ], gradientBrush )
         END
         DeleteObject( gradientBrush )
      ELSE
         IF lHighlightOnHover
            IF splitter:lRounded
               FillRgn( hWnd, rect[ 1 ], rect[ 2 ], rect[ 3 ], rect[ 4 ], ROUND_VALUE, ROUND_VALUE, bColorHoverBrush )
            ELSE
               FillRect( hWnd, rect[ 1 ], rect[ 2 ], rect[ 3 ], rect[ 4 ], bColorHoverBrush )
            END
            
            FillRect( hWnd, rect[ 1 ] + 2, rect[ 2 ] + blank, rect[ 3 ] - 2, rect[ 4 ] - blank, colorHoverBrush )
         ELSE
            IF splitter:lRounded
               FillRgn( hWnd, rect[ 1 ], rect[ 2 ], rect[ 3 ], rect[ 4 ], ROUND_VALUE, ROUND_VALUE, bColorBrush )
            ELSE
               FillRect( hWnd, rect[ 1 ], rect[ 2 ], rect[ 3 ], rect[ 4 ], bColorBrush )
            END
            
            FillRect( hWnd, rect[ 1 ] + 2, rect[ 2 ] + blank, rect[ 3 ] - 2, rect[ 4 ] - blank, colorBrush )
         END
      END
   END

   DeleteObject( colorBrush )
   DeleteObject( bColorBrush )
   DeleteObject( colorHoverBrush )
   DeleteObject( bColorHoverBrush )

   // Drawing an arrow
   IF splitter:lShowArrow
      IF splitter:lVertical .AND. _HMG_aControlHeight[ i ] > 40
         IF ( splitter:color[ 1 ] = splitter:bcolor[ 1 ] .AND. splitter:color[ 2 ] = splitter:bcolor[ 2 ] .AND. splitter:color[ 3 ] = splitter:bcolor[ 3 ] ) .OR. splitter:lUseGradient
            width := _HMG_aControlWidth[ i ]
         ELSE
            width := _HMG_aControlWidth[ i ] - ( 2 * blank )
         END
         middleW := Floor( ( rect[ 3 ] - rect[ 1 ] ) / 2 )
         middleH := Floor( ( rect[ 4 ] - rect[ 2 ] ) / 2 )

         IF width >= 10 // width of the arrow is 6 pixels + 2 pixels on both sides
            IF ! splitter:lRolled
               PolygonDraw( hWnd, { middleW - 3, middleW - 3, middleW + 3 }, { middleH - 8, middleH + 8, middleH }, { 0, 0, 0 }, 1, { 0, 0, 0 }, .T. )
            ELSE
               PolygonDraw( hWnd, { middleW + 3, middleW + 3, middleW - 3 }, { middleH - 8, middleH + 8, middleH }, { 0, 0, 0 }, 1, { 0, 0, 0 }, .T. )
            END
         END
      ELSEIF !splitter:lVertical .AND. _HMG_aControlWidth[ i ] > 40
         IF ( splitter:color[ 1 ] = splitter:bcolor[ 1 ] .AND. splitter:color[ 2 ] = splitter:bcolor[ 2 ] .AND. splitter:color[ 3 ] = splitter:bcolor[ 3 ] ) .OR. splitter:lUseGradient
            height := _HMG_aControlHeight[ i ]
         ELSE
            height := _HMG_aControlHeight[ i ] - ( 2 * blank )
         END
         middleW := Floor( ( rect[ 3 ] - rect[ 1 ] ) / 2 )
         middleH := Floor( ( rect[ 4 ] - rect[ 2 ] ) / 2 )

         IF height >= 10 // height of the arrow is 6 pixels + 2 pixels on both sides
            IF ! splitter:lRolled
               PolygonDraw( hWnd, { middleW - 8, middleW + 8, middleW }, { middleH - 3, middleH - 3, middleH + 3 }, { 0, 0, 0 }, 1, { 0, 0, 0 }, .T. )
            ELSE
               PolygonDraw( hWnd, { middleW - 8, middleW + 8, middleW }, { middleH + 3, middleH + 3, middleH - 3 }, { 0, 0, 0 }, 1, { 0, 0, 0 }, .T. )
            END
         END
      END
   END

RETURN NIL


*-----------------------------------------------------------------------------*
FUNCTION CalculateLimits( hWnd )
*-----------------------------------------------------------------------------*
   LOCAL i, j, k
   LOCAL rect
   LOCAL splitter
   LOCAL width, height

   LOCAL lowestX, lowestY
   LOCAL highestX, highestY

   // Additional 1 pixel is added (or removed) to nFrom and nTo to prevent controls from dissappearing completely if splitter is moved to the edge

   j := AScan( _HMG_aControlHandles, hWnd )
   width := _HMG_aControlWidth[ j ]
   height := _HMG_aControlHeight[ j ]
   splitter := _HMG_aControlMiscData1[ j ]

   // Vertical splitter
   IF ( splitter:lVertical )

      // ----------------------------- LEFT  CONTROLS ------------------------------- *
      lowestX := 100000

      IF Len( splitter:aLeft ) > 0
         // Find lowest x position from aLeft array
         FOR i := 1 TO Len( splitter:aLeft )
            k := AScan ( _HMG_aControlNames, splitter:aLeft[ i ] )
            lowestX := Min( lowestX, _HMG_aControlCol[ k ] )
         NEXT

         splitter:nFrom := lowestX + splitter:nLeftLimit + 1
         splitter:nLeftEdge := lowestX
         splitter:nLeftSize := _HMG_aControlCol[ j ] - lowestX
      ELSE
         rect := { 0, 0, 0, 0 }
         GetClientRect( splitter:parentHandle, @rect )
         splitter:nLeftEdge := rect[ 1 ]
         splitter:nLeftSize := _HMG_aControlCol[ j ] - rect[ 1 ]
         splitter:nFrom := rect[ 1 ] + splitter:nLeftLimit + 1
      END
      // ----------------------------- LEFT  CONTROLS ------------------------------- *


      // ----------------------------- RIGTH CONTROLS ------------------------------- *
      highestX := 0

      IF Len( splitter:aRight ) > 0
         // Find highest x position from aRight array
         FOR i := 1 TO Len( splitter:aRight )
            k := AScan ( _HMG_aControlNames, splitter:aRight[ i ] )
            highestX := Max( highestX, _HMG_aControlCol[ k ] + _HMG_aControlWidth[ k ] )
         NEXT

         splitter:nTo := highestX - width - splitter:nRightLimit - 1
         splitter:nRightEdge := highestX
         splitter:nRightSize := highestX - _HMG_aControlCol[ j ] - width
      ELSE
         rect := { 0, 0, 0, 0 }
         GetClientRect( splitter:parentHandle, @rect )
         splitter:nRightEdge := rect[ 3 ]
         splitter:nRightSize := rect[ 3 ] - _HMG_aControlCol[ j ] - width
         splitter:nTo := rect[ 3 ] - width - splitter:nRightLimit - 1
      END
      // ----------------------------- RIGTH CONTROLS ------------------------------- *

   ELSE // Horizontal splitter

      // -----------------------------  TOP   CONTROLS ------------------------------- *
      lowestY := 100000

      IF Len( splitter:aLeft ) > 0
         FOR i := 1 TO Len( splitter:aLeft )
            k := AScan( _HMG_aControlNames, splitter:aLeft[ i ] )
            lowestY := Min( lowestY, _HMG_aControlRow[ k ] )
         NEXT

         splitter:nFrom := lowestY + splitter:nLeftLimit + 1
         splitter:nLeftEdge := lowestY
         splitter:nLeftSize := _HMG_aControlRow[ j ] - lowestY
      ELSE
         rect := { 0, 0, 0, 0 }
         GetClientRect( splitter:parentHandle, @rect )
         splitter:nLeftEdge := rect[ 2 ]
         splitter:nLeftSize := _HMG_aControlRow[ j ] - rect[ 2 ]
         splitter:nFrom := rect[ 2 ] + splitter:nLeftLimit + 1
      END
      // -----------------------------  TOP   CONTROLS ------------------------------- *


      // ----------------------------- BOTTOM CONTROLS ------------------------------- *
      highestY := 0

      IF Len( splitter:aRight ) > 0
         FOR i := 1 TO Len( splitter:aRight )
            k := AScan ( _HMG_aControlNames, splitter:aRight[ i ] )
            highestY := Max( highestY, _HMG_aControlRow[ k ] + _HMG_aControlHeight[ k ] )
         NEXT

         splitter:nTo := highestY - height - splitter:nRightLimit - 1
         splitter:nRightEdge := highestY
         splitter:nRightSize := highestY - _HMG_aControlRow[ j ] - height
      ELSE
         rect := { 0, 0, 0, 0 }
         GetClientRect( splitter:parentHandle, @rect )
         splitter:nRightEdge := rect[ 4 ]
         splitter:nRightSize := rect[ 4 ] - _HMG_aControlRow[ j ] - height
         splitter:nTo := rect[ 4 ] - height - splitter:nRightLimit - 1
      END
      // ----------------------------- BOTTOM CONTROLS ------------------------------- *

   END

RETURN NIL
 

*-----------------------------------------------------------------------------*
FUNCTION CalculateSizePos( aLeft, aRight, lVertical, originX, originY, originWidth, originHeight )
*-----------------------------------------------------------------------------*
   LOCAL nMinLeftX, nMinLeftY, nMinRightX, nMinRightY
   LOCAL nMaxLeftX, nMaxLeftY, nMaxRightX, nMaxRightY
   LOCAL i, j
   LOCAL x, y
   LOCAL width, height
   LOCAL lLeftNotEmpty  := .F.
   LOCAL lRightNotEmpty := .F.
   LOCAL gap

   IF lVertical
      nMaxLeftX  := 0      // rightmost x position of left controls
      nMinLeftY  := 10000  // topmost y position of left controls
      nMinRightY := 10000  // topmost y position of right controls
      nMinRightX := 10000  // leftmost x position of right controls
      nMaxLeftY  := 0      // bottommost y position of left controls
      nMaxRightY := 0      // bottommost y position of right controls

      IF Len( aLeft ) > 0
         lLeftNotEmpty := .T.
         FOR i := 1 TO Len( aLeft )
            j := AScan ( _HMG_aControlNames, aLeft[ i ] )

            nMaxLeftX := Max( nMaxLeftX, _HMG_aControlCol[ j ] + _HMG_aControlWidth[ j ] )
            nMinLeftY := Min( nMinLeftY, _HMG_aControlRow[ j ] )
            nMaxLeftY := Max( nMaxLeftY, _HMG_aControlRow[ j ] + _HMG_aControlHeight[ j ] )
         NEXT
      END

      IF Len( aRight ) > 0
         lRightNotEmpty := .T.
         FOR i := 1 TO Len( aRight )
            j := AScan( _HMG_aControlNames, aRight[ i ] )

            nMinRightX := Min( nMinRightX, _HMG_aControlCol[ j ] )
            nMinRightY := Min( nMinRightY, _HMG_aControlRow[ j ] )
            nMaxRightY := Max( nMaxRightY, _HMG_aControlRow[ j ] + _HMG_aControlHeight[ j ] )
         NEXT
      END

      IF lLeftNotEmpty .AND. lRightNotEmpty
         width := nMinRightX - nMaxLeftX
         height := Max( nMaxLeftY, nMaxRightY ) - Min( nMinLeftY, nMinRightY )
         x := nMaxLeftX
         y := Min( nMinLeftY, nMinRightY )
      ELSEIF lLeftNotEmpty
         width := 10
         height := nMaxLeftY - nMinLeftY
         x := nMaxLeftX
         y := nMinLeftY
      ELSEIF lRightNotEmpty
         width := 10
         height := nMaxRightY - nMinRightY
         x := nMinRightX - 10
         y := nMinRightY
      ELSE
         MsgMiniGuiError( "ERROR: Cannot find dimensions for a splitter without controls" )
      END

      // Checks
      IF originWidth > width
         MsgMiniGuiError( "ERROR: Cannot create a splitter with provided width" )
      END

      IF originX != -1 .AND. (originX < nMaxLeftX .OR. originX > nMinRightX)
         MsgMiniGuiError( "ERROR: Cannot create a splitter with provided x coordinate" )
      END
      
      height := iif(originHeight = 0, height, originHeight)      
      y := iif(originY = -1, y, originY)

      IF originX = -1 .AND. originWidth > 0
         gap := Floor((width - originWidth) / 2)
         x   := x + gap
      ELSE
         x := iif(originX = -1, x, originX)
      END

      width := iif(originWidth = 0, width, originWidth)

   ELSE

      nMaxLeftY := 0
      nMinLeftX := 10000
      nMaxLeftX := 0
      nMinRightY := 10000
      nMinRightX := 10000
      nMaxRightX := 0

      IF Len( aLeft ) > 0
         lLeftNotEmpty := .T.
         FOR i := 1 TO Len( aLeft )
            j := AScan ( _HMG_aControlNames, aLeft[ i ] )

            nMinLeftX := Min( nMinLeftX, _HMG_aControlCol[ j ] )
            nMaxLeftX := Max( nMaxLeftX, _HMG_aControlCol[ j ] + _HMG_aControlWidth[ j ] )
            nMaxLeftY := Max( nMaxLeftY, _HMG_aControlRow[ j ] + _HMG_aControlHeight[ j ] )
         NEXT
      END

      IF Len( aRight ) > 0
         lRightNotEmpty := .T.
         FOR i := 1 TO Len( aRight )
            j := AScan( _HMG_aControlNames, aRight[ i ] )

            nMinRightX := Min( nMinRightX, _HMG_aControlCol[ j ] )
            nMaxRightX := Max( nMaxRightX, _HMG_aControlCol[ j ] + _HMG_aControlWidth[ j ] )
            nMinRightY := Min( nMinRightY, _HMG_aControlRow[ j ] )
         NEXT
      END

      IF lLeftNotEmpty .AND. lRightNotEmpty
         width := Max( nMaxLeftX, nMaxRightX ) - Min( nMinLeftX, nMinRightX )
         height := nMinRightY - nMaxLeftY
         x := Min( nMinLeftX, nMinRightX )
         y := nMaxLeftY
      ELSEIF lLeftNotEmpty
         width := nMaxLeftX - nMinLeftX
         height := 10
         x := nMinLeftX
         y := nMaxLeftY
      ELSEIF lRightNotEmpty
         width := nMaxRightX - nMinRightX
         height := 10
         x := nMinRightX
         y := nMinRightY - 10
      ELSE
         MsgMiniGuiError( "ERROR: Cannot find dimensions for a splitter without controls" )
      END

      // Checks
      IF originHeight > height
         MsgMiniGuiError( "ERROR: Cannot create a splitter with provided width" )
      END

      IF originY != -1 .AND. (originY < nMaxLeftY .OR. originY > nMinRightY)
         MsgMiniGuiError( "ERROR: Cannot create a splitter with provided x coordinate" )
      END

      x      := iif(originX = -1, x, originX)
      width := iif(originWidth = 0, width, originWidth)

      IF originY = -1 .AND. originHeight > 0
         gap := Floor((height - originHeight) / 2)
         y   := y + gap
      ELSE
         y := iif(originY = -1, y, originY)
      END

      height := iif(originHeight = 0, height, originHeight)

   END

   IF ( width <= 0 .OR. height <= 0 )
      MsgMiniGuiError( "Cannot automatically create dimensions of splitter for specified controls" )
   END

RETURN { x, y, width, height }



// Slightly modified function _AutoAdjust from MiniGui Extended h_events.prg

*-----------------------------------------------------------------------------*
Function AdjustControls ( hWnd, initSize, newSize, newPos, lLeftControls)  // lLeftControls -> .T. if left/top controls | .F. if right/bottom
*-----------------------------------------------------------------------------*
    Local ParentForm, ParentName
    Local ControlName, ControlType
    Local nDiv, nDiv2
    Local lAutoZooming := ( _HMG_AutoZooming == .T. )
    Local cControlExcept := "IMAGE"
    Local lInvisible := .T.

    Local i, j, k
    Local splitter
    Local row, col, width, height
    Local leftEdge

    #ifdef _TSBROWSE_
        Local oBrw
    #endif

    // nWidth := iif( GetDesktopWidth() < GetWindowWidth( hWnd ), GetDesktopWidth(), GetWindowWidth( hWnd ) )
    // nHeight := iif( GetDesktopHeight() < GetWindowHeight( hWnd ), GetDesktopHeight(), GetWindowHeight( hWnd ) )

    IF IsWindowVisible( hWnd ) .AND. ! IsAppXPThemed()
        HideWindow( hWnd )
    ELSE
        lInvisible := .NOT. LockWindowUpdate( hWnd )
    ENDIF

    i := AScan( _HMG_aControlHandles, hWnd )
    splitter := _HMG_aControlMiscData1 [i]

    ParentForm := _HMG_aControlParenthandles [i]
    j := Ascan (_HMG_aFormHandles, ParentForm)
    ParentName := _HMG_aFormNames [j]

    nDiv := newSize / initSize

    ****Not sure what it does, leaving it in case it's needed in future

    // IF _HMG_aFormVirtualWidth [i] > 0 .AND. _HMG_aFormVirtualHeight [i] > 0
    //     nDivw := newWidth / _HMG_aFormVirtualWidth [i]
    //     nDivh := newHeight / _HMG_aFormVirtualHeight [i]
    // ELSE
    //     nDivw := 1
    //     nDivh := 1
    // ENDIF

    IF lAutoZooming
        nDiv2 := nDiv
    ELSEIF _HMG_AutoAdjustException == .T.
        cControlExcept += ",OBUTTON,CHECKBOX"
    ENDIF


    IF splitter:lVertical
        IF lLeftControls // Left controls
            FOR j := 1 TO Len(splitter:aLeft)
                k := AScan (_HMG_aControlNames, splitter:aLeft[j])

                ControlName := _HMG_aControlNames [k]
                ControlType := _HMG_aControlType [k]
                IF ! Empty( ControlName ) .AND. !( ControlType $ "MENU,HOTKEY,TOOLBAR,MESSAGEBAR,ITEMMESSAGE,TIMER" ) .AND. ;
                    Empty( GetControlContainerHandle( ControlName, ParentName ) )

                    IF ControlType == "RADIOGROUP"
                       _HMG_aControlSpacing [k] := _HMG_aControlSpacing [k] * nDiv
                    ENDIF

                    IF ! lAutoZooming
                        DO CASE
                            CASE ControlType $ cControlExcept .OR. "PICK" $ ControlType
                                nDiv2 := 1
                            CASE "TEXT" $ ControlType .OR. "LABEL" $ ControlType .OR. ControlType $ "GETBOX,SPINNER,HYPERLINK,PROGRESSBAR,COMBO,HOTKEYBOX"
                                nDiv2 := nDiv
                            OTHERWISE
                                nDiv2 := nDiv
                        ENDCASE
                    ENDIF

                    col := _HMG_aControlCol[k]
                    row := _HMG_aControlRow[k]
                    width := _HMG_aControlWidth[k]
                    height := _HMG_aControlHeight[k]

                    // Control is not the leftmost control
                    IF col > splitter:nLeftEdge
                        col := splitter:nLeftEdge + ((col - splitter:nLeftEdge) * nDiv)
                    ENDIF

                    width *= nDiv2

                    // Change splitter's control x attribute
                    IF ControlType == "SPLITTER"
                        // _HMG_aControlMiscData1[k]:x := col
                        _HMG_aControlCol[k] := col
                    ENDIF

                    _SetControlSizePos(ControlName, ParentName, row-iif( _HMG_aControlContainerRow [k] == -1, 0, _HMG_aControlContainerRow [k] ), col-iif( _HMG_aControlContainerCol [k] == -1, 0, _HMG_aControlContainerCol [k] ), width, height)

                    #ifdef _TSBROWSE_
                        IF ControlType == "TBROWSE"
                            oBrw := _HMG_aControlIds [k]
                            IF oBrw:lIsDbf
                                oBrw:UpStable()
                            ELSE
                                oBrw:Refresh( .T. )
                                oBrw:DrawSelect()
                            ENDIF
                        ELSEIF lAutoZooming .AND. ControlType <> "SLIDER"
                    #else
                        IF lAutoZooming .AND. ControlType <> "SLIDER"
                    #endif
                            _SetFontSize( ControlName, ParentForm, _HMG_aControlFontSize [k] * nDiv )
                        ENDIF
                ENDIF

            NEXT j

        ELSE // Right controls
            leftEdge := _HMG_aControlCol[i] + _HMG_aControlWidth[i]

            FOR j := 1 TO Len(splitter:aRight)
                k := AScan (_HMG_aControlNames, splitter:aRight[j])

                ControlName := _HMG_aControlNames [k]
                ControlType := _HMG_aControlType [k]
                IF ! Empty( ControlName ) .AND. !( ControlType $ "MENU,HOTKEY,TOOLBAR,MESSAGEBAR,ITEMMESSAGE,TIMER" ) .AND. ;
                    Empty( GetControlContainerHandle( ControlName, ParentName ) )

                    IF ControlType == "RADIOGROUP"
                       _HMG_aControlSpacing [k] := _HMG_aControlSpacing [k] * nDiv
                    ENDIF

                    IF ! lAutoZooming
                        DO CASE
                            CASE ControlType $ cControlExcept .OR. "PICK" $ ControlType
                                nDiv2 := 1
                            CASE "TEXT" $ ControlType .OR. "LABEL" $ ControlType .OR. ControlType $ "GETBOX,SPINNER,HYPERLINK,PROGRESSBAR,COMBO,HOTKEYBOX"
                                nDiv2 := nDiv
                            OTHERWISE
                                nDiv2 := nDiv
                        ENDCASE
                    ENDIF

                    col := _HMG_aControlCol[k]
                    row := _HMG_aControlRow[k]
                    width := _HMG_aControlWidth[k]
                    height := _HMG_aControlHeight[k]

                    col := (newPos + _HMG_aControlWidth[i]) + ((col - leftEdge) * nDiv)

                    width *= nDiv2

                    // Change splitter's control x attribute
                    IF ControlType == "SPLITTER"
                        // _HMG_aControlMiscData1[k]:x := col
                        _HMG_aControlCol[k] := col
                    ENDIF

                    _SetControlSizePos(ControlName, ParentName, row-iif( _HMG_aControlContainerRow [k] == -1, 0, _HMG_aControlContainerRow [k] ), col-iif( _HMG_aControlContainerCol [k] == -1, 0, _HMG_aControlContainerCol [k] ), width, height)

                    #ifdef _TSBROWSE_
                        IF ControlType == "TBROWSE"
                            oBrw := _HMG_aControlIds [k]
                            IF oBrw:lIsDbf
                                oBrw:UpStable()
                            ELSE
                                oBrw:Refresh( .T. )
                                oBrw:DrawSelect()
                            ENDIF
                        ELSEIF lAutoZooming .AND. ControlType <> "SLIDER"
                    #else
                        IF lAutoZooming .AND. ControlType <> "SLIDER"
                    #endif
                            _SetFontSize( ControlName, ParentForm, _HMG_aControlFontSize [k] * nDiv )
                        ENDIF
                ENDIF

            NEXT j
        ENDIF
    ELSE
        IF lLeftControls // Left controls
            FOR j := 1 TO Len(splitter:aLeft)
                k := AScan (_HMG_aControlNames, splitter:aLeft[j])

                ControlName := _HMG_aControlNames [k]
                ControlType := _HMG_aControlType [k]
                IF ! Empty( ControlName ) .AND. !( ControlType $ "MENU,HOTKEY,TOOLBAR,MESSAGEBAR,ITEMMESSAGE,TIMER" ) .AND. ;
                    Empty( GetControlContainerHandle( ControlName, ParentName ) )

                    IF ControlType == "RADIOGROUP"
                       _HMG_aControlSpacing [k] := _HMG_aControlSpacing [k] * nDiv
                    ENDIF

                    IF ! lAutoZooming
                        DO CASE
                            CASE ControlType $ cControlExcept .OR. "PICK" $ ControlType
                                nDiv2 := 1
                            CASE "TEXT" $ ControlType .OR. "LABEL" $ ControlType .OR. ControlType $ "GETBOX,SPINNER,HYPERLINK,PROGRESSBAR,COMBO,HOTKEYBOX"
                                nDiv2 := nDiv
                            OTHERWISE
                                nDiv2 := nDiv
                        ENDCASE
                    ENDIF

                    col := _HMG_aControlCol[k]
                    row := _HMG_aControlRow[k]
                    width := _HMG_aControlWidth[k]
                    height := _HMG_aControlHeight[k]

                    // Control is not at the top side
                    IF row > splitter:nLeftEdge
                        row := splitter:nLeftEdge + ((row - splitter:nLeftEdge) * nDiv)
                    ENDIF

                    height *= nDiv2

                    // Change splitter's control x attribute
                    IF ControlType == "SPLITTER"
                        // _HMG_aControlMiscData1[k]:y := row
                        _HMG_aControlRow[k] := row
                    ENDIF

                    _SetControlSizePos(ControlName, ParentName, row-iif( _HMG_aControlContainerRow [k] == -1, 0, _HMG_aControlContainerRow [k] ), col-iif( _HMG_aControlContainerCol [k] == -1, 0, _HMG_aControlContainerCol [k] ), width, height)

                    #ifdef _TSBROWSE_
                        IF ControlType == "TBROWSE"
                            oBrw := _HMG_aControlIds [k]
                            IF oBrw:lIsDbf
                                oBrw:UpStable()
                            ELSE
                                oBrw:Refresh( .T. )
                                oBrw:DrawSelect()
                            ENDIF
                        ELSEIF lAutoZooming .AND. ControlType <> "SLIDER"
                    #else
                        IF lAutoZooming .AND. ControlType <> "SLIDER"
                    #endif
                            _SetFontSize( ControlName, ParentForm, _HMG_aControlFontSize [k] * nDiv )
                        ENDIF
                ENDIF

            NEXT j

        ELSE // Right controls
            leftEdge := _HMG_aControlRow[i] + _HMG_aControlHeight[i]

            FOR j := 1 TO Len(splitter:aRight)
                k := AScan (_HMG_aControlNames, splitter:aRight[j])

                ControlName := _HMG_aControlNames [k]
                ControlType := _HMG_aControlType [k]
                IF ! Empty( ControlName ) .AND. !( ControlType $ "MENU,HOTKEY,TOOLBAR,MESSAGEBAR,ITEMMESSAGE,TIMER" ) .AND. ;
                    Empty( GetControlContainerHandle( ControlName, ParentName ) )

                    IF ControlType == "RADIOGROUP"
                       _HMG_aControlSpacing [k] := _HMG_aControlSpacing [k] * nDiv
                    ENDIF

                    IF ! lAutoZooming
                        DO CASE
                            CASE ControlType $ cControlExcept .OR. "PICK" $ ControlType
                                nDiv2 := 1
                            CASE "TEXT" $ ControlType .OR. "LABEL" $ ControlType .OR. ControlType $ "GETBOX,SPINNER,HYPERLINK,PROGRESSBAR,COMBO,HOTKEYBOX"
                                nDiv2 := nDiv
                            OTHERWISE
                                nDiv2 := nDiv
                        ENDCASE
                    ENDIF

                    col := _HMG_aControlCol[k]
                    row := _HMG_aControlRow[k]
                    width := _HMG_aControlWidth[k]
                    height := _HMG_aControlHeight[k]

                    row := (newPos + _HMG_aControlHeight[i]) + ((row - leftEdge) * nDiv)

                    height *= nDiv2

                    // Change splitter's control x attribute
                    IF ControlType == "SPLITTER"
                        // _HMG_aControlMiscData1[k]:y := row
                        _HMG_aControlRow[k] := row
                    ENDIF

                    _SetControlSizePos(ControlName, ParentName, row-iif( _HMG_aControlContainerRow [k] == -1, 0, _HMG_aControlContainerRow [k] ), col-iif( _HMG_aControlContainerCol [k] == -1, 0, _HMG_aControlContainerCol [k] ), width, height)

                    #ifdef _TSBROWSE_
                        IF ControlType == "TBROWSE"
                            oBrw := _HMG_aControlIds [k]
                            IF oBrw:lIsDbf
                                oBrw:UpStable()
                            ELSE
                                oBrw:Refresh( .T. )
                                oBrw:DrawSelect()
                            ENDIF
                        ELSEIF lAutoZooming .AND. ControlType <> "SLIDER"
                    #else
                        IF lAutoZooming .AND. ControlType <> "SLIDER"
                    #endif
                            _SetFontSize( ControlName, ParentForm, _HMG_aControlFontSize [k] * nDiv )
                        ENDIF
                ENDIF

            NEXT j
        ENDIF
    ENDIF

    IF lInvisible
        ShowWindow( hWnd )
    ELSE
        LockWindowUpdate( 0 )
        RedrawWindow ( hWnd )
    ENDIF

RETURN NIL



// Taken, and slightly modified from h_gradient.prg

// --------------------- C Gradient Code -----------------------------*

#pragma BEGINDUMP

#include <mgdefs.h>

extern HB_PTRUINT wapi_GetProcAddress( HMODULE hModule, LPCSTR lpProcName );

typedef BOOL ( WINAPI * GradientFillPtr )( HDC, CONST PTRIVERTEX, DWORD, CONST PVOID, DWORD, DWORD );

static HINSTANCE s_hDLL = NULL;

static GradientFillPtr   f_GradientFill   = NULL;

void RegisterResource( HANDLE hResource, LPCSTR szType );

HBRUSH ThreeColorGradientBrush( HDC pDC, long cx, long cy, COLORREF crFrom, COLORREF crMiddle, COLORREF crTo, BOOL bVert );

HB_FUNC( __INITGRADIENTFUNC )
{
   s_hDLL = LoadLibrary( TEXT( "gdi32.dll" ) );

   if( s_hDLL != NULL )
   {
      f_GradientFill   = ( GradientFillPtr ) wapi_GetProcAddress( s_hDLL, "GdiGradientFill" );

      if(( f_GradientFill == NULL ) )
      {
         FreeLibrary( s_hDLL );
         s_hDLL = NULL;
      }
   }

   if( s_hDLL == NULL )
   {
      s_hDLL = LoadLibrary( TEXT( "msimg32.dll" ) );

      if( s_hDLL != NULL )
      {
         f_GradientFill   = ( GradientFillPtr ) wapi_GetProcAddress( s_hDLL, "GradientFill" );

         if( ( f_GradientFill == NULL ) )
         {
            FreeLibrary( s_hDLL );
            s_hDLL = NULL;
         }
      }
   }

}

HB_FUNC( __EXITGRADIENTFUNC )
{
   if( s_hDLL != NULL )
   {
      FreeLibrary( s_hDLL );
      s_hDLL = NULL;
   }
}

HB_FUNC( CREATETHREECOLORGRADIENTBRUSH )
{
   HWND   hwnd = hmg_par_raw_HWND( 1 );
   HDC    hdc;
   HBRUSH hBrush;

   if( ! IsWindow( hwnd ) )
      hwnd = GetDesktopWindow();

   hdc = GetDC( hwnd );

   hBrush = ThreeColorGradientBrush( hdc, hb_parnl( 2 ), hb_parnl( 3 ),
                                 hmg_par_COLORREF( 4 ), hmg_par_COLORREF( 5 ), hmg_par_COLORREF( 6 ),
                                 hb_parl( 7 ) );

   RegisterResource( hBrush, "BRUSH" );
   hmg_ret_raw_HBRUSH( hBrush );

   ReleaseDC( hwnd, hdc );
}

HBRUSH ThreeColorGradientBrush( HDC pDC, long cx, long cy, COLORREF crFrom, COLORREF crMiddle, COLORREF crTo, BOOL bVert )
{
   HDC     memDC;
   HBITMAP memBmp;
   HBRUSH  pGradientBrush = ( HBRUSH ) NULL;

   memDC  = CreateCompatibleDC( pDC );
   memBmp = CreateCompatibleBitmap( pDC, cx, cy );

   if( memDC && memBmp )
   {
      TRIVERTEX     rcVertex[ 4 ];
        GRADIENT_RECT gRect[2]= {
        {0, 1},
        {2, 3}
      };


    if (bVert) {
      rcVertex[ 0 ].x     = 0;
      rcVertex[ 0 ].y     = 0;
      rcVertex[ 0 ].Red   = ( unsigned short ) ( GetRValue( crFrom ) << 8 );
      rcVertex[ 0 ].Green = ( unsigned short ) ( GetGValue( crFrom ) << 8 );
      rcVertex[ 0 ].Blue  = ( unsigned short ) ( GetBValue( crFrom ) << 8 );
      rcVertex[ 0 ].Alpha = 0;

      rcVertex[ 1 ].x     = cx;
      rcVertex[ 1 ].y     = cy / 2;
      rcVertex[ 1 ].Red   = ( unsigned short ) ( GetRValue( crMiddle ) << 8 );
      rcVertex[ 1 ].Green = ( unsigned short ) ( GetGValue( crMiddle ) << 8 );
      rcVertex[ 1 ].Blue  = ( unsigned short ) ( GetBValue( crMiddle ) << 8 );
      rcVertex[ 1 ].Alpha = 0;

      rcVertex[ 2 ].x     = 0;
      rcVertex[ 2 ].y     = cy / 2;
      rcVertex[ 2 ].Red   = ( unsigned short ) ( GetRValue( crMiddle ) << 8 );
      rcVertex[ 2 ].Green = ( unsigned short ) ( GetGValue( crMiddle ) << 8 );
      rcVertex[ 2 ].Blue  = ( unsigned short ) ( GetBValue( crMiddle ) << 8 );
      rcVertex[ 2 ].Alpha = 0;

      rcVertex[ 3 ].x     = cx;
      rcVertex[ 3 ].y     = cy;
      rcVertex[ 3 ].Red   = ( unsigned short ) ( GetRValue( crTo ) << 8 );
      rcVertex[ 3 ].Green = ( unsigned short ) ( GetGValue( crTo ) << 8 );
      rcVertex[ 3 ].Blue  = ( unsigned short ) ( GetBValue( crTo ) << 8 );
      rcVertex[ 3 ].Alpha = 0;
    } else {
      rcVertex[ 0 ].x     = 0;
      rcVertex[ 0 ].y     = 0;
      rcVertex[ 0 ].Red   = ( unsigned short ) ( GetRValue( crFrom ) << 8 );
      rcVertex[ 0 ].Green = ( unsigned short ) ( GetGValue( crFrom ) << 8 );
      rcVertex[ 0 ].Blue  = ( unsigned short ) ( GetBValue( crFrom ) << 8 );
      rcVertex[ 0 ].Alpha = 0;

      rcVertex[ 1 ].x     = cx / 2;
      rcVertex[ 1 ].y     = cy;
      rcVertex[ 1 ].Red   = ( unsigned short ) ( GetRValue( crMiddle ) << 8 );
      rcVertex[ 1 ].Green = ( unsigned short ) ( GetGValue( crMiddle ) << 8 );
      rcVertex[ 1 ].Blue  = ( unsigned short ) ( GetBValue( crMiddle ) << 8 );
      rcVertex[ 1 ].Alpha = 0;

      rcVertex[ 2 ].x     = cx / 2;
      rcVertex[ 2 ].y     = 0;
      rcVertex[ 2 ].Red   = ( unsigned short ) ( GetRValue( crMiddle ) << 8 );
      rcVertex[ 2 ].Green = ( unsigned short ) ( GetGValue( crMiddle ) << 8 );
      rcVertex[ 2 ].Blue  = ( unsigned short ) ( GetBValue( crMiddle ) << 8 );
      rcVertex[ 2 ].Alpha = 0;

      rcVertex[ 3 ].x     = cx;
      rcVertex[ 3 ].y     = cy;
      rcVertex[ 3 ].Red   = ( unsigned short ) ( GetRValue( crTo ) << 8 );
      rcVertex[ 3 ].Green = ( unsigned short ) ( GetGValue( crTo ) << 8 );
      rcVertex[ 3 ].Blue  = ( unsigned short ) ( GetBValue( crTo ) << 8 );
      rcVertex[ 3 ].Alpha = 0;
      }

      SelectObject( memDC, memBmp );

      if( s_hDLL != NULL )
      {
         if( f_GradientFill != NULL )
         {
            f_GradientFill( memDC, rcVertex, 4, &gRect, 2,
                            bVert ? GRADIENT_FILL_RECT_V : GRADIENT_FILL_RECT_H );
         }
      }
      pGradientBrush = CreatePatternBrush( memBmp );

      DeleteObject( memBmp );
      DeleteObject( memDC );

   }

   return pGradientBrush;
}

HB_FUNC( FILLRGN )
{
   HWND  hWnd = hmg_par_raw_HWND( 1 );
   HDC   hDC;
   BOOL  bDC = FALSE;

   if( IsWindow( hWnd ) )
   {
      hDC = GetDC( hWnd );
      bDC = TRUE;
   }
   else
   {
      hDC = hmg_par_raw_HDC( 1 );
   }

   if( GetObjectType( ( HGDIOBJ ) hDC ) == OBJ_DC )
   {
      HRGN hRgn = CreateRoundRectRgn(hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), hb_parni( 7 ));  

      hmg_ret_NINT( FillRgn( hDC, hRgn, hmg_par_raw_HBRUSH( 8 ) ) );

      if( bDC )
      {
         ReleaseDC( hWnd, hDC );
      }
   }
   else
   {
      hb_retni( 0 );
   }
}

#pragma ENDDUMP
