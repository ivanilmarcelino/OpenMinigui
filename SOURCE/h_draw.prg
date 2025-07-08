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

#include "minigui.ch"

/*-----------------------------------------------------------------------------
* FUNCTION drawtextout( window, row, col, string, fontcolor, backcolor, fontname, fontsize, bold, italic, underline, strikeout, transparent, angle, once )
* 
*
*  Purpose:
*     Draws text on a specified window (form or DC).  It also stores the drawing
*     operation in an array so that it can be redrawn when the window is refreshed.
*
*  Parameters:
*     window      - The window handle or the name of the form where the text will be drawn.
*     row         - The row coordinate (Y-coordinate) of the top-left corner of the text.
*     col         - The column coordinate (X-coordinate) of the top-left corner of the text.
*     string      - The text string to be drawn.
*     fontcolor   - An array containing the RGB values for the font color (e.g., {255, 0, 0} for red).
*     backcolor   - An array containing the RGB values for the background color.
*     fontname    - The name of the font to use (e.g., "Arial").
*     fontsize    - The size of the font in points.
*     bold        - A logical value indicating whether the font should be bold (.T. for bold, .F. for normal).
*     italic      - A logical value indicating whether the font should be italic (.T. for italic, .F. for normal).
*     underline   - A logical value indicating whether the font should be underlined (.T. for underlined, .F. for normal).
*     strikeout   - A logical value indicating whether the font should be struck out (.T. for struck out, .F. for normal).
*     transparent - A logical value indicating whether the background should be transparent (.T. for transparent, .F. for opaque).
*     angle       - The angle at which the text should be drawn (in degrees).
*     once        - A logical value indicating whether the text should be drawn only once and not stored for redraws (.T. for once, .F. for persistent).
*
*  Return Value:
*     NIL
*
*  Notes:
*     - The function retrieves the window handle based on the provided window parameter (either a handle or a form name).
*     - It uses default values for font name, font size, background color, font color, and angle if they are not provided.
*     - The textdraw function (assumed to be a low-level drawing function) is called to perform the actual drawing.
*     - If the 'once' parameter is .F., the drawing operation is added to an array (_HMG_aFormGraphTasks) for later redrawing.
*     - The torow and tocol variables are calculated to define the bottom-right corner of the text area, taking into account transparency and angle.
*     - Error handling: Checks if the window handle is valid or if it's a DC object.
*/
FUNCTION drawtextout( window, row, col, string, fontcolor, backcolor, fontname, fontsize, bold, italic, underline, strikeout, transparent, angle, once )
   LOCAL FormHandle, FontHandle
   LOCAL torow, tocol
   LOCAL i

   IF hb_IsString( window )
      IF ( i := GetFormIndex( window ) ) > 0
         FormHandle := _HMG_aFormHandles [i]
      ENDIF
   ELSE
      FormHandle := window
   ENDIF

   IF IsWindowHandle( FormHandle ) .OR. ( GetObjectType( FormHandle ) == OBJ_DC )
      IF ( FontHandle := GetFontHandle( FontName ) ) != 0
         GetFontParamByRef( FontHandle, @FontName, @FontSize, @bold, @italic, @underline, @strikeout, @angle )
      ENDIF

      __defaultNIL( @fontname, _HMG_DefaultFontName )
      __defaultNIL( @fontsize, _HMG_DefaultFontSize )
      hb_default( @backcolor, { 255, 255, 255 } )
      hb_default( @fontcolor, { 0, 0, 0 } )
      hb_default( @angle, 0 )
      hb_default( @once, .F. )

      torow := row + iif( transparent .OR. !Empty( angle ), 0, fontsize + 4 )
      tocol := col + ( Len( string ) - 1 ) * fontsize
      textdraw( FormHandle, row, col, string, torow, tocol, fontcolor, backcolor, fontname, fontsize, bold, italic, underline, strikeout, transparent, angle )
      IF ! once
         AAdd ( _HMG_aFormGraphTasks [i], {|| textdraw( FormHandle,row,col,string,torow,tocol,fontcolor,backcolor,fontname,fontsize,bold,italic,underline,strikeout,transparent,angle ) } )
      ENDIF
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
* FUNCTION drawline( window, row, col, row1, col1, penrgb, penwidth )
* 
*
*  Purpose:
*     Draws a line on a specified window (form).  It also stores the drawing
*     operation in an array so that it can be redrawn when the window is refreshed.
*
*  Parameters:
*     window   - The window handle or the name of the form where the line will be drawn.
*     row      - The row coordinate (Y-coordinate) of the starting point of the line.
*     col      - The column coordinate (X-coordinate) of the starting point of the line.
*     row1     - The row coordinate (Y-coordinate) of the ending point of the line.
*     col1     - The column coordinate (X-coordinate) of the ending point of the line.
*     penrgb   - An array containing the RGB values for the pen color (e.g., {255, 0, 0} for red).
*     penwidth - The width of the pen in pixels.
*
*  Return Value:
*     NIL
*
*  Notes:
*     - The function retrieves the window handle based on the provided window parameter (either a handle or a form name).
*     - It uses default values for pen color and pen width if they are not provided.
*     - The linedraw function (assumed to be a low-level drawing function) is called to perform the actual drawing.
*     - The drawing operation is added to an array (_HMG_aFormGraphTasks) for later redrawing.
*     - Error handling: Checks if the form index is valid.
*/
FUNCTION drawline( window, row, col, row1, col1, penrgb, penwidth )
   LOCAL FormHandle
   LOCAL i

   IF ( i := GetFormIndex ( Window ) ) > 0
      FormHandle := _HMG_aFormHandles [i]
      hb_default( @penrgb, { 0, 0, 0 } )
      hb_default( @penwidth, 1 )

      linedraw( FormHandle, row, col, row1, col1, penrgb, penwidth )
      AAdd ( _HMG_aFormGraphTasks [i] , {|| linedraw( FormHandle,row,col,row1,col1,penrgb,penwidth ) } )
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
* FUNCTION drawrect( window, row, col, row1, col1, penrgb, penwidth, fillrgb )
* 
*
*  Purpose:
*     Draws a rectangle on a specified window (form).  It also stores the drawing
*     operation in an array so that it can be redrawn when the window is refreshed.
*
*  Parameters:
*     window   - The window handle or the name of the form where the rectangle will be drawn.
*     row      - The row coordinate (Y-coordinate) of the top-left corner of the rectangle.
*     col      - The column coordinate (X-coordinate) of the top-left corner of the rectangle.
*     row1     - The row coordinate (Y-coordinate) of the bottom-right corner of the rectangle.
*     col1     - The column coordinate (X-coordinate) of the bottom-right corner of the rectangle.
*     penrgb   - An array containing the RGB values for the pen color (e.g., {255, 0, 0} for red).
*     penwidth - The width of the pen in pixels.
*     fillrgb  - An array containing the RGB values for the fill color (e.g., {255, 255, 255} for white). If not an array, it defaults to white.
*
*  Return Value:
*     NIL
*
*  Notes:
*     - The function retrieves the window handle based on the provided window parameter (either a handle or a form name).
*     - It uses default values for pen color, pen width, and fill color if they are not provided.
*     - The rectdraw function (assumed to be a low-level drawing function) is called to perform the actual drawing.
*     - The drawing operation is added to an array (_HMG_aFormGraphTasks) for later redrawing.
*     - The 'fill' variable determines whether the rectangle should be filled based on whether fillrgb is an array.
*     - Error handling: Checks if the form index is valid.
*/
FUNCTION drawrect( window, row, col, row1, col1, penrgb, penwidth, fillrgb )
   LOCAL FormHandle
   LOCAL i
   LOCAL fill

   IF ( i := GetFormIndex ( Window ) ) > 0
      FormHandle := _HMG_aFormHandles [i]
      hb_default( @penrgb, { 0, 0, 0 } )
      hb_default( @penwidth, 1 )
      IF !( fill := ISARRAY( fillrgb ) )
         fillrgb := { 255, 255, 255 }
      ENDIF

      rectdraw( FormHandle, row, col, row1, col1, penrgb, penwidth, fillrgb, fill )
      AAdd ( _HMG_aFormGraphTasks [i] , {|| rectdraw( FormHandle,row,col,row1,col1,penrgb,penwidth,fillrgb,fill ) } )
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
* FUNCTION drawroundrect( window, row, col, row1, col1, width, height, penrgb, penwidth, fillrgb )
* 
*
*  Purpose:
*     Draws a rounded rectangle on a specified window (form).  It also stores the drawing
*     operation in an array so that it can be redrawn when the window is refreshed.
*
*  Parameters:
*     window   - The window handle or the name of the form where the rounded rectangle will be drawn.
*     row      - The row coordinate (Y-coordinate) of the top-left corner of the rectangle.
*     col      - The column coordinate (X-coordinate) of the top-left corner of the rectangle.
*     row1     - The row coordinate (Y-coordinate) of the bottom-right corner of the rectangle.
*     col1     - The column coordinate (X-coordinate) of the bottom-right corner of the rectangle.
*     width    - The width of the rounded corner.
*     height   - The height of the rounded corner.
*     penrgb   - An array containing the RGB values for the pen color (e.g., {255, 0, 0} for red).
*     penwidth - The width of the pen in pixels.
*     fillrgb  - An array containing the RGB values for the fill color (e.g., {255, 255, 255} for white). If not an array, it defaults to white.
*
*  Return Value:
*     NIL
*
*  Notes:
*     - The function retrieves the window handle based on the provided window parameter (either a handle or a form name).
*     - It uses default values for pen color, pen width, and fill color if they are not provided.
*     - The roundrectdraw function (assumed to be a low-level drawing function) is called to perform the actual drawing.
*     - The drawing operation is added to an array (_HMG_aFormGraphTasks) for later redrawing.
*     - The 'fill' variable determines whether the rectangle should be filled based on whether fillrgb is an array.
*     - Error handling: Checks if the form index is valid.
*/
FUNCTION drawroundrect( window, row, col, row1, col1, width, height, penrgb, penwidth, fillrgb )
   LOCAL FormHandle
   LOCAL i
   LOCAL fill

   IF ( i := GetFormIndex ( Window ) ) > 0
      FormHandle := _HMG_aFormHandles [i]
      hb_default( @penrgb, { 0, 0, 0 } )
      hb_default( @penwidth, 1 )
      IF !( fill := ISARRAY( fillrgb ) )
         fillrgb := { 255, 255, 255 }
      ENDIF

      roundrectdraw( FormHandle, row, col, row1, col1, width, height, penrgb, penwidth, fillrgb, fill )
      AAdd ( _HMG_aFormGraphTasks [i] , {|| roundrectdraw( FormHandle,row,col,row1,col1,width,height,penrgb,penwidth,fillrgb,fill ) } )
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
* FUNCTION drawellipse( window, row, col, row1, col1, penrgb, penwidth, fillrgb )
* 
*
*  Purpose:
*     Draws an ellipse on a specified window (form).  It also stores the drawing
*     operation in an array so that it can be redrawn when the window is refreshed.
*
*  Parameters:
*     window   - The window handle or the name of the form where the ellipse will be drawn.
*     row      - The row coordinate (Y-coordinate) of the top-left corner of the bounding rectangle.
*     col      - The column coordinate (X-coordinate) of the top-left corner of the bounding rectangle.
*     row1     - The row coordinate (Y-coordinate) of the bottom-right corner of the bounding rectangle.
*     col1     - The column coordinate (X-coordinate) of the bottom-right corner of the bounding rectangle.
*     penrgb   - An array containing the RGB values for the pen color (e.g., {255, 0, 0} for red).
*     penwidth - The width of the pen in pixels.
*     fillrgb  - An array containing the RGB values for the fill color (e.g., {255, 255, 255} for white). If not an array, it defaults to white.
*
*  Return Value:
*     NIL
*
*  Notes:
*     - The function retrieves the window handle based on the provided window parameter (either a handle or a form name).
*     - It uses default values for pen color, pen width, and fill color if they are not provided.
*     - The ellipsedraw function (assumed to be a low-level drawing function) is called to perform the actual drawing.
*     - The drawing operation is added to an array (_HMG_aFormGraphTasks) for later redrawing.
*     - The 'fill' variable determines whether the ellipse should be filled based on whether fillrgb is an array.
*     - Error handling: Checks if the form index is valid.
*/
FUNCTION drawellipse( window, row, col, row1, col1, penrgb, penwidth, fillrgb )
   LOCAL FormHandle
   LOCAL i
   LOCAL fill

   IF ( i := GetFormIndex ( Window ) ) > 0
      FormHandle := _HMG_aFormHandles [i]
      hb_default( @penrgb, { 0, 0, 0 } )
      hb_default( @penwidth, 1 )
      IF !( fill := ISARRAY( fillrgb ) )
         fillrgb := { 255, 255, 255 }
      ENDIF

      ellipsedraw( FormHandle, row, col, row1, col1, penrgb, penwidth, fillrgb, fill )
      AAdd ( _HMG_aFormGraphTasks [i] , {|| ellipsedraw( FormHandle,row,col,row1,col1,penrgb,penwidth,fillrgb,fill ) } )
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
* FUNCTION drawarc( window, row, col, row1, col1, rowr, colr, rowr1, colr1, penrgb, penwidth )
* 
*
*  Purpose:
*     Draws an arc on a specified window (form).  It also stores the drawing
*     operation in an array so that it can be redrawn when the window is refreshed.
*
*  Parameters:
*     window   - The window handle or the name of the form where the arc will be drawn.
*     row      - The row coordinate (Y-coordinate) of the top-left corner of the bounding rectangle.
*     col      - The column coordinate (X-coordinate) of the top-left corner of the bounding rectangle.
*     row1     - The row coordinate (Y-coordinate) of the bottom-right corner of the bounding rectangle.
*     col1     - The column coordinate (X-coordinate) of the bottom-right corner of the bounding rectangle.
*     rowr     - The row coordinate (Y-coordinate) of the starting point of the arc.
*     colr     - The column coordinate (X-coordinate) of the starting point of the arc.
*     rowr1    - The row coordinate (Y-coordinate) of the ending point of the arc.
*     colr1    - The column coordinate (X-coordinate) of the ending point of the arc.
*     penrgb   - An array containing the RGB values for the pen color (e.g., {255, 0, 0} for red).
*     penwidth - The width of the pen in pixels.
*
*  Return Value:
*     NIL
*
*  Notes:
*     - The function retrieves the window handle based on the provided window parameter (either a handle or a form name).
*     - It uses default values for pen color and pen width if they are not provided.
*     - The arcdraw function (assumed to be a low-level drawing function) is called to perform the actual drawing.
*     - The drawing operation is added to an array (_HMG_aFormGraphTasks) for later redrawing.
*     - Error handling: Checks if the form index is valid.
*/
FUNCTION drawarc( window, row, col, row1, col1, rowr, colr, rowr1, colr1, penrgb, penwidth )
   LOCAL FormHandle
   LOCAL i

   IF ( i := GetFormIndex ( Window ) ) > 0
      FormHandle := _HMG_aFormHandles [i]
      hb_default( @penrgb, { 0, 0, 0 } )
      hb_default( @penwidth, 1 )

      arcdraw( FormHandle, row, col, row1, col1, rowr, colr, rowr1, colr1, penrgb, penwidth )
      AAdd ( _HMG_aFormGraphTasks [i] , {|| arcdraw( FormHandle,row,col,row1,col1,rowr,colr,rowr1,colr1,penrgb,penwidth ) } )
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
* FUNCTION drawpie( window, row, col, row1, col1, rowr, colr, rowr1, colr1, penrgb, penwidth, fillrgb )
* 
*
*  Purpose:
*     Draws a pie slice on a specified window (form).  It also stores the drawing
*     operation in an array so that it can be redrawn when the window is refreshed.
*
*  Parameters:
*     window   - The window handle or the name of the form where the pie slice will be drawn.
*     row      - The row coordinate (Y-coordinate) of the top-left corner of the bounding rectangle.
*     col      - The column coordinate (X-coordinate) of the top-left corner of the bounding rectangle.
*     row1     - The row coordinate (Y-coordinate) of the bottom-right corner of the bounding rectangle.
*     col1     - The column coordinate (X-coordinate) of the bottom-right corner of the bounding rectangle.
*     rowr     - The row coordinate (Y-coordinate) of the starting point of the pie slice.
*     colr     - The column coordinate (X-coordinate) of the starting point of the pie slice.
*     rowr1    - The row coordinate (Y-coordinate) of the ending point of the pie slice.
*     colr1    - The column coordinate (X-coordinate) of the ending point of the pie slice.
*     penrgb   - An array containing the RGB values for the pen color (e.g., {255, 0, 0} for red).
*     penwidth - The width of the pen in pixels.
*     fillrgb  - An array containing the RGB values for the fill color (e.g., {255, 255, 255} for white). If not an array, it defaults to white.
*
*  Return Value:
*     NIL
*
*  Notes:
*     - The function retrieves the window handle based on the provided window parameter (either a handle or a form name).
*     - It uses default values for pen color, pen width, and fill color if they are not provided.
*     - The piedraw function (assumed to be a low-level drawing function) is called to perform the actual drawing.
*     - The drawing operation is added to an array (_HMG_aFormGraphTasks) for later redrawing.
*     - The 'fill' variable determines whether the pie slice should be filled based on whether fillrgb is an array.
*     - Error handling: Checks if the form index is valid.
*/
FUNCTION drawpie( window, row, col, row1, col1, rowr, colr, rowr1, colr1, penrgb, penwidth, fillrgb )
   LOCAL FormHandle
   LOCAL i
   LOCAL fill

   IF ( i := GetFormIndex ( Window ) ) > 0
      FormHandle := _HMG_aFormHandles [i]
      hb_default( @penrgb, { 0, 0, 0 } )
      hb_default( @penwidth, 1 )
      IF !( fill := ISARRAY( fillrgb ) )
         fillrgb := { 255, 255, 255 }
      ENDIF

      piedraw( FormHandle, row, col, row1, col1, rowr, colr, rowr1, colr1, penrgb, penwidth, fillrgb, fill )
      AAdd( _HMG_aFormGraphTasks [i] , {|| piedraw( FormHandle,row,col,row1,col1,rowr,colr,rowr1,colr1,penrgb,penwidth,fillrgb,fill ) } )
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
* FUNCTION drawpolygon( window, apoints, penrgb, penwidth, fillrgb )
* 
*
*  Purpose:
*     Draws a polygon on a specified window (form).  It also stores the drawing
*     operation in an array so that it can be redrawn when the window is refreshed.
*
*  Parameters:
*     window   - The window handle or the name of the form where the polygon will be drawn.
*     apoints  - An array of arrays, where each inner array contains the Y and X coordinates of a vertex (e.g., {{y1, x1}, {y2, x2}, ...}).
*     penrgb   - An array containing the RGB values for the pen color (e.g., {255, 0, 0} for red).
*     penwidth - The width of the pen in pixels.
*     fillrgb  - An array containing the RGB values for the fill color (e.g., {255, 255, 255} for white). If not an array, it defaults to white.
*
*  Return Value:
*     NIL
*
*  Notes:
*     - The function retrieves the window handle based on the provided window parameter (either a handle or a form name).
*     - It uses default values for pen color, pen width, and fill color if they are not provided.
*     - The AEval function iterates through the apoints array to extract the Y and X coordinates into separate arrays (yarr and xarr).
*     - The polygondraw function (assumed to be a low-level drawing function) is called to perform the actual drawing.
*     - The drawing operation is added to an array (_HMG_aFormGraphTasks) for later redrawing.
*     - The 'fill' variable determines whether the polygon should be filled based on whether fillrgb is an array.
*     - Error handling: Checks if the form index is valid.
*/
FUNCTION drawpolygon( window, apoints, penrgb, penwidth, fillrgb )
   LOCAL FormHandle
   LOCAL i
   LOCAL fill
   LOCAL xarr := {}, yarr := {}

   IF ( i := GetFormIndex ( Window ) ) > 0
      FormHandle := _HMG_aFormHandles [i]
      hb_default( @penrgb, { 0, 0, 0 } )
      hb_default( @penwidth, 1 )
      IF !( fill := ISARRAY( fillrgb ) )
         fillrgb := { 255, 255, 255 }
      ENDIF

      AEval( apoints, { | x | AAdd( yarr, x[1] ), AAdd( xarr, x[2] ) } )
      polygondraw( FormHandle, xarr, yarr, penrgb, penwidth, fillrgb, fill )
      AAdd( _HMG_aFormGraphTasks [i] , {|| polygondraw( FormHandle,xarr,yarr,penrgb,penwidth,fillrgb,fill ) } )
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
* FUNCTION drawpolybezier( window, apoints, penrgb, penwidth )
* 
*
*  Purpose:
*     Draws a polybezier curve on a specified window (form).  It also stores the drawing
*     operation in an array so that it can be redrawn when the window is refreshed.
*
*  Parameters:
*     window   - The window handle or the name of the form where the polybezier curve will be drawn.
*     apoints  - An array of arrays, where each inner array contains the Y and X coordinates of a control point (e.g., {{y1, x1}, {y2, x2}, ...}).
*     penrgb   - An array containing the RGB values for the pen color (e.g., {255, 0, 0} for red).
*     penwidth - The width of the pen in pixels.
*
*  Return Value:
*     NIL
*
*  Notes:
*     - The function retrieves the window handle based on the provided window parameter (either a handle or a form name).
*     - It uses default values for pen color and pen width if they are not provided.
*     - The AEval function iterates through the apoints array to extract the Y and X coordinates into separate arrays (yarr and xarr).
*     - The polybezierdraw function (assumed to be a low-level drawing function) is called to perform the actual drawing.
*     - The drawing operation is added to an array (_HMG_aFormGraphTasks) for later redrawing.
*     - Error handling: Checks if the form index is valid.
*/
FUNCTION drawpolybezier( window, apoints, penrgb, penwidth )
   LOCAL FormHandle
   LOCAL xarr := {}, yarr := {}
   LOCAL i

   IF ( i := GetFormIndex ( Window ) ) > 0
      FormHandle := _HMG_aFormHandles [i]
      hb_default( @penrgb, { 0, 0, 0 } )
      hb_default( @penwidth, 1 )

      AEval( apoints, { | x | AAdd( yarr, x[1] ), AAdd( xarr, x[2] ) } )
      polybezierdraw( FormHandle, xarr, yarr, penrgb, penwidth )
      AAdd( _HMG_aFormGraphTasks [i] , {|| polybezierdraw( FormHandle,xarr,yarr,penrgb,penwidth ) } )
   ENDIF

RETURN NIL

#define COLOR_BTNFACE	15
/*-----------------------------------------------------------------------------
* FUNCTION HMG_DrawIcon( window, icon, row, col, w, h, rgb, transparent )
* 
*
*  Purpose:
*     Draws an icon on a specified window (form).  It also stores the drawing
*     operation in an array so that it can be redrawn when the window is refreshed.
*
*  Parameters:
*     window      - The window handle or the name of the form where the icon will be drawn.
*     icon        - The icon to draw. Can be either a numeric icon handle or a string representing the icon name.
*     row         - The row coordinate (Y-coordinate) of the top-left corner of the icon.
*     col         - The column coordinate (X-coordinate) of the top-left corner of the icon.
*     w           - The width of the icon. If 0, the default icon width is used.
*     h           - The height of the icon. If 0, the default icon height is used.
*     rgb         - An array containing the RGB values for the background color of the icon (used when transparent is .F.).
*     transparent - A logical value indicating whether the icon background should be transparent (.T. for transparent, .F. for opaque).
*
*  Return Value:
*     NIL
*
*  Notes:
*     - The function retrieves the window handle based on the provided window parameter (either a handle or a form name).
*     - It uses default values for width, height, transparency, and background color if they are not provided.
*     - If transparent is .T., the background color is set to the form's background color.
*     - If rgb is an array, it's converted to a single RGB value using the RGB() function.
*     - If the icon is a number, it's treated as an icon handle. If it's a string, it's treated as an icon name and loaded using LoadIconByName().
*     - The DrawIconEx function (assumed to be a low-level drawing function) is called to perform the actual drawing.
*     - The drawing operation is added to an array (_HMG_aFormGraphTasks) for later redrawing.
*     - Error handling: Checks if the form index is valid.
*/
FUNCTION HMG_DrawIcon( window, icon, row, col, w, h, rgb, transparent )
   LOCAL FormHandle
   LOCAL backcolor
   LOCAL i

   IF ( i := GetFormIndex ( Window ) ) > 0
      FormHandle := _HMG_aFormHandles [i]
      hb_default( @w, 0 )
      hb_default( @h, 0 )
      hb_default( @transparent, .F. )
      IF transparent
         backcolor := _HMG_aFormBkColor [i]
         IF IsArrayRGB( backcolor )
            rgb := RGB( backcolor [1], backcolor [2], backcolor [3] )
         ENDIF
      ELSE
         IF IsArrayRGB( rgb )
            rgb := RGB( rgb [1], rgb [2], rgb [3] )
         ENDIF
      ENDIF
      hb_default( @rgb, GetSysColor( COLOR_BTNFACE ) )

      IF ISNUMERIC( icon )
         DrawIconEx( FormHandle, Col, Row, icon, w, h, rgb, .F. )
         AAdd( _HMG_aFormGraphTasks [i] , {|| DrawIconEx( FormHandle, Col, Row, icon, w, h, rgb, .F. ) } )
      ELSEIF ISSTRING( icon )
         DrawIconEx( FormHandle, Col, Row, LoadIconByName( icon, w, h ), w, h, rgb, .T. )
         AAdd( _HMG_aFormGraphTasks [i] , {|| DrawIconEx( FormHandle, Col, Row, LoadIconByName( icon, w, h ), w, h, rgb, .T. ) } )
      ENDIF
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
// FUNCTION HMG_DrawSysIcon( window, cIconDll, icon, row, col, w, h, rgb, transparent )
//
// Draws a system icon from a specified DLL on a window.
//
// Parameters:
//   window      - The window handle or name where the icon will be drawn.
//   cIconDll    - The path to the DLL containing the icon. Defaults to "imageres.dll" in the system folder.
//   icon        - The index of the icon within the DLL.
//   row         - The row coordinate (Y-axis) where the icon will be drawn.
//   col         - The column coordinate (X-axis) where the icon will be drawn.
//   w           - The width of the icon. If 0, the default icon width is used.
//   h           - The height of the icon. If 0, the default icon height is used.
//   rgb         - The RGB color value for the icon's background. If transparent is .T., this is used for transparency.
//                 If an array is passed, it's assumed to be an RGB array (e.g., {255, 0, 0} for red).
//   transparent - A logical value indicating whether the icon should be drawn with transparency.
//                 If .T., the 'rgb' parameter is used as the transparent color.
//
// Returns:
//   NIL
//
// Remarks:
//   - This function retrieves the form handle based on the 'window' parameter.
//   - It uses ExtractIcon to extract the icon from the specified DLL.
//   - It adds a task to the form's graph tasks array (_HMG_aFormGraphTasks) to redraw the icon when the form is refreshed.
//   - The default DLL is "imageres.dll" located in the system folder.
//   - If 'transparent' is .T., the form's background color is used as the transparent color if 'rgb' is not provided.
//   - If 'rgb' is not provided, the system button face color (COLOR_BTNFACE) is used as the default.
//-----------------------------------------------------------------------------*/
FUNCTION HMG_DrawSysIcon( window, cIconDll, icon, row, col, w, h, rgb, transparent )
   LOCAL FormHandle
   LOCAL backcolor
   LOCAL i

   IF ( i := GetFormIndex ( Window ) ) > 0
      FormHandle := _HMG_aFormHandles [i]
      hb_default( @w, 0 )
      hb_default( @h, 0 )
      hb_default( @transparent, .F. )
      IF transparent
         backcolor := _HMG_aFormBkColor [i]
         IF IsArrayRGB( backcolor )
            rgb := RGB( backcolor [1], backcolor [2], backcolor [3] )
         ENDIF
      ELSE
         IF IsArrayRGB( rgb )
            rgb := RGB( rgb [1], rgb [2], rgb [3] )
         ENDIF
      ENDIF
      hb_default( @rgb, GetSysColor( COLOR_BTNFACE ) )
      hb_default( @cIconDll, System.SystemFolder + hb_ps() + "imageres.dll" )

      IF ISNUMERIC( icon )
         AAdd( _HMG_aFormGraphTasks [i] , {|| DrawIconEx( FormHandle, Col, Row, ExtractIcon( cIconDll, icon ), w, h, rgb, .T. ) } )
      ENDIF
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
// FUNCTION EraseWindow( window )
//
// Erases the graphical elements drawn on a window by clearing the graph tasks array.
//
// Parameters:
//   window - The window handle or name to erase.
//
// Returns:
//   NIL
//
// Remarks:
//   - This function retrieves the form handle based on the 'window' parameter.
//   - It clears the _HMG_aFormGraphTasks array associated with the window, effectively removing all stored drawing tasks.
//   - It then calls RedrawWindow to force a repaint of the window, which will now be cleared of the previously drawn elements.
//   - It checks if the form has been deleted before attempting to erase it.
//-----------------------------------------------------------------------------*/
FUNCTION EraseWindow( window )
   LOCAL i

   IF ( i := GetFormIndex ( Window ) ) > 0
      IF ! _HMG_aFormDeleted [i]
         IF ISARRAY ( _HMG_aFormGraphTasks [i] )
            ASize ( _HMG_aFormGraphTasks [i], 0 )
            RedrawWindow ( _HMG_aFormHandles [i] )
         ENDIF
      ENDIF
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
// FUNCTION DrawWindowBoxIn( window, row, col, rowr, colr )
//
// Draws an inset box (sunken effect) on a window.
//
// Parameters:
//   window - The window handle or name where the box will be drawn.
//   row    - The top row coordinate (Y-axis) of the box.
//   col    - The left column coordinate (X-axis) of the box.
//   rowr   - The bottom row coordinate (Y-axis) of the box.
//   colr   - The right column coordinate (X-axis) of the box.
//
// Returns:
//   NIL
//
// Remarks:
//   - This function retrieves the form handle based on the 'window' parameter.
//   - It obtains the device context (DC) of the window.
//   - It calls WndBoxIn to draw the inset box using the device context and coordinates.
//   - It releases the device context.
//   - It adds a task to the form's graph tasks array (_HMG_aFormGraphTasks) to redraw the box when the form is refreshed.
//-----------------------------------------------------------------------------*/
FUNCTION DrawWindowBoxIn( window, row, col, rowr, colr )
   LOCAL FormHandle
   LOCAL hDC
   LOCAL i

   IF ( i := GetFormIndex ( Window ) ) > 0
      FormHandle := _HMG_aFormHandles [i]
      hDC := GetDC ( FormHandle )
      WndBoxIn ( hDC, row, col, rowr, colr )
      ReleaseDC ( FormHandle, hDC )
      AAdd( _HMG_aFormGraphTasks [i] , {|| WndBoxIn( ( hDC := GetDC( FormHandle ) ), row, col, rowr, colr ), ReleaseDC( FormHandle, hDC ) } )
   ENDIF

RETURN NIL

/*-----------------------------------------------------------------------------
// FUNCTION DrawWindowBoxRaised( window, row, col, rowr, colr )
//
// Draws a raised box (3D effect) on a window.
//
// Parameters:
//   window - The window handle or name where the box will be drawn.
//   row    - The top row coordinate (Y-axis) of the box.
//   col    - The left column coordinate (X-axis) of the box.
//   rowr   - The bottom row coordinate (Y-axis) of the box.
//   colr   - The right column coordinate (X-axis) of the box.
//
// Returns:
//   NIL
//
// Remarks:
//   - This function retrieves the form handle based on the 'window' parameter.
//   - It obtains the device context (DC) of the window.
//   - It calls WndBoxRaised to draw the raised box using the device context and coordinates.
//   - It releases the device context.
//   - It adds a task to the form's graph tasks array (_HMG_aFormGraphTasks) to redraw the box when the form is refreshed.
//-----------------------------------------------------------------------------*/
FUNCTION DrawWindowBoxRaised( window, row, col, rowr, colr )
   LOCAL FormHandle
   LOCAL hDC
   LOCAL i

   IF ( i := GetFormIndex ( Window ) ) > 0
      FormHandle := _HMG_aFormHandles [i]
      hDC := GetDC ( FormHandle )
      WndBoxRaised ( hDC, row, col, rowr, colr )
      ReleaseDC ( FormHandle, hDC )
      AAdd( _HMG_aFormGraphTasks [i] , {|| WndBoxRaised( ( hDC := GetDC( FormHandle ) ), row, col, rowr, colr ), ReleaseDC( FormHandle, hDC ) } )
   ENDIF

RETURN NIL
