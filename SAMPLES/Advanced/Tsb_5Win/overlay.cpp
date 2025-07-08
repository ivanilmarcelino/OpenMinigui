/*
 * Harbour MiniGUI Demo
 *
 * (c) 2019 Artyom Verchenko <artyomskynet@gmail.com>
 *     31.05.2019
 */

#include <windows.h>
#include <hbapi.h>
#include <vector>
#include <map>
#include <cstdlib>

typedef std::vector<HWND> HWND_ARRAY;
std::map<HWND, HWND_ARRAY *> arrOverlays;
std::map<HWND, HBRUSH>       overlaysBrushes;

HWND WIN32_CreateOverlay( HINSTANCE hInstance, HWND hwndParent, BYTE cRed, BYTE cGreen, BYTE cBlue, BYTE cAlpha );
void WIN32_VanishOverlay( HWND hWndParent, BOOL isShow );
void WIN32_UpdateOverlay( HWND hWndParent );

static LRESULT CALLBACK WndProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam );
static void MakeWindowTransparent( HWND hWnd, BYTE level );
static void SetBackgroundColor( HWND hWnd, BYTE red, BYTE green, BYTE blue );
static RECT GetWorkingArea( HWND hwnd );
static void AdjustParentCords( HWND hWnd, HWND hwndParent );

HB_FUNC( OVERLAYCREATE ) // hWnd -> pDisp
{
   HINSTANCE hInstance = GetModuleHandle( NULL );
   HWND      hWnd      = ( HWND ) hb_parnl( 1 );
   BYTE      colRed    = ( BYTE ) hb_parni( 2 );
   BYTE      colGreen  = ( BYTE ) hb_parni( 3 );
   BYTE      colBlue   = ( BYTE ) hb_parni( 4 );
   BYTE      alpha     = ( BYTE ) hb_parni( 5 );
   HWND      hOverlay  = WIN32_CreateOverlay( hInstance, hWnd, colRed, colGreen, colBlue, alpha );

   if( ! arrOverlays.count( hWnd ) )
      arrOverlays[ hWnd ] = new HWND_ARRAY();
   arrOverlays[ hWnd ]->push_back( hOverlay );
}

HB_FUNC( OVERLAYCLOSE )
{
   HWND hWndParent = ( HWND ) hb_parnl( 1 );

   if( ! arrOverlays.count( hWndParent ) )
      return;
   HWND_ARRAY * array = arrOverlays[ hWndParent ];
   for( std::vector<int>::size_type i = 0; i != array->size(); i++ )
   {
      HWND hWnd = array->at( i );
      DestroyWindow( hWnd );
      if( overlaysBrushes.count( hWnd ) > 0 )
      {
         HBRUSH hBrush = overlaysBrushes[ hWnd ];
         DeleteObject( hBrush );
      }
   }
   array->clear();
   delete arrOverlays[ hWndParent ];
   arrOverlays.erase( hWndParent );

   // WinXP HACK - Обновяем окно родителя, чтобы убрать графические баги
   InvalidateRect( hWndParent, NULL, TRUE );
}

HB_FUNC( OVERLAYWNDPROC )
{
   HWND hWnd    = ( HWND ) hb_parnl( 1 );
   long message = hb_parnl( 2 );

   switch( message )
   {
      case WM_MOVE:
         WIN32_UpdateOverlay( hWnd );
      case WM_SIZE:
         WIN32_UpdateOverlay( hWnd );
   }
}

HB_FUNC( OVERLAYUPDATE )
{
   HWND hWndParent = ( HWND ) hb_parnl( 1 );

   WIN32_UpdateOverlay( hWndParent );
}

HB_FUNC( OVERLAYVANISH )
{
   HWND hWndParent = ( HWND ) hb_parnl( 1 );
   BOOL isShow     = hb_parl( 2 );

   WIN32_VanishOverlay( hWndParent, isShow );
}

void WIN32_UpdateOverlay( HWND hWndParent )
{
   if( ! arrOverlays.count( hWndParent ) )
      return;
   HWND_ARRAY * array = arrOverlays[ hWndParent ];
   for( std::vector<int>::size_type i = 0; i != array->size(); i++ )
   {
      AdjustParentCords( array->at( i ), hWndParent );
   }
}

void WIN32_VanishOverlay( HWND hWndParent, BOOL isShow )
{
   if( ! arrOverlays.count( hWndParent ) )
      return;
   HWND_ARRAY * array = arrOverlays[ hWndParent ];
   for( std::vector<int>::size_type i = 0; i != array->size(); i++ )
   {
      ShowWindow( array->at( i ), isShow ? SW_SHOW : SW_HIDE );
   }
}

HWND WIN32_CreateOverlay( HINSTANCE hInstance, HWND hwndParent, BYTE cRed, BYTE cGreen, BYTE cBlue, BYTE cAlpha )
{
   WNDCLASS wc;
   RECT     lpRect;
   int      X, Y, Width, Height;
   HWND     hWnd;

   char class_name[ 255 ];

   sprintf( class_name, "OverlayWindow" );

   memset( &wc, 0, sizeof( WNDCLASS ) );
   wc.lpszClassName = class_name;
   wc.hInstance     = hInstance;
   wc.hbrBackground = GetSysColorBrush( COLOR_3DFACE );
   wc.hCursor       = LoadCursor( 0, IDC_ARROW );
   wc.lpfnWndProc   = WndProc;

   lpRect = GetWorkingArea( hwndParent );

   X      = lpRect.left;
   Y      = lpRect.top;
   Width  = lpRect.right - X;
   Height = lpRect.bottom - Y;

   RegisterClass( &wc );
   hWnd = CreateWindowEx( WS_EX_TOOLWINDOW, wc.lpszClassName, TEXT( "" ),
                          WS_VISIBLE, X, Y, Width, Height, NULL, NULL, hInstance, NULL );

   // WinXP HACK - Прячем окно, прежде чем сделать его прозрачным
   ShowWindow( hWnd, SW_HIDE );

   SetWindowLong( hWnd, GWL_STYLE, 0 );
   SetBackgroundColor( hWnd, cRed, cGreen, cBlue );
   // Окно будет всегда ренедерится поверх другого
   SetWindowLong( hWnd, GWL_HWNDPARENT, ( long ) hwndParent );
   MakeWindowTransparent( hWnd, cAlpha );
   // Отключаем окно оверлея, чтобы на него нельзя было установить фокус
   EnableWindow( hWnd, FALSE );

   // WinXP HACK - Показываем окно
   ShowWindow( hWnd, SW_SHOWNORMAL );
   InvalidateRect( hWnd, NULL, TRUE );

   return hWnd;
}

static void AdjustParentCords( HWND hWnd, HWND hwndParent )
{
   int  X, Y, Width, Height;
   RECT lpRect;

   lpRect = GetWorkingArea( hwndParent );
   X      = lpRect.left;
   Y      = lpRect.top;
   Width  = lpRect.right - X;
   Height = lpRect.bottom - Y;
   SetWindowPos( hWnd, hwndParent, X, Y, Width, Height, SWP_NOZORDER );
}

static RECT GetWorkingArea( HWND hwnd )
{
   RECT rc;

   GetClientRect( hwnd, &rc );
   MapWindowPoints( hwnd, NULL, ( POINT * ) &rc, 2 );
   return rc;
}

static void SetBackgroundColor( HWND hWnd, BYTE red, BYTE green, BYTE blue )
{
   HBRUSH brush = CreateSolidBrush( RGB( red, green, blue ) );

   overlaysBrushes[ hWnd ] = brush;
}

static void MakeWindowTransparent( HWND hWnd, BYTE level )
{
   SetWindowLong( hWnd, GWL_EXSTYLE,
                  GetWindowLong( hWnd, GWL_EXSTYLE ) | WS_EX_LAYERED );
   SetLayeredWindowAttributes( hWnd, 0, level, LWA_ALPHA );
}

static LRESULT CALLBACK WndProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   switch( message )
   {
      case WM_PAINT:
      {
         RECT        rect;
         PAINTSTRUCT ps;
         HDC         hdc;
         GetClientRect( hWnd, &rect );
         hdc = BeginPaint( hWnd, &ps );
         FillRect( hdc, &rect, overlaysBrushes[ hWnd ] );
         EndPaint( hWnd, &ps );
      }
      break;
      case WM_CLOSE:
         PostQuitMessage( 0 );
         return 0;
      default:
         break;
   }
   return DefWindowProc( hWnd, message, wParam, lParam );
}
