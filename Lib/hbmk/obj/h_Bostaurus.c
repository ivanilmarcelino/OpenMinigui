/*
 * Harbour 3.2.0dev (r2510040809)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_Bostaurus.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC_STATIC( BT_WINHANDLE );
HB_FUNC_EXTERN( VALTYPE );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC_STATIC( BT_FILLRECTISNIL );
HB_FUNC_STATIC( BT_ADJUSTWIDTHHEIGHTRECT );
HB_FUNC_STATIC( BT_LISTCALLEDFUNCTIONS );
HB_FUNC_EXTERN( PROCNAME );
HB_FUNC_EXTERN( LTRIM );
HB_FUNC_EXTERN( STR );
HB_FUNC_EXTERN( PROCLINE );
HB_FUNC( BT_INFONAME );
HB_FUNC_EXTERN( ALLTRIM );
HB_FUNC( BT_INFOVERSION );
HB_FUNC( BT_INFOAUTHOR );
HB_FUNC( BT_CREATEDC );
HB_FUNC_EXTERN( BT_DC_CREATE );
HB_FUNC( BT_DELETEDC );
HB_FUNC_EXTERN( MSGBOX );
HB_FUNC_EXTERN( RELEASEALLWINDOWS );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( BT_DC_DELETE );
HB_FUNC( BT_DRAWGETPIXEL );
HB_FUNC_EXTERN( BT_DRAW_HDC_PIXEL );
HB_FUNC( BT_DRAWSETPIXEL );
HB_FUNC( BT_DRAWBITMAP );
HB_FUNC_EXTERN( BT_BMP_GETINFO );
HB_FUNC_EXTERN( BT_DRAW_HDC_BITMAP );
HB_FUNC( BT_DRAWBITMAPTRANSPARENT );
HB_FUNC( BT_DRAWBITMAPALPHABLEND );
HB_FUNC_EXTERN( BT_DRAW_HDC_BITMAPALPHABLEND );
HB_FUNC( BT_DRAWDCTODC );
HB_FUNC_EXTERN( BT_DRAW_HDC_TO_HDC );
HB_FUNC( BT_DRAWDCTODCTRANSPARENT );
HB_FUNC( BT_DRAWDCTODCALPHABLEND );
HB_FUNC_EXTERN( BT_DRAW_HDC_TO_HDC_ALPHABLEND );
HB_FUNC( BT_DRAWGRADIENTFILLHORIZONTAL );
HB_FUNC_EXTERN( BT_DRAW_HDC_GRADIENTFILL );
HB_FUNC( BT_DRAWGRADIENTFILLVERTICAL );
HB_FUNC( BT_DRAWTEXT );
HB_FUNC_EXTERN( BT_DRAW_HDC_TEXTOUT );
HB_FUNC( BT_DRAWTEXTEX );
HB_FUNC_EXTERN( BT_DRAW_HDC_DRAWTEXT );
HB_FUNC( BT_DRAWTEXTSIZE );
HB_FUNC_EXTERN( BT_DRAW_HDC_TEXTSIZE );
HB_FUNC( BT_DRAWPOLYLINE );
HB_FUNC_EXTERN( BT_DRAW_HDC_POLY );
HB_FUNC( BT_DRAWPOLYGON );
HB_FUNC( BT_DRAWPOLYBEZIER );
HB_FUNC( BT_DRAWARC );
HB_FUNC_EXTERN( BT_DRAW_HDC_ARCX );
HB_FUNC( BT_DRAWCHORD );
HB_FUNC( BT_DRAWPIE );
HB_FUNC( BT_DRAWLINE );
HB_FUNC( BT_DRAWRECTANGLE );
HB_FUNC( BT_DRAWELLIPSE );
HB_FUNC( BT_DRAWFILLRECTANGLE );
HB_FUNC_EXTERN( BT_DRAW_HDC_FILLEDOBJECT );
HB_FUNC( BT_DRAWFILLELLIPSE );
HB_FUNC( BT_DRAWFILLROUNDRECT );
HB_FUNC( BT_DRAWFILLFLOOD );
HB_FUNC( BT_GETDESKTOPHANDLE );
HB_FUNC_EXTERN( BT_SCR_GETDESKTOPHANDLE );
HB_FUNC( BT_DESKTOPWIDTH );
HB_FUNC_EXTERN( BT_SCR_GETINFO );
HB_FUNC( BT_DESKTOPHEIGHT );
HB_FUNC( BT_WINDOWWIDTH );
HB_FUNC( BT_WINDOWHEIGHT );
HB_FUNC( BT_CLIENTAREAWIDTH );
HB_FUNC( BT_CLIENTAREAHEIGHT );
HB_FUNC( BT_STATUSBARHANDLE );
HB_FUNC_EXTERN( GETCONTROLHANDLE );
HB_FUNC( BT_STATUSBARWIDTH );
HB_FUNC( BT_STATUSBARHEIGHT );
HB_FUNC( BT_TOOLBARBOTTOMHANDLE );
HB_FUNC_EXTERN( AND );
HB_FUNC_EXTERN( GETWINDOWLONG );
HB_FUNC( BT_TOOLBARBOTTOMHEIGHT );
HB_FUNC( BT_TOOLBARBOTTOMWIDTH );
HB_FUNC( BT_TOOLBARTOPHANDLE );
HB_FUNC( BT_TOOLBARTOPHEIGHT );
HB_FUNC( BT_TOOLBARTOPWIDTH );
HB_FUNC( BT_CLIENTAREAINVALIDATEALL );
HB_FUNC_EXTERN( BT_SCR_INVALIDATERECT );
HB_FUNC( BT_CLIENTAREAINVALIDATERECT );
HB_FUNC( BT_BITMAPLOADFILE );
HB_FUNC_EXTERN( BT_BMP_LOADFILE );
HB_FUNC( BT_BITMAPSAVEFILE );
HB_FUNC_EXTERN( BT_BMP_SAVEFILE );
HB_FUNC( BT_BITMAPCREATENEW );
HB_FUNC_EXTERN( BT_BMP_CREATE );
HB_FUNC( BT_BITMAPRELEASE );
HB_FUNC_EXTERN( BT_BMP_RELEASE );
HB_FUNC( BT_BITMAPWIDTH );
HB_FUNC( BT_BITMAPHEIGHT );
HB_FUNC( BT_BITMAPBITSPERPIXEL );
HB_FUNC( BT_BITMAPINVERT );
HB_FUNC_EXTERN( BT_BMP_PROCESS );
HB_FUNC( BT_BITMAPGRAYNESS );
HB_FUNC( BT_BITMAPBRIGHTNESS );
HB_FUNC( BT_BITMAPCONTRAST );
HB_FUNC( BT_BITMAPMODIFYCOLOR );
HB_FUNC( BT_BITMAPGAMMACORRECT );
HB_FUNC( BT_BITMAPCONVOLUTIONFILTER3X3 );
HB_FUNC_EXTERN( BT_BMP_FILTER3X3 );
HB_FUNC( BT_BITMAPTRANSFORM );
HB_FUNC_EXTERN( BT_BMP_TRANSFORM );
HB_FUNC( BT_BITMAPCLONE );
HB_FUNC_EXTERN( BT_BMP_CLONE );
HB_FUNC( BT_BITMAPCOPYANDRESIZE );
HB_FUNC_EXTERN( BT_BMP_COPYANDRESIZE );
HB_FUNC( BT_BITMAPPASTE );
HB_FUNC_EXTERN( BT_BMP_PASTE );
HB_FUNC( BT_BITMAPPASTETRANSPARENT );
HB_FUNC( BT_BITMAPPASTEALPHABLEND );
HB_FUNC_EXTERN( BT_BMP_PASTE_ALPHABLEND );
HB_FUNC( BT_BITMAPCAPTUREDESKTOP );
HB_FUNC_EXTERN( BT_BMP_CAPTURESCR );
HB_FUNC( BT_BITMAPCAPTUREWINDOW );
HB_FUNC( BT_BITMAPCAPTURECLIENTAREA );
HB_FUNC( BT_BITMAPCLIPBOARDGET );
HB_FUNC_EXTERN( BT_BMP_GET_CLIPBOARD );
HB_FUNC( BT_BITMAPCLIPBOARDPUT );
HB_FUNC_EXTERN( BT_BMP_PUT_CLIPBOARD );
HB_FUNC( BT_BITMAPCLIPBOARDCLEAN );
HB_FUNC_EXTERN( BT_BMP_CLEAN_CLIPBOARD );
HB_FUNC( BT_BITMAPCLIPBOARDISEMPTY );
HB_FUNC_EXTERN( BT_BMP_CLIPBOARD_ISEMPTY );
HB_FUNC( BT_HMGGETIMAGE );
HB_FUNC_EXTERN( GETPROPERTY );
HB_FUNC( BT_HMGCLONEIMAGE );
HB_FUNC( BT_HMGSETIMAGE );
HB_FUNC_EXTERN( SETPROPERTY );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_BOSTAURUS )
{ "BT_WINHANDLE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_WINHANDLE )}, NULL },
{ "VALTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( VALTYPE )}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "BT_FILLRECTISNIL", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_FILLRECTISNIL )}, NULL },
{ "BT_ADJUSTWIDTHHEIGHTRECT", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_ADJUSTWIDTHHEIGHTRECT )}, NULL },
{ "BT_LISTCALLEDFUNCTIONS", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_LISTCALLEDFUNCTIONS )}, NULL },
{ "PROCNAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( PROCNAME )}, NULL },
{ "LTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( LTRIM )}, NULL },
{ "STR", {HB_FS_PUBLIC}, {HB_FUNCNAME( STR )}, NULL },
{ "PROCLINE", {HB_FS_PUBLIC}, {HB_FUNCNAME( PROCLINE )}, NULL },
{ "BT_INFONAME", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_INFONAME )}, NULL },
{ "ALLTRIM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ALLTRIM )}, NULL },
{ "BT_INFOVERSION", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_INFOVERSION )}, NULL },
{ "BT_INFOAUTHOR", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_INFOAUTHOR )}, NULL },
{ "BT_CREATEDC", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_CREATEDC )}, NULL },
{ "BT_DC_CREATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_DC_CREATE )}, NULL },
{ "BT_DELETEDC", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_DELETEDC )}, NULL },
{ "MSGBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGBOX )}, NULL },
{ "RELEASEALLWINDOWS", {HB_FS_PUBLIC}, {HB_FUNCNAME( RELEASEALLWINDOWS )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "BT_DC_DELETE", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_DC_DELETE )}, NULL },
{ "BT_DRAWGETPIXEL", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_DRAWGETPIXEL )}, NULL },
{ "BT_DRAW_HDC_PIXEL", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_DRAW_HDC_PIXEL )}, NULL },
{ "BT_DRAWSETPIXEL", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_DRAWSETPIXEL )}, NULL },
{ "BT_DRAWBITMAP", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_DRAWBITMAP )}, NULL },
{ "BT_BMP_GETINFO", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_BMP_GETINFO )}, NULL },
{ "BT_DRAW_HDC_BITMAP", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_DRAW_HDC_BITMAP )}, NULL },
{ "BT_DRAWBITMAPTRANSPARENT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_DRAWBITMAPTRANSPARENT )}, NULL },
{ "BT_DRAWBITMAPALPHABLEND", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_DRAWBITMAPALPHABLEND )}, NULL },
{ "BT_DRAW_HDC_BITMAPALPHABLEND", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_DRAW_HDC_BITMAPALPHABLEND )}, NULL },
{ "BT_DRAWDCTODC", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_DRAWDCTODC )}, NULL },
{ "BT_DRAW_HDC_TO_HDC", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_DRAW_HDC_TO_HDC )}, NULL },
{ "BT_DRAWDCTODCTRANSPARENT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_DRAWDCTODCTRANSPARENT )}, NULL },
{ "BT_DRAWDCTODCALPHABLEND", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_DRAWDCTODCALPHABLEND )}, NULL },
{ "BT_DRAW_HDC_TO_HDC_ALPHABLEND", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_DRAW_HDC_TO_HDC_ALPHABLEND )}, NULL },
{ "BT_DRAWGRADIENTFILLHORIZONTAL", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_DRAWGRADIENTFILLHORIZONTAL )}, NULL },
{ "BT_DRAW_HDC_GRADIENTFILL", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_DRAW_HDC_GRADIENTFILL )}, NULL },
{ "BT_DRAWGRADIENTFILLVERTICAL", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_DRAWGRADIENTFILLVERTICAL )}, NULL },
{ "BT_DRAWTEXT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_DRAWTEXT )}, NULL },
{ "BT_DRAW_HDC_TEXTOUT", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_DRAW_HDC_TEXTOUT )}, NULL },
{ "BT_DRAWTEXTEX", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_DRAWTEXTEX )}, NULL },
{ "BT_DRAW_HDC_DRAWTEXT", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_DRAW_HDC_DRAWTEXT )}, NULL },
{ "BT_DRAWTEXTSIZE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_DRAWTEXTSIZE )}, NULL },
{ "BT_DRAW_HDC_TEXTSIZE", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_DRAW_HDC_TEXTSIZE )}, NULL },
{ "BT_DRAWPOLYLINE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_DRAWPOLYLINE )}, NULL },
{ "BT_DRAW_HDC_POLY", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_DRAW_HDC_POLY )}, NULL },
{ "BT_DRAWPOLYGON", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_DRAWPOLYGON )}, NULL },
{ "BT_DRAWPOLYBEZIER", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_DRAWPOLYBEZIER )}, NULL },
{ "BT_DRAWARC", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_DRAWARC )}, NULL },
{ "BT_DRAW_HDC_ARCX", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_DRAW_HDC_ARCX )}, NULL },
{ "BT_DRAWCHORD", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_DRAWCHORD )}, NULL },
{ "BT_DRAWPIE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_DRAWPIE )}, NULL },
{ "BT_DRAWLINE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_DRAWLINE )}, NULL },
{ "BT_DRAWRECTANGLE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_DRAWRECTANGLE )}, NULL },
{ "BT_DRAWELLIPSE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_DRAWELLIPSE )}, NULL },
{ "BT_DRAWFILLRECTANGLE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_DRAWFILLRECTANGLE )}, NULL },
{ "BT_DRAW_HDC_FILLEDOBJECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_DRAW_HDC_FILLEDOBJECT )}, NULL },
{ "BT_DRAWFILLELLIPSE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_DRAWFILLELLIPSE )}, NULL },
{ "BT_DRAWFILLROUNDRECT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_DRAWFILLROUNDRECT )}, NULL },
{ "BT_DRAWFILLFLOOD", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_DRAWFILLFLOOD )}, NULL },
{ "BT_GETDESKTOPHANDLE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_GETDESKTOPHANDLE )}, NULL },
{ "BT_SCR_GETDESKTOPHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_SCR_GETDESKTOPHANDLE )}, NULL },
{ "BT_DESKTOPWIDTH", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_DESKTOPWIDTH )}, NULL },
{ "BT_SCR_GETINFO", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_SCR_GETINFO )}, NULL },
{ "BT_DESKTOPHEIGHT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_DESKTOPHEIGHT )}, NULL },
{ "BT_WINDOWWIDTH", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_WINDOWWIDTH )}, NULL },
{ "BT_WINDOWHEIGHT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_WINDOWHEIGHT )}, NULL },
{ "BT_CLIENTAREAWIDTH", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_CLIENTAREAWIDTH )}, NULL },
{ "BT_CLIENTAREAHEIGHT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_CLIENTAREAHEIGHT )}, NULL },
{ "BT_STATUSBARHANDLE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_STATUSBARHANDLE )}, NULL },
{ "GETCONTROLHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLHANDLE )}, NULL },
{ "BT_STATUSBARWIDTH", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_STATUSBARWIDTH )}, NULL },
{ "BT_STATUSBARHEIGHT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_STATUSBARHEIGHT )}, NULL },
{ "BT_TOOLBARBOTTOMHANDLE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_TOOLBARBOTTOMHANDLE )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "AND", {HB_FS_PUBLIC}, {HB_FUNCNAME( AND )}, NULL },
{ "GETWINDOWLONG", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETWINDOWLONG )}, NULL },
{ "BT_TOOLBARBOTTOMHEIGHT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_TOOLBARBOTTOMHEIGHT )}, NULL },
{ "BT_TOOLBARBOTTOMWIDTH", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_TOOLBARBOTTOMWIDTH )}, NULL },
{ "BT_TOOLBARTOPHANDLE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_TOOLBARTOPHANDLE )}, NULL },
{ "BT_TOOLBARTOPHEIGHT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_TOOLBARTOPHEIGHT )}, NULL },
{ "BT_TOOLBARTOPWIDTH", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_TOOLBARTOPWIDTH )}, NULL },
{ "BT_CLIENTAREAINVALIDATEALL", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_CLIENTAREAINVALIDATEALL )}, NULL },
{ "BT_SCR_INVALIDATERECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_SCR_INVALIDATERECT )}, NULL },
{ "BT_CLIENTAREAINVALIDATERECT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_CLIENTAREAINVALIDATERECT )}, NULL },
{ "BT_BITMAPLOADFILE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_BITMAPLOADFILE )}, NULL },
{ "BT_BMP_LOADFILE", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_BMP_LOADFILE )}, NULL },
{ "BT_BITMAPSAVEFILE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_BITMAPSAVEFILE )}, NULL },
{ "BT_BMP_SAVEFILE", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_BMP_SAVEFILE )}, NULL },
{ "BT_BITMAPCREATENEW", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_BITMAPCREATENEW )}, NULL },
{ "BT_BMP_CREATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_BMP_CREATE )}, NULL },
{ "BT_BITMAPRELEASE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_BITMAPRELEASE )}, NULL },
{ "BT_BMP_RELEASE", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_BMP_RELEASE )}, NULL },
{ "BT_BITMAPWIDTH", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_BITMAPWIDTH )}, NULL },
{ "BT_BITMAPHEIGHT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_BITMAPHEIGHT )}, NULL },
{ "BT_BITMAPBITSPERPIXEL", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_BITMAPBITSPERPIXEL )}, NULL },
{ "BT_BITMAPINVERT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_BITMAPINVERT )}, NULL },
{ "BT_BMP_PROCESS", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_BMP_PROCESS )}, NULL },
{ "BT_BITMAPGRAYNESS", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_BITMAPGRAYNESS )}, NULL },
{ "BT_BITMAPBRIGHTNESS", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_BITMAPBRIGHTNESS )}, NULL },
{ "BT_BITMAPCONTRAST", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_BITMAPCONTRAST )}, NULL },
{ "BT_BITMAPMODIFYCOLOR", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_BITMAPMODIFYCOLOR )}, NULL },
{ "BT_BITMAPGAMMACORRECT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_BITMAPGAMMACORRECT )}, NULL },
{ "BT_BITMAPCONVOLUTIONFILTER3X3", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_BITMAPCONVOLUTIONFILTER3X3 )}, NULL },
{ "BT_BMP_FILTER3X3", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_BMP_FILTER3X3 )}, NULL },
{ "BT_BITMAPTRANSFORM", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_BITMAPTRANSFORM )}, NULL },
{ "BT_BMP_TRANSFORM", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_BMP_TRANSFORM )}, NULL },
{ "BT_BITMAPCLONE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_BITMAPCLONE )}, NULL },
{ "BT_BMP_CLONE", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_BMP_CLONE )}, NULL },
{ "BT_BITMAPCOPYANDRESIZE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_BITMAPCOPYANDRESIZE )}, NULL },
{ "BT_BMP_COPYANDRESIZE", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_BMP_COPYANDRESIZE )}, NULL },
{ "BT_BITMAPPASTE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_BITMAPPASTE )}, NULL },
{ "BT_BMP_PASTE", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_BMP_PASTE )}, NULL },
{ "BT_BITMAPPASTETRANSPARENT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_BITMAPPASTETRANSPARENT )}, NULL },
{ "BT_BITMAPPASTEALPHABLEND", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_BITMAPPASTEALPHABLEND )}, NULL },
{ "BT_BMP_PASTE_ALPHABLEND", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_BMP_PASTE_ALPHABLEND )}, NULL },
{ "BT_BITMAPCAPTUREDESKTOP", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_BITMAPCAPTUREDESKTOP )}, NULL },
{ "BT_BMP_CAPTURESCR", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_BMP_CAPTURESCR )}, NULL },
{ "BT_BITMAPCAPTUREWINDOW", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_BITMAPCAPTUREWINDOW )}, NULL },
{ "BT_BITMAPCAPTURECLIENTAREA", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_BITMAPCAPTURECLIENTAREA )}, NULL },
{ "BT_BITMAPCLIPBOARDGET", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_BITMAPCLIPBOARDGET )}, NULL },
{ "BT_BMP_GET_CLIPBOARD", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_BMP_GET_CLIPBOARD )}, NULL },
{ "BT_BITMAPCLIPBOARDPUT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_BITMAPCLIPBOARDPUT )}, NULL },
{ "BT_BMP_PUT_CLIPBOARD", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_BMP_PUT_CLIPBOARD )}, NULL },
{ "BT_BITMAPCLIPBOARDCLEAN", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_BITMAPCLIPBOARDCLEAN )}, NULL },
{ "BT_BMP_CLEAN_CLIPBOARD", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_BMP_CLEAN_CLIPBOARD )}, NULL },
{ "BT_BITMAPCLIPBOARDISEMPTY", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_BITMAPCLIPBOARDISEMPTY )}, NULL },
{ "BT_BMP_CLIPBOARD_ISEMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( BT_BMP_CLIPBOARD_ISEMPTY )}, NULL },
{ "BT_HMGGETIMAGE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_HMGGETIMAGE )}, NULL },
{ "GETPROPERTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETPROPERTY )}, NULL },
{ "BT_HMGCLONEIMAGE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_HMGCLONEIMAGE )}, NULL },
{ "BT_HMGSETIMAGE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( BT_HMGSETIMAGE )}, NULL },
{ "SETPROPERTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETPROPERTY )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_BOSTAURUS, "h_Bostaurus.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_BOSTAURUS
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_BOSTAURUS )
   #include "hbiniseg.h"
#endif

HB_FUNC_STATIC( BT_WINHANDLE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 60 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "N", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushLocal( 1 );
	goto lab00002;
lab00001: ;
	hb_xvmPushFuncSymbol( symbols + 2 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
lab00002: ;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 62 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( BT_FILLRECTISNIL )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 8 );
	hb_xvmSetLine( 67 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushLocal( 5 );
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 1 );
lab00002: ;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 68 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushLocal( 6 );
	goto lab00004;
lab00003: ;
	hb_xvmPushLocal( 2 );
lab00004: ;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 69 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmPushLocal( 7 );
	goto lab00006;
lab00005: ;
	hb_xvmPushLocal( 3 );
lab00006: ;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 70 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmPushLocal( 8 );
	goto lab00008;
lab00007: ;
	hb_xvmPushLocal( 4 );
lab00008: ;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 72 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( BT_ADJUSTWIDTHHEIGHTRECT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 6 );
	hb_xvmSetLine( 77 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmMinus() ) break;
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 3 );
lab00002: ;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 78 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmMinus() ) break;
	goto lab00004;
lab00003: ;
	hb_xvmPushLocal( 4 );
lab00004: ;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 80 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( BT_LISTCALLEDFUNCTIONS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 85 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 87 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "N", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 88 );
	hb_xvmLocalSetInt( 1, 1L );
lab00001: ;
	hb_xvmSetLine( 90 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmSetLine( 91 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushStringConst( "Called from:", 12 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "(", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ")", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x0D", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x0A", 1 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 92 );
	if( hb_xvmLocalInc( 1 ) ) break;
	goto lab00001;
lab00002: ;
	hb_xvmSetLine( 95 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_INFONAME )
{
   do {
	hb_xvmSetLine( 104 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushStringConst( "Bos Taurus", 10 );
	if( hb_xvmDo( 1 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_INFOVERSION )
{
   do {
	hb_xvmSetLine( 108 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( ".", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ".", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushInteger( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_INFOAUTHOR )
{
   do {
	hb_xvmSetLine( 112 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushStringConst( "(c) Dr. Claudio Soto (from Uruguay)", 35 );
	if( hb_xvmDo( 1 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_CREATEDC )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 3 );
	hb_xvmSetLine( 125 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 126 );
	hb_xvmLocalSetInt( 4, 0L );
	goto lab00003;
lab00001: ;
	hb_xvmSetLine( 127 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmEqualIntIs( 5L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 128 );
	hb_xvmCopyLocals( 1, 4 );
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 130 );
	hb_xvmPushFuncSymbol( symbols + 0 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
lab00003: ;
	hb_xvmSetLine( 132 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 133 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 135 );
	hb_xvmPushLocal( 5 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_DELETEDC )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 142 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "A", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 143 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushStringConst( "Error in call to ", 17 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ": The second parameter is not an array", 38 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x0D", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x0A", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "BT Fatal Error", 14 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 144 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	if( hb_xvmDo( 0 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 145 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNotEqualIntIs( 50L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 146 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	hb_xvmPushStringConst( "Error in call to ", 17 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ": The second parameter is an corrupted array ", 45 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x0D", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "\x0A", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "BT Fatal Error", 14 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 147 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	if( hb_xvmDo( 0 ) ) break;
lab00002: ;
	hb_xvmSetLine( 149 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 151 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_DRAWGETPIXEL )
{
   do {
	hb_xvmFrame( 1, 3 );
	hb_xvmSetLine( 165 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 5 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 167 );
	hb_xvmPushLocal( 4 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_DRAWSETPIXEL )
{
   do {
	hb_xvmFrame( 1, 4 );
	hb_xvmSetLine( 174 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmFunction( 5 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 176 );
	hb_xvmPushLocal( 5 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_DRAWBITMAP )
{
   do {
	hb_xvmFrame( 2, 7 );
	hb_xvmSetLine( 182 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 183 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 185 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmDo( 8 ) ) break;
	hb_xvmSetLine( 186 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 13 ) ) break;
	hb_xvmSetLine( 188 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_DRAWBITMAPTRANSPARENT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 8 );
	hb_xvmSetLine( 193 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushInteger( 3 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 4 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
lab00002: ;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 194 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 195 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 197 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmDo( 8 ) ) break;
	hb_xvmSetLine( 198 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmDo( 13 ) ) break;
	hb_xvmSetLine( 200 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_DRAWBITMAPALPHABLEND )
{
   do {
	hb_xvmFrame( 2, 8 );
	hb_xvmSetLine( 205 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 206 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 208 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmDo( 8 ) ) break;
	hb_xvmSetLine( 209 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 12 ) ) break;
	hb_xvmSetLine( 211 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_DRAWDCTODC )
{
   do {
	hb_xvmFrame( 0, 11 );
	hb_xvmSetLine( 217 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 13 ) ) break;
	hb_xvmSetLine( 219 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_DRAWDCTODCTRANSPARENT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 12 );
	hb_xvmSetLine( 224 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 12 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 12 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 12 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
lab00002: ;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 226 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmDo( 13 ) ) break;
	hb_xvmSetLine( 228 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_DRAWDCTODCALPHABLEND )
{
   do {
	hb_xvmFrame( 0, 12 );
	hb_xvmSetLine( 233 );
	hb_xvmPushFuncSymbol( symbols + 34 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 12 ) ) break;
	hb_xvmSetLine( 235 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_DRAWGRADIENTFILLHORIZONTAL )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 7 );
	hb_xvmSetLine( 241 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 3 );
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 6 );
lab00002: ;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 242 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 255 );
	hb_xvmArrayGen( 3 );
	goto lab00004;
lab00003: ;
	hb_xvmPushLocal( 7 );
lab00004: ;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 243 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 8 ) ) break;
	hb_xvmSetLine( 245 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_DRAWGRADIENTFILLVERTICAL )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 7 );
	hb_xvmSetLine( 250 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 255 );
	hb_xvmArrayGen( 3 );
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 6 );
lab00002: ;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 251 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 3 );
	goto lab00004;
lab00003: ;
	hb_xvmPushLocal( 7 );
lab00004: ;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 252 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 8 ) ) break;
	hb_xvmSetLine( 254 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_DRAWTEXT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 11 );
	hb_xvmSetLine( 260 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 3 );
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 7 );
lab00002: ;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 261 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 255 );
	hb_xvmArrayGen( 3 );
	goto lab00004;
lab00003: ;
	hb_xvmPushLocal( 8 );
lab00004: ;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 262 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmPushInteger( 0 );
	goto lab00006;
lab00005: ;
	hb_xvmPushLocal( 9 );
lab00006: ;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 263 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmPushInteger( 0 );
	goto lab00008;
lab00007: ;
	hb_xvmPushLocal( 10 );
lab00008: ;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 264 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmPushInteger( 0 );
	goto lab00010;
lab00009: ;
	hb_xvmPushLocal( 11 );
lab00010: ;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 265 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmDo( 11 ) ) break;
	hb_xvmSetLine( 267 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_DRAWTEXTEX )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 13 );
	hb_xvmSetLine( 272 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 3 );
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 9 );
lab00002: ;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 273 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 255 );
	hb_xvmPushInteger( 255 );
	hb_xvmArrayGen( 3 );
	goto lab00004;
lab00003: ;
	hb_xvmPushLocal( 10 );
lab00004: ;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 274 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmPushInteger( 0 );
	goto lab00006;
lab00005: ;
	hb_xvmPushLocal( 11 );
lab00006: ;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 275 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmPushInteger( 272 );
	goto lab00008;
lab00007: ;
	hb_xvmPushLocal( 12 );
lab00008: ;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 276 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmPushInteger( 0 );
	goto lab00010;
lab00009: ;
	hb_xvmPushLocal( 13 );
lab00010: ;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 277 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmDo( 13 ) ) break;
	hb_xvmSetLine( 279 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_DRAWTEXTSIZE )
{
   do {
	hb_xvmFrame( 1, 5 );
	hb_xvmSetLine( 284 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 5 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 286 );
	hb_xvmPushLocal( 6 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_DRAWPOLYLINE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 5 );
	hb_xvmSetLine( 292 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 293 );
	hb_xvmLocalSetInt( 5, 1L );
lab00001: ;
	hb_xvmSetLine( 295 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 7 ) ) break;
	hb_xvmSetLine( 297 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_DRAWPOLYGON )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 6 );
	hb_xvmSetLine( 302 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 303 );
	hb_xvmLocalSetInt( 5, 1L );
lab00001: ;
	hb_xvmSetLine( 305 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 7 ) ) break;
	hb_xvmSetLine( 307 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_DRAWPOLYBEZIER )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 5 );
	hb_xvmSetLine( 312 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 313 );
	hb_xvmLocalSetInt( 5, 1L );
lab00001: ;
	hb_xvmSetLine( 315 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmDo( 7 ) ) break;
	hb_xvmSetLine( 317 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_DRAWARC )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 11 );
	hb_xvmSetLine( 322 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 323 );
	hb_xvmLocalSetInt( 11, 1L );
lab00001: ;
	hb_xvmSetLine( 325 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 11 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 13 ) ) break;
	hb_xvmSetLine( 327 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_DRAWCHORD )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 12 );
	hb_xvmSetLine( 332 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 333 );
	hb_xvmLocalSetInt( 11, 1L );
lab00001: ;
	hb_xvmSetLine( 335 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 12 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 12 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 13 ) ) break;
	hb_xvmSetLine( 337 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_DRAWPIE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 12 );
	hb_xvmSetLine( 342 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 343 );
	hb_xvmLocalSetInt( 11, 1L );
lab00001: ;
	hb_xvmSetLine( 345 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 12 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 12 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushInteger( 2 );
	if( hb_xvmDo( 13 ) ) break;
	hb_xvmSetLine( 347 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_DRAWLINE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 7 );
	hb_xvmSetLine( 352 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 5 );
	hb_xvmArrayGen( 2 );
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 353 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	hb_xvmArrayGen( 2 );
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 355 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 356 );
	hb_xvmLocalSetInt( 7, 1L );
lab00001: ;
	hb_xvmSetLine( 358 );
	hb_xvmPushFuncSymbol( symbols + 44 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 5 ) ) break;
	hb_xvmSetLine( 360 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_DRAWRECTANGLE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 7 );
	hb_xvmSetLine( 365 );
	hb_xvmPushInteger( 5 );
	hb_xvmArrayDim( 1 );
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 366 );
	hb_xvmPushInteger( 5 );
	hb_xvmArrayDim( 1 );
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 368 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPop( 1L ) ) break;
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPop( 1L ) ) break;
	hb_xvmSetLine( 369 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPop( 2L ) ) break;
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPop( 2L ) ) break;
	hb_xvmSetLine( 370 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPop( 3L ) ) break;
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPop( 3L ) ) break;
	hb_xvmSetLine( 371 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPop( 4L ) ) break;
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPop( 4L ) ) break;
	hb_xvmSetLine( 372 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPop( 5L ) ) break;
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPop( 5L ) ) break;
	hb_xvmSetLine( 373 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 374 );
	hb_xvmLocalSetInt( 7, 1L );
lab00001: ;
	hb_xvmSetLine( 376 );
	hb_xvmPushFuncSymbol( symbols + 44 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 5 ) ) break;
	hb_xvmSetLine( 378 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_DRAWELLIPSE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 6, 7 );
	hb_xvmSetLine( 387 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmLocalAdd( 12 );
	hb_xvmSetLine( 388 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 5 );
	hb_xvmLocalAdd( 13 );
	hb_xvmSetLine( 389 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 9 );
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 390 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 11 );
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 391 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 392 );
	hb_xvmLocalSetInt( 7, 1L );
lab00001: ;
	hb_xvmSetLine( 394 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 11 ) ) break;
	hb_xvmSetLine( 396 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_DRAWFILLRECTANGLE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 8 );
	hb_xvmSetLine( 402 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 403 );
	hb_xvmCopyLocals( 6, 7 );
	hb_xvmSetLine( 404 );
	hb_xvmLocalSetInt( 8, 1L );
lab00001: ;
	hb_xvmSetLine( 406 );
	hb_xvmPushFuncSymbol( symbols + 56 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 8 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 11 ) ) break;
	hb_xvmSetLine( 408 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_DRAWFILLELLIPSE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 8 );
	hb_xvmSetLine( 413 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 414 );
	hb_xvmCopyLocals( 6, 7 );
	hb_xvmSetLine( 415 );
	hb_xvmLocalSetInt( 8, 1L );
lab00001: ;
	hb_xvmSetLine( 417 );
	hb_xvmPushFuncSymbol( symbols + 56 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 8 );
	hb_xvmPushInteger( 2 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 11 ) ) break;
	hb_xvmSetLine( 419 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_DRAWFILLROUNDRECT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 10 );
	hb_xvmSetLine( 424 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 425 );
	hb_xvmCopyLocals( 8, 9 );
	hb_xvmSetLine( 426 );
	hb_xvmLocalSetInt( 10, 1L );
lab00001: ;
	hb_xvmSetLine( 428 );
	hb_xvmPushFuncSymbol( symbols + 56 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 10 );
	hb_xvmPushInteger( 3 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 11 ) ) break;
	hb_xvmSetLine( 430 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_DRAWFILLFLOOD )
{
   do {
	hb_xvmFrame( 0, 4 );
	hb_xvmSetLine( 435 );
	hb_xvmPushFuncSymbol( symbols + 56 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushInteger( 4 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 11 ) ) break;
	hb_xvmSetLine( 437 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_GETDESKTOPHANDLE )
{
   do {
	hb_xvmSetLine( 442 );
	hb_xvmPushFuncSymbol( symbols + 61 );
	if( hb_xvmDo( 0 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_DESKTOPWIDTH )
{
   do {
	hb_xvmFrame( 1, 0 );
	hb_xvmSetLine( 447 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 449 );
	hb_xvmPushLocal( 1 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_DESKTOPHEIGHT )
{
   do {
	hb_xvmFrame( 1, 0 );
	hb_xvmSetLine( 454 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 456 );
	hb_xvmPushLocal( 1 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_WINDOWWIDTH )
{
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 461 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushFuncSymbol( symbols + 0 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 463 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_WINDOWHEIGHT )
{
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 468 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushFuncSymbol( symbols + 0 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 470 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_CLIENTAREAWIDTH )
{
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 475 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushFuncSymbol( symbols + 0 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 2 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 477 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_CLIENTAREAHEIGHT )
{
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 482 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushFuncSymbol( symbols + 0 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 2 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 484 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_STATUSBARHANDLE )
{
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 490 );
	hb_xvmPushFuncSymbol( symbols + 70 );
	hb_xvmPushStringConst( "StatusBar", 9 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 492 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_STATUSBARWIDTH )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 1 );
	hb_xvmSetLine( 497 );
	hb_xvmPushFuncSymbol( symbols + 69 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 498 );
	hb_xvmLocalSetInt( 3, 0L );
	hb_xvmSetLine( 500 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 501 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 3 );
lab00001: ;
	hb_xvmSetLine( 504 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_STATUSBARHEIGHT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 1 );
	hb_xvmSetLine( 509 );
	hb_xvmPushFuncSymbol( symbols + 69 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 510 );
	hb_xvmLocalSetInt( 3, 0L );
	hb_xvmSetLine( 512 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 513 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 3 );
lab00001: ;
	hb_xvmSetLine( 516 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_TOOLBARBOTTOMHANDLE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 1 );
	hb_xvmSetLine( 522 );
	hb_xvmPushFuncSymbol( symbols + 0 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 523 );
	hb_xvmLocalSetInt( 4, 0L );
	hb_xvmSetLine( 525 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 3 );
	goto lab00003;
lab00001: ;
	hb_xvmSetLine( 527 );
	if( hb_xvmPushMemvar( symbols + 74 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushStringConst( "TOOLBAR", 7 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	if( hb_xvmPushMemvar( symbols + 74 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 75 );
	hb_xvmPushFuncSymbol( symbols + 76 );
	if( hb_xvmPushMemvar( symbols + 74 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushInteger( -16 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmEqualIntIs( 3L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 528 );
	if( hb_xvmPushMemvar( symbols + 74 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 4 );
lab00002: ;
	hb_xvmSetLine( 525 );
	if( hb_xvmLocalIncPush( 3 ) ) break;
lab00003: ;
	hb_xvmPushFuncSymbol( symbols + 19 );
	if( hb_xvmPushMemvar( symbols + 74 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 532 );
	hb_xvmPushLocal( 4 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_TOOLBARBOTTOMHEIGHT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 1 );
	hb_xvmSetLine( 537 );
	hb_xvmPushFuncSymbol( symbols + 73 );
	hb_xvmPushFuncSymbol( symbols + 0 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 538 );
	hb_xvmLocalSetInt( 3, 0L );
	hb_xvmSetLine( 540 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 541 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 3 );
lab00001: ;
	hb_xvmSetLine( 544 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_TOOLBARBOTTOMWIDTH )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 1 );
	hb_xvmSetLine( 549 );
	hb_xvmPushFuncSymbol( symbols + 73 );
	hb_xvmPushFuncSymbol( symbols + 0 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 550 );
	hb_xvmLocalSetInt( 3, 0L );
	hb_xvmSetLine( 552 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 553 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 3 );
lab00001: ;
	hb_xvmSetLine( 556 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_TOOLBARTOPHANDLE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 1 );
	hb_xvmSetLine( 562 );
	hb_xvmPushFuncSymbol( symbols + 0 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 563 );
	hb_xvmLocalSetInt( 4, 0L );
	hb_xvmSetLine( 565 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 3 );
	goto lab00003;
lab00001: ;
	hb_xvmSetLine( 567 );
	if( hb_xvmPushMemvar( symbols + 74 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushStringConst( "TOOLBAR", 7 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	if( hb_xvmPushMemvar( symbols + 74 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 75 );
	hb_xvmPushFuncSymbol( symbols + 76 );
	if( hb_xvmPushMemvar( symbols + 74 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushInteger( -16 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmNotEqualIntIs( 3L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 568 );
	if( hb_xvmPushMemvar( symbols + 74 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 4 );
lab00002: ;
	hb_xvmSetLine( 565 );
	if( hb_xvmLocalIncPush( 3 ) ) break;
lab00003: ;
	hb_xvmPushFuncSymbol( symbols + 19 );
	if( hb_xvmPushMemvar( symbols + 74 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 572 );
	hb_xvmPushLocal( 4 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_TOOLBARTOPHEIGHT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 1 );
	hb_xvmSetLine( 577 );
	hb_xvmPushFuncSymbol( symbols + 79 );
	hb_xvmPushFuncSymbol( symbols + 0 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 578 );
	hb_xvmLocalSetInt( 3, 0L );
	hb_xvmSetLine( 580 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 581 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 3 );
lab00001: ;
	hb_xvmSetLine( 584 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_TOOLBARTOPWIDTH )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 1 );
	hb_xvmSetLine( 589 );
	hb_xvmPushFuncSymbol( symbols + 79 );
	hb_xvmPushFuncSymbol( symbols + 0 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 590 );
	hb_xvmLocalSetInt( 3, 0L );
	hb_xvmSetLine( 592 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 593 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 3 );
lab00001: ;
	hb_xvmSetLine( 596 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_CLIENTAREAINVALIDATEALL )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 2 );
	hb_xvmSetLine( 602 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 603 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 2 );
lab00001: ;
	hb_xvmSetLine( 605 );
	hb_xvmPushFuncSymbol( symbols + 83 );
	hb_xvmPushFuncSymbol( symbols + 0 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushNil();
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 607 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_CLIENTAREAINVALIDATERECT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 6 );
	hb_xvmSetLine( 612 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 613 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 6 );
lab00001: ;
	hb_xvmSetLine( 615 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushFuncSymbol( symbols + 67 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 68 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 8 ) ) break;
	hb_xvmSetLine( 616 );
	hb_xvmPushFuncSymbol( symbols + 83 );
	hb_xvmPushFuncSymbol( symbols + 0 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPlus() ) break;
	hb_xvmArrayGen( 4 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 618 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_BITMAPLOADFILE )
{
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 630 );
	hb_xvmPushFuncSymbol( symbols + 86 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 632 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_BITMAPSAVEFILE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 3 );
	hb_xvmSetLine( 639 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 640 );
	hb_xvmLocalSetInt( 3, 0L );
lab00001: ;
	hb_xvmSetLine( 642 );
	hb_xvmPushFuncSymbol( symbols + 88 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 644 );
	hb_xvmPushLocal( 4 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_BITMAPCREATENEW )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 3 );
	hb_xvmSetLine( 652 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 653 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmArrayGen( 3 );
	hb_xvmPopLocal( 3 );
lab00001: ;
	hb_xvmSetLine( 655 );
	hb_xvmPushFuncSymbol( symbols + 90 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 657 );
	hb_xvmPushLocal( 4 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_BITMAPRELEASE )
{
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 662 );
	hb_xvmPushFuncSymbol( symbols + 92 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 664 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_BITMAPWIDTH )
{
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 670 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 672 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_BITMAPHEIGHT )
{
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 677 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 679 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_BITMAPBITSPERPIXEL )
{
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 684 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 686 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_BITMAPINVERT )
{
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 692 );
	hb_xvmPushFuncSymbol( symbols + 97 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 694 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_BITMAPGRAYNESS )
{
   do {
	hb_xvmFrame( 0, 2 );
	hb_xvmSetLine( 698 );
	hb_xvmPushFuncSymbol( symbols + 97 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 700 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_BITMAPBRIGHTNESS )
{
   do {
	hb_xvmFrame( 0, 2 );
	hb_xvmSetLine( 704 );
	hb_xvmPushFuncSymbol( symbols + 97 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 2 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 706 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_BITMAPCONTRAST )
{
   do {
	hb_xvmFrame( 0, 2 );
	hb_xvmSetLine( 710 );
	hb_xvmPushFuncSymbol( symbols + 97 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 3 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 712 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_BITMAPMODIFYCOLOR )
{
   do {
	hb_xvmFrame( 0, 4 );
	hb_xvmSetLine( 716 );
	hb_xvmPushFuncSymbol( symbols + 97 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 4 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmArrayGen( 3 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 718 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_BITMAPGAMMACORRECT )
{
   do {
	hb_xvmFrame( 0, 4 );
	hb_xvmSetLine( 722 );
	hb_xvmPushFuncSymbol( symbols + 97 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 5 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmArrayGen( 3 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 724 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_BITMAPCONVOLUTIONFILTER3X3 )
{
   do {
	hb_xvmFrame( 0, 2 );
	hb_xvmSetLine( 728 );
	hb_xvmPushFuncSymbol( symbols + 104 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 730 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_BITMAPTRANSFORM )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 4 );
	hb_xvmSetLine( 736 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 3 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 4 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
lab00002: ;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 738 );
	hb_xvmPushFuncSymbol( symbols + 106 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 4 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 740 );
	hb_xvmPushLocal( 5 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_BITMAPCLONE )
{
   do {
	hb_xvmFrame( 3, 5 );
	hb_xvmSetLine( 747 );
	hb_xvmPushFuncSymbol( symbols + 93 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 748 );
	hb_xvmPushFuncSymbol( symbols + 94 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 750 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmDo( 8 ) ) break;
	hb_xvmSetLine( 751 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmDo( 6 ) ) break;
	hb_xvmSetLine( 752 );
	hb_xvmPushFuncSymbol( symbols + 108 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 5 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 754 );
	hb_xvmPushLocal( 6 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_BITMAPCOPYANDRESIZE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 5 );
	hb_xvmSetLine( 761 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 762 );
	hb_xvmLocalSetInt( 4, 1L );
lab00001: ;
	hb_xvmSetLine( 764 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 765 );
	hb_xvmLocalSetInt( 5, 1L );
lab00002: ;
	hb_xvmSetLine( 767 );
	hb_xvmPushFuncSymbol( symbols + 110 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 5 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 769 );
	hb_xvmPushLocal( 6 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_BITMAPPASTE )
{
   do {
	hb_xvmFrame( 4, 7 );
	hb_xvmSetLine( 774 );
	hb_xvmPushFuncSymbol( symbols + 93 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 775 );
	hb_xvmPushFuncSymbol( symbols + 94 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 776 );
	hb_xvmPushFuncSymbol( symbols + 93 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 777 );
	hb_xvmPushFuncSymbol( symbols + 94 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 779 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmDo( 8 ) ) break;
	hb_xvmSetLine( 780 );
	hb_xvmPushFuncSymbol( symbols + 112 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 13 ) ) break;
	hb_xvmSetLine( 782 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_BITMAPPASTETRANSPARENT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 5, 8 );
	hb_xvmSetLine( 787 );
	hb_xvmPushFuncSymbol( symbols + 93 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 788 );
	hb_xvmPushFuncSymbol( symbols + 94 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 789 );
	hb_xvmPushFuncSymbol( symbols + 93 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 790 );
	hb_xvmPushFuncSymbol( symbols + 94 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 792 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushInteger( 3 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 4 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
lab00002: ;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 794 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmDo( 8 ) ) break;
	hb_xvmSetLine( 795 );
	hb_xvmPushFuncSymbol( symbols + 112 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmDo( 13 ) ) break;
	hb_xvmSetLine( 797 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_BITMAPPASTEALPHABLEND )
{
   do {
	hb_xvmFrame( 4, 8 );
	hb_xvmSetLine( 802 );
	hb_xvmPushFuncSymbol( symbols + 93 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 803 );
	hb_xvmPushFuncSymbol( symbols + 94 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 804 );
	hb_xvmPushFuncSymbol( symbols + 93 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 805 );
	hb_xvmPushFuncSymbol( symbols + 94 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 807 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmDo( 8 ) ) break;
	hb_xvmSetLine( 808 );
	hb_xvmPushFuncSymbol( symbols + 115 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 12 ) ) break;
	hb_xvmSetLine( 810 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_BITMAPCAPTUREDESKTOP )
{
   do {
	hb_xvmFrame( 4, 4 );
	hb_xvmSetLine( 817 );
	hb_xvmPushFuncSymbol( symbols + 60 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 818 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 819 );
	hb_xvmPushFuncSymbol( symbols + 64 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 821 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 1 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmDo( 8 ) ) break;
	hb_xvmSetLine( 822 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmDo( 6 ) ) break;
	hb_xvmSetLine( 823 );
	hb_xvmPushFuncSymbol( symbols + 117 );
	hb_xvmPushFuncSymbol( symbols + 0 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 6 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 825 );
	hb_xvmPushLocal( 5 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_BITMAPCAPTUREWINDOW )
{
   do {
	hb_xvmFrame( 3, 5 );
	hb_xvmSetLine( 831 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 832 );
	hb_xvmPushFuncSymbol( symbols + 66 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 834 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmDo( 8 ) ) break;
	hb_xvmSetLine( 835 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmDo( 6 ) ) break;
	hb_xvmSetLine( 836 );
	hb_xvmPushFuncSymbol( symbols + 117 );
	hb_xvmPushFuncSymbol( symbols + 0 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 6 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 838 );
	hb_xvmPushLocal( 6 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_BITMAPCAPTURECLIENTAREA )
{
   do {
	hb_xvmFrame( 3, 5 );
	hb_xvmSetLine( 844 );
	hb_xvmPushFuncSymbol( symbols + 67 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 845 );
	hb_xvmPushFuncSymbol( symbols + 68 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 847 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocalByRef( 2 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmDo( 8 ) ) break;
	hb_xvmSetLine( 848 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmDo( 6 ) ) break;
	hb_xvmSetLine( 849 );
	hb_xvmPushFuncSymbol( symbols + 117 );
	hb_xvmPushFuncSymbol( symbols + 0 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 6 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 851 );
	hb_xvmPushLocal( 6 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_BITMAPCLIPBOARDGET )
{
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 859 );
	hb_xvmPushFuncSymbol( symbols + 121 );
	hb_xvmPushFuncSymbol( symbols + 0 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 861 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_BITMAPCLIPBOARDPUT )
{
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 866 );
	hb_xvmPushFuncSymbol( symbols + 123 );
	hb_xvmPushFuncSymbol( symbols + 0 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 868 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_BITMAPCLIPBOARDCLEAN )
{
   do {
	hb_xvmFrame( 1, 1 );
	hb_xvmSetLine( 873 );
	hb_xvmPushFuncSymbol( symbols + 125 );
	hb_xvmPushFuncSymbol( symbols + 0 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 875 );
	hb_xvmPushLocal( 2 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_BITMAPCLIPBOARDISEMPTY )
{
   do {
	hb_xvmFrame( 1, 0 );
	hb_xvmSetLine( 880 );
	hb_xvmPushFuncSymbol( symbols + 127 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 882 );
	hb_xvmPushLocal( 1 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_HMGGETIMAGE )
{
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 896 );
	hb_xvmPushFuncSymbol( symbols + 129 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "hBitmap", 7 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 912 );
	hb_xvmPushLocal( 3 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_HMGCLONEIMAGE )
{
   do {
	hb_xvmFrame( 1, 2 );
	hb_xvmSetLine( 917 );
	hb_xvmPushFuncSymbol( symbols + 128 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 919 );
	hb_xvmPushFuncSymbol( symbols + 107 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( BT_HMGSETIMAGE )
{
   do {
	hb_xvmFrame( 0, 4 );
	hb_xvmSetLine( 928 );
	hb_xvmPushFuncSymbol( symbols + 132 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( "hBitmap", 7 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 960 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

