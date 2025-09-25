/*
 * Harbour 3.2.0dev (r2507191744)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "miniprint.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( _HMG_PRINTER_SHOWPREVIEW );
HB_FUNC_EXTERN( _SETGETGLOBAL );
HB_FUNC_EXTERN( ISVISTAORLATER );
HB_FUNC_EXTERN( ISAPPXPTHEMED );
HB_FUNC_EXTERN( GETTEMPFOLDER );
HB_FUNC_EXTERN( HB_PS );
HB_FUNC_EXTERN( DISABLEWINDOW );
HB_FUNC_EXTERN( _ISWINDOWDEFINED );
HB_FUNC_EXTERN( GETDESKTOPHEIGHT );
HB_FUNC( _HMG_PRINTER_GETPAGEHEIGHT );
HB_FUNC_EXTERN( _DEFINEWINDOW );
HB_FUNC_EXTERN( _DEFINELABEL );
HB_FUNC_EXTERN( _ENDWINDOW );
HB_FUNC_EXTERN( DOMETHOD );
HB_FUNC_EXTERN( HB_NTOS );
HB_FUNC_EXTERN( GETDESKTOPWIDTH );
HB_FUNC_EXTERN( _DELGLOBAL );
HB_FUNC( _HMG_PRINTER_PREVIEWREFRESH );
HB_FUNC( ISWIN8ORLATER );
HB_FUNC_EXTERN( _DEFINESPLITBOX );
HB_FUNC_EXTERN( _BEGINTOOLBAR );
HB_FUNC_EXTERN( _DEFINETOOLBUTTON );
HB_FUNC( _HMG_PRINTER_GO_TO_PAGE );
HB_FUNC( _HMG_PRINTER_PROCESSTHUMBNAILS );
HB_FUNC( _HMG_PRINTER_ZOOM );
HB_FUNC( _HMG_PRINTER_PRINTPAGES );
HB_FUNC( _HMG_PRINTER_SAVE_PDF_PAGES );
HB_FUNC( _HMG_PRINTER_SEND_MAIL );
HB_FUNC( _HMG_PRINTER_PREVIEWCLOSE );
HB_FUNC_EXTERN( _ENDTOOLBAR );
HB_FUNC_EXTERN( _DEFINESPLITCHILDWINDOW );
HB_FUNC_STATIC( _HMG_PRINTER_SCROLLLEFT );
HB_FUNC_STATIC( _HMG_PRINTER_SCROLLRIGHT );
HB_FUNC_STATIC( _HMG_PRINTER_SCROLLUP );
HB_FUNC_STATIC( _HMG_PRINTER_SCROLLDOWN );
HB_FUNC_STATIC( _HMG_PRINTER_HSCROLLBOXPROCESS );
HB_FUNC_STATIC( _HMG_PRINTER_VSCROLLBOXPROCESS );
HB_FUNC_STATIC( _HMG_PRINTER_SETKEYS );
HB_FUNC_EXTERN( INSTALLEVENTHANDLER );
HB_FUNC_EXTERN( _ENDSPLITBOX );
HB_FUNC_EXTERN( GETTITLEHEIGHT );
HB_FUNC_EXTERN( _DEFINEHOTKEY );
HB_FUNC_EXTERN( HIDEWINDOW );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC_EXTERN( ENABLEWINDOW );
HB_FUNC( _HMG_PRINTER_PRINTPAGESDO );
HB_FUNC_EXTERN( _BEGINFRAME );
HB_FUNC_EXTERN( GETPROPERTY );
HB_FUNC_EXTERN( SETPROPERTY );
HB_FUNC_EXTERN( _DEFINERADIOGROUP );
HB_FUNC_EXTERN( _DEFINESPINNER );
HB_FUNC_EXTERN( _DEFINECOMBO );
HB_FUNC_EXTERN( _DEFINEOWNERBUTTON );
HB_FUNC_EXTERN( _ISCONTROLDEFINED );
HB_FUNC_EXTERN( _DEFINECHECKBOX );
HB_FUNC( _HMG_PRINTER_GETPAGEWIDTH );
HB_FUNC_EXTERN( INT );
HB_FUNC_EXTERN( GETHSCROLLBARHEIGHT );
HB_FUNC_EXTERN( GETBORDERHEIGHT );
HB_FUNC( _HMG_PRINTER_PREVIEW_DISABLESCROLLBARS );
HB_FUNC_EXTERN( SETSCROLLRANGE );
HB_FUNC_EXTERN( SETSCROLLPOS );
HB_FUNC( _HMG_PRINTER_PREVIEW_DISABLEHSCROLLBAR );
HB_FUNC_EXTERN( _ACTIVATEWINDOW );
HB_FUNC_EXTERN( LEN );
HB_FUNC_EXTERN( HB_ADEL );
HB_FUNC_EXTERN( SETFOCUS );
HB_FUNC( _HMG_PRINTER_MOUSEZOOM );
HB_FUNC_STATIC( _HMG_PRINTER_THUMBNAILTOGGLE );
HB_FUNC_STATIC( CREATETHUMBNAILS );
HB_FUNC_EXTERN( SHOWWINDOW );
HB_FUNC_EXTERN( GETFORMTOOLTIPHANDLE );
HB_FUNC( _DEFINEEMFFILE );
HB_FUNC_EXTERN( STRZERO );
HB_FUNC_EXTERN( SETTOOLTIP );
HB_FUNC_EXTERN( GETCONTROLHANDLE );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC_EXTERN( PUTFILE );
HB_FUNC_EXTERN( GETCURRENTFOLDER );
HB_FUNC_EXTERN( HMG_SYSWAIT );
HB_FUNC_EXTERN( HB_DIRSEPADD );
HB_FUNC_EXTERN( HB_VFDIRECTORY );
HB_FUNC_EXTERN( AEVAL );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( HB_FNAMEEXTSETDEF );
HB_FUNC_EXTERN( _CREATEPDF );
HB_FUNC_EXTERN( ASORT );
HB_FUNC_EXTERN( GETSTARTUPFOLDER );
HB_FUNC_EXTERN( FILE );
HB_FUNC_EXTERN( HB_CWD );
HB_FUNC_EXTERN( STRTRAN );
HB_FUNC_EXTERN( LEFT );
HB_FUNC_EXTERN( HB_TTOS );
HB_FUNC_EXTERN( HB_DATETIME );
HB_FUNC_EXTERN( MSGINFO );
HB_FUNC_EXTERN( HB_USERNAME );
HB_FUNC_EXTERN( _BEGININI );
HB_FUNC_EXTERN( _GETINI );
HB_FUNC_EXTERN( _ENDINI );
HB_FUNC_EXTERN( TCDOMAIL );
HB_FUNC_EXTERN( HB_VFERASE );
HB_FUNC_EXTERN( GETSCROLLPOS );
HB_FUNC_STATIC( _HMG_PRINTER_CLEANPREVIEW );
HB_FUNC_STATIC( _HMG_PRINTER_SPLTCHLDMOUSECURSOR );
HB_FUNC_EXTERN( VALTYPE );
HB_FUNC_EXTERN( GETCURSORPOS );
HB_FUNC_EXTERN( PTINRECT );
HB_FUNC_EXTERN( SETWINDOWCURSOR );
HB_FUNC( _HMG_PRINTER_SPLTCHLDMOUSECLICK );
HB_FUNC_EXTERN( DIRECTORY );
HB_FUNC_EXTERN( FERASE );
HB_FUNC_EXTERN( GETSCROLLRANGEMAX );
HB_FUNC_EXTERN( _HMG_SETVSCROLLVALUE );
HB_FUNC_EXTERN( PLAYBEEP );
HB_FUNC_EXTERN( INVALIDATERECT );
HB_FUNC( _HMG_PRINTER_SHOWPAGE );
HB_FUNC( _HMG_PRINTER_STARTDOC );
HB_FUNC_EXTERN( __MVEXIST );
HB_FUNC_EXTERN( __MVPUT );
HB_FUNC_EXTERN( MSGMINIGUIERROR );
HB_FUNC( _HMG_PRINTER_PRINTPAGE );
HB_FUNC( _HMG_PRINTER_ENDDOC );
HB_FUNC( GETPRINTER );
HB_FUNC( APRINTERS );
HB_FUNC( GETDEFAULTPRINTER );
HB_FUNC_EXTERN( TYPE );
HB_FUNC_EXTERN( _HMG_PRINTER_INITUSERMESSAGES );
HB_FUNC_EXTERN( _DEFINEMODALWINDOW );
HB_FUNC_EXTERN( _DEFINEBUTTON );
HB_FUNC( _HMG_PRINTER_H_PRINT );
HB_FUNC_EXTERN( DTOC );
HB_FUNC_EXTERN( UPPER );
HB_FUNC( SETTEXTALIGN );
HB_FUNC( _HMG_PRINTER_C_PRINT );
HB_FUNC( _HMG_PRINTER_H_MULTILINE_PRINT );
HB_FUNC( _HMG_PRINTER_C_MULTILINE_PRINT );
HB_FUNC( _HMG_PRINTER_H_IMAGE );
HB_FUNC( _HMG_PRINTER_C_IMAGE );
HB_FUNC( _HMG_PRINTER_H_LINE );
HB_FUNC( _HMG_PRINTER_C_LINE );
HB_FUNC( _HMG_PRINTER_H_RECTANGLE );
HB_FUNC( _HMG_PRINTER_C_RECTANGLE );
HB_FUNC( _HMG_PRINTER_H_ROUNDRECTANGLE );
HB_FUNC( _HMG_PRINTER_C_ROUNDRECTANGLE );
HB_FUNC( GETPRINTABLEAREAWIDTH );
HB_FUNC( _HMG_PRINTER_GETPRINTERWIDTH );
HB_FUNC( GETPRINTABLEAREAHEIGHT );
HB_FUNC( _HMG_PRINTER_GETPRINTERHEIGHT );
HB_FUNC( GETPRINTABLEAREAHORIZONTALOFFSET );
HB_FUNC( _HMG_PRINTER_GETPRINTABLEAREAPHYSICALOFFSETX );
HB_FUNC( _HMG_PRINTER_GETPRINTABLEAREALOGPIXELSX );
HB_FUNC( GETPRINTABLEAREAVERTICALOFFSET );
HB_FUNC( _HMG_PRINTER_GETPRINTABLEAREAPHYSICALOFFSETY );
HB_FUNC( _HMG_PRINTER_GETPRINTABLEAREALOGPIXELSY );
HB_FUNC( _HMG_PRINTER_PREVIEW_ENABLESCROLLBARS );
HB_FUNC( _HMG_PRINTER_SETJOBNAME );
HB_FUNC_EXTERN( HB_DEFAULTVALUE );
HB_FUNC( HMG_PRINTGETJOBINFO );
HB_FUNC( _HMG_PRINTGETJOBINFO );
HB_FUNC( HMG_PRINTERGETSTATUS );
HB_FUNC( _HMG_PRINTERGETSTATUS );
HB_FUNC_EXTERN( _GETCONTROLFREE );
HB_FUNC( INITEMFFILE );
HB_FUNC_EXTERN( _SETNAMELIST );
HB_FUNC( C_SETEMFFILE );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_MINIPRINT )
{ "_HMG_PRINTER_SHOWPREVIEW", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_SHOWPREVIEW )}, NULL },
{ "_SETGETGLOBAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETGETGLOBAL )}, NULL },
{ "ISVISTAORLATER", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISVISTAORLATER )}, NULL },
{ "ISAPPXPTHEMED", {HB_FS_PUBLIC}, {HB_FUNCNAME( ISAPPXPTHEMED )}, NULL },
{ "GETTEMPFOLDER", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETTEMPFOLDER )}, NULL },
{ "HB_PS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_PS )}, NULL },
{ "_HMG_MINIPRINT", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "DISABLEWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( DISABLEWINDOW )}, NULL },
{ "_ISWINDOWDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISWINDOWDEFINED )}, NULL },
{ "GETDESKTOPHEIGHT", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETDESKTOPHEIGHT )}, NULL },
{ "_HMG_PRINTER_GETPAGEHEIGHT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_GETPAGEHEIGHT )}, NULL },
{ "_DEFINEWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINEWINDOW )}, NULL },
{ "_DEFINELABEL", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINELABEL )}, NULL },
{ "_ENDWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ENDWINDOW )}, NULL },
{ "DOMETHOD", {HB_FS_PUBLIC}, {HB_FUNCNAME( DOMETHOD )}, NULL },
{ "HB_NTOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_NTOS )}, NULL },
{ "GETDESKTOPWIDTH", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETDESKTOPWIDTH )}, NULL },
{ "_DELGLOBAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DELGLOBAL )}, NULL },
{ "_HMG_PRINTER_PREVIEWREFRESH", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_PREVIEWREFRESH )}, NULL },
{ "ISWIN8ORLATER", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( ISWIN8ORLATER )}, NULL },
{ "_DEFINESPLITBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINESPLITBOX )}, NULL },
{ "_BEGINTOOLBAR", {HB_FS_PUBLIC}, {HB_FUNCNAME( _BEGINTOOLBAR )}, NULL },
{ "_DEFINETOOLBUTTON", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINETOOLBUTTON )}, NULL },
{ "_HMG_PRINTER_GO_TO_PAGE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_GO_TO_PAGE )}, NULL },
{ "_HMG_PRINTER_PROCESSTHUMBNAILS", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_PROCESSTHUMBNAILS )}, NULL },
{ "_HMG_PRINTER_ZOOM", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_ZOOM )}, NULL },
{ "_HMG_PRINTER_PRINTPAGES", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_PRINTPAGES )}, NULL },
{ "_HMG_PRINTER_SAVE_PDF_PAGES", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_SAVE_PDF_PAGES )}, NULL },
{ "_HMG_PRINTER_SEND_MAIL", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_SEND_MAIL )}, NULL },
{ "_HMG_PRINTER_PREVIEWCLOSE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_PREVIEWCLOSE )}, NULL },
{ "_ENDTOOLBAR", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ENDTOOLBAR )}, NULL },
{ "_DEFINESPLITCHILDWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINESPLITCHILDWINDOW )}, NULL },
{ "_HMG_PRINTER_SCROLLLEFT", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_SCROLLLEFT )}, NULL },
{ "_HMG_PRINTER_SCROLLRIGHT", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_SCROLLRIGHT )}, NULL },
{ "_HMG_PRINTER_SCROLLUP", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_SCROLLUP )}, NULL },
{ "_HMG_PRINTER_SCROLLDOWN", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_SCROLLDOWN )}, NULL },
{ "_HMG_PRINTER_HSCROLLBOXPROCESS", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_HSCROLLBOXPROCESS )}, NULL },
{ "_HMG_PRINTER_VSCROLLBOXPROCESS", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_VSCROLLBOXPROCESS )}, NULL },
{ "_HMG_PRINTER_SETKEYS", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_SETKEYS )}, NULL },
{ "INSTALLEVENTHANDLER", {HB_FS_PUBLIC}, {HB_FUNCNAME( INSTALLEVENTHANDLER )}, NULL },
{ "_ENDSPLITBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ENDSPLITBOX )}, NULL },
{ "GETTITLEHEIGHT", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETTITLEHEIGHT )}, NULL },
{ "_DEFINEHOTKEY", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINEHOTKEY )}, NULL },
{ "HIDEWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( HIDEWINDOW )}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "ENABLEWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( ENABLEWINDOW )}, NULL },
{ "_HMG_PRINTER_PRINTPAGESDO", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_PRINTPAGESDO )}, NULL },
{ "_BEGINFRAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( _BEGINFRAME )}, NULL },
{ "GETPROPERTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETPROPERTY )}, NULL },
{ "SETPROPERTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETPROPERTY )}, NULL },
{ "_DEFINERADIOGROUP", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINERADIOGROUP )}, NULL },
{ "_DEFINESPINNER", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINESPINNER )}, NULL },
{ "_DEFINECOMBO", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINECOMBO )}, NULL },
{ "_DEFINEOWNERBUTTON", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINEOWNERBUTTON )}, NULL },
{ "_ISCONTROLDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISCONTROLDEFINED )}, NULL },
{ "_DEFINECHECKBOX", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINECHECKBOX )}, NULL },
{ "_HMG_PRINTER_GETPAGEWIDTH", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_GETPAGEWIDTH )}, NULL },
{ "INT", {HB_FS_PUBLIC}, {HB_FUNCNAME( INT )}, NULL },
{ "GETHSCROLLBARHEIGHT", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETHSCROLLBARHEIGHT )}, NULL },
{ "GETBORDERHEIGHT", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETBORDERHEIGHT )}, NULL },
{ "_HMG_PRINTER_PREVIEW_DISABLESCROLLBARS", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_PREVIEW_DISABLESCROLLBARS )}, NULL },
{ "SETSCROLLRANGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETSCROLLRANGE )}, NULL },
{ "SETSCROLLPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETSCROLLPOS )}, NULL },
{ "_HMG_PRINTER_PREVIEW_DISABLEHSCROLLBAR", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_PREVIEW_DISABLEHSCROLLBAR )}, NULL },
{ "_ACTIVATEWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ACTIVATEWINDOW )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "HB_ADEL", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ADEL )}, NULL },
{ "SETFOCUS", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETFOCUS )}, NULL },
{ "_HMG_PRINTER_MOUSEZOOM", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_MOUSEZOOM )}, NULL },
{ "_HMG_PRINTER_THUMBNAILTOGGLE", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_THUMBNAILTOGGLE )}, NULL },
{ "CREATETHUMBNAILS", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( CREATETHUMBNAILS )}, NULL },
{ "SHOWWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( SHOWWINDOW )}, NULL },
{ "GETFORMTOOLTIPHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMTOOLTIPHANDLE )}, NULL },
{ "_DEFINEEMFFILE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINEEMFFILE )}, NULL },
{ "STRZERO", {HB_FS_PUBLIC}, {HB_FUNCNAME( STRZERO )}, NULL },
{ "SETTOOLTIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETTOOLTIP )}, NULL },
{ "GETCONTROLHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLHANDLE )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "PUTFILE", {HB_FS_PUBLIC}, {HB_FUNCNAME( PUTFILE )}, NULL },
{ "GETCURRENTFOLDER", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCURRENTFOLDER )}, NULL },
{ "HMG_SYSWAIT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HMG_SYSWAIT )}, NULL },
{ "HB_DIRSEPADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DIRSEPADD )}, NULL },
{ "HB_VFDIRECTORY", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_VFDIRECTORY )}, NULL },
{ "AEVAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( AEVAL )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "HB_FNAMEEXTSETDEF", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_FNAMEEXTSETDEF )}, NULL },
{ "_CREATEPDF", {HB_FS_PUBLIC}, {HB_FUNCNAME( _CREATEPDF )}, NULL },
{ "ASORT", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASORT )}, NULL },
{ "GETSTARTUPFOLDER", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETSTARTUPFOLDER )}, NULL },
{ "FILE", {HB_FS_PUBLIC}, {HB_FUNCNAME( FILE )}, NULL },
{ "HB_CWD", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_CWD )}, NULL },
{ "STRTRAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( STRTRAN )}, NULL },
{ "LEFT", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEFT )}, NULL },
{ "HB_TTOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_TTOS )}, NULL },
{ "HB_DATETIME", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DATETIME )}, NULL },
{ "MSGINFO", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGINFO )}, NULL },
{ "HB_USERNAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_USERNAME )}, NULL },
{ "_BEGININI", {HB_FS_PUBLIC}, {HB_FUNCNAME( _BEGININI )}, NULL },
{ "_GETINI", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETINI )}, NULL },
{ "_ENDINI", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ENDINI )}, NULL },
{ "NEW", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "TCDOMAIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( TCDOMAIL )}, NULL },
{ "_CSERVER", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NPORT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CUSER", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CPASS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_AORIGIN", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_ARECIPIENTS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_CCOPY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_LRECEIPT", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_NPRIORITY", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "ACTIVATE", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "LSUCCESS", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "HB_VFERASE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_VFERASE )}, NULL },
{ "GETSCROLLPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETSCROLLPOS )}, NULL },
{ "_HMG_PRINTER_CLEANPREVIEW", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_CLEANPREVIEW )}, NULL },
{ "_HMG_PRINTER_SPLTCHLDMOUSECURSOR", {HB_FS_STATIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_SPLTCHLDMOUSECURSOR )}, NULL },
{ "VALTYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( VALTYPE )}, NULL },
{ "GETCURSORPOS", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCURSORPOS )}, NULL },
{ "PTINRECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PTINRECT )}, NULL },
{ "SETWINDOWCURSOR", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETWINDOWCURSOR )}, NULL },
{ "_HMG_PRINTER_SPLTCHLDMOUSECLICK", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_SPLTCHLDMOUSECLICK )}, NULL },
{ "DIRECTORY", {HB_FS_PUBLIC}, {HB_FUNCNAME( DIRECTORY )}, NULL },
{ "FERASE", {HB_FS_PUBLIC}, {HB_FUNCNAME( FERASE )}, NULL },
{ "GETSCROLLRANGEMAX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETSCROLLRANGEMAX )}, NULL },
{ "_HMG_SETVSCROLLVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _HMG_SETVSCROLLVALUE )}, NULL },
{ "PLAYBEEP", {HB_FS_PUBLIC}, {HB_FUNCNAME( PLAYBEEP )}, NULL },
{ "INVALIDATERECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( INVALIDATERECT )}, NULL },
{ "_HMG_PRINTER_SHOWPAGE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_SHOWPAGE )}, NULL },
{ "_HMG_PRINTER_STARTDOC", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_STARTDOC )}, NULL },
{ "__MVEXIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( __MVEXIST )}, NULL },
{ "__MVPUT", {HB_FS_PUBLIC}, {HB_FUNCNAME( __MVPUT )}, NULL },
{ "MSGMINIGUIERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGMINIGUIERROR )}, NULL },
{ "_HMG_PRINTER_PRINTPAGE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_PRINTPAGE )}, NULL },
{ "_HMG_PRINTER_ENDDOC", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_ENDDOC )}, NULL },
{ "GETPRINTER", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( GETPRINTER )}, NULL },
{ "APRINTERS", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( APRINTERS )}, NULL },
{ "GETDEFAULTPRINTER", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( GETDEFAULTPRINTER )}, NULL },
{ "TYPE", {HB_FS_PUBLIC}, {HB_FUNCNAME( TYPE )}, NULL },
{ "_HMG_PRINTER_INITUSERMESSAGES", {HB_FS_PUBLIC}, {HB_FUNCNAME( _HMG_PRINTER_INITUSERMESSAGES )}, NULL },
{ "_DEFINEMODALWINDOW", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINEMODALWINDOW )}, NULL },
{ "_DEFINEBUTTON", {HB_FS_PUBLIC}, {HB_FUNCNAME( _DEFINEBUTTON )}, NULL },
{ "_HMG_PRINTER_H_PRINT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_H_PRINT )}, NULL },
{ "DTOC", {HB_FS_PUBLIC}, {HB_FUNCNAME( DTOC )}, NULL },
{ "UPPER", {HB_FS_PUBLIC}, {HB_FUNCNAME( UPPER )}, NULL },
{ "SETTEXTALIGN", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( SETTEXTALIGN )}, NULL },
{ "_HMG_PRINTER_C_PRINT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_C_PRINT )}, NULL },
{ "_HMG_PRINTER_H_MULTILINE_PRINT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_H_MULTILINE_PRINT )}, NULL },
{ "_HMG_PRINTER_C_MULTILINE_PRINT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_C_MULTILINE_PRINT )}, NULL },
{ "_HMG_PRINTER_H_IMAGE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_H_IMAGE )}, NULL },
{ "_HMG_PRINTER_C_IMAGE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_C_IMAGE )}, NULL },
{ "_HMG_PRINTER_H_LINE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_H_LINE )}, NULL },
{ "_HMG_PRINTER_C_LINE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_C_LINE )}, NULL },
{ "_HMG_PRINTER_H_RECTANGLE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_H_RECTANGLE )}, NULL },
{ "_HMG_PRINTER_C_RECTANGLE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_C_RECTANGLE )}, NULL },
{ "_HMG_PRINTER_H_ROUNDRECTANGLE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_H_ROUNDRECTANGLE )}, NULL },
{ "_HMG_PRINTER_C_ROUNDRECTANGLE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_C_ROUNDRECTANGLE )}, NULL },
{ "GETPRINTABLEAREAWIDTH", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( GETPRINTABLEAREAWIDTH )}, NULL },
{ "_HMG_PRINTER_GETPRINTERWIDTH", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_GETPRINTERWIDTH )}, NULL },
{ "GETPRINTABLEAREAHEIGHT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( GETPRINTABLEAREAHEIGHT )}, NULL },
{ "_HMG_PRINTER_GETPRINTERHEIGHT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_GETPRINTERHEIGHT )}, NULL },
{ "GETPRINTABLEAREAHORIZONTALOFFSET", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( GETPRINTABLEAREAHORIZONTALOFFSET )}, NULL },
{ "_HMG_PRINTER_GETPRINTABLEAREAPHYSICALOFFSETX", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_GETPRINTABLEAREAPHYSICALOFFSETX )}, NULL },
{ "_HMG_PRINTER_GETPRINTABLEAREALOGPIXELSX", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_GETPRINTABLEAREALOGPIXELSX )}, NULL },
{ "GETPRINTABLEAREAVERTICALOFFSET", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( GETPRINTABLEAREAVERTICALOFFSET )}, NULL },
{ "_HMG_PRINTER_GETPRINTABLEAREAPHYSICALOFFSETY", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_GETPRINTABLEAREAPHYSICALOFFSETY )}, NULL },
{ "_HMG_PRINTER_GETPRINTABLEAREALOGPIXELSY", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_GETPRINTABLEAREALOGPIXELSY )}, NULL },
{ "_HMG_PRINTER_PREVIEW_ENABLESCROLLBARS", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_PREVIEW_ENABLESCROLLBARS )}, NULL },
{ "_HMG_PRINTER_SETJOBNAME", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTER_SETJOBNAME )}, NULL },
{ "HB_DEFAULTVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULTVALUE )}, NULL },
{ "HMG_PRINTGETJOBINFO", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMG_PRINTGETJOBINFO )}, NULL },
{ "_HMG_PRINTGETJOBINFO", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTGETJOBINFO )}, NULL },
{ "HMG_PRINTERGETSTATUS", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( HMG_PRINTERGETSTATUS )}, NULL },
{ "_HMG_PRINTERGETSTATUS", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _HMG_PRINTERGETSTATUS )}, NULL },
{ "_GETCONTROLFREE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCONTROLFREE )}, NULL },
{ "INITEMFFILE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( INITEMFFILE )}, NULL },
{ "_SETNAMELIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETNAMELIST )}, NULL },
{ "C_SETEMFFILE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( C_SETEMFFILE )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_MINIPRINT, "miniprint.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_MINIPRINT
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_MINIPRINT )
   #include "hbiniseg.h"
#endif

HB_FUNC( _HMG_PRINTER_SHOWPREVIEW )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 7, 0 );
	hb_xvmSetLine( 78 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushStringConst( "IsVistaThemed", 13 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 79 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushStringConst( "IsVistaThemed", 13 );
	hb_xvmPushFuncSymbol( symbols + 2 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmDuplicate();
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_stackPop();
	hb_xvmPushFuncSymbol( symbols + 3 );
	if( hb_xvmFunction( 0 ) ) break;
lab00001: ;
	if( hb_xvmDo( 2 ) ) break;
lab00002: ;
	hb_xvmSetLine( 82 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 5 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 17L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_hmg_print_preview_", 19 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 3L ) ) break;
	hb_xvmSetLine( 83 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 4L ) ) break;
	hb_xvmSetLine( 84 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 6L ) ) break;
	hb_xvmSetLine( 85 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 7L ) ) break;
	hb_xvmSetLine( 86 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 8L ) ) break;
	hb_xvmSetLine( 87 );
	hb_xvmPushInteger( 10 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 9L ) ) break;
	hb_xvmSetLine( 88 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 10L ) ) break;
	hb_xvmSetLine( 89 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 11L ) ) break;
	hb_xvmSetLine( 90 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 13L ) ) break;
	hb_xvmSetLine( 91 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 15L ) ) break;
	hb_xvmSetLine( 93 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 91L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 95 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 275L ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 97 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 91L ) ) break;
	hb_xvmSetLine( 98 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 275L ) ) break;
	hb_xvmSetLine( 100 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 1 ) ) break;
	goto lab00004;
lab00003: ;
	hb_xvmSetLine( 104 );
	hb_xvmLocalSetInt( 1, 0L );
lab00004: ;
	hb_xvmSetLine( 108 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmSetLine( 109 );
	/* *** END PROC *** */
	break;
lab00005: ;
	hb_xvmSetLine( 112 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 113 );
	/* *** END PROC *** */
	break;
lab00006: ;
	hb_xvmSetLine( 116 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 184L ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 118 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 184L ) ) break;
	hb_xvmSetLine( 120 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 11 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDivide() ) break;
	hb_xvmPushDouble( * ( double * ) ")\\\x8F\xC2\xF5(\xE4\?", 10, 2 );
	if( hb_xvmMult() ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 5L ) ) break;
	hb_xvmSetLine( 122 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 370L, &fValue ) ) break;
	if( !fValue )
		goto lab00007;
	hb_xvmSetLine( 123 );
	hb_xvmPushInteger( -250 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 16L ) ) break;
	goto lab00008;
lab00007: ;
	hb_xvmSetLine( 125 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 16L ) ) break;
lab00008: ;
	hb_xvmSetLine( 128 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushStringConst( "_HMG_PRINTER_Wait", 17 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 310 );
	hb_xvmPushInteger( 85 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmArrayGen( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmArrayGen( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 58 ) ) break;
	hb_xvmSetLine( 129 );
	hb_xvmPushStringConst( "label_1", 7 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 316L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 317L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 406L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 334L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 360L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 361L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 362L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 363L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 364L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 358L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 359L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 319L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 329L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 330L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 312L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 313L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 314L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 315L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 324L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 341L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 308L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 304L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 326L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 327L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 348L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 368L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 278L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 399L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 303L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 288L ) ) break;
	hb_xvmSetLine( 130 );
	hb_xvmPushInteger( 30 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmSetLine( 131 );
	hb_xvmPushInteger( 5 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmSetLine( 132 );
	hb_xvmPushInteger( 300 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmSetLine( 133 );
	hb_xvmPushInteger( 30 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmSetLine( 134 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmArrayItemPush( 29L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 334L ) ) break;
	hb_xvmSetLine( 135 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 288L ) ) break;
	hb_xvmSetLine( 136 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 316L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 317L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 332L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 331L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 334L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 320L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 321L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 322L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 323L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 312L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 360L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 361L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 362L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 363L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 364L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 358L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 359L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 319L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 324L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 329L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 330L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 313L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 315L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 314L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 308L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 341L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 288L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 304L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 326L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 327L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 368L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 278L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 406L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 399L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 348L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 303L ) ) break;
	if( hb_xvmDo( 36 ) ) break;
	hb_xvmSetLine( 137 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 139 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushStringConst( "_HMG_PRINTER_Wait", 17 );
	hb_xvmPushStringConst( "Center", 6 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 155 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PPNAV", 18 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushStringConst( " [", 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 16 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "/", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 16 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 18L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "]", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmAddInt( -103L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushStringConst( "IsVistaThemed", 13 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmPushInteger( 25 );
	goto lab00010;
lab00009: ;
	hb_xvmPushInteger( 0 );
lab00010: ;
	if( hb_xvmMinus() ) break;
	hb_xvmPushFuncSymbol( symbols + 10 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmAddInt( -66L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushStringConst( "IsVistaThemed", 13 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmPushInteger( 25 );
	goto lab00012;
lab00011: ;
	hb_xvmPushInteger( 0 );
lab00012: ;
	if( hb_xvmMinus() ) break;
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmArrayGen( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmArrayGen( 2 );
	hb_xvmPushNil();
	{
		static const HB_BYTE codeblock[ 63 ] = {
			176, 18, 0, 106, 14, 73, 115, 86, 105, 115, 116, 97, 84, 104, 101, 109, 
			101, 100, 0, 20, 1, 176, 18, 0, 106, 8, 97, 67, 111, 111, 114, 100, 
			115, 0, 20, 1, 176, 18, 0, 106, 19, 95, 72, 77, 71, 95, 80, 82, 
			73, 78, 84, 69, 82, 95, 108, 70, 108, 97, 103, 0, 12, 1, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 19, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushInteger( 128 );
	hb_xvmPushInteger( 128 );
	hb_xvmPushInteger( 128 );
	hb_xvmArrayGen( 3 );
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 19, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	{
		static const HB_BYTE codeblock[ 16 ] = {
			176, 20, 0, 12, 0, 28, 9, 176, 19, 0, 12, 0, 25, 3, 100, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 58 ) ) break;
	hb_xvmSetLine( 157 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 159 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushStringConst( "ToolBar_1", 9 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushInteger( 25 );
	hb_xvmPushInteger( 25 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 22 ) ) break;
	hb_xvmSetLine( 161 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushStringConst( "b2", 2 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 27L ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	{
		static const HB_BYTE codeblock[ 13 ] = {
			98, 6, 0, 92, 4, 148, 169, 176, 19, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "HP_BACK2", 8 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushInteger( -1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 22 ) ) break;
	hb_xvmSetLine( 163 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushStringConst( "b3", 2 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 27L ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	{
		static const HB_BYTE codeblock[ 13 ] = {
			98, 6, 0, 92, 4, 148, 170, 176, 19, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "HP_NEXT2", 8 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmArrayItemPush( 5L ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushInteger( -1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 22 ) ) break;
	hb_xvmSetLine( 165 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushStringConst( "b1", 2 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 27L ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	{
		static const HB_BYTE codeblock[ 13 ] = {
			122, 98, 6, 0, 92, 4, 2, 176, 19, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "HP_TOP2", 7 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushInteger( -1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 22 ) ) break;
	hb_xvmSetLine( 167 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushStringConst( "b4", 2 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 27L ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	{
		static const HB_BYTE codeblock[ 18 ] = {
			98, 6, 0, 92, 18, 1, 98, 6, 0, 92, 4, 2, 176, 19, 0, 12, 
			0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "HP_END2", 7 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmArrayItemPush( 6L ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushInteger( -1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 22 ) ) break;
	hb_xvmSetLine( 169 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushStringConst( "GoToPage", 8 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 27L ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 24, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "HP_GOPAGE2", 10 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	hb_xvmPushStringConst( " [Ctrl+G]", 9 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushInteger( -1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 22 ) ) break;
	hb_xvmSetLine( 171 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushStringConst( "thumbswitch", 11 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 27L ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 25, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "HP_THUMBNAIL2", 13 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmArrayItemPush( 28L ) ) break;
	hb_xvmPushStringConst( " [Ctrl+T]", 9 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushInteger( -1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 22 ) ) break;
	hb_xvmSetLine( 173 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushStringConst( "b5", 2 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 27L ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 26, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "HP_ZOOM2", 8 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmArrayItemPush( 8L ) ) break;
	hb_xvmPushStringConst( " [*]", 4 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushInteger( -1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 22 ) ) break;
	hb_xvmSetLine( 175 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushStringConst( "b12", 3 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 27L ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 27, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "HP_PRINT2", 9 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmArrayItemPush( 9L ) ) break;
	hb_xvmPushStringConst( " [Ctrl+P]", 9 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushInteger( -1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 22 ) ) break;
	hb_xvmSetLine( 177 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushStringConst( "b7", 2 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 27L ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 28, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "HP_SAVE2", 8 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmArrayItemPush( 27L ) ) break;
	hb_xvmPushStringConst( " [Ctrl+S]", 9 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushInteger( -1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 22 ) ) break;
	hb_xvmSetLine( 179 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushStringConst( "b8", 2 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 27L ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 29, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "HP_EMAIL", 8 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmArrayItemPush( 30L ) ) break;
	hb_xvmPushStringConst( " [Ctrl+E]", 9 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushInteger( -1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 22 ) ) break;
	hb_xvmSetLine( 181 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushStringConst( "b6", 2 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 27L ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 30, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushStringConst( "HP_CLOSE2", 9 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmArrayItemPush( 26L ) ) break;
	hb_xvmPushStringConst( " [Ctrl+C]", 9 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushInteger( -1 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 22 ) ) break;
	hb_xvmSetLine( 183 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 196 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmAddInt( -103L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushStringConst( "IsVistaThemed", 13 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmPushInteger( 25 );
	goto lab00014;
lab00013: ;
	hb_xvmPushInteger( 0 );
lab00014: ;
	if( hb_xvmMinus() ) break;
	hb_xvmPushFuncSymbol( symbols + 10 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmAddInt( -140L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushStringConst( "IsVistaThemed", 13 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmPushInteger( 25 );
	goto lab00016;
lab00015: ;
	hb_xvmPushInteger( 0 );
lab00016: ;
	if( hb_xvmMinus() ) break;
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushFuncSymbol( symbols + 10 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmAddInt( -140L ) ) break;
	if( hb_xvmMultByInt( 2L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 17 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmAddInt( -103L ) ) break;
	if( hb_xvmMultByInt( 2L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 33, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 34, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 35, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 36, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 37, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 38, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	hb_xvmPushNil();
	if( hb_xvmDo( 21 ) ) break;
	hb_xvmSetLine( 198 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 200 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 202 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SpltChldMouseClick()", 33 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 204 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 206 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PPNAV", 18 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 208 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 216 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PRINTPAGES", 23 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmArrayItemPush( 9L ) ) break;
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 430 );
	hb_xvmPushInteger( 170 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmArrayGen( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmArrayGen( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 58 ) ) break;
	hb_xvmSetLine( 218 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 27 );
	{
		static const HB_BYTE codeblock[ 188 ] = {
			176, 44, 0, 176, 45, 0, 106, 24, 95, 72, 77, 71, 95, 80, 82, 73, 
			78, 84, 69, 82, 95, 80, 82, 73, 78, 84, 80, 65, 71, 69, 83, 0, 
			12, 1, 20, 1, 176, 46, 0, 176, 45, 0, 106, 25, 95, 72, 77, 71, 
			95, 80, 82, 73, 78, 84, 69, 82, 95, 83, 72, 79, 87, 80, 82, 69, 
			86, 73, 69, 87, 0, 12, 1, 20, 1, 176, 46, 0, 176, 45, 0, 106, 
			28, 95, 72, 77, 71, 95, 80, 82, 73, 78, 84, 69, 82, 95, 83, 72, 
			79, 87, 84, 72, 85, 77, 66, 78, 65, 73, 76, 83, 0, 12, 1, 20, 
			1, 176, 46, 0, 176, 45, 0, 106, 19, 95, 72, 77, 71, 95, 80, 82, 
			73, 78, 84, 69, 82, 95, 80, 80, 78, 65, 86, 0, 12, 1, 20, 1, 
			176, 15, 0, 106, 25, 95, 72, 77, 71, 95, 80, 82, 73, 78, 84, 69, 
			82, 95, 83, 72, 79, 87, 80, 82, 69, 86, 73, 69, 87, 0, 106, 9, 
			115, 101, 116, 102, 111, 99, 117, 115, 0, 12, 2, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 219 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 13 );
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 47, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 221 );
	hb_xvmPushStringConst( "Frame_1", 7 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 316L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 317L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 406L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 318L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 358L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 359L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 364L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 345L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 330L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 399L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 312L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 313L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 314L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 315L ) ) break;
	hb_xvmSetLine( 222 );
	hb_xvmPushInteger( 5 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmSetLine( 223 );
	hb_xvmPushInteger( 10 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmSetLine( 224 );
	hb_xvmPushInteger( 275 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmSetLine( 225 );
	hb_xvmPushInteger( 150 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmSetLine( 226 );
	hb_xvmPushStringConst( "Arial", 5 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmSetLine( 227 );
	hb_xvmPushInteger( 9 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmSetLine( 228 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmArrayItemPush( 15L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 318L ) ) break;
	hb_xvmSetLine( 229 );
	hb_xvmPushFuncSymbol( symbols + 48 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 316L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 317L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 331L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 332L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 320L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 321L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 318L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 322L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 323L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 345L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 312L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 313L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 315L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 314L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 358L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 359L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 364L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 330L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 406L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 399L ) ) break;
	if( hb_xvmDo( 20 ) ) break;
	hb_xvmSetLine( 231 );
	hb_xvmPushStringConst( "Radio_1", 7 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 316L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 317L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 406L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 372L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 334L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 324L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 335L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 326L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 327L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 358L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 359L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 373L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 329L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 330L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 328L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 312L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 313L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 314L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 308L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 364L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 342L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 315L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 399L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 279L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 285L ) ) break;
	hb_xvmSetLine( 232 );
	hb_xvmPushInteger( 25 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmSetLine( 233 );
	hb_xvmPushInteger( 20 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmSetLine( 234 );
	hb_xvmPushStringConst( "Arial", 5 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmSetLine( 235 );
	hb_xvmPushInteger( 9 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmSetLine( 236 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 334L ) ) break;
	hb_xvmSetLine( 237 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmArrayItemPush( 16L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmArrayItemPush( 17L ) ) break;
	hb_xvmArrayGen( 2 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 372L ) ) break;
	hb_xvmSetLine( 238 );
	{
		static const HB_BYTE codeblock[ 726 ] = {
			0, 0, 0, 0, 176, 49, 0, 98, 7, 0, 93, 254, 0, 1, 98, 7, 
			0, 93, 255, 0, 1, 106, 6, 118, 97, 108, 117, 101, 0, 12, 3, 122, 
			8, 29, 66, 1, 176, 50, 0, 106, 24, 95, 72, 77, 71, 95, 80, 82, 
			73, 78, 84, 69, 82, 95, 80, 82, 73, 78, 84, 80, 65, 71, 69, 83, 
			0, 106, 8, 76, 97, 98, 101, 108, 95, 49, 0, 106, 8, 69, 110, 97, 
			98, 108, 101, 100, 0, 9, 20, 4, 176, 50, 0, 106, 24, 95, 72, 77, 
			71, 95, 80, 82, 73, 78, 84, 69, 82, 95, 80, 82, 73, 78, 84, 80, 
			65, 71, 69, 83, 0, 106, 8, 76, 97, 98, 101, 108, 95, 50, 0, 106, 
			8, 69, 110, 97, 98, 108, 101, 100, 0, 9, 20, 4, 176, 50, 0, 106, 
			24, 95, 72, 77, 71, 95, 80, 82, 73, 78, 84, 69, 82, 95, 80, 82, 
			73, 78, 84, 80, 65, 71, 69, 83, 0, 106, 10, 83, 112, 105, 110, 110, 
			101, 114, 95, 49, 0, 106, 8, 69, 110, 97, 98, 108, 101, 100, 0, 9, 
			20, 4, 176, 50, 0, 106, 24, 95, 72, 77, 71, 95, 80, 82, 73, 78, 
			84, 69, 82, 95, 80, 82, 73, 78, 84, 80, 65, 71, 69, 83, 0, 106, 
			10, 83, 112, 105, 110, 110, 101, 114, 95, 50, 0, 106, 8, 69, 110, 97, 
			98, 108, 101, 100, 0, 9, 20, 4, 176, 50, 0, 106, 24, 95, 72, 77, 
			71, 95, 80, 82, 73, 78, 84, 69, 82, 95, 80, 82, 73, 78, 84, 80, 
			65, 71, 69, 83, 0, 106, 8, 67, 111, 109, 98, 111, 95, 49, 0, 106, 
			8, 69, 110, 97, 98, 108, 101, 100, 0, 9, 20, 4, 176, 50, 0, 106, 
			24, 95, 72, 77, 71, 95, 80, 82, 73, 78, 84, 69, 82, 95, 80, 82, 
			73, 78, 84, 80, 65, 71, 69, 83, 0, 106, 8, 76, 97, 98, 101, 108, 
			95, 52, 0, 106, 8, 69, 110, 97, 98, 108, 101, 100, 0, 9, 12, 4, 
			26, 117, 1, 176, 50, 0, 106, 24, 95, 72, 77, 71, 95, 80, 82, 73, 
			78, 84, 69, 82, 95, 80, 82, 73, 78, 84, 80, 65, 71, 69, 83, 0, 
			106, 8, 76, 97, 98, 101, 108, 95, 49, 0, 106, 8, 69, 110, 97, 98, 
			108, 101, 100, 0, 120, 20, 4, 176, 50, 0, 106, 24, 95, 72, 77, 71, 
			95, 80, 82, 73, 78, 84, 69, 82, 95, 80, 82, 73, 78, 84, 80, 65, 
			71, 69, 83, 0, 106, 8, 76, 97, 98, 101, 108, 95, 50, 0, 106, 8, 
			69, 110, 97, 98, 108, 101, 100, 0, 120, 20, 4, 176, 50, 0, 106, 24, 
			95, 72, 77, 71, 95, 80, 82, 73, 78, 84, 69, 82, 95, 80, 82, 73, 
			78, 84, 80, 65, 71, 69, 83, 0, 106, 10, 83, 112, 105, 110, 110, 101, 
			114, 95, 49, 0, 106, 8, 69, 110, 97, 98, 108, 101, 100, 0, 120, 20, 
			4, 176, 50, 0, 106, 24, 95, 72, 77, 71, 95, 80, 82, 73, 78, 84, 
			69, 82, 95, 80, 82, 73, 78, 84, 80, 65, 71, 69, 83, 0, 106, 10, 
			83, 112, 105, 110, 110, 101, 114, 95, 50, 0, 106, 8, 69, 110, 97, 98, 
			108, 101, 100, 0, 120, 20, 4, 176, 50, 0, 106, 24, 95, 72, 77, 71, 
			95, 80, 82, 73, 78, 84, 69, 82, 95, 80, 82, 73, 78, 84, 80, 65, 
			71, 69, 83, 0, 106, 8, 67, 111, 109, 98, 111, 95, 49, 0, 106, 8, 
			69, 110, 97, 98, 108, 101, 100, 0, 120, 20, 4, 176, 50, 0, 106, 24, 
			95, 72, 77, 71, 95, 80, 82, 73, 78, 84, 69, 82, 95, 80, 82, 73, 
			78, 84, 80, 65, 71, 69, 83, 0, 106, 8, 76, 97, 98, 101, 108, 95, 
			52, 0, 106, 8, 69, 110, 97, 98, 108, 101, 100, 0, 120, 20, 4, 176, 
			15, 0, 106, 24, 95, 72, 77, 71, 95, 80, 82, 73, 78, 84, 69, 82, 
			95, 80, 82, 73, 78, 84, 80, 65, 71, 69, 83, 0, 106, 10, 83, 112, 
			105, 110, 110, 101, 114, 95, 49, 0, 106, 9, 83, 101, 116, 70, 111, 99, 
			117, 115, 0, 12, 3, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 335L ) ) break;
	hb_xvmSetLine( 239 );
	hb_xvmPushFuncSymbol( symbols + 51 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 316L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 317L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 332L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 331L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 372L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 334L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 322L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 323L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 324L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 335L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 320L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 373L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 329L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 330L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 328L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 312L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 313L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 315L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 314L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 358L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 359L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 364L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 279L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 285L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 342L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 308L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 326L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 327L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 406L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 399L ) ) break;
	if( hb_xvmDo( 30 ) ) break;
	hb_xvmSetLine( 241 );
	hb_xvmPushStringConst( "Label_1", 7 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 316L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 317L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 406L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 334L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 360L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 361L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 362L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 363L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 364L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 358L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 359L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 319L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 329L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 330L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 312L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 313L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 314L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 315L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 324L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 341L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 308L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 304L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 326L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 327L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 348L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 368L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 278L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 399L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 303L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 288L ) ) break;
	hb_xvmSetLine( 242 );
	hb_xvmPushInteger( 84 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmSetLine( 243 );
	hb_xvmPushInteger( 50 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmSetLine( 244 );
	hb_xvmPushInteger( 50 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmSetLine( 245 );
	hb_xvmPushInteger( 25 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmSetLine( 246 );
	hb_xvmPushStringConst( "Arial", 5 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmSetLine( 247 );
	hb_xvmPushInteger( 8 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmSetLine( 248 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmArrayItemPush( 18L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 334L ) ) break;
	hb_xvmSetLine( 249 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 316L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 317L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 332L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 331L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 334L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 320L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 321L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 322L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 323L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 312L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 360L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 361L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 362L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 363L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 364L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 358L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 359L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 319L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 324L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 329L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 330L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 313L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 315L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 314L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 308L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 341L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 288L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 304L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 326L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 327L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 368L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 278L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 406L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 399L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 348L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 303L ) ) break;
	if( hb_xvmDo( 36 ) ) break;
	hb_xvmSetLine( 251 );
	hb_xvmPushStringConst( "Spinner_1", 9 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 316L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 317L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 334L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 366L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 367L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 324L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 335L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 327L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 326L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 329L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 312L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 313L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 314L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 315L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 358L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 359L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 372L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 279L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 303L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 342L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 304L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 330L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 399L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 328L ) ) break;
	hb_xvmSetLine( 252 );
	hb_xvmPushInteger( 81 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmSetLine( 253 );
	hb_xvmPushInteger( 110 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmSetLine( 254 );
	hb_xvmPushInteger( 50 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmSetLine( 255 );
	hb_xvmPushStringConst( "Arial", 5 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmSetLine( 256 );
	hb_xvmPushInteger( 9 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmSetLine( 257 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 334L ) ) break;
	hb_xvmSetLine( 258 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 366L ) ) break;
	hb_xvmSetLine( 259 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 18L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 367L ) ) break;
	hb_xvmSetLine( 260 );
	hb_xvmPushFuncSymbol( symbols + 52 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 316L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 317L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 332L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 331L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 320L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 334L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 322L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 323L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 366L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 367L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 324L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 335L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 327L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 326L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 321L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 329L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 279L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 330L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 328L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 312L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 313L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 315L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 314L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 303L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 342L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 304L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 358L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 359L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 372L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 399L ) ) break;
	if( hb_xvmDo( 30 ) ) break;
	hb_xvmSetLine( 262 );
	hb_xvmPushStringConst( "Label_2", 7 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 316L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 317L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 406L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 334L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 360L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 361L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 362L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 363L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 364L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 358L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 359L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 319L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 329L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 330L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 312L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 313L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 314L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 315L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 324L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 341L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 308L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 304L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 326L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 327L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 348L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 368L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 278L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 399L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 303L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 288L ) ) break;
	hb_xvmSetLine( 263 );
	hb_xvmPushInteger( 84 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmSetLine( 264 );
	hb_xvmPushInteger( 175 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmSetLine( 265 );
	hb_xvmPushInteger( 35 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmSetLine( 266 );
	hb_xvmPushInteger( 25 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmSetLine( 267 );
	hb_xvmPushStringConst( "Arial", 5 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmSetLine( 268 );
	hb_xvmPushInteger( 8 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmSetLine( 269 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmArrayItemPush( 19L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 334L ) ) break;
	hb_xvmSetLine( 270 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 316L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 317L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 332L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 331L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 334L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 320L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 321L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 322L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 323L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 312L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 360L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 361L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 362L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 363L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 364L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 358L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 359L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 319L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 324L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 329L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 330L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 313L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 315L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 314L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 308L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 341L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 288L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 304L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 326L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 327L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 368L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 278L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 406L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 399L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 348L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 303L ) ) break;
	if( hb_xvmDo( 36 ) ) break;
	hb_xvmSetLine( 272 );
	hb_xvmPushStringConst( "Spinner_2", 9 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 316L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 317L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 334L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 366L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 367L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 324L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 335L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 327L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 326L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 329L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 312L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 313L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 314L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 315L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 358L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 359L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 372L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 279L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 303L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 342L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 304L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 330L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 399L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 328L ) ) break;
	hb_xvmSetLine( 273 );
	hb_xvmPushInteger( 81 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmSetLine( 274 );
	hb_xvmPushInteger( 205 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmSetLine( 275 );
	hb_xvmPushInteger( 50 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmSetLine( 276 );
	hb_xvmPushStringConst( "Arial", 5 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmSetLine( 277 );
	hb_xvmPushInteger( 9 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmSetLine( 278 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 18L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 334L ) ) break;
	hb_xvmSetLine( 279 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 366L ) ) break;
	hb_xvmSetLine( 280 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 18L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 367L ) ) break;
	hb_xvmSetLine( 281 );
	hb_xvmPushFuncSymbol( symbols + 52 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 316L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 317L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 332L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 331L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 320L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 334L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 322L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 323L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 366L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 367L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 324L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 335L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 327L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 326L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 321L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 329L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 279L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 330L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 328L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 312L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 313L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 315L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 314L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 303L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 342L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 304L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 358L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 359L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 372L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 399L ) ) break;
	if( hb_xvmDo( 30 ) ) break;
	hb_xvmSetLine( 283 );
	hb_xvmPushStringConst( "Label_4", 7 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 316L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 317L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 406L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 334L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 360L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 361L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 362L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 363L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 364L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 358L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 359L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 319L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 329L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 330L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 312L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 313L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 314L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 315L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 324L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 341L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 308L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 304L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 326L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 327L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 348L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 368L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 278L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 399L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 303L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 288L ) ) break;
	hb_xvmSetLine( 284 );
	hb_xvmPushInteger( 115 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmSetLine( 285 );
	hb_xvmPushInteger( 50 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmSetLine( 286 );
	hb_xvmPushInteger( 55 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmSetLine( 287 );
	hb_xvmPushInteger( 25 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmSetLine( 288 );
	hb_xvmPushStringConst( "Arial", 5 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmSetLine( 289 );
	hb_xvmPushInteger( 8 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmSetLine( 290 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmArrayItemPush( 9L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 334L ) ) break;
	hb_xvmSetLine( 291 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 316L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 317L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 332L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 331L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 334L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 320L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 321L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 322L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 323L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 312L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 360L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 361L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 362L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 363L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 364L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 358L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 359L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 319L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 324L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 329L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 330L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 313L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 315L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 314L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 308L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 341L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 288L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 304L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 326L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 327L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 368L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 278L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 406L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 399L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 348L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 303L ) ) break;
	if( hb_xvmDo( 36 ) ) break;
	hb_xvmSetLine( 293 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 89L ) ) break;
	hb_xvmPushStringConst( "Combo_1", 7 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 316L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 317L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 406L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 337L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 334L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 324L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 358L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 359L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 326L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 328L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 365L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 378L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 379L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 372L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 335L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 327L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 338L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 329L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 330L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 312L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 313L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 301L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 302L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 314L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 344L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 290L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 291L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 292L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 315L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 303L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 381L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 373L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 278L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 403L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 370L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 399L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 371L ) ) break;
	hb_xvmSetLine( 294 );
	hb_xvmPushInteger( 113 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmSetLine( 295 );
	hb_xvmPushInteger( 110 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmSetLine( 296 );
	hb_xvmPushInteger( 145 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmSetLine( 297 );
	hb_xvmPushStringConst( "Arial", 5 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmSetLine( 298 );
	hb_xvmPushInteger( 9 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmSetLine( 299 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 334L ) ) break;
	hb_xvmSetLine( 300 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmArrayItemPush( 21L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmArrayItemPush( 23L ) ) break;
	hb_xvmArrayGen( 3 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 337L ) ) break;
	hb_xvmSetLine( 301 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 89L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 53 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 316L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 317L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 332L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 331L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 320L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 337L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 334L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 322L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 323L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 324L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 335L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 321L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 326L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 327L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 338L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 329L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 330L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 328L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 365L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 312L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 313L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 315L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 314L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 301L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 302L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 291L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 292L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 344L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 290L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 373L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 406L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 370L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 371L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 358L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 359L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 378L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 379L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 372L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 278L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 303L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 381L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 403L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 399L ) ) break;
	if( hb_xvmDo( 43 ) ) break;
	hb_xvmSetLine( 303 );
	hb_xvmPushStringConst( "Ok", 2 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 316L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 317L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 406L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 318L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 319L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 324L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 325L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 326L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 327L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 328L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 329L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 330L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 333L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 286L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 351L ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 364L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 284L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 312L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 313L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 314L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 315L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 360L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 399L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 358L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 359L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 403L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 278L ) ) break;
	hb_xvmSetLine( 304 );
	hb_xvmPushInteger( 10 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmSetLine( 305 );
	hb_xvmPushInteger( 300 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmSetLine( 306 );
	hb_xvmPushInteger( 105 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmSetLine( 307 );
	hb_xvmPushInteger( 25 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmSetLine( 308 );
	hb_xvmPushStringConst( "Arial", 5 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmSetLine( 309 );
	hb_xvmPushInteger( 9 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmSetLine( 310 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmArrayItemPush( 11L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 318L ) ) break;
	hb_xvmSetLine( 311 );
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 47, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 319L ) ) break;
	hb_xvmSetLine( 312 );
	hb_xvmPushFuncSymbol( symbols + 54 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 316L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 317L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 332L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 331L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 318L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 319L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 320L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 321L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 333L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 324L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 326L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 327L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 325L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 364L ) ) break;
	if( hb_xvmNot() ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 329L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 330L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 328L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 278L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 286L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 322L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 323L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 312L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 313L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 315L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 314L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 368L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 280L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 281L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 358L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 359L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 283L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 284L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 352L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 287L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 339L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 282L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 372L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 279L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 399L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 403L ) ) break;
	if( hb_xvmDo( 40 ) ) break;
	hb_xvmSetLine( 314 );
	hb_xvmPushStringConst( "Cancel", 6 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 316L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 317L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 406L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 318L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 319L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 324L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 325L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 326L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 327L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 328L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 329L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 330L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 333L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 286L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 351L ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 364L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 284L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 312L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 313L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 314L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 315L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 360L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 399L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 358L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 359L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 403L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 278L ) ) break;
	hb_xvmSetLine( 315 );
	hb_xvmPushInteger( 40 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmSetLine( 316 );
	hb_xvmPushInteger( 300 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmSetLine( 317 );
	hb_xvmPushInteger( 105 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmSetLine( 318 );
	hb_xvmPushInteger( 25 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmSetLine( 319 );
	hb_xvmPushStringConst( "Arial", 5 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmSetLine( 320 );
	hb_xvmPushInteger( 9 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmSetLine( 321 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmArrayItemPush( 12L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 318L ) ) break;
	hb_xvmSetLine( 322 );
	{
		static const HB_BYTE codeblock[ 188 ] = {
			176, 46, 0, 176, 45, 0, 106, 25, 95, 72, 77, 71, 95, 80, 82, 73, 
			78, 84, 69, 82, 95, 83, 72, 79, 87, 80, 82, 69, 86, 73, 69, 87, 
			0, 12, 1, 20, 1, 176, 46, 0, 176, 45, 0, 106, 28, 95, 72, 77, 
			71, 95, 80, 82, 73, 78, 84, 69, 82, 95, 83, 72, 79, 87, 84, 72, 
			85, 77, 66, 78, 65, 73, 76, 83, 0, 12, 1, 20, 1, 176, 46, 0, 
			176, 45, 0, 106, 19, 95, 72, 77, 71, 95, 80, 82, 73, 78, 84, 69, 
			82, 95, 80, 80, 78, 65, 86, 0, 12, 1, 20, 1, 176, 44, 0, 176, 
			45, 0, 106, 24, 95, 72, 77, 71, 95, 80, 82, 73, 78, 84, 69, 82, 
			95, 80, 82, 73, 78, 84, 80, 65, 71, 69, 83, 0, 12, 1, 20, 1, 
			176, 15, 0, 106, 25, 95, 72, 77, 71, 95, 80, 82, 73, 78, 84, 69, 
			82, 95, 83, 72, 79, 87, 80, 82, 69, 86, 73, 69, 87, 0, 106, 9, 
			115, 101, 116, 102, 111, 99, 117, 115, 0, 12, 2, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 319L ) ) break;
	hb_xvmSetLine( 323 );
	hb_xvmPushFuncSymbol( symbols + 54 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 316L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 317L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 332L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 331L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 318L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 319L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 320L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 321L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 333L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 324L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 326L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 327L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 325L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 364L ) ) break;
	if( hb_xvmNot() ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 329L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 330L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 328L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 278L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 286L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 322L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 323L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 312L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 313L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 315L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 314L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 368L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 280L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 281L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 358L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 359L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 283L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 284L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 352L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 287L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 339L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 282L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 372L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 279L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 399L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 403L ) ) break;
	if( hb_xvmDo( 40 ) ) break;
	hb_xvmSetLine( 325 );
	hb_xvmPushStringConst( "Label_3", 7 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 316L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 317L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 406L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 334L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 360L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 361L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 362L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 363L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 364L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 358L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 359L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 319L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 329L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 330L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 312L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 313L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 314L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 315L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 324L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 341L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 308L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 304L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 326L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 327L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 348L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 368L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 278L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 399L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 303L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 288L ) ) break;
	hb_xvmSetLine( 326 );
	hb_xvmPushInteger( 103 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmSetLine( 327 );
	hb_xvmPushInteger( 295 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmSetLine( 328 );
	hb_xvmPushInteger( 55 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmSetLine( 329 );
	hb_xvmPushInteger( 25 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmSetLine( 330 );
	hb_xvmPushStringConst( "Arial", 5 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmSetLine( 331 );
	hb_xvmPushInteger( 8 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmSetLine( 332 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 334L ) ) break;
	hb_xvmSetLine( 333 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 316L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 317L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 332L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 331L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 334L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 320L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 321L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 322L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 323L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 312L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 360L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 361L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 362L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 363L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 364L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 358L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 359L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 319L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 324L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 329L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 330L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 313L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 315L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 314L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 308L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 341L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 288L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 304L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 326L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 327L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 368L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 278L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 406L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 399L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 348L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 303L ) ) break;
	if( hb_xvmDo( 36 ) ) break;
	hb_xvmSetLine( 335 );
	hb_xvmPushStringConst( "Spinner_3", 9 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 316L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 317L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 334L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 366L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 367L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 324L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 335L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 327L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 326L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 329L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 312L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 313L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 314L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 315L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 358L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 359L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 372L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 279L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 303L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 342L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 304L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 330L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 399L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 328L ) ) break;
	hb_xvmSetLine( 336 );
	hb_xvmPushInteger( 100 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmSetLine( 337 );
	hb_xvmPushInteger( 355 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmSetLine( 338 );
	hb_xvmPushInteger( 50 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmSetLine( 339 );
	hb_xvmPushStringConst( "Arial", 5 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmSetLine( 340 );
	hb_xvmPushInteger( 9 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmSetLine( 341 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 14L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 334L ) ) break;
	hb_xvmSetLine( 342 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 366L ) ) break;
	hb_xvmSetLine( 343 );
	hb_xvmPushInteger( 999 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 367L ) ) break;
	hb_xvmSetLine( 344 );
	{
		static const HB_BYTE codeblock[ 194 ] = {
			176, 55, 0, 106, 11, 67, 104, 101, 99, 107, 66, 111, 120, 95, 49, 0, 
			106, 24, 95, 72, 77, 71, 95, 80, 82, 73, 78, 84, 69, 82, 95, 80, 
			82, 73, 78, 84, 80, 65, 71, 69, 83, 0, 12, 2, 29, 148, 0, 176, 
			49, 0, 98, 7, 0, 93, 254, 0, 1, 98, 7, 0, 93, 255, 0, 1, 
			106, 6, 86, 97, 108, 117, 101, 0, 12, 3, 122, 15, 28, 59, 176, 50, 
			0, 106, 24, 95, 72, 77, 71, 95, 80, 82, 73, 78, 84, 69, 82, 95, 
			80, 82, 73, 78, 84, 80, 65, 71, 69, 83, 0, 106, 11, 67, 104, 101, 
			99, 107, 66, 111, 120, 95, 49, 0, 106, 8, 69, 110, 97, 98, 108, 101, 
			100, 0, 120, 12, 4, 25, 60, 176, 50, 0, 106, 24, 95, 72, 77, 71, 
			95, 80, 82, 73, 78, 84, 69, 82, 95, 80, 82, 73, 78, 84, 80, 65, 
			71, 69, 83, 0, 106, 11, 67, 104, 101, 99, 107, 66, 111, 120, 95, 49, 
			0, 106, 8, 69, 110, 97, 98, 108, 101, 100, 0, 9, 12, 4, 25, 3, 
			100, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 335L ) ) break;
	hb_xvmSetLine( 345 );
	hb_xvmPushFuncSymbol( symbols + 52 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 316L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 317L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 332L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 331L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 320L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 334L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 322L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 323L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 366L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 367L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 324L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 335L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 327L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 326L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 321L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 329L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 279L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 330L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 328L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 312L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 313L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 315L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 314L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 303L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 342L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 304L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 358L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 359L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 372L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 399L ) ) break;
	if( hb_xvmDo( 30 ) ) break;
	hb_xvmSetLine( 347 );
	hb_xvmPushStringConst( "CheckBox_1", 10 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 316L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 317L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 406L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 318L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 334L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 324L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 326L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 335L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 327L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 338L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 329L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 330L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 328L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 312L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 313L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 314L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 315L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 358L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 359L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 308L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 364L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 404L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 285L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 360L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 399L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 405L ) ) break;
	hb_xvmSetLine( 348 );
	hb_xvmPushInteger( 132 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmSetLine( 349 );
	hb_xvmPushInteger( 295 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmSetLine( 350 );
	hb_xvmPushInteger( 120 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmSetLine( 351 );
	hb_xvmPushStringConst( "Arial", 5 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmSetLine( 352 );
	hb_xvmPushInteger( 8 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmSetLine( 353 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 15L ) ) break;
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00017;
	hb_xvmPushLogical( HB_TRUE );
	goto lab00018;
lab00017: ;
	hb_xvmPushLogical( HB_FALSE );
lab00018: ;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 334L ) ) break;
	hb_xvmSetLine( 354 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmArrayItemPush( 14L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 318L ) ) break;
	hb_xvmSetLine( 355 );
	hb_xvmPushFuncSymbol( symbols + 56 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 316L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 317L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 332L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 331L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 318L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 334L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 322L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 323L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 324L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 335L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 320L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 321L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 327L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 326L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 329L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 330L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 328L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 312L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 313L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 315L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 314L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 404L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 358L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 359L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 364L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 285L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 405L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 338L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 308L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 360L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 406L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 399L ) ) break;
	if( hb_xvmDo( 32 ) ) break;
	hb_xvmSetLine( 357 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 359 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PRINTPAGES", 23 );
	hb_xvmPushStringConst( "Center", 6 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 367 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushStringConst( "_HMG_PRINTER_GO_TO_PAGE", 23 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 195 );
	hb_xvmPushInteger( 90 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmArrayGen( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmArrayGen( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 58 ) ) break;
	hb_xvmSetLine( 369 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 27 );
	{
		static const HB_BYTE codeblock[ 188 ] = {
			176, 44, 0, 176, 45, 0, 106, 24, 95, 72, 77, 71, 95, 80, 82, 73, 
			78, 84, 69, 82, 95, 71, 79, 95, 84, 79, 95, 80, 65, 71, 69, 0, 
			12, 1, 20, 1, 176, 46, 0, 176, 45, 0, 106, 25, 95, 72, 77, 71, 
			95, 80, 82, 73, 78, 84, 69, 82, 95, 83, 72, 79, 87, 80, 82, 69, 
			86, 73, 69, 87, 0, 12, 1, 20, 1, 176, 46, 0, 176, 45, 0, 106, 
			28, 95, 72, 77, 71, 95, 80, 82, 73, 78, 84, 69, 82, 95, 83, 72, 
			79, 87, 84, 72, 85, 77, 66, 78, 65, 73, 76, 83, 0, 12, 1, 20, 
			1, 176, 46, 0, 176, 45, 0, 106, 19, 95, 72, 77, 71, 95, 80, 82, 
			73, 78, 84, 69, 82, 95, 80, 80, 78, 65, 86, 0, 12, 1, 20, 1, 
			176, 15, 0, 106, 25, 95, 72, 77, 71, 95, 80, 82, 73, 78, 84, 69, 
			82, 95, 83, 72, 79, 87, 80, 82, 69, 86, 73, 69, 87, 0, 106, 9, 
			115, 101, 116, 102, 111, 99, 117, 115, 0, 12, 2, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 370 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 13 );
	{
		static const HB_BYTE codeblock[ 250 ] = {
			176, 49, 0, 106, 24, 95, 72, 77, 71, 95, 80, 82, 73, 78, 84, 69, 
			82, 95, 71, 79, 95, 84, 79, 95, 80, 65, 71, 69, 0, 106, 10, 83, 
			112, 105, 110, 110, 101, 114, 95, 49, 0, 106, 6, 86, 97, 108, 117, 101, 
			0, 12, 3, 98, 6, 0, 92, 4, 2, 176, 44, 0, 176, 45, 0, 106, 
			24, 95, 72, 77, 71, 95, 80, 82, 73, 78, 84, 69, 82, 95, 71, 79, 
			95, 84, 79, 95, 80, 65, 71, 69, 0, 12, 1, 20, 1, 176, 46, 0, 
			176, 45, 0, 106, 25, 95, 72, 77, 71, 95, 80, 82, 73, 78, 84, 69, 
			82, 95, 83, 72, 79, 87, 80, 82, 69, 86, 73, 69, 87, 0, 12, 1, 
			20, 1, 176, 46, 0, 176, 45, 0, 106, 28, 95, 72, 77, 71, 95, 80, 
			82, 73, 78, 84, 69, 82, 95, 83, 72, 79, 87, 84, 72, 85, 77, 66, 
			78, 65, 73, 76, 83, 0, 12, 1, 20, 1, 176, 46, 0, 176, 45, 0, 
			106, 19, 95, 72, 77, 71, 95, 80, 82, 73, 78, 84, 69, 82, 95, 80, 
			80, 78, 65, 86, 0, 12, 1, 20, 1, 176, 19, 0, 20, 0, 176, 15, 
			0, 106, 25, 95, 72, 77, 71, 95, 80, 82, 73, 78, 84, 69, 82, 95, 
			83, 72, 79, 87, 80, 82, 69, 86, 73, 69, 87, 0, 106, 9, 115, 101, 
			116, 102, 111, 99, 117, 115, 0, 12, 2, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 372 );
	hb_xvmPushStringConst( "Label_1", 7 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 316L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 317L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 406L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 334L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 360L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 361L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 362L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 363L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 364L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 358L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 359L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 319L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 329L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 330L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 312L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 313L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 314L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 315L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 324L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 341L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 308L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 304L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 326L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 327L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 348L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 368L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 278L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 399L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 303L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 288L ) ) break;
	hb_xvmSetLine( 373 );
	hb_xvmPushInteger( 13 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmSetLine( 374 );
	hb_xvmPushInteger( 10 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmSetLine( 375 );
	hb_xvmPushInteger( 94 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmSetLine( 376 );
	hb_xvmPushInteger( 25 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmSetLine( 377 );
	hb_xvmPushStringConst( "Arial", 5 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmSetLine( 378 );
	hb_xvmPushInteger( 9 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmSetLine( 379 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmArrayItemPush( 10L ) ) break;
	hb_xvmPushStringConst( ":", 1 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 334L ) ) break;
	hb_xvmSetLine( 380 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 316L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 317L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 332L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 331L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 334L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 320L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 321L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 322L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 323L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 312L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 360L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 361L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 362L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 363L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 364L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 358L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 359L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 319L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 324L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 329L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 330L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 313L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 315L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 314L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 308L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 341L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 288L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 304L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 326L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 327L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 368L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 278L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 406L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 399L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 348L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 303L ) ) break;
	if( hb_xvmDo( 36 ) ) break;
	hb_xvmSetLine( 382 );
	hb_xvmPushStringConst( "Spinner_1", 9 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 316L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 317L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 334L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 366L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 367L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 324L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 335L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 327L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 326L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 329L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 312L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 313L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 314L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 315L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 358L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 359L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 372L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 279L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 303L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 342L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 304L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 330L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 399L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 328L ) ) break;
	hb_xvmSetLine( 383 );
	hb_xvmPushInteger( 10 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmSetLine( 384 );
	hb_xvmPushInteger( 105 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmSetLine( 385 );
	hb_xvmPushInteger( 75 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmSetLine( 386 );
	hb_xvmPushStringConst( "Arial", 5 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmSetLine( 387 );
	hb_xvmPushInteger( 9 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmSetLine( 388 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 334L ) ) break;
	hb_xvmSetLine( 389 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 366L ) ) break;
	hb_xvmSetLine( 390 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 18L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 367L ) ) break;
	hb_xvmSetLine( 391 );
	hb_xvmPushFuncSymbol( symbols + 52 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 316L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 317L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 332L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 331L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 320L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 334L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 322L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 323L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 366L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 367L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 324L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 335L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 327L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 326L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 321L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 329L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 279L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 330L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 328L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 312L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 313L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 315L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 314L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 303L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 342L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 304L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 358L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 359L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 372L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 399L ) ) break;
	if( hb_xvmDo( 30 ) ) break;
	hb_xvmSetLine( 393 );
	hb_xvmPushStringConst( "Ok", 2 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 316L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 317L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 406L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 318L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 319L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 324L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 325L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 326L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 327L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 328L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 329L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 330L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 333L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 286L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 351L ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 364L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 284L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 312L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 313L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 314L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 315L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 360L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 399L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 358L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 359L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 403L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 278L ) ) break;
	hb_xvmSetLine( 394 );
	hb_xvmPushInteger( 48 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmSetLine( 395 );
	hb_xvmPushInteger( 10 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmSetLine( 396 );
	hb_xvmPushInteger( 80 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmSetLine( 397 );
	hb_xvmPushInteger( 25 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmSetLine( 398 );
	hb_xvmPushStringConst( "Arial", 5 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmSetLine( 399 );
	hb_xvmPushInteger( 9 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmSetLine( 400 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmArrayItemPush( 11L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 318L ) ) break;
	hb_xvmSetLine( 401 );
	{
		static const HB_BYTE codeblock[ 250 ] = {
			176, 49, 0, 106, 24, 95, 72, 77, 71, 95, 80, 82, 73, 78, 84, 69, 
			82, 95, 71, 79, 95, 84, 79, 95, 80, 65, 71, 69, 0, 106, 10, 83, 
			112, 105, 110, 110, 101, 114, 95, 49, 0, 106, 6, 86, 97, 108, 117, 101, 
			0, 12, 3, 98, 6, 0, 92, 4, 2, 176, 44, 0, 176, 45, 0, 106, 
			24, 95, 72, 77, 71, 95, 80, 82, 73, 78, 84, 69, 82, 95, 71, 79, 
			95, 84, 79, 95, 80, 65, 71, 69, 0, 12, 1, 20, 1, 176, 46, 0, 
			176, 45, 0, 106, 25, 95, 72, 77, 71, 95, 80, 82, 73, 78, 84, 69, 
			82, 95, 83, 72, 79, 87, 80, 82, 69, 86, 73, 69, 87, 0, 12, 1, 
			20, 1, 176, 46, 0, 176, 45, 0, 106, 28, 95, 72, 77, 71, 95, 80, 
			82, 73, 78, 84, 69, 82, 95, 83, 72, 79, 87, 84, 72, 85, 77, 66, 
			78, 65, 73, 76, 83, 0, 12, 1, 20, 1, 176, 46, 0, 176, 45, 0, 
			106, 19, 95, 72, 77, 71, 95, 80, 82, 73, 78, 84, 69, 82, 95, 80, 
			80, 78, 65, 86, 0, 12, 1, 20, 1, 176, 19, 0, 20, 0, 176, 15, 
			0, 106, 25, 95, 72, 77, 71, 95, 80, 82, 73, 78, 84, 69, 82, 95, 
			83, 72, 79, 87, 80, 82, 69, 86, 73, 69, 87, 0, 106, 9, 115, 101, 
			116, 102, 111, 99, 117, 115, 0, 12, 2, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 319L ) ) break;
	hb_xvmSetLine( 402 );
	hb_xvmPushFuncSymbol( symbols + 54 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 316L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 317L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 332L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 331L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 318L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 319L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 320L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 321L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 333L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 324L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 326L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 327L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 325L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 364L ) ) break;
	if( hb_xvmNot() ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 329L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 330L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 328L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 278L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 286L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 322L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 323L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 312L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 313L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 315L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 314L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 368L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 280L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 281L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 358L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 359L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 283L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 284L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 352L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 287L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 339L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 282L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 372L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 279L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 399L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 403L ) ) break;
	if( hb_xvmDo( 40 ) ) break;
	hb_xvmSetLine( 404 );
	hb_xvmPushStringConst( "Cancel", 6 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 316L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 317L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 406L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 318L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 319L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 324L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 325L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 326L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 327L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 328L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 329L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 330L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 333L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 286L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 351L ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 364L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 284L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 312L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 313L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 314L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 315L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 360L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 399L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 358L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 359L ) ) break;
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 403L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 278L ) ) break;
	hb_xvmSetLine( 405 );
	hb_xvmPushInteger( 48 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 331L ) ) break;
	hb_xvmSetLine( 406 );
	hb_xvmPushInteger( 100 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 332L ) ) break;
	hb_xvmSetLine( 407 );
	hb_xvmPushInteger( 80 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 320L ) ) break;
	hb_xvmSetLine( 408 );
	hb_xvmPushInteger( 25 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 321L ) ) break;
	hb_xvmSetLine( 409 );
	hb_xvmPushStringConst( "Arial", 5 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 322L ) ) break;
	hb_xvmSetLine( 410 );
	hb_xvmPushInteger( 9 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 323L ) ) break;
	hb_xvmSetLine( 411 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmArrayItemPush( 12L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 318L ) ) break;
	hb_xvmSetLine( 412 );
	{
		static const HB_BYTE codeblock[ 188 ] = {
			176, 44, 0, 176, 45, 0, 106, 24, 95, 72, 77, 71, 95, 80, 82, 73, 
			78, 84, 69, 82, 95, 71, 79, 95, 84, 79, 95, 80, 65, 71, 69, 0, 
			12, 1, 20, 1, 176, 46, 0, 176, 45, 0, 106, 25, 95, 72, 77, 71, 
			95, 80, 82, 73, 78, 84, 69, 82, 95, 83, 72, 79, 87, 80, 82, 69, 
			86, 73, 69, 87, 0, 12, 1, 20, 1, 176, 46, 0, 176, 45, 0, 106, 
			28, 95, 72, 77, 71, 95, 80, 82, 73, 78, 84, 69, 82, 95, 83, 72, 
			79, 87, 84, 72, 85, 77, 66, 78, 65, 73, 76, 83, 0, 12, 1, 20, 
			1, 176, 46, 0, 176, 45, 0, 106, 19, 95, 72, 77, 71, 95, 80, 82, 
			73, 78, 84, 69, 82, 95, 80, 80, 78, 65, 86, 0, 12, 1, 20, 1, 
			176, 15, 0, 106, 25, 95, 72, 77, 71, 95, 80, 82, 73, 78, 84, 69, 
			82, 95, 83, 72, 79, 87, 80, 82, 69, 86, 73, 69, 87, 0, 106, 9, 
			115, 101, 116, 102, 111, 99, 117, 115, 0, 12, 2, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 319L ) ) break;
	hb_xvmSetLine( 413 );
	hb_xvmPushFuncSymbol( symbols + 54 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 316L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 317L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 332L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 331L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 318L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 319L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 320L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 321L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 333L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 324L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 326L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 327L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 325L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 364L ) ) break;
	if( hb_xvmNot() ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 329L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 330L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 328L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 278L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 286L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 322L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 323L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 312L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 313L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 315L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 314L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 368L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 280L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 281L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 358L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 359L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 283L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 284L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 352L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 287L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 339L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 282L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 372L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 279L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 399L ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 403L ) ) break;
	if( hb_xvmDo( 40 ) ) break;
	hb_xvmSetLine( 415 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 417 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushStringConst( "_HMG_PRINTER_GO_TO_PAGE", 23 );
	hb_xvmPushStringConst( "Center", 6 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 419 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 57 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00019;
	hb_xvmSetLine( 420 );
	hb_xvmPushDouble( * ( double * ) ")\\\x8F\xC2\xF5(\xDC\?", 10, 2 );
	hb_xvmPopLocal( 5 );
	goto lab00020;
lab00019: ;
	hb_xvmSetLine( 422 );
	hb_xvmPushDouble( * ( double * ) "\xA4" "p=\x0A\xD7\xA3\xD0\?", 10, 2 );
	hb_xvmPopLocal( 5 );
lab00020: ;
	hb_xvmSetLine( 425 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmMult() ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 427 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 429 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 18L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmAddInt( 10L ) ) break;
	if( hb_xvmMult() ) break;
	hb_xvmPushFuncSymbol( symbols + 59 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 42 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 60 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmMultByInt( 2L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmAddInt( 7L ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 431 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmAddInt( -66L ) ) break;
	if( hb_xvmLessEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00021;
	hb_xvmSetLine( 432 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 12L ) ) break;
	hb_xvmSetLine( 433 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmAddInt( -65L ) ) break;
	hb_xvmPopLocal( 6 );
	goto lab00022;
lab00021: ;
	hb_xvmSetLine( 435 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 12L ) ) break;
lab00022: ;
	hb_xvmSetLine( 451 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWTHUMBNAILS", 27 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmArrayItemPush( 28L ) ) break;
	hb_xvmPushInteger( 5 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 130 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmAddInt( -66L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushStringConst( "IsVistaThemed", 13 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00023;
	hb_xvmPushInteger( 25 );
	goto lab00024;
lab00023: ;
	hb_xvmPushInteger( 0 );
lab00024: ;
	if( hb_xvmMinus() ) break;
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmArrayGen( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmArrayGen( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushInteger( 128 );
	hb_xvmPushInteger( 128 );
	hb_xvmPushInteger( 128 );
	hb_xvmArrayGen( 3 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 131 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 58 ) ) break;
	hb_xvmSetLine( 453 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWTHUMBNAILS", 27 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 455 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 457 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 12L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00025;
	hb_xvmSetLine( 458 );
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWTHUMBNAILS", 27 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00025: ;
	hb_xvmSetLine( 461 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 100 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 5 ) ) break;
	hb_xvmSetLine( 462 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 100 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 5 ) ) break;
	hb_xvmSetLine( 464 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 50 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 465 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 50 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 467 );
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 469 );
	hb_xvmPushFuncSymbol( symbols + 64 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWTHUMBNAILS", 27 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 471 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PPNAV", 18 );
	hb_xvmPushStringConst( "Center", 6 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 473 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PPNAV", 18 );
	hb_xvmPushStringConst( "ROW", 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 475 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWTHUMBNAILS", 27 );
	hb_xvmPushStringConst( "ROW", 3 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 477 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PRINTPAGES", 23 );
	hb_xvmPushStringConst( "_HMG_PRINTER_GO_TO_PAGE", 23 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWTHUMBNAILS", 27 );
	hb_xvmPushStringConst( "_HMG_PRINTER_Wait", 17 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PPNAV", 18 );
	hb_xvmArrayGen( 5 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 479 );
	hb_xvmPushFuncSymbol( symbols + 66 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 172L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00026;
	hb_xvmPushFuncSymbol( symbols + 67 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 172L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 66 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 172L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 3 ) ) break;
lab00026: ;
	hb_xvmSetLine( 481 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 19L ) ) break;
	hb_xvmSetLine( 483 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00033;
	hb_xvmSetLine( 485 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 3 );
	goto lab00029;
lab00027: ;
	hb_xvmSetLine( 486 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 92L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00028;
	hb_xvmSetLine( 487 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 96L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushStringConst( "X", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00028;
	hb_xvmSetLine( 488 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00028;
	hb_xvmSetLine( 489 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00028: ;
	hb_xvmSetLine( 485 );
	if( hb_xvmLocalIncPush( 3 ) ) break;
lab00029: ;
	hb_xvmPushFuncSymbol( symbols + 66 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00027;
	hb_xvmSetLine( 495 );
	hb_xvmPushFuncSymbol( symbols + 46 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 497 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 3 );
	goto lab00032;
lab00030: ;
	hb_xvmSetLine( 498 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 92L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00031;
	hb_xvmSetLine( 499 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 96L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushStringConst( "P", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00031;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 97L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00031;
	hb_xvmSetLine( 500 );
	hb_xvmPushFuncSymbol( symbols + 46 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00031: ;
	hb_xvmSetLine( 497 );
	if( hb_xvmLocalIncPush( 3 ) ) break;
lab00032: ;
	hb_xvmPushFuncSymbol( symbols + 66 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 94L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00030;
	hb_xvmSetLine( 505 );
	hb_xvmPushFuncSymbol( symbols + 68 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 507 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 91L ) ) break;
	hb_xvmSetLine( 508 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 275L ) ) break;
lab00033: ;
	hb_xvmSetLine( 512 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPop( 184L ) ) break;
	hb_xvmSetLine( 514 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( _HMG_PRINTER_SETKEYS )
{
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 519 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 36 );
	{
		static const HB_BYTE codeblock[ 13 ] = {
			122, 98, 6, 0, 92, 4, 2, 176, 19, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 520 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 33 );
	{
		static const HB_BYTE codeblock[ 13 ] = {
			98, 6, 0, 92, 4, 148, 169, 176, 19, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 521 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 34 );
	{
		static const HB_BYTE codeblock[ 13 ] = {
			98, 6, 0, 92, 4, 148, 170, 176, 19, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 522 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 35 );
	{
		static const HB_BYTE codeblock[ 18 ] = {
			98, 6, 0, 92, 18, 1, 98, 6, 0, 92, 4, 2, 176, 19, 0, 12, 
			0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 523 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 2 );
	hb_xvmPushInteger( 80 );
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 27, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 524 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 2 );
	hb_xvmPushInteger( 71 );
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 24, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 525 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 27 );
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 30, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 526 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 106 );
	{
		static const HB_BYTE codeblock[ 85 ] = {
			176, 50, 0, 106, 19, 95, 72, 77, 71, 95, 80, 82, 73, 78, 84, 69, 
			82, 95, 80, 80, 78, 65, 86, 0, 106, 3, 98, 53, 0, 106, 6, 118, 
			97, 108, 117, 101, 0, 176, 49, 0, 106, 19, 95, 72, 77, 71, 95, 80, 
			82, 73, 78, 84, 69, 82, 95, 80, 80, 78, 65, 86, 0, 106, 3, 98, 
			53, 0, 106, 6, 118, 97, 108, 117, 101, 0, 12, 3, 68, 20, 4, 176, 
			69, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 527 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 2 );
	hb_xvmPushInteger( 67 );
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 30, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 528 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 115 );
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 30, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 529 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 2 );
	hb_xvmPushInteger( 83 );
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 28, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 530 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 2 );
	hb_xvmPushInteger( 69 );
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 29, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 531 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 2 );
	hb_xvmPushInteger( 84 );
	{
		static const HB_BYTE codeblock[ 6 ] = {
			176, 70, 0, 12, 0, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 533 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( CREATETHUMBNAILS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 7, 0 );
	hb_xvmSetLine( 545 );
	hb_xvmPushFuncSymbol( symbols + 55 );
	hb_xvmPushStringConst( "Image1", 6 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWTHUMBNAILS", 27 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 546 );
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 549 );
	hb_xvmPushFuncSymbol( symbols + 72 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_Wait", 17 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 551 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 57 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 552 );
	hb_xvmPushDouble( * ( double * ) ")\\\x8F\xC2\xF5(\xDC\?", 10, 2 );
	hb_xvmPopLocal( 1 );
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 554 );
	hb_xvmPushDouble( * ( double * ) "333333\xD3\?", 10, 2 );
	hb_xvmPopLocal( 1 );
lab00003: ;
	hb_xvmSetLine( 557 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmMult() ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 558 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmMult() ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 560 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 562 );
	hb_xvmPushFuncSymbol( symbols + 73 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWTHUMBNAILS", 27 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 564 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 5 );
	goto lab00005;
lab00004: ;
	hb_xvmSetLine( 566 );
	hb_xvmPushStringConst( "Image", 5 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmLocalAdd( 6 );
	hb_xvmSetLine( 568 );
	hb_xvmPushStringConst( "_HMG_MINIPRINT [4] := ", 22 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ", _HMG_MINIPRINT [11] := .F., _HMG_PRINTER_PREVIEWRefresh(), _HMG_MINIPRINT [11] := .T.", 87 );
	hb_xvmLocalAdd( 7 );
	hb_xvmSetLine( 583 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWTHUMBNAILS", 27 );
	hb_xvmPushInteger( 10 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmAddInt( 10L ) ) break;
	if( hb_xvmMult() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 75 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ".emf", 4 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushStringConst( "{||", 3 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "}", 1 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmMacroPush( 43 ) ) break;
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 12 ) ) break;
	hb_xvmSetLine( 585 );
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushFuncSymbol( symbols + 77 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWTHUMBNAILS", 27 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushStringConst( " ", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " [Click]", 8 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 564 );
	if( hb_xvmLocalIncPush( 5 ) ) break;
lab00005: ;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 18L ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 589 );
	hb_xvmPushFuncSymbol( symbols + 44 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_Wait", 17 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 591 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( _HMG_PRINTER_THUMBNAILTOGGLE )
{
   do {
	hb_xvmSetLine( 596 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PPNAV", 18 );
	hb_xvmPushStringConst( "thumbswitch", 11 );
	hb_xvmPushStringConst( "Value", 5 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PPNAV", 18 );
	hb_xvmPushStringConst( "thumbswitch", 11 );
	hb_xvmPushStringConst( "Value", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmNot() ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 598 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 600 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _HMG_PRINTER_PROCESSTHUMBNAILS )
{
   HB_BOOL fValue;
   do {
	hb_xvmSetLine( 605 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PPNAV", 18 );
	hb_xvmPushStringConst( "thumbswitch", 11 );
	hb_xvmPushStringConst( "Value", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 607 );
	hb_xvmPushFuncSymbol( symbols + 71 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 609 );
	hb_xvmPushInteger( 90 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 10L ) ) break;
	hb_xvmSetLine( 611 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 11 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDivide() ) break;
	hb_xvmPushDouble( * ( double * ) "\x8F\xC2\xF5(\\\x8F\xE2\?", 10, 2 );
	if( hb_xvmMult() ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 5L ) ) break;
	hb_xvmSetLine( 613 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PPNAV", 18 );
	hb_xvmPushStringConst( "Width", 5 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmAddInt( -148L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushStringConst( "IsVistaThemed", 13 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushInteger( 30 );
	goto lab00002;
lab00001: ;
	hb_xvmPushInteger( 0 );
lab00002: ;
	if( hb_xvmMinus() ) break;
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 615 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PPNAV", 18 );
	hb_xvmPushStringConst( "Col", 3 );
	hb_xvmPushInteger( 138 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushStringConst( "IsVistaThemed", 13 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushInteger( 20 );
	goto lab00004;
lab00003: ;
	hb_xvmPushInteger( 0 );
lab00004: ;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 617 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 619 );
	hb_xvmPushFuncSymbol( symbols + 72 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWTHUMBNAILS", 27 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	goto lab00006;
lab00005: ;
	hb_xvmSetLine( 623 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 10L ) ) break;
	hb_xvmSetLine( 625 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 11 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDivide() ) break;
	hb_xvmPushDouble( * ( double * ) ")\\\x8F\xC2\xF5(\xE4\?", 10, 2 );
	if( hb_xvmMult() ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 5L ) ) break;
	hb_xvmSetLine( 627 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PPNAV", 18 );
	hb_xvmPushStringConst( "Width", 5 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmAddInt( -103L ) ) break;
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 629 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PPNAV", 18 );
	hb_xvmPushStringConst( "Col", 3 );
	hb_xvmPushInteger( 51 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 631 );
	hb_xvmPushFuncSymbol( symbols + 44 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWTHUMBNAILS", 27 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 633 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	hb_xvmPushStringConst( "SetFocus", 8 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 635 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	if( hb_xvmDo( 0 ) ) break;
lab00006: ;
	hb_xvmSetLine( 639 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _HMG_PRINTER_SAVE_PDF_PAGES )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 3 );
	hb_xvmSetLine( 645 );
	hb_xvmPushFuncSymbol( symbols + 78 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 646 );
	hb_xvmPushFuncSymbol( symbols + 79 );
	hb_xvmPushStringConst( "PDF Files", 9 );
	hb_xvmPushStringConst( "*.pdf", 5 );
	hb_xvmArrayGen( 2 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushNil();
	hb_xvmPushFuncSymbol( symbols + 80 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 21L ) ) break;
	if( hb_xvmFunction( 5 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 647 );
	hb_xvmPushFuncSymbol( symbols + 78 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 648 );
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 652 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 653 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PPNAV", 18 );
	hb_xvmPushStringConst( "b7", 2 );
	hb_xvmPushStringConst( "Enabled", 7 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 654 );
	hb_xvmPushFuncSymbol( symbols + 81 );
	if( hb_xvmDo( 0 ) ) break;
lab00002: ;
	hb_xvmSetLine( 657 );
	hb_xvmPushFuncSymbol( symbols + 82 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 658 );
	hb_xvmPushFuncSymbol( symbols + 83 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 17L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_hmg_print_preview_*.Emf", 24 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 660 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 661 );
	hb_xvmPushFuncSymbol( symbols + 84 );
	hb_xvmPushLocal( 5 );
	{
		static const HB_BYTE codeblock[ 23 ] = {
			1, 0, 2, 0, 6, 0, 4, 0, 176, 85, 0, 95, 255, 95, 254, 95, 
			1, 122, 1, 72, 12, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 663 );
	hb_xvmPushFuncSymbol( symbols + 86 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( ".pdf", 4 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 664 );
	hb_xvmPushFuncSymbol( symbols + 87 );
	hb_xvmPushFuncSymbol( symbols + 88 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 666 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PPNAV", 18 );
	hb_xvmPushStringConst( "b7", 2 );
	hb_xvmPushStringConst( "Enabled", 7 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 667 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	hb_xvmPushStringConst( "SetFocus", 8 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 669 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _HMG_PRINTER_SEND_MAIL )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 16, 0 );
	hb_xvmSetLine( 676 );
	hb_xvmPushFuncSymbol( symbols + 89 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 5 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "mail.cfg", 8 );
	hb_xvmLocalAdd( 15 );
	hb_xvmSetLine( 677 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 679 );
	hb_xvmPushFuncSymbol( symbols + 90 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 680 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PPNAV", 18 );
	hb_xvmPushStringConst( "b8", 2 );
	hb_xvmPushStringConst( "Enabled", 7 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 681 );
	hb_xvmPushFuncSymbol( symbols + 81 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 682 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 21L ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 684 );
	hb_xvmPushFuncSymbol( symbols + 91 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 92 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushStringConst( " ", 1 );
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 93 );
	hb_xvmPushFuncSymbol( symbols + 94 );
	hb_xvmPushFuncSymbol( symbols + 95 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 14 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ".pdf", 4 );
	hb_xvmLocalAdd( 3 );
	hb_xvmSetLine( 686 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLocal( 2 );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 688 );
	hb_xvmPushFuncSymbol( symbols + 90 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 689 );
	hb_xvmPushFuncSymbol( symbols + 96 );
	hb_xvmPushStringConst( "PDF not saved to send Email", 27 );
	if( hb_xvmDo( 1 ) ) break;
	goto lab00006;
lab00001: ;
	hb_xvmSetLine( 691 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 692 );
	hb_xvmLocalSetInt( 6, 0L );
	hb_xvmSetLine( 693 );
	hb_xvmPushFuncSymbol( symbols + 97 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 694 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 695 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 696 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 697 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 698 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 699 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 700 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 701 );
	hb_xvmLocalSetInt( 14, 1L );
	hb_xvmSetLine( 703 );
	hb_xvmPushFuncSymbol( symbols + 98 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 704 );
	hb_xvmPushFuncSymbol( symbols + 99 );
	hb_xvmPushStringConst( "Mail", 4 );
	hb_xvmPushStringConst( "Server", 6 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 4 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 705 );
	hb_xvmPushFuncSymbol( symbols + 99 );
	hb_xvmPushStringConst( "Mail", 4 );
	hb_xvmPushStringConst( "Port", 4 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 4 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 706 );
	hb_xvmPushFuncSymbol( symbols + 99 );
	hb_xvmPushStringConst( "Mail", 4 );
	hb_xvmPushStringConst( "UserName", 8 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 4 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 707 );
	hb_xvmPushFuncSymbol( symbols + 99 );
	hb_xvmPushStringConst( "Mail", 4 );
	hb_xvmPushStringConst( "From", 4 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 4 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 708 );
	hb_xvmPushFuncSymbol( symbols + 99 );
	hb_xvmPushStringConst( "Mail", 4 );
	hb_xvmPushStringConst( "PassWord", 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 4 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 709 );
	hb_xvmPushFuncSymbol( symbols + 99 );
	hb_xvmPushStringConst( "Mail", 4 );
	hb_xvmPushStringConst( "Recipient1", 10 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 4 ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 710 );
	hb_xvmPushFuncSymbol( symbols + 99 );
	hb_xvmPushStringConst( "Mail", 4 );
	hb_xvmPushStringConst( "Recipient2", 10 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 4 ) ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 711 );
	hb_xvmPushFuncSymbol( symbols + 99 );
	hb_xvmPushStringConst( "Mail", 4 );
	hb_xvmPushStringConst( "Copy", 4 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmFunction( 4 ) ) break;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 712 );
	hb_xvmPushFuncSymbol( symbols + 99 );
	hb_xvmPushStringConst( "Mail", 4 );
	hb_xvmPushStringConst( "TextBody", 8 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 4 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 713 );
	hb_xvmPushFuncSymbol( symbols + 99 );
	hb_xvmPushStringConst( "Mail", 4 );
	hb_xvmPushStringConst( "Receipt", 7 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmFunction( 4 ) ) break;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 714 );
	hb_xvmPushFuncSymbol( symbols + 99 );
	hb_xvmPushStringConst( "Mail", 4 );
	hb_xvmPushStringConst( "Priority", 8 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmFunction( 4 ) ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 715 );
	hb_xvmPushFuncSymbol( symbols + 100 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 717 );
	hb_xvmPushFuncSymbol( symbols + 78 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmSetLine( 718 );
	hb_xvmPushFuncSymbol( symbols + 85 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushNil();
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
lab00002: ;
	hb_xvmSetLine( 720 );
	hb_xvmPushFuncSymbol( symbols + 78 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmSetLine( 721 );
	hb_xvmPushFuncSymbol( symbols + 85 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushNil();
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
lab00003: ;
	hb_xvmSetLine( 727 );
	hb_xvmPushSymbol( symbols + 101 );
	hb_xvmPushFuncSymbol( symbols + 102 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmArrayGen( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmArrayGen( 1 );
	hb_xvmArrayGen( 1 );
	if( hb_xvmSend( 11 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 729 );
	hb_xvmPushLocal( 1 );
	hb_xvmWithObjectStart();
	hb_xvmSetLine( 730 );
	hb_xvmWithObjectMessage( symbols + 103 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 731 );
	hb_xvmPushFuncSymbol( symbols + 78 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00004;
	hb_xvmSetLine( 732 );
	hb_xvmWithObjectMessage( symbols + 104 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00004: ;
	hb_xvmSetLine( 734 );
	hb_xvmWithObjectMessage( symbols + 105 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 735 );
	hb_xvmWithObjectMessage( symbols + 106 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 736 );
	hb_xvmWithObjectMessage( symbols + 107 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	hb_xvmArrayGen( 2 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 737 );
	hb_xvmWithObjectMessage( symbols + 108 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 738 );
	hb_xvmPushFuncSymbol( symbols + 78 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmSetLine( 739 );
	hb_xvmWithObjectMessage( symbols + 109 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
lab00005: ;
	hb_xvmSetLine( 741 );
	hb_xvmWithObjectMessage( symbols + 110 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 742 );
	hb_xvmWithObjectMessage( symbols + 111 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmSend( 1 ) ) break;
	hb_stackPop();
	hb_xvmWithObjectEnd();
	hb_xvmSetLine( 745 );
	hb_xvmPushSymbol( symbols + 112 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 747 );
	hb_xvmPushSymbol( symbols + 113 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmSend( 0 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmPushFuncSymbol( symbols + 96 );
	hb_xvmPushStringConst( "Mail sent", 9 );
	if( hb_xvmDo( 1 ) ) break;
lab00006: ;
	hb_xvmSetLine( 750 );
	hb_xvmPushFuncSymbol( symbols + 90 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 751 );
	hb_xvmPushFuncSymbol( symbols + 114 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 1 ) ) break;
lab00007: ;
	hb_xvmSetLine( 754 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PPNAV", 18 );
	hb_xvmPushStringConst( "b8", 2 );
	hb_xvmPushStringConst( "Enabled", 7 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
lab00008: ;
	hb_xvmSetLine( 757 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	hb_xvmPushStringConst( "SetFocus", 8 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 759 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _HMG_PRINTER_GO_TO_PAGE )
{
   do {
	hb_xvmSetLine( 764 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PPNAV", 18 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 766 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWTHUMBNAILS", 27 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 768 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 770 );
	hb_xvmPushFuncSymbol( symbols + 72 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_GO_TO_PAGE", 23 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 772 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( _HMG_PRINTER_HSCROLLBOXPROCESS )
{
   do {
	hb_xvmFrame( 1, 0 );
	hb_xvmSetLine( 778 );
	hb_xvmPushFuncSymbol( symbols + 115 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 780 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmAddInt( -50L ) ) break;
	if( hb_xvmNegate() ) break;
	if( hb_xvmMultByInt( 10L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 6L ) ) break;
	hb_xvmSetLine( 782 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 784 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( _HMG_PRINTER_VSCROLLBOXPROCESS )
{
   do {
	hb_xvmFrame( 1, 0 );
	hb_xvmSetLine( 790 );
	hb_xvmPushFuncSymbol( symbols + 115 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 792 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmAddInt( -50L ) ) break;
	if( hb_xvmNegate() ) break;
	if( hb_xvmMultByInt( 10L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 7L ) ) break;
	hb_xvmSetLine( 794 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 796 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _HMG_PRINTER_PREVIEWCLOSE )
{
   do {
	hb_xvmSetLine( 801 );
	hb_xvmPushFuncSymbol( symbols + 116 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 803 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushStringConst( "_HMG_PRINTER_Wait", 17 );
	hb_xvmPushStringConst( "Release", 7 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 804 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWTHUMBNAILS", 27 );
	hb_xvmPushStringConst( "Release", 7 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 805 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushStringConst( "_HMG_PRINTER_GO_TO_PAGE", 23 );
	hb_xvmPushStringConst( "Release", 7 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 806 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PRINTPAGES", 23 );
	hb_xvmPushStringConst( "Release", 7 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 807 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PPNAV", 18 );
	hb_xvmPushStringConst( "Release", 7 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 809 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( _HMG_PRINTER_SPLTCHLDMOUSECURSOR )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 0 );
	hb_xvmSetLine( 813 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushStringConst( "aCoords", 7 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 815 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushStringConst( "_HMG_PRINTER_lFlag", 18 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 816 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushStringConst( "_HMG_PRINTER_lFlag", 18 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
lab00001: ;
	hb_xvmSetLine( 819 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 118 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "A", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 820 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 821 );
	hb_xvmPushFuncSymbol( symbols + 119 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 823 );
	hb_xvmPushFuncSymbol( symbols + 120 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 825 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushStringConst( "_HMG_PRINTER_lFlag", 18 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmSetLine( 826 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushStringConst( "_HMG_PRINTER_lFlag", 18 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 827 );
	hb_xvmPushFuncSymbol( symbols + 121 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "HP_GLASS", 8 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 828 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushStringConst( "_HMG_PRINTER_lFlag", 18 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 829 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushStringConst( "_HMG_PRINTER_lFlag", 18 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 830 );
	hb_xvmPushFuncSymbol( symbols + 121 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 32512 );
	if( hb_xvmDo( 2 ) ) break;
lab00003: ;
	hb_xvmSetLine( 834 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushStringConst( "_HMG_PRINTER_lFlag", 18 );
	if( hb_xvmDo( 1 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _HMG_PRINTER_SPLTCHLDMOUSECLICK )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 4 );
	hb_xvmSetLine( 838 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 839 );
	hb_xvmPushFuncSymbol( symbols + 117 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 844 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmEqualIntIs( 32L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 845 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushFuncSymbol( symbols + 77 );
	hb_xvmPushStringConst( "TOOLBAR_1", 9 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PPNAV", 18 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 846 );
	hb_xvmLocalSetInt( 5, 0L );
	hb_xvmSetLine( 847 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PPNAV", 18 );
	hb_xvmPushStringConst( "SetFocus", 8 );
	if( hb_xvmDo( 2 ) ) break;
lab00001: ;
	hb_xvmSetLine( 852 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmEqualIntIs( 513L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmPushLocal( 6 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 853 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 854 );
	hb_xvmLocalSetInt( 5, 0L );
	hb_xvmSetLine( 855 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PPNAV", 18 );
	hb_xvmPushStringConst( "b5", 2 );
	hb_xvmPushStringConst( "Value", 5 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PPNAV", 18 );
	hb_xvmPushStringConst( "b5", 2 );
	hb_xvmPushStringConst( "Value", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmNot() ) break;
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 856 );
	hb_xvmPushFuncSymbol( symbols + 69 );
	if( hb_xvmDo( 0 ) ) break;
lab00002: ;
	hb_xvmSetLine( 860 );
	hb_xvmPushLocal( 5 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( _HMG_PRINTER_CLEANPREVIEW )
{
   do {
	hb_xvmFrame( 1, 0 );
	hb_xvmSetLine( 864 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 5 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmLocalAdd( 1 );
	hb_xvmSetLine( 867 );
	hb_xvmPushFuncSymbol( symbols + 84 );
	hb_xvmPushFuncSymbol( symbols + 123 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 17L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_hmg_print_preview_*.Emf", 24 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	{
		static const HB_BYTE codeblock[ 19 ] = {
			1, 0, 1, 0, 1, 0, 176, 124, 0, 95, 255, 95, 1, 122, 1, 72, 
			12, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 869 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _HMG_PRINTER_PREVIEWREFRESH )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 0 );
	hb_xvmSetLine( 877 );
	hb_xvmPushFuncSymbol( symbols + 55 );
	hb_xvmPushStringConst( "Image", 5 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWTHUMBNAILS", 27 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 11L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 12L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 879 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 13L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 881 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 13L ) ) break;
	hb_xvmSetLine( 882 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWTHUMBNAILS", 27 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 883 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWTHUMBNAILS", 27 );
	hb_xvmPushStringConst( "Image", 5 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "Row", 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 884 );
	hb_xvmPushFuncSymbol( symbols + 125 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 886 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 18L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 888 );
	hb_xvmPushFuncSymbol( symbols + 115 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 889 );
	hb_xvmPushFuncSymbol( symbols + 126 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00004;
lab00001: ;
	hb_xvmSetLine( 892 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 894 );
	hb_xvmPushFuncSymbol( symbols + 115 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmSetLine( 895 );
	hb_xvmPushFuncSymbol( symbols + 126 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00004;
lab00002: ;
	hb_xvmSetLine( 900 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmAddInt( -9L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmLess() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 901 );
	hb_xvmPushFuncSymbol( symbols + 126 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmAddInt( -9L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	goto lab00004;
lab00003: ;
	hb_xvmSetLine( 903 );
	hb_xvmPushFuncSymbol( symbols + 115 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 904 );
	hb_xvmPushFuncSymbol( symbols + 126 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 2 ) ) break;
lab00004: ;
	hb_xvmSetLine( 914 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmLessThenIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmSetLine( 915 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 4L ) ) break;
	hb_xvmSetLine( 916 );
	hb_xvmPushFuncSymbol( symbols + 127 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 917 );
	/* *** END PROC *** */
	break;
lab00005: ;
	hb_xvmSetLine( 920 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 18L ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 921 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 18L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 4L ) ) break;
	hb_xvmSetLine( 922 );
	hb_xvmPushFuncSymbol( symbols + 127 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 923 );
	/* *** END PROC *** */
	break;
lab00006: ;
	hb_xvmSetLine( 926 );
	hb_xvmPushFuncSymbol( symbols + 128 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 929 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	hb_xvmPushStringConst( "aCoords", 7 );
	hb_xvmPushFuncSymbol( symbols + 129 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 75 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ".emf", 4 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 5L ) ) break;
	if( hb_xvmMultByInt( 10000L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 8L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 6L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmFunction( 7 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 931 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PPNAV", 18 );
	hb_xvmPushStringConst( "TITLE", 5 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushStringConst( " [", 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 16 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "/", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 16 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 18L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "]", 1 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 933 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _HMG_PRINTER_PRINTPAGES )
{
   HB_BOOL fValue;
   do {
	hb_xvmSetLine( 938 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PPNAV", 18 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 939 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWTHUMBNAILS", 27 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 940 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 942 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PRINTPAGES", 23 );
	hb_xvmPushStringConst( "Radio_1", 7 );
	hb_xvmPushStringConst( "Value", 5 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 944 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PRINTPAGES", 23 );
	hb_xvmPushStringConst( "Label_1", 7 );
	hb_xvmPushStringConst( "Enabled", 7 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 945 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PRINTPAGES", 23 );
	hb_xvmPushStringConst( "Label_2", 7 );
	hb_xvmPushStringConst( "Enabled", 7 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 946 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PRINTPAGES", 23 );
	hb_xvmPushStringConst( "Label_4", 7 );
	hb_xvmPushStringConst( "Enabled", 7 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 947 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PRINTPAGES", 23 );
	hb_xvmPushStringConst( "Spinner_1", 9 );
	hb_xvmPushStringConst( "Enabled", 7 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 948 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PRINTPAGES", 23 );
	hb_xvmPushStringConst( "Spinner_2", 9 );
	hb_xvmPushStringConst( "Enabled", 7 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 949 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PRINTPAGES", 23 );
	hb_xvmPushStringConst( "Combo_1", 7 );
	hb_xvmPushStringConst( "Enabled", 7 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 950 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PRINTPAGES", 23 );
	hb_xvmPushStringConst( "CheckBox_1", 10 );
	hb_xvmPushStringConst( "Enabled", 7 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 954 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 24L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 25L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
lab00001: ;
	hb_xvmSetLine( 956 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PRINTPAGES", 23 );
	hb_xvmPushStringConst( "Spinner_3", 9 );
	hb_xvmPushStringConst( "Enabled", 7 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 4 ) ) break;
lab00002: ;
	hb_xvmSetLine( 960 );
	hb_xvmPushFuncSymbol( symbols + 72 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PRINTPAGES", 23 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 962 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _HMG_PRINTER_PRINTPAGESDO )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 6, 0 );
	hb_xvmSetLine( 970 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 971 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 973 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PRINTPAGES", 23 );
	hb_xvmPushStringConst( "Radio_1", 7 );
	hb_xvmPushStringConst( "Value", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 975 );
	hb_xvmLocalSetInt( 2, 1L );
	hb_xvmSetLine( 976 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 18L ) ) break;
	hb_xvmPopLocal( 3 );
	goto lab00003;
lab00001: ;
	hb_xvmSetLine( 978 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PRINTPAGES", 23 );
	hb_xvmPushStringConst( "Radio_1", 7 );
	hb_xvmPushStringConst( "Value", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmEqualIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 980 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PRINTPAGES", 23 );
	hb_xvmPushStringConst( "Spinner_1", 9 );
	hb_xvmPushStringConst( "Value", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 981 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PRINTPAGES", 23 );
	hb_xvmPushStringConst( "Spinner_2", 9 );
	hb_xvmPushStringConst( "Value", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 983 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PRINTPAGES", 23 );
	hb_xvmPushStringConst( "Combo_1", 7 );
	hb_xvmPushStringConst( "Value", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmEqualIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 984 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 5 );
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 985 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PRINTPAGES", 23 );
	hb_xvmPushStringConst( "Combo_1", 7 );
	hb_xvmPushStringConst( "Value", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmEqualIntIs( 3L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 986 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 6 );
lab00003: ;
	hb_xvmSetLine( 991 );
	hb_xvmPushFuncSymbol( symbols + 130 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 21L ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 26L ) ) break;
	hb_xvmSetLine( 993 );
	hb_xvmPushFuncSymbol( symbols + 78 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 27L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00005;
	hb_xvmSetLine( 994 );
	hb_xvmPushFuncSymbol( symbols + 131 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 27L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 995 );
	hb_xvmPushFuncSymbol( symbols + 132 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 27L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 26L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmArrayGen( 2 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00005;
lab00004: ;
	hb_xvmSetLine( 997 );
	hb_xvmPushFuncSymbol( symbols + 133 );
	hb_xvmPushStringConst( "START PRINTDOC STOREJOBDATA: ", 29 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 27L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " must be declared as Public or Private.", 39 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00005: ;
	hb_xvmSetLine( 1001 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PRINTPAGES", 23 );
	hb_xvmPushStringConst( "Spinner_3", 9 );
	hb_xvmPushStringConst( "Value", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00011;
	hb_xvmSetLine( 1003 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 1 );
	goto lab00010;
lab00006: ;
	hb_xvmSetLine( 1005 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 1006 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDivideByInt( 2L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDivideByInt( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 1007 );
	hb_xvmPushFuncSymbol( symbols + 134 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 75 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ".emf", 4 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	goto lab00009;
lab00007: ;
	hb_xvmSetLine( 1009 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 1010 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDivideByInt( 2L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDivideByInt( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 1011 );
	hb_xvmPushFuncSymbol( symbols + 134 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 75 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ".emf", 4 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	goto lab00009;
lab00008: ;
	hb_xvmSetLine( 1014 );
	hb_xvmPushFuncSymbol( symbols + 134 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 75 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ".emf", 4 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00009: ;
	hb_xvmSetLine( 1003 );
	if( hb_xvmLocalIncPush( 1 ) ) break;
lab00010: ;
	hb_xvmPushLocal( 3 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	goto lab00027;
lab00011: ;
	hb_xvmSetLine( 1021 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PRINTPAGES", 23 );
	hb_xvmPushStringConst( "CheckBox_1", 10 );
	hb_xvmPushStringConst( "Value", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00019;
	hb_xvmSetLine( 1023 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 4 );
	goto lab00018;
lab00012: ;
	hb_xvmSetLine( 1025 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 1 );
	goto lab00017;
lab00013: ;
	hb_xvmSetLine( 1027 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
	hb_xvmSetLine( 1028 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDivideByInt( 2L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDivideByInt( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00016;
	hb_xvmSetLine( 1029 );
	hb_xvmPushFuncSymbol( symbols + 134 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 75 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ".emf", 4 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	goto lab00016;
lab00014: ;
	hb_xvmSetLine( 1031 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmSetLine( 1032 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDivideByInt( 2L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDivideByInt( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00016;
	hb_xvmSetLine( 1033 );
	hb_xvmPushFuncSymbol( symbols + 134 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 75 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ".emf", 4 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	goto lab00016;
lab00015: ;
	hb_xvmSetLine( 1036 );
	hb_xvmPushFuncSymbol( symbols + 134 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 75 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ".emf", 4 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00016: ;
	hb_xvmSetLine( 1025 );
	if( hb_xvmLocalIncPush( 1 ) ) break;
lab00017: ;
	hb_xvmPushLocal( 3 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmSetLine( 1023 );
	if( hb_xvmLocalIncPush( 4 ) ) break;
lab00018: ;
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PRINTPAGES", 23 );
	hb_xvmPushStringConst( "Spinner_3", 9 );
	hb_xvmPushStringConst( "Value", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	goto lab00027;
lab00019: ;
	hb_xvmSetLine( 1045 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 1 );
	goto lab00026;
lab00020: ;
	hb_xvmSetLine( 1047 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 4 );
	goto lab00025;
lab00021: ;
	hb_xvmSetLine( 1049 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00022;
	hb_xvmSetLine( 1050 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDivideByInt( 2L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDivideByInt( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00024;
	hb_xvmSetLine( 1051 );
	hb_xvmPushFuncSymbol( symbols + 134 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 75 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ".emf", 4 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	goto lab00024;
lab00022: ;
	hb_xvmSetLine( 1053 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00023;
	hb_xvmSetLine( 1054 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDivideByInt( 2L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDivideByInt( 2L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00024;
	hb_xvmSetLine( 1055 );
	hb_xvmPushFuncSymbol( symbols + 134 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 75 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ".emf", 4 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
	goto lab00024;
lab00023: ;
	hb_xvmSetLine( 1058 );
	hb_xvmPushFuncSymbol( symbols + 134 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 75 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( ".emf", 4 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00024: ;
	hb_xvmSetLine( 1047 );
	if( hb_xvmLocalIncPush( 4 ) ) break;
lab00025: ;
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PRINTPAGES", 23 );
	hb_xvmPushStringConst( "Spinner_3", 9 );
	hb_xvmPushStringConst( "Value", 5 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00021;
	hb_xvmSetLine( 1045 );
	if( hb_xvmLocalIncPush( 1 ) ) break;
lab00026: ;
	hb_xvmPushLocal( 3 );
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00020;
lab00027: ;
	hb_xvmSetLine( 1069 );
	hb_xvmPushFuncSymbol( symbols + 135 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1071 );
	hb_xvmPushFuncSymbol( symbols + 46 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1072 );
	hb_xvmPushFuncSymbol( symbols + 46 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWTHUMBNAILS", 27 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1073 );
	hb_xvmPushFuncSymbol( symbols + 46 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PPNAV", 18 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1075 );
	hb_xvmPushFuncSymbol( symbols + 44 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_PRINTPAGES", 23 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	hb_xvmSetLine( 1077 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	hb_xvmPushStringConst( "setfocus", 8 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1079 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( _HMG_PRINTER_SCROLLLEFT )
{
   HB_BOOL fValue;
   do {
	hb_xvmSetLine( 1083 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 6L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 9L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 6L ) ) break;
	hb_xvmSetLine( 1084 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 6L ) ) break;
	if( hb_xvmGreaterEqualThenIntIs( 500L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 1085 );
	hb_xvmPushInteger( 500 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 6L ) ) break;
	hb_xvmSetLine( 1086 );
	hb_xvmPushFuncSymbol( symbols + 127 );
	if( hb_xvmDo( 0 ) ) break;
lab00001: ;
	hb_xvmSetLine( 1088 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 1089 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( _HMG_PRINTER_SCROLLRIGHT )
{
   HB_BOOL fValue;
   do {
	hb_xvmSetLine( 1093 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 6L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 9L ) ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 6L ) ) break;
	hb_xvmSetLine( 1094 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 6L ) ) break;
	if( hb_xvmLessEqualThenIntIs( -500L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 1095 );
	hb_xvmPushInteger( -500 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 6L ) ) break;
	hb_xvmSetLine( 1096 );
	hb_xvmPushFuncSymbol( symbols + 127 );
	if( hb_xvmDo( 0 ) ) break;
lab00001: ;
	hb_xvmSetLine( 1098 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 1099 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( _HMG_PRINTER_SCROLLUP )
{
   HB_BOOL fValue;
   do {
	hb_xvmSetLine( 1103 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 9L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 7L ) ) break;
	hb_xvmSetLine( 1104 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmGreaterEqualThenIntIs( 500L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 1105 );
	hb_xvmPushInteger( 500 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 7L ) ) break;
	hb_xvmSetLine( 1106 );
	hb_xvmPushFuncSymbol( symbols + 127 );
	if( hb_xvmDo( 0 ) ) break;
lab00001: ;
	hb_xvmSetLine( 1108 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 1109 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC_STATIC( _HMG_PRINTER_SCROLLDOWN )
{
   HB_BOOL fValue;
   do {
	hb_xvmSetLine( 1113 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 9L ) ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 7L ) ) break;
	hb_xvmSetLine( 1114 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmLessEqualThenIntIs( -500L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 1115 );
	hb_xvmPushInteger( -500 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 7L ) ) break;
	hb_xvmSetLine( 1116 );
	hb_xvmPushFuncSymbol( symbols + 127 );
	if( hb_xvmDo( 0 ) ) break;
lab00001: ;
	hb_xvmSetLine( 1119 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 1120 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( GETPRINTER )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 5, 0 );
	hb_xvmSetLine( 1124 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 1125 );
	hb_xvmPushFuncSymbol( symbols + 88 );
	hb_xvmPushFuncSymbol( symbols + 137 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 1126 );
	hb_xvmPushFuncSymbol( symbols + 138 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 1128 );
	hb_xvmLocalSetInt( 5, 0L );
	hb_xvmSetLine( 1130 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 4 );
	goto lab00003;
lab00001: ;
	hb_xvmSetLine( 1132 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 1133 );
	hb_xvmCopyLocals( 4, 5 );
	goto lab00004;
lab00002: ;
	hb_xvmSetLine( 1130 );
	if( hb_xvmLocalIncPush( 4 ) ) break;
lab00003: ;
	hb_xvmPushFuncSymbol( symbols + 66 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
lab00004: ;
	hb_xvmSetLine( 1139 );
	hb_xvmPushFuncSymbol( symbols + 139 );
	hb_xvmPushStringConst( "_HMG_MINIPRINT[22]", 18 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 1140 );
	hb_xvmPushFuncSymbol( symbols + 140 );
	if( hb_xvmDo( 0 ) ) break;
lab00005: ;
	hb_xvmSetLine( 1149 );
	hb_xvmPushFuncSymbol( symbols + 141 );
	hb_xvmPushStringConst( "_HMG_PRINTER_GETPRINTER", 23 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmArrayItemPush( 13L ) ) break;
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 345 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmAddInt( 100L ) ) break;
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmArrayGen( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmArrayGen( 2 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 43 ) ) break;
	hb_xvmSetLine( 1151 );
	hb_xvmPushFuncSymbol( symbols + 53 );
	hb_xvmPushStringConst( "Combo_1", 7 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 10 );
	hb_xvmPushInteger( 15 );
	hb_xvmPushInteger( 320 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 43 ) ) break;
	hb_xvmSetLine( 1153 );
	hb_xvmPushFuncSymbol( symbols + 142 );
	hb_xvmPushStringConst( "Ok", 2 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 65 );
	hb_xvmPushInteger( 53 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmArrayItemPush( 11L ) ) break;
	{
		static const HB_BYTE codeblock[ 104 ] = {
			0, 0, 2, 0, 2, 0, 1, 0, 95, 255, 176, 49, 0, 106, 24, 95, 
			72, 77, 71, 95, 80, 82, 73, 78, 84, 69, 82, 95, 71, 69, 84, 80, 
			82, 73, 78, 84, 69, 82, 0, 106, 8, 67, 111, 109, 98, 111, 95, 49, 
			0, 106, 6, 86, 97, 108, 117, 101, 0, 12, 3, 1, 80, 254, 176, 15, 
			0, 106, 24, 95, 72, 77, 71, 95, 80, 82, 73, 78, 84, 69, 82, 95, 
			71, 69, 84, 80, 82, 73, 78, 84, 69, 82, 0, 106, 8, 82, 101, 108, 
			101, 97, 115, 101, 0, 12, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 25 ) ) break;
	hb_xvmSetLine( 1155 );
	hb_xvmPushFuncSymbol( symbols + 142 );
	hb_xvmPushStringConst( "Cancel", 6 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 175 );
	hb_xvmPushInteger( 53 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmArrayItemPush( 12L ) ) break;
	{
		static const HB_BYTE codeblock[ 53 ] = {
			0, 0, 1, 0, 1, 0, 106, 1, 0, 80, 255, 176, 15, 0, 106, 24, 
			95, 72, 77, 71, 95, 80, 82, 73, 78, 84, 69, 82, 95, 71, 69, 84, 
			80, 82, 73, 78, 84, 69, 82, 0, 106, 8, 82, 101, 108, 101, 97, 115, 
			101, 0, 12, 2, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 25 ) ) break;
	hb_xvmSetLine( 1157 );
	hb_xvmPushFuncSymbol( symbols + 43 );
	hb_xvmPushNil();
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 27 );
	{
		static const HB_BYTE codeblock[ 51 ] = {
			176, 15, 0, 106, 24, 95, 72, 77, 71, 95, 80, 82, 73, 78, 84, 69, 
			82, 95, 71, 69, 84, 80, 82, 73, 78, 84, 69, 82, 0, 106, 7, 67, 
			97, 110, 99, 101, 108, 0, 106, 8, 79, 110, 67, 108, 105, 99, 107, 0, 
			12, 3, 6 };
		hb_xvmPushBlockShort( codeblock, symbols );
	}
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1158 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 1160 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushStringConst( "_HMG_PRINTER_GETPRINTER", 23 );
	hb_xvmPushStringConst( "Center", 6 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1162 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushStringConst( "_HMG_PRINTER_GETPRINTER", 23 );
	hb_xvmArrayGen( 1 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1164 );
	hb_xvmPushLocal( 1 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _HMG_PRINTER_H_PRINT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 19 );
	hb_xvmSetLine( 1173 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 20 );
	hb_xvmSetLine( 1176 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 18 );
lab00001: ;
	hb_xvmPushLocal( 19 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmLocalSetInt( 19, 0L );
lab00002: ;
	hb_xvmSetLine( 1178 );
	hb_xvmPushFuncSymbol( symbols + 118 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "N", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 1179 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 9 );
	goto lab00011;
lab00003: ;
	hb_xvmSetLine( 1180 );
	hb_xvmPushFuncSymbol( symbols + 118 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "D", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 1181 );
	hb_xvmPushFuncSymbol( symbols + 144 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 9 );
	goto lab00011;
lab00004: ;
	hb_xvmSetLine( 1182 );
	hb_xvmPushFuncSymbol( symbols + 118 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "L", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 1183 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmArrayItemPush( 24L ) ) break;
	goto lab00006;
lab00005: ;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmArrayItemPush( 25L ) ) break;
lab00006: ;
	hb_xvmPopLocal( 9 );
	goto lab00011;
lab00007: ;
	hb_xvmSetLine( 1184 );
	hb_xvmPushFuncSymbol( symbols + 118 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "A", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 1185 );
	/* *** END PROC *** */
	break;
lab00008: ;
	hb_xvmSetLine( 1186 );
	hb_xvmPushFuncSymbol( symbols + 118 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "B", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 1187 );
	/* *** END PROC *** */
	break;
lab00009: ;
	hb_xvmSetLine( 1188 );
	hb_xvmPushFuncSymbol( symbols + 118 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "O", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 1189 );
	/* *** END PROC *** */
	break;
lab00010: ;
	hb_xvmSetLine( 1190 );
	hb_xvmPushFuncSymbol( symbols + 118 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 1191 );
	/* *** END PROC *** */
	break;
lab00011: ;
	hb_xvmSetLine( 1194 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmMultByInt( 10000L ) ) break;
	if( hb_xvmDivideByInt( 254L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 1195 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmMultByInt( 10000L ) ) break;
	if( hb_xvmDivideByInt( 254L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 1197 );
	hb_xvmPushFuncSymbol( symbols + 118 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmSetLine( 1198 );
	hb_xvmPushFuncSymbol( symbols + 145 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "CENTER", 6 );
	if( hb_xvmEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmSetLine( 1199 );
	hb_xvmPushFuncSymbol( symbols + 146 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 6 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1200 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 20 );
	goto lab00013;
lab00012: ;
	hb_xvmSetLine( 1201 );
	hb_xvmPushFuncSymbol( symbols + 145 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "RIGHT", 5 );
	if( hb_xvmEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmSetLine( 1202 );
	hb_xvmPushFuncSymbol( symbols + 146 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1203 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 20 );
lab00013: ;
	hb_xvmSetLine( 1207 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00014;
	hb_xvmSetLine( 1208 );
	hb_xvmPushLocalByRef( 19 );
	hb_xvmPushInteger( 10 );
	if( hb_xvmMultEqPop() ) break;
lab00014: ;
	hb_xvmSetLine( 1211 );
	hb_xvmPushFuncSymbol( symbols + 147 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmDo( 18 ) ) break;
	hb_xvmSetLine( 1213 );
	hb_xvmPushLocal( 20 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmSetLine( 1214 );
	hb_xvmPushFuncSymbol( symbols + 146 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
lab00015: ;
	hb_xvmSetLine( 1217 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _HMG_PRINTER_H_MULTILINE_PRINT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 19 );
	hb_xvmSetLine( 1222 );
	hb_xvmLocalSetInt( 20, 0L );
	hb_xvmSetLine( 1224 );
	hb_xvmPushFuncSymbol( symbols + 118 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "N", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1225 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 11 );
	goto lab00009;
lab00001: ;
	hb_xvmSetLine( 1226 );
	hb_xvmPushFuncSymbol( symbols + 118 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "D", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 1227 );
	hb_xvmPushFuncSymbol( symbols + 144 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 11 );
	goto lab00009;
lab00002: ;
	hb_xvmSetLine( 1228 );
	hb_xvmPushFuncSymbol( symbols + 118 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "L", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 1229 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmArrayItemPush( 24L ) ) break;
	goto lab00004;
lab00003: ;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	if( hb_xvmArrayItemPush( 25L ) ) break;
lab00004: ;
	hb_xvmPopLocal( 11 );
	goto lab00009;
lab00005: ;
	hb_xvmSetLine( 1230 );
	hb_xvmPushFuncSymbol( symbols + 118 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "A", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 1231 );
	/* *** END PROC *** */
	break;
lab00006: ;
	hb_xvmSetLine( 1232 );
	hb_xvmPushFuncSymbol( symbols + 118 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "B", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 1233 );
	/* *** END PROC *** */
	break;
lab00007: ;
	hb_xvmSetLine( 1234 );
	hb_xvmPushFuncSymbol( symbols + 118 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "O", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 1235 );
	/* *** END PROC *** */
	break;
lab00008: ;
	hb_xvmSetLine( 1236 );
	hb_xvmPushFuncSymbol( symbols + 118 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 1237 );
	/* *** END PROC *** */
	break;
lab00009: ;
	hb_xvmSetLine( 1240 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmMultByInt( 10000L ) ) break;
	if( hb_xvmDivideByInt( 254L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 1241 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmMultByInt( 10000L ) ) break;
	if( hb_xvmDivideByInt( 254L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 1242 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmMultByInt( 10000L ) ) break;
	if( hb_xvmDivideByInt( 254L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 1243 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmMultByInt( 10000L ) ) break;
	if( hb_xvmDivideByInt( 254L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 1245 );
	hb_xvmPushFuncSymbol( symbols + 118 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "C", 1 );
	if( hb_xvmEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 1246 );
	hb_xvmPushFuncSymbol( symbols + 145 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "CENTER", 6 );
	if( hb_xvmEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmSetLine( 1247 );
	hb_xvmLocalSetInt( 20, 6L );
	goto lab00011;
lab00010: ;
	hb_xvmSetLine( 1248 );
	hb_xvmPushFuncSymbol( symbols + 145 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "RIGHT", 5 );
	if( hb_xvmEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 1249 );
	hb_xvmLocalSetInt( 20, 2L );
lab00011: ;
	hb_xvmSetLine( 1253 );
	hb_xvmPushFuncSymbol( symbols + 149 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 20 );
	if( hb_xvmDo( 19 ) ) break;
	hb_xvmSetLine( 1255 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _HMG_PRINTER_H_IMAGE )
{
   do {
	hb_xvmFrame( 0, 8 );
	hb_xvmSetLine( 1260 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmMultByInt( 10000L ) ) break;
	if( hb_xvmDivideByInt( 254L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 1261 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmMultByInt( 10000L ) ) break;
	if( hb_xvmDivideByInt( 254L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 1262 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmMultByInt( 10000L ) ) break;
	if( hb_xvmDivideByInt( 254L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 1263 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmMultByInt( 10000L ) ) break;
	if( hb_xvmDivideByInt( 254L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 1265 );
	hb_xvmPushFuncSymbol( symbols + 151 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmDo( 8 ) ) break;
	hb_xvmSetLine( 1267 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _HMG_PRINTER_H_LINE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 12 );
	hb_xvmSetLine( 1272 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmMultByInt( 10000L ) ) break;
	if( hb_xvmDivideByInt( 254L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 1273 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmMultByInt( 10000L ) ) break;
	if( hb_xvmDivideByInt( 254L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 1274 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmMultByInt( 10000L ) ) break;
	if( hb_xvmDivideByInt( 254L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 1275 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmMultByInt( 10000L ) ) break;
	if( hb_xvmDivideByInt( 254L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 1277 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmLocalSetInt( 12, 0L );
lab00001: ;
	hb_xvmSetLine( 1279 );
	hb_xvmPushFuncSymbol( symbols + 118 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 1280 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmMultByInt( 10000L ) ) break;
	if( hb_xvmDivideByInt( 254L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 6 );
	goto lab00007;
lab00002: ;
	hb_xvmSetLine( 1281 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00007;
	hb_xvmSetLine( 1282 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 1283 );
	hb_xvmLocalSetInt( 6, 1L );
	goto lab00006;
lab00003: ;
	hb_xvmSetLine( 1284 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmEqualIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmSetLine( 1285 );
	hb_xvmLocalSetInt( 6, 3L );
	goto lab00006;
lab00004: ;
	hb_xvmSetLine( 1286 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmEqualIntIs( 3L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmSetLine( 1287 );
	hb_xvmLocalSetInt( 6, 10L );
	goto lab00006;
lab00005: ;
	hb_xvmSetLine( 1288 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmEqualIntIs( 4L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 1289 );
	hb_xvmLocalSetInt( 6, 12L );
lab00006: ;
	hb_xvmSetLine( 1291 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 10 );
lab00007: ;
	hb_xvmSetLine( 1294 );
	hb_xvmPushFuncSymbol( symbols + 153 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmDo( 12 ) ) break;
	hb_xvmSetLine( 1296 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _HMG_PRINTER_H_RECTANGLE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 13 );
	hb_xvmSetLine( 1301 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmMultByInt( 10000L ) ) break;
	if( hb_xvmDivideByInt( 254L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 1302 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmMultByInt( 10000L ) ) break;
	if( hb_xvmDivideByInt( 254L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 1303 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmMultByInt( 10000L ) ) break;
	if( hb_xvmDivideByInt( 254L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 1304 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmMultByInt( 10000L ) ) break;
	if( hb_xvmDivideByInt( 254L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 1306 );
	hb_xvmPushFuncSymbol( symbols + 118 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1307 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmMultByInt( 10000L ) ) break;
	if( hb_xvmDivideByInt( 254L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 6 );
lab00001: ;
	hb_xvmSetLine( 1310 );
	hb_xvmPushFuncSymbol( symbols + 155 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 12 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmDo( 13 ) ) break;
	hb_xvmSetLine( 1312 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _HMG_PRINTER_H_ROUNDRECTANGLE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 12 );
	hb_xvmSetLine( 1317 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmMultByInt( 10000L ) ) break;
	if( hb_xvmDivideByInt( 254L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 1318 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmMultByInt( 10000L ) ) break;
	if( hb_xvmDivideByInt( 254L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 1319 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmMultByInt( 10000L ) ) break;
	if( hb_xvmDivideByInt( 254L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 1320 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmMultByInt( 10000L ) ) break;
	if( hb_xvmDivideByInt( 254L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 1322 );
	hb_xvmPushFuncSymbol( symbols + 118 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1323 );
	hb_xvmPushFuncSymbol( symbols + 58 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmMultByInt( 10000L ) ) break;
	if( hb_xvmDivideByInt( 254L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 6 );
lab00001: ;
	hb_xvmSetLine( 1326 );
	hb_xvmPushFuncSymbol( symbols + 157 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmDo( 12 ) ) break;
	hb_xvmSetLine( 1328 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( GETPRINTABLEAREAWIDTH )
{
   do {
	hb_xvmSetLine( 1335 );
	hb_xvmPushFuncSymbol( symbols + 159 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 19L ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( GETPRINTABLEAREAHEIGHT )
{
   do {
	hb_xvmSetLine( 1341 );
	hb_xvmPushFuncSymbol( symbols + 161 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 19L ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( GETPRINTABLEAREAHORIZONTALOFFSET )
{
   HB_BOOL fValue;
   do {
	hb_xvmSetLine( 1347 );
	hb_xvmPushFuncSymbol( symbols + 139 );
	hb_xvmPushStringConst( "_hmg_miniprint[19]", 18 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1348 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 1351 );
	hb_xvmPushFuncSymbol( symbols + 163 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 19L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 164 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 19L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDivide() ) break;
	hb_xvmPushDouble( * ( double * ) "ffffff9@", 10, 1 );
	if( hb_xvmMult() ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( GETPRINTABLEAREAVERTICALOFFSET )
{
   HB_BOOL fValue;
   do {
	hb_xvmSetLine( 1357 );
	hb_xvmPushFuncSymbol( symbols + 139 );
	hb_xvmPushStringConst( "_hmg_miniprint[19]", 18 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1358 );
	hb_xvmRetInt( 0L );
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 1361 );
	hb_xvmPushFuncSymbol( symbols + 166 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 19L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 167 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 19L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDivide() ) break;
	hb_xvmPushDouble( * ( double * ) "ffffff9@", 10, 1 );
	if( hb_xvmMult() ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _HMG_PRINTER_MOUSEZOOM )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 0 );
	hb_xvmSetLine( 1366 );
	hb_xvmPushFuncSymbol( symbols + 17 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 1367 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 1368 );
	hb_xvmLocalSetInt( 3, 0L );
	hb_xvmSetLine( 1369 );
	hb_xvmPushInteger( 45 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushFuncSymbol( symbols + 60 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmLocalAdd( 4 );
	hb_xvmSetLine( 1371 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 8L ) ) break;
	hb_xvmPushInteger( 1000 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 16L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1373 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 8L ) ) break;
	hb_xvmSetLine( 1374 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 6L ) ) break;
	hb_xvmSetLine( 1375 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 7L ) ) break;
	hb_xvmSetLine( 1377 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 50 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1378 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 50 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1380 );
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	goto lab00014;
lab00001: ;
	hb_xvmSetLine( 1388 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 32L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDivideByInt( 2L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 10L ) ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmLessEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 31L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmDivideByInt( 2L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmLessEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 1390 );
	hb_xvmLocalSetInt( 3, 1L );
	goto lab00005;
lab00002: ;
	hb_xvmSetLine( 1394 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 32L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDivideByInt( 2L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 10L ) ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 31L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmDivideByInt( 2L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmLessEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 1396 );
	hb_xvmLocalSetInt( 3, 2L );
	goto lab00005;
lab00003: ;
	hb_xvmSetLine( 1400 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 32L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDivideByInt( 2L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 10L ) ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmLessEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 31L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmDivideByInt( 2L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmSetLine( 1402 );
	hb_xvmLocalSetInt( 3, 3L );
	goto lab00005;
lab00004: ;
	hb_xvmSetLine( 1406 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 32L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmDivideByInt( 2L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 10L ) ) break;
	if( hb_xvmMinus() ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 31L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmDivideByInt( 2L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmMinus() ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmSetLine( 1408 );
	hb_xvmLocalSetInt( 3, 4L );
lab00005: ;
	hb_xvmSetLine( 1414 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 57 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmSetLine( 1418 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 1419 );
	hb_xvmPushInteger( 1000 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 16L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 8L ) ) break;
	hb_xvmSetLine( 1420 );
	hb_xvmPushInteger( 100 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 6L ) ) break;
	hb_xvmSetLine( 1421 );
	hb_xvmPushInteger( 400 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 7L ) ) break;
	hb_xvmSetLine( 1422 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 10 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1423 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 40 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00013;
lab00006: ;
	hb_xvmSetLine( 1424 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmEqualIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00007;
	hb_xvmSetLine( 1425 );
	hb_xvmPushInteger( 1000 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 16L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 8L ) ) break;
	hb_xvmSetLine( 1426 );
	hb_xvmPushInteger( -100 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 6L ) ) break;
	hb_xvmSetLine( 1427 );
	hb_xvmPushInteger( 400 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 7L ) ) break;
	hb_xvmSetLine( 1428 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 10 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1429 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 60 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00013;
lab00007: ;
	hb_xvmSetLine( 1430 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmEqualIntIs( 3L, &fValue ) ) break;
	if( !fValue )
		goto lab00008;
	hb_xvmSetLine( 1431 );
	hb_xvmPushInteger( 1000 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 16L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 8L ) ) break;
	hb_xvmSetLine( 1432 );
	hb_xvmPushInteger( 100 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 6L ) ) break;
	hb_xvmSetLine( 1433 );
	hb_xvmPushInteger( -400 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 7L ) ) break;
	hb_xvmSetLine( 1434 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 90 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1435 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 40 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00013;
lab00008: ;
	hb_xvmSetLine( 1436 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmEqualIntIs( 4L, &fValue ) ) break;
	if( !fValue )
		goto lab00013;
	hb_xvmSetLine( 1437 );
	hb_xvmPushInteger( 1000 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 16L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 8L ) ) break;
	hb_xvmSetLine( 1438 );
	hb_xvmPushInteger( -100 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 6L ) ) break;
	hb_xvmSetLine( 1439 );
	hb_xvmPushInteger( -400 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 7L ) ) break;
	hb_xvmSetLine( 1440 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 90 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1441 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 60 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00013;
lab00009: ;
	hb_xvmSetLine( 1448 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00010;
	hb_xvmSetLine( 1449 );
	hb_xvmPushInteger( 1000 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 16L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 8L ) ) break;
	hb_xvmSetLine( 1450 );
	hb_xvmPushInteger( 500 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 6L ) ) break;
	hb_xvmSetLine( 1451 );
	hb_xvmPushInteger( 300 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 7L ) ) break;
	hb_xvmSetLine( 1452 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 20 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1453 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00013;
lab00010: ;
	hb_xvmSetLine( 1454 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmEqualIntIs( 2L, &fValue ) ) break;
	if( !fValue )
		goto lab00011;
	hb_xvmSetLine( 1455 );
	hb_xvmPushInteger( 1000 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 16L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 8L ) ) break;
	hb_xvmSetLine( 1456 );
	hb_xvmPushInteger( -500 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 6L ) ) break;
	hb_xvmSetLine( 1457 );
	hb_xvmPushInteger( 300 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 7L ) ) break;
	hb_xvmSetLine( 1458 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 20 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1459 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 99 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00013;
lab00011: ;
	hb_xvmSetLine( 1460 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmEqualIntIs( 3L, &fValue ) ) break;
	if( !fValue )
		goto lab00012;
	hb_xvmSetLine( 1461 );
	hb_xvmPushInteger( 1000 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 16L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 8L ) ) break;
	hb_xvmSetLine( 1462 );
	hb_xvmPushInteger( 500 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 6L ) ) break;
	hb_xvmSetLine( 1463 );
	hb_xvmPushInteger( -300 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 7L ) ) break;
	hb_xvmSetLine( 1464 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 80 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1465 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00013;
lab00012: ;
	hb_xvmSetLine( 1466 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmEqualIntIs( 4L, &fValue ) ) break;
	if( !fValue )
		goto lab00013;
	hb_xvmSetLine( 1467 );
	hb_xvmPushInteger( 1000 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 16L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 8L ) ) break;
	hb_xvmSetLine( 1468 );
	hb_xvmPushInteger( -500 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 6L ) ) break;
	hb_xvmSetLine( 1469 );
	hb_xvmPushInteger( -300 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 7L ) ) break;
	hb_xvmSetLine( 1470 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 80 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1471 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 99 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
lab00013: ;
	hb_xvmSetLine( 1476 );
	hb_xvmPushFuncSymbol( symbols + 168 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00014: ;
	hb_xvmSetLine( 1480 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 1482 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _HMG_PRINTER_ZOOM )
{
   HB_BOOL fValue;
   do {
	hb_xvmSetLine( 1488 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 8L ) ) break;
	hb_xvmPushInteger( 1000 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 16L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1490 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 8L ) ) break;
	hb_xvmSetLine( 1491 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 6L ) ) break;
	hb_xvmSetLine( 1492 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 7L ) ) break;
	hb_xvmSetLine( 1494 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 50 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1495 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 50 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1497 );
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
	goto lab00004;
lab00001: ;
	hb_xvmSetLine( 1503 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushFuncSymbol( symbols + 57 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 20L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 1505 );
	hb_xvmPushInteger( 1000 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 16L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 8L ) ) break;
	hb_xvmSetLine( 1506 );
	hb_xvmPushInteger( 100 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 6L ) ) break;
	hb_xvmSetLine( 1507 );
	hb_xvmPushInteger( 400 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 7L ) ) break;
	hb_xvmSetLine( 1508 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 10 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1509 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 40 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 1513 );
	hb_xvmPushInteger( 1000 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 16L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 8L ) ) break;
	hb_xvmSetLine( 1514 );
	hb_xvmPushInteger( 500 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 6L ) ) break;
	hb_xvmSetLine( 1515 );
	hb_xvmPushInteger( 300 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPop( 7L ) ) break;
	hb_xvmSetLine( 1516 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 1 );
	hb_xvmPushInteger( 20 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 1517 );
	hb_xvmPushFuncSymbol( symbols + 63 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushInteger( 0 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 4 ) ) break;
lab00003: ;
	hb_xvmSetLine( 1521 );
	hb_xvmPushFuncSymbol( symbols + 168 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushStringConst( "_HMG_PRINTER_SHOWPREVIEW", 24 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00004: ;
	hb_xvmSetLine( 1525 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	if( hb_xvmDo( 0 ) ) break;
	hb_xvmSetLine( 1527 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _HMG_PRINTER_SETJOBNAME )
{
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 1532 );
	hb_xvmPushFuncSymbol( symbols + 170 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "HMGPrintSys", 11 );
	if( hb_xvmDo( 2 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( HMG_PRINTGETJOBINFO )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 1537 );
	hb_xvmPushFuncSymbol( symbols + 118 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1538 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 26L ) ) break;
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmArrayGen( 2 );
	hb_xvmPopLocal( 1 );
lab00001: ;
	hb_xvmSetLine( 1541 );
	hb_xvmPushFuncSymbol( symbols + 172 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( HMG_PRINTERGETSTATUS )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 0, 1 );
	hb_xvmSetLine( 1546 );
	hb_xvmPushFuncSymbol( symbols + 118 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1547 );
	if( hb_xvmPushMemvar( symbols + 6 ) ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 1 );
lab00001: ;
	hb_xvmSetLine( 1550 );
	hb_xvmPushFuncSymbol( symbols + 174 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmDo( 1 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _DEFINEEMFFILE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 5, 13 );
	hb_xvmSetLine( 1556 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 1559 );
	hb_xvmPushFuncSymbol( symbols + 118 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushStringConst( "U", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 1560 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPopLocal( 8 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 1562 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 16 );
lab00002: ;
	hb_xvmSetLine( 1565 );
	hb_xvmPushLocal( 11 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 11 );
lab00003: ;
	hb_xvmPushLocal( 12 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00004;
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 12 );
lab00004: ;
	hb_xvmPushLocal( 13 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPopLocal( 13 );
lab00005: ;
	hb_xvmSetLine( 1567 );
	hb_xvmPushStringConst( "_", 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 15 );
	hb_xvmSetLine( 1568 );
	hb_xvmPushFuncSymbol( symbols + 175 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 17 );
	hb_xvmSetLine( 1570 );
	hb_xvmPushFuncSymbol( symbols + 45 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 1572 );
	hb_xvmPushFuncSymbol( symbols + 176 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmFunction( 6 ) ) break;
	hb_xvmPopLocal( 18 );
	hb_xvmSetLine( 1575 );
	hb_xvmPushFuncSymbol( symbols + 177 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 1580 );
	hb_xvmPushStringConst( "IMAGE", 5 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1581 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1582 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1583 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1584 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1585 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1586 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1587 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmPushInteger( 1 );
	goto lab00007;
lab00006: ;
	hb_xvmPushInteger( 0 );
lab00007: ;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1588 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmPushInteger( 1 );
	goto lab00009;
lab00008: ;
	hb_xvmPushInteger( 0 );
lab00009: ;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1589 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1590 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1591 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1592 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1593 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1594 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1595 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1596 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1597 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1598 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1599 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1600 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1601 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00010;
	hb_xvmPushInteger( 1 );
	goto lab00011;
lab00010: ;
	hb_xvmPushInteger( 0 );
lab00011: ;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1602 );
	hb_xvmPushInteger( -1 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1603 );
	hb_xvmPushInteger( -1 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1604 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1605 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1606 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1607 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1608 );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmPushLogical( HB_FALSE );
	hb_xvmArrayGen( 4 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1609 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1610 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1611 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1612 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1613 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	hb_xvmPushLogical( HB_FALSE );
	goto lab00013;
lab00012: ;
	hb_xvmPushLogical( HB_TRUE );
lab00013: ;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1614 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 209L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1615 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1616 );
	hb_xvmPushFuncSymbol( symbols + 178 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 6 ) ) break;
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1617 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1618 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1619 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 7 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 1621 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

#line 1627 "miniprint.prg"

#ifndef CINTERFACE
  #define CINTERFACE
#endif

#define NO_LEAN_AND_MEAN

#include <mgdefs.h>
#include "hbapiitm.h"

#include <olectl.h>

#ifndef WC_STATIC
  #define WC_STATIC   TEXT( "Static" )
#endif

static DWORD charset = DEFAULT_CHARSET;

#ifdef UNICODE
  LPWSTR AnsiToWide( LPCSTR );
  LPSTR  WideToAnsi( LPWSTR );
#endif
HINSTANCE GetInstance( void );

#ifdef __cplusplus
  extern "C" {
#endif
extern HBITMAP HMG_LoadImage( char * FileName );
#ifdef __cplusplus
  }
#endif


HB_FUNC( _HMG_SETCHARSET )
{
   charset = hmg_par_DWORD( 1 );
}

HB_FUNC( _HMG_PRINTER_ABORTDOC )
{
   HDC hdcPrint = hmg_par_raw_HDC( 1 );

   AbortDoc( hdcPrint );
}

HB_FUNC( _HMG_PRINTER_STARTDOC )
{

   DOCINFO docInfo;

#ifndef UNICODE
   LPTSTR lpText = ( LPTSTR ) hb_parc( 2 );
#else
   LPWSTR lpText = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif
   HDC hdcPrint = hmg_par_raw_HDC( 1 );

   if( hdcPrint != 0 )
   {
      ZeroMemory( &docInfo, sizeof( docInfo ) );
      docInfo.cbSize      = sizeof( docInfo );
      docInfo.lpszDocName = lpText;

      hb_retni( StartDoc( hdcPrint, &docInfo ) );
   }
}

HB_FUNC( _HMG_PRINTER_STARTPAGE )
{

   HDC hdcPrint = hmg_par_raw_HDC( 1 );

   if( hdcPrint != 0 )
      StartPage( hdcPrint );

}

HB_FUNC( _HMG_PRINTER_C_PRINT )
{

   // 1:  Hdc
   // 2:  y
   // 3:  x
   // 4:  FontName
   // 5:  FontSize
   // 6:  R Color
   // 7:  G Color
   // 8:  B Color
   // 9:  Text
   // 10: Bold
   // 11: Italic
   // 12: Underline
   // 13: StrikeOut
   // 14: Color Flag
   // 15: FontName Flag
   // 16: FontSize Flag
   // 17: Angle Flag
   // 18: Angle

   HGDIOBJ hgdiobj;

   TCHAR FontName[ 32 ];
   int  FontSize;

#ifdef UNICODE
   LPWSTR pFontName, pText;
#endif

   DWORD fdwItalic;
   DWORD fdwUnderline;
   DWORD fdwStrikeOut;

   int fnWeight;
   int r;
   int g;
   int b;

   int x = hb_parni( 3 );
   int y = hb_parni( 2 );

   HFONT hfont;

   HDC hdcPrint = hmg_par_raw_HDC( 1 );

   int FontHeight;
   int FontAngle;

   if( hdcPrint != 0 )
   {

      // Bold

      if( hb_parl( 10 ) )
         fnWeight = FW_BOLD;
      else
         fnWeight = FW_NORMAL;

      // Italic

      if( hb_parl( 11 ) )
         fdwItalic = TRUE;
      else
         fdwItalic = FALSE;

      // UnderLine

      if( hb_parl( 12 ) )
         fdwUnderline = TRUE;
      else
         fdwUnderline = FALSE;

      // StrikeOut

      if( hb_parl( 13 ) )
         fdwStrikeOut = TRUE;
      else
         fdwStrikeOut = FALSE;

      // Color

      if( hb_parl( 14 ) )
      {
         r = hb_parni( 6 );
         g = hb_parni( 7 );
         b = hb_parni( 8 );
      }
      else
      {
         r = 0;
         g = 0;
         b = 0;
      }

      // Fontname

      if( hb_parl( 15 ) )
      {
#ifndef UNICODE
         lstrcpy( FontName, hb_parc( 4 ) );
#else
         pFontName = AnsiToWide( hb_parc( 4 ) );
         lstrcpy( FontName, pFontName );
         hb_xfree( pFontName );
#endif
      }
      else
         lstrcpy( FontName, TEXT( "Arial" ) );

      // FontSize

      if( hb_parl( 16 ) )
         FontSize = hb_parni( 5 );
      else
         FontSize = 10;

      // Angle

      if( hb_parl( 17 ) )
         FontAngle = hb_parni( 18 );
      else
         FontAngle = 0;

      FontHeight = -MulDiv( FontSize, GetDeviceCaps( hdcPrint, LOGPIXELSY ), 72 );

      hfont = CreateFont
              (
         FontHeight,
         0,
         FontAngle,
         FontAngle,
         fnWeight,
         fdwItalic,
         fdwUnderline,
         fdwStrikeOut,
         charset,
         OUT_TT_PRECIS,
         CLIP_DEFAULT_PRECIS,
         DEFAULT_QUALITY,
         FF_DONTCARE,
         FontName
              );

      hgdiobj = SelectObject( hdcPrint, hfont );

      SetTextColor( hdcPrint, RGB( r, g, b ) );
      SetBkMode( hdcPrint, TRANSPARENT );

#ifndef UNICODE
      TextOut( hdcPrint,
               ( x * GetDeviceCaps( hdcPrint, LOGPIXELSX ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETX ),
               ( y * GetDeviceCaps( hdcPrint, LOGPIXELSY ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETY ),
               hb_parc( 9 ),
               strlen( hb_parc( 9 ) ) );
#else
      pText = AnsiToWide( hb_parc( 9 ) );
      TextOut( hdcPrint,
               ( x * GetDeviceCaps( hdcPrint, LOGPIXELSX ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETX ),
               ( y * GetDeviceCaps( hdcPrint, LOGPIXELSY ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETY ),
               pText,
               lstrlen( pText ) );
      hb_xfree( pText );
#endif

      SelectObject( hdcPrint, hgdiobj );

      DeleteObject( hfont );

   }

}

HB_FUNC( _HMG_PRINTER_C_MULTILINE_PRINT )
{

   // 1:  Hdc
   // 2:  y
   // 3:  x
   // 4:  FontName
   // 5:  FontSize
   // 6:  R Color
   // 7:  G Color
   // 8:  B Color
   // 9:  Text
   // 10: Bold
   // 11: Italic
   // 12: Underline
   // 13: StrikeOut
   // 14: Color Flag
   // 15: FontName Flag
   // 16: FontSize Flag
   // 17: ToRow
   // 18: ToCol
   // 19: Alignment

   UINT uFormat = 0;

   HGDIOBJ hgdiobj;

   TCHAR FontName[ 32 ];
   int  FontSize;

#ifdef UNICODE
   LPWSTR pFontName, pText;
#endif

   DWORD fdwItalic;
   DWORD fdwUnderline;
   DWORD fdwStrikeOut;

   RECT rect;

   int fnWeight;
   int r;
   int g;
   int b;

   int x   = hb_parni( 3 );
   int y   = hb_parni( 2 );
   int toy = hb_parni( 17 );
   int tox = hb_parni( 18 );

   HFONT hfont;

   HDC hdcPrint = hmg_par_raw_HDC( 1 );

   int FontHeight;

   if( hdcPrint != 0 )
   {

      // Bold

      if( hb_parl( 10 ) )
         fnWeight = FW_BOLD;
      else
         fnWeight = FW_NORMAL;

      // Italic

      if( hb_parl( 11 ) )
         fdwItalic = TRUE;
      else
         fdwItalic = FALSE;

      // UnderLine

      if( hb_parl( 12 ) )
         fdwUnderline = TRUE;
      else
         fdwUnderline = FALSE;

      // StrikeOut

      if( hb_parl( 13 ) )
         fdwStrikeOut = TRUE;
      else
         fdwStrikeOut = FALSE;

      // Color

      if( hb_parl( 14 ) )
      {
         r = hb_parni( 6 );
         g = hb_parni( 7 );
         b = hb_parni( 8 );
      }
      else
      {
         r = 0;
         g = 0;
         b = 0;
      }

      // Fontname

      if( hb_parl( 15 ) )
      {
#ifndef UNICODE
         lstrcpy( FontName, hb_parc( 4 ) );
#else
         pFontName = AnsiToWide( hb_parc( 4 ) );
         lstrcpy( FontName, pFontName );
         hb_xfree( pFontName );
#endif
      }
      else
         lstrcpy( FontName, TEXT( "Arial" ) );

      // FontSize

      if( hb_parl( 16 ) )
         FontSize = hb_parni( 5 );
      else
         FontSize = 10;

      FontHeight = -MulDiv( FontSize, GetDeviceCaps( hdcPrint, LOGPIXELSY ), 72 );

      hfont = CreateFont
              (
         FontHeight,
         0,
         0,
         0,
         fnWeight,
         fdwItalic,
         fdwUnderline,
         fdwStrikeOut,
         charset,
         OUT_TT_PRECIS,
         CLIP_DEFAULT_PRECIS,
         DEFAULT_QUALITY,
         FF_DONTCARE,
         FontName
              );

      if( hb_parni( 19 ) == 0 )
         uFormat = DT_END_ELLIPSIS | DT_NOPREFIX | DT_WORDBREAK | DT_LEFT;
      else if( hb_parni( 19 ) == 2 )
         uFormat = DT_END_ELLIPSIS | DT_NOPREFIX | DT_WORDBREAK | DT_RIGHT;
      else if( hb_parni( 19 ) == 6 )
         uFormat = DT_END_ELLIPSIS | DT_NOPREFIX | DT_WORDBREAK | DT_CENTER;

      hgdiobj = SelectObject( hdcPrint, hfont );

      SetTextColor( hdcPrint, RGB( r, g, b ) );
      SetBkMode( hdcPrint, TRANSPARENT );

      rect.left   = ( x * GetDeviceCaps( hdcPrint, LOGPIXELSX ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETX );
      rect.top    = ( y * GetDeviceCaps( hdcPrint, LOGPIXELSY ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETY );
      rect.right  = ( tox * GetDeviceCaps( hdcPrint, LOGPIXELSX ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETX );
      rect.bottom = ( toy * GetDeviceCaps( hdcPrint, LOGPIXELSY ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETY );

#ifndef UNICODE
      DrawText( hdcPrint,
                hb_parc( 9 ),
                strlen( hb_parc( 9 ) ),
                &rect,
                uFormat
                );
#else
      pText = AnsiToWide( hb_parc( 9 ) );
      DrawText( hdcPrint,
                pText,
                lstrlen( pText ),
                &rect,
                uFormat
                );
      hb_xfree( pText );
#endif

      SelectObject( hdcPrint, hgdiobj );

      DeleteObject( hfont );

   }

}

HB_FUNC( _HMG_PRINTER_ENDPAGE )
{
   HDC hdcPrint = hmg_par_raw_HDC( 1 );

   if( hdcPrint != 0 )
      EndPage( hdcPrint );
}

HB_FUNC( _HMG_PRINTER_ENDDOC )
{
   HDC hdcPrint = hmg_par_raw_HDC( 1 );

   if( hdcPrint != 0 )
      EndDoc( hdcPrint );
}

HB_FUNC( _HMG_PRINTER_DELETEDC )
{
   HDC hdcPrint = hmg_par_raw_HDC( 1 );

   DeleteDC( hdcPrint );
}

HB_FUNC( _HMG_PRINTER_PRINTDIALOG )
{

#ifdef UNICODE
   LPSTR pStr;
#endif
   PRINTDLG pd;

   LPDEVMODE pDevMode;

   pd.lStructSize         = sizeof( PRINTDLG );
   pd.hDevMode            = ( HANDLE ) NULL;
   pd.hDevNames           = ( HANDLE ) NULL;
   pd.Flags               = PD_RETURNDC | PD_PRINTSETUP;
   pd.hwndOwner           = NULL;
   pd.hDC                 = NULL;
   pd.nFromPage           = 1;
   pd.nToPage             = 0xFFFF;
   pd.nMinPage            = 1;
   pd.nMaxPage            = 0xFFFF;
   pd.nCopies             = 1;
   pd.hInstance           = ( HINSTANCE ) NULL;
   pd.lCustData           = 0L;
   pd.lpfnPrintHook       = ( LPPRINTHOOKPROC ) NULL;
   pd.lpfnSetupHook       = ( LPSETUPHOOKPROC ) NULL;
   pd.lpPrintTemplateName = NULL;
   pd.lpSetupTemplateName = NULL;
   pd.hPrintTemplate      = ( HANDLE ) NULL;
   pd.hSetupTemplate      = ( HANDLE ) NULL;

   if( PrintDlg( &pd ) )
   {
      pDevMode = ( LPDEVMODE ) GlobalLock( pd.hDevMode );

      hb_reta( 4 );
      hmg_storvnl_HANDLE( pd.hDC, -1, 1 );
#ifndef UNICODE
      HB_STORC( ( const char * ) pDevMode->dmDeviceName, -1, 2 );
#else
      pStr = WideToAnsi( pDevMode->dmDeviceName );
      HB_STORC( pStr, -1, 2 );
      hb_xfree( pStr );
#endif
      HB_STORNI( pDevMode->dmCopies > 1 ? pDevMode->dmCopies : pd.nCopies, -1, 3 );
      HB_STORNI( pDevMode->dmCollate, -1, 4 );

      GlobalUnlock( pd.hDevMode );
   }
   else
   {
      hb_reta( 4 );
      HB_STORVNL( 0, -1, 1 );
#ifndef UNICODE
      HB_STORC( "", -1, 2 );
#else
      pStr = WideToAnsi( TEXT( "" ) );
      HB_STORC( pStr, -1, 2 );
      hb_xfree( pStr );
#endif
      HB_STORNI( 0, -1, 3 );
      HB_STORNI( 0, -1, 4 );
   }

}

HB_FUNC( APRINTERS )
{

   OSVERSIONINFO osvi;

   HGLOBAL cBuffer;
   HGLOBAL pBuffer;

   DWORD dwSize     = 0;
   DWORD dwPrinters = 0;
   DWORD i;

#ifdef UNICODE
   LPSTR pStr;
#endif

   PRINTER_INFO_4 * pInfo4 = NULL;
   PRINTER_INFO_5 * pInfo  = NULL;

   osvi.dwOSVersionInfoSize = sizeof( OSVERSIONINFO );

   GetVersionEx( &osvi );

   if( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT )
      EnumPrinters( PRINTER_ENUM_LOCAL | PRINTER_ENUM_CONNECTIONS, NULL, 4, NULL, 0, &dwSize, &dwPrinters );
   else
      EnumPrinters( PRINTER_ENUM_LOCAL, NULL, 5, NULL, 0, &dwSize, &dwPrinters );

   pBuffer = ( char * ) GlobalAlloc( GPTR, dwSize );

   if( pBuffer == NULL )
   {
      hb_reta( 0 );
      GlobalFree( pBuffer );
      return;
   }

   if( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT )
      EnumPrinters( PRINTER_ENUM_LOCAL | PRINTER_ENUM_CONNECTIONS, NULL, 4, ( LPBYTE ) pBuffer, dwSize, &dwSize, &dwPrinters );
   else
      EnumPrinters( PRINTER_ENUM_LOCAL, NULL, 5, ( LPBYTE ) pBuffer, dwSize, &dwSize, &dwPrinters );

   if( dwPrinters == 0 )
   {
      hb_reta( 0 );
      GlobalFree( pBuffer );
      return;
   }

   if( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT )
      pInfo4 = ( PRINTER_INFO_4 * ) pBuffer;
   else
      pInfo = ( PRINTER_INFO_5 * ) pBuffer;

   hb_reta( dwPrinters );

   if( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT )
      for( i = 0; i < dwPrinters; i++, pInfo4++ )
      {
         cBuffer = GlobalAlloc( GPTR, 256 );
         lstrcat( cBuffer, pInfo4->pPrinterName );
#ifndef UNICODE
         HB_STORC( ( const char * ) cBuffer, -1, i + 1 );
#else
         pStr = WideToAnsi( cBuffer );
         HB_STORC( pStr, -1, i + 1 );
         hb_xfree( pStr );
#endif
         GlobalFree( cBuffer );
      }
   else
      for( i = 0; i < dwPrinters; i++, pInfo++ )
      {
         cBuffer = GlobalAlloc( GPTR, 256 );
         lstrcat( cBuffer, pInfo->pPrinterName );
#ifndef UNICODE
         HB_STORC( ( const char * ) cBuffer, -1, i + 1 );
#else
         pStr = WideToAnsi( cBuffer );
         HB_STORC( pStr, -1, i + 1 );
         hb_xfree( pStr );
#endif
         GlobalFree( cBuffer );
      }

   GlobalFree( pBuffer );

}

HB_FUNC( _HMG_PRINTER_C_RECTANGLE )
{

   // 1: hDC
   // 2: y
   // 3: x
   // 4: toy
   // 5: tox
   // 6: width
   // 7: R Color
   // 8: G Color
   // 9: B Color
   // 10: lWidth
   // 11: lColor
   // 12: lFilled

   int r;
   int g;
   int b;

   int x = hb_parni( 3 );
   int y = hb_parni( 2 );

   int tox = hb_parni( 5 );
   int toy = hb_parni( 4 );

   int width;

   HDC     hdcPrint = hmg_par_raw_HDC( 1 );
   HGDIOBJ hgdiobj;
   HBRUSH  hbrush = NULL;
   HPEN    hpen   = NULL;
   RECT    rect;

   if( hdcPrint != 0 )
   {

      // Width

      if( hb_parl( 10 ) )
         width = hb_parni( 6 );
      else
         width = 1 * 10000 / 254;

      // Color

      if( hb_parl( 11 ) )
      {
         r = hb_parni( 7 );
         g = hb_parni( 8 );
         b = hb_parni( 9 );
      }
      else
      {
         r = 0;
         g = 0;
         b = 0;
      }

      // Filled

      if( hb_parl( 12 ) )
      {
         hbrush  = CreateSolidBrush( ( COLORREF ) RGB( r, g, b ) );
         hgdiobj = SelectObject( hdcPrint, hbrush );
      }
      else
      {
         hpen    = CreatePen( PS_SOLID, ( width * GetDeviceCaps( hdcPrint, LOGPIXELSX ) / 1000 ), ( COLORREF ) RGB( r, g, b ) );
         hgdiobj = SelectObject( hdcPrint, hpen );
      }

      // Border  ( contributed by Alen Uzelac 08.06.2011 )

      rect.left   = ( x * GetDeviceCaps( hdcPrint, LOGPIXELSX ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETX );
      rect.top    = ( y * GetDeviceCaps( hdcPrint, LOGPIXELSY ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETY );
      rect.right  = ( tox * GetDeviceCaps( hdcPrint, LOGPIXELSX ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETX );
      rect.bottom = ( toy * GetDeviceCaps( hdcPrint, LOGPIXELSY ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETY );

      if( hb_parl( 12 ) && hb_parl( 13 ) )
         FillRect( hdcPrint, &rect, ( HBRUSH ) hbrush );
      else
         Rectangle( hdcPrint, rect.left, rect.top, rect.right, rect.bottom );

      SelectObject( hdcPrint, ( HGDIOBJ ) hgdiobj );

      if( hb_parl( 12 ) )
         DeleteObject( hbrush );
      else
         DeleteObject( hpen );

   }

}

HB_FUNC( _HMG_PRINTER_C_ROUNDRECTANGLE )
{

   // 1: hDC
   // 2: y
   // 3: x
   // 4: toy
   // 5: tox
   // 6: width
   // 7: R Color
   // 8: G Color
   // 9: B Color
   // 10: lWidth
   // 11: lColor
   // 12: lFilled

   int r;
   int g;
   int b;

   int x = hb_parni( 3 );
   int y = hb_parni( 2 );

   int tox = hb_parni( 5 );
   int toy = hb_parni( 4 );

   int width;

   int w, h, p;

   HDC     hdcPrint = hmg_par_raw_HDC( 1 );
   HGDIOBJ hgdiobj;
   HBRUSH  hbrush = NULL;
   HPEN    hpen   = NULL;

   if( hdcPrint != 0 )
   {

      // Width

      if( hb_parl( 10 ) )
         width = hb_parni( 6 );
      else
         width = 1 * 10000 / 254;

      // Color

      if( hb_parl( 11 ) )
      {
         r = hb_parni( 7 );
         g = hb_parni( 8 );
         b = hb_parni( 9 );
      }
      else
      {
         r = 0;
         g = 0;
         b = 0;
      }

      // Filled

      if( hb_parl( 12 ) )
      {
         hbrush  = CreateSolidBrush( ( COLORREF ) RGB( r, g, b ) );
         hgdiobj = SelectObject( ( HDC ) hdcPrint, hbrush );
      }
      else
      {
         hpen    = CreatePen( PS_SOLID, ( width * GetDeviceCaps( hdcPrint, LOGPIXELSX ) / 1000 ), ( COLORREF ) RGB( r, g, b ) );
         hgdiobj = SelectObject( ( HDC ) hdcPrint, hpen );
      }

      w = ( tox * GetDeviceCaps( hdcPrint, LOGPIXELSX ) / 1000 ) - ( x * GetDeviceCaps( hdcPrint, LOGPIXELSX ) / 1000 );
      h = ( toy * GetDeviceCaps( hdcPrint, LOGPIXELSY ) / 1000 ) - ( y * GetDeviceCaps( hdcPrint, LOGPIXELSY ) / 1000 );
      p = ( w + h ) / 2;
      p = p / 10;

      RoundRect( ( HDC ) hdcPrint,
                 ( x * GetDeviceCaps( hdcPrint, LOGPIXELSX ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETX ),
                 ( y * GetDeviceCaps( hdcPrint, LOGPIXELSY ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETY ),
                 ( tox * GetDeviceCaps( hdcPrint, LOGPIXELSX ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETX ),
                 ( toy * GetDeviceCaps( hdcPrint, LOGPIXELSY ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETY ),
                 p,
                 p
                 );

      SelectObject( hdcPrint, ( HGDIOBJ ) hgdiobj );

      if( hb_parl( 12 ) )
         DeleteObject( hbrush );
      else
         DeleteObject( hpen );

   }

}

HB_FUNC( _HMG_PRINTER_C_LINE )
{

   // 1: hDC
   // 2: y
   // 3: x
   // 4: toy
   // 5: tox
   // 6: width
   // 7: R Color
   // 8: G Color
   // 9: B Color
   // 10: lWidth
   // 11: lColor
   // 12: nStyle

   int r;
   int g;
   int b;

   int x = hb_parni( 3 );
   int y = hb_parni( 2 );

   int tox = hb_parni( 5 );
   int toy = hb_parni( 4 );

   int width;
   int Style;

   HDC     hdcPrint = hmg_par_raw_HDC( 1 );
   HGDIOBJ hgdiobj;
   HPEN    hpen;

   if( hdcPrint != 0 )
   {

      // Width

      if( hb_parl( 10 ) )
         width = hb_parni( 6 );
      else
         width = 1 * 10000 / 254;

      // Color

      if( hb_parl( 11 ) )
      {
         r = hb_parni( 7 );
         g = hb_parni( 8 );
         b = hb_parni( 9 );
      }
      else
      {
         r = 0;
         g = 0;
         b = 0;
      }

      switch( hb_parni( 12 ) )
      {
         case 1:
            Style = PS_DOT;
            break;
         case 2:
            Style = PS_DASH;
            break;
         case 3:
            Style = PS_DASHDOT;
            break;
         case 4:
            Style = PS_DASHDOTDOT;
            break;
         default:
            Style = PS_SOLID;
      }

      hpen = CreatePen( Style, ( width * GetDeviceCaps( hdcPrint, LOGPIXELSX ) / 1000 ), ( COLORREF ) RGB( r, g, b ) );

      hgdiobj = SelectObject( hdcPrint, hpen );

      MoveToEx( hdcPrint,
                ( x * GetDeviceCaps( hdcPrint, LOGPIXELSX ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETX ),
                ( y * GetDeviceCaps( hdcPrint, LOGPIXELSY ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETY ),
                NULL
                );

      LineTo( hdcPrint,
              ( tox * GetDeviceCaps( hdcPrint, LOGPIXELSX ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETX ),
              ( toy * GetDeviceCaps( hdcPrint, LOGPIXELSY ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETY )
              );

      SelectObject( hdcPrint, ( HGDIOBJ ) hgdiobj );

      DeleteObject( hpen );

   }

}

HB_FUNC( _HMG_PRINTER_SETPRINTERPROPERTIES )
{
   HANDLE hPrinter = NULL;
   DWORD  dwNeeded = 0;
   PRINTER_INFO_2 * pi2;
   DEVMODE *        pDevMode = NULL;
   BOOL bFlag;
   LONG lFlag;

   HDC hdcPrint;

#ifdef UNICODE
   LPWSTR pPrinterName, pDeviceName, pwszDevice;
   LPSTR  pStr;
#endif

   int fields = 0;

#ifndef UNICODE
   bFlag = OpenPrinter( ( LPSTR ) hb_parc( 1 ), &hPrinter, NULL );
#else
   pPrinterName = AnsiToWide( hb_parc( 1 ) );
   bFlag        = OpenPrinter( pPrinterName, &hPrinter, NULL );
   hb_xfree( pPrinterName );
#endif

   if( ! bFlag || ( hPrinter == NULL ) )
   {
#ifdef _ERRORMSG_
      MessageBox( 0, TEXT( "Printer Configuration Failed! (001)" ), TEXT( "Error!" ), MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL );
#endif
      hb_reta( 4 );
      HB_STORVNL( 0, -1, 1 );
      HB_STORC( "", -1, 2 );
      HB_STORNI( 0, -1, 3 );
      HB_STORNI( 0, -1, 4 );

      return;
   }

   SetLastError( 0 );

   bFlag = GetPrinter( hPrinter, 2, 0, 0, &dwNeeded );

   if( ( ! bFlag ) && ( ( GetLastError() != ERROR_INSUFFICIENT_BUFFER ) || ( dwNeeded == 0 ) ) )
   {
      ClosePrinter( hPrinter );
#ifdef _ERRORMSG_
      MessageBox( 0, TEXT( "Printer Configuration Failed! (002)" ), TEXT( "Error!" ), MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL );
#endif
      hb_reta( 4 );
      HB_STORVNL( 0, -1, 1 );
      HB_STORC( "", -1, 2 );
      HB_STORNI( 0, -1, 3 );
      HB_STORNI( 0, -1, 4 );

      return;
   }

   pi2 = ( PRINTER_INFO_2 * ) GlobalAlloc( GPTR, dwNeeded );

   if( pi2 == NULL )
   {
      ClosePrinter( hPrinter );
#ifdef _ERRORMSG_
      MessageBox( 0, TEXT( "Printer Configuration Failed! (003)" ), TEXT( "Error!" ), MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL );
#endif
      hb_reta( 4 );
      HB_STORVNL( 0, -1, 1 );
      HB_STORC( "", -1, 2 );
      HB_STORNI( 0, -1, 3 );
      HB_STORNI( 0, -1, 4 );

      return;
   }

   bFlag = GetPrinter( hPrinter, 2, ( LPBYTE ) pi2, dwNeeded, &dwNeeded );

   if( ! bFlag )
   {
      GlobalFree( pi2 );
      ClosePrinter( hPrinter );
#ifdef _ERRORMSG_
      MessageBox( 0, TEXT( "Printer Configuration Failed! (004)" ), TEXT( "Error!" ), MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL );
#endif
      hb_reta( 4 );
      HB_STORVNL( 0, -1, 1 );
      HB_STORC( "", -1, 2 );
      HB_STORNI( 0, -1, 3 );
      HB_STORNI( 0, -1, 4 );

      return;
   }

   if( pi2->pDevMode == NULL )
   {
#ifndef UNICODE
      dwNeeded = DocumentProperties( NULL, hPrinter, ( LPSTR ) hb_parc( 1 ), NULL, NULL, 0 );
#else
      pDeviceName = AnsiToWide( hb_parc( 1 ) );
      dwNeeded    = DocumentProperties( NULL, hPrinter, pDeviceName, NULL, NULL, 0 );
      hb_xfree( pDeviceName );
#endif
      if( dwNeeded > 0 )
         pDevMode = ( DEVMODE * ) GlobalAlloc( GPTR, dwNeeded );
      else
      {
         GlobalFree( pi2 );
         ClosePrinter( hPrinter );
#ifdef _ERRORMSG_
         MessageBox( 0, TEXT( "Printer Configuration Failed! (005)" ), TEXT( "Error!" ), MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL );
#endif
         hb_reta( 4 );
         HB_STORVNL( 0, -1, 1 );
         HB_STORC( "", -1, 2 );
         HB_STORNI( 0, -1, 3 );
         HB_STORNI( 0, -1, 4 );

         return;
      }

      if( pDevMode == NULL )
      {
         GlobalFree( pi2 );
         ClosePrinter( hPrinter );
#ifdef _ERRORMSG_
         MessageBox( 0, TEXT( "Printer Configuration Failed! (006)" ), TEXT( "Error! (006)" ), MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL );
#endif
         hb_reta( 4 );
         HB_STORVNL( 0, -1, 1 );
         HB_STORC( "", -1, 2 );
         HB_STORNI( 0, -1, 3 );
         HB_STORNI( 0, -1, 4 );

         return;
      }

#ifndef UNICODE
      lFlag = DocumentProperties( NULL, hPrinter, ( LPSTR ) hb_parc( 1 ), pDevMode, NULL, DM_OUT_BUFFER );
#else
      pDeviceName = AnsiToWide( hb_parc( 1 ) );
      lFlag       = DocumentProperties( NULL, hPrinter, pDeviceName, pDevMode, NULL, DM_OUT_BUFFER );
      hb_xfree( pDeviceName );
#endif
      if( lFlag != IDOK || pDevMode == NULL )
      {
         GlobalFree( pDevMode );
         GlobalFree( pi2 );
         ClosePrinter( hPrinter );
#ifdef _ERRORMSG_
         MessageBox( 0, TEXT( "Printer Configuration Failed! (007)" ), TEXT( "Error!" ), MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL );
#endif
         hb_reta( 4 );
         HB_STORVNL( 0, -1, 1 );
         HB_STORC( "", -1, 2 );
         HB_STORNI( 0, -1, 3 );
         HB_STORNI( 0, -1, 4 );

         return;
      }

      pi2->pDevMode = pDevMode;
   }

   ///////////////////////////////////////////////////////////////////////
   // Specify Fields
   //////////////////////////////////////////////////////////////////////
   // Orientation
   if( hb_parni( 2 ) != -999 )
      fields = fields | DM_ORIENTATION;

   // PaperSize
   if( hb_parni( 3 ) != -999 )
      fields = fields | DM_PAPERSIZE;

   // PaperLength
   if( hb_parni( 4 ) != -999 )
      fields = fields | DM_PAPERLENGTH;

   // PaperWidth
   if( hb_parni( 5 ) != -999 )
      fields = fields | DM_PAPERWIDTH;

   // Copies
   if( hb_parni( 6 ) != -999 )
      fields = fields | DM_COPIES;

   // Default Source
   if( hb_parni( 7 ) != -999 )
      fields = fields | DM_DEFAULTSOURCE;

   // Print Quality
   if( hb_parni( 8 ) != -999 )
      fields = fields | DM_PRINTQUALITY;

   // Print Color
   if( hb_parni( 9 ) != -999 )
      fields = fields | DM_COLOR;

   // Print Duplex Mode
   if( hb_parni( 10 ) != -999 )
      fields = fields | DM_DUPLEX;

   // Print Collate
   if( hb_parni( 11 ) != -999 )
      fields = fields | DM_COLLATE;

   pi2->pDevMode->dmFields = fields;

   ///////////////////////////////////////////////////////////////////////
   // Load Fields
   //////////////////////////////////////////////////////////////////////
   // Orientation
   if( hb_parni( 2 ) != -999 )
   {
      if( ! ( pi2->pDevMode->dmFields & DM_ORIENTATION ) )
      {
#ifdef _ERRORMSG_
         MessageBox( 0, TEXT( "Printer Configuration Failed: ORIENTATION Property Not Supported By Selected Printer" ), TEXT( "Error!" ),
                     MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL );
#endif
         hb_reta( 4 );
         HB_STORVNL( 0, -1, 1 );
         HB_STORC( "", -1, 2 );
         HB_STORNI( 0, -1, 3 );
         HB_STORNI( 0, -1, 4 );

         return;
      }

      pi2->pDevMode->dmOrientation = hmg_par_short( 2 );
   }

   // PaperSize
   if( hb_parni( 3 ) != -999 )
   {
      if( ! ( pi2->pDevMode->dmFields & DM_PAPERSIZE ) )
      {
#ifdef _ERRORMSG_
         MessageBox( 0, TEXT( "Printer Configuration Failed: PAPERSIZE Property Not Supported By Selected Printer" ), TEXT( "Error!" ),
                     MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL );
#endif
         hb_reta( 4 );
         HB_STORVNL( 0, -1, 1 );
         HB_STORC( "", -1, 2 );
         HB_STORNI( 0, -1, 3 );
         HB_STORNI( 0, -1, 4 );

         return;
      }

      pi2->pDevMode->dmPaperSize = hmg_par_short( 3 );
   }

   // PaperLength
   if( hb_parni( 4 ) != -999 )
   {
      if( ! ( pi2->pDevMode->dmFields & DM_PAPERLENGTH ) )
      {
#ifdef _ERRORMSG_
         MessageBox( 0, TEXT( "Printer Configuration Failed: PAPERLENGTH Property Not Supported By Selected Printer" ), TEXT( "Error!" ),
                     MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL );
#endif
         hb_reta( 4 );
         HB_STORVNL( 0, -1, 1 );
         HB_STORC( "", -1, 2 );
         HB_STORNI( 0, -1, 3 );
         HB_STORNI( 0, -1, 4 );

         return;
      }

      pi2->pDevMode->dmPaperLength = ( short ) ( hb_parni( 4 ) * 10 );
   }

   // PaperWidth
   if( hb_parni( 5 ) != -999 )
   {
      if( ! ( pi2->pDevMode->dmFields & DM_PAPERWIDTH ) )
      {
#ifdef _ERRORMSG_
         MessageBox( 0, TEXT( "Printer Configuration Failed: PAPERWIDTH Property Not Supported By Selected Printer" ), TEXT( "Error!" ),
                     MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL );
#endif
         hb_reta( 4 );
         HB_STORVNL( 0, -1, 1 );
         HB_STORC( "", -1, 2 );
         HB_STORNI( 0, -1, 3 );
         HB_STORNI( 0, -1, 4 );

         return;
      }

      pi2->pDevMode->dmPaperWidth = ( short ) ( hb_parni( 5 ) * 10 );
   }

   // Copies
   if( hb_parni( 6 ) != -999 )
   {
      if( ! ( pi2->pDevMode->dmFields & DM_COPIES ) )
      {
#ifdef _ERRORMSG_
         MessageBox( 0, TEXT( "Printer Configuration Failed: COPIES Property Not Supported By Selected Printer" ), TEXT( "Error!" ), MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL );
#endif
         hb_reta( 4 );
         HB_STORVNL( 0, -1, 1 );
         HB_STORC( "", -1, 2 );
         HB_STORNI( 0, -1, 3 );
         HB_STORNI( 0, -1, 4 );

         return;
      }

      pi2->pDevMode->dmCopies = hmg_par_short( 6 );
   }

   // Default Source
   if( hb_parni( 7 ) != -999 )
   {
      if( ! ( pi2->pDevMode->dmFields & DM_DEFAULTSOURCE ) )
      {
#ifdef _ERRORMSG_
         MessageBox( 0, TEXT( "Printer Configuration Failed: DEFAULTSOURCE Property Not Supported By Selected Printer" ), TEXT( "Error!" ),
                     MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL );
#endif
         hb_reta( 4 );
         HB_STORVNL( 0, -1, 1 );
         HB_STORC( "", -1, 2 );
         HB_STORNI( 0, -1, 3 );
         HB_STORNI( 0, -1, 4 );

         return;
      }

      pi2->pDevMode->dmDefaultSource = hmg_par_short( 7 );
   }

   // Print Quality
   if( hb_parni( 8 ) != -999 )
   {
      if( ! ( pi2->pDevMode->dmFields & DM_PRINTQUALITY ) )
      {
#ifdef _ERRORMSG_
         MessageBox( 0, TEXT( "Printer Configuration Failed: QUALITY Property Not Supported By Selected Printer" ), TEXT( "Error!" ),
                     MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL );
#endif
         hb_reta( 4 );
         HB_STORVNL( 0, -1, 1 );
         HB_STORC( "", -1, 2 );
         HB_STORNI( 0, -1, 3 );
         HB_STORNI( 0, -1, 4 );

         return;
      }

      pi2->pDevMode->dmPrintQuality = hmg_par_short( 8 );
   }

   // Print Color
   if( hb_parni( 9 ) != -999 )
   {
      if( ! ( pi2->pDevMode->dmFields & DM_COLOR ) )
      {
#ifdef _ERRORMSG_
         MessageBox( 0, TEXT( "Printer Configuration Failed: COLOR Property Not Supported By Selected Printer" ), TEXT( "Error!" ), MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL );
#endif
         hb_reta( 4 );
         HB_STORVNL( 0, -1, 1 );
         HB_STORC( "", -1, 2 );
         HB_STORNI( 0, -1, 3 );
         HB_STORNI( 0, -1, 4 );

         return;
      }

      pi2->pDevMode->dmColor = hmg_par_short( 9 );
   }

   // Print Duplex
   if( hb_parni( 10 ) != -999 )
   {
      if( ! ( pi2->pDevMode->dmFields & DM_DUPLEX ) )
      {
#ifdef _ERRORMSG_
         MessageBox( 0, TEXT( "Printer Configuration Failed: DUPLEX Property Not Supported By Selected Printer" ), TEXT( "Error!" ), MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL );
#endif
         hb_reta( 4 );
         HB_STORVNL( 0, -1, 1 );
         HB_STORC( "", -1, 2 );
         HB_STORNI( 0, -1, 3 );
         HB_STORNI( 0, -1, 4 );

         return;
      }

      pi2->pDevMode->dmDuplex = hmg_par_short( 10 );
   }

   // Print Collate
   if( hb_parni( 11 ) != -999 )
   {
      if( ! ( pi2->pDevMode->dmFields & DM_COLLATE ) )
      {
#ifdef _ERRORMSG_
         MessageBox( 0, TEXT( "Printer Configuration Failed: COLLATE Property Not Supported By Selected Printer" ), TEXT( "Error!" ),
                     MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL );
#endif
         hb_reta( 4 );
         HB_STORVNL( 0, -1, 1 );
         HB_STORC( "", -1, 2 );
         HB_STORNI( 0, -1, 3 );
         HB_STORNI( 0, -1, 4 );

         return;
      }

      pi2->pDevMode->dmCollate = hmg_par_short( 11 );
   }

   //////////////////////////////////////////////////////////////////////

   pi2->pSecurityDescriptor = NULL;

#ifndef UNICODE
   lFlag = DocumentProperties( NULL, hPrinter, ( LPSTR ) hb_parc( 1 ), pi2->pDevMode, pi2->pDevMode, DM_IN_BUFFER | DM_OUT_BUFFER );
#else
   pDeviceName = AnsiToWide( hb_parc( 1 ) );
   lFlag       = DocumentProperties( NULL, hPrinter, pDeviceName, pi2->pDevMode, pi2->pDevMode, DM_IN_BUFFER | DM_OUT_BUFFER );
   hb_xfree( pDeviceName );
#endif
   if( lFlag != IDOK )
   {
      GlobalFree( pi2 );
      ClosePrinter( hPrinter );
      if( pDevMode )
         GlobalFree( pDevMode );
#ifdef _ERRORMSG_
      MessageBox( 0, TEXT( "Printer Configuration Failed! (008)" ), TEXT( "Error!" ), MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL );
#endif
      hb_reta( 4 );
      HB_STORVNL( 0, -1, 1 );
      HB_STORC( "", -1, 2 );
      HB_STORNI( 0, -1, 3 );
      HB_STORNI( 0, -1, 4 );

      return;
   }

#ifdef UNICODE
   pwszDevice = AnsiToWide( ( char * ) hb_parc( 1 ) );
   hdcPrint   = CreateDC( NULL, pwszDevice, NULL, pi2->pDevMode );
#else
   hdcPrint = CreateDC( NULL, hb_parc( 1 ), NULL, pi2->pDevMode );
#endif

   if( hdcPrint != NULL )
   {
      hb_reta( 4 );
      HB_STORVNL( ( LONG_PTR ) hdcPrint, -1, 1 );
#ifndef UNICODE
      HB_STORC( hb_parc( 1 ), -1, 2 );
#else
      pStr = WideToAnsi( pwszDevice );
      HB_STORC( hb_parc( 1 ), -1, 2 );
      hb_xfree( pStr );
#endif
      HB_STORNI( ( INT ) pi2->pDevMode->dmCopies, -1, 3 );
      HB_STORNI( ( INT ) pi2->pDevMode->dmCollate, -1, 4 );
   }
   else
   {
      hb_reta( 4 );
      HB_STORVNL( 0, -1, 1 );
      HB_STORC( "", -1, 2 );
      HB_STORNI( 0, -1, 3 );
      HB_STORNI( 0, -1, 4 );
   }

#ifdef UNICODE
   hb_xfree( pwszDevice );
#endif

   if( pi2 )
      GlobalFree( pi2 );

   if( hPrinter )
      ClosePrinter( hPrinter );

   if( pDevMode )
      GlobalFree( pDevMode );

}

#if ! defined( __XHARBOUR__ ) && ! ( defined( __MINGW32__ ) || defined( __POCC__ ) )

HB_FUNC( GETDEFAULTPRINTER )
{

   OSVERSIONINFO    osvi;
   LPPRINTER_INFO_5 PrinterInfo;
   DWORD Needed, Returned;
   DWORD BufferSize = 254;

   TCHAR DefaultPrinter[ 254 ];

#ifdef UNICODE
   LPSTR pStr;
#endif

   osvi.dwOSVersionInfoSize = sizeof( OSVERSIONINFO );

   GetVersionEx( &osvi );

   if( osvi.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS )
   {

      EnumPrinters( PRINTER_ENUM_DEFAULT, NULL, 5, NULL, 0, &Needed, &Returned );
      PrinterInfo = ( LPPRINTER_INFO_5 ) LocalAlloc( LPTR, Needed );
      EnumPrinters( PRINTER_ENUM_DEFAULT, NULL, 5, ( LPBYTE ) PrinterInfo, Needed, &Needed, &Returned );
      lstrcpy( DefaultPrinter, PrinterInfo->pPrinterName );
      LocalFree( PrinterInfo );

   }
   else if( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT )
   {

      GetProfileString( TEXT( "windows" ), TEXT( "device" ), TEXT( "" ), DefaultPrinter, BufferSize );
      _tcstok( DefaultPrinter, TEXT( "," ) );

   }

#ifndef UNICODE
   hb_retc( DefaultPrinter );
#else
   pStr = WideToAnsi( DefaultPrinter );
   hb_retc( pStr );
   hb_xfree( pStr );
#endif

}

#endif

HB_FUNC( _HMG_PRINTER_STARTPAGE_PREVIEW )
{

#ifndef UNICODE
   LPSTR FileName = ( LPSTR ) hb_parc( 2 );
#else
   LPWSTR FileName = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif
   HDC  tmpDC;
   RECT emfrect;

   SetRect( &emfrect, 0, 0, GetDeviceCaps( hmg_par_raw_HDC( 1 ), HORZSIZE ) * 100, GetDeviceCaps( hmg_par_raw_HDC( 1 ), VERTSIZE ) * 100 );

   tmpDC = CreateEnhMetaFile( hmg_par_raw_HDC( 1 ), FileName, &emfrect, TEXT( "" ) );

   hmg_ret_raw_HDC( tmpDC );

}

HB_FUNC( _HMG_PRINTER_ENDPAGE_PREVIEW )
{
   DeleteEnhMetaFile( CloseEnhMetaFile( hmg_par_raw_HDC( 1 ) ) );
}

HB_FUNC( _HMG_PRINTER_SHOWPAGE )
{

   HENHMETAFILE hemf;

#ifndef UNICODE
   LPSTR FileName = ( LPSTR ) hb_parc( 1 );
#else
   LPWSTR FileName = AnsiToWide( ( char * ) hb_parc( 1 ) );
#endif
   HWND        hWnd       = hmg_par_raw_HWND( 2 );
   HDC         hDCPrinter = hmg_par_raw_HDC( 3 );
   RECT         rct;
   RECT         aux;
   int          zw;
   int          zh;
   int          ClientWidth;
   int          ClientHeight;
   int          xOffset;
   int          yOffset;
   PAINTSTRUCT  ps;
   HDC          hDC = BeginPaint( hWnd, &ps );

   hemf = GetEnhMetaFile( FileName );

   GetClientRect( hWnd, &rct );

   ClientWidth  = rct.right - rct.left;
   ClientHeight = rct.bottom - rct.top;

   zw = hb_parni( 5 ) * GetDeviceCaps( hDCPrinter, HORZSIZE ) / 750;
   zh = hb_parni( 5 ) * GetDeviceCaps( hDCPrinter, VERTSIZE ) / 750;

   xOffset = ( ClientWidth - ( GetDeviceCaps( hDCPrinter, HORZSIZE ) * hb_parni( 4 ) / 10000 ) ) / 2;
   yOffset = ( ClientHeight - ( GetDeviceCaps( hDCPrinter, VERTSIZE ) * hb_parni( 4 ) / 10000 ) ) / 2;

   SetRect( &rct,
            xOffset + hb_parni( 6 ) - zw,
            yOffset + hb_parni( 7 ) - zh,
            xOffset + ( GetDeviceCaps( hDCPrinter, HORZSIZE ) * hb_parni( 4 ) / 10000 ) + hb_parni( 6 ) + zw,
            yOffset + ( GetDeviceCaps( hDCPrinter, VERTSIZE ) * hb_parni( 4 ) / 10000 ) + hb_parni( 7 ) + zh
            );

   FillRect( hDC, &rct, ( HBRUSH ) RGB( 255, 255, 255 ) );

   PlayEnhMetaFile( hDC, hemf, &rct );

   // Remove prints outside printable area

   // Right
   aux.top    = 0;
   aux.left   = rct.right;
   aux.right  = ClientWidth;
   aux.bottom = ClientHeight;
   FillRect( hDC, &aux, ( HBRUSH ) GetStockObject( GRAY_BRUSH ) );

   // Bottom
   aux.top    = rct.bottom;
   aux.left   = 0;
   aux.right  = ClientWidth;
   aux.bottom = ClientHeight;
   FillRect( hDC, &aux, ( HBRUSH ) GetStockObject( GRAY_BRUSH ) );

   // Top
   aux.top    = 0;
   aux.left   = 0;
   aux.right  = ClientWidth;
   aux.bottom = yOffset + hb_parni( 7 ) - zh;
   FillRect( hDC, &aux, ( HBRUSH ) GetStockObject( GRAY_BRUSH ) );

   // Left
   aux.top    = 0;
   aux.left   = 0;
   aux.right  = xOffset + hb_parni( 6 ) - zw;
   aux.bottom = ClientHeight;
   FillRect( hDC, &aux, ( HBRUSH ) GetStockObject( GRAY_BRUSH ) );

   // Clean up

   DeleteEnhMetaFile( hemf );

   EndPaint( hWnd, &ps );

   hb_reta (4);
   HB_STORVNL ((LONG) rct.left,   -1, 1); 
   HB_STORVNL ((LONG) rct.top,    -1, 2); 
   HB_STORVNL ((LONG) rct.right,  -1, 3); 
   HB_STORVNL ((LONG) rct.bottom, -1, 4); 
}

HB_FUNC( _HMG_PRINTER_GETPAGEWIDTH )
{
   hb_retni( GetDeviceCaps( hmg_par_raw_HDC( 1 ), HORZSIZE ) );
}

HB_FUNC( _HMG_PRINTER_GETPAGEHEIGHT )
{
   hb_retni( GetDeviceCaps( hmg_par_raw_HDC( 1 ), VERTSIZE ) );
}

HB_FUNC( _HMG_PRINTER_PRINTPAGE )
{

#ifndef UNICODE
   LPSTR FileName = ( LPSTR ) hb_parc( 2 );
#else
   LPWSTR FileName = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif
   HENHMETAFILE hemf;

   RECT rect;

   hemf = GetEnhMetaFile( FileName );

   SetRect( &rect, 0, 0, GetDeviceCaps( hmg_par_raw_HDC( 1 ), HORZRES ), GetDeviceCaps( hmg_par_raw_HDC( 1 ), VERTRES ) );

   StartPage( hmg_par_raw_HDC( 1 ) );

   PlayEnhMetaFile( hmg_par_raw_HDC( 1 ), ( HENHMETAFILE ) hemf, &rect );

   EndPage( hmg_par_raw_HDC( 1 ) );

   DeleteEnhMetaFile( hemf );

}

HB_FUNC( _HMG_PRINTER_PREVIEW_ENABLESCROLLBARS )
{
   EnableScrollBar( hmg_par_raw_HWND( 1 ), SB_BOTH, ESB_ENABLE_BOTH  );
}

HB_FUNC( _HMG_PRINTER_PREVIEW_DISABLESCROLLBARS )
{
   EnableScrollBar( hmg_par_raw_HWND( 1 ), SB_BOTH, ESB_DISABLE_BOTH );
}

HB_FUNC( _HMG_PRINTER_PREVIEW_DISABLEHSCROLLBAR )
{
   EnableScrollBar( hmg_par_raw_HWND( 1 ), SB_HORZ, ESB_DISABLE_BOTH );
}

HB_FUNC( _HMG_PRINTER_GETPRINTERWIDTH )
{
   HDC hdc = hmg_par_raw_HDC( 1 );

   hb_retnl( GetDeviceCaps( hdc, HORZSIZE ) );
}

HB_FUNC( _HMG_PRINTER_GETPRINTERHEIGHT )
{
   HDC hdc = hmg_par_raw_HDC( 1 );

   hb_retnl( GetDeviceCaps( hdc, VERTSIZE ) );
}

HB_FUNC( _HMG_PRINTER_GETPRINTABLEAREAPHYSICALOFFSETX )
{
   HDC hdc = hmg_par_raw_HDC( 1 );

   hb_retnl( GetDeviceCaps( hdc, PHYSICALOFFSETX ) );
}

HB_FUNC( _HMG_PRINTER_GETPRINTABLEAREALOGPIXELSX )
{
   HDC hdc = hmg_par_raw_HDC( 1 );

   hb_retnl( GetDeviceCaps( hdc, LOGPIXELSX ) );
}

HB_FUNC( _HMG_PRINTER_GETPRINTABLEAREAPHYSICALOFFSETY )
{
   HDC hdc = hmg_par_raw_HDC( 1 );

   hb_retnl( GetDeviceCaps( hdc, PHYSICALOFFSETY ) );
}

HB_FUNC( _HMG_PRINTER_GETPRINTABLEAREALOGPIXELSY )
{
   HDC hdc = hmg_par_raw_HDC( 1 );

   hb_retnl( GetDeviceCaps( hdc, LOGPIXELSY ) );
}

HB_FUNC( _HMG_PRINTER_C_IMAGE )
{
   // 1: hDC
   // 2: Image File
   // 3: Row
   // 4: Col
   // 5: Height
   // 6: Width
   // 7: Stretch
   // 8: Transparent

   HDC hdcPrint = hmg_par_raw_HDC( 1 );

#ifndef UNICODE
   LPSTR FileName = ( LPSTR ) hb_parc( 2 );
#else
   LPWSTR FileName = AnsiToWide( ( char * ) hb_parc( 2 ) );
#endif
   BOOL    bBmpImage = TRUE;
   HBITMAP hBitmap;
   HRGN    hRgn;
   HDC     memDC;
   INT     nWidth, nHeight;
   POINT   Point;
   BITMAP  Bmp;
   int     r   = hb_parni( 3 ); // Row
   int     c   = hb_parni( 4 ); // Col
   int     odr = hb_parni( 5 ); // Height
   int     odc = hb_parni( 6 ); // Width
   int     dr;
   int     dc;

   if( hdcPrint != NULL )
   {
      c  = ( c * GetDeviceCaps( hdcPrint, LOGPIXELSX ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETX );
      r  = ( r * GetDeviceCaps( hdcPrint, LOGPIXELSY ) / 1000 ) - GetDeviceCaps( hdcPrint, PHYSICALOFFSETY );
      dc = ( odc * GetDeviceCaps( hdcPrint, LOGPIXELSX ) / 1000 );
      dr = ( odr * GetDeviceCaps( hdcPrint, LOGPIXELSY ) / 1000 );

      hBitmap = ( HBITMAP ) LoadImage( GetInstance(), FileName, IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION );

      if( hBitmap == NULL )
         hBitmap = ( HBITMAP ) LoadImage( NULL, FileName, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE | LR_CREATEDIBSECTION );

      if( hBitmap == NULL )
      {
         bBmpImage = FALSE;
         hBitmap   = HMG_LoadImage( ( char * ) hb_parc( 2 ) );
      }
      if( hBitmap == NULL )
         return;

      GetObject( hBitmap, sizeof( BITMAP ), &Bmp );
      nWidth  = Bmp.bmWidth;
      nHeight = Bmp.bmHeight;

      if( ! hb_parl( 7 ) ) // Scale
      {
         if( odr * nHeight / nWidth <= odr )
            dr = odc * GetDeviceCaps( hdcPrint, LOGPIXELSY ) / 1000 * nHeight / nWidth;
         else
            dc = odr * GetDeviceCaps( hdcPrint, LOGPIXELSX ) / 1000 * nWidth / nHeight;
      }

      GetViewportOrgEx( hdcPrint, &Point );

      hRgn = CreateRectRgn( c + Point.x,
                            r + Point.y,
                            c + dc + Point.x - 1,
                            r + dr + Point.y - 1 );

      SelectClipRgn( hdcPrint, hRgn );

      if( ! bBmpImage )
      {
         if( hb_parl( 7 ) )             // Stretch
            SetStretchBltMode( hdcPrint, COLORONCOLOR );
         else
         {
            GetBrushOrgEx( hdcPrint, &Point );
            SetStretchBltMode( hdcPrint, HALFTONE );
            SetBrushOrgEx( hdcPrint, Point.x, Point.y, NULL );
         }
      }

      memDC = CreateCompatibleDC( hdcPrint );
      SelectObject( memDC, hBitmap );

      if( hb_parl( 8 ) && ! bBmpImage ) // Transparent
         TransparentBlt( hdcPrint, c, r, dc, dr, memDC, 0, 0, nWidth, nHeight, GetPixel( memDC, 0, 0 ) );
      else
         StretchBlt( hdcPrint, c, r, dc, dr, memDC, 0, 0, nWidth, nHeight, SRCCOPY );

      SelectClipRgn( hdcPrint, NULL );

      DeleteObject( hBitmap );
      DeleteDC( memDC );
   }
}

//  GetJobInfo ( cPrinterName, nJobID ) --> { nJobID, cPrinterName, cMachineName, cUserName, cDocument, cDataType, cStatus, nStatus
//                                            nPriorityLevel, nPositionPrintQueue, nTotalPages, nPagesPrinted, cLocalDate, cLocalTime }
HB_FUNC( _HMG_PRINTGETJOBINFO )
{

#ifndef UNICODE
   LPSTR cPrinterName = ( LPSTR ) hb_parc( 1 );
#else
   LPWSTR cPrinterName = AnsiToWide( ( char * ) hb_parc( 1 ) );
   LPSTR  pStr;
#endif
   DWORD      nJobID   = hmg_par_DWORD( 2 );
   HANDLE     hPrinter     = NULL;
   TCHAR      cDateTime[ 256 ];
   SYSTEMTIME LocalSystemTime;

   if( OpenPrinter( cPrinterName, &hPrinter, NULL ) )
   {
      DWORD        nBytesNeeded = 0;
      DWORD        nBytesUsed   = 0;
      JOB_INFO_1 * Job_Info_1;

      GetJob( hPrinter, nJobID, 1, NULL, 0, &nBytesNeeded );

      if( nBytesNeeded > 0 )
      {
         Job_Info_1 = ( JOB_INFO_1 * ) hb_xgrab( nBytesNeeded );
         ZeroMemory( Job_Info_1, nBytesNeeded );

         if( GetJob( hPrinter, nJobID, 1, ( LPBYTE ) Job_Info_1, nBytesNeeded, &nBytesUsed ) )
         {
            hb_reta( 14 );
            HB_STORNI( ( INT ) Job_Info_1->JobId, -1, 1 );
#ifndef UNICODE
            HB_STORC(      Job_Info_1->pPrinterName, -1, 2 );
            HB_STORC(      Job_Info_1->pMachineName, -1, 3 );
            HB_STORC(      Job_Info_1->pUserName, -1, 4 );
            HB_STORC(      Job_Info_1->pDocument, -1, 5 );
            HB_STORC(      Job_Info_1->pDatatype, -1, 6 );
            HB_STORC(      Job_Info_1->pStatus, -1, 7 );
#else
            pStr = WideToAnsi( Job_Info_1->pPrinterName );
            HB_STORC(      pStr, -1, 2 );
            hb_xfree( pStr );
            pStr = WideToAnsi( Job_Info_1->pMachineName );
            HB_STORC(      pStr, -1, 3 );
            hb_xfree( pStr );
            pStr = WideToAnsi( Job_Info_1->pUserName );
            HB_STORC(      pStr, -1, 4 );
            hb_xfree( pStr );
            pStr = WideToAnsi( Job_Info_1->pDocument );
            HB_STORC(      pStr, -1, 5 );
            hb_xfree( pStr );
            pStr = WideToAnsi( Job_Info_1->pDatatype );
            HB_STORC(      pStr, -1, 6 );
            hb_xfree( pStr );
            pStr = WideToAnsi( Job_Info_1->pStatus );
            HB_STORC(      pStr, -1, 7 );
            hb_xfree( pStr );
#endif
            HB_STORNI( ( INT ) Job_Info_1->Status, -1, 8 );
            HB_STORNI( ( INT ) Job_Info_1->Priority, -1, 9 );
            HB_STORNI( ( INT ) Job_Info_1->Position, -1, 10 );
            HB_STORNI( ( INT ) Job_Info_1->TotalPages, -1, 11 );
            HB_STORNI( ( INT ) Job_Info_1->PagesPrinted, -1, 12 );

            SystemTimeToTzSpecificLocalTime( NULL, &Job_Info_1->Submitted, &LocalSystemTime );

            wsprintf( cDateTime, TEXT( "%02d/%02d/%02d" ), LocalSystemTime.wYear, LocalSystemTime.wMonth, LocalSystemTime.wDay );
#ifndef UNICODE
            HB_STORC( cDateTime, -1, 13 );
#else
            pStr = WideToAnsi( cDateTime );
            HB_STORC( pStr, -1, 13 );
            hb_xfree( pStr );
#endif

            wsprintf( cDateTime, TEXT( "%02d:%02d:%02d" ), LocalSystemTime.wHour, LocalSystemTime.wMinute, LocalSystemTime.wSecond );
#ifndef UNICODE
            HB_STORC( cDateTime, -1, 14 );
#else
            pStr = WideToAnsi( cDateTime );
            HB_STORC( pStr, -1, 14 );
            hb_xfree( pStr );
#endif
         }
         else
            hb_reta( 0 );

         if( Job_Info_1 )
            hb_xfree( ( void * ) Job_Info_1 );
      }
      else
         hb_reta( 0 );

      ClosePrinter( hPrinter );
   }
   else
      hb_reta( 0 );
}

HB_FUNC( _HMG_PRINTERGETSTATUS )
{

#ifndef UNICODE
   LPSTR cPrinterName = ( LPSTR ) hb_parc( 1 );
#else
   LPWSTR cPrinterName = AnsiToWide( ( char * ) hb_parc( 1 ) );
#endif
   HANDLE hPrinter     = NULL;
   DWORD  nBytesNeeded = 0;
   DWORD  nBytesUsed   = 0;
   PRINTER_INFO_6 * Printer_Info_6;

   if( OpenPrinter( cPrinterName, &hPrinter, NULL ) )
   {
      GetPrinter( hPrinter, 6, NULL, 0, &nBytesNeeded );
      if( nBytesNeeded > 0 )
      {
         Printer_Info_6 = ( PRINTER_INFO_6 * ) hb_xgrab( nBytesNeeded );
         ZeroMemory( Printer_Info_6, nBytesNeeded );

         if( GetPrinter( hPrinter, 6, ( LPBYTE ) Printer_Info_6, nBytesNeeded, &nBytesUsed ) )
            hb_retnl( Printer_Info_6->dwStatus );
         else
            hb_retnl( PRINTER_STATUS_NOT_AVAILABLE );

         if( Printer_Info_6 )
            hb_xfree( ( void * ) Printer_Info_6 );
      }
      else
         hb_retnl( PRINTER_STATUS_NOT_AVAILABLE );

      ClosePrinter( hPrinter );
   }
   else
      hb_retnl( PRINTER_STATUS_NOT_AVAILABLE );
}

HB_FUNC( GETTEXTALIGN )
{
   hb_retni( GetTextAlign( hmg_par_raw_HDC( 1 ) ) );
}

HB_FUNC( SETTEXTALIGN )
{
   hb_retni( SetTextAlign( hmg_par_raw_HDC( 1 ), hmg_par_UINT( 2 ) ) );
}

static HBITMAP loademffile( TCHAR * filename, int width, int height, HWND handle, int scalestrech, int whitebackground );

HB_FUNC( INITEMFFILE )
{

   DWORD Style = WS_CHILD | SS_BITMAP;

   if( ! hb_parl( 5 ) )
      Style |= WS_VISIBLE;

   if( hb_parl( 6 ) )
      Style |= SS_NOTIFY;

   hmg_ret_raw_HWND( CreateWindowEx( 0, WC_STATIC, NULL, Style, hb_parni( 3 ), hb_parni( 4 ), 0, 0, hmg_par_raw_HWND( 1 ), hmg_par_raw_HMENU( 2 ), GetInstance(), NULL ) );

}

HB_FUNC( C_SETEMFFILE )
{

#ifndef UNICODE
   TCHAR * cFileName = ( TCHAR * ) hb_parc( 2 );
#else
   TCHAR * cFileName = ( TCHAR * ) hb_osStrU16Encode( hb_parc( 2 ) );
#endif
   HBITMAP hBitmap;

   if( hb_parclen( 2 ) == 0 )
      hmg_ret_raw_HANDLE( NULL );

   hBitmap = loademffile( cFileName, hb_parni( 3 ), hb_parni( 4 ), hmg_par_raw_HWND( 1 ), hb_parni( 5 ), hb_parni( 6 ) );

   if( hBitmap != NULL )
      SendMessage( hmg_par_raw_HWND( 1 ), ( UINT ) STM_SETIMAGE, ( WPARAM ) IMAGE_BITMAP, ( LPARAM ) hBitmap );

   hmg_ret_raw_HANDLE( hBitmap );

}

static BOOL read_image( TCHAR * filename, DWORD * nFileSize, HGLOBAL * hMem )
{
   HANDLE hFile;
   LPVOID lpDest;
   DWORD  dwFileSize;
   DWORD  dwBytesRead = 0;
   BOOL   bRead;

   // open the file
   hFile = CreateFile( filename, GENERIC_READ, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL );
   if( hFile == INVALID_HANDLE_VALUE )
      return FALSE;
   // we will read the whole file in global memory, find the size first
   dwFileSize = GetFileSize( hFile, NULL );
   // allocate memory to read the whole file
   if( dwFileSize == INVALID_FILE_SIZE || ( *hMem = GlobalAlloc( GHND, dwFileSize ) ) == NULL )
   {
      CloseHandle( hFile );
      return FALSE;
   }
   *nFileSize = dwFileSize;
   // lock memory for image
   lpDest = GlobalLock( *hMem );
   if( lpDest == NULL )
   {
      GlobalFree( *hMem );
      CloseHandle( hFile );
      return FALSE;
   }
   // read file and store in global memory
   bRead = ReadFile( hFile, lpDest, dwFileSize, &dwBytesRead, NULL );

   GlobalUnlock( *hMem );
   CloseHandle( hFile );

   if( ! bRead )
   {
      GlobalFree( *hMem );
      return FALSE;
   }
   return TRUE;
}

static void calc_rect( HWND handle, int width, int height, int scalestrech, LONG lWidth, LONG lHeight, RECT * rect, RECT * rect2 )
{
   if( width == 0 && height == 0 )
      GetClientRect( handle, rect );
   else
      SetRect( rect, 0, 0, width, height );

   SetRect( rect2, 0, 0, rect->right, rect->bottom );

   if( scalestrech == 0 )
   {
      if( ( int ) lWidth * rect->bottom / lHeight <= rect->right )
         rect->right = ( int ) lWidth * rect->bottom / lHeight;
      else
         rect->bottom = ( int ) lHeight * rect->right / lWidth;
   }

   rect->left = ( int ) ( width - rect->right ) / 2;
   rect->top  = ( int ) ( height - rect->bottom ) / 2;
}

static HBITMAP loademffile( TCHAR * filename, int width, int height, HWND handle, int scalestrech, int whitebackground )
{
   IStream *  iStream;
   IPicture * iPicture = NULL;
   HGLOBAL    hMem     = ( HGLOBAL ) NULL;
   HRESULT    hr;
   DWORD      nFileSize = 0;
   RECT       rect, rect2;
   HBITMAP    bitmap;
   LONG       lWidth, lHeight;
   HDC        imgDC = GetDC( handle );
   HDC        tmpDC;

   if( read_image( filename, &nFileSize, &hMem ) == FALSE )
   {
      ReleaseDC( handle, imgDC );
      return NULL;
   }
   // don't delete memory on object's release
   hr = CreateStreamOnHGlobal( hMem, FALSE, &iStream );
   if( hr != S_OK || iStream == NULL )
   {
      GlobalFree( hMem );
      ReleaseDC( handle, imgDC );
      return NULL;
   }
   // Load from stream
#if defined( __cplusplus )
   hr = OleLoadPicture( iStream, nFileSize, ( nFileSize == 0 ), IID_IPicture, ( LPVOID * ) &iPicture );
#else
   hr = OleLoadPicture( iStream, nFileSize, ( nFileSize == 0 ), &IID_IPicture, ( LPVOID * ) &iPicture );
   iStream->lpVtbl->Release( iStream );
#endif
   if( hr != S_OK || iPicture == NULL )
   {
      GlobalFree( hMem );
      ReleaseDC( handle, imgDC );
      return NULL;
   }

   iPicture->lpVtbl->get_Width( iPicture, &lWidth );
   iPicture->lpVtbl->get_Height( iPicture, &lHeight );

   calc_rect( handle, width, height, scalestrech, lWidth, lHeight, &rect, &rect2 );

   tmpDC  = CreateCompatibleDC( imgDC );
   bitmap = CreateCompatibleBitmap( imgDC, width, height );
   SelectObject( tmpDC, bitmap );

   if( whitebackground == 1 )
      FillRect( tmpDC, &rect2, ( HBRUSH ) GetStockObject( WHITE_BRUSH ) );
   else
      FillRect( tmpDC, &rect2, ( HBRUSH ) GetSysColorBrush( COLOR_BTNFACE ) );

   // Render to device context
   iPicture->lpVtbl->Render( iPicture, tmpDC, rect.left, rect.top, rect.right, rect.bottom, 0, lHeight, lWidth, -lHeight, NULL );
   iPicture->lpVtbl->Release( iPicture );
   GlobalFree( hMem );

   DeleteDC( tmpDC );
   ReleaseDC( handle, imgDC );

   return bitmap;
}

HB_FUNC( ISWIN8ORLATER )
{
   OSVERSIONINFO osvi;

   osvi.dwOSVersionInfoSize = sizeof( OSVERSIONINFO );
   GetVersionEx( ( OSVERSIONINFO * ) &osvi );

   hb_retl( osvi.dwMajorVersion >= 6 && osvi.dwMinorVersion > 1 );
}

