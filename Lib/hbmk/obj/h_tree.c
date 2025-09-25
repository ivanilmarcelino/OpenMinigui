/*
 * Harbour 3.2.0dev (r2507191744)
 * MinGW GNU C 15.2 (32-bit)
 * Generated C source from "h_tree.prg"
 */

#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"
#include "hbxvm.h"


HB_FUNC( _DEFINETREE );
HB_FUNC_EXTERN( ODLU2PIXEL );
HB_FUNC_EXTERN( GETFONTHANDLE );
HB_FUNC_EXTERN( GETFONTPARAMBYREF );
HB_FUNC_EXTERN( __DEFAULTNIL );
HB_FUNC_EXTERN( _SETGETGLOBAL );
HB_FUNC_EXTERN( _ISWINDOWDEFINED );
HB_FUNC_EXTERN( MSGMINIGUIERROR );
HB_FUNC_EXTERN( HB_ISSTRING );
HB_FUNC_EXTERN( HMG_GETUNIQUENAME );
HB_FUNC_EXTERN( _ISCONTROLDEFINED );
HB_FUNC_EXTERN( HB_ISNUMERIC );
HB_FUNC_EXTERN( HB_DEFAULT );
HB_FUNC_EXTERN( _GETCONTROLFREE );
HB_FUNC_EXTERN( HB_ISLOGICAL );
HB_FUNC_EXTERN( LEN );
HB_FUNC( INITDIALOGTREE );
HB_FUNC_EXTERN( AADD );
HB_FUNC_EXTERN( GETDIALOGITEMHANDLE );
HB_FUNC_EXTERN( SETWINDOWSTYLE );
HB_FUNC_EXTERN( GETWINDOWRECT );
HB_FUNC_EXTERN( HB_ISARRAY );
HB_FUNC_EXTERN( INITTREEVIEWBITMAP );
HB_FUNC_EXTERN( GETFORMHANDLE );
HB_FUNC_EXTERN( GETFORMINDEX );
HB_FUNC_EXTERN( INITTREE );
HB_FUNC_EXTERN( ADDSPLITBOXITEM );
HB_FUNC_EXTERN( _SETFONTHANDLE );
HB_FUNC_EXTERN( _SETFONT );
HB_FUNC_EXTERN( SETTOOLTIP );
HB_FUNC_EXTERN( GETFORMTOOLTIPHANDLE );
HB_FUNC_EXTERN( SENDMESSAGE );
HB_FUNC_EXTERN( _SETNAMELIST );
HB_FUNC_EXTERN( _WINDOWOBJ );
HB_FUNC_EXTERN( _CONTROLOBJ );
HB_FUNC_EXTERN( DO_CONTROLEVENTPROCEDURE );
HB_FUNC_EXTERN( ADDTREEVIEWBITMAP );
HB_FUNC_EXTERN( ADDTREEITEM );
HB_FUNC_EXTERN( ACLONE );
HB_FUNC_EXTERN( TREEVIEW_SELECTITEM );
HB_FUNC_EXTERN( ASCAN );
HB_FUNC( _DEFINETREENODE );
HB_FUNC( _ENDTREENODE );
HB_FUNC( _DEFINETREEITEM );
HB_FUNC( _ENDTREE );
HB_FUNC_EXTERN( TREEVIEW_GETCOUNT );
HB_FUNC( _COLLAPSE );
HB_FUNC_EXTERN( GETCONTROLINDEX );
HB_FUNC( TREEITEMGETHANDLE );
HB_FUNC_EXTERN( TREEVIEW_EXPANDCHILDRENRECURSIVE );
HB_FUNC_EXTERN( HB_DEFAULTVALUE );
HB_FUNC( _EXPAND );
HB_FUNC( TREEITEMGETROOTVALUE );
HB_FUNC_EXTERN( GETCONTROLHANDLE );
HB_FUNC_EXTERN( TREEVIEW_GETROOT );
HB_FUNC_EXTERN( TREEITEM_GETID );
HB_FUNC( TREEITEMGETPARENTVALUE );
HB_FUNC_EXTERN( TREEVIEW_GETPARENT );
HB_FUNC( TREEITEMGETALLVALUES );
HB_FUNC_EXTERN( AEVAL );
HB_FUNC_EXTERN( EMPTY );
HB_FUNC( TREEITEMGETCHILDVALUES );
HB_FUNC_EXTERN( TREEVIEW_GETCHILD );
HB_FUNC_EXTERN( TREEVIEW_GETNEXTSIBLING );
HB_FUNC( TREEITEMGETSIBLINGVALUES );
HB_FUNC_EXTERN( TREEVIEW_GETPREVSIBLING );
HB_FUNC( TREEITEMGETFIRSTITEMVALUE );
HB_FUNC_EXTERN( GETPROPERTY );
HB_FUNC( TREEITEMSORT );
HB_FUNC_EXTERN( TREEVIEW_SORTCHILDRENRECURSIVECB );
HB_FUNC( TREEITEMISTRUENODE );
HB_FUNC( TREEITEMSETNODEFLAG );
HB_FUNC_EXTERN( TREEITEM_SETNODEFLAG );
HB_FUNC( TREEITEMGETNODEFLAG );
HB_FUNC_EXTERN( TREEITEM_GETNODEFLAG );
HB_FUNC( TREEITEMSETIMAGEINDEX );
HB_FUNC_EXTERN( TREEITEM_SETIMAGEINDEX );
HB_FUNC( TREEITEMGETIMAGEINDEX );
HB_FUNC_EXTERN( TREEITEM_GETIMAGEINDEX );
HB_FUNC( TREEITEMISEXPAND );
HB_FUNC_EXTERN( AND );
HB_FUNC_EXTERN( TREEVIEW_GETITEMSTATE );
HB_FUNC( TREENODEITEMCARGO );
HB_FUNC_EXTERN( PCOUNT );


HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_H_TREE )
{ "_DEFINETREE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINETREE )}, NULL },
{ "ODLU2PIXEL", {HB_FS_PUBLIC}, {HB_FUNCNAME( ODLU2PIXEL )}, NULL },
{ "_HMG_SYSDATA", {HB_FS_PUBLIC | HB_FS_MEMVAR}, {NULL}, NULL },
{ "GETFONTHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFONTHANDLE )}, NULL },
{ "GETFONTPARAMBYREF", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFONTPARAMBYREF )}, NULL },
{ "__DEFAULTNIL", {HB_FS_PUBLIC}, {HB_FUNCNAME( __DEFAULTNIL )}, NULL },
{ "_SETGETGLOBAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETGETGLOBAL )}, NULL },
{ "_ISWINDOWDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISWINDOWDEFINED )}, NULL },
{ "MSGMINIGUIERROR", {HB_FS_PUBLIC}, {HB_FUNCNAME( MSGMINIGUIERROR )}, NULL },
{ "HB_ISSTRING", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISSTRING )}, NULL },
{ "HMG_GETUNIQUENAME", {HB_FS_PUBLIC}, {HB_FUNCNAME( HMG_GETUNIQUENAME )}, NULL },
{ "_ISCONTROLDEFINED", {HB_FS_PUBLIC}, {HB_FUNCNAME( _ISCONTROLDEFINED )}, NULL },
{ "HB_ISNUMERIC", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISNUMERIC )}, NULL },
{ "HB_DEFAULT", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULT )}, NULL },
{ "_GETCONTROLFREE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _GETCONTROLFREE )}, NULL },
{ "HB_ISLOGICAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISLOGICAL )}, NULL },
{ "LEN", {HB_FS_PUBLIC}, {HB_FUNCNAME( LEN )}, NULL },
{ "INITDIALOGTREE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( INITDIALOGTREE )}, NULL },
{ "AADD", {HB_FS_PUBLIC}, {HB_FUNCNAME( AADD )}, NULL },
{ "GETDIALOGITEMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETDIALOGITEMHANDLE )}, NULL },
{ "SETWINDOWSTYLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETWINDOWSTYLE )}, NULL },
{ "GETWINDOWRECT", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETWINDOWRECT )}, NULL },
{ "HB_ISARRAY", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_ISARRAY )}, NULL },
{ "INITTREEVIEWBITMAP", {HB_FS_PUBLIC}, {HB_FUNCNAME( INITTREEVIEWBITMAP )}, NULL },
{ "GETFORMHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMHANDLE )}, NULL },
{ "GETFORMINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMINDEX )}, NULL },
{ "INITTREE", {HB_FS_PUBLIC}, {HB_FUNCNAME( INITTREE )}, NULL },
{ "ADDSPLITBOXITEM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ADDSPLITBOXITEM )}, NULL },
{ "_SETFONTHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFONTHANDLE )}, NULL },
{ "_SETFONT", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETFONT )}, NULL },
{ "SETTOOLTIP", {HB_FS_PUBLIC}, {HB_FUNCNAME( SETTOOLTIP )}, NULL },
{ "GETFORMTOOLTIPHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETFORMTOOLTIPHANDLE )}, NULL },
{ "SENDMESSAGE", {HB_FS_PUBLIC}, {HB_FUNCNAME( SENDMESSAGE )}, NULL },
{ "_SETNAMELIST", {HB_FS_PUBLIC}, {HB_FUNCNAME( _SETNAMELIST )}, NULL },
{ "EVAL", {HB_FS_PUBLIC | HB_FS_MESSAGE}, {NULL}, NULL },
{ "_WINDOWOBJ", {HB_FS_PUBLIC}, {HB_FUNCNAME( _WINDOWOBJ )}, NULL },
{ "_CONTROLOBJ", {HB_FS_PUBLIC}, {HB_FUNCNAME( _CONTROLOBJ )}, NULL },
{ "DO_CONTROLEVENTPROCEDURE", {HB_FS_PUBLIC}, {HB_FUNCNAME( DO_CONTROLEVENTPROCEDURE )}, NULL },
{ "ADDTREEVIEWBITMAP", {HB_FS_PUBLIC}, {HB_FUNCNAME( ADDTREEVIEWBITMAP )}, NULL },
{ "ADDTREEITEM", {HB_FS_PUBLIC}, {HB_FUNCNAME( ADDTREEITEM )}, NULL },
{ "ACLONE", {HB_FS_PUBLIC}, {HB_FUNCNAME( ACLONE )}, NULL },
{ "TREEVIEW_SELECTITEM", {HB_FS_PUBLIC}, {HB_FUNCNAME( TREEVIEW_SELECTITEM )}, NULL },
{ "ASCAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( ASCAN )}, NULL },
{ "_DEFINETREENODE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINETREENODE )}, NULL },
{ "_ENDTREENODE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _ENDTREENODE )}, NULL },
{ "_DEFINETREEITEM", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _DEFINETREEITEM )}, NULL },
{ "_ENDTREE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _ENDTREE )}, NULL },
{ "TREEVIEW_GETCOUNT", {HB_FS_PUBLIC}, {HB_FUNCNAME( TREEVIEW_GETCOUNT )}, NULL },
{ "_COLLAPSE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _COLLAPSE )}, NULL },
{ "GETCONTROLINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLINDEX )}, NULL },
{ "TREEITEMGETHANDLE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( TREEITEMGETHANDLE )}, NULL },
{ "TREEVIEW_EXPANDCHILDRENRECURSIVE", {HB_FS_PUBLIC}, {HB_FUNCNAME( TREEVIEW_EXPANDCHILDRENRECURSIVE )}, NULL },
{ "HB_DEFAULTVALUE", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_DEFAULTVALUE )}, NULL },
{ "_EXPAND", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( _EXPAND )}, NULL },
{ "TREEITEMGETROOTVALUE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( TREEITEMGETROOTVALUE )}, NULL },
{ "GETCONTROLHANDLE", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETCONTROLHANDLE )}, NULL },
{ "TREEVIEW_GETROOT", {HB_FS_PUBLIC}, {HB_FUNCNAME( TREEVIEW_GETROOT )}, NULL },
{ "TREEITEM_GETID", {HB_FS_PUBLIC}, {HB_FUNCNAME( TREEITEM_GETID )}, NULL },
{ "TREEITEMGETPARENTVALUE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( TREEITEMGETPARENTVALUE )}, NULL },
{ "TREEVIEW_GETPARENT", {HB_FS_PUBLIC}, {HB_FUNCNAME( TREEVIEW_GETPARENT )}, NULL },
{ "TREEITEMGETALLVALUES", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( TREEITEMGETALLVALUES )}, NULL },
{ "AEVAL", {HB_FS_PUBLIC}, {HB_FUNCNAME( AEVAL )}, NULL },
{ "EMPTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( EMPTY )}, NULL },
{ "TREEITEMGETCHILDVALUES", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( TREEITEMGETCHILDVALUES )}, NULL },
{ "TREEVIEW_GETCHILD", {HB_FS_PUBLIC}, {HB_FUNCNAME( TREEVIEW_GETCHILD )}, NULL },
{ "TREEVIEW_GETNEXTSIBLING", {HB_FS_PUBLIC}, {HB_FUNCNAME( TREEVIEW_GETNEXTSIBLING )}, NULL },
{ "TREEITEMGETSIBLINGVALUES", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( TREEITEMGETSIBLINGVALUES )}, NULL },
{ "TREEVIEW_GETPREVSIBLING", {HB_FS_PUBLIC}, {HB_FUNCNAME( TREEVIEW_GETPREVSIBLING )}, NULL },
{ "TREEITEMGETFIRSTITEMVALUE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( TREEITEMGETFIRSTITEMVALUE )}, NULL },
{ "GETPROPERTY", {HB_FS_PUBLIC}, {HB_FUNCNAME( GETPROPERTY )}, NULL },
{ "TREEITEMSORT", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( TREEITEMSORT )}, NULL },
{ "TREEVIEW_SORTCHILDRENRECURSIVECB", {HB_FS_PUBLIC}, {HB_FUNCNAME( TREEVIEW_SORTCHILDRENRECURSIVECB )}, NULL },
{ "TREEITEMISTRUENODE", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( TREEITEMISTRUENODE )}, NULL },
{ "TREEITEMSETNODEFLAG", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( TREEITEMSETNODEFLAG )}, NULL },
{ "TREEITEM_SETNODEFLAG", {HB_FS_PUBLIC}, {HB_FUNCNAME( TREEITEM_SETNODEFLAG )}, NULL },
{ "TREEITEMGETNODEFLAG", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( TREEITEMGETNODEFLAG )}, NULL },
{ "TREEITEM_GETNODEFLAG", {HB_FS_PUBLIC}, {HB_FUNCNAME( TREEITEM_GETNODEFLAG )}, NULL },
{ "TREEITEMSETIMAGEINDEX", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( TREEITEMSETIMAGEINDEX )}, NULL },
{ "TREEITEM_SETIMAGEINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( TREEITEM_SETIMAGEINDEX )}, NULL },
{ "TREEITEMGETIMAGEINDEX", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( TREEITEMGETIMAGEINDEX )}, NULL },
{ "TREEITEM_GETIMAGEINDEX", {HB_FS_PUBLIC}, {HB_FUNCNAME( TREEITEM_GETIMAGEINDEX )}, NULL },
{ "TREEITEMISEXPAND", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( TREEITEMISEXPAND )}, NULL },
{ "AND", {HB_FS_PUBLIC}, {HB_FUNCNAME( AND )}, NULL },
{ "TREEVIEW_GETITEMSTATE", {HB_FS_PUBLIC}, {HB_FUNCNAME( TREEVIEW_GETITEMSTATE )}, NULL },
{ "TREENODEITEMCARGO", {HB_FS_PUBLIC | HB_FS_LOCAL}, {HB_FUNCNAME( TREENODEITEMCARGO )}, NULL },
{ "PCOUNT", {HB_FS_PUBLIC}, {HB_FUNCNAME( PCOUNT )}, NULL }
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_H_TREE, "h_tree.prg", 0x0, 0x0003 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_H_TREE
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_H_TREE )
   #include "hbiniseg.h"
#endif

HB_FUNC( _DEFINETREE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 14, 32 );
	hb_xvmSetLine( 59 );
	hb_xvmPushInteger( 4 );
	hb_xvmArrayDim( 1 );
	hb_xvmPopLocal( 38 );
	hb_xvmSetLine( 66 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 45 );
	hb_xvmPushNil();
	hb_xvmPopLocal( 46 );
	hb_xvmSetLine( 69 );
	hb_xvmPushFuncSymbol( symbols + 1 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 46 );
	hb_xvmSetLine( 72 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPop( 6L ) ) break;
	hb_xvmSetLine( 73 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPop( 8L ) ) break;
	hb_xvmSetLine( 74 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmArrayItemPop( 1L ) ) break;
	hb_xvmSetLine( 75 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPop( 9L ) ) break;
	hb_xvmSetLine( 76 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPop( 10L ) ) break;
	hb_xvmSetLine( 77 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPop( 11L ) ) break;
	hb_xvmSetLine( 79 );
	hb_xvmPushFuncSymbol( symbols + 3 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 35 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 80 );
	hb_xvmPushFuncSymbol( symbols + 4 );
	hb_xvmPushLocal( 35 );
	hb_xvmPushLocalByRef( 9 );
	hb_xvmPushLocalByRef( 10 );
	hb_xvmPushLocalByRef( 20 );
	hb_xvmPushLocalByRef( 21 );
	hb_xvmPushLocalByRef( 22 );
	hb_xvmPushLocalByRef( 23 );
	if( hb_xvmDo( 7 ) ) break;
lab00001: ;
	hb_xvmSetLine( 83 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 34L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
lab00002: ;
	hb_xvmSetLine( 84 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 63L ) ) break;
	goto lab00004;
lab00003: ;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 33L ) ) break;
lab00004: ;
	hb_xvmPopLocal( 2 );
	hb_xvmSetLine( 85 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 35L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 86 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocalByRef( 10 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 36L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00005: ;
	hb_xvmSetLine( 89 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 431L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00006;
	hb_xvmSetLine( 90 );
	hb_xvmPushLocalByRef( 4 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 91 );
	hb_xvmPushLocalByRef( 3 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPlusEqPop() ) break;
	hb_xvmSetLine( 92 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 38L ) ) break;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 2 );
lab00006: ;
	hb_xvmSetLine( 95 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "_HMG_aNodeItemCargo", 19 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmSetLine( 96 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "_HMG_aNodeItemCargo", 19 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmDo( 2 ) ) break;
lab00007: ;
	hb_xvmSetLine( 99 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "_HMG_lDialogInMemory", 20 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 73L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 101 );
	hb_xvmPushFuncSymbol( symbols + 7 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00010;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 73L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00010;
	hb_xvmSetLine( 102 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushStringConst( "Window: ", 8 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmPushStringConst( "Parent", 6 );
	goto lab00009;
lab00008: ;
	hb_xvmPushLocal( 2 );
lab00009: ;
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " is not defined.", 16 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00010: ;
	hb_xvmSetLine( 105 );
	hb_xvmPushFuncSymbol( symbols + 9 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "0", 1 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 106 );
	hb_xvmPushFuncSymbol( symbols + 10 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 1 );
lab00011: ;
	hb_xvmSetLine( 109 );
	hb_xvmPushFuncSymbol( symbols + 11 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00012;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 73L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00012;
	hb_xvmSetLine( 110 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushStringConst( "Control: ", 9 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " Of ", 4 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( " Already defined.", 17 );
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 1 ) ) break;
lab00012: ;
	hb_xvmSetLine( 113 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 15 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00013;
	hb_xvmPushLocal( 15 );
	goto lab00014;
lab00013: ;
	hb_xvmPushInteger( 0 );
lab00014: ;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPop( 4L ) ) break;
	hb_xvmSetLine( 115 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushInteger( 120 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 116 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushInteger( 120 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 117 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocalByRef( 7 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 118 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocalByRef( 11 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 119 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocalByRef( 12 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 120 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocalByRef( 13 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 121 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocalByRef( 32 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 123 );
	hb_xvmPushStringConst( "_", 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushStringConst( "_", 1 );
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmLocalAdd( 39 );
	hb_xvmSetLine( 124 );
	hb_xvmPushFuncSymbol( symbols + 14 );
	if( hb_xvmFunction( 0 ) ) break;
	hb_xvmPopLocal( 40 );
	hb_xvmSetLine( 126 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 65L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00026;
	hb_xvmSetLine( 128 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 64L ) ) break;
	hb_xvmPopLocal( 33 );
	hb_xvmSetLine( 130 );
	hb_xvmPushFuncSymbol( symbols + 15 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00017;
	hb_xvmSetLine( 131 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00015;
	hb_xvmPushInteger( 0 );
	goto lab00016;
lab00015: ;
	hb_xvmPushInteger( 4 );
lab00016: ;
	hb_xvmPopLocal( 41 );
lab00017: ;
	hb_xvmSetLine( 134 );
#if INT_MAX >= INT32_MAX
	hb_xvmPushInteger( 1350631459 );
#else
	hb_xvmPushLong( 1350631459L );
#endif
	hb_xvmPushLocal( 41 );
	hb_xvmLocalAdd( 42 );
	hb_xvmSetLine( 136 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 71L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00018;
	hb_xvmSetLine( 139 );
	{
		static const HB_BYTE codeblock[ 16 ] = {
			3, 0, 0, 0, 176, 17, 0, 95, 1, 95, 2, 95, 3, 12, 3, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	hb_xvmPopLocal( 43 );
	hb_xvmSetLine( 140 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPop( 74L ) ) break;
	hb_xvmSetLine( 141 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 72L ) ) break;
	hb_xvmPushLocal( 30 );
	hb_xvmPushLocal( 40 );
	hb_xvmPushStringConst( "SysTreeView32", 13 );
	hb_xvmPushLocal( 42 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 23 );
	hb_xvmPushLocal( 43 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 42L ) ) break;
	hb_xvmArrayGen( 22 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00042;
lab00018: ;
	hb_xvmSetLine( 145 );
	hb_xvmPushFuncSymbol( symbols + 19 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 30 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 34 );
	hb_xvmSetLine( 147 );
	hb_xvmPushFuncSymbol( symbols + 20 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushLocal( 42 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 3 ) ) break;
	hb_xvmSetLine( 149 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 150 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushInteger( 1 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 151 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushInteger( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 152 );
	hb_xvmPushFuncSymbol( symbols + 21 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushInteger( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 154 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00019;
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmFunction( 1 ) ) break;
	goto lab00020;
lab00019: ;
	hb_xvmPushInteger( 0 );
lab00020: ;
	hb_xvmPopLocal( 36 );
	hb_xvmSetLine( 155 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00021;
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmFunction( 1 ) ) break;
	goto lab00022;
lab00021: ;
	hb_xvmPushInteger( 0 );
lab00022: ;
	hb_xvmPopLocal( 37 );
	hb_xvmSetLine( 157 );
	hb_xvmPushLocal( 36 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00025;
	hb_xvmSetLine( 159 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayItemPop( 1L ) ) break;
	hb_xvmSetLine( 160 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayItemPop( 2L ) ) break;
	hb_xvmSetLine( 162 );
	hb_xvmPushLocal( 37 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00023;
	hb_xvmSetLine( 164 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayItemPop( 3L ) ) break;
	hb_xvmSetLine( 165 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayItemPop( 4L ) ) break;
	goto lab00024;
lab00023: ;
	hb_xvmSetLine( 169 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayItemPop( 3L ) ) break;
	hb_xvmSetLine( 170 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayItemPop( 4L ) ) break;
lab00024: ;
	hb_xvmSetLine( 174 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushLocal( 38 );
	hb_xvmPushLocal( 32 );
	if( hb_xvmDo( 3 ) ) break;
lab00025: ;
	hb_xvmSetLine( 178 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPop( 6L ) ) break;
	goto lab00042;
lab00026: ;
	hb_xvmSetLine( 184 );
	hb_xvmPushFuncSymbol( symbols + 24 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 33 );
	hb_xvmSetLine( 186 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00027;
	hb_xvmPushLocal( 4 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00031;
lab00027: ;
	hb_xvmSetLine( 188 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 22L ) ) break;
	hb_xvmPushStringConst( "TOOLBAR", 7 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00028;
	hb_xvmSetLine( 189 );
	hb_xvmPushLogical( HB_TRUE );
	hb_xvmPopLocal( 14 );
lab00028: ;
	hb_xvmSetLine( 192 );
	hb_xvmPushFuncSymbol( symbols + 25 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 44 );
	hb_xvmSetLine( 194 );
	hb_xvmPushLocal( 44 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00034;
	hb_xvmSetLine( 196 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 114L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00029;
	hb_xvmPushInteger( 1 );
	goto lab00030;
lab00029: ;
	hb_xvmPushInteger( 0 );
lab00030: ;
	if( hb_xvmFunction( 9 ) ) break;
	hb_xvmPopLocal( 34 );
	hb_xvmSetLine( 198 );
	hb_xvmPushFuncSymbol( symbols + 27 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 114L ) ) break;
	hb_xvmPushLocal( 44 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 14 );
	hb_xvmPushNil();
	hb_xvmPushNil();
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 232L ) ) break;
	if( hb_xvmDo( 8 ) ) break;
	hb_xvmSetLine( 200 );
	hb_xvmPushStringConst( "TREE", 4 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPop( 22L ) ) break;
	goto lab00034;
lab00031: ;
	hb_xvmSetLine( 206 );
	hb_xvmPushFuncSymbol( symbols + 26 );
	hb_xvmPushLocal( 33 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushStringConst( "", 0 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 19 );
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00032;
	hb_xvmPushInteger( 1 );
	goto lab00033;
lab00032: ;
	hb_xvmPushInteger( 0 );
lab00033: ;
	if( hb_xvmFunction( 9 ) ) break;
	hb_xvmPopLocal( 34 );
lab00034: ;
	hb_xvmSetLine( 210 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00035;
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmFunction( 1 ) ) break;
	goto lab00036;
lab00035: ;
	hb_xvmPushInteger( 0 );
lab00036: ;
	hb_xvmPopLocal( 36 );
	hb_xvmSetLine( 211 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00037;
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmFunction( 1 ) ) break;
	goto lab00038;
lab00037: ;
	hb_xvmPushInteger( 0 );
lab00038: ;
	hb_xvmPopLocal( 37 );
	hb_xvmSetLine( 213 );
	hb_xvmPushLocal( 36 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00041;
	hb_xvmSetLine( 215 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayItemPop( 1L ) ) break;
	hb_xvmSetLine( 216 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayItemPop( 2L ) ) break;
	hb_xvmSetLine( 218 );
	hb_xvmPushLocal( 37 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00039;
	hb_xvmSetLine( 220 );
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayItemPop( 3L ) ) break;
	hb_xvmSetLine( 221 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 37 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayItemPop( 4L ) ) break;
	goto lab00040;
lab00039: ;
	hb_xvmSetLine( 225 );
	hb_xvmPushLocal( 17 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayItemPop( 3L ) ) break;
	hb_xvmSetLine( 226 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushLocal( 36 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 38 );
	if( hb_xvmArrayItemPop( 4L ) ) break;
lab00040: ;
	hb_xvmSetLine( 230 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushLocal( 38 );
	hb_xvmPushLocal( 32 );
	if( hb_xvmDo( 3 ) ) break;
lab00041: ;
	hb_xvmSetLine( 234 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPop( 6L ) ) break;
lab00042: ;
	hb_xvmSetLine( 238 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 73L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00051;
	hb_xvmSetLine( 240 );
	hb_xvmPushLocal( 35 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00043;
	hb_xvmSetLine( 241 );
	hb_xvmPushFuncSymbol( symbols + 28 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushLocal( 35 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00044;
lab00043: ;
	hb_xvmSetLine( 243 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocalByRef( 9 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 212L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 244 );
	hb_xvmPushFuncSymbol( symbols + 5 );
	hb_xvmPushLocalByRef( 10 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 213L ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 245 );
	hb_xvmPushFuncSymbol( symbols + 29 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 10 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 23 );
	if( hb_xvmFunction( 7 ) ) break;
	hb_xvmPopLocal( 35 );
lab00044: ;
	hb_xvmSetLine( 248 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 41L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00045;
	hb_xvmSetLine( 249 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 45L ) ) break;
	hb_xvmPushLocal( 34 );
	if( hb_xvmDo( 2 ) ) break;
lab00045: ;
	hb_xvmSetLine( 252 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushNil();
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00046;
	hb_xvmSetLine( 253 );
	hb_xvmPushFuncSymbol( symbols + 30 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushFuncSymbol( symbols + 31 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmDo( 3 ) ) break;
lab00046: ;
	hb_xvmSetLine( 256 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00047;
	hb_xvmSetLine( 257 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushInteger( 4381 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 25 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 4 ) ) break;
lab00047: ;
	hb_xvmSetLine( 260 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00048;
	hb_xvmSetLine( 261 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushInteger( 4382 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 26 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 26 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 4 ) ) break;
lab00048: ;
	hb_xvmSetLine( 264 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00049;
	hb_xvmSetLine( 265 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushInteger( 4392 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmMultByInt( 256L ) ) break;
	if( hb_xvmPlus() ) break;
	hb_xvmPushLocal( 27 );
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmMultByInt( 65536L ) ) break;
	if( hb_xvmPlus() ) break;
	if( hb_xvmDo( 4 ) ) break;
lab00049: ;
	hb_xvmSetLine( 268 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 28 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00050;
	hb_xvmSetLine( 269 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushInteger( 4359 );
	hb_xvmPushLocal( 28 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 4 ) ) break;
lab00050: ;
	hb_xvmSetLine( 272 );
	hb_xvmPushFuncSymbol( symbols + 12 );
	hb_xvmPushLocal( 29 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00051;
	hb_xvmSetLine( 273 );
	hb_xvmPushFuncSymbol( symbols + 32 );
	hb_xvmPushLocal( 34 );
	hb_xvmPushInteger( 4379 );
	hb_xvmPushLocal( 29 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 4 ) ) break;
lab00051: ;
	hb_xvmSetLine( 279 );
	hb_xvmPushFuncSymbol( symbols + 33 );
	hb_xvmPushLocal( 39 );
	hb_xvmPushLocal( 40 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 284 );
	hb_xvmPushLocal( 40 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPop( 5L ) ) break;
	hb_xvmSetLine( 286 );
	hb_xvmPushStringConst( "TREE", 4 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 135L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 287 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 136L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 288 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 289 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 138L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 290 );
	hb_xvmPushLocal( 30 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 139L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 291 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 140L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 292 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 293 );
	hb_xvmPushNil();
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 142L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 294 );
	hb_xvmPushLocal( 24 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 295 );
	hb_xvmPushLocal( 12 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 144L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 296 );
	hb_xvmPushLocal( 11 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 145L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 297 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 146L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 298 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 299 );
	hb_xvmPushLocal( 25 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 147L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 300 );
	hb_xvmPushLocal( 26 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 148L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 301 );
	hb_xvmPushLocal( 13 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 149L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 302 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 303 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 151L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 304 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 152L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 305 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 153L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 306 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 154L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 307 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 155L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 308 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00052;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 39L ) ) break;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00053;
lab00052: ;
	hb_xvmPushInteger( -1 );
lab00053: ;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 156L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 309 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00054;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 40L ) ) break;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 37L ) ) break;
	if( hb_xvmArrayPush() ) break;
	goto lab00055;
lab00054: ;
	hb_xvmPushInteger( -1 );
lab00055: ;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 157L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 310 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 311 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 159L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 312 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 160L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 313 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 161L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 314 );
	hb_xvmPushLocal( 20 );
	hb_xvmPushLocal( 21 );
	hb_xvmPushLocal( 22 );
	hb_xvmPushLocal( 23 );
	hb_xvmArrayGen( 4 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 168L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 315 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 162L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 316 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 163L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 317 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 164L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 318 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 165L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 319 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 166L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 320 );
	hb_xvmPushLocal( 16 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 209L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 321 );
	hb_xvmPushLocal( 35 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 167L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 322 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 169L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 323 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 170L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 324 );
	hb_xvmPushInteger( 0 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushLocal( 18 );
	hb_xvmPushLocal( 32 );
	hb_xvmArrayGen( 4 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 325 );
	hb_xvmPushStringConst( "", 0 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 424L ) ) break;
	hb_xvmPushLocal( 40 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 327 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00056;
	hb_xvmSetLine( 328 );
	hb_xvmPushSymbol( symbols + 34 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 448L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPushLocal( 40 );
	hb_xvmPushLocal( 39 );
	if( hb_xvmSend( 2 ) ) break;
	hb_stackPop();
	hb_xvmSetLine( 330 );
	hb_xvmPushFuncSymbol( symbols + 35 );
	hb_xvmPushLocal( 33 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 46 );
	hb_xvmSetLine( 331 );
	hb_xvmPushFuncSymbol( symbols + 36 );
	hb_xvmPushLocal( 34 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 45 );
lab00056: ;
	hb_xvmSetLine( 335 );
	hb_xvmPushFuncSymbol( symbols + 37 );
	hb_xvmPushLocal( 31 );
	hb_xvmPushLocal( 40 );
	hb_xvmPushLocal( 46 );
	hb_xvmPushLocal( 45 );
	if( hb_xvmDo( 4 ) ) break;
	hb_xvmSetLine( 337 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( INITDIALOGTREE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 17, 3 );
	hb_xvmSetLine( 342 );
	hb_xvmPushInteger( 4 );
	hb_xvmArrayDim( 1 );
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 356 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 357 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 358 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPopLocal( 10 );
	hb_xvmSetLine( 360 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmPushInteger( 0 );
lab00002: ;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 361 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	goto lab00004;
lab00003: ;
	hb_xvmPushInteger( 0 );
lab00004: ;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 363 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00007;
	hb_xvmSetLine( 364 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 1L ) ) break;
	hb_xvmSetLine( 365 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 2L ) ) break;
	hb_xvmSetLine( 366 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmSetLine( 367 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 3L ) ) break;
	hb_xvmSetLine( 368 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 4L ) ) break;
	goto lab00006;
lab00005: ;
	hb_xvmSetLine( 370 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 3L ) ) break;
	hb_xvmSetLine( 371 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPop( 4L ) ) break;
lab00006: ;
	hb_xvmSetLine( 373 );
	hb_xvmPushFuncSymbol( symbols + 23 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 10 );
	if( hb_xvmDo( 3 ) ) break;
lab00007: ;
	hb_xvmSetLine( 376 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPop( 6L ) ) break;
	hb_xvmSetLine( 378 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 19 );
	goto lab00018;
lab00008: ;
	hb_xvmSetLine( 379 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 74L ) ) break;
	hb_xvmPushLocal( 19 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 380 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 74L ) ) break;
	hb_xvmPushLocal( 19 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPopLocal( 13 );
	hb_xvmSetLine( 381 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 74L ) ) break;
	hb_xvmPushLocal( 19 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	hb_xvmPopLocal( 15 );
	hb_xvmSetLine( 382 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 74L ) ) break;
	hb_xvmPushLocal( 19 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPopLocal( 18 );
	hb_xvmSetLine( 383 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 74L ) ) break;
	hb_xvmPushLocal( 19 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 6L ) ) break;
	hb_xvmPopLocal( 20 );
	hb_xvmSetLine( 384 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00009;
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	goto lab00010;
lab00009: ;
	hb_xvmPushInteger( 0 );
lab00010: ;
	hb_xvmPopLocal( 14 );
	hb_xvmSetLine( 385 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 11 );
	hb_xvmSetLine( 387 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "_HMG_aNodeItemCargo", 19 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 20 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 389 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00012;
	hb_xvmSetLine( 390 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 74L ) ) break;
	hb_xvmPushLocal( 19 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 5L ) ) break;
	hb_xvmPushStringConst( "NODE", 4 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00011;
	hb_xvmSetLine( 391 );
	hb_xvmLocalSetInt( 16, 0L );
	hb_xvmSetLine( 392 );
	hb_xvmLocalSetInt( 17, 1L );
	goto lab00015;
lab00011: ;
	hb_xvmSetLine( 394 );
	hb_xvmLocalSetInt( 16, 2L );
	hb_xvmSetLine( 395 );
	hb_xvmLocalSetInt( 17, 3L );
	goto lab00015;
lab00012: ;
	hb_xvmSetLine( 398 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 6L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmDec() ) break;
	hb_xvmPopLocal( 16 );
	hb_xvmSetLine( 399 );
	hb_xvmPushLocal( 14 );
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00013;
	hb_xvmPushLocal( 16 );
	goto lab00014;
lab00013: ;
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 6L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	hb_xvmPushLocal( 10 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmDec() ) break;
lab00014: ;
	hb_xvmPopLocal( 17 );
lab00015: ;
	hb_xvmSetLine( 402 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 74L ) ) break;
	hb_xvmPushLocal( 19 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 5L ) ) break;
	hb_xvmPushStringConst( "NODE", 4 );
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00016;
	hb_xvmSetLine( 403 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 6L ) ) break;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmDec() ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmFunction( 7 ) ) break;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 404 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 9L ) ) break;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	hb_xvmPushLocal( 18 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 405 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 10L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00017;
lab00016: ;
	hb_xvmSetLine( 407 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 6L ) ) break;
	hb_xvmPushLocal( 11 );
	hb_xvmPushLocal( 13 );
	hb_xvmPushLocal( 16 );
	hb_xvmPushLocal( 17 );
	hb_xvmPushLocal( 15 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmFunction( 7 ) ) break;
	hb_xvmPopLocal( 12 );
	hb_xvmSetLine( 408 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 9L ) ) break;
	hb_xvmPushLocal( 12 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 409 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 10L ) ) break;
	hb_xvmPushLocal( 15 );
	if( hb_xvmDo( 2 ) ) break;
lab00017: ;
	hb_xvmSetLine( 378 );
	if( hb_xvmLocalIncPush( 19 ) ) break;
lab00018: ;
	hb_xvmPushFuncSymbol( symbols + 16 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 74L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmGreater() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00008;
	hb_xvmSetLine( 413 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 9L ) ) break;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 5L ) ) break;
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 414 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 10L ) ) break;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 5L ) ) break;
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 415 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "_HMG_aNodeItemCargo", 19 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 5L ) ) break;
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 417 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00020;
	hb_xvmSetLine( 419 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 11L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00019;
	hb_xvmSetLine( 420 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 6L ) ) break;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 9L ) ) break;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 2 ) ) break;
	goto lab00020;
lab00019: ;
	hb_xvmSetLine( 422 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 6L ) ) break;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 9L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 42 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 10L ) ) break;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00020: ;
	hb_xvmSetLine( 426 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 71L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00021;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 71L ) ) break;
	if( hb_xvmArrayItemPush( 3L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00021;
	hb_xvmSetLine( 427 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 134L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPop() ) break;
lab00021: ;
	hb_xvmSetLine( 430 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _DEFINETREENODE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 4 );
	hb_xvmSetLine( 438 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 440 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "_HMG_lDialogInMemory", 20 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 442 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	hb_xvmPushInteger( 8 );
	if( hb_xvmArrayPushRef() ) break;
	if( hb_xvmIncEqPop() ) break;
	hb_xvmSetLine( 443 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 74L ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 8L ) ) break;
	hb_xvmPushStringConst( "NODE", 4 );
	hb_xvmPushLocal( 4 );
	hb_xvmArrayGen( 6 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00008;
lab00001: ;
	hb_xvmSetLine( 447 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	goto lab00003;
lab00002: ;
	hb_xvmPushInteger( 0 );
lab00003: ;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 449 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmSetLine( 450 );
	hb_xvmLocalSetInt( 7, 0L );
	hb_xvmSetLine( 451 );
	hb_xvmLocalSetInt( 8, 1L );
	goto lab00007;
lab00004: ;
	hb_xvmSetLine( 453 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 5L ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 454 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 6L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmDec() ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 455 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmPushLocal( 7 );
	goto lab00006;
lab00005: ;
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 6L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmDec() ) break;
lab00006: ;
	hb_xvmPopLocal( 8 );
lab00007: ;
	hb_xvmSetLine( 459 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	hb_xvmPushInteger( 8 );
	if( hb_xvmArrayPushRef() ) break;
	if( hb_xvmIncEqPop() ) break;
	hb_xvmSetLine( 460 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 6L ) ) break;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 8L ) ) break;
	if( hb_xvmDec() ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmFunction( 7 ) ) break;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 8L ) ) break;
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 461 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 9L ) ) break;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 8L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 462 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 10L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 463 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "_HMG_aNodeItemCargo", 19 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 2 ) ) break;
lab00008: ;
	hb_xvmSetLine( 467 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _ENDTREENODE )
{
   do {
	hb_xvmSetLine( 473 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	hb_xvmPushInteger( 8 );
	if( hb_xvmArrayPushRef() ) break;
	if( hb_xvmDecEqPop() ) break;
	hb_xvmSetLine( 475 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _DEFINETREEITEM )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 5, 4 );
	hb_xvmSetLine( 484 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocalByRef( 3 );
	hb_xvmPushInteger( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 486 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "_HMG_lDialogInMemory", 20 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 488 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 74L ) ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 8L ) ) break;
	hb_xvmPushStringConst( "ITEM", 4 );
	hb_xvmPushLocal( 4 );
	hb_xvmArrayGen( 6 );
	if( hb_xvmDo( 2 ) ) break;
	goto lab00008;
lab00001: ;
	hb_xvmSetLine( 492 );
	hb_xvmPushFuncSymbol( symbols + 22 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmPushFuncSymbol( symbols + 16 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 1 ) ) break;
	goto lab00003;
lab00002: ;
	hb_xvmPushInteger( 0 );
lab00003: ;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 494 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmSetLine( 495 );
	hb_xvmLocalSetInt( 8, 2L );
	hb_xvmSetLine( 496 );
	hb_xvmLocalSetInt( 9, 3L );
	goto lab00007;
lab00004: ;
	hb_xvmSetLine( 498 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 5L ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 499 );
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 6L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmDec() ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 500 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmEqualIntIs( 1L, &fValue ) ) break;
	if( !fValue )
		goto lab00005;
	hb_xvmPushLocal( 8 );
	goto lab00006;
lab00005: ;
	hb_xvmPushFuncSymbol( symbols + 38 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 6L ) ) break;
	hb_xvmPushLocal( 2 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 171L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmDec() ) break;
lab00006: ;
	hb_xvmPopLocal( 9 );
lab00007: ;
	hb_xvmSetLine( 504 );
	hb_xvmPushFuncSymbol( symbols + 39 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 6L ) ) break;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 7L ) ) break;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 8L ) ) break;
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmFunction( 7 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 505 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 9L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 506 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 10L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 507 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "_HMG_aNodeItemCargo", 19 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 2 ) ) break;
lab00008: ;
	hb_xvmSetLine( 511 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _ENDTREE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 1, 0 );
	hb_xvmSetLine( 518 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "_HMG_lDialogInMemory", 20 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00003;
	hb_xvmSetLine( 520 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 9L ) ) break;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 5L ) ) break;
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 521 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 10L ) ) break;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 5L ) ) break;
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 522 );
	hb_xvmPushFuncSymbol( symbols + 40 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "_HMG_aNodeItemCargo", 19 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 5L ) ) break;
	if( hb_xvmArrayPop() ) break;
	hb_xvmSetLine( 524 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00003;
	hb_xvmSetLine( 526 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 11L ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 527 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	hb_xvmPushFuncSymbol( symbols + 47 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 6L ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmLessEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmSetLine( 528 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 6L ) ) break;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 9L ) ) break;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 2 ) ) break;
	goto lab00003;
lab00001: ;
	hb_xvmSetLine( 531 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 10L ) ) break;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 4L ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 1 );
	hb_xvmSetLine( 532 );
	hb_xvmPushLocal( 1 );
	if( hb_xvmEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 533 );
	hb_xvmPushFuncSymbol( symbols + 8 );
	hb_xvmPushStringConst( "Value Property: Invalid TreeItem Reference.", 43 );
	if( hb_xvmDo( 1 ) ) break;
lab00002: ;
	hb_xvmSetLine( 535 );
	hb_xvmPushFuncSymbol( symbols + 41 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 6L ) ) break;
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 9L ) ) break;
	hb_xvmPushLocal( 1 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00003: ;
	hb_xvmSetLine( 542 );
	hb_xvmPushFuncSymbol( symbols + 6 );
	hb_xvmPushStringConst( "_HMG_aNodeItemCargo", 19 );
	hb_xvmArrayGen( 0 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 544 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _COLLAPSE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 4 );
	hb_xvmSetLine( 551 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 5 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 552 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 553 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 554 );
	hb_xvmPushFuncSymbol( symbols + 51 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 1 );
	hb_xvmPushFuncSymbol( symbols + 52 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmDo( 4 ) ) break;
lab00001: ;
	hb_xvmSetLine( 558 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( _EXPAND )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 4 );
	hb_xvmSetLine( 565 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 5 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 566 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 567 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 568 );
	hb_xvmPushFuncSymbol( symbols + 51 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 137L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 6 );
	hb_xvmPushInteger( 2 );
	hb_xvmPushFuncSymbol( symbols + 52 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmDo( 4 ) ) break;
lab00001: ;
	hb_xvmSetLine( 572 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( TREEITEMGETHANDLE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 3 );
	hb_xvmSetLine( 577 );
	hb_xvmLocalSetInt( 4, 0L );
	hb_xvmSetLine( 581 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 6 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 583 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 584 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmLessEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 585 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 4 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 588 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 589 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 590 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 4 );
lab00002: ;
	hb_xvmSetLine( 596 );
	hb_xvmPushLocal( 4 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( TREEITEMGETROOTVALUE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 5, 2 );
	hb_xvmSetLine( 601 );
	hb_xvmPushFuncSymbol( symbols + 55 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 602 );
	hb_xvmPushFuncSymbol( symbols + 56 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 607 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 7 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmPushLocal( 4 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 608 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 609 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 610 );
	hb_xvmPushLocal( 5 );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 612 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 613 );
	hb_xvmPushLocal( 6 );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00002: ;
	hb_xvmSetLine( 617 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( TREEITEMGETPARENTVALUE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 5, 3 );
	hb_xvmSetLine( 622 );
	hb_xvmPushFuncSymbol( symbols + 55 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 623 );
	hb_xvmPushFuncSymbol( symbols + 59 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 628 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 8 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmPushLocal( 5 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 629 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 630 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 631 );
	hb_xvmPushLocal( 6 );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 633 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 634 );
	hb_xvmPushLocal( 7 );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00002: ;
	hb_xvmSetLine( 638 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( TREEITEMGETALLVALUES )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 4, 2 );
	hb_xvmSetLine( 643 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 644 );
	hb_xvmLocalSetInt( 4, 1L );
	hb_xvmSetLine( 646 );
	hb_xvmPushFuncSymbol( symbols + 47 );
	hb_xvmPushFuncSymbol( symbols + 55 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 648 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 5 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmPushLocal( 6 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 649 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 650 );
	hb_xvmPushLocal( 6 );
	hb_xvmArrayDim( 1 );
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 651 );
	hb_xvmPushFuncSymbol( symbols + 61 );
	hb_xvmPushLocal( 3 );
	{
		static const HB_BYTE codeblock[ 23 ] = {
			2, 0, 2, 0, 3, 0, 4, 0, 96, 254, 255, 158, 170, 95, 255, 95, 
			2, 2, 100, 165, 80, 1, 6 };
		hb_xvmPushBlock( codeblock, symbols );
	}
	if( hb_xvmDo( 2 ) ) break;
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 653 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 5 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 3 );
lab00002: ;
	hb_xvmSetLine( 657 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00003;
	hb_xvmPushNil();
	goto lab00004;
lab00003: ;
	hb_xvmPushLocal( 3 );
lab00004: ;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( TREEITEMGETCHILDVALUES )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 5, 3 );
	hb_xvmSetLine( 662 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 663 );
	hb_xvmPushFuncSymbol( symbols + 55 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 664 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 665 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 667 );
	hb_xvmPushFuncSymbol( symbols + 64 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
lab00001: ;
	hb_xvmSetLine( 668 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00004;
	hb_xvmSetLine( 669 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00002;
	hb_xvmSetLine( 670 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 8 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	goto lab00003;
lab00002: ;
	hb_xvmSetLine( 672 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00003: ;
	hb_xvmSetLine( 674 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	goto lab00001;
lab00004: ;
	hb_xvmSetLine( 677 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00005;
	hb_xvmPushNil();
	goto lab00006;
lab00005: ;
	hb_xvmPushLocal( 5 );
lab00006: ;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( TREEITEMGETSIBLINGVALUES )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 6, 3 );
	hb_xvmSetLine( 682 );
	hb_xvmArrayGen( 0 );
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 683 );
	hb_xvmPushFuncSymbol( symbols + 55 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 7 );
	hb_xvmSetLine( 684 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 685 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 9 );
	hb_xvmSetLine( 687 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushFuncSymbol( symbols + 56 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNotEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00006;
	hb_xvmSetLine( 688 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 4 );
	hb_xvmPopLocal( 5 );
lab00001: ;
	hb_xvmSetLine( 689 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 690 );
	hb_xvmPushFuncSymbol( symbols + 67 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 691 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00001;
	hb_xvmSetLine( 692 );
	hb_xvmCopyLocals( 4, 5 );
	goto lab00001;
lab00002: ;
	hb_xvmSetLine( 695 );
	hb_xvmCopyLocals( 5, 4 );
lab00003: ;
	hb_xvmSetLine( 696 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmNotEqualIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00006;
	hb_xvmSetLine( 697 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00004;
	hb_xvmSetLine( 698 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 141L ) ) break;
	hb_xvmPushLocal( 9 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
	goto lab00005;
lab00004: ;
	hb_xvmSetLine( 700 );
	hb_xvmPushFuncSymbol( symbols + 18 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushFuncSymbol( symbols + 57 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmDo( 2 ) ) break;
lab00005: ;
	hb_xvmSetLine( 702 );
	hb_xvmPushFuncSymbol( symbols + 65 );
	hb_xvmPushLocal( 7 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	goto lab00003;
lab00006: ;
	hb_xvmSetLine( 706 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushLocal( 6 );
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00007;
	hb_xvmPushNil();
	goto lab00008;
lab00007: ;
	hb_xvmPushLocal( 6 );
lab00008: ;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( TREEITEMGETFIRSTITEMVALUE )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 2 );
	hb_xvmSetLine( 711 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 3 );
	hb_xvmSetLine( 712 );
	hb_xvmLocalSetInt( 4, 1L );
	hb_xvmSetLine( 715 );
	hb_xvmPushFuncSymbol( symbols + 69 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushStringConst( "ItemCount", 9 );
	if( hb_xvmFunction( 3 ) ) break;
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 716 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 717 );
	hb_xvmPushLocal( 4 );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00001: ;
	hb_xvmSetLine( 719 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 720 );
	hb_xvmPushLocal( 5 );
	hb_xvmRetValue();
	/* *** END PROC *** */
	break;
lab00002: ;
	hb_xvmSetLine( 724 );
	hb_xvmRetNil();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( TREEITEMSORT )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 2, 7 );
	hb_xvmSetLine( 731 );
	hb_xvmPushFuncSymbol( symbols + 55 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 8 );
	hb_xvmSetLine( 733 );
	hb_xvmPushLocal( 3 );
	hb_xvmPushNil();
	if( hb_xvmExactlyEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00001;
	hb_xvmSetLine( 734 );
	hb_xvmPushFuncSymbol( symbols + 56 );
	hb_xvmPushLocal( 8 );
	if( hb_xvmFunction( 1 ) ) break;
	hb_xvmPopLocal( 9 );
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 736 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 9 );
lab00002: ;
	hb_xvmSetLine( 739 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocalByRef( 4 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 740 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocalByRef( 5 );
	hb_xvmPushLogical( HB_FALSE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 741 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocalByRef( 6 );
	hb_xvmPushLogical( HB_TRUE );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 742 );
	hb_xvmPushFuncSymbol( symbols + 13 );
	hb_xvmPushLocalByRef( 7 );
	hb_xvmPushInteger( 2 );
	if( hb_xvmDo( 2 ) ) break;
	hb_xvmSetLine( 744 );
	hb_xvmPushFuncSymbol( symbols + 71 );
	hb_xvmPushLocal( 8 );
	hb_xvmPushLocal( 9 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 7 );
	if( hb_xvmDo( 6 ) ) break;
	hb_xvmSetLine( 746 );
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( TREEITEMISTRUENODE )
{
   do {
	hb_xvmFrame( 2, 3 );
	hb_xvmSetLine( 751 );
	hb_xvmPushFuncSymbol( symbols + 55 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 752 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 754 );
	hb_xvmPushFuncSymbol( symbols + 62 );
	hb_xvmPushFuncSymbol( symbols + 64 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmNot() ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( TREEITEMSETNODEFLAG )
{
   do {
	hb_xvmFrame( 2, 4 );
	hb_xvmSetLine( 759 );
	hb_xvmPushFuncSymbol( symbols + 55 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 760 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 762 );
	hb_xvmPushFuncSymbol( symbols + 74 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmDo( 3 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( TREEITEMGETNODEFLAG )
{
   do {
	hb_xvmFrame( 2, 3 );
	hb_xvmSetLine( 767 );
	hb_xvmPushFuncSymbol( symbols + 55 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 768 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 770 );
	hb_xvmPushFuncSymbol( symbols + 76 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 2 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( TREEITEMSETIMAGEINDEX )
{
   do {
	hb_xvmFrame( 2, 4 );
	hb_xvmSetLine( 775 );
	hb_xvmPushFuncSymbol( symbols + 55 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 776 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 6 );
	hb_xvmSetLine( 778 );
	hb_xvmPushFuncSymbol( symbols + 78 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushLocal( 6 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 1L ) ) break;
	hb_xvmPushLocal( 4 );
	if( hb_xvmArrayItemPush( 2L ) ) break;
	if( hb_xvmDo( 4 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( TREEITEMGETIMAGEINDEX )
{
   do {
	hb_xvmFrame( 2, 3 );
	hb_xvmSetLine( 783 );
	hb_xvmPushFuncSymbol( symbols + 55 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 784 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 786 );
	hb_xvmPushFuncSymbol( symbols + 80 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	if( hb_xvmDo( 2 ) ) break;
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( TREEITEMISEXPAND )
{
   do {
	hb_xvmFrame( 2, 3 );
	hb_xvmSetLine( 792 );
	hb_xvmPushFuncSymbol( symbols + 55 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPopLocal( 4 );
	hb_xvmSetLine( 793 );
	hb_xvmPushFuncSymbol( symbols + 50 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 795 );
	hb_xvmPushFuncSymbol( symbols + 82 );
	hb_xvmPushFuncSymbol( symbols + 83 );
	hb_xvmPushLocal( 4 );
	hb_xvmPushLocal( 5 );
	hb_xvmPushInteger( 32 );
	if( hb_xvmFunction( 3 ) ) break;
	hb_xvmPushInteger( 32 );
	if( hb_xvmFunction( 2 ) ) break;
	if( hb_xvmEqualInt( 32L ) ) break;
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

HB_FUNC( TREENODEITEMCARGO )
{
   HB_BOOL fValue;
   do {
	hb_xvmFrame( 3, 4 );
	hb_xvmSetLine( 804 );
	hb_xvmPushFuncSymbol( symbols + 49 );
	hb_xvmPushLocal( 1 );
	hb_xvmPushLocal( 2 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 7 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 806 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 143L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( fValue )
		goto lab00001;
	hb_xvmSetLine( 808 );
	hb_xvmPushLocal( 3 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmPushLocal( 3 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmLessEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 809 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 810 );
	hb_xvmPushFuncSymbol( symbols + 85 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmGreaterThenIntIs( 3L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 811 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmArrayPop() ) break;
	goto lab00002;
lab00001: ;
	hb_xvmSetLine( 817 );
	hb_xvmPushFuncSymbol( symbols + 42 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 158L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 3 );
	if( hb_xvmFunction( 2 ) ) break;
	hb_xvmPushUnRef();
	hb_xvmPopLocal( 6 );
	if( hb_xvmGreaterThenIntIs( 0L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmPushLocal( 6 );
	hb_xvmPushFuncSymbol( symbols + 16 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPush() ) break;
	if( hb_xvmFunction( 1 ) ) break;
	if( hb_xvmLessEqual() ) break;
	if( hb_xvmPopLogical( &fValue ) ) break;
	if( ! fValue )
		goto lab00002;
	hb_xvmSetLine( 818 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPopLocal( 5 );
	hb_xvmSetLine( 819 );
	hb_xvmPushFuncSymbol( symbols + 85 );
	if( hb_xvmFunction( 0 ) ) break;
	if( hb_xvmGreaterThenIntIs( 3L, &fValue ) ) break;
	if( !fValue )
		goto lab00002;
	hb_xvmSetLine( 820 );
	hb_xvmPushLocal( 4 );
	if( hb_xvmPushMemvar( symbols + 2 ) ) break;
	if( hb_xvmArrayItemPush( 150L ) ) break;
	hb_xvmPushLocal( 7 );
	if( hb_xvmArrayPush() ) break;
	hb_xvmPushLocal( 6 );
	if( hb_xvmArrayPop() ) break;
lab00002: ;
	hb_xvmSetLine( 828 );
	hb_xvmPushLocal( 5 );
	hb_xvmRetValue();
	/* *** END PROC *** */
   } while( 0 );
}

