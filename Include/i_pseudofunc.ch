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

////////////////////////////
// MiniGUI pseudo-functions
////////////////////////////

#xtranslate MsgInfo ( <c>, <t> )	=> MsgInfo ( <c>, <t>, , .F. )
#xtranslate MsgStop ( <c>, <t> )	=> MsgStop ( <c>, <t>, , .F. )
#xtranslate MsgYesNo( <c>, <t> )	=> MsgYesNo(<c>, <t>, , , .F.)
#xtranslate MsgAlert( <c>, <t> )	=> MsgExclamation( <c>, <t>, , .F. )
#xtranslate MsgExclamation( <c>, <t> )	=> MsgExclamation( <c>, <t>, , .F. )
#xtranslate MsgYesNoCancel( <c>, <t> )	=> MsgYesNoCancel( <c>, <t>, , .F.)
#xtranslate MsgRetryCancel( <c>, <t> )	=> MsgRetryCancel( <c>, <t>, , .F. )
#xtranslate MsgOkCancel ( <c>, <t> )	=> MsgOkCancel( <c>, <t>, , .F. )

// ============================================================================

#translate IsControlDefined ( <ControlName> , <FormName> ) ;
=> ;
_IsControlDefined ( <(ControlName)> , <(FormName)> )

#translate IsWindowActive ( <FormName> ) ;
=> ;
_IsWindowActive ( <(FormName)> )

#translate IsWindowDefined ( <FormName> ) ;
=> ;
_IsWindowDefined ( <(FormName)> )

// ============================================================================

#translate GetProperty ( <FormName> , "ClientWidth" ) ;
=> ;
_GetClientRect ( GetFormHandle ( <FormName> ) ) \[3]

#translate GetProperty ( <FormName> , "ClientHeight" ) ;
=> ;
_GetClientRect ( GetFormHandle ( <FormName> ) ) \[4]

#translate GetProperty ( <FormName> , <ControlName> , "ClientWidth" ) ;
=> ;
_GetClientRect ( GetControlHandle ( <ControlName> , <FormName> ) ) \[3]

#translate GetProperty ( <FormName> , <ControlName> , "ClientHeight" ) ;
=> ;
_GetClientRect ( GetControlHandle ( <ControlName> , <FormName> ) ) \[4]

#translate DoMethod ( <FormName> , "Print" ) ;
=> ;
PrintWindow( <FormName> )

#translate DoMethod ( <FormName> , "SaveAs", <FileName> ) ;
=> ;
WndCopy( GetFormHandle( <FormName> ), .f., <FileName> )

#translate DoMethod ( <FormName> , <ControlName> , "SaveAs" , <FileName> ) ;
=> ;
WndCopy( GetControlHandle( <ControlName> , <FormName> ), .t., <FileName> )

// ============================================================================

#xtranslate GetProperty ( <FormName> , <ControlName> , "ImageWidth" ) ;
=> ;
_HMG_aControlHeadClick \[ GetControlIndex ( <ControlName>, <FormName> ) ] \[1]

#xtranslate GetProperty ( <FormName> , <ControlName> , "ImageHeight" ) ;
=> ;
_HMG_aControlHeadClick \[ GetControlIndex ( <ControlName>, <FormName> ) ] \[2]

#xtranslate SetProperty ( <FormName> , <ControlName> , "ImageWidth", <w> ) ;
=> ;
_HMG_aControlHeadClick \[ GetControlIndex ( <ControlName>, <FormName> ) ] \[1] := <w>

#xtranslate SetProperty ( <FormName> , <ControlName> , "ImageHeight", <h> ) ;
=> ;
_HMG_aControlHeadClick \[ GetControlIndex ( <ControlName>, <FormName> ) ] \[2] := <h>

#xtranslate SetProperty ( <FormName> , <ControlName> , "Velocity" , <Value> ) ;
=> ;
SendMessage( GetControlHandle( <ControlName> , <FormName> ) , WM_USER+10 , iif(<Value> > 0, 1, 0) , <Value> )

#xtranslate SetProperty ( <FormName> , <ControlName> , "Stretch" , <Value> ) ;
=> ;
SetProperty( <FormName> , <ControlName>, "Value", iif(<Value>, 1, 0) )

////////////////////////////////////////////
// Variable type identifier pseudo-functions
////////////////////////////////////////////
#ifndef HB_COMMON_CH_
   /* Type checking macros */
#  translate ISNIL( <xValue> )       => ( <xValue> == NIL )
#  translate ISARRAY( <xValue> )     => hb_IsArray( <xValue> )
#  translate ISBLOCK( <xValue> )     => hb_IsBlock( <xValue> )
#  translate ISCHARACTER( <xValue> ) => hb_IsString( <xValue> )
#  translate ISDATE( <xValue> )      => hb_IsDate( <xValue> )
#  translate ISLOGICAL( <xValue> )   => hb_IsLogical( <xValue> )
#  translate ISMEMO( <xValue> )      => hb_IsMemo( <xValue> )
#  translate ISNUMBER( <xValue> )    => hb_IsNumeric( <xValue> )
#  translate ISOBJECT( <xValue> )    => hb_IsObject( <xValue> )
#endif

#translate ISCHAR( <xValue> )    => hb_IsString( <xValue> )
#translate ISSTRING( <xValue> )  => hb_IsString( <xValue> )
#translate ISNUMERIC( <xValue> ) => hb_IsNumeric( <xValue> )

#translate IFNIL( <v1>,<exp1>,<exp2> )       => iif( (<v1>) == NIL,<exp1>,<exp2> )
#translate IFARRAY( <v1>,<exp1>,<exp2> )     => iif( ISARRAY( <v1> ),<exp1>,<exp2> )
#translate IFBLOCK( <v1>,<exp1>,<exp2> )     => iif( ISBLOCK( <v1> ),<exp1>,<exp2> )
#translate IFCHARACTER( <v1>,<exp1>,<exp2> ) => iif( ISCHARACTER( <v1> ),<exp1>,<exp2> )
#translate IFCHAR( <v1>,<exp1>,<exp2> )      => iif( ISCHAR( <v1> ),<exp1>,<exp2> )
#translate IFSTRING( <v1>,<exp1>,<exp2> )    => iif( ISSTRING( <v1> ),<exp1>,<exp2> )
#translate IFDATE( <v1>,<exp1>,<exp2> )      => iif( ISDATE( <v1> ),<exp1>,<exp2> )
#translate IFLOGICAL( <v1>,<exp1>,<exp2> )   => iif( ISLOGICAL( <v1> ),<exp1>,<exp2> )
#translate IFNUMBER( <v1>,<exp1>,<exp2> )    => iif( ISNUMBER( <v1> ),<exp1>,<exp2> )
#translate IFNUMERIC( <v1>,<exp1>,<exp2> )   => iif( ISNUMERIC( <v1> ),<exp1>,<exp2> )
#translate IFOBJECT( <v1>,<exp1>,<exp2> )    => iif( ISOBJECT( <v1> ),<exp1>,<exp2> )
#translate IFEMPTY( <v1>,<exp1>,<exp2> )     => iif( EMPTY( <v1> ),<exp1>,<exp2> )

/////////////////////////////////////
// Abbreviated flow control modifiers
/////////////////////////////////////

#xcommand BREAKIF <log>  => IF ( <log> ) ; BREAK ; END
#xcommand EXITIF <log>   => IF ( <log> ) ; EXIT ; END
#xcommand LOOPIF <log>   => IF ( <log> ) ; LOOP ; END

// Extended commands
// ============================================================================

/* REPEAT ... UNTIL support */
#command  REPEAT         => DO WHILE .T.
#command  UNTIL <lCond>  => IF !<lCond> ; EXIT ; END ; ENDDO

#ifndef HB_COMMON_CH_
   /* Friendly logical aliases */
#  define TRUE	.T.
#  define FALSE	.F.
#  define YES	.T.
#  define NO	.F.

   /* DEFAULT and UPDATE commands */
#  xcommand DEFAULT <v1> TO <x1> [, <vn> TO <xn> ] => ;
				IF <v1> == NIL ; <v1> := <x1> ; END ;
				[; IF <vn> == NIL ; <vn> := <xn> ; END ]

#  command UPDATE <v1> IF <exp> TO <v2> ;
	=>				;
	IF <exp> ; <v1> := <v2> ; END
#endif

// ============================================================================

#define MAKELRESULT( lw, hw )          MAKELONG( lw, hw ) 

#ifndef HB_SYMBOL_UNUSED
   #define HB_SYMBOL_UNUSED( symbol )  ( ( symbol ) )
#endif

#xtranslate frac( <num> ) => ( <num> - int( <num> ) )

// ============================================================================

#xtranslate cFileName( <cPathMask> ) => hb_FNameNameExt( <cPathMask> )

#xtranslate ChangeFileExt( <cFile>, <cExt> ) ;
=> ;
cFilePath( <cFile> ) + "\" + cFileNoExt( <cFile> ) + <cExt>

#xtranslate hb_Ccompiler()                => Eval( {| c | iif( Empty( c := BorlandC() ), hb_Compiler(), c ) } )

#ifndef __XHARBOUR__
#  include "hbver.ch"
//#define __WIN98__

#ifndef __WIN98__
#  include "hbgtinfo.ch"
#  xtranslate gtSetClipboard( <x> )       => hb_gtInfo( HB_GTI_CLIPBOARDDATA, <x> )
#  xtranslate gtGetClipboard()            => hb_gtInfo( HB_GTI_CLIPBOARDDATA )

#  xtranslate RetrieveTextFromClipboard() => gtGetClipboard()
#  xtranslate CopyToClipboard ( <x> )     => gtSetClipboard( <x> )
#  xtranslate CopyToClipboard ()          => gtSetClipboard( "" )
#endif
//#undef __WIN98__

/* SWITCH ... ; CASE ... ; DEFAULT ; ... ; END */
#  xcommand DEFAULT                       => OTHERWISE

/* FOR EACH hb_enumIndex() */
#  xtranslate hb_enumIndex( <!v!> )       => <v>:__enumIndex()

/* TRY / CATCH / FINALLY / END */
#xcommand TRY                             => BEGIN SEQUENCE WITH __BreakBlock()
#xcommand CATCH [<!oErr!>]                => RECOVER [USING <oErr>] <-oErr->
#xcommand FINALLY                         => ALWAYS

/* workaround for problem with command using FIELDS keyword which can
   wrongly translate FIELD->fieldname.
*/
#translate FIELD-><!name!> => _FIELD-><name>

#define BLANK_DATE            hb_SToD()

#if ( __HARBOUR__ - 0 > 0x020000 )
#  xtranslate CurDrive()                  => hb_CurDrive()
#  xtranslate dbPack()                    => hb_dbPack()
#  xtranslate dbZap()                     => hb_dbZap()
#endif

#if ( __HARBOUR__ - 0 < 0x030200 )
#  xtranslate hb_IsFunction( <c> )        => ( Type( <c> + "()" ) == "UI" )
#  xtranslate hb_default( @<v>, <x> )     => iif( StrTran( ValType( <v> ), "M", "C" ) == StrTran( ValType( <x> ), "M", "C" ), Nil, <v> := <x> )
#  xtranslate hb_defaultValue( <v>, <x> ) => iif( StrTran( ValType( <v> ), "M", "C" ) == StrTran( ValType( <x> ), "M", "C" ), <v>, <x> )
#  xtranslate __defaultNIL( @<v>, <x> )   => ( <v> := iif( <v> == NIL, <x>, <v> ) )
#  xtranslate __MvGetDef( <x> , <v> )     => iif( __MvExist ( <x> ), __MvGet( <x> ), iif( ValType( <v> ) <> "U", <v>, NIL ) )
#  xtranslate __MvGetDef( <x> )           => iif( __MvExist ( <x> ), __MvGet( <x> ), NIL )
#  xtranslate hb_cdpCharMax()             => 255
#  xtranslate hb_osIsWin10()              => '10' $ WinVersion() \[ 1 ]
#  xtranslate hb_BLen( <c> )              => Len( <c> )
#endif

#if ( __HARBOUR__ - 0 > 0x030200 )
#  xtranslate hb_oemtoansi( <x> )         => win_oemtoansi( <x> )
#  xtranslate __MvGetDef( <x> , <v> )     => iif( __MvExist ( <x> ), __MvGet( <x> ), iif( ValType( <v> ) <> "U", <v>, NIL ) )
#  xtranslate __MvGetDef( <x> )           => iif( __MvExist ( <x> ), __MvGet( <x> ), NIL )
#  xtranslate hb_osIsWin10()              => os_IsWin10()
#endif

#  xtranslate IsExe64()                   => ( hb_Version( HB_VERSION_BITWIDTH ) == 64 )
#  xtranslate GetComputerName()           => NetName()
#  xtranslate GetUserName()               => hb_UserName()
#  xtranslate GetExeFilename()            => hb_ProgName()
#  xuntranslate AIns(                     =>
#  xuntranslate ADel(                     =>
#  xtranslate AIns( <a>, <n>, [<x,...>] ) => hb_AIns( <a>, <n>, <x> )
#  xtranslate ADel( <a>, <n>, <l> )       => hb_ADel( <a>, <n>, <l> )
#  xuntranslate AScan(                    =>
#  xuntranslate At(                       =>
#  xtranslate AScan(<a>,<b>,[<c>],[<d>],<e>) ;
                                          => hb_AScan( <a>, <b>, <c>, <d>, <e> )
#  xtranslate At( <a>, <b>, [<x,...>] )   => hb_At( <a>, <b>, <x> )
#else
#ifdef __XCC__
#  xtranslate hb_FileExists( <c> )        => File( <c> )
#endif
#  xtranslate GetComputerName()           => NetName()
#  xtranslate GetUserName()               => NetName( 1 )
#  xtranslate GetExeFilename()            => ExeName()
#  xtranslate hb_IsFunction( <c> )        => ( Type( <c> + "()" ) == "UI" )
#  xtranslate hb_FNameDir( <cFile> )      => cFilePath( <cFile> ) + "\"
#  xtranslate hb_MemoRead( [<x,...>] )    => MemoRead( <x> ) 
#  xtranslate hb_Filedelete( [<x>] )      => FErase( <x> ) 
#  xtranslate hb_ADel( [<x,...>] )        => ADel( <x> ) 
#  xtranslate hb_default( @<v>, <x> )     => iif( StrTran( ValType( <v> ), "M", "C" ) == StrTran( ValType( <x> ), "M", "C" ), Nil, <v> := <x> )
#  xtranslate hb_defaultValue( <v>, <x> ) => iif( StrTran( ValType( <v> ), "M", "C" ) == StrTran( ValType( <x> ), "M", "C" ), <v>, <x> )
#  xtranslate __defaultNIL( @<v>, <x> )   => ( <v> := iif( <v> == NIL, <x>, <v> ) )
#  xtranslate __MvGetDef( <x> , <v> )     => iif( __MvExist ( <x> ), __MvGet( <x> ), iif( ValType( <v> ) <> "U", <v>, NIL ) )
#  xtranslate __MvGetDef( <x> )           => iif( __MvExist ( <x> ), __MvGet( <x> ), NIL )
#  xtranslate hb_cdpCharMax()             => 255
#  xtranslate hb_DirBase()                => Left( ExeName(), RAt( '\', ExeName() ) )
#  xtranslate hb_osIsWin10()              => '10' $ WinVersion() \[ 1 ]

#define BLANK_DATE            CToD( "" )

#endif /* __XHARBOUR__ */

// ============================================================================
// Strongly Typed Variables			  (c) 1996-1997, Bryan Duchesne
// ============================================================================
/*
 * Adapted for MiniGUI Extended Edition by Grigory Filatov - 2010
 */

// This command replaces the traditional := assignment

#xcommand ASSIGN <cVar> := <cExp> ;
=> ;
<cVar> := _IsTyped( <cVar>, <cExp> )

// declare your variables as strongly typed
// ============================================================================

#xcommand LOCAL <cVar> AS <xtype:STRING,NUMERIC,LOGICAL,DATE,BLOCK,ARRAY> ;
             [,<cVarn> AS <xtypen:STRING,NUMERIC,LOGICAL,DATE,BLOCK,ARRAY>] ;
=> ;
LOCAL <cVar> := _SetType( <"xtype"> ) [, <cVarn> := _SetType( <"xtypen"> ) ]

#xcommand STATIC <cVar> AS <xtype:STRING,NUMERIC,LOGICAL,DATE,BLOCK,ARRAY> ;
             [,<cVarn> AS <xtypen:STRING,NUMERIC,LOGICAL,DATE,BLOCK,ARRAY>] ;
=> ;
STATIC <cVar> [, <cVarn> ] ;;
<cVar> := _SetType( <"xtype"> ) [, <cVarn> := _SetType( <"xtypen"> ) ]

#xcommand PUBLIC <cVar> AS <xtype:STRING,NUMERIC,LOGICAL,DATE,BLOCK,ARRAY> ;
             [,<cVarn> AS <xtypen:STRING,NUMERIC,LOGICAL,DATE,BLOCK,ARRAY>] ;
=> ;
PUBLIC <cVar> := _SetType( <"xtype"> ) [, <cVarn> := _SetType( <"xtypen"> ) ]

#xcommand PRIVATE <cVar> AS <xtype:STRING,NUMERIC,LOGICAL,DATE,BLOCK,ARRAY> ;
             [,<cVarn> AS <xtypen:STRING,NUMERIC,LOGICAL,DATE,BLOCK,ARRAY>] ;
=> ;
PRIVATE <cVar> := _SetType( <"xtype"> ) [, <cVarn> := _SetType( <"xtypen"> ) ]

// declare static variables as global value
// ============================================================================

#xcommand STATIC <cVar> AS GLOBAL VALUE <xVal> [,<cVarn> AS GLOBAL VALUE <xValn> ] ;
=> ;
_SetGetGlobal( <"cVar">, <xVal> ) [;; _SetGetGlobal( <"cVarn">, <xValn> ) ]

#xcommand ASSIGN GLOBAL <cVar> := <cExp> ;
=> ;
_SetGetGlobal( <"cVar">, <cExp> )
