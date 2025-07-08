/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
 *
 * Copyright 2012 Grigory Filatov <gfilatov@inbox.ru>
 *
 * Revisioned on 2018 by Pierpaolo Martinello
*/

#include "minigui.ch"
#include "hbhrb.ch"

MEMVAR cPrg

PROCEDURE Main

   PRIVATE cPrg

   cPrg := ;
      "PROCEDURE p()" + hb_eol() + ;
      "   SetProperty ( 'Win_Main', 'StatusBar' , 'Item' , 1 ,'  Hello World!' ) " + hb_eol() + ;
      "   Msgdebug( 'Hello World!' )" + hb_eol() + ;
      "RETURN" + hb_eol() + ;
      "  " + hb_eol() + ;
      "FUNCTION Func1( xvalue )" + hb_eol() + ;
      "         Msginfo( 'Func1 ',xValue )" + hb_eol() + ;
      "RETURN xvalue" + hb_eol() + ;
      "  " + hb_eol() + ;
      "FUNCTION Func3( )" + hb_eol() + ;
      [         MsgExclamation( "This is a internal Msgexclamation from 'Func3'" )] + hb_eol() + ;
      [RETURN "This is the current time: "+time()]

   DEFINE WINDOW Win_Main ;
         AT 0, 0 ;
         WIDTH 600 ;
         HEIGHT 500 ;
         TITLE 'Harbour Script Usage Demo' ;
         ICON "HMGE.ico" ;
         MAIN ;
         ON INIT SetDim() ;
         ON SIZE SetDim() ;
         ON MAXIMIZE SetDim() ;
         ON RESTORE SetDim() ;
         FONT 'Times New Roman' SIZE 12

      DEFINE STATUSBAR ;
            FONT 'Times New Roman' SIZE 12
         STATUSITEM '' WIDTH 200
         STATUSITEM '  Pierpaolo Martinello 2018' WIDTH 190
      END STATUSBAR

      @ 45, 10 EDITBOX Edit_1 ;
         WIDTH win_Main.WIDTH - 35 ;
         HEIGHT win_Main.HEIGHT - 115 ;
         VALUE "" ;
         READONLY ;

         @ 10, 10 BUTTONEX Btn_1 ;
         CAPTION 'Run Script From Variable' ;
         WIDTH 200 ;
         HEIGHT 25 ;
         ONCLICK RunScript() ;
         BACKCOLOR { 255, 155, 0 } ;
         ON GOTFOCUS ( WIN_MAIN.EDIT_1.VALUE := cPrg, WIN_MAIN.EDIT_1.BACKCOLOR := { 255, 155, 0 } ) ;
         NOXPSTYLE

      @ 10, 370 BUTTONEX Btn_2 ;
         CAPTION 'Run Script From File' ;
         WIDTH 200 ;
         HEIGHT 25 ;
         ONCLICK WriteFromFile () ;
         BACKCOLOR { 255, 255, 102 } ;
         ON GOTFOCUS ( WIN_MAIN.EDIT_1.VALUE := hb_MemoRead( "LibPRG.prg" ), WIN_MAIN.EDIT_1.BACKCOLOR := { 255, 255, 102 } ) ;
         NOXPSTYLE

      ON KEY ESCAPE ACTION Win_Main.RELEASE

   END WINDOW

   Win_Main.StatusBar.Item( 1 ) := '  HMG Power Ready'

   CENTER WINDOW Win_Main
   ACTIVATE WINDOW Win_Main

   RELEASE cPrg

RETURN

PROCEDURE SetDim()
   WIN_MAIN.BTN_2.COL := WIN_MAIN.WIDTH - 225
   WIN_MAIN.EDIT_1.WIDTH := WIN_MAIN.WIDTH - 35
   WIN_MAIN.EDIT_1.HEIGHT := WIN_MAIN.HEIGHT - 115
   IF GetProperty ( "WIN_MAIN", "WIDTH" ) < 600 .OR. GetProperty ( "WIN_MAIN", "HEIGHT" ) < 500
      // MsgStop("Wrong Size!"+CRLF+CRLF+ "Now i fix them!","Error")
      WIN_MAIN.WIDTH := 600
      WIN_MAIN.HEIGHT := 500
      SetDim()
   ENDIF

RETURN

PROCEDURE RunScript // encrypted version

   LOCAL cContent, hHandle_Hrb, cHrbCode
   LOCAL cFile := "Script1.hrb", cPassword := "My_Password_Key"

   // ferase("Script1.hrb")

   IF ! File( cFile )

      cHrbCode := hb_compileFromBuf( cPrg, "harbour", "-n", "-w3", "-es2", "-q0" )

      MemoWrit( cFile, sx_Encrypt( cHrbCode, cPassword ) )

   ENDIF

   cContent := sx_Decrypt( MemoRead( cFile ), cPassword )

   hHandle_Hrb := hb_hrbLoad( HB_HRB_BIND_DEFAULT, cContent )

   IF Empty( Hhandle_Hrb )
      MsgInfo( "Bug in script" )
   ELSE
      // Run the script but execute only the first function
      hb_hrbDo( hHandle_Hrb )
      // but you recall the function using hb_ExecFromArray
      MsgInfo( hb_ExecFromArray( "Func3" ), "The return value from Func3 is:" )
   ENDIF
   // Close the script
   hb_hrbUnload( hHandle_Hrb )

RETURN

FUNCTION WriteFromFile ()

   LOCAL cByteCode, hHandle_Hrb
   LOCAL aFuncName, aFuncReturn := Array( 3 )

   FErase ( "Script2.hrb" )

   cPrg := hb_MemoRead( "LibPRG.prg" )

   // cByteCode := hb_compileFromBuf( cPrg, "harbour", "-n", "-w3", "-es2", "-q0","-IC:\Minigui\harbour\include", "-IC:\Minigui\include")

   // Compile but require include folder available on the client pc, otherwise return error!
   cByteCode := hb_compileBuf( "harbour", "LibPRG.prg", "-n", "-w3", "-es2", "-q0", "-IC:\Minigui\harbour\include", "-IC:\Minigui\include" )

   IF Empty( cByteCode )
      MsgStop( "Compile error !" )
   ELSE
      // Save To File
      hb_MemoWrit( "Script2.hrb", cByteCode )

      // Load From saved File
      cByteCode := hb_MemoRead( "Script2.hrb" )

      hHandle_Hrb := hb_hrbLoad( HB_HRB_BIND_DEFAULT, cByteCode )

      // retrieve a Function List
      aFuncName := hb_hrbGetFunList( hHandle_Hrb, HB_HRB_FUNC_PUBLIC )

      // Show the List
      msgdebug( "Function List -> ", aFuncName )

      // pass a parameter to the function
      aFuncReturn[ 1 ] := hb_ExecFromArray( aFuncName[ 1 ], { "Hi guys!" } )

      // pass two parameters to the function
      aFuncReturn[ 2 ] := hb_ExecFromArray( aFuncName[ 2 ], { 33, "Param2", Date() } )

      // does not pass parameters to the function
      aFuncReturn[ 3 ] := hb_ExecFromArray( aFuncName[ 3 ] )

      // Show the return values
      msgdebug( "Return values of the function ->", aFuncReturn )

      // Close the script
      hb_hrbUnload( hHandle_Hrb )

   ENDIF

RETURN NIL
