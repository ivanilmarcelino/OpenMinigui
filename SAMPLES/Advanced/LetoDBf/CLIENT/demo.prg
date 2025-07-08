/*
 * MINIGUI - Harbour Win32 GUI library Demo
*/

#define _HMG_OUTLOG

#include "hmg.ch"
#include "TSBrowse.ch"

REQUEST DBFCDX, LETO, HB_MEMIO
REQUEST rddinfo
REQUEST leto_VarGet, leto_varSet, leto_varGetCached, leto_varDel
REQUEST DbSetIndex, DbClearIndex
REQUEST DBORDERINFO, ORDLISTCLEAR, ORDBAGCLEAR, ORDDESTROY
REQUEST LETO_DBEVAL, DBINFO

MEMVAR oMain

#define _WAIT_  "... W A I T ..."

PROCEDURE Main( cPath )
   LOCAL i, j, k, hSpl, cLetoVer := ""
   LOCAL nPort := 2812

   SetsEnv()

   IF Empty( cPath )
      cPath := "127.0.0.1:2812"
   ENDIF

   If ( i := AT(".", cPath) ) > 0 .and. ( k := AT(":", cPath) ) > 0 .and. k > i
      j := "//" + cPath + iif( ":" $ cPath, "", ":" + ALLTRIM( STR( nPort ) ) )
      j += Iif( Right(cPath,1) == "/", "", "/" )
      IF leto_Connect( j ) < 0
         MsgStop('Connect error : '+hb_ntos(LETO_CONNECT_ERR())+CRLF+CRLF+ ;
                  upper(LETO_CONNECT_ERR(.T.))+' !', 'LetoDBf')
         RETURN
      ELSE
         cLetoVer := LETO_GetServerVersion()
         leto_Disconnect()
      ENDIF
   EndIf

   DEFINE FONT Serif_N FONTNAME "Times New Roman" SIZE 8
   DEFINE FONT Serif_B FONTNAME "Times New Roman" SIZE 8 BOLD

   DEFINE WINDOW wMain AT 0, 0 ;
      WIDTH System.ClientWidth * 0.9 HEIGHT System.ClientHeight * 0.9 ;
      TITLE "LetoDBf testing"  ;
      MAIN NOMAXIMIZE NOSIZE

      PUBLIC oMain := This.Object

      DEFINE SPLITBOX HANDLE hSpl
		DEFINE TOOLBAR ToolBar_1 CAPTION ""                BUTTONSIZE 100,32 FLAT
			BUTTON 01 CAPTION 'Test_dbf.prg'  PICTURE 'n1'  ACTION wPost(1)   SEPARATOR
			BUTTON 02 CAPTION 'Test_dbfe.prg' PICTURE 'n2'  ACTION wPost(2)   SEPARATOR
			BUTTON 03 CAPTION 'Test_file.prg' PICTURE 'n3'  ACTION wPost(3)   SEPARATOR
			BUTTON 04 CAPTION 'Test_filt.prg' PICTURE 'n4'  ACTION wPost(4)   SEPARATOR
			BUTTON 05 CAPTION 'Test_mem.prg'  PICTURE 'n5'  SEPARATOR         WHOLEDROPDOWN
			BUTTON 06 CAPTION 'Test_ta.prg'   PICTURE 'n6'  ACTION wPost(6)   SEPARATOR
			BUTTON 07 CAPTION 'Test_tr.prg'   PICTURE 'n7'  ACTION wPost(7)   SEPARATOR
			BUTTON 08 CAPTION 'Test_var.prg'  PICTURE 'n8'  ACTION wPost(8)   SEPARATOR
			BUTTON 09 CAPTION 'Demo4.prg'     PICTURE 'n9'  ACTION wPost(9)   SEPARATOR
         DEFINE DROPDOWN MENU BUTTON 05
            ITEM "__AUTOINC__"                                 IMAGE 'n1'  ACTION wPost(51)
            ITEM "__TRANSACT__"                                IMAGE 'n2'  ACTION wPost(52)
            ITEM "__AUTOINC__+__TRANSACT__"                    IMAGE 'n3'  ACTION wPost(53)
            SEPARATOR                                         
            ITEM "__MEM_IO__+__AUTOINC__"                      IMAGE 'n4'  ACTION wPost(54)
            ITEM "__MEM_IO__+__TRANSACT__"                     IMAGE 'n5'  ACTION wPost(55)
            ITEM "__MEM_IO__+__AUTOINC__+__TRANSACT__"         IMAGE 'n6'  ACTION wPost(56)
            SEPARATOR
            ITEM "__LZ4__+__AUTOINC__"                         IMAGE 'n7'  ACTION wPost(57)
            ITEM "__LZ4__+__TRANSACT__"                        IMAGE 'n8'  ACTION wPost(58)
            ITEM "__LZ4__+__AUTOINC__+__TRANSACT__"            IMAGE 'n9'  ACTION wPost(59)
            SEPARATOR
            ITEM "__LZ4__+__MEM_IO__+__AUTOINC__"              IMAGE 'n10' ACTION wPost(60)
            ITEM "__LZ4__+__MEM_IO__+__TRANSACT__"             IMAGE 'n11' ACTION wPost(61)
            ITEM "__LZ4__+__MEM_IO__+__AUTOINC__+__TRANSACT__" IMAGE 'n12' ACTION wPost(62)
         END MENU
		END TOOLBAR

		DEFINE TOOLBAR ToolBar_2 CAPTION ""                BUTTONSIZE 42,32 FLAT
			BUTTON Exit  CAPTION 'Exit'    PICTURE 'exit'   ACTION wPost(99)
		END TOOLBAR
		END SPLITBOX

   DEFINE STATUSBAR
      STATUSITEM ""                               FONTCOLOR BLUE
      STATUSITEM "" WIDTH This.ClientWidth * 0.20 FONTCOLOR BLUE
      STATUSITEM "" WIDTH This.ClientWidth * 0.30 FONTCOLOR BLUE
      STATUSITEM "" WIDTH This.ClientWidth * 0.20 FONTCOLOR BLUE
   END STATUSBAR

   WITH OBJECT This.Object
   :Event( 1, {|ow| ow:SendMsg(22), test_dbf (ow, cPath), ow:SendMsg(23) })
   :Event( 2, {|ow| ow:SendMsg(22), test_dbfe(ow, cPath), ow:SendMsg(23) })
   :Event( 3, {|ow| ow:SendMsg(22), test_file(ow, cPath), ow:SendMsg(23) })
   :Event( 4, {|ow| ow:SendMsg(22), test_filt(ow, cPath), ow:SendMsg(23) })
   :Event( 6, {|ow| ow:SendMsg(22), test_ta  (ow, cPath), ow:SendMsg(23) })
   :Event( 7, {|ow| ow:SendMsg(22), test_tr  (ow, cPath), ow:SendMsg(23) })
   :Event( 8, {|ow| ow:SendMsg(22), test_var (ow, cPath), ow:SendMsg(23) })
   :Event( 9, {|ow| demo4(ow, cPath) }) 

   :Event(51, {|ow| ow:SendMsg(22), test_mem (ow, cPath, 1 ), ow:SendMsg(23) })
   :Event(52, {|ow| ow:SendMsg(22), test_mem (ow, cPath, 2 ), ow:SendMsg(23) })
   :Event(53, {|ow| ow:SendMsg(22), test_mem (ow, cPath, 3 ), ow:SendMsg(23) })
   :Event(54, {|ow| ow:SendMsg(22), test_mem (ow, cPath, 4 ), ow:SendMsg(23) })
   :Event(55, {|ow| ow:SendMsg(22), test_mem (ow, cPath, 5 ), ow:SendMsg(23) })
   :Event(56, {|ow| ow:SendMsg(22), test_mem (ow, cPath, 6 ), ow:SendMsg(23) })
   :Event(57, {|ow| ow:SendMsg(22), test_mem (ow, cPath, 7 ), ow:SendMsg(23) })
   :Event(58, {|ow| ow:SendMsg(22), test_mem (ow, cPath, 8 ), ow:SendMsg(23) })
   :Event(59, {|ow| ow:SendMsg(22), test_mem (ow, cPath, 9 ), ow:SendMsg(23) })
   :Event(60, {|ow| ow:SendMsg(22), test_mem (ow, cPath, 10), ow:SendMsg(23) })
   :Event(61, {|ow| ow:SendMsg(22), test_mem (ow, cPath, 11), ow:SendMsg(23) })
   :Event(62, {|ow| ow:SendMsg(22), test_mem (ow, cPath, 12), ow:SendMsg(23) })

   :Event(20, {|  | This.Edit.Value := ""                         })
   :Event(21, {|  | This.Edit.Value := hb_memoread('_Msglog.txt') })

   :Event(22, {|ow| ow:StatusBar:Say(_WAIT_) })
   :Event(23, {|ow| ow:StatusBar:Say(''), ow:StatusBar:Say('', 2) })

   :Event(99, {|ow| ow:Release() })

   :StatusBar:Say(MiniGUIVersion(), 3)
   :StatusBar:Say(cLetoVer        , 4)
   END WITH

   y := x := 0
   g := 2
   w := 90
   h := 30

   y += GetWindowHeight(hSpl)
   w := This.ClientWidth
   h := This.ClientHeight - This.StatusBar.Height - y

   @ y, x EDITBOX Edit  WIDTH w  HEIGHT h  VALUE '' ;
          FONT "Courier New" Size 12      

   END WINDOW

   wMain.Center
   wMain.Activate

RETURN

