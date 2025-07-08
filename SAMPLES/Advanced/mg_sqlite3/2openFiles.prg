/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2024 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 *
 * Меню ввода пароля и открытия файла и получение списка таблиц
 * Menu for entering password and opening file and getting list of tables
*/
#define  _HMG_OUTLOG
#include "minigui.ch"
#include "error.ch"
#include "hbsqlit3.ch"
#include "tsbrowse.ch"
#include "dbinfo.ch"

/////////////////////////////////////////////////////
FUNCTION Menu2OpenFile(oWnd)
   LOCAL cPsw, cFile, aTbl, cErr, cVers, aIndx
   LOCAL cMsg, a2Dim, nIndx, nI, cVal, cTbl

   ? ProcNL(), oWnd:ClassName
   oWnd:Cargo:cLine3 := ""

   cFile := oWnd:Cargo:cFile
   cVers := FileStr(cFile,15)
   IF UPPER(cVers) # UPPER("SQLite format 3")
       cMsg := IIF( App.Cargo:cLang == "RU", "Не тот формат файла !;Нет строки ",;
                      "Wrong file format!;No line ")
       cErr := cFile + ';;' + cMsg + ' "SQLite format 3"'
       AlertStop( cErr, , "ZZZ_B_STOP64", 64 )
       RETURN .F.
    ENDIF

   //Использование пароля входа к сожалению кардинально не решит проблему безопасности,
   //так как файл базы данных все так же, можно будет открыть любым SQLite browser'ом,
   //и прочитать данные из таблиц, так как данные не шифруются.
   //Шифрование базы данных в будущем планируется.
   cPsw  := ""   // резерв на будущее
   //oWnd:Cargo:cAccPsw := IIF( LEN(cPsw) > 0, cPsw, "" )
   //myGetPassword( oWnd )          // см. внизу
   //cPsw := oWnd:Cargo:cAccPsw

   // Список таблиц в файле
   aTbl  := SQLITE_TABLES(cFile)  // -> Util_sqlite.prg

   a2Dim := {}
   nIndx := 0
   FOR nI := 1 TO LEN(aTbl)
      cTbl  := aTbl[nI]
      aIndx := SQLITE_INDEXES( cTbl, cFile )
      cVal  := HB_ValtoExp(aIndx)
      cVal  := ALLTRIM( CharRem('{}',cVal) )
      IF LEN(cVal) > 0
         nIndx++
      ENDIF
      AADD(a2Dim, {cTbl,cVal} )
   NEXT

   ? "a2Dim=",a2Dim ; ?v a2Dim
   //AlertInfo(a2Dim, cFile, ,64,{ {0,120,215} } , , , .T.)

   oWnd:Cargo:cLine3 += "TABLES USER DATA: " + HB_NtoS( LEN(aTbl) )
   oWnd:Cargo:cLine3 += "  INDEX USER DATA: " + HB_NtoS(nIndx)

   // записать список таблиц на Cargo окна
   oWnd:Cargo:aTable  := aTbl    // запомнить/remember
   oWnd:Cargo:a2Table := a2Dim   // запомнить/remember

   rddSetDefault( "SQLMIX" )

   IF rddInfo( RDDI_CONNECT, { "SQLITE3", cFile } ) == 0
      cMsg := IIF( App.Cargo:cLang == "RU", "Невозможно подключиться к серверу SQLMIX !",;
                      "Unable to connect to SQLMIX server !")
      cMsg += ";" + cFile
      AlertStop( cMsg, "Error", , 64, {RED} )
      RETURN .F.
   ENDIF

   DO EVENTS

RETURN .T.

////////////////////////////////////////////////////////////////
//STATIC FUNCTION myGetPassword( oWnd )
FUNCTION myGetPassword( oWnd )
   LOCAL cMsg, cTtl, bInit, aBack

   SET MSGALERT BACKCOLOR TO oWnd:Cargo:aBClrPsw STOREIN aBack
   SET MSGALERT FONTCOLOR TO BLACK

   bInit := {||
      Local cMsg, oDlu, aFont, cFont, nSize
      Local y, x, w, h

      aFont := GetFontParam("DlgFont")
      cFont := aFont[1]
      nSize := aFont[2]
      oDlu  := oDlu4Font(nSize)
      x     := oDlu:Left
      w     := oDlu:W1   // oDlu:W(1.5)  // oDlu:W2  // задаем размер по width для Label
      h     := oDlu:H1 + 6
      This.Say_01.Row    := 20
      This.Say_01.Height := h * 2
      y     := This.Say_01.Row + This.Say_01.Height + 25 //oDlu:Top

      This.Topmost := .F.
      IF !HB_ISOBJECT( This.Cargo ) ; This.Cargo := oHmgData()
      ENDIF
      This.Cargo:lClose := .F.
      This.Cargo:o2Wnd  := oWnd:Cargo
      This.OnInterActiveClose := {|| This.Cargo:lClose }    // обязательно !!!
      oWnd:Cargo:cGetValue := "+"

      //@ y,x LABEL Lbl_1 WIDTH oDlu:W1 HEIGHT oDlu:H1 FONT "DlgFont" ;
      //      VALUE '№:' VCENTERALIGN FONTCOLOR WHITE TRANSPARENT
      //  x += This.Lbl_1.Width + oDlu:GapsWidth

      @ y,x TEXTBOX Get_1 WIDTH This.ClientWidth - x * 2 HEIGHT h ;
            VALUE oWnd:Cargo:cAccPsw FONT "DlgFont" MAXLENGTH 60
        y += This.Get_1.Height + 2 //oDlu:GapsHeight
        x := oDlu:Left

      IF App.Cargo:cLang == "RU"
         cMsg := "Файл может быть без пароля, тогда нажмите Отмена"
      ELSE
         cMsg := "The file may be without a password, then press Cancel"
      ENDIF
      @ y,x LABEL Lbl_2 WIDTH This.ClientWidth - x * 2 HEIGHT h-2 FONT "Comic Sans MS";
        SIZE nSize-1  VALUE cMsg VCENTERALIGN CENTERALIGN FONTCOLOR RED TRANSPARENT
        x += This.Lbl_2.Width + oDlu:GapsWidth
      //@ y,x TEXTBOX Get_2 WIDTH This.ClientWidth - x - oDlu:Left HEIGHT h ;
      //                    VALUE "Get Value 2" FONT "DlgFont" MAXLENGTH 30
        y := This.Btn_01.Row + oDlu:Top * 2 + oDlu:GapsHeight
        This.Btn_01.Row := y
        This.Btn_02.Row := y
        This.Height := This.Height + oDlu:Top * 2
        This.Btn_01.Action := {|| _wPost(99,, This.Get_1.Value) }
        This.Btn_02.Action := {|| _wPost(99) }
        This.Get_1.SetFocus
        _PushKey( VK_END )
        (This.Object):Event(99, {|ow,ky,cv|
                      ? ProcNL(), ow:Name,ky,cv
                      IF !Empty(cv)
                         //o2Crg:cGetValue := cv
                         oWnd:Cargo:cGetValue := cv
                         //MsgBox("Get_1 = "+ ky:cGetValue + CRLF + ;
                         //       "Text2 = "+ ky:cText2, "Press OK")
                      ELSE
                         oWnd:Cargo:cGetValue := ""
                      ENDIF
                      DO EVENTS
                      ow:Cargo:lClose := .T.
                      ow:Release()
                      Return Nil
                      })
      Return Nil
     }

   IF App.Cargo:cLang == "RU"
      cMsg := "Ввод пароля для файла БД: "
      cTtl := "Внимание!"
   ELSE
      cMsg := "Entering password for DB file: "
      cTtl := "Attention!"
   ENDIF
   cMsg += ";" + cFileNoPath(oWnd:Cargo:cFile) + SPACE(5)
   AlertOKCancel( cMsg, cTtl, , "iAccPass64", 64, { {56,196,56}, RED }, .T., bInit )

   oWnd:Cargo:cAccPsw := oWnd:Cargo:cGetValue  // вернуть исправленный пароль

   SET MSGALERT BACKCOLOR TO aBack[1]
   SET MSGALERT FONTCOLOR TO aBack[2]

RETURN Nil

