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
#include "tsbrowse.ch"
#include "hbclass.ch"

#DEFINE adSchemaTables 20
#DEFINE adSchemaColumns 4
Static  bErrStd := {|oE| if(oE:GenCode==5, 0, Break(oE))}
///////////////////////////////////////////////////////////////////////////////
FUNCTION Menu2OpenFile(oWnd)
   LOCAL cPsw, cFpsw, oTbl, cStr, cFile, oConx, oRSet, cTbl, aTbl, i, aRSet
   LOCAL cErr, oErr, bErr, lOpen, cLng, oRS, nMode

   ? ProcNL(), oWnd:ClassName
   oWnd:Cargo:cLine3 := ""

   cPsw  := ""
   cFile := oWnd:Cargo:cFile
   cFpsw := cFile + '.psw'
   IF FILE(cFpsw)  // если есть файл с паролем
      cPsw := HB_MEMOREAD(cFpsw)
   ENDIF

   oWnd:Cargo:cAccPsw := IIF( LEN(cPsw) > 0, cPsw, "" )
   myGetPassword( oWnd )          // см. внизу
   cPsw := oWnd:Cargo:cAccPsw

   lOpen := .F.
   bErr  := ErrorBlock(bErrStd)
   BEGIN SEQUENCE //WITH { |e|break(e) }
      // открытие файла Access и получение списка таблиц
      IF '.accdb' $ LOWER(cFile)
         cStr := "Provider=Microsoft.ACE.OLEDB.16.0"
      ELSE
         cStr := "Provider=Microsoft.Jet.OLEDB.4.0"
      ENDIF
      cStr += ";Data Source=" + cFile
      cStr += ";User Id=admin;Password=" + cPsw + ";"

      // задание Mode - открытия файла  ---vvv
      //"Provider=Microsoft.ACE.OLEDB.12.0;Mode=Share Exclusive;User ID=Admin;" + ;
      //"Data Source=c:\dev\vfp\test_access\DatabaseBorey.accdb;"
      //   Про режимы доступа в ADO:
      //   https://learn.microsoft.com/en-us/sql/ado/reference/ado-api/connectmodeenum?view=sql-server-ver16
      //
      //   https://learn.microsoft.com/en-us/answers/questions/247558/what-are-the-possible-mode-for-provider-microsoft
      //   Microsoft плохо документирует это. Я думаю, что возможные настройки следующие, но я не уверен:
      //   Read — Только чтение.
      //   ReadWrite — Чтение и запись.
      //   Share Deny None — Ни чтение, ни запись не могут быть запрещены другим.
      //   Share Deny Read — Запрещает другим открывать в режиме чтения.
      //   Share Deny Write — Запрещает другим открывать в режиме записи.
      //   Share Exclusive — Запрещает другим открывать в режиме чтения/записи.
      //   Write — Только запись.

      oConx := TOleAuto():new( "ADODB.connection" )
      oConx:ConnectionString := cStr
      oConx:Open()
      nMode := oConx:Mode
      App.Cargo:oConx := oConx   // запомнить/remember

      lOpen := .T.

   RECOVER USING oErr
      // обработка возникшей ошибки, данные о ней в oErr
      cLng := IIF( App.Cargo:cLang == "RU", "Или пароль не верен !",;
                     "Or the password is incorrect!")
      ? ProcNL(), oErr
      ? REPL(".",5), oErr:description, oErr:operation, oErr:genCode
      cErr := cFile + ';;' + oErr:description
      cErr += if(!Empty(oErr:operation),';'+oErr:operation,'')
      cErr += ' (' + HB_NtoS(oErr:genCode) + ');;' + cLng
      AlertStop( cErr, "Error", "ZZZ_B_STOP64", 64 )
      ? cErr
   END SEQUENCE
   ErrorBlock(bErr)

   IF lOpen

      oTbl := TOleAuto():new( "ADOX.Catalog" )
      oTbl:ActiveConnection := oConx
      App.Cargo:oTbl := oTbl   // запомнить/remember

      //msgdebug("TABLES TOTAL: ", oTbl:Tables:Count, VALTYPE(oTbl:Tables:Count) )
      oWnd:Cargo:cLine3 += "TABLES TOTAL: " + HB_NtoS( oTbl:Tables:Count )  // запомнить

      aTbl := {}
      /*For oRSet := 0 to oTbl:Tables:Count - 1
         cTbl := oTbl:Tables(oRSet):Name
         // пропуск системых таблиц Access
         If !( "MSys" $ cTbl ) .and. cTbl != "language"
            Aadd(aTbl, cTbl)
         Endif
      Next
      //msgdebug("TABLES USER DATA: ", aTbl)
      oWnd:Cargo:cLine3 += ",  TABLES USER DATA: " + HB_NtoS( LEN(aTbl) )
      */

      // лучше так
      oRS := oConx:OpenSchema(adSchemaTables)

      oRS:Filter := "TABLE_TYPE='TABLE'"
      ? "  TABLES USER DATA: " + HB_NtoS(oRs:RecordCount)

      DO WHILE !oRS:EOF()
         cTbl := oRS:Fields("TABLE_NAME"):Value
         Aadd(aTbl, cTbl)
         oRs:MoveNext()
         ? "   TABLE_NAME:", cTbl
      ENDDO
      oWnd:Cargo:cLine3 += "  TABLES USER DATA: " + HB_NtoS(LEN(aTbl))
      oWnd:Cargo:cLine3 += "  MODE: " + ModeAccess(nMode)
      ? oWnd:Cargo:cLine3

      // записать список таблиц
      oWnd:Cargo:aTable := aTbl   // запомнить/remember

      aRSet := ARRAY(LEN(aTbl))
      FOR i := 1 TO LEN(aTbl)

         oRSet := TOleAuto():New( "ADODB.RecordSet" )
         With Object oRSet
            :CursorLocation   := adUseClient
            :CursorType       := adOpenDynamic
            :LockType         := adLockOptimistic
            :ActiveConnection := oConx
            :Source           := "SELECT * FROM " + aTbl[i] //CUSTOMER"
            //:Open()
            //:Sort           := :Fields( 0 ):Name
         End With

         aRSet[i] := oRSet

      NEXT
      oWnd:Cargo:aRSet := aRSet // запомнить/remember

   ENDIF  // lOpen

   DO EVENTS

RETURN lOpen

////////////////////////////////////////////////////////////////
//STATIC FUNCTION myGetPassword( oWnd )
FUNCTION myGetPassword( oWnd )
   LOCAL cMsg, cTtl, bInit, aBack

   SET MSGALERT BACKCOLOR TO oWnd:Cargo:aBClrPsw STOREIN aBack
   SET MSGALERT FONTCOLOR TO BLACK

   bInit := {||
      Local ow := ThisWindow.Object
      Local cMsg, oDlu, aFont, cFont, nSize
      Local y, x, w, h, cObj, cForm, i, aObj

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
      This.Cargo:lClose       := .F.
      This.Cargo:o2Wnd        := oWnd:Cargo
      This.OnInterActiveClose := {|| This.Cargo:lClose }    // обязательно !!!
      oWnd:Cargo:cGetValue    := "+"

      aObj := HMG_GetFormControls(ow:Name) // все объекты
      cForm   := ow:Name
      //? ProcNL(), ow:Name, "aObj=", aObj ; ?v aObj
      For i :=  1 TO Len(aObj)
         If "Say_" $ aObj[i]
           cObj := aObj[i]
           cMsg := GetProperty( cForm, cObj, "Value" )
           If "-.-." $ cMsg
              y := GetProperty( cForm, cObj, "Row" )
              SetProperty( cForm, cObj, "Value", "" )
              EXIT
           Endif
         Endif
      Next
      y += This.&(cObj).Height
      ? "-.-.  y=", y

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
   cMsg += ";;" + cFileNoPath(oWnd:Cargo:cFile)
   cMsg += ";" + REPL("-.", 10)
   cMsg += ";;"  // обязательно !!!

   AlertOKCancel( cMsg, cTtl, , "iAccPass64", 64, { {56,196,56}, RED }, .T., bInit )

   oWnd:Cargo:cAccPsw := oWnd:Cargo:cGetValue  // вернуть исправленный пароль

   SET MSGALERT BACKCOLOR TO aBack[1]
   SET MSGALERT FONTCOLOR TO aBack[2]

RETURN Nil

////////////////////////////////////////////////////////////////////////////////
// Открываю файл Access.
// В каком режиме он открывается - монопольно или многопользовательском ?
STATIC FUNCTION ModeAccess(n)
   LOCAL cRet
/*Определяет режим доступа для изменения данных в сеансе. Возможные значения:
    adModeUnknown(0) - режим доступа не установлен или не может быть определён (по умолчанию).
    adModeRead(1) - режим только для чтения.
    adModeWrite(2) - режим только для записи.
    adModeReadWrite(3) - режим для чтения и записи.
    adModeShareDenyRead(4) - не разрешает открывать соединение на чтение другим пользователям.
    adModeShareDenyWrite(8) - не разрешает открывать соединение на запись другим пользователям.
    adModeShareExclusive(12) - не разрешает открывать соединение другим пользователям.
    adModeShareDenyNone(16) - разрешает открывать соединение с любым видом доступа другим пользователям.
Вы можете установить это свойство только тогда, когда объект Connection закрыт. */
   IF n == 0
      cRet := "ModeUnknown(0) - режим доступа не установлен или не может быть определён"
   ELSEIF n == 1
      cRet := "Read — только чтение"
   ELSEIF n == 2
      cRet := "Write — только запись"
   ELSEIF n == 3
      cRet := "ReadWrite — чтение и запись"
   ELSEIF n == 4
      cRet := "ShareDenyRead - не разрешает открывать соединение на чтение другим пользователям"
   ELSEIF n == 8
      cRet := "ShareDenyWrite - не разрешает открывать соединение на запись другим пользователям"
   ELSEIF n == 12
      cRet := "ShareExclusive - не разрешает открывать соединение другим пользователям"
   ELSEIF n == 16
      cRet := "ShareDenyNone - разрешает открывать соединение с любым видом доступа другим пользователям"
   ELSE
      cRet := "(" + HB_NtoS(n) + ") ???"
   ENDIF

RETURN cRet
