/*
 * MINIGUI - Harbour Win32 GUI library Demo
 * Copyright 2023 Sergej Kiselev <bilance@bilance.lv>
 * Copyright 2023 Verchenko Andrey <verchenkoag@gmail.com>
 *
 * Разное для TsbViewer() / Miscellaneous for TsbViewer()
*/

#include "minigui.ch"
#include "i_winuser.ch"

///////////////////////////////////////////////////////////////////
Function myGetPathTemp()         // где создавать временные файлы
   LOCAL cMsg, cPath, cFile := "\MG_test.txt"

   cPath := GetUserTempFolder()
   HB_MemoWrit( cPath + cFile, "// где создавать временные файлы ?" )
   IF !FILE(cPath + cFile)
      cPath := GetTempFolder()
      HB_MemoWrit( cPath + cFile, "// где создавать временные файлы ?" )
      IF !FILE(cPath + cFile)
         cMsg := "Error ! I can't find the folder for temporary files !;"
         cMsg += cPath + ";;" + ProcNL() + ";" + ProcNL(1)
         AlertStop( cMsg, "Result", "ZZZ_B_STOP64", 64 )
         cPath := GetStartUpFolder()
      ENDIF
   ENDIF

RETURN cPath

///////////////////////////////////////////////////////////////////
// color of the Active window caption
Function HMG_ColorWinActiveCaption()
   LOCAL aClr

   aClr := HMG_n2RGB( GetSysColor(COLOR_ACTIVECAPTION) )
   aClr := IIF( IsWin10OrLater(), Color10_ActiveCaption(), aClr )

RETURN aClr

///////////////////////////////////////////////////////////////////
Function Color10_ActiveCaption  // "Active window caption"
   LOCAL i, xKey, aClr := RED
   // Если флаг ColorPrevalence имеет значение 0 то цвет окна либо чёрный либо белый.
   // Если флаг имеет значение 1 то цвет окна берётся из значения AccentColor.
   i := win_regRead( "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\DWM\ColorPrevalence")
   IF i == 1
      xKey := win_regRead( "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\DWM\AccentColor")
      aClr := HMG_n2RGB(xKey)
   ELSE
      IF HMG_AppsUseTheme(.F.) == 0
         aClr := BLACK
      ELSE
         aClr := WHITE
      ENDIF
   ENDIF
RETURN aClr

///////////////////////////////////////////////////////////////////
Function HMG_AppsUseTheme(lRet)
  LOCAL i, cRet
  DEFAULT lRet := .T.
  i := win_regRead( "HKEY_CURRENT_USER\SOFTWARE\Microsoft\Windows\CurrentVersion\Themes\Personalize\AppsUseLightTheme")
  IF i == 0
     cRet := "(0) dark theme"
  ELSE
     cRet := "(1) light theme"
  ENDIF
RETURN IIF(lRet,cRet,i)

///////////////////////////////////////////////////////////////////
FUNCTION ProcNL(nVal)
   DEFAULT nVal := 0
RETURN "Call from: " + ProcName( nVal + 1 ) + "(" + ;
        hb_ntos( ProcLine( nVal + 1 ) ) + ") --> " + ;
        ProcFile( nVal + 1 )

//////////////////////////////////////////////////////////////
// Открыть таблицу или DBF
// Copyright 2023 Sergej Kiselev <bilance@bilance.lv>
FUNCTION myUseArea( cDbf, cAls, lShared, cRdd, cCdp, nWhl )
   LOCAL lRet := .T., cPth, cFil, cExt
   Default cAls := ""
   Default lShared := .T.
   Default cRdd := RddSetDefault()
   Default cCdp := "RU866"
   DEFAULT nWhl := 10

   If !hb_vfExists(cDbf)
      AlertStop("No database file !;" + cDbf + ";;" + ProcNL() + ";" + ProcNL(1) )
      RETURN .F.
   ENDIF

   NetErr( .F. )

   hb_FNameSplit(cDbf, @cPth, @cFil, @cExt)
   cDbf := hb_FNameMerge(cPth, cFil, "")

   IF LEN(cAls) == 0
      cAls := cFileNoPath(cDbf)
      cAls := SUBSTR(cAls,1,AT(".",cAls)-1)
   ENDIF

   IF SELECT(cAls) > 0
      AlertStop("There is already such an alias !;" + ;
                 "ALIAS()=" + cAls + ";"  + ;
                 cDbf + ";;" + ProcNL() + ";" + ProcNL(1) )
      RETURN .F.
   ENDIF

   SELECT 0
   If     empty (cAls)    ; cAls := '_XYZ_'+hb_ntos(select())
   ElseIf select(cAls) > 0; cAls += '_'    +hb_ntos(select())
   EndIf
   //!!!
   DO WHILE nWhl-- > 0
      lRet := .F.
      BEGIN SEQUENCE WITH { |e|break(e) }          // .F. - lReadonly
         DbUseArea(.T., cRdd, cDbf, cAls, lShared, .F., cCdp)
         lRet := ! NetErr() .and. Used()
      END SEQUENCE
      IF lRet; EXIT
      ENDIF
      wApi_Sleep(100)
   ENDDO
   // ^^^ - цикл главное, т.к. из за сбоя сети пробуем несколько раз открыть
   IF lRet
      dbGoTop()
   ENDIF
   DbSelectArea(cAls)
   ? ProcNL(), "Opening the database: " + cAls, "Number of attempts (10--) =", nWhl

RETURN lRet

///////////////////////////////////////////////////////////////////
FUNCTION myLogCargo(o,cMsg)
   LOCAL a2Dim, nI, a

   a2Dim  := o:GetAll()               // get an array with a key
   FOR EACH a IN a2Dim                // this is a list of ALL keys and their values
      nI := hb_EnumIndex( a )
      ? cMsg, nI, "Key =", a[1], "Val ="
      IF hb_IsArray(a[2])
         ?? SayHmgArray(a[2])
      ELSEIF hb_IsChar(a[2])
         ?? AtRepl( CRLF, a[2], " | " )
      ELSEIF hb_IsObject(a[2])
         ?? SayHmgObject(a[2])
      ELSE
         ?? a[2]
      ENDIF
   NEXT

RETURN NIL
//////////////////////////////////////////////////////////////////////////////////
FUNCTION SayHmgObject(ob)
   LOCAL cStr, aName

   IF !hb_IsObject(ob)
      RETURN " This is not an Object !"
   ENDIF

   cStr := "'OBJECT' " + ob:ClassName + " "
   IF ob:ClassName $ "THMGDATA,TKEYDATA,TTHRDATA"                // контейнеры
      cStr += "ARRAY["  + hb_NToS( Len( ob:GetAll() ) ) + "] "
      cStr += HB_ValToExp( ob:GetAll() )
   ELSEIF ob:ClassName == "TSBROWSE"                             // таблица
      cStr += ob:cParentWnd + " " + ob:cControlName + " "
      cStr += ob:cAlias + " " + HB_NtoS( ob:nLen )
      aName := __objGetMethodList( ob )
   ELSEIF ob:ClassName $ "TCNLDATA,TGETDATA,TSTBDATA,TTSBDATA"  // контролы
      cStr += ob:Name + " " + ob:Type + " " + ob:Index
      cStr += " " + HB_NtoS( ob:Handle )
   ELSEIF ob:ClassName $ "TINIDATA"                             //  ini file
      cStr += ob:cIni + " " + cValToChar(ob:lIni) + " "
      cStr += cValToChar( ob:lUtf8 )
   ELSEIF ob:ClassName $ "TWNDDATA"                             //  окно формы
      cStr += ob:Name + " "
      cStr += "ARRAY["  + hb_NToS( Len( ob:GETLISTTYPE() ) ) + "] "
      cStr += HB_ValToExp( ob:GETLISTTYPE() )
      cStr += " " + HB_NtoS( ob:CLIENTWIDTH ) + " " + HB_NtoS( ob:CLIENTHEIGHT)
   ELSE
      aName := __objGetMethodList( ob )
      cStr += HB_ValToExp(aName)
   ENDIF

RETURN cStr

//////////////////////////////////////////////////////////////////////////////////
FUNCTION SayHmgArray(a)
   LOCAL i, cStr := ""

   IF !hb_IsArray(a)
      RETURN " This is not an array !"
   ENDIF

   IF Len(a) > 0 .and. hb_IsArray(a[1])
      cStr += "{ "
      FOR i := 1 TO LEN(a)
         cStr += HB_ValToExp(a[i]) + IIF( i==LEN(a), "", "," )
      NEXT
      cStr += " }"
   ELSE
      cStr += HB_ValToExp(a)
   ENDIF
   cStr := "ARRAY["  + hb_NToS( Len(a) ) + "] = " + cStr

RETURN cStr


//////////////////////////////////////////////////////////////////////////////////
FUNCTION MsgListAllWinTsbView(cMenu)            // Список окон в программе
   LOCAL nI, cForm, aFrm, cMsg := ""

   aFrm := HMG_GetForms()
   cMsg += "Number of open windows: " + HB_NtoS(LEN(aFrm)) + CRLF

   FOR nI := 1 TO LEN(aFrm)
      cForm := UPPER(aFrm[nI])
      cMsg += HB_NtoS(nI) + ") "
      cMsg += ' Form: ' + cForm + ', Type: "'+_HMG_aFormType[nI]+'" '
      cMsg += ', Handle: '+HB_NtoS(_HMG_aFormHandles[nI])
      cMsg += ', Deleted: ' + cValToChar( _HMG_aFormDeleted[nI] )
      cMsg += ', Visible: ' + cValToChar( IsWindowVisible( GetFormHandle( cForm ) ) )
      cMsg += ', Title: ' + GetProperty( cForm, "Title" ) + " ;"
   NEXT
   cMsg += REPL(" ; ", 20)

   AlertInfo( cMsg, cMenu )

RETURN NIL

///////////////////////////////////////////////////////////////////
FUNCTION Icon32TempCreate()
   LOCAL cBuff := "AAABAAEAICAAAAEAIACoEAAAFgAAACgAAAAgAAAAQAAAAAEAIAAAAAAAgBAAAAAAAAAAAAAAAAAAAAAAAAAAAP/DAgL+3wsL+t8FBfzfBQX83wUF/N8LC/rfAAD+3wAA/98NDfrfBwf83wUF/N8FBfzfBQX83wsL+t8AAP7fAAD/3w0N+t8HB/zfBQX83wUF/N8LC/rfAAD+3wAA/98NDfrfBwf83wUF/N8FBfzfBQX83wsL+t8CAv7fAAD/wwAA/99eXuD/wcG+/7u7wP+7u8D/u7vA/8HBvv9dXeD/Cgr8/6urxv+9vcD/u7vA/7u7wP+7u8D/wcG+/11d4P8KCvz/q6vG/729wP+7u8D/u7vA/8HBvv9dXeD/Cgr8/6urxv+9vcD/u7vA/7u7wP+7u8D/wcG+/15e4P8AAP/fAAD/3wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD7/wAA/f8AAP3/AAD9/wAA/f8AAPv/AAD+/wAA//8AAPv/AAD9/wAA/f8AAP3/AAD7/wAA/v8AAP//AAD7/wAA/f8AAP3/AAD9/wAA/f8AAPv/AAD+/wAA/98AAP/fMDDv/2lp3P9gYN//YGDf/2Bg3/9padz/LS3v/wAA//+AgP//hYX//4KC//+Cgv//goL//42N//8+Pv//AAD//4CA//+Fhf//goL//4KC//+Njf//Pj7//wAA//+AgP//hYX//4KC//+Cgv//goL//42N//9BQf//AAD+3wAA/99oaNz/2Ni4/87OvP/Nzb7/zc3A/9bWvP9jY+L/CQn//////////////////////////////////4aG//8FBf////////////////////////////+Ghv//Bgb//////////////////////////////////4uL//8AAP7fAAD/3wAA//8AAP//AAD//wMD9f8DA/z/CQn6/wAA+/8AAPb/AAD//wAA//8AAP//AAD+/wMD9f8LC/3/AAD9/wAA/f8ODvj/AAD//wAA//8AAP//AAD+/wAA8/8AAPP/AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA/t8AAP/fMDDv/2tr6f9hYXL/Xl4A/2BgBP9hYQP/YmIB/2JiE/99fe3/hYX//4WF//9wcIH/WloA/2BgAv9iYgL/YmIC/2JiAP9KSkf/dnb7/4qK//9ycoL/XFwA/2BgDv+EhPP/goL//4CA//+AgP//gID//4yM//9AQP//AAD+3wAA/99oaNz/2NjK/5ycWv9oaAD/ZGQA/2FhAP9hYQD/Y2MA/6ysTv////3//////7+/fv9lZQD/ZGQA/19fAP9fXwD/ZGQA/2dnAP+vr0f//////7+/gP9lZQD/enoA////+////////////////////////////4uL//8AAP7fAAD/3wAA//8AAP//KyuD/2VlAP9nZwH/tbWE/5OTSv9XVwD/W1sA/0hIR/8AAP//KCiD/2JiAP9oaAP/tbWF/7e3iP9xcRL/YGAA/1BQAP8AAP//KCiF/2NjAP9UVAT/AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA/t8AAP/fMDDv/2ho8f9jY3D/ZGQA/2xsCv///////////5GRR/9eXgD/ZmYA/4OD//90dIX/YmIA/3JyFP///////////5GRSP9iYgD/ZmYB/4OD//90dIb/ZGQA/2pqDP91dfH/dnb//3V18/+Dg///gYH//4yM//9AQP//AAD+3wAA/99oaNz/19fL/5ubXf9jYwD/Z2cC////////////traF/1lZAP95eQL//////7+/hv9iYgD/YGAA/25uDf9tbQv/Xl4A/2ZmAP91dQr//////7+/hP9jYwD/aGgA/319DP9/fwD/kJAO////8////////////4uL//8AAP7fAAD/3wAA//8AAP//KyuD/2NjAP9nZwL//v79//////+0tIH/V1cA/1NTAv8AAP//KSmG/2NjAP9gYAD/W1sA/1paAP9gYAD/YmIA/xAQrv8AAP//KiqD/2NjAP9gYAD/WloA/1VVAP9HRwD/AADz/wAA//8AAP//AAD//wAA/t8AAP/fMDDv/2ho8f9jY3D/Y2MA/2hoBP///////////7Cwev9cXAD/ZmYB/4OD//90dIb/YmIA/2hoA/+1tYX/t7eI/3JyEf9kZAD/RkZR/319//91dYP/YmIA/2hoA/+0tIP/ubmG/7S0h/+AgP//fn7//4yM//9AQP//AAD+3wAA/99oaNz/19fL/5ubXf9kZAD/bW0M//r69v/Z2cD/bGwG/2RkAP92dgj//////7+/hP9iYgD/cnIU////////////kZFI/2RkAP97ewD//////7+/hf9iYgD/cnIU////////////////////+////////////4uM//8AAP7fAAD/3wAA//8AAP//KyuD/2VlAP9gYAD/cHAR/2VlAP9cXAD/U1MA/wAAsP8AAP//KSmC/2JiAP9gYAD/bm4N/21tC/9dXQD/X18A/0hICP8AAP//KCiE/2JiAP9gYAD/bW0M/2hoBP9oaAD/W1sO/wAA8/8AAf//AAD//wAA/t8AAP/fMDDv/2ho7v9hYWb/Y2MA/2FhAP9iYgD/ZmYA/2hoAP+hobP/fn7//4GB//9vb3T/XFwA/19fAP9kYwD/ZGQA/19fAP9vbwD/np6v/319//9xcXb/ZWQA/2VlAP9hYQD/W1sA/1paAP9eXgD/fHzz/4yL//9AP///AAD+3wAA/99obNz/2drA/7Cwjv+QkFf/kpJh/5SUYP9hZGv/LC+7//r6/////////////+PjwP+urnT/tbSA/3F0gP86PoD/o6R7/9jYuv///////////+fmwf9ucXT/Oj6A/6amgP+ysoD/sLB9/7m5gf////7//////4uM//8AAP7fAAD/3wAH//8AAP//AAD//wAA//8AAP//AAD//wAC//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AA///AAD//wAA//8AAP//AAD//wAA//8AAP//AgX//wAA//8AAP//AAD//wAA//8AAv//AAD//wAA/t8AAP/fMDvv/2lr3P9gYN//YGDf/2Bg3/9paNz/LTbv/wAD//9/gP//g4P//4CA//+AgP//gID//4yM//89O///AAD//3+A//+Dg///gID//4CA//+Mi///PUX//wAA//9/gP//g4P//4CA//+AgP//gID//4yL//9AP///AAD+3wAA/99oaNz/2Ne2/8/Puv/Pz7r/z8+6/9jYtv9lZNz/Cwz//////////////////////////////////4iJ//8ICv////////////////////////////+Iif//CAr//////////////////////////////////4uM//8AAP7fAAD/3wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAv//AAD//wAA/t8AAP/fMDDv/2lp3P9gYN//YGDf/2Bg3/9padz/LSzv/wAC//9/gf//g4P//4CA//+AgP//gID//4yL//89Rv//AAD//3+A//+Dg///gID//4CA//+Mi///PUb//wAA//9/fv//g4P//4CA//+AgP//gID//4yL//9AP///AAD+3wAA/99obNz/2Ni2/8/Puv/Pz7r/z8+6/9jYtv9lZNz/Cw3//////////////////////////////////4iG//8ICf////////////////////////////+Ih///CAn//////////////////////////////////4uL//8AAP7fAAD/3wAH//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA/t8AAP/fMDvv/2lq3P9gYN//YGDf/2Bg3/9padz/LSzv/wAA//9/fv//g4P//4CA//+AgP//gID//4yL//89Rv//AAH//3+A//+Dg///gID//4CA//+Mi///PUb//wAB//9/gP//g4P//4CA//+AgP//gID//4yM//9AQP//AAD+3wAA/99oa9z/2Ni2/8/Puv/Pz7r/z8+6/9jXtv9lZ9z/Cw7//////////////////////////////////4iJ//8ICv////////////////////////////+Iif//CAr//////////////////////////////////4uL//8AAP7fAAD/3wAH//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAP//wAA//8AAP//AAD//wAA//8AAP//AAH//wAA//8AAv//AAD//wAA//8AAP//AAD//wAA//8AAP//AAP//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA/98AAP/fLjnw/2Vn3f9cW+D/XFzg/1xc4P9lZN3/KzTw/wAA//9bXd//XVze/1pa3/9aWt//Wlrf/2Rj3P8rNPD/AAD//1ta3/9dXd7/Wlrf/1pa3/9kZNz/Kynw/wAA//9bXd//XVze/1pa3/9aWt//Wlrf/2Rk3P8tLe//AAD/3wAA/99ra9v/4+Oz/9XVuP/V1bj/1dW4/+Pjs/9oZ9z/AAD//8zLuv/Z2bb/1dW4/9XVuP/V1bj/4+Oz/2hn3P8AAP//zMy6/9nZtv/V1bj/1dW4/+Pjs/9oZ9z/AAD//8zLuv/Z2bb/1dW4/9XVuP/V1bj/4+Oz/2tr2/8AAP/fAAD/319f3//Jybz/vb3A/729wP+9vcD/yci8/1xg4P8AAP//tLTC/8DAvv+9vcD/vb3A/729wP/Jybz/XFzg/wAA//+0tML/wMC+/729wP+9vcD/ycm8/1xc4P8AAf//tLXC/8DAvv+9vcD/vb3A/729wP/Jybz/X1/f/wAA/98AAP/fAQH//wgI/f8CAv//AgL//wIC//8IB/3/AAv//wAA//8JCfz/AwP+/wIC//8CAv//AgL//wgI/f8AAP//AAD//wkJ/P8DA/7/AgL//wIC//8ICP3/AAD//wAA//8JDfz/AwP+/wIC//8CAv//AgL//wgI/f8BAf//AAD/3wAA/sMAAP/fAAD/3wAA/98AAP/fAAD/3wAA/98AAP/fAAD/3wAA/98AAP/fAAD/3wAA/98AAP/fAAD/3wAA/98AAP/fAAD/3wAA/98AAP/fAAD/3wAA/98AAP/fAAD/3wAA/98AAP/fAAD/3wAA/98AAP/fAAD/3wAA/98AAP7DAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
   LOCAL cBin, cFile := App.Cargo:cTempPath + "\MiniGui_2dbf32.ico"

   cBin := HB_Base64Decode( cBuff, LEN(cBuff) )
   HB_MemoWrit( cFile, cBin )

RETURN cFile

///////////////////////////////////////////////////////////////////
FUNCTION Icon64TempCreate()
   LOCAL cBuff := "AAABAAEAQEAAAAEAIAAoQgAAFgAAACgAAABAAAAAgAAAAAEAIAAAAAAAAEIAAAAAAAAAAAAAAAAAAAAAAAAAAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA//8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//AAD//wAA//8AAP///////////////////////////////////////////////////////////////////////wAA//8AAP//AAD/////////////////////////////////////////////////////////////AAD//wAA//8AAP///////////////////////////////////////////////////////////////////////wAA//8AAP//AAD//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//wAA//8AAP//AAD///////////////////////////////////////////////////////////////////////8AAP//AAD//wAA/////////////////////////////////////////////////////////////wAA//8AAP//AAD///////////////////////////////////////////////////////////////////////8AAP//AAD//wAA//8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA////////////////////////////////////////////////////////////////////////AAD//wAA//8AAP////////////////////////////////////////////////////////////8AAP//AAD//wAA////////////////////////////////////////////////////////////////////////AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAAA/wAAAP8AAAD/AAAA/wAAAP8AAAD/AAAA/wAAAP8AAAD/AAAA/wAAAP8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAAA/wAAAP8AAAD/AAAA/wAAAP8AAAD/AAAA/wAAAP8AAAD/AAAA/wAAAP8AAAD/AAD//wAA//8AAP//AAD//wAA//8AAP//AAAA/wAAAP8AAAD/AAAA/wAAAP8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//wAAAP9H////R////0f///9H////R////0f///9H////R////0f///8AAAD//////////////////////////////////////wAAAP9H////R////0f///9H////R////0f///9H////R////0f///9H////R////wAAAP///////////////////////////wAAAP9H////R////0f///8AAAD//////////////////////////////////////////////////////////////////////wAA//8AAP//AAD//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/8AAAD/R////0f///9H////R////0f///9H////R////0f///9H////R////wAAAP////////////////////////////////8AAAD/R////0f///9H////R////0f///9H////R////0f///9H////R////0f///9H////AAAA//////////////////////8AAAD/R////0f///9H////AAAA//////////////////////////////////////////////////////////////////////8AAP//AAD//wAA//8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//AAAA/0f///9H////R////0f///9H////R////0f///9H////R////0f///9H////AAAA////////////////////////////AAAA/0f///9H////R////0f///9H////R////0f///9H////R////0f///9H////R////0f///8AAAD/////////////////AAAA/0f///9H////R////wAAAP//////////////////////////////////////////////////////////////////////AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAAAP9H////R////0f///8AAAD/AAAA/wAAAP8AAAD/R////0f///9H////R////0f///8AAAD/AAD//wAA//8AAP//AAD//wAAAP9H////R////0f///8AAAD/AAAA/wAAAP8AAAD/AAAA/wAAAP9H////R////0f///9H////AAAA/wAA//8AAP//AAD//wAAAP9H////R////0f///8AAAD/AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAAD/R////0f///9H////AAAA/wAA//8AAP//AAD//wAAAP9H////R////0f///9H////R////wAAAP8AAP//AAD//wAA//8AAAD/R////0f///9H////AAAA//////////////////////8AAAD/AAAA/0f///9H////R////wAAAP8AAP//AAD//wAA//8AAAD/R////0f///9H////AAAA/wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAAA/0f///9H////R////wAAAP8AAP//AAD//wAA//8AAP//AAAA/0f///9H////R////0f///8AAAD/AAD//wAA//8AAP//AAAA/0f///9H////R////wAAAP///////////////////////////wAAAP9H////R////0f///8AAAD/AAD//wAA//8AAP//AAAA/0f///9H////R////wAAAP8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//wAAAP9H////R////0f///8AAAD/v7+//7+/v/+/v7//AAD//wAA//8AAAD/R////0f///9H////AAAA/////////////////wAAAP9H////R////0f///8AAAD//////////////////////wAAAP8AAAD/R////0f///9H////AAAA/////////////////wAAAP9H////R////0f///8AAAD//////////////////////////////////////////////////////////////////////wAA//8AAP//AAD//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/8AAAD/R////0f///9H////AAAA/7+/v/+/v7//v7+//wAA//8AAP//AAAA/0f///9H////R////wAAAP////////////////8AAAD/R////0f///9H////AAAA/wAAAP8AAAD/AAAA/wAAAP8AAAD/R////0f///9H////R////wAAAP////////////////8AAAD/R////0f///9H////AAAA/wAAAP8AAAD/AAAA/wAAAP8AAAD/AAAA//////////////////////////////////////8AAP//AAD//wAA//8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//AAAA/0f///9H////R////wAAAP+/v7//v7+//7+/v/8AAP//AAD//wAAAP9H////R////0f///8AAAD/////////////////AAAA/0f///9H////R////0f///9H////R////0f///9H////R////0f///9H////R////0f///8AAAD/////////////////AAAA/0f///9H////R////0f///9H////R////0f///9H////R////wAAAP//////////////////////////////////////AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAAAP9H////R////0f///8AAAD/AAD//wAA//8AAP//AAD//wAA//8AAAD/R////0f///9H////AAAA/wAA//8AAP//AAD//wAAAP9H////R////0f///9H////R////0f///9H////R////0f///9H////R////0f///8AAAD/AAD//wAA//8AAP//AAD//wAAAP9H////R////0f///9H////R////0f///9H////R////0f///8AAAD/AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAAD/R////0f///9H////AAAA/wAA//8AAP//AAD//wAA//8AAP//AAAA/0f///9H////R////wAAAP8AAP//AAD//wAA//8AAAD/R////0f///9H////R////0f///9H////R////0f///9H////R////0f///8AAAD/AAD//wAA//8AAP//AAD//wAA//8AAAD/R////0f///9H////R////0f///9H////R////0f///9H////AAAA/wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAAA/0f///9H////R////wAAAP8AAP//AAD//wAA//8AAP//AAD//wAAAP9H////R////0f///8AAAD/AAD//wAA//8AAP//AAAA/0f///9H////R////wAAAP8AAAD/AAAA/wAAAP8AAAD/AAAA/0f///9H////R////wAAAP8AAP//AAD//wAA//8AAP//AAAA/0f///9H////R////wAAAP8AAAD/AAAA/wAAAP8AAAD/AAAA/wAAAP8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//wAAAP9H////R////0f///8AAAD/v7+//7+/v/+/v7//AAD//wAA//8AAAD/R////0f///9H////AAAA/////////////////wAAAP9H////R////0f///8AAAD//////////////////////wAAAP8AAAD/R////0f///9H////AAAA/////////////////wAAAP9H////R////0f///8AAAD//////////////////////////////////////////////////////////////////////wAA//8AAP//AAD//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/8AAAD/R////0f///9H////AAAA/7+/v/+/v7//v7+//wAA//8AAAD/R////0f///9H////R////wAAAP////////////////8AAAD/R////0f///9H////AAAA////////////////////////////AAAA/0f///9H////R////wAAAP////////////////8AAAD/R////0f///9H////AAAA//////////////////////////////////////////////////////////////////////8AAP//AAD//wAA//8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//AAAA/0f///9H////R////wAAAP+/v7//v7+//7+/v/8AAAD/R////0f///9H////R////0f///8AAAD/////////////////AAAA/0f///9H////R////wAAAP//////////////////////AAAA/wAAAP9H////R////0f///8AAAD/////////////////AAAA/0f///9H////R////wAAAP//////////////////////////////////////////////////////////////////////AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAAAP9H////R////0f///8AAAD/AAAA/wAAAP8AAAD/AAAA/0f///9H////R////0f///8AAAD/AAD//wAA//8AAP//AAD//wAAAP9H////R////0f///8AAAD/AAAA/wAAAP8AAAD/AAAA/wAAAP9H////R////0f///9H////AAAA/wAA//8AAP//AAD//wAAAP9H////R////0f///8AAAD/AAAA/wAAAP8AAAD/AAAA/wAAAP8AAAD/AAAA/wAAAP8AAP//AAD//wAA//8AN///AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAAD/R////0f///9H////R////0f///9H////R////0f///9H////R////0f///8AAAD/AAD//wAA//8AAP//AAD//wAA//8AAAD/R////0f///9H////R////0f///9H////R////0f///9H////R////0f///9H////R////wAAAP8AAP//AAD//wAA//8AAAD/R////0f///9H////R////0f///9H////R////0f///9H////R////0f///8AAAD/AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAAA/0f///9H////R////0f///9H////R////0f///9H////R////0f///8AAAD/AAD//wAA//8AAP//AAD//wAA//8AAP//AAAA/0f///9H////R////0f///9H////R////0f///9H////R////0f///9H////R////wAAAP8AAP//AAD//wAA//8AAP//AAAA/0f///9H////R////0f///9H////R////0f///9H////R////0f///9H////AAAA/wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//wAAAP9H////R////0f///9H////R////0f///9H////R////0f///8AAAD//////////////////////////////////////wAAAP9H////R////0f///9H////R////0f///9H////R////0f///9H////R////wAAAP///////////////////////////wAAAP9H////R////0f///9H////R////0f///9H////R////0f///9H////R////wAAAP///////////////////////////wAA//8AAP//AAD//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/8AAAD/AAAA/wAAAP8AAAD/AAAA/wAAAP8AAAD/AAAA/wAAAP8AAAD/AAD///////////////////////////////////////8AAAD/AAAA/wAAAP8AAAD/AAAA/wAAAP8AAAD/AAAA/wAAAP8AAAD/AAAA/wAAAP////////////////////////////////8AAAD/AAAA/wAAAP8AAAD/AAAA/wAAAP8AAAD/AAAA/wAAAP8AAAD/AAAA/wAAAP8AAAD///////////////////////////8AAP//AAD//wAA//8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA////////////////////////////////////////////////////////////////////////AAD//wAA//8AAP////////////////////////////////////////////////////////////8AAP//AAD//wAA////////////////////////////////////////////////////////////////////////AAD//wAA//8AAP//AAD//wAA//8AAP//ADf//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//ADj//wAA//8AL///AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wA3//8AAP//ADf//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//ADj//wAA//8AN///AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AN///AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AN///AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AMP//AAD//wAw//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AMP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AMP//AAD//wAw//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//AAD//wAA//8AAP///////////////////////////////////////////////////////////////////////wAA//8AAP//AAD/////////////////////////////////////////////////////////////AAD//wAA//8AAP///////////////////////////////////////////////////////////////////////wAA//8AAP//AAD//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//wAA//8AAP//AAD///////////////////////////////////////////////////////////////////////8AAP//AAD//wAA/////////////////////////////////////////////////////////////wAA//8AAP//AAD///////////////////////////////////////////////////////////////////////8AAP//AAD//wAA//8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA////////////////////////////////////////////////////////////////////////AAD//wAA//8AAP////////////////////////////////////////////////////////////8AAP//AAD//wAA////////////////////////////////////////////////////////////////////////AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AMP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAw//8AAP//ADD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//ADD//wAA//8AMP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AN///AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAw//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//ADD//wAA//8AMP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AMP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//AAD//wAA//8AAP///////////////////////////////////////////////////////////////////////wAA//8AAP//AAD/////////////////////////////////////////////////////////////AAD//wAA//8AAP///////////////////////////////////////////////////////////////////////wAA//8AAP//AAD//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//wAA//8AAP//AAD///////////////////////////////////////////////////////////////////////8AAP//AAD//wAA/////////////////////////////////////////////////////////////wAA//8AAP//AAD///////////////////////////////////////////////////////////////////////8AAP//AAD//wAA//8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA////////////////////////////////////////////////////////////////////////AAD//wAA//8AAP////////////////////////////////////////////////////////////8AAP//AAD//wAA////////////////////////////////////////////////////////////////////////AAD//wAA//8AAP//AAD//wAA//8AAP//ADf//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AMP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//ADD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AMP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AN///AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//ADD//wAA//8AMP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AMP//AAD//wAw//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//AAD//wAA//8AAP///////////////////////////////////////////////////////////////////////wAA//8AAP//AAD/////////////////////////////////////////////////////////////AAD//wAA//8AAP///////////////////////////////////////////////////////////////////////wAA//8AAP//AAD//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//wAA//8AAP//AAD///////////////////////////////////////////////////////////////////////8AAP//AAD//wAA/////////////////////////////////////////////////////////////wAA//8AAP//AAD///////////////////////////////////////////////////////////////////////8AAP//AAD//wAA//8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA////////////////////////////////////////////////////////////////////////AAD//wAA//8AAP////////////////////////////////////////////////////////////8AAP//AAD//wAA////////////////////////////////////////////////////////////////////////AAD//wAA//8AAP//AAD//wAA//8AAP//ADf//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//ADD//wAA//8AMP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAw//8AAP//ADD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//ADD//wAA//8AMP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AN///AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AMP//AAD//wAw//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//ADD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAw//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//wAA//8AAP//AAD//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA//8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//AAD//wAA//8AAP//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//AAD//wAA//8AAP//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//wAA//8AAP//AAD//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA//+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//wAA//8AAP//AAD//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/+/v7//v7+//7+/v/8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AN///AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wA3//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAD//wAA//8AAP//AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
   LOCAL cBin, cFile, cPath := App.Cargo:cTempPath

   IF VALTYPE(App.Cargo:cTempPath) == "U"
      cPath := myGetPathTemp()     // где создавать временные файлы
   ENDIF

   cFile := cPath + "\MiniGui_2dbf64.ico"
   cBin  := HB_Base64Decode( cBuff, LEN(cBuff) )
   HB_MemoWrit( cFile, cBin )

   //MsgDebug(cFile, FILE(cFile) )
   //IF !FILE()
   //ENDIF

RETURN cFile

///////////////////////////////////////////////////////////////////
FUNCTION BmpTemp_RecDel()
   LOCAL cBuff := "Qk32AAAAAAAAAHYAAAAoAAAAEAAAABAAAAABAAQAAAAAAIAAAADEDgAAxA4AAAAAAAAAAAAAAAAAAAAAgAAAgAAAAICAAIAAAACAAIAAgIAAAICAgADAwMAAAAD/AAD/AAAA//8A/wAAAP8A/wD//wAA////AKqqqqqqqqqqqqqqqqqqqqqqd3eqqqqHd6iZmaqqqpmXqJmZeqqpmZeqiZmXqpmZiqqomZl5mYiqqqqJmZmYqqqqqqiZmYqqqqqqeZmZeqqqqqeZmZmXqqqqeZmaiZl6qqqJmaqomZeqqoiaqqqJmYqqqqqqqqiIiqqqqqqqqqqq"
   LOCAL cBin, cFile := App.Cargo:cTempPath + "\MG_Tsb_RecDel.bmp"

   cBin := HB_Base64Decode( cBuff, LEN(cBuff) )
   HB_MemoWrit( cFile, cBin )

RETURN cFile

///////////////////////////////////////////////////////////////////
FUNCTION PngTemp_Arrow_Down24()
   LOCAL cBuff := "iVBORw0KGgoAAAANSUhEUgAAABgAAAAYCAYAAADgdz34AAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAgY0hSTQAAeiYAAICEAAD6AAAAgOgAAHUwAADqYAAAOpgAABdwnLpRPAAAAAlwSFlzAAAOwwAADsMBx2+oZAAAAAd0SU1FB+ULBQU6O1g6FAQAAAGeSURBVEhL1ZXNasJAFIVPSmxXQtQX8DV0LWSRhYI7n6CLttB1n0XBlQvBfUFXKrhUuiiNtdAfWhXygxCClMl0JphirDbGTqEdchkShvPdc7mZK1GAPb+4OOAauDsD9EvgXkRcAJMr4GYBmDx9esrEmQfCwhMY00dg5gNY1pOVMC+XkDgB5s8s/g4gmUzSdDpNU6mUvycSiW+dxnIgSRJtNpt0PB5TXdf9vVAoiAUMh0O6vsrlsljAYDAIAUql0j8C8NbddFAsFsU54IB+vx8qkaqqPwKQSqVi93o90ul0aLvdprZthwCj0cj/3u12aaPRcDKZjLP+k0a2qaIobqvVMj3PIyHljRdCiKNpmsVamV81n64iAavDbq1W2wkxTdPJ5/PWtmtmXwBlmbnVavULxDAMJ5fLbRXnye0N4Ic3IVx8V+ZBmWIBgnLV63XTsqxFlHhsB0FWsiy72WyWlyXySj/EQaToIV0US3Qn4Bx42Md2nDNHgPEUTDQ+oGXgjdVtJiKOmbAC3L4CU39k8unPBvT0hQFEBNOac/F3YPkBo80ppfO7NLgAAAAASUVORK5CYII="
   LOCAL cBin, cFile := App.Cargo:cTempPath + "\MG_Arrow_Down24.png"

   cBin := HB_Base64Decode( cBuff, LEN(cBuff) )
   HB_MemoWrit( cFile, cBin )

RETURN cFile

///////////////////////////////////////////////////////////////////
FUNCTION BmpTemp_CheckF24()
   LOCAL cBuff := "Qk32BgAAAAAAADYAAAAoAAAAGAAAABgAAAABABgAAAAAAAAAAAATCwAAEwsAAAAAAAAAAAAA/////////////////////////Pz8rKysXl5eRUVFSEhIS0tLS0tLSUlJRkZGSUlJgICA2dnZ////////////////////////////////////////////vr6+PT09Ojo6jY2Nz8/P9fX1////////////5eXltLS0YmJiLCwsdXV18/Pz////////////////////////////////i4uLJCQkk5OT+Pj4////////////////////////////////////09PTTExMOjo63d3d////////////////////////gICALS0t0dHR////////////////////////////////////////////////////e3t7Kysr29vb////////////////oaGhKCgo3t7e////////////////////////////////////////////////////////////fHx8NTU19PT0////////4eHhISEhwcHB////////////////////////////////////////////////////////////////////R0dHc3Nz////////ampqYGBg////////////////////////////////////////////////////////////////////////1tbWIyMj2tra8PDwLCws09PT////////////////////////////////////////////////////////////////////////////X19ffX19oaGhR0dH////////////////////////////////////////////////////////////////////////////////tra2R0dHQUFBhoaG////////////////////////////////////////////////////////////////////////////////5+fnREREBwcHsLCw////////////////////////////////////////////////////////////////////////////////////SUlJAAAAv7+/////////////////////////////////////////////////////////////////////////////////////S0tLAAAAurq6////////////////////////////////////////////////////////////////////////////////////SkpKHR0dn5+f////////////////////////////////////////////////////////////////////////////////9vb2R0dHbm5uZ2dn////////////////////////////////////////////////////////////////////////////////09PTQkJCz8/PMjIy8/Pz////////////////////////////////////////////////////////////////////////////jo6OWlpa////Pj4+np6e/////////////////////////////////////////////////////////////////////////Pz8NTU1rKys////qampLCws9fX1////////////////////////////////////////////////////////////////////k5OTODg4+vr6////////UFBQampq////////////////////////////////////////////////////////////////1NTUHBwcv7+/////////////6OjoMDAwgoKC////////////////////////////////////////////////////////3NzcKysriYmJ////////////////////2dnZMDAwaGho8fHx////////////////////////////////////////////uLi4KCgof39/////////////////////////////6OjoUlJSMzMznZ2d8fHx////////////////////////////z8/PZGRkJiYmn5+f////////////////////////////////////////p6enQUFBNjY2Z2dnnp6eubm5vr6+rq6uhoaGSUlJMjIybGxs4ODg////////////////////////////////////////////////////zc3Nb29vHx8fAAAAAAAABQUFQEBAnZ2d8PDw////////////////////////////"
   LOCAL cBin, cFile := App.Cargo:cTempPath + "\MG_bCheckF24.bmp"

   cBin := HB_Base64Decode( cBuff, LEN(cBuff) )
   HB_MemoWrit( cFile, cBin )

RETURN cFile

///////////////////////////////////////////////////////////////////
FUNCTION BmpTemp_CheckT24()
   LOCAL cBuff := "Qk32BgAAAAAAADYAAAAoAAAAGAAAABgAAAABABgAAAAAAAAAAAATCwAAEwsAAAAAAAAAAAAA/////////////////////////f39tra2aWlpNDQ0FhYWBAQEAQEBCwsLJCQkS0tLjY2N4ODg////////////////////////////////////////////xMTEQ0NDAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAFxcXgoKC9fX1////////////////////////////////lZWVCwsLAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQ0ND39/f////////////////////////iIiIAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAALi4u4eHh////////////////rKysAQEBAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAPz8/9vb2////////5eXlHBwcAAAAAAAAAAAAAAAAAAAAAAAAAAAAPj4+UVFRAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAgoKC////////d3d3AAAAAAAAAAAAAAAAAAAAAAAAAAAAPz8/7e3t/f39W1tbAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAEhIS4eHh9vb2HBwcAAAAAAAAAAAAAAAAAAAAAAAAPT096enp////////+fn5XFxcAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAjIyMsLCwAAAAAAAAAAAAAAAAAAAAAAAANDQ05eXl////////////////+vr6W1tbAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAASUlJVVVVAAAAAAAAAAAAAAAAAAAALi4u39/f////////p6enj4+P////////+vr6W1tbAAAAAAAAAAAAAAAAAAAAAAAAAAAAIiIiGRkZAAAAAAAAAAAAAAAAICAg4uLi////////qampBQUFAAAAh4eH////////+fn5XFxcAAAAAAAAAAAAAAAAAAAAAAAACgoKAAAAAAAAAAAAAAAAAAAAGBgYw8PD////rq6uCAgIAAAAAAAAAAAAhoaG////////+vr6W1tbAAAAAAAAAAAAAAAAAAAAAAAACAgIAAAAAAAAAAAAAAAAAAAAERERbW1tDQ0NAAAAAAAAAAAAAAAAAAAAhYWF////////+vr6W1tbAAAAAAAAAAAAAAAABAQEMzMzAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAhISE////////+fn5YGBgAgICAAAAAAAAFRUVgYGBAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAhISE////////7+/vGRkZAAAAAAAAMzMz29vbBgYGAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAhoaG8vLyZWVlAwMDAAAAAAAAZ2dn////Q0NDAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAKSkpAAAAAAAAAAAAAAAAuLi4////tLS0AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAQEBA+/v7////////XFxcAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACAgIycnJ////////////6+vrMzMzAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAlpaW////////////////////3NzcNDQ0AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAgICi4uL////////////////////////////6urqXl5eAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAICAgqqqq////////////////////////////////////////sbGxRkZGCQkJAAAAAAAAAAAAAAAAAAAAAAAAAAAAICAgenp65eXl////////////////////////////////////////////////////19fXgoKCNDQ0CgoKAgICGxsbV1dXr6+v9/f3////////////////////////////"
   LOCAL cBin, cFile := App.Cargo:cTempPath + "\MG_bCheckT24.bmp"

   cBin := HB_Base64Decode( cBuff, LEN(cBuff) )
   HB_MemoWrit( cFile, cBin )

RETURN cFile

///////////////////////////////////////////////////////////////////
//  myBase64CreateText()  // создать строки ресурсы для программы
FUNCTION myBase64CreateText()
   LOCAL cFile, cRes, cBuff, nI, aFile := {}
/*
MG_2Dbf64         ICON          res\MG_2dbf64.ico
MG_TSB_RECDEL     BITMAP        res\MG_Tsb_RecDel.bmp
MG_TSB_UP         PNG           res\MG_Arrow_Up24.png
MG_TSB_DW         PNG           res\MG_Arrow_Down24.png
MG_TSB_ChkF24     BITMAP        res\MG_bCheckF24.bmp
MG_TSB_ChkT24     BITMAP        res\MG_bCheckT24.bmp
*/
   AADD( aFile, "res\MG_Tsb_RecDel.bmp  " )
   AADD( aFile, "res\MG_Arrow_Down24.png" )
   AADD( aFile, "res\MG_bCheckF24.bmp   " )
   AADD( aFile, "res\MG_bCheckT24.bmp   " )

   FOR nI := 1 TO LEN(aFile)
       cFile := aFile[nI]
       cBuff := HB_MemoRead(cFile)
       cRes  := hb_Base64Encode( cBuff )
       ? 'cFile := GetUserTempFolder() + "\' + SUBSTR(cFile,5) + '"'
       ? 'cBuff := "' + cRes + '"' ; ?
   NEXT

RETURN NIL
