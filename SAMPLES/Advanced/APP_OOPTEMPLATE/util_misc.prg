/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * The idea of 2013-2023 Verchenko Andrey <verchenkoag@gmail.com>
 * Implementation (c) 2013-14 Grigory Filatov <gfilatov@inbox.ru>
 * Fixed (c) 2023 Sergej Kiselev <bilance@bilance.lv>
*/
#define _HMG_OUTLOG
#include "hmg.ch"
#include 'fileio.ch'

#xtranslate MiniGuiVersionChar()  => Substr( MiniGuiVersion(), At(".", MiniGuiVersion()) - 2, 8 ) 
#xtranslate MiniGuiVersionNumba() => Int( Val( MiniGuiVersionChar() ) * 10000 + Val( Right(MiniGuiVersionChar(), 2) ) ) 
////////////////////////////////////////////////////////////////////////
FUNCTION MGVersChar()
   RETURN MiniGuiVersionChar()

////////////////////////////////////////////////////////////////////////
FUNCTION MGVersNumba()
   RETURN MiniGuiVersionNumba()

/*-----------------------------------------------------------------------------*
* взято из h_ini.prg
*-----------------------------------------------------------------------------*
#ifndef __XHARBOUR__
FUNCTION _LogFile( lCrLf, ... )
#else
FUNCTION _LogFile( ... )
#endif
*-----------------------------------------------------------------------------*
   LOCAL hFile, i, xVal, cTp
   LOCAL aParams := hb_AParams()
   LOCAL nParams := Len( aParams )
   LOCAL cFile := hb_defaultValue( _SetGetLogFile(), GetStartUpFolder() + hb_ps() + "_MsgLog.txt" )
#ifdef __XHARBOUR__
   LOCAL lCrLf
#endif
   // ---- 09.11.23
   LOCAL cLang, cOldCS

   cLang  := Upper( Left( Set ( _SET_LANGUAGE ), 2 ) )
   cOldCS := hb_CdpSelect()

   IF TYPE('cPubGlobLang') != "U"
      IF cLang # M->cPubGlobLang    // смотреть main.prg
         // установить язык вывода, который был при старте программы
         hb_CdpSelect(M->cPubLangCdp)
      ENDIF
   ENDIF

   IF nParams > 0
      IF HB_ISCHAR( aParams[ 1 ] )
         aParams[ 1 ] := { .T., aParams[ 1 ] }
      ENDIF
      IF HB_ISARRAY( aParams[ 1 ] )
         IF Len( aParams[ 1 ] ) > 1
            IF HB_ISLOGICAL( aParams[ 1 ][ 1 ] )     // { .T.\.F. , cFile }
               cTp := aParams[ 1 ][ 2 ]
               aParams[ 1 ] := aParams[ 1 ][ 1 ]
            ELSEIF HB_ISLOGICAL( aParams[ 1 ][ 2 ] ) // { cFile , .T.\.F. }
               cTp := aParams[ 1 ][ 1 ]
               aParams[ 1 ] := aParams[ 1 ][ 2 ]
            ELSE                // errors
               aParams[ 1 ] := .T.
            ENDIF
            IF !Empty( cTp )
               IF !hb_ps() $ cTp
                  cTp := GetStartUpFolder() + hb_ps() + cTp
               ENDIF
               cFile := cTp
            ENDIF
         ELSE                   // errors
            aParams[ 1 ] := .T.
         ENDIF
         cTp := NIL
         lCrlf := aParams[ 1 ]
      ENDIF
   ENDIF
   IF !Empty( cFile )
      hFile := iif( File( cFile ), FOpen( cFile, FO_READWRITE ), FCreate( cFile, FC_NORMAL ) )
      IF hFile == F_ERROR
         RETURN .F.
      ENDIF
      FSeek( hFile, 0, FS_END )
      IF nParams > 1
#ifdef __XHARBOUR__
         lCrLf := aParams[ 1 ]
#endif
         IF ( lCrLf := hb_defaultValue( lCrLf, .T. ) )
            FWrite( hFile, CRLF, 2 )
         ENDIF
         IF nParams == 2 .AND. HB_ISNIL( aParams[ 2 ] ) .AND. lCrLf
         ELSE
            FOR i := 2 TO nParams
               xVal := aParams[ i ]
               cTp  := ValType( xVal )
               IF     cTp == 'C' ; xVal := iif( Empty( xVal ), "'" + "'", Trim( xVal ) )
               ELSEIF cTp == 'N' ; xVal := hb_ntos( xVal )
               ELSEIF cTp == 'L' ; xVal := iif( xVal, ".T.", ".F." )
#ifdef __XHARBOUR__
               ELSEIF cTp == 'D' ; xVal := DToC( xVal )
#else
               ELSEIF cTp == 'D' ; xVal := hb_DToC( xVal, 'DD.MM.YYYY' )
#endif
               ELSEIF cTp == 'A' ; xVal := "ARRAY["  + hb_ntos( Len( xVal ) ) + "]"
               ELSEIF cTp == 'H' ; xVal :=  "HASH["  + hb_ntos( Len( xVal ) ) + "]"
               ELSEIF cTp == 'B' ; xVal := "'" + "B" + "'"
               ELSEIF cTp == 'T' ; xVal := hb_TSToStr( xVal, .T. )
               ELSEIF cTp == 'U' ; xVal := 'NIL'
               ELSE              ; xVal := "'" + cTp + "'"
               ENDIF
               FWrite( hFile, xVal + Chr( 9 ) )
            NEXT
         ENDIF
      ELSE
         FWrite( hFile, CRLF, 2 )
      ENDIF
      FClose( hFile )
   ENDIF

   // ---- 09.11.23
   IF TYPE('cPubGlobLang') != "U"
      IF cLang # M->cPubGlobLang    // смотреть main.prg
         // востановить язык вывода
         hb_CdpSelect(cOldCS)
      ENDIF
   ENDIF

RETURN .T.
*/
*--------------------------------------------------------*
Function GetMajorVersion()
  Local aVer := GetWinVersionInfo()
Return aVer[1]

*--------------------------------------------------------*
Function GetMinorVersion()
  Local aVer := GetWinVersionInfo()
Return aVer[2]

*--------------------------------------------------------*
Function GetBuildNumber()
  Local aVer := GetWinVersionInfo()
Return aVer[3]

*--------------------------------------------------------*
Function GetPlatformId()
  Local aVer := GetWinVersionInfo()
Return aVer[4]

*--------------------------------------------------------*
Function GetWinVersionString()
  Local aVer := WinVersion()
Return aVer[1]

*--------------------------------------------------------*
Function GetServicePackString()
  Local aVer := WinVersion()
Return aVer[2]

*--------------------------------------------------------*
Function GetServicePackNT()
Return IF(IsWin2KorLater(), Val( Right( Trim( GetServicePackString() ), 1 ) ), 0)

*--------------------------------------------------------*
Function IsWin7()
Return iif(IsVistaOrLater(), '7' $ GetWinVersionString(), .F.)

*--------------------------------------------------------*
Function IsMSVC()
Return iif( 'Microsoft' $ hb_Ccompiler(), .T. , .F.)

//Function IsMSVC()
//Return ( MSC_VER() > 0 )

*--------------------------------------------------------*
FUNCTION To2Log(cLog)
   DEFAULT cLog := "_c.log"
   /*_o2Log(_oThis(), 19, "*** oThis =>")
   _o2Log(oGetForms(.F.), 12, "*** aForm =>")
   _o2Log(_HMG_aEventInfo,12, "*** _HMG_aEventInfo =>")
   _o2Log("Forms*")
   _o2Log("Forms*S")
   _o2Log("procNL*")
   _o2Log("procNL*3")
   _o2Log(App.Cargo, 19, "*** App.Cargo =>")
   */
   //
   //o2Log(_oThis(), 19, "*** oThis =>")
   //o2Log(oGetForms(.F.), 12, "*** aForm =>")
   //o2Log(_HMG_aEventInfo,12, "*** _HMG_aEventInfo =>")
   //o2Log("procNL*")

    _o2Log(_oThis(), 19, "*** _oThis() .F. =>", .F., cLog)
    ?
    _o2Log(_oThis(), 19, "*** _oThis() .T. =>", .T., cLog)
    ?
    _o2Log(_oThis():FormCargo, 19, "*** _oThis():FormCargo .T. =>", .T., cLog)
    ?
    _o2Log(_HMG_aEventInfo,12, "*** _HMG_aEventInfo =>", , cLog)
    _o2Log("Forms*" , , , , cLog)
    _o2Log("procNL*", , , , cLog)
    _o2Log(App.Cargo, 19, "*** App.Cargo =>", , , , cLog)
    ?

RETURN Nil

*----------------------------------------------------------------------------*
FUNCTION o2Log( o, nLen, cMsg, lExt, cLog ) // текст аналогичен _o2Log() в LIB
*----------------------------------------------------------------------------*
   LOCAL a, b, c, i, j, k := pCount()
   DEFAULT lExt := .F., cLog := _SetGetLogFile()

   IF HB_ISCHAR(o)
      IF ( i := At("*", o) ) > 0
         c := o
         o := upper(o)
         b := subs(o+" ", i + 1)

         IF left(o, i) $ "FORM*,FORMS*,AFORM*,AFORMS*"
            DEFAULT nLen := 12
            o := {}
            b := iif( Empty(b), "", upper(alltrim(b)) )
            FOR EACH a IN HMG_GetForms( , .T. )
                IF iif( Empty(b), .T., a:Type $ b )
                   AAdd(o, {hb_enumIndex(a), a:Type, a:Index, a:Name, a:Handle, a:Title})
                ENDIF
            NEXT
            c := iif( Empty(b), "", "<"+c+">"+" " )
            Default cMsg := "==> aForms: " + c
            k := 3

         ELSEIF left(o, i) $ "P*,PROC*, PROCNL*"
            DEFAULT nLen := 25
            b := Val( b )
            i := 0
            o := oHmgData()
            WHILE ( ++i < 100 )
               IF Empty( j := procname(i) ) ; EXIT
               ENDIF
               o:Set(TR0(ProcFile(i), nLen-7)+str(procline(i), 7), j)
               IF b > 0 .and. i >= b ; EXIT
               ENDIF
            END
            b := i := Nil
            Default cMsg := "==> ProcNL: "
            k := 3
         ENDIF
      ENDIF
   ENDIF

   DEFAULT nLen := 19

   IF k > 2 .and. HB_ISCHAR(cMsg) ; _LogFile({.T., cLog}, cMsg)
   ELSEIF !Empty(cMsg)            ; _LogFile({.T., cLog}, Nil)
   ENDIF

   IF HB_ISOBJECT( o )
      _LogFile({.F., cLog}, "O:"+o:ClassName)
      IF o:ClassName $ "THMGDATA,TKEYDATA,TTHRDATA,TINIDATA"
         _LogFile({.F., cLog}, o:GetAll())
         FOR EACH a IN o:GetAll()
             i := hb_enumIndex( a )
             b := iif( HB_ISCHAR( a[1] ), TR0(a[1], nLen), a[1] )
             _LogFile({.T., cLog}, TR0(i, nLen-1)+".", b, "=")
             IF    HB_ISOBJECT( a[2] )
                IF lExt
                   o2Log( a[2], nLen + 5, , , cLog )
                ELSE
                   _LogFile({.F., cLog}, "O:"+a[2]:ClassName)
                   IF a[2]:ClassName $ "THMGDATA,TKEYDATA,TTHRDATA,TINIDATA"
                      _LogFile({.F., cLog}, a[2]:GetAll())
                   ELSEIF a[2]:ClassName == "TWNDDATA"
                      _LogFile({.F., cLog}, a[2]:Name, a[2]:Type)
                   ELSEIF a[2]:ClassName $ "TCNLDATA,TTSBDATA,TGETDATA,TSTBDATA"
                      _LogFile({.F., cLog}, a[2]:Window, a[2]:Name, a[2]:Type)
                   ELSEIF a[2]:ClassName == "TBROWSE"
                     _LogFile({.F., cLog}, a[2]:cControlName, a[2]:cParentWnd)
                   ENDIF
                ENDIF
             ELSEIF HB_ISARRAY( a[2] ) .and. lExt
                 _LogFile({.F., cLog}, hb_valtoexp( a[2] ))
             ELSE
                _LogFile({.F., cLog}, a[2])
             ENDIF
         NEXT
      ELSEIF o:ClassName == "TWNDDATA"
         _LogFile({.F., cLog}, o:Name, o:Type)
      ELSEIF o:ClassName $ "TCNLDATA,TTSBDATA,TGETDATA,TSTBDATA"
         _LogFile({.F., cLog}, o:Window, o:Name, o:Type)
      ELSEIF o:ClassName == "TBROWSE"
         _LogFile({.F., cLog}, o:cControlName, o:cParentWnd)
      ENDIF

   ELSEIF HB_ISARRAY( o )
      _LogFile({.F., cLog}, o)
      FOR EACH a IN o
          i := hb_enumIndex( a )
          IF HB_ISOBJECT( a )
             o2Log( a, nLen, , , cLog )
          ELSEIF HB_ISARRAY( a )
             _LogFile({.T., cLog}, TR0(i, nLen-1)+".")
             IF lExt
                _LogFile({.F., cLog}, a)
                FOR EACH j IN a
                    k := hb_enumIndex( j )
                    _LogFile({.T., cLog}, TR0(k, nLen-1+5)+".", hb_valtoexp( j ))
                NEXT
             ELSE
                _LogFile({.F., cLog}, hb_valtoexp( a ))
             ENDIF
          ELSE
             _LogFile({.F., cLog}, a)
          ENDIF
      NEXT

   ELSE
      _LogFile({.F., cLog}, o)

   ENDIF

RETURN .T.

*----------------------------------------------------------------------------*
FUNCTION TR0( cTxt, nLen, cSim )
*----------------------------------------------------------------------------*
   IF !HB_ISCHAR(cTxt) ; cTxt := cValToChar(cTxt)
   ENDIF
   Default nLen := Len(cTxt)
   IF cSim == Nil; cSim := " "
   ENDIF

RETURN PadL( AllTrim( cTxt ), nLen, cSim )

//////////////////////////////////////////////////////////////////////////
FUNCTION _o2LogVar( o, cTxt, lExt, nLen, cOut ) 
   Local cTmp, cMsg 
   Default cOut := ".\_tmp_.log", nLen := 20, lExt := .T., cTxt := " O: " 
 
   cMsg := "==> "+iif( lExt, ".T.", ".F.")+" "+cTxt 
   fErase(cOut) 
   _o2Log(o,  nLen, cMsg, lExt, cOut) 
   cTmp := hb_memoread(cOut) 
   fErase(cOut) 
 
Return cTmp 
 
//////////////////////////////////////////////////////////////////////////
// Общее количество объектов на открытых формах
// Total number of objects on open forms
FUNCTION myGetFormsObjects()
   LOCAL aFrm, nI, nJ, aVal, cRet, cForm, nObj, aNObj, cNobj, aSum

   // Список для подсчёта объектов - добавить, если нужно
   aNObj := { "OBUTTON", "LABEL", "FRAME", "GETBOX", "CHECKBOX", "CHECKLABEL" }
   aSum  := ARRAY(LEN(aNObj))
   AFILL(aSum, 0)

   aFrm := HMG_GetForms()
   nObj := 0

   FOR nI := 1 TO LEN(aFrm)
      cForm := UPPER(aFrm[nI])
      //? nI, cForm, _HMG_aFormType[nI], _HMG_aFormHandles[nI], _HMG_aFormDeleted[nI]
      //?? "Visible=", IsWindowVisible( GetFormHandle( cForm ) )
      //?? GetProperty( cForm, "Visible" )
      aVal  := HMG_GetFormControls(cForm, ) // все объекты
      nObj  += LEN(aVal)
      FOR nJ := 1 TO LEN(aNObj)
         cNobj := aNObj[nJ]
         aVal  := HMG_GetFormControls(cForm, cNobj)
         aSum[nJ] += LEN(aVal)
      NEXT
   NEXT

   cRet := HB_NtoS( nObj ) + ", "
   FOR nI := 1 TO LEN(aNObj)
      cRet += LOWER(aNObj[nI]) + ":"
      cRet += HB_NtoS(aSum[nI]) + ", "
   NEXT

RETURN cRet

////////////////////////////////////////////////////////////
FUNC This_ToLog(cMsg)
   IF !Empty(cMsg)
      ? cMsg
   ENDIF
   ? "   _HMG_ThisFormName", _HMG_ThisFormName
   ? "       _HMG_ThisType", _HMG_ThisType
   ? "  _HMG_ThisFormIndex", _HMG_ThisFormIndex
   ? "      _HMG_ThisIndex", _HMG_ThisIndex
   ? "_HMG_ThisControlName", _HMG_ThisControlName
   ? "  _HMG_ThisEventType", _HMG_ThisEventType
   ?
RETURN Nil

////////////////////////////////////////////////////////////
FUNCTION To_Focus( cForm, cControl )
   LOCAL hForm, lRet
   DEFAULT cForm    := ""
   DEFAULT cControl := ""

   IF LEN(cForm) == 0
      MsgStop("Undefined Window ! cForm = NIL !" + ;
            CRLF + CRLF + ProcNL(0) + CRLF + ProcNL(1) )
      ? ProcNL(), cForm, cControl, "Undefined Window ! cForm = NIL !"
      RETURN .F.
   ENDIF

   lRet := _IsWindowDefined(cForm)
   IF lRet
      hForm := GetFormHandle(cForm)
      IF hForm != 0
         IF IsIconic( hForm ) ; _Restore( hForm )
         ENDIF
         SetProperty(cForm, "Topmost", .T.)
         DoMethod(cForm, "SetFocus")
         IF LEN(cControl) == 0
            // нет cControl на форме
         ELSE
            IF _IsControlDefined(cControl, cForm)
               DoMethod(cForm, cControl, "SetFocus")
            ENDIF
         ENDIF
         SetProperty(cForm, "Topmost", .F.)
      ENDIF
   ENDIF

RETURN lRet

//////////////////////////////////////////////////////////////////////////////
FUNCTION HelpThisWindow(x)
   LOCAL cMsg, hWnd, nIndexForm, nIndexControl, nW, nH
   LOCAL cFormName, cControlName, cFormParentName

   cMsg := "=== список переменных This через имена ==="  + CRLF
   cMsg += "_HMG_ThisFormName=    " + _HMG_ThisFormName    + CRLF
   cMsg += "_HMG_ThisFormIndex=   " + HB_NtoS(_HMG_ThisFormIndex) + CRLF
   cMsg += "_HMG_ThisEventType=   " + _HMG_ThisEventType   + CRLF
   cMsg += "_HMG_ThisType=        " + _HMG_ThisType        + CRLF
   cMsg += "_HMG_ThisIndex=       " + HB_NtoS(_HMG_ThisIndex) + CRLF
   cMsg += "_HMG_ThisControlName= " + _HMG_ThisControlName + CRLF
   cMsg += "_HMG_InplaceParentHandle= " + HB_NtoS(_HMG_InplaceParentHandle) + CRLF
   cMsg += "GetFormHandle(FormName)= " + HB_NtoS(GetFormHandle(_HMG_ThisFormName)) + CRLF + CRLF
   cMsg += "GetFocus()="
   hWnd := GetFocus()               // если 0, то он не на программе MiniGui, если нет, то строки ниже
   cMsg += HB_NtoS(hWnd) + ' // если 0, то он не на программе MiniGui, если нет, то строки ниже' + CRLF

   nIndexForm := GetFormNameByHandle( hWnd , @cFormName )
   cMsg += "GetFormNameByHandle( hWnd , @cFormName )=" + HB_NtoS(nIndexForm) + "  // если 0, то след. строка внизу" + CRLF
   cMsg += "  cFormName=" + cFormName + CRLF + CRLF

   nIndexControl := GetControlNameByHandle( hWnd , @cControlName , @cFormParentName )
   cMsg += "GetControlNameByHandle ( hWnd , @cControlName , @cFormParentName )=" + HB_NtoS(nIndexControl) + CRLF
   cMsg += "  cControlName=" + cControlName + "   cFormParentName=" + cFormParentName + CRLF + CRLF

   cMsg += "ThisWindow.Name= " + ThisWindow.Name  + CRLF
   cMsg += "ThisWindow.Handle= " + HB_NtoS(ThisWindow.Handle)  + CRLF

   IF Empty( x )
      AlertInfo(cMsg)
   ELSE
      cFormName := _HMG_ThisFormName + "_Modal"
      DEFINE WINDOW &cFormName AT 0,0 WIDTH 500 HEIGHT 500 TITLE " " ;
         MODAL NOSIZE NOSYSMENU NOCAPTION ;
         BACKCOLOR  { 238, 249, 142 } ;
         ON INIT    {|| DoEvents(), _wPost(0) } ;
         ON RELEASE NIL

         nW := This.ClientWidth
         nH := This.ClientHeight

         cMsg := "TEST ==== " + cFormName + CRLF + CRLF + cMsg
         @ 10, 10 EDITBOX Edit_Memo WIDTH nW-20 HEIGHT nH-20 VALUE cMsg ;
           BACKCOLOR SILVER FONTCOLOR BLACK SIZE 11  ;
           MAXLENGTH 1200 NOHSCROLL

         (This.Object):Event(0, {|ow| wApi_Sleep(1000), ow:Release() })

      END WINDOW
      CENTER WINDOW &cFormName
      ACTIVATE WINDOW &cFormName
   ENDIF

RETURN NIL

////////////////////////////////////////////////////////////////////
FUNCTION ToRGB(aDim)
   RETURN RGB(aDim[1],aDim[2],aDim[3])

///////////////////////////////////////////////////////////////////
FUNCTION CLIPPERTORGB(cStr,nType)
   LOCAL nRet := 0
   LOCAL aParts := HB_ATokens(cStr, "/")
   nRet := StrToRGB(aParts[nType])
RETURN nRet

///////////////////////////////////////////////////////////////////
FUNCTION n2RGB(nColor)
   LOCAL nR := 0, nG := 0, nB := 0, cColor := NTOC(nColor, 16)

   nR := CTON(SUBSTR( cColor, 5, 2 ), 16)
   nG := CTON(SUBSTR( cColor, 3, 2 ), 16)
   nB := CTON(SUBSTR( cColor, 1, 2 ), 16)
RETURN {nR, nG, nB }

///////////////////////////////////////////////////////////////////
FUNCTION StrToRGB(cStr)
    LOCAL nRet := 0
    LOCAL aRet := {0, 0, 0}
   SWITCH cStr
   CASE "0";  aRet := {0,   0,     0}; EXIT
   CASE "1";  aRet := {0,   0,   128}; EXIT
   CASE "2";  aRet := {0,   128,   0}; EXIT
   CASE "3";  aRet := {0,   128, 128}; EXIT
   CASE "4";  aRet := {128, 0,     0}; EXIT
   CASE "5";  aRet := {128, 0,   128}; EXIT
   CASE "6";  aRet := {128, 128,   0}; EXIT
   CASE "7";  aRet := {192, 192, 192}; EXIT
   CASE "8";  aRet := {128, 128, 128}; EXIT
   CASE "9";  aRet := {0,   0,   255}; EXIT
   CASE "10"; aRet := {0,   255,   0}; EXIT
   CASE "11"; aRet := {0,   255, 255}; EXIT
   CASE "12"; aRet := {255, 0,     0}; EXIT
   CASE "13"; aRet := {255, 0,   255}; EXIT
   CASE "14"; aRet := {255, 255,   0}; EXIT
   CASE "15"; aRet := {255, 255, 255}; EXIT
        END SWITCH
    nRet := 1 * aRet[1] + 256 * aRet[2] + (256 * 256) * aRet[3]
RETURN nRet

////////////////////////////////////////////////////////////////////
FUNCTION HMG_SetMousePos( nHandle, y1, x1 )
   LOCAL c := _HMG_MouseCol
   LOCAL r := _HMG_MouseRow
   Local y := GetWindowRow(nHandle)
   Local x := GetWindowCol(nHandle)
   Default y1 := 1, x1 := 1

   SetCursorPos( x + x1, y + y1 )

RETURN {c,r}

////////////////////////////////////////////////////////////////////
FUNCTION HMG_MouseGet()
   LOCAL x := _HMG_MouseCol
   LOCAL y := _HMG_MouseRow
RETURN {x,y}

////////////////////////////////////////////////////////////////////
FUNCTION HMG_MouseSet(aXY)
   LOCAL aXYold := HMG_MouseGet()
   SetCursorPos( aXY[1], aXY[2] )
RETURN aXYold

////////////////////////////////////////////////////////////////////
Function SwitchToWin( cForm )

    If _IsWindowDefined( cForm )
       DO EVENTS
       If IsIconic( GetFormHandle(cForm) )
          _Restore( GetFormHandle(cForm) )
       Else
          DoMethod( cForm, "SetFocus" )
       EndIf
    EndIf

Return Nil

//////////////////////////////////////////////////////////////////////////
FUNCTION AboutComputer()
   LOCAL cComp, lRdp, aRet

   ? "Computer name:", GetComputerName()
   ? "Current  user:", GetUserName()
   ? "   User admin:", IIF( OS_ISUSERANADMIN(), "Yes", "No" )
   ? "           OS:", OS()
   ? "          CPU:", myCPUName()
   ? " Multi Thread:", IIF( HB_MultiThread(), "Yes", "No" )

   ? "Screen resolution: " + HB_NtoS(GetDesktopWidth())+" x "+HB_NtoS(GetDesktopHeight())
   ? MiniGuiVersion() , "<|>", MGVersChar(), MGVersNumba()
   ? Version() ; ? hb_Ccompiler()

   cComp := GetComputerName()
   lRdp  := win_OsIsTSClient()
   ? "Where is the program running ? RDP session:", lRdp
   IF lRdp
      aRet := HmgLogonSession(cComp)
      ? "   hmgLogonSession()=", aRet, HB_ValToExp(aRet)
   ENDIF

Return Nil

////////////////////////////////////////////////////////////////////////////////
Function MyCPUName()
   LOCAL lWinNT := Win_osIsNT(), cRet

   IF lWinNT
      cRet := GetRegistry( HKEY_LOCAL_MACHINE, "HARDWARE\DESCRIPTION\System\CentralProcessor\0", "ProcessorNameString" )
      IF cRet == NIL
         cRet := "Not key CPU for " + OS()
      ELSE
         cRet := Ltrim( cRet )
      ENDIF
   ELSE
      cRet := "None CPU for " + OS()
   ENDIF

   cRet := STRTRAN(cRet,'  ')
   cRet += " " + IF(lWinNT, "[~", "") + myGetCPUSpeed() + " MHz" + IF(lWinNT, "]", "")

Return cRet

////////////////////////////////////////////////////////////////////////////////
Function MyGetCPUSpeed()
   LOCAL lWinNT := Win_osIsNT(), cRet, n := 0

   IF lWinNT
      n := GetRegistry( HKEY_LOCAL_MACHINE, "HARDWARE\DESCRIPTION\System\CentralProcessor\0", "~MHz" )
      IF n == NIL
         cRet := "Not key CPUSpeed for " + OS()
      ELSE
         cRet := LTrim( Str( n+1 ) )
      ENDIF
   ELSE
      cRet := "None CPUSpeed for " + OS()
   ENDIF

RETURN cRet

/////////////////////////////////////////////////////////////////////
FUNCTION myCPUComp()
   LOCAL lWinNT := Win_osIsNT(), cRet
   cRet := myCPUName() + " " + IF(lWinNT, "[~", "") + MyGetCPUSpeed() + " MHz" + IF(lWinNT, "]", "")
RETURN cRet

/////////////////////////////////////////////////////////////////////
FUNCTION WMIService( cComp )
   LOCAL oWMI, oLocator
   DEFAULT cComp := "."

   BEGIN SEQUENCE WITH {| oErr | Break( oErr ) }
      oLocator:= win_oleCreateObject( "wbemScripting.SwbemLocator" )
      oWMI:= oLocator:ConnectServer(cComp,"ROOT\CIMV2")
   END SEQUENCE

RETURN oWMI

/////////////////////////////////////////////////////////////////////
// Функция, позволяющая получить информацию о терминальных сессиях
// на сервере (локальный/удаленный компьютер).
// Рекомендация:
// Перед применением проверить на компьютерах
// Windows Management Instrumentation (WMI):
// 1. Состояние служб.
// 2. Разрешение в брандмауэре.
//
FUNCTION hmgLogonSession(cComp)
   LOCAL aRet, objWMI, nI, elem, oTerminal, objItem, userlist, id
   DEFAULT cComp := "."

   nI   := 0
   aRet := {}
   //? REPL("-",40)

   objWMI := WmiService(cComp)

   objWMI:Security_:ImpersonationLevel  := 3
   objWMI:Security_:AuthenticationLevel := 6

   oTerminal := objWMI:ExecQuery("Select * from Win32_LogonSession Where LogonType=10")
   //? "===== Number of terminal sessions:",oTerminal:count

   FOR EACH objItem IN oTerminal
      // objItem:Name и остальные фишки почему то возвращают NIL
      //? "   Caption               ",  objItem:Caption
      //? "   Description           ",  objItem:Description
      //? "   InstallDate           ",  objItem:InstallDate
      //? "   Name                  ",  objItem:Name
      //? "   Status                ",  objItem:Status
      //? "   StartTime             ",  objItem:StartTime
      //? "   AuthenticationPackage ",  objItem:AuthenticationPackage
      //? "   LogonId               ",  objItem:LogonId
      //? "   LogonType             ",  objItem:LogonType
      //? "objItem:LogonId", objItem:LogonId
      //? "objItem:StartTime", objItem:StartTime
      if !empty(objItem:LogonId)
          nI++
      endif
      //? REPL("-",40)
   NEXT
   //? "Number of terminal sessions:",nI

   //? REPL("-",40)
   FOR EACH objItem IN oTerminal
      id := objItem:LogonId
      userlist := objWMI:ExecQuery("ASSOCIATORS OF {Win32_LogonSession.LogonId="+id+"} WHERE AssocClass=Win32_LoggedOnUser Role=Dependent")
      //? id, userlist
      FOR EACH elem IN userlist
         AADD( aRet, elem:name + " Status:" + elem:Status )
      NEXT
   NEXT

RETURN aRet
