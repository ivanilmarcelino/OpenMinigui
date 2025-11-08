/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2025 Verchenko Andrey <verchenkoag@gmail.com> Dmitrov, Moscow region
 * Copyright 2025 Sergej Kiselev https://clipper.borda.ru/?32-sergkis
 * Copyright 2025 Dmitry Ryzhkov https://clipper.borda.ru/?32-dima
 *
 * Обновить программу с сайта https://
 * Update the program from the website https://
 * Работа через libcurl.dll / Working via libcurl.dll
 * + \contrib\hbcurl
*/
#define  _HMG_OUTLOG
#require "hbcurl"
#include "hmg.ch"
#include "hbcurl.ch"

REQUEST HB_CODEPAGE_RU866, HB_CODEPAGE_RU1251
REQUEST HB_CODEPAGE_UTF8, HB_CODEPAGE_UTF8EX, HB_CODEPAGE_UTF16LE

#define PROGRAM   "MG update the program from the website https://..."
#define PROGVER   "Version 0.2 (01.11.2025) Option-3"
////////////////////////////////////////////////////////////////////////
Function Main()
   LOCAL cForm, aBClr, o, owc, obt, nY, nX, nW, nH, nG, cTitle, cPath
   LOCAL cFont, nFSize, cObj, aTxt, nWBtn, nHBtn, cCapt, cTool, nLen
   LOCAL lExit, aBtn, nBtn, aFont, nHIco

   cForm  := "wMain"
   nG     := 10
   nH     := GetDesktopRealHeight()
   //nH     -= GetTaskBarHeight()
   nW     := GetDesktopRealWidth()
   aBClr  := {115,208,208}
   cTitle := PROGRAM + SPACE(5) + PROGVER + SPACE(5)
   cTitle += MiniGuiVersion()
   aBtn   := { ;
                { "Update-1" , "https://hmgextended.com/files/CONTRIB/hmg-version.txt" } ,;
                { "Update-2" , "https://20ba98e6-9459-4a9c-8ee1-667c0f5399de.selstorage.ru/mg_version.txt" } ,;
                { "Update-3" , "https://abonent4.ru/downloads/minigui.json" } ,;
                { "Exit"     , "EXIT" }  ;
             }
   aFont  := GetFontParam(GetFontHandle("Normal"))
   ? ProcNL(), aFont, HB_VAlToExp(aFont)
   cFont  := _HMG_DefaultFontName
   nFSize := _HMG_DefaultFontSize
   ? ProcNL(), cFont, nFSize

   nLen   := LEN(aBtn[1,1]) + 2
   nWBtn  := GetFontWidth("Normal", nLen )
   nHBtn  := int(GetFontParam( GetFontHandle("Normal") )[ 9 ] * 2.1)
   cPath  := GetStartUpFolder() + "\"

   DEFINE WINDOW &cForm TITLE cTitle WIDTH  nW HEIGHT nH ;
          MAIN NOMAXIMIZE NOSIZE TOPMOST BACKCOLOR aBClr ;
          ON INIT    ( This.Topmost := .F., _wPost(0) )  ;
          ON RELEASE ( This.Hide, _wSend(90) )
          This.Cargo := oHmgData() ; o := This.Object ; owc := o:Cargo

      nW := This.ClientWidth
      nH := This.ClientHeight
      owc:aBClr := This.Backcolor
      owc:nG    := nG
      owc:nHBtn := nHBtn
      owc:nWBtn := nW - nG*2
      owc:nH    := nH
      owc:nW    := nW

      nY := nX := 5
      @ nY, nX LABEL Buff VALUE "" WIDTH nW-nX*2 HEIGHT nG ;
        FONTCOLOR WHITE TRANSPARENT RIGHTALIGN

      aBClr := { ORANGE, YELLOW, {75,155,155}, GRAY }
      nY    := nX := nG
      FOR EACH aTxt IN aBtn
          nBtn  := hb_enumindex(aTxt)
          cCapt := aTxt[1]
          lExit := IIF( "EXIT" $ UPPER(cCapt), .T., .F. )
          cTool := "Download and show link " + ALLTRIM(aTxt[2])
          cTool := IIF( lExit, "Exiting the program", cTool )
          cObj  := "Btn_" + hb_ntos( nBtn )
          @ nY, nX BUTTONEX &cObj WIDTH nWBtn HEIGHT nHBtn CAPTION cCapt ;
                            NOHOTLIGHT NOXPSTYLE HANDCURSOR NOTABSTOP    ;
                            BACKCOLOR aBClr[nBtn] TOOLTIP cTool          ;
                            ON INIT {|| This.Cargo := oHmgData() }       ;
                            ACTION  {|| This.Enabled := .F.,             ;
                                     _wPost(This.Cargo:Post,,This.Name) }

          obt := This.&(cObj).Cargo
          obt:nBtn  := nBtn
          obt:cObj  := cObj                         // button object name
          obt:cUrl  := ALLTRIM(aTxt[2])             // download link
          obt:Post  := IIF(lExit, 99, 10)           // event on the form
          //This.&(cObj).Backcolor := aBClr[nBtn]
          //This.&(cObj).Cargo := oHmgData()
          //This.&(cObj).Action := {|| This.Enabled := .F., _wPost(This.Cargo:Post, , This.Name) }

          nX += This.&(cObj).Width + nG
      NEXT
      nY += nHBtn + nG

      nHIco := 72
      DRAW ICON IN WINDOW &cForm AT 5, nW-nHIco-5 PICTURE "cUrl" WIDTH nHIco HEIGHT nHIco COLOR owc:aBClr

      @ nY, nG LABEL Lbl_Info VALUE "" WIDTH nW-nG*2 HEIGHT nFSize*3 TRANSPARENT
      nY += This.Lbl_Info.Height + nG * 2

      @ nY, nG LABEL Lbl_List VALUE "" WIDTH nW-nG*2 HEIGHT nFSize*2 TRANSPARENT CENTERALIGN
      nY += This.Lbl_List.Height + nG

      owc:nYEnd := nY

      ON KEY F1     ACTION NIL
      ON KEY ESCAPE ACTION ThisWindow.Release

      o:Event( 0, {|ow| // ON INIT
                    ow:Setfocus("Buff")
                    Return Nil
                    })

       o:Event(10, {|ow,ky,cn| // pressing buttons
                    Local obt  := This.&(cn).Cargo
                    Local aObj := ow:Cargo:aDelObj
                    Local i, cText, cForm := ow:Name
                    If IsArray(aObj) .AND. Len(aObj) > 0
                       For i := 1 To Len(aObj)
                          IF _IsControlDefined(aObj[i], cForm)
                             DoMethod(cForm, aObj[i], "Release" )
                          Endif
                       Next
                    Endif
                    SetProperty( cForm, "Lbl_List", "Value", "" )
                    DO EVENTS
                    If obt:nBtn == 3
                       cText := List_Download(ow, cPath, obt:cUrl)
                       If Len(cText) > 0
                          ShowTsb(ow, cPath, obt:cUrl, cText)   // -> demoxTsb.prg
                       Endif
                    Else
                       DownloadLink(ow, cPath, obt:cUrl, obt:nBtn)
                    Endif
                    ow:Enabler(cn, .T.)
                    ow:Setfocus('Buff')
                    ky := cn
                    Return Nil
                    } )

       o:Event(19, {|ow,ky,ob| ob:DrawFooters() , ky := ow })  // when changing the table cursor - redraw the footer

       o:Event(20, {|ow,ky,cn| // pressing buttons
                     Local obt := This.&(cn).Cargo
                     DownloadLink2(ow, cPath, obt:cUrl, obt:nBtn)
                     ow:Enabler(cn, .T.)
                     ow:Setfocus('Buff')
                     ky := cn
                     Return Nil
                     } )

       o:Event(40, {|ow,ky,ob| // editing a table cell
                     Local cUrl, xval, oc, nAt := ob:nAt
                     xval := ob:GetValue(ob:nCell)
                     FOR EACH oc IN ob:aColumns
                        xval := ob:GetValue(oc:cName)
                     NEXT
                     cUrl := xval
                     DownloadLink2(ow, cPath, cUrl)
                     ow:Cargo:oBrw:Setfocus()
                     ky := nAt
                     Return Nil
                     } )

       o:Event(90, {|ow,ky| // ON Release windows
                     Local cm := ProcNL()
                     ?  cm, "---[ "+ow:Name+":Event("+hb_ntos(ky)+") ]---"
                     ?  Repl(".", Len(cm)), "=> RELEASE WINDOW <=", ow:Name
                     DO EVENTS
                     Return Nil
                     })

      o:Event(99, {|ow| ow:Release() })

   END WINDOW

   // CENTER WINDOW &cForm
   ACTIVATE WINDOW &cForm

RETURN NIL

//////////////////////////////////////////////////////////////////////////////////
INIT PROCEDURE Sets_ENV()
   LOCAL cFont := "DejaVu Sans Mono", nSize := 13
   LOCAL o, cLog := hb_FNameExtSet( App.ExeName, '.log' )

   //rddSetDefault( "DBFCDX" )
   _SetGetLogFile( cLog ) ; hb_FileDelete( cLog ) ; SET LOGERROR ON

   SET EPOCH   TO 2000
   SET DATE    TO GERMAN
   SET CENTURY ON
   SET AUTOPEN OFF
   SET DELETED OFF
   /////////////
   SET OOP ON
   /////////////
   //
   Set ShowRedAlert On
   //
   SET MULTIPLE QUIT WARNING
   SET DEFAULT ICON TO "1MG"
   SET TOOLTIPSTYLE BALLOON
   SET NAVIGATION EXTENDED
   SET MENUSTYLE  EXTENDED
   //
   // Alert* colors
   SET MSGALERT BACKCOLOR  TO  {192,185,154}
   SET MSGALERT FONTCOLOR  TO  {0,0,0}
   //
   _SetGetLogFile( cLog ) ; hb_FileDelete( cLog ) ; SET LOGERROR ON
   //
   IF !HB_ISOBJECT( App.Cargo ) ; App.Cargo := oHmgData()
   ENDIF
   o := App.Cargo
   //
   IF     Sys.DesktopWidth >= 1920 ; nSize += 4
   ELSEIF Sys.DesktopWidth >  1280 ; nSize += 2
   ENDIF
   //
   SET FONT TO cFont, nSize

   _DefineFont("Normal"  , cFont, nSize  , .F., .F. )
   // Alert* font
   _DefineFont("DlgFont" , cFont, nSize+2, .F., .F. )

   SET WINDOW MAIN OFF

   ? REPL("=",20) + " Program start - " + HB_TTOC( HB_DATETIME() ) + " " + REPL("=",20)
   ? MiniGuiVersion() , Version(), hb_Ccompiler()
   ? "Computer/User: " + NetName() + "/" + hb_UserName()
   ? ProcNL(), cFont, nSize

RETURN

//////////////////////////////////////////////////////////////////////////////////
FUNCTION DownloadLink(oWnd, cPath, cUrl, nIBtn)
   LOCAL cForm, aBClr, cVal, cTmpFile, cText, aTxt, aBtn, nY, nX, nG, nBtn, nI
   LOCAL aDim, aVal, nWBtn, nHBtn, cObj, cCapt, obt, cBuff, aDelObj, cErr
   LOCAL aRet, lRet

   cForm    := oWnd:Name
   aBClr    := oWnd:Cargo:aBClr
   cTmpFile := cPath + cFileNoPath(cUrl)

   // download a file from the Internet
   aRet := Curl_downloadFile( cUrl, cTmpFile )
   lRet := aRet[1]
   cErr := aRet[2]
   ? ProcNL(), "lRet=", lRet
   IF !lRet
      cVal := "!!! Error downloading file - " + cUrl + " -> " + cErr
      SetProperty( cForm, "Lbl_Info", "Fontcolor", RED )
      SetProperty( cForm, "Lbl_Info", "Value", cVal )
      RETURN NIL
   ENDIF

   HB_MemoWrit( cTmpFile, cBuff )
   cTmpFile := cPath + cFileNoPath(cUrl)
   cText    := HB_MemoRead(cTmpFile)
   cVal     := "File has been downloaded from the link - " + cUrl + " in " + cTmpFile
   SetProperty( cForm, "Lbl_Info", "Fontcolor", BLUE )
   SetProperty( cForm, "Lbl_Info", "Value", cVal )

   cVal := "List of program update files in file " + cFileNoPath(cTmpFile)
   SetProperty( cForm, "Lbl_List", "Value", cVal )

   aDim := HB_ATokens(cText, CRLF)
   aBtn := {}
   FOR nI := 1 TO LEN(aDim)
      cVal := ALLTRIM(aDim[nI])
      IF LEN(cVal) > 0
         aVal := HB_ATokens(cVal, ";")
         IF LEN(aVal) == 2
            AADD(aBtn, aVal)
         ENDIF
      ENDIF
   NEXT

   ? ProcNL(), "aBtn=", aBtn ; ?v aBtn
   IF LEN(aBtn) == 0
      cVal := "ERROR! There is no update file list in file " + cFileNoPath(cTmpFile)
      cVal += " ! WRONG FILE STRUCTURE !"
      SetProperty( cForm, "Lbl_List", "Fontcolor", MAROON )
      SetProperty( cForm, "Lbl_List", "Value", cVal )
      RETURN NIL
   ENDIF

   nX      := nG := oWnd:Cargo:nG
   nY      := oWnd:Cargo:nYEnd
   nHBtn   := oWnd:Cargo:nHBtn
   nWBtn   := oWnd:Cargo:nWBtn - nG
   aDelObj := {}

   FOR EACH aTxt IN aBtn
       nBtn  := hb_enumindex(aTxt)
       cCapt := aTxt[1]
       cObj  := "Btn_2" + hb_ntos( nBtn )
       @ nY, nX BUTTONEX &cObj PARENT &cForm WIDTH nWBtn HEIGHT nHBtn ;
         CAPTION cCapt NOHOTLIGHT NOXPSTYLE HANDCURSOR NOTABSTOP

       This.&(cObj).Backcolor := IIF( nIBtn==1, ORANGE, YELLOW )
       This.&(cObj).Cargo := oHmgData() ; obt := This.&(cObj).Cargo
       obt:nBtn  := nBtn
       obt:cObj  := cObj                 // button object name
       obt:cUrl := ALLTRIM(aTxt[2])     // download link
       obt:Post  := 20                   // event on the form

       This.&(cObj).Action := {|| This.Enabled := .F., _wPost(This.Cargo:Post, , This.Name) }

       nY += This.&(cObj).Height + nG

       AADD( aDelObj , cObj )
   NEXT

   @ nY, nX LABEL Lbl_Rezult PARENT &cForm VALUE "" WIDTH nWBtn HEIGHT 32 TRANSPARENT
   AADD( aDelObj , "Lbl_Rezult" )

   oWnd:Cargo:aDelObj := aDelObj

RETURN NIL

//////////////////////////////////////////////////////////////////////////////////
FUNCTION DownloadLink2(oWnd, cPath, cUrl, nIBtn)
   LOCAL cForm, cTmpFile, lRet, cMsg, aDim, cww, cErr, aRet, cNew, cFil, aBClr
   DEFAULT nIBtn := 0

   ? ProcNL(), oWnd, cPath, cUrl, nIBtn
   cForm    := oWnd:Name
   aBClr    := {7,54,83}
   cTmpFile := cPath + cFileNoPath(cUrl)
   aDim     := { cFileNoPath(App.Exename), "from  - " + cFileNoPath(cUrl) ,;
                  "to - " + cFileNoPath(cTmpFile), "..." }

   cMsg := "ATTENTION !;;"
   cMsg += "Do you want to download the file from the link ?;"
   cMsg += cUrl
   IF !AlertYesNo(cMsg,,,,64,{LGREEN,RED})
      RETURN NIL
   ENDIF

   cww  := WaitWindow( aDim, .T., 500, 14, NIL, WHITE, aBClr )
   App.Cargo:cww := cww
   DO EVENTS
   // download a file from the Internet
   cFil := hb_FNameNameExt(cUrl)
   cFil := hb_StrToUtf8(cFil)               // file names on the site in UTF8
   cFil := tip_URLEncode(cFil)              // REQUIRED! for special characters '#', '&', etc.
   cNew := hb_FNameDir(cUrl)
   cUrl := cNew + cFil
   aRet := Curl_downloadFile( cUrl, cTmpFile, cww )
   lRet := aRet[1]
   cErr := aRet[2]
   WaitWindow()

   ? ProcNL(), "aRet=", aRet, HB_ValToExp(aRet)
   ? "       ["+cUrl+"]", "("+cTmpFile+")"
   IF !lRet
      cMsg := "File download error !;;"
      cMsg += "from  - " + cUrl + ";"
      cMsg += "to - " + cTmpFile+ ";;"
      cMsg += cErr
      AlertStop( cMsg, , , 64, {RED} )
   ELSE
      cMsg := "The file was saved successfully !;;"
      cMsg += "from  - " + cUrl + ";"
      cMsg += "to - " + cTmpFile + ";;"
      cMsg += "Do you want to open this file ?"
      IF AlertYesNo(cMsg, "Open file", .T.,, 64, { LGREEN, RED }, .T.)
         // Как программно открыть папку и выделить файл ?
         // How to programmatically open a folder and select a file ?
         ShellExecute( 0, "open", "explorer.exe", '/select, ' + cTmpFile, , SW_SHOWNORMAL )
      ENDIF
      //AlertInfo( cMsg, , , 64, {55,142,209} )
   ENDIF

RETURN NIL

//////////////////////////////////////////////////////////////////////////////////
FUNCTION List_Download(oWnd, cPath, cUrl)
   LOCAL cForm, aBClr, cVal, cTmpFile, cText, cBuff, lRet, aRet, cErr, cLink

   cForm    := oWnd:Name
   aBClr    := oWnd:Cargo:aBClr
   cTmpFile := cPath + cFileNoPath(cUrl)
   cLink    := hb_FNameDir(cUrl) + "minigui/"
   cText    := ""

   // download a file from the Internet
   aRet := Curl_downloadFile( cUrl, cTmpFile )
   lRet := aRet[1]
   cErr := aRet[2]
   ? ProcNL(), "lRet=", lRet
   IF !lRet
      cVal := "!!! Error downloading file - " + cUrl
      cVal += " ! " + cErr
      SetProperty( cForm, "Lbl_Info", "Fontcolor", RED )
      SetProperty( cForm, "Lbl_Info", "Value", cVal )
      RETURN cText
   ENDIF

   HB_MemoWrit( cTmpFile, cBuff )
   cTmpFile := cPath + cFileNoPath(cUrl)
   cText    := HB_MemoRead(cTmpFile)
   cText    := hb_Utf8ToStr(cText, hb_CdpSelect() )

   cVal     := "File has been downloaded from the link - " + cUrl + " in " + cTmpFile
   SetProperty( cForm, "Lbl_Info", "Fontcolor", BLUE )
   SetProperty( cForm, "Lbl_Info", "Value", cVal )

   cVal := "List of program update files in file " + cFileNoPath(cTmpFile)
   SetProperty( cForm, "Lbl_List", "Value", cVal )

RETURN cText

**********************************************************************
FUNCTION Curl_DownLoadFile(cUrl, cOutputFile, cWaitWnd)
   LOCAL hCurl, nErr, cErr, nI, aInfo, xVal, lRet := .F.
   LOCAL aLabel, cWaitLbl, cVal, cLbl
   DEFAULT cWaitWnd := ""

   // Initializing file download percentages
   IF LEN(cWaitWnd) > 0
      App.Cargo:nCountPos := 0
      cWaitLbl := ""
      aLabel   := HMG_GetFormControls(cWaitWnd, "LABEL")
      FOR EACH cLbl IN aLabel
          cVal := GetProperty( cWaitWnd, cLbl, "Value" )
          IF "..." $ cVal ; cWaitLbl := cLbl
          ENDIF
      NEXT
   ENDIF

   curl_global_init()

   // Initialization curl
   hCurl = curl_easy_init()

   IF Empty(curl)
      cErr := "Failed to initialize cURL"
      RETURN { lRet, cErr }
   ENDIF

   ? "   Version: ", curl_version()

   aInfo := curl_version_info()
   FOR nI := 1 TO Len( aInfo )
      ? "   aInfo[" + HB_NtoS(nI) + "]="
      xVal := aInfo[nI]
      IF IsArray(xVal) ; ?? HB_ValToExp(xVal)
      ELSE             ; ?? xVal
      ENDIF
   NEXT

   curl_easy_reset( hCurl )
   curl_easy_setopt( hCurl, HB_CURLOPT_SSL_VERIFYPEER, 0)    // ignore SSL certificates
   curl_easy_setopt( hCurl, HB_CURLOPT_DOWNLOAD )
   curl_easy_setopt( hCurl, HB_CURLOPT_URL, cUrl )           // Where to download from - website address
   curl_easy_setopt( hCurl, HB_CURLOPT_FOLLOWLOCATION, .t.)  // take redirects into account
   curl_easy_setopt( hCurl, HB_CURLOPT_FAILONERROR,.t.)      // Give an error if the file is not downloaded
   curl_easy_setopt( hCurl, HB_CURLOPT_DL_FILE_SETUP, cOutputFile )
   curl_easy_setopt( hCurl, HB_CURLOPT_NOPROGRESS, 0 )
   curl_easy_setopt( hCurl, HB_CURLOPT_BUFFERSIZE, 1024 * 1024 )
   curl_easy_setopt( hCurl, HB_CURLOPT_PROTOCOLS, HB_CURLPROTO_HTTPS )

   IF LEN(cWaitWnd) > 0 .AND. _IsWindowDefined(cWaitWnd)
      curl_easy_setopt( hCurl, HB_CURLOPT_PROGRESSBLOCK, {|npos,nlen| Curl_Runner(npos,nlen,cWaitWnd,cWaitLbl) })
   ENDIF

   // We execute the request
   cErr := ""
   nErr := curl_easy_perform(hCurl)
   IF nErr == HB_CURLE_OK
      ? "   File downloaded successfully:", cOutputFile
      lRet := .T.
   ELSE
      ? "   Error loading file:", cOutputFile
      cErr := curl_easy_strerror(nErr)+' (' + hb_ntos(nErr) + ')'
   ENDIF

   curl_easy_cleanup(hCurl)

RETURN { lRet, cErr }

**********************************************************************
STATIC FUNCTION Curl_Runner(nPos, nLen, cForm, cLabel)
   LOCAL cVal, cPrc

   IF _IsWindowDefined(cForm)
      IF !IsNumeric(App.Cargo:nCountPos)
         App.Cargo:nCountPos := 0
      ENDIF
      App.Cargo:nCountPos++
      IF !Empty( cForm ) .and. !Empty( cLabel )
         cPrc := cValToChar(INT((nPos/nLen) * 100)) + "%"
         cVal := hb_ntos(App.Cargo:nCountPos) + space(5) + cPrc
         SetProperty( cForm, cLabel, "Value", cVal )
         SetProperty( cForm, "Title", "DownLoad: " + cPrc )
         DO EVENTS
      ENDIF
   ENDIF

RETURN NIL
