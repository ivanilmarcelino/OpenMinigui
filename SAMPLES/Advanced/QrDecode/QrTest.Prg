/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2002-2008 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
 *
 * Activex Sample: Inspired by Freewin Activex inplementation by 
 * Oscar Joel Lira Lira http://sourceforge.net/projects/freewin
 * Read qrcode using Zbar utility http://zbar.sourceforge.net/
 * Copyright Sept 2019 Pierpaolo Martinello

/*********************************************************************************
   Filename        : QrTest.prg

   Created         : 17 September 2019 (21:00:20)
   Created by      : Pierpaolo Martinello

   Comments        : Free for all purposes
                     This Program is intended for discover the content of a qrcode
                     or show a graphical file
                     You can drag the file to check directly over QrTest.exe, or
                     type it as the QrTest.exe parameter.

**********************************************************************************/

#include "Minigui.ch"

MEMVAR hWin,address, DragFile

Procedure Main ( nFile )
Default nfile to GetTempFolder ( )+"\QrTipo.jpg"
Private hwin := GetDesktopHeight() - GetTaskBarHeight()-20
Private address, DragFile

    m->address  := ""
    m->DragFile := nFile

    IF hwin <= 800
       hwin  := MIN( HWIN, 790)
    EndIF

    DEFINE WINDOW Win1 ;
        AT 0,0 ;
        WIDTH 750 ;
        HEIGHT hwin ;
        TITLE ' ( ESC = ends the program )' ;
        Icon "Basic" ;
        MAIN ;
        NOMAXIMIZE ;
        NOMINIMIZE ;
        NOSIZE ;
        ON INIT TestNavigate( m->DragFile ) ;
        ON MAXIMIZE ( Win1.Test.Width := (Win1.Width) - 100, Win1.Test.Height := (Win1.Height) - 100 ) ;
        ON RELEASE hb_DirRemoveAll( GetTempFolder()+"\UnzipQr\" ) ;

        DEFINE FRAME Frame_1
            ROW    7
            COL    20
            WIDTH  700
            HEIGHT hwin -70
        End Frame

        DEFINE LABEL cResult
            ROW 10
            COL 20
            WIDTH 700
            HEIGHT hwin -75
            FONTNAME "ARIAL"
            FONTSIZE 14
            FONTBOLD .T.
            VALUE ""
            VCENTERALIGN .T.
            CENTERALIGN .T.
        End Label

        DEFINE IMAGE Image
               COL  20
               ROW  (hwin -75)/4
               PICTURE ""
               WIDTH 700
               HEIGHT (hwin -75)/2
        End Image

        DEFINE LABEL ImgDesc
            ROW 490 + ((hwin -75)/4)
            COL 25
            WIDTH 700
            FONTNAME "ARIAL"
            FONTSIZE 12
            FONTBOLD .T.
            VALUE ""
            Autosize .T.
        End Label

        DEFINE ACTIVEX Test
               ROW 10
               COL 20
               WIDTH 700
               HEIGHT hwin -75
               PROGID "shell.explorer.2"
        End Activex

        DEFINE Statusbar
                   STATUSITEM " Click here to info about the url. " action ToClipboard()
                   STATUSITEM " QrCode Check By Pierpaolo Martinello 2019" WIDTH 260 FONTCOLOR BLUE  BACKCOLOR YELLOW
        End Statusbar

        ON KEY escape action win1.release

    END WINDOW

    CENTER WINDOW Win1

    ACTIVATE WINDOW Win1 ON INIT This.OnDropFiles := {| aFiles | ResolveDrop( "Win1", HMG_GetFormControls( "Win1" ), aFiles ,"Frame_1") }

    Release hwin, address, DragFile

Return
/*
*/
*------------------------------------------------------------------------------*
Procedure Termina
*------------------------------------------------------------------------------*
   IF iswindowdefined("Win1")
      Win1.release
   EndIF
Return
/*
*/
*------------------------------------------------------------------------------*
Procedure TestNavigate( img )
*------------------------------------------------------------------------------*
Local cAddress, url, dlocal := GetTempFolder()+"\UnzipQr\" , aFiles  , aFiles1:={}
Local ValidExt :={".ZIP",".JPG",".PNG",".BMP",".ICO",".GIF",".TIF"}

    IF valtype(img)== "A"
       img := img[1]
    EndIF

    IF file( img ) // Processing images

       IF upper(hb_FNameExt( IMG )) == ".ZIP"  //extract First image from Zip
          IF DirChange( dLocal ) > 0
             MakeDir( dLocal )
          EndIF
          IF HB_UNZIPFILE( img,,.f.,, dLocal )
             inkeygui(1000)
             aFiles := directory(dlocal+"*.*")
             ASort(afiles,,, { |x, y| x[1] > y[1] })
             aeval(afiles,{|x|aadd(afiles1,dlocal+x[1])})
             IF len (aFiles1) > 0
                Img := aFiles1[1]
                m->DragFile := IMG
             Else
                msgStop("Error checking out the file in "+dlocal,"Aborted procedure [1] " )
                Termina ()
             EndIF
          Else
             msgStop("Error checking out the file in "+dlocal,"Aborted procedure [2] " )
             Termina ()
          EndIF

       EndIF

       IF ASCAN(ValidExt, upper(hb_FNameExt( IMG )) ) < 1      // reject other file formats
          msgstop(img,"Invalid file format !")
          Return
       EndIF

       Url :=  scan( img )    // Read The img

    Else
       Return
    EndIF

    IF Valtype(url) == "U"   // Prevent missing part executables and necessarie
       msgStop("Missing Zbar program!"+CRLF+CRLF+"Please copy Zbar program in: ";
              +cFilePath(GetExeFileName())+CRLF+CRLF+"Action Ended!","Unable to use this demo")
       Return
    Else                     //  Image found but no valid read  -> Show Img
       IF Empty(Url) .And. ASCAN(ValidExt, upper(hb_FNameExt( IMG )) ) > 1
          Win1.Image.Picture   := Img
          Win1.ImgDesc.value   := Img
          Win1.ImgDesc.Visible := .T.
          Win1.Image.Visible   := .T.
          Win1.Test.Visible    := .F.
          Win1.cResult.Visible := .F.
          Win1.ImgDesc.value   := Img
          Win1.Frame_1.Redraw
          m->address := Img
          Return
       EndIF
    EndIF

    IF (lower(left(url,4)) = "http" )
       Win1.Test.Visible := .T.
    Else                    //  Hide Other Controls
       Win1.cResult.Visible := .T.
       Win1.Test.Visible    := .F.
       Win1.Image.Visible   := .F.
       Win1.ImgDesc.Visible := .F.
       Win1.cResult.value   := Url
    EndIF

    IF empty( url )         //  Latest possibility to navigate with a valid url
       cAddress := InputBox ('Navigate:','Enter Address', url )
    Else
       cAddress := Url
    EndIF

    m->address := cAddress

    IF Empty (cAddress)
       Termina ()
    ElseIF lower(left(url,4)) = "http"
       Win1.Test.XObject:Navigate(cAddress)
    EndIF

Return

*-----------------------------------------------------------------------------*
FUNCTION Scan( img )
*-----------------------------------------------------------------------------*
   LOCAL cCommand := "zbarimg.exe --raw -q " + ["]+img+["]
   LOCAL hProcess, cResult
   LOCAL hStdOut, hStderr, nState, nBytes
   LOCAL cBuff := Space( 1024 )

   hProcess := hb_ProcessOpen( cCommand, NIL, @hStdOut, @hStdErr, .T. )

   IF hProcess != -1

      nState := hb_ProcessValue( hProcess, .T. )

      WHILE nState <> -1

         nBytes := FRead( hStdOut, @cBuff, 1024 /* cBuff length */ )

         IF nBytes == 0
            EXIT
         EndIF

         nState := hb_ProcessValue( hProcess, .T. )

      END

      cBuff   := StrTran( cBuff, Chr( 13 ) )
      cBuff   := StrTran( cBuff, Chr( 10 ) )
      cResult := CharRem( " ", cBuff )

      hb_ProcessClose( hProcess )

   EndIF

Return cResult
/*
*/
*-----------------------------------------------------------------------------*
Procedure ToClipboard ()
*-----------------------------------------------------------------------------*
   IF empty(m->address)
      msgstop("There are no valid addresses with: "+CRLF+CRLF+ m->address )
      Return
   EndIF
   IF msgyesno( m->address+CRLF+CRLF+"Do you want to copy the Url to the clipboard?","Reading the qrcode shows:")
      CopyToClipboard( m->address )
   EndIF
Return
/*
*/
*-----------------------------------------------------------------------------*
Procedure ResolveDrop( cForm, aCtrl, aFiles, cTForm )
* 13/08/2019 modified by Pierpaolo Martinello
*-----------------------------------------------------------------------------*
   LOCAL mx, my, ni, tx, ty, bx, by, ct
   LOCAL aRect := { 0, 0, 0, 0 } /* tx, ty, bx, by */
   LOCAL aCtlPos := {}, cTarget
   DEFAULT cTForm TO "Frame_1"
   cTForm := upper(cTForm)

   my := GetCursorRow()  /* Mouse y position on desktop */
   mx := GetCursorCol()  /* Mouse x position on desktop */

   FOR ni = 1 TO Len( aCtrl )
      GetWindowRect( GetControlHandle( aCtrl[ ni ], cForm ), aRect )
      AAdd( aCtlPos, { upper(aCtrl[ ni ]), aRect[ 1 ], aRect[ 2 ], aRect[ 3 ], aRect[ 4 ] } )
   NEXT ni

   cTarget := ""
   ni      := 0
   DO WHILE ni < Len( aCtlPos ) .AND. Len( cTarget ) == 0
      ni += 1
      tx := aCtlPos[ ni, 2 ] /* Top-Left Corner x */
      ty := aCtlPos[ ni, 3 ] /* Top-Left Corner y */
      bx := aCtlPos[ ni, 4 ] /* Right-Bottom Corner x */
      by := aCtlPos[ ni, 5 ] /* Right-Bottom Corner y */

      IF mx >= tx .AND. mx <= bx .AND. my >= ty .AND. my <= by .and. actlpos[ni,1] == cTForm
         cTarget := upper(aCtlPos[ ni, 1 ])
      EndIF
   ENDDO

   IF Len( cTarget ) > 0
      cTarget := upper (cTarget)
      ct := GetControlType( cTarget, cForm )
      IF CT == "FRAME" .and. cTForm == cTarget
         IF valtype(aFiles)== "A"
            m->DragFile := Afiles[1]
         EndIF
         TestNavigate( m->DragFile )
      EndIF
   EndIF

Return
