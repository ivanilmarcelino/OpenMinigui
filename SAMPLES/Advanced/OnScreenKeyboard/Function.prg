/*************************************************************************
* MINIGUI - Harbour Win32 GUI library Demo                               *
*                                                                        *
* Copyright 2002-05 Roberto Lopez <harbourminigui@gmail.com>             *
* http://harbourminigui.googlepages.com/                                 *
*                                                                        *
* Copyright 2003-18 Grigory Filatov <gfilatov@inbox.ru>                  *
*                                                                        *
*                                                                        *
* Copyright 1989-2018 Kristjan Žagar <kristjan.zagar@me.com>             *
* Updated 12.10.2017                                                     *
* Europa Slovenija                                                       *
* Edited with PSPad                                                      *
*************************************************************************/


#include "hmg.ch"

#DEFINE PROGRAM 'OnScreenKeyboard'
#DEFINE VERSION ' Version S7.40'
#DEFINE COPYRIGHT ' By Kristjan, 1989-2017'
#DEFINE NTRIM( n ) LTRIM( STR( n ) )

Function LeftMouseClick()
Return ( _HMG_MouseState == 1 )

Function RightMouseClick()
Return ( _HMG_MouseState == 2 )


*------------------------------------------------------------*
STATIC FUNCTION To_Unicode( cString )
*------------------------------------------------------------*
     LOCAL i, cTemp := ""

     FOR i := 1 TO Len( cString )
          cTemp += SubStr( cString, i, 1 ) + Chr( 0 )
     NEXT
     cTemp += Chr( 0 )

RETURN cTemp

/****************************************************************************************************
** font_size ()         return exact font width and height
**    fontsize("label",sFontmane,nfontsize,lbold,litalic)
*     option return width,height, (if u have already writen on screen to get exact row and col,
*     move for row +return(,,row) , col+return(,,,col)
** 1989-2018 Kristjan Žagar <kristjan.zagar@me.com>                                                 *
** Europa Slovenija                                                                                 *
** Edited with PSPad                                                                                *
****************************************************************************************************/

function font_size(txt_1,lxname,nxsize,lxbold,lxitalic)
     LOCAL hDC, BTstruct, nTypeText, nAlingText, nOrientation, t_hDC, t_BTstruct, t_BTSTRUCT_TEST, tn_BTSTRUCT_TEST, newStruct,;
          t_hBitmap, tb_hBitmap, t_aRGBcolor := {153,217,234}, t_nTypeText, t_nAlingText, t_nOrientation, t_ik:=0, m_zg:=0, m_sp:=0 ,;
          m_lv:=0, m_ds:=0, levo_pix:=0, ZG_PIX:=0

     t_nTypeText    := BT_TEXT_OPAQUE               // +  BT_TEXT_TRANSPARENT
     IF lxbold
          t_nTypeText    := BT_TEXT_BOLD            // BT_TEXT_OPAQUE + BT_TEXT_BOLD   BT_TEXT_TRANSPARENT +
     ENDIF
     IF   lxItalic
          t_nTypeText := t_nTypeText +  BT_TEXT_ITALIC
     ENDIF
     IF  lUnderline
          t_nTypeText := t_nTypeText +  BT_TEXT_UNDERLINE
     ENDIF
     IF lStrikeout
          t_nTypeText := t_nTypeText + BT_TEXT_STRIKEOUT
     ENDIF
     t_hBitmapTEST := BT_BitmapCreateNew (0, 0, { GetRed(nBkColor),GetGreen(nBkColor),GetBlue(nBkColor) })
     t_hDCTEST := BT_CreateDC (t_hBitmapTEST, BT_HDC_BITMAP, @t_BTSTRUCT_TEST)
     t_SIZE_TXT:=  BT_DrawTextSize (t_hDCTEST, txt_1, lxname, nxsize, t_nTypeText)
     BT_BitmapRelease( t_hBitmapTEST )
     BT_DeleteDC (t_BTSTRUCT_TEST)
     t_nAlingText   := BT_TEXT_LEFT  + BT_TEXT_TOP  //BT_TEXT_TOP   //+ BT_TEXT_TOP   BT_TEXT_LEFT + BT_TEXT_BASELINE
     t_nOrientation :=  BT_TEXT_NORMAL_ORIENTATION
     t_hBitmap := BT_BitmapCreateNew (t_SIZE_TXT[1]+100, t_SIZE_TXT[2]+100,{ GetRed(nBkColor),GetGreen(nBkColor),GetBlue(nBkColor) } )
     t_hDC := BT_CreateDC (t_hBitmap, BT_HDC_BITMAP, @t_BTstruct)
     BT_DrawText (t_hDC,50,50,txt_1,lxname, nxsize, { GetRed(nColor),GetGreen(nColor),GetBlue(nColor) },;
          WHITE, t_nTypeText, t_nAlingText, t_nOrientation)
     aPixnic:=  BT_DrawGetPixel (t_hDC,0,0)
     maxi_n:=BT_BitmapHeight (t_hBitmap)
     maxi_k:=BT_BitmapWidth (t_hBitmap)
     for k:=0 to maxi_k -1
          for n:=0 to maxi_n -1
               aPix:=  BT_DrawGetPixel (t_hDC,n,k)
               if !(aPixnic[1]= aPix[1] .and. aPixnic[2]= aPix[2] .and. aPixnic[3]= aPix[3] )
                    levo_pix=k
                    exit
               endif
          next n
          if levo_pix>0
               exit
          endif
     next k
     for n:=0 to maxi_n -1
          for k:=maxi_k -1   to 0 step -1
               aPix:=  BT_DrawGetPixel (t_hDC,n,k)
               if !(aPixnic[1]= aPix[1] .and. aPixnic[2]= aPix[2] .and. aPixnic[3]= aPix[3] )
                    zg_PIX=n
                    exit
               endif
          next k
          if ZG_PIX>0
               exit
          endif
     next n
     BT_DrawText (t_hDC,50,50,txt_1,lxname, nxsize, { GetRed(nColor),GetGreen(nColor),GetBlue(nColor) },;
          { GetRed(nBkColor),GetGreen(nBkColor),GetBlue(nBkColor) }, t_nTypeText, t_nAlingText, t_nOrientation)
     for k:=0 to maxi_k -1
          for n:=0 to maxi_n -1
               aPix:=  BT_DrawGetPixel (t_hDC,n,k)
               if !(aPixnic[1]= aPix[1] .and. aPixnic[2]= aPix[2] .and. aPixnic[3]= aPix[3] )
                    m_lv=k
                    exit
               endif
          next n
          if m_lv>0
               exit
          endif
     next k
     for k:=maxi_k -1 to 0 step -1
          for n:=0 to maxi_n -1
               aPix:=  BT_DrawGetPixel (t_hDC,n,k)
               if !(aPixnic[1]= aPix[1] .and. aPixnic[2]= aPix[2] .and. aPixnic[3]= aPix[3] )
                    m_ds=k
                    exit
               endif
          next n
          if m_ds>0
               exit
          endif
     next k
     for n:=maxi_n -1 to 0   step -1
          for k:=0 to maxi_k -1
               aPix:=  BT_DrawGetPixel (t_hDC,n,k)
               if !(aPixnic[1]= aPix[1] .and. aPixnic[2]= aPix[2] .and. aPixnic[3]= aPix[3] )
                    m_sp=n
                    exit
               endif
          next k
          if m_sp>0
               exit
          endif
     next n
     for n:=0 to maxi_n -1
          for k:=maxi_k -1   to 0 step -1
               aPix:=  BT_DrawGetPixel (t_hDC,n,k)
               if !(aPixnic[1]= aPix[1] .and. aPixnic[2]= aPix[2] .and. aPixnic[3]= aPix[3] )
                    m_zg=n
                    exit
               endif
          next k
          if m_zg>0
               exit
          endif
     next n
     m_VIS:=m_sp-m_zg
     m_SIR:=m_ds-m_lv
     BT_BitmapRelease( t_hBitmap )
     BT_DeleteDC (t_BTstruct)
     clean memory
     ///width  , height , corection height, corection width  (move row and col "label")
RETURN  {m_SIR+2,m_VIS+2,(m_zg-ZG_PIX),(m_lv-LEVO_PIX)}


FUNCTION SysWait( nWait )
     LOCAL iTime := SECONDS()
     DEFAULT nWait TO 2
     DO WHILE SECONDS() - iTime < nWait
          INKEY(0.01)
          DO EVENTS
     ENDDO
RETURN NIL

/****************************************************************************************************
** chartonum ()         Character to numeric - strzero                                              *
** 1989-2017 Kristjan Žagar <kristjan.zagar@me.com>                                                 *
** Europa Slovenija                                                                                 *
** Edited with PSPad                                                                                *
****************************************************************************************************/

function chartonum(cNUM)
     local numb_er:=0,lnum:=0,nbr:=0 , n:=0
     default cNum to 0
     cnum:=CHARONLY("0123456789.",cnum)
     cNumDec:=cnum
     cnum:=alltrim(cnum)
     lnum:=len (cnum)
     for n:=1 to lnum
          nbr:=AsciiSum (substr(cNum,lnum-(n-1),1))- 48
          SWITCH n
     CASE 0
          numb_er:=numb_er+(0*nbr)
          EXIT
     CASE 1
          numb_er:=numb_er+(1*nbr)
          EXIT
     CASE 2
          numb_er:=numb_er+(10*nbr)
          EXIT
     CASE 3
          numb_er:=numb_er+(100*nbr)
          EXIT
     CASE 4
          numb_er:=numb_er+(1000*nbr)
          EXIT
     CASE 5
          numb_er:=numb_er+(10000*nbr)
          EXIT
     CASE  6
          numb_er:=numb_er+(100000*nbr)
          EXIT
     CASE 7
          numb_er:=numb_er+(1000000*nbr)
          EXIT
     CASE 8
          numb_er:=numb_er+(10000000*nbr)
          EXIT
          END SWITCH
     next n
     poz:= at(".",cNumDec)
     if poz=0
          return numb_er
     else
          dec_numb_er:=numb_er
          nDec:=substr(cNumDec,poz+1)
          numb_er:=0
          lnum:=0
          nbr:=0
          n:=0
          nDec:=alltrim(nDec)
          lnum:=len (nDec)
          for n:=1 to lnum
               nbr:=AsciiSum (substr(nDec,lnum-(n-1),1))- 48
               SWITCH n
          CASE 0
               numb_er:=numb_er+(0.0*nbr)
               EXIT
          CASE 1
               numb_er:=numb_er+(0.1*nbr)
               EXIT
          CASE 2
               numb_er:=numb_er+(0.01*nbr)
               EXIT
          CASE 3
               numb_er:=numb_er+(0.001*nbr)
               EXIT
          CASE 4
               numb_er:=numb_er+(0.0001*nbr)
               EXIT
          CASE 5
               numb_er:=numb_er+(0.00001*nbr)
               EXIT
          CASE  6
               numb_er:=numb_er+(0.000001*nbr)
               EXIT
          CASE 7
               numb_er:=numb_er+(0.0000001*nbr)
               EXIT
          CASE 8
               numb_er:=numb_er+(0.00000001*nbr)
               EXIT
               END SWITCH
          next n

          numb_er:= dec_numb_er+numb_er
     endif
return numb_er

********************************************************************************
*   MyRGB( { 183, 130, 122 } )                                                 *
* function for rgb colors                                                      *
********************************************************************************
FUNCTION MyRGB(aDim)
     default aDim to {245,245,245}
     RETURN RGB(aDim[1],aDim[2],aDim[3])