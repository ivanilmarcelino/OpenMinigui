/*
   Lira Lira Oscar Joel [oSkAr]
   Simple Example of ActiveX With Events Support

   Adapted for MiniGUI by Grigory Filatov
*/

#include "hmg.ch"
#include "hbclass.ch"

/*-----------------------------------------------------------------------------------------------*/
FUNCTION Main()
   InternetExplorer():New()
   RETURN NIL

/*-----------------------------------------------------------------------------------------------*/
CLASS InternetExplorer
   DATA oWActiveX
   DATA oActiveX
   METHOD New()
   METHOD Navigate()
   METHOD Resize()
   METHOD OnStatusBar(p1)
   METHOD OnProgressChange(nProgress, nProgressMax)
   METHOD OnTitleChange(cText)
ENDCLASS

/*-----------------------------------------------------------------------------------------------*/
METHOD New()
   DEFINE WINDOW Win_Nav ;
         WIDTH 640 ;
         HEIGHT 480 ;
         TITLE 'ActiveX With Events Support' ;
         MAIN ;
         ON INIT ChangeStyle( ::oWActiveX:hWnd, WS_EX_STATICEDGE, , .T. ) ;
         ON RELEASE ::oWActivex:Release() ;
         ON SIZE ::ReSize() ;
         ON MAXIMIZE ::ReSize()

      DEFINE STATUSBAR
         STATUSITEM ""
         PROGRESSITEM WIDTH 100 RANGE 0, 100
      END STATUSBAR

      @ 0, 04 LABEL Lbl_Dir ;
         VALUE "Direction:" ;
         WIDTH 54 ;
         HEIGHT 23 ;
         VCENTERALIGN

      @ 0, GetProperty( "Win_Nav", "Lbl_Dir", "Width" ) + 4 TEXTBOX Txb_Edit ;
         HEIGHT 23 ;
         WIDTH 500 ;
         ON ENTER ::Navigate()

      @ 0, GetProperty( "Win_Nav", "Txb_Edit", "Col" ) + 500 BUTTON Btn_Navigate ;
         CAPTION 'Navigate' ;
         ACTION ::Navigate() ;
         WIDTH 70 ;
         HEIGHT 23

   END WINDOW

   WITH OBJECT ::oWActiveX := TActiveX():New( "Win_Nav", "Shell.Explorer.2", 24, 0, ;
         GetProperty( "Win_Nav", "width" ) - 2 * GetBorderHeight(), ;
         GetProperty( "Win_Nav", "height" ) - 24 - GetTitleHeight() - 2 * GetBorderHeight() - Win_Nav.Statusbar.Height )
      //Event Map
      :EventMap( 102, { |p1|    ::OnStatusBar( p1 ) } )
      :EventMap( 108, { |p1,p2| ::OnProgressChange( p1, p2 ) } )
      :EventMap( 113, { |p1|    ::OnTitleChange( p1 ) } )
      //End Event Map
   END

   SetProperty( "Win_Nav", "Txb_Edit", "value", "http://hmgextended.com" )

   WITH OBJECT ::oActiveX := ::oWActiveX:Load()
      :Silent := 1
      :Navigate( GetProperty( "Win_Nav", "Txb_Edit", "value" ) )
   END

   ::ReSize()

   CENTER WINDOW Win_Nav
   ACTIVATE WINDOW Win_Nav
   RETURN NIL

/*-----------------------------------------------------------------------------------------------*/
METHOD Navigate()
   ::oActiveX:Navigate( GetProperty( "Win_Nav", "Txb_Edit", "value" ) )
   RETURN NIL

/*-----------------------------------------------------------------------------------------------*/
METHOD Resize()
   LOCAL nWidth := GetProperty( "Win_Nav", "width" ),;
      nBtnWidth := GetProperty( "Win_Nav", "Btn_Navigate", "Width" )

   SetProperty( "Win_Nav", "Btn_Navigate", "Col", nWidth - nBtnWidth - 2 * GetBorderHeight() )
   SetProperty( "Win_Nav", "Txb_Edit", "Width", nWidth - nBtnWidth - GetProperty( "Win_Nav", "Lbl_Dir", "Width" ) - 4 - 2 * GetBorderHeight() )

   ::oWActiveX:Adjust()
   RETURN NIL

/*-----------------------------------------------------------------------------------------------*/
METHOD OnStatusBar( p1 )
   Win_Nav.StatusBar.Item( 1 ) := p1
   RETURN NIL

/*-----------------------------------------------------------------------------------------------*/
METHOD OnProgressChange( nProgress, nProgressMax )
   SET STATUSBAR ProgressItem OF Win_Nav RANGE TO 0, nProgressMax
   SET STATUSBAR ProgressItem OF Win_Nav POSITION TO nProgress
   RETURN NIL

/*-----------------------------------------------------------------------------------------------*/
METHOD OnTitleChange( cText )
   Win_Nav.Title := cText
   RETURN NIL