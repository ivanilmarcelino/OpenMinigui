/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
*/
#define  _HMG_OUTLOG

#include "hmg.ch"
#include "tsbrowse.ch"
#include "dbinfo.ch"

REQUEST DBFCDX

SET PROCEDURE TO demo3_.prg

FUNCTION Main()
   LOCAL cForm := "wMain", oac := App.Cargo
   LOCAL nY, nX, nH, nW, nGw, owc
   LOCAL cTitl := "Array TBrowse. DEMO3. "
   LOCAL aBColor := {0, 191, 255}
   
   nY := nX := 0
   nW := Sys.DesktopWidth
   nH := GetTitleHeight() + GetBorderHeight()
   nH += App.Object:H2 + App.Object:nMargHeight * 2

   DEFINE WINDOW &cForm AT nY, nX WIDTH nW HEIGHT nH        ;
          TITLE cTitl + space(3) + MiniGuiVersion()         ;
          MAIN NOSIZE  TOPMOST BACKCOLOR aBColor            ;
          ON INTERACTIVECLOSE Len(HMG_GetForms("S")) == 0   ;
          ON INIT    ( This.Topmost := .F., _wPost(0) )     ;
          ON RELEASE ( This.Hide, _wSend(90) )
          This.Cargo := oHmgData() ; owc := This.Cargo

      owc:lOnInit   := .F.    //
      owc:cFocus    := "Buff" // remember the focus of the restore on the form
      owc:cBtn_Exit := ""     // exit button name
      owc:cLastBtn  := ""     // button name standard window
      owc:nLastWnd  := 0      // standard window counter for sorting
      owc:aBtn := {{"File 1"    , "Test window STANDARD"}, ; // 1
                   {"File 2"    , "Test window STANDARD"}, ; // 2
                   {"File 1 x 2", "Test window STANDARD"}, ; // 3
                   {"File 2 x 2", "Test window STANDARD"}, ; // 4
                   {"File 1 x 9", "Test window STANDARD"}, ; // 5
                   {"File 2 x 9", "Test window STANDARD"}, ; // 6
                   {                                    }, ; // 7
                   {                                    }, ; // 8
                    "Exit"}

      @ 0,0 LABEL &(owc:cFocus) VALUE "" WIDTH 2 HEIGHT 2 TRANSPARENT 

      ON KEY F1 ACTION NIL

      ButtonBar(owc:aBtn)

      WITH OBJECT This.Object
       :Event( 0, {|ow      | 
                    Local owc := ow:Cargo
                    owc:lOnInit := .T.
                    ow:SetFocus(owc:cFocus) 
                    Return Nil
                    })
       :Event( 1, {|ow,ky,cn|                   // button 1
                    Local owc := ow:Cargo
                    Local aBC := {255, 250, 205}   // LemonChiffon  
                    Local x := ow:Col + This.&(cn).Col 
                    Local y := ow:Row + ow:Height 
                    ow:Enabler(cn, .T.)
                    ow:Enabler(owc:cBtn_Exit, .F.)
                    ky := My_File(ow, cn, "S", y, x, 1, 0, aBC)
                    IF Empty(ky) ; _wPost(22, ow:Name)
                    ENDIF
                    Return Nil
                    })
       :Event( 2, {|ow,ky,cn|                   // button 2
                    Local owc := ow:Cargo
                    Local aBC := {240, 230, 140}   // Khaki  
                    Local x := ow:Col + This.&(cn).Col 
                    Local y := ow:Row + ow:Height 
                    ow:Enabler(cn, .T.)
                    ow:Enabler(owc:cBtn_Exit, .F.)
                    ky := My_File(ow, cn, "S", y, x, 2, 0, aBC)
                    IF Empty(ky) ; _wPost(22, ow:Name)
                    ENDIF
                    Return Nil
                    })
       :Event( 3, {|ow,ky,cn|                   // button 3
                    Local owc := ow:Cargo
                    Local aBC := {216, 191, 216}   // Thistle  
                    Local x := ow:Col + This.&(cn).Col 
                    Local y := ow:Row + ow:Height
                    ow:Enabler(cn, .T.)
                    ow:Enabler(owc:cBtn_Exit, .F.)
                    ky := My_File(ow, cn, "S", y, x, 1, 1, aBC)
                    IF Empty(ky) ; _wPost(22, ow:Name)
                    ENDIF
                    Return Nil
                    })
       :Event( 4, {|ow,ky,cn|                   // button 4
                    Local owc := ow:Cargo
                    Local aBC := {147, 112, 219}   // MediumPurple  
                    Local x := ow:Col + This.&(cn).Col 
                    Local y := ow:Row + ow:Height
                    ow:Enabler(cn, .T.)
                    ow:Enabler(owc:cBtn_Exit, .F.)
                    ky := My_File(ow, cn, "S", y, x, 2, 1, aBC)
                    IF Empty(ky) ; _wPost(22, ow:Name)
                    ENDIF
                    Return Nil
                    })
       :Event( 5, {|ow,ky,cn|                   // button 5
                    Local owc := ow:Cargo
                    Local aBC := {244, 164, 96}   // SandyBrown  
                    Local x := ow:Col + This.&(cn).Col 
                    Local y := ow:Row + ow:Height
                    ow:Enabler(cn, .T.)
                    ow:Enabler(owc:cBtn_Exit, .F.)
                    ky := My_File(ow, cn, "S", y, x, 1, 9, aBC)
                    IF Empty(ky) ; _wPost(22, ow:Name)
                    ENDIF
                    Return Nil
                    })
       :Event( 6, {|ow,ky,cn|                   // button 6
                    Local owc := ow:Cargo
                    Local aBC := {144, 238, 144}   // LightGreen  
                    Local x := ow:Col + This.&(cn).Col 
                    Local y := ow:Row + ow:Height
                    ow:Enabler(cn, .T.)
                    ow:Enabler(owc:cBtn_Exit, .F.)
                    ky := My_File(ow, cn, "S", y, x, 2, 9, aBC)
                    IF Empty(ky) ; _wPost(22, ow:Name)
                    ENDIF
                    Return Nil
                    })
       :Event(22, {|ow|                         // is window standard
                    Local owc := ow:Cargo, awo, nwo, ows, cws, i
                    Local awn := HMG_GetForms("S")  // standard
                    Local awm := HMG_GetForms("M")  // modal
                    IF Len(awn) == 0
                       owc:cLastBtn := ""      // no last button pressed
                       owc:nLastWnd := 0       // ID counter from the beginning
                       ow:Enabler(owc:cBtn_Exit, .T.)
                    ELSEIF Len(awm) > 0
                    ELSE
                       awo := HMG_GetForms("S", .T.)    // windows object
                       nwo := Len(awo)
                       awn := array(nwo)
                       FOR i := 1 TO nwo
                           ows := awo[ i ]      // object window standard
                           awn[ i ] := StrZero(ows:Cargo:nLastWnd, 19)+"_"+ows:Name
                       NEXT
                       IF Len(awn) > 1 ; ASort(awn)
                       ENDIF
                       cws := ATail(awn)
                       cws := subs(cws, At("_", cws)+1) // name window standard
                       DoMethod(cws, "SetFocus")
                       DO EVENTS
                    ENDIF
                    Return Nil
                    })
       :Event(99, {|ow| ow:Release() })
      END WITH

   END WINDOW

   ACTIVATE WINDOW &cForm

RETURN NIL

*----------------------------------------------------------------------------*
INIT PROCEDURE Sets_ENV()
*----------------------------------------------------------------------------*
   LOCAL cFont := "Arial", nSize := 12, lDebug := .F., o
   LOCAL cLog  := hb_FNameDir (App.ExeName) + "_" + ;
                  hb_FNameName(App.ExeName) + ".log"

   rddSetDefault( "DBFCDX" )

   SET DECIMALS  TO 4
   SET EPOCH     TO 2000
   SET DATE      TO GERMAN
   SET CENTURY   ON
   SET DELETED   OFF
   SET AUTOPEN   OFF  
   SET EXACT     ON
   SET EXCLUSIVE ON
   SET SOFTSEEK  ON
   SET OOP ON
   SET TOOLTIPSTYLE BALLOON
   //
   IF !HB_ISOBJECT( App.Cargo ) ; App.Cargo := oHmgData()
   ENDIF

   Set ShowRedAlert On 

   SET MULTIPLE QUIT WARNING  
   SET WINDOW MAIN OFF

   SET WINDOW MODAL PARENT HANDLE ON
   SET TOOLTIPSTYLE BALLOON
   SET NAVIGATION EXTENDED
   SET MENUSTYLE  EXTENDED
   // Alert* colors
   SET MSGALERT BACKCOLOR  TO  { 247,172, 8 }
   SET MSGALERT FONTCOLOR  TO  { 0  ,  0, 0 }
   
   Sets_ENV_my()

RETURN 
