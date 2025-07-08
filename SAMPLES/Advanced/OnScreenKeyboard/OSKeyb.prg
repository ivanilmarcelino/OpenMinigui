/*************************************************************************
* MINIGUI - Harbour Win32 GUI library Demo                               *
*                                                                        *
*                                                                        *
* Copyright 1989-2018 Kristjan Žagar <kristjan.zagar@me.com>             *
* please type slow                                                       *
* Updated 12.10.2018                                                     *
* Europa Slovenija                                                       *
* Edited with PSPad                                                      *
*************************************************************************/

ANNOUNCE RDDSYS

#include "hmg.ch"

#DEFINE PROGRAM 'OnScreenKeyboard'
#DEFINE VERSION ' Version D10XS'
#DEFINE COPYRIGHT ' By Kristjan, 1989-2018'
#DEFINE NTRIM( n ) LTRIM( STR( n ) )

#DEFINE CLR_PINK   RGB( 255, 128, 128)
#DEFINE CLR_NBLUE  RGB( 128, 128, 192)
#DEFINE CLR_NBROWN  RGB( 130, 99, 53)
#DEFINE CLR_BLACK  RGB( 0, 0, 0)
#DEFINE CLR_GREEN  RGB( 0, 255, 0)
#DEFINE CLR_ORANGE  RGB( 255, 128, 0)
#DEFINE CLR_1 RGB( 190, 215, 190 )
#DEFINE CLR_2 RGB( 230, 230, 230 )
#DEFINE CLR_3 RGB( 217, 217, 255 )

DECLARE WINDOW Form_0
DECLARE WINDOW form_key

MEMVAR aButStyles

FUNCTION Main()
     //my ovn btn
     Local i, cItemName
     Local HBtn1DropMenu, HBtn2DropMenu, HBtn3DropMenu, HBtn4DropMenu

     public N_W :=0, M_W :=0, nw_Tf :=.f., mw_tf :=.f.,;
          bela_barva :={255,255,255},  max_x :=0, max_Y :=0,GUMB_BARVA :={245,245,245} , visina_program :=25,;
          rob:=0, width:=0, height:=0, Crna_barva:={255,255,255}, BELA_BARVA , Barva_okno :={60,60,60},  Barva_Okno_vrsta :={ 128,126,205},;
          control_search:="", VIsina_gumba_tipk := 150,  rob_tipk := 5, OLD_CAPSL:=.F., OLD_RCONTROL:=.f. ,;
          Button_block := 1 , CRKE_MALE [1 ]  [69 ] ,CRKE_VELIKE [1 ]  [69 ] ,CRKE_shift_MALE [1 ]  [69 ] ,CRKE_shift_VELIKE [1 ]  [69 ] ,;
          CRKE_altgr [1 ]  [69 ] ,CRKE_fn [1 ]  [69 ] ,crke [1 ]  [69 ] ,Visina_crk_t [1 ]  [69 ]  ,TIPKA_SHIFT := .F., tipka_altgr := .f.,;
          TIPKA_LCONTROL := .F.,TIPKA_RCONTROL := .F.,TIPKA_ALT := .F. ,TIPKA_FN := .F. ,TIMER_CHANGE := .F.,;
          OLD_CAPS := .F.,OLD_SHIFT := .F.,OLD_LCONTROL := .F.,OLD_ALT := .F.,OLD_ALTGR := .F.,OLD_RCONTROL := .F.,old_fn := .f., ID_stevc := 1 ,;
          win_n:= "", control_name:=  "" ,  cPicture:="",cPicture_1:="",nev_ctrlx:="", SHOW_KEYB:=.F. ,control_focus,;
          rob_tipk := 4, crke_font := 13, svetlo_rumena := { 245,245,245 } ,drsaj_naziv_t := 0 , nw := 0,drsaj_end := 0  ,;
          crke_cargo [1 ]  [69 ] ,crke_cargo_MALE [1 ]  [69 ] ,crke_cargo_VELIKE [1 ]  [69 ] ,crke_cargo_shift_MALE [1 ]  [69 ] ,crke_cargo_shift_VELIKE [1 ]  [69 ] ,;
          crke_cargo_FN [1 ]  [69 ] ,;
          n_pos := 0 ,n_pos_1 := 0, del_all := .t.,star_stg := 0 ,  Button_block := 1 ,;
          CRKE_MALE [1 ]  [69 ] ,CRKE_VELIKE [1 ]  [69 ] ,CRKE_shift_MALE [1 ]  [69 ] ,CRKE_shift_VELIKE [1 ]  [69 ] ,CRKE_altgr [1 ]  [69 ] ,CRKE_fn [1 ]  [69 ] ,;
          crke [1 ]  [69 ] ,Visina_crk_t [1 ]  [69 ]  ,;
          TIPKA_SHIFT := .F., tipka_altgr := .f., TIPKA_LCONTROL := .F.,TIPKA_RCONTROL := .F.,TIPKA_ALT := .F. ,TIPKA_FN := .F. ,TIMER_CHANGE := .F.,;
          OLD_CAPS := .F.,OLD_SHIFT := .F.,OLD_LCONTROL := .F.,OLD_ALT := .F.,OLD_ALTGR := .F.,OLD_RCONTROL := .F.,old_fn := .f.

     max_x :=GetDesktopHEIGHT ()
     max_y :=GetDesktopWIDTH ()

     /// my own button :) i will try it
     aButStyles :={;
          {0,4, Barva_okno  ,Barva_okno  ,Barva_okno  ,Barva_okno  , Crna_barva,Crna_barva,1, Crna_barva,Crna_barva },;
          {998,4,    {0,255,164}  ,  {255,255,255}, {0,255,164} , {255,255,255},  BLACK,BLACK,6, {255,0,100},WHITE },;  // KEYBOARD
     {999,6,    {130,130,130}  ,  {255,255,255},  {110,110,110} , {255,255,255},  BLACK,BLACK,6, {255,0,100},WHITE },;  // KEYBOARD
     }


     cPath :=GetStartupFolder()

     DEFINE FONT FONT_KEYB FONTNAME 'Arial' SIZE 13 DEFAULT  // font tipkovnice

     SET CENTURY ON
     SET DELETED ON
     SET BROWSESYNC ON
     SET EPOCH TO ( YEAR(DATE())-50 )
     SET LANGUAGE TO SLOVENIAN
     SET CODEPAGE TO SLOVENIAN
     SET DATE GERMAN
     SET DATE FormAT TO "dd.MM.yyyy"
     SET DECIMALS TO 8
     SET INTERACTIVECLOSE ON
     SET AUTOADJUST ON
     SET AUTOZOOMING ON
     SET MENUSTYLE EXTENDED
     SET MULTIPLE OFF WARNING
     SET MENUCURSOR FULL
     SET MENUSEPARATOR SINGLE LEFTALIGN
     SET LOGERROR ON
     Set ShowDetailError On

     DEFINE WINDOW Form_0 			;
          AT 0,0 				;
          clientarea 660,400;
          TITLE PROGRAM		;
          backcolor {60,60,60};
          ICON 'MAINICO' ;
          MAIN


     DEFINE LABEL Label_10_1_1
     ROW    10
     COL    30
     WIDTH  120
     HEIGHT 20
     VALUE "Simple Code:"
     TRANSPARENT .T.
     END LABEL


     DEFINE LABEL Label_10_1_2
     ROW    60
     COL    30
     WIDTH  120
     HEIGHT 20
     VALUE "ZFA-17529/Z"
     TRANSPARENT .T.
     END LABEL

     DEFINE LABEL Label_10_1_3
     ROW    110
     COL    30
     WIDTH  120
     HEIGHT 20
     VALUE "12.123.123"
     TRANSPARENT .T.
     END LABEL

     DEFINE LABEL Label_10_1_4
     ROW    160
     COL    30
     WIDTH  120
     HEIGHT 20
     VALUE "1234-1234-1234-1234"
     TRANSPARENT .T.
     END LABEL

     DEFINE LABEL Label_10_1_5
     ROW    210
     COL    30
     WIDTH  120
     HEIGHT 20
     VALUE "Label_5"
     TRANSPARENT .T.
     END LABEL


     DEFINE GETBOX EDIT_10_1
     ROW    30
     COL    30
     WIDTH  200
     HEIGHT 24
     FONTNAME 'Arial'
     picture  "xxxxxxxxxxxxxxxxxxxxxxxxxxxx"
     VALUE  "                                                                                   "
     END GETBOX

     DEFINE GETBOX EDIT_10_2
     ROW    80
     COL    30
     WIDTH  200
     HEIGHT 24
     FONTNAME 'Arial'
     picture  "AAA-99999/A"
     VALUE  "                                                                                   "
     END GETBOX

     DEFINE GETBOX EDIT_10_3
     ROW    130
     COL    30
     WIDTH  200
     HEIGHT 24
     FONTNAME 'Arial'
     picture  "99.999.999"
     VALUE  "                                                                                   "
     END GETBOX

     DEFINE GETBOX EDIT_10_4
     ROW    180
     COL    30
     WIDTH  200
     HEIGHT 24
     FONTNAME 'Arial'
     picture  "9999-9999-9999-9999"
     VALUE  "                                                                                   "
     END GETBOX

     DEFINE GETBOX EDIT_10_5
     ROW    230
     COL    30
     WIDTH  200
     HEIGHT 24
     FONTNAME 'Arial'
     picture  "AA-999/9(A-AAA)"
     VALUE  "                                                                                   "
     END GETBOX

     DEFINE TEXTBOX EDIT_10_6
     ROW    30
     COL    270
     WIDTH  80
     FONTNAME 'Arial'
     FONTSIZE 9
     FONTBOLD .F.
     FONTITALIC .F.
     FONTUNDERLINE .F.
     FONTSTRIKEOUT .F.
     TOOLTIP ''
     VALUE ""
     END TEXTBOX

     DEFINE IPADDRESS EDIT_10_7
     ROW    80
     COL    270
     WIDTH  120
     HEIGHT 24
     FONTNAME 'Arial'
     TOOLTIP ''
     END IPADDRESS

     DEFINE RICHEDITBOX EDIT_10_8
     ROW    130
     COL    270
     WIDTH  126
     HEIGHT 125
     VALUE ''
     FONTNAME 'Arial'
     TOOLTIP ''
     MAXLENGTH  NIL
     END RICHEDITBOX

     DEFINE EDITBOX EDIT_10_9
     ROW    30
     COL    420
     WIDTH  213
     HEIGHT 226
     VALUE ''
     FONTNAME 'Arial'
     TOOLTIP ''
     MAXLENGTH  NIL
     END EDITBOX

     DEFINE BUTTONEX Button_1
     ROW    315
     COL    440
     WIDTH  44
     HEIGHT 27
     FONTNAME 'Arial'
     PICTURE "GOR_K"
     TOOLTIP ''
     backcolor WHITE
     noxpstyle .T.
     NOtabstop .T.
     VCENTERALIGN .t.
     CENTERALIGN .t.
     FLAT .T.
     BORDER .T.
     adjust .t.
     BACKGROUNDCOLOR YELLOW
     TRANSPARENT .T.
     ACTION IF(iswindowactive("FORM_KEY"),(SETPROPERTY("FORM_KEY","ROW",GETPROPERTY("FORM_KEY","ROW")-5)),)
     END BUTTON

     DEFINE BUTTONEX Button_2
     ROW    355
     COL    440
     WIDTH  44
     HEIGHT 27
     FONTNAME 'Arial'
     PICTURE "DOL_K"
     TOOLTIP ''
     backcolor WHITE
     noxpstyle .T.
     NOtabstop .T.
     VCENTERALIGN .t.
     CENTERALIGN .t.
     FLAT .T.
     BORDER .T.
     adjust .t.
     BACKGROUNDCOLOR YELLOW
     TRANSPARENT .T.
     ACTION IF(iswindowactive("FORM_KEY"),(SETPROPERTY("FORM_KEY","ROW",GETPROPERTY("FORM_KEY","ROW")+5)),)
     END BUTTON

     DEFINE BUTTONEX Button_3
     ROW    355
     COL    500
     WIDTH  44
     HEIGHT 27
     FONTNAME 'Arial'
     PICTURE "DESNO_K"
     TOOLTIP ''
     backcolor WHITE
     noxpstyle .T.
     NOtabstop .T.
     VCENTERALIGN .t.
     CENTERALIGN .t.
     FLAT .T.
     BORDER .T.
     adjust .t.
     BACKGROUNDCOLOR YELLOW
     TRANSPARENT .T.
     ACTION IF(iswindowactive("FORM_KEY"),(SETPROPERTY("FORM_KEY","COL",GETPROPERTY("FORM_KEY","COL")+5)),)
     END BUTTON

     DEFINE BUTTONEX Button_4
     ROW    355
     COL    380
     WIDTH  44
     HEIGHT 27
     FONTNAME 'Arial'
     PICTURE "LEVO_K"
     TOOLTIP ''
     backcolor WHITE
     noxpstyle .T.
     NOtabstop .T.
     VCENTERALIGN .t.
     CENTERALIGN .t.
     FLAT .T.
     BORDER .T.
     adjust .t.
     BACKGROUNDCOLOR YELLOW
     TRANSPARENT .T.
     ACTION IF(iswindowactive("FORM_KEY"),(SETPROPERTY("FORM_KEY","COL",GETPROPERTY("FORM_KEY","COL")-5)),)
     END BUTTON

     DEFINE BUTTONEX ButtonEX_1
     ROW    300
     COL    20
     WIDTH  80
     HEIGHT 80
     FONTNAME 'Arial'
     PICTURE "TIPKO"
     TOOLTIP ''
     HANDCURSOR .F.
     action  IF( IsWindowDEFINEd("Form_KEY")  , ( DoMethod("Form_KEY","release"), SETPROPERTY("Form_0","ButtonEX_1","PICTURE", "TIPKO") ,;
          SETPROPERTY("Form_0","WIDTH_KEYB","READONLY", .F.)             ,SETPROPERTY("Form_0","ROW",FORM_0_ROW) ), (IF (IsCapsLockActive(), , _PushKey(20)),;
          (  SETPROPERTY("Form_0","ButtonEX_1","PICTURE", "TIPKO_OFF"), SETPROPERTY("Form_0","WIDTH_KEYB","READONLY", .T.) ,SETPROPERTY("Form_0","ROW",50);
          ,Tipkovnica(CHARTONUM(GETPROPERTY("FORM_0","WIDTH_KEYB","VALUE")),"Form_0","Edit_10_1",cPicture_1,RED))  )  )
     noxpstyle .T.
     NOtabstop .T.
     VCENTERALIGN .t.
     CENTERALIGN .t.
     FLAT .T.
     BORDER .T.
     adjust .t.
     END BUTTONEX
     K_WIDTH:=ALLTRIM(STR(max_y-500))

     DEFINE TEXTBOX WIDTH_KEYB
     ROW    365
     COL    270
     WIDTH  60
     HEIGHT 24
     FONTNAME 'Arial'
     VALUE K_WIDTH
     FONTCOLOR BLUE
     BACKCOLOR WHITE
     CENTERALIGN  .T.
     TOOLTIP   "GetDesktopWIDTH () - 500, default"
     END TEXTBOX

     DEFINE LABEL Label_6
     ROW    367
     COL    140
     WIDTH  120
     HEIGHT 24
     VALUE "Keyboard width"
     TRANSPARENT .T.
     RIGHTALIGN .T.
     FONTCOLOR YELLOW
     END LABEL

     tab_check(thiswindow.name)

END WINDOW
CENTER WINDOW Form_0
FORM_0_ROW:=GETPROPERTY("fORM_0","ROW")
ACTIVATE WINDOW Form_0

RETURN NIL


function tab_check(win_named)
     if !_IsControlDefined ("Timer_edit_check", win_named)
          DEFINE TIMER Timer_edit_check of &win_named;
               INTERVAL 150;
               ACTION Timer_edit_check_action_search(win_named,control_search)
     endif
return nil

function Timer_edit_check_action_search(win_n,control_s)
     local ctrl_n:=getproperty(win_n,"FocusedControl")

     if  !control_search = ctrl_n
          if substr(upper(ctrl_n),1,5)="EDIT_"      // a little trick on
               if  !_IsControlDefined ("edit_check_focus",win_n)
                    define timer edit_check_focus of &win_n;
                         interval 200;
                         action GET_CLEAR_ACTION_focus_search(win_n,ctrl_n)
               endif
          endif
     endif
return nil

     /****************************************************************************************************
** GET_CLEAR_ACTION ()        Get clear position action of 0                                        *
** 1989-2016 Kristjan Žagar <kristjan.zagar@me.com>                                                 *
** Europa Slovenija                                                                                 *
** Edited with PSPad                                                                                *
****************************************************************************************************/

FUNCTION GET_CLEAR_ACTION_focus_search(win_n,ctrl_nam)
     local nev_ctrlx:=""
     default ctrl_nam to ""
     default win_n to ""
     nev_ctrlx:=getproperty(win_n,"FocusedControl")
     IF  ctrl_nam=nev_ctrlx

          if substr(upper(nev_ctrlx),1,5)="EDIT_"   // a little trick only "edit_"
               control_search:=nev_ctrlx

               setproperty( win_n,"Label_10_1_1","fontcolor", WHITE )
               setproperty( win_n,"Label_10_1_2","fontcolor", WHITE )
               setproperty( win_n,"Label_10_1_3","FONTcolor", WHITE )
               setproperty( win_n,"Label_10_1_4","fontcolor", WHITE )
               setproperty( win_n,"Label_10_1_5","fontcolor", WHITE )

               setproperty( win_n,"Label_10_1_1","fontbold", .F. )
               setproperty( win_n,"Label_10_1_2","fontbold", .F. )
               setproperty( win_n,"Label_10_1_3","fontbold", .F. )
               setproperty( win_n,"Label_10_1_4","fontbold", .F. )
               setproperty( win_n,"Label_10_1_5","fontbold", .F. )

               setproperty( win_n,"EDIT_10_1","BACKCOLOR", {255,255,255} )
               setproperty( win_n,"EDIT_10_2","BACKCOLOR", {255,255,255} )
               setproperty( win_n,"EDIT_10_3","BACKCOLOR", {255,255,255} )
               setproperty( win_n,"EDIT_10_4","BACKCOLOR", {255,255,255} )
               setproperty( win_n,"EDIT_10_5","BACKCOLOR", {255,255,255} )
               setproperty( win_n,"EDIT_10_6","BACKCOLOR", {255,255,255} )
               setproperty( win_n,"EDIT_10_7","BACKCOLOR", {255,255,255} )
               setproperty( win_n,"EDIT_10_8","BACKCOLOR", {255,255,255} )
               setproperty( win_n,"EDIT_10_9","BACKCOLOR", {255,255,255} )


               do case
               case  control_search ="EDIT_10_1"
                    //  setproperty( win_n,"Button_10_1_X","caption","Išèi ID")  // if you have search button you can change text here
                    //  skuparray:SETCOLOR( { 4 },  {MyRGB( { 255, 255, 0 }),  MyRGB(BARVA_OKNO) },  1  )  // if you use tbrowse you can color tbrowse head here
                    setproperty( win_n,"Label_10_1_1","fontcolor", { 255, 255, 0 } )
                    setproperty( win_n,"Label_10_1_1","fontbold", .T. )
                    setproperty( win_n,"EDIT_10_1","BACKCOLOR", {245,255,165} )
               case  control_search ="EDIT_10_2"
                    setproperty( win_n,"Label_10_1_2", "fontcolor", { 255, 255, 0 } )
                    setproperty( win_n,"Label_10_1_2","fontbold", .T. )
                    setproperty( win_n,"EDIT_10_2","BACKCOLOR", {245,255,165} )
               case  control_search ="EDIT_10_3"
                    setproperty( win_n,"Label_10_1_3","fontcolor", { 255, 255, 0 } )
                    setproperty( win_n,"Label_10_1_3","fontbold", .T. )
                    setproperty( win_n,"EDIT_10_3","BACKCOLOR", {245,255,165} )
               case  control_search ="EDIT_10_4"
                    setproperty( win_n,"Label_10_1_4","fontcolor", { 255, 255, 0 } )
                    setproperty( win_n,"Label_10_1_4","fontbold", .T. )
                    setproperty( win_n,"EDIT_10_4","BACKCOLOR", {245,255,165} )
               case  control_search ="EDIT_10_5"
                    setproperty( win_n,"Label_10_1_5","fontcolor", { 255, 255, 0 } )
                    setproperty( win_n,"Label_10_1_5","fontbold", .T. )
                    setproperty( win_n,"EDIT_10_5","BACKCOLOR", {245,255,165} )
               case  control_search ="EDIT_10_6"
                    setproperty( win_n,"EDIT_10_6","BACKCOLOR", {245,255,165} )
               case  control_search ="EDIT_10_7"
                    setproperty( win_n,"EDIT_10_7","BACKCOLOR", {245,255,165} )
               case  control_search ="EDIT_10_8"
                    setproperty( win_n,"EDIT_10_8","BACKCOLOR", {245,255,165} )
               case  control_search ="EDIT_10_9"
                    setproperty( win_n,"EDIT_10_9","BACKCOLOR", {245,255,165} )
               otherwise
                    control_search =""
               endcase
          endif
     ENDIF
     domethod( win_n,"edit_check_focus","release")
     RETURN NIL

/***************************************************************************************************
*****                                                                                              *
*****                                      THE END                                                 *
*****                                                                                              *
***************************************************************************************************/