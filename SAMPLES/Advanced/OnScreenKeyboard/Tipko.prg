/****************************************************************************************************
**  OnScreenKeyboard                                                                                *
** 1989-2018 Kristjan Žagar <kristjan.zagar@me.com>                                                 *
**                                                                                                  *
** Europa Slovenija                                                                                 *
** Edited with PSPad                                                                                *
** P.S. Don't type too fast!                                                                        *
****************************************************************************************************/

#INCLUDE "hmg.ch"


/****************************************************************************************************
**  Virtual keybard Version 1.0                                                                     *
** 1989-2016 Kristjan Žagar <kristjan.zagar@me.com>                                                 *
** Europa Slovenija                                                                                 *
** Edited with PSPad                                                                                *
****************************************************************************************************/
** about parameters -max_y width keyboard form  i use GetDesktopWIDTH()-200
*                   -win_name form name  in witch key are inputed
*                   -control_n name of control first control EDIT_10_1
*                                                     socond EDIT_10_3 .... EDIT_10_5
*                    if u like to use text box or any other control wit text input name it
*                    in count EDIT_10_4 (don't lose NOtabstop)
*                    any edit EDIT_10_1 control must have c_picture_1 .... c_picture_7
*                   -cPicture  name of parameter for picture value
*                   -Farba_edit not implentended jet)
*                   TIMEPICKER -not implemented jet
*****************************************************************************************************
FUNCTION tipkovnica(max_yt,win_n,control_n,cPicture, Farba_edit)
     local full:=0,  width:=0, height:=0  , star_text ,ADT:=0  ,Visina_crk_t[1][69] ,;
          t_nam:="",t_wname:=""

     control_focus:=control_n
     BUTT_KEY  := GetFontHandle( "FONT_KEYB" )
     VIsina_gumba_tipk := 150
     win_name_ed  :=  win_n
     control_name :=  control_n
     SET TYPEAHEAD TO 200
     win_name:="Form_KEY"
     EDIT_NOV:=.T.
     max_x:=GETDESKTOPHEIGHT()
     max_y:=GETDESKTOPWIDTH()
     cPath:=GetStartupFolder()
     rob_med:=12
     v:=visina_gumba_tipk
     Visina_crk_t:={v+31,v-1,  v-1,v-1,v-1,v-1,v-1,v-1,v-1,v-1,v-1,v-1,  v-1,v-1,    v+40  ,;
          v+55,    v,v,v,v,v,v,v,v,v,v,v,v,      v+77  ,;
          v+77,    v,v,v,v,v,v,v,v,v,v,v,v,      v+51  ,;
          v+58,    v,v,v,v,v,v,v,v,v,v,v,  v+11,v-5,v+6,;
          v+31,    v,v, v+276, v+16,v,v+16,v+32,v+6,v-5,v+6}
     kk:=0
     for n:=1 to 15
          kk:=kk+ Visina_crk_t[n]
     next n
     kk:=kk+(rob_med*14)
     full:=rob_tipk + kk + rob_tipk +2
     do while .t.                                   // precalculate width of all keys
          if  full>=max_yt
               for n:=1 to 69
                    Visina_crk_t[n]:= Visina_crk_t[n]/1.002004008016032
               next n
               visina_gumba_tipk:=visina_gumba_tipk/1.002004008016032
               rob_med:=(visina_gumba_tipk    /(100+18))*18
               kk:=0
               for n:=1 to 15
                    kk:=kk+ Visina_crk_t[n]
               next n
               kk:=kk+(rob_med*14)
               full:=rob_tipk + kk +rob_tipk
          else
               exit
          endif
     enddo
     old_vis:=Visina_program
     Visina_program:=0
     crke_font:=(visina_gumba_tipk /(100+25))*25
     full_x:= visina_program+rob_tipk+rob_med+visina_gumba_tipk +rob_med+visina_gumba_tipk+rob_med+visina_gumba_tipk+rob_med+visina_gumba_tipk+visina_gumba_tipk+rob_tipk+1
     at_y:=(max_y/2)-(full/2)
     at_x:=max_x-full_x-rob
     crke_MALE:={ "Esc", '¸', "1", "2", "3", "4", "5", "6", "7", "8", "9", "0", '`', "+", "Bksp" ,;  // upper keys
     "Tab", "q", "w", "e", "r", "t", "z", "u", "i", "o", "p", "š", "ð", ""          ,;
          "Caps","a","s","d","f","g","h","j","k","l","è","æ","ž","",;
          "Shift","<","y","x","c","v","b","n","m",",",".","-","Shift","","Del",;
          "Ctrl","","Alt","Space","AltGr","","Fn","Ctrl","","",""}
     crke_VELIKE:={ "Esc", '¸', "1", "2", "3", "4", "5", "6", "7", "8", "9", "0", '`', "+", "Bksp" ,;  // lower key
     "Tab", "Q", "W", "E", "R", "T", "Z", "U", "I", "O", "P", "Š", "Ð", ""          ,;
          "Caps","A","S","D","F","G","H","J","K","L","È","Æ","Ž","",;
          "Shift","<","Y","X","C","V","B","N","M",",",".","-","Shift","","Del",;
          "Ctrl","","Alt","Space","AltGr","","Fn","Ctrl","","",""}
     crke_shift_MALE:={ "Esc", '¨', "!",'"' , "#", "$", "%", "&&", "/", "(", ")", "=", '?', "*", "Bksp" ,;  // shift + lower keys
     "Tab", "Q", "W", "E", "R", "T", "Z", "U", "I", "O", "P", "Š", "Ð", ""          ,;
          "Caps","A","S","D","F","G","H","J","K","L","È","Æ","Ž","",;
          "Shift",">","Y","X","C","V","B","N","M",";",":","_","Shift","","Del",;
          "Ctrl","","Alt","Space","AltGr","","Fn","Ctrl","","",""}
     crke_shift_VELIKE:={ "Esc", '¨', "!",'"' , "#", "$", "%", "&&", "/", "(", ")", "=", '?', "*", "Bksp" ,;  // shift + upper keys
     "Tab", "q", "w", "e", "r", "t", "z", "u", "i", "o", "p", "š", "ð", ""          ,;
          "Caps","a","s","d","f","g","h","j","k","k","l","è","è","",;
          "Shift",">","y","x","c","v","b","n","m",";",":","_","Shift","","Del",;
          "Ctrl","","Alt","Space","AltGr","","Fn","Ctrl","","",""}
     crke_altgr:={ "Esc", '', "~",'¡' , "^", "¢", "°", "²", "`", "ÿ", "´", "½", '¨', "¸", "Bksp" ,;  // AltGr keys
     "Tab", "\", "|", "€", "", "", "", "", "", "", "", "÷", "×", ""          ,;
          "Caps","","","","[","]","","","³",chr(163),"","ß","¤","",;
          "Shift","","","","","@","{","}","§","<",">","","Shift","","Del",;
          "Ctrl","","Alt","Space","AltGr","","Fn","Ctrl","","",""}
     crke_FN:={ "Esc", '¸', "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", 'F11', "F12", "Bksp" ,;  // fuznction keys
     "Tab", "", "", "", "", "", "", "Home", "End", "PgUp", "", "", "", ""          ,;
          "Caps","","","","","","","","Insert","PgDn","","","","",;
          "Shift","","","","","","","","","","","","Shift","","Del",;
          "Ctrl","","Alt","Space","AltGr","","Fn","Ctrl","","",""}
     crke_cargo_MALE:={}
     AADD(crke_cargo_MALE, {VK_ESCAPE, 192, VK_1, VK_2, VK_3, VK_4, VK_5, VK_6, VK_7, VK_8, VK_9, VK_0, 191, 187, 8,;
          9, VK_Q, VK_W, VK_E, VK_R, VK_T, VK_Z, VK_U, VK_I, VK_O, VK_P, 219, 221, 13,;
          2010, VK_A, VK_S, VK_D, VK_F, VK_G, VK_H, VK_J, VK_K, VK_L, 186, 222,220,  13,;
          2000,226, VK_Y, VK_X, VK_C, VK_V, VK_B, VK_N, VK_M, 188 ,190, 189, 2000, 38, VK_DELETE,;
          2001, VK_LWIN, 2002,       32,     2003, VK_APPS,2011, 2015,37,40,39 })
     crke_cargo_VELIKE:={}
     AADD(crke_cargo_VELIKE, {VK_ESCAPE, 192, VK_1, VK_2, VK_3, VK_4, VK_5, VK_6, VK_7, VK_8, VK_9, VK_0, 191, 187, 8,;
          9, VK_Q, VK_W, VK_E, VK_R, VK_T, VK_Z, VK_U, VK_I, VK_O, VK_P, 219, 221, 13,;
          2010, VK_A, VK_S, VK_D, VK_F, VK_G, VK_H, VK_J, VK_K, VK_L, 186, 222,220,  13,;
          2000,226, VK_Y, VK_X, VK_C, VK_V, VK_B, VK_N, VK_M, 188 ,190, 189, 2000, 38, VK_DELETE,;
          2001, VK_LWIN, 2002,       32,     2003, VK_APPS,2011, 2015,37,40,39 })
     crke_cargo_shift_VELIKE:={}
     AADD(crke_cargo_shift_VELIKE, {VK_ESCAPE, 192, VK_1, VK_2, VK_3, VK_4, VK_5, VK_6, VK_7, VK_8, VK_9, VK_0, 191, 187, 8,;
          9, VK_Q, VK_W, VK_E, VK_R, VK_T, VK_Z, VK_U, VK_I, VK_O, VK_P, 219, 221, 13,;
          2010, VK_A, VK_S, VK_D, VK_F, VK_G, VK_H, VK_J, VK_K, VK_L, 186, 222,220,  13,;
          2000,226, VK_Y, VK_X, VK_C, VK_V, VK_B, VK_N, VK_M, 188 ,190, 189, 2000, 38, VK_DELETE,;
          2001, VK_LWIN, 2002,       32,     2003, VK_APPS,2011, 2015,37,40,39 })
     crke_cargo_shift_male:={}
     AADD(crke_cargo_shift_male, {VK_ESCAPE, 192, VK_1, VK_2, VK_3, VK_4, VK_5, VK_6, VK_7, VK_8, VK_9, VK_0, 191, 187, 8,;
          9, VK_Q, VK_W, VK_E, VK_R, VK_T, VK_Z, VK_U, VK_I, VK_O, VK_P, 219, 221, 13,;
          2010, VK_A, VK_S, VK_D, VK_F, VK_G, VK_H, VK_J, VK_K, VK_L, 186, 222,220,  13,;
          2000,226, VK_Y, VK_X, VK_C, VK_V, VK_B, VK_N, VK_M, 188 ,190, 189, 2000, 38, VK_DELETE,;
          2001, VK_LWIN, 2002,       32,     2003, VK_APPS,255, 2015,37,40,39 })
     crke_cargo_FN:={}
     AADD(crke_cargo_FN, {VK_ESCAPE, 192, VK_F1, VK_F2, VK_F3, VK_F4, VK_F5, VK_F6, VK_F7, VK_F8, VK_F9, VK_F10, VK_F11, VK_F12, 8,;
          9, 0, 0, 0, 0, 0, 0, VK_HOME, VK_END, VK_PRIOR, 0, 219, 221, 13,;
          2010, 0, 0, 0, 0, 0, 0, 0, VK_INSERT, VK_NEXT, 186, 222,220,  13,;
          2000,226, 0, 0, 0, 0, 0, 0, 0, 188 ,190, 189, 2000, 38, VK_DELETE,;
          2001, VK_LWIN, 2002,       32,     2003, VK_APPS,2011, 2015,37,40,39 })
     IF IsCapsLockActive()
          tipka_caps:=.t.
          CRKE:=crke_velike
          crke_cargo:=crke_cargo_VELIKE
     ELSE
          tipka_caps:=.f.
          CRKE:=crke_male
          crke_cargo:=crke_cargo_MALE
     ENDIF
     // shift 2000, ctrl 2001 , alt 2002, altgr2003, winkey, 2004, win secend key 2005,
     DEFINE WINDOW  Form_KEY		;
          AT  GetDesktopHEIGHT ()-GetTaskbarHEIGHT() - FULL_X   , (GetDesktopWIDTH ()- FULL)/2  ;
          WIDTH full;
          HEIGHT full_x;
          CHILD;
          TOPMOST NOCAPTION;
          NOMINIMIZE NOMAXIMIZE nosize;
          BACKCOLOR WHITE;
          ON INIT ( TIMER_FOR_EDIT (win_name_ed,control_name), timer_display_key() );

          @ at_x,at_y LABEL  Label_150 of &Win_name WIDTH Form_KEY.width  HEIGHT Form_KEY.height     BACKCOLOR  WHITE
          Barva_okno:={49,48,50}
          WIDTH:=Form_KEY.WIDTH
          HEIGHT:=Form_KEY.height
          @ 1,1 LABEL  Label_15_1 WIDTH (width-2) HEIGHT (height-Visina_program-2)    BACKCOLOR  Barva_okno  //window
          col_T:=rob_tipk
          row_t:=visina_program+rob_tipk +1
          for n:=1 to 69
               ADt:="BUTT_15_"+ LTRIM(STR(n))
               define BUTTONEX  &ADT
                    row row_t
                    col col_t
                    WIDTH Visina_crk_t[n]
                    HEIGHT visina_gumba_tipk
                    caption crke[n]
                    FONTCOLOR BLACK
                    FONTNAME BUTT_KEY
                    FONTSIZE Crke_font
                    backcolor {gumb_barva[1],gumb_barva[2],gumb_barva[3]}
                    PICTURE RETU_PICT(ADT)
                    noxpstyle .T.
                    NOtabstop .T.
                    VCENTERALIGN .t.
                    CENTERALIGN .t.
                    FLAT .T.
                    BORDER .T.
                    adjust .t.
                    BACKGROUNDCOLOR Barva_okno
                    TRANSPARENT .T.
                    action (t_nam:=this.name,t_wname:=thiswindow.name,_DisableControl ( t_nam, t_wname ), show_temp_form(win_name_ed,control_name), Form_KEY.&(T_NAM).BACKCOLOR:={255,255,255},;
                         ( press_key(this.name,win_name_ed,control_name,cPicture)),;
                         Form_KEY.&(T_NAM).BACKCOLOR:=Zat_Button_(.f.,Form_KEY.&(T_NAM).backcolor,{gumb_barva[1],gumb_barva[2],gumb_barva[3]}),;
                         restore_temp_form(win_name_ed),_EnableControl (t_nam, t_wname ))
                    ON MOUSEHOVER (adt:=this.name,  MOUSE_FOC(win_name,adt) )
                    ON MOUSELEAVE (adt:=this.name, MOUSE_FOC_LEAVE(win_name, adt , {gumb_barva[1],gumb_barva[2],gumb_barva[3]} )  )
               end BUTTONEX
               SETPROPERTY(win_name,adt,"FONTSIZE",gETPROPERTY(win_name,adt,"FONTSIZE"))
               SETPROPERTY(win_name,adt,"CARGO","999")
               do case
               case n=15
                    row_t:=row_t+visina_gumba_tipk+rob_med
                    col_t:=rob_tipk +1
               case n=29
                    row_t:=row_t+visina_gumba_tipk+rob_med
                    col_t:=rob_tipk +1
               case n=43
                    row_t:=row_t+visina_gumba_tipk+rob_med
                    col_t:=rob_tipk +1
               case n=58
                    row_t:=row_t+visina_gumba_tipk+rob_med
                    col_t:=rob_tipk +1
               otherwise
                    col_t:=getproperty(win_name,ADt,"col")+getproperty(win_name,ADt,"width")+rob_med
               end case
               SETPROPERTY(win_name,adt,"backcolor",WHITE)
          next n
          rob_desni:=getproperty(win_name,"BUTT_15_15","col")+getproperty(win_name,"BUTT_15_15","width")
          rob_do:=getproperty(win_name,"BUTT_15_29","col")+getproperty(win_name,"BUTT_15_29","width")
          dolzinca:=rob_desni-rob_do
          if rob_do<rob_desni
               dolzinca:=rob_desni-rob_do
               setproperty(win_name,"BUTT_15_29","width",getproperty(win_name,"BUTT_15_29","width" )   + dolzinca )
          endif
          if rob_do>rob_desni
               dolzinca:=abs(rob_desni-rob_do)
               setproperty(win_name,"BUTT_15_29","width",getproperty(win_name,"BUTT_15_29","width" )   - dolzinca  )
          endif
          rob_do:=getproperty(win_name,"BUTT_15_43","col")+getproperty(win_name,"BUTT_15_43","width")
          dolzinca:=rob_desni-rob_do
          if rob_do<rob_desni
               dolzinca:=rob_desni-rob_do
               setproperty(win_name,"BUTT_15_43","width",getproperty(win_name,"BUTT_15_43","width" )   + dolzinca   )
          endif
          if rob_do>rob_desni
               dolzinca:=abs(rob_desni-rob_do)
               setproperty(win_name,"BUTT_15_43","width",getproperty(win_name,"BUTT_15_43","width" )   - dolzinca    )
          endif
          rob_do:=getproperty(win_name,"BUTT_15_58","col")+getproperty(win_name,"BUTT_15_58","width")
          if rob_do<rob_desni
               dolzinca:=rob_desni-rob_do
               setproperty(win_name,"BUTT_15_56","width",getproperty(win_name,"BUTT_15_56","width" )   + dolzinca     )
               setproperty(win_name,"BUTT_15_57","col",getproperty(win_name,"BUTT_15_57","col" )   + dolzinca    )
               setproperty(win_name,"BUTT_15_58","col",getproperty(win_name,"BUTT_15_58","col" )   + dolzinca   )
          endif
          if rob_do>rob_desni
               dolzinca:=abs(rob_desni-rob_do)
               setproperty(win_name,"BUTT_15_56","width",getproperty(win_name,"BUTT_15_56","width" )   - dolzinca  )
               setproperty(win_name,"BUTT_15_57","col",getproperty(win_name,"BUTT_15_57","col" )   - dolzinca     )
               setproperty(win_name,"BUTT_15_58","col",getproperty(win_name,"BUTT_15_58","col" )   - dolzinca     )
          endif
          rob_do:=getproperty(win_name,"BUTT_15_69","col")+getproperty(win_name,"BUTT_15_69","width")
          if rob_do<rob_desni
               dolzinca:=rob_desni-rob_do
               setproperty(win_name,"BUTT_15_62","width",getproperty(win_name,"BUTT_15_62","width" )   + dolzinca   )
               setproperty(win_name,"BUTT_15_63","col",getproperty(win_name,"BUTT_15_63","col" )   + dolzinca        )
               setproperty(win_name,"BUTT_15_64","col",getproperty(win_name,"BUTT_15_64","col" )   + dolzinca      )
               setproperty(win_name,"BUTT_15_65","col",getproperty(win_name,"BUTT_15_65","col" )   + dolzinca      )
               setproperty(win_name,"BUTT_15_66","col",getproperty(win_name,"BUTT_15_66","col" )   + dolzinca      )
               setproperty(win_name,"BUTT_15_67","col",getproperty(win_name,"BUTT_15_67","col" )   + dolzinca      )
               setproperty(win_name,"BUTT_15_68","col",getproperty(win_name,"BUTT_15_68","col" )   + dolzinca      )
               setproperty(win_name,"BUTT_15_69","col",getproperty(win_name,"BUTT_15_69","col" )   + dolzinca      )
          endif
          if rob_do>rob_desni
               dolzinca:=abs(rob_desni-rob_do)
               setproperty(win_name,"BUTT_15_62","width",getproperty(win_name,"BUTT_15_62","width" )   - dolzinca   )
               setproperty(win_name,"BUTT_15_63","col",getproperty(win_name,"BUTT_15_63","col" )   - dolzinca       )
               setproperty(win_name,"BUTT_15_64","col",getproperty(win_name,"BUTT_15_64","col" )   - dolzinca      )
               setproperty(win_name,"BUTT_15_65","col",getproperty(win_name,"BUTT_15_65","col" )   - dolzinca       )
               setproperty(win_name,"BUTT_15_66","col",getproperty(win_name,"BUTT_15_66","col" )   - dolzinca       )
               setproperty(win_name,"BUTT_15_67","col",getproperty(win_name,"BUTT_15_67","col" )   - dolzinca       )
               setproperty(win_name,"BUTT_15_68","col",getproperty(win_name,"BUTT_15_68","col" )   - dolzinca       )
               setproperty(win_name,"BUTT_15_69","col",getproperty(win_name,"BUTT_15_69","col" )   - dolzinca       )
          endif
          setproperty(win_name,"BUTT_15_43","row",getproperty(win_name,"BUTT_15_43","row" )-(rob_med ))
          setproperty(win_name,"BUTT_15_43","height",getproperty(win_name,"BUTT_15_43","height" )+(rob_med ))
          Visina_program:=old_vis
          TIMER_CHANGE:=.t.
     end window
     SET WINDOW &win_name TRANSPARENT TO 220
     Form_KEY.SHOW
     ACTIVATE WINDOW  Form_KEY
RETURN NIL

function save_temp_form(editwindow,cont_nam)        // becouse i write letters on screen i hide and restore preoperty on screen
     IF DIRCHANGE("\Temp") == -3                    // Path not found
          DIRMAKE("\Temp")
     ENDIF
     DoMethod( editwindow,cont_nam, 'SaveAs', cPath+ + "\Temp\"+"_temp_keyb.bmp" )
return nil

function show_temp_form(editwindow,cont_nam)
     if !_IsControlDefined ('IMAGE_tempx',editwindow)
          @ getproperty(editwindow,cont_nam,"row"),getproperty(editwindow,cont_nam,"col") IMAGE  IMAGE_tempx of &editwindow PICTURE (cPath+ + "\Temp\"+"_temp.bmp");
               WIDTH getproperty(editwindow,cont_nam,"width") HEIGHT getproperty(editwindow,cont_nam,"height")
     endif
return nil

function restore_temp_form(editwindow)
     if _IsControlDefined ('IMAGE_tempx',editwindow)
          DoMethod( editwindow, 'IMAGE_tempx', 'release' )
     endif
     deletefile(cPath+ + "\Temp\"+"_temp.bmp")
return nil

/****************************************************************************************************
** MOUSE_FOC ()               Mouse get focus                                                       *
** 1989-2016 Kristjan Žagar <kristjan.zagar@me.com>                                                 *
** Europa Slovenija                                                                                 *
** Edited with PSPad                                                                                *
****************************************************************************************************/
FUNCTION MOUSE_FOC(WIN_NAME_FO,adt)
     setproperty(WIN_NAME_FO,adt,"Fontbold",.t.)
     do case
     case  IsCapsLockActive()  .and. adt="BUTT_15_30"
     case TIPKA_SHIFT  .and. adt="BUTT_15_44"
     case TIPKA_SHIFT  .and. adt="BUTT_15_56"
     case TIPKA_lcontrol .and. adt="BUTT_15_59"
     case TIPKA_rcontrol .and. adt="BUTT_15_66"
     case TIPKA_alt  .and. adt="BUTT_15_61"
     case TIPKA_altgr  .and. adt="BUTT_15_63"
     case TIPKA_fn  .and. adt="BUTT_15_65"
     otherwise
          setproperty(WIN_NAME_FO,adt,"backcolor",Z_Button_(.t.,Getproperty(WIN_NAME_FO,adt,"backcolor"),{60,60,50}))
          end case
RETURN .T.

/****************************************************************************************************
** MOUSE_FOC_LEAVE ()               Mouse lost focus                                                *
** 1989-2016 Kristjan Žagar <kristjan.zagar@me.com>                                                 *
** Europa Slovenija                                                                                 *
** Edited with PSPad                                                                                *
****************************************************************************************************/

FUNCTION MOUSE_FOC_LEAVE(WIN_NAME_FO,adt,BARvi_CA)
     DEFAULT BARvi_CA TO {0,0,0}
     setproperty(WIN_NAME_FO,adt,"Fontbold",.F.)
     do case
     case  IsCapsLockActive()  .and. adt="BUTT_15_30"
     case TIPKA_SHIFT  .and. adt="BUTT_15_44"
     case TIPKA_SHIFT  .and. adt="BUTT_15_56"
     case TIPKA_lcontrol .and. adt="BUTT_15_59"
     case TIPKA_rcontrol .and. adt="BUTT_15_66"
     case TIPKA_alt  .and. adt="BUTT_15_61"
     case TIPKA_altgr  .and. adt="BUTT_15_63"
     case TIPKA_fn  .and. adt="BUTT_15_65"
     otherwise
          setproperty( WIN_NAME_FO , adt , "backcolor" , Zat_Button_(.f. , Getproperty ( WIN_NAME_FO , adt , "backcolor" ) , BARvi_CA ) )
          end case
RETURN NIL
function press_key( NUMT,Win_Name_ed,control_name_1,cPicture)
     control_name:=control_name_1
     if ValType( getproperty(Win_Name_ed,control_name,"value")) =="N"
          press_key_num( NUMT,Win_Name_ed,control_name,cPicture)
     endif
     if ValType( getproperty(Win_Name_ed,control_name,"value")) =="C"
          press_key_char( NUMT,Win_Name_ed,control_name,cPicture)
     endif
return nil

/****************************************************************************************************
** press_key_num ()          Pres numeric type key                                                  *
** 1989-2016 Kristjan Žagar <kristjan.zagar@me.com>                                                 *
** Europa Slovenija                                                                                 *
** Edited with PSPad                                                                                *
****************************************************************************************************/
function press_key_num( NUMT,Win_Name_ed,control_name_2,cPicture)  //  if picture is numeric
     LOCAL kklloT:=0, IM:=0, NT:=0 , poz_n:=0, nglen:=0 ,VAL_UE,press_time:=0.02
     DEFAULT numT TO ""
     control_name:=control_name_2
     if Button_block>=1
          Button_block++
          press_n_pos_k:= n_pos
          do while .t.
               val_ue:=getproperty(Win_Name_ed,Control_name,"value")
               val_ue_d:=getproperty(Win_Name_ed,Control_name,"displayvalue")
               full_len:=len(substr(cPicture,At( "9", cPicture )))
               TRUE_LEN:=LEN(ALLTRIM(STR(VAL_UE)))
               numt:=chartonum(alltrim(SUBSTR(numT, 9)))
               if  (crke_cargo[1][numt])>=2000
                    DoMethod(Win_Name_ed,control_name,"setfocus")
                    setproperty(Win_Name_ed,control_name,"caretpos",press_n_pos_k)
                    do case
                    case  (crke_cargo[1][numt])=2004  // WINKEY
                         _PushKey  (MOD_WIN)
                         exit
                    case  (crke_cargo[1][numt])=2001  // alt
                         Tipka_lcontrol:=!Tipka_lcontrol
                         TIMER_CHANGE:=.T.
                         exit
                    case  (crke_cargo[1][numt])=2015  // alt
                         Tipka_rcontrol:=!Tipka_rcontrol
                         TIMER_CHANGE:=.T.
                         exit
                    case  (crke_cargo[1][numt])=2002  // control
                         Tipka_Alt:=!Tipka_Alt
                         TIMER_CHANGE:=.T.
                         exit
                    case  (crke_cargo[1][numt])=2011  // control
                         Tipka_FN:=!Tipka_FN
                         TIMER_CHANGE:=.T.
                         exit
                    case  (crke_cargo[1][numt])=2003  // AltGr
                         Tipka_AltGr:=!Tipka_AltGr
                         TIMER_CHANGE:=.T.
                         exit
                    case  (crke_cargo[1][numt])=2000   .or. (crke_cargo[1][numt])=2010  // shift     & capital
                         if (crke_cargo[1][numt])=2000
                              TIPKA_SHIFT:=!tIPKA_SHIFT
                         endif
                         if (crke_cargo[1][numt])=2010
                              _PushKey  (20)
                              SysWaitx( 0.05 )
                              TIPKA_SHIFT := .F.
                         endif
                         TIMER_CHANGE:=.T.
                         exit
                    OTHERWISE
                         EXIT
                    endcase
               endif
               DoMethod(Win_Name_ed,control_name,"setfocus")
               // Enter is pressed
               if  (crke_cargo[1][numt])=13
                    DoMethod(Win_Name_ed,control_name,"setfocus")
                    _PushKey  ((crke_cargo[1][numt]))
                    return nil
               endif
               if ValType( getproperty(Win_Name_ed,control_name,"value")) =="N"
                    DEC_POS:=AT(".",cPicture)
                    if press_n_pos_k<>0
                         IF !DEC_POS=0
                              DEC_CHR:=SUBSTR(CpICTURE,DEC_POS+1)
                              DEC_NUM:=LEN(DEC_CHR)
                              if press_n_pos_k>=full_len-dec_num
                                   if press_n_pos_k=full_len
                                        press_n_pos_k:=press_n_pos_k-1
                                   endif
                                   _PushKey  (VK_DECIMAL)
                                   SysWaitt( press_time )
                                   setproperty(Win_Name_ed,control_name,"caretpos",press_n_pos_k)
                                   SysWaitt( press_time )
                                   _PushKey  ((crke_cargo[1][numt]))
                                   SysWaitt( press_time )
                                   n_pos:= getproperty(Win_Name_ed,control_name,"caretpos")
                                   exit
                              endif
                         endif
                    else
                         IF !DEC_POS=0
                              if numt= 53 .or. numt=54
                                   _PushKey  (VK_DECIMAL)
                                   exit
                              endif
                         endif
                    endif
                    vrednost:=val_ue
                    nglen:=alltrim(str(vrednost))
                    IF EDIT_NOV
                         if  len(nglen) > 0
                              for nn := 1 to len(alltrim(nglen)) +1
                                   _PushKey (   AsciiSum  ( substr(alltrim(nglen),NN-1,1))  )
                              next nn
                         endif
                         SysWaitx( 0.1 )
                         setproperty(Win_Name_ed,control_name,"caretpos",press_n_pos_k)
                         _PushKey  ((crke_cargo[1][numt]))
                    else
                         _PushKey  (VK_RIGHT)
                         SysWaitx( 0.05 )
                         setproperty(Win_Name_ed,control_name,"caretpos",press_n_pos_k)
                         SysWaitx( 0.05 )
                         _PushKey  ((crke_cargo[1][numt]))
                    endif
               endif
               exit
          enddo
          n_pos:= getproperty(Win_Name_ed,control_name,"caretpos")
          syswaitt(0.06)
          save_temp_form(Win_Name_ed,control_name)
          Button_block:=1
     endif
return nil

/****************************************************************************************************
** press_key_char ()          Pres character type key                                               *
** 1989-2016 Kristjan Žagar <kristjan.zagar@me.com>                                                 *
** Europa Slovenija                                                                                 *
** Edited with PSPad                                                                                *
***************************************************************************************************/

function press_key_char( NUMT,Win_Name_ed,control_name_3,cPicture)
     LOCAL kklloT:=0, IM:=0, NT:=0 , poz_n:=0, nglen:=0 ,VAL_UE,press_time:=0.01
     DEFAULT numT TO ""
     control_name:=control_name_3
     if Button_block>=1
          Button_block++
          do while .t.
               press_n_pos_k:= n_pos
               numt:=chartonum(alltrim(SUBSTR(numT, 9)))
               val_ue:=getproperty(Win_Name_ed,Control_name,"value")
               val_ue_d:=getproperty(Win_Name_ed,Control_name,"displayvalue")
               if  (crke_cargo[1][numt])>=2000
                    DoMethod(Win_Name_ed,control_name,"setfocus")
                    setproperty(Win_Name_ed,control_name,"caretpos",press_n_pos_k)
                    do case
                    case  (crke_cargo[1][numt])=2004  // WINKEY
                         _PushKey  (MOD_WIN)
                         exit
                    case  (crke_cargo[1][numt])=2001  // alt
                         Tipka_lcontrol:=!Tipka_lcontrol
                         TIMER_CHANGE:=.T.
                         exit
                    case  (crke_cargo[1][numt])=2015  // alt
                         Tipka_rcontrol:=!Tipka_rcontrol
                         TIMER_CHANGE:=.T.
                         exit
                    case  (crke_cargo[1][numt])=2002  // control
                         Tipka_Alt:=!Tipka_Alt
                         TIMER_CHANGE:=.T.
                         exit
                    case  (crke_cargo[1][numt])=2011  // control
                         Tipka_FN:=!Tipka_FN
                         TIMER_CHANGE:=.T.
                         exit
                    case  (crke_cargo[1][numt])=2003  // AltGr
                         Tipka_AltGr:=!Tipka_AltGr
                         TIMER_CHANGE:=.T.
                         exit
                    case  (crke_cargo[1][numt])=2000   .or. (crke_cargo[1][numt])=2010  // shift     & capital
                         if (crke_cargo[1][numt])=2000
                              TIPKA_SHIFT:=!tIPKA_SHIFT
                         endif
                         if (crke_cargo[1][numt])=2010
                              _PushKey  (20)
                              SysWaitx( 0.05 )
                              TIPKA_SHIFT := .F.
                         endif
                         TIMER_CHANGE:=.T.
                         exit
                    OTHERWISE
                         EXIT
                    endcase
               endif

               /*             if  (crke_cargo[1][numt])=27
               for n:=1 to 10000000000
                    CLEAR TYPEAHEAD
                    a:=inkeygui(0)
                    setproperty(Win_Name_ed,control_name,"value",Alltrim(str(a)) )
                    if a=32
                         exit
                    endif
               next n
               exit
          endif
          */
          if  (crke_cargo[1][numt])=13
               DoMethod(Win_Name_ed,control_name,"setfocus")
               _PushKey  ((crke_cargo[1][numt]))
               //                IF IsWindowDEFINEd("Form_KEY")
               //                    Form_KEY.release  // pazi lahko da se ti tekst okno zapre
               //                    endif
               exit
          endif
          full_len:=len(substr(cPicture,At( "x", cPicture )))
          if full_len=0
               full_len:=len(substr(cPicture,At( "X", cPicture )))
          endif
          TRUE_LEN:=LEN(ALLTRIM(VAL_UE))
          nglen:=alltrim(val_ue)
          DoMethod(Win_Name_ed,control_name,"setfocus")
          setproperty(Win_Name_ed,control_name,"caretpos",press_n_pos_k)
          if tipka_shift = .t.
               keybd_event( VK_SHIFT  ,0,0,0)       // Shift
          ENDIF
          if Tipka_AltGr = .t.
               keybd_event( VK_RMENU  ,0,0,0)       //AltGr
          ENDIF
          if Tipka_lControl = .t.
               keybd_event( VK_LCONTROL  ,0,0,0)    //AltGr
          ENDIF
          if Tipka_rControl = .t.
               keybd_event( VK_RCONTROL  ,0,0,0)    //AltGr
          ENDIF
          if Tipka_Alt = .t.
               keybd_event( VK_LMENU  ,0,0,0)       //AltGr
          ENDIF
          SysWaitx( 0.01 )
          setproperty(Win_Name_ed,control_name,"caretpos",press_n_pos_k)
          _PushKey  ((crke_cargo[1][numt]))
          if tipka_shift = .t.
               _PushKey  (VK_SHIFT)                 //Shift off
               tipka_shift:=.f.
               TIMER_CHANGE:=.t.
          ENDIF
          if Tipka_AltGr = .t.
               _PushKey( VK_RMENU )                 //AltGr       - off
               Tipka_AltGr:=.f.
               old_altgr:=.f.
               TIMER_CHANGE:=.t.
          ENDIF
          if Tipka_lControl = .t.
               _PushKey( VK_LCONTROL)               //AltGr
               Tipka_lControl:=.f.
               old_lcontrol:=.f.
               TIMER_CHANGE:=.t.
          ENDIF
          if Tipka_rControl = .t.
               _PushKey( VK_RCONTROL)               //AltGr
               Tipka_RControl:=.f.
               old_rcontrol:=.f.
               TIMER_CHANGE:=.t.
          ENDIF
          if Tipka_Alt = .t.
               _PushKey( VK_LMENU)                  //AltGr
               Tipka_Alt:=.f.
               old_alt:=.f.
               TIMER_CHANGE:=.t.
          ENDIF
          IF TIPKA_FN
               TIPKA_FN:=.F.
               TIMER_CHANGE:=.t.
          ENDIF
          exit
     enddo
     n_POS:= getproperty(Win_Name_ed,control_name,"caretpos")
     syswaitt(0.06)
     save_temp_form(Win_Name_ed,control_name)
     Button_block:=1
endif
return nil


/****************************************************************************************************
** Timer_display_key ()       timer  for locking keys like caps lock ctrl altgr...                  *
** 1989-2016 Kristjan Žagar <kristjan.zagar@me.com>                                                 *
** Europa Slovenija                                                                                 *
** Edited with PSPad                                                                                *
****************************************************************************************************/
function timer_display_key()
     if !_IsControlDefined ("Timer_display_keybd","Form_KEY")
          DEFINE TIMER Timer_display_keybd of Form_KEY;
               INTERVAL 150;
               ACTION Timer_display_keybd_action()
     endif
RETURN NIL

/****************************************************************************************************
** Timer_display_keybd_action ()       timer action for locking keys like caps lock ctrl altgr...   *
** 1989-2016 Kristjan Žagar <kristjan.zagar@me.com>                                                 *
** Europa Slovenija                                                                                 *
** Edited with PSPad                                                                                *
****************************************************************************************************/

function Timer_display_keybd_action()
     if    !OLD_CAPS = IsCapsLockActive()
          tipka_caps:= IsCapsLockActive()
          tipka_shift:=.f.
          TIMER_CHANGE:=.T.
     ENDIF
     if CheckBit( GetKeyState( VK_SHIFT ) , 32768 ) = .t.
          tipka_shift:=.t.
          TIMER_CHANGE:=.T.
     ENDIF
     if !OLD_RCONTROL = CheckBit( GetKeyState( VK_RCONTROL ) , 32768 )
          tipka_rcontrol:=CheckBit( GetKeyState( VK_RCONTROL ) , 32768 )
          TIMER_CHANGE:=.T.
     ENDIF
     if !OLD_LCONTROL = CheckBit( GetKeyState( VK_LCONTROL ) , 32768 )
          tipka_lcontrol:=CheckBit( GetKeyState( VK_LCONTROL ) , 32768 )

          TIMER_CHANGE:=.T.
     ENDIF
     if !OLD_ALT = CheckBit( GetKeyState( VK_LMENU ) , 32768 )
          tipka_alt:= CheckBit( GetKeyState( VK_LMENU ) , 32768 )
          TIMER_CHANGE:=.T.
     ENDIF
     if !OLD_ALTGR = CheckBit( GetKeyState( VK_RMENU ) , 32768 )
          tipka_altgr:=CheckBit( GetKeyState( VK_RMENU ) , 32768 )
          TIMER_CHANGE:=.T.
     ENDIF
     IF TIMER_CHANGE
          IF IsCapsLockActive()
               if TIPKA_SHIFT
                    CRKE:=crke_shift_velike
                    crke_cargo:=crke_cargo_shift_VELIKE
                    setproperty("Form_KEY","BUTT_15_44","backcolor",{183,183,183})
                    setproperty("Form_KEY","BUTT_15_56","backcolor",{183,183,183})
               else
                    CRKE:=crke_velike
                    crke_cargo:=crke_cargo_VELIKE
                    setproperty("Form_KEY","BUTT_15_44","backcolor",{gumb_barva[1],gumb_barva[2],gumb_barva[3]})
                    setproperty("Form_KEY","BUTT_15_56","backcolor",{gumb_barva[1],gumb_barva[2],gumb_barva[3]})
               endif
               setproperty("Form_KEY","BUTT_15_30","backcolor",{183,183,183})
          ELSE
               if TIPKA_SHIFT
                    CRKE:=crke_shift_male
                    crke_cargo:=crke_cargo_shift_male
                    setproperty("Form_KEY","BUTT_15_44","backcolor",{183,183,183})
                    setproperty("Form_KEY","BUTT_15_56","backcolor",{183,183,183})
               else
                    CRKE:=crke_male
                    crke_cargo:=crke_cargo_MALE
                    setproperty("Form_KEY","BUTT_15_44","backcolor",{gumb_barva[1],gumb_barva[2],gumb_barva[3]})
                    setproperty("Form_KEY","BUTT_15_56","backcolor",{gumb_barva[1],gumb_barva[2],gumb_barva[3]})
               endif
               setproperty("Form_KEY","BUTT_15_30","backcolor",{gumb_barva[1],gumb_barva[2],gumb_barva[3]})
          ENDIF
          if tipka_lcontrol .and. tipka_alt
               CRKE:=crke_altgr
          endif
          if tipka_rcontrol .and. tipka_alt
               CRKE:=crke_altgr
          endif
          if tipka_altgr = .t.
               CRKE:=crke_altgr
          endif
          if tipka_lcontrol
               setproperty("Form_KEY","BUTT_15_59","backcolor",{183,183,183})
          else
               setproperty("Form_KEY","BUTT_15_59","backcolor",{gumb_barva[1],gumb_barva[2],gumb_barva[3]})
          endif
          if tipka_alt
               setproperty("Form_KEY","BUTT_15_61","backcolor",{183,183,183})
          else
               setproperty("Form_KEY","BUTT_15_61","backcolor",{gumb_barva[1],gumb_barva[2],gumb_barva[3]})
          endif
          if tipka_altgr
               setproperty("Form_KEY","BUTT_15_63","backcolor",{183,183,183})
          else
               setproperty("Form_KEY","BUTT_15_63","backcolor",{gumb_barva[1],gumb_barva[2],gumb_barva[3]})
          endif
          if tipka_rcontrol
               setproperty("Form_KEY","BUTT_15_66","backcolor",{183,183,183})
          else
               setproperty("Form_KEY","BUTT_15_66","backcolor",{gumb_barva[1],gumb_barva[2],gumb_barva[3]})
          endif
          IF TIPKA_FN
               CRKE:=CRKE_FN
               CRKE_CARGO:=CRKE_CARGO_FN
               setproperty("Form_KEY","BUTT_15_65","backcolor",{183,183,183})
          else
               setproperty("Form_KEY","BUTT_15_65","backcolor",{gumb_barva[1],gumb_barva[2],gumb_barva[3]})
          ENDIF
          FOR N:=1 TO 69
               ADtX:="BUTT_15_"+ LTRIM(STR(n))
               SETPROPERTY("Form_KEY",adtX,"CAPTION",CRKE[N])
               SETPROPERTY("Form_KEY",adtX,"FONTSIZE",gETPROPERTY("Form_KEY",adtX,"FONTSIZE"))
          NEXT N
     ENDIF
     OLD_CAPS :=tipka_caps
     TIMER_CHANGE:=.F.
     clear typeahead
RETURN NIL

/****************************************************************************************************
** TIMER_FOR_EDIT ()      Timer for finding clear position of caretpos                              *
** 1989-2016 Kristjan Žagar <kristjan.zagar@me.com>                                                 *
** Europa Slovenija                                                                                 *
** Edited with PSPad                                                                                *
****************************************************************************************************/

FUNCTION TIMER_FOR_EDIT (win_name_ed_x,control_name_5)
     control_name:=control_name_5
     if !_IsControlDefined ("Timer_edit","Form_KEY")
          DEFINE TIMER Timer_Edit of Form_KEY;
               INTERVAL 10;
               ACTION TIMER_FOR_EDIT_ACTION(win_name_ed_x,control_name)
     endif
RETURN NIL

/****************************************************************************************************
** TIMER_FOR_EDIT_ACTION ()        Timer for finding caretpos position action                       *
** 1989-2016 Kristjan Žagar <kristjan.zagar@me.com>                                                 *
** Europa Slovenija                                                                                 *
** Edited with PSPad                                                                                *
****************************************************************************************************/

FUNCTION TIMER_FOR_EDIT_ACTION(win_name_ed_1,control_name_6)
     local mk, on_pos:=0, ol_value:="" ,ctrl_name:=""
     ctrl_name:=control_search                      //getproperty(win_name_ed_1,"FocusedControl")
     control_name:=control_name_6
     if  !control_name = ctrl_name
          get_clear_focus(win_name_ed_1,ctrl_name)
     endif
     if !control_name = control_focus
          control_name := control_focus
     endif
     on_pos:= getproperty(Win_Name_ed_1,control_name,"caretpos")
     if !on_pos=n_pos .or. edit_nov=.f.
          if on_pos=0
               GET_clear(on_POS,win_name_ed_1,control_name)
          ELSE
               GET_clear_POZ(on_POS,win_name_ed_1,control_name)
          ENDIF
     endif
RETURN NIL

/****************************************************************************************************
** GET_clear ()       Get clear focus                                                               *
** 1989-2016 Kristjan Žagar <kristjan.zagar@me.com>                                                 *
** Europa Slovenija                                                                                 *
** Edited with PSPad                                                                                *
****************************************************************************************************/

function get_clear_focus(win_name_ed_1,ctrl_named)
     default ctrl_named to ""
     if  !_IsControlDefined ("edit_check_focus","Form_KEY")
          define timer edit_check_focus of Form_KEY;
               interval 200;
               action GET_CLEAR_ACTION_focus(win_name_ed_1,ctrl_named)
     endif
return nil
/****************************************************************************************************
** GET_CLEAR_ACTION ()        Get clear position action of 0                                        *
** 1989-2016 Kristjan Žagar <kristjan.zagar@me.com>                                                 *
** Europa Slovenija                                                                                 *
** Edited with PSPad                                                                                *
****************************************************************************************************/

FUNCTION GET_CLEAR_ACTION_focus(win_name_ed_1,ctrl_named)
     local nev_ctrl:=""
     default ctrl_named to ""
     default win_name_ed_1 to ""
     nev_ctrl:=control_search                       //getproperty(win_name_ed_1,"FocusedControl")
     IF  ctrl_named=nev_ctrl
          control_focus:=nev_ctrl
     ENDIF
     Form_KEY.edit_check_focus.release
RETURN NIL

/****************************************************************************************************
** GET_clear ()       Get clear position of 0                                                       *
** 1989-2016 Kristjan Žagar <kristjan.zagar@me.com>                                                 *
** Europa Slovenija                                                                                 *
** Edited with PSPad                                                                                *
****************************************************************************************************/

function GET_clear(olX_pos,win_name_ed,control_name_7)
     default OLX_POS to 0
     control_name:=control_name_7
     if !_IsControlDefined ("edit_check","Form_KEY")
          define timer edit_check of Form_KEY;
               interval 200;
               action GET_CLEAR_ACTION(OLX_POS,win_name_ed,control_name)
     endif
return nil

/****************************************************************************************************
** GET_CLEAR_ACTION ()        Get clear position action of 0                                        *
** 1989-2016 Kristjan Žagar <kristjan.zagar@me.com>                                                 *
** Europa Slovenija                                                                                 *
** Edited with PSPad                                                                                *
****************************************************************************************************/

FUNCTION GET_CLEAR_ACTION(OLQ_POS,win_name_ed,control_name_8)
     local x:=0,x1:=0,y:=0,y1:=0 ,aCoords[2], k_posi:=0 ,ol_value:=""
     default OLQ_POS to 0
     control_name :=control_name_8
     x :=  getproperty(Win_Name_ed,control_name,"row")
     y :=  getproperty(Win_Name_ed,control_name,"col")
     x1:=  getproperty(Win_Name_ed,control_name,"row") +   getproperty(Win_Name_ed,control_name,"height")
     y1:=  getproperty(Win_Name_ed,control_name,"col") +   getproperty(Win_Name_ed,control_name,"width")
     aCoords[1]:=0
     aCoords[2]:=0
     k_posi:=0
     k_posi:= getproperty(Win_Name_ed,control_name,"caretpos")
     ol_value:=substr( getproperty(Win_Name_ed,control_name,"VALUE"),1,1)  //substr(getproperty(Win_Name_ed,control_name,"DisplayValue"),1,1)
     IF  olQ_pos=k_posi
          N_POS:=K_POSI
          aCoords := GetCursorPos()
          aCoords[1]:=aCoords[1]-   getproperty(Win_Name_ed,"row")
          aCoords[2]:=aCoords[2]-getproperty(Win_Name_ed,"col")
          if aCoords[1]>x .and. aCoords[1]<x1 .and. aCoords[2]>y .and. aCoords[2]<y1
               if ( k_posi = 0 .and. ol_value <> " " ) .or. ( alltrim(ol_value)="" .and. k_posi=0)
                    eDIT_NOV:=.T.
               endif
          ENDIF
     ENDIF
     Form_KEY.edit_check.release
RETURN NIL

/****************************************************************************************************
** GET_clear_POZ ()         Get clear position           biger than 0                               *
** 1989-2016 Kristjan Žagar <kristjan.zagar@me.com>                                                 *
** Europa Slovenija                                                                                 *
** Edited with PSPad                                                                                *
****************************************************************************************************/
function GET_clear_POZ(olD_pos,win_name_ed,control_name_10)
     default OLD_POS to 0
     control_name:=control_name_10
     if !_IsControlDefined ("edit_check2","Form_KEY")
          define timer edit_check2 of Form_KEY;
               interval 200;
               action  GET_clear_POZ_ACTION(OLD_POS,win_name_ed,control_name)
     endif
return nil

/****************************************************************************************************
** GET_clear_POZ_ACTION ()      Get clear position Action                                           *
** 1989-2016 Kristjan Žagar <kristjan.zagar@me.com>                                                 *
** Europa Slovenija                                                                                 *
** Edited with PSPad                                                                                *
****************************************************************************************************/
FUNCTION GET_clear_POZ_ACTION(OLDI_POS,win_name_ed,control_name_11)
     LOCAL XN_POS:=0, OL_VALUE:=""
     default OLDI_POS to 0
     control_name:=control_name_11
     xn_POS:=  getproperty(Win_Name_ed,control_name,"caretpos")
     IF xN_POS=olDI_pos
          n_pos:=xn_pos
          ol_value:= substr( getproperty(Win_Name_ed,control_name,"VALUE"),1,1)  //   substr( getproperty(Win_Name_ed,control_name,,"DISPLAYVALUE"),1,1)
          IF OL_VALUE=" " .AND. XN_POS>0
               EDIT_NOV:=.F.
          ENDIF
     ENDIF
     Form_KEY.edit_check2.release
RETURN NIL

/****************************************************************************************************
** Zat_Button_ ()        Dim button                                                                 *
** 1989-2016 Kristjan Žagar <kristjan.zagar@me.com>                                                 *
** Europa Slovenija                                                                                 *
** Edited with PSPad                                                                                *
****************************************************************************************************/
Function Zat_Button_(zat,v,numx)
     DEFAULT v TO {}
     DEFAULT zat TO .t.
     DEFAULT numx  TO {200,200,200}
     IF v[1] == nil
          RETURN nil
     ENDIF
     if zat=.t.
          v[1]:=255
          v[2]:=255
          v[3]:=255
     else
          v:=NUMX
     endif
return v

/****************************************************************************************************
** Z_Button_ ()        Un         Dim button                                                        *
** 1989-2016 Kristjan Žagar <kristjan.zagar@me.com>                                                 *
** Europa Slovenija                                                                                 *
** Edited with PSPad                                                                                *
****************************************************************************************************/
Function Z_Button_(zat,v,num)
     IF v[1] == nil
          RETURN nil
     ENDIF
     if zat=.t.
          v:=num
     endif
return v

FUNCTION RETU_PICT(BUTT_NAME)
     LOCAL PIC_NAME:=""
     BUTT_NAME:=ALLTRIM(BUTT_NAME)
     DO CASE
     CASE BUTT_NAME=="BUTT_15_60"
          PIC_NAME:="W10"
     CASE BUTT_NAME=="BUTT_15_69"
          PIC_NAME:="DESNO_K"
     CASE BUTT_NAME=="BUTT_15_67"
          PIC_NAME:="LEVO_K"
     CASE BUTT_NAME=="BUTT_15_68"
          PIC_NAME:="DOL_K"
     CASE BUTT_NAME=="BUTT_15_57"
          PIC_NAME:="GOR_K"
     CASE BUTT_NAME=="BUTT_15_16"
          PIC_NAME:="TAB_K"
     CASE BUTT_NAME=="BUTT_15_15"
          PIC_NAME:="BACKSPACE_K"
     CASE BUTT_NAME=="BUTT_15_29"
          PIC_NAME:="RETURN_K"
     OTHERWISE
          PIC_NAME:=""
     ENDCASE
RETURN (PIC_NAME)

FUNCTION SysWaitX( nWait )
     LOCAL iTime := SECONDS()
     DEFAULT nWait TO 2
     DO WHILE SECONDS() - iTime < nWait
          INKEY(0.01)
          DO EVENTS
     ENDDO
RETURN NIL
FUNCTION SysWaitt( nWait )
     Local iTime := Seconds()
     DEFAULT nWait TO 2
     Do While Seconds() - iTime < nWait
          BT_DELAY_EXECUTION (10)
          DO EVENTS
     EndDo
     RETURN nil