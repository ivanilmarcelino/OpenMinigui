/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
*/
#define  _HMG_OUTLOG

#include "hmg.ch"
#include "tsbrowse.ch"
#include "dbinfo.ch"

REQUEST DBFCDX

FUNCTION Main()
   LOCAL cForm := "wMain"
   LOCAL o, owc, oTsb, oBrw, nW, nH

   oTsb := oTsb_Def():Clone()

   nW := oTsb:nWndWidth
   nH := Sys.ClientHeight*0.7

   DEFINE WINDOW &cForm TITLE "Colors TBrowse. DEMO2. " ;
          AT 0,0 WIDTH nW HEIGHT nH                     ;
          MAIN NOSIZE TOPMOST                           ;
          ON INIT    ( This.Topmost := .F., _wPost(0) ) ;
          ON RELEASE ( This.Hide, _wSend(90) )
          This.Cargo := oHmgData() ; o := This.Object ; owc := o:Cargo

      oBrw := _TBrowse(oTsb)

      ON KEY F1     ACTION NIL
      ON KEY ESCAPE ACTION ThisWindow.Release

      o:Event( 0, {|  | 
                    Local oo := App.Object:Cargo:oALL
                    //_o2log(oo,, ">>>", .T.) ; ?
                    //? oo:Values() ; ?v oo:Values() ; ?
                    Return Nil
                    })
      o:Event(99, {|ow| ow:Release() })
      
   END WINDOW

     CENTER WINDOW &cForm
   ACTIVATE WINDOW &cForm

RETURN NIL

FUNCTION oTsb_Def(oTsb)
   LOCAL a, nW := 0, nH := 0, nC := 0

   Default oTsb := oHmgData()
   oTsb:aFont      := { "Normal", "Bold", "Bold", "Italic", "Bold" }
   oTsb:lZebra     := .T.
   oTsb:aFoot      := .T.
   oTsb:uSelector  := 20
   oTsb:aNumber    := { 1, App.Object:W(0.5) } 
   oTsb:aHead      := {"Name", "CLR", "RGB", "HTML"}
   oTsb:aName      := {"NAME", "CLR", "RGB", "HTML"}
   oTsb:aAlign     := {   0  ,   1  ,   1  ,   1  } 
   oTsb:aSizeLen   := {  15  ,  10  ,  10  ,  10  } 
   oTsb:nHeightRow := 10
   oTsb:aFoot      := .T.
   oTsb:lSuperHd   := .T.
   oTsb:cSuperHd   := " "
   //
   oTsb:uAlias   := {}               // HEX = upper(HMG_ClrToHTML(a[2]))
   FOR EACH a IN App.Object:Cargo:oALL:Values()
       AAdd(oTsb:uAlias, {a[1], a[2], hb_valtoexp(a[3]), a[4]})
   NEXT
   //
   oTsb:bChange    := {|ob| ob:cTextSupHdSet(1, " "), ob:DrawSuper() }
   oTsb:bLDblClick := .T.
   oTsb:aUserKeys  := {}
   AAdd(oTsb:aUserKeys, {VK_RETURN, {|ob|  
                                     Local oc := ob:aColumns[ob:nCell]
                                     Local a, c, s
                                     s := alltrim(ob:GetValue("NAME"))+" "
                                     IF oc:cName == "NAME"
                                        c := hb_ntos(ob:GetValue("CLR"))+" "
                                        c += ob:GetValue("RGB")+" "
                                        c += ob:GetValue("HTML")+" "
                                     ELSEIF oc:cName == "CLR"
                                        c := hb_ntos(ob:GetValue("CLR"))
                                     ELSE
                                        c := ob:GetValue(oc)
                                     ENDIF
                                     c += space(3)+"// "+s+" "
                                     Sys.Clipboard := c
                                     ob:cTextSupHdSet(1, "Clipboard <=  "+c)
                                     ob:DrawSuper()
                                     Return Nil
                                     }})
   //
   oTsb:nHeightHead  := App.Object:H(1.1)
   oTsb:nHeightCell  := App.Object:H(1.1)
   oTsb:nHeightFoot  := App.Object:H(1.1)
   oTsb:nHeightSuper := App.Object:H(1.2)
   Default oTsb:nHeightSpecHd := oTsb:nHeightCell
   //
   nC := oTsb:nHeightRow
   nH := oTsb:nHeightRow * oTsb:nHeightCell
   IF !Empty(oTsb:lSuperHd) ; nH += oTsb:nHeightSuper  ; nC += 2
   ENDIF
   IF !Empty(oTsb:aFoot)    ; nH += oTsb:nHeightFoot   ; nC += 1
   ENDIF
   IF !Empty(oTsb:lSpecHd)  ; nH += oTsb:nHeightSpecHd ; nC += 1
   ENDIF
   nH += oTsb:nHeightCell * 3 ; nC += 3
   //
   nW := 2 ; AEval(oTsb:aSizeLen, {|ln| nW += ln }) ; nW += int( nW * 0.3 )
   oTsb:nWndWidth  := GetFontWidth ("Normal", nW)
   oTsb:nWndHeight := oTsb:nHeightCell * nC
   //
   oTsb:b_Init_Def := {|ob|
                       Local i, oc
                       ob:Cargo:nPos_CLR := ob:nColumn("CLR")
                       IF ob:nColumn("SELECTOR", .T.) > 0
                          ob:Cargo:nPos_CLR -= 1
                       ENDIF
                       IF ob:nColumn("ARRAYNO", .T.) > 0
                          ob:Cargo:nPos_CLR -= 1
                       ENDIF
                       FOR i := ( ob:nColumn("NAME") + 1 ) TO Len(ob:aColumns)
                           oc := ob:aColumns[ i ]
                           oc:nClrBack := {|na,nc,obr|
                                           Local np := obr:Cargo:nPos_CLR
                                           nc := obr:aArray[na][np]
                                           Return nc
                                           }
                       NEXT
                       Return Nil
                       }
   //
RETURN oTsb

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
   //
   _SetGetLogFile( cLog ) ; hb_FileDelete( cLog ) ; SET LOGERROR ON
   //
   IF     Sys.DesktopWidth >= 1920 ; nSize += 4 
   ELSEIF Sys.DesktopWidth >  1280 ; nSize += 2 
   ENDIF
   //
   SET FONT TO cFont, nSize

   _DefineFont("Normal"  , cFont, nSize  , .F., .F. )
   _DefineFont("Bold"    , cFont, nSize  , .T., .F. )
   _DefineFont("Italic"  , cFont, nSize-4, .F., .T. )
   // Alert* font
   _DefineFont("DlgFont" , cFont, nSize+2, .F., .F. )
   //
   ALTD(iif( lDebug, 1, 0 ))       // 1 - debug mode, 0 - no debug mode
   //
   o := my_Colors3()

   App.Object:Cargo := oHmgData()
   App.Object:Cargo:oCLR := o:oCLR
   App.Object:Cargo:oRGB := o:oRGB
   App.Object:Cargo:oALL := o:oALL

RETURN 

*----------------------------------------------------------------------------*
FUNCTION ButtonBar(aBtn, cPref, nY, nX, nW, nH, nG, l99)
*----------------------------------------------------------------------------*
   LOCAL cTxt, nLen, nBtn, cBtn, nMsg, aNam := {}, cTool, cCapt, nButt := 0

   IF IsNumeric(cPref)      // начало, база для номерации button
      nButt := cPref
      cPref := NIL
   ENDIF

   Default cPref := "Btn_", l99 := .T.

   Default nG := App.Object:nMargWidth, aBtn := {}
   Default nY := nG, nX := nG , ;
           nW := App.Object:W1, ;
           nH := App.Object:H2

   nBtn := 0
   FOR EACH cTxt IN aBtn
       nLen := hb_enumindex(cTxt)
       IF Empty(cTxt) ; nX += nW + nG ; LOOP
       ENDIF
       nBtn := ++nBtn + nButt
       cBtn := cPref  + hb_ntos( nBtn )
       nMsg := nBtn 
       IF IsArray(cTxt) ; cTool := cTxt[2] ; cCapt := cTxt[1]
       ELSE             ; cTool := NIL     ; cCapt := cTxt
       ENDIF
       @ nY, nX BUTTONEX &cBtn WIDTH nW HEIGHT nH CAPTION cCapt ;
                TOOLTIP  cTool                                  ;
                NOHOTLIGHT NOXPSTYLE HANDCURSOR NOTABSTOP       ;
                ACTION ( This.Enabled := .F., _wPost(This.Cargo,, This.Name) )
       AAdd(aNam, cBtn)
       nX += This.&(cBtn).Width + nG
       This.&(cBtn).Cargo   := iif( l99 .and. nLen == Len(aBtn), 99, nMsg )
       This.Cargo:cBtn_Exit := cBtn
   NEXT

RETURN aNam

FUNCTION my_Colors3()
   LOCAL a, b, i, m, o := oHmgData()

   m := {}
   a := {"Red_tones", {}}
   AAdd(a[2], {"IndianRed", 6053069, {205, 92, 92}, "#CD5C5C"})
   AAdd(a[2], {"LightCoral", 8421616, {240, 128, 128}, "#F08080"})
   AAdd(a[2], {"Salmon", 7504122, {250, 128, 114}, "#FA8072"})
   AAdd(a[2], {"DarkSalmon", 8034025, {233, 150, 122}, "#E9967A"})
   AAdd(a[2], {"LightSalmon", 8036607, {255, 160, 122}, "#FFA07A"})
   AAdd(a[2], {"Crimson", 3937500, {220, 20, 60}, "#DC143C"})
   AAdd(a[2], {"Red", 255, {255, 0, 0}, "#FF0000"})
   AAdd(a[2], {"FireBrick", 2237106, {178, 34, 34}, "#B22222"})
   AAdd(a[2], {"DarkRed", 139, {139, 0, 0}, "#8B0000"})
   AAdd(m, a)

   a := {"Pink_tones", {}}
   AAdd(a[2], {"Pink", 13353215, {255, 192, 203}, "#FFC0CB"})
   AAdd(a[2], {"LightPink", 12695295, {255, 182, 193}, "#FFB6C1"})
   AAdd(a[2], {"HotPink", 11823615, {255, 105, 180}, "#FF69B4"})
   AAdd(a[2], {"DeepPink", 9639167, {255, 20, 147}, "#FF1493"})
   AAdd(a[2], {"MediumVioletRed", 8721863, {199, 21, 133}, "#C71585"})
   AAdd(a[2], {"PaleVioletRed", 9662683, {219, 112, 147}, "#DB7093"})
   AAdd(m, a)

   a := {"Orange_tones", {}}
   AAdd(a[2], {"LightSalmon", 8036607, {255, 160, 122}, "#FFA07A"})
   AAdd(a[2], {"Coral", 5275647, {255, 127, 80}, "#FF7F50"})
   AAdd(a[2], {"Tomato", 4678655, {255, 99, 71}, "#FF6347"})
   AAdd(a[2], {"OrangeRed", 17919, {255, 69, 0}, "#FF4500"})
   AAdd(a[2], {"DarkOrange", 36095, {255, 140, 0}, "#FF8C00"})
   AAdd(a[2], {"Orange", 42495, {255, 165, 0}, "#FFA500"})
   AAdd(m, a)

   a := {"Yellow_tones", {}}
   AAdd(a[2], {"Gold", 55295, {255, 215, 0}, "#FFD700"})
   AAdd(a[2], {"Yellow", 65535, {255, 255, 0}, "#FFFF00"})
   AAdd(a[2], {"LightYellow", 14745599, {255, 255, 224}, "#FFFFE0"})
   AAdd(a[2], {"LemonChiffon", 13499135, {255, 250, 205}, "#FFFACD"})
   AAdd(a[2], {"LightGoldenrodYellow", 13826810, {250, 250, 210}, "#FAFAD2"})
   AAdd(a[2], {"PapayaWhip", 14020607, {255, 239, 213}, "#FFEFD5"})
   AAdd(a[2], {"Moccasin", 11920639, {255, 228, 181}, "#FFE4B5"})
   AAdd(a[2], {"PeachPuff", 12180223, {255, 218, 185}, "#FFDAB9"})
   AAdd(a[2], {"PaleGoldenrod", 11200750, {238, 232, 170}, "#EEE8AA"})
   AAdd(a[2], {"Khaki", 9234160, {240, 230, 140}, "#F0E68C"})
   AAdd(a[2], {"DarkKhaki", 7059389, {189, 183, 107}, "#BDB76B"})
   AAdd(m, a)

   a := {"Purple_tones", {}}
   AAdd(a[2], {"Lavender", 16443110, {230, 230, 250}, "#E6E6FA"})
   AAdd(a[2], {"Thistle", 14204888, {216, 191, 216}, "#D8BFD8"})
   AAdd(a[2], {"Plum", 14524637, {221, 160, 221}, "#DDA0DD"})
   AAdd(a[2], {"Violet", 15631086, {238, 130, 238}, "#EE82EE"})
   AAdd(a[2], {"Orchid", 14053594, {218, 112, 214}, "#DA70D6"})
   AAdd(a[2], {"Fuchsia", 16711935, {255, 0, 255}, "#FF00FF"})
   AAdd(a[2], {"Magenta", 16711935, {255, 0, 255}, "#FF00FF"})
   AAdd(a[2], {"MediumOrchid", 13850042, {186, 85, 211}, "#BA55D3"})
   AAdd(a[2], {"MediumPurple", 14381203, {147, 112, 219}, "#9370DB"})
   AAdd(a[2], {"BlueViolet", 14822282, {138, 43, 226}, "#8A2BE2"})
   AAdd(a[2], {"DarkViolet", 13828244, {148, 0, 211}, "#9400D3"})
   AAdd(a[2], {"DarkOrchid", 13382297, {153, 50, 204}, "#9932CC"})
   AAdd(a[2], {"DarkMagenta", 9109643, {139, 0, 139}, "#8B008B"})
   AAdd(a[2], {"Purple", 8388736, {128, 0, 128}, "#800080"})
   AAdd(a[2], {"Indigo", 8519755, {75, 0, 130}, "#4B0082"})
   AAdd(a[2], {"SlateBlue", 13458026, {106, 90, 205}, "#6A5ACD"})
   AAdd(a[2], {"DarkSlateBlue", 9125192, {72, 61, 139}, "#483D8B"})
   AAdd(m, a)

   a := {"Brown_tones", {}}
   AAdd(a[2], {"Cornsilk", 14481663, {255, 248, 220}, "#FFF8DC"})
   AAdd(a[2], {"BlanchedAlmond", 13495295, {255, 235, 205}, "#FFEBCD"})
   AAdd(a[2], {"Bisque", 12903679, {255, 228, 196}, "#FFE4C4"})
   AAdd(a[2], {"NavajoWhite", 11394815, {255, 222, 173}, "#FFDEAD"})
   AAdd(a[2], {"Wheat", 11788021, {245, 222, 179}, "#F5DEB3"})
   AAdd(a[2], {"BurlyWood", 8894686, {222, 184, 135}, "#DEB887"})
   AAdd(a[2], {"Tan", 9221330, {210, 180, 140}, "#D2B48C"})
   AAdd(a[2], {"RosyBrown", 9408444, {188, 143, 143}, "#BC8F8F"})
   AAdd(a[2], {"SandyBrown", 6333684, {244, 164, 96}, "#F4A460"})
   AAdd(a[2], {"Goldenrod", 2139610, {218, 165, 32}, "#DAA520"})
   AAdd(a[2], {"DarkGoldenRod", 755384, {184, 134, 11}, "#B8860B"})
   AAdd(a[2], {"Peru", 4163021, {205, 133, 63}, "#CD853F"})
   AAdd(a[2], {"Chocolate", 1993170, {210, 105, 30}, "#D2691E"})
   AAdd(a[2], {"SaddleBrown", 1262987, {139, 69, 19}, "#8B4513"})
   AAdd(a[2], {"Sienna", 2970272, {160, 82, 45}, "#A0522D"})
   AAdd(a[2], {"Brown", 2763429, {165, 42, 42}, "#A52A2A"})
   AAdd(a[2], {"Maroon", 128, {128, 0, 0}, "#800000"})
   AAdd(m, a)

   a := {"Primary_colors", {}}
   AAdd(a[2], {"Black", 0, {0, 0, 0}, "#000000"})
   AAdd(a[2], {"Gray", 8421504, {128, 128, 128}, "#808080"})
   AAdd(a[2], {"Silver", 12632256, {192, 192, 192}, "#C0C0C0"})
   AAdd(a[2], {"White", 16777215, {255, 255, 255}, "#FFFFFF"})
   AAdd(a[2], {"Fuchsia", 16711935, {255, 0, 255}, "#FF00FF"})
   AAdd(a[2], {"Purple", 8388736, {128, 0, 128}, "#800080"})
   AAdd(a[2], {"Red", 255, {255, 0, 0}, "#FF0000"})
   AAdd(a[2], {"Maroon", 128, {128, 0, 0}, "#800000"})
   AAdd(a[2], {"Yellow", 65535, {255, 255, 0}, "#FFFF00"})
   AAdd(a[2], {"Olive", 32896, {128, 128, 0}, "#808000"})
   AAdd(a[2], {"Lime", 65280, {0, 255, 0}, "#00FF00"})
   AAdd(a[2], {"Green", 32768, {0, 128, 0}, "#008000"})
   AAdd(a[2], {"Aqua", 16776960, {0, 255, 255}, "#00FFFF"})
   AAdd(a[2], {"Teal", 8421376, {0, 128, 128}, "#008080"})
   AAdd(a[2], {"Blue", 16711680, {0, 0, 255}, "#0000FF"})
   AAdd(a[2], {"Navy", 8388608, {0, 0, 128}, "#000080"})
   AAdd(m, a)

   a := {"Green_tones", {}}
   AAdd(a[2], {"GreenYellow", 3145645, {173, 255, 47}, "#ADFF2F"})
   AAdd(a[2], {"Chartreuse", 65407, {127, 255, 0}, "#7FFF00"})
   AAdd(a[2], {"LawnGreen", 64636, {124, 252, 0}, "#7CFC00"})
   AAdd(a[2], {"Lime", 65280, {0, 255, 0}, "#00FF00"})
   AAdd(a[2], {"LimeGreen", 3329330, {50, 205, 50}, "#32CD32"})
   AAdd(a[2], {"PaleGreen", 10025880, {152, 251, 152}, "#98FB98"})
   AAdd(a[2], {"LightGreen", 9498256, {144, 238, 144}, "#90EE90"})
   AAdd(a[2], {"MediumSpringGreen", 10156544, {0, 250, 154}, "#00FA9A"})
   AAdd(a[2], {"SpringGreen", 8388352, {0, 255, 127}, "#00FF7F"})
   AAdd(a[2], {"MediumSeaGreen", 7451452, {60, 179, 113}, "#3CB371"})
   AAdd(a[2], {"SeaGreen", 5737262, {46, 139, 87}, "#2E8B57"})
   AAdd(a[2], {"ForestGreen", 2263842, {34, 139, 34}, "#228B22"})
   AAdd(a[2], {"Green", 32768, {0, 128, 0}, "#008000"})
   AAdd(a[2], {"DarkGreen", 25600, {0, 100, 0}, "#006400"})
   AAdd(a[2], {"YellowGreen", 3329434, {154, 205, 50}, "#9ACD32"})
   AAdd(a[2], {"OliveDrab", 2330219, {107, 142, 35}, "#6B8E23"})
   AAdd(a[2], {"Olive", 32896, {128, 128, 0}, "#808000"})
   AAdd(a[2], {"DarkOliveGreen", 3107669, {85, 107, 47}, "#556B2F"})
   AAdd(a[2], {"MediumAquamarine", 11193702, {102, 205, 170}, "#66CDAA"})
   AAdd(a[2], {"DarkSeaGreen", 9419919, {143, 188, 143}, "#8FBC8F"})
   AAdd(a[2], {"LightSeaGreen", 11186720, {32, 178, 170}, "#20B2AA"})
   AAdd(a[2], {"DarkCyan", 9145088, {0, 139, 139}, "#008B8B"})
   AAdd(a[2], {"Teal", 8421376, {0, 128, 128}, "#008080"})
   AAdd(m, a)

   a := {"Blue_tones", {}}
   AAdd(a[2], {"Aqua", 16776960, {0, 255, 255}, "#00FFFF"})
   AAdd(a[2], {"Cyan", 16776960, {0, 255, 255}, "#00FFFF"})
   AAdd(a[2], {"LightCyan", 16777184, {224, 255, 255}, "#E0FFFF"})
   AAdd(a[2], {"PaleTurquoise", 15658671, {175, 238, 238}, "#AFEEEE"})
   AAdd(a[2], {"Aquamarine", 13959039, {127, 255, 212}, "#7FFFD4"})
   AAdd(a[2], {"Turquoise", 13688896, {64, 224, 208}, "#40E0D0"})
   AAdd(a[2], {"MediumTurquoise", 13422920, {72, 209, 204}, "#48D1CC"})
   AAdd(a[2], {"DarkTurquoise", 13749760, {0, 206, 209}, "#00CED1"})
   AAdd(a[2], {"CadetBlue", 10526303, {95, 158, 160}, "#5F9EA0"})
   AAdd(a[2], {"SteelBlue", 11829830, {70, 130, 180}, "#4682B4"})
   AAdd(a[2], {"LightSteelBlue", 14599344, {176, 196, 222}, "#B0C4DE"})
   AAdd(a[2], {"PowderBlue", 15130800, {176, 224, 230}, "#B0E0E6"})
   AAdd(a[2], {"LightBlue", 15128749, {173, 216, 230}, "#ADD8E6"})
   AAdd(a[2], {"SkyBlue", 15453831, {135, 206, 235}, "#87CEEB"})
   AAdd(a[2], {"LightSkyBlue", 16436871, {135, 206, 250}, "#87CEFA"})
   AAdd(a[2], {"DeepSkyBlue", 16760576, {0, 191, 255}, "#00BFFF"})
   AAdd(a[2], {"DodgerBlue", 16748574, {30, 144, 255}, "#1E90FF"})
   AAdd(a[2], {"CornflowerBlue", 15570276, {100, 149, 237}, "#6495ED"})
   AAdd(a[2], {"MediumSlateBlue", 15624315, {123, 104, 238}, "#7B68EE"})
   AAdd(a[2], {"RoyalBlue", 14772545, {65, 105, 225}, "#4169E1"})
   AAdd(a[2], {"Blue", 16711680, {0, 0, 255}, "#0000FF"})
   AAdd(a[2], {"MediumBlue", 13434880, {0, 0, 205}, "#0000CD"})
   AAdd(a[2], {"DarkBlue", 9109504, {0, 0, 139}, "#00008B"})
   AAdd(a[2], {"Navy", 8388608, {0, 0, 128}, "#000080"})
   AAdd(a[2], {"MidnightBlue", 7346457, {25, 25, 112}, "#191970"})
   AAdd(m, a)

   a := {"White_tones", {}}
   AAdd(a[2], {"White", 16777215, {255, 255, 255}, "#FFFFFF"})
   AAdd(a[2], {"Snow", 16448255, {255, 250, 250}, "#FFFAFA"})
   AAdd(a[2], {"Honeydew", 15794160, {240, 255, 240}, "#F0FFF0"})
   AAdd(a[2], {"MintCream", 16449525, {245, 255, 250}, "#F5FFFA"})
   AAdd(a[2], {"Azure", 16777200, {240, 255, 255}, "#F0FFFF"})
   AAdd(a[2], {"AliceBlue", 16775408, {240, 248, 255}, "#F0F8FF"})
   AAdd(a[2], {"GhostWhite", 16775416, {248, 248, 255}, "#F8F8FF"})
   AAdd(a[2], {"WhiteSmoke", 16119285, {245, 245, 245}, "#F5F5F5"})
   AAdd(a[2], {"Seashell", 15660543, {255, 245, 238}, "#FFF5EE"})
   AAdd(a[2], {"Beige", 14480885, {245, 245, 220}, "#F5F5DC"})
   AAdd(a[2], {"OldLace", 15136253, {253, 245, 230}, "#FDF5E6"})
   AAdd(a[2], {"FloralWhite", 15792895, {255, 250, 240}, "#FFFAF0"})
   AAdd(a[2], {"Ivory", 15794175, {255, 255, 240}, "#FFFFF0"})
   AAdd(a[2], {"AntiqueWhite", 14150650, {250, 235, 215}, "#FAEBD7"})
   AAdd(a[2], {"Linen", 15134970, {250, 240, 230}, "#FAF0E6"})
   AAdd(a[2], {"LavenderBlush", 16118015, {255, 240, 245}, "#FFF0F5"})
   AAdd(a[2], {"MistyRose", 14804223, {255, 228, 225}, "#FFE4E1"})
   AAdd(m, a)

   a := {"Gray_tones", {}}
   AAdd(a[2], {"Gainsboro", 14474460, {220, 220, 220}, "#DCDCDC"})
   AAdd(a[2], {"LightGrey", 13882323, {211, 211, 211}, "#D3D3D3"})
   AAdd(a[2], {"LightGray", 13882323, {211, 211, 211}, "#D3D3D3"})
   AAdd(a[2], {"Silver", 12632256, {192, 192, 192}, "#C0C0C0"})
   AAdd(a[2], {"DarkGray", 11119017, {169, 169, 169}, "#A9A9A9"})
   AAdd(a[2], {"DarkGrey", 11119017, {169, 169, 169}, "#A9A9A9"})
   AAdd(a[2], {"Gray", 8421504, {128, 128, 128}, "#808080"})
   AAdd(a[2], {"Grey", 8421504, {128, 128, 128}, "#808080"})
   AAdd(a[2], {"DimGray", 6908265, {105, 105, 105}, "#696969"})
   AAdd(a[2], {"DimGrey", 6908265, {105, 105, 105}, "#696969"})
   AAdd(a[2], {"LightSlateGray", 10061943, {119, 136, 153}, "#778899"})
   AAdd(a[2], {"LightSlateGrey", 10061943, {119, 136, 153}, "#778899"})
   AAdd(a[2], {"SlateGray", 9470064, {112, 128, 144}, "#708090"})
   AAdd(a[2], {"SlateGrey", 9470064, {112, 128, 144}, "#708090"})
   AAdd(a[2], {"DarkSlateGray", 5197615, {47, 79, 79}, "#2F4F4F"})
   AAdd(a[2], {"DarkSlateGrey", 5197615, {47, 79, 79}, "#2F4F4F"})
   AAdd(a[2], {"Black", 0, {0, 0, 0}, "#000000"})
   AAdd(m, a)

   o:oCLR  := oHmgData()
   o:oRGB  := oHmgData()
   o:oHEX  := oHmgData()
   o:oNAME := oHmgData()
   o:oALL  := oHmgData()
   FOR EACH a IN m
       FOR EACH b IN a[2]
           o:oCLR:Set (b[1], b[2])
           o:oRGB:Set (b[1], b[3])
           o:oHEX:Set (b[1], b[4])
           o:oNAME:Set(b[1], b[1])
           o:oALL:Set (b[1], b   )
       NEXT
   NEXT

RETURN o
