/*
 * MINIGUI - Harbour Win32 GUI library
 *
 * Copyright 2013-2023 Verchenko Andrey <verchenkoag@gmail.com>
 * Fixed (c) 2023 Sergej Kiselev <bilance@bilance.lv>
 *
*/

#include "minigui.ch"
#include "metrocolor.ch"
#include "i_winuser.ch"

///////////////////////////////////////////////////////////////////
// преобразовать строку в функцию далее в значение
// "YELLOW()" => YELLOW => {x,x,x}
FUNCTION myStrFuncValue(xStr)
   LOCAL xRet, cRun, cMsg

   IF HB_ISCHAR(xStr)
      IF AT("()",xStr) > 0
         cRun := SUBSTR( xStr, 1 , AT("(",xStr)-1 )
         IF !hb_IsFunction( cRun )
            cMsg := 'Functions: ' + cRun + '() not in EXE file!;'
            cMsg += 'Received string: "' + xStr + '";;'
            cMsg += ProcNL(0) + ";" + ProcNL(1) + ";" +ProcNL(2)
            AlertStop( cMsg, "Run functions error" )
            xRet := xStr
         ELSE
            xRet := &xStr
         ENDIF
         //xRet := hb_ExecFromArray( xStr )
      ELSE
         xRet := xStr
      ENDIF
   ELSE
      xRet := xStr
   ENDIF

RETURN xRet

////////////////////////////////////////////////////////////////////
// Функции ЦВЕТА для ИНИ-файл
FUNCTION COLOR_BACK_dark_cyan()
   RETURN COLOR_BACK_DARK_CYAN

FUNCTION COLOR_BACK_dark_green
   RETURN COLOR_BACK_DARK_GREEN

FUNCTION COLOR_BACK_dark_purple
   RETURN COLOR_BACK_DARK_PURPLE

FUNCTION COLOR_BACK_dark_yellow
   RETURN COLOR_BACK_DARK_YELLOW

FUNCTION COLOR_BACK_bright_green
   RETURN COLOR_BACK_BRIGHT_GREEN

FUNCTION COLOR_BACK_yellow_orange
   RETURN COLOR_BACK_YELLOW_ORANGE

FUNCTION COLOR_dark_blue
   RETURN COLOR_DARK_BLUE

FUNCTION COLOR_blue_METRO
   RETURN COLOR_BLUE_METRO

FUNCTION COLOR_blue_blue
   RETURN COLOR_BLUE_BLUE

FUNCTION COLOR_blue_skype
   RETURN COLOR_BLUE_SKYPE

FUNCTION COLOR_OF2003_blue
   RETURN COLOR_OF2003_BLUE

FUNCTION COLOR_light_blue
   RETURN COLOR_LIGHT_BLUE

FUNCTION COLOR_orange_METRO
   RETURN COLOR_ORANGE_METRO

FUNCTION COLOR_yellow_METRO
   RETURN COLOR_YELLOW_METRO

FUNCTION COLOR_purple_METRO
   RETURN COLOR_PURPLE_METRO

FUNCTION COLOR_dark_purple
   RETURN COLOR_DARK_PURPLE

FUNCTION COLOR_bright_purple
   RETURN COLOR_BRIGHT_PURPLE

FUNCTION COLOR_green_METRO
   RETURN COLOR_GREEN_METRO

FUNCTION COLOR_bright_green
   RETURN COLOR_BRIGHT_GREEN

FUNCTION COLOR_light_green
   RETURN COLOR_LIGHT_GREEN

FUNCTION COLOR_red_METRO
   RETURN COLOR_RED_METRO

FUNCTION COLOR_bright_red
   RETURN COLOR_BRIGHT_RED

FUNCTION COLOR_light_red
   RETURN COLOR_LIGHT_RED

FUNCTION COLOR_dark_red
   RETURN COLOR_DARK_RED     // темно-красный

FUNCTION COLOR_bright_grey
   RETURN COLOR_BRIGHT_GREY

FUNCTION COLOR_grey_METRO
   RETURN COLOR_GREY_METRO

FUNCTION COLOR_dark_grey
   RETURN COLOR_DARK_GREY

FUNCTION COLOR_bright_brown
   RETURN COLOR_BRIGHT_BROWN

FUNCTION COLOR_light_ORANGE
RETURN COLOR_LIGHT_ORANGE

FUNCTION COLOR_BRIGHT_orange()
RETURN COLOR_BRIGHT_ORANGE

FUNCTION COLOR_OF2003_green()
RETURN COLOR_OF2003_GREEN

FUNCTION color_LIGHT_2BLUE
RETURN COLOR_LIGHT_2BLUE

FUNCTION color_LIGHT_GREEN_LIME
RETURN COLOR_LIGHT_GREEN_LIME

FUNCTION color_OF2003_ORANGE()
RETURN COLOR_OF2003_ORANGE


FUNCTION color_AZURE1
RETURN COLOR_AZURE1

FUNCTION color_AZURE2
RETURN COLOR_AZURE2

FUNCTION color_AZURE3
RETURN COLOR_AZURE3

FUNCTION color_AZURE4
RETURN COLOR_AZURE4

FUNCTION color_OF2003_SILVER
RETURN   COLOR_OF2003_SILVER

FUNCTION color_LIGHT_PURPLE
RETURN   COLOR_LIGHT_PURPLE

FUNCTION  COLOR_purple40 // фиолетовый-осветленный 40%
RETURN COLOR_PURPLE40

////////////////////////////////////////////////////////
FUNCTION yellow()
   RETURN YELLOW

FUNCTION pink()
   RETURN PINK

FUNCTION Red()
   RETURN RED

FUNCTION Fuchsia
   RETURN FUCHSIA

FUNCTION brown
   RETURN BROWN

FUNCTION orange()
   RETURN ORANGE

FUNCTION green
   RETURN GREEN

FUNCTION purple
   RETURN PURPLE

FUNCTION black
   RETURN BLACK

FUNCTION white
   RETURN WHITE

FUNCTION gray
   RETURN GRAY

FUNCTION blue
   RETURN BLUE

FUNCTION silver
   RETURN SILVER

FUNCTION maroon
   RETURN MAROON

FUNCTION olive
   RETURN OLIVE

FUNCTION lgreen
   RETURN LGREEN

FUNCTION aqua
   RETURN AQUA

FUNCTION navy
   RETURN NAVY

FUNCTION teal
   RETURN TEAL

/////////////////////////// TSBROWSE.CH ///////////////////////
// Horizontal alignment
/*#ifndef DT_LEFT
   #define DT_LEFT              0
   #define DT_CENTER            1
   #define DT_RIGHT             2
#endif
*/
FUNCTION DT_left()
RETURN 0
FUNCTION DT_center()
RETURN 1
FUNCTION DT_Right()
RETURN 2

/////////////////////////// TSBROWSE.CH ///////////////////////
// Функции для ИНИ-файл
FUNCTION myGetTitleHeight()
RETURN GetTitleHeight()

////////////////////////////////////////////////////////////////////
// Функции ЦВЕТА для ИНИ-файл
FUNCTION myRGB(aDim, lNum)
   LOCAL aDef := { 245, 245, 245 }
   STATIC s_oRgbColor

   IF s_oRgbColor == NIL
      s_oRgbColor := oHmgData()
      s_oRgbColor:aqua    := {   0 , 255 , 255 }
      s_oRgbColor:black   := {   0 ,   0 ,   0 }
      s_oRgbColor:blue    := {   0 ,   0 , 255 }
      s_oRgbColor:brown   := { 128 ,  64 ,  64 }
      s_oRgbColor:cyan    := { 153 , 217 , 234 }
      s_oRgbColor:fuchsia := { 255 ,   0 , 255 }
      s_oRgbColor:gray    := { 128 , 128 , 128 }
      s_oRgbColor:grey    := { 128 , 128 , 128 }
      s_oRgbColor:green   := {   0 , 255 ,   0 }
      s_oRgbColor:lgreen  := {   0 , 128 ,   0 }
      s_oRgbColor:lime    := { 191,  255 ,   0 }
      s_oRgbColor:maroon  := { 128 ,   0 ,   0 }
      s_oRgbColor:navy    := {   0 ,   0 , 128 }
      s_oRgbColor:olive   := { 128 , 128 ,   0 }
      s_oRgbColor:orange  := { 255 , 128 ,  64 }
      s_oRgbColor:pink    := { 255 , 128 , 192 }
      s_oRgbColor:purple  := { 128 ,   0 , 128 }
      s_oRgbColor:red     := { 255 ,   0 ,   0 }
      s_oRgbColor:silver  := { 192 , 192 , 192 }
      s_oRgbColor:teal    := {   0 , 128 , 128 }
      s_oRgbColor:white   := { 255 , 255 , 255 }
      s_oRgbColor:yellow  := { 255 , 255 ,   0 }
      // можно еще продолжить список и добавить свои цвета тут
      // например из MetroColor.ch
      s_oRgbColor:color_BACK_DARK_CYAN     := { 0,64,80 }    // темно-синий
      s_oRgbColor:color_BACK_DARK_GREEN    := { 0,62, 0 }    // темно-зеленый
      s_oRgbColor:color_BACK_DARK_PURPLE   := {37, 0,64 }    // темно-фиолетовый
      s_oRgbColor:color_BACK_DARK_YELLOW   := {215,166, 0}   // темно-желтый
      s_oRgbColor:color_BACK_BRIGHT_GREEN  := {105,182,34}   // ярко-зеленый
      s_oRgbColor:color_BACK_YELLOW_ORANGE := {149,67, 1 }   // желто-оранжевый
      s_oRgbColor:color_DARK_BLUE          := {  0,155,173}  // темно-голубой
      s_oRgbColor:color_BLUE_METRO         := { 40,122,237}  // голубой
      s_oRgbColor:color_BLUE_BLUE          := {  9, 77,181}  // сине-голубой
      s_oRgbColor:color_BLUE_SKYPE         := {  0,176,240}  // голубой, как SKYPE
      s_oRgbColor:color_LIGHT_BLUE         := {159,191,236}  // Office_2003 Blue
      s_oRgbColor:color_OF2003_BLUE        := {159,191,236}  // Office_2003 Blue
      s_oRgbColor:color_LIGHT_2BLUE        := {123,140,253}  // светло-синий-голубой
      s_oRgbColor:color_ORANGE_METRO       := {210, 71, 38}  // серо-оранжевый
      s_oRgbColor:color_BRIGHT_ORANGE      := {239, 71, 38}  // ярко-оранжевый
      s_oRgbColor:color_LIGHT_ORANGE       := {255,160, 66}  // светло-оранжевый
      s_oRgbColor:color_YELLOW_METRO       := {231,178, 30}  // серо-желтый
      s_oRgbColor:color_BRIGHT_BROWN       := {138, 85, 77}  // ярко-коричневый
      s_oRgbColor:color_OF2003_ORANGE      := {251,230,148}  // Office_2003 ORANGE
      s_oRgbColor:color_ORANGE60           := {251,213,181}  // оранжевый-осветленный 60%
      s_oRgbColor:color_PURPLE_METRO       := { 94, 59,185}  // фиолетовый
      s_oRgbColor:color_DARK_PURPLE        := { 82,  0,141}  // темно-фиолетовый
      s_oRgbColor:color_BRIGHT_PURPLE      := {151,  0,160}  // ярко-фиолетовый
      s_oRgbColor:color_LIGHT_PURPLE       := {192,  0,255}  // светло-фиолетовый
      s_oRgbColor:color_PURPLE40           := {178,162,199}  // фиолетовый-осветленный 40%
      s_oRgbColor:color_GREEN_METRO        := {  0,145,  0}  // зеленый
      s_oRgbColor:color_BRIGHT_GREEN       := { 35,179, 15}  // ярко-зеленый
      s_oRgbColor:color_LIGHT_GREEN_LIME   := { 0,255, 153}  // ярко-зеленый-салатовый
      s_oRgbColor:color_LIGHT_GREEN        := {195,224,133}  // светло-зеленый
      s_oRgbColor:color_OF2003_GREEN       := {234,240,207}  // Office_2003 GREEN
      s_oRgbColor:color_OF2016_GREEN       := { 34,116, 71}  // Office_2016 GREEN
      s_oRgbColor:color_RED_METRO          := {189, 30, 73}  // красный
      s_oRgbColor:color_BRIGHT_RED         := {254, 73, 83}  // ярко-красный
      s_oRgbColor:color_LIGHT_RED          := {255,178,178}  // светло-красный
      s_oRgbColor:color_DARK_RED           := {128,  0,  0}  // темно-красный
      s_oRgbColor:color_BRIGHT_GREY        := {240,240,240}  // ярко-серый
      s_oRgbColor:color_GREY_METRO         := {221,221,221}  // серый
      s_oRgbColor:color_DARK_GREY          := { 91, 91, 91}  // темно-серый
      s_oRgbColor:color_OF2003_SILVER      := {225,226,236}  // Office_2003 SILVER
      // --------------------- Office_2003 -----------------------------
      s_oRgbColor:color_AZURE1             := {222, 218, 202}   // серый цвет
      s_oRgbColor:color_AZURE2             := {242, 240, 234}   // серый цвет
      s_oRgbColor:color_AZURE3             := {192, 185, 154}   // серый цвет
      s_oRgbColor:color_AZURE4             := {129, 127, 118}   // серый цвет
      s_oRgbColor:color_AZURE5             := { 29,  27,  18}   // серый цвет
      s_oRgbColor:color_DARK_BLUE1         := { 89, 135, 214}
      s_oRgbColor:color_DARK_BLUE2         := {224, 233, 248}
      s_oRgbColor:color_DARK_BLUE3         := {  4,  57, 148}
      s_oRgbColor:color_DARK_BLUE4         := {  0,   9,   7}
      s_oRgbColor:color_LIGHT_GREEN1       := {235, 245, 214}
      s_oRgbColor:color_LIGHT_GREEN2       := {249, 252, 243}
      s_oRgbColor:color_LIGHT_GREEN3       := {195, 224, 133}
      s_oRgbColor:color_LIGHT_GREEN4       := { 37,  49,  13}
      // --------------------- мои цвета -----------------------------
      s_oRgbColor:clr_SKYPE                := {   0, 176, 240 }
      s_oRgbColor:clr_VIBER                := { 125,  82, 158 }
      s_oRgbColor:clr_VK                   := {  93, 114, 148 }
      s_oRgbColor:clr_TWIT                 := { 118, 170, 235 }
      s_oRgbColor:clr_FB                   := {  71,  89, 149 }
      s_oRgbColor:clr_OK                   := { 238, 130,   8 }
      s_oRgbColor:color_SKYPE              := {   0, 176, 240 }
      s_oRgbColor:color_VIBER              := { 125,  82, 158 }
      s_oRgbColor:color_VK                 := {  93, 114, 148 }
      s_oRgbColor:color_TWIT               := { 118, 170, 235 }
      s_oRgbColor:color_FB                 := {  71,  89, 149 }
      s_oRgbColor:color_OK                 := { 238, 130,   8 }
      //
      // системные цвета из Windows
      s_oRgbColor:color_GRID       := GetSysColor( COLOR_GRADIENTINACTIVECAPTION )
      s_oRgbColor:color_BTNFACE    := GetSysColor( COLOR_BTNFACE )          // селектора/нумератора/вирт.колонки
      s_oRgbColor:color_WINDOWTEXT := GetSysColor( COLOR_WINDOWTEXT )

      // еще добавить можно и числа из tsbtowse.ch строки #define CLR_...
      s_oRgbColor:clr_BLACK     :=        0               // RGB(   0,   0,   0 )
      s_oRgbColor:clr_BLUE      :=  8388608               // RGB(   0,   0, 128 )
      s_oRgbColor:clr_GREEN     :=    32768               // RGB(   0, 128,   0 )
      s_oRgbColor:clr_CYAN      :=  8421376               // RGB(   0, 128, 128 )
      s_oRgbColor:clr_RED       :=      128               // RGB( 128,   0,   0 )
      s_oRgbColor:clr_MAGENTA   :=  8388736               // RGB( 128,   0, 128 )
      s_oRgbColor:clr_BROWN     :=    32896               // RGB( 128, 128,   0 )
      s_oRgbColor:clr_HGRAY     := 12632256               // RGB( 192, 192, 192 )
      s_oRgbColor:clr_LIGHTGRAY := 12632256               // CLR_HGRAY
      //----------------------------------------------------------------------------//
      //                       High Intensity Colors
      //----------------------------------------------------------------------------//
      s_oRgbColor:clr_GRAY      :=  8421504               // RGB( 128, 128, 128 )
      s_oRgbColor:clr_HBLUE     := 16711680               // RGB(   0,   0, 255 )
      s_oRgbColor:clr_HGREEN    :=    65280               // RGB(   0, 255,   0 )
      s_oRgbColor:clr_HCYAN     := 16776960               // RGB(   0, 255, 255 )
      s_oRgbColor:clr_HRED      :=      255               // RGB( 255,   0,   0 )
      s_oRgbColor:clr_HMAGENTA  := 16711935               // RGB( 255,   0, 255 )
      s_oRgbColor:clr_YELLOW    :=    65535               // RGB( 255, 255,   0 )
      s_oRgbColor:clr_WHITE     := 16777215               // RGB( 255, 255, 255 )
   ENDIF

   IF pCount() == 0
      RETURN s_oRgbColor   // что бы пополнять базу цветов на лету
   ELSEIF HB_ISCHAR(aDim)
      IF upper(left(aDim, 4)) == "CLR_" .and. lNum == Nil  // всегда возврат число, если на
         lNum := .T.                                       // ключе лежит массив цвета
      ENDIF
      aDim := s_oRgbColor:Get(aDim, aDef)
      IF Empty( lNum ) .or. HB_ISNUMERIC( aDim ) // Nil or .F. or число aDim
         RETURN aDim                     // возвращаем массив или число, если
      ENDIF                              // на ключ оно задано
   ENDIF
   Default aDim := aDef

RETURN RGB( aDim[1], aDim[2], aDim[3] )  // возвращаем число

////////////////////////////////////////////////////
/* добывать цвета

 ? myRGB("RED"), myRGB("BLUE"), myRGB("color_BACK_DARK_CYAN"), ;
   myRGB("COLOR_AZURE1")         // вернут массив цвета

 ? myRGB("RED",.T.), myRGB("BLUE",.T.), myRGB("color_BACK_DARK_CYAN",.T.), ;
   myRGB("COLOR_AZURE1",.T.)     // вернут число цвета

 ? myRGB("CLR_WHITE"), myRGB("CLR_YELLOW"), myRGB("CLR_GRAY") // вернут число цвета

пополнять базу цветов на лету можно так
 aClr := { ;
          {"CLR_SKYPE", RGB(   0, 176, 240 )}, ;
          {"CLR_VIBER", RGB( 125,  82, 158 )}, ;
          {"CLR_VK"   , RGB(  93, 114, 148 )}, ;
          {"CLR_TWIT" , RGB( 118, 170, 235 )}, ;
          {"CLR_FB"   , RGB(  71,  89, 149 )}, ;
          {"CLR_OK"   , RGB( 238, 130,   8 )}  ;
         }

  o := myRGB()
  FOR EACH a IN aClr
      o:Set(a[1], a[2])
  NEXT
добывать
 ? myRGB("CLR_SKYPE"), myRGB("CLR_VK"), myRGB("CLR_OK")
*/
