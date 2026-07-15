/*
  MINIGUI - Harbour Win32 GUI library

  Copyright 2002-2010 Roberto Lopez <harbourminigui@gmail.com>
  http://harbourminigui.googlepages.com

  Author: S.Rathinagiri <srathinagiri@gmail.com>
*/

/* Legend position */
#define LEGEND_ON_RIGHT  0
#define LEGEND_ON_BOTTOM 1

/* Graph type */
#define GT_DEFAULT       1
#define GT_COLUMNS       1
#define GT_LINE          2
#define GT_POINTS        3
#define GT_PIE           4
#define GT_BAR           5
#define GT_FUNNEL        6
#define GT_STACKEDCOLUMN 7
#define GT_STACKEDBAR    8
#define GT_AREA          9
#define GT_SCATTERXY    10
#define GT_DOUGHNUT     11
#define GT_SUNBURST     12
#define GT_WATERFALL    13
#define GT_TREEMAP        14


/* Color theme */
#define THEME_PALETTE_1  1
#define THEME_PALETTE_2  2
#define THEME_PALETTE_3  3
#define THEME_PALETTE_4  4

/* Colors */
#ifdef CLR_WHITE
#undef CLR_WHITE
#endif
#define CLR_WHITE { 255, 255, 255 }

#define CLR_HAVELOCK_BLUE  {  68 , 115 , 197 }
#define CLR_WESTSIDE       { 237 , 125 ,  49 }
#define CLR_LIGHT_GREY     { 165 , 165 , 165 }
#define CLR_ORANGE_YELLOW  { 255 , 192 ,   0 }
#define CLR_PICTON_BLUE    {  91 , 155 , 213 }
#define CLR_APPLE          { 112 , 173 ,  71 }

#define CLR_DARK_GREY      {  99 ,  99 ,  99 }
#define CLR_ENDEAVOUR      {  37 ,  95 , 145 }
#define CLR_SADDLE_BROWN   { 160 ,  71 ,  13 }
#define CLR_DELL           {  66 , 104 ,  43 }

#define CLR_LIGHT_BROWN    { 152 , 115 ,   0 }
#define CLR_DARKER_BLUE    {  38 ,  68 , 120 }
