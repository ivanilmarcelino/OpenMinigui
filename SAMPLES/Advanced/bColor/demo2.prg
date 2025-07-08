/*
 * MiniGUI bColor2(...) Demo
 * bColor2('R'), bColor2('R+'), bColor2('R++'), bColor2('R-'), bColor2('R--')
*/

#include "minigui.ch"

Function Main
LOCAL a,y,x,i,j,k

DEFINE WINDOW Form_1 ;
	AT 0,0   ;
	WIDTH  510 ;
	HEIGHT 680 ;
	TITLE 'Test bColor2(...) Demo' ;
	MAIN

       a := {;
/*  1 */   "YL Straw",;
/*  2 */   "Y  Yellow",;
/*  3 */   "H  Brown",;
/*  4 */   "HR Brownish reddish",;
/*  5 */   "RH Red-brownish",;
/*  6 */   "R  Red",;
/*  7 */   "RD Dark Red",;
/*  8 */   "RM Red with raspberry",;
/*  9 */   "MR Crimson Red",;
/* 10 */   "M  Raspberry",;
/* 11 */   "ML Light Raspberry",;
/* 12 */   "MV Raspberry with violet",;
/* 13 */   "VM Violet with raspberry",;
/* 14 */   "V  Violet",;
/* 15 */   "VD Dark Violet",;
/* 16 */   "VB Violet with light blue",;
/* 17 */   "BD Faded light blue but dark",;
/* 18 */   "B  Blue",;
/* 19 */   "BC Blue faded to the turquoise",;
/* 20 */   "CD Dark turquoise",;
/* 21 */   "CL Faded turquoise",;
/* 22 */   "C  Turquoise",;
/* 23 */   "CG Turquoise with greens",;
/* 24 */   "AC Light green turquoise",;
/* 25 */   "A  Sea wave",;
/* 26 */   "GA Faded greens",;
/* 27 */   "GL Faded bright greens",;
/* 28 */   "G  Green",;
/* 29 */   "GF Dark green faded",;
/* 30 */   "GG Emerald",;
/* 31 */   "GN Grayish greens",;
/* 32 */   "GD Dark green",;
/* 33 */   "GO Faded gray greens",;
/* 34 */   "OL Dirty greens",;
/* 35 */   "O  Olives",;
/* 36 */   "W  Black and white",;
/* 37 */   "Z  Salary",;
/* 38 */   "ZB Salary Blue",;
/* 39 */   "ZG Salary Green" }


       y := 20
       k := 0

       FOR i := 1 TO Len(a)
           j := trim(Left(a[ i ], 2))
           x := iif(i > 20, k, 10)

           @ y, x LABEL &('Lbl_'+hb_ntos(i)+'0') WIDTH 24 HEIGHT 24       ;
                  VALUE ' '+hb_ntos(i)+'.'

           x += 24
           @ y, x LABEL &('Lbl_'+hb_ntos(i)+'1') WIDTH 40 HEIGHT 24       ;
                  VALUE ' '+j+'-- '              BACKCOLOR bColor2(j+'--')

           x += 40
           @ y, x LABEL &('Lbl_'+hb_ntos(i)+'2') WIDTH 40 HEIGHT 24       ;
                  VALUE ' '+j+'-  '              BACKCOLOR bColor2(j+'-')

           x += 40
           @ y, x LABEL &('Lbl_'+hb_ntos(i)+'3') WIDTH 40 HEIGHT 24       ;
                  VALUE ' '+j+'   '              BACKCOLOR bColor2(j)

           x += 40
           @ y, x LABEL &('Lbl_'+hb_ntos(i)+'4') WIDTH 40 HEIGHT 24       ;
                  VALUE ' '+j+'+  '              BACKCOLOR bColor2(j+'+')

           x += 40
           @ y, x LABEL &('Lbl_'+hb_ntos(i)+'5') WIDTH 40 HEIGHT 24       ;
                  VALUE ' '+j+'++ '              BACKCOLOR bColor2(j+'++')

           y += 30
           x += 40
           IF k < 1; k := x + 20
           ENDIF
           IF i == 20; y := 20
           ENDIF
       NEXT

END WINDOW

CENTER WINDOW Form_1

ACTIVATE WINDOW Form_1

Return Nil

#include "c_bcolor.c"
