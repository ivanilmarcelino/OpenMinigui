/*

 BadaSystem
 Program       : update
 Modulo        : update.prg

 Compilador    : MINIGUI - Harbour Win32 GUI
 Link          : BCC 32 bit
 Autor         : Marcos Jarrín
 email         : marvijarrin@gmail.com
 website       : badasystem.org
 Date          : 17/04/2021

 */


#define BADAERR_NONE                0
#define BADASERR_PARAMS            -1
#define BADASERR_BAD_KEY           -2
#define BADAERR_ABORT_IN_IDLE      -3
#define BADASERR_TIMEOUT           -4
#define BADASERR_FILE_NOT_FOUND    -5
#define BADASERR_CREATE            -6
#define BADASERR_OPEN_SHARED       -7
#define BASDAERR_OPEN_EXCLUSIVE    -8
#define BASDAERR_ERASE             -9
#define BASDAERR_RENAME           -10
#define BASDAERR_WRITE            -11
#define BASDAERR_NO_STRUCTURE     -12
#define BASDAERR_CANT_UPDATE      -13

#xtranslate nTrim( <expN> ) => alltrim( str( <expN> ) )

#xtranslate ASSUME <memvar> IS <value> [IF MISSING] ;
   => <memvar> := IF( <memvar> == NIL, <value>, <memvar> )

#xtranslate upTrim( <expC> ) => upper( alltrim(<expC>) )
