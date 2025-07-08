/*
 * HMG Embedded Child Window Demo
 * (c) 2002-2010 Roberto Lopez
*/

#include "hmg.ch"

FUNCTION Main

   LOCAL aRows[ 20 ][ 3 ]

   aRows[ 1 ] := { 'Simpson', 'Homer', '555-5555' }
   aRows[ 2 ] := { 'Mulder', 'Fox', '324-6432' }
   aRows[ 3 ] := { 'Smart', 'Max', '432-5892' }
   aRows[ 4 ] := { 'Grillo', 'Pepe', '894-2332' }
   aRows[ 5 ] := { 'Kirk', 'James', '346-9873' }

   DEFINE WINDOW Win_1 ;
         ROW 0 ;
         COL 0 ;
         WIDTH 600 ;
         HEIGHT 500 ;
         TITLE 'Panel Window Demo 2' ;
         WINDOWTYPE MAIN

      DEFINE TAB Tab_1 ;
            AT 10, 10 ;
            WIDTH 500 ;
            HEIGHT 400 ;
            VALUE 1 ;
            TOOLTIP 'Tab Control'

         PAGE 'Page 1'

            DEFINE WINDOW Win_2 ;
                  ROW 50 ;
                  COL 20 ;
                  WIDTH 300 ;
                  HEIGHT 200 ;
                  VIRTUAL WIDTH 400 ;
                  VIRTUAL HEIGHT 400 ;
                  WINDOWTYPE PANEL

               DEFINE LABEL Label_1
                  ROW 10
                  COL 10
                  VALUE 'Panel windows Can do This...' + CRLF + CRLF + ;
                        'Press the F2 hotkey to toggle the border visibility on or off.'
                  WIDTH 250
                  HEIGHT 70
               END LABEL

               DEFINE TEXTBOX Text_1
                  ROW 90
                  COL 10
                  VALUE "Can do this!"
               END TEXTBOX

            END WINDOW

         END PAGE

         PAGE 'Page &2'

            DEFINE RADIOGROUP R1
               ROW 100
               COL 100
               OPTIONS { '1', '2', '3' }
               VALUE 1
            END RADIOGROUP

         END PAGE

         PAGE 'Page 3'

            DEFINE WINDOW Win_3 ;
                  ROW 50 ;
                  COL 20 ;
                  WIDTH 200 ;
                  HEIGHT 200 ;
                  VIRTUAL WIDTH 300 ;
                  VIRTUAL HEIGHT 300 ;
                  WINDOWTYPE PANEL

               DEFINE LABEL Label_1
                  ROW 10
                  COL 10
                  VALUE 'Panel window...'
                  WIDTH 150
               END LABEL

               DEFINE TEXTBOX Text_1
                  ROW 90
                  COL 10
                  VALUE "Can do this!"
               END TEXTBOX

            END WINDOW

            @ 100, 250 SPINNER Spinner_1 ;
               RANGE 0, 10 ;
               VALUE 5 ;
               WIDTH 100 ;
               TOOLTIP 'Range 0,10' ;
               ON CHANGE PlayBeep()

         END PAGE

         PAGE 'Page 4'

            @ 50, 50 GRID Grid_1 ;
               WIDTH 300 ;
               HEIGHT 330 ;
               HEADERS { 'Last Name', 'First Name', 'Phone' } ;
               WIDTHS { 140, 140, 140 } ;
               ITEMS aRows ;
               VALUE 1

         END PAGE

      END TAB

      ON KEY F2 ACTION Win_2.Border := !(Win_2.Border)

   END WINDOW

   Win_2.Text_1.Setfocus

   CENTER WINDOW Win_1

   // Panel windows are automatically activated through its parent
   // so, only Win_1 must be activated.

   ACTIVATE WINDOW Win_1

RETURN NIL
