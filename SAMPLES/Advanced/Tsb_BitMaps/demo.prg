/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
*/

#include "minigui.ch"
#include "tsbrowse.ch"

REQUEST DBFCDX

PROCEDURE Main()

   LOCAL i, oBrw, aStru
   LOCAL oCol

   rddSetDefault( "DBFCDX" )

   SET DELETED   ON
   SET EXCLUSIVE ON
   SET AUTOPEN   ON

   // --------------
   SET OOP       ON
   // --------------

   SET NAVIGATION EXTENDED

   aStru := { ;
      { "ID", "+", 5, 0 }, ;
      { "INFO", "C", 15, 0 }, ;
      { "FLD1", "L", 1, 0 }, ;
      { "FLD2", "L", 1, 0 }, ;
      { "FLD3", "L", 1, 0 }, ;
      { "FLD4", "L", 1, 0 }, ;
      { "FLD5", "L", 1, 0 }, ;
      { "FLD6", "N", 1, 0 }, ;
      { "FLD7", "N", 1, 0 } ;
      }

   IF ! hb_FileExists( "datab.dbf" )
      dbCreate( "datab", aStru )
   ENDIF

   USE datab ALIAS base NEW

   IF LastRec() == 0
      FOR i := 1 TO 99
         APPEND BLANK
         REPLACE INFO WITH RandStr( 15 ), ;
            FLD1 WITH If( i % 3 == 0, .T., .F. ), ;
            FLD2 WITH If( i % 3 == 0, .F., .T. ), ;
            FLD3 WITH If( i % 4 == 0, .T., .F. ), ;
            FLD4 WITH i % 2 == 0, ;
            FLD5 WITH i % 3 == 0, ;
            FLD6 WITH i % 3, ;
            FLD7 WITH i % 6
      NEXT
      dbGoTop()
   ENDIF


   DEFINE WINDOW win_1 AT 0, 0 WIDTH 700 HEIGHT 500 ;
         MAIN TITLE "TSBrowse aBitMaps and aCheck Usage Test" NOMAXIMIZE NOSIZE

      DEFINE TBROWSE obrw AT 40, 10 GRID ALIAS "base" ;
            WIDTH 650 HEIGHT 422

         ADD COLUMN TO oBrw DATA FieldWBlock( "ID", Select( "base" ) ) ;
            HEADER "ID" FOOTER "" ;
            ALIGN DT_CENTER, DT_CENTER, DT_CENTER ;
            SIZE 60

         ADD COLUMN TO oBrw DATA FieldWBlock( "INFO", Select( "base" ) ) ;
            HEADER "Name" FOOTER "" ;
            ALIGN DT_LEFT, DT_CENTER, DT_RIGHT ;
            WIDTH 150

         ADD COLUMN TO oBrw DATA FieldWBlock( "FLD1", Select( "base" ) ) ;
            HEADER "Print" FOOTER "" ;
            ALIGN DT_CENTER, DT_CENTER, DT_CENTER ;
            WIDTH 50 ;
            CHECK ;
            EDITABLE ;
            MOVE DT_DONT_MOVE

         ADD COLUMN TO oBrw DATA FieldWBlock( "FLD2", Select( "base" ) ) ;
            HEADER "Save" FOOTER "" ;
            ALIGN DT_CENTER, DT_CENTER, DT_CENTER ;
            WIDTH 50 ;
            CHECK ;
            EDITABLE ;
            MOVE DT_DONT_MOVE

         ADD COLUMN TO oBrw DATA FieldWBlock( "FLD3", Select( "base" ) ) ;
            HEADER "Mail" FOOTER "" ;
            ALIGN DT_CENTER, DT_CENTER, DT_CENTER ;
            WIDTH 50 ;
            CHECK ;
            EDITABLE ;
            MOVE DT_DONT_MOVE

         ADD COLUMN TO oBrw DATA FieldWBlock( "FLD4", Select( "base" ) ) ;
            HEADER "Check" FOOTER "" ;
            ALIGN DT_CENTER, DT_CENTER, DT_CENTER ;
            WIDTH 50 ;
            CHECK ;
            EDITABLE ;
            MOVE DT_DONT_MOVE

         ADD COLUMN TO oBrw DATA FieldWBlock( "FLD5", Select( "base" ) ) ;
            HEADER "Stock" FOOTER "" ;
            ALIGN DT_CENTER, DT_CENTER, DT_CENTER ;
            WIDTH 50 ;
            CHECK ;
            EDITABLE ;
            MOVE DT_DONT_MOVE

         ADD COLUMN TO oBrw DATA FieldWBlock( "FLD6", Select( "base" ) ) ;
            HEADER "oCol:aBitMaps" FOOTER "" ;
            ALIGN DT_CENTER, DT_CENTER, DT_CENTER ;
            WIDTH 100

         ADD COLUMN TO oBrw DATA FieldWBlock( "FLD7", Select( "base" ) ) ;
            HEADER "Flag" FOOTER "" ;
            ALIGN DT_CENTER, DT_CENTER, DT_CENTER ;
            WIDTH 50

         AEval( oBrw:aColumns, {| oCol, nCol | oCol:lFixLite := .T., ;
            oCol:cName := aStru[ nCol ][ 1 ] } )

         oBrw:nWheelLines := 1
         oBrw:nClrLine := COLOR_GRID

         oBrw:nHeightCell := 38
         oBrw:nHeightHead := oBrw:nHeightCell
         oBrw:nHeightFoot := oBrw:nHeightCell

         oBrw:GetColumn( "FLD1" ):aCheck := { LoadImage( ".\RES\Print32.png" ), NIL }
         oBrw:GetColumn( "FLD2" ):aCheck := { LoadImage( ".\RES\Save32.png" ), NIL }
         oBrw:GetColumn( "FLD3" ):aCheck := { LoadImage( ".\RES\Mail32.png" ), NIL }
         oBrw:GetColumn( "FLD4" ):aCheck := { LoadImage( ".\RES\check1.bmp" ), ;
            LoadImage( ".\RES\check0.bmp" ) }

         oBrw:lCheckBoxAllReturn := .T.
         // oBrw:GetColumn("FLD5"):lCheckBoxNoReturn := .F.

         oBrw:GetColumn( "FLD6" ):lBitMap := .T.
         oBrw:GetColumn( "FLD6" ):aBitMaps := { LoadImage( ".\RES\edit_delete.bmp" ), ;
            LoadImage( ".\RES\edit_cancel.bmp" ) }

         oBrw:SetColor( { 6 }, { {| a, b, c | If( c:nCell == b, { Rgb( 66, 255, 236 ), Rgb( 209, 227, 248 ) }, ;
            { Rgb( 220, 220, 220 ), Rgb( 220, 220, 220 ) } ) } } )

         oBrw:GetColumn( "FLD7" ):lBitMap := .T.
         oBrw:aBitMaps := { LoadImage( ".\RES\flag_bel.bmp" ), ;
                            LoadImage( ".\RES\flag_en.bmp" ), ;
                            LoadImage( ".\RES\flag_kaz.bmp" ), ;
                            LoadImage( ".\RES\flag_ru.bmp" ), ;
                            LoadImage( ".\RES\flag_ua.bmp" ), ;
                            StockBmp( 7 ), ;
                            StockBmp( 6 ) ;
                          }

         oCol := :GetColumn( "ID" )
         oCol:Cargo := oKeyData()
         oCol:Cargo:oSelect := oKeyData()
         oCol:Cargo:nSeleBack := CLR_YELLOW
         oCol:Cargo:aSeleBack := { RGB(220, 220, 220), RGB(220, 220, 220) }

         oCol:uBmpCell := {| nc, ob |
            LOCAL oc := ob:aColumns[ nc ]
            LOCAL nr := oc:Cargo:oSelect:Get( ob:nAtPos, 0 )
            RETURN ob:aBitMaps[ 6 + nr ]
           }
         oCol:nClrBack := {|na,nc,ob|
            Local oc := ob:aColumns[ nc ]
            na := oc:Cargo:oSelect:Get( ob:nAtPos, 0 )
            Return iif( na > 0, oc:Cargo:nSeleBack, ob:nClrPane )
           }
         oCol:nClrFocuBack := {|na,nc,ob|
            Local oc := ob:aColumns[ nc ]
            na := oc:Cargo:oSelect:Get( ob:nAtPos, 0 )
            Return iif( na > 0, oc:Cargo:nSeleBack, oc:Cargo:aSeleBack )
           }

         :nFreeze := 1
         :lLockFreeze := .T.
         :nCell := :nFreeze + 1

         :UserKeys( VK_SPACE, {| ob | // Select\unSelect
                           LOCAL oc := ob:GetColumn( "ID" )
                           LOCAL nr := ob:nAtPos, np
                           IF ( np := oc:Cargo:oSelect:Get( nr, 0 ) ) > 0
                              oc:Cargo:oSelect:Del( nr )
                           ELSE
                              oc:Cargo:oSelect:Set( nr, 1 )
                           ENDIF
                           ob:DrawSelect() ; DO EVENTS
                           RETURN NIL
                           } )
         :UserKeys( VK_F2, {| ob | // Select all
                           LOCAL cAls := ob:cAlias, nRec
                           LOCAL nOld := ( cAls )->( RecNo() )
                           LOCAL oc := ob:GetColumn( "ID" )
                           oc:Cargo:oSelect := oKeyData()
                           ( cAls )->( dbGoTop() )
                           DO WHILE ( cAls )->( ! Eof() )
                              nRec := ( cAls )->( RecNo() )
                              oc:Cargo:oSelect:Set( nRec, 1 )
                              ( cAls )->( dbSkip() )
                           ENDDO
                           ( cAls )->( dbGoto( nOld ) )
                           ob:Refresh() ; DO EVENTS
                           RETURN NIL
                           } )
         :UserKeys( VK_F3, {| ob | // unSelect all
                           LOCAL oc := ob:GetColumn( "ID" )
                           oc:Cargo:oSelect := oKeyData()
                           ob:Refresh() ; DO EVENTS
                           RETURN NIL
                           } )
         :UserKeys( VK_F5, {| ob | // Get values all Selected line
                           LOCAL oc := ob:GetColumn( "ID" )
                           LOCAL ar := oc:Cargo:oSelect:GetAll( .F. )
                           LOCAL nk := Len( ar )
                           LOCAL cs := "", ni
                           FOR ni := 1 TO nk
                              cs += hb_ValToExp( ar[ ni ] ) + iif( ni == nk, "", ";" )
                           NEXT
                           AlertInfo( "Selected : " + iif( Empty( cs ), "0", ";" + cs ) )
                           RETURN NIL
                           } )

      END TBROWSE

      ON KEY ESCAPE ACTION ThisWindow.RELEASE

   END WINDOW

   oBrw:SetFocus()

   CENTER WINDOW win_1
   ACTIVATE WINDOW win_1

RETURN


FUNCTION RandStr( nLen )

   LOCAL cSet := "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM"
   LOCAL cPass := ""
   LOCAL i

   IF PCount() < 1
      cPass := " "
   ELSE
      FOR i := 1 TO nLen
         cPass += SubStr( cSet, Random( 52 ), 1 )
      NEXT
   ENDIF

RETURN cPass
