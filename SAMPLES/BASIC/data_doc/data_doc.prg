/*

    PROCEDURE file: DATA_DOC.PRG

            System: Data Documentor!
            Author: Phil Barnett
     Last modified: 07/11/1993      1:31

     Procs & Fncts: DATA_DOC()
                  : PICK_FILE()
                  : SEND()


             Calls: DBFSIZE()

         Documented 07/11/1993 at 01:33

   Migrated to harbor Minigui Extended
   Date   : 30/06/2024
   Update : 01/07/2024
   Author : Marcos Jarrin

*/

#include "minigui.ch"

REQUEST DBFCDX

#define dbfsize() ( ( recsize() * lastrec() ) + header() + 1 )

STATIC allparms, h_prn, h_vid, h_fil, pass
STATIC Copyright
STATIC Progname


/*

       Function: DATA_DOC()

          Calls: PICK_FILE()        (function  in data_doc.prg)
               : SEND()             (function  in data_doc.prg)
               : FEEL()             (function  in prb_feel.prg)
               : FULLDATE()         (function  in prb_full.prg)
               : AM_PM()            (function  in prb_ampm.prg)

*/
PROCEDURE Main()

   LOCAL nRow, nCol
   LOCAL nSpace0, nSpace1

   rddSetDefault( "DBFCDX" )

   Copyright := "Public Domain, 1996-2024, Phil Barnett - Marcos Jarrin"
   Progname := "Data Documentor! 3.0 "

   nRow := 0
   nCol := 50
   nSpace0 := 5
   nSpace1 := 25

   DEFINE WINDOW data_doc ;
         AT 0, 0 ;
         WIDTH Max( 800, getdesktopwidth() - 750 ) ;
         HEIGHT Max( 270, getdesktopheight() - 580 ) ;
         TITLE "Data Documentor 3.0 - Public Domain, 1996-2024, Phil Barnett - Marcos Jarrin" ;
         MAIN ;
         NOMAXIMIZE

      @nRow + nSpace0, nCol + 220 LABEL TITLE_01 VALUE "DBF Table documenter" WIDTH 400 SIZE 16
      nSpace0 += nSpace1 + 15

      @nRow + nSpace0, nCol + 155 LABEL MENSAJE01 VALUE "Document your .DBF's and/or Create Flatfile Conversion Scripts" WIDTH 400 SIZE 12
      nSpace1 := 28
      nSpace0 += nSpace1 + 5

      @nRow + nSpace0, nCol LABEL MENSAJE03 VALUE 'Use a file name, or wildcards ? *. Must end with DBF. Example *.dbf. Enter to select file ' WIDTH 510 SIZE 10
      @nRow + nSpace0, nCol + 515 TEXTBOX Parameter_1 WIDTH 180 MAXLENGTH 256 VALUE "*.DBF" ON ENTER FileUpload()
      nSpace0 += nSpace1

      @nRow + nSpace0, nCol RADIOGROUP Radio_1 ;
         OPTIONS { 'Only .STU Structures', 'Only .CV? Structures', 'Both Structures (.STU and .CV?)' } ;
         VALUE 1 ;
         WIDTH 100 ;
         TOOLTIP 'RadioGroup 1 leftjustify vertical(default)'

      nSpace0 += ( nSpace1 + nSpace1 + nSpace1 )

      @nRow + nSpace0, nCol + 250 BUTTON ACEPTAR CAPTION '&Process' ACTION Process()
      @nRow + nSpace0, nCol + 405 BUTTON CANCELAR CAPTION 'C&ancel' ACTION ExitProgram()

   END WINDOW

   data_doc.center()
   data_doc.activate()

RETURN


/*

  File Upload

*/
FUNCTION FileUpload()

   data_doc.Parameter_1.VALUE := Getfile ( { { 'DBF files', '*.dbf' } }, 'Select file' )

RETURN .T.


/*

Exit the program

*/
PROCEDURE ExitProgram()

   QUIT

RETURN


/*

  Process

*/
PROCEDURE Process()

   LOCAL dirlist, howmanCol, pwidth, sepline, dblsepline, title_line, dirpass, whichone
   LOCAL fld_list, fld_list_2, AUTO, offset, dotat, rootname, mDate, mTime, x
   LOCAL datatype, output, do_it, ntx_ary, num_ntxs, indx_string, ntxptr
   LOCAL parm1 := data_doc.Parameter_1.VALUE
   LOCAL sp := Space( 6 )
   LOCAL oErr

   ErrorBlock( {| objErr | Break( objErr ) } )

   dirlist := ASort( pick_file( parm1 ) )
   howmanCol := Len( dirlist )

   IF howmanCol == 0
      msgbox( 'No files found for ' + parm1 )
      RETURN
   ENDIF

   // pass 2 = FILE
   pass := 2
   pwidth := 79

   MessageBoxTimeout( "Building File Output", "Working", , 500 )
   sepline := Replicate( '-', pwidth )
   dblsepline := Replicate( '*', pwidth )
   title_line := '  ' + progname + PadL( Copyright + '  ', pwidth - 22 )

   FOR dirpass := 1 TO howmanCol

      whichone := dirlist[ dirpass ]

      MessageBoxTimeout( '     Working on File: ' + Pad( whichone, 16 ) + LTrim( Str( dirpass ) ) + ' of ' + LTrim( Str( howmanCol ) ) + '     ', "Working", , 500 )

      BEGIN SEQUENCE
         USE ( whichone ) ALIAS temp_alias
      RECOVER USING oErr

         IF oErr <> NIL
            msgbox( "Error. Corrupted table structure." )
         ENDIF

      END

      fld_list := dbStruct()
      fld_list_2 := AClone( fld_list )

      AUTO := .T.
      offset := 1
      dotat := At( '.', whichone )
      rootname := Left( whichone, dotat - 1 )

      // ************************** Natural Output Script ***********************

      IF !( data_doc.radio_1.VALUE == 2 )

         IF pass == 2
            h_fil := FCreate( rootname + '.STU' )
         ENDIF

         mdate := Date()
         mtime := ""
         feel( whichone, @mDATE, @mtime )

         send( title_line )
         send( sepline )
         send( Pad( "  DATABASE: " + whichone, 25 ) + PadL( 'Last Modified:   ' + fulldate( mdate ) + ' ' + LTrim( am_pm(mtime ) ) + '  ', pwidth - 25 ) )
         send( sepline )
         send( '' )
         send( Pad( '  Record Count: ' + LTrim( Str(LastRec() ) ), 22 ) + PadL( 'Table Size: ' + LTrim( Transform(dbfsize(),'999,999,999,999' ) ) + '  ', pwidth - 22 ) )
         send( '' )
         send( '  Field    Field Name  Type           Width       Dec.    Begin      End' )
         send( '' )

         FOR X := 1 TO FCount()

            DO CASE
            CASE fld_list[ x, 2 ] == 'C'
               datatype := "Character"
            CASE fld_list[ x, 2 ] == 'N'
               datatype := "Numeric  "
            CASE fld_list[ x, 2 ] == 'L'
               datatype := "Logical  "
            CASE fld_list[ x, 2 ] == 'M'
               datatype := "Memo     "
            CASE fld_list[ x, 2 ] == 'D'
               datatype := "Date     "
            OTHERWISE
               datatype := fld_list[ x, 2 ]
            ENDCASE

            IF fld_list[ x, 4 ] == 0
               output := Space( 4 )
            ELSE
               output := Str( fld_list[ x, 4 ], 4 )
            ENDIF

            send( Str( X, 5 ) + sp + Pad( fld_list[ x, 1 ], 12 ) + datatype + sp + Str( fld_list[ x, 3 ], 4 ) + sp + output + sp + Str( offset, 4 ) + sp + Str( offset + fld_list[ x, 3 ] - 1, 4 ) )

            offset += fld_list[ x, 3 ]

         NEXT

         send( '' )
         send( '           Total                ' + Str( offset ) )

         DO CASE
         CASE pass == 1
            send( '' )
            send( dblsepline, .F. )
         CASE pass == 2
            FClose( h_fil )
         CASE pass == 3
            send( Chr( 12 ), .F. )
         ENDCASE

      ENDIF

      // ************************ Flat File Import Script (.CVI) *****************

      IF ( data_doc.radio_1.VALUE == 2 ) .OR. ( data_doc.radio_1.VALUE == 3 )

         do_it := .T.

         IF pass == 2
            h_fil := FCreate( rootname + '.CVI' )
         ELSE
            send( title_line )
            send( sepline )
            send( '  Script Name:   ' + rootname + '.CVI' )
            send( sepline )
            send( '' )
         ENDIF

         IF do_it

            ntx_ary := Directory( Left( rootname, 7 ) + '*.NTX' )
            num_ntxs := Len( ntx_ary )
            indx_string := ''

            FOR ntxptr := 1 TO num_ntxs
               indx_string += Left( rootname, 7 ) + AllTrim( Str( ntxptr ) ) + '.NTX,'
            NEXT

            IF Right( indx_string, 1 ) == ','
               indx_string := Left( indx_string, Len( indx_string ) - 1 )
            ENDIF

            send( 'TEXTIN = ' + rootname + '.IN' )
            send( 'DBFOUT = ' + whichone )
            send( 'INDEX = ' + indx_string )
            send( 'DELIMITER = POSITION' )
            send( 'FIELDS = ' + AllTrim( Str( FCount() ) ) )

            offset := 1

            FOR X := 1 TO FCount()

               output := StrZero( offset, 4 ) + '   ' + StrZero( offset + fld_list_2[ x, 3 ] - 1, 4 ) + '    ' + Pad( fld_list_2[ x, 1 ], 15 )

               DO CASE
               CASE fld_list_2[ x, 2 ] == 'N'
                  output += 'VAL(LTRIM(TOKEN))'
               CASE fld_list_2[ x, 2 ] == 'L'
                  output += '(TOKEN $ "YT")'
               CASE fld_list_2[ x, 2 ] == 'D'
                  output += 'MMDDYY2D(TOKEN)'
               ENDCASE

               send( output )

               offset += fld_list_2[ x, 3 ]

            NEXT

            send( '' )

            DO CASE
            CASE pass == 1
               send( '' )
               send( dblsepline, .F. )
            CASE pass == 2
               FClose( h_fil )
            CASE pass == 3
               send( Chr( 12 ), .F. )
            ENDCASE

         ENDIF

         // ****************** .DBF Export to Flat File Script (.CVO) **************

         do_it := .T.

         IF pass == 2
            IF File( rootname + '.CVO' ) .AND. ! AUTO
               h_fil := FCreate( rootname + '.CVO' )
            ELSE
               h_fil := FCreate( rootname + '.CVO' )
            ENDIF
         ELSE
            send( title_line )
            send( sepline )
            send( '  Script Name:   ' + rootname + '.CVO' )
            send( sepline )
            send( '' )
         ENDIF

         IF do_it
            send( 'TEXTOUT = ' + rootname + '.OUT' )
            send( 'DBFIN = ' + whichone )
            send( 'INDEX = ' + Left( rootname, 7 ) + '1.NTX' )
            send( 'DELIMITER = ""' )
            send( 'FIELDS = ' + AllTrim( Str( FCount() ) ) )

            FOR X := 1 TO FCount()

               DO CASE
               CASE fld_list_2[ x, 2 ] == 'C'
                  send( fld_list[ x, 1 ] )
               CASE fld_list_2[ x, 2 ] == 'N'
                  send( 'STR(' + fld_list[ x, 1 ] + ',' + LTrim( Str( fld_list_2[ x, 3 ], 4 ) ) + ',' + LTrim( Str( fld_list_2[ x, 4 ], 4 ) ) + ')' )
               CASE fld_list_2[ x, 2 ] == 'L'
                  send( 'IF(' + fld_list[ x, 1 ] + ',"Y","N")' )
               CASE fld_list_2[ x, 2 ] == 'D'
                  send( 'D2MMDDYY(' + fld_list[ x, 1 ] + ')' )
               ENDCASE

            NEXT

            send( '' )

            DO CASE
            CASE pass == 1
               send( '' )
               send( dblsepline, .F. )
            CASE pass == 2
               FClose( h_fil )
            CASE pass == 3
               send( Chr( 12 ), .F. )
            ENDCASE

         ENDIF

      ENDIF

      IF pass == 1
         send( '' )
      ENDIF

   NEXT

   msgbox( 'Process completed.' )
   FClose( h_vid )

RETURN


*!*****************************************************************************
*!
*!       Function: PICK_FILE()
*!
*!      Called by: DATA_DOC()         (function  in DATA_DOC.PRG)
*!
*!*****************************************************************************
FUNCTION pick_file( file_spec )

   LOCAL dirlist := Directory( file_spec )
   LOCAL picklist := {}
   LOCAL num_files := Len( dirlist )
   LOCAL X

   FOR X := 1 TO num_files
      AAdd( picklist, dirlist[ x, 1 ] )
   NEXT

RETURN picklist


*!*****************************************************************************
*!
*!       Function: SEND()
*!
*!      Called by: DATA_DOC()         (function  in DATA_DOC.PRG)
*!
*!*****************************************************************************
FUNCTION send( txt, newline )

   newline := IF( newline == NIL, .T., newline )

   IF newline
      txt += Chr( 13 ) + Chr( 10 )
   ENDIF

   IF pass == 1
      FWrite( h_vid, txt )
   ENDIF

   IF pass == 2
      FWrite( h_fil, txt )
   ENDIF

RETURN NIL

// EOF: DATA_DOC.PRG
