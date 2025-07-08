#include "hmg.ch"

#include "dbinfo.ch"

REQUEST DBFCDX

static cUser

field ID in RECYCLE

//----------------------------------------------------------------------------//

function Main()

   RDDSETDEFAULT( "DBFCDX" )

   SET DELETED ON
   SET TIME FORMAT TO "HH:MM:SS"

   cUser := hb_UserName()

	DEFINE WINDOW Form_1 ;
		WIDTH 640 HEIGHT 480 ;
		TITLE "DBF: Recycling of deleted records" ;
		MAIN NOMAXIMIZE ;
		ON INIT OpenTable() ;
		ON RELEASE CloseTable()

                ON KEY ESCAPE ACTION Form_1.Release     

		DEFINE MAIN MENU 
			POPUP 'File'
				ITEM 'Append record'	ACTION Append_record()
				ITEM 'Delete record'	ACTION Delete_record()
				SEPARATOR
				ITEM 'Exit'		ACTION Form_1.Release
			END POPUP
			POPUP 'Help'
				ITEM 'About'		ACTION MsgInfo ("MiniGUI Browse Demo") 
			END POPUP

		END MENU

		DEFINE STATUSBAR
			STATUSITEM ''
		END STATUSBAR

		@ 10,10 BROWSE Browse_1 ;
			WIDTH 610 ; 
			HEIGHT 313 ; 	
			HEADERS { 'Code' , 'Lines' , 'Number', 'Updated DateTime' } ;
			WIDTHS { 50 , 120 , 120 , 200 } ;
			WORKAREA RECYCLE ;
			FIELDS {'RECYCLE->ID' , 'RECYCLE->NAME1' , 'RECYCLE->NAME2' , 'RECYCLE->UPDT' } ;
			ON CHANGE ( ChangeTest() , PositionData() );
			JUSTIFY { BROWSE_JTFY_RIGHT, BROWSE_JTFY_CENTER, BROWSE_JTFY_RIGHT, BROWSE_JTFY_LEFT} ;
                        EDIT LOCK ;
                        WHEN { {|| .f. }, , , }

*------------------------------------------ Records  ---------------------------------------------------*

              @ 354, 020 Label Label_1G    ;
                     width   060           ;
                     height  035           ;
                     FONT "Arial" SIZE 9 BOLD ITALIC;
                     Value "Records:"
                   
              @ 354, 080 Label Label_2G    ;
                     width   085           ;
                     height  035           ;
                     FONT "Arial" SIZE 9 BOLD ITALIC;
                     Value ""

*-------------------------------------------------------------------------------------------------------*

                @ 350,165 BUTTON Button_1 ;
                          CAPTION 'Append record' ;
                          WIDTH 140 ;
                          ACTION Append_record() ;
                          TOOLTIP 'Append a new record'

                @ 350,315 BUTTON Button_2 ;
                          CAPTION 'Delete record' ;
                          WIDTH 140 ;
                          ACTION Delete_record() ;
                          TOOLTIP 'Delete the current record'

*----------------------------------------------------  Up-Down  ----------------------------------------*

    DEFINE BUTTON But_01  
         ROW    350    
         COL    495          
         WIDTH  030
         HEIGHT 030 
         PICTURE "Resource/Imagen01.bmp"            
         TOOLTIP 'First'
         ACTION ( DbGoTop() , Form_1.Browse_1.value := RECYCLE->(RecNo()) , Form_1.Browse_1.SetFocus )
     END BUTTON

    DEFINE BUTTON But_02 
         ROW    350  
         COL    525  
         WIDTH  030
         HEIGHT 030 
         PICTURE "Resource/Imagen02.bmp"     
         TOOLTIP 'Previous'
         ACTION ( DbSkip(-1) , Form_1.Browse_1.value := RECYCLE->(RecNo()) , Form_1.Browse_1.SetFocus )
    END BUTTON
 
    DEFINE BUTTON But_03
         ROW    350 
         COL    555  
         WIDTH  030
         HEIGHT 030 
         PICTURE "Resource/Imagen03.bmp"
         TOOLTIP 'Next'
         ACTION   ( DbSkip(1) ,if ( eof() , DbGoBottom() , Nil ) , Form_1.Browse_1.value := RECYCLE->(RecNo()) , Form_1.Browse_1.SetFocus )
    END BUTTON

    DEFINE BUTTON But_04
         ROW    350 
         COL    585  
         WIDTH  030
         HEIGHT 030 
         PICTURE "Resource/Imagen04.bmp"   
         TOOLTIP 'Last'
         ACTION ( DbGoBottom() , Form_1.Browse_1.value := RECYCLE->(RecNo()) , Form_1.Browse_1.SetFocus )      
    END BUTTON 
  
  END WINDOW

   SETWINDOWCURSOR (Form_1.Button_2.Handle, "resource\SmoothHand.cur")

   CENTER WINDOW Form_1
   ACTIVATE WINDOW Form_1

return nil

//----------------------------------------------------------------------------//

Function PositionData()  // Nº de Registro / Total

   local nValor , nCouR

   nValor := RECYCLE->( RecNo() )
   nCouR  := RECYCLE->( LastRec() )

   Form_1.Label_2G.value := hb_ntos(nValor) + Space(1) + "/" + Space(1) + hb_ntos(nCouR)

Return Nil

//----------------------------------------------------------------------------//

Procedure OpenTable()

   local n

   if ! file( "RECYCLE.dbf" )
      CreateTestDbf()
   endif

   USE RECYCLE NEW SHARED VIA "DBFCDX"

   // Create Memory Indexes
   for n := 1 to FCount()
      CreateTag( FieldName( n ), FieldType( n ) )
   next

   INDEX ON DELETED() TAG DELETED TO RECYCLE MEMORY ADDITIVE
//   INDEX ON RECNO() TAG RECYCLE TO RECYCLE FOR DELETED() MEMORY ADDITIVE

   OrdSetFocus( 1 )
   GO TOP

   Form_1.Browse_1.Value := RecNo()
   Form_1.Browse_1.SetFocus

Return

//----------------------------------------------------------------------------//

Procedure CloseTable()

   CLOSE RECYCLE

Return

//----------------------------------------------------------------------------//

Procedure ChangeTest()

   local nCurrentValue := GetProperty ( 'Form_1', 'Browse_1', 'Value' )

   Form_1.StatusBar.Item(1) := 'RecNo: ' + hb_ntos ( nCurrentValue )

   IF RECYCLE->( RecNo() ) != nCurrentValue
      GO nCurrentValue
   ENDIF

Return 

//----------------------------------------------------------------------------//

Procedure Append_record()

   if RECYCLE->( DBFAPPEND() ) > 0
      _FIELD->NAME1 := "Line " + LTRIM( STR( ID ) )
      _FIELD->NAME2 := LTRIM( STR( ID * 10 ) )
      _FIELD->USER  := cUser

      Form_1.Browse_1.Value := RECYCLE->( RecNo() )
   endif

   Form_1.Browse_1.SetFocus

Return

//----------------------------------------------------------------------------//

Procedure Delete_record()

   RECYCLE->( NetDelete() )

   if ! NetError()
      RECYCLE->( dbSkip() )
      if ( eof() , DbGoBottom() , Nil )
      Form_1.Browse_1.Value := RECYCLE->( RecNo() )
   endif

   Form_1.Browse_1.SetFocus

Return

//----------------------------------------------------------------------------//

function CreateTestDbf()

   local n
   local lCreated := .f.

   local aCols := {  {  "ID",       "+",   4, 0 }, ;
                     {  "NAME1",    "C",  20, 0 }, ;
                     {  "NAME2",    "C",  30, 0 }, ;
                     {  "USER",     "C",  10, 0 }, ;
                     {  "UPDT",     "=",   8, 0 }  }

   TRY
      DBCREATE( "RECYCLE", aCols, "DBFCDX", .T., "RCL" )
      lCreated := .t.
   CATCH
   END

   if lCreated

      for n := 1 to 20

         if DBFAPPEND( .f. ) > 0
            _FIELD->NAME1 := "Line " + LTRIM( STR( n ) )
            _FIELD->NAME2 := LTRIM( STR( n * 10 ) )
            _FIELD->USER  := cUser
         endif

      next

      CLOSE RCL

   endif

return nil

//----------------------------------------------------------------------------//

static function CreateTag( cTag, cType )

#pragma /a+

   PRIVATE cExpr

   if cType == 'C'
      cExpr := "UPPER(" + Trim( cTag ) + ")"
      INDEX ON &cExpr TAG &cTag TO RECYCLE MEMORY ADDITIVE
   else
      INDEX ON &ctag TAG &ctag TO RECYCLE MEMORY ADDITIVE
   endif

return nil

//----------------------------------------------------------------------------//

function DBFAPPEND( lRecycleDeleted )

   local ord, ordName
   local lRecycled := .F.
   local nRecNo := 0
   local cAlias := Alias()
   local i := 0
   local lOk := .F.

   __defaultNIL( @lRecycleDeleted, .T. )

   if lRecycleDeleted

      WHILE ! Empty( ordName( ++i ) )

         if Upper( ordKey( i ) ) == "DELETED()" .or. Upper( ordFor( i ) ) == "DELETED()"
            ordName := ordName( i )
            lOk := .T.
            EXIT
         endif

      END

   endif

   lRecycleDeleted := lOK

   if lRecycleDeleted

      SET DELETED OFF

      ord := OrdSetFocus( ordName )

      if ( nRecNo := (cAlias)->( SeekDeleted() ) ) > 0

         GO nRecNo

         if (cAlias)->( dbInfo( DBI_SHARED ) )

            if (cAlias)->( NetRecall() )

               if NetRecLock()
                  for i:=1 to FCount()
                     if !( FieldType( i ) $ "+=" )
                        FieldPut( i, Blank( FieldGet( i ) ) )
                     endif
                  next
               endif

               lRecycled := .T.

            endif

         else

            (cAlias)->( dbRecall() )

            for i:=1 to FCount()
               if !( FieldType( i ) $ "+=" )
                  FieldPut( i, Blank( FieldGet( i ) ) )
               endif
            next

            lRecycled := .T.

         endif

      endif

      SET DELETED ON

      OrdSetFocus( ord )

   endif

   if lRecycled

      nRecNo := (cAlias)->( RecNo() )

   else

      if (cAlias)->( dbInfo( DBI_SHARED ) )

         if (cAlias)->( NetAppend() )
            nRecNo := (cAlias)->( RecNo() )
         endif

      else

         (cAlias)->( dbAppend() )
         nRecNo := (cAlias)->( RecNo() )

      endif

   endif

return nRecNo

//----------------------------------------------------------------------------//

static function SeekDeleted()

   local nDeletedRec := 0
   local nCurrentRec := RecNo()

   GO TOP
   IF Deleted()
      return( RecNo() )
   ENDIF

   IF dbSeek( .T. )

      IF Deleted()
         nDeletedRec := RecNo()
      ENDIF

   ENDIF

   GO nCurrentRec

return nDeletedRec
