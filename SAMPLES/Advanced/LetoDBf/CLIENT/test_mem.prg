/*
 * This sample tests working with dbf files
 * Just change the cPath value to that one you need.
 */

/* .T. or .F. */
#define __DELAYED_ERROR__  .T.

//#define __MEM_IO__   1
//#define __LZ4__      1
// #define __AUTOINC__  1

/* activate for transaction test */
// #define __TRANSACT__ 1

#define __RANDOM__   1

/* 341 FILED_IDS' == 2046 !max! fields  *  5000 ROUNDS = ~ 4 GB */
#define FIELD_IDS    1
#define ROUNDS       10000

#ifdef __XHARBOUR__
   #define hb_milliseconds   LETO_MILLISEC
   #define hb_BLen( cTest )  LEN( cTest )
#endif

//REQUEST DBORDERINFO, ORDLISTCLEAR, ORDBAGCLEAR, ORDDESTROY
//REQUEST LETO
//REQUEST DBFNTX

#define _HMG_OUTLOG

#include "hmg.ch"
#include "rddleto.ch"
#include "dbinfo.ch"
#include "set.ch"

 FIELD NAME, NUM, INFO, DINFO, MINFO
 FIELD INTEGER, AUTOINC

Function Test_mem( oWnd, cPath, nMode )
 LOCAL nRounds := ROUNDS
 LOCAL aNames := { "Petr", "Ivan", "Alexander", "Pavel", "Alexey", "Fedor", ;
                   "Konstantin", "Vladimir", "Nikolay", "Andrey", "Dmitry", "Sergey" }
 LOCAL i, ii, aStru
 LOCAL nPort := 2812
 LOCAL nHotbuf := 100
 LOCAL nKey, nSec
 LOCAL cFile1 := "test1"
 LOCAL cFile2 := "test2"
 LOCAL nDeleted
 LOCAL aStruct, aStructAll := {}
 LOCAL aRandom, iii, nCnt := 0
 LOCAL __AUTOINC__  
 LOCAL __TRANSACT__ 
 LOCAL __MEM_IO__   
 LOCAL __LZ4__   

 DEFAULT nMode := 1

 oWnd:SendMsg(20)
 oWnd:Action := .F.
 fErase('_Msglog.txt')

 ? Procname()+'(', nMode, ')'
 ? repl('=', len(Procname())+20)

 If nMode == 1
    __AUTOINC__  := 1
    ? "__AUTOINC__"
 ElseIf nMode == 2
    __TRANSACT__ := 1
    ? "__TRANSACT__"
 ElseIf nMode == 3
    __AUTOINC__  := __TRANSACT__ := 1
    ? "__AUTOINC__+__TRANSACT__"
 ElseIf nMode == 4
    __AUTOINC__  := __MEM_IO__   := 1
    ? "__MEM_IO__+__AUTOINC__"
 ElseIf nMode == 5
    __TRANSACT__ := __MEM_IO__   := 1
    ? "__MEM_IO__+__TRANSACT__"
 ElseIf nMode == 6
    __AUTOINC__  := __TRANSACT__ := __MEM_IO__   := 1
    ? "__MEM_IO__+__AUTOINC__+__TRANSACT__"
 ElseIf nMode == 7
    __LZ4__ := __AUTOINC__  := 1
    ? "__LZ4__+__AUTOINC__"
 ElseIf nMode == 8
    __LZ4__ := __TRANSACT__ := 1
    ? "__LZ4__+__TRANSACT__"
 ElseIf nMode == 9
    __LZ4__ := __AUTOINC__  := __TRANSACT__ := 1
    ? "__LZ4__+__AUTOINC__+__TRANSACT__"
 ElseIf nMode == 10
    __LZ4__ := __AUTOINC__  := __MEM_IO__   := 1
    ? "__LZ4__+__MEM_IO__+__AUTOINC__"
 ElseIf nMode == 11
    __LZ4__ := __TRANSACT__ := __MEM_IO__   := 1
    ? "__LZ4__+__MEM_IO__+__TRANSACT__"
 ElseIf nMode == 12
    __LZ4__ := __AUTOINC__  := __TRANSACT__ := __MEM_IO__   := 1
    ? "__LZ4__+__MEM_IO__+__AUTOINC__+__TRANSACT__"
 EndIf

 __AUTOINC__  := !Empty( __AUTOINC__  )
 __TRANSACT__ := !Empty( __TRANSACT__ )
 __MEM_IO__   := !Empty( __MEM_IO__   )
 __LZ4__      := !Empty( __LZ4__      )

//   SETMODE( 25, 80 )
//   SET DATE FORMAT "dd/mm/yy"
//   ALTD()

   IF Empty( cPath )
      cPath := ""
      RDDSETDEFAULT( "DBFCDX" )
   ELSE
      cPath := "//" + cPath + IiF( ":" $ cPath, "", ":" + ALLTRIM( STR( nPort ) ) )
      cPath += Iif( Right(cPath,1) == "/", "", "/" )
      RDDSETDEFAULT( "LETO" )
   ENDIF

   IF RDDSETDEFAULT() == "LETO"
      IF EMPTY( cPath ) .OR. leto_Connect( cPath, /*user*/, /*pass*/, /*timeout*/, nHotBuf, __DELAYED_ERROR__ ) == -1
//         ALERT("NO LETODB SERVER FOUND" )
//         QUIT
         ? "NO LETODB SERVER FOUND"
         oWnd:Action := .T.
         oWnd:PostMsg(21)
         DO EVENTS
         RETURN
      ELSE
         ? LETO_GetServerVersion()
         if __MEM_IO__
            IF ! ".2." $ LETO_GetServerVersion()
               cFile1 := "mem:" + cFile1
               cFile2 := "mem:" + cFile2
            ENDIF
         endif
         if __LZ4__
            LETO_TOGGLEZIP( 1 )
         Else
            LETO_TOGGLEZIP( 0 )
         endif
         ? "NETWORK TRAFFIC COMPRESSION:", Iif( LETO_TOGGLEZIP() > 0, "ON", "OFF" )
      ENDIF
   ELSE
      if __MEM_IO__
         cFile1 := "mem:" + cFile1
         cFile2 := "mem:" + cFile2
      endif
   ENDIF

   ? cFile1, cFile2

   IF ! DbExists( cFile1 )

      if __AUTOINC__
         aStruct := { { "NAME",    "C",  10, 0 },;
                      { "NUM",     "N",   4, 0 },;
                      { "INFO",    "C",  32, 0 },;
                      { "DINFO",   "D",   8, 0 },;
                      { "TINFO",   "@",   8, 0 },;
                      { "MINFO",   "M",  10, 0 },;
                      { "INTEGER", "I", 8, 0 },;
                      { "AUTOINC", "+",   4, 0 },;
                      { "CURRENCY","Y",   8, 4 } }
      else
         aStruct := { { "NAME",  "C", 10, 0 },;
                      { "NUM",   "N",  4, 0 },;
                      { "INFO",  "C", 64, 0 },;
                      { "DINFO", "D",  8, 0 },;
                      { "TINFO", "T",  4, 0 },;
                      { "MINFO", "M", 10, 0 } }
      endif

      oWnd:StatusBar:Say(hb_ntos(++nCnt), 2)

      FOR i := 1 TO FIELD_IDS
         FOR ii := 1 TO LEN( aStruct )
            AADD( aStructAll, ACLONE( aStruct[ ii ] ) )
            IF i > 1
               aStructAll[ ( ( i - 1 )  * 6 ) + ii, 1 ] := ALLTRIM( aStruct[ ii, 1 ] ) + hb_ntos( i )
            ENDIF
         NEXT ii
      NEXT i
      IF dbCreate( cFile1, aStructAll )
         ? "File " + cFile1 + hb_rddInfo( RDDI_TABLEEXT ) + " has been created"
      ELSE
//         ALERT( "DBF CREATE FAILED" + IIF( NetErr(), ", TABLE IN USE BY OTHER", "" ) )
//         QUIT
         ? "DBF CREATE FAILED" + IIF( NetErr(), ", TABLE IN USE BY OTHER", "" )
         oWnd:Action := .T.
         oWnd:PostMsg(21)
         DO EVENTS
         RETURN
      ENDIF
   ENDIF

   USE ( cFile1 ) SHARED NEW
   IF ! NetErr() .AND. ! EMPTY( ALIAS() )
      ? "File " + cFile1 + hb_rddInfo( RDDI_TABLEEXT ) + " have been opened shared"
      IF Leto_IsErrOptim()
         ?? " (optimized)"
      ENDIF
   ELSE
      ? "ERROR opening database! -- press any key to quit"
//      Inkey( 0 )
//      QUIT
      oWnd:Action := .T.
      oWnd:PostMsg(21)
      DO EVENTS
      RETURN
   ENDIF

   aStru := dbStruct()
   FOR i := 1 TO Len( aStru )
      ? i, PadL( aStru[ i, 1 ], 10 ), PadR( aStru[ i, 2 ], 7 ), STR( aStru[ i, 3 ], 5, 0 ), aStru[ i, 4 ]
   NEXT

   oWnd:StatusBar:Say(hb_ntos(++nCnt), 2)

   if __TRANSACT__
      ? "with transaction"
   else
      ?
   endif
   IF RecCount() == 0
#ifdef __RANDOM__
      /* create a random set of 99 record numbers for special action */
      aRandom := {}
      i := 0
      DO WHILE i < 99
         oWnd:StatusBar:Say(hb_ntos(++nCnt), 2)
         ii := INT( hb_random() * ( nRounds * LEN( aNames ) ) )
         IF ASCAN( aRandom, ii ) < 1
            AADD( aRandom, ii )
            i++
         ENDIF
         DO EVENTS
      ENDDO
#endif
      ? "please wait, appending " + hb_ntos( nRounds * LEN( aNames ) ) + " records ..."
      RddInfo( RDDI_REFRESHCOUNT, .F. )
      nSec := hb_milliseconds()

      IF FLock()
         if __TRANSACT__
            leto_BeginTransaction()
         endif
         if __AUTOINC__
            DbFieldInfo( DBS_STEP, 7, 10 )
         endif
         FOR ii := 1 TO nRounds
             If ii % 10 == 0
                oWnd:StatusBar:Say(hb_ntos(ii), 2)
            EndIf
            FOR i := 1 TO Len( aNames )
               DO EVENTS
               IF DbAppend()
                  /* Harbour returns .T. if successful, else alike for: APPEND BLANK, check for NetErr() before proceeding */
                  REPLACE NAME  WITH aNames[ i ],;
                          NUM   WITH i + 1000, ;
                          INFO  WITH "This is a record number "+ Ltrim( Str( i ) ),;
                          DINFO WITH Date() + i - 1, ;
                          MINFO WITH "elk test" + STR( i, 10, 0 )
#ifdef __RANDOM__
                  iii := ASCAN( aRandom, RECNO() )
                  IF( iii > 0 )
                     REPLACE INFO WITH "wild  Strick" + IIF( iii < 10, "0", "" ) + hb_ntos( iii ) + "land match, Alex"
                  ENDIF
#endif
               if __AUTOINC__
                  REPLACE INTEGER WITH ( ii * 1000 ) + i //,;
//                          CURRENCY WITH 1234
               endif
               ENDIF
            NEXT i
            DO EVENTS
         NEXT ii
         if __TRANSACT__
            leto_CommitTransaction()
         else
            DbUnlock()
         endif
         DO EVENTS
      ENDIF

      ?? STR( ( hb_milliseconds() - nSec ) / 1000, 6, 2 ), "s -- "
      ?? STR( ( Len( aNames ) * nRounds ) / ( ( hb_milliseconds() - nSec ) / 1000 ), 7, 0 ), "/ s"
      if __TRANSACT__
         DbUnLock()
      endif
      IF RDDSETDEFAULT() == "LETO"
         ? "file size: ", Leto_FileSize( cFile1 + hb_rddInfo( RDDI_TABLEEXT ) ) / 1024 / 1024, "MB; record length " +;
                          hb_ntos( dbInfo( DBI_GETRECSIZE ) )
      ELSE
         ? "file size: ", hb_FSize( cFile1 + hb_rddInfo( RDDI_TABLEEXT ) ) / 1024 / 1024, "MB"
      ENDIF
      oWnd:StatusBar:Say(hb_ntos(++nCnt)+' Index 1', 2)
      DO EVENTS
      nSec := hb_milliseconds()
      INDEX ON NAME TAG NAME TO ( cFile1 )
      ? ( hb_milliseconds() - nSec ) / 1000, "s, INDEX KEY 1:", IIF( indexord() == 1, "(ok)", "(fail)" ), indexkey( 1 )
      oWnd:StatusBar:Say(hb_ntos(++nCnt)+' Index 2', 2)
      DO EVENTS
      nSec := hb_milliseconds()
      INDEX ON Str(NUM,4) TAG NUMS TO ( cFile2 ) ADDITIVE
      ? ( hb_milliseconds() - nSec ) / 1000, "s, INDEX KEY 2:", IIF( indexord() == 2, "(ok)", "(fail)" ), indexkey( 2 )
      ? "File has been indexed"
      oWnd:StatusBar:Say(hb_ntos(++nCnt), 2)
      DO EVENTS
   ELSE
      IF RDDSETDEFAULT() == "LETO"
         leto_Commit()
         ? RecCount(), "records, file size: ", Leto_FileSize( cFile1 + hb_rddInfo( RDDI_TABLEEXT ) ) / 1024 / 1024, "MB"
      ELSE
         dbCommit()
         ? RecCount(), "records, file size: ", hb_FSize( cFile1 + hb_rddInfo( RDDI_TABLEEXT ) ) / 1024 / 1024, "MB"
      ENDIF
      IF ! DbExists( cFile1 + hb_rddInfo( RDDI_ORDBAGEXT ) )
         oWnd:StatusBar:Say(hb_ntos(++nCnt)+' Index 1', 2)
         DO EVENTS
         INDEX ON NAME TAG NAME TO ( cFile1 )
      ENDIF
      IF ! DbExists( cFile2 + hb_rddInfo( RDDI_ORDBAGEXT ) )
         oWnd:StatusBar:Say(hb_ntos(++nCnt)+' Index 2', 2)
         DO EVENTS
         INDEX ON Str(NUM,4) TAG NUMS TO ( cFile2 )
      ENDIF
      IF EMPTY( IndexKey( 1 ) )  /* auto open */
         DbSetIndex( cFile1 )
      ENDIF
      DbSetIndex( cFile2 )
      oWnd:StatusBar:Say(hb_ntos(++nCnt), 2)
      DO EVENTS
   ENDIF

   if __AUTOINC__
   DbGoBottom()
   IF AUTOINC == RECNO() .AND. IIF( VALTYPE( DbFieldInfo( DBS_STEP, 7 ) ) == "N",;
                                    INTEGER == ( 10 * RECNO() ) - DbFieldInfo( DBS_STEP, 7 ) + 1, .T. )
      ? "AutoIncrement fine"
   ELSE
      ? "AutoIncrement fail"
   ENDIF
   ?? "; next for autoinc: ", DbFieldInfo( DBS_COUNTER, 7 ), "integer; ", DbFieldInfo( DBS_COUNTER, 8 )
   endif


/* --- benchmarks --- */

   oWnd:StatusBar:Say(hb_ntos(++nCnt), 2)
   /* test diffeent skipbuffer sizes from 21 to 525 */
   ? "skipping top to bottom ..."
   FOR i := 1 TO 5
     LETO_SETSKIPBUFFER( 21 * i * i )
     DO EVENTS
     nSec := hb_milliseconds()
      DbGoTop()
      DO WHILE ! EOF()
         If RECNO() % 100 == 0
            oWnd:StatusBar:Say(hb_ntos(RECNO())+' Skipping', 2)
         EndIf
         DO EVENTS
         DbSkip( 1 )
      ENDDO
      ?? STR( ( hb_milliseconds() - nSec ) / 1000, 7,2 ), "s;"
   NEXT i
   LETO_SETSKIPBUFFER( 21 )
   oWnd:StatusBar:Say(hb_ntos(++nCnt), 2)
   DO EVENTS

#ifdef __RANDOM__
   ? "skipping wildmatch_i filter 10x top->bottom "
   /* CB not needed for server side filter, only used if server can't evaluate condition */
   DbSetFilter( {|| HB_WILDMATCHi( "*strick??lanD*", INFO ) },;
                "HB_WILDMATCHi( " + CHR( 34 ) + "*strick??lanD*" + CHR( 34 ) + ", INFO )" )
   IF( LETO_ISFLTOPTIM() )
      ?? "( optimized ) "
   ENDIF
   ii := 0
   nSec := hb_milliseconds()
   FOR i := 1 TO 10
      DbGoTop()
      DO WHILE ! EOF()
         If RECNO() % 100 == 0
            oWnd:StatusBar:Say(hb_ntos(RECNO())+' Wildmath filter', 2)
         EndIf
         DO EVENTS
         IF "Alex" $ INFO
            ii++
         ENDIF
         DbSkip( 1 )
      ENDDO
      oWnd:StatusBar:Say(hb_ntos(++nCnt), 2)
      DO EVENTS
   NEXT i
   ?? STR( ( hb_milliseconds() - nSec ) / 1000, 5,2 ), "s ", STR( ii / 10, 3, 0 ), "found"
   DbClearFilter()
#else
   ? "GoTop, seek ...                                    "
   DbSetorder( 1 )
   nSec := hb_milliseconds()
   DbGoTop()
   ii := LEN( aNames )
   FOR i := 1 TO 10000
      DbGoTop()
      DO EVENTS
      IF ! DbSeek( aNames[ ( i % ii ) + 1 ] )
         EXIT
      ENDIF
      If i % 10 == 0
         oWnd:StatusBar:Say(hb_ntos(i)+' Seek', 2)
      EndIf
   NEXT i
   ?? STR( i - 1, 7, 0 ), "times ", STR( ( hb_milliseconds() - nSec ) / 1000, 7, 2 ), "s"
#endif

   IF FLock()
      if __TRANSACT__
         leto_BeginTransaction()
      endif
      ? "FILE locked, skip through records, data change two fields    ...  "
      oWnd:StatusBar:Say(hb_ntos(++nCnt), 2)
      DO EVENTS
      nSec := hb_milliseconds()
      DbGoTop()
      DO WHILE ! EOF()
         If RECNO() % 100 == 0
            oWnd:StatusBar:Say(hb_ntos(RECNO())+' Replace', 2)
         EndIf
         DO EVENTS
         Replace INFO  WITH "This is a record number " + hb_ntos( RECNO() ),;
                 DINFO WITH DATE(),;
                 MINFO WITH "elk tested" + STR( RECNO(), 10, 0 )
         DbSkip( 1 )
      ENDDO
      ?? STR( ( hb_milliseconds() - nSec ) / 1000, 7, 2 ), "s"
      if __TRANSACT__
         leto_CommitTransaction()
      else
         DbUnLock()
      endif
      oWnd:StatusBar:Say(hb_ntos(++nCnt), 2)
      DO EVENTS
   ENDIF

   ? "record locking, skip through records, data change two fields ...  "
   nSec := hb_milliseconds()
   DbGoTop()
   if __TRANSACT__
      leto_BeginTransaction()
   endif
   DO WHILE ! EOF()
      DO EVENTS
      IF RLock()
         Replace INFO  WITH "This is a record number " + hb_ntos( RECNO() ),;
                 DINFO WITH DATE()
         if ! __TRANSACT__
            DbUnLock()
         endif
      ENDIF
      If RECNO() % 100 == 0
         oWnd:StatusBar:Say(hb_ntos(RECNO())+' Replace DINFO', 2)
      EndIf
      DbSkip( 1 )
   ENDDO
   if __TRANSACT__
      leto_CommitTransaction()
   endif
   ?? STR( ( hb_milliseconds() - nSec ) / 1000, 7, 2 ), "s"

   ? "skipping from top to bottom, delete every 3rd record         ...  "
   if __TRANSACT__
      leto_BeginTransaction()
   endif
   nSec := hb_milliseconds()
   DbGoTop()
   nDeleted := 0
   DO WHILE ! EOF()
      DO EVENTS
      IF RECNO() % 3 == 0 .AND. RLock()
         DELETE
         if ! __TRANSACT__
            DbUnlock()
         endif
         nDeleted++
         oWnd:StatusBar:Say(hb_ntos(RECNO())+' Delete', 2)
      ENDIF
      DbSkip( 1 )
   ENDDO
   DO EVENTS
   if __TRANSACT__
      leto_CommitTransaction()
   endif
   ?? STR( ( hb_milliseconds() - nSec ) / 1000, 7, 2 ), "s"
   DO EVENTS

   ? "verify deleted record ..."
   ii := 0
   nSec := hb_milliseconds()
   DbGoTop()
   DO WHILE ! EOF()
      IF DELETED()
         ii++
         IF ii % 3 == 0
            oWnd:StatusBar:Say(hb_ntos(RECNO())+' Verify deleted', 2)
         ENDIF
      ENDIF
      DO EVENTS
      DbSkip( 1 )
   ENDDO
   ?? STR( ( hb_milliseconds() - nSec ) / 1000, 7, 2 ), "seconds", IIF( ii == nDeleted, "( Ok )", "FAIL!" )
   oWnd:StatusBar:Say(hb_ntos(++nCnt), 2)

   ? "( amount of buffered SKIPs: " + STR( LETO_SETSKIPBUFFER(), 10, 0 ) + " )"
   if __MEM_IO__
   IF RDDSETDEFAULT() == "LETO"
      ? "determine maximum request-rate ... "
      ii := 0
      nSec := hb_milliseconds()
      DO WHILE ii < 150000
         IF leto_Ping()
            ii++
         ENDIF
         If ii % 100 == 0
            oWnd:StatusBar:Say(hb_ntos(ii)+' Ping', 2)
         EndIf
         DO EVENTS
      ENDDO
      ?? STR( ii / ( ( hb_milliseconds() - nSec ) / 1000 ), 9, 2 ), "request/ s"
   ENDIF
   endif
   oWnd:StatusBar:Say(hb_ntos(++nCnt), 2)

   ?
   ? "Press ENTER to delete test DBF, any other key to finish, ..."
//   nKey := Inkey( 0 )
   nKey := 13

   DBCLOSEALL()

   IF nKey == 13
      hb_dbDrop( cFile1 + hb_rddInfo( RDDI_TABLEEXT ) )
      hb_dbDrop( cFile1 + hb_rddInfo( RDDI_ORDBAGEXT ) )
      hb_dbDrop( cFile2 + hb_rddInfo( RDDI_ORDBAGEXT ) )
      IF hb_dbexists( cFile1 + hb_rddInfo( RDDI_TABLEEXT ) ) .OR.;
         hb_dbExists( cFile1 + hb_rddInfo( RDDI_ORDBAGEXT ) ) .OR.;
         hb_dbExists( cFile2 + hb_rddInfo( RDDI_ORDBAGEXT ) )
         ? "drop dbf: - Failure "
      ELSE
         ? "drop dbf: -  Ok"
      ENDIF

      ?
      ? "Press any key to finish ..."
//      INKEY( 0 )
   ENDIF

   IF RDDSETDEFAULT() == "LETO"
      leto_Disconnect()
   ENDIF

   oWnd:Action := .T.
   oWnd:SendMsg(21)
   DO EVENTS

   fErase('_Msglog.txt')

Return Nil
