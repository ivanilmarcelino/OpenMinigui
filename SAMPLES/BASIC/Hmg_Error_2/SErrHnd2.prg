#include <hmg.ch>

#define CRLF2 CRLF + CRLF

/*
   Simple Error Handling sample - 2.

   Description :

      This program inspired by ErrorSys.prg of Steve Straley.

      Thanks to him and Dilip Patel as introducer of program and helper to development .

      Error Handling is one of most important issue in programmer's work.

      I hope that will be useful to all xBase family, especially Harbour and HMG users.

   Thanks in advance.  

   Viva Harbour, Viva HMG.

   Bicahi Esgici

   July 2012

*/

MEMVAR bOldErrHandler
MEMVAR lMaintMode

MEMVAR cThisWSNam,;
       cPrgFNam,;
       cProcName,;
       cProcLine,;
       dErrrDate,;
       cErrrTime,;
       cLogFilNam,;
       cMsgExtLns,;
       lSeeLogFil

PROCEDURE SmpErrHandler02( oCurrentError )  // Simple Error handle procedure

   LOCAL cErrorMesage  := '',;
         nMsgExtResult := 0,;
         bInit

   // If an error occurs While local (new) error handler active, result may be problematic;
   // so at beginning of local (new) error handler first we terminate activation of new handler
   // and re-activate the old (standart). If an error occurse, standard (old) handler will run.

   // Return the previous (original) error handler

   ERRORBLOCK( { | oError | EVAL( bOldErrHandler, oError ) } )

   PRIVATE cThisWSNam := GetComputerName(),;  // name of this work station
           cPrgFNam   := '',;                 // Program file only name   
           cProcName  := PROCNAME( 2 ) ,;
           cProcLine  := PROCLINE( 2 ) ,;
           dErrrDate  := DATE(),;
           cErrrTime  := TIME(),;
           cLogFilNam := "ERR" + SUBSTR( DTOS( DATE() ), 3 ) + "_" + STRTRAN( TIME(), ":","" ) + ".LOG",;
           cMsgExtLns,;
           lSeeLogFil := .F.

   PlayExclamation()

   HB_MEMOWRIT( cLogFilNam, ErrorReport( oCurrentError ) )

   cMsgExtLns := "A Run-Time Error Has Occured;;"+;               // Message Lines
                 "All necessary information logged "+;
                 "to an errors file called;;"+;
                 cLogFilNam+;
                 ";;* * * CONTACT PROGRAMMER IMMEDIATELY * * *"

   SET MSGALERT BACKCOLOR TO MAROON
   SET MSGALERT FONTCOLOR TO WHITE

   DEFINE FONT DlgFont FONTNAME "Verdana" SIZE 16

   bInit := {|| AEval( HMG_GetFormControls( ThisWindow.Name, "LABEL" ), ;
          {|ctl| This.&(ctl).FontBold := .T., This.&(ctl).Alignment := "CENTER" } ), ;
          This.Say_03.FontSize := 12, ;
          This.Say_05.FontColor := YELLOW, ;
          This.Say_05.FontSize := 13, ;
          iif( Len( HMG_GetFormControls( ThisWindow.Name, "OBUTTON" ) ) == 1, ;
          This.Btn_01.Col := (This.Width / 2) - 10, ) }

   IF TYPE( "lMaintMode" ) == "L" .AND. lMaintMode
      cMsgExtLns += ";;Do you want to see the Log File now ?"
      IF AlertYesNo( cMsgExtLns, "Run-time error", .T., "ZZZ_B_STOP64", 64, { LGREEN, RED }, .T., bInit )
         EXECUTE FILE "NOTEPAD.EXE" PARAMETERS ( cLogFilNam )
      ENDIF   
   ELSE
      AlertInfo( cMsgExtLns, "Run-time error", "ZZZ_B_STOP64", 64, { LGREEN }, .T., bInit )
   ENDIF

   RELEASE FONT DlgFont

   AddErr2Log( oCurrentError )

   // Our local error handler reach to end; this means no problem occured.
   // So we can reactivate it.

   ERRORBLOCK( { | oError | SmpErrHandler02( oError  ) } )

RETURN // SmpErrHandler02()

*-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.

FUNCTION ErrorReport( oError )             // Assemble all reports     

   LOCAL cReport := PADC(" Error Log File ", 79, "*" ) + CRLF2 +;
                    SystemInfo() + ;
                    ProgramInfo() + ;
                    CRLF + ;
                    ErrrInfo( oError ) + ;   // Internal Error Information
                    CurSets() + ;            // Current settings
                    CRLF + ;
                    TablsWAs() + ;           // Tables and Work Area Items
                    MemVarInfo()             // Memory variables list  
                    
RETURN cReport // ErrorReport()

*-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.

FUNCTION SystemInfo()                     // System Info

   LOCAL cSysInfo := PADC(" System Information ", 79, "-" ) + CRLF2 ,;
         aSysInfo := { { "Date, Time", DTOC( DATE() ) + "  "  + TIME() },;
                       { "Computer", cThisWSNam },;
                       { "OS & Version", HL_OsVers() },;
                       { "Memory (MB)" },;
                       { "Total ",            HL_Any2Str( MemoryStatus( 1 ) ) },;     
                       { "Available",         HL_Any2Str( MemoryStatus( 2 ) ) },;     
                       { "Total page",        HL_Any2Str( MemoryStatus( 3 ) ) },;     
                       { "Available page",    HL_Any2Str( MemoryStatus( 4 ) ) },;     
                       { "Total Virtual",     HL_Any2Str( MemoryStatus( 5 ) ) },;
                       { "Available virtual", HL_Any2Str( MemoryStatus( 6 ) ) },;    
                       { "Current Drive",     HB_CurDrive() + ":" },;                       
                       { "Available Disk Space", HB_NTOS( DiskSpace() / 10^9 ) + " GB" },;
                       { "Current Folder", "\" + CURDIR() },;
                       { '' } }

   
   AEVAL( aSysInfo, { | a1 | cSysInfo += PADL( a1[ 1 ], 20 ) + ;
                                          IF( LEN( a1 ) < 2, "",;    
                                                              " : " + a1[ 2 ] ) + CRLF } )
RETURN  cSysInfo // SystemInfo()

*-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.

FUNCTION ProgramInfo()                    // Program Info
                      

   LOCAL cPrgOrig := ExeName(),;             // Program file full name 
         cPrgFold := '',;                    // Program file folder name 
         cPrgInfo := PADC(" Program Information ", 79, "-" ) + CRLF2,;   
         aPrgInfo := {}

   cPrgFold := LEFT( cPrgOrig, RAT( "\", cPrgOrig ) - 1 )
   
   cPrgFNam := SUBSTR( cPrgOrig, RAT( "\", cPrgOrig ) + 1 )     
   
   aPrgInfo := { { "Program File" },; 
                 { "Folder", cPrgFold },;                        
                 { "Name", cPrgFNam },;                        
                 { "Date", HL_Any2Str(  FILEDATE( cPrgOrig ) ) },;
                 { "Time", HL_Any2Str(  FILETIME( cPrgOrig ) ) },;
                 { "Size", HL_Any2Str(  FILESIZE( cPrgOrig ) ) } }                       
                 
   AEVAL( aPrgInfo, { | a1 | cPrgInfo += PADL( a1[ 1 ], 20 ) + ;
                                          IF( LEN( a1 ) < 2, "",;    
                                                              " : " + a1[ 2 ] ) + CRLF } )
RETURN  cPrgInfo // ProgramInfo()

*-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.

FUNCTION CurSets()                  // Current Settings
   
   LOCAL cReport_2 := PADC( " Current Settings ", 79, "-" ) + CRLF2,;
         aReport_2 := { { "Exact",           SET( _SET_EXACT )      },; 
                        { "Fixed",           SET( _SET_FIXED )      },;
                        { "Decimals",        SET( _SET_DECIMALS )   },;
                        { "Path",            SET( _SET_PATH )       },;
                        { "Default",         SET( _SET_DEFAULT )    },;
                        { "Epoch",           SET( _SET_EPOCH )      },;
                        { "Date format",     SET( _SET_DATEFORMAT ) },;
                        { "Alternate",       SET( _SET_ALTERNATE )  },;
                        { "Alt file",        SET( _SET_ALTFILE )    },;
                        { "Console",         SET( _SET_CONSOLE )    },;
                        { "Margin",          SET( _SET_MARGIN )     },;
                        { "Printer",         SET( _SET_PRINTER )    },;
                        { "Print file",      SET( _SET_PRINTFILE )  },;
                        { "Device",          SET( _SET_DEVICE )     },;
                        { "Bell",            SET( _SET_BELL )       },;
                        { "Delimiters",      SET( _SET_DELIMITERS ) },;
                        { "Delim chars",     SET( _SET_DELIMCHARS ) },;
                        { "Confirm",         SET( _SET_CONFIRM )    },;
                        { "Escape",          SET( _SET_ESCAPE )     },;
                        { "Intensity",       SET( _SET_INTENSITY )  },;
                        { "Scoreboard",      SET( _SET_SCOREBOARD ) },;
                        { "Wrap",            SET( _SET_WRAP )       },;
                        { "Message line",    SET( _SET_MESSAGE )    },;
                        { "Message center",  SET( _SET_MCENTER )    },;
                        { "Exclusive",       SET( _SET_EXCLUSIVE )  },;
                        { "Softseek",        SET( _SET_SOFTSEEK )   },;
                        { "Unique",          SET( _SET_UNIQUE )     },;
                        { "Deleted",         SET( _SET_DELETED )    } }
                        
   AEVAL( aReport_2, { | a1 | cReport_2 += PADL( a1[ 1 ], 20 ) + ;
                                           IF( LEN( a1 ) < 2, "",;    
                                             " : " + HL_Any2Str( a1[ 2 ] ) ) + CRLF } )
                     
RETURN cReport_2 // CurSets()

*-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.
 
FUNCTION TablsWAs()                       // Tables in use & Work Areas info

   LOCAL nWorkAreaNo :=  0,;
         aSTRUCT     := {},;
         cTablFFNam  := '',;   // Table full file name
         cTablFoldr  := '',;   // Table folder
         cTableName  := '',;   // Table folder
         nUsedWACo   :=  0     // Used Work area count
   
   LOCAL cWAStatus := PADC( " Tables in use & Work Areas info ", 79, "-" ) + CRLF2,;
         aWAStatus := {} 
   
      FOR nWorkAreaNo := 1 TO 64
      
         SELECT( nWorkAreaNo )
         
         IF !EMPTY( ALIAS( nWorkAreaNo ) )
         
            ++nUsedWACo
            
            cTablFFNam  := DBINFO( 10 )
            cTablFoldr  := IF( "\" $ cTablFFNam, ;
                              LEFT( cTablFFNam, RAT( "\", cTablFFNam ) ) -1,;
                              GetCurrentFolder() )
            cTableName  := SUBSTR( cTablFFNam, RAT( "\", cTablFFNam ) +1 )
            
            aSTRUCT     := DBSTRUCT()

            aWAStatus := { { 'Work area no',     SELECT()               },;
                           { 'RDD Name',         RDDNAME()              },;
                           { 'Table Extention',  DBINFO( 9 )            },;
                           { 'Bag Extention',    ORDBAGEXT()            },;
                           { 'Table Folder',     cTablFoldr             },; 
                           { 'Table Name',       cTableName             },; 
                           { "Date",             FILEDATE( cTablFFNam ) },;
                           { "Time",             FILETIME( cTablFFNam ) },;
                           { "Size",             FILESIZE( cTablFFNam ) },;                       
                           { 'Alias',            ALIAS()                },; 
                           { 'Last Update',      LUPDATE()              },; 
                           { 'Header Size',      HEADER()               },;
                           { 'Field Count',      FCOUNT()               },;
                           { 'Record Size',      RECSIZE()              },;
                           { 'Record Count',     LASTREC()              },;
                           { 'Current RecNo',    RECNO()                },;
                           { 'Current Order',    INDEXORD()             },;
                           { 'Current filter',   DBFILTER()             },;
                           { 'Relation exp',     DBRELATION()           },; 
                           { 'Index order',      INDEXORD()             },;
                           { 'Active Index Key', INDEXKEY( INDEXORD() ) },;
                           {''},;
                           { 'Current Record' } }
                           
            AEVAL( aWAStatus, { | a1 | cWAStatus += PADL( a1[ 1 ], 20 ) + ;
                                           IF( LEN( a1 ) < 2, "",;    
                                             " : " + HL_Any2Str( a1[ 2 ] ) ) + CRLF } )
                           
            AEVAL( aSTRUCT, { | a1, i1 | cWAStatus += SPACE( 10 ) + ;
                                             PADL( a1[ 1 ], 12 ) + " : " + ;
                                             HL_Any2Str( FIELDGET( i1 ), a1[ 3] )  + CRLF } )
                                                  
            cWAStatus += CRLF + REPLICATE( "¨", 79 ) + CRLF2 
            
         ENDIF
         
      NEXT 
      
      IF EMPTY( nUsedWACo )
         cWAStatus := PADC( "No work area in use.", 79, "-" ) + CRLF2
      ENDIF   
      
RETURN cWAStatus // TablsWAs()

*-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.

FUNCTION ErrrInfo(;                       // Error Information
                   oError )
                        
   LOCAL aErrSeverities := { 'No Problem',;         //  0
                             'Warning',;            //  1
                             'General Error',;      //  2
                             'Fatal Error' },;      //  3
         aGenErrCodes   := { 'Argument Error',;     //  1
                             'Bound Error',;        //  2
                             'String Overflow',;    //  3
                             'Numeric Overflow',;   //  4
                             'Zero Division',;      //  5
                             'Numeric Error',;      //  6
                             'Syntax Error',;       //  7
                             'Complexity Error',;   //  8
                             'Reserved',;           //  9
                             'Reserved',;           // 10
                             'Memory Error',;       // 11
                             'No function',;        // 12
                             'No Method',;          // 13
                             'No Variable',;        // 14
                             'No Alias',;           // 15
                             'No Varable Method',;  // 16
                             'Bad Alias',;          // 17
                             'Duplicate Alias',;    // 18
                             'Reserved',;           // 19
                             'Create Error',;       // 20
                             'Open Error',;         // 21
                             'Close Error',;        // 22
                             'Read Error',;         // 23
                             'Write Error',;        // 24
                             'Print Error',;        // 25
                             'Reserved',;           // 26
                             'Reserved',;           // 27
                             'Reserved',;           // 28
                             'Reserved',;           // 29
                             'Unsupported Item',;   // 30
                             'Limit Error',;        // 31
                             'Corruption',;         // 32
                             'Datatype Error',;     // 33
                             'Datawidth Error',;    // 34
                             'No table',;           // 35
                             'No Order',;           // 36
                             'Shared',;             // 37
                             'Unlocked',;           // 38
                             'Readonly Error',;     // 39
                             'Append Lock Error',;  // 40
                             'Lock Error',;         // 41
                             'Reserved',;           // 42
                             'Reserved',;           // 43
                             'Reserved',;           // 44
                             'Destructor Error',;   // 45
                             'Array Access Err.',;  // 46
                             'Array Assign Err.',;  // 47
                             'Array Dim. Err.',;    // 48
                             'Not Array',;          // 49
                             'Condition Error' }    // 50
          
   LOCAL cErrorInfo := PADC(" Error Information ",79,"-") + CRLF +;
                       CRLF,;
         aErrorInfo := { { 'Error',       oError:Description   },;
                         { "Break Point", HL_CallSequence( 5 ) },;
                         { 'Sub Code',       oError:SubCode       },;
                         { 'Can Substitute', oError:CanSubstitute },;
                         { 'Can Default',    oError:CanDefault    },;
                         { 'Can Retry',      oError:CanRetry      },;
                         { 'Description',    oError:Description   },;
                         { 'Operation',      oError:Operation     },;
                         { 'Filename',       oError:FileName      },;
                         { 'OS Code',        oError:OSCode        },;
                         { 'Generic Code',   HL_Any2Str( oError:GenCode ) + " : " + ;
                                             aGenErrCodes[ oError:GenCode ] },;
                         { 'Severity',       HL_Any2Str( oError:Severity ) + " : " + ;     
                                             aErrSeverities[ oError:Severity + 1 ] },;
                         { '' } }
                         
                           
   AEVAL( aErrorInfo, { | a1 | cErrorInfo += PADL( a1[ 1 ], 20 ) + ;
                                           IF( LEN( a1 ) < 2, "",;    
                                             " : " + HL_Any2Str( a1[ 2 ] ) ) + CRLF } )

RETURN cErrorInfo // ErrrInfo()
      
*-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.

PROCEDURE AddErr2Log( ;                    // Add error info to log file (table)          
                      oError )  

   LOCAL cLTbFnam := "ERRRLOGG.dbf",;                // LOG table (.dbf) file name
         aLTbFStr := { { "WS_NAME",  "C", 20, 0 },;   // Name of work station
                       { "PROGNAME", "C", 20, 0 },;   // Name of program
                       { "PROCNAME", "C", 20, 0 },;   // Name of Procedure
                       { "PROCLINE", "N",  6, 0 },;   // Line no in Procedure
                       { "ERR_DATE", "D", 10, 0 },;   // Date at error occured
                       { "ERR_TIME", "C",  8, 0 },;   // Time  at error occured
                       { "DESCRIPT", "C", 32, 0 },;   // Description of Error
                       { "SUB_CODE", "N",  6, 0 },;   // Sub Code
                       { "OPERATIO", "C", 32, 0 },;   // Operation
                       { "FILENAME", "C", 32, 0 },;   // File Name                       
                       { "OS_CODE",  "N",  6, 0 },;   // OS Code
                       { "GEN_CODE", "N",  6, 0 },;   // Generic Code
                       { "SEVERITY", "N",  6, 0 },;   // Severity
                       { "REPRFILE", "C", 32, 0 },;   // Name Error Report File
                       { "COMMENTS", "C", 64, 0 } }   // Anything to record come here ( will assign by programmer )
                       
   IF ! FILE( cLTbFnam )
      DBCREATE( cLTbFnam, aLTbFStr )
   ENDIF   
    
   USE ( cLTbFnam ) NEW ALIAS ELOG
   
   IF ALIAS() = "ELOG"
      APPEND BLANK
      REPLACE WS_NAME  WITH cThisWSNam ,;
              PROGNAME WITH cPrgFNam   ,;
              PROCNAME WITH cProcName  ,;
              PROCLINE WITH cProcLine  ,;
              ERR_DATE WITH dErrrDate  ,;
              ERR_TIME WITH cErrrTime  ,;
              SUB_CODE WITH oError:SubCode ,;
              OPERATIO WITH oError:Operation ,;
              FILENAME WITH oError:FileName  ,;
              OS_CODE  WITH oError:OSCode    ,;
              GEN_CODE WITH oError:GenCode   ,;
              SEVERITY WITH oError:Severity  ,;
              REPRFILE WITH cLogFilNam ,;
              COMMENTS WITH "Anything to record come here"
      USE                               
   ELSE
      MsgStop( "Somethings wrong;" + CRLF + ;
               "Error info didn't recorded.", "Error ! " )
   ENDIF               
    
RETURN // AddErr2Log()

*-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.

FUNCTION MemVarInfo()

   LOCAL aMemVars := {},;
         cMemFNam := "MEMVARS.MEM",;   
         CResult  := PADC(" Memory  Variable List ",79,"-") + CRLF +;
                     CRLF + ;
                     PADC(" ( Public and Private Variables ) ",79) + CRLF +;
                     CRLF + ;
                     "    Variable Name T Width Dec Value" + CRLF +;
                     "    ------------- - ----- --- " + REPLICATE( "-", 46 ) + CRLF,;
         c1RLine  := '',;
         a1Var    := {},;
         c1VName  := '',;
         c1VType  := '',;
         nActLen  :=  0,;
         n1VDeci  :=  0,;
         x1VValu

   SAVE ALL EXCEPT _* TO ( cMemFNam ) // MEMVARS
   
   aMemVars := HL_MemVarList( cMemFNam )

   FOR EACH a1Var IN  aMemVars
   
      c1VName := a1Var[ 1 ] 
      c1VType := a1Var[ 2 ] 
      nActLen := a1Var[ 3 ] 
      n1VDeci := a1Var[ 4 ] 
      x1VValu := a1Var[ 5 ] 
   
      c1RLine := SPACE( 4 ) +;              // Left marj
                 PAD( c1VName, 14 ) +;      // Variable name
                 c1VType +;                 // Variable type
                 STR( nActLen, 6 ) + ;      // Actual length
                 STR( n1VDeci, 4 ) + ;      // Variable decimal
                 " [" + HL_Any2Str( x1VValu )+ "]" + CRLF   // Recorded value, enclosed by brackets   
      
      CResult += c1RLine
      
   NEXT
   
   FERASE( ( cMemFNam )  ) // For inspect .mem file, rem this line
   
   CResult += "    ------------- - ----- --- " + REPLICATE( "-", 46 ) +;
              CRLF2 + PADC(" End of Error Report ",79, "*" ) + CRLF
   
RETURN cResult // MemVarInfo()

*-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.-._.
