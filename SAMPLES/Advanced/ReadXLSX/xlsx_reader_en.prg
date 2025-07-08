* :encoding=UTF-8:    ąćęłńóśżźĄĆĘŁŃÓŚŻŹ

#include "hmg.ch"
#include "hbextcdp.ch"

Function Main

Set Decimal To 18
SET DATE TO ANSI
SET Century ON

DEFINE WINDOW Form_1 ;
    AT 0,0 ;
    WIDTH 1100 ;
    HEIGHT 660 ;
    TITLE 'Reading xlsx files' ;
    MAIN 
    
    DEFINE MAIN MENU
        DEFINE POPUP 'File'
            MENUITEM 'Open xlsx file (show cell values only)'              ACTION Read_xlsx ( 0 )
            MENUITEM 'Open xlsx file (show styles, formulas, cell values)' ACTION Read_xlsx ( 1 )
            SEPARATOR
            MENUITEM 'Exit' ACTION ThisWindow.Release    
        END POPUP
    END MENU
    
    DEFINE STATUSBAR
        STATUSITEM "" WIDTH Form_1.Width / 2
        STATUSITEM "" WIDTH Form_1.Width / 2 
    END STATUSBAR

    ShowProgressBar( 1, , , (Form_1.Width / 2) + 5 , Form_1.Width / 2 - 30 )        //init progress bar
    ShowProgressBar( 0 )                                                            //hide progress bar
        
END WINDOW

CENTER WINDOW Form_1
ACTIVATE WINDOW Form_1

Return Nil

**********************
Procedure Read_xlsx ( nMode )

Local i, aListOfSheets, oExcel 

Local cXlsxFile := GetFile ( {{'xlsx files','*.xlsx'}}, "Open xlsx file" , hb_CWD(), .F. , .T. )

Default nMode := 0

IF Empty ( cXlsxFile ) .Or. .Not. File ( cXlsxFile )
    Return
ENDIF

IF IsControlDefined ( Grid_1, Form_1 )
    Form_1.Grid_1.Release
ENDIF

IF IsControlDefined ( Sheets_1, Form_1 )
    Form_1.Sheets_1.Release
ENDIF

Form_1.StatusBar.Item(1) := "Reading the list of sheets ..."

ShowProgressBar( 2 )        //show progress bar

oExcel:= SmallXLSXReader():Open( cXlsxFile, { | cFile, i, nFiles | ( Form_1.PBar.Value := i / nFiles * 100, doEvents() ) } , { | j, n | Form_1.PBar.Value := j / n * 100 }, nMode <> 0 )

aListOfSheets := oExcel:WorkSheetList ()
//msgdebug ( aListOfSheets ) 

DEFINE TAB Sheets_1 OF Form_1 AT 559, 10 WIDTH 1050 HEIGHT 20 VALUE 1 ON CHANGE { || LoadSheet ( oExcel, Form_1.Sheets_1.Caption ( Form_1.Sheets_1.Value ), nMode) }
    FOR i := 1 TO Len ( aListOfSheets )
        PAGE aListOfSheets [ i ]
        END PAGE
    NEXT i
END TAB

Form_1.StatusBar.Item(1) := ""
ShowProgressBar( 0 )        //hide progress bar

IF Len ( aListOfSheets ) > 0
    LoadSheet (oExcel, aListOfSheets [ 1 ], nMode)
ENDIF

Return

*******************************************************************
Procedure LoadSheet ( oExcel, cSheet, nMode )
Local i, aHeaders := {}, aWidths := {}, n1, aData

IF IsControlDefined ( Grid_1, Form_1 )
    Form_1.Grid_1.Release
ENDIF

ShowProgressBar( 2 )
Form_1.StatusBar.Item(1) := "Reading data from a spreadsheet " + cSheet + " ..."
n1 := Seconds()
aData := oExcel:WorkSheet ( cSheet, { | cFile, i, nFiles | ( Form_1.PBar.Value := i / nFiles * 100, doEvents() ) }, { | i, n | Form_1.PBar.Value := i / n * 100 } )
Form_1.Title := oExcel:cXlsxFile + "   Read time " + hb_nToS ( Round( Seconds() - n1, 3 ) ) 

IF !Empty ( aData )
    For i := 1 TO Len ( aData [ 1 ] )
        AADD( aHeaders, ColExcel ( i ) )
        AADD( aWidths, IF (nMode = 0, 120, 300 ) )
    Next i
ENDIF

@ 10,10 GRID Grid_1 ;
    OF Form_1 ;
    WIDTH 1060 ;
    HEIGHT 545 ;
    VALUE 1 ;
    HEADERS aHeaders ;
    WIDTHS aWidths;
    VIRTUAL ;
    ITEMCOUNT Len ( aData ) ;
    ON QUERYDATA QuerySheet( aData ) ;
    CELLNAVIGATION
    
Form_1.Grid_1.DisableUpdate

#ifdef MG_VER_H_    //MiniGui
    Form_1.Grid_1.ColumnsAutoFit
#else               //HMG
    _SetColumnsWidthAuto( 'Grid_1', 'Form_1' )
#endif

Form_1.Grid_1.EnableUpdate    
Form_1.Grid_1.PaintDoubleBuffer := .T.
Form_1.Grid_1.Value := {1,1}
Form_1.Grid_1.Setfocus

Form_1.StatusBar.Item(1) := ""
ShowProgressBar( 0 )        //hide progress bar

RETURN 
*****************************
Function QuerySheet( aData )
Local xToDisplay
IF Empty ( aData ) .Or. This.QueryRowIndex > Len ( aData )
    Return Nil
ENDIF
xToDisplay := aData [ This.QueryRowIndex ] [ This.QueryColIndex ]
IF .Not. Valtype ( xToDisplay ) $ "CDNMU"
    xToDisplay := hb_valToExp( xToDisplay )
ELSEIF xToDisplay == NIL
    xToDisplay := ""
ENDIF
This.QueryData := xToDisplay
Return Nil

*******************************
Function ColExcel ( nCol )

Local dividend := nCol 
Local cCol := ""
Local modulo

Do While (dividend > 0)
    modulo := (dividend - 1) % 26
    cCol := Chr ( 65 + modulo ) + cCol
    dividend := INT ((dividend - modulo) / 26)
    DO Events
EndDo

Return cCol
***************************************************************************
Function ShowProgressBar( nMode, nMin, nMax, nCol, nLenght )
Default nMode := 1        //1 = init, 2 = set/show, 3 = close, 0/other = hide
Default nMin:=1, nMax:=100
Default nCol:=20, nLenght:=740
DO CASE 
    Case nMode = 1 
        DEFINE PROGRESSBAR PBar
            PARENT Form_1
            ROW    5 
            COL    nCol
            WIDTH  nLenght
            HEIGHT 12
            RANGEMIN nMin
            RANGEMAX nMax
            VALUE nMin
            TOOLTIP ""
            HELPID Nil
            VISIBLE .F.
            SMOOTH .T.
            VERTICAL .F. 
            BACKCOLOR Nil
            FORECOLOR Nil
        END PROGRESSBAR
        
        /* put the progress bar in the status bar */
        SETPARENT(Form_1.PBar.Handle, Form_1.STATUSBAR.Handle)
        
    Case nMode = 3
        Form_1.PBar.Release
        
    Case nMode = 2
        Form_1.PBar.RangeMin :=  nMin
        Form_1.PBar.RangeMax :=  nMax
        Form_1.PBar.Value :=  nMin
        Form_1.PBar.Visible := .T.
        DO EVENTS
    
    Other
        Form_1.PBar.Visible := .F.
        DO EVENTS
ENDCASE
RETURN Nil

#ifndef MG_VER_H_       //HMG

    #define LVSCW_AUTOSIZE              -1
    #define LVSCW_AUTOSIZE_USEHEADER    -2

    #xtranslate ListView_SetColumnWidthAuto ( <h>, <nColumn> ) ;
    => ;
    ListView_SetColumnWidth ( <h>, <nColumn>, LVSCW_AUTOSIZE )
    
    #xtranslate _HMG_aControlHandles		=> _HMG_SYSDATA\[3]

    *-----------------------------------------------------------------------------*
    FUNCTION _SetColumnsWidthAuto( ControlName , ParentForm )
    *-----------------------------------------------------------------------------*
       LOCAL ColumnCount
       LOCAL lSuccess := .F.
       LOCAL z, i, h
    
       i := GetControlIndex( ControlName , ParentForm )
       
       IF i <= Len ( _HMG_aControlHandles )
           h := _HMG_aControlHandles [i]
        
           IF ( ColumnCount := ListView_GetColumnCount( h ) ) > 0
              FOR z := 1 TO ColumnCount
                 lSuccess := ListView_SetColumnWidthAuto( h , z - 1 )
              NEXT z
           ENDIF
       ENDIF
    
    RETURN lSuccess
    
#endif