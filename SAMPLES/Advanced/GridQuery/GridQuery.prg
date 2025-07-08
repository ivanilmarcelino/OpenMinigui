/* Based on DbQuery.prg in Grigory's excellent program DbView located in
   \minigui\SAMPLES\Applications\DBFview\source
   All Copyright of respective owners
*/

#include "minigui.ch"

#define PROGRAM 'GridQuery'
#define COMPILE(c_b) &("{|e|" + c_b + "}")
#define MsgAlert( c ) MsgEXCLAMATION( c, PROGRAM, , .f. )
#define MsgInfo( c ) MsgInfo( c, PROGRAM, , .f. )
#define MsgYesNo( c, t ) MsgYesNo( c, t, , , .f. )

DECLARE WINDOW FrmMain
DECLARE WINDOW Form_Query

#define Q_FILE  1   // For the aQuery_ array
#define Q_DESC  2
#define Q_EXPR  3

STATIC aLangStrings := {}

memvar lXPTheme
memvar aEditcontrols

/* Test Grid Query */
Procedure Main
   local aGrid, nGrid
   LOCAL aMinMaxInfo := {}, i, nWidth
   local aHeaders := {"Sno","Words","Numeric","Factor","Value","Y","Date"}
   local aWidths := {50,70,70,50,70,50,80}
   local aJustify := {2,0,1,0,0,2,2}
   local cWin := "FrmMain"
   Set( _SET_DEBUG, .f. )
   Set Century ON
   Set Date British

   DEFINE FONT FontBold FONTNAME _HMG_DefaultFontName SIZE _HMG_DefaultFontSize + 1 BOLD

   aGrid := { { '001', 'One',   100, 4, 400,.t.,Date() },;
              { '002', 'Two',   100, 6, 600,.t.,Date()-10 },;
              { '003', 'Three', 100, 7, 700,.t.,Date()-365 },;
              { '004', 'Four',  100, 5, 500,.f.,Date()-100 },;
              { '005', 'Five',  100, 3, 300,.t.,Date()-50 },;
              { '006', 'Six',   100, 5, 500,.t.,Date()-90 },;
              { '007', 'Seven', 100, 7, 700,.f.,Date()-200 },;
              { '008', 'Eight', 100, 9, 900,.t.,Date()-150 },;
              { '009', 'Nine',  100, 5, 500,.t.,Date()-60 },;
              { '010', 'Ten',   100, 4, 400,.f.,Date()-80 },;
              { '011', 'Eleven',100, 6, 600,.t.,Date()-70 },;
              { '012', 'Twelve',100, 7, 700,.f.,Date()-30 } }
   nGrid := 0
   AEval(aWidths, {|e| nGrid += e})

   define window &cWin at 0,0 width 640 height 480 title "F2 - Query" main

      ON KEY ESCAPE OF &cWin Action thisWindow.Release
      ON KEY F2 of &cWin Action GridQuery(cWin,"Grid1")

      @ 10,10 Grid Grid1 of &cWin autosizewidth nGrid height 400 Headers aHeaders Widths aWidths ;
            Items aGrid Value 1 justify aJustify backcolor BROWN fontcolor YELLOW nosortheaders

   for i := 1 to getproperty(cWin,"Grid1","ColumnCount")
      // Dynamic Header
      FrmMain.Grid1.HeaderDYNAMICFONT(i) := {|| 'FontBold' }
      FrmMain.Grid1.HeaderDYNAMICBACKCOLOR(i) := {|| BROWN }
      FrmMain.Grid1.HeaderDYNAMICFORECOLOR(i) := {|| YELLOW }
      nWidth := FrmMain.Grid1.COLUMNWIDTH(i)
      AAdd( aMinMaxInfo, { nWidth, nWidth } )
   next

   FrmMain.Grid1.COLUMNWIDTHLIMITS := aMinMaxInfo

   end window
   DoMethod(cWin,"Activate")
return




*--------------------------------------------------------*
Procedure GridQuery(WindowName,GridName,aGrid)
*--------------------------------------------------------*
   Local cField, cComp, cType := "", i
   Local cChar := "", nNum := 0, dDate := ctod(""), nLog := 1
   Local cExpr := "", nColCount
   Local aQuery_ := {"", "", ""}
   Local aUndo_ := {}
   Local aFlds_ := {}

   Local aComp_ := { "== Equal"           , ;
                     "<> Not equal"       , ;
                     "<  Less than"       , ;
                     ">  Greater than"    , ;
                     "<= Less or equal"   , ;
                     ">= Greater or equal", ;
                     "$  Contains"        ,  ;
                     "!$ Does not Contain", ;
                     "() Is contained in" , ;
                     '"" Is empty (blank)', ;
                     "B  Begins With",;
                     "E  Ends With"  }

   Private lXPTheme := IsThemed()
   private aEditcontrols := {}
   aGrid := IIf(aGrid != NIL,aGrid, _HMG_aControlMiscData1 [ GetProperty (WindowName , GridName , "Index"), 4 ])
   nColCount := getproperty(windowname,gridname,"ColumnCount")
   for i := 1 to nColCount
      aadd(aFlds_,getproperty(windowname,gridname,"header",i))
   next
   cField := aFlds_[1]
   cComp := aComp_[7]

   DEFINE WINDOW Form_Query ;
      AT 0, 0 WIDTH 570 HEIGHT 340 + IF(lXPTheme, 8, 0) ;
      TITLE 'Query' ;
      ICON "FILTER" ;
      MODAL ;
      ON INIT ( Form_Query.List_1.Setfocus, cType := GetType(cField, aFlds_, @cChar,aGrid[2]), ;
         Form_Query.Text_1.Enabled := ( cType == "C" ), ;
         Form_Query.Text_2.Enabled := ( cType == "N" ), ;
         Form_Query.Date_1.Enabled := ( cType == "D" ), ;
         Form_Query.Combo_1.Enabled := ( cType == "L" ) ) ;
      FONT "MS Sans Serif" ;
      SIZE 8

       DEFINE FRAME Frame_1
            ROW    10
            COL    260
            WIDTH  290
            HEIGHT 135
            CAPTION 'Value'
            OPAQUE .T.
       END FRAME

       DEFINE LABEL Label_1
            ROW    30
            COL    270
            WIDTH  60
            HEIGHT 20
            VALUE 'Character'+":"
            VISIBLE .T.
       END LABEL

       DEFINE LABEL Label_2
            ROW    60
            COL    270
            WIDTH  60
            HEIGHT 20
            VALUE 'Numeric'+":"
            VISIBLE .T.
       END LABEL

       DEFINE LABEL Label_3
            ROW    90
            COL    270
            WIDTH  60
            HEIGHT 20
            VALUE 'Date'+":"
            VISIBLE .T.
       END LABEL

       DEFINE LABEL Label_4
            ROW    120
            COL    270
            WIDTH  60
            HEIGHT 20
            VALUE 'Logical'+":"
            VISIBLE .T.
       END LABEL

       DEFINE LABEL Label_5
            ROW    6
            COL    12
            WIDTH  80
            HEIGHT 16
            VALUE 'Field'
            VISIBLE .T.
       END LABEL

       DEFINE LABEL Label_6
            ROW    6
            COL    134
            WIDTH  120
            HEIGHT 16
            VALUE 'Comparison'
            VISIBLE .T.
       END LABEL

       DEFINE LISTBOX List_1
            ROW    20
            COL    10
            WIDTH  114
            HEIGHT 130
            ITEMS aFlds_
            VALUE 1
            ONCHANGE ( cField := aFlds_[This.Value], cType := GetType(cField, aFlds_, @cChar,aGrid[2]), ;
                       Form_Query.Text_1.Enabled := ( cType == "C" ), ;
                       Form_Query.Text_2.Enabled := ( cType == "N" ), ;
                       Form_Query.Date_1.Enabled := ( cType == "D" ), ;
                       Form_Query.Combo_1.Enabled := ( cType == "L" ) )
            ONDBLCLICK Form_Query.Button_1.OnClick
            TABSTOP .T.
            VISIBLE .T.
            SORT .F.
            MULTISELECT .F.
       END LISTBOX

       DEFINE LISTBOX List_2
            ROW    20
            COL    132
            WIDTH  118
            HEIGHT 160
            ITEMS aComp_
            VALUE 7
            ONCHANGE cComp := aComp_[This.Value]
            ONLOSTFOCUS IF( CheckComp(cType, cComp), , Form_Query.List_2.Setfocus )
            ONDBLCLICK Form_Query.Button_1.OnClick
            TABSTOP .T.
            VISIBLE .T.
            SORT .F.
            MULTISELECT .F.
       END LISTBOX

       DEFINE EDITBOX Edit_1
            ROW    190
            COL    10
            WIDTH  240
            HEIGHT 100
            VALUE ""
            ONCHANGE ( cExpr := This.Value, ;
      Form_Query.Button_2.Enabled := ( !empty(cExpr) ), ;
      Form_Query.Button_8.Enabled := ( !empty(cExpr) ), ;
      Form_Query.Button_10.Enabled := ( !empty(cExpr) ) )
            ONGOTFOCUS Nil
            ONLOSTFOCUS Nil
            FONTBOLD .T.
            TABSTOP .T.
            VISIBLE .T.
       END EDITBOX

       DEFINE LABEL Label_7
            ROW    175
            COL    12
            WIDTH  100
            HEIGHT 16
            VALUE 'Query expression'+":"
            VISIBLE .T.
       END LABEL

       DEFINE TEXTBOX Text_1
            ROW    26
            COL    340
            WIDTH  200
            HEIGHT 24
            ONCHANGE cChar := upper(This.Value)
            ONGOTFOCUS Form_Query.Text_1.Enabled := ( cType == "C" )
            ONENTER ( Form_Query.Button_1.OnClick, Form_Query.Button_8.Setfocus )
            FONTBOLD .T.
            TABSTOP .T.
            VISIBLE .T.
            VALUE alltrim(cChar)
       END TEXTBOX

       DEFINE TEXTBOX Text_2
           ROW    56
           COL    340
           WIDTH  200
           HEIGHT 24
           NUMERIC .T.
           INPUTMASK "9999999.99"
           RIGHTALIGN .T.
           MAXLENGTH 10
           ONCHANGE nNum := This.Value
           ONGOTFOCUS Form_Query.Text_2.Enabled := ( cType == "N" )
           ONENTER ( Form_Query.Button_1.OnClick, Form_Query.Button_8.Setfocus )
           FONTBOLD .T.
           TABSTOP .T.
           VISIBLE .T.
           VALUE nNum
       END TEXTBOX

      DEFINE DATEPICKER Date_1
           ROW    86
           COL    340
           WIDTH  110
           HEIGHT 24
           VALUE dDate
           SHOWNONE .T.
           UPDOWN .T.
           ONCHANGE dDate := This.Value
           ONGOTFOCUS Form_Query.Date_1.Enabled := ( cType == "D" )
           FONTBOLD .T.
           TABSTOP .T.
           VISIBLE .T.
      END DATEPICKER

       DEFINE COMBOBOX Combo_1
           ROW    116
           COL    340
           WIDTH  110
           HEIGHT 60
           ITEMS {"True (.T.)", "False (.F.)"}
           VALUE nLog
           ONCHANGE nLog := This.Value
           ONGOTFOCUS Form_Query.Combo_1.Enabled := ( cType == "L" )
           ONENTER ( Form_Query.Button_1.OnClick, Form_Query.Button_8.Setfocus )
           FONTBOLD .T.
           TABSTOP .T.
           VISIBLE .T.
       END COMBOBOX

       DEFINE BUTTON Button_1
           ROW    156
           COL    260
           WIDTH  136
           HEIGHT 24
           CAPTION 'A&dd'
           ACTION IF( CheckComp(cType, cComp), ( AddExpr(@cExpr, aUndo_, cField, cComp, ;
         iif(cType == "C", cChar, iif(cType == "N", nNum, ;
         iif(cType == "D", dDate, (nLog == 1)))),aFlds_), ;
         Form_Query.Button_2.Enabled := ( Len(aUndo_) > 0 ), ;
         Form_Query.Button_8.Enabled := ( !empty(cExpr) ), ;
         Form_Query.Button_10.Enabled := ( !empty(cExpr) ) ), Form_Query.List_2.Setfocus )
           ONLOSTFOCUS Form_Query.Button_2.Enabled := ( Len(aUndo_) > 0 )
           TABSTOP .T.
           VISIBLE .T.
       END BUTTON

       DEFINE BUTTON Button_2
           ROW    156
           COL    414
           WIDTH  136
           HEIGHT 24
           CAPTION '&Undo'
           ACTION ( Undo(@cExpr, aUndo_), ;
         Form_Query.Button_2.Enabled := ( Len(aUndo_) > 0 ), ;
         Form_Query.Button_8.Enabled := ( !empty(cExpr) ), ;
         Form_Query.Button_10.Enabled := ( !empty(cExpr) ) )
           ONLOSTFOCUS Form_Query.Button_2.Enabled := ( Len(aUndo_) > 0 )
           TABSTOP .T.
           VISIBLE .T.
       END BUTTON

       DEFINE BUTTON Button_3
           ROW    196
           COL    260
           WIDTH  44
           HEIGHT 24
           CAPTION 'and'
           ACTION AddText(@cExpr, aUndo_, " .and. ")
           TABSTOP .T.
           VISIBLE .T.
       END BUTTON

       DEFINE BUTTON Button_4
           ROW    196
           COL    321
           WIDTH  44
           HEIGHT 24
           CAPTION 'or'
           ACTION AddText(@cExpr, aUndo_, " .or. ")
           TABSTOP .T.
           VISIBLE .T.
       END BUTTON

       DEFINE BUTTON Button_5
           ROW    196
           COL    383
           WIDTH  44
           HEIGHT 24
           CAPTION 'not'
           ACTION AddText(@cExpr, aUndo_, " .not. ")
           TABSTOP .T.
           VISIBLE .T.
       END BUTTON

       DEFINE BUTTON Button_6
           ROW    196
           COL    444
           WIDTH  44
           HEIGHT 24
           CAPTION "("
           ACTION AddText(@cExpr, aUndo_, "( ")
           TABSTOP .T.
           VISIBLE .T.
       END BUTTON

       DEFINE BUTTON Button_7
           ROW    196
           COL    505
           WIDTH  44
           HEIGHT 24
           CAPTION ")"
           ACTION AddText(@cExpr, aUndo_, " ) ")
           TABSTOP .T.
           VISIBLE .T.
       END BUTTON

       DEFINE BUTTON Button_8
           ROW    236
           COL    260
           WIDTH  62
           HEIGHT 24
           CAPTION '&Apply'
           ACTION IF( RunQuery(cExpr,WindowName,GridName,aGrid), Form_Query.Button_9.OnClick, )
           ONLOSTFOCUS Form_Query.Button_8.Enabled := ( !empty(cExpr) )
           TABSTOP .T.
           VISIBLE .T.
       END BUTTON

       DEFINE BUTTON Button_9
           ROW    236
           COL    336
           WIDTH  62
           HEIGHT 24
           CAPTION '&Close'
           ACTION ( SetProperty( WindowName, GridName, "Value", 1 ), ;
                     DoMethod( WindowName, GridName, 'Refresh' ), ;
                     Form_Query.Release , ;
                     DoMethod( WindowName, 'Setfocus' ) )
           TABSTOP .T.
           VISIBLE .T.
       END BUTTON

       DEFINE BUTTON Button_10
           ROW    236
           COL    412
           WIDTH  62
           HEIGHT 24
           CAPTION '&Save'
           ACTION SaveQuery(cExpr, aQuery_)
           ONLOSTFOCUS Form_Query.Button_10.Enabled := ( !empty(cExpr) )
           TABSTOP .T.
           VISIBLE .T.
       END BUTTON

       DEFINE BUTTON Button_11
           ROW    236
           COL    488
           WIDTH  62
           HEIGHT 24
           CAPTION '&Load'
           ACTION iif( LoadQuery(@cExpr, aQuery_), ( aUndo_ := {}, ;
         Form_Query.Button_8.Enabled := ( !empty(cExpr) ), ;
         Form_Query.Button_10.Enabled := ( !empty(cExpr) ) ), )
           TABSTOP .T.
           VISIBLE .T.
       END BUTTON

       ON KEY ESCAPE ACTION IF( CheckComp(cType, cComp), Form_Query.Button_9.OnClick, Form_Query.List_2.Setfocus )


   END WINDOW

   Form_Query.Text_1.Enabled := .F.
   Form_Query.Text_2.Enabled := .F.
   Form_Query.Date_1.Enabled := .F.
   Form_Query.Combo_1.Enabled := .F.
   Form_Query.Button_2.Enabled := .F.
   Form_Query.Button_8.Enabled := .F.
   Form_Query.Button_10.Enabled := .F.
   Form_Query.Button_11.Enabled := .F.

   CENTER WINDOW Form_Query

   ACTIVATE WINDOW Form_Query

RETURN



Function AddText(cExpr, aUndo_, cText)

  cExpr += cText
  aadd(aUndo_, cText)
  Form_Query.Edit_1.Value := cExpr
  DO EVENTS

Return(NIL)


Function GetType(cField, aFlds_, cChar,aValues)
  local cType, n
  /*
  n := len(aFlds_)

  if cField == aFlds_[n]    // Deleted() == Logical
    cType := "L"
   else
  */
    n := ascan(aFlds_, cField)
    cType := valtype(aValues[n])
    if cType == "M"
      cType := "C"
    elseif cType == "C"
      cChar := padr(cChar, len(aValues[n] ))
    endif
 * endif

Return(cType)


Function CheckComp(cType, cComp)
  local lOk := .T.
  local cTemp := left(cComp, 2)

  do case
    case cType $ "ND"
      if cTemp $ "$ ()"
        lOk := .F.
      endif
    case cType == "L"
      if cTemp <> "==" .and. cTemp <> "<>" .and. cTemp <> '""'
        lOk := .F.
      endif
    otherwise     // All are Ok for character variables
      lOk := .T.
  endcase

  if !lOk
    MsgAlert("Invalid comparison for selected data type.")
  endif

Return(lOk)


Function AddExpr(cExpr, aUndo_, cField, cComp, uVal,aFlds_)
  local cVT, cTemp :='', nPos
  local xFieldVal := uVal
  nPos := ascan(aFlds_, cField)
  cComp := alltrim(left(cComp, 2))
  cField := 'e['+hb_ntos(nPos)+']'
  if cComp == '()'
    cTemp := '"' + rtrim(uVal) + '" $ ' + cField
  elseif cComp == '""'
    cTemp := "empty(" + cField + ")"
  else
    cTemp := cField + ' ' + cComp + ' '
    cVT := valtype(uVal)
    do case
    CASE cComp == "B"
       cTemp := "SUBSTR( Upper("+cField+"),1,"+hb_ntos(Len(ALLTRIM(uVal)))+') =  "'+UPPER(ALLTRIM(uVal))+'"'
    CASE cComp == "E"
       cTemp := "SUBSTR( Upper("+cField+"),"+hb_ntos(Len(ALLTRIM(uVal))*-1)+','+hb_ntos(Len(ALLTRIM(uVal)))+') =  "'+UPPER(ALLTRIM(uVal))+'"'
     case cVT == 'C' .and. '!$' $ cComp
       cTemp := '!"' + padr(uVal, len(xFieldVal)) + '" $ Upper('+cField+')'
     case cVT == 'C' .and. '$' $ cComp
       cTemp := '"' + padr(uVal, len(xFieldVal)) + '" '+cComp+' Upper('+cField+')'
     case cVT == 'C'
        cTemp += '"' + padr(uVal, len(xFieldVal)) + '"'
      case cVT == 'N'
        cTemp += ltrim(str(uVal))
      case cVT == 'D'
        cTemp += 'ctod("' + dtoc(uVal) + '")'
      case cVT == "L"
        cTemp += iif(uVal, '.T.', '.F.')
    endcase
  endif

  cTemp += " "

  AddText(@cExpr, aUndo_, cTemp)

  /*
  if cVT == '()'
     cTemp := 'e[' +hb_ntos(nPos)+'] $ "'+uVal+'"'
  elseif cVT == '""'
    cTemp := "empty(e[" + hb_ntos(nPos) + "])"
  else
    cTemp := cField + ' ' + cVT + ' '
    cVT := valtype(uVal)
    do case
    case cVT == 'C'
        cTemp += '"' + padr(uVal, len(xFieldVal)) + '"'
      case cVT == 'N'
        cTemp += ltrim(str(uVal))
      case cVT == 'D'
        cTemp += 'ctod("' + dtoc(uVal) + '")'
      case cVT == "L"
        cTemp += iif(uVal, '.T.', '.F.')
    endcase
  endif
    *cTemp += alltrim(left(cComp, 2)) + ' Upper(e['+hb_ntos(nPos)+'])'

  cTemp += " "

  AddText(@cExpr, aUndo_, cTemp)
   */
Return(NIL)


Function Undo(cExpr, aUndo_)
  local l := len(aUndo_)
  local x, cTemp := cExpr

  if (x := rat(aUndo_[l], cTemp)) > 0
    cExpr := InsDel(cTemp, x, len(aUndo_[l]), "")
    Form_Query.Edit_1.Value := cExpr
    DO EVENTS
  endif

  asize(aUndo_, l - 1)

Return(NIL)


Function RunQuery(cExpr,cWin,GridName,aGrid)
   LOCAL oError, Filterblock
   TRY
      IF _IsControlDefined(GridName,cWin)
         Filterblock := COMPILE(cExpr)
         DoMethod(cWin,GridName,"deleteallitems")
         AEVAL(aGrid,{|e| IIf(EVAL(FilterBlock,e),DoMethod(cWin,GridName,"AddItem",e),"")})
      endif
   CATCH oError
        Errorsys(oError)
   FINALLY
     doMethod(cWin,GridName,"Refresh")
  END
Return(.t.)


Function SaveQuery(/*cExpr, aQuery_*/)
  /*
  local cDesc := "", cPath, nSaveRec := 0
  local cQFile := ""
  local lAppend := .T., x
  dstk_push()
  if !empty(aQuery_[Q_DESC])
    cDesc := alltrim(aQuery_[Q_DESC])
  endif

  cDesc := InputBox( 'Enter a brief description of this query'+":" , 'Query Description' , cDesc )
  if len(cDesc) == 0  // Rather than empty() because they may hit 'Ok' on
    Return(NIL)       // just spaces and that is acceptable.
  endif

  cQFile := "QueriesDbq"
  if empty(cQFile)
    Return(NIL)
   endif
  if !MedIsTable(cQFile)
    Cr_QFile(cQFile)
  endif
  cDbFile := "Query"
  aQuery_[Q_FILE] := padr(cDBFile, 12)
  aQuery_[Q_DESC] := padr(cDesc, 80)
  aQuery_[Q_EXPR] := cExpr

  IF OpenDataBaseFile( cQFile, "QFile", .T., .F., RDDSETDEFAULT() )

    if QFile->( NotDBQ(cQFile) )
      QFile->( DBCloseArea() )
      Return(NIL)
    endif
    QFile->( DBGoTop() )
    do while !QFile->( eof() )
        if Upper(AllTrim(QFile->DESC)) == Upper(AllTrim(aQuery_[Q_DESC]))
          x := MsgYesNoCancel( 'A query with the same description was found for this database' + "." + CRLF + ;
                         'Do you wish to overwrite the existing query or append a new one?', 'Duplicate Query', , .f. )
          if x == 6
            lAppend := .F.
          elseif x == 2
            QFile->( DBCloseArea() )
            Return(NIL)
          endif
        endif
        QFile->( DBSkip() )
     ENDDO
     if lAppend
        nSaveRec := RECNO()
        sele Qfile
       BEGIN TRANSACTION
         if nettry(1,;
                {||dbappend(.f.),!neterr()})
           dbrunlock(nsaverec)
         endif
          if nettry(1,;
               {||dbrlock(),!neterr()})
                  aQuery_[Q_FILE]:= "Query"+AllTrim(Str(qFile->(RECNO())))
                  QFile->FILENAME := aQuery_[Q_FILE]
                  QFile->DESC := aQuery_[Q_DESC]
                  QFile->EXPR := aQuery_[Q_EXPR]
          ENDIF
       COMMIT TRANSACTION
     endif
    QFile->( DBCloseArea() )
    MsgInfo('Query Saved')
  ENDIF
  dstk_pop()
  */
Return(NIL)


Function LoadQuery(/*cExpr, aQuery_*/)
  /*
  local cQFile := "", nOldArea := SELECT()
  local lLoaded := .F., lCancel := .F.
  ADIR := ShowDatabases()
  DEFINE WINDOW FrmDbf AT 10, 10 Width 250 Height 300 modal
  @ 10,10 LISTBOX ListBox_1 WIDTH 200 HEIGHT 190 ;
      ITEMS aDir;
      value 1
     DEFINE BUTTON Button_1
           ROW    230
           COL    150
           WIDTH  62
           HEIGHT 24
           CAPTION '&Ok'
           ACTION (  cQFile := ADIR[FrmDbf.ListBox_1.Value], ;
                     FrmDbf.Release )
       END BUTTON
    END WINDOW
  FrmDbf.Center
  DoMethod("FrmDbf","Activate")
  cQFile := "QueriesDbq"
  if empty(cQFile)
    Return(lLoaded)
 endif
  dstk_push()
  IF OpenDataBaseFile( cQFile, "QFile", .T., .F., RDDSETDEFAULT() )
    if QFile->( NotDBQ(cQFile) )
       QFile->( DBCloseArea() )
       dstk_pop()
      Return(lLoaded)
    elseif QFile->( eof() )
      MsgInfo(cQFile + " " + 'does not contain any queries' + "!")
    else
      DEFINE WINDOW Form_Load ;
      AT 0, 0 WIDTH 700 HEIGHT 300 ;
      TITLE 'Load Query' + " - " + cQFile ;
      ICON 'CHILD' ;
      MODAL ;
      FONT "Tahoma" ;
      SIZE 8

      DEFINE BROWSE Browse_1
         ROW 10
         COL 10
         WIDTH GetProperty( 'Form_Load', 'Width' ) - 28
         HEIGHT GetProperty( 'Form_Load', 'Height' ) - 78
         HEADERS { "X", 'Database', 'Description', 'Query Expression' }
         WIDTHS { 21, 86, 174, if(QFile->( Lastrec() ) > 8, 500, 516) }
         FIELDS { 'iif(QFile->( deleted() ), " X", "  ")', ;
                                  'QFile->FILENAME', ;
                                  'QFile->DESC', ;
                                  'QFile->EXPR' }
         WORKAREA QFile
         VALUE QFile->( Recno() )
         VSCROLLBAR QFile->( Lastrec() ) > 8
         READONLYFIELDS { .t., .t., .t., .t. }
         ONDBLCLICK Form_Load.Button_1.OnClick
         END BROWSE


      DEFINE BUTTON Button_1
           ROW    GetProperty( 'Form_Load', 'Height' ) - 58
           COL    186
           WIDTH  80
           HEIGHT 24
           CAPTION '&Load'
           ACTION iif(LoadIt(aQuery_),ThisWindow.Release, )
           TABSTOP .T.
           VISIBLE .T.
      END BUTTON

    endif

      DEFINE BUTTON Button_2
           ROW    GetProperty( 'Form_Load', 'Height' ) - 58
           COL    286
           WIDTH  80
           HEIGHT 24
           CAPTION '&Close'
           ACTION (lCancel := .T., ThisWindow.Release )
           TABSTOP .T.
           VISIBLE .T.
      END BUTTON

      DEFINE BUTTON Button_3
           ROW    GetProperty( 'Form_Load', 'Height' ) - 58
           COL    386
           WIDTH  80
           HEIGHT 24
           CAPTION '&Delete'
           ACTION iif(QFile->( DelRec() ), ( Form_Load.Browse_1.Refresh, Form_Load.Browse_1.Setfocus ), )
           TABSTOP .T.
           VISIBLE .T.
      END BUTTON

      ON KEY ESCAPE ACTION Form_Load.Button_2.OnClick

   END WINDOW

   CENTER WINDOW Form_Load

   ACTIVATE WINDOW Form_Load

      if !lCancel
        cExpr := aQuery_[Q_EXPR]
        Form_Query.Edit_1.Value := cExpr
        lLoaded := .T.
      endif

    endif

    QFile->( __DBPack() )
    QFile->( DBCloseArea() )
    SELECT(nOldArea)
  */
Return(NIL/*lLoaded*/)


STATIC Function NotDBQ(cQFile)
  local lNot := .F.

  if fieldpos("FILENAME") == 0 .or. ;
     fieldpos("DESC") == 0 .or. ;
     fieldpos("EXPR") == 0
    lNot := .T.
    MsgAlert(cQFile + " " + 'is not a DataBase query file' + ".")
  endif

Return(lNot)


STATIC Function LoadIt(aQuery_)
  local lLoaded := .t.
  if lLoaded
    aQuery_[Q_FILE] := alltrim(QFile->FILENAME)
    aQuery_[Q_DESC] := alltrim(QFile->DESC)
    aQuery_[Q_EXPR] := alltrim(QFile->EXPR) + " "
  endif

Return(lLoaded)


Function DelRec()
  local lDel := .F.
  local cMsg, cTitle

  if deleted()
    cMsg := 'Are you sure you wish to recall this record?'
    cTitle := 'Recall'
  else
    cMsg := 'Are you sure you wish to delete this record?'
    cTitle := 'Delete'
  endif

  if MsgYesNo(cMsg, cTitle)
    if deleted()
      DBRecall()
    else
      DBDelete()
    endif
    lDel := .T.
  endif

Return(lDel)


STATIC Function QueryError(e)
  local cMsg := 'Syntax error in Query expression!'

  if valtype(e:description) == "C"
    cMsg := e:description
    cMsg += if(!empty(e:filename), ": " + e:filename, ;
            if(!empty(e:operation), ": " + e:operation, "" ))
  endif
  MsgSTOP(cMsg)
  dbcloseall()
  *doMethod(cWin,"Release")
Return NIl

STATIC Procedure Cr_QFile(cQFile)
  local aArray_ := { { "FILENAME", "C",  12, 0 }, ;
         { "DESC", "C",  80, 0 }, ;
         { "EXPR", "C", 255, 0 } }

  DBCreate(cQFile, aArray_)

Return

STATIC Function InsDel(cOrig, nStart, nDelete, cInsert)
  local cLeft := left(cOrig, nStart - 1)
  local cRight := substr(cOrig, nStart + nDelete)

Return(cLeft + cInsert + cRight)

function LangStrings
Local aStr := { ;
   "&File", ;      // Main Menu
   "&Open...", ;
   "&Open in", ;
   "&Close", ;
   "&Save", ;
   "Save &As/Export...", ;
   'Recent Files', ;
   'Empty', ;
   "E&xit", ;
   "&Edit", ;
   "&Find...", ;
   "&Replace...", ;
   "&Go To...", ;
   "&Append Record", ;
   "Emp&ty", ;
   "Copy C&urrent", ;
   "&Delete status", ;
   "&Delete Record", ;
   "&Undelete Record", ;
   "&Toggle Delete", ;
   "Delete &All", ;
   "U&ndelete All", ;
   "&Pack Table", ;
   "&Zap Table", ;
   "&View", ;
   "&Adjust Columns", ;
   "&Refresh", ;
   "&Background color...", ;
   "&Font...", ;
   "&Language...", ;
   "&Table", ;
   "&Codepage...", ;
   "&Query...", ;
   "&Properties...", ;
   "&Window", ;
   "&Cascade", ;
   "&Tile", ;
   "&Hide All", ;
   "&Show All", ;
   "&Help", ;
   "&Index", ;
   "&About", ;
   "Open", ;      // Tooltips
   "Save", ;
   "Toggle delete", ;
   "Find", ;
   "Go To", ;
   "Codepage", ;
   "Properties", ;
   "Adjust columns", ;
   "Query", ;
   "Refresh", ;
   "About", ;
   "Font Name", ;
   "Font Size", ;
   "Table", ;      // Child window
   "Append &New", ;
   "Append C&opy", ;
   "Record", ;
   "of", ;
   "already exists", ;  // Export
   "Overwrite existing file?", ;
   "Sorry, you may open 12 files only!", ;
   "<all columns>", ;   // S&R
   "Replace", ;
   "Search", ;
   "Look for", ;
   "Replace with", ;
   "Direction", ;
   "Forward" , ;
   "Backward" , ;
   "Entire scope", ;
   "Search in column", ;
   "Match &case", ;
   "Match &whole word only", ;
   "&Find Next", ;
   "Replace &All", ;
   "Can not find the string", ;
   "There are no still such records!", ;
   "Go to", ;      // Go To
   "&Top", ;
   "&Bottom", ;
   "Ro&w", ;
   "&Record", ;
   "OK", ;
   "Cancel", ;
   "Select language", ; // Lang
   "Date format", ;
   "<no codepage set>", ; // CP
   "Select codepage", ;
   "Select codepage for the current table", ;
   "Table properties", ;   // Prop
   "File", ;
   "Size", ;
   "byte(s)", ;
   "Created", ;
   "Modified", ;
   "Number of records", ;
   "Header size", ;
   "Record size", ;
   "Number of fields", ;
   "Packing the table permanently removes records that are marked for deletion", ;
   "Pack the table", ;
   "Zaping the table permanently removes the ALL records", ;
   "Zap the table", ;
   "The file is not found", ; // Open
   "Query", ;      // Query
   "Field", ;
   "Comparison", ;
   "== Equal           ", ;
   "<> Not equal       ", ;
   "<  Less than       ", ;
   ">  Greater than    ", ;
   "<= Less or equal   ", ;
   ">= Greater or equal", ;
   "() Contains        ", ;
   "$  Is contained in ", ;
   '"" Is empty (blank)', ;
   "deleted()", ;
   "Value", ;
   "Character", ;
   "Numeric", ;
   "Date", ;
   "Logical", ;
   "Query expression", ;
   "A&dd", ;
   "&Undo", ;
   ".and.", ;
   ".or.", ;
   ".not.", ;
   "&Apply", ;
   "&Load", ;
   "There are no such records!", ;
   "Enter a brief description of this query", ;
   "Query Description", ;
   "Save Query", ;
   "A query with the same description was found for this database", ;
   "Do you wish to overwrite the existing query or append a new one?", ;
   "Duplicate Query", ;
   "Query Saved", ;
   "Select a Query to Load", ;
   "does not contain any queries", ;
   "Load Query", ;
   "Database", ;
   "Description", ;
   "Query Expression", ;
   "&Delete", ;
   "is not a DataBase query file", ;
   "The query's filename does not match that of the currently loaded file", ;
   "Load it anyway?", ;
   "Different Filename", ;
   "Are you sure you wish to recall this record?", ;
   "Recall", ;
   "Are you sure you wish to delete this record?", ;
   "Delete", ;
   "Syntax error in Query expression!" }

Return aStr

FUNCTION OpenDataBaseFile( cDataBaseFileName, cAlias, lExclusive, lReadOnly, cDriverName, lNew )
*--------------------------------------------------------*
 Local _bLastHandler := ErrorBlock( {|o| Break(o)} ), _lGood := .T., oError

   If PCount() < 6 .or. ValType(lNew) <> "L"
      lNew := .T.
   EndIf

   BEGIN SEQUENCE
   dbUseArea( lNew, cDriverName, cDataBaseFileName, cAlias, !lExclusive, lReadOnly )

   RECOVER USING oError
      _lGood := .F.
      QueryError(oError)
   END
   ErrorBlock( _bLastHandler )

Return( _lGood )


Function TestaLangString
/*
   load_file("temp.txt")
   printarray(aLangStrings)
   unload_file()
   reportfile("temp.txt")
   printfile("temp.txt")
*/
return NIL
/*
FUNCTION sfq_cntain(cInstring,cDelimstr)
local nIter,cPartOf
nIter := 1
cPartOf = takeout(cDelimStr,';',nIter)
DO WHILE !EMPTY(cPartOf)
  IF cPartOf$cInstring
    RETURN .T.
  ENDIF
  nIter++
  cPartOf = takeout(cDelimStr,';',nIter)
ENDDO
RETURN .F.
*/

Function ShowDatabases
   local aDatabase :={}
 // databases only  SELECT schema_name FROM information_schema.schemata
 // database +tables SELECT table_schema,table_name FROM information_schema.tables
  /*
   use ShowDb as "SELECT table_schema,table_name FROM information_schema.tables "+;
                 "where table_schema NOT LIKE('%SCHEMA%') AND "+;
                 "table_schema NOT LIKE ('%MYSQL%')" NEW
   use ShowDb as "SELECT table_schema,table_name FROM information_schema.tables "+;
                 "where table_schema NOT RLIKE('SCHEMA|MYSQL')" NEW
   dbeval({|| aadd(aDatabase,alltrim(fieldget(1))+"\"+alltrim(fieldget(2)))},{|| "DBQ" $ Upper(FieldGet(2))})
   close showDb
  */
return aDatabase
