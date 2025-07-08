/*
 * cas_dbf.prg
 * autor: CAS <cas_webnet@yahoo.com.br>
 *
 * Este programa abre arquivos .DBF
 * e faz procura pelos campos criando um arquivo .CDX temporario,
 * é so clicar na coluna do campo e escrever o que deseja procurar
 * por enquanto esta fazendo somente busca por campos do tipo
 * caracter, numerico, data e logica
 *
 * Falta procurar por campo memo, quem sabe na proxima versao.

  19/04/2020 Add an enhanced demo with Tsbrowse search
  If present an logic field, you can change the color of tsbrowse row
  The searches on tsbrowse are also activated by clicking on the column head
  or by choosing the field in the combobox.
  If the search field is logical, you can search for the value using "1", "T", "ON", "S", "Y", ". T."
  like TRUE, otherwise everything is considered to be FALSE
  If the search field is a date, you can search for total date (28/1/2020), or
  month plus year, or only year (2 or 4 digit), the date separator can be "/" or ".", or "-"
  If the search field is a Number the decimal separator accept "." or "," and expression like >= etc.
  Part of this code is an "academic exercise"
  If you want use a file index comment Line 35  #DEFINE MEM_INDEX
*/

*.(cas)....................................................................................*

#include "minigui.ch"
#include "Dbstruct.ch"
#include "Tsbrowse.ch"

MemVar a_fields, a_width, x_campo, x_macro, m_filtrado, h_inicial
MemVar h_hora, var, BfColor, cPath, SMALL

#DEFINE MEM_INDEX    //   If enable this, all index will be created on memory

#IFDEF MEM_INDEX
   REQUEST HB_MEMIO
#ENDIF
/*
*/
*-----------------------------------------------------------------------------*
Procedure Main
*-----------------------------------------------------------------------------*
Local n_for
private var
Private BfColor
private a_fields := {}
private a_width  := {}
private x_campo := '', x_macro
private m_filtrado := 0
private h_inicial, h_hora
Private cPath   := hb_FNameDir(hb_ProgName()+".EXE" )
Private Cleanfile

m->BfColor := {255,204,102}

REQUEST DBFCDX, DBFFPT
RDDSETDEFAULT( "DBFCDX" )
SET AUTOADJUST ON

Use MUSIC Via "DBFCDX"
var := alias()

IF !used()
   MSGSTOP ("I can't open Database in exclusive mode !"+hb_eol();
           +chr(9)+"Program terminated !" ,"File Busy !" )
   exitprocess()
Endif

For n_for=1 to fcount()
    aadd( a_fields , fieldname( n_For ) )
    aadd( a_width  , 150 )
Next

SET DATE BRITISH
SET CENTURY ON
SET DELETED ON
SET BROWSESYNC ON

DEFINE WINDOW Form_1 ;
    AT 0,0 WIDTH 640 HEIGHT 480 ;
    TITLE 'CAS_DBF - By CAS - cas_webnet@yahoo.com.br' ;
    MAIN NOMAXIMIZE ;
    ICON "ICON_CAS.ICO" ;
    ON INIT OpenTables() ;
    ON RELEASE CloseTables()

    @ 0,250 COMBOBOX Combo_2 of Form_1 ;
        WIDTH 150 ;
        ITEMS {'Ascending','Descending'} ;
        VALUE 1 ;
        ON CHANGE f_change_order() ;
        FONT 'Courier' SIZE 10

    DEFINE BUTTONEX Button_2
        ROW    0
        COL    460
        HEIGHT 24
        CAPTION "TBrowse Search"
        BACKCOLOR WHITE
        GRADIENTFILL { { BfColor,BfColor } }
        ACTION  TsbSrc()
        FLAT .T.
        TOOLTIP "Tbrowse Search demo"
    END BUTTONex

    f_menu()

    DEFINE STATUSBAR
        STATUSITEM ''
    END STATUSBAR

    f_browse()
    ON KEY ESCAPE ACTION Form_1.RELEASE

END WINDOW

CENTER WINDOW Form_1
Form_1.Browse_1.SetFocus
ACTIVATE WINDOW Form_1

Return

*.(cas)....................................................................................*
/*
*/
*-----------------------------------------------------------------------------*
Procedure OpenTables()
*-----------------------------------------------------------------------------*
Ferase("TMP.CDX")

Form_1.Browse_1.Value := RecNo()
f_autofit()

Return

*.(cas)....................................................................................*

/*
*/
*-----------------------------------------------------------------------------*
Procedure CloseTables()
*-----------------------------------------------------------------------------*
    Use
    Ferase(m->Cleanfile)
Return

*.(cas)....................................................................................*
/*
*/
*-----------------------------------------------------------------------------*
Procedure f_seek( x_campo )
*-----------------------------------------------------------------------------*
Local m_entrada, tmpfile

#IFDEF MEM_INDEX
   tmpfile := "Mem:Cas"+x_campo
#ELSE
   tmpfile := GetTempFolder()+"\CasTmp_"+x_campo+".Cdx"
#ENDIF

m->cleanfile  := TmpFile

if iswindowdefined("_InputBox")  // avoid the problems of a double click Pierpaolo 4/2020
   DoMethod("_InputBox","_TextBox","SETFOCUS")
   Return
Else
   m_entrada:= alltrim( InputBox( 'Campo: '+x_campo , 'cas_webnet@yahoo.com.br' ) )
Endif

if empty( m_entrada )
    set index to
    go top
    form_1.browse_1.value := recno()
    MsgStop( "Busca cancelada" )
    Return
EndiF

if type(x_campo) = 'N'
    x_macro := m_entrada +'='+ x_campo

elseif type(x_campo) = 'D'
    x_macro := [ctod("] + m_entrada + [")] + " = " + x_campo

elseif type(x_campo) = 'L'
    x_macro := m_entrada +'='+ x_campo

else
    x_macro := [upper("] + m_entrada + [")] + " $ upper(" + x_campo + ")"

EndiF

set index to

Ferase ( TmpFile )

h_inicial := time()
h_hora    := time()

if form_1.combo_1.value # 0
    x_campo := a_fields[ form_1.combo_1.value ]
end

if form_1.combo_2.value = 2   && ascending / descending
   index on  &x_campo  TAG CAS_TAG to ( TmpFile ) ;
         For &x_macro ;
         EVAL cas_Progress( 1 , h_hora, h_inicial )  EVERY  LASTREC()/10 DESCENDING
else
   index on  &x_campo  TAG CAS_TAG to ( TmpFile ) ;
         For &x_macro ;
         EVAL cas_Progress( 1 , h_hora, h_inicial )  EVERY  LASTREC()/10
EndiF

go top

form_1.browse_1.value := recno()
form_1.browse_1.refresh

Show_Status()

f_autofit()

Return

*.(cas)....................................................................................*
/*
*/
*-----------------------------------------------------------------------------*
Function cas_Progress( m_pos , t_time , t_inicio )
*-----------------------------------------------------------------------------*

Local cComplete := LTRIM(STR((RECNO()/LASTREC()) * 100))
Local x_indice  := "Indice: " +alltrim(str(m_pos))

Form_1.StatusBar.Item(1) := ;
    x_indice + "   Indexing..." +;
    cComplete + "%" +;
    "     Time:" + elaptime( t_time , time() ) +' - '+;
    "    Total:" + elaptime( t_inicio , time() )

Return .T.
*.(cas)....................................................................................*

/*
*/
*-----------------------------------------------------------------------------*
Procedure Show_Status
*-----------------------------------------------------------------------------*

Form_1.StatusBar.Item(1) := ;
'Lastrec '+alltrim(str(lastrec())) +' / '+;
alltrim(str(recno())) + ' Recno      Itens filtrado(s) = ' + alltrim(str(f_filtro())) +;
'       OrdKeyNo() = ' + alltrim(str( OrdKeyNo() )) +;
'       Porc = ' + alltrim(str( OrdKeyNo() / m_filtrado * 100 ) ) + '%'

Return
*.(cas)....................................................................................*
/*
*/
*-----------------------------------------------------------------------------*
Procedure f_autofit
*-----------------------------------------------------------------------------*
Form_1.Browse_1.DisableUpdate
Form_1.Browse_1.ColumnsAutoFitH
Form_1.Browse_1.ColumnsAutoFit
Form_1.Browse_1.EnableUpdate

Return

*.(cas)....................................................................................*
/*
*/
*-----------------------------------------------------------------------------*
Procedure f_read
*-----------------------------------------------------------------------------*
Local File_mp3,  n_for
Local varios := .t.   && selecionar varios arquivos

Local arq_mp3 := GetFile ( { ;
                           {'Files DBF' , '*.DBF'}  } ,;
                           'Open File DBF' , '' , varios , .t. )

if len( arq_mp3 ) = 0
    Return
EndiF

form_1.browse_1.release
form_1.combo_1.release
form_1.label_1.release

Close data

// I only choose the first file selected
arq_mp3 := asort(arq_mp3,,,{|x| x})
File_mp3 := arq_mp3[ 1 ]
m->var   := hb_FnameName(File_mp3)

USE &file_mp3 ALIAS &var

IF !used()
   MSGSTOP ("I can't open Database in exclusive mode !"+hb_eol();
           +chr(9)+"Program terminated !" ,"File Busy !" )
   exitprocess()
Endif

a_fields := {}
a_width  := {}
For n_for = 1 to fcount()
    aadd( a_fields , fieldname( n_For ) )
    aadd( a_width  , 150 )
Next

f_browse()
form_1.browse_1.refresh

Return
*.(cas)....................................................................................*
/*
*/
*-----------------------------------------------------------------------------*
Procedure f_browse
*-----------------------------------------------------------------------------*

Local n_for
Local a_seek := {}
MemVar xx_var
private xx_var

For n_for=1 to fcount()
    xx_var := "'" + fieldname( n_For ) + "'"
    aadd( a_seek , { || f_seek( &xx_var ) } )
Next

@ 40,20 BROWSE Browse_1 of Form_1 ;
        WIDTH 590 ;
        HEIGHT 340 ;
        HEADERS a_fields ;
        WIDTHS a_width ;
        WORKAREA &var ;
        FIELDS a_fields ;
        TOOLTIP 'Browse' ;
        ON CHANGE Show_Status() ;
        ON HEADCLICK a_seek ;
        DELETE ;
        LOCK ;
        EDIT INPLACE

@ 0,5 label label_1 of form_1 value 'Order by' autosize

@ 0,60 COMBOBOX Combo_1 of Form_1 ;
       WIDTH 150 ;
       ITEMS a_fields ;
       VALUE 1 ;
       ON ENTER MsgInfo ( Str(Form_1.Combo_1.value) ) ;
       ON CHANGE f_change_order() ;
       FONT 'Courier' SIZE 10

f_menu()

Return
*.(cas)....................................................................................*
/*
*/
*-----------------------------------------------------------------------------*
Procedure f_menu
*-----------------------------------------------------------------------------*
Local n_for, x_for, x_act

DEFINE MAIN MENU OF Form_1
    POPUP 'Arquivo'
        ITEM 'Abrir'    ACTION f_read()
        ITEM 'AutoFit'    ACTION f_autofit()
        ITEM  "Extended Search" action TsbSrc()
        ITEM 'Sair'    ACTION Form_1.Release
        SEPARATOR
        For n_for=1 to fcount()
            x_For = fieldname( n_For )
            x_act = "f_seek('" + x_For + "')"
            ITEM x_For action &x_act
        Next

    END POPUP
    POPUP '?'
        ITEM 'Sobre'    ACTION MsgInfo( MiniGuiVersion()+CRLF+CRLF+"April 2020 revision By Pierpaolo Martinello", 'CAS_DBF by CAS' )
    END POPUP
END MENU

DEFINE CONTEXT MENU OF Form_1
       ITEM  "Extended Search" action TsbSrc()
       For n_for=1 to fcount()
           x_For := fieldname( n_For )
           x_act := "f_seek('" + x_For + "')"
           ITEM x_For action &x_act
       Next
END MENU

Return

*.(cas)....................................................................................*
/*
*/
*-----------------------------------------------------------------------------*
Procedure f_change_order
*-----------------------------------------------------------------------------*
Local TmpFile

#IFDEF MEM_INDEX
   tmpfile := "Mem:Cas_"+x_campo
#ELSE
   tmpfile := GetTempFolder()+"\CasTmp_"+x_campo+".Cdx"
#ENDIF
m->cleanfile  := TmpFile

set index to

Ferase ( TmpFile )

h_inicial := time()
h_hora    := time()

if form_1.combo_1.value # 0
    x_campo := a_fields[ form_1.combo_1.value ]
end

x_macro := '.t.'

if form_1.combo_2.value = 2   && ascending / descending
    index on  &x_campo  TAG CAS_TAG to ( TmpFile ) ;
          For &x_macro ;
          EVAL cas_Progress( 1 , h_hora, h_inicial )  EVERY  LASTREC()/10 DESCENDING
else
    index on  &x_campo  TAG CAS_TAG to ( TmpFile ) ;
          For &x_macro ;
          EVAL cas_Progress( 1 , h_hora, h_inicial )  EVERY  LASTREC()/10
EndiF

go top
form_1.browse_1.value := recno()
form_1.browse_1.refresh

Return
// Start of new code using TsBrowse instead of Browse
/*
*/
*-----------------------------------------------------------------------------*
Procedure TsbSrc( )
*-----------------------------------------------------------------------------*
          Local noldorder  := indexord()
          Local old_sele   := select()
          Local nFld       := Fcount()
          Local Head       := {}
          Local campi      := {}
          Local nLogic     := {}
          Local dimensione := {}
          Local warea      := DBF() ,ic , scambio,pfsee, nchoice, ecolor
          Local Ahead      := array( nFld )
          Local aEst       := DBstruct()

          Public CRCN

          For ic := 1 to nFld
                 aadd(campi,aEst[ic,1])
                 aadd(Head,aEst[ic,1])
                 if aEst[ic,2]=="L"
                    aadd(nLogic,aEst[ic,1])
                 Endif
          Next

          dbsetorder(1)
          DBGOTOP()

          m->CRCN := 0

          nchoice := combo_choice( campi )

          If nChoice = 0
             Pfsee := ""
          Else
             Pfsee := campi[nchoice]
          Endif

          if len (nLogic) > 0
              nchoice := combo_choice( nLogic ,"Choose the field under color control: ","Field to check")

              If nChoice # 0
                 ecolor := nLogic[nchoice]
              Endif
          Endif

          scambio := Smalln1(campi,Head,warea,dimensione,campi[1],aHead,,,pfsee, ecolor )

          if scambio # nil .and. len(scambio) > 0 .and. valtype(scambio)# "N"
             set filter to
             DBGOTO( m->CRCN )
             msgbox ( scambio, "You have chosen :")
          EndiF
          m->CRCN := 0
          Dbsetorder(noldorder)
          sele (old_sele)
          Domethod('FORM_1','setfocus')
            */
Return
/*
*/
*-----------------------------------------------------------------------------*
Function Smalln1(_campi,_Head,warea,_Dimensione,ritorna ,headcode,bcolor,Real_fld,pfsee, eColor)
*-----------------------------------------------------------------------------*
         Local parto_da   := &warea->(recno()), ncl := 0
         Local OLDFILTER  := DBFILTER(), old_ord    := indexord()
         Local ndeci      := set(_SET_DECIMALS)
         Local Icount     := 1, I, Rv := 2 , retval :=''
         Local cAlias     := &warea->(alias())
         Local Adummy , oCol, addH := 20
         private ASindex, NomeCampo , small, campo1
         EMPTY(m->NomeCampo)
         m->ASindex := {}

         DEFAULT bcolor to {},real_fld to{}, warea to alias(), _dimensione to {580},pfsee to ""

         If Valtype( ecolor ) == "C"
            ecolor :=  &("{||eval(Small:GetColumn("+hb_ntos(fieldnum(ecolor))+"):bData) = .T.}")
         Else
            ecolor := {||.F.}
         EndIf

         SET DECIMAL TO 4

         if len(bcolor) < len (Headcode)
            aeval(Headcode,{||aadd(bcolor,rgb(255,255,255))})
         EndiF

         if len(Real_fld) < len (_campi)
            Asize(real_fld,0)
            aeval(_campi,{|x|aadd(real_fld,x)})
         EndiF

         if valtype(ritorna)=="C"
            m->NomeCampo := ritorna
         elseif valtype(ritorna)=="N"
            m->NomeCampo := substr(_campi[ritorna],at(">",_campi[ritorna])+1)
         else
            m->NomeCampo := substr(_campi[1],at(">",_campi[1])+1)
         EndiF

         m->campo1 := UPPER(substr(_campi[1],at(">",_campi[1])+1))

         if eof(); parto_da :=1 ; EndiF

         if len(_campi) > 1 .and. len(_dimensione) < 2
            _dimensione := find_len(_campi,60)
         EndiF

         do while !empty(indexkey(icount))
            i := ordbagname(icount)
            //msgdebug(i)
            #IFDEF MEM_INDEX
                   aadd(m->aSindex,i) //tmpfile := "Mem:Cas"+x_campo
            #ELSE
                   aadd(m->aSindex,GetTempFolder()+[\]+i )
            #ENDIF
            icount ++
         enddo

         SET FILTER TO
         SET DELETED ON

         DEFINE WINDOW FormT_S ;
              AT 140,235 ;
              WIDTH IF (getdesktopwidth() >= 1024, 1080,938) HEIGHT 600  ;
              TITLE 'Field research table [ '+_head[1] +" ]";
              MODAL NOSYSMENU;
              ON INIT {||oldfilter:=dbfilter(),DBCLEARFILTER(),f_change(),PutMouse( "COMBO_101", "formt_s" ),Putmouse("Tabe_small","FormT_S",{9,36}) };
              ON RELEASE DbGoto(parto_da) ;
              BACKCOLOR m->BfColor

              On KEY ESCAPE Action FormT_S.release

         DEFINE STATUSBAR FONT "Arial" SIZE 12 BOLD
                STATUSITEM "" WIDTH 0 ACTION _dummy() FLAT
                STATUSITEM "Search ..." WIDTH 135 ACTION {||FormT_S.Tabe_small.value:="",FormT_S.Tabe_small.setfocus}
                STATUSITEM "Record in the table =" WIDTH 400
                STATUSITEM "Esc = Exit" WIDTH 110 ACTION FormT_S.Release FLAT
         END STATUSBAR

         IF !EMPTY(pfsee)
             DEFINE LABEL T_PfSee
                    ROW    thiswindow.Height-88
                    COL    20
                    WIDTH  60
                    HEIGHT 20
                    VALUE IF (!EMPTY(pfsee),pfsee+" : " ,"")
                    AUTOSIZE .t.
                    FONTCOLOR {0,0,255}
                    Fontbold .T.
                    TRANSPARENT .T.
                    VCENTERALIGN .T.
                    visible  (!EMPTY(pfsee))
             END LABEL

             DEFINE LABEL PfSee
                    ROW    thiswindow.Height-88
                    COL    20+FormT_s.t_pfSee.WIDTH
                    WIDTH  45
                    HEIGHT 20
                    VALUE IF (EMPTY(pfsee),"",hb_ValtoExp( FieldBlock( pfsee ) ) )
                    AUTOSIZE .t.
                    RIGHTALIGN .T.
                    FONTCOLOR {0,0,255}
                    Fontbold .T.
                    TRANSPARENT .T.
                    VCENTERALIGN .T.
                    visible  (!EMPTY(pfsee))
             END LABEL
             addH -= 20
         EndiF

         DEFINE FRAME Frame_1
                ROW    thiswindow.Height-185+addH
                COL    10
                WIDTH  thiswindow.width-40
                HEIGHT 93
                CAPTION "[ Filters ]"
                OPAQUE .T.
                FONTCOLOR NAVY
                BACKCOLOR m->BfColor
         END FRAME

         DEFINE LABEL Label_25
                ROW    thiswindow.Height-163 + addH
                COL    thiswindow.width-490
                WIDTH  65
                HEIGHT 21
                VALUE "the value: "
                AUTOSIZE .F.
                VCENTERALIGN .T.
                RIGHTALIGN .T.
                TRANSPARENT .T.
         END LABEL

         DEFINE RADIOGROUP RadioGroup_101
                ROW    thiswindow.Height-165 + addH
                COL    21
                WIDTH  115
                HEIGHT 30
                OPTIONS {"Search in:","Generic search"}
                VALUE 2
                ONCHANGE (Rv := this.value,IF (rv = 2,( FindHead( Real_fld[FormT_S.Combo_101.value];
                                                  ,_Head[FormT_S.Combo_101.value],"FormT_S","$CLEAN$") ),NIL ) ;
                                                  , FormT_S.Tabe_small.value:= "", FormT_S.Tabe_small.setfocus )
                SPACING 30
                READONLY NIL
                BACKCOLOR m->BfColor
         END RADIOGROUP

         DEFINE COMBOBOX Combo_101
                ROW    thiswindow.Height-164 + addH
                COL    280
                WIDTH  128
                HEIGHT 230
                ITEMS _head
                FONTBOLD .T.
                VALUE  1
                ON ENTER FormT_S.Tabe_small.value:= ""
                ONCHANGE (IF (Rv > 1, FormT_S.RadioGroup_101.value := 1,FormT_S.Tabe_small.value:= ""),FormT_S.Title:='Custom research table in the field [ '+_head[this.value]+' ]')
                ONGOTFOCUS (FormT_S.RadioGroup_101.value := 1)
         END COMBOBOX

         DEFINE TEXTBOX Tabe_small
                ROW    thiswindow.Height-165 + addH
                COL    thiswindow.width-418
                WIDTH  370
                HEIGHT 24
                FONTSIZE 12
                FONTBOLD .T.
                ON ENTER  (FormT_S.Button_Conferma.Setfocus,_pushkey(VK_SPACE))
                MAXLENGTH  35
                UPPERCASE .T.
         END TEXTBOX

         DEFINE BUTTONEX Button_Conferma
                ROW     thiswindow.Height-135 + addH
                COL    thiswindow.width-418
                WIDTH  100
                HEIGHT 33
                ACTION  if( Rv = 2, retval:=OUT_Small(FormT_S.small.value,.t.);
                ,FindHead(real_fld[FormT_S.Combo_101.value],_Head[FormT_S.Combo_101.value],"FormT_S",FormT_S.Tabe_small.value) )
                CAPTION "C&onferma"
                PICTURE "Minigui_EDIT_OK"
         END BUTTONEX

         DEFINE BUTTONEX Button_Esci
                ROW    thiswindow.Height-135 + addH
                COL    thiswindow.width-148
                WIDTH  100
                HEIGHT 33
                CAPTION "&Esci"
                ACTION  FormT_S.Release
                PICTURE "Minigui_EDIT_CANCEL"
         END BUTTONEX

         DEFINE TBROWSE Small AT 10,10  ;
                ALIAS cAlias WIDTH thiswindow.width-40 HEIGHT thiswindow.Height-197+addH ;
                VALUE Parto_da ;
                FONT 'Arial' SIZE 10 BOLD ;
                ON CHANGE (f_change(pfsee),IF (rv=2,FormT_S.Tabe_small.value := hb_valtostr( fieldget(fieldpos(m->NomeCampo) ) ),'') ) ;
                ON DBLCLICK (adummy := {,nrow,ncol,nflags},retval := Out_small(FormT_S.Tabe_small.value,.f.) )

                For ncl = 1 to len (_campi)
                    ADD COLUMN TO TBROWSE Small DATA FieldwBlock(_campi[ncl],select() ) ;
                        TITLE _Head[ncl] ;
                        SIZE  _dimensione[ncl] ;
                        COLORS {||If(eval(ecolor),CLR_HRED,CLR_BLACK)} ;
                        ,rgb(255,255,220) ,,;
                        ,CLR_YELLOW ;
                        ,{||If(eval(ecolor),CLR_HRED,CLR_BLUE)} ,,,,;
                        ,CLR_YELLOW ;
                        ,{||If(eval(ecolor),rgb(255,112,50),rgb(50,112,255)) }
                Next

                WITH OBJECT small
                     For ncl := 1 To :nColCount()
                         oCol            := :aColumns[ ncl ]
                         oCol:bHLClicked := {|nrp,ncp,nat,obr| HeadClick(obr,nrp,ncp,nat) }
                     Next
                END WITH

                Small:nHeightHead += 9
                Small:nWheelLines := 5
                Small:lNoResetPos := .T.
                Small:bKeyDown    := { | nKey | If( nKey == VK_RETURN , ;
                                     retval := OUT_Small(FormT_S.small.value,.f.), Nil ) }

         END TBROWSE

         DEFINE BUTTONEX Button_Print
                ROW    thiswindow.Height-135 + addH //310
                COL    thiswindow.width-285
                WIDTH  100
                HEIGHT 33
                CAPTION "&Stampa"
                ACTION  freport(_campi)
                PICTURE "hbprint_print"    //"STAMPAG" //"Minigui_EDIT_CANCEL"
                NOTABSTOP  .T.
                VISIBLE  .t.
         END BUTTONEX

     END WINDOW

     CENTER WINDOW FormT_S
     tbs_restart()
     FormT_S.Small.SetFocus
     ACTIVATE WINDOW FormT_S

     set filter to &oldFilter
     SET BROWSESYNC ON

     (alias())->(DBCLearIndex())
     #IFNDEF MEM_INDEX
     aeval(m->aSindex,{|x|(dbf())->(DBSetIndex( x ))} )
     #ENDIF

     dbsetorder(old_ord)
     set decimal to ndeci
     release m->NomeCampo,campo1,m->aSindex,small

Return retval
/*
*/
*-----------------------------------------------------------------------------*
PROCEDURE freport(_campi)
*-----------------------------------------------------------------------------*
      Local nCamp, aEst, aNomb, aLong, i, nCnt := 0, Lok := .F.
      Local cPrinter,dfwp

      nCamp := len(_campi) //Fcount()
      aEst  := DBstruct()

      aNomb := {0} ; aLong := {0}

      For i := 1 to nCamp

          AEval( aEst, {| x,y | if( x[ 1 ] == _campi[i], ( LoK := .T.,nCnt:= y ) ,'' ) } )

          IF lOk
             aadd(aNomb,aEst[nCnt,1])
             aadd(aLong,Max(100,Min(160,aEst[nCnt,3]*14)))
             Lok := .F.
          EndiF
      Next

      dfwp:= GetdefaultPrinter()
      cPrinter := GetPrinter()
      SetDefaultPrinter (cPrinter)
      printlist(DBF() , aNomb, aLong)
      SetDefaultPrinter (dfwp)

Return
/*
*/
*--------------------------------------------------------*
Procedure printlist(cBase, aNomb, aLong)
*--------------------------------------------------------*
    Local aHdr  := aClone(aNomb)
    Local aLen  := aClone(aLong)
    Local aHdr1 , aTot , aFmt ,nAlen ,mlen:=0
    Local cTitle := "Report of "+ cBase

    aeval(aLen, {|e,i| aLen[i] := e/9})
    hb_adel(aLen, 1,.T.)
    hb_adel(aHdr, 1,.T.)
    nAlen := len(aHdr)
    aHdr1 := array(nAlen)
    aTot  := array(nAlen)
    aFmt  := array(nAlen)
    afill(aHdr1, '')
    afill(aTot, .f.)
    afill(aFmt, '')

    aeval(alen,{|x|mlen += x})

    ( cBase )->( dbgotop() )

    if mlen > 150       // Does the sheet rotation need?
       DO REPORT ;
          TITLE  cTitle        ;
          HEADERS  aHdr1, aHdr ;
          FIELDS   aHdr        ;
          WIDTHS   aLen        ;
          TOTALS   aTot        ;
          NFORMATS aFmt        ;
          WORKAREA &cBase      ;
          LMARGIN  5           ;
          TMARGIN  3           ;
          PAPERSIZE DMPAPER_A4 ;
          PREVIEW  ;
          LANDSCAPE
    Else
       DO REPORT ;
          TITLE  cTitle        ;
          HEADERS  aHdr1, aHdr ;
          FIELDS   aHdr        ;
          WIDTHS   aLen        ;
          TOTALS   aTot        ;
          NFORMATS aFmt        ;
          WORKAREA &cBase      ;
          LMARGIN  5           ;
          TMARGIN  3           ;
          PAPERSIZE DMPAPER_A4 ;
          PREVIEW
    EndiF

do events
Return
/*
*/
*-----------------------------------------------------------------------------*
Procedure HeadClick( oBrw,nRowPix, nColPix, nAt )
*-----------------------------------------------------------------------------*
   HB_SYMBOL_UNUSED ( nRowpix )
   HB_SYMBOL_UNUSED (nAt)
   FormT_S .Combo_101.value := Max(oBrw:nAtCol(nColPix), 1)
   FormT_S .RadioGroup_101.value:= 1
   FormT_S.Tabe_small.setfocus

Return
/*
*/
*-----------------------------------------------------------------------------*
Procedure f_change(pfsee)
*-----------------------------------------------------------------------------*
   Local I_s := 'Record '+hb_ntoc(recno()) +' of '+ hb_ntoc(lastrec())
   Local nFiltered :=  f_filtro()
   if FormT_S.RadioGroup_101.value = 1 .and. nfiltered < lastrec()
      I_S += '   Found = ' + hb_ntoc(nFiltered)
   EndiF
   FormT_S.StatusBar.Item(3) :=  I_s
   IF !empty(pfsee)
      FormT_S.Pfsee.Value  :=  IF (!empty(pfsee),eval( FieldBlock( pfsee ) ) ,"")
   EndiF
Return
/*
*/
*-----------------------------------------------------------------------------*
Function f_filtro()
*-----------------------------------------------------------------------------*
   Local n_recno := recno(),m_filtrato
   go bottom
   m_filtrato := OrdKeyNo()
   go n_recno
Return m_filtrato
/*
*/
*-----------------------------------------------------------------------------*
Procedure FINDHEAD( x_campo,d_field,cWin,SeekFor)
*-----------------------------------------------------------------------------*
#include 'ord.ch'
Local x_macro, opos, oget, tmpfile,noper

#IFDEF MEM_INDEX
   tmpfile := "Mem:"+x_campo
#ELSE
   tmpfile := GetTempFolder()+"\Tmp_"+x_campo+".Cdx"
#ENDIF
m->cleanfile  := TmpFile

DEFAULT d_field to '', cWin to upper(thisWindow.name), SeekFor to ''

if empty(SeekFor)
   SeekFor := alltrim( InputBox( 'Field: '+ d_field , 'Searches (Empty or Cancel remove the filters)' ) )
EndiF

   if empty( SeekFor ) .or. SeekFor == "$CLEAN$"
      ordlistclear()
      aeval(m->aSindex,{|x|(dbf())->(DBSetIndex( x ))} )

      IF LEN(m->aSindex) > 0
         IF "FormT_S" == cWin
            opos := recno()
            oGet := _HMG_aControlIds [GetControlIndex ( "Small","FormT_S")]
            oGet:GoPos( opos )
         else
           ordsetfocus(m->aSindex[3])
         EndiF
      EndiF
      FormT_S.RadioGroup_101.value := 2
      TbS_Restart()
      Return
   EndiF

   if type(x_campo) == 'N'
      SeekFor := Strtran(charrem(" ",SeekFor),",",".")
      noper := F_Operat(SeekFor)
      SeekFor := hb_ntoc( TrueVal(SeekFor),FieldDeci(FieldNum(x_campo)) ) // avoid errors between commas and dots, and undesidered chars
      if empty(noper)
         x_macro := SeekFor +'= ('+x_campo+')'
      Else
         x_macro := x_campo+ nOper +SeekFor
      Endif

   elseif type(x_campo) == 'D'
      x_macro := for_sep(seekfor,x_campo)

   elseif type(x_campo) == 'L'
      SeekFor := IF (hb_Ascan({"1","T","ON","S","Y",".T.",,.T.},Seekfor) > 0,".T.",".F." )
      x_macro := SeekFor +'='+ x_campo

   else   //  Character field
      x_macro := [Upper("] + SeekFor + [")] + " $ upper(" + x_campo + ")"
   EndiF

   if NetFileLock(5 )
      ordlistclear()
      Ferase(tmpfile)
      INDEX ON  &x_campo TAG Small TO (tmpfile) For &x_macro
      DBunlock()
   else
      HMG_ALERT ("Search temporarily disabled !",2,"File Busy !",4 )
      Return
   EndiF

   if eof()
      ordlistclear()
      aeval(m->aSindex,{|x|(dbf())->(DBSetIndex( x ))} )
      IF LEN(m->aSindex) > 0
         IF "FormT_S" == cWin
            ordsetfocus(m->aSindex[1])
         Else
            ordsetfocus(m->aSindex[3])
         EndiF
      EndiF
      Small:SetFilter( "", "" )
      go top

      HMG_ALERT ("There are no matches !",2,"Search",3)
      FormT_S.RadioGroup_101.value := 1
      FormT_S.Tabe_small.value := ''
      Putmouse("Button_Conferma","FormT_S")
      _Pushkey(VK_TAB)   //reposition focus on search textbox
      _Pushkey(VK_TAB)
      _Pushkey(VK_TAB)
   EndiF

   IF "FormT_S" == cWin
      FormT_S.Title:='Custom research table in the field [ '+ d_field+' ]'
   EndiF
   TbS_Restart()

Return
/*
*/
*-----------------------------------------------------------------------------*
Function For_Sep(arg1,x_campo)
*-----------------------------------------------------------------------------*
Local Rtv ,nRtv,cDat,tmp
arg1 := StrTRan(arg1,"*","/")
arg1 := StrTRan(arg1,".","/")
arg1 := StrTRan(arg1,"-","/")

Rtv  := hb_aTokens(arg1,"/")
nRtv := len(rtv)
IF nRtv = 2
   if len(Rtv[1]+Rtv[2]) = 0
      nRtv := 0
   EndiF
EndiF
Switch nRtv
       case 1  //year
            if len(rtv[1]) < 4
               tmp := left(hb_ntoc(year( date() ) ) ,2 )+right( rtv[1],2)
            Else
               tmp := Rtv[1]
            EndiF
            cDat := Tmp +[ = year( * )]
            Exit

       case 2  // month and year
            if len(rtv[2]) < 4
               tmp := "["+left(hb_ntoc(year( date() ) ) ,2 )+right( rtv[2],2)
            Else
               tmp := "["+Rtv[2]
            EndiF
            tmp += strzero(val(Rtv[1]),2)
            cDat := Tmp + "] = Left(DTOS( * ),6)"
            Exit

       case 3  // full date
            cdat := [ctod("] + arg1 + [")] + [ = * ]
            Exit

       DEFAULT
            cdat := [ctod(" /  /  ")] + [ = * ]
EndSwitch
cdat := strTran(cdat,"*",x_campo)
Return cdat
/*
*/
*-----------------------------------------------------------------------------*
Function F_Operat(arg1)
*-----------------------------------------------------------------------------*
   Local Rtv:="", c
   DEFAULT arg1 to ""
   arg1 := charrem(" ",arg1)
   For each c in arg1
       if c $ "<>="
          Rtv += c
       Endif
   Next
   If len(rtv) > 1 .and. left(Rtv,1) == "="
      Rtv := charMirr(rtv)
   Endif
Return rtv
/*
*/
*-----------------------------------------------------------------------------*
FUNCTION Trueval(string)
*-----------------------------------------------------------------------------*
   Local c, outval:=''
   default string to ''
   string := charrem(" ",string)
   For each c in string
       If c  $ "-0123456789."
          outval += C
       ENDIF
   NEXT
Return VAL(outval)
/*
*/
*-----------------------------------------------------------------------------*
Procedure TbS_Restart()
*-----------------------------------------------------------------------------*
   dbgotop()
   FormT_s.Small.setfocus
   small:reset()
   Small:Gotop()
   small:BugUp()
Return
/*
*/
*-----------------------------------------------------------------------------*
static FUNCTION Find_S()
*-----------------------------------------------------------------------------*
Local posizione := small:nLogicPos() //Local posizione:=FormT_S.small.value //Local posizione := small:nLogicPos()
Local string    := alltrim(FormT_S.tabe_small.value)
Local ritorno   :=.t. , KeySeek := UPPER(indexkey(indexord()))
Local Ks:= {|x| if("(" $ x,substr(x,at("(",x)+1,rat([)],x)-at("(",x)-1),x)}
Local Field_key := eval(ks,KeySeek)
Local campo , opos, oGet
campo := if( empty(Field_key),m->NomeCampo,Field_key)

      IF "*" <> Left(string,1) .and. (m->campo1 $ KeySeek .or. indexord() > 0)
         IF "VAL" $ keySeek .or. if("+" $ KeySeek,.f.,valtype(field->&Field_key)= "N")
            string := val(string)
         EndiF
         if dbseek(string)
            opos :=  recno()
            oGet := _HMG_aControlIds [GetControlIndex ( "Small","FormT_S")]
            oGet:GoPos( opos )
            if m->CAMPO1 = UPPER(CAMPO)
               FormT_S.Tabe_small.Value  := (alias())->&(m->NomeCampo)
            else
               FormT_S.Tabe_small.Value  := (alias())->&(m->CAMPO1)
            EndiF
         else
            msginfo("Data not present in the archive! ",[Index search])
            FormT_S.Tabe_small.Value  := ''
            TbS_Restart()
            ritorno:=.f.
         EndiF
      else
         if Left(string,1) = "*"
            string:=substr(string,2)
         EndiF
         dbgotop()
         if valtype((alias())->&(m->NomeCampo))=='C'
            locate For string $ upper(&(campo))
         elseif valtype((alias())->&(m->NomeCampo))=='N'
            locate For val(string) = &(campo)
         elseif valtype((alias())->&(m->NomeCampo))=='D'
            locate For ctod(string) = &(campo)
         EndiF
         if found()
             FormT_S.Small.Refresh
         else
            FormT_S.Small.Value  := posizione
            msginfo("Data not present in the field "+m->NomeCampo,[Search without index])
            FormT_S.Tabe_small.Value  := ''
            TbS_Restart()
            ritorno:=.f.
         EndiF
      EndiF

Return ritorno
/*
*/
*-----------------------------------------------------------------------------*
static Function Out_Small(retval,chkFind)
*-----------------------------------------------------------------------------*
     DEFAULT chkfind to .t.
     IF (if(chkFind,Find_S(),.t.))
        m->CRCN := RECNO()
        retval := (alias())->&(m->NomeCampo)
        IF !empty(retval) .and. chkfind
           FormT_S.Small.setfocus
           small:BugUp()
           do events
           if msgYesNo("Confirm the selected value ?")
              retval := (alias())->&(m->NomeCampo)
              FormT_S.Release
           else
              retval:=''
           EndiF
        else
           FormT_S.Release
        EndiF
     else
       retval:=''
     EndiF

Return retval
/*
*/
*------------------------------------------------------------------------------*
 Procedure PutMouse( obj, form, rect )
*------------------------------------------------------------------------------*
 Local ocol,orow
 DEFAULT form to ThisWindow.name, rect to {20,40}

 ocol  := GetProperty ( Form , "col" ) + GetProperty ( Form, OBJ ,"Col" )+rect[1]
 orow  := GetProperty ( Form , "row" ) + GetProperty ( Form, OBJ ,"row" )+rect[2]

 SETCURSORPOS(ocol,orow)
 _SETFOCUS(obj,FORM)

Return
/*
*/
*------------------------------------------------------------------------------*
Function find_len(campi,MinValue)
*------------------------------------------------------------------------------*
Local ritorna := {}, k,astrutt ,nMax
DEFAULT MinValue to 60
astrutt:=(alias())->(dbstruct())

For k:=1 to len(astrutt)
    if ascan(campi,astrutt[k,1]) > 0
       nMax := Max(MinVAlue, Max( GetTextWidth( 0, repl("B",astrutt[K,3] ) ) , GetTextWidth( 0,astrutt[K,1]) ) )
       aadd(ritorna,nMax)
    EndiF
Next

Return ritorna
/*
*/
*------------------------------------------------------------------------------*
Function combo_choice( arg1 ,argT, argl)
*------------------------------------------------------------------------------*
Local rtv := 0
DEFAULT arg1 to {""},argt to "Choose the name of the field to highlight :" ,argL to "[ I choose to highlight: ]"

DEFINE WINDOW c_choice AT 171 , 99 WIDTH 360 HEIGHT 164 TITLE argT  ICON "Icon_Cas.Ico" MODAL NOSIZE BACKCOLOR {255,255,220}

     ON KEY ESCAPE ACTION ( rtv := 0,c_choice.release )

     DEFINE FRAME Frame_1
            ROW    10
            COL    10
            WIDTH  330
            HEIGHT 60
            CAPTION argL
            OPAQUE .T.
     END FRAME

     DEFINE COMBOBOX Combo_1
            ROW    30
            COL    20
            WIDTH  310
            HEIGHT 100
            ITEMS arg1
     END COMBOBOX

     DEFINE BUTTONEX ButtonEX_1
           ROW    80
           COL    20
           WIDTH  100
           HEIGHT 30
           ACTION (rtv:= c_choice.Combo_1.value, c_choice.release )
           CAPTION "C&onfirm"
           PICTURE "MINIGUI_EDIT_OK"
           ICON NIL
     END BUTTONEX

    DEFINE BUTTONEX ButtonEX_2
           ROW    80
           COL    230
           WIDTH  100
           HEIGHT 30
           CAPTION "&Cancel"
           PICTURE "MINIGUI_EDIT_CANCEL"
           ICON NIL
           ACTION ( rtv := 0,c_choice.release )
     END BUTTONEX

END WINDOW

c_choice.center
c_choice.activate

return rtv
