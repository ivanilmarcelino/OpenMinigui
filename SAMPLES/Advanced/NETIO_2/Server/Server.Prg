/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Copyright 2002 Roberto Lopez <harbourminigui@gmail.com>
 * http://harbourminigui.googlepages.com/
 *
 *********************************************************************************
 * Simple Server for test with NETIO
 *
 * Created by : Paulo Sérgio Durço - 23/10/2009 8:30 Hrs
 *
 * Thanks Master Roberto Lopez for your great work.
 *
 * Enhanced By Pierpaolo Martinello 2018-2020
 *********************************************************************************
*/

#include "minigui.ch"

#TRANSLATE ZAPS(<X>) => ALLTRIM(STR(<X>) )
#define NTrim( n ) LTRIM( STR( n,20, IF( n == INT( n ), 0, set(_SET_DECIMALS) ) ))
#define EM_SETPASSWORDCHAR      0x00CC
#define CBSTATUS  "TEST"
#define ITA 2
#define ENG 1

Memvar c_TITLE, pListenSocket , c_path, MAIN_DATA, ListNet, cPasswd, cCryptKey, LNG, cLNG ,aLbl

Function Main

    Local c_oPath
    Local c_Port := "2941" 
    Local c_Addr := "127.0.0.1"
    Local nTime  := 5000
    Local cLNG := "ENG"
    Local aLng   := {"ENG","ITA"}
    Local Listnet :={"127.0.0.1"}

    Public c_Title := "HMG Server NetIO 1.9 - "+if(IsExe64(),"64","32")+" Bit. "+CBSTATUS
    Public pListenSocket
    Public c_Path  := HB_CWD()
    Public cPasswd := "secret"
    PRIVATE cCryptKey := REPL( "#$@%&", 2 )
    Public LNG := 1
    Public aLbl := { ;
                    "iMnu1"               => {'About...','Circa...'} ,;
                    "iMnu2"               => {'Restart Server','Restart Server'} ,;
                    "iMnu3"               => {'Configurations','Configurazione'} ,;
                    "iMnu4"               => {'Exit Server','Termina il Server'} ,;
                    "Label_1.VALUE"       => {"Port","Porta"} ,;
                    "Check_1.CAPTION"     => {"Mask Password","Nascondi Pwd"} ,;
                    "Label_3.VALUE"       => {"Database Path","Percorso Dbf"} ,;
                    "Label_4.VALUE"       => {"Ip Address","Indirizzo IP"} ,;
                    "IpAddress_1.TOOLTIP" => {"Enter the address of this server.","Inserire l'indirizzo di questo server."} , ;
                    "Button_1.ONCLICK"    => {"Choose database path:","Scegli il percorso del database:"} ,;
                    "Button_2.TOOLTIP"    => {"Enter the address of this server.","Inserisce l'indirizzo di questo server."} , ;
                    "Button_3.CAPTION"    => {"Save && Restart","Salva && Riavvia"} , ;
                    "Button_4.CAPTION"    => {"Undo config","Annulla modifiche"}  , ;
                    "Button_5.CAPTION"    => {"Close Config","Esci da Configura"} , ;
                    "MSGTIMEOUT1"         => {"Hmg Server NetIO is running at Port: ","Server Hmg NetIO è in esecuzione sulla porta: "} ,;
                    "MSGTIMEOUT1.1"       => {"Database Path : ","Percorso database: "} ,;
                    "MSGTIMEOUT2"         => {"Hmg Server NetIO is stopped!","Il server Hmg NetIO è stato arrestato!"} ;
                    }

    SET MULTIPLE OFF
    SET DATE TO BRITISH
    SET EPOCH TO 2000
    SET NAVIGATION EXTENDED
    SET INTERACTIVECLOSE QUERY

    REQUEST DBFNTX
    REQUEST DBFCDX
    DBSETDRIVER("DBFCDX")

    pListenSocket := NIL

    if !File("Config.ini")
       Begin ini file ("Config.ini")
             Set Section "CONFIGURATION" ENTRY "Port"     To c_Port
             Set Section "CONFIGURATION" ENTRY "Path"     To c_Path
             Set Section "CONFIGURATION" ENTRY "Address"  To c_Addr
             Set Section "CONFIGURATION" ENTRY "Password" To encripta( cPasswd, cCryptKey )
             Set Section "CONFIGURATION" ENTRY "LANG"     To "ENG"
       End ini
    else
       Begin ini file ("Config.ini")
             Get c_Port   Section "CONFIGURATION" ENTRY "Port" default c_Port
             Get c_Path   Section "CONFIGURATION" ENTRY "Path" default c_path
             Get c_Addr   Section "CONFIGURATION" ENTRY "Address"
             Get cPasswd  Section "CONFIGURATION" ENTRY "Password" default encripta( cPasswd, cCryptKey )
             Get cLNG     Section "CONFIGURATION" ENTRY "LANG"
       End Ini

       LNG := max (1,ascan(aLng,cLNG))
       cPasswd := decripta( cPasswd, cCryptKey )
    Endif

    AEVAL (INETGETHOSTS(NETNAME()),{|x|aadd(Listnet,x)} )

    c_oPath := c_Path

    IF LNG = 2
       SET LANGUAGE TO ITALIAN
       _HMG_MESSAGE [2] := Space(10)+"Uscire da questo server?"
    Else
        _HMG_MESSAGE [2] := Space(15)+"Quit this server ?"
    ENDIF

    DEFINE WINDOW Form_1 ;
        AT 0,0 ;
        WIDTH 595 HEIGHT 195 ;
        TITLE 'Configurations -- '+c_Title  ;
        MAIN NOSHOW;
        ON INIT  Restart_Server(1500) ;
        ICON "MAIN" ;
        NOTIFYICON "STOP" ;
        NOTIFYTOOLTIP c_Title ;
        ON NOTIFYCLICK Show_Status(5000) ;
        NOMAXIMIZE ;
        ON MINIMIZE Form_1.Hide

        ON KEY ESCAPE action FORM_1.hide

        DEFINE NOTIFY MENU
            ITEM aLbl ["iMnu1"] [LNG] ACTION MsgInfo( 'WinCrf Server NETIO '+ CRLF+CRLF+"By Pierpaolo Martinello - Minigui Team.", c_TITLE )
            ITEM aLbl ["iMnu2"] [LNG] ACTION Restart_Server()
            ITEM aLbl ["iMnu3"] [LNG] ACTION Show_Config()  IMAGE 'TOOL'
            SEPARATOR
            ITEM aLbl ["iMnu4"] [LNG] ACTION Form_1.Release
        END MENU

        DEFINE FRAME Frame_1
                ROW 10
                COL 10
                WIDTH  560
                HEIGHT 140
                OPAQUE .T.
        END FRAME

        DEFINE LABEL Label_1
               ROW 22
               COL 22
               WIDTH  40
               HEIGHT 20
               VALUE aLbl ["Label_1.VALUE"] [LNG]
        END LABEL

        DEFINE TEXTBOX Text_1
               ROW  20
               COL 110
               WIDTH  45
               HEIGHT 20
               VALUE c_Port
               INPUTMASK "99999"
        END TEXTBOX

        // This label has been inserted to allow you to reveal the password avoiding curious and geeks
        // Esta etiqueta se ha agregado para permitirle revelar su contraseña evitando curiosos y geeks
        // Questa label è stata inserita per permettere di svelare la password evitando curiosi e smanettoni
        DEFINE LABEL Label_5
               ROW    20
               COL    162
               WIDTH  8
               HEIGHT 20
               VALUE " "
               TRANSPARENT .T.
               BACKCOLOR YELLOW
               ACTION ( Form_1.check_1.visible := !Form_1.check_1.visible )
        END LABEL

        DEFINE LABEL Label_2
               ROW    20
               COL    170
               WIDTH  60
               HEIGHT 20
               VALUE "Password:"
               TRANSPARENT .T.
               BACKCOLOR YELLOW
               CENTERALIGN .f.
               VCENTERALIGN .T.
        END LABEL

        DEFINE TEXTBOX Text_3
               ROW 20
               COL 232
               WIDTH  220
               HEIGHT 20
               VALUE m->cPasswd
               PASSWORD .T.
        END TEXTBOX

        DEFINE CHECKBOX Check_1
               ROW    16
               COL    460
               WIDTH  100
               HEIGHT 28
               CAPTION aLbl["Check_1.CAPTION"] [LNG]
               VALUE .T.
               ONCHANGE ShowPassword()
               VISIBLE .F.
        END CHECKBOX

        DEFINE LABEL Label_3
               ROW  52
               COL 22
               WIDTH  90
               HEIGHT 20
               VALUE albl["Label_3.VALUE"] [LNG]
        END LABEL

        DEFINE TEXTBOX Text_2
               ROW 50
               COL 110
               WIDTH  410
               HEIGHT 20
               VALUE c_Path
               READONLY .T.
               MAXLENGTH 100
        END TEXTBOX

        DEFINE BUTTONEX Button_1
               ROW 50
               COL 530
               CAPTION "..."
               ON CLICK SetProperty( "Form_1", "Text_2", "Value", (c_Path := GetFolder( aLbl["Button_1.ONCLICK"] [LNG],hb_cwd()),if( empty(c_Path),c_Path:=c_oPath,c_Path) ) )
               WIDTH  30
               HEIGHT 20
               BACKCOLOR WHITE
               GRADIENTFILL { { RED ,YELLOW } }
               NOTABSTOP .T.
        END BUTTONEX

        DEFINE LABEL Label_4
                ROW 82
                COL 22
                WIDTH  90
                HEIGHT 20
                VALUE aLbl["Label_4.VALUE"] [LNG]
        END LABEL

        DEFINE IPADDRESS IpAddress_1
               ROW    80
               COL    110
               WIDTH  125
               HEIGHT 20
               FONTNAME 'Arial'
               VALUE Str2ip(c_addr)
               TOOLTIP aLbl["IpAddress_1.TOOLTIP"] [LNG]
        END IPADDRESS

        DEFINE BUTTONEX Button_2
               ROW 80
               COL 247
               CAPTION "..."
               ON CLICK  CorreggiIp(Listnet,Scegli(listnet,Space(10)+{"Choose the appropriate network segment",Space(5)+"Scegli il segmento di rete appropriato"}[LNG],"",1))
               WIDTH  30
               HEIGHT 20
               TOOLTIP aLbl["Button_2.TOOLTIP"] [LNG]
               BACKCOLOR WHITE
               GRADIENTFILL { { RED ,YELLOW } }
               NOTABSTOP .T.
        END BUTTONEX

        DEFINE BUTTONEX Button_3
               ROW 110
               COL 20
               CAPTION aLbl["Button_3.CAPTION"] [LNG]
               ON CLICK ( Form_1.Hide, Write_Ini(.t.) )
               WIDTH  100
               HEIGHT 28
               BACKCOLOR WHITE
               GRADIENTFILL { {1, GREEN,GREEN } }
        END BUTTONEX

        DEFINE BUTTONEX Button_4
               ROW 110
               COL 247
               CAPTION aLbl["Button_4.CAPTION"] [LNG]
               ON CLICK Start_server (nTime,.t.)
               WIDTH  100
               HEIGHT 28
               BACKCOLOR WHITE
               GRADIENTFILL { {1, {255,255,0},{255,255,0} } }
        END BUTTONEX

        DEFINE BUTTONex Button_5
               ROW 110
               COL 460
               CAPTION aLbl["Button_5.CAPTION"] [LNG]
               ON CLICK Form_1.Hide
               WIDTH  100
               HEIGHT 28
               BACKCOLOR WHITE
               GRADIENTFILL { {1, {0,255,255},{0,255,255} }}
               DEFAULT .T.
        END BUTTONex

    END WINDOW

    CENTER WINDOW Form_1

    ACTIVATE WINDOW Form_1

    RELEASE cCryptKey

Return Nil
/*
*/
* Show config window
*-----------------------------------------------------------------------------*
Procedure Show_Config()
*-----------------------------------------------------------------------------*
    Form_1.Restore
Return
/*
*/
* Start the Server
*-----------------------------------------------------------------------------*
Function Start_Server(nTime,Norestart)
*-----------------------------------------------------------------------------*
     Local c_Port   := "2941"
     Local c_Addr   := "127.0.0.1"
     LOCAL lRPC     := .T.

     LOCAL cRootDir := "."
     LOCAL nCompressionLevel    := 9
     DEFAULT Norestart to .f.

     Begin ini file ("Config.ini")
           Get c_Port  Section "CONFIGURATION" ENTRY "Port"
           Get c_Path  Section "CONFIGURATION" ENTRY "Path" default c_path
           Get c_Addr  Section "CONFIGURATION" ENTRY "Address"
           Get cPasswd Section "CONFIGURATION" ENTRY "Password" default encripta( "secret", cCryptKey )
     End Ini

    cPasswd := decripta( cPasswd, cCryptKey )

    Form_1.Text_1.Value      := c_Port
    Form_1.Text_2.Value      := c_Path
    Form_1.Text_3.Value      := cPasswd
    Form_1.IpAddress_1.Value := Str2ip(c_Addr)

    if Norestart
       Return Nil
    Endif

//pListenSocket := netio_mtserver( Val( AllTrim(Form_1.Text_1.Value) ), c_Addr )
/*
    netio_MTServer( [<nPort>], [<cIfAddr>], [<cRootDir>],
                   [<xRPC> | <sFuncSym> | <hValue>],
                   [<cPasswd>], [<nCompressionLevel>], [<nStrategy>],
                   [<sSrvFunc>] )
*/
// NETIO_MTSERVER( Val( AllTrim(Form_1.Text_1.Value) ) , c_Addr , cRootDir , lRPC , cPasswd , nCompressionLevel , nStrategy )

   pListenSocket := NETIO_MTSERVER( Val( AllTrim(Form_1.Text_1.Value) ) , c_Addr , cRootDir , lRPC , cPasswd , nCompressionLevel )

    if empty( pListenSocket )
        SetProperty( "Form_1","NOTIFYICON","STOP" )
        MsgStop( "Cannot start server!", c_TITLE )
    else
        SetProperty( "Form_1","NOTIFYICON","MAIN" )
        Show_Status(nTime)
    endif

Return Nil
/*
*/
* Stop the Server
*-----------------------------------------------------------------------------*
Procedure Stop_Server(nTime)
*-----------------------------------------------------------------------------*
     if !empty( pListenSocket )
        SetProperty( "Form_1","NOTIFYICON","STOP" )
        netio_serverstop( pListenSocket )
        pListenSocket := NIL
        Show_Status(nTime)
     endif
Return
/*
*/
* Show Server status
*-----------------------------------------------------------------------------*
Procedure Show_Status(nTime)
*-----------------------------------------------------------------------------*
DEFAULT nTime to 5000
     if !empty( pListenSocket )
        MessageboxTimeout(aLbl ["MSGTIMEOUT1"] [LNG] ;
                 + AllTrim(Form_1.Text_1.Value) + CHR(13)+CHR(13) ;
                 + aLbl ["MSGTIMEOUT1.1"] [LNG] ;
                 + AllTrim(Form_1.Text_2.Value), c_TITLE ,64,nTime)
     else
        MessageboxTimeout( aLbl ["MSGTIMEOUT2"] [LNG], c_TITLE ,64,2000)
     endif
Return
*/

/*
*/
* Restart the Server
*-----------------------------------------------------------------------------*
Procedure Restart_Server(ntime)
*-----------------------------------------------------------------------------*
          Stop_Server(nTime)

          Start_Server(nTime)
Return
/*
*/
* Write the INI file with content in textbox on Form_1 (Config Window)
*-----------------------------------------------------------------------------*
Procedure Write_Ini(lforce)
*-----------------------------------------------------------------------------*
     LOCAL ip:='', gip := Form_1.ipaddress_1.Value, aLng := {"ENG","ITA"}
     default lforce to .F.

     if gip[1] = 0
        gip := {127,0,0,1}
     Endif

     Aeval(gip ,{|x|ip += hb_ntos(x)+"."})
     ip := RemRight(ip,".")

     if !File( "Config.ini" ) .or. lForce
        Begin ini file "Config.ini"
              Set Section "CONFIGURATION" ENTRY "Port"     To Form_1.Text_1.Value
              Set Section "CONFIGURATION" ENTRY "Path"     To Form_1.Text_2.Value
              Set Section "CONFIGURATION" ENTRY "Address"  To ip
              Set Section "CONFIGURATION" ENTRY "Password" To encripta( Form_1.Text_3.Value, cCryptKey )
              Set Section "CONFIGURATION" ENTRY "LANG"     To aLng [LNG]
        End ini

     endif

     Restart_Server()
Return
/*
*/
*-----------------------------------------------------------------------------*
Procedure ShowPassword()
*-----------------------------------------------------------------------------*
  LOCAL cPass := Form_1.Text_3.VALUE
  LOCAL lShowPass := !Form_1.Check_1.VALUE

  If lShowPass
     SendMessage(Form_1.Text_3.HANDLE, EM_SETPASSWORDCHAR, 0, 0)
     Form_1.Text_3.REFRESH
  Else
     Form_1.Text_3.RELEASE
     DoEvents()
     DEFINE TEXTBOX Text_3
       PARENT Form_1
       ROW    20
       COL    230
       WIDTH  220
       HEIGHT 20
       VALUE  cPass
       PASSWORD  .T.
     END TEXTBOX
  EndIf

Return
/*
* Add By Pierpaolo 2018-20
*/
*-----------------------------------------------------------------------------*
Procedure CorreggiIp(Listnet,arg1)
*-----------------------------------------------------------------------------*
Local ipLav
   if arg1 = 0
      Return
   Endif

   ipLav := Listnet[arg1]
   iplav := hb_ATokens( iplav, ".", .T., .F. )
   aeval(iplav,{|x,y|iplav[y]:=val(x)} )

   Form_1.ipaddress_1.value := iplav

Return
/*
*/
*-----------------------------------------------------------------------------*
Function Str2Ip(arg1)
*-----------------------------------------------------------------------------*
local aIp
DEFAULT arg1 to "127.0.0.1"
aIp := hb_ATokens( arg1, ".", .T., .F. )
aeval(aIp,{|x,y|aIp[y] :=val(x)} )
Return aIp
/*
*/
*-----------------------------------------------------------------------------*
FUNCTION Decripta ( cString,chiave )
*-----------------------------------------------------------------------------*
   LOCAL nTam, cCrypt := "", i

   Chiave  := iif( Empty( Chiave ), "@#$%", Chiave )
   Chiave  := _adc( chiave, ordinale( 1 ) )
   cString := iif( Empty( cString ) .OR. Len ( cString ) < 3  ;
      , "NOPASSW0RD",cString )
   nTam := Len( cString )
   DO WHILE Len( Chiave ) < nTam
      Chiave += Chiave
   ENDDO
   cCrypt := ""
   FOR i := 1 TO nTam
      cCrypt += Chr( Asc( SubStr( cString, i, 1 ) ) - Asc( SubStr( Chiave, i, 1 ) ) )
   NEXT

Return cCrypt
/*
*/
*-----------------------------------------------------------------------------*
FUNCTION Encripta( cString,chiave )
*-----------------------------------------------------------------------------*
   LOCAL nTam, cCrypt := "", i

   Chiave  := iif( Empty( Chiave ), "@#$%", Chiave )
   Chiave  := _adc( chiave, ordinale( 1 ) )
   cString := iif( Empty( cString ) .OR. Len ( cString ) < 3  ;
      , "NOPASSW0RD",cString )
   nTam := Len( cString )
   DO WHILE Len( Chiave ) < nTam
      Chiave += Chiave
   ENDDO
   cCrypt := ""
   FOR i := 1 TO nTam
      cCrypt += Chr( Asc( SubStr( cString, i, 1 ) ) + Asc( SubStr( Chiave, i, 1 ) ) )
   NEXT

Return cCrypt
/*
*/
* questa funzione permette di usare la stessa password codificandola
* in base all'id, ma in questo caso non serve
* this function allows you to use the same password by encoding it
* based on id, but in this case it is not needed
*-----------------------------------------------------------------------------*
FUNCTION Ordinale( nmb )
*-----------------------------------------------------------------------------*
   LOCAL nString
   nmb     := iif( nmb == NIL, 0, nmb )
   nString := Right( Str( nmb ), 1 )

Return nString
/*
*/
*-----------------------------------------------------------------------------*
FUNCTION _adc ( iString, adc )
*-----------------------------------------------------------------------------*
   LOCAL lRes := '', i
   FOR i = 1 TO Len ( iString )
      lres += SubStr( iString, i, 1 )
      IF i = Val( adc )
         lres += adc
      ENDIF
   NEXT

Return lres
/*
*/
*-----------------------------------------------------------------------------*
FUNCTION _sdc ( iString, adc )
*-----------------------------------------------------------------------------*
   LOCAL lRes := '', i
   FOR i = 1 TO Len ( iString )
      IF i = Val( adc )
         LOOP
      ENDIF
      lres += SubStr( iString, i, 1 )
   NEXT

Return lres
/*
*/
*-----------------------------------------------------------------------------*
Function Scegli(opt,title,note,def)
*-----------------------------------------------------------------------------*
   local r:= 0 , S_HG
   default title to {"Choice of prints","Scelta stampe"}[LNG], opt to {"This Card", "All","Questa Scheda","Tutte"} [LNG]
   Default note to "", def to 1
   note := space(10)+ note
   s_hg := len (opt)*25 + 150

   DEFINE WINDOW SCEGLI WIDTH 300 HEIGHT S_hg TITLE "My Choice" ;
                 ICON NIL MODAL NOSIZE NOSYSMENU CURSOR NIL ;
                 ON INIT Load_Scegli_base(title, def, note) ;

          DEFINE STATUSBAR BOLD
                 STATUSITEM note
          END STATUSBAR

          DEFINE RADIOGROUP RadioGroup_1
                 ROW    11
                 COL    22
                 WIDTH  230
                 HEIGHT 59
                 OPTIONS OPT
                 VALUE 1
                 FONTNAME "Arial"
                 FONTSIZE 9
                 SPACING 25
                 ON CHANGE PutMouse( "Button_1", "Scegli")    // Auto choice ok
          END RADIOGROUP

          DEFINE BUTTONEX Button_1
                 ROW    S_HG - 105
                 COL    20
                 WIDTH  100
                 HEIGHT 40
                 PICTURE "Minigui_EDIT_OK"
                 CAPTION _HMG_aLangButton[8]
                 ACTION  ( r:= Scegli.RadioGroup_1.value ,Scegli.release)
                 FONTNAME  "Arial"
                 FONTSIZE  9
          END BUTTONEX

          DEFINE BUTTONEX Button_2
                 ROW    S_Hg - 105
                 COL    174
                 WIDTH  100
                 HEIGHT 40
                 PICTURE "Minigui_EDIT_CANCEL"
                 CAPTION _HMG_aLangButton[7]
                 ACTION   Scegli.release
                 FONTNAME  "Arial"
                 FONTSIZE  9
          END BUTTONEX

   END WINDOW

   Scegli.center
   Scegli.activate
   RELEASE FONT DlgFont
Return r
/*
*/
*-----------------------------------------------------------------------------*
Procedure load_Scegli_base(title,def, note)
*-----------------------------------------------------------------------------*
   Local nWidthCli := Max( 286 , GetTextWidth( Getdc (this.handle), note , GetFontHandle( GetDefaultFontname() ) ) )
   ON KEY Return OF SCEGLI ACTION ( SCEGLI.BUTTON_1.SETFOCUS, _PUSHKEY( VK_SPACE ) )
   escape_on('Scegli')
   Scegli.Title := Title
   Scegli.RadioGroup_1.value := def
   Scegli.width := nWidthCli + iif( _HMG_IsXPorLater, 2, 1 ) * GetBorderWidth()
   Scegli.Button_2.col := Scegli.width -125
   PutMouse( "RadioGroup_1", "Scegli")

Return
/*
*/
*------------------------------------------------------------------------------*
Procedure PutMouse( obj, form, rect )
*------------------------------------------------------------------------------*
   Local ocol, orow

   DEFAULT form TO "Win_1", rect TO {20,40}, obj to m->aControlList[Form_1.Combo_2.VALUE]

   if obj = "Win_1"
      obj := m->cNameControl
   endif

   ocol  := GetProperty( Form, "col" ) + GetProperty( Form, obj, "Col" ) + rect [1]
   orow  := GetProperty( Form, "row" ) + GetProperty( Form, obj, "row" ) + rect [2]

   _SETFOCUS( obj, FORM )
   SETCURSORPOS( ocol, orow )

Return
/*
*/
*-----------------------------------------------------------------------------*
Procedure ESCAPE_ON(ARG1)
*-----------------------------------------------------------------------------*
     local WinName:=if(arg1==NIL,procname(1),arg1)
     IF upper(WinName)<>'OFF'
        _definehotkey(arg1,0,27,{||_releasewindow(arg1)})
     else
        ON KEY ESCAPE ACTION nil
     EndIF
Return
/*
* End of Patch Pierpaolo 2018-2020
*/
