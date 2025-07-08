/*
  Permette di visualizzare:
  - Una finestra con un messaggio 
  - Una finestra di dialogo con un messaggio 

  Sintassi
   MyMsgCustom([sMessage], [sTitle], [nRowWnd], [nColWnd], [sWndIcon], [aBtns], [sAlertIcon], [aBtnBColor], 
              [aLblProperty], [aLblOpt]) -> nChoice

  Parametri:
   sMessage
      Mmessaggio di tipo carattere da visualizzare. Può essere diviso su più righe utilizzando la funzione 
      HB_EOL(). DEFAULT -> "" (no messaggio)                                                                    
      Es: sMessage := "Messaggio prima riga" + HB_EOL() + "Messaggio seconda riga"              
   
   sTitle         
      Intestazione finestra. DEFAULT -> ""                      
   
   nRowWnd,nColWnd
      Coordinate dell'angolo superiore sinistro della finestra.
      E' possibile fare in modo che la finestra sia:
      - Centrata rispetto al DESKTOP, passando al parametro nRowWnd il valore -1
      - Centrata rispetto alla finestra chiamante, passando al parametro nRowWnd il valore -2
      Al posto dei valori -1 e -2 è possibile utilizzare le corrispettive costanti _MSG_CENTER_IN_DESKTOP e
      _MSG_CENTER_IN_PARENT presenti nel file MSG.CH.
      Il valore di DEFAULT di nRowWnd è -1 (finestra centrata rispetto al DESKTOP) e quello di nColWnd è 0
      (tuttavia  nel caso in cui il valore di nRowWnd è -1 o -2 il valore di nColWnd non viene preso in 
      considerazione.                
      
   sWndIcon
      Icona da visualizzare nell'intestazione della finestra. Il nome del file immagine deve essere completo
      di PATH ed estensione. DEFAULT -> NIL (nessuna immagine)
   
   aBtns
      Matrice contenente i parametri delle opzioni di scelta da selezionare. DEFAULT -> NIL (Finestra con 
      solo messaggio, senza bottoni).
      Formato di aButtons: 
       - aButtons[01] -> Numero bottone selezionato per DEFAULT.
       - aButtons[02] -> Array contenente la formattazione delle opzioni con il seguente formato:
         - Array[01] -> Nome del FONT da utilizzare
         - Array[02] -> Dimesione del FONT
         - Array[03] -> Colore del primo piano del FONT
         - Array[04] -> Colore di sfondo del FONT
         - Array[05] -> Valore logico per il grassetto
         - Array[06] -> Valore logico per lo stile italico
         - Array[07] -> Valore logico per la sottolineatura
         DEFAULT -> {"Arial",10,Black,White,.F.,.F.,.F.}   
       - aButtons[03] -> Valore da visualizzare (proprietà CAPTION).
       - aButtons[04] -> Posizione del carattere contenuto in aButtons[03] da utilizzare come scorciatoia da 
                         tastiera per selezionare il bottone. Il valore 0 indica che non deve essere
                         evidenziata nessuna lettera. DEFAULT -> vedi funzione __CtrlKeys
       - aButtons[05] -> Immagine da visualizzare (proprietà CAPTION). Il nome del file immagine deve 
                         essere completo di PATH ed estensione. DEFAULT -> NIL (nessuna immagine)
       - aButtons[06] ->  Valore da assegnare alla proprietà TOOLTIP del bottone. DEFAULT -> ""
      Esempio aBtns := {1,{"Arial",10,Black,White,.F.,.F.,.F.},{"Ok",1,"_Ok.png",""}}
   
   sAlertIcon
      Immagine da visualizzare all'interno della finestra. Il nome del file immagine deve essere completo di 
      PATH ed estensione. DEFAULT -> NIL (nessuna immagine) 
   
   aBtnBColor
      Colore di sfondo della finestra nella zona dei bottoni. In caso di presenza di bottoni la finestra 
      visivamente viene divisa in due parti: la prima contiene l'immagine ed il testo del messaggio (in 
      questo caso il colore corrisponde al colore di sfondo del messaggio), la seconda contiene i bottoni 
      delle le pzioni selezionabili (aBtnBColor). DEFAULT -> Colore di sfondo standard della finestra   

   aLblProperty
      Array contenente i valori di DEFAULT da utilizzare per la visualizzazione del messaggio. Questi valori
      corrispondono a:
       - Nome del FONT da utilizzare
       - Dimesione del FONT
       - Colore del primo piano del FONT
       - Colore di sfondo del FONT
       - Valore logico per il grassetto
       - Valore logico per lo stile italico
       - Valore logico per la sottolineatura
      DEFAULT ->  aLblProperty := {"Arial",10,COLOR_Black,COLOR_White,.F.,.F.,.F.}
   
   aLblOpt
      Matrice contenente:
       - come primo elemento un valore logico che indica se il testo del messaggio deve essere modificato a 
         livello di parola (.T.) o di carattere (.F.). E' possibile utilizzare le corrispettive costanti 
         _MSG_WORD_SELECTION e _MSG_CHAR_SELECTION presenti nel file MSG.CH.
       - come elementi successivi un array contenente i seguenti valori:
         - Posizione della parola o del carattere da modificare all'interno del messaggio.
           ATTENZIONE: gli eventuali ritorni a capo (HB_EOL()) non vengono considerati. Questo aspetto è
           importantissimo nel caso in cui la selezione viene fatta per carattere (_MSG_CHAR_SELECTION).
           Supponendo, ad esempio, che il messaggio sia "Test di" + HB_EOL() + "prova" la lettera 'p' della 
           parola 'prova' sarà il carattere numero 8 (come se il testo fosse "Test diprova"  
         - Nome del FONT da utilizzare per il carattere o la parola specificata con il valore precedente                      
         - Dimesione del FONT. DEFAULT -> Valore corrispondente a quello del parametro aLblProperty                           
         - Colore del primo piano del FONT. DEFAULT -> Valore corrispondente a quello del parametro                           
           aLblProperty                                                                                                       
         - Colore di sfondo del FONT. DEFAULT -> Valore corrispondente a quello del parametro aLblProperty                    
         - Valore logico per il grassetto. DEFAULT -> Valore corrispondente a quello del parametro aLblProperty               
         - Valore logico per lo stile italico. DEFAULT -> Valore corrispondente a quello del parametro                        
           aLblProperty                                                                                                       
         - Valore logico per la sottolineatura. DEFAULT -> Valore corrispondente a quello del parametro                       
           aLblProperty                                                                                                       
         - Testo da visualizzare al passaggio del mouse sul carattere o sulla parola indicata (TOOLTIP).                      
           DEFAULT -> NIL (nessun testo)                                                                                      
         - Valore logico per modificare o meno la forma del cursore in una mano al passaggio del mouse sul                    
           carattere o sulla parola indicata. DEFAULT -> .F.                                                                  
         - Blocco di codice da eseguire cliccando sul carattere o sulla parola indicata. DEFAULT -> NIL                       
      il valore di DEFAULT di aLblOpt è {_MSG_WORD_SELECTION). Questo significa che verrà visualizzato un
      messaggio con testo uniforme.

  Restituisce:
   nChoice
      Il numero della posizione dell'opzione selezionata.     

  Descrizione
   MyMsgColor() visualizza una finestra contenente un messaggio di testo ed alla quale è possibile aggiungere
   una serie di opzioni da selezionare (tipo ALERT()).
   La particolarità della funzione è che il contenuto del messaggio può essere modificato a livello di 
   carattere o di parola. Le modifiche possibili riguardano il tipo di FONT, la sua dimensione, il suo colore 
   di primo piano, il suo colore di sfondo, l'utilizzo o meno del grassetto, l'utilizzo o meno dello stile
   italico, l'utilizzo o meno della sottolineatura, la possibilità di visualizzare un testo al passaggio del
   mouse sull'elemento voluto, la possibilità di cambiare la forma dell'icona del mouse in quella di una mano 
   al passaggio del mouse e l'esecuzione di un blocco di codice al click sinistro del mouse.

  Esempio:
   
  #INCLUDE "HMG.CH"                                                                                                                                                                                                                                                                                  
                                                                                                                                                                                                                                                                                                     
  FUNCTION MAIN()                                                                                                                                                                                                                                                                                    
     LOCAL aLblOptions1:={.F.,;                                                                                                                                                                                                                                                                      
                         {01,NIL,25,NIL,NIL,.T.,.T.,.T.,"Tooltip",.T.,{|o| MsgInfo( 'Value: ' + _GetValue( _HMG_ThisControlName, _HMG_ThisFormName ), 'Class: ' + o:ClassName)}}, ;                                                                                                                  
                         {15,"arial",30,COLOR_Blue}, ;                                                                                                                                                                                                                                               
                         {16,"arial",28,COLOR_Red}, ;                                                                                                                                                                                                                                                
                         {17,"arial",26,COLOR_Green}, ;                                                                                                                                                                                                                                              
                         {18,"arial",24,COLOR_Brown}, ;                                                                                                                                                                                                                                              
                         {19,"arial",22,COLOR_Magenta4}, ;                                                                                                                                                                                                                                           
                         {36,"impact",25,,Color_Yellow}, ;                                                                                                                                                                                                                                           
                         {49,"Courier",25,COLOR_Blue,Color_Yellow }}                                                                                                                                                                                                                                 
     LOCAL aLblOptions2:= {.T.,;                                                                                                                                                                                                                                                                     
                         {04,"Courier",25,COLOR_Red,COLOR_Yellow,.T.,.T.,.T.,"Tooltip",.T.,{|o| MsgInfo( 'Value: ' + _GetValue( _HMG_ThisControlName, _HMG_ThisFormName ), 'Class: ' + o:ClassName)}}, ;                                                                                             
                         {06,"impact",25,COLOR_Red}, ;                                                                                                                                                                                                                                               
                         {08,"arial",20}}                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                     
                                                                                                                                                                                                                                                                                                     
     LOCAL sTxt := "Test di prova label personalizzata." + HB_OSNEWLINE() + "Seconda riga." + HB_OSNEWLINE() +"Terza riga."                                                                                                                                                                          
               //  "Test di prova label personalizzata.Seconda riga"Terza riga"                                                                                                                                                                                                                      
               //   123456789012345678901234567890123456789012345678901234567890                                                                                                                                                                                                                     
               //            1         2         3         4         5         6                                                                                                                                                                                                                     
                                                                                                                                                                                                                                                                                                     
     DEFINE WINDOW Win_1 ;                                                                                                                                                                                                                                                                           
     WIDTH 640 HEIGHT 440 ;                                                                                                                                                                                                                                                                          
     TITLE 'Test' ;                                                                                                                                                                                                                                                                                  
     MAIN                                                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                                                                     
      DEFINE BUTTON Button_1                                                                                                                                                                                                            
         ROW  70                                                                                                                                                                                                                        
         COL  70                                                                                                                                                                                                                        
         WIDTH 250                                                                                                                                                                                                                      
         CAPTION "Personalizzazione per carattere"                                                                                                                                                                                                                
         ACTION MyMsgCustom(sTxt,"Messaggio senza opzioni",-1,0,"Resource\_Info.ico",,,,,aLblOptions1)                                                                                                                            
      END BUTTON                                                                                                                                                                                                                        
      DEFINE BUTTON Button_2                                                                                                                                                                                                            
         ROW  110                                                                                                                                                                                                                       
         COL  70                                                                                                                                                                                                                        
         WIDTH 250                                                                                                                                                                                                                      
         CAPTION "Personalizzazione per parola"                                                                                                                                                                                                                
         ACTION MSGINFO(MyMsgCustom(sTxt,"Messaggio con opzioni",-1,0,"Resource\_Help.ico", ;
                      {1,{"Si",1,"Resource\_Ok.png"},{"No",1,"Resource\_Ko.png"}}, ;
                      "Resource\_Help.png",COLOR_Red,{nil,10,COLOR_BLUE,COLOR_YELLOW,.T.},aLblOptions2))        
      END BUTTON                                                                                                                                                                                                                        
                                                                                                                                                                                                                                                                                                     
     END WINDOW                                                                                                                                                                                                                                                                                      
                                                                                                                                                                                                                                                                                                     
     CENTER WINDOW Win_1                                                                                                                                                                                                                                                                             
     ACTIVATE WINDOW Win_1                                                                                                                                                                                                                                                                           
  RETURN NIL                                                                                                                                                                                                                                                                                         
*/

#include "hmg.ch"
#include "mymessage.ch"
#include "colors.ch"

#define SPACE_BETWEEN_BUTTONS 7     

#define _MSG_BTN_OPTION  01
#define _MSG_BTN_KEY     02
#define _MSG_BTN_ICON    03
#define _MSG_BTN_TOOLTIP 04

#define _MSG_FNAME   01
#define _MSG_FSIZE   02
#define _MSG_FCOLOR  03
#define _MSG_BCOLOR  04
#define _MSG_BOLD    05
#define _MSG_ITALIC  06
#define _MSG_UNDERL  07
        
#define _BTN_FNAME   01
#define _BTN_FSIZE   02
#define _BTN_FCOLOR  03
#define _BTN_BCOLOR  04
#define _BTN_BOLD    05
#define _BTN_ITALIC  06
#define _BTN_UNDERL  07
        
#define _MSG_WND_WIDTH       01
#define _MSG_WND_HEIGHT      02

REQUEST __MyMsgCustomExit
REQUEST __CtrlKeys
REQUEST __FormatCaption

FUNCTION MyMsgCustom(sMessage,sTitle,nRowWnd,nColWnd,sWndIcon,aBtns,sAlertIcon,aBtnColor,aLblProperty,aLblOpt)
   LOCAL lIcon   := .F. // Indica se almeno in un bottone verrà inserita un'immagine o no
   LOCAL nRowLbl := 0
   LOCAL nChoice := 0
   LOCAL aButtons
   LOCAL aBtnsProperty
   LOCAL aLblOptions
   LOCAL nLen     
   LOCAL aTxt
   LOCAL oLbl
   LOCAL nOptions                                                                                                                                                                                                                
   LOCAL nDefaultOption
   LOCAL sNameBtn
   LOCAL nHeight   
   LOCAL nWidthBtn   
   LOCAL nWidthTotBtn
   LOCAL nWidthWnd 
   LOCAL nHeightWnd
   LOCAL aWndDim                                                                       
   LOCAL i 

   /*
     Se non intendo utilizzare le opzioni faccio in modo che venga elaborata la 'modalità' parola che
     comporta meno elaborazione.
   */
   DEFAULT aLblOpt  TO {_MSG_WORD_SELECTION}
   DEFAULT sMessage TO ""                                                                                                                                                                                                        
   DEFAULT sTitle   TO ""                                                                                                                                                                                    
   DEFAULT nRowWnd  TO _MSG_CENTER_IN_DESKTOP          
   DEFAULT nColWnd  TO 0
   
   IF aLblProperty == NIL                                      
      /*
        Si è scelto di utilizzare il font Arial al posto di _HMG_DefaultFontName in quanto il font
        _HMG_DefaultFontName (Segoe UI) non viene correttamente dimensionato con dimensioni superiori a 26.
        Si è scelto di utilizzare la dimensione 10 al posto di _HMG_DefaultFontSize in quanto la dimensione
        _HMG_DefaultFontSize (9) lo ritengo troppo piccolo.
      */
      aLblProperty := {"Arial",10,COLOR_Black,COLOR_White,.F.,.F.,.F.}
   ELSE
      // Faccio in modo che ogni array di aLblOptions contenga 7 elementi
      FOR i := LEN(aLblProperty) TO 7
          AADD(aLblProperty,NIL)
      NEXT
   ENDIF
   /*                                    
     Copio il valore di aBtns in aButtons perché in caso contrario, dal momento in cui gli array 
     vengono passati per riferimento, quando i valori di aBtns verrano modificati dalle funzioni 
     ProcessText() o ProcessWords() verrebbero modificati anche i valori dell'array della funzione chiamante.
     In questo modo le modifiche apportate in aButtons non si riflettono su aBtns.
     E' necessario utilizzare la funzione aClone() in quanto il codice aButtons := aBtns non 
     risolverebbe il problema.
   */
   aButtons := ACLONE(aBtns)       

   IF aButtons != NIL 
      DEFAULT aBtnColor TO _COLOR_WINDOWS_MINIGUI

      nDefaultOption := aButtons[1]
      aBtnsProperty  := aButtons[2]                                  

      IF aBtnsProperty == NIL
         DEFAULT aBtnsProperty TO {"Arial",10,COLOR_Black,_COLOR_WINDOWS_MINIGUI,.F.,.F.,.F.}
      ELSE                            
         FOR i := LEN(aBtnsProperty) TO 6
             AADD(aBtnsProperty,NIL)
         NEXT
         IF aBtnsProperty[_BTN_FNAME] == NIL
            aBtnsProperty[_BTN_FNAME] := "Arial"
         ENDIF
         IF aBtnsProperty[_BTN_FSIZE] == NIL
            aBtnsProperty[_BTN_FSIZE] := 10
         ENDIF
         IF aBtnsProperty[_BTN_FCOLOR] == NIL
            aBtnsProperty[_BTN_FCOLOR] := COLOR_Black
         ENDIF
         IF aBtnsProperty[_BTN_BCOLOR] == NIL
            aBtnsProperty[_BTN_BCOLOR] := _COLOR_WINDOWS_MINIGUI
         ENDIF
         IF aBtnsProperty[_BTN_BOLD] == NIL
            aBtnsProperty[_BTN_BOLD] := .F.
         ENDIF
         IF aBtnsProperty[_BTN_ITALIC] == NIL
            aBtnsProperty[_BTN_ITALIC] := .F.
         ENDIF
         IF aBtnsProperty[_BTN_UNDERL] == NIL
            aBtnsProperty[_BTN_UNDERL] := .F.
         ENDIF
      ENDIF

      ADEL(aButtons,1,.T.)  // Elimino il primo elemento che indica il bottone di DEFAULT
      ADEL(aButtons,1,.T.)  // Elimino anche il secondoelemento  (diventato il primo) contenente la formattazione
      
      // Ogni elemento di aButtons deve avere 4 elementi
      FOR EACH aLblOptions IN aButtons // Presa in prestito                                                           
          FOR i := LEN(aLblOptions) TO 3                                                                                
              AADD(aLblOptions,NIL)                                                                                
          NEXT                                                                                                  
          /*                                                                                                       
            Evito di assegnare una stringa vuota al posto di NIL in quanto voglio fare in modo che se viene        
            passata una stringa vuota non devo assegnare nessuna lettera                                           
          */                                                                                                       
          IF aLblOptions[_MSG_BTN_ICON] == NIL                                                                                                                         
             aLblOptions[_MSG_BTN_ICON] := ""                                                                                                      
          ENDIF                                                                                                                                   
          IF !(aLblOptions[_MSG_BTN_ICON] == NIL)                                                                                                                         
             lIcon := .T.                                                                                                      
          ENDIF                                                                                                                                   
          IF aLblOptions[_MSG_BTN_TOOLTIP] == NIL                                                                                                                         
             aLblOptions[_MSG_BTN_TOOLTIP] := ""                                                                          
          ENDIF                                                                                                                                   
      NEXT                                                                                                         
      __CtrlKeys(aButtons)   
   ENDIF

   IF (nLen := LEN(aLblOpt) - 1) > 0
      /*                                                                                                                                            
        Copio il valore di aLblOpt in aLblOptions perché in caso contrario, dal momento in cui gli array                                            
        vengono passati per riferimento, quando i valori di aLblOpt verrano modificati dalle funzioni                                               
        ProcessText() o ProcessWords() verrebbero modificati anche i valori dell'array della funzione 
        chiamante.                                    
        In questo modo le modifiche apportate in aLblOptions non si riflettono su aLblOpt.                                                          
        E' necessario utilizzare la funzione aClone() in quanto il codice aLblOptions := aLblOpt non                                                
        risolverebbe il problema.                                                                                                                   
      */                                                                                                                                            
      aLblOptions := ACLONE(aLblOpt)                                                                                                                
      // In minigui sostituire ADEL() con HB_ADEL()                                                                                                 
      ADEL(aLblOptions,1,.T.)  // Tolgo il primo elemento che indica se elaborare il singolo carattere o la parola                                  
      // Faccio in modo che ogni array di aLblOptions contenga 11 elementi                                                                          
      FOR EACH aTxt IN aLblOptions                                                                                                                  
          FOR i := LEN(aTxt) TO 10 // 11                                                                                                            
              AADD(aTxt,NIL)                                                                                                                        
          NEXT                                                                                                                                      
      NEXT                                                                                                                                          
   ENDIF

   IF aLblOpt[1]
      /*
        Processo sMessage in modo che aTxt contenga le parole contenute in sMessage e modifica aLblOptions in
        modo che punti alle parole volute (se aLblOprions contiene valori)
      */
      aTxt := MyATokens(sMessage)
      IF nLen > 0
         ProcessWords(aTxt,aLblOptions) 
      ENDIF
   ELSE
      /*
        Processo sMessage in modo che aTxt contenga tutti i caratteri contenuti in sMessage e modifica aLblOptions 
        in modo che punti ai caratteri voluti
      */
      aTxt := ProcessText(sMessage,aLblOptions)
   ENDIF                                     
   
   oLbl := CustomLabel():New(aTxt,aLblProperty[_MSG_FNAME],aLblProperty[_MSG_FSIZE], ; 
                          aLblProperty[_MSG_FCOLOR],aLblProperty[_MSG_BCOLOR], ;
                          aLblProperty[_MSG_BOLD],aLblProperty[_MSG_ITALIC], ;
                          aLblProperty[_MSG_UNDERL])

   FOR i := 1 TO nLen         
       *     1    2     3     4      5      6     7       8       9     10    11
       * Add(cSub,FName,FSize,FColor,BColor,lBold,lItalic,lUnderl,sTool,lHand,bAct)
       oLbl:Add(aLblOptions[i,1],aLblOptions[i,2],aLblOptions[i,3],aLblOptions[i,4],aLblOptions[i,5],aLblOptions[i,6],aLblOptions[i,7],aLblOptions[i,8],aLblOptions[i,9],aLblOptions[i,10],aLblOptions[i,11])
   NEXT

   IF !IsWindowDefined (_WND_Alert)                                                                                                                      
      DEFINE WINDOW _WND_Alert      ;                                                                                                                                 
         ROW       nRowWnd ;
         COL       nColWnd ;                             
         TITLE     sTitle ;
	     ICON      sWndIcon ;
         BACKCOLOR IIF(aButtons != NIL,aBtnColor,aLblProperty[_MSG_BCOLOR]) ; 
         MODAL

         ON KEY Escape ACTION (_HMG_InteractiveClose := 1, _WND_Alert.Release)

         IF aButtons != NIL
            SET INTERACTIVECLOSE OFF

            DEFINE LABEL LBL_Tmp                                                   
               ROW 0                                                               
               COL 0                                                               
               FONTNAME   aBtnsProperty[_BTN_FNAME]                                
               FONTSIZE   aBtnsProperty[_BTN_FSIZE]                                
               FONTBOLD   aBtnsProperty[_BTN_BOLD]                                 
               FONTITALIC aBtnsProperty[_BTN_ITALIC]                               
               VALUE      aButtons[APosMaxLen(aButtons,_MSG_BTN_OPTION),_MSG_BTN_OPTION]           
               AUTOSIZE   .T.                                                      
               INVISIBLE  .T.                                                      
            END LABEL                                                              
            nHeight   := _WND_Alert.LBL_Tmp.Height + 7                                 
            nWidthBtn := _WND_Alert.LBL_Tmp.Width + IIF(lIcon,30,0) + 30           

            nOptions  := LEN(aButtons)                                                                                                                                                                                                  
            IF nWidthBtn < 90                                                      
               nWidthBtn := 90 // E' la dimensione standard di un BUTTON             
            ENDIF                                                                  
            nWidthTotBtn := ((nWidthBtn + SPACE_BETWEEN_BUTTONS) * nOptions)       

            DEFINE LABEL LBL_Frame        
               ROW       0                                                    
               COL       0               
               BACKCOLOR aLblProperty[_MSG_BCOLOR]
            END LABEL                     
         ENDIF

         IF sAlertIcon != NIL            
            DEFINE IMAGE IMG_Alert                                                                                                                                                                                                     
               ROW             10                                                                                                                                                                                                      
               COL             10      
               WIDTH           32
               HEIGHT          32
               PICTURE         sAlertIcon
               TRANSPARENT     .T.                                                                                                                 
            END IMAGE        
            nRowLbl := 15
            nColWnd := 60  // Presa a prestito
         ELSE   
            nColWnd := 0  // Presa a prestito
         ENDIF                                                                                                                                                                                                                  

         aWndDim := oLbl:Out(nRowLbl,nColWnd) // aWndDim contiene la larghezza e l'altezza della finestra dopo le label
         IF sAlertIcon != NIL 
            /*
              Se è prevista un'immagine e il messaggio è di un solo rigo devo allargare la finestra in modo 
              che la LABEL copra l'intera immagine. 
            */        
            aWndDim[_MSG_WND_HEIGHT] := MAX(aWndDim[_MSG_WND_HEIGHT],85)
         ENDIF

         IF aButtons != NIL
            aWndDim[_MSG_WND_HEIGHT]    += 12
            _WND_Alert.LBL_Frame.Height := aWndDim[_MSG_WND_HEIGHT] - 37                                                                                                                                                                           
            nWidthWnd                   := MAX(aWndDim[_MSG_WND_WIDTH],(nWidthTotBtn + 30))  
            nHeightWnd                  := aWndDim[_MSG_WND_HEIGHT] + nHeight + 22                                                                                                                                                                
            _WND_Alert.LBL_Frame.Width  := nWidthWnd - 15                                                                                                                                                                         
            nColWnd                     := nWidthWnd - nWidthTotBtn - 20 // Presa a prestito      
            FOR i := 1 TO nOptions
            
                sNameBtn := "BTN_" + STRZERO(i,2)                            
	            DEFINE BUTTONEX &sNameBtn                                                                                                                            
 	              ROW           aWndDim[_MSG_WND_HEIGHT] - 26                                                                                                                                         
                  COL           nColWnd                                          
                  WIDTH         nWidthBtn                                     
                  HEIGHT        nHeight   
                  FONTNAME      aBtnsProperty[_BTN_FNAME]          
                  FONTSIZE      aBtnsProperty[_BTN_FSIZE]          
                  FONTCOLOR     aBtnsProperty[_BTN_FCOLOR]
                  BACKCOLOR     aBtnsProperty[_BTN_BCOLOR]
                  FONTBOLD      aBtnsProperty[_BTN_BOLD]           
                  FONTITALIC    aBtnsProperty[_BTN_ITALIC]         
                  FONTUNDERLINE aBtnsProperty[_BTN_UNDERL]                                                                                                              
                  CAPTION       __FormatCaption(aButtons[i])                  
                  PICTURE       aButtons[i,_MSG_BTN_ICON]                 
                  IMAGEWIDTH    15                                            
                  IMAGEHEIGHT   15                                            
                  TOOLTIP       aButtons[i,_MSG_BTN_TOOLTIP]                                                                                                                                                                                             
                  ONCLICK       nChoice := __MyMsgCustomExit()
                  NOXPSTYLE     !(aBtnsProperty[_BTN_BCOLOR,1] == 240 .AND. aBtnsProperty[_BTN_BCOLOR,2] == 240 .AND. aBtnsProperty[_BTN_BCOLOR,3] == 240)
	            END BUTTONEX

                nColWnd += (nWidthBtn + SPACE_BETWEEN_BUTTONS)                                                                   
            NEXT
         ELSE
            nWidthWnd  := aWndDim[_MSG_WND_WIDTH]
            nHeightWnd := aWndDim[_MSG_WND_HEIGHT]                                                                                                                                                                
         ENDIF                                                       
      END WINDOW           
      _WND_Alert.Width  := nWidthWnd  
      _WND_Alert.Height := nHeightWnd                                                                                                                                                                
      IF nRowWnd == _MSG_CENTER_IN_DESKTOP                     
         CENTER WINDOW _WND_Alert
      ELSEIF nRowWnd == _MSG_CENTER_IN_PARENT                     
        CenterWindow("_WND_Alert")             
      ENDIF                                     
      IF aButtons != NIL
        _WND_Alert.&("BTN_" + STRZERO(nDefaultOption,2)).SetFocus
      ENDIF                                                                                                                                                                  
	    ACTIVATE WINDOW _WND_Alert                                                                                                                          
   ENDIF
RETURN nChoice
 ******************************************
STATIC FUNCTION ProcessText(sMessage,aLblOptions)
   LOCAL aTxt    := {}
   LOCAL aTmpTxt := {}
   LOCAL lEOL    := .F.       
   LOCAL nEOL    := 0                                    
   LOCAL xTmp

/*                                                                                                         
  Creo due array:
  - Nel primo ogni elemento contiene un carattere di sMessage (I caratteri 13/10 vengono inseriti nello stesso 
    elemento e non singolarmente come nel caso precedente).
  - Nel secondo ogni elemento contiene lo stesso valore del primo ma con in più la posizione all'interno di 
    sMessage (I caratteri 13/10 vengono esclusi).
*/
   FOR EACH xTmp IN sMessage // xTmp -> tipo carattere   
       IF xTmp == CHR(13)
          IF lEOL
             AADD(aTmpTxt,{HB_ENUMINDEX() + nEOL,xTmp})
             AADD(aTxt,xTmp)
          ELSE
             lEOL := .T.
          ENDIF
       ELSEIF xTmp == CHR(10)
          IF lEOL 
             AADD(aTxt,CHR(13) + CHR(10))
             lEOL := .F.
             nEOL--
          ELSE
             AADD(aTmpTxt,{HB_ENUMINDEX() + nEOL,xTmp})
             AADD(aTxt,xTmp)
          ENDIF
       ELSE
          IF lEOL 
             AADD(aTmpTxt,{HB_ENUMINDEX() + nEOL,xTmp})
             AADD(aTxt,CHR(13))
             lEOL := .F.
          ENDIF
          AADD(aTmpTxt,{HB_ENUMINDEX() + nEOL,xTmp})
          AADD(aTxt,xTmp)
       ENDIF
   NEXT 
   /*
     Modifico aLblOptions sostituendo la posizione dei caratteri all'interno di aTxt con l'effettiva
     posizione all'interno di aTxt.
     Il valore viene convertito in tipo carattere in quanto dovrà essere assegnato come valore principale
     di un Hash che accetta solo valori di tipo carattere
   */   
   FOR EACH xTmp IN aLblOptions // xTmp -> tipo array
       aLblOptions[HB_ENUMINDEX(),1] := HB_NTOS(aTmpTxt[xTmp[1],1])    
   NEXT 
RETURN aTxt
 ******************************************
#define _LETTERS "ABCDEFGHIJKLMNOPQRSTUWXYZabcdefghijklmnopqrstuvwxyzìèéòàù"

/*
  La funzione scompone il testo passato come parametro in valori da caricare in un array. 
  Il contenuto di ogni elemento dell'array restituito potrà essere:
   - Una stringa contenente solo caratteri albabetici (comprese le lettere accentate). La stringa potrà
     essere composta anche da un solo carattere.
   - Una stringa contenente tutti i caratteri non alfabetici
   - Una coppia di caratteri 13/10 (HB_EOL())
  * Il carattere apostrofo (') viene considerato un carattere alfabetico se si trova tra due di esi in modo da
  * considerare parole come, ad esempio, "l'albero" come se fosse un'unica parola. In caso contrario viene
  * considerato come carattere non alfabetico.
  * Errore conosciuto: una stringa contenente un'alternanza di caratteri alfabetici e apostrofi, ad esempio
  * "P'a'e's'e", viene considerata un'unica parola.
  
  Esempio:
  
   LOCAL sTesto := "*** ATTENZIONE ***" + HB_EOL() + "Questa è una 'stringa di prova' con l'apostrofo"
   LOCAL aArray := MyATokens(sTesto)
   
   ? ValToPrg(aArray) 
   WAIT
   
   Contenuto aArray:
   {"*** ",
   "ATTENZIONE",
   " ***",
   HB_EOL(),
   "Questa",
   " ",
   "è",
   " ",
   "una",
   " '",
   "stringa",
   " ",
   "di",
   " ",
   "prova",
   "' ",
   "con",
   " ",
   "l'apostrofo" // Sostituito con
                    "l",
                    "'",
                    "apostrofo" 
*/
FUNCTION MyATokens(sTxt)
   LOCAL aTxt       := {}
   LOCAL sTmpWord   := ""
   LOCAL sTmpChar   := ""
   LOCAL lEOL       := .F.       
   LOCAL cTxt  
                               
   FOR EACH cTxt IN sTxt   
       IF AT(cTxt,_LETTERS) > 0
          IF !(sTmpChar == "")
             AADD(aTxt,sTmpChar)
             sTmpChar := ""
          ENDIF      
          sTmpWord += cTxt
       ELSE
             IF !(sTmpWord == "")                       
                IF lEOL                                 
                   AADD(aTxt,CHR(13))                   
                   lEOL := .F.                          
                ENDIF                                   
                AADD(aTxt,sTmpWord)                     
                sTmpWord := ""                          
             ENDIF                                      
             IF cTxt == CHR(13)                         
                lEOL := .T.                             
             ELSEIF cTxt == CHR(10)                     
                IF lEOL                   
                   IF !(sTmpChar == "")                       
                      AADD(aTxt,sTmpChar)
                      sTmpChar := ""
                   ENDIF
                   AADD(aTxt,CHR(13) + CHR(10))         
                   lEOL := .F.                          
                ELSE                                    
                   sTmpChar += cTxt                     
                ENDIF                                   
             ELSE                                       
                IF lEOL                                 
                   AADD(aTxt,CHR(13))                   
                   lEOL := .F.                          
                ENDIF                                   
                sTmpChar += cTxt                        
             ENDIF                                      
       ENDIF   
   NEXT 
   IF !(sTmpWord == "")
      AADD(aTxt,sTmpWord)
   ENDIF               
   IF !(sTmpChar == "")
      AADD(aTxt,sTmpChar)
   ENDIF               
RETURN aTxt
 ******************************************
STATIC FUNCTION ProcessWords(aTxt,aLblOptions)
   LOCAL aWords := {}
   LOCAL xTmp                                        

   // Creo un array che contiene solo le parole e le relative posizioni all'interno di aTxt 
   FOR EACH xTmp IN aTxt // xTmp -> tipo carattere
       IF AT(LEFT(xTmp,1),_LETTERS) > 0
          AADD(aWords,{HB_ENUMINDEX(),xTmp})
       ENDIF                                                                                                                                                                                                                                                    
   NEXT 
   /*
      Modifico aLblOptions sostituendo la posizione della paroìa all'interno di aWords con l'effettiva
      posizione all'interno di aTxt.
      Il valore viene convertito in tipo carattere in quanto dovrà essere assegnato come valore principale
      di un Hash che accetta solo valori di tipo carattere
   */   
   FOR EACH xTmp IN aLblOptions  // xTmp -> tipo array
       aLblOptions[HB_ENUMINDEX(),1] := HB_NTOS(aWords[xTmp[1],1])    
   NEXT 
RETURN NIL
 *****************
/*
  Questa funzione verifica e completa la dimensione _MSG_BTN_KEY della matrice aButtons. 
  Nello specifico:
  Verifico che i caratteri puntati da _MSG_BTN_KEY siano univoci. 
  - Se _MSG_BTN_KEY è uguale a zero significa che non è richiesta la scorciatoia da tastiera
  - Se _MSG_BTN_KEY è uguale a NIL significa che il carattere per la scorciatoia da tastiera deve essere 
    assegnato dal programma
  - Se _MSG_BTN_KEY punta ad un carattere SPAZIO o il suo valore è superioe al numero di caratteri
    _MSG_BTN_OPTION faccio in modo che il carattere per la scorciatoia da tastiera venga assegnato dal 
    programma
  - Se _MSG_BTN_KEY punta ad un carattere che è già stato puntato da un _MSG_BTN_KEY precedente faccio in 
    modo che un nuovo carattere per la scorciatoia da tastiera venga assegnato dal programma
  Se non vi sono caratteri disponibili non ne verrà assegnato nessuno non consentendo la scorciatoia da 
  tastiera   
*/
STATIC FUNCTION __CtrlKeys(aButtons)
   LOCAL aArray := {}
   LOCAL sKey   := ""
   LOCAL cKey
   LOCAL nKey
   LOCAL cVar
   LOCAL nPos
   
   FOR EACH aArray IN aButtons
       IF (nKey := aArray[_MSG_BTN_KEY]) != NIL
          IF nKey != 0 // E' richiesta la scorciatoia da tastiera                                                                
             IF (cKey := UPPER(SUBSTR(aArray[_MSG_BTN_OPTION],nKey,1))) == "" // Se nKey è maggiore del numero di caratteri
                aButtons[HB_EnumIndex(),_MSG_BTN_KEY] := NIL // Faccio in modo che venga trovato un altro carattere valido    
             ELSE     
                IF !(cKey == " ") // Se cKey contiene un carattere potenzialmente valido                                                         
                   IF cKey $ sKey // Carattere non specificato o già presente                                                                    
                      aButtons[HB_EnumIndex(),_MSG_BTN_KEY] := NIL // Faccio in modo che venga trovato un altro carattere valido                   
                   ELSE                                                                                                                          
                      sKey += cKey                                                                                                               
                   ENDIF                                                                                                                         
                ELSE                                                                                                                             
                   aButtons[HB_EnumIndex(),_MSG_BTN_KEY] := NIL // Faccio in modo che venga trovato un altro carattere valido                      
                ENDIF                                                                                                                            
             ENDIF                                                                                              
          ENDIF                                                                                                 
       ENDIF                                                                                                 
   NEXT             
   // Assegno un carattere a tutti i valori NIL
   FOR EACH aArray IN aButtons
       IF aArray[_MSG_BTN_KEY] == NIL       
          cKey := UPPER(LEFT(aArray[_MSG_BTN_OPTION],1)) // Controllo il primo carattere di _MSG_BTN_OPTION è valido                                          
          IF cKey $ sKey // Carattere già presente                                                                                                        
             /*                                                                                                                                           
               Verifico se _MSG_BTN_OPTION è composta da più di una parola e, in caso positivo, controllo il 
               primo carattere di ogni parola fino a quando non trovo un carattere valido                                                                       
             */                                                                                                                                           
             DO WHILE (nPos := AT(" ",aArray[_MSG_BTN_OPTION])) > 0                                                                                         
                cKey := UPPER(SUBSTR(aArray[_MSG_BTN_OPTION],++nPos,1))                                                                                       
                IF !(cKey $ sKey) // Carattere non presente                                                                                               
                   aButtons[HB_EnumIndex(),_MSG_BTN_KEY] := nPos                                                                                            
                   sKey += cKey                                                                                            
                   EXIT                                                                                                                                   
                ENDIF                                                                                                                                     
                aArray[_MSG_BTN_OPTION] := SUBSTR(aArray[_MSG_BTN_OPTION],nPos)                                                                               
             ENDDO                                                                                                                                        
             IF aButtons[HB_EnumIndex(),_MSG_BTN_KEY] == NIL // Nessun carattere trovato
                nPos := HB_EnumIndex()                                                                                  
                FOR EACH cVar IN aArray[_MSG_BTN_OPTION] // Cerco il primo carattere valido dell'intero _MSG_BTN_OPTION                                       
                    IF !(cVar == " ")                                                                                                                      
                       cKey := UPPER(cVar)                                                                                                                
                       IF !(cKey $ sKey)                                                                                                                  
                          aButtons[nPos,_MSG_BTN_KEY] := HB_EnumIndex()                                                                                     
                          sKey += cKey                                                                                     
                          EXIT                                                                                                                            
                       ENDIF                                                                                                                              
                    ENDIF                                                                                                                                 
                NEXT                                                                                                                                      
                IF aButtons[nPos,_MSG_BTN_KEY] == NIL // Nessun carattere trovato                                                                                  
                   aButtons[nPos,_MSG_BTN_KEY] := 0   // Non assegno nessun carattere                                                              
                ENDIF                                                                                                                                     
             ENDIF                                                                                                                                        
          ELSE                                                                                                                                            
             aButtons[HB_EnumIndex(),_MSG_BTN_KEY] := 1                                                                                                  
             sKey += cKey                                                                                                  
          ENDIF                                                                                                                                           
       ENDIF                                                                                                                                           
   NEXT
RETURN NIL
 *****************
STATIC FUNCTION __MyMsgCustomExit()
   LOCAL nChoice  := VAL(SUBSTR(_HMG_ThisControlName,RAT("_",_HMG_ThisControlName) + 1))

   SET INTERACTIVECLOSE ON
   
   _WND_Alert.Release
RETURN nChoice
 *****************
 /*
  Questa funzione formatta la stringa 'opzione' antecedendo il simbolo '&' al carattere voluto per poterlo
  usare come scorciatoia da tastiera per la selezione (ALT + ...).
 */ 
STATIC FUNCTION __FormatCaption(aArray)
   LOCAL sRetVal := aArray[_MSG_BTN_OPTION]
   IF aArray[_MSG_BTN_KEY] != 0
      sRetVal := STUFF(sRetVal,aArray[_MSG_BTN_KEY],0,"&")
   ENDIF
RETURN sRetVal
 *****************
/*
  Crea il parametro aLblOpt della funzione MyMsgCustom (utile quando non si conosce la dimensione della
  parola da customizzare).
  
  aCustom -> Matrice così composta 
   - Posizione del primo carattere
   - Dimensione della parola
   - Array così composto:
     - Nome font
     - Dimensione font
     - Colore primo piano
     - Colore sfondo
     - Valore logico grassetto
     - Valore logico italic
     - Valore logico sottolineatura
  Es.: {{10,20,{"Arial",10,COLOR_Blue,COLOR_Yellow,.T.,.T.,.T.}}}        
*/
FUNCTION MakeArrayCustom(aCustom)
   LOCAL aRetVal := {}
   LOCAL aTmp     
   LOCAL i
   
   AADD(aRetVal,_MSG_CHAR_SELECTION)
   FOR EACH aTmp IN aCustom    
       FOR i := 1 TO aTmp[2]
           AADD(aRetVal,{aTmp[1]++,aTmp[3,1],aTmp[3,2],aTmp[3,3],aTmp[3,4],aTmp[3,5],aTmp[3,6],aTmp[3,7]})
       NEXT
   NEXT       
RETURN aRetVal
 **************************************
FUNCTION MyMsgInfo(sMessage,sTitle,nRow,nCol,aCustom)
   LOCAL sPath := HB_FNameDir(HB_Progname())

   DEFAULT sTitle TO "Info"                                                                                                                                                                                    
RETURN MyMsgCustom(sMessage,sTitle,nRow,nCol,sPath + "Resource\_Info.ico",{1,NIL,{"Ok",1,"Resource\_Ok.png",""}},sPath + "Resource\_Info.png",NIL,NIL,aCustom)
 *****************
// Restituisco .T. o .F. per compatibilità con le funzioni standard
FUNCTION MyMsgNoYes(sMessage,sTitle,nRow,nCol,aCustom)
   LOCAL sPath := HB_FNameDir(HB_Progname())

   DEFAULT sTitle TO "Seleziona un'opzione"                                                                                                                                                                                    
RETURN IIF(MyMsgCustom(sMessage,sTitle,nRow,nCol,sPath + "Resource\_Help.ico",{2,NIL,{"Yes",1,"Resource\_Ok.png",""},{"No",1,"Resource\_Ko.png",""}},sPath + "Resource\_Help.png",NIL,NIL,aCustom) == 2,.F.,.T.)
 *****************
FUNCTION MyMsgStop(sMessage,sTitle,nRow,nCol,aCustom)
   LOCAL sPath := HB_FNameDir(HB_Progname())

   DEFAULT sTitle TO "Attenzione"                                                                                                                                                                                    
RETURN MyMsgCustom(sMessage,sTitle,nRow,nCol,sPath + "Resource\_Stop.ico",{1,NIL,{"Ok",1,"Resource\_Ok.png",""}},sPath + "Resource\_Stop.png",NIL,NIL,aCustom)
 *****************
// Restituisco .T. o .F. per compatibilità con le funzioni standard
FUNCTION MyMsgYesNo(sMessage,sTitle,nRow,nCol,aCustom)
   LOCAL sPath := HB_FNameDir(HB_Progname())

   DEFAULT sTitle TO "Seleziona un'opzione"                                                                                                                                                                                    
RETURN IIF(MyMsgCustom(sMessage,sTitle,nRow,nCol,sPath + "Resource\_Help.ico",{1,NIL,{"Yes",1,"Resource\_Ok.png",""},{"No",1,"Resource\_Ko.png",""}},sPath + "Resource\_Help.png",NIL,NIL,aCustom) == 1,.T.,.F.)
 *****************
FUNCTION MyMsgWarning(sMessage,sTitle,nRow,nCol,aCustom)
   LOCAL sPath := HB_FNameDir(HB_Progname())

   DEFAULT sTitle TO "Attenzione"                                                                                                                                                                                    
RETURN MyMsgCustom(sMessage,sTitle,nRow,nCol,sPath + "Resource\_Warning.ico",{1,NIL,{"Ok",1,"Resource\_Ok.png",""}},sPath + "Resource\_Warning.png",NIL,NIL,aCustom)


 **************************************************************************************************
 **************************************************************************************************
 **************************************************************************************************
 **************************************************************************************************
 *
 *                                     UTILITY 
 *
 **************************************************************************************************
 **************************************************************************************************
 **************************************************************************************************
 **************************************************************************************************
FUNCTION CenterWindow(sWindowModal)
   LOCAL sWindowMain := GetProperty(sWindowModal,'Parent')

   SetProperty(sWindowModal,"Row",GetProperty(sWindowMain,"Row") + ((GetProperty(sWindowMain,"Height") - GetProperty(sWindowModal,"Height")) / 2 ))
   SetProperty(sWindowModal,"Col",GetProperty(sWindowMain,"Col") + ((GetProperty(sWindowMain,"Width")  - GetProperty(sWindowModal,"Width")) / 2 ))
RETURN NIL

 **

FUNCTION APosMaxLen(aArray,nDim)
   LOCAL nMaxLen := -1
   LOCAL nLen    := LEN(aArray)
   LOCAL nTmp    := -1
   LOCAL nElement
   LOCAL i
                                                                    
   FOR i := 1 TO nLen                                               
       nMaxLen := MAX(nMaxLen,IIF(nDim == NIL,LEN(aArray[i]),LEN(aArray[i,nDim])))                       
       IF nTmp != nMaxLen                                           
          nTmp     := nMaxLen                                       
          nElement := i                                             
       ENDIF                                                        
   NEXT                                                             
RETURN nElement

 **

FUNCTION SetRowColumnW(sWindow,aControl)
   LOCAL nRow := RowBottom(sWindow) - GetProperty(sWindow,aControl[1],"Height") - 47
   LOCAL nLen := LEN(aControl)
   LOCAL i

   SetProperty(sWindow,aControl[1],"Row",nRow)
   SetProperty(sWindow,aControl[1],"Col",ColRight(sWindow) - GetProperty(sWindow,aControl[1],"Width") - 25)
   FOR i := 2 TO nLen
       SetProperty(sWindow,aControl[i],"Row",nRow)
       SetProperty(sWindow,aControl[i],"Col",GetProperty(sWindow,aControl[i-1],"Col") - GetProperty(sWindow,aControl[i],"Width") - 10)
   NEXT
RETURN NIL

 **
 
FUNCTION ColRight(sWindow,sControl) 
RETURN IIF(sControl == NIL,GetProperty(sWindow,"Col") + GetProperty(sWindow,"Width"),GetProperty(sWindow,sControl,"Col") + GetProperty(sWindow,sControl,"Width"))  

 **
 
FUNCTION RowBottom(sWindow,sControl)
RETURN IIF(sControl == NIL,GetProperty(sWindow,"Row") + GetProperty(sWindow,"Height"),GetProperty(sWindow,sControl,"Row") + GetProperty(sWindow,sControl,"Height"))  
