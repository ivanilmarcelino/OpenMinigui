#include "hbclass.ch"
#include "hmg.ch"

#define _LBL_FNAME    01
#define _LBL_FSIZE    02
#define _LBL_FCOLOR   03
#define _LBL_BCOLOR   04
#define _LBL_BOLD     05
#define _LBL_ITALIC   06
#define _LBL_UNDERL   07
#define _LBL_TOOLTIP  08
#define _LBL_HAND     09
#define _LBL_BLOCK    10

CLASS CustomLabel
   VAR cForm
   VAR cFont
   VAR nSize
   VAR hSub
   VAR aTxt
   VAR cTxt     
   VAR BColor
   VAR FColor
   VAR nLbl     // INIT 1 
   VAR lBold    // INIT .F.
   VAR lItalic  // INIT .F.
   VAR lUnderl  // INIT .F.
   VAR sLbl     INIT '_Out_'

   METHOD New(aTxt,cFont,nSize,FColor,BColor,lBold,lItalic,lUnderl)
   METHOD Out(nRowLbl,nColLbl)
   METHOD Add(cSub,FName,FSize,FColor,BColor,lBold,lItalic,lUnderl,sTool,lHand,bAct) INLINE ;
         (hb_HSet(::hSub,cSub,{FName,FSize,FColor,BColor,lBold,lItalic,lUnderl,sTool,lHand,bAct}))
   METHOD Block()

ENDCLASS


METHOD New(aTxt,cFont,nSize,FColor,BColor,lBold,lItalic,lUnderl) CLASS CustomLabel
   STATIC nLbl := 0
                 
   /*
     Nel caso in cui la funzione ColorLbl() viene chiamata più di una volta il numero che viene assegnato
     alla fine del nome della LABEL non riparte da uno ma continua dall'ultimo numero assegnato al nome
     dell'ultima LABEL creata con la precedente chiamata alla funzione.
     Per fare in modo che il numero assegnato al nome della prima LABEL di ogni chiamata alla funzione
     parta da 1, necessario per il corretto funzionamento del metodo Block(), riassegno il valore di
     ::nLbl all'interno di questo modulo invece di inizializzarlo all'interno del metodo New(). 
     Per evitare che le prime LABEL della prima e delle successive chiamate alla funzione abbiano tutte
     il nome _out_1 faccio in modo che al valore _out_ venga aggiunto un numero corrispondente al
     numero di chiamata alla funzione. In questo modo la prima LABEL della prima chiamata alla funzione
     sarà _out_1_1, quello della seconda chiamata sarà _out_2_1 e così via. 
   */
   ::sLbl += HB_NTOS(++nLbl) + "_" 
   ::nLbl := 1
              
   ::cForm := _HMG_ThisFormName
   ::cFont := "Arial" // _HMG_DefaultFontName
   ::nSize := 10 // _HMG_DefaultFontSize
   ::hSub  := hb_Hash()
   ::aTxt  := aTxt

   IF cFont != NIL
      ::cFont  := cFont
   ENDIF
   IF nSize != NIL
      ::nSize  := nSize
   ENDIF
   IF FColor != NIL
      ::FColor := FColor
   ENDIF
   IF BColor != NIL
      ::BColor := BColor
   ENDIF
   IF lBold != NIL
      ::lBold := lBold
   ELSE
      ::lBold := .F.
   ENDIF
   IF lItalic != NIL
      ::lItalic := lItalic
   ELSE
      ::lItalic := .F.
   ENDIF
   IF lUnderl != NIL
      ::lUnderl := lUnderl
   ELSE
      ::lUnderl := .F.
   ENDIF
RETURN Self

METHOD Out(nRowLbl,nColLbl) CLASS CustomLabel
//   LOCAL cWnd        := ::cForm
   LOCAL oBlock      := Self
   LOCAL nHeight     := 0
   LOCAL lEOL        := .F. // Indica che non è stato ancora incontrato la coppia di caratteri 13/10
   LOCAL nEOL        := 0
   LOCAL nWhidthW    := 0
   LOCAL nCol         
   LOCAL nHeightW    
   LOCAL lMouseHover
   LOCAL sFontName   
   LOCAL nFontSize   
   LOCAL aFontColor
   LOCAL lFontBold
   LOCAL lFontItalic
   LOCAL lFontUnderLine
   LOCAL sTooltip
   LOCAL aLblOptions
   LOCAL sLblName
   LOCAL sLblValue
   LOCAL aBackColor 

   DEFAULT nRowLbl TO 0 
   DEFAULT nColLbl TO 0 
   
   nCol     := nColLbl
   nHeightW := nRowLbl
   
   // Probabilmente non necessario
//   DO WHILE _IsControlDefined(::sLbl + HB_NTOS(::nLbl),cWnd)
//      ::nLbl++
//   ENDDO

   FOR EACH sLblValue IN ::aTxt          
      IF sLblValue == HB_EOL()
         /*
           Se lEOL è .T. significa che anche il valore precedente di sLblValue era un ritorno a capo e,
           quindi, devo inserire una riga vuota.
           Per simulare il rigo vuoto sposto ulteriormente il rigo in cui inserire la prossima LABEL.
         */
         IF lEOL                                
            nRowLbl  += 09   
            nHeightW += 10 
         ENDIF
         nRowLbl  += nHeight    
         nWhidthW := MAX(nWhidthW,nCol)                                                                                                 
         nHeightW += nHeight + 1 
         nCol     := nColLbl
         nHeight  := 0
         lEOL     := .T.
         nEOL++
      ELSE   
         lEOL        := .F.
         sLblName    := ::sLbl + HB_NTOS(::nLbl)                                                                                                 
         aLblOptions := HB_HGETDEF(::hSub,HB_NTOS(HB_ENUMINDEX()),NIL)
         IF !EMPTY(aLblOptions)                                                                                                                 
            *             1     2     3      4      5     6       7       8     9     10
            * aLblOptions(FName,FSize,FColor,BColor,lBold,lItalic,lUnderl,sTool,lHand,bAct)
            sFontName      := IIF(aLblOptions[_LBL_FNAME] == NIL,::cFont,aLblOptions[_LBL_FNAME])
            nFontSize      := IIF(aLblOptions[_LBL_FSIZE] == NIL,::nSize,aLblOptions[_LBL_FSIZE])
            aFontColor     := IIF(aLblOptions[_LBL_FCOLOR] == NIL,::FColor,aLblOptions[_LBL_FCOLOR])                                                                                                         
            aBackColor     := IIF(aLblOptions[_LBL_BCOLOR] == NIL,::BColor,aLblOptions[_LBL_BCOLOR])               
//            lFontBold      := !EMPTY(aLblOptions[_LBL_BOLD])
//            lFontItalic    := !EMPTY(aLblOptions[_LBL_ITALIC])                                                                                                       
//            lFontUnderLine := !EMPTY(aLblOptions[_LBL_UNDERL])                                                                                                       
            lFontBold      := IIF(aLblOptions[_LBL_BOLD] == NIL,::lBold,aLblOptions[_LBL_BOLD])
            lFontItalic    := IIF(aLblOptions[_LBL_ITALIC] == NIL,::lItalic,aLblOptions[_LBL_ITALIC])
            lFontUnderLine := IIF(aLblOptions[_LBL_UNDERL] == NIL,::lUnderl,aLblOptions[_LBL_UNDERL])
            sTooltip       := aLblOptions[_LBL_TOOLTIP]                                                                                                         
            lMouseHover    := !EMPTY(aLblOptions[_LBL_HAND])                                                                                                       
         ELSE                                                                                                                            
            sFontName      := ::cFont
            nFontSize      := ::nSize
            aFontColor     := ::FColor                                                                                                               
            aBackColor     := ::BColor                                                                                                               
            lFontBold      := ::lBold                                                                                                                
            lFontItalic    := ::lItalic                                                                                                              
            lFontUnderLine := ::lUnderl                                                                                                              
            sTooltip       := NIL                                                                                                                    
            lMouseHover    := .F.                                                                                                       
         ENDIF                                                                                                                           
                                                                                                                                         
         DEFINE LABEL    &sLblName                                                                                                              
           ROW           nRowLbl                                                                                                               
           COL           nCol                                                                                                               
           VALUE         sLblValue                                                                                                               
           FONTNAME      sFontName                                                                                                              
           FONTSIZE      nFontSize                                                                                                              
           FONTBOLD      lFontBold                                                                                                              
           FONTITALIC    lFontItalic                                                                                                              
           FONTUNDERLINE lFontUnderLine                                                                                                              
           FONTCOLOR     aFontColor                                                                                                              
           BACKCOLOR     aBackColor                                                                                                              
           TOOLTIP       sTooltip                                                                                                              
           VCENTERALIGN  .T.                                                                                                             
           AUTOSIZE      .T.                                                                                                             
           ACTION        oBlock:Block()                                                                                                       
           IF lMouseHover                                                                                                                         
              ON MOUSEHOVER CURSORHAND()                                                                                                 
           ENDIF                                                                                                                         
         END LABEL                                                                                                                       
                                                                                                                                         
         nCol += This.&(sLblName).Width                                                                                                                          
         nHeight := MAX(nHeight,This.&(sLblName).Height)                                                                                                 
      ENDIF
      ::nLbl++
   NEXT
   nWhidthW := MAX(nWhidthW,nCol) + 25 // Larghezza di una LABEL con valori di DEFAULT                                                                                               
   nHeightW := nHeightW + nHeight + 37 - nEOL 
RETURN {nWhidthW,nHeightW}

METHOD Block() CLASS CustomLabel
   LOCAL sLblValue := _HMG_ThisControlName
   LOCAL aArray    := HB_HGETDEF(::hSub,SUBSTR(sLblValue,RAT("_",sLblValue) + 1),NIL)
                                                           
   IF HB_ISARRAY(aArray) .AND. HB_ISBLOCK(aArray[_LBL_BLOCK])
      EVAL(aArray[_LBL_BLOCK],Self)
   ENDIF
RETURN NIL
