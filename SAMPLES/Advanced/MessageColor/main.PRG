#include "hmg.ch"
#include "mymessage.ch"
#include "colors.ch"

SET PROCEDURE TO CustomLabel
SET PROCEDURE TO MyMessage

FUNCTION MAIN()
   LOCAL aLblOptions1:={.F.,;
                       {01,NIL,25,NIL,NIL,.T.,.T.,.T.,"Tooltip",.T.,{|o| MsgInfo( 'Value: ' + _GetValue( _HMG_ThisControlName, _HMG_ThisFormName ), 'Class: ' + o:ClassName)}}, ;
                       {25,"arial",30,COLOR_Blue}, ;                                                                                                                                     
                       {26,"arial",28,COLOR_Red}, ;                                                                                                                                     
                       {27,"arial",26,COLOR_Green}, ;                                                                                                                                     
                       {28,"arial",24,COLOR_Brown}, ;                                                                                                                                     
                       {29,"arial",22,COLOR_Magenta}}
   LOCAL aLblOptions2:= {.T.,;
                       {04,"Courier",25,COLOR_Red,COLOR_Yellow,.T.}, ;
                       {05,"Courier",25,COLOR_Red,COLOR_Yellow,.T.,.T.,.T.,"Tooltip",.T.,{|o| MsgInfo( 'Value: ' + _GetValue( _HMG_ThisControlName, _HMG_ThisFormName ), 'Class: ' + o:ClassName)}}}

   LOCAL sTxt := "Testing of a customised label.", cFileName 
   
   DEFINE WINDOW Win_1 ;
      WIDTH 365 HEIGHT 360 ;
      TITLE 'Test' ;
      ON INIT (SETPROPERTY("Win_1","Row",GETPROPERTY("Win_1","Row") + 100),SETPROPERTY("Win_1","Col",GETPROPERTY("Win_1","Col") + 100)) ;
      MAIN
                                              
      DEFINE BUTTON Button_1                                                                                                                                                                                                            
         ROW  20                                                                                                                                                                                                                        
         COL  20                                                                                                                                                                                                                        
         WIDTH 300                                                                                                                                                                                                                      
         CAPTION "Standard message CENTER IN DESKTOP"                                                                                                                                                                                                                
         ACTION MyMsgCustom(HB_EOL() + HB_EOL() + sTxt + HB_EOL() + HB_EOL(),"Standard message",_MSG_CENTER_IN_DESKTOP)                                                                                                                            
      END BUTTON                                                                                                                                                                                                                        
      DEFINE BUTTON Button_2                                                                                                                                                                                                            
         ROW  60                                                                                                                                                                                                                        
         COL  20                                                                                                                                                                                                                        
         WIDTH 300                                                                                                                                                                                                                      
         CAPTION "Customised message by character NO CENTER"                                                                                                                                                                                                                
         ACTION MyMsgCustom(sTxt,"Customised message by character",100,100,"Resource\_Info.ico",,,,,aLblOptions1)                                                                                                                            
      END BUTTON                                                                                                                                                                                                                        
      DEFINE BUTTON Button_3                                                                                                                                                                                                            
         ROW  100                                                                                                                                                                                                                       
         COL  20                                                                                                                                                                                                                        
         WIDTH 300                                                                                                                                                                                                                      
         CAPTION "Customised message by Word CENTER IN PARENT"                                                                                                                                                                                                                
         ACTION MyMsgCustom(sTxt,"Customised message by word",_MSG_CENTER_IN_PARENT,0,"Resource\_Info.ico",,,,,aLblOptions2)                                                                                                                            
      END BUTTON                                                                                                                                                                                                                        
      DEFINE BUTTON Button_4                                                                                                                                                                                                            
         ROW  140                                                                                                                                                                                                                       
         COL  20                                                                                                                                                                                                                        
         WIDTH 300                                                                                                                                                                                                                      
         CAPTION "Standard message 'MsgYesNo()'"                                                                                                                                                                                                                
         ACTION MSGINFO(MyMsgYesNo("Deletion of the file test.txt." + HB_EOL() + "Do you confirm?"))        
      END BUTTON                        
      DEFINE BUTTON Button_5                                                                                                                                                                                                            
         ROW  180                                                                                                                                                                                                                       
         COL  20                                                                                                                                                                                                                        
         WIDTH 300                                                                                                                                                                                                                      
         CAPTION "Customised message 'MsgYesNo()' #1"                                                                                                                                                                                                                
         ACTION MSGINFO(MyMsgYesNo("Deletion of the file test.txt." + HB_EOL() + "Do you confirm?",NIL,NIL,NIL, ;
                      MakeArrayCustom({{22,8,{NIL,NIL,COLOR_Blue,NIL,.T.,NIL,NIL}}})))                                                                                                                                         
      END BUTTON                                                                                                                                                                                                                        
      DEFINE BUTTON Button_6                                                                                                                                                                                                            
         ROW  220                                                                                                                                                                                                                       
         COL  20                                                                                                                                                                                                                        
         WIDTH 300                                                                                                                                                                                                                      
         CAPTION "Customised message 'MsgYesNo()' #2"                                                                                                                                                                                                                
         ACTION MSGINFO(MyMsgYesNo("               WARNING               " + HB_EOL() + HB_EOL() + ;
                      "Missing example archive!" + HB_EOL() + HB_EOL() + ;
                      "Would you like to create it?" + HB_EOL() + ;
                      "(This warning will no longer be repeated. It will always be possible to" + HB_EOL() + ;                                                                                    
                      "to create it later via the menu item 'Tools - Create Sample Archive').", ;
                      "Example archive",_MSG_CENTER_IN_PARENT,NIL, ;
                       MakeArrayCustom({{1,37,{NIL,NIL,COLOR_Red,COLOR_Yellow,.T.,NIL,NIL}}, ;
                                        {90,109,{NIL,8,COLOR_Grey51,NIL,NIL,NIL,NIL}}, ;
                                        {199,5,{NIL,8,COLOR_RoyalBlue1,NIL,NIL,NIL,NIL}}, ;
                                        {204,3,{NIL,8,COLOR_Grey51,NIL,NIL,NIL,NIL}}, ;
                                        {207,21,{NIL,8,COLOR_RoyalBlue1,NIL,NIL,NIL,NIL}}, ;
                                        {228,3,{NIL,8,COLOR_Grey51,NIL,NIL,NIL,NIL}}})))
      END BUTTON                                                                                                                                                                                                                        
      DEFINE BUTTON Button_7                                                                                                                                                                                                            
         ROW  260                                                                                                                                                                                                                       
         COL  20                                                                                                                                                                                                                        
         WIDTH 300                                                                                                                                                                                                                      
         CAPTION "Message with variable-length word to customise"                                                                                                                                                                                                                
         ACTION (cFileName := TEST(), ;
               MSGINFO((MyMsgCustom("Deletion of the file" + HB_EOL() + cFileName + HB_EOL() + "Do you confirm?", ;
               "Select",_MSG_CENTER_IN_DESKTOP,0,"Resource\_Info.ico",{1,{"Courier",11,COLOR_SlateBlue4,COLOR_Yellow,.T.,.T.,.F.}, ;
               {"Ch 1",4},{"Ch 2",4},{"Ch 3",4},{"Ch 4",4}},"Resource\_Help.png",;
               COLOR_Red,{NIL,NIL,COLOR_DarkGreen,COLOR_Beige},MakeArrayCustom({{21,LEN(cFileName), ;
               {NIL,NIL,COLOR_Blue,NIL,.T.,NIL,NIL}}})))))
//   MyMsgCustom([sMessage], [sTitle], [nRowWnd], [nColWnd], [sWndIcon], [aBtns], [sAlertIcon], [aBtnBColor], 
//              [aLblProperty], [aLblOpt]) -> nChoice
      END BUTTON                                                                                                                                                                                                                        
   END WINDOW

   CENTER WINDOW Win_1
   ACTIVATE WINDOW Win_1
return nil


FUNCTION TEST()
   LOCAL cFileName := "Test file"

   DEFINE WINDOW Win_2 ;
      WIDTH     308 ;                                                                                                                                                                                                         
      HEIGHT    155 ;                                                             
      TITLE     "File Name" ;
      FONT      _HMG_DefaultFontName SIZE 10 ;
      MODAL                                                                                                                                                                                                                    
                                                                                                                                                                                                                            
      SET INTERACTIVECLOSE OFF

      DEFINE LABEL LBL_1                                                                                                                                                                                             
         ROW       5                                                                                                                                                                                                        
         COL       12                                          
         VALUE     "Insert the name of the file to be deleted"                                                                                                                                                                                  
         AUTOSIZE  .T.                                                                                                                                                                                                      
      END LABEL                                                                                                                                                                                                             
                                                                                                                                                                                                                            
      DEFINE TEXTBOX TXB_1                                                                                                                                                                                           
         ROW         30                                                                                                                                                                                                       
         COL         12                                                                                                                                                                                                       
         WIDTH       271                                                                                                                                                                                                      
         FONTSIZE    08                                                                                                                                                                                                       
         HEIGHT      20                                                                                                                                                                                                       
         VALUE       ""                                                                                                                                                                                                     
         MAXLENGTH   30
      END TEXTBOX                                                                                                                                                                                                           
                                                                                                                                                                                                                            
      DEFINE BUTTON BTN_Confirm                                                                                                                                                                                                
         CAPTION     "&Confirm"
         ONCLICK     (IIF(GETPROPERTY("Win_2","TXB_1","Value") == "",NIL,cFileName := GETPROPERTY("Win_2","TXB_1","Value")),Win_2.Release)
      END BUTTON                                                                                                                                                                                                            
                                                                                                                                                                                                                            
      DEFINE BUTTON BTN_Cancel                                                                                                                                                                                              
         CAPTION     "C&ancel"                                                                                                                                                                                                 
         ONCLICK     Win_2.Release
      END BUTTON                                                                                                                                                                                                            
   END WINDOW                                                                                                                                                                                                               

   SetRowColumnW("Win_2",{"BTN_Cancel","BTN_Confirm"})
   CENTER WINDOW Win_2
   ACTIVATE WINDOW Win_2

RETURN cFileName
    