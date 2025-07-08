#include "hmg.ch"


/****************************************************************************************
 * HMG EDIT EXTENDED command demo
 * (c) Roberto López [mail.box.hmg@gmail.com]
 *     Cristóbal Mollá [cemese@terra.es]
 *
 * Application: DEMO.EXE
 *    Function: Main()
 * Description: Main function of the demo. Create the main window.
 *  Parameters: None
 *      Return: NIL
****************************************************************************************/
function Main()

   // Database driver.
   REQUEST DBFCDX

   // [x]Harbour modifiers.
   SET CENTURY ON
   SET DELETED OFF
   SET DATE TO BRITISH

   // Request available languages for test.
   REQUEST HB_LANG_PT      // Portuguese.
   REQUEST HB_LANG_EU      // Basque.
   REQUEST HB_LANG_EN      // English.
   REQUEST HB_LANG_ES      // Spanish.
   REQUEST HB_LANG_FR      // French.
   REQUEST HB_LANG_IT      // Italian.
//        REQUEST HB_LANG_NL      // Dutch.
   REQUEST HB_LANG_PLWIN   // Polish Windows CP-1250
   REQUEST HB_LANG_DE      // German.


   // Set default language to English.
   HB_LANGSELECT( "EN" )


////////// Declaración de variables PUBLICAS.-----------------------------------
PUBLIC aOptions[1,2]
aOptions[1,1] := "Edit Etiquetas y Controles"
aOptions[1,2] := { || ModificarArray() }

cCities := "Ciudades->City"
aStates := {"AB","CD","AR","CA","AO","ZC","SC","WY","NE","IL","VT","IA","LA"}
aCity_State  := {,,,cCities,aStates,,,,,,}

   // Define the main window.
   DEFINE WINDOW Win_1                  ;
      AT         0,0                    ;
      WIDTH      getdesktopWidth()      ;
      HEIGHT     getDeskTopRealHeight() ;
      TITLE      "EDIT EXTENDED Demo"   ;
      MAIN                              ;
      NOMAXIMIZE                        ;
      NOSIZE                            ;
      ON INIT    OpenTable()            ;
      ON RELEASE CloseTable()           ;
      BACKCOLOR  GRAY

      DEFINE MAIN MENU OF Win_1
         POPUP "Make Arrays"
            ITEM "&Make all 4 Arrays"        ;
               ACTION ABM3generar()
         END POPUP
         POPUP "1 Column Compatibility"
            ITEM "Standard layout 1 column"                        ;
               ACTION ABM3()
         END POPUP
         POPUP "2 Columns"
            ITEM "2 col Horizontal - ComboBox+ListBox"                           ;
               ACTION ABM3 (,,,,,,aOptions,,,,"TEST1.2h.arr",aCity_State)
            ITEM "2 col Vertical - ComboBox+ListBox"      ;
               ACTION ABM3 (,,,,,,aOptions,,,,"TEST1.2v.arr",aCity_State)
         END POPUP
         POPUP "3 Columns"
            ITEM "3 col Horizontal - ComboBox+ListBox"                           ;
               ACTION ABM3 (,,,,,,aOptions,,,,"TEST1.3h.arr",aCity_State)
            ITEM "3 col Vertical - ComboBox+ListBox"      ;
               ACTION ABM3 (,,,,,,aOptions,,,,"TEST1.3v.arr",aCity_State)
         END POPUP
         POPUP "&Language"
            ITEM "&Select language"                                   ;
               ACTION SelectLang()
         END POPUP
         POPUP "&Exit"
            ITEM "About EDIT EXTENDED demo"                        ;
               ACTION About()
            SEPARATOR
            ITEM "&Exit demo"                                      ;
               ACTION Win_1.Release
         END POPUP
      END MENU

      DEFINE STATUSBAR FONT "ms sans serif" SIZE 9
          STATUSITEM "HMG EDIT EXTENDED command demo"
      END STATUSBAR

   END WINDOW

   // Open window.
   ACTIVATE WINDOW Win_1

return NIL



/****************************************************************************************
 * HMG EDIT EXTENDED command demo
 * (c) Roberto López [mail.box.hmg@gmail.com]
 *     Cristóbal Mollá [cemese@terra.es]
 *
 * Application: DEMO.EXE
 *    Function: OpenTable()
 * Description: Open the database files and check the index files.
 *  Parameters: None
 *      Return: NIL
****************************************************************************************/
procedure OpenTable()

   // Open the TEST1 database file with the DBFCDX Driver.----------------
   dbUseArea( .t., "DBFCDX", "Ciudades.dbf", "Ciudades" )
   dbUseArea( .t., "DBFCDX", "TEST1.DBF", "TEST1" )

   if !File( "Ciudades.cdx" )
      TEST1->( ordCreate( "Ciudades.cdx",                       ;
                "City",                             ;
                "Ciudades->City",                   ;
                {|| Ciudades->City } ) )
   endif                

   // Check the existence of the index files.-----------------------------
   if !File( "TEST1.CDX" )

      // Create order by first field plus last field.
      // You can't search by this order. Only for test.
      TEST1->( ordCreate( "TEST1.CDX",                        ;
                "First Name",                       ;
                "TEST1->First + TEST1->Last",       ;
                {|| TEST1->First  + TEST1->Last } ) )

      // Create order by last field.
      TEST1->( ordCreate( "TEST1.CDX",                        ;
                "Last Name",                        ;
                "TEST1->Last",                      ;
                {|| TEST1->Last } ) )

      // Create order by hiredate field.
      TEST1->( ordCreate( "TEST1.CDX",                        ;
                "Hire Date",                        ;
                "TEST1->Hiredate",                  ;
                {|| TEST1->Hiredate } ) )

      // Create order by age field.
      TEST1->( ordCreate( "TEST1.CDX",                        ;
                "Age",                              ;
                "TEST1->Age",                       ;
                {|| TEST1->Age } ) )

      // Create order by.
      // You can't search by this order. Only for test.
      TEST1->( ordCreate( "TEST1.CDX",                        ;
                "Married",                          ;
                "TEST1->Married",                   ;
                {|| TEST1->Married } ) )
   endif

   // Open the index files for TEST1 workarea. ---------------------------
   TEST1->( ordListAdd( "TEST1.CDX", "First Name" ) )
   TEST1->( ordListAdd( "TEST1.CDX", "Last Name" ) )
   TEST1->( ordListAdd( "TEST1.CDX", "Hire Date" ) )
   TEST1->( ordListAdd( "TEST1.CDX", "Age" ) )
   TEST1->( ordListAdd( "TEST1.CDX", "Married" ) )
   TEST1->( ordSetFocus( 1 ) )

return



/****************************************************************************************
 * HMG EDIT EXTENDED command demo
 * (c) Roberto López [mail.box.hmg@gmail.com]
 *     Cristóbal Mollá [cemese@terra.es]
 *
 * Application: DEMO.EXE
 *    Function: CloseTable()
 * Description: Closes the active databases.
 *  Parameters: None
 *      Return: NIL
****************************************************************************************/
procedure CloseTable()

   CLOSE TEST1
   CLOSE Ciudades

return



/****************************************************************************************
 * HMG EDIT EXTENDED command demo
 * (c) Roberto López [mail.box.hmg@gmail.com]
 *     Cristóbal Mollá [cemese@terra.es]
 *
 * Application: DEMO.EXE
 *    Function: BasicDemo( cArea)
 * Description: Run a basic demo (without parameters) of EDIT command.
 *  Parameters: [cArea]         Character. Name of the workarea.
 *      Return: NIL
****************************************************************************************/
procedure BasicDemo( cArea )

   // Basic demo of EDIT command.
*     EDIT EXTENDED WORKAREA &cArea
   ABM3 ( cArea )

return



/****************************************************************************************
 * HMG EDIT EXTENDED command demo
 * (c) Roberto López [mail.box.hmg@gmail.com]
 *     Cristóbal Mollá [cemese@terra.es]
 *
 * Application: DEMO.EXE
 *    Function: SelectLang()
 * Description: Select the [x]Harbour default language.
 *  Parameters: None
 *      Return: NIL
****************************************************************************************/
procedure SelectLang()

   LOCAL cMessage  := ""
   LOCAL nItem     := 0
   LOCAL aLangName := { "Basque"             ,;
              "Dutch"              ,;
              "English"            ,;
              "French"             ,;
              "German"             ,;
              "Italian"            ,;
              "Polish"             ,;
              "Portuguese"         ,;
              "Spanish"             }
   LOCAL aLangID   := { "EU"    ,;
              "NL"    ,;
              "EN"    ,;
              "FR"    ,;
              "DE"    ,;
              "IT"    ,;
              "PLWIN" ,;
              "PT"    ,;
              "ES"     }

   // Language selection.
   cMessage := CRLF
   cMessage += "You can change EDIT EXTENDED interface default language, by changing   " + CRLF
   cMessage += "[x]Harbour default language with HB_LANGSELECT() fuction.   " + CRLF
   cMessage += CRLF
   cMessage += "If your language is not supported and you want translate   " + CRLF
   cMessage += "the EDIT EXTENDED interface to it, please post a message to the   " + CRLF
   cMessage += "HMG discussion group at Yahoo Groups.   " + CRLF
   MsgInfo( cMessage , "EDIT EXTENDED demo" )
   nItem := SelectItem( aLangName )
   IF .NOT. nItem == 0
      HB_LANGSELECT( aLangID[nItem] )
   ENDIF

return



/****************************************************************************************
 * HMG EDIT EXTENDED command demo
 * (c) Roberto López [mail.box.hmg@gmail.com]
 *     Cristóbal Mollá [cemese@terra.es]
 *
 * Application: DEMO.EXE
 *    Function: SelectItem( acItems )
 * Description: Select an item from an array of character items.
 *  Parameters: [acItems]       Array of character items.
 *      Return: [nItem]         Number of selected item.
****************************************************************************************/
function SelectItem( acItems )

   // Local variable declarations.----------------------------------------
   LOCAL nItem := 0

   // Create the selection window.----------------------------------------
   DEFINE WINDOW wndSelItem ;
      AT 0, 0 ;
      WIDTH 265 ;
      HEIGHT 160 ;
      TITLE "Select item" ;
      MODAL ;
      NOSIZE ;
      NOSYSMENU

      @ 20, 20 LISTBOX lbxItems ;
         WIDTH 140 ;
         HEIGHT 100 ;
         ITEMS acItems ;
         VALUE 1 ;
         FONT "Arial" ;
         SIZE 9

      @ 20, 170 BUTTON btnSel ;
         CAPTION "&Select" ;
         ACTION {|| nItem := wndSelItem.lbxItems.Value, wndSelItem.Release } ;
         WIDTH 70 ;
         HEIGHT 30 ;
         FONT "ms sans serif" ;
         SIZE 8

   END WINDOW

   // Activate the window.------------------------------------------------
   wndSelItem.lbxItems.SetFocus
   CENTER WINDOW wndSelItem
   ACTIVATE WINDOW wndSelItem

return ( nItem )



/****************************************************************************************
 * HMG EDIT EXTENDED command demo
 * (c) Roberto López [mail.box.hmg@gmail.com]
 *     Cristóbal Mollá [cemese@terra.es]
 *
 * Application: DEMO.EXE
 *    Function: About()
 * Description: Shows the about window.
 *  Parameters: None
 *      Return: NIL
****************************************************************************************/
procedure About()

   // Local variable declaration.-----------------------------------------
   LOCAL cMessage := ""

   // Shows the about window.---------------------------------------------
   cMessage := CRLF
   cMessage += "EDIT EXTENDED command for MiniGUI" + CRLF
   cMessage += "Last Update: October 2019    " + CRLF
   cMessage += CRLF
   cMessage += "The EDIT EXTENDED command was developed by:  " + CRLF
   cMessage += "---> Roberto López" + CRLF
   cMessage += "---> Cristóbal Mollá" + CRLF
   cMessage += CRLF
   cMessage += "Please report bugs to HMG discusion group at http://www.hmgforum.com" + CRLF
   MsgInfo( cMessage, "About EDIT EXTENDED command demo" )

return
