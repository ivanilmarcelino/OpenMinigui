
#include "MiniGui.ch"

//-----------------------------------------------------------------------------

procedure Main ()
local cText1 := "MiniGUI Extended already has a number of functions to manage .INI files. " + ;
                "But I needed to do more than these functions can do. " +                ;
                "So, based on this existing group of functions, I have created " +       ;
                "a new bunch of functions with more validation plus a few new "+         ;
                "functions to suit my needs. I also included an .INI editor, "+          ;
                "which is based on Grigory Filatov's sample file «INIFileEditor», "+     ;
                "which I also adapted to my needs."+ CRLF + CRLF +                       ;
                "This small tutorial will walk you through the capabilities of this "+   ;
                "little library. Let's try it out..."+ CRLF +                            ;
                "First, enter the name of the .INI file you want to test, then "+        ;
                "press the «Start Test» button..."

local cText2 := "It would be nice to be able to edit the INI file as well. Well, that's possible "+     ;
                "too. Based on Grigory Filatov's example file 'INIFileEditor' that I slightly "+        ;
                "modified and integrated into this small library, it is possible to call a function " + ;
                "named «IniEditor()» directly from your application without the need to call "+         ;
                "an external program, since it is included in the library."+ CRLF +                     ;
                "Thanks to Grigory for this useful utility, it easily supports your INI files."

set date format "dd/mm/yyyy"
set century ON
set epoch to Year(Date())-25

define window oWndTest      ;
       at 0, 0              ;
       width 400 height 400 ;
       title "IniTest"      ;
       main nosize

   @  10,  10 editbox oEdtBox         ;
              width 380 height 200    ;
              value cText1            ;
              backcolor {150,200,200} ;
              readonly nohscroll notabstop

   @ 220,  10 label oLblIniName width 125 height 20 value "INI file name to create :" rightalign
   @ 220, 140 textbox oTxtIniName width 100 height 20     ;
              value Space(12)                             ;
              uppercase                                   ;
              on enter {|| oWndTest.oBtnStart.SetFocus()} ;
              on lostfocus {|| ValidName(oWndTest.oTxtIniName.Value)}

   @ 260,  10 button oBtnStart      ;
              caption "Start Test"  ;
              width 100 height 30   ;
              font "TAHOMA" size 10 ;
              action {|| TestIni(oWndTest.oTxtIniName.Value), ;
                         oWndTest.oBtnStart.Enabled := .F.,   ;
                         oWndTest.oBtnEdit.Enabled := .T.,    ;
                         oWndTest.oBtnEdit.SetFocus(),        ;
                         oWndTest.oEdtBox.Value := cText2,    ;
                         oWndTest.oEdtBox.BackColor := {205,200,255}}

   @ 260, 280 button oBtnEdit       ;
              caption "Edit INI"    ;
              width 100 height 30   ;
              font "TAHOMA" size 10 ;
              action {|| IniEditor(oWndTest.oTxtIniName.Value)}

   @ 320, 150 button oBtnCancel     ;
              caption "Exit (Esc)"  ;
              width 100 height 30   ;
              font "TAHOMA" size 10 ;
              action {|| oWndTest.Release()}

   _DefineHotKey("oWndTest",, VK_ESCAPE, {|| oWndTest.Release()})

end window

oWndTest.oBtnEdit.Enabled := .F.

oWndTest.Center()
oWndTest.Activate()

Return

//-----------------------------------------------------------------------------

function ValidName (cIniName)
local cMessage := ""

if This.FocusedControl == "oBtnCancel"
   Return (NIL)
endif

do case
case (cIniName == NIL .or. Empty(cIniName))
   cMessage := "INI file name not specified !"
   oWndTest.oTxtIniName.SetFocus()
case Empty(HB_FNameName(cIniName)) .and. !Empty(HB_FNameExt(cIniName))
   cMessage := "File name not complete !"
   oWndTest.oTxtIniName.SetFocus()
case !Empty(HB_FNameName(cIniName)) .and. Empty(HB_FNameExt(cIniName))
   oWndTest.oTxtIniName.Value := AllTrim(cIniName) +".INI"
   oWndTest.oBtnStart.SetFocus()
otherwise
   oWndTest.oBtnStart.SetFocus()
endcase
if !Empty(cMessage)
   MsgStop(cMessage, "Error")
endif

Return (NIL)

//-----------------------------------------------------------------------------

function TestIni (cTestFile)
local aIni
local aArray
local aMatrix
local aHash
local lVerbose := .T.

aIni := {"[MP3]",      ;
         "DirInMP3=",  ;
         "DirOutMP3=", ;
         "",           ;
         "[CAT]",      ;
         "DirInCAT=",  ;
         "DirOutCAT="}

aArray  := {"Madame Blue", "Crystal Ball", "Mr. Roboto", "Sweet Mademoiselle", "Paradise Theatre"}
aMatrix := {{"Madame Blue", 1, .F.}, {"Crystal Ball", 2, .T.}, {"Mr. Roboto", 3, .T.}, {"Sweet Mademoiselle", 4, .T.}, {"Paradise Theatre", 5, .F.}}
aHash   := {"Madame Blue" => 1, "Crystal Ball" => 2, "Mr. Roboto" => 3, "Sweet Mademoiselle" => 4, "Paradise Theatre" => 5}

// IniCreate () -----------------------------------------------------------------------------
MsgText("Let's create an initial INI file with the function «IniCreate()» using an array:"+ CRLF + CRLF + ;
        "aIni := {'[MP3]',      ;"+ CRLF +                                                                ;
        "         'DirInMP3=',  ;"+ CRLF +                                                                ;
        "         'DirOutMP3=', ;"+ CRLF +                                                                ;
        "         '',           ;"+ CRLF +                                                                ;
        "         '[CAT]',      ;"+ CRLF +                                                                ;
        "         'DirInCAT=',  ;"+ CRLF +                                                                ;
        "         'DirOutCAT='}  "+ CRLF + CRLF +                                                         ;
        "Then function is called as :  IniCreate(cTestFile, aIni)")
IniCreate(cTestFile, aIni)
ShellExecute(, "OPEN", "NOTEPAD.EXE", cTestFile, HB_Dirbase(), 1)
// MsgInfo("Continue...")
// CloseProcess("NOTEPAD.EXE")

// IniAddEntry () -----------------------------------------------------------------------------
MsgText("Now let's put some values in the Section Entries..."+ CRLF + CRLF + ;
        "For this I use the following syntax:" + CRLF + CRLF +               ;
        "IniAddEntry(cTestFile, 'CAT', 'DirInCAT', 'Z:\Music\CATIn'")
CloseProcess("NOTEPAD.EXE")
IniAddEntry(cTestFile, "MP3", "DirInMP3", "Z:\Music\MP3In")
IniAddEntry(cTestFile, "MP3", "DirOutMP3", "Z:\Music\MP3Out")
IniAddEntry(cTestFile, "CAT", "DirInCAT", "Z:\Music\CATIn")
IniAddEntry(cTestFile, "CAT", "DirOutCAT", "Z:\Music\CATOut")
ShellExecute(, "OPEN", "NOTEPAD.EXE", cTestFile, HB_Dirbase(), 1)
// MsgInfo("Continue...")
// CloseProcess("NOTEPAD.EXE")

// IniAddBlank () -----------------------------------------------------------------------------
MsgText("So far so good. How about adding a SECTION and an ENTRY with a VALUE."+ CRLF + CRLF + ;
        "But first let's add a blank line before doing this."+ CRLF +                          ;
        "Using the following function will allow to do that:" + CRLF + CRLF +                  ;
        "IniAddBlank(cTestFile)"+ CRLF + CRLF +                                                ;
        "then"+ CRLF + CRLF +                                                                  ;
        "IniAddEntry(cTestFile, 'ALBUM', 'Artist', 'Styx')"+ CRLF + CRLF +                     ;
        "and so on until all ENTRIES have been made for this new SECTION"+ CRLF + CRLF +       ;
        "Let's see if it works...")
CloseProcess("NOTEPAD.EXE")
IniAddBlank(cTestFile)
IniAddEntry(cTestFile, "ALBUM", "Artist", "Styx")
IniAddEntry(cTestFile, "ALBUM", "Year", 1977)
IniAddEntry(cTestFile, "ALBUM", "Date", CtoD("05/07/1977"))
IniAddEntry(cTestFile, "ALBUM", "Stock", .T.)
ShellExecute(, "OPEN", "NOTEPAD.EXE", cTestFile, HB_Dirbase(), 1)
// MsgInfo("Continue...")
// CloseProcess("NOTEPAD.EXE")

// IniAddEntry () (Arrays) ---------------------------------------------------------------------
MsgText("Ok. What just happened is I added a STRING, a NUMERIC, a DATE and a LOGIC "+  ;
        "expression to 4 new entries in a new section. But what about «Arrays». "+     ;
        "Is it possible to also store them into an INI file ?"+ CRLF + CRLF +          ;
        "Let's try to do something crazy. I have three arrays to save:"+ CRLF + CRLF + ;
        "Array 1 => is a 1 dimension array"+ CRLF +                                    ;
        "Array 2 => is a 2 dimensions array"+ CRLF +                                   ;
        "Array 3 => is an Hash array")
CloseProcess("NOTEPAD.EXE")
IniAddEntry(cTestFile, "ALBUM", "Tracks (Array)", aArray)    // Array 1
IniAddEntry(cTestFile, "ALBUM", "Tracks (Matrix)", aMatrix)  // Array 2
IniAddEntry(cTestFile, "ALBUM", "Tracks (Hash)", aHash)      // Array 3
ShellExecute(, "OPEN", "NOTEPAD.EXE", cTestFile, HB_Dirbase(), 1)
// MsgInfo("Continue...")
// CloseProcess("NOTEPAD.EXE")

// IniGetEntry () -----------------------------------------------------------------------------
MsgText("Yes it can be done, and using the original syntax."+ + CRLF + CRLF +         ;
        "Hum... At some point I will need to read those values back from the file. "+ ;
        "It Should work when using the following function:"+ CRLF + CRLF +            ;
        "IniGetEntry(cTestFile, 'ALBUM', 'Artist')"+ CRLF + CRLF +                    ;
        "There's actually more about this function because you can pass a "+          ;
        "DEFAULT value, a value TYPE and a VERBOSE parameters. More information "+    ;
        "in the documentation."+ CRLF + CRLF +                                        ;
        "For now, let's go on with the test.")
CloseProcess("NOTEPAD.EXE")
MsgTest(IniGetEntry(cTestFile, "ALBUM", "Artist"))
MsgTest(IniGetEntry(cTestFile, "ALBUM", "Year"))
MsgTest(IniGetEntry(cTestFile, "ALBUM", "Date"))
MsgTest(IniGetEntry(cTestFile, "ALBUM", "Stock"))

// IniGetEntry () -----------------------------------------------------------------------------
MsgText("Notice that the results from the function «IniGetEntry()» has converted the "+               ;
        "data to it's original type without passing any parameter that specify's how to "+            ;
        "behave. The function try's to do so automatically when the 'cType' parameter "+              ;
        "is not specified. It is possible to pass that parameter, but it could be tricky "+           ;
        "and could cause a crash if the parameter does not correspond to the reality."+ CRLF + CRLF + ;
        "Ex.: IniGetEntry(cTestFile, 'ALBUM', 'Artist',, 'A')"+ CRLF + CRLF +                         ;
        "In this case converting a String expression to an Array expression could cause "+            ;
        "problems. Not passing «cType» parameter will, in the worst case, return the "+               ;
        "retrieved value as String expression if the function can't identify the appropiate "+        ;
        "conversion to apply.")
MsgText("Now let's try to retreive something that does not exist, but this time using "+ ;
        "the VERBOSE parameter. This parameter allows the functions to display a "+      ;
        "message if something is incorrect. So let's do some intentional errors.")
IniGetEntry(cTestFile,, "Keyboard",,, lVerbose)
IniGetEntry(cTestFile, "MEMBERS",,,, lVerbose)
IniGetEntry(cTestFile, "MEMBERS", "Keyboard",,, lVerbose)
IniGetEntry(cTestFile, "ALBUM", "Keyboard",,, lVerbose)
IniGetEntry(cTestFile, "ALBUM", "Artist",, "v", lVerbose)

// IniGetEntry () (Array) ------------------------------------------------------------------------
MsgText("We have retrieved some Entry values, like String, Numeric, Date & Logic expression, "+ ;
        "but remember the three type of array that were saved earlier to the INI file ? Well "+ ;
        "let's try to fetch those too and see what happens..."+ CRLF +                          ;
        "First the simple 1 dimension Array...")
MsgTest(IniGetEntry(cTestFile, "ALBUM", "Tracks (Array)"), "1 Dimension => Tracks (Array) Entry")

// IniGetEntry () (Matrix) -----------------------------------------------------------------------
MsgText("Second the 2 dimensions array...")
MsgTest(IniGetEntry(cTestFile, "ALBUM", "Tracks (Matrix)"), "2 Dimension => Tracks (Matrix) Entry")

// IniGetEntry () (Hash) -------------------------------------------------------------------------
MsgText("And finally the Hash array...")
MsgTest(IniGetEntry(cTestFile, "ALBUM", "Tracks (Hash)"), "Hash => Tracks (Hash) Entry")

// IniIsSection () & IniIsEntry () ---------------------------------------------------------------
MsgText("Next, I need to know if a Section or an Entry exist. Can do that too ! "+ ;
        "Simply use the following functions:"+ CRLF + CRLF +                       ;
        "IniIsSection(cTestFile, 'MP3')"+ CRLF + CRLF +                            ;
        "or"+ CRLF + CRLF +                                                        ;
        "IniIsEntry(cTestFile, 'ALBUM', 'DirOutCAT')")
ShellExecute(, "OPEN", "NOTEPAD.EXE", cTestFile, HB_Dirbase(), 1)
MsgInfo("Section MP3"+ CRLF + iif(IniIsSection(cTestFile, "MP3"), "TRUE", "FALSE"))
MsgInfo("Section ALBUM, Entry DirOutCAT"+ CRLF + iif(IniIsEntry(cTestFile, "ALBUM", "DirOutCAT"), "TRUE", "FALSE"))
// MsgInfo("Continue...")
CloseProcess("NOTEPAD.EXE")

// IniGetSections () & IniGetEntries () ----------------------------------------------------------
MsgText("Another nice feature of MiniGUI is the fact that it is possible to retrieve a list of "+    ;
        "either all the Sections in an INI file or all the Entries of a Section in that same INI "+  ;
        "file. Since I had created a bunch of functions with validation, may as well do the same "+  ;
        "for those next two functions by wrapping them in the same way. So: "+ CRLF + CRLF +         ;
        "IniGetSections() and IniGetEntries() have the same validation features than the rest of "+  ;
        "the functions.")
MsgTest(IniGetSections(cTestFile), "List of Sections")
MsgTest(IniGetEntries(cTestFile, "ALBUM"), "List of Entries of Section ALBUM")

// IniDelValue () -----------------------------------------------------------------------------
MsgText("We've done a lot of things since the start of this demo. But one thing we did'nt do is deleting "+ ;
        "data from an INI file. So let's try that too..." + CRLF + CRLF +                                   ;
        "Four functions are available for deleting data from an INI file."+ CRLF +                          ;
        "First => IniDelValue(cTestFile, 'ALBUM', 'Date') will empty the Value in the Entry of a Section")
IniDelValue(cTestFile, "ALBUM", "Date")
ShellExecute(, "OPEN", "NOTEPAD.EXE", cTestFile, HB_Dirbase(), 1)
// MsgInfo("Continue...")

// IniDelEntry () -----------------------------------------------------------------------------
MsgText("Second => IniDelEntry(cTestFile, 'ALBUM', 'Year') will remove an Entry of a Section")
CloseProcess("NOTEPAD.EXE")
IniDelEntry(cTestFile, "ALBUM", "Year")
ShellExecute(, "OPEN", "NOTEPAD.EXE", cTestFile, HB_Dirbase(), 1)
// MsgInfo("Continue...")

// IniDelSection () ---------------------------------------------------------------------------
MsgText("Third => IniDelSection(cTestFile, 'ALBUM') will remove a whole Section")
CloseProcess("NOTEPAD.EXE")
IniDelSection(cTestFile, "ALBUM", lVerbose)
ShellExecute(, "OPEN", "NOTEPAD.EXE", cTestFile, HB_Dirbase(), 1)
// MsgInfo("Continue...")

// IniZap () -----------------------------------------------------------------------------
MsgText("Finally the fourth function => IniZap(cTestFile) will empty the INI file completely")
CloseProcess("NOTEPAD.EXE")
IniZap(cTestFile, lVerbose)
ShellExecute(, "OPEN", "NOTEPAD.EXE", cTestFile, HB_Dirbase(), 1)
MsgInfo("Continue...")
CloseProcess("NOTEPAD.EXE")

// IniSet () & IniReset () -----------------------------------------------------------------------
MsgText("Now that we have an empty INI file, there's not much we can do with it other than "+    ;
        "recreating it. Let's do this, but this time using two functions that will make "+       ;
        "things a bit easier."+ CRLF + CRLF +                                                    ;
        "Function «IniSet()» will allows to set two parameters that will remain active until "+  ;
        "function «IniReset() is called. That permits to call the other functions without "+     ;
        "having to specify the INI file name and verbose parameters on every functions."+ CRLF + ;
        "This is true for all functions except two: IniCreate() and IniZap()."+ CRLF + CRLF +    ;
        "Ex.: IniCreate(cIniFile, aArray, lVerbose)"+ CRLF +                                     ;
        "     if IniSet(cIniFile, lVerbose)"+ CRLF +                                             ;
        "        IniAddEntry(, 'MP3', 'DirInMP3', 'Z:\Music\MP3In')"+ CRLF +                     ;
        "        IniAddEntry(, 'MP3', 'DirOutMP3', 'Z:\Music\MP3Out')"+ CRLF +                   ;
        "        IniAddBlank()"+ CRLF +                                                          ;
        "        IniAddEntry(, 'ALBUM', 'Artist', 'Styx')"+ CRLF +                               ;
        "        ..."+ CRLF +                                                                    ;
        "        ..."+ CRLF +                                                                    ;
        "        if IniIsSection(, 'ALBUM')"+ CRLF +                                             ;
        "           aEntries := IniGetEntries(, 'ALBUM')"+ CRLF +                                ;
        "           nYear := IniGetEntry(, 'ALBUM', 'Year')"+ CRLF +                             ;
        "        endif"+ CRLF +                                                                  ;
        "        IniDelValue(, 'ALBUM', 'Date')"+ CRLF +                                         ;
        "     endif"+ CRLF +                                                                     ;
        "     IniReset()")
IniCreate(cTestFile, aIni, lVerbose)
IniSet()
if IniSet(cTestFile, lVerbose)
   IniSet()
   IniAddEntry(, "MP3", "DirInMP3", "Z:\Music\MP3In")
   IniAddEntry(, "MP3", "DirOutMP3", "Z:\Music\MP3Out")
   IniAddEntry(, "CAT", "DirInCAT", "Z:\Music\CATIn")
   IniAddEntry(, "CAT", "DirOutCAT", "Z:\Music\CATOut")
   IniAddBlank()
   IniAddEntry(, "ALBUM", "Artist", "Styx")
   IniAddEntry(, "ALBUM", "Year", 1977)
   IniAddEntry(, "ALBUM", "Date", CtoD("05/07/1977"))
   IniAddEntry(, "ALBUM", "Stock", .T.)
   IniAddEntry(, "ALBUM", "Tracks (Array)", aArray)    // Array 1
   IniAddEntry(, "ALBUM", "Tracks (Matrix)", aMatrix)  // Array 2
   IniAddEntry(, "ALBUM", "Tracks (Hash)", aHash)      // Array 3
   MsgInfo("Let's re-test the same errors !")
   IniGetEntry(cTestFile,, "Keyboard")
   IniGetEntry(cTestFile, "MEMBERS")
   IniGetEntry(cTestFile, "MEMBERS", "Keyboard")
   IniGetEntry(cTestFile, "ALBUM", "Keyboard")
   IniGetEntry(cTestFile, "ALBUM", "Artist",, "v")
   if IniIsSection(, "ALBUM") .and. IniIsEntry(, "ALBUM", "Artist")
      MsgInfo("Section «ALBUM» and Entry «Artist» have been found."+ CRLF + ;
              "Value is : "+ IniGetEntry(, "ALBUM", "Artist"))
   endif
   if IniIsEntry(, "ALBUM", "Tracks (Hash)")
      MsgTest(IniGetEntry(, "ALBUM", "Tracks (Hash)"), "Hash => Tracks (Hash) Entry")
   endif
endif
IniReset()
ShellExecute(, "OPEN", "NOTEPAD.EXE", cTestFile, HB_Dirbase(), 1)
MsgText("We now have an INI file to work with again.")
CloseProcess("NOTEPAD.EXE")

Return (NIL)

//-----------------------------------------------------------------------------

function MsgText (cText, cTitle)

cTitle := iif(cTitle == NIL, "Information", cTitle)

define window oDlgText      ;
       at 0, 0              ;
       width 800 height 290 ;
       title cTitle         ;
       modal nosize nosysmenu

   @  10,  10 editbox oEdtText            ;
              width 780 height 200        ;
              value cText                 ;
              backcolor {255,255,255}     ;
              readonly nohscroll notabstop

   @ 225, 350 button oBtnQuit       ;
              caption "Continue..." ;
              width 100 height 30   ;
              font "TAHOMA" size 10 ;
              action {|| oDlgText.Release()}

end window

oDlgText.oBtnQuit.SetFocus()

oDlgText.Center()
oDlgText.Activate()

Return (NIL)


//---------------------------------------------------------------------------
//
// MsgTest (uValue, cValue)
//
// Purpose:  Display dialog box with results of testing
//
//  Params:  <uValue> User value to be displayed
//                     -> Can be String, Memo, Numeric, Date, Array expression or Database
//           <cValue> Result identifier
//
//  Return: Nothing
//
//---------------------------------------------------------------------------

function MsgTest (uValue, cValue)

cValue := iif(cValue == NIL, "", cValue)
do case
case Select(uValue) <> 0
   _DbfBrowse("Database ("+ cValue +")", uValue)

case HB_IsString(uValue) .and. Len(uValue) > 255
   _MemoEdit("Long String ("+ cValue +")", uValue)

case HB_IsString(uValue)
   MsgInfo("String Expression ("+ cValue +") = "+ uValue + Chr(32) +Chr(10) + ;
           "Lenght of Variable ="+ AllTrim(Str(Len(uValue))))

case HB_IsNumeric(uValue)
   MsgInfo("Numeric Expression ("+ cValue +") = "+ Str(uValue))

case HB_IsDate(uValue)
   MsgInfo("Date Expression ("+ cValue +") = "+ DtoC(uValue))

case HB_IsLogical(uValue)
   MsgInfo("Logical Expression ("+ cValue +") = "+ iif(uValue, "TRUE", "FALSE"))

case HB_IsMemo(uValue)
   _MemoEdit("Memo field ("+ cValue +")", uValue)

case HB_IsHash(uValue)
   _HashBrowse("Hash Array ("+ cValue +")", uValue)

case HB_IsArray(uValue)
   _ArrBrowse("Array ("+ cValue +")", uValue)

case HB_IsNIL(uValue)
   MsgInfo("NIL Expression ("+ cValue +")")

case HB_IsNull(uValue)
   MsgInfo("Null Expression ("+ cValue +")")

endcase

Return (NIL)

//-----------------------------------------------------------------------------

function _MemoEdit (cValue, uValue)

define window oDlgResult    ;
       at 0, 0              ;
       width 640 height 400 ;
       title cValue         ;
       modal                ;
       nosize

   @  10,  10 editbox oEditMemo    ;
              width 610 height 300 ;
              value uValue         ;
              readonly notabstop nohscroll

   @ 330, 260 button oBtnEnd ;
              caption "&End" ;
              action {|| oDlgResult.Release()}

   _DefineHotKey("oDlgResult",,         VK_ESCAPE, {|| oDlgResult.oBtnEnd.Action()})
   _DefineHotKey("oDlgResult", MOD_ALT, VK_X,      {|| oDlgResult.oBtnEnd.Action()})

end window

oDlgResult.Center()
oDlgResult.Activate()

Return (NIL)

//-----------------------------------------------------------------------------

static function _ArrBrowse (cTitle, uMatrix)
local z, x
local oDlgResult
local oGrdResult
local nArrRow
local nArrCol
local aMatrix
local aHeader  := {}
local aColSize := {}
local nRow
local nCol

cTitle  := iif(cTitle == NIL, "Array", cTitle)
nArrRow := Len(uMatrix)
if nArrRow == 0
   MsgInfo("Array is empty")
   Return (NIL)
endif
nArrCol := iif(ValType(uMatrix[1]) <> "A", 1, Len(uMatrix[1]))
if nArrCol == 0
   MsgInfo("Array is empty")
   Return (NIL)
endif
++nArrCol

aMatrix := Array(nArrRow, nArrCol)
for z = 1 to nArrRow
   for x = 1 to nArrCol
      do case
      case x == 1
         aMatrix[z, x] := Str(z, 4)
      otherwise
         if nArrCol == 2
            aMatrix[z, x] := _Convert(uMatrix[z])
         else
            aMatrix[z, x] := _Convert(uMatrix[z, x -1])
         endif
      endcase
   next x
next z
for x = 1 to nArrCol
   if x == 1
      AAdd(aHeader, "Row")
      AAdd(aColSize, 35)
   else
      AAdd(aHeader, "Field "+ AllTrim(Str(x -1)))
      AAdd(aColSize, 100)
   endif
next z

nRow := Application.Row + Int(Application.Height /2) - Int(400 /2)
nCol := Application.Col + Int(Application.Width /2) - Int(900 /2)

define window oDlgResult ;
       at     nRow, nCol ;
       width  900        ;
       height 400        ;
       title  cTitle     ;
       modal

   @ 5, 5 grid oGrdResult   ;
          width    885      ;
          height   300      ;
          headers  aHeader  ;
          widths   aColSize ;
          items    aMatrix  ;
          value    1        ;

   @ 320, 400 button oBtnEnd ;
              caption "&End" ;
              action (oDlgResult.Release)

   _DefineHotKey("oDlgResult",,         VK_ESCAPE, {|| oDlgResult.oBtnEnd.Action()})
   _DefineHotKey("oDlgResult", MOD_ALT, VK_X,      {|| oDlgResult.oBtnEnd.Action()})

end window
oDlgResult.oBtnEnd.SetFocus()

oDlgResult.Activate()

Return (NIL)

//-----------------------------------------------------------------------------

static function _HashBrowse (cTitle, uMatrix)
local z
local oDlgResult
local oGrdResult
local nArrRow
local nArrCol  := 4
local aTemp    := {}
local aMatrix
local aHeader  := {}
local aColSize := {}
local nRow
local nCol

cTitle  := iif(cTitle == NIL, "Hash Array", cTitle)
nArrRow := Len(uMatrix)
if nArrRow == 0
   MsgInfo("Hash Array is empty")
   Return (NIL)
endif

for z = 1 to nArrRow
   AAdd(aTemp, hb_HPairAt(uMatrix, z))
next z

aMatrix := Array(nArrRow, nArrCol)
for z = 1 to nArrRow
   aMatrix[z, 1] := Str(z, 4)
   aMatrix[z, 2] := _Convert(aTemp[z, 1])
   aMatrix[z, 3] := "=>"
   aMatrix[z, 4] := _Convert(aTemp[z, 2])
next z

AAdd(aHeader, "Idx")
AAdd(aColSize, 35)
AAdd(aHeader, "HKeys")
AAdd(aColSize, 100)
AAdd(aHeader, "=>")
AAdd(aColSize, 35)
AAdd(aHeader, "HValues")
AAdd(aColSize, 100)

nRow := Application.Row + Int(Application.Height /2) - Int(400 /2)
nCol := Application.Col + Int(Application.Width /2) - Int(900 /2)

define window oDlgResult ;
       at     nRow, nCol ;
       width  900        ;
       height 400        ;
       title  cTitle     ;
       modal

   @ 5, 5 grid oGrdResult   ;
          width    885      ;
          height   300      ;
          headers  aHeader  ;
          widths   aColSize ;
          items    aMatrix  ;
          value    1        ;

   @ 320, 400 button oBtnEnd ;
              caption "&End" ;
              action (oDlgResult.Release)

   _DefineHotKey("oDlgResult",,         VK_ESCAPE, {|| oDlgResult.oBtnEnd.Action()})
   _DefineHotKey("oDlgResult", MOD_ALT, VK_X,      {|| oDlgResult.oBtnEnd.Action()})

end window
oDlgResult.oBtnEnd.SetFocus

oDlgResult.Activate()

Return (NIL)

//-----------------------------------------------------------------------------

static function _DbfBrowse (cTitle, cAlias)
local z
local oDlgResult
local oBrwResult
local nDbfCol     := (cAlias)->(FCount())
local aHeader  := {}
local aColSize := {}
local aField   := {}
local nRow
local nCol

cTitle := iif(cTitle == NIL, "Database", cTitle)

for z = 1 to nDbfCol
   AAdd(aHeader, (cAlias)->(FieldName(z)))
   AAdd(aField, (cAlias)->(FieldName(z)))
   AAdd(aColSize, (cAlias)->(FieldSize(z)) *10)
next z

nRow := Application.Row + Int(Application.Height /2) - Int(400 /2)
nCol := Application.Col + Int(Application.Width /2) - Int(700 /2)

define window oDlgResult ;
       at     nRow, nCol ;
       width  700        ;
       height 400        ;
       title  cTitle     ;
       modal

   @ 5, 5 browse oBrwResult  ;
          width    685       ;
          height   300       ;
          headers  aHeader   ;
          widths   aColSize  ;
          workarea &(cAlias) ;
          fields   aField

   @ 320, 300 button oBtnEnd ;
              caption "&End" ;
              action (oDlgResult.Release)

   _DefineHotKey("oDlgResult",,         VK_ESCAPE, {|| oDlgResult.oBtnEnd.Action()})
   _DefineHotKey("oDlgResult", MOD_ALT, VK_X,      {|| oDlgResult.oBtnEnd.Action()})

end window

oDlgResult.Activate()

Return (NIL)

//---------------------------------------------------------------------------

static function _Convert (uValue)
local cReturn := " "

do case
case uValue == NIL
   cReturn := "NIL"
case ValType(uValue) == "C"
   cReturn := uValue
case ValType(uValue) == "N"
   cReturn := Str(uValue)
case ValType(uValue) == "D"
   cReturn := DtoC(uValue)
case ValType(uValue) == "L"
   cReturn := iif(uValue, "T", "F")
case ValType(uValue) == "B"
   cReturn := "{|x|}"
case ValType(uValue) == "A"
   cReturn := "{...}"
case ValType(uValue) == "H"
   cReturn := "{...}"
endcase

Return (cReturn)

//-----------------------------------------------------------------------------

function CloseProcess (cProcName)
local z
local aProcess := {}
local nProcess
local lReturn := .T.

cProcName := Upper(AllTrim(cProcName))

do case
case cProcName == NIL
   lReturn := .F.
case cProcName == ""
   lReturn := .F.
case HB_FNameName(cProcName) == ""
   lReturn := .F.
endcase
if !lReturn
   Return (NIL)
endif

if HB_FNameExt(cProcName) == ""
   cProcName := cProcName +".EXE"
endif

aProcess := _GetExePid()
nProcess := Len(aProcess)

for z = 1 to nProcess
   if Upper(AllTrim(aProcess[z, 1])) == cProcName
      _KillExec(aProcess[z, 2])
   endif
next z

Return (NIL)

//-----------------------------------------------------------------------------

static function _GetExePid ()
local oWMI
local oItem
local aFetch := {}
local cQuery := "SELECT * FROM Win32_Process"

oWMI := _WMIService()
for each oItem in oWMI:ExecQuery(cQuery)
   AAdd(aFetch, {oItem:Caption, HB_ValToStr(oItem:ProcessId)})
next

Return (aFetch)

//-----------------------------------------------------------------------------

static function _WMIService ()
local  oLocator
static oWMI

if oWMI == NIL
   oLocator := CreateObject("wbemScripting.SwbemLocator")
   oWMI     := oLocator:ConnectServer()
endif

Return (oWMI)

//-----------------------------------------------------------------------------

static function _KillExec (cPID, cResult)
local cCommand := "wmic process " + cPID + " delete"

Return (_SysCmd(cCommand, @cResult))

//-----------------------------------------------------------------------------

static function _SysCmd (cCommand, /*@*/ cResult)
local hProcess
local hStdOut
local hStderr
local nState
local nBytes
local cBuff := Space(1024)

hProcess := hb_processOpen(cCommand, NIL, @hStdOut, @hStdErr, .T.)

if hProcess != -1
   nState := hb_processValue(hProcess, .T.)
   while nState <> -1
      nBytes := FRead(hStdOut, @cBuff, 1024 /* cBuff length */)
      if nBytes == 0
         exit
      endif
      nState := hb_processValue( hProcess, .T. )
   end
   cBuff   := StrTran(cBuff, Chr(13))
   cBuff   := StrTran(cBuff, Chr(10))
   cResult := CharRem(" ", cBuff)

   hb_processClose(hProcess)
endif

Return (hProcess)

