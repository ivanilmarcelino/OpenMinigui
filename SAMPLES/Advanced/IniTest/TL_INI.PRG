
#include "MiniGUI.ch"

//-----------------------------------------------------------------------------

#define TAB           Chr(9)

//-----------------------------------------------------------------------------

static scIniFile   := ""
static scActiveIni := ""
static slVerbose   := NIL

//-----------------------------------------------------------------------------

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//
// IniCreate (<cIniFile>, <aArray>, lVerbose)
//
// Purpose:  Creates INI file
//
//  Params:  <cIniFile>  Name of the file to be created or edited
//                       -> If extension name not specified .INI will be assumed
//             <aArray>  The array containing initial Sections and Entries
//                       -> Cannot be blank or NIL
//           <lVerbose>  Display error message or not
//
//  Return:  Logical expression
//           -> .T. if file creation succeded
//
//   Usage:  aArray := {"[MP3]",      ;
//                      "DirInMP3=",  ;
//                      "DirOutMP3=", ;
//                      "",           ;
//                      "[CAT]",      ;
//                      "DirInCAT=",  ;
//                      "DirOutCAT="}
//
//           IniCreate("TEST", aArray)
//
// Program:  TL_INI.PRG
//
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

function IniCreate (cIniFile, aArray, lVerbose)
local cMessage := ""
local nBytes   := 0
local hIniFile
local z

lVerbose := iif(lVerbose == NIL, .F., lVerbose)

do case
case cIniFile == NIL
   cMessage := "INI file name not specified !"
case Empty(cIniFile)
   cMessage := "INI file name not specified !"
case HB_FNameName(cIniFile) == ""
   cMessage := "INI file name not specified !"
case aArray == NIL
   cMessage := "Data to write to INI file not specified !"
case Len(aArray) == 0
   cMessage := "No data to write to INI file !"
endcase
if !Empty(cMessage)
   if lVerbose
      MsgStop(cMessage, "IniCreate() Error")
   endif
   Return (.F.)
endif

if HB_FNameExt(cIniFile) == ""
   cIniFile := AllTrim(cIniFile) +".INI"
endif

hIniFile := FCreate(cIniFile)
if FError() <> 0
   if lVerbose
      MsgStop("Could not create "+ cIniFile +" => Error : "+ HB_NtoS(FError()), "IniCreate() Error")
   endif
   Return (.F.)
endif

for z = 1 to Len(aArray)
   nBytes += FWrite(hIniFile, aArray[z] + CRLF)
next z

do case
case nBytes == 0
   cMessage := "0 Bytes written to "+ cIniFile +" file !"
case FError() <> 0
   cMessage := "Error creating "+ cIniFile +" => Error : "+ HB_NtoS(FError())
endcase

FClose(hIniFile)

if !Empty(cMessage)
   if lVerbose
      MsgStop(cMessage, "IniCreate() Error")
   endif
   Return (.F.)
endif

Return (.T.)



// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//
// IniSet (cIniFile, lVerbose)
//
// Purpose:  Test availability of INI file and set active INI file STATIC variables
//
//  Params:  <cIniFile>  Sets the INI FILENAME to be tested and used
//                       -> If extension name not specified .INI is be assumed
//           <lVerbose>  Sets VERBOSE value for error messages display
//                       -> Default to .F.
//
//  Return:  Logic Expression
//            -> .T. if operation succeded
//       or
//           Array expression
//            -> {scActiveIni, slVerbose}
//
//   Notes:  Using this function sets two STATIC variables:
//           One for ini FILENAME and a second for VERBOSE parameters, that allows
//           using all other related functions like IniGetEntry(), IniAddEntry(),
//           etc. without having to specify the FILENAME and the VERBOSE values
//           until IniReset() is called.
//           If IniSet() is called with no parameters then it will return a
//           1 dimension array with the STATIC value of FILENAME and STATIC value
//           of VERBOSE parameter.
//
// Example:  IniSet("TEST", .T.)
//           IniAddEntry(, cSection, cEntry, cValue)
//           IniDelSection(, cSection)
//           IniGetEntry(, cSection, cEntry, cValue)
//           IniReset()
//
// Program:  TL_INI.PRG
//
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

function IniSet (cIniFile, lVerbose)
local cMessage := ""
local hIniFile := 0
local lReturn

if Empty(cIniFile) .and. Empty(lVerbose)
   MsgInfo("INI file name : "+ scActiveIni + CRLF + ;
           "Verbose set to : "+ iif(slVerbose == NIL, "NIL", iif(slVerbose, "TRUE", "FALSE")), "IniSet() Info")
   Return ({scActiveIni, slVerbose})
endif

lVerbose := iif(lVerbose == NIL, .F., lVerbose)

do case
case cIniFile == NIL
   cMessage := "INI file name not specified !"
case Empty(cIniFile)
   cMessage := "INI file name not specified !"
case HB_FNameName(cIniFile) == ""
   cMessage := "INI file name not specified !"
endcase

if HB_FNameExt(cIniFile) == ""
   cIniFile := AllTrim(cIniFile) +".INI"
endif

if !File(cIniFile)
   cMessage := "INI file not found !"
endif

if !Empty(cMessage)
   if lVerbose
      MsgStop(cMessage, "IniSet() Error")
   endif
   Return (.F.)
endif

IniReset()
if RAt("\", cIniFile) == 0
   cIniFile := HB_DirBase() + cIniFile
endif

hIniFile := FOpen(cIniFile, 66)  // FO_READWRITE (2) + FO_SHARED (64)
if FError() <> 0
   if lVerbose
      MsgStop("Could not open "+ cIniFile +" => Error : "+ HB_NtoS(FError()), "IniSet() Error")
   endif
   Return(.F.)
endif
FClose(hIniFile)

scActiveIni := cIniFile
slVerbose   := lVerbose

Return (.T.)



// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//
// IniReset ()
//
// Purpose:  Reset the currently active INI file STATIC variables
//
//  Params:  None
//
//  Return:  Nothing
//
//    Note:  Reset both FILENAME and VERBOSE STATIC variables
//
// Example:  if IniSet("TEST", .T.)
//              IniAddEntry(, cSection, cEntry, cValue)
//              IniDelSection(, cSection)
//              IniGetEntry(, cSection, cEntry, cValue)
//              IniReset()
//           endif
//
// Program:  TL_INI.PRG
//
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

function IniReset ()

scActiveIni := ""
slVerbose   := NIL

Return (NIL)



// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//
// IniAddBlank (cIniFile, lVerbose)
//
// Purpose:  Add blank line at end of INI file
//
//  Params:  <cIniFile>  Name of the file to be updated
//                       -> If extension name not specified .INI will be assumed
//           <lVerbose>  Display error message or not
//
//  Return:  Logical expression
//           -> .T. if action succeded
//
//    Note:  This function can be used as standalone function or in conjunction
//           with IniSet() and IniReset().
//           If use in standalone function <cIniFile> must be passed
//           For error meaning refer to xHarbour FError() codes
//
// Program:  TL_INI.PRG
//
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

function IniAddBlank (cIniFile, lVerbose)
local cMessage := ""
local hIniFile

lVerbose := iif(lVerbose == NIL, .F., lVerbose)
lVerbose := iif(slVerbose <> NIL, slVerbose, lVerbose)

do case
case Empty(scActiveIni) .and. cIniFile == NIL
   cMessage := "INI file name not specified !"
case Empty(scActiveIni) .and. Empty(cIniFile)
   cMessage := "INI file name not specified !"
case Empty(scActiveIni) .and. HB_FNameName(cIniFile) == ""
   cMessage := "INI file name not specified !"
endcase

if Empty(scActiveIni) .and. HB_FNameExt(cIniFile) == ""
   cIniFile := AllTrim(cIniFile) +".INI"
endif

if Empty(scActiveIni) .and. !File(cIniFile)
   cMessage := "INI file not found !"
endif

if !Empty(cMessage)
   if lVerbose
      MsgStop(cMessage, "IniAddBlank() Error")
   endif
   Return (.F.)
endif

if Empty(scActiveIni)
   if RAt("\", cIniFile) == 0
      cIniFile := HB_DirBase() + cIniFile
   endif
else
   cIniFile := scActiveIni
endif

hIniFile := FOpen(cIniFile, 1)
if FError() <> 0
   if lVerbose
      MsgStop("Cannot write to "+ cIniFile +" => Error: "+ HB_NtoS(FError()), "IniAddBlank() Error")
   endif
   Return (.F.)
endif
FSeek(hIniFile, 0, 2)   // Seek end of file
FWrite(hIniFile, CRLF)  // Write CRLF at end of file
FClose(hIniFile)

Return (.T.)



// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//
// IniAddEntry (cIniFile, cSection, cEntry, cValue, lVerbose)
//
// Purpose:  Stores the key value of a Section
//
//  Params:  <cIniFile>  Name of the file to be created or edited
//                       -> If extension name not specified .INI will be assumed
//           <cSection>  Name of Section to be created or edited
//             <cEntry>  Name of Entry to be created or edited
//             <cValue>  Value to be stored in Entry
//           <lVerbose>  Display error message or not
//
//    Note:  This function can be used as standalone function or in conjunction
//           with IniSet() and IniReset().
//
//  Return:  Logical expression
//           -> .T. if action succeded
//
// Program:  TL_INI.PRG
//
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

function IniAddEntry (cIniFile, cSection, cEntry, cValue, lVerbose)
local cMessage := ""
local lReturn

lVerbose := iif(lVerbose == NIL, .F., lVerbose)
lVerbose := iif(slVerbose <> NIL, slVerbose, lVerbose)

do case
case Empty(scActiveIni) .and. cIniFile == NIL
   cMessage := "INI file name not specified !"
case Empty(scActiveIni) .and. Empty(cIniFile)
   cMessage := "INI file name not specified !"
case Empty(scActiveIni) .and. HB_FNameName(cIniFile) == ""
   cMessage := "INI file name not specified !"
case cSection == NIL
   cMessage := "Section name not specified !"
case Empty(cSection)
   cMessage := "Section name is empty !"
case cEntry == NIL
   cMessage := "Entry name not specified !"
case Empty(cEntry)
   cMessage := "Entry name is empty !"
case cValue == NIL
   cMessage := "Entry value not specified !"
endcase

if Empty(scActiveIni) .and. HB_FNameExt(cIniFile) == ""
   cIniFile := AllTrim(cIniFile) +".INI"
endif

if Empty(scActiveIni) .and. !File(cIniFile)
   cMessage := "INI file not found !"
endif

if !Empty(cMessage)
   if lVerbose
      MsgStop(cMessage, "IniAddEntry() Error")
   endif
   Return (.F.)
endif

if Empty(scActiveIni)
   if RAt("\", cIniFile) == 0
      cIniFile := HB_DirBase() + cIniFile
   endif
else
   cIniFile := scActiveIni
endif

lReturn := WritePrivateProfileString(cSection, cEntry, _XToStr(cValue), cIniFile)

Return (lReturn)



// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//
// IniAddEntry (cIniFile, cSection, cEntry, xDefault, cType, lVerbose)
//
// Purpose:  Retreive the Value of an Entry in a Section
//
//  Params:  <cIniFile>  Name of the file to be searched
//                       -> If extension name not specified .INI will be assumed
//           <cSection>  Name of section to be searched
//             <cEntry>  Name of entry to be retreived
//           <xDefault>  Default value of entry if empty
//              <cType>  Convertion to make according to type of data
//                       -> If NIL cType will be auto determinate
//           <lVerbose>  Display error message or not
//
//    Note:  This function can be used as standalone function or in conjunction
//           with IniSet() and IniReset().
//
//  Return:  String, Numeric, Date, Logic, Array or NIL expression
//           -> The Entry or Default value pass in xDefault
//              depending on the format of the passed default value
//
// Program:  TL_INI.PRG
//
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

function IniGetEntry (cIniFile, cSection, cEntry, xDefault, cType, lVerbose)
local cMessage := ""
local xReturn

xDefault := iif(xDefault == NIL, NIL, xDefault)
lVerbose := iif(lVerbose == NIL, .F., lVerbose)
lVerbose := iif(slVerbose <> NIL, slVerbose, lVerbose)

do case
case Empty(scActiveIni) .and. cIniFile == NIL
   cMessage := "INI file name not specified !"
case Empty(scActiveIni) .and. Empty(cIniFile)
   cMessage := "INI file name not specified !"
case Empty(scActiveIni) .and. HB_FNameName(cIniFile) == ""
   cMessage := "INI file name not specified !"
case cSection == NIL
   cMessage := "Section name not specified !"
case Empty(cSection)
   cMessage := "Section name is empty !"
case cEntry == NIL
   cMessage := "Entry name not specified !"
case Empty(cEntry)
   cMessage := "Entry name is empty !"
case cType <> NIL
   cType := Upper(cType)
   if !cType $ "CMNDLAH"
      cMessage := "Data type «"+ cType +"» unknown !"
   endif
endcase

if Empty(scActiveIni) .and. HB_FNameExt(cIniFile) == ""
   cIniFile := AllTrim(cIniFile) +".INI"
endif

if Empty(scActiveIni) .and. !File(cIniFile)
   cMessage := "INI file not found !"
endif

if !Empty(cMessage)
   if lVerbose
      MsgStop(cMessage, "IniGetEntry() Error")
   endif
   Return (xReturn)
endif

if Empty(scActiveIni)
   if RAt("\", cIniFile) == 0
      cIniFile := HB_DirBase() + cIniFile
   endif
else
   cIniFile := scActiveIni
endif

do case
case !IniIsSection(cIniFile, cSection)
   cMessage := "Section «"+ cSection +"» not found !"
case !IniIsEntry(cIniFile, cSection, cEntry)
   cMessage := "Entry «"+ cEntry +"» not found !"
endcase

if !Empty(cMessage)
   if lVerbose
      MsgStop(cMessage, "IniGetEntry() Error")
   endif
   Return (xReturn)
endif

xReturn := GetPrivateProfileString(cSection, cEntry,, cIniFile)

if Empty(xReturn)
   xReturn := xDefault
endif
xReturn := _StrToX(xReturn, cType)

Return (xReturn)



// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//
// IniIsSection (cIniFile, cSection, lVerbose)
//
// Purpose:  Test if specific section name exist in INI file
//
//  Params:  <cIniFile>  The name of the file to be searched
//                       -> If extension name not specified .INI will be assumed
//           <cSection>  The section to be tested
//           <lVerbose>  Display error message or not
//
//    Note:  This function can be used as standalone function or in conjunction
//           with IniSet() and IniReset().
//
//  Return:  Logical expression
//           -> .T. if the specified Section exist
//
// Program:  TL_INI.PRG
//
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

function IniIsSection (cIniFile, cSection, lVerbose)
local z
local aSections := {}
local cMessage  := ""
local lReturn   := .F.

lVerbose := iif(lVerbose == NIL, .F., lVerbose)
lVerbose := iif(slVerbose <> NIL, slVerbose, lVerbose)

do case
case Empty(scActiveIni) .and. cIniFile == NIL
   cMessage := "INI file name not specified !"
case Empty(scActiveIni) .and. Empty(cIniFile)
   cMessage := "INI file name not specified !"
case Empty(scActiveIni) .and. HB_FNameName(cIniFile) == ""
   cMessage := "INI file name not specified !"
case cSection == NIL
   cMessage := "Section name not specified !"
case Empty(cSection)
   cMessage := "Section name is empty !"
endcase

if Empty(scActiveIni) .and. HB_FNameExt(cIniFile) == ""
   cIniFile := AllTrim(cIniFile) +".INI"
endif

if Empty(scActiveIni) .and. !File(cIniFile)
   cMessage := "INI file not found !"
endif

if !Empty(cMessage)
   if lVerbose
      MsgStop(cMessage, "IniIsSection() Error")
   endif
   Return (.F.)
endif

if Empty(scActiveIni)
   if RAt("\", cIniFile) == 0
      cIniFile := HB_DirBase() + cIniFile
   endif
else
   cIniFile := scActiveIni
endif

aSections := _GetSectionNames(cIniFile)

for z = 1 to Len(aSections)
   if Upper(aSections[z]) == Upper(cSection)
      lReturn := .T.
      exit
   endif
next z
if !lReturn .and. lVerbose
   MsgStop("Section <"+ cSection +"> not found !", "IniIsSection() Error")
   lReturn := .F.
endif

Return (lReturn)



// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//
// IniIsEntry (cIniFile, cSection, cEntry, lVerbose)
//
// Purpose:  Test if specific entry of a specific section exist in INI file
//
//  Params:  <cIniFile>  The name of the file to be searched
//                       -> If extension name not specified .INI will be assumed
//           <cSection>  The section to be searched
//             <cEntry>  The entry to be tested
//           <lVerbose>  Display error message or not
//
//    Note:  This function can be used as standalone function or in conjunction
//           with IniSet() and IniReset().
//
//  Return:  Logical expression
//           -> .T. if the specified Entry exist
//
// Program:  TL_INI.PRG
//
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

function IniIsEntry (cIniFile, cSection, cEntry, lVerbose)
local z
local nSection  := 0
local aSections := {}
local aEntries  := {}
local cMessage  := ""
local lReturn   := .F.

lVerbose := iif(lVerbose == NIL, .F., lVerbose)
lVerbose := iif(slVerbose <> NIL, slVerbose, lVerbose)

do case
case Empty(scActiveIni) .and. cIniFile == NIL
   cMessage := "INI file name not specified !"
case Empty(scActiveIni) .and. Empty(cIniFile)
   cMessage := "INI file name not specified !"
case Empty(scActiveIni) .and. HB_FNameName(cIniFile) == ""
   cMessage := "INI file name not specified !"
case cSection == NIL
   cMessage := "Section name not specified !"
case Empty(cSection)
   cMessage := "Section name is empty !"
case cEntry == NIL
   cMessage := "Entry name not specified !"
case Empty(cEntry)
   cMessage := "Entry name is empty !"
endcase

if Empty(scActiveIni) .and. HB_FNameExt(cIniFile) == ""
   cIniFile := AllTrim(cIniFile) +".INI"
endif

if Empty(scActiveIni) .and. !File(cIniFile)
   cMessage := "INI file not found !"
endif

if !Empty(cMessage)
   if lVerbose
      MsgStop(cMessage, "IniIsEntry() Error")
   endif
   Return (.F.)
endif

if Empty(scActiveIni)
   if RAt("\", cIniFile) == 0
      cIniFile := HB_DirBase() + cIniFile
   endif
else
   cIniFile := scActiveIni
endif

aSections := _GetSectionNames(cIniFile)

for z = 1 to Len(aSections)
   if Upper(aSections[z]) == Upper(cSection)
      nSection := z
      exit
   endif
next z
if nSection == 0 .and. lVerbose
   MsgStop("Section <"+ cSection +"> not found !", "IniIsEntry() Error")
endif

if nSection > 0
   aEntries := _GetSection(cSection, cIniFile)
   for z = 1 to Len(aEntries)
      if Upper(aEntries[z, 1]) == Upper(cEntry)
         lReturn := .T.
         exit
      endif
   next z
endif
if nSection > 0 .and. !lReturn .and. lVerbose
   MsgStop("Entry <"+ cEntry +"> not found !", "IniIsEntry() Error")
endif

Return (lReturn)



// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//
// IniDelSection (cIniFile, cSection, lVerbose)
//
// Purpose:  Delete a whole Section of the INI file
//
//  Params:  <cIniFile>  Name of the file to be processed
//                       -> If extension name not specified .INI will be assumed
//           <cSection>  Name of Section to be deleted
//           <lVerbose>  Display error message or not
//
//    Note:  This function can be used as standalone function or in conjunction
//           with IniSet() and IniReset().
//
//  Return:  Logical expression
//           -> .T. if action succeded
//
// Program:  TL_INI.PRG
//
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

function IniDelSection (cIniFile, cSection, lVerbose)
local cMessage := ""
local lReturn

lVerbose := iif(lVerbose == NIL, .F., lVerbose)
lVerbose := iif(slVerbose <> NIL, slVerbose, lVerbose)

do case
case Empty(scActiveIni) .and. cIniFile == NIL
   cMessage := "INI file name not specified !"
case Empty(scActiveIni) .and. Empty(cIniFile)
   cMessage := "INI file name not specified !"
case Empty(scActiveIni) .and. HB_FNameName(cIniFile) == ""
   cMessage := "INI file name not specified !"
case cSection == NIL
   cMessage := "Section name not specified !"
case Empty(cSection)
   cMessage := "Section name is empty !"
endcase

if Empty(scActiveIni) .and. HB_FNameExt(cIniFile) == ""
   cIniFile := AllTrim(cIniFile) +".INI"
endif

if Empty(scActiveIni) .and. !File(cIniFile)
   cMessage := "INI file not found !"
endif

if !Empty(cMessage)
   if lVerbose
      MsgStop(cMessage, "IniDelSection() Error")
   endif
   Return (.F.)
endif

if Empty(scActiveIni)
   if RAt("\", cIniFile) == 0
      cIniFile := HB_DirBase() + cIniFile
   endif
else
   cIniFile := scActiveIni
endif

lReturn := DelIniSection(cSection, cIniFile)

Return (lReturn)



// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//
// IniDelSection (cIniFile, cSection, cEntry, lVerbose)
//
// Purpose:  Delete an Entry from a Section of an INI file
//
//  Params:  <cIniFile>  Name of the file to be processed
//                       -> If extension name not specified .INI will be assumed
//           <cSection>  Name of Section to be deleted
//             <cEntry>  Name of Entry to be deleted
//           <lVerbose>  Display error message or not
//
//    Note:  This function can be used as standalone function or in conjunction
//           with IniSet() and IniReset().
//
//  Return:  Logical expression
//           -> .T. if action succeded
//
// Program:  TL_INI.PRG
//
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

function IniDelEntry (cIniFile, cSection, cEntry, lVerbose)
local cMessage := ""
local lReturn

lVerbose := iif(lVerbose == NIL, .F., lVerbose)
lVerbose := iif(slVerbose <> NIL, slVerbose, lVerbose)

do case
case Empty(scActiveIni) .and. cIniFile == NIL
   cMessage := "INI file name not specified !"
case Empty(scActiveIni) .and. Empty(cIniFile)
   cMessage := "INI file name not specified !"
case Empty(scActiveIni) .and. HB_FNameName(cIniFile) == ""
   cMessage := "INI file name not specified !"
case cSection == NIL
   cMessage := "Section name not specified !"
case Empty(cSection)
   cMessage := "Section name is empty !"
case cEntry == NIL
   cMessage := "Entry name not specified !"
case Empty(cEntry)
   cMessage := "Entry name is empty !"
endcase

if Empty(scActiveIni) .and. HB_FNameExt(cIniFile) == ""
   cIniFile := AllTrim(cIniFile) +".INI"
endif

if Empty(scActiveIni) .and. !File(cIniFile)
   cMessage := "INI file not found !"
endif

if !Empty(cMessage)
   if lVerbose
      MsgStop(cMessage, "IniDelEntry() Error")
   endif
   Return (.F.)
endif

if Empty(scActiveIni)
   if RAt("\", cIniFile) == 0
      cIniFile := HB_DirBase() + cIniFile
   endif
else
   cIniFile := scActiveIni
endif

lReturn := DelIniEntry(cSection, cEntry, cIniFile)

Return (lReturn)




// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//
// IniDelEntry (cIniFile, cSection, cEntry, lVerbose)
//
// Purpose:  Delete an Entry value from a Section of an INI file
//
//  Params:  <cIniFile>  Name of the file to be processed
//                       -> If extension name not specified .INI will be assumed
//           <cSection>  Name of Section to be deleted
//             <cEntry>  Name of Entry to be empty from it's value
//           <lVerbose>  Display error message or not
//
//    Note:  This function can be used as standalone function or in conjunction
//           with IniSet() and IniReset().
//
//  Return:  Logical expression
//           -> .T. if action succeded
//
// Program:  TL_INI.PRG
//
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

function IniDelValue (cIniFile, cSection, cEntry, lVerbose)
local cMessage := ""
local lReturn

lVerbose := iif(lVerbose == NIL, .F., lVerbose)
lVerbose := iif(slVerbose <> NIL, slVerbose, lVerbose)

do case
case Empty(scActiveIni) .and. cIniFile == NIL
   cMessage := "INI file name not specified !"
case Empty(scActiveIni) .and. Empty(cIniFile)
   cMessage := "INI file name not specified !"
case Empty(scActiveIni) .and. HB_FNameName(cIniFile) == ""
   cMessage := "INI file name not specified !"
case cSection == NIL
   cMessage := "Section name not specified !"
case Empty(cSection)
   cMessage := "Section name is empty !"
case cEntry == NIL
   cMessage := "Entry name not specified !"
case Empty(cEntry)
   cMessage := "Entry name is empty !"
endcase

if Empty(scActiveIni) .and. HB_FNameExt(cIniFile) == ""
   cIniFile := AllTrim(cIniFile) +".INI"
endif

if Empty(scActiveIni) .and. !File(cIniFile)
   cMessage := "INI file not found !"
endif

if !Empty(cMessage)
   if lVerbose
      MsgStop(cMessage, "IniDelEntry() Error")
   endif
   Return (.F.)
endif

if Empty(scActiveIni)
   if RAt("\", cIniFile) == 0
      cIniFile := HB_DirBase() + cIniFile
   endif
else
   cIniFile := scActiveIni
endif

lReturn := WritePrivateProfileString(cSection, cEntry, "", cIniFile)

Return (lReturn)



// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//
// IniZap (cIniFile, lVerbose)
//
// Purpose:  Empty an INI file
//
//  Params:  <cIniFile>  Name of the file to be created or edited
//                       -> If extension name not specified .INI will be assumed
//           <lVerbose>  Display error message or not
//
//  Return:  Logical expression
//            -> .T. if file creation succeded
//
//   Usage:
//           cIniFile := "TEST.INI"
//           IniCreate(cIniFile, aArray)
//           IniSet(cIniFile)
//           IniAddEntry(cIniFile, cSection, cEntry, cValue)
//           IniReset ()
//           IniZap(cIniFile)
//
// Program:  TL_INI.PRG
//
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

function IniZap (cIniFile, lVerbose)
local cMessage := ""
local hIniFile

lVerbose := iif(lVerbose == NIL, .F., lVerbose)
lVerbose := iif(slVerbose <> NIL, slVerbose, lVerbose)

if cIniFile == NIL .or. Empty(cIniFile)
   cMessage := "INI file name not specified !"
endif

if HB_FNameExt(cIniFile) == ""
   cIniFile := AllTrim(cIniFile) +".INI"
endif

if !File(cIniFile)
   cMessage := "INI file not found !"
endif

if !Empty(cMessage)
   if lVerbose
      MsgStop(cMessage, "IniZap() Error")
   endif
   Return (.F.)
endif

hIniFile := FCreate(cIniFile)
if hIniFile == -1
   MsgStop("Cannot empty .INI file !", "IniZap() Error")
   Return (.F.)
endif

FClose(hIniFile)

Return (.T.)



// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//
// IniGetSections (cIniFile, lVerbose)
//
// Purpose:  Read all section names into a one dimension Array
//
//  Params:  <cIniFile>  Name of the file to be processed
//                       -> If extension name not specified .INI will be assumed
//           <lVerbose>  Display error message or not
//
//    Note:  This function can be used as standalone function or in conjunction
//           with IniSet() and IniReset().
//
//  Return:  Array expression
//           -> Array containing all section names
//
// Program:  TL_INI.PRG
//
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

function IniGetSections (cIniFile, lVerbose)
local aSections := {}
local cMessage  := ""

lVerbose := iif(lVerbose == NIL, .F., lVerbose)
lVerbose := iif(slVerbose <> NIL, slVerbose, lVerbose)

do case
case Empty(scActiveIni) .and. cIniFile == NIL
   cMessage := "INI file name not specified !"
case Empty(scActiveIni) .and. Empty(cIniFile)
   cMessage := "INI file name not specified !"
endcase

if Empty(scActiveIni) .and. HB_FNameExt(cIniFile) == ""
   cIniFile := AllTrim(cIniFile) +".INI"
endif

if Empty(scActiveIni) .and. !File(cIniFile)
   cMessage := "INI file not found !"
endif

if !Empty(cMessage)
   if lVerbose
      MsgStop(cMessage, "IniGetSections() Error")
   endif
   Return (aSections)
endif

if Empty(scActiveIni)
   if RAt("\", cIniFile) == 0
      cIniFile := HB_DirBase() + cIniFile
   endif
else
   cIniFile := scActiveIni
endif

aSections := _GetSectionNames(cIniFile)

Return (aSections)



// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//
// IniGetEntries (cIniFile, cSection, lVerbose)
//
// Purpose:  Read all entries of a section to a two dimentional Array
//
//  Params:  <cIniFile>  Name of the file to be processed
//                       -> If extension name not specified .INI will be assumed
//           <cSection>  Name of the section to be processed
//           <lVerbose>  Display error message or not
//
//    Note:  This function can be used as standalone function or in conjunction
//           with IniSet() and IniReset().
//
//  Return:  Array expression
//           -> Array containing all the entries of a section
//
// Program:  TL_INI.PRG
//
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

function IniGetEntries (cIniFile, cSection, lVerbose)
local aEntries := {}
local cMessage := ""

lVerbose := iif(lVerbose == NIL, .F., lVerbose)
lVerbose := iif(slVerbose <> NIL, slVerbose, lVerbose)

do case
case Empty(scActiveIni) .and. cIniFile == NIL
   cMessage := "INI file name not specified !"
case Empty(scActiveIni) .and. Empty(cIniFile)
   cMessage := "INI file name not specified !"
case cSection == NIL
   cMessage := "Section name not specified !"
case Empty(cSection)
   cMessage := "Section name is empty !"
endcase

if Empty(scActiveIni) .and. HB_FNameExt(cIniFile) == ""
   cIniFile := AllTrim(cIniFile) +".INI"
endif

if Empty(scActiveIni) .and. !File(cIniFile)
   cMessage := "INI file not found !"
endif

if !Empty(cMessage)
   if lVerbose
      MsgStop(cMessage, "IniGetEntries() Error")
   endif
   Return (.F.)
endif

if Empty(scActiveIni)
   if RAt("\", cIniFile) == 0
      cIniFile := HB_DirBase() + cIniFile
   endif
else
   cIniFile := scActiveIni
endif

aEntries := _GetSection(cSection, cIniFile)

Return (aEntries)

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
// Static functions
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------

static function _XToStr (xValue)
local cType  := ValType(xValue)
local cValue := ""

do case
case cType == "C"
   cValue := xValue
case cType == "M"
   cValue := xValue
case cType == "N"
   cValue := AllTrim(Str(xValue))
case cType == "D"
   cValue := DToC(xValue)
case cType == "L"
   cValue := LtoC(xValue)
case cType == "A"
   cValue := _ArrToStr(xValue)
case cType == "H"
   cValue := _HashToStr(xValue)
otherwise
   cValue := "NIL"
endcase

Return (cValue)

//-----------------------------------------------------------------------------

static function _StrToX (cValue, cType)
local xValue

if (cType == NIL .or. Empty(cType))
   cType := _StrType(cValue)
endif

do case
case cType == "C"
   xValue := cValue
case cType == "M"
   xValue := cValue
case cType == "D"
   xValue := CToD(cValue)
case cType == "N"
   xValue := Val(cValue)
case cType == "L"
   xValue := _CtoL(cValue)
case cType == "A"
   xValue := _StrToArr(cValue)
case cType == "H"
   xValue := _StrToHash(cValue)
otherwise
   xValue := cValue
endcase

Return (xValue)

//-----------------------------------------------------------------------------

static function _ArrToStr (aArray)
local z, y
local nType  := 0
local nArrRow
local nArrCol
local nElement
local cElement
local cSubArray := ""
local cArray    := ""

nArrRow := Len(aArray)
if nArrRow > 0
   nType := 1  // 1D Array
endif
nArrCol := iif(ValType(aArray[1]) <> "A", 1, Len(aArray[1]))
if nArrCol > 1
   nType := 2  // 2D Array
endif

do case
case nType == 1
   nElement := Len(aArray)
   for z = 1 to nElement
      cElement := AllTrim(_XToStr(aArray[z]))
      if z == nElement
         cArray += cElement
      else
         cArray += cElement +","
      endif
   next

case nType == 2
   nElement := Len(aArray)
   for z = 1 to nElement
      cSubArray := ""
      for y = 1 to Len(aArray[z])
         cElement := AllTrim(_XToStr(aArray[z, y]))
         if y == Len(aArray[z])
            cSubArray += cElement
         else
            cSubArray += cElement +","
         endif
      next y
      if z == nElement
         cSubArray := "{"+ cSubArray +"}"
      else
         cSubArray := "{"+ cSubArray +"},"
      endif
      cArray += cSubArray
   next z

endcase
cArray := "{"+ cArray +"}"

Return (cArray)

//-----------------------------------------------------------------------------

static function _StrToArr (cValue)
local z, y
local cItem
local aArray := {}
local aTemp1
local aTemp2

do case
case Left(cValue, 2) == "{{" .and. Right(cValue, 2) == "}}"
   aTemp1 := HB_ATokens(cValue, "},")
   for z = 1 to Len(aTemp1)
      aTemp1[z] := CharRem("{}", aTemp1[z])
      aTemp2 := HB_ATokens(aTemp1[z], ",")
      for y = 1 to Len(aTemp2)
         aTemp2[y] := _StrToX(aTemp2[y])
      next y
      AAdd(aArray, aTemp2)
   next z

case Left(cValue, 1) == "{" .and. Right(cValue, 1) == "}"
   cValue := CharRem("{}", cValue)
   aArray := HB_ATokens(cValue, ",")

endcase

Return (aArray)

//-----------------------------------------------------------------------------

static function _HashToStr (aHash)
local z
local nElement
local cKey
local cVal
local cHash := ""

nElement := Len(aHash)
for z = 1 to nElement
   cKey := AllTrim(_XToStr(HB_HKeyAt(aHash, z)))
   cVal := AllTrim(_XToStr(HB_HValueAt(aHash, z)))
   if z == nElement
      cHash += (cKey +"=>"+ cVal)
   else
      cHash += (cKey +"=>"+ cVal +",")
   endif
next z
cHash := "{"+ cHash +"}"

Return (cHash)

//-----------------------------------------------------------------------------

static function _StrToHash (cValue)
local z
local xHKey
local xHVal
local aArray
local aHash := HB_Hash()

cValue := AllTrim(CharRem("{}", cValue))
aArray := HB_ATokens(cValue, ",")

for z = 1 to Len(aArray)
   xHKey := _StrToX(Token(aArray[z], "=>", 1))
   xHVal := _StrToX(Token(aArray[z], "=>", 2))
   HB_HSet(aHash, xHKey, xHVal)
next z

Return (aHash)

//-----------------------------------------------------------------------------

static function _StrType (cValue)
local z
local nLen

if cValue == NIL
   Return (NIL)
endif

cValue := AllTrim(cValue)

// Is it a Numeric
if IsDigit(cValue)
   nLen := Len(cValue)
   for z = 1 to nLen
      if !SubStr(cValue, z, 1) $ "0123456789.,"
         exit
      endif
   next z
   if z > nLen
      Return ("N")
   endif
endif

// Is it a Date
if (IsDigit(cValue) .and. NumAt("/", cValue) == 2)
   Return ("D")
endif

// Is it a Logic
do case
case Upper(cValue) $ "TF"          // (T/F)
   Return ("L")
case Upper(cValue) $ "YN"          // (Y/N)
   Return ("L")
case Upper(cValue) $ "YESNO"       // (YES/NO)
   Return ("L")
case Upper(cValue) $ "TRUEFALSE"   // (TRUE/FALSE)
   Return ("L")
endcase

// Is it a Hash
if (At("{", cValue) > 0 .and. At("}", cValue) > 0 .and. At("=>", cValue) > 0)
   Return ("H")
endif

// Is it a Array
if (At("{", cValue) > 0 .and. At("}", cValue) > 0)
   Return ("A")
endif

// Is it a String
if HB_IsString(cValue)
   Return ("C")
endif

Return ("C")

//-----------------------------------------------------------------------------

static function _CtoL (cValue)
local lReturn

cValue := Upper(cValue)
do case
case cValue $ "TYYESTRUE"    // (T/Y/YES/TRUE)
   lReturn := .T.
case cValue $ "FNNOFALSE"    // (F/N/NO/FALSE)
   lReturn := .F.
otherwise
   lReturn := NIL
endcase

Return (lReturn)

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
// META FUNCTION
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------

procedure IniEditor (cFile)

if cFile <> NIL
   if HB_FNameExt(cFile) == ""
      cFile := AllTrim(cFile) +".INI"
   endif
endif

define window oWndEdit                  ;
       at 0, 0                          ;
       width 1160 height 600            ;
       title "INI File Editor Ver. 2.0" ;
       font "MS Sans Serif" size 9      ;
       modal nosize                     ;
       on init {|| _OnInit(cFile)}      ;
       on interactiveclose {|| _Quit()}

   define toolbarex oTlbMain buttonsize 72, 72 bold flat border

      button oBtnExit picture "EXIT_48" ;
      caption "Quit"                    ;
      tooltip "Quit Application"        ;
      action  {|| _Quit()}              ;
      separator

      button oBtnOpen picture "NEW_48"  ;
      caption "New"                     ;
      tooltip "Create new INI file"     ;
      action  {|| _NewIni()}            ;
      separator

      button oBtnOpen picture "OPEN_48" ;
      caption "Open"                    ;
      tooltip "Open INI file"           ;
      action  {|| _OpenIni()}

      button oBtnSort picture "SORT_48" ;
      caption "Sort"                    ;
      tooltip "Sort Tabs of INI File"   ;
      action  {|| _TabSort()}

      button oBtnJump picture "GOTO_48" ;
      caption "Tab Jump"                ;
      tooltip "Jump to selected Tab"    ;
      action  {|| _TabJump()}           ;
      separator

      button oBtnAbout picture "INFO_48" ;
      caption "About"                    ;
      tooltip "See credentials"          ;
      action  {|| _MsgAbout()}           ;
      separator

   end toolbar

   @  95,   10 label oLblCurrFile width 300 height 15 value "Current File: "+ scIniFile bold
   @  95,  925 label oLblHelp width 260 height 15 value "Right-Click to display context menu" bold

   @ 145,   10 label oLblSection width 300 height 15 value "Section:" bold

   define tab oTabSection      ;
          at 115, 10           ;
          width 1135 height 25 ;
          hottrack notabstop   ;
          on change {|| _OnTabChange()}
   end tab

   @ 165,  10 grid oGrdKeys                      ;
              width 1135 height 400              ;
              widths {1125}                      ;
              nolines noheaders                  ;
              on gotfocus {|| _SetContextMenu()} ;
              on change   {|| _SetContextMenu()} ;
              on dblclick {|| _EdtValue()}

end window

define context menu control oGrdKeys of oWndEdit
   item "Add Section"        + Chr(9) +" Ctrl-Ins"  action {|| _AddSection()} image "ADD_20" name oMnuAddSection
   item "Delete Section"     + Chr(9) +" Ctrl-Del"  action {|| _DelSection()} image "DEL_20" name oMnuDelSection
   separator
   item "Add Entry"          + Chr(9) +"  Alt-Ins"  action {|| _AddEntry()}   image "ADD_20" name oMnuAddValue
   item "Edit Entry"         + Chr(9) +"  Alt-Ent"  action {|| _EdtValue()}   image "EDT_20" name oMnuEdtValue
   item "Delete Entry"       + Chr(9) +"  Alt-Del"  action {|| _DelEntry()}   image "DEL_20" name oMnuDelValue
end menu

_DefineHotKey("oWndEdit", MOD_CONTROL, VK_INSERT, {|| _AddSection()})
_DefineHotKey("oWndEdit", MOD_CONTROL, VK_DELETE, {|| _DelSection()})

_DefineHotKey("oWndEdit",, VK_INSERT, {|| _AddEntry()})
_DefineHotKey("oWndEdit",, VK_RETURN, {|| _EdtValue()})
_DefineHotKey("oWndEdit",, VK_DELETE, {|| _DelEntry()})

_DefineHotKey("oWndEdit", MOD_ALT, VK_N, {|| _NewIni()})
_DefineHotKey("oWndEdit", MOD_ALT, VK_O, {|| _OpenIni()})
_DefineHotKey("oWndEdit", MOD_ALT, VK_I, {|| _MsgAbout()})
_DefineHotKey("oWndEdit", MOD_ALT, VK_S, {|| _TabSort()})
_DefineHotKey("oWndEdit", MOD_ALT, VK_J, {|| _TabJump()})
_DefineHotKey("oWndEdit", MOD_ALT, VK_X, {|| _Quit()})

oWndEdit.Center()
oWndEdit.Activate()

Return

//-----------------------------------------------------------------------------

static function _OnInit (cFile)

if ValType(cFile) == "C" .and. File(cFile)
   _OpenIni(cFile)
else
   oWndEdit.oBtnSort.Enabled := .F.
   oWndEdit.oBtnJump.Enabled := .F.
endif

Return (NIL)

//-----------------------------------------------------------------------------

static function _Quit ()

scIniFile := ""
oWndEdit.Release()

Return (NIL)

//-----------------------------------------------------------------------------

static function _NewIni ()
local cIniFile := ""
local hIniFile
local lCancel
local z

cIniFile := InputBox("Enter file name : ", "New File", cIniFile,,,, @lCancel)
do case
case lCancel
   Return (NIL)
case Empty(cIniFile)
   Return (NIL)
case HB_FNameExt(cIniFile) == ""
   cIniFile := Upper(AllTrim(cIniFile)) +".INI"
endcase
cIniFile := HB_DirBase() + AllTrim(cIniFile)

if !Empty(scIniFile)
   for z = 1 to Len(_GetSectionNames(scIniFile))
      oWndEdit.oTabSection.DeletePage(1)
   next
endif
scIniFile := cIniFile
hIniFile  := FCreate(scIniFile)
FClose(hIniFile)
oWndEdit.oLblCurrFile.Value := "Current File: "+ scIniFile
oWndEdit.oLblSection.Value  := "Current Section:"
oWndEdit.oGrdKeys.DeleteAllItems()
_SetContextMenu()

Return (NIL)

//-----------------------------------------------------------------------------

static function _OpenIni (cOpenFile)
local z
local aSections := {}
local aEntries  := {}

default cOpenFile := ""
if Empty(cOpenFile)
   cOpenFile := GetFile({{"Ini Files", "*.ini"}, {"EDPM Files", "*.edp"}}, "INI File Selection")
endif

if !Empty(cOpenFile)
   if !Empty(scIniFile)
      for z = 1 to Len(IniGetSections(scIniFile))
         oWndEdit.oTabSection.DeletePage(1)
      next
   endif

   oWndEdit.oGrdKeys.DeleteAllItems()
   oWndEdit.oLblCurrFile.Value := "Current File:"
   oWndEdit.oLblSection.Value  := "Current Section:"
   scIniFile                   := iif(RAt("\", cOpenFile) == 0, HB_DirBase() + cOpenFile, cOpenFile)
   aSections                   := IniGetSections(scIniFile)

   if Len(aSections) > 0
      for z = 1 to Len(aSections)
         oWndEdit.oTabSection.AddPage(z, aSections[z])
      next
      aEntries := IniGetEntries(scIniFile, aSections[1])
      if Len(aEntries) > 0
         oWndEdit.oGrdKeys.DisableUpdate()
         for z = 1 to Len(aEntries)
            oWndEdit.oGrdKeys.AddItem({aEntries[z, 1] +"="+ aEntries[z, 2]})
         next
         oWndEdit.oGrdKeys.EnableUpdate()
      endif

      oWndEdit.oLblCurrFile.Value := "Current File: "+ scIniFile
      oWndEdit.oLblSection.Value  := "Current Section: ["+ aSections[1] +"]"
      oWndEdit.oTabSection.Value  := 1
      oWndEdit.oBtnSort.Enabled   := .T.
      oWndEdit.oBtnJump.Enabled   := .T.
   else
      scIniFile := ""
      MsgStop("Failed to retrieve a Tab Section", "Error",, .F.)
   endif
endif
_SetContextMenu()

Return (NIL)

//-----------------------------------------------------------------------------

static function _SetContextMenu ()
local lEnabled := oWndEdit.oGrdKeys.Value > 0

if Empty(scIniFile)
   set context menu control oGrdKeys of oWndEdit OFF
   oWndEdit.oLblHelp.Visible := .F.
else
   oWndEdit.oLblHelp.Visible := .T.
   set context menu control oGrdKeys of oWndEdit ON
   oWndEdit.oMnuEdtValue.Enabled := lEnabled
   oWndEdit.oMnuDelValue.Enabled := lEnabled
endif

if oWndEdit.oTabSection.ItemCount < 2
   oWndEdit.oBtnSort.Enabled := .F.
   oWndEdit.oBtnJump.Enabled := .F.
else
   oWndEdit.oBtnSort.Enabled := .T.
   oWndEdit.oBtnJump.Enabled := .T.
endif

Return (NIL)

//-----------------------------------------------------------------------------

static function _AddSection ()

define window oDlgAddSect           ;
       at 0, 0 width 330 height 400 ;
       title  "Add Section"         ;
       font "MS Sans Serif" size 9  ;
       modal nosize

   @  10,  10 label oLblSection value "Section Name:" autosize
   @  10,  90 textbox oGetSection value "" ;
              width 220 height 20          ;
              uppercase

   @  35,  10 label oLblValue width 260 height 20 value "Entry and Value in Format: 'Entry=Value'"
   @  60,  08 editbox oGetValues value "" ;
              width 300 height 235

   @ 305,  20 buttonex oBtnCancel   ;
              width 120  height 40  ;
              caption "Cancel"      ;
              font "TAHOMA" size 10 ;
              picture "CANCEL_24"   ;
              notabstop             ;
              action {|| oDlgAddSect.Release}

   @ 305, 180 buttonex oBtnAccept                                                                   ;
              width 120  height 40                                                                  ;
              caption "Save"                                                                        ;
              font "TAHOMA" size 10                                                                 ;
              picture "ACCEPT_24"                                                                   ;
              notabstop                                                                             ;
              action {|| _SaveSection(oDlgAddSect.oGetSection.Value, oDlgAddSect.oGetValues.Value), ;
                         oDlgAddSect.Release()}

end window

_DefineHotKey("oDlgAddSect",,             VK_ESCAPE, {|| oDlgAddSect.oBtnCancel.OnClick()})
_DefineHotKey("oDlgAddSect", MOD_CONTROL, VK_RETURN, {|| oDlgAddSect.oBtnAccept.OnClick()})

oDlgAddSect.Center()
oDlgAddSect.Activate()

Return (NIL)

//-----------------------------------------------------------------------------

static function _SaveSection (cSection, cLines)
local z
local nItems
local cLine
local cEntry
local cValue

if !Empty(cSection)
   nItems := MLCount(cLines)
   if nItems > 0
      IniAddBlank(scIniFile)
   endif
   for z = 1 to nItems
      cLine := MemoLine(cLines,, z)
      if At("=", cLine) > 0
         cEntry := Token(cLine, "=", 1)
         cValue := Token(cLine, "=", 2)
         if cValue == NIL
            cValue := ""
         endif
         IniAddEntry(scIniFile, cSection, cEntry, cValue, .T.)
      endif
   next z
   oWndEdit.oTabSection.AddPage(Len(IniGetSections(scIniFile, .T.)), cSection)
   _OnTabChange()
endif

Return (NIL)

//-----------------------------------------------------------------------------

static function _DelSection ()
local nSection := oWndEdit.oTabSection.Value
local cSection := oWndEdit.oTabSection.Header(nSection)

if nSection > 0
   if MsgYesNo("Do you really want to delete the entire section ?", cSection)
      if IniDelSection(scIniFile, cSection)
         oWndEdit.oTabSection.DeletePage(nSection)
         oWndEdit.oTabSection.Value := iif(nSection > 1, nSection -1, 1)
      endif
      _OnTabChange()
      _SetContextMenu()
   endif
endif

Return (NIL)

//-----------------------------------------------------------------------------

static function _AddEntry ()
local cSection := oWndEdit.oTabSection.Header(oWndEdit.oTabSection.Value)

define window oDlgAddEntry                                  ;
       at 0, 0 width 375 height 150                         ;
       title  "Add Entry and Value to Section : "+ cSection ;
       font "MS Sans Serif" size 9                          ;
       modal nosize

   @  10,  10 label oLblEntry width 80 height 20 value "New Entry :" rightalign
   @  10,  95 textbox oTxtEntry width 250 height 20 value ""

   @  35,  10 label oLblValue width 80 height 20 value "Entry Value :" rightalign
   @  35,  95 textbox oTxtValue width 250 height 20 value ""

   @  70,  95 buttonex oBtnCancel   ;
              width 120 height 40   ;
              caption "Cancel"      ;
              font "TAHOMA" size 10 ;
              picture "CANCEL_24"   ;
              notabstop             ;
              action {|| oDlgAddEntry.Release()}

   @  70, 225 buttonex oBtnAccept                                                                          ;
              width 120 height 40                                                                          ;
              caption "Save"                                                                               ;
              font "TAHOMA" size 10                                                                        ;
              picture "ACCEPT_24"                                                                          ;
              notabstop                                                                                    ;
              action {|| _SaveEntry(cSection, oDlgAddEntry.oTxtEntry.Value, oDlgAddEntry.oTxtValue.Value), ;
                         oDlgAddEntry.Release()}
end window

_DefineHotKey("oDlgAddEntry",,             VK_ESCAPE, {|| oDlgAddEntry.oBtnCancel.OnClick()})
_DefineHotKey("oDlgAddEntry", MOD_CONTROL, VK_RETURN, {|| oDlgAddEntry.oBtnAccept.OnClick()})

oDlgAddEntry.Center()
oDlgAddEntry.Activate()

Return (NIL)

//-----------------------------------------------------------------------------

static function _SaveEntry (cSection, cEntry, cValue)

if Empty(cEntry)
   Return (NIL)
endif

IniAddEntry(scIniFile, cSection, cEntry, cValue)

oWndEdit.oGrdKeys.AddItem({cEntry +"="+ cValue})
oWndEdit.oGrdKeys.Value := oWndEdit.oGrdKeys.ItemCount

Return (NIL)

//-----------------------------------------------------------------------------

static function _DelEntry ()
local cSection := oWndEdit.oTabSection.Header(oWndEdit.oTabSection.Value)
local nLine    := oWndEdit.oGrdKeys.Value
local cItem    := oWndEdit.oGrdKeys.Cell(nLine, 1)
local cEntry   := Token(cItem, "=", 1)
local lResult

if nLine > 0
   if MsgYesNo("Are you sure you want to delete the Entry and Value ?", "INI File Editor")
      lResult := IniDelEntry(scIniFile, cSection, cEntry)
      oWndEdit.oGrdKeys.DeleteItem(nLine)
      oWndEdit.oGrdKeys.Value := iif(nLine > 1, nLine -1, 1)
   endif
endif

Return (NIL)

//-----------------------------------------------------------------------------

static function _EdtValue ()
local nLine    := oWndEdit.oGrdKeys.Value
local cSection := oWndEdit.oTabSection.Header(oWndEdit.oTabSection.Value)
local cItem    := oWndEdit.oGrdKeys.Cell(nLine, 1)
local cEntry   := Token(cItem, "=", 1)
local cValue   := Token(cItem, "=", 2)

if nLine > 0
   define window oDlgEdtValue                     ;
          at 0, 0 width 375 height 150            ;
          title  "Edit Value of Entry : "+ cEntry ;
          font "MS Sans Serif" size 9             ;
          modal nosize

      @  20,  10 label oLblValue width 80 height 20 value "Entry Value :" rightalign
      @  20,  95 textbox oTxtValue width 250 height 20 value cValue

      @  70,  95 buttonex oBtnCancel   ;
                 width 120 height 40   ;
                 caption "Cancel"      ;
                 font "TAHOMA" size 10 ;
                 picture "CANCEL_24"   ;
                 notabstop             ;
                 action {|| oDlgEdtValue.Release()}

      @  70, 225 buttonex oBtnAccept                                                    ;
                 width 120 height 40                                                    ;
                 caption "Save"                                                         ;
                 font "TAHOMA" size 10                                                  ;
                 picture "ACCEPT_24"                                                    ;
                 notabstop                                                              ;
                 action {|| _SaveValue(cSection, cEntry, oDlgEdtValue.oTxtValue.Value), ;
                            oDlgEdtValue.Release()}

   end window

   _DefineHotKey("oDlgEdtValue",,             VK_ESCAPE, {|| oDlgEdtValue.oBtnCancel.OnClick()})
   _DefineHotKey("oDlgEdtValue", MOD_CONTROL, VK_RETURN, {|| oDlgEdtValue.oBtnAccept.OnClick()})

   oDlgEdtValue.Center()
   oDlgEdtValue.Activate()

endif

Return (NIL)

//-----------------------------------------------------------------------------

static function _SaveValue (cSection, cEntry, cValue)

IniAddEntry(scIniFile, cSection, cEntry, cValue)
oWndEdit.oGrdKeys.Cell(oWndEdit.oGrdKeys.Value, 1) := cEntry +"="+ cValue

Return (NIL)

//-----------------------------------------------------------------------------

static function _OnTabChange ()
local z
local aEntries  := {}
local nSection  := oWndEdit.oTabSection.Value
local cSection

if nSection > 0
   cSection := oWndEdit.oTabSection.Header(nSection)
   aEntries := IniGetEntries(scIniFile, cSection)
   oWndEdit.oGrdKeys.DeleteAllItems()
   if Len(aEntries) > 0
      oWndEdit.oGrdKeys.DisableUpdate()
      for z = 1 to Len(aEntries)
         oWndEdit.oGrdKeys.AddItem({aEntries[z, 1] +"="+ aEntries[z, 2]})
      next
      oWndEdit.oGrdKeys.EnableUpdate()
   endif
   oWndEdit.oLblSection.Value := "Current Section: ["+ cSection +"]"
else
   oWndEdit.oGrdKeys.DeleteAllItems()
   oWndEdit.oLblSection.Value := "Current Section:"
endif

Return (NIL)

//-----------------------------------------------------------------------------

static function _TabSort ()
local cSection  := oWndEdit.oTabSection.Header(oWndEdit.oTabSection.Value)
local aSections := ASort(IniGetSections(scIniFile),,, {|a,b| Upper(a) < Upper(b)})

AEval(aSections, {|e,z| oWndEdit.oTabSection.Header(z) := e})
oWndEdit.oTabSection.Value := AScan(aSections, {|e,z| oWndEdit.oTabSection.Header(z) == cSection})

Return (NIL)

//-----------------------------------------------------------------------------

static function _TabJump ()
local cSection
local aSections := aSort(IniGetSections(scIniFile),,, {|a,b| Upper(a) < Upper(b)})

define window oDlgJump              ;
       at 0, 0 width 240 height 325 ;
       title  "Tab Jumper"          ;
       font "MS Sans Serif" size 9  ;
       modal nosize

   @  10,  10 listbox oBrwSection   ;
              width 230 height 320  ;
              items aSections       ;
              value 0               ;
              on dblclick {|| cSection := oDlgJump.oBrwSection.Item(oDlgJump.oBrwSection.Value),                                  ;
                              oWndEdit.oTabSection.Value := AScan(aSections, {|e,z| oWndEdit.oTabSection.Header(z) == cSection}), ;
                              _OnTabChange(), oDlgJump.Release()}

end window

_DefineHotKey("oDlgJump", 0, VK_ESCAPE, {|| oDlgJump.Release()})

oDlgJump.Center()
oDlgJump.Activate()

Return (NIL)

//-----------------------------------------------------------------------------

static function _MsgAbout ()

Return (MsgInfo("INI File Editor Version 2.0"+ CRLF + "Based on Grigory Filatov «INIFileEditor»"+ CRLF + ;
        HB_Compiler() +", "+ Version()))

