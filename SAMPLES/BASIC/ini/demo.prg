#include "minigui.ch"

FUNCTION main()

   LOCAL cIniFile := GetStartupFolder() + '\demo.ini'
   LOCAL aRet, cBeg, cEnd

   SetIniValue(cIniFile)

   aRet := GetIniValue(cIniFile)

   AEVAL( aRet, { |x| MsgInfo( x, 'Type is ' + ValType(x) ) } )

   BEGIN INI FILE cIniFile
      GET BEGIN COMMENT TO cBeg
      GET END COMMENT TO cEnd
   END INI

   MsgDebug( cBeg, cEnd )

RETURN NIL


PROCEDURE SetIniValue( cIni )

   BEGIN INI FILE cIni
      SET SECTION 'Project' ENTRY 'Name' TO 'My Project'
      SET BEGIN COMMENT TO "it's a top line."
      SET END COMMENT TO Time() + " it's a bottom line."
      SET BEGIN COMMENT TO Time() + " it's a first line."
   END INI

RETURN


FUNCTION GetIniValue( cIni )

   LOCAL cName, nVers

   BEGIN INI FILE (cIni)
      GET cName SECTION 'Project' ENTRY 'Name' DEFAULT ''
      GET nVers SECTION 'Project' ENTRY 'Vers' DEFAULT 1.01
   END INI

RETURN { cName, nVers }
