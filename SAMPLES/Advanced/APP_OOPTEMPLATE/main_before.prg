/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * (c) 2023 Verchenko Andrey <verchenkoag@gmail.com>
*/

#include "minigui.ch"
#include "metrocolor.ch"

///////////////////////////////////////////////////////////////////
// Перед запуском основной формы / Before starting the main form
// Все PUBLIC переменные
FUNCTION BeforeStartingMain()
   LOCAL aDim, aTmp, nI, cTemp
   LOCAL cPathTemo := GetUserTempFolder() + "\"
   LOCAL cPath     := GetStartupFolder()  + "\"
   LOCAL cPathExe  := GetStartupFolder()  + "\"

   ? SPACE(3)+ProcNL(), "Перед запуском основной формы / Before starting the main form" ; ?

   // в качестве примера / as an example
   aDim := Directory( cPath + "*.prg" )
   aTmp := Directory( cPath + "*.ch"  )
   aMerge( aDim, aTmp )

   M->aPubListFrom := {}
   IF Len( aDim ) > 0
      FOR nI := 1 TO Len( aDim )
         AAdd( M->aPubListFrom, aDim[ nI, 1 ] )
      NEXT
   ENDIF
   ? SPACE(3) + "M->aPubListFrom=",M->aPubListFrom,HB_ValToExp(M->aPubListFrom)
   aDim := {}
   aTmp := {}

   aDim := Directory( cPathExe + "*.exe" )
   aMerge( aDim, aTmp )
   aTmp := Directory( cPathExe + "*.hbp" )
   aMerge( aDim, aTmp )
   aTmp := Directory( cPathExe + "*.rc"  )
   aMerge( aDim, aTmp )
   aTmp := Directory( cPathExe + "*.lib" )
   aMerge( aDim, aTmp )

   M->aPubListTo := {}
   IF Len( aDim ) > 0
      FOR nI := 1 TO Len( aDim )
         AAdd( M->aPubListTo, aDim[ nI, 1 ] )
      NEXT
   ENDIF

   ? SPACE(3) + "M->aPubDirTo=",M->aPubListTo,HB_ValToExp(M->aPubListTo) ; ?

   // в качестве примера
   // ВНИМАНИЕ ! Желательно ограничиться использование PUBLIC переменных в программе,
   // лучше перейти на App.Cargo:cSample1
   // as an example
   // ATTENTION ! It is desirable to limit the use of PUBLIC variables in the program,
   // better go to App.Cargo:cSample1
   FOR nI := 1 TO 100
       cTemp := "cPubSample" + HB_NtoS(nI)
       PUBLIC &cTemp
       M->&cTemp := cTemp
   NEXT

RETURN Nil


///////////////////////////////////////////////////////////////////
// Перед запуском основной формы / Before starting the main form
FUNCTION BeforeStartingDisplay()
   LOCAL cFileCnf := ChangeFileExt( App.ExeName, ".display" )
   LOCAL aDim, cStr, cTek

   IF FILE(cFileCnf)

      cStr := ALLTRIM( hb_MemoRead(cFileCnf) )
      IF LEN(cStr) == 0
        aDim := {}
      ELSE
         IF AT( "{", cStr ) > 0 .AND. AT( "}", cStr ) > 0
            aDim := &cStr
         ELSE
            aDim := {}
         ENDIF
      ENDIF

      IF LEN(aDim) > 0
         cStr := HB_NtoS(aDim[1]) + "x" + HB_NtoS(aDim[2])
         cTek := HB_NtoS(System.DesktopWidth) + "x" + HB_NtoS(System.DesktopHeight)
         IF aDim[1] > System.DesktopWidth
            aDim := { System.DesktopWidth , System.DesktopHeight }
            cStr := cTek
         ENDIF
         // разрешение программы и проверка экранных форм
         App.Cargo:aDisplayMode := aDim
         App.Cargo:cDisplayMode := cStr
      ENDIF

   ENDIF

RETURN Nil

