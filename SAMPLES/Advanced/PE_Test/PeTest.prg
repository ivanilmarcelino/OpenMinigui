/******************************************************************************
   Filename        : PeTest.prg

   Created         : 1 September 2019 (12:00:20)
   Created by      : Pierpaolo Martinello

   Comments        : Free for all purposes
                     This Program is intended for discover the architectural type
                     of executables or dll.
                     You can drag the file to check directly over PeTest.exe, or
                     type it as the PeTest.exe parameter.

*******************************************************************************/

#include "Minigui.ch"
#include "Fileio.ch"

#define IMAGE_FILE_MACHINE_I386  0x14c
#define IMAGE_FILE_MACHINE_IA64  0x200
#define IMAGE_FILE_MACHINE_AMD64 0x8664

PROCEDURE Main(ArgIn)
LOCAL cBuffer, nHandle, nBytes, nPointer
LOCAL cMachineType, hHex
LOCAL cText := "It has a UNKNOWN architecture."
Local cTTExt, nDll

   IF !FILE(ArgIn)
      ArgIn := GetFile({{"Pe Files","*.dll;*.exe"}} ,'OpenFiles(s)',GetcurrentFolder(),.F.,.T. )
      if Empty(ArgIn) // Do not require any action and terminate the prg
         Quit
      Endif
   ENDIF

   nHandle  := FOpen( ArgIn , FO_READ )
   cBuffer  := Space(1024) // some executables have the PE string allocated over 512 bits
   nBytes   := FRead(nHandle, @cBuffer, 1024)
   FClose( nHandle )

   nPointer := AT( "PE"+CHR(0)+CHR(0), cBuffer )

   IF nPointer > 0

      // check if file is a true dll or exe
      nDll := Bin2l( SUBSTR(cBuffer,nPointer+23,1) )

      if nDll > 31 .and. nDll < 36
         cTTExt := "as a dll."
      Else
         cTTExt := "as exe."
      Endif

      cMachineType := SUBSTR(cBuffer,nPointer+4,2)

      hHex := Bin2L (cMachineType)

      DO CASE
         CASE hHex = IMAGE_FILE_MACHINE_I386
              cText := "With a 32 Bit architecture "+cTTExt

         CASE hHex = IMAGE_FILE_MACHINE_IA64 .OR. ;
              hHex = IMAGE_FILE_MACHINE_AMD64
              cText := "With a 64 Bit architecture "+cTTExt

      ENDCASE
      MsgInfo(cText,Space(3)+cFilenopath(ArgIn)+ " is build" )
   Else
      MsgStop(cText,Space(3)+cFilenopath(ArgIn))
   Endif

RETURN