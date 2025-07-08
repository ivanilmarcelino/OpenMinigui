#include <minigui.ch>

FUNCTION Main

   LOAD WINDOW readsection
   readsection.Title := "Reading of Exe file internal sections - Test Program <arcangelo.molinaro@fastwebnet.it>"
   readsection.Center
   readsection.Activate

RETURN NIL

FUNCTION leggi_Sezioni

   LOCAL cFileName, aReturn, nLen, i
   LOCAL cVirtualAddress, cVirtualSize, cSectionName
   LOCAL cPRawData, cSRawData, cCharacter, nChar

   cFilename := HBChoose_File()
   IF Empty( cFilename )
      RETURN NIL
   ENDIF
   aReturn := HBR_DCSECT( cFilename )

   readsection.Title := "Sections for " + cFilename + " file."
   readsection.Lbl_1.VALUE := "Section Name" + CRLF + CRLF
   readsection.Lbl_2.VALUE := "Virtual Address " + CRLF + CRLF
   readsection.Lbl_3.VALUE := "Virtual Size " + CRLF + CRLF
   readsection.Lbl_4.VALUE := "Raw Address " + CRLF + CRLF
   readsection.Lbl_5.VALUE := "Raw Size " + CRLF + CRLF
   readsection.Lbl_6.VALUE := "Characteristics" + CRLF + CRLF
   readsection.Lbl_7.VALUE := "Note" + CRLF + CRLF

   nLen := Len( aReturn )
   IF nLen <> 0
      FOR i = 1 TO nLen
         cSectionName := aReturn[ i ][ 1 ] + CRLF
         cVirtualAddress := "0x" + IF( Empty( DecToHexa( aReturn[ i ][ 2 ] ) ), "00", IF( Len( DecToHexa(aReturn[ i ][ 2 ] ) ) < 2, "0" + DecToHexa( aReturn[ i ][ 2 ] ), DecToHexa( aReturn[ i ][ 2 ] ) ) ) + CRLF
         cVirtualSize := AllTrim( Str( aReturn[ i ][ 3 ] ) ) + CRLF
         cpRawData := "0x" + IF( Empty( DecToHexa( aReturn[ i ][ 4 ] ) ), "00", IF( Len( DecToHexa(aReturn[ i ][ 4 ] ) ) < 2, "0" + DecToHexa( aReturn[ i ][ 4 ] ), DecToHexa( aReturn[ i ][ 4 ] ) ) ) + CRLF
         cSRawData := AllTrim( Str( aReturn[ i ][ 5 ] ) ) + CRLF
         IF aReturn[ i ][ 6 ] > 0
            cCharacter := "0x" + IF( Empty( DecToHexa( aReturn[ i ][ 6 ] ) ), "00", IF( Len( DecToHexa(aReturn[ i ][ 6 ] ) ) < 2, "0" + DecToHexa( aReturn[ i ][ 6 ] ), DecToHexa( aReturn[ i ][ 6 ] ) ) ) + CRLF
         ELSE
            nChar := ( +4294967295 + ( aReturn[ i ][ 6 ] ) + 1 ) // 4294967295 => 0xFFFFFF
            cCharacter := "0x" + IF( Empty( DecToHexa( nChar ) ), "00", IF( Len( DecToHexa(nChar ) ) < 2, "0" + DecToHexa( nChar ), DecToHexa( nChar ) ) ) + CRLF
         ENDIF
         readsection.Lbl_1.VALUE := readsection.Lbl_1.VALUE +cSectionName
         readsection.Lbl_2.VALUE := readsection.Lbl_2.VALUE +cVirtualAddress
         readsection.Lbl_3.VALUE := readsection.Lbl_3.VALUE +cVirtualSize
         readsection.Lbl_4.VALUE := readsection.Lbl_4.VALUE +cPRawData
         readsection.Lbl_5.VALUE := readsection.Lbl_5.VALUE +cSRawData
         readsection.Lbl_6.VALUE := readsection.Lbl_6.VALUE +cCharacter
         DO CASE
         CASE cCharacter == "0xC0000040" + CRLF
            readsection.Lbl_7.VALUE := readsection.Lbl_7.VALUE +"Contains initialized data,Readable,Writeable" + CRLF
         CASE cCharacter == "0x60000020" + CRLF
            readsection.Lbl_7.VALUE := readsection.Lbl_7.VALUE +"Contains code,Executable,Readable" + CRLF
         CASE cCharacter == "0x50000040" + CRLF
            readsection.Lbl_7.VALUE := readsection.Lbl_7.VALUE +"Contains initialized data,Shareable,Readable" + CRLF
         CASE cCharacter == "0x40000040" + CRLF
            readsection.Lbl_7.VALUE := readsection.Lbl_7.VALUE +"Contains initialized data,Readable" + CRLF
         CASE cCharacter == "0xE0000040" + CRLF
            readsection.Lbl_7.VALUE := readsection.Lbl_7.VALUE +"Contains initialized data,Executable,Shareable,Readable,Writable" + CRLF
         CASE cCharacter == "0xE0000080" + CRLF
            readsection.Lbl_7.VALUE := readsection.Lbl_7.VALUE +"Contains uninitialized data,Executable,Shareable,Readable,Writable" + CRLF
         OTHERWISE
            readsection.Lbl_7.VALUE := readsection.Lbl_7.VALUE +SubStr( cCharacter, 1, Len( cCharacter ) - 2 ) + "=> not coded YET" + CRLF
         ENDCASE
      NEXT i
   ENDIF

RETURN NIL

FUNCTION HBChoose_File()

   LOCAL cFileName

   cFileName := Getfile ( { { 'Executable Files', '*.exe' } }, 'Open a File', , .F., .T. )

RETURN cFileName

#PRAGMA BEGINDUMP
#define _WIN32_IE      0x0500
#define HB_OS_WIN_USED
#define _WIN32_WINNT   0x0400
#define WINVER   0x0400
#include <windows.h>
#include <io.h>
#include <hbapiitm.h>

HB_FUNC( HBR_DCSECT )
{
  HANDLE hFile;
  BYTE *BaseAddress;
  WORD x;
  DWORD FileSize, BR;

  IMAGE_DOS_HEADER      *ImageDosHeader;
  IMAGE_NT_HEADERS      *ImageNtHeaders;
  IMAGE_SECTION_HEADER  *ImageSectionHeader;
  PHB_ITEM              pReturn;
  PHB_ITEM              pData;
  const char            *FileName = hb_parc(1);

  hFile = CreateFile(FileName, GENERIC_READ, FILE_SHARE_READ, 0,OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  {
  if (hFile == INVALID_HANDLE_VALUE)
    hb_retc("Cannot Open the File");
  }

  FileSize = GetFileSize(hFile, NULL);
  BaseAddress = (BYTE *) malloc(FileSize);

 if (!ReadFile(hFile, BaseAddress, FileSize, &BR, NULL))
  {
    free (BaseAddress);
    CloseHandle (hFile);
    hb_retc("Cannot Read the File");
  }

  ImageDosHeader = (IMAGE_DOS_HEADER *) BaseAddress;

// controlliamo il Dos Header

if (ImageDosHeader->e_magic != IMAGE_DOS_SIGNATURE)
 {
  free (BaseAddress);
  CloseHandle(hFile);
  hb_retc("Invalid Dos Header");
 }

 ImageNtHeaders = (IMAGE_NT_HEADERS *)(ImageDosHeader->e_lfanew + (DWORD) ImageDosHeader);

// controlliamo il PE Header
 if (ImageNtHeaders->Signature != IMAGE_NT_SIGNATURE)
 {
  free (BaseAddress);
  CloseHandle (hFile);
  hb_retc("Invalid PE Header");
 }


  pReturn = hb_itemNew(NULL);
  pData = hb_itemNew(NULL);
  hb_arrayNew(pReturn,0);
  hb_arrayNew(pData,6);

// prende l'indirizzo della prima sezione
 ImageSectionHeader = IMAGE_FIRST_SECTION(ImageNtHeaders);

//      mostra la section table
 for (x = 0; x < ImageNtHeaders->FileHeader.NumberOfSections; x++)
    {
     hb_arraySetC (pData,1,(const char *) ImageSectionHeader[x].Name );
     hb_arraySetNL(pData,2,ImageSectionHeader[x].VirtualAddress );
     hb_arraySetNL(pData,3,ImageSectionHeader[x].Misc.VirtualSize);
     hb_arraySetNL(pData,4,ImageSectionHeader[x].PointerToRawData);
     hb_arraySetNL(pData,5,ImageSectionHeader[x].SizeOfRawData );
     hb_arraySetNI(pData,6,ImageSectionHeader[x].Characteristics );
     hb_arrayAdd(pReturn,pData);
     hb_arrayNew(pData,6);
     }
   free (BaseAddress);
   CloseHandle (hFile);
   hb_itemRelease(pData);
   hb_itemRelease(hb_itemReturn(pReturn));
}

#pragma ENDDUMP
