/*
 * MiniGUI WMI Service Demo
 *
 * (c) 2019 Grigory Filatov <gfilatov@inbox.ru>
*/

#include "minigui.ch"

PROCEDURE Main
  
	DEFINE WINDOW Form_1 ;
		AT 0,0 ;
		WIDTH 400 ;
		HEIGHT 200 ;
		TITLE 'WMI Service Demo' ;
		MAIN ;
		ON INTERACTIVECLOSE MsgYesNo ( 'Are You Sure ?', 'Exit' )

		DEFINE BUTTON Button_1
			ROW	10
			COL	10
			WIDTH	120
			CAPTION 'File System Info'
			ACTION WMIFileSystem( Getfile() )
		END BUTTON

		DEFINE BUTTON Button_2
			ROW	40
			COL	10
			WIDTH	120
			CAPTION 'Cancel'
			ACTION ThisWindow.Release
		END BUTTON

	END WINDOW

	CENTER WINDOW Form_1
	ACTIVATE WINDOW Form_1

RETURN


FUNCTION WMIFileSystem( cFullPath )

   Local oFSO, oFile
   Local cInfo := ""

   IF !Empty( cFullPath )

      oFSO  := CreateObject( "Scripting.FileSystemObject" )
      oFile := oFSO:GetFile( cFullPath )

      cInfo += "Date created: " + hb_ValToStr( oFile:DateCreated ) + CRLF
      cInfo += "Date last accessed: " + hb_ValToStr( oFile:DateLastAccessed ) + CRLF
      cInfo += "Date last modified: " + hb_ValToStr( oFile:DateLastModified ) + CRLF
      cInfo += "Parent folder: " + cFilePath( cFullPath ) + CRLF
      cInfo += "Name: " + oFile:Name + CRLF
      cInfo += "Short name: " + oFile:ShortName + CRLF
      cInfo += "Short path: " + oFile:ShortPath + CRLF
      cInfo += "Size: " + cValToChar( oFile:Size ) + " bytes" + CRLF
      cInfo += "Type: " + oFile:Type + CRLF

      cInfo += CRLF
      cInfo += "MD5: " + Upper( hb_ValToStr( hb_md5file( cFullPath ) ) ) + CRLF
      cInfo += "CRC32: " + hb_ValToStr( hb_crc32( cFullPath ) )

      MsgInfo( cInfo, "File Properties" )

   ENDIF

RETURN NIL
