#include "minigui.ch"

// Declare the FreeImage header
#include "FreeImage.ch"

FUNCTION Main()
   LOCAL hImage := NIL
   LOCAL cFilePath := ""
   LOCAL nWidth := 0, nHeight := 0, cFormat := ""

   FI_Initialise()

   DEFINE WINDOW Win_Main ;
      WIDTH 600 HEIGHT 525 ;
      TITLE "FreeImage Test" MAIN ;
      ON RELEASE FI_Deinitialise() ;
      FONT 'MS Sans Serif' SIZE 10

      @ 10, 10 BUTTON Btn_Load CAPTION "Load Image" ACTION LoadImage(hImage, cFilePath, nWidth, nHeight, cFormat)
      @ 10, 120 BUTTON Btn_Close CAPTION "Close" ACTION Win_Main.Release
      
      @ 50, 10 LABEL lblPath VALUE "File: " WIDTH 500
      @ 75, 10 LABEL lblSize VALUE "Size: " WIDTH 500
      @ 100, 10 LABEL lblFormat VALUE "Format: " WIDTH 500
      
      @ 125, 10 IMAGE imgDisplay PICTURE "" WIDTH 560 HEIGHT 350 ;
         STRETCH ON CLICK iif(At(".", Win_Main.lblPath.Value) == 0, , SetComments())
   
   END WINDOW

   CENTER WINDOW Win_Main
   ACTIVATE WINDOW Win_Main
RETURN NIL

FUNCTION LoadImage(hImage, cFilePath, nWidth, nHeight, cFormat)
   LOCAL aFilter := { {"Image Files (*.jpg;*.png;*.bmp)", "*.jpg;*.png;*.bmp"} }
   LOCAL cSelectedFile := GetFile(aFilter, "Select an image")
   LOCAL hBitmap, cFileExt, cFif

   IF Empty(cSelectedFile)
      RETURN NIL
   ENDIF

   cFilePath := cSelectedFile
   Win_Main.lblPath.Value := "File: " + cFilePath

   cFileExt  := HB_FNameExt(cFilePath)
   IF cFileExt == ".jpg"
      cFormat := "JPEG"
      cFif := FIF_JPEG
   ELSEIF cFileExt == ".png"
      cFormat := "PNG"
      cFif := FIF_PNG
   ELSE
      cFormat := "BITMAP"
      cFif := FIF_BMP
   ENDIF

   // Load image using FreeImage wrapper
   hImage := FI_LOAD(cFif, cFilePath, 0)
   IF hImage == NIL
      MsgStop("Failed to load image.", "Error")
      RETURN NIL
   ENDIF

   // Get image width and height
   nWidth := FI_GETWIDTH(hImage)
   nHeight := FI_GETHEIGHT(hImage)

   Win_Main.lblSize.Value := "Size: " + hb_ntos(nWidth) + " x " + hb_ntos(nHeight)

   Win_Main.lblFormat.Value := "Format: " + cFormat

   // Stretch to IMAGE control size
   hImage := FI_Rescale(hImage, Win_Main.imgDisplay.Width, Win_Main.imgDisplay.Height, FILTER_BICUBIC)
   // Convert to HBITMAP
   hBitmap := WIN_P2N(FI_WINCONVTODIB(hImage))
   IF ! Empty(hBitmap)
      Win_Main.imgDisplay.hBitmap := hBitmap
   ELSE
      MsgStop("Failed to display image.", "Error")
   ENDIF

   // Free image memory
   FI_UNLOAD(hImage)
RETURN NIL

FUNCTION SetComments()
   LOCAL cFilePath := SubStr(Win_Main.lblPath.Value, At(": ", Win_Main.lblPath.Value)+2)
   LOCAL aComments := {}
   LOCAL cText := ""
   LOCAL handle, cFileExt

   cFileExt  := HB_FNameExt(cFilePath)
   IF cFileExt == ".jpg"
      handle := FI_LOAD(FIF_JPEG, cFilePath, 0)
      IF FI_GetMetadataCount(FIMD_COMMENTS, handle) > 0
         aComments := FI_GETCOMMENTS(FIMD_COMMENTS, handle)
      ENDIF
      IF Len(aComments) > 0
         cText := aComments[2]
      ENDIF
      // Get comment from user input
      cText := InputBox('Enter your comment:', 'New comment', cText)
      // Write comment
      FI_SETCOMMENTS(FIMD_COMMENTS, handle, cText, Len(cText))
      // Save modified file
      FI_Save(FIF_JPEG, handle, cFilePath, JPEG_DEFAULT)
      // Free image memory
      FI_UNLOAD(handle)
   ENDIF
RETURN NIL
