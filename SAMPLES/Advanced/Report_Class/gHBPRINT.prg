//  gHBPRINT
//  E:\Appl\Harbour_MiniGui\Report_Class\
//  Anand K Gupta  Wed, 22 Feb 2017

#include "hmg.ch"
#include "minigui.ch"

//*****************************************************************************
func hbDevOut(nDevRow, nDevCol, cVar)

@ nDevRow * 5, nDevCol * 2.5 PRINT cVar

return nil

//*****************************************************************************
func HbInitPrinter()
local cSel,lOk

cSel := Select()
lOk := .f.

while .t.
	SELECT PRINTER DEFAULT ORIENTATION PRINTER_ORIENT_PORTRAIT PREVIEW

//   HO := GetPrintableAreaHorizontalOffset()
//   VO := GetPrintableAreaVerticalOffset()
//
//   W := GetPrintableAreaWidth()
//   H := getPrintableAreaHeight()

//	define font "f0" name "courier new" size 10
//	define font "f1" name "arial" size 10
//
//	if lProportionate
//		select font "f1"
//	else
//		select font "f0"
//	endif

//	IF ! lSuccess
//		MSGSTOP ('Print Cancelled!')
//		exit
//	ENDIF

	START PRINTDOC

	lOk := .t.
	exit
end

Select (cSel)

return lOk

//*****************************************************************************
func hbResetPrinter()
local cSel,lOk

cSel := Select()
lOk := .f.

while .t.
	END PRINTDOC

	lOk := .t.
	exit
end

Select (cSel)

return lOk

//*****************************************************************************
func hbshowRepHeader()
local cSel,lOk

cSel := Select()
lOk := .f.

while .t.
	START PRINTPAGE

	lOk := .t.
	exit
end

Select (cSel)

return lOk

//*****************************************************************************
func hbshowRepFooter()
local cSel,lOk

cSel := Select()
lOk := .f.

while .t.
	END PRINTPAGE

	lOk := .t.
	exit
end

Select (cSel)

return lOk

//*****************************************************************************
func hbshowHeader()
local cSel,lOk

cSel := Select()
lOk := .f.

while .t.
	START PRINTPAGE

	lOk := .t.
	exit
end

Select (cSel)

return lOk

//*****************************************************************************
func hbshowFooter()
local cSel,lOk

cSel := Select()
lOk := .f.

while .t.
	END PRINTPAGE

	lOk := .t.
	exit
end

Select (cSel)

return lOk

