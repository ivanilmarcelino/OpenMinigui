//  gPdfPRINT
//  E:\Appl\Alaska\Ace10\
//  Anand K Gupta  Tue, 28 Feb 2017

#include "hmg.ch"
#include "hmg_hpdf.ch"

memvar cOutputFile

//*****************************************************************************
func PdfDevOut(nDevRow, nDevCol, cVar)

@ nDevRow * 5, nDevCol * 2.5 HPDFPRINT cVar

return nil

//*****************************************************************************
func PdfInitPrinter()
local cSel,lOk,lSuccess

cSel := Select()
lOk := .f.

while .t.
	SELECT PRINTER DEFAULT ORIENTATION PRINTER_ORIENT_PORTRAIT PREVIEW
	SELECT HPDFDOC (cOutputFile) TO lSuccess papersize HPDF_PAPER_A4

	IF ! lSuccess
		MSGSTOP ('Print Cancelled!')
		exit
	ENDIF

	START HPDFDOC

	lOk := .t.
	exit
end

Select (cSel)

return lOk

//*****************************************************************************
func PdfResetPrinter()
local cSel,lOk

cSel := Select()
lOk := .f.

while .t.
	END HPDFDOC

	lOk := .t.
	exit
end

Select (cSel)

return lOk

//*****************************************************************************
func PdfshowRepHeader()
local cSel,lOk

cSel := Select()
lOk := .f.

while .t.
	START HPDFPAGE

	lOk := .t.
	exit
end

Select (cSel)

return lOk

//*****************************************************************************
func PdfshowRepFooter()
local cSel,lOk

cSel := Select()
lOk := .f.

while .t.
	END HPDFPAGE

	lOk := .t.
	exit
end

Select (cSel)

return lOk

//*****************************************************************************
func PdfshowHeader()
local cSel,lOk

cSel := Select()
lOk := .f.

while .t.
	START HPDFPAGE

	lOk := .t.
	exit
end

Select (cSel)

return lOk

//*****************************************************************************
func PdfshowFooter()
local cSel,lOk

cSel := Select()
lOk := .f.

while .t.
	END HPDFPAGE

	lOk := .t.
	exit
end

Select (cSel)

return lOk
