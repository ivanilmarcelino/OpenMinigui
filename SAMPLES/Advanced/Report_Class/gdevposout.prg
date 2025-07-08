//  gdevposout
//  E:\Appl\Harbour_MiniGui\Report_Class\
//  Anand K Gupta  Wed, 22 Feb 2017

#include "minigui.ch"

memvar cOutputFile,oExcel,oXls

static nDevRow, nDevCol, cDevOutputOn, lFontProportionate := .f., lHtmlAbsolutePosition := .f.

//*****************************************************************************
func gDevice(cDevice, lProportionate, lAbsolutePosition)
local xDevice

if cDevice == nil
	xDevice := cDevOutputOn
else
	cDevOutputOn := cDevice
endif
if lProportionate <> nil
	lFontProportionate := lProportionate
endif
if lAbsolutePosition <> nil
	lHtmlAbsolutePosition := lAbsolutePosition
endif

return xDevice

//*****************************************************************************
func gDevPos(nRow, nCol)

nDevRow := nRow
nDevCol := nCol

return nil

//*****************************************************************************
func gDevOut(cVar)

if cDevOutputOn == "TEXT"
	DevPos( nDevRow, nDevCol )
	DevOut( cVar )
elseif cDevOutputOn == "HBPRINT"
	hbDevOut(nDevRow, nDevCol, cVar)
elseif cDevOutputOn == "PDF"
	PdfDevOut(nDevRow, nDevCol, cVar)
elseif cDevOutputOn == "HTM"
	if lHtmlAbsolutePosition
		HtmAbsPosDevOut(nDevRow, nDevCol, cVar)
	else
		HtmDevOut(nDevRow, nDevCol, cVar)
	endif
elseif cDevOutputOn == "XLS"
	XlsDevOut(nDevRow, nDevCol, cVar)
endif

return nil

//*****************************************************************************
func gDevOutPict(xVar, cPict)

gDevOut( Transform(xVar, cPict) )

return nil

//*****************************************************************************
func gInitPrinter(oReport)
local cSel,lOk

cSel := Select()
lOk := .f.

while .t.
	if cDevOutputOn == "TEXT"
		cOutputFile := "REPORT.TXT"
		SET PRINTER TO (cOutputFile)
		SET DEVICE TO PRINT
	elseif cDevOutputOn == "HBPRINT"
		HbInitPrinter(lFontProportionate)
		oReport:nStartCol := 5
		oReport:nLastRow := (GetPrintableAreaHeight() / 5 ) - 5
	elseif cDevOutputOn == "PDF"
		cOutputFile := "REPORT.PDF"
		If file(cOutputFile)
			Ferase(cOutputFile)
		Endif
		PdfInitPrinter()
		oReport:nStartCol := 5
		oReport:nLastRow := (GetPrintableAreaHeight() / 5 ) - 5
	elseif cDevOutputOn == "HTM"
		cOutputFile := "REPORT.HTM"
		SET PRINTER TO (cOutputFile)
		SET DEVICE TO PRINT
		if lHtmlAbsolutePosition
			HtmAbsPosInitPrinter(oReport)
		else
			HtmInitPrinter(oReport)
		endif
	elseif cDevOutputOn == "XLS"
		cOutputFile := "REPORT.XLS"
		if !XLSInitPrinter(oReport)
			exit
		endif
	endif

	lOk := .t.
	exit
end

Select (cSel)

return lOk

//*****************************************************************************
func gResetPrinter()
local cSel,lOk

cSel := Select()
lOk := .f.

while .t.
	if cDevOutputOn == "TEXT"
		SET PRINTER TO
		SET DEVICE TO SCREEN
		ShellExecute( , 'open', cOutputFile, "", "" , SW_SHOWNORMAL )
	elseif cDevOutputOn == "HBPRINT"
		hbResetPrinter()
	elseif cDevOutputOn == "PDF"
		PdfResetPrinter()
		execute file (cOutputFile)
	elseif cDevOutputOn == "HTM"
		if lHtmlAbsolutePosition
			HtmAbsPosResetPrinter()
		else
			HtmResetPrinter()
		endif
		SET PRINTER TO
		SET DEVICE TO SCREEN
		ShellExecute( , 'open', cOutputFile, "", "" , SW_SHOWNORMAL )
	elseif cDevOutputOn == "XLS"
		XLSResetPrinter()
		oXls:Cells( 1, 1 ):Select()
		oExcel:Visible := .T.
	endif

	lOk := .t.
	exit
end

Select (cSel)

return lOk

//*****************************************************************************
func gshowRepHeader()
local cSel,lOk

cSel := Select()
lOk := .f.

while .t.
	if cDevOutputOn == "TEXT"
		//
	elseif cDevOutputOn == "HBPRINT"
		hbshowRepHeader()
	elseif cDevOutputOn == "PDF"
		PdfshowRepHeader()
	elseif cDevOutputOn == "HTM"
		HtmshowRepHeader()
	elseif cDevOutputOn == "XLS"
		XLSshowRepHeader()
	endif

	lOk := .t.
	exit
end

Select (cSel)

return lOk

//*****************************************************************************
func gshowRepFooter()
local cSel,lOk

cSel := Select()
lOk := .f.

while .t.
	if cDevOutputOn == "TEXT"
		//
	elseif cDevOutputOn == "HBPRINT"
		hbshowRepFooter()
	elseif cDevOutputOn == "PDF"
		PdfshowRepFooter()
	elseif cDevOutputOn == "HTM"
		HtmshowRepFooter()
	elseif cDevOutputOn == "XLS"
		XLSshowRepFooter()
	endif

	lOk := .t.
	exit
end

Select (cSel)

return lOk

//*****************************************************************************
func gshowHeader()
local cSel,lOk

cSel := Select()
lOk := .f.

while .t.
	if cDevOutputOn == "TEXT"
		//
	elseif cDevOutputOn == "HBPRINT"
		hbshowHeader()
	elseif cDevOutputOn == "PDF"
		PdfshowHeader()
	elseif cDevOutputOn == "HTM"
		HtmshowHeader()
	elseif cDevOutputOn == "XLS"
		XLSshowHeader()
	endif

	lOk := .t.
	exit
end

Select (cSel)

return lOk

//*****************************************************************************
func gshowFooter()
local cSel,lOk

cSel := Select()
lOk := .f.

while .t.
	if cDevOutputOn == "TEXT"
		//
	elseif cDevOutputOn == "HBPRINT"
		hbshowFooter()
	elseif cDevOutputOn == "PDF"
		PdfshowFooter()
	elseif cDevOutputOn == "HTM"
		HtmshowFooter()
	elseif cDevOutputOn == "XLS"
		XLSshowFooter()
	endif

	lOk := .t.
	exit
end

Select (cSel)

return lOk

//*****************************************************************************
func gEject()
local cSel,lOk

cSel := Select()
lOk := .f.

while .t.
	if cDevOutputOn == "TEXT"
		eject
	elseif cDevOutputOn == "HBPRINT"
		//
	elseif cDevOutputOn == "PDF"
		//
	elseif cDevOutputOn == "HTM"
		//
	elseif cDevOutputOn == "XLS"
		//
	endif

	lOk := .t.
	exit
end

Select (cSel)

return lOk

//*****************************************************************************
func gHtmTableCloseOpen()
local cSel,lOk,cTxt

cSel := Select()
lOk := .f.

while .t.
	if lHtmlAbsolutePosition
	else
		cTxt := '</TABLE>' + CRLF
		cTxt += '</BODY>' + CRLF
		cTxt += '</HTML>' + CRLF
		DevOut( cTxt )

		cTxt := ''
		cTxt += '<HTML>' + CRLF
		cTxt += '<BODY>' + CRLF
		cTxt += '<TABLE>' + CRLF
		DevOut( cTxt )
	endif

	lOk := .t.
	exit
end

Select (cSel)

return lOk
