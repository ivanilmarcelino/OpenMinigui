//  gHTMPRINT
//  E:\Appl\Harbour_MiniGui\Report_Class\
//  Anand K Gupta  Thu, 02 Mar 2017

#define CRLF    chr(13) + chr(10)

memvar cOutputFile

static nLastRow := 0, nLastCol := 0, lRowStart := .f., oReport

//*****************************************************************************
func HtmDevOut(nDevRow, nDevCol, cVar)
local i,cTxt,j,k

cTxt := ''
if nLastRow == 0
	cTxt += '<TR>' + CRLF
	nLastCol := 0
	lRowStart := .t.
elseif nLastRow <> nDevRow
	if lRowStart
		cTxt += '</TR>' + CRLF
	endif
	cTxt += '<TR>' + CRLF
	nLastCol := 0
	lRowStart := .t.
endif
nLastRow := nDevRow

for i := 1 to len(oReport:aColInfo)
	if oReport:aColInfo[i]:nCol == nDevCol
		if i > 1 .and. nLastCol == oReport:aColInfo[i-1]:nCol
			cTxt += '<TD>' + cVar + "</TD>"
		else
			k := ascan(oReport:aColInfo, {|e| e:nCol == nLastCol})
			for j := k to i-1
				cTxt += '<TD>'  + "</TD>"
			next
			cTxt += '<TD>' + cVar + "</TD>"
		endif
	endif
next
nLastCol := nDevCol

if ! Empty(cVar) .and. !(cVar $ cTxt)
	cTxt += '<TD>' + cVar + "</TD>"
else
endif

DevOut( cTxt )

return nil

//*****************************************************************************
func HtmInitPrinter(oRep)
local cSel,lOk,cTxt

cSel := Select()
lOk := .f.

while .t.
	oReport := oRep
	nLastRow := 0
	nLastCol := 0
	lRowStart := .f.

	cTxt := ''
	cTxt += '<HTML>' + CRLF
	cTxt += '<BODY>' + CRLF
	cTxt += '<TABLE>' + CRLF
	DevOut( cTxt )

	lOk := .t.
	exit
end

Select (cSel)

return lOk

//*****************************************************************************
func HtmResetPrinter()
local cSel,lOk,cTxt

cSel := Select()
lOk := .f.

while .t.
	cTxt := ""
	if lRowStart
		cTxt += '</TR>' + CRLF
	endif
	cTxt += '</TABLE>' + CRLF
	cTxt += '</BODY>' + CRLF
	cTxt += '</HTML>' + CRLF
	DevOut( cTxt )

	lOk := .t.
	exit
end

Select (cSel)

return lOk

//*****************************************************************************
func HtmshowRepHeader()
local cSel,lOk

cSel := Select()
lOk := .f.

while .t.

	lOk := .t.
	exit
end

Select (cSel)

return lOk

//*****************************************************************************
func HtmshowRepFooter()
local cSel,lOk

cSel := Select()
lOk := .f.

while .t.

	lOk := .t.
	exit
end

Select (cSel)

return lOk

//*****************************************************************************
func HtmshowHeader()
local cSel,lOk

cSel := Select()
lOk := .f.

while .t.

	lOk := .t.
	exit
end

Select (cSel)

return lOk

//*****************************************************************************
func HtmshowFooter()
local cSel,lOk

cSel := Select()
lOk := .f.

while .t.

	lOk := .t.
	exit
end

Select (cSel)

return lOk

