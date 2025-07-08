//  gHtmAbsPosPRINT
//  E:\Appl\Harbour_HMG\Report_Class_HMG\
//  Anand K Gupta  Mon, 08 May 2017

#include "minigui.ch"

#define ntrim( n )    hb_ntos( n )

memvar cOutputFile

static nLastRow := 0, nLastPg := 1, nPageRow := 0, oReport

//*****************************************************************************
func HtmAbsPosDevOut(nDevRow, nDevCol, cVar)
local cTxt,nCol,nRow

if oReport:nPageNo <> nLastPg
	nPageRow := nLastRow
	nLastPg := oReport:nPageNo
else
endif

nDevRow += nPageRow

nRow := nDevRow * 20
nCol := nDevCol * 10
//cTxt := '<div style="position: absolute;left: '+ntrim(nCol)+'px;top: '+ntrim(nRow)+'px;width: 114px;text-align: left;">This is Header</div>'
cTxt := '<div style="position: absolute;left: '+ntrim(nCol)+'px;top: '+ntrim(nRow)+'px;text-align: left;">'+cVar+'</div>'

DevOut( cTxt )

nLastRow := nDevRow

return nil

//*****************************************************************************
func HtmAbsPosInitPrinter(oRep)
local cSel,lOk,cTxt

cSel := Select()
lOk := .f.

while .t.
	oReport := oRep
	nLastRow := 0
	nLastPg := 1
	nPageRow := 0

	cTxt := ''
	cTxt += '<HTML>' + CRLF
	cTxt += '<BODY>' + CRLF
	DevOut( cTxt )

	lOk := .t.
	exit
end

Select (cSel)

return lOk

//*****************************************************************************
func HtmAbsPosResetPrinter()
local cSel,lOk,cTxt

cSel := Select()
lOk := .f.

while .t.
	cTxt := ""
	cTxt += '</BODY>' + CRLF
	cTxt += '</HTML>' + CRLF
	DevOut( cTxt )

	lOk := .t.
	exit
end

Select (cSel)

return lOk

////*****************************************************************************
//func HtmshowRepHeader()
//local i,cSel,lOk
//
//cSel := Select()
//lOk := .f.
//
//while .t.
//
//	lOk := .t.
//	exit
//end
//
//Select (cSel)
//
//return lOk
//
////*****************************************************************************
//func HtmshowRepFooter()
//local i,cSel,lOk
//
//cSel := Select()
//lOk := .f.
//
//while .t.
//
//	lOk := .t.
//	exit
//end
//
//Select (cSel)
//
//return lOk
//
////*****************************************************************************
//func HtmshowHeader()
//local i,cSel,lOk
//
//cSel := Select()
//lOk := .f.
//
//while .t.
//
//	lOk := .t.
//	exit
//end
//
//Select (cSel)
//
//return lOk
//
////*****************************************************************************
//func HtmshowFooter()
//local i,cSel,lOk
//
//cSel := Select()
//lOk := .f.
//
//while .t.
//
//	lOk := .t.
//	exit
//end
//
//Select (cSel)
//
//return lOk

