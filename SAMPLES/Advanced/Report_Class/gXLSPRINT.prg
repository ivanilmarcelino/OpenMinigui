//  gXLSPRINT
//  E:\Appl\Harbour_MiniGui\Report_Class\
//  Anand K Gupta  Fri, 03 Mar 2017

#define CRLF    chr(13) + chr(10)

memvar cOutputFile,oExcel,oXls

static nLastRow := 0, nLastCol := 0, lRowStart := .f., nXlsRow, oReport

//*****************************************************************************
func XlsDevOut(nDevRow, nDevCol, cVar)
local i,lValue

if nLastRow == 0
	nXlsRow := 1
elseif nLastRow <> nDevRow
	nXlsRow ++
endif
nLastRow := nDevRow

if left(cVar,1) == "="
	cVar := "'"+cVar
endif

lValue := .f.
for i := 1 to len(oReport:aColInfo)
	if oReport:aColInfo[i]:nCol == nDevCol
		oXls:Cells( nXlsRow, i ):Value := cVar
		lValue := .t.
	endif
next

if ! lValue
	oXls:Cells( nXlsRow, 1 ):Value := cVar
else
endif
return nil

//*****************************************************************************
func XlsInitPrinter(oRep)
local cSel,lOk

cSel := Select()
lOk := .f.

while .t.
	oReport := oRep
   BEGIN SEQUENCE WITH {|e| Break( e ) }
	oExcel := CreateObject( "Excel.Application" )
   RECOVER
	exit
   END SEQUENCE
	oExcel:WorkBooks:Add()
	oXls := oExcel:ActiveSheet()
	oXls:Cells:Font:Name := "Arial"
	oXls:Cells:Font:Size := 9
	//oExcel:Visible := .T.

	lOk := .t.
	exit
end

Select (cSel)

return lOk

//*****************************************************************************
func XlsResetPrinter()
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
func XlsshowRepHeader()
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
func XlsshowRepFooter()
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
func XlsshowHeader()
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
func XlsshowFooter()
local cSel,lOk

cSel := Select()
lOk := .f.

while .t.

	lOk := .t.
	exit
end

Select (cSel)

return lOk

