My small class for reading xlsx files directly (without using OLE, libraries, etc.)

Once I needed to read large xlsx files (over 200k rows) and all xml-parser solutions
I knew were too slow. So I decided to delve into the structure of xlsx files and
built a class that does not create temporary files and loads data into memory and
uses tokens for xml analysis.

The class is small and consists of the following methods:

oXLSX := SmallXLSXReader():Open( cXlsxFileOpen, bBlockUnZip, bBlockParser, lAttributes )

Opens xlsx file, parameters are given: file name with path, optional code block
executed during xml component unpacking, optional code block executed during xml data
processing, optional logical parameter specifying whether attributes*) should be
included in the returned data also.
*) attributes are a hash table with keys:
"col" - cell address,
"style" - style used in the cell,
"DateTime" - whether the data is date and time,
"type" - data type,
"value" - value in the cell (entered or calculated according to a formula),
"formula" - used formula

aSheetList := oXLSX:WorkSheetList()
Returns an array of sheet names in the workbook.

aSheetData := oXLSX:WorkSheet( cSheetName, bBlockUnZip, bBlockParser )
Reads data from the sheet and returns it as an array

oXLSX:Close()
Releases variables stored in the class.


The class was written for my needs and I reserve that there is no guarantee that
it will work with all xlsx workbooks, but maybe it will be useful to someone.

Edward
