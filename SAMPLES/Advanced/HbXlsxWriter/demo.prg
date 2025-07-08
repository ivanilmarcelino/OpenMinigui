/*
 * A simple example of some of the features of the hblibxlsxwriter library.
 *
 */

#include "minigui.ch"
#include "hblibxlsxwriter.ch"

PROCEDURE main()

	LOCAL cWorkBook := "demo.xlsx", cErrMsg := "Error in workbook_close()."

	LOCAL nError := CreateSpreadsheet( cWorkBook )

	IF nError == S_OK 
		ShellExecute( 0, "open", cWorkBook,,, SW_SHOW ) 
	ELSE
		MsgStop( cErrMsg ) 
	ENDIF
		
	RETURN


FUNCTION CreateSpreadsheet( cName )
    local workbook, worksheet, format

    lxw_init() 

    /* Create a new workbook and add a worksheet. */
    workbook  = lxw_workbook_new("demo.xlsx")
    worksheet = lxw_workbook_add_worksheet(workbook, NIL)

    /* Add a format. */
    format = lxw_workbook_add_format(workbook)

    /* Set the bold property for the format */
    lxw_format_set_bold(format)

    /* Change the column width for clarity. */
    lxw_worksheet_set_column(worksheet, 0, 0, 20, NIL)

    /* Write some simple text. */
    lxw_worksheet_write_string(worksheet, 0, 0, "Hello", NIL)

    /* Text with formatting. */
    lxw_worksheet_write_string(worksheet, 1, 0, "World", format)

    /* Write some numbers. */
    lxw_worksheet_write_number(worksheet, 2, 0, 123,     NIL)
    lxw_worksheet_write_number(worksheet, 3, 0, 123.456, NIL)

    /* Insert an image. */
    lxw_worksheet_insert_image(worksheet, 1, 2, "logo.png")

    return lxw_workbook_close(workbook)
