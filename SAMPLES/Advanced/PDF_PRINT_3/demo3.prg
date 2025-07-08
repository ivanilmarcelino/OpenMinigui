/*
 * MINIGUI - Harbour Win32 GUI library Demo
 *
 * Demo was contributed to HMG forum by Edward 04/Dec/2018
 *
 * Adapted for MiniGUI Extended Edition by Grigory Filatov
 */

#include "hmg.ch"
#include "hmg_hpdf.ch"
#include "harupdf.ch"

Function Main()

Local cFonte1:="segoeui.ttf" //"Helvetica"		

Local cEdit := "Whitnesseth that said Grantor, for and in consideration of the sum of ten dollars, and other good and valuable considerations " +;
				"to said Grantor in hand paid by said Grantee, the receipt whereof is hereby acknowledged, has remised, released and quit claimed, " +;
				"and by these presents does remise, release and quitclaim unto the Grantee all the right, title, interest, claim and demand which " +;
				"the said Grantor has in and the following described land situate, lying and being in current state."

Local nHeight, nNextRow, nBottomRow

SELECT HPDFDOC "sample3.pdf" PAPERLENGTH 300 PAPERWIDTH 300
START HPDFDOC
	SET HPDFDOC ENCODING TO "WinAnsiEncoding"
		
	START HPDFPAGE

		//Adjust the text height of the area and get the bottom row using a variable by reference
		@ 20, 15 HPDFPRINT cEdit TO 25, 185 FONT cFonte1 size 9 STRIKEOUT HEIGHTFIT GETBOTTOM @nBottomRow

		nNextRow := nBottomRow + 1
		
		//Calculate the height of the text of the area with word wrapping.
		nHeight := _HMG_HPDF_GetHeight_MULTILINE_PRINT ( cEdit /* cText */ , 185 - 15 /* nLen ( nToCol - nCol ) */ , cFonte1 /* cFontName */, 9 /* nFontSize */,  /* lBold */, /* lItalic */ , .f. /* lWrap */ )		
		//Print area with calculated height.
		@ nNextRow, 15 HPDFPRINT cEdit TO nNextRow + nHeight , 185 FONT cFonte1 size 9 COLOR BLUE JUSTIFY STRIKEOUT
		
		nNextRow += nHeight + 1
		
		//Calculate the height of the text of the area without word wrapping.
		nHeight := _HMG_HPDF_GetHeight_MULTILINE_PRINT ( cEdit /* cText */ , 185 - 15 /* nLen ( nToCol - nCol ) */ , cFonte1 /* cFontName */, 10 /* nFontSize */, .T. /* lBold */, /* lItalic */ , .F. /* lWrap */ )	
		//Print  area with calculated height.
		@ nNextRow, 15 HPDFPRINT cEdit TO nNextRow + nHeight , 185 FONT cFonte1 size 10 BOLD COLOR GREEN RIGHT UNDERLINE	
		
		nNextRow += nHeight + 1
		
		//Change line spacing to 6 px
		SET HPDFPAGE LINESPACING TO _HMG_HPDF_Pixel2MM( 9 )		
		
		//Calculate the text height of the area with the previously set line spacing.
		nHeight := _HMG_HPDF_GetHeight_MULTILINE_PRINT ( cEdit /* cText */ , 185 - 15 /* nLen ( nToCol - nCol ) */ , cFonte1 /* cFontName */, 12.5 /* nFontSize */, /* lBold */, /* lItalic */ , /* lWrap */ )
		//Print area with calculated height.
		@ nNextRow, 15 HPDFPRINT cEdit TO nNextRow + nHeight, 185 FONT cFonte1 size 12.5 COLOR RED CENTER STRIKEOUT 
		
		//Restore line spacing to defaults
		SET HPDFPAGE LINESPACING TO 0
		
		nNextRow += nHeight + 1
		
		//Adjust the text height of the area and get the bottom row using a variable by reference
		@ nNextRow, 15 HPDFPRINT "Some short text ABC E xyz" TO nNextRow, 185 FONT cFonte1 size 8 UNDERLINE HEIGHTFIT GETBOTTOM @nBottomRow


		nNextRow := nBottomRow + 1
		
		//Calculate the text height of the area.
		nHeight := _HMG_HPDF_GetHeight_MULTILINE_PRINT ( "Some short text ABC E xyz" /* cText */ , 185 - 15 /* nLen ( nToCol - nCol ) */ , cFonte1 /* cFontName */, 8 /* nFontSize */, .T. /* lBold */, /* lItalic */ , /* lWrap */ )	
		//Print area with calculated height.
		@ nNextRow, 15 HPDFPRINT "Some short text ABC E xyz" TO nNextRow + nHeight, 185 FONT cFonte1 size 8 BOLD RIGHT UNDERLINE
		
		nNextRow += nHeight + 1
		
		//Adjust the font size of the text to the fixed height of the area and get the bottom row using the variable by reference.
		@ nNextRow, 15 HPDFPRINT cEdit TO nNextRow + 12, 185 FONT cFonte1 size 10 UNDERLINE FONTSIZEFIT GETBOTTOM @nBottomRow  
	
		//Print the line of text in the last row.
		@ nBottomRow, 15 HPDFPRINT "Last Row := " + AllTrim(Str(nBottomRow)) + " mm" SIZE 6
		
	END HPDFPAGE
END HPDFDOC
Execute File 'sample3.pdf'
Return Nil
