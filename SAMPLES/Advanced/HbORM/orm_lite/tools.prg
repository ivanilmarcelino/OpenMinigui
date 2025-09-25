/*

 BadaSystem
 Program       : HbORM - Harbour Object-Relational Mapping
 Module        : Tools - Utilities for the Harbour ORM
 Compiler      : MINIGUI - Harbour Win32 GUI
 Compiler-C    : BCC 32 bit
 Author        : Marcos Jarrin
 Email         : marvijarrin@gmail.com
 Date          : 10/07/2025
 Update        : 05/08/2025
 Rev           : 0.1
 Description: Functions to extend HbORM

*/

#include "minigui.ch"

/**
 * Converts an array to a delimited string list
 * function ArrayToList
 * memberof Tools
 * param {Array} aArray - The array to convert
 * param {String} [cDelimiter=","] - The delimiter to use between elements
 * param {Boolean} [lQuote=.F.] - Whether to quote each element
 * returns {String} A string containing all array elements joined by the delimiter
 * example
 * // Returns "apple,orange,banana"
 * ArrayToList({"apple", "orange", "banana"})
 *
 * // Returns "'apple','orange','banana'"
 * ArrayToList({"apple", "orange", "banana"}, ",", .T.)
 */
FUNCTION ArrayToList(aArray, cDelimiter, lQuote)   //STATIC
   LOCAL cList := ""
   LOCAL xItem

   DEFAULT cDelimiter := ","
   DEFAULT lQuote     := .F.

   FOR EACH xItem IN aArray
      IF !Empty(cList)
         cList += cDelimiter
      ENDIF

      IF lQuote
         cList += "'" + ValToStr(xItem) + "'"
      ELSE
         cList += ValToStr(xItem)
      ENDIF
   NEXT

   RETURN cList


/**
 * Converts any value to its string representation
 * function ValToStr
 * memberof Tools
 * private
 * param {*} xValue - The value to convert to string
 * returns {String} The string representation of the value
 * example
 * // Returns "Hello"
 * ValToStr("Hello")
 *
 * // Returns "42"
 * ValToStr(42)
 *
 * // Returns ".T."
 * ValToStr(.T.)
 *
 * // Returns "2025-08-02"
 * ValToStr(CTOD("2025-08-02"))
 */
STATIC FUNCTION ValToStr(xValue)

   LOCAL xReturn

   DO CASE
      CASE ValType(xValue) == "C"
         xReturn := xValue
      CASE ValType(xValue) == "N"
         xReturn := AllTrim(Str(xValue))
      CASE ValType(xValue) == "D"
         xReturn := DToC(xValue)
      CASE ValType(xValue) == "L"
         xReturn := IIf(xValue, ".T.", ".F.")
      CASE ValType(xValue) == "A"
         xReturn := "Array(" + AllTrim(Str(Len(xValue))) + ")"
      CASE ValType(xValue) == "H"
         xReturn := "Hash(" + AllTrim(Str(Len(hb_HKeys(xValue)))) + ")"
      CASE ValType(xValue) == "B"
         xReturn := "Code block"
      CASE xValue == NIL
         xReturn := "NIL"
      OTHERWISE
         xReturn := "Unknown"
   ENDCASE

RETURN xReturn
